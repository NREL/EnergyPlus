// EnergyPlus, Copyright (c) 1996-2019, The Board of Trustees of the University of Illinois,
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

// EnergyPlus::Indoor Ice Rink Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/IndoorIceRink.hh>
#include <EnergyPlus/ScheduleManager.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::DataPlant;
using namespace EnergyPlus::IceRink;
using namespace EnergyPlus::DataGlobalConstants;
using namespace EnergyPlus::ScheduleManager;

TEST_F(EnergyPlusFixture, IceRink_GetInput)
{
    std::string const idf_objects = delimited_string({
        "   IceRink:Indoor,                                                                ",
        "       Indoor Ice Rink,         !- Name                                           ",
        "       AlwaysOnSchedule,        !- Availability Schedule Name                     ",
        "       Main Floor,              !- Zone Name                                      ",
        "       Floor,                   !- Surface Name                                   ",
        "       0.013,                   !- Tube Diameter {m}                              ",
        "       100,                     !- Tube Length {m}                                ",
        "       STC,                     !- Ice Rink Control Strategy                      ",
        "       0.718,                   !- Maximum Refrigerant Volume Flow Rate {m3/s}    ",
        "       0.1,                     !- Minimum Refrigerant Volume Flow Rate {m3/s}    ",
        "       Rink Inlet Node,         !- Refrigerant Inlet Node Name                    ",
        "       Rink Outlet Node,        !- Refrigerant Outlet Node Name                   ",
        "       RefrigSched,             !- Refrigerant Outlet Temperature Schedule Name   ",
        "       IceSched,                !- Ice Surface Temperature Schedule Name          ",
        "       PeopleHGSched,           !- People Heat Gain Schedule Name                 ",
        "       PeopleSched,             !- People Schedule Name                           ",
        "       100,                     !- Maximum Number of People {dimensionless}       ",
        "       60,                      !- Rink Length {m}                                ",
        "       30,                      !- Rink Width {m}                                 ",
        "       1,                       !- Rink Depth {m}                                 ",
        "       0.0254,                  !- Ice Thickness {m}                              ",
        "       15;                      !- Flood Water Temperature {C}                    ",
    });

    ASSERT_TRUE(process_idf(idf_objects, false));

    GetIndoorIceRink();

    // For Ice Rink Input:
    EXPECT_EQ(Rink(NumOfRinks).Name, "INDOOR ICE RINK");
    EXPECT_EQ(Rink(NumOfRinks).SchedName, "ALWAYSONSCHEDULE");
    EXPECT_EQ(Rink(NumOfRinks).ZoneName, "MAIN FLOOR");
    EXPECT_EQ(Rink(NumOfRinks).SurfaceName, "FLOOR");
    EXPECT_EQ(Rink(NumOfRinks).TubeDiameter, 0.013);
    EXPECT_EQ(Rink(NumOfRinks).TubeLength, 100);
    EXPECT_EQ(Rink(NumOfRinks).ControlStrategy, SurfaceTempControl);
    EXPECT_EQ(Rink(NumOfRinks).MaxRefrigMassFlow, 0.718);
    EXPECT_EQ(Rink(NumOfRinks).MinRefrigMassFlow, 0.1);
    EXPECT_EQ(Rink(NumOfRinks).RefrigSetptSched, "REFRIGSCHED");
    EXPECT_EQ(Rink(NumOfRinks).IceSetptSched, "ICESCHED");
    EXPECT_EQ(Rink(NumOfRinks).PeopleHeatGainSchedName, "PEOPLEHGSCHED");
    EXPECT_EQ(Rink(NumOfRinks).PeopleSchedName, "PEOPLESCHED");
    EXPECT_EQ(Rink(NumOfRinks).MaxNumOfPeople, 100);
    EXPECT_EQ(Rink(NumOfRinks).LengthRink, 60);
    EXPECT_EQ(Rink(NumOfRinks).WidthRink, 30);
    EXPECT_EQ(Rink(NumOfRinks).DepthRink, 1);
    EXPECT_EQ(Rink(NumOfRinks).IceThickness, 0.0254);
    EXPECT_EQ(Rink(NumOfRinks).FloodWaterTemp, 15);
}

TEST_F(EnergyPlusFixture, IceRink_Resurfacer_GetInput)
{
    std::string const idf_objects = delimited_string({
        "IceRink:Resurfacer,                                                            ",
        "Resurfacer,              !- Name                                           ",
        "ResurfSched,             !- Resurfacing Schedule Name                      ",
        "1,                       !- Number Of Resurfacing Events                   ",
        "15,                      !- Resurfacing Water Temperature {C}              ",
        "10,                      !- Initial Water Temperature {C}                  ",
        "3;                       !- Resurfacer Tank Capacity                       ",
    });

    ASSERT_TRUE(process_idf(idf_objects, false));

    GetResurfacer();

    // For Resurfacer Input:
    EXPECT_EQ(Resurfacer(NumOfResurfacers).Name, "RESURFACER");
    EXPECT_EQ(Resurfacer(NumOfResurfacers).ResurfacingSchedName, "RESURFSCHED");
    EXPECT_EQ(Resurfacer(NumOfResurfacers).NoOfResurfEvents, 1);
    EXPECT_EQ(Resurfacer(NumOfResurfacers).ResurfacingWaterTemp, 15);
    EXPECT_EQ(Resurfacer(NumOfResurfacers).InitWaterTemp, 10);
    EXPECT_EQ(Resurfacer(NumOfResurfacers).TankCapacity, 3);
}

TEST_F(EnergyPlusFixture, IceRink_PeopleHG)
{
    Rink.allocate(1);
    Schedule.allocate(2);
    Schedule(1).CurrentValue = 1.0;
    Schedule(2).CurrentValue = 25;
    Rink(1).PeopleHeatGainSchedPtr = 1;
    Rink(1).PeopleSchedPtr = 2;
    Rink(1).Name = "INDOORICERINK";

    Real64 Q_people = Rink(1).PeopleHG();

    EXPECT_EQ(Q_people, 25.0);
}

TEST_F(EnergyPlusFixture, IceRink_Freezing)
{
    Real64 Q;
    Rink.allocate(1);
    Schedule.allocate(1);

    Rink(1).FloodWaterTemp = 15.0;
    Rink(1).WaterIndex = 1;
    Rink(1).LengthRink = 60.0;
    Rink(1).WidthRink = 30.0;
    Rink(1).IceThickness = 0.0254;
    Schedule(1).CurrentValue = -3.0;
    Rink(1).IceSetptSchedPtr = 1;

    Rink(1).IceRinkFreezing(Q);

    EXPECT_NEAR(Q, 18392544.07, 1);
}

TEST_F(EnergyPlusFixture, IceRink_Effectiveness)
{
    Rink.allocate(1);
    PlantLoop.allocate(1);

    Rink(1).LoopNum = 1;
    Rink(1).TubeDiameter = 0.05;
    Rink(1).TubeLength = 10.0;
    Rink(1).NumCircuits = 1;

    Real64 Temperature;
    Real64 MassFlowRate;
    Real64 MassFlowrate1;
    Temperature = 10.0;
    MassFlowRate = 0.1;
    MassFlowrate1 = 5;

    PlantLoop(Rink(1).LoopNum).FluidName = "WATER";
    PlantLoop(Rink(1).LoopNum).FluidIndex = 1;

    Real64 Result = Rink(1).calcEffectiveness(Temperature, MassFlowRate);

    EXPECT_NEAR(Result, 0.147, 0.1);

    Real64 Result1 = Rink(1).calcEffectiveness(Temperature, MassFlowrate1);

    EXPECT_NEAR(Result1, 0.319, 0.1);
}
