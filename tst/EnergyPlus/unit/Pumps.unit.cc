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

// EnergyPlus::Pumps Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

#include "Fixtures/EnergyPlusFixture.hh"
#include <DataPlant.hh>
#include <DataSizing.hh>
#include <Pumps.hh>
#include <SizingManager.hh>

namespace EnergyPlus {

TEST_F(EnergyPlusFixture, HeaderedVariableSpeedPumpSizingPowerTest)
{
    std::string const idf_objects = delimited_string({
        "Version,8.5;",

        "HeaderedPumps:VariableSpeed,",
        "Chilled Water Headered Pumps,  !- Name",
        "CW Supply Inlet Node,    !- Inlet Node Name",
        "CW Pumps Outlet Node,    !- Outlet Node Name",
        "0.001,                   !- Total Design Flow Rate {m3/s}",
        "2,                       !- Number of Pumps in Bank",
        "SEQUENTIAL,              !- Flow Sequencing Control Scheme",
        "100000,                  !- Design Pump Head {Pa}",
        "autosize,                !- Design Power Consumption {W}",
        "0.8,                     !- Motor Efficiency",
        "0.0,                     !- Fraction of Motor Inefficiencies to Fluid Stream",
        "0,                       !- Coefficient 1 of the Part Load Performance Curve",
        "1,                       !- Coefficient 2 of the Part Load Performance Curve",
        "0,                       !- Coefficient 3 of the Part Load Performance Curve",
        "0,                       !- Coefficient 4 of the Part Load Performance Curve",
        "0.1,                     !- Minimum Flow Rate Fraction",
        "INTERMITTENT,            !- Pump Control Type",
        "CoolingPumpAvailSched,   !- Pump Flow Rate Schedule Name",
        ",                        !- Zone Name",
        ",                        !- Skin Loss Radiative Fraction",
        "PowerPerFlowPerPressure, !- Design Power Sizing Method",
        ",                        !- Design Electric Power per Unit Flow Rate",
        "1.3;                     !- Design Shaft Power per Unit Flow Rate per Unit Head",
    });

    ASSERT_TRUE(process_idf(idf_objects));
    Pumps::GetPumpInput();
    Pumps::SizePump(1);
    EXPECT_NEAR(Pumps::PumpEquip(1).NomPowerUse, 162.5, 0.0001);
}

TEST_F(EnergyPlusFixture, HeaderedVariableSpeedPumpSizingPower22W_per_gpm)
{
    std::string const idf_objects = delimited_string({
        "Version,8.5;",

        "HeaderedPumps:VariableSpeed,",
        "Chilled Water Headered Pumps,  !- Name",
        "CW Supply Inlet Node,    !- Inlet Node Name",
        "CW Pumps Outlet Node,    !- Outlet Node Name",
        "0.001,                   !- Total Design Flow Rate {m3/s}",
        "2,                       !- Number of Pumps in Bank",
        "SEQUENTIAL,              !- Flow Sequencing Control Scheme",
        "100000,                  !- Design Pump Head {Pa}",
        "autosize,                !- Design Power Consumption {W}",
        "0.8,                     !- Motor Efficiency",
        "0.0,                     !- Fraction of Motor Inefficiencies to Fluid Stream",
        "0,                       !- Coefficient 1 of the Part Load Performance Curve",
        "1,                       !- Coefficient 2 of the Part Load Performance Curve",
        "0,                       !- Coefficient 3 of the Part Load Performance Curve",
        "0,                       !- Coefficient 4 of the Part Load Performance Curve",
        "0.1,                     !- Minimum Flow Rate Fraction",
        "INTERMITTENT,            !- Pump Control Type",
        "CoolingPumpAvailSched,   !- Pump Flow Rate Schedule Name",
        ",                        !- Zone Name",
        ",                        !- Skin Loss Radiative Fraction",
        "PowerPerFlow,            !- Design Power Sizing Method",
        ",                        !- Design Electric Power per Unit Flow Rate",
        ";                        !- Design Shaft Power per Unit Flow Rate per Unit Head",
    });

    ASSERT_TRUE(process_idf(idf_objects));
    Pumps::GetPumpInput();
    Pumps::SizePump(1);
    EXPECT_NEAR(Pumps::PumpEquip(1).NomPowerUse, 348.7011, 0.0001);
}

TEST_F(EnergyPlusFixture, HeaderedVariableSpeedPumpSizingPowerDefault)
{
    std::string const idf_objects = delimited_string({
        "Version,8.5;",

        "HeaderedPumps:VariableSpeed,",
        "Chilled Water Headered Pumps,  !- Name",
        "CW Supply Inlet Node,    !- Inlet Node Name",
        "CW Pumps Outlet Node,    !- Outlet Node Name",
        "0.001,                   !- Total Design Flow Rate {m3/s}",
        "2,                       !- Number of Pumps in Bank",
        "SEQUENTIAL,              !- Flow Sequencing Control Scheme",
        ",                        !- Design Pump Head {Pa}",
        "autosize,                !- Design Power Consumption {W}",
        ",                        !- Motor Efficiency",
        "0.0,                     !- Fraction of Motor Inefficiencies to Fluid Stream",
        "0,                       !- Coefficient 1 of the Part Load Performance Curve",
        "1,                       !- Coefficient 2 of the Part Load Performance Curve",
        "0,                       !- Coefficient 3 of the Part Load Performance Curve",
        "0,                       !- Coefficient 4 of the Part Load Performance Curve",
        "0.1,                     !- Minimum Flow Rate Fraction",
        "INTERMITTENT,            !- Pump Control Type",
        "CoolingPumpAvailSched,   !- Pump Flow Rate Schedule Name",
        ",                        !- Zone Name",
        ",                        !- Skin Loss Radiative Fraction",
        ",                        !- Design Power Sizing Method",
        ",                        !- Design Electric Power per Unit Flow Rate",
        ";                        !- Design Shaft Power per Unit Flow Rate per Unit Head",
    });

    ASSERT_TRUE(process_idf(idf_objects));
    Pumps::GetPumpInput();
    Pumps::SizePump(1);
    EXPECT_NEAR(Pumps::PumpEquip(1).NomPowerUse, 255.4872, 0.0001);
}

TEST_F(EnergyPlusFixture, HeaderedConstantSpeedPumpSizingPowerTest)
{
    std::string const idf_objects = delimited_string({
        "Version,8.5;",

        "HeaderedPumps:ConstantSpeed,",
        "Chilled Water Headered Pumps,  !- Name",
        "CW Supply Inlet Node,    !- Inlet Node Name",
        "CW Pumps Outlet Node,    !- Outlet Node Name",
        "0.001,                   !- Total Design Flow Rate {m3/s}",
        "2,                       !- Number of Pumps in Bank",
        "SEQUENTIAL,              !- Flow Sequencing Control Scheme",
        "100000,                  !- Design Pump Head {Pa}",
        "autosize,                !- Design Power Consumption {W}",
        "0.8,                     !- Motor Efficiency",
        "0.0,                     !- Fraction of Motor Inefficiencies to Fluid Stream",
        "INTERMITTENT,            !- Pump Control Type",
        "CoolingPumpAvailSched,   !- Pump Flow Rate Schedule Name",
        ",                        !- Zone Name",
        ",                        !- Skin Loss Radiative Fraction",
        "PowerPerFlowPerPressure, !- Design Power Sizing Method",
        ",                        !- Design Electric Power per Unit Flow Rate",
        "1.3;                     !- Design Shaft Power per Unit Flow Rate per Unit Head",
    });

    ASSERT_TRUE(process_idf(idf_objects));
    Pumps::GetPumpInput();
    Pumps::SizePump(1);
    EXPECT_NEAR(Pumps::PumpEquip(1).NomPowerUse, 162.5, 0.0001);
}

TEST_F(EnergyPlusFixture, HeaderedConstantSpeedPumpSizingPower19W_per_gpm)
{
    std::string const idf_objects = delimited_string({
        "Version,8.5;",

        "HeaderedPumps:ConstantSpeed,",
        "Chilled Water Headered Pumps,  !- Name",
        "CW Supply Inlet Node,    !- Inlet Node Name",
        "CW Pumps Outlet Node,    !- Outlet Node Name",
        "0.001,                   !- Total Design Flow Rate {m3/s}",
        "2,                       !- Number of Pumps in Bank",
        "SEQUENTIAL,              !- Flow Sequencing Control Scheme",
        ",                        !- Design Pump Head {Pa}",
        "autosize,                !- Design Power Consumption {W}",
        ",                        !- Motor Efficiency",
        "0.0,                     !- Fraction of Motor Inefficiencies to Fluid Stream",
        "INTERMITTENT,            !- Pump Control Type",
        "CoolingPumpAvailSched,   !- Pump Flow Rate Schedule Name",
        ",                        !- Zone Name",
        ",                        !- Skin Loss Radiative Fraction",
        "PowerPerFlow,            !- Design Power Sizing Method",
        "301156.1,                !- Design Electric Power per Unit Flow Rate",
        ";                        !- Design Shaft Power per Unit Flow Rate per Unit Head",
    });

    ASSERT_TRUE(process_idf(idf_objects));
    Pumps::GetPumpInput();
    Pumps::SizePump(1);
    EXPECT_NEAR(Pumps::PumpEquip(1).NomPowerUse, 301.1561, 0.0001);
}

TEST_F(EnergyPlusFixture, HeaderedConstantSpeedPumpSizingPowerDefault)
{
    std::string const idf_objects = delimited_string({
        "Version,8.5;",

        "HeaderedPumps:ConstantSpeed,",
        "Chilled Water Headered Pumps,  !- Name",
        "CW Supply Inlet Node,    !- Inlet Node Name",
        "CW Pumps Outlet Node,    !- Outlet Node Name",
        "0.001,                   !- Total Design Flow Rate {m3/s}",
        "2,                       !- Number of Pumps in Bank",
        "SEQUENTIAL,              !- Flow Sequencing Control Scheme",
        ",                        !- Design Pump Head {Pa}",
        "autosize,                !- Design Power Consumption {W}",
        ",                        !- Motor Efficiency",
        "0.0,                     !- Fraction of Motor Inefficiencies to Fluid Stream",
        "INTERMITTENT,            !- Pump Control Type",
        "CoolingPumpAvailSched,   !- Pump Flow Rate Schedule Name",
        ",                        !- Zone Name",
        ",                        !- Skin Loss Radiative Fraction",
        ",                        !- Design Power Sizing Method",
        ",                        !- Design Electric Power per Unit Flow Rate",
        ";                        !- Design Shaft Power per Unit Flow Rate per Unit Head",
    });

    ASSERT_TRUE(process_idf(idf_objects));
    Pumps::GetPumpInput();
    Pumps::SizePump(1);
    EXPECT_NEAR(Pumps::PumpEquip(1).NomPowerUse, 255.4872, 0.0001);
}

TEST_F(EnergyPlusFixture, VariableSpeedPumpSizingMinVolFlowRate)
{
    std::string const idf_objects = delimited_string({
        "Version,8.5;",

        "Pump:VariableSpeed,",
        "CoolSys1 Pump,           !- Name",
        "CoolSys1 Supply Inlet Node,  !- Inlet Node Name",
        "CoolSys1 Pump-CoolSys1 ChillerNodeviaConnector,  !- Outlet Node Name",
        "0.001,                !- Design Flow Rate {m3/s}",
        "100000,                  !- Design Pump Head {Pa}",
        "AUTOSIZE,                !- Design Power Consumption {W}",
        "0.8,                     !- Motor Efficiency",
        "0.0,                     !- Fraction of Motor Inefficiencies to Fluid Stream",
        "0,                       !- Coefficient 1 of the Part Load Performance Curve",
        "1,                       !- Coefficient 2 of the Part Load Performance Curve",
        "0,                       !- Coefficient 3 of the Part Load Performance Curve",
        "0,                       !- Coefficient 4 of the Part Load Performance Curve",
        "autosize,                !- Minimum Flow Rate {m3/s}",
        "Intermittent,            !- Pump Control Type",
        ",                        !- Pump Flow Rate Schedule Name",
        ",                        !- Pump Curve Name",
        ",                        !- Impeller Diameter",
        ",                        !- VFD Control Type",
        ",                        !- Pump rpm Schedule Name",
        ",                        !- Minimum Pressure Schedule",
        ",                        !- Maximum Pressure Schedule",
        ",                        !- Minimum RPM Schedule",
        ",                        !- Maximum RPM Schedule",
        ",                        !- Zone Name",
        ",                        !- Skin Loss Radiative Fraction",
        "PowerPerFlowPerPressure, !- Design Power Sizing Method",
        ",                        !- Design Electric Power per Unit Flow Rate",
        "1.3,                     !- Design Shaft Power per Unit Flow Rate per Unit Head",
        "0.3;                        !- Design Minimum Flow Rate Sizing Factor",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    Pumps::GetPumpInput();
    EXPECT_NEAR(Pumps::PumpEquip(1).MinVolFlowRate, DataSizing::AutoSize, 0.000001);
    Pumps::SizePump(1);
    EXPECT_NEAR(Pumps::PumpEquip(1).MinVolFlowRate, 0.0003, 0.00001);
}

TEST_F(EnergyPlusFixture, VariableSpeedPumpSizingPowerPerPressureTest)
{
    std::string const idf_objects = delimited_string({
        "Version,8.5;",

        "Pump:VariableSpeed,",
        "CoolSys1 Pump,           !- Name",
        "CoolSys1 Supply Inlet Node,  !- Inlet Node Name",
        "CoolSys1 Pump-CoolSys1 ChillerNodeviaConnector,  !- Outlet Node Name",
        "0.001,                !- Design Flow Rate {m3/s}",
        "100000,                  !- Design Pump Head {Pa}",
        "AUTOSIZE,                !- Design Power Consumption {W}",
        "0.8,                     !- Motor Efficiency",
        "0.0,                     !- Fraction of Motor Inefficiencies to Fluid Stream",
        "0,                       !- Coefficient 1 of the Part Load Performance Curve",
        "1,                       !- Coefficient 2 of the Part Load Performance Curve",
        "0,                       !- Coefficient 3 of the Part Load Performance Curve",
        "0,                       !- Coefficient 4 of the Part Load Performance Curve",
        "autosize,                !- Minimum Flow Rate {m3/s}",
        "Intermittent,            !- Pump Control Type",
        ",                        !- Pump Flow Rate Schedule Name",
        ",                        !- Pump Curve Name",
        ",                        !- Impeller Diameter",
        ",                        !- VFD Control Type",
        ",                        !- Pump rpm Schedule Name",
        ",                        !- Minimum Pressure Schedule",
        ",                        !- Maximum Pressure Schedule",
        ",                        !- Minimum RPM Schedule",
        ",                        !- Maximum RPM Schedule",
        ",                        !- Zone Name",
        ",                        !- Skin Loss Radiative Fraction",
        "PowerPerFlowPerPressure, !- Design Power Sizing Method",
        ",                        !- Design Electric Power per Unit Flow Rate",
        "1.3,                     !- Design Shaft Power per Unit Flow Rate per Unit Head",
        ";                        !- Design Minimum Flow Rate Sizing Factor",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    Pumps::GetPumpInput();
    Pumps::SizePump(1);
    EXPECT_NEAR(Pumps::PumpEquip(1).NomPowerUse, 162.5, 0.0001);
}

TEST_F(EnergyPlusFixture, VariableSpeedPumpSizingPowerDefault)
{
    std::string const idf_objects = delimited_string({
        "Version,8.5;",

        "Pump:VariableSpeed,",
        "CoolSys1 Pump,           !- Name",
        "CoolSys1 Supply Inlet Node,  !- Inlet Node Name",
        "CoolSys1 Pump-CoolSys1 ChillerNodeviaConnector,  !- Outlet Node Name",
        "0.001,                   !- Design Flow Rate {m3/s}",
        ",                        !- Design Pump Head {Pa}",
        "AUTOSIZE,                !- Design Power Consumption {W}",
        ",                        !- Motor Efficiency",
        "0.0,                     !- Fraction of Motor Inefficiencies to Fluid Stream",
        "0,                       !- Coefficient 1 of the Part Load Performance Curve",
        "1,                       !- Coefficient 2 of the Part Load Performance Curve",
        "0,                       !- Coefficient 3 of the Part Load Performance Curve",
        "0,                       !- Coefficient 4 of the Part Load Performance Curve",
        "autosize,                !- Minimum Flow Rate {m3/s}",
        "Intermittent,            !- Pump Control Type",
        ",                        !- Pump Flow Rate Schedule Name",
        ",                        !- Pump Curve Name",
        ",                        !- Impeller Diameter",
        ",                        !- VFD Control Type",
        ",                        !- Pump rpm Schedule Name",
        ",                        !- Minimum Pressure Schedule",
        ",                        !- Maximum Pressure Schedule",
        ",                        !- Minimum RPM Schedule",
        ",                        !- Maximum RPM Schedule",
        ",                        !- Zone Name",
        ",                        !- Skin Loss Radiative Fraction",
        ",                        !- Design Power Sizing Method",
        ",                        !- Design Electric Power per Unit Flow Rate",
        ",                        !- Design Shaft Power per Unit Flow Rate per Unit Head",
        ";                        !- Design Minimum Flow Rate Sizing Factor",
    });

    ASSERT_TRUE(process_idf(idf_objects));
    Pumps::GetPumpInput();
    Pumps::SizePump(1);
    EXPECT_NEAR(Pumps::PumpEquip(1).NomPowerUse, 255.4872, 0.0001);
}

TEST_F(EnergyPlusFixture, VariableSpeedPumpSizingPower22W_per_GPM)
{
    std::string const idf_objects = delimited_string({
        "Version,8.5;",

        "Pump:VariableSpeed,",
        "CoolSys1 Pump,           !- Name",
        "CoolSys1 Supply Inlet Node,  !- Inlet Node Name",
        "CoolSys1 Pump-CoolSys1 ChillerNodeviaConnector,  !- Outlet Node Name",
        "0.001,                   !- Design Flow Rate {m3/s}",
        "179352,                  !- Design Pump Head {Pa}",
        "AUTOSIZE,                !- Design Power Consumption {W}",
        "0.9,                     !- Motor Efficiency",
        "0.0,                     !- Fraction of Motor Inefficiencies to Fluid Stream",
        "0,                       !- Coefficient 1 of the Part Load Performance Curve",
        "1,                       !- Coefficient 2 of the Part Load Performance Curve",
        "0,                       !- Coefficient 3 of the Part Load Performance Curve",
        "0,                       !- Coefficient 4 of the Part Load Performance Curve",
        "autosize,                !- Minimum Flow Rate {m3/s}",
        "Intermittent,            !- Pump Control Type",
        ",                        !- Pump Flow Rate Schedule Name",
        ",                        !- Pump Curve Name",
        ",                        !- Impeller Diameter",
        ",                        !- VFD Control Type",
        ",                        !- Pump rpm Schedule Name",
        ",                        !- Minimum Pressure Schedule",
        ",                        !- Maximum Pressure Schedule",
        ",                        !- Minimum RPM Schedule",
        ",                        !- Maximum RPM Schedule",
        ",                        !- Zone Name",
        ",                        !- Skin Loss Radiative Fraction",
        "PowerPerFlow,            !- Design Power Sizing Method",
        ",                        !- Design Electric Power per Unit Flow Rate",
        ",                        !- Design Shaft Power per Unit Flow Rate per Unit Head",
        "0.0;                     !- Design Minimum Flow Rate Sizing Factor",
    });
    ASSERT_TRUE(process_idf(idf_objects));
    Pumps::GetPumpInput();
    Pumps::SizePump(1);
    EXPECT_NEAR(Pumps::PumpEquip(1).NomPowerUse, 348.7011, 0.0001);
}

TEST_F(EnergyPlusFixture, ConstantSpeedPumpSizingPower19W_per_gpm)
{
    std::string const idf_objects = delimited_string({
        "Version,8.5;",

        "Pump:ConstantSpeed,",
        "TowerWaterSys Pump,      !- Name",
        "TowerWaterSys Supply Inlet Node,  !- Inlet Node Name",
        "TowerWaterSys Pump-TowerWaterSys CoolTowerNodeviaConnector,  !- Outlet Node Name",
        "0.001,                   !- Design Flow Rate {m3/s}",
        "179352,                  !- Design Pump Head {Pa}",
        "AUTOSIZE,                !- Design Power Consumption {W}",
        "0.87,                    !- Motor Efficiency",
        "0.0,                     !- Fraction of Motor Inefficiencies to Fluid Stream",
        "Intermittent,            !- Pump Control Type",
        ",                        !- Pump Flow Rate Schedule Name",
        ",                        !- Pump Curve Name",
        ",                        !- Impeller Diameter",
        ",                        !- Rotational Speed",
        ",                        !- Zone Name",
        ",                        !- Skin Loss Radiative Fraction",
        "PowerPerFlow,            !- Design Power Sizing Method",
        "301156.1,                !- Design Electric Power per Unit Flow Rate",
        ";                        !- Design Shaft Power per Unit Flow Rate per Unit Head",
    });

    ASSERT_TRUE(process_idf(idf_objects));
    Pumps::GetPumpInput();
    Pumps::SizePump(1);
    EXPECT_NEAR(Pumps::PumpEquip(1).NomPowerUse, 301.1561, 0.0001);
}

TEST_F(EnergyPlusFixture, ConstantSpeedPumpSizingPowerPerPressureTest)
{
    std::string const idf_objects = delimited_string({
        "Version,8.5;",

        "Pump:ConstantSpeed,",
        "TowerWaterSys Pump,      !- Name",
        "TowerWaterSys Supply Inlet Node,  !- Inlet Node Name",
        "TowerWaterSys Pump-TowerWaterSys CoolTowerNodeviaConnector,  !- Outlet Node Name",
        "0.001,                   !- Design Flow Rate {m3/s}",
        "100000,                  !- Design Pump Head {Pa}",
        "AUTOSIZE,                !- Design Power Consumption {W}",
        "0.8,                     !- Motor Efficiency",
        "0.0,                     !- Fraction of Motor Inefficiencies to Fluid Stream",
        "Intermittent,            !- Pump Control Type",
        ",                        !- Pump Flow Rate Schedule Name",
        ",                        !- Pump Curve Name",
        ",                        !- Impeller Diameter",
        ",                        !- Rotational Speed",
        ",                        !- Zone Name",
        ",                        !- Skin Loss Radiative Fraction",
        "PowerPerFlowPerPressure, !- Design Power Sizing Method",
        ",                        !- Design Electric Power per Unit Flow Rate",
        "1.3;                     !- Design Shaft Power per Unit Flow Rate per Unit Head",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    Pumps::GetPumpInput();
    Pumps::SizePump(1);
    EXPECT_NEAR(Pumps::PumpEquip(1).NomPowerUse, 162.5, 0.0001);
}

TEST_F(EnergyPlusFixture, ConstantSpeedPumpSizingPowerDefaults)
{
    std::string const idf_objects = delimited_string({
        "Version,8.5;",

        "Pump:ConstantSpeed,",
        "TowerWaterSys Pump,      !- Name",
        "TowerWaterSys Supply Inlet Node,  !- Inlet Node Name",
        "TowerWaterSys Pump-TowerWaterSys CoolTowerNodeviaConnector,  !- Outlet Node Name",
        "0.001,                   !- Design Flow Rate {m3/s}",
        ",                        !- Design Pump Head {Pa}",
        "AUTOSIZE,                !- Design Power Consumption {W}",
        ",                        !- Motor Efficiency",
        "0.0,                     !- Fraction of Motor Inefficiencies to Fluid Stream",
        "Intermittent,            !- Pump Control Type",
        ",                        !- Pump Flow Rate Schedule Name",
        ",                        !- Pump Curve Name",
        ",                        !- Impeller Diameter",
        ",                        !- Rotational Speed",
        ",                        !- Zone Name",
        ",                        !- Skin Loss Radiative Fraction",
        ",                        !- Design Power Sizing Method",
        ",                        !- Design Electric Power per Unit Flow Rate",
        ";                        !- Design Shaft Power per Unit Flow Rate per Unit Head",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    Pumps::GetPumpInput();
    Pumps::SizePump(1);
    EXPECT_NEAR(Pumps::PumpEquip(1).NomPowerUse, 255.4872, 0.0001);
}

TEST_F(EnergyPlusFixture, CondensatePumpSizingPowerDefaults)
{
    std::string const idf_objects = delimited_string({
        "Version,8.5;",

        "Pump:VariableSpeed:Condensate,",
        "Steam Boiler Plant Steam Circ Pump,  !- Name",
        "Steam Boiler Plant Steam Supply Inlet Node,  !- Inlet Node Name",
        "Steam Boiler Plant Steam Pump Outlet Node,  !- Outlet Node Name",
        "1.0,                     !- Design Flow Rate {m3/s}",
        ",                        !- Design Pump Head {Pa}",
        "autosize,                !- Design Power Consumption {W}",
        ",                        !- Motor Efficiency",
        "0.0,                     !- Fraction of Motor Inefficiencies to Fluid Stream",
        "0,                       !- Coefficient 1 of the Part Load Performance Curve",
        "1,                       !- Coefficient 2 of the Part Load Performance Curve",
        "0,                       !- Coefficient 3 of the Part Load Performance Curve",
        "0,                       !- Coefficient 4 of the Part Load Performance Curve",
        ",                        !- Pump Flow Rate Schedule Name",
        ",                        !- Zone Name",
        ",                        !- Skin Loss Radiative Fraction",
        ",                        !- Design Power Sizing Method",
        ",                        !- Design Electric Power per Unit Flow Rate",
        ";                        !- Design Shaft Power per Unit Flow Rate per Unit Head",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    Pumps::GetPumpInput();
    Pumps::SizePump(1);
    EXPECT_NEAR(Pumps::PumpEquip(1).NomPowerUse, 153.3, 0.1);
}

TEST_F(EnergyPlusFixture, CondensatePumpSizingPower19W_per_gpm)
{
    std::string const idf_objects = delimited_string({
        "Version,8.5;",

        "Pump:VariableSpeed:Condensate,",
        "Steam Boiler Plant Steam Circ Pump,  !- Name",
        "Steam Boiler Plant Steam Supply Inlet Node,  !- Inlet Node Name",
        "Steam Boiler Plant Steam Pump Outlet Node,  !- Outlet Node Name",
        "1.0,                     !- Design Flow Rate {m3/s}",
        "179352,                  !- Design Pump Head {Pa}",
        "autosize,                !- Design Power Consumption {W}",
        "0.9,                     !- Motor Efficiency",
        "0.0,                     !- Fraction of Motor Inefficiencies to Fluid Stream",
        "0,                       !- Coefficient 1 of the Part Load Performance Curve",
        "1,                       !- Coefficient 2 of the Part Load Performance Curve",
        "0,                       !- Coefficient 3 of the Part Load Performance Curve",
        "0,                       !- Coefficient 4 of the Part Load Performance Curve",
        ",                        !- Pump Flow Rate Schedule Name",
        ",                        !- Zone Name",
        ",                        !- Skin Loss Radiative Fraction",
        "PowerPerFlow,            !- Design Power Sizing Method",
        "301156.1,                !- Design Electric Power per Unit Flow Rate",
        ";                        !- Design Shaft Power per Unit Flow Rate per Unit Head",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    Pumps::GetPumpInput();
    Pumps::SizePump(1);
    EXPECT_NEAR(Pumps::PumpEquip(1).NomPowerUse, 180.7, 0.1);
}

TEST_F(EnergyPlusFixture, CondensatePumpSizingPowerTest)
{
    std::string const idf_objects = delimited_string({
        "Version,8.5;",

        "Pump:VariableSpeed:Condensate,",
        "Steam Boiler Plant Steam Circ Pump,  !- Name",
        "Steam Boiler Plant Steam Supply Inlet Node,  !- Inlet Node Name",
        "Steam Boiler Plant Steam Pump Outlet Node,  !- Outlet Node Name",
        "1.0,                     !- Design Flow Rate {m3/s}",
        "100000,                  !- Design Pump Head {Pa}",
        "autosize,                !- Design Power Consumption {W}",
        "0.8,                     !- Motor Efficiency",
        "0.0,                     !- Fraction of Motor Inefficiencies to Fluid Stream",
        "0,                       !- Coefficient 1 of the Part Load Performance Curve",
        "1,                       !- Coefficient 2 of the Part Load Performance Curve",
        "0,                       !- Coefficient 3 of the Part Load Performance Curve",
        "0,                       !- Coefficient 4 of the Part Load Performance Curve",
        ",                        !- Pump Flow Rate Schedule Name",
        ",                        !- Zone Name",
        ",                        !- Skin Loss Radiative Fraction",
        "PowerPerFlowPerPressure, !- Design Power Sizing Method",
        ",                        !- Design Electric Power per Unit Flow Rate",
        "1.3;                     !- Design Shaft Power per Unit Flow Rate per Unit Head",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    Pumps::GetPumpInput();
    Pumps::SizePump(1);
    EXPECT_NEAR(Pumps::PumpEquip(1).NomPowerUse, 97.5, 0.1);
}

} // namespace EnergyPlus