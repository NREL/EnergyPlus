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

// EnergyPlus::HVACDXSystem and VariableSpeedCoils Unit Tests

// Google Test Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/HVACDXSystem.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/VariableSpeedCoils.hh>
#include <gtest/gtest.h>

namespace EnergyPlus {

TEST_F(EnergyPlusFixture, VariableSpeedCoils_DOASDXCoilTest)
{
    // issue #6040
    std::string const idf_objects = delimited_string({

        "  Schedule:Compact,",
        "    FANANDCOILAVAILSCHED,    !- Name",
        "    FRACTION,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 19",
        "    For: Alldays,            !- Field 20",
        "    Until: 24:00,1.00;       !- Field 21",

        "  CoilSystem:Cooling:DX,",
        "    DX Cooling Coil System,  !- Name",
        "    FanAndCoilAvailSched,    !- Availability Schedule Name",
        "    DX Cooling Coil Air Inlet Node,  !- DX Cooling Coil System Inlet Node Name",
        "    Heating Coil Air Inlet Node,  !- DX Cooling Coil System Outlet Node Name",
        "    Heating Coil Air Inlet Node,  !- DX Cooling Coil System Sensor Node Name",
        "    Coil:Cooling:DX:VariableSpeed,!- Cooling Coil Object Type",
        "    VS DX Cooling Coil,      !- Cooling Coil Name",
        "    None,                    !- Dehumidification Control Type",
        "    yes,                     !- Run on Sensible Load",
        "    No,                      !- Run on Latent Load",
        "    Yes;                     !- Use Outdoor Air DX Cooling Coil",

        "  Coil:Cooling:DX:VariableSpeed,",
        "    VS DX Cooling Coil,              !- Name",
        "    DX Cooling Coil Air Inlet Node,  !- Air Inlet Node Name",
        "    Heating Coil Air Inlet Node,     !- Air Outlet Node Name",
        "    5,                       !- Number of Speeds {dimensionless}",
        "    5,                       !- Nominal Speed Level {dimensionless}",
        "    autosize,                !- Rated Total Cooling Capacity At Selected Nominal Speed Level {w}",
        "    autosize,                !- Rated Volumetric Air Flow Rate At Selected Nominal Speed Level {m3/s}",
        "    0,                       !- Nominal Time for Condensate to Begin Leaving the Coil {s}",
        "    0,                       !- Initial Moisture Evaporation Rate Divided by Steady-State AC Latent Capacity {dimensionless}",
        "    PLF Curve MultiComp 30% unloading, !- Energy Part Load Fraction Curve Name",
        "    ,                        !- Condenser Air Inlet Node Name",
        "    AirCooled,               !- Condenser Type",
        "    ,                        !- Evaporative Condenser Pump Rated Power Consumption {W}",
        "    ,                        !- Crankcase Heater Capacity {W}",
        "    10,                      !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
        "    ,                        !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "    ,                        !- Supply Water Storage Tank Name",
        "    ,                        !- Condensate Collection Water Storage Tank Name",
        "    ,                        !- Basin Heater Capacity {W/K}",
        "    2,                       !- Basin Heater Setpoint Temperature {C}",
        "    ,                        !- Basin Heater Operating Schedule Name",
        "    33861.72,                !- Speed 1 Reference Unit Total Cooling Capacity At Rated Conditions {w}",
        "    0.70,                    !- Speed 1 Reference Unit Sensible Heat Ratio At Rated Conditions {dimensionless}",
        "    4.328973,                !- Speed 1 Reference Unit COP At Rated Conditions {dimensionless}",
        "    1.396964,                !- Speed 1 Reference Unit Air Flow Rate At Rated Conditions {m3/s}",
        "    ,                        !- Speed 1 Reference Unit Condenser Flow Rate at Rated Conditions {m3/s}",
        "    ,                        !- Speed 1 Reference Unit Pad Effectiveness of Evap Precooling at Rated Conditions {dimensionless}",
        "    5CapacityCurveIpakCS,    !- Speed 1 Total Cooling Capacity Function of Temperature Curve Name",
        "    CAPFF Curve IpakCS,      !- Speed 1 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "    5PowerCurveIpakCS,       !- Speed 1 Energy Input Ratio Function of Temperature Curve Name",
        "    EIRFF Curve IpakCS,      !- Speed 1 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "    35516.08,                !- Speed 2 Reference Unit Total Cooling Capacity At Rated Conditions {w}",
        "    0.78,                    !- Speed 2 Reference Unit Sensible Heat Ratio At Rated Conditions {dimensionless}",
        "    4.540061,                !- Speed 2 Reference Unit COP At Rated Conditions {dimensionless}",
        "    1.88779,                 !- Speed 2 Reference Unit Air Flow Rate At Rated Conditions {m3/s}",
        "    ,                        !- Speed 2 Reference Unit Condenser Flow Rate at Rated Conditions {m3/s}",
        "    ,                        !- Speed 2 Reference Unit Pad Effectiveness of Evap Precooling at Rated Conditions {dimensionless}",
        "    4CapacityCurve,          !- Speed 2 Total Cooling Capacity Function of Temperature Curve Name",
        "    CAPFF Curve IpakCS,      !- Speed 2 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "    4PowerCurveIpakCS,       !- Speed 2 Energy Input Ratio Function of Temperature Curve Name",
        "    EIRFF Curve IpakCS,      !- Speed 2 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "    65133.17,                !- Speed 3 Reference Unit Total Cooling Capacity At Rated Conditions {w}",
        "    0.70,                    !- Speed 3 Reference Unit Sensible Heat Ratio At Rated Conditions {dimensionless}",
        "    4.164418,                !- Speed 3 Reference Unit COP At Rated Conditions {dimensionless}",
        "    2.831685,                !- Speed 3 Reference Unit Air Flow Rate At Rated Conditions {m3/s}",
        "    ,                        !- Speed 3 Reference Unit Condenser Flow Rate at Rated Conditions {m3/s}",
        "    ,                        !- Speed 3 Reference Unit Pad Effectiveness of Evap Precooling at Rated Conditions {dimensionless}",
        "    3CapacityCurveIpakCS,    !- Speed 3 Total Cooling Capacity Function of Temperature Curve Name",
        "    CAPFF Curve IpakCS,      !- Speed 3 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "    3PowerCurveIpakCS,       !- Speed 3 Energy Input Ratio Function of Temperature Curve Name",
        "    EIRFF Curve IpakCS,      !- Speed 3 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "    119583.3,                !- Speed 4 Reference Unit Total Cooling Capacity At Rated Conditions {w}",
        "    0.62,                    !- Speed 4 Reference Unit Sensible Heat Ratio At Rated Conditions {dimensionless}",
        "    3.469661,                !- Speed 4 Reference Unit COP At Rated Conditions {dimensionless}",
        "    3.553764,                !- Speed 4 Reference Unit Air Flow Rate At Rated Conditions {m3/s}",
        "    ,                        !- Speed 4 Reference Unit Condenser Flow Rate at Rated Conditions {m3/s}",
        "    ,                        !- Speed 4 Reference Unit Pad Effectiveness of Evap Precooling at Rated Conditions {dimensionless}",
        "    2CapacityCurveIpakCS,    !- Speed 4 Total Cooling Capacity Function of Temperature Curve Name",
        "    CAPFF Curve IpakCS,      !- Speed 4 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "    2PowerCurveIpakCS ,      !- Speed 4 Energy Input Ratio Function of Temperature Curve Name",
        "    EIRFF Curve IpakCS,      !- Speed 4 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "    132769.7,                !- Speed 5 Reference Unit Total Cooling Capacity At Rated Conditions {w}",
        "    0.69,                    !- Speed 5 Reference Unit Sensible Heat Ratio At Rated Conditions {dimensionless}",
        "    3.822957,                !- Speed 5 Reference Unit COP At Rated Conditions {dimensionless}",
        "    5.66336932,              !- Speed 5 Reference Unit Air Flow Rate At Rated Conditions {m3/s}",
        "    ,                        !- Speed 5 Reference Unit Condenser Flow Rate at Rated Conditions {m3/s}",
        "    ,                        !- Speed 5 Reference Unit Pad Effectiveness of Evap Precooling at Rated Conditions {dimensionless}",
        "    1CapacityCurveIpakCS,    !- Speed 5 Total Cooling Capacity Function of Temperature Curve Name",
        "    CAPFF Curve IpakCS,      !- Speed 5 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "    1PowerCurveIpakCS,       !- Speed 5 Energy Input Ratio Function of Temperature Curve Name",
        "    EIRFF Curve IpakCS;      !- Speed 5 Energy Input Ratio Function of Air Flow Fraction Curve Name",

        "Curve:Quadratic,",
        "    PLF Curve MultiComp 30% unloading,  !- Name",
        "    0.85,                    !- Coefficient1 Constant",
        "    0.8333,                  !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.0,                     !- Minimum Value of x",
        "    0.3,                     !- Maximum Value of x",
        "    0.85,                    !- Minimum Curve Output",
        "    1.0,                     !- Maximum Curve Output",
        "    Dimensionless,           !- Input Unit Type for X",
        "    Dimensionless;           !- Output Unit Type",

        "Curve:Biquadratic,",
        "    4CapacityCurve,          !- Name",
        "    1.16,                    !- Coefficient1 Constant",
        "    -.0155,                  !- Coefficient2 x",
        "    0.00128,                 !- Coefficient3 x**2",
        "    -.00673,                 !- Coefficient4 y",
        "    0.0000797,               !- Coefficient5 y**2",
        "    -.000305,                !- Coefficient6 x*y",
        "    10,                      !- Minimum Value of x",
        "    23.88889,                !- Maximum Value of x",
        "    23.88889,                !- Minimum Value of y",
        "    40.55556,                !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Dimensionless,           !- Input Unit Type for X",
        "    Dimensionless,           !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "Curve:Cubic,",
        "    CAPFF Curve IpakCS,      !- Name",
        "    1,                       !- Coefficient1 Constant",
        "    0,                       !- Coefficient2 x",
        "    0,                       !- Coefficient3 x**2",
        "    0,                       !- Coefficient4 x**3",
        "    0,                       !- Minimum Value of x",
        "    1,                       !- Maximum Value of x",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Dimensionless,           !- Input Unit Type for X",
        "    Dimensionless;           !- Output Unit Type",

        "Curve:Cubic,",
        "    EIRFF Curve IpakCS,      !- Name",
        "    1,                       !- Coefficient1 Constant",
        "    0,                       !- Coefficient2 x",
        "    0,                       !- Coefficient3 x**",
        "    0,                       !- Coefficient4 x**3",
        "    0,                       !- Minimum Value of x",
        "    1,                       !- Maximum Value of x",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Dimensionless,           !- Input Unit Type for X",
        "    Dimensionless;           !- Output Unit Type",

        "Curve:Biquadratic,",
        "    1CapacityCurveIpakCS,    !- Name",
        "    0.483,                   !- Coefficient1 Constant",
        "    0.0305,                  !- Coefficient2 x",
        "    0.0000458,               !- Coefficient3 x**2",
        "    0.00511,                 !- Coefficient4 y",
        "    -1.50E-04,               !- Coefficient5 y**2",
        "    -1.28E-04,               !- Coefficient6 x*y",
        "    8.88889,                 !- Minimum Value of x",
        "    21.6667,                 !- Maximum Value of x",
        "    12.7778,                 !- Minimum Value of y",
        "    51.6667,                 !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "Curve:Biquadratic,",
        "    1PowerCurveIpakCS,       !- Name",
        "    1.33E+00,                !- Coefficient1 Constant",
        "    -3.40E-02,               !- Coefficient2 x",
        "    0.000939,                !- Coefficient3 x**2",
        "    -0.00858,                !- Coefficient4 y",
        "    0.000769,                !- Coefficient5 y**2",
        "    -0.000972,               !- Coefficient6 x*y",
        "    8.88889,                 !- Minimum Value of x",
        "    21.6667,                 !- Maximum Value of x",
        "    12.7778,                 !- Minimum Value of y",
        "    51.6667,                 !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "Curve:Biquadratic,",
        "    2CapacityCurveIpakCS,    !- Name",
        "    0.515,                   !- Coefficient1 Constant",
        "    0.026,                   !- Coefficient2 x",
        "    0.000275,                !- Coefficient3 x**2",
        "    0.00377,                 !- Coefficient4 y",
        "    -0.000115,               !- Coefficient5 y**2",
        "    -0.00017,                !- Coefficient6 x*y",
        "    8.88889,                 !- Minimum Value of x",
        "    21.6667,                 !- Maximum Value of x",
        "    12.7778,                 !- Minimum Value of y",
        "    51.6667,                 !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Dimensionless,           !- Input Unit Type for X",
        "    Dimensionless,           !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "Curve:Biquadratic,",
        "    2PowerCurveIpakCS,       !- Name",
        "    1.41,                    !- Coefficient1 Constant",
        "    -0.0457,                 !- Coefficient2 x",
        "    0.00116,                 !- Coefficient3 x**2",
        "    -0.00625,                !- Coefficient4 y",
        "    0.000707,                !- Coefficient5 y**2",
        "    -0.000889,               !- Coefficient6 x*y",
        "    8.88889,                 !- Minimum Value of x",
        "    21.6667,                 !- Maximum Value of x",
        "    12.7778,                 !- Minimum Value of y",
        "    51.6667,                 !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Dimensionless,           !- Input Unit Type for X",
        "    Dimensionless,           !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "Curve:Biquadratic,",
        "    3CapacityCurveIpakCS,    !- Name",
        "    0.5,                     !- Coefficient1 Constant",
        "    0.0316,                  !- Coefficient2 x",
        "    0.00013,                 !- Coefficient3 x**2",
        "    0.00213,                 !- Coefficient4 y",
        "    -0.000107,               !- Coefficient5 y**2",
        "    -0.000157,               !- Coefficient6 x*y",
        "    8.88889,                 !- Minimum Value of x",
        "    21.6667,                 !- Maximum Value of x",
        "    12.7778,                 !- Minimum Value of y",
        "    51.6667,                 !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Dimensionless,           !- Input Unit Type for X",
        "    Dimensionless,           !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "Curve:Biquadratic,",
        "    3PowerCurveIpakCS,       !- Name",
        "    0.99,                    !- Coefficient1 Constant",
        "    -0.039,                  !- Coefficient2 x",
        "    0.00111,                 !- Coefficient3 x**2",
        "    0.0144,                  !- Coefficient4 y",
        "    0.000489,                !- Coefficient5 y**2",
        "    -0.00111,                !- Coefficient6 x*y",
        "    8.88889,                 !- Minimum Value of x",
        "    21.6667,                 !- Maximum Value of x",
        "    12.7778,                 !- Minimum Value of y",
        "    51.6667,                 !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Dimensionless,           !- Input Unit Type for X",
        "    Dimensionless,           !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "Curve:Biquadratic,",
        "    4CapacityCurveIpakCS,          !- Name",
        "    0.528,                   !- Coefficient1 Constant",
        "    0.0324,                  !- Coefficient2 x",
        "    -0.0000156,              !- Coefficient3 x**2",
        "    0.00265,                 !- Coefficient4 y",
        "    -0.000125,               !- Coefficient5 y**2",
        "    -0.000134,               !- Coefficient6 x*y",
        "    8.88889,                 !- Minimum Value of x",
        "    21.6667,                 !- Maximum Value of x",
        "    12.7778,                 !- Minimum Value of y",
        "    51.6667,                 !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Dimensionless,           !- Input Unit Type for X",
        "    Dimensionless,           !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "Curve:Biquadratic,",
        "    4PowerCurveIpakCS,       !- Name",
        "    0.972,                   !- Coefficient1 Constant",
        "    -0.0357,                 !- Coefficient2 x",
        "    0.00105,                 !- Coefficient3 x**2",
        "    0.0109,                  !- Coefficient4 y",
        "    0.000517,                !- Coefficient5 y**2",
        "    -0.00101,                !- Coefficient6 x*y",
        "    8.88889,                 !- Minimum Value of x",
        "    21.6667,                 !- Maximum Value of x",
        "    12.7778,                 !- Minimum Value of y",
        "    51.6667,                 !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Dimensionless,           !- Input Unit Type for X",
        "    Dimensionless,           !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "Curve:Biquadratic,",
        "    5CapacityCurveIpakCS,    !- Name",
        "    0.551,                   !- Coefficient1 Constant",
        "    0.0291,                  !- Coefficient2 x",
        "    0.000124,                !- Coefficient3 x**2",
        "    0.00196,                 !- Coefficient4 y",
        "    -0.00011,                !- Coefficient5 y**",
        "    -0.000144,               !- Coefficient6 x*y",
        "    8.88889,                 !- Minimum Value of x",
        "    21.6667,                 !- Maximum Value of x",
        "    12.7778,                 !- Minimum Value of y",
        "    51.6667,                 !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Dimensionless,           !- Input Unit Type for X",
        "    Dimensionless,           !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "Curve:Biquadratic,",
        "    5PowerCurveIpakCS,       !- Name",
        "    0.916,                   !- Coefficient1 Constant",
        "    -0.0306,                 !- Coefficient2 x",
        "    0.000885,                !- Coefficient3 x**2",
        "    0.0126,                  !- Coefficient4 y",
        "    0.000495,                !- Coefficient5 y**2",
        "    -0.00103,                !- Coefficient6 x*y",
        "    8.88889,                 !- Minimum Value of x",
        "    21.6667,                 !- Maximum Value of x",
        "    12.7778,                 !- Minimum Value of y",
        "    51.6667,                 !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Dimensionless,           !- Input Unit Type for X",
        "    Dimensionless,           !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type    "

    });

    ASSERT_TRUE(process_idf(idf_objects));

    DataGlobals::NumOfTimeStepInHour = 1;
    DataGlobals::MinutesPerTimeStep = 60;
    ScheduleManager::ProcessScheduleInput();

    HVACDXSystem::GetDXCoolingSystemInput();
    EXPECT_EQ(HVACDXSystem::DXCoolingSystem(1).Name, "DX COOLING COIL SYSTEM");
    EXPECT_FALSE(HVACDXSystem::DXCoolingSystem(1).ISHundredPercentDOASDXCoil);
    EXPECT_EQ(VariableSpeedCoils::VarSpeedCoil(1).Name, "VS DX COOLING COIL");
}

TEST_F(EnergyPlusFixture, VariableSpeedCoils_RHControl)
{
    // issue #6920
    std::string const idf_objects = delimited_string({

        "  Schedule:Compact, AVAILSCHED, FRACTION, Through: 12/31, For: Alldays, Until: 24:00,1.00; ",

        "  CoilSystem:Cooling:DX,",
        "    DX Cooling Coil System,  !- Name",
        "    AvailSched,    !- Availability Schedule Name",
        "    DX Cooling Coil Air Inlet Node,  !- DX Cooling Coil System Inlet Node Name",
        "    Heating Coil Air Inlet Node,  !- DX Cooling Coil System Outlet Node Name",
        "    Heating Coil Air Inlet Node,  !- DX Cooling Coil System Sensor Node Name",
        "    Coil:Cooling:DX:VariableSpeed,!- Cooling Coil Object Type",
        "    VS DX Cooling Coil,      !- Cooling Coil Name",
        "    CoolReheat,              !- Dehumidification Control Type",
        "    Yes,                     !- Run on Sensible Load",
        "    Yes,                     !- Run on Latent Load",
        "    No;                      !- Use Outdoor Air DX Cooling Coil",

        "  Coil:Cooling:DX:VariableSpeed,",
        "    VS DX Cooling Coil,              !- Name",
        "    DX Cooling Coil Air Inlet Node,  !- Air Inlet Node Name",
        "    Heating Coil Air Inlet Node,     !- Air Outlet Node Name",
        "    5,                       !- Number of Speeds {dimensionless}",
        "    5,                       !- Nominal Speed Level {dimensionless}",
        "    132769.7,                !- Rated Total Cooling Capacity At Selected Nominal Speed Level {w}",
        "    5.66336932,              !- Rated Volumetric Air Flow Rate At Selected Nominal Speed Level {m3/s}",
        "    0,                       !- Nominal Time for Condensate to Begin Leaving the Coil {s}",
        "    0,                       !- Initial Moisture Evaporation Rate Divided by Steady-State AC Latent Capacity {dimensionless}",
        "    PLFCurve,                !- Energy Part Load Fraction Curve Name",
        "    ,                        !- Condenser Air Inlet Node Name",
        "    AirCooled,               !- Condenser Type",
        "    ,                        !- Evaporative Condenser Pump Rated Power Consumption {W}",
        "    ,                        !- Crankcase Heater Capacity {W}",
        "    10,                      !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
        "    ,                        !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "    ,                        !- Supply Water Storage Tank Name",
        "    ,                        !- Condensate Collection Water Storage Tank Name",
        "    ,                        !- Basin Heater Capacity {W/K}",
        "    2,                       !- Basin Heater Setpoint Temperature {C}",
        "    ,                        !- Basin Heater Operating Schedule Name",
        "    33861.72,                !- Speed 1 Reference Unit Total Cooling Capacity At Rated Conditions {w}",
        "    0.78,                    !- Speed 1 Reference Unit Sensible Heat Ratio At Rated Conditions {dimensionless}",
        "    4.328973,                !- Speed 1 Reference Unit COP At Rated Conditions {dimensionless}",
        "    1.396964,                !- Speed 1 Reference Unit Air Flow Rate At Rated Conditions {m3/s}",
        "    ,                        !- Speed 1 Reference Unit Condenser Flow Rate at Rated Conditions {m3/s}",
        "    ,                        !- Speed 1 Reference Unit Pad Effectiveness of Evap Precooling at Rated Conditions {dimensionless}",
        "    1Cap,                    !- Speed 1 Total Cooling Capacity Function of Temperature Curve Name",
        "    CAPFF,                   !- Speed 1 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "    1Pow,                    !- Speed 1 Energy Input Ratio Function of Temperature Curve Name",
        "    EIRFF,                   !- Speed 1 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "    35516.08,                !- Speed 2 Reference Unit Total Cooling Capacity At Rated Conditions {w}",
        "    0.78,                    !- Speed 2 Reference Unit Sensible Heat Ratio At Rated Conditions {dimensionless}",
        "    4.540061,                !- Speed 2 Reference Unit COP At Rated Conditions {dimensionless}",
        "    1.88779,                 !- Speed 2 Reference Unit Air Flow Rate At Rated Conditions {m3/s}",
        "    ,                        !- Speed 2 Reference Unit Condenser Flow Rate at Rated Conditions {m3/s}",
        "    ,                        !- Speed 2 Reference Unit Pad Effectiveness of Evap Precooling at Rated Conditions {dimensionless}",
        "    1Cap,                    !- Speed 2 Total Cooling Capacity Function of Temperature Curve Name",
        "    CAPFF,                   !- Speed 2 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "    1Pow,                    !- Speed 2 Energy Input Ratio Function of Temperature Curve Name",
        "    EIRFF,                   !- Speed 2 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "    65133.17,                !- Speed 3 Reference Unit Total Cooling Capacity At Rated Conditions {w}",
        "    0.77,                    !- Speed 3 Reference Unit Sensible Heat Ratio At Rated Conditions {dimensionless}",
        "    4.164418,                !- Speed 3 Reference Unit COP At Rated Conditions {dimensionless}",
        "    2.831685,                !- Speed 3 Reference Unit Air Flow Rate At Rated Conditions {m3/s}",
        "    ,                        !- Speed 3 Reference Unit Condenser Flow Rate at Rated Conditions {m3/s}",
        "    ,                        !- Speed 3 Reference Unit Pad Effectiveness of Evap Precooling at Rated Conditions {dimensionless}",
        "    1Cap,                    !- Speed 3 Total Cooling Capacity Function of Temperature Curve Name",
        "    CAPFF,                   !- Speed 3 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "    1Pow,                    !- Speed 3 Energy Input Ratio Function of Temperature Curve Name",
        "    EIRFF,                   !- Speed 3 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "    119583.3,                !- Speed 4 Reference Unit Total Cooling Capacity At Rated Conditions {w}",
        "    0.76,                    !- Speed 4 Reference Unit Sensible Heat Ratio At Rated Conditions {dimensionless}",
        "    3.469661,                !- Speed 4 Reference Unit COP At Rated Conditions {dimensionless}",
        "    3.553764,                !- Speed 4 Reference Unit Air Flow Rate At Rated Conditions {m3/s}",
        "    ,                        !- Speed 4 Reference Unit Condenser Flow Rate at Rated Conditions {m3/s}",
        "    ,                        !- Speed 4 Reference Unit Pad Effectiveness of Evap Precooling at Rated Conditions {dimensionless}",
        "    1Cap,                    !- Speed 4 Total Cooling Capacity Function of Temperature Curve Name",
        "    CAPFF,                   !- Speed 4 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "    1Pow,                    !- Speed 4 Energy Input Ratio Function of Temperature Curve Name",
        "    EIRFF,                   !- Speed 4 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "    132769.7,                !- Speed 5 Reference Unit Total Cooling Capacity At Rated Conditions {w}",
        "    0.76,                    !- Speed 5 Reference Unit Sensible Heat Ratio At Rated Conditions {dimensionless}",
        "    3.822957,                !- Speed 5 Reference Unit COP At Rated Conditions {dimensionless}",
        "    5.66336932,              !- Speed 5 Reference Unit Air Flow Rate At Rated Conditions {m3/s}",
        "    ,                        !- Speed 5 Reference Unit Condenser Flow Rate at Rated Conditions {m3/s}",
        "    ,                        !- Speed 5 Reference Unit Pad Effectiveness of Evap Precooling at Rated Conditions {dimensionless}",
        "    1Cap,                    !- Speed 5 Total Cooling Capacity Function of Temperature Curve Name",
        "    CAPFF,                   !- Speed 5 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "    1Pow,                    !- Speed 5 Energy Input Ratio Function of Temperature Curve Name",
        "    EIRFF;                   !- Speed 5 Energy Input Ratio Function of Air Flow Fraction Curve Name",

        "Curve:Quadratic, PLFCurve, 0.85, 0.83, 0.0, 0.0, 0.3, 0.85, 1.0, Dimensionless, Dimensionless; ",
        "Curve:Cubic, CAPFF, 1, 0, 0, 0, 0, 1, , , Dimensionless, Dimensionless; ",
        "Curve:Cubic, EIRFF, 1, 0, 0, 0, 0, 1, , , Dimensionless, Dimensionless; ",
        "Curve:Biquadratic, 1Cap, 0.483, 0.0305, 0.0000458, 0.00511, -1.50E-04, -1.28E-04, 8.89, 21.67, 12.78, 51.67, , , Temperature, Temperature, "
        "Dimensionless; ",
        "Curve:Biquadratic, 1Pow, 1.33, -0.034, 0.00094, -0.0086, 0.00077, -0.000972, 8.89, 21.7, 12.8, 51.7, , , Temperature, Temperature, "
        "Dimensionless; ",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    DataGlobals::NumOfTimeStepInHour = 1;
    DataGlobals::MinutesPerTimeStep = 60;
    OutputReportPredefined::SetPredefinedTables();
    ScheduleManager::ProcessScheduleInput();
    ScheduleManager::Schedule(1).CurrentValue = 1.0; // Enable schedule without calling schedule manager

    int DXSystemNum = 1;
    bool FirstHVACIteration = true;
    bool HXUnitOn = false;
    int InletNode = 1;
    int ControlNode = 2; // same as outlet node number

    HVACDXSystem::GetDXCoolingSystemInput();
    EXPECT_EQ(HVACDXSystem::DXCoolingSystem(DXSystemNum).Name, "DX COOLING COIL SYSTEM");
    EXPECT_FALSE(HVACDXSystem::DXCoolingSystem(DXSystemNum).ISHundredPercentDOASDXCoil);
    EXPECT_EQ(VariableSpeedCoils::VarSpeedCoil(DXSystemNum).Name, "VS DX COOLING COIL");
    EXPECT_EQ(2, HVACDXSystem::DXCoolingSystem(DXSystemNum).DXSystemControlNodeNum);

    // set up outdoor environment
    DataEnvironment::OutDryBulbTemp = 35.0;
    DataEnvironment::OutHumRat = 0.0196;
    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::OutWetBulbTemp = 27.0932;

    // set up inputs to test coil control
    HVACDXSystem::DXCoolingSystem(DXSystemNum).DesiredOutletTemp = 18.0;
    HVACDXSystem::DXCoolingSystem(DXSystemNum).DesiredOutletHumRat = 1.0;
    DataEnvironment::StdRhoAir = 1.2;
    DataLoopNode::Node(InletNode).MassFlowRate = 5.66336932 * DataEnvironment::StdRhoAir;
    DataLoopNode::Node(InletNode).Temp = 24.0;
    DataLoopNode::Node(InletNode).HumRat = 0.012143698;
    DataLoopNode::Node(InletNode).Enthalpy = 55029.3778; // conditions at 65 % RH
    DataLoopNode::Node(ControlNode).TempSetPoint = HVACDXSystem::DXCoolingSystem(DXSystemNum).DesiredOutletTemp;
    Real64 RHControlHumRat = 0.01119276; // humrat at 24C, 60% RH
    DataLoopNode::Node(ControlNode).HumRatMax = RHControlHumRat;

    // test sensible control
    HVACDXSystem::ControlDXSystem(DXSystemNum, FirstHVACIteration, HXUnitOn);
    // system meets temperature set point
    EXPECT_NEAR(HVACDXSystem::DXCoolingSystem(DXSystemNum).DesiredOutletTemp, DataLoopNode::Node(ControlNode).Temp, 0.00001);
    // system was not told to meet humidity ratio set point (since DesiredOutletHumRat = 1.0)
    EXPECT_GT(DataLoopNode::Node(ControlNode).HumRat, DataLoopNode::Node(ControlNode).HumRatMax);
    // sensible load met by compressor speed 3
    EXPECT_EQ(3, HVACDXSystem::DXCoolingSystem(DXSystemNum).SpeedNum);

    // test latent control
    HVACDXSystem::DXCoolingSystem(DXSystemNum).DesiredOutletHumRat = RHControlHumRat;
    HVACDXSystem::ControlDXSystem(DXSystemNum, FirstHVACIteration, HXUnitOn);

    // system over cools past temperature set point
    EXPECT_GT(HVACDXSystem::DXCoolingSystem(DXSystemNum).DesiredOutletTemp, DataLoopNode::Node(ControlNode).Temp);
    // system does meet humidity ratio set point
    EXPECT_NEAR(DataLoopNode::Node(ControlNode).HumRat, DataLoopNode::Node(ControlNode).HumRatMax, 0.0000001);
    // latent load needed to increase compressor speed to speed 4
    EXPECT_EQ(4, HVACDXSystem::DXCoolingSystem(DXSystemNum).SpeedNum);
}

} // namespace EnergyPlus
