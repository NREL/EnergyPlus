// EnergyPlus, Copyright (c) 1996-2020, The Board of Trustees of the University of Illinois,
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

// EnergyPlus::WaterThermalTank Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus/DXCoils.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/WaterToAirHeatPumpSimple.hh>
#include <EnergyPlus/InternalHeatGains.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterThermalTanks.hh>
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/Psychrometrics.hh>

using namespace EnergyPlus;
using namespace OutputReportPredefined;

TEST_F(EnergyPlusFixture, HeatPumpWaterHeaterTests_TestQsourceCalcs)
{
    Real64 DeltaT = 0.0;
    Real64 const SourceInletTemp = 62.0;
    Real64 const Cp = 4178.; // water, J/(kg * K)
    Real64 const SetPointTemp = 60.0;
    Real64 const SourceMassFlowRateOrig = 0.378529822165; // water, 6 gal/min
    Real64 SourceMassFlowRate = SourceMassFlowRateOrig;
    Real64 Qheatpump = 0.0;
    Real64 Qsource = 0.0;

    // Mixed Tank

    // Test case without HPWH
    WaterThermalTanks::CalcMixedTankSourceSideHeatTransferRate(DeltaT, SourceInletTemp, Cp, SetPointTemp, SourceMassFlowRate, Qheatpump, Qsource);
    // Qsource is non zero and calculated relative to the tank setpoint.
    EXPECT_DOUBLE_EQ(SourceMassFlowRate * Cp * (SourceInletTemp - SetPointTemp), Qsource);
    // Qheatpump is zero
    EXPECT_DOUBLE_EQ(Qheatpump, 0.0);
    // SourceMassFlowRate is unchanged
    EXPECT_DOUBLE_EQ(SourceMassFlowRateOrig, SourceMassFlowRate);

    // Test case with HPWH
    DeltaT = 5.0;
    WaterThermalTanks::CalcMixedTankSourceSideHeatTransferRate(DeltaT, SourceInletTemp, Cp, SetPointTemp, SourceMassFlowRate, Qheatpump, Qsource);
    // Qsource is Qheatpump
    EXPECT_DOUBLE_EQ(Qsource, Qheatpump);
    // Qheatpump is the heat transfer rate from the input DeltaT
    EXPECT_DOUBLE_EQ(SourceMassFlowRateOrig * Cp * DeltaT, Qheatpump);
    // SourceMassFlowRate is zero
    EXPECT_DOUBLE_EQ(SourceMassFlowRate, 0.0);
}

TEST_F(EnergyPlusFixture, WaterThermalTankData_GetDeadBandTemp)
{

    WaterThermalTanks::WaterThermalTankData thisTank;
    thisTank.SetPointTemp = 10;
    thisTank.DeadBandDeltaTemp = 1;

    // first the hot water tank
    thisTank.IsChilledWaterTank = false;
    EXPECT_DOUBLE_EQ(9.0, thisTank.getDeadBandTemp());

    // then the chilled water tank
    thisTank.IsChilledWaterTank = true;
    EXPECT_DOUBLE_EQ(11.0, thisTank.getDeadBandTemp());
}

TEST_F(EnergyPlusFixture, HPWHZoneEquipSeqenceNumberWarning)
{
    std::string const idf_objects = delimited_string({
        "  Schedule:Constant, DummySch, , 1.0;",
        "  WaterHeater:HeatPump:PumpedCondenser,",
        "    Zone4HeatPumpWaterHeater,!- Name",
        "    ,  !- Availability Schedule Name",
        "    DummySch,             !- Compressor Setpoint Temperature Schedule Name",
        "    2.0,                     !- Dead Band Temperature Difference {deltaC}",
        "    Zone4WaterInletNode,     !- Condenser Water Inlet Node Name",
        "    Zone4WaterOutletNode,    !- Condenser Water Outlet Node Name",
        "    autocalculate,           !- Condenser Water Flow Rate {m3/s}",
        "    autocalculate,           !- Evaporator Air Flow Rate {m3/s}",
        "    ZoneAirOnly,             !- Inlet Air Configuration",
        "    Zone4AirOutletNode,      !- Air Inlet Node Name",
        "    Zone4AirInletNode,       !- Air Outlet Node Name",
        "    ,                        !- Outdoor Air Node Name",
        "    ,                        !- Exhaust Air Node Name",
        "    ,                        !- Inlet Air Temperature Schedule Name",
        "    ,                        !- Inlet Air Humidity Schedule Name",
        "    SPACE4-1,                !- Inlet Air Zone Name",
        "    WaterHeater:Mixed,       !- Tank Object Type",
        "    Zone4HPWHTank,           !- Tank Name",
        "    ,                        !- Tank Use Side Inlet Node Name",
        "    ,                        !- Tank Use Side Outlet Node Name",
        "    Coil:WaterHeating:AirToWaterHeatPump:Pumped,  !- DX Coil Object Type",
        "    Zone4HPWHDXCoil,         !- DX Coil Name",
        "    5.0,                     !- Minimum Inlet Air Temperature for Compressor Operation {C}",
        "    ,                        !- Maximum Inlet Air Temperature for Compressor Operation {C}",
        "    Zone,                    !- Compressor Location",
        "    ,                        !- Compressor Ambient Temperature Schedule Name",
        "    Fan:OnOff,               !- Fan Object Type",
        "    Zone4HPWHFan,            !- Fan Name",
        "    DrawThrough,             !- Fan Placement",
        "    15.0,                    !- On Cycle Parasitic Electric Load {W}",
        "    5.0,                     !- Off Cycle Parasitic Electric Load {W}",
        "    Zone;                    !- Parasitic Heat Rejection Location",
        "  WaterHeater:Mixed,",
        "    Zone4HPWHTank,           !- Name",
        "    0.3785,                  !- Tank Volume {m3}",
        "    DummySch,                !- Setpoint Temperature Schedule Name",
        "    2.0,                     !- Deadband Temperature Difference {deltaC}",
        "    82.2222,                 !- Maximum Temperature Limit {C}",
        "    CYCLE,                   !- Heater Control Type",
        "    5000,                    !- Heater Maximum Capacity {W}",
        "    0,                       !- Heater Minimum Capacity {W}",
        "    ,                        !- Heater Ignition Minimum Flow Rate {m3/s}",
        "    ,                        !- Heater Ignition Delay {s}",
        "    ELECTRICITY,             !- Heater Fuel Type",
        "    0.95,                    !- Heater Thermal Efficiency",
        "    ,                        !- Part Load Factor Curve Name",
        "    10,                      !- Off Cycle Parasitic Fuel Consumption Rate {W}",
        "    ELECTRICITY,             !- Off Cycle Parasitic Fuel Type",
        "    0,                       !- Off Cycle Parasitic Heat Fraction to Tank",
        "    30,                      !- On Cycle Parasitic Fuel Consumption Rate {W}",
        "    ELECTRICITY,             !- On Cycle Parasitic Fuel Type",
        "    0,                       !- On Cycle Parasitic Heat Fraction to Tank",
        "    Schedule,                !- Ambient Temperature Indicator",
        "    DummySch,                !- Ambient Temperature Schedule Name",
        "    ,                        !- Ambient Temperature Zone Name",
        "    ,                        !- Ambient Temperature Outdoor Air Node Name",
        "    2.0,                     !- Off Cycle Loss Coefficient to Ambient Temperature {W/K}",
        "    1.0,                     !- Off Cycle Loss Fraction to Zone",
        "    2.0,                     !- On Cycle Loss Coefficient to Ambient Temperature {W/K}",
        "    1.0,                     !- On Cycle Loss Fraction to Zone",
        "    0.00379,                 !- Peak Use Flow Rate {m3/s}",
        "    DummySch,                !- Use Flow Rate Fraction Schedule Name",
        "    ,                        !- Cold Water Supply Temperature Schedule Name",
        "    ,                        !- Use Side Inlet Node Name",
        "    ,                        !- Use Side Outlet Node Name",
        "    ,                        !- Use Side Effectiveness",
        "    Zone4WaterOutletNode,    !- Source Side Inlet Node Name",
        "    Zone4WaterInletNode,     !- Source Side Outlet Node Name",
        "    0.95;                    !- Source Side Effectiveness",
        "  Coil:WaterHeating:AirToWaterHeatPump:Pumped,",
        "    Zone4HPWHDXCoil,         !- Name",
        "    4000.0,                  !- Rated Heating Capacity {W}",
        "    3.2,                     !- Rated COP {W/W}",
        "    0.6956,                  !- Rated Sensible Heat Ratio",
        "    29.44,                   !- Rated Evaporator Inlet Air Dry-Bulb Temperature {C}",
        "    22.22,                   !- Rated Evaporator Inlet Air Wet-Bulb Temperature {C}",
        "    55.72,                   !- Rated Condenser Inlet Water Temperature {C}",
        "    autocalculate,           !- Rated Evaporator Air Flow Rate {m3/s}",
        "    autocalculate,           !- Rated Condenser Water Flow Rate {m3/s}",
        "    No,                      !- Evaporator Fan Power Included in Rated COP",
        "    No,                      !- Condenser Pump Power Included in Rated COP",
        "    No,                      !- Condenser Pump Heat Included in Rated Heating Capacity and Rated COP",
        "    150.0,                   !- Condenser Water Pump Power {W}",
        "    0.1,                     !- Fraction of Condenser Pump Heat to Water",
        "    Zone4AirOutletNode,      !- Evaporator Air Inlet Node Name",
        "    Zone4DXCoilAirOutletNode,!- Evaporator Air Outlet Node Name",
        "    Zone4WaterInletNode,     !- Condenser Water Inlet Node Name",
        "    Zone4WaterOutletNode,    !- Condenser Water Outlet Node Name",
        "    100.0,                   !- Crankcase Heater Capacity {W}",
        "    5.0,                     !- Maximum Ambient Temperature for Crankcase Heater Operation {C}",
        "    WetBulbTemperature,      !- Evaporator Air Temperature Type for Curve Objects",
        "    HPWHHeatingCapFTemp,     !- Heating Capacity Function of Temperature Curve Name",
        "    ,                        !- Heating Capacity Function of Air Flow Fraction Curve Name",
        "    ,                        !- Heating Capacity Function of Water Flow Fraction Curve Name",
        "    HPWHHeatingCOPFTemp,     !- Heating COP Function of Temperature Curve Name",
        "    ,                        !- Heating COP Function of Air Flow Fraction Curve Name",
        "    ,                        !- Heating COP Function of Water Flow Fraction Curve Name",
        "    HPWHPLFFPLR;             !- Part Load Fraction Correlation Curve Name",
        "  Fan:OnOff,",
        "    Zone4HPWHFan,            !- Name",
        "    ,  !- Availability Schedule Name",
        "    0.7,                     !- Fan Total Efficiency",
        "    100.0,                   !- Pressure Rise {Pa}",
        "    0.2685,                  !- Maximum Flow Rate {m3/s}",
        "    0.9,                     !- Motor Efficiency",
        "    1.0,                     !- Motor In Airstream Fraction",
        "    Zone4DXCoilAirOutletNode,!- Air Inlet Node Name",
        "    Zone4AirInletNode;       !- Air Outlet Node Name",
        "  Curve:Biquadratic,",
        "    HPWHHeatingCapFTemp,     !- Name",
        "    0.369827,                !- Coefficient1 Constant",
        "    0.043341,                !- Coefficient2 x",
        "    -0.00023,                !- Coefficient3 x**2",
        "    0.000466,                !- Coefficient4 y",
        "    0.000026,                !- Coefficient5 y**2",
        "    -0.00027,                !- Coefficient6 x*y",
        "    0.0,                     !- Minimum Value of x",
        "    40.0,                    !- Maximum Value of x",
        "    20.0,                    !- Minimum Value of y",
        "    90.0,                    !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",
        "  Curve:Biquadratic,",
        "    HPWHHeatingCOPFTemp,     !- Name",
        "    1.19713,                 !- Coefficient1 Constant",
        "    0.077849,                !- Coefficient2 x",
        "    -0.0000016,              !- Coefficient3 x**2",
        "    -0.02675,                !- Coefficient4 y",
        "    0.000296,                !- Coefficient5 y**2",
        "    -0.00112,                !- Coefficient6 x*y",
        "    0.0,                     !- Minimum Value of x",
        "    40.0,                    !- Maximum Value of x",
        "    20.0,                    !- Minimum Value of y",
        "    90.0,                    !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",
        "  Curve:Quadratic,",
        "    HPWHPLFFPLR,             !- Name",
        "    0.75,                    !- Coefficient1 Constant",
        "    0.25,                    !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.0,                     !- Minimum Value of x",
        "    1.0;                     !- Maximum Value of x",
        "  Zone,",
        "    SPACE4-1,                !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    2.438400269,             !- Ceiling Height {m}",
        "    103.311355591;           !- Volume {m3}",
        "  ZoneHVAC:IdealLoadsAirSystem,",
        "    SPACE4-1 AirSys, !- Name",
        "    ,                        !- Availability Schedule Name",
        "    IdealLoadInNode,         !- Zone Supply Air Node Name",
        "    IdealLoadOutNode,        !- Zone Exhaust Air Node Name",
        "    ,                        !- System Inlet Air Node Name",
        "    50,                      !- Maximum Heating Supply Air Temperature {C}",
        "    13,                      !- Minimum Cooling Supply Air Temperature {C}",
        "    0.015,                   !- Maximum Heating Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "    0.009,                   !- Minimum Cooling Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "    NoLimit,                 !- Heating Limit",
        "    autosize,                !- Maximum Heating Air Flow Rate {m3/s}",
        "    ,                        !- Maximum Sensible Heating Capacity {W}",
        "    NoLimit,                 !- Cooling Limit",
        "    autosize,                !- Maximum Cooling Air Flow Rate {m3/s}",
        "    ,                        !- Maximum Total Cooling Capacity {W}",
        "    ,                        !- Heating Availability Schedule Name",
        "    ,                        !- Cooling Availability Schedule Name",
        "    ConstantSupplyHumidityRatio,  !- Dehumidification Control Type",
        "    ,                        !- Cooling Sensible Heat Ratio {dimensionless}",
        "    ConstantSupplyHumidityRatio,  !- Humidification Control Type",
        "    ,                        !- Design Specification Outdoor Air Object Name",
        "    ,                        !- Outdoor Air Inlet Node Name",
        "    ,                        !- Demand Controlled Ventilation Type",
        "    ,                        !- Outdoor Air Economizer Type",
        "    ,                        !- Heat Recovery Type",
        "    ,                        !- Sensible Heat Recovery Effectiveness {dimensionless}",
        "    ;                        !- Latent Heat Recovery Effectiveness {dimensionless}",
        "  ZoneHVAC:EquipmentConnections,",
        "    SPACE4-1,                !- Zone Name",
        "    SPACE4-1 Eq,             !- Zone Conditioning Equipment List Name",
        "    SPACE4-1 In Nodes,       !- Zone Air Inlet Node or NodeList Name",
        "    SPACE4-1 Out Nodes,      !- Zone Air Exhaust Node or NodeList Name",
        "    SPACE4-1 Node,           !- Zone Air Node Name",
        "    SPACE4-1 Out Node;       !- Zone Return Air Node Name",
        "  ZoneHVAC:EquipmentList,",
        "    SPACE4-1 Eq,             !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:IdealLoadsAirSystem,  !- Zone Equipment 1 Object Type",
        "    SPACE4-1 AirSys,            !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1,                       !- Zone Equipment 1 Heating or No-Load Sequence",
        "    ,                        !- Zone Equipment 1 Sequential Cooling Fraction",
        "    ,                        !- Zone Equipment 1 Sequential Heating Fraction",
        "    WaterHeater:HeatPump:PumpedCondenser,    !- Zone Equipment 2 Object Type",
        "    Zone4HeatPumpWaterHeater,!- Zone Equipment 2 Name",
        "    2,                       !- Zone Equipment 2 Cooling Sequence",
        "    2,                       !- Zone Equipment 2 Heating or No-Load Sequence",
        "    ,                        !- Zone Equipment 2 Sequential Cooling Fraction",
        "    ;                        !- Zone Equipment 2 Sequential Heating Fraction",
        "  NodeList,",
        "    SPACE4-1 In Nodes,       !- Name",
        "    SPACE4-1 In Node,        !- Node 1 Name",
        "    Zone4AirInletNode,       !- Node 2 Name",
        "    IdealLoadInNode;       !- Node 2 Name",
        "  NodeList,",
        "    SPACE4-1 Out Nodes,      !- Name",
        "    Zone4AirOutletNode,      !- Node 1 Name",
        "    IdealLoadOutNode;      !- Node 1 Name",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound = false;
    HeatBalanceManager::GetZoneData(ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    EXPECT_FALSE(WaterThermalTanks::GetWaterThermalTankInput());
}

TEST_F(EnergyPlusFixture, HPWHWrappedDummyNodeConfig)
{
    // Unit test for #5127

    std::vector<std::string> idf_lines({"Schedule:Constant,DummySch,,1.0;",
                                        "Curve:Biquadratic,",
                                        "    HPWH-Htg-Cap-fT,         !- Name",
                                        "    0.563,                   !- Coefficient1 Constant",
                                        "    0.0437,                  !- Coefficient2 x",
                                        "    0.000039,                !- Coefficient3 x**2",
                                        "    0.0055,                  !- Coefficient4 y",
                                        "    -.000148,                !- Coefficient5 y**2",
                                        "    -.000145,                !- Coefficient6 x*y",
                                        "    0,                       !- Minimum Value of x",
                                        "    100,                     !- Maximum Value of x",
                                        "    0,                       !- Minimum Value of y",
                                        "    100,                     !- Maximum Value of y",
                                        "    0;                       !- Minimum Curve Output",
                                        "Curve:Biquadratic,",
                                        "    HPWH-Htg-COP-fT,         !- Name",
                                        "    1.1332,                  !- Coefficient1 Constant",
                                        "    0.063,                   !- Coefficient2 x",
                                        "    -.0000979,               !- Coefficient3 x**2",
                                        "    -.00972,                 !- Coefficient4 y",
                                        "    -.0000214,               !- Coefficient5 y**2",
                                        "    -.000686,                !- Coefficient6 x*y",
                                        "    0,                       !- Minimum Value of x",
                                        "    100,                     !- Maximum Value of x",
                                        "    0,                       !- Minimum Value of y",
                                        "    100;                     !- Maximum Value of y",
                                        "Curve:Quadratic,",
                                        "    HPWH-COP-fPLR,           !- Name",
                                        "    1,                       !- Coefficient1 Constant",
                                        "    0,                       !- Coefficient2 x",
                                        "    0,                       !- Coefficient3 x**2",
                                        "    0,                       !- Minimum Value of x",
                                        "    1;                       !- Maximum Value of x"});
    for (int i = 1; i <= 2; ++i) {
        std::string const i_str = std::to_string(i);
        idf_lines.push_back("Coil:WaterHeating:AirToWaterHeatPump:Wrapped,");
        idf_lines.push_back("    HPWH Coil " + i_str + ",               !- Name");
        idf_lines.push_back("    2349.6,                  !- Rated Heating Capacity {W}");
        idf_lines.push_back("    2.4,                     !- Rated COP {W/W}");
        idf_lines.push_back("    0.981,                   !- Rated Sensible Heat Ratio");
        idf_lines.push_back("    19.72,                   !- Rated Evaporator Inlet Air Dry-Bulb Temperature {C}");
        idf_lines.push_back("    13.5,                    !- Rated Evaporator Inlet Air Wet-Bulb Temperature {C}");
        idf_lines.push_back("    48.89,                   !- Rated Condenser Water Temperature {C}");
        idf_lines.push_back("    0.189,                   !- Rated Evaporator Air Flow Rate {m3/s}");
        idf_lines.push_back("    Yes,                     !- Evaporator Fan Power Included in Rated COP");
        idf_lines.push_back("    HPWH Air Inlet " + i_str + ",          !- Evaporator Air Inlet Node Name");
        idf_lines.push_back("    HPWH Coil Outlet Fan Inlet " + i_str + ",  !- Evaporator Air Outlet Node Name");
        idf_lines.push_back("    0,                       !- Crankcase Heater Capacity {W}");
        idf_lines.push_back("    10,                      !- Maximum Ambient Temperature for Crankcase Heater Operation {C}");
        idf_lines.push_back("    WetBulbTemperature,      !- Evaporator Air Temperature Type for Curve Objects");
        idf_lines.push_back("    HPWH-Htg-Cap-fT,         !- Heating Capacity Function of Temperature Curve Name");
        idf_lines.push_back("    ,                        !- Heating Capacity Function of Air Flow Fraction Curve Name");
        idf_lines.push_back("    HPWH-Htg-COP-fT,         !- Heating COP Function of Temperature Curve Name");
        idf_lines.push_back("    ,                        !- Heating COP Function of Air Flow Fraction Curve Name");
        idf_lines.push_back("    HPWH-COP-fPLR;           !- Part Load Fraction Correlation Curve Name");
        idf_lines.push_back("Fan:OnOff,");
        idf_lines.push_back("    HPWH Fan " + i_str + ",                !- Name");
        idf_lines.push_back("    DummySch,                !- Availability Schedule Name");
        idf_lines.push_back("    0.1722,                   !- Fan Total Efficiency");
        idf_lines.push_back("    65,                     !- Pressure Rise {Pa}");
        idf_lines.push_back("    0.2279,                   !- Maximum Flow Rate {m3/s}");
        idf_lines.push_back("    1,                       !- Motor Efficiency");
        idf_lines.push_back("    0,                       !- Motor In Airstream Fraction");
        idf_lines.push_back("    HPWH Coil Outlet Fan Inlet " + i_str + ",  !- Air Inlet Node Name");
        idf_lines.push_back("    HPWH Air Outlet " + i_str + ",         !- Air Outlet Node Name");
        idf_lines.push_back("    ,                        !- Fan Power Ratio Function of Speed Ratio Curve Name");
        idf_lines.push_back("    ,                        !- Fan Efficiency Ratio Function of Speed Ratio Curve Name");
        idf_lines.push_back("    Water Heater;            !- End-Use Subcategory");
        idf_lines.push_back("WaterHeater:Stratified,");
        idf_lines.push_back("    HPWH Tank " + i_str + ",               !- Name");
        idf_lines.push_back("    Water Heater,            !- End-Use Subcategory");
        idf_lines.push_back("    0.287691,                !- Tank Volume {m3}");
        idf_lines.push_back("    1.594,                   !- Tank Height {m}");
        idf_lines.push_back("    VerticalCylinder,        !- Tank Shape");
        idf_lines.push_back("    ,                        !- Tank Perimeter {m}");
        idf_lines.push_back("    100,                     !- Maximum Temperature Limit {C}");
        idf_lines.push_back("    MasterSlave,             !- Heater Priority Control");
        idf_lines.push_back("    DummySch,                !- Heater 1 Setpoint Temperature Schedule Name");
        idf_lines.push_back("    18.5,                    !- Heater 1 Deadband Temperature Difference {deltaC}");
        idf_lines.push_back("    4500,                    !- Heater 1 Capacity {W}");
        idf_lines.push_back("    1.129,                   !- Heater 1 Height {m}");
        idf_lines.push_back("    DummySch,                !- Heater 2 Setpoint Temperature Schedule Name");
        idf_lines.push_back("    18.5,                    !- Heater 2 Deadband Temperature Difference {deltaC}");
        idf_lines.push_back("    0,                       !- Heater 2 Capacity {W}");
        idf_lines.push_back("    0.266,                   !- Heater 2 Height {m}");
        idf_lines.push_back("    Electricity,             !- Heater Fuel Type");
        idf_lines.push_back("    1,                       !- Heater Thermal Efficiency");
        idf_lines.push_back("    8.3,                     !- Off Cycle Parasitic Fuel Consumption Rate {W}");
        idf_lines.push_back("    Electricity,             !- Off Cycle Parasitic Fuel Type");
        idf_lines.push_back("    0,                       !- Off Cycle Parasitic Heat Fraction to Tank");
        idf_lines.push_back("    1,                       !- Off Cycle Parasitic Height {m}");
        idf_lines.push_back("    8.3,                     !- On Cycle Parasitic Fuel Consumption Rate {W}");
        idf_lines.push_back("    Electricity,             !- On Cycle Parasitic Fuel Type");
        idf_lines.push_back("    0,                       !- On Cycle Parasitic Heat Fraction to Tank");
        idf_lines.push_back("    1,                       !- On Cycle Parasitic Height {m}");
        idf_lines.push_back("    Schedule,                !- Ambient Temperature Indicator");
        idf_lines.push_back("    DummySch,                !- Ambient Temperature Schedule Name");
        idf_lines.push_back("    ,                        !- Ambient Temperature Zone Name");
        idf_lines.push_back("    ,                        !- Ambient Temperature Outdoor Air Node Name");
        idf_lines.push_back("    0.7878,                  !- Uniform Skin Loss Coefficient per Unit Area to Ambient Temperature {W/m2-K}");
        idf_lines.push_back("    1,                       !- Skin Loss Fraction to Zone");
        idf_lines.push_back("    ,                        !- Off Cycle Flue Loss Coefficient to Ambient Temperature {W/K}");
        idf_lines.push_back("    1,                       !- Off Cycle Flue Loss Fraction to Zone");
        idf_lines.push_back("    0.001,                   !- Peak Use Flow Rate {m3/s}");
        idf_lines.push_back("    DummySch,                !- Use Flow Rate Fraction Schedule Name");
        idf_lines.push_back("    DummySch,                !- Cold Water Supply Temperature Schedule Name");
        idf_lines.push_back("    ,                        !- Use Side Inlet Node Name");
        idf_lines.push_back("    ,                        !- Use Side Outlet Node Name");
        idf_lines.push_back("    1,                       !- Use Side Effectiveness");
        idf_lines.push_back("    0,                       !- Use Side Inlet Height {m}");
        idf_lines.push_back("    autocalculate,           !- Use Side Outlet Height {m}");
        idf_lines.push_back("    ,                        !- Source Side Inlet Node Name");
        idf_lines.push_back("    ,                        !- Source Side Outlet Node Name");
        idf_lines.push_back("    1,                       !- Source Side Effectiveness");
        idf_lines.push_back("    0.7,                     !- Source Side Inlet Height {m}");
        idf_lines.push_back("    0,                       !- Source Side Outlet Height {m}");
        idf_lines.push_back("    Fixed,                   !- Inlet Mode");
        idf_lines.push_back("    autosize,                !- Use Side Design Flow Rate {m3/s}");
        idf_lines.push_back("    autosize,                !- Source Side Design Flow Rate {m3/s}");
        idf_lines.push_back("    1.5,                     !- Indirect Water Heating Recovery Time {hr}");
        idf_lines.push_back("    12;                      !- Number of Nodes");
        idf_lines.push_back("WaterHeater:HeatPump:WrappedCondenser,");
        idf_lines.push_back("    HPWH " + i_str + ",                    !- Name");
        idf_lines.push_back("    DummySch,                !- Availability Schedule Name");
        idf_lines.push_back("    DummySch,                !- Compressor Setpoint Temperature Schedule Name");
        idf_lines.push_back("    3.89,                    !- Dead Band Temperature Difference {deltaC}");
        idf_lines.push_back("    0.0664166667,            !- Condenser Bottom Location");
        idf_lines.push_back("    0.8634166667,            !- Condenser Top Location");
        idf_lines.push_back("    0.2279,                  !- Evaporator Air Flow Rate {m3/s}");
        idf_lines.push_back("    Schedule,                !- Inlet Air Configuration");
        idf_lines.push_back("    HPWH Air Inlet " + i_str + ",          !- Air Inlet Node Name");
        idf_lines.push_back("    HPWH Air Outlet " + i_str + ",         !- Air Outlet Node Name");
        idf_lines.push_back("    ,                        !- Outdoor Air Node Name");
        idf_lines.push_back("    ,                        !- Exhaust Air Node Name");
        idf_lines.push_back("    DummySch,                !- Inlet Air Temperature Schedule Name");
        idf_lines.push_back("    DummySch,                !- Inlet Air Humidity Schedule Name");
        idf_lines.push_back("    ,                        !- Inlet Air Zone Name");
        idf_lines.push_back("    WaterHeater:Stratified,  !- Tank Object Type");
        idf_lines.push_back("    HPWH Tank " + i_str + ",               !- Tank Name");
        idf_lines.push_back("    ,                        !- Tank Use Side Inlet Node Name");
        idf_lines.push_back("    ,                        !- Tank Use Side Outlet Node Name");
        idf_lines.push_back("    Coil:WaterHeating:AirToWaterHeatPump:Wrapped,   !- DX Coil Object Type");
        idf_lines.push_back("    HPWH Coil " + i_str + ",               !- DX Coil Name");
        idf_lines.push_back("    7.2,                     !- Minimum Inlet Air Temperature for Compressor Operation {C}");
        idf_lines.push_back("    ,                        !- Maximum Inlet Air Temperature for Compressor Operation {C}");
        idf_lines.push_back("    Schedule,                !- Compressor Location");
        idf_lines.push_back("    DummySch,                !- Compressor Ambient Temperature Schedule Name");
        idf_lines.push_back("    Fan:OnOff,               !- Fan Object Type");
        idf_lines.push_back("    HPWH Fan " + i_str + ",                !- Fan Name");
        idf_lines.push_back("    DrawThrough,             !- Fan Placement");
        idf_lines.push_back("    0,                       !- On Cycle Parasitic Electric Load {W}");
        idf_lines.push_back("    0,                       !- Off Cycle Parasitic Electric Load {W}");
        idf_lines.push_back("    Outdoors,                !- Parasitic Heat Rejection Location");
        idf_lines.push_back("    ,                        !- Inlet Air Mixer Node Name");
        idf_lines.push_back("    ,                        !- Outlet Air Splitter Node Name");
        idf_lines.push_back("    ,                        !- Inlet Air Mixer Schedule Name");
        idf_lines.push_back("    MutuallyExclusive,       !- Tank Element Control Logic");
        idf_lines.push_back("    1.262,                   !- Control Sensor 1 Height In Stratified Tank");
        idf_lines.push_back("    0.75,                    !- Control Sensor 1 Weight");
        idf_lines.push_back("    0.464;                   !- Control Sensor 2 Height In Stratified Tank");
    }
    std::string const idf_objects = delimited_string(idf_lines);

    ASSERT_TRUE(process_idf(idf_objects));

    WaterThermalTanks::GetWaterThermalTankInput();

    for (int i = 1; i <= WaterThermalTanks::NumHeatPumpWaterHeater; ++i) {
        auto const &HPWH = WaterThermalTanks::HPWaterHeater(i);
        auto const &Tank = WaterThermalTanks::WaterThermalTank(HPWH.WaterHeaterTankNum);
        EXPECT_EQ(HPWH.CondWaterInletNode, Tank.SourceOutletNode);
        EXPECT_EQ(HPWH.CondWaterOutletNode, Tank.SourceInletNode);
    }
}

TEST_F(EnergyPlusFixture, HPWHEnergyBalance)
{
    using DataGlobals::HourOfDay;
    using DataGlobals::TimeStep;
    using DataGlobals::TimeStepZone;
    using DataHVACGlobals::SysTimeElapsed;
    using DataHVACGlobals::TimeStepSys;
    using FluidProperties::GetSpecificHeatGlycol;
    using FluidProperties::Water;
    using WaterThermalTanks::CalcHeatPumpWaterHeater;
    using WaterThermalTanks::HPWaterHeater;
    using WaterThermalTanks::WaterThermalTank;

    std::string idf_objects = delimited_string({
        "Schedule:Constant,",
        "    WaterHeaterSP1Schedule,  !- Name",
        "    ,             !- Schedule Type Limits Name",
        "    51.666666666666664;      !- Hourly Value",
        "Schedule:Constant,",
        "    HPWH_Tamb_1,             !- Name",
        "    ,             !- Schedule Type Limits Name",
        "    23;                      !- Hourly Value",
        "Schedule:Constant,",
        "    HPWH_RHamb_1,            !- Name",
        "    ,        !- Schedule Type Limits Name",
        "    0.5;                     !- Hourly Value",
        "Schedule:Constant,",
        "    HPWHBottomElementSetpoint_1,  !- Name",
        "    ,             !- Schedule Type Limits Name",
        "    -60;                     !- Hourly Value",
        "Schedule:Constant,",
        "    HPWHTopElementSetpoint_1,!- Name",
        "    ,             !- Schedule Type Limits Name",
        "    42.666566666666675;      !- Hourly Value",
        "Schedule:Constant, ZeroSch,,0.0;",
        "Schedule:Constant, UseInSch,,15.624554988670047;",
        "WaterHeater:Stratified,",
        "    Water Heater_1,          !- Name",
        "    Domestic Hot Water_1,    !- End-Use Subcategory",
        "    0.170343531,             !- Tank Volume {m3}",
        "    1.0335,                  !- Tank Height {m}",
        "    VerticalCylinder,        !- Tank Shape",
        "    ,                        !- Tank Perimeter {m}",
        "    ,                        !- Maximum Temperature Limit {C}",
        "    MasterSlave,             !- Heater Priority Control",
        "    HPWHTopElementSetpoint_1,!- Heater 1 Setpoint Temperature Schedule Name",
        "    25,                      !- Heater 1 Deadband Temperature Difference {deltaC}",
        "    4500,                    !- Heater 1 Capacity {W}",
        "    0.7320625,               !- Heater 1 Height {m}",
        "    HPWHBottomElementSetpoint_1,  !- Heater 2 Setpoint Temperature Schedule Name",
        "    30,                      !- Heater 2 Deadband Temperature Difference {deltaC}",
        "    4500,                    !- Heater 2 Capacity {W}",
        "    0.1291875,               !- Heater 2 Height {m}",
        "    Electricity,             !- Heater Fuel Type",
        "    1,                       !- Heater Thermal Efficiency",
        "    3,                       !- Off Cycle Parasitic Fuel Consumption Rate {W}",
        "    Electricity,             !- Off Cycle Parasitic Fuel Type",
        "    0,                       !- Off Cycle Parasitic Heat Fraction to Tank",
        "    0,                       !- Off Cycle Parasitic Height {m}",
        "    3,                       !- On Cycle Parasitic Fuel Consumption Rate {W}",
        "    Electricity,             !- On Cycle Parasitic Fuel Type",
        "    0,                       !- On Cycle Parasitic Heat Fraction to Tank",
        "    0,                       !- On Cycle Parasitic Height {m}",
        "    Schedule,                !- Ambient Temperature Indicator",
        "    HPWH_Tamb_1,             !- Ambient Temperature Schedule Name",
        "    ,                        !- Ambient Temperature Zone Name",
        "    ,                        !- Ambient Temperature Outdoor Air Node Name",
        "    1.132213669226055,       !- Uniform Skin Loss Coefficient per Unit Area to Ambient Temperature {W/m2-K}",
        "    1,                       !- Skin Loss Fraction to Zone",
        "    0,                       !- Off Cycle Flue Loss Coefficient to Ambient Temperature {W/K}",
        "    1,                       !- Off Cycle Flue Loss Fraction to Zone",
        "    0.00038754,              !- Peak Use Flow Rate {m3/s}",
        "    ZeroSch,                 !- Use Flow Rate Fraction Schedule Name",
        "    UseInSch,                !- Cold Water Supply Temperature Schedule Name",
        "    Water Heater Use Inlet Node_1,            !- Use Side Inlet Node Name",
        "    Water Heater Use Outlet Node_1,           !- Use Side Outlet Node Name",
        "    1,                       !- Use Side Effectiveness",
        "    0,                       !- Use Side Inlet Height {m}",
        "    autocalculate,           !- Use Side Outlet Height {m}",
        "    ,                        !- Source Side Inlet Node Name",
        "    ,                        !- Source Side Outlet Node Name",
        "    1,                       !- Source Side Effectiveness",
        "    0.1,                     !- Source Side Inlet Height {m}",
        "    0,                       !- Source Side Outlet Height {m}",
        "    Fixed,                   !- Inlet Mode",
        "    autosize,                !- Use Side Design Flow Rate {m3/s}",
        "    autosize,                !- Source Side Design Flow Rate {m3/s}",
        "    ,                        !- Indirect Water Heating Recovery Time {hr}",
        "    12,                      !- Number of Nodes",
        "    0;                       !- Additional Destratification Conductivity {W/m-K}",
        "WaterHeater:HeatPump:WrappedCondenser,",
        "    HPWH_1,                  !- Name",
        "    ,                        !- Availability Schedule Name",
        "    WaterHeaterSP1Schedule,  !- Compressor Setpoint Temperature Schedule Name",
        "    0.5,                     !- Dead Band Temperature Difference {deltaC}",
        "    0.0869862499999999,      !- Condenser Bottom Location {m}",
        "    0.5598125000000002,      !- Condenser Top Location {m}",
        "    0.08542248664,           !- Evaporator Air Flow Rate {m3/s}",
        "    Schedule,                !- Inlet Air Configuration",
        "    HPWH Air Inlet Node_1,   !- Air Inlet Node Name",
        "    HPWH Air Outlet Node_1,  !- Air Outlet Node Name",
        "    ,                        !- Outdoor Air Node Name",
        "    ,                        !- Exhaust Air Node Name",
        "    HPWH_Tamb_1,             !- Inlet Air Temperature Schedule Name",
        "    HPWH_RHamb_1,            !- Inlet Air Humidity Schedule Name",
        "    ,                        !- Inlet Air Zone Name",
        "    WaterHeater:Stratified,  !- Tank Object Type",
        "    Water Heater_1,          !- Tank Name",
        "    Water Heater Use Inlet Node_1,  !- Tank Use Side Inlet Node Name",
        "    Water Heater Use Outlet Node_1,  !- Tank Use Side Outlet Node Name",
        "    Coil:WaterHeating:AirToWaterHeatPump:Wrapped,  !- DX Coil Object Type",
        "    HPWH Coil_1,             !- DX Coil Name",
        "    7.222222222222222,       !- Minimum Inlet Air Temperature for Compressor Operation {C}",
        "    48.888888888888886,      !- Maximum Inlet Air Temperature for Compressor Operation {C}",
        "    Schedule,                !- Compressor Location",
        "    HPWH_Tamb_1,             !- Compressor Ambient Temperature Schedule Name",
        "    Fan:OnOff,               !- Fan Object Type",
        "    HPWH Fan_1,              !- Fan Name",
        "    DrawThrough,             !- Fan Placement",
        "    0,                       !- On Cycle Parasitic Electric Load {W}",
        "    0,                       !- Off Cycle Parasitic Electric Load {W}",
        "    Outdoors,                !- Parasitic Heat Rejection Location",
        "    ,                        !- Inlet Air Mixer Node Name",
        "    ,                        !- Outlet Air Splitter Node Name",
        "    ,                        !- Inlet Air Mixer Schedule Name",
        "    MutuallyExclusive,       !- Tank Element Control Logic",
        "    0.8181875000000001,      !- Control Sensor 1 Height In Stratified Tank {m}",
        "    1,                       !- Control Sensor 1 Weight {dimensionless}",
        "    0.8181875000000001;      !- Control Sensor 2 Height In Stratified Tank {m}",
        "Coil:WaterHeating:AirToWaterHeatPump:Wrapped,",
        "    HPWH Coil_1,             !- Name",
        "    1400,                    !- Rated Heating Capacity {W}",
        "    2.8,                     !- Rated COP {W/W}",
        "    0.88,                    !- Rated Sensible Heat Ratio",
        "    19.72222222222222,       !- Rated Evaporator Inlet Air Dry-Bulb Temperature {C}",
        "    13.533905564389693,      !- Rated Evaporator Inlet Air Wet-Bulb Temperature {C}",
        "    48.89,                   !- Rated Condenser Water Temperature {C}",
        "    0.08542248664,           !- Rated Evaporator Air Flow Rate {m3/s}",
        "    Yes,                     !- Evaporator Fan Power Included in Rated COP",
        "    HPWH Air Inlet Node_1,   !- Evaporator Air Inlet Node Name",
        "    HPWH CoilAirOutlet FanAirInlet_1,  !- Evaporator Air Outlet Node Name",
        "    0,                       !- Crankcase Heater Capacity {W}",
        "    0,                       !- Maximum Ambient Temperature for Crankcase Heater Operation {C}",
        "    WetBulbTemperature,      !- Evaporator Air Temperature Type for Curve Objects",
        "    HPWH-Cap-fT,             !- Heating Capacity Function of Temperature Curve Name",
        "    ,                        !- Heating Capacity Function of Air Flow Fraction Curve Name",
        "    HPWH-COP-fT;             !- Heating COP Function of Temperature Curve Name",
        "Curve:Biquadratic,",
        "    HPWH-Cap-fT,             !- Name",
        "    0.563,                   !- Coefficient1 Constant",
        "    0.0437,                  !- Coefficient2 x",
        "    0.000039,                !- Coefficient3 x**2",
        "    0.0055,                  !- Coefficient4 y",
        "    -0.000148,               !- Coefficient5 y**2",
        "    -0.000145,               !- Coefficient6 x*y",
        "    0,                       !- Minimum Value of x",
        "    100,                     !- Maximum Value of x",
        "    0,                       !- Minimum Value of y",
        "    100;                     !- Maximum Value of y",
        "Curve:Biquadratic,",
        "    HPWH-COP-fT,             !- Name",
        "    1.1332,                  !- Coefficient1 Constant",
        "    0.063,                   !- Coefficient2 x",
        "    -0.0000979,              !- Coefficient3 x**2",
        "    -0.00972,                !- Coefficient4 y",
        "    -0.0000214,              !- Coefficient5 y**2",
        "    -0.000686,               !- Coefficient6 x*y",
        "    0,                       !- Minimum Value of x",
        "    100,                     !- Maximum Value of x",
        "    0,                       !- Minimum Value of y",
        "    100;                     !- Maximum Value of y",
        "Fan:OnOff,",
        "    HPWH Fan_1,              !- Name",
        "    ,                !- Availability Schedule Name",
        "    0.2349521887445888,      !- Fan Total Efficiency",
        "    23,                      !- Pressure Rise {Pa}",
        "    0.08542248664,           !- Maximum Flow Rate {m3/s}",
        "    1,                       !- Motor Efficiency",
        "    1,                       !- Motor In Airstream Fraction",
        "    HPWH CoilAirOutlet FanAirInlet_1,  !- Air Inlet Node Name",
        "    HPWH Air Outlet Node_1,  !- Air Outlet Node Name",
        "    ,                        !- Fan Power Ratio Function of Speed Ratio Curve Name",
        "    ,                        !- Fan Efficiency Ratio Function of Speed Ratio Curve Name",
        "    Water Heating_1;         !- End-Use Subcategory",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    ASSERT_FALSE(WaterThermalTanks::GetWaterThermalTankInput());

    WaterThermalTanks::WaterThermalTankData &Tank = WaterThermalTank(1);
    WaterThermalTanks::HeatPumpWaterHeaterData &HPWH = HPWaterHeater(Tank.HeatPumpNum);
    DXCoils::DXCoilData &Coil = DXCoils::DXCoil(HPWH.DXCoilNum);
    Tank.Node(1).SavedTemp = 51.190278176501131;
    Tank.Node(2).SavedTemp = 51.190445301209223;
    Tank.Node(3).SavedTemp = 51.190593898651336;
    Tank.Node(4).SavedTemp = 51.190723967704933;
    Tank.Node(5).SavedTemp = 51.190835506744591;
    Tank.Node(6).SavedTemp = 51.190928513641957;
    Tank.Node(7).SavedTemp = 51.191002985765977;
    Tank.Node(8).SavedTemp = 51.191058919982886;
    Tank.Node(9).SavedTemp = 51.191096312656462;
    Tank.Node(10).SavedTemp = 51.191115159648149;
    Tank.Node(11).SavedTemp = 51.191115456317263;
    Tank.Node(12).SavedTemp = 50.719215567792681;
    Tank.TankTemp = 0.0;
    for (int i = 1; i <= Tank.Nodes; ++i) {
        Tank.Node(i).Temp = Tank.Node(i).SavedTemp;
        Tank.TankTemp += Tank.Node(i).Temp;
    }
    Tank.TankTemp /= Tank.Nodes;
    Tank.SavedHeaterOn1 = false;
    Tank.HeaterOn1 = Tank.SavedHeaterOn1;
    Tank.SavedHeaterOn2 = false;
    Tank.HeaterOn2 = Tank.SavedHeaterOn2;
    Tank.SavedUseOutletTemp = 51.213965403927645;
    Tank.UseOutletTemp = Tank.SavedUseOutletTemp;
    Tank.SavedSourceOutletTemp = 51.214754672592335;
    Tank.SourceOutletTemp = Tank.SavedSourceOutletTemp;
    Tank.UseInletTemp = 15.624554988670047;
    Tank.AmbientTemp = 23.0;

    HourOfDay = 0;
    TimeStep = 1;
    TimeStepZone = 10. / 60.;
    TimeStepSys = TimeStepZone;
    SysTimeElapsed = 0.0;
    Tank.TimeElapsed = HourOfDay + TimeStep * TimeStepZone + SysTimeElapsed;

    DataHVACGlobals::HPWHInletDBTemp = 21.666666666666668;
    DataHVACGlobals::HPWHInletWBTemp = 14.963459972723468;
    HPWH.SetPointTemp = 51.666666666666664;
    OutputReportPredefined::pdstHeatCoil = -1;
    WaterThermalTanks::MdotAir = 0.0993699992873531;

    int GlycolIndex = 0;
    const Real64 Cp = FluidProperties::GetSpecificHeatGlycol(Water, Tank.TankTemp, GlycolIndex, "HPWHEnergyBalance");

    CalcHeatPumpWaterHeater(1, false);

    const Real64 HeatFromCoil = Coil.TotalHeatingEnergyRate * TimeStepSys * 3600; // J
    Real64 TankEnergySum = 0;
    for (int i = 1; i <= Tank.Nodes; ++i) {
        const WaterThermalTanks::StratifiedNodeData &Node = Tank.Node(i);

        // I don't want to have to deal with source or use side heat transfer, so verify it's not happening.
        EXPECT_TRUE(Node.UseMassFlowRate == 0);
        EXPECT_TRUE(Node.SourceMassFlowRate == 0);

        // Sum up the energy accounted for in the change in tank temperature
        TankEnergySum += Node.Mass * Cp * (Node.Temp - Node.SavedTemp);
    }

    // Add back in the energy that was lost to ambient
    TankEnergySum -= Tank.LossRate * TimeStepSys * 3600;

    const Real64 ErrorBound = HeatFromCoil * 0.0001; // Within 0.01% of each other
    EXPECT_NEAR(HeatFromCoil, TankEnergySum, ErrorBound);
}

TEST_F(EnergyPlusFixture, HPWHSizing)
{
    std::string const idf_objects = delimited_string({
        "  Schedule:Constant, DummySch, , 1.0;",
        "  WaterHeater:HeatPump:PumpedCondenser,",
        "    Zone4HeatPumpWaterHeater,!- Name",
        "    ,  !- Availability Schedule Name",
        "    DummySch,             !- Compressor Setpoint Temperature Schedule Name",
        "    2.0,                     !- Dead Band Temperature Difference {deltaC}",
        "    Zone4WaterInletNode,     !- Condenser Water Inlet Node Name",
        "    Zone4WaterOutletNode,    !- Condenser Water Outlet Node Name",
        "    autocalculate,           !- Condenser Water Flow Rate {m3/s}",
        "    autocalculate,           !- Evaporator Air Flow Rate {m3/s}",
        "    ZoneAirOnly,             !- Inlet Air Configuration",
        "    Zone4AirOutletNode,      !- Air Inlet Node Name",
        "    Zone4AirInletNode,       !- Air Outlet Node Name",
        "    ,                        !- Outdoor Air Node Name",
        "    ,                        !- Exhaust Air Node Name",
        "    ,                        !- Inlet Air Temperature Schedule Name",
        "    ,                        !- Inlet Air Humidity Schedule Name",
        "    SPACE4-1,                !- Inlet Air Zone Name",
        "    WaterHeater:Mixed,       !- Tank Object Type",
        "    Zone4HPWHTank,           !- Tank Name",
        "    ,                        !- Tank Use Side Inlet Node Name",
        "    ,                        !- Tank Use Side Outlet Node Name",
        "    Coil:WaterHeating:AirToWaterHeatPump:Pumped,  !- DX Coil Object Type",
        "    Zone4HPWHDXCoil,         !- DX Coil Name",
        "    5.0,                     !- Minimum Inlet Air Temperature for Compressor Operation {C}",
        "    ,                        !- Maximum Inlet Air Temperature for Compressor Operation {C}",
        "    Zone,                    !- Compressor Location",
        "    ,                        !- Compressor Ambient Temperature Schedule Name",
        "    Fan:OnOff,               !- Fan Object Type",
        "    Zone4HPWHFan,            !- Fan Name",
        "    DrawThrough,             !- Fan Placement",
        "    15.0,                    !- On Cycle Parasitic Electric Load {W}",
        "    5.0,                     !- Off Cycle Parasitic Electric Load {W}",
        "    Zone;                    !- Parasitic Heat Rejection Location",
        "  WaterHeater:Mixed,",
        "    Zone4HPWHTank,           !- Name",
        "    0.3785,                  !- Tank Volume {m3}",
        "    DummySch,                !- Setpoint Temperature Schedule Name",
        "    2.0,                     !- Deadband Temperature Difference {deltaC}",
        "    82.2222,                 !- Maximum Temperature Limit {C}",
        "    CYCLE,                   !- Heater Control Type",
        "    5000,                    !- Heater Maximum Capacity {W}",
        "    0,                       !- Heater Minimum Capacity {W}",
        "    ,                        !- Heater Ignition Minimum Flow Rate {m3/s}",
        "    ,                        !- Heater Ignition Delay {s}",
        "    ELECTRICITY,             !- Heater Fuel Type",
        "    0.95,                    !- Heater Thermal Efficiency",
        "    ,                        !- Part Load Factor Curve Name",
        "    10,                      !- Off Cycle Parasitic Fuel Consumption Rate {W}",
        "    ELECTRICITY,             !- Off Cycle Parasitic Fuel Type",
        "    0,                       !- Off Cycle Parasitic Heat Fraction to Tank",
        "    30,                      !- On Cycle Parasitic Fuel Consumption Rate {W}",
        "    ELECTRICITY,             !- On Cycle Parasitic Fuel Type",
        "    0,                       !- On Cycle Parasitic Heat Fraction to Tank",
        "    Schedule,                !- Ambient Temperature Indicator",
        "    DummySch,                !- Ambient Temperature Schedule Name",
        "    ,                        !- Ambient Temperature Zone Name",
        "    ,                        !- Ambient Temperature Outdoor Air Node Name",
        "    2.0,                     !- Off Cycle Loss Coefficient to Ambient Temperature {W/K}",
        "    1.0,                     !- Off Cycle Loss Fraction to Zone",
        "    2.0,                     !- On Cycle Loss Coefficient to Ambient Temperature {W/K}",
        "    1.0,                     !- On Cycle Loss Fraction to Zone",
        "    0.00379,                 !- Peak Use Flow Rate {m3/s}",
        "    DummySch,                !- Use Flow Rate Fraction Schedule Name",
        "    ,                        !- Cold Water Supply Temperature Schedule Name",
        "    ,                        !- Use Side Inlet Node Name",
        "    ,                        !- Use Side Outlet Node Name",
        "    ,                        !- Use Side Effectiveness",
        "    Zone4WaterOutletNode,    !- Source Side Inlet Node Name",
        "    Zone4WaterInletNode,     !- Source Side Outlet Node Name",
        "    0.95;                    !- Source Side Effectiveness",
        "  Coil:WaterHeating:AirToWaterHeatPump:Pumped,",
        "    Zone4HPWHDXCoil,         !- Name",
        "    4000.0,                  !- Rated Heating Capacity {W}",
        "    3.2,                     !- Rated COP {W/W}",
        "    0.6956,                  !- Rated Sensible Heat Ratio",
        "    29.44,                   !- Rated Evaporator Inlet Air Dry-Bulb Temperature {C}",
        "    22.22,                   !- Rated Evaporator Inlet Air Wet-Bulb Temperature {C}",
        "    55.72,                   !- Rated Condenser Inlet Water Temperature {C}",
        "    autocalculate,           !- Rated Evaporator Air Flow Rate {m3/s}",
        "    autocalculate,           !- Rated Condenser Water Flow Rate {m3/s}",
        "    No,                      !- Evaporator Fan Power Included in Rated COP",
        "    No,                      !- Condenser Pump Power Included in Rated COP",
        "    No,                      !- Condenser Pump Heat Included in Rated Heating Capacity and Rated COP",
        "    150.0,                   !- Condenser Water Pump Power {W}",
        "    0.1,                     !- Fraction of Condenser Pump Heat to Water",
        "    Zone4AirOutletNode,      !- Evaporator Air Inlet Node Name",
        "    Zone4DXCoilAirOutletNode,!- Evaporator Air Outlet Node Name",
        "    Zone4WaterInletNode,     !- Condenser Water Inlet Node Name",
        "    Zone4WaterOutletNode,    !- Condenser Water Outlet Node Name",
        "    100.0,                   !- Crankcase Heater Capacity {W}",
        "    5.0,                     !- Maximum Ambient Temperature for Crankcase Heater Operation {C}",
        "    WetBulbTemperature,      !- Evaporator Air Temperature Type for Curve Objects",
        "    HPWHHeatingCapFTemp,     !- Heating Capacity Function of Temperature Curve Name",
        "    ,                        !- Heating Capacity Function of Air Flow Fraction Curve Name",
        "    ,                        !- Heating Capacity Function of Water Flow Fraction Curve Name",
        "    HPWHHeatingCOPFTemp,     !- Heating COP Function of Temperature Curve Name",
        "    ,                        !- Heating COP Function of Air Flow Fraction Curve Name",
        "    ,                        !- Heating COP Function of Water Flow Fraction Curve Name",
        "    HPWHPLFFPLR;             !- Part Load Fraction Correlation Curve Name",
        "  Fan:OnOff,",
        "    Zone4HPWHFan,            !- Name",
        "    ,  !- Availability Schedule Name",
        "    0.7,                     !- Fan Total Efficiency",
        "    100.0,                   !- Pressure Rise {Pa}",
        "    autosize,                  !- Maximum Flow Rate {m3/s}",
        "    0.9,                     !- Motor Efficiency",
        "    1.0,                     !- Motor In Airstream Fraction",
        "    Zone4DXCoilAirOutletNode,!- Air Inlet Node Name",
        "    Zone4AirInletNode;       !- Air Outlet Node Name",
        "  Curve:Biquadratic,",
        "    HPWHHeatingCapFTemp,     !- Name",
        "    0.369827,                !- Coefficient1 Constant",
        "    0.043341,                !- Coefficient2 x",
        "    -0.00023,                !- Coefficient3 x**2",
        "    0.000466,                !- Coefficient4 y",
        "    0.000026,                !- Coefficient5 y**2",
        "    -0.00027,                !- Coefficient6 x*y",
        "    0.0,                     !- Minimum Value of x",
        "    40.0,                    !- Maximum Value of x",
        "    20.0,                    !- Minimum Value of y",
        "    90.0,                    !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",
        "  Curve:Biquadratic,",
        "    HPWHHeatingCOPFTemp,     !- Name",
        "    1.19713,                 !- Coefficient1 Constant",
        "    0.077849,                !- Coefficient2 x",
        "    -0.0000016,              !- Coefficient3 x**2",
        "    -0.02675,                !- Coefficient4 y",
        "    0.000296,                !- Coefficient5 y**2",
        "    -0.00112,                !- Coefficient6 x*y",
        "    0.0,                     !- Minimum Value of x",
        "    40.0,                    !- Maximum Value of x",
        "    20.0,                    !- Minimum Value of y",
        "    90.0,                    !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",
        "  Curve:Quadratic,",
        "    HPWHPLFFPLR,             !- Name",
        "    0.75,                    !- Coefficient1 Constant",
        "    0.25,                    !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.0,                     !- Minimum Value of x",
        "    1.0;                     !- Maximum Value of x",
        "  Zone,",
        "    SPACE4-1,                !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    2.438400269,             !- Ceiling Height {m}",
        "    103.311355591;           !- Volume {m3}",
        "  ZoneHVAC:IdealLoadsAirSystem,",
        "    SPACE4-1 AirSys, !- Name",
        "    ,                        !- Availability Schedule Name",
        "    IdealLoadInNode,         !- Zone Supply Air Node Name",
        "    IdealLoadOutNode,        !- Zone Exhaust Air Node Name",
        "    ,                        !- System Inlet Air Node Name",
        "    50,                      !- Maximum Heating Supply Air Temperature {C}",
        "    13,                      !- Minimum Cooling Supply Air Temperature {C}",
        "    0.015,                   !- Maximum Heating Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "    0.009,                   !- Minimum Cooling Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "    NoLimit,                 !- Heating Limit",
        "    autosize,                !- Maximum Heating Air Flow Rate {m3/s}",
        "    ,                        !- Maximum Sensible Heating Capacity {W}",
        "    NoLimit,                 !- Cooling Limit",
        "    autosize,                !- Maximum Cooling Air Flow Rate {m3/s}",
        "    ,                        !- Maximum Total Cooling Capacity {W}",
        "    ,                        !- Heating Availability Schedule Name",
        "    ,                        !- Cooling Availability Schedule Name",
        "    ConstantSupplyHumidityRatio,  !- Dehumidification Control Type",
        "    ,                        !- Cooling Sensible Heat Ratio {dimensionless}",
        "    ConstantSupplyHumidityRatio,  !- Humidification Control Type",
        "    ,                        !- Design Specification Outdoor Air Object Name",
        "    ,                        !- Outdoor Air Inlet Node Name",
        "    ,                        !- Demand Controlled Ventilation Type",
        "    ,                        !- Outdoor Air Economizer Type",
        "    ,                        !- Heat Recovery Type",
        "    ,                        !- Sensible Heat Recovery Effectiveness {dimensionless}",
        "    ;                        !- Latent Heat Recovery Effectiveness {dimensionless}",
        "  ZoneHVAC:EquipmentConnections,",
        "    SPACE4-1,                !- Zone Name",
        "    SPACE4-1 Eq,             !- Zone Conditioning Equipment List Name",
        "    SPACE4-1 In Nodes,       !- Zone Air Inlet Node or NodeList Name",
        "    SPACE4-1 Out Nodes,      !- Zone Air Exhaust Node or NodeList Name",
        "    SPACE4-1 Node,           !- Zone Air Node Name",
        "    SPACE4-1 Out Node;       !- Zone Return Air Node Name",
        "  ZoneHVAC:EquipmentList,",
        "    SPACE4-1 Eq,             !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:IdealLoadsAirSystem,  !- Zone Equipment 1 Object Type",
        "    SPACE4-1 AirSys,            !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1,                       !- Zone Equipment 1 Heating or No-Load Sequence",
        "    ,                        !- Zone Equipment 1 Sequential Cooling Fraction",
        "    ,                        !- Zone Equipment 1 Sequential Heating Fraction",
        "    WaterHeater:HeatPump:PumpedCondenser,    !- Zone Equipment 2 Object Type",
        "    Zone4HeatPumpWaterHeater,!- Zone Equipment 2 Name",
        "    2,                       !- Zone Equipment 2 Cooling Sequence",
        "    2,                       !- Zone Equipment 2 Heating or No-Load Sequence",
        "    ,                        !- Zone Equipment 2 Sequential Cooling Fraction",
        "    ;                        !- Zone Equipment 2 Sequential Heating Fraction",
        "  NodeList,",
        "    SPACE4-1 In Nodes,       !- Name",
        "    SPACE4-1 In Node,        !- Node 1 Name",
        "    Zone4AirInletNode,       !- Node 2 Name",
        "    IdealLoadInNode;       !- Node 2 Name",
        "  NodeList,",
        "    SPACE4-1 Out Nodes,      !- Name",
        "    Zone4AirOutletNode,      !- Node 1 Name",
        "    IdealLoadOutNode;      !- Node 1 Name",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound = false;
    int CompIndex = 1;
    Real64 SenseLoadMet = 0;
    Real64 LatLoadMet = 0;
    HeatBalanceManager::GetZoneData(ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    DataGlobals::OutputFileInits = GetNewUnitNumber();
    DataHVACGlobals::TimeStepSys = 1;
    SetPredefinedTables();
    DataHeatBalFanSys::MAT.allocate(1);
    DataHeatBalFanSys::MAT(1) = 20.0;
    WaterThermalTanks::SimHeatPumpWaterHeater("Zone4HeatPumpWaterHeater", true, SenseLoadMet, LatLoadMet, CompIndex);
    EXPECT_EQ(Fans::Fan(1).MaxAirFlowRate, WaterThermalTanks::HPWaterHeater(1).OperatingAirFlowRate);
    EXPECT_EQ(Fans::Fan(1).MaxAirFlowRate, DXCoils::DXCoil(1).RatedAirVolFlowRate(1));
}

TEST_F(EnergyPlusFixture, WaterThermalTank_CalcTempIntegral)
{

    Real64 Ti = 57.22;   // Initial tank temperature (C)
    Real64 Tf = 52.22;   // Final tank temperature (C)
    Real64 Ta = 19.72;   // Ambient environment temperature (C)
    Real64 T1 = 14.44;   // Temperature of flow 1 (C)
    Real64 T2 = 0.00;    // Temperature of flow 2 (C)
    Real64 m = 148.67;   // Mass of tank fluid (kg)
    Real64 Cp = 4183.9;  // Specific heat of fluid (J/kg deltaC)
    Real64 m1 = 0.06761; // Mass flow rate 1 (kg/s)
    Real64 m2 = 0.00;    // Mass flow rate 2 (kg/s)
    Real64 UA = 5.0;     // Heat loss coefficient to ambient environment (W/deltaC)
    Real64 Q = 0.0;      // Net heating rate for non-temp dependent sources, i.e. heater and parasitics (W)
    Real64 t = 269.2;    // Time elapsed from Ti to Tf (s)

    EXPECT_NEAR(14716.6, WaterThermalTanks::CalcTempIntegral(Ti, Tf, Ta, T1, T2, m, Cp, m1, m2, UA, Q, t), 0.1);

    EXPECT_NEAR(0.0, WaterThermalTanks::CalcTempIntegral(Ti, Tf, Ta, T1, T2, m, Cp, m1, m2, UA, Q, 0.0), 0.1); // elapsed time is zero

    EXPECT_NEAR(15403.6,
                WaterThermalTanks::CalcTempIntegral(Ti, Ti, Ta, T1, T2, m, Cp, m1, m2, UA, Q, t),
                0.1); // final tank temperature same as initial tank temperature

    EXPECT_NEAR(15461.9,
                WaterThermalTanks::CalcTempIntegral(Ti, Tf, Ta, T1, T2, m, Cp, 0., 0., 0., 1000.0, t),
                0.1); // UA, m1, m2 all zero, Q = 1000W

    EXPECT_NEAR(14772.5, WaterThermalTanks::CalcTempIntegral(Ti, Tf, Ta, T1, T2, m, Cp, m1, m2, UA, 1000.0, t), 0.1); // Q = 1000W
}

TEST_F(EnergyPlusFixture, HPWHOutdoorAirMissingNodeNameWarning)
{
    std::string const idf_objects = delimited_string({
        "  Schedule:Constant, DummySch, , 1.0;",

        "  WaterHeater:HeatPump:PumpedCondenser,",
        "    Zone4HeatPumpWaterHeater,!- Name",
        "    ,                        !- Availability Schedule Name",
        "    DummySch,                !- Compressor Setpoint Temperature Schedule Name",
        "    2.0,                     !- Dead Band Temperature Difference {deltaC}",
        "    Zone4WaterInletNode,     !- Condenser Water Inlet Node Name",
        "    Zone4WaterOutletNode,    !- Condenser Water Outlet Node Name",
        "    autocalculate,           !- Condenser Water Flow Rate {m3/s}",
        "    autocalculate,           !- Evaporator Air Flow Rate {m3/s}",
        "    OutdoorAirOnly,          !- Inlet Air Configuration",
        "    Zone4AirOutletNode,      !- Air Inlet Node Name",
        "    Zone4AirInletNode,       !- Air Outlet Node Name",
        "    ,                        !- Outdoor Air Node Name",
        "    ,                        !- Exhaust Air Node Name",
        "    ,                        !- Inlet Air Temperature Schedule Name",
        "    ,                        !- Inlet Air Humidity Schedule Name",
        "    SPACE4-1,                !- Inlet Air Zone Name",
        "    WaterHeater:Mixed,       !- Tank Object Type",
        "    Zone4HPWHTank,           !- Tank Name",
        "    ,                        !- Tank Use Side Inlet Node Name",
        "    ,                        !- Tank Use Side Outlet Node Name",

        "    Coil:WaterHeating:AirToWaterHeatPump:Pumped,  !- DX Coil Object Type",
        "    Zone4HPWHDXCoil,         !- DX Coil Name",
        "    5.0,                     !- Minimum Inlet Air Temperature for Compressor Operation {C}",
        "    ,                        !- Maximum Inlet Air Temperature for Compressor Operation {C}",
        "    Outdoors,                !- Compressor Location",
        "    ,                        !- Compressor Ambient Temperature Schedule Name",
        "    Fan:OnOff,               !- Fan Object Type",
        "    Zone4HPWHFan,            !- Fan Name",
        "    DrawThrough,             !- Fan Placement",
        "    15.0,                    !- On Cycle Parasitic Electric Load {W}",
        "    5.0,                     !- Off Cycle Parasitic Electric Load {W}",
        "    ;                        !- Parasitic Heat Rejection Location",

        "  WaterHeater:Mixed,",
        "    Zone4HPWHTank,           !- Name",
        "    0.3785,                  !- Tank Volume {m3}",
        "    DummySch,                !- Setpoint Temperature Schedule Name",
        "    2.0,                     !- Deadband Temperature Difference {deltaC}",
        "    82.2222,                 !- Maximum Temperature Limit {C}",
        "    CYCLE,                   !- Heater Control Type",
        "    5000,                    !- Heater Maximum Capacity {W}",
        "    0,                       !- Heater Minimum Capacity {W}",
        "    ,                        !- Heater Ignition Minimum Flow Rate {m3/s}",
        "    ,                        !- Heater Ignition Delay {s}",
        "    ELECTRICITY,             !- Heater Fuel Type",
        "    0.95,                    !- Heater Thermal Efficiency",
        "    ,                        !- Part Load Factor Curve Name",
        "    10,                      !- Off Cycle Parasitic Fuel Consumption Rate {W}",
        "    ELECTRICITY,             !- Off Cycle Parasitic Fuel Type",
        "    0,                       !- Off Cycle Parasitic Heat Fraction to Tank",
        "    30,                      !- On Cycle Parasitic Fuel Consumption Rate {W}",
        "    ELECTRICITY,             !- On Cycle Parasitic Fuel Type",
        "    0,                       !- On Cycle Parasitic Heat Fraction to Tank",
        "    Schedule,                !- Ambient Temperature Indicator",
        "    DummySch,                !- Ambient Temperature Schedule Name",
        "    ,                        !- Ambient Temperature Zone Name",
        "    ,                        !- Ambient Temperature Outdoor Air Node Name",
        "    2.0,                     !- Off Cycle Loss Coefficient to Ambient Temperature {W/K}",
        "    1.0,                     !- Off Cycle Loss Fraction to Zone",
        "    2.0,                     !- On Cycle Loss Coefficient to Ambient Temperature {W/K}",
        "    1.0,                     !- On Cycle Loss Fraction to Zone",
        "    0.00379,                 !- Peak Use Flow Rate {m3/s}",
        "    DummySch,                !- Use Flow Rate Fraction Schedule Name",
        "    ,                        !- Cold Water Supply Temperature Schedule Name",
        "    ,                        !- Use Side Inlet Node Name",
        "    ,                        !- Use Side Outlet Node Name",
        "    ,                        !- Use Side Effectiveness",
        "    Zone4WaterOutletNode,    !- Source Side Inlet Node Name",
        "    Zone4WaterInletNode,     !- Source Side Outlet Node Name",
        "    0.95;                    !- Source Side Effectiveness",

        "  Coil:WaterHeating:AirToWaterHeatPump:Pumped,",
        "    Zone4HPWHDXCoil,         !- Name",
        "    4000.0,                  !- Rated Heating Capacity {W}",
        "    3.2,                     !- Rated COP {W/W}",
        "    0.6956,                  !- Rated Sensible Heat Ratio",
        "    29.44,                   !- Rated Evaporator Inlet Air Dry-Bulb Temperature {C}",
        "    22.22,                   !- Rated Evaporator Inlet Air Wet-Bulb Temperature {C}",
        "    55.72,                   !- Rated Condenser Inlet Water Temperature {C}",
        "    autocalculate,           !- Rated Evaporator Air Flow Rate {m3/s}",
        "    autocalculate,           !- Rated Condenser Water Flow Rate {m3/s}",
        "    No,                      !- Evaporator Fan Power Included in Rated COP",
        "    No,                      !- Condenser Pump Power Included in Rated COP",
        "    No,                      !- Condenser Pump Heat Included in Rated Heating Capacity and Rated COP",
        "    150.0,                   !- Condenser Water Pump Power {W}",
        "    0.1,                     !- Fraction of Condenser Pump Heat to Water",
        "    Zone4AirOutletNode,      !- Evaporator Air Inlet Node Name",
        "    Zone4DXCoilAirOutletNode,!- Evaporator Air Outlet Node Name",
        "    Zone4WaterInletNode,     !- Condenser Water Inlet Node Name",
        "    Zone4WaterOutletNode,    !- Condenser Water Outlet Node Name",
        "    100.0,                   !- Crankcase Heater Capacity {W}",
        "    5.0,                     !- Maximum Ambient Temperature for Crankcase Heater Operation {C}",
        "    WetBulbTemperature,      !- Evaporator Air Temperature Type for Curve Objects",
        "    HPWHHeatingCapFTemp,     !- Heating Capacity Function of Temperature Curve Name",
        "    ,                        !- Heating Capacity Function of Air Flow Fraction Curve Name",
        "    ,                        !- Heating Capacity Function of Water Flow Fraction Curve Name",
        "    HPWHHeatingCapFTemp,     !- Heating COP Function of Temperature Curve Name",
        "    ,                        !- Heating COP Function of Air Flow Fraction Curve Name",
        "    ,                        !- Heating COP Function of Water Flow Fraction Curve Name",
        "    HPWHPLFFPLR;             !- Part Load Fraction Correlation Curve Name",

        "  Fan:OnOff,",
        "    Zone4HPWHFan,            !- Name",
        "    ,  !- Availability Schedule Name",
        "    0.7,                     !- Fan Total Efficiency",
        "    100.0,                   !- Pressure Rise {Pa}",
        "    0.2685,                  !- Maximum Flow Rate {m3/s}",
        "    0.9,                     !- Motor Efficiency",
        "    1.0,                     !- Motor In Airstream Fraction",
        "    Zone4DXCoilAirOutletNode,!- Air Inlet Node Name",
        "    Zone4AirInletNode;       !- Air Outlet Node Name",

        "  Curve:Biquadratic,",
        "    HPWHHeatingCapFTemp,     !- Name",
        "    0.369827,                !- Coefficient1 Constant",
        "    0.043341,                !- Coefficient2 x",
        "    -0.00023,                !- Coefficient3 x**2",
        "    0.000466,                !- Coefficient4 y",
        "    0.000026,                !- Coefficient5 y**2",
        "    -0.00027,                !- Coefficient6 x*y",
        "    0.0,                     !- Minimum Value of x",
        "    40.0,                    !- Maximum Value of x",
        "    20.0,                    !- Minimum Value of y",
        "    90.0,                    !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "  Curve:Quadratic,",
        "    HPWHPLFFPLR,             !- Name",
        "    0.75,                    !- Coefficient1 Constant",
        "    0.25,                    !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.0,                     !- Minimum Value of x",
        "    1.0;                     !- Maximum Value of x",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound = false;
    ASSERT_FALSE(ErrorsFound);
    EXPECT_TRUE(WaterThermalTanks::GetWaterThermalTankInputData(ErrorsFound));
    ASSERT_TRUE(ErrorsFound);

    std::string const error_string =
        delimited_string({"   ** Severe  ** WaterHeater:HeatPump:PumpedCondenser=\"ZONE4HEATPUMPWATERHEATER\":",
                          "   **   ~~~   ** When Inlet Air Configuration=\"OUTDOORAIRONLY\".",
                          "   **   ~~~   ** Outdoor Air Node Name and Exhaust Air Node Name must be specified.",
                          "   ** Severe  ** WaterHeater:HeatPump:PumpedCondenser=\"ZONE4HEATPUMPWATERHEATER\":",
                          "   **   ~~~   ** Heat pump water heater fan outlet node name does not match next connected component.",
                          "   **   ~~~   ** Fan outlet node name = ZONE4AIRINLETNODE"});

    EXPECT_TRUE(compare_err_stream(error_string, true));
}

TEST_F(EnergyPlusFixture, HPWHTestSPControl)
{
    std::string const idf_objects = delimited_string({
        "  Schedule:Constant, CompSetSch, , 60.0;",
        "  Schedule:Constant, TankSetSch, , 30.0;",
        "  Schedule:Constant, AmbTempSch, , 25.0;",
        "  Schedule:Constant, UseFlowSch, , 0.0;",

        "  WaterHeater:HeatPump:PumpedCondenser,",
        "    HeatPumpWaterHeater,     !- Name",
        "    ,                        !- Availability Schedule Name",
        "    CompSetSch,              !- Compressor Setpoint Temperature Schedule Name",
        "    2.0,                     !- Dead Band Temperature Difference {deltaC}",
        "    HPWHWaterInletNode,      !- Condenser Water Inlet Node Name",
        "    HPWHWaterOutletNode,     !- Condenser Water Outlet Node Name",
        "    autocalculate,           !- Condenser Water Flow Rate {m3/s}",
        "    autocalculate,           !- Evaporator Air Flow Rate {m3/s}",
        "    OutdoorAirOnly,          !- Inlet Air Configuration",
        "    ,                        !- Air Inlet Node Name",
        "    ,                        !- Air Outlet Node Name",
        "    HPWHAirInletNode,        !- Outdoor Air Node Name",
        "    HPWHAirOutletNode,       !- Exhaust Air Node Name",
        "    ,                        !- Inlet Air Temperature Schedule Name",
        "    ,                        !- Inlet Air Humidity Schedule Name",
        "    ,                        !- Inlet Air Zone Name",
        "    WaterHeater:Mixed,       !- Tank Object Type",
        "    HPWHTank,                !- Tank Name",
        "    HPWHUseInletNode,        !- Tank Use Side Inlet Node Name",
        "    HPWHUseOutletNode,       !- Tank Use Side Outlet Node Name",
        "    Coil:WaterHeating:AirToWaterHeatPump:Pumped,  !- DX Coil Object Type",
        "    HPWHDXCoil,              !- DX Coil Name",
        "    5.0,                     !- Minimum Inlet Air Temperature for Compressor Operation {C}",
        "    ,                        !- Maximum Inlet Air Temperature for Compressor Operation {C}",
        "    Outdoors,                !- Compressor Location",
        "    ,                        !- Compressor Ambient Temperature Schedule Name",
        "    Fan:OnOff,               !- Fan Object Type",
        "    HPWHFan,                 !- Fan Name",
        "    DrawThrough,             !- Fan Placement",
        "    15.0,                    !- On Cycle Parasitic Electric Load {W}",
        "    5.0,                     !- Off Cycle Parasitic Electric Load {W}",
        "    ;                        !- Parasitic Heat Rejection Location",

        "  WaterHeater:Mixed,",
        "    HPWHTank,                !- Name",
        "    0.3785,                  !- Tank Volume {m3}",
        "    TankSetSch,              !- Setpoint Temperature Schedule Name",
        "    2.0,                     !- Deadband Temperature Difference {deltaC}",
        "    82.2222,                 !- Maximum Temperature Limit {C}",
        "    CYCLE,                   !- Heater Control Type",
        "    5000,                    !- Heater Maximum Capacity {W}",
        "    0,                       !- Heater Minimum Capacity {W}",
        "    ,                        !- Heater Ignition Minimum Flow Rate {m3/s}",
        "    ,                        !- Heater Ignition Delay {s}",
        "    ELECTRICITY,             !- Heater Fuel Type",
        "    0.95,                    !- Heater Thermal Efficiency",
        "    ,                        !- Part Load Factor Curve Name",
        "    10,                      !- Off Cycle Parasitic Fuel Consumption Rate {W}",
        "    ELECTRICITY,             !- Off Cycle Parasitic Fuel Type",
        "    0,                       !- Off Cycle Parasitic Heat Fraction to Tank",
        "    30,                      !- On Cycle Parasitic Fuel Consumption Rate {W}",
        "    ELECTRICITY,             !- On Cycle Parasitic Fuel Type",
        "    0,                       !- On Cycle Parasitic Heat Fraction to Tank",
        "    Schedule,                !- Ambient Temperature Indicator",
        "    AmbTempSch,              !- Ambient Temperature Schedule Name",
        "    ,                        !- Ambient Temperature Zone Name",
        "    ,                        !- Ambient Temperature Outdoor Air Node Name",
        "    0.0,                     !- Off Cycle Loss Coefficient to Ambient Temperature {W/K}",
        "    0.0,                     !- Off Cycle Loss Fraction to Zone",
        "    0.0,                     !- On Cycle Loss Coefficient to Ambient Temperature {W/K}",
        "    0.0,                     !- On Cycle Loss Fraction to Zone",
        "    0.00379,                 !- Peak Use Flow Rate {m3/s}",
        "    UseFlowSch,              !- Use Flow Rate Fraction Schedule Name",
        "    ,                        !- Cold Water Supply Temperature Schedule Name",
        "    HPWHUseInletNode,        !- Use Side Inlet Node Name",
        "    HPWHUseOutletNode,       !- Use Side Outlet Node Name",
        "    ,                        !- Use Side Effectiveness",
        "    HPWHWaterOutletNode,     !- Source Side Inlet Node Name",
        "    HPWHWaterInletNode,      !- Source Side Outlet Node Name",
        "    0.95;                    !- Source Side Effectiveness",

        "  Coil:WaterHeating:AirToWaterHeatPump:Pumped,",
        "    HPWHDXCoil,              !- Name",
        "    4000.0,                  !- Rated Heating Capacity {W}",
        "    3.2,                     !- Rated COP {W/W}",
        "    0.6956,                  !- Rated Sensible Heat Ratio",
        "    29.44,                   !- Rated Evaporator Inlet Air Dry-Bulb Temperature {C}",
        "    22.22,                   !- Rated Evaporator Inlet Air Wet-Bulb Temperature {C}",
        "    55.72,                   !- Rated Condenser Inlet Water Temperature {C}",
        "    autocalculate,           !- Rated Evaporator Air Flow Rate {m3/s}",
        "    autocalculate,           !- Rated Condenser Water Flow Rate {m3/s}",
        "    No,                      !- Evaporator Fan Power Included in Rated COP",
        "    No,                      !- Condenser Pump Power Included in Rated COP",
        "    No,                      !- Condenser Pump Heat Included in Rated Heating Capacity and Rated COP",
        "    150.0,                   !- Condenser Water Pump Power {W}",
        "    0.1,                     !- Fraction of Condenser Pump Heat to Water",
        "    HPWHAirInletNode,        !- Evaporator Air Inlet Node Name",
        "    DXCoilAirOutletNode,     !- Evaporator Air Outlet Node Name",
        "    HPWHWaterInletNode,      !- Condenser Water Inlet Node Name",
        "    HPWHWaterOutletNode,     !- Condenser Water Outlet Node Name",
        "    100.0,                   !- Crankcase Heater Capacity {W}",
        "    5.0,                     !- Maximum Ambient Temperature for Crankcase Heater Operation {C}",
        "    WetBulbTemperature,      !- Evaporator Air Temperature Type for Curve Objects",
        "    HPWHHeatingCapFTemp,     !- Heating Capacity Function of Temperature Curve Name",
        "    ,                        !- Heating Capacity Function of Air Flow Fraction Curve Name",
        "    ,                        !- Heating Capacity Function of Water Flow Fraction Curve Name",
        "    HPWHHeatingCapFTemp,     !- Heating COP Function of Temperature Curve Name",
        "    ,                        !- Heating COP Function of Air Flow Fraction Curve Name",
        "    ,                        !- Heating COP Function of Water Flow Fraction Curve Name",
        "    HPWHPLFFPLR;             !- Part Load Fraction Correlation Curve Name",

        "  Fan:OnOff,",
        "    HPWHFan,                 !- Name",
        "    ,  !- Availability Schedule Name",
        "    0.7,                     !- Fan Total Efficiency",
        "    100.0,                   !- Pressure Rise {Pa}",
        "    0.2685,                  !- Maximum Flow Rate {m3/s}",
        "    0.9,                     !- Motor Efficiency",
        "    1.0,                     !- Motor In Airstream Fraction",
        "    DXCoilAirOutletNode,     !- Air Inlet Node Name",
        "    HPWHAirOutletNode;       !- Air Outlet Node Name",

        "  Curve:Biquadratic,",
        "    HPWHHeatingCapFTemp,     !- Name",
        "    0.369827,                !- Coefficient1 Constant",
        "    0.043341,                !- Coefficient2 x",
        "    -0.00023,                !- Coefficient3 x**2",
        "    0.000466,                !- Coefficient4 y",
        "    0.000026,                !- Coefficient5 y**2",
        "    -0.00027,                !- Coefficient6 x*y",
        "    0.0,                     !- Minimum Value of x",
        "    40.0,                    !- Maximum Value of x",
        "    20.0,                    !- Minimum Value of y",
        "    90.0,                    !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "  Curve:Quadratic,",
        "    HPWHPLFFPLR,             !- Name",
        "    0.75,                    !- Coefficient1 Constant",
        "    0.25,                    !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.0,                     !- Minimum Value of x",
        "    1.0;                     !- Maximum Value of x",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    DataGlobals::OutputFileInits = GetNewUnitNumber();
    DataHVACGlobals::TimeStepSys = 1;
    DataGlobals::NumOfTimeStepInHour = 1;
    DataGlobals::MinutesPerTimeStep = 60 / DataGlobals::NumOfTimeStepInHour;
    DataGlobals::TimeStep = 1;
    DataGlobals::HourOfDay = 1;
    DataEnvironment::DayOfWeek = 1;
    DataEnvironment::DayOfYear_Schedule = 1;
    SetPredefinedTables();
    ScheduleManager::UpdateScheduleValues();

    EXPECT_FALSE(WaterThermalTanks::GetWaterThermalTankInput());

    WaterThermalTanks::WaterThermalTankData &Tank = WaterThermalTanks::WaterThermalTank(1);
    WaterThermalTanks::HeatPumpWaterHeaterData &HeatPump = WaterThermalTanks::HPWaterHeater(1);
    DataHVACGlobals::HPWHInletDBTemp = 30.0;
    DataEnvironment::WaterMainsTemp = 40.0;
    DataHVACGlobals::DXCoilTotalCapacity = 3500.0;
    DXCoils::HPWHHeatingCapacity = 4000.0;

    DataLoopNode::Node(3).Temp = 30.0;
    DataLoopNode::Node(3).HumRat = 0.01;
    DataEnvironment::OutBaroPress = 101325.0;

    bool FirstHVACIteration(true);
    DataGlobals::WarmupFlag = true;

    //	HeatPump.SetPointTemp = 60.0, deadband = 2C, HP on at 58 C and off at 60 C
    //	Tank.SetPointTemp = 30.0, tank elements should not be used
    //	Tank.TankTemp = 60.0; // based on schedule
    //	Tank.SavedTankTemp = 60.0;
    HeatPump.SaveMode = WaterThermalTanks::FloatMode;
    Tank.Mode = WaterThermalTanks::FloatMode;
    WaterThermalTanks::InitWaterThermalTank(1, FirstHVACIteration);
    DataGlobals::WarmupFlag = false;
    WaterThermalTanks::InitWaterThermalTank(1, FirstHVACIteration); // read set point schedules on second pass when WarmupFlag is false.
    WaterThermalTanks::CalcHeatPumpWaterHeater(1, FirstHVACIteration);
    WaterThermalTanks::UpdateWaterThermalTank(1);
    // no standby losses, tank at 60 C, tank should remain at 60 C and HP should be off.
    EXPECT_NEAR(60.0, Tank.TankTemp, 0.0000001);
    EXPECT_NEAR(0.0, HeatPump.HeatingPLR, 0.0000001);
    EXPECT_EQ(WaterThermalTanks::FloatMode, HeatPump.Mode); // expect HP to remain in floating mode
    EXPECT_NEAR(60.0, Tank.TankTempAvg, 0.0000001);         // average tank temp over time step
    EXPECT_NEAR(60.0, Tank.SourceOutletTemp, 0.0000001);    // source outlet = average tank temp

    FirstHVACIteration = false;

    // HP in heating mode and tank at low temp needing full HP operation. Use nodes not adding heat to tank.
    Tank.TankTemp = 50.0;
    Tank.SavedTankTemp = 50.0;
    HeatPump.SaveMode = WaterThermalTanks::HeatMode;
    Tank.SavedMode = WaterThermalTanks::HeatMode;
    WaterThermalTanks::InitWaterThermalTank(1, FirstHVACIteration);
    WaterThermalTanks::CalcHeatPumpWaterHeater(1, FirstHVACIteration);
    WaterThermalTanks::UpdateWaterThermalTank(1);
    // no standby losses, tank at 50 C, tank should heat up and HP should be on.
    EXPECT_NEAR(57.2000377, Tank.TankTemp, 0.0000001);         // final tank temperature
    EXPECT_NEAR(1.0, HeatPump.HeatingPLR, 0.0000001);          // HP operating at full capacity
    EXPECT_EQ(WaterThermalTanks::HeatMode, HeatPump.Mode);     // expect HP to remain in heating mode
    EXPECT_NEAR(53.6000188, Tank.TankTempAvg, 0.0000001);      // average tank temp over time step
    EXPECT_NEAR(53.6000188, Tank.SourceOutletTemp, 0.0000001); // source outlet = average tank temp

    // HP in heating mode and tank at moderate temp needing only partial HP operation. Use nodes not adding heat to tank.
    Tank.TankTemp = 56.0;
    Tank.SavedTankTemp = 56.0;
    HeatPump.SaveMode = WaterThermalTanks::HeatMode;
    Tank.SavedMode = WaterThermalTanks::HeatMode;
    WaterThermalTanks::CalcHeatPumpWaterHeater(1, FirstHVACIteration);
    WaterThermalTanks::UpdateWaterThermalTank(1);
    // no standby losses, tank at 56 C, tank should heat up to 60 C (within convergence tolerance) and HP should cycle.
    EXPECT_NEAR(60.00110205, Tank.TankTemp, 0.0000001);
    EXPECT_NEAR(0.5550125, HeatPump.HeatingPLR, 0.0000001);
    EXPECT_EQ(WaterThermalTanks::FloatMode, HeatPump.Mode);     // expect HP to switch to floating mode since it reached set point
    EXPECT_NEAR(58.00055103, Tank.TankTempAvg, 0.0000001);      // average tank temp over time step
    EXPECT_NEAR(58.00055103, Tank.SourceOutletTemp, 0.0000001); // source outlet = average tank temp

    // HP in heating mode and tank at moderate temp with use node adding heat to tank
    Tank.TankTemp = 56.0;
    Tank.SavedTankTemp = 56.0;
    HeatPump.SaveMode = WaterThermalTanks::HeatMode;
    Tank.SavedMode = WaterThermalTanks::HeatMode;
    Tank.UseMassFlowRate = 0.02;
    Tank.UseInletTemp = 90.0;
    WaterThermalTanks::CalcHeatPumpWaterHeater(1, FirstHVACIteration);
    WaterThermalTanks::UpdateWaterThermalTank(1);
    // no standby losses, tank at 56 C, tank should heat up > 60 C since use nodes add heat to tank and HP should be off and floating.
    EXPECT_NEAR(61.96991668, Tank.TankTemp, 0.0000001);
    EXPECT_NEAR(0.0, HeatPump.HeatingPLR, 0.0000001);
    EXPECT_EQ(WaterThermalTanks::FloatMode,
              HeatPump.Mode); // expect HP to switch to floating mode since use nodes added sufficient heat to exceed set point
    EXPECT_NEAR(59.08095576, Tank.TankTempAvg, 0.0000001);      // average tank temp over time step
    EXPECT_NEAR(59.08095576, Tank.SourceOutletTemp, 0.0000001); // source outlet = average tank temp

    // HP in floating mode and tank at moderate temp with use node adding heat to tank
    Tank.TankTemp = 56.0;
    Tank.SavedTankTemp = 56.0;
    HeatPump.SaveMode = WaterThermalTanks::FloatMode;
    Tank.SavedMode = WaterThermalTanks::FloatMode;
    Tank.UseMassFlowRate = 0.02;
    Tank.UseInletTemp = 90.0;
    WaterThermalTanks::CalcHeatPumpWaterHeater(1, FirstHVACIteration);
    WaterThermalTanks::UpdateWaterThermalTank(1);
    // no standby losses, tank at 56 C, tank should heat up > 60 C since use nodes add heat to tank and HP should be off and floating.
    EXPECT_NEAR(61.96991668, Tank.TankTemp, 0.0000001);
    EXPECT_NEAR(0.0, HeatPump.HeatingPLR, 0.0000001);
    EXPECT_EQ(WaterThermalTanks::FloatMode,
              HeatPump.Mode); // expect HP to remain in floating mode since use nodes added sufficient heat to exceed set point
    EXPECT_NEAR(59.08095576, Tank.TankTempAvg, 0.0000001);      // average tank temp over time step
    EXPECT_NEAR(59.08095576, Tank.SourceOutletTemp, 0.0000001); // source outlet = average tank temp

    // HP in floating mode and HP set point temp was reduced (#5400)
    Tank.TankTemp = 56.0;
    Tank.SavedTankTemp = 56.0;
    HeatPump.SetPointTemp = 30.0; // SP reduced below tank temperature while in heating mode
    HeatPump.SaveMode = WaterThermalTanks::HeatMode;
    Tank.SavedMode = WaterThermalTanks::HeatMode;
    Tank.UseMassFlowRate = 0.0;
    Tank.UseInletTemp = 90.0;
    WaterThermalTanks::CalcHeatPumpWaterHeater(1, FirstHVACIteration);
    WaterThermalTanks::UpdateWaterThermalTank(1);
    // no standby losses, tank at 56 C, tank should remain at 56 C since use nodes are not adding heat to tank and HP set point temp was reduced
    EXPECT_NEAR(56.0, Tank.TankTemp, 0.0000001);
    EXPECT_NEAR(0.0, HeatPump.HeatingPLR, 0.0000001);
    EXPECT_EQ(WaterThermalTanks::FloatMode, HeatPump.Mode); // expect HP to switch to floating mode since HP set point temperature was reduced
    EXPECT_NEAR(56.0, Tank.TankTempAvg, 0.0000001);         // average tank temp over time step
    EXPECT_NEAR(56.0, Tank.SourceOutletTemp, 0.0000001);    // source outlet = average tank temp
}

TEST_F(EnergyPlusFixture, StratifiedTankUseEnergy)
{
    using DataGlobals::HourOfDay;
    using DataGlobals::TimeStep;
    using DataGlobals::TimeStepZone;
    using DataHVACGlobals::SysTimeElapsed;
    using DataHVACGlobals::TimeStepSys;
    using WaterThermalTanks::WaterThermalTank;

    std::string const idf_objects = delimited_string({
        "Schedule:Constant, Hot Water Demand Schedule, , 1.0;",
        "Schedule:Constant, Ambient Temp Schedule, , 20.0;",
        "Schedule:Constant, Inlet Water Temperature, , 10.0;",
        "Schedule:Constant, Hot Water Setpoint Temp Schedule, , 48.89;",
        "WaterHeater:Stratified,",
        "  Stratified Tank,         !- Name",
        "  ,                        !- End-Use Subcategory",
        "  0.17,                    !- Tank Volume {m3}",
        "  1.4,                     !- Tank Height {m}",
        "  VerticalCylinder,        !- Tank Shape",
        "  ,                        !- Tank Perimeter {m}",
        "  82.2222,                 !- Maximum Temperature Limit {C}",
        "  MasterSlave,             !- Heater Priority Control",
        "  Hot Water Setpoint Temp Schedule,  !- Heater 1 Setpoint Temperature Schedule Name",
        "  2.0,                     !- Heater 1 Deadband Temperature Difference {deltaC}",
        "  4500,                    !- Heater 1 Capacity {W}",
        "  1.0,                     !- Heater 1 Height {m}",
        "  Hot Water Setpoint Temp Schedule,  !- Heater 2 Setpoint Temperature Schedule Name",
        "  5.0,                     !- Heater 2 Deadband Temperature Difference {deltaC}",
        "  4500,                    !- Heater 2 Capacity {W}",
        "  0.0,                     !- Heater 2 Height {m}",
        "  ELECTRICITY,             !- Heater Fuel Type",
        "  1,                    !- Heater Thermal Efficiency",
        "  ,                        !- Off Cycle Parasitic Fuel Consumption Rate {W}",
        "  ELECTRICITY,             !- Off Cycle Parasitic Fuel Type",
        "  ,                        !- Off Cycle Parasitic Heat Fraction to Tank",
        "  ,                        !- Off Cycle Parasitic Height {m}",
        "  ,                        !- On Cycle Parasitic Fuel Consumption Rate {W}",
        "  ELECTRICITY,             !- On Cycle Parasitic Fuel Type",
        "  ,                        !- On Cycle Parasitic Heat Fraction to Tank",
        "  ,                        !- On Cycle Parasitic Height {m}",
        "  SCHEDULE,                !- Ambient Temperature Indicator",
        "  Ambient Temp Schedule,   !- Ambient Temperature Schedule Name",
        "  ,                        !- Ambient Temperature Zone Name",
        "  ,                        !- Ambient Temperature Outdoor Air Node Name",
        "  0,                   !- Uniform Skin Loss Coefficient per Unit Area to Ambient Temperature {W/m2-K}",
        "  ,                        !- Skin Loss Fraction to Zone",
        "  ,                        !- Off Cycle Flue Loss Coefficient to Ambient Temperature {W/K}",
        "  ,                        !- Off Cycle Flue Loss Fraction to Zone",
        "  0.000189,                !- Peak Use Flow Rate {m3/s}",
        "  Hot Water Demand Schedule,  !- Use Flow Rate Fraction Schedule Name",
        "  Inlet Water Temperature,                        !- Cold Water Supply Temperature Schedule Name",
        "  ,                        !- Use Side Inlet Node Name",
        "  ,                        !- Use Side Outlet Node Name",
        "  ,                        !- Use Side Effectiveness",
        "  1.0,                        !- Use Side Inlet Height {m}",
        "  0.5,                        !- Use Side Outlet Height {m}",
        "  ,                        !- Source Side Inlet Node Name",
        "  ,                        !- Source Side Outlet Node Name",
        "  ,                        !- Source Side Effectiveness",
        "  ,                        !- Source Side Inlet Height {m}",
        "  ,                        !- Source Side Outlet Height {m}",
        "  FIXED,                   !- Inlet Mode",
        "  ,                        !- Use Side Design Flow Rate {m3/s}",
        "  ,                        !- Source Side Design Flow Rate {m3/s}",
        "  ,                        !- Indirect Water Heating Recovery Time {hr}",
        "  10,                      !- Number of Nodes",
        "  0.1;                     !- Additional Destratification Conductivity {W/m-K}",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound = false;
    EXPECT_FALSE(WaterThermalTanks::GetWaterThermalTankInputData(ErrorsFound));

    WaterThermalTanks::WaterThermalTankData &Tank = WaterThermalTank(1);

    for (int i = 1; i <= Tank.Nodes; ++i) {
        auto &node = Tank.Node(i);
        node.SavedTemp = 48.89;
        node.Temp = 48.89;
    }

    Tank.TankTemp = 48.89;
    HourOfDay = 0;
    TimeStep = 1;
    TimeStepZone = 10. / 60.;
    TimeStepSys = TimeStepZone;
    SysTimeElapsed = 0.0;
    Tank.TimeElapsed = HourOfDay + TimeStep * TimeStepZone + SysTimeElapsed;
    Tank.AmbientTemp = 20.0;
    Tank.UseInletTemp = 10.0;
    Tank.SetPointTemp = 48.89;
    Tank.SetPointTemp2 = Tank.SetPointTemp;
    Tank.UseMassFlowRate = 0.000189;

    WaterThermalTanks::CalcWaterThermalTankStratified(1);

    // Energy Use is negative relative to the tank
    ASSERT_LT(Tank.UseRate, 0.0);
}

TEST_F(EnergyPlusFixture, StratifiedTankSourceTemperatures)
{
    using DataGlobals::HourOfDay;
    using DataGlobals::TimeStep;
    using DataGlobals::TimeStepZone;
    using DataHVACGlobals::SysTimeElapsed;
    using DataHVACGlobals::TimeStepSys;
    using WaterThermalTanks::WaterThermalTank;

    std::string const idf_objects = delimited_string({
        "Schedule:Constant, Hot Water Demand Schedule, , 1.0;",
        "Schedule:Constant, Ambient Temp Schedule, , 20.0;",
        "Schedule:Constant, Inlet Water Temperature, , 10.0;",
        "Schedule:Constant, CW-Tank-Temp-Schedule, , 7.5;",

        "  Zone,",
        "    Zone_TES,                !- Name",
        "    0.0000,                  !- Direction of Relative North {deg}",
        "    10.0,                    !- X Origin {m}",
        "    10.0,                    !- Y Origin {m}",
        "    0.0,                     !- Z Origin {m}",
        "    1,                       !- Type",
        "    1.00,                    !- Multiplier",
        "    3.00,                    !- Ceiling Height {m}",
        "    300.0,                   !- Volume {m3}",
        "    100.0;                   !- Floor Area {m2}",

        "  Schedule:Compact,",
        "    ALWAYS_ON,               !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,1.0;        !- Field 3",

        "  ThermalStorage:ChilledWater:Stratified,",
        "    Chilled Water Storage,   !- Name",
        "    50.0,                    !- Tank Volume {m3}",
        "    8.0,                     !- Tank Height {m}",
        "    VerticalCylinder,        !- Tank Shape",
        "    ,                        !- Tank Perimeter {m}",
        "    CW-Tank-Temp-Schedule,   !- Setpoint Temperature Schedule Name",
        "    2.5,                     !- Deadband Temperature Difference {deltaC}",
        "    6.5,                     !- Temperature Sensor Height {m}",
        "    1.0,                     !- Minimum Temperature Limit {C}",
        "    50000,                   !- Nominal Cooling Capacity {W}",
        "    Zone,                    !- Ambient Temperature Indicator",
        "    ,                        !- Ambient Temperature Schedule Name",
        "    Zone_TES,                !- Ambient Temperature Zone Name",
        "    ,                        !- Ambient Temperature Outdoor Air Node Name",
        "    1.0,                     !- Uniform Skin Loss Coefficient per Unit Area to Ambient Temperature {W/m2-K}",
        "    CW Tank Discharge Inlet node,  !- Use Side Inlet Node Name",
        "    CW Tank Discharge Outlet node,  !- Use Side Outlet Node Name",
        "    1.0,                     !- Use Side Heat Transfer Effectiveness",
        "    ALWAYS_ON,               !- Use Side Availability Schedule Name",
        "    7.85,                    !- Use Side Inlet Height {m}",
        "    0.15,                    !- Use Side Outlet Height {m}",
        "    5.0E-3,                  !- Use Side Design Flow Rate {m3/s}",
        "    CW Tank Charge Inlet Node,  !- Source Side Inlet Node Name",
        "    CW Tank Charge Outlet Node,  !- Source Side Outlet Node Name",
        "    1.0,                     !- Source Side Heat Transfer Effectiveness",
        "    ALWAYS_ON,               !- Source Side Availability Schedule Name",
        "    0.15,                    !- Source Side Inlet Height {m}",
        "    7.85,                    !- Source Side Outlet Height {m}",
        "    5.0E-3,                  !- Source Side Design Flow Rate {m3/s}",
        "    4.0,                     !- Tank Recovery Time {hr}",
        "    Seeking,                 !- Inlet Mode",
        "    6,                       !- Number of Nodes",
        "    0.0;                     !- Additional Destratification Conductivity {W/m-K}",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound = false;
    HeatBalanceManager::GetZoneData(ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);

    InternalHeatGains::GetInternalHeatGainsInput();

    DataGlobals::NumOfTimeStepInHour = 1; // must initialize this to get schedules initialized
    DataGlobals::MinutesPerTimeStep = 60; // must initialize this to get schedules initialized
    ScheduleManager::ProcessScheduleInput();
    ScheduleManager::ScheduleInputProcessed = true;

    DataGlobals::TimeStep = 1;
    DataGlobals::HourOfDay = 1;
    DataEnvironment::Month = 7;
    DataEnvironment::DayOfMonth = 21;
    DataGlobals::HourOfDay = 1;
    DataEnvironment::DSTIndicator = 0;
    DataEnvironment::DayOfWeek = 2;
    DataEnvironment::HolidayIndex = 0;
    DataEnvironment::DayOfYear_Schedule = General::OrdinalDay(DataEnvironment::Month, DataEnvironment::DayOfMonth, 1);
    ScheduleManager::UpdateScheduleValues();

    int TankNum(1);
    ErrorsFound = false;
    EXPECT_FALSE(WaterThermalTanks::GetWaterThermalTankInputData(ErrorsFound));

    WaterThermalTanks::WaterThermalTankData &Tank = WaterThermalTank(TankNum);

    for (int i = 1; i <= Tank.Nodes; ++i) {
        auto &node = Tank.Node(i);
        node.SavedTemp = 7.5;
        node.Temp = 7.5;
    }

    Tank.TankTemp = 7.5;
    Tank.AmbientTemp = 20.0;
    Tank.UseInletTemp = 20.0;
    Tank.SetPointTemp = 7.5;
    Tank.SetPointTemp2 = Tank.SetPointTemp;
    Tank.UseMassFlowRate = 5.0;
    Tank.SourceInletTemp = 5.0;
    Tank.SourceOutletTemp = 7.5;
    Tank.SourceMassFlowRate = 5.0;
    Tank.TimeElapsed = 0.0;

    HourOfDay = 0;
    TimeStep = 1;
    TimeStepZone = 15. / 60.;
    TimeStepSys = TimeStepZone;
    SysTimeElapsed = 0.0;

    WaterThermalTanks::CalcWaterThermalTankStratified(TankNum);

    // check source inlet and outlet temperatures are different
    EXPECT_EQ(Tank.SourceInletTemp, 5.0);
    EXPECT_NEAR(Tank.SourceOutletTemp, 10.34, 0.01);
}


TEST_F(EnergyPlusFixture, MixedTankTimeNeededCalc)
{
    using DataGlobals::HourOfDay;
    using DataGlobals::TimeStep;
    using DataGlobals::TimeStepZone;
    using DataHVACGlobals::SysTimeElapsed;
    using DataHVACGlobals::TimeStepSys;
    using WaterThermalTanks::WaterThermalTank;

    std::string const idf_objects = delimited_string({
        "  Schedule:Constant, Hot Water Demand Schedule, , 1.0;",
        "  Schedule:Constant, Inlet Water Temperature, , 10.0;",
        "  Schedule:Constant, Water Heater Setpoint Temperature, ,60;",

        "  Zone,",
        "    BasementZone,            !- Name",
        "   -0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0;                       !- Z Origin {m}",

        "  WaterHeater:Mixed,",
        "    ChilledWaterTank,        !- Name",
        "    0.07,                    !- Tank Volume {m3}",
        "    Water Heater Setpoint Temperature,  !- Setpoint Temperature Schedule Name",
        "    2,                       !- Deadband Temperature Difference {deltaC}",
        "    30,                      !- Maximum Temperature Limit {C}",
        "    Cycle,                   !- Heater Control Type",
        "    0,                       !- Heater Maximum Capacity {W}",
        "    ,                        !- Heater Minimum Capacity {W}",
        "    0,                       !- Heater Ignition Minimum Flow Rate {m3/s}",
        "    0,                       !- Heater Ignition Delay {s}",
        "    Electricity,             !- Heater Fuel Type",
        "    0.8,                     !- Heater Thermal Efficiency",
        "    ,                        !- Part Load Factor Curve Name",
        "    0,                       !- Off Cycle Parasitic Fuel Consumption Rate {W}",
        "    Electricity,             !- Off Cycle Parasitic Fuel Type",
        "    0.8,                     !- Off Cycle Parasitic Heat Fraction to Tank",
        "    0,                       !- On Cycle Parasitic Fuel Consumption Rate {W}",
        "    Electricity,             !- On Cycle Parasitic Fuel Type",
        "    0,                       !- On Cycle Parasitic Heat Fraction to Tank",
        "    Zone,                    !- Ambient Temperature Indicator",
        "    ,                        !- Ambient Temperature Schedule Name",
        "    BasementZone,            !- Ambient Temperature Zone Name",
        "    ,                        !- Ambient Temperature Outdoor Air Node Name",
        "    1,                       !- Off Cycle Loss Coefficient to Ambient Temperature {W/K}",
        "    1,                       !- Off Cycle Loss Fraction to Zone",
        "    6,                       !- On Cycle Loss Coefficient to Ambient Temperature {W/K}",
        "    1,                       !- On Cycle Loss Fraction to Zone",
        "    ,                        !- Peak Use Flow Rate {m3/s}",
        "    ,                        !- Use Flow Rate Fraction Schedule Name",
        "    ,                        !- Cold Water Supply Temperature Schedule Name",
        "    ,                        !- Use Side Inlet Node Name",
        "    ,                        !- Use Side Outlet Node Name",
        "    1,                       !- Use Side Effectiveness",
        "    ,                        !- Source Side Inlet Node Name",
        "    ,                        !- Source Side Outlet Node Name",
        "    1,                       !- Source Side Effectiveness",
        "    Autosize,                !- Use Side Design Flow Rate {m3/s}",
        "    Autosize,                !- Source Side Design Flow Rate {m3/s}",
        "    1.5;                     !- Indirect Water Heating Recovery Time {hr}",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound = false;
    HeatBalanceManager::GetZoneData(ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);

    InternalHeatGains::GetInternalHeatGainsInput();
    ErrorsFound = false;
    EXPECT_FALSE(WaterThermalTanks::GetWaterThermalTankInputData(ErrorsFound));

    int TankNum(1);
    WaterThermalTanks::WaterThermalTankData &Tank = WaterThermalTank(TankNum);

    HourOfDay = 0;
    TimeStep = 1;
    TimeStepZone = 1.0 / 60.0; // one-minute system time step
    TimeStepSys = TimeStepZone;
    Tank.TankTemp = 60.0;
    Tank.AmbientTempZone = 20.0;
    Tank.AmbientTemp = 20.0;
    Tank.UseInletTemp = 20.0;
    Tank.SetPointTemp = 15.0;
    Tank.SetPointTemp2 = Tank.SetPointTemp;
    Tank.TimeElapsed = 0.0;

    // very low use mass flow rate
    Tank.UseMassFlowRate = 0.00005;
    // zero source mass flow rate
    Tank.SourceMassFlowRate = 0.0;

    WaterThermalTanks::CalcWaterThermalTankMixed(TankNum);

    // steady state estimated tank skin heat loss rate (1 minute time step)
    Real64 TankSkinHeatLossRate = -Tank.OffCycLossFracToZone * Tank.OffCycLossCoeff * (Tank.AmbientTempZone - Tank.TankTempAvg);
    // expected tank avg temp less than starting value of 60 C
    EXPECT_LT(Tank.TankTempAvg, 60.0);
    // check tank skin heat loss rate to the zone
    EXPECT_NEAR(Tank.AmbientZoneGain, TankSkinHeatLossRate, 0.000001);
}

TEST_F(EnergyPlusFixture, StratifiedTankCalc)
{
    using DataGlobals::HourOfDay;
    using DataGlobals::TimeStep;
    using DataGlobals::TimeStepZone;
    using DataHVACGlobals::SysTimeElapsed;
    using DataHVACGlobals::TimeStepSys;
    using WaterThermalTanks::WaterThermalTank;

    std::string const idf_objects = delimited_string({
        "Schedule:Constant, Hot Water Setpoint Temp Schedule, , 60.0;",
        "Schedule:Constant, Ambient Temp Schedule, , 22.0;",
        "Schedule:Constant, Hot Water Demand Schedule, , 0.0;",
        "WaterHeater:Stratified,",
        "  Stratified Tank,         !- Name",
        "  ,                        !- End-Use Subcategory",
        "  0.17,                    !- Tank Volume {m3}",
        "  1.4,                     !- Tank Height {m}",
        "  VerticalCylinder,        !- Tank Shape",
        "  ,                        !- Tank Perimeter {m}",
        "  100.0,                   !- Maximum Temperature Limit {C}",
        "  MasterSlave,             !- Heater Priority Control",
        "  Hot Water Setpoint Temp Schedule,  !- Heater 1 Setpoint Temperature Schedule Name",
        "  2.0,                     !- Heater 1 Deadband Temperature Difference {deltaC}",
        "  4500,                    !- Heater 1 Capacity {W}",
        "  1.0,                     !- Heater 1 Height {m}",
        "  Hot Water Setpoint Temp Schedule,  !- Heater 2 Setpoint Temperature Schedule Name",
        "  5.0,                     !- Heater 2 Deadband Temperature Difference {deltaC}",
        "  4500,                    !- Heater 2 Capacity {W}",
        "  0.0,                     !- Heater 2 Height {m}",
        "  ELECTRICITY,             !- Heater Fuel Type",
        "  0.98,                    !- Heater Thermal Efficiency",
        "  ,                        !- Off Cycle Parasitic Fuel Consumption Rate {W}",
        "  ELECTRICITY,             !- Off Cycle Parasitic Fuel Type",
        "  ,                        !- Off Cycle Parasitic Heat Fraction to Tank",
        "  ,                        !- Off Cycle Parasitic Height {m}",
        "  ,                        !- On Cycle Parasitic Fuel Consumption Rate {W}",
        "  ELECTRICITY,             !- On Cycle Parasitic Fuel Type",
        "  ,                        !- On Cycle Parasitic Heat Fraction to Tank",
        "  ,                        !- On Cycle Parasitic Height {m}",
        "  SCHEDULE,                !- Ambient Temperature Indicator",
        "  Ambient Temp Schedule,   !- Ambient Temperature Schedule Name",
        "  ,                        !- Ambient Temperature Zone Name",
        "  ,                        !- Ambient Temperature Outdoor Air Node Name",
        "  0.846,                   !- Uniform Skin Loss Coefficient per Unit Area to Ambient Temperature {W/m2-K}",
        "  ,                        !- Skin Loss Fraction to Zone",
        "  ,                        !- Off Cycle Flue Loss Coefficient to Ambient Temperature {W/K}",
        "  ,                        !- Off Cycle Flue Loss Fraction to Zone",
        "  0.000189,                !- Peak Use Flow Rate {m3/s}",
        "  Hot Water Demand Schedule,  !- Use Flow Rate Fraction Schedule Name",
        "  ,                        !- Cold Water Supply Temperature Schedule Name",
        "  ,                        !- Use Side Inlet Node Name",
        "  ,                        !- Use Side Outlet Node Name",
        "  ,                        !- Use Side Effectiveness",
        "  ,                        !- Use Side Inlet Height {m}",
        "  ,                        !- Use Side Outlet Height {m}",
        "  ,                        !- Source Side Inlet Node Name",
        "  ,                        !- Source Side Outlet Node Name",
        "  ,                        !- Source Side Effectiveness",
        "  ,                        !- Source Side Inlet Height {m}",
        "  ,                        !- Source Side Outlet Height {m}",
        "  FIXED,                   !- Inlet Mode",
        "  ,                        !- Use Side Design Flow Rate {m3/s}",
        "  ,                        !- Source Side Design Flow Rate {m3/s}",
        "  ,                        !- Indirect Water Heating Recovery Time {hr}",
        "  12,                      !- Number of Nodes",
        "  ,                        !- Additional Destratification Conductivity {W/m-K}",
        "  ,                        !- Node 1 Additional Loss Coefficient {W/K}",
        "  ,                        !- Node 2 Additional Loss Coefficient {W/K}",
        "  ,                        !- Node 3 Additional Loss Coefficient {W/K}",
        "  ,                        !- Node 4 Additional Loss Coefficient {W/K}",
        "  ,                        !- Node 5 Additional Loss Coefficient {W/K}",
        "  ,                        !- Node 6 Additional Loss Coefficient {W/K}",
        "  ,                        !- Node 7 Additional Loss Coefficient {W/K}",
        "  ,                        !- Node 8 Additional Loss Coefficient {W/K}",
        "  ,                        !- Node 9 Additional Loss Coefficient {W/K}",
        "  ;                        !- Node 10 Additional Loss Coefficient {W/K}",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound = false;
    HeatBalanceManager::GetZoneData(ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);

    InternalHeatGains::GetInternalHeatGainsInput();
    ErrorsFound = false;
    EXPECT_FALSE(WaterThermalTanks::GetWaterThermalTankInputData(ErrorsFound));

    HourOfDay = 0;
    TimeStep = 1;
    TimeStepZone = 20.0 / 60.0;
    TimeStepSys = TimeStepZone;
    const int TankNum = 1;
    WaterThermalTanks::WaterThermalTankData &Tank = WaterThermalTank(TankNum);
    for (auto &node : Tank.Node) {
        node.Temp = 60.0;
        node.SavedTemp = 60.0;
    }
    Tank.AmbientTempZone = 22.0;
    Tank.AmbientTemp = 22.0;
    Tank.UseInletTemp = 20.0;
    Tank.SetPointTemp = 60.0;
    Tank.SetPointTemp2 = Tank.SetPointTemp;
    Tank.TimeElapsed = 0.0;
    Tank.UseMassFlowRate = 0.0;
    Tank.SourceMassFlowRate = 0.0;

    WaterThermalTanks::CalcWaterThermalTankStratified(TankNum);

    std::vector<Real64> NodeTemps;
    NodeTemps.resize(Tank.Nodes);
    for (int i = 0; i < Tank.Nodes; ++i) {
        NodeTemps[i] = Tank.Node[i].Temp;
    }

    // Verify there are no temperature inversions.
    for (int i = 0; i < Tank.Nodes - 1; ++i) {
        EXPECT_GE(NodeTemps[i], NodeTemps[i+1]);
    }

    EXPECT_LT(Tank.Node(Tank.HeaterNode1).Temp, Tank.SetPointTemp);
    EXPECT_LT(Tank.Node(Tank.HeaterNode2).Temp, Tank.SetPointTemp2);
    EXPECT_GT(Tank.Node(Tank.HeaterNode1).Temp, Tank.SetPointTemp - Tank.DeadBandDeltaTemp);
    EXPECT_GT(Tank.Node(Tank.HeaterNode2).Temp, Tank.SetPointTemp2 - Tank.DeadBandDeltaTemp2);


    // Run a test where the tank turns on a heater during the timestep.
    for (auto &node : Tank.Node) {
        node.Temp = 58.05;
        node.SavedTemp = 58.05;
    }

    WaterThermalTanks::CalcWaterThermalTankStratified(TankNum);

    for (int i = 0; i < Tank.Nodes; ++i) {
        NodeTemps[i] = Tank.Node[i].Temp;
    }
    
    // Verify there are no temperature inversions.
    for (int i = 0; i < Tank.Nodes - 1; ++i) {
        EXPECT_GE(NodeTemps[i], NodeTemps[i+1]);
    }

    // Run a test with a use flow rate
    std::vector<Real64> PrevNodeTemps;
    PrevNodeTemps.resize(Tank.Nodes);
    for (int i = 0; i < Tank.Nodes; ++i) {
        auto &node = Tank.Node[i];
        node.Temp = 60.0;
        node.SavedTemp = 60.0;
        PrevNodeTemps[i] = node.Temp;
    }

    Tank.UseMassFlowRate = 2 * 6.30901964e-5 * 997; // 2 gal/min

    WaterThermalTanks::CalcWaterThermalTankStratified(TankNum);

    for (int i = 0; i < Tank.Nodes; ++i) {
        NodeTemps[i] = Tank.Node[i].Temp;
    }

    // Verify there are no temperature inversions.
    for (int i = 0; i < Tank.Nodes - 1; ++i) {
        EXPECT_GE(NodeTemps[i], NodeTemps[i+1]);
    }
    const Real64 SecInTimeStep = TimeStepSys * DataGlobals::SecInHour;
    int DummyIndex = 1;
    Real64 TankNodeEnergy = 0;
    for (int i = 0; i < Tank.Nodes; ++i) {
        auto &node = Tank.Node[i];
        TankNodeEnergy += node.Mass * (NodeTemps[i] - PrevNodeTemps[i]);
    }
    Real64 Cp = FluidProperties::GetSpecificHeatGlycol("WATER", 60.0, DummyIndex, "StratifiedTankCalcNoDraw");
    TankNodeEnergy *= Cp;
    EXPECT_NEAR(Tank.NetHeatTransferRate * SecInTimeStep, TankNodeEnergy, fabs(TankNodeEnergy * 0.0001));

    EXPECT_TRUE(Tank.HeaterOn1);
    EXPECT_FALSE(Tank.HeaterOn2);

    Tank.TankTempLimit = 80.0;
    Tank.UseMassFlowRate = 0.0;

    for (int i = 0; i < Tank.Nodes; ++i) {
        auto &node = Tank.Node[i];
        if (i <= 4) {
            node.Temp = 81.0;
        } else {
            node.Temp = 60.0;
        }
        node.SavedTemp = node.Temp;
    }

    WaterThermalTanks::CalcWaterThermalTankStratified(TankNum);

    for (auto &node : Tank.Node) {
        EXPECT_LE(node.Temp, Tank.TankTempLimit);
    }
    EXPECT_LT(Tank.VentRate, 0.0);
    const Real64 ExpectedVentedEnergy = Tank.Node[0].Mass * Cp * 5.0 / SecInTimeStep;
    EXPECT_NEAR(ExpectedVentedEnergy, -Tank.VentRate, fabs(ExpectedVentedEnergy) * 0.05);

}

TEST_F(EnergyPlusFixture, StratifiedTankSourceFlowRateCalc) {
    using DataGlobals::HourOfDay;
    using DataGlobals::TimeStep;
    using DataGlobals::TimeStepZone;
    using DataGlobals::SecInHour;
    using DataHVACGlobals::SysTimeElapsed;
    using DataHVACGlobals::TimeStepSys;
    using WaterThermalTanks::WaterThermalTank;

    std::string const idf_objects = delimited_string({
        "Schedule:Constant, Hot Water Setpoint Temp Schedule, , 60.0;",
        "Schedule:Constant, Ambient Temp Schedule, , 22.0;",
        "Schedule:Constant, Hot Water Demand Schedule, , 0.0;",
        "WaterHeater:Stratified,",
        "  Stratified Tank,         !- Name",
        "  ,                        !- End-Use Subcategory",
        "  0.17,                    !- Tank Volume {m3}",
        "  1.4,                     !- Tank Height {m}",
        "  VerticalCylinder,        !- Tank Shape",
        "  ,                        !- Tank Perimeter {m}",
        "  100.0,                   !- Maximum Temperature Limit {C}",
        "  MasterSlave,             !- Heater Priority Control",
        "  Hot Water Setpoint Temp Schedule,  !- Heater 1 Setpoint Temperature Schedule Name",
        "  2.0,                     !- Heater 1 Deadband Temperature Difference {deltaC}",
        "  4500,                    !- Heater 1 Capacity {W}",
        "  1.0,                     !- Heater 1 Height {m}",
        "  Hot Water Setpoint Temp Schedule,  !- Heater 2 Setpoint Temperature Schedule Name",
        "  5.0,                     !- Heater 2 Deadband Temperature Difference {deltaC}",
        "  4500,                    !- Heater 2 Capacity {W}",
        "  0.0,                     !- Heater 2 Height {m}",
        "  ELECTRICITY,             !- Heater Fuel Type",
        "  0.98,                    !- Heater Thermal Efficiency",
        "  ,                        !- Off Cycle Parasitic Fuel Consumption Rate {W}",
        "  ELECTRICITY,             !- Off Cycle Parasitic Fuel Type",
        "  ,                        !- Off Cycle Parasitic Heat Fraction to Tank",
        "  ,                        !- Off Cycle Parasitic Height {m}",
        "  ,                        !- On Cycle Parasitic Fuel Consumption Rate {W}",
        "  ELECTRICITY,             !- On Cycle Parasitic Fuel Type",
        "  ,                        !- On Cycle Parasitic Heat Fraction to Tank",
        "  ,                        !- On Cycle Parasitic Height {m}",
        "  SCHEDULE,                !- Ambient Temperature Indicator",
        "  Ambient Temp Schedule,   !- Ambient Temperature Schedule Name",
        "  ,                        !- Ambient Temperature Zone Name",
        "  ,                        !- Ambient Temperature Outdoor Air Node Name",
        "  0.0,                   !- Uniform Skin Loss Coefficient per Unit Area to Ambient Temperature {W/m2-K}",
        "  ,                        !- Skin Loss Fraction to Zone",
        "  ,                        !- Off Cycle Flue Loss Coefficient to Ambient Temperature {W/K}",
        "  ,                        !- Off Cycle Flue Loss Fraction to Zone",
        "  0.000189,                !- Peak Use Flow Rate {m3/s}",
        "  Hot Water Demand Schedule,  !- Use Flow Rate Fraction Schedule Name",
        "  ,                        !- Cold Water Supply Temperature Schedule Name",
        "  ,                        !- Use Side Inlet Node Name",
        "  ,                        !- Use Side Outlet Node Name",
        "  ,                        !- Use Side Effectiveness",
        "  ,                        !- Use Side Inlet Height {m}",
        "  ,                        !- Use Side Outlet Height {m}",
        "  ,                        !- Source Side Inlet Node Name",
        "  ,                        !- Source Side Outlet Node Name",
        "  ,                        !- Source Side Effectiveness",
        "  0.5,                        !- Source Side Inlet Height {m}",
        "  0.01,                        !- Source Side Outlet Height {m}",
        "  FIXED,                   !- Inlet Mode",
        "  ,                        !- Use Side Design Flow Rate {m3/s}",
        "  ,                        !- Source Side Design Flow Rate {m3/s}",
        "  ,                        !- Indirect Water Heating Recovery Time {hr}",
        "  12,                      !- Number of Nodes",
        "  ,                        !- Additional Destratification Conductivity {W/m-K}",
        "  ,                        !- Node 1 Additional Loss Coefficient {W/K}",
        "  ,                        !- Node 2 Additional Loss Coefficient {W/K}",
        "  ,                        !- Node 3 Additional Loss Coefficient {W/K}",
        "  ,                        !- Node 4 Additional Loss Coefficient {W/K}",
        "  ,                        !- Node 5 Additional Loss Coefficient {W/K}",
        "  ,                        !- Node 6 Additional Loss Coefficient {W/K}",
        "  ,                        !- Node 7 Additional Loss Coefficient {W/K}",
        "  ,                        !- Node 8 Additional Loss Coefficient {W/K}",
        "  ,                        !- Node 9 Additional Loss Coefficient {W/K}",
        "  ;                        !- Node 10 Additional Loss Coefficient {W/K}",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound = false;
    HeatBalanceManager::GetZoneData(ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);

    InternalHeatGains::GetInternalHeatGainsInput();
    ErrorsFound = false;
    EXPECT_FALSE(WaterThermalTanks::GetWaterThermalTankInputData(ErrorsFound));
    const int TankNum = 1;
    WaterThermalTanks::WaterThermalTankData &Tank = WaterThermalTank(TankNum);
    Tank.SourceInletNode = 1;
    Tank.SourceOutletNode = 2;
    WaterThermalTanks::SetupStratifiedNodes(TankNum);

    HourOfDay = 0;
    TimeStep = 1;
    TimeStepZone = 20.0 / 60.0;
    TimeStepSys = TimeStepZone;

    // Test a constant temperature source flow rate
    for (auto &node : Tank.Node) {
        node.Temp = 60.0;
        node.SavedTemp = 60.0;
    }
    Tank.AmbientTempZone = 22.0;
    Tank.AmbientTemp = 22.0;
    Tank.UseInletTemp = 20.0;
    Tank.SetPointTemp = 60.0;
    Tank.SetPointTemp2 = Tank.SetPointTemp;
    Tank.TimeElapsed = 0.0;
    Tank.UseMassFlowRate = 0.0;
    Tank.SourceInletTemp = 70.0;
    Tank.SourceMassFlowRate = 6.30901964e-5 * 997; // 1 gal/min

    int DummyIndex = 1;
    Real64 Cp = FluidProperties::GetSpecificHeatGlycol("WATER", 60.0, DummyIndex, "StratifiedTankCalcNoDraw");

    WaterThermalTanks::CalcWaterThermalTankStratified(TankNum);

    Real64 EnergySum = 0.0;
    for (int i = 0; i < Tank.Nodes; ++i) {
        auto &node = Tank.Node[i];
        EnergySum += node.Mass * Cp * (node.Temp - 60.0);
    }
    Real64 Esource = Tank.SourceEffectiveness * Tank.SourceMassFlowRate * Cp * (Tank.SourceInletTemp - Tank.Node(Tank.SourceOutletStratNode).TempAvg) * TimeStepSys * SecInHour;
    EXPECT_NEAR(Esource, EnergySum, EnergySum * 0.001);


}

TEST_F(EnergyPlusFixture, DesuperheaterTimeAdvanceCheck){
    using DataLoopNode::Node;
    using DataGlobals::HourOfDay;
    using DataGlobals::TimeStep;
    using DataGlobals::TimeStepZone;
    using DataHVACGlobals::SysTimeElapsed;
    using DataHVACGlobals::TimeStepSys;
    using WaterThermalTanks::WaterThermalTank;
    using WaterThermalTanks::WaterHeaterDesuperheater;
    using DXCoils::DXCoil;
    using DataHeatBalance::HeatReclaimDXCoil;

    std::string const idf_objects = delimited_string({
        "Schedule:Constant, Hot Water Demand Schedule, , 1.0;",
        "Schedule:Constant, Ambient Temp Schedule, , 20.0;",
        "Schedule:Constant, Inlet Water Temperature, , 10.0;",
        "Schedule:Constant, Desuperheater-Schedule, , 55.0;",
        "Schedule:Constant, WH Setpoint Temp, , 50.0;",        

        "  Zone,",
        "    Zone_TES,                !- Name",
        "    0.0000,                  !- Direction of Relative North {deg}",
        "    10.0,                    !- X Origin {m}",
        "    10.0,                    !- Y Origin {m}",
        "    0.0,                     !- Z Origin {m}",
        "    1,                       !- Type",
        "    1.00,                    !- Multiplier",
        "    3.00,                    !- Ceiling Height {m}",
        "    300.0,                   !- Volume {m3}",
        "    100.0;                   !- Floor Area {m2}",

        "  Schedule:Compact,",
        "    ALWAYS_ON,               !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,1.0;        !- Field 3",

        "WaterHeater:Mixed,",
        "  Mixed tank with desuperheater,          !- Name",
        "  0.136274824222915,                      !- Tank Volume {m3}",
        "  WH Setpoint Temp,                       !- Setpoint Temperature Schedule Name",
        "  2,                                      !- Deadband Temperature Difference {deltaC}",
        "  99,                                     !- Maximum Temperature Limit {C}",
        "  Cycle,                                  !- Heater Control Type",
        "  5500.06477392209,                       !- Heater Maximum Capacity {W}",
        "  0,                                      !- Heater Minimum Capacity {W}",
        "  0,                                      !- Heater Ignition Minimum Flow Rate {m3/s}",
        "  0,                                      !- Heater Ignition Delay {s}",
        "  Electricity,                            !- Heater Fuel Type",
        "  0.8,                                    !- Heater Thermal Efficiency",
        "  ,                                       !- Part Load Factor Curve Name",
        "  0,                                      !- Off Cycle Parasitic Fuel Consumption Rate {W}",
        "  Electricity,                            !- Off Cycle Parasitic Fuel Type",
        "  0,                                      !- Off Cycle Parasitic Heat Fraction to Tank",
        "  0,                                      !- On Cycle Parasitic Fuel Consumption Rate {W}",
        "  Electricity,                            !- On Cycle Parasitic Fuel Type",
        "  0,                                      !- On Cycle Parasitic Heat Fraction to Tank",
        "  Schedule,                               !- Ambient Temperature Indicator",
        "  Ambient Temp Schedule,                  !- Ambient Temperature Schedule Name",
        "  ,                                 	   !- Ambient Temperature Zone Name",
        "  ,                                       !- Ambient Temperature Outdoor Air Node Name",
        "  0.704227539803499,                      !- Off Cycle Loss Coefficient to Ambient Temperature {W/K}",
        "  1,                                      !- Off Cycle Loss Fraction to Zone",
        "  0.704227539803499,                      !- On Cycle Loss Coefficient to Ambient Temperature {W/K}",
        "  1,                                      !- On Cycle Loss Fraction to Zone",
        "  ,                                       !- Peak Use Flow Rate {m3/s}",
        "  ALWAYS_ON,                              !- Use Flow Rate Fraction Schedule Name",
        "  ,                                       !- Cold Water Supply Temperature Schedule Name",
        "  ,                                       !- Use Side Inlet Node Name",
        "  ,                                       !- Use Side Outlet Node Name",
        "  1,                                      !- Use Side Effectiveness",
        "  DesuperheaterOut,			           !- Source Side Inlet Node Name",
        "  DesuperheaterIn,         	           !- Source Side Outlet Node Name",
        "  1,                                      !- Source Side Effectiveness",
        "  0.00283433494640006,                    !- Use Side Design Flow Rate {m3/s}",
        "  ,                                       !- Source Side Design Flow Rate {m3/s}",
        "  1.5,                                    !- Indirect Water Heating Recovery Time {hr}",
        "  IndirectHeatPrimarySetpoint,            !- Source Side Flow Control Mode",
        "  ,                                       !- Indirect Alternate Setpoint Temperature Schedule Name",
        "  General;                                !- End-Use Subcategory",

        "Coil:WaterHeating:Desuperheater,",
        "    Desuperheater,           !- Name",
        "    ALWAYS_ON,               !- Availability Schedule Name",
        "    Desuperheater-Schedule,  !- Setpoint Temperature Schedule Name",
        "    2,                       !- Dead Band Temperature Difference {deltaC}",
        "    0.25,                    !- Rated Heat Reclaim Recovery Efficiency",
        "    50,                      !- Rated Inlet Water Temperature {C}",
        "    35,                      !- Rated Outdoor Air Temperature {C}",
        "    60,                      !- Maximum Inlet Water Temperature for Heat Reclaim {C}",
        "    ,                        !- Heat Reclaim Efficiency Function of Temperature Curve Name",
        "    DesuperheaterIn,         !- Water Inlet Node Name",
        "    DesuperheaterOut,        !- Water Outlet Node Name",
        "    WaterHeater:Mixed,       !- Tank Object Type",
        "    Mixed tank with desuperheater,  !- Tank Name",
        "    Coil:Cooling:DX:SingleSpeed,  !- Heating Source Object Type",
        "    SingleSpeed_COIL,              !- Heating Source Name",
        "    0.0001,                  !- Water Flow Rate {m3/s}",
        "    ,                        !- Water Pump Power {W}",
        "    0.2;                     !- Fraction of Pump Heat to Water",


        "Coil:Cooling:DX:SingleSpeed,",
        "  SingleSpeed_COIL,                        !- Name",
        "  ALWAYS_ON,                     !- Availability Schedule Name",
        "  14067.4113682534,        !- Gross Rated Total Cooling Capacity {W}",
        "  0.740402528813699,       !- Gross Rated Sensible Heat Ratio",
        "  3.99990781858502,        !- Gross Rated Cooling COP {W/W}",
        "  0.728875631277391,       !- Rated Air Flow Rate {m3/s}",
        "  773.3912012006,          !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "  central ac unitary system Fan - Cooling Coil Node,  !- Air Inlet Node Name",
        "  Node 5,                  !- Air Outlet Node Name",
        "  Cool-Cap-fT1,            !- Total Cooling Capacity Function of Temperature Curve Name",
        "  Cool-Cap-fFF1,           !- Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  Cool-EIR-fT1,            !- Energy Input Ratio Function of Temperature Curve Name",
        "  Cool-EIR-fFF1,           !- Energy Input Ratio Function of Flow Fraction Curve Name",
        "  Cool-PLF-fPLR1,          !- Part Load Fraction Correlation Curve Name",
        "  ,                        !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "  1000,                    !- Nominal Time for Condensate Removal to Begin {s}",
        "  1.5,                     !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity {dimensionless}",
        "  3,                       !- Maximum Cycling Rate {cycles/hr}",
        "  45,                      !- Latent Capacity Time Constant {s}",
        "  ,                        !- Condenser Air Inlet Node Name",
        "  AirCooled,               !- Condenser Type",
        "  0,                       !- Evaporative Condenser Effectiveness {dimensionless}",
        "  Autosize,                !- Evaporative Condenser Air Flow Rate {m3/s}",
        "  Autosize,                !- Evaporative Condenser Pump Rated Power Consumption {W}"
        "  50,                      !- Crankcase Heater Capacity {W}",
        "  10,                      !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
        ",",
        "  ,                        !- Supply Water Storage Tank Name",
        "  ,                        !- Condensate Collection Water Storage Tank Name",
        "  0,                       !- Basin Heater Capacity {W/K}",
        "  10;                      !- Basin Heater Setpoint Temperature {C}",

        "Curve:Biquadratic,",
        "  Cool-Cap-fT1,                           !- Name",
        "  1.550902001,                             !- Coefficient1 Constant",
        "  -0.0750500892,                          !- Coefficient2 x",
        "  0.00309713544,                          !- Coefficient3 x**2",
        "  0.00240111,                            !- Coefficient4 y",
        "  -5.0544e-005,                          !- Coefficient5 y**2",
        "  -0.00042728148,                         !- Coefficient6 x*y",
        "  13.88,                                  !- Minimum Value of x {BasedOnField A2}",
        "  23.88,                                  !- Maximum Value of x {BasedOnField A2}",
        "  18.33,                                  !- Minimum Value of y {BasedOnField A3}",
        "  51.66;                                  !- Maximum Value of y {BasedOnField A3}",

        "Curve:Quadratic,",
        "  Cool-Cap-fFF1,                          !- Name",
        "  0.718605468,                             !- Coefficient1 Constant",
        "  0.410099989,                            !- Coefficient2 x",
        "  -0.128705457,                           !- Coefficient3 x**2",
        "  0,                                      !- Minimum Value of x {BasedOnField A2}",
        "  2,                                      !- Maximum Value of x {BasedOnField A2}",
        "  0,                                      !- Minimum Curve Output {BasedOnField A3}",
        "  2;                                      !- Maximum Curve Output {BasedOnField A3}",

        "Curve:Biquadratic,",
        "  Cool-EIR-fT1,                           !- Name",
        "  -0.304282997000001,                             !- Coefficient1 Constant",
        "  0.1180477062,                           !- Coefficient2 x",
        "  -0.00342466704,                          !- Coefficient3 x**2",
        "  -0.0062619138,                          !- Coefficient4 y",
        "  0.00069542712,                          !- Coefficient5 y**2",
        "  -0.00046997496,                         !- Coefficient6 x*y",
        "  13.88,                                  !- Minimum Value of x {BasedOnField A2}",
        "  23.88,                                  !- Maximum Value of x {BasedOnField A2}",
        "  18.33,                                  !- Minimum Value of y {BasedOnField A3}",
        "  51.66;                                  !- Maximum Value of y {BasedOnField A3}",

        "Curve:Quadratic,",
        "  Cool-EIR-fFF1,                          !- Name",
        "  1.32299905,                            !- Coefficient1 Constant",
        "  -0.477711207,                           !- Coefficient2 x",
        "  0.154712157,                            !- Coefficient3 x**2",
        "  0,                                      !- Minimum Value of x {BasedOnField A2}",
        "  2,                                      !- Maximum Value of x {BasedOnField A2}",
        "  0,                                      !- Minimum Curve Output {BasedOnField A3}",
        "  2;                                      !- Maximum Curve Output {BasedOnField A3}",

        "Curve:Quadratic,",
        "  Cool-PLF-fPLR1,                         !- Name",
        "  0.93,                                   !- Coefficient1 Constant",
        "  0.07,                                   !- Coefficient2 x",
        "  0,                                      !- Coefficient3 x**2",
        "  0,                                      !- Minimum Value of x {BasedOnField A2}",
        "  1,                                      !- Maximum Value of x {BasedOnField A2}",
        "  0.7,                                    !- Minimum Curve Output {BasedOnField A3}",
        "  1;                                      !- Maximum Curve Output {BasedOnField A3}",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound = false;
    HeatBalanceManager::GetZoneData(ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);

    DataGlobals::NumOfTimeStepInHour = 1; // must initialize this to get schedules initialized
    DataGlobals::MinutesPerTimeStep = 60; // must initialize this to get schedules initialized
    ScheduleManager::ProcessScheduleInput();
    ScheduleManager::ScheduleInputProcessed = true;

    DataGlobals::TimeStep = 1;
    DataGlobals::HourOfDay = 1;
    DataEnvironment::Month = 7;
    DataEnvironment::DayOfMonth = 21;
    DataGlobals::HourOfDay = 1;
    DataEnvironment::DSTIndicator = 0;
    DataEnvironment::DayOfWeek = 2;
    DataEnvironment::HolidayIndex = 0;
    DataEnvironment::DayOfYear_Schedule = General::OrdinalDay(DataEnvironment::Month, DataEnvironment::DayOfMonth, 1);
    ScheduleManager::UpdateScheduleValues();

    int TankNum = 1;
    int DXNum = 1;
    bool FirstHVAC = true;

    ErrorsFound = false;
    EXPECT_FALSE(WaterThermalTanks::GetWaterThermalTankInputData(ErrorsFound));

    WaterThermalTanks::WaterThermalTankData &Tank = WaterThermalTank(TankNum);
    WaterThermalTanks::WaterHeaterDesuperheaterData &Desuperheater = WaterHeaterDesuperheater(Tank.DesuperheaterNum);

    //Inititate tank conditions
    HourOfDay = 0;
    TimeStep = 1;
    TimeStepZone = 1;
    TimeStepSys = TimeStepZone;
    SysTimeElapsed = 0.0;

    // First iteration condition set (extreme)
    Tank.TankTemp = 50;
    Tank.SavedTankTemp = 52; // previous time step temperature
    Tank.AmbientTemp = 50.0; // Assume no loss
    Tank.UseInletTemp = 10;
    Tank.UseMassFlowRate = 0.0; 
    Tank.SourceOutletTemp = 50;
    Tank.SavedSourceOutletTemp = 52;
    Tank.TimeElapsed = 0.0;
    Desuperheater.SetPointTemp = 55;
    Desuperheater.Mode = 1;
    DataHeatBalance::HeatReclaimDXCoil(DXNum).AvailCapacity = 0;
    DXCoil(DXNum).PartLoadRatio = 1.0;

    Tank.Mode = 0;
    Tank.SetPointTemp = 50;
    EXPECT_TRUE(Desuperheater.FirstTimeThroughFlag);
    WaterThermalTanks::CalcDesuperheaterWaterHeater(TankNum,FirstHVAC);
    // FirsttimeThroughFlag attribute supposed to set as false after first run
    EXPECT_FALSE(Desuperheater.FirstTimeThroughFlag);
    // Advanced to next time step (0 => 1)
    EXPECT_EQ(Node(Desuperheater.WaterInletNode).Temp, 50);
    EXPECT_EQ(Tank.SavedTankTemp, 50);
    // No loss no source, Tank temperature supposed to be 50
    EXPECT_EQ(Tank.TankTemp,50);

    // Assumed next iteration with FirstHVAC condition not changed
    DataHeatBalance::HeatReclaimDXCoil(DXNum).AvailCapacity = 500;
    Tank.TankTemp = 20.0; // Assumed Tank temperature from previous iteration
    EXPECT_FALSE(Desuperheater.FirstTimeThroughFlag);
    WaterThermalTanks::CalcDesuperheaterWaterHeater(TankNum,FirstHVAC);
    EXPECT_FALSE(Desuperheater.FirstTimeThroughFlag);
    EXPECT_EQ(Node(Desuperheater.WaterInletNode).Temp, 50);
    // Saved temperature not supposed to change
    EXPECT_EQ(Tank.SavedTankTemp,50);
    // The source side inlet temperature calculated based on saved temperatures
    // Tank temperature should be calculated based on saved temperatures
    EXPECT_GT(Tank.SourceInletTemp, 50);
    EXPECT_GT(Tank.TankTemp, 50);

    // Assumed next iteration with FirstHVAC condition not changed
    FirstHVAC = false;
    Tank.TankTemp = 20.0;
    DataHeatBalance::HeatReclaimDXCoil(DXNum).AvailCapacity = 500;
    WaterThermalTanks::CalcDesuperheaterWaterHeater(TankNum,FirstHVAC);
    // Flag changed from false to true
    EXPECT_TRUE(Desuperheater.FirstTimeThroughFlag);
    // Saved temperature not supposed to change
    EXPECT_EQ(Tank.SavedTankTemp,50);
    EXPECT_EQ(Node(Desuperheater.WaterInletNode).Temp, 50);
    EXPECT_GT(Tank.SourceInletTemp, 50);
    EXPECT_GT(Tank.TankTemp, 50);

}

TEST_F(EnergyPlusFixture, StratifiedTank_GSHP_DesuperheaterSourceHeat)
{
    using DataLoopNode::Node;
    using DataGlobals::HourOfDay;
    using DataGlobals::TimeStep;
    using DataGlobals::TimeStepZone;
    using DataHVACGlobals::SysTimeElapsed;
    using DataHVACGlobals::TimeStepSys;
    using WaterThermalTanks::WaterThermalTank;
    using WaterThermalTanks::WaterHeaterDesuperheater;
    using WaterToAirHeatPumpSimple::SimpleWatertoAirHP;
    using DataPlant::PlantLoop;

    std::string const idf_objects = delimited_string({
        "Schedule:Constant, Hot Water Demand Schedule, , 1.0;",
        "Schedule:Constant, Ambient Temp Schedule, , 20.0;",
        "Schedule:Constant, Inlet Water Temperature, , 10.0;",
        "Schedule:Constant, Desuperheater-Schedule, , 60.0;",
        "Schedule:Constant, WH Setpoint Temp, , 45.0;",        

        "  Zone,",
        "    Zone_TES,                !- Name",
        "    0.0000,                  !- Direction of Relative North {deg}",
        "    10.0,                    !- X Origin {m}",
        "    10.0,                    !- Y Origin {m}",
        "    0.0,                     !- Z Origin {m}",
        "    1,                       !- Type",
        "    1.00,                    !- Multiplier",
        "    3.00,                    !- Ceiling Height {m}",
        "    300.0,                   !- Volume {m3}",
        "    100.0;                   !- Floor Area {m2}",

        "  Schedule:Compact,",
        "    ALWAYS_ON,               !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,1.0;        !- Field 3",

        "  WaterHeater:Stratified,",
        "    Stratified tank with desuperheater,   !- Name",
        "    Domestic Hot Water,      !- End-Use Subcategory",
        "    0.170343530278643,       !- Tank Volume {m3}",
        "    1.2192,                  !- Tank Height {m}",
        "    VerticalCylinder,        !- Tank Shape",
        "    ,                        !- Tank Perimeter {m}",
        "    90,                      !- Maximum Temperature Limit {C}",
        "    MasterSlave,             !- Heater Priority Control",
        "    WH Setpoint Temp,        !- Heater 1 Setpoint Temperature Schedule Name",
        "    2,                       !- Heater 1 Deadband Temperature Difference {deltaC}",
        "    4498.64092714361,        !- Heater 1 Capacity {W}",
        "    0.89408,                 !- Heater 1 Height {m}",
        "    WH Setpoint Temp,        !- Heater 2 Setpoint Temperature Schedule Name",
        "    2,                       !- Heater 2 Deadband Temperature Difference {deltaC}",
        "    4498.64092714361,        !- Heater 2 Capacity {W}",
        "    0.16256,                 !- Heater 2 Height {m}",
        "    Electricity,             !- Heater Fuel Type",
        "    1,                       !- Heater Thermal Efficiency",
        "    0,                       !- Off Cycle Parasitic Fuel Consumption Rate {W}",
        "    Electricity,             !- Off Cycle Parasitic Fuel Type",
        "    0,                       !- Off Cycle Parasitic Heat Fraction to Tank",
        "    0,                       !- Off Cycle Parasitic Height {m}",
        "    0,                       !- On Cycle Parasitic Fuel Consumption Rate {W}",
        "    Electricity,             !- On Cycle Parasitic Fuel Type",
        "    0,                       !- On Cycle Parasitic Heat Fraction to Tank",
        "    0,                       !- On Cycle Parasitic Height {m}",
        "    Schedule,                        !- Ambient Temperature Indicator",
        "    Ambient Temp Schedule,   !- Ambient Temperature Schedule Name",
        "    ,                        !- Ambient Temperature Zone Name",
        "    ,                        !- Ambient Temperature Outdoor Air Node Name",
        "    0.614007341138612,       !- Uniform Skin Loss Coefficient per Unit Area to Ambient Temperature {W/m2-K}",
        "    1,                       !- Skin Loss Fraction to Zone",
        "    0,                       !- Off Cycle Flue Loss Coefficient to Ambient Temperature {W/K}",
        "    1,                       !- Off Cycle Flue Loss Fraction to Zone",
        "    ,                        !- Peak Use Flow Rate {m3/s}",
        "    ALWAYS_ON,               !- Use Flow Rate Fraction Schedule Name",
        "    ,                        !- Cold Water Supply Temperature Schedule Name",
        "    ,                        !- Use Side Inlet Node Name",
        "    ,                        !- Use Side Outlet Node Name",
        "    ,                        !- Use Side Effectiveness",
        "    ,                        !- Use Side Inlet Height {m}",
        "    ,                        !- Use Side Outlet Height {m}",
        "    DesuperheaterOut,        !- Source Side Inlet Node Name",
        "    DesuperheaterIn,         !- Source Side Outlet Node Name",
        "    1,                       !- Source Side Effectiveness",
        "    ,                       !- Source Side Inlet Height {m}",
        "    ,                       !- Source Side Outlet Height {m}",
        "    Fixed,                   !- Inlet Mode",
        "    0.00283433494640006,     !- Use Side Design Flow Rate {m3/s}",
        "    0,                       !- Source Side Design Flow Rate {m3/s}",
        "    1.5,                     !- Indirect Water Heating Recovery Time {hr}",
        "    12,                      !- Number of Nodes",
        ";",

        "Coil:WaterHeating:Desuperheater,",
        "    Desuperheater,           !- Name",
        "    ALWAYS_ON,               !- Availability Schedule Name",
        "    Desuperheater-Schedule,  !- Setpoint Temperature Schedule Name",
        "    5,                       !- Dead Band Temperature Difference {deltaC}",
        "    0.25,                    !- Rated Heat Reclaim Recovery Efficiency",
        "    50,                      !- Rated Inlet Water Temperature {C}",
        "    35,                      !- Rated Outdoor Air Temperature {C}",
        "    60,                      !- Maximum Inlet Water Temperature for Heat Reclaim {C}",
        "    ,                        !- Heat Reclaim Efficiency Function of Temperature Curve Name",
        "    DesuperheaterIn,         !- Water Inlet Node Name",
        "    DesuperheaterOut,        !- Water Outlet Node Name",
        "    WaterHeater:Stratified,       !- Tank Object Type",
        "    Stratified tank with desuperheater,  !- Tank Name",
        "    Coil:Cooling:WaterToAirHeatPump:EquationFit,  !- Heating Source Object Type",
        "    GSHP_COIL1,              !- Heating Source Name",
        "    0.0001,                  !- Water Flow Rate {m3/s}",
        "    ,                        !- Water Pump Power {W}",
        "    0.2;                     !- Fraction of Pump Heat to Water",

            
        "Coil:Cooling:WaterToAirHeatPump:EquationFit,",
        "    GSHP_COIL1,       !- Name",
        "    Node 42,                 !- Water Inlet Node Name",
        "    Node 43,                 !- Water Outlet Node Name",
        "    res gshp clg unitary system Fan - Cooling Coil Node,  !- Air Inlet Node Name",
        "    Node 45,                 !- Air Outlet Node Name",
        "    0.951796450842996,       !- Rated Air Flow Rate {m3/s}",
        "    0.000567811767595478,    !- Rated Water Flow Rate {m3/s}",
        "    14067.4113682534,        !- Gross Rated Total Cooling Capacity {W}",
        "    10297.3451215615,        !- Gross Rated Sensible Cooling Capacity {W}",
        "    5.3555091458258,         !- Gross Rated Cooling COP",
        "    -3.9160645386,           !- Total Cooling Capacity Coefficient 1",
        "    7.042944024,             !- Total Cooling Capacity Coefficient 2",
        "    -2.270589372,            !- Total Cooling Capacity Coefficient 3",
        "    0,                       !- Total Cooling Capacity Coefficient 4",
        "    0,                       !- Total Cooling Capacity Coefficient 5",
        "    26.7839398084,           !- Sensible Cooling Capacity Coefficient 1",
        "    0,                       !- Sensible Cooling Capacity Coefficient 2",
        "    -23.832385974,           !- Sensible Cooling Capacity Coefficient 3",
        "    -1.115743914,            !- Sensible Cooling Capacity Coefficient 4",
        "    0,                       !- Sensible Cooling Capacity Coefficient 5",
        "    0,                       !- Sensible Cooling Capacity Coefficient 6",
        "    -6.2337364523,           !- Cooling Power Consumption Coefficient 1",
        "    1.610096238,             !- Cooling Power Consumption Coefficient 2",
        "    5.317076448,             !- Cooling Power Consumption Coefficient 3",
        "    0,                       !- Cooling Power Consumption Coefficient 4",
        "    0,                       !- Cooling Power Consumption Coefficient 5",
        "    1000,                    !- Nominal Time for Condensate Removal to Begin {s}",
        "    1.5;                     !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity {dimensionless}",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound = false;
    HeatBalanceManager::GetZoneData(ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);

    DataGlobals::NumOfTimeStepInHour = 1; // must initialize this to get schedules initialized
    DataGlobals::MinutesPerTimeStep = 60; // must initialize this to get schedules initialized
    ScheduleManager::ProcessScheduleInput();
    ScheduleManager::ScheduleInputProcessed = true;

    DataGlobals::TimeStep = 1;
    DataGlobals::HourOfDay = 1;
    DataEnvironment::Month = 7;
    DataEnvironment::DayOfMonth = 21;
    DataGlobals::HourOfDay = 1;
    DataEnvironment::DSTIndicator = 0;
    DataEnvironment::DayOfWeek = 2;
    DataEnvironment::HolidayIndex = 0;
    DataEnvironment::DayOfYear_Schedule = General::OrdinalDay(DataEnvironment::Month, DataEnvironment::DayOfMonth, 1);
    ScheduleManager::UpdateScheduleValues();
    DataPlant::TotNumLoops=1;
    int TankNum(1);
    int HPNum(1);
    int CyclingScheme(1);
    int LoopNum(1);
    int DemandSide(1);
    int SupplySide(2);
    int BranchNum(1);
    int CompNum(1);
    Real64 PLR(0.5);
    ErrorsFound = false;
    EXPECT_FALSE(WaterThermalTanks::GetWaterThermalTankInputData(ErrorsFound));
    //Test if the new data Structure for water to air heat pump coils successfully initialized
    EXPECT_EQ(DataHeatBalance::HeatReclaimSimple_WAHPCoil(1).Name, "GSHP_COIL1");
    EXPECT_EQ(DataHeatBalance::HeatReclaimSimple_WAHPCoil(1).SourceType, "Coil:Cooling:WaterToAirHeatPump:EquationFit");
    //Air node
    Node(5).MassFlowRate = 0.005;
    Node(5).Temp = 28.0;
    Node(5).HumRat = 0.2 ;
    //Water node
    Node(3).Temp = 15.0;
    Node(3).MassFlowRate = 0.05;
    //Plant loop must be initialized
    SimpleWatertoAirHP(HPNum).LoopNum = 1;
    PlantLoop.allocate(LoopNum);
    PlantLoop(SimpleWatertoAirHP(HPNum).LoopNum).FluidIndex = 1;
    PlantLoop(LoopNum).LoopSide.allocate(DemandSide);
    PlantLoop(LoopNum).LoopSide.allocate(SupplySide);
    auto &SupplySideloop(PlantLoop(LoopNum).LoopSide(SupplySide));
    SupplySideloop.TotalBranches = 1;
    SupplySideloop.Branch.allocate(BranchNum);
    auto &CoilBranch(SupplySideloop.Branch(BranchNum));
    CoilBranch.TotalComponents = 1;
    CoilBranch.Comp.allocate(CompNum);
    CoilBranch.Comp(CompNum).TypeOf_Num = 67;
    CoilBranch.Comp(CompNum).Name = "GSHP_COIL1";

    DataGlobals::BeginEnvrnFlag = true;
    WaterToAirHeatPumpSimple::InitSimpleWatertoAirHP(HPNum, 10.0, 1.0, 0.0, 10.0, 10.0, CyclingScheme, 1.0, 1);
    WaterToAirHeatPumpSimple::CalcHPCoolingSimple(HPNum, CyclingScheme, 1.0, 10.0, 10.0, 1, PLR, 1.0);
    //Coil source side heat successfully passed to HeatReclaimSimple_WAHPCoil(1).AvailCapacity
    EXPECT_EQ(DataHeatBalance::HeatReclaimSimple_WAHPCoil(1).AvailCapacity, SimpleWatertoAirHP(1).QSource);
    //Reclaimed heat successfully returned to reflect the plant impact
    DataHeatBalance::HeatReclaimSimple_WAHPCoil(1).WaterHeatingDesuperheaterReclaimedHeat(1) = 100.0;
    WaterToAirHeatPumpSimple::CalcHPCoolingSimple(HPNum, CyclingScheme, 1.0, 10.0, 10.0, 1, PLR, 1.0);
    EXPECT_EQ(SimpleWatertoAirHP(1).QSource, DataHeatBalance::HeatReclaimSimple_WAHPCoil(1).AvailCapacity - 100.0);

    WaterThermalTanks::WaterThermalTankData &Tank = WaterThermalTank(TankNum);
    WaterThermalTanks::WaterHeaterDesuperheaterData &Desuperheater = WaterHeaterDesuperheater(Tank.DesuperheaterNum);

    for (int i = 1; i <= Tank.Nodes; ++i) {
        auto &node = Tank.Node(i);
        node.SavedTemp = 39.0;
        node.Temp = 39.0;
    }

    Tank.TankTemp = 39.0;
    Tank.AmbientTemp = 20.0;
    Tank.UseInletTemp = 10.0;
    Tank.UseMassFlowRate = 0.003;
    Tank.SourceOutletTemp = 39.0;
    Tank.SourceMassFlowRate = 0.003;
    Tank.TimeElapsed = 0.0;
    Desuperheater.SetPointTemp = 60.0;
    Desuperheater.Mode = 1;
    Node(Desuperheater.WaterInletNode).Temp = Tank.SourceOutletTemp;
    
    HourOfDay = 0;
    TimeStep = 1;
    TimeStepZone = 1. / 60.;
    TimeStepSys = TimeStepZone;
    SysTimeElapsed = 0.0;
    DataHeatBalance::HeatReclaimSimple_WAHPCoil(1).AvailCapacity = 1000;
    WaterToAirHeatPumpSimple::SimpleWatertoAirHP(1).PartLoadRatio = 0.0;
    WaterThermalTanks::InitWaterThermalTank(TankNum, true);
    Tank.SetPointTemp = 45;
    Tank.SetPointTemp2 = 45;
    WaterThermalTanks::CalcDesuperheaterWaterHeater(TankNum, true);
    // If there's no demand in water thermal tank, no heat is reclaimed 
    EXPECT_EQ(Desuperheater.HeaterRate, 0);

    for (int i = 1; i <= Tank.Nodes; ++i) {
        auto &node = Tank.Node(i);
        node.SavedTemp = 39;
        node.Temp = 39;
    }

    Tank.SavedTankTemp = 39.0;
    Tank.SavedSourceOutletTemp = 39.0;
    Tank.SourceMassFlowRate = 0.003;
    Node(Desuperheater.WaterInletNode).Temp = Tank.SavedSourceOutletTemp;

    WaterToAirHeatPumpSimple::SimpleWatertoAirHP(1).PartLoadRatio = 0.8;
    WaterThermalTanks::InitWaterThermalTank(TankNum, false);
    Desuperheater.SaveMode = 1;
    WaterThermalTanks::CalcDesuperheaterWaterHeater(TankNum, false);
    //The HVAC part load ratio is successfully passed to waterthermaltank desuperheater data struct
    EXPECT_EQ(Desuperheater.DXSysPLR, 0.8);
    //The heater rate is correctly calculated
    EXPECT_EQ(Desuperheater.HeaterRate, 1000 * 0.25 / Desuperheater.DXSysPLR * Desuperheater.DesuperheaterPLR);
    //The source rate calculated in stratified tank calculation function is near the heater rate calcualted in desuperheater function
    EXPECT_NEAR(Tank.SourceRate, Desuperheater.HeaterRate, Tank.SourceRate * 0.05);

}

TEST_F(EnergyPlusFixture, Desuperheater_Multispeed_Coil_Test)
{
    using DataLoopNode::Node;
    using DataGlobals::HourOfDay;
    using DataGlobals::TimeStep;
    using DataGlobals::TimeStepZone;
    using DataHVACGlobals::SysTimeElapsed;
    using DataHVACGlobals::TimeStepSys;
    using WaterThermalTanks::WaterThermalTank;
    using WaterThermalTanks::WaterHeaterDesuperheater;
    using DXCoils::DXCoil;

    std::string const idf_objects = delimited_string({
        "Schedule:Constant, Hot Water Demand Schedule, , 1.0;",
        "Schedule:Constant, Ambient Temp Schedule, , 20.0;",
        "Schedule:Constant, Inlet Water Temperature, , 10.0;",
        "Schedule:Constant, Desuperheater-Schedule, , 60.0;",
        "Schedule:Constant, WH Setpoint Temp, , 45.0;",        

        "  Zone,",
        "    Zone_TES,                !- Name",
        "    0.0000,                  !- Direction of Relative North {deg}",
        "    10.0,                    !- X Origin {m}",
        "    10.0,                    !- Y Origin {m}",
        "    0.0,                     !- Z Origin {m}",
        "    1,                       !- Type",
        "    1.00,                    !- Multiplier",
        "    3.00,                    !- Ceiling Height {m}",
        "    300.0,                   !- Volume {m3}",
        "    100.0;                   !- Floor Area {m2}",

        "  Schedule:Compact,",
        "    ALWAYS_ON,               !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,1.0;        !- Field 3",

        "WaterHeater:Mixed,",
        "  Mixed tank with desuperheater,          !- Name",
        "  0.136274824222915,                      !- Tank Volume {m3}",
        "  WH Setpoint Temp,                       !- Setpoint Temperature Schedule Name",
        "  2,                                      !- Deadband Temperature Difference {deltaC}",
        "  99,                                     !- Maximum Temperature Limit {C}",
        "  Cycle,                                  !- Heater Control Type",
        "  5500.06477392209,                       !- Heater Maximum Capacity {W}",
        "  0,                                      !- Heater Minimum Capacity {W}",
        "  0,                                      !- Heater Ignition Minimum Flow Rate {m3/s}",
        "  0,                                      !- Heater Ignition Delay {s}",
        "  Electricity,                            !- Heater Fuel Type",
        "  0.8,                                    !- Heater Thermal Efficiency",
        "  ,                                       !- Part Load Factor Curve Name",
        "  0,                                      !- Off Cycle Parasitic Fuel Consumption Rate {W}",
        "  Electricity,                            !- Off Cycle Parasitic Fuel Type",
        "  0,                                      !- Off Cycle Parasitic Heat Fraction to Tank",
        "  0,                                      !- On Cycle Parasitic Fuel Consumption Rate {W}",
        "  Electricity,                            !- On Cycle Parasitic Fuel Type",
        "  0,                                      !- On Cycle Parasitic Heat Fraction to Tank",
        "  Schedule,                               !- Ambient Temperature Indicator",
        "  Ambient Temp Schedule,                  !- Ambient Temperature Schedule Name",
        "  ,                                 	   !- Ambient Temperature Zone Name",
        "  ,                                       !- Ambient Temperature Outdoor Air Node Name",
        "  0.704227539803499,                      !- Off Cycle Loss Coefficient to Ambient Temperature {W/K}",
        "  1,                                      !- Off Cycle Loss Fraction to Zone",
        "  0.704227539803499,                      !- On Cycle Loss Coefficient to Ambient Temperature {W/K}",
        "  1,                                      !- On Cycle Loss Fraction to Zone",
        "  ,                                       !- Peak Use Flow Rate {m3/s}",
        "  ALWAYS_ON,                              !- Use Flow Rate Fraction Schedule Name",
        "  ,                                       !- Cold Water Supply Temperature Schedule Name",
        "  ,                                       !- Use Side Inlet Node Name",
        "  ,                                       !- Use Side Outlet Node Name",
        "  1,                                      !- Use Side Effectiveness",
        "  DesuperheaterOut,			           !- Source Side Inlet Node Name",
        "  DesuperheaterIn,         	           !- Source Side Outlet Node Name",
        "  1,                                      !- Source Side Effectiveness",
        "  0.00283433494640006,                    !- Use Side Design Flow Rate {m3/s}",
        "  ,                                       !- Source Side Design Flow Rate {m3/s}",
        "  1.5,                                    !- Indirect Water Heating Recovery Time {hr}",
        "  IndirectHeatPrimarySetpoint,            !- Source Side Flow Control Mode",
        "  ,                                       !- Indirect Alternate Setpoint Temperature Schedule Name",
        "  General;                                !- End-Use Subcategory",

        "Coil:WaterHeating:Desuperheater,",
        "    Desuperheater,           !- Name",
        "    ALWAYS_ON,               !- Availability Schedule Name",
        "    Desuperheater-Schedule,  !- Setpoint Temperature Schedule Name",
        "    5,                       !- Dead Band Temperature Difference {deltaC}",
        "    0.25,                    !- Rated Heat Reclaim Recovery Efficiency",
        "    50,                      !- Rated Inlet Water Temperature {C}",
        "    35,                      !- Rated Outdoor Air Temperature {C}",
        "    60,                      !- Maximum Inlet Water Temperature for Heat Reclaim {C}",
        "    ,                        !- Heat Reclaim Efficiency Function of Temperature Curve Name",
        "    DesuperheaterIn,         !- Water Inlet Node Name",
        "    DesuperheaterOut,        !- Water Outlet Node Name",
        "    WaterHeater:Mixed,       !- Tank Object Type",
        "    Mixed tank with desuperheater,  !- Tank Name",
        "    Coil:Cooling:DX:MultiSpeed,  !- Heating Source Object Type",
        "    MultiSpeed_COIL,              !- Heating Source Name",
        "    0.0001,                  !- Water Flow Rate {m3/s}",
        "    ,                        !- Water Pump Power {W}",
        "    0.2;                     !- Fraction of Pump Heat to Water",


        "Coil:Cooling:DX:MultiSpeed,",
        "  MultiSpeed_COIL,                        !- Name",
        "  ALWAYS_ON,                     !- Availability Schedule Name",
        "  res ac unitary system Fan - Cooling Coil Node, !- Air Inlet Node Name",
        "  Node 17,                                !- Air Outlet Node Name",
        "  ,                                       !- Condenser Air Inlet Node Name",
        "  AirCooled,                              !- Condenser Type",
        "  ,                                       !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "  ,                                       !- Supply Water Storage Tank Name",
        "  ,                                       !- Condensate Collection Water Storage Tank Name",
        "  No,                                     !- Apply Part Load Fraction to Speeds Greater than 1",
        "  No,                                     !- Apply Latent Degradation to Speeds Greater than 1",
        "  0,                                      !- Crankcase Heater Capacity {W}",
        "  12.7777777777778,                       !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
        "  0,                                      !- Basin Heater Capacity {W/K}",
        "  2,                                      !- Basin Heater Setpoint Temperature {C}",
        "  ,                                       !- Basin Heater Operating Schedule Name",
        "  Electricity,                            !- Fuel Type",
        "  2,                                      !- Number of Speeds",
        "  10128.5361851424,                       !- Speed Gross Rated Total Cooling Capacity 1 {W}",
        "  0.714816560872937,                      !- Speed Gross Rated Sensible Heat Ratio 1",
        "  4.84033093564236,                       !- Speed Gross Rated Cooling COP 1 {W/W}",
        "  0.576666917476216,                      !- Speed Rated Air Flow Rate 1 {m3/s}",
        "  773.3,                                  !- Speed Rated Evaporator Fan Power Per Volume Flow Rate 1 {W/(m3/s)}",
        "  Cool-Cap-fT1,                           !- Speed Total Cooling Capacity Function of Temperature Curve Name 1",
        "  Cool-Cap-fFF1,                          !- Speed Total Cooling Capacity Function of Flow Fraction Curve Name 1",
        "  Cool-EIR-fT1,                           !- Speed Energy Input Ratio Function of Temperature Curve Name 1",
        "  Cool-EIR-fFF1,                          !- Speed Energy Input Ratio Function of Flow Fraction Curve Name 1",
        "  Cool-PLF-fPLR1,                         !- Speed Part Load Fraction Correlation Curve Name 1",
        "  1000,                                   !- Speed Nominal Time for Condensate Removal to Begin 1 {s}",
        "  1.5,                                    !- Speed Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity 1 {dimensionless}",
        "  3,                                      !- Speed Maximum Cycling Rate 1 {cycles/hr}",
        "  45,                                     !- Speed Latent Capacity Time Constant 1 {s}",
        "  0.2,                                    !- Speed Rated Waste Heat Fraction of Power Input 1 {dimensionless}",
        "  ConstantBiquadratic,                    !- Speed Waste Heat Function of Temperature Curve Name 1",
        "  0.9,                                    !- Speed Evaporative Condenser Effectiveness 1 {dimensionless}",
        "  0.05,                                   !- Speed Evaporative Condenser Air Flow Rate 1 {m3/s}",
        "  50,                                     !- Speed Rated Evaporative Condenser Pump Power Consumption 1 {W}",
        "  14067.4113682534,                       !- Speed Gross Rated Total Cooling Capacity 2 {W}",
        "  0.733764546660947,                      !- Speed Gross Rated Sensible Heat Ratio 2",
        "  4.34646555035634,                       !- Speed Gross Rated Cooling COP 2 {W/W}",
        "  0.670542927297926,                      !- Speed Rated Air Flow Rate 2 {m3/s}",
        "  773.3,                                  !- Speed Rated Evaporator Fan Power Per Volume Flow Rate 2 {W/(m3/s)}",
        "  Cool-Cap-fT2,                           !- Speed Total Cooling Capacity Function of Temperature Curve Name 2",
        "  Cool-Cap-fFF2,                          !- Speed Total Cooling Capacity Function of Flow Fraction Curve Name 2",
        "  Cool-EIR-fT2,                           !- Speed Energy Input Ratio Function of Temperature Curve Name 2",
        "  Cool-EIR-fFF2,                          !- Speed Energy Input Ratio Function of Flow Fraction Curve Name 2",
        "  Cool-PLF-fPLR2,                         !- Speed Part Load Fraction Correlation Curve Name 2",
        "  1000,                                   !- Speed Nominal Time for Condensate Removal to Begin 2 {s}",
        "  1.5,                                    !- Speed Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity 2 {dimensionless}",
        "  3,                                      !- Speed Maximum Cycling Rate 2 {cycles/hr}",
        "  45,                                     !- Speed Latent Capacity Time Constant 2 {s}",
        "  0.2,                                    !- Speed Rated Waste Heat Fraction of Power Input 2 {dimensionless}",
        "  ConstantBiquadratic,                    !- Speed Waste Heat Function of Temperature Curve Name 2",
        "  0.9,                                    !- Speed Evaporative Condenser Effectiveness 2 {dimensionless}",
        "  0.3,                                    !- Speed Evaporative Condenser Air Flow Rate 2 {m3/s}",
        "  100;                                    !- Speed Rated Evaporative Condenser Pump Power Consumption 2 {W}",

        "Curve:Biquadratic,",
        "  Cool-Cap-fT1,                           !- Name",
        "  1.66457706,                             !- Coefficient1 Constant",
        "  -0.0803905902,                          !- Coefficient2 x",
        "  0.00330252552,                          !- Coefficient3 x**2",
        "  0.001238751,                            !- Coefficient4 y",
        "  -3.08772e-005,                          !- Coefficient5 y**2",
        "  -0.00052377192,                         !- Coefficient6 x*y",
        "  13.88,                                  !- Minimum Value of x {BasedOnField A2}",
        "  23.88,                                  !- Maximum Value of x {BasedOnField A2}",
        "  18.33,                                  !- Minimum Value of y {BasedOnField A3}",
        "  51.66;                                  !- Maximum Value of y {BasedOnField A3}",

        "Curve:Quadratic,",
        "  Cool-Cap-fFF1,                          !- Name",
        "  0.65673024,                             !- Coefficient1 Constant",
        "  0.516470835,                            !- Coefficient2 x",
        "  -0.172887149,                           !- Coefficient3 x**2",
        "  0,                                      !- Minimum Value of x {BasedOnField A2}",
        "  2,                                      !- Maximum Value of x {BasedOnField A2}",
        "  0,                                      !- Minimum Curve Output {BasedOnField A3}",
        "  2;                                      !- Maximum Curve Output {BasedOnField A3}",

        "Curve:Biquadratic,",
        "  Cool-EIR-fT1,                           !- Name",
        "  -0.4273762,                             !- Coefficient1 Constant",
        "  0.1419060744,                           !- Coefficient2 x",
        "  -0.0041237262,                          !- Coefficient3 x**2",
        "  -0.0140625414,                          !- Coefficient4 y",
        "  0.00083109888,                          !- Coefficient5 y**2",
        "  -0.00043266636,                         !- Coefficient6 x*y",
        "  13.88,                                  !- Minimum Value of x {BasedOnField A2}",
        "  23.88,                                  !- Maximum Value of x {BasedOnField A2}",
        "  18.33,                                  !- Minimum Value of y {BasedOnField A3}",
        "  51.66;                                  !- Maximum Value of y {BasedOnField A3}",

        "Curve:Quadratic,",
        "  Cool-EIR-fFF1,                          !- Name",
        "  1.562945114,                            !- Coefficient1 Constant",
        "  -0.791859997,                           !- Coefficient2 x",
        "  0.230030877,                            !- Coefficient3 x**2",
        "  0,                                      !- Minimum Value of x {BasedOnField A2}",
        "  2,                                      !- Maximum Value of x {BasedOnField A2}",
        "  0,                                      !- Minimum Curve Output {BasedOnField A3}",
        "  2;                                      !- Maximum Curve Output {BasedOnField A3}",

        "Curve:Quadratic,",
        "  Cool-PLF-fPLR1,                         !- Name",
        "  0.89,                                   !- Coefficient1 Constant",
        "  0.11,                                   !- Coefficient2 x",
        "  0,                                      !- Coefficient3 x**2",
        "  0,                                      !- Minimum Value of x {BasedOnField A2}",
        "  1,                                      !- Maximum Value of x {BasedOnField A2}",
        "  0.7,                                    !- Minimum Curve Output {BasedOnField A3}",
        "  1;                                      !- Maximum Curve Output {BasedOnField A3}",

        "  Curve:Biquadratic,",
        "  Cool-Cap-fT2,                           !- Name",
        "  1.367878711,                            !- Coefficient1 Constant",
        "  -0.0625665258,                          !- Coefficient2 x",
        "  0.00279689112,                          !- Coefficient3 x**2",
        "  0.0050409684,                           !- Coefficient4 y",
        "  -6.804e-005,                            !- Coefficient5 y**2",
        "  -0.00045420264,                         !- Coefficient6 x*y",
        "  13.88,                                  !- Minimum Value of x {BasedOnField A2}",
        "  23.88,                                  !- Maximum Value of x {BasedOnField A2}",
        "  18.33,                                  !- Minimum Value of y {BasedOnField A3}",
        "  51.66;                                  !- Maximum Value of y {BasedOnField A3}",

        "Curve:Quadratic,",
        "  Cool-Cap-fFF2,                          !- Name",
        "  0.690334551,                            !- Coefficient1 Constant",
        "  0.464383753,                            !- Coefficient2 x",
        "  -0.154507638,                           !- Coefficient3 x**2",
        "  0,                                      !- Minimum Value of x {BasedOnField A2}",
        "  2,                                      !- Maximum Value of x {BasedOnField A2}",
        "  0,                                      !- Minimum Curve Output {BasedOnField A3}",
        "  2;                                      !- Maximum Curve Output {BasedOnField A3}",

        "Curve:Biquadratic,",
        "  Cool-EIR-fT2,                           !- Name",
        "  0.0423235170000003,                     !- Coefficient1 Constant",
        "  0.0789200082,                           !- Coefficient2 x",
        "  -0.002376054,                           !- Coefficient3 x**2",
        "  -0.0030359106,                          !- Coefficient4 y",
        "  0.00053492076,                          !- Coefficient5 y**2",
        "  -0.000323028,                           !- Coefficient6 x*y",
        "  13.88,                                  !- Minimum Value of x {BasedOnField A2}",
        "  23.88,                                  !- Maximum Value of x {BasedOnField A2}",
        "  18.33,                                  !- Minimum Value of y {BasedOnField A3}",
        "  51.66;                                  !- Maximum Value of y {BasedOnField A3}",

        "Curve:Quadratic,",
        "  Cool-EIR-fFF2,                          !- Name",
        "  1.31565404,                             !- Coefficient1 Constant",
        "  -0.482467162,                           !- Coefficient2 x",
        "  0.166239001,                            !- Coefficient3 x**2",
        "  0,                                      !- Minimum Value of x {BasedOnField A2}",
        "  2,                                      !- Maximum Value of x {BasedOnField A2}",
        "  0,                                      !- Minimum Curve Output {BasedOnField A3}",
        "  2;                                      !- Maximum Curve Output {BasedOnField A3}",

        "Curve:Quadratic,",
        "  Cool-PLF-fPLR2,                         !- Name",
        "  0.89,                                   !- Coefficient1 Constant",
        "  0.11,                                   !- Coefficient2 x",
        "  0,                                      !- Coefficient3 x**2",
        "  0,                                      !- Minimum Value of x {BasedOnField A2}",
        "  1,                                      !- Maximum Value of x {BasedOnField A2}",
        "  0.7,                                    !- Minimum Curve Output {BasedOnField A3}",
        "  1;                                      !- Maximum Curve Output {BasedOnField A3}",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound = false;
    HeatBalanceManager::GetZoneData(ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);

    DataGlobals::NumOfTimeStepInHour = 1; // must initialize this to get schedules initialized
    DataGlobals::MinutesPerTimeStep = 60; // must initialize this to get schedules initialized
    ScheduleManager::ProcessScheduleInput();
    ScheduleManager::ScheduleInputProcessed = true;

    DataGlobals::TimeStep = 1;
    DataGlobals::HourOfDay = 1;
    DataEnvironment::Month = 7;
    DataEnvironment::DayOfMonth = 21;
    DataGlobals::HourOfDay = 1;
    DataEnvironment::DSTIndicator = 0;
    DataEnvironment::DayOfWeek = 2;
    DataEnvironment::HolidayIndex = 0;
    DataEnvironment::DayOfYear_Schedule = General::OrdinalDay(DataEnvironment::Month, DataEnvironment::DayOfMonth, 1);
    ScheduleManager::UpdateScheduleValues();

    int TankNum(1);
    int DXNum(1);

    ErrorsFound = false;
    EXPECT_FALSE(WaterThermalTanks::GetWaterThermalTankInputData(ErrorsFound));
    //Source name and type successfully passed to DataHeatBalance::HeatReclaimDXCoil data struct
    EXPECT_EQ(DataHeatBalance::HeatReclaimDXCoil(1).Name, "MULTISPEED_COIL");
    EXPECT_EQ(DataHeatBalance::HeatReclaimDXCoil(1).SourceType, "Coil:Cooling:DX:MultiSpeed");

    //Initiate conditions for multispeed coil calculation
    DataEnvironment::OutDryBulbTemp = 32.0;
    DataEnvironment::OutHumRat = 0.02;
    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::OutWetBulbTemp = Psychrometrics::PsyTwbFnTdbWPb(32.0, 0.02, 101325.0);

    DXCoil(1).MSRatedAirMassFlowRate(1) = DXCoil(1).MSRatedAirVolFlowRate(1) * 1.2;
    DXCoil(1).MSRatedAirMassFlowRate(2) = DXCoil(1).MSRatedAirVolFlowRate(2) * 1.2;
    DXCoil(1).InletAirMassFlowRate = DXCoil(1).MSRatedAirMassFlowRate(2);
    DataHVACGlobals::MSHPMassFlowRateLow = DXCoil(1).MSRatedAirMassFlowRate(1);
    DataHVACGlobals::MSHPMassFlowRateHigh = DXCoil(1).MSRatedAirMassFlowRate(2);

    DXCoil(1).InletAirTemp = 27.0;
    DXCoil(1).InletAirHumRat = 0.005;
    DXCoil(1).InletAirEnthalpy = Psychrometrics::PsyHFnTdbW(27.0, 0.005);
    DXCoil(1).SchedPtr = 1;
    ScheduleManager::Schedule(DXCoil(1).SchedPtr).CurrentValue = 1.0; // enable the VRF condenser
    DXCoil(1).MSRatedCBF(1) = 0.1262;
    DXCoil(1).MSRatedCBF(2) = 0.0408;

    //Calculate multispeed DX cooling coils
    DXCoils::CalcMultiSpeedDXCoilCooling(DXNum, 1, 1, 2, 1, 1, 1);

    //Source availably heat successfully passed to DataHeatBalance::HeatReclaimDXCoil data struct
    EXPECT_EQ(DataHeatBalance::HeatReclaimDXCoil(DXNum).AvailCapacity, DXCoil(DXNum).TotalCoolingEnergyRate + DXCoil(DXNum).ElecCoolingPower);

    //Now move to the water thermal tank calculation
    WaterThermalTanks::WaterThermalTankData &Tank = WaterThermalTank(TankNum);
    WaterThermalTanks::WaterHeaterDesuperheaterData &Desuperheater = WaterHeaterDesuperheater(Tank.DesuperheaterNum);

    //Inititate tank conditions
    HourOfDay = 0;
    TimeStep = 1;
    TimeStepZone = 1;
    TimeStepSys = TimeStepZone;
    SysTimeElapsed = 0.0;
    Tank.TankTemp = 45.0;
    Tank.AmbientTemp = 20.0;
    Tank.UseInletTemp = 10.0;
    Tank.UseMassFlowRate = 0.00001;
    Tank.SourceOutletTemp = 45.0;
    Node(Desuperheater.WaterInletNode).Temp = Tank.SourceOutletTemp;
    Tank.SourceMassFlowRate = 0.003;
    Tank.TimeElapsed = 0.0;
    Desuperheater.SetPointTemp = 50.0;
    Desuperheater.Mode = 1;

    Tank.Mode = 0;
    Tank.SetPointTemp = 40.0;
    Desuperheater.FirstTimeThroughFlag = true;
    WaterThermalTanks::CalcDesuperheaterWaterHeater(TankNum,true);

    EXPECT_EQ(Desuperheater.DXSysPLR, DXCoil(DXNum).PartLoadRatio);
    //if desuperheater was not on through all the timestep, part load ratio is searched to meet load demand
    EXPECT_GE(Desuperheater.DXSysPLR, Desuperheater.DesuperheaterPLR);
    //total available capacity is substrated by used desuperheater reclaim heat
    EXPECT_EQ(DXCoil(DXNum).TotalCoolingEnergyRate + DXCoil(DXNum).ElecCoolingPower, DataHeatBalance::HeatReclaimDXCoil(DXNum).AvailCapacity + Desuperheater.HeaterRate);
    //Desuperheater heater rate is correctly calculated
    EXPECT_EQ(Desuperheater.HeaterRate, (DataHeatBalance::HeatReclaimDXCoil(DXNum).AvailCapacity + Desuperheater.HeaterRate) / Desuperheater.DXSysPLR * Desuperheater.DesuperheaterPLR * 0.25);

    //Test the float mode
    Tank.SavedTankTemp = 61.0;
    Tank.SavedSourceOutletTemp = 61.0;
    Desuperheater.SaveMode = 0;
    Node(Desuperheater.WaterInletNode).Temp = Tank.SavedSourceOutletTemp;
    Tank.SourceMassFlowRate = 0.003;
    DXCoils::CalcMultiSpeedDXCoilCooling(DXNum, 1, 1, 2, 1, 1, 1);
    WaterThermalTanks::CalcDesuperheaterWaterHeater(TankNum, false);
    EXPECT_EQ(Desuperheater.Mode, 0);
    EXPECT_EQ(Desuperheater.HeaterRate, 0.0);
    EXPECT_EQ(Tank.SourceRate, 0.0);
}

TEST_F(EnergyPlusFixture, MixedTankAlternateSchedule){
    using DataGlobals::HourOfDay;
    using DataGlobals::TimeStep;
    using DataGlobals::TimeStepZone;
    using DataGlobals::SecInHour;
    using DataHVACGlobals::SysTimeElapsed;
    using DataHVACGlobals::TimeStepSys;
    using WaterThermalTanks::WaterThermalTank;
    using FluidProperties::GetDensityGlycol;

    std::string const idf_objects = delimited_string({
        "Schedule:Constant, Inlet Water Temperature, , 10.0;",
        "Schedule:Constant, Water Heater Setpoint Temperature, ,55.0;",
        "Schedule:Constant, Water Heater AltSetpoint Temperature, ,70.0;",
        "Schedule:Constant, Ambient Temp Schedule, , 20.0;",

        "WaterHeater:Mixed,",
        "    IndirectWaterTank,       !- Name",
        "    0.07,                    !- Tank Volume {m3}",
        "    Water Heater Setpoint Temperature,  !- Setpoint Temperature Schedule Name",
        "    2,                       !- Deadband Temperature Difference {deltaC}",
        "    99,                      !- Maximum Temperature Limit {C}",
        "    Cycle,                   !- Heater Control Type",
        "    0,                       !- Heater Maximum Capacity {W}",
        "    ,                        !- Heater Minimum Capacity {W}",
        "    0,                       !- Heater Ignition Minimum Flow Rate {m3/s}",
        "    0,                       !- Heater Ignition Delay {s}",
        "    Electricity,             !- Heater Fuel Type",
        "    0.8,                     !- Heater Thermal Efficiency",
        "    ,                        !- Part Load Factor Curve Name",
        "    0,                       !- Off Cycle Parasitic Fuel Consumption Rate {W}",
        "    Electricity,             !- Off Cycle Parasitic Fuel Type",
        "    0.8,                     !- Off Cycle Parasitic Heat Fraction to Tank",
        "    0,                       !- On Cycle Parasitic Fuel Consumption Rate {W}",
        "    Electricity,             !- On Cycle Parasitic Fuel Type",
        "    0,                       !- On Cycle Parasitic Heat Fraction to Tank",
        "    SCHEDULE,                !- Ambient Temperature Indicator",
        "    Ambient Temp Schedule,   !- Ambient Temperature Schedule Name",
        "    ,                        !- Ambient Temperature Zone Name",
        "    ,                        !- Ambient Temperature Outdoor Air Node Name",
        "    1,                       !- Off Cycle Loss Coefficient to Ambient Temperature {W/K}",
        "    1,                       !- Off Cycle Loss Fraction to Zone",
        "    6,                       !- On Cycle Loss Coefficient to Ambient Temperature {W/K}",
        "    1,                       !- On Cycle Loss Fraction to Zone",
        "    ,                        !- Peak Use Flow Rate {m3/s}",
        "    ,                        !- Use Flow Rate Fraction Schedule Name",
        "    ,                        !- Cold Water Supply Temperature Schedule Name",
        "    ,                        !- Use Side Inlet Node Name",
        "    ,                        !- Use Side Outlet Node Name",
        "    1,                       !- Use Side Effectiveness",
        "    DemandIn,                !- Source Side Inlet Node Name",
        "    DemandOut,               !- Source Side Outlet Node Name",
        "    1,                       !- Source Side Effectiveness",
        "    Autosize,                !- Use Side Design Flow Rate {m3/s}",
        "    0.0005,                  !- Source Side Design Flow Rate {m3/s}",
        "    1.5,                     !- Indirect Water Heating Recovery Time {hr}",
        "    IndirectHeatAlternateSetpoint,  !- Source Side Flow Control Mode",
        "    Water Heater AltSetpoint Temperature,      !- Indirect Alternate Setpoint Temperature Schedule Name",
        "    General;                 !- End-Use Subcategory",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound = false;

    //Schedules setup
    DataGlobals::NumOfTimeStepInHour = 1; // must initialize this to get schedules initialized
    DataGlobals::MinutesPerTimeStep = 60; // must initialize this to get schedules initialized
    ScheduleManager::ProcessScheduleInput();
    ScheduleManager::ScheduleInputProcessed = true;

    DataGlobals::TimeStep = 1;
    DataGlobals::HourOfDay = 1;
    DataEnvironment::Month = 7;
    DataEnvironment::DayOfMonth = 21;
    DataGlobals::HourOfDay = 1;
    DataEnvironment::DSTIndicator = 0;
    DataEnvironment::DayOfWeek = 2;
    DataEnvironment::HolidayIndex = 0;
    DataEnvironment::DayOfYear_Schedule = General::OrdinalDay(DataEnvironment::Month, DataEnvironment::DayOfMonth, 1);
    ScheduleManager::UpdateScheduleValues();

    //Get tank input data
    ErrorsFound = false;
    EXPECT_FALSE(WaterThermalTanks::GetWaterThermalTankInputData(ErrorsFound));

    int TankNum(1);
    int DemandSide(1);
    Real64 rho; 
    int WaterIndex(1);
    bool NeedsHeatOrCool;
    WaterThermalTanks::WaterThermalTankData &Tank = WaterThermalTank(TankNum);

    //set tank temp to be alternate setpoint
    Tank.TankTemp = 70.0;
    Tank.SetPointTemp = 55.0;

    //Source side is in the demand side of the plant loop
    Tank.SourceSidePlantLoopSide = DemandSide;
    Tank.SavedSourceOutletTemp = 60.0;
    rho = GetDensityGlycol("Water", Tank.TankTemp, WaterIndex , "MixedTankAlternateSchedule");

    //Set the available max flow rates for tank and node
    Tank.PlantSourceMassFlowRateMax = Tank.SourceDesignVolFlowRate * rho;
    DataLoopNode::Node(1).MassFlowRateMax = Tank.PlantSourceMassFlowRateMax;
    DataLoopNode::Node(1).MassFlowRateMaxAvail = Tank.PlantSourceMassFlowRateMax;

    NeedsHeatOrCool = WaterThermalTanks::SourceHeatNeed(Tank, 70.0, Tank.SetPointTemp - 2.0, Tank.SetPointTemp);
    EXPECT_FALSE(NeedsHeatOrCool);

    //set tank temp between 55 to 70 to enable alternate setpoint control
    Tank.TankTemp = 60.0;
    NeedsHeatOrCool = WaterThermalTanks::SourceHeatNeed(Tank, 60.0, Tank.SetPointTemp - 2.0, Tank.SetPointTemp);
    EXPECT_TRUE(NeedsHeatOrCool);

    //plant mass flow rate logic for firstHVAC mode not crashed
    WaterThermalTanks::InitWaterThermalTank(TankNum,true);
    EXPECT_EQ(Tank.SourceMassFlowRate, 0.0005* rho);

    //plant mass flow rate logic added to other iterations run
    WaterThermalTanks::InitWaterThermalTank(TankNum,false);
    EXPECT_EQ(Tank.SourceMassFlowRate, 0.0005* rho);

}

TEST_F(EnergyPlusFixture, MixedTank_WarnPotentialFreeze)
{
    std::string const idf_objects = delimited_string({
        "  Schedule:Constant, Water Heater Setpoint Temperature, ,12;",
        "  Schedule:Constant, Tank Ambient Temperature, , -40;", // That's cold!

        "  WaterHeater:Mixed,",
        "    ChilledWaterTank,        !- Name",
        "    0.07,                    !- Tank Volume {m3}",
        "    Water Heater Setpoint Temperature,  !- Setpoint Temperature Schedule Name",
        "    2,                       !- Deadband Temperature Difference {deltaC}",
        "    30,                      !- Maximum Temperature Limit {C}",
        "    Cycle,                   !- Heater Control Type",
        "    0,                       !- Heater Maximum Capacity {W}",
        "    ,                        !- Heater Minimum Capacity {W}",
        "    0,                       !- Heater Ignition Minimum Flow Rate {m3/s}",
        "    0,                       !- Heater Ignition Delay {s}",
        "    Electricity,             !- Heater Fuel Type",
        "    0.8,                     !- Heater Thermal Efficiency",
        "    ,                        !- Part Load Factor Curve Name",
        "    0,                       !- Off Cycle Parasitic Fuel Consumption Rate {W}",
        "    Electricity,             !- Off Cycle Parasitic Fuel Type",
        "    0.8,                     !- Off Cycle Parasitic Heat Fraction to Tank",
        "    0,                       !- On Cycle Parasitic Fuel Consumption Rate {W}",
        "    Electricity,             !- On Cycle Parasitic Fuel Type",
        "    0,                       !- On Cycle Parasitic Heat Fraction to Tank",
        "    Schedule,                !- Ambient Temperature Indicator",
        "    Tank Ambient Temperature,!- Ambient Temperature Schedule Name",
        "    ,                        !- Ambient Temperature Zone Name",
        "    ,                        !- Ambient Temperature Outdoor Air Node Name",
        "    6,                       !- Off Cycle Loss Coefficient to Ambient Temperature {W/K}",
        "    1,                       !- Off Cycle Loss Fraction to Zone",
        "    6,                       !- On Cycle Loss Coefficient to Ambient Temperature {W/K}",
        "    1,                       !- On Cycle Loss Fraction to Zone",
        "    ,                        !- Peak Use Flow Rate {m3/s}",
        "    ,                        !- Use Flow Rate Fraction Schedule Name",
        "    ,                        !- Cold Water Supply Temperature Schedule Name",
        "    ,                        !- Use Side Inlet Node Name",
        "    ,                        !- Use Side Outlet Node Name",
        "    1,                       !- Use Side Effectiveness",
        "    ,                        !- Source Side Inlet Node Name",
        "    ,                        !- Source Side Outlet Node Name",
        "    1,                       !- Source Side Effectiveness",
        "    Autosize,                !- Use Side Design Flow Rate {m3/s}",
        "    Autosize,                !- Source Side Design Flow Rate {m3/s}",
        "    1.5;                     !- Indirect Water Heating Recovery Time {hr}",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound = false;
    //HeatBalanceManager::GetZoneData(ErrorsFound); // read zone data
    //EXPECT_FALSE(ErrorsFound);

    //InternalHeatGains::GetInternalHeatGainsInput();
    //ErrorsFound = false;
    EXPECT_FALSE(WaterThermalTanks::GetWaterThermalTankInputData(ErrorsFound));

    int TankNum(1);
    WaterThermalTanks::WaterThermalTankData &Tank = WaterThermalTanks::WaterThermalTank(TankNum);

    DataGlobals::HourOfDay = 0;
    DataGlobals::TimeStep = 1;
    DataGlobals::TimeStepZone = 1.0 / 60.0; // one-minute system time step
    DataHVACGlobals::TimeStepSys = DataGlobals::TimeStepZone;

    Tank.TankTemp = 2.0;
    Tank.AmbientTemp = -40;
    Tank.UseInletTemp = 3.0;
    Tank.SetPointTemp = 3.0;
    Tank.SetPointTemp2 = Tank.SetPointTemp;
    Tank.TimeElapsed = 0.0;

    // very low use mass flow rate
    Tank.UseMassFlowRate = 0.00005;
    // zero source mass flow rate
    Tank.SourceMassFlowRate = 0.0;

    // Calls CalcWaterThermalTankMixed
    WaterThermalTanks::CalcWaterThermalTank(TankNum);

    // expected tank avg temp less than starting value of 2 C
    EXPECT_LT(Tank.TankTempAvg, 2.0);
    // And the final tank temp too, which is the one triggering the warning
    EXPECT_LT(Tank.TankTemp, 2.0);

    std::string const error_string = delimited_string({
      "   ** Warning ** CalcWaterThermalTankMixed: WaterHeater:Mixed = 'CHILLEDWATERTANK':  Temperature of tank < 2C indicates of possibility of freeze. Tank Temperature = 1.95 C.",
      "   **   ~~~   **  Environment=, at Simulation time= 00:-1 - 00:00"
    });
    EXPECT_TRUE(compare_err_stream(error_string, true));

}

TEST_F(EnergyPlusFixture, StratifiedTank_WarnPotentialFreeze)
{
    std::string const idf_objects = delimited_string({
        "  Schedule:Constant, Water Heater Setpoint Temperature, ,12;",
        "  Schedule:Constant, Tank Ambient Temperature, , -40;", // That's cold!

        "WaterHeater:Stratified,",
        "  Stratified ChilledWaterTank, !- Name",
        "  ,                        !- End-Use Subcategory",
        "  0.17,                    !- Tank Volume {m3}",
        "  1.4,                     !- Tank Height {m}",
        "  VerticalCylinder,        !- Tank Shape",
        "  ,                        !- Tank Perimeter {m}",
        "  82.2222,                 !- Maximum Temperature Limit {C}",
        "  MasterSlave,             !- Heater Priority Control",
        "  Water Heater Setpoint Temperature,  !- Heater 1 Setpoint Temperature Schedule Name",
        "  2.0,                     !- Heater 1 Deadband Temperature Difference {deltaC}",
        "  0,                       !- Heater 1 Capacity {W}",
        "  1.0,                     !- Heater 1 Height {m}",
        "  Water Heater Setpoint Temperature,  !- Heater 2 Setpoint Temperature Schedule Name",
        "  5.0,                     !- Heater 2 Deadband Temperature Difference {deltaC}",
        "  0,                       !- Heater 2 Capacity {W}",
        "  0.0,                     !- Heater 2 Height {m}",
        "  ELECTRICITY,             !- Heater Fuel Type",
        "  1,                       !- Heater Thermal Efficiency",
        "  ,                        !- Off Cycle Parasitic Fuel Consumption Rate {W}",
        "  ELECTRICITY,             !- Off Cycle Parasitic Fuel Type",
        "  ,                        !- Off Cycle Parasitic Heat Fraction to Tank",
        "  ,                        !- Off Cycle Parasitic Height {m}",
        "  ,                        !- On Cycle Parasitic Fuel Consumption Rate {W}",
        "  ELECTRICITY,             !- On Cycle Parasitic Fuel Type",
        "  ,                        !- On Cycle Parasitic Heat Fraction to Tank",
        "  ,                        !- On Cycle Parasitic Height {m}",
        "  SCHEDULE,                !- Ambient Temperature Indicator",
        "  Tank Ambient Temperature,!- Ambient Temperature Schedule Name",
        "  ,                        !- Ambient Temperature Zone Name",
        "  ,                        !- Ambient Temperature Outdoor Air Node Name",
        "  6,                       !- Uniform Skin Loss Coefficient per Unit Area to Ambient Temperature {W/m2-K}",
        "  1,                       !- Skin Loss Fraction to Zone",
        "  6,                       !- Off Cycle Flue Loss Coefficient to Ambient Temperature {W/K}",
        "  1,                       !- Off Cycle Flue Loss Fraction to Zone",
        "  ,                        !- Peak Use Flow Rate {m3/s}",
        "  ,                        !- Use Flow Rate Fraction Schedule Name",
        "  ,                        !- Cold Water Supply Temperature Schedule Name",
        "  ,                        !- Use Side Inlet Node Name",
        "  ,                        !- Use Side Outlet Node Name",
        "  ,                        !- Use Side Effectiveness",
        "  1.0,                     !- Use Side Inlet Height {m}",
        "  0.5,                     !- Use Side Outlet Height {m}",
        "  ,                        !- Source Side Inlet Node Name",
        "  ,                        !- Source Side Outlet Node Name",
        "  ,                        !- Source Side Effectiveness",
        "  ,                        !- Source Side Inlet Height {m}",
        "  ,                        !- Source Side Outlet Height {m}",
        "  FIXED,                   !- Inlet Mode",
        "  ,                        !- Use Side Design Flow Rate {m3/s}",
        "  ,                        !- Source Side Design Flow Rate {m3/s}",
        "  ,                        !- Indirect Water Heating Recovery Time {hr}",
        "  10,                      !- Number of Nodes",
        "  0.1;                     !- Additional Destratification Conductivity {W/m-K}",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound = false;
    //HeatBalanceManager::GetZoneData(ErrorsFound); // read zone data
    //EXPECT_FALSE(ErrorsFound);

    //InternalHeatGains::GetInternalHeatGainsInput();
    //ErrorsFound = false;
    EXPECT_FALSE(WaterThermalTanks::GetWaterThermalTankInputData(ErrorsFound));

    int TankNum(1);
    WaterThermalTanks::WaterThermalTankData &Tank = WaterThermalTanks::WaterThermalTank(TankNum);

    DataGlobals::HourOfDay = 0;
    DataGlobals::TimeStep = 1;
    DataGlobals::TimeStepZone = 1.0 / 60.0; // one-minute system time step
    DataHVACGlobals::TimeStepSys = DataGlobals::TimeStepZone;

    Tank.TankTemp = 2.0;
    for (auto &node : Tank.Node) {
        node.Temp = 2.0;
        node.SavedTemp = 2.0;
    }

    Tank.AmbientTemp = -40;
    Tank.UseInletTemp = 3.0;
    Tank.SetPointTemp = 3.0;
    Tank.SetPointTemp2 = Tank.SetPointTemp;
    Tank.TimeElapsed = 0.0;

    // very low use mass flow rate
    Tank.UseMassFlowRate = 0.00005;
    // zero source mass flow rate
    Tank.SourceMassFlowRate = 0.0;

    // Calls CalcWaterThermalTankStratified
    WaterThermalTanks::CalcWaterThermalTank(TankNum);

    // expected tank avg temp less than starting value of 2 C
    EXPECT_LT(Tank.TankTempAvg, 2.0);
    // And the final tank temp too, which is the one triggering the warning
    EXPECT_LT(Tank.TankTemp, 2.0);
    // Might as well check the node temps too
    for (int i = 0; i < Tank.Nodes; ++i) {
        EXPECT_LT(Tank.Node[i].Temp, 2.0) << "Node i=" << i;
    }

    std::string const error_string = delimited_string({
      "   ** Warning ** CalcWaterThermalTankStratified: WaterHeater:Stratified = 'STRATIFIED CHILLEDWATERTANK':  Temperature of tank < 2C indicates of possibility of freeze. Tank Temperature = 1.75 C.",
      "   **   ~~~   **  Environment=, at Simulation time= 00:-1 - 00:00"
    });
    EXPECT_TRUE(compare_err_stream(error_string, true));

}
