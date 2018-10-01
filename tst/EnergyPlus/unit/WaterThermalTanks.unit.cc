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
#include <EnergyPlus/InternalHeatGains.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterThermalTanks.hh>
#include <Fixtures/EnergyPlusFixture.hh>

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
        "    WaterHeater:HeatPump:PumpedCondenser,    !- Zone Equipment 2 Object Type",
        "    Zone4HeatPumpWaterHeater,!- Zone Equipment 2 Name",
        "    2,                       !- Zone Equipment 2 Cooling Sequence",
        "    2;                       !- Zone Equipment 2 Heating or No-Load Sequence",
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

    std::vector<std::string> idf_lines({"Version, 8.4;",
                                        "Schedule:Constant,DummySch,,1.0;",
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
        "Version, 8.4;",
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
        "    WaterHeater:HeatPump:PumpedCondenser,    !- Zone Equipment 2 Object Type",
        "    Zone4HeatPumpWaterHeater,!- Zone Equipment 2 Name",
        "    2,                       !- Zone Equipment 2 Cooling Sequence",
        "    2;                       !- Zone Equipment 2 Heating or No-Load Sequence",
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
    EXPECT_NEAR(Tank.SourceOutletTemp, 10.347, 0.001);
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

