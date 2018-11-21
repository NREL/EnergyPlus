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

// EnergyPlus::HVACVariableRefrigerantFlow unit tests

// Google test headers
#include <gtest/gtest.h>

// C++ Headers
#include <cassert>
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <BranchInputManager.hh>
#include <CurveManager.hh>
#include <DXCoils.hh>
#include <DataAirLoop.hh>
#include <DataAirSystems.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataHVACGlobals.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalance.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataSizing.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <Fans.hh>
#include <FluidProperties.hh>
#include <GlobalNames.hh>
#include <HVACFan.hh>
#include <HVACVariableRefrigerantFlow.hh>
#include <HeatBalanceManager.hh>
#include <OutputReportPredefined.hh>
#include <Plant/PlantManager.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <SizingManager.hh>

using namespace EnergyPlus;
using namespace DXCoils;
using namespace EnergyPlus::BranchInputManager;
using namespace EnergyPlus::CurveManager;
using namespace EnergyPlus::DataAirLoop;
using namespace EnergyPlus::DataAirSystems;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataHeatBalFanSys;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataPlant;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DataZoneEnergyDemands;
using namespace EnergyPlus::FluidProperties;
using namespace EnergyPlus::DXCoils;
using namespace EnergyPlus::Fans;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::HVACFan;
using namespace EnergyPlus::HVACVariableRefrigerantFlow;
using namespace EnergyPlus::GlobalNames;
using namespace EnergyPlus::OutputReportPredefined;
using namespace EnergyPlus::PlantManager;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::SizingManager;

namespace EnergyPlus {

//*****************VRF-FluidTCtrl Model
TEST_F(EnergyPlusFixture, VRF_FluidTCtrl_VRFOU_Compressor)
{
    //   PURPOSE OF THIS TEST:
    //   Test a group of methods related with the outdoor unit compressor calculations in the VRF_FluidTCtrl model.

    // Inputs_general
    int const FlagCondMode(0); // Flag for running as condenser [-]
    int const FlagEvapMode(1); // Flag for running as evaporator [-]
    bool ErrorsFound(false);   // function returns true on error
    int VRFCond(1);            // index to VRF condenser

    std::string const idf_objects = delimited_string({
        "Version,8.5;",
        " ",
        "AirConditioner:VariableRefrigerantFlow:FluidTemperatureControl:HR,                         ",
        " VRF Heat Pump,           !- Name                                                          ",
        " ,                        !- Availability Schedule Name                                    ",
        " VRF Heat Pump TU List,   !- Zone Terminal Unit List Name                                  ",
        " R410A,                   !- Refrigerant Type                                              ",
        " 48757,                   !- Rated Evaporative Capacity {W}                                ",
        " 0.214,                   !- Rated Compressor Power Per Unit of Rated Evaporative Capacity ",
        " -6,                      !- Minimum Outdoor Air Temperature in Cooling Only Mode {C}      ",
        " 43,                      !- Maximum Outdoor Air Temperature in Cooling Only Mode {C}      ",
        " -20,                     !- Minimum Outdoor Air Temperature in Heating Only Mode {C}      ",
        " 26,                      !- Maximum Outdoor Air Temperature in Heating Only Mode {C}      ",
        " -20,                     !- Minimum Outdoor Air Temperature in Heat Recovery Mode {C}     ",
        " 26,                      !- Maximum Outdoor Air Temperature in Heat Recovery Mode {C}     ",
        " ConstantTemp,            !- Refrigerant Temperature Control Algorithm for Indoor Unit     ",
        " 6,                       !- Reference Evaporating Temperature for Indoor Unit {C}         ",
        " 44,                      !- Reference Condensing Temperature for Indoor Unit {C}          ",
        " 5,                       !- Variable Evaporating Temperature Minimum for Indoor Unit {C}  ",
        " 14,                      !- Variable Evaporating Temperature Maximum for Indoor Unit {C}  ",
        " 36,                      !- Variable Condensing Temperature Minimum for Indoor Unit {C}   ",
        " 46,                      !- Variable Condensing Temperature Maximum for Indoor Unit {C}   ",
        " 3,                       !- Outdoor Unit Evaporator Reference Superheating {C}            ",
        " 3,                       !- Outdoor Unit Condenser Reference Subcooling {C}               ",
        " 0.28,                    !- Outdoor Unit Evaporator Rated Bypass Factor                   ",
        " 0.05,                    !- Outdoor Unit Condenser Rated Bypass Factor                    ",
        " 5,                       !- Difference between Outdoor Unit Evaporating Temperature and Ou",
        " 0.3,                     !- Outdoor Unit Heat Exchanger Capacity Ratio                    ",
        " 2.67E-2,                 !- Outdoor Unit Fan Power Per Unit of Rated Evaporative Capacity ",
        " 1.13E-4,                 !- Outdoor Unit Fan Flow Rate Per Unit of Rated Evaporative Capac",
        " OUEvapTempCurve,         !- Outdoor Unit Evaporating Temperature Function of Superheating ",
        " OUCondTempCurve,         !- Outdoor Unit Condensing Temperature Function of Subcooling Cur",
        " 0.0349,                  !- Diameter of Main Pipe for Suction Gas {m}                     ",
        " 0.0286,                  !- Diameter of Main Pipe for Discharge Gas {m}                   ",
        " 30,                      !- Length of main pipe connecting outdoor unit to indoor units {m",
        " 36,                      !- Equivalent length of main pipe connecting outdoor unit to indo",
        " 5,                       !- Height difference between the outdoor unit node and indoor uni",
        " 0.02,                    !- Insulation thickness of the main pipe {m}                     ",
        " 0.032,                   !- Thermal conductivity of the main pipe insulation material {W/m",
        " 33,                      !- Crankcase Heater Power per Compressor {W}                     ",
        " 3,                       !- Number of Compressors                                         ",
        " 0.33,                    !- Ratio of Compressor Size to Total Compressor Capacity         ",
        " 7,                       !- Maximum Outdoor Dry-bulb Temperature for Crankcase Heater {C} ",
        " ,                        !- Defrost Strategy                                              ",
        " ,                        !- Defrost Control                                               ",
        " ,                        !- Defrost Energy Input Ratio Modifier Function of Temperature Cu",
        " ,                        !- Defrost Time Period Fraction                                  ",
        " ,                        !- Resistive Defrost Heater Capacity {W}                         ",
        " ,                        !- Maximum Outdoor Dry-bulb Temperature for Defrost Operation {C}",
        " ,                        !- Initial Heat Recovery Cooling Capacity Fraction {W/W}         ",
        " ,                        !- Heat Recovery Cooling Capacity Time Constant {hr}             ",
        " ,                        !- Initial Heat Recovery Cooling Energy Fraction {W/W}           ",
        " ,                        !- Heat Recovery Cooling Energy Time Constant {hr}               ",
        " ,                        !- Initial Heat Recovery Heating Capacity Fraction {W/W}         ",
        " ,                        !- Heat Recovery Heating Capacity Time Constant {hr}             ",
        " ,                        !- Initial Heat Recovery Heating Energy Fraction {W/W}           ",
        " ,                        !- Heat Recovery Heating Energy Time Constant {hr}               ",
        " 4500000,                 !- Compressor maximum delta Pressure {Pa}                        ",
        " 0.95,                    !- Compressor Inverter Efficiency                                ",
        " ,                        !- Compressor Evaporative Capacity Correction Factor             ",
        " 3,                       !- Number of Compressor Loading Index Entries                    ",
        " 1500,                    !- Compressor Speed at Loading Index 1 {rev/min}                 ",
        " MinSpdCooling,           !- Loading Index 1 Evaporative Capacity Multiplier Function of Te",
        " MinSpdPower,             !- Loading Index 1 Compressor Power Multiplier Function of Temper",
        " 3600,                    !- Compressor Speed at Loading Index 2 {rev/min}                 ",
        " Spd1Cooling,             !- Loading Index 2 Evaporative Capacity Multiplier Function of Te",
        " Spd1Power,               !- Loading Index 2 Compressor Power Multiplier Function of Temper",
        " 6000,                    !- Compressor Speed at Loading Index 3 {rev/min}                 ",
        " Spd2Cooling,             !- Loading Index 3 Evaporative Capacity Multiplier Function of Te",
        " Spd2Power;               !- Loading Index 3 Compressor Power Multiplier Function of Temper",
        "                                                                                           ",
        "Curve:Quadratic,                                     ",
        " OUEvapTempCurve,         !- Name                    ",
        " 0,                       !- Coefficient1 Constant   ",
        " 6.05E-1,                 !- Coefficient2 x          ",
        " 2.50E-2,                 !- Coefficient3 x**2       ",
        " 0,                       !- Minimum Value of x      ",
        " 15,                      !- Maximum Value of x      ",
        " ,                        !- Minimum Curve Output    ",
        " ,                        !- Maximum Curve Output    ",
        " Temperature,             !- Input Unit Type for X   ",
        " Temperature;             !- Output Unit Type        ",
        "                                                     ",
        "Curve:Quadratic,                                     ",
        " OUCondTempCurve,         !- Name                    ",
        " 0,                       !- Coefficient1 Constant   ",
        " -2.91,                   !- Coefficient2 x          ",
        " 1.180,                   !- Coefficient3 x**2       ",
        " 0,                       !- Minimum Value of x      ",
        " 20,                      !- Maximum Value of x      ",
        " ,                        !- Minimum Curve Output    ",
        " ,                        !- Maximum Curve Output    ",
        " Temperature,             !- Input Unit Type for X   ",
        " Temperature;             !- Output Unit Type        ",
        "                                                     ",
        "Curve:Biquadratic,                                   ",
        " MinSpdCooling,           !- Name                    ",
        " 3.19E-01,                !- Coefficient1 Constant   ",
        " -1.26E-03,               !- Coefficient2 x          ",
        " -2.15E-05,               !- Coefficient3 x**2       ",
        " 1.20E-02,                !- Coefficient4 y          ",
        " 1.05E-04,                !- Coefficient5 y**2       ",
        " -8.66E-05,               !- Coefficient6 x*y        ",
        " 15,                      !- Minimum Value of x      ",
        " 65,                      !- Maximum Value of x      ",
        " -30,                     !- Minimum Value of y      ",
        " 15,                      !- Maximum Value of y      ",
        " ,                        !- Minimum Curve Output    ",
        " ,                        !- Maximum Curve Output    ",
        " Temperature,             !- Input Unit Type for X   ",
        " Temperature,             !- Input Unit Type for Y   ",
        " Dimensionless;           !- Output Unit Type        ",
        "                                                     ",
        "Curve:Biquadratic,                                   ",
        " MinSpdPower,             !- Name                    ",
        " 8.79E-02 ,               !- Coefficient1 Constant   ",
        " -1.72E-04,               !- Coefficient2 x          ",
        " 6.93E-05 ,               !- Coefficient3 x**2       ",
        " -3.38E-05,               !- Coefficient4 y          ",
        " -8.10E-06,               !- Coefficient5 y**2       ",
        " -1.04E-05,               !- Coefficient6 x*y        ",
        " 15,                      !- Minimum Value of x      ",
        " 65,                      !- Maximum Value of x      ",
        " -30,                     !- Minimum Value of y      ",
        " 15,                      !- Maximum Value of y      ",
        " ,                        !- Minimum Curve Output    ",
        " ,                        !- Maximum Curve Output    ",
        " Temperature,             !- Input Unit Type for X   ",
        " Temperature,             !- Input Unit Type for Y   ",
        " Dimensionless;           !- Output Unit Type        ",
        "                                                     ",
        "Curve:Biquadratic,                                   ",
        " Spd1Cooling,             !- Name                    ",
        " 8.12E-01 ,               !- Coefficient1 Constant   ",
        " -4.23E-03,               !- Coefficient2 x          ",
        " -4.11E-05,               !- Coefficient3 x**2       ",
        " 2.97E-02 ,               !- Coefficient4 y          ",
        " 2.67E-04 ,               !- Coefficient5 y**2       ",
        " -2.23E-04,               !- Coefficient6 x*y        ",
        " 15,                      !- Minimum Value of x      ",
        " 65,                      !- Maximum Value of x      ",
        " -30,                     !- Minimum Value of y      ",
        " 15,                      !- Maximum Value of y      ",
        " ,                        !- Minimum Curve Output    ",
        " ,                        !- Maximum Curve Output    ",
        " Temperature,             !- Input Unit Type for X   ",
        " Temperature,             !- Input Unit Type for Y   ",
        " Dimensionless;           !- Output Unit Type        ",
        "                                                     ",
        "Curve:Biquadratic,                                   ",
        " Spd1Power,               !- Name                    ",
        " 3.26E-01 ,               !- Coefficient1 Constant   ",
        " -2.20E-03,               !- Coefficient2 x          ",
        " 1.42E-04 ,               !- Coefficient3 x**2       ",
        " 2.82E-03 ,               !- Coefficient4 y          ",
        " 2.86E-05 ,               !- Coefficient5 y**2       ",
        " -3.50E-05,               !- Coefficient6 x*y        ",
        " 15,                      !- Minimum Value of x      ",
        " 65,                      !- Maximum Value of x      ",
        " -30,                     !- Minimum Value of y      ",
        " 15,                      !- Maximum Value of y      ",
        " ,                        !- Minimum Curve Output    ",
        " ,                        !- Maximum Curve Output    ",
        " Temperature,             !- Input Unit Type for X   ",
        " Temperature,             !- Input Unit Type for Y   ",
        " Dimensionless;           !- Output Unit Type        ",
        "                                                     ",
        "Curve:Biquadratic,                                   ",
        " Spd2Cooling,             !- Name                    ",
        " 1.32E+00 ,               !- Coefficient1 Constant   ",
        " -6.20E-03,               !- Coefficient2 x          ",
        " -7.10E-05,               !- Coefficient3 x**2       ",
        " 4.89E-02 ,               !- Coefficient4 y          ",
        " 4.59E-04 ,               !- Coefficient5 y**2       ",
        " -3.67E-04,               !- Coefficient6 x*y        ",
        " 15,                      !- Minimum Value of x      ",
        " 65,                      !- Maximum Value of x      ",
        " -30,                     !- Minimum Value of y      ",
        " 15,                      !- Maximum Value of y      ",
        " ,                        !- Minimum Curve Output    ",
        " ,                        !- Maximum Curve Output    ",
        " Temperature,             !- Input Unit Type for X   ",
        " Temperature,             !- Input Unit Type for Y   ",
        " Dimensionless;           !- Output Unit Type        ",
        "                                                     ",
        "Curve:Biquadratic,                                   ",
        " Spd2Power,               !- Name                    ",
        " 6.56E-01 ,               !- Coefficient1 Constant   ",
        " -3.71E-03,               !- Coefficient2 x          ",
        " 2.07E-04 ,               !- Coefficient3 x**2       ",
        " 1.05E-02 ,               !- Coefficient4 y          ",
        " 7.36E-05 ,               !- Coefficient5 y**2       ",
        " -1.57E-04,               !- Coefficient6 x*y        ",
        " 15,                      !- Minimum Value of x      ",
        " 65,                      !- Maximum Value of x      ",
        " -30,                     !- Minimum Value of y      ",
        " 15,                      !- Maximum Value of y      ",
        " ,                        !- Minimum Curve Output    ",
        " ,                        !- Maximum Curve Output    ",
        " Temperature,             !- Input Unit Type for X   ",
        " Temperature,             !- Input Unit Type for Y   ",
        " Dimensionless;           !- Output Unit Type        ",

        "ZoneTerminalUnitList,",
        "  VRF Heat Pump TU List,    !- Zone Terminal Unit List Name",
        "  TU1;                      !- Zone Terminal Unit Name 1",

        "ZoneHVAC:TerminalUnit:VariableRefrigerantFlow,",
        "  TU1,                      !- Zone Terminal Unit Name",
        "  ,                         !- Terminal Unit Availability Schedule",
        "  TU1 Inlet Node,           !- Terminal Unit Air Inlet Node Name",
        "  TU1 Outlet Node,          !- Terminal Unit Air Outlet Node Name",
        "  autosize,                 !- Supply Air Flow Rate During Cooling Operation {m3/s}",
        "  0,                        !- Supply Air Flow Rate When No Cooling is Needed {m3/s}",
        "  autosize,                 !- Supply Air Flow Rate During Heating Operation {m3/s}",
        "  0,                        !- Supply Air Flow Rate When No Heating is Needed {m3/s}",
        "  autosize,                 !- Outdoor Air Flow Rate During Cooling Operation {m3/s}",
        "  autosize,                 !- Outdoor Air Flow Rate During Heating Operation {m3/s}",
        "  0,                        !- Outdoor Air Flow Rate When No Cooling or Heating is Needed {m3/s}",
        "  VRFFanSchedule,           !- Supply Air Fan Operating Mode Schedule Name",
        "  drawthrough,              !- Supply Air Fan Placement",
        "  Fan:SystemModel,          !- Supply Air Fan Object Type",
        "  TU1 VRF Supply Fan,       !- Supply Air Fan Object Name",
        "  ,                         !- Outside Air Mixer Object Type",
        "  ,                         !- Outside Air Mixer Object Name",
        "  COIL:Cooling:DX:VariableRefrigerantFlow:FluidTemperatureControl,  !- Cooling Coil Object Type",
        "  TU1 VRF DX Cooling Coil,  !- Cooling Coil Object Name",
        "  COIL:Heating:DX:VariableRefrigerantFlow:FluidTemperatureControl,  !- Heating Coil Object Type",
        "  TU1 VRF DX Heating Coil,  !- Heating Coil Object Name",
        "  30,                       !- Zone Terminal Unit On Parasitic Electric Energy Use {W}",
        "  20;                       !- Zone Terminal Unit Off Parasitic Electric Energy Use{ W }",

        "Schedule:Compact,",
        "  VRFFanSchedule,          !- Name",
        "  Any Number,              !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 7:00,1.0,         !- Field 3",
        "  Until: 18:00,1.0,        !- Field 5",
        "  Until: 24:00,1.0;        !- Field 7",

        "ScheduleTypeLimits,",
        "  Any Number;              !- Name",

        " Coil:Cooling:DX:VariableRefrigerantFlow:FluidTemperatureControl,  ",
        " 	 TU1 VRF DX Cooling Coil, !- Name							   ",
        " 	 ,                        !- Availability Schedule Name		   ",
        " 	 TU1 Inlet Node,          !- Coil Air Inlet Node		   ",
        " 	 TU1 VRF DX CCoil Outlet Node, !- Coil Air Outlet Node		   ",
        " 	 2200,                    !- Rated Total Cooling Capacity {W}   ",
        " 	 0.865,                   !- Rated Sensible Heat Ratio		   ",
        " 	 3,                       !- Indoor Unit Reference Superheating ",
        " 	 IUEvapTempCurve,         !- Indoor Unit Evaporating Temperature",
        " 	 ;                        !- Name of Water Storage Tank for Cond",

        " Curve:Quadratic,												   ",
        "     IUEvapTempCurve,         !- Name							   ",
        "     0,                       !- Coefficient1 Const				   ",
        "     0.80404,                 !- Coefficient2 x					   ",
        "     0,                       !- Coefficient3 x**2				   ",
        "     0,                       !- Minimum Value of x				   ",
        "     15,                      !- Maximum Value of x				   ",
        "     ,                        !- Minimum Curve Outp				   ",
        "     ,                        !- Maximum Curve Outp				   ",
        "     Dimensionless,           !- Input Unit Type fo				   ",
        "     Dimensionless;           !- Output Unit Type				   ",

        "  Coil:Heating:DX:VariableRefrigerantFlow:FluidTemperatureControl,",
        "    TU1 VRF DX Heating Coil, !- Name",
        "    ,                        !- Availability Schedule",
        "    TU1 VRF DX CCoil Outlet Node,  !- Coil Air Inlet Node",
        "    TU1 VRF DX HCoil Outlet Node,  !- Coil Air Outlet Node",
        "    6500.0,                  !- Rated Total Heating Capacity {W}",
        "    5,                       !- Indoor Unit Reference Subcooling Degrees Setpoint {C}    ",
        "    IUCondTempCurve;         !- Indoor Unit Condensing Temperature Function of Subcooling Curve Name",

        "  Curve:Quadratic,",
        "    IUCondTempCurve,         !- Name",
        "    -1.85,                   !- Coefficient1 Constant",
        "    0.411,                   !- Coefficient2 x",
        "    0.0196,                  !- Coefficient3 x**2",
        "    0,                       !- Minimum Value of x    ",
        "    20,                      !- Maximum Value of x    ",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature;             !- Output Unit Type",

        "  Fan:SystemModel,",
        "    TU1 VRF Supply Fan,          !- Name",
        "    ,                            !- Availability Schedule Name",
        "    TU1 VRF DX HCoil Outlet Node,  !- Air Inlet Node Name",
        "    TU1 Outlet Node,             !- Air Outlet Node Name",
        "    1.0 ,                        !- Design Maximum Air Flow Rate",
        "    Discrete ,                   !- Speed Control Method",
        "    0.0,                         !- Electric Power Minimum Flow Rate Fraction",
        "    100.0,                       !- Design Pressure Rise",
        "    0.9 ,                        !- Motor Efficiency",
        "    1.0 ,                        !- Motor In Air Stream Fraction",
        "    AUTOSIZE,                    !- Design Electric Power Consumption",
        "    TotalEfficiencyAndPressure,  !- Design Power Sizing Method",
        "    ,                            !- Electric Power Per Unit Flow Rate",
        "    ,                            !- Electric Power Per Unit Flow Rate Per Unit Pressure",
        "    0.50;                        !- Fan Total Efficiency",

        " !-   ===========  ALL OBJECTS IN CLASS: FLUIDPROPERTIES:NAME ===========            ",
        "                                                                                     ",
        "   FluidProperties:Name,                                                             ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Refrigerant;             !- Fluid Type                                          ",
        "                                                                                     ",
        " !-   ===========  ALL OBJECTS IN CLASS: FLUIDPROPERTIES:TEMPERATURES ===========    ",
        "                                                                                     ",
        "   FluidProperties:Temperatures,                                                     ",
        "     R410aSaturatedTemperatures,  !- Name                                            ",
        "     -72.000,-69.000,-66.000,-63.000,-60.000,-57.000,-54.000,                        ",
        "     -51.000,-48.000,-45.000,-42.000,-39.000,-36.000,-33.000,                        ",
        "     -30.000,-27.000,-24.000,-21.000,-18.000,-15.000,-12.000,                        ",
        "     -9.000,-6.000,-3.000,0.000,3.000,6.000,9.000,                                   ",
        "     12.000,15.000,18.000,21.000,24.000,27.000,30.000,                               ",
        "     33.000,36.000,39.000,42.000,45.000,48.000,51.000,                               ",
        "     54.000,57.000,60.000,63.000,66.000,69.000;                                      ",
        "                                                                                     ",
        "   FluidProperties:Temperatures,                                                     ",
        "     R410aSuperHeatTemperatures,  !- Name                                            ",
        "     -72.000,-66.000,-60.000,-54.000,-48.000,-45.000,-42.000,                        ",
        "     -39.000,-36.000,-33.000,-30.000,-27.000,-24.000,-21.000,                        ",
        "     -18.000,-15.000,-12.000,-9.000,-6.000,-3.000,0.000,                             ",
        "     3.000,6.000,9.000,12.000,15.000,18.000,21.000,                                  ",
        "     24.000,27.000,30.000,33.000,36.000,39.000,42.000,                               ",
        "     45.000,48.000,51.000,54.000,57.000,60.000,63.000,                               ",
        "     66.000,69.000;                                                                  ",
        "                                                                                     ",
        " !-   ===========  ALL OBJECTS IN CLASS: FLUIDPROPERTIES:SATURATED ===========       ",
        "                                                                                     ",
        "   FluidProperties:Saturated,                                                        ",
        "     R410a,                   !- Name                                                ",
        "     Pressure,                !- Fluid Property Type                                 ",
        "     FluidGas,                !- Fluid Phase                                         ",
        "     R410aSaturatedTemperatures,  !- Temperature Values Name                         ",
        "     3.1238E+04,3.7717E+04,4.5248E+04,5.3954E+04,6.3963E+04,7.5412E+04,8.8445E+04,   ",
        "     1.0321E+05,1.1988E+05,1.3860E+05,1.5955E+05,1.8292E+05,2.0888E+05,2.3762E+05,   ",
        "     2.6935E+05,3.0426E+05,3.4257E+05,3.8449E+05,4.3024E+05,4.8004E+05,5.3412E+05,   ",
        "     5.9273E+05,6.5609E+05,7.2446E+05,7.9808E+05,8.7722E+05,9.6214E+05,1.0531E+06,   ",
        "     1.1504E+06,1.2543E+06,1.3651E+06,1.4831E+06,1.6086E+06,1.7419E+06,1.8834E+06,   ",
        "     2.0334E+06,2.1923E+06,2.3604E+06,2.5382E+06,2.7261E+06,2.9246E+06,3.1341E+06,   ",
        "     3.3552E+06,3.5886E+06,3.8348E+06,4.0949E+06,4.3697E+06,4.6607E+06;              ",
        "                                                                                     ",
        "   FluidProperties:Saturated,                                                        ",
        "     R410a,                   !- Name                                                ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     Fluid,                   !- Fluid Phase                                         ",
        "     R410aSaturatedTemperatures,  !- Temperature Values Name                         ",
        "     9.8535E+04,1.0259E+05,1.0665E+05,1.1072E+05,1.1479E+05,1.1888E+05,1.2297E+05,   ",
        "     1.2707E+05,1.3119E+05,1.3532E+05,1.3947E+05,1.4363E+05,1.4782E+05,1.5202E+05,   ",
        "     1.5624E+05,1.6048E+05,1.6475E+05,1.6904E+05,1.7337E+05,1.7772E+05,1.8210E+05,   ",
        "     1.8652E+05,1.9097E+05,1.9547E+05,2.0000E+05,2.0458E+05,2.0920E+05,2.1388E+05,   ",
        "     2.1861E+05,2.2340E+05,2.2825E+05,2.3316E+05,2.3815E+05,2.4322E+05,2.4838E+05,   ",
        "     2.5363E+05,2.5899E+05,2.6447E+05,2.7008E+05,2.7585E+05,2.8180E+05,2.8797E+05,   ",
        "     2.9441E+05,3.0120E+05,3.0848E+05,3.1650E+05,3.2578E+05,3.3815E+05;              ",
        "                                                                                     ",
        "   FluidProperties:Saturated,                                                        ",
        "     R410a,                   !- Name                                                ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     FluidGas,                !- Fluid Phase                                         ",
        "     R410aSaturatedTemperatures,  !- Temperature Values Name                         ",
        "     3.8813E+05,3.8981E+05,3.9148E+05,3.9313E+05,3.9476E+05,3.9637E+05,3.9796E+05,   ",
        "     3.9953E+05,4.0108E+05,4.0260E+05,4.0410E+05,4.0557E+05,4.0701E+05,4.0842E+05,   ",
        "     4.0980E+05,4.1114E+05,4.1245E+05,4.1373E+05,4.1496E+05,4.1615E+05,4.1730E+05,   ",
        "     4.1840E+05,4.1945E+05,4.2045E+05,4.2139E+05,4.2227E+05,4.2308E+05,4.2382E+05,   ",
        "     4.2448E+05,4.2507E+05,4.2556E+05,4.2595E+05,4.2624E+05,4.2641E+05,4.2646E+05,   ",
        "     4.2635E+05,4.2609E+05,4.2564E+05,4.2498E+05,4.2408E+05,4.2290E+05,4.2137E+05,   ",
        "     4.1941E+05,4.1692E+05,4.1370E+05,4.0942E+05,4.0343E+05,3.9373E+05;              ",
        "                                                                                     ",
        "   FluidProperties:Saturated,                                                        ",
        "     R410a,                   !- Name                                                ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     Fluid,                   !- Fluid Phase                                         ",
        "     R410aSaturatedTemperatures,  !- Temperature Values Name                         ",
        "     1.4127E+03,1.4036E+03,1.3946E+03,1.3854E+03,1.3762E+03,1.3669E+03,1.3576E+03,   ",
        "     1.3482E+03,1.3387E+03,1.3291E+03,1.3194E+03,1.3097E+03,1.2998E+03,1.2898E+03,   ",
        "     1.2797E+03,1.2694E+03,1.2591E+03,1.2486E+03,1.2379E+03,1.2271E+03,1.2160E+03,   ",
        "     1.2048E+03,1.1934E+03,1.1818E+03,1.1699E+03,1.1578E+03,1.1454E+03,1.1328E+03,   ",
        "     1.1197E+03,1.1064E+03,1.0927E+03,1.0785E+03,1.0639E+03,1.0488E+03,1.0331E+03,   ",
        "     1.0167E+03,9.9971E+02,9.8187E+02,9.6308E+02,9.4319E+02,9.2198E+02,8.9916E+02,   ",
        "     8.7429E+02,8.4672E+02,8.1537E+02,7.7825E+02,7.3095E+02,6.5903E+02;              ",
        "                                                                                     ",
        "   FluidProperties:Saturated,                                                        ",
        "     R410a,                   !- Name                                                ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     FluidGas,                !- Fluid Phase                                         ",
        "     R410aSaturatedTemperatures,  !- Temperature Values Name                         ",
        "     1.3845E+00,1.6517E+00,1.9588E+00,2.3100E+00,2.7097E+00,3.1627E+00,3.6737E+00,   ",
        "     4.2482E+00,4.8916E+00,5.6098E+00,6.4088E+00,7.2952E+00,8.2758E+00,9.3578E+00,   ",
        "     1.0549E+01,1.1857E+01,1.3292E+01,1.4861E+01,1.6576E+01,1.8447E+01,2.0485E+01,   ",
        "     2.2702E+01,2.5113E+01,2.7732E+01,3.0575E+01,3.3659E+01,3.7005E+01,4.0634E+01,   ",
        "     4.4571E+01,4.8844E+01,5.3483E+01,5.8525E+01,6.4012E+01,6.9991E+01,7.6520E+01,   ",
        "     8.3666E+01,9.1511E+01,1.0016E+02,1.0973E+02,1.2038E+02,1.3233E+02,1.4585E+02,   ",
        "     1.6135E+02,1.7940E+02,2.0095E+02,2.2766E+02,2.6301E+02,3.1759E+02;              ",
        "                                                                                     ",
        "   FluidProperties:Saturated,                                                        ",
        "     R410a,                   !- Name                                                ",
        "     SpecificHeat,            !- Fluid Property Type                                 ",
        "     Fluid,                   !- Fluid Phase                                         ",
        "     R410aSaturatedTemperatures,  !- Temperature Values Name                         ",
        "     1.3499E+03,1.3515E+03,1.3534E+03,1.3557E+03,1.3584E+03,1.3614E+03,1.3648E+03,   ",
        "     1.3686E+03,1.3728E+03,1.3774E+03,1.3825E+03,1.3881E+03,1.3941E+03,1.4007E+03,   ",
        "     1.4078E+03,1.4155E+03,1.4238E+03,1.4327E+03,1.4424E+03,1.4527E+03,1.4639E+03,   ",
        "     1.4759E+03,1.4888E+03,1.5027E+03,1.5177E+03,1.5340E+03,1.5515E+03,1.5706E+03,   ",
        "     1.5914E+03,1.6141E+03,1.6390E+03,1.6664E+03,1.6968E+03,1.7307E+03,1.7689E+03,   ",
        "     1.8123E+03,1.8622E+03,1.9204E+03,1.9895E+03,2.0732E+03,2.1774E+03,2.3116E+03,   ",
        "     2.4924E+03,2.7507E+03,3.1534E+03,3.8723E+03,5.5190E+03,1.2701E+04;              ",
        "                                                                                     ",
        "   FluidProperties:Saturated,                                                        ",
        "     R410a,                   !- Name                                                ",
        "     SpecificHeat,            !- Fluid Property Type                                 ",
        "     FluidGas,                !- Fluid Phase                                         ",
        "     R410aSaturatedTemperatures,  !- Temperature Values Name                         ",
        "     7.2387E+02,7.3519E+02,7.4693E+02,7.5910E+02,7.7167E+02,7.8465E+02,7.9802E+02,   ",
        "     8.1178E+02,8.2594E+02,8.4050E+02,8.5546E+02,8.7085E+02,8.8668E+02,9.0298E+02,   ",
        "     9.1979E+02,9.3715E+02,9.5511E+02,9.7372E+02,9.9307E+02,1.0132E+03,1.0343E+03,   ",
        "     1.0564E+03,1.0796E+03,1.1042E+03,1.1302E+03,1.1580E+03,1.1877E+03,1.2196E+03,   ",
        "     1.2541E+03,1.2917E+03,1.3329E+03,1.3783E+03,1.4287E+03,1.4853E+03,1.5494E+03,   ",
        "     1.6228E+03,1.7078E+03,1.8078E+03,1.9274E+03,2.0735E+03,2.2562E+03,2.4922E+03,   ",
        "     2.8094E+03,3.2596E+03,3.9504E+03,5.1465E+03,7.7185E+03,1.7076E+04;              ",
        "                                                                                     ",
        " !-   ===========  ALL OBJECTS IN CLASS: FLUIDPROPERTIES:SUPERHEATED ===========     ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     3.1238E+04,              !- Pressure {Pa}                                       ",
        "     3.8813E+05,3.9245E+05,3.9675E+05,4.0105E+05,4.0536E+05,4.0753E+05,4.0970E+05,   ",
        "     4.1189E+05,4.1408E+05,4.1628E+05,4.1849E+05,4.2071E+05,4.2294E+05,4.2518E+05,   ",
        "     4.2743E+05,4.2969E+05,4.3196E+05,4.3425E+05,4.3655E+05,4.3885E+05,4.4118E+05,   ",
        "     4.4351E+05,4.4586E+05,4.4821E+05,4.5058E+05,4.5297E+05,4.5536E+05,4.5777E+05,   ",
        "     4.6020E+05,4.6263E+05,4.6508E+05,4.6754E+05,4.7002E+05,4.7251E+05,4.7501E+05,   ",
        "     4.7752E+05,4.8005E+05,4.8259E+05,4.8515E+05,4.8772E+05,4.9030E+05,4.9290E+05,   ",
        "     4.9551E+05,4.9813E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     3.1238E+04,              !- Pressure {Pa}                                       ",
        "     1.3845E+00,1.3404E+00,1.2997E+00,1.2617E+00,1.2262E+00,1.2092E+00,1.1928E+00,   ",
        "     1.1768E+00,1.1613E+00,1.1462E+00,1.1316E+00,1.1173E+00,1.1034E+00,1.0898E+00,   ",
        "     1.0766E+00,1.0638E+00,1.0512E+00,1.0390E+00,1.0271E+00,1.0154E+00,1.0040E+00,   ",
        "     9.9285E-01,9.8197E-01,9.7133E-01,9.6093E-01,9.5075E-01,9.4079E-01,9.3104E-01,   ",
        "     9.2150E-01,9.1215E-01,9.0299E-01,8.9403E-01,8.8524E-01,8.7662E-01,8.6817E-01,   ",
        "     8.5989E-01,8.5177E-01,8.4380E-01,8.3598E-01,8.2831E-01,8.2077E-01,8.1338E-01,   ",
        "     8.0612E-01,7.9899E-01;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     4.5248E+04,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,3.9148E+05,3.9593E+05,4.0034E+05,4.0474E+05,4.0694E+05,4.0915E+05,   ",
        "     4.1136E+05,4.1358E+05,4.1580E+05,4.1803E+05,4.2027E+05,4.2252E+05,4.2478E+05,   ",
        "     4.2705E+05,4.2933E+05,4.3161E+05,4.3391E+05,4.3622E+05,4.3854E+05,4.4088E+05,   ",
        "     4.4322E+05,4.4558E+05,4.4794E+05,4.5032E+05,4.5272E+05,4.5512E+05,4.5754E+05,   ",
        "     4.5997E+05,4.6241E+05,4.6486E+05,4.6733E+05,4.6981E+05,4.7231E+05,4.7481E+05,   ",
        "     4.7733E+05,4.7987E+05,4.8241E+05,4.8497E+05,4.8755E+05,4.9013E+05,4.9273E+05,   ",
        "     4.9535E+05,4.9797E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     4.5248E+04,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,1.9588E+00,1.8968E+00,1.8395E+00,1.7863E+00,1.7610E+00,1.7365E+00,   ",
        "     1.7128E+00,1.6898E+00,1.6674E+00,1.6457E+00,1.6246E+00,1.6041E+00,1.5842E+00,   ",
        "     1.5647E+00,1.5458E+00,1.5273E+00,1.5093E+00,1.4918E+00,1.4747E+00,1.4580E+00,   ",
        "     1.4416E+00,1.4257E+00,1.4101E+00,1.3949E+00,1.3800E+00,1.3654E+00,1.3512E+00,   ",
        "     1.3372E+00,1.3236E+00,1.3102E+00,1.2971E+00,1.2843E+00,1.2717E+00,1.2594E+00,   ",
        "     1.2473E+00,1.2355E+00,1.2239E+00,1.2125E+00,1.2013E+00,1.1903E+00,1.1796E+00,   ",
        "     1.1690E+00,1.1586E+00;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     6.3963E+04,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,3.9476E+05,3.9935E+05,4.0388E+05,4.0614E+05,4.0839E+05,   ",
        "     4.1064E+05,4.1290E+05,4.1516E+05,4.1742E+05,4.1969E+05,4.2196E+05,4.2425E+05,   ",
        "     4.2654E+05,4.2884E+05,4.3114E+05,4.3346E+05,4.3579E+05,4.3813E+05,4.4047E+05,   ",
        "     4.4283E+05,4.4520E+05,4.4758E+05,4.4997E+05,4.5238E+05,4.5479E+05,4.5722E+05,   ",
        "     4.5966E+05,4.6211E+05,4.6457E+05,4.6705E+05,4.6954E+05,4.7204E+05,4.7455E+05,   ",
        "     4.7708E+05,4.7962E+05,4.8217E+05,4.8474E+05,4.8732E+05,4.8991E+05,4.9252E+05,   ",
        "     4.9513E+05,4.9777E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     6.3963E+04,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,2.7097E+00,2.6240E+00,2.5451E+00,2.5078E+00,2.4718E+00,   ",
        "     2.4370E+00,2.4034E+00,2.3708E+00,2.3393E+00,2.3086E+00,2.2789E+00,2.2500E+00,   ",
        "     2.2219E+00,2.1945E+00,2.1679E+00,2.1420E+00,2.1167E+00,2.0921E+00,2.0681E+00,   ",
        "     2.0446E+00,2.0217E+00,1.9994E+00,1.9776E+00,1.9562E+00,1.9354E+00,1.9150E+00,   ",
        "     1.8950E+00,1.8755E+00,1.8564E+00,1.8377E+00,1.8194E+00,1.8014E+00,1.7839E+00,   ",
        "     1.7666E+00,1.7497E+00,1.7332E+00,1.7169E+00,1.7010E+00,1.6854E+00,1.6700E+00,   ",
        "     1.6550E+00,1.6402E+00;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     8.8445E+04,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,3.9796E+05,4.0270E+05,4.0503E+05,4.0736E+05,   ",
        "     4.0967E+05,4.1198E+05,4.1429E+05,4.1660E+05,4.1891E+05,4.2122E+05,4.2354E+05,   ",
        "     4.2586E+05,4.2819E+05,4.3052E+05,4.3286E+05,4.3521E+05,4.3757E+05,4.3994E+05,   ",
        "     4.4232E+05,4.4470E+05,4.4710E+05,4.4951E+05,4.5193E+05,4.5436E+05,4.5680E+05,   ",
        "     4.5925E+05,4.6171E+05,4.6419E+05,4.6668E+05,4.6918E+05,4.7169E+05,4.7421E+05,   ",
        "     4.7675E+05,4.7930E+05,4.8186E+05,4.8443E+05,4.8702E+05,4.8962E+05,4.9223E+05,   ",
        "     4.9486E+05,4.9749E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     8.8445E+04,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,3.6737E+00,3.5570E+00,3.5024E+00,3.4500E+00,   ",
        "     3.3995E+00,3.3509E+00,3.3039E+00,3.2585E+00,3.2146E+00,3.1720E+00,3.1308E+00,   ",
        "     3.0907E+00,3.0518E+00,3.0140E+00,2.9772E+00,2.9414E+00,2.9065E+00,2.8726E+00,   ",
        "     2.8395E+00,2.8072E+00,2.7757E+00,2.7449E+00,2.7149E+00,2.6856E+00,2.6569E+00,   ",
        "     2.6289E+00,2.6015E+00,2.5747E+00,2.5485E+00,2.5228E+00,2.4977E+00,2.4731E+00,   ",
        "     2.4490E+00,2.4254E+00,2.4022E+00,2.3795E+00,2.3573E+00,2.3354E+00,2.3140E+00,   ",
        "     2.2930E+00,2.2724E+00;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     1.1988E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,4.0108E+05,4.0354E+05,4.0597E+05,   ",
        "     4.0838E+05,4.1077E+05,4.1315E+05,4.1552E+05,4.1788E+05,4.2025E+05,4.2261E+05,   ",
        "     4.2497E+05,4.2734E+05,4.2971E+05,4.3209E+05,4.3447E+05,4.3685E+05,4.3925E+05,   ",
        "     4.4165E+05,4.4406E+05,4.4648E+05,4.4891E+05,4.5135E+05,4.5380E+05,4.5626E+05,   ",
        "     4.5873E+05,4.6121E+05,4.6370E+05,4.6620E+05,4.6871E+05,4.7124E+05,4.7377E+05,   ",
        "     4.7632E+05,4.7888E+05,4.8145E+05,4.8404E+05,4.8663E+05,4.8924E+05,4.9186E+05,   ",
        "     4.9450E+05,4.9715E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     1.1988E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,4.8918E+00,4.8116E+00,4.7352E+00,   ",
        "     4.6621E+00,4.5920E+00,4.5247E+00,4.4599E+00,4.3974E+00,4.3370E+00,4.2787E+00,   ",
        "     4.2221E+00,4.1674E+00,4.1143E+00,4.0627E+00,4.0126E+00,3.9639E+00,3.9165E+00,   ",
        "     3.8704E+00,3.8255E+00,3.7817E+00,3.7390E+00,3.6974E+00,3.6567E+00,3.6171E+00,   ",
        "     3.5783E+00,3.5405E+00,3.5035E+00,3.4673E+00,3.4319E+00,3.3973E+00,3.3634E+00,   ",
        "     3.3302E+00,3.2977E+00,3.2659E+00,3.2347E+00,3.2041E+00,3.1742E+00,3.1448E+00,   ",
        "     3.1160E+00,3.0877E+00;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     1.3860E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,4.0260E+05,4.0510E+05,   ",
        "     4.0757E+05,4.1002E+05,4.1244E+05,4.1485E+05,4.1726E+05,4.1965E+05,4.2204E+05,   ",
        "     4.2444E+05,4.2683E+05,4.2922E+05,4.3162E+05,4.3402E+05,4.3642E+05,4.3883E+05,   ",
        "     4.4125E+05,4.4368E+05,4.4611E+05,4.4855E+05,4.5100E+05,4.5346E+05,4.5593E+05,   ",
        "     4.5841E+05,4.6090E+05,4.6340E+05,4.6591E+05,4.6843E+05,4.7097E+05,4.7351E+05,   ",
        "     4.7606E+05,4.7863E+05,4.8121E+05,4.8380E+05,4.8640E+05,4.8902E+05,4.9165E+05,   ",
        "     4.9428E+05,4.9694E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     1.3860E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,5.6098E+00,5.5173E+00,   ",
        "     5.4293E+00,5.3451E+00,5.2645E+00,5.1871E+00,5.1127E+00,5.0409E+00,4.9717E+00,   ",
        "     4.9047E+00,4.8399E+00,4.7772E+00,4.7163E+00,4.6573E+00,4.5999E+00,4.5442E+00,   ",
        "     4.4900E+00,4.4372E+00,4.3859E+00,4.3358E+00,4.2870E+00,4.2394E+00,4.1930E+00,   ",
        "     4.1476E+00,4.1033E+00,4.0601E+00,4.0178E+00,3.9765E+00,3.9360E+00,3.8965E+00,   ",
        "     3.8578E+00,3.8199E+00,3.7828E+00,3.7464E+00,3.7108E+00,3.6759E+00,3.6417E+00,   ",
        "     3.6081E+00,3.5752E+00;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     1.5955E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,4.0410E+05,   ",
        "     4.0664E+05,4.0915E+05,4.1163E+05,4.1409E+05,4.1654E+05,4.1898E+05,4.2140E+05,   ",
        "     4.2383E+05,4.2625E+05,4.2867E+05,4.3109E+05,4.3351E+05,4.3593E+05,4.3836E+05,   ",
        "     4.4080E+05,4.4324E+05,4.4569E+05,4.4815E+05,4.5061E+05,4.5309E+05,4.5557E+05,   ",
        "     4.5806E+05,4.6056E+05,4.6307E+05,4.6559E+05,4.6812E+05,4.7066E+05,4.7321E+05,   ",
        "     4.7578E+05,4.7835E+05,4.8094E+05,4.8354E+05,4.8615E+05,4.8877E+05,4.9140E+05,   ",
        "     4.9404E+05,4.9670E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     1.5955E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,6.4087E+00,   ",
        "     6.3023E+00,6.2010E+00,6.1045E+00,6.0120E+00,5.9233E+00,5.8380E+00,5.7559E+00,   ",
        "     5.6767E+00,5.6001E+00,5.5261E+00,5.4544E+00,5.3850E+00,5.3176E+00,5.2521E+00,   ",
        "     5.1885E+00,5.1267E+00,5.0666E+00,5.0080E+00,4.9509E+00,4.8953E+00,4.8411E+00,   ",
        "     4.7882E+00,4.7366E+00,4.6862E+00,4.6369E+00,4.5888E+00,4.5417E+00,4.4957E+00,   ",
        "     4.4507E+00,4.4066E+00,4.3635E+00,4.3212E+00,4.2799E+00,4.2393E+00,4.1996E+00,   ",
        "     4.1607E+00,4.1225E+00;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     1.8292E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     4.0557E+05,4.0816E+05,4.1071E+05,4.1323E+05,4.1573E+05,4.1821E+05,4.2068E+05,   ",
        "     4.2313E+05,4.2559E+05,4.2804E+05,4.3049E+05,4.3293E+05,4.3538E+05,4.3784E+05,   ",
        "     4.4029E+05,4.4275E+05,4.4522E+05,4.4769E+05,4.5017E+05,4.5266E+05,4.5516E+05,   ",
        "     4.5766E+05,4.6017E+05,4.6270E+05,4.6523E+05,4.6777E+05,4.7032E+05,4.7288E+05,   ",
        "     4.7546E+05,4.7804E+05,4.8063E+05,4.8324E+05,4.8586E+05,4.8848E+05,4.9112E+05,   ",
        "     4.9378E+05,4.9644E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     1.8292E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     7.2953E+00,7.1732E+00,7.0571E+00,6.9465E+00,6.8408E+00,6.7394E+00,6.6420E+00,   ",
        "     6.5482E+00,6.4578E+00,6.3706E+00,6.2862E+00,6.2046E+00,6.1255E+00,6.0488E+00,   ",
        "     5.9743E+00,5.9020E+00,5.8317E+00,5.7633E+00,5.6968E+00,5.6320E+00,5.5688E+00,   ",
        "     5.5072E+00,5.4472E+00,5.3885E+00,5.3313E+00,5.2754E+00,5.2208E+00,5.1674E+00,   ",
        "     5.1152E+00,5.0641E+00,5.0141E+00,4.9652E+00,4.9173E+00,4.8703E+00,4.8244E+00,   ",
        "     4.7793E+00,4.7352E+00;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     2.0888E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,4.0701E+05,4.0964E+05,4.1224E+05,4.1480E+05,4.1733E+05,4.1985E+05,   ",
        "     4.2235E+05,4.2485E+05,4.2733E+05,4.2981E+05,4.3229E+05,4.3477E+05,4.3724E+05,   ",
        "     4.3972E+05,4.4221E+05,4.4469E+05,4.4719E+05,4.4968E+05,4.5219E+05,4.5470E+05,   ",
        "     4.5722E+05,4.5974E+05,4.6228E+05,4.6482E+05,4.6738E+05,4.6994E+05,4.7251E+05,   ",
        "     4.7510E+05,4.7769E+05,4.8029E+05,4.8291E+05,4.8553E+05,4.8817E+05,4.9082E+05,   ",
        "     4.9348E+05,4.9615E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     2.0888E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,8.2759E+00,8.1361E+00,8.0034E+00,7.8770E+00,7.7563E+00,7.6407E+00,   ",
        "     7.5297E+00,7.4230E+00,7.3201E+00,7.2209E+00,7.1251E+00,7.0323E+00,6.9425E+00,   ",
        "     6.8555E+00,6.7710E+00,6.6890E+00,6.6093E+00,6.5318E+00,6.4564E+00,6.3830E+00,   ",
        "     6.3115E+00,6.2417E+00,6.1738E+00,6.1074E+00,6.0426E+00,5.9794E+00,5.9176E+00,   ",
        "     5.8572E+00,5.7981E+00,5.7404E+00,5.6839E+00,5.6286E+00,5.5744E+00,5.5214E+00,   ",
        "     5.4694E+00,5.4185E+00;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     2.3762E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,4.0842E+05,4.1110E+05,4.1374E+05,4.1634E+05,4.1892E+05,   ",
        "     4.2147E+05,4.2401E+05,4.2654E+05,4.2905E+05,4.3157E+05,4.3407E+05,4.3658E+05,   ",
        "     4.3909E+05,4.4159E+05,4.4410E+05,4.4662E+05,4.4914E+05,4.5166E+05,4.5419E+05,   ",
        "     4.5672E+05,4.5927E+05,4.6182E+05,4.6437E+05,4.6694E+05,4.6952E+05,4.7210E+05,   ",
        "     4.7470E+05,4.7730E+05,4.7992E+05,4.8254E+05,4.8517E+05,4.8782E+05,4.9048E+05,   ",
        "     4.9315E+05,4.9582E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     2.3762E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,9.3578E+00,9.1979E+00,9.0465E+00,8.9024E+00,8.7650E+00,   ",
        "     8.6335E+00,8.5073E+00,8.3861E+00,8.2694E+00,8.1569E+00,8.0482E+00,7.9431E+00,   ",
        "     7.8414E+00,7.7429E+00,7.6473E+00,7.5546E+00,7.4645E+00,7.3769E+00,7.2917E+00,   ",
        "     7.2088E+00,7.1280E+00,7.0493E+00,6.9726E+00,6.8977E+00,6.8246E+00,6.7533E+00,   ",
        "     6.6836E+00,6.6155E+00,6.5489E+00,6.4838E+00,6.4200E+00,6.3577E+00,6.2967E+00,   ",
        "     6.2369E+00,6.1783E+00;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     2.6935E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,4.0980E+05,4.1253E+05,4.1521E+05,4.1786E+05,   ",
        "     4.2047E+05,4.2307E+05,4.2564E+05,4.2820E+05,4.3075E+05,4.3330E+05,4.3584E+05,   ",
        "     4.3837E+05,4.4091E+05,4.4345E+05,4.4599E+05,4.4853E+05,4.5107E+05,4.5362E+05,   ",
        "     4.5617E+05,4.5873E+05,4.6130E+05,4.6388E+05,4.6646E+05,4.6905E+05,4.7165E+05,   ",
        "     4.7425E+05,4.7687E+05,4.7950E+05,4.8213E+05,4.8478E+05,4.8743E+05,4.9010E+05,   ",
        "     4.9278E+05,4.9546E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     2.6935E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,1.0549E+01,1.0367E+01,1.0194E+01,1.0030E+01,   ",
        "     9.8741E+00,9.7248E+00,9.5817E+00,9.4443E+00,9.3122E+00,9.1848E+00,9.0619E+00,   ",
        "     8.9431E+00,8.8282E+00,8.7170E+00,8.6091E+00,8.5045E+00,8.4029E+00,8.3042E+00,   ",
        "     8.2081E+00,8.1147E+00,8.0237E+00,7.9351E+00,7.8487E+00,7.7644E+00,7.6822E+00,   ",
        "     7.6019E+00,7.5235E+00,7.4469E+00,7.3721E+00,7.2989E+00,7.2273E+00,7.1572E+00,   ",
        "     7.0886E+00,7.0214E+00;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     3.0426E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,4.1114E+05,4.1392E+05,4.1665E+05,   ",
        "     4.1934E+05,4.2200E+05,4.2463E+05,4.2725E+05,4.2984E+05,4.3243E+05,4.3501E+05,   ",
        "     4.3758E+05,4.4015E+05,4.4272E+05,4.4528E+05,4.4785E+05,4.5042E+05,4.5299E+05,   ",
        "     4.5556E+05,4.5814E+05,4.6073E+05,4.6332E+05,4.6592E+05,4.6853E+05,4.7114E+05,   ",
        "     4.7376E+05,4.7639E+05,4.7903E+05,4.8168E+05,4.8434E+05,4.8701E+05,4.8968E+05,   ",
        "     4.9237E+05,4.9507E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     3.0426E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,1.1857E+01,1.1650E+01,1.1453E+01,   ",
        "     1.1267E+01,1.1090E+01,1.0921E+01,1.0759E+01,1.0604E+01,1.0454E+01,1.0310E+01,   ",
        "     1.0172E+01,1.0038E+01,9.9083E+00,9.7830E+00,9.6615E+00,9.5438E+00,9.4294E+00,   ",
        "     9.3184E+00,9.2104E+00,9.1054E+00,9.0032E+00,8.9037E+00,8.8067E+00,8.7121E+00,   ",
        "     8.6198E+00,8.5297E+00,8.4418E+00,8.3559E+00,8.2719E+00,8.1898E+00,8.1095E+00,   ",
        "     8.0310E+00,7.9541E+00;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     3.4257E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,4.1245E+05,4.1529E+05,   ",
        "     4.1807E+05,4.2080E+05,4.2350E+05,4.2617E+05,4.2883E+05,4.3146E+05,4.3408E+05,   ",
        "     4.3670E+05,4.3930E+05,4.4190E+05,4.4450E+05,4.4709E+05,4.4969E+05,4.5229E+05,   ",
        "     4.5489E+05,4.5749E+05,4.6010E+05,4.6271E+05,4.6533E+05,4.6795E+05,4.7058E+05,   ",
        "     4.7322E+05,4.7587E+05,4.7852E+05,4.8118E+05,4.8385E+05,4.8653E+05,4.8922E+05,   ",
        "     4.9192E+05,4.9463E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     3.4257E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,1.3292E+01,1.3056E+01,   ",
        "     1.2833E+01,1.2622E+01,1.2421E+01,1.2230E+01,1.2047E+01,1.1871E+01,1.1703E+01,   ",
        "     1.1541E+01,1.1385E+01,1.1234E+01,1.1088E+01,1.0947E+01,1.0811E+01,1.0678E+01,   ",
        "     1.0550E+01,1.0425E+01,1.0304E+01,1.0187E+01,1.0072E+01,9.9605E+00,9.8518E+00,   ",
        "     9.7459E+00,9.6426E+00,9.5417E+00,9.4433E+00,9.3472E+00,9.2533E+00,9.1615E+00,   ",
        "     9.0717E+00,8.9839E+00;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     3.8449E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,4.1373E+05,   ",
        "     4.1661E+05,4.1944E+05,4.2222E+05,4.2497E+05,4.2768E+05,4.3038E+05,4.3305E+05,   ",
        "     4.3571E+05,4.3836E+05,4.4100E+05,4.4363E+05,4.4626E+05,4.4889E+05,4.5151E+05,   ",
        "     4.5414E+05,4.5677E+05,4.5940E+05,4.6203E+05,4.6467E+05,4.6732E+05,4.6997E+05,   ",
        "     4.7262E+05,4.7529E+05,4.7796E+05,4.8063E+05,4.8332E+05,4.8601E+05,4.8872E+05,   ",
        "     4.9143E+05,4.9415E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     3.8449E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,1.4861E+01,   ",
        "     1.4593E+01,1.4341E+01,1.4102E+01,1.3875E+01,1.3659E+01,1.3452E+01,1.3255E+01,   ",
        "     1.3065E+01,1.2882E+01,1.2707E+01,1.2537E+01,1.2374E+01,1.2216E+01,1.2063E+01,   ",
        "     1.1914E+01,1.1771E+01,1.1631E+01,1.1495E+01,1.1364E+01,1.1236E+01,1.1111E+01,   ",
        "     1.0989E+01,1.0871E+01,1.0755E+01,1.0643E+01,1.0533E+01,1.0426E+01,1.0321E+01,   ",
        "     1.0218E+01,1.0118E+01;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     4.3024E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     4.1496E+05,4.1790E+05,4.2078E+05,4.2361E+05,4.2641E+05,4.2916E+05,4.3190E+05,   ",
        "     4.3461E+05,4.3731E+05,4.3999E+05,4.4267E+05,4.4533E+05,4.4800E+05,4.5066E+05,   ",
        "     4.5331E+05,4.5597E+05,4.5863E+05,4.6129E+05,4.6395E+05,4.6662E+05,4.6929E+05,   ",
        "     4.7197E+05,4.7465E+05,4.7734E+05,4.8003E+05,4.8273E+05,4.8544E+05,4.8816E+05,   ",
        "     4.9089E+05,4.9362E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     4.3024E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     1.6576E+01,1.6272E+01,1.5986E+01,1.5716E+01,1.5460E+01,1.5216E+01,1.4983E+01,   ",
        "     1.4761E+01,1.4547E+01,1.4343E+01,1.4145E+01,1.3955E+01,1.3772E+01,1.3595E+01,   ",
        "     1.3424E+01,1.3258E+01,1.3097E+01,1.2941E+01,1.2789E+01,1.2642E+01,1.2499E+01,   ",
        "     1.2360E+01,1.2224E+01,1.2092E+01,1.1964E+01,1.1838E+01,1.1716E+01,1.1596E+01,   ",
        "     1.1480E+01,1.1365E+01;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     4.8004E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,4.1615E+05,4.1915E+05,4.2209E+05,4.2497E+05,4.2781E+05,4.3061E+05,   ",
        "     4.3339E+05,4.3614E+05,4.3888E+05,4.4160E+05,4.4431E+05,4.4701E+05,4.4971E+05,   ",
        "     4.5240E+05,4.5509E+05,4.5778E+05,4.6047E+05,4.6316E+05,4.6585E+05,4.6855E+05,   ",
        "     4.7124E+05,4.7395E+05,4.7666E+05,4.7937E+05,4.8209E+05,4.8482E+05,4.8755E+05,   ",
        "     4.9029E+05,4.9304E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     4.8004E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,1.8447E+01,1.8102E+01,1.7778E+01,1.7473E+01,1.7184E+01,1.6910E+01,   ",
        "     1.6648E+01,1.6398E+01,1.6158E+01,1.5928E+01,1.5707E+01,1.5495E+01,1.5289E+01,   ",
        "     1.5091E+01,1.4900E+01,1.4715E+01,1.4535E+01,1.4361E+01,1.4192E+01,1.4028E+01,   ",
        "     1.3869E+01,1.3714E+01,1.3563E+01,1.3416E+01,1.3273E+01,1.3133E+01,1.2997E+01,   ",
        "     1.2864E+01,1.2734E+01;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     5.3412E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,4.1730E+05,4.2036E+05,4.2335E+05,4.2629E+05,4.2917E+05,   ",
        "     4.3202E+05,4.3485E+05,4.3764E+05,4.4042E+05,4.4318E+05,4.4593E+05,4.4867E+05,   ",
        "     4.5140E+05,4.5413E+05,4.5685E+05,4.5957E+05,4.6229E+05,4.6501E+05,4.6773E+05,   ",
        "     4.7045E+05,4.7318E+05,4.7591E+05,4.7865E+05,4.8139E+05,4.8413E+05,4.8689E+05,   ",
        "     4.8965E+05,4.9241E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     5.3412E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,2.0485E+01,2.0094E+01,1.9728E+01,1.9383E+01,1.9058E+01,   ",
        "     1.8749E+01,1.8455E+01,1.8174E+01,1.7905E+01,1.7648E+01,1.7400E+01,1.7162E+01,   ",
        "     1.6933E+01,1.6712E+01,1.6498E+01,1.6292E+01,1.6092E+01,1.5898E+01,1.5710E+01,   ",
        "     1.5527E+01,1.5350E+01,1.5178E+01,1.5010E+01,1.4847E+01,1.4688E+01,1.4533E+01,   ",
        "     1.4382E+01,1.4234E+01;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     5.9273E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,4.1840E+05,4.2153E+05,4.2458E+05,4.2756E+05,   ",
        "     4.3050E+05,4.3340E+05,4.3627E+05,4.3911E+05,4.4193E+05,4.4473E+05,4.4752E+05,   ",
        "     4.5029E+05,4.5306E+05,4.5582E+05,4.5858E+05,4.6133E+05,4.6408E+05,4.6683E+05,   ",
        "     4.6959E+05,4.7234E+05,4.7509E+05,4.7785E+05,4.8062E+05,4.8338E+05,4.8616E+05,   ",
        "     4.8894E+05,4.9172E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     5.9273E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,2.2703E+01,2.2260E+01,2.1846E+01,2.1458E+01,   ",
        "     2.1091E+01,2.0744E+01,2.0413E+01,2.0098E+01,1.9798E+01,1.9509E+01,1.9233E+01,   ",
        "     1.8967E+01,1.8711E+01,1.8465E+01,1.8227E+01,1.7996E+01,1.7774E+01,1.7558E+01,   ",
        "     1.7349E+01,1.7146E+01,1.6950E+01,1.6758E+01,1.6572E+01,1.6391E+01,1.6215E+01,   ",
        "     1.6043E+01,1.5876E+01;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     6.5609E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,4.1945E+05,4.2264E+05,4.2575E+05,   ",
        "     4.2880E+05,4.3179E+05,4.3474E+05,4.3765E+05,4.4054E+05,4.4340E+05,4.4625E+05,   ",
        "     4.4907E+05,4.5189E+05,4.5469E+05,4.5749E+05,4.6028E+05,4.6307E+05,4.6585E+05,   ",
        "     4.6864E+05,4.7142E+05,4.7420E+05,4.7699E+05,4.7978E+05,4.8257E+05,4.8536E+05,   ",
        "     4.8816E+05,4.9097E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     6.5609E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,2.5113E+01,2.4612E+01,2.4145E+01,   ",
        "     2.3707E+01,2.3294E+01,2.2904E+01,2.2533E+01,2.2180E+01,2.1844E+01,2.1522E+01,   ",
        "     2.1213E+01,2.0916E+01,2.0631E+01,2.0356E+01,2.0091E+01,1.9836E+01,1.9588E+01,   ",
        "     1.9349E+01,1.9117E+01,1.8892E+01,1.8673E+01,1.8461E+01,1.8255E+01,1.8055E+01,   ",
        "     1.7860E+01,1.7670E+01;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     7.2446E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,4.2045E+05,4.2371E+05,   ",
        "     4.2688E+05,4.2999E+05,4.3304E+05,4.3604E+05,4.3900E+05,4.4194E+05,4.4484E+05,   ",
        "     4.4773E+05,4.5060E+05,4.5345E+05,4.5630E+05,4.5913E+05,4.6196E+05,4.6478E+05,   ",
        "     4.6760E+05,4.7041E+05,4.7323E+05,4.7604E+05,4.7886E+05,4.8168E+05,4.8450E+05,   ",
        "     4.8732E+05,4.9015E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     7.2446E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,2.7732E+01,2.7164E+01,   ",
        "     2.6636E+01,2.6143E+01,2.5678E+01,2.5240E+01,2.4825E+01,2.4430E+01,2.4053E+01,   ",
        "     2.3694E+01,2.3349E+01,2.3019E+01,2.2701E+01,2.2396E+01,2.2101E+01,2.1817E+01,   ",
        "     2.1542E+01,2.1277E+01,2.1019E+01,2.0770E+01,2.0529E+01,2.0294E+01,2.0066E+01,   ",
        "     1.9844E+01,1.9629E+01;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     7.9808E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,4.2139E+05,   ",
        "     4.2472E+05,4.2797E+05,4.3113E+05,4.3424E+05,4.3730E+05,4.4031E+05,4.4329E+05,   ",
        "     4.4625E+05,4.4918E+05,4.5209E+05,4.5499E+05,4.5787E+05,4.6074E+05,4.6361E+05,   ",
        "     4.6646E+05,4.6932E+05,4.7217E+05,4.7502E+05,4.7786E+05,4.8071E+05,4.8356E+05,   ",
        "     4.8641E+05,4.8926E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     7.9808E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,3.0574E+01,   ",
        "     2.9931E+01,2.9335E+01,2.8779E+01,2.8257E+01,2.7765E+01,2.7299E+01,2.6857E+01,   ",
        "     2.6436E+01,2.6035E+01,2.5651E+01,2.5283E+01,2.4930E+01,2.4590E+01,2.4263E+01,   ",
        "     2.3948E+01,2.3644E+01,2.3349E+01,2.3065E+01,2.2789E+01,2.2521E+01,2.2262E+01,   ",
        "     2.2010E+01,2.1766E+01;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     8.7722E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     4.2227E+05,4.2568E+05,4.2899E+05,4.3223E+05,4.3540E+05,4.3851E+05,4.4158E+05,   ",
        "     4.4461E+05,4.4761E+05,4.5059E+05,4.5355E+05,4.5649E+05,4.5941E+05,4.6232E+05,   ",
        "     4.6523E+05,4.6812E+05,4.7101E+05,4.7389E+05,4.7678E+05,4.7966E+05,4.8254E+05,   ",
        "     4.8542E+05,4.8830E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     8.7722E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     3.3659E+01,3.2930E+01,3.2256E+01,3.1629E+01,3.1042E+01,3.0490E+01,2.9968E+01,   ",
        "     2.9474E+01,2.9004E+01,2.8557E+01,2.8129E+01,2.7720E+01,2.7327E+01,2.6950E+01,   ",
        "     2.6587E+01,2.6238E+01,2.5900E+01,2.5575E+01,2.5260E+01,2.4955E+01,2.4660E+01,   ",
        "     2.4374E+01,2.4096E+01;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     9.6214E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,4.2308E+05,4.2658E+05,4.2997E+05,4.3327E+05,4.3650E+05,4.3968E+05,   ",
        "     4.4280E+05,4.4589E+05,4.4894E+05,4.5197E+05,4.5497E+05,4.5795E+05,4.6092E+05,   ",
        "     4.6387E+05,4.6681E+05,4.6975E+05,4.7267E+05,4.7559E+05,4.7851E+05,4.8142E+05,   ",
        "     4.8434E+05,4.8725E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     9.6214E+05,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,3.7005E+01,3.6178E+01,3.5416E+01,3.4709E+01,3.4049E+01,3.3429E+01,   ",
        "     3.2845E+01,3.2292E+01,3.1768E+01,3.1269E+01,3.0793E+01,3.0338E+01,2.9902E+01,   ",
        "     2.9484E+01,2.9082E+01,2.8694E+01,2.8321E+01,2.7961E+01,2.7613E+01,2.7277E+01,   ",
        "     2.6951E+01,2.6636E+01;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     1.0531E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,4.2382E+05,4.2741E+05,4.3088E+05,4.3426E+05,4.3756E+05,   ",
        "     4.4079E+05,4.4398E+05,4.4712E+05,4.5023E+05,4.5330E+05,4.5635E+05,4.5938E+05,   ",
        "     4.6239E+05,4.6539E+05,4.6837E+05,4.7134E+05,4.7430E+05,4.7726E+05,4.8021E+05,   ",
        "     4.8316E+05,4.8611E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     1.0531E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,4.0634E+01,3.9695E+01,3.8832E+01,3.8035E+01,3.7292E+01,   ",
        "     3.6597E+01,3.5943E+01,3.5325E+01,3.4740E+01,3.4184E+01,3.3654E+01,3.3149E+01,   ",
        "     3.2665E+01,3.2201E+01,3.1755E+01,3.1327E+01,3.0915E+01,3.0517E+01,3.0133E+01,   ",
        "     2.9762E+01,2.9403E+01;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     1.1504E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,4.2448E+05,4.2817E+05,4.3173E+05,4.3518E+05,   ",
        "     4.3855E+05,4.4186E+05,4.4511E+05,4.4831E+05,4.5147E+05,4.5459E+05,4.5769E+05,   ",
        "     4.6077E+05,4.6383E+05,4.6686E+05,4.6989E+05,4.7290E+05,4.7591E+05,4.7890E+05,   ",
        "     4.8189E+05,4.8488E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     1.1504E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,4.4572E+01,4.3503E+01,4.2527E+01,4.1626E+01,   ",
        "     4.0790E+01,4.0010E+01,3.9277E+01,3.8587E+01,3.7934E+01,3.7314E+01,3.6725E+01,   ",
        "     3.6164E+01,3.5627E+01,3.5113E+01,3.4620E+01,3.4146E+01,3.3691E+01,3.3252E+01,   ",
        "     3.2828E+01,3.2419E+01;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     1.2543E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,4.2507E+05,4.2886E+05,4.3251E+05,   ",
        "     4.3605E+05,4.3949E+05,4.4287E+05,4.4618E+05,4.4944E+05,4.5266E+05,4.5584E+05,   ",
        "     4.5899E+05,4.6212E+05,4.6522E+05,4.6831E+05,4.7138E+05,4.7443E+05,4.7748E+05,   ",
        "     4.8051E+05,4.8354E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     1.2543E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,4.8844E+01,4.7627E+01,4.6519E+01,   ",
        "     4.5502E+01,4.4560E+01,4.3684E+01,4.2863E+01,4.2091E+01,4.1363E+01,4.0673E+01,   ",
        "     4.0018E+01,3.9394E+01,3.8799E+01,3.8230E+01,3.7685E+01,3.7161E+01,3.6658E+01,   ",
        "     3.6174E+01,3.5708E+01;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     1.3651E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,4.2556E+05,4.2947E+05,   ",
        "     4.3322E+05,4.3684E+05,4.4037E+05,4.4382E+05,4.4720E+05,4.5053E+05,4.5381E+05,   ",
        "     4.5705E+05,4.6025E+05,4.6343E+05,4.6658E+05,4.6971E+05,4.7283E+05,4.7592E+05,   ",
        "     4.7901E+05,4.8209E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     1.3651E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,5.3484E+01,5.2094E+01,   ",
        "     5.0835E+01,4.9684E+01,4.8623E+01,4.7638E+01,4.6718E+01,4.5855E+01,4.5042E+01,   ",
        "     4.4274E+01,4.3546E+01,4.2854E+01,4.2194E+01,4.1564E+01,4.0961E+01,4.0383E+01,   ",
        "     3.9828E+01,3.9294E+01;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     1.4831E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,4.2595E+05,   ",
        "     4.2999E+05,4.3385E+05,4.3757E+05,4.4119E+05,4.4471E+05,4.4817E+05,4.5156E+05,   ",
        "     4.5490E+05,4.5820E+05,4.6146E+05,4.6469E+05,4.6790E+05,4.7108E+05,4.7424E+05,   ",
        "     4.7738E+05,4.8051E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     1.4831E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,5.8526E+01,   ",
        "     5.6935E+01,5.5502E+01,5.4199E+01,5.3001E+01,5.1893E+01,5.0861E+01,4.9896E+01,   ",
        "     4.8989E+01,4.8133E+01,4.7324E+01,4.6555E+01,4.5824E+01,4.5127E+01,4.4461E+01,   ",
        "     4.3823E+01,4.3211E+01;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     1.6086E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     4.2624E+05,4.3041E+05,4.3439E+05,4.3822E+05,4.4193E+05,4.4554E+05,4.4907E+05,   ",
        "     4.5254E+05,4.5595E+05,4.5931E+05,4.6263E+05,4.6591E+05,4.6917E+05,4.7240E+05,   ",
        "     4.7561E+05,4.7880E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     1.6086E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     6.4013E+01,6.2185E+01,6.0551E+01,5.9071E+01,5.7718E+01,5.6470E+01,5.5313E+01,   ",
        "     5.4232E+01,5.3220E+01,5.2267E+01,5.1367E+01,5.0514E+01,4.9704E+01,4.8933E+01,   ",
        "     4.8196E+01,4.7492E+01;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     1.7419E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,4.2642E+05,4.3074E+05,4.3485E+05,4.3879E+05,4.4260E+05,4.4630E+05,   ",
        "     4.4991E+05,4.5345E+05,4.5693E+05,4.6036E+05,4.6374E+05,4.6709E+05,4.7040E+05,   ",
        "     4.7368E+05,4.7694E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     1.7419E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,6.9990E+01,6.7883E+01,6.6014E+01,6.4331E+01,6.2800E+01,6.1394E+01,   ",
        "     6.0094E+01,5.8884E+01,5.7753E+01,5.6691E+01,5.5691E+01,5.4744E+01,5.3847E+01,   ",
        "     5.2994E+01,5.2181E+01;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     1.8834E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,4.2646E+05,4.3096E+05,4.3521E+05,4.3927E+05,4.4319E+05,   ",
        "     4.4699E+05,4.5069E+05,4.5431E+05,4.5786E+05,4.6136E+05,4.6481E+05,4.6821E+05,   ",
        "     4.7158E+05,4.7492E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     1.8834E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,7.6519E+01,7.4080E+01,7.1935E+01,7.0017E+01,6.8281E+01,   ",
        "     6.6694E+01,6.5232E+01,6.3876E+01,6.2613E+01,6.1429E+01,6.0316E+01,5.9266E+01,   ",
        "     5.8272E+01,5.7328E+01;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     2.0334E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,4.2635E+05,4.3105E+05,4.3546E+05,4.3966E+05,   ",
        "     4.4369E+05,4.4760E+05,4.5139E+05,4.5510E+05,4.5873E+05,4.6230E+05,4.6582E+05,   ",
        "     4.6929E+05,4.7272E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     2.0334E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,8.3665E+01,8.0827E+01,7.8356E+01,7.6164E+01,   ",
        "     7.4192E+01,7.2398E+01,7.0753E+01,6.9232E+01,6.7819E+01,6.6499E+01,6.5261E+01,   ",
        "     6.4095E+01,6.2993E+01;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     2.1923E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,4.2609E+05,4.3101E+05,4.3560E+05,   ",
        "     4.3995E+05,4.4411E+05,4.4812E+05,4.5202E+05,4.5582E+05,4.5953E+05,4.6318E+05,   ",
        "     4.6677E+05,4.7030E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     2.1923E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,9.1511E+01,8.8190E+01,8.5332E+01,   ",
        "     8.2819E+01,8.0573E+01,7.8542E+01,7.6687E+01,7.4980E+01,7.3398E+01,7.1925E+01,   ",
        "     7.0547E+01,6.9252E+01;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     2.3604E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,4.2564E+05,4.3082E+05,   ",
        "     4.3561E+05,4.4013E+05,4.4443E+05,4.4856E+05,4.5257E+05,4.5646E+05,4.6027E+05,   ",
        "     4.6400E+05,4.6766E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     2.3604E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,1.0015E+02,9.6239E+01,   ",
        "     9.2918E+01,9.0027E+01,8.7463E+01,8.5159E+01,8.3065E+01,8.1145E+01,7.9374E+01,   ",
        "     7.7729E+01,7.6195E+01;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     2.5382E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,4.2498E+05,   ",
        "     4.3047E+05,4.3549E+05,4.4019E+05,4.4464E+05,4.4891E+05,4.5303E+05,4.5703E+05,   ",
        "     4.6093E+05,4.6475E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     2.5382E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,1.0972E+02,   ",
        "     1.0507E+02,1.0119E+02,9.7851E+01,9.4915E+01,9.2295E+01,8.9926E+01,8.7766E+01,   ",
        "     8.5780E+01,8.3942E+01;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     2.7261E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     4.2408E+05,4.2993E+05,4.3522E+05,4.4012E+05,4.4475E+05,4.4916E+05,4.5341E+05,   ",
        "     4.5752E+05,4.6152E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     2.7261E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     1.2038E+02,1.1479E+02,1.1023E+02,1.0635E+02,1.0298E+02,9.9995E+01,9.7312E+01,   ",
        "     9.4877E+01,9.2647E+01;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     2.9246E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,4.2290E+05,4.2918E+05,4.3478E+05,4.3992E+05,4.4473E+05,4.4931E+05,   ",
        "     4.5369E+05,4.5792E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     2.9246E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,1.3233E+02,1.2554E+02,1.2013E+02,1.1562E+02,1.1173E+02,1.0831E+02,   ",
        "     1.0527E+02,1.0252E+02;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     3.1341E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,4.2137E+05,4.2820E+05,4.3416E+05,4.3957E+05,4.4459E+05,   ",
        "     4.4934E+05,4.5387E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     3.1341E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,1.4585E+02,1.3748E+02,1.3102E+02,1.2572E+02,1.2122E+02,   ",
        "     1.1731E+02,1.1384E+02;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     3.3552E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,4.1941E+05,4.2695E+05,4.3334E+05,4.3905E+05,   ",
        "     4.4432E+05,4.4926E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     3.3552E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,1.6135E+02,1.5082E+02,1.4302E+02,1.3677E+02,   ",
        "     1.3154E+02,1.2704E+02;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     3.5886E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,4.1692E+05,4.2539E+05,4.3229E+05,   ",
        "     4.3836E+05,4.4389E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     3.5886E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,1.7941E+02,1.6585E+02,1.5632E+02,   ",
        "     1.4890E+02,1.4279E+02;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     3.8348E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,4.1370E+05,4.2346E+05,   ",
        "     4.3100E+05,4.3748E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     3.8348E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,2.0095E+02,1.8289E+02,   ",
        "     1.7111E+02,1.6223E+02;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     4.0949E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,4.0942E+05,   ",
        "     4.2109E+05,4.2943E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     4.0949E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,2.2766E+02,   ",
        "     2.0246E+02,1.8765E+02;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     4.3697E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     4.0343E+05,4.1823E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     4.3697E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     2.6302E+02,2.2513E+02;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Enthalpy,                !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     4.6607E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,3.9373E+05;                                                          ",
        "                                                                                     ",
        "   FluidProperties:Superheated,                                                      ",
        "     R410a,                   !- Fluid Name                                          ",
        "     Density,                 !- Fluid Property Type                                 ",
        "     R410aSuperHeatTemperatures,  !- Temperature Values Name                         ",
        "     4.6607E+06,              !- Pressure {Pa}                                       ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,0.0000E+00,   ",
        "     0.0000E+00,3.1758E+02;                                                          ",
        "                                                                                     ",
        " !***************************************************************************        ",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    DataGlobals::BeginEnvrnFlag = true;
    DataSizing::CurZoneEqNum = 1;
    DataEnvironment::OutBaroPress = 101325;          // sea level
    DataZoneEquipment::ZoneEquipInputsFilled = true; // denotes zone equipment has been read in
    StdRhoAir = PsyRhoAirFnPbTdbW(DataEnvironment::OutBaroPress, 20.0, 0.0);

    // Read in IDF
    ProcessScheduleInput();                    // read schedules
    CurveManager::GetCurveInput();             // read curves
    FluidProperties::GetFluidPropertiesData(); // read refrigerant properties

    // set up ZoneEquipConfig data
    DataGlobals::NumOfZones = 1;
    DataZoneEquipment::ZoneEquipConfig.allocate(1);
    DataZoneEquipment::ZoneEquipConfig(1).IsControlled = true;
    DataZoneEquipment::ZoneEquipConfig(1).NumInletNodes = 1;
    DataZoneEquipment::ZoneEquipConfig(1).NumExhaustNodes = 1;
    DataZoneEquipment::ZoneEquipConfig(1).InletNode.allocate(1);
    DataZoneEquipment::ZoneEquipConfig(1).ExhaustNode.allocate(1);
    DataZoneEquipment::ZoneEquipConfig(1).InletNode(1) = 2;
    DataZoneEquipment::ZoneEquipConfig(1).ExhaustNode(1) = 1;

    GetVRFInputData(ErrorsFound); // read VRF
    EXPECT_FALSE(ErrorsFound);

    // Check expected result from GetInput

    // #6218 Fan:SystemModel is used and DX coil RatedVolAirFlowRate was not set equal to system fan designAirVolFlowRate
    EXPECT_EQ(DXCoil(1).RatedAirVolFlowRate(1), 1.0);
    EXPECT_EQ(DXCoil(2).RatedAirVolFlowRate(1), 1.0);
    EXPECT_EQ(HVACFan::fanObjs[VRFTU(1).FanIndex]->designAirVolFlowRate, 1.0);

    // Run and Check: GetSupHeatTempRefrig
    {
        //   Test the method GetSupHeatTempRefrig, which determines the refrigerant temperature corresponding to the given
        //   enthalpy and pressure.

        // Inputs_condition
        std::string Refrigerant = "R410A";
        Real64 Pressure = 2419666.67; // actual pressure given as input [Pa]
        Real64 Enthalpy = 432842;     // actual enthalpy given as input [kJ/kg]
        Real64 TempLow = 40;          // lower bound of temperature in the iteration [C]
        Real64 TempUp = 60;           // upper bound of temperature in the iteration [C]
        int RefrigIndex = 2;          // Index to Refrigerant Properties
        Real64 Temperature = 44;      // temperature to be returned [C]
        std::string CalledFrom = "EnergyPlusFixture:VRF_FluidTCtrl_VRFOU_Compressor";

        DataEnvironment::OutDryBulbTemp = 10.35;

        // Run
        Temperature = GetSupHeatTempRefrig(Refrigerant, Pressure, Enthalpy, TempLow, TempUp, RefrigIndex, CalledFrom);

        // Test
        EXPECT_NEAR(Temperature, 44.5, 0.5);
    }

    // Run and Check: VRFHR_OU_HR_Mode
    {
        //   Test the method VRFOU_CalcCompH, which determines the operational mode of the VRF-HR system, given the terminal unit side load
        //   conditions. Compressor and OU hex performance are analysed for each mode.

        // Inputs_condition
        Real64 const h_IU_evap_in = 225017; // enthalpy of IU evaporator at inlet [kJ/kg]
        Real64 const h_comp_out = 432950;   // enthalpy of refrigerant at compressor outlet [kJ/kg]
        Real64 const Q_c_TU_PL = 4972;      // IU evaporator load, including piping loss [W]
        Real64 const Q_h_TU_PL = 9954;      // IU condenser load, including piping loss [W]
        Real64 const Tdischarge = 36.37;    // VRF Compressor discharge refrigerant temperature [C]
        Real64 Tsuction = 4.86;             // VRF compressor suction refrigerant temperature [C]
        Real64 Te_update = 5;               // updated evaporating temperature, only updated when Tsuction is updated [C]
        Real64 h_comp_in = 429529;          // enthalpy of refrigerant at compressor inlet [kJ/kg]
        Real64 h_IU_PLc_out = 429529;       // enthalpy of refrigerant at the outlet of IU evaporator side main pipe [kJ/kg]
        Real64 Pipe_Q_c = 0;                // IU evaporator side piping loss [W]
        Real64 Q_c_OU;                      // OU evaporator load [W]
        Real64 Q_h_OU;                      // OU condenser load [W]
        Real64 m_ref_IU_evap;               // mass flow rate of Refrigerant through IU evaporators [kg/s]
        Real64 m_ref_OU_evap;               // mass flow rate of Refrigerant through OU evaporator [kg/s]
        Real64 m_ref_OU_cond;               // mass flow rate of Refrigerant through OU condenser [kg/s]
        Real64 N_fan_OU;                    // outdoor unit fan power [W]
        Real64 CompSpdActual;               // Actual compressor running speed [rps]
        Real64 Ncomp;                       // compressor power [W]

        DataEnvironment::OutDryBulbTemp = 10.35;

        // Run
        VRF(VRFCond).VRFHR_OU_HR_Mode(h_IU_evap_in,
                                      h_comp_out,
                                      Q_c_TU_PL,
                                      Q_h_TU_PL,
                                      Tdischarge,
                                      Tsuction,
                                      Te_update,
                                      h_comp_in,
                                      h_IU_PLc_out,
                                      Pipe_Q_c,
                                      Q_c_OU,
                                      Q_h_OU,
                                      m_ref_IU_evap,
                                      m_ref_OU_evap,
                                      m_ref_OU_cond,
                                      N_fan_OU,
                                      CompSpdActual,
                                      Ncomp);

        // Test
        EXPECT_NEAR(756, CompSpdActual, 1);
        EXPECT_NEAR(899, Ncomp, 1);
        EXPECT_NEAR(3186, Q_c_OU, 1);
        EXPECT_NEAR(0, Q_h_OU, 1);
        EXPECT_NEAR(242, N_fan_OU, 1);
        EXPECT_NEAR(0.0158, m_ref_OU_evap, 0.0005);
        EXPECT_NEAR(0, m_ref_OU_cond, 0.0005);
    }

    // Run and Check: VRFOU_CapModFactor
    {
        //   Test the method VRFOU_CapModFactor, which calculates capacity modification factor for the compressors at Outdoor Unit.
        //   This factor is used to modify the system evaporative capacity, by describing
        //   the difference between rated conditions and real conditions.

        // Inputs_condition
        Real64 const h_comp_in_real = 429529; // Enthalpy of refrigerant at the compressor inlet at real conditions [kJ/kg]
        Real64 const h_evap_in_real = 225016; // Enthalpy of refrigerant at the evaporator inlet at real conditions [kJ/kg]
        Real64 const P_evap_real = 509784;    // Evaporative pressure at real conditions [Pa]
        Real64 const T_comp_in_real = 0.65;   // Temperature of the refrigerant at the compressor inlet at real conditions [C]
        Real64 const T_comp_in_rate = -5.35;  // Temperature of the refrigerant at the compressor inlet at rated conditions [C]
        Real64 const T_cond_out_rate = 31.38; // Temperature of the refrigerant at the condensor outlet at rated conditions [C]
        Real64 C_cap_operation;

        // Run
        C_cap_operation =
            VRF(VRFCond).VRFOU_CapModFactor(h_comp_in_real, h_evap_in_real, P_evap_real, T_comp_in_real, T_comp_in_rate, T_cond_out_rate);

        // Test
        EXPECT_NEAR(0.879, C_cap_operation, 0.005);
    }

    // Run and Check: VRFOU_CompSpd
    {//   Test the method VRFOU_CompSpd, which calculates the compressor speed at given
     //   operational conditions to meet the evaporator or condenser capacity provided.

     {// a. Evaporator

      // Inputs_condition
      Real64 const Q_req = 6971;        // Required capacity [W]
    Real64 const T_suction = -13.35;    // Compressor suction temperature Te' [C]
    Real64 const T_discharge = 36.37;   // Compressor discharge temperature Tc' [C]
    Real64 const h_IU_evap_in = 225016; // Enthalpy of IU at inlet, for C_cap_operation calculation [kJ/kg]
    Real64 const h_comp_in = 429529;    // Enthalpy after piping loss (compressor inlet), for C_cap_operation calculation [kJ/kg]
    Real64 CompSpdActual;               // Actual compressor running speed [rps]

    // Run
    VRF(VRFCond).VRFOU_CompSpd(Q_req, FlagEvapMode, T_suction, T_discharge, h_IU_evap_in, h_comp_in, CompSpdActual);

    // Test
    EXPECT_NEAR(1295, CompSpdActual, 5);
}

{
    // b. Condenser

    // Inputs_condition
    Real64 const Q_req = 6953;          // Required capacity [W]
    Real64 const T_suction = -13.35;    // Compressor suction temperature Te' [C]
    Real64 const T_discharge = 36.37;   // Compressor discharge temperature Tc' [C]
    Real64 const h_IU_evap_in = 225016; // Enthalpy of IU at inlet, for C_cap_operation calculation [kJ/kg]
    Real64 const h_comp_in = 429529;    // Enthalpy after piping loss (compressor inlet), for C_cap_operation calculation [kJ/kg]
    Real64 CompSpdActual;               // Actual compressor running speed [rps]

    // Run
    VRF(VRFCond).VRFOU_CompSpd(Q_req, FlagCondMode, T_suction, T_discharge, h_IU_evap_in, h_comp_in, CompSpdActual);

    // Test
    EXPECT_NEAR(950, CompSpdActual, 5);
}
} // namespace EnergyPlus

// Run and Check: VRFOU_CompCap
{
    //   Test the method VRFOU_CompCap, which calculates the compressor performance (power and capacity)
    //   at given compressor speed and operational conditions.

    // Inputs_condition
    Real64 const CompSpdActual = 1298;  // Actual compressor running speed [rps]
    Real64 const T_suction = -13.35;    // Compressor suction temperature Te' [C]
    Real64 const T_discharge = 36.37;   // Compressor discharge temperature Tc' [C]
    Real64 const h_IU_evap_in = 225016; // Enthalpy of IU at inlet, for C_cap_operation calculation [kJ/kg]
    Real64 const h_comp_in = 429529;    // Enthalpy after piping loss (compressor inlet), for C_cap_operation calculation [kJ/kg]
    Real64 Q_c_tot;                     // Compressor evaporative capacity [W]
    Real64 Ncomp;                       // Compressor power [W]

    // Run
    VRF(VRFCond).VRFOU_CompCap(CompSpdActual, T_suction, T_discharge, h_IU_evap_in, h_comp_in, Q_c_tot, Ncomp);

    // Test
    EXPECT_NEAR(6990, Q_c_tot, 10);
    EXPECT_NEAR(1601, Ncomp, 10);
}

// Run and Check: VRFOU_CalcComp
{
    //   Test the method VRFOU_CalcCompH, which simulates the compressor performance at given oprtaional conditions. More specifically, it sepcifies
    //   the compressor speed to provide sufficient evaporative capacity, and calculate the power of the compressor running at the specified
    //   speed. Note that it may be needed to manipulate the operational conditions to further adjust system capacity at low load conditions.
    //   The low load modification logics are different for cooling mode and heating mode.

    // Inputs_condition
    Real64 TU_load = 6006;              // Indoor unit cooling load [W]
    Real64 T_suction = 8.86;            // Compressor suction temperature Te' [C]
    Real64 T_discharge = 40.26;         // Compressor discharge temperature Tc' [C]
    Real64 Pipe_h_out_ave = 233428;     // Average Enthalpy of the refrigerant leaving IUs [kJ/kg]
    Real64 IUMaxCondTemp = 36;          // VRV IU condensing temperature, max among all indoor units [C]
    Real64 MinOutdoorUnitTe = -72;      // The minimum temperature that OU Te can be at cooling mode (only used for calculating Min capacity)
    Real64 Tfs = 10.90;                 // Temperature of the air at the OU evaporator coil surface [C]]
    Real64 Pipe_Q = 162.67;             // Piping Loss Algorithm Parameter: Heat loss [W]
    Real64 OUEvapHeatExtract = 5110.40; // Evaporator heat extract [W]
    Real64 Ncomp = 1058;                // Compressor power [W]
    Real64 CompSpdActual;               // Actual compressor running speed [rps]

    // Run
    VRF(VRFCond).VRFOU_CalcCompH(
        TU_load, T_suction, T_discharge, Pipe_h_out_ave, IUMaxCondTemp, MinOutdoorUnitTe, Tfs, Pipe_Q, OUEvapHeatExtract, CompSpdActual, Ncomp);

    // Test
    EXPECT_NEAR(5110, OUEvapHeatExtract, 1);
    EXPECT_NEAR(1500, CompSpdActual, 1);
    EXPECT_NEAR(2080, Ncomp, 1);
    EXPECT_EQ(Node(VRFTU(1).VRFTUInletNodeNum).MassFlowRate, 0.0);
}
}

TEST_F(EnergyPlusFixture, VRF_FluidTCtrl_VRFOU_Coil)
{
    //   PURPOSE OF THIS TEST:
    //   Test a group of methods related with the outdoor unit coil calculations in the VRF_FluidTCtrl model.

    using namespace HVACVariableRefrigerantFlow;

    // Allocate
    int NumVRFCondenser = 1;
    int VRFCond = 1;
    VRF.allocate(NumVRFCondenser);

    // Inputs_general
    int const FlagCondMode(0);   // Flag for running as condenser [-]
    int const FlagEvapMode(1);   // Flag for running as evaporator [-]
    Real64 OutDryBulbTemp;       // Temperature of outdoor air [C]
    Real64 OutHumRat;            // Humidity ratio of outdoor air [kg/kg]
    Real64 OutBaroPress(101325); // Outdoor air pressure [Pa]
    Real64 SC;                   // Outdoor unit subcooling [C]
    Real64 SH;                   // Outdoor unit superheating [C]
    Real64 Tdischarge;           // VRF Compressor discharge refrigerant temperature [C]
    Real64 Tsuction;             // VRF compressor suction refrigerant temperature [C]
    Real64 Q_h_OU;               // OU condenser heat exchange rate [W]
    Real64 Q_c_OU;               // OU evaporator heat exchange rate [W]
    Real64 m_air;                // OU coil air mass flow rate [kg/s]
    Real64 temp;                 // OU coil air mass flow rate [kg/s]

    // Inputs_VRF configurations
    VRF(VRFCond).RateBFOUCond = 0.05;
    VRF(VRFCond).RateBFOUEvap = 0.281;
    VRF(VRFCond).C1Te = 0;
    VRF(VRFCond).C2Te = 6.05E-1;
    VRF(VRFCond).C3Te = 2.50E-2;
    VRF(VRFCond).C1Tc = 0;
    VRF(VRFCond).C2Tc = -0.091;
    VRF(VRFCond).C3Tc = 0.075;

    // Pre-process
    DataEnvironment::OutBaroPress = OutBaroPress;
    InitializePsychRoutines();

    // Run and Check: VRFOU_Cap
    { //   Test the method VRFOU_Cap, which determines the VRF OU heat transfer rate, given refrigerant side temperature,
      //   i.e., condensing temperature and SC for condenser, or evaporating temperature and SH for evaporator.
     {// a. Condenser

      // Inputs_condition
      m_air = 3.6;
    OutDryBulbTemp = 28;
    OutHumRat = 0.0146;
    SC = 1;
    Tdischarge = 36;

    // Run
    Q_h_OU = VRF(VRFCond).VRFOU_Cap(FlagCondMode, Tdischarge, SC, m_air, OutDryBulbTemp, OutHumRat);

    // Test
    EXPECT_NEAR(27551, Q_h_OU, 10);
}

{
    // b. Evaporator

    // Inputs_condition
    m_air = 3.6;
    OutDryBulbTemp = 7;
    OutHumRat = 0.0019;
    SH = 1;
    Tsuction = -3;

    // Run
    Q_c_OU = VRF(VRFCond).VRFOU_Cap(FlagEvapMode, Tsuction, SH, m_air, OutDryBulbTemp, OutHumRat);

    // Test
    EXPECT_NEAR(24456, Q_c_OU, 10);
}
}

// Run and Check: VRFOU_FlowRate
{ //   Test the method VRFOU_Cap, which calculates the outdoor unit fan flow rate, given VRF OU load and refrigerant side temperature, i.e.,
  //   condensing temperature and SC for condenser, or evaporating temperature and SH for evaporator.
 {// a. Condenser

  // Inputs_condition
  Q_h_OU = 27551;
OutDryBulbTemp = 28;
OutHumRat = 0.0146;
SC = 1;
Tdischarge = 36;

// Run
m_air = VRF(VRFCond).VRFOU_FlowRate(FlagCondMode, Tdischarge, SC, Q_h_OU, OutDryBulbTemp, OutHumRat);

// Test
EXPECT_NEAR(3.6, m_air, 0.01);
}

{
    // b. Evaporator

    // Inputs_condition
    Q_c_OU = 24456;
    OutDryBulbTemp = 7;
    OutHumRat = 0.0019;
    SH = 1;
    Tsuction = -3;

    // Run
    m_air = VRF(VRFCond).VRFOU_FlowRate(FlagEvapMode, Tsuction, SH, Q_c_OU, OutDryBulbTemp, OutHumRat);

    // Test
    EXPECT_NEAR(3.6, m_air, 0.01);
}
}

// Run and Check: VRFOU_TeTc
{ //   Test the method VRFOU_Cap, which calculates the VRF OU refrigerant side temperature, i.e., condensing temperature
  //   at cooling mode, or evaporating temperature at heating mode, given the coil heat
  //   release/extract amount and air side parameters.
 {// a. Condenser

  // Inputs_condition
  m_air = 3.6;
Q_h_OU = 27551;
OutDryBulbTemp = 28;
OutHumRat = 0.0146;
SC = 1;

// Run
VRF(VRFCond).VRFOU_TeTc(FlagCondMode, Q_h_OU, SC, m_air, OutDryBulbTemp, OutHumRat, OutBaroPress, temp, Tdischarge);

// Test
EXPECT_NEAR(36, Tdischarge, 0.05);
}

{
    // b. Evaporator

    // Inputs_condition
    m_air = 3.6;
    Q_c_OU = 24456;
    OutDryBulbTemp = 7;
    OutHumRat = 0.0019;
    SH = 1;
    Tsuction = -3;

    // Run
    VRF(VRFCond).VRFOU_TeTc(FlagEvapMode, Q_c_OU, SH, m_air, OutDryBulbTemp, OutHumRat, OutBaroPress, temp, Tsuction);

    // Test
    EXPECT_NEAR(-3, Tsuction, 0.05);
}
}

// Run and Check: VRFOU_SCSH
{
    //   Test the method VRFOU_Cap, which calculates the VRF OU refrigerant side temperature, i.e., condensing temperature
    //   at cooling mode, or evaporating temperature at heating mode, given the coil heat
    //   release/extract amount and air side parameters.
    {
        // a. Condenser

        // Inputs_condition
        m_air = 3.6;
        Q_h_OU = 27551;
        OutDryBulbTemp = 28;
        OutHumRat = 0.0146;
        Tdischarge = 36;

        // Run
        SC = VRF(VRFCond).VRFOU_SCSH(FlagCondMode, Q_h_OU, Tdischarge, m_air, OutDryBulbTemp, OutHumRat, OutBaroPress);

        // Test
        EXPECT_NEAR(1, SC, 0.01);
    }

    {
        // b. Evaporator

        // Inputs_condition
        m_air = 3.6;
        Q_c_OU = 24456;
        OutDryBulbTemp = 7;
        OutHumRat = 0.0019;
        Tsuction = -3;

        // Run
        SH = VRF(VRFCond).VRFOU_SCSH(FlagEvapMode, Q_c_OU, Tsuction, m_air, OutDryBulbTemp, OutHumRat, OutBaroPress);

        // Test
        EXPECT_NEAR(1, SH, 0.01);
    }
}
// Clean up
VRF.deallocate();
}

TEST_F(EnergyPlusFixture, VRF_FluidTCtrl_GetCoilInput)
{
    // PURPOSE OF THE TEST:
    //   IDF Read in for the new coil type: Coil:Cooling:DX:VariableRefrigerantFlow:FluidTemperatureControl

    std::string const idf_objects =
        delimited_string({" Coil:Cooling:DX:VariableRefrigerantFlow:FluidTemperatureControl,  ",
                          " 	 TU1 VRF DX Cooling Coil, !- Name							   ",
                          " 	 VRFAvailSched,           !- Availability Schedule Name		   ",
                          " 	 TU1 VRF DX CCoil Inlet Node,  !- Coil Air Inlet Node		   ",
                          " 	 TU1 VRF DX CCoil Outlet Node, !- Coil Air Outlet Node		   ",
                          " 	 2200,                    !- Rated Total Cooling Capacity {W}   ",
                          " 	 0.865,                   !- Rated Sensible Heat Ratio		   ",
                          " 	 3,                       !- Indoor Unit Reference Superheating ",
                          " 	 IUEvapTempCurve,         !- Indoor Unit Evaporating Temperature",
                          " 	 ;                        !- Name of Water Storage Tank for Cond",
                          " Curve:Quadratic,												   ",
                          "     IUEvapTempCurve,         !- Name							   ",
                          "     0,                       !- Coefficient1 Const				   ",
                          "     0.80404,                 !- Coefficient2 x					   ",
                          "     0,                       !- Coefficient3 x**2				   ",
                          "     0,                       !- Minimum Value of x				   ",
                          "     15,                      !- Maximum Value of x				   ",
                          "     ,                        !- Minimum Curve Outp				   ",
                          "     ,                        !- Maximum Curve Outp				   ",
                          "     Dimensionless,           !- Input Unit Type fo				   ",
                          "     Dimensionless;           !- Output Unit Type				   "});

    ASSERT_TRUE(process_idf(idf_objects));

    // Run the method
    GetDXCoils();

    // Check the results
    ASSERT_EQ(1, NumDXCoils);
    EXPECT_EQ(DXCoil(1).DXCoilType_Num, 33);
    EXPECT_EQ(DXCoil(1).RatedTotCap(1), 2200);
    EXPECT_EQ(DXCoil(1).RatedSHR(1), 0.865);
    EXPECT_EQ(DXCoil(1).C1Te, 0);
    EXPECT_EQ(DXCoil(1).C2Te, 0.80404);
    EXPECT_EQ(DXCoil(1).C3Te, 0);
    EXPECT_EQ(DXCoil(1).SH, 3);
}

TEST_F(EnergyPlusFixture, VRF_FluidTCtrl_CompResidual)
{
    // PURPOSE OF THIS SUBROUTINE:
    //  Calculates residual function ((VRV terminal unit cooling output - Zone sensible cooling load)

    using namespace CurveManager;

    int CurveNum = 1;
    int NumPar;
    double Te = -2.796; // Outdoor unit evaporating temperature
    double Tdis = 40.093;
    double CondHeat = 1864.44;
    Array1D<Real64> Par;

    // Allocate
    NumCurves = 1; // CurveManager::NumCurves
    PerfCurve.allocate(NumCurves);

    NumPar = 3;
    Par.allocate(NumPar);

    // Inputs: curve parameters
    Par(1) = Tdis;
    Par(2) = CondHeat;
    Par(3) = CurveNum;

    // Inputs: parameters
    PerfCurve(CurveNum).CurveType = CurveManager::BiQuadratic;
    PerfCurve(CurveNum).ObjectType = "Curve:Biquadratic";
    PerfCurve(CurveNum).InterpolationType = EvaluateCurveToLimits;
    PerfCurve(CurveNum).Coeff1 = 724.71125;  // Coefficient1 Constant
    PerfCurve(CurveNum).Coeff2 = -21.867868; // Coefficient2 x
    PerfCurve(CurveNum).Coeff3 = 0.52480042; // Coefficient3 x**2
    PerfCurve(CurveNum).Coeff4 = -17.043566; // Coefficient4 y
    PerfCurve(CurveNum).Coeff5 = -.40346383; // Coefficient5 y**2
    PerfCurve(CurveNum).Coeff6 = 0.29573589; // Coefficient6 x*y
    PerfCurve(CurveNum).Var1Min = 15;        // Minimum Value of x
    PerfCurve(CurveNum).Var1Max = 65;        // Maximum Value of x
    PerfCurve(CurveNum).Var2Min = -30;       // Minimum Value of y
    PerfCurve(CurveNum).Var2Max = 15;        // Maximum Value of y

    // Run and Check
    double CompResidual = HVACVariableRefrigerantFlow::CompResidual_FluidTCtrl(Te, Par);
    EXPECT_NEAR(1.652, CompResidual, 0.005);

    // Clean up
    PerfCurve.deallocate();
    Par.deallocate();
}

TEST_F(EnergyPlusFixture, VRF_FluidTCtrl_FanSpdResidualCool)
{
    // PURPOSE OF THIS TEST:
    //   Test the method FanSpdResidualCool.

    using namespace DXCoils;

    int NumPar;
    double FanSpdRto;
    double ZnSenLoad;
    double Th2;
    double TairInlet;
    double Garate;
    double BF;
    Array1D<Real64> Par;

    // Allocate
    NumPar = 6;
    Par.allocate(6);

    // Inputs:
    FanSpdRto = 0.5;
    ZnSenLoad = 2716.62;
    Th2 = 17.41212;
    TairInlet = 25.55534;
    Garate = 0.20664;
    BF = 0.0592;
    Par(1) = ZnSenLoad;
    Par(2) = Th2;
    Par(3) = TairInlet;
    Par(4) = Garate;
    Par(5) = BF;

    // Run and Check
    double FanSpdResidual = FanSpdResidualCool(FanSpdRto, Par);
    EXPECT_NEAR(-0.707, FanSpdResidual, 0.0005);

    // Clean up
    Par.deallocate();
}

TEST_F(EnergyPlusFixture, VRF_FluidTCtrl_FanSpdResidualHeat)
{
    // PURPOSE OF THIS TEST:
    //   Test the method FanSpdResidualHeat.

    using namespace DXCoils;

    int NumPar;
    double FanSpdRto;
    double ZnSenLoad;
    double Th2;
    double TairInlet;
    double Garate;
    double BF;
    Array1D<Real64> Par;

    // Allocate
    NumPar = 6;
    Par.allocate(6);

    // Inputs:
    FanSpdRto = 0.5;
    ZnSenLoad = 4241.66;
    Th2 = 41.221;
    TairInlet = 20.236;
    Garate = 0.21136;
    BF = 0.1360;
    Par(1) = ZnSenLoad;
    Par(2) = Th2;
    Par(3) = TairInlet;
    Par(4) = Garate;
    Par(5) = BF;

    // Run and Check
    double FanSpdResidual = FanSpdResidualHeat(FanSpdRto, Par);
    EXPECT_NEAR(-0.5459, FanSpdResidual, 0.0005);

    // Clean up
    Par.deallocate();
}

TEST_F(EnergyPlusFixture, VRF_FluidTCtrl_CalcVRFIUAirFlow)
{
    // PURPOSE OF THIS TEST:
    //   Test the method CalcVRFIUAirFlow, which analyzes the VRF Indoor Unit operations given zonal loads.
    //   Calculated parameters includie: (1) Fan Speed Ratio, (2) SH/SC Degrees, and (3) Coil Inlet/Outlet conditions

    using namespace DXCoils;
    using namespace DataZoneEnergyDemands;
    using namespace EnergyPlus::Psychrometrics;
    using DataEnvironment::OutBaroPress;

    int ZoneIndex;      // index to zone where the VRF Terminal Unit resides
    int CoolCoilIndex;  // index to VRFTU cooling coil
    int HeatCoilIndex;  // index to VRFTU heating coil
    int Mode;           // mode 0 for cooling, 1 for heating, 2 for neither cooling nor heating
    Real64 Temp;        // evaporating or condensing temperature
    Real64 FanSpdRatio; // fan speed ratio
    Real64 Wout;        // outlet air humidity ratio
    Real64 Toutlet;     // outlet air temperature
    Real64 Houtlet;     // outlet air enthalpy
    Real64 SHact;       // actual SH
    Real64 SCact;       // actual SC

    // Allocate
    int NumCoils = 2;
    DXCoil.allocate(NumCoils);
    int NumZones = 2;
    ZoneSysEnergyDemand.allocate(NumZones);

    // Common Inputs
    CoolCoilIndex = 1;
    HeatCoilIndex = 2;
    FanSpdRatio = 0;
    Wout = 1;
    OutBaroPress = 101570;
    InitializePsychRoutines();

    DXCoil(CoolCoilIndex).C1Te = 0;
    DXCoil(CoolCoilIndex).C2Te = 0.804;
    DXCoil(CoolCoilIndex).C3Te = 0;
    DXCoil(CoolCoilIndex).SH = 3.00;
    DXCoil(CoolCoilIndex).SupplyFanIndex = 0;
    DXCoil(HeatCoilIndex).C1Tc = -1.905;
    DXCoil(HeatCoilIndex).C2Tc = 0.4333;
    DXCoil(HeatCoilIndex).C3Tc = 0.0207;
    DXCoil(HeatCoilIndex).SC = 5.00;
    DXCoil(HeatCoilIndex).SupplyFanIndex = 0;

    // Run and Check for Cooling Mode
    Mode = 0;
    Temp = 15;
    ZoneIndex = 1;

    ZoneSysEnergyDemand(ZoneIndex).OutputRequiredToCoolingSP = -2716.6229;
    ZoneSysEnergyDemand(ZoneIndex).OutputRequiredToHeatingSP = -45507.8487;

    DXCoil(CoolCoilIndex).RatedAirMassFlowRate(1) = 0.2066;
    DXCoil(CoolCoilIndex).InletAirTemp = 25.5553;
    DXCoil(CoolCoilIndex).InletAirHumRat = 8.4682e-3;
    DXCoil(CoolCoilIndex).InletAirEnthalpy = 47259.78;

    ControlVRFIUCoil(CoolCoilIndex,
                     ZoneSysEnergyDemand(ZoneIndex).OutputRequiredToCoolingSP,
                     25.5553,
                     8.4682e-3,
                     Temp,
                     0,
                     FanSpdRatio,
                     Wout,
                     Toutlet,
                     Houtlet,
                     SHact,
                     SCact);
    EXPECT_NEAR(Toutlet, 17.89, 0.01);
    EXPECT_NEAR(Houtlet, 39440, 1);
    EXPECT_NEAR(SHact, 3.00, 0.01);

    // Run and Check for Heating Mode
    Mode = 1;
    Temp = 42;
    ZoneIndex = 2;

    ZoneSysEnergyDemand(ZoneIndex).OutputRequiredToCoolingSP = 43167.2628;
    ZoneSysEnergyDemand(ZoneIndex).OutputRequiredToHeatingSP = 4241.66099;

    DXCoil(HeatCoilIndex).RatedAirMassFlowRate(1) = 0.21136;
    DXCoil(HeatCoilIndex).InletAirTemp = 20.2362;
    DXCoil(HeatCoilIndex).InletAirHumRat = 4.1053e-3;
    DXCoil(HeatCoilIndex).InletAirEnthalpy = 30755.6253;

    ControlVRFIUCoil(HeatCoilIndex,
                     ZoneSysEnergyDemand(ZoneIndex).OutputRequiredToHeatingSP,
                     20.2362,
                     4.1053e-3,
                     Temp,
                     0,
                     FanSpdRatio,
                     Wout,
                     Toutlet,
                     Houtlet,
                     SHact,
                     SCact);
    EXPECT_NEAR(Toutlet, 38.37, 0.01);
    EXPECT_NEAR(Houtlet, 49113, 1);
    EXPECT_NEAR(SCact, 5.00, 0.01);

    // Clean up
    ZoneSysEnergyDemand.deallocate();
}

TEST_F(EnergyPlusFixture, VRF_FluidTCtrl_CalcVRFIUTeTc)
{
    // PURPOSE OF THIS TEST:
    //   Test the method CalcVRFIUTeTc_FluidTCtrl, which determines the VRF evaporating temperature at
    //   cooling mode and the condensing temperature at heating mode.

    using namespace HVACVariableRefrigerantFlow;

    // Allocate
    int NumVRFCondenser = 1;
    VRF.allocate(NumVRFCondenser);
    int NumTUList = 1;
    TerminalUnitList.allocate(NumTUList);

    // Common Inputs
    int IndexVRFCondenser = 1;
    int IndexTUList = 1;

    TerminalUnitList(IndexTUList).NumTUInList = 2;

    VRF(IndexVRFCondenser).ZoneTUListPtr = 1;
    VRF(IndexVRFCondenser).AlgorithmIUCtrl = 0;
    VRF(IndexVRFCondenser).EvapTempFixed = 3;
    VRF(IndexVRFCondenser).CondTempFixed = 5;

    // Run and Check
    VRF(IndexVRFCondenser).CalcVRFIUTeTc_FluidTCtrl();

    EXPECT_EQ(VRF(IndexVRFCondenser).IUEvaporatingTemp, 3);
    EXPECT_EQ(VRF(IndexVRFCondenser).IUCondensingTemp, 5);

    // Clean up
    VRF.deallocate();
    TerminalUnitList.deallocate();
}

//*****************VRF-SysCurve Model
TEST_F(EnergyPlusFixture, VRFTest_SysCurve)
{

    bool ErrorsFound(false);       // function returns true on error
    bool FirstHVACIteration(true); // simulate the first pass through HVAC simulation, use false for next iteration
    int VRFCond(1);                // index to VRF condenser
    int VRFTUNum(1);               // index to VRF terminal unit
    int EquipPtr(1);               // index to equipment list
    int CurZoneNum(1);             // index to zone
    int ZoneInletAirNode(0);       // zone inlet node number
    Real64 DefrostWatts(0.0);      // calculation of VRF defrost power [W]
    Real64 SysOutputProvided(0.0); // function returns sensible capacity [W]
    Real64 LatOutputProvided(0.0); // function returns latent capacity [W]

    std::string const idf_objects = delimited_string({
        "Version,8.3;",
        " ",
        "AirConditioner:VariableRefrigerantFlow,",
        "  VRF Heat Pump,           !- Heat Pump Name",
        "  VRFCondAvailSched,       !- Availability Schedule Name",
        "  autosize,                !- Gross Rated Total Cooling Capacity {W}",
        "  3.2917,                  !- Gross Rated Cooling COP {W/W}",
        "  -5,                      !- Minimum Outdoor Temperature in Cooling Mode {C}",
        "  43,                      !- Maximum Outdoor Temperature in Cooling Mode {C}",
        "  VRFCoolCapFT,            !- Cooling Capacity Ratio Modifier Function of Low Temperature Curve Name",
        "  VRFCoolCapFTBoundary,    !- Cooling Capacity Ratio Boundary Curve Name",
        "  VRFCoolCapFTHi,          !- Cooling Capacity Ratio Modifier Function of High Temperature Curve Name",
        "  VRFCoolEIRFT,            !- Cooling Energy Input Ratio Modifier Function of Low Temperature Curve Name",
        "  VRFCoolEIRFTBoundary,    !- Cooling Energy Input Ratio Boundary Curve Name",
        "  VRFCoolEIRFTHi,          !- Cooling Energy Input Ratio Modifier Function of High Temperature Curve Name",
        "  CoolingEIRLowPLR,        !- Cooling Energy Input Ratio Modifier Function of Low Part-Load Ratio Curve Name",
        "  CoolingEIRHiPLR,         !- Cooling Energy Input Ratio Modifier Function of High Part-Load Ratio Curve Name",
        "  CoolingCombRatio,        !- Cooling Combination Ratio Correction Factor Curve Name",
        "  VRFCPLFFPLR,             !- Cooling Part-Load Fraction Correlation Curve Name",
        "  autosize,                !- Gross Rated Heating Capacity {W}",
        "  ,                        !- Rated Heating Capacity Sizing Ratio {W/W}",
        "  3.5484,                  !- Gross Rated Heating COP {W/W}",
        "  -20,                     !- Minimum Outdoor Temperature in Heating Mode {C}",
        "  20,                      !- Maximum Outdoor Temperature in Heating Mode {C}",
        "  VRFHeatCapFT,            !- Heating Capacity Ratio Modifier Function of Low Temperature Curve Name",
        "  VRFHeatCapFTBoundary,    !- Heating Capacity Ratio Boundary Curve Name",
        "  VRFHeatCapFTHi,          !- Heating Capacity Ratio Modifier Function of High Temperature Curve Name",
        "  VRFHeatEIRFT,            !- Heating Energy Input Ratio Modifier Function of Low Temperature Curve Name",
        "  VRFHeatEIRFTBoundary,    !- Heating Energy Input Ratio Boundary Curve Name",
        "  VRFHeatEIRFTHi,          !- Heating Energy Input Ratio Modifier Function of High Temperature Curve Name",
        "  WetBulbTemperature,      !- Heating Performance Curve Outdoor Temperature Type",
        "  HeatingEIRLowPLR,        !- Heating Energy Input Ratio Modifier Function of Low Part-Load Ratio Curve Name",
        "  HeatingEIRHiPLR,         !- Heating Energy Input Ratio Modifier Function of High Part-Load Ratio Curve Name",
        "  HeatingCombRatio,        !- Heating Combination Ratio Correction Factor Curve Name",
        "  VRFCPLFFPLR,             !- Heating Part-Load Fraction Correlation Curve Name",
        "  0.25,                    !- Minimum Heat Pump Part-Load Ratio {dimensionless}",
        "  SPACE1-1,                !- Zone Name for Master Thermostat Location",
        "  LoadPriority,            !- Master Thermostat Priority Control Type",
        "  ,                        !- Thermostat Priority Schedule Name",
        "  VRF Heat Pump TU List,   !- Zone Terminal Unit List Name",
        "  No,                      !- Heat Pump Waste Heat Recovery",
        "  30,                      !- Equivalent Piping Length used for Piping Correction Factor in Cooling Mode {m}",
        "  10,                      !- Vertical Height used for Piping Correction Factor {m}",
        "  CoolingLengthCorrectionFactor,  !- Piping Correction Factor for Length in Cooling Mode Curve Name",
        "  -0.000386,               !- Piping Correction Factor for Height in Cooling Mode Coefficient {1/m}",
        "  30,                      !- Equivalent Piping Length used for Piping Correction Factor in Heating Mode {m}",
        "  ,                        !- Piping Correction Factor for Length in Heating Mode Curve Name",
        "  ,                        !- Piping Correction Factor for Height in Heating Mode Coefficient {1/m}",
        "  15,                      !- Crankcase Heater Power per Compressor {W}",
        "  3,                       !- Number of Compressors {dimensionless}",
        "  0.33,                    !- Ratio of Compressor Size to Total Compressor Capacity {W/W}",
        "  7,                       !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater {C}",
        "  ReverseCycle,            !- Defrost Strategy",
        "  Timed,                   !- Defrost Control",
        "  DefrostEIRSched,         !- Defrost Energy Input Ratio Modifier Function of Temperature Curve Name",
        "  ,                        !- Defrost Time Period Fraction {dimensionless}",
        "  autosize,                !- Resistive Defrost Heater Capacity {W}",
        "  7,                       !- Maximum Outdoor Dry-bulb Temperature for Defrost Operation {C}",
        "  AirCooled,               !- Condenser Type",
        "  MyVRFOANode,             !- Condenser Inlet Node Name",
        "  ,                        !- Condenser Outlet Node Name",
        "  ,                        !- Water Condenser Volume Flow Rate {m3/s}",
        "  ,                        !- Evaporative Condenser Effectiveness {dimensionless}",
        "  ,                        !- Evaporative Condenser Air Flow Rate {m3/s}",
        "  0,                       !- Evaporative Condenser Pump Rated Power Consumption {W}",
        "  ,                        !- Supply Water Storage Tank Name",
        "  0,                       !- Basin Heater Capacity {W/K}",
        "  ,                        !- Basin Heater Setpoint Temperature {C}",
        "  ,                        !- Basin Heater Operating Schedule Name",
        "  Electricity;             !- Fuel Type",
        " ",
        "Zone,",
        "  SPACE1-1,                !- Name",
        "  0,                       !- Direction of Relative North {deg}",
        "  0,                       !- X Origin {m}",
        "  0,                       !- Y Origin {m}",
        "  0,                       !- Z Origin {m}",
        "  1,                       !- Type",
        "  1,                       !- Multiplier",
        "  2.438400269,             !- Ceiling Height {m}",
        "  239.247360229;           !- Volume {m3}",
        " ",
        "ZoneHVAC:EquipmentConnections,",
        "  SPACE1-1,                !- Zone Name",
        "  SPACE1-1 Eq,             !- Zone Conditioning Equipment List Name",
        "  TU1 Outlet Node,         !- Zone Air Inlet Node or NodeList Name",
        "  TU1 Inlet Node,          !- Zone Air Exhaust Node or NodeList Name",
        "  SPACE1-1 Node,           !- Zone Air Node Name",
        "  SPACE1-1 Out Node;       !- Zone Return Air Node Name", // not used anywhere else in the example file
        " ",
        "ZoneHVAC:EquipmentList,",
        "  SPACE1-1 Eq,             !- Name",
        "  SequentialLoad,          !- Load Distribution Scheme",
        "  ZoneHVAC:TerminalUnit:VariableRefrigerantFlow,  !- Zone Equipment 1 Object Type",
        "  TU1,                     !- Zone Equipment 1 Name",
        "  1,                       !- Zone Equipment 1 Cooling Sequence",
        "  1;                       !- Zone Equipment 1 Heating or No-Load Sequence",
        " ",
        "ZoneTerminalUnitList,",
        "  VRF Heat Pump TU List,    !- Zone Terminal Unit List Name",
        "  TU1;                      !- Zone Terminal Unit Name 1",
        " ",
        "ZoneHVAC:TerminalUnit:VariableRefrigerantFlow,",
        "  TU1,                      !- Zone Terminal Unit Name",
        "  VRFAvailSched,            !- Terminal Unit Availability Schedule",
        "  TU1 Inlet Node,           !- Terminal Unit Air Inlet Node Name",
        "  TU1 Outlet Node,          !- Terminal Unit Air Outlet Node Name",
        "  autosize,                 !- Supply Air Flow Rate During Cooling Operation {m3/s}",
        "  0,                        !- Supply Air Flow Rate When No Cooling is Needed {m3/s}",
        "  autosize,                 !- Supply Air Flow Rate During Heating Operation {m3/s}",
        "  0,                        !- Supply Air Flow Rate When No Heating is Needed {m3/s}",
        "  autosize,                 !- Outdoor Air Flow Rate During Cooling Operation {m3/s}",
        "  autosize,                 !- Outdoor Air Flow Rate During Heating Operation {m3/s}",
        "  0,                        !- Outdoor Air Flow Rate When No Cooling or Heating is Needed {m3/s}",
        "  VRFFanSchedule,           !- Supply Air Fan Operating Mode Schedule Name",
        "  drawthrough,              !- Supply Air Fan Placement",
        "  Fan:OnOff,                !- Supply Air Fan Object Type",
        "  TU1 VRF Supply Fan,       !- Supply Air Fan Object Name",
        "  OutdoorAir:Mixer,         !- Outside Air Mixer Object Type",
        "  TU1 OA Mixer,             !- Outside Air Mixer Object Name",
        "  COIL:Cooling:DX:VariableRefrigerantFlow,  !- Cooling Coil Object Type",
        "  TU1 VRF DX Cooling Coil,  !- Cooling Coil Object Name",
        "  COIL:Heating:DX:VariableRefrigerantFlow,  !- Heating Coil Object Type",
        "  TU1 VRF DX Heating Coil,  !- Heating Coil Object Name",
        "  30,                       !- Zone Terminal Unit On Parasitic Electric Energy Use {W}",
        "  20;                       !- Zone Terminal Unit Off Parasitic Electric Energy Use{ W }",
        " ",
        "Fan:OnOff,",
        "  TU1 VRF Supply Fan,       !- Name",
        "  VRFAvailSched,            !- Availability Schedule Name",
        "  0.7,                      !- Fan Total Efficiency",
        "  600.0,                    !- Pressure Rise{ Pa }",
        "  autosize,                 !- Maximum Flow Rate{ m3 / s }",
        "  0.9,                      !- Motor Efficiency",
        "  1.0,                      !- Motor In Airstream Fraction",
        "  TU1 VRF DX HCoil Outlet Node, !- Air Inlet Node Name",
        "  TU1 Outlet Node;          !- Air Outlet Node Name",
        " ",
        "OutdoorAir:Mixer,",
        "  TU1 OA Mixer,             !- Name",
        "  TU1 VRF DX CCoil Inlet Node, !- Mixed Air Node Name",
        "  Outside Air Inlet Node 1, !- Outdoor Air Stream Node Name",
        "  Relief Air Outlet Node 1, !- Relief Air Stream Node Name",
        "  TU1 Inlet Node;           !- Return Air Stream Node Name",
        " ",
        "OutdoorAir:NodeList,",
        "  OutsideAirInletNodes;     !- Node or NodeList Name 1",
        " ",
        "NodeList,",
        "  OutsideAirInletNodes, !- Name",
        "  Outside Air Inlet Node 1, !- Node 1 Name",
        "  MyVRFOANode;  !- Node 1 Name",
        " ",
        "COIL:Cooling:DX:VariableRefrigerantFlow,",
        "  TU1 VRF DX Cooling Coil, !- Name",
        "  VRFAvailSched,           !- Availability Schedule Name",
        "  autosize,                !- Gross Rated Total Cooling Capacity {W}",
        "  autosize,                !- Gross Rated Sensible Heat Ratio",
        "  autosize,                !- Rated Air Flow Rate {m3/s}",
        "  VRFTUCoolCapFT,          !- Cooling Capacity Ratio Modifier Function of Temperature Curve Name",
        "  VRFACCoolCapFFF,         !- Cooling Capacity Modifier Curve Function of Flow Fraction Name",
        "  TU1 VRF DX CCoil Inlet Node,  !- Coil Air Inlet Node",
        "  TU1 VRF DX CCoil Outlet Node,  !- Coil Air Outlet Node",
        "  ;                        !- Name of Water Storage Tank for Condensate Collection",
        " ",
        "COIL:Heating:DX:VariableRefrigerantFlow,",
        "  TU1 VRF DX Heating Coil, !- Name",
        "  VRFAvailSched,           !- Availability Schedule",
        "  autosize,                !- Gross Rated Heating Capacity {W}",
        "  autosize,                !- Rated Air Flow Rate {m3/s}",
        "  TU1 VRF DX CCoil Outlet Node,  !- Coil Air Inlet Node",
        "  TU1 VRF DX HCoil Outlet Node,  !- Coil Air Outlet Node",
        "  VRFTUHeatCapFT,          !- Heating Capacity Ratio Modifier Function of Temperature Curve Name",
        "  VRFACCoolCapFFF;         !- Heating Capacity Modifier Function of Flow Fraction Curve Name",
        " ",
        "ScheduleTypeLimits,",
        "  Any Number;              !- Name",
        " ",
        "Schedule:Compact,",
        "  VRFAvailSched,           !- Name",
        "  Any Number,              !- Schedule Type Limits Name",
        "  Through: 3/31,           !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00,1.0,        !- Field 3",
        "  Through: 9/30,           !- Field 4",
        "  For: WeekDays,           !- Field 5",
        "  Until: 7:00,1.0,         !- Field 6",
        "  Until: 17:00,1.0,        !- Field 7",
        "  Until: 24:00,1.0,        !- Field 8",
        "  For: SummerDesignDay WinterDesignDay, !- Field 9",
        "  Until: 24:00,1.0,        !- Field 10",
        "  For: AllOtherDays,       !- Field 11",
        "  Until: 24:00,1.0,        !- Field 12",
        "  Through: 12/31,          !- Field 13",
        "  For: AllDays,            !- Field 14",
        "  Until: 24:00,1.0;        !- Field 15",
        " ",
        "Schedule:Compact,",
        "  VRFCondAvailSched,       !- Name",
        "  Any Number,              !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 12:00,1.0,        !- Field 3",
        "  Until: 13:00,1.0,        !- Field 4",
        "  Until: 24:00,1.0;        !- Field 5",
        " ",
        "Schedule:Compact,",
        "  VRFFanSchedule,          !- Name",
        "  Any Number,              !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 7:00,1.0,         !- Field 3",
        "  Until: 18:00,1.0,        !- Field 5",
        "  Until: 24:00,1.0;        !- Field 7",
        " ",
        "Curve:Biquadratic,",
        "  DefrostEIRSched,         !- Name",
        "  1.0,                     !- Coefficient1 Constant",
        "  0.0,                     !- Coefficient2 x",
        "  0.0,                     !- Coefficient3 x**2",
        "  0.0,                     !- Coefficient4 y",
        "  0.0,                     !- Coefficient5 y**2",
        "  0.0,                     !- Coefficient6 x*y",
        "  15,                      !- Minimum Value of x",
        "  24,                      !- Maximum Value of x",
        "  -5,                      !- Minimum Value of y",
        "  23,                      !- Maximum Value of y",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Biquadratic,",
        "  VRFCoolCapFT,            !- Name",
        "  0.576882692,             !- Coefficient1 Constant",
        "  0.017447952,             !- Coefficient2 x",
        "  0.000583269,             !- Coefficient3 x**2",
        "  -1.76324E-06,            !- Coefficient4 y",
        "  -7.474E-09,              !- Coefficient5 y**2",
        "  -1.30413E-07,            !- Coefficient6 x*y",
        "  15,                      !- Minimum Value of x",
        "  24,                      !- Maximum Value of x",
        "  -5,                      !- Minimum Value of y",
        "  23,                      !- Maximum Value of y",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Cubic,",
        "  VRFCoolCapFTBoundary,    !- Name",
        "  25.73473775,             !- Coefficient1 Constant",
        "  -0.03150043,             !- Coefficient2 x",
        "  -0.01416595,             !- Coefficient3 x**2",
        "  0,                       !- Coefficient4 x**3",
        "  11,                      !- Minimum Value of x",
        "  30,                      !- Maximum Value of x",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature;             !- Output Unit Type",
        " ",
        "Curve:Biquadratic,",
        "  VRFCoolCapFTHi,          !- Name",
        "  0.6867358,               !- Coefficient1 Constant",
        "  0.0207631,               !- Coefficient2 x",
        "  0.0005447,               !- Coefficient3 x**2",
        "  -0.0016218,              !- Coefficient4 y",
        "  -4.259E-07,              !- Coefficient5 y**2",
        "  -0.0003392,              !- Coefficient6 x*y",
        "  15,                      !- Minimum Value of x",
        "  24,                      !- Maximum Value of x",
        "  16,                      !- Minimum Value of y",
        "  43,                      !- Maximum Value of y",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Biquadratic,",
        "  VRFCoolEIRFT,            !- Name",
        "  0.989010541,             !- Coefficient1 Constant",
        "  -0.02347967,             !- Coefficient2 x",
        "  0.000199711,             !- Coefficient3 x**2",
        "  0.005968336,             !- Coefficient4 y",
        "  -1.0289E-07,             !- Coefficient5 y**2",
        "  -0.00015686,             !- Coefficient6 x*y",
        "  15,                      !- Minimum Value of x",
        "  24,                      !- Maximum Value of x",
        "  -5,                      !- Minimum Value of y",
        "  23,                      !- Maximum Value of y",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Cubic,",
        "  VRFCoolEIRFTBoundary,    !- Name",
        "  25.73473775,             !- Coefficient1 Constant",
        "  -0.03150043,             !- Coefficient2 x",
        "  -0.01416595,             !- Coefficient3 x**2",
        "  0,                       !- Coefficient4 x**3",
        "  15,                      !- Minimum Value of x",
        "  24,                      !- Maximum Value of x",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature;             !- Output Unit Type",
        " ",
        "Curve:Biquadratic,",
        "  VRFCoolEIRFTHi,          !- Name",
        "  0.14351470,              !- Coefficient1 Constant",
        "  0.01860035,              !- Coefficient2 x",
        "  -0.0003954,              !- Coefficient3 x**2",
        "  0.02485219,              !- Coefficient4 y",
        "  0.00016329,              !- Coefficient5 y**2",
        "  -0.0006244,              !- Coefficient6 x*y",
        "  15,                      !- Minimum Value of x",
        "  24,                      !- Maximum Value of x",
        "  16,                      !- Minimum Value of y",
        "  43,                      !- Maximum Value of y",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Cubic,",
        "  CoolingEIRLowPLR,        !- Name",
        "  0.4628123,               !- Coefficient1 Constant",
        "  -1.0402406,              !- Coefficient2 x",
        "  2.17490997,              !- Coefficient3 x**2",
        "  -0.5974817,              !- Coefficient4 x**3",
        "  0,                       !- Minimum Value of x",
        "  1,                       !- Maximum Value of x",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature;             !- Output Unit Type",
        " ",
        "Curve:Quadratic,",
        "  CoolingEIRHiPLR,         !- Name",
        "  1.0,                     !- Coefficient1 Constant",
        "  0.0,                     !- Coefficient2 x",
        "  0.0,                     !- Coefficient3 x**2",
        "  1.0,                     !- Minimum Value of x",
        "  1.5,                     !- Maximum Value of x",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Dimensionless,           !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Linear,",
        "  CoolingCombRatio,        !- Name",
        "  0.618055,                !- Coefficient1 Constant",
        "  0.381945,                !- Coefficient2 x",
        "  1.0,                     !- Minimum Value of x",
        "  1.5,                     !- Maximum Value of x",
        "  1.0,                     !- Minimum Curve Output",
        "  1.2,                     !- Maximum Curve Output",
        "  Dimensionless,           !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "CURVE:QUADRATIC,",
        "  VRFCPLFFPLR,             !- Name",
        "  0.85,                    !- Coefficient1 Constant",
        "  0.15,                    !- Coefficient2 x",
        "  0.0,                     !- Coefficient3 x**2",
        "  0.0,                     !- Minimum Value of x",
        "  1.0,                     !- Maximum Value of x",
        "  0.85,                    !- Minimum Curve Output",
        "  1.0,                     !- Maximum Curve Output",
        "  Dimensionless,           !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Biquadratic,",
        "  VRFHeatCapFT,            !- Name",
        "  1.014599599,             !- Coefficient1 Constant",
        "  -0.002506703,            !- Coefficient2 x",
        "  -0.000141599,            !- Coefficient3 x**2",
        "  0.026931595,             !- Coefficient4 y",
        "  1.83538E-06,             !- Coefficient5 y**2",
        "  -0.000358147,            !- Coefficient6 x*y",
        "  15,                      !- Minimum Value of x",
        "  27,                      !- Maximum Value of x",
        "  -20,                     !- Minimum Value of y",
        "  15,                      !- Maximum Value of y",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Cubic,",
        "  VRFHeatCapFTBoundary,    !- Name",
        "  -7.6000882,              !- Coefficient1 Constant",
        "  3.05090016,              !- Coefficient2 x",
        "  -0.1162844,              !- Coefficient3 x**2",
        "  0.0,                     !- Coefficient4 x**3",
        "  15,                      !- Minimum Value of x",
        "  27,                      !- Maximum Value of x",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature;             !- Output Unit Type",
        " ",
        "Curve:Biquadratic,",
        "  VRFHeatCapFTHi,          !- Name",
        "  1.161134821,             !- Coefficient1 Constant",
        "  0.027478868,             !- Coefficient2 x",
        "  -0.00168795,             !- Coefficient3 x**2",
        "  0.001783378,             !- Coefficient4 y",
        "  2.03208E-06,             !- Coefficient5 y**2",
        "  -6.8969E-05,             !- Coefficient6 x*y",
        "  15,                      !- Minimum Value of x",
        "  27,                      !- Maximum Value of x",
        "  -10,                     !- Minimum Value of y",
        "  15,                      !- Maximum Value of y",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Biquadratic,",
        "  VRFHeatEIRFT,            !- Name",
        "  0.87465501,              !- Coefficient1 Constant",
        "  -0.01319754,             !- Coefficient2 x",
        "  0.00110307,              !- Coefficient3 x**2",
        "  -0.0133118,              !- Coefficient4 y",
        "  0.00089017,              !- Coefficient5 y**2",
        "  -0.00012766,             !- Coefficient6 x*y",
        "  15,                      !- Minimum Value of x",
        "  27,                      !- Maximum Value of x",
        "  -20,                     !- Minimum Value of y",
        "  12,                      !- Maximum Value of y",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Cubic,",
        "  VRFHeatEIRFTBoundary,    !- Name",
        "  -7.6000882,              !- Coefficient1 Constant",
        "  3.05090016,              !- Coefficient2 x",
        "  -0.1162844,              !- Coefficient3 x**2",
        "  0.0,                     !- Coefficient4 x**3",
        "  15,                      !- Minimum Value of x",
        "  27,                      !- Maximum Value of x",
        "  -20,                     !- Minimum Curve Output",
        "  15,                      !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature;             !- Output Unit Type",
        " ",
        "Curve:Biquadratic,",
        "  VRFHeatEIRFTHi,          !- Name",
        "  2.504005146,             !- Coefficient1 Constant",
        "  -0.05736767,             !- Coefficient2 x",
        "  4.07336E-05,             !- Coefficient3 x**2",
        "  -0.12959669,             !- Coefficient4 y",
        "  0.00135839,              !- Coefficient5 y**2",
        "  0.00317047,              !- Coefficient6 x*y",
        "  15,                      !- Minimum Value of x",
        "  27,                      !- Maximum Value of x",
        "  -10,                     !- Minimum Value of y",
        "  15,                      !- Maximum Value of y",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Cubic,",
        "  HeatingEIRLowPLR,        !- Name",
        "  0.1400093,               !- Coefficient1 Constant",
        "  0.6415002,               !- Coefficient2 x",
        "  0.1339047,               !- Coefficient3 x**2",
        "  0.0845859,               !- Coefficient4 x**3",
        "  0,                       !- Minimum Value of x",
        "  1,                       !- Maximum Value of x",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Dimensionless,           !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Quadratic,",
        "  HeatingEIRHiPLR,         !- Name",
        "  2.4294355,               !- Coefficient1 Constant",
        "  -2.235887,               !- Coefficient2 x",
        "  0.8064516,               !- Coefficient3 x**2",
        "  1.0,                     !- Minimum Value of x",
        "  1.5,                     !- Maximum Value of x",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Dimensionless,           !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Linear,",
        "  HeatingCombRatio,        !- Name",
        "  0.96034,                 !- Coefficient1 Constant",
        "  0.03966,                 !- Coefficient2 x",
        "  1.0,                     !- Minimum Value of x",
        "  1.5,                     !- Maximum Value of x",
        "  1.0,                     !- Minimum Curve Output",
        "  1.023,                   !- Maximum Curve Output",
        "  Dimensionless,           !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Biquadratic,",
        "  CoolingLengthCorrectionFactor,  !- Name",
        "  1.0693794,               !- Coefficient1 Constant",
        "  -0.0014951,              !- Coefficient2 x",
        "  2.56E-06,                !- Coefficient3 x**2",
        "  -0.1151104,              !- Coefficient4 y",
        "  0.0511169,               !- Coefficient5 y**2",
        "  -0.0004369,              !- Coefficient6 x*y",
        "  8,                       !- Minimum Value of x",
        "  175,                     !- Maximum Value of x",
        "  0.5,                     !- Minimum Value of y",
        "  1.5,                     !- Maximum Value of y",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Cubic,",
        "  VRFTUCoolCapFT,          !- Name",
        "  0.504547273506488,       !- Coefficient1 Constant",
        "  0.0288891279198444,      !- Coefficient2 x",
        "  -0.000010819418650677,   !- Coefficient3 x**2",
        "  0.0000101359395177008,   !- Coefficient4 x**3",
        "  0.0,                     !- Minimum Value of x",
        "  50.0,                    !- Maximum Value of x",
        "  0.5,                     !- Minimum Curve Output",
        "  1.5,                     !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Quadratic,",
        "  VRFACCoolCapFFF,         !- Name",
        "  0.8,                     !- Coefficient1 Constant",
        "  0.2,                     !- Coefficient2 x",
        "  0.0,                     !- Coefficient3 x**2",
        "  0.5,                     !- Minimum Value of x",
        "  1.5;                     !- Maximum Value of x",
        " ",
        "Curve:Cubic,",
        "  VRFTUHeatCapFT,          !- Name",
        "  -0.390708928227928,      !- Coefficient1 Constant",
        "  0.261815023760162,       !- Coefficient2 x",
        "  -0.0130431603151873,     !- Coefficient3 x**2",
        "  0.000178131745997821,    !- Coefficient4 x**3",
        "  0.0,                     !- Minimum Value of x",
        "  50.0,                    !- Maximum Value of x",
        "  0.5,                     !- Minimum Curve Output",
        "  1.5,                     !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    DataGlobals::BeginEnvrnFlag = true;
    DataSizing::CurZoneEqNum = 1;
    DataEnvironment::OutBaroPress = 101325;          // sea level
    DataZoneEquipment::ZoneEquipInputsFilled = true; // denotes zone equipment has been read in
    StdRhoAir = PsyRhoAirFnPbTdbW(DataEnvironment::OutBaroPress, 20.0, 0.0);
    ZoneEqSizing.allocate(1);
    ZoneSizingRunDone = true;
    ZoneEqSizing(CurZoneEqNum).DesignSizeFromParent = false;
    ZoneEqSizing(CurZoneEqNum).SizingMethod.allocate(25);
    ZoneEqSizing(CurZoneEqNum).SizingMethod(DataHVACGlobals::SystemAirflowSizing) = DataSizing::SupplyAirFlowRate;

    FinalZoneSizing.allocate(1);
    FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow = 0.566337; // 400 cfm * 3 tons = 1200 cfm
    FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow = 0.566337;

    ZoneSysEnergyDemand.allocate(1);

    ProcessScheduleInput();   // read schedules
    GetCurveInput();          // read curves
    GetZoneData(ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);

    DXCoils::GetCoilsInputFlag = true; // remove this when clear_state gets added to DXCoils
    GlobalNames::NumCoils = 0;         // remove this when clear_state gets added to GlobalNames
    GlobalNames::CoilNames.clear();    // remove this when clear_state gets added to GlobalNames

    GetZoneEquipmentData();                                // read equipment list and connections
    ZoneInletAirNode = GetVRFTUZoneInletAirNode(VRFTUNum); // trigger GetVRFInput by calling a mining function

    Schedule(VRF(VRFCond).SchedPtr).CurrentValue = 1.0;             // enable the VRF condenser
    Schedule(VRFTU(VRFTUNum).SchedPtr).CurrentValue = 1.0;          // enable the terminal unit
    Schedule(VRFTU(VRFTUNum).FanAvailSchedPtr).CurrentValue = 1.0;  // turn on fan
    Schedule(VRFTU(VRFTUNum).FanOpModeSchedPtr).CurrentValue = 0.0; // set cycling fan operating mode

    // Test coil sizing

    ZoneSysEnergyDemand(CurZoneNum).RemainingOutputRequired = 0.0; // set load = 0
    ZoneSysEnergyDemand(CurZoneNum).RemainingOutputReqToCoolSP = 0.0;
    ZoneSysEnergyDemand(CurZoneNum).RemainingOutputReqToHeatSP = 0.0;

    FinalZoneSizing(CurZoneEqNum).ZoneRetTempAtCoolPeak = 26.66667;
    FinalZoneSizing(CurZoneEqNum).ZoneHumRatAtCoolPeak = 0.01117049470250416; // AHRI condition at 80 F db / 67 F wb
    Node(VRF(VRFCond).CondenserNodeNum).Temp = 35.0;                          // AHRI condition at 95 F db
    Node(VRFTU(VRFTUNum).VRFTUOAMixerOANodeNum).Temp = 35.0;                  // AHRI condition at 95 F db
    Node(VRFTU(VRFTUNum).VRFTUOAMixerOANodeNum).HumRat = 0.01;                // don't care
    FinalZoneSizing(CurZoneEqNum).CoolDDNum = 1;
    FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax = 1;
    DesDayWeath.allocate(1);
    DesDayWeath(1).Temp.allocate(1);
    DesDayWeath(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Temp(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax) = 35.0;

    FinalZoneSizing(CurZoneEqNum).CoolDesTemp = 13.1;                   // 55.58 F
    FinalZoneSizing(CurZoneEqNum).CoolDesHumRat = 0.009297628698818194; // humrat at 12.77777 C db / 12.6 C wb

    SimulateVRF(
        VRFTU(VRFTUNum).Name, CurZoneNum, FirstHVACIteration, SysOutputProvided, LatOutputProvided, ZoneEquipList(CurZoneEqNum).EquipIndex(EquipPtr));

    ASSERT_EQ(1, NumVRFCond);
    ASSERT_EQ(ZoneInletAirNode,
              ZoneEquipConfig(VRFTU(VRFTUNum).ZoneNum).InletNode(1)); // only 1 inlet node specified above in ZoneHVAC:EquipmentConnections
    ASSERT_EQ(1.0, VRF(VRFCond).CoolingCombinationRatio);
    EXPECT_NEAR(11176.29, VRF(VRFCond).CoolingCapacity, 0.01);
    EXPECT_NEAR(11176.29, VRF(VRFCond).HeatingCapacity, 0.01);
    EXPECT_EQ(0.0, VRF(VRFCond).DefrostPower);

    // test defrost operation Issue #4950 - Reverse cycle with timed defrost = 0

    // set OA node temperatures for heating where defrost should be active
    Node(VRF(VRFCond).CondenserNodeNum).Temp = VRF(VRFCond).MaxOATDefrost - 1.0;
    Node(VRF(VRFCond).CondenserNodeNum).HumRat = 0.005;
    Node(VRFTU(VRFTUNum).VRFTUOAMixerOANodeNum).Temp = Node(VRF(VRFCond).CondenserNodeNum).Temp;

    // set zone load to heating
    ZoneSysEnergyDemand(CurZoneNum).RemainingOutputRequired = VRF(VRFCond).HeatingCapacity; // set load equal to the VRF heating capacity
    ZoneSysEnergyDemand(CurZoneNum).RemainingOutputReqToCoolSP =
        VRF(VRFCond).HeatingCapacity + 1000.0; // simulates a dual Tstat with load to cooling SP > load to heating SP
    ZoneSysEnergyDemand(CurZoneNum).RemainingOutputReqToHeatSP = VRF(VRFCond).HeatingCapacity;

    SimulateVRF(
        VRFTU(VRFTUNum).Name, CurZoneNum, FirstHVACIteration, SysOutputProvided, LatOutputProvided, ZoneEquipList(CurZoneEqNum).EquipIndex(EquipPtr));

    ASSERT_TRUE(VRF(VRFCond).DefrostPower > 0.0); // defrost power should be greater than 0
    DefrostWatts = VRF(VRFCond).VRFCondRTF * (VRF(VRFCond).HeatingCapacity / 1.01667) * VRF(VRFCond).DefrostFraction;
    ASSERT_EQ(DefrostWatts, VRF(VRFCond).DefrostPower); // defrost power calculation check

    // test other ThermostatPriority control types
    ZoneThermostatSetPointHi.allocate(1);
    ZoneThermostatSetPointHi = 24.0;
    ZoneThermostatSetPointLo.allocate(1);
    ZoneThermostatSetPointLo = 21.0;
    TempControlType.allocate(1);
    TempControlType = 4;
    ZT.allocate(1);
    ZT = 25.0;

    ZoneSysEnergyDemand(CurZoneNum).RemainingOutputRequired =
        -VRF(VRFCond).CoolingCapacity * 0.75; // set load equal to the VRF cooling capacity adjusted for SHR
    ZoneSysEnergyDemand(CurZoneNum).RemainingOutputReqToCoolSP = -VRF(VRFCond).CoolingCapacity * 0.75;
    ZoneSysEnergyDemand(CurZoneNum).RemainingOutputReqToHeatSP = -VRF(VRFCond).CoolingCapacity * 0.75 - 1000.0;

    Node(VRF(VRFCond).CondenserNodeNum).Temp = 35.0;     // AHRI condition at 95 F db
    Node(VRFTU(VRFTUNum).VRFTUInletNodeNum).Temp = 27.0; // some zone return condition
    Node(VRFTU(VRFTUNum).VRFTUInletNodeNum).HumRat = 0.011;
    Node(VRFTU(VRFTUNum).VRFTUInletNodeNum).Enthalpy = 55194.0; // VRF DX coil model uses node enthalpy
    Node(VRFTU(VRFTUNum).VRFTUOAMixerOANodeNum).Temp = 35.0;    // AHRI condition at 95 F db
    Node(VRFTU(VRFTUNum).VRFTUOAMixerOANodeNum).HumRat = 0.008; // don't care
    Node(VRFTU(VRFTUNum).VRFTUOAMixerOANodeNum).Enthalpy = 55698.0;
    Node(VRFTU(VRFTUNum).VRFTUOAMixerOANodeNum).OutAirWetBulb = 19.652;

    VRF(VRFCond).MasterZonePtr = 0;
    VRF(VRFCond).MasterZoneTUIndex = 0;
    VRF(VRFCond).ThermostatPriority = ThermostatOffsetPriority;
    SimulateVRF(
        VRFTU(VRFTUNum).Name, CurZoneNum, FirstHVACIteration, SysOutputProvided, LatOutputProvided, ZoneEquipList(CurZoneEqNum).EquipIndex(EquipPtr));
    EXPECT_NEAR(SysOutputProvided,
                ZoneSysEnergyDemand(CurZoneNum).RemainingOutputRequired,
                5.0); // system output should be less than 0 and approx = to VRF capacity * SHR

    // ensure that TU turns off when fan heat exceeds the heating load
    ZT = 20.0;                                       // set zone temp below heating SP (SP=21) to ensure heating mode
    Node(VRF(VRFCond).CondenserNodeNum).Temp = 19.0; // within the heating temperature range of VRF outdoor unit
    Node(VRFTU(VRFTUNum).VRFTUOAMixerOANodeNum).Temp = 19.0;
    ZoneSysEnergyDemand(CurZoneNum).RemainingOutputRequired = 400.0; // set load equal to small value less than expected fan heat
    ZoneSysEnergyDemand(CurZoneNum).RemainingOutputReqToCoolSP = 500.0;
    ZoneSysEnergyDemand(CurZoneNum).RemainingOutputReqToHeatSP = 400.0;
    Schedule(VRFTU(VRFTUNum).FanOpModeSchedPtr).CurrentValue = 1.0; // set constant fan operating mode
    SimulateVRF(
        VRFTU(VRFTUNum).Name, CurZoneNum, FirstHVACIteration, SysOutputProvided, LatOutputProvided, ZoneEquipList(CurZoneEqNum).EquipIndex(EquipPtr));
    EXPECT_EQ(SysOutputProvided,
              0.0); // for this system with 0 no load flow rate output should be = 0 when fan heat at very low TU PLR (1E-20) is greater than load
    EXPECT_EQ(VRF(VRFCond).VRFCondPLR, 0.0); // system should be off

    // ensure that TU operates when fan heat does not exceed the heating load
    ZT = 20.0;                                       // set zone temp below heating SP (SP=21) to ensure heating mode
    Node(VRF(VRFCond).CondenserNodeNum).Temp = 19.0; // within the heating temperature range of VRF outdoor unit
    Node(VRFTU(VRFTUNum).VRFTUOAMixerOANodeNum).Temp = 19.0;
    ZoneSysEnergyDemand(CurZoneNum).RemainingOutputRequired = 800.0; // set load equal to small value less than expected fan heat
    ZoneSysEnergyDemand(CurZoneNum).RemainingOutputReqToCoolSP = 900.0;
    ZoneSysEnergyDemand(CurZoneNum).RemainingOutputReqToHeatSP = 800.0;
    Schedule(VRFTU(VRFTUNum).FanOpModeSchedPtr).CurrentValue = 1.0; // set constant fan operating mode
    SimulateVRF(
        VRFTU(VRFTUNum).Name, CurZoneNum, FirstHVACIteration, SysOutputProvided, LatOutputProvided, ZoneEquipList(CurZoneEqNum).EquipIndex(EquipPtr));
    EXPECT_NEAR(SysOutputProvided, ZoneSysEnergyDemand(CurZoneNum).RemainingOutputRequired, 5.0); // system should meet the heating load
    EXPECT_GT(VRF(VRFCond).VRFCondPLR, 0.0);                                                      // system should be on
}

TEST_F(EnergyPlusFixture, VRFTest_SysCurve_GetInputFailers)
{
    // Author: R. Raustad, FSEC

    bool ErrorsFound(false); // function returns true on error
    int VRFTUNum(1);         // index to VRF terminal unit

    std::string const idf_objects = delimited_string({
        "Version,8.3;",
        " ",
        "AirConditioner:VariableRefrigerantFlow,",
        "  VRF Heat Pump,           !- Heat Pump Name",
        "  VRFCondAvailSched,       !- Availability Schedule Name",
        "  autosize,                !- Gross Rated Total Cooling Capacity {W}",
        "  3.2917,                  !- Gross Rated Cooling COP {W/W}",
        "  -5,                      !- Minimum Outdoor Temperature in Cooling Mode {C}",
        "  43,                      !- Maximum Outdoor Temperature in Cooling Mode {C}",
        "  VRFCoolCapFT,            !- Cooling Capacity Ratio Modifier Function of Low Temperature Curve Name",
        "  VRFCoolCapFTBoundary,    !- Cooling Capacity Ratio Boundary Curve Name",
        "  VRFCoolCapFTHi,          !- Cooling Capacity Ratio Modifier Function of High Temperature Curve Name",
        "  VRFCoolEIRFT,            !- Cooling Energy Input Ratio Modifier Function of Low Temperature Curve Name",
        "  VRFCoolEIRFTBoundary,    !- Cooling Energy Input Ratio Boundary Curve Name",
        "  VRFCoolEIRFTHi,          !- Cooling Energy Input Ratio Modifier Function of High Temperature Curve Name",
        "  CoolingEIRLowPLR,        !- Cooling Energy Input Ratio Modifier Function of Low Part-Load Ratio Curve Name",
        "  CoolingEIRHiPLR,         !- Cooling Energy Input Ratio Modifier Function of High Part-Load Ratio Curve Name",
        "  CoolingCombRatio,        !- Cooling Combination Ratio Correction Factor Curve Name",
        "  VRFCPLFFPLR,             !- Cooling Part-Load Fraction Correlation Curve Name",
        "  autosize,                !- Gross Rated Heating Capacity {W}",
        "  ,                        !- Rated Heating Capacity Sizing Ratio {W/W}",
        "  3.5484,                  !- Gross Rated Heating COP {W/W}",
        "  -20,                     !- Minimum Outdoor Temperature in Heating Mode {C}",
        "  20,                      !- Maximum Outdoor Temperature in Heating Mode {C}",
        "  VRFHeatCapFT,            !- Heating Capacity Ratio Modifier Function of Low Temperature Curve Name",
        "  VRFHeatCapFTBoundary,    !- Heating Capacity Ratio Boundary Curve Name",
        "  VRFHeatCapFTHi,          !- Heating Capacity Ratio Modifier Function of High Temperature Curve Name",
        "  VRFHeatEIRFT,            !- Heating Energy Input Ratio Modifier Function of Low Temperature Curve Name",
        "  VRFHeatEIRFTBoundary,    !- Heating Energy Input Ratio Boundary Curve Name",
        "  VRFHeatEIRFTHi,          !- Heating Energy Input Ratio Modifier Function of High Temperature Curve Name",
        "  WetBulbTemperature,      !- Heating Performance Curve Outdoor Temperature Type",
        "  HeatingEIRLowPLR,        !- Heating Energy Input Ratio Modifier Function of Low Part-Load Ratio Curve Name",
        "  HeatingEIRHiPLR,         !- Heating Energy Input Ratio Modifier Function of High Part-Load Ratio Curve Name",
        "  HeatingCombRatio,        !- Heating Combination Ratio Correction Factor Curve Name",
        "  VRFCPLFFPLR,             !- Heating Part-Load Fraction Correlation Curve Name",
        "  0.25,                    !- Minimum Heat Pump Part-Load Ratio {dimensionless}",
        "  SPACE1-1,                !- Zone Name for Master Thermostat Location",
        "  LoadPriority,            !- Master Thermostat Priority Control Type",
        "  ,                        !- Thermostat Priority Schedule Name",
        "  VRF Heat Pump TU List,   !- Zone Terminal Unit List Name",
        "  No,                      !- Heat Pump Waste Heat Recovery",
        "  30,                      !- Equivalent Piping Length used for Piping Correction Factor in Cooling Mode {m}",
        "  10,                      !- Vertical Height used for Piping Correction Factor {m}",
        "  CoolingLengthCorrectionFactor,  !- Piping Correction Factor for Length in Cooling Mode Curve Name",
        "  -0.000386,               !- Piping Correction Factor for Height in Cooling Mode Coefficient {1/m}",
        "  30,                      !- Equivalent Piping Length used for Piping Correction Factor in Heating Mode {m}",
        "  ,                        !- Piping Correction Factor for Length in Heating Mode Curve Name",
        "  ,                        !- Piping Correction Factor for Height in Heating Mode Coefficient {1/m}",
        "  15,                      !- Crankcase Heater Power per Compressor {W}",
        "  3,                       !- Number of Compressors {dimensionless}",
        "  0.33,                    !- Ratio of Compressor Size to Total Compressor Capacity {W/W}",
        "  7,                       !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater {C}",
        "  ReverseCycle,            !- Defrost Strategy",
        "  Timed,                   !- Defrost Control",
        "  DefrostEIRSched,         !- Defrost Energy Input Ratio Modifier Function of Temperature Curve Name",
        "  ,                        !- Defrost Time Period Fraction {dimensionless}",
        "  autosize,                !- Resistive Defrost Heater Capacity {W}",
        "  7,                       !- Maximum Outdoor Dry-bulb Temperature for Defrost Operation {C}",
        "  AirCooled,               !- Condenser Type",
        "  MyVRFOANode,             !- Condenser Inlet Node Name",
        "  ,                        !- Condenser Outlet Node Name",
        "  ,                        !- Water Condenser Volume Flow Rate {m3/s}",
        "  ,                        !- Evaporative Condenser Effectiveness {dimensionless}",
        "  ,                        !- Evaporative Condenser Air Flow Rate {m3/s}",
        "  0,                       !- Evaporative Condenser Pump Rated Power Consumption {W}",
        "  ,                        !- Supply Water Storage Tank Name",
        "  0,                       !- Basin Heater Capacity {W/K}",
        "  ,                        !- Basin Heater Setpoint Temperature {C}",
        "  ,                        !- Basin Heater Operating Schedule Name",
        "  Electricity;             !- Fuel Type",
        " ",
        "Zone,",
        "  SPACE1-1,                !- Name",
        "  0,                       !- Direction of Relative North {deg}",
        "  0,                       !- X Origin {m}",
        "  0,                       !- Y Origin {m}",
        "  0,                       !- Z Origin {m}",
        "  1,                       !- Type",
        "  1,                       !- Multiplier",
        "  2.438400269,             !- Ceiling Height {m}",
        "  239.247360229;           !- Volume {m3}",
        " ",
        "ZoneHVAC:EquipmentConnections,",
        "  SPACE1-1,                !- Zone Name",
        "  SPACE1-1 Eq,             !- Zone Conditioning Equipment List Name",
        "  TU1 Outlet Node,         !- Zone Air Inlet Node or NodeList Name",
        "  TU1 Inlet Node,          !- Zone Air Exhaust Node or NodeList Name",
        "  SPACE1-1 Node,           !- Zone Air Node Name",
        "  SPACE1-1 Out Node;       !- Zone Return Air Node Name", // not used anywhere else in the example file
        " ",
        "ZoneHVAC:EquipmentList,",
        "  SPACE1-1 Eq,             !- Name",
        "  SequentialLoad,          !- Load Distribution Scheme",
        "  ZoneHVAC:TerminalUnit:VariableRefrigerantFlow,  !- Zone Equipment 1 Object Type",
        "  TU1,                     !- Zone Equipment 1 Name",
        "  1,                       !- Zone Equipment 1 Cooling Sequence",
        "  1;                       !- Zone Equipment 1 Heating or No-Load Sequence",
        " ",
        "ZoneTerminalUnitList,",
        "  VRF Heat Pump TU List,    !- Zone Terminal Unit List Name",
        "  TU 1;                     !- Zone Terminal Unit Name 1", // different terminal unit name than specified in
                                                                    // ZoneHVAC:TerminalUnit:VariableRefrigerantFlow
        " ",
        "ZoneHVAC:TerminalUnit:VariableRefrigerantFlow,",
        "  TU1,                      !- Zone Terminal Unit Name",
        "  VRFAvailSched,            !- Terminal Unit Availability Schedule",
        "  TU1 Inlet Node,           !- Terminal Unit Air Inlet Node Name",
        "  TU1 Outlet Node,          !- Terminal Unit Air Outlet Node Name",
        "  autosize,                 !- Supply Air Flow Rate During Cooling Operation {m3/s}",
        "  autosize,                 !- Supply Air Flow Rate When No Cooling is Needed {m3/s}",
        "  autosize,                 !- Supply Air Flow Rate During Heating Operation {m3/s}",
        "  autosize,                 !- Supply Air Flow Rate When No Heating is Needed {m3/s}",
        "  autosize,                 !- Outdoor Air Flow Rate During Cooling Operation {m3/s}",
        "  autosize,                 !- Outdoor Air Flow Rate During Heating Operation {m3/s}",
        "  autosize,                 !- Outdoor Air Flow Rate When No Cooling or Heating is Needed {m3/s}",
        "  VRFFanSchedule,           !- Supply Air Fan Operating Mode Schedule Name",
        "  drawthrough,              !- Supply Air Fan Placement",
        "  Fan:ConstantVolume,       !- Supply Air Fan Object Type",
        "  TU1 VRF Supply Fan,       !- Supply Air Fan Object Name",
        "  OutdoorAir:Mixer,         !- Outside Air Mixer Object Type",
        "  TU1 OA Mixer,             !- Outside Air Mixer Object Name",
        "  COIL:Cooling:DX:VariableRefrigerantFlow,  !- Cooling Coil Object Type",
        "  TU1 VRF DX Cooling Coil,  !- Cooling Coil Object Name",
        "  COIL:Heating:DX:VariableRefrigerantFlow,  !- Heating Coil Object Type",
        "  TU1 VRF DX Heating Coil,  !- Heating Coil Object Name",
        "  30,                       !- Zone Terminal Unit On Parasitic Electric Energy Use {W}",
        "  20;                       !- Zone Terminal Unit Off Parasitic Electric Energy Use{ W }",
        " ",
        "Fan:ConstantVolume,",
        "  TU1 VRF Supply Fan,       !- Name",
        "  VRFAvailSched,            !- Availability Schedule Name",
        "  0.7,                      !- Fan Total Efficiency",
        "  600.0,                    !- Pressure Rise{ Pa }",
        "  autosize,                 !- Maximum Flow Rate{ m3 / s }",
        "  0.9,                      !- Motor Efficiency",
        "  1.0,                      !- Motor In Airstream Fraction",
        "  TU1 VRF DX HCoil Outlet Node, !- Air Inlet Node Name",
        "  TU1 Outlet Node;          !- Air Outlet Node Name",
        " ",
        "OutdoorAir:Mixer,",
        "  TU1 OA Mixer,             !- Name",
        "  TU1 VRF DX CCoil Inlet Node, !- Mixed Air Node Name",
        "  Outside Air Inlet Node 1, !- Outdoor Air Stream Node Name",
        "  Relief Air Outlet Node 1, !- Relief Air Stream Node Name",
        "  TU1 Inlet Node;           !- Return Air Stream Node Name",
        " ",
        "OutdoorAir:NodeList,",
        "  OutsideAirInletNodes;     !- Node or NodeList Name 1",
        " ",
        "NodeList,",
        "  OutsideAirInletNodes, !- Name",
        "  Outside Air Inlet Node 1, !- Node 1 Name",
        "  MyVRFOANode;  !- Node 1 Name",
        " ",
        "COIL:Cooling:DX:VariableRefrigerantFlow,",
        "  TU1 VRF DX Cooling Coil, !- Name",
        "  VRFAvailSched,           !- Availability Schedule Name",
        "  autosize,                !- Gross Rated Total Cooling Capacity {W}",
        "  autosize,                !- Gross Rated Sensible Heat Ratio",
        "  autosize,                !- Rated Air Flow Rate {m3/s}",
        "  VRFTUCoolCapFT,          !- Cooling Capacity Ratio Modifier Function of Temperature Curve Name",
        "  VRFACCoolCapFFF,         !- Cooling Capacity Modifier Curve Function of Flow Fraction Name",
        "  TU1 VRF DX CCoil Inlet Node,  !- Coil Air Inlet Node",
        "  TU1 VRF DX CCoil Outlet Node,  !- Coil Air Outlet Node",
        "  ;                        !- Name of Water Storage Tank for Condensate Collection",
        " ",
        "COIL:Heating:DX:VariableRefrigerantFlow,",
        "  TU1 VRF DX Heating Coil, !- Name",
        "  VRFAvailSched,           !- Availability Schedule",
        "  autosize,                !- Gross Rated Heating Capacity {W}",
        "  autosize,                !- Rated Air Flow Rate {m3/s}",
        "  TU1 VRF DX CCoil Outlet Node,  !- Coil Air Inlet Node",
        "  TU1 VRF DX HCoil Outlet Node,  !- Coil Air Outlet Node",
        "  VRFTUHeatCapFT,          !- Heating Capacity Ratio Modifier Function of Temperature Curve Name",
        "  VRFACCoolCapFFF;         !- Heating Capacity Modifier Function of Flow Fraction Curve Name",
        " ",
        "ScheduleTypeLimits,",
        "  Any Number;              !- Name",
        " ",
        "Schedule:Compact,",
        "  VRFAvailSched,           !- Name",
        "  Any Number,              !- Schedule Type Limits Name",
        "  Through: 3/31,           !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00,1.0,        !- Field 3",
        "  Through: 9/30,           !- Field 4",
        "  For: WeekDays,           !- Field 5",
        "  Until: 7:00,1.0,         !- Field 6",
        "  Until: 17:00,1.0,        !- Field 7",
        "  Until: 24:00,1.0,        !- Field 8",
        "  For: SummerDesignDay WinterDesignDay, !- Field 9",
        "  Until: 24:00,1.0,        !- Field 10",
        "  For: AllOtherDays,       !- Field 11",
        "  Until: 24:00,1.0,        !- Field 12",
        "  Through: 12/31,          !- Field 13",
        "  For: AllDays,            !- Field 14",
        "  Until: 24:00,1.0;        !- Field 15",
        " ",
        "Schedule:Compact,",
        "  VRFCondAvailSched,       !- Name",
        "  Any Number,              !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 12:00,1.0,        !- Field 3",
        "  Until: 13:00,1.0,        !- Field 4",
        "  Until: 24:00,1.0;        !- Field 5",
        " ",
        "Schedule:Compact,",
        "  VRFFanSchedule,          !- Name",
        "  Any Number,              !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 7:00,1.0,         !- Field 3",
        "  Until: 18:00,1.0,        !- Field 5",
        "  Until: 24:00,1.0;        !- Field 7",
        " ",
        "Curve:Biquadratic,",
        "  DefrostEIRSched,         !- Name",
        "  1.0,                     !- Coefficient1 Constant",
        "  0.0,                     !- Coefficient2 x",
        "  0.0,                     !- Coefficient3 x**2",
        "  0.0,                     !- Coefficient4 y",
        "  0.0,                     !- Coefficient5 y**2",
        "  0.0,                     !- Coefficient6 x*y",
        "  15,                      !- Minimum Value of x",
        "  24,                      !- Maximum Value of x",
        "  -5,                      !- Minimum Value of y",
        "  23,                      !- Maximum Value of y",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Biquadratic,",
        "  VRFCoolCapFT,            !- Name",
        "  0.576882692,             !- Coefficient1 Constant",
        "  0.017447952,             !- Coefficient2 x",
        "  0.000583269,             !- Coefficient3 x**2",
        "  -1.76324E-06,            !- Coefficient4 y",
        "  -7.474E-09,              !- Coefficient5 y**2",
        "  -1.30413E-07,            !- Coefficient6 x*y",
        "  15,                      !- Minimum Value of x",
        "  24,                      !- Maximum Value of x",
        "  -5,                      !- Minimum Value of y",
        "  23,                      !- Maximum Value of y",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Cubic,",
        "  VRFCoolCapFTBoundary,    !- Name",
        "  25.73473775,             !- Coefficient1 Constant",
        "  -0.03150043,             !- Coefficient2 x",
        "  -0.01416595,             !- Coefficient3 x**2",
        "  0,                       !- Coefficient4 x**3",
        "  11,                      !- Minimum Value of x",
        "  30,                      !- Maximum Value of x",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature;             !- Output Unit Type",
        " ",
        "Curve:Biquadratic,",
        "  VRFCoolCapFTHi,          !- Name",
        "  0.6867358,               !- Coefficient1 Constant",
        "  0.0207631,               !- Coefficient2 x",
        "  0.0005447,               !- Coefficient3 x**2",
        "  -0.0016218,              !- Coefficient4 y",
        "  -4.259E-07,              !- Coefficient5 y**2",
        "  -0.0003392,              !- Coefficient6 x*y",
        "  15,                      !- Minimum Value of x",
        "  24,                      !- Maximum Value of x",
        "  16,                      !- Minimum Value of y",
        "  43,                      !- Maximum Value of y",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Biquadratic,",
        "  VRFCoolEIRFT,            !- Name",
        "  0.989010541,             !- Coefficient1 Constant",
        "  -0.02347967,             !- Coefficient2 x",
        "  0.000199711,             !- Coefficient3 x**2",
        "  0.005968336,             !- Coefficient4 y",
        "  -1.0289E-07,             !- Coefficient5 y**2",
        "  -0.00015686,             !- Coefficient6 x*y",
        "  15,                      !- Minimum Value of x",
        "  24,                      !- Maximum Value of x",
        "  -5,                      !- Minimum Value of y",
        "  23,                      !- Maximum Value of y",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Cubic,",
        "  VRFCoolEIRFTBoundary,    !- Name",
        "  25.73473775,             !- Coefficient1 Constant",
        "  -0.03150043,             !- Coefficient2 x",
        "  -0.01416595,             !- Coefficient3 x**2",
        "  0,                       !- Coefficient4 x**3",
        "  15,                      !- Minimum Value of x",
        "  24,                      !- Maximum Value of x",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature;             !- Output Unit Type",
        " ",
        "Curve:Biquadratic,",
        "  VRFCoolEIRFTHi,          !- Name",
        "  0.14351470,              !- Coefficient1 Constant",
        "  0.01860035,              !- Coefficient2 x",
        "  -0.0003954,              !- Coefficient3 x**2",
        "  0.02485219,              !- Coefficient4 y",
        "  0.00016329,              !- Coefficient5 y**2",
        "  -0.0006244,              !- Coefficient6 x*y",
        "  15,                      !- Minimum Value of x",
        "  24,                      !- Maximum Value of x",
        "  16,                      !- Minimum Value of y",
        "  43,                      !- Maximum Value of y",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Cubic,",
        "  CoolingEIRLowPLR,        !- Name",
        "  0.4628123,               !- Coefficient1 Constant",
        "  -1.0402406,              !- Coefficient2 x",
        "  2.17490997,              !- Coefficient3 x**2",
        "  -0.5974817,              !- Coefficient4 x**3",
        "  0,                       !- Minimum Value of x",
        "  1,                       !- Maximum Value of x",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature;             !- Output Unit Type",
        " ",
        "Curve:Quadratic,",
        "  CoolingEIRHiPLR,         !- Name",
        "  1.0,                     !- Coefficient1 Constant",
        "  0.0,                     !- Coefficient2 x",
        "  0.0,                     !- Coefficient3 x**2",
        "  1.0,                     !- Minimum Value of x",
        "  1.5,                     !- Maximum Value of x",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Dimensionless,           !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Linear,",
        "  CoolingCombRatio,        !- Name",
        "  0.618055,                !- Coefficient1 Constant",
        "  0.381945,                !- Coefficient2 x",
        "  1.0,                     !- Minimum Value of x",
        "  1.5,                     !- Maximum Value of x",
        "  1.0,                     !- Minimum Curve Output",
        "  1.2,                     !- Maximum Curve Output",
        "  Dimensionless,           !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "CURVE:QUADRATIC,",
        "  VRFCPLFFPLR,             !- Name",
        "  0.85,                    !- Coefficient1 Constant",
        "  0.15,                    !- Coefficient2 x",
        "  0.0,                     !- Coefficient3 x**2",
        "  0.0,                     !- Minimum Value of x",
        "  1.0,                     !- Maximum Value of x",
        "  0.85,                    !- Minimum Curve Output",
        "  1.0,                     !- Maximum Curve Output",
        "  Dimensionless,           !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Biquadratic,",
        "  VRFHeatCapFT,            !- Name",
        "  1.014599599,             !- Coefficient1 Constant",
        "  -0.002506703,            !- Coefficient2 x",
        "  -0.000141599,            !- Coefficient3 x**2",
        "  0.026931595,             !- Coefficient4 y",
        "  1.83538E-06,             !- Coefficient5 y**2",
        "  -0.000358147,            !- Coefficient6 x*y",
        "  15,                      !- Minimum Value of x",
        "  27,                      !- Maximum Value of x",
        "  -20,                     !- Minimum Value of y",
        "  15,                      !- Maximum Value of y",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Cubic,",
        "  VRFHeatCapFTBoundary,    !- Name",
        "  -7.6000882,              !- Coefficient1 Constant",
        "  3.05090016,              !- Coefficient2 x",
        "  -0.1162844,              !- Coefficient3 x**2",
        "  0.0,                     !- Coefficient4 x**3",
        "  15,                      !- Minimum Value of x",
        "  27,                      !- Maximum Value of x",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature;             !- Output Unit Type",
        " ",
        "Curve:Biquadratic,",
        "  VRFHeatCapFTHi,          !- Name",
        "  1.161134821,             !- Coefficient1 Constant",
        "  0.027478868,             !- Coefficient2 x",
        "  -0.00168795,             !- Coefficient3 x**2",
        "  0.001783378,             !- Coefficient4 y",
        "  2.03208E-06,             !- Coefficient5 y**2",
        "  -6.8969E-05,             !- Coefficient6 x*y",
        "  15,                      !- Minimum Value of x",
        "  27,                      !- Maximum Value of x",
        "  -10,                     !- Minimum Value of y",
        "  15,                      !- Maximum Value of y",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Biquadratic,",
        "  VRFHeatEIRFT,            !- Name",
        "  0.87465501,              !- Coefficient1 Constant",
        "  -0.01319754,             !- Coefficient2 x",
        "  0.00110307,              !- Coefficient3 x**2",
        "  -0.0133118,              !- Coefficient4 y",
        "  0.00089017,              !- Coefficient5 y**2",
        "  -0.00012766,             !- Coefficient6 x*y",
        "  15,                      !- Minimum Value of x",
        "  27,                      !- Maximum Value of x",
        "  -20,                     !- Minimum Value of y",
        "  12,                      !- Maximum Value of y",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Cubic,",
        "  VRFHeatEIRFTBoundary,    !- Name",
        "  -7.6000882,              !- Coefficient1 Constant",
        "  3.05090016,              !- Coefficient2 x",
        "  -0.1162844,              !- Coefficient3 x**2",
        "  0.0,                     !- Coefficient4 x**3",
        "  15,                      !- Minimum Value of x",
        "  27,                      !- Maximum Value of x",
        "  -20,                     !- Minimum Curve Output",
        "  15,                      !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature;             !- Output Unit Type",
        " ",
        "Curve:Biquadratic,",
        "  VRFHeatEIRFTHi,          !- Name",
        "  2.504005146,             !- Coefficient1 Constant",
        "  -0.05736767,             !- Coefficient2 x",
        "  4.07336E-05,             !- Coefficient3 x**2",
        "  -0.12959669,             !- Coefficient4 y",
        "  0.00135839,              !- Coefficient5 y**2",
        "  0.00317047,              !- Coefficient6 x*y",
        "  15,                      !- Minimum Value of x",
        "  27,                      !- Maximum Value of x",
        "  -10,                     !- Minimum Value of y",
        "  15,                      !- Maximum Value of y",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Cubic,",
        "  HeatingEIRLowPLR,        !- Name",
        "  0.1400093,               !- Coefficient1 Constant",
        "  0.6415002,               !- Coefficient2 x",
        "  0.1339047,               !- Coefficient3 x**2",
        "  0.0845859,               !- Coefficient4 x**3",
        "  0,                       !- Minimum Value of x",
        "  1,                       !- Maximum Value of x",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Dimensionless,           !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Quadratic,",
        "  HeatingEIRHiPLR,         !- Name",
        "  2.4294355,               !- Coefficient1 Constant",
        "  -2.235887,               !- Coefficient2 x",
        "  0.8064516,               !- Coefficient3 x**2",
        "  1.0,                     !- Minimum Value of x",
        "  1.5,                     !- Maximum Value of x",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Dimensionless,           !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Linear,",
        "  HeatingCombRatio,        !- Name",
        "  0.96034,                 !- Coefficient1 Constant",
        "  0.03966,                 !- Coefficient2 x",
        "  1.0,                     !- Minimum Value of x",
        "  1.5,                     !- Maximum Value of x",
        "  1.0,                     !- Minimum Curve Output",
        "  1.023,                   !- Maximum Curve Output",
        "  Dimensionless,           !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Biquadratic,",
        "  CoolingLengthCorrectionFactor,  !- Name",
        "  1.0693794,               !- Coefficient1 Constant",
        "  -0.0014951,              !- Coefficient2 x",
        "  2.56E-06,                !- Coefficient3 x**2",
        "  -0.1151104,              !- Coefficient4 y",
        "  0.0511169,               !- Coefficient5 y**2",
        "  -0.0004369,              !- Coefficient6 x*y",
        "  8,                       !- Minimum Value of x",
        "  175,                     !- Maximum Value of x",
        "  0.5,                     !- Minimum Value of y",
        "  1.5,                     !- Maximum Value of y",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Cubic,",
        "  VRFTUCoolCapFT,          !- Name",
        "  0.504547273506488,       !- Coefficient1 Constant",
        "  0.0288891279198444,      !- Coefficient2 x",
        "  -0.000010819418650677,   !- Coefficient3 x**2",
        "  0.0000101359395177008,   !- Coefficient4 x**3",
        "  0.0,                     !- Minimum Value of x",
        "  50.0,                    !- Maximum Value of x",
        "  0.5,                     !- Minimum Curve Output",
        "  1.5,                     !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Quadratic,",
        "  VRFACCoolCapFFF,         !- Name",
        "  0.8,                     !- Coefficient1 Constant",
        "  0.2,                     !- Coefficient2 x",
        "  0.0,                     !- Coefficient3 x**2",
        "  0.5,                     !- Minimum Value of x",
        "  1.5;                     !- Maximum Value of x",
        " ",
        "Curve:Cubic,",
        "  VRFTUHeatCapFT,          !- Name",
        "  -0.390708928227928,      !- Coefficient1 Constant",
        "  0.261815023760162,       !- Coefficient2 x",
        "  -0.0130431603151873,     !- Coefficient3 x**2",
        "  0.000178131745997821,    !- Coefficient4 x**3",
        "  0.0,                     !- Minimum Value of x",
        "  50.0,                    !- Maximum Value of x",
        "  0.5,                     !- Minimum Curve Output",
        "  1.5,                     !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    DataGlobals::BeginEnvrnFlag = true;
    DataSizing::CurZoneEqNum = 1;
    DataEnvironment::OutBaroPress = 101325;          // sea level
    DataZoneEquipment::ZoneEquipInputsFilled = true; // denotes zone equipment has been read in
    StdRhoAir = PsyRhoAirFnPbTdbW(DataEnvironment::OutBaroPress, 20.0, 0.0);
    ZoneEqSizing.allocate(1);
    ZoneSizingRunDone = true;
    ZoneEqSizing(CurZoneEqNum).DesignSizeFromParent = false;
    ZoneEqSizing(CurZoneEqNum).SizingMethod.allocate(25);
    ZoneEqSizing(CurZoneEqNum).SizingMethod(DataHVACGlobals::SystemAirflowSizing) = DataSizing::SupplyAirFlowRate;

    FinalZoneSizing.allocate(1);
    FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow = 0.566337; // 400 cfm * 3 tons = 1200 cfm
    FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow = 0.566337;

    ZoneSysEnergyDemand.allocate(1);

    ProcessScheduleInput();   // read schedules
    GetCurveInput();          // read curves
    GetZoneData(ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);

    GetZoneEquipmentData(); // read equipment list and connections
    GetVRFInputData(ErrorsFound);
    EXPECT_TRUE(ErrorsFound);
    EXPECT_EQ(0, VRFTU(VRFTUNum).VRFSysNum);
    EXPECT_EQ(0, VRFTU(VRFTUNum).ZoneNum);
    EXPECT_EQ(0, VRFTU(VRFTUNum).TUListIndex);
    EXPECT_EQ(0, VRFTU(VRFTUNum).IndexToTUInTUList);

    // clean up
    ZoneSysEnergyDemand.deallocate();
}

TEST_F(EnergyPlusFixture, VRFTest_SysCurve_WaterCooled)
{

    static std::string const RoutineName("VRFTest_WaterCooled");
    bool ErrorsFound(false);       // function returns true on error
    bool FirstHVACIteration(true); // simulate the first pass through HVAC simulation, use false for next iteration
    int VRFCond(1);                // index to VRF condenser
    int VRFTUNum(1);               // index to VRF terminal unit
    int EquipPtr(1);               // index to equipment list
    int CurZoneNum(1);             // index to zone
    int ZoneInletAirNode(0);       // zone inlet node number
    Real64 SysOutputProvided(0.0); // function returns sensible capacity [W]
    Real64 LatOutputProvided(0.0); // function returns latent capacity [W]
    Real64 CurLoad(0.0);
    Real64 MaxLoad(0.0);
    Real64 MinLoad(0.0);
    Real64 OptLoad(0.0);
    int LoopNum(0);
    Real64 rho(0.0);
    Real64 Cp(0.0);
    Real64 CondVolFlowRate(0.0);

    std::string const idf_objects = delimited_string({
        "Version,8.3;",
        " BUILDING, VRFTest_SysCurve_WaterCooled, 0.0, Suburbs, .04, .4, FullExterior, 25, 6;",
        " ",
        "AirConditioner:VariableRefrigerantFlow,",
        "  VRF Water Cooled HP,     !- Heat Pump Name",
        "  VRFCondAvailSched,       !- Availability Schedule Name",
        "  autosize,                !- Gross Rated Total Cooling Capacity {W}",
        "  3.2917,                  !- Gross Rated Cooling COP {W/W}",
        "  -5,                      !- Minimum Outdoor Temperature in Cooling Mode {C}",
        "  43,                      !- Maximum Outdoor Temperature in Cooling Mode {C}",
        "  VRFCoolCapFT,            !- Cooling Capacity Ratio Modifier Function of Low Temperature Curve Name",
        "  VRFCoolCapFTBoundary,    !- Cooling Capacity Ratio Boundary Curve Name",
        "  VRFCoolCapFTHi,          !- Cooling Capacity Ratio Modifier Function of High Temperature Curve Name",
        "  VRFCoolEIRFT,            !- Cooling Energy Input Ratio Modifier Function of Low Temperature Curve Name",
        "  VRFCoolEIRFTBoundary,    !- Cooling Energy Input Ratio Boundary Curve Name",
        "  VRFCoolEIRFTHi,          !- Cooling Energy Input Ratio Modifier Function of High Temperature Curve Name",
        "  CoolingEIRLowPLR,        !- Cooling Energy Input Ratio Modifier Function of Low Part-Load Ratio Curve Name",
        "  CoolingEIRHiPLR,         !- Cooling Energy Input Ratio Modifier Function of High Part-Load Ratio Curve Name",
        "  CoolingCombRatio,        !- Cooling Combination Ratio Correction Factor Curve Name",
        "  VRFCPLFFPLR,             !- Cooling Part-Load Fraction Correlation Curve Name",
        "  autosize,                !- Gross Rated Heating Capacity {W}",
        "  ,                        !- Rated Heating Capacity Sizing Ratio {W/W}",
        "  3.5484,                  !- Gross Rated Heating COP {W/W}",
        "  -20,                     !- Minimum Outdoor Temperature in Heating Mode {C}",
        "  20,                      !- Maximum Outdoor Temperature in Heating Mode {C}",
        "  VRFHeatCapFT,            !- Heating Capacity Ratio Modifier Function of Low Temperature Curve Name",
        "  VRFHeatCapFTBoundary,    !- Heating Capacity Ratio Boundary Curve Name",
        "  VRFHeatCapFTHi,          !- Heating Capacity Ratio Modifier Function of High Temperature Curve Name",
        "  VRFHeatEIRFT,            !- Heating Energy Input Ratio Modifier Function of Low Temperature Curve Name",
        "  VRFHeatEIRFTBoundary,    !- Heating Energy Input Ratio Boundary Curve Name",
        "  VRFHeatEIRFTHi,          !- Heating Energy Input Ratio Modifier Function of High Temperature Curve Name",
        "  WetBulbTemperature,      !- Heating Performance Curve Outdoor Temperature Type",
        "  HeatingEIRLowPLR,        !- Heating Energy Input Ratio Modifier Function of Low Part-Load Ratio Curve Name",
        "  HeatingEIRHiPLR,         !- Heating Energy Input Ratio Modifier Function of High Part-Load Ratio Curve Name",
        "  HeatingCombRatio,        !- Heating Combination Ratio Correction Factor Curve Name",
        "  VRFCPLFFPLR,             !- Heating Part-Load Fraction Correlation Curve Name",
        "  0.25,                    !- Minimum Heat Pump Part-Load Ratio {dimensionless}",
        "  SPACE1-1,                !- Zone Name for Master Thermostat Location",
        "  LoadPriority,            !- Master Thermostat Priority Control Type",
        "  ,                        !- Thermostat Priority Schedule Name",
        "  VRF Heat Pump TU List,   !- Zone Terminal Unit List Name",
        "  No,                      !- Heat Pump Waste Heat Recovery",
        "  30,                      !- Equivalent Piping Length used for Piping Correction Factor in Cooling Mode {m}",
        "  10,                      !- Vertical Height used for Piping Correction Factor {m}",
        "  CoolingLengthCorrectionFactor,  !- Piping Correction Factor for Length in Cooling Mode Curve Name",
        "  -0.000386,               !- Piping Correction Factor for Height in Cooling Mode Coefficient {1/m}",
        "  30,                      !- Equivalent Piping Length used for Piping Correction Factor in Heating Mode {m}",
        "  ,                        !- Piping Correction Factor for Length in Heating Mode Curve Name",
        "  ,                        !- Piping Correction Factor for Height in Heating Mode Coefficient {1/m}",
        "  15,                      !- Crankcase Heater Power per Compressor {W}",
        "  3,                       !- Number of Compressors {dimensionless}",
        "  0.33,                    !- Ratio of Compressor Size to Total Compressor Capacity {W/W}",
        "  7,                       !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater {C}",
        "  ReverseCycle,            !- Defrost Strategy",
        "  Timed,                   !- Defrost Control",
        "  DefrostEIRSched,         !- Defrost Energy Input Ratio Modifier Function of Temperature Curve Name",
        "  ,                        !- Defrost Time Period Fraction {dimensionless}",
        "  autosize,                !- Resistive Defrost Heater Capacity {W}",
        "  7,                       !- Maximum Outdoor Dry-bulb Temperature for Defrost Operation {C}",
        "  WaterCooled,             !- Condenser Type",
        "  VRF Water Inlet Node,    !- Condenser Inlet Node Name",
        "  VRF Water Outlet Node,   !- Condenser Outlet Node Name",
        "  autosize,                !- Water Condenser Volume Flow Rate {m3/s}",
        "  ,                        !- Evaporative Condenser Effectiveness {dimensionless}",
        "  ,                        !- Evaporative Condenser Air Flow Rate {m3/s}",
        "  0,                       !- Evaporative Condenser Pump Rated Power Consumption {W}",
        "  ,                        !- Supply Water Storage Tank Name",
        "  0,                       !- Basin Heater Capacity {W/K}",
        "  ,                        !- Basin Heater Setpoint Temperature {C}",
        "  ,                        !- Basin Heater Operating Schedule Name",
        "  Electricity;             !- Fuel Type",
        " ",
        "Zone,",
        "  SPACE1-1,                !- Name",
        "  0,                       !- Direction of Relative North {deg}",
        "  0,                       !- X Origin {m}",
        "  0,                       !- Y Origin {m}",
        "  0,                       !- Z Origin {m}",
        "  1,                       !- Type",
        "  1,                       !- Multiplier",
        "  2.438400269,             !- Ceiling Height {m}",
        "  239.247360229;           !- Volume {m3}",
        " ",
        "ZoneHVAC:EquipmentConnections,",
        "  SPACE1-1,                !- Zone Name",
        "  SPACE1-1 Eq,             !- Zone Conditioning Equipment List Name",
        "  TU1 Outlet Node,         !- Zone Air Inlet Node or NodeList Name",
        "  TU1 Inlet Node,          !- Zone Air Exhaust Node or NodeList Name",
        "  SPACE1-1 Node,           !- Zone Air Node Name",
        "  SPACE1-1 Out Node;       !- Zone Return Air Node Name", // not used anywhere else in the example file
        " ",
        "ZoneHVAC:EquipmentList,",
        "  SPACE1-1 Eq,             !- Name",
        "  SequentialLoad,          !- Load Distribution Scheme",
        "  ZoneHVAC:TerminalUnit:VariableRefrigerantFlow,  !- Zone Equipment 1 Object Type",
        "  TU1,                     !- Zone Equipment 1 Name",
        "  1,                       !- Zone Equipment 1 Cooling Sequence",
        "  1;                       !- Zone Equipment 1 Heating or No-Load Sequence",
        " ",
        "ZoneTerminalUnitList,",
        "  VRF Heat Pump TU List,    !- Zone Terminal Unit List Name",
        "  TU1;                      !- Zone Terminal Unit Name 1",
        " ",
        "ZoneHVAC:TerminalUnit:VariableRefrigerantFlow,",
        "  TU1,                      !- Zone Terminal Unit Name",
        "  VRFAvailSched,            !- Terminal Unit Availability Schedule",
        "  TU1 Inlet Node,           !- Terminal Unit Air Inlet Node Name",
        "  TU1 Outlet Node,          !- Terminal Unit Air Outlet Node Name",
        "  autosize,                 !- Supply Air Flow Rate During Cooling Operation {m3/s}",
        "  autosize,                 !- Supply Air Flow Rate When No Cooling is Needed {m3/s}",
        "  autosize,                 !- Supply Air Flow Rate During Heating Operation {m3/s}",
        "  autosize,                 !- Supply Air Flow Rate When No Heating is Needed {m3/s}",
        "  autosize,                 !- Outdoor Air Flow Rate During Cooling Operation {m3/s}",
        "  autosize,                 !- Outdoor Air Flow Rate During Heating Operation {m3/s}",
        "  autosize,                 !- Outdoor Air Flow Rate When No Cooling or Heating is Needed {m3/s}",
        "  VRFWaterFanSchedule,      !- Supply Air Fan Operating Mode Schedule Name",
        "  drawthrough,              !- Supply Air Fan Placement",
        "  Fan:OnOff,                !- Supply Air Fan Object Type",
        "  TU1 VRF Supply Fan,       !- Supply Air Fan Object Name",
        "  OutdoorAir:Mixer,         !- Outside Air Mixer Object Type",
        "  TU1 OA Mixer,             !- Outside Air Mixer Object Name",
        "  COIL:Cooling:DX:VariableRefrigerantFlow,  !- Cooling Coil Object Type",
        "  TU1 VRF DX Cooling Coil,  !- Cooling Coil Object Name",
        "  COIL:Heating:DX:VariableRefrigerantFlow,  !- Heating Coil Object Type",
        "  TU1 VRF DX Heating Coil,  !- Heating Coil Object Name",
        "  30,                       !- Zone Terminal Unit On Parasitic Electric Energy Use {W}",
        "  20;                       !- Zone Terminal Unit Off Parasitic Electric Energy Use{ W }",
        " ",
        "Fan:OnOff,",
        "  TU1 VRF Supply Fan,       !- Name",
        "  VRFAvailSched,            !- Availability Schedule Name",
        "  0.7,                      !- Fan Total Efficiency",
        "  600.0,                    !- Pressure Rise{ Pa }",
        "  autosize,                 !- Maximum Flow Rate{ m3 / s }",
        "  0.9,                      !- Motor Efficiency",
        "  1.0,                      !- Motor In Airstream Fraction",
        "  TU1 VRF DX HCoil Outlet Node, !- Air Inlet Node Name",
        "  TU1 Outlet Node;          !- Air Outlet Node Name",
        " ",
        "OutdoorAir:Mixer,",
        "  TU1 OA Mixer,             !- Name",
        "  TU1 VRF DX CCoil Inlet Node, !- Mixed Air Node Name",
        "  Outside Air Inlet Node 1, !- Outdoor Air Stream Node Name",
        "  Relief Air Outlet Node 1, !- Relief Air Stream Node Name",
        "  TU1 Inlet Node;           !- Return Air Stream Node Name",
        " ",
        "OutdoorAir:NodeList,",
        "  OutsideAirInletNodes;     !- Node or NodeList Name 1",
        " ",
        "NodeList,",
        "  OutsideAirInletNodes, !- Name",
        "  Outside Air Inlet Node 1, !- Node 1 Name",
        "  MyVRFOANode;  !- Node 1 Name",
        " ",
        "COIL:Cooling:DX:VariableRefrigerantFlow,",
        "  TU1 VRF DX Cooling Coil, !- Name",
        "  VRFAvailSched,           !- Availability Schedule Name",
        "  autosize,                !- Gross Rated Total Cooling Capacity {W}",
        "  autosize,                !- Gross Rated Sensible Heat Ratio",
        "  autosize,                !- Rated Air Flow Rate {m3/s}",
        "  VRFTUCoolCapFT,          !- Cooling Capacity Ratio Modifier Function of Temperature Curve Name",
        "  VRFACCoolCapFFF,         !- Cooling Capacity Modifier Curve Function of Flow Fraction Name",
        "  TU1 VRF DX CCoil Inlet Node,  !- Coil Air Inlet Node",
        "  TU1 VRF DX CCoil Outlet Node,  !- Coil Air Outlet Node",
        "  ;                        !- Name of Water Storage Tank for Condensate Collection",
        " ",
        "COIL:Heating:DX:VariableRefrigerantFlow,",
        "  TU1 VRF DX Heating Coil, !- Name",
        "  VRFAvailSched,           !- Availability Schedule",
        "  autosize,                !- Gross Rated Heating Capacity {W}",
        "  autosize,                !- Rated Air Flow Rate {m3/s}",
        "  TU1 VRF DX CCoil Outlet Node,  !- Coil Air Inlet Node",
        "  TU1 VRF DX HCoil Outlet Node,  !- Coil Air Outlet Node",
        "  VRFTUHeatCapFT,          !- Heating Capacity Ratio Modifier Function of Temperature Curve Name",
        "  VRFACCoolCapFFF;         !- Heating Capacity Modifier Function of Flow Fraction Curve Name",
        " ",
        "ScheduleTypeLimits,",
        "  Any Number;              !- Name",
        " ",
        "Schedule:Compact,",
        "  VRFAvailSched,           !- Name",
        "  Any Number,              !- Schedule Type Limits Name",
        "  Through: 3/31,           !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00,1.0,        !- Field 3",
        "  Through: 9/30,           !- Field 4",
        "  For: WeekDays,           !- Field 5",
        "  Until: 7:00,1.0,         !- Field 6",
        "  Until: 17:00,1.0,        !- Field 7",
        "  Until: 24:00,1.0,        !- Field 8",
        "  For: SummerDesignDay WinterDesignDay, !- Field 9",
        "  Until: 24:00,1.0,        !- Field 10",
        "  For: AllOtherDays,       !- Field 11",
        "  Until: 24:00,1.0,        !- Field 12",
        "  Through: 12/31,          !- Field 13",
        "  For: AllDays,            !- Field 14",
        "  Until: 24:00,1.0;        !- Field 15",
        " ",
        "Schedule:Compact,",
        "  VRFCondAvailSched,       !- Name",
        "  Any Number,              !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 12:00,1.0,        !- Field 3",
        "  Until: 13:00,1.0,        !- Field 4",
        "  Until: 24:00,1.0;        !- Field 5",
        " ",
        "Schedule:Compact,",
        "  VRFWaterFanSchedule,     !- Name",
        "  Any Number,              !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00, 1.0;       !- Field 7",
        " ",
        "Curve:Biquadratic,",
        "  DefrostEIRSched,         !- Name",
        "  1.0,                     !- Coefficient1 Constant",
        "  0.0,                     !- Coefficient2 x",
        "  0.0,                     !- Coefficient3 x**2",
        "  0.0,                     !- Coefficient4 y",
        "  0.0,                     !- Coefficient5 y**2",
        "  0.0,                     !- Coefficient6 x*y",
        "  15,                      !- Minimum Value of x",
        "  24,                      !- Maximum Value of x",
        "  -5,                      !- Minimum Value of y",
        "  23,                      !- Maximum Value of y",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Biquadratic,",
        "  VRFCoolCapFT,            !- Name",
        "  0.576882692,             !- Coefficient1 Constant",
        "  0.017447952,             !- Coefficient2 x",
        "  0.000583269,             !- Coefficient3 x**2",
        "  -1.76324E-06,            !- Coefficient4 y",
        "  -7.474E-09,              !- Coefficient5 y**2",
        "  -1.30413E-07,            !- Coefficient6 x*y",
        "  15,                      !- Minimum Value of x",
        "  24,                      !- Maximum Value of x",
        "  -5,                      !- Minimum Value of y",
        "  23,                      !- Maximum Value of y",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Cubic,",
        "  VRFCoolCapFTBoundary,    !- Name",
        "  25.73473775,             !- Coefficient1 Constant",
        "  -0.03150043,             !- Coefficient2 x",
        "  -0.01416595,             !- Coefficient3 x**2",
        "  0,                       !- Coefficient4 x**3",
        "  11,                      !- Minimum Value of x",
        "  30,                      !- Maximum Value of x",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature;             !- Output Unit Type",
        " ",
        "Curve:Biquadratic,",
        "  VRFCoolCapFTHi,          !- Name",
        "  0.6867358,               !- Coefficient1 Constant",
        "  0.0207631,               !- Coefficient2 x",
        "  0.0005447,               !- Coefficient3 x**2",
        "  -0.0016218,              !- Coefficient4 y",
        "  -4.259E-07,              !- Coefficient5 y**2",
        "  -0.0003392,              !- Coefficient6 x*y",
        "  15,                      !- Minimum Value of x",
        "  24,                      !- Maximum Value of x",
        "  16,                      !- Minimum Value of y",
        "  43,                      !- Maximum Value of y",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Biquadratic,",
        "  VRFCoolEIRFT,            !- Name",
        "  0.989010541,             !- Coefficient1 Constant",
        "  -0.02347967,             !- Coefficient2 x",
        "  0.000199711,             !- Coefficient3 x**2",
        "  0.005968336,             !- Coefficient4 y",
        "  -1.0289E-07,             !- Coefficient5 y**2",
        "  -0.00015686,             !- Coefficient6 x*y",
        "  15,                      !- Minimum Value of x",
        "  24,                      !- Maximum Value of x",
        "  -5,                      !- Minimum Value of y",
        "  23,                      !- Maximum Value of y",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Cubic,",
        "  VRFCoolEIRFTBoundary,    !- Name",
        "  25.73473775,             !- Coefficient1 Constant",
        "  -0.03150043,             !- Coefficient2 x",
        "  -0.01416595,             !- Coefficient3 x**2",
        "  0,                       !- Coefficient4 x**3",
        "  15,                      !- Minimum Value of x",
        "  24,                      !- Maximum Value of x",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature;             !- Output Unit Type",
        " ",
        "Curve:Biquadratic,",
        "  VRFCoolEIRFTHi,          !- Name",
        "  0.14351470,              !- Coefficient1 Constant",
        "  0.01860035,              !- Coefficient2 x",
        "  -0.0003954,              !- Coefficient3 x**2",
        "  0.02485219,              !- Coefficient4 y",
        "  0.00016329,              !- Coefficient5 y**2",
        "  -0.0006244,              !- Coefficient6 x*y",
        "  15,                      !- Minimum Value of x",
        "  24,                      !- Maximum Value of x",
        "  16,                      !- Minimum Value of y",
        "  43,                      !- Maximum Value of y",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Cubic,",
        "  CoolingEIRLowPLR,        !- Name",
        "  0.4628123,               !- Coefficient1 Constant",
        "  -1.0402406,              !- Coefficient2 x",
        "  2.17490997,              !- Coefficient3 x**2",
        "  -0.5974817,              !- Coefficient4 x**3",
        "  0,                       !- Minimum Value of x",
        "  1,                       !- Maximum Value of x",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature;             !- Output Unit Type",
        " ",
        "Curve:Quadratic,",
        "  CoolingEIRHiPLR,         !- Name",
        "  1.0,                     !- Coefficient1 Constant",
        "  0.0,                     !- Coefficient2 x",
        "  0.0,                     !- Coefficient3 x**2",
        "  1.0,                     !- Minimum Value of x",
        "  1.5,                     !- Maximum Value of x",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Dimensionless,           !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Linear,",
        "  CoolingCombRatio,        !- Name",
        "  0.618055,                !- Coefficient1 Constant",
        "  0.381945,                !- Coefficient2 x",
        "  1.0,                     !- Minimum Value of x",
        "  1.5,                     !- Maximum Value of x",
        "  1.0,                     !- Minimum Curve Output",
        "  1.2,                     !- Maximum Curve Output",
        "  Dimensionless,           !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "CURVE:QUADRATIC,",
        "  VRFCPLFFPLR,             !- Name",
        "  0.85,                    !- Coefficient1 Constant",
        "  0.15,                    !- Coefficient2 x",
        "  0.0,                     !- Coefficient3 x**2",
        "  0.0,                     !- Minimum Value of x",
        "  1.0,                     !- Maximum Value of x",
        "  0.85,                    !- Minimum Curve Output",
        "  1.0,                     !- Maximum Curve Output",
        "  Dimensionless,           !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Biquadratic,",
        "  VRFHeatCapFT,            !- Name",
        "  1.014599599,             !- Coefficient1 Constant",
        "  -0.002506703,            !- Coefficient2 x",
        "  -0.000141599,            !- Coefficient3 x**2",
        "  0.026931595,             !- Coefficient4 y",
        "  1.83538E-06,             !- Coefficient5 y**2",
        "  -0.000358147,            !- Coefficient6 x*y",
        "  15,                      !- Minimum Value of x",
        "  27,                      !- Maximum Value of x",
        "  -20,                     !- Minimum Value of y",
        "  15,                      !- Maximum Value of y",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Cubic,",
        "  VRFHeatCapFTBoundary,    !- Name",
        "  -7.6000882,              !- Coefficient1 Constant",
        "  3.05090016,              !- Coefficient2 x",
        "  -0.1162844,              !- Coefficient3 x**2",
        "  0.0,                     !- Coefficient4 x**3",
        "  15,                      !- Minimum Value of x",
        "  27,                      !- Maximum Value of x",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature;             !- Output Unit Type",
        " ",
        "Curve:Biquadratic,",
        "  VRFHeatCapFTHi,          !- Name",
        "  1.161134821,             !- Coefficient1 Constant",
        "  0.027478868,             !- Coefficient2 x",
        "  -0.00168795,             !- Coefficient3 x**2",
        "  0.001783378,             !- Coefficient4 y",
        "  2.03208E-06,             !- Coefficient5 y**2",
        "  -6.8969E-05,             !- Coefficient6 x*y",
        "  15,                      !- Minimum Value of x",
        "  27,                      !- Maximum Value of x",
        "  -10,                     !- Minimum Value of y",
        "  15,                      !- Maximum Value of y",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Biquadratic,",
        "  VRFHeatEIRFT,            !- Name",
        "  0.87465501,              !- Coefficient1 Constant",
        "  -0.01319754,             !- Coefficient2 x",
        "  0.00110307,              !- Coefficient3 x**2",
        "  -0.0133118,              !- Coefficient4 y",
        "  0.00089017,              !- Coefficient5 y**2",
        "  -0.00012766,             !- Coefficient6 x*y",
        "  15,                      !- Minimum Value of x",
        "  27,                      !- Maximum Value of x",
        "  -20,                     !- Minimum Value of y",
        "  12,                      !- Maximum Value of y",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Cubic,",
        "  VRFHeatEIRFTBoundary,    !- Name",
        "  -7.6000882,              !- Coefficient1 Constant",
        "  3.05090016,              !- Coefficient2 x",
        "  -0.1162844,              !- Coefficient3 x**2",
        "  0.0,                     !- Coefficient4 x**3",
        "  15,                      !- Minimum Value of x",
        "  27,                      !- Maximum Value of x",
        "  -20,                     !- Minimum Curve Output",
        "  15,                      !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature;             !- Output Unit Type",
        " ",
        "Curve:Biquadratic,",
        "  VRFHeatEIRFTHi,          !- Name",
        "  2.504005146,             !- Coefficient1 Constant",
        "  -0.05736767,             !- Coefficient2 x",
        "  4.07336E-05,             !- Coefficient3 x**2",
        "  -0.12959669,             !- Coefficient4 y",
        "  0.00135839,              !- Coefficient5 y**2",
        "  0.00317047,              !- Coefficient6 x*y",
        "  15,                      !- Minimum Value of x",
        "  27,                      !- Maximum Value of x",
        "  -10,                     !- Minimum Value of y",
        "  15,                      !- Maximum Value of y",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Cubic,",
        "  HeatingEIRLowPLR,        !- Name",
        "  0.1400093,               !- Coefficient1 Constant",
        "  0.6415002,               !- Coefficient2 x",
        "  0.1339047,               !- Coefficient3 x**2",
        "  0.0845859,               !- Coefficient4 x**3",
        "  0,                       !- Minimum Value of x",
        "  1,                       !- Maximum Value of x",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Dimensionless,           !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Quadratic,",
        "  HeatingEIRHiPLR,         !- Name",
        "  2.4294355,               !- Coefficient1 Constant",
        "  -2.235887,               !- Coefficient2 x",
        "  0.8064516,               !- Coefficient3 x**2",
        "  1.0,                     !- Minimum Value of x",
        "  1.5,                     !- Maximum Value of x",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Dimensionless,           !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Linear,",
        "  HeatingCombRatio,        !- Name",
        "  0.96034,                 !- Coefficient1 Constant",
        "  0.03966,                 !- Coefficient2 x",
        "  1.0,                     !- Minimum Value of x",
        "  1.5,                     !- Maximum Value of x",
        "  1.0,                     !- Minimum Curve Output",
        "  1.023,                   !- Maximum Curve Output",
        "  Dimensionless,           !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Biquadratic,",
        "  CoolingLengthCorrectionFactor,  !- Name",
        "  1.0693794,               !- Coefficient1 Constant",
        "  -0.0014951,              !- Coefficient2 x",
        "  2.56E-06,                !- Coefficient3 x**2",
        "  -0.1151104,              !- Coefficient4 y",
        "  0.0511169,               !- Coefficient5 y**2",
        "  -0.0004369,              !- Coefficient6 x*y",
        "  8,                       !- Minimum Value of x",
        "  175,                     !- Maximum Value of x",
        "  0.5,                     !- Minimum Value of y",
        "  1.5,                     !- Maximum Value of y",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Cubic,",
        "  VRFTUCoolCapFT,          !- Name",
        "  0.504547273506488,       !- Coefficient1 Constant",
        "  0.0288891279198444,      !- Coefficient2 x",
        "  -0.000010819418650677,   !- Coefficient3 x**2",
        "  0.0000101359395177008,   !- Coefficient4 x**3",
        "  0.0,                     !- Minimum Value of x",
        "  50.0,                    !- Maximum Value of x",
        "  0.5,                     !- Minimum Curve Output",
        "  1.5,                     !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "Curve:Quadratic,",
        "  VRFACCoolCapFFF,         !- Name",
        "  0.8,                     !- Coefficient1 Constant",
        "  0.2,                     !- Coefficient2 x",
        "  0.0,                     !- Coefficient3 x**2",
        "  0.5,                     !- Minimum Value of x",
        "  1.5;                     !- Maximum Value of x",
        " ",
        "Curve:Cubic,",
        "  VRFTUHeatCapFT,          !- Name",
        "  -0.390708928227928,      !- Coefficient1 Constant",
        "  0.261815023760162,       !- Coefficient2 x",
        "  -0.0130431603151873,     !- Coefficient3 x**2",
        "  0.000178131745997821,    !- Coefficient4 x**3",
        "  0.0,                     !- Minimum Value of x",
        "  50.0,                    !- Maximum Value of x",
        "  0.5,                     !- Minimum Curve Output",
        "  1.5,                     !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        " ",
        "PlantLoop,",
        "  Main Loop, !- Name",
        "  WATER, !- Fluid Type",
        ", !- User Defined Fluid Type",
        "Main Loop Operation, !- Plant Equipment Operation Scheme Name",
        "Supply Outlet Node, !- Loop Temperature Setpoint Node Name",
        "100, !- Maximum Loop Temperature{ C }",
        "3, !- Minimum Loop Temperature{ C }",
        "0.003, !- Maximum Loop Flow Rate{ m3 / s }",
        "0, !- Minimum Loop Flow Rate{ m3 / s }",
        "autocalculate, !- Plant Loop Volume{ m3 }",
        "Supply Inlet Node, !- Plant Side Inlet Node Name",
        "Supply Outlet Node, !- Plant Side Outlet Node Name",
        "Supply Branches, !- Plant Side Branch List Name",
        "Supply Connectors, !- Plant Side Connector List Name",
        "Demand Inlet Node, !- Demand Side Inlet Node Name",
        "Demand Outlet Node, !- Demand Side Outlet Node Name",
        "Demand Branches, !- Demand Side Branch List Name",
        "Demand Connectors, !- Demand Side Connector List Name",
        "Optimal, !- Load Distribution Scheme",
        ", !- Availability Manager List Name",
        ", !- Plant Loop Demand Calculation Scheme",
        ", !- Common Pipe Simulation",
        ", !- Pressure Simulation Type",
        "2.0; !- Loop Circulation Time {minutes}",
        " ",
        "Sizing:Plant,",
        "  Main Loop, !- Plant or Condenser Loop Name",
        "  heating, !- Loop Type",
        "  82., !- Design Loop Exit Temperature{ C }",
        "  11, !- Loop Design Temperature Difference{ deltaC }",
        "  Coincident, !- Sizing Option",
        "  1, !- Zone Timesteps in Averaging Window",
        "  None;                    !- Coincident Sizing Factor Mode",
        " ",
        "SetpointManager:Scheduled,",
        "  Main Loop Setpoint Manager, !- Name",
        "  Temperature, !- Control Variable",
        "  Main Loop Temp Sch, !- Schedule Name",
        "  Main Loop Setpoint Node List;  !- Setpoint Node or NodeList Name",
        " ",
        "NodeList,",
        "  Main Loop Setpoint Node List, !- Name",
        "  Supply Outlet Node;      !- Node 1 Name",
        " ",
        "PlantEquipmentOperationSchemes,",
        "  Main Loop Operation, !- Name",
        "  PlantEquipmentOperation:HeatingLoad, !- Control Scheme 1 Object Type",
        "  Purchased Only, !- Control Scheme 1 Name",
        "  AlwaysOnSchedule;        !- Control Scheme 1 Schedule Name",
        " ",
        "PlantEquipmentOperation:HeatingLoad,",
        "  Purchased Only, !- Name",
        "  0, !- Load Range 1 Lower Limit{ W }",
        "  10000000, !- Load Range 1 Upper Limit{ W }",
        "  Heating Plant;           !- Range 1 Equipment List Name",
        " ",
        "PlantEquipmentList,",
        "  Heating Plant, !- Name",
        "  DistrictHeating, !- Equipment 1 Object Type",
        "  Purchased Heating;       !- Equipment 1 Name",
        " ",
        "BranchList,",
        "  Supply Branches, !- Name",
        "  Supply Inlet Branch, !- Branch 1 Name",
        "  Heating Branch, !- Branch 2 Name",
        "  Supply Outlet Branch;    !- Branch 3 Name",
        " ",
        "ConnectorList,",
        "  Supply Connectors, !- Name",
        "  Connector:Splitter, !- Connector 1 Object Type",
        "  Supply Splitter, !- Connector 1 Name",
        "  Connector:Mixer, !- Connector 2 Object Type",
        "  Supply Mixer;            !- Connector 2 Name",
        " ",
        "Connector:Splitter,",
        "  Supply Splitter, !- Name",
        "  Supply Inlet Branch, !- Inlet Branch Name",
        "  Heating Branch;          !- Outlet Branch 1 Name",
        " ",
        "Connector:Mixer,",
        "  Supply Mixer, !- Name",
        "  Supply Outlet Branch, !- Outlet Branch Name",
        "  Heating Branch;          !- Inlet Branch 1 Name",
        " ",
        "Branch,",
        "  Supply Inlet Branch, !- Name",
        "  , !- Pressure Drop Curve Name",
        "  Pump:VariableSpeed, !- Component 1 Object Type",
        "  Pump, !- Component 1 Name",
        "  Supply Inlet Node, !- Component 1 Inlet Node Name",
        "  Supply Pump-Heating Node; !- Component 1 Outlet Node Name",
        " ",
        "Pump:VariableSpeed,",
        "  Pump, !- Name",
        "  Supply Inlet Node, !- Inlet Node Name",
        "  Supply Pump-Heating Node, !- Outlet Node Name",
        "  0.005, !- Rated Flow Rate{ m3 / s }",
        "  300000, !- Rated Pump Head{ Pa }",
        "  2250, !- Rated Power Consumption{ W }",
        "  0.87, !- Motor Efficiency",
        "  0.0, !- Fraction of Motor Inefficiencies to Fluid Stream",
        "  0, !- Coefficient 1 of the Part Load Performance Curve",
        "  1, !- Coefficient 2 of the Part Load Performance Curve",
        "  0, !- Coefficient 3 of the Part Load Performance Curve",
        "  0, !- Coefficient 4 of the Part Load Performance Curve",
        "  0, !- Minimum Flow Rate{ m3 / s }",
        "  INTERMITTENT;            !- Pump Control Type",
        " ",
        "Branch,",
        "  Heating Branch, !- Name",
        "  , !- Pressure Drop Curve Name",
        "  DistrictHeating, !- Component 1 Object Type",
        "  Purchased Heating, !- Component 1 Name",
        "  Supply Heating Inlet Node, !- Component 1 Inlet Node Name",
        "  Supply Heating Outlet Node; !- Component 1 Outlet Node Name",
        " ",
        "DistrictHeating,",
        "  Purchased Heating, !- Name",
        "  Supply Heating Inlet Node, !- Hot Water Inlet Node Name",
        "  Supply Heating Outlet Node, !- Hot Water Outlet Node Name",
        "  1000000;                 !- Nominal Capacity{ W }",
        " ",
        "Branch,",
        "  Supply Outlet Branch, !- Name",
        "  , !- Pressure Drop Curve Name",
        "  Pipe:Adiabatic, !- Component 1 Object Type",
        "  Supply Outlet Pipe, !- Component 1 Name",
        "  Supply Heating-Pipe Node, !- Component 1 Inlet Node Name",
        "  Supply Outlet Node; !- Component 1 Outlet Node Name",
        " ",
        "Pipe:Adiabatic,",
        "  Supply Outlet Pipe, !- Name",
        "  Supply Heating-Pipe Node, !- Inlet Node Name",
        "  Supply Outlet Node;      !- Outlet Node Name",
        " ",
        "BranchList,",
        "  Demand Branches, !- Name",
        "  Demand Inlet Branch, !- Branch 1 Name",
        "  Load Profile Branch 1, !- Branch 2 Name",
        "  Demand Outlet Branch;    !- Branch 3 Name",
        " ",
        "ConnectorList,",
        "  Demand Connectors, !- Name",
        "  Connector:Splitter, !- Connector 1 Object Type",
        "  Demand Splitter, !- Connector 1 Name",
        "  Connector:Mixer, !- Connector 2 Object Type",
        "  Demand Mixer;            !- Connector 2 Name",
        " ",
        "Connector:Splitter,",
        "  Demand Splitter, !- Name",
        "  Demand Inlet Branch, !- Inlet Branch Name",
        "  Load Profile Branch 1;   !- Outlet Branch 1 Name",
        " ",
        "Connector:Mixer,",
        "  Demand Mixer, !- Name",
        "  Demand Outlet Branch, !- Outlet Branch Name",
        "  Load Profile Branch 1;   !- Inlet Branch 1 Name",
        " ",
        "Branch,",
        "  Demand Inlet Branch, !- Name",
        "  , !- Pressure Drop Curve Name",
        "  Pipe:Adiabatic, !- Component 1 Object Type",
        "  Demand Inlet Pipe, !- Component 1 Name",
        "  Demand Inlet Node, !- Component 1 Inlet Node Name",
        "  VRF Water Inlet Node; !- Component 1 Outlet Node Name",
        " ",
        "Pipe:Adiabatic,",
        "  Demand Inlet Pipe, !- Name",
        "  Demand Inlet Node, !- Inlet Node Name",
        "  VRF Water Inlet Node;  !- Outlet Node Name",
        " ",
        "Branch,",
        "  Load Profile Branch 1, !- Name",
        "  , !- Pressure Drop Curve Name",
        "  AirConditioner:VariableRefrigerantFlow, !- Component 1 Object Type",
        "  VRF Water Cooled HP, !- Component 1 Name",
        "  VRF Water Inlet Node, !- Component 1 Inlet Node Name",
        "  VRF Water Outlet Node; !- Component 1 Outlet Node Name",
        " ",
        "Branch,",
        "  Demand Outlet Branch, !- Name",
        "  , !- Pressure Drop Curve Name",
        "  Pipe:Adiabatic, !- Component 1 Object Type",
        "  Demand Outlet Pipe, !- Component 1 Name",
        "  VRF Water Outlet Node, !- Component 1 Inlet Node Name",
        "  Demand Outlet Node; !- Component 1 Outlet Node Name",
        " ",
        "Pipe:Adiabatic,",
        "  Demand Outlet Pipe, !- Name",
        "  VRF Water Outlet Node, !- Inlet Node Name",
        "  Demand Outlet Node;      !- Outlet Node Name",
        " ",
        "ScheduleTypeLimits,",
        "  On/Off, !- Name",
        "  0, !- Lower Limit Value",
        "  1, !- Upper Limit Value",
        "  DISCRETE;                !- Numeric Type",
        " ",
        "Schedule:Compact,",
        "  Main Loop Temp Sch, !- Name",
        "  Any Number, !- Schedule Type Limits Name",
        "  THROUGH: 12/31, !- Field 1",
        "  FOR: AllDays, !- Field 2",
        "  UNTIL: 24:00, 60.0;       !- Field 3",
        " ",
        "Schedule:Compact,",
        "  AlwaysOnSchedule, !- Name",
        "  On/Off, !- Schedule Type Limits Name",
        "  THROUGH: 12/31, !- Field 1",
        "  FOR: AllDays, !- Field 2",
        "  UNTIL: 24:00, 1;          !- Field 3",
        " ",
        "Schedule:Compact,",
        "  Load Profile 1 Load Schedule, !- Name",
        "  Any Number, !- Schedule Type Limits Name",
        "  THROUGH: 12/31, !- Field 1",
        "  FOR: AllDays, !- Field 2",
        "  UNTIL: 4:00, 8000, !- Field 3",
        "  UNTIL: 8:00, 6000, !- Field 5",
        "  UNTIL: 9:00, 0, !- Field 7",
        "  UNTIL: 12:00, 6000, !- Field 9",
        "  UNTIL: 24:00, 10000;      !- Field 11",
        " ",
        "Schedule:Compact,",
        "  Load Profile 1 Flow Frac Schedule, !- Name",
        "  Any Number, !- Schedule Type Limits Name",
        "  THROUGH: 12/31, !- Field 1",
        "  FOR: AllDays, !- Field 2",
        "  UNTIL: 24:00, 1.0;        !- Field 3",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    DataGlobals::BeginEnvrnFlag = true;
    DataSizing::CurZoneEqNum = 1;
    DataEnvironment::OutBaroPress = 101325;          // sea level
    DataZoneEquipment::ZoneEquipInputsFilled = true; // denotes zone equipment has been read in
    DataEnvironment::StdRhoAir = PsyRhoAirFnPbTdbW(DataEnvironment::OutBaroPress, 20.0, 0.0);
    DataSizing::ZoneEqSizing.allocate(1);
    DataSizing::ZoneSizingRunDone = true;
    DataSizing::ZoneEqSizing(CurZoneEqNum).DesignSizeFromParent = false;
    DataSizing::ZoneEqSizing(CurZoneEqNum).SizingMethod.allocate(25);
    DataSizing::ZoneEqSizing(CurZoneEqNum).SizingMethod(DataHVACGlobals::SystemAirflowSizing) = DataSizing::SupplyAirFlowRate;

    DataSizing::FinalZoneSizing.allocate(1);
    DataSizing::FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow = 0.566337; // 400 cfm * 3 tons = 1200 cfm
    DataSizing::FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow = 0.566337;

    DataZoneEnergyDemands::ZoneSysEnergyDemand.allocate(1);

    Array2D<Real64> DummyArray; // Sky temperature
    DataGlobals::NumOfTimeStepInHour = 4;
    DataGlobals::MinutesPerTimeStep = 60 / DataGlobals::NumOfTimeStepInHour;
    DummyArray.allocate(DataGlobals::NumOfTimeStepInHour, 24);
    DummyArray = 0.0;
    ScheduleManager::GetScheduleValuesForDay(1, DummyArray, 58, 3);

    CurveManager::GetCurveInput();                // read curves
    HeatBalanceManager::GetZoneData(ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);

    DataZoneEquipment::GetZoneEquipmentData(); // read equipment list and connections

    BranchInputManager::ManageBranchInput();
    // Get plant loop data
    PlantManager::GetPlantLoopData();
    PlantManager::GetPlantInput();

    HVACVariableRefrigerantFlow::MyEnvrnFlag = true;
    ZoneInletAirNode = GetVRFTUZoneInletAirNode(VRFTUNum); // trigger GetVRFInput by calling a mining function

    Schedule(VRF(VRFCond).SchedPtr).CurrentValue = 1.0;             // enable the VRF condenser
    Schedule(VRFTU(VRFTUNum).SchedPtr).CurrentValue = 1.0;          // enable the terminal unit
    Schedule(VRFTU(VRFTUNum).FanAvailSchedPtr).CurrentValue = 1.0;  // turn on fan
    Schedule(VRFTU(VRFTUNum).FanOpModeSchedPtr).CurrentValue = 0.0; // set cycling fan operating mode

    // Test coil sizing

    DataZoneEnergyDemands::ZoneSysEnergyDemand(CurZoneNum).RemainingOutputRequired = 0.0; // set load = 0
    DataZoneEnergyDemands::ZoneSysEnergyDemand(CurZoneNum).RemainingOutputReqToCoolSP = 0.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(CurZoneNum).RemainingOutputReqToHeatSP = 0.0;

    DataSizing::FinalZoneSizing(CurZoneEqNum).ZoneRetTempAtCoolPeak = 26.66667;
    DataSizing::FinalZoneSizing(CurZoneEqNum).ZoneHumRatAtCoolPeak = 0.01117049470250416; // AHRI condition at 80 F db / 67 F wb
    DataLoopNode::Node(VRF(VRFCond).CondenserNodeNum).Temp = 35.0;                        // AHRI condition at 95 F db
    DataLoopNode::Node(VRFTU(VRFTUNum).VRFTUOAMixerOANodeNum).Temp = 35.0;                // AHRI condition at 95 F db
    DataLoopNode::Node(VRFTU(VRFTUNum).VRFTUOAMixerOANodeNum).HumRat = 0.01;              // don't care
    DataSizing::FinalZoneSizing(CurZoneEqNum).CoolDDNum = 1;
    DataSizing::FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax = 1;
    DataSizing::DesDayWeath.allocate(1);
    DataSizing::DesDayWeath(1).Temp.allocate(1);
    DataSizing::DesDayWeath(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Temp(FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax) = 35.0;

    DataSizing::FinalZoneSizing(CurZoneEqNum).CoolDesTemp = 13.1;                   // 55.58 F
    DataSizing::FinalZoneSizing(CurZoneEqNum).CoolDesHumRat = 0.009297628698818194; // humrat at 12.77777 C db / 12.6 C wb

    SizingManager::GetPlantSizingInput();
    PlantManager::InitOneTimePlantSizingInfo(1);
    PlantManager::SizePlantLoop(1, true);
    PlantManager::InitLoopEquip = true;
    // call air-side VRF
    SimulateVRF(
        VRFTU(VRFTUNum).Name, CurZoneNum, FirstHVACIteration, SysOutputProvided, LatOutputProvided, ZoneEquipList(CurZoneEqNum).EquipIndex(EquipPtr));
    // call plant-side VRF
    SimVRFCondenserPlant(SimPlantEquipTypes(VRF(VRFCond).VRFPlantTypeOfNum),
                         VRF(VRFCond).VRFPlantTypeOfNum,
                         VRF(VRFCond).Name,
                         VRFCond,
                         FirstHVACIteration,
                         InitLoopEquip,
                         CurLoad,
                         MaxLoad,
                         MinLoad,
                         OptLoad,
                         LoopNum);

    DataZoneEnergyDemands::ZoneSysEnergyDemand(CurZoneNum).RemainingOutputRequired = -1000.0; // set cooling load
    DataZoneEnergyDemands::ZoneSysEnergyDemand(CurZoneNum).RemainingOutputReqToCoolSP = -1000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(CurZoneNum).RemainingOutputReqToHeatSP = -2000.0;

    BeginEnvrnFlag = true;
    DataLoopNode::Node(VRFTU(VRFTUNum).VRFTUInletNodeNum).Temp = 24.0;
    DataLoopNode::Node(VRFTU(VRFTUNum).VRFTUInletNodeNum).HumRat = 0.0093;
    DataLoopNode::Node(VRFTU(VRFTUNum).VRFTUInletNodeNum).Enthalpy = 47794.1;
    DataEnvironment::OutDryBulbTemp = 35.0;
    DataEnvironment::OutHumRat = 0.017767; // 50% RH
    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::OutWetBulbTemp = 26.045;
    SimulateVRF(
        VRFTU(VRFTUNum).Name, CurZoneNum, FirstHVACIteration, SysOutputProvided, LatOutputProvided, ZoneEquipList(CurZoneEqNum).EquipIndex(EquipPtr));
    EXPECT_TRUE(VRF(VRFCond).VRFCondPLR > 0.0);
    EXPECT_NEAR(SysOutputProvided, ZoneSysEnergyDemand(CurZoneNum).RemainingOutputReqToCoolSP, 1.0);

    rho = GetDensityGlycol(
        PlantLoop(VRF(VRFCond).SourceLoopNum).FluidName, PlantSizData(1).ExitTemp, PlantLoop(VRF(VRFCond).SourceLoopNum).FluidIndex, RoutineName);
    Cp = GetSpecificHeatGlycol(
        PlantLoop(VRF(VRFCond).SourceLoopNum).FluidName, PlantSizData(1).ExitTemp, PlantLoop(VRF(VRFCond).SourceLoopNum).FluidIndex, RoutineName);
    CondVolFlowRate = max(VRF(VRFCond).CoolingCapacity, VRF(VRFCond).HeatingCapacity) / (PlantSizData(1).DeltaT * Cp * rho);

    EXPECT_DOUBLE_EQ(CondVolFlowRate, VRF(VRFCond).WaterCondVolFlowRate);

    rho = GetDensityGlycol(
        PlantLoop(VRF(VRFCond).SourceLoopNum).FluidName, InitConvTemp, PlantLoop(VRF(VRFCond).SourceLoopNum).FluidIndex, RoutineName);
    EXPECT_DOUBLE_EQ(VRF(VRFCond).WaterCondenserDesignMassFlow, (VRF(VRFCond).WaterCondVolFlowRate * rho));

    // set zone load to heating
    DataZoneEnergyDemands::ZoneSysEnergyDemand(CurZoneNum).RemainingOutputRequired =
        VRF(VRFCond).HeatingCapacity; // set load equal to the VRF heating capacity
    DataZoneEnergyDemands::ZoneSysEnergyDemand(CurZoneNum).RemainingOutputReqToCoolSP =
        VRF(VRFCond).HeatingCapacity + 1000.0; // simulates a dual Tstat with load to cooling SP > load to heating SP
    DataZoneEnergyDemands::ZoneSysEnergyDemand(CurZoneNum).RemainingOutputReqToHeatSP = VRF(VRFCond).HeatingCapacity;

    DataLoopNode::Node(VRF(VRFCond).CondenserNodeNum).Temp = 7.0;             // water inlet temperature
    DataLoopNode::Node(VRFTU(VRFTUNum).VRFTUInletNodeNum).Temp = 20.0;        // TU inlet air temp
    DataLoopNode::Node(VRFTU(VRFTUNum).VRFTUInletNodeNum).HumRat = 0.0056;    // TU inlet air humrat
    DataLoopNode::Node(VRFTU(VRFTUNum).VRFTUInletNodeNum).Enthalpy = 34823.5; // TU inlet air enthalpy
    DataEnvironment::OutDryBulbTemp = 5.0;
    DataEnvironment::OutHumRat = 0.00269; // 50% RH
    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::OutWetBulbTemp = 1.34678;
    SimulateVRF(
        VRFTU(VRFTUNum).Name, CurZoneNum, FirstHVACIteration, SysOutputProvided, LatOutputProvided, ZoneEquipList(CurZoneEqNum).EquipIndex(EquipPtr));

    EXPECT_TRUE(VRF(VRFCond).VRFCondPLR > 0.0);
    EXPECT_NEAR(SysOutputProvided, ZoneSysEnergyDemand(CurZoneNum).RemainingOutputReqToHeatSP, 1.0);

    ASSERT_EQ(VRF(VRFCond).WaterCondenserDesignMassFlow,
              Node(VRF(VRFCond).CondenserNodeNum).MassFlowRate); // Condenser flow rate should be set for active cooling
    ASSERT_EQ(VRF(VRFCond).WaterCondenserDesignMassFlow, Node(VRF(VRFCond).CondenserOutletNodeNum).MassFlowRate); // outlet node should also be set

    // clean up
    ZoneSysEnergyDemand.deallocate();
}

TEST_F(EnergyPlusFixture, VRFTest_TU_NoLoad_OAMassFlowRateTest)
{

    // static std::string const RoutineName( "VRFTest_NoLoadOAFlowTest" );
    bool ErrorsFound(false);       // function returns true on error
    bool FirstHVACIteration(true); // simulate the first pass through HVAC simulation, use false for next iteration
    int VRFTUNum(1);               // index to VRF terminal unit
    int CurZoneNum(1);             // index to zone
    int ZoneInletAirNode(0);       // zone inlet node number
    int OutsideAirNode(0);         // VRFTU Outside air inlet node
    Real64 AverageOAMassFlow(0.0); // VRFTU Outside air mass flow rate
    int ZoneNum(1);                // current zone index
    Real64 QZnReq(0.0);            // current zone load to set point
    Real64 PartLoadRatio(0.0);     // unit part load ratio
    Real64 OnOffAirFlowRatio(1.0); // ratio of compressor ON airflow to average airflow over timestep

    std::string const idf_objects = delimited_string({

        "  Version,8.4;",

        "  ScheduleTypeLimits,",
        "    Fraction,                !- Name",
        "    0.0,                     !- Lower Limit Value",
        "    1.0,                     !- Upper Limit Value",
        "    CONTINUOUS;              !- Numeric Type",

        "  Schedule:Compact,",
        "    AlwaysOn,                !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,1;          !- Field 3",

        "  ZoneHVAC:TerminalUnit:VariableRefrigerantFlow,",
        "    Level1:Office1 VRF Indoor Unit,  !- Zone Terminal Unit Name",
        "    AlwaysOn,                !- Terminal Unit Availability Schedule",
        "    Level1:Office1 VRF Indoor Unit Return,  !- Terminal Unit Air Inlet Node Name",
        "    Level1:Office1 VRF Indoor Unit Supply Inlet,  !- Terminal Unit Air Outlet Node Name",
        "    0.111000,                !- Cooling Supply Air Flow Rate {m3/s}",
        "    0.056000,                !- No Cooling Supply Air Flow Rate {m3/s}",
        "    0.111000,                !- Heating Supply Air Flow Rate {m3/s}",
        "    0.056000,                !- No Heating Supply Air Flow Rate {m3/s}",
        "    0.028000,                !- Cooling Outdoor Air Flow Rate {m3/s}",
        "    0.028000,                !- Heating Outdoor Air Flow Rate {m3/s}",
        "    0.014000,                !- No Load Outdoor Air Flow Rate {m3/s}",
        "    AlwaysOn,                !- Supply Air Fan Operating Mode Schedule Name",
        "    BlowThrough,             !- Supply Air Fan Placement",
        "    Fan:ConstantVolume,      !- Supply Air Fan Object Type",
        "    Level1:Office1 VRF Indoor Unit Supply Fan,  !- Supply Air Fan Object Name",
        "    OutdoorAir:Mixer,        !- Outside Air Mixer Object Type",
        "    Level1:Office1 VRF Indoor Unit Outdoor Air Mixer,  !- Outside Air Mixer Object Name",
        "    Coil:Cooling:DX:VariableRefrigerantFlow,  !- Cooling Coil Object Type",
        "    Level1:Office1 VRF Indoor Unit DX Cooling Coil,  !- Cooling Coil Object Name",
        "    Coil:Heating:DX:VariableRefrigerantFlow,  !- Heating Coil Object Type",
        "    Level1:Office1 VRF Indoor Unit DX Heating Coil,  !- Heating Coil Object Name",
        "    30.0000,                 !- Zone Terminal Unit On Parasitic Electric Energy Use {W}",
        "    20.0000;                 !- Zone Terminal Unit Off Parasitic Electric Energy Use {W}",

        "  ZoneHVAC:EquipmentList,",
        "    Level1:Office1 Equipment,!- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:TerminalUnit:VariableRefrigerantFlow,  !- Zone Equipment 1 Object Type",
        "    Level1:Office1 VRF Indoor Unit,  !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",

        "  ZoneHVAC:EquipmentConnections,",
        "    Level1:Office1,          !- Zone Name",
        "    Level1:Office1 Equipment,!- Zone Conditioning Equipment List Name",
        "    Level1:Office1 Air Inlet Node List,  !- Zone Air Inlet Node or NodeList Name",
        "    Level1:Office1 Air Exhaust Node List,  !- Zone Air Exhaust Node or NodeList Name",
        "    Level1:Office1 Zone Air Node,  !- Zone Air Node Name",
        "    Level1:Office1 Return Outlet;  !- Zone Return Air Node Name",

        "  Zone,",
        "    Level1:Office1,          !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    ,                        !- Ceiling Height {m}",
        "    68.2413,                 !- Volume {m3}",
        "    19.4975,                 !- Floor Area {m2}",
        "    TARP;                    !- Zone Inside Convection Algorithm",

        "  Fan:ConstantVolume,",
        "    Level1:Office1 VRF Indoor Unit Supply Fan,  !- Name",
        "    AlwaysOn,                 !- Availability Schedule Name",
        "    0.70,                    !- Fan Total Efficiency",
        "    100.00,                  !- Pressure Rise {Pa}",
        "    0.111000,                !- Maximum Flow Rate {m3/s}",
        "    0.90,                    !- Motor Efficiency",
        "    1.00,                    !- Motor In Airstream Fraction",
        "    Level1:Office1 VRF Indoor Unit Mixed Air Outlet,  !- Air Inlet Node Name",
        "    Level1:Office1 VRF Indoor Unit Supply Fan Outlet,  !- Air Outlet Node Name",
        "    General;                 !- End-Use Subcategory",

        "  Coil:Cooling:DX:VariableRefrigerantFlow,",
        "    Level1:Office1 VRF Indoor Unit DX Cooling Coil,  !- Name",
        "    AlwaysOn,                 !- Availability Schedule Name",
        "    4000.0,                  !- Gross Rated Total Cooling Capacity {W}",
        "    0.75,                    !- Gross Rated Sensible Heat Ratio",
        "    0.111000,                !- Rated Air Flow Rate {m3/s}",
        "    VRFTUCoolCapFT,          !- Cooling Capacity Ratio Modifier Function of Temperature Curve Name",
        "    VRFACCoolCapFFF,         !- Cooling Capacity Modifier Curve Function of Flow Fraction Name",
        "    Level1:Office1 VRF Indoor Unit Supply Fan Outlet,  !- Coil Air Inlet Node",
        "    Level1:Office1 VRF Indoor Unit DX Cooling Coil Outlet;  !- Coil Air Outlet Node",

        "  Coil:Heating:DX:VariableRefrigerantFlow,",
        "    Level1:Office1 VRF Indoor Unit DX Heating Coil,  !- Name",
        "    AlwaysOn,                !- Availability Schedule",
        "    4000.0,                  !- Gross Rated Heating Capacity {W}",
        "    0.111000,                !- Rated Air Flow Rate {m3/s}",
        "    Level1:Office1 VRF Indoor Unit DX Cooling Coil Outlet,  !- Coil Air Inlet Node",
        "    Level1:Office1 VRF Indoor Unit Supply Inlet,  !- Coil Air Outlet Node",
        "    VRFTUHeatCapFT,          !- Heating Capacity Ratio Modifier Function of Temperature Curve Name",
        "    VRFACCoolCapFFF;         !- Heating Capacity Modifier Function of Flow Fraction Curve Name",

        "  AirConditioner:VariableRefrigerantFlow,",
        "    VRF Outdoor Unit,        !- Heat Pump Name",
        "    AlwaysOn,                !- Availability Schedule Name",
        "    7040.0,                  !- Gross Rated Total Cooling Capacity {W}",
        "    3.3,                     !- Gross Rated Cooling COP {W/W}",
        "    -6,                      !- Minimum Outdoor Temperature in Cooling Mode {C}",
        "    43,                      !- Maximum Outdoor Temperature in Cooling Mode {C}",
        "    VRFCoolCapFT,            !- Cooling Capacity Ratio Modifier Function of Low Temperature Curve Name",
        "    VRFCoolCapFTBoundary,    !- Cooling Capacity Ratio Boundary Curve Name",
        "    VRFCoolCapFTHi,          !- Cooling Capacity Ratio Modifier Function of High Temperature Curve Name",
        "    VRFCoolEIRFT,            !- Cooling Energy Input Ratio Modifier Function of Low Temperature Curve Name",
        "    VRFCoolEIRFTBoundary,    !- Cooling Energy Input Ratio Boundary Curve Name",
        "    VRFCoolEIRFTHi,          !- Cooling Energy Input Ratio Modifier Function of High Temperature Curve Name",
        "    CoolingEIRLowPLR,        !- Cooling Energy Input Ratio Modifier Function of Low Part-Load Ratio Curve Name",
        "    CoolingEIRHiPLR,         !- Cooling Energy Input Ratio Modifier Function of High Part-Load Ratio Curve Name",
        "    CoolingCombRatio,        !- Cooling Combination Ratio Correction Factor Curve Name",
        "    VRFCPLFFPLR,             !- Cooling Part-Load Fraction Correlation Curve Name",
        "    autosize,                !- Gross Rated Heating Capacity {W}",
        "    1,                       !- Rated Heating Capacity Sizing Ratio {W/W}",
        "    3.4,                     !- Gross Rated Heating COP {W/W}",
        "    -20,                     !- Minimum Outdoor Temperature in Heating Mode {C}",
        "    40,                      !- Maximum Outdoor Temperature in Heating Mode {C}",
        "    VRFHeatCapFT,            !- Heating Capacity Ratio Modifier Function of Low Temperature Curve Name",
        "    VRFHeatCapFTBoundary,    !- Heating Capacity Ratio Boundary Curve Name",
        "    VRFHeatCapFTHi,          !- Heating Capacity Ratio Modifier Function of High Temperature Curve Name",
        "    VRFHeatEIRFT,            !- Heating Energy Input Ratio Modifier Function of Low Temperature Curve Name",
        "    VRFHeatEIRFTBoundary,    !- Heating Energy Input Ratio Boundary Curve Name",
        "    VRFHeatEIRFTHi,          !- Heating Energy Input Ratio Modifier Function of High Temperature Curve Name",
        "    WetBulbTemperature,      !- Heating Performance Curve Outdoor Temperature Type",
        "    HeatingEIRLowPLR,        !- Heating Energy Input Ratio Modifier Function of Low Part-Load Ratio Curve Name",
        "    HeatingEIRHiPLR,         !- Heating Energy Input Ratio Modifier Function of High Part-Load Ratio Curve Name",
        "    HeatingCombRatio,        !- Heating Combination Ratio Correction Factor Curve Name",
        "    VRFCPLFFPLR,             !- Heating Part-Load Fraction Correlation Curve Name",
        "    0.15,                    !- Minimum Heat Pump Part-Load Ratio {dimensionless}",
        "    <Select zone>,           !- Zone Name for Master Thermostat Location",
        "    LoadPriority,            !- Master Thermostat Priority Control Type",
        "    ,                        !- Thermostat Priority Schedule Name",
        "    VRF Outdoor Unit Zone List,  !- Zone Terminal Unit List Name",
        "    No,                      !- Heat Pump Waste Heat Recovery",
        "    50,                      !- Equivalent Piping Length used for Piping Correction Factor in Cooling Mode {m}",
        "    15,                      !- Vertical Height used for Piping Correction Factor {m}",
        "    CoolingLengthCorrectionFactor,  !- Piping Correction Factor for Length in Cooling Mode Curve Name",
        "    -.000386,                !- Piping Correction Factor for Height in Cooling Mode Coefficient {1/m}",
        "    50,                      !- Equivalent Piping Length used for Piping Correction Factor in Heating Mode {m}",
        "    VRF Piping Correction Factor for Length in Heating Mode,  !- Piping Correction Factor for Length in Heating Mode Curve Name",
        "    0,                       !- Piping Correction Factor for Height in Heating Mode Coefficient {1/m}",
        "    15,                      !- Crankcase Heater Power per Compressor {W}",
        "    2,                       !- Number of Compressors {dimensionless}",
        "    0.5,                     !- Ratio of Compressor Size to Total Compressor Capacity {W/W}",
        "    5,                       !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater {C}",
        "    Resistive,               !- Defrost Strategy",
        "    Timed,                   !- Defrost Control",
        "    DXHtgCoilDefrostEIRFT,   !- Defrost Energy Input Ratio Modifier Function of Temperature Curve Name",
        "    0.058333,                !- Defrost Time Period Fraction {dimensionless}",
        "    autosize,                !- Resistive Defrost Heater Capacity {W}",
        "    5,                       !- Maximum Outdoor Dry-bulb Temperature for Defrost Operation {C}",
        "    AirCooled,               !- Condenser Type",
        "    VRF Outdoor Unit Outdoor Air Node,  !- Condenser Inlet Node Name",
        "    ,                        !- Condenser Outlet Node Name",
        "    autosize,                !- Water Condenser Volume Flow Rate {m3/s}",
        "    0.9,                     !- Evaporative Condenser Effectiveness {dimensionless}",
        "    autosize,                !- Evaporative Condenser Air Flow Rate {m3/s}",
        "    autosize,                !- Evaporative Condenser Pump Rated Power Consumption {W}",
        "    ,                        !- Supply Water Storage Tank Name",
        "    0,                       !- Basin Heater Capacity {W/K}",
        "    2,                       !- Basin Heater Setpoint Temperature {C}",
        "    AlwaysOn,                 !- Basin Heater Operating Schedule Name",
        "    Electricity,             !- Fuel Type",
        "    -10,                     !- Minimum Outdoor Temperature in Heat Recovery Mode {C}",
        "    40,                      !- Maximum Outdoor Temperature in Heat Recovery Mode {C}",
        "    VRF Heat Recovery Cooling Capacity Modifier,  !- Heat Recovery Cooling Capacity Modifier Curve Name",
        "    0.5,                     !- Initial Heat Recovery Cooling Capacity Fraction {W/W}",
        "    0.15,                    !- Heat Recovery Cooling Capacity Time Constant {hr}",
        "    VRF Heat Recovery Cooling Energy Modifier,  !- Heat Recovery Cooling Energy Modifier Curve Name",
        "    1,                       !- Initial Heat Recovery Cooling Energy Fraction {W/W}",
        "    0,                       !- Heat Recovery Cooling Energy Time Constant {hr}",
        "    VRF Heat Recovery Heating Capacity Modifier,  !- Heat Recovery Heating Capacity Modifier Curve Name",
        "    1,                       !- Initial Heat Recovery Heating Capacity Fraction {W/W}",
        "    0.15,                    !- Heat Recovery Heating Capacity Time Constant {hr}",
        "    VRF Heat Recovery Heating Energy Modifier,  !- Heat Recovery Heating Energy Modifier Curve Name",
        "    1,                       !- Initial Heat Recovery Heating Energy Fraction {W/W}",
        "    0;                       !- Heat Recovery Heating Energy Time Constant {hr}",

        "  ZoneTerminalUnitList,",
        "    VRF Outdoor Unit Zone List,  !- Zone Terminal Unit List Name",
        "    Level1:Office1 VRF Indoor Unit;  !- Zone Terminal Unit Name 1",

        "  OutdoorAir:Mixer,",
        "    Level1:Office1 VRF Indoor Unit Outdoor Air Mixer,  !- Name",
        "    Level1:Office1 VRF Indoor Unit Mixed Air Outlet,  !- Mixed Air Node Name",
        "    Level1:Office1 VRF Indoor Unit Outdoor Air Node Name,  !- Outdoor Air Stream Node Name",
        "    Level1:Office1 VRF Indoor Unit Air Relief Node Name,  !- Relief Air Stream Node Name",
        "    Level1:Office1 VRF Indoor Unit Return;  !- Return Air Stream Node Name",

        "  NodeList,",
        "    Level1:Office1 Air Inlet Node List,  !- Name",
        "    Level1:Office1 VRF Indoor Unit Supply Inlet;  !- Node 1 Name",

        "  NodeList,",
        "    Level1:Office1 Air Exhaust Node List,  !- Name",
        "    Level1:Office1 VRF Indoor Unit Return;  !- Node 1 Name",

        "  OutdoorAir:NodeList,",
        "    VRF Outdoor Unit Outdoor Air Node;  !- Node or NodeList Name 1",

        "  OutdoorAir:NodeList,",
        "    Level1:Office1 VRF Indoor Unit Outdoor Air Node Name;  !- Node or NodeList Name 1",

        "  Curve:Linear,",
        "    CoolingCombRatio,        !- Name",
        "    0.618055,                !- Coefficient1 Constant",
        "    0.381945,                !- Coefficient2 x",
        "    1.0,                     !- Minimum Value of x",
        "    1.5;                     !- Maximum Value of x",

        "  Curve:Linear,",
        "    HeatingCombRatio,        !- Name",
        "    0.96034,                 !- Coefficient1 Constant",
        "    0.03966,                 !- Coefficient2 x",
        "    1.0,                     !- Minimum Value of x",
        "    1.5;                     !- Maximum Value of x",

        "  Curve:Quadratic,",
        "    VRFACCoolCapFFF,         !- Name",
        "    0.8,                     !- Coefficient1 Constant",
        "    0.2,                     !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.5,                     !- Minimum Value of x",
        "    1.5;                     !- Maximum Value of x",

        "  Curve:Quadratic,",
        "    CoolingEIRHiPLR,         !- Name",
        "    1.0,                     !- Coefficient1 Constant",
        "    0.0,                     !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    1.0,                     !- Minimum Value of x",
        "    1.5;                     !- Maximum Value of x",

        "  Curve:Quadratic,",
        "    VRFCPLFFPLR,             !- Name",
        "    0.85,                    !- Coefficient1 Constant",
        "    0.15,                    !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.0,                     !- Minimum Value of x",
        "    1.0;                     !- Maximum Value of x",

        "  Curve:Quadratic,",
        "    HeatingEIRHiPLR,         !- Name",
        "    2.4294355,               !- Coefficient1 Constant",
        "    -2.235887,               !- Coefficient2 x",
        "    0.8064516,               !- Coefficient3 x**2",
        "    1.0,                     !- Minimum Value of x",
        "    1.5;                     !- Maximum Value of x",

        "  Curve:Cubic,",
        "    DefaultFanEffRatioCurve, !- Name",
        "    0.33856828,              !- Coefficient1 Constant",
        "    1.72644131,              !- Coefficient2 x",
        "    -1.49280132,             !- Coefficient3 x**2",
        "    0.42776208,              !- Coefficient4 x**3",
        "    0.5,                     !- Minimum Value of x",
        "    1.5,                     !- Maximum Value of x",
        "    0.3,                     !- Minimum Curve Output",
        "    1.0;                     !- Maximum Curve Output",

        "  Curve:Cubic,",
        "    VRFTUCoolCapFT,          !- Name",
        "    0.504547273506488,       !- Coefficient1 Constant",
        "    0.0288891279198444,      !- Coefficient2 x",
        "    -0.000010819418650677,   !- Coefficient3 x**2",
        "    0.0000101359395177008,   !- Coefficient4 x**3",
        "    0.0,                     !- Minimum Value of x",
        "    50.0,                    !- Maximum Value of x",
        "    0.5,                     !- Minimum Curve Output",
        "    1.5,                     !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Dimensionless;           !- Output Unit Type",

        "  Curve:Cubic,",
        "    VRFTUHeatCapFT,          !- Name",
        "    -0.390708928227928,      !- Coefficient1 Constant",
        "    0.261815023760162,       !- Coefficient2 x",
        "    -0.0130431603151873,     !- Coefficient3 x**2",
        "    0.000178131745997821,    !- Coefficient4 x**3",
        "    0.0,                     !- Minimum Value of x",
        "    50.0,                    !- Maximum Value of x",
        "    0.5,                     !- Minimum Curve Output",
        "    1.5,                     !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Dimensionless;           !- Output Unit Type",

        "  Curve:Cubic,",
        "    VRFCoolCapFTBoundary,    !- Name",
        "    25.73473775,             !- Coefficient1 Constant",
        "    -0.03150043,             !- Coefficient2 x",
        "    -0.01416595,             !- Coefficient3 x**2",
        "    0,                       !- Coefficient4 x**3",
        "    11,                      !- Minimum Value of x",
        "    30,                      !- Maximum Value of x",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature;             !- Output Unit Type",

        "  Curve:Cubic,",
        "    VRFCoolEIRFTBoundary,    !- Name",
        "    25.73473775,             !- Coefficient1 Constant",
        "    -0.03150043,             !- Coefficient2 x",
        "    -0.01416595,             !- Coefficient3 x**2",
        "    0,                       !- Coefficient4 x**3",
        "    15,                      !- Minimum Value of x",
        "    24,                      !- Maximum Value of x",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature;             !- Output Unit Type",

        "  Curve:Cubic,",
        "    CoolingEIRLowPLR,        !- Name",
        "    0.4628123,               !- Coefficient1 Constant",
        "    -1.0402406,              !- Coefficient2 x",
        "    2.17490997,              !- Coefficient3 x**2",
        "    -0.5974817,              !- Coefficient4 x**3",
        "    0,                       !- Minimum Value of x",
        "    1,                       !- Maximum Value of x",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature;             !- Output Unit Type",

        "  Curve:Cubic,",
        "    VRFHeatCapFTBoundary,    !- Name",
        "    -7.6000882,              !- Coefficient1 Constant",
        "    3.05090016,              !- Coefficient2 x",
        "    -0.1162844,              !- Coefficient3 x**2",
        "    0.0,                     !- Coefficient4 x**3",
        "    15,                      !- Minimum Value of x",
        "    27,                      !- Maximum Value of x",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature;             !- Output Unit Type",

        "  Curve:Cubic,",
        "    VRFHeatEIRFTBoundary,    !- Name",
        "    -7.6000882,              !- Coefficient1 Constant",
        "    3.05090016,              !- Coefficient2 x",
        "    -0.1162844,              !- Coefficient3 x**2",
        "    0.0,                     !- Coefficient4 x**3",
        "    15,                      !- Minimum Value of x",
        "    27,                      !- Maximum Value of x",
        "    -20,                     !- Minimum Curve Output",
        "    15,                      !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature;             !- Output Unit Type",

        "  Curve:Cubic,",
        "    HeatingEIRLowPLR,        !- Name",
        "    0.1400093,               !- Coefficient1 Constant",
        "    0.6415002,               !- Coefficient2 x",
        "    0.1339047,               !- Coefficient3 x**2",
        "    0.0845859,               !- Coefficient4 x**3",
        "    0,                       !- Minimum Value of x",
        "    1,                       !- Maximum Value of x",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Dimensionless,           !- Input Unit Type for X",
        "    Dimensionless;           !- Output Unit Type",

        "  Curve:Exponent,",
        "    DefaultFanPowerRatioCurve,  !- Name",
        "    0,                       !- Coefficient1 Constant",
        "    1,                       !- Coefficient2 Constant",
        "    3,                       !- Coefficient3 Constant",
        "    0,                       !- Minimum Value of x",
        "    1.5,                     !- Maximum Value of x",
        "    0.01,                    !- Minimum Curve Output",
        "    1.5;                     !- Maximum Curve Output",

        "  Curve:Biquadratic,",
        "    DXHtgCoilDefrostEIRFT,   !- Name",
        "    1.0,                     !- Coefficient1 Constant",
        "    0.0,                     !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.0,                     !- Coefficient4 y",
        "    0,                       !- Coefficient5 y**2",
        "    0,                       !- Coefficient6 x*y",
        "    0.0,                     !- Minimum Value of x",
        "    50.0,                    !- Maximum Value of x",
        "    0.0,                     !- Minimum Value of y",
        "    50.0,                    !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "  Curve:Biquadratic,",
        "    VRFCoolCapFT,            !- Name",
        "    0.576882692,             !- Coefficient1 Constant",
        "    0.017447952,             !- Coefficient2 x",
        "    0.000583269,             !- Coefficient3 x**2",
        "    -1.76324E-06,            !- Coefficient4 y",
        "    -7.474E-09,              !- Coefficient5 y**2",
        "    -1.30413E-07,            !- Coefficient6 x*y",
        "    15,                      !- Minimum Value of x",
        "    24,                      !- Maximum Value of x",
        "    -5,                      !- Minimum Value of y",
        "    23,                      !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "  Curve:Biquadratic,",
        "    VRFCoolCapFTHi,          !- Name",
        "    0.6867358,               !- Coefficient1 Constant",
        "    0.0207631,               !- Coefficient2 x",
        "    0.0005447,               !- Coefficient3 x**2",
        "    -0.0016218,              !- Coefficient4 y",
        "    -4.259E-07,              !- Coefficient5 y**2",
        "    -0.0003392,              !- Coefficient6 x*y",
        "    15,                      !- Minimum Value of x",
        "    24,                      !- Maximum Value of x",
        "    16,                      !- Minimum Value of y",
        "    43,                      !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "  Curve:Biquadratic,",
        "    VRFCoolEIRFT,            !- Name",
        "    0.989010541,             !- Coefficient1 Constant",
        "    -0.02347967,             !- Coefficient2 x",
        "    0.000199711,             !- Coefficient3 x**2",
        "    0.005968336,             !- Coefficient4 y",
        "    -1.0289E-07,             !- Coefficient5 y**2",
        "    -0.00015686,             !- Coefficient6 x*y",
        "    15,                      !- Minimum Value of x",
        "    24,                      !- Maximum Value of x",
        "    -5,                      !- Minimum Value of y",
        "    23,                      !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "  Curve:Biquadratic,",
        "    VRFCoolEIRFTHi,          !- Name",
        "    0.14351470,              !- Coefficient1 Constant",
        "    0.01860035,              !- Coefficient2 x",
        "    -0.0003954,              !- Coefficient3 x**2",
        "    0.02485219,              !- Coefficient4 y",
        "    0.00016329,              !- Coefficient5 y**2",
        "    -0.0006244,              !- Coefficient6 x*y",
        "    15,                      !- Minimum Value of x",
        "    24,                      !- Maximum Value of x",
        "    16,                      !- Minimum Value of y",
        "    43,                      !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "  Curve:Biquadratic,",
        "    VRFHeatCapFT,            !- Name",
        "    1.014599599,             !- Coefficient1 Constant",
        "    -0.002506703,            !- Coefficient2 x",
        "    -0.000141599,            !- Coefficient3 x**2",
        "    0.026931595,             !- Coefficient4 y",
        "    1.83538E-06,             !- Coefficient5 y**2",
        "    -0.000358147,            !- Coefficient6 x*y",
        "    15,                      !- Minimum Value of x",
        "    27,                      !- Maximum Value of x",
        "    -20,                     !- Minimum Value of y",
        "    15,                      !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "  Curve:Biquadratic,",
        "    VRFHeatCapFTHi,          !- Name",
        "    1.161134821,             !- Coefficient1 Constant",
        "    0.027478868,             !- Coefficient2 x",
        "    -0.00168795,             !- Coefficient3 x**2",
        "    0.001783378,             !- Coefficient4 y",
        "    2.03208E-06,             !- Coefficient5 y**2",
        "    -6.8969E-05,             !- Coefficient6 x*y",
        "    15,                      !- Minimum Value of x",
        "    27,                      !- Maximum Value of x",
        "    -10,                     !- Minimum Value of y",
        "    15,                      !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "  Curve:Biquadratic,",
        "    VRFHeatEIRFT,            !- Name",
        "    0.87465501,              !- Coefficient1 Constant",
        "    -0.01319754,             !- Coefficient2 x",
        "    0.00110307,              !- Coefficient3 x**2",
        "    -0.0133118,              !- Coefficient4 y",
        "    0.00089017,              !- Coefficient5 y**2",
        "    -0.00012766,             !- Coefficient6 x*y",
        "    15,                      !- Minimum Value of x",
        "    27,                      !- Maximum Value of x",
        "    -20,                     !- Minimum Value of y",
        "    12,                      !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "  Curve:Biquadratic,",
        "    VRFHeatEIRFTHi,          !- Name",
        "    2.504005146,             !- Coefficient1 Constant",
        "    -0.05736767,             !- Coefficient2 x",
        "    4.07336E-05,             !- Coefficient3 x**2",
        "    -0.12959669,             !- Coefficient4 y",
        "    0.00135839,              !- Coefficient5 y**2",
        "    0.00317047,              !- Coefficient6 x*y",
        "    15,                      !- Minimum Value of x",
        "    27,                      !- Maximum Value of x",
        "    -10,                     !- Minimum Value of y",
        "    15,                      !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "  Curve:Biquadratic,",
        "    CoolingLengthCorrectionFactor,  !- Name",
        "    1.0693794,               !- Coefficient1 Constant",
        "    -0.0014951,              !- Coefficient2 x",
        "    2.56E-06,                !- Coefficient3 x**2",
        "    -0.1151104,              !- Coefficient4 y",
        "    0.0511169,               !- Coefficient5 y**2",
        "    -0.0004369,              !- Coefficient6 x*y",
        "    8,                       !- Minimum Value of x",
        "    175,                     !- Maximum Value of x",
        "    0.5,                     !- Minimum Value of y",
        "    1.5,                     !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "  Curve:Biquadratic,",
        "    VRF Piping Correction Factor for Length in Heating Mode,  !- Name",
        "    0.989916,                !- Coefficient1 Constant",
        "    0.001961,                !- Coefficient2 x",
        "    -.000036,                !- Coefficient3 x**2",
        "    0,                       !- Coefficient4 y",
        "    0,                       !- Coefficient5 y**2",
        "    0,                       !- Coefficient6 x*y",
        "    7,                       !- Minimum Value of x",
        "    106.5,                   !- Maximum Value of x",
        "    1,                       !- Minimum Value of y",
        "    1,                       !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Distance,                !- Input Unit Type for X",
        "    Dimensionless,           !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "  Curve:Biquadratic,",
        "    VRF Heat Recovery Cooling Capacity Modifier,  !- Name",
        "    0.9,                     !- Coefficient1 Constant",
        "    0,                       !- Coefficient2 x",
        "    0,                       !- Coefficient3 x**2",
        "    0,                       !- Coefficient4 y",
        "    0,                       !- Coefficient5 y**2",
        "    0,                       !- Coefficient6 x*y",
        "    -100,                    !- Minimum Value of x",
        "    100,                     !- Maximum Value of x",
        "    -100,                    !- Minimum Value of y",
        "    100,                     !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "  Curve:Biquadratic,",
        "    VRF Heat Recovery Cooling Energy Modifier,  !- Name",
        "    1.1,                     !- Coefficient1 Constant",
        "    0,                       !- Coefficient2 x",
        "    0,                       !- Coefficient3 x**2",
        "    0,                       !- Coefficient4 y",
        "    0,                       !- Coefficient5 y**2",
        "    0,                       !- Coefficient6 x*y",
        "    -100,                    !- Minimum Value of x",
        "    100,                     !- Maximum Value of x",
        "    -100,                    !- Minimum Value of y",
        "    100,                     !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "  Curve:Biquadratic,",
        "    VRF Heat Recovery Heating Capacity Modifier,  !- Name",
        "    0.9,                     !- Coefficient1 Constant",
        "    0,                       !- Coefficient2 x",
        "    0,                       !- Coefficient3 x**2",
        "    0,                       !- Coefficient4 y",
        "    0,                       !- Coefficient5 y**2",
        "    0,                       !- Coefficient6 x*y",
        "    -100,                    !- Minimum Value of x",
        "    100,                     !- Maximum Value of x",
        "    -100,                    !- Minimum Value of y",
        "    100,                     !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "  Curve:Biquadratic,",
        "    VRF Heat Recovery Heating Energy Modifier,  !- Name",
        "    1.1,                     !- Coefficient1 Constant",
        "    0,                       !- Coefficient2 x",
        "    0,                       !- Coefficient3 x**2",
        "    0,                       !- Coefficient4 y",
        "    0,                       !- Coefficient5 y**2",
        "    0,                       !- Coefficient6 x*y",
        "    -100,                    !- Minimum Value of x",
        "    100,                     !- Maximum Value of x",
        "    -100,                    !- Minimum Value of y",
        "    100,                     !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    DataGlobals::BeginEnvrnFlag = true;
    DataSizing::CurZoneEqNum = 1;
    DataEnvironment::OutBaroPress = 101325;          // sea level
    DataZoneEquipment::ZoneEquipInputsFilled = true; // denotes zone equipment has been read in
    DataEnvironment::StdRhoAir = PsyRhoAirFnPbTdbW(DataEnvironment::OutBaroPress, 20.0, 0.0);
    DataGlobals::SysSizingCalc = true;
    DataGlobals::NumOfTimeStepInHour = 1;
    DataGlobals::MinutesPerTimeStep = 60;

    CurveManager::GetCurveInput();                // read curves
    HeatBalanceManager::GetZoneData(ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);

    DataZoneEquipment::GetZoneEquipmentData(); // read equipment list and connections
    HVACVariableRefrigerantFlow::MyEnvrnFlag = true;
    ZoneInletAirNode = GetVRFTUZoneInletAirNode(VRFTUNum);  // trigger GetVRFInput by calling a mining function
    OutsideAirNode = VRFTU(VRFTUNum).VRFTUOAMixerOANodeNum; // outside air air inlet node num
    DataZoneEnergyDemands::ZoneSysEnergyDemand.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(CurZoneNum).RemainingOutputRequired = 0.0;    // No load
    DataZoneEnergyDemands::ZoneSysEnergyDemand(CurZoneNum).RemainingOutputReqToCoolSP = 0.0; // No load
    DataZoneEnergyDemands::ZoneSysEnergyDemand(CurZoneNum).RemainingOutputReqToHeatSP = 0.0; // No load
    QZnReq = DataZoneEnergyDemands::ZoneSysEnergyDemand(CurZoneNum).RemainingOutputRequired; // No load
    // Initialize terminal unit
    Schedule(VRFTU(VRFTUNum).FanOpModeSchedPtr).CurrentValue = 1.0;            // set continuous fan operating mode
    InitVRF(VRFTUNum, ZoneNum, FirstHVACIteration, OnOffAirFlowRatio, QZnReq); // Initialize all VRFTU related parameters
    ASSERT_EQ(VRFTU(VRFTUNum).OpMode, DataHVACGlobals::ContFanCycCoil);        // continuous fan cycling coil operating mode
    // Set average OA flow rate when there in no load for cont. fan cyc. coil operating mode
    SetAverageAirFlow(VRFTUNum, PartLoadRatio, OnOffAirFlowRatio);
    AverageOAMassFlow = DataEnvironment::StdRhoAir * VRFTU(VRFTUNum).NoCoolHeatOutAirVolFlow;
    EXPECT_EQ(AverageOAMassFlow, Node(OutsideAirNode).MassFlowRate);

    // clean up
    ZoneSysEnergyDemand.deallocate();
}

TEST_F(EnergyPlusFixture, VRFTest_CondenserCalcTest)
{

    std::string const idf_objects = delimited_string({

        "  Curve:Biquadratic,",
        "    BiquadraticCurve,        !- Name",
        "    1.0,                     !- Coefficient1 Constant",
        "    0,                       !- Coefficient2 x",
        "    0,                       !- Coefficient3 x**2",
        "    0,                       !- Coefficient4 y",
        "    0,                       !- Coefficient5 y**2",
        "    0,                       !- Coefficient6 x*y",
        "    -100,                    !- Minimum Value of x",
        "    100,                     !- Maximum Value of x",
        "    -100,                    !- Minimum Value of y",
        "    100,                     !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "  Curve:Cubic,",
        "    EIRfPLR,                 !- Name",
        "    0.0,                     !- Coefficient1 Constant",
        "    1.0,                     !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.0,                     !- Coefficient4 x**3",
        "    0,                       !- Minimum Value of x",
        "    1,                       !- Maximum Value of x",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Dimensionless,           !- Input Unit Type for X",
        "    Dimensionless;           !- Output Unit Type",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    CurveManager::GetCurveInput();

    int VRFCond = 1;
    VRF.allocate(1);
    VRF(VRFCond).CondenserNodeNum = 0;
    VRF(VRFCond).CondenserType = 1; // DataHVACGlobals::AirCooled
    VRF(VRFCond).ZoneTUListPtr = 1;
    VRF(VRFCond).CoolingCapacity = 20000.0;
    VRF(VRFCond).HeatingCapacity = 20000.0;
    VRF(VRFCond).CoolingCOP = 3.0;
    VRF(VRFCond).HeatingCOP = 3.0;
    VRF(VRFCond).RatedCoolingPower = VRF(VRFCond).CoolingCapacity / VRF(VRFCond).CoolingCOP;
    VRF(VRFCond).RatedHeatingPower = VRF(VRFCond).HeatingCapacity / VRF(VRFCond).HeatingCOP;
    VRF(VRFCond).PipingCorrectionCooling = 1.0;
    VRF(VRFCond).PipingCorrectionHeating = 1.0;
    VRF(VRFCond).CoolCapFT = 1;
    VRF(VRFCond).CoolEIRFT = 1;
    VRF(VRFCond).CoolEIRFPLR1 = 2;
    VRF(VRFCond).HeatEIRFPLR1 = 2;
    CoolCombinationRatio.allocate(1);
    CoolCombinationRatio(VRFCond) = 1.0;
    HeatCombinationRatio.allocate(1);
    HeatCombinationRatio(VRFCond) = 1.0;
    LastModeCooling.allocate(1);
    LastModeHeating.allocate(1);

    TerminalUnitList.allocate(1);
    TerminalUnitList(1).NumTUInList = 5;
    TerminalUnitList(1).TotalCoolLoad.allocate(5);
    TerminalUnitList(1).TotalHeatLoad.allocate(5);
    TerminalUnitList(1).ZoneTUPtr.allocate(5);
    TerminalUnitList(1).HRCoolRequest.allocate(5);
    TerminalUnitList(1).HRHeatRequest.allocate(5);
    TerminalUnitList(1).HRCoolRequest = false;
    TerminalUnitList(1).HRHeatRequest = false;

    TerminalUnitList(1).CoolingCoilAvailable.allocate(5);
    TerminalUnitList(1).HeatingCoilAvailable.allocate(5);
    // all TU coils are available
    TerminalUnitList(1).CoolingCoilAvailable = true;
    TerminalUnitList(1).HeatingCoilAvailable = true;

    CoolingLoad.allocate(1);
    HeatingLoad.allocate(1);
    CoolingLoad(VRFCond) = false;
    HeatingLoad(VRFCond) = false;
    LastModeCooling(VRFCond) = false;
    LastModeHeating(VRFCond) = false;

    DXCoilCoolInletAirWBTemp.allocate(10);
    DXCoilHeatInletAirDBTemp.allocate(10);
    DXCoilHeatInletAirWBTemp.allocate(10);

    VRFTU.allocate(5);
    for (int NumTU = 1; NumTU <= TerminalUnitList(1).NumTUInList; ++NumTU) {
        VRFTU(NumTU).CoolCoilIndex = NumTU;
        VRFTU(NumTU).HeatCoilIndex = TerminalUnitList(1).NumTUInList + NumTU;
        TerminalUnitList(1).ZoneTUPtr(NumTU) = NumTU;
        // initialize DX coil inlet conditions
        DXCoilCoolInletAirWBTemp(NumTU) = 19.4;
        DXCoilHeatInletAirDBTemp(TerminalUnitList(1).NumTUInList + NumTU) = 20.0;
        DXCoilHeatInletAirWBTemp(TerminalUnitList(1).NumTUInList + NumTU) = 17.0;
    }

    // set up environment
    DataGlobals::DayOfSim = 1;
    DataGlobals::CurrentTime = 0.25;
    DataGlobals::TimeStepZone = 0.25;
    DataHVACGlobals::SysTimeElapsed = 0.0;
    DataEnvironment::OutDryBulbTemp = 35.0;
    DataEnvironment::OutHumRat = 0.01;
    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::OutWetBulbTemp = 21.1340575;

    // TU's are off
    TerminalUnitList(1).TotalCoolLoad(1) = 0.0;
    TerminalUnitList(1).TotalCoolLoad(2) = 0.0;
    TerminalUnitList(1).TotalCoolLoad(3) = 0.0;
    TerminalUnitList(1).TotalCoolLoad(4) = 0.0;
    TerminalUnitList(1).TotalCoolLoad(5) = 0.0;
    TerminalUnitList(1).TotalHeatLoad(1) = 0.0;
    TerminalUnitList(1).TotalHeatLoad(2) = 0.0;
    TerminalUnitList(1).TotalHeatLoad(3) = 0.0;
    TerminalUnitList(1).TotalHeatLoad(4) = 0.0;
    TerminalUnitList(1).TotalHeatLoad(5) = 0.0;

    CalcVRFCondenser(VRFCond, false);

    EXPECT_DOUBLE_EQ(VRF(VRFCond).ElecCoolingPower, 0.0);
    EXPECT_DOUBLE_EQ(VRF(VRFCond).ElecHeatingPower, 0.0);
    EXPECT_DOUBLE_EQ(VRF(VRFCond).CrankCaseHeaterPower, 0.0);
    EXPECT_DOUBLE_EQ(VRF(VRFCond).EvapCondPumpElecPower, 0.0);
    EXPECT_DOUBLE_EQ(VRF(VRFCond).EvapWaterConsumpRate, 0.0);
    EXPECT_DOUBLE_EQ(VRF(VRFCond).DefrostPower, 0.0);
    EXPECT_DOUBLE_EQ(VRF(VRFCond).OperatingCoolingCOP, 0.0);
    EXPECT_DOUBLE_EQ(VRF(VRFCond).OperatingHeatingCOP, 0.0);
    EXPECT_DOUBLE_EQ(VRF(VRFCond).OperatingCOP, 0.0);
    EXPECT_DOUBLE_EQ(VRF(VRFCond).SCHE, 0.0);
    EXPECT_DOUBLE_EQ(VRF(VRFCond).BasinHeaterPower, 0.0);
    EXPECT_DOUBLE_EQ(VRF(VRFCond).SUMultiplier, 0.0);
    EXPECT_DOUBLE_EQ(VRF(VRFCond).VRFCondPLR, 0.0);
    EXPECT_DOUBLE_EQ(VRF(VRFCond).VRFCondRTF, 0.0);
    EXPECT_DOUBLE_EQ(VRF(VRFCond).VRFCondCyclingRatio, 0.0);
    EXPECT_DOUBLE_EQ(VRF(VRFCond).QCondEnergy, 0.0);
    EXPECT_DOUBLE_EQ(VRF(VRFCond).TotalCoolingCapacity, 0.0);
    EXPECT_DOUBLE_EQ(VRF(VRFCond).TotalHeatingCapacity, 0.0);
    EXPECT_DOUBLE_EQ(VRF(VRFCond).OperatingMode, 0.0);
    EXPECT_FALSE(VRF(VRFCond).HRHeatingActive);
    EXPECT_FALSE(VRF(VRFCond).HRCoolingActive);

    // TU's are in cooling mode only
    CoolingLoad(VRFCond) = true;
    LastModeCooling(VRFCond) = true;
    LastModeHeating(VRFCond) = false;
    TerminalUnitList(1).TotalCoolLoad(1) = 1000.0;
    TerminalUnitList(1).TotalCoolLoad(2) = 1000.0;
    TerminalUnitList(1).TotalCoolLoad(3) = 1000.0;
    TerminalUnitList(1).TotalCoolLoad(4) = 1000.0;
    TerminalUnitList(1).TotalCoolLoad(5) = 1000.0;

    CalcVRFCondenser(VRFCond, false);
    EXPECT_FALSE(VRF(VRFCond).HRHeatingActive);
    EXPECT_FALSE(VRF(VRFCond).HRCoolingActive);
    EXPECT_EQ(VRF(VRFCond).TotalCoolingCapacity, 5000.0);
    EXPECT_EQ(VRF(VRFCond).TUCoolingLoad, 5000.0);
    EXPECT_EQ(VRF(VRFCond).TotalHeatingCapacity, 0.0);
    EXPECT_EQ(VRF(VRFCond).TUHeatingLoad, 0.0);
    EXPECT_EQ(VRF(VRFCond).VRFCondPLR, 0.25);
    EXPECT_EQ(VRF(VRFCond).VRFCondRTF, 1.0); // unit is not cycling below min PLR
    EXPECT_EQ(VRF(VRFCond).SUMultiplier, 1.0);
    EXPECT_FALSE(VRF(VRFCond).ModeChange);
    EXPECT_FALSE(VRF(VRFCond).HRModeChange);
    EXPECT_EQ(VRF(VRFCond).ElecCoolingPower, VRF(VRFCond).RatedCoolingPower * VRF(VRFCond).VRFCondPLR);
    EXPECT_EQ(VRF(VRFCond).ElecHeatingPower, 0.0);

    // TU's are in heating mode only
    CoolingLoad(VRFCond) = false;
    HeatingLoad(VRFCond) = true;
    LastModeCooling(VRFCond) = false;
    LastModeHeating(VRFCond) = true;
    TerminalUnitList(1).TotalCoolLoad(1) = 0.0;
    TerminalUnitList(1).TotalCoolLoad(2) = 0.0;
    TerminalUnitList(1).TotalCoolLoad(3) = 0.0;
    TerminalUnitList(1).TotalCoolLoad(4) = 0.0;
    TerminalUnitList(1).TotalCoolLoad(5) = 0.0;
    TerminalUnitList(1).TotalHeatLoad(1) = 1000.0;
    TerminalUnitList(1).TotalHeatLoad(2) = 1000.0;
    TerminalUnitList(1).TotalHeatLoad(3) = 1000.0;
    TerminalUnitList(1).TotalHeatLoad(4) = 1000.0;
    TerminalUnitList(1).TotalHeatLoad(5) = 1000.0;

    CalcVRFCondenser(VRFCond, false);
    EXPECT_FALSE(VRF(VRFCond).HRHeatingActive);
    EXPECT_FALSE(VRF(VRFCond).HRCoolingActive);
    EXPECT_EQ(VRF(VRFCond).TotalCoolingCapacity, 0.0);
    EXPECT_EQ(VRF(VRFCond).TUCoolingLoad, 0.0);
    EXPECT_EQ(VRF(VRFCond).TotalHeatingCapacity, 5000.0);
    EXPECT_EQ(VRF(VRFCond).TUHeatingLoad, 5000.0);
    EXPECT_EQ(VRF(VRFCond).VRFCondPLR, 0.25);
    EXPECT_EQ(VRF(VRFCond).VRFCondRTF, 1.0); // unit is not cycling below min PLR
    EXPECT_EQ(VRF(VRFCond).SUMultiplier, 1.0);
    EXPECT_FALSE(VRF(VRFCond).ModeChange);
    EXPECT_FALSE(VRF(VRFCond).HRModeChange);
    EXPECT_EQ(VRF(VRFCond).ElecCoolingPower, 0.0);
    EXPECT_EQ(VRF(VRFCond).ElecHeatingPower, VRF(VRFCond).RatedHeatingPower * VRF(VRFCond).VRFCondPLR);

    // increment time step
    DataGlobals::CurrentTime += DataGlobals::TimeStepZone; // 0.5
    // set TU's to request both cooling and heating
    TerminalUnitList(1).TotalCoolLoad(1) = 0.0;
    TerminalUnitList(1).HRCoolRequest(1) = false;
    TerminalUnitList(1).TotalCoolLoad(2) = 1000.0;
    TerminalUnitList(1).HRCoolRequest(2) = true;
    TerminalUnitList(1).TotalCoolLoad(3) = 0.0;
    TerminalUnitList(1).HRCoolRequest(3) = false;
    TerminalUnitList(1).TotalCoolLoad(4) = 1000.0;
    TerminalUnitList(1).HRCoolRequest(4) = true;
    TerminalUnitList(1).TotalCoolLoad(5) = 0.0;
    TerminalUnitList(1).HRCoolRequest(5) = false;
    TerminalUnitList(1).TotalHeatLoad(1) = 1000.0;
    TerminalUnitList(1).HRHeatRequest(1) = true;
    TerminalUnitList(1).TotalHeatLoad(2) = 0.0;
    TerminalUnitList(1).HRHeatRequest(2) = false;
    TerminalUnitList(1).TotalHeatLoad(3) = 1000.0;
    TerminalUnitList(1).HRHeatRequest(3) = true;
    TerminalUnitList(1).TotalHeatLoad(4) = 0.0;
    TerminalUnitList(1).HRHeatRequest(4) = false;
    TerminalUnitList(1).TotalHeatLoad(5) = 1000.0;
    TerminalUnitList(1).HRHeatRequest(5) = true;
    VRF(VRFCond).HeatRecoveryUsed = true;

    // set heat recovery time constant to non-zero value (means mode change will degrade performance)
    VRF(VRFCond).HRHeatCapTC = 0.25; // 15 min exponential rise
    // last operating mode was heating
    CalcVRFCondenser(VRFCond, false);
    EXPECT_TRUE(VRF(VRFCond).HRHeatingActive);
    EXPECT_FALSE(VRF(VRFCond).HRCoolingActive);
    EXPECT_EQ(VRF(VRFCond).TotalCoolingCapacity, 0.0);
    EXPECT_EQ(VRF(VRFCond).TUCoolingLoad, 2000.0);
    EXPECT_EQ(VRF(VRFCond).TotalHeatingCapacity, 3000.0);
    EXPECT_EQ(VRF(VRFCond).TUHeatingLoad, 3000.0);
    EXPECT_NEAR(VRF(VRFCond).VRFCondPLR, 0.13636, 0.00001);
    EXPECT_EQ(VRF(VRFCond).VRFCondRTF, 1.0); // unit is not cycling below min PLR
    EXPECT_NEAR(VRF(VRFCond).SUMultiplier, 0.63212, 0.00001);
    EXPECT_TRUE(VRF(VRFCond).ModeChange);
    EXPECT_FALSE(VRF(VRFCond).HRModeChange);
    EXPECT_EQ(VRF(VRFCond).ElecCoolingPower, 0.0);

    // make adjustment for heat recovery startup degradation
    Real64 HREIRFTConst = VRF(VRFCond).HRCAPFTHeatConst;
    Real64 HRInitialEIRFrac = VRF(VRFCond).HRInitialHeatEIRFrac;
    Real64 HREIRAdjustment = HRInitialEIRFrac + (HREIRFTConst - HRInitialEIRFrac) * VRF(VRFCond).SUMultiplier;

    EXPECT_EQ(VRF(VRFCond).ElecHeatingPower, VRF(VRFCond).RatedHeatingPower * VRF(VRFCond).VRFCondPLR * HREIRAdjustment);

    // last operating mode was cooling should give same answer since TU heating request > TU cooling request * ( 1 + 1/COP)
    CoolingLoad(VRFCond) = true;
    HeatingLoad(VRFCond) = false;
    LastModeCooling(VRFCond) = true;
    LastModeHeating(VRFCond) = false;

    DataGlobals::CurrentTime += DataGlobals::TimeStepZone; // 0.75 - CalcVRFCondenser saves last time stamp for use in exponential curve, increment by
                                                           // 1 time step to get same answer
    CalcVRFCondenser(VRFCond, false);
    EXPECT_TRUE(VRF(VRFCond).HRHeatingActive);
    EXPECT_FALSE(VRF(VRFCond).HRCoolingActive);
    EXPECT_EQ(VRF(VRFCond).TotalCoolingCapacity, 0.0);
    EXPECT_EQ(VRF(VRFCond).TUCoolingLoad, 2000.0);
    EXPECT_EQ(VRF(VRFCond).TotalHeatingCapacity, 3000.0);
    EXPECT_EQ(VRF(VRFCond).TUHeatingLoad, 3000.0);
    EXPECT_NEAR(VRF(VRFCond).VRFCondPLR, 0.13636, 0.00001);
    EXPECT_EQ(VRF(VRFCond).VRFCondRTF, 1.0); // unit is not cycling below min PLR
    EXPECT_NEAR(VRF(VRFCond).SUMultiplier, 0.63212, 0.00001);
    EXPECT_TRUE(VRF(VRFCond).ModeChange);
    EXPECT_FALSE(VRF(VRFCond).HRModeChange);
    EXPECT_EQ(VRF(VRFCond).ElecCoolingPower, 0.0);
    EXPECT_EQ(VRF(VRFCond).ElecHeatingPower, VRF(VRFCond).RatedHeatingPower * VRF(VRFCond).VRFCondPLR * HREIRAdjustment);
    EXPECT_NEAR(HREIRAdjustment, 1.06321, 0.00001);

    // simulate again and see that power has exponential changed from previous time step
    DataGlobals::CurrentTime += DataGlobals::TimeStepZone; // 1.0
    CoolingLoad(VRFCond) = false;
    HeatingLoad(VRFCond) = true;
    LastModeCooling(VRFCond) = false;
    LastModeHeating(VRFCond) = true;

    CalcVRFCondenser(VRFCond, false);

    HREIRAdjustment = HRInitialEIRFrac + (HREIRFTConst - HRInitialEIRFrac) * VRF(VRFCond).SUMultiplier;

    EXPECT_NEAR(VRF(VRFCond).SUMultiplier, 0.86466, 0.00001); // will exponentially rise towards 1.0
    EXPECT_EQ(VRF(VRFCond).ElecHeatingPower, VRF(VRFCond).RatedHeatingPower * VRF(VRFCond).VRFCondPLR * HREIRAdjustment);
    EXPECT_NEAR(HREIRAdjustment, 1.08646, 0.00001); // will exponentially rise towards VRF( VRFCond ).HRCAPFTHeatConst = 1.1

    // simulate again and see that power has exponential changed from previous time step
    DataGlobals::CurrentTime += DataGlobals::TimeStepZone; // 1.25
    CalcVRFCondenser(VRFCond, false);
    HREIRAdjustment = HRInitialEIRFrac + (HREIRFTConst - HRInitialEIRFrac) * VRF(VRFCond).SUMultiplier;
    EXPECT_NEAR(VRF(VRFCond).SUMultiplier, 0.95021, 0.00001); // will exponentially rise towards 1.0
    EXPECT_EQ(VRF(VRFCond).ElecHeatingPower, VRF(VRFCond).RatedHeatingPower * VRF(VRFCond).VRFCondPLR * HREIRAdjustment);
    EXPECT_NEAR(HREIRAdjustment, 1.09502, 0.00001); // will exponentially rise towards VRF( VRFCond ).HRCAPFTHeatConst = 1.1

    // simulate again and see that power has exponential changed from previous time step
    DataGlobals::CurrentTime += DataGlobals::TimeStepZone; // 1.5
    CalcVRFCondenser(VRFCond, false);
    HREIRAdjustment = HRInitialEIRFrac + (HREIRFTConst - HRInitialEIRFrac) * VRF(VRFCond).SUMultiplier;
    EXPECT_NEAR(VRF(VRFCond).SUMultiplier, 0.98168, 0.00001); // will exponentially rise towards 1.0
    EXPECT_EQ(VRF(VRFCond).ElecHeatingPower, VRF(VRFCond).RatedHeatingPower * VRF(VRFCond).VRFCondPLR * HREIRAdjustment);
    EXPECT_NEAR(HREIRAdjustment, 1.09817, 0.00001); // will exponentially rise towards VRF( VRFCond ).HRCAPFTHeatConst = 1.1

    // simulate again and see that power has exponential changed from previous time step
    DataGlobals::CurrentTime += DataGlobals::TimeStepZone; // 1.75
    CalcVRFCondenser(VRFCond, false);
    HREIRAdjustment = HRInitialEIRFrac + (HREIRFTConst - HRInitialEIRFrac) * VRF(VRFCond).SUMultiplier;
    EXPECT_NEAR(VRF(VRFCond).SUMultiplier, 1.0, 0.00001); // will exponentially rise towards 1.0
    EXPECT_EQ(VRF(VRFCond).ElecHeatingPower, VRF(VRFCond).RatedHeatingPower * VRF(VRFCond).VRFCondPLR * HREIRAdjustment);
    EXPECT_NEAR(HREIRAdjustment, 1.1, 0.00001); // will exponentially rise towards VRF( VRFCond ).HRCAPFTHeatConst = 1.1

    // at end of exponential decay (when SUMultiplier = 1), HREIRAdjustment = VRF( VRFCond ).HRCAPFTHeatConst
    EXPECT_EQ(HREIRAdjustment, VRF(VRFCond).HRCAPFTHeatConst);
}
}
