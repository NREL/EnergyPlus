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

// EnergyPlus::AirflowNetworkBalanceManager unit tests

// Google test headers
#include <gtest/gtest.h>
#include <exception>

// EnergyPlus Headers
#include <AirflowNetworkBalanceManager.hh>
#include <DataAirflowNetwork.hh>
#include <DataSurfaces.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DataAirLoop.hh>

#include <EnergyPlus/DataAirSystems.hh>

#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/SurfaceGeometry.hh>
#include <EnergyPlus/UtilityRoutines.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace AirflowNetworkBalanceManager;
using namespace DataAirflowNetwork;
using namespace DataSurfaces;
using namespace DataHeatBalance;
using namespace DataGlobals;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::ScheduleManager;
using namespace OutAirNodeManager;

namespace EnergyPlus {

TEST_F(EnergyPlusFixture, AirflowNetworkBalanceManagerTest_TestOtherSideCoefficients)
{

    int i = 2;

    AirflowNetworkNumOfExtSurfaces = 2;
    AirflowNetworkNumOfSurfaces = 2;

    MultizoneSurfaceData.allocate(i);
    Surface.allocate(i);
    Surface(1).ExtBoundCond = -2;
    Surface(2).ExtBoundCond = -2;
    Surface(1).ExtWind = true;
    Surface(2).ExtWind = true;
    Surface(1).Tilt = 90.0;
    Surface(2).Tilt = 90.0;
    Surface(1).Azimuth = 0.0;
    Surface(2).Azimuth = 180.0;

    MultizoneSurfaceData(1).SurfNum = 1;
    MultizoneSurfaceData(2).SurfNum = 2;

    CalcWindPressureCoeffs();
    EXPECT_EQ(1, MultizoneSurfaceData(1).NodeNums(2));
    EXPECT_EQ(2, MultizoneSurfaceData(2).NodeNums(2));
    EXPECT_EQ(1, MultizoneExternalNodeData(1).curve);
    EXPECT_EQ(3, MultizoneExternalNodeData(2).curve);

    MultizoneSurfaceData.deallocate();
    MultizoneExternalNodeData.deallocate();
    Surface.deallocate();
}

TEST_F(EnergyPlusFixture, TestZoneVentingSch)
{

    // Unit test for #5021

    Zone.allocate(1);
    Zone(1).Name = "SALA DE AULA";

    Surface.allocate(2);
    Surface(1).Name = "WINDOW AULA 1";
    Surface(1).Zone = 1;
    Surface(1).ZoneName = "SALA DE AULA";
    Surface(1).Azimuth = 0.0;
    Surface(1).ExtBoundCond = 0;
    Surface(1).HeatTransSurf = true;
    Surface(1).Tilt = 90.0;
    Surface(1).Sides = 4;
    Surface(2).Name = "WINDOW AULA 2";
    Surface(2).Zone = 1;
    Surface(2).ZoneName = "SALA DE AULA";
    Surface(2).Azimuth = 180.0;
    Surface(2).ExtBoundCond = 0;
    Surface(2).HeatTransSurf = true;
    Surface(2).Tilt = 90.0;
    Surface(2).Sides = 4;

    SurfaceWindow.allocate(2);
    SurfaceWindow(1).OriginalClass = 11;
    SurfaceWindow(2).OriginalClass = 11;
    NumOfZones = 1;

    std::string const idf_objects = delimited_string({
        "Version,8.3;",
        "Schedule:Constant,OnSch,,1.0;",
        "Schedule:Constant,Aula people sched,,0.0;",
        "Schedule:Constant,Sempre 21,,21.0;",
        "AirflowNetwork:SimulationControl,",
        "  NaturalVentilation, !- Name",
        "  MultizoneWithoutDistribution, !- AirflowNetwork Control",
        "  SurfaceAverageCalculation, !- Wind Pressure Coefficient Type",
        "  , !- Height Selection for Local Wind Pressure Calculation",
        "  LOWRISE, !- Building Type",
        "  1000, !- Maximum Number of Iterations{ dimensionless }",
        "  LinearInitializationMethod, !- Initialization Type",
        "  0.0001, !- Relative Airflow Convergence Tolerance{ dimensionless }",
        "  0.0001, !- Absolute Airflow Convergence Tolerance{ kg / s }",
        "  -0.5, !- Convergence Acceleration Limit{ dimensionless }",
        "  90, !- Azimuth Angle of Long Axis of Building{ deg }",
        "  0.36;                    !- Ratio of Building Width Along Short Axis to Width Along Long Axis",
        "AirflowNetwork:MultiZone:Zone,",
        "  sala de aula, !- Zone Name",
        "  Temperature, !- Ventilation Control Mode",
        "  Sempre 21, !- Ventilation Control Zone Temperature Setpoint Schedule Name",
        "  1, !- Minimum Venting Open Factor{ dimensionless }",
        "  , !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor{ deltaC }",
        "  100, !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor{ deltaC }",
        "  , !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor{ deltaJ / kg }",
        "  300000, !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor{ deltaJ / kg }",
        "  Aula people sched, !- Venting Availability Schedule Name",
        "  Standard;                !- Single Sided Wind Pressure Coefficient Algorithm",
        "AirflowNetwork:MultiZone:Surface,",
        "  window aula 1, !- Surface Name",
        "  Simple Window, !- Leakage Component Name",
        "  , !- External Node Name",
        "  1, !- Window / Door Opening Factor, or Crack Factor{ dimensionless }",
        "  ZoneLevel, !- Ventilation Control Mode",
        "  , !- Ventilation Control Zone Temperature Setpoint Schedule Name",
        "  , !- Minimum Venting Open Factor{ dimensionless }",
        "  , !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor{ deltaC }",
        "  100, !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor{ deltaC }",
        "  , !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor{ deltaJ / kg }",
        "  300000, !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor{ deltaJ / kg }",
        "  Aula people sched;       !- Venting Availability Schedule Name",
        "AirflowNetwork:MultiZone:Surface,",
        "  window aula 2, !- Surface Name",
        "  Simple Window, !- Leakage Component Name",
        "  , !- External Node Name",
        "  1, !- Window / Door Opening Factor, or Crack Factor{ dimensionless }",
        "  Temperature, !- Ventilation Control Mode",
        "  Sempre 21, !- Ventilation Control Zone Temperature Setpoint Schedule Name",
        "  1, !- Minimum Venting Open Factor{ dimensionless }",
        "  , !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor{ deltaC }",
        "  100, !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor{ deltaC }",
        "  , !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor{ deltaJ / kg }",
        "  300000, !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor{ deltaJ / kg }",
        "  Aula people sched;       !- Venting Availability Schedule Name",
        "AirflowNetwork:MultiZone:Component:SimpleOpening,",
        "  Simple Window, !- Name",
        "  0.0010, !- Air Mass Flow Coefficient When Opening is Closed{ kg / s - m }",
        "  0.65, !- Air Mass Flow Exponent When Opening is Closed{ dimensionless }",
        "  0.01, !- Minimum Density Difference for Two - Way Flow{ kg / m3 }",
        "  0.78;                    !- Discharge Coefficient{ dimensionless }",
    });

    ASSERT_TRUE(process_idf(idf_objects));
    GetAirflowNetworkInput();

    // MultizoneZoneData has only 1 element so may be hardcoded
    auto GetIndex = UtilityRoutines::FindItemInList(MultizoneZoneData(1).VentingSchName, Schedule({1, NumSchedules}));
    EXPECT_EQ(GetIndex, MultizoneZoneData(1).VentingSchNum);

    Zone.deallocate();
    Surface.deallocate();
    SurfaceWindow.deallocate();
}

TEST_F(EnergyPlusFixture, AirflowNetworkBalanceManager_TestTriangularWindowWarning)
{

    // Unit test for #5384

    Zone.allocate(1);
    Zone(1).Name = "WEST_ZONE";

    Surface.allocate(3);
    Surface(1).Name = "SURFACE_1";
    Surface(1).Zone = 1;
    Surface(1).ZoneName = "WEST_ZONE";
    Surface(1).Azimuth = 0.0;
    Surface(1).ExtBoundCond = 0;
    Surface(1).HeatTransSurf = true;
    Surface(1).Tilt = 90.0;
    Surface(1).Sides = 4;
    Surface(2).Name = "SURFACE_2";
    Surface(2).Zone = 1;
    Surface(2).ZoneName = "WEST_ZONE";
    Surface(2).Azimuth = 180.0;
    Surface(2).ExtBoundCond = 0;
    Surface(2).HeatTransSurf = true;
    Surface(2).Tilt = 90.0;
    Surface(2).Sides = 4;
    Surface(3).Name = "WINDOW1";
    Surface(3).Zone = 1;
    Surface(3).ZoneName = "WEST_ZONE";
    Surface(3).Azimuth = 180.0;
    Surface(3).ExtBoundCond = 0;
    Surface(3).HeatTransSurf = true;
    Surface(3).Tilt = 90.0;
    Surface(3).Sides = 3;
    Surface(3).Vertex.allocate(3);
    Surface(3).Vertex(1).x = 3.0;
    Surface(3).Vertex(2).x = 3.0;
    Surface(3).Vertex(3).x = 1.0;
    Surface(3).Vertex(1).y = 10.778;
    Surface(3).Vertex(2).y = 10.778;
    Surface(3).Vertex(3).y = 10.778;
    Surface(3).Vertex(1).z = 2.0;
    Surface(3).Vertex(2).z = 1.0;
    Surface(3).Vertex(3).z = 1.0;

    SurfaceWindow.allocate(3);
    SurfaceWindow(1).OriginalClass = 11;
    SurfaceWindow(2).OriginalClass = 11;
    SurfaceWindow(3).OriginalClass = 11;
    NumOfZones = 1;

    std::string const idf_objects = delimited_string({
        "Version,8.3;",
        "Schedule:Constant,OnSch,,1.0;",
        "Schedule:Constant,Aula people sched,,0.0;",
        "Schedule:Constant,Sempre 21,,21.0;",
        "AirflowNetwork:SimulationControl,",
        "  NaturalVentilation, !- Name",
        "  MultizoneWithoutDistribution, !- AirflowNetwork Control",
        "  SurfaceAverageCalculation, !- Wind Pressure Coefficient Type",
        "  , !- Height Selection for Local Wind Pressure Calculation",
        "  LOWRISE, !- Building Type",
        "  1000, !- Maximum Number of Iterations{ dimensionless }",
        "  LinearInitializationMethod, !- Initialization Type",
        "  0.0001, !- Relative Airflow Convergence Tolerance{ dimensionless }",
        "  0.0001, !- Absolute Airflow Convergence Tolerance{ kg / s }",
        "  -0.5, !- Convergence Acceleration Limit{ dimensionless }",
        "  90, !- Azimuth Angle of Long Axis of Building{ deg }",
        "  0.36;                    !- Ratio of Building Width Along Short Axis to Width Along Long Axis",
        "AirflowNetwork:MultiZone:Zone,",
        "  WEST_ZONE, !- Zone Name",
        "  Temperature, !- Ventilation Control Mode",
        "  Sempre 21, !- Ventilation Control Zone Temperature Setpoint Schedule Name",
        "  1, !- Minimum Venting Open Factor{ dimensionless }",
        "  , !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor{ deltaC }",
        "  100, !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor{ deltaC }",
        "  , !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor{ deltaJ / kg }",
        "  300000, !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor{ deltaJ / kg }",
        "  Aula people sched, !- Venting Availability Schedule Name",
        "  Standard;                !- Single Sided Wind Pressure Coefficient Algorithm",
        "AirflowNetwork:MultiZone:Surface,",
        "  Surface_1, !- Surface Name",
        "  CR-1, !- Leakage Component Name",
        "  , !- External Node Name",
        "  1; !- Window / Door Opening Factor, or Crack Factor{ dimensionless }",
        "AirflowNetwork:MultiZone:Surface,",
        "  Surface_2, !- Surface Name",
        "  CR-1, !- Leakage Component Name",
        "  , !- External Node Name",
        "  1; !- Window / Door Opening Factor, or Crack Factor{ dimensionless }",
        "AirflowNetwork:MultiZone:Surface,",
        "  Window1, !- Surface Name",
        "  Simple Window, !- Leakage Component Name",
        "  , !- External Node Name",
        "  1; !- Window / Door Opening Factor, or Crack Factor{ dimensionless }",
        "AirflowNetwork:MultiZone:Component:SimpleOpening,",
        "  Simple Window, !- Name",
        "  0.0010, !- Air Mass Flow Coefficient When Opening is Closed{ kg / s - m }",
        "  0.65, !- Air Mass Flow Exponent When Opening is Closed{ dimensionless }",
        "  0.01, !- Minimum Density Difference for Two - Way Flow{ kg / m3 }",
        "  0.78;                    !- Discharge Coefficient{ dimensionless }",
        "AirflowNetwork:MultiZone:ReferenceCrackConditions,",
        "  ReferenceCrackConditions, !- Name",
        "  20.0, !- Reference Temperature{ C }",
        "  101320, !- Reference Barometric Pressure{ Pa }",
        "  0.005;                   !- Reference Humidity Ratio{ kgWater / kgDryAir }",
        "AirflowNetwork:MultiZone:Surface:Crack,",
        "  CR-1, !- Name",
        "  0.01, !- Air Mass Flow Coefficient at Reference Conditions{ kg / s }",
        "  0.667, !- Air Mass Flow Exponent{ dimensionless }",
        "  ReferenceCrackConditions; !- Reference Crack Conditions",
    });

    ASSERT_TRUE(process_idf(idf_objects));
    GetAirflowNetworkInput();
    std::string const error_string = delimited_string({
        "   ** Warning ** GetAirflowNetworkInput: AirflowNetwork:MultiZone:Surface=\"WINDOW1\".",
        "   **   ~~~   ** The opening is a Triangular subsurface. A rectangular subsurface will be used with equivalent width and height.",
    });

    EXPECT_TRUE(compare_err_stream(error_string, true));

    AirflowNetworkNodeData.deallocate();
    AirflowNetworkCompData.deallocate();
    MultizoneExternalNodeData.deallocate();
    Zone.deallocate();
    Surface.deallocate();
    SurfaceWindow.deallocate();
}

TEST_F(EnergyPlusFixture, TestAFNPressureStat)
{

    // Unit test for a new feature of PressureStat and #5687
    int i;

    std::string const idf_objects = delimited_string({
        "Version,8.4;",
        "  Building,",
        "    Small Office with AirflowNetwork model,  !- Name",
        "    0,                       !- North Axis {deg}",
        "    Suburbs,                 !- Terrain",
        "    0.001,                   !- Loads Convergence Tolerance Value",
        "    0.0050000,               !- Temperature Convergence Tolerance Value {deltaC}",
        "    FullInteriorAndExterior, !- Solar Distribution",
        "    25,                      !- Maximum Number of Warmup Days",
        "    6;                       !- Minimum Number of Warmup Days",

        "  Timestep,6;",

        "  SurfaceConvectionAlgorithm:Inside,TARP;",

        "  SurfaceConvectionAlgorithm:Outside,DOE-2;",

        "  HeatBalanceAlgorithm,ConductionTransferFunction;",

        "  Schedule:Constant,FanAndCoilAvailSched,,1.0;",
        "  Schedule:Constant,On,,1.0;",
        "  Schedule:Constant,WindowVentSched,,21.0;",
        "  Schedule:Constant,VentingSched,,0.0;",

        "  Site:GroundTemperature:BuildingSurface,20.03,20.03,20.13,20.30,20.43,20.52,20.62,20.77,20.78,20.55,20.44,20.20;",

        "  Material,",
        "    A1 - 1 IN STUCCO,        !- Name",
        "    Smooth,                  !- Roughness",
        "    2.5389841E-02,           !- Thickness {m}",
        "    0.6918309,               !- Conductivity {W/m-K}",
        "    1858.142,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.9200000,               !- Solar Absorptance",
        "    0.9200000;               !- Visible Absorptance",

        "  Material,",
        "    C4 - 4 IN COMMON BRICK,  !- Name",
        "    Rough,                   !- Roughness",
        "    0.1014984,               !- Thickness {m}",
        "    0.7264224,               !- Conductivity {W/m-K}",
        "    1922.216,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7600000,               !- Solar Absorptance",
        "    0.7600000;               !- Visible Absorptance",

        "  Material,",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Name",
        "    Smooth,                  !- Roughness",
        "    1.9050000E-02,           !- Thickness {m}",
        "    0.7264224,               !- Conductivity {W/m-K}",
        "    1601.846,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.9200000,               !- Solar Absorptance",
        "    0.9200000;               !- Visible Absorptance",

        "  Material,",
        "    C6 - 8 IN CLAY TILE,     !- Name",
        "    Smooth,                  !- Roughness",
        "    0.2033016,               !- Thickness {m}",
        "    0.5707605,               !- Conductivity {W/m-K}",
        "    1121.292,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.8200000,               !- Solar Absorptance",
        "    0.8200000;               !- Visible Absorptance",

        "  Material,",
        "    C10 - 8 IN HW CONCRETE,  !- Name",
        "    MediumRough,             !- Roughness",
        "    0.2033016,               !- Thickness {m}",
        "    1.729577,                !- Conductivity {W/m-K}",
        "    2242.585,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.6500000,               !- Solar Absorptance",
        "    0.6500000;               !- Visible Absorptance",

        "  Material,",
        "    E2 - 1 / 2 IN SLAG OR STONE,  !- Name",
        "    Rough,                   !- Roughness",
        "    1.2710161E-02,           !- Thickness {m}",
        "    1.435549,                !- Conductivity {W/m-K}",
        "    881.0155,                !- Density {kg/m3}",
        "    1673.600,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.5500000,               !- Solar Absorptance",
        "    0.5500000;               !- Visible Absorptance",

        "  Material,",
        "    E3 - 3 / 8 IN FELT AND MEMBRANE,  !- Name",
        "    Rough,                   !- Roughness",
        "    9.5402403E-03,           !- Thickness {m}",
        "    0.1902535,               !- Conductivity {W/m-K}",
        "    1121.292,                !- Density {kg/m3}",
        "    1673.600,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",

        "  Material,",
        "    B5 - 1 IN DENSE INSULATION,  !- Name",
        "    VeryRough,               !- Roughness",
        "    2.5389841E-02,           !- Thickness {m}",
        "    4.3239430E-02,           !- Conductivity {W/m-K}",
        "    91.30524,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.5000000,               !- Solar Absorptance",
        "    0.5000000;               !- Visible Absorptance",

        "  Material,",
        "    C12 - 2 IN HW CONCRETE,  !- Name",
        "    MediumRough,             !- Roughness",
        "    5.0901599E-02,           !- Thickness {m}",
        "    1.729577,                !- Conductivity {W/m-K}",
        "    2242.585,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.6500000,               !- Solar Absorptance",
        "    0.6500000;               !- Visible Absorptance",

        "  Material,",
        "    1.375in-Solid-Core,      !- Name",
        "    Smooth,                  !- Roughness",
        "    3.4925E-02,              !- Thickness {m}",
        "    0.1525000,               !- Conductivity {W/m-K}",
        "    614.5000,                !- Density {kg/m3}",
        "    1630.0000,               !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.9200000,               !- Solar Absorptance",
        "    0.9200000;               !- Visible Absorptance",

        "  Construction,",
        "    EXTWALL80,               !- Name",
        "    A1 - 1 IN STUCCO,        !- Outside Layer",
        "    C4 - 4 IN COMMON BRICK,  !- Layer 2",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 3",

        "  Construction,",
        "    PARTITION06,             !- Name",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Outside Layer",
        "    C6 - 8 IN CLAY TILE,     !- Layer 2",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 3",

        "  Construction,",
        "    FLOOR SLAB 8 IN,         !- Name",
        "    C10 - 8 IN HW CONCRETE;  !- Outside Layer",

        "  Construction,",
        "    ROOF34,                  !- Name",
        "    E2 - 1 / 2 IN SLAG OR STONE,  !- Outside Layer",
        "    E3 - 3 / 8 IN FELT AND MEMBRANE,  !- Layer 2",
        "    C12 - 2 IN HW CONCRETE;  !- Layer 3",

        "  Construction,",
        "    CEILING:ZONE,            !- Name",
        "    B5 - 1 IN DENSE INSULATION,  !- Outside Layer",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 2",

        "  Construction,",
        "    CEILING:ATTIC,           !- Name",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Outside Layer",
        "    B5 - 1 IN DENSE INSULATION;  !- Layer 2",

        "  Construction,",
        "    DOOR-CON,                !- Name",
        "    1.375in-Solid-Core;      !- Outside Layer",

        " Construction,",
        "  window - 90.1 - 2004 - nonres - fixed, !- Name",
        "  ASHRAE NonRes Fixed Assembly Window;  !- Outside Layer",

        " WindowMaterial:SimpleGlazingSystem,",
        "  ASHRAE NonRes Fixed Assembly Window, !- Name",
        "  3.23646, !- U - Factor{ W / m2 - K }",
        "  0.39, !- Solar Heat Gain Coefficient",
        "  ;                        !- Visible Transmittance",

        "  Zone,",
        "    West Zone,               !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    autocalculate,           !- Ceiling Height {m}",
        "    autocalculate;           !- Volume {m3}",

        "  Zone,",
        "    EAST ZONE,               !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    autocalculate,           !- Ceiling Height {m}",
        "    autocalculate;           !- Volume {m3}",

        "  Zone,",
        "    NORTH ZONE,              !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    autocalculate,           !- Ceiling Height {m}",
        "    autocalculate;           !- Volume {m3}",
        "  Zone,",
        "    ATTIC ZONE,              !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    autocalculate,           !- Ceiling Height {m}",
        "    autocalculate;           !- Volume {m3}",

        "  GlobalGeometryRules,",
        "    UpperLeftCorner,         !- Starting Vertex Position",
        "    CounterClockWise,        !- Vertex Entry Direction",
        "    World;                   !- Coordinate System",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall001,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    West Zone,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000,0,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall002,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    West Zone,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,0,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall003,           !- Name",
        "    Wall,                    !- Surface Type",
        "    PARTITION06,             !- Construction Name",
        "    West Zone,               !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn003:Wall004,           !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall004,           !- Name",
        "    Wall,                    !- Surface Type",
        "    PARTITION06,             !- Construction Name",
        "    West Zone,               !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn002:Wall004,           !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Flr001,            !- Name",
        "    Floor,                   !- Surface Type",
        "    FLOOR SLAB 8 IN,         !- Construction Name",
        "    West Zone,               !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn001:Flr001,            !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    1.000000,                !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000,0,0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Ceil001,           !- Name",
        "    CEILING,                 !- Surface Type",
        "    CEILING:ZONE,            !- Construction Name",
        "    West Zone,               !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn004:Flr001,            !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,0,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn002:Wall001,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    12.19200,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    9.144000,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    9.144000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn002:Wall002,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    12.19200,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    12.19200,0,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn002:Wall003,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    12.19200,0,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    12.19200,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    12.19200,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    12.19200,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn002:Wall004,           !- Name",
        "    Wall,                    !- Surface Type",
        "    PARTITION06,             !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn001:Wall004,           !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000,0,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn002:Wall005,           !- Name",
        "    Wall,                    !- Surface Type",
        "    PARTITION06,             !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn003:Wall005,           !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    9.144000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    9.144000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn002:Flr001,            !- Name",
        "    Floor,                   !- Surface Type",
        "    FLOOR SLAB 8 IN,         !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn002:Flr001,            !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    1.000000,                !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,0,0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    12.19200,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    12.19200,0,0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn002:Ceil001,           !- Name",
        "    CEILING,                 !- Surface Type",
        "    CEILING:ZONE,            !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn004:Flr002,            !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    12.19200,0,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    12.19200,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn003:Wall001,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    NORTH ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,12.19200,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,12.19200,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn003:Wall002,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    NORTH ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    9.144000,12.19200,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    9.144000,12.19200,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,12.19200,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,12.19200,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn003:Wall003,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    NORTH ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    9.144000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    9.144000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    9.144000,12.19200,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    9.144000,12.19200,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn003:Wall004,           !- Name",
        "    Wall,                    !- Surface Type",
        "    PARTITION06,             !- Construction Name",
        "    NORTH ZONE,              !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn001:Wall003,           !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn003:Wall005,           !- Name",
        "    Wall,                    !- Surface Type",
        "    PARTITION06,             !- Construction Name",
        "    NORTH ZONE,              !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn002:Wall005,           !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    9.144000,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    9.144000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn003:Flr001,            !- Name",
        "    Floor,                   !- Surface Type",
        "    FLOOR SLAB 8 IN,         !- Construction Name",
        "    NORTH ZONE,              !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn003:Flr001,            !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    1.000000,                !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,6.096000,0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,12.19200,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    9.144000,12.19200,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    9.144000,6.096000,0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn003:Ceil001,           !- Name",
        "    CEILING,                 !- Surface Type",
        "    CEILING:ZONE,            !- Construction Name",
        "    NORTH ZONE,              !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn004:Flr003,            !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,12.19200,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    9.144000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    9.144000,12.19200,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Wall001,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,0,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000,0,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Wall002,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,0,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    12.19200,0,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    12.19200,0,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Wall003,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    12.19200,0,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
        "    12.19200,0,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    12.19200,6.096000,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Wall004,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    12.19200,6.096000,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
        "    12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    9.144000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    9.144000,6.096000,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Wall005,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    9.144000,6.096000,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
        "    9.144000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    9.144000,12.19200,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    9.144000,12.19200,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Wall006,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    9.144000,12.19200,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
        "    9.144000,12.19200,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,12.19200,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,12.19200,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Wall007,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,12.19200,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,12.19200,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,6.096000,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Wall008,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,6.096000,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,0,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,0,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Roof001,           !- Name",
        "    Roof,                    !- Surface Type",
        "    ROOF34,                  !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,6.096000,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,0,3.962400,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,0,3.962400,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000,6.096000,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Roof002,           !- Name",
        "    Roof,                    !- Surface Type",
        "    ROOF34,                  !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,6.096000,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,0,3.962400,  !- X,Y,Z ==> Vertex 2 {m}",
        "    12.19200,0,3.962400,  !- X,Y,Z ==> Vertex 3 {m}",
        "    12.19200,6.096000,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Roof003,           !- Name",
        "    Roof,                    !- Surface Type",
        "    ROOF34,                  !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,12.19200,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,6.096000,3.962400,  !- X,Y,Z ==> Vertex 2 {m}",
        "    9.144000,6.096000,3.962400,  !- X,Y,Z ==> Vertex 3 {m}",
        "    9.144000,12.19200,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Flr001,            !- Name",
        "    Floor,                   !- Surface Type",
        "    CEILING:ATTIC,           !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn001:Ceil001,           !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    1.000000,                !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000,0,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Flr002,            !- Name",
        "    Floor,                   !- Surface Type",
        "    CEILING:ATTIC,           !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn002:Ceil001,           !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    1.000000,                !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    12.19200,0,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Flr003,            !- Name",
        "    Floor,                   !- Surface Type",
        "    CEILING:ATTIC,           !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn003:Ceil001,           !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    1.000000,                !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,12.19200,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    9.144000,12.19200,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    9.144000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  FenestrationSurface:Detailed,",
        "    Zn001:Wall001:Win001,    !- Name",
        "    Window,                  !- Surface Type",
        "    window - 90.1 - 2004 - nonres - fixed,  !- Construction Name",
        "    Zn001:Wall001,           !- Building Surface Name",
        "    ,                        !- Outside Boundary Condition Object",
        "    0.5000000,               !- View Factor to Ground",
        "    ,                        !- Frame and Divider Name",
        "    1.0,                     !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    0.548000,0,2.5000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0.548000,0,0.5000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    5.548000,0,0.5000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    5.548000,0,2.5000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  FenestrationSurface:Detailed,",
        "    Zn001:Wall003:Door001,   !- Name",
        "    DOOR,                    !- Surface Type",
        "    DOOR-CON,                !- Construction Name",
        "    Zn001:Wall003,           !- Building Surface Name",
        "    Zn003:Wall004:Door001,   !- Outside Boundary Condition Object",
        "    0.5000000,               !- View Factor to Ground",
        "    ,                        !- Frame and Divider Name",
        "    1.0,                     !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    3.500,6.096000,2.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    3.500,6.096000,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    2.500,6.096000,0.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    2.500,6.096000,2.0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  FenestrationSurface:Detailed,",
        "    Zn003:Wall002:Win001,    !- Name",
        "    Window,                  !- Surface Type",
        "    window - 90.1 - 2004 - nonres - fixed,  !- Construction Name",
        "    Zn003:Wall002,           !- Building Surface Name",
        "    ,                        !- Outside Boundary Condition Object",
        "    0.5000000,               !- View Factor to Ground",
        "    ,                        !- Frame and Divider Name",
        "    1.0,                     !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    5.548000,12.19200,2.5000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    5.548000,12.19200,0.5000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0.548000,12.19200,0.5000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0.548000,12.19200,2.5000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  FenestrationSurface:Detailed,",
        "    Zn003:Wall004:Door001,   !- Name",
        "    DOOR,                    !- Surface Type",
        "    DOOR-CON,                !- Construction Name",
        "    Zn003:Wall004,           !- Building Surface Name",
        "    Zn001:Wall003:Door001,   !- Outside Boundary Condition Object",
        "    0.5000000,               !- View Factor to Ground",
        "    ,                        !- Frame and Divider Name",
        "    1.0,                     !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    2.500,6.096000,2.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    2.500,6.096000,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    3.500,6.096000,0.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    3.500,6.096000,2.0;  !- X,Y,Z ==> Vertex 4 {m}",
        "	OutdoorAir:Mixer,",
        "	OA Mixing Box 1, !- Name",
        "	Mixed Air Node, !- Mixed Air Node Name",
        "	Outside Air Inlet Node, !- Outdoor Air Stream Node Name",
        "	Relief Air Outlet Node, !- Relief Air Stream Node Name",
        "	Air Loop Inlet Node;     !- Return Air Stream Node Name",
        "  AirflowNetwork:SimulationControl,",
        "    AriflowNetwork_All,      !- Name",
        "    MultizoneWithDistribution,  !- AirflowNetwork Control",
        "    INPUT,                   !- Wind Pressure Coefficient Type",
        "    ExternalNode,            !- Height Selection for Local Wind Pressure Calculation",
        "    LOWRISE,                 !- Building Type",
        "    500,                     !- Maximum Number of Iterations {dimensionless}",
        "    ZeroNodePressures,       !- Initialization Type",
        "    1.0E-05,                 !- Relative Airflow Convergence Tolerance {dimensionless}",
        "    1.0E-06,                 !- Absolute Airflow Convergence Tolerance {kg/s}",
        "    -0.5,                    !- Convergence Acceleration Limit {dimensionless}",
        "    0.0,                     !- Azimuth Angle of Long Axis of Building {deg}",
        "    1.0;                     !- Ratio of Building Width Along Short Axis to Width Along Long Axis",

        "  AirflowNetwork:MultiZone:Zone,",
        "    West Zone,               !- Zone Name",
        "    Temperature,             !- Ventilation Control Mode",
        "    WindowVentSched,         !- Ventilation Control Zone Temperature Setpoint Schedule Name",
        "    0.3,                     !- Minimum Venting Open Factor {dimensionless}",
        "    5.0,                     !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor {deltaC}",
        "    10.0,                    !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor {deltaC}",
        "    0.0,                     !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor {deltaJ/kg}",
        "    300000.0,                !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor {deltaJ/kg}",
        "    VentingSched;            !- Venting Availability Schedule Name",

        "  AirflowNetwork:MultiZone:Zone,",
        "    EAST ZONE,               !- Zone Name",
        "    NoVent,                  !- Ventilation Control Mode",
        "    ,                        !- Ventilation Control Zone Temperature Setpoint Schedule Name",
        "    1.0,                     !- Minimum Venting Open Factor {dimensionless}",
        "    0.0,                     !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor {deltaC}",
        "    100.0,                   !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor {deltaC}",
        "    0.0,                     !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor {deltaJ/kg}",
        "    300000.0;                !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor {deltaJ/kg}",

        "  AirflowNetwork:MultiZone:Zone,",
        "    NORTH ZONE,              !- Zone Name",
        "    Temperature,             !- Ventilation Control Mode",
        "    WindowVentSched,         !- Ventilation Control Zone Temperature Setpoint Schedule Name",
        "    1.0,                     !- Minimum Venting Open Factor {dimensionless}",
        "    0.0,                     !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor {deltaC}",
        "    100.0,                   !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor {deltaC}",
        "    0.0,                     !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor {deltaJ/kg}",
        "    300000.0,                !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor {deltaJ/kg}",
        "    VentingSched;            !- Venting Availability Schedule Name",

        "  AirflowNetwork:MultiZone:Zone,",
        "    ATTIC ZONE,              !- Zone Name",
        "    NoVent,                  !- Ventilation Control Mode",
        "    ,                        !- Ventilation Control Zone Temperature Setpoint Schedule Name",
        "    1.0,                     !- Minimum Venting Open Factor {dimensionless}",
        "    0.0,                     !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor {deltaC}",
        "    100.0,                   !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor {deltaC}",
        "    0.0,                     !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor {deltaJ/kg}",
        "    300000.0;                !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor {deltaJ/kg}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn001:Wall001,           !- Surface Name",
        "    ELA-1,                   !- Leakage Component Name",
        "    SFacade,                 !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn001:Wall001:Win001,    !- Surface Name",
        "    CRcri,                   !- Leakage Component Name",
        "    SFacade,                 !- External Node Name",
        "    0.5;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn001:Wall002,           !- Surface Name",
        "    CR-1,                    !- Leakage Component Name",
        "    WFacade,                 !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn001:Wall003,           !- Surface Name",
        "    CRcri,                   !- Leakage Component Name",
        "    ,                        !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn001:Wall003:Door001,   !- Surface Name",
        "    CRcri,                   !- Leakage Component Name",
        "    ,                        !- External Node Name",
        "    0.5;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn001:Wall004,           !- Surface Name",
        "    CRcri,                   !- Leakage Component Name",
        "    ,                        !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn001:Ceil001,           !- Surface Name",
        "    CRcri,                   !- Leakage Component Name",
        "    ,                        !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn002:Wall002,           !- Surface Name",
        "    CR-1,                    !- Leakage Component Name",
        "    SFacade,                 !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn002:Wall003,           !- Surface Name",
        "    CR-1,                    !- Leakage Component Name",
        "    EFacade,                 !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn002:Wall005,           !- Surface Name",
        "    CRcri,                   !- Leakage Component Name",
        "    ,                        !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn002:Ceil001,           !- Surface Name",
        "    CRcri,                   !- Leakage Component Name",
        "    ,                        !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn003:Wall001,           !- Surface Name",
        "    CR-1,                    !- Leakage Component Name",
        "    WFacade,                 !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn003:Wall002,           !- Surface Name",
        "    CR-1,                    !- Leakage Component Name",
        "    NFacade,                 !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn003:Wall002:Win001,    !- Surface Name",
        "    CRcri,                   !- Leakage Component Name",
        "    NFacade,                 !- External Node Name",
        "    0.5;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn003:Wall003,           !- Surface Name",
        "    Zone3 Exhaust Fan,       !- Leakage Component Name",
        "    EFacade,                 !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn003:Ceil001,           !- Surface Name",
        "    CRcri,                   !- Leakage Component Name",
        "    ,                        !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn004:Roof001,           !- Surface Name",
        "    CR-1,                    !- Leakage Component Name",
        "    Horizontal,              !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn004:Roof002,           !- Surface Name",
        "    CR-1,                    !- Leakage Component Name",
        "    Horizontal,              !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn004:Roof003,           !- Surface Name",
        "    CR-1,                    !- Leakage Component Name",
        "    Horizontal,              !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:ReferenceCrackConditions,",
        "    ReferenceCrackConditions,!- Name",
        "    20.0,                    !- Reference Temperature {C}",
        "    101325,                  !- Reference Barometric Pressure {Pa}",
        "    0.0;                     !- Reference Humidity Ratio {kgWater/kgDryAir}",

        "  AirflowNetwork:MultiZone:Surface:Crack,",
        "    CR-1,                    !- Name",
        "    0.01,                    !- Air Mass Flow Coefficient at Reference Conditions {kg/s}",
        "    0.667;                   !- Air Mass Flow Exponent {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface:Crack,",
        "    CRcri,                   !- Name",
        "    0.05,                    !- Air Mass Flow Coefficient at Reference Conditions {kg/s}",
        "    0.667;                   !- Air Mass Flow Exponent {dimensionless}",

        "  AirflowNetwork:MultiZone:Component:ZoneExhaustFan,",
        "    Zone3 Exhaust Fan,       !- Name",
        "    0.01,                    !- Air Mass Flow Coefficient When the Zone Exhaust Fan is Off at Reference Conditions {kg/s}",
        "    0.667;                   !- Air Mass Flow Exponent When the Zone Exhaust Fan is Off {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface:EffectiveLeakageArea,",
        "    ELA-1,                   !- Name",
        "    0.007,                   !- Effective Leakage Area {m2}",
        "    1.0,                     !- Discharge Coefficient {dimensionless}",
        "    4.0,                     !- Reference Pressure Difference {Pa}",
        "    0.667;                   !- Air Mass Flow Exponent {dimensionless}",

        "  AirflowNetwork:MultiZone:ExternalNode,",
        "    NFacade,                 !- Name",
        "    1.524,                   !- External Node Height {m}",
        "    NFacade_WPCValue;        !- Wind Pressure Coefficient Values Object Name",

        "  AirflowNetwork:MultiZone:ExternalNode,",
        "    EFacade,                 !- Name",
        "    1.524,                   !- External Node Height {m}",
        "    EFacade_WPCValue;        !- Wind Pressure Coefficient Values Object Name",

        "  AirflowNetwork:MultiZone:ExternalNode,",
        "    SFacade,                 !- Name",
        "    1.524,                   !- External Node Height {m}",
        "    SFacade_WPCValue;        !- Wind Pressure Coefficient Values Object Name",

        "  AirflowNetwork:MultiZone:ExternalNode,",
        "    WFacade,                 !- Name",
        "    1.524,                   !- External Node Height {m}",
        "    WFacade_WPCValue;        !- Wind Pressure Coefficient Values Object Name",

        "  AirflowNetwork:MultiZone:ExternalNode,",
        "    Horizontal,              !- Name",
        "    3.028,                   !- External Node Height {m}",
        "    Horizontal_WPCValue;     !- Wind Pressure Coefficient Values Object Name",

        "  AirflowNetwork:MultiZone:WindPressureCoefficientArray,",
        "    Every 30 Degrees,        !- Name",
        "    0,                       !- Wind Direction 1 {deg}",
        "    30,                      !- Wind Direction 2 {deg}",
        "    60,                      !- Wind Direction 3 {deg}",
        "    90,                      !- Wind Direction 4 {deg}",
        "    120,                     !- Wind Direction 5 {deg}",
        "    150,                     !- Wind Direction 6 {deg}",
        "    180,                     !- Wind Direction 7 {deg}",
        "    210,                     !- Wind Direction 8 {deg}",
        "    240,                     !- Wind Direction 9 {deg}",
        "    270,                     !- Wind Direction 10 {deg}",
        "    300,                     !- Wind Direction 11 {deg}",
        "    330;                     !- Wind Direction 12 {deg}",

        "  AirflowNetwork:MultiZone:WindPressureCoefficientValues,",
        "    NFacade_WPCValue,        !- Name",
        "    Every 30 Degrees,        !- AirflowNetwork:MultiZone:WindPressureCoefficientArray Name",
        "    0.60,                    !- Wind Pressure Coefficient Value 1 {dimensionless}",
        "    0.48,                    !- Wind Pressure Coefficient Value 2 {dimensionless}",
        "    0.04,                    !- Wind Pressure Coefficient Value 3 {dimensionless}",
        "    -0.56,                   !- Wind Pressure Coefficient Value 4 {dimensionless}",
        "    -0.56,                   !- Wind Pressure Coefficient Value 5 {dimensionless}",
        "    -0.42,                   !- Wind Pressure Coefficient Value 6 {dimensionless}",
        "    -0.37,                   !- Wind Pressure Coefficient Value 7 {dimensionless}",
        "    -0.42,                   !- Wind Pressure Coefficient Value 8 {dimensionless}",
        "    -0.56,                   !- Wind Pressure Coefficient Value 9 {dimensionless}",
        "    -0.56,                   !- Wind Pressure Coefficient Value 10 {dimensionless}",
        "    0.04,                    !- Wind Pressure Coefficient Value 11 {dimensionless}",
        "    0.48;                    !- Wind Pressure Coefficient Value 12 {dimensionless}",

        "  AirflowNetwork:MultiZone:WindPressureCoefficientValues,",
        "    EFacade_WPCValue,        !- Name",
        "    Every 30 Degrees,        !- AirflowNetwork:MultiZone:WindPressureCoefficientArray Name",
        "    -0.56,                   !- Wind Pressure Coefficient Value 1 {dimensionless}",
        "    0.04,                    !- Wind Pressure Coefficient Value 2 {dimensionless}",
        "    0.48,                    !- Wind Pressure Coefficient Value 3 {dimensionless}",
        "    0.60,                    !- Wind Pressure Coefficient Value 4 {dimensionless}",
        "    0.48,                    !- Wind Pressure Coefficient Value 5 {dimensionless}",
        "    0.04,                    !- Wind Pressure Coefficient Value 6 {dimensionless}",
        "    -0.56,                   !- Wind Pressure Coefficient Value 7 {dimensionless}",
        "    -0.56,                   !- Wind Pressure Coefficient Value 8 {dimensionless}",
        "    -0.42,                   !- Wind Pressure Coefficient Value 9 {dimensionless}",
        "    -0.37,                   !- Wind Pressure Coefficient Value 10 {dimensionless}",
        "    -0.42,                   !- Wind Pressure Coefficient Value 11 {dimensionless}",
        "    -0.56;                   !- Wind Pressure Coefficient Value 12 {dimensionless}",

        "  AirflowNetwork:MultiZone:WindPressureCoefficientValues,",
        "    SFacade_WPCValue,        !- Name",
        "    Every 30 Degrees,        !- AirflowNetwork:MultiZone:WindPressureCoefficientArray Name",
        "    -0.37,                   !- Wind Pressure Coefficient Value 1 {dimensionless}",
        "    -0.42,                   !- Wind Pressure Coefficient Value 2 {dimensionless}",
        "    -0.56,                   !- Wind Pressure Coefficient Value 3 {dimensionless}",
        "    -0.56,                   !- Wind Pressure Coefficient Value 4 {dimensionless}",
        "    0.04,                    !- Wind Pressure Coefficient Value 5 {dimensionless}",
        "    0.48,                    !- Wind Pressure Coefficient Value 6 {dimensionless}",
        "    0.60,                    !- Wind Pressure Coefficient Value 7 {dimensionless}",
        "    0.48,                    !- Wind Pressure Coefficient Value 8 {dimensionless}",
        "    0.04,                    !- Wind Pressure Coefficient Value 9 {dimensionless}",
        "    -0.56,                   !- Wind Pressure Coefficient Value 10 {dimensionless}",
        "    -0.56,                   !- Wind Pressure Coefficient Value 11 {dimensionless}",
        "    -0.42;                   !- Wind Pressure Coefficient Value 12 {dimensionless}",

        "  AirflowNetwork:MultiZone:WindPressureCoefficientValues,",
        "    WFacade_WPCValue,        !- Name",
        "    Every 30 Degrees,        !- AirflowNetwork:MultiZone:WindPressureCoefficientArray Name",
        "    -0.56,                   !- Wind Pressure Coefficient Value 1 {dimensionless}",
        "    -0.56,                   !- Wind Pressure Coefficient Value 2 {dimensionless}",
        "    -0.42,                   !- Wind Pressure Coefficient Value 3 {dimensionless}",
        "    -0.37,                   !- Wind Pressure Coefficient Value 4 {dimensionless}",
        "    -0.42,                   !- Wind Pressure Coefficient Value 5 {dimensionless}",
        "    -0.56,                   !- Wind Pressure Coefficient Value 6 {dimensionless}",
        "    -0.56,                   !- Wind Pressure Coefficient Value 7 {dimensionless}",
        "    0.04,                    !- Wind Pressure Coefficient Value 8 {dimensionless}",
        "    0.48,                    !- Wind Pressure Coefficient Value 9 {dimensionless}",
        "    0.60,                    !- Wind Pressure Coefficient Value 10 {dimensionless}",
        "    0.48,                    !- Wind Pressure Coefficient Value 11 {dimensionless}",
        "    0.04;                    !- Wind Pressure Coefficient Value 12 {dimensionless}",

        "  AirflowNetwork:MultiZone:WindPressureCoefficientValues,",
        "    Horizontal_WPCValue,     !- Name",
        "    Every 30 Degrees,        !- AirflowNetwork:MultiZone:WindPressureCoefficientArray Name",
        "    0.00,                    !- Wind Pressure Coefficient Value 1 {dimensionless}",
        "    0.00,                    !- Wind Pressure Coefficient Value 2 {dimensionless}",
        "    0.00,                    !- Wind Pressure Coefficient Value 3 {dimensionless}",
        "    0.00,                    !- Wind Pressure Coefficient Value 4 {dimensionless}",
        "    0.00,                    !- Wind Pressure Coefficient Value 5 {dimensionless}",
        "    0.00,                    !- Wind Pressure Coefficient Value 6 {dimensionless}",
        "    0.00,                    !- Wind Pressure Coefficient Value 7 {dimensionless}",
        "    0.00,                    !- Wind Pressure Coefficient Value 8 {dimensionless}",
        "    0.00,                    !- Wind Pressure Coefficient Value 9 {dimensionless}",
        "    0.00,                    !- Wind Pressure Coefficient Value 10 {dimensionless}",
        "    0.00,                    !- Wind Pressure Coefficient Value 11 {dimensionless}",
        "    0.00;                    !- Wind Pressure Coefficient Value 12 {dimensionless}",

        "  AirflowNetwork:Distribution:Node,",
        "    EquipmentInletNode,      !- Name",
        "    Zone Equipment Inlet Node,  !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    EquipmentOutletNode,     !- Name",
        "    ,                        !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    SupplyMainNode,          !- Name",
        "    ,                        !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    MainSplitterNode,        !- Name",
        "    ,                        !- Component Name or Node Name",
        "    AirLoopHVAC:ZoneSplitter,!- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Zone1SupplyNode,         !- Name",
        "    ,                        !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    ReheatInlet1Node,        !- Name",
        "    Zone 1 Reheat Air Inlet Node,  !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Zone1SupplyRegisterNode, !- Name",
        "    Zone 1 Reheat Air Outlet Node,  !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Zone1OutletNode,         !- Name",
        "    Zone 1 Outlet Node,      !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Zone2SupplyNode,         !- Name",
        "    ,                        !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    ReheatInlet2Node,        !- Name",
        "    Zone 2 Reheat Air Inlet Node,  !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Zone2SupplyRegisterNode, !- Name",
        "    Zone 2 Reheat Air Outlet Node,  !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Zone2OutletNode,         !- Name",
        "    Zone 2 Outlet Node,      !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Zone3SupplyNode,         !- Name",
        "    ,                        !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Zone3SupplyRegisterNode, !- Name",
        "    Zone 3 Inlet Node,       !- Component Name or Node Name",
        "    ,                        !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Zone3OutletNode,         !- Name",
        "    Zone 3 Outlet Node,      !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Zone1ReturnNode,         !- Name",
        "    ,                        !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Zone2ReturnNode,         !- Name",
        "    ,                        !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Zone3ReturnNode,         !- Name",
        "    ,                        !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    MainMixerNode,           !- Name",
        "    ,                        !- Component Name or Node Name",
        "    AirLoopHVAC:ZoneMixer,   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    MainReturnNode,          !- Name",
        "    Return Air Mixer Outlet, !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    MainInletNode,           !- Name",
        "    Air Loop Inlet Node,     !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    OA System Node,          !- Name",
        "    ,                        !- Component Name or Node Name",
        "    AirLoopHVAC:OutdoorAirSystem,  !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    OA Inlet Node,           !- Name",
        "    Outside Air Inlet Node,  !- Component Name or Node Name",
        "    OAMixerOutdoorAirStreamNode,  !- Component Object Type or Node Type",
        "    1.5;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    FanInletNode,            !- Name",
        "    Mixed Air Node,          !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    FanOutletNode,           !- Name",
        "    Cooling Coil Air Inlet Node,  !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    HeatingInletNode,        !- Name",
        "    Heating Coil Air Inlet Node,  !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    HeatingOutletNode,       !- Name",
        "    Air Loop Outlet Node,    !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Component:Leak,",
        "    MainSupplyLeak,          !- Name",
        "    0.0025,                  !- Air Mass Flow Coefficient {kg/s}",
        "    0.65;                    !- Air Mass Flow Exponent {dimensionless}",

        "  AirflowNetwork:Distribution:Component:ConstantPressureDrop,",
        "    SupplyCPDComp,           !- Name",
        "    1.0;                     !- Pressure Difference Across the Component {Pa}",

        "  AirflowNetwork:Distribution:Component:LeakageRatio,",
        "    ZoneSupplyELR1,          !- Name",
        "    0.01,                    !- Effective Leakage Ratio {dimensionless}",
        "    1.9,                     !- Maximum Flow Rate {m3/s}",
        "    59.0,                    !- Reference Pressure Difference {Pa}",
        "    0.65;                    !- Air Mass Flow Exponent {dimensionless}",

        "  AirflowNetwork:Distribution:Component:LeakageRatio,",
        "    ZoneSupplyELR2,          !- Name",
        "    0.01,                    !- Effective Leakage Ratio {dimensionless}",
        "    1.9,                     !- Maximum Flow Rate {m3/s}",
        "    59.0,                    !- Reference Pressure Difference {Pa}",
        "    0.65;                    !- Air Mass Flow Exponent {dimensionless}",

        "  AirflowNetwork:Distribution:Component:LeakageRatio,",
        "    ZoneSupplyELR3,          !- Name",
        "    0.01,                    !- Effective Leakage Ratio {dimensionless}",
        "    1.9,                     !- Maximum Flow Rate {m3/s}",
        "    59.0,                    !- Reference Pressure Difference {Pa}",
        "    0.65;                    !- Air Mass Flow Exponent {dimensionless}",

        "  AirflowNetwork:Distribution:Component:LeakageRatio,",
        "    ReturnLeakELR1,          !- Name",
        "    0.03,                    !- Effective Leakage Ratio {dimensionless}",
        "    1.9,                     !- Maximum Flow Rate {m3/s}",
        "    41.0,                    !- Reference Pressure Difference {Pa}",
        "    0.65;                    !- Air Mass Flow Exponent {dimensionless}",

        "  AirflowNetwork:Distribution:Component:LeakageRatio,",
        "    ReturnLeakELR2,          !- Name",
        "    0.03,                    !- Effective Leakage Ratio {dimensionless}",
        "    1.9,                     !- Maximum Flow Rate {m3/s}",
        "    40.0,                    !- Reference Pressure Difference {Pa}",
        "    0.65;                    !- Air Mass Flow Exponent {dimensionless}",

        "  AirflowNetwork:Distribution:Component:LeakageRatio,",
        "    ReturnLeakELR3,          !- Name",
        "    0.04,                    !- Effective Leakage Ratio {dimensionless}",
        "    1.9,                     !- Maximum Flow Rate {m3/s}",
        "    43.0,                    !- Reference Pressure Difference {Pa}",
        "    0.65;                    !- Air Mass Flow Exponent {dimensionless}",

        "  AirflowNetwork:Distribution:Component:Duct,",
        "    MainTruck1,              !- Name",
        "    3.0,                     !- Duct Length {m}",
        "    0.6,                     !- Hydraulic Diameter {m}",
        "    0.2827,                  !- Cross Section Area {m2}",
        "    0.0009,                  !- Surface Roughness {m}",
        "    0.01,                    !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
        "    0.772,                   !- Overall Heat Transmittance Coefficient (U-Factor) from Air to Air {W/m2-K}",
        "    0.0001;                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",

        "  AirflowNetwork:Distribution:Component:Duct,",
        "    MainTruck2,              !- Name",
        "    4.0,                     !- Duct Length {m}",
        "    0.6,                     !- Hydraulic Diameter {m}",
        "    0.2827,                  !- Cross Section Area {m2}",
        "    0.0009,                  !- Surface Roughness {m}",
        "    0.01,                    !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
        "    0.772,                   !- Overall Heat Transmittance Coefficient (U-Factor) from Air to Air {W/m2-K}",
        "    0.0001;                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",

        "  AirflowNetwork:Distribution:Component:Duct,",
        "    Zone1Supply,             !- Name",
        "    5.0,                     !- Duct Length {m}",
        "    0.4,                     !- Hydraulic Diameter {m}",
        "    0.1256,                  !- Cross Section Area {m2}",
        "    0.0009,                  !- Surface Roughness {m}",
        "    1.00,                    !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
        "    0.772,                   !- Overall Heat Transmittance Coefficient (U-Factor) from Air to Air {W/m2-K}",
        "    0.0001;                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",

        "  AirflowNetwork:Distribution:Component:Duct,",
        "    Zone2Supply,             !- Name",
        "    4.0,                     !- Duct Length {m}",
        "    0.39,                    !- Hydraulic Diameter {m}",
        "    0.1195,                  !- Cross Section Area {m2}",
        "    0.0009,                  !- Surface Roughness {m}",
        "    2.5,                     !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
        "    0.772,                   !- Overall Heat Transmittance Coefficient (U-Factor) from Air to Air {W/m2-K}",
        "    0.0001;                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",

        "  AirflowNetwork:Distribution:Component:Duct,",
        "    Zone3Supply,             !- Name",
        "    4.0,                     !- Duct Length {m}",
        "    0.44,                    !- Hydraulic Diameter {m}",
        "    0.1521,                  !- Cross Section Area {m2}",
        "    0.0009,                  !- Surface Roughness {m}",
        "    1.0,                     !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
        "    0.772,                   !- Overall Heat Transmittance Coefficient (U-Factor) from Air to Air {W/m2-K}",
        "    0.0001;                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",

        "  AirflowNetwork:Distribution:Component:Duct,",
        "    Zone1Return,             !- Name",
        "    4.0,                     !- Duct Length {m}",
        "    0.50,                    !- Hydraulic Diameter {m}",
        "    0.1963,                  !- Cross Section Area {m2}",
        "    0.0009,                  !- Surface Roughness {m}",
        "    1.0,                     !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
        "    0.772,                   !- Overall Heat Transmittance Coefficient (U-Factor) from Air to Air {W/m2-K}",
        "    0.0001;                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",

        "  AirflowNetwork:Distribution:Component:Duct,",
        "    Zone2Return,             !- Name",
        "    4.0,                     !- Duct Length {m}",
        "    0.48,                    !- Hydraulic Diameter {m}",
        "    0.1809,                  !- Cross Section Area {m2}",
        "    0.0009,                  !- Surface Roughness {m}",
        "    1.0,                     !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
        "    0.772,                   !- Overall Heat Transmittance Coefficient (U-Factor) from Air to Air {W/m2-K}",
        "    0.0001;                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",

        "  AirflowNetwork:Distribution:Component:Duct,",
        "    Zone3Return,             !- Name",
        "    4.0,                     !- Duct Length {m}",
        "    0.55,                    !- Hydraulic Diameter {m}",
        "    0.2376,                  !- Cross Section Area {m2}",
        "    0.0009,                  !- Surface Roughness {m}",
        "    1.0,                     !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
        "    0.772,                   !- Overall Heat Transmittance Coefficient (U-Factor) from Air to Air {W/m2-K}",
        "    0.0001;                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",

        "  AirflowNetwork:Distribution:Component:Duct,",
        "    ZoneConnectionDuct,      !- Name",
        "    0.1,                     !- Duct Length {m}",
        "    1.0,                     !- Hydraulic Diameter {m}",
        "    0.7854,                  !- Cross Section Area {m2}",
        "    0.0001,                  !- Surface Roughness {m}",
        "    30.00,                   !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
        "    0.001,                   !- Overall Heat Transmittance Coefficient (U-Factor) from Air to Air {W/m2-K}",
        "    0.0001;                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",

        "  AirflowNetwork:Distribution:Component:Duct,",
        "    MixerConnectionDuct,     !- Name",
        "    0.1,                     !- Duct Length {m}",
        "    1.0,                     !- Hydraulic Diameter {m}",
        "    0.7854,                  !- Cross Section Area {m2}",
        "    0.0001,                  !- Surface Roughness {m}",
        "    1.00,                    !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
        "    0.001,                   !- Overall Heat Transmittance Coefficient (U-Factor) from Air to Air {W/m2-K}",
        "    0.0001;                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",

        "  AirflowNetwork:Distribution:Component:Duct,",
        "    AirLoopReturn,           !- Name",
        "    0.1,                     !- Duct Length {m}",
        "    1.0,                     !- Hydraulic Diameter {m}",
        "    0.7854,                  !- Cross Section Area {m2}",
        "    0.0001,                  !- Surface Roughness {m}",
        "    1.00,                    !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
        "    0.001,                   !- Overall Heat Transmittance Coefficient (U-Factor) from Air to Air {W/m2-K}",
        "    0.0001;                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",

        "  AirflowNetwork:Distribution:Component:Duct,",
        "    AirLoopSupply,           !- Name",
        "    0.1,                     !- Duct Length {m}",
        "    1.0,                     !- Hydraulic Diameter {m}",
        "    0.7854,                  !- Cross Section Area {m2}",
        "    0.0001,                  !- Surface Roughness {m}",
        "    1.00,                    !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
        "    0.001,                   !- Overall Heat Transmittance Coefficient (U-Factor) from Air to Air {W/m2-K}",
        "    0.0001;                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",

        "  AirflowNetwork:Distribution:Component:Fan,",
        "    Supply Fan 1,            !- Fan Name",
        "    Fan:ConstantVolume;      !- Supply Fan Object Type",

        "  AirflowNetwork:Distribution:Component:Coil,",
        "    ACDXCoil 1,              !- Coil Name",
        "    Coil:Cooling:DX:SingleSpeed,  !- Coil Object Type",
        "    0.1,                     !- Air Path Length {m}",
        "    1.00;                    !- Air Path Hydraulic Diameter {m}",

        "  AirflowNetwork:Distribution:Component:Coil,",
        "    Main Heating Coil 1,     !- Coil Name",
        "    Coil:Heating:Fuel,        !- Coil Object Type",
        "    0.1,                     !- Air Path Length {m}",
        "    1.00;                    !- Air Path Hydraulic Diameter {m}",

        "  AirflowNetwork:Distribution:Component:TerminalUnit,",
        "    Reheat Zone 1,           !- Terminal Unit Name",
        "    AirTerminal:SingleDuct:ConstantVolume:Reheat,  !- Terminal Unit Object Type",
        "    0.1,                     !- Air Path Length {m}",
        "    0.44;                    !- Air Path Hydraulic Diameter {m}",

        "  AirflowNetwork:Distribution:Component:TerminalUnit,",
        "    Reheat Zone 2,           !- Terminal Unit Name",
        "    AirTerminal:SingleDuct:ConstantVolume:Reheat,  !- Terminal Unit Object Type",
        "    0.1,                     !- Air Path Length {m}",
        "    0.44;                    !- Air Path Hydraulic Diameter {m}",

        "  AirflowNetwork:Distribution:Component:Leak,",
        "    OAFlow,          !- Name",
        "    0.025,                  !- Air Mass Flow Coefficient {kg/s}",
        "    0.65;                    !- Air Mass Flow Exponent {dimensionless}",

        "  AirflowNetwork:Distribution:Component:Leak,",
        "    OAFlow1,          !- Name",
        "    0.025,                  !- Air Mass Flow Coefficient {kg/s}",
        "    0.65;                    !- Air Mass Flow Exponent {dimensionless}",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Main Link 1,             !- Name",
        "    EquipmentInletNode,      !- Node 1 Name",
        "    EquipmentOutletNode,     !- Node 2 Name",
        "    MainTruck1,              !- Component Name",
        "    Attic Zone;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Main CDP Link,           !- Name",
        "    EquipmentOutletNode,     !- Node 1 Name",
        "    SupplyMainNode,          !- Node 2 Name",
        "    SupplyCPDComp;           !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Main Link 2,             !- Name",
        "    SupplyMainNode,          !- Node 1 Name",
        "    MainSplitterNode,        !- Node 2 Name",
        "    MainTruck2,              !- Component Name",
        "    Attic Zone;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone1Supply1Link,        !- Name",
        "    MainSplitterNode,        !- Node 1 Name",
        "    Zone1SupplyNode,         !- Node 2 Name",
        "    Zone1Supply,             !- Component Name",
        "    Attic Zone;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone1Supply2Link,        !- Name",
        "    Zone1SupplyNode,         !- Node 1 Name",
        "    ReheatInlet1Node,        !- Node 2 Name",
        "    Zone1Supply,             !- Component Name",
        "    Attic Zone;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone1ReheatCoilLink,     !- Name",
        "    ReheatInlet1Node,        !- Node 1 Name",
        "    Zone1SupplyRegisterNode, !- Node 2 Name",
        "    Reheat Zone 1;           !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone1SupplyConnectionLink,  !- Name",
        "    Zone1SupplyRegisterNode, !- Node 1 Name",
        "    West Zone,               !- Node 2 Name",
        "    ZoneConnectionDuct;      !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone1ReturnConnectionLink,  !- Name",
        "    West Zone,               !- Node 1 Name",
        "    Zone1OutletNode,         !- Node 2 Name",
        "    ZoneConnectionDuct;      !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone2Supply1Link,        !- Name",
        "    MainSplitterNode,        !- Node 1 Name",
        "    Zone2SupplyNode,         !- Node 2 Name",
        "    Zone2Supply,             !- Component Name",
        "    Attic Zone;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone2Supply2Link,        !- Name",
        "    Zone2SupplyNode,         !- Node 1 Name",
        "    ReheatInlet2Node,        !- Node 2 Name",
        "    Zone2Supply,             !- Component Name",
        "    Attic Zone;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone2ReheatCoilLink,     !- Name",
        "    ReheatInlet2Node,        !- Node 1 Name",
        "    Zone2SupplyRegisterNode, !- Node 2 Name",
        "    Reheat Zone 2;           !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone2SupplyConnectionLink,  !- Name",
        "    Zone2SupplyRegisterNode, !- Node 1 Name",
        "    EAST ZONE,               !- Node 2 Name",
        "    ZoneConnectionDuct;      !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone2returnConnectionLink,  !- Name",
        "    EAST ZONE,               !- Node 1 Name",
        "    Zone2OutletNode,         !- Node 2 Name",
        "    ZoneConnectionDuct;      !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone3Supply1Link,        !- Name",
        "    MainSplitterNode,        !- Node 1 Name",
        "    Zone3SupplyNode,         !- Node 2 Name",
        "    Zone3Supply,             !- Component Name",
        "    Attic Zone;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone3SupplyLink,         !- Name",
        "    Zone3SupplyNode,         !- Node 1 Name",
        "    Zone3SupplyRegisterNode, !- Node 2 Name",
        "    Zone3Supply,             !- Component Name",
        "    Attic Zone;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone3SupplyConnectionLink,  !- Name",
        "    Zone3SupplyRegisterNode, !- Node 1 Name",
        "    NORTH ZONE,              !- Node 2 Name",
        "    ZoneConnectionDuct;      !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone3ReturnConnectionLink,  !- Name",
        "    NORTH ZONE,              !- Node 1 Name",
        "    Zone3OutletNode,         !- Node 2 Name",
        "    ZoneConnectionDuct;      !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone1Return1Link,        !- Name",
        "    Zone1OutletNode,         !- Node 1 Name",
        "    Zone1ReturnNode,         !- Node 2 Name",
        "    Zone1Return,             !- Component Name",
        "    Attic Zone;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone1Return2Link,        !- Name",
        "    Zone1ReturnNode,         !- Node 1 Name",
        "    MainMixerNode,           !- Node 2 Name",
        "    Zone1Return,             !- Component Name",
        "    Attic Zone;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone2Return1Link,        !- Name",
        "    Zone2OutletNode,         !- Node 1 Name",
        "    Zone2ReturnNode,         !- Node 2 Name",
        "    Zone2Return,             !- Component Name",
        "    Attic Zone;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone2Return2Link,        !- Name",
        "    Zone2ReturnNode,         !- Node 1 Name",
        "    MainMixerNode,           !- Node 2 Name",
        "    Zone2Return,             !- Component Name",
        "    Attic Zone;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone3Return1Link,        !- Name",
        "    Zone3OutletNode,         !- Node 1 Name",
        "    Zone3ReturnNode,         !- Node 2 Name",
        "    Zone3Return,             !- Component Name",
        "    Attic Zone;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone3Return2Link,        !- Name",
        "    Zone3ReturnNode,         !- Node 1 Name",
        "    MainMixerNode,           !- Node 2 Name",
        "    Zone3Return,             !- Component Name",
        "    Attic Zone;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    ReturnMixerLink,         !- Name",
        "    MainMixerNode,           !- Node 1 Name",
        "    MainReturnNode,          !- Node 2 Name",
        "    MixerConnectionDuct,     !- Component Name",
        "    Attic Zone;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    AirLoopReturnLink,       !- Name",
        "    MainReturnNode,          !- Node 1 Name",
        "    MainInletNode,           !- Node 2 Name",
        "    AirLoopReturn;           !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    OASystemInletLink,       !- Name",
        "    MainInletNode,           !- Node 1 Name",
        "    OA System Node,          !- Node 2 Name",
        "    ZoneConnectionDuct;      !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    OASystemFanLink,         !- Name",
        "    OA Inlet Node,           !- Node 1 Name",
        "    OA System Node,          !- Node 2 Name",
        "    OA Fan;                  !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    OASystemReliefLink,      !- Name",
        "    OA System Node,          !- Node 1 Name",
        "    OA Inlet Node,           !- Node 2 Name",
        "    Relief Fan;              !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    OAMixerOutletLink,       !- Name",
        "    OA System Node,          !- Node 1 Name",
        "    FanInletNode,            !- Node 2 Name",
        "    ZoneConnectionDuct;      !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    SupplyFanLink,           !- Name",
        "    FanInletNode,            !- Node 1 Name",
        "    FanOutletNode,           !- Node 2 Name",
        "    Supply Fan 1;            !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    CoolingCoilLink,         !- Name",
        "    FanOutletNode,           !- Node 1 Name",
        "    HeatingInletNode,        !- Node 2 Name",
        "    ACDXCoil 1;              !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    HeatingCoilLink,         !- Name",
        "    HeatingInletNode,        !- Node 1 Name",
        "    HeatingOutletNode,       !- Node 2 Name",
        "    Main Heating Coil 1;     !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    EquipmentAirLoopLink,    !- Name",
        "    HeatingOutletNode,       !- Node 1 Name",
        "    EquipmentInletNode,      !- Node 2 Name",
        "    AirLoopSupply;           !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone1ReturnLeakLink,     !- Name",
        "    Zone1ReturnNode,         !- Node 1 Name",
        "    OA Inlet Node,           !- Node 2 Name",
        "    ReturnLeakELR1;          !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    MainSupplyLeakLink,      !- Name",
        "    SupplyMainNode,          !- Node 1 Name",
        "    ATTIC ZONE,              !- Node 2 Name",
        "    MainSupplyLeak;          !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone1SupplyLeakLink,     !- Name",
        "    Zone1SupplyNode,         !- Node 1 Name",
        "    ATTIC ZONE,              !- Node 2 Name",
        "    ZoneSupplyELR1;          !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone2ReturnLeakLink,     !- Name",
        "    Zone2ReturnNode,         !- Node 1 Name",
        "    OA Inlet Node,           !- Node 2 Name",
        "    ReturnLeakELR2;          !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone3ReturnLeakLink,     !- Name",
        "    Zone3ReturnNode,         !- Node 1 Name",
        "    OA Inlet Node,           !- Node 2 Name",
        "    ReturnLeakELR3;          !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone2SupplyLeakLink,     !- Name",
        "    Zone2SupplyNode,         !- Node 1 Name",
        "    ATTIC ZONE,              !- Node 2 Name",
        "    ZoneSupplyELR2;          !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone3SupplyLeakLink,     !- Name",
        "    Zone3SupplyNode,         !- Node 1 Name",
        "    ATTIC ZONE,              !- Node 2 Name",
        "    ZoneSupplyELR3;          !- Component Name",

        "  AirflowNetwork:ZoneControl:PressureController,",
        "    PressureController,      !- Name",
        "    NORTH ZONE,              !- Control Zone Name",
        "    AirflowNetwork:Distribution:Component:ReliefAirFlow, !- Control Object Type",
        "    Relief Fan,              !- Control Object Name",
        "    ,                        !- Pressure Control Availability Schedule Name",
        "    Pressure Setpoint Schedule; !- Pressure Setpoint Schedule Name",

        "  Schedule:Compact,",
        "    Pressure Setpoint Schedule,   !- Name",
        "    Any Number,              !- Schedule Type Limits Name",
        "    Through: 3/31,           !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,0.5,        !- Field 3",
        "    Through: 9/30,           !- Field 4",
        "    For: AllDays,            !- Field 5",
        "    Until: 24:00,3.5,        !- Field 6",
        "    Through: 12/31,          !- Field 7",
        "    For: AllDays,            !- Field 8",
        "    Until: 24:00,0.5;        !- Field 9",

        "  AirflowNetwork:Distribution:Component:OutdoorAirFlow,",
        "    OA Fan,                  !- Name",
        "	 OA Mixing Box 1, !- Outdoor Air Mixer Name",
        "    0.001,                   !- Air Mass Flow Coefficient When the Zone Exhaust Fan is Off at Reference Conditions {kg/s}",
        "    0.667;                   !- Air Mass Flow Exponent When the Zone Exhaust Fan is Off {dimensionless}",

        "  AirflowNetwork:Distribution:Component:ReliefAirFlow,",
        "    Relief Fan,              !- Name",
        "	 OA Mixing Box 1, !- Outdoor Air Mixer Name",
        "    0.001,                   !- Air Mass Flow Coefficient When the Zone Exhaust Fan is Off at Reference Conditions {kg/s}",
        "    0.667;                   !- Air Mass Flow Exponent When the Zone Exhaust Fan is Off {dimensionless}",

        "  AirLoopHVAC,",
        "    Typical Terminal Reheat 1,  !- Name",
        "    ,                        !- Controller List Name",
        "    Reheat System 1 Avail List,  !- Availability Manager List Name",
        "    1.9,                     !- Design Supply Air Flow Rate {m3/s}",
        "    Air Loop Branches,       !- Branch List Name",
        "    ,                        !- Connector List Name",
        "    Air Loop Inlet Node,     !- Supply Side Inlet Node Name",
        "    Return Air Mixer Outlet, !- Demand Side Outlet Node Name",
        "    Zone Equipment Inlet Node,  !- Demand Side Inlet Node Names",
        "    Air Loop Outlet Node;    !- Supply Side Outlet Node Names",

        "  Fan:ZoneExhaust,",
        "    Zone3 Exhaust Fan,       !- Name",
        "    On,    !- Availability Schedule Name",
        "    0.7,                     !- Fan Total Efficiency",
        "    500,                     !- Pressure Rise {Pa}",
        "    0.01,                    !- Maximum Flow Rate {m3/s}",
        "    Zone3 Exhaust Node,      !- Air Inlet Node Name",
        "    Zone3 Exhaust Fan Outlet Node,  !- Air Outlet Node Name",
        "    Zone Exhaust;            !- End-Use Subcategory",

        "  Fan:ConstantVolume,",
        "    Supply Fan 1,            !- Name",
        "    FanAndCoilAvailSched,    !- Availability Schedule Name",
        "    0.7,                     !- Fan Total Efficiency",
        "    600.0,                   !- Pressure Rise {Pa}",
        "    1.9,                     !- Maximum Flow Rate {m3/s}",
        "    0.9,                     !- Motor Efficiency",
        "    1.0,                     !- Motor In Airstream Fraction",
        "    Mixed Air Node,          !- Air Inlet Node Name",
        "    Cooling Coil Air Inlet Node;  !- Air Outlet Node Name",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    using namespace EnergyPlus::DataIPShortCuts;

    lNumericFieldBlanks.allocate(1000);
    lAlphaFieldBlanks.allocate(1000);
    cAlphaFieldNames.allocate(1000);
    cNumericFieldNames.allocate(1000);
    cAlphaArgs.allocate(1000);
    rNumericArgs.allocate(1000);
    lNumericFieldBlanks = false;
    lAlphaFieldBlanks = false;
    cAlphaFieldNames = " ";
    cNumericFieldNames = " ";
    cAlphaArgs = " ";
    rNumericArgs = 0.0;

    bool ErrorsFound = false;
    // Read objects
    HeatBalanceManager::GetProjectControlData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetZoneData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetWindowGlassSpectralData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetMaterialData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetConstructData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    SurfaceGeometry::GetGeometryParameters(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    SurfaceGeometry::CosBldgRotAppGonly = 1.0;
    SurfaceGeometry::SinBldgRotAppGonly = 0.0;
    SurfaceGeometry::GetSurfaceData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    // Read AirflowNetwork inputs
    GetAirflowNetworkInput();

    Real64 PresssureSet = 0.5;

    Schedule(UtilityRoutines::FindItemInList("PRESSURE SETPOINT SCHEDULE", Schedule({1, NumSchedules}))).CurrentValue =
        PresssureSet; // Pressure setpoint
    Schedule(UtilityRoutines::FindItemInList("FANANDCOILAVAILSCHED", Schedule({1, NumSchedules}))).CurrentValue =
        1.0;                                                                                         // set availability and fan schedule to 1
    Schedule(UtilityRoutines::FindItemInList("ON", Schedule({1, NumSchedules}))).CurrentValue = 1.0; // On
    Schedule(UtilityRoutines::FindItemInList("VENTINGSCHED", Schedule({1, NumSchedules}))).CurrentValue = 25.55;  // VentingSched
    Schedule(UtilityRoutines::FindItemInList("WINDOWVENTSCHED", Schedule({1, NumSchedules}))).CurrentValue = 1.0; // WindowVentSched

    AirflowNetworkFanActivated = true;
    DataEnvironment::OutDryBulbTemp = -17.29025;
    DataEnvironment::OutHumRat = 0.0008389;
    DataEnvironment::OutBaroPress = 99063.0;
    DataEnvironment::WindSpeed = 4.9;
    DataEnvironment::WindDir = 270.0;

    int index = UtilityRoutines::FindItemInList("OA INLET NODE", AirflowNetworkNodeData);
    for (i = 1; i <= 36; ++i) {
        AirflowNetworkNodeSimu(i).TZ = 23.0;
        AirflowNetworkNodeSimu(i).WZ = 0.0008400;
        if ((i > 4 && i < 10) || i == index) { // NFACADE, EFACADE, SFACADE, WFACADE, HORIZONTAL are always at indexes 5 through 9
            AirflowNetworkNodeSimu(i).TZ =
                DataEnvironment::OutDryBulbTempAt(AirflowNetworkNodeData(i).NodeHeight); // AirflowNetworkNodeData vals differ
            AirflowNetworkNodeSimu(i).WZ = DataEnvironment::OutHumRat;
        }
    }

    // Set up node values
    Node.allocate(10);
    if (MultizoneCompExhaustFanData(1).InletNode == 0) {
        MultizoneCompExhaustFanData(1).InletNode = 3;
    }
    Node(MultizoneCompExhaustFanData(1).InletNode).MassFlowRate = 0.1005046;

    if (DisSysCompCVFData(1).InletNode == 0) {
        DisSysCompCVFData(1).InletNode = 1;
    }
    Node(DisSysCompCVFData(1).InletNode).MassFlowRate = 2.23418088;
    DisSysCompCVFData(1).FlowRate = Node(DisSysCompCVFData(1).InletNode).MassFlowRate;

    if (DisSysCompOutdoorAirData(1).InletNode == 0) {
        DisSysCompOutdoorAirData(1).InletNode = 5;
        DisSysCompOutdoorAirData(1).OutletNode = 6;
    }
    Node(DisSysCompOutdoorAirData(1).InletNode).MassFlowRate = 0.5095108;
    Node(DisSysCompOutdoorAirData(1).OutletNode).MassFlowRate = 0.5095108;

    if (DisSysCompReliefAirData(1).InletNode == 0) {
        DisSysCompReliefAirData(1).InletNode = 6;
        DisSysCompReliefAirData(1).OutletNode = 5;
    }
    AirflowNetworkNodeData(3).AirLoopNum = 1;
    AirflowNetworkLinkageData(46).AirLoopNum = 1;

    DataAirLoop::AirLoopAFNInfo.allocate(1);
//    DataAirLoop::LoopOnOffFanPartLoadRatio.allocate(1);
    DataAirLoop::AirLoopAFNInfo(1).LoopFanOperationMode = 0.0;
    DataAirLoop::AirLoopAFNInfo(1).LoopOnOffFanPartLoadRatio = 0.0;
    // Calculate mass flow rate based on pressure setpoint
    PressureControllerData(1).OANodeNum = DisSysCompReliefAirData(1).OutletNode;
    CalcAirflowNetworkAirBalance();

    // Check indoor pressure and mass flow rate
    EXPECT_NEAR(PresssureSet, AirflowNetworkNodeSimu(3).PZ, 0.0001);
    EXPECT_NEAR(0.00255337, ReliefMassFlowRate, 0.0001);

    // Start a test for #5687 to report zero values of AirflowNetwork:Distribution airflow and pressure outputs when a system is off
    AirflowNetworkFanActivated = false;

    AirflowNetworkExchangeData.allocate(NumOfZones);

    UpdateAirflowNetwork();

    EXPECT_NEAR(0.0, AirflowNetworkNodeSimu(10).PZ, 0.0001);
    EXPECT_NEAR(0.0, AirflowNetworkNodeSimu(20).PZ, 0.0001);
    EXPECT_NEAR(0.0, AirflowNetworkLinkReport(20).FLOW, 0.0001);
    EXPECT_NEAR(0.0, AirflowNetworkLinkReport(50).FLOW, 0.0001);

    AirflowNetworkExchangeData.deallocate();

    // Start a test for #6005
    ANZT = 26.0;
    MultizoneSurfaceData(2).HybridVentClose = true;
    MultizoneSurfaceData(5).HybridVentClose = true;
    MultizoneSurfaceData(14).HybridVentClose = true;
    CalcAirflowNetworkAirBalance();
    EXPECT_EQ(0.0, MultizoneSurfaceData(2).OpenFactor);
    EXPECT_EQ(0.0, MultizoneSurfaceData(5).OpenFactor);
    EXPECT_EQ(0.0, MultizoneSurfaceData(14).OpenFactor);
    EXPECT_EQ(0.0, SurfaceWindow(2).VentingOpenFactorMultRep);
    EXPECT_EQ(0.0, SurfaceWindow(5).VentingOpenFactorMultRep);
    EXPECT_EQ(0.0, SurfaceWindow(14).VentingOpenFactorMultRep);

    Node.deallocate();
}
TEST_F(EnergyPlusFixture, TestZoneVentingSchWithAdaptiveCtrl)
{

    // Unit test for #5490

    Zone.allocate(1);
    Zone(1).Name = "SOFF";

    Surface.allocate(2);
    Surface(1).Name = "WINDOW 1";
    Surface(1).Zone = 1;
    Surface(1).ZoneName = "SOFF";
    Surface(1).Azimuth = 0.0;
    Surface(1).ExtBoundCond = 0;
    Surface(1).HeatTransSurf = true;
    Surface(1).Tilt = 90.0;
    Surface(1).Sides = 4;
    Surface(2).Name = "WINDOW 2";
    Surface(2).Zone = 1;
    Surface(2).ZoneName = "SOFF";
    Surface(2).Azimuth = 180.0;
    Surface(2).ExtBoundCond = 0;
    Surface(2).HeatTransSurf = true;
    Surface(2).Tilt = 90.0;
    Surface(2).Sides = 4;

    SurfaceWindow.allocate(2);
    SurfaceWindow(1).OriginalClass = 11;
    SurfaceWindow(2).OriginalClass = 11;
    NumOfZones = 1;

    TotPeople = 1; // Total number of people statements
    People.allocate(TotPeople);
    People(1).ZonePtr = 1;
    People(1).NumberOfPeople = 100.0;
    People(1).NumberOfPeoplePtr = 1; // From dataglobals, always returns a 1 for schedule value
    People(1).AdaptiveCEN15251 = true;

    std::string const idf_objects = delimited_string({
        "Version,8.5;",
        "Schedule:Constant,OnSch,,1.0;",
        "Schedule:Constant,FreeRunningSeason,,0.0;",
        "Schedule:Constant,Sempre 21,,21.0;",
        "AirflowNetwork:SimulationControl,",
        "  NaturalVentilation, !- Name",
        "  MultizoneWithoutDistribution, !- AirflowNetwork Control",
        "  SurfaceAverageCalculation, !- Wind Pressure Coefficient Type",
        "  , !- Height Selection for Local Wind Pressure Calculation",
        "  LOWRISE, !- Building Type",
        "  1000, !- Maximum Number of Iterations{ dimensionless }",
        "  ZeroNodePressures, !- Initialization Type",
        "  0.0001, !- Relative Airflow Convergence Tolerance{ dimensionless }",
        "  0.0001, !- Absolute Airflow Convergence Tolerance{ kg / s }",
        "  -0.5, !- Convergence Acceleration Limit{ dimensionless }",
        "  90, !- Azimuth Angle of Long Axis of Building{ deg }",
        "  0.36;                    !- Ratio of Building Width Along Short Axis to Width Along Long Axis",
        "AirflowNetwork:MultiZone:Zone,",
        "  Soff, !- Zone Name",
        "  CEN15251Adaptive, !- Ventilation Control Mode",
        "  , !- Ventilation Control Zone Temperature Setpoint Schedule Name",
        "  , !- Minimum Venting Open Factor{ dimensionless }",
        "  , !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor{ deltaC }",
        "  100, !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor{ deltaC }",
        "  , !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor{ deltaJ / kg }",
        "  300000, !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor{ deltaJ / kg }",
        "  FreeRunningSeason; !- Venting Availability Schedule Name",
        "AirflowNetwork:MultiZone:Surface,",
        "  window 1, !- Surface Name",
        "  Simple Window, !- Leakage Component Name",
        "  , !- External Node Name",
        "  1, !- Window / Door Opening Factor, or Crack Factor{ dimensionless }",
        "  ZoneLevel; !- Ventilation Control Mode",
        "AirflowNetwork:MultiZone:Surface,",
        "  window 2, !- Surface Name",
        "  Simple Window, !- Leakage Component Name",
        "  , !- External Node Name",
        "  1, !- Window / Door Opening Factor, or Crack Factor{ dimensionless }",
        "  ZoneLevel; !- Ventilation Control Mode",
        "AirflowNetwork:MultiZone:Component:SimpleOpening,",
        "  Simple Window, !- Name",
        "  0.0010, !- Air Mass Flow Coefficient When Opening is Closed{ kg / s - m }",
        "  0.65, !- Air Mass Flow Exponent When Opening is Closed{ dimensionless }",
        "  0.01, !- Minimum Density Difference for Two - Way Flow{ kg / m3 }",
        "  0.78;                    !- Discharge Coefficient{ dimensionless }",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    GetAirflowNetworkInput();

    // The original value before fix is zero. After the fix, the correct schedule number is assigned.

    // changed index 2 to 1 because in new sorted scheedule MultizoneZone(1).VentingSchName ("FREERUNNINGSEASON")
    // has index 1 which is the .VentSchNum
    auto GetIndex = UtilityRoutines::FindItemInList(MultizoneZoneData(1).VentingSchName, Schedule({1, NumSchedules}));
    EXPECT_EQ(GetIndex, MultizoneZoneData(1).VentingSchNum);

    Zone.deallocate();
    Surface.deallocate();
    SurfaceWindow.deallocate();
    People.deallocate();
}

TEST_F(EnergyPlusFixture, AirflowNetworkBalanceManagerTest_PolygonalWindows)
{

    // Unit test for a new feature

    Zone.allocate(1);
    Zone(1).Name = "ZONE 1";

    Surface.allocate(14);
    // Rectangular base surface
    Surface(1).Name = "LIVING:NORTH";
    Surface(1).Zone = 1;
    Surface(1).ZoneName = "ZONE 1";
    Surface(1).Azimuth = 180.0;
    Surface(1).ExtBoundCond = 0;
    Surface(1).HeatTransSurf = true;
    Surface(1).Tilt = 90.0;
    Surface(1).Sides = 4;
    Surface(1).Area = 25.17;
    Surface(1).Vertex.allocate(4);
    Surface(1).Vertex(1).x = 10.323;
    Surface(1).Vertex(2).x = 10.323;
    Surface(1).Vertex(3).x = 0.0;
    Surface(1).Vertex(4).x = 0.0;
    Surface(1).Vertex(1).y = 10.778;
    Surface(1).Vertex(2).y = 10.778;
    Surface(1).Vertex(3).y = 10.778;
    Surface(1).Vertex(4).y = 10.778;
    Surface(1).Vertex(1).z = 2.4384;
    Surface(1).Vertex(2).z = 0.0;
    Surface(1).Vertex(3).z = 0.0;
    Surface(1).Vertex(4).z = 2.4384;

    // Rectangular base surface
    Surface(2).Name = "LIVING:SOUTH";
    Surface(2).Zone = 1;
    Surface(2).ZoneName = "ZONE 1";
    Surface(2).Azimuth = 0.0;
    Surface(2).ExtBoundCond = 0;
    Surface(2).HeatTransSurf = true;
    Surface(2).Tilt = 90.0;
    Surface(2).Sides = 4;
    Surface(2).Width = 10.323;
    Surface(2).Height = 2.4384;
    Surface(2).Area = 25.17;
    Surface(2).Vertex.allocate(4);
    Surface(2).Vertex(1).x = 10.323;
    Surface(2).Vertex(2).x = 10.323;
    Surface(2).Vertex(3).x = 0.0;
    Surface(2).Vertex(4).x = 0.0;
    Surface(2).Vertex(1).y = 0.0;
    Surface(2).Vertex(2).y = 0.0;
    Surface(2).Vertex(3).y = 0.0;
    Surface(2).Vertex(4).y = 0.0;
    Surface(2).Vertex(1).z = 2.4384;
    Surface(2).Vertex(2).z = 0.0;
    Surface(2).Vertex(3).z = 0.0;
    Surface(2).Vertex(4).z = 2.4384;

    // Polygonal base surface
    Surface(3).Name = "LIVING:EAST";
    Surface(3).Zone = 1;
    Surface(3).ZoneName = "ZONE 1";
    Surface(3).Azimuth = 90.0;
    Surface(3).ExtBoundCond = 0;
    Surface(3).HeatTransSurf = true;
    Surface(3).Tilt = 90.0;
    Surface(3).Sides = 5;
    Surface(3).Area = 25.17;
    Surface(3).Vertex.allocate(5);
    Surface(3).Vertex(1).x = 10.0;
    Surface(3).Vertex(2).x = 10.0;
    Surface(3).Vertex(3).x = 10.0;
    Surface(3).Vertex(4).x = 10.0;
    Surface(3).Vertex(5).x = 10.0;
    Surface(3).Vertex(1).y = 0.0;
    Surface(3).Vertex(2).y = 0.0;
    Surface(3).Vertex(3).y = 10.0;
    Surface(3).Vertex(4).y = 10.0;
    Surface(3).Vertex(5).y = 5.0;
    Surface(3).Vertex(1).z = 2.0;
    Surface(3).Vertex(2).z = 0.0;
    Surface(3).Vertex(3).z = 0.0;
    Surface(3).Vertex(4).z = 2.0;
    Surface(3).Vertex(5).z = 2.5;

    // Triangular window sub surface
    Surface(4).Name = "NORTH:WINDOW1";
    Surface(4).Zone = 1;
    Surface(4).ZoneName = "ZONE 1";
    Surface(4).Azimuth = 180.0;
    Surface(4).ExtBoundCond = 0;
    Surface(4).HeatTransSurf = true;
    Surface(4).Tilt = 90.0;
    Surface(4).Sides = 3;
    Surface(4).Area = 1.0;
    Surface(4).BaseSurf = 1;
    Surface(4).Vertex.allocate(3);
    Surface(4).Vertex(1).x = 3.0;
    Surface(4).Vertex(2).x = 3.0;
    Surface(4).Vertex(3).x = 1.0;
    Surface(4).Vertex(1).y = 10.778;
    Surface(4).Vertex(2).y = 10.778;
    Surface(4).Vertex(3).y = 10.778;
    Surface(4).Vertex(1).z = 2.0;
    Surface(4).Vertex(2).z = 1.0;
    Surface(4).Vertex(3).z = 1.0;

    // Polygonal window sub surface
    Surface(5).Name = "NORTH:WINDOW2";
    Surface(5).Zone = 1;
    Surface(5).ZoneName = "ZONE 1";
    Surface(5).Azimuth = 180.0;
    Surface(5).ExtBoundCond = 0;
    Surface(5).HeatTransSurf = true;
    Surface(5).Tilt = 90.0;
    Surface(5).Sides = 5;
    Surface(5).Area = 2.5;
    Surface(5).BaseSurf = 1;
    Surface(5).Vertex.allocate(5);
    Surface(5).Vertex(1).x = 5.0;
    Surface(5).Vertex(2).x = 5.0;
    Surface(5).Vertex(3).x = 3.0;
    Surface(5).Vertex(4).x = 3.0;
    Surface(5).Vertex(5).x = 4.0;
    Surface(5).Vertex(1).y = 10.778;
    Surface(5).Vertex(2).y = 10.778;
    Surface(5).Vertex(3).y = 10.778;
    Surface(5).Vertex(4).y = 10.778;
    Surface(5).Vertex(5).y = 10.778;
    Surface(5).Vertex(1).z = 2.0;
    Surface(5).Vertex(2).z = 1.0;
    Surface(5).Vertex(3).z = 1.0;
    Surface(5).Vertex(4).z = 2.0;
    Surface(5).Vertex(5).z = 2.5;

    // Triangular window sub surface
    Surface(6).Name = "SOUTH:WINDOW1";
    Surface(6).Zone = 1;
    Surface(6).ZoneName = "ZONE 1";
    Surface(6).Azimuth = 0.0;
    Surface(6).ExtBoundCond = 0;
    Surface(6).HeatTransSurf = true;
    Surface(6).Tilt = 90.0;
    Surface(6).Sides = 3;
    Surface(6).Area = 0.5;
    Surface(6).BaseSurf = 2;
    Surface(6).Vertex.allocate(3);
    Surface(6).Vertex(1).x = 9.0;
    Surface(6).Vertex(2).x = 9.0;
    Surface(6).Vertex(3).x = 8.0;
    Surface(6).Vertex(1).y = 0.0;
    Surface(6).Vertex(2).y = 0.0;
    Surface(6).Vertex(3).y = 0.0;
    Surface(6).Vertex(1).z = 2.0;
    Surface(6).Vertex(2).z = 1.0;
    Surface(6).Vertex(3).z = 1.0;

    // Triangular window sub surface
    Surface(9).Name = "SOUTH:WINDOW2";
    Surface(9).Zone = 1;
    Surface(9).ZoneName = "ZONE 1";
    Surface(9).Azimuth = 0.0;
    Surface(9).ExtBoundCond = 0;
    Surface(9).HeatTransSurf = true;
    Surface(9).Tilt = 90.0;
    Surface(9).Sides = 3;
    Surface(9).Area = 0.5;
    Surface(9).BaseSurf = 2;
    Surface(9).Vertex.allocate(3);
    Surface(9).Vertex(1).x = 7.0;
    Surface(9).Vertex(2).x = 7.0;
    Surface(9).Vertex(3).x = 6.0;
    Surface(9).Vertex(1).y = 0.0;
    Surface(9).Vertex(2).y = 0.0;
    Surface(9).Vertex(3).y = 0.0;
    Surface(9).Vertex(1).z = 2.0;
    Surface(9).Vertex(2).z = 1.0;
    Surface(9).Vertex(3).z = 1.0;

    Surface(10).Name = "EAST:WINDOW1";
    Surface(10).Zone = 1;
    Surface(10).ZoneName = "ZONE 1";
    Surface(10).Azimuth = 90.0;
    Surface(10).ExtBoundCond = 0;
    Surface(10).HeatTransSurf = true;
    Surface(10).Tilt = 90.0;
    Surface(10).Sides = 3;
    Surface(10).Area = 0.5;
    Surface(10).BaseSurf = 3;
    Surface(10).Vertex.allocate(3);
    Surface(10).Vertex(1).x = 10.0;
    Surface(10).Vertex(2).x = 10.0;
    Surface(10).Vertex(3).x = 10.0;
    Surface(10).Vertex(1).y = 1.0;
    Surface(10).Vertex(2).y = 1.0;
    Surface(10).Vertex(3).y = 2.0;
    Surface(10).Vertex(1).z = 2.0;
    Surface(10).Vertex(2).z = 1.0;
    Surface(10).Vertex(3).z = 1.0;

    // Polygonal horizontal base surface
    Surface(7).Name = "ROOF-POLY";
    Surface(7).Zone = 1;
    Surface(7).ZoneName = "ZONE 1";
    Surface(7).Azimuth = 0.0;
    Surface(7).ExtBoundCond = 0;
    Surface(7).HeatTransSurf = true;
    Surface(7).Tilt = 0.0;
    Surface(7).Sides = 5;
    Surface(7).Area = 55.0;
    Surface(7).Vertex.allocate(5);
    Surface(7).Vertex(1).x = 0.0;
    Surface(7).Vertex(2).x = 0.0;
    Surface(7).Vertex(3).x = 10.0;
    Surface(7).Vertex(4).x = 10.0;
    Surface(7).Vertex(5).x = 5.0;
    Surface(7).Vertex(1).y = 10.0;
    Surface(7).Vertex(2).y = 5.0;
    Surface(7).Vertex(3).y = 5.0;
    Surface(7).Vertex(4).y = 10.0;
    Surface(7).Vertex(5).y = 11.0;
    Surface(7).Vertex(1).z = 2.4384;
    Surface(7).Vertex(2).z = 2.4384;
    Surface(7).Vertex(3).z = 2.4384;
    Surface(7).Vertex(4).z = 2.4384;
    Surface(7).Vertex(5).z = 2.4384;

    // Polygonal horizontal base surface
    Surface(13).Name = "ROOF-POLY-WINDOW1";
    Surface(13).Zone = 1;
    Surface(13).ZoneName = "ZONE 1";
    Surface(13).Azimuth = 0.0;
    Surface(13).ExtBoundCond = 0;
    Surface(13).HeatTransSurf = true;
    Surface(13).Tilt = 0.0;
    Surface(13).Sides = 3;
    Surface(13).Area = 1;
    Surface(13).BaseSurf = 7;
    Surface(13).Vertex.allocate(3);
    Surface(13).Vertex(1).x = 8.0;
    Surface(13).Vertex(2).x = 8.0;
    Surface(13).Vertex(3).x = 9.0;
    Surface(13).Vertex(1).y = 9.0;
    Surface(13).Vertex(2).y = 7.0;
    Surface(13).Vertex(3).y = 7.0;
    Surface(13).Vertex(1).z = 2.4384;
    Surface(13).Vertex(2).z = 2.4384;
    Surface(13).Vertex(3).z = 2.4384;

    // Polygonal horizontal base surface
    Surface(14).Name = "ROOF-POLY-WINDOW2";
    Surface(14).Zone = 1;
    Surface(14).ZoneName = "ZONE 1";
    Surface(14).Azimuth = 0.0;
    Surface(14).ExtBoundCond = 0;
    Surface(14).HeatTransSurf = true;
    Surface(14).Tilt = 0.0;
    Surface(14).Sides = 3;
    Surface(14).Area = 1;
    Surface(14).BaseSurf = 7;
    Surface(14).Vertex.allocate(3);
    Surface(14).Vertex(1).x = 6.0;
    Surface(14).Vertex(2).x = 6.0;
    Surface(14).Vertex(3).x = 7.0;
    Surface(14).Vertex(1).y = 9.0;
    Surface(14).Vertex(2).y = 7.0;
    Surface(14).Vertex(3).y = 7.0;
    Surface(14).Vertex(1).z = 2.4384;
    Surface(14).Vertex(2).z = 2.4384;
    Surface(14).Vertex(3).z = 2.4384;

    // Rectangular horizontal base surface
    Surface(8).Name = "ROOF-REC";
    Surface(8).Zone = 1;
    Surface(8).ZoneName = "ZONE 1";
    Surface(8).Azimuth = 0.0;
    Surface(8).ExtBoundCond = 0;
    Surface(8).HeatTransSurf = true;
    Surface(8).Tilt = 0.0;
    Surface(8).Sides = 4;
    Surface(8).Area = 50.0;
    Surface(8).Width = 10.0;
    Surface(8).Height = 5.0;
    Surface(8).Vertex.allocate(4);
    Surface(8).Vertex(1).x = 0.0;
    Surface(8).Vertex(2).x = 0.0;
    Surface(8).Vertex(3).x = 10.0;
    Surface(8).Vertex(4).x = 10.0;
    Surface(8).Vertex(1).y = 5.0;
    Surface(8).Vertex(2).y = 0.0;
    Surface(8).Vertex(3).y = 0.0;
    Surface(8).Vertex(4).y = 5.0;
    Surface(8).Vertex(1).z = 2.4384;
    Surface(8).Vertex(2).z = 2.4384;
    Surface(8).Vertex(3).z = 2.4384;
    Surface(8).Vertex(4).z = 2.4384;

    // Rectangular horizontal base surface
    Surface(12).Name = "ROOF-REC-WINDOW1";
    Surface(12).Zone = 1;
    Surface(12).ZoneName = "ZONE 1";
    Surface(12).Azimuth = 0.0;
    Surface(12).ExtBoundCond = 0;
    Surface(12).HeatTransSurf = true;
    Surface(12).Tilt = 0.0;
    Surface(12).Sides = 3;
    Surface(12).Area = 0.5;
    Surface(12).BaseSurf = 8;
    Surface(12).Vertex.allocate(3);
    Surface(12).Vertex(1).x = 8.0;
    Surface(12).Vertex(2).x = 8.0;
    Surface(12).Vertex(3).x = 9.0;
    Surface(12).Vertex(1).y = 4.0;
    Surface(12).Vertex(2).y = 3.0;
    Surface(12).Vertex(3).y = 3.0;
    Surface(12).Vertex(1).z = 2.4384;
    Surface(12).Vertex(2).z = 2.4384;
    Surface(12).Vertex(3).z = 2.4384;

    Surface(11).Name = "ROOF-REC-WINDOW2";
    Surface(11).Zone = 1;
    Surface(11).ZoneName = "ZONE 1";
    Surface(11).Azimuth = 0.0;
    Surface(11).ExtBoundCond = 0;
    Surface(11).HeatTransSurf = true;
    Surface(11).Tilt = 0.0;
    Surface(11).Sides = 3;
    Surface(11).Area = 0.5;
    Surface(11).BaseSurf = 8;
    Surface(11).Vertex.allocate(3);
    Surface(11).Vertex(1).x = 7.0;
    Surface(11).Vertex(2).x = 7.0;
    Surface(11).Vertex(3).x = 8.0;
    Surface(11).Vertex(1).y = 4.0;
    Surface(11).Vertex(2).y = 3.0;
    Surface(11).Vertex(3).y = 3.0;
    Surface(11).Vertex(1).z = 2.4384;
    Surface(11).Vertex(2).z = 2.4384;
    Surface(11).Vertex(3).z = 2.4384;

    SurfaceWindow.allocate(14);
    SurfaceWindow(4).OriginalClass = 11;
    SurfaceWindow(5).OriginalClass = 11;
    SurfaceWindow(6).OriginalClass = 11;
    SurfaceWindow(9).OriginalClass = 11;
    SurfaceWindow(10).OriginalClass = 11;
    SurfaceWindow(11).OriginalClass = 11;
    SurfaceWindow(12).OriginalClass = 11;
    SurfaceWindow(13).OriginalClass = 11;
    SurfaceWindow(14).OriginalClass = 11;
    NumOfZones = 1;

    std::string const idf_objects = delimited_string({
        "Version,8.6;",
        "Schedule:Constant,OnSch,,1.0;",
        "Schedule:Constant,Aula people sched,,0.0;",
        "Schedule:Constant,Sempre 21,,21.0;",
        "AirflowNetwork:SimulationControl,",
        "  NaturalVentilation, !- Name",
        "  MultizoneWithoutDistribution, !- AirflowNetwork Control",
        "  SurfaceAverageCalculation, !- Wind Pressure Coefficient Type",
        "  , !- Height Selection for Local Wind Pressure Calculation",
        "  LOWRISE, !- Building Type",
        "  1000, !- Maximum Number of Iterations{ dimensionless }",
        "  LinearInitializationMethod, !- Initialization Type",
        "  0.0001, !- Relative Airflow Convergence Tolerance{ dimensionless }",
        "  0.0001, !- Absolute Airflow Convergence Tolerance{ kg / s }",
        "  -0.5, !- Convergence Acceleration Limit{ dimensionless }",
        "  90, !- Azimuth Angle of Long Axis of Building{ deg }",
        "  0.36;                    !- Ratio of Building Width Along Short Axis to Width Along Long Axis",
        "AirflowNetwork:MultiZone:Zone,",
        "  ZONE 1, !- Zone Name",
        "  Temperature, !- Ventilation Control Mode",
        "  Sempre 21, !- Ventilation Control Zone Temperature Setpoint Schedule Name",
        "  1, !- Minimum Venting Open Factor{ dimensionless }",
        "  , !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor{ deltaC }",
        "  100, !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor{ deltaC }",
        "  , !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor{ deltaJ / kg }",
        "  300000, !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor{ deltaJ / kg }",
        "  Aula people sched, !- Venting Availability Schedule Name",
        "  Standard;                !- Single Sided Wind Pressure Coefficient Algorithm",
        "AirflowNetwork:MultiZone:Surface,",
        "  NORTH:WINDOW1, !- Surface Name",
        "  Simple Window, !- Leakage Component Name",
        "  , !- External Node Name",
        "  1, !- Window / Door Opening Factor, or Crack Factor{ dimensionless }",
        "  ZoneLevel, !- Ventilation Control Mode",
        "  , !- Ventilation Control Zone Temperature Setpoint Schedule Name",
        "  , !- Minimum Venting Open Factor{ dimensionless }",
        "  , !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor{ deltaC }",
        "  100, !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor{ deltaC }",
        "  , !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor{ deltaJ / kg }",
        "  300000, !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor{ deltaJ / kg }",
        "  Aula people sched;       !- Venting Availability Schedule Name",
        "AirflowNetwork:MultiZone:Surface,",
        "  NORTH:WINDOW2, !- Surface Name",
        "  Simple Window, !- Leakage Component Name",
        "  , !- External Node Name",
        "  1, !- Window / Door Opening Factor, or Crack Factor{ dimensionless }",
        "  Temperature, !- Ventilation Control Mode",
        "  Sempre 21, !- Ventilation Control Zone Temperature Setpoint Schedule Name",
        "  1, !- Minimum Venting Open Factor{ dimensionless }",
        "  , !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor{ deltaC }",
        "  100, !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor{ deltaC }",
        "  , !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor{ deltaJ / kg }",
        "  300000, !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor{ deltaJ / kg }",
        "  Aula people sched;       !- Venting Availability Schedule Name",
        "AirflowNetwork:MultiZone:Surface,",
        "  SOUTH:WINDOW1, !- Surface Name",
        "  Simple Window, !- Leakage Component Name",
        "  , !- External Node Name",
        "  1, !- Window / Door Opening Factor, or Crack Factor{ dimensionless }",
        "  ZoneLevel, !- Ventilation Control Mode",
        "  , !- Ventilation Control Zone Temperature Setpoint Schedule Name",
        "  , !- Minimum Venting Open Factor{ dimensionless }",
        "  , !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor{ deltaC }",
        "  100, !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor{ deltaC }",
        "  , !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor{ deltaJ / kg }",
        "  300000, !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor{ deltaJ / kg }",
        "  Aula people sched,       !- Venting Availability Schedule Name",
        "  ,       !- Occupant Ventilation Control Name",
        "  BaseSurfaceAspectRatio;       !- Equivalent Rectangle Method",
        "AirflowNetwork:MultiZone:Surface,",
        "  SOUTH:WINDOW2, !- Surface Name",
        "  Simple Window, !- Leakage Component Name",
        "  , !- External Node Name",
        "  1, !- Window / Door Opening Factor, or Crack Factor{ dimensionless }",
        "  Temperature, !- Ventilation Control Mode",
        "  Sempre 21, !- Ventilation Control Zone Temperature Setpoint Schedule Name",
        "  1, !- Minimum Venting Open Factor{ dimensionless }",
        "  , !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor{ deltaC }",
        "  100, !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor{ deltaC }",
        "  , !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor{ deltaJ / kg }",
        "  300000, !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor{ deltaJ / kg }",
        "  Aula people sched,       !- Venting Availability Schedule Name",
        "  ,       !- Occupant Ventilation Control Name",
        "  UserDefinedAspectRatio,       !- Equivalent Rectangle Method",
        "  1.0;       !- Equivalent Rectangle Aspect Ratio",
        "AirflowNetwork:MultiZone:Surface,",
        "  EAST:WINDOW1, !- Surface Name",
        "  Simple Window, !- Leakage Component Name",
        "  , !- External Node Name",
        "  1, !- Window / Door Opening Factor, or Crack Factor{ dimensionless }",
        "  ZoneLevel, !- Ventilation Control Mode",
        "  , !- Ventilation Control Zone Temperature Setpoint Schedule Name",
        "  , !- Minimum Venting Open Factor{ dimensionless }",
        "  , !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor{ deltaC }",
        "  100, !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor{ deltaC }",
        "  , !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor{ deltaJ / kg }",
        "  300000, !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor{ deltaJ / kg }",
        "  Aula people sched,       !- Venting Availability Schedule Name",
        "  ,       !- Occupant Ventilation Control Name",
        "  BaseSurfaceAspectRatio;       !- Equivalent Rectangle Method",

        "AirflowNetwork:MultiZone:Surface,",
        "  ROOF-REC-WINDOW1, !- Surface Name",
        "  CR-1, !- Leakage Component Name",
        "  , !- External Node Name",
        "  1, !- Window / Door Opening Factor, or Crack Factor{ dimensionless }",
        "  ZoneLevel, !- Ventilation Control Mode",
        "  , !- Ventilation Control Zone Temperature Setpoint Schedule Name",
        "  , !- Minimum Venting Open Factor{ dimensionless }",
        "  , !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor{ deltaC }",
        "  100, !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor{ deltaC }",
        "  , !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor{ deltaJ / kg }",
        "  300000, !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor{ deltaJ / kg }",
        "  Aula people sched;       !- Venting Availability Schedule Name",
        "AirflowNetwork:MultiZone:Surface,",
        "  ROOF-REC-WINDOW2, !- Surface Name",
        "  CR-1, !- Leakage Component Name",
        "  , !- External Node Name",
        "  1, !- Window / Door Opening Factor, or Crack Factor{ dimensionless }",
        "  ZoneLevel, !- Ventilation Control Mode",
        "  , !- Ventilation Control Zone Temperature Setpoint Schedule Name",
        "  , !- Minimum Venting Open Factor{ dimensionless }",
        "  , !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor{ deltaC }",
        "  100, !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor{ deltaC }",
        "  , !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor{ deltaJ / kg }",
        "  300000, !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor{ deltaJ / kg }",
        "  Aula people sched,       !- Venting Availability Schedule Name",
        "  ,       !- Occupant Ventilation Control Name",
        "  BaseSurfaceAspectRatio;       !- Equivalent Rectangle Method",

        "AirflowNetwork:MultiZone:Surface,",
        "  ROOF-POLY-WINDOW1, !- Surface Name",
        "  CR-1, !- Leakage Component Name",
        "  , !- External Node Name",
        "  1, !- Window / Door Opening Factor, or Crack Factor{ dimensionless }",
        "  ZoneLevel, !- Ventilation Control Mode",
        "  , !- Ventilation Control Zone Temperature Setpoint Schedule Name",
        "  , !- Minimum Venting Open Factor{ dimensionless }",
        "  , !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor{ deltaC }",
        "  100, !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor{ deltaC }",
        "  , !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor{ deltaJ / kg }",
        "  300000, !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor{ deltaJ / kg }",
        "  Aula people sched,       !- Venting Availability Schedule Name",
        "  ,       !- Occupant Ventilation Control Name",
        "  BaseSurfaceAspectRatio;       !- Equivalent Rectangle Method",
        "AirflowNetwork:MultiZone:Surface,",
        "  ROOF-POLY-WINDOW2, !- Surface Name",
        "  CR-1, !- Leakage Component Name",
        "  , !- External Node Name",
        "  1, !- Window / Door Opening Factor, or Crack Factor{ dimensionless }",
        "  ZoneLevel, !- Ventilation Control Mode",
        "  , !- Ventilation Control Zone Temperature Setpoint Schedule Name",
        "  , !- Minimum Venting Open Factor{ dimensionless }",
        "  , !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor{ deltaC }",
        "  100, !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor{ deltaC }",
        "  , !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor{ deltaJ / kg }",
        "  300000, !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor{ deltaJ / kg }",
        "  Aula people sched;       !- Venting Availability Schedule Name",

        "AirflowNetwork:MultiZone:Component:SimpleOpening,",
        "  Simple Window, !- Name",
        "  0.0010, !- Air Mass Flow Coefficient When Opening is Closed{ kg / s - m }",
        "  0.65, !- Air Mass Flow Exponent When Opening is Closed{ dimensionless }",
        "  0.01, !- Minimum Density Difference for Two - Way Flow{ kg / m3 }",
        "  0.78;                    !- Discharge Coefficient{ dimensionless }",
        "AirflowNetwork:MultiZone:Surface:Crack,",
        "  CR-1, !- Name",
        "  0.05, !- Air Mass Flow Coefficient at Reference Conditions{ kg / s }",
        "  0.667;                   !- Air Mass Flow Exponent{ dimensionless }",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    GetAirflowNetworkInput();

    // Choice: Height; Base Surface: Vertical Rectangular
    EXPECT_NEAR(1.0, MultizoneSurfaceData(1).Width, 0.0001);
    EXPECT_NEAR(1.0, MultizoneSurfaceData(1).Height, 0.0001);
    // Choice: Height; Base Surface: Vertical Polygon
    EXPECT_NEAR(1.666667, MultizoneSurfaceData(2).Width, 0.0001);
    EXPECT_NEAR(1.5, MultizoneSurfaceData(2).Height, 0.0001);
    // Choice: Base aspect ratio; Base Surface: Vertical Rectangular
    EXPECT_NEAR(1.454907, MultizoneSurfaceData(3).Width, 0.0001);
    EXPECT_NEAR(0.343664, MultizoneSurfaceData(3).Height, 0.0001);
    // Choice: User aspect ratio; Base Surface: Vertical Rectangular
    EXPECT_NEAR(0.70711, MultizoneSurfaceData(4).Width, 0.0001);
    EXPECT_NEAR(0.70711, MultizoneSurfaceData(4).Height, 0.0001);
    // Choice: Base aspect ratio --> Height; Base Surface: Vertical Polygon
    EXPECT_NEAR(0.5, MultizoneSurfaceData(5).Width, 0.0001);
    EXPECT_NEAR(1.0, MultizoneSurfaceData(5).Height, 0.0001);
    // Choice: Height --> Base aspect ratio; Base Surface: Horizontal Rectangular
    EXPECT_NEAR(1.0, MultizoneSurfaceData(6).Width, 0.0001);
    EXPECT_NEAR(0.5, MultizoneSurfaceData(6).Height, 0.0001);
    // Choice: Base aspect ratio; Base Surface: Horizontal Rectangular
    EXPECT_NEAR(1.0, MultizoneSurfaceData(7).Width, 0.0001);
    EXPECT_NEAR(0.5, MultizoneSurfaceData(7).Height, 0.0001);
    // Choice: Base aspect ratio --> User Aspect Ratio; Base Surface: Horizontal Polygon
    EXPECT_NEAR(1.0, MultizoneSurfaceData(8).Width, 0.0001);
    EXPECT_NEAR(1.0, MultizoneSurfaceData(8).Height, 0.0001);
    // Choice: Height --> User Aspect Ratio; Base Surface: Horizontal Polygon
    EXPECT_NEAR(1.0, MultizoneSurfaceData(9).Width, 0.0001);
    EXPECT_NEAR(1.0, MultizoneSurfaceData(9).Height, 0.0001);

    Zone.deallocate();
    Surface.deallocate();
    SurfaceWindow.deallocate();
}

TEST_F(EnergyPlusFixture, AirflowNetworkBalanceManager_AFNUserDefinedDuctViewFactors)
{

    std::string const idf_objects = delimited_string({
        "  Version,9.0;",

        "  SimulationControl,",
        "    No,                      !- Do Zone Sizing Calculation",
        "    No,                      !- Do System Sizing Calculation",
        "    No,                      !- Do Plant Sizing Calculation",
        "    No,                      !- Run Simulation for Sizing Periods",
        "    Yes;                     !- Run Simulation for Weather File Run Periods",

        "  Building,",
        "    Exercise 1A,             !- Name",
        "    0.0,                     !- North Axis {deg}",
        "    Country,                 !- Terrain",
        "    0.04,                    !- Loads Convergence Tolerance Value",
        "    0.4,                     !- Temperature Convergence Tolerance Value {deltaC}",
        "    FullInteriorAndExterior, !- Solar Distribution",
        "    ,                        !- Maximum Number of Warmup Days",
        "    6;                       !- Minimum Number of Warmup Days",

        "  ShadowCalculation,",
        "    AverageOverDaysInFrequency,  !- Calculation Method",
        "    20;                      !- Calculation Frequency",

        "  SurfaceConvectionAlgorithm:Inside,",
        "    TARP;                    !- Algorithm",

        "  SurfaceConvectionAlgorithm:Outside,",
        "    TARP;                    !- Algorithm",

        "  HeatBalanceAlgorithm,",
        "    ConductionTransferFunction;  !- Algorithm",

        "  Timestep,",
        "    4;                       !- Number of Timesteps per Hour",

        "  Site:Location,",
        "    Pheonix,                 !- Name",
        "    33.43,                   !- Latitude {deg}",
        "    -112.02,                 !- Longitude {deg}",
        "    -7.0,                    !- Time Zone {hr}",
        "    339.0;                   !- Elevation {m}",

        "  SizingPeriod:DesignDay,",
        "    CHICAGO_IL_USA Cooling .4% Conditions DB=>MWB,  !- Name",
        "    7,                       !- Month",
        "    21,                      !- Day of Month",
        "    SummerDesignDay,         !- Day Type",
        "    32.80000,                !- Maximum Dry-Bulb Temperature {C}",
        "    10.90000,                !- Daily Dry-Bulb Temperature Range {deltaC}",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Type",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name",
        "    Wetbulb,                 !- Humidity Condition Type",
        "    23.60000,                !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}",
        "    ,                        !- Humidity Condition Day Schedule Name",
        "    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}",
        "    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}",
        "    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}",
        "    99063.21,                !- Barometric Pressure {Pa}",
        "    0.0,                     !- Wind Speed {m/s}",
        "    0.0,                     !- Wind Direction {deg}",
        "    No,                      !- Rain Indicator",
        "    No,                      !- Snow Indicator",
        "    No,                      !- Daylight Saving Time Indicator",
        "    ASHRAEClearSky,          !- Solar Model Indicator",
        "    ,                        !- Beam Solar Day Schedule Name",
        "    ,                        !- Diffuse Solar Day Schedule Name",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensionless}",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimensionless}",
        "    1.000000;                !- Sky Clearness",

        "  SizingPeriod:DesignDay,",
        "    CHICAGO_IL_USA Heating 99.6% Conditions,  !- Name",
        "    1,                       !- Month",
        "    21,                      !- Day of Month",
        "    WinterDesignDay,         !- Day Type",
        "    -21.20000,               !- Maximum Dry-Bulb Temperature {C}",
        "    0.0,                     !- Daily Dry-Bulb Temperature Range {deltaC}",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Type",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name",
        "    Wetbulb,                 !- Humidity Condition Type",
        "    -21.20000,               !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}",
        "    ,                        !- Humidity Condition Day Schedule Name",
        "    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}",
        "    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}",
        "    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}",
        "    99063.21,                !- Barometric Pressure {Pa}",
        "    4.600000,                !- Wind Speed {m/s}",
        "    270.0000,                !- Wind Direction {deg}",
        "    No,                      !- Rain Indicator",
        "    No,                      !- Snow Indicator",
        "    No,                      !- Daylight Saving Time Indicator",
        "    ASHRAEClearSky,          !- Solar Model Indicator",
        "    ,                        !- Beam Solar Day Schedule Name",
        "    ,                        !- Diffuse Solar Day Schedule Name",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensionless}",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimensionless}",
        "    0.0;                     !- Sky Clearness",

        "  RunPeriod,",
        "    ,                        !- Name",
        "    1,                       !- Begin Month",
        "    1,                       !- Begin Day of Month",
        "    ,                        !- Begin Year",
        "    12,                      !- End Month",
        "    31,                      !- End Day of Month",
        "    ,                        !- End Year",
        "    Tuesday,                 !- Day of Week for Start Day",
        "    Yes,                     !- Use Weather File Holidays and Special Days",
        "    Yes,                     !- Use Weather File Daylight Saving Period",
        "    No,                      !- Apply Weekend Holiday Rule",
        "    Yes,                     !- Use Weather File Rain Indicators",
        "    Yes;                     !- Use Weather File Snow Indicators",

        "  Site:GroundTemperature:BuildingSurface,",
        "    23.0,                    !- January Ground Temperature {C}",
        "    23.0,                    !- February Ground Temperature {C}",
        "    23.0,                    !- March Ground Temperature {C}",
        "    23.0,                    !- April Ground Temperature {C}",
        "    23.0,                    !- May Ground Temperature {C}",
        "    23.0,                    !- June Ground Temperature {C}",
        "    23.0,                    !- July Ground Temperature {C}",
        "    23.0,                    !- August Ground Temperature {C}",
        "    23.0,                    !- September Ground Temperature {C}",
        "    23.0,                    !- October Ground Temperature {C}",
        "    23.0,                    !- November Ground Temperature {C}",
        "    23.0;                    !- December Ground Temperature {C}",

        "  ScheduleTypeLimits,",
        "    Temperature,             !- Name",
        "    -60,                     !- Lower Limit Value",
        "    200,                     !- Upper Limit Value",
        "    CONTINUOUS,              !- Numeric Type",
        "    Temperature;             !- Unit Type",

        "  ScheduleTypeLimits,",
        "    Control Type,            !- Name",
        "    0,                       !- Lower Limit Value",
        "    4,                       !- Upper Limit Value",
        "    DISCRETE;                !- Numeric Type",

        "  ScheduleTypeLimits,",
        "    Fraction,                !- Name",
        "    0.0,                     !- Lower Limit Value",
        "    1.0,                     !- Upper Limit Value",
        "    CONTINUOUS;              !- Numeric Type",

        "  Schedule:Compact,",
        "    HVACAvailSched,          !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,            !- Field 3",
        "    1.0;                     !- Field 4",

        "  Schedule:Compact,",
        "    Dual Heating Setpoints,  !- Name",
        "    Temperature,             !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,            !- Field 3",
        "    23.0;                    !- Field 4",

        "  Schedule:Compact,",
        "    Dual Cooling Setpoints,  !- Name",
        "    Temperature,             !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,            !- Field 3",
        "    23.0;                    !- Field 4",

        "  Schedule:Compact,",
        "    Dual Zone Control Type Sched,  !- Name",
        "    Control Type,            !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,            !- Field 3",
        "    4;                       !- Field 4",

        "  Material,",
        "    Gypsum Board,            !- Name",
        "    MediumSmooth,            !- Roughness",
        "    0.0127,                  !- Thickness {m}",
        "    0.160158849,             !- Conductivity {W/m-K}",
        "    800.923168698,           !- Density {kg/m3}",
        "    1087.84,                 !- Specific Heat {J/kg-K}",
        "    0.9,                     !- Thermal Absorptance",
        "    0.9,                     !- Solar Absorptance",
        "    1.0;                     !- Visible Absorptance",

        "  Material,",
        "    Gypsum Board Wall,       !- Name",
        "    MediumSmooth,            !- Roughness",
        "    0.0127,                  !- Thickness {m}",
        "    0.160158849,             !- Conductivity {W/m-K}",
        "    800.923168698,           !- Density {kg/m3}",
        "    1087.84,                 !- Specific Heat {J/kg-K}",
        "    1e-6,                    !- Thermal Absorptance",
        "    1e-6,                    !- Solar Absorptance",
        "    1.0;                     !- Visible Absorptance",

        "  Material,",
        "    R-19 Insulation,         !- Name",
        "    Rough,                   !- Roughness",
        "    0.88871384,              !- Thickness {m}",
        "    0.25745056,              !- Conductivity {W/m-K}",
        "    3.05091836,              !- Density {kg/m3}",
        "    794.96,                  !- Specific Heat {J/kg-K}",
        "    0.9,                     !- Thermal Absorptance",
        "    0.9,                     !- Solar Absorptance",
        "    1.0;                     !- Visible Absorptance",

        "  Material,",
        "    R-A Lot,                 !- Name",
        "    Rough,                   !- Roughness",
        "    1.25,                    !- Thickness {m}",
        "    0.001,                   !- Conductivity {W/m-K}",
        "    3.05091836,              !- Density {kg/m3}",
        "    794.96,                  !- Specific Heat {J/kg-K}",
        "    0.9,                     !- Thermal Absorptance",
        "    0.9,                     !- Solar Absorptance",
        "    1.0;                     !- Visible Absorptance",

        "  Material,",
        "    Shingles,                !- Name",
        "    Rough,                   !- Roughness",
        "    0.006348984,             !- Thickness {m}",
        "    0.081932979,             !- Conductivity {W/m-K}",
        "    1121.292436177,          !- Density {kg/m3}",
        "    1256.04,                 !- Specific Heat {J/kg-K}",
        "    0.9,                     !- Thermal Absorptance",
        "    0.9,                     !- Solar Absorptance",
        "    1.0;                     !- Visible Absorptance",

        "  Material,",
        "    Felt,                    !- Name",
        "    Rough,                   !- Roughness",
        "    0.00216408,              !- Thickness {m}",
        "    0.081932979,             !- Conductivity {W/m-K}",
        "    1121.292436177,          !- Density {kg/m3}",
        "    1507.248,                !- Specific Heat {J/kg-K}",
        "    0.9,                     !- Thermal Absorptance",
        "    0.9,                     !- Solar Absorptance",
        "    1.0;                     !- Visible Absorptance",

        "  Material,",
        "    Plywood,                 !- Name",
        "    Rough,                   !- Roughness",
        "    0.012701016,             !- Thickness {m}",
        "    0.11544,                 !- Conductivity {W/m-K}",
        "    544.627754714,           !- Density {kg/m3}",
        "    1214.172,                !- Specific Heat {J/kg-K}",
        "    0.9,                     !- Thermal Absorptance",
        "    0.9,                     !- Solar Absorptance",
        "    1.0;                     !- Visible Absorptance",

        "  Material,",
        "    Hardboard Siding-Gable,  !- Name",
        "    MediumSmooth,            !- Roughness",
        "    0.0111125,               !- Thickness {m}",
        "    0.214957246,             !- Conductivity {W/m-K}",
        "    640.736,                 !- Density {kg/m3}",
        "    1172.304,                !- Specific Heat {J/kg-K}",
        "    0.90,                    !- Thermal Absorptance",
        "    0.7,                     !- Solar Absorptance",
        "    1.0;                     !- Visible Absorptance",

        "  Material,",
        "    Studs,                   !- Name",
        "    Rough,                   !- Roughness",
        "    0.0003137,               !- Thickness {m}",
        "    0.02189835,              !- Conductivity {W/m-K}",
        "    448.516974471,           !- Density {kg/m3}",
        "    1632.852,                !- Specific Heat {J/kg-K}",
        "    0.90,                    !- Thermal Absorptance",
        "    0.7,                     !- Solar Absorptance",
        "    1.0;                     !- Visible Absorptance",

        "  Material,",
        "    Hardboard Siding-Eave,   !- Name",
        "    MediumSmooth,            !- Roughness",
        "    0.0111125,               !- Thickness {m}",
        "    0.214957246,             !- Conductivity {W/m-K}",
        "    640.736,                 !- Density {kg/m3}",
        "    1172.304,                !- Specific Heat {J/kg-K}",
        "    0.90,                    !- Thermal Absorptance",
        "    0.7,                     !- Solar Absorptance",
        "    1.0;                     !- Visible Absorptance",

        "  Material,",
        "    HF-C5,                   !- Name",
        "    MediumRough,             !- Roughness",
        "    0.1015000,               !- Thickness {m}",
        "    1.729600,                !- Conductivity {W/m-K}",
        "    2243.000,                !- Density {kg/m3}",
        "    837.0000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.6500000,               !- Solar Absorptance",
        "    1.0;                     !- Visible Absorptance",

        "  Construction,",
        "    CeilingConstruction,     !- Name",
        "    R-19 Insulation,         !- Outside Layer",
        "    Gypsum Board;            !- Layer 2",

        "  Construction,",
        "    Reverse:CeilingConstruction,  !- Name",
        "    Gypsum Board,            !- Outside Layer",
        "    R-19 Insulation;         !- Layer 2",

        "  Construction,",
        "    Roof,                    !- Name",
        "    Shingles,                !- Outside Layer",
        "    Felt,                    !- Layer 2",
        "    Plywood;                 !- Layer 3",

        "  Construction,",
        "    Gables,                  !- Name",
        "    Hardboard Siding-Eave;   !- Outside Layer",

        "  Construction,",
        "    Eave Walls,              !- Name",
        "    Hardboard Siding-Eave;   !- Outside Layer",

        "  Construction,",
        "    Walls,                   !- Name",
        "    Hardboard Siding-Eave,   !- Outside Layer",
        "    R-A Lot,                 !- Layer 2",
        "    Gypsum Board Wall;       !- Layer 3",

        "  Construction,",
        "    LTFLOOR,                 !- Name",
        "    HF-C5;                   !- Outside Layer",

        "  GlobalGeometryRules,",
        "    UpperLeftCorner,         !- Starting Vertex Position",
        "    Counterclockwise,        !- Vertex Entry Direction",
        "    World;                   !- Coordinate System",

        "  Zone,",
        "    OCCUPIED ZONE,           !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    autocalculate,           !- Ceiling Height {m}",
        "    autocalculate;           !- Volume {m3}",

        "  Zone,",
        "    ATTIC ZONE,              !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    autocalculate,           !- Ceiling Height {m}",
        "    autocalculate;           !- Volume {m3}",

        "  BuildingSurface:Detailed,",
        "    North Wall,              !- Name",
        "    Wall,                    !- Surface Type",
        "    Walls,                   !- Construction Name",
        "    OCCUPIED ZONE,           !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.50,                    !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    16.764,                  !- Vertex 1 X-coordinate {m}",
        "    8.5344,                  !- Vertex 1 Y-coordinate {m}",
        "    2.70,                    !- Vertex 1 Z-coordinate {m}",
        "    16.764,                  !- Vertex 2 X-coordinate {m}",
        "    8.5344,                  !- Vertex 2 Y-coordinate {m}",
        "    0,                       !- Vertex 2 Z-coordinate {m}",
        "    0,                       !- Vertex 3 X-coordinate {m}",
        "    8.5344,                  !- Vertex 3 Y-coordinate {m}",
        "    0,                       !- Vertex 3 Z-coordinate {m}",
        "    0,                       !- Vertex 4 X-coordinate {m}",
        "    8.5344,                  !- Vertex 4 Y-coordinate {m}",
        "    2.70;                    !- Vertex 4 Z-coordinate {m}",

        "  BuildingSurface:Detailed,",
        "    North Wall Attic,        !- Name",
        "    Wall,                    !- Surface Type",
        "    Eave Walls,              !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.50,                    !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    16.764,                  !- Vertex 1 X-coordinate {m}",
        "    8.5344,                  !- Vertex 1 Y-coordinate {m}",
        "    2.7254,                  !- Vertex 1 Z-coordinate {m}",
        "    16.764,                  !- Vertex 2 X-coordinate {m}",
        "    8.5344,                  !- Vertex 2 Y-coordinate {m}",
        "    2.70,                    !- Vertex 2 Z-coordinate {m}",
        "    0,                       !- Vertex 3 X-coordinate {m}",
        "    8.5344,                  !- Vertex 3 Y-coordinate {m}",
        "    2.70,                    !- Vertex 3 Z-coordinate {m}",
        "    0,                       !- Vertex 4 X-coordinate {m}",
        "    8.5344,                  !- Vertex 4 Y-coordinate {m}",
        "    2.7254;                  !- Vertex 4 Z-coordinate {m}",

        "  BuildingSurface:Detailed,",
        "    East Wall,               !- Name",
        "    Wall,                    !- Surface Type",
        "    Walls,                   !- Construction Name",
        "    OCCUPIED ZONE,           !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.50,                    !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    16.764,                  !- Vertex 1 X-coordinate {m}",
        "    0,                       !- Vertex 1 Y-coordinate {m}",
        "    2.70,                    !- Vertex 1 Z-coordinate {m}",
        "    16.764,                  !- Vertex 2 X-coordinate {m}",
        "    0,                       !- Vertex 2 Y-coordinate {m}",
        "    0,                       !- Vertex 2 Z-coordinate {m}",
        "    16.764,                  !- Vertex 3 X-coordinate {m}",
        "    8.5344,                  !- Vertex 3 Y-coordinate {m}",
        "    0,                       !- Vertex 3 Z-coordinate {m}",
        "    16.764,                  !- Vertex 4 X-coordinate {m}",
        "    8.5344,                  !- Vertex 4 Y-coordinate {m}",
        "    2.70;                    !- Vertex 4 Z-coordinate {m}",

        "  BuildingSurface:Detailed,",
        "    East Wall Attic,         !- Name",
        "    Wall,                    !- Surface Type",
        "    Gables,              !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.50,                    !- View Factor to Ground",
        "    5,                       !- Number of Vertices",
        "    16.764,                  !- Vertex 1 X-coordinate {m}",
        "    0,                       !- Vertex 1 Y-coordinate {m}",
        "    2.7254,                  !- Vertex 1 Z-coordinate {m}",
        "    16.764,                  !- Vertex 2 X-coordinate {m}",
        "    0,                       !- Vertex 2 Y-coordinate {m}",
        "    2.70,                    !- Vertex 2 Z-coordinate {m}",
        "    16.764,                  !- Vertex 3 X-coordinate {m}",
        "    8.5344,                  !- Vertex 3 Y-coordinate {m}",
        "    2.70,                    !- Vertex 3 Z-coordinate {m}",
        "    16.764,                  !- Vertex 4 X-coordinate {m}",
        "    8.5344,                  !- Vertex 4 Y-coordinate {m}",
        "    2.7254,                  !- Vertex 4 Z-coordinate {m}",
        "    16.764,                  !- Vertex 5 X-coordinate {m}",
        "    4.2672,                  !- Vertex 5 Y-coordinate {m}",
        "    4.5034;                  !- Vertex 5 Z-coordinate {m}",

        "  BuildingSurface:Detailed,",
        "    South Wall,              !- Name",
        "    Wall,                    !- Surface Type",
        "    Walls,                   !- Construction Name",
        "    OCCUPIED ZONE,           !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.50,                    !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,                       !- Vertex 1 X-coordinate {m}",
        "    0,                       !- Vertex 1 Y-coordinate {m}",
        "    2.70,                    !- Vertex 1 Z-coordinate {m}",
        "    0,                       !- Vertex 2 X-coordinate {m}",
        "    0,                       !- Vertex 2 Y-coordinate {m}",
        "    0,                       !- Vertex 2 Z-coordinate {m}",
        "    16.764,                  !- Vertex 3 X-coordinate {m}",
        "    0,                       !- Vertex 3 Y-coordinate {m}",
        "    0,                       !- Vertex 3 Z-coordinate {m}",
        "    16.764,                  !- Vertex 4 X-coordinate {m}",
        "    0,                       !- Vertex 4 Y-coordinate {m}",
        "    2.70;                    !- Vertex 4 Z-coordinate {m}",

        "  BuildingSurface:Detailed,",
        "    South Wall Attic,        !- Name",
        "    Wall,                    !- Surface Type",
        "    Eave Walls,              !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.50,                    !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,                       !- Vertex 1 X-coordinate {m}",
        "    0,                       !- Vertex 1 Y-coordinate {m}",
        "    2.7254,                  !- Vertex 1 Z-coordinate {m}",
        "    0,                       !- Vertex 2 X-coordinate {m}",
        "    0,                       !- Vertex 2 Y-coordinate {m}",
        "    2.70,                    !- Vertex 2 Z-coordinate {m}",
        "    16.764,                  !- Vertex 3 X-coordinate {m}",
        "    0,                       !- Vertex 3 Y-coordinate {m}",
        "    2.70,                    !- Vertex 3 Z-coordinate {m}",
        "    16.764,                  !- Vertex 4 X-coordinate {m}",
        "    0,                       !- Vertex 4 Y-coordinate {m}",
        "    2.7254;                  !- Vertex 4 Z-coordinate {m}",

        "  BuildingSurface:Detailed,",
        "    West Wall,               !- Name",
        "    Wall,                    !- Surface Type",
        "    Walls,                   !- Construction Name",
        "    OCCUPIED ZONE,           !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.50,                    !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,                       !- Vertex 1 X-coordinate {m}",
        "    8.5344,                  !- Vertex 1 Y-coordinate {m}",
        "    2.70,                    !- Vertex 1 Z-coordinate {m}",
        "    0,                       !- Vertex 2 X-coordinate {m}",
        "    8.5344,                  !- Vertex 2 Y-coordinate {m}",
        "    0,                       !- Vertex 2 Z-coordinate {m}",
        "    0,                       !- Vertex 3 X-coordinate {m}",
        "    0,                       !- Vertex 3 Y-coordinate {m}",
        "    0,                       !- Vertex 3 Z-coordinate {m}",
        "    0,                       !- Vertex 4 X-coordinate {m}",
        "    0,                       !- Vertex 4 Y-coordinate {m}",
        "    2.70;                    !- Vertex 4 Z-coordinate {m}",

        "  BuildingSurface:Detailed,",
        "    West Wall Attic,         !- Name",
        "    Wall,                    !- Surface Type",
        "    Gables,              !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.50,                    !- View Factor to Ground",
        "    5,                       !- Number of Vertices",
        "    0,                       !- Vertex 1 X-coordinate {m}",
        "    8.5344,                  !- Vertex 1 Y-coordinate {m}",
        "    2.7254,                  !- Vertex 1 Z-coordinate {m}",
        "    0,                       !- Vertex 2 X-coordinate {m}",
        "    8.5344,                  !- Vertex 2 Y-coordinate {m}",
        "    2.70,                    !- Vertex 2 Z-coordinate {m}",
        "    0,                       !- Vertex 3 X-coordinate {m}",
        "    0,                       !- Vertex 3 Y-coordinate {m}",
        "    2.70,                    !- Vertex 3 Z-coordinate {m}",
        "    0,                       !- Vertex 4 X-coordinate {m}",
        "    0,                       !- Vertex 4 Y-coordinate {m}",
        "    2.7254,                  !- Vertex 4 Z-coordinate {m}",
        "    0,                       !- Vertex 5 X-coordinate {m}",
        "    4.2672,                  !- Vertex 5 Y-coordinate {m}",
        "    4.5034;                  !- Vertex 5 Z-coordinate {m}",

        "  BuildingSurface:Detailed,",
        "    Zone Floor,              !- Name",
        "    Floor,                   !- Surface Type",
        "    LTFLOOR,                 !- Construction Name",
        "    OCCUPIED ZONE,           !- Zone Name",
        "    Ground,                  !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,                       !- Vertex 1 X-coordinate {m}",
        "    0,                       !- Vertex 1 Y-coordinate {m}",
        "    0,                       !- Vertex 1 Z-coordinate {m}",
        "    0,                       !- Vertex 2 X-coordinate {m}",
        "    8.5344,                  !- Vertex 2 Y-coordinate {m}",
        "    0,                       !- Vertex 2 Z-coordinate {m}",
        "    16.764,                  !- Vertex 3 X-coordinate {m}",
        "    8.5344,                  !- Vertex 3 Y-coordinate {m}",
        "    0,                       !- Vertex 3 Z-coordinate {m}",
        "    16.764,                  !- Vertex 4 X-coordinate {m}",
        "    0,                       !- Vertex 4 Y-coordinate {m}",
        "    0;                       !- Vertex 4 Z-coordinate {m}",

        "  BuildingSurface:Detailed,",
        "    Zone Ceiling,            !- Name",
        "    Ceiling,                 !- Surface Type",
        "    CeilingConstruction,     !- Construction Name",
        "    OCCUPIED ZONE,           !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Attic Floor,             !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,                       !- Vertex 1 X-coordinate {m}",
        "    8.5344,                  !- Vertex 1 Y-coordinate {m}",
        "    2.70,                    !- Vertex 1 Z-coordinate {m}",
        "    0,                       !- Vertex 2 X-coordinate {m}",
        "    0,                       !- Vertex 2 Y-coordinate {m}",
        "    2.70,                    !- Vertex 2 Z-coordinate {m}",
        "    16.764,                  !- Vertex 3 X-coordinate {m}",
        "    0,                       !- Vertex 3 Y-coordinate {m}",
        "    2.70,                    !- Vertex 3 Z-coordinate {m}",
        "    16.764,                  !- Vertex 4 X-coordinate {m}",
        "    8.5344,                  !- Vertex 4 Y-coordinate {m}",
        "    2.70;                    !- Vertex 4 Z-coordinate {m}",

        "  BuildingSurface:Detailed,",
        "    Attic Floor,             !- Name",
        "    Floor,                   !- Surface Type",
        "    Reverse:CeilingConstruction,  !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zone Ceiling,            !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    16.764,                  !- Vertex 1 X-coordinate {m}",
        "    8.5344,                  !- Vertex 1 Y-coordinate {m}",
        "    2.70,                    !- Vertex 1 Z-coordinate {m}",
        "    16.764,                  !- Vertex 2 X-coordinate {m}",
        "    0,                       !- Vertex 2 Y-coordinate {m}",
        "    2.70,                    !- Vertex 2 Z-coordinate {m}",
        "    0,                       !- Vertex 3 X-coordinate {m}",
        "    0,                       !- Vertex 3 Y-coordinate {m}",
        "    2.70,                    !- Vertex 3 Z-coordinate {m}",
        "    0,                       !- Vertex 4 X-coordinate {m}",
        "    8.5344,                  !- Vertex 4 Y-coordinate {m}",
        "    2.70;                    !- Vertex 4 Z-coordinate {m}",

        "  BuildingSurface:Detailed,",
        "    Attic Roof South,        !- Name",
        "    Roof,                    !- Surface Type",
        "    Roof,                    !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,                       !- Vertex 1 X-coordinate {m}",
        "    4.2672,                  !- Vertex 1 Y-coordinate {m}",
        "    4.5034,                  !- Vertex 1 Z-coordinate {m}",
        "    0,                       !- Vertex 2 X-coordinate {m}",
        "    0,                       !- Vertex 2 Y-coordinate {m}",
        "    2.7254,                  !- Vertex 2 Z-coordinate {m}",
        "    16.764,                  !- Vertex 3 X-coordinate {m}",
        "    0,                       !- Vertex 3 Y-coordinate {m}",
        "    2.7254,                  !- Vertex 3 Z-coordinate {m}",
        "    16.764,                  !- Vertex 4 X-coordinate {m}",
        "    4.2672,                  !- Vertex 4 Y-coordinate {m}",
        "    4.5034;                  !- Vertex 4 Z-coordinate {m}",

        "  BuildingSurface:Detailed,",
        "    Attic Roof North,        !- Name",
        "    Roof,                    !- Surface Type",
        "    Roof,                    !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    16.764,                  !- Vertex 1 X-coordinate {m}",
        "    4.2672,                  !- Vertex 1 Y-coordinate {m}",
        "    4.5034,                  !- Vertex 1 Z-coordinate {m}",
        "    16.764,                  !- Vertex 2 X-coordinate {m}",
        "    8.5344,                  !- Vertex 2 Y-coordinate {m}",
        "    2.7254,                  !- Vertex 2 Z-coordinate {m}",
        "    0,                       !- Vertex 3 X-coordinate {m}",
        "    8.5344,                  !- Vertex 3 Y-coordinate {m}",
        "    2.7254,                  !- Vertex 3 Z-coordinate {m}",
        "    0,                       !- Vertex 4 X-coordinate {m}",
        "    4.2672,                  !- Vertex 4 Y-coordinate {m}",
        "    4.5034;                  !- Vertex 4 Z-coordinate {m}",

        "  ZoneProperty:UserViewFactors:bySurfaceName,",
        "    ATTIC ZONE,              !- Zone Name",
        "    Attic Floor,		!=From Surface 1",
        "    Attic Floor,		!=To Surface 1",
        "    0.000000,",
        "    Attic Floor,		!=From Surface 1",
        "    Attic Roof South,		!=To Surface 2",
        "    0.476288,",
        "    Attic Floor,		!=From Surface 1",
        "    Attic Roof North,		!=To Surface 3",
        "    0.476288,",
        "    Attic Floor,		!=From Surface 1",
        "    East Wall Attic,		!=To Surface 4",
        "    0.023712,",
        "    Attic Floor,		!=From Surface 1",
        "    West Wall Attic,		!=To Surface 5",
        "    0.023712,",
        "    Attic Floor,		!=From Surface 1",
        "    North Wall Attic,		!=To Surface 6",
        "    0.000000,",
        "    Attic Floor,		!=From Surface 1",
        "    South Wall Attic,		!=To Surface 7",
        "    0.000000,",
        "    Attic Roof South,		!=From Surface 2",
        "    Attic Floor,		!=To Surface 1",
        "    0.879300,",
        "    Attic Roof South,		!=From Surface 2",
        "    Attic Roof South,		!=To Surface 2",
        "    0.000000,",
        "    Attic Roof South,		!=From Surface 2",
        "    Attic Roof North,		!=To Surface 3",
        "    0.067378,",
        "    Attic Roof South,		!=From Surface 2",
        "    East Wall Attic,		!=To Surface 4",
        "    0.026661,",
        "    Attic Roof South,		!=From Surface 2",
        "    West Wall Attic,		!=To Surface 5",
        "    0.026661,",
        "    Attic Roof South,		!=From Surface 2",
        "    North Wall Attic,		!=To Surface 6",
        "    0.000000,",
        "    Attic Roof South,		!=From Surface 2",
        "    South Wall Attic,		!=To Surface 7",
        "    0.000000,",
        "    Attic Roof North,		!=From Surface 3",
        "    Attic Floor,		!=To Surface 1",
        "    0.879300,",
        "    Attic Roof North,		!=From Surface 3",
        "    Attic Roof South,		!=To Surface 2",
        "    0.067378,",
        "    Attic Roof North,		!=From Surface 3",
        "    Attic Roof North,		!=To Surface 3",
        "    0.000000,",
        "    Attic Roof North,		!=From Surface 3",
        "    East Wall Attic,		!=To Surface 4",
        "    0.026661,",
        "    Attic Roof North,		!=From Surface 3",
        "    West Wall Attic,		!=To Surface 5",
        "    0.026661,",
        "    Attic Roof North,		!=From Surface 3",
        "    North Wall Attic,		!=To Surface 6",
        "    0.000000,",
        "    Attic Roof North,		!=From Surface 3",
        "    South Wall Attic,		!=To Surface 7",
        "    0.000000,",
        "    East Wall Attic,		!=From Surface 4",
        "    Attic Floor,		!=To Surface 1",
        "    0.447134,",
        "    East Wall Attic,		!=From Surface 4",
        "    Attic Roof South,		!=To Surface 2",
        "    0.272318,",
        "    East Wall Attic,		!=From Surface 4",
        "    Attic Roof North,		!=To Surface 3",
        "    0.272318,",
        "    East Wall Attic,		!=From Surface 4",
        "    East Wall Attic,		!=To Surface 4",
        "    0.000000,",
        "    East Wall Attic,		!=From Surface 4",
        "    West Wall Attic,		!=To Surface 5",
        "    0.008231,",
        "    East Wall Attic,		!=From Surface 4",
        "    North Wall Attic,		!=To Surface 6",
        "    0.000000,",
        "    East Wall Attic,		!=From Surface 4",
        "    South Wall Attic,		!=To Surface 7",
        "    0.000000,",
        "    West Wall Attic,		!=From Surface 5",
        "    Attic Floor,		!=To Surface 1",
        "    0.447134,",
        "    West Wall Attic,		!=From Surface 5",
        "    Attic Roof South,		!=To Surface 2",
        "    0.272318,",
        "    West Wall Attic,		!=From Surface 5",
        "    Attic Roof North,		!=To Surface 3",
        "    0.272318,",
        "    West Wall Attic,		!=From Surface 5",
        "    East Wall Attic,		!=To Surface 4",
        "    0.008231,",
        "    West Wall Attic,		!=From Surface 5",
        "    West Wall Attic,		!=To Surface 5",
        "    0.000000,",
        "    West Wall Attic,		!=From Surface 5",
        "    North Wall Attic,		!=To Surface 6",
        "    0.000000,",
        "    West Wall Attic,		!=From Surface 5",
        "    South Wall Attic,		!=To Surface 7",
        "    0.000000,",
        "    North Wall Attic,		!=From Surface 6",
        "    Attic Floor,		!=To Surface 1",
        "    0.000000,",
        "    North Wall Attic,		!=From Surface 6",
        "    Attic Roof South,		!=To Surface 2",
        "    0.000000,",
        "    North Wall Attic,		!=From Surface 6",
        "    Attic Roof North,		!=To Surface 3",
        "    0.000000,",
        "    North Wall Attic,		!=From Surface 6",
        "    East Wall Attic,		!=To Surface 4",
        "    0.000000,",
        "    North Wall Attic,		!=From Surface 6",
        "    West Wall Attic,		!=To Surface 5",
        "    0.000000,",
        "    North Wall Attic,		!=From Surface 6",
        "    North Wall Attic,		!=To Surface 6",
        "    1.000000,",
        "    North Wall Attic,		!=From Surface 6",
        "    South Wall Attic,		!=To Surface 7",
        "    0.000000,",
        "    South Wall Attic,		!=From Surface 7",
        "    Attic Floor,		!=To Surface 1",
        "    0.000000,",
        "    South Wall Attic,		!=From Surface 7",
        "    Attic Roof South,		!=To Surface 2",
        "    0.000000,",
        "    South Wall Attic,		!=From Surface 7",
        "    Attic Roof North,		!=To Surface 3",
        "    0.000000,",
        "    South Wall Attic,		!=From Surface 7",
        "    East Wall Attic,		!=To Surface 4",
        "    0.000000,",
        "    South Wall Attic,		!=From Surface 7",
        "    West Wall Attic,		!=To Surface 5",
        "    0.000000,",
        "    South Wall Attic,		!=From Surface 7",
        "    North Wall Attic,		!=To Surface 6",
        "    0.000000,",
        "    South Wall Attic,		!=From Surface 7",
        "    South Wall Attic,		!=To Surface 7",
        "    1.000000;",

        "  AirflowNetwork:SimulationControl,",
        "    House AirflowNetwork,    !- Name",
        "    MultizoneWithDistribution,  !- AirflowNetwork Control",
        "    SurfaceAverageCalculation,  !- Wind Pressure Coefficient Type",
        "    ,                        !- Height Selection for Local Wind Pressure Calculation",
        "    LOWRISE,                 !- Building Type",
        "    500,                     !- Maximum Number of Iterations {dimensionless}",
        "    ZeroNodePressures,       !- Initialization Type",
        "    1.0E-04,                 !- Relative Airflow Convergence Tolerance {dimensionless}",
        "    1.0E-06,                 !- Absolute Airflow Convergence Tolerance {kg/s}",
        "    -0.5,                    !- Convergence Acceleration Limit {dimensionless}",
        "    0.0,                     !- Azimuth Angle of Long Axis of Building {deg}",
        "    1.0;                     !- Ratio of Building Width Along Short Axis to Width Along Long Axis",

        "  AirflowNetwork:MultiZone:Zone,",
        "    OCCUPIED ZONE,           !- Zone Name",
        "    NOVENT,                  !- Ventilation Control Mode",
        "    ,                        !- Ventilation Control Zone Temperature Setpoint Schedule Name",
        "    1.0,                     !- Minimum Venting Open Factor {dimensionless}",
        "    0.0,                     !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor {deltaC}",
        "    100.0,                   !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor {deltaC}",
        "    0.0,                     !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor {deltaJ/kg}",
        "    300000.0;                !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor {deltaJ/kg}",

        "  AirflowNetwork:MultiZone:Zone,",
        "    ATTIC ZONE,              !- Zone Name",
        "    NoVent,                  !- Ventilation Control Mode",
        "    ,                        !- Ventilation Control Zone Temperature Setpoint Schedule Name",
        "    1.0,                     !- Minimum Venting Open Factor {dimensionless}",
        "    0.0,                     !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor {deltaC}",
        "    100.0,                   !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor {deltaC}",
        "    0.0,                     !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor {deltaJ/kg}",
        "    300000.0;                !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor {deltaJ/kg}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    North Wall Attic,        !- Surface Name",
        "    NorthEaveLeak,           !- Leakage Component Name",
        "    ,                        !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    South Wall Attic,        !- Surface Name",
        "    SouthEaveLeak,           !- Leakage Component Name",
        "    ,                        !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    East Wall,               !- Surface Name",
        "    EastLeak,                !- Leakage Component Name",
        "    ,                        !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    West Wall,               !- Surface Name",
        "    WestLeak,                !- Leakage Component Name",
        "    ,                        !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:ReferenceCrackConditions,",
        "    ReferenceCrackConditions,!- Name",
        "    20.0,                    !- Reference Temperature {C}",
        "    101325,                  !- Reference Barometric Pressure {Pa}",
        "    0.0;                     !- Reference Humidity Ratio {kgWater/kgDryAir}",

        "  AirflowNetwork:MultiZone:Surface:Crack,",
        "    NorthEaveLeak,           !- Name",
        "    0.2,                     !- Air Mass Flow Coefficient at Reference Conditions {kg/s}",
        "    0.65,                    !- Air Mass Flow Exponent {dimensionless}",
        "    ReferenceCrackConditions;!- Reference Crack Conditions",

        "  AirflowNetwork:MultiZone:Surface:Crack,",
        "    SouthEaveLeak,           !- Name",
        "    0.2,                     !- Air Mass Flow Coefficient at Reference Conditions {kg/s}",
        "    0.65,                    !- Air Mass Flow Exponent {dimensionless}",
        "    ReferenceCrackConditions;!- Reference Crack Conditions",

        "  AirflowNetwork:MultiZone:Surface:Crack,",
        "    EastLeak,                !- Name",
        "    0.05,                    !- Air Mass Flow Coefficient at Reference Conditions {kg/s}",
        "    0.65,                    !- Air Mass Flow Exponent {dimensionless}",
        "    ReferenceCrackConditions;!- Reference Crack Conditions",

        "  AirflowNetwork:MultiZone:Surface:Crack,",
        "    WestLeak,                !- Name",
        "    0.03,                    !- Air Mass Flow Coefficient at Reference Conditions {kg/s}",
        "    0.65,                    !- Air Mass Flow Exponent {dimensionless}",
        "    ReferenceCrackConditions;!- Reference Crack Conditions",

        "  AirflowNetwork:Distribution:Node,",
        "    EquipmentInletNode,      !- Name",
        "    Zone Equipment Inlet Node,  !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    SplitterNode,            !- Name",
        "    ,                        !- Component Name or Node Name",
        "    AirLoopHVAC:ZoneSplitter,!- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    ZoneSupplyNode,          !- Name",
        "    ,                        !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    ZoneSupplyRegisterNode,  !- Name",
        "    Zone Inlet Node,         !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    ZoneOutletNode,          !- Name",
        "    Zone Outlet Node,        !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    ZoneReturnNode,          !- Name",
        "    ,                        !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    MixerNode,               !- Name",
        "    ,                        !- Component Name or Node Name",
        "    AirLoopHVAC:ZoneMixer,   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    MainReturnNode,          !- Name",
        "    Return Air Mixer Outlet, !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    MainInletNode,           !- Name",
        "    Air Loop Inlet Node,     !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    FanInletNode,            !- Name",
        "    Air Loop Inlet Node,     !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    FanOutletNode,           !- Name",
        "    Cooling Coil Air Inlet Node,  !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    HeatingInletNode,        !- Name",
        "    Heating Coil Air Inlet Node,  !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    HeatingOutletNode,       !- Name",
        "    Air Loop Outlet Node,    !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Component:Duct,",
        "    MainTrunk,               !- Name",
        "    2.0,                     !- Duct Length {m}",
        "    0.4064,                  !- Hydraulic Diameter {m}",
        "    0.1297,                  !- Cross Section Area {m2}",
        "    0.0009,                  !- Surface Roughness {m}",
        "    0.01,                    !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
        "    0.7139,                  !- Heat Transmittance Coefficient (U-Factor) for Duct Construction {W/m2-K}",
        "    0.0000001;               !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",

        "  AirflowNetwork:Distribution:Component:Duct,",
        "    ZoneSupply,              !- Name",
        "    16.76,                   !- Duct Length {m}",
        "    0.3048,                  !- Hydraulic Diameter {m}",
        "    0.073205,                !- Cross Section Area {m2}",
        "    0.0009,                  !- Surface Roughness {m}",
        "    0.91,                    !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
        "    0.00613207547169811,     !- Overall Heat Transmittance Coefficient (U-Factor) from Air to Air {W/m2-K}",
        "    0.0000001,               !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",
        "    0.0325,                  !- Outside Convection Coefficient {W/m2-K}",
        "    0.1625;                  !- Inside Convection Coefficient {W/m2-K}",

        "  AirflowNetwork:Distribution:Component:Duct,",
        "    ZoneReturn,              !- Name",
        "    3.0,                     !- Duct Length {m}",
        "    0.50,                    !- Hydraulic Diameter {m}",
        "    0.1963,                  !- Cross Section Area {m2}",
        "    0.0009,                  !- Surface Roughness {m}",
        "    0.01,                    !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
        "    0.00122641509433962,     !- Overall Heat Transmittance Coefficient (U-Factor) from Air to Air {W/m2-K}",
        "    0.0000001,               !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",
        "    0.0065,                  !- Outside Convection Coefficient {W/m2-K}",
        "    0.0325;                  !- Inside Convection Coefficient {W/m2-K}",

        "  AirflowNetwork:Distribution:Component:Duct,",
        "    ZoneConnectionDuct,      !- Name",
        "    0.1,                     !- Duct Length {m}",
        "    1.0,                     !- Hydraulic Diameter {m}",
        "    0.7854,                  !- Cross Section Area {m2}",
        "    0.0001,                  !- Surface Roughness {m}",
        "    0.00,                    !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
        "    0.00122641509433962,     !- Overall Heat Transmittance Coefficient (U-Factor) from Air to Air {W/m2-K}",
        "    0.0000001,               !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",
        "    0.0065,                  !- Outside Convection Coefficient {W/m2-K}",
        "    0.0325;                  !- Inside Convection Coefficient {W/m2-K}",

        "  AirflowNetwork:Distribution:Component:Duct,",
        "    MainReturn,              !- Name",
        "    1.0,                     !- Duct Length {m}",
        "    0.50,                    !- Hydraulic Diameter {m}",
        "    0.1963,                  !- Cross Section Area {m2}",
        "    0.0009,                  !- Surface Roughness {m}",
        "    0.01,                    !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
        "    0.00122641509433962,     !- Overall Heat Transmittance Coefficient (U-Factor) from Air to Air {W/m2-K}",
        "    0.0000001,               !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",
        "    0.0065,                  !- Outside Convection Coefficient {W/m2-K}",
        "    0.0325;                  !- Inside Convection Coefficient {W/m2-K}",

        "  AirflowNetwork:Distribution:Component:Duct,",
        "    AirLoopReturn,           !- Name",
        "    0.1,                     !- Duct Length {m}",
        "    1.00,                    !- Hydraulic Diameter {m}",
        "    0.7854,                  !- Cross Section Area {m2}",
        "    0.0001,                  !- Surface Roughness {m}",
        "    0.00,                    !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
        "    0.00122641509433962,     !- Overall Heat Transmittance Coefficient (U-Factor) from Air to Air {W/m2-K}",
        "    0.0000001,               !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",
        "    0.0065,                  !- Outside Convection Coefficient {W/m2-K}",
        "    0.0325;                  !- Inside Convection Coefficient {W/m2-K}",

        "  AirflowNetwork:Distribution:Component:Duct,",
        "    AirLoopSupply,           !- Name",
        "    0.1,                     !- Duct Length {m}",
        "    1.00,                    !- Hydraulic Diameter {m}",
        "    0.7854,                  !- Cross Section Area {m2}",
        "    0.0001,                  !- Surface Roughness {m}",
        "    0.00,                    !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
        "    0.00122641509433962,     !- Overall Heat Transmittance Coefficient (U-Factor) from Air to Air {W/m2-K}",
        "    0.0000001,               !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",
        "    0.0065,                  !- Outside Convection Coefficient {W/m2-K}",
        "    0.0325;                  !- Inside Convection Coefficient {W/m2-K}",

        "  AirflowNetwork:Distribution:Component:Fan,",
        "    Supply Fan 1,            !- Fan Name",
        "    Fan:ConstantVolume;      !- Supply Fan Object Type",

        "  AirflowNetwork:Distribution:Component:Coil,",
        "    ACDXCoil 1,              !- Coil Name",
        "    Coil:Cooling:DX:SingleSpeed,  !- Coil Object Type",
        "    0.1,                     !- Air Path Length {m}",
        "    1.000;                   !- Air Path Hydraulic Diameter {m}",

        "  AirflowNetwork:Distribution:Component:Coil,",
        "    Main Heating Coil 1,     !- Coil Name",
        "    Coil:Heating:Fuel,       !- Coil Object Type",
        "    0.1,                     !- Air Path Length {m}",
        "    1.000;                   !- Air Path Hydraulic Diameter {m}",

        "  AirflowNetwork:Distribution:DuctViewFactors,",
        "    ZoneSupplyLink1,         !- Name of Linkage",
        "    1.0,                     !- Surface Exposure Fraction",
        "    0.9,                     !- Duct surface emittance",
        "    Attic Floor,             !- Surface 1",
        "    0.483577,                !- View Factor for Surface 1",
        "    Attic Roof North,        !- Surface 2",
        "    0.237692,                !- View Factor for Surface 2",
        "    Attic Roof South,        !- Surface 3",
        "    0.237692,                !- View Factor for Surface 3",
        "    East Wall Attic,         !- Surface 4",
        "    0.02052,                 !- View Factor for Surface 4",
        "    West Wall Attic,         !- Surface 5",
        "    0.02052;                 !- View Factor for Surface 5",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Main Link,               !- Name",
        "    EquipmentInletNode,      !- Node 1 Name",
        "    SplitterNode,            !- Node 2 Name",
        "    MainTrunk,               !- Component Name",
        "    Attic Zone;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    ZoneSupplyLink1,         !- Name",
        "    SplitterNode,            !- Node 1 Name",
        "    ZoneSupplyNode,          !- Node 2 Name",
        "    ZoneSupply,              !- Component Name",
        "    Attic Zone;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    ZoneSupply1Link2,        !- Name",
        "    ZoneSupplyNode,          !- Node 1 Name",
        "    ZoneSupplyRegisterNode,  !- Node 2 Name",
        "    ZoneSupply,              !- Component Name",
        "    Attic Zone;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    ZoneSupplyConnectionLink,!- Name",
        "    ZoneSupplyRegisterNode,  !- Node 1 Name",
        "    OCCUPIED ZONE,           !- Node 2 Name",
        "    ZoneConnectionDuct;      !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    ZoneReturnConnectionLink,!- Name",
        "    OCCUPIED ZONE,           !- Node 1 Name",
        "    ZoneOutletNode,          !- Node 2 Name",
        "    ZoneConnectionDuct;      !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    ZoneReturn1Link,         !- Name",
        "    ZoneOutletNode,          !- Node 1 Name",
        "    ZoneReturnNode,          !- Node 2 Name",
        "    ZoneReturn,              !- Component Name",
        "    Attic Zone;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    ZoneReturn2Link,         !- Name",
        "    ZoneReturnNode,          !- Node 1 Name",
        "    MixerNode,               !- Node 2 Name",
        "    ZoneReturn,              !- Component Name",
        "    OCCUPIED Zone;           !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    ReturnMixerLink,         !- Name",
        "    MixerNode,               !- Node 1 Name",
        "    MainReturnNode,          !- Node 2 Name",
        "    MainReturn,              !- Component Name",
        "    OCCUPIED Zone;           !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    SystemReturnLink,        !- Name",
        "    MainReturnNode,          !- Node 1 Name",
        "    MainInletNode,           !- Node 2 Name",
        "    AirLoopReturn;           !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    SystemInletLink,         !- Name",
        "    MainInletNode,           !- Node 1 Name",
        "    FanInletNode,            !- Node 2 Name",
        "    MainReturn,              !- Component Name",
        "    OCCUPIED ZONE;           !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    SupplyFanLink,           !- Name",
        "    FanInletNode,            !- Node 1 Name",
        "    FanOutletNode,           !- Node 2 Name",
        "    Supply Fan 1;            !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    CoolingCoilLink,         !- Name",
        "    FanOutletNode,           !- Node 1 Name",
        "    HeatingInletNode,        !- Node 2 Name",
        "    ACDXCoil 1;              !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    HeatingCoilLink,         !- Name",
        "    HeatingInletNode,        !- Node 1 Name",
        "    HeatingOutletNode,       !- Node 2 Name",
        "    Main Heating Coil 1;     !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    EquipmentAirLoopLink,    !- Name",
        "    HeatingOutletNode,       !- Node 1 Name",
        "    EquipmentInletNode,      !- Node 2 Name",
        "    AirLoopSupply;           !- Component Name",

        "  ZoneControl:Thermostat,",
        "    Zone Thermostat,         !- Name",
        "    OCCUPIED ZONE,           !- Zone or ZoneList Name",
        "    Dual Zone Control Type Sched,  !- Control Type Schedule Name",
        "    ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
        "    Setpoints;               !- Control 1 Name",

        "  ThermostatSetpoint:DualSetpoint,",
        "    Setpoints,               !- Name",
        "    Dual Heating Setpoints,  !- Heating Setpoint Temperature Schedule Name",
        "    Dual Cooling Setpoints;  !- Cooling Setpoint Temperature Schedule Name",

        "  ZoneHVAC:AirDistributionUnit,",
        "    ZoneDirectAirADU,        !- Name",
        "    Zone1NoReheatAirOutletNode,  !- Air Distribution Unit Outlet Node Name",
        "    AirTerminal:SingleDuct:ConstantVolume:NoReheat,  !- Air Terminal Object Type",
        "    ZoneDirectAir;           !- Air Terminal Name",

        "  AirTerminal:SingleDuct:ConstantVolume:NoReheat,",
        "    ZoneDirectAir,           !- Name",
        "    HVACAvailSched,          !- Availability Schedule Name",
        "    Zone Inlet Node 2AT,     !- Air Inlet Node Name",
        "    Zone Inlet Node,         !- Zone Supply Air Node Name",
        "    2.36;                    !- Maximum Air Flow Rate {m3/s}",

        "  ZoneHVAC:EquipmentList,",
        "    ZoneEquipment,           !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        "    ZoneDirectAirADU,        !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",

        "  ZoneHVAC:EquipmentConnections,",
        "    OCCUPIED ZONE,           !- Zone Name",
        "    ZoneEquipment,           !- Zone Conditioning Equipment List Name",
        "    ZoneInlets,              !- Zone Air Inlet Node or NodeList Name",
        "    ,                        !- Zone Air Exhaust Node or NodeList Name",
        "    Zone Node,               !- Zone Air Node Name",
        "    Zone Outlet Node;        !- Zone Return Air Node Name",

        "  Fan:ConstantVolume,",
        "    Supply Fan 1,            !- Name",
        "    HVACAvailSched,          !- Availability Schedule Name",
        "    0.7,                     !- Fan Total Efficiency",
        "    400.0,                   !- Pressure Rise {Pa}",
        "    2.36,                    !- Maximum Flow Rate {m3/s}",
        "    0.9,                     !- Motor Efficiency",
        "    1.0,                     !- Motor In Airstream Fraction",
        "    Air Loop Inlet Node,     !- Air Inlet Node Name",
        "    Cooling Coil Air Inlet Node;  !- Air Outlet Node Name",

        "  Coil:Cooling:DX:SingleSpeed,",
        "    ACDXCoil 1,              !- Name",
        "    HVACAvailSched,          !- Availability Schedule Name",
        "    21000,                   !- Gross Rated Total Cooling Capacity {W}",
        "    0.8,                     !- Gross Rated Sensible Heat Ratio",
        "    3.0,                     !- Gross Rated Cooling COP {W/W}",
        "    2.36,                    !- Rated Air Flow Rate {m3/s}",
        "    ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "    Cooling Coil Air Inlet Node,  !- Air Inlet Node Name",
        "    Heating Coil Air Inlet Node,  !- Air Outlet Node Name",
        "    WindACCoolCapFT,         !- Total Cooling Capacity Function of Temperature Curve Name",
        "    WindACCoolCapFFF,        !- Total Cooling Capacity Function of Flow Fraction Curve Name",
        "    WindACEIRFT,             !- Energy Input Ratio Function of Temperature Curve Name",
        "    WindACEIRFFF,            !- Energy Input Ratio Function of Flow Fraction Curve Name",
        "    WindACPLFFPLR;           !- Part Load Fraction Correlation Curve Name",

        "  Coil:Heating:Fuel,",
        "    Main Heating Coil 1,     !- Name",
        "    HVACAvailSched,          !- Availability Schedule Name",
        "    NaturalGas,              !- Fuel Type",
        "    0.8,                     !- Burner Efficiency",
        "    8000000,                 !- Nominal Capacity {W}",
        "    Heating Coil Air Inlet Node,  !- Air Inlet Node Name",
        "    Air Loop Outlet Node,    !- Air Outlet Node Name",
        "    Air Loop Outlet Node;    !- Temperature Setpoint Node Name",

        "  CoilSystem:Cooling:DX,",
        "    DX Cooling Coil System 1,!- Name",
        "    HVACAvailSched,          !- Availability Schedule Name",
        "    Cooling Coil Air Inlet Node,  !- DX Cooling Coil System Inlet Node Name",
        "    Heating Coil Air Inlet Node,  !- DX Cooling Coil System Outlet Node Name",
        "    Heating Coil Air Inlet Node,  !- DX Cooling Coil System Sensor Node Name",
        "    Coil:Cooling:DX:SingleSpeed,  !- Cooling Coil Object Type",
        "    ACDXCoil 1;              !- Cooling Coil Name",

        "  AirLoopHVAC,",
        "    Typical Residential System,  !- Name",
        "    ,                        !- Controller List Name",
        "    Reheat System 1 Avail List,  !- Availability Manager List Name",
        "    2.36,                    !- Design Supply Air Flow Rate {m3/s}",
        "    Air Loop Branches,       !- Branch List Name",
        "    ,                        !- Connector List Name",
        "    Air Loop Inlet Node,     !- Supply Side Inlet Node Name",
        "    Return Air Mixer Outlet, !- Demand Side Outlet Node Name",
        "    Zone Equipment Inlet Node,  !- Demand Side Inlet Node Names",
        "    Air Loop Outlet Node;    !- Supply Side Outlet Node Names",

        "  AirLoopHVAC:ZoneSplitter,",
        "    Zone Supply Air Splitter,!- Name",
        "    Zone Equipment Inlet Node,  !- Inlet Node Name",
        "    Zone Inlet Node 2AT;      !- Outlet 1 Node Name",

        "  AirLoopHVAC:SupplyPath,",
        "    TermReheatSupplyPath,    !- Name",
        "    Zone Equipment Inlet Node,  !- Supply Air Path Inlet Node Name",
        "    AirLoopHVAC:ZoneSplitter,!- Component 1 Object Type",
        "    Zone Supply Air Splitter;!- Component 1 Name",

        "  AirLoopHVAC:ZoneMixer,",
        "    Zone Return Air Mixer,   !- Name",
        "    Return Air Mixer Outlet, !- Outlet Node Name",
        "    Zone Outlet Node;        !- Inlet 1 Node Name",

        "  AirLoopHVAC:ReturnPath,",
        "    TermReheatReturnPath,    !- Name",
        "    Return Air Mixer Outlet, !- Return Air Path Outlet Node Name",
        "    AirLoopHVAC:ZoneMixer,   !- Component 1 Object Type",
        "    Zone Return Air Mixer;   !- Component 1 Name",

        "  Branch,",
        "    Air Loop Main Branch,    !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Fan:ConstantVolume,      !- Component 1 Object Type",
        "    Supply Fan 1,            !- Component 1 Name",
        "    Air Loop Inlet Node,     !- Component 1 Inlet Node Name",
        "    Cooling Coil Air Inlet Node,  !- Component 1 Outlet Node Name",
        "    CoilSystem:Cooling:DX,   !- Component 2 Object Type",
        "    DX Cooling Coil System 1,!- Component 2 Name",
        "    Cooling Coil Air Inlet Node,  !- Component 2 Inlet Node Name",
        "    Heating Coil Air Inlet Node,  !- Component 2 Outlet Node Name",
        "    Coil:Heating:Fuel,       !- Component 3 Object Type",
        "    Main Heating Coil 1,     !- Component 3 Name",
        "    Heating Coil Air Inlet Node,  !- Component 3 Inlet Node Name",
        "    Air Loop Outlet Node;    !- Component 3 Outlet Node Name",

        "  BranchList,",
        "    Air Loop Branches,       !- Name",
        "    Air Loop Main Branch;    !- Branch 1 Name",

        "  NodeList,",
        "    ZoneInlets,              !- Name",
        "    Zone Inlet Node;         !- Node 1 Name",

        "  NodeList,",
        "    Supply Air Temp Nodes,   !- Name",
        "    Heating Coil Air Inlet Node,  !- Node 1 Name",
        "    Air Loop Outlet Node;    !- Node 2 Name",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound = false;
    // Read objects
    SimulationManager::GetProjectData();
    HeatBalanceManager::GetProjectControlData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetZoneData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetWindowGlassSpectralData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetMaterialData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetConstructData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetHeatBalanceInput();
    HeatBalanceManager::AllocateHeatBalArrays();
    DataEnvironment::OutBaroPress = 101000;
    DataHVACGlobals::TimeStepSys = DataGlobals::TimeStepZone;
    SurfaceGeometry::GetGeometryParameters(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    SurfaceGeometry::CosBldgRotAppGonly = 1.0;
    SurfaceGeometry::SinBldgRotAppGonly = 0.0;
    SurfaceGeometry::GetSurfaceData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    // Read AirflowNetwork inputs
    GetAirflowNetworkInput();
    InitAirflowNetwork();

    // Check inputs
    EXPECT_EQ(AirflowNetworkLinkageViewFactorData(1).LinkageName, "ZONESUPPLYLINK1");
    EXPECT_EQ(AirflowNetworkLinkageViewFactorData(1).DuctExposureFraction, 1.0);
    EXPECT_EQ(AirflowNetworkLinkageViewFactorData(1).DuctEmittance, 0.9);
    EXPECT_EQ(AirflowNetworkLinkageViewFactorData(1).LinkageSurfaceData(1).SurfaceName, "ATTIC FLOOR");
    EXPECT_EQ(AirflowNetworkLinkageViewFactorData(1).LinkageSurfaceData(1).ViewFactor, 0.483577);
    EXPECT_EQ(AirflowNetworkLinkageViewFactorData(1).LinkageSurfaceData(2).SurfaceName, "ATTIC ROOF NORTH");
    EXPECT_EQ(AirflowNetworkLinkageViewFactorData(1).LinkageSurfaceData(2).ViewFactor, 0.237692);
    EXPECT_EQ(AirflowNetworkLinkageViewFactorData(1).LinkageSurfaceData(3).SurfaceName, "ATTIC ROOF SOUTH");
    EXPECT_EQ(AirflowNetworkLinkageViewFactorData(1).LinkageSurfaceData(3).ViewFactor, 0.237692);
    EXPECT_EQ(AirflowNetworkLinkageViewFactorData(1).LinkageSurfaceData(4).SurfaceName, "EAST WALL ATTIC");
    EXPECT_EQ(AirflowNetworkLinkageViewFactorData(1).LinkageSurfaceData(4).ViewFactor, 0.02052);
    EXPECT_EQ(AirflowNetworkLinkageViewFactorData(1).LinkageSurfaceData(5).SurfaceName, "WEST WALL ATTIC");
    EXPECT_EQ(AirflowNetworkLinkageViewFactorData(1).LinkageSurfaceData(5).ViewFactor, 0.02052);

    Real64 const tol = 0.01;

    // Outside convection coefficients
    // Calculate convection resistance given a convection coefficient
    EXPECT_NEAR(CalcDuctOutsideConvResist(20, 10, 0.001, 101000, 1, 2, 5), 0.2, tol);
    EXPECT_NEAR(CalcDuctOutsideConvResist(20, 10, 0.001, 101000, 1, 2, 20), 0.05, tol);
    EXPECT_NEAR(CalcDuctOutsideConvResist(20, 10, 0.001, 101000, 1, 2, 0.1), 10, tol);

    //// Calculate convection resistance from correlation
    EXPECT_NEAR(CalcDuctOutsideConvResist(20, 10, 0.001, 101000, 0.1, 2, 0), 0.2297, tol);
    EXPECT_NEAR(CalcDuctOutsideConvResist(20, 10, 0.001, 101000, 1.0, 2, 0), 0.4093, tol);
    EXPECT_NEAR(CalcDuctOutsideConvResist(20, 10, 0.001, 101000, 1.5, 2, 0), 0.4531, tol);

    EXPECT_NEAR(CalcDuctOutsideConvResist(10, 20, 0.001, 101000, 0.1, 2, 0), 0.2368, tol);
    EXPECT_NEAR(CalcDuctOutsideConvResist(10, 20, 0.001, 101000, 1.0, 2, 0), 0.4218, tol);
    EXPECT_NEAR(CalcDuctOutsideConvResist(10, 20, 0.001, 101000, 1.5, 2, 0), 0.4670, tol);

    // Calculate convection resistance given a convection coefficient
    EXPECT_NEAR(CalcDuctInsideConvResist(20, 0.1, 1, 5), 0.2, tol);
    EXPECT_NEAR(CalcDuctInsideConvResist(20, 0.1, 1, 20), 0.05, tol);
    EXPECT_NEAR(CalcDuctInsideConvResist(20, 0.1, 1, 0.1), 10, tol);

    // Calculate convection resistance from correlation
    EXPECT_NEAR(CalcDuctInsideConvResist(20, 0.1, 1, 0), 1.611, tol);
    EXPECT_NEAR(CalcDuctInsideConvResist(20, 1.0, 1, 0), 0.2554, tol);
    EXPECT_NEAR(CalcDuctInsideConvResist(40, 0.1, 1, 0), 1.5879, tol);
    EXPECT_NEAR(CalcDuctInsideConvResist(40, 1.0, 1, 0), 0.2516, tol);
}

TEST_F(EnergyPlusFixture, AirflowNetworkBalanceManager_AirThermConductivity)
{

    Real64 const tol = 0.00001;

    EXPECT_NEAR(airThermConductivity(-30), 0.02212, tol);
    EXPECT_NEAR(airThermConductivity(-20), 0.02212, tol);
    EXPECT_NEAR(airThermConductivity(0), 0.02364, tol);
    EXPECT_NEAR(airThermConductivity(20), 0.02514, tol);
    EXPECT_NEAR(airThermConductivity(40), 0.02662, tol);
    EXPECT_NEAR(airThermConductivity(60), 0.02808, tol);
    EXPECT_NEAR(airThermConductivity(70), 0.02881, tol);
    EXPECT_NEAR(airThermConductivity(80), 0.02881, tol);
}

TEST_F(EnergyPlusFixture, AirflowNetworkBalanceManager_AirDynamicVisc)
{

    Real64 const tol = 0.000001;

    EXPECT_NEAR(airDynamicVisc(-30), 1.635e-5, tol);
    EXPECT_NEAR(airDynamicVisc(-20), 1.635e-5, tol);
    EXPECT_NEAR(airDynamicVisc(0), 1.729e-5, tol);
    EXPECT_NEAR(airDynamicVisc(20), 1.823e-5, tol);
    EXPECT_NEAR(airDynamicVisc(40), 1.917e-5, tol);
    EXPECT_NEAR(airDynamicVisc(60), 2.011e-5, tol);
    EXPECT_NEAR(airDynamicVisc(70), 2.058e-5, tol);
    EXPECT_NEAR(airDynamicVisc(80), 2.058e-5, tol);
}

TEST_F(EnergyPlusFixture, AirflowNetworkBalanceManager_AirKinematicVisc)
{

    Real64 const tol = 0.000001;

    EXPECT_NEAR(airKinematicVisc(-30, 0.001, 101000), 1.169e-5, tol);
    EXPECT_NEAR(airKinematicVisc(-20, 0.001, 101000), 1.169e-5, tol);
    EXPECT_NEAR(airKinematicVisc(0, 0.001, 101000), 1.338e-5, tol);
    EXPECT_NEAR(airKinematicVisc(20, 0.001, 101000), 1.516e-5, tol);
    EXPECT_NEAR(airKinematicVisc(40, 0.001, 101000), 1.702e-5, tol);
    EXPECT_NEAR(airKinematicVisc(60, 0.001, 101000), 1.896e-5, tol);
    EXPECT_NEAR(airKinematicVisc(70, 0.001, 101000), 1.995e-5, tol);
    EXPECT_NEAR(airKinematicVisc(80, 0.001, 101000), 1.995e-5, tol);
}

TEST_F(EnergyPlusFixture, AirflowNetworkBalanceManager_AirThermalDiffusivity)
{

    Real64 const tol = 0.000001;

    EXPECT_NEAR(airThermalDiffusivity(-30, 0.001, 101000), 1.578e-5, tol);
    EXPECT_NEAR(airThermalDiffusivity(-20, 0.001, 101000), 1.578e-5, tol);
    EXPECT_NEAR(airThermalDiffusivity(0, 0.001, 101000), 1.818e-5, tol);
    EXPECT_NEAR(airThermalDiffusivity(20, 0.001, 101000), 2.074e-5, tol);
    EXPECT_NEAR(airThermalDiffusivity(40, 0.001, 101000), 2.346e-5, tol);
    EXPECT_NEAR(airThermalDiffusivity(60, 0.001, 101000), 2.632e-5, tol);
    EXPECT_NEAR(airThermalDiffusivity(70, 0.001, 101000), 2.780e-5, tol);
    EXPECT_NEAR(airThermalDiffusivity(80, 0.001, 101000), 2.780e-5, tol);
}

TEST_F(EnergyPlusFixture, AirflowNetworkBalanceManager_AirPrandtl)
{

    Real64 const tol = 0.0001;

    EXPECT_NEAR(airPrandtl(-30, 0.001, 101000), 0.7362, tol);
    EXPECT_NEAR(airPrandtl(-20, 0.001, 101000), 0.7362, tol);
    EXPECT_NEAR(airPrandtl(0, 0.001, 101000), 0.7300, tol);
    EXPECT_NEAR(airPrandtl(20, 0.001, 101000), 0.7251, tol);
    EXPECT_NEAR(airPrandtl(40, 0.001, 101000), 0.7213, tol);
    EXPECT_NEAR(airPrandtl(60, 0.001, 101000), 0.7184, tol);
    EXPECT_NEAR(airPrandtl(70, 0.001, 101000), 0.7172, tol);
    EXPECT_NEAR(airPrandtl(80, 0.001, 101000), 0.7172, tol);
}

TEST_F(EnergyPlusFixture, TestWindPressureTable)
{
    // Test a Table:OneIV object as a wind pressure curve
    std::string const idf_objects = delimited_string({"Version,8.6;",
                                                      "Table:OneIndependentVariable,",
                                                      "  EFacade_WPCCurve,        !- Name",
                                                      "  Linear,                  !- Curve Type",
                                                      "  LinearInterpolationOfTable,  !- Interpolation Method",
                                                      "  0,                       !- Minimum Value of X",
                                                      "  360,                     !- Maximum Value of X",
                                                      "  -1,                      !- Minimum Table Output",
                                                      "  1,                       !- Maximum Table Output",
                                                      "  Dimensionless,           !- Input Unit Type for X",
                                                      "  Dimensionless,           !- Output Unit Type",
                                                      "  1,                       !- Normalization Reference",
                                                      "  0,                       !- X Value #1",
                                                      "  -0.56,                   !- Output Value #1",
                                                      "  30,                      !- X Value #2",
                                                      "  0.04,                    !- Output Value #2",
                                                      "  60,                      !- X Value #3",
                                                      "  0.48,                    !- Output Value #3",
                                                      "  90,                      !- X Value #4",
                                                      "  0.6,                     !- Output Value #4",
                                                      "  120,                     !- X Value #5",
                                                      "  0.48,                    !- Output Value #5",
                                                      "  150,                     !- X Value #6",
                                                      "  0.04,                    !- Output Value #6",
                                                      "  180,                     !- X Value #7",
                                                      "  -0.56,                   !- Output Value #7",
                                                      "  210,                     !- N20",
                                                      "  -0.56,                   !- N21",
                                                      "  240,                     !- N22",
                                                      "  -0.42,                   !- N23",
                                                      "  270,                     !- N24",
                                                      "  -0.37,                   !- N25",
                                                      "  300,                     !- N26",
                                                      "  -0.42,                   !- N27",
                                                      "  330,                     !- N28",
                                                      "  -0.56,                   !- N29",
                                                      "  360,                     !- N30",
                                                      "  -0.56;                   !- N31"});

    // Load and verify the table
    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_EQ(0, CurveManager::NumCurves);
    CurveManager::GetCurveInput();
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(1, CurveManager::NumCurves);
    EXPECT_EQ(1, CurveManager::PerfCurve(1).NumDims);
    EXPECT_EQ("EFACADE_WPCCURVE", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("EFACADE_WPCCURVE"));
    EXPECT_EQ("Table:OneIndependentVariable", CurveManager::PerfCurve(1).ObjectType);
    EXPECT_DOUBLE_EQ(-0.56, CurveManager::CurveValue(1, 0.0));   // In-range value
    EXPECT_DOUBLE_EQ(0.54, CurveManager::CurveValue(1, 105.0));  // In-range value
    EXPECT_DOUBLE_EQ(-0.56, CurveManager::CurveValue(1, -10.0)); // Minimum x
    EXPECT_DOUBLE_EQ(-0.56, CurveManager::CurveValue(1, 5000));  // Maximum x
    EXPECT_FALSE(has_err_output());

    // Set up some environmental parameters
    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::OutDryBulbTemp = 25.0;
    DataEnvironment::WindDir = 105.0;
    DataEnvironment::OutHumRat = 0.0;        // Dry air only
    DataEnvironment::SiteTempGradient = 0.0; // Disconnect z from testing

    // Make sure we can compute the right density
    Real64 rho = Psychrometrics::PsyRhoAirFnPbTdbW(DataEnvironment::OutBaroPress, DataEnvironment::OutDryBulbTemp, DataEnvironment::OutHumRat);
    EXPECT_DOUBLE_EQ(1.1841123742118911, rho);
    // CalcWindPressure(MultizoneExternalNodeData(i).curve, 1
    //	Vref, 1
    // AirflowNetworkNodeData(n).NodeHeight,  10
    // MultizoneExternalNodeData(i).azimuth, 0
    // MultizoneExternalNodeData(i).symmetricCurve, MultizoneExternalNodeData(i).useRelativeAngle);
    // Compute wind pressure with current defaults
    Real64 windSpeed = 1.0;
    Real64 dryBulb = DataEnvironment::OutDryBulbTempAt(10.0);
    Real64 azimuth = 0.0;
    Real64 windDir = DataEnvironment::WindDir;
    Real64 humRat = DataEnvironment::OutHumRat;
    Real64 p = AirflowNetworkBalanceManager::CalcWindPressure(1, false, false, azimuth, windSpeed, windDir, dryBulb, humRat);
    EXPECT_DOUBLE_EQ(0.54 * 0.5 * 1.1841123742118911, p);
    // Test on an east wall, which has a relative angle of 15 (for wind direction 105)
    azimuth = 90.0;
    p = AirflowNetworkBalanceManager::CalcWindPressure(1, false, true, azimuth, windSpeed, windDir, dryBulb, humRat);
    EXPECT_DOUBLE_EQ(-0.26 * 0.5 * 1.1841123742118911, p);
    // Test on a wall with azimuth 105, for a zero relative angle
    azimuth = 105.0;
    p = AirflowNetworkBalanceManager::CalcWindPressure(1, false, true, azimuth, windSpeed, windDir, dryBulb, humRat);
    EXPECT_DOUBLE_EQ(-0.56 * 0.5 * 1.1841123742118911, p);
}

TEST_F(EnergyPlusFixture, TestWPCValue)
{
    // Test loading a WPC object into a Table:OneIV
    std::string const idf_objects = delimited_string({"Version,8.6;",
                                                      "AirflowNetwork:MultiZone:WindPressureCoefficientArray,",
                                                      "  Every 30 Degrees,        !- Name",
                                                      "  0,                       !- Wind Direction 1 {deg}",
                                                      "  30,                      !- Wind Direction 2 {deg}",
                                                      "  60,                      !- Wind Direction 3 {deg}",
                                                      "  90,                      !- Wind Direction 4 {deg}",
                                                      "  120,                     !- Wind Direction 5 {deg}",
                                                      "  150,                     !- Wind Direction 6 {deg}",
                                                      "  180,                     !- Wind Direction 7 {deg}",
                                                      "  210,                     !- Wind Direction 8 {deg}",
                                                      "  240,                     !- Wind Direction 9 {deg}",
                                                      "  270,                     !- Wind Direction 10 {deg}",
                                                      "  300,                     !- Wind Direction 11 {deg}",
                                                      "  330;                     !- Wind Direction 12 {deg}",
                                                      "AirflowNetwork:MultiZone:WindPressureCoefficientValues,",
                                                      "  NFacade_WPCValue,        !- Name",
                                                      "  Every 30 Degrees,        !- AirflowNetwork:MultiZone:WindPressureCoefficientArray Name",
                                                      "  0.60,                    !- Wind Pressure Coefficient Value 1 {dimensionless}",
                                                      "  0.48,                    !- Wind Pressure Coefficient Value 2 {dimensionless}",
                                                      "  0.04,                    !- Wind Pressure Coefficient Value 3 {dimensionless}",
                                                      "  -0.56,                   !- Wind Pressure Coefficient Value 4 {dimensionless}",
                                                      "  -0.56,                   !- Wind Pressure Coefficient Value 5 {dimensionless}",
                                                      "  -0.42,                   !- Wind Pressure Coefficient Value 6 {dimensionless}",
                                                      "  -0.37,                   !- Wind Pressure Coefficient Value 7 {dimensionless}",
                                                      "  -0.42,                   !- Wind Pressure Coefficient Value 8 {dimensionless}",
                                                      "  -0.56,                   !- Wind Pressure Coefficient Value 9 {dimensionless}",
                                                      "  -0.56,                   !- Wind Pressure Coefficient Value 10 {dimensionless}",
                                                      "  0.04,                    !- Wind Pressure Coefficient Value 11 {dimensionless}",
                                                      "  0.48;                    !- Wind Pressure Coefficient Value 12 {dimensionless}"});

    // Load and verify the table
    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_EQ(0, CurveManager::NumCurves);
    CurveManager::GetCurveInput();
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(1, CurveManager::NumCurves);
    EXPECT_EQ(1, CurveManager::PerfCurve(1).NumDims);
    EXPECT_EQ("NFACADE_WPCVALUE", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("NFACADE_WPCVALUE"));
    EXPECT_EQ("AirflowNetwork:MultiZone:WindPressureCoefficientValues", CurveManager::PerfCurve(1).ObjectType);
    EXPECT_DOUBLE_EQ(0.6, CurveManager::CurveValue(1, 0.0));     // In-range value
    EXPECT_DOUBLE_EQ(-0.56, CurveManager::CurveValue(1, 105.0)); // In-range value
    EXPECT_DOUBLE_EQ(0.6, CurveManager::CurveValue(1, -10.0));   // Minimum x
    EXPECT_DOUBLE_EQ(0.6, CurveManager::CurveValue(1, 5000));    // Maximum x
    EXPECT_FALSE(has_err_output());

    // Set up some environmental parameters
    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::OutDryBulbTemp = 25.0;
    DataEnvironment::WindDir = 105.0;
    DataEnvironment::OutHumRat = 0.0;        // Dry air only
    DataEnvironment::SiteTempGradient = 0.0; // Disconnect z from testing

    Real64 windSpeed = 1.0;
    Real64 dryBulb = DataEnvironment::OutDryBulbTempAt(10.0);
    Real64 azimuth = 0.0;
    Real64 windDir = DataEnvironment::WindDir;
    Real64 humRat = DataEnvironment::OutHumRat;

    // Make sure we can compute the right density
    Real64 rho = Psychrometrics::PsyRhoAirFnPbTdbW(DataEnvironment::OutBaroPress, DataEnvironment::OutDryBulbTemp, DataEnvironment::OutHumRat);
    EXPECT_DOUBLE_EQ(1.1841123742118911, rho);

    // Compute wind pressure with current defaults
    Real64 p = AirflowNetworkBalanceManager::CalcWindPressure(1, false, false, azimuth, windSpeed, windDir, dryBulb, humRat);
    EXPECT_DOUBLE_EQ(-0.56 * 0.5 * 1.1841123742118911, p);
    // Test on an east wall, which has a relative angle of 15 (for wind direction 105)
    azimuth = 90.0;
    p = AirflowNetworkBalanceManager::CalcWindPressure(1, false, true, azimuth, windSpeed, windDir, dryBulb, humRat);
    EXPECT_DOUBLE_EQ(0.54 * 0.5 * 1.1841123742118911, p);
    // Test on a wall with azimuth 105, for a zero relative angle
    azimuth = 105.0;
    p = AirflowNetworkBalanceManager::CalcWindPressure(1, false, true, azimuth, windSpeed, windDir, dryBulb, humRat);
    EXPECT_DOUBLE_EQ(0.6 * 0.5 * 1.1841123742118911, p);
}

TEST_F(EnergyPlusFixture, TestExternalNodes)
{
    std::string const idf_objects = delimited_string(
        {"Version,8.6;", "Material,", "  A1 - 1 IN STUCCO,        !- Name", "  Smooth,                  !- Roughness",
         "  2.5389841E-02,           !- Thickness {m}", "  0.6918309,               !- Conductivity {W/m-K}",
         "  1858.142,                !- Density {kg/m3}", "  836.8000,                !- Specific Heat {J/kg-K}",
         "  0.9000000,               !- Thermal Absorptance", "  0.9200000,               !- Solar Absorptance",
         "  0.9200000;               !- Visible Absorptance", "Material,", "  C4 - 4 IN COMMON BRICK,  !- Name",
         "  Rough,                   !- Roughness", "  0.1014984,               !- Thickness {m}",
         "  0.7264224,               !- Conductivity {W/m-K}", "  1922.216,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}", "  0.9000000,               !- Thermal Absorptance",
         "  0.7600000,               !- Solar Absorptance", "  0.7600000;               !- Visible Absorptance", "Material,",
         "  E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Name", "  Smooth,                  !- Roughness", "  1.905E-02,               !- Thickness {m}",
         "  0.7264224,               !- Conductivity {W/m-K}", "  1601.846,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}", "  0.9000000,               !- Thermal Absorptance",
         "  0.9200000,               !- Solar Absorptance", "  0.9200000;               !- Visible Absorptance", "Material,",
         "  C6 - 8 IN CLAY TILE,     !- Name", "  Smooth,                  !- Roughness", "  0.2033016,               !- Thickness {m}",
         "  0.5707605,               !- Conductivity {W/m-K}", "  1121.292,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}", "  0.9000000,               !- Thermal Absorptance",
         "  0.8200000,               !- Solar Absorptance", "  0.8200000;               !- Visible Absorptance", "Material,",
         "  C10 - 8 IN HW CONCRETE,  !- Name", "  MediumRough,             !- Roughness", "  0.2033016,               !- Thickness {m}",
         "  1.729577,                !- Conductivity {W/m-K}", "  2242.585,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}", "  0.9000000,               !- Thermal Absorptance",
         "  0.6500000,               !- Solar Absorptance", "  0.6500000;               !- Visible Absorptance", "Material,",
         "  E2 - 1 / 2 IN SLAG OR STONE,  !- Name", "  Rough,                   !- Roughness", "  1.2710161E-02,           !- Thickness {m}",
         "  1.435549,                !- Conductivity {W/m-K}", "  881.0155,                !- Density {kg/m3}",
         "  1673.600,                !- Specific Heat {J/kg-K}", "  0.9000000,               !- Thermal Absorptance",
         "  0.5500000,               !- Solar Absorptance", "  0.5500000;               !- Visible Absorptance", "Material,",
         "  E3 - 3 / 8 IN FELT AND MEMBRANE,  !- Name", "  Rough,                   !- Roughness", "  9.5402403E-03,           !- Thickness {m}",
         "  0.1902535,               !- Conductivity {W/m-K}", "  1121.292,                !- Density {kg/m3}",
         "  1673.600,                !- Specific Heat {J/kg-K}", "  0.9000000,               !- Thermal Absorptance",
         "  0.7500000,               !- Solar Absorptance", "  0.7500000;               !- Visible Absorptance", "Material,",
         "  B5 - 1 IN DENSE INSULATION,  !- Name", "  VeryRough,               !- Roughness", "  2.5389841E-02,           !- Thickness {m}",
         "  4.3239430E-02,           !- Conductivity {W/m-K}", "  91.30524,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}", "  0.9000000,               !- Thermal Absorptance",
         "  0.5000000,               !- Solar Absorptance", "  0.5000000;               !- Visible Absorptance", "Material,",
         "  C12 - 2 IN HW CONCRETE,  !- Name", "  MediumRough,             !- Roughness", "  5.0901599E-02,           !- Thickness {m}",
         "  1.729577,                !- Conductivity {W/m-K}", "  2242.585,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}", "  0.9000000,               !- Thermal Absorptance",
         "  0.6500000,               !- Solar Absorptance", "  0.6500000;               !- Visible Absorptance", "Material,",
         "  1.375in-Solid-Core,      !- Name", "  Smooth,                  !- Roughness", "  3.4925E-02,              !- Thickness {m}",
         "  0.1525000,               !- Conductivity {W/m-K}", "  614.5000,                !- Density {kg/m3}",
         "  1630.0000,               !- Specific Heat {J/kg-K}", "  0.9000000,               !- Thermal Absorptance",
         "  0.9200000,               !- Solar Absorptance", "  0.9200000;               !- Visible Absorptance", "WindowMaterial:Glazing,",
         "  WIN-LAY-GLASS-LIGHT,     !- Name", "  SpectralAverage,         !- Optical Data Type",
         "  ,                        !- Window Glass Spectral Data Set Name", "  0.0025,                  !- Thickness {m}",
         "  0.850,                   !- Solar Transmittance at Normal Incidence",
         "  0.075,                   !- Front Side Solar Reflectance at Normal Incidence",
         "  0.075,                   !- Back Side Solar Reflectance at Normal Incidence",
         "  0.901,                   !- Visible Transmittance at Normal Incidence",
         "  0.081,                   !- Front Side Visible Reflectance at Normal Incidence",
         "  0.081,                   !- Back Side Visible Reflectance at Normal Incidence",
         "  0.0,                     !- Infrared Transmittance at Normal Incidence",
         "  0.84,                    !- Front Side Infrared Hemispherical Emissivity",
         "  0.84,                    !- Back Side Infrared Hemispherical Emissivity", "  0.9;                     !- Conductivity {W/m-K}",
         "Construction,", "  DOOR-CON,                !- Name", "  1.375in-Solid-Core;      !- Outside Layer", "Construction,",
         "  EXTWALL80,               !- Name", "  A1 - 1 IN STUCCO,        !- Outside Layer", "  C4 - 4 IN COMMON BRICK,  !- Layer 2",
         "  E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 3", "Construction,", "  PARTITION06,             !- Name",
         "  E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Outside Layer", "  C6 - 8 IN CLAY TILE,     !- Layer 2",
         "  E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 3", "  Construction,", "  FLOOR SLAB 8 IN,         !- Name",
         "  C10 - 8 IN HW CONCRETE;  !- Outside Layer", "Construction,", "  ROOF34,                  !- Name",
         "  E2 - 1 / 2 IN SLAG OR STONE,  !- Outside Layer", "  E3 - 3 / 8 IN FELT AND MEMBRANE,  !- Layer 2",
         "  B5 - 1 IN DENSE INSULATION,  !- Layer 3", "  C12 - 2 IN HW CONCRETE;  !- Layer 4", "Construction,", "  WIN-CON-LIGHT,           !- Name",
         "  WIN-LAY-GLASS-LIGHT;     !- Outside Layer", "Zone,", "  WEST_ZONE,               !- Name",
         "  0,                       !- Direction of Relative North {deg}", "  0,                       !- X Origin {m}",
         "  0,                       !- Y Origin {m}", "  0,                       !- Z Origin {m}", "  1,                       !- Type",
         "  1,                       !- Multiplier", "  autocalculate;           !- Ceiling Height {m}", "Zone,",
         "  EAST_ZONE,               !- Name", "  0,                       !- Direction of Relative North {deg}",
         "  0,                       !- X Origin {m}", "  0,                       !- Y Origin {m}", "  0,                       !- Z Origin {m}",
         "  1,                       !- Type", "  1,                       !- Multiplier", "  autocalculate;           !- Ceiling Height {m}",
         "Zone,", "  NORTH_ZONE,              !- Name", "  0,                       !- Direction of Relative North {deg}",
         "  0,                       !- X Origin {m}", "  0,                       !- Y Origin {m}", "  0,                       !- Z Origin {m}",
         "  1,                       !- Type", "  1,                       !- Multiplier", "  autocalculate;           !- Ceiling Height {m}",
         "GlobalGeometryRules,", "  UpperLeftCorner,         !- Starting Vertex Position", "  CounterClockWise,        !- Vertex Entry Direction",
         "  World;                   !- Coordinate System", "BuildingSurface:Detailed,", "  Surface_1,               !- Name",
         "  WALL,                    !- Surface Type", "  EXTWALL80,               !- Construction Name", "  WEST_ZONE,               !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition", "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure", "  WindExposed,             !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground", "  4,                       !- Number of Vertices",
         "  0,0,3.048000,            !- X,Y,Z ==> Vertex 1 {m}", "  0,0,0,                   !- X,Y,Z ==> Vertex 2 {m}",
         "  6.096000,0,0,            !- X,Y,Z ==> Vertex 3 {m}", "  6.096000,0,3.048000;     !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,",
         "  Surface_2,               !- Name", "  WALL,                    !- Surface Type", "  EXTWALL80,               !- Construction Name",
         "  WEST_ZONE,               !- Zone Name", "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object", "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure", "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices", "  0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  0,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}", "  0,0,0,  !- X,Y,Z ==> Vertex 3 {m}", "  0,0,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,", "  Surface_3,               !- Name", "  WALL,                    !- Surface Type",
         "  PARTITION06,             !- Construction Name", "  WEST_ZONE,               !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition", "  Surface_17,              !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure", "  NoWind,                  !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground", "  4,                       !- Number of Vertices",
         "  6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}", "  6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
         "  0,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}", "  0,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,",
         "  Surface_4,               !- Name", "  WALL,                    !- Surface Type", "  PARTITION06,             !- Construction Name",
         "  WEST_ZONE,               !- Zone Name", "  Surface,                 !- Outside Boundary Condition",
         "  Surface_10,              !- Outside Boundary Condition Object", "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure", "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices", "  6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  6.096000,0,0,  !- X,Y,Z ==> Vertex 2 {m}", "  6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,", "  Surface_5,               !- Name",
         "  FLOOR,                   !- Surface Type", "  FLOOR SLAB 8 IN,         !- Construction Name", "  WEST_ZONE,               !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition", "  Surface_5,               !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure", "  NoWind,                  !- Wind Exposure",
         "  1.000000,                !- View Factor to Ground", "  4,                       !- Number of Vertices",
         "  0,0,0,  !- X,Y,Z ==> Vertex 1 {m}", "  0,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}", "  6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096000,0,0;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,", "  Surface_6,               !- Name",
         "  ROOF,                    !- Surface Type", "  ROOF34,                  !- Construction Name", "  WEST_ZONE,               !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition", "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure", "  WindExposed,             !- Wind Exposure",
         "  0,                       !- View Factor to Ground", "  4,                       !- Number of Vertices",
         "  0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}", "  0,0,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
         "  6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 3 {m}", "  6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,",
         "  Surface_8,               !- Name", "  WALL,                    !- Surface Type", "  EXTWALL80,               !- Construction Name",
         "  EAST_ZONE,               !- Zone Name", "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object", "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure", "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices", "  6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  6.096000,0,0,  !- X,Y,Z ==> Vertex 2 {m}", "  12.19200,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
         "  12.19200,0,3.048000;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,", "  Surface_9,               !- Name",
         "  WALL,                    !- Surface Type", "  EXTWALL80,               !- Construction Name", "  EAST_ZONE,               !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition", "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure", "  WindExposed,             !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground", "  4,                       !- Number of Vertices",
         "  12.19200,0,3.048000,  !- X,Y,Z ==> Vertex 1 {m}", "  12.19200,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}", "  12.19200,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,",
         "  Surface_10,              !- Name", "  WALL,                    !- Surface Type", "  PARTITION06,             !- Construction Name",
         "  EAST_ZONE,               !- Zone Name", "  Surface,                 !- Outside Boundary Condition",
         "  Surface_4,               !- Outside Boundary Condition Object", "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure", "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices", "  6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}", "  6.096000,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096001,0,3.048000;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,", "  Surface_11,              !- Name",
         "  WALL,                    !- Surface Type", "  PARTITION06,             !- Construction Name", "  EAST_ZONE,               !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition", "  Surface_18,              !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure", "  NoWind,                  !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground", "  4,                       !- Number of Vertices",
         "  12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}", "  12.19200,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
         "  6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}", "  6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,",
         "  Surface_12,              !- Name", "  FLOOR,                   !- Surface Type", "  FLOOR SLAB 8 IN,         !- Construction Name",
         "  EAST_ZONE,               !- Zone Name", "  Surface,                 !- Outside Boundary Condition",
         "  Surface_12,              !- Outside Boundary Condition Object", "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure", "  1.000000,                !- View Factor to Ground",
         "  4,                       !- Number of Vertices", "  6.096000,0,0,  !- X,Y,Z ==> Vertex 1 {m}",
         "  6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}", "  12.19200,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
         "  12.19200,0,0;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,", "  Surface_13,              !- Name",
         "  ROOF,                    !- Surface Type", "  ROOF34,                  !- Construction Name", "  EAST_ZONE,               !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition", "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure", "  WindExposed,             !- Wind Exposure",
         "  0,                       !- View Factor to Ground", "  4,                       !- Number of Vertices",
         "  6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}", "  6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,0,3.048000,  !- X,Y,Z ==> Vertex 3 {m}", "  12.19200,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,",
         "  Surface_14,              !- Name", "  WALL,                    !- Surface Type", "  EXTWALL80,               !- Construction Name",
         "  NORTH_ZONE,              !- Zone Name", "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object", "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure", "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices", "  0,12.19200,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  0,12.19200,0,  !- X,Y,Z ==> Vertex 2 {m}", "  0,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
         "  0,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,", "  Surface_15,              !- Name",
         "  WALL,                    !- Surface Type", "  EXTWALL80,               !- Construction Name", "  NORTH_ZONE,              !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition", "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure", "  WindExposed,             !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground", "  4,                       !- Number of Vertices",
         "  12.19200,12.19200,3.048000,  !- X,Y,Z ==> Vertex 1 {m}", "  12.19200,12.19200,0,  !- X,Y,Z ==> Vertex 2 {m}",
         "  0,12.19200,0,  !- X,Y,Z ==> Vertex 3 {m}", "  0,12.19200,3.048000;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,",
         "  Surface_16,              !- Name", "  WALL,                    !- Surface Type", "  EXTWALL80,               !- Construction Name",
         "  NORTH_ZONE,              !- Zone Name", "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object", "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure", "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices", "  12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  12.19200,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}", "  12.19200,12.19200,0,  !- X,Y,Z ==> Vertex 3 {m}",
         "  12.19200,12.19200,3.048000;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,", "  Surface_17,              !- Name",
         "  WALL,                    !- Surface Type", "  PARTITION06,             !- Construction Name", "  NORTH_ZONE,              !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition", "  Surface_3,               !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure", "  NoWind,                  !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground", "  4,                       !- Number of Vertices",
         "  0.000,6.096,3.048,  !- X,Y,Z ==> Vertex 1 {m}", "  0.000,6.096,0.000,  !- X,Y,Z ==> Vertex 2 {m}",
         "  6.096,6.096,0.000,  !- X,Y,Z ==> Vertex 3 {m}", "  6.096,6.096,3.048;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,",
         "  Surface_18,              !- Name", "  WALL,                    !- Surface Type", "  PARTITION06,             !- Construction Name",
         "  NORTH_ZONE,              !- Zone Name", "  Surface,                 !- Outside Boundary Condition",
         "  Surface_11,              !- Outside Boundary Condition Object", "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure", "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices", "  6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}", "  12.19200,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
         "  12.19200,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,", "  Surface_19,              !- Name",
         "  FLOOR,                   !- Surface Type", "  FLOOR SLAB 8 IN,         !- Construction Name", "  NORTH_ZONE,              !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition", "  Surface_19,              !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure", "  NoWind,                  !- Wind Exposure",
         "  1.000000,                !- View Factor to Ground", "  4,                       !- Number of Vertices",
         "  0,6.096000,0,  !- X,Y,Z ==> Vertex 1 {m}", "  0,12.19200,0,  !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,12.19200,0,  !- X,Y,Z ==> Vertex 3 {m}", "  12.19200,6.096000,0;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,",
         "  Surface_20,              !- Name", "  ROOF,                    !- Surface Type", "  ROOF34,                  !- Construction Name",
         "  NORTH_ZONE,              !- Zone Name", "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object", "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure", "  0,                       !- View Factor to Ground",
         "  4,                       !- Number of Vertices", "  0,12.19200,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 2 {m}", "  12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
         "  12.19200,12.19200,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
         /*"FenestrationSurface:Detailed,",
         "  WINDOW11,                !- Name",
         "  WINDOW,                  !- Surface Type",
         "  WIN-CON-LIGHT,           !- Construction Name",
         "  Surface_1,               !- Building Surface Name",
                        "  ,                        !- Outside Boundary Condition Object",
                        "  0.5000000,               !- View Factor to Ground",
         "  ,                        !- Frame and Divider Name",
         "  1.0,                     !- Multiplier",
         "  3,                       !- Number of Vertices",
         "  1.00000,0,2.500000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  1.00000,0,1.0000000,  !- X,Y,Z ==> Vertex 2 {m}",
         "  5.000000,0,1.0000000;  !- X,Y,Z ==> Vertex 3 {m}",
         "FenestrationSurface:Detailed,",
         "  WINDOW12,                !- Name",
         "  WINDOW,                  !- Surface Type",
         "  WIN-CON-LIGHT,           !- Construction Name",
         "  Surface_1,               !- Building Surface Name",
                        "  ,                        !- Outside Boundary Condition Object",
                        "  0.5000000,               !- View Factor to Ground",

                        "  4,                       !- Number of Vertices",
                        "  0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
                        "  0,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
                        "  0,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
                        "  0,0,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
                        "BuildingSurface:Detailed,",
                        "  Surface_3,               !- Name",
                        "  WALL,                    !- Surface Type",
                        "  PARTITION06,             !- Construction Name",
                        "  WEST_ZONE,               !- Zone Name",
                        "  Surface,                 !- Outside Boundary Condition",
                        "  Surface_17,              !- Outside Boundary Condition Object",
                        "  NoSun,                   !- Sun Exposure",
                        "  NoWind,                  !- Wind Exposure",
                        "  0.5000000,               !- View Factor to Ground",
                        "  4,                       !- Number of Vertices",
                        "  6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
                        "  6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
                        "  0,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
                        "  0,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
                        "BuildingSurface:Detailed,",
                        "  Surface_4,               !- Name",
                        "  WALL,                    !- Surface Type",
                        "  PARTITION06,             !- Construction Name",
                        "  WEST_ZONE,               !- Zone Name",
                        "  Surface,                 !- Outside Boundary Condition",
                        "  Surface_10,              !- Outside Boundary Condition Object",
                        "  NoSun,                   !- Sun Exposure",
                        "  NoWind,                  !- Wind Exposure",
                        "  0.5000000,               !- View Factor to Ground",
                        "  4,                       !- Number of Vertices",
                        "  6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
                        "  6.096000,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
                        "  6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
                        "  6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
                        "BuildingSurface:Detailed,",
                        "  Surface_5,               !- Name",
                        "  FLOOR,                   !- Surface Type",
                        "  FLOOR SLAB 8 IN,         !- Construction Name",
                        "  WEST_ZONE,               !- Zone Name",
                        "  Surface,                 !- Outside Boundary Condition",
                        "  Surface_5,               !- Outside Boundary Condition Object",
                        "  NoSun,                   !- Sun Exposure",
                        "  NoWind,                  !- Wind Exposure",
                        "  1.000000,                !- View Factor to Ground",
                        "  4,                       !- Number of Vertices",
                        "  0,0,0,  !- X,Y,Z ==> Vertex 1 {m}",
                        "  0,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
                        "  6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
                        "  6.096000,0,0;  !- X,Y,Z ==> Vertex 4 {m}",
                        "BuildingSurface:Detailed,",
                        "  Surface_6,               !- Name",
                        "  ROOF,                    !- Surface Type",
                        "  ROOF34,                  !- Construction Name",
                        "  WEST_ZONE,               !- Zone Name",
                        "  Outdoors,                !- Outside Boundary Condition",
                        "  ,                        !- Outside Boundary Condition Object",
                        "  SunExposed,              !- Sun Exposure",
                        "  WindExposed,             !- Wind Exposure",
                        "  0,                       !- View Factor to Ground",
                        "  4,                       !- Number of Vertices",
                        "  0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
                        "  0,0,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
                        "  6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
                        "  6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
                        "BuildingSurface:Detailed,",
                        "  Surface_8,               !- Name",
                        "  WALL,                    !- Surface Type",
                        "  EXTWALL80,               !- Construction Name",
                        "  EAST_ZONE,               !- Zone Name",
                        "  Outdoors,                !- Outside Boundary Condition",
                        "  ,                        !- Outside Boundary Condition Object",
                        "  SunExposed,              !- Sun Exposure",
                        "  WindExposed,             !- Wind Exposure",
                        "  0.5000000,               !- View Factor to Ground",
                        "  4,                       !- Number of Vertices",
                        "  6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
                        "  6.096000,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
                        "  12.19200,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
                        "  12.19200,0,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
                        "BuildingSurface:Detailed,",
                        "  Surface_9,               !- Name",
                        "  WALL,                    !- Surface Type",
                        "  EXTWALL80,               !- Construction Name",
                        "  EAST_ZONE,               !- Zone Name",
                        "  Outdoors,                !- Outside Boundary Condition",
                        "  ,                        !- Outside Boundary Condition Object",
                        "  SunExposed,              !- Sun Exposure",
                        "  WindExposed,             !- Wind Exposure",
                        "  0.5000000,               !- View Factor to Ground",
                        "  4,                       !- Number of Vertices",
                        "  12.19200,0,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
                        "  12.19200,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
                        "  12.19200,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
                        "  12.19200,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
                        "BuildingSurface:Detailed,",
                        "  Surface_10,              !- Name",
                        "  WALL,                    !- Surface Type",
                        "  PARTITION06,             !- Construction Name",
                        "  EAST_ZONE,               !- Zone Name",
                        "  Surface,                 !- Outside Boundary Condition",
                        "  Surface_4,               !- Outside Boundary Condition Object",
                        "  NoSun,                   !- Sun Exposure",
                        "  NoWind,                  !- Wind Exposure",
                        "  0.5000000,               !- View Factor to Ground",
                        "  4,                       !- Number of Vertices",
                        "  6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
                        "  6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
                        "  6.096000,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
                        "  6.096001,0,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
                        "BuildingSurface:Detailed,",
                        "  Surface_11,              !- Name",
                        "  WALL,                    !- Surface Type",
                        "  PARTITION06,             !- Construction Name",
                        "  EAST_ZONE,               !- Zone Name",
                        "  Surface,                 !- Outside Boundary Condition",
                        "  Surface_18,              !- Outside Boundary Condition Object",
                        "  NoSun,                   !- Sun Exposure",
                        "  NoWind,                  !- Wind Exposure",
                        "  0.5000000,               !- View Factor to Ground",
                        "  4,                       !- Number of Vertices",
                        "  12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
                        "  12.19200,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
                        "  6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
                        "  6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
                        "BuildingSurface:Detailed,",
                        "  Surface_12,              !- Name",
                        "  FLOOR,                   !- Surface Type",
                        "  FLOOR SLAB 8 IN,         !- Construction Name",
                        "  EAST_ZONE,               !- Zone Name",
                        "  Surface,                 !- Outside Boundary Condition",
                        "  Surface_12,              !- Outside Boundary Condition Object",
                        "  NoSun,                   !- Sun Exposure",
                        "  NoWind,                  !- Wind Exposure",
                        "  1.000000,                !- View Factor to Ground",
                        "  4,                       !- Number of Vertices",
                        "  6.096000,0,0,  !- X,Y,Z ==> Vertex 1 {m}",
                        "  6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
                        "  12.19200,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
                        "  12.19200,0,0;  !- X,Y,Z ==> Vertex 4 {m}",
                        "BuildingSurface:Detailed,",
                        "  Surface_13,              !- Name",
                        "  ROOF,                    !- Surface Type",
                        "  ROOF34,                  !- Construction Name",
                        "  EAST_ZONE,               !- Zone Name",
                        "  Outdoors,                !- Outside Boundary Condition",
                        "  ,                        !- Outside Boundary Condition Object",
                        "  SunExposed,              !- Sun Exposure",
                        "  WindExposed,             !- Wind Exposure",
                        "  0,                       !- View Factor to Ground",
                        "  4,                       !- Number of Vertices",
                        "  6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
                        "  6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
                        "  12.19200,0,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
                        "  12.19200,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
                        "BuildingSurface:Detailed,",
                        "  Surface_14,              !- Name",
                        "  WALL,                    !- Surface Type",
                        "  EXTWALL80,               !- Construction Name",
                        "  NORTH_ZONE,              !- Zone Name",
                        "  Outdoors,                !- Outside Boundary Condition",
                        "  ,                        !- Outside Boundary Condition Object",
                        "  SunExposed,              !- Sun Exposure",
                        "  WindExposed,             !- Wind Exposure",
                        "  0.5000000,               !- View Factor to Ground",
                        "  4,                       !- Number of Vertices",
                        "  0,12.19200,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
                        "  0,12.19200,0,  !- X,Y,Z ==> Vertex 2 {m}",
                        "  0,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
                        "  0,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
                        "BuildingSurface:Detailed,",
                        "  Surface_15,              !- Name",
                        "  WALL,                    !- Surface Type",
                        "  EXTWALL80,               !- Construction Name",
                        "  NORTH_ZONE,              !- Zone Name",
                        "  Outdoors,                !- Outside Boundary Condition",
                        "  ,                        !- Outside Boundary Condition Object",
                        "  SunExposed,              !- Sun Exposure",
                        "  WindExposed,             !- Wind Exposure",
                        "  0.5000000,               !- View Factor to Ground",
                        "  4,                       !- Number of Vertices",
                        "  12.19200,12.19200,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
                        "  12.19200,12.19200,0,  !- X,Y,Z ==> Vertex 2 {m}",
                        "  0,12.19200,0,  !- X,Y,Z ==> Vertex 3 {m}",
                        "  0,12.19200,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
                        "BuildingSurface:Detailed,",
                        "  Surface_16,              !- Name",
                        "  WALL,                    !- Surface Type",
                        "  EXTWALL80,               !- Construction Name",
                        "  NORTH_ZONE,              !- Zone Name",
                        "  Outdoors,                !- Outside Boundary Condition",
                        "  ,                        !- Outside Boundary Condition Object",
                        "  SunExposed,              !- Sun Exposure",
                        "  WindExposed,             !- Wind Exposure",
                        "  0.5000000,               !- View Factor to Ground",
                        "  4,                       !- Number of Vertices",
                        "  12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
                        "  12.19200,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
                        "  12.19200,12.19200,0,  !- X,Y,Z ==> Vertex 3 {m}",
                        "  12.19200,12.19200,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
                        "BuildingSurface:Detailed,",
                        "  Surface_17,              !- Name",
                        "  WALL,                    !- Surface Type",
                        "  PARTITION06,             !- Construction Name",
                        "  NORTH_ZONE,              !- Zone Name",
                        "  Surface,                 !- Outside Boundary Condition",
                        "  Surface_3,               !- Outside Boundary Condition Object",
                        "  NoSun,                   !- Sun Exposure",
                        "  NoWind,                  !- Wind Exposure",
                        "  0.5000000,               !- View Factor to Ground",
                        "  4,                       !- Number of Vertices",
                        "  0.000,6.096,3.048,  !- X,Y,Z ==> Vertex 1 {m}",
                        "  0.000,6.096,0.000,  !- X,Y,Z ==> Vertex 2 {m}",
                        "  6.096,6.096,0.000,  !- X,Y,Z ==> Vertex 3 {m}",
                        "  6.096,6.096,3.048;  !- X,Y,Z ==> Vertex 4 {m}",
                        "BuildingSurface:Detailed,",
                        "  Surface_18,              !- Name",
                        "  WALL,                    !- Surface Type",
                        "  PARTITION06,             !- Construction Name",
                        "  NORTH_ZONE,              !- Zone Name",
                        "  Surface,                 !- Outside Boundary Condition",
                        "  Surface_11,              !- Outside Boundary Condition Object",
                        "  NoSun,                   !- Sun Exposure",
                        "  NoWind,                  !- Wind Exposure",
                        "  0.5000000,               !- View Factor to Ground",
                        "  4,                       !- Number of Vertices",
                        "  6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
                        "  6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
                        "  12.19200,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
                        "  12.19200,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
                        "BuildingSurface:Detailed,",
                        "  Surface_19,              !- Name",
                        "  FLOOR,                   !- Surface Type",
                        "  FLOOR SLAB 8 IN,         !- Construction Name",
                        "  NORTH_ZONE,              !- Zone Name",
                        "  Surface,                 !- Outside Boundary Condition",
                        "  Surface_19,              !- Outside Boundary Condition Object",
                        "  NoSun,                   !- Sun Exposure",
                        "  NoWind,                  !- Wind Exposure",
                        "  1.000000,                !- View Factor to Ground",
                        "  4,                       !- Number of Vertices",
                        "  0,6.096000,0,  !- X,Y,Z ==> Vertex 1 {m}",
                        "  0,12.19200,0,  !- X,Y,Z ==> Vertex 2 {m}",
                        "  12.19200,12.19200,0,  !- X,Y,Z ==> Vertex 3 {m}",
                        "  12.19200,6.096000,0;  !- X,Y,Z ==> Vertex 4 {m}",
                        "BuildingSurface:Detailed,",
                        "  Surface_20,              !- Name",
                        "  ROOF,                    !- Surface Type",
                        "  ROOF34,                  !- Construction Name",
                        "  NORTH_ZONE,              !- Zone Name",
                        "  Outdoors,                !- Outside Boundary Condition",
                        "  ,                        !- Outside Boundary Condition Object",
                        "  SunExposed,              !- Sun Exposure",
                        "  WindExposed,             !- Wind Exposure",
                        "  0,                       !- View Factor to Ground",
                        "  4,                       !- Number of Vertices",
                        "  0,12.19200,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
                        "  0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
                        "  12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
                        "  12.19200,12.19200,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
                        "FenestrationSurface:Detailed,",
                        "  WINDOW11,                !- Name",
                        "  WINDOW,                  !- Surface Type",
                        "  WIN-CON-LIGHT,           !- Construction Name",
                        "  Surface_1,               !- Building Surface Name",
                        "  ,                        !- Outside Boundary Condition Object",
                        "  0.5000000,               !- View Factor to Ground",
                        "  ,                        !- Frame and Divider Name",
                        "  1.0,                     !- Multiplier",
                        "  3,                       !- Number of Vertices",
                        "  1.00000,0,2.500000,  !- X,Y,Z ==> Vertex 1 {m}",
                        "  1.00000,0,1.0000000,  !- X,Y,Z ==> Vertex 2 {m}",
                        "  5.000000,0,1.0000000;  !- X,Y,Z ==> Vertex 3 {m}",
                        "FenestrationSurface:Detailed,",
                        "  WINDOW12,                !- Name",
                        "  WINDOW,                  !- Surface Type",
                        "  WIN-CON-LIGHT,           !- Construction Name",
                        "  Surface_1,               !- Building Surface Name",
                        "  ,                        !- Outside Boundary Condition Object",
                        "  0.5000000,               !- View Factor to Ground",
                        "  ,                        !- Frame and Divider Name",
                        "  1.0,                     !- Multiplier",
                        "  3,                       !- Number of Vertices",
                        "  5.00000,0,1.0000000,  !- X,Y,Z ==> Vertex 2 {m}",
                        "  5.000000,0,2.5000000,  !- X,Y,Z ==> Vertex 3 {m}",
                        "  1.000000,0,2.500000;  !- X,Y,Z ==> Vertex 4 {m}",
                        "FenestrationSurface:Detailed,",
                        "  DoorInSurface_3,         !- Name",
                        "  DOOR,                    !- Surface Type",
                        "  DOOR-CON,                !- Construction Name",
                        "  Surface_3,               !- Building Surface Name",
                        "  DoorInSurface_17,        !- Outside Boundary Condition Object",
                        "  0.5000000,               !- View Factor to Ground",
                        "  ,                        !- Frame and Divider Name",
                        "  1.0,                     !- Multiplier",
                        "  4,                       !- Number of Vertices",
                        "  3.500,6.096000,2.0,  !- X,Y,Z ==> Vertex 1 {m}",
                        "  3.500,6.096000,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
                        "  2.500,6.096000,0.0,  !- X,Y,Z ==> Vertex 3 {m}",
                        "  2.500,6.096000,2.0;  !- X,Y,Z ==> Vertex 4 {m}",
                        "FenestrationSurface:Detailed,",
                        "  WINDOW2,                 !- Name",
                        "  WINDOW,                  !- Surface Type",
                        "  WIN-CON-LIGHT,           !- Construction Name",
                        "  Surface_15,              !- Building Surface Name",
                        "  ,                        !- Outside Boundary Condition Object",
                        "  0.5000000,               !- View Factor to Ground",
                        "  ,                        !- Frame and Divider Name",
                        "  1.0,                     !- Multiplier",
                        "  4,                       !- Number of Vertices",
                        "  6.000000,12.19200,2.333000,  !- X,Y,Z ==> Vertex 1 {m}",
                        "  6.000000,12.19200,1.000000,  !- X,Y,Z ==> Vertex 2 {m}",
                        "  3.000000,12.19200,1.000000,  !- X,Y,Z ==> Vertex 3 {m}",
                        "  3.000000,12.19200,2.333000;  !- X,Y,Z ==> Vertex 4 {m}",
                        "FenestrationSurface:Detailed,",
                        "  DoorInSurface_17,        !- Name",
                        "  DOOR,                    !- Surface Type",
                        "  DOOR-CON,                !- Construction Name",
                        "  Surface_17,              !- Building Surface Name",
                        "  DoorInSurface_3,         !- Outside Boundary Condition Object",
                        "  0.5000000,               !- View Factor to Ground",
                        "  ,                        !- Frame and Divider Name",
                        "  1.0,                     !- Multiplier",
                        "  4,                       !- Number of Vertices",
                        "  2.500,6.096000,2.0,  !- X,Y,Z ==> Vertex 1 {m}",
                        "  2.500,6.096000,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
                        "  3.500,6.096000,0.0,  !- X,Y,Z ==> Vertex 3 {m}",
                        "  3.500,6.096000,2.0;  !- X,Y,Z ==> Vertex 4 {m}",*/
         "AirflowNetwork:SimulationControl,", "  NaturalVentilation,      !- Name", "  MultizoneWithoutDistribution,  !- AirflowNetwork Control",
         "  INPUT,                   !- Wind Pressure Coefficient Type",
         "  ExternalNode,            !- Height Selection for Local Wind Pressure Calculation", "  LOWRISE,                 !- Building Type",
         "  500,                     !- Maximum Number of Iterations {dimensionless}", "  ZeroNodePressures,       !- Initialization Type",
         "  1.0E-05,                 !- Relative Airflow Convergence Tolerance {dimensionless}",
         "  1.0E-06,                 !- Absolute Airflow Convergence Tolerance {kg/s}",
         "  -0.5,                    !- Convergence Acceleration Limit {dimensionless}",
         "  0.0,                     !- Azimuth Angle of Long Axis of Building {deg}",
         "  1.0;                     !- Ratio of Building Width Along Short Axis to Width Along Long Axis", "AirflowNetwork:MultiZone:Zone,",
         "  WEST_ZONE,               !- Zone Name", "  NoVent,                  !- Ventilation Control Mode",
         "  ,                        !- Ventilation Control Zone Temperature Setpoint Schedule Name",
         //"  Temperature,             !- Ventilation Control Mode",
         //"  WindowVentSched,         !- Ventilation Control Zone Temperature Setpoint Schedule Name",
         "  0.3,                     !- Minimum Venting Open Factor {dimensionless}",
         "  5.0,                     !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor {deltaC}",
         "  10.0,                    !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor {deltaC}",
         "  0.0,                     !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor {deltaJ/kg}",
         "  300000.0;                !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor {deltaJ/kg}",
         "AirflowNetwork:MultiZone:Zone,", "  EAST_ZONE,               !- Zone Name", "  NoVent,                  !- Ventilation Control Mode",
         "  ,                        !- Ventilation Control Zone Temperature Setpoint Schedule Name",
         "  1.0,                     !- Minimum Venting Open Factor {dimensionless}",
         "  0.0,                     !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor {deltaC}",
         "  100.0,                   !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor {deltaC}",
         "  0.0,                     !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor {deltaJ/kg}",
         "  300000.0;                !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor {deltaJ/kg}",
         "AirflowNetwork:MultiZone:Zone,", "  NORTH_ZONE,              !- Zone Name", "  NoVent,                  !- Ventilation Control Mode",
         "  ,                        !- Ventilation Control Zone Temperature Setpoint Schedule Name",
         //"  Temperature,             !- Ventilation Control Mode",
         //"  WindowVentSched,         !- Ventilation Control Zone Temperature Setpoint Schedule Name",
         "  1.0,                     !- Minimum Venting Open Factor {dimensionless}",
         "  0.0,                     !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor {deltaC}",
         "  100.0,                   !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor {deltaC}",
         "  0.0,                     !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor {deltaJ/kg}",
         "  300000.0;                !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor {deltaJ/kg}",
         "AirflowNetwork:MultiZone:Surface,", "  Surface_1,               !- Surface Name", "  CR-1,                    !- Leakage Component Name",
         "  SFacade,                 !- External Node Name",
         "  1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}", "AirflowNetwork:MultiZone:Surface,",
         "  Surface_4,               !- Surface Name", "  CR-1,                    !- Leakage Component Name",
         "  ,                        !- External Node Name",
         "  1.0;                     !- Window / Door Opening Factor, or Crack Factor{ dimensionless }", "AirflowNetwork:MultiZone:Surface,",
         "  Surface_11,              !- Surface Name", "  CR-1,                    !- Leakage Component Name",
         "  ,                        !- External Node Name",
         "  1.0;                     !- Window / Door Opening Factor, or Crack Factor{ dimensionless }", "AirflowNetwork:MultiZone:Surface,",
         "  Surface_15,              !- Surface Name", "  CR-1,                    !- Leakage Component Name",
         "  NFacade,                 !- External Node Name",
         "  1.0;                     !- Window / Door Opening Factor, or Crack Factor{ dimensionless }", "AirflowNetwork:MultiZone:ExternalNode,",
         "  NFacade,                 !- Name", "  1.524,                   !- External Node Height{ m }",
         "  NFacade_WPCValue;        !- Wind Pressure Coefficient Values Object Name", "AirflowNetwork:MultiZone:ExternalNode,",
         "  SFacade,                 !- Name", "  1.524,                   !- External Node Height{ m }",
         "  SFacade_WPCValue,        !- Wind Pressure Coefficient Values Object Name",
         "  No,                      !- Symmetric Wind Pressure Coefficient Curve", "  Absolute;                !- Wind Angle Type",
         "AirflowNetwork:MultiZone:ReferenceCrackConditions,", "  ReferenceCrackConditions,!- Name",
         "  20.0,                    !- Reference Temperature{ C }", "  101320,                  !- Reference Barometric Pressure{ Pa }",
         "  0.005;                   !- Reference Humidity Ratio{ kgWater / kgDryAir }", "AirflowNetwork:MultiZone:Surface:Crack,",
         "  CR-1,                    !- Name", "  0.01,                    !- Air Mass Flow Coefficient at Reference Conditions{ kg / s }",
         "  0.667,                   !- Air Mass Flow Exponent{ dimensionless }", "  ReferenceCrackConditions;!- Reference Crack Conditions",
         "AirflowNetwork:MultiZone:WindPressureCoefficientArray,", "  Every 30 Degrees,        !- Name",
         "  0,                       !- Wind Direction 1 {deg}", "  30,                      !- Wind Direction 2 {deg}",
         "  60,                      !- Wind Direction 3 {deg}", "  90,                      !- Wind Direction 4 {deg}",
         "  120,                     !- Wind Direction 5 {deg}", "  150,                     !- Wind Direction 6 {deg}",
         "  180,                     !- Wind Direction 7 {deg}", "  210,                     !- Wind Direction 8 {deg}",
         "  240,                     !- Wind Direction 9 {deg}", "  270,                     !- Wind Direction 10 {deg}",
         "  300,                     !- Wind Direction 11 {deg}", "  330;                     !- Wind Direction 12 {deg}",
         "AirflowNetwork:MultiZone:WindPressureCoefficientValues,", "  NFacade_WPCValue,        !- Name",
         "  Every 30 Degrees,        !- AirflowNetwork:MultiZone:WindPressureCoefficientArray Name",
         "  0.60,                    !- Wind Pressure Coefficient Value 1 {dimensionless}",
         "  0.48,                    !- Wind Pressure Coefficient Value 2 {dimensionless}",
         "  0.04,                    !- Wind Pressure Coefficient Value 3 {dimensionless}",
         "  -0.56,                   !- Wind Pressure Coefficient Value 4 {dimensionless}",
         "  -0.56,                   !- Wind Pressure Coefficient Value 5 {dimensionless}",
         "  -0.42,                   !- Wind Pressure Coefficient Value 6 {dimensionless}",
         "  -0.37,                   !- Wind Pressure Coefficient Value 7 {dimensionless}",
         "  -0.42,                   !- Wind Pressure Coefficient Value 8 {dimensionless}",
         "  -0.56,                   !- Wind Pressure Coefficient Value 9 {dimensionless}",
         "  -0.56,                   !- Wind Pressure Coefficient Value 10 {dimensionless}",
         "  0.04,                    !- Wind Pressure Coefficient Value 11 {dimensionless}",
         "  0.48;                    !- Wind Pressure Coefficient Value 12 {dimensionless}",
         "AirflowNetwork:MultiZone:WindPressureCoefficientValues,", "  SFacade_WPCValue,        !- Name",
         "  Every 30 Degrees,        !- AirflowNetwork:MultiZone:WindPressureCoefficientArray Name",
         "  -0.37,                   !- Wind Pressure Coefficient Value 1 {dimensionless}",
         "  -0.42,                   !- Wind Pressure Coefficient Value 2 {dimensionless}",
         "  -0.56,                   !- Wind Pressure Coefficient Value 3 {dimensionless}",
         "  -0.56,                   !- Wind Pressure Coefficient Value 4 {dimensionless}",
         "  0.04,                    !- Wind Pressure Coefficient Value 5 {dimensionless}",
         "  0.48,                    !- Wind Pressure Coefficient Value 6 {dimensionless}",
         "  0.60,                    !- Wind Pressure Coefficient Value 7 {dimensionless}",
         "  0.48,                    !- Wind Pressure Coefficient Value 8 {dimensionless}",
         "  0.04,                    !- Wind Pressure Coefficient Value 9 {dimensionless}",
         "  -0.56,                   !- Wind Pressure Coefficient Value 10 {dimensionless}",
         "  -0.56,                   !- Wind Pressure Coefficient Value 11 {dimensionless}",
         "  -0.42;                   !- Wind Pressure Coefficient Value 12 {dimensionless}", "SurfaceConvectionAlgorithm:Inside,TARP;",
         "SurfaceConvectionAlgorithm:Outside,DOE-2;", "HeatBalanceAlgorithm,ConductionTransferFunction;", "ZoneAirHeatBalanceAlgorithm,",
         "  AnalyticalSolution;      !- Algorithm"});
    ASSERT_TRUE(process_idf(idf_objects));

    bool errors = false;

    HeatBalanceManager::GetMaterialData(errors); // read material data
    EXPECT_FALSE(errors);                        // expect no errors

    HeatBalanceManager::GetConstructData(errors); // read construction data
    EXPECT_FALSE(errors);                         // expect no errors

    HeatBalanceManager::GetZoneData(errors); // read zone data
    EXPECT_FALSE(errors);                    // expect no errors

    // Magic to get surfaces read in correctly
    DataHeatBalance::HeatTransferAlgosUsed.allocate(1);
    DataHeatBalance::HeatTransferAlgosUsed(1) = OverallHeatTransferSolutionAlgo;
    SurfaceGeometry::CosBldgRotAppGonly = 1.0;
    SurfaceGeometry::SinBldgRotAppGonly = 0.0;

    SurfaceGeometry::GetSurfaceData(errors); // setup zone geometry and get zone data
    EXPECT_FALSE(errors);                    // expect no errors

    CurveManager::GetCurveInput();
    EXPECT_EQ(CurveManager::NumCurves, 2);

    AirflowNetworkBalanceManager::GetAirflowNetworkInput();

    // Check the airflow elements
    EXPECT_EQ(2u, DataAirflowNetwork::MultizoneExternalNodeData.size());
    EXPECT_EQ(3u, DataAirflowNetwork::MultizoneZoneData.size());
    EXPECT_EQ(4u, DataAirflowNetwork::MultizoneSurfaceData.size());
    EXPECT_EQ(1u, DataAirflowNetwork::MultizoneSurfaceCrackData.size());
    EXPECT_EQ(2u, DataAirflowNetwork::MultizoneSurfaceStdConditionsCrackData.size());

    EXPECT_EQ(0.0, DataAirflowNetwork::MultizoneExternalNodeData(1).azimuth);
    EXPECT_FALSE(DataAirflowNetwork::MultizoneExternalNodeData(1).symmetricCurve);
    EXPECT_FALSE(DataAirflowNetwork::MultizoneExternalNodeData(1).useRelativeAngle);
    EXPECT_EQ(1, DataAirflowNetwork::MultizoneExternalNodeData(1).curve);

    EXPECT_EQ(180.0, DataAirflowNetwork::MultizoneExternalNodeData(2).azimuth);
    EXPECT_FALSE(DataAirflowNetwork::MultizoneExternalNodeData(2).symmetricCurve);
    EXPECT_FALSE(DataAirflowNetwork::MultizoneExternalNodeData(2).useRelativeAngle);
    EXPECT_EQ(2, DataAirflowNetwork::MultizoneExternalNodeData(2).curve);

    // Set up some environmental parameters
    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::OutDryBulbTemp = 25.0;
    DataEnvironment::WindDir = 105.0;
    DataEnvironment::OutHumRat = 0.0;        // Dry air only
    DataEnvironment::SiteTempGradient = 0.0; // Disconnect z from testing
    DataEnvironment::SiteWindExp = 0.0;      // Disconnect variation by height
    DataEnvironment::WindSpeed = 10.0;

    // Make sure we can compute the right wind pressure
    Real64 rho = Psychrometrics::PsyRhoAirFnPbTdbW(DataEnvironment::OutBaroPress, DataEnvironment::OutDryBulbTemp, DataEnvironment::OutHumRat);
    EXPECT_DOUBLE_EQ(1.1841123742118911, rho);
    Real64 p =
        AirflowNetworkBalanceManager::CalcWindPressure(DataAirflowNetwork::MultizoneExternalNodeData(1).curve, false, false, 0.0, 1.0,
                                                       DataEnvironment::WindDir, DataEnvironment::OutDryBulbTempAt(10.0), DataEnvironment::OutHumRat);
    EXPECT_DOUBLE_EQ(-0.56 * 0.5 * 1.1841123742118911, p);

    // Make sure the reference velocity comes out right
    EXPECT_DOUBLE_EQ(10.0, DataEnvironment::WindSpeedAt(MultizoneExternalNodeData(1).height));

    EXPECT_EQ(5u, DataAirflowNetwork::AirflowNetworkNodeSimu.size());

    // Run the balance routine, for now only to get the pressure set at the external nodes
    AirflowNetworkBalanceManager::CalcAirflowNetworkAirBalance();

    EXPECT_DOUBLE_EQ(-0.56 * 0.5 * 118.41123742118911, DataAirflowNetwork::AirflowNetworkNodeSimu(4).PZ);
    EXPECT_DOUBLE_EQ(-0.26 * 0.5 * 118.41123742118911, DataAirflowNetwork::AirflowNetworkNodeSimu(5).PZ);
}

TEST_F(EnergyPlusFixture, TestExternalNodesWithTables)
{
    std::string const idf_objects = delimited_string(
        {"Version,8.6;", "Material,", "  A1 - 1 IN STUCCO,        !- Name", "  Smooth,                  !- Roughness",
         "  2.5389841E-02,           !- Thickness {m}", "  0.6918309,               !- Conductivity {W/m-K}",
         "  1858.142,                !- Density {kg/m3}", "  836.8000,                !- Specific Heat {J/kg-K}",
         "  0.9000000,               !- Thermal Absorptance", "  0.9200000,               !- Solar Absorptance",
         "  0.9200000;               !- Visible Absorptance", "Material,", "  C4 - 4 IN COMMON BRICK,  !- Name",
         "  Rough,                   !- Roughness", "  0.1014984,               !- Thickness {m}",
         "  0.7264224,               !- Conductivity {W/m-K}", "  1922.216,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}", "  0.9000000,               !- Thermal Absorptance",
         "  0.7600000,               !- Solar Absorptance", "  0.7600000;               !- Visible Absorptance", "Material,",
         "  E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Name", "  Smooth,                  !- Roughness", "  1.905E-02,               !- Thickness {m}",
         "  0.7264224,               !- Conductivity {W/m-K}", "  1601.846,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}", "  0.9000000,               !- Thermal Absorptance",
         "  0.9200000,               !- Solar Absorptance", "  0.9200000;               !- Visible Absorptance", "Material,",
         "  C6 - 8 IN CLAY TILE,     !- Name", "  Smooth,                  !- Roughness", "  0.2033016,               !- Thickness {m}",
         "  0.5707605,               !- Conductivity {W/m-K}", "  1121.292,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}", "  0.9000000,               !- Thermal Absorptance",
         "  0.8200000,               !- Solar Absorptance", "  0.8200000;               !- Visible Absorptance", "Material,",
         "  C10 - 8 IN HW CONCRETE,  !- Name", "  MediumRough,             !- Roughness", "  0.2033016,               !- Thickness {m}",
         "  1.729577,                !- Conductivity {W/m-K}", "  2242.585,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}", "  0.9000000,               !- Thermal Absorptance",
         "  0.6500000,               !- Solar Absorptance", "  0.6500000;               !- Visible Absorptance", "Material,",
         "  E2 - 1 / 2 IN SLAG OR STONE,  !- Name", "  Rough,                   !- Roughness", "  1.2710161E-02,           !- Thickness {m}",
         "  1.435549,                !- Conductivity {W/m-K}", "  881.0155,                !- Density {kg/m3}",
         "  1673.600,                !- Specific Heat {J/kg-K}", "  0.9000000,               !- Thermal Absorptance",
         "  0.5500000,               !- Solar Absorptance", "  0.5500000;               !- Visible Absorptance", "Material,",
         "  E3 - 3 / 8 IN FELT AND MEMBRANE,  !- Name", "  Rough,                   !- Roughness", "  9.5402403E-03,           !- Thickness {m}",
         "  0.1902535,               !- Conductivity {W/m-K}", "  1121.292,                !- Density {kg/m3}",
         "  1673.600,                !- Specific Heat {J/kg-K}", "  0.9000000,               !- Thermal Absorptance",
         "  0.7500000,               !- Solar Absorptance", "  0.7500000;               !- Visible Absorptance", "Material,",
         "  B5 - 1 IN DENSE INSULATION,  !- Name", "  VeryRough,               !- Roughness", "  2.5389841E-02,           !- Thickness {m}",
         "  4.3239430E-02,           !- Conductivity {W/m-K}", "  91.30524,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}", "  0.9000000,               !- Thermal Absorptance",
         "  0.5000000,               !- Solar Absorptance", "  0.5000000;               !- Visible Absorptance", "Material,",
         "  C12 - 2 IN HW CONCRETE,  !- Name", "  MediumRough,             !- Roughness", "  5.0901599E-02,           !- Thickness {m}",
         "  1.729577,                !- Conductivity {W/m-K}", "  2242.585,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}", "  0.9000000,               !- Thermal Absorptance",
         "  0.6500000,               !- Solar Absorptance", "  0.6500000;               !- Visible Absorptance", "Material,",
         "  1.375in-Solid-Core,      !- Name", "  Smooth,                  !- Roughness", "  3.4925E-02,              !- Thickness {m}",
         "  0.1525000,               !- Conductivity {W/m-K}", "  614.5000,                !- Density {kg/m3}",
         "  1630.0000,               !- Specific Heat {J/kg-K}", "  0.9000000,               !- Thermal Absorptance",
         "  0.9200000,               !- Solar Absorptance", "  0.9200000;               !- Visible Absorptance", "WindowMaterial:Glazing,",
         "  WIN-LAY-GLASS-LIGHT,     !- Name", "  SpectralAverage,         !- Optical Data Type",
         "  ,                        !- Window Glass Spectral Data Set Name", "  0.0025,                  !- Thickness {m}",
         "  0.850,                   !- Solar Transmittance at Normal Incidence",
         "  0.075,                   !- Front Side Solar Reflectance at Normal Incidence",
         "  0.075,                   !- Back Side Solar Reflectance at Normal Incidence",
         "  0.901,                   !- Visible Transmittance at Normal Incidence",
         "  0.081,                   !- Front Side Visible Reflectance at Normal Incidence",
         "  0.081,                   !- Back Side Visible Reflectance at Normal Incidence",
         "  0.0,                     !- Infrared Transmittance at Normal Incidence",
         "  0.84,                    !- Front Side Infrared Hemispherical Emissivity",
         "  0.84,                    !- Back Side Infrared Hemispherical Emissivity", "  0.9;                     !- Conductivity {W/m-K}",
         "Construction,", "  DOOR-CON,                !- Name", "  1.375in-Solid-Core;      !- Outside Layer", "Construction,",
         "  EXTWALL80,               !- Name", "  A1 - 1 IN STUCCO,        !- Outside Layer", "  C4 - 4 IN COMMON BRICK,  !- Layer 2",
         "  E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 3", "Construction,", "  PARTITION06,             !- Name",
         "  E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Outside Layer", "  C6 - 8 IN CLAY TILE,     !- Layer 2",
         "  E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 3", "  Construction,", "  FLOOR SLAB 8 IN,         !- Name",
         "  C10 - 8 IN HW CONCRETE;  !- Outside Layer", "Construction,", "  ROOF34,                  !- Name",
         "  E2 - 1 / 2 IN SLAG OR STONE,  !- Outside Layer", "  E3 - 3 / 8 IN FELT AND MEMBRANE,  !- Layer 2",
         "  B5 - 1 IN DENSE INSULATION,  !- Layer 3", "  C12 - 2 IN HW CONCRETE;  !- Layer 4", "Construction,", "  WIN-CON-LIGHT,           !- Name",
         "  WIN-LAY-GLASS-LIGHT;     !- Outside Layer", "Zone,", "  WEST_ZONE,               !- Name",
         "  0,                       !- Direction of Relative North {deg}", "  0,                       !- X Origin {m}",
         "  0,                       !- Y Origin {m}", "  0,                       !- Z Origin {m}", "  1,                       !- Type",
         "  1,                       !- Multiplier", "  autocalculate;           !- Ceiling Height {m}", "Zone,",
         "  EAST_ZONE,               !- Name", "  0,                       !- Direction of Relative North {deg}",
         "  0,                       !- X Origin {m}", "  0,                       !- Y Origin {m}", "  0,                       !- Z Origin {m}",
         "  1,                       !- Type", "  1,                       !- Multiplier", "  autocalculate;           !- Ceiling Height {m}",
         "Zone,", "  NORTH_ZONE,              !- Name", "  0,                       !- Direction of Relative North {deg}",
         "  0,                       !- X Origin {m}", "  0,                       !- Y Origin {m}", "  0,                       !- Z Origin {m}",
         "  1,                       !- Type", "  1,                       !- Multiplier", "  autocalculate;           !- Ceiling Height {m}",
         "GlobalGeometryRules,", "  UpperLeftCorner,         !- Starting Vertex Position", "  CounterClockWise,        !- Vertex Entry Direction",
         "  World;                   !- Coordinate System", "BuildingSurface:Detailed,", "  Surface_1,               !- Name",
         "  WALL,                    !- Surface Type", "  EXTWALL80,               !- Construction Name", "  WEST_ZONE,               !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition", "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure", "  WindExposed,             !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground", "  4,                       !- Number of Vertices",
         "  0,0,3.048000,            !- X,Y,Z ==> Vertex 1 {m}", "  0,0,0,                   !- X,Y,Z ==> Vertex 2 {m}",
         "  6.096000,0,0,            !- X,Y,Z ==> Vertex 3 {m}", "  6.096000,0,3.048000;     !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,",
         "  Surface_2,               !- Name", "  WALL,                    !- Surface Type", "  EXTWALL80,               !- Construction Name",
         "  WEST_ZONE,               !- Zone Name", "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object", "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure", "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices", "  0,6.096000,3.048000,     !- X,Y,Z ==> Vertex 1 {m}",
         "  0,6.096000,0,            !- X,Y,Z ==> Vertex 2 {m}", "  0,0,0,                   !- X,Y,Z ==> Vertex 3 {m}",
         "  0,0,3.048000;            !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,", "  Surface_3,               !- Name",
         "  WALL,                    !- Surface Type", "  PARTITION06,             !- Construction Name", "  WEST_ZONE,               !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition", "  Surface_17,              !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure", "  NoWind,                  !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground", "  4,                       !- Number of Vertices",
         "  6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}", "  6.096000,6.096000,0,     !- X,Y,Z ==> Vertex 2 {m}",
         "  0,6.096000,0,            !- X,Y,Z ==> Vertex 3 {m}", "  0,6.096000,3.048000;     !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,",
         "  Surface_4,               !- Name", "  WALL,                    !- Surface Type", "  PARTITION06,             !- Construction Name",
         "  WEST_ZONE,               !- Zone Name", "  Surface,                 !- Outside Boundary Condition",
         "  Surface_10,              !- Outside Boundary Condition Object", "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure", "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices", "  6.096000,0,3.048000,     !- X,Y,Z ==> Vertex 1 {m}",
         "  6.096000,0,0,            !- X,Y,Z ==> Vertex 2 {m}", "  6.096000,6.096000,0,     !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,", "  Surface_5,               !- Name",
         "  FLOOR,                   !- Surface Type", "  FLOOR SLAB 8 IN,         !- Construction Name", "  WEST_ZONE,               !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition", "  Surface_5,               !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure", "  NoWind,                  !- Wind Exposure",
         "  1.000000,                !- View Factor to Ground", "  4,                       !- Number of Vertices",
         "  0,0,0,                   !- X,Y,Z ==> Vertex 1 {m}", "  0,6.096000,0,            !- X,Y,Z ==> Vertex 2 {m}",
         "  6.096000,6.096000,0,     !- X,Y,Z ==> Vertex 3 {m}", "  6.096000,0,0;            !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,",
         "  Surface_6,               !- Name", "  ROOF,                    !- Surface Type", "  ROOF34,                  !- Construction Name",
         "  WEST_ZONE,               !- Zone Name", "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object", "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure", "  0,                       !- View Factor to Ground",
         "  4,                       !- Number of Vertices", "  0,6.096000,3.048000,     !- X,Y,Z ==> Vertex 1 {m}",
         "  0,0,3.048000,            !- X,Y,Z ==> Vertex 2 {m}", "  6.096000,0,3.048000,     !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,", "  Surface_8,               !- Name",
         "  WALL,                    !- Surface Type", "  EXTWALL80,               !- Construction Name", "  EAST_ZONE,               !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition", "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure", "  WindExposed,             !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground", "  4,                       !- Number of Vertices",
         "  6.096000,0,3.048000,     !- X,Y,Z ==> Vertex 1 {m}", "  6.096000,0,0,            !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,0,0,            !- X,Y,Z ==> Vertex 3 {m}", "  12.19200,0,3.048000;     !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,",
         "  Surface_9,               !- Name", "  WALL,                    !- Surface Type", "  EXTWALL80,               !- Construction Name",
         "  EAST_ZONE,               !- Zone Name", "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object", "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure", "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices", "  12.19200,0,3.048000,     !- X,Y,Z ==> Vertex 1 {m}",
         "  12.19200,0,0,            !- X,Y,Z ==> Vertex 2 {m}", "  12.19200,6.096000,0,     !- X,Y,Z ==> Vertex 3 {m}",
         "  12.19200,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,", "  Surface_10,              !- Name",
         "  WALL,                    !- Surface Type", "  PARTITION06,             !- Construction Name", "  EAST_ZONE,               !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition", "  Surface_4,               !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure", "  NoWind,                  !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground", "  4,                       !- Number of Vertices",
         "  6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}", "  6.096000,6.096000,0,     !- X,Y,Z ==> Vertex 2 {m}",
         "  6.096000,0,0,            !- X,Y,Z ==> Vertex 3 {m}", "  6.096001,0,3.048000;     !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,",
         "  Surface_11,              !- Name", "  WALL,                    !- Surface Type", "  PARTITION06,             !- Construction Name",
         "  EAST_ZONE,               !- Zone Name", "  Surface,                 !- Outside Boundary Condition",
         "  Surface_18,              !- Outside Boundary Condition Object", "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure", "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices", "  12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  12.19200,6.096000,0,     !- X,Y,Z ==> Vertex 2 {m}", "  6.096000,6.096000,0,     !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,", "  Surface_12,              !- Name",
         "  FLOOR,                   !- Surface Type", "  FLOOR SLAB 8 IN,         !- Construction Name", "  EAST_ZONE,               !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition", "  Surface_12,              !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure", "  NoWind,                  !- Wind Exposure",
         "  1.000000,                !- View Factor to Ground", "  4,                       !- Number of Vertices",
         "  6.096000,0,0,            !- X,Y,Z ==> Vertex 1 {m}", "  6.096000,6.096000,0,     !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,6.096000,0,     !- X,Y,Z ==> Vertex 3 {m}", "  12.19200,0,0;            !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,",
         "  Surface_13,              !- Name", "  ROOF,                    !- Surface Type", "  ROOF34,                  !- Construction Name",
         "  EAST_ZONE,               !- Zone Name", "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object", "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure", "  0,                       !- View Factor to Ground",
         "  4,                       !- Number of Vertices", "  6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  6.096000,0,3.048000,     !- X,Y,Z ==> Vertex 2 {m}", "  12.19200,0,3.048000,     !- X,Y,Z ==> Vertex 3 {m}",
         "  12.19200,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,", "  Surface_14,              !- Name",
         "  WALL,                    !- Surface Type", "  EXTWALL80,               !- Construction Name", "  NORTH_ZONE,              !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition", "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure", "  WindExposed,             !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground", "  4,                       !- Number of Vertices",
         "  0,12.19200,3.048000,     !- X,Y,Z ==> Vertex 1 {m}", "  0,12.19200,0,            !- X,Y,Z ==> Vertex 2 {m}",
         "  0,6.096000,0,            !- X,Y,Z ==> Vertex 3 {m}", "  0,6.096000,3.048000;     !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,",
         "  Surface_15,              !- Name", "  WALL,                    !- Surface Type", "  EXTWALL80,               !- Construction Name",
         "  NORTH_ZONE,              !- Zone Name", "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object", "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure", "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices", "  12.19200,12.19200,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  12.19200,12.19200,0,  !- X,Y,Z ==> Vertex 2 {m}", "  0,12.19200,0,  !- X,Y,Z ==> Vertex 3 {m}",
         "  0,12.19200,3.048000;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,", "  Surface_16,              !- Name",
         "  WALL,                    !- Surface Type", "  EXTWALL80,               !- Construction Name", "  NORTH_ZONE,              !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition", "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure", "  WindExposed,             !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground", "  4,                       !- Number of Vertices",
         "  12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}", "  12.19200,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,12.19200,0,  !- X,Y,Z ==> Vertex 3 {m}", "  12.19200,12.19200,3.048000;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,",
         "  Surface_17,              !- Name", "  WALL,                    !- Surface Type", "  PARTITION06,             !- Construction Name",
         "  NORTH_ZONE,              !- Zone Name", "  Surface,                 !- Outside Boundary Condition",
         "  Surface_3,               !- Outside Boundary Condition Object", "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure", "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices", "  0.000,6.096,3.048,  !- X,Y,Z ==> Vertex 1 {m}",
         "  0.000,6.096,0.000,  !- X,Y,Z ==> Vertex 2 {m}", "  6.096,6.096,0.000,  !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096,6.096,3.048;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,", "  Surface_18,              !- Name",
         "  WALL,                    !- Surface Type", "  PARTITION06,             !- Construction Name", "  NORTH_ZONE,              !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition", "  Surface_11,              !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure", "  NoWind,                  !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground", "  4,                       !- Number of Vertices",
         "  6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}", "  6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}", "  12.19200,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,",
         "  Surface_19,              !- Name", "  FLOOR,                   !- Surface Type", "  FLOOR SLAB 8 IN,         !- Construction Name",
         "  NORTH_ZONE,              !- Zone Name", "  Surface,                 !- Outside Boundary Condition",
         "  Surface_19,              !- Outside Boundary Condition Object", "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure", "  1.000000,                !- View Factor to Ground",
         "  4,                       !- Number of Vertices", "  0,6.096000,0,  !- X,Y,Z ==> Vertex 1 {m}",
         "  0,12.19200,0,  !- X,Y,Z ==> Vertex 2 {m}", "  12.19200,12.19200,0,  !- X,Y,Z ==> Vertex 3 {m}",
         "  12.19200,6.096000,0;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,", "  Surface_20,              !- Name",
         "  ROOF,                    !- Surface Type", "  ROOF34,                  !- Construction Name", "  NORTH_ZONE,              !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition", "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure", "  WindExposed,             !- Wind Exposure",
         "  0,                       !- View Factor to Ground", "  4,                       !- Number of Vertices",
         "  0,12.19200,3.048000,  !- X,Y,Z ==> Vertex 1 {m}", "  0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 3 {m}", "  12.19200,12.19200,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
         "AirflowNetwork:SimulationControl,", "  NaturalVentilation,      !- Name", "  MultizoneWithoutDistribution,  !- AirflowNetwork Control",
         "  INPUT,                   !- Wind Pressure Coefficient Type",
         "  ExternalNode,            !- Height Selection for Local Wind Pressure Calculation", "  LOWRISE,                 !- Building Type",
         "  500,                     !- Maximum Number of Iterations {dimensionless}", "  ZeroNodePressures,       !- Initialization Type",
         "  1.0E-05,                 !- Relative Airflow Convergence Tolerance {dimensionless}",
         "  1.0E-06,                 !- Absolute Airflow Convergence Tolerance {kg/s}",
         "  -0.5,                    !- Convergence Acceleration Limit {dimensionless}",
         "  0.0,                     !- Azimuth Angle of Long Axis of Building {deg}",
         "  1.0;                     !- Ratio of Building Width Along Short Axis to Width Along Long Axis", "AirflowNetwork:MultiZone:Zone,",
         "  WEST_ZONE,               !- Zone Name", "  NoVent,                  !- Ventilation Control Mode",
         "  ,                        !- Ventilation Control Zone Temperature Setpoint Schedule Name",
         //"  Temperature,             !- Ventilation Control Mode",
         //"  WindowVentSched,         !- Ventilation Control Zone Temperature Setpoint Schedule Name",
         "  0.3,                     !- Minimum Venting Open Factor {dimensionless}",
         "  5.0,                     !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor {deltaC}",
         "  10.0,                    !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor {deltaC}",
         "  0.0,                     !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor {deltaJ/kg}",
         "  300000.0;                !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor {deltaJ/kg}",
         "AirflowNetwork:MultiZone:Zone,", "  EAST_ZONE,               !- Zone Name", "  NoVent,                  !- Ventilation Control Mode",
         "  ,                        !- Ventilation Control Zone Temperature Setpoint Schedule Name",
         "  1.0,                     !- Minimum Venting Open Factor {dimensionless}",
         "  0.0,                     !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor {deltaC}",
         "  100.0,                   !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor {deltaC}",
         "  0.0,                     !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor {deltaJ/kg}",
         "  300000.0;                !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor {deltaJ/kg}",
         "AirflowNetwork:MultiZone:Zone,", "  NORTH_ZONE,              !- Zone Name", "  NoVent,                  !- Ventilation Control Mode",
         "  ,                        !- Ventilation Control Zone Temperature Setpoint Schedule Name",
         //"  Temperature,             !- Ventilation Control Mode",
         //"  WindowVentSched,         !- Ventilation Control Zone Temperature Setpoint Schedule Name",
         "  1.0,                     !- Minimum Venting Open Factor {dimensionless}",
         "  0.0,                     !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor {deltaC}",
         "  100.0,                   !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor {deltaC}",
         "  0.0,                     !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor {deltaJ/kg}",
         "  300000.0;                !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor {deltaJ/kg}",
         "AirflowNetwork:MultiZone:Surface,", "  Surface_1,               !- Surface Name", "  CR-1,                    !- Leakage Component Name",
         "  SFacade,                 !- External Node Name",
         "  1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}", "AirflowNetwork:MultiZone:Surface,",
         "  Surface_4,               !- Surface Name", "  CR-1,                    !- Leakage Component Name",
         "  ,                        !- External Node Name",
         "  1.0;                     !- Window / Door Opening Factor, or Crack Factor{ dimensionless }", "AirflowNetwork:MultiZone:Surface,",
         "  Surface_11,              !- Surface Name", "  CR-1,                    !- Leakage Component Name",
         "  ,                        !- External Node Name",
         "  1.0;                     !- Window / Door Opening Factor, or Crack Factor{ dimensionless }", "AirflowNetwork:MultiZone:Surface,",
         "  Surface_15,              !- Surface Name", "  CR-1,                    !- Leakage Component Name",
         "  NFacade,                 !- External Node Name",
         "  1.0;                     !- Window / Door Opening Factor, or Crack Factor{ dimensionless }", "AirflowNetwork:MultiZone:ExternalNode,",
         "  NFacade,                 !- Name", "  1.524,                   !- External Node Height{ m }",
         "  NFacade_WPCCurve;        !- Wind Pressure Coefficient Values Object Name", "AirflowNetwork:MultiZone:ExternalNode,",
         "  SFacade,                 !- Name", "  1.524,                   !- External Node Height{ m }",
         "  SFacade_WPCCurve,        !- Wind Pressure Coefficient Values Object Name",
         "  No,                      !- Symmetric Wind Pressure Coefficient Curve", "  Absolute;                !- Wind Angle Type",
         "AirflowNetwork:MultiZone:ReferenceCrackConditions,", "  ReferenceCrackConditions,!- Name",
         "  20.0,                    !- Reference Temperature{ C }", "  101320,                  !- Reference Barometric Pressure{ Pa }",
         "  0.005;                   !- Reference Humidity Ratio{ kgWater / kgDryAir }", "AirflowNetwork:MultiZone:Surface:Crack,",
         "  CR-1,                    !- Name", "  0.01,                    !- Air Mass Flow Coefficient at Reference Conditions{ kg / s }",
         "  0.667,                   !- Air Mass Flow Exponent{ dimensionless }", "  ReferenceCrackConditions;!- Reference Crack Conditions",
         "Table:OneIndependentVariable,", "  NFacade_WPCCurve,        !- Name", "  Linear,                  !- Curve Type",
         "  LinearInterpolationOfTable,  !- Interpolation Method", "  0,                       !- Minimum Value of X",
         "  360,                     !- Maximum Value of X", "  -1,                      !- Minimum Table Output",
         "  1,                       !- Maximum Table Output", "  Dimensionless,           !- Input Unit Type for X",
         "  Dimensionless,           !- Output Unit Type", "  1,                       !- Normalization Reference",
         "  0, 0.60,                 !- X,Y Pair #1", "  30, 0.48,                !- X,Y Pair #2", "  60, 0.04,                !- X,Y Pair #3",
         "  90, -0.56,               !- X,Y Pair #4", "  120, -0.56,              !- X,Y Pair #5", "  150, -0.42,              !- X,Y Pair #6",
         "  180, -0.37,              !- X,Y Pair #7", "  210, -0.42,              !- X,Y Pair #8", "  240, -0.56,              !- X,Y Pair #9",
         "  270, -0.56,              !- X,Y Pair #10", "  300, 0.04,               !- X,Y Pair #11", "  330, 0.48,               !- X,Y Pair #12",
         "  360, 0.60;               !- X,Y Pair #13", "Table:OneIndependentVariable,", "  SFacade_WPCCurve,        !- Name",
         "  Linear,                  !- Curve Type", "  LinearInterpolationOfTable,  !- Interpolation Method",
         "  0,                       !- Minimum Value of X", "  360,                     !- Maximum Value of X",
         "  -1,                      !- Minimum Table Output", "  1,                       !- Maximum Table Output",
         "  Dimensionless,           !- Input Unit Type for X", "  Dimensionless,           !- Output Unit Type",
         "  1,                       !- Normalization Reference", "  0, -0.37,                !- X,Y Pair #1",
         "  30, -0.42,               !- X,Y Pair #2", "  60, -0.56,               !- X,Y Pair #3", "  90, -0.56,               !- X,Y Pair #4",
         "  120, 0.04,               !- X,Y Pair #5", "  150, 0.48,               !- X,Y Pair #6", "  180, 0.60,               !- X,Y Pair #7",
         "  210, 0.48,               !- X,Y Pair #8", "  240, 0.04,               !- X,Y Pair #9", "  270, -0.56,              !- X,Y Pair #10",
         "  300, -0.56,              !- X,Y Pair #11", "  330, -0.42,              !- X,Y Pair #12", "  360, -0.37;              !- X,Y Pair #13",
         "SurfaceConvectionAlgorithm:Inside,TARP;", "SurfaceConvectionAlgorithm:Outside,DOE-2;", "HeatBalanceAlgorithm,ConductionTransferFunction;",
         "ZoneAirHeatBalanceAlgorithm,", "  AnalyticalSolution;      !- Algorithm"});
    ASSERT_TRUE(process_idf(idf_objects));

    bool errors = false;

    HeatBalanceManager::GetMaterialData(errors); // read material data
    EXPECT_FALSE(errors);                        // expect no errors

    HeatBalanceManager::GetConstructData(errors); // read construction data
    EXPECT_FALSE(errors);                         // expect no errors

    HeatBalanceManager::GetZoneData(errors); // read zone data
    EXPECT_FALSE(errors);                    // expect no errors

    // Magic to get surfaces read in correctly
    DataHeatBalance::HeatTransferAlgosUsed.allocate(1);
    DataHeatBalance::HeatTransferAlgosUsed(1) = OverallHeatTransferSolutionAlgo;
    SurfaceGeometry::CosBldgRotAppGonly = 1.0;
    SurfaceGeometry::SinBldgRotAppGonly = 0.0;

    SurfaceGeometry::GetSurfaceData(errors); // setup zone geometry and get zone data
    EXPECT_FALSE(errors);                    // expect no errors

    CurveManager::GetCurveInput();
    EXPECT_EQ(CurveManager::NumCurves, 2);

    AirflowNetworkBalanceManager::GetAirflowNetworkInput();

    // Check the airflow elements
    EXPECT_EQ(2u, DataAirflowNetwork::MultizoneExternalNodeData.size());
    EXPECT_EQ(3u, DataAirflowNetwork::MultizoneZoneData.size());
    EXPECT_EQ(4u, DataAirflowNetwork::MultizoneSurfaceData.size());
    EXPECT_EQ(1u, DataAirflowNetwork::MultizoneSurfaceCrackData.size());
    EXPECT_EQ(2u, DataAirflowNetwork::MultizoneSurfaceStdConditionsCrackData.size());

    EXPECT_EQ(0.0, DataAirflowNetwork::MultizoneExternalNodeData(1).azimuth);
    EXPECT_FALSE(DataAirflowNetwork::MultizoneExternalNodeData(1).symmetricCurve);
    EXPECT_FALSE(DataAirflowNetwork::MultizoneExternalNodeData(1).useRelativeAngle);
    EXPECT_EQ(1, DataAirflowNetwork::MultizoneExternalNodeData(1).curve);

    EXPECT_EQ(180.0, DataAirflowNetwork::MultizoneExternalNodeData(2).azimuth);
    EXPECT_FALSE(DataAirflowNetwork::MultizoneExternalNodeData(2).symmetricCurve);
    EXPECT_FALSE(DataAirflowNetwork::MultizoneExternalNodeData(2).useRelativeAngle);
    EXPECT_EQ(2, DataAirflowNetwork::MultizoneExternalNodeData(2).curve);

    // Set up some environmental parameters
    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::OutDryBulbTemp = 25.0;
    DataEnvironment::WindDir = 105.0;
    DataEnvironment::OutHumRat = 0.0;        // Dry air only
    DataEnvironment::SiteTempGradient = 0.0; // Disconnect z from testing
    DataEnvironment::SiteWindExp = 0.0;      // Disconnect variation by height
    DataEnvironment::WindSpeed = 10.0;

    // Make sure we can compute the right wind pressure
    Real64 rho = Psychrometrics::PsyRhoAirFnPbTdbW(DataEnvironment::OutBaroPress, DataEnvironment::OutDryBulbTemp, DataEnvironment::OutHumRat);
    EXPECT_DOUBLE_EQ(1.1841123742118911, rho);
    Real64 p =
        AirflowNetworkBalanceManager::CalcWindPressure(DataAirflowNetwork::MultizoneExternalNodeData(1).curve, false, false, 0.0, 1.0,
                                                       DataEnvironment::WindDir, DataEnvironment::OutDryBulbTempAt(10.0), DataEnvironment::OutHumRat);
    EXPECT_DOUBLE_EQ(-0.56 * 0.5 * 1.1841123742118911, p);

    // Make sure the reference velocity comes out right
    EXPECT_DOUBLE_EQ(10.0, DataEnvironment::WindSpeedAt(MultizoneExternalNodeData(1).height));

    EXPECT_EQ(5u, DataAirflowNetwork::AirflowNetworkNodeSimu.size());

    // Run the balance routine, for now only to get the pressure set at the external nodes
    AirflowNetworkBalanceManager::CalcAirflowNetworkAirBalance();

    EXPECT_DOUBLE_EQ(-0.56 * 0.5 * 118.41123742118911, DataAirflowNetwork::AirflowNetworkNodeSimu(4).PZ);
    EXPECT_DOUBLE_EQ(-0.26 * 0.5 * 118.41123742118911, DataAirflowNetwork::AirflowNetworkNodeSimu(5).PZ);
}

TEST_F(EnergyPlusFixture, TestExternalNodesWithNoInput)
{
    std::string const idf_objects = delimited_string(
        {"Version,8.6;",
         "Curve:Quartic,",
         "  WindPressureFit,         !- Name",
         "  0.592,                   !- Coefficient1 Constant",
         "  0.0148,                  !- Coefficient2 x",
         "  -.000757,                !- Coefficient3 x**2",
         "  0.00000662,              !- Coefficient4 x**3",
         "  -.0000000169,            !- Coefficient5 x**4",
         "  0,                       !- Minimum Value of x",
         "  180,                     !- Maximum Value of x",
         "  -1,                      !- Minimum Curve Output",
         "  1,                       !- Maximum Curve Output",
         "  Dimensionless,           !- Input Unit Type for X",
         "  Dimensionless;           !- Output Unit Type",
         "Material,",
         "  A1 - 1 IN STUCCO,        !- Name",
         "  Smooth,                  !- Roughness",
         "  2.5389841E-02,           !- Thickness {m}",
         "  0.6918309,               !- Conductivity {W/m-K}",
         "  1858.142,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}",
         "  0.9000000,               !- Thermal Absorptance",
         "  0.9200000,               !- Solar Absorptance",
         "  0.9200000;               !- Visible Absorptance",
         "Material,",
         "  C4 - 4 IN COMMON BRICK,  !- Name",
         "  Rough,                   !- Roughness",
         "  0.1014984,               !- Thickness {m}",
         "  0.7264224,               !- Conductivity {W/m-K}",
         "  1922.216,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}",
         "  0.9000000,               !- Thermal Absorptance",
         "  0.7600000,               !- Solar Absorptance",
         "  0.7600000;               !- Visible Absorptance",
         "Material,",
         "  E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Name",
         "  Smooth,                  !- Roughness",
         "  1.905E-02,               !- Thickness {m}",
         "  0.7264224,               !- Conductivity {W/m-K}",
         "  1601.846,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}",
         "  0.9000000,               !- Thermal Absorptance",
         "  0.9200000,               !- Solar Absorptance",
         "  0.9200000;               !- Visible Absorptance",
         "Material,",
         "  C6 - 8 IN CLAY TILE,     !- Name",
         "  Smooth,                  !- Roughness",
         "  0.2033016,               !- Thickness {m}",
         "  0.5707605,               !- Conductivity {W/m-K}",
         "  1121.292,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}",
         "  0.9000000,               !- Thermal Absorptance",
         "  0.8200000,               !- Solar Absorptance",
         "  0.8200000;               !- Visible Absorptance",
         "Material,",
         "  C10 - 8 IN HW CONCRETE,  !- Name",
         "  MediumRough,             !- Roughness",
         "  0.2033016,               !- Thickness {m}",
         "  1.729577,                !- Conductivity {W/m-K}",
         "  2242.585,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}",
         "  0.9000000,               !- Thermal Absorptance",
         "  0.6500000,               !- Solar Absorptance",
         "  0.6500000;               !- Visible Absorptance",
         "Material,",
         "  E2 - 1 / 2 IN SLAG OR STONE,  !- Name",
         "  Rough,                   !- Roughness",
         "  1.2710161E-02,           !- Thickness {m}",
         "  1.435549,                !- Conductivity {W/m-K}",
         "  881.0155,                !- Density {kg/m3}",
         "  1673.600,                !- Specific Heat {J/kg-K}",
         "  0.9000000,               !- Thermal Absorptance",
         "  0.5500000,               !- Solar Absorptance",
         "  0.5500000;               !- Visible Absorptance",
         "Material,",
         "  E3 - 3 / 8 IN FELT AND MEMBRANE,  !- Name",
         "  Rough,                   !- Roughness",
         "  9.5402403E-03,           !- Thickness {m}",
         "  0.1902535,               !- Conductivity {W/m-K}",
         "  1121.292,                !- Density {kg/m3}",
         "  1673.600,                !- Specific Heat {J/kg-K}",
         "  0.9000000,               !- Thermal Absorptance",
         "  0.7500000,               !- Solar Absorptance",
         "  0.7500000;               !- Visible Absorptance",
         "Material,",
         "  B5 - 1 IN DENSE INSULATION,  !- Name",
         "  VeryRough,               !- Roughness",
         "  2.5389841E-02,           !- Thickness {m}",
         "  4.3239430E-02,           !- Conductivity {W/m-K}",
         "  91.30524,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}",
         "  0.9000000,               !- Thermal Absorptance",
         "  0.5000000,               !- Solar Absorptance",
         "  0.5000000;               !- Visible Absorptance",
         "Material,",
         "  C12 - 2 IN HW CONCRETE,  !- Name",
         "  MediumRough,             !- Roughness",
         "  5.0901599E-02,           !- Thickness {m}",
         "  1.729577,                !- Conductivity {W/m-K}",
         "  2242.585,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}",
         "  0.9000000,               !- Thermal Absorptance",
         "  0.6500000,               !- Solar Absorptance",
         "  0.6500000;               !- Visible Absorptance",
         "Material,",
         "  1.375in-Solid-Core,      !- Name",
         "  Smooth,                  !- Roughness",
         "  3.4925E-02,              !- Thickness {m}",
         "  0.1525000,               !- Conductivity {W/m-K}",
         "  614.5000,                !- Density {kg/m3}",
         "  1630.0000,               !- Specific Heat {J/kg-K}",
         "  0.9000000,               !- Thermal Absorptance",
         "  0.9200000,               !- Solar Absorptance",
         "  0.9200000;               !- Visible Absorptance",
         "WindowMaterial:Glazing,",
         "  WIN-LAY-GLASS-LIGHT,     !- Name",
         "  SpectralAverage,         !- Optical Data Type",
         "  ,                        !- Window Glass Spectral Data Set Name",
         "  0.0025,                  !- Thickness {m}",
         "  0.850,                   !- Solar Transmittance at Normal Incidence",
         "  0.075,                   !- Front Side Solar Reflectance at Normal Incidence",
         "  0.075,                   !- Back Side Solar Reflectance at Normal Incidence",
         "  0.901,                   !- Visible Transmittance at Normal Incidence",
         "  0.081,                   !- Front Side Visible Reflectance at Normal Incidence",
         "  0.081,                   !- Back Side Visible Reflectance at Normal Incidence",
         "  0.0,                     !- Infrared Transmittance at Normal Incidence",
         "  0.84,                    !- Front Side Infrared Hemispherical Emissivity",
         "  0.84,                    !- Back Side Infrared Hemispherical Emissivity",
         "  0.9;                     !- Conductivity {W/m-K}",
         "Construction,",
         "  DOOR-CON,                !- Name",
         "  1.375in-Solid-Core;      !- Outside Layer",
         "Construction,",
         "  EXTWALL80,               !- Name",
         "  A1 - 1 IN STUCCO,        !- Outside Layer",
         "  C4 - 4 IN COMMON BRICK,  !- Layer 2",
         "  E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 3",
         "Construction,",
         "  PARTITION06,             !- Name",
         "  E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Outside Layer",
         "  C6 - 8 IN CLAY TILE,     !- Layer 2",
         "  E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 3",
         "  Construction,",
         "  FLOOR SLAB 8 IN,         !- Name",
         "  C10 - 8 IN HW CONCRETE;  !- Outside Layer",
         "Construction,",
         "  ROOF34,                  !- Name",
         "  E2 - 1 / 2 IN SLAG OR STONE,  !- Outside Layer",
         "  E3 - 3 / 8 IN FELT AND MEMBRANE,  !- Layer 2",
         "  B5 - 1 IN DENSE INSULATION,  !- Layer 3",
         "  C12 - 2 IN HW CONCRETE;  !- Layer 4",
         "Construction,",
         "  WIN-CON-LIGHT,           !- Name",
         "  WIN-LAY-GLASS-LIGHT;     !- Outside Layer",
         "Zone,",
         "  WEST_ZONE,               !- Name",
         "  0,                       !- Direction of Relative North {deg}",
         "  0,                       !- X Origin {m}",
         "  0,                       !- Y Origin {m}",
         "  0,                       !- Z Origin {m}",
         "  1,                       !- Type",
         "  1,                       !- Multiplier",
         "  autocalculate;           !- Ceiling Height {m}",
         "Zone,",
         "  EAST_ZONE,               !- Name",
         "  0,                       !- Direction of Relative North {deg}",
         "  0,                       !- X Origin {m}",
         "  0,                       !- Y Origin {m}",
         "  0,                       !- Z Origin {m}",
         "  1,                       !- Type",
         "  1,                       !- Multiplier",
         "  autocalculate;           !- Ceiling Height {m}",
         "Zone,",
         "  NORTH_ZONE,              !- Name",
         "  0,                       !- Direction of Relative North {deg}",
         "  0,                       !- X Origin {m}",
         "  0,                       !- Y Origin {m}",
         "  0,                       !- Z Origin {m}",
         "  1,                       !- Type",
         "  1,                       !- Multiplier",
         "  autocalculate;           !- Ceiling Height {m}",
         "GlobalGeometryRules,",
         "  UpperLeftCorner,         !- Starting Vertex Position",
         "  CounterClockWise,        !- Vertex Entry Direction",
         "  World;                   !- Coordinate System",
         "BuildingSurface:Detailed,",
         "  Surface_1,               !- Name",
         "  WALL,                    !- Surface Type",
         "  EXTWALL80,               !- Construction Name",
         "  WEST_ZONE,               !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  0,0,3.048000,            !- X,Y,Z ==> Vertex 1 {m}",
         "  0,0,0,                   !- X,Y,Z ==> Vertex 2 {m}",
         "  6.096000,0,0,            !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096000,0,3.048000;     !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_2,               !- Name",
         "  WALL,                    !- Surface Type",
         "  EXTWALL80,               !- Construction Name",
         "  WEST_ZONE,               !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  0,6.096000,3.048000,     !- X,Y,Z ==> Vertex 1 {m}",
         "  0,6.096000,0,            !- X,Y,Z ==> Vertex 2 {m}",
         "  0,0,0,                   !- X,Y,Z ==> Vertex 3 {m}",
         "  0,0,3.048000;            !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_3,               !- Name",
         "  WALL,                    !- Surface Type",
         "  PARTITION06,             !- Construction Name",
         "  WEST_ZONE,               !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition",
         "  Surface_17,              !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  6.096000,6.096000,0,     !- X,Y,Z ==> Vertex 2 {m}",
         "  0,6.096000,0,            !- X,Y,Z ==> Vertex 3 {m}",
         "  0,6.096000,3.048000;     !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_4,               !- Name",
         "  WALL,                    !- Surface Type",
         "  PARTITION06,             !- Construction Name",
         "  WEST_ZONE,               !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition",
         "  Surface_10,              !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  6.096000,0,3.048000,     !- X,Y,Z ==> Vertex 1 {m}",
         "  6.096000,0,0,            !- X,Y,Z ==> Vertex 2 {m}",
         "  6.096000,6.096000,0,     !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_5,               !- Name",
         "  FLOOR,                   !- Surface Type",
         "  FLOOR SLAB 8 IN,         !- Construction Name",
         "  WEST_ZONE,               !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition",
         "  Surface_5,               !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure",
         "  1.000000,                !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  0,0,0,                   !- X,Y,Z ==> Vertex 1 {m}",
         "  0,6.096000,0,            !- X,Y,Z ==> Vertex 2 {m}",
         "  6.096000,6.096000,0,     !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096000,0,0;            !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_6,               !- Name",
         "  ROOF,                    !- Surface Type",
         "  ROOF34,                  !- Construction Name",
         "  WEST_ZONE,               !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  0,                       !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  0,6.096000,3.048000,     !- X,Y,Z ==> Vertex 1 {m}",
         "  0,0,3.048000,            !- X,Y,Z ==> Vertex 2 {m}",
         "  6.096000,0,3.048000,     !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_8,               !- Name",
         "  WALL,                    !- Surface Type",
         "  EXTWALL80,               !- Construction Name",
         "  EAST_ZONE,               !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  6.096000,0,3.048000,     !- X,Y,Z ==> Vertex 1 {m}",
         "  6.096000,0,0,            !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,0,0,            !- X,Y,Z ==> Vertex 3 {m}",
         "  12.19200,0,3.048000;     !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_9,               !- Name",
         "  WALL,                    !- Surface Type",
         "  EXTWALL80,               !- Construction Name",
         "  EAST_ZONE,               !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  12.19200,0,3.048000,     !- X,Y,Z ==> Vertex 1 {m}",
         "  12.19200,0,0,            !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,6.096000,0,     !- X,Y,Z ==> Vertex 3 {m}",
         "  12.19200,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_10,              !- Name",
         "  WALL,                    !- Surface Type",
         "  PARTITION06,             !- Construction Name",
         "  EAST_ZONE,               !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition",
         "  Surface_4,               !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  6.096000,6.096000,0,     !- X,Y,Z ==> Vertex 2 {m}",
         "  6.096000,0,0,            !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096001,0,3.048000;     !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_11,              !- Name",
         "  WALL,                    !- Surface Type",
         "  PARTITION06,             !- Construction Name",
         "  EAST_ZONE,               !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition",
         "  Surface_18,              !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  12.19200,6.096000,0,     !- X,Y,Z ==> Vertex 2 {m}",
         "  6.096000,6.096000,0,     !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_12,              !- Name",
         "  FLOOR,                   !- Surface Type",
         "  FLOOR SLAB 8 IN,         !- Construction Name",
         "  EAST_ZONE,               !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition",
         "  Surface_12,              !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure",
         "  1.000000,                !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  6.096000,0,0,            !- X,Y,Z ==> Vertex 1 {m}",
         "  6.096000,6.096000,0,     !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,6.096000,0,     !- X,Y,Z ==> Vertex 3 {m}",
         "  12.19200,0,0;            !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_13,              !- Name",
         "  ROOF,                    !- Surface Type",
         "  ROOF34,                  !- Construction Name",
         "  EAST_ZONE,               !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  0,                       !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  6.096000,0,3.048000,     !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,0,3.048000,     !- X,Y,Z ==> Vertex 3 {m}",
         "  12.19200,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_14,              !- Name",
         "  WALL,                    !- Surface Type",
         "  EXTWALL80,               !- Construction Name",
         "  NORTH_ZONE,              !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  0,12.19200,3.048000,     !- X,Y,Z ==> Vertex 1 {m}",
         "  0,12.19200,0,            !- X,Y,Z ==> Vertex 2 {m}",
         "  0,6.096000,0,            !- X,Y,Z ==> Vertex 3 {m}",
         "  0,6.096000,3.048000;     !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_15,              !- Name",
         "  WALL,                    !- Surface Type",
         "  EXTWALL80,               !- Construction Name",
         "  NORTH_ZONE,              !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  12.19200,12.19200,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  12.19200,12.19200,0,  !- X,Y,Z ==> Vertex 2 {m}",
         "  0,12.19200,0,  !- X,Y,Z ==> Vertex 3 {m}",
         "  0,12.19200,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_16,              !- Name",
         "  WALL,                    !- Surface Type",
         "  EXTWALL80,               !- Construction Name",
         "  NORTH_ZONE,              !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  12.19200,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,12.19200,0,  !- X,Y,Z ==> Vertex 3 {m}",
         "  12.19200,12.19200,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_17,              !- Name",
         "  WALL,                    !- Surface Type",
         "  PARTITION06,             !- Construction Name",
         "  NORTH_ZONE,              !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition",
         "  Surface_3,               !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  0.000,6.096,3.048,  !- X,Y,Z ==> Vertex 1 {m}",
         "  0.000,6.096,0.000,  !- X,Y,Z ==> Vertex 2 {m}",
         "  6.096,6.096,0.000,  !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096,6.096,3.048;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_18,              !- Name",
         "  WALL,                    !- Surface Type",
         "  PARTITION06,             !- Construction Name",
         "  NORTH_ZONE,              !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition",
         "  Surface_11,              !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
         "  12.19200,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_19,              !- Name",
         "  FLOOR,                   !- Surface Type",
         "  FLOOR SLAB 8 IN,         !- Construction Name",
         "  NORTH_ZONE,              !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition",
         "  Surface_19,              !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure",
         "  1.000000,                !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  0,6.096000,0,  !- X,Y,Z ==> Vertex 1 {m}",
         "  0,12.19200,0,  !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,12.19200,0,  !- X,Y,Z ==> Vertex 3 {m}",
         "  12.19200,6.096000,0;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_20,              !- Name",
         "  ROOF,                    !- Surface Type",
         "  ROOF34,                  !- Construction Name",
         "  NORTH_ZONE,              !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  0,                       !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  0,12.19200,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
         "  12.19200,12.19200,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
         "AirflowNetwork:SimulationControl,",
         "  NaturalVentilation,      !- Name",
         "  MultizoneWithoutDistribution,  !- AirflowNetwork Control",
         "  SurfaceAverageCalculation,     !- Wind Pressure Coefficient Type",
         "  ExternalNode,            !- Height Selection for Local Wind Pressure Calculation",
         "  LOWRISE,                 !- Building Type",
         "  500,                     !- Maximum Number of Iterations {dimensionless}",
         "  ZeroNodePressures,       !- Initialization Type",
         "  1.0E-05,                 !- Relative Airflow Convergence Tolerance {dimensionless}",
         "  1.0E-06,                 !- Absolute Airflow Convergence Tolerance {kg/s}",
         "  -0.5,                    !- Convergence Acceleration Limit {dimensionless}",
         "  0.0,                     !- Azimuth Angle of Long Axis of Building {deg}",
         "  1.0;                     !- Ratio of Building Width Along Short Axis to Width Along Long Axis",
         "AirflowNetwork:MultiZone:Zone,",
         "  WEST_ZONE,               !- Zone Name",
         "  NoVent,                  !- Ventilation Control Mode",
         "  ,                        !- Ventilation Control Zone Temperature Setpoint Schedule Name",
         "  0.3,                     !- Minimum Venting Open Factor {dimensionless}",
         "  5.0,                     !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor {deltaC}",
         "  10.0,                    !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor {deltaC}",
         "  0.0,                     !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor {deltaJ/kg}",
         "  300000.0;                !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor {deltaJ/kg}",
         "AirflowNetwork:MultiZone:Zone,",
         "  EAST_ZONE,               !- Zone Name",
         "  NoVent,                  !- Ventilation Control Mode",
         "  ,                        !- Ventilation Control Zone Temperature Setpoint Schedule Name",
         "  1.0,                     !- Minimum Venting Open Factor {dimensionless}",
         "  0.0,                     !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor {deltaC}",
         "  100.0,                   !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor {deltaC}",
         "  0.0,                     !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor {deltaJ/kg}",
         "  300000.0;                !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor {deltaJ/kg}",
         "AirflowNetwork:MultiZone:Zone,",
         "  NORTH_ZONE,              !- Zone Name",
         "  NoVent,                  !- Ventilation Control Mode",
         "  ,                        !- Ventilation Control Zone Temperature Setpoint Schedule Name",
         "  1.0,                     !- Minimum Venting Open Factor {dimensionless}",
         "  0.0,                     !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor {deltaC}",
         "  100.0,                   !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor {deltaC}",
         "  0.0,                     !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor {deltaJ/kg}",
         "  300000.0;                !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor {deltaJ/kg}",
         "AirflowNetwork:MultiZone:Surface,",
         "  Surface_1,               !- Surface Name",
         "  CR-1,                    !- Leakage Component Name",
         "  ,                        !- External Node Name",
         "  1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",
         "AirflowNetwork:MultiZone:Surface,",
         "  Surface_4,               !- Surface Name",
         "  CR-1,                    !- Leakage Component Name",
         "  ,                        !- External Node Name",
         "  1.0;                     !- Window / Door Opening Factor, or Crack Factor{ dimensionless }",
         "AirflowNetwork:MultiZone:Surface,",
         "  Surface_11,              !- Surface Name",
         "  CR-1,                    !- Leakage Component Name",
         "  ,                        !- External Node Name",
         "  1.0;                     !- Window / Door Opening Factor, or Crack Factor{ dimensionless }",
         "AirflowNetwork:MultiZone:Surface,",
         "  Surface_15,              !- Surface Name",
         "  CR-1,                    !- Leakage Component Name",
         "  ,                        !- External Node Name",
         "  1.0;                     !- Window / Door Opening Factor, or Crack Factor{ dimensionless }",
         "AirflowNetwork:MultiZone:ReferenceCrackConditions,",
         "  ReferenceCrackConditions,!- Name",
         "  20.0,                    !- Reference Temperature{ C }",
         "  101320,                  !- Reference Barometric Pressure{ Pa }",
         "  0.005;                   !- Reference Humidity Ratio{ kgWater / kgDryAir }",
         "AirflowNetwork:MultiZone:Surface:Crack,",
         "  CR-1,                    !- Name",
         "  0.01,                    !- Air Mass Flow Coefficient at Reference Conditions{ kg / s }",
         "  0.667,                   !- Air Mass Flow Exponent{ dimensionless }",
         "  ReferenceCrackConditions;!- Reference Crack Conditions",
         "SurfaceConvectionAlgorithm:Inside,TARP;",
         "SurfaceConvectionAlgorithm:Outside,DOE-2;",
         "HeatBalanceAlgorithm,ConductionTransferFunction;",
         "ZoneAirHeatBalanceAlgorithm,",
         "  AnalyticalSolution;      !- Algorithm"});
    ASSERT_TRUE(process_idf(idf_objects));

    bool errors = false;

    HeatBalanceManager::GetMaterialData(errors); // read material data
    EXPECT_FALSE(errors);                        // expect no errors

    HeatBalanceManager::GetConstructData(errors); // read construction data
    EXPECT_FALSE(errors);                         // expect no errors

    HeatBalanceManager::GetZoneData(errors); // read zone data
    EXPECT_FALSE(errors);                    // expect no errors

    // Magic to get surfaces read in correctly
    DataHeatBalance::HeatTransferAlgosUsed.allocate(1);
    DataHeatBalance::HeatTransferAlgosUsed(1) = OverallHeatTransferSolutionAlgo;
    SurfaceGeometry::CosBldgRotAppGonly = 1.0;
    SurfaceGeometry::SinBldgRotAppGonly = 0.0;

    SurfaceGeometry::GetSurfaceData(errors); // setup zone geometry and get zone data
    EXPECT_FALSE(errors);                    // expect no errors

    CurveManager::GetCurveInput();
    EXPECT_EQ(CurveManager::NumCurves, 1);

    AirflowNetworkBalanceManager::GetAirflowNetworkInput();

    EXPECT_EQ(CurveManager::NumCurves, 6);

    // Check the curves
    Real64 cp105N = -0.5 * (0.44267457181949038 + 0.68051108580039887);
    Real64 cp105S = 0.5 * (0.11880548415819636 - 0.44267457181949038);
    EXPECT_DOUBLE_EQ(0.60345944298105458, CurveManager::CurveValue(2, 0));     // In-range value
    EXPECT_DOUBLE_EQ(cp105N, CurveManager::CurveValue(2, 105));                // In-range value
    EXPECT_DOUBLE_EQ(0.60345944298105458, CurveManager::CurveValue(2, -10.0)); // Minimum x
    EXPECT_DOUBLE_EQ(0.60345944298105458, CurveManager::CurveValue(2, 5000));  // Maximum x
    // Check the other curve
    EXPECT_DOUBLE_EQ(0.592, CurveManager::CurveValue(1, 0)); // In-range value

    // Check the airflow elements
    EXPECT_EQ(2u, DataAirflowNetwork::MultizoneExternalNodeData.size());
    EXPECT_EQ(3u, DataAirflowNetwork::MultizoneZoneData.size());
    EXPECT_EQ(4u, DataAirflowNetwork::MultizoneSurfaceData.size());
    EXPECT_EQ(1u, DataAirflowNetwork::MultizoneSurfaceCrackData.size());
    EXPECT_EQ(2u, DataAirflowNetwork::MultizoneSurfaceStdConditionsCrackData.size());

    EXPECT_EQ(180.0, DataAirflowNetwork::MultizoneExternalNodeData(1).azimuth);
    EXPECT_FALSE(DataAirflowNetwork::MultizoneExternalNodeData(1).symmetricCurve);
    EXPECT_FALSE(DataAirflowNetwork::MultizoneExternalNodeData(1).useRelativeAngle);
    EXPECT_EQ(4, DataAirflowNetwork::MultizoneExternalNodeData(1).curve);

    EXPECT_EQ(0.0, DataAirflowNetwork::MultizoneExternalNodeData(2).azimuth);
    EXPECT_FALSE(DataAirflowNetwork::MultizoneExternalNodeData(2).symmetricCurve);
    EXPECT_FALSE(DataAirflowNetwork::MultizoneExternalNodeData(2).useRelativeAngle);
    EXPECT_EQ(2, DataAirflowNetwork::MultizoneExternalNodeData(2).curve);

    // Set up some environmental parameters
    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::OutDryBulbTemp = 25.0;
    DataEnvironment::WindDir = 105.0;
    DataEnvironment::OutHumRat = 0.0;        // Dry air only
    DataEnvironment::SiteTempGradient = 0.0; // Disconnect z from testing
    DataEnvironment::SiteWindExp = 0.0;      // Disconnect variation by height
    DataEnvironment::WindSpeed = 10.0;

    // Make sure we can compute the right wind pressure
    Real64 rho = Psychrometrics::PsyRhoAirFnPbTdbW(DataEnvironment::OutBaroPress, DataEnvironment::OutDryBulbTemp, DataEnvironment::OutHumRat);
    EXPECT_DOUBLE_EQ(1.1841123742118911, rho);
    Real64 p =
        AirflowNetworkBalanceManager::CalcWindPressure(DataAirflowNetwork::MultizoneExternalNodeData(2).curve, false, false, 0.0, 1.0,
                                                       DataEnvironment::WindDir, DataEnvironment::OutDryBulbTempAt(10.0), DataEnvironment::OutHumRat);
    EXPECT_DOUBLE_EQ(cp105N * 0.5 * 1.1841123742118911, p);
    p = AirflowNetworkBalanceManager::CalcWindPressure(DataAirflowNetwork::MultizoneExternalNodeData(1).curve, false, false, 0.0, 1.0,
                                                       DataEnvironment::WindDir, DataEnvironment::OutDryBulbTempAt(10.0), DataEnvironment::OutHumRat);
    EXPECT_DOUBLE_EQ(cp105S * 0.5 * 1.1841123742118911, p);

    // Make sure the reference velocity comes out right
    EXPECT_DOUBLE_EQ(10.0, DataEnvironment::WindSpeedAt(MultizoneExternalNodeData(1).height));

    EXPECT_EQ(5u, DataAirflowNetwork::AirflowNetworkNodeSimu.size());

    // Run the balance routine, for now only to get the pressure set at the external nodes
    AirflowNetworkBalanceManager::CalcAirflowNetworkAirBalance();

    EXPECT_DOUBLE_EQ(cp105N * 0.5 * 118.41123742118911, DataAirflowNetwork::AirflowNetworkNodeSimu(5).PZ);
    EXPECT_DOUBLE_EQ(cp105S * 0.5 * 118.41123742118911, DataAirflowNetwork::AirflowNetworkNodeSimu(4).PZ);
}

TEST_F(EnergyPlusFixture, TestExternalNodesWithSymmetricTable)
{
    std::string const idf_objects = delimited_string(
        {"Version,8.6;", "Material,", "  A1 - 1 IN STUCCO,        !- Name", "  Smooth,                  !- Roughness",
         "  2.5389841E-02,           !- Thickness {m}", "  0.6918309,               !- Conductivity {W/m-K}",
         "  1858.142,                !- Density {kg/m3}", "  836.8000,                !- Specific Heat {J/kg-K}",
         "  0.9000000,               !- Thermal Absorptance", "  0.9200000,               !- Solar Absorptance",
         "  0.9200000;               !- Visible Absorptance", "Material,", "  C4 - 4 IN COMMON BRICK,  !- Name",
         "  Rough,                   !- Roughness", "  0.1014984,               !- Thickness {m}",
         "  0.7264224,               !- Conductivity {W/m-K}", "  1922.216,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}", "  0.9000000,               !- Thermal Absorptance",
         "  0.7600000,               !- Solar Absorptance", "  0.7600000;               !- Visible Absorptance", "Material,",
         "  E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Name", "  Smooth,                  !- Roughness", "  1.905E-02,               !- Thickness {m}",
         "  0.7264224,               !- Conductivity {W/m-K}", "  1601.846,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}", "  0.9000000,               !- Thermal Absorptance",
         "  0.9200000,               !- Solar Absorptance", "  0.9200000;               !- Visible Absorptance", "Material,",
         "  C6 - 8 IN CLAY TILE,     !- Name", "  Smooth,                  !- Roughness", "  0.2033016,               !- Thickness {m}",
         "  0.5707605,               !- Conductivity {W/m-K}", "  1121.292,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}", "  0.9000000,               !- Thermal Absorptance",
         "  0.8200000,               !- Solar Absorptance", "  0.8200000;               !- Visible Absorptance", "Material,",
         "  C10 - 8 IN HW CONCRETE,  !- Name", "  MediumRough,             !- Roughness", "  0.2033016,               !- Thickness {m}",
         "  1.729577,                !- Conductivity {W/m-K}", "  2242.585,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}", "  0.9000000,               !- Thermal Absorptance",
         "  0.6500000,               !- Solar Absorptance", "  0.6500000;               !- Visible Absorptance", "Material,",
         "  E2 - 1 / 2 IN SLAG OR STONE,  !- Name", "  Rough,                   !- Roughness", "  1.2710161E-02,           !- Thickness {m}",
         "  1.435549,                !- Conductivity {W/m-K}", "  881.0155,                !- Density {kg/m3}",
         "  1673.600,                !- Specific Heat {J/kg-K}", "  0.9000000,               !- Thermal Absorptance",
         "  0.5500000,               !- Solar Absorptance", "  0.5500000;               !- Visible Absorptance", "Material,",
         "  E3 - 3 / 8 IN FELT AND MEMBRANE,  !- Name", "  Rough,                   !- Roughness", "  9.5402403E-03,           !- Thickness {m}",
         "  0.1902535,               !- Conductivity {W/m-K}", "  1121.292,                !- Density {kg/m3}",
         "  1673.600,                !- Specific Heat {J/kg-K}", "  0.9000000,               !- Thermal Absorptance",
         "  0.7500000,               !- Solar Absorptance", "  0.7500000;               !- Visible Absorptance", "Material,",
         "  B5 - 1 IN DENSE INSULATION,  !- Name", "  VeryRough,               !- Roughness", "  2.5389841E-02,           !- Thickness {m}",
         "  4.3239430E-02,           !- Conductivity {W/m-K}", "  91.30524,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}", "  0.9000000,               !- Thermal Absorptance",
         "  0.5000000,               !- Solar Absorptance", "  0.5000000;               !- Visible Absorptance", "Material,",
         "  C12 - 2 IN HW CONCRETE,  !- Name", "  MediumRough,             !- Roughness", "  5.0901599E-02,           !- Thickness {m}",
         "  1.729577,                !- Conductivity {W/m-K}", "  2242.585,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}", "  0.9000000,               !- Thermal Absorptance",
         "  0.6500000,               !- Solar Absorptance", "  0.6500000;               !- Visible Absorptance", "Material,",
         "  1.375in-Solid-Core,      !- Name", "  Smooth,                  !- Roughness", "  3.4925E-02,              !- Thickness {m}",
         "  0.1525000,               !- Conductivity {W/m-K}", "  614.5000,                !- Density {kg/m3}",
         "  1630.0000,               !- Specific Heat {J/kg-K}", "  0.9000000,               !- Thermal Absorptance",
         "  0.9200000,               !- Solar Absorptance", "  0.9200000;               !- Visible Absorptance", "WindowMaterial:Glazing,",
         "  WIN-LAY-GLASS-LIGHT,     !- Name", "  SpectralAverage,         !- Optical Data Type",
         "  ,                        !- Window Glass Spectral Data Set Name", "  0.0025,                  !- Thickness {m}",
         "  0.850,                   !- Solar Transmittance at Normal Incidence",
         "  0.075,                   !- Front Side Solar Reflectance at Normal Incidence",
         "  0.075,                   !- Back Side Solar Reflectance at Normal Incidence",
         "  0.901,                   !- Visible Transmittance at Normal Incidence",
         "  0.081,                   !- Front Side Visible Reflectance at Normal Incidence",
         "  0.081,                   !- Back Side Visible Reflectance at Normal Incidence",
         "  0.0,                     !- Infrared Transmittance at Normal Incidence",
         "  0.84,                    !- Front Side Infrared Hemispherical Emissivity",
         "  0.84,                    !- Back Side Infrared Hemispherical Emissivity", "  0.9;                     !- Conductivity {W/m-K}",
         "Construction,", "  DOOR-CON,                !- Name", "  1.375in-Solid-Core;      !- Outside Layer", "Construction,",
         "  EXTWALL80,               !- Name", "  A1 - 1 IN STUCCO,        !- Outside Layer", "  C4 - 4 IN COMMON BRICK,  !- Layer 2",
         "  E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 3", "Construction,", "  PARTITION06,             !- Name",
         "  E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Outside Layer", "  C6 - 8 IN CLAY TILE,     !- Layer 2",
         "  E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 3", "  Construction,", "  FLOOR SLAB 8 IN,         !- Name",
         "  C10 - 8 IN HW CONCRETE;  !- Outside Layer", "Construction,", "  ROOF34,                  !- Name",
         "  E2 - 1 / 2 IN SLAG OR STONE,  !- Outside Layer", "  E3 - 3 / 8 IN FELT AND MEMBRANE,  !- Layer 2",
         "  B5 - 1 IN DENSE INSULATION,  !- Layer 3", "  C12 - 2 IN HW CONCRETE;  !- Layer 4", "Construction,", "  WIN-CON-LIGHT,           !- Name",
         "  WIN-LAY-GLASS-LIGHT;     !- Outside Layer", "Zone,", "  WEST_ZONE,               !- Name",
         "  0,                       !- Direction of Relative North {deg}", "  0,                       !- X Origin {m}",
         "  0,                       !- Y Origin {m}", "  0,                       !- Z Origin {m}", "  1,                       !- Type",
         "  1,                       !- Multiplier", "  autocalculate;           !- Ceiling Height {m}", "Zone,",
         "  EAST_ZONE,               !- Name", "  0,                       !- Direction of Relative North {deg}",
         "  0,                       !- X Origin {m}", "  0,                       !- Y Origin {m}", "  0,                       !- Z Origin {m}",
         "  1,                       !- Type", "  1,                       !- Multiplier", "  autocalculate;           !- Ceiling Height {m}",
         "Zone,", "  NORTH_ZONE,              !- Name", "  0,                       !- Direction of Relative North {deg}",
         "  0,                       !- X Origin {m}", "  0,                       !- Y Origin {m}", "  0,                       !- Z Origin {m}",
         "  1,                       !- Type", "  1,                       !- Multiplier", "  autocalculate;           !- Ceiling Height {m}",
         "GlobalGeometryRules,", "  UpperLeftCorner,         !- Starting Vertex Position", "  CounterClockWise,        !- Vertex Entry Direction",
         "  World;                   !- Coordinate System", "BuildingSurface:Detailed,", "  Surface_1,               !- Name",
         "  WALL,                    !- Surface Type", "  EXTWALL80,               !- Construction Name", "  WEST_ZONE,               !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition", "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure", "  WindExposed,             !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground", "  4,                       !- Number of Vertices",
         "  0,0,3.048000,            !- X,Y,Z ==> Vertex 1 {m}", "  0,0,0,                   !- X,Y,Z ==> Vertex 2 {m}",
         "  6.096000,0,0,            !- X,Y,Z ==> Vertex 3 {m}", "  6.096000,0,3.048000;     !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,",
         "  Surface_2,               !- Name", "  WALL,                    !- Surface Type", "  EXTWALL80,               !- Construction Name",
         "  WEST_ZONE,               !- Zone Name", "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object", "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure", "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices", "  0,6.096000,3.048000,     !- X,Y,Z ==> Vertex 1 {m}",
         "  0,6.096000,0,            !- X,Y,Z ==> Vertex 2 {m}", "  0,0,0,                   !- X,Y,Z ==> Vertex 3 {m}",
         "  0,0,3.048000;            !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,", "  Surface_3,               !- Name",
         "  WALL,                    !- Surface Type", "  PARTITION06,             !- Construction Name", "  WEST_ZONE,               !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition", "  Surface_17,              !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure", "  NoWind,                  !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground", "  4,                       !- Number of Vertices",
         "  6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}", "  6.096000,6.096000,0,     !- X,Y,Z ==> Vertex 2 {m}",
         "  0,6.096000,0,            !- X,Y,Z ==> Vertex 3 {m}", "  0,6.096000,3.048000;     !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,",
         "  Surface_4,               !- Name", "  WALL,                    !- Surface Type", "  PARTITION06,             !- Construction Name",
         "  WEST_ZONE,               !- Zone Name", "  Surface,                 !- Outside Boundary Condition",
         "  Surface_10,              !- Outside Boundary Condition Object", "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure", "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices", "  6.096000,0,3.048000,     !- X,Y,Z ==> Vertex 1 {m}",
         "  6.096000,0,0,            !- X,Y,Z ==> Vertex 2 {m}", "  6.096000,6.096000,0,     !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,", "  Surface_5,               !- Name",
         "  FLOOR,                   !- Surface Type", "  FLOOR SLAB 8 IN,         !- Construction Name", "  WEST_ZONE,               !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition", "  Surface_5,               !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure", "  NoWind,                  !- Wind Exposure",
         "  1.000000,                !- View Factor to Ground", "  4,                       !- Number of Vertices",
         "  0,0,0,                   !- X,Y,Z ==> Vertex 1 {m}", "  0,6.096000,0,            !- X,Y,Z ==> Vertex 2 {m}",
         "  6.096000,6.096000,0,     !- X,Y,Z ==> Vertex 3 {m}", "  6.096000,0,0;            !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,",
         "  Surface_6,               !- Name", "  ROOF,                    !- Surface Type", "  ROOF34,                  !- Construction Name",
         "  WEST_ZONE,               !- Zone Name", "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object", "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure", "  0,                       !- View Factor to Ground",
         "  4,                       !- Number of Vertices", "  0,6.096000,3.048000,     !- X,Y,Z ==> Vertex 1 {m}",
         "  0,0,3.048000,            !- X,Y,Z ==> Vertex 2 {m}", "  6.096000,0,3.048000,     !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,", "  Surface_8,               !- Name",
         "  WALL,                    !- Surface Type", "  EXTWALL80,               !- Construction Name", "  EAST_ZONE,               !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition", "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure", "  WindExposed,             !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground", "  4,                       !- Number of Vertices",
         "  6.096000,0,3.048000,     !- X,Y,Z ==> Vertex 1 {m}", "  6.096000,0,0,            !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,0,0,            !- X,Y,Z ==> Vertex 3 {m}", "  12.19200,0,3.048000;     !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,",
         "  Surface_9,               !- Name", "  WALL,                    !- Surface Type", "  EXTWALL80,               !- Construction Name",
         "  EAST_ZONE,               !- Zone Name", "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object", "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure", "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices", "  12.19200,0,3.048000,     !- X,Y,Z ==> Vertex 1 {m}",
         "  12.19200,0,0,            !- X,Y,Z ==> Vertex 2 {m}", "  12.19200,6.096000,0,     !- X,Y,Z ==> Vertex 3 {m}",
         "  12.19200,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,", "  Surface_10,              !- Name",
         "  WALL,                    !- Surface Type", "  PARTITION06,             !- Construction Name", "  EAST_ZONE,               !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition", "  Surface_4,               !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure", "  NoWind,                  !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground", "  4,                       !- Number of Vertices",
         "  6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}", "  6.096000,6.096000,0,     !- X,Y,Z ==> Vertex 2 {m}",
         "  6.096000,0,0,            !- X,Y,Z ==> Vertex 3 {m}", "  6.096001,0,3.048000;     !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,",
         "  Surface_11,              !- Name", "  WALL,                    !- Surface Type", "  PARTITION06,             !- Construction Name",
         "  EAST_ZONE,               !- Zone Name", "  Surface,                 !- Outside Boundary Condition",
         "  Surface_18,              !- Outside Boundary Condition Object", "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure", "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices", "  12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  12.19200,6.096000,0,     !- X,Y,Z ==> Vertex 2 {m}", "  6.096000,6.096000,0,     !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,", "  Surface_12,              !- Name",
         "  FLOOR,                   !- Surface Type", "  FLOOR SLAB 8 IN,         !- Construction Name", "  EAST_ZONE,               !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition", "  Surface_12,              !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure", "  NoWind,                  !- Wind Exposure",
         "  1.000000,                !- View Factor to Ground", "  4,                       !- Number of Vertices",
         "  6.096000,0,0,            !- X,Y,Z ==> Vertex 1 {m}", "  6.096000,6.096000,0,     !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,6.096000,0,     !- X,Y,Z ==> Vertex 3 {m}", "  12.19200,0,0;            !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,",
         "  Surface_13,              !- Name", "  ROOF,                    !- Surface Type", "  ROOF34,                  !- Construction Name",
         "  EAST_ZONE,               !- Zone Name", "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object", "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure", "  0,                       !- View Factor to Ground",
         "  4,                       !- Number of Vertices", "  6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  6.096000,0,3.048000,     !- X,Y,Z ==> Vertex 2 {m}", "  12.19200,0,3.048000,     !- X,Y,Z ==> Vertex 3 {m}",
         "  12.19200,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,", "  Surface_14,              !- Name",
         "  WALL,                    !- Surface Type", "  EXTWALL80,               !- Construction Name", "  NORTH_ZONE,              !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition", "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure", "  WindExposed,             !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground", "  4,                       !- Number of Vertices",
         "  0,12.19200,3.048000,     !- X,Y,Z ==> Vertex 1 {m}", "  0,12.19200,0,            !- X,Y,Z ==> Vertex 2 {m}",
         "  0,6.096000,0,            !- X,Y,Z ==> Vertex 3 {m}", "  0,6.096000,3.048000;     !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,",
         "  Surface_15,              !- Name", "  WALL,                    !- Surface Type", "  EXTWALL80,               !- Construction Name",
         "  NORTH_ZONE,              !- Zone Name", "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object", "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure", "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices", "  12.19200,12.19200,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  12.19200,12.19200,0,  !- X,Y,Z ==> Vertex 2 {m}", "  0,12.19200,0,  !- X,Y,Z ==> Vertex 3 {m}",
         "  0,12.19200,3.048000;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,", "  Surface_16,              !- Name",
         "  WALL,                    !- Surface Type", "  EXTWALL80,               !- Construction Name", "  NORTH_ZONE,              !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition", "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure", "  WindExposed,             !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground", "  4,                       !- Number of Vertices",
         "  12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}", "  12.19200,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,12.19200,0,  !- X,Y,Z ==> Vertex 3 {m}", "  12.19200,12.19200,3.048000;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,",
         "  Surface_17,              !- Name", "  WALL,                    !- Surface Type", "  PARTITION06,             !- Construction Name",
         "  NORTH_ZONE,              !- Zone Name", "  Surface,                 !- Outside Boundary Condition",
         "  Surface_3,               !- Outside Boundary Condition Object", "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure", "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices", "  0.000,6.096,3.048,  !- X,Y,Z ==> Vertex 1 {m}",
         "  0.000,6.096,0.000,  !- X,Y,Z ==> Vertex 2 {m}", "  6.096,6.096,0.000,  !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096,6.096,3.048;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,", "  Surface_18,              !- Name",
         "  WALL,                    !- Surface Type", "  PARTITION06,             !- Construction Name", "  NORTH_ZONE,              !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition", "  Surface_11,              !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure", "  NoWind,                  !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground", "  4,                       !- Number of Vertices",
         "  6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}", "  6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}", "  12.19200,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,",
         "  Surface_19,              !- Name", "  FLOOR,                   !- Surface Type", "  FLOOR SLAB 8 IN,         !- Construction Name",
         "  NORTH_ZONE,              !- Zone Name", "  Surface,                 !- Outside Boundary Condition",
         "  Surface_19,              !- Outside Boundary Condition Object", "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure", "  1.000000,                !- View Factor to Ground",
         "  4,                       !- Number of Vertices", "  0,6.096000,0,  !- X,Y,Z ==> Vertex 1 {m}",
         "  0,12.19200,0,  !- X,Y,Z ==> Vertex 2 {m}", "  12.19200,12.19200,0,  !- X,Y,Z ==> Vertex 3 {m}",
         "  12.19200,6.096000,0;  !- X,Y,Z ==> Vertex 4 {m}", "BuildingSurface:Detailed,", "  Surface_20,              !- Name",
         "  ROOF,                    !- Surface Type", "  ROOF34,                  !- Construction Name", "  NORTH_ZONE,              !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition", "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure", "  WindExposed,             !- Wind Exposure",
         "  0,                       !- View Factor to Ground", "  4,                       !- Number of Vertices",
         "  0,12.19200,3.048000,  !- X,Y,Z ==> Vertex 1 {m}", "  0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 3 {m}", "  12.19200,12.19200,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
         "AirflowNetwork:SimulationControl,", "  NaturalVentilation,      !- Name", "  MultizoneWithoutDistribution,  !- AirflowNetwork Control",
         "  INPUT,                   !- Wind Pressure Coefficient Type",
         "  ExternalNode,            !- Height Selection for Local Wind Pressure Calculation", "  LOWRISE,                 !- Building Type",
         "  500,                     !- Maximum Number of Iterations {dimensionless}", "  ZeroNodePressures,       !- Initialization Type",
         "  1.0E-05,                 !- Relative Airflow Convergence Tolerance {dimensionless}",
         "  1.0E-06,                 !- Absolute Airflow Convergence Tolerance {kg/s}",
         "  -0.5,                    !- Convergence Acceleration Limit {dimensionless}",
         "  0.0,                     !- Azimuth Angle of Long Axis of Building {deg}",
         "  1.0;                     !- Ratio of Building Width Along Short Axis to Width Along Long Axis", "AirflowNetwork:MultiZone:Zone,",
         "  WEST_ZONE,               !- Zone Name", "  NoVent,                  !- Ventilation Control Mode",
         "  ,                        !- Ventilation Control Zone Temperature Setpoint Schedule Name",
         //"  Temperature,             !- Ventilation Control Mode",
         //"  WindowVentSched,         !- Ventilation Control Zone Temperature Setpoint Schedule Name",
         "  0.3,                     !- Minimum Venting Open Factor {dimensionless}",
         "  5.0,                     !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor {deltaC}",
         "  10.0,                    !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor {deltaC}",
         "  0.0,                     !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor {deltaJ/kg}",
         "  300000.0;                !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor {deltaJ/kg}",
         "AirflowNetwork:MultiZone:Zone,", "  EAST_ZONE,               !- Zone Name", "  NoVent,                  !- Ventilation Control Mode",
         "  ,                        !- Ventilation Control Zone Temperature Setpoint Schedule Name",
         "  1.0,                     !- Minimum Venting Open Factor {dimensionless}",
         "  0.0,                     !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor {deltaC}",
         "  100.0,                   !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor {deltaC}",
         "  0.0,                     !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor {deltaJ/kg}",
         "  300000.0;                !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor {deltaJ/kg}",
         "AirflowNetwork:MultiZone:Zone,", "  NORTH_ZONE,              !- Zone Name", "  NoVent,                  !- Ventilation Control Mode",
         "  ,                        !- Ventilation Control Zone Temperature Setpoint Schedule Name",
         //"  Temperature,             !- Ventilation Control Mode",
         //"  WindowVentSched,         !- Ventilation Control Zone Temperature Setpoint Schedule Name",
         "  1.0,                     !- Minimum Venting Open Factor {dimensionless}",
         "  0.0,                     !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor {deltaC}",
         "  100.0,                   !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor {deltaC}",
         "  0.0,                     !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor {deltaJ/kg}",
         "  300000.0;                !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor {deltaJ/kg}",
         "AirflowNetwork:MultiZone:Surface,", "  Surface_1,               !- Surface Name", "  CR-1,                    !- Leakage Component Name",
         "  SFacade,                 !- External Node Name",
         "  1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}", "AirflowNetwork:MultiZone:Surface,",
         "  Surface_4,               !- Surface Name", "  CR-1,                    !- Leakage Component Name",
         "  ,                        !- External Node Name",
         "  1.0;                     !- Window / Door Opening Factor, or Crack Factor{ dimensionless }", "AirflowNetwork:MultiZone:Surface,",
         "  Surface_11,              !- Surface Name", "  CR-1,                    !- Leakage Component Name",
         "  ,                        !- External Node Name",
         "  1.0;                     !- Window / Door Opening Factor, or Crack Factor{ dimensionless }", "AirflowNetwork:MultiZone:Surface,",
         "  Surface_15,              !- Surface Name", "  CR-1,                    !- Leakage Component Name",
         "  NFacade,                 !- External Node Name",
         "  1.0;                     !- Window / Door Opening Factor, or Crack Factor{ dimensionless }", "AirflowNetwork:MultiZone:ExternalNode,",
         "  NFacade,                 !- Name", "  1.524,                   !- External Node Height{ m }",
         "  NFacade_WPCCurve,        !- Wind Pressure Coefficient Values Object Name",
         "  Yes;                     !- Symmetric Wind Pressure Coefficient Curve", "AirflowNetwork:MultiZone:ExternalNode,",
         "  SFacade,                 !- Name", "  1.524,                   !- External Node Height{ m }",
         "  NFacade_WPCCurve,        !- Wind Pressure Coefficient Values Object Name",
         "  Yes,                     !- Symmetric Wind Pressure Coefficient Curve", "  Relative;                !- Wind Angle Type",
         "AirflowNetwork:MultiZone:ReferenceCrackConditions,", "  ReferenceCrackConditions,!- Name",
         "  20.0,                    !- Reference Temperature{ C }", "  101320,                  !- Reference Barometric Pressure{ Pa }",
         "  0.005;                   !- Reference Humidity Ratio{ kgWater / kgDryAir }", "AirflowNetwork:MultiZone:Surface:Crack,",
         "  CR-1,                    !- Name", "  0.01,                    !- Air Mass Flow Coefficient at Reference Conditions{ kg / s }",
         "  0.667,                   !- Air Mass Flow Exponent{ dimensionless }", "  ReferenceCrackConditions;!- Reference Crack Conditions",
         "Table:OneIndependentVariable,", "  NFacade_WPCCurve,        !- Name", "  Linear,                  !- Curve Type",
         "  LinearInterpolationOfTable,  !- Interpolation Method", "  0,                       !- Minimum Value of X",
         "  180,                     !- Maximum Value of X", "  -1,                      !- Minimum Table Output",
         "  1,                       !- Maximum Table Output", "  Dimensionless,           !- Input Unit Type for X",
         "  Dimensionless,           !- Output Unit Type", "  1,                       !- Normalization Reference",
         "  0, 0.60,                 !- X,Y Pair #1", "  30, 0.48,                !- X,Y Pair #2", "  60, 0.04,                !- X,Y Pair #3",
         "  90, -0.56,               !- X,Y Pair #4", "  120, -0.56,              !- X,Y Pair #5", "  150, -0.42,              !- X,Y Pair #6",
         "  180, -0.37;              !- X,Y Pair #7", "SurfaceConvectionAlgorithm:Inside,TARP;", "SurfaceConvectionAlgorithm:Outside,DOE-2;",
         "HeatBalanceAlgorithm,ConductionTransferFunction;", "ZoneAirHeatBalanceAlgorithm,", "  AnalyticalSolution;      !- Algorithm"});
    ASSERT_TRUE(process_idf(idf_objects));

    bool errors = false;

    HeatBalanceManager::GetMaterialData(errors); // read material data
    EXPECT_FALSE(errors);                        // expect no errors

    HeatBalanceManager::GetConstructData(errors); // read construction data
    EXPECT_FALSE(errors);                         // expect no errors

    HeatBalanceManager::GetZoneData(errors); // read zone data
    EXPECT_FALSE(errors);                    // expect no errors

    // Magic to get surfaces read in correctly
    DataHeatBalance::HeatTransferAlgosUsed.allocate(1);
    DataHeatBalance::HeatTransferAlgosUsed(1) = OverallHeatTransferSolutionAlgo;
    SurfaceGeometry::CosBldgRotAppGonly = 1.0;
    SurfaceGeometry::SinBldgRotAppGonly = 0.0;

    SurfaceGeometry::GetSurfaceData(errors); // setup zone geometry and get zone data
    EXPECT_FALSE(errors);                    // expect no errors

    CurveManager::GetCurveInput();
    EXPECT_EQ(CurveManager::NumCurves, 1);

    AirflowNetworkBalanceManager::GetAirflowNetworkInput();

    // Check the airflow elements
    EXPECT_EQ(2u, DataAirflowNetwork::MultizoneExternalNodeData.size());
    EXPECT_EQ(3u, DataAirflowNetwork::MultizoneZoneData.size());
    EXPECT_EQ(4u, DataAirflowNetwork::MultizoneSurfaceData.size());
    EXPECT_EQ(1u, DataAirflowNetwork::MultizoneSurfaceCrackData.size());
    EXPECT_EQ(2u, DataAirflowNetwork::MultizoneSurfaceStdConditionsCrackData.size());

    EXPECT_EQ(0.0, DataAirflowNetwork::MultizoneExternalNodeData(1).azimuth);
    EXPECT_TRUE(DataAirflowNetwork::MultizoneExternalNodeData(1).symmetricCurve);
    EXPECT_FALSE(DataAirflowNetwork::MultizoneExternalNodeData(1).useRelativeAngle);
    EXPECT_EQ(1, DataAirflowNetwork::MultizoneExternalNodeData(1).curve);

    EXPECT_EQ(180.0, DataAirflowNetwork::MultizoneExternalNodeData(2).azimuth);
    EXPECT_TRUE(DataAirflowNetwork::MultizoneExternalNodeData(2).symmetricCurve);
    EXPECT_TRUE(DataAirflowNetwork::MultizoneExternalNodeData(2).useRelativeAngle);
    EXPECT_EQ(1, DataAirflowNetwork::MultizoneExternalNodeData(2).curve);

    // Set up some environmental parameters
    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::OutDryBulbTemp = 25.0;
    DataEnvironment::WindDir = 105.0;
    DataEnvironment::OutHumRat = 0.0;        // Dry air only
    DataEnvironment::SiteTempGradient = 0.0; // Disconnect z from testing
    DataEnvironment::SiteWindExp = 0.0;      // Disconnect variation by height
    DataEnvironment::WindSpeed = 10.0;

    // Make sure we can compute the right wind pressure
    Real64 rho = Psychrometrics::PsyRhoAirFnPbTdbW(DataEnvironment::OutBaroPress, DataEnvironment::OutDryBulbTemp, DataEnvironment::OutHumRat);
    EXPECT_DOUBLE_EQ(1.1841123742118911, rho);
    Real64 p =
        AirflowNetworkBalanceManager::CalcWindPressure(DataAirflowNetwork::MultizoneExternalNodeData(1).curve, false, false, 0.0, 1.0,
                                                       DataEnvironment::WindDir, DataEnvironment::OutDryBulbTempAt(10.0), DataEnvironment::OutHumRat);
    EXPECT_DOUBLE_EQ(-0.56 * 0.5 * 1.1841123742118911, p);

    // Make sure the reference velocity comes out right
    EXPECT_DOUBLE_EQ(10.0, DataEnvironment::WindSpeedAt(MultizoneExternalNodeData(1).height));

    EXPECT_EQ(5u, DataAirflowNetwork::AirflowNetworkNodeSimu.size());

    // Run the balance routine, for now only to get the pressure set at the external nodes
    AirflowNetworkBalanceManager::CalcAirflowNetworkAirBalance();

    EXPECT_DOUBLE_EQ(-0.56 * 0.5 * 118.41123742118911, DataAirflowNetwork::AirflowNetworkNodeSimu(4).PZ);
    EXPECT_DOUBLE_EQ(-0.26 * 0.5 * 118.41123742118911, DataAirflowNetwork::AirflowNetworkNodeSimu(5).PZ);
}

TEST_F(EnergyPlusFixture, TestExternalNodesWithSymmetricCurve)
{
    std::string const idf_objects = delimited_string(
        {"Version,8.6;",
         "Curve:Quartic,",
         "  WindPressureFit,         !- Name",
         "  0.592,                   !- Coefficient1 Constant",
         "  0.0148,                  !- Coefficient2 x",
         "  -.000757,                !- Coefficient3 x**2",
         "  0.00000662,              !- Coefficient4 x**3",
         "  -.0000000169,            !- Coefficient5 x**4",
         "  0,                       !- Minimum Value of x",
         "  180,                     !- Maximum Value of x",
         "  -1,                      !- Minimum Curve Output",
         "  1,                       !- Maximum Curve Output",
         "  Dimensionless,           !- Input Unit Type for X",
         "  Dimensionless;           !- Output Unit Type",
         "Material,",
         "  A1 - 1 IN STUCCO,        !- Name",
         "  Smooth,                  !- Roughness",
         "  2.5389841E-02,           !- Thickness {m}",
         "  0.6918309,               !- Conductivity {W/m-K}",
         "  1858.142,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}",
         "  0.9000000,               !- Thermal Absorptance",
         "  0.9200000,               !- Solar Absorptance",
         "  0.9200000;               !- Visible Absorptance",
         "Material,",
         "  C4 - 4 IN COMMON BRICK,  !- Name",
         "  Rough,                   !- Roughness",
         "  0.1014984,               !- Thickness {m}",
         "  0.7264224,               !- Conductivity {W/m-K}",
         "  1922.216,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}",
         "  0.9000000,               !- Thermal Absorptance",
         "  0.7600000,               !- Solar Absorptance",
         "  0.7600000;               !- Visible Absorptance",
         "Material,",
         "  E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Name",
         "  Smooth,                  !- Roughness",
         "  1.905E-02,               !- Thickness {m}",
         "  0.7264224,               !- Conductivity {W/m-K}",
         "  1601.846,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}",
         "  0.9000000,               !- Thermal Absorptance",
         "  0.9200000,               !- Solar Absorptance",
         "  0.9200000;               !- Visible Absorptance",
         "Material,",
         "  C6 - 8 IN CLAY TILE,     !- Name",
         "  Smooth,                  !- Roughness",
         "  0.2033016,               !- Thickness {m}",
         "  0.5707605,               !- Conductivity {W/m-K}",
         "  1121.292,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}",
         "  0.9000000,               !- Thermal Absorptance",
         "  0.8200000,               !- Solar Absorptance",
         "  0.8200000;               !- Visible Absorptance",
         "Material,",
         "  C10 - 8 IN HW CONCRETE,  !- Name",
         "  MediumRough,             !- Roughness",
         "  0.2033016,               !- Thickness {m}",
         "  1.729577,                !- Conductivity {W/m-K}",
         "  2242.585,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}",
         "  0.9000000,               !- Thermal Absorptance",
         "  0.6500000,               !- Solar Absorptance",
         "  0.6500000;               !- Visible Absorptance",
         "Material,",
         "  E2 - 1 / 2 IN SLAG OR STONE,  !- Name",
         "  Rough,                   !- Roughness",
         "  1.2710161E-02,           !- Thickness {m}",
         "  1.435549,                !- Conductivity {W/m-K}",
         "  881.0155,                !- Density {kg/m3}",
         "  1673.600,                !- Specific Heat {J/kg-K}",
         "  0.9000000,               !- Thermal Absorptance",
         "  0.5500000,               !- Solar Absorptance",
         "  0.5500000;               !- Visible Absorptance",
         "Material,",
         "  E3 - 3 / 8 IN FELT AND MEMBRANE,  !- Name",
         "  Rough,                   !- Roughness",
         "  9.5402403E-03,           !- Thickness {m}",
         "  0.1902535,               !- Conductivity {W/m-K}",
         "  1121.292,                !- Density {kg/m3}",
         "  1673.600,                !- Specific Heat {J/kg-K}",
         "  0.9000000,               !- Thermal Absorptance",
         "  0.7500000,               !- Solar Absorptance",
         "  0.7500000;               !- Visible Absorptance",
         "Material,",
         "  B5 - 1 IN DENSE INSULATION,  !- Name",
         "  VeryRough,               !- Roughness",
         "  2.5389841E-02,           !- Thickness {m}",
         "  4.3239430E-02,           !- Conductivity {W/m-K}",
         "  91.30524,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}",
         "  0.9000000,               !- Thermal Absorptance",
         "  0.5000000,               !- Solar Absorptance",
         "  0.5000000;               !- Visible Absorptance",
         "Material,",
         "  C12 - 2 IN HW CONCRETE,  !- Name",
         "  MediumRough,             !- Roughness",
         "  5.0901599E-02,           !- Thickness {m}",
         "  1.729577,                !- Conductivity {W/m-K}",
         "  2242.585,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}",
         "  0.9000000,               !- Thermal Absorptance",
         "  0.6500000,               !- Solar Absorptance",
         "  0.6500000;               !- Visible Absorptance",
         "Material,",
         "  1.375in-Solid-Core,      !- Name",
         "  Smooth,                  !- Roughness",
         "  3.4925E-02,              !- Thickness {m}",
         "  0.1525000,               !- Conductivity {W/m-K}",
         "  614.5000,                !- Density {kg/m3}",
         "  1630.0000,               !- Specific Heat {J/kg-K}",
         "  0.9000000,               !- Thermal Absorptance",
         "  0.9200000,               !- Solar Absorptance",
         "  0.9200000;               !- Visible Absorptance",
         "WindowMaterial:Glazing,",
         "  WIN-LAY-GLASS-LIGHT,     !- Name",
         "  SpectralAverage,         !- Optical Data Type",
         "  ,                        !- Window Glass Spectral Data Set Name",
         "  0.0025,                  !- Thickness {m}",
         "  0.850,                   !- Solar Transmittance at Normal Incidence",
         "  0.075,                   !- Front Side Solar Reflectance at Normal Incidence",
         "  0.075,                   !- Back Side Solar Reflectance at Normal Incidence",
         "  0.901,                   !- Visible Transmittance at Normal Incidence",
         "  0.081,                   !- Front Side Visible Reflectance at Normal Incidence",
         "  0.081,                   !- Back Side Visible Reflectance at Normal Incidence",
         "  0.0,                     !- Infrared Transmittance at Normal Incidence",
         "  0.84,                    !- Front Side Infrared Hemispherical Emissivity",
         "  0.84,                    !- Back Side Infrared Hemispherical Emissivity",
         "  0.9;                     !- Conductivity {W/m-K}",
         "Construction,",
         "  DOOR-CON,                !- Name",
         "  1.375in-Solid-Core;      !- Outside Layer",
         "Construction,",
         "  EXTWALL80,               !- Name",
         "  A1 - 1 IN STUCCO,        !- Outside Layer",
         "  C4 - 4 IN COMMON BRICK,  !- Layer 2",
         "  E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 3",
         "Construction,",
         "  PARTITION06,             !- Name",
         "  E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Outside Layer",
         "  C6 - 8 IN CLAY TILE,     !- Layer 2",
         "  E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 3",
         "  Construction,",
         "  FLOOR SLAB 8 IN,         !- Name",
         "  C10 - 8 IN HW CONCRETE;  !- Outside Layer",
         "Construction,",
         "  ROOF34,                  !- Name",
         "  E2 - 1 / 2 IN SLAG OR STONE,  !- Outside Layer",
         "  E3 - 3 / 8 IN FELT AND MEMBRANE,  !- Layer 2",
         "  B5 - 1 IN DENSE INSULATION,  !- Layer 3",
         "  C12 - 2 IN HW CONCRETE;  !- Layer 4",
         "Construction,",
         "  WIN-CON-LIGHT,           !- Name",
         "  WIN-LAY-GLASS-LIGHT;     !- Outside Layer",
         "Zone,",
         "  WEST_ZONE,               !- Name",
         "  0,                       !- Direction of Relative North {deg}",
         "  0,                       !- X Origin {m}",
         "  0,                       !- Y Origin {m}",
         "  0,                       !- Z Origin {m}",
         "  1,                       !- Type",
         "  1,                       !- Multiplier",
         "  autocalculate;           !- Ceiling Height {m}",
         "Zone,",
         "  EAST_ZONE,               !- Name",
         "  0,                       !- Direction of Relative North {deg}",
         "  0,                       !- X Origin {m}",
         "  0,                       !- Y Origin {m}",
         "  0,                       !- Z Origin {m}",
         "  1,                       !- Type",
         "  1,                       !- Multiplier",
         "  autocalculate;           !- Ceiling Height {m}",
         "Zone,",
         "  NORTH_ZONE,              !- Name",
         "  0,                       !- Direction of Relative North {deg}",
         "  0,                       !- X Origin {m}",
         "  0,                       !- Y Origin {m}",
         "  0,                       !- Z Origin {m}",
         "  1,                       !- Type",
         "  1,                       !- Multiplier",
         "  autocalculate;           !- Ceiling Height {m}",
         "GlobalGeometryRules,",
         "  UpperLeftCorner,         !- Starting Vertex Position",
         "  CounterClockWise,        !- Vertex Entry Direction",
         "  World;                   !- Coordinate System",
         "BuildingSurface:Detailed,",
         "  Surface_1,               !- Name",
         "  WALL,                    !- Surface Type",
         "  EXTWALL80,               !- Construction Name",
         "  WEST_ZONE,               !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  0,0,3.048000,            !- X,Y,Z ==> Vertex 1 {m}",
         "  0,0,0,                   !- X,Y,Z ==> Vertex 2 {m}",
         "  6.096000,0,0,            !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096000,0,3.048000;     !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_2,               !- Name",
         "  WALL,                    !- Surface Type",
         "  EXTWALL80,               !- Construction Name",
         "  WEST_ZONE,               !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  0,6.096000,3.048000,     !- X,Y,Z ==> Vertex 1 {m}",
         "  0,6.096000,0,            !- X,Y,Z ==> Vertex 2 {m}",
         "  0,0,0,                   !- X,Y,Z ==> Vertex 3 {m}",
         "  0,0,3.048000;            !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_3,               !- Name",
         "  WALL,                    !- Surface Type",
         "  PARTITION06,             !- Construction Name",
         "  WEST_ZONE,               !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition",
         "  Surface_17,              !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  6.096000,6.096000,0,     !- X,Y,Z ==> Vertex 2 {m}",
         "  0,6.096000,0,            !- X,Y,Z ==> Vertex 3 {m}",
         "  0,6.096000,3.048000;     !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_4,               !- Name",
         "  WALL,                    !- Surface Type",
         "  PARTITION06,             !- Construction Name",
         "  WEST_ZONE,               !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition",
         "  Surface_10,              !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  6.096000,0,3.048000,     !- X,Y,Z ==> Vertex 1 {m}",
         "  6.096000,0,0,            !- X,Y,Z ==> Vertex 2 {m}",
         "  6.096000,6.096000,0,     !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_5,               !- Name",
         "  FLOOR,                   !- Surface Type",
         "  FLOOR SLAB 8 IN,         !- Construction Name",
         "  WEST_ZONE,               !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition",
         "  Surface_5,               !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure",
         "  1.000000,                !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  0,0,0,                   !- X,Y,Z ==> Vertex 1 {m}",
         "  0,6.096000,0,            !- X,Y,Z ==> Vertex 2 {m}",
         "  6.096000,6.096000,0,     !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096000,0,0;            !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_6,               !- Name",
         "  ROOF,                    !- Surface Type",
         "  ROOF34,                  !- Construction Name",
         "  WEST_ZONE,               !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  0,                       !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  0,6.096000,3.048000,     !- X,Y,Z ==> Vertex 1 {m}",
         "  0,0,3.048000,            !- X,Y,Z ==> Vertex 2 {m}",
         "  6.096000,0,3.048000,     !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_8,               !- Name",
         "  WALL,                    !- Surface Type",
         "  EXTWALL80,               !- Construction Name",
         "  EAST_ZONE,               !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  6.096000,0,3.048000,     !- X,Y,Z ==> Vertex 1 {m}",
         "  6.096000,0,0,            !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,0,0,            !- X,Y,Z ==> Vertex 3 {m}",
         "  12.19200,0,3.048000;     !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_9,               !- Name",
         "  WALL,                    !- Surface Type",
         "  EXTWALL80,               !- Construction Name",
         "  EAST_ZONE,               !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  12.19200,0,3.048000,     !- X,Y,Z ==> Vertex 1 {m}",
         "  12.19200,0,0,            !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,6.096000,0,     !- X,Y,Z ==> Vertex 3 {m}",
         "  12.19200,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_10,              !- Name",
         "  WALL,                    !- Surface Type",
         "  PARTITION06,             !- Construction Name",
         "  EAST_ZONE,               !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition",
         "  Surface_4,               !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  6.096000,6.096000,0,     !- X,Y,Z ==> Vertex 2 {m}",
         "  6.096000,0,0,            !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096001,0,3.048000;     !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_11,              !- Name",
         "  WALL,                    !- Surface Type",
         "  PARTITION06,             !- Construction Name",
         "  EAST_ZONE,               !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition",
         "  Surface_18,              !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  12.19200,6.096000,0,     !- X,Y,Z ==> Vertex 2 {m}",
         "  6.096000,6.096000,0,     !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_12,              !- Name",
         "  FLOOR,                   !- Surface Type",
         "  FLOOR SLAB 8 IN,         !- Construction Name",
         "  EAST_ZONE,               !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition",
         "  Surface_12,              !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure",
         "  1.000000,                !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  6.096000,0,0,            !- X,Y,Z ==> Vertex 1 {m}",
         "  6.096000,6.096000,0,     !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,6.096000,0,     !- X,Y,Z ==> Vertex 3 {m}",
         "  12.19200,0,0;            !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_13,              !- Name",
         "  ROOF,                    !- Surface Type",
         "  ROOF34,                  !- Construction Name",
         "  EAST_ZONE,               !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  0,                       !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  6.096000,0,3.048000,     !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,0,3.048000,     !- X,Y,Z ==> Vertex 3 {m}",
         "  12.19200,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_14,              !- Name",
         "  WALL,                    !- Surface Type",
         "  EXTWALL80,               !- Construction Name",
         "  NORTH_ZONE,              !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  0,12.19200,3.048000,     !- X,Y,Z ==> Vertex 1 {m}",
         "  0,12.19200,0,            !- X,Y,Z ==> Vertex 2 {m}",
         "  0,6.096000,0,            !- X,Y,Z ==> Vertex 3 {m}",
         "  0,6.096000,3.048000;     !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_15,              !- Name",
         "  WALL,                    !- Surface Type",
         "  EXTWALL80,               !- Construction Name",
         "  NORTH_ZONE,              !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  12.19200,12.19200,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  12.19200,12.19200,0,  !- X,Y,Z ==> Vertex 2 {m}",
         "  0,12.19200,0,  !- X,Y,Z ==> Vertex 3 {m}",
         "  0,12.19200,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_16,              !- Name",
         "  WALL,                    !- Surface Type",
         "  EXTWALL80,               !- Construction Name",
         "  NORTH_ZONE,              !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  12.19200,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,12.19200,0,  !- X,Y,Z ==> Vertex 3 {m}",
         "  12.19200,12.19200,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_17,              !- Name",
         "  WALL,                    !- Surface Type",
         "  PARTITION06,             !- Construction Name",
         "  NORTH_ZONE,              !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition",
         "  Surface_3,               !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  0.000,6.096,3.048,  !- X,Y,Z ==> Vertex 1 {m}",
         "  0.000,6.096,0.000,  !- X,Y,Z ==> Vertex 2 {m}",
         "  6.096,6.096,0.000,  !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096,6.096,3.048;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_18,              !- Name",
         "  WALL,                    !- Surface Type",
         "  PARTITION06,             !- Construction Name",
         "  NORTH_ZONE,              !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition",
         "  Surface_11,              !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
         "  12.19200,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_19,              !- Name",
         "  FLOOR,                   !- Surface Type",
         "  FLOOR SLAB 8 IN,         !- Construction Name",
         "  NORTH_ZONE,              !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition",
         "  Surface_19,              !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure",
         "  1.000000,                !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  0,6.096000,0,  !- X,Y,Z ==> Vertex 1 {m}",
         "  0,12.19200,0,  !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,12.19200,0,  !- X,Y,Z ==> Vertex 3 {m}",
         "  12.19200,6.096000,0;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_20,              !- Name",
         "  ROOF,                    !- Surface Type",
         "  ROOF34,                  !- Construction Name",
         "  NORTH_ZONE,              !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  0,                       !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  0,12.19200,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
         "  12.19200,12.19200,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
         "AirflowNetwork:SimulationControl,",
         "  NaturalVentilation,      !- Name",
         "  MultizoneWithoutDistribution,  !- AirflowNetwork Control",
         "  INPUT,                   !- Wind Pressure Coefficient Type",
         "  ExternalNode,            !- Height Selection for Local Wind Pressure Calculation",
         "  LOWRISE,                 !- Building Type",
         "  500,                     !- Maximum Number of Iterations {dimensionless}",
         "  ZeroNodePressures,       !- Initialization Type",
         "  1.0E-05,                 !- Relative Airflow Convergence Tolerance {dimensionless}",
         "  1.0E-06,                 !- Absolute Airflow Convergence Tolerance {kg/s}",
         "  -0.5,                    !- Convergence Acceleration Limit {dimensionless}",
         "  0.0,                     !- Azimuth Angle of Long Axis of Building {deg}",
         "  1.0;                     !- Ratio of Building Width Along Short Axis to Width Along Long Axis",
         "AirflowNetwork:MultiZone:Zone,",
         "  WEST_ZONE,               !- Zone Name",
         "  NoVent,                  !- Ventilation Control Mode",
         "  ,                        !- Ventilation Control Zone Temperature Setpoint Schedule Name",
         "  0.3,                     !- Minimum Venting Open Factor {dimensionless}",
         "  5.0,                     !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor {deltaC}",
         "  10.0,                    !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor {deltaC}",
         "  0.0,                     !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor {deltaJ/kg}",
         "  300000.0;                !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor {deltaJ/kg}",
         "AirflowNetwork:MultiZone:Zone,",
         "  EAST_ZONE,               !- Zone Name",
         "  NoVent,                  !- Ventilation Control Mode",
         "  ,                        !- Ventilation Control Zone Temperature Setpoint Schedule Name",
         "  1.0,                     !- Minimum Venting Open Factor {dimensionless}",
         "  0.0,                     !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor {deltaC}",
         "  100.0,                   !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor {deltaC}",
         "  0.0,                     !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor {deltaJ/kg}",
         "  300000.0;                !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor {deltaJ/kg}",
         "AirflowNetwork:MultiZone:Zone,",
         "  NORTH_ZONE,              !- Zone Name",
         "  NoVent,                  !- Ventilation Control Mode",
         "  ,                        !- Ventilation Control Zone Temperature Setpoint Schedule Name",
         "  1.0,                     !- Minimum Venting Open Factor {dimensionless}",
         "  0.0,                     !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor {deltaC}",
         "  100.0,                   !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor {deltaC}",
         "  0.0,                     !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor {deltaJ/kg}",
         "  300000.0;                !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor {deltaJ/kg}",
         "AirflowNetwork:MultiZone:Surface,",
         "  Surface_1,               !- Surface Name",
         "  CR-1,                    !- Leakage Component Name",
         "  SFacade,                 !- External Node Name",
         "  1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",
         "AirflowNetwork:MultiZone:Surface,",
         "  Surface_4,               !- Surface Name",
         "  CR-1,                    !- Leakage Component Name",
         "  ,                        !- External Node Name",
         "  1.0;                     !- Window / Door Opening Factor, or Crack Factor{ dimensionless }",
         "AirflowNetwork:MultiZone:Surface,",
         "  Surface_11,              !- Surface Name",
         "  CR-1,                    !- Leakage Component Name",
         "  ,                        !- External Node Name",
         "  1.0;                     !- Window / Door Opening Factor, or Crack Factor{ dimensionless }",
         "AirflowNetwork:MultiZone:Surface,",
         "  Surface_15,              !- Surface Name",
         "  CR-1,                    !- Leakage Component Name",
         "  NFacade,                 !- External Node Name",
         "  1.0;                     !- Window / Door Opening Factor, or Crack Factor{ dimensionless }",
         "AirflowNetwork:MultiZone:ExternalNode,",
         "  NFacade,                 !- Name",
         "  1.524,                   !- External Node Height{ m }",
         "  WindPressureFit,         !- Wind Pressure Coefficient Values Object Name",
         "  Yes;                     !- Symmetric Wind Pressure Coefficient Curve",
         "AirflowNetwork:MultiZone:ExternalNode,",
         "  SFacade,                 !- Name",
         "  1.524,                   !- External Node Height{ m }",
         "  WindPressureFit,         !- Wind Pressure Coefficient Values Object Name",
         "  Yes,                     !- Symmetric Wind Pressure Coefficient Curve",
         "  Relative;                !- Wind Angle Type",
         "AirflowNetwork:MultiZone:ReferenceCrackConditions,",
         "  ReferenceCrackConditions,!- Name",
         "  20.0,                    !- Reference Temperature{ C }",
         "  101320,                  !- Reference Barometric Pressure{ Pa }",
         "  0.005;                   !- Reference Humidity Ratio{ kgWater / kgDryAir }",
         "AirflowNetwork:MultiZone:Surface:Crack,",
         "  CR-1,                    !- Name",
         "  0.01,                    !- Air Mass Flow Coefficient at Reference Conditions{ kg / s }",
         "  0.667,                   !- Air Mass Flow Exponent{ dimensionless }",
         "  ReferenceCrackConditions;!- Reference Crack Conditions",
         "SurfaceConvectionAlgorithm:Inside,TARP;",
         "SurfaceConvectionAlgorithm:Outside,DOE-2;",
         "HeatBalanceAlgorithm,ConductionTransferFunction;",
         "ZoneAirHeatBalanceAlgorithm,",
         "  AnalyticalSolution;      !- Algorithm"});
    ASSERT_TRUE(process_idf(idf_objects));

    bool errors = false;

    HeatBalanceManager::GetMaterialData(errors); // read material data
    EXPECT_FALSE(errors);                        // expect no errors

    HeatBalanceManager::GetConstructData(errors); // read construction data
    EXPECT_FALSE(errors);                         // expect no errors

    HeatBalanceManager::GetZoneData(errors); // read zone data
    EXPECT_FALSE(errors);                    // expect no errors

    // Magic to get surfaces read in correctly
    DataHeatBalance::HeatTransferAlgosUsed.allocate(1);
    DataHeatBalance::HeatTransferAlgosUsed(1) = OverallHeatTransferSolutionAlgo;
    SurfaceGeometry::CosBldgRotAppGonly = 1.0;
    SurfaceGeometry::SinBldgRotAppGonly = 0.0;

    SurfaceGeometry::GetSurfaceData(errors); // setup zone geometry and get zone data
    EXPECT_FALSE(errors);                    // expect no errors

    CurveManager::GetCurveInput();
    EXPECT_EQ(CurveManager::NumCurves, 1);

    AirflowNetworkBalanceManager::GetAirflowNetworkInput();

    // Check the airflow elements
    EXPECT_EQ(2u, DataAirflowNetwork::MultizoneExternalNodeData.size());
    EXPECT_EQ(3u, DataAirflowNetwork::MultizoneZoneData.size());
    EXPECT_EQ(4u, DataAirflowNetwork::MultizoneSurfaceData.size());
    EXPECT_EQ(1u, DataAirflowNetwork::MultizoneSurfaceCrackData.size());
    EXPECT_EQ(2u, DataAirflowNetwork::MultizoneSurfaceStdConditionsCrackData.size());

    EXPECT_EQ(0.0, DataAirflowNetwork::MultizoneExternalNodeData(1).azimuth);
    EXPECT_TRUE(DataAirflowNetwork::MultizoneExternalNodeData(1).symmetricCurve);
    EXPECT_FALSE(DataAirflowNetwork::MultizoneExternalNodeData(1).useRelativeAngle);
    EXPECT_EQ(1, DataAirflowNetwork::MultizoneExternalNodeData(1).curve);

    EXPECT_EQ(180.0, DataAirflowNetwork::MultizoneExternalNodeData(2).azimuth);
    EXPECT_TRUE(DataAirflowNetwork::MultizoneExternalNodeData(2).symmetricCurve);
    EXPECT_TRUE(DataAirflowNetwork::MultizoneExternalNodeData(2).useRelativeAngle);
    EXPECT_EQ(1, DataAirflowNetwork::MultizoneExternalNodeData(2).curve);

    // Check the curves
    Real64 cp105N = -0.590653062499999;
    Real64 cp105S = -0.298039062499999;
    EXPECT_DOUBLE_EQ(0.592, CurveManager::CurveValue(1, 0));                   // In-range value
    EXPECT_NEAR(cp105N, CurveManager::CurveValue(1, 105), 1e-14);              // In-range value
    EXPECT_NEAR(cp105S, CurveManager::CurveValue(1, 75), 1e-14);               // In-range value
    EXPECT_DOUBLE_EQ(0.592, CurveManager::CurveValue(1, -10.0));               // Minimum x
    EXPECT_NEAR(-0.403903999999994, CurveManager::CurveValue(1, 5000), 1e-14); // Maximum x

    // Set up some environmental parameters
    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::OutDryBulbTemp = 25.0;
    DataEnvironment::WindDir = 105.0;
    DataEnvironment::OutHumRat = 0.0;        // Dry air only
    DataEnvironment::SiteTempGradient = 0.0; // Disconnect z from testing
    DataEnvironment::SiteWindExp = 0.0;      // Disconnect variation by height
    DataEnvironment::WindSpeed = 10.0;

    // Make sure we can compute the right wind pressure
    Real64 rho = Psychrometrics::PsyRhoAirFnPbTdbW(DataEnvironment::OutBaroPress, DataEnvironment::OutDryBulbTemp, DataEnvironment::OutHumRat);
    EXPECT_DOUBLE_EQ(1.1841123742118911, rho);
    Real64 p =
        AirflowNetworkBalanceManager::CalcWindPressure(DataAirflowNetwork::MultizoneExternalNodeData(1).curve, false, false, 0.0, 1.0,
                                                       DataEnvironment::WindDir, DataEnvironment::OutDryBulbTempAt(10.0), DataEnvironment::OutHumRat);
    EXPECT_DOUBLE_EQ(cp105N * 0.5 * 1.1841123742118911, p);

    // Make sure the reference velocity comes out right
    EXPECT_DOUBLE_EQ(10.0, DataEnvironment::WindSpeedAt(MultizoneExternalNodeData(1).height));

    EXPECT_EQ(5u, DataAirflowNetwork::AirflowNetworkNodeSimu.size());

    // Run the balance routine, for now only to get the pressure set at the external nodes
    AirflowNetworkBalanceManager::CalcAirflowNetworkAirBalance();

    EXPECT_NEAR(cp105N * 0.5 * 118.41123742118911, DataAirflowNetwork::AirflowNetworkNodeSimu(4).PZ, 1e-13);
    EXPECT_NEAR(cp105S * 0.5 * 118.41123742118911, DataAirflowNetwork::AirflowNetworkNodeSimu(5).PZ, 1e-13);
}

TEST_F(EnergyPlusFixture, TestExternalNodesWithLocalAirNode)
{
    std::string const idf_objects = delimited_string(
        {"Version,9.0;",
         "Material,",
         "  A1 - 1 IN STUCCO,        !- Name",
         "  Smooth,                  !- Roughness",
         "  2.5389841E-02,           !- Thickness {m}",
         "  0.6918309,               !- Conductivity {W/m-K}",
         "  1858.142,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}",
         "  0.9000000,               !- Thermal Absorptance",
         "  0.9200000,               !- Solar Absorptance",
         "  0.9200000;               !- Visible Absorptance",
         "Material,",
         "  C4 - 4 IN COMMON BRICK,  !- Name",
         "  Rough,                   !- Roughness",
         "  0.1014984,               !- Thickness {m}",
         "  0.7264224,               !- Conductivity {W/m-K}",
         "  1922.216,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}",
         "  0.9000000,               !- Thermal Absorptance",
         "  0.7600000,               !- Solar Absorptance",
         "  0.7600000;               !- Visible Absorptance",
         "Material,",
         "  E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Name",
         "  Smooth,                  !- Roughness",
         "  1.905E-02,               !- Thickness {m}",
         "  0.7264224,               !- Conductivity {W/m-K}",
         "  1601.846,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}",
         "  0.9000000,               !- Thermal Absorptance",
         "  0.9200000,               !- Solar Absorptance",
         "  0.9200000;               !- Visible Absorptance",
         "Material,",
         "  C6 - 8 IN CLAY TILE,     !- Name",
         "  Smooth,                  !- Roughness",
         "  0.2033016,               !- Thickness {m}",
         "  0.5707605,               !- Conductivity {W/m-K}",
         "  1121.292,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}",
         "  0.9000000,               !- Thermal Absorptance",
         "  0.8200000,               !- Solar Absorptance",
         "  0.8200000;               !- Visible Absorptance",
         "Material,",
         "  C10 - 8 IN HW CONCRETE,  !- Name",
         "  MediumRough,             !- Roughness",
         "  0.2033016,               !- Thickness {m}",
         "  1.729577,                !- Conductivity {W/m-K}",
         "  2242.585,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}",
         "  0.9000000,               !- Thermal Absorptance",
         "  0.6500000,               !- Solar Absorptance",
         "  0.6500000;               !- Visible Absorptance",
         "Material,",
         "  E2 - 1 / 2 IN SLAG OR STONE,  !- Name",
         "  Rough,                   !- Roughness",
         "  1.2710161E-02,           !- Thickness {m}",
         "  1.435549,                !- Conductivity {W/m-K}",
         "  881.0155,                !- Density {kg/m3}",
         "  1673.600,                !- Specific Heat {J/kg-K}",
         "  0.9000000,               !- Thermal Absorptance",
         "  0.5500000,               !- Solar Absorptance",
         "  0.5500000;               !- Visible Absorptance",
         "Material,",
         "  E3 - 3 / 8 IN FELT AND MEMBRANE,  !- Name",
         "  Rough,                   !- Roughness",
         "  9.5402403E-03,           !- Thickness {m}",
         "  0.1902535,               !- Conductivity {W/m-K}",
         "  1121.292,                !- Density {kg/m3}",
         "  1673.600,                !- Specific Heat {J/kg-K}",
         "  0.9000000,               !- Thermal Absorptance",
         "  0.7500000,               !- Solar Absorptance",
         "  0.7500000;               !- Visible Absorptance",
         "Material,",
         "  B5 - 1 IN DENSE INSULATION,  !- Name",
         "  VeryRough,               !- Roughness",
         "  2.5389841E-02,           !- Thickness {m}",
         "  4.3239430E-02,           !- Conductivity {W/m-K}",
         "  91.30524,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}",
         "  0.9000000,               !- Thermal Absorptance",
         "  0.5000000,               !- Solar Absorptance",
         "  0.5000000;               !- Visible Absorptance",
         "Material,",
         "  C12 - 2 IN HW CONCRETE,  !- Name",
         "  MediumRough,             !- Roughness",
         "  5.0901599E-02,           !- Thickness {m}",
         "  1.729577,                !- Conductivity {W/m-K}",
         "  2242.585,                !- Density {kg/m3}",
         "  836.8000,                !- Specific Heat {J/kg-K}",
         "  0.9000000,               !- Thermal Absorptance",
         "  0.6500000,               !- Solar Absorptance",
         "  0.6500000;               !- Visible Absorptance",
         "Material,",
         "  1.375in-Solid-Core,      !- Name",
         "  Smooth,                  !- Roughness",
         "  3.4925E-02,              !- Thickness {m}",
         "  0.1525000,               !- Conductivity {W/m-K}",
         "  614.5000,                !- Density {kg/m3}",
         "  1630.0000,               !- Specific Heat {J/kg-K}",
         "  0.9000000,               !- Thermal Absorptance",
         "  0.9200000,               !- Solar Absorptance",
         "  0.9200000;               !- Visible Absorptance",
         "WindowMaterial:Glazing,",
         "  WIN-LAY-GLASS-LIGHT,     !- Name",
         "  SpectralAverage,         !- Optical Data Type",
         "  ,                        !- Window Glass Spectral Data Set Name",
         "  0.0025,                  !- Thickness {m}",
         "  0.850,                   !- Solar Transmittance at Normal Incidence",
         "  0.075,                   !- Front Side Solar Reflectance at Normal Incidence",
         "  0.075,                   !- Back Side Solar Reflectance at Normal Incidence",
         "  0.901,                   !- Visible Transmittance at Normal Incidence",
         "  0.081,                   !- Front Side Visible Reflectance at Normal Incidence",
         "  0.081,                   !- Back Side Visible Reflectance at Normal Incidence",
         "  0.0,                     !- Infrared Transmittance at Normal Incidence",
         "  0.84,                    !- Front Side Infrared Hemispherical Emissivity",
         "  0.84,                    !- Back Side Infrared Hemispherical Emissivity",
         "  0.9;                     !- Conductivity {W/m-K}",
         "Construction,",
         "  DOOR-CON,                !- Name",
         "  1.375in-Solid-Core;      !- Outside Layer",
         "Construction,",
         "  EXTWALL80,               !- Name",
         "  A1 - 1 IN STUCCO,        !- Outside Layer",
         "  C4 - 4 IN COMMON BRICK,  !- Layer 2",
         "  E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 3",
         "Construction,",
         "  PARTITION06,             !- Name",
         "  E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Outside Layer",
         "  C6 - 8 IN CLAY TILE,     !- Layer 2",
         "  E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 3",
         "  Construction,",
         "  FLOOR SLAB 8 IN,         !- Name",
         "  C10 - 8 IN HW CONCRETE;  !- Outside Layer",
         "Construction,",
         "  ROOF34,                  !- Name",
         "  E2 - 1 / 2 IN SLAG OR STONE,  !- Outside Layer",
         "  E3 - 3 / 8 IN FELT AND MEMBRANE,  !- Layer 2",
         "  B5 - 1 IN DENSE INSULATION,  !- Layer 3",
         "  C12 - 2 IN HW CONCRETE;  !- Layer 4",
         "Construction,",
         "  WIN-CON-LIGHT,           !- Name",
         "  WIN-LAY-GLASS-LIGHT;     !- Outside Layer",
         "Zone,",
         "  WEST_ZONE,               !- Name",
         "  0,                       !- Direction of Relative North {deg}",
         "  0,                       !- X Origin {m}",
         "  0,                       !- Y Origin {m}",
         "  0,                       !- Z Origin {m}",
         "  1,                       !- Type",
         "  1,                       !- Multiplier",
         "  autocalculate;           !- Ceiling Height {m}",
         "Zone,",
         "  EAST_ZONE,               !- Name",
         "  0,                       !- Direction of Relative North {deg}",
         "  0,                       !- X Origin {m}",
         "  0,                       !- Y Origin {m}",
         "  0,                       !- Z Origin {m}",
         "  1,                       !- Type",
         "  1,                       !- Multiplier",
         "  autocalculate;           !- Ceiling Height {m}",
         "Zone,",
         "  NORTH_ZONE,              !- Name",
         "  0,                       !- Direction of Relative North {deg}",
         "  0,                       !- X Origin {m}",
         "  0,                       !- Y Origin {m}",
         "  0,                       !- Z Origin {m}",
         "  1,                       !- Type",
         "  1,                       !- Multiplier",
         "  autocalculate;           !- Ceiling Height {m}",
         "GlobalGeometryRules,",
         "  UpperLeftCorner,         !- Starting Vertex Position",
         "  CounterClockWise,        !- Vertex Entry Direction",
         "  World;                   !- Coordinate System",
         "BuildingSurface:Detailed,",
         "  Surface_1,               !- Name",
         "  WALL,                    !- Surface Type",
         "  EXTWALL80,               !- Construction Name",
         "  WEST_ZONE,               !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  0,0,3.048000,            !- X,Y,Z ==> Vertex 1 {m}",
         "  0,0,0,                   !- X,Y,Z ==> Vertex 2 {m}",
         "  6.096000,0,0,            !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096000,0,3.048000;     !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_2,               !- Name",
         "  WALL,                    !- Surface Type",
         "  EXTWALL80,               !- Construction Name",
         "  WEST_ZONE,               !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  0,6.096000,3.048000,     !- X,Y,Z ==> Vertex 1 {m}",
         "  0,6.096000,0,            !- X,Y,Z ==> Vertex 2 {m}",
         "  0,0,0,                   !- X,Y,Z ==> Vertex 3 {m}",
         "  0,0,3.048000;            !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_3,               !- Name",
         "  WALL,                    !- Surface Type",
         "  PARTITION06,             !- Construction Name",
         "  WEST_ZONE,               !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition",
         "  Surface_17,              !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  6.096000,6.096000,0,     !- X,Y,Z ==> Vertex 2 {m}",
         "  0,6.096000,0,            !- X,Y,Z ==> Vertex 3 {m}",
         "  0,6.096000,3.048000;     !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_4,               !- Name",
         "  WALL,                    !- Surface Type",
         "  PARTITION06,             !- Construction Name",
         "  WEST_ZONE,               !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition",
         "  Surface_10,              !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  6.096000,0,3.048000,     !- X,Y,Z ==> Vertex 1 {m}",
         "  6.096000,0,0,            !- X,Y,Z ==> Vertex 2 {m}",
         "  6.096000,6.096000,0,     !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_5,               !- Name",
         "  FLOOR,                   !- Surface Type",
         "  FLOOR SLAB 8 IN,         !- Construction Name",
         "  WEST_ZONE,               !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition",
         "  Surface_5,               !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure",
         "  1.000000,                !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  0,0,0,                   !- X,Y,Z ==> Vertex 1 {m}",
         "  0,6.096000,0,            !- X,Y,Z ==> Vertex 2 {m}",
         "  6.096000,6.096000,0,     !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096000,0,0;            !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_6,               !- Name",
         "  ROOF,                    !- Surface Type",
         "  ROOF34,                  !- Construction Name",
         "  WEST_ZONE,               !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  0,                       !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  0,6.096000,3.048000,     !- X,Y,Z ==> Vertex 1 {m}",
         "  0,0,3.048000,            !- X,Y,Z ==> Vertex 2 {m}",
         "  6.096000,0,3.048000,     !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_8,               !- Name",
         "  WALL,                    !- Surface Type",
         "  EXTWALL80,               !- Construction Name",
         "  EAST_ZONE,               !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  6.096000,0,3.048000,     !- X,Y,Z ==> Vertex 1 {m}",
         "  6.096000,0,0,            !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,0,0,            !- X,Y,Z ==> Vertex 3 {m}",
         "  12.19200,0,3.048000;     !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_9,               !- Name",
         "  WALL,                    !- Surface Type",
         "  EXTWALL80,               !- Construction Name",
         "  EAST_ZONE,               !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  12.19200,0,3.048000,     !- X,Y,Z ==> Vertex 1 {m}",
         "  12.19200,0,0,            !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,6.096000,0,     !- X,Y,Z ==> Vertex 3 {m}",
         "  12.19200,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_10,              !- Name",
         "  WALL,                    !- Surface Type",
         "  PARTITION06,             !- Construction Name",
         "  EAST_ZONE,               !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition",
         "  Surface_4,               !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  6.096000,6.096000,0,     !- X,Y,Z ==> Vertex 2 {m}",
         "  6.096000,0,0,            !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096001,0,3.048000;     !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_11,              !- Name",
         "  WALL,                    !- Surface Type",
         "  PARTITION06,             !- Construction Name",
         "  EAST_ZONE,               !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition",
         "  Surface_18,              !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  12.19200,6.096000,0,     !- X,Y,Z ==> Vertex 2 {m}",
         "  6.096000,6.096000,0,     !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_12,              !- Name",
         "  FLOOR,                   !- Surface Type",
         "  FLOOR SLAB 8 IN,         !- Construction Name",
         "  EAST_ZONE,               !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition",
         "  Surface_12,              !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure",
         "  1.000000,                !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  6.096000,0,0,            !- X,Y,Z ==> Vertex 1 {m}",
         "  6.096000,6.096000,0,     !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,6.096000,0,     !- X,Y,Z ==> Vertex 3 {m}",
         "  12.19200,0,0;            !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_13,              !- Name",
         "  ROOF,                    !- Surface Type",
         "  ROOF34,                  !- Construction Name",
         "  EAST_ZONE,               !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  0,                       !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  6.096000,0,3.048000,     !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,0,3.048000,     !- X,Y,Z ==> Vertex 3 {m}",
         "  12.19200,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_14,              !- Name",
         "  WALL,                    !- Surface Type",
         "  EXTWALL80,               !- Construction Name",
         "  NORTH_ZONE,              !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  0,12.19200,3.048000,     !- X,Y,Z ==> Vertex 1 {m}",
         "  0,12.19200,0,            !- X,Y,Z ==> Vertex 2 {m}",
         "  0,6.096000,0,            !- X,Y,Z ==> Vertex 3 {m}",
         "  0,6.096000,3.048000;     !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_15,              !- Name",
         "  WALL,                    !- Surface Type",
         "  EXTWALL80,               !- Construction Name",
         "  NORTH_ZONE,              !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  12.19200,12.19200,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  12.19200,12.19200,0,  !- X,Y,Z ==> Vertex 2 {m}",
         "  0,12.19200,0,  !- X,Y,Z ==> Vertex 3 {m}",
         "  0,12.19200,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_16,              !- Name",
         "  WALL,                    !- Surface Type",
         "  EXTWALL80,               !- Construction Name",
         "  NORTH_ZONE,              !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  12.19200,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,12.19200,0,  !- X,Y,Z ==> Vertex 3 {m}",
         "  12.19200,12.19200,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_17,              !- Name",
         "  WALL,                    !- Surface Type",
         "  PARTITION06,             !- Construction Name",
         "  NORTH_ZONE,              !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition",
         "  Surface_3,               !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  0.000,6.096,3.048,  !- X,Y,Z ==> Vertex 1 {m}",
         "  0.000,6.096,0.000,  !- X,Y,Z ==> Vertex 2 {m}",
         "  6.096,6.096,0.000,  !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096,6.096,3.048;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_18,              !- Name",
         "  WALL,                    !- Surface Type",
         "  PARTITION06,             !- Construction Name",
         "  NORTH_ZONE,              !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition",
         "  Surface_11,              !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure",
         "  0.5000000,               !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
         "  12.19200,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_19,              !- Name",
         "  FLOOR,                   !- Surface Type",
         "  FLOOR SLAB 8 IN,         !- Construction Name",
         "  NORTH_ZONE,              !- Zone Name",
         "  Surface,                 !- Outside Boundary Condition",
         "  Surface_19,              !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure",
         "  1.000000,                !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  0,6.096000,0,  !- X,Y,Z ==> Vertex 1 {m}",
         "  0,12.19200,0,  !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,12.19200,0,  !- X,Y,Z ==> Vertex 3 {m}",
         "  12.19200,6.096000,0;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  Surface_20,              !- Name",
         "  ROOF,                    !- Surface Type",
         "  ROOF34,                  !- Construction Name",
         "  NORTH_ZONE,              !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  0,                       !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  0,12.19200,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
         "  12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
         "  12.19200,12.19200,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",
         "AirflowNetwork:SimulationControl,",
         "  NaturalVentilation,      !- Name",
         "  MultizoneWithoutDistribution,  !- AirflowNetwork Control",
         "  INPUT,                   !- Wind Pressure Coefficient Type",
         "  ExternalNode,            !- Height Selection for Local Wind Pressure Calculation",
         "  LOWRISE,                 !- Building Type",
         "  500,                     !- Maximum Number of Iterations {dimensionless}",
         "  ZeroNodePressures,       !- Initialization Type",
         "  1.0E-05,                 !- Relative Airflow Convergence Tolerance {dimensionless}",
         "  1.0E-06,                 !- Absolute Airflow Convergence Tolerance {kg/s}",
         "  -0.5,                    !- Convergence Acceleration Limit {dimensionless}",
         "  0.0,                     !- Azimuth Angle of Long Axis of Building {deg}",
         "  1.0;                     !- Ratio of Building Width Along Short Axis to Width Along Long Axis",
         "AirflowNetwork:MultiZone:Zone,",
         "  WEST_ZONE,               !- Zone Name",
         "  NoVent,                  !- Ventilation Control Mode",
         "  ,                        !- Ventilation Control Zone Temperature Setpoint Schedule Name",
         "  0.3,                     !- Minimum Venting Open Factor {dimensionless}",
         "  5.0,                     !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor {deltaC}",
         "  10.0,                    !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor {deltaC}",
         "  0.0,                     !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor {deltaJ/kg}",
         "  300000.0;                !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor {deltaJ/kg}",
         "AirflowNetwork:MultiZone:Zone,",
         "  EAST_ZONE,               !- Zone Name",
         "  NoVent,                  !- Ventilation Control Mode",
         "  ,                        !- Ventilation Control Zone Temperature Setpoint Schedule Name",
         "  1.0,                     !- Minimum Venting Open Factor {dimensionless}",
         "  0.0,                     !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor {deltaC}",
         "  100.0,                   !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor {deltaC}",
         "  0.0,                     !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor {deltaJ/kg}",
         "  300000.0;                !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor {deltaJ/kg}",
         "AirflowNetwork:MultiZone:Zone,",
         "  NORTH_ZONE,              !- Zone Name",
         "  NoVent,                  !- Ventilation Control Mode",
         "  ,                        !- Ventilation Control Zone Temperature Setpoint Schedule Name",
         "  1.0,                     !- Minimum Venting Open Factor {dimensionless}",
         "  0.0,                     !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor {deltaC}",
         "  100.0,                   !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor {deltaC}",
         "  0.0,                     !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor {deltaJ/kg}",
         "  300000.0;                !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor {deltaJ/kg}",
         "AirflowNetwork:MultiZone:Surface,",
         "  Surface_1,               !- Surface Name",
         "  CR-1,                    !- Leakage Component Name",
         "  SFacade,                 !- External Node Name",
         "  1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",
         "AirflowNetwork:MultiZone:Surface,",
         "  Surface_4,               !- Surface Name",
         "  CR-1,                    !- Leakage Component Name",
         "  ,                        !- External Node Name",
         "  1.0;                     !- Window / Door Opening Factor, or Crack Factor{ dimensionless }",
         "AirflowNetwork:MultiZone:Surface,",
         "  Surface_11,              !- Surface Name",
         "  CR-1,                    !- Leakage Component Name",
         "  ,                        !- External Node Name",
         "  1.0;                     !- Window / Door Opening Factor, or Crack Factor{ dimensionless }",
         "AirflowNetwork:MultiZone:Surface,",
         "  Surface_15,              !- Surface Name",
         "  CR-1,                    !- Leakage Component Name",
         "  NFacade,                 !- External Node Name",
         "  1.0;                     !- Window / Door Opening Factor, or Crack Factor{ dimensionless }",
         "AirflowNetwork:MultiZone:ExternalNode,",
         "  NFacade,                 !- Name",
         "  1.524,                   !- External Node Height{ m }",
         "  NFacade_WPCCurve;        !- Wind Pressure Coefficient Values Object Name",
         "OutdoorAir:Node,",
         "  SFacade,                 !- Name",
         "  ,                        !- Height Above Ground",
         "  ,                        !- Drybulb Temperature Schedule Name",
         "  ,                        !- Wetbulb Temperature Schedule Name",
         "  ,                        !- Wind Speed Schedule Name",
         "  ,                        !- Wind Direction Schedule Name",
         "  SFacade_WPCCurve,        !- Wind Pressure Coefficient Values Object Name",
         "  No,                      !- Symmetric Wind Pressure Coefficient Curve",
         "  Absolute;                !- Wind Angle Type",
         "AirflowNetwork:MultiZone:ReferenceCrackConditions,",
         "  ReferenceCrackConditions,!- Name",
         "  20.0,                    !- Reference Temperature{ C }",
         "  101320,                  !- Reference Barometric Pressure{ Pa }",
         "  0.005;                   !- Reference Humidity Ratio{ kgWater / kgDryAir }",
         "AirflowNetwork:MultiZone:Surface:Crack,",
         "  CR-1,                    !- Name",
         "  0.01,                    !- Air Mass Flow Coefficient at Reference Conditions{ kg / s }",
         "  0.667,                   !- Air Mass Flow Exponent{ dimensionless }",
         "  ReferenceCrackConditions;!- Reference Crack Conditions",
         "Table:OneIndependentVariable,",
         "  NFacade_WPCCurve,        !- Name",
         "  Linear,                  !- Curve Type",
         "  LinearInterpolationOfTable,  !- Interpolation Method",
         "  0,                       !- Minimum Value of X",
         "  360,                     !- Maximum Value of X",
         "  -1,                      !- Minimum Table Output",
         "  1,                       !- Maximum Table Output",
         "  Dimensionless,           !- Input Unit Type for X",
         "  Dimensionless,           !- Output Unit Type",
         "  1,                       !- Normalization Reference",
         "  0, 0.60,                 !- X,Y Pair #1",
         "  30, 0.48,                !- X,Y Pair #2",
         "  60, 0.04,                !- X,Y Pair #3",
         "  90, -0.56,               !- X,Y Pair #4",
         "  120, -0.56,              !- X,Y Pair #5",
         "  150, -0.42,              !- X,Y Pair #6",
         "  180, -0.37,              !- X,Y Pair #7",
         "  210, -0.42,              !- X,Y Pair #8",
         "  240, -0.56,              !- X,Y Pair #9",
         "  270, -0.56,              !- X,Y Pair #10",
         "  300, 0.04,               !- X,Y Pair #11",
         "  330, 0.48,               !- X,Y Pair #12",
         "  360, 0.60;               !- X,Y Pair #13",
         "Table:OneIndependentVariable,",
         "  SFacade_WPCCurve,        !- Name",
         "  Linear,                  !- Curve Type",
         "  LinearInterpolationOfTable,  !- Interpolation Method",
         "  0,                       !- Minimum Value of X",
         "  360,                     !- Maximum Value of X",
         "  -1,                      !- Minimum Table Output",
         "  1,                       !- Maximum Table Output",
         "  Dimensionless,           !- Input Unit Type for X",
         "  Dimensionless,           !- Output Unit Type",
         "  1,                       !- Normalization Reference",
         "  0, -0.37,                !- X,Y Pair #1",
         "  30, -0.42,               !- X,Y Pair #2",
         "  60, -0.56,               !- X,Y Pair #3",
         "  90, -0.56,               !- X,Y Pair #4",
         "  120, 0.04,               !- X,Y Pair #5",
         "  150, 0.48,               !- X,Y Pair #6",
         "  180, 0.60,               !- X,Y Pair #7",
         "  210, 0.48,               !- X,Y Pair #8",
         "  240, 0.04,               !- X,Y Pair #9",
         "  270, -0.56,              !- X,Y Pair #10",
         "  300, -0.56,              !- X,Y Pair #11",
         "  330, -0.42,              !- X,Y Pair #12",
         "  360, -0.37;              !- X,Y Pair #13",
         "SurfaceConvectionAlgorithm:Inside,TARP;",
         "SurfaceConvectionAlgorithm:Outside,DOE-2;",
         "HeatBalanceAlgorithm,ConductionTransferFunction;",
         "ZoneAirHeatBalanceAlgorithm,",
         "  AnalyticalSolution;      !- Algorithm"});
    ASSERT_TRUE(process_idf(idf_objects));

    bool errors = false;

    // Set up some environmental parameters
    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::OutDryBulbTemp = 25.0;
    DataEnvironment::WindDir = 105.0;
    DataEnvironment::OutHumRat = 0.0;        // Dry air only
    DataEnvironment::SiteTempGradient = 0.0; // Disconnect z from testing
    DataEnvironment::SiteWindExp = 0.0;      // Disconnect variation by height
    DataEnvironment::WindSpeed = 10.0;

    bool ErrorsFound = false;
    // Read objects
    SimulationManager::GetProjectData();
    HeatBalanceManager::GetProjectControlData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetZoneData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetWindowGlassSpectralData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetMaterialData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetConstructData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetHeatBalanceInput();
    HeatBalanceManager::AllocateHeatBalArrays();
    DataHVACGlobals::TimeStepSys = DataGlobals::TimeStepZone;
    SurfaceGeometry::GetGeometryParameters(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    // Magic to get surfaces read in correctly
    DataHeatBalance::HeatTransferAlgosUsed.allocate(1);
    DataHeatBalance::HeatTransferAlgosUsed(1) = OverallHeatTransferSolutionAlgo;
    SurfaceGeometry::CosBldgRotAppGonly = 1.0;
    SurfaceGeometry::SinBldgRotAppGonly = 0.0;
    SurfaceGeometry::GetSurfaceData(errors); // setup zone geometry and get zone data
    EXPECT_FALSE(errors);                    // expect no errors

    CurveManager::GetCurveInput();
    EXPECT_EQ(CurveManager::NumCurves, 2);

    DataGlobals::AnyLocalEnvironmentsInModel = true;
    OutAirNodeManager::SetOutAirNodes();
    GetAirflowNetworkInput();
    InitAirflowNetwork();

    // Check the airflow elements
    EXPECT_EQ(2u, DataAirflowNetwork::MultizoneExternalNodeData.size());
    EXPECT_EQ(3u, DataAirflowNetwork::MultizoneZoneData.size());
    EXPECT_EQ(4u, DataAirflowNetwork::MultizoneSurfaceData.size());
    EXPECT_EQ(1u, DataAirflowNetwork::MultizoneSurfaceCrackData.size());
    EXPECT_EQ(2u, DataAirflowNetwork::MultizoneSurfaceStdConditionsCrackData.size());

    EXPECT_EQ(0.0, DataAirflowNetwork::MultizoneExternalNodeData(1).azimuth);
    EXPECT_FALSE(DataAirflowNetwork::MultizoneExternalNodeData(1).symmetricCurve);
    EXPECT_FALSE(DataAirflowNetwork::MultizoneExternalNodeData(1).useRelativeAngle);
    EXPECT_EQ(1, DataAirflowNetwork::MultizoneExternalNodeData(1).curve);

    EXPECT_EQ(180.0, DataAirflowNetwork::MultizoneExternalNodeData(2).azimuth);
    EXPECT_FALSE(DataAirflowNetwork::MultizoneExternalNodeData(2).symmetricCurve);
    EXPECT_FALSE(DataAirflowNetwork::MultizoneExternalNodeData(2).useRelativeAngle);
    EXPECT_EQ(2, DataAirflowNetwork::MultizoneExternalNodeData(2).curve);

    // Make sure we can compute the right wind pressure
    Node(1).OutAirWindSpeed = 1.0;
    Node(1).OutAirDryBulb = 15.0;
    Real64 rho_1 =
        Psychrometrics::PsyRhoAirFnPbTdbW(DataEnvironment::OutBaroPress, DataLoopNode::Node(1).OutAirDryBulb, DataLoopNode::Node(1).HumRat);
    Real64 rho_2 = Psychrometrics::PsyRhoAirFnPbTdbW(DataEnvironment::OutBaroPress, DataEnvironment::OutDryBulbTemp, DataEnvironment::OutHumRat);
    EXPECT_DOUBLE_EQ(1.2252059842834473, rho_1);
    EXPECT_DOUBLE_EQ(1.1841123742118911, rho_2);

    Real64 p = AirflowNetworkBalanceManager::CalcWindPressure(DataAirflowNetwork::MultizoneExternalNodeData(1).curve, false, false, 0.0,
                                                              DataLoopNode::Node(1).OutAirWindSpeed, DataLoopNode::Node(1).OutAirWindDir,
                                                              DataLoopNode::Node(1).OutAirDryBulb, DataLoopNode::Node(1).HumRat);
    EXPECT_DOUBLE_EQ(-0.56 * 0.5 * 1.2252059842834473, p);

    // Run the balance routine, for now only to get the pressure set at the external nodes

    AirflowNetworkBalanceManager::CalcAirflowNetworkAirBalance();
    // Make sure we set the right temperature
    EXPECT_DOUBLE_EQ(25.0, DataAirflowNetwork::AirflowNetworkNodeSimu(4).TZ);
    EXPECT_DOUBLE_EQ(15.0, DataAirflowNetwork::AirflowNetworkNodeSimu(5).TZ);
    EXPECT_DOUBLE_EQ(4.7384645696854548, DataEnvironment::WindSpeedAt(1.524));
    // Global wind speed 10 m/s, temp 25 C; Local wind speed 1 m/s, temp 15 C;
    EXPECT_DOUBLE_EQ(-0.56 * 0.5 * rho_2 * 4.7384645696854548 * 4.7384645696854548, DataAirflowNetwork::AirflowNetworkNodeSimu(4).PZ);
    EXPECT_DOUBLE_EQ(-0.26 * 0.5 * rho_1, DataAirflowNetwork::AirflowNetworkNodeSimu(5).PZ);
}

TEST_F(EnergyPlusFixture, BasicAdvancedSingleSided)
{
    std::string const idf_objects = delimited_string(
        {"Version,9.0;",
         "SimulationControl,",
         "  No,                      !- Do Zone Sizing Calculation",
         "  No,                      !- Do System Sizing Calculation",
         "  No,                      !- Do Plant Sizing Calculation",
         "  No,                      !- Run Simulation for Sizing Periods",
         "  Yes;                     !- Run Simulation for Weather File Run Periods",
         "Building,",
         "  Single Sided Demo Building,  !- Name",
         "  0.0,                     !- North Axis {deg}",
         "  Suburbs,                 !- Terrain",
         "  0.04,                    !- Loads Convergence Tolerance Value",
         "  0.4,                     !- Temperature Convergence Tolerance Value {deltaC}",
         "  FullInteriorAndExterior, !- Solar Distribution",
         "  25,                      !- Maximum Number of Warmup Days",
         "  ;                        !- Minimum Number of Warmup Days",
         "Timestep,4;",
         "Site:Location,",
         "  San Francisco Intl Ap_CA_USA Design_Conditions,  !- Name",
         "  37.62,                   !- Latitude {deg}",
         "  -122.40,                 !- Longitude {deg}",
         "  -8.00,                   !- Time Zone {hr}",
         "  2.00;                    !- Elevation {m}",
         "Site:GroundTemperature:BuildingSurface,19.905,19.922,19.910,19.932,19.949,20.038,20.327,20.062,20.443,20.088,19.986,19.948;",
         "RunPeriod,",
         "  RunPeriod1,              !- Name",
         "  1,                       !- Begin Month",
         "  1,                       !- Begin Day of Month",
         "  ,                        !- Begin Year",
         "  12,                      !- End Month",
         "  31,                      !- End Day of Month",
         "  ,                        !- End Year",
         "  Sunday,                  !- Day of Week for Start Day",
         "  Yes,                     !- Use Weather File Holidays and Special Days",
         "  Yes,                     !- Use Weather File Daylight Saving Period",
         "  No,                      !- Apply Weekend Holiday Rule",
         "  Yes,                     !- Use Weather File Rain Indicators",
         "  Yes;                     !- Use Weather File Snow Indicators",
         "Material,",
         "  F08 Metal surface,       !- Name",
         "  Smooth,                  !- Roughness",
         "  0.0008,                  !- Thickness {m}",
         "  45.28,                   !- Conductivity {W/m-K}",
         "  7824,                    !- Density {kg/m3}",
         "  500;                     !- Specific Heat {J/kg-K}",
         "Material,",
         "  I01 25mm insulation board,  !- Name",
         "  MediumRough,             !- Roughness",
         "  0.0254,                  !- Thickness {m}",
         "  0.03,                    !- Conductivity {W/m-K}",
         "  43,                      !- Density {kg/m3}",
         "  1210;                    !- Specific Heat {J/kg-K}",
         "Material,",
         "  I02 50mm insulation board,  !- Name",
         "  MediumRough,             !- Roughness",
         "  0.0508,                  !- Thickness {m}",
         "  0.03,                    !- Conductivity {W/m-K}",
         "  43,                      !- Density {kg/m3}",
         "  1210;                    !- Specific Heat {J/kg-K}",
         "Material,",
         "  G01a 19mm gypsum board,  !- Name",
         "  MediumSmooth,            !- Roughness",
         "  0.019,                   !- Thickness {m}",
         "  0.16,                    !- Conductivity {W/m-K}",
         "  800,                     !- Density {kg/m3}",
         "  1090;                    !- Specific Heat {J/kg-K}",
         "Material,",
         "  M11 100mm lightweight concrete,  !- Name",
         "  MediumRough,             !- Roughness",
         "  0.1016,                  !- Thickness {m}",
         "  0.53,                    !- Conductivity {W/m-K}",
         "  1280,                    !- Density {kg/m3}",
         "  840;                     !- Specific Heat {J/kg-K}",
         "Material,",
         "  F16 Acoustic tile,       !- Name",
         "  MediumSmooth,            !- Roughness",
         "  0.0191,                  !- Thickness {m}",
         "  0.06,                    !- Conductivity {W/m-K}",
         "  368,                     !- Density {kg/m3}",
         "  590;                     !- Specific Heat {J/kg-K}",
         "Material,",
         "  M01 100mm brick,         !- Name",
         "  MediumRough,             !- Roughness",
         "  0.1016,                  !- Thickness {m}",
         "  0.89,                    !- Conductivity {W/m-K}",
         "  1920,                    !- Density {kg/m3}",
         "  790;                     !- Specific Heat {J/kg-K}",
         "Material,",
         "  M15 200mm heavyweight concrete,  !- Name",
         "  MediumRough,             !- Roughness",
         "  0.2032,                  !- Thickness {m}",
         "  1.95,                    !- Conductivity {W/m-K}",
         "  2240,                    !- Density {kg/m3}",
         "  900;                     !- Specific Heat {J/kg-K}",
         "Material,",
         "  M05 200mm concrete block,!- Name",
         "  MediumRough,             !- Roughness",
         "  0.2032,                  !- Thickness {m}",
         "  1.11,                    !- Conductivity {W/m-K}",
         "  800,                     !- Density {kg/m3}",
         "  920;                     !- Specific Heat {J/kg-K}",
         "Material,",
         "  G05 25mm wood,           !- Name",
         "  MediumSmooth,            !- Roughness",
         "  0.0254,                  !- Thickness {m}",
         "  0.15,                    !- Conductivity {W/m-K}",
         "  608,                     !- Density {kg/m3}",
         "  1630;                    !- Specific Heat {J/kg-K}",
         "Material:AirGap,",
         "  F04 Wall air space resistance,  !- Name",
         "  0.15;                    !- Thermal Resistance {m2-K/W}",
         "Material:AirGap,",
         "  F05 Ceiling air space resistance,  !- Name",
         "  0.18;                    !- Thermal Resistance {m2-K/W}",
         "WindowMaterial:Glazing,",
         "  Clear 3mm,               !- Name",
         "  SpectralAverage,         !- Optical Data Type",
         "  ,                        !- Window Glass Spectral Data Set Name",
         "  0.003,                   !- Thickness {m}",
         "  0.837,                   !- Solar Transmittance at Normal Incidence",
         "  0.075,                   !- Front Side Solar Reflectance at Normal Incidence",
         "  0.075,                   !- Back Side Solar Reflectance at Normal Incidence",
         "  0.898,                   !- Visible Transmittance at Normal Incidence",
         "  0.081,                   !- Front Side Visible Reflectance at Normal Incidence",
         "  0.081,                   !- Back Side Visible Reflectance at Normal Incidence",
         "  0,                       !- Infrared Transmittance at Normal Incidence",
         "  0.84,                    !- Front Side Infrared Hemispherical Emissivity",
         "  0.84,                    !- Back Side Infrared Hemispherical Emissivity",
         "  0.9;                     !- Conductivity {W/m-K}",
         "WindowMaterial:Gas,",
         "  Air 13mm,                !- Name",
         "  Air,                     !- Gas Type",
         "  0.0127;                  !- Thickness {m}",
         "Construction,",
         "  Exterior Floor,          !- Name",
         "  I02 50mm insulation board,  !- Outside Layer",
         "  M15 200mm heavyweight concrete;  !- Layer 2",
         "Construction,",
         "  Exterior Wall,           !- Name",
         "  M01 100mm brick,         !- Outside Layer",
         "  M15 200mm heavyweight concrete,  !- Layer 2",
         "  I02 50mm insulation board,  !- Layer 3",
         "  F04 Wall air space resistance,  !- Layer 4",
         "  G01a 19mm gypsum board;  !- Layer 5",
         "Construction,",
         "  Interior Wall,           !- Name",
         "  G01a 19mm gypsum board,  !- Outside Layer",
         "  F04 Wall air space resistance,  !- Layer 2",
         "  G01a 19mm gypsum board;  !- Layer 3",
         "Construction,",
         "  Exterior Roof,           !- Name",
         "  M11 100mm lightweight concrete,  !- Outside Layer",
         "  F05 Ceiling air space resistance,  !- Layer 2",
         "  F16 Acoustic tile;       !- Layer 3",
         "Construction,",
         "  Exterior Window,         !- Name",
         "  Clear 3mm,               !- Outside Layer",
         "  Air 13mm,                !- Layer 2",
         "  Clear 3mm;               !- Layer 3",
         "GlobalGeometryRules,",
         "  UpperLeftCorner,         !- Starting Vertex Position",
         "  Counterclockwise,        !- Vertex Entry Direction",
         "  World;                   !- Coordinate System",
         "Zone,",
         "  West_Zone,               !- Name",
         "  0.0,                     !- Direction of Relative North {deg}",
         "  0.0,                     !- X Origin {m}",
         "  0.0,                     !- Y Origin {m}",
         "  0.0,                     !- Z Origin {m}",
         "  ,                        !- Type",
         "  1;                       !- Multiplier",
         "BuildingSurface:Detailed,",
         "  West_Zone_Floor,         !- Name",
         "  Floor,                   !- Surface Type",
         "  Exterior Floor,          !- Construction Name",
         "  West_Zone,               !- Zone Name",
         "  Ground,                  !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  NoSun,                   !- Sun Exposure",
         "  NoWind,                  !- Wind Exposure",
         "  0.0,                     !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  6.096000000000,12.192000000000,0.000000000000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  6.096000000000,0.000000000000,0.000000000000,  !- X,Y,Z ==> Vertex 2 {m}",
         "  0.000000000000,0.000000000000,0.000000000000,  !- X,Y,Z ==> Vertex 3 {m}",
         "  0.000000000000,12.192000000000,0.000000000000;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  West_Zone_West_Wall,     !- Name",
         "  Wall,                    !- Surface Type",
         "  Exterior Wall,           !- Construction Name",
         "  West_Zone,               !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  ,                        !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  0.000000000000,12.192000000000,3.048000000000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  0.000000000000,12.192000000000,0.000000000000,  !- X,Y,Z ==> Vertex 2 {m}",
         "  0.000000000000,0.000000000000,0.000000000000,  !- X,Y,Z ==> Vertex 3 {m}",
         "  0.000000000000,0.000000000000,3.048000000000;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  West_Zone_South_Wall,    !- Name",
         "  Wall,                    !- Surface Type",
         "  Exterior Wall,           !- Construction Name",
         "  West_Zone,               !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  ,                        !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  0.000000000000,0.000000000000,3.048000000000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  0.000000000000,0.000000000000,0.000000000000,  !- X,Y,Z ==> Vertex 2 {m}",
         "  6.096000000000,0.000000000000,0.000000000000,  !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096000000000,0.000000000000,3.048000000000;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  West_Zone_North_Wall,    !- Name",
         "  Wall,                    !- Surface Type",
         "  Exterior Wall,           !- Construction Name",
         "  West_Zone,               !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  ,                        !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  6.096000000000,12.192000000000,3.048000000000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  6.096000000000,12.192000000000,0.000000000000,  !- X,Y,Z ==> Vertex 2 {m}",
         "  0.000000000000,12.192000000000,0.000000000000,  !- X,Y,Z ==> Vertex 3 {m}",
         "  0.000000000000,12.192000000000,3.048000000000;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  West_Zone_Roof,          !- Name",
         "  Roof,                    !- Surface Type",
         "  Exterior Roof,           !- Construction Name",
         "  West_Zone,               !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  ,                        !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  0.000000000000,12.192000000000,3.048000000000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  0.000000000000,0.000000000000,3.048000000000,  !- X,Y,Z ==> Vertex 2 {m}",
         "  6.096000000000,0.000000000000,3.048000000000,  !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096000000000,12.192000000000,3.048000000000;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  West_Zone_East_Wall_S,   !- Name",
         "  Wall,                    !- Surface Type",
         "  Exterior Wall,           !- Construction Name",
         "  West_Zone,               !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  ,                        !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  6.096000000000,0.000000000000,3.048000000000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  6.096000000000,0.000000000000,0.000000000000,  !- X,Y,Z ==> Vertex 2 {m}",
         "  6.096000000000,6.096000000000,0.000000000000,  !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096000000000,6.096000000000,3.048000000000;  !- X,Y,Z ==> Vertex 4 {m}",
         "BuildingSurface:Detailed,",
         "  West_Zone_East_Wall_N,   !- Name",
         "  Wall,                    !- Surface Type",
         "  Exterior Wall,           !- Construction Name",
         "  West_Zone,               !- Zone Name",
         "  Outdoors,                !- Outside Boundary Condition",
         "  ,                        !- Outside Boundary Condition Object",
         "  SunExposed,              !- Sun Exposure",
         "  WindExposed,             !- Wind Exposure",
         "  ,                        !- View Factor to Ground",
         "  4,                       !- Number of Vertices",
         "  6.096000000000,6.096000000000,3.048000000000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  6.096000000000,6.096000000000,0.000000000000,  !- X,Y,Z ==> Vertex 2 {m}",
         "  6.096000000000,12.192000000000,0.000000000000,  !- X,Y,Z ==> Vertex 3 {m}",
         "  6.096000000000,12.192000000000,3.048000000000;  !- X,Y,Z ==> Vertex 4 {m}",
         "FenestrationSurface:Detailed,",
         "  West_Zone_West_Wall_Left_Window,  !- Name",
         "  Window,                  !- Surface Type",
         "  Exterior Window,         !- Construction Name",
         "  West_Zone_West_Wall,     !- Building Surface Name",
         "  ,                        !- Outside Boundary Condition Object",
         "  ,                        !- View Factor to Ground",
         "  ,                        !- Frame and Divider Name",
         "  ,                        !- Multiplier",
         "  4,                       !- Number of Vertices",
         "  0.000000000000,10.668000000000,2.286000000000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  0.000000000000,10.668000000000,0.762000000000,  !- X,Y,Z ==> Vertex 2 {m}",
         "  0.000000000000,9.144000000000,0.762000000000,  !- X,Y,Z ==> Vertex 3 {m}",
         "  0.000000000000,9.144000000000,2.286000000000;  !- X,Y,Z ==> Vertex 4 {m}",
         "FenestrationSurface:Detailed,",
         "  West_Zone_West_Wall_Right_Window,  !- Name",
         "  Window,                  !- Surface Type",
         "  Exterior Window,         !- Construction Name",
         "  West_Zone_West_Wall,     !- Building Surface Name",
         "  ,                        !- Outside Boundary Condition Object",
         "  ,                        !- View Factor to Ground",
         "  ,                        !- Frame and Divider Name",
         "  ,                        !- Multiplier",
         "  4,                       !- Number of Vertices",
         "  0.000000000000,3.048000000000,2.286000000000,  !- X,Y,Z ==> Vertex 1 {m}",
         "  0.000000000000,3.048000000000,0.762000000000,  !- X,Y,Z ==> Vertex 2 {m}",
         "  0.000000000000,1.524000000000,0.762000000000,  !- X,Y,Z ==> Vertex 3 {m}",
         "  0.000000000000,1.524000000000,2.286000000000;  !- X,Y,Z ==> Vertex 4 {m}",
         "AirflowNetwork:SimulationControl,",
         "  AFN_SimulationControl_1, !- Name",
         "  MultizoneWithoutDistribution,  !- AirflowNetwork Control",
         "  SurfaceAverageCalculation,  !- Wind Pressure Coefficient Type",
         "  OpeningHeight,           !- Height Selection for Local Wind Pressure Calculation",
         "  LowRise,                 !- Building Type",
         "  500,                     !- Maximum Number of Iterations {dimensionless}",
         "  ZeroNodePressures,       !- Initialization Type",
         "  1.0E-05,                 !- Relative Airflow Convergence Tolerance {dimensionless}",
         "  1.0E-06,                 !- Absolute Airflow Convergence Tolerance {kg/s}",
         "  -0.5,                    !- Convergence Acceleration Limit {dimensionless}",
         "  90,                      !- Azimuth Angle of Long Axis of Building {deg}",
         "  0.4;                     !- Ratio of Building Width Along Short Axis to Width Along Long Axis",
         "AirflowNetwork:MultiZone:Zone,",
         "  West_Zone,               !- Zone Name",
         "  Constant,                !- Ventilation Control Mode",
         "  ,                        !- Ventilation Control Zone Temperature Setpoint Schedule Name",
         "  ,                        !- Minimum Venting Open Factor {dimensionless}",
         "  ,                        !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor {deltaC}",
         "  ,                        !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor {deltaC}",
         "  ,                        !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor {deltaJ/kg}",
         "  ,                        !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor {deltaJ/kg}",
         "  ,                        !- Venting Availability Schedule Name",
         "  Advanced,                !- Single Sided Wind Pressure Coefficient Algorithm",
         "  12.19;                   !- Facade Width {m}",
         "AirflowNetwork:MultiZone:Surface,",
         "  West_Zone_West_Wall_Right_Window,  !- Surface Name",
         "  SliderWindow,            !- Leakage Component Name",
         "  ,                        !- External Node Name",
         "  1;                       !- Window/Door Opening Factor, or Crack Factor {dimensionless}",
         "AirflowNetwork:MultiZone:Surface,",
         "  West_Zone_West_Wall_Left_Window,  !- Surface Name",
         "  SliderWindow,            !- Leakage Component Name",
         "  ,                        !- External Node Name",
         "  1;                       !- Window/Door Opening Factor, or Crack Factor {dimensionless}",
         "",
         "AirflowNetwork:MultiZone:Surface,",
         "  West_Zone_West_Wall,     !- Surface Name",
         "  WallCrack,               !- Leakage Component Name",
         "  ,                        !- External Node Name",
         "  1;                       !- Window/Door Opening Factor, or Crack Factor {dimensionless}",
         "AirflowNetwork:MultiZone:Component:DetailedOpening,",
         "  SliderWindow,            !- Name",
         "  0.001,                   !- Air Mass Flow Coefficient When Opening is Closed {kg/s-m}",
         "  0.667,                   !- Air Mass Flow Exponent When Opening is Closed {dimensionless}",
         "  NonPivoted,              !- Type of Rectangular Large Vertical Opening (LVO)",
         "  0.0,                     !- Extra Crack Length or Height of Pivoting Axis {m}",
         "  2,                       !- Number of Sets of Opening Factor Data",
         "  0.0,                     !- Opening Factor 1 {dimensionless}",
         "  0.61,                    !- Discharge Coefficient for Opening Factor 1 {dimensionless}",
         "  0.0,                     !- Width Factor for Opening Factor 1 {dimensionless}",
         "  1.0,                     !- Height Factor for Opening Factor 1 {dimensionless}",
         "  0.0,                     !- Start Height Factor for Opening Factor 1 {dimensionless}",
         "  1.0,                     !- Opening Factor 2 {dimensionless}",
         "  0.61,                    !- Discharge Coefficient for Opening Factor 2 {dimensionless}",
         "  0.5,                     !- Width Factor for Opening Factor 2 {dimensionless}",
         "  1.0,                     !- Height Factor for Opening Factor 2 {dimensionless}",
         "  0.0;                     !- Start Height Factor for Opening Factor 2 {dimensionless}",
         "AirflowNetwork:MultiZone:Surface:Crack,",
         "  WallCrack,               !- Name",
         "  0.01,                    !- Air Mass Flow Coefficient at Reference Conditions {kg/s}",
         "  0.667;                   !- Air Mass Flow Exponent {dimensionless}",
         "AirflowNetwork:MultiZone:ReferenceCrackConditions,",
         "  ReferenceCrackConditions,!- Name",
         "  20.0,                    !- Reference Temperature {C}",
         "  101320,                  !- Reference Barometric Pressure {Pa}",
         "  0.005;                   !- Reference Humidity Ratio {kgWater/kgDryAir}"});

    std::vector<Real64> valsForLeftWindow = {
        -1.3130779955194276,  -1.7404152241877022,  -1.9384350312723766,  -1.8112879523426120,   -1.4903484929957291,  -1.1589328567607411,
        -0.90795075070620501, -0.75899946242534944, -0.70518117657458634, -0.73794026769189536,  -0.70518117657458634, -0.75899946242534944,
        -0.90795075070620501, -1.1589328567607411,  -1.4903484929957291,  -1.8112879523426120,   -1.9384350312723766,  -1.7404152241877022,
        -1.3130779955194276,  -0.81787534059945755, -0.32789581427586245, -0.051561623181424314, 0.15922353989407620,  0.38420139813526627,
        0.61892682388527165,  0.85109949645405880,  1.0664888091014251,   1.2510276050004789,    1.0664888091014251,   0.85109949645405880,
        0.61892682388527165,  0.38420139813526627,  0.15922353989407620,  -0.051561623181424314, -0.32789581427586245, -0.81787534059945755,
        -1.3130779955194276};

    std::vector<Real64> valsForRightWindow = {
        -0.56146269488642231,  -0.81031499432463261,  -0.88587800418632712,  -0.70219756773378639, -0.39543597375365452,  -0.14821874325853215,
        -0.045339946833489957, -0.097330100392452740, -0.28213089764929783,  -0.57310708635195429, -0.28213089764929783,  -0.097330100392452740,
        -0.045339946833489957, -0.14821874325853215,  -0.39543597375365452,  -0.70219756773378639, -0.88587800418632712,  -0.81031499432463261,
        -0.56146269488642231,  -0.28653692388308377,  -0.041152159946210520, 0.37465991281286887,  0.81696925904461237,   1.1829453813575432,
        1.4391966568855996,    1.5699546250680769,    1.5837385005116038,    1.5105973452216215,   1.5837385005116038,    1.5699546250680769,
        1.4391966568855996,    1.1829453813575432,    0.81696925904461237,   0.37465991281286887,  -0.041152159946210520, -0.28653692388308377,
        -0.56146269488642231};

    ASSERT_TRUE(process_idf(idf_objects));

    bool errors = false;

    HeatBalanceManager::GetMaterialData(errors); // read material data
    EXPECT_FALSE(errors);                        // expect no errors

    HeatBalanceManager::GetConstructData(errors); // read construction data
    EXPECT_FALSE(errors);                         // expect no errors

    HeatBalanceManager::GetZoneData(errors); // read zone data
    EXPECT_FALSE(errors);                    // expect no errors

    // Magic to get surfaces read in correctly
    DataHeatBalance::HeatTransferAlgosUsed.allocate(1);
    DataHeatBalance::HeatTransferAlgosUsed(1) = OverallHeatTransferSolutionAlgo;
    SurfaceGeometry::CosBldgRotAppGonly = 1.0;
    SurfaceGeometry::SinBldgRotAppGonly = 0.0;

    SurfaceGeometry::GetSurfaceData(errors); // setup zone geometry and get zone data
    EXPECT_FALSE(errors);                    // expect no errors

    CurveManager::GetCurveInput();
    EXPECT_EQ(0, CurveManager::NumCurves);

    AirflowNetworkBalanceManager::GetAirflowNetworkInput();

    // Check that the correct number of curves has been generated (5 facade directions + 2 windows)
    EXPECT_EQ(7, CurveManager::NumCurves);

    // Check the airflow elements
    ASSERT_EQ(3u, DataAirflowNetwork::MultizoneExternalNodeData.size());
    EXPECT_EQ(1u, DataAirflowNetwork::MultizoneZoneData.size());
    EXPECT_EQ(3u, DataAirflowNetwork::MultizoneSurfaceData.size());
    EXPECT_EQ(1u, DataAirflowNetwork::MultizoneSurfaceCrackData.size());
    EXPECT_EQ(1u, DataAirflowNetwork::MultizoneCompDetOpeningData.size());
    EXPECT_EQ(2u, DataAirflowNetwork::MultizoneSurfaceStdConditionsCrackData.size());

    EXPECT_EQ(270.0, DataAirflowNetwork::MultizoneExternalNodeData(1).azimuth);
    EXPECT_EQ(270.0, DataAirflowNetwork::MultizoneExternalNodeData(2).azimuth);
    EXPECT_EQ(270.0, DataAirflowNetwork::MultizoneExternalNodeData(3).azimuth);

    // Check the curve values for the left window, taken from v8.6.0 on Windows
    unsigned i = 0;
    for (auto value : CurveManager::PerfCurveTableData(7).Y) {
        EXPECT_NEAR(valsForLeftWindow[i++], value, 1.0e-12) << ("Issue at index: " + std::to_string(i));
    }

    // Check the curve values for the left window, taken from v8.6.0 on Windows
    i = 0;
    for (auto value : CurveManager::PerfCurveTableData(6).Y) {
        EXPECT_NEAR(valsForRightWindow[i++], value, 1.0e-12) << ("Issue at index: " + std::to_string(i));
    }
}

TEST_F(EnergyPlusFixture, MultiAirLoopTest)
{

    std::string const idf_objects = delimited_string({

        "  Version,8.9;",

        "  Building,",
        "    Small Office with AirflowNetwork model,  !- Name",
        "    0,                       !- North Axis {deg}",
        "    Suburbs,                 !- Terrain",
        "    0.001,                   !- Loads Convergence Tolerance Value",
        "    0.0050000,               !- Temperature Convergence Tolerance Value {deltaC}",
        "    FullInteriorAndExterior, !- Solar Distribution",
        "    25,                      !- Maximum Number of Warmup Days",
        "    6;                       !- Minimum Number of Warmup Days",

        "  Timestep,6;",

        "  SurfaceConvectionAlgorithm:Inside,TARP;",

        "  SurfaceConvectionAlgorithm:Outside,DOE-2;",

        "  HeatBalanceAlgorithm,ConductionTransferFunction;",

        "  Output:DebuggingData,0,0;",

        "  ZoneCapacitanceMultiplier:ResearchSpecial,",
        "    Multiplier,              !- Name",
        "    ,                        !- Zone or ZoneList Name",
        "    1.0,                     !- Temperature Capacity Multiplier",
        "    1.0,                     !- Humidity Capacity Multiplier",
        "    1.0,                     !- Carbon Dioxide Capacity Multiplier",
        "    ;                        !- Generic Contaminant Capacity Multiplier",

        "  SimulationControl,",
        "    No,                     !- Do Zone Sizing Calculation",
        "    No,                     !- Do System Sizing Calculation",
        "    No,                      !- Do Plant Sizing Calculation",
        "    Yes,                     !- Run Simulation for Sizing Periods",
        "    No;                      !- Run Simulation for Weather File Run Periods",

        "  Sizing:Parameters,",
        "    1.53,                    !- Heating Sizing Factor",
        "    1.70,                    !- Cooling Sizing Factor",
        "    6;                       !- Timesteps in Averaging Window",

        "  RunPeriod,",
        "    Spring run,              !- Name",
        "    4,                       !- Begin Month",
        "    1,                       !- Begin Day of Month",
        "    ,                        !- Begin Year",
        "    4,                       !- End Month",
        "    1,                       !- End Day of Month",
        "    ,                        !- End Year",
        "    Tuesday,                 !- Day of Week for Start Day",
        "    Yes,                     !- Use Weather File Holidays and Special Days",
        "    Yes,                     !- Use Weather File Daylight Saving Period",
        "    No,                      !- Apply Weekend Holiday Rule",
        "    Yes,                     !- Use Weather File Rain Indicators",
        "    Yes;                     !- Use Weather File Snow Indicators",

        "  RunPeriod,",
        "    Winter run,              !- Name",
        "    1,                       !- Begin Month",
        "    14,                      !- Begin Day of Month",
        "    ,                        !- Begin Year",
        "    1,                       !- End Month",
        "    14,                      !- End Day of Month",
        "    ,                        !- End Year",
        "    Tuesday,                 !- Day of Week for Start Day",
        "    Yes,                     !- Use Weather File Holidays and Special Days",
        "    Yes,                     !- Use Weather File Daylight Saving Period",
        "    No,                      !- Apply Weekend Holiday Rule",
        "    Yes,                     !- Use Weather File Rain Indicators",
        "    Yes;                     !- Use Weather File Snow Indicators",

        "  RunPeriod,",
        "    Summer run,              !- Name",
        "    7,                       !- Begin Month",
        "    7,                       !- Begin Day of Month",
        "    ,                        !- Begin Year",
        "    7,                       !- End Month",
        "    7,                       !- End Day of Month",
        "    ,                        !- End Year",
        "    Tuesday,                 !- Day of Week for Start Day",
        "    Yes,                     !- Use Weather File Holidays and Special Days",
        "    Yes,                     !- Use Weather File Daylight Saving Period",
        "    No,                      !- Apply Weekend Holiday Rule",
        "    Yes,                     !- Use Weather File Rain Indicators",
        "    Yes;                     !- Use Weather File Snow Indicators",

        "  Site:Location,",
        "    CHICAGO_IL_USA TMY2-94846,  !- Name",
        "    41.78,                   !- Latitude {deg}",
        "    -87.75,                  !- Longitude {deg}",
        "    -6.00,                   !- Time Zone {hr}",
        "    190.00;                  !- Elevation {m}",

        "  SizingPeriod:DesignDay,",
        "    CHICAGO_IL_USA Annual Heating 99% Design Conditions DB,  !- Name",
        "    1,                       !- Month",
        "    21,                      !- Day of Month",
        "    WinterDesignDay,         !- Day Type",
        "    -17.3,                   !- Maximum Dry-Bulb Temperature {C}",
        "    0.0,                     !- Daily Dry-Bulb Temperature Range {deltaC}",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Type",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name",
        "    Wetbulb,                 !- Humidity Condition Type",
        "    -17.3,                   !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}",
        "    ,                        !- Humidity Condition Day Schedule Name",
        "    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}",
        "    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}",
        "    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}",
        "    99063.,                  !- Barometric Pressure {Pa}",
        "    4.9,                     !- Wind Speed {m/s}",
        "    270,                     !- Wind Direction {deg}",
        "    No,                      !- Rain Indicator",
        "    No,                      !- Snow Indicator",
        "    No,                      !- Daylight Saving Time Indicator",
        "    ASHRAEClearSky,          !- Solar Model Indicator",
        "    ,                        !- Beam Solar Day Schedule Name",
        "    ,                        !- Diffuse Solar Day Schedule Name",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensionless}",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimensionless}",
        "    0.0;                     !- Sky Clearness",

        "  SizingPeriod:DesignDay,",
        "    CHICAGO_IL_USA Annual Cooling 1% Design Conditions DB/MCWB,  !- Name",
        "    7,                       !- Month",
        "    21,                      !- Day of Month",
        "    SummerDesignDay,         !- Day Type",
        "    31.5,                    !- Maximum Dry-Bulb Temperature {C}",
        "    10.7,                    !- Daily Dry-Bulb Temperature Range {deltaC}",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Type",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name",
        "    Wetbulb,                 !- Humidity Condition Type",
        "    23.0,                    !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}",
        "    ,                        !- Humidity Condition Day Schedule Name",
        "    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}",
        "    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}",
        "    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}",
        "    99063.,                  !- Barometric Pressure {Pa}",
        "    5.3,                     !- Wind Speed {m/s}",
        "    230,                     !- Wind Direction {deg}",
        "    No,                      !- Rain Indicator",
        "    No,                      !- Snow Indicator",
        "    No,                      !- Daylight Saving Time Indicator",
        "    ASHRAEClearSky,          !- Solar Model Indicator",
        "    ,                        !- Beam Solar Day Schedule Name",
        "    ,                        !- Diffuse Solar Day Schedule Name",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensionless}",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimensionless}",
        "    1.0;                     !- Sky Clearness",

        "  Material,",
        "    A1 - 1 IN STUCCO,        !- Name",
        "    Smooth,                  !- Roughness",
        "    2.5389841E-02,           !- Thickness {m}",
        "    0.6918309,               !- Conductivity {W/m-K}",
        "    1858.142,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.9200000,               !- Solar Absorptance",
        "    0.9200000;               !- Visible Absorptance",

        "  Material,",
        "    C4 - 4 IN COMMON BRICK,  !- Name",
        "    Rough,                   !- Roughness",
        "    0.1014984,               !- Thickness {m}",
        "    0.7264224,               !- Conductivity {W/m-K}",
        "    1922.216,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7600000,               !- Solar Absorptance",
        "    0.7600000;               !- Visible Absorptance",

        "  Material,",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Name",
        "    Smooth,                  !- Roughness",
        "    1.9050000E-02,           !- Thickness {m}",
        "    0.7264224,               !- Conductivity {W/m-K}",
        "    1601.846,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.9200000,               !- Solar Absorptance",
        "    0.9200000;               !- Visible Absorptance",

        "  Material,",
        "    C6 - 8 IN CLAY TILE,     !- Name",
        "    Smooth,                  !- Roughness",
        "    0.2033016,               !- Thickness {m}",
        "    0.5707605,               !- Conductivity {W/m-K}",
        "    1121.292,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.8200000,               !- Solar Absorptance",
        "    0.8200000;               !- Visible Absorptance",

        "  Material,",
        "    C10 - 8 IN HW CONCRETE,  !- Name",
        "    MediumRough,             !- Roughness",
        "    0.2033016,               !- Thickness {m}",
        "    1.729577,                !- Conductivity {W/m-K}",
        "    2242.585,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.6500000,               !- Solar Absorptance",
        "    0.6500000;               !- Visible Absorptance",

        "  Material,",
        "    E2 - 1 / 2 IN SLAG OR STONE,  !- Name",
        "    Rough,                   !- Roughness",
        "    1.2710161E-02,           !- Thickness {m}",
        "    1.435549,                !- Conductivity {W/m-K}",
        "    881.0155,                !- Density {kg/m3}",
        "    1673.600,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.5500000,               !- Solar Absorptance",
        "    0.5500000;               !- Visible Absorptance",

        "  Material,",
        "    E3 - 3 / 8 IN FELT AND MEMBRANE,  !- Name",
        "    Rough,                   !- Roughness",
        "    9.5402403E-03,           !- Thickness {m}",
        "    0.1902535,               !- Conductivity {W/m-K}",
        "    1121.292,                !- Density {kg/m3}",
        "    1673.600,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",

        "  Material,",
        "    B5 - 1 IN DENSE INSULATION,  !- Name",
        "    VeryRough,               !- Roughness",
        "    2.5389841E-02,           !- Thickness {m}",
        "    4.3239430E-02,           !- Conductivity {W/m-K}",
        "    91.30524,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.5000000,               !- Solar Absorptance",
        "    0.5000000;               !- Visible Absorptance",

        "  Material,",
        "    C12 - 2 IN HW CONCRETE,  !- Name",
        "    MediumRough,             !- Roughness",
        "    5.0901599E-02,           !- Thickness {m}",
        "    1.729577,                !- Conductivity {W/m-K}",
        "    2242.585,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.6500000,               !- Solar Absorptance",
        "    0.6500000;               !- Visible Absorptance",

        "  Material,",
        "    1.375in-Solid-Core,      !- Name",
        "    Smooth,                  !- Roughness",
        "    3.4925E-02,              !- Thickness {m}",
        "    0.1525000,               !- Conductivity {W/m-K}",
        "    614.5000,                !- Density {kg/m3}",
        "    1630.0000,               !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.9200000,               !- Solar Absorptance",
        "    0.9200000;               !- Visible Absorptance",

        "  WindowMaterial:Glazing,",
        "    ELECTRO GLASS LIGHT STATE,  !- Name",
        "    SpectralAverage,         !- Optical Data Type",
        "    ,                        !- Window Glass Spectral Data Set Name",
        "    0.006,                   !- Thickness {m}",
        "    0.814,                   !- Solar Transmittance at Normal Incidence",
        "    0.086,                   !- Front Side Solar Reflectance at Normal Incidence",
        "    0.086,                   !- Back Side Solar Reflectance at Normal Incidence",
        "    0.847,                   !- Visible Transmittance at Normal Incidence",
        "    0.099,                   !- Front Side Visible Reflectance at Normal Incidence",
        "    0.099,                   !- Back Side Visible Reflectance at Normal Incidence",
        "    0.0,                     !- Infrared Transmittance at Normal Incidence",
        "    0.84,                    !- Front Side Infrared Hemispherical Emissivity",
        "    0.84,                    !- Back Side Infrared Hemispherical Emissivity",
        "    0.9;                     !- Conductivity {W/m-K}",

        "  WindowMaterial:Glazing,",
        "    ELECTRO GLASS DARK STATE,!- Name",
        "    SpectralAverage,         !- Optical Data Type",
        "    ,                        !- Window Glass Spectral Data Set Name",
        "    0.006,                   !- Thickness {m}",
        "    0.111,                   !- Solar Transmittance at Normal Incidence",
        "    0.179,                   !- Front Side Solar Reflectance at Normal Incidence",
        "    0.179,                   !- Back Side Solar Reflectance at Normal Incidence",
        "    0.128,                   !- Visible Transmittance at Normal Incidence",
        "    0.081,                   !- Front Side Visible Reflectance at Normal Incidence",
        "    0.081,                   !- Back Side Visible Reflectance at Normal Incidence",
        "    0.0,                     !- Infrared Transmittance at Normal Incidence",
        "    0.84,                    !- Front Side Infrared Hemispherical Emissivity",
        "    0.84,                    !- Back Side Infrared Hemispherical Emissivity",
        "    0.9;                     !- Conductivity {W/m-K}",

        "  WindowMaterial:Gas,",
        "    WinAirGap,               !- Name",
        "    AIR,                     !- Gas Type",
        "    0.013;                   !- Thickness {m}",

        "  Construction,",
        "    EXTWALL80,               !- Name",
        "    A1 - 1 IN STUCCO,        !- Outside Layer",
        "    C4 - 4 IN COMMON BRICK,  !- Layer 2",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 3",

        "  Construction,",
        "    PARTITION06,             !- Name",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Outside Layer",
        "    C6 - 8 IN CLAY TILE,     !- Layer 2",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 3",

        "  Construction,",
        "    FLOOR SLAB 8 IN,         !- Name",
        "    C10 - 8 IN HW CONCRETE;  !- Outside Layer",

        "  Construction,",
        "    ROOF34,                  !- Name",
        "    E2 - 1 / 2 IN SLAG OR STONE,  !- Outside Layer",
        "    E3 - 3 / 8 IN FELT AND MEMBRANE,  !- Layer 2",
        "    C12 - 2 IN HW CONCRETE;  !- Layer 3",

        "  Construction,",
        "    CEILING:ZONE,            !- Name",
        "    B5 - 1 IN DENSE INSULATION,  !- Outside Layer",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 2",

        "  Construction,",
        "    CEILING:ATTIC,           !- Name",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Outside Layer",
        "    B5 - 1 IN DENSE INSULATION;  !- Layer 2",

        "  Construction,",
        "    ELECTRO-CON-LIGHT,       !- Name",
        "    ELECTRO GLASS LIGHT STATE,  !- Outside Layer",
        "    WinAirGap,               !- Layer 2",
        "    ELECTRO GLASS LIGHT STATE;  !- Layer 3",

        "  Construction,",
        "    ELECTRO-CON-DARK,        !- Name",
        "    ELECTRO GLASS DARK STATE, !- Outside Layer",
        "    WinAirGap,               !- Layer 2",
        "    ELECTRO GLASS DARK STATE; !- Layer 3",

        "  Construction,",
        "    DOOR-CON,                !- Name",
        "    1.375in-Solid-Core;      !- Outside Layer",

        "  Zone,",
        "    West Zone,               !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    autocalculate,           !- Ceiling Height {m}",
        "    autocalculate;           !- Volume {m3}",

        "  Zone,",
        "    EAST ZONE,               !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    autocalculate,           !- Ceiling Height {m}",
        "    autocalculate;           !- Volume {m3}",

        "  Zone,",
        "    NORTH ZONE,              !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    autocalculate,           !- Ceiling Height {m}",
        "    autocalculate;           !- Volume {m3}",

        "  Zone,",
        "    ATTIC ZONE,              !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    autocalculate,           !- Ceiling Height {m}",
        "    autocalculate;           !- Volume {m3}",

        "  Zone,",
        "    ATTIC NORTH ZONE,        !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    autocalculate,           !- Ceiling Height {m}",
        "    autocalculate;           !- Volume {m3}",

        "  GlobalGeometryRules,",
        "    UpperLeftCorner,         !- Starting Vertex Position",
        "    CounterClockWise,        !- Vertex Entry Direction",
        "    World;                   !- Coordinate System",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall001,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    West Zone,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000,0,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall002,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    West Zone,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,0,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall003,           !- Name",
        "    Wall,                    !- Surface Type",
        "    PARTITION06,             !- Construction Name",
        "    West Zone,               !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn003:Wall004,           !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall004,           !- Name",
        "    Wall,                    !- Surface Type",
        "    PARTITION06,             !- Construction Name",
        "    West Zone,               !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn002:Wall004,           !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Flr001,            !- Name",
        "    Floor,                   !- Surface Type",
        "    FLOOR SLAB 8 IN,         !- Construction Name",
        "    West Zone,               !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn001:Flr001,            !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    1.000000,                !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000,0,0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Ceil001,           !- Name",
        "    CEILING,                 !- Surface Type",
        "    CEILING:ZONE,            !- Construction Name",
        "    West Zone,               !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn004:Flr001,            !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,0,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn002:Wall001,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    12.19200,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    9.144000,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    9.144000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn002:Wall002,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    12.19200,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    12.19200,0,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn002:Wall003,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    12.19200,0,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    12.19200,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    12.19200,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    12.19200,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn002:Wall004,           !- Name",
        "    Wall,                    !- Surface Type",
        "    PARTITION06,             !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn001:Wall004,           !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000,0,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn002:Wall005,           !- Name",
        "    Wall,                    !- Surface Type",
        "    PARTITION06,             !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn003:Wall005,           !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    9.144000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    9.144000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn002:Flr001,            !- Name",
        "    Floor,                   !- Surface Type",
        "    FLOOR SLAB 8 IN,         !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn002:Flr001,            !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    1.000000,                !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,0,0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    12.19200,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    12.19200,0,0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn002:Ceil001,           !- Name",
        "    CEILING,                 !- Surface Type",
        "    CEILING:ZONE,            !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn004:Flr002,            !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    12.19200,0,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    12.19200,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn003:Wall001,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    NORTH ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,12.19200,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,12.19200,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn003:Wall002,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    NORTH ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    9.144000,12.19200,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    9.144000,12.19200,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,12.19200,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,12.19200,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn003:Wall003,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    NORTH ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    9.144000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    9.144000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    9.144000,12.19200,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    9.144000,12.19200,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn003:Wall004,           !- Name",
        "    Wall,                    !- Surface Type",
        "    PARTITION06,             !- Construction Name",
        "    NORTH ZONE,              !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn001:Wall003,           !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn003:Wall005,           !- Name",
        "    Wall,                    !- Surface Type",
        "    PARTITION06,             !- Construction Name",
        "    NORTH ZONE,              !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn002:Wall005,           !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    9.144000,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    9.144000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn003:Flr001,            !- Name",
        "    Floor,                   !- Surface Type",
        "    FLOOR SLAB 8 IN,         !- Construction Name",
        "    NORTH ZONE,              !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn003:Flr001,            !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    1.000000,                !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,6.096000,0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,12.19200,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    9.144000,12.19200,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    9.144000,6.096000,0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn003:Ceil001,           !- Name",
        "    CEILING,                 !- Surface Type",
        "    CEILING:ZONE,            !- Construction Name",
        "    NORTH ZONE,              !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn005:Flr001,            !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,12.19200,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    9.144000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    9.144000,12.19200,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Wall001,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,0,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000,0,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Wall002,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,0,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    12.19200,0,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    12.19200,0,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Wall003,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    12.19200,0,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
        "    12.19200,0,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    12.19200,6.096000,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Wall004,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    12.19200,6.096000,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
        "    12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    9.144000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    9.144000,6.096000,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Wall008,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,6.096000,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,0,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,0,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Wall009,           !- Name",
        "    Wall,                    !- Surface Type",
        "    PARTITION06,             !- Construction Name",
        "    ATTIC Zone,               !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn005:Wall004,           !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,6.096000,3.9624,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,6.096000,3.9624;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Wall010,           !- Name",
        "    Wall,                    !- Surface Type",
        "    PARTITION06,             !- Construction Name",
        "    ATTIC ZONE,               !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn005:Wall005,           !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    9.144000,6.096000,3.9624,  !- X,Y,Z ==> Vertex 1 {m}",
        "    9.144000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000,6.096000,3.9624;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Roof001,           !- Name",
        "    Roof,                    !- Surface Type",
        "    ROOF34,                  !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,6.096000,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,0,3.962400,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,0,3.962400,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000,6.096000,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Roof002,           !- Name",
        "    Roof,                    !- Surface Type",
        "    ROOF34,                  !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,6.096000,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,0,3.962400,  !- X,Y,Z ==> Vertex 2 {m}",
        "    12.19200,0,3.962400,  !- X,Y,Z ==> Vertex 3 {m}",
        "    12.19200,6.096000,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Flr001,            !- Name",
        "    Floor,                   !- Surface Type",
        "    CEILING:ATTIC,           !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn001:Ceil001,           !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    1.000000,                !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000,0,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn004:Flr002,            !- Name",
        "    Floor,                   !- Surface Type",
        "    CEILING:ATTIC,           !- Construction Name",
        "    ATTIC ZONE,              !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn002:Ceil001,           !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    1.000000,                !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    12.19200,0,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn005:Wall001,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    ATTIC NORTH ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    9.144000,6.096000,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
        "    9.144000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    9.144000,12.19200,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    9.144000,12.19200,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn005:Wall002,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    ATTIC NORTH ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    9.144000,12.19200,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
        "    9.144000,12.19200,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,12.19200,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,12.19200,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn005:Wall003,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    ATTIC NORTH ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,12.19200,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,12.19200,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,6.096000,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn005:Wall004,           !- Name",
        "    Wall,                    !- Surface Type",
        "    PARTITION06,             !- Construction Name",
        "    ATTIC NORTH ZONE,              !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn004:Wall009,           !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,6.096000,3.9624,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000,6.096000,3.9624;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn005:Wall005,           !- Name",
        "    Wall,                    !- Surface Type",
        "    PARTITION06,             !- Construction Name",
        "    ATTIC NORTH ZONE,              !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn004:Wall010,           !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,6.096000,3.9624,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    9.144000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    9.144000,6.096000,3.9624;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn005:Flr001,            !- Name",
        "    Floor,                   !- Surface Type",
        "    CEILING:ATTIC,           !- Construction Name",
        "    ATTIC NORTH ZONE,              !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn003:Ceil001,           !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    1.000000,                !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,12.19200,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    9.144000,12.19200,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    9.144000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn005:Roof001,           !- Name",
        "    Roof,                    !- Surface Type",
        "    ROOF34,                  !- Construction Name",
        "    ATTIC NORTH ZONE,              !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,12.19200,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,6.096000,3.962400,  !- X,Y,Z ==> Vertex 2 {m}",
        "    9.144000,6.096000,3.962400,  !- X,Y,Z ==> Vertex 3 {m}",
        "    9.144000,12.19200,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

        "  FenestrationSurface:Detailed,",
        "    Zn001:Wall001:Win001,    !- Name",
        "    Window,                  !- Surface Type",
        "    ELECTRO-CON-LIGHT,       !- Construction Name",
        "    Zn001:Wall001,           !- Building Surface Name",
        "    ,                        !- Outside Boundary Condition Object",
        "    0.5000000,               !- View Factor to Ground",
        "    ,     !- Frame and Divider Name",
        "    1.0,                     !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    0.548000,0,2.5000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0.548000,0,0.5000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    5.548000,0,0.5000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    5.548000,0,2.5000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  FenestrationSurface:Detailed,",
        "    Zn001:Wall003:Door001,   !- Name",
        "    DOOR,                    !- Surface Type",
        "    DOOR-CON,                !- Construction Name",
        "    Zn001:Wall003,           !- Building Surface Name",
        "    Zn003:Wall004:Door001,   !- Outside Boundary Condition Object",
        "    0.5000000,               !- View Factor to Ground",
        "    ,                        !- Frame and Divider Name",
        "    1.0,                     !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    3.500,6.096000,2.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    3.500,6.096000,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    2.500,6.096000,0.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    2.500,6.096000,2.0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  FenestrationSurface:Detailed,",
        "    Zn003:Wall002:Win001,    !- Name",
        "    Window,                  !- Surface Type",
        "    ELECTRO-CON-LIGHT,       !- Construction Name",
        "    Zn003:Wall002,           !- Building Surface Name",
        "    ,                        !- Outside Boundary Condition Object",
        "    0.5000000,               !- View Factor to Ground",
        "    ,     !- Frame and Divider Name",
        "    1.0,                     !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    5.548000,12.19200,2.5000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    5.548000,12.19200,0.5000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0.548000,12.19200,0.5000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0.548000,12.19200,2.5000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  FenestrationSurface:Detailed,",
        "    Zn003:Wall004:Door001,   !- Name",
        "    DOOR,                    !- Surface Type",
        "    DOOR-CON,                !- Construction Name",
        "    Zn003:Wall004,           !- Building Surface Name",
        "    Zn001:Wall003:Door001,   !- Outside Boundary Condition Object",
        "    0.5000000,               !- View Factor to Ground",
        "    ,                        !- Frame and Divider Name",
        "    1.0,                     !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    2.500,6.096000,2.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    2.500,6.096000,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    3.500,6.096000,0.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    3.500,6.096000,2.0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  ScheduleTypeLimits,",
        "    Any Number;              !- Name",

        "  ScheduleTypeLimits,",
        "    Fraction,                !- Name",
        "    0.0,                     !- Lower Limit Value",
        "    1.0,                     !- Upper Limit Value",
        "    CONTINUOUS;              !- Numeric Type",

        "  ScheduleTypeLimits,",
        "    Temperature,             !- Name",
        "    -60,                     !- Lower Limit Value",
        "    200,                     !- Upper Limit Value",
        "    CONTINUOUS,              !- Numeric Type",
        "    Temperature;             !- Unit Type",

        "  ScheduleTypeLimits,",
        "    Control Type,            !- Name",
        "    0,                       !- Lower Limit Value",
        "    4,                       !- Upper Limit Value",
        "    DISCRETE;                !- Numeric Type",

        "  ScheduleTypeLimits,",
        "    On/Off,                  !- Name",
        "    0,                       !- Lower Limit Value",
        "    1,                       !- Upper Limit Value",
        "    DISCRETE;                !- Numeric Type",

        "  Schedule:Compact,",
        "    WindowVentSched,         !- Name",
        "    Any Number,              !- Schedule Type Limits Name",
        "    Through: 3/31,           !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,25.55,      !- Field 3",
        "    Through: 9/30,           !- Field 5",
        "    For: AllDays,            !- Field 6",
        "    Until: 24:00,21.11,      !- Field 7",
        "    Through: 12/31,          !- Field 9",
        "    For: AllDays,            !- Field 10",
        "    Until: 24:00,25.55;      !- Field 11",

        "  Schedule:Compact,",
        "    Activity Sch,            !- Name",
        "    Any Number,              !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,131.8;      !- Field 3",

        "  Schedule:Compact,",
        "    Work Eff Sch,            !- Name",
        "    Any Number,              !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,0.0;        !- Field 3",

        "  Schedule:Compact,",
        "    Clothing Sch,            !- Name",
        "    Any Number,              !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,1.0;        !- Field 3",

        "  Schedule:Compact,",
        "    Air Velo Sch,            !- Name",
        "    Any Number,              !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,0.137;      !- Field 3",

        "  Schedule:Compact,",
        "    OFFICE OCCUPANCY,        !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: WeekDays,           !- Field 2",
        "    Until: 6:00,0.0,         !- Field 3",
        "    Until: 7:00,0.10,        !- Field 5",
        "    Until: 8:00,0.50,        !- Field 7",
        "    Until: 12:00,1.00,       !- Field 9",
        "    Until: 13:00,0.50,       !- Field 11",
        "    Until: 16:00,1.00,       !- Field 13",
        "    Until: 17:00,0.50,       !- Field 15",
        "    Until: 18:00,0.10,       !- Field 17",
        "    Until: 24:00,0.0,        !- Field 19",
        "    For: AllOtherDays,       !- Field 21",
        "    Until: 24:00,0.0;        !- Field 22",

        "  Schedule:Compact,",
        "    INTERMITTENT,            !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: WeekDays,           !- Field 2",
        "    Until: 8:00,0.0,         !- Field 3",
        "    Until: 18:00,1.00,       !- Field 5",
        "    Until: 24:00,0.0,        !- Field 7",
        "    For: AllOtherDays,       !- Field 9",
        "    Until: 24:00,0.0;        !- Field 10",

        "  Schedule:Compact,",
        "    OFFICE LIGHTING,         !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: WeekDays,           !- Field 2",
        "    Until: 6:00,0.05,        !- Field 3",
        "    Until: 7:00,0.20,        !- Field 5",
        "    Until: 17:00,1.00,       !- Field 7",
        "    Until: 18:00,0.50,       !- Field 9",
        "    Until: 24:00,0.05,       !- Field 11",
        "    For: AllOtherDays,       !- Field 13",
        "    Until: 24:00,0.05;       !- Field 14",

        "  Schedule:Compact,",
        "    FanAndCoilAvailSched,    !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 3/31,           !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,1.0,        !- Field 3",
        "    Through: 9/30,           !- Field 5",
        "    For: WeekDays SummerDesignDay, !- Field 6",
        "    Until: 7:00,0.0,         !- Field 7",
        "    Until: 17:00,1.0,        !- Field 9",
        "    Until: 24:00,0.0,        !- Field 11",
        "    For: WinterDesignDay,    !- Field 13",
        "    Until: 24:00,0.0,        !- Field 14",
        "    For: AllOtherDays,       !- Field 16",
        "    Until: 24:00,0.0,        !- Field 17",
        "    Through: 12/31,          !- Field 19",
        "    For: AllDays,            !- Field 20",
        "    Until: 24:00,1.0;        !- Field 21",

        "  Schedule:Compact,",
        "    CoolingCoilAvailSched,   !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 3/31,           !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,0.0,        !- Field 3",
        "    Through: 9/30,           !- Field 5",
        "    For: WeekDays SummerDesignDay, !- Field 6",
        "    Until: 7:00,0.0,         !- Field 7",
        "    Until: 17:00,1.0,        !- Field 9",
        "    Until: 24:00,0.0,        !- Field 11",
        "    For: WinterDesignDay,    !- Field 13",
        "    Until: 24:00,0.0,        !- Field 14",
        "    For: AllOtherDays,       !- Field 16",
        "    Until: 24:00,0.0,        !- Field 17",
        "    Through: 12/31,          !- Field 19",
        "    For: AllDays,            !- Field 20",
        "    Until: 24:00,0.0;        !- Field 21",

        "  Schedule:Compact,",
        "    Dual Heating Setpoints,  !- Name",
        "    Temperature,             !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until:  7:00,15.0,       !- Field 3",
        "    Until: 17:00,22.0,       !- Field 5",
        "    Until: 24:00,15.0;       !- Field 7",

        "  Schedule:Compact,",
        "    Dual Cooling Setpoints,  !- Name",
        "    Temperature,             !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until:  7:00,35.0,       !- Field 3",
        "    Until: 17:00,24.0,       !- Field 5",
        "    Until: 24:00,40.0;       !- Field 7",

        "  Schedule:Compact,",
        "    Dual Zone Control Type Sched,  !- Name",
        "    Control Type,            !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,4;          !- Field 3",

        "  Schedule:Compact,",
        "    VentingSched,            !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until:  7:00,1.0,        !- Field 3",
        "    Until: 17:00,0.0,        !- Field 5",
        "    Until: 24:00,1.0;        !- Field 7",

        "  Schedule:Compact,",
        "    Minimum OA Sch,          !- Name",
        "    Temperature,             !- Schedule Type Limits Name",
        "    Through: 3/31,           !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until:  7:00,0.10,       !- Field 3",
        "    Until: 17:00,0.25,       !- Field 5",
        "    Until: 24:00,0.10,       !- Field 7",
        "    Through: 9/30,           !- Field 9",
        "    For: AllDays,            !- Field 10",
        "    Until: 24:00,0.25,       !- Field 11",
        "    Through: 12/31,          !- Field 13",
        "    For: AllDays,            !- Field 14",
        "    Until:  7:00,0.10,       !- Field 15",
        "    Until: 17:00,0.25,       !- Field 17",
        "    Until: 24:00,0.10;       !- Field 19",

        "  People,",
        "    West Zone People,        !- Name",
        "    West Zone,               !- Zone or ZoneList Name",
        "    OFFICE OCCUPANCY,        !- Number of People Schedule Name",
        "    people,                  !- Number of People Calculation Method",
        "    3.000000,                !- Number of People",
        "    ,                        !- People per Zone Floor Area {person/m2}",
        "    ,                        !- Zone Floor Area per Person {m2/person}",
        "    0.3000000,               !- Fraction Radiant",
        "    ,                        !- Sensible Heat Fraction",
        "    Activity Sch,            !- Activity Level Schedule Name",
        "    3.82E-8,                 !- Carbon Dioxide Generation Rate {m3/s-W}",
        "    ,                        !- Enable ASHRAE 55 Comfort Warnings",
        "    zoneaveraged,            !- Mean Radiant Temperature Calculation Type",
        "    ,                        !- Surface Name/Angle Factor List Name",
        "    Work Eff Sch,            !- Work Efficiency Schedule Name",
        "    ClothingInsulationSchedule,  !- Clothing Insulation Calculation Method",
        "    ,                        !- Clothing Insulation Calculation Method Schedule Name",
        "    Clothing Sch,            !- Clothing Insulation Schedule Name",
        "    Air Velo Sch,            !- Air Velocity Schedule Name",
        "    FANGER;                  !- Thermal Comfort Model 1 Type",

        "  People,",
        "    EAST ZONE People,        !- Name",
        "    EAST ZONE,               !- Zone or ZoneList Name",
        "    OFFICE OCCUPANCY,        !- Number of People Schedule Name",
        "    people,                  !- Number of People Calculation Method",
        "    3.000000,                !- Number of People",
        "    ,                        !- People per Zone Floor Area {person/m2}",
        "    ,                        !- Zone Floor Area per Person {m2/person}",
        "    0.3000000,               !- Fraction Radiant",
        "    ,                        !- Sensible Heat Fraction",
        "    Activity Sch,            !- Activity Level Schedule Name",
        "    3.82E-8,                 !- Carbon Dioxide Generation Rate {m3/s-W}",
        "    ,                        !- Enable ASHRAE 55 Comfort Warnings",
        "    zoneaveraged,            !- Mean Radiant Temperature Calculation Type",
        "    ,                        !- Surface Name/Angle Factor List Name",
        "    Work Eff Sch,            !- Work Efficiency Schedule Name",
        "    ClothingInsulationSchedule,  !- Clothing Insulation Calculation Method",
        "    ,                        !- Clothing Insulation Calculation Method Schedule Name",
        "    Clothing Sch,            !- Clothing Insulation Schedule Name",
        "    Air Velo Sch,            !- Air Velocity Schedule Name",
        "    FANGER;                  !- Thermal Comfort Model 1 Type",

        "  People,",
        "    NORTH ZONE People,       !- Name",
        "    NORTH ZONE,              !- Zone or ZoneList Name",
        "    OFFICE OCCUPANCY,        !- Number of People Schedule Name",
        "    people,                  !- Number of People Calculation Method",
        "    4.000000,                !- Number of People",
        "    ,                        !- People per Zone Floor Area {person/m2}",
        "    ,                        !- Zone Floor Area per Person {m2/person}",
        "    0.3000000,               !- Fraction Radiant",
        "    ,                        !- Sensible Heat Fraction",
        "    Activity Sch,            !- Activity Level Schedule Name",
        "    3.82E-8,                 !- Carbon Dioxide Generation Rate {m3/s-W}",
        "    ,                        !- Enable ASHRAE 55 Comfort Warnings",
        "    zoneaveraged,            !- Mean Radiant Temperature Calculation Type",
        "    ,                        !- Surface Name/Angle Factor List Name",
        "    Work Eff Sch,            !- Work Efficiency Schedule Name",
        "    ClothingInsulationSchedule,  !- Clothing Insulation Calculation Method",
        "    ,                        !- Clothing Insulation Calculation Method Schedule Name",
        "    Clothing Sch,            !- Clothing Insulation Schedule Name",
        "    Air Velo Sch,            !- Air Velocity Schedule Name",
        "    FANGER;                  !- Thermal Comfort Model 1 Type",

        "  Lights,",
        "    West Zone Lights 1,      !- Name",
        "    West Zone,               !- Zone or ZoneList Name",
        "    OFFICE LIGHTING,         !- Schedule Name",
        "    LightingLevel,           !- Design Level Calculation Method",
        "    1464.375,                !- Lighting Level {W}",
        "    ,                        !- Watts per Zone Floor Area {W/m2}",
        "    ,                        !- Watts per Person {W/person}",
        "    0,                       !- Return Air Fraction",
        "    0.2000000,               !- Fraction Radiant",
        "    0.2000000,               !- Fraction Visible",
        "    1.0,                     !- Fraction Replaceable",
        "    GeneralLights;           !- End-Use Subcategory",

        "  Lights,",
        "    EAST ZONE Lights 1,      !- Name",
        "    EAST ZONE,               !- Zone or ZoneList Name",
        "    OFFICE LIGHTING,         !- Schedule Name",
        "    LightingLevel,           !- Design Level Calculation Method",
        "    1464.375,                !- Lighting Level {W}",
        "    ,                        !- Watts per Zone Floor Area {W/m2}",
        "    ,                        !- Watts per Person {W/person}",
        "    0,                       !- Return Air Fraction",
        "    0.2000000,               !- Fraction Radiant",
        "    0.2000000,               !- Fraction Visible",
        "    1.0,                     !- Fraction Replaceable",
        "    GeneralLights;           !- End-Use Subcategory",

        "  Lights,",
        "    NORTH ZONE Lights 1,     !- Name",
        "    NORTH ZONE,              !- Zone or ZoneList Name",
        "    OFFICE LIGHTING,         !- Schedule Name",
        "    LightingLevel,           !- Design Level Calculation Method",
        "    1464.375,                !- Lighting Level {W}",
        "    ,                        !- Watts per Zone Floor Area {W/m2}",
        "    ,                        !- Watts per Person {W/person}",
        "    0,                       !- Return Air Fraction",
        "    0.2000000,               !- Fraction Radiant",
        "    0.2000000,               !- Fraction Visible",
        "    1.0,                     !- Fraction Replaceable",
        "    GeneralLights;           !- End-Use Subcategory",

        "  ElectricEquipment,",
        "    West Zone ElecEq 1,      !- Name",
        "    West Zone,               !- Zone or ZoneList Name",
        "    INTERMITTENT,            !- Schedule Name",
        "    EquipmentLevel,          !- Design Level Calculation Method",
        "    2928.751,                !- Design Level {W}",
        "    ,                        !- Watts per Zone Floor Area {W/m2}",
        "    ,                        !- Watts per Person {W/person}",
        "    0,                       !- Fraction Latent",
        "    0.3000000,               !- Fraction Radiant",
        "    0;                       !- Fraction Lost",

        "  ElectricEquipment,",
        "    EAST ZONE ElecEq 1,      !- Name",
        "    EAST ZONE,               !- Zone or ZoneList Name",
        "    INTERMITTENT,            !- Schedule Name",
        "    EquipmentLevel,          !- Design Level Calculation Method",
        "    1464.375,                !- Design Level {W}",
        "    ,                        !- Watts per Zone Floor Area {W/m2}",
        "    ,                        !- Watts per Person {W/person}",
        "    0,                       !- Fraction Latent",
        "    0.3000000,               !- Fraction Radiant",
        "    0;                       !- Fraction Lost",

        "  ElectricEquipment,",
        "    NORTH ZONE ElecEq 1,     !- Name",
        "    NORTH ZONE,              !- Zone or ZoneList Name",
        "    INTERMITTENT,            !- Schedule Name",
        "    EquipmentLevel,          !- Design Level Calculation Method",
        "    2928.751,                !- Design Level {W}",
        "    ,                        !- Watts per Zone Floor Area {W/m2}",
        "    ,                        !- Watts per Person {W/person}",
        "    0,                       !- Fraction Latent",
        "    0.3000000,               !- Fraction Radiant",
        "    0;                       !- Fraction Lost",

        "  Daylighting:Controls,",
        "    West Zone_DaylCtrl,      !- Name",
        "    West Zone,               !- Zone Name",
        "    SplitFlux,               !- Daylighting Method",
        "    ,                        !- Availability Schedule Name",
        "    Continuous,              !- Lighting Control Type",
        "    0.3,                     !- Minimum Input Power Fraction for Continuous or ContinuousOff Dimming Control",
        "    0.2,                     !- Minimum Light Output Fraction for Continuous or ContinuousOff Dimming Control",
        "    ,                        !- Number of Stepped Control Steps",
        "    1.0,                     !- Probability Lighting will be Reset When Needed in Manual Stepped Control",
        "    West Zone_DaylRefPt1,    !- Glare Calculation Daylighting Reference Point Name",
        "    180.0,                   !- Glare Calculation Azimuth Angle of View Direction Clockwise from Zone y-Axis {deg}",
        "    20.0,                    !- Maximum Allowable Discomfort Glare Index",
        "    ,                        !- DElight Gridding Resolution {m2}",
        "    West Zone_DaylRefPt1,    !- Daylighting Reference Point 1 Name",
        "    1.0,                     !- Fraction of Zone Controlled by Reference Point 1",
        "    500.;                    !- Illuminance Setpoint at Reference Point 1 {lux}",

        "  Daylighting:ReferencePoint,",
        "    West Zone_DaylRefPt1,    !- Name",
        "    West Zone,               !- Zone Name",
        "    3.048,                   !- X-Coordinate of Reference Point {m}",
        "    3.048,                   !- Y-Coordinate of Reference Point {m}",
        "    0.9;                     !- Z-Coordinate of Reference Point {m}",

        "  Curve:Biquadratic,",
        "    WindACCoolCapFT,         !- Name",
        "    0.942587793,             !- Coefficient1 Constant",
        "    0.009543347,             !- Coefficient2 x",
        "    0.000683770,             !- Coefficient3 x**2",
        "    -0.011042676,            !- Coefficient4 y",
        "    0.000005249,             !- Coefficient5 y**2",
        "    -0.000009720,            !- Coefficient6 x*y",
        "    12.77778,                !- Minimum Value of x",
        "    23.88889,                !- Maximum Value of x",
        "    18.0,                    !- Minimum Value of y",
        "    46.11111,                !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "  Curve:Biquadratic,",
        "    WindACEIRFT,             !- Name",
        "    0.342414409,             !- Coefficient1 Constant",
        "    0.034885008,             !- Coefficient2 x",
        "    -0.000623700,            !- Coefficient3 x**2",
        "    0.004977216,             !- Coefficient4 y",
        "    0.000437951,             !- Coefficient5 y**2",
        "    -0.000728028,            !- Coefficient6 x*y",
        "    12.77778,                !- Minimum Value of x",
        "    23.88889,                !- Maximum Value of x",
        "    18.0,                    !- Minimum Value of y",
        "    46.11111,                !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "  Curve:Quadratic,",
        "    WindACCoolCapFFF,        !- Name",
        "    0.8,                     !- Coefficient1 Constant",
        "    0.2,                     !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.5,                     !- Minimum Value of x",
        "    1.5;                     !- Maximum Value of x",

        "  Curve:Quadratic,",
        "    WindACEIRFFF,            !- Name",
        "    1.1552,                  !- Coefficient1 Constant",
        "    -0.1808,                 !- Coefficient2 x",
        "    0.0256,                  !- Coefficient3 x**2",
        "    0.5,                     !- Minimum Value of x",
        "    1.5;                     !- Maximum Value of x",

        "  Curve:Quadratic,",
        "    WindACPLFFPLR,           !- Name",
        "    0.85,                    !- Coefficient1 Constant",
        "    0.15,                    !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.0,                     !- Minimum Value of x",
        "    1.0;                     !- Maximum Value of x",

        "  NodeList,",
        "    OutsideAirInletNodes,    !- Name",
        "    Outside Air Inlet Node;  !- Node 1 Name",

        "  NodeList,",
        "    Zone1Inlets,             !- Name",
        "    Zone 1 Inlet Node;  !- Node 1 Name",

        "  NodeList,",
        "    Zone2Inlets,             !- Name",
        "    Zone 2 Reheat Air Outlet Node;  !- Node 1 Name",

        "  NodeList,",
        "    Supply Air Temp Nodes,   !- Name",
        "    Heating Coil Air Inlet Node,  !- Node 1 Name",
        "    Air Loop Outlet Node;    !- Node 2 Name",

        "  BranchList,",
        "    Air Loop Branches,       !- Name",
        "    Air Loop Main Branch;    !- Branch 1 Name",

        "  Branch,",
        "    Air Loop Main Branch,    !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    AirLoopHVAC:OutdoorAirSystem,  !- Component 1 Object Type",
        "    OA Sys 1,                !- Component 1 Name",
        "    Air Loop Inlet Node,     !- Component 1 Inlet Node Name",
        "    Mixed Air Node,          !- Component 1 Outlet Node Name",
        "    Fan:ConstantVolume,      !- Component 2 Object Type",
        "    Supply Fan 1,            !- Component 2 Name",
        "    Mixed Air Node,          !- Component 2 Inlet Node Name",
        "    Cooling Coil Air Inlet Node,  !- Component 2 Outlet Node Name",
        "    CoilSystem:Cooling:DX,   !- Component 3 Object Type",
        "    DX Cooling Coil System 1,!- Component 3 Name",
        "    Cooling Coil Air Inlet Node,  !- Component 3 Inlet Node Name",
        "    Heating Coil Air Inlet Node,  !- Component 3 Outlet Node Name",
        "    Coil:Heating:Fuel,       !- Component 4 Object Type",
        "    Main Heating Coil 1,     !- Component 4 Name",
        "    Heating Coil Air Inlet Node,  !- Component 4 Inlet Node Name",
        "    Air Loop Outlet Node;    !- Component 4 Outlet Node Name",

        "  AirLoopHVAC,",
        "    Typical Terminal Reheat 1,  !- Name",
        "    ,                        !- Controller List Name",
        "    Reheat System 1 Avail List,  !- Availability Manager List Name",
        "    1.16,                     !- Design Supply Air Flow Rate {m3/s}",
        "    Air Loop Branches,       !- Branch List Name",
        "    ,                        !- Connector List Name",
        "    Air Loop Inlet Node,     !- Supply Side Inlet Node Name",
        "    Return Air Mixer Outlet, !- Demand Side Outlet Node Name",
        "    Zone Equipment Inlet Node,  !- Demand Side Inlet Node Names",
        "    Air Loop Outlet Node;    !- Supply Side Outlet Node Names",

        "  AirLoopHVAC:ControllerList,",
        "    OA Sys 1 Controllers,    !- Name",
        "    Controller:OutdoorAir,   !- Controller 1 Object Type",
        "    OA Controller 1;         !- Controller 1 Name",

        "  AirLoopHVAC:OutdoorAirSystem:EquipmentList,",
        "    OA Sys 1 Equipment,      !- Name",
        "    OutdoorAir:Mixer,        !- Component 1 Object Type",
        "    OA Mixing Box 1;         !- Component 1 Name",

        "  AirLoopHVAC:OutdoorAirSystem,",
        "    OA Sys 1,                !- Name",
        "    OA Sys 1 Controllers,    !- Controller List Name",
        "    OA Sys 1 Equipment,      !- Outdoor Air Equipment List Name",
        "    Reheat System 1 Avail List;  !- Availability Manager List Name",

        "  OutdoorAir:NodeList,",
        "    OutsideAirInletNodes;    !- Node or NodeList Name 1",

        "  OutdoorAir:Mixer,",
        "    OA Mixing Box 1,         !- Name",
        "    Mixed Air Node,          !- Mixed Air Node Name",
        "    Outside Air Inlet Node,  !- Outdoor Air Stream Node Name",
        "    Relief Air Outlet Node,  !- Relief Air Stream Node Name",
        "    Air Loop Inlet Node;     !- Return Air Stream Node Name",

        "  AirflowNetwork:SimulationControl,",
        "    AirflowNetwork_All,      !- Name",
        "    MultizoneWithDistribution,  !- AirflowNetwork Control",
        "    INPUT,                   !- Wind Pressure Coefficient Type",
        "    ExternalNode,            !- Height Selection for Local Wind Pressure Calculation",
        "    LOWRISE,                 !- Building Type",
        "    500,                     !- Maximum Number of Iterations {dimensionless}",
        "    ZeroNodePressures,       !- Initialization Type",
        "    1.0E-04,                 !- Relative Airflow Convergence Tolerance {dimensionless}",
        "    1.0E-04,                 !- Absolute Airflow Convergence Tolerance {kg/s}",
        "    -0.5,                    !- Convergence Acceleration Limit {dimensionless}",
        "    0.0,                     !- Azimuth Angle of Long Axis of Building {deg}",
        "    1.0;                     !- Ratio of Building Width Along Short Axis to Width Along Long Axis",

        "  AirflowNetwork:MultiZone:Zone,",
        "    West Zone,               !- Zone Name",
        "    Temperature,             !- Ventilation Control Mode",
        "    WindowVentSched,         !- Ventilation Control Zone Temperature Setpoint Schedule Name",
        "    0.3,                     !- Minimum Venting Open Factor {dimensionless}",
        "    5.0,                     !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor {deltaC}",
        "    10.0,                    !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor {deltaC}",
        "    0.0,                     !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor {deltaJ/kg}",
        "    300000.0,                !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor {deltaJ/kg}",
        "    VentingSched;            !- Venting Availability Schedule Name",

        "  AirflowNetwork:MultiZone:Zone,",
        "    EAST ZONE,               !- Zone Name",
        "    NoVent,                  !- Ventilation Control Mode",
        "    ,                        !- Ventilation Control Zone Temperature Setpoint Schedule Name",
        "    1.0,                     !- Minimum Venting Open Factor {dimensionless}",
        "    0.0,                     !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor {deltaC}",
        "    100.0,                   !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor {deltaC}",
        "    0.0,                     !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor {deltaJ/kg}",
        "    300000.0;                !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor {deltaJ/kg}",

        "  AirflowNetwork:MultiZone:Zone,",
        "    NORTH ZONE,              !- Zone Name",
        "    Temperature,             !- Ventilation Control Mode",
        "    WindowVentSched,         !- Ventilation Control Zone Temperature Setpoint Schedule Name",
        "    1.0,                     !- Minimum Venting Open Factor {dimensionless}",
        "    0.0,                     !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor {deltaC}",
        "    100.0,                   !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor {deltaC}",
        "    0.0,                     !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor {deltaJ/kg}",
        "    300000.0,                !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor {deltaJ/kg}",
        "    VentingSched;            !- Venting Availability Schedule Name",

        "  AirflowNetwork:MultiZone:Zone,",
        "    ATTIC ZONE,              !- Zone Name",
        "    NoVent,                  !- Ventilation Control Mode",
        "    ,                        !- Ventilation Control Zone Temperature Setpoint Schedule Name",
        "    1.0,                     !- Minimum Venting Open Factor {dimensionless}",
        "    0.0,                     !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor {deltaC}",
        "    100.0,                   !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor {deltaC}",
        "    0.0,                     !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor {deltaJ/kg}",
        "    300000.0;                !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor {deltaJ/kg}",

        "  AirflowNetwork:MultiZone:Zone,",
        "    ATTIC NORTH ZONE,              !- Zone Name",
        "    NoVent,                  !- Ventilation Control Mode",
        "    ,                        !- Ventilation Control Zone Temperature Setpoint Schedule Name",
        "    1.0,                     !- Minimum Venting Open Factor {dimensionless}",
        "    0.0,                     !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor {deltaC}",
        "    100.0,                   !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor {deltaC}",
        "    0.0,                     !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor {deltaJ/kg}",
        "    300000.0;                !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor {deltaJ/kg}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn001:Wall001,           !- Surface Name",
        "    ELA-1,                   !- Leakage Component Name",
        "    SFacade,                 !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn001:Wall001:Win001,    !- Surface Name",
        "    CR-1,                    !- Leakage Component Name",
        "    SFacade,                 !- External Node Name",
        "    0.5;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn001:Wall002,           !- Surface Name",
        "    CR-1,                    !- Leakage Component Name",
        "    WFacade,                 !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn001:Wall003,           !- Surface Name",
        "    CRcri,                   !- Leakage Component Name",
        "    ,                        !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn001:Wall003:Door001,   !- Surface Name",
        "    CRcri,                   !- Leakage Component Name",
        "    ,                        !- External Node Name",
        "    0.5;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn001:Wall004,           !- Surface Name",
        "    CRcri,                   !- Leakage Component Name",
        "    ,                        !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn001:Ceil001,           !- Surface Name",
        "    CRcri,                   !- Leakage Component Name",
        "    ,                        !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn002:Wall002,           !- Surface Name",
        "    CR-1,                    !- Leakage Component Name",
        "    SFacade,                 !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn002:Wall003,           !- Surface Name",
        "    CR-1,                    !- Leakage Component Name",
        "    EFacade,                 !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn002:Wall005,           !- Surface Name",
        "    CRcri,                   !- Leakage Component Name",
        "    ,                        !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn002:Ceil001,           !- Surface Name",
        "    CRcri,                   !- Leakage Component Name",
        "    ,                        !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn003:Wall001,           !- Surface Name",
        "    CR-1,                    !- Leakage Component Name",
        "    WFacade,                 !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn003:Wall002,           !- Surface Name",
        "    CR-1,                    !- Leakage Component Name",
        "    NFacade,                 !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn003:Wall002:Win001,    !- Surface Name",
        "    CR-1,                    !- Leakage Component Name",
        "    NFacade,                 !- External Node Name",
        "    0.5;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn003:Wall003,           !- Surface Name",
        "    NORTH ZONE Exhaust Fan,       !- Leakage Component Name",
        "    EFacade,                 !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn003:Ceil001,           !- Surface Name",
        "    CRcri,                   !- Leakage Component Name",
        "    ,                        !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn004:Roof001,           !- Surface Name",
        "    CR-1,                    !- Leakage Component Name",
        "    Horizontal,              !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn004:Roof002,           !- Surface Name",
        "    CR-1,                    !- Leakage Component Name",
        "    Horizontal,              !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn005:Roof001,           !- Surface Name",
        "    CR-1,                    !- Leakage Component Name",
        "    Horizontal,              !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn005:Wall004,           !- Surface Name",
        "    CR-1,                    !- Leakage Component Name",
        "    ,              !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface,",
        "    Zn005:Wall005,           !- Surface Name",
        "    CR-1,                    !- Leakage Component Name",
        "    ,              !- External Node Name",
        "    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

        "  AirflowNetwork:MultiZone:Component:DetailedOpening,",
        "    WiOpen1,                 !- Name",
        "    0.0001,                  !- Air Mass Flow Coefficient When Opening is Closed {kg/s-m}",
        "    0.667,                   !- Air Mass Flow Exponent When Opening is Closed {dimensionless}",
        "    NonPivoted,              !- Type of Rectangular Large Vertical Opening (LVO)",
        "    0.0,                     !- Extra Crack Length or Height of Pivoting Axis {m}",
        "    2,                       !- Number of Sets of Opening Factor Data",
        "    0.0,                     !- Opening Factor 1 {dimensionless}",
        "    0.5,                     !- Discharge Coefficient for Opening Factor 1 {dimensionless}",
        "    0.0,                     !- Width Factor for Opening Factor 1 {dimensionless}",
        "    1.0,                     !- Height Factor for Opening Factor 1 {dimensionless}",
        "    0.0,                     !- Start Height Factor for Opening Factor 1 {dimensionless}",
        "    1.0,                     !- Opening Factor 2 {dimensionless}",
        "    0.6,                     !- Discharge Coefficient for Opening Factor 2 {dimensionless}",
        "    1.0,                     !- Width Factor for Opening Factor 2 {dimensionless}",
        "    1.0,                     !- Height Factor for Opening Factor 2 {dimensionless}",
        "    0.0,                     !- Start Height Factor for Opening Factor 2 {dimensionless}",
        "    0,                       !- Opening Factor 3 {dimensionless}",
        "    0,                       !- Discharge Coefficient for Opening Factor 3 {dimensionless}",
        "    0,                       !- Width Factor for Opening Factor 3 {dimensionless}",
        "    0,                       !- Height Factor for Opening Factor 3 {dimensionless}",
        "    0,                       !- Start Height Factor for Opening Factor 3 {dimensionless}",
        "    0,                       !- Opening Factor 4 {dimensionless}",
        "    0,                       !- Discharge Coefficient for Opening Factor 4 {dimensionless}",
        "    0,                       !- Width Factor for Opening Factor 4 {dimensionless}",
        "    0,                       !- Height Factor for Opening Factor 4 {dimensionless}",
        "    0;                       !- Start Height Factor for Opening Factor 4 {dimensionless}",

        "  AirflowNetwork:MultiZone:Component:SimpleOpening,",
        "    DrOpen,                  !- Name",
        "    0.0001,                  !- Air Mass Flow Coefficient When Opening is Closed {kg/s-m}",
        "    0.667,                   !- Air Mass Flow Exponent When Opening is Closed {dimensionless}",
        "    0.0001,                  !- Minimum Density Difference for Two-Way Flow {kg/m3}",
        "    0.55;                    !- Discharge Coefficient {dimensionless}",

        "  AirflowNetwork:MultiZone:ReferenceCrackConditions,",
        "    ReferenceCrackConditions,!- Name",
        "    20.0,                    !- Reference Temperature {C}",
        "    101325,                  !- Reference Barometric Pressure {Pa}",
        "    0.0;                     !- Reference Humidity Ratio {kgWater/kgDryAir}",

        "  AirflowNetwork:MultiZone:Surface:Crack,",
        "    CR-1,                    !- Name",
        "    0.001,                   !- Air Mass Flow Coefficient at Reference Conditions {kg/s}",
        "    0.667;                   !- Air Mass Flow Exponent {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface:Crack,",
        "    CRcri,                   !- Name",
        "    0.05,                    !- Air Mass Flow Coefficient at Reference Conditions {kg/s}",
        "    0.667;                   !- Air Mass Flow Exponent {dimensionless}",

        "  AirflowNetwork:MultiZone:Component:ZoneExhaustFan,",
        "    NORTH ZONE Exhaust Fan,       !- Name",
        "    0.01,                    !- Air Mass Flow Coefficient When the Zone Exhaust Fan is Off at Reference Conditions {kg/s}",
        "    0.667;                   !- Air Mass Flow Exponent When the Zone Exhaust Fan is Off {dimensionless}",

        "  AirflowNetwork:MultiZone:Surface:EffectiveLeakageArea,",
        "    ELA-1,                   !- Name",
        "    0.007,                   !- Effective Leakage Area {m2}",
        "    1.0,                     !- Discharge Coefficient {dimensionless}",
        "    4.0,                     !- Reference Pressure Difference {Pa}",
        "    0.667;                   !- Air Mass Flow Exponent {dimensionless}",

        "  AirflowNetwork:MultiZone:ExternalNode,",
        "    NFacade,                 !- Name",
        "    1.524,                   !- External Node Height {m}",
        "    NFacade_WPCValue;        !- Wind Pressure Coefficient Curve Name",

        "  AirflowNetwork:MultiZone:ExternalNode,",
        "    EFacade,                 !- Name",
        "    1.524,                   !- External Node Height {m}",
        "    EFacade_WPCValue;        !- Wind Pressure Coefficient Curve Name",

        "  AirflowNetwork:MultiZone:ExternalNode,",
        "    SFacade,                 !- Name",
        "    1.524,                   !- External Node Height {m}",
        "    SFacade_WPCValue;        !- Wind Pressure Coefficient Curve Name",

        "  AirflowNetwork:MultiZone:ExternalNode,",
        "    WFacade,                 !- Name",
        "    1.524,                   !- External Node Height {m}",
        "    WFacade_WPCValue;        !- Wind Pressure Coefficient Curve Name",

        "  AirflowNetwork:MultiZone:ExternalNode,",
        "    Horizontal,              !- Name",
        "    3.028,                   !- External Node Height {m}",
        "    Horizontal_WPCValue;     !- Wind Pressure Coefficient Curve Name",

        "  AirflowNetwork:MultiZone:WindPressureCoefficientArray,",
        "    Every 30 Degrees,        !- Name",
        "    0,                       !- Wind Direction 1 {deg}",
        "    30,                      !- Wind Direction 2 {deg}",
        "    60,                      !- Wind Direction 3 {deg}",
        "    90,                      !- Wind Direction 4 {deg}",
        "    120,                     !- Wind Direction 5 {deg}",
        "    150,                     !- Wind Direction 6 {deg}",
        "    180,                     !- Wind Direction 7 {deg}",
        "    210,                     !- Wind Direction 8 {deg}",
        "    240,                     !- Wind Direction 9 {deg}",
        "    270,                     !- Wind Direction 10 {deg}",
        "    300,                     !- Wind Direction 11 {deg}",
        "    330;                     !- Wind Direction 12 {deg}",

        "  AirflowNetwork:MultiZone:WindPressureCoefficientValues,",
        "    NFacade_WPCValue,        !- Name",
        "    Every 30 Degrees,        !- AirflowNetwork:MultiZone:WindPressureCoefficientArray Name",
        "    0.60,                    !- Wind Pressure Coefficient Value 1 {dimensionless}",
        "    0.48,                    !- Wind Pressure Coefficient Value 2 {dimensionless}",
        "    0.04,                    !- Wind Pressure Coefficient Value 3 {dimensionless}",
        "    -0.56,                   !- Wind Pressure Coefficient Value 4 {dimensionless}",
        "    -0.56,                   !- Wind Pressure Coefficient Value 5 {dimensionless}",
        "    -0.42,                   !- Wind Pressure Coefficient Value 6 {dimensionless}",
        "    -0.37,                   !- Wind Pressure Coefficient Value 7 {dimensionless}",
        "    -0.42,                   !- Wind Pressure Coefficient Value 8 {dimensionless}",
        "    -0.56,                   !- Wind Pressure Coefficient Value 9 {dimensionless}",
        "    -0.56,                   !- Wind Pressure Coefficient Value 10 {dimensionless}",
        "    0.04,                    !- Wind Pressure Coefficient Value 11 {dimensionless}",
        "    0.48;                    !- Wind Pressure Coefficient Value 12 {dimensionless}",

        "  AirflowNetwork:MultiZone:WindPressureCoefficientValues,",
        "    EFacade_WPCValue,        !- Name",
        "    Every 30 Degrees,        !- AirflowNetwork:MultiZone:WindPressureCoefficientArray Name",
        "    -0.56,                   !- Wind Pressure Coefficient Value 1 {dimensionless}",
        "    0.04,                    !- Wind Pressure Coefficient Value 2 {dimensionless}",
        "    0.48,                    !- Wind Pressure Coefficient Value 3 {dimensionless}",
        "    0.60,                    !- Wind Pressure Coefficient Value 4 {dimensionless}",
        "    0.48,                    !- Wind Pressure Coefficient Value 5 {dimensionless}",
        "    0.04,                    !- Wind Pressure Coefficient Value 6 {dimensionless}",
        "    -0.56,                   !- Wind Pressure Coefficient Value 7 {dimensionless}",
        "    -0.56,                   !- Wind Pressure Coefficient Value 8 {dimensionless}",
        "    -0.42,                   !- Wind Pressure Coefficient Value 9 {dimensionless}",
        "    -0.37,                   !- Wind Pressure Coefficient Value 10 {dimensionless}",
        "    -0.42,                   !- Wind Pressure Coefficient Value 11 {dimensionless}",
        "    -0.56;                   !- Wind Pressure Coefficient Value 12 {dimensionless}",

        "  AirflowNetwork:MultiZone:WindPressureCoefficientValues,",
        "    SFacade_WPCValue,        !- Name",
        "    Every 30 Degrees,        !- AirflowNetwork:MultiZone:WindPressureCoefficientArray Name",
        "    -0.37,                   !- Wind Pressure Coefficient Value 1 {dimensionless}",
        "    -0.42,                   !- Wind Pressure Coefficient Value 2 {dimensionless}",
        "    -0.56,                   !- Wind Pressure Coefficient Value 3 {dimensionless}",
        "    -0.56,                   !- Wind Pressure Coefficient Value 4 {dimensionless}",
        "    0.04,                    !- Wind Pressure Coefficient Value 5 {dimensionless}",
        "    0.48,                    !- Wind Pressure Coefficient Value 6 {dimensionless}",
        "    0.60,                    !- Wind Pressure Coefficient Value 7 {dimensionless}",
        "    0.48,                    !- Wind Pressure Coefficient Value 8 {dimensionless}",
        "    0.04,                    !- Wind Pressure Coefficient Value 9 {dimensionless}",
        "    -0.56,                   !- Wind Pressure Coefficient Value 10 {dimensionless}",
        "    -0.56,                   !- Wind Pressure Coefficient Value 11 {dimensionless}",
        "    -0.42;                   !- Wind Pressure Coefficient Value 12 {dimensionless}",

        "  AirflowNetwork:MultiZone:WindPressureCoefficientValues,",
        "    WFacade_WPCValue,        !- Name",
        "    Every 30 Degrees,        !- AirflowNetwork:MultiZone:WindPressureCoefficientArray Name",
        "    -0.56,                   !- Wind Pressure Coefficient Value 1 {dimensionless}",
        "    -0.56,                   !- Wind Pressure Coefficient Value 2 {dimensionless}",
        "    -0.42,                   !- Wind Pressure Coefficient Value 3 {dimensionless}",
        "    -0.37,                   !- Wind Pressure Coefficient Value 4 {dimensionless}",
        "    -0.42,                   !- Wind Pressure Coefficient Value 5 {dimensionless}",
        "    -0.56,                   !- Wind Pressure Coefficient Value 6 {dimensionless}",
        "    -0.56,                   !- Wind Pressure Coefficient Value 7 {dimensionless}",
        "    0.04,                    !- Wind Pressure Coefficient Value 8 {dimensionless}",
        "    0.48,                    !- Wind Pressure Coefficient Value 9 {dimensionless}",
        "    0.60,                    !- Wind Pressure Coefficient Value 10 {dimensionless}",
        "    0.48,                    !- Wind Pressure Coefficient Value 11 {dimensionless}",
        "    0.04;                    !- Wind Pressure Coefficient Value 12 {dimensionless}",

        "  AirflowNetwork:MultiZone:WindPressureCoefficientValues,",
        "    Horizontal_WPCValue,     !- Name",
        "    Every 30 Degrees,        !- AirflowNetwork:MultiZone:WindPressureCoefficientArray Name",
        "    0.00,                    !- Wind Pressure Coefficient Value 1 {dimensionless}",
        "    0.00,                    !- Wind Pressure Coefficient Value 2 {dimensionless}",
        "    0.00,                    !- Wind Pressure Coefficient Value 3 {dimensionless}",
        "    0.00,                    !- Wind Pressure Coefficient Value 4 {dimensionless}",
        "    0.00,                    !- Wind Pressure Coefficient Value 5 {dimensionless}",
        "    0.00,                    !- Wind Pressure Coefficient Value 6 {dimensionless}",
        "    0.00,                    !- Wind Pressure Coefficient Value 7 {dimensionless}",
        "    0.00,                    !- Wind Pressure Coefficient Value 8 {dimensionless}",
        "    0.00,                    !- Wind Pressure Coefficient Value 9 {dimensionless}",
        "    0.00,                    !- Wind Pressure Coefficient Value 10 {dimensionless}",
        "    0.00,                    !- Wind Pressure Coefficient Value 11 {dimensionless}",
        "    0.00;                    !- Wind Pressure Coefficient Value 12 {dimensionless}",

        "  AirflowNetwork:Distribution:Node,",
        "    EquipmentInletNode,      !- Name",
        "    Zone Equipment Inlet Node,  !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    EquipmentOutletNode,     !- Name",
        "    ,                        !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    SupplyMainNode,          !- Name",
        "    ,                        !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    MainSplitterNode,        !- Name",
        "    ,                        !- Component Name or Node Name",
        "    AirLoopHVAC:ZoneSplitter,!- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Zone1SupplyNode,         !- Name",
        "    ,                        !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Zone1SupplyRegisterNode, !- Name",
        "    Zone 1 Inlet Node,       !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Zone1OutletNode,         !- Name",
        "    Zone 1 Outlet Node,      !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Zone2SupplyNode,         !- Name",
        "    ,                        !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    ReheatInlet2Node,        !- Name",
        "    Zone 2 Reheat Air Inlet Node,  !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Zone2SupplyRegisterNode, !- Name",
        "    Zone 2 Reheat Air Outlet Node,  !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Zone2OutletNode,         !- Name",
        "    Zone 2 Outlet Node,      !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Zone1ReturnNode,         !- Name",
        "    ,                        !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Zone2ReturnNode,         !- Name",
        "    ,                        !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    MainMixerNode,           !- Name",
        "    ,                        !- Component Name or Node Name",
        "    AirLoopHVAC:ZoneMixer,   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    MainReturnNode,          !- Name",
        "    Return Air Mixer Outlet, !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    MainInletNode,           !- Name",
        "    Air Loop Inlet Node,     !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    OA System Node,          !- Name",
        "    ,                        !- Component Name or Node Name",
        "    AirLoopHVAC:OutdoorAirSystem,  !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    OA Inlet Node,           !- Name",
        "    Outside Air Inlet Node,  !- Component Name or Node Name",
        "    OAMixerOutdoorAirStreamNode,  !- Component Object Type or Node Type",
        "    1.5;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    FanInletNode,            !- Name",
        "    Mixed Air Node,          !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    FanOutletNode,           !- Name",
        "    Cooling Coil Air Inlet Node,  !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    HeatingInletNode,        !- Name",
        "    Heating Coil Air Inlet Node,  !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    HeatingOutletNode,       !- Name",
        "    Air Loop Outlet Node,    !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Component:Leak,",
        "    MainSupplyLeak,          !- Name",
        "    0.0025,                  !- Air Mass Flow Coefficient {kg/s}",
        "    0.65;                    !- Air Mass Flow Exponent {dimensionless}",

        "  AirflowNetwork:Distribution:Component:ConstantPressureDrop,",
        "    SupplyCPDComp,           !- Name",
        "    1.0;                     !- Pressure Difference Across the Component {Pa}",

        "  AirflowNetwork:Distribution:Component:LeakageRatio,",
        "    ZoneSupplyELR1,          !- Name",
        "    0.01,                    !- Effective Leakage Ratio {dimensionless}",
        "    1.9,                     !- Maximum Flow Rate {m3/s}",
        "    59.0,                    !- Reference Pressure Difference {Pa}",
        "    0.65;                    !- Air Mass Flow Exponent {dimensionless}",

        "  AirflowNetwork:Distribution:Component:LeakageRatio,",
        "    ZoneSupplyELR2,          !- Name",
        "    0.01,                    !- Effective Leakage Ratio {dimensionless}",
        "    1.9,                     !- Maximum Flow Rate {m3/s}",
        "    59.0,                    !- Reference Pressure Difference {Pa}",
        "    0.65;                    !- Air Mass Flow Exponent {dimensionless}",

        "  AirflowNetwork:Distribution:Component:LeakageRatio,",
        "    ReturnLeakELR1,          !- Name",
        "    0.03,                    !- Effective Leakage Ratio {dimensionless}",
        "    1.9,                     !- Maximum Flow Rate {m3/s}",
        "    41.0,                    !- Reference Pressure Difference {Pa}",
        "    0.65;                    !- Air Mass Flow Exponent {dimensionless}",

        "  AirflowNetwork:Distribution:Component:LeakageRatio,",
        "    ReturnLeakELR2,          !- Name",
        "    0.03,                    !- Effective Leakage Ratio {dimensionless}",
        "    1.9,                     !- Maximum Flow Rate {m3/s}",
        "    40.0,                    !- Reference Pressure Difference {Pa}",
        "    0.65;                    !- Air Mass Flow Exponent {dimensionless}",

        "  AirflowNetwork:Distribution:Component:Duct,",
        "    MainTruck1,              !- Name",
        "    3.0,                     !- Duct Length {m}",
        "    0.6,                     !- Hydraulic Diameter {m}",
        "    0.2827,                  !- Cross Section Area {m2}",
        "    0.0009,                  !- Surface Roughness {m}",
        "    0.01,                    !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
        "    0.946792,                !- Heat Transmittance Coefficient (U-Factor) for Duct Wall Construction {W/m2-K}",
        "    0.0001,                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",
        "    5.018000,                !- Outside Convection Coefficient {W/m2-K}",
        "    25.090000;               !- Inside Convection Coefficient {W/m2-K}",

        "  AirflowNetwork:Distribution:Component:Duct,",
        "    MainTruck2,              !- Name",
        "    4.0,                     !- Duct Length {m}",
        "    0.6,                     !- Hydraulic Diameter {m}",
        "    0.2827,                  !- Cross Section Area {m2}",
        "    0.0009,                  !- Surface Roughness {m}",
        "    0.01,                    !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
        "    0.946792,                !- Heat Transmittance Coefficient (U-Factor) for Duct Wall Construction {W/m2-K}",
        "    0.0001,                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",
        "    5.018000,                !- Outside Convection Coefficient {W/m2-K}",
        "    25.090000;               !- Inside Convection Coefficient {W/m2-K}",

        "  AirflowNetwork:Distribution:Component:Duct,",
        "    Zone1Supply,             !- Name",
        "    5.0,                     !- Duct Length {m}",
        "    0.4,                     !- Hydraulic Diameter {m}",
        "    0.1256,                  !- Cross Section Area {m2}",
        "    0.0009,                  !- Surface Roughness {m}",
        "    1.00,                    !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
        "    0.946792,                !- Heat Transmittance Coefficient (U-Factor) for Duct Wall Construction {W/m2-K}",
        "    0.0001,                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",
        "    5.018000,                !- Outside Convection Coefficient {W/m2-K}",
        "    25.090000;               !- Inside Convection Coefficient {W/m2-K}",

        "  AirflowNetwork:Distribution:Component:Duct,",
        "    Zone2Supply,             !- Name",
        "    4.0,                     !- Duct Length {m}",
        "    0.39,                    !- Hydraulic Diameter {m}",
        "    0.1195,                  !- Cross Section Area {m2}",
        "    0.0009,                  !- Surface Roughness {m}",
        "    2.5,                     !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
        "    0.946792,                !- Heat Transmittance Coefficient (U-Factor) for Duct Wall Construction {W/m2-K}",
        "    0.0001,                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",
        "    5.018000,                !- Outside Convection Coefficient {W/m2-K}",
        "    25.090000;               !- Inside Convection Coefficient {W/m2-K}",

        "  AirflowNetwork:Distribution:Component:Duct,",
        "    Zone1Return,             !- Name",
        "    4.0,                     !- Duct Length {m}",
        "    0.50,                    !- Hydraulic Diameter {m}",
        "    0.1963,                  !- Cross Section Area {m2}",
        "    0.0009,                  !- Surface Roughness {m}",
        "    1.0,                     !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
        "    0.946792,                !- Heat Transmittance Coefficient (U-Factor) for Duct Wall Construction {W/m2-K}",
        "    0.0001,                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",
        "    5.018000,                !- Outside Convection Coefficient {W/m2-K}",
        "    25.090000;               !- Inside Convection Coefficient {W/m2-K}",

        "  AirflowNetwork:Distribution:Component:Duct,",
        "    Zone2Return,             !- Name",
        "    4.0,                     !- Duct Length {m}",
        "    0.48,                    !- Hydraulic Diameter {m}",
        "    0.1809,                  !- Cross Section Area {m2}",
        "    0.0009,                  !- Surface Roughness {m}",
        "    1.0,                     !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
        "    0.946792,                !- Heat Transmittance Coefficient (U-Factor) for Duct Wall Construction {W/m2-K}",
        "    0.0001,                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",
        "    5.018000,                !- Outside Convection Coefficient {W/m2-K}",
        "    25.090000;               !- Inside Convection Coefficient {W/m2-K}",

        "  AirflowNetwork:Distribution:Component:Duct,",
        "    ZoneConnectionDuct,      !- Name",
        "    0.1,                     !- Duct Length {m}",
        "    1.0,                     !- Hydraulic Diameter {m}",
        "    0.7854,                  !- Cross Section Area {m2}",
        "    0.0001,                  !- Surface Roughness {m}",
        "    30.00,                   !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
        "    0.001226,                !- Heat Transmittance Coefficient (U-Factor) for Duct Wall Construction {W/m2-K}",
        "    0.0001,                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",
        "    0.006500,                !- Outside Convection Coefficient {W/m2-K}",
        "    0.032500;                !- Inside Convection Coefficient {W/m2-K}",

        "  AirflowNetwork:Distribution:Component:Duct,",
        "    MixerConnectionDuct,     !- Name",
        "    0.1,                     !- Duct Length {m}",
        "    1.0,                     !- Hydraulic Diameter {m}",
        "    0.7854,                  !- Cross Section Area {m2}",
        "    0.0001,                  !- Surface Roughness {m}",
        "    1.00,                    !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
        "    0.001226,                !- Heat Transmittance Coefficient (U-Factor) for Duct Wall Construction {W/m2-K}",
        "    0.0001,                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",
        "    0.006500,                !- Outside Convection Coefficient {W/m2-K}",
        "    0.032500;                !- Inside Convection Coefficient {W/m2-K}",

        "  AirflowNetwork:Distribution:Component:Duct,",
        "    AirLoopReturn,           !- Name",
        "    0.1,                     !- Duct Length {m}",
        "    1.0,                     !- Hydraulic Diameter {m}",
        "    0.7854,                  !- Cross Section Area {m2}",
        "    0.0001,                  !- Surface Roughness {m}",
        "    1.00,                    !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
        "    0.001226,                !- Heat Transmittance Coefficient (U-Factor) for Duct Wall Construction {W/m2-K}",
        "    0.0001,                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",
        "    0.006500,                !- Outside Convection Coefficient {W/m2-K}",
        "    0.032500;                !- Inside Convection Coefficient {W/m2-K}",

        "  AirflowNetwork:Distribution:Component:Duct,",
        "    AirLoopSupply,           !- Name",
        "    0.1,                     !- Duct Length {m}",
        "    1.0,                     !- Hydraulic Diameter {m}",
        "    0.7854,                  !- Cross Section Area {m2}",
        "    0.0001,                  !- Surface Roughness {m}",
        "    1.00,                    !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
        "    0.001226,                !- Heat Transmittance Coefficient (U-Factor) for Duct Wall Construction {W/m2-K}",
        "    0.0001,                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",
        "    0.006500,                !- Outside Convection Coefficient {W/m2-K}",
        "    0.032500;                !- Inside Convection Coefficient {W/m2-K}",

        "  AirflowNetwork:Distribution:Component:Fan,",
        "    Supply Fan 1,            !- Fan Name",
        "    Fan:ConstantVolume;      !- Supply Fan Object Type",

        "  AirflowNetwork:Distribution:Component:Coil,",
        "    ACDXCoil 1,              !- Coil Name",
        "    Coil:Cooling:DX:SingleSpeed,  !- Coil Object Type",
        "    0.1,                     !- Air Path Length {m}",
        "    1.00;                    !- Air Path Hydraulic Diameter {m}",

        "  AirflowNetwork:Distribution:Component:Coil,",
        "    Main Heating Coil 1,     !- Coil Name",
        "    Coil:Heating:Fuel,       !- Coil Object Type",
        "    0.1,                     !- Air Path Length {m}",
        "    1.00;                    !- Air Path Hydraulic Diameter {m}",

        "  AirflowNetwork:Distribution:Component:TerminalUnit,",
        "    Reheat Zone 2,           !- Terminal Unit Name",
        "    AirTerminal:SingleDuct:ConstantVolume:Reheat,  !- Terminal Unit Object Type",
        "    0.1,                     !- Air Path Length {m}",
        "    0.44;                    !- Air Path Hydraulic Diameter {m}",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Main Link 1,             !- Name",
        "    EquipmentInletNode,      !- Node 1 Name",
        "    EquipmentOutletNode,     !- Node 2 Name",
        "    MainTruck1,              !- Component Name",
        "    Attic Zone;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Main CDP Link,           !- Name",
        "    EquipmentOutletNode,     !- Node 1 Name",
        "    SupplyMainNode,          !- Node 2 Name",
        "    SupplyCPDComp;           !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Main Link 2,             !- Name",
        "    SupplyMainNode,          !- Node 1 Name",
        "    MainSplitterNode,        !- Node 2 Name",
        "    MainTruck2,              !- Component Name",
        "    Attic Zone;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone1Supply1Link,        !- Name",
        "    MainSplitterNode,        !- Node 1 Name",
        "    Zone1SupplyNode,         !- Node 2 Name",
        "    Zone1Supply,             !- Component Name",
        "    Attic Zone;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone1Supply2Link,        !- Name",
        "    Zone1SupplyNode,         !- Node 1 Name",
        "    Zone1SupplyRegisterNode,        !- Node 2 Name",
        "    Zone1Supply,             !- Component Name",
        "    Attic Zone;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone1SupplyConnectionLink,  !- Name",
        "    Zone1SupplyRegisterNode, !- Node 1 Name",
        "    West Zone,               !- Node 2 Name",
        "    ZoneConnectionDuct;      !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone1ReturnConnectionLink,  !- Name",
        "    West Zone,               !- Node 1 Name",
        "    Zone1OutletNode,         !- Node 2 Name",
        "    ZoneConnectionDuct;      !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone2Supply1Link,        !- Name",
        "    MainSplitterNode,        !- Node 1 Name",
        "    Zone2SupplyNode,         !- Node 2 Name",
        "    Zone2Supply,             !- Component Name",
        "    Attic Zone;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone2Supply2Link,        !- Name",
        "    Zone2SupplyNode,         !- Node 1 Name",
        "    ReheatInlet2Node,        !- Node 2 Name",
        "    Zone2Supply,             !- Component Name",
        "    Attic Zone;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone2ReheatCoilLink,     !- Name",
        "    ReheatInlet2Node,        !- Node 1 Name",
        "    Zone2SupplyRegisterNode, !- Node 2 Name",
        "    Reheat Zone 2;           !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone2SupplyConnectionLink,  !- Name",
        "    Zone2SupplyRegisterNode, !- Node 1 Name",
        "    EAST ZONE,               !- Node 2 Name",
        "    ZoneConnectionDuct;      !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone2returnConnectionLink,  !- Name",
        "    EAST ZONE,               !- Node 1 Name",
        "    Zone2OutletNode,         !- Node 2 Name",
        "    ZoneConnectionDuct;      !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone1Return1Link,        !- Name",
        "    Zone1OutletNode,         !- Node 1 Name",
        "    Zone1ReturnNode,         !- Node 2 Name",
        "    Zone1Return,             !- Component Name",
        "    Attic Zone;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone1Return2Link,        !- Name",
        "    Zone1ReturnNode,         !- Node 1 Name",
        "    MainMixerNode,           !- Node 2 Name",
        "    Zone1Return,             !- Component Name",
        "    Attic Zone;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone2Return1Link,        !- Name",
        "    Zone2OutletNode,         !- Node 1 Name",
        "    Zone2ReturnNode,         !- Node 2 Name",
        "    Zone2Return,             !- Component Name",
        "    Attic Zone;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone2Return2Link,        !- Name",
        "    Zone2ReturnNode,         !- Node 1 Name",
        "    MainMixerNode,           !- Node 2 Name",
        "    Zone2Return,             !- Component Name",
        "    Attic Zone;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    ReturnMixerLink,         !- Name",
        "    MainMixerNode,           !- Node 1 Name",
        "    MainReturnNode,          !- Node 2 Name",
        "    MixerConnectionDuct,     !- Component Name",
        "    Attic Zone;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    AirLoopReturnLink,       !- Name",
        "    MainReturnNode,          !- Node 1 Name",
        "    MainInletNode,           !- Node 2 Name",
        "    AirLoopReturn;           !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    OASystemInletLink,       !- Name",
        "    MainInletNode,           !- Node 1 Name",
        "    OA System Node,          !- Node 2 Name",
        "    ZoneConnectionDuct;      !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    OAMixerOutletLink,       !- Name",
        "    OA System Node,          !- Node 1 Name",
        "    FanInletNode,            !- Node 2 Name",
        "    ZoneConnectionDuct;      !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    SupplyFanLink,           !- Name",
        "    FanInletNode,            !- Node 1 Name",
        "    FanOutletNode,           !- Node 2 Name",
        "    Supply Fan 1;            !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    CoolingCoilLink,         !- Name",
        "    FanOutletNode,           !- Node 1 Name",
        "    HeatingInletNode,        !- Node 2 Name",
        "    ACDXCoil 1;              !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    HeatingCoilLink,         !- Name",
        "    HeatingInletNode,        !- Node 1 Name",
        "    HeatingOutletNode,       !- Node 2 Name",
        "    Main Heating Coil 1;     !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    EquipmentAirLoopLink,    !- Name",
        "    HeatingOutletNode,       !- Node 1 Name",
        "    EquipmentInletNode,      !- Node 2 Name",
        "    AirLoopSupply;           !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone1ReturnLeakLink,     !- Name",
        "    Zone1ReturnNode,         !- Node 1 Name",
        "    OA Inlet Node,           !- Node 2 Name",
        "    ReturnLeakELR1;          !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    MainSupplyLeakLink,      !- Name",
        "    SupplyMainNode,          !- Node 1 Name",
        "    ATTIC ZONE,              !- Node 2 Name",
        "    MainSupplyLeak;          !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone1SupplyLeakLink,     !- Name",
        "    Zone1SupplyNode,         !- Node 1 Name",
        "    ATTIC ZONE,              !- Node 2 Name",
        "    ZoneSupplyELR1;          !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone2ReturnLeakLink,     !- Name",
        "    Zone2ReturnNode,         !- Node 1 Name",
        "    OA Inlet Node,           !- Node 2 Name",
        "    ReturnLeakELR2;          !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Zone2SupplyLeakLink,     !- Name",
        "    Zone2SupplyNode,         !- Node 1 Name",
        "    ATTIC ZONE,              !- Node 2 Name",
        "    ZoneSupplyELR2;          !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    OASystemFanLink,       !- Name",
        "    OA Inlet Node,           !- Node 1 Name",
        "    OA System Node,          !- Node 2 Name",
        "    OA Fan;                  !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    OASystemReliefLink,      !- Name",
        "    OA System Node,          !- Node 1 Name",
        "    OA Inlet Node,           !- Node 2 Name",
        "    Relief Fan;              !- Component Name",

        "  AirflowNetwork:Distribution:Component:OutdoorAirFlow,",
        "    OA Fan,                  !- Name",
        "    OA Mixing Box 1,         !- Outdoor Air Mixer Name",
        "    0.001,                   !- Air Mass Flow Coefficient When No Outdoor Air Flow at Reference Conditions {kg/s}",
        "    0.667;                   !- Air Mass Flow Exponent When No Outdoor Air Flow {dimensionless}",

        "  AirflowNetwork:Distribution:Component:ReliefAirFlow,",
        "    Relief Fan,              !- Name",
        "    OA Mixing Box 1,         !- Outdoor Air Mixer Name",
        "    0.001,                   !- Air Mass Flow Coefficient When No Outdoor Air Flow at Reference Conditions {kg/s}",
        "    0.667;                   !- Air Mass Flow Exponent When No Outdoor Air Flow {dimensionless}",

        "  Schedule:Compact,",
        "    Pressure Setpoint Schedule,  !- Name",
        "    Any Number,              !- Schedule Type Limits Name",
        "    Through: 3/31,           !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,5.0,        !- Field 3",
        "    Through: 9/30,           !- Field 5",
        "    For: AllDays,            !- Field 6",
        "    Until: 24:00,9.5,        !- Field 7",
        "    Through: 12/31,          !- Field 9",
        "    For: AllDays,            !- Field 10",
        "    Until: 24:00,5.0;        !- Field 11",

        "  AirflowNetwork:Distribution:Node,",
        "    Heat Pump 1 EquipmentInletNode,      !- Name",
        "    Heat Pump 1 Supply Path Inlet,  !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Heat Pump 1 EquipmentOutletNode,     !- Name",
        "    ,                        !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Heat Pump 1 SupplyMainNode,          !- Name",
        "    ,                        !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Heat Pump 1 MainSplitterNode,        !- Name",
        "    ,                        !- Component Name or Node Name",
        "    AirLoopHVAC:ZoneSplitter,!- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Heat Pump 1 Zone1SupplyNode,         !- Name",
        "    ,                        !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Heat Pump 1 Zone1SupplyRegisterNode, !- Name",
        "    NORTH ZONE Zone Equip Inlet,  !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Heat Pump 1 Zone1OutletNode,         !- Name",
        "    NORTH ZONE Return Outlet,      !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Heat Pump 1 Zone1ReturnNode,         !- Name",
        "    ,                        !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Heat Pump 1 MainMixerNode,           !- Name",
        "    ,                        !- Component Name or Node Name",
        "    AirLoopHVAC:ZoneMixer,   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Heat Pump 1 MainReturnNode,          !- Name",
        "    Heat Pump 1 Return Air Outlet, !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Heat Pump 1 MainInletNode,           !- Name",
        "    Heat Pump 1 Air Loop Inlet,     !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Heat Pump 1 OA System Node,          !- Name",
        "    ,                        !- Component Name or Node Name",
        "    AirLoopHVAC:OutdoorAirSystem,  !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Heat Pump 1 OA Inlet Node,           !- Name",
        "    Heat Pump 1 Outside Air Inlet,  !- Component Name or Node Name",
        "    OAMixerOutdoorAirStreamNode,  !- Component Object Type or Node Type",
        "    1.5;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Heat Pump 1 FanInletNode,            !- Name",
        "    Heat Pump 1 Mixed Air Outlet,          !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Heat Pump 1 FanOutletNode,           !- Name",
        "    Heat Pump 1 Supply Fan Outlet,  !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Heat Pump 1 HeatingInletNode,        !- Name",
        "    Heat Pump 1 Cooling Coil Outlet,  !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Heat Pump 1 HeatingOutletNode,       !- Name",
        "    Heat Pump 1 Heating Coil Outlet,    !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",

        "  AirflowNetwork:Distribution:Node,",
        "    Heat Pump 1 SuppHeatingOutletNode,       !- Name",
        "    Heat Pump 1 Air Loop Outlet,    !- Component Name or Node Name",
        "    Other,                   !- Component Object Type or Node Type",
        "    3.0;                     !- Node Height {m}",
        "  AirflowNetwork:Distribution:Component:Leak,",
        "    Heat Pump 1 MainSupplyLeak,          !- Name",
        "    0.0025,                  !- Air Mass Flow Coefficient {kg/s}",
        "    0.65;                    !- Air Mass Flow Exponent {dimensionless}",

        "  AirflowNetwork:Distribution:Component:LeakageRatio,",
        "    Heat Pump 1 ZoneSupplyELR1,          !- Name",
        "    0.01,                    !- Effective Leakage Ratio {dimensionless}",
        "    1.9,                     !- Maximum Flow Rate {m3/s}",
        "    59.0,                    !- Reference Pressure Difference {Pa}",
        "    0.65;                    !- Air Mass Flow Exponent {dimensionless}",

        "  AirflowNetwork:Distribution:Component:LeakageRatio,",
        "    Heat Pump 1 ReturnLeakELR1,          !- Name",
        "    0.03,                    !- Effective Leakage Ratio {dimensionless}",
        "    1.9,                     !- Maximum Flow Rate {m3/s}",
        "    41.0,                    !- Reference Pressure Difference {Pa}",
        "    0.65;                    !- Air Mass Flow Exponent {dimensionless}",

        "  AirflowNetwork:Distribution:Component:Duct,",
        "    Heat Pump 1 MainTruck1,              !- Name",
        "    3.0,                     !- Duct Length {m}",
        "    0.6,                     !- Hydraulic Diameter {m}",
        "    0.2827,                  !- Cross Section Area {m2}",
        "    0.0009,                  !- Surface Roughness {m}",
        "    5.0,                    !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
        "    0.946792,                !- Heat Transmittance Coefficient (U-Factor) for Duct Wall Construction {W/m2-K}",
        "    0.0001,                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",
        "    5.018000,                !- Outside Convection Coefficient {W/m2-K}",
        "    25.090000;               !- Inside Convection Coefficient {W/m2-K}",

        "  AirflowNetwork:Distribution:Component:Duct,",
        "    Heat Pump 1 MainTruck2,              !- Name",
        "    4.0,                     !- Duct Length {m}",
        "    0.6,                     !- Hydraulic Diameter {m}",
        "    0.2827,                  !- Cross Section Area {m2}",
        "    0.0009,                  !- Surface Roughness {m}",
        "    0.01,                    !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
        "    0.946792,                !- Heat Transmittance Coefficient (U-Factor) for Duct Wall Construction {W/m2-K}",
        "    0.0001,                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",
        "    5.018000,                !- Outside Convection Coefficient {W/m2-K}",
        "    25.090000;               !- Inside Convection Coefficient {W/m2-K}",

        "  AirflowNetwork:Distribution:Component:Duct,",
        "    Heat Pump 1 Zone1Supply,             !- Name",
        "    5.0,                     !- Duct Length {m}",
        "    0.4,                     !- Hydraulic Diameter {m}",
        "    0.1256,                  !- Cross Section Area {m2}",
        "    0.0009,                  !- Surface Roughness {m}",
        "    1.00,                    !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
        "    0.946792,                !- Heat Transmittance Coefficient (U-Factor) for Duct Wall Construction {W/m2-K}",
        "    0.0001,                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",
        "    5.018000,                !- Outside Convection Coefficient {W/m2-K}",
        "    25.090000;               !- Inside Convection Coefficient {W/m2-K}",

        "  AirflowNetwork:Distribution:Component:Duct,",
        "    Heat Pump 1 Zone1Return,             !- Name",
        "    4.0,                     !- Duct Length {m}",
        "    0.50,                    !- Hydraulic Diameter {m}",
        "    0.1963,                  !- Cross Section Area {m2}",
        "    0.0009,                  !- Surface Roughness {m}",
        "    1.0,                     !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
        "    0.946792,                !- Heat Transmittance Coefficient (U-Factor) for Duct Wall Construction {W/m2-K}",
        "    0.0001,                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",
        "    5.018000,                !- Outside Convection Coefficient {W/m2-K}",
        "    25.090000;               !- Inside Convection Coefficient {W/m2-K}",

        "  AirflowNetwork:Distribution:Component:Fan,",
        "    Heat Pump 1 Supply Fan,            !- Fan Name",
        "    Fan:OnOff;      !- Supply Fan Object Type",

        "  AirflowNetwork:Distribution:Component:Coil,",
        "    Heat Pump 1 Cooling Coil,              !- Coil Name",
        "    Coil:Cooling:DX:SingleSpeed,  !- Coil Object Type",
        "    0.1,                     !- Air Path Length {m}",
        "    1.00;                    !- Air Path Hydraulic Diameter {m}",

        "  AirflowNetwork:Distribution:Component:Coil,",
        "    Heat Pump 1 HP Heating Coil,     !- Coil Name",
        "    Coil:Heating:DX:SingleSpeed,       !- Coil Object Type",
        "    0.1,                     !- Air Path Length {m}",
        "    1.00;                    !- Air Path Hydraulic Diameter {m}",

        "  AirflowNetwork:Distribution:Component:Coil,",
        "    Heat Pump 1 Sup Heat Coil,     !- Coil Name",
        "    Coil:Heating:Electric,       !- Coil Object Type",
        "    0.1,                     !- Air Path Length {m}",
        "    1.00;                    !- Air Path Hydraulic Diameter {m}",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Heat Pump 1 Main Link 1,             !- Name",
        "    Heat Pump 1 EquipmentInletNode,      !- Node 1 Name",
        "    Heat Pump 1 EquipmentOutletNode,     !- Node 2 Name",
        "    Heat Pump 1 MainTruck1,              !- Component Name",
        "    Attic NORTH ZONE;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Heat Pump 1 Main Link 2,             !- Name",
        "    Heat Pump 1 EquipmentOutletNode,          !- Node 1 Name",
        "    Heat Pump 1 SupplyMainNode,        !- Node 2 Name",
        "    Heat Pump 1 MainTruck2,              !- Component Name",
        "    Attic NORTH ZONE;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Heat Pump 1 Zone1Supply1Link,        !- Name",
        "    Heat Pump 1 SupplyMainNode,        !- Node 1 Name",
        "    Heat Pump 1 MainSplitterNode,         !- Node 2 Name",
        "    Heat Pump 1 Zone1Supply,             !- Component Name",
        "    Attic NORTH ZONE;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Heat Pump 1 Zone1Supply2Link,        !- Name",
        "    Heat Pump 1 MainSplitterNode,         !- Node 1 Name",
        "    Heat Pump 1 Zone1SupplyNode,        !- Node 2 Name",
        "    Heat Pump 1 Zone1Supply,             !- Component Name",
        "    Attic NORTH Zone;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Heat Pump 1 Zone1InletlLink,     !- Name",
        "    Heat Pump 1 Zone1SupplyNode,        !- Node 1 Name",
        "    Heat Pump 1 Zone1SupplyRegisterNode, !- Node 2 Name",
        "    Heat Pump 1 Zone1Supply,             !- Component Name",
        "    Attic NORTH Zone;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Heat Pump 1 Zone1SupplyConnectionLink,  !- Name",
        "    Heat Pump 1 Zone1SupplyRegisterNode, !- Node 1 Name",
        "    NORTH Zone,               !- Node 2 Name",
        "    ZoneConnectionDuct;      !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Heat Pump 1 Zone1ReturnConnectionLink,  !- Name",
        "    NORTH Zone,               !- Node 1 Name",
        "    Heat Pump 1 Zone1OutletNode,         !- Node 2 Name",
        "    ZoneConnectionDuct;      !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Heat Pump 1 Zone1Return1Link,        !- Name",
        "    Heat Pump 1 Zone1OutletNode,         !- Node 1 Name",
        "    Heat Pump 1 Zone1ReturnNode,         !- Node 2 Name",
        "    Heat Pump 1 Zone1Return,             !- Component Name",
        "    Attic NORTH Zone;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Heat Pump 1 Zone1Return2Link,        !- Name",
        "    Heat Pump 1 Zone1ReturnNode,         !- Node 1 Name",
        "    Heat Pump 1 MainMixerNode,           !- Node 2 Name",
        "    Heat Pump 1 Zone1Return,             !- Component Name",
        "    Attic NORTH Zone;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Heat Pump 1 ReturnMixerLink,         !- Name",
        "    Heat Pump 1 MainMixerNode,           !- Node 1 Name",
        "    Heat Pump 1 MainReturnNode,          !- Node 2 Name",
        "    MixerConnectionDuct,     !- Component Name",
        "    Attic NORTH Zone;              !- Thermal Zone Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Heat Pump 1 AirLoopReturnLink,       !- Name",
        "    Heat Pump 1 MainReturnNode,          !- Node 1 Name",
        "    Heat Pump 1 MainInletNode,           !- Node 2 Name",
        "    AirLoopReturn;           !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Heat Pump 1 OASystemInletLink,       !- Name",
        "    Heat Pump 1 MainInletNode,           !- Node 1 Name",
        "    Heat Pump 1 OA System Node,          !- Node 2 Name",
        "    ZoneConnectionDuct;      !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Heat Pump 1 OAMixerOutletLink,       !- Name",
        "    Heat Pump 1 OA System Node,          !- Node 1 Name",
        "    Heat Pump 1 FanInletNode,            !- Node 2 Name",
        "    ZoneConnectionDuct;      !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Heat Pump 1 OASystemFanLink,       !- Name",
        "    Heat Pump 1 OA Inlet Node,           !- Node 1 Name",
        "    Heat Pump 1 OA System Node,          !- Node 2 Name",
        "    Heat Pump 1 OA Fan;                  !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Heat Pump 1 SupplyFanLink,           !- Name",
        "    Heat Pump 1 FanInletNode,            !- Node 1 Name",
        "    Heat Pump 1 FanOutletNode,           !- Node 2 Name",
        "    Heat Pump 1 Supply Fan;            !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Heat Pump 1 CoolingCoilLink,         !- Name",
        "    Heat Pump 1 FanOutletNode,           !- Node 1 Name",
        "    Heat Pump 1 HeatingInletNode,        !- Node 2 Name",
        "    Heat Pump 1 Cooling Coil;              !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Heat Pump 1 HeatingCoilLink,         !- Name",
        "    Heat Pump 1 HeatingInletNode,        !- Node 1 Name",
        "    Heat Pump 1 HeatingOutletNode,       !- Node 2 Name",
        "    Heat Pump 1 HP Heating Coil;     !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Heat Pump 1 SuppHeatingCoilLink,         !- Name",
        "    Heat Pump 1 HeatingOutletNode,        !- Node 1 Name",
        "    Heat Pump 1 SuppHeatingOutletNode,       !- Node 2 Name",
        "    Heat Pump 1 Sup Heat Coil;     !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Heat Pump 1 EquipmentAirLoopLink,    !- Name",
        "    Heat Pump 1 SuppHeatingOutletNode,       !- Node 1 Name",
        "    Heat Pump 1 EquipmentInletNode,      !- Node 2 Name",
        "    AirLoopSupply;           !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Heat Pump 1 Zone1ReturnLeakLink,     !- Name",
        "    Heat Pump 1 Zone1ReturnNode,         !- Node 1 Name",
        "    Heat Pump 1 OA Inlet Node,           !- Node 2 Name",
        "    Heat Pump 1 ReturnLeakELR1;          !- Component Name",

        "  AirflowNetwork:Distribution:Linkage,",
        "    Heat Pump 1 MainSupplyLeakLink,      !- Name",
        "    Heat Pump 1 SupplyMainNode,          !- Node 1 Name",
        "    ATTIC NORTH ZONE,              !- Node 2 Name",
        "    Heat Pump 1 MainSupplyLeak;          !- Component Name",
        "  AirflowNetwork:Distribution:Component:OutdoorAirFlow,",
        "    Heat Pump 1 OA Fan,                  !- Name",
        "    Heat Pump 1 OA Mixing Box,         !- Outdoor Air Mixer Name",
        "    0.001,                   !- Air Mass Flow Coefficient When No Outdoor Air Flow at Reference Conditions {kg/s}",
        "    0.667;                   !- Air Mass Flow Exponent When No Outdoor Air Flow {dimensionless}",

        "  AvailabilityManagerAssignmentList,",
        "    Reheat System 1 Avail List,  !- Name",
        "    AvailabilityManager:Scheduled,  !- Availability Manager 1 Object Type",
        "    Reheat System 1 Avail;   !- Availability Manager 1 Name",

        "  AvailabilityManager:Scheduled,",
        "    Reheat System 1 Avail,   !- Name",
        "    FanAndCoilAvailSched;    !- Schedule Name",

        "  SetpointManager:SingleZone:Reheat,",
        "    Supply Air Temp Manager, !- Name",
        "    Temperature,             !- Control Variable",
        "    13.,                     !- Minimum Supply Air Temperature {C}",
        "    45.,                     !- Maximum Supply Air Temperature {C}",
        "    WEST ZONE,              !- Control Zone Name",
        "    Zone 1 Node,             !- Zone Node Name",
        "    Zone 1 Inlet Node,       !- Zone Inlet Node Name",
        "    Supply Air Temp Nodes;   !- Setpoint Node or NodeList Name",

        "  Controller:OutdoorAir,",
        "    OA Controller 1,         !- Name",
        "    Relief Air Outlet Node,  !- Relief Air Outlet Node Name",
        "    Air Loop Inlet Node,     !- Return Air Node Name",
        "    Mixed Air Node,          !- Mixed Air Node Name",
        "    Outside Air Inlet Node,  !- Actuator Node Name",
        "    0.2333,                  !- Minimum Outdoor Air Flow Rate {m3/s}",
        "    1.16,                     !- Maximum Outdoor Air Flow Rate {m3/s}",
        "    NoEconomizer,            !- Economizer Control Type",
        "    ModulateFlow,            !- Economizer Control Action Type",
        "    19.,                     !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
        "    ,                        !- Economizer Maximum Limit Enthalpy {J/kg}",
        "    ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
        "    ,                        !- Electronic Enthalpy Limit Curve Name",
        "    4.,                      !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
        "    NoLockout,               !- Lockout Type",
        "    FixedMinimum;            !- Minimum Limit Type",

        "  ZoneHVAC:EquipmentConnections,",
        "    West Zone,               !- Zone Name",
        "    Zone1Equipment,          !- Zone Conditioning Equipment List Name",
        "    Zone1Inlets,             !- Zone Air Inlet Node or NodeList Name",
        "    ,                        !- Zone Air Exhaust Node or NodeList Name",
        "    Zone 1 Node,             !- Zone Air Node Name",
        "    Zone 1 Outlet Node;      !- Zone Return Air Node or NodeList Name",

        "  ZoneHVAC:EquipmentConnections,",
        "    EAST ZONE,               !- Zone Name",
        "    Zone2Equipment,          !- Zone Conditioning Equipment List Name",
        "    Zone2Inlets,             !- Zone Air Inlet Node or NodeList Name",
        "    ,                        !- Zone Air Exhaust Node or NodeList Name",
        "    Zone 2 Node,             !- Zone Air Node Name",
        "    Zone 2 Outlet Node;      !- Zone Return Air Node or NodeList Name",

        "  ZoneHVAC:EquipmentList,",
        "    Zone1Equipment,          !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    AirTerminal:SingleDuct:Uncontrolled,  !- Zone Equipment 1 Object Type",
        "    Zone1DirectAir,          !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",

        "  ZoneHVAC:EquipmentList,",
        "    Zone2Equipment,          !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        "    Zone2TermReheat,         !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",

        "  AirTerminal:SingleDuct:Uncontrolled,",
        "    Zone1DirectAir,          !- Name",
        "    FanAndCoilAvailSched,    !- Availability Schedule Name",
        "    Zone 1 Inlet Node,       !- Zone Supply Air Node Name",
        "    0.64;                    !- Maximum Air Flow Rate {m3/s}",

        "  ZoneHVAC:AirDistributionUnit,",
        "    Zone2TermReheat,         !- Name",
        "    Zone 2 Reheat Air Outlet Node,  !- Air Distribution Unit Outlet Node Name",
        "    AirTerminal:SingleDuct:ConstantVolume:Reheat,  !- Air Terminal Object Type",
        "    Reheat Zone 2;           !- Air Terminal Name",

        "  CoilSystem:Cooling:DX,",
        "    DX Cooling Coil System 1,!- Name",
        "    CoolingCoilAvailSched,   !- Availability Schedule Name",
        "    Cooling Coil Air Inlet Node,  !- DX Cooling Coil System Inlet Node Name",
        "    Heating Coil Air Inlet Node,  !- DX Cooling Coil System Outlet Node Name",
        "    Heating Coil Air Inlet Node,  !- DX Cooling Coil System Sensor Node Name",
        "    Coil:Cooling:DX:SingleSpeed,  !- Cooling Coil Object Type",
        "    ACDXCoil 1;              !- Cooling Coil Name",

        "  AirTerminal:SingleDuct:ConstantVolume:Reheat,",
        "    Reheat Zone 2,           !- Name",
        "    FanAndCoilAvailSched,    !- Availability Schedule Name",
        "    Zone 2 Reheat Air Outlet Node,  !- Air Outlet Node Name",
        "    Zone 2 Reheat Air Inlet Node,  !- Air Inlet Node Name",
        "    0.52,                    !- Maximum Air Flow Rate {m3/s}",
        "    Coil:Heating:Fuel,       !- Reheat Coil Object Type",
        "    Reheat Coil Zone 2,      !- Reheat Coil Name",
        "    0.0,                     !- Maximum Hot Water or Steam Flow Rate {m3/s}",
        "    0.0,                     !- Minimum Hot Water or Steam Flow Rate {m3/s}",
        "    0.001;                   !- Convergence Tolerance",

        "  ZoneControl:Thermostat,",
        "    Zone 1 Thermostat,       !- Name",
        "    West Zone,               !- Zone or ZoneList Name",
        "    Dual Zone Control Type Sched,  !- Control Type Schedule Name",
        "    ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
        "    Setpoints;               !- Control 1 Name",

        "  ZoneControl:Thermostat,",
        "    Zone 2 Thermostat,       !- Name",
        "    EAST ZONE,               !- Zone or ZoneList Name",
        "    Dual Zone Control Type Sched,  !- Control Type Schedule Name",
        "    ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
        "    Setpoints;               !- Control 1 Name",

        "  AirLoopHVAC:SupplyPath,",
        "    TermReheatSupplyPath,    !- Name",
        "    Zone Equipment Inlet Node,  !- Supply Air Path Inlet Node Name",
        "    AirLoopHVAC:ZoneSplitter,!- Component 1 Object Type",
        "    Zone Supply Air Splitter;!- Component 1 Name",

        "  AirLoopHVAC:ReturnPath,",
        "    TermReheatReturnPath,    !- Name",
        "    Return Air Mixer Outlet, !- Return Air Path Outlet Node Name",
        "    AirLoopHVAC:ZoneMixer,   !- Component 1 Object Type",
        "    Zone Return Air Mixer;   !- Component 1 Name",

        "  AirLoopHVAC:ZoneSplitter,",
        "    Zone Supply Air Splitter,!- Name",
        "    Zone Equipment Inlet Node,  !- Inlet Node Name",
        "    Zone 1 Inlet Node,  !- Outlet 1 Node Name",
        "    Zone 2 Reheat Air Inlet Node;  !- Outlet 2 Node Name",

        "  AirLoopHVAC:ZoneMixer,",
        "    Zone Return Air Mixer,   !- Name",
        "    Return Air Mixer Outlet, !- Outlet Node Name",
        "    Zone 1 Outlet Node,      !- Inlet 1 Node Name",
        "    Zone 2 Outlet Node;      !- Inlet 2 Node Name",

        "  Coil:Heating:Fuel,",
        "    Main Heating Coil 1,     !- Name",
        "    FanAndCoilAvailSched,    !- Availability Schedule Name",
        "    NaturalGas,              !- Fuel Type",
        "    0.8,                     !- Burner Efficiency",
        "    45000,                   !- Nominal Capacity {W}",
        "    Heating Coil Air Inlet Node,  !- Air Inlet Node Name",
        "    Air Loop Outlet Node,    !- Air Outlet Node Name",
        "    Air Loop Outlet Node;    !- Temperature Setpoint Node Name",

        "  Coil:Heating:Fuel,",
        "    Reheat Coil Zone 2,      !- Name",
        "    FanAndCoilAvailSched,    !- Availability Schedule Name",
        "    NaturalGas,              !- Fuel Type",
        "    1.0,                     !- Burner Efficiency",
        "    3000,                    !- Nominal Capacity {W}",
        "    Zone 2 Reheat Air Inlet Node,  !- Air Inlet Node Name",
        "    Zone 2 Reheat Air Outlet Node;  !- Air Outlet Node Name",

        "  Coil:Cooling:DX:SingleSpeed,",
        "    ACDXCoil 1,              !- Name",
        "    CoolingCoilAvailSched,   !- Availability Schedule Name",
        "    20000,                   !- Gross Rated Total Cooling Capacity {W}",
        "    0.75,                     !- Gross Rated Sensible Heat Ratio",
        "    3.0,                     !- Gross Rated Cooling COP {W/W}",
        "    1.16,                     !- Rated Air Flow Rate {m3/s}",
        "    ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "    Cooling Coil Air Inlet Node,  !- Air Inlet Node Name",
        "    Heating Coil Air Inlet Node,  !- Air Outlet Node Name",
        "    WindACCoolCapFT,         !- Total Cooling Capacity Function of Temperature Curve Name",
        "    WindACCoolCapFFF,        !- Total Cooling Capacity Function of Flow Fraction Curve Name",
        "    WindACEIRFT,             !- Energy Input Ratio Function of Temperature Curve Name",
        "    WindACEIRFFF,            !- Energy Input Ratio Function of Flow Fraction Curve Name",
        "    WindACPLFFPLR;           !- Part Load Fraction Correlation Curve Name",

        "  Fan:ConstantVolume,",
        "    Supply Fan 1,            !- Name",
        "    FanAndCoilAvailSched,    !- Availability Schedule Name",
        "    0.7,                     !- Fan Total Efficiency",
        "    600.0,                   !- Pressure Rise {Pa}",
        "    1.16,                     !- Maximum Flow Rate {m3/s}",
        "    0.9,                     !- Motor Efficiency",
        "    1.0,                     !- Motor In Airstream Fraction",
        "    Mixed Air Node,          !- Air Inlet Node Name",
        "    Cooling Coil Air Inlet Node;  !- Air Outlet Node Name",

        "  ZoneControl:Thermostat,",
        "    Zone 3 Thermostat,       !- Name",
        "    NORTH ZONE,               !- Zone or ZoneList Name",
        "    Dual Zone Control Type Sched,  !- Control Type Schedule Name",
        "    ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
        "    Setpoints;               !- Control 1 Name",

        "  ThermostatSetpoint:DualSetpoint,",
        "    Setpoints,               !- Name",
        "    Dual Heating Setpoints,  !- Heating Setpoint Temperature Schedule Name",
        "    Dual Cooling Setpoints;  !- Cooling Setpoint Temperature Schedule Name",

        "ScheduleTypeLimits,",
        "  HVACTemplate Any Number;                                 !- Name",

        "DesignSpecification:OutdoorAir,",
        "  SZ DSOA NORTH ZONE,                                      !- Name",
        "  flow/person,                                             !- Outdoor Air Method",
        "  0.00944,                                                 !- Outdoor Air Flow per Person {m3/s}",
        "  0.0,                                                     !- Outdoor Air Flow per Zone Floor Area {m3/s-m2}",
        "  0.0;                                                     !- Outdoor Air Flow per Zone {m3/s}",

        "Sizing:Zone,",
        "  NORTH ZONE,                                              !- Zone or ZoneList Name",
        "  SupplyAirTemperature,                                    !- Zone Cooling Design Supply Air Temperature Input Method",
        "  14,                                                      !- Zone Cooling Design Supply Air Temperature {C}",
        "  11.11,                                                   !- Zone Cooling Design Supply Air Temperature Difference {delta C}",
        "  SupplyAirTemperature,                                    !- Zone Heating Design Supply Air Temperature Input Method",
        "  50.0,                                                    !- Zone Heating Design Supply Air Temperature {C}",
        "  ,                                                        !- Zone Heating Design Supply Air Temperature Difference {delta C}",
        "  0.008,                                                   !- Zone Cooling Design Supply Air Humidity Ratio {kg-H20/kg-air}",
        "  0.008,                                                   !- Zone Heating Design Supply Air Humidity Ratio {kg-H2O/kg-air}",
        "  SZ DSOA NORTH ZONE,                                      !- Design Specification Outdoor Air Object Name",
        "  ,                                                        !- Zone Heating Sizing Factor",
        "  ,                                                        !- Zone Cooling Sizing Factor",
        "  DesignDay,                                               !- Cooling Design Air Flow Method",
        "  0,                                                       !- Cooling Design Air Flow Rate {m3/s}",
        "  ,                                                        !- Cooling Minimum Air Flow per Zone Floor Area {m3/s-m2}",
        "  ,                                                        !- Cooling Minimum Air Flow {m3/s}",
        "  0,                                                       !- Cooling Minimum Air Flow Fraction {}",
        "  DesignDay,                                               !- Heating Design Air Flow Method",
        "  0,                                                       !- Heating Design Air Flow Rate {m3/s}",
        "  ,                                                        !- Heating Maximum Air Flow per Zone Floor Area {m3/s-m2}",
        "  ,                                                        !- Heating Maximum Air Flow {m3/s}",
        "  0,                                                       !- Heating Maximum Air Flow Fraction {}",
        "  SZ DSZAD NORTH ZONE;                                     !- Design Specification Zone Air Distribution Object Name",

        "DesignSpecification:ZoneAirDistribution,",
        "  SZ DSZAD NORTH ZONE,                                     !- Name",
        "  1,                                                       !- Zone Air Distribution Effectiveness in Cooling Mode {}",
        "  1;                                                       !- Zone Air Distribution Effectiveness in Heating Mode {}",

        "ZoneHVAC:EquipmentConnections,",
        "  NORTH ZONE,                                              !- Zone Name",
        "  NORTH ZONE Equipment,                                    !- Zone Conditioning Equipment List Name",
        "  NORTH ZONE Zone Equip Inlet,                             !- Zone Air Inlet Node or NodeList Name",
        "  NORTH ZONE Exhaust Node,                                                        !- Zone Air Exhaust Node or NodeList Name",
        "  NORTH ZONE Zone Air Node,                                !- Zone Air Node Name",
        "  NORTH ZONE Return Outlet;                                !- Zone Return Air Node Name",

        "ZoneHVAC:EquipmentList,",
        "  NORTH ZONE Equipment,                                    !- Name",
        "  SequentialLoad,          !- Load Distribution Scheme",
        "  AirTerminal:SingleDuct:Uncontrolled,                     !- Zone Equipment Object Type",
        "  NORTH ZONE Air Terminal,                                 !- Zone Equipment Name",
        "  1,                                                       !- Zone Equipment Cooling Sequence",
        "  1,                                                       !- Zone Equipment Heating or No-Load Sequence",
        "  Fan:ZoneExhaust,         !- Zone Equipment 2 Object Type",
        "  NORTH ZONE Exhaust Fan,       !- Zone Equipment 2 Name",
        "  2,                       !- Zone Equipment 2 Cooling Sequence",
        "  2;                       !- Zone Equipment 2 Heating or No-Load Sequence",

        "Fan:ZoneExhaust,",
        "  NORTH ZONE Exhaust Fan,       !- Name",
        "  FanAndCoilAvailSched,    !- Availability Schedule Name",
        "  0.7,                     !- Fan Total Efficiency",
        "  500,                     !- Pressure Rise {Pa}",
        "  0.15,                    !- Maximum Flow Rate {m3/s}",
        "  NORTH ZONE Exhaust Node,      !- Air Inlet Node Name",
        "  NORTH ZONE Exhaust Fan Outlet Node,  !- Air Outlet Node Name",
        "  NORTH ZONE Exhaust;            !- End-Use Subcategory",

        "AirTerminal:SingleDuct:Uncontrolled,",
        "  NORTH ZONE Air Terminal,                                 !- Name",
        "  ,                                                        !- Availability Schedule Name",
        "  NORTH ZONE Zone Equip Inlet,                             !- Zone Supply Air Node Name",
        "  0.5;                                                !- Maximum air flow rate {m3/s}",

        "AirLoopHVAC,",
        "  Heat Pump 1,                                             !- Name",
        "  ,                                                        !- Controller List Name",
        "  Heat Pump 1 Availability Managers,                       !- Availability Manager List Name",
        "  0.5,                                                !- Design Supply Air Flow Rate {m3/s}",
        "  Heat Pump 1 Branches,                                    !- Branch List Name",
        "  ,                                                        !- Connector List Name",
        "  Heat Pump 1 Air Loop Inlet,                              !- Supply Side Inlet Node Name",
        "  Heat Pump 1 Return Air Outlet,                           !- Demand Side Outlet Node Name",
        "  Heat Pump 1 Supply Path Inlet,                           !- Demand Side Inlet Node Names",
        "  Heat Pump 1 Air Loop Outlet;                             !- Supply Side Outlet Node Names",

        "BranchList,",
        "  Heat Pump 1 Branches,                                    !- Name",
        "  Heat Pump 1 Main Branch;                                 !- Branch Name",

        "Branch,",
        "  Heat Pump 1 Main Branch,                                 !- Name",
        "  ,                                                        !- Pressure Drop Curve Name",
        "  AirLoopHVAC:OutdoorAirSystem,                            !- Component Object Type",
        "  Heat Pump 1 OA System,                                   !- Component Name",
        "  Heat Pump 1 Air Loop Inlet,                              !- Component Inlet Node Name",
        "  Heat Pump 1 Mixed Air Outlet,                            !- Component Outlet Node Name",
        "  AirLoopHVAC:UnitaryHeatPump:AirToAir,                    !- Component Object Type",
        "  Heat Pump 1 Heat Pump,                                   !- Component Name",
        "  Heat Pump 1 Mixed Air Outlet,                            !- Component Inlet Node Name",
        "  Heat Pump 1 Air Loop Outlet;                             !- Component Outlet Node Name",

        "AirLoopHVAC:SupplyPath,",
        "  Heat Pump 1 Supply Path,                                 !- Name",
        "  Heat Pump 1 Supply Path Inlet,                           !- Supply Air Path Inlet Node Name",
        "  AirLoopHVAC:ZoneSplitter,                                !- Component Object Type",
        "  Heat Pump 1 Zone Splitter;                               !- Component Name",

        "AirLoopHVAC:ZoneSplitter,",
        "  Heat Pump 1 Zone Splitter,                               !- Name",
        "  Heat Pump 1 Supply Path Inlet,                           !- Inlet Node Name",
        "  NORTH ZONE Zone Equip Inlet;                             !- Outlet Node Name",

        "AirLoopHVAC:ReturnPath,",
        "  Heat Pump 1 Return Path,                                 !- Name",
        "  Heat Pump 1 Return Air Outlet,                           !- Return Air Path Outlet Node Name",
        "  AirLoopHVAC:ZoneMixer,                                   !- Component Object Type",
        "  Heat Pump 1 Zone Mixer;                                  !- Component Name",

        "AirLoopHVAC:ZoneMixer,",
        "  Heat Pump 1 Zone Mixer,                                  !- Name",
        "  Heat Pump 1 Return Air Outlet,                           !- Outlet Node Name",
        "  NORTH ZONE Return Outlet;                                !- Inlet Node Name",

        "AvailabilityManagerAssignmentList,",
        "  Heat Pump 1 Availability Managers,                       !- Name",
        "  AvailabilityManager:NightCycle,                          !- Availability Manager Object Type",
        "  Heat Pump 1 Availability;                                !- Availability Manager Name",

        "AvailabilityManager:NightCycle,",
        "  Heat Pump 1 Availability,                                !- Name",
        "  HVACTemplate-Always 1,                                   !- Applicability Schedule Name",
        "  FanAndCoilAvailSched,                                           !- Fan Schedule Name",
        "  CycleOnAny,                                              !- Control Type",
        "  0.2,                                                     !- Thermostat Tolerance {deltaC}",
        "  FixedRunTime,                                            !- Cycling Run Time Control Type",
        "  3600,                                                    !- Cycling run time {s}",
        "  ;                                                        !- Control Zone Name",

        "Schedule:Compact,",
        "  HVACTemplate-Always 1,                                   !- Name",
        "  HVACTemplate Any Number,                                 !- Schedule Type Limits Name",
        "  Through: 12/31,                                          !- Field 1",
        "  For: AllDays,                                            !- Field 2",
        "  Until: 24:00,                                            !- Field 3",
        "  1;                                                       !- Field 4",

        "AirLoopHVAC:UnitaryHeatPump:AirToAir,",
        "  Heat Pump 1 Heat Pump,                                   !- Name",
        "  ,                                                        !- Availability Schedule Name",
        "  Heat Pump 1 Mixed Air Outlet,                            !- Air Inlet Node Name",
        "  Heat Pump 1 Air Loop Outlet,                             !- Air Outlet Node Name",
        "  0.5,                                                !- Cooling Supply Air Flow Rate",
        "  0.5,                                                !- Heating Supply Air Flow Rate",
        "  0.0,                                                !- No Load Supply Air Flow Rate",
        "  NORTH ZONE,                                              !- Controlling Zone or Thermostat Location",
        "  Fan:OnOff,                                               !- Supply Air Fan Object Type",
        "  Heat Pump 1 Supply Fan,                                  !- Supply Air Fan Name",
        "  Coil:Heating:DX:SingleSpeed,                             !- Heating Coil Object Type",
        "  Heat Pump 1 HP Heating Coil,                             !- Heating Coil Name",
        "  Coil:Cooling:DX:SingleSpeed,                             !- Cooling Coil Object Type",
        "  Heat Pump 1 Cooling Coil,                                !- Cooling Coil Name",
        "  Coil:Heating:Electric,                                   !- Supplemental Heating Coil Object Type",
        "  Heat Pump 1 Sup Heat Coil,                               !- Supplemental Heating Coil Name",
        "  80.0,                                                !- Maximum Supply Air Temperature from Supplemental Heater",
        "  21,                                                      !- Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation",
        "  BlowThrough,                                             !- Fan Placement",
        "  HVACTemplate-Always 0;                                   !- Supply Air Fan Operating Mode Schedule Name",

        "Schedule:Compact,",
        "  HVACTemplate-Always 0,                                   !- Name",
        "  HVACTemplate Any Number,                                 !- Schedule Type Limits Name",
        "  Through: 12/31,                                          !- Field 1",
        "  For: AllDays,                                            !- Field 2",
        "  Until: 24:00,                                            !- Field 3",
        "  0;                                                       !- Field 4",

        "Coil:Heating:DX:SingleSpeed,",
        "  Heat Pump 1 HP Heating Coil,                             !- Name",
        "  ,                                                        !- Availability Schedule Name",
        "  9000.0,                                                !- Rated Total Heating Capacity {W}",
        "  2.75,                                                    !- Rated COP",
        "  0.5,                                                !- Rated Air Flow Rate {m3/s}",
        "  ,                                                        !- Rated Evaporator Fan Power Per Volume Flow Rate",
        "  Heat Pump 1 Cooling Coil Outlet,                         !- Air Inlet Node Name",
        "  Heat Pump 1 Heating Coil Outlet,                         !- Air Outlet Node Name",
        "  Heat Pump 1 HP Heating Coil Cap-FT,                      !- Total Heating Capacity Function of Temperature Curve Name",
        "  Heat Pump 1 HP Heating Coil Cap-FF,                      !- Total Heating Capacity Function of Flow Fraction Curve Name",
        "  Heat Pump 1 HP Heating Coil EIR-FT,                      !- Energy Input Ratio Function of Temperature Curve Name",
        "  Heat Pump 1 HP Heating Coil EIR-FF,                      !- Energy Input Ratio Function of Flow Fraction Curve Name",
        "  Heat Pump 1 HP Heating Coil PLF,                         !- Part Load Fraction Correlation Curve Name",
        "  Heat Pump 1 HP Heating Coil DefrEIR-FT,                  !- Defrost Energy Input Ratio Function of Temperature Curve Name",
        "  -8,                                                      !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "  ,                                                        !- Outdoor Dry-Bulb Temperature to Turn On Compressor",
        "  5,                                                       !- Maximum Outdoor Dry-Bulb Temperature for Defrost Operation {C}",
        "  0,                                                       !- Crankcase Heater Capacity {W}",
        "  0,                                                       !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
        "  ReverseCycle,                                            !- Defrost Strategy",
        "  Timed,                                                   !- Defrost Control",
        "  0.058333,                                                !- Defrost Time Period Fraction",
        "  10.0;                                                !- Resistive Defrost Heater Capacity {W}",

        "Curve:Cubic,",
        "  Heat Pump 1 HP Heating Coil Cap-FT,                      !- Name",
        "  0.758746,                                                !- Coefficient1 Constant",
        "  0.027626,                                                !- Coefficient2 x",
        "  0.000148716,                                             !- Coefficient3 x**2",
        "  0.0000034992,                                            !- Coefficient4 x**3",
        "  -20.0,                                                   !- Minimum Value of x",
        "  20.0;                                                    !- Maximum Value of x",

        "Curve:Cubic,",
        "  Heat Pump 1 HP Heating Coil Cap-FF,                      !- Name",
        "  0.84,                                                    !- Coefficient1 Constant",
        "  0.16,                                                    !- Coefficient2 x",
        "  0.0,                                                     !- Coefficient3 x**2",
        "  0.0,                                                     !- Coefficient4 x**3",
        "  0.5,                                                     !- Minimum Value of x",
        "  1.5;                                                     !- Maximum Value of x",

        "Curve:Cubic,",
        "  Heat Pump 1 HP Heating Coil EIR-FT,                      !- Name",
        "  1.19248,                                                 !- Coefficient1 Constant",
        "  -0.0300438,                                              !- Coefficient2 x",
        "  0.00103745,                                              !- Coefficient3 x**2",
        "  -0.000023328,                                            !- Coefficient4 x**3",
        "  -20.0,                                                   !- Minimum Value of x",
        "  20.0;                                                    !- Maximum Value of x",

        "Curve:Quadratic,",
        "  Heat Pump 1 HP Heating Coil EIR-FF,                      !- Name",
        "  1.3824,                                                  !- Coefficient1 Constant",
        "  -0.4336,                                                 !- Coefficient2 x",
        "  0.0512,                                                  !- Coefficient3 x**2",
        "  0.0,                                                     !- Minimum Value of x",
        "  1.0;                                                     !- Maximum Value of x",

        "Curve:Quadratic,",
        "  Heat Pump 1 HP Heating Coil PLF,                         !- Name",
        "  0.75,                                                    !- Coefficient1 Constant",
        "  0.25,                                                    !- Coefficient2 x",
        "  0.0,                                                     !- Coefficient3 x**2",
        "  0.0,                                                     !- Minimum Value of x",
        "  1.0;                                                     !- Maximum Value of x",

        "Curve:Biquadratic,",
        "  Heat Pump 1 HP Heating Coil DefrEIR-FT,                  !- Name",
        "  1,                                                       !- Coefficient1 Constant",
        "  0,                                                       !- Coefficient2 x",
        "  0,                                                       !- Coefficient3 x**2",
        "  0,                                                       !- Coefficient4 y",
        "  0,                                                       !- Coefficient5 y**2",
        "  0,                                                       !- Coefficient6 x*y",
        "  0,                                                       !- Minimum Value of x",
        "  50,                                                      !- Maximum Value of x",
        "  0,                                                       !- Minimum Value of y",
        "  50;                                                      !- Maximum Value of y",

        "Coil:Heating:Electric,",
        "  Heat Pump 1 Sup Heat Coil,                               !- Name",
        "  ,                                                        !- Availability Schedule Name",
        "  1,                                                       !- Efficiency",
        "  12000.0,                                                !- Nominal Capacity of the Coil {W}",
        "  Heat Pump 1 Heating Coil Outlet,                         !- Air Inlet Node Name",
        "  Heat Pump 1 Air Loop Outlet,                             !- Air Outlet Node Name",
        "  ;                                                        !- Coil Temp Setpoint Node",

        "Coil:Cooling:DX:SingleSpeed,",
        "  Heat Pump 1 Cooling Coil,                                !- Name",
        "  ,                                                        !- Availability Schedule Name",
        "  9000.0,                                                !- Gross Rated Total Cooling Capacity {W}",
        "  0.8,                                                !- Gross Rated Sensible Heat Ratio",
        "  3,                                                       !- Rated COP",
        "  0.5,                                                !- Rated Air Flow Rate {m3/s}",
        "  ,                                                        !- Rated Evaporator Fan Power per Volume Flow Rate {W/(m3/s)}",
        "  Heat Pump 1 Supply Fan Outlet,                           !- Air Inlet Node Name",
        "  Heat Pump 1 Cooling Coil Outlet,                         !- Air Outlet Node Name",
        "  Heat Pump 1 Cool Coil Cap-FT,                            !- Total Cooling Capacity Function of Temperature Curve Name",
        "  Heat Pump 1 Cool Coil Cap-FF,                            !- Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  Heat Pump 1 Cool Coil EIR-FT,                            !- Energy Input Ratio Function of Temperature Curve Name",
        "  Heat Pump 1 Cool Coil EIR-FF,                            !- Energy Input Ratio Function of Flow Fraction Curve Name",
        "  Heat Pump 1 Cool Coil PLF,                               !- Part Load Fraction Correlation Curve Name",
        "  ,                                                        !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "  0,                                                       !- Nominal Time for Condensate Removal to Begin",
        "  0,                                                       !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity",
        "  0,                                                       !- Maximum Cycling Rate",
        "  0,                                                       !- Latent Capacity Time Constant",
        "  Heat Pump 1 Cooling Coil Condenser Inlet,                !- Condenser Air Inlet Node Name",
        "  AirCooled,                                               !- Condenser Type",
        "  0,                                                       !- Evaporative Condenser Effectiveness",
        "  ,                                                        !- Evaporative Condenser Air Flow Rate",
        "  0,                                                       !- Evaporative Condenser Pump Rated Power Consumption",
        "  0,                                                       !- Crankcase Heater Capacity",
        "  10;                                                      !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation",

        "Curve:Biquadratic,",
        "  Heat Pump 1 Cool Coil Cap-FT,                            !- Name",
        "  0.942587793,                                             !- Coefficient1 Constant",
        "  0.009543347,                                             !- Coefficient2 x",
        "  0.00068377,                                              !- Coefficient3 x**2",
        "  -0.011042676,                                            !- Coefficient4 y",
        "  0.000005249,                                             !- Coefficient5 y**2",
        "  -0.00000972,                                             !- Coefficient6 x*y",
        "  12.77778,                                                !- Minimum Value of x",
        "  23.88889,                                                !- Maximum Value of x",
        "  18.0,                                                    !- Minimum Value of y",
        "  46.11111;                                                !- Maximum Value of y",

        "Curve:Quadratic,",
        "  Heat Pump 1 Cool Coil Cap-FF,                            !- Name",
        "  0.8,                                                     !- Coefficient1 Constant",
        "  0.2,                                                     !- Coefficient2 x",
        "  0,                                                       !- Coefficient3 x**2",
        "  0.5,                                                     !- Minimum Value of x",
        "  1.5;                                                     !- Maximum Value of x",

        "Curve:Biquadratic,",
        "  Heat Pump 1 Cool Coil EIR-FT,                            !- Name",
        "  0.342414409,                                             !- Coefficient1 Constant",
        "  0.034885008,                                             !- Coefficient2 x",
        "  -0.0006237,                                              !- Coefficient3 x**2",
        "  0.004977216,                                             !- Coefficient4 y",
        "  0.000437951,                                             !- Coefficient5 y**2",
        "  -0.000728028,                                            !- Coefficient6 x*y",
        "  12.77778,                                                !- Minimum Value of x",
        "  23.88889,                                                !- Maximum Value of x",
        "  18.0,                                                    !- Minimum Value of y",
        "  46.11111;                                                !- Maximum Value of y",

        "Curve:Quadratic,",
        "  Heat Pump 1 Cool Coil EIR-FF,                            !- Name",
        "  1.1552,                                                  !- Coefficient1 Constant",
        "  -0.1808,                                                 !- Coefficient2 x",
        "  0.0256,                                                  !- Coefficient3 x**2",
        "  0.5,                                                     !- Minimum Value of x",
        "  1.5;                                                     !- Maximum Value of x",

        "Curve:Quadratic,",
        "  Heat Pump 1 Cool Coil PLF,                               !- Name",
        "  0.85,                                                    !- Coefficient1 Constant",
        "  0.15,                                                    !- Coefficient2 x",
        "  0,                                                       !- Coefficient3 x**2",
        "  0,                                                       !- Minimum Value of x",
        "  1;                                                       !- Maximum Value of x",

        "OutdoorAir:Node,",
        "  Heat Pump 1 Cooling Coil Condenser Inlet,                !- Name",
        "  -1;                                                      !- Height Above Ground",

        "Fan:OnOff,",
        "  Heat Pump 1 Supply Fan,                                  !- Name",
        "  FanAndCoilAvailSched,                                           !- Availability Schedule Name",
        "  0.7,                                                     !- Fan Efficiency",
        "  600,                                                     !- Pressure Rise {Pa}",
        "  0.5,                                                !- Maximum Flow Rate {m3/s}",
        "  0.9,                                                     !- Motor Efficiency",
        "  1,                                                       !- Motor in Airstream Fraction",
        "  Heat Pump 1 Mixed Air Outlet,                            !- Air Inlet Node Name",
        "  Heat Pump 1 Supply Fan Outlet;                           !- Air Outlet Node Name",

        "OutdoorAir:NodeList,",
        "  Heat Pump 1 Outside Air Inlet;                           !- Node or NodeList Name 1",

        "AirLoopHVAC:OutdoorAirSystem,",
        "  Heat Pump 1 OA System,                                   !- Name",
        "  Heat Pump 1 OA System Controllers,                       !- Controller List Name",
        "  Heat Pump 1 OA System Equipment,                         !- Outdoor Air Equipment List Name",
        "  Heat Pump 1 Availability Managers;                       !- Availability Manager List Name",

        "AirLoopHVAC:ControllerList,",
        "  Heat Pump 1 OA System Controllers,                       !- Name",
        "  Controller:OutdoorAir,                                   !- Controller Object Type",
        "  Heat Pump 1 OA Controller;                               !- Controller Name",

        "AirLoopHVAC:OutdoorAirSystem:EquipmentList,",
        "  Heat Pump 1 OA System Equipment,                         !- Name",
        "  OutdoorAir:Mixer,                                        !- Component Object Type",
        "  Heat Pump 1 OA Mixing Box;                               !- Component Name",

        "OutdoorAir:Mixer,",
        "  Heat Pump 1 OA Mixing Box,                               !- Name",
        "  Heat Pump 1 Mixed Air Outlet,                            !- Mixed Air Node Name",
        "  Heat Pump 1 Outside Air Inlet,                           !- Outside Air Stream Node Name",
        "  Heat Pump 1 Relief Air Outlet,                           !- Relief Air Stream Node Name",
        "  Heat Pump 1 Air Loop Inlet;                              !- Return Air Stream Node Name",

        "SetpointManager:SingleZone:Cooling,",
        "  Heat Pump 1 Economizer Supply Air Temp Manager,          !- Name",
        "  Temperature,                                             !- Control Variable",
        "  13,                                                      !- minimum supply air temperature {C}",
        "  45,                                                      !- maximum supply air temperature {C}",
        "  NORTH ZONE,                                              !- Control Zone Name",
        "  NORTH ZONE Zone Air Node,                                !- Zone Node Name",
        "  NORTH ZONE Zone Equip Inlet,                             !- Zone Inlet Node Name",
        "  Heat Pump 1 Air Loop Outlet;                             !- Setpoint Node or NodeList Name",

        "SetpointManager:MixedAir,",
        "  Heat Pump 1 Cooling Coil Air Temp Manager,               !- Name",
        "  Temperature,                                             !- Control Variable",
        "  Heat Pump 1 Air Loop Outlet,                             !- Reference Setpoint Node Name",
        "  Heat Pump 1 Mixed Air Outlet,                            !- Fan Inlet Node Name",
        "  Heat Pump 1 Supply Fan Outlet,                           !- Fan Outlet Node Name",
        "  Heat Pump 1 Mixed Air Outlet;                            !- Setpoint Node or NodeList Name",

        "Controller:OutdoorAir,",
        "  Heat Pump 1 OA Controller,                               !- Name",
        "  Heat Pump 1 Relief Air Outlet,                           !- Relief Air Outlet Node Name",
        "  Heat Pump 1 Air Loop Inlet,                              !- Return Air Node Name",
        "  Heat Pump 1 Mixed Air Outlet,                            !- Mixed Air Node Name",
        "  Heat Pump 1 Outside Air Inlet,                           !- Actuator Node Name",
        "  0.1,                                                !- Minimum Outdoor Air Flow Rate {m3/s}",
        "  0.3,                                                !- Maximum Outdoor Air Flow Rate {m3/s}",
        "  DifferentialDryBulb,                                     !- Economizer Control Type",
        "  ModulateFlow,                                            !- Economizer Control Action Type",
        "  19,                                                      !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
        "  ,                                                        !- Economizer Maximum Limit Enthalpy {J/kg}",
        "  ,                                                        !- Economizer Maximum Limit Dewpoint Temperature (C)",
        "  ,                                                        !- Electronic Enthalpy Limit Curve Name",
        "  ,                                                        !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
        "  NoLockout,                                               !- Lockout Type",
        "  FixedMinimum,                                            !- Minimum Limit Type",
        "  Minimum OA Sch,                                            !- Minimum Outdoor Air Schedule Name",
        "  ,                                                        !- Minimum Fraction of Outdoor Air Schedule Name",
        "  ,                                                        !- Maximum Fraction of Outdoor Air Schedule Name",
        "  ;                                                        !- Mechanical Ventilation Controller Name",

        "Site:GroundTemperature:BuildingSurface,",
        "  20.03,                                                   !- January Ground Temperature",
        "  20.03,                                                   !- February Ground Temperature",
        "  20.13,                                                   !- March Ground Temperature",
        "  20.30,                                                   !- April Ground Temperature",
        "  20.43,                                                   !- May Ground Temperature",
        "  20.52,                                                   !- June Ground Temperature",
        "  20.62,                                                   !- July Ground Temperature",
        "  20.77,                                                   !- August Ground Temperature",
        "  20.78,                                                   !- September Ground Temperature",
        "  20.55,                                                   !- October Ground Temperature",
        "  20.44,                                                   !- November Ground Temperature",
        "  20.20;                                                   !- December Ground Temperature",

        "Output:Diagnostics,DisplayExtraWarnings;",

        "Output:Diagnostics,DisplayUnusedSchedules;"});

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound = false;
    // Read objects
    HeatBalanceManager::GetProjectControlData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetZoneData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetWindowGlassSpectralData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetMaterialData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetConstructData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    SurfaceGeometry::GetGeometryParameters(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    SurfaceGeometry::CosBldgRotAppGonly = 1.0;
    SurfaceGeometry::SinBldgRotAppGonly = 0.0;
    SurfaceGeometry::GetSurfaceData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    // Read AirflowNetwork inputs
    GetAirflowNetworkInput();

    Real64 PresssureSet = 0.5;
    // Assign values
    Schedule(1).CurrentValue = 25.55;         // WindowVentSched
    Schedule(9).CurrentValue = 1.0;           // FanAndCoilAvailSched
    Schedule(14).CurrentValue = 1.0;          // VentingSched
    Schedule(16).CurrentValue = PresssureSet; // Pressure setpoint
    Schedule(17).CurrentValue = 1.0;          // HVACTemplate-Always 1
    Schedule(18).CurrentValue = 0.0;          // HVACTemplate-Always 0

    AirflowNetworkFanActivated = true;
    DataEnvironment::OutDryBulbTemp = -17.29025;
    DataEnvironment::OutHumRat = 0.0008389;
    DataEnvironment::OutBaroPress = 99063.0;
    DataEnvironment::WindSpeed = 4.9;
    DataEnvironment::WindDir = 270.0;

    for (int i = 1; i <= 50; ++i) {
        AirflowNetworkNodeSimu(i).TZ = 23.0;
        AirflowNetworkNodeSimu(i).WZ = 0.0008400;
        if ((i > 4 && i < 10) || i == 32) {
            AirflowNetworkNodeSimu(i).TZ = DataEnvironment::OutDryBulbTempAt(AirflowNetworkNodeData(i).NodeHeight);
            AirflowNetworkNodeSimu(i).WZ = DataEnvironment::OutHumRat;
        }
    }

    // Set up node values
    Node.allocate(17);
    Node(MultizoneCompExhaustFanData(1).InletNode).MassFlowRate = 0.1005046;

    Node(DisSysCompCVFData(1).InletNode).MassFlowRate = 1.40;
    DisSysCompCVFData(1).FlowRate = Node(DisSysCompCVFData(1).InletNode).MassFlowRate;
    Node(DisSysCompCVFData(2).InletNode).MassFlowRate = 0.52;
    DisSysCompCVFData(2).FlowRate = Node(DisSysCompCVFData(2).InletNode).MassFlowRate;

    DisSysCompOutdoorAirData(1).InletNode = 1;
    Node(DisSysCompOutdoorAirData(1).InletNode).MassFlowRate = 0.2795108;
    DisSysCompOutdoorAirData(2).InletNode = 6;
    Node(DisSysCompOutdoorAirData(2).InletNode).MassFlowRate = 0.1095108;

    if (DisSysCompReliefAirData(1).InletNode == 0) {
        DisSysCompReliefAirData(1).OutletNode = 1;
    }

    AirflowNetworkNodeData(3).AirLoopNum = 1;
    AirflowNetworkLinkageData(51).AirLoopNum = 1;
    AirflowNetworkLinkageData(52).AirLoopNum = 1;
    AirflowNetworkLinkageData(66).AirLoopNum = 2;
    AirflowNetworkLinkageData(42).AirLoopNum = 1;
    AirflowNetworkLinkageData(67).AirLoopNum = 2;

    DataAirLoop::AirLoopAFNInfo.allocate(2);
    DataAirLoop::AirLoopAFNInfo(1).LoopFanOperationMode = 0.0;
    DataAirLoop::AirLoopAFNInfo(2).LoopFanOperationMode = 1.0;
    DataAirLoop::AirLoopAFNInfo(1).LoopOnOffFanPartLoadRatio = 0.0;
    DataAirLoop::AirLoopAFNInfo(2).LoopOnOffFanPartLoadRatio = 1.0;
    DataAirLoop::AirLoopAFNInfo(2).LoopSystemOnMassFlowrate = 0.52;

    CalcAirflowNetworkAirBalance();

    // Check mass flow rate
    EXPECT_NEAR(1.40, AirflowNetworkLinkSimu(42).FLOW, 0.0001);
    EXPECT_NEAR(0.52, AirflowNetworkLinkSimu(67).FLOW, 0.0001);
    EXPECT_NEAR(0.2795108, AirflowNetworkLinkSimu(51).FLOW, 0.0001);
    EXPECT_NEAR(0.1095108, AirflowNetworkLinkSimu(66).FLOW, 0.0001);
    EXPECT_NEAR(0.1005046, AirflowNetworkLinkSimu(15).FLOW, 0.0001);

    AirflowNetworkFanActivated = false;

}

TEST_F(EnergyPlusFixture, AFN_CheckNumOfFansInAirLoopTest)
{
    DataAirSystems::PrimaryAirSystem.allocate(1);
    DataAirSystems::PrimaryAirSystem(1).NumBranches = 1;
    DataAirSystems::PrimaryAirSystem(1).Branch.allocate(1);
    DataAirSystems::PrimaryAirSystem(1).Branch(1).TotalComponents = 3;
    DataAirSystems::PrimaryAirSystem(1).Branch(1).Comp.allocate(3);
    DataAirSystems::PrimaryAirSystem(1).Branch(1).Comp(1).TypeOf = "Fan:ConstantVolume";
    DataAirSystems::PrimaryAirSystem(1).Branch(1).Comp(2).TypeOf = "Fan:VariableVolume";
    DataAirSystems::PrimaryAirSystem(1).Branch(1).Comp(1).Name = "CVF";
    DataAirSystems::PrimaryAirSystem(1).Branch(1).Comp(2).Name = "VAV";

    ASSERT_THROW(ValidateDistributionSystem(), std::runtime_error);

    std::string const error_string = delimited_string({
        "   ** Severe  ** ValidateDistributionSystem: An AirLoop branch, , has two or more fans: CVF,VAV",
        "   **   ~~~   ** The AirflowNetwork model allows a single supply fan in an AirLoop only. Please make "
        "changes in the input file accordingly.",
        "   **  Fatal  ** ValidateDistributionSystem: Program terminates for preceding reason(s).",
        "   ...Summary of Errors that led to program termination:",
        "   ..... Reference severe error count=1",
        "   ..... Last severe error=ValidateDistributionSystem: An AirLoop branch, , has two or more fans: CVF,VAV",
        });

    EXPECT_TRUE(compare_err_stream(error_string, true));

}

TEST_F(EnergyPlusFixture, BasicAdvancedSingleSidedAvoidCrashTest)
{
    std::string const idf_objects = delimited_string(
        { "Version,9.0;",
        "SimulationControl,",
        "  No,                      !- Do Zone Sizing Calculation",
        "  No,                      !- Do System Sizing Calculation",
        "  No,                      !- Do Plant Sizing Calculation",
        "  No,                      !- Run Simulation for Sizing Periods",
        "  Yes;                     !- Run Simulation for Weather File Run Periods",
        "Building,",
        "  Single Sided Demo Building,  !- Name",
        "  0.0,                     !- North Axis {deg}",
        "  Suburbs,                 !- Terrain",
        "  0.04,                    !- Loads Convergence Tolerance Value",
        "  0.4,                     !- Temperature Convergence Tolerance Value {deltaC}",
        "  FullInteriorAndExterior, !- Solar Distribution",
        "  25,                      !- Maximum Number of Warmup Days",
        "  ;                        !- Minimum Number of Warmup Days",
        "Timestep,4;",
        "Site:Location,",
        "  San Francisco Intl Ap_CA_USA Design_Conditions,  !- Name",
        "  37.62,                   !- Latitude {deg}",
        "  -122.40,                 !- Longitude {deg}",
        "  -8.00,                   !- Time Zone {hr}",
        "  2.00;                    !- Elevation {m}",
        "Site:GroundTemperature:BuildingSurface,19.905,19.922,19.910,19.932,19.949,20.038,20.327,20.062,20.443,20.088,19.986,19.948;",
        "RunPeriod,",
        "  RunPeriod1,              !- Name",
        "  1,                       !- Begin Month",
        "  1,                       !- Begin Day of Month",
         " ,                        !- Begin Year",
        "  12,                      !- End Month",
        "  31,                      !- End Day of Month",
         " ,                        !- End Year",
        "  Sunday,                  !- Day of Week for Start Day",
        "  Yes,                     !- Use Weather File Holidays and Special Days",
        "  Yes,                     !- Use Weather File Daylight Saving Period",
        "  No,                      !- Apply Weekend Holiday Rule",
        "  Yes,                     !- Use Weather File Rain Indicators",
        "  Yes;                     !- Use Weather File Snow Indicators",
        "Material,",
        "  F08 Metal surface,       !- Name",
        "  Smooth,                  !- Roughness",
        "  0.0008,                  !- Thickness {m}",
        "  45.28,                   !- Conductivity {W/m-K}",
        "  7824,                    !- Density {kg/m3}",
        "  500;                     !- Specific Heat {J/kg-K}",
        "Material,",
        "  I01 25mm insulation board,  !- Name",
        "  MediumRough,             !- Roughness",
        "  0.0254,                  !- Thickness {m}",
        "  0.03,                    !- Conductivity {W/m-K}",
        "  43,                      !- Density {kg/m3}",
        "  1210;                    !- Specific Heat {J/kg-K}",
        "Material,",
        "  I02 50mm insulation board,  !- Name",
        "  MediumRough,             !- Roughness",
        "  0.0508,                  !- Thickness {m}",
        "  0.03,                    !- Conductivity {W/m-K}",
        "  43,                      !- Density {kg/m3}",
        "  1210;                    !- Specific Heat {J/kg-K}",
        "Material,",
        "  G01a 19mm gypsum board,  !- Name",
        "  MediumSmooth,            !- Roughness",
        "  0.019,                   !- Thickness {m}",
        "  0.16,                    !- Conductivity {W/m-K}",
        "  800,                     !- Density {kg/m3}",
        "  1090;                    !- Specific Heat {J/kg-K}",
        "Material,",
        "  M11 100mm lightweight concrete,  !- Name",
        "  MediumRough,             !- Roughness",
        "  0.1016,                  !- Thickness {m}",
        "  0.53,                    !- Conductivity {W/m-K}",
        "  1280,                    !- Density {kg/m3}",
        "  840;                     !- Specific Heat {J/kg-K}",
        "Material,",
        "  F16 Acoustic tile,       !- Name",
        "  MediumSmooth,            !- Roughness",
        "  0.0191,                  !- Thickness {m}",
        "  0.06,                    !- Conductivity {W/m-K}",
        "  368,                     !- Density {kg/m3}",
        "  590;                     !- Specific Heat {J/kg-K}",
        "Material,",
        "  M01 100mm brick,         !- Name",
        "  MediumRough,             !- Roughness",
        "  0.1016,                  !- Thickness {m}",
        "  0.89,                    !- Conductivity {W/m-K}",
        "  1920,                    !- Density {kg/m3}",
        "  790;                     !- Specific Heat {J/kg-K}",
        "Material,",
        "  M15 200mm heavyweight concrete,  !- Name",
        "  MediumRough,             !- Roughness",
        "  0.2032,                  !- Thickness {m}",
        "  1.95,                    !- Conductivity {W/m-K}",
        "  2240,                    !- Density {kg/m3}",
        "  900;                     !- Specific Heat {J/kg-K}",
        "Material,",
        "  M05 200mm concrete block,!- Name",
        "  MediumRough,             !- Roughness",
        "  0.2032,                  !- Thickness {m}",
        "  1.11,                    !- Conductivity {W/m-K}",
        "  800,                     !- Density {kg/m3}",
        "  920;                     !- Specific Heat {J/kg-K}",
        "Material,",
        "  G05 25mm wood,           !- Name",
        "  MediumSmooth,            !- Roughness",
        "  0.0254,                  !- Thickness {m}",
        "  0.15,                    !- Conductivity {W/m-K}",
        "  608,                     !- Density {kg/m3}",
        "  1630;                    !- Specific Heat {J/kg-K}",
        "Material:AirGap,",
        "  F04 Wall air space resistance,  !- Name",
        "  0.15;                    !- Thermal Resistance {m2-K/W}",
        "Material:AirGap,",
        "  F05 Ceiling air space resistance,  !- Name",
        "  0.18;                    !- Thermal Resistance {m2-K/W}",
        "WindowMaterial:Glazing,",
        "  Clear 3mm,               !- Name",
        "  SpectralAverage,         !- Optical Data Type",
        "  ,                        !- Window Glass Spectral Data Set Name",
        "  0.003,                   !- Thickness {m}",
        "  0.837,                   !- Solar Transmittance at Normal Incidence",
        "  0.075,                   !- Front Side Solar Reflectance at Normal Incidence",
        "  0.075,                   !- Back Side Solar Reflectance at Normal Incidence",
        "  0.898,                   !- Visible Transmittance at Normal Incidence",
        "  0.081,                   !- Front Side Visible Reflectance at Normal Incidence",
        "  0.081,                   !- Back Side Visible Reflectance at Normal Incidence",
        "  0,                       !- Infrared Transmittance at Normal Incidence",
        "  0.84,                    !- Front Side Infrared Hemispherical Emissivity",
        "  0.84,                    !- Back Side Infrared Hemispherical Emissivity",
        "  0.9;                     !- Conductivity {W/m-K}",
        "WindowMaterial:Gas,",
        "  Air 13mm,                !- Name",
        "  Air,                     !- Gas Type",
        "  0.0127;                  !- Thickness {m}",
        "Construction,",
        "  Exterior Floor,          !- Name",
        "  I02 50mm insulation board,  !- Outside Layer",
        "  M15 200mm heavyweight concrete;  !- Layer 2",
        "Construction,",
        "  Exterior Wall,           !- Name",
        "  M01 100mm brick,         !- Outside Layer",
        "  M15 200mm heavyweight concrete,  !- Layer 2",
        "  I02 50mm insulation board,  !- Layer 3",
        "  F04 Wall air space resistance,  !- Layer 4",
        "  G01a 19mm gypsum board;  !- Layer 5",
        "Construction,",
        "  Interior Wall,           !- Name",
        "  G01a 19mm gypsum board,  !- Outside Layer",
        "  F04 Wall air space resistance,  !- Layer 2",
        "  G01a 19mm gypsum board;  !- Layer 3",
        "Construction,",
        "  Exterior Roof,           !- Name",
        "  M11 100mm lightweight concrete,  !- Outside Layer",
        "  F05 Ceiling air space resistance,  !- Layer 2",
        "  F16 Acoustic tile;       !- Layer 3",
        "Construction,",
        "  Exterior Window,         !- Name",
        "  Clear 3mm,               !- Outside Layer",
        "  Air 13mm,                !- Layer 2",
        "  Clear 3mm;               !- Layer 3",
        "GlobalGeometryRules,",
        "  UpperLeftCorner,         !- Starting Vertex Position",
        "  Counterclockwise,        !- Vertex Entry Direction",
        "  World;                   !- Coordinate System",
        "Zone,",
        "  West_Zone,               !- Name",
        "  0.0,                     !- Direction of Relative North {deg}",
        "  0.0,                     !- X Origin {m}",
        "  0.0,                     !- Y Origin {m}",
        "  0.0,                     !- Z Origin {m}",
        "  ,                        !- Type",
        "  1;                       !- Multiplier",
        "BuildingSurface:Detailed,",
        "  West_Zone_Floor,         !- Name",
        "  Floor,                   !- Surface Type",
        "  Exterior Floor,          !- Construction Name",
        "  West_Zone,               !- Zone Name",
        "  Ground,                  !- Outside Boundary Condition",
        "  ,                        !- Outside Boundary Condition Object",
        "  NoSun,                   !- Sun Exposure",
        "  NoWind,                  !- Wind Exposure",
        "  0.0,                     !- View Factor to Ground",
        "  4,                       !- Number of Vertices",
        "  6.096000000000,12.192000000000,0.000000000000,  !- X,Y,Z ==> Vertex 1 {m}",
        "  6.096000000000,0.000000000000,0.000000000000,  !- X,Y,Z ==> Vertex 2 {m}",
        "  0.000000000000,0.000000000000,0.000000000000,  !- X,Y,Z ==> Vertex 3 {m}",
        "  0.000000000000,12.192000000000,0.000000000000;  !- X,Y,Z ==> Vertex 4 {m}",
        "BuildingSurface:Detailed,",
        "  West_Zone_West_Wall,     !- Name",
        "  Wall,                    !- Surface Type",
        "  Exterior Wall,           !- Construction Name",
        "  West_Zone,               !- Zone Name",
        "  Outdoors,                !- Outside Boundary Condition",
        "  ,                        !- Outside Boundary Condition Object",
        "  SunExposed,              !- Sun Exposure",
        "  WindExposed,             !- Wind Exposure",
        "  ,                        !- View Factor to Ground",
        "  4,                       !- Number of Vertices",
        "  0.000000000000,12.192000000000,3.048000000000,  !- X,Y,Z ==> Vertex 1 {m}",
        "  0.000000000000,12.192000000000,0.000000000000,  !- X,Y,Z ==> Vertex 2 {m}",
        "  0.000000000000,0.000000000000,0.000000000000,  !- X,Y,Z ==> Vertex 3 {m}",
        "  0.000000000000,0.000000000000,3.048000000000;  !- X,Y,Z ==> Vertex 4 {m}",
        "BuildingSurface:Detailed,",
        "  West_Zone_South_Wall,    !- Name",
        "  Wall,                    !- Surface Type",
        "  Exterior Wall,           !- Construction Name",
        "  West_Zone,               !- Zone Name",
        "  Outdoors,                !- Outside Boundary Condition",
        "  ,                        !- Outside Boundary Condition Object",
        "  SunExposed,              !- Sun Exposure",
        "  WindExposed,             !- Wind Exposure",
        "  ,                        !- View Factor to Ground",
        "  4,                       !- Number of Vertices",
        "  0.000000000000,0.000000000000,3.048000000000,  !- X,Y,Z ==> Vertex 1 {m}",
        "  0.000000000000,0.000000000000,0.000000000000,  !- X,Y,Z ==> Vertex 2 {m}",
        "  6.096000000000,0.000000000000,0.000000000000,  !- X,Y,Z ==> Vertex 3 {m}",
        "  6.096000000000,0.000000000000,3.048000000000;  !- X,Y,Z ==> Vertex 4 {m}",
        "BuildingSurface:Detailed,",
        "  West_Zone_North_Wall,    !- Name",
        "  Wall,                    !- Surface Type",
        "  Exterior Wall,           !- Construction Name",
        "  West_Zone,               !- Zone Name",
        "  Outdoors,                !- Outside Boundary Condition",
        "  ,                        !- Outside Boundary Condition Object",
        "  SunExposed,              !- Sun Exposure",
        "  WindExposed,             !- Wind Exposure",
        "  ,                        !- View Factor to Ground",
        "  4,                       !- Number of Vertices",
        "  6.096000000000,12.192000000000,3.048000000000,  !- X,Y,Z ==> Vertex 1 {m}",
        "  6.096000000000,12.192000000000,0.000000000000,  !- X,Y,Z ==> Vertex 2 {m}",
        "  0.000000000000,12.192000000000,0.000000000000,  !- X,Y,Z ==> Vertex 3 {m}",
        "  0.000000000000,12.192000000000,3.048000000000;  !- X,Y,Z ==> Vertex 4 {m}",
        "BuildingSurface:Detailed,",
        "  West_Zone_Roof,          !- Name",
        "  Roof,                    !- Surface Type",
        "  Exterior Roof,           !- Construction Name",
        "  West_Zone,               !- Zone Name",
        "  Outdoors,                !- Outside Boundary Condition",
        "  ,                        !- Outside Boundary Condition Object",
        "  SunExposed,              !- Sun Exposure",
        "  WindExposed,             !- Wind Exposure",
        "  ,                        !- View Factor to Ground",
        "  4,                       !- Number of Vertices",
        "  0.000000000000,12.192000000000,3.048000000000,  !- X,Y,Z ==> Vertex 1 {m}",
        "  0.000000000000,0.000000000000,3.048000000000,  !- X,Y,Z ==> Vertex 2 {m}",
        "  6.096000000000,0.000000000000,3.048000000000,  !- X,Y,Z ==> Vertex 3 {m}",
        "  6.096000000000,12.192000000000,3.048000000000;  !- X,Y,Z ==> Vertex 4 {m}",
        "BuildingSurface:Detailed,",
        "  West_Zone_East_Wall_S,   !- Name",
        "  Wall,                    !- Surface Type",
        "  Exterior Wall,           !- Construction Name",
        "  West_Zone,               !- Zone Name",
        "  Outdoors,                !- Outside Boundary Condition",
        "  ,                        !- Outside Boundary Condition Object",
        "  SunExposed,              !- Sun Exposure",
        "  WindExposed,             !- Wind Exposure",
        "  ,                        !- View Factor to Ground",
        "  4,                       !- Number of Vertices",
        "  6.096000000000,0.000000000000,3.048000000000,  !- X,Y,Z ==> Vertex 1 {m}",
        "  6.096000000000,0.000000000000,0.000000000000,  !- X,Y,Z ==> Vertex 2 {m}",
        "  6.096000000000,6.096000000000,0.000000000000,  !- X,Y,Z ==> Vertex 3 {m}",
        "  6.096000000000,6.096000000000,3.048000000000;  !- X,Y,Z ==> Vertex 4 {m}",
        "BuildingSurface:Detailed,",
        "  West_Zone_East_Wall_N,   !- Name",
        "  Wall,                    !- Surface Type",
        "  Exterior Wall,           !- Construction Name",
        "  West_Zone,               !- Zone Name",
        "  Outdoors,                !- Outside Boundary Condition",
        "  ,                        !- Outside Boundary Condition Object",
        "  SunExposed,              !- Sun Exposure",
        "  WindExposed,             !- Wind Exposure",
        "  ,                        !- View Factor to Ground",
        "  4,                       !- Number of Vertices",
        "  6.096000000000,6.096000000000,3.048000000000,  !- X,Y,Z ==> Vertex 1 {m}",
        "  6.096000000000,6.096000000000,0.000000000000,  !- X,Y,Z ==> Vertex 2 {m}",
        "  6.096000000000,12.192000000000,0.000000000000,  !- X,Y,Z ==> Vertex 3 {m}",
        "  6.096000000000,12.192000000000,3.048000000000;  !- X,Y,Z ==> Vertex 4 {m}",
        "FenestrationSurface:Detailed,",
        "  West_Zone_West_Wall_Left_Window,  !- Name",
        "  Window,                  !- Surface Type",
        "  Exterior Window,         !- Construction Name",
        "  West_Zone_West_Wall,     !- Building Surface Name",
        "  ,                        !- Outside Boundary Condition Object",
        "  ,                        !- View Factor to Ground",
        "  ,                        !- Frame and Divider Name",
        "  ,                        !- Multiplier",
        "  4,                       !- Number of Vertices",
        "  0.000000000000,10.668000000000,2.286000000000,  !- X,Y,Z ==> Vertex 1 {m}",
        "  0.000000000000,10.668000000000,0.762000000000,  !- X,Y,Z ==> Vertex 2 {m}",
        "  0.000000000000,9.144000000000,0.762000000000,  !- X,Y,Z ==> Vertex 3 {m}",
        "  0.000000000000,9.144000000000,2.286000000000;  !- X,Y,Z ==> Vertex 4 {m}",
        "FenestrationSurface:Detailed,",
        "  West_Zone_West_Wall_Right_Window,  !- Name",
        "  Window,                  !- Surface Type",
        "  Exterior Window,         !- Construction Name",
        "  West_Zone_West_Wall,     !- Building Surface Name",
        "  ,                        !- Outside Boundary Condition Object",
        "  ,                        !- View Factor to Ground",
        "  ,                        !- Frame and Divider Name",
        "  ,                        !- Multiplier",
        "  4,                       !- Number of Vertices",
        "  0.000000000000,3.048000000000,2.286000000000,  !- X,Y,Z ==> Vertex 1 {m}",
        "  0.000000000000,3.048000000000,0.762000000000,  !- X,Y,Z ==> Vertex 2 {m}",
        "  0.000000000000,1.524000000000,0.762000000000,  !- X,Y,Z ==> Vertex 3 {m}",
        "  0.000000000000,1.524000000000,2.286000000000;  !- X,Y,Z ==> Vertex 4 {m}",
        "AirflowNetwork:SimulationControl,",
        "  AFN_SimulationControl_1, !- Name",
        "  MultizoneWithoutDistribution,  !- AirflowNetwork Control",
        "  SurfaceAverageCalculation,  !- Wind Pressure Coefficient Type",
        "  OpeningHeight,           !- Height Selection for Local Wind Pressure Calculation",
        "  LowRise,                 !- Building Type",
        "  500,                     !- Maximum Number of Iterations {dimensionless}",
        "  ZeroNodePressures,       !- Initialization Type",
        "  1.0E-05,                 !- Relative Airflow Convergence Tolerance {dimensionless}",
        "  1.0E-06,                 !- Absolute Airflow Convergence Tolerance {kg/s}",
        "  -0.5,                    !- Convergence Acceleration Limit {dimensionless}",
        "  90,                      !- Azimuth Angle of Long Axis of Building {deg}",
        "  0.4;                     !- Ratio of Building Width Along Short Axis to Width Along Long Axis",
        "AirflowNetwork:MultiZone:Zone,",
        "  West_Zone,               !- Zone Name",
        "  Constant,                !- Ventilation Control Mode",
        "  ,                        !- Ventilation Control Zone Temperature Setpoint Schedule Name",
        "  ,                        !- Minimum Venting Open Factor {dimensionless}",
        "  ,                        !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor {deltaC}",
        "  ,                        !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor {deltaC}",
        "  ,                        !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor {deltaJ/kg}",
        "  ,                        !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor {deltaJ/kg}",
        "  ,                        !- Venting Availability Schedule Name",
        "  Advanced,                !- Single Sided Wind Pressure Coefficient Algorithm",
        "  12.19;                   !- Facade Width {m}",
        "AirflowNetwork:MultiZone:Surface,",
        "  West_Zone_West_Wall_Right_Window,  !- Surface Name",
        "  SliderWindow,            !- Leakage Component Name",
        "  ,                        !- External Node Name",
        "  1;                       !- Window/Door Opening Factor, or Crack Factor {dimensionless}",
        "AirflowNetwork:MultiZone:Surface,",
        "  West_Zone_West_Wall_Left_Window,  !- Surface Name",
        "  SliderWindow,            !- Leakage Component Name",
        "  ,                        !- External Node Name",
        "  1;                       !- Window/Door Opening Factor, or Crack Factor {dimensionless}",
        "",
        "AirflowNetwork:MultiZone:Surface,",
        "  West_Zone_West_Wall,     !- Surface Name",
        "  WallCrack,               !- Leakage Component Name",
        "  ,                        !- External Node Name",
        "  1;                       !- Window/Door Opening Factor, or Crack Factor {dimensionless}",
        "AirflowNetwork:MultiZone:Component:DetailedOpening,",
        "  SliderWindow,            !- Name",
        "  0.001,                   !- Air Mass Flow Coefficient When Opening is Closed {kg/s-m}",
        "  0.667,                   !- Air Mass Flow Exponent When Opening is Closed {dimensionless}",
        "  NonPivoted,              !- Type of Rectangular Large Vertical Opening (LVO)",
        "  0.0,                     !- Extra Crack Length or Height of Pivoting Axis {m}",
        "  2,                       !- Number of Sets of Opening Factor Data",
        "  0.0,                     !- Opening Factor 1 {dimensionless}",
        "  0.61,                    !- Discharge Coefficient for Opening Factor 1 {dimensionless}",
        "  0.0,                     !- Width Factor for Opening Factor 1 {dimensionless}",
        "  1.0,                     !- Height Factor for Opening Factor 1 {dimensionless}",
        "  0.0,                     !- Start Height Factor for Opening Factor 1 {dimensionless}",
        "  1.0,                     !- Opening Factor 2 {dimensionless}",
        "  0.61,                    !- Discharge Coefficient for Opening Factor 2 {dimensionless}",
        "  0.5,                     !- Width Factor for Opening Factor 2 {dimensionless}",
        "  1.0,                     !- Height Factor for Opening Factor 2 {dimensionless}",
        "  0.0;                     !- Start Height Factor for Opening Factor 2 {dimensionless}",
        "AirflowNetwork:MultiZone:Surface:Crack,",
        "  WallCrack,               !- Name",
        "  0.01,                    !- Air Mass Flow Coefficient at Reference Conditions {kg/s}",
        "  0.667;                   !- Air Mass Flow Exponent {dimensionless}",
        "AirflowNetwork:MultiZone:ReferenceCrackConditions,",
        "  ReferenceCrackConditions,!- Name",
        "  20.0,                    !- Reference Temperature {C}",
        "  101320,                  !- Reference Barometric Pressure {Pa}",
        "  0.005;                   !- Reference Humidity Ratio {kgWater/kgDryAir}" });

    std::vector<Real64> valsForLeftWindow = {
        -1.3130779955194276,  -1.7404152241877022,  -1.9384350312723766,  -1.8112879523426120,   -1.4903484929957291,  -1.1589328567607411,
        -0.90795075070620501, -0.75899946242534944, -0.70518117657458634, -0.73794026769189536,  -0.70518117657458634, -0.75899946242534944,
        -0.90795075070620501, -1.1589328567607411,  -1.4903484929957291,  -1.8112879523426120,   -1.9384350312723766,  -1.7404152241877022,
        -1.3130779955194276,  -0.81787534059945755, -0.32789581427586245, -0.051561623181424314, 0.15922353989407620,  0.38420139813526627,
        0.61892682388527165,  0.85109949645405880,  1.0664888091014251,   1.2510276050004789,    1.0664888091014251,   0.85109949645405880,
        0.61892682388527165,  0.38420139813526627,  0.15922353989407620,  -0.051561623181424314, -0.32789581427586245, -0.81787534059945755,
        -1.3130779955194276 };

    std::vector<Real64> valsForRightWindow = {
        -0.56146269488642231,  -0.81031499432463261,  -0.88587800418632712,  -0.70219756773378639, -0.39543597375365452,  -0.14821874325853215,
        -0.045339946833489957, -0.097330100392452740, -0.28213089764929783,  -0.57310708635195429, -0.28213089764929783,  -0.097330100392452740,
        -0.045339946833489957, -0.14821874325853215,  -0.39543597375365452,  -0.70219756773378639, -0.88587800418632712,  -0.81031499432463261,
        -0.56146269488642231,  -0.28653692388308377,  -0.041152159946210520, 0.37465991281286887,  0.81696925904461237,   1.1829453813575432,
        1.4391966568855996,    1.5699546250680769,    1.5837385005116038,    1.5105973452216215,   1.5837385005116038,    1.5699546250680769,
        1.4391966568855996,    1.1829453813575432,    0.81696925904461237,   0.37465991281286887,  -0.041152159946210520, -0.28653692388308377,
        -0.56146269488642231 };

    ASSERT_TRUE(process_idf(idf_objects));

    bool errors = false;

    HeatBalanceManager::GetMaterialData(errors); // read material data
    EXPECT_FALSE(errors);                        // expect no errors

    HeatBalanceManager::GetConstructData(errors); // read construction data
    EXPECT_FALSE(errors);                         // expect no errors

    HeatBalanceManager::GetZoneData(errors); // read zone data
    EXPECT_FALSE(errors);                    // expect no errors

                                             // Magic to get surfaces read in correctly
    DataHeatBalance::HeatTransferAlgosUsed.allocate(1);
    DataHeatBalance::HeatTransferAlgosUsed(1) = OverallHeatTransferSolutionAlgo;
    SurfaceGeometry::CosBldgRotAppGonly = 1.0;
    SurfaceGeometry::SinBldgRotAppGonly = 0.0;

    SurfaceGeometry::GetSurfaceData(errors); // setup zone geometry and get zone data
    EXPECT_FALSE(errors);                    // expect no errors

    CurveManager::GetCurveInput();
    EXPECT_EQ(0, CurveManager::NumCurves);

    // #6912
    DataHeatBalFanSys::MAT.allocate(1);
    DataHeatBalFanSys::ZoneAirHumRat.allocate(1);
    DataHeatBalFanSys::MAT(1) = 23.0;
    DataHeatBalFanSys::ZoneAirHumRat(1) = 0.001;
    DataEnvironment::OutDryBulbTemp = -17.29025;
    DataEnvironment::OutHumRat = 0.0008389;
    DataEnvironment::OutBaroPress = 99063.0;
    DataEnvironment::WindSpeed = 4.9;
    DataEnvironment::WindDir = 270.0;
    bool First = false;
    int iter = 1;
    bool resimu = false;

    Zone(1).OutDryBulbTemp = DataEnvironment::OutDryBulbTemp;
    AirflowNetworkBalanceManager::GetAirflowNetworkInput();
    AirflowNetworkGetInputFlag = false;
    AirflowNetworkExchangeData.allocate(1);
    ManageAirflowNetworkBalance(First, iter, resimu);
    EXPECT_FALSE(resimu);     

}

} // namespace EnergyPlus
