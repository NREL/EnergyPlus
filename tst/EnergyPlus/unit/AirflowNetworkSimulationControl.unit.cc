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

// EnergyPlus::AirflowNetwork unit tests

// Google test headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <AirflowNetwork/Elements.hpp>
#include <AirflowNetwork/Solver.hpp>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/IOFiles.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/SurfaceGeometry.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace AirflowNetwork;
using namespace DataSurfaces;
using namespace DataHeatBalance;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::ScheduleManager;

namespace EnergyPlus {

TEST_F(EnergyPlusFixture, AirflowNetworkSimulationControl_DefaultSolver)
{

    state->dataHeatBal->Zone.allocate(1);
    state->dataHeatBal->Zone(1).Name = "SOFF";

    state->dataSurface->Surface.allocate(2);
    state->dataSurface->Surface(1).Name = "WINDOW 1";
    state->dataSurface->Surface(1).Zone = 1;
    state->dataSurface->Surface(1).ZoneName = "SOFF";
    state->dataSurface->Surface(1).Azimuth = 0.0;
    state->dataSurface->Surface(1).ExtBoundCond = 0;
    state->dataSurface->Surface(1).HeatTransSurf = true;
    state->dataSurface->Surface(1).Tilt = 90.0;
    state->dataSurface->Surface(1).Sides = 4;
    state->dataSurface->Surface(2).Name = "WINDOW 2";
    state->dataSurface->Surface(2).Zone = 1;
    state->dataSurface->Surface(2).ZoneName = "SOFF";
    state->dataSurface->Surface(2).Azimuth = 180.0;
    state->dataSurface->Surface(2).ExtBoundCond = 0;
    state->dataSurface->Surface(2).HeatTransSurf = true;
    state->dataSurface->Surface(2).Tilt = 90.0;
    state->dataSurface->Surface(2).Sides = 4;

    SurfaceGeometry::AllocateSurfaceWindows(*state, 2);
    state->dataSurface->SurfWinOriginalClass(1) = DataSurfaces::SurfaceClass::Window;
    state->dataSurface->SurfWinOriginalClass(2) = DataSurfaces::SurfaceClass::Window;
    state->dataGlobal->NumOfZones = 1;

    state->dataHeatBal->TotPeople = 1; // Total number of people statements
    state->dataHeatBal->People.allocate(state->dataHeatBal->TotPeople);
    state->dataHeatBal->People(1).ZonePtr = 1;
    state->dataHeatBal->People(1).NumberOfPeople = 100.0;
    state->dataHeatBal->People(1).NumberOfPeoplePtr = 1; // From dataglobals, always returns a 1 for schedule value
    state->dataHeatBal->People(1).AdaptiveCEN15251 = true;

    std::string const idf_objects = delimited_string({
        "Schedule:Constant,OnSch,,1.0;",
        "Schedule:Constant,FreeRunningSeason,,0.0;",
        "Schedule:Constant,Sempre 21,,21.0;",
        "AirflowNetwork:SimulationControl,",
        "  NaturalVentilation,           !- Name",
        "  MultizoneWithoutDistribution, !- AirflowNetwork Control",
        "  SurfaceAverageCalculation,    !- Wind Pressure Coefficient Type",
        "  ,                             !- Height Selection for Local Wind Pressure Calculation",
        "  LOWRISE,                      !- Building Type",
        "  1000,                         !- Maximum Number of Iterations{ dimensionless }",
        "  ZeroNodePressures,            !- Initialization Type",
        "  0.0001,                       !- Relative Airflow Convergence Tolerance{ dimensionless }",
        "  0.0001,                       !- Absolute Airflow Convergence Tolerance{ kg / s }",
        "  -0.5,                         !- Convergence Acceleration Limit{ dimensionless }",
        "  90,                           !- Azimuth Angle of Long Axis of Building{ deg }",
        "  0.36;                         !- Ratio of Building Width Along Short Axis to Width Along Long Axis",
        "AirflowNetwork:MultiZone:Zone,",
        "  Soff,                         !- Zone Name",
        "  CEN15251Adaptive,             !- Ventilation Control Mode",
        "  ,                             !- Ventilation Control Zone Temperature Setpoint Schedule Name",
        "  ,                             !- Minimum Venting Open Factor{ dimensionless }",
        "  ,                             !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor{ deltaC }",
        "  100,                          !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor{ deltaC }",
        "  ,                             !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor{ deltaJ / kg }",
        "  300000,                       !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor{ deltaJ / kg }",
        "  FreeRunningSeason;            !- Venting Availability Schedule Name",
        "AirflowNetwork:MultiZone:Surface,",
        "  window 1,                     !- Surface Name",
        "  Simple Window,                !- Leakage Component Name",
        "  ,                             !- External Node Name",
        "  1,                            !- Window / Door Opening Factor, or Crack Factor{ dimensionless }",
        "  ZoneLevel;                    !- Ventilation Control Mode",
        "AirflowNetwork:MultiZone:Surface,",
        "  window 2,                     !- Surface Name",
        "  Simple Window,                !- Leakage Component Name",
        "  ,                             !- External Node Name",
        "  1,                            !- Window / Door Opening Factor, or Crack Factor{ dimensionless }",
        "  ZoneLevel;                    !- Ventilation Control Mode",
        "AirflowNetwork:MultiZone:Component:SimpleOpening,",
        "  Simple Window,                !- Name",
        "  0.0010,                       !- Air Mass Flow Coefficient When Opening is Closed{ kg / s - m }",
        "  0.65,                         !- Air Mass Flow Exponent When Opening is Closed{ dimensionless }",
        "  0.01,                         !- Minimum Density Difference for Two - Way Flow{ kg / m3 }",
        "  0.78;                         !- Discharge Coefficient{ dimensionless }",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    state->afn->get_input();

    EXPECT_TRUE(compare_enums(AirflowNetwork::AirflowNetworkSimuProp::Solver::SkylineLU, state->afn->AirflowNetworkSimu.solver));

    state->dataHeatBal->Zone.deallocate();
    state->dataSurface->Surface.deallocate();
    state->dataHeatBal->People.deallocate();
}

TEST_F(EnergyPlusFixture, AirflowNetworkSimulationControl_SetSolver)
{

    state->dataHeatBal->Zone.allocate(1);
    state->dataHeatBal->Zone(1).Name = "SOFF";

    state->dataSurface->Surface.allocate(2);
    state->dataSurface->Surface(1).Name = "WINDOW 1";
    state->dataSurface->Surface(1).Zone = 1;
    state->dataSurface->Surface(1).ZoneName = "SOFF";
    state->dataSurface->Surface(1).Azimuth = 0.0;
    state->dataSurface->Surface(1).ExtBoundCond = 0;
    state->dataSurface->Surface(1).HeatTransSurf = true;
    state->dataSurface->Surface(1).Tilt = 90.0;
    state->dataSurface->Surface(1).Sides = 4;
    state->dataSurface->Surface(2).Name = "WINDOW 2";
    state->dataSurface->Surface(2).Zone = 1;
    state->dataSurface->Surface(2).ZoneName = "SOFF";
    state->dataSurface->Surface(2).Azimuth = 180.0;
    state->dataSurface->Surface(2).ExtBoundCond = 0;
    state->dataSurface->Surface(2).HeatTransSurf = true;
    state->dataSurface->Surface(2).Tilt = 90.0;
    state->dataSurface->Surface(2).Sides = 4;

    SurfaceGeometry::AllocateSurfaceWindows(*state, 2);
    state->dataSurface->SurfWinOriginalClass(1) = DataSurfaces::SurfaceClass::Window;
    ;
    state->dataSurface->SurfWinOriginalClass(2) = DataSurfaces::SurfaceClass::Window;
    ;
    state->dataGlobal->NumOfZones = 1;

    state->dataHeatBal->TotPeople = 1; // Total number of people statements
    state->dataHeatBal->People.allocate(state->dataHeatBal->TotPeople);
    state->dataHeatBal->People(1).ZonePtr = 1;
    state->dataHeatBal->People(1).NumberOfPeople = 100.0;
    state->dataHeatBal->People(1).NumberOfPeoplePtr = 1; // From dataglobals, always returns a 1 for schedule value
    state->dataHeatBal->People(1).AdaptiveCEN15251 = true;

    std::string const idf_objects = delimited_string({
        "Schedule:Constant,OnSch,,1.0;",
        "Schedule:Constant,FreeRunningSeason,,0.0;",
        "Schedule:Constant,Sempre 21,,21.0;",
        "AirflowNetwork:SimulationControl,",
        "  NaturalVentilation,           !- Name",
        "  MultizoneWithoutDistribution, !- AirflowNetwork Control",
        "  SurfaceAverageCalculation,    !- Wind Pressure Coefficient Type",
        "  ,                             !- Height Selection for Local Wind Pressure Calculation",
        "  LOWRISE,                      !- Building Type",
        "  1000,                         !- Maximum Number of Iterations{ dimensionless }",
        "  ZeroNodePressures,            !- Initialization Type",
        "  0.0001,                       !- Relative Airflow Convergence Tolerance{ dimensionless }",
        "  0.0001,                       !- Absolute Airflow Convergence Tolerance{ kg / s }",
        "  -0.5,                         !- Convergence Acceleration Limit{ dimensionless }",
        "  90,                           !- Azimuth Angle of Long Axis of Building{ deg }",
        "  1.0,                          !- Ratio of Building Width Along Short Axis to Width Along Long Axis",
        "  No,                           !- Height Dependence of External Node Temperature",
        "  SkylineLU;                    !- Solver",
        "AirflowNetwork:MultiZone:Zone,",
        "  Soff,                         !- Zone Name",
        "  CEN15251Adaptive,             !- Ventilation Control Mode",
        "  ,                             !- Ventilation Control Zone Temperature Setpoint Schedule Name",
        "  ,                             !- Minimum Venting Open Factor{ dimensionless }",
        "  ,                             !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor{ deltaC }",
        "  100,                          !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor{ deltaC }",
        "  ,                             !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor{ deltaJ / kg }",
        "  300000,                       !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor{ deltaJ / kg }",
        "  FreeRunningSeason;            !- Venting Availability Schedule Name",
        "AirflowNetwork:MultiZone:Surface,",
        "  window 1,                     !- Surface Name",
        "  Simple Window,                !- Leakage Component Name",
        "  ,                             !- External Node Name",
        "  1,                            !- Window / Door Opening Factor, or Crack Factor{ dimensionless }",
        "  ZoneLevel;                    !- Ventilation Control Mode",
        "AirflowNetwork:MultiZone:Surface,",
        "  window 2,                     !- Surface Name",
        "  Simple Window,                !- Leakage Component Name",
        "  ,                             !- External Node Name",
        "  1,                            !- Window / Door Opening Factor, or Crack Factor{ dimensionless }",
        "  ZoneLevel;                    !- Ventilation Control Mode",
        "AirflowNetwork:MultiZone:Component:SimpleOpening,",
        "  Simple Window,                !- Name",
        "  0.0010,                       !- Air Mass Flow Coefficient When Opening is Closed{ kg / s - m }",
        "  0.65,                         !- Air Mass Flow Exponent When Opening is Closed{ dimensionless }",
        "  0.01,                         !- Minimum Density Difference for Two - Way Flow{ kg / m3 }",
        "  0.78;                         !- Discharge Coefficient{ dimensionless }",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    state->afn->get_input();

    EXPECT_TRUE(compare_enums(AirflowNetwork::AirflowNetworkSimuProp::Solver::SkylineLU, state->afn->AirflowNetworkSimu.solver));

    state->dataHeatBal->Zone.deallocate();
    state->dataSurface->Surface.deallocate();
    state->dataHeatBal->People.deallocate();
}

} // namespace EnergyPlus
