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

// EnergyPlus::VariableSpeedCoils Unit Tests

// Google Test Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/BranchInputManager.hh>
#include <EnergyPlus/DXCoils.hh>
#include <EnergyPlus/DataDefineEquip.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/HeatingCoils.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/PackagedTerminalHeatPump.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/SingleDuct.hh>
#include <EnergyPlus/SizingManager.hh>
#include <EnergyPlus/VariableSpeedCoils.hh>
#include <EnergyPlus/ZoneAirLoopEquipmentManager.hh>
#include <EnergyPlus/ZoneTempPredictorCorrector.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/gio.hh>
#include <gtest/gtest.h>

using namespace EnergyPlus;
using namespace EnergyPlus::BranchInputManager;
using namespace EnergyPlus::DataDefineEquip;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::DataHeatBalFanSys;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataPlant;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataZoneEnergyDemands;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DXCoils;
using namespace EnergyPlus::Fans;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::HeatingCoils;
using namespace EnergyPlus::PackagedTerminalHeatPump;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::SimulationManager;
using namespace EnergyPlus::SingleDuct;
using namespace EnergyPlus::SizingManager;
using namespace EnergyPlus::VariableSpeedCoils;
using namespace EnergyPlus::ZoneAirLoopEquipmentManager;
using namespace EnergyPlus::ZoneTempPredictorCorrector;
using namespace ObjexxFCL;

namespace EnergyPlus {

TEST_F(EnergyPlusFixture, PackagedTerminalHP_VSCoils_Sizing)
{
    std::string const idf_objects = delimited_string({

        "  Zone, Space, 0.0, 0.0, 0.0, 0.0, 1, 1, 2.4, , autocalculate, , , Yes; ",
        "  ZoneHVAC:EquipmentConnections, Space, Space Eq, Space In Node, Space Out Node, Space Node, Space Ret Node; ",
        "  ZoneHVAC:EquipmentList, Space Eq, SequentialLoad, ZoneHVAC:WaterToAirHeatPump, Zone WSHP, 1, 1; ",
        "  Schedule:Compact, OnSched, Fraction, Through: 12/31, For: AllDays, Until: 24:00, 1.0; ",
        "  ScheduleTypeLimits, Fraction, 0.0, 1.0, CONTINUOUS; ",
        "  OutdoorAir:Node, PSZ-AC_1:5 OA Node;",
        "  OutdoorAir:Node, Lobby_ZN_1_FLR_2 WSHP OA Node;",
        "  Curve:Exponent, FanPowerCurve, 0.254542407, 0.837259009, 3, 0.458, 1, , , Dimensionless, Dimensionless; ",
        "  Curve:Quadratic, PLF Curve, 0.85, 0.15, 0, 0, 1, 0.0, 1.0, Dimensionless, Dimensionless; ",
        "  Curve:Cubic, CubicCurve, 1.0, 0.0, 0.0, 0.0, 0.76, 1.09, , , Dimensionless, Dimensionless; ",
        "  Curve:Biquadratic, BiquadraticCurve, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 10, 25.6, 7.2, 48.9, , , Temperature, Temperature, Dimensionless; ",

        "  ZoneHVAC:WaterToAirHeatPump,",
        "    Zone WSHP,            !- Name",
        "    OnSched,              !- Availability Schedule Name",
        "    Space Out Node,       !- Air Inlet Node Name",
        "    Space In Node,        !- Air Outlet Node Name",
        "    ,                     !- Outdoor Air Mixer Object Type",
        "    ,                     !- Outdoor Air Mixer Name",
        "    Autosize,             !- Supply Air Flow Rate During Cooling Operation {m3/s}",
        "    Autosize,             !- Supply Air Flow Rate During Heating Operation {m3/s}",
        "    ,                     !- Supply Air Flow Rate When No Cooling or Heating is Needed {m3/s}",
        "    0.0,                  !- Outdoor Air Flow Rate During Cooling Operation {m3/s}",
        "    0.0,                  !- Outdoor Air Flow Rate During Heating Operation {m3/s}",
        "    ,                     !- Outdoor Air Flow Rate When No Cooling or Heating is Needed {m3/s}",
        "    Fan:OnOff,            !- Supply Air Fan Object Type",
        "    Lobby_ZN_1_FLR_2 WSHP Fan,                                 !- Supply Air Fan Name",
        "    Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit,  !- Heating Coil Object Type",
        "    Lobby_ZN_1_FLR_2 WSHP Heating Mode,                        !- Heating Coil Name",
        "    Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit,  !- Cooling Coil Object Type",
        "    Lobby_ZN_1_FLR_2 WSHP Cooling Mode,                        !- Cooling Coil Name",
        "    2.5,                  !- Maximum Cycling Rate {cycles/hr}",
        "    60.0,                 !- Heat Pump Time Constant {s}",
        "    0.01,                 !- Fraction of On-Cycle Power Use",
        "    60,                   !- Heat Pump Fan Delay Time {s}",
        "    Coil:Heating:Electric,                    !- Supplemental Heating Coil Object Type",
        "    Lobby_ZN_1_FLR_2 WSHP Supp Heating Coil,  !- Supplemental Heating Coil Name",
        "    50.0,                 !- Maximum Supply Air Temperature from Supplemental Heater {C}",
        "    20.0,                 !- Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation {C}",
        "    Lobby_ZN_1_FLR_2 WSHP OA Node,  !- Outdoor Dry-Bulb Temperature Sensor Node Name",
        "    BlowThrough,          !- Fan Placement",
        "    OnSched,              !- Supply Air Fan Operating Mode Schedule Name",
        "    ,                     !- Heat Pump Coil Water Flow Mode",
        "    ;                     !- Design Specification ZoneHVAC Sizing Object Name",

        "  Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit,",
        "    Lobby_ZN_1_FLR_2 WSHP Cooling Mode,                    !- Name",
        "    Lobby_ZN_1_FLR_2 WSHP Cooling Source Side Inlet Node,  !- Water-to-Refrigerant HX Water Inlet Node Name",
        "    Lobby_ZN_1_FLR_2 WSHP Cooling Source Side Outlet Node, !- Water-to-Refrigerant HX Water Outlet Node Name",
        "    Lobby_ZN_1_FLR_2 WSHP Cooling Coil Air Inlet Node,     !- Indoor Air Inlet Node Name",
        "    Lobby_ZN_1_FLR_2 WSHP Heating Coil Air Inlet Node,     !- Indoor Air Outlet Node Name",
        "    9,                    !- Number of Speeds {dimensionless}",
        "    9,                    !- Nominal Speed Level {dimensionless}",
        "    Autosize,             !- Gross Rated Total Cooling Capacity At Selected Nominal Speed Level {w}",
        "    Autosize,             !- Rated Air Flow Rate At Selected Nominal Speed Level {m3/s}",
        "    Autosize,             !- Rated Water Flow Rate At Selected Nominal Speed Level {m3/s}",
        "    0.0,                  !- Nominal Time for Condensate to Begin Leaving the Coil {s}",
        "    0.0,                  !- Initial Moisture Evaporation Rate Divided by Steady-State AC Latent Capacity {dimensionless}",
        "    0,                    !- Flag for Using Hot Gas Reheat, 0 or 1 {dimensionless}",
        "    PLF Curve,            !- Energy Part Load Fraction Curve Name",
        "    4682.3964854,         !- Speed 1 Reference Unit Gross Rated Total Cooling Capacity {w}",
        "    0.97,                 !- Speed 1 Reference Unit Gross Rated Sensible Heat Ratio {dimensionless}",
        "    8.031554863,          !- Speed 1 Reference Unit Gross Rated Cooling COP {dimensionless}",
        "    0.408706486,          !- Speed 1 Reference Unit Rated Air Flow Rate {m3/s}",
        "    0.0008201726 ,        !- Speed 1 Reference Unit Rated Water Flow Rate {m3/s}",
        "    BiquadraticCurve,     !- Speed 1 Total Cooling Capacity Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 1 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 1 Total Cooling Capacity Function of Water Flow Fraction Curve Name",
        "    BiquadraticCurve,     !- Speed 1 Energy Input Ratio Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 1 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 1 Energy Input Ratio Function of Water Flow Fraction Curve Name",
        "    0.0,                  !- Speed 1 Reference Unit Waste Heat Fraction of Input Power At Rated Conditions {dimensionless}",
        "    BiquadraticCurve,     !- Speed 1 Waste Heat Function of Temperature Curve Name",
        "    5733.6424135,         !- Speed 2 Reference Unit Gross Rated Total Cooling Capacity {w}",
        "    0.96,                 !- Speed 2 Reference Unit Gross Rated Sensible Heat Ratio {dimensionless}",
        "    8.132826118,          !- Speed 2 Reference Unit Gross Rated Cooling COP {dimensionless}",
        "    0.449293966,          !- Speed 2 Reference Unit Rated Air Flow Rate {m3/s}",
        "    0.0008201726 ,        !- Speed 2 Reference Unit Rated Water Flow Rate {m3/s}",
        "    BiquadraticCurve,     !- Speed 2 Total Cooling Capacity Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 2 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 2 Total Cooling Capacity Function of Water Flow Fraction Curve Name",
        "    BiquadraticCurve,     !- Speed 2 Energy Input Ratio Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 2 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 2 Energy Input Ratio Function of Water Flow Fraction Curve Name",
        "    0.0,                  !- Speed 2 Reference Unit Waste Heat Fraction of Input Power At Rated Conditions {dimensionless}",
        "    BiquadraticCurve,     !- Speed 2 Waste Heat Function of Temperature Curve Name",
        "    6783.7160573,         !- Speed 3 Reference Unit Gross Rated Total Cooling Capacity {w}",
        "    0.95,                 !- Speed 3 Reference Unit Gross Rated Sensible Heat Ratio {dimensionless}",
        "    8.133952107,          !- Speed 3 Reference Unit Gross Rated Cooling COP {dimensionless}",
        "    0.489881446,          !- Speed 3 Reference Unit Rated Air Flow Rate {m3/s}",
        "    0.0008201726 ,        !- Speed 3 Reference Unit Rated Water Flow Rate {m3/s}",
        "    BiquadraticCurve,     !- Speed 3 Total Cooling Capacity Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 3 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 3 Total Cooling Capacity Function of Water Flow Fraction Curve Name",
        "    BiquadraticCurve,     !- Speed 3 Energy Input Ratio Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 3 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 3 Energy Input Ratio Function of Water Flow Fraction Curve Name",
        "    0.0,                  !- Speed 3 Reference Unit Waste Heat Fraction of Input Power At Rated Conditions {dimensionless}",
        "    BiquadraticCurve,     !- Speed 3 Waste Heat Function of Temperature Curve Name",
        "    7819.1361476,         !- Speed 4 Reference Unit Gross Rated Total Cooling Capacity {w}",
        "    0.91,                 !- Speed 4 Reference Unit Gross Rated Sensible Heat Ratio {dimensionless}",
        "    8.077619987,          !- Speed 4 Reference Unit Gross Rated Cooling COP {dimensionless}",
        "    0.530468926,          !- Speed 4 Reference Unit Rated Air Flow Rate {m3/s}",
        "    0.0008201726 ,        !- Speed 4 Reference Unit Rated Water Flow Rate {m3/s}",
        "    BiquadraticCurve,     !- Speed 4 Total Cooling Capacity Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 4 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 4 Total Cooling Capacity Function of Water Flow Fraction Curve Name",
        "    BiquadraticCurve,     !- Speed 4 Energy Input Ratio Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 4 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 4 Energy Input Ratio Function of Water Flow Fraction Curve Name",
        "    0.0,                  !- Speed 4 Reference Unit Waste Heat Fraction of Input Power At Rated Conditions {dimensionless}",
        "    BiquadraticCurve,     !- Speed 4 Waste Heat Function of Temperature Curve Name",
        "    8827.8867705,         !- Speed 5 Reference Unit Gross Rated Total Cooling Capacity {w}",
        "    0.871,                !- Speed 5 Reference Unit Gross Rated Sensible Heat Ratio {dimensionless}",
        "    7.974604129,          !- Speed 5 Reference Unit Gross Rated Cooling COP {dimensionless}",
        "    0.571056406,          !- Speed 5 Reference Unit Rated Air Flow Rate {m3/s}",
        "    0.0008201726 ,        !- Speed 5 Reference Unit Rated Water Flow Rate {m3/s}",
        "    BiquadraticCurve,     !- Speed 5 Total Cooling Capacity Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 5 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 5 Total Cooling Capacity Function of Water Flow Fraction Curve Name",
        "    BiquadraticCurve,     !- Speed 5 Energy Input Ratio Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 5 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 5 Energy Input Ratio Function of Water Flow Fraction Curve Name",
        "    0.0,                  !- Speed 5 Reference Unit Waste Heat Fraction of Input Power At Rated Conditions {dimensionless}",
        "    BiquadraticCurve,     !- Speed 5 Waste Heat Function of Temperature Curve Name",
        "    10734.02101,          !- Speed 6 Reference Unit Gross Rated Total Cooling Capacity {w}",
        "    0.816,                !- Speed 6 Reference Unit Gross Rated Sensible Heat Ratio {dimensionless}",
        "    7.661685232,          !- Speed 6 Reference Unit Gross Rated Cooling COP {dimensionless}",
        "    0.652231367,          !- Speed 6 Reference Unit Rated Air Flow Rate {m3/s}",
        "    0.0008201726 ,        !- Speed 6 Reference Unit Rated Water Flow Rate {m3/s}",
        "    BiquadraticCurve,     !- Speed 6 Total Cooling Capacity Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 6 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 6 Total Cooling Capacity Function of Water Flow Fraction Curve Name",
        "    BiquadraticCurve,     !- Speed 6 Energy Input Ratio Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 6 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 6 Energy Input Ratio Function of Water Flow Fraction Curve Name",
        "    0.0,                  !- Speed 6 Reference Unit Waste Heat Fraction of Input Power At Rated Conditions {dimensionless}",
        "    BiquadraticCurve ,    !- Speed 6 Waste Heat Function of Temperature Curve Name",
        "    12454.348191,         !- Speed 7 Reference Unit Gross Rated Total Cooling Capacity {w}",
        "    0.784,                !- Speed 7 Reference Unit Gross Rated Sensible Heat Ratio {dimensionless}",
        "    7.257778666,          !- Speed 7 Reference Unit Gross Rated Cooling COP {dimensionless}",
        "    0.732934379,          !- Speed 7 Reference Unit Rated Air Flow Rate {m3/s}",
        "    0.0008201726 ,        !- Speed 7 Reference Unit Rated Water Flow Rate {m3/s}",
        "    BiquadraticCurve,     !- Speed 7 Total Cooling Capacity Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 7 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 7 Total Cooling Capacity Function of Water Flow Fraction Curve Name",
        "    BiquadraticCurve,     !- Speed 7 Energy Input Ratio Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 7 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 7 Energy Input Ratio Function of Water Flow Fraction Curve Name",
        "    0.0,                  !- Speed 7 Reference Unit Waste Heat Fraction of Input Power At Rated Conditions {dimensionless}",
        "    BiquadraticCurve,     !- Speed 7 Waste Heat Function of Temperature Curve Name",
        "    13963.37113,          !- Speed 8 Reference Unit Gross Rated Total Cooling Capacity {w}",
        "    0.766,                !- Speed 8 Reference Unit Gross Rated Sensible Heat Ratio {dimensionless}",
        "    6.804761759,          !- Speed 8 Reference Unit Gross Rated Cooling COP {dimensionless}",
        "    0.81410934,           !- Speed 8 Reference Unit Rated Air Flow Rate {m3/s}",
        "    0.0008201726 ,        !- Speed 8 Reference Unit Rated Water Flow Rate {m3/s}",
        "    BiquadraticCurve,     !- Speed 8 Total Cooling Capacity Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 8 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 8 Total Cooling Capacity Function of Water Flow Fraction Curve Name",
        "    BiquadraticCurve,     !- Speed 8 Energy Input Ratio Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 8 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 8 Energy Input Ratio Function of Water Flow Fraction Curve Name",
        "    0.0,                  !- Speed 8 Reference Unit Waste Heat Fraction of Input Power At Rated Conditions {dimensionless}",
        "    BiquadraticCurve,     !- Speed 8 Waste Heat Function of Temperature Curve Name",
        "    16092.825525,         !- Speed 9 Reference Unit Gross Rated Total Cooling Capacity {w}",
        "    0.739,                !- Speed 9 Reference Unit Gross Rated Sensible Heat Ratio {dimensionless}",
        "    5.765971166,          !- Speed 9 Reference Unit Gross Rated Cooling COP {dimensionless}",
        "    0.891980668,          !- Speed 9 Reference Unit Rated Air Flow Rate {m3/s}",
        "    0.0008201726 ,        !- Speed 9 Reference Unit Rated Water Flow Rate {m3/s}",
        "    BiquadraticCurve,     !- Speed 9 Total Cooling Capacity Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 9 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 9 Total Cooling Capacity Function of Water Flow Fraction Curve Name",
        "    BiquadraticCurve,     !- Speed 9 Energy Input Ratio Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 9 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 9 Energy Input Ratio Function of Water Flow Fraction Curve Name",
        "    0.0,                  !- Speed 9 Reference Unit Waste Heat Fraction of Input Power At Rated Conditions {dimensionless}",
        "    BiquadraticCurve;     !- Speed 9 Waste Heat Function of Temperature Curve Name",

        "  Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit,",
        "    Lobby_ZN_1_FLR_2 WSHP Heating Mode,                    !- Name",
        "    Lobby_ZN_1_FLR_2 WSHP Heating Source Side Inlet Node,  !- Water-to-Refrigerant HX Water Inlet Node Name",
        "    Lobby_ZN_1_FLR_2 WSHP Heating Source Side Outlet Node, !- Water-to-Refrigerant HX Water Outlet Node Name",
        "    Lobby_ZN_1_FLR_2 WSHP Heating Coil Air Inlet Node,     !- Indoor Air Inlet Node Name",
        "    Lobby_ZN_1_FLR_2 WSHP SuppHeating Coil Air Inlet Node, !- Indoor Air Outlet Node Name",
        "    9,                    !- Number of Speeds {dimensionless}",
        "    9,                    !- Nominal Speed Level {dimensionless}",
        "    autosize,             !- Rated Heating Capacity At Selected Nominal Speed Level {w}",
        "    autosize,             !- Rated Air Flow Rate At Selected Nominal Speed Level {m3/s}",
        "    autosize,             !- Rated Water Flow Rate At Selected Nominal Speed Level {m3/s}",
        "    PLF Curve,            !- Energy Part Load Fraction Curve Name",
        "    6437.5991236,         !- Speed 1 Reference Unit Gross Rated Heating Capacity {w}",
        "    9.965323721,          !- Speed 1 Reference Unit Gross Rated Heating COP {dimensionless}",
        "    0.408706486,          !- Speed 1 Reference Unit Rated Air Flow {m3/s}",
        "    0.0008201726 ,        !- Speed 1 Reference Unit Rated Water Flow Rate {m3/s}",
        "    BiquadraticCurve,     !- Speed 1 Heating Capacity Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 1 Total  Heating Capacity Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 1 Heating Capacity Function of Water Flow Fraction Curve Name",
        "    BiquadraticCurve,     !- Speed 1 Energy Input Ratio Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 1 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 1 Energy Input Ratio Function of Water Flow Fraction Curve Name",
        "    0.0,                  !- Speed 1 Reference Unit Waste Heat Fraction of Input Power At Rated Conditions {dimensionless}",
        "    BiquadraticCurve,     !- Speed 1 Waste Heat Function of Temperature Curve Name",
        "    7521.3759405,         !- Speed 2 Reference Unit Gross Rated Heating Capacity {w}",
        "    9.3549452,            !- Speed 2 Reference Unit Gross Rated Heating COP {dimensionless}",
        "    0.449293966,          !- Speed 2 Reference Unit Rated Air Flow Rate {m3/s}",
        "    0.0008201726 ,        !- Speed 2 Reference Unit Rated Water Flow Rate {m3/s}",
        "    BiquadraticCurve,     !- Speed 2 Heating Capacity Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 2 Total  Heating Capacity Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 2 Heating Capacity Function of Water Flow Fraction Curve Name",
        "    BiquadraticCurve,     !- Speed 2 Energy Input Ratio Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 2 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 2 Energy Input Ratio Function of Water Flow Fraction Curve Name",
        "    0.0,                  !- Speed 2 Reference Unit Waste Heat Fraction of Input Power At Rated Conditions {dimensionless}",
        "    BiquadraticCurve,     !- Speed 2 Waste Heat Function of Temperature Curve Name",
        "    8601.0497624,         !- Speed 3 Reference Unit Gross Rated Heating Capacity {w}",
        "    8.857929724,          !- Speed 3 Reference Unit Gross Rated Heating COP {dimensionless}",
        "    0.489881446,          !- Speed 3 Reference Unit Rated Air Flow Rate {m3/s}",
        "    0.0008201726 ,        !- Speed 3 Reference Unit Rated Water Flow Rate {m3/s}",
        "    BiquadraticCurve,     !- Speed 3 Heating Capacity Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 3 Total  Heating Capacity Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 3 Heating Capacity Function of Water Flow Fraction Curve Name",
        "    BiquadraticCurve,     !- Speed 3 Energy Input Ratio Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 3 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 3 Energy Input Ratio Function of Water Flow Fraction Curve Name",
        "    0.0,                  !- Speed 3 Reference Unit Waste Heat Fraction of Input Power At Rated Conditions {dimensionless}",
        "    BiquadraticCurve,     !- Speed 3 Waste Heat Function of Temperature Curve Name",
        "    9675.1552339,         !- Speed 4 Reference Unit Gross Rated Heating Capacity {w}",
        "    8.442543834,          !- Speed 4 Reference Unit Gross Rated Heating COP {dimensionless}",
        "    0.530468926,          !- Speed 4 Reference Unit Rated Air Flow Rate {m3/s}",
        "    0.0008201726 ,        !- Speed 4 Reference Unit Rated Water Flow Rate {m3/s}",
        "    BiquadraticCurve,     !- Speed 4 Heating Capacity Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 4 Total  Heating Capacity Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 4 Heating Capacity Function of Water Flow Fraction Curve Name",
        "    BiquadraticCurve,     !- Speed 4 Energy Input Ratio Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 4 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 4 Energy Input Ratio Function of Water Flow Fraction Curve Name",
        "    0.0,                  !- Speed 4 Reference Unit Waste Heat Fraction of Input Power At Rated Conditions {dimensionless}",
        "    BiquadraticCurve,     !- Speed 4 Waste Heat Function of Temperature Curve Name",
        "    10743.692355,         !- Speed 5 Reference Unit Gross Rated Heating Capacity {w}",
        "    8.090129785,          !- Speed 5 Reference Unit Gross Rated Heating COP {dimensionless}",
        "    0.571056406,          !- Speed 5 Reference Unit Rated Air Flow Rate {m3/s}",
        "    0.0008201726 ,        !- Speed 5 Reference Unit Rated Water Flow Rate {m3/s}",
        "    BiquadraticCurve,     !- Speed 5 Heating Capacity Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 5 Total  Heating Capacity Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 5 Heating Capacity Function of Water Flow Fraction Curve Name",
        "    BiquadraticCurve,     !- Speed 5 Energy Input Ratio Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 5 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 5 Energy Input Ratio Function of Water Flow Fraction Curve Name",
        "    0.0,                  !- Speed 5 Reference Unit Waste Heat Fraction of Input Power At Rated Conditions {dimensionless}",
        "    BiquadraticCurve,     !- Speed 5 Waste Heat Function of Temperature Curve Name",
        "    12861.716978,         !- Speed 6 Reference Unit Gross Rated Heating Capacity {w}",
        "    7.521471917,          !- Speed 6 Reference Unit Gross Rated Heating COP {dimensionless}",
        "    0.652231367,          !- Speed 6 Reference Unit Rated Air Flow Rate {m3/s}",
        "    0.0008201726 ,        !- Speed 6 Reference Unit Rated Water Flow Rate {m3/s}",
        "    BiquadraticCurve,     !- Speed 6 Heating Capacity Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 6 Total  Heating Capacity Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 6 Heating Capacity Function of Water Flow Fraction Curve Name",
        "    BiquadraticCurve,     !- Speed 6 Energy Input Ratio Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 6 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 6 Energy Input Ratio Function of Water Flow Fraction Curve Name",
        "    0.0,                  !- Speed 6 Reference Unit Waste Heat Fraction of Input Power At Rated Conditions {dimensionless}",
        "    BiquadraticCurve,     !- Speed 6 Waste Heat Function of Temperature Curve Name",
        "    14951.606778,         !- Speed 7 Reference Unit Gross Rated Heating Capacity {w}",
        "    7.072661674,          !- Speed 7 Reference Unit Gross Rated Heating COP {dimensionless}",
        "    0.732934379,          !- Speed 7 Reference Unit Rated Air Flow Rate {m3/s}",
        "    0.0008201726 ,        !- Speed 7 Reference Unit Rated Water Flow Rate {m3/s}",
        "    BiquadraticCurve,     !- Speed 7 Heating Capacity Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 7 Total  Heating Capacity Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 7 Heating Capacity Function of Water Flow Fraction Curve Name",
        "    BiquadraticCurve,     !- Speed 7 Energy Input Ratio Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 7 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 7 Energy Input Ratio Function of Water Flow Fraction Curve Name",
        "    0.0,                  !- Speed 7 Reference Unit Waste Heat Fraction of Input Power At Rated Conditions {dimensionless}",
        "    BiquadraticCurve,     !- Speed 7 Waste Heat Function of Temperature Curve Name",
        "    17011.8964,           !- Speed 8 Reference Unit Gross Rated Heating Capacity {w}",
        "    6.710807258,          !- Speed 8 Reference Unit Gross Rated Heating COP {dimensionless}",
        "    0.81410934,           !- Speed 8 Reference Unit Rated Air Flow Rate {m3/s}",
        "    0.0008201726 ,        !- Speed 8 Reference Unit Rated Water Flow Rate {m3/s}",
        "    BiquadraticCurve,     !- Speed 8 Heating Capacity Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 8 Total  Heating Capacity Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 8 Heating Capacity Function of Water Flow Fraction Curve Name",
        "    BiquadraticCurve,     !- Speed 8 Energy Input Ratio Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 8 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 8 Energy Input Ratio Function of Water Flow Fraction Curve Name",
        "    0.0 ,                 !- Speed 8 Reference Unit Waste Heat Fraction of Input Power At Rated Conditions {dimensionless}",
        "    BiquadraticCurve,     !- Speed 8 Waste Heat Function of Temperature Curve Name",
        "    20894.501936,         !- Speed 9 Reference Unit Gross Rated Heating Capacity {w}",
        "    5.89906887,           !- Speed 9 Reference Unit Gross Rated Heating COP {dimensionless}",
        "    0.891980668,          !- Speed 9 Reference Unit Rated Air Flow Rate {m3/s}",
        "    0.0008201726 ,        !- Speed 9 Reference Unit Rated Water Flow Rate {m3/s}",
        "    BiquadraticCurve,     !- Speed 9 Heating Capacity Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 9 Total  Heating Capacity Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 9 Heating Capacity Function of Water Flow Fraction Curve Name",
        "    BiquadraticCurve,     !- Speed 9 Energy Input Ratio Function of Temperature Curve Name",
        "    CubicCurve,           !- Speed 9 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "    CubicCurve,           !- Speed 9 Energy Input Ratio Function of Water Flow Fraction Curve Name",
        "    0.0,                  !- Speed 9 Reference Unit Waste Heat Fraction of Input Power At Rated Conditions {dimensionless}",
        "    BiquadraticCurve;     !- Speed 9 Waste Heat Function of Temperature Curve Name",

        "  Fan:OnOff,",
        "    Lobby_ZN_1_FLR_2 WSHP Fan,              !- Name",
        "    OnSched,    !- Availability Schedule Name",
        "    0.7,                     !- Fan Total Efficiency",
        "    113,                   !- Pressure Rise {Pa}",
        "    Autosize,                !- Maximum Flow Rate {m3/s}",
        "    0.9,                     !- Motor Efficiency",
        "    1.0,                     !- Motor In Airstream Fraction",
        "    Space Out Node,    !- Air Inlet Node Name",
        "    Lobby_ZN_1_FLR_2 WSHP Cooling Coil Air Inlet Node,  !- Air Outlet Node Name",
        "    FanPowerCurve, !- Fan Efficiency Ratio Function of Speed Ratio Curve Name",
        "    ,",
        "    WSHP;",

        "  Coil:Heating:Electric,",
        "    Lobby_ZN_1_FLR_2 WSHP Supp Heating Coil,  !- Name",
        "    OnSched,    !- Availability Schedule Name",
        "    1.0,                     !- Gas Burner Efficiency",
        "    Autosize,                   !- Nominal Capacity {W}",
        "    Lobby_ZN_1_FLR_2 WSHP SuppHeating Coil Air Inlet Node,  !- Air Inlet Node Name",
        "    Space In Node;       !- Air Outlet Node Name",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound(false);
    GetZoneData(ErrorsFound);
    GetZoneEquipmentData();
    GetPTUnit();

    TotNumLoops = 2;
    PlantLoop.allocate(TotNumLoops);

    for (int l = 1; l <= TotNumLoops; ++l) {
        auto &loop(PlantLoop(l));
        loop.LoopSide.allocate(2);
        auto &loopside(PlantLoop(l).LoopSide(1));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(PlantLoop(l).LoopSide(1).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }
    PlantLoop(2).Name = "ChilledWaterLoop";
    PlantLoop(2).FluidName = "ChilledWater";
    PlantLoop(2).FluidIndex = 1;
    PlantLoop(2).FluidName = "WATER";
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).Name = VarSpeedCoil(1).Name;
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = DataPlant::TypeOf_CoilVSWAHPCoolingEquationFit;
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumIn = VarSpeedCoil(1).WaterInletNodeNum;
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumOut = VarSpeedCoil(1).WaterOutletNodeNum;

    PlantLoop(1).Name = "HotWaterLoop";
    PlantLoop(1).FluidName = "HotWater";
    PlantLoop(1).FluidIndex = 1;
    PlantLoop(1).FluidName = "WATER";
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).Name = VarSpeedCoil(2).Name;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = DataPlant::TypeOf_CoilVSWAHPHeatingEquationFit;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = VarSpeedCoil(2).WaterInletNodeNum;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumOut = VarSpeedCoil(2).WaterOutletNodeNum;

    DataSizing::CurZoneEqNum = 1;
    DataSizing::ZoneSizingRunDone = true;
    DataSizing::FinalZoneSizing.allocate(1);
    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesCoolVolFlow = 1.0;
    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesCoolCoilInTemp = 24.0;
    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesCoolCoilInHumRat = 0.09;
    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).CoolDesTemp = 12.0;
    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).CoolDesHumRat = 0.05;
    DataEnvironment::OutBaroPress = 101325;
    OutputReportPredefined::SetPredefinedTables();
    DataSizing::ZoneEqSizing.allocate(1);
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).SizingMethod.allocate(16);
    SizePTUnit(1);

    // This VS coil is rather quirky. It sizes the capacity based on zone sizing air flow rate.
    // Then uses that capacity to back calculate the air flow needed to keep the reference air flow per capacity ratio constant.
    // For this reason, the parent object would size to an air flow that was different than the child.

    // identify coil
    EXPECT_EQ(VariableSpeedCoils::VarSpeedCoil(1).Name, "LOBBY_ZN_1_FLR_2 WSHP COOLING MODE");

    // expect coil air flow to equal PTUnit cooling air flow
    EXPECT_EQ(VariableSpeedCoils::VarSpeedCoil(1).RatedAirVolFlowRate, PTUnit(1).MaxCoolAirVolFlow);
    EXPECT_EQ(VariableSpeedCoils::VarSpeedCoil(1).MSRatedAirVolFlowRate(9), PTUnit(1).MaxCoolAirVolFlow);

    // expect the ratio of air flow to capacity to be equal to the reference air flow and capacity ratio specified in coil input
    Real64 refAirflowCapacityRatio = 0.891980668 / 16092.825525; // speed 9 reference cooling data
    Real64 sizingAirflowCapacityRatio =
        VariableSpeedCoils::VarSpeedCoil(1).MSRatedAirVolFlowRate(9) / VariableSpeedCoils::VarSpeedCoil(1).MSRatedTotCap(9);
    EXPECT_EQ(refAirflowCapacityRatio, sizingAirflowCapacityRatio);

    // this same ratio should also equal the internal flow per capacity variable used to back calculate operating air flow rate
    EXPECT_EQ(sizingAirflowCapacityRatio, VariableSpeedCoils::VarSpeedCoil(1).MSRatedAirVolFlowPerRatedTotCap(9));

    // identify coil
    EXPECT_EQ(VariableSpeedCoils::VarSpeedCoil(2).Name, "LOBBY_ZN_1_FLR_2 WSHP HEATING MODE");

    // expect coil air flow to equal PTUnit heating air flow
    EXPECT_EQ(VariableSpeedCoils::VarSpeedCoil(2).RatedAirVolFlowRate, PTUnit(1).MaxHeatAirVolFlow);
    EXPECT_EQ(VariableSpeedCoils::VarSpeedCoil(2).MSRatedAirVolFlowRate(9), PTUnit(1).MaxHeatAirVolFlow);

    // expect the ratio of air flow to capacity to equal to the reference air flow and capacity specified in coil input
    refAirflowCapacityRatio = 0.891980668 / 20894.501936; // speed 9 reference heating data
    sizingAirflowCapacityRatio = VariableSpeedCoils::VarSpeedCoil(2).MSRatedAirVolFlowRate(9) / VariableSpeedCoils::VarSpeedCoil(2).MSRatedTotCap(9);
    EXPECT_EQ(refAirflowCapacityRatio, sizingAirflowCapacityRatio);

    // this same ratio should also equal the internal flow per capacity variable used to back calculate operating air flow rate
    EXPECT_EQ(sizingAirflowCapacityRatio, VariableSpeedCoils::VarSpeedCoil(2).MSRatedAirVolFlowPerRatedTotCap(9));

    SizeFan(1);
    // the fan vol flow rate should equal the max of cooling and heating coil flow rates
    Real64 maxCoilAirFlow = max(VariableSpeedCoils::VarSpeedCoil( 1 ).RatedAirVolFlowRate, VariableSpeedCoils::VarSpeedCoil( 2 ).RatedAirVolFlowRate);
    EXPECT_EQ(Fan(1).MaxAirFlowRate, maxCoilAirFlow);
    EXPECT_EQ(Fan(1).MaxAirFlowRate, max(PTUnit(1).MaxCoolAirVolFlow, PTUnit(1).MaxHeatAirVolFlow));

    // Initialize the packaged terminal heat pump
    Real64 OnOffAirFlowRatio(1.0); // ratio of compressor ON airflow to average airflow over timestep
    Real64 ZoneLoad(0.0);          // cooling or heating needed by zone [watts]

    InitPTUnit(1, DataSizing::CurZoneEqNum, true, OnOffAirFlowRatio, ZoneLoad);

    // check that an intermediate speed has the correct flow ratio
    Real64 refAirflowRatio = 0.530468926 / 0.891980668; // speed 4 reference cooling data and full flow rate at speed 9
    Real64 expectedAirFlowRate = refAirflowRatio * PTUnit(1).MaxCoolAirVolFlow;
    EXPECT_NEAR(expectedAirFlowRate, PTUnit(1).CoolVolumeFlowRate(4), 0.00000001);
    EXPECT_NEAR(expectedAirFlowRate, 3.939704195, 0.00000001);

    refAirflowRatio = 0.530468926 / 0.891980668; // speed 4 reference heating data and full flow rate at speed 9
    expectedAirFlowRate = refAirflowRatio * PTUnit(1).MaxHeatAirVolFlow;
    EXPECT_NEAR(expectedAirFlowRate, PTUnit(1).HeatVolumeFlowRate(4), 0.00001);
    EXPECT_NEAR(expectedAirFlowRate, 3.034337569, 0.00000001);

    // #6028 child components not sizing correctly on air flow rate
    // VS coils set SystemAirFlow to true and AirVolFlow to a value, all PTUnits set CoolingAirFlow and HeatingAirFlow, and CoolingAirVolFlow and
    // HeatingAirVolFlow
    EXPECT_TRUE(ZoneEqSizing(1).SystemAirFlow);
    EXPECT_EQ(ZoneEqSizing(1).AirVolFlow, VariableSpeedCoils::VarSpeedCoil(1).RatedAirVolFlowRate);
    EXPECT_TRUE(ZoneEqSizing(1).CoolingAirFlow);
    EXPECT_TRUE(ZoneEqSizing(1).HeatingAirFlow);
    EXPECT_EQ(ZoneEqSizing(1).CoolingAirVolFlow, PTUnit(1).MaxCoolAirVolFlow);
    EXPECT_EQ(ZoneEqSizing(1).HeatingAirVolFlow, PTUnit(1).MaxHeatAirVolFlow);
    EXPECT_EQ(Fan(1).MaxAirFlowRate, ZoneEqSizing(1).AirVolFlow);
    EXPECT_EQ(Fan(1).MaxAirFlowRate, max(ZoneEqSizing(1).CoolingAirVolFlow, ZoneEqSizing(1).HeatingAirVolFlow));
}

TEST_F(EnergyPlusFixture, AirTerminalSingleDuctMixer_SimPTAC_HeatingCoilTest)
{

    bool ErrorsFound(false);
    bool FirstHVACIteration(false);
    Real64 HVACInletMassFlowRate(0.0);
    Real64 PrimaryAirMassFlowRate(0.0);
    Real64 OnOffAirFlowRatio(1.0);
    Real64 LatOutputProvided(0.0);
    Real64 QUnitOut(0.0);
    Real64 QZnReq(0.0);
    int ZoneNum(1);
    int PTUnitNum(1);

    std::string const idf_objects = delimited_string({
        "Version,9.0;",

        "Schedule:Compact,",
        "    FanAvailSched,           !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,            !- Field 16",
        "    1.0;                     !- Field 17",

        "Schedule:Compact,",
        "    ContinuousFanSch,        !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,            !- Field 3",
        "    1.0;                     !- Field 4",

        "ZoneHVAC:EquipmentList,",
        "    SPACE1-1 Equipment,      !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:PackagedTerminalAirConditioner,  !- Zone Equipment 1 Object Type",
        "    SPACE1-1 PTAC,           !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",

        "  ZoneHVAC:PackagedTerminalAirConditioner,",
        "    SPACE1-1 PTAC,           !- Name",
        "    FanAvailSched,           !- Availability Schedule Name",
        "    SPACE1-1 HP Inlet Node,  !- Air Inlet Node Name",
        "    SPACE1-1 Supply Inlet,   !- Air Outlet Node Name",
        "    OutdoorAir:Mixer,        !- Outdoor Air Mixer Object Type",
        "    PTACOAMixer,             !- Outdoor Air Mixer Name",
        "    0.500,                   !- Supply Air Flow Rate During Cooling Operation {m3/s}",
        "    0.500,                   !- Supply Air Flow Rate During Heating Operation {m3/s}",
        "    ,                        !- Supply Air Flow Rate When No Cooling or Heating is Needed {m3/s}",
        "    0.200,                   !- Outdoor Air Flow Rate During Cooling Operation {m3/s}",
        "    0.200,                   !- Outdoor Air Flow Rate During Heating Operation {m3/s}",
        "    0.200,                   !- Outdoor Air Flow Rate When No Cooling or Heating is Needed {m3/s}",
        "    Fan:ConstantVolume,      !- Supply Air Fan Object Type",
        "    SPACE1-1 Supply Fan,     !- Supply Air Fan Name",
        "    Coil:Heating:Fuel,       !- Heating Coil Object Type",
        "    SPACE1-1 Heating Coil,   !- Heating Coil Name",
        "    Coil:Cooling:DX:SingleSpeed,  !- Cooling Coil Object Type",
        "    SPACE1-1 PTAC CCoil,     !- Cooling Coil Name",
        "    BlowThrough,             !- Fan Placement",
        "    ContinuousFanSch;        !- Supply Air Fan Operating Mode Schedule Name",

        "  OutdoorAir:Mixer,",
        "	 PTACOAMixer,             !- Name",
        "	 PTACOAMixerOutletNode,   !- Mixed Air Node Name",
        "    PTACOAInNode,            !- Outdoor Air Stream Node Name",
        "    ZoneExhausts,            !- Relief Air Stream Node Name",
        "    SPACE1-1 HP Inlet Node;  !- Return Air Stream Node Name",

        "Fan:ConstantVolume,",
        "    SPACE1-1 Supply Fan,     !- Name",
        "    FanAvailSched,           !- Availability Schedule Name",
        "    0.7,                     !- Fan Total Efficiency",
        "    75,                      !- Pressure Rise {Pa}",
        "    0.500,                   !- Maximum Flow Rate {m3/s}",
        "    0.9,                     !- Motor Efficiency",
        "    1,                       !- Motor In Airstream Fraction",
        "    PTACOAMixerOutletNode,   !- Air Inlet Node Name",
        "    SPACE1-1 Fan Outlet Node;!- Air Outlet Node Name",

        "Coil:Heating:Fuel,",
        "    SPACE1-1 Heating Coil,   !- Name",
        "    FanAvailSched,           !- Availability Schedule Name",
        "    Gas,                     !- Fuel Type",
        "    0.8,                     !- Gas Burner Efficiency",
        "    10000.0,                 !- Nominal Capacity {W}",
        "    SPACE1-1 CCoil Outlet Node,  !- Air Inlet Node Name",
        "    SPACE1-1 Supply Inlet;   !- Air Outlet Node Name",

        "  Coil:Cooling:DX:SingleSpeed,",
        "    SPACE1-1 PTAC CCoil,     !- Name",
        "    FanAvailSched,           !- Availability Schedule Name",
        "    6680.0,                  !- Gross Rated Total Cooling Capacity {W}",
        "    0.75,                    !- Gross Rated Sensible Heat Ratio",
        "    3.0,                     !- Gross Rated Cooling COP {W/W}",
        "    0.500,                   !- Rated Air Flow Rate {m3/s}",
        "    ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "    SPACE1-1 Fan Outlet Node,!- Air Inlet Node Name",
        "    SPACE1-1 CCoil Outlet Node,  !- Air Outlet Node Name",
        "    HPACCoolCapFT,           !- Total Cooling Capacity Function of Temperature Curve Name",
        "    HPACCoolCapFFF,          !- Total Cooling Capacity Function of Flow Fraction Curve Name",
        "    HPACEIRFT,               !- Energy Input Ratio Function of Temperature Curve Name",
        "    HPACEIRFFF,              !- Energy Input Ratio Function of Flow Fraction Curve Name",
        "    HPACPLFFPLR;             !- Part Load Fraction Correlation Curve Name",

        "  Curve:Quadratic,",
        "    HPACCoolCapFFF,          !- Name",
        "    0.8,                     !- Coefficient1 Constant",
        "    0.2,                     !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.5,                     !- Minimum Value of x",
        "    1.5;                     !- Maximum Value of x",

        "  Curve:Quadratic,",
        "    HPACEIRFFF,              !- Name",
        "    1.1552,                  !- Coefficient1 Constant",
        "    -0.1808,                 !- Coefficient2 x",
        "    0.0256,                  !- Coefficient3 x**2",
        "    0.5,                     !- Minimum Value of x",
        "    1.5;                     !- Maximum Value of x",

        "  Curve:Quadratic,",
        "    HPACPLFFPLR,             !- Name",
        "    0.85,                    !- Coefficient1 Constant",
        "    0.15,                    !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.0,                     !- Minimum Value of x",
        "    1.0;                     !- Maximum Value of x",

        "  Curve:Cubic,",
        "    FanEffRatioCurve,        !- Name",
        "    0.33856828,              !- Coefficient1 Constant",
        "    1.72644131,              !- Coefficient2 x",
        "    -1.49280132,             !- Coefficient3 x**2",
        "    0.42776208,              !- Coefficient4 x**3",
        "    0.5,                     !- Minimum Value of x",
        "    1.5,                     !- Maximum Value of x",
        "    0.3,                     !- Minimum Curve Output",
        "    1.0;                     !- Maximum Curve Output",

        "  Curve:Exponent,",
        "    FanPowerRatioCurve,      !- Name",
        "    0.0,                     !- Coefficient1 Constant",
        "    1.0,                     !- Coefficient2 Constant",
        "    3.0,                     !- Coefficient3 Constant",
        "    0.0,                     !- Minimum Value of x",
        "    1.5,                     !- Maximum Value of x",
        "    0.01,                    !- Minimum Curve Output",
        "    1.5;                     !- Maximum Curve Output",

        "  Curve:Biquadratic,",
        "    HPACCoolCapFT,           !- Name",
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
        "    HPACEIRFT,               !- Name",
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

        "Zone,",
        "    SPACE1-1,                !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    2.438400269,             !- Ceiling Height {m}",
        "    239.247360229;           !- Volume {m3}",

        "ZoneHVAC:EquipmentConnections,",
        "    SPACE1-1,                !- Zone Name",
        "    SPACE1-1 Equipment,      !- Zone Conditioning Equipment List Name",
        "    SPACE1-1 Inlets,         !- Zone Air Inlet Node or NodeList Name",
        "    SPACE1-1 Exhausts,       !- Zone Air Exhaust Node or NodeList Name",
        "    SPACE1-1 Zone Air Node,  !- Zone Air Node Name",
        "    SPACE1-1 Return Outlet;  !- Zone Return Air Node Name",

        "NodeList,",
        "    SPACE1-1 Inlets,         !- Name",
        "    SPACE1-1 Supply Inlet;   !- Node 1 Name",

        "NodeList,",
        "    SPACE1-1 Exhausts,       !- Name",
        "    SPACE1-1 HP Inlet Node;  !- Node 1 Name",

        "NodeList,",
        "    OutsideAirInletNodes,    !- Name",
        "    PTACOAInNode;            !- Node 1 Name",

        "OutdoorAir:NodeList,",
        "    OutsideAirInletNodes;    !- Name",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    DataGlobals::NumOfTimeStepInHour = 1;
    DataGlobals::TimeStep = 1;
    DataGlobals::MinutesPerTimeStep = 60;
    ProcessScheduleInput(); // read schedules
    InitializePsychRoutines();
    OutputReportPredefined::SetPredefinedTables();

    GetZoneData(ErrorsFound);
    ASSERT_FALSE(ErrorsFound);

    GetZoneEquipmentData1();
    GetZoneAirLoopEquipment();
    GetPTUnit();
    GetPTUnitInputFlag = false;

    //// get input test for terminal air single duct mixer on inlet side of PTAC
    ASSERT_EQ(1, NumPTAC);
    EXPECT_EQ("ZoneHVAC:PackagedTerminalAirConditioner", PTUnit(1).UnitType); // zoneHVAC equipment type
    EXPECT_EQ("COIL:HEATING:FUEL", PTUnit(1).ACHeatCoilType);                 // PTAC heating coil type
    EXPECT_EQ(HeatingCoil(1).HCoilType_Num, Coil_HeatingGasOrOtherFuel);      // gas heating coil type

    BeginEnvrnFlag = false;

    // set input variables
    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::OutDryBulbTemp = 10.0;
    DataEnvironment::OutHumRat = 0.0075;
    DataEnvironment::OutEnthalpy = Psychrometrics::PsyHFnTdbW(DataEnvironment::OutDryBulbTemp, DataEnvironment::OutHumRat);
    DataEnvironment::StdRhoAir = 1.20;
    HVACInletMassFlowRate = 0.50;
    PrimaryAirMassFlowRate = 0.20;

    // set zoneNode air condition
    Node(ZoneEquipConfig(1).ZoneNode).Temp = 21.1;
    Node(ZoneEquipConfig(1).ZoneNode).HumRat = 0.0075;
    Node(ZoneEquipConfig(1).ZoneNode).Enthalpy =
        Psychrometrics::PsyHFnTdbW(Node(ZoneEquipConfig(1).ZoneNode).Temp, Node(ZoneEquipConfig(1).ZoneNode).HumRat);

    PackagedTerminalHeatPump::HeatingLoad = false;
    PackagedTerminalHeatPump::CoolingLoad = false;
    PackagedTerminalHeatPump::CompOnMassFlow = HVACInletMassFlowRate;     // supply air mass flow rate
    PackagedTerminalHeatPump::CompOffMassFlow = HVACInletMassFlowRate;    // supply air mass flow rate during comp off
    PackagedTerminalHeatPump::OACompOnMassFlow = PrimaryAirMassFlowRate;  // OA mass flow rate during comp on
    PackagedTerminalHeatPump::OACompOffMassFlow = PrimaryAirMassFlowRate; // OA mass flow rate during comp off
    PackagedTerminalHeatPump::CompOnFlowRatio = 1.0;
    DataHVACGlobals::ZoneCompTurnFansOff = false;
    DataHVACGlobals::ZoneCompTurnFansOn = true;

    PTUnitNum = 1;
    PTUnit(1).OpMode = ContFanCycCoil;

    Schedule(PTUnit(1).FanSchedPtr).CurrentValue = 1.0;      // unit is always on
    Schedule(PTUnit(1).SchedPtr).CurrentValue = 1.0;         // unit is always available
    Schedule(PTUnit(1).FanAvailSchedPtr).CurrentValue = 1.0; // fan is always available

    // initialize mass flow rates
    Node(PTUnit(1).AirInNode).MassFlowRate = HVACInletMassFlowRate;
    Node(PTUnit(1).OutsideAirNode).MassFlowRate = PrimaryAirMassFlowRate;
    Node(PTUnit(1).OutsideAirNode).MassFlowRateMaxAvail = PrimaryAirMassFlowRate;

    // set fan parameters
    Fan(1).MaxAirMassFlowRate = HVACInletMassFlowRate;
    Fan(1).InletAirMassFlowRate = HVACInletMassFlowRate;
    Fan(1).RhoAirStdInit = DataEnvironment::StdRhoAir;
    Node(Fan(1).InletNodeNum).MassFlowRateMaxAvail = HVACInletMassFlowRate;
    Node(Fan(1).OutletNodeNum).MassFlowRateMax = HVACInletMassFlowRate;

    // set DX coil rated performance parameters
    DXCoil(1).RatedCBF(1) = 0.05;
    DXCoil(1).RatedAirMassFlowRate(1) = HVACInletMassFlowRate;

    // primary air condition set at outdoor air condition
    Node(PTUnit(PTUnitNum).OutsideAirNode).Temp = DataEnvironment::OutDryBulbTemp;
    Node(PTUnit(PTUnitNum).OutsideAirNode).HumRat = DataEnvironment::OutHumRat;
    Node(PTUnit(PTUnitNum).OutsideAirNode).Enthalpy = DataEnvironment::OutEnthalpy;

    // set secondary air (recirculating air) conditions to zone air node
    Node(PTUnit(1).AirInNode).Temp = Node(ZoneEquipConfig(1).ZoneNode).Temp;
    Node(PTUnit(1).AirInNode).HumRat = Node(ZoneEquipConfig(1).ZoneNode).HumRat;
    Node(PTUnit(1).AirInNode).Enthalpy = Node(ZoneEquipConfig(1).ZoneNode).Enthalpy;

    PTUnit(1).ControlZoneNum = 1;
    SysSizingRunDone = true;
    ZoneSizingRunDone = true;
    SysSizingCalc = true;

    TempControlType.allocate(1);
    TempControlType(1) = 1;

    ZoneSysEnergyDemand.allocate(1);
    ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP = 0.0;    // set heating load to zero
    ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = 0.0;    // set cooling load to zero
    QZnReq = ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP; // zero zone heating load

    // supply fan is continuous flow
    PTUnit(1).MaxHeatAirMassFlow = HVACInletMassFlowRate;
    PTUnit(1).HeatingSpeedRatio = 1.0;
    PTUnit(1).HeatOutAirMassFlow = PrimaryAirMassFlowRate;
    PTUnit(1).MaxNoCoolHeatAirMassFlow = PrimaryAirMassFlowRate;
    PTUnit(1).NoHeatCoolSpeedRatio = 1.0;
    PTUnit(1).NoCoolHeatOutAirMassFlow = PrimaryAirMassFlowRate;
    PTUnit(1).AirFlowControl = UseCompressorOnFlow;
    PTUnit(1).LastMode = 2;

    // initialized to false
    ASSERT_FALSE(PackagedTerminalHeatPump::HeatingLoad);
    // Init PTAC zoneHVAC equipment
    InitPTUnit(PTUnitNum, ZoneNum, FirstHVACIteration, OnOffAirFlowRatio, QZnReq);
    // init sets heating mode to true due to cold ventilation air
    ASSERT_TRUE(PackagedTerminalHeatPump::HeatingLoad);
    // simulate PTAC zoneHVAC equipment
    SimPTUnit(PTUnitNum, ZoneNum, FirstHVACIteration, QUnitOut, OnOffAirFlowRatio, QZnReq, LatOutputProvided);
    // no zone heating load
    ASSERT_DOUBLE_EQ(QZnReq, 0.0);
    // no net heating delivered to the zone
    ASSERT_DOUBLE_EQ(QUnitOut, 0.0);
    // heating coil inlet air temperature
    ASSERT_NEAR(HeatingCoils::HeatingCoil(1).InletAirTemp, 16.74764, 0.00001);
    // heating coil tempers cold ventilation air to neutral (zone air temp)
    ASSERT_NEAR(HeatingCoils::HeatingCoil(1).OutletAirTemp, 21.1, 0.00001);
    // heating coil air flow rate, continuous fan operation
    ASSERT_NEAR(HeatingCoils::HeatingCoil(1).OutletAirMassFlowRate, 0.50, 0.00001);
    // heating coil load due to cold ventilation air
    ASSERT_NEAR(HeatingCoils::HeatingCoil(1).HeatingCoilRate, 2217.0, 1.0);
}

TEST_F(EnergyPlusFixture, SimPTAC_SZVAVTest)
{

    bool ErrorsFound(false);
    bool FirstHVACIteration(false);
    Real64 HVACInletMassFlowRate(0.0);
    Real64 OnOffAirFlowRatio(1.0);
    Real64 LatOutputProvided(0.0);
    Real64 QUnitOut(0.0);
    Real64 QZnReq(0.0);
    int ZoneNum(1);
    int PTUnitNum(1);

    std::string const idf_objects = delimited_string({
        "  Schedule:Compact,",
        "    FanAvailSched,           !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,            !- Field 16",
        "    1.0;                     !- Field 17",

        "  Schedule:Compact,",
        "    ContinuousFanSch,        !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,            !- Field 3",
        "    1.0;                     !- Field 4",

        "  ZoneHVAC:EquipmentList,",
        "    SPACE1-1 Equipment,      !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:PackagedTerminalAirConditioner,  !- Zone Equipment 1 Object Type",
        "    SPACE1-1 PTAC,           !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",

        "  ZoneHVAC:PackagedTerminalAirConditioner,",
        "    SPACE1-1 PTAC,           !- Name",
        "    FanAvailSched,           !- Availability Schedule Name",
        "    SPACE1-1 HP Inlet Node,  !- Air Inlet Node Name",
        "    SPACE1-1 Supply Inlet,   !- Air Outlet Node Name",
        "    OutdoorAir:Mixer,        !- Outdoor Air Mixer Object Type",
        "    PTACOAMixer,             !- Outdoor Air Mixer Name",
        "    0.500,                   !- Supply Air Flow Rate During Cooling Operation {m3/s}",
        "    0.500,                   !- Supply Air Flow Rate During Heating Operation {m3/s}",
        "    0.335,                   !- Supply Air Flow Rate When No Cooling or Heating is Needed {m3/s}",
        "    0.200,                   !- Outdoor Air Flow Rate During Cooling Operation {m3/s}",
        "    0.200,                   !- Outdoor Air Flow Rate During Heating Operation {m3/s}",
        "    0.200,                   !- Outdoor Air Flow Rate When No Cooling or Heating is Needed {m3/s}",
        "    Fan:ConstantVolume,      !- Supply Air Fan Object Type",
        "    SPACE1-1 Supply Fan,     !- Supply Air Fan Name",
        "    Coil:Heating:Fuel,       !- Heating Coil Object Type",
        "    SPACE1-1 Heating Coil,   !- Heating Coil Name",
        "    Coil:Cooling:DX:SingleSpeed,  !- Cooling Coil Object Type",
        "    SPACE1-1 PTAC CCoil,     !- Cooling Coil Name",
        "    BlowThrough,             !- Fan Placement",
        "    FanAvailSched,           !- Supply Air Fan Operating Mode Schedule Name",
        "    ,                        !- Availability Manager List Name",
        "    ,                        !- Design Specification ZoneHVAC Sizing Object Name",
        "    SingleZoneVAV,           !- Capacity Control Method",
        "    18.0,                    !- Minimum Supply Air Temperature in Cooling Mode",
        "    26.0;                    !- Maximum Supply Air Temperature in Heating Mode",

        "  OutdoorAir:Mixer,",
        "	 PTACOAMixer,             !- Name",
        "	 PTACOAMixerOutletNode,   !- Mixed Air Node Name",
        "    PTACOAInNode,            !- Outdoor Air Stream Node Name",
        "    ZoneExhausts,            !- Relief Air Stream Node Name",
        "    SPACE1-1 HP Inlet Node;  !- Return Air Stream Node Name",

        "Fan:ConstantVolume,",
        "    SPACE1-1 Supply Fan,     !- Name",
        "    FanAvailSched,           !- Availability Schedule Name",
        "    0.7,                     !- Fan Total Efficiency",
        "    75,                      !- Pressure Rise {Pa}",
        "    0.500,                   !- Maximum Flow Rate {m3/s}",
        "    0.9,                     !- Motor Efficiency",
        "    1,                       !- Motor In Airstream Fraction",
        "    PTACOAMixerOutletNode,   !- Air Inlet Node Name",
        "    SPACE1-1 Fan Outlet Node;!- Air Outlet Node Name",

        "Coil:Heating:Fuel,",
        "    SPACE1-1 Heating Coil,   !- Name",
        "    FanAvailSched,           !- Availability Schedule Name",
        "    Gas,                     !- Fuel Type",
        "    0.8,                     !- Gas Burner Efficiency",
        "    10000.0,                 !- Nominal Capacity {W}",
        "    SPACE1-1 CCoil Outlet Node,  !- Air Inlet Node Name",
        "    SPACE1-1 Supply Inlet;   !- Air Outlet Node Name",

        "  Coil:Cooling:DX:SingleSpeed,",
        "    SPACE1-1 PTAC CCoil,     !- Name",
        "    FanAvailSched,           !- Availability Schedule Name",
        "    6680.0,                  !- Gross Rated Total Cooling Capacity {W}",
        "    0.75,                    !- Gross Rated Sensible Heat Ratio",
        "    3.0,                     !- Gross Rated Cooling COP {W/W}",
        "    0.500,                   !- Rated Air Flow Rate {m3/s}",
        "    ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "    SPACE1-1 Fan Outlet Node,!- Air Inlet Node Name",
        "    SPACE1-1 CCoil Outlet Node,  !- Air Outlet Node Name",
        "    HPACCoolCapFT,           !- Total Cooling Capacity Function of Temperature Curve Name",
        "    HPACCoolCapFFF,          !- Total Cooling Capacity Function of Flow Fraction Curve Name",
        "    HPACEIRFT,               !- Energy Input Ratio Function of Temperature Curve Name",
        "    HPACEIRFFF,              !- Energy Input Ratio Function of Flow Fraction Curve Name",
        "    HPACPLFFPLR;             !- Part Load Fraction Correlation Curve Name",

        "  Curve:Quadratic,",
        "    HPACCoolCapFFF,          !- Name",
        "    0.8,                     !- Coefficient1 Constant",
        "    0.2,                     !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.5,                     !- Minimum Value of x",
        "    1.5;                     !- Maximum Value of x",

        "  Curve:Quadratic,",
        "    HPACEIRFFF,              !- Name",
        "    1.1552,                  !- Coefficient1 Constant",
        "    -0.1808,                 !- Coefficient2 x",
        "    0.0256,                  !- Coefficient3 x**2",
        "    0.5,                     !- Minimum Value of x",
        "    1.5;                     !- Maximum Value of x",

        "  Curve:Quadratic,",
        "    HPACPLFFPLR,             !- Name",
        "    0.85,                    !- Coefficient1 Constant",
        "    0.15,                    !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.0,                     !- Minimum Value of x",
        "    1.0;                     !- Maximum Value of x",

        "  Curve:Cubic,",
        "    FanEffRatioCurve,        !- Name",
        "    0.33856828,              !- Coefficient1 Constant",
        "    1.72644131,              !- Coefficient2 x",
        "    -1.49280132,             !- Coefficient3 x**2",
        "    0.42776208,              !- Coefficient4 x**3",
        "    0.5,                     !- Minimum Value of x",
        "    1.5,                     !- Maximum Value of x",
        "    0.3,                     !- Minimum Curve Output",
        "    1.0;                     !- Maximum Curve Output",

        "  Curve:Exponent,",
        "    FanPowerRatioCurve,      !- Name",
        "    0.0,                     !- Coefficient1 Constant",
        "    1.0,                     !- Coefficient2 Constant",
        "    3.0,                     !- Coefficient3 Constant",
        "    0.0,                     !- Minimum Value of x",
        "    1.5,                     !- Maximum Value of x",
        "    0.01,                    !- Minimum Curve Output",
        "    1.5;                     !- Maximum Curve Output",

        "  Curve:Biquadratic,",
        "    HPACCoolCapFT,           !- Name",
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
        "    HPACEIRFT,               !- Name",
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

        "Zone,",
        "    SPACE1-1,                !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    2.438400269,             !- Ceiling Height {m}",
        "    239.247360229;           !- Volume {m3}",

        "ZoneHVAC:EquipmentConnections,",
        "    SPACE1-1,                !- Zone Name",
        "    SPACE1-1 Equipment,      !- Zone Conditioning Equipment List Name",
        "    SPACE1-1 Inlets,         !- Zone Air Inlet Node or NodeList Name",
        "    SPACE1-1 Exhausts,       !- Zone Air Exhaust Node or NodeList Name",
        "    SPACE1-1 Zone Air Node,  !- Zone Air Node Name",
        "    SPACE1-1 Return Outlet;  !- Zone Return Air Node Name",

        "NodeList,",
        "    SPACE1-1 Inlets,         !- Name",
        "    SPACE1-1 Supply Inlet;   !- Node 1 Name",

        "NodeList,",
        "    SPACE1-1 Exhausts,       !- Name",
        "    SPACE1-1 HP Inlet Node;  !- Node 1 Name",

        "NodeList,",
        "    OutsideAirInletNodes,    !- Name",
        "    PTACOAInNode;            !- Node 1 Name",

        "OutdoorAir:NodeList,",
        "    OutsideAirInletNodes;    !- Name",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    DataGlobals::NumOfTimeStepInHour = 1;
    DataGlobals::TimeStep = 1;
    DataGlobals::MinutesPerTimeStep = 60;
    ProcessScheduleInput(); // read schedules
    InitializePsychRoutines();
    OutputReportPredefined::SetPredefinedTables();

    GetZoneData(ErrorsFound);
    ASSERT_FALSE(ErrorsFound);

    GetZoneEquipmentData1();
    GetZoneAirLoopEquipment();
    GetPTUnit();
    GetPTUnitInputFlag = false;

    BeginEnvrnFlag = true;

    // set input variables
    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::OutDryBulbTemp = 10.0;
    DataEnvironment::OutHumRat = 0.0075;
    DataEnvironment::OutEnthalpy = Psychrometrics::PsyHFnTdbW(DataEnvironment::OutDryBulbTemp, DataEnvironment::OutHumRat);
    DataEnvironment::StdRhoAir = 1.20;
    HVACInletMassFlowRate = 0.50;
    //		PrimaryAirMassFlowRate = 0.20;

    // set zoneNode air condition
    Node(ZoneEquipConfig(1).ZoneNode).Temp = 21.1;
    Node(ZoneEquipConfig(1).ZoneNode).HumRat = 0.0075;
    Node(ZoneEquipConfig(1).ZoneNode).Enthalpy =
        Psychrometrics::PsyHFnTdbW(Node(ZoneEquipConfig(1).ZoneNode).Temp, Node(ZoneEquipConfig(1).ZoneNode).HumRat);

    PackagedTerminalHeatPump::HeatingLoad = false;
    PackagedTerminalHeatPump::CoolingLoad = false;
    //		PackagedTerminalHeatPump::CompOnMassFlow = HVACInletMassFlowRate; // supply air mass flow rate
    //		PackagedTerminalHeatPump::CompOffMassFlow = HVACInletMassFlowRate; // supply air mass flow rate during comp off
    //		PackagedTerminalHeatPump::OACompOnMassFlow = PrimaryAirMassFlowRate; // OA mass flow rate during comp on
    //		PackagedTerminalHeatPump::OACompOffMassFlow = PrimaryAirMassFlowRate; // OA mass flow rate during comp off
    //		PackagedTerminalHeatPump::CompOnFlowRatio = 1.0;
    DataHVACGlobals::ZoneCompTurnFansOff = false;
    DataHVACGlobals::ZoneCompTurnFansOn = true;

    PTUnitNum = 1;
    PTUnit(1).OpMode = ContFanCycCoil;

    Schedule(PTUnit(1).FanSchedPtr).CurrentValue = 1.0;      // unit is always on
    Schedule(PTUnit(1).SchedPtr).CurrentValue = 1.0;         // unit is always available
    Schedule(PTUnit(1).FanAvailSchedPtr).CurrentValue = 1.0; // fan is always available

    // initialize mass flow rates
    //		Node( PTUnit( 1 ).AirInNode ).MassFlowRate = HVACInletMassFlowRate;
    //		Node( PTUnit( 1 ).OutsideAirNode ).MassFlowRate = PrimaryAirMassFlowRate;
    //		Node( PTUnit( 1 ).OutsideAirNode ).MassFlowRateMaxAvail = PrimaryAirMassFlowRate;

    // set fan parameters
    Fan(1).MaxAirMassFlowRate = HVACInletMassFlowRate;
    Fan(1).InletAirMassFlowRate = HVACInletMassFlowRate;
    Fan(1).RhoAirStdInit = DataEnvironment::StdRhoAir;
    Node(Fan(1).InletNodeNum).MassFlowRateMaxAvail = HVACInletMassFlowRate;
    Node(Fan(1).OutletNodeNum).MassFlowRateMax = HVACInletMassFlowRate;

    // set DX coil rated performance parameters
    DXCoil(1).RatedCBF(1) = 0.05;
    DXCoil(1).RatedAirMassFlowRate(1) = HVACInletMassFlowRate;

    // primary air condition set at outdoor air condition
    Node(PTUnit(PTUnitNum).OutsideAirNode).Temp = DataEnvironment::OutDryBulbTemp;
    Node(PTUnit(PTUnitNum).OutsideAirNode).HumRat = DataEnvironment::OutHumRat;
    Node(PTUnit(PTUnitNum).OutsideAirNode).Enthalpy = DataEnvironment::OutEnthalpy;

    // set secondary air (recirculating air) conditions to zone air node
    Node(PTUnit(1).AirInNode).Temp = Node(ZoneEquipConfig(1).ZoneNode).Temp;
    Node(PTUnit(1).AirInNode).HumRat = Node(ZoneEquipConfig(1).ZoneNode).HumRat;
    Node(PTUnit(1).AirInNode).Enthalpy = Node(ZoneEquipConfig(1).ZoneNode).Enthalpy;

    PTUnit(1).ControlZoneNum = 1;
    SysSizingRunDone = true;
    ZoneSizingRunDone = true;
    SysSizingCalc = false;
    DataZoneEquipment::ZoneEquipInputsFilled = true; // denotes zone equipment has been read in

    TempControlType.allocate(1);
    TempControlType(1) = 1;

    ZoneSysEnergyDemand.allocate(1);
    ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP = 0.0;    // set heating load to zero
    ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = 0.0;    // set cooling load to zero
    QZnReq = ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP; // zero zone heating load

    // supply fan is continuous flow
    //		PTUnit( 1 ).MaxHeatAirMassFlow = HVACInletMassFlowRate;
    //		PTUnit( 1 ).HeatingSpeedRatio = 1.0;
    //		PTUnit( 1 ).HeatOutAirMassFlow = PrimaryAirMassFlowRate;
    //		PTUnit( 1 ).MaxNoCoolHeatAirMassFlow = PrimaryAirMassFlowRate;
    //		PTUnit( 1 ).NoHeatCoolSpeedRatio = 1.0;
    //		PTUnit( 1 ).NoCoolHeatOutAirMassFlow = PrimaryAirMassFlowRate;
    //		PTUnit( 1 ).AirFlowControl = UseCompressorOnFlow;
    //		PTUnit( 1 ).LastMode = 2;

    // initialized to false
    ASSERT_FALSE(PackagedTerminalHeatPump::HeatingLoad);
    // Init PTAC zoneHVAC equipment
    InitPTUnit(PTUnitNum, ZoneNum, FirstHVACIteration, OnOffAirFlowRatio, QZnReq);
    BeginEnvrnFlag = false;
    InitPTUnit(PTUnitNum, ZoneNum, FirstHVACIteration, OnOffAirFlowRatio, QZnReq);
    // init sets heating mode to true due to cold ventilation air
    ASSERT_TRUE(PackagedTerminalHeatPump::HeatingLoad);
    // simulate PTAC zoneHVAC equipment
    SimPTUnit(PTUnitNum, ZoneNum, FirstHVACIteration, QUnitOut, OnOffAirFlowRatio, QZnReq, LatOutputProvided);
    // no zone heating load
    ASSERT_DOUBLE_EQ(QZnReq, 0.0);
    // no net heating delivered to the zone
    EXPECT_NEAR(QUnitOut, 0.0, 0.0000001);
    // heating coil inlet air temperature
    ASSERT_NEAR(HeatingCoils::HeatingCoil(1).InletAirTemp, 14.560774, 0.00001);
    // heating coil tempers cold ventilation air to neutral (zone air temp, otherwise QUnitOut out would be non-zero above)
    ASSERT_NEAR(HeatingCoils::HeatingCoil(1).OutletAirTemp, 21.1, 0.00001);
    // heating coil air flow rate, operate at minimum air flow rate
    ASSERT_NEAR(HeatingCoils::HeatingCoil(1).OutletAirMassFlowRate, 0.40200, 0.00001);
    // heating coil load due to cold ventilation air (but total load delivered by PTUnit is 0)
    ASSERT_NEAR(HeatingCoils::HeatingCoil(1).HeatingCoilRate, 2678.1427, 0.0001);

    // Boundary load for this system in Region 1 at minimum air flow rate is 2006.8 W (lower boundary load in Region 1)
    // loads below the bounday load should operate at the minimum air flow rate
    ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP = 1000.0; // set heating load to non-zero value below lower boundary load
    QZnReq = ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP; // initialize zone heating load
    SimPTUnit(PTUnitNum, ZoneNum, FirstHVACIteration, QUnitOut, OnOffAirFlowRatio, QZnReq, LatOutputProvided);
    ASSERT_NEAR(Node(PTUnit(PTUnitNum).AirInNode).MassFlowRate, PTUnit(PTUnitNum).MaxNoCoolHeatAirMassFlow, 0.001);
    ASSERT_GT(PTUnit(PTUnitNum).DesignMaxOutletTemp, HeatingCoils::HeatingCoil(1).OutletAirTemp);

    ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP = 2000.0; // set heating load to just below lower boundary load
    QZnReq = ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP; // initialize zone heating load
    SimPTUnit(PTUnitNum, ZoneNum, FirstHVACIteration, QUnitOut, OnOffAirFlowRatio, QZnReq, LatOutputProvided);
    ASSERT_NEAR(Node(PTUnit(PTUnitNum).AirInNode).MassFlowRate, PTUnit(PTUnitNum).MaxNoCoolHeatAirMassFlow, 0.001);
    ASSERT_GT(PTUnit(PTUnitNum).DesignMaxOutletTemp, HeatingCoils::HeatingCoil(1).OutletAirTemp);

    // loads above the lower bounday load should operate above the minimum air flow rate
    ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP = 2010.0; // set heating load to just above lower boundary load
    QZnReq = ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP; // initialize zone heating load
    SimPTUnit(PTUnitNum, ZoneNum, FirstHVACIteration, QUnitOut, OnOffAirFlowRatio, QZnReq, LatOutputProvided);
    ASSERT_NEAR(PTUnit(PTUnitNum).DesignMaxOutletTemp, HeatingCoils::HeatingCoil(1).OutletAirTemp, 0.1);
    ASSERT_GT(Node(PTUnit(PTUnitNum).AirInNode).MassFlowRate, PTUnit(PTUnitNum).MaxNoCoolHeatAirMassFlow);

    // Boundary load for this system in Region 1 at maximum air flow rate is 2995.2 W (upper boundary load of Region 1)
    // system should operate below the maximum air flow rate at loads less than 2995.2 W
    ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP = 2990.0; // set heating load to just below upper boundary load
    QZnReq = ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP; // initialize zone heating load
    SimPTUnit(PTUnitNum, ZoneNum, FirstHVACIteration, QUnitOut, OnOffAirFlowRatio, QZnReq, LatOutputProvided);
    ASSERT_NEAR(PTUnit(PTUnitNum).DesignMaxOutletTemp, HeatingCoils::HeatingCoil(1).OutletAirTemp, 0.1);
    ASSERT_GT(Node(PTUnit(PTUnitNum).AirInNode).MassFlowRate, PTUnit(PTUnitNum).MaxNoCoolHeatAirMassFlow);
    ASSERT_LT(Node(PTUnit(PTUnitNum).AirInNode).MassFlowRate, PTUnit(PTUnitNum).MaxHeatAirMassFlow);

    // Boundary load for this system in Region 1 at maximum air flow rate is 2995.2 W
    // system should operate at maximum air flow rate for loads greater than 2995.2 W
    // outlet air temperture is allowed to be above the design maximum supply air temperature in heating mode
    ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP = 3000.0; // set heating load to just above upper boundary load
    QZnReq = ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP; // initialize zone heating load
    SimPTUnit(PTUnitNum, ZoneNum, FirstHVACIteration, QUnitOut, OnOffAirFlowRatio, QZnReq, LatOutputProvided);
    ASSERT_GT(HeatingCoils::HeatingCoil(1).OutletAirTemp, PTUnit(PTUnitNum).DesignMaxOutletTemp);
    ASSERT_NEAR(Node(PTUnit(PTUnitNum).AirInNode).MassFlowRate, PTUnit(PTUnitNum).MaxHeatAirMassFlow, 0.0001);
}

} // namespace EnergyPlus
