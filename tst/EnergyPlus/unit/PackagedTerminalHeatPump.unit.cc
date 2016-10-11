// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
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
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
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
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

// EnergyPlus::VariableSpeedCoils Unit Tests

// Google Test Headers
#include <gtest/gtest.h>
#include "Fixtures/EnergyPlusFixture.hh"
#include <ObjexxFCL/Array1D.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/PackagedTerminalHeatPump.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/VariableSpeedCoils.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DataPlant;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::Fans;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::PackagedTerminalHeatPump;
using namespace EnergyPlus::SimulationManager;
using namespace EnergyPlus::VariableSpeedCoils;
using namespace ObjexxFCL;

namespace EnergyPlus {

	TEST_F( EnergyPlusFixture, PackagedTerminalHP_VSCoils_Sizing ) {
		std::string const idf_objects = delimited_string({

		"  Zone, Space, 0.0, 0.0, 0.0, 0.0, 1, 1, 2.4, , autocalculate, , , Yes; ",
		"  ZoneHVAC:EquipmentConnections, Space, Space Eq, Space In Node, Space Out Node, Space Node, Space Ret Node; ",
		"  ZoneHVAC:EquipmentList, Space Eq, ZoneHVAC:WaterToAirHeatPump, Zone WSHP, 1, 1; ",
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

		ASSERT_FALSE( process_idf( idf_objects ) );

		bool ErrorsFound( false );
		GetZoneData( ErrorsFound );
		GetZoneEquipmentData();
		GetPTUnit();

		TotNumLoops = 2;
		PlantLoop.allocate( TotNumLoops );

		for( int l = 1; l <= TotNumLoops; ++l ) {
			auto & loop( PlantLoop( l ) );
			loop.LoopSide.allocate( 2 );
			auto & loopside( PlantLoop( l ).LoopSide( 1 ) );
			loopside.TotalBranches = 1;
			loopside.Branch.allocate( 1 );
			auto & loopsidebranch( PlantLoop( l ).LoopSide( 1 ).Branch( 1 ) );
			loopsidebranch.TotalComponents = 1;
			loopsidebranch.Comp.allocate( 1 );
		}
		PlantLoop( 2 ).Name = "ChilledWaterLoop";
		PlantLoop( 2 ).FluidName = "ChilledWater";
		PlantLoop( 2 ).FluidIndex = 1;
		PlantLoop( 2 ).FluidName = "WATER";
		PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).Name = VarSpeedCoil( 2 ).Name;
		PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).TypeOf_Num = VarSpeedCoil( 2 ).VSCoilTypeOfNum;
		PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = VarSpeedCoil( 2 ).WaterInletNodeNum;
		PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumOut = VarSpeedCoil( 2 ).WaterOutletNodeNum;

		PlantLoop( 1 ).Name = "HotWaterLoop";
		PlantLoop( 1 ).FluidName = "HotWater";
		PlantLoop( 1 ).FluidIndex = 1;
		PlantLoop( 1 ).FluidName = "WATER";
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).Name = VarSpeedCoil( 1 ).Name;
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).TypeOf_Num = VarSpeedCoil( 1 ).VSCoilTypeOfNum;
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = VarSpeedCoil( 1 ).WaterInletNodeNum;
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumOut = VarSpeedCoil( 1 ).WaterOutletNodeNum;

		DataSizing::CurZoneEqNum = 1;
		DataSizing::ZoneSizingRunDone = true;
		DataSizing::FinalZoneSizing.allocate( 1 );
		DataSizing::FinalZoneSizing( DataSizing::CurZoneEqNum ).DesCoolVolFlow = 1.0;
		DataSizing::FinalZoneSizing( DataSizing::CurZoneEqNum ).DesCoolCoilInTemp = 24.0;
		DataSizing::FinalZoneSizing( DataSizing::CurZoneEqNum ).DesCoolCoilInHumRat = 0.09;
		DataSizing::FinalZoneSizing( DataSizing::CurZoneEqNum ).CoolDesTemp = 12.0;
		DataSizing::FinalZoneSizing( DataSizing::CurZoneEqNum ).CoolDesHumRat = 0.05;
		DataEnvironment::OutBaroPress = 101325;
		OutputReportPredefined::SetPredefinedTables();
		DataSizing::ZoneEqSizing.allocate( 1 );
		DataSizing::ZoneEqSizing( DataSizing::CurZoneEqNum ).SizingMethod.allocate( 16 );
		SizePTUnit( 1 );

		// This VS coil is rather quirky. It sizes the capacity based on zone sizing air flow rate.
		// Then uses that capacity to back calculate the air flow needed to keep the reference air flow per capacity ratio constant.
		// For this reason, the parent object would size to an air flow that was different than the chile.

		// identify coil
		EXPECT_EQ ( VariableSpeedCoils::VarSpeedCoil( 1 ).Name, "LOBBY_ZN_1_FLR_2 WSHP COOLING MODE" );

		// expect coil air flow to equal PTUnit cooling air flow
		EXPECT_EQ( VariableSpeedCoils::VarSpeedCoil( 1 ).RatedAirVolFlowRate, PTUnit( 1 ).MaxCoolAirVolFlow );
		EXPECT_EQ( VariableSpeedCoils::VarSpeedCoil( 1 ).MSRatedAirVolFlowRate( 9 ), PTUnit( 1 ).MaxCoolAirVolFlow );

		// expect the ratio of air flow to capacity to equal to the reference air flow and capacity specified in coil input
		Real64 refAirflowCapacityRatio = 0.891980668 / 16092.825525; // speed 9 reference cooling data
		Real64 sizingAirflowCapacityRatio = VariableSpeedCoils::VarSpeedCoil( 1 ).MSRatedAirVolFlowRate( 9 ) / VariableSpeedCoils::VarSpeedCoil( 1 ).MSRatedTotCap( 9 );
		EXPECT_EQ( refAirflowCapacityRatio, sizingAirflowCapacityRatio );

		// this same ratio should also equal the internal flow per capacity variable used to back calculate operating air flow rate
		EXPECT_EQ( sizingAirflowCapacityRatio, VariableSpeedCoils::VarSpeedCoil( 1 ).MSRatedAirVolFlowPerRatedTotCap( 9 ) );

		// identify coil
		EXPECT_EQ( VariableSpeedCoils::VarSpeedCoil( 2 ).Name, "LOBBY_ZN_1_FLR_2 WSHP HEATING MODE" );

		// expect coil air flow to equal PTUnit heating air flow
		EXPECT_EQ( VariableSpeedCoils::VarSpeedCoil( 2 ).RatedAirVolFlowRate, PTUnit( 1 ).MaxHeatAirVolFlow );
		EXPECT_EQ( VariableSpeedCoils::VarSpeedCoil( 2 ).MSRatedAirVolFlowRate( 9 ), PTUnit( 1 ).MaxHeatAirVolFlow );

		// expect the ratio of air flow to capacity to equal to the reference air flow and capacity specified in coil input
		refAirflowCapacityRatio = 0.891980668 / 20894.501936; // speed 9 reference heating data
		sizingAirflowCapacityRatio = VariableSpeedCoils::VarSpeedCoil( 2 ).MSRatedAirVolFlowRate( 9 ) / VariableSpeedCoils::VarSpeedCoil( 2 ).MSRatedTotCap( 9 );
		EXPECT_EQ( refAirflowCapacityRatio, sizingAirflowCapacityRatio );

		// this same ratio should also equal the internal flow per capacity variable used to back calculate operating air flow rate
		EXPECT_EQ( sizingAirflowCapacityRatio, VariableSpeedCoils::VarSpeedCoil( 2 ).MSRatedAirVolFlowPerRatedTotCap( 9 ) );

		SizeFan( 1 );
		// the fan vol flow rate should equal the max of cooling and heating coil flow rates
		EXPECT_EQ( Fan( 1 ).MaxAirFlowRate, max( VariableSpeedCoils::VarSpeedCoil( 1 ).RatedAirVolFlowRate, VariableSpeedCoils::VarSpeedCoil( 2 ).RatedAirVolFlowRate ) );
		EXPECT_EQ( Fan( 1 ).MaxAirFlowRate, max( PTUnit( 1 ).MaxCoolAirVolFlow, PTUnit( 1 ).MaxHeatAirVolFlow ) );

	}
}
