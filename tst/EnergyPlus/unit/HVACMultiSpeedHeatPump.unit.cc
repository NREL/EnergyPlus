// EnergyPlus, Copyright (c) 1996-2015, The Board of Trustees of the University of Illinois and
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

// EnergyPlus::MultiSpeedAirToAirHeatPump Unit Tests

// Google Test Headers
#include <gtest/gtest.h>
#include "Fixtures/EnergyPlusFixture.hh"

#include <EnergyPlus/BranchInputManager.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataZoneControls.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/DirectAirManager.hh>
#include <EnergyPlus/HVACMultiSpeedHeatPump.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SimAirServingZones.hh>
#include <EnergyPlus/SplitterComponent.hh>
#include <EnergyPlus/ZoneTempPredictorCorrector.hh>

using namespace EnergyPlus::BranchInputManager;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataZoneControls;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DataZoneEnergyDemands;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::DirectAirManager;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::HVACMultiSpeedHeatPump;
using namespace EnergyPlus::MixedAir;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::SimAirServingZones;
using namespace EnergyPlus::SplitterComponent;
using namespace EnergyPlus::ZoneTempPredictorCorrector;


namespace EnergyPlus {


	TEST_F( EnergyPlusFixture, HVACMultiSpeedHeatPump_ReportVariableInitTest ) {

		bool ErrorsFound( false );
		int const MSHeatPumpNum( 2 );
		bool const FirstHVACIteration( true );
		int const AirLoopNum( 2 );
		Real64 QZnReq( -10000.0 );
		Real64 OnOffAirFlowRatio( 1.0 );


		std::string const idf_objects = delimited_string({

			"  Version,8.4;",

			"!-   ===========  ALL OBJECTS IN CLASS: ZONECONTROL:THERMOSTAT ===========",


			"  ZoneControl:Thermostat,",
			"    Z401TempCtrl,            !- Name",
			"    401,                     !- Zone or ZoneList Name",
			"    Zone Control Type Sched, !- Control Type Schedule Name",
			"    ThermostatSetpoint:DualSetpoint,   !- Control 3 Object Type",
			"    DualSetPoint1;            !- Control 3 Name",

			"  ThermostatSetpoint:DualSetpoint,",
			"    DualSetPoint1,            !- Name",
			"    401HeatingSP,            !- Heating Setpoint Temperature Schedule Name",
			"    401CoolingSP;            !- Cooling Setpoint Temperature Schedule Name",

			"  ZoneControl:Thermostat,",
			"    Z402TempCtrl,            !- Name",
			"    402,                     !- Zone or ZoneList Name",
			"    Zone Control Type Sched, !- Control Type Schedule Name",
			"    ThermostatSetpoint:DualSetpoint,   !- Control 3 Object Type",
			"    DualSetPoint2;            !- Control 3 Name",

			"  ThermostatSetpoint:DualSetpoint,",
			"    DualSetPoint2,            !- Name",
			"    402HeatingSP,            !- Heating Setpoint Temperature Schedule Name",
			"    402CoolingSP;            !- Cooling Setpoint Temperature Schedule Name",

			"!-   ===========  ALL OBJECTS IN CLASS: AIRTERMINAL:SINGLEDUCT:UNCONTROLLED ===========",

			"  AirTerminal:SingleDuct:Uncontrolled,",
			"    401directair,            !- Name",
			"    AC-24sched,              !- Availability Schedule Name",
			"    Z401 zone inlet,         !- Zone Supply Air Node Name",
			"    3.209;                   !- Maximum Air Flow Rate {m3/s}",

			"  AirTerminal:SingleDuct:Uncontrolled,",
			"    402directair,            !- Name",
			"    AC-25 sched,             !- Availability Schedule Name",
			"    Z402 zone inlet,         !- Zone Supply Air Node Name",
			"    3.209;                   !- Maximum Air Flow Rate {m3/s}",

			"!-   ===========  ALL OBJECTS IN CLASS: ZONEHVAC:EQUIPMENTLIST ===========",

			"  ZoneHVAC:EquipmentList,",
			"    Z401 terminal list,      !- Name",
			"    AirTerminal:SingleDuct:Uncontrolled,  !- Zone Equipment 1 Object Type",
			"    401directair,            !- Zone Equipment 1 Name",
			"    1,                       !- Zone Equipment 1 Cooling Sequence",
			"    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",

			"  ZoneHVAC:EquipmentList,",
			"    Z402 terminal list,      !- Name",
			"    AirTerminal:SingleDuct:Uncontrolled,  !- Zone Equipment 1 Object Type",
			"    402directair,            !- Zone Equipment 1 Name",
			"    1,                       !- Zone Equipment 1 Cooling Sequence",
			"    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",

			"!-   ===========  ALL OBJECTS IN CLASS: ZONEHVAC:EQUIPMENTCONNECTIONS ===========",

			"  ZoneHVAC:EquipmentConnections,",
			"    401,                     !- Zone Name",
			"    Z401 terminal list,      !- Zone Conditioning Equipment List Name",
			"    Z401 zone inlet,         !- Zone Air Inlet Node or NodeList Name",
			"    ,                        !- Zone Air Exhaust Node or NodeList Name",
			"    Z401 air node,           !- Zone Air Node Name",
			"    Z401 outlet node;        !- Zone Return Air Node Name",

			"  ZoneHVAC:EquipmentConnections,",
			"    402,                     !- Zone Name",
			"    Z402 terminal list,      !- Zone Conditioning Equipment List Name",
			"    Z402 zone inlet,         !- Zone Air Inlet Node or NodeList Name",
			"    ,                        !- Zone Air Exhaust Node or NodeList Name",
			"    Z402 air node,           !- Zone Air Node Name",
			"    Z402 outlet node;        !- Zone Return Air Node Name",

			"!-   ===========  ALL OBJECTS IN CLASS: FAN:ONOFF ===========",
			"  Fan:OnOff,",
			"    AC24_Fan,                !- Name",
			"    AC-24sched,              !- Availability Schedule Name",
			"    0.25,                    !- Fan Total Efficiency",
			"    249.089,                 !- Pressure Rise {Pa}",
			"    3.209,                   !- Maximum Flow Rate {m3/s}",
			"    0.85,                    !- Motor Efficiency",
			"    1,                       !- Motor In Airstream Fraction",
			"    AC-24 SF inlet air node, !- Air Inlet Node Name",
			"    AC-24 SF outlet air node,!- Air Outlet Node Name",
			"    ,                        !- Fan Power Ratio Function of Speed Ratio Curve Name",
			"    ,                        !- Fan Efficiency Ratio Function of Speed Ratio Curve Name",
			"    General;                 !- End-Use Subcategory",
			"  Fan:OnOff,",
			"    AC25_Fan,                !- Name",
			"    AC-25 sched,             !- Availability Schedule Name",
			"    0.25,                    !- Fan Total Efficiency",
			"    249.089,                 !- Pressure Rise {Pa}",
			"    3.209,                   !- Maximum Flow Rate {m3/s}",
			"    0.85,                    !- Motor Efficiency",
			"    1,                       !- Motor In Airstream Fraction",
			"    AC-25 SF inlet air node, !- Air Inlet Node Name",
			"    AC-25 SF outlet air node,!- Air Outlet Node Name",
			"    ,                        !- Fan Power Ratio Function of Speed Ratio Curve Name",
			"    ,                        !- Fan Efficiency Ratio Function of Speed Ratio Curve Name",
			"    General;                 !- End-Use Subcategory",
			"  Coil:Cooling:DX:MultiSpeed,",
			"    AC24_cooling,            !- Name",
			"    AC-24sched,              !- Availability Schedule Name",
			"    AC-24 SF outlet air node,!- Air Inlet Node Name",
			"    AC-24 HC inlet node,     !- Air Outlet Node Name",
			"    ,                        !- Condenser Air Inlet Node Name",
			"    AirCooled,               !- Condenser Type",
			"    ,                        !- Supply Water Storage Tank Name",
			"    ,                        !- Condensate Collection Water Storage Tank Name",
			"    No,                      !- Apply Part Load Fraction to Speeds Greater than 1",
			"    No,                      !- Apply Latent Degradation to Speeds Greater than 1",
			"    0,                       !- Crankcase Heater Capacity {W}",
			"    10,                      !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
			"    ,                        !- Basin Heater Capacity {W/K}",
			"    ,                        !- Basin Heater Setpoint Temperature {C}",
			"    ,                        !- Basin Heater Operating Schedule Name",
			"    Electricity,             !- Fuel Type",
			"    2,                       !- Number of Speeds",
			"    53500,                   !- Speed 1 Gross Rated Total Cooling Capacity {W}",
			"    0.737,                   !- Speed 1 Gross Rated Sensible Heat Ratio",
			"    3.42,                    !- Speed 1 Gross Rated Cooling COP {W/W}",
			"    3.209,                   !- Speed 1 Rated Air Flow Rate {m3/s}",
			"    ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
			"    CoolingTempCurve,        !- Speed 1 Total Cooling Capacity Function of Temperature Curve Name",
			"    CoolingFlowCurve,        !- Speed 1 Total Cooling Capacity Function of Flow Fraction Curve Name",
			"    EIRTempCurve,            !- Speed 1 Energy Input Ratio Function of Temperature Curve Name",
			"    EIRFlowCurve,            !- Speed 1 Energy Input Ratio Function of Flow Fraction Curve Name",
			"    PLFCurve,                !- Speed 1 Part Load Fraction Correlation Curve Name",
			"    ,                        !- Speed 1 Nominal Time for Condensate Removal to Begin {s}",
			"    ,                        !- Speed 1 Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity {dimensionless}",
			"    3,                       !- Speed 1 Maximum Cycling Rate {cycles/hr}",
			"    ,                        !- Speed 1 Latent Capacity Time Constant {s}",
			"    0.001,                   !- Speed 1 Rated Waste Heat Fraction of Power Input {dimensionless}",
			"    WasteHeatTempCurve,      !- Speed 1 Waste Heat Function of Temperature Curve Name",
			"    0.9,                     !- Speed 1 Evaporative Condenser Effectiveness {dimensionless}",
			"    ,                        !- Speed 1 Evaporative Condenser Air Flow Rate {m3/s}",
			"    ,                        !- Speed 1 Rated Evaporative Condenser Pump Power Consumption {W}",
			"    67992.5,                 !- Speed 2 Gross Rated Total Cooling Capacity {W}",
			"    0.737,                   !- Speed 2 Gross Rated Sensible Heat Ratio",
			"    3.42,                    !- Speed 2 Gross Rated Cooling COP {W/W}",
			"    3.209,                   !- Speed 2 Rated Air Flow Rate {m3/s}",
			"    ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
			"    CoolingTempCurve,        !- Speed 2 Total Cooling Capacity Function of Temperature Curve Name",
			"    CoolingFlowCurve,        !- Speed 2 Total Cooling Capacity Function of Flow Fraction Curve Name",
			"    EIRTempCurve,            !- Speed 2 Energy Input Ratio Function of Temperature Curve Name",
			"    EIRFlowCurve,            !- Speed 2 Energy Input Ratio Function of Flow Fraction Curve Name",
			"    PLFCurve,                !- Speed 2 Part Load Fraction Correlation Curve Name",
			"    ,                        !- Speed 2 Nominal Time for Condensate Removal to Begin {s}",
			"    ,                        !- Speed 2 Ratio of Initial Moisture Evaporation Rate and steady state Latent Capacity {dimensionless}",
			"    0,                       !- Speed 2 Maximum Cycling Rate {cycles/hr}",
			"    ,                        !- Speed 2 Latent Capacity Time Constant {s}",
			"    0.001,                   !- Speed 2 Rated Waste Heat Fraction of Power Input {dimensionless}",
			"    WasteHeatTempCurve,      !- Speed 2 Waste Heat Function of Temperature Curve Name",
			"    0.9,                     !- Speed 2 Evaporative Condenser Effectiveness {dimensionless}",
			"    ,                        !- Speed 2 Evaporative Condenser Air Flow Rate {m3/s}",
			"    ,                        !- Speed 2 Rated Evaporative Condenser Pump Power Consumption {W}",
			"    ,                        !- Speed 3 Gross Rated Total Cooling Capacity {W}",
			"    ,                        !- Speed 3 Gross Rated Sensible Heat Ratio",
			"    3,                       !- Speed 3 Gross Rated Cooling COP {W/W}",
			"    ,                        !- Speed 3 Rated Air Flow Rate {m3/s}",
			"    ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
			"    ,                        !- Speed 3 Total Cooling Capacity Function of Temperature Curve Name",
			"    ,                        !- Speed 3 Total Cooling Capacity Function of Flow Fraction Curve Name",
			"    ,                        !- Speed 3 Energy Input Ratio Function of Temperature Curve Name",
			"    ,                        !- Speed 3 Energy Input Ratio Function of Flow Fraction Curve Name",
			"    ,                        !- Speed 3 Part Load Fraction Correlation Curve Name",
			"    ,                        !- Speed 3 Nominal Time for Condensate Removal to Begin {s}",
			"    ,                        !- Speed 3 Ratio of Initial Moisture Evaporation Rate and steady state Latent Capacity {dimensionless}",
			"    ,                        !- Speed 3 Maximum Cycling Rate {cycles/hr}",
			"    ,                        !- Speed 3 Latent Capacity Time Constant {s}",
			"    ,                        !- Speed 3 Rated Waste Heat Fraction of Power Input {dimensionless}",
			"    ,                        !- Speed 3 Waste Heat Function of Temperature Curve Name",
			"    0.9,                     !- Speed 3 Evaporative Condenser Effectiveness {dimensionless}",
			"    ,                        !- Speed 3 Evaporative Condenser Air Flow Rate {m3/s}",
			"    ,                        !- Speed 3 Rated Evaporative Condenser Pump Power Consumption {W}",
			"    ,                        !- Speed 4 Gross Rated Total Cooling Capacity {W}",
			"    ,                        !- Speed 4 Gross Rated Sensible Heat Ratio",
			"    3,                       !- Speed 4 Gross Rated Cooling COP {W/W}",
			"    ,                        !- Speed 4 Rated Air Flow Rate {m3/s}",
			"    ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
			"    ,                        !- Speed 4 Total Cooling Capacity Function of Temperature Curve Name",
			"    ,                        !- Speed 4 Total Cooling Capacity Function of Flow Fraction Curve Name",
			"    ,                        !- Speed 4 Energy Input Ratio Function of Temperature Curve Name",
			"    ,                        !- Speed 4 Energy Input Ratio Function of Flow Fraction Curve Name",
			"    ,                        !- Speed 4 Part Load Fraction Correlation Curve Name",
			"    ,                        !- Speed 4 Nominal Time for Condensate Removal to Begin {s}",
			"    ,                        !- Speed 4 Ratio of Initial Moisture Evaporation Rate and steady state Latent Capacity {dimensionless}",
			"    ,                        !- Speed 4 Maximum Cycling Rate {cycles/hr}",
			"    ,                        !- Speed 4 Latent Capacity Time Constant {s}",
			"    ,                        !- Speed 4 Rated Waste Heat Fraction of Power Input {dimensionless}",
			"    ,                        !- Speed 4 Waste Heat Function of Temperature Curve Name",
			"    0.9;                     !- Speed 4 Evaporative Condenser Effectiveness {dimensionless}",
			"  Coil:Cooling:DX:MultiSpeed,",
			"    AC25_cooling,            !- Name",
			"    AC-25 sched,             !- Availability Schedule Name",
			"    AC-25 SF outlet air node,!- Air Inlet Node Name",
			"    AC-25 HC inlet node,     !- Air Outlet Node Name",
			"    ,                        !- Condenser Air Inlet Node Name",
			"    AirCooled,               !- Condenser Type",
			"    ,                        !- Supply Water Storage Tank Name",
			"    ,                        !- Condensate Collection Water Storage Tank Name",
			"    No,                      !- Apply Part Load Fraction to Speeds Greater than 1",
			"    No,                      !- Apply Latent Degradation to Speeds Greater than 1",
			"    0,                       !- Crankcase Heater Capacity {W}",
			"    10,                      !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
			"    ,                        !- Basin Heater Capacity {W/K}",
			"    ,                        !- Basin Heater Setpoint Temperature {C}",
			"    ,                        !- Basin Heater Operating Schedule Name",
			"    Electricity,             !- Fuel Type",
			"    2,                       !- Number of Speeds",
			"    53500,                   !- Speed 1 Gross Rated Total Cooling Capacity {W}",
			"    0.737,                   !- Speed 1 Gross Rated Sensible Heat Ratio",
			"    3.42,                    !- Speed 1 Gross Rated Cooling COP {W/W}",
			"    3.209,                   !- Speed 1 Rated Air Flow Rate {m3/s}",
			"    ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
			"    CoolingTempCurve,        !- Speed 1 Total Cooling Capacity Function of Temperature Curve Name",
			"    CoolingFlowCurve,        !- Speed 1 Total Cooling Capacity Function of Flow Fraction Curve Name",
			"    EIRTempCurve,            !- Speed 1 Energy Input Ratio Function of Temperature Curve Name",
			"    EIRFlowCurve,            !- Speed 1 Energy Input Ratio Function of Flow Fraction Curve Name",
			"    PLFCurve,                !- Speed 1 Part Load Fraction Correlation Curve Name",
			"    ,                        !- Speed 1 Nominal Time for Condensate Removal to Begin {s}",
			"    ,                        !- Speed 1 Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity {dimensionless}",
			"    3,                       !- Speed 1 Maximum Cycling Rate {cycles/hr}",
			"    ,                        !- Speed 1 Latent Capacity Time Constant {s}",
			"    0.001,                   !- Speed 1 Rated Waste Heat Fraction of Power Input {dimensionless}",
			"    WasteHeatTempCurve,      !- Speed 1 Waste Heat Function of Temperature Curve Name",
			"    0.9,                     !- Speed 1 Evaporative Condenser Effectiveness {dimensionless}",
			"    ,                        !- Speed 1 Evaporative Condenser Air Flow Rate {m3/s}",
			"    ,                        !- Speed 1 Rated Evaporative Condenser Pump Power Consumption {W}",
			"    67992.5,                 !- Speed 2 Gross Rated Total Cooling Capacity {W}",
			"    0.737,                   !- Speed 2 Gross Rated Sensible Heat Ratio",
			"    3.42,                    !- Speed 2 Gross Rated Cooling COP {W/W}",
			"    3.209,                   !- Speed 2 Rated Air Flow Rate {m3/s}",
			"    ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
			"    CoolingTempCurve,        !- Speed 2 Total Cooling Capacity Function of Temperature Curve Name",
			"    CoolingFlowCurve,        !- Speed 2 Total Cooling Capacity Function of Flow Fraction Curve Name",
			"    EIRTempCurve,            !- Speed 2 Energy Input Ratio Function of Temperature Curve Name",
			"    EIRFlowCurve,            !- Speed 2 Energy Input Ratio Function of Flow Fraction Curve Name",
			"    PLFCurve,                !- Speed 2 Part Load Fraction Correlation Curve Name",
			"    ,                        !- Speed 2 Nominal Time for Condensate Removal to Begin {s}",
			"    ,                        !- Speed 2 Ratio of Initial Moisture Evaporation Rate and steady state Latent Capacity {dimensionless}",
			"    0,                       !- Speed 2 Maximum Cycling Rate {cycles/hr}",
			"    ,                        !- Speed 2 Latent Capacity Time Constant {s}",
			"    0.001,                   !- Speed 2 Rated Waste Heat Fraction of Power Input {dimensionless}",
			"    WasteHeatTempCurve,      !- Speed 2 Waste Heat Function of Temperature Curve Name",
			"    0.9,                     !- Speed 2 Evaporative Condenser Effectiveness {dimensionless}",
			"    ,                        !- Speed 2 Evaporative Condenser Air Flow Rate {m3/s}",
			"    ,                        !- Speed 2 Rated Evaporative Condenser Pump Power Consumption {W}",
			"    ,                        !- Speed 3 Gross Rated Total Cooling Capacity {W}",
			"    ,                        !- Speed 3 Gross Rated Sensible Heat Ratio",
			"    3,                       !- Speed 3 Gross Rated Cooling COP {W/W}",
			"    ,                        !- Speed 3 Rated Air Flow Rate {m3/s}",
			"    ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
			"    ,                        !- Speed 3 Total Cooling Capacity Function of Temperature Curve Name",
			"    ,                        !- Speed 3 Total Cooling Capacity Function of Flow Fraction Curve Name",
			"    ,                        !- Speed 3 Energy Input Ratio Function of Temperature Curve Name",
			"    ,                        !- Speed 3 Energy Input Ratio Function of Flow Fraction Curve Name",
			"    ,                        !- Speed 3 Part Load Fraction Correlation Curve Name",
			"    ,                        !- Speed 3 Nominal Time for Condensate Removal to Begin {s}",
			"    ,                        !- Speed 3 Ratio of Initial Moisture Evaporation Rate and steady state Latent Capacity {dimensionless}",
			"    ,                        !- Speed 3 Maximum Cycling Rate {cycles/hr}",
			"    ,                        !- Speed 3 Latent Capacity Time Constant {s}",
			"    ,                        !- Speed 3 Rated Waste Heat Fraction of Power Input {dimensionless}",
			"    ,                        !- Speed 3 Waste Heat Function of Temperature Curve Name",
			"    0.9,                     !- Speed 3 Evaporative Condenser Effectiveness {dimensionless}",
			"    ,                        !- Speed 3 Evaporative Condenser Air Flow Rate {m3/s}",
			"    ,                        !- Speed 3 Rated Evaporative Condenser Pump Power Consumption {W}",
			"    ,                        !- Speed 4 Gross Rated Total Cooling Capacity {W}",
			"    ,                        !- Speed 4 Gross Rated Sensible Heat Ratio",
			"    3,                       !- Speed 4 Gross Rated Cooling COP {W/W}",
			"    ,                        !- Speed 4 Rated Air Flow Rate {m3/s}",
			"    ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
			"    ,                        !- Speed 4 Total Cooling Capacity Function of Temperature Curve Name",
			"    ,                        !- Speed 4 Total Cooling Capacity Function of Flow Fraction Curve Name",
			"    ,                        !- Speed 4 Energy Input Ratio Function of Temperature Curve Name",
			"    ,                        !- Speed 4 Energy Input Ratio Function of Flow Fraction Curve Name",
			"    ,                        !- Speed 4 Part Load Fraction Correlation Curve Name",
			"    ,                        !- Speed 4 Nominal Time for Condensate Removal to Begin {s}",
			"    ,                        !- Speed 4 Ratio of Initial Moisture Evaporation Rate and steady state Latent Capacity {dimensionless}",
			"    ,                        !- Speed 4 Maximum Cycling Rate {cycles/hr}",
			"    ,                        !- Speed 4 Latent Capacity Time Constant {s}",
			"    ,                        !- Speed 4 Rated Waste Heat Fraction of Power Input {dimensionless}",
			"    ,                        !- Speed 4 Waste Heat Function of Temperature Curve Name",
			"    0.9;                     !- Speed 4 Evaporative Condenser Effectiveness {dimensionless}",
			"!-   ===========  ALL OBJECTS IN CLASS: COIL:HEATING:ELECTRIC ===========",
			"  Coil:Heating:Electric,",
			"    AC24ElecHeater,          !- Name",
			"    AC-24sched,              !- Availability Schedule Name",
			"    1,                       !- Efficiency",
			"    55000,                   !- Nominal Capacity {W}",
			"    AC-24 RHC inlet node,    !- Air Inlet Node Name",
			"    AC-24 airloop outlet node;  !- Air Outlet Node Name",
			"  Coil:Heating:Electric,",
			"    AC25ElecHeater,          !- Name",
			"    AC-25 sched,             !- Availability Schedule Name",
			"    1,                       !- Efficiency",
			"    55000,                   !- Nominal Capacity {W}",
			"    AC-25 RHC inlet node,    !- Air Inlet Node Name",
			"    AC-25 airloop outlet node;  !- Air Outlet Node Name",

			"!-   ===========  ALL OBJECTS IN CLASS: COIL:HEATING:DX:MULTISPEED ===========",
			"  Coil:Heating:DX:MultiSpeed,",
			"    AC24Heating,             !- Name",
			"    AC-24sched,              !- Availability Schedule Name",
			"    AC-24 HC inlet node,     !- Air Inlet Node Name",
			"    AC-24 RHC inlet node,    !- Air Outlet Node Name",
			"    -8,                      !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
			"    ,                        !- Outdoor Dry-Bulb Temperature to Turn On Compressor {C}",
			"    0,                       !- Crankcase Heater Capacity {W}",
			"    10,                      !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
			"    DefrostTempCurve,        !- Defrost Energy Input Ratio Function of Temperature Curve Name",
			"    4.4,                     !- Maximum Outdoor Dry-Bulb Temperature for Defrost Operation {C}",
			"    ReverseCycle,            !- Defrost Strategy",
			"    OnDemand,                !- Defrost Control",
			"    0.087,                   !- Defrost Time Period Fraction",
			"    0,                       !- Resistive Defrost Heater Capacity {W}",
			"    No,                      !- Apply Part Load Fraction to Speeds Greater than 1",
			"    Electricity,             !- Fuel Type",
			"    ,                        !- Region number for Calculating HSPF",
			"    2,                       !- Number of Speeds",
			"    53500,                   !- Speed 1 Gross Rated Heating Capacity {W}",
			"    2.85,                    !- Speed 1 Gross Rated Heating COP {W/W}",
			"    3.209,                   !- Speed 1 Rated Air Flow Rate {m3/s}",
			"    ,                        !- Speed 1 Rated Supply Air Fan Power Per Volume Flow Rate {W/(m3/s)}",
			"    HeatingTempCurve,        !- Speed 1 Heating Capacity Function of Temperature Curve Name",
			"    HeatingFlowCurve,        !- Speed 1 Heating Capacity Function of Flow Fraction Curve Name",
			"    HeatingEIRTempCurve,     !- Speed 1 Energy Input Ratio Function of Temperature Curve Name",
			"    EIRFlowCurve,            !- Speed 1 Energy Input Ratio Function of Flow Fraction Curve Name",
			"    PLFCurve,                !- Speed 1 Part Load Fraction Correlation Curve Name",
			"    0.0001,                  !- Speed 1 Rated Waste Heat Fraction of Power Input {dimensionless}",
			"    WasteHeatTempCurve,      !- Speed 1 Waste Heat Function of Temperature Curve Name",
			"    59587.2,                 !- Speed 2 Gross Rated Heating Capacity {W}",
			"    2.85,                    !- Speed 2 Gross Rated Heating COP {W/W}",
			"    3.209,                   !- Speed 2 Rated Air Flow Rate {m3/s}",
			"    ,                        !- Speed 2 Rated Supply Air Fan Power Per Volume Flow Rate {W/(m3/s)}",
			"    HeatingTempCurve,        !- Speed 2 Heating Capacity Function of Temperature Curve Name",
			"    HeatingFlowCurve,        !- Speed 2 Heating Capacity Function of Flow Fraction Curve Name",
			"    HeatingEIRTempCurve,     !- Speed 2 Energy Input Ratio Function of Temperature Curve Name",
			"    EIRFlowCurve,            !- Speed 2 Energy Input Ratio Function of Flow Fraction Curve Name",
			"    PLFCurve,                !- Speed 2 Part Load Fraction Correlation Curve Name",
			"    0.0001,                  !- Speed 2 Rated Waste Heat Fraction of Power Input {dimensionless}",
			"    WasteHeatTempCurve;      !- Speed 2 Waste Heat Function of Temperature Curve Name",

			"  Coil:Heating:DX:MultiSpeed,",
			"    AC25Heating,             !- Name",
			"    AC-25 sched,             !- Availability Schedule Name",
			"    AC-25 HC inlet node,     !- Air Inlet Node Name",
			"    AC-25 RHC inlet node,    !- Air Outlet Node Name",
			"    -8,                      !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
			"    ,                        !- Outdoor Dry-Bulb Temperature to Turn On Compressor {C}",
			"    0,                       !- Crankcase Heater Capacity {W}",
			"    10,                      !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
			"    DefrostTempCurve,        !- Defrost Energy Input Ratio Function of Temperature Curve Name",
			"    4.4,                     !- Maximum Outdoor Dry-Bulb Temperature for Defrost Operation {C}",
			"    ReverseCycle,            !- Defrost Strategy",
			"    OnDemand,                !- Defrost Control",
			"    0.087,                   !- Defrost Time Period Fraction",
			"    0,                       !- Resistive Defrost Heater Capacity {W}",
			"    No,                      !- Apply Part Load Fraction to Speeds Greater than 1",
			"    Electricity,             !- Fuel Type",
			"    ,                        !- Region number for Calculating HSPF",
			"    2,                       !- Number of Speeds",
			"    53500,                   !- Speed 1 Gross Rated Heating Capacity {W}",
			"    2.85,                    !- Speed 1 Gross Rated Heating COP {W/W}",
			"    3.209,                   !- Speed 1 Rated Air Flow Rate {m3/s}",
			"    ,                        !- Speed 1 Rated Supply Air Fan Power Per Volume Flow Rate {W/(m3/s)}",
			"    HeatingTempCurve,        !- Speed 1 Heating Capacity Function of Temperature Curve Name",
			"    HeatingFlowCurve,        !- Speed 1 Heating Capacity Function of Flow Fraction Curve Name",
			"    HeatingEIRTempCurve,     !- Speed 1 Energy Input Ratio Function of Temperature Curve Name",
			"    EIRFlowCurve,            !- Speed 1 Energy Input Ratio Function of Flow Fraction Curve Name",
			"    PLFCurve,                !- Speed 1 Part Load Fraction Correlation Curve Name",
			"    0.0001,                  !- Speed 1 Rated Waste Heat Fraction of Power Input {dimensionless}",
			"    WasteHeatTempCurve,      !- Speed 1 Waste Heat Function of Temperature Curve Name",
			"    59587.2,                 !- Speed 2 Gross Rated Heating Capacity {W}",
			"    2.85,                    !- Speed 2 Gross Rated Heating COP {W/W}",
			"    3.209,                   !- Speed 2 Rated Air Flow Rate {m3/s}",
			"    ,                        !- Speed 2 Rated Supply Air Fan Power Per Volume Flow Rate {W/(m3/s)}",
			"    HeatingTempCurve,        !- Speed 2 Heating Capacity Function of Temperature Curve Name",
			"    HeatingFlowCurve,        !- Speed 2 Heating Capacity Function of Flow Fraction Curve Name",
			"    HeatingEIRTempCurve,     !- Speed 2 Energy Input Ratio Function of Temperature Curve Name",
			"    EIRFlowCurve,            !- Speed 2 Energy Input Ratio Function of Flow Fraction Curve Name",
			"    PLFCurve,                !- Speed 2 Part Load Fraction Correlation Curve Name",
			"    0.0001,                  !- Speed 2 Rated Waste Heat Fraction of Power Input {dimensionless}",
			"    WasteHeatTempCurve;      !- Speed 2 Waste Heat Function of Temperature Curve Name",

			"!-   ===========  ALL OBJECTS IN CLASS: AIRLOOPHVAC:UNITARYHEATPUMP:AIRTOAIR:MULTISPEED ===========",
			"  AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed,",
			"    AC-24 heat pump,         !- Name",
			"    AC-24sched,              !- Availability Schedule Name",
			"    AC-24 SF inlet air node, !- Air Inlet Node Name",
			"    AC-24 airloop outlet node,  !- Air Outlet Node Name",
			"    401,                     !- Controlling Zone or Thermostat Location",
			"    Fan:OnOff,               !- Supply Air Fan Object Type",
			"    AC24_Fan,                !- Supply Air Fan Name",
			"    BlowThrough,             !- Supply Air Fan Placement",
			"    AC-24sched,              !- Supply Air Fan Operating Mode Schedule Name",
			"    Coil:Heating:DX:MultiSpeed,  !- Heating Coil Object Type",
			"    AC24Heating,             !- Heating Coil Name",
			"    -8,                      !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
			"    Coil:Cooling:DX:MultiSpeed,  !- Cooling Coil Object Type",
			"    AC24_cooling,            !- Cooling Coil Name",
			"    Coil:Heating:Electric,   !- Supplemental Heating Coil Object Type",
			"    AC24ElecHeater,          !- Supplemental Heating Coil Name",
			"    45,                      !- Maximum Supply Air Temperature from Supplemental Heater {C}",
			"    15,                      !- Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation {C}",
			"    0,                       !- Auxiliary On-Cycle Electric Power {W}",
			"    0,                       !- Auxiliary Off-Cycle Electric Power {W}",
			"    0,                       !- Design Heat Recovery Water Flow Rate {m3/s}",
			"    80,                      !- Maximum Temperature for Heat Recovery {C}",
			"    ,                        !- Heat Recovery Water Inlet Node Name",
			"    ,                        !- Heat Recovery Water Outlet Node Name",
			"    3.209,                   !- No Load Supply Air Flow Rate {m3/s}",
			"    2,                       !- Number of Speeds for Heating",
			"    2,                       !- Number of Speeds for Cooling",
			"    3.209,                   !- Heating Speed 1 Supply Air Flow Rate {m3/s}",
			"    3.209,                   !- Heating Speed 2 Supply Air Flow Rate {m3/s}",
			"    ,                        !- Heating Speed 3 Supply Air Flow Rate {m3/s}",
			"    ,                        !- Heating Speed 4 Supply Air Flow Rate {m3/s}",
			"    3.209,                   !- Cooling Speed 1 Supply Air Flow Rate {m3/s}",
			"    3.209;                   !- Cooling Speed 2 Supply Air Flow Rate {m3/s}",

			"  AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed,",
			"    AC-25 heat pump,         !- Name",
			"    AC-25 sched,             !- Availability Schedule Name",
			"    AC-25 SF inlet air node, !- Air Inlet Node Name",
			"    AC-25 airloop outlet node,  !- Air Outlet Node Name",
			"    402,                     !- Controlling Zone or Thermostat Location",
			"    Fan:OnOff,               !- Supply Air Fan Object Type",
			"    AC25_Fan,                !- Supply Air Fan Name",
			"    BlowThrough,             !- Supply Air Fan Placement",
			"    AC-25 sched,             !- Supply Air Fan Operating Mode Schedule Name",
			"    Coil:Heating:DX:MultiSpeed,  !- Heating Coil Object Type",
			"    AC25Heating,             !- Heating Coil Name",
			"    -8,                      !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
			"    Coil:Cooling:DX:MultiSpeed,  !- Cooling Coil Object Type",
			"    AC25_cooling,            !- Cooling Coil Name",
			"    Coil:Heating:Electric,   !- Supplemental Heating Coil Object Type",
			"    AC25ElecHeater,          !- Supplemental Heating Coil Name",
			"    45,                      !- Maximum Supply Air Temperature from Supplemental Heater {C}",
			"    15,                      !- Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation {C}",
			"    0,                       !- Auxiliary On-Cycle Electric Power {W}",
			"    0,                       !- Auxiliary Off-Cycle Electric Power {W}",
			"    0,                       !- Design Heat Recovery Water Flow Rate {m3/s}",
			"    80,                      !- Maximum Temperature for Heat Recovery {C}",
			"    ,                        !- Heat Recovery Water Inlet Node Name",
			"    ,                        !- Heat Recovery Water Outlet Node Name",
			"    3.209,                   !- No Load Supply Air Flow Rate {m3/s}",
			"    2,                       !- Number of Speeds for Heating",
			"    2,                       !- Number of Speeds for Cooling",
			"    3.209,                   !- Heating Speed 1 Supply Air Flow Rate {m3/s}",
			"    3.209,                   !- Heating Speed 2 Supply Air Flow Rate {m3/s}",
			"    ,                        !- Heating Speed 3 Supply Air Flow Rate {m3/s}",
			"    ,                        !- Heating Speed 4 Supply Air Flow Rate {m3/s}",
			"    3.209,                   !- Cooling Speed 1 Supply Air Flow Rate {m3/s}",
			"    3.209;                   !- Cooling Speed 2 Supply Air Flow Rate {m3/s}",
			
			"  Timestep,6;",
			
			"  Zone,",
			"    402,                     !- Name",
			"    48.33,                   !- Direction of Relative North {deg}",
			"    -21.306405,              !- X Origin {m}",
			"    46.910971,               !- Y Origin {m}",
			"    0.0,                     !- Z Origin {m}",
			"    ,                        !- Type",
			"    1;                       !- Multiplier",

			"  Zone,",
			"    401,                     !- Name",
			"    48.33,                   !- Direction of Relative North {deg}",
			"    15.002524,               !- X Origin {m}",
			"    -50.24491,               !- Y Origin {m}",
			"    0.0,                     !- Z Origin {m}",
			"    ,                        !- Type",
			"    1;                       !- Multiplier",

			"  Schedule:Compact,",
			"    ActSchd,                 !- Name",
			"    Any Number,              !- Schedule Type Limits Name",
			"    Through: 12/31,          !- Field 1",
			"    For: AllDays,            !- Field 2",
			"    Until: 24:00,118;        !- Field 3",

			"  Schedule:Compact,",
			"    Space temp SP,           !- Name",
			"    Temperature,             !- Schedule Type Limits Name",
			"    Through: 12/31,          !- Field 1",
			"    For: AllDays,            !- Field 2",
			"    Until: 24:00,22.00;      !- Field 3",

			"  Schedule:Compact,",
			"    Zone Control Type Sched, !- Name",
			"    Control Type,            !- Schedule Type Limits Name",
			"    Through: 12/31,          !- Field 1",
			"    For: AllDays,            !- Field 2",
			"    Until: 24:00,4.00;       !- Field 3",

			"  Schedule:Compact,",
			"    Fan_schd,                !- Name",
			"    Fraction,                !- Schedule Type Limits Name",
			"    Through: 12/31,          !- Field 1",
			"    For: Alldays,            !- Field 2",
			"    Until: 24:00,1;          !- Field 17",

			"  Schedule:Compact,",
			"    401HeatingSP,            !- Name",
			"    Temperature,             !- Schedule Type Limits Name",
			"    Through: 12/31,          !- Field 1",
			"    For: Alldays,            !- Field 2",
			"    Until: 24:00,21.11;      !- Field 7",

			"  Schedule:Compact,",
			"    401CoolingSP,            !- Name",
			"    Temperature,             !- Schedule Type Limits Name",
			"    Through: 12/31,          !- Field 1",
			"    For: Alldays,            !- Field 2",
			"    Until: 24:00,22.2;       !- Field 7",

			"  Schedule:Compact,",
			"    HeatP fan cyc_sched,     !- Name",
			"    Fraction,                !- Schedule Type Limits Name",
			"    Through: 12/31,          !- Field 1",
			"    For: Alldays,            !- Field 2",
			"    Until: 24:00,0;          !- Field 10",

			"  Schedule:Compact,",
			"    AC-28 sched,             !- Name",
			"    Fraction,                !- Schedule Type Limits Name",
			"    Through: 12/31,          !- Field 1",
			"    For: Alldays,            !- Field 2",
			"    Until: 24:00,1;          !- Field 10",

			"  Schedule:Compact,",
			"    AC-25 sched,             !- Name",
			"    Fraction,                !- Schedule Type Limits Name",
			"    Through: 12/31,          !- Field 1",
			"    For: Alldays,            !- Field 2",
			"    Until: 24:00,1.00;       !- Field 3",

			"  Schedule:Compact,",
			"    Econ sched,              !- Name",
			"    Fraction,                !- Schedule Type Limits Name",
			"    Through: 12/31,          !- Field 1",
			"    For: Alldays,            !- Field 2",
			"    Until: 24:00,1.00;       !- Field 3",

			"  Schedule:Compact,",
			"    HeatingPlant ON,         !- Name",
			"    Fraction,                !- Schedule Type Limits Name",
			"    Through: 12/31,          !- Field 1",
			"    For: Alldays,            !- Field 2",
			"    Until: 24:00,1.00;       !- Field 3",

			"  Schedule:Compact,",
			"    INFIL_QUARTER_ON_SCH,    !- Name",
			"    Fraction,                !- Schedule Type Limits Name",
			"    Through: 12/31,          !- Field 1",
			"    For: Alldays,            !- Field 2",
			"    Until: 24:00,1.00;       !- Field 3",

			"  Schedule:Compact,",
			"    402HeatingSP,            !- Name",
			"    Temperature,             !- Schedule Type Limits Name",
			"    Through: 12/31,          !- Field 1",
			"    For: Alldays,            !- Field 2",
			"    Until: 24:00,21.11;      !- Field 7",

			"  Schedule:Compact,",
			"    402CoolingSP,            !- Name",
			"    Temperature,             !- Schedule Type Limits Name",
			"    Through: 12/31,          !- Field 1",
			"    For: Alldays,            !- Field 2",
			"    Until: 24:00,22.2;       !- Field 7",

			"  Schedule:Compact,",
			"    HeatingSetpoints,        !- Name",
			"    Temperature,             !- Schedule Type Limits Name",
			"    Through: 12/31,          !- Field 1",
			"    For: Alldays,            !- Field 2",
			"    Until: 24:00,21.11;      !- Field 7",

			"  Schedule:Compact,",
			"    CoolingSetpoints,        !- Name",
			"    Temperature,             !- Schedule Type Limits Name",
			"    Through: 12/31,          !- Field 1",
			"    For: Alldays,            !- Field 2",
			"    Until: 24:00,22.2;       !- Field 7",

			"  Schedule:Compact,",
			"    AC-24sched,              !- Name",
			"    Fraction,                !- Schedule Type Limits Name",
			"    Through: 12/31,          !- Field 1",
			"	 For: Alldays,			  !- Field 2",
			"    Until: 24:00,1;          !- Field 3",

			"!-   ===========  ALL OBJECTS IN CLASS: CONTROLLER:OUTDOORAIR ===========",

			"  Controller:OutdoorAir,",
			"    AC-24 OA Controller,     !- Name",
			"    AC-24 EA node,           !- Relief Air Outlet Node Name",
			"    AC-24 airloop inlet node,!- Return Air Node Name",
			"    AC-24 SF inlet air node, !- Mixed Air Node Name",
			"    AC-24 OA inlet node,     !- Actuator Node Name",
			"    0.481,                   !- Minimum Outdoor Air Flow Rate {m3/s}",
			"    0.481,                   !- Maximum Outdoor Air Flow Rate {m3/s}",
			"    NoEconomizer,            !- Economizer Control Type",
			"    ,                        !- Economizer Control Action Type",
			"    ,                        !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
			"    ,                        !- Economizer Maximum Limit Enthalpy {J/kg}",
			"    ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
			"    ,                        !- Electronic Enthalpy Limit Curve Name",
			"    ,                        !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
			"    ,                        !- Lockout Type",
			"    ;                        !- Minimum Limit Type",

			"  Controller:OutdoorAir,",
			"    AC-25 OA Controller,     !- Name",
			"    AC-25 EA node,           !- Relief Air Outlet Node Name",
			"    AC-25 airloop inlet node,!- Return Air Node Name",
			"    AC-25 SF inlet air node, !- Mixed Air Node Name",
			"    AC-25 OA inlet node,     !- Actuator Node Name",
			"    0.481,                   !- Minimum Outdoor Air Flow Rate {m3/s}",
			"    0.481,                   !- Maximum Outdoor Air Flow Rate {m3/s}",
			"    NoEconomizer,            !- Economizer Control Type",
			"    ,                        !- Economizer Control Action Type",
			"    ,                        !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
			"    ,                        !- Economizer Maximum Limit Enthalpy {J/kg}",
			"    ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
			"    ,                        !- Electronic Enthalpy Limit Curve Name",
			"    ,                        !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
			"    ,                        !- Lockout Type",
			"    ;                        !- Minimum Limit Type",

			"!-   ===========  ALL OBJECTS IN CLASS: AIRLOOPHVAC:CONTROLLERLIST ===========",

			"  AirLoopHVAC:ControllerList,",
			"    Z401OA controller list,  !- Name",
			"    Controller:OutdoorAir,   !- Controller 1 Object Type",
			"    AC-24 OA Controller;     !- Controller 1 Name",

			"  AirLoopHVAC:ControllerList,",
			"    Z402OA controller list,  !- Name",
			"    Controller:OutdoorAir,   !- Controller 1 Object Type",
			"    AC-25 OA Controller;     !- Controller 1 Name",

			"!-   ===========  ALL OBJECTS IN CLASS: AIRLOOPHVAC ===========",

			"  AirLoopHVAC,",
			"    Z401 airloop,            !- Name",
			"    ,                        !- Controller List Name",
			"    AC24,                    !- Availability Manager List Name",
			"    3.209,                   !- Design Supply Air Flow Rate {m3/s}",
			"    Z401 branch list,        !- Branch List Name",
			"    ,                        !- Connector List Name",
			"    AC-24 airloop inlet node,!- Supply Side Inlet Node Name",
			"    Z401 RA node,            !- Demand Side Outlet Node Name",
			"    Z401 splitter inlet,     !- Demand Side Inlet Node Names",
			"    AC-24 airloop outlet node;  !- Supply Side Outlet Node Names",

			"  AirLoopHVAC,",
			"    Z402 airloop,            !- Name",
			"    ,                        !- Controller List Name",
			"    AC25,                    !- Availability Manager List Name",
			"    3.209,                   !- Design Supply Air Flow Rate {m3/s}",
			"    Z402 branch list,        !- Branch List Name",
			"    ,                        !- Connector List Name",
			"    AC-25 airloop inlet node,!- Supply Side Inlet Node Name",
			"    Z402 RA node,            !- Demand Side Outlet Node Name",
			"    Z402 splitter inlet,     !- Demand Side Inlet Node Names",
			"    AC-25 airloop outlet node;  !- Supply Side Outlet Node Names",

			"!-   ===========  ALL OBJECTS IN CLASS: AIRLOOPHVAC:OUTDOORAIRSYSTEM:EQUIPMENTLIST ===========",

			"  AirLoopHVAC:OutdoorAirSystem:EquipmentList,",
			"    Z401OA equip list,       !- Name",
			"    OutdoorAir:Mixer,        !- Component 1 Object Type",
			"    AC-24 OA intake;         !- Component 1 Name",

			"  AirLoopHVAC:OutdoorAirSystem:EquipmentList,",
			"    Z402OA equip list,       !- Name",
			"    OutdoorAir:Mixer,        !- Component 1 Object Type",
			"    AC-25 OA intake;         !- Component 1 Name",

			"!-   ===========  ALL OBJECTS IN CLASS: AIRLOOPHVAC:OUTDOORAIRSYSTEM ===========",

			"  AirLoopHVAC:OutdoorAirSystem,",
			"    Z401 OA Sys,             !- Name",
			"    Z401OA controller list,  !- Controller List Name",
			"    Z401OA equip list;       !- Outdoor Air Equipment List Name",

			"  AirLoopHVAC:OutdoorAirSystem,",
			"    Z402 OA Sys,             !- Name",
			"    Z402OA controller list,  !- Controller List Name",
			"    Z402OA equip list;       !- Outdoor Air Equipment List Name",

			"!-   ===========  ALL OBJECTS IN CLASS: OUTDOORAIR:MIXER ===========",

			"  OutdoorAir:Mixer,",
			"    AC-24 OA intake,         !- Name",
			"    AC-24 SF inlet air node, !- Mixed Air Node Name",
			"    AC-24 OA inlet node,     !- Outdoor Air Stream Node Name",
			"    AC-24 EA node,           !- Relief Air Stream Node Name",
			"    AC-24 airloop inlet node;!- Return Air Stream Node Name",

			"  OutdoorAir:Mixer,",
			"    AC-25 OA intake,         !- Name",
			"    AC-25 SF inlet air node, !- Mixed Air Node Name",
			"    AC-25 OA inlet node,     !- Outdoor Air Stream Node Name",
			"    AC-25 EA node,           !- Relief Air Stream Node Name",
			"    AC-25 airloop inlet node;!- Return Air Stream Node Name",

			"!-   ===========  ALL OBJECTS IN CLASS: AIRLOOPHVAC:ZONESPLITTER ===========",

			"  AirLoopHVAC:ZoneSplitter,",
			"    Z401 SA splitter,        !- Name",
			"    Z401 splitter inlet,     !- Inlet Node Name",
			"    Z401 zone inlet;         !- Outlet 1 Node Name",

			"  AirLoopHVAC:ZoneSplitter,",
			"    Z402 SA splitter,        !- Name",
			"    Z402 splitter inlet,     !- Inlet Node Name",
			"    Z402 zone inlet;         !- Outlet 1 Node Name",

			"!-   ===========  ALL OBJECTS IN CLASS: AIRLOOPHVAC:SUPPLYPATH ===========",

			"  AirLoopHVAC:SupplyPath,",
			"    Z401SupplyPath,          !- Name",
			"    Z401 splitter inlet,     !- Supply Air Path Inlet Node Name",
			"    AirLoopHVAC:ZoneSplitter,!- Component 1 Object Type",
			"    Z401 SA splitter;        !- Component 1 Name",

			"  AirLoopHVAC:SupplyPath,",
			"    Z402SupplyPath,          !- Name",
			"    Z402 splitter inlet,     !- Supply Air Path Inlet Node Name",
			"    AirLoopHVAC:ZoneSplitter,!- Component 1 Object Type",
			"    Z402 SA splitter;        !- Component 1 Name",

			"!-   ===========  ALL OBJECTS IN CLASS: AIRLOOPHVAC:ZONEMIXER ===========",

			"  AirLoopHVAC:ZoneMixer,",
			"    Z401 RA mixer,           !- Name",
			"    Z401 RA node,            !- Outlet Node Name",
			"    Z401 outlet node;        !- Inlet 1 Node Name",

			"  AirLoopHVAC:ZoneMixer,",
			"    Z402 RA mixer,           !- Name",
			"    Z402 RA node,            !- Outlet Node Name",
			"    Z402 outlet node;        !- Inlet 1 Node Name",

			"!-   ===========  ALL OBJECTS IN CLASS: AIRLOOPHVAC:RETURNPATH ===========",

			"  AirLoopHVAC:ReturnPath,",
			"    Z401RApath,              !- Name",
			"    Z401 RA node,            !- Return Air Path Outlet Node Name",
			"    AirLoopHVAC:ZoneMixer,   !- Component 1 Object Type",
			"    Z401 RA mixer;           !- Component 1 Name",

			"  AirLoopHVAC:ReturnPath,",
			"    Z402RApath,              !- Name",
			"    Z402 RA node,            !- Return Air Path Outlet Node Name",
			"    AirLoopHVAC:ZoneMixer,   !- Component 1 Object Type",
			"    Z402 RA mixer;           !- Component 1 Name",

			"!-   ===========  ALL OBJECTS IN CLASS: BRANCH ===========",

			"  Branch,",
			"    Z401 main branch,        !- Name",
			"    3.209,                   !- Maximum Flow Rate {m3/s}",
			"    ,                        !- Pressure Drop Curve Name",
			"    AirLoopHVAC:OutdoorAirSystem,  !- Component 1 Object Type",
			"    Z401 OA Sys,             !- Component 1 Name",
			"    AC-24 airloop inlet node,!- Component 1 Inlet Node Name",
			"    AC-24 SF inlet air node, !- Component 1 Outlet Node Name",
			"    Passive,                 !- Component 1 Branch Control Type",
			"    AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed,  !- Component 2 Object Type",
			"    AC-24 heat pump,         !- Component 2 Name",
			"    AC-24 SF inlet air node, !- Component 2 Inlet Node Name",
			"    AC-24 airloop outlet node,  !- Component 2 Outlet Node Name",
			"    Active;                  !- Component 2 Branch Control Type",

			"  Branch,",
			"    Z402 main branch,        !- Name",
			"    3.209,                   !- Maximum Flow Rate {m3/s}",
			"    ,                        !- Pressure Drop Curve Name",
			"    AirLoopHVAC:OutdoorAirSystem,  !- Component 1 Object Type",
			"    Z402 OA Sys,             !- Component 1 Name",
			"    AC-25 airloop inlet node,!- Component 1 Inlet Node Name",
			"    AC-25 SF inlet air node, !- Component 1 Outlet Node Name",
			"    Passive,                 !- Component 1 Branch Control Type",
			"    AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed,  !- Component 2 Object Type",
			"    AC-25 heat pump,         !- Component 2 Name",
			"    AC-25 SF inlet air node, !- Component 2 Inlet Node Name",
			"    AC-25 airloop outlet node,  !- Component 2 Outlet Node Name",
			"    Active;                  !- Component 2 Branch Control Type",

			"!-   ===========  ALL OBJECTS IN CLASS: BRANCHLIST ===========",

			"  BranchList,",
			"    Z401 branch list,        !- Name",
			"    Z401 main branch;        !- Branch 1 Name",

			"  BranchList,",
			"    Z402 branch list,        !- Name",
			"    Z402 main branch;        !- Branch 1 Name",

			"!-   ===========  ALL OBJECTS IN CLASS: NODELIST ===========",

			"  NodeList,",
			"    AC-24 OA intake list,    !- Name",
			"    AC-24 OA inlet node;     !- Node 1 Name",

			"  NodeList,",
			"    AC-25 OA intake list,    !- Name",
			"    AC-25 OA inlet node;     !- Node 1 Name",

			"!-   ===========  ALL OBJECTS IN CLASS: OUTDOORAIR:NODELIST ===========",

			"  OutdoorAir:NodeList,",
			"    AC-24 OA Intake list;    !- Node or NodeList Name 1",

			"  OutdoorAir:NodeList,",
			"    AC-25 OA Intake list;    !- Node or NodeList Name 1",

			"!-   ===========  ALL OBJECTS IN CLASS: AVAILABILITYMANAGER:SCHEDULED ===========",

			"  AvailabilityManager:Scheduled,",
			"    AC24 OnOff,              !- Name",
			"    AC-24sched;              !- Schedule Name",

			"  AvailabilityManager:Scheduled,",
			"    AC25 OnOff,              !- Name",
			"    AC-25 sched;             !- Schedule Name",

			"!-   ===========  ALL OBJECTS IN CLASS: AVAILABILITYMANAGERASSIGNMENTLIST ===========",

			"  AvailabilityManagerAssignmentList,",
			"    AC24,                    !- Name",
			"    AvailabilityManager:Scheduled,  !- Availability Manager 1 Object Type",
			"    AC24 OnOff;              !- Availability Manager 1 Name",

			"  AvailabilityManagerAssignmentList,",
			"    AC25,                    !- Name",
			"    AvailabilityManager:Scheduled,  !- Availability Manager 1 Object Type",
			"    AC25 OnOff;              !- Availability Manager 1 Name",

			"!-   ===========  ALL OBJECTS IN CLASS: CURVE:QUADRATIC ===========",


			"!-   ===========  ALL OBJECTS IN CLASS: CURVE:QUADRATIC ===========",

			"  Curve:Quadratic,",
			"    CoolingFlowCurve,        !- Name",
			"    1,                       !- Coefficient1 Constant",
			"    0,                       !- Coefficient2 x",
			"    0,                       !- Coefficient3 x**2",
			"    0.1,                     !- Minimum Value of x",
			"    2;                       !- Maximum Value of x",

			"  Curve:Quadratic,",
			"    EIRFlowCurve,            !- Name",
			"    1,                       !- Coefficient1 Constant",
			"    0,                       !- Coefficient2 x",
			"    0,                       !- Coefficient3 x**2",
			"    0.1,                     !- Minimum Value of x",
			"    2;                       !- Maximum Value of x",

			"  Curve:Quadratic,",
			"    PLFCurve,                !- Name",
			"    0.75,                    !- Coefficient1 Constant",
			"    0.25,                    !- Coefficient2 x",
			"    0,                       !- Coefficient3 x**2",
			"    0.1,                     !- Minimum Value of x",
			"    1;                       !- Maximum Value of x",

			"  Curve:Quadratic,",
			"    HeatingFlowCurve,        !- Name",
			"    1,                       !- Coefficient1 Constant",
			"    0,                       !- Coefficient2 x",
			"    0,                       !- Coefficient3 x**2",
			"    0.1,                     !- Minimum Value of x",
			"    2;                       !- Maximum Value of x",

			"  Curve:Quadratic,",
			"    BLCoolingFlowCurve,      !- Name",
			"    0.86187,                 !- Coefficient1 Constant",
			"    0.14853,                 !- Coefficient2 x",
			"    -.009899,                !- Coefficient3 x**2",
			"    0.1,                     !- Minimum Value of x",
			"    2;                       !- Maximum Value of x",

			"  Curve:Quadratic,",
			"    BLEIRFlowCurve,          !- Name",
			"    1.1085,                  !- Coefficient1 Constant",
			"    -.1266,                  !- Coefficient2 x",
			"    0.017998,                !- Coefficient3 x**2",
			"    0.1,                     !- Minimum Value of x",
			"    2;                       !- Maximum Value of x",

			"!-   ===========  ALL OBJECTS IN CLASS: CURVE:BIQUADRATIC ===========",

			"  Curve:Biquadratic,",
			"    CoolingTempCurve,        !- Name",
			"    0.66896,                 !- Coefficient1 Constant",
			"    0.023936,                !- Coefficient2 x",
			"    -.00015091,              !- Coefficient3 x**2",
			"    -.00128818,              !- Coefficient4 y",
			"    -.000168897,             !- Coefficient5 y**2",
			"    0.000258167,             !- Coefficient6 x*y",
			"    0,                       !- Minimum Value of x",
			"    20,                      !- Maximum Value of x",
			"    0,                       !- Minimum Value of y",
			"    40;                      !- Maximum Value of y",

			"  Curve:Biquadratic,",
			"    EIRTempCurve,            !- Name",
			"    0.574095,                !- Coefficient1 Constant",
			"    -.003463,                !- Coefficient2 x",
			"    0.00029161,              !- Coefficient3 x**2",
			"    0.012714,                !- Coefficient4 y",
			"    0.00034595,              !- Coefficient5 y**2",
			"    -.000714,                !- Coefficient6 x*y",
			"    0,                       !- Minimum Value of x",
			"    20,                      !- Maximum Value of x",
			"    0,                       !- Minimum Value of y",
			"    40;                      !- Maximum Value of y",

			"  Curve:Biquadratic,",
			"    WasteHeatFrac,           !- Name",
			"    0,                       !- Coefficient1 Constant",
			"    0,                       !- Coefficient2 x",
			"    0,                       !- Coefficient3 x**2",
			"    0,                       !- Coefficient4 y",
			"    0,                       !- Coefficient5 y**2",
			"    0,                       !- Coefficient6 x*y",
			"    0,                       !- Minimum Value of x",
			"    50,                      !- Maximum Value of x",
			"    0,                       !- Minimum Value of y",
			"    50;                      !- Maximum Value of y",

			"  Curve:Biquadratic,",
			"    DefrostTempCurve,        !- Name",
			"    0.43264,                 !- Coefficient1 Constant",
			"    0.0013974,               !- Coefficient2 x",
			"    0.00026958,              !- Coefficient3 x**2",
			"    -.02639,                 !- Coefficient4 y",
			"    -.000142377,             !- Coefficient5 y**2",
			"    -.0016066,               !- Coefficient6 x*y",
			"    -10,                     !- Minimum Value of x",
			"    20,                      !- Maximum Value of x",
			"    -10,                     !- Minimum Value of y",
			"    50;                      !- Maximum Value of y",

			"  Curve:Biquadratic,",
			"    HeatingTempCurve,        !- Name",
			"    0.830527,                !- Coefficient1 Constant",
			"    -.00086702,              !- Coefficient2 x",
			"    -.00016316,              !- Coefficient3 x**2",
			"    0.022539,                !- Coefficient4 y",
			"    0.000142796,             !- Coefficient5 y**2",
			"    0.000046064,             !- Coefficient6 x*y",
			"    -10,                     !- Minimum Value of x",
			"    50,                      !- Maximum Value of x",
			"    -10,                     !- Minimum Value of y",
			"    20;                      !- Maximum Value of y",

			"  Curve:Biquadratic,",
			"    HeatingEIRTempCurve,     !- Name",
			"    0.9111,                  !- Coefficient1 Constant",
			"    -.0106,                  !- Coefficient2 x",
			"    0.000879,                !- Coefficient3 x**2",
			"    0.0051,                  !- Coefficient4 y",
			"    0.00119,                 !- Coefficient5 y**2",
			"    -.00147,                 !- Coefficient6 x*y",
			"    -10,                     !- Minimum Value of x",
			"    50,                      !- Maximum Value of x",
			"    -10,                     !- Minimum Value of y",
			"    20;                      !- Maximum Value of y",

			"  Curve:Biquadratic,",
			"    WasteHeatTempCurve,      !- Name",
			"    1,                       !- Coefficient1 Constant",
			"    0,                       !- Coefficient2 x",
			"    0,                       !- Coefficient3 x**2",
			"    0,                       !- Coefficient4 y",
			"    0,                       !- Coefficient5 y**2",
			"    0,                       !- Coefficient6 x*y",
			"    -10,                     !- Minimum Value of x",
			"    50,                      !- Maximum Value of x",
			"    -10,                     !- Minimum Value of y",
			"    50;                      !- Maximum Value of y",

			"  Curve:Biquadratic,",
			"    AC28HeatingTempCurve,    !- Name",
			"    0.9427,                  !- Coefficient1 Constant",
			"    -.005897,                !- Coefficient2 x",
			"    -.00005806,              !- Coefficient3 x**2",
			"    0.02442,                 !- Coefficient4 y",
			"    0.0001265,               !- Coefficient5 y**2",
			"    -.000006653,             !- Coefficient6 x*y",
			"    -10,                     !- Minimum Value of x",
			"    50,                      !- Maximum Value of x",
			"    -10,                     !- Minimum Value of y",
			"    50;                      !- Maximum Value of y",

			"  Curve:Biquadratic,",
			"    AC28HeatingEIRCurve,     !- Name",
			"    0.84489,                 !- Coefficient1 Constant",
			"    -.002944,                !- Coefficient2 x",
			"    0.0006633,               !- Coefficient3 x**2",
			"    0.0042478,               !- Coefficient4 y",
			"    0.00098998,              !- Coefficient5 y**2",
			"    -.0012759,               !- Coefficient6 x*y",
			"    -10,                     !- Minimum Value of x",
			"    50,                      !- Maximum Value of x",
			"    -10,                     !- Minimum Value of y",
			"    50;                      !- Maximum Value of y",

			"  Curve:Biquadratic,",
			"    AC28DefrostTempCurve,    !- Name",
			"    0.65186,                 !- Coefficient1 Constant",
			"    -.0090539,               !- Coefficient2 x",
			"    0.00032925,              !- Coefficient3 x**2",
			"    0.001465,                !- Coefficient4 y",
			"    0.00043463,              !- Coefficient5 y**2",
			"    -.0031187,               !- Coefficient6 x*y",
			"    -10,                     !- Minimum Value of x",
			"    50,                      !- Maximum Value of x",
			"    -10,                     !- Minimum Value of y",
			"    50;                      !- Maximum Value of y",

			"  Curve:Biquadratic,",
			"    AC28CoolingTempCurve,    !- Name",
			"    1.68985,                 !- Coefficient1 Constant",
			"    -.07506,                 !- Coefficient2 x",
			"    0.003149,                !- Coefficient3 x**2",
			"    -.00541,                 !- Coefficient4 y",
			"    0.00003154,              !- Coefficient5 y**2",
			"    -.0004042,               !- Coefficient6 x*y",
			"    -10,                     !- Minimum Value of x",
			"    50,                      !- Maximum Value of x",
			"    -10,                     !- Minimum Value of y",
			"    50;                      !- Maximum Value of y",

			"  Curve:Biquadratic,",
			"    AC28EIRTempCurve,        !- Name",
			"    0.05573,                 !- Coefficient1 Constant",
			"    0.055223,                !- Coefficient2 x",
			"    -.0018377,               !- Coefficient3 x**2",
			"    0.012791,                !- Coefficient4 y",
			"    0.00019079,              !- Coefficient5 y**2",
			"    -.00016265,              !- Coefficient6 x*y",
			"    -10,                     !- Minimum Value of x",
			"    50,                      !- Maximum Value of x",
			"    -10,                     !- Minimum Value of y",
			"    50;                      !- Maximum Value of y",

			"  Curve:Biquadratic,",
			"    AC29CoolingTempCurve,    !- Name",
			"    1.61164,                 !- Coefficient1 Constant",
			"    -.08591,                 !- Coefficient2 x",
			"    0.0032792,               !- Coefficient3 x**2",
			"    0.0038143,               !- Coefficient4 y",
			"    -.0001393,               !- Coefficient5 y**2",
			"    -.000229,                !- Coefficient6 x*y",
			"    -10,                     !- Minimum Value of x",
			"    50,                      !- Maximum Value of x",
			"    -10,                     !- Minimum Value of y",
			"    50;                      !- Maximum Value of y",

			"  Curve:Biquadratic,",
			"    AC29EIRTempCurve,        !- Name",
			"    0.12954,                 !- Coefficient1 Constant",
			"    0.063575,                !- Coefficient2 x",
			"    -.0019562,               !- Coefficient3 x**2",
			"    0.0089583,               !- Coefficient4 y",
			"    0.00022236,              !- Coefficient5 y**2",
			"    -.0002964,               !- Coefficient6 x*y",
			"    -10,                     !- Minimum Value of x",
			"    50,                      !- Maximum Value of x",
			"    -10,                     !- Minimum Value of y",
			"    50;                      !- Maximum Value of y",

			"  Curve:Biquadratic,",
			"    AC32CoolingTempCurve,    !- Name",
			"    0.48671,                 !- Coefficient1 Constant",
			"    0.075657,                !- Coefficient2 x",
			"    -.0016748,               !- Coefficient3 x**2",
			"    -.0158888,               !- Coefficient4 y",
			"    -.00010629,              !- Coefficient5 y**2",
			"    0.00053274,              !- Coefficient6 x*y",
			"    -10,                     !- Minimum Value of x",
			"    50,                      !- Maximum Value of x",
			"    -10,                     !- Minimum Value of y",
			"    50;                      !- Maximum Value of y",

			"  Curve:Biquadratic,",
			"    BLCoolingTempCurve,      !- Name",
			"    0.54353,                 !- Coefficient1 Constant",
			"    0.020175,                !- Coefficient2 x",
			"    0.00034548,              !- Coefficient3 x**2",
			"    0.00085622,              !- Coefficient4 y",
			"    -.000085034,             !- Coefficient5 y**2",
			"    -.00007994,              !- Coefficient6 x*y",
			"    -10,                     !- Minimum Value of x",
			"    50,                      !- Maximum Value of x",
			"    -10,                     !- Minimum Value of y",
			"    50;                      !- Maximum Value of y",

			"  Curve:Biquadratic,",
			"    BLEIRTempCurve,          !- Name",
			"    1.01728,                 !- Coefficient1 Constant",
			"    -.021724,                !- Coefficient2 x",
			"    0.00025326,              !- Coefficient3 x**2",
			"    0.017851,                !- Coefficient4 y",
			"    0.00014881,              !- Coefficient5 y**2",
			"    -.00043614,              !- Coefficient6 x*y",
			"    -10,                     !- Minimum Value of x",
			"    50,                      !- Maximum Value of x",
			"    -10,                     !- Minimum Value of y",
			"    50;                      !- Maximum Value of y",

			});

			ASSERT_FALSE( process_idf( idf_objects ) );
			
			NumOfTimeStepInHour = 1; // must initialize this to get schedules initialized
			MinutesPerTimeStep = 60; // must initialize this to get schedules initialized
			ProcessScheduleInput();

			HeatBalanceManager::GetZoneData( ErrorsFound ); // read zone data
			EXPECT_FALSE( ErrorsFound ); // zones are specified in the idf snippet

			// Get Zone Equipment Configuration ata
			DataZoneEquipment::GetZoneEquipmentData();			
			MixedAir::GetOutsideAirSysInputs();
			MixedAir::GetOAControllerInputs();
			SplitterComponent::GetSplitterInput();
			BranchInputManager::GetMixerInput();
			BranchInputManager::ManageBranchInput();
			DirectAirManager::GetDirectAirInput();
			// Get Air Loop HVAC Data
			SimAirServingZones::GetAirPathData();
			SimAirServingZones::InitAirLoops( FirstHVACIteration );
			ZoneTempPredictorCorrector::GetZoneAirSetPoints();

			CurDeadBandOrSetback.allocate( 2 );
			CurDeadBandOrSetback( 1 ) = false;
			CurDeadBandOrSetback( 2 ) = false;

			ZoneSysEnergyDemand.allocate( 2 );
			ZoneSysEnergyDemand( 1 ).RemainingOutputRequired = -2500;
			ZoneSysEnergyDemand( 1 ).OutputRequiredToHeatingSP = -20000;
			ZoneSysEnergyDemand( 1 ).OutputRequiredToCoolingSP = -2500;
			ZoneSysEnergyDemand( 2 ).RemainingOutputRequired = -2500;
			ZoneSysEnergyDemand( 2 ).OutputRequiredToHeatingSP = -20000;
			ZoneSysEnergyDemand( 2 ).OutputRequiredToCoolingSP = -2500;

			ZoneSysEnergyDemand( 1 ).SequencedOutputRequired.allocate( 1 );
			ZoneSysEnergyDemand( 1 ).SequencedOutputRequiredToCoolingSP.allocate( 1 );
			ZoneSysEnergyDemand( 1 ).SequencedOutputRequiredToHeatingSP.allocate( 1 );
			ZoneSysEnergyDemand( 2 ).SequencedOutputRequired.allocate( 1 );
			ZoneSysEnergyDemand( 2 ).SequencedOutputRequiredToCoolingSP.allocate( 1 );
			ZoneSysEnergyDemand( 2 ).SequencedOutputRequiredToHeatingSP.allocate( 1 );

			ZoneSysEnergyDemand( 1 ).SequencedOutputRequired( 1 ) = ZoneSysEnergyDemand( 1 ).RemainingOutputRequired;
			ZoneSysEnergyDemand( 1 ).SequencedOutputRequiredToCoolingSP( 1 ) = ZoneSysEnergyDemand( 1 ).OutputRequiredToCoolingSP;
			ZoneSysEnergyDemand( 1 ).SequencedOutputRequiredToHeatingSP( 1 ) = ZoneSysEnergyDemand( 1 ).OutputRequiredToHeatingSP;
			ZoneSysEnergyDemand( 2 ).SequencedOutputRequired( 1 ) = ZoneSysEnergyDemand( 2 ).RemainingOutputRequired;
			ZoneSysEnergyDemand( 2 ).SequencedOutputRequiredToCoolingSP( 1 ) = ZoneSysEnergyDemand( 2 ).OutputRequiredToCoolingSP;
			ZoneSysEnergyDemand( 2 ).SequencedOutputRequiredToHeatingSP( 1 ) = ZoneSysEnergyDemand( 2 ).OutputRequiredToHeatingSP;

			HVACMultiSpeedHeatPump::GetMSHeatPumpInput();

			DataGlobals::SysSizingCalc = true; // disable sizing calculation
			MSHeatPump( 1 ).TotHeatEnergyRate = 1000.0;
			MSHeatPump( 1 ).TotCoolEnergyRate = 1000.0;
			MSHeatPump( 2 ).TotHeatEnergyRate = 1000.0;
			MSHeatPump( 2 ).TotCoolEnergyRate = 1000.0;

			// InitMSHeatPump resets the current MSHeatPumpNum only 
			HVACMultiSpeedHeatPump::InitMSHeatPump(MSHeatPumpNum, FirstHVACIteration, AirLoopNum, QZnReq, OnOffAirFlowRatio );
	
			EXPECT_DOUBLE_EQ( 1000.0, MSHeatPump( 1 ).TotHeatEnergyRate );
			EXPECT_DOUBLE_EQ( 1000.0, MSHeatPump( 1 ).TotCoolEnergyRate );
			EXPECT_DOUBLE_EQ( 0.0, MSHeatPump( 2 ).TotHeatEnergyRate );
			EXPECT_DOUBLE_EQ( 0.0, MSHeatPump( 2 ).TotCoolEnergyRate );

			ZoneSysEnergyDemand.deallocate();
			CurDeadBandOrSetback.deallocate();
	}
}
