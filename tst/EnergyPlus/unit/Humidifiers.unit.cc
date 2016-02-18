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

// EnergyPlus::Humidifiers Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers

// EnergyPlus Headers
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/Humidifiers.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus::Humidifiers;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::CurveManager;

namespace EnergyPlus {

	TEST_F( EnergyPlusFixture, Humidifiers_Sizing ) {
		SysSizingRunDone = true;
		CurSysNum = 1;
		NumElecSteamHums = 0;
		NumGasSteamHums = 1;
		NumHumidifiers = 1;

		HumidifierData thisHum;

		thisHum.HumType_Code = 2;
		thisHum.NomCapVol = 4.00E-5;
		thisHum.NomPower = AutoSize;
		thisHum.ThermalEffRated = 1.0;
		thisHum.FanPower = 0.0;
		thisHum.StandbyPower = 0.0;
		thisHum.SchedPtr = ScheduleAlwaysOn;
		thisHum.SchedPtr = ScheduleAlwaysOn;

		FinalSysSizing.allocate( CurSysNum );
		FinalSysSizing( CurSysNum ).MixTempAtCoolPeak = 30.0;
		FinalSysSizing( CurSysNum ).MixHumRatAtCoolPeak = 0.090;
		FinalSysSizing( CurSysNum ).DesMainVolFlow = 1.60894;
		FinalSysSizing( CurSysNum ).HeatMixHumRat = 0.05;
		FinalSysSizing( CurSysNum ).CoolSupHumRat = 0.07;
		FinalSysSizing( CurSysNum ).HeatSupHumRat = 0.10;

		// autosize nominal gas use rate
		OutBaroPress = 101325.0;
		thisHum.SizeHumidifier();
		EXPECT_DOUBLE_EQ( 4.00E-5, thisHum.NomCapVol );
		EXPECT_DOUBLE_EQ( 0.040000010708118504, thisHum.NomCap );
		EXPECT_DOUBLE_EQ( 103710.42776358133, thisHum.NomPower );

	}

	TEST_F( EnergyPlusFixture, Humidifiers_AutoSizing ) {
		SysSizingRunDone = true;
		CurSysNum = 1;
		NumElecSteamHums = 0;
		NumGasSteamHums = 1;
		NumHumidifiers = 1;

		HumidifierData thisHum;

		thisHum.HumType_Code = 2;
		thisHum.NomCapVol = AutoSize;
		thisHum.NomPower = AutoSize;
		thisHum.ThermalEffRated = 0.80;
		thisHum.FanPower = 0.0;
		thisHum.StandbyPower = 0.0;
		thisHum.SchedPtr = ScheduleAlwaysOn;
		thisHum.SchedPtr = ScheduleAlwaysOn;

		FinalSysSizing.allocate( CurSysNum );
		FinalSysSizing( CurSysNum ).MixTempAtCoolPeak = 30.0;
		FinalSysSizing( CurSysNum ).MixHumRatAtCoolPeak = 0.090;
		FinalSysSizing( CurSysNum ).DesMainVolFlow = 1.60894;
		FinalSysSizing( CurSysNum ).HeatMixHumRat = 0.05;
		FinalSysSizing( CurSysNum ).CoolSupHumRat = 0.07;
		FinalSysSizing( CurSysNum ).HeatSupHumRat = 0.10;

		OutBaroPress = 101325.0;
		// volumetric capacity autosize unit test
		thisHum.NomCapVol = AutoSize;
		CurZoneEqNum = 0; // size it based on system
		thisHum.SizeHumidifier();
		// test autosized nominal capacity
		EXPECT_NEAR( 8.185E-05, thisHum.NomCapVol, 1.0E-06 ); // m3/s
		// test autosized nominal capacity
		EXPECT_NEAR( 0.0818, thisHum.NomCap, 1.0E-04 ); // kg/s
		// test autosized nominal gas use rate
		EXPECT_NEAR( 265257.67, thisHum.NomPower, 1.0E-02 ); // Watts
	}

	TEST_F( EnergyPlusFixture, Humidifiers_EnergyUse ) {
		HumidifierData thisHum;

		TimeStepSys = 0.25;
		SysSizingRunDone = true;
		CurSysNum = 1;

		NumElecSteamHums = 0;
		NumGasSteamHums = 1;
		NumHumidifiers = 1;
		Humidifier.allocate( NumGasSteamHums );
		thisHum.HumType_Code = 2;
		thisHum.NomCapVol = 4.00E-5;
		thisHum.NomPower = 103710.0;
		thisHum.ThermalEffRated = 1.0;
		thisHum.FanPower = 0.0;
		thisHum.StandbyPower = 0.0;
		thisHum.SchedPtr = ScheduleAlwaysOn;
		thisHum.SchedPtr = ScheduleAlwaysOn;

		FinalSysSizing.allocate( CurSysNum );
		FinalSysSizing( CurSysNum ).MixTempAtCoolPeak = 20.0;
		FinalSysSizing( CurSysNum ).MixHumRatAtCoolPeak = 0.00089;
		FinalSysSizing( CurSysNum ).DesMainVolFlow = 1.60894;
		FinalSysSizing( CurSysNum ).HeatMixHumRat = 0.05;
		FinalSysSizing( CurSysNum ).CoolSupHumRat = 0.07;
		FinalSysSizing( CurSysNum ).HeatSupHumRat = 0.10;

		// resize the humidifier nominal capacity and gas use rate
		thisHum.NomCapVol = 4.00E-5;
		thisHum.NomPower = 103710;
		OutBaroPress = 101325.0;
		thisHum.SizeHumidifier();
		EXPECT_DOUBLE_EQ( 0.040000010708118504, thisHum.NomCap );
		EXPECT_DOUBLE_EQ( 103710.42776358133, thisHum.NomPower );

		// calculate gas use rate and energy at full load
		thisHum.AirInMassFlowRate = 1.8919;
		thisHum.AirInTemp = 20.0;
		thisHum.AirInEnthalpy = 25000.0;
		thisHum.InletWaterTempOption = 1;
		thisHum.CurMakeupWaterTemp = 20.0;
		OutBaroPress = 101325.0;

		thisHum.CalcGasSteamHumidifier( 0.040000010708118504 );
		EXPECT_DOUBLE_EQ( 103710.42776358133, thisHum.GasUseRate );

		thisHum.ReportHumidifier();
		EXPECT_DOUBLE_EQ( 93339384.987223208, thisHum.GasUseEnergy );
	}

	TEST_F( EnergyPlusFixture, Humidifiers_GetHumidifierInput ) {
		std::string const idf_objects = delimited_string({
			"Version,8.3;",
			"Humidifier:Steam:Gas,",
			"  Main Gas Humidifier,     !- Name",
			"  ,                        !- Availability Schedule Name",
			"  autosize,                !- Rated Capacity {m3/s}",
			"  autosize,                !- Rated Gas Use Rate {W}",
			"  0.80,                    !- Thermal Efficiency {-} ",
			"  ThermalEfficiencyFPLR,   !- Thermal Efficiency Modifier Curve Name",
			"  0,                       !- Rated Fan Power {W}",
			"  0,                       !- Auxiliary Electric Power {W}",
			"  Mixed Air Node 1,        !- Air Inlet Node Name",
			"  Main Humidifier Outlet Node,  !- Air Outlet Node Name",
			"  ;                        !- Water Storage Tank Name",
			"  Curve:Cubic,",
			"    ThermalEfficiencyFPLR,   !- Name",
			"    1.0,                     !- Coefficient1 Constant",
			"    0.0,                     !- Coefficient2 x",
			"    0.0,                     !- Coefficient3 x**2",
			"    0.0,                     !- Coefficient4 x**3",
			"    0.0,                     !- Minimum Value of x",
			"    1.5,                     !- Maximum Value of x",
			"    ,                        !- Minimum Curve Output",
			"    ,                        !- Maximum Curve Output",
			"    Dimensionless,           !- Input Unit Type for X",
			"    Dimensionless;           !- Output Unit Type",
		});

		ASSERT_FALSE( process_idf( idf_objects ) );

		GetHumidifierInput();
		ASSERT_EQ( 1, NumHumidifiers );
		EXPECT_EQ( 1, Humidifier( 1 ).EfficiencyCurvePtr );

	}

	TEST_F( EnergyPlusFixture, Humidifiers_ThermalEfficiency ) {
		// tests thermal efficiency modifier curve use

		HumidifierData thisHum;

		TimeStepSys = 0.25;
		SysSizingRunDone = true;
		CurSysNum = 1;

		NumElecSteamHums = 0;
		NumGasSteamHums = 1;
		NumHumidifiers = 1;
		Humidifier.allocate( NumGasSteamHums );
		thisHum.HumType_Code = 2;
		thisHum.NomCapVol = 4.00E-5;
		thisHum.NomCap = 4.00E-2;
		thisHum.NomPower = 103720.0;
		thisHum.ThermalEffRated = 0.80;
		thisHum.FanPower = 0.0;
		thisHum.StandbyPower = 0.0;
		thisHum.SchedPtr = ScheduleAlwaysOn;
		thisHum.SchedPtr = ScheduleAlwaysOn;

		FinalSysSizing.allocate( CurSysNum );
		FinalSysSizing( CurSysNum ).MixTempAtCoolPeak = 20.0;
		FinalSysSizing( CurSysNum ).MixHumRatAtCoolPeak = 0.00089;
		FinalSysSizing( CurSysNum ).DesMainVolFlow = 1.60894;
		FinalSysSizing( CurSysNum ).HeatMixHumRat = 0.05;
		FinalSysSizing( CurSysNum ).CoolSupHumRat = 0.07;
		FinalSysSizing( CurSysNum ).HeatSupHumRat = 0.10;

		// calculate gas use rate and energy at full load
		thisHum.AirInMassFlowRate = 1.8919;
		thisHum.AirInTemp = 20.0;
		thisHum.AirInEnthalpy = 25000.0;
		thisHum.InletWaterTempOption = 1;
		thisHum.CurMakeupWaterTemp = 20.0;
		OutBaroPress = 101325.0;

		std::string const idf_objects = delimited_string({
			"Version,8.3;",
			"  Curve:Quadratic,",
			"    ThermalEfficiencyFPLR,   !- Name",
			"    0.9375,                  !- Coefficient1 Constant",
			"    0.0625,                  !- Coefficient2 x",
			"    -7.0E-15,                !- Coefficient3 x**2",
			"    0.0,                     !- Minimum Value of x",
			"    1.2;                     !- Maximum Value of x",
		});

		ASSERT_FALSE( process_idf( idf_objects ) );

		thisHum.EfficiencyCurvePtr = CurveManager::GetCurveIndex( "THERMALEFFICIENCYFPLR" );
		thisHum.EfficiencyCurveType = CurveManager::Quadratic;

		thisHum.CalcGasSteamHumidifier( 0.030 );
		EXPECT_NEAR( 0.7875, thisHum.ThermalEff, 0.001 );

	}

}
