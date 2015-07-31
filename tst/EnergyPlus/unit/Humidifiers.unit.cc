// EnergyPlus::Humidifiers Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers

// EnergyPlus Headers
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/Humidifiers.hh>

#include "Fixtures/HVACFixture.hh"

using namespace EnergyPlus::Humidifiers;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::CurveManager;

namespace EnergyPlus {

	TEST_F( HVACFixture, Humidifiers_Sizing ) {
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

	TEST_F( HVACFixture, Humidifiers_AutoSizing ) {
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

	TEST_F( HVACFixture, Humidifiers_EnergyUse ) {
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

	TEST_F( HVACFixture, Humidifiers_GetHumidifierInput ) {
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

	TEST_F( HVACFixture, Humidifiers_ThermalEfficiency ) {
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
