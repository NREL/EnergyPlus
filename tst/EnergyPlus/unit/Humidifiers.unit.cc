// EnergyPlus::Humidifiers Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <EnergyPlus/Humidifiers.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportSizingManager.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SQLiteProcedures.hh>


using namespace ObjexxFCL;
using namespace EnergyPlus;
using namespace EnergyPlus::Humidifiers;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::ReportSizingManager;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::DataHVACGlobals;


TEST( GasFiredHumidifierTest, Sizing ) {
	ShowMessage( "Begin Test: GasFiredHumidifierTest, Sizing" );

	int write_stat;
	// Open the Initialization Output File (lifted from SimulationManager.cc)
	OutputFileInits = GetNewUnitNumber();
	{ IOFlags flags; flags.ACTION( "write" ); flags.STATUS( "UNKNOWN" ); gio::open( OutputFileInits, "eplusout.eio", flags ); write_stat = flags.ios(); }

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

	// clean up
	FinalSysSizing.deallocate();


}
TEST( GasFiredHumidifierTest, AutoSizing ) {
	ShowMessage( "Begin Test: GasFiredHumidifierTest, AutoSizing" );

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

	// clean up
	FinalSysSizing.deallocate();
}

TEST( GasFiredHumidifierTest, EnergyUse ) {
	ShowMessage( "Begin Test: GasFiredHumidifierTest, EnergyUse" );

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

	InitializePsychRoutines();

	thisHum.CalcGasSteamHumidifier( 0.040000010708118504 );
	EXPECT_DOUBLE_EQ( 103710.42776358133, thisHum.GasUseRate );

	thisHum.ReportHumidifier();
	EXPECT_DOUBLE_EQ( 93339384.987223208, thisHum.GasUseEnergy );

	// clean up
	FinalSysSizing.deallocate();
	Humidifier.deallocate();

	// Close and delete eio output file
	{ IOFlags flags; flags.DISPOSE( "DELETE" ); gio::close( OutputFileInits, flags ); }
}
