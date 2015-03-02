// EnergyPlus::Humidifiers Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/IOFlags.hh>
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

	//// autosize nominal gas use rate
	//int write_stat;
	//OutputFileInits = GetNewUnitNumber();
	//{ IOFlags flags; flags.ACTION( "write" ); flags.STATUS( "UNKNOWN" ); gio::open( OutputFileInits, "eplusout.eio", flags ); write_stat = flags.ios(); }
	//try {
		//EnergyPlus::sqlite = std::unique_ptr<SQLite>( new SQLite() );
	//}
	//catch ( const std::runtime_error& error ) {
			//ShowFatalError( error.what() );		
	//}

	OutBaroPress = 101325.0;
	thisHum.SizeHumidifier();
	EXPECT_DOUBLE_EQ( 4.00E-5, thisHum.NomCapVol );
	EXPECT_DOUBLE_EQ( 0.040000010708118504, thisHum.NomCap );
	EXPECT_DOUBLE_EQ( 103710.42776358133, thisHum.NomPower );

}

TEST( GasFiredHumidifierTest, EnergyUse ) {

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

	//// autosize capacity and nominal power(or nominal gas use rate)
	//int write_stat;
	//OutputFileInits = GetNewUnitNumber();
	//{ IOFlags flags; flags.ACTION( "write" ); flags.STATUS( "UNKNOWN" ); gio::open( OutputFileInits, "eplusout.eio", flags ); write_stat = flags.ios(); }
	//try {
		//EnergyPlus::sqlite = std::unique_ptr<SQLite>( new SQLite() );
	//} catch ( const std::runtime_error& error ) {
		//ShowFatalError( error.what() );
	//}

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
}
