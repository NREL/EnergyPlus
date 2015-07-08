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
#include <EnergyPlus/InputProcessor.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataStringGlobals.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/DataLoopNode.hh>

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
using namespace EnergyPlus::InputProcessor;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::CurveManager;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::NodeInputManager;

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
TEST( GasFiredHumidifierTest, GetHumidifierInput ) {
	ShowMessage( "Begin Test: GasFiredHumidifierTest, GetHumidifierInput" );

	// Test get input for gas fired humidifier object
	//Humidifier:Steam:Gas,
	//  Main Gas Humidifier,     !- Name
	//  ALWAYS_ON,               !- Availability Schedule Name
	//  autosize,                !- Rated Capacity {m3/s}
	//  autosize,                !- Rated Gas Use Rate {W}
	//  0.80,                    !- Thermal Efficiency {-} 
	//  ThermalEfficiencyFPLR,   !- Thermal Efficiency Modifier Curve Name
	//  0,                       !- Rated Fan Power {W}
	//  0,                       !- Auxiliary Electric Power {W}
	//  Mixed Air Node 1,        !- Air Inlet Node Name
	//  Main Humidifier Outlet Node,  !- Air Outlet Node Name
	//  ;                        !- Water Storage Tank Name

	int NumGasSteamHums( 1 ); // Zone number
	int NumAlphas;
	int NumNumbers;
	int CurveNum;

	NumAlphas = 7;
	NumNumbers = 5;

	// GetNumObjectsFound function requires the IDF object for Construction
	NumObjectDefs = 2;
	ListOfObjects.allocate( NumObjectDefs );
	ListOfObjects( 1 ) = "HUMIDIFIER:STEAM:GAS";
	iListOfObjects.allocate( NumObjectDefs );
	iListOfObjects( 1 ) = 1;

	ObjectStartRecord.allocate( NumObjectDefs );
	ObjectStartRecord( 1 ) = 1;
	ObjectGotCount.allocate( NumObjectDefs );
	IDFRecordsGotten.allocate( NumObjectDefs );
	IDFRecordsGotten( NumObjectDefs ) = false;
	ObjectDef.allocate( NumObjectDefs );
	ObjectDef( 1 ).NumFound = 1;
	ObjectDef( 1 ).NumParams = NumAlphas + NumNumbers;
	ObjectDef( 1 ).NumAlpha = NumAlphas;
	ObjectDef( 1 ).NumNumeric = NumNumbers;
	ObjectDef( 1 ).AlphFieldChks.allocate( NumAlphas );
	ObjectDef( 1 ).AlphFieldChks = " ";
	ObjectDef( 1 ).NumRangeChks.allocate( NumNumbers );

	NumIDFRecords = 1;
	IDFRecords.allocate( 1 );
	IDFRecords( 1 ).Name = ListOfObjects( 1 );
	IDFRecords( 1 ).NumNumbers = NumNumbers;
	IDFRecords( 1 ).NumAlphas = NumAlphas;
	IDFRecords( 1 ).ObjectDefPtr = ObjectDef( 1 ).NumFound;
	IDFRecords( 1 ).Alphas.allocate( NumAlphas );
	IDFRecords( 1 ).Alphas( 1 ) = "Main Gas Humidifier";
	IDFRecords( 1 ).Alphas( 2 ) = "";
	IDFRecords( 1 ).Alphas( 3 ) = "ThermalEfficiencyFPLR";
	IDFRecords( 1 ).Alphas( 4 ) = "Mixed Air Node 1";
	IDFRecords( 1 ).Alphas( 5 ) = "Main Humidifier Outlet Node";
	IDFRecords( 1 ).Alphas( 6 ) = "";
	IDFRecords( 1 ).Alphas( 7 ) = "";
	IDFRecords( 1 ).AlphBlank.allocate( NumAlphas );
	IDFRecords( 1 ).AlphBlank( 1 ) = false;
	IDFRecords( 1 ).AlphBlank( 2 ) = true;
	IDFRecords( 1 ).AlphBlank( 3 ) = false;
	IDFRecords( 1 ).AlphBlank( 4 ) = false;
	IDFRecords( 1 ).AlphBlank( 5 ) = false;
	IDFRecords( 1 ).AlphBlank( 6 ) = true;
	IDFRecords( 1 ).AlphBlank( 7 ) = true;
	IDFRecords( 1 ).Numbers.allocate( NumNumbers );
	IDFRecords( 1 ).Numbers( 1 ) = 0.00045;
	IDFRecords( 1 ).Numbers( 2 ) = 10000.0;
	IDFRecords( 1 ).Numbers( 3 ) = 0.80;
	IDFRecords( 1 ).Numbers( 4 ) = 0.0;
	IDFRecords( 1 ).Numbers( 5 ) = 0.0;
	IDFRecords( 1 ).NumBlank.allocate( NumNumbers );
	IDFRecords( 1 ).NumBlank = false;

	// the following array size allocation is done only once every simulation in
	// GetObjectItem function in InputProcessor module.
	// AlphaArgs.allocate( MaxAlphaArgsFound );
	// NumberArgs.allocate( MaxNumericArgsFound );
	// thus maximum array size is already fixed by unit tests (HeatBalanceManager.unit.cc)
	// that use these variables and called prior to this unit test. This initialization is 
	// used only if want to run this unit test individually, or some changes the calling 
	// sequence of the unit test.
	MaxAlphaArgsFound = 20;
	MaxNumericArgsFound = 20;

	// GetOnlySingleNode requires that the IDD object definition
	ListOfObjects( 2 ) = "NodeList";
	iListOfObjects( 2 ) = 2;
	ObjectDef( 2 ).NumParams = 2;
	ObjectDef( 2 ).NumAlpha = 2;
	ObjectDef( 2 ).NumNumeric = 0;

	CurveNum = 1;
	CurveManager::NumCurves = 1; 
	CurveManager::GetCurvesInputFlag = false;
	NodeInputManager::GetNodeInputFlag = false;
	PerfCurve.allocate( CurveNum );
	PerfCurve( CurveNum ).Name = "ThermalEfficiencyFPLR";
	PerfCurve( CurveNum ).CurveType = CurveManager::Quadratic;
	PerfCurve( CurveNum ).ObjectType = CurveType_Quadratic;
	PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
	PerfCurve( CurveNum ).Coeff1 = 0.9375;
	PerfCurve( CurveNum ).Coeff2 = 0.0625;
	PerfCurve( CurveNum ).Coeff3 = -7.0E-15;
	PerfCurve( CurveNum ).Coeff4 = 0.0;
	PerfCurve( CurveNum ).Coeff5 = 0.0;
	PerfCurve( CurveNum ).Coeff6 = 0.0;
	PerfCurve( CurveNum ).Var1Min = 0.0;
	PerfCurve( CurveNum ).Var1Max = 1.2;
	PerfCurve( CurveNum ).Var2Min = 0.7;
	PerfCurve( CurveNum ).Var2Max = 1.0;

	// call to get valid window material types
	GetHumidifierInput(); // 
	EXPECT_EQ( 1, Humidifier( 1 ).EfficiencyCurvePtr );

	// clear varaibles
	NumIDFRecords = 0;
	NumObjectDefs = 0;
	NumGasSteamHums = 0;
	NumCurves = 0;
	MaxAlphaArgsFound = 0;
	MaxNumericArgsFound = 0;
	CurveManager::GetCurvesInputFlag = true;
	NodeInputManager::GetNodeInputFlag = true;
	NodeInputManager::NumOfUniqueNodeNames = 0;

	// deallocate variables
	Humidifier.deallocate();
	ListOfObjects.deallocate();
	iListOfObjects.deallocate();
	ObjectStartRecord.deallocate();
	ObjectGotCount.deallocate();
	IDFRecordsGotten.deallocate();
	ObjectDef( 2 ).AlphFieldChks.deallocate();
	ObjectDef( 2 ).NumRangeChks.deallocate();
	ObjectDef.deallocate();
	IDFRecords( 1 ).Alphas.deallocate();
	IDFRecords( 1 ).AlphBlank.deallocate();
	IDFRecords( 1 ).Numbers.deallocate();
	IDFRecords( 1 ).NumBlank.deallocate();
	IDFRecords.deallocate();
	PerfCurve.deallocate();

}
TEST( GasFiredHumidifierTest, ThermalEfficiency ) {
	ShowMessage( "Begin Test: GasFiredHumidifierTest, ThermalEfficiency" );

	// tests thermal efficiency modifier curve use

	HumidifierData thisHum;
	int CurveNum;

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

	InitializePsychRoutines();

	CurveNum = 1;
	thisHum.EfficiencyCurvePtr = 1;
	thisHum.EfficiencyCurveType = 2;
	CurveManager::NumCurves = 1;
	CurveManager::GetCurvesInputFlag = false;
	PerfCurve.allocate( CurveNum );
	PerfCurve( CurveNum ).Name = "ThermalEfficiencyFPLR";
	PerfCurve( CurveNum ).CurveType = CurveManager::Quadratic;
	PerfCurve( CurveNum ).ObjectType = CurveType_Quadratic;
	PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
	PerfCurve( CurveNum ).Coeff1 = 0.9375;
	PerfCurve( CurveNum ).Coeff2 = 0.0625;
	PerfCurve( CurveNum ).Coeff3 =-7.0E-15;
	PerfCurve( CurveNum ).Coeff4 = 0.0;
	PerfCurve( CurveNum ).Coeff5 = 0.0;
	PerfCurve( CurveNum ).Coeff6 = 0.0;
	PerfCurve( CurveNum ).Var1Min = 0.0;
	PerfCurve( CurveNum ).Var1Max = 1.2;
	PerfCurve( CurveNum ).Var2Min = 0.7;
	PerfCurve( CurveNum ).Var2Max = 1.0;

	thisHum.CalcGasSteamHumidifier( 0.030 );
	EXPECT_NEAR( 0.7875, thisHum.ThermalEff, 0.001 );

	// clean up
	CurveManager::GetCurvesInputFlag = true;
	FinalSysSizing.deallocate();
	Humidifier.deallocate();
	PerfCurve.deallocate();
}