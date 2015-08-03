// EnergyPlus::Standalone ERV Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataSizing.hh>
#include <ReportSizingManager.hh>
#include <DataPrecisionGlobals.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <ObjexxFCL/gio.hh>

using namespace EnergyPlus;
using namespace ObjexxFCL;
using namespace EnergyPlus::DataAirSystems;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::Fans;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::ReportSizingManager;

TEST( ReportSizingManager, GetCoilDesFlowT )
{
	ShowMessage( "Begin Test: ReportSizingManager, GetCoilDesFlowT" );

	// setup global allocation
	DataSizing::SysSizInput.allocate(1);
	DataSizing::SysSizPeakDDNum.allocate(1);
	DataSizing::FinalSysSizing.allocate(1);
	DataSizing::CalcSysSizing.allocate(1);
	DataSizing::CalcSysSizing(1).SumZoneCoolLoadSeq.allocate(1);
	DataSizing::CalcSysSizing(1).CoolZoneAvgTempSeq.allocate(1);
	DataSizing::SysSizPeakDDNum(1).TimeStepAtSensCoolPk.allocate(1);
	DataSizing::SysSizPeakDDNum(1).TimeStepAtCoolFlowPk.allocate(1);
	DataSizing::SysSizPeakDDNum(1).TimeStepAtTotCoolPk.allocate(1);
	
	// one-time global initialization
	int const DesignDayForPeak = 1;
	DataSizing::SysSizPeakDDNum(1).SensCoolPeakDD = DesignDayForPeak;
	DataSizing::SysSizPeakDDNum(1).CoolFlowPeakDD = DesignDayForPeak;
	DataSizing::SysSizPeakDDNum(1).TotCoolPeakDD = DesignDayForPeak;
	DataSizing::SysSizPeakDDNum(1).TimeStepAtSensCoolPk(DesignDayForPeak) = 1;
	DataSizing::SysSizPeakDDNum(1).TimeStepAtCoolFlowPk(DesignDayForPeak) = 1;
	DataSizing::SysSizPeakDDNum(1).TimeStepAtTotCoolPk(DesignDayForPeak) = 1;
	DataSizing::FinalSysSizing(1).CoolSupTemp = 10;
	DataSizing::FinalSysSizing(1).MassFlowAtCoolPeak = 2.0;
	DataSizing::FinalSysSizing(1).DesCoolVolFlow = 0.15;
	DataSizing::DataAirFlowUsedForSizing = 0.2;
	DataEnvironment::StdRhoAir = 1000;
	DataSizing::CalcSysSizing(1).SumZoneCoolLoadSeq(1) = 1250000;
	
	// one-time argument initialization
	int const sysNum = 1;
	Real64 const CpAir = 4179;
	
	// argument return values
	Real64 designFlowValue;
	Real64 designExitTemp;
	
	DataSizing::SysSizInput(1).CoolingPeakLoadType = DataSizing::TotalCoolingLoad;
	DataSizing::FinalSysSizing(1).CoolingPeakLoadType = DataSizing::TotalCoolingLoad;

	// Single path for VAV
	DataSizing::SysSizInput(1).CoolCapControl = DataSizing::VAV;
	ReportSizingManager::GetCoilDesFlowT(sysNum, CpAir, designFlowValue, designExitTemp);
	EXPECT_DOUBLE_EQ(DataSizing::FinalSysSizing(1).CoolSupTemp, designExitTemp);
	EXPECT_DOUBLE_EQ(0.002, designFlowValue);
	
	// Single path for OnOff
	DataSizing::SysSizInput(1).CoolCapControl = DataSizing::OnOff;
	ReportSizingManager::GetCoilDesFlowT(sysNum, CpAir, designFlowValue, designExitTemp);
	EXPECT_DOUBLE_EQ(DataSizing::FinalSysSizing(1).CoolSupTemp, designExitTemp);
	EXPECT_DOUBLE_EQ(0.2, designFlowValue);
	
	// Two paths for VT: 
	// CoolSupTemp > calculated value
	DataSizing::SysSizInput(1).CoolCapControl = DataSizing::VT;
	DataSizing::CalcSysSizing(1).CoolZoneAvgTempSeq(1) = 10;
	ReportSizingManager::GetCoilDesFlowT(sysNum, CpAir, designFlowValue, designExitTemp);
	EXPECT_DOUBLE_EQ(DataSizing::FinalSysSizing(1).CoolSupTemp, designExitTemp);
	EXPECT_DOUBLE_EQ(DataSizing::FinalSysSizing(1).DesCoolVolFlow, designFlowValue);
	// CoolSupTemp < calculated value
	DataSizing::SysSizInput(1).CoolCapControl = DataSizing::VT;
	DataSizing::CalcSysSizing(1).CoolZoneAvgTempSeq(1) = 15;
	ReportSizingManager::GetCoilDesFlowT(sysNum, CpAir, designFlowValue, designExitTemp);
	EXPECT_NEAR(13.00590, designExitTemp, 0.0001);
	EXPECT_DOUBLE_EQ(DataSizing::FinalSysSizing(1).DesCoolVolFlow, designFlowValue);
	
	// Two paths for bypass:
	// MixTemp > DesExitTemp
	DataSizing::SysSizInput(1).CoolCapControl = DataSizing::Bypass;
	DataSizing::CalcSysSizing(1).CoolZoneAvgTempSeq(1) = 13;
	DataSizing::CalcSysSizing(1).MixTempAtCoolPeak = 15;
	ReportSizingManager::GetCoilDesFlowT(sysNum, CpAir, designFlowValue, designExitTemp);
	EXPECT_DOUBLE_EQ(10, designExitTemp);
	EXPECT_NEAR(0.119823, designFlowValue, 0.0001);
	// MixTemp < DesExitTemp
	DataSizing::CalcSysSizing(1).MixTempAtCoolPeak = 5;
	ReportSizingManager::GetCoilDesFlowT(sysNum, CpAir, designFlowValue, designExitTemp);
	EXPECT_DOUBLE_EQ(10, designExitTemp);
	EXPECT_DOUBLE_EQ(DataSizing::FinalSysSizing(1).DesCoolVolFlow, designFlowValue);
	
	// Oh and the sensible cases
	DataSizing::SysSizInput(1).CoolingPeakLoadType = DataSizing::SensibleCoolingLoad;
	DataSizing::FinalSysSizing(1).CoolingPeakLoadType = DataSizing::SensibleCoolingLoad;
	// Repeat a VT case
	DataSizing::SysSizInput(1).CoolCapControl = DataSizing::VT;
	DataSizing::CalcSysSizing(1).CoolZoneAvgTempSeq(1) = 10;
	ReportSizingManager::GetCoilDesFlowT(sysNum, CpAir, designFlowValue, designExitTemp);
	EXPECT_DOUBLE_EQ(DataSizing::FinalSysSizing(1).CoolSupTemp, designExitTemp);
	EXPECT_DOUBLE_EQ(DataSizing::FinalSysSizing(1).DesCoolVolFlow, designFlowValue);
	// And a bypass case
	DataSizing::SysSizInput(1).CoolCapControl = DataSizing::Bypass;
	DataSizing::CalcSysSizing(1).CoolZoneAvgTempSeq(1) = 13;
	DataSizing::CalcSysSizing(1).MixTempAtCoolPeak = 15;
	ReportSizingManager::GetCoilDesFlowT(sysNum, CpAir, designFlowValue, designExitTemp);
	EXPECT_DOUBLE_EQ(10, designExitTemp);
	EXPECT_NEAR(0.119823, designFlowValue, 0.0001);
	
	
	// tear down
	DataSizing::DataAirFlowUsedForSizing = 0.0;
	DataSizing::CalcSysSizing(1).SumZoneCoolLoadSeq.deallocate();
	DataSizing::CalcSysSizing(1).CoolZoneAvgTempSeq.deallocate();
	DataSizing::SysSizPeakDDNum(1).TimeStepAtSensCoolPk.deallocate();
	DataSizing::SysSizPeakDDNum(1).TimeStepAtCoolFlowPk.deallocate();
	DataSizing::SysSizPeakDDNum(1).TimeStepAtTotCoolPk.deallocate();
	DataSizing::SysSizInput.deallocate();
	DataSizing::SysSizPeakDDNum.deallocate();
	DataSizing::FinalSysSizing.deallocate();
	DataSizing::CalcSysSizing.deallocate();
	
}
TEST( ReportSizingManager, RequestSizingSystem ) {
	ShowMessage( "Begin Test: ReportSizingManager, RequestSizingSystem" );

	int write_stat;
	// Open the Initialization Output File
	OutputFileInits = GetNewUnitNumber();
	{ IOFlags flags; flags.ACTION( "write" ); flags.STATUS( "UNKNOWN" ); gio::open( OutputFileInits, "eplusout.eio", flags ); write_stat = flags.ios(); }

	std::string CompName; // component name
	std::string CompType; // component type
	std::string SizingString; // input field sizing description
	int SizingType; // integer type of sizing requested
	Real64 SizingResult; // autosized value of coil input field
	bool PrintWarning; // true when sizing information is reported in the eio file
	std::string CallingRoutine; // calling routine

	DataConstantUsedForSizing = 1.0;
	DataFractionUsedForSizing = 1.0;
	DataTotCapCurveIndex = 0;
	DataDesOutletAirTemp = 0.0;

	CurZoneEqNum = 0;
	CurOASysNum = 0;
	CurSysNum = 1;
	FinalSysSizing.allocate( 1 );
	FinalSysSizing( CurSysNum ).CoolSupTemp = 12.0;
	FinalSysSizing( CurSysNum ).CoolSupHumRat = 0.0085;
	FinalSysSizing( CurSysNum ).MixTempAtCoolPeak = 28.0;
	FinalSysSizing( CurSysNum ).MixHumRatAtCoolPeak = 0.0075;
	FinalSysSizing( CurSysNum ).DesCoolVolFlow = 1.00;
	FinalSysSizing( CurSysNum ).DesOutAirVolFlow = 0.2;

	PrimaryAirSystem.allocate( 1 );
	PrimaryAirSystem( CurSysNum ).NumOACoolCoils = 0;
	PrimaryAirSystem( CurSysNum ).SupFanNum = 0;
	PrimaryAirSystem( CurSysNum ).RetFanNum = 0;

	SysSizingRunDone = true;
	SysSizInput.allocate( 1 );
	SysSizInput( 1 ).AirLoopNum = CurSysNum;
	DataSizing::NumSysSizInput = 1;

	StdBaroPress = 101325.0;
	InitializePsychRoutines();

	DataFlowUsedForSizing = FinalSysSizing( CurSysNum ).DesCoolVolFlow;
	// Need this to prevent crash in RequestSizing
	UnitarySysEqSizing.allocate( 1 );
	OASysEqSizing.allocate( 1 );

	CompType = "COIL:COOLING:DX:SINGLESPEED";
	CompName = "Single Speed DX Cooling Coil";
	SizingType = DataHVACGlobals::CoolingCapacitySizing;
	SizingString = "Nominal Capacity";
	SizingResult = DataSizing::AutoSize;
	PrintWarning = true;
	CallingRoutine = "RequestSizing";
	DataIsDXCoil = true;

	// dx cooling coil capacity sizing
	ReportSizingManager::RequestSizing( CompType, CompName, SizingType, SizingString, SizingResult, PrintWarning, CallingRoutine );
	EXPECT_NEAR( 18882.0, SizingResult, 0.1 );

	CompType = "COIL:COOLING:WATER";
	CompName = "Chilled Water Cooling Coil";
	SizingResult = DataSizing::AutoSize;
	DataEnvironment::StdRhoAir = 1.18;
	DataIsDXCoil = false;

	// chilled water cooling coil capacity sizing
	ReportSizingManager::RequestSizing( CompType, CompName, SizingType, SizingString, SizingResult, PrintWarning, CallingRoutine );
	EXPECT_NEAR( 19234.6, SizingResult, 0.1 );

	// close and delete eio output file
	{ IOFlags flags; flags.DISPOSE( "DELETE" ); gio::close( OutputFileInits, flags ); }

	// clean
	DataSizing::NumSysSizInput = 0;
	FinalSysSizing.deallocate();
	PrimaryAirSystem.deallocate();
	SysSizInput.deallocate();
	UnitarySysEqSizing.deallocate();
	OASysEqSizing.deallocate();
}

TEST( ReportSizingManager, RequestSizingZone ) {
	ShowMessage( "Begin Test: ReportSizingManager, RequestSizingZone" );

	int write_stat;
	// Open the Initialization Output File
	OutputFileInits = GetNewUnitNumber();
	{ IOFlags flags; flags.ACTION( "write" ); flags.STATUS( "UNKNOWN" ); gio::open( OutputFileInits, "eplusout.eio", flags ); write_stat = flags.ios(); }

	int const ZoneNum = 1;
	std::string CompName; // component name
	std::string CompType; // component type
	std::string SizingString; // input field sizing description
	int SizingType; // integerized type of sizing requested
	Real64 SizingResult; // autosized value of coil input field
	bool PrintWarning; // true when sizing information is reported in the eio file
	std::string CallingRoutine; // calling routine

	DataConstantUsedForSizing = 1.0;
	DataFractionUsedForSizing = 1.0;
	DataTotCapCurveIndex = 0;
	DataDesOutletAirTemp = 0.0;

	CurZoneEqNum = 1;
	CurOASysNum = 0;
	CurSysNum = 0;
	FinalZoneSizing.allocate( 1 );
	FinalZoneSizing( CurZoneEqNum ).CoolDesTemp = 12.0;
	FinalZoneSizing( CurZoneEqNum ).CoolDesHumRat = 0.0085;
	FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInTemp = 28.0;
	FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInHumRat = 0.0075;
	FinalZoneSizing( CurZoneEqNum ).DesCoolOAFlowFrac = 0.2;
	FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow = 0.30;

	ZoneSizingRunDone = true;
	StdBaroPress = 101325.0;
	InitializePsychRoutines();

	// Need this to prevent crash in RequestSizing
	ZoneEqSizing.allocate( 1 );
	ZoneSizingInput.allocate( 1 );
	ZoneSizingInput( 1 ).ZoneNum = ZoneNum;
	DataSizing::NumZoneSizingInput = 1;
	ZoneEqSizing( CurZoneEqNum ).DesignSizeFromParent = false;
	DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow;

	CompType = "COIL:COOLING:DX:SINGLESPEED";
	CompName = "Single Speed DX Cooling Coil";
	SizingType = DataHVACGlobals::CoolingCapacitySizing;
	SizingString = "Nominal Capacity";
	SizingResult = DataSizing::AutoSize;
	PrintWarning = true;
	CallingRoutine = "RequestSizing";
	DataIsDXCoil = true;

	// dx cooling coil capacity sizing
	ReportSizingManager::RequestSizing( CompType, CompName, SizingType, SizingString, SizingResult, PrintWarning, CallingRoutine );
	EXPECT_NEAR( 5664.6, SizingResult, 0.1 );

	CompType = "COIL:COOLING:WATER";
	CompName = "Chilled Water Cooling Coil";
	SizingResult = DataSizing::AutoSize;
	DataEnvironment::StdRhoAir = 1.18;
	FinalZoneSizing( CurZoneEqNum ).DesCoolMassFlow = FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow * StdRhoAir;
	DataIsDXCoil = false;

	// chilled water cooling coil capacity sizing
	ReportSizingManager::RequestSizing( CompType, CompName, SizingType, SizingString, SizingResult, PrintWarning, CallingRoutine );
	EXPECT_NEAR( 5770.4, SizingResult, 0.1 );

	// close and delete eio output file
	{ IOFlags flags; flags.DISPOSE( "DELETE" ); gio::close( OutputFileInits, flags ); }

	// clean
	DataSizing::NumZoneSizingInput = 0;
	FinalZoneSizing.deallocate();
	ZoneEqSizing.deallocate();
	ZoneSizingInput.deallocate();

}
