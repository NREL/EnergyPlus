// EnergyPlus::Standalone ERV Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataSizing.hh>
#include <ReportSizingManager.hh>
#include <DataPrecisionGlobals.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;

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
