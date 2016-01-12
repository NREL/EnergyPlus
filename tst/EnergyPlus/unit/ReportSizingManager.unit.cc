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

// EnergyPlus::Standalone ERV Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/gio.hh>

#include "Fixtures/EnergyPlusFixture.hh"

// EnergyPlus Headers
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportSizingManager.hh>

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

TEST_F( EnergyPlusFixture, ReportSizingManager_GetCoilDesFlowT )
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
TEST_F( EnergyPlusFixture, ReportSizingManager_RequestSizingSystem ) {

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

TEST_F( EnergyPlusFixture, ReportSizingManager_RequestSizingZone ) {
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
