// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
// National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other
// contributors. All rights reserved.
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
//     similar designation, without the U.S. Department of Energy's prior written consent.
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

// EnergyPlus::WaterCoils Unit Tests
// FUNCTION INFORMATION:
//       AUTHOR         R Raustad
//       DATE WRITTEN   March 2015
//       MODIFIED
//       RE-ENGINEERED  na

// PURPOSE OF THIS FUNCTION:
// This function performs the sizing calculation on a chilled water coil.
// The water coil should size to use sizing data or Data* globals. After a coil is sized, the Data* variables should all be reset.
// Defect file showed a second coil in the input that autosized the same as the first (incorrect based on zone flow rates).


// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <Psychrometrics.hh>
#include <EnergyPlus/ReportCoilSelection.hh>
#include <EnergyPlus/SizingManager.hh>
#include <EnergyPlus/WaterCoils.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/Psychrometrics.hh>

#include <Fixtures/EnergyPlusFixture.hh>

using namespace EnergyPlus;
using namespace DataAirLoop;
using namespace DataAirSystems;
using namespace DataEnvironment;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::DataPlant;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::FluidProperties;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::SizingManager;
using namespace EnergyPlus::WaterCoils;
using namespace EnergyPlus::Psychrometrics;

class WaterCoilsTest : public EnergyPlusFixture
{

public:

	static void SetUpTestCase() {
		EnergyPlusFixture::SetUpTestCase();  // Sets up the base fixture
	}
	static void TearDownTestCase() { }

	virtual void SetUp() {
		EnergyPlusFixture::SetUp();  // Sets up individual test cases.

		CurZoneEqNum = 0;
		CurSysNum = 0;
		CurOASysNum = 0;
		NumWaterCoils = 1;
		WaterCoil.allocate( NumWaterCoils );
		WaterCoilNumericFields.allocate( NumWaterCoils );
		WaterCoilNumericFields(NumWaterCoils).FieldNames.allocate(17); // max N fields for water coil
		TotNumLoops = 1;
		PlantLoop.allocate( TotNumLoops );
		PlantSizData.allocate( 1 );
		ZoneEqSizing.allocate( 1 );
		UnitarySysEqSizing.allocate( 1 );
		OASysEqSizing.allocate( 1 );
		SysSizInput.allocate( 1 );
		ZoneSizingInput.allocate( 1 );
		SysSizPeakDDNum.allocate( 1 );
		SysSizPeakDDNum( 1 ).TimeStepAtSensCoolPk.allocate( 1 );
		SysSizPeakDDNum( 1 ).TimeStepAtCoolFlowPk.allocate( 1 );
		SysSizPeakDDNum( 1 ).TimeStepAtTotCoolPk.allocate( 1 );
		SysSizPeakDDNum( 1 ).SensCoolPeakDD = 1;
		SysSizPeakDDNum( 1 ).CoolFlowPeakDD = 1;
		SysSizPeakDDNum( 1 ).TotCoolPeakDD = 1;
		FinalSysSizing.allocate( 1 );
		FinalZoneSizing.allocate( 1 );
		PrimaryAirSystem.allocate( 1 );
		AirLoopControlInfo.allocate( 1 );
		InitializePsychRoutines();
	}

	virtual void TearDown() {
		EnergyPlusFixture::TearDown();  // Remember to tear down the base fixture after cleaning up derived fixture!

		NumWaterCoils = 0;
		WaterCoil.clear();
		WaterCoilNumericFields.clear();
		PlantLoop.clear();
		PlantSizData.clear();
		ZoneEqSizing.clear();
		UnitarySysEqSizing.clear();
		OASysEqSizing.clear();
		SysSizInput.clear();
		SysSizPeakDDNum.clear();
		FinalSysSizing.clear();
		SysSizPeakDDNum.clear();
		PrimaryAirSystem.clear();
		AirLoopControlInfo.clear();
		cached_Twb.clear();
		cached_Psat.clear();
	}

};

TEST_F( WaterCoilsTest, WaterCoolingCoilSizing )
{
	OutBaroPress = 101325.0;
	StdRhoAir = PsyRhoAirFnPbTdbW( OutBaroPress, 20.0, 0.0 );

	// set up sizing flags
	SysSizingRunDone = true;

	// set up plant sizing
	NumPltSizInput = 1;
	PlantSizData( 1 ).PlantLoopName = "WaterLoop";

	// set up plant loop
	for ( int l = 1; l <= TotNumLoops; ++l ) {
		auto & loop( PlantLoop( l ) );
		loop.LoopSide.allocate( 2 );
		auto & loopside( PlantLoop( 1 ).LoopSide( 1 ) );
		loopside.TotalBranches = 1;
		loopside.Branch.allocate( 1 );
		auto & loopsidebranch( PlantLoop( 1 ).LoopSide(1).Branch( 1 ) );
		loopsidebranch.TotalComponents = 1;
		loopsidebranch.Comp.allocate( 1 );
	}
	PlantLoop( 1 ).Name = "WaterLoop";
	PlantLoop( 1 ).FluidName = "FluidWaterLoop";
	PlantLoop( 1 ).FluidIndex = 1;
	PlantLoop( 1 ).FluidName = "WATER";
	PlantLoop( 1 ).FluidIndex = 1;

	// set up sizing data
	FinalSysSizing( 1 ).MixTempAtCoolPeak = 20.0;
	FinalSysSizing( 1 ).CoolSupTemp = 10.0;
	FinalSysSizing( 1 ).MixHumRatAtCoolPeak = 0.01;
	FinalSysSizing( 1 ).DesMainVolFlow = 0.00159;
	FinalSysSizing( 1 ).HeatSupTemp = 25.0;
	FinalSysSizing( 1 ).HeatOutTemp = 5.0;
	FinalSysSizing( 1 ).HeatRetTemp = 20.0;

	// set up water coil
	int CoilNum = 1;
	WaterCoil( CoilNum ).Name = "Test Water Cooling Coil";
	WaterCoil( CoilNum ).WaterLoopNum = 1;
	WaterCoil( CoilNum ).WaterCoilType = CoilType_Cooling;
	WaterCoil( CoilNum ).RequestingAutoSize = true;
	WaterCoil( CoilNum ).DesAirVolFlowRate = AutoSize;
	WaterCoil( CoilNum ).DesInletAirTemp = AutoSize;
	WaterCoil( CoilNum ).DesOutletAirTemp = AutoSize;
	WaterCoil( CoilNum ).DesInletWaterTemp = AutoSize;
	WaterCoil( CoilNum ).DesInletAirHumRat = AutoSize;
	WaterCoil( CoilNum ).DesOutletAirHumRat = AutoSize;
	WaterCoil( CoilNum ).MaxWaterVolFlowRate = AutoSize;

	WaterCoilNumericFields( CoilNum ).FieldNames( 3 ) = "Maximum Flow Rate";
	WaterCoil( CoilNum ).WaterInletNodeNum = 1;
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = WaterCoil( CoilNum ).WaterInletNodeNum;

	CurZoneEqNum = 0;
	CurSysNum = 1;
	CurOASysNum = 0;
	SysSizInput( 1 ).CoolCapControl = VAV;
	PlantSizData( 1 ).ExitTemp = 5.7;
	PlantSizData( 1 ).DeltaT = 5.0;
	FinalSysSizing( 1 ).MassFlowAtCoolPeak = FinalSysSizing( 1 ).DesMainVolFlow * StdRhoAir;
	DataWaterLoopNum = 1;
	NumOfGlycols = 1;

	createCoilSelectionReportObj();
	SizeWaterCoil( CoilNum );

	EXPECT_DOUBLE_EQ( 0.00159, WaterCoil( CoilNum ).DesAirVolFlowRate );
	// Check that all Data* variables have been reset
	EXPECT_EQ( 0, DataPltSizCoolNum );
	EXPECT_EQ( 0, DataWaterLoopNum );
	EXPECT_DOUBLE_EQ( 0.0, DataConstantUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataFractionUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataAirFlowUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataFlowUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataWaterFlowUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataCapacityUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataDesInletAirTemp );
	EXPECT_DOUBLE_EQ( 0.0, DataDesOutletAirTemp );
	EXPECT_DOUBLE_EQ( 0.0, DataDesOutletAirHumRat );
	EXPECT_DOUBLE_EQ( 0.0, DataDesInletAirHumRat );
	EXPECT_DOUBLE_EQ( 0.0, DataDesInletWaterTemp );

	// set second cooling coil to size at a different air flow, adjust sizing data, and autosize input data
	FinalSysSizing( 1 ).DesMainVolFlow = 0.00259;
	FinalSysSizing( 1 ).MassFlowAtCoolPeak = FinalSysSizing( 1 ).DesMainVolFlow * StdRhoAir;

	WaterCoil( CoilNum ).Name = "Test Water Cooling Coil 2";
	WaterCoil( CoilNum ).DesAirVolFlowRate = AutoSize;
	WaterCoil( CoilNum ).DesInletAirTemp = AutoSize;
	WaterCoil( CoilNum ).DesOutletAirTemp = AutoSize;
	WaterCoil( CoilNum ).DesInletWaterTemp = AutoSize;
	WaterCoil( CoilNum ).DesInletAirHumRat = AutoSize;
	WaterCoil( CoilNum ).DesOutletAirHumRat = AutoSize;
	WaterCoil( CoilNum ).MaxWaterVolFlowRate = AutoSize;

	SizeWaterCoil( CoilNum );

	EXPECT_DOUBLE_EQ( 0.00259, WaterCoil( CoilNum ).DesAirVolFlowRate );
	EXPECT_EQ( 0, DataPltSizCoolNum );
	EXPECT_EQ( 0, DataWaterLoopNum );
	EXPECT_DOUBLE_EQ( 0.0, DataConstantUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataFractionUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataAirFlowUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataFlowUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataWaterFlowUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataCapacityUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataDesInletAirTemp );
	EXPECT_DOUBLE_EQ( 0.0, DataDesOutletAirTemp );
	EXPECT_DOUBLE_EQ( 0.0, DataDesOutletAirHumRat );
	EXPECT_DOUBLE_EQ( 0.0, DataDesInletAirHumRat );
	EXPECT_DOUBLE_EQ( 0.0, DataDesInletWaterTemp );

	// size heating coil
	CurZoneEqNum = 0;
	CurSysNum = 1;
	CurOASysNum = 0;
	FinalSysSizing( 1 ).DesMainVolFlow = 0.00359;
	FinalSysSizing( 1 ).MassFlowAtCoolPeak = FinalSysSizing( 1 ).DesMainVolFlow * StdRhoAir;
	AirLoopControlInfo( 1 ).UnitarySys = true;

	WaterCoil( CoilNum ).Name = "Test Water Heating Coil";
	WaterCoil( CoilNum ).WaterCoilType = CoilType_Heating;
	WaterCoil( CoilNum ).DesAirVolFlowRate = AutoSize;
	WaterCoil( CoilNum ).DesInletAirTemp = AutoSize;
	WaterCoil( CoilNum ).DesOutletAirTemp = AutoSize;
	WaterCoil( CoilNum ).DesInletWaterTemp = AutoSize;
	WaterCoil( CoilNum ).DesInletAirHumRat = AutoSize;
	WaterCoil( CoilNum ).DesOutletAirHumRat = AutoSize;
	WaterCoil( CoilNum ).MaxWaterVolFlowRate = AutoSize;

	SizeWaterCoil( CoilNum );

	EXPECT_DOUBLE_EQ( 0.00359, WaterCoil( CoilNum ).DesAirVolFlowRate );
	EXPECT_EQ( 0, DataPltSizCoolNum );
	EXPECT_EQ( 0, DataWaterLoopNum );
	EXPECT_DOUBLE_EQ( 0.0, DataConstantUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataFractionUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataAirFlowUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataFlowUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataWaterFlowUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataCapacityUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataDesInletAirTemp );
	EXPECT_DOUBLE_EQ( 0.0, DataDesOutletAirTemp );
	EXPECT_DOUBLE_EQ( 0.0, DataDesOutletAirHumRat );
	EXPECT_DOUBLE_EQ( 0.0, DataDesInletAirHumRat );
	EXPECT_DOUBLE_EQ( 0.0, DataDesInletWaterTemp );

	// size 2nd heating coil
	FinalSysSizing( 1 ).DesMainVolFlow = 0.00459;
	FinalSysSizing( 1 ).MassFlowAtCoolPeak = FinalSysSizing( 1 ).DesMainVolFlow * StdRhoAir;

	WaterCoil( CoilNum ).Name = "Test Water Heating Coil 2";
	WaterCoil( CoilNum ).WaterCoilType = CoilType_Heating;
	WaterCoil( CoilNum ).DesAirVolFlowRate = AutoSize;
	WaterCoil( CoilNum ).DesInletAirTemp = AutoSize;
	WaterCoil( CoilNum ).DesOutletAirTemp = AutoSize;
	WaterCoil( CoilNum ).DesInletWaterTemp = AutoSize;
	WaterCoil( CoilNum ).DesInletAirHumRat = AutoSize;
	WaterCoil( CoilNum ).DesOutletAirHumRat = AutoSize;
	WaterCoil( CoilNum ).MaxWaterVolFlowRate = AutoSize;

	SizeWaterCoil( CoilNum );

	EXPECT_DOUBLE_EQ( 0.00459, WaterCoil( CoilNum ).DesAirVolFlowRate );
	EXPECT_EQ( 0, DataPltSizCoolNum );
	EXPECT_EQ( 0, DataWaterLoopNum );
	EXPECT_DOUBLE_EQ( 0.0, DataConstantUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataFractionUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataAirFlowUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataFlowUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataWaterFlowUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataCapacityUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataDesInletAirTemp );
	EXPECT_DOUBLE_EQ( 0.0, DataDesOutletAirTemp );
	EXPECT_DOUBLE_EQ( 0.0, DataDesOutletAirHumRat );
	EXPECT_DOUBLE_EQ( 0.0, DataDesInletAirHumRat );
	EXPECT_DOUBLE_EQ( 0.0, DataDesInletWaterTemp );

	// size zone water heating coil
	CurZoneEqNum = 1;
	CurSysNum = 0;
	PlantSizData( 1 ).ExitTemp = 60.0;
	DataSizing::NumZoneSizingInput = 1;
	DataSizing::ZoneSizingRunDone = true;
	ZoneSizingInput( 1 ).ZoneNum = CurZoneEqNum;
	ZoneEqSizing( 1 ).SizingMethod.allocate( 25 );
	ZoneEqSizing( 1 ).SizingMethod( DataHVACGlobals::SystemAirflowSizing ) = DataSizing::SupplyAirFlowRate;
	FinalZoneSizing( 1 ).ZoneTempAtHeatPeak = 20.0;
	FinalZoneSizing( 1 ).OutTempAtHeatPeak = -20.0;
	FinalZoneSizing( 1 ).DesHeatCoilInTemp = -20.0; // simulates zone heating air flow rate <= zone OA flow rate
	FinalZoneSizing( 1 ).DesHeatCoilInHumRat = 0.005;
	FinalZoneSizing( 1 ).HeatDesTemp = 30.0;
	FinalZoneSizing( 1 ).HeatDesHumRat = 0.005;
	ZoneEqSizing( 1 ).OAVolFlow = 0.01;
	ZoneEqSizing( 1 ).HeatingAirFlow = true;
	ZoneEqSizing( 1 ).HeatingAirVolFlow = 0.1;
	FinalZoneSizing( 1 ).DesHeatMassFlow = StdRhoAir * ZoneEqSizing( 1 ).HeatingAirVolFlow;
	MySizeFlag.allocate( 1 );
	MySizeFlag( CoilNum ) = true;
	MyUAAndFlowCalcFlag.allocate( 1 );
	MyUAAndFlowCalcFlag( CoilNum ) = true;

	WaterCoil( CoilNum ).UACoil = AutoSize;
	WaterCoil( CoilNum ).DesTotWaterCoilLoad = AutoSize;
	WaterCoil( CoilNum ).DesAirVolFlowRate = AutoSize;
	WaterCoil( CoilNum ).DesInletAirTemp = AutoSize;
	WaterCoil( CoilNum ).DesOutletAirTemp = AutoSize;
	WaterCoil( CoilNum ).DesInletWaterTemp = AutoSize;
	WaterCoil( CoilNum ).DesInletAirHumRat = AutoSize;
	WaterCoil( CoilNum ).DesOutletAirHumRat = AutoSize;
	WaterCoil( CoilNum ).MaxWaterVolFlowRate = AutoSize;

	SizeWaterCoil( CoilNum );
	EXPECT_NEAR( WaterCoil( CoilNum ).InletAirTemp, 16.0, 0.0001 ); // a mixture of zone air (20 C) and 10% OA (-20 C) = 16 C
	EXPECT_NEAR( WaterCoil( CoilNum ).DesTotWaterCoilLoad, 1709.8638, 0.0001 );
	EXPECT_NEAR( WaterCoil( CoilNum ).UACoil, 51.3278, 0.0001 );
	EXPECT_NEAR( WaterCoil( CoilNum ).OutletAirTemp, 30.1302, 0.0001 ); // reasonable delta T above inlet air temp

}

TEST_F( WaterCoilsTest, TdbFnHRhPbTest )
{

    // using IP PsyCalc
    //   http://linricsoftw.web701.discountasp.net/webpsycalc.aspx

	EXPECT_NEAR( 25.0, TdbFnHRhPb( 45170., 0.40, 101312. ), 0.05 );
	EXPECT_NEAR( 20.0, TdbFnHRhPb( 34760., 0.40, 101312. ), 0.05 );
	EXPECT_NEAR( 25.0, TdbFnHRhPb( 50290., 0.50, 101312. ), 0.05 );
	EXPECT_NEAR( 20.0, TdbFnHRhPb( 38490., 0.50, 101312. ), 0.05 );

}

TEST_F( WaterCoilsTest, CoilHeatingWaterUASizing )
{
	OutBaroPress = 101325.0;
	StdRhoAir = PsyRhoAirFnPbTdbW( OutBaroPress, 20.0, 0.0 );

	// set up sizing flags
	SysSizingRunDone = true;

	// set up plant sizing
	NumPltSizInput = 1;
	PlantSizData( 1 ).PlantLoopName = "HotWaterLoop";
	PlantSizData( 1 ).ExitTemp = 60.0;
	PlantSizData( 1 ).DeltaT = 10.0;

	// set up plant loop
	for ( int l = 1; l <= TotNumLoops; ++l ) {
		auto & loop( PlantLoop( l ) );
		loop.LoopSide.allocate( 2 );
		auto & loopside( PlantLoop( 1 ).LoopSide( 1 ) );
		loopside.TotalBranches = 1;
		loopside.Branch.allocate( 1 );
		auto & loopsidebranch( PlantLoop( 1 ).LoopSide(1).Branch( 1 ) );
		loopsidebranch.TotalComponents = 1;
		loopsidebranch.Comp.allocate( 1 );
	}
	PlantLoop( 1 ).Name = "HotWaterLoop";
	PlantLoop( 1 ).FluidName = "FluidWaterLoop";
	PlantLoop( 1 ).FluidIndex = 1;
	PlantLoop( 1 ).FluidName = "WATER";
	PlantLoop( 1 ).FluidIndex = 1;

	// set up sizing data
	FinalSysSizing( 1 ).DesMainVolFlow = 1.00;
	FinalSysSizing( 1 ).HeatSupTemp = 40.0;
	FinalSysSizing( 1 ).HeatOutTemp = 5.0;
	FinalSysSizing( 1 ).HeatRetTemp = 20.0;
	FinalSysSizing( 1 ).HeatOAOption = 1;

	// set up water coil
	int CoilNum = 1;
	WaterCoil( CoilNum ).Name = "Water Heating Coil";
	WaterCoil( CoilNum ).WaterLoopNum = 1;
	WaterCoil( CoilNum ).WaterCoilType = CoilType_Heating;
	WaterCoil( CoilNum ).WaterCoilType_Num = WaterCoil_SimpleHeating;  // Coil:Heating:Water
	WaterCoil( CoilNum ).WaterCoilModel = CoilType_Heating;
	WaterCoil( CoilNum ).SchedPtr = DataGlobals::ScheduleAlwaysOn;

	WaterCoil( CoilNum ).RequestingAutoSize = true;
	WaterCoil( CoilNum ).DesAirVolFlowRate = AutoSize;
	WaterCoil( CoilNum ).UACoil = AutoSize;
	WaterCoil( CoilNum ).MaxWaterVolFlowRate = AutoSize;
	WaterCoil( CoilNum ).CoilPerfInpMeth = UAandFlow;
	WaterCoil( CoilNum ).DesInletAirTemp = AutoSize;
	WaterCoil( CoilNum ).DesOutletAirTemp = AutoSize;
	WaterCoil( CoilNum ).DesInletWaterTemp = AutoSize;
	WaterCoil( CoilNum ).DesInletAirHumRat = AutoSize;
	WaterCoil( CoilNum ).DesOutletAirHumRat = AutoSize;

	WaterCoilNumericFields( CoilNum ).FieldNames( 2 ) = "Maximum Water Flow Rate";
	WaterCoil( CoilNum ).WaterInletNodeNum = 1;
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = WaterCoil( CoilNum ).WaterInletNodeNum;

	CurZoneEqNum = 0;
	CurSysNum = 1;
	CurOASysNum = 0;
	DataWaterLoopNum = 1;
	NumOfGlycols = 1;

	MyUAAndFlowCalcFlag.allocate( 1 );
	MyUAAndFlowCalcFlag( 1 ) =  true;

	MySizeFlag.allocate( 1 );
	MySizeFlag( 1 ) =  true;

	// run water coil sizing
	createCoilSelectionReportObj();
	SizeWaterCoil( CoilNum );
	EXPECT_DOUBLE_EQ( 1.0, WaterCoil( CoilNum ).DesAirVolFlowRate );

	Real64 CpAirStd = 0.0;
	Real64 DesMassFlow = 0.0;
	Real64 DesCoilHeatingLoad = 0.0;

	CpAirStd = PsyCpAirFnWTdb(0.0, 20.0);
	DesMassFlow = WaterCoil( CoilNum ).DesAirVolFlowRate * StdRhoAir;
	DesCoilHeatingLoad = CpAirStd * DesMassFlow * ( 40.0 - 5.0 );

	// check heating coil design load
	EXPECT_DOUBLE_EQ( DesCoilHeatingLoad, WaterCoil( CoilNum ).DesWaterHeatingCoilRate );

	Real64 Cp = 0;
	Real64 rho = 0;
	Real64 DesWaterFlowRate = 0;

	Cp = GetSpecificHeatGlycol( PlantLoop( 1 ).FluidName, DataGlobals::HWInitConvTemp, PlantLoop( 1 ).FluidIndex, "Unit Test" );
	rho = GetDensityGlycol( PlantLoop( 1 ).FluidName, DataGlobals::CWInitConvTemp, PlantLoop( 1 ).FluidIndex, "Unit Test" );
	DesWaterFlowRate = WaterCoil( CoilNum ).DesWaterHeatingCoilRate / ( 10.0 * Cp * rho );

	// check heating coil design water flow rate
	EXPECT_DOUBLE_EQ( DesWaterFlowRate, WaterCoil( CoilNum ).MaxWaterVolFlowRate );

	// check coil UA-value sizing
	EXPECT_NEAR( 1439.30, WaterCoil( CoilNum ).UACoil, 0.01 );

	// test single zone VAV reheat coil sizing
	CurZoneEqNum = 1;
	CurTermUnitSizingNum = 1;
	CurSysNum = 0;
	TermUnitSizing.allocate( 1 );
	TermUnitFinalZoneSizing.allocate( 1 );
	TermUnitSizing( CurTermUnitSizingNum ).AirVolFlow = WaterCoil( CoilNum ).DesAirVolFlowRate / 3.0; // DesAirVolFlowRate = 1.0
	TermUnitSizing( CurTermUnitSizingNum ).MaxHWVolFlow = WaterCoil( CoilNum ).MaxWaterVolFlowRate / 3.0;
	TermUnitSizing( CurTermUnitSizingNum ).MinFlowFrac = 0.5;
	DataSizing::TermUnitSingDuct = true;

	WaterCoil( CoilNum ).DesAirVolFlowRate = AutoSize;
	WaterCoil( CoilNum ).UACoil = AutoSize;
	WaterCoil( CoilNum ).MaxWaterVolFlowRate = AutoSize;
	WaterCoil( CoilNum ).CoilPerfInpMeth = UAandFlow;
	WaterCoil( CoilNum ).DesInletAirTemp = AutoSize;
	WaterCoil( CoilNum ).DesOutletAirTemp = AutoSize;
	WaterCoil( CoilNum ).DesInletWaterTemp = AutoSize;
	WaterCoil( CoilNum ).DesInletAirHumRat = AutoSize;
	WaterCoil( CoilNum ).DesOutletAirHumRat = AutoSize;

	SysSizingRunDone = false;
	ZoneSizingRunDone = true;
	NumZoneSizingInput = 1;
	ZoneSizingInput.allocate( 1 );
	ZoneSizingInput( 1 ).ZoneNum = 1;
	ZoneEqSizing.allocate( 1 );
	ZoneEqSizing( CurZoneEqNum ).SizingMethod.allocate( 20 );
	ZoneEqSizing( CurZoneEqNum ).SizingMethod( DataHVACGlobals::HeatingAirflowSizing ) = DataHVACGlobals::HeatingAirflowSizing;
	ZoneEqSizing( CurZoneEqNum ).CoolingAirVolFlow = 0.0;
	ZoneEqSizing( CurZoneEqNum ).HeatingAirVolFlow = 1.0;
	FinalZoneSizing.allocate( 1 );
	FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow = 0.0;
	FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow = 1.0;
	FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTempTU = 10.0;
	FinalZoneSizing( CurZoneEqNum ).ZoneTempAtHeatPeak = 21.0;
	FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInHumRatTU = 0.006;
	FinalZoneSizing( CurZoneEqNum ).ZoneHumRatAtHeatPeak = 0.008;
	TermUnitFinalZoneSizing( CurTermUnitSizingNum ) = FinalZoneSizing( CurZoneEqNum );


	MySizeFlag( 1 ) = true;
	// run water coil sizing
	SizeWaterCoil( CoilNum );

	// check coil UA-value sizing
	EXPECT_NEAR( 558.656, WaterCoil( CoilNum ).UACoil, 0.01 ); // smaller UA than result above at 1435.00

}

TEST_F( WaterCoilsTest, CoilHeatingWaterLowAirFlowUASizing ) {
	OutBaroPress = 101325.0;
	StdRhoAir = PsyRhoAirFnPbTdbW( OutBaroPress, 20.0, 0.0 );


	// set up sizing flags
	SysSizingRunDone = true;

	// set up plant sizing
	NumPltSizInput = 1;
	PlantSizData( 1 ).PlantLoopName = "HotWaterLoop";
	PlantSizData( 1 ).ExitTemp = 60.0;
	PlantSizData( 1 ).DeltaT = 10.0;

	// set up plant loop
	for( int l = 1; l <= TotNumLoops; ++l ) {
		auto & loop( PlantLoop( l ) );
		loop.LoopSide.allocate( 2 );
		auto & loopside( PlantLoop( 1 ).LoopSide( 1 ) );
		loopside.TotalBranches = 1;
		loopside.Branch.allocate( 1 );
		auto & loopsidebranch( PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ) );
		loopsidebranch.TotalComponents = 1;
		loopsidebranch.Comp.allocate( 1 );
	}
	PlantLoop( 1 ).Name = "HotWaterLoop";
	PlantLoop( 1 ).FluidName = "FluidWaterLoop";
	PlantLoop( 1 ).FluidIndex = 1;
	PlantLoop( 1 ).FluidName = "WATER";
	PlantLoop( 1 ).FluidIndex = 1;

	// set up sizing data
	FinalSysSizing( 1 ).DesMainVolFlow = 1.00;
	FinalSysSizing( 1 ).HeatSupTemp = 40.0;
	FinalSysSizing( 1 ).HeatOutTemp = 5.0;
	FinalSysSizing( 1 ).HeatRetTemp = 20.0;
	FinalSysSizing( 1 ).HeatOAOption = 1;

	// set up water coil
	int CoilNum = 1;
	WaterCoil( CoilNum ).Name = "Water Heating Coil";
	WaterCoil( CoilNum ).WaterLoopNum = 1;
	WaterCoil( CoilNum ).WaterCoilType = CoilType_Heating;
	WaterCoil( CoilNum ).WaterCoilType_Num = WaterCoil_SimpleHeating;  // Coil:Heating:Water
	WaterCoil( CoilNum ).WaterCoilModel = CoilType_Heating;
	WaterCoil( CoilNum ).SchedPtr = DataGlobals::ScheduleAlwaysOn;

	WaterCoil( CoilNum ).RequestingAutoSize = true;
	WaterCoil( CoilNum ).DesAirVolFlowRate = AutoSize;
	WaterCoil( CoilNum ).UACoil = AutoSize;
	WaterCoil( CoilNum ).MaxWaterVolFlowRate = AutoSize;
	WaterCoil( CoilNum ).CoilPerfInpMeth = UAandFlow;
	WaterCoil( CoilNum ).DesInletAirTemp = AutoSize;
	WaterCoil( CoilNum ).DesOutletAirTemp = AutoSize;
	WaterCoil( CoilNum ).DesInletWaterTemp = AutoSize;
	WaterCoil( CoilNum ).DesInletAirHumRat = AutoSize;
	WaterCoil( CoilNum ).DesOutletAirHumRat = AutoSize;

	WaterCoilNumericFields( CoilNum ).FieldNames( 2 ) = "Maximum Water Flow Rate";
	WaterCoil( CoilNum ).WaterInletNodeNum = 1;
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = WaterCoil( CoilNum ).WaterInletNodeNum;

	CurZoneEqNum = 0;
	CurSysNum = 1;
	CurOASysNum = 0;
	DataWaterLoopNum = 1;
	NumOfGlycols = 1;

	MyUAAndFlowCalcFlag.allocate( 1 );
	MyUAAndFlowCalcFlag( 1 ) = true;

	MySizeFlag.allocate( 1 );
	MySizeFlag( 1 ) = true;

	// run water coil sizing
	createCoilSelectionReportObj();
	SizeWaterCoil( CoilNum );
	EXPECT_DOUBLE_EQ( 1.0, WaterCoil( CoilNum ).DesAirVolFlowRate );

	Real64 CpAirStd = 0.0;
	Real64 DesMassFlow = 0.0;
	Real64 DesCoilHeatingLoad = 0.0;

	CpAirStd = PsyCpAirFnWTdb( 0.0, 20.0 );
	DesMassFlow = WaterCoil( CoilNum ).DesAirVolFlowRate * StdRhoAir;
	DesCoilHeatingLoad = CpAirStd * DesMassFlow * ( 40.0 - 5.0 );

	// check heating coil design load
	EXPECT_DOUBLE_EQ( DesCoilHeatingLoad, WaterCoil( CoilNum ).DesWaterHeatingCoilRate );

	Real64 Cp = 0;
	Real64 rho = 0;
	Real64 DesWaterFlowRate = 0;

	Cp = GetSpecificHeatGlycol( PlantLoop( 1 ).FluidName, DataGlobals::HWInitConvTemp, PlantLoop( 1 ).FluidIndex, "Unit Test" );
	rho = GetDensityGlycol( PlantLoop( 1 ).FluidName, DataGlobals::CWInitConvTemp, PlantLoop( 1 ).FluidIndex, "Unit Test" );
	DesWaterFlowRate = WaterCoil( CoilNum ).DesWaterHeatingCoilRate / ( 10.0 * Cp * rho );

	// check heating coil design water flow rate
	EXPECT_DOUBLE_EQ( DesWaterFlowRate, WaterCoil( CoilNum ).MaxWaterVolFlowRate );

	// check coil UA-value sizing
	EXPECT_NEAR( 1439.30, WaterCoil( CoilNum ).UACoil, 0.01 );

	// test single zone VAV reheat coil sizing
	CurZoneEqNum = 1;
	CurSysNum = 0;
	CurTermUnitSizingNum = 1;
	TermUnitSizing.allocate( 1 );
	TermUnitFinalZoneSizing.allocate( 1 );
	TermUnitSizing( CurTermUnitSizingNum ).AirVolFlow = WaterCoil( CoilNum ).DesAirVolFlowRate / 1500.0; // DesAirVolFlowRate = 1.0 so TU air flow = 0.00067 (lower than 0.001)
	TermUnitSizing( CurTermUnitSizingNum ).MaxHWVolFlow = WaterCoil( CoilNum ).MaxWaterVolFlowRate / 1500.0;
	TermUnitSizing( CurTermUnitSizingNum ).MinFlowFrac = 0.5;
	DataSizing::TermUnitSingDuct = true;

	WaterCoil( CoilNum ).DesAirVolFlowRate = AutoSize;
	WaterCoil( CoilNum ).UACoil = AutoSize;
	WaterCoil( CoilNum ).MaxWaterVolFlowRate = AutoSize;
	WaterCoil( CoilNum ).CoilPerfInpMeth = UAandFlow;
	WaterCoil( CoilNum ).DesInletAirTemp = AutoSize;
	WaterCoil( CoilNum ).DesOutletAirTemp = AutoSize;
	WaterCoil( CoilNum ).DesInletWaterTemp = AutoSize;
	WaterCoil( CoilNum ).DesInletAirHumRat = AutoSize;
	WaterCoil( CoilNum ).DesOutletAirHumRat = AutoSize;

	SysSizingRunDone = false;
	ZoneSizingRunDone = true;
	NumZoneSizingInput = 1;
	ZoneSizingInput.allocate( 1 );
	ZoneSizingInput( 1 ).ZoneNum = 1;
	ZoneEqSizing.allocate( 1 );
	ZoneEqSizing( CurZoneEqNum ).SizingMethod.allocate( 20 );
	ZoneEqSizing( CurZoneEqNum ).SizingMethod( DataHVACGlobals::HeatingAirflowSizing ) = DataHVACGlobals::HeatingAirflowSizing;
	ZoneEqSizing( CurZoneEqNum ).CoolingAirVolFlow = 0.0;
	ZoneEqSizing( CurZoneEqNum ).HeatingAirVolFlow = 1.0;
	FinalZoneSizing.allocate( 1 );
	FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow = 0.0;
	FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow = 0.00095;
	FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTempTU = 10.0;
	FinalZoneSizing( CurZoneEqNum ).ZoneTempAtHeatPeak = 21.0;
	FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInHumRatTU = 0.006;
	FinalZoneSizing( CurZoneEqNum ).ZoneHumRatAtHeatPeak = 0.008;
	TermUnitFinalZoneSizing( CurTermUnitSizingNum ) = FinalZoneSizing( CurZoneEqNum );

	MySizeFlag( 1 ) = true;
	// run water coil sizing
	SizeWaterCoil( CoilNum );

	// water flow rate should be non-zero, and air flow rate being so small will get set to 0 during sizing
	EXPECT_GT( WaterCoil( CoilNum ).InletWaterMassFlowRate, 0.0 );
	EXPECT_EQ( 0.0, WaterCoil( CoilNum ).InletAirMassFlowRate );
	// check coil UA-value sizing
	EXPECT_NEAR( 1.0, WaterCoil( CoilNum ).UACoil, 0.0001 ); // TU air flow is too low to size, set to 0, so UA is set to 1.0

}

TEST_F( WaterCoilsTest, CoilHeatingWaterUASizingLowHwaterInletTemp )
{
	OutBaroPress = 101325.0;
	StdRhoAir = PsyRhoAirFnPbTdbW( OutBaroPress, 20.0, 0.0 );

	// set up sizing flags
	SysSizingRunDone = true;

	// set up plant sizing
	NumPltSizInput = 1;
	PlantSizData( 1 ).PlantLoopName = "HotWaterLoop";
	PlantSizData( 1 ).ExitTemp = 40.0; // low heating coil inlet water temp
	PlantSizData( 1 ).DeltaT = 10.0;

	// set up plant loop
	for ( int l = 1; l <= TotNumLoops; ++l ) {
		auto & loop( PlantLoop( l ) );
		loop.LoopSide.allocate( 2 );
		auto & loopside( PlantLoop( 1 ).LoopSide( 1 ) );
		loopside.TotalBranches = 1;
		loopside.Branch.allocate( 1 );
		auto & loopsidebranch( PlantLoop( 1 ).LoopSide(1).Branch( 1 ) );
		loopsidebranch.TotalComponents = 1;
		loopsidebranch.Comp.allocate( 1 );
	}
	PlantLoop( 1 ).Name = "HotWaterLoop";
	PlantLoop( 1 ).FluidName = "FluidWaterLoop";
	PlantLoop( 1 ).FluidIndex = 1;
	PlantLoop( 1 ).FluidName = "WATER";
	PlantLoop( 1 ).FluidIndex = 1;

	// set up sizing data
	FinalSysSizing( 1 ).DesMainVolFlow = 1.00;
	FinalSysSizing( 1 ).HeatSupTemp = 40.0;
	FinalSysSizing( 1 ).HeatOutTemp = 5.0;
	FinalSysSizing( 1 ).HeatRetTemp = 20.0;
	FinalSysSizing( 1 ).HeatOAOption = 1;

	// set up water coil
	int CoilNum = 1;
	WaterCoil( CoilNum ).Name = "Water Heating Coil";
	WaterCoil( CoilNum ).WaterLoopNum = 1;
	WaterCoil( CoilNum ).WaterCoilType = CoilType_Heating;
	WaterCoil( CoilNum ).WaterCoilType_Num = WaterCoil_SimpleHeating;  // Coil:Heating:Water
	WaterCoil( CoilNum ).WaterCoilModel = CoilType_Heating;
	WaterCoil( CoilNum ).SchedPtr = DataGlobals::ScheduleAlwaysOn;

	WaterCoil( CoilNum ).RequestingAutoSize = true;
	WaterCoil( CoilNum ).DesAirVolFlowRate = AutoSize;
	WaterCoil( CoilNum ).UACoil = AutoSize;
	WaterCoil( CoilNum ).MaxWaterVolFlowRate = AutoSize;
	WaterCoil( CoilNum ).CoilPerfInpMeth = UAandFlow;
	WaterCoil( CoilNum ).DesInletAirTemp = AutoSize;
	WaterCoil( CoilNum ).DesOutletAirTemp = AutoSize;
	WaterCoil( CoilNum ).DesInletWaterTemp = AutoSize;
	WaterCoil( CoilNum ).DesInletAirHumRat = AutoSize;
	WaterCoil( CoilNum ).DesOutletAirHumRat = AutoSize;

	WaterCoilNumericFields( CoilNum ).FieldNames( 2 ) = "Maximum Water Flow Rate";
	WaterCoil( CoilNum ).WaterInletNodeNum = 1;
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = WaterCoil( CoilNum ).WaterInletNodeNum;

	CurZoneEqNum = 0;
	CurSysNum = 1;
	CurOASysNum = 0;

	DataWaterLoopNum = 1;
	NumOfGlycols = 1;

	MyUAAndFlowCalcFlag.allocate( 1 );
	MyUAAndFlowCalcFlag( 1 ) =  true;

	MySizeFlag.allocate( 1 );
	MySizeFlag( 1 ) =  true;

	// run water coil sizing
	createCoilSelectionReportObj();
	SizeWaterCoil( CoilNum );
	EXPECT_DOUBLE_EQ( 1.0, WaterCoil( CoilNum ).DesAirVolFlowRate );

	Real64 CpAirStd = 0.0;
	Real64 DesMassFlow = 0.0;
	Real64 DesCoilHeatingLoad = 0.0;

	CpAirStd = PsyCpAirFnWTdb(0.0, 20.0);
	DesMassFlow = WaterCoil( CoilNum ).DesAirVolFlowRate * StdRhoAir;
	DesCoilHeatingLoad = CpAirStd * DesMassFlow * ( 40.0 - 5.0 );

	// check heating coil design load
	EXPECT_DOUBLE_EQ( DesCoilHeatingLoad, WaterCoil( CoilNum ).DesWaterHeatingCoilRate );

	Real64 Cp = 0;
	Real64 rho = 0;
	Real64 DesWaterFlowRate = 0;

	Cp = GetSpecificHeatGlycol( PlantLoop( 1 ).FluidName, DataGlobals::HWInitConvTemp, PlantLoop( 1 ).FluidIndex, "Unit Test" );
	rho = GetDensityGlycol( PlantLoop( 1 ).FluidName, DataGlobals::CWInitConvTemp, PlantLoop( 1 ).FluidIndex, "Unit Test" );
	DesWaterFlowRate = WaterCoil( CoilNum ).DesWaterHeatingCoilRate / ( 10.0 * Cp * rho );

	// check heating coil design water flow rate
	EXPECT_DOUBLE_EQ( DesWaterFlowRate, WaterCoil( CoilNum ).MaxWaterVolFlowRate );

	// check coil UA-value sizing for low design loop exit temp
	EXPECT_NEAR( 2491.37, WaterCoil( CoilNum ).UACoil, 0.01 );

	Real64 DesCoilInletWaterTempUsed = 0.0;
	Real64 DataFanOpMode = ContFanCycCoil;
	Real64 UAMax = WaterCoil( CoilNum ).DesWaterHeatingCoilRate;

	// check if coil design inlet water temperature is increased above the plant loop exit temp
	EstimateCoilInletWaterTemp( CoilNum, DataFanOpMode, 1.0, UAMax, DesCoilInletWaterTempUsed );
	EXPECT_GT( DesCoilInletWaterTempUsed, PlantSizData( 1 ).ExitTemp );
	EXPECT_NEAR( 48.73, DesCoilInletWaterTempUsed, 0.01 );
}

TEST_F( WaterCoilsTest, CoilCoolingWaterSimpleSizing )
 {
 	InitializePsychRoutines();
 	OutBaroPress = 101325.0;
 	StdRhoAir = PsyRhoAirFnPbTdbW( OutBaroPress, 20.0, 0.0 );
 	ShowMessage( "Begin Test: WaterCoilsTest, CoilCoolingWaterSimpleSizing" );

 	// set up sizing flags
 	SysSizingRunDone = true;

 	// set up plant sizing
 	NumPltSizInput = 1;
 	PlantSizData( 1 ).PlantLoopName = "WaterLoop";

 	// set up plant loop
 	for ( int l = 1; l <= TotNumLoops; ++l ) {
 		auto & loop( PlantLoop( l ) );
 		loop.LoopSide.allocate( 2 );
 		auto & loopside( PlantLoop( 1 ).LoopSide( 1 ) );
 		loopside.TotalBranches = 1;
 		loopside.Branch.allocate( 1 );
 		auto & loopsidebranch( PlantLoop( 1 ).LoopSide(1).Branch( 1 ) );
 		loopsidebranch.TotalComponents = 1;
 		loopsidebranch.Comp.allocate( 1 );
 	}
 	PlantLoop( 1 ).Name = "WaterLoop";
 	PlantLoop( 1 ).FluidName = "FluidWaterLoop";
 	PlantLoop( 1 ).FluidIndex = 1;
 	PlantLoop( 1 ).FluidName = "WATER";
 	PlantLoop( 1 ).FluidIndex = 1;

 	// set up sizing data
 	FinalSysSizing( 1 ).MixTempAtCoolPeak = 20.0;
 	FinalSysSizing( 1 ).MixHumRatAtCoolPeak = 0.01;
 	FinalSysSizing( 1 ).CoolSupTemp = 10.0;
 	FinalSysSizing( 1 ).CoolSupHumRat = 0.0085;
 	FinalSysSizing( 1 ).DesMainVolFlow = 1.00;
 	FinalSysSizing( 1 ).MassFlowAtCoolPeak = FinalSysSizing( 1 ).DesMainVolFlow * StdRhoAir;

 	// set up water coil
 	int CoilNum = 1;
 	WaterCoil( CoilNum ).Name = "Test Simple Water Cooling Coil";
 	WaterCoil( CoilNum ).WaterLoopNum = 1;
 	WaterCoil( CoilNum ).WaterCoilType = CoilType_Cooling;
 	WaterCoil( CoilNum ).WaterCoilType_Num = WaterCoil_Cooling;  // Coil:Cooling:Water
 	WaterCoil( CoilNum ).WaterCoilModel = CoilModel_Cooling;

 	WaterCoil( CoilNum ).RequestingAutoSize = true;
 	WaterCoil( CoilNum ).DesAirVolFlowRate = AutoSize;
 	WaterCoil( CoilNum ).DesInletAirTemp = AutoSize;
 	WaterCoil( CoilNum ).DesOutletAirTemp = AutoSize;
 	WaterCoil( CoilNum ).DesInletWaterTemp = AutoSize;
 	WaterCoil( CoilNum ).DesInletAirHumRat = AutoSize;
 	WaterCoil( CoilNum ).DesOutletAirHumRat = AutoSize;
 	WaterCoil( CoilNum ).MaxWaterVolFlowRate = AutoSize;

 	WaterCoil( CoilNum ).DesignWaterDeltaTemp = 6.67;
 	WaterCoil( CoilNum ).UseDesignWaterDeltaTemp = true;

 	WaterCoilNumericFields( CoilNum ).FieldNames( 1 ) = "Design Water Flow Rate";
 	WaterCoil( CoilNum ).WaterInletNodeNum = 1;
 	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = WaterCoil( CoilNum ).WaterInletNodeNum;

 	CurZoneEqNum = 0;
 	CurSysNum = 1;
 	CurOASysNum = 0;
 	SysSizInput( 1 ).CoolCapControl = VAV;
 	PlantSizData( 1 ).ExitTemp = 5.7;
 	PlantSizData( 1 ).DeltaT = 5.0;

 	DataWaterLoopNum = 1;
 	NumOfGlycols = 1;

 	// run water coil sizing
 	createCoilSelectionReportObj();
	SizeWaterCoil( CoilNum );
 	EXPECT_DOUBLE_EQ( 1.0, WaterCoil( CoilNum ).DesAirVolFlowRate );

 	Real64 DesCoilCoolingLoad = 0.0;
 	Real64 CoilInEnth = 0.0;
 	Real64 CoilOutEnth = 0.0;

 	CoilInEnth = PsyHFnTdbW ( 20.0, 0.01 );
 	CoilOutEnth = PsyHFnTdbW ( 10.0, 0.0085 );
 	DesCoilCoolingLoad = WaterCoil( CoilNum ).DesAirVolFlowRate * StdRhoAir * ( CoilInEnth - CoilOutEnth );

 	// check cooling coil design load
 	EXPECT_DOUBLE_EQ( DesCoilCoolingLoad, WaterCoil( CoilNum ).DesWaterCoolingCoilRate );

 	Real64 Cp = 0;
 	Real64 rho = 0;
 	Real64 DesWaterFlowRate = 0;

 	Cp = GetSpecificHeatGlycol( PlantLoop( 1 ).FluidName, 5.0, PlantLoop( 1 ).FluidIndex, "Unit Test" );
 	rho = GetDensityGlycol( PlantLoop( 1 ).FluidName, DataGlobals::CWInitConvTemp, PlantLoop( 1 ).FluidIndex, "Unit Test" );
 	DesWaterFlowRate = WaterCoil( CoilNum ).DesWaterCoolingCoilRate / ( WaterCoil( CoilNum ).DesignWaterDeltaTemp * Cp * rho );

 	// check cooling coil design water flow rate
 	EXPECT_DOUBLE_EQ( DesWaterFlowRate, WaterCoil( CoilNum ).MaxWaterVolFlowRate );

 }

 TEST_F( WaterCoilsTest, CoilCoolingWaterDetailedSizing )
 {
 	InitializePsychRoutines();
 	OutBaroPress = 101325.0;
 	StdRhoAir = PsyRhoAirFnPbTdbW( OutBaroPress, 20.0, 0.0 );
 	ShowMessage( "Begin Test: WaterCoilsTest, CoilCoolingWaterDetailedSizing" );

 	// set up sizing flags
 	SysSizingRunDone = true;

 	// set up plant sizing
 	NumPltSizInput = 1;
 	PlantSizData( 1 ).PlantLoopName = "WaterLoop";

 	// set up plant loop
 	for ( int l = 1; l <= TotNumLoops; ++l ) {
 		auto & loop( PlantLoop( l ) );
 		loop.LoopSide.allocate( 2 );
 		auto & loopside( PlantLoop( 1 ).LoopSide( 1 ) );
 		loopside.TotalBranches = 1;
 		loopside.Branch.allocate( 1 );
 		auto & loopsidebranch( PlantLoop( 1 ).LoopSide(1).Branch( 1 ) );
 		loopsidebranch.TotalComponents = 1;
 		loopsidebranch.Comp.allocate( 1 );
 	}
 	PlantLoop( 1 ).Name = "WaterLoop";
 	PlantLoop( 1 ).FluidName = "FluidWaterLoop";
 	PlantLoop( 1 ).FluidIndex = 1;
 	PlantLoop( 1 ).FluidName = "WATER";
 	PlantLoop( 1 ).FluidIndex = 1;

 	// set up sizing data
 	FinalSysSizing( 1 ).MixTempAtCoolPeak = 20.0;
 	FinalSysSizing( 1 ).MixHumRatAtCoolPeak = 0.01;
 	FinalSysSizing( 1 ).CoolSupTemp = 10.0;
 	FinalSysSizing( 1 ).CoolSupHumRat = 0.0085;
 	FinalSysSizing( 1 ).DesMainVolFlow = 1.00;
 	FinalSysSizing( 1 ).MassFlowAtCoolPeak = FinalSysSizing( 1 ).DesMainVolFlow * StdRhoAir;

 	// set up water coil
 	int CoilNum = 1;
 	WaterCoil( CoilNum ).Name = "Test Detailed Water Cooling Coil";
 	WaterCoil( CoilNum ).WaterLoopNum = 1;
 	WaterCoil( CoilNum ).WaterCoilType = CoilType_Cooling;
 	WaterCoil( CoilNum ).WaterCoilType_Num = WaterCoil_DetFlatFinCooling;  // Coil:Cooling:Water:DetailedGeometry
 	WaterCoil( CoilNum ).WaterCoilModel = CoilModel_Detailed;

 	WaterCoil( CoilNum ).TubeOutsideSurfArea = 6.23816;
 	WaterCoil( CoilNum ).TotTubeInsideArea = 6.20007018;
 	WaterCoil( CoilNum ).FinSurfArea = 101.7158224;
 	WaterCoil( CoilNum ).MinAirFlowArea = 0.810606367;
 	WaterCoil( CoilNum ).CoilDepth = 0.165097968;
 	WaterCoil( CoilNum ).FinDiam = 0.43507152;
 	WaterCoil( CoilNum ).FinThickness = 0.001499982;
 	WaterCoil( CoilNum ).TubeInsideDiam = 0.014449958;
 	WaterCoil( CoilNum ).TubeOutsideDiam = 0.015879775;
 	WaterCoil( CoilNum ).TubeThermConductivity = 385.764854;
 	WaterCoil( CoilNum ).FinThermConductivity = 203.882537;
 	WaterCoil( CoilNum ).FinSpacing = 0.001814292;
 	WaterCoil( CoilNum ).TubeDepthSpacing = 0.02589977;
 	WaterCoil( CoilNum ).NumOfTubeRows = 6;
 	WaterCoil( CoilNum ).NumOfTubesPerRow = 16;
 	WaterCoil( CoilNum ).DesignWaterDeltaTemp = 6.67;
 	WaterCoil( CoilNum ).UseDesignWaterDeltaTemp = true;

 	WaterCoil( CoilNum ).RequestingAutoSize = true;
 	WaterCoil( CoilNum ).DesAirVolFlowRate = AutoSize;
 	WaterCoil( CoilNum ).MaxWaterVolFlowRate = AutoSize;
 	WaterCoil( CoilNum ).DesInletAirTemp = AutoSize;
 	WaterCoil( CoilNum ).DesOutletAirTemp = AutoSize;
 	WaterCoil( CoilNum ).DesInletWaterTemp = AutoSize;
 	WaterCoil( CoilNum ).DesInletAirHumRat = AutoSize;
 	WaterCoil( CoilNum ).DesOutletAirHumRat = AutoSize;
 	WaterCoil( CoilNum ).MaxWaterVolFlowRate = AutoSize;

 	WaterCoilNumericFields( CoilNum ).FieldNames( 1 ) = "Design Water Flow Rate";
 	WaterCoil( CoilNum ).WaterInletNodeNum = 1;
 	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = WaterCoil( CoilNum ).WaterInletNodeNum;

 	CurZoneEqNum = 0;
 	CurSysNum = 1;
 	CurOASysNum = 0;
 	SysSizInput( 1 ).CoolCapControl = VAV;
 	PlantSizData( 1 ).ExitTemp = 5.7;
 	PlantSizData( 1 ).DeltaT = 5.0;
 	DataWaterLoopNum = 1;
 	NumOfGlycols = 1;

 	// run water coil sizing
 	createCoilSelectionReportObj();
	SizeWaterCoil( CoilNum );
 	EXPECT_DOUBLE_EQ( 1.0, WaterCoil( CoilNum ).DesAirVolFlowRate );

 	Real64 DesCoilCoolingLoad = 0.0;
 	Real64 CoilInEnth = 0.0;
 	Real64 CoilOutEnth = 0.0;

 	CoilInEnth = PsyHFnTdbW ( FinalSysSizing( 1 ).MixTempAtCoolPeak, FinalSysSizing( 1 ).MixHumRatAtCoolPeak );
 	CoilOutEnth = PsyHFnTdbW ( FinalSysSizing( 1 ).CoolSupTemp, FinalSysSizing( 1 ).CoolSupHumRat );
 	DesCoilCoolingLoad = WaterCoil( CoilNum ).DesAirVolFlowRate * StdRhoAir * ( CoilInEnth - CoilOutEnth );
 	// check cooling coil design load
 	EXPECT_DOUBLE_EQ( DesCoilCoolingLoad, WaterCoil( CoilNum ).DesWaterCoolingCoilRate );

 	Real64 Cp = 0;
 	Real64 rho = 0;
 	Real64 DesWaterFlowRate = 0;

 	Cp = GetSpecificHeatGlycol( PlantLoop( 1 ).FluidName, 5.0, PlantLoop( 1 ).FluidIndex, "Unit Test" );
 	rho = GetDensityGlycol( PlantLoop( 1 ).FluidName, DataGlobals::CWInitConvTemp, PlantLoop( 1 ).FluidIndex, "Unit Test" );
 	DesWaterFlowRate = WaterCoil( CoilNum ).DesWaterCoolingCoilRate / ( 6.67 * Cp * rho );
 	// check cooling coil design water flow rate
 	EXPECT_DOUBLE_EQ( DesWaterFlowRate, WaterCoil( CoilNum ).MaxWaterVolFlowRate );

 }
 TEST_F( WaterCoilsTest, CoilHeatingWaterSimpleSizing )
 {
 	InitializePsychRoutines();
 	OutBaroPress = 101325.0;
 	StdRhoAir = PsyRhoAirFnPbTdbW( OutBaroPress, 20.0, 0.0 );
 	ShowMessage( "Begin Test: WaterCoilsTest, CoilHeatingWaterSimpleSizing" );

 	// set up sizing flags
 	SysSizingRunDone = true;

 	// set up plant sizing
 	NumPltSizInput = 1;
 	PlantSizData( 1 ).PlantLoopName = "WaterLoop";

 	// set up plant loop
 	for ( int l = 1; l <= TotNumLoops; ++l ) {
 		auto & loop( PlantLoop( l ) );
 		loop.LoopSide.allocate( 2 );
 		auto & loopside( PlantLoop( 1 ).LoopSide( 1 ) );
 		loopside.TotalBranches = 1;
 		loopside.Branch.allocate( 1 );
 		auto & loopsidebranch( PlantLoop( 1 ).LoopSide(1).Branch( 1 ) );
 		loopsidebranch.TotalComponents = 1;
 		loopsidebranch.Comp.allocate( 1 );
 	}
 	PlantLoop( 1 ).Name = "WaterLoop";
 	PlantLoop( 1 ).FluidName = "FluidWaterLoop";
 	PlantLoop( 1 ).FluidIndex = 1;
 	PlantLoop( 1 ).FluidName = "WATER";
 	PlantLoop( 1 ).FluidIndex = 1;

 	// set up sizing data
 	FinalSysSizing( 1 ).DesMainVolFlow = 1.00;
 	FinalSysSizing( 1 ).HeatSupTemp = 40.0;
 	FinalSysSizing( 1 ).HeatOutTemp = 5.0;
 	FinalSysSizing( 1 ).HeatRetTemp = 20.0;
 	FinalSysSizing( 1 ).HeatOAOption = 1;

 	// set up water coil
 	int CoilNum = 1;
 	WaterCoil( CoilNum ).Name = "Test Simple Water Heating Coil";
 	WaterCoil( CoilNum ).WaterLoopNum = 1;
 	WaterCoil( CoilNum ).WaterCoilType = CoilType_Heating;
 	WaterCoil( CoilNum ).WaterCoilType_Num = WaterCoil_SimpleHeating;  // Coil:Heating:Water
 	WaterCoil( CoilNum ).WaterCoilModel = CoilType_Heating;
 	WaterCoil( CoilNum ).RequestingAutoSize = true;
 	WaterCoil( CoilNum ).DesAirVolFlowRate = AutoSize;
 	WaterCoil( CoilNum ).DesInletAirTemp = AutoSize;
 	WaterCoil( CoilNum ).DesOutletAirTemp = AutoSize;
 	WaterCoil( CoilNum ).DesInletWaterTemp = AutoSize;
 	WaterCoil( CoilNum ).DesInletAirHumRat = AutoSize;
 	WaterCoil( CoilNum ).DesOutletAirHumRat = AutoSize;
 	WaterCoil( CoilNum ).MaxWaterVolFlowRate = AutoSize;

 	WaterCoil( CoilNum ).DesignWaterDeltaTemp = 11.0;
 	WaterCoil( CoilNum ).UseDesignWaterDeltaTemp = true;

 	WaterCoilNumericFields( CoilNum ).FieldNames( 2 ) = "Maximum Water Flow Rate";
 	WaterCoil( CoilNum ).WaterInletNodeNum = 1;
 	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = WaterCoil( CoilNum ).WaterInletNodeNum;

 	CurZoneEqNum = 0;
 	CurSysNum = 1;
 	CurOASysNum = 0;

 	PlantSizData( 1 ).ExitTemp = 60.0;
 	PlantSizData( 1 ).DeltaT = 10.0;

 	DataWaterLoopNum = 1;
 	NumOfGlycols = 1;

 	// run water coil sizing
 	createCoilSelectionReportObj();
	SizeWaterCoil( CoilNum );
 	EXPECT_DOUBLE_EQ( 1.0, WaterCoil( CoilNum ).DesAirVolFlowRate );

 	Real64 CpAirStd = 0.0;
 	Real64 DesMassFlow = 0.0;
 	Real64 DesCoilHeatingLoad = 0.0;

 	CpAirStd = PsyCpAirFnWTdb(0.0, 20.0);
 	DesMassFlow = FinalSysSizing( 1 ).DesMainVolFlow * StdRhoAir;
 	DesCoilHeatingLoad = CpAirStd * DesMassFlow * ( 40.0 - 5.0 );

 	// check heating coil design load
 	EXPECT_DOUBLE_EQ( DesCoilHeatingLoad, WaterCoil( CoilNum ).DesWaterHeatingCoilRate );

 	Real64 Cp = 0;
 	Real64 rho = 0;
 	Real64 DesWaterFlowRate = 0;

 	Cp = GetSpecificHeatGlycol( PlantLoop( 1 ).FluidName, DataGlobals::HWInitConvTemp, PlantLoop( 1 ).FluidIndex, "Unit Test" );
 	rho = GetDensityGlycol( PlantLoop( 1 ).FluidName, DataGlobals::CWInitConvTemp, PlantLoop( 1 ).FluidIndex, "Unit Test" );
 	DesWaterFlowRate = WaterCoil( CoilNum ).DesWaterHeatingCoilRate / ( 11.0 * Cp * rho );

 	// check heating coil design water flow rate
 	EXPECT_DOUBLE_EQ( DesWaterFlowRate, WaterCoil( CoilNum ).MaxWaterVolFlowRate );
  }
