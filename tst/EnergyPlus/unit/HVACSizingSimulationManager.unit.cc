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

// EnergyPlus::DataPlant Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/HVACSizingSimulationManager.hh>
#include <EnergyPlus/SizingAnalysisObjects.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace DataGlobals;
using namespace DataPlant;
using namespace DataSizing;
using namespace OutputReportPredefined;
using namespace WeatherManager;
using namespace DataLoopNode;
using namespace OutputProcessor;
using namespace DataHVACGlobals;

class HVACSizingSimulationManagerTest : public EnergyPlusFixture
{
protected:
	virtual void SetUp() {
		EnergyPlusFixture::SetUp();  // Sets up the base fixture first.

		// setup weather manager state needed
		NumOfEnvrn = 2;
		Environment.allocate( NumOfEnvrn );
		Environment(1).KindOfEnvrn = ksDesignDay;
		Environment(1).DesignDayNum = 1;

		Environment(2).KindOfEnvrn = ksDesignDay;
		Environment(2).DesignDayNum = 2;

		// setup plant sizing data structure
		NumPltSizInput = 1;
		PlantSizData.allocate( NumPltSizInput );
		PlantSizData( NumPltSizInput ).SizingFactorOption = NoSizingFactorMode;
		PlantSizData( NumPltSizInput ).DesVolFlowRate = 0.002;
		PlantSizData( NumPltSizInput ).DeltaT = 10;
		PlantSizData( NumPltSizInput ).ConcurrenceOption = Coincident;
		PlantSizData( NumPltSizInput ).NumTimeStepsInAvg = 1;
		PlantSizData( NumPltSizInput ).PlantLoopName = "Test Plant Loop 1";
		PlantSizData( NumPltSizInput ).LoopType = HeatingLoop;

		//set up a plant loop
		TotNumLoops = 1;
		PlantLoop.allocate( TotNumLoops );
		for ( int l = 1; l <= TotNumLoops; ++l ) {
			auto & loop( PlantLoop( l ) );
			loop.LoopSide.allocate( 2 );
		}
		PlantLoop( 1 ).Name = "Test Plant Loop 1";
		PlantLoop( 1 ).MaxVolFlowRateWasAutoSized = true;
		PlantLoop( 1 ).MaxVolFlowRate = 0.002;
		PlantLoop( 1 ).MaxMassFlowRate = 2.0;
		PlantLoop( 1 ).VolumeWasAutoSized = true;
		PlantLoop( 1 ).FluidName = "WATER";
		PlantLoop( 1 ).FluidIndex = 1;
		PlantLoop( 1 ).LoopSide( SupplySide ).NodeNumIn = 1;

		//set up plant loop Reporting

		PlantReport.allocate( TotNumLoops );

		SetPredefinedTables();

		// need a node to log mass flow rate from
		Node.allocate( 1 );
		TimeValue.allocate( 2 );
		// set up time related
		SetupTimePointers( "Zone", TimeStepZone ); // Set up Time pointer for HB/Zone Simulation
		SetupTimePointers( "HVAC", TimeStepSys );

		NumOfTimeStepInHour = 4;
		TimeStepFraction = 1.0 / double( NumOfTimeStepInHour );

		TimeValue( 1 ).TimeStep >>= TimeStepZone;
		TimeValue( 1 ).CurMinute = 0; // init
		TimeValue( 2 ).TimeStep >>= TimeStepSys;
		TimeValue( 2 ).CurMinute = 0;
	}

	virtual void TearDown() {
		EnergyPlusFixture::TearDown();  // Remember to tear down the base fixture after cleaning up derived fixture!
	}
};

TEST_F( HVACSizingSimulationManagerTest, WeatherFileDaysTest3 )
{

// this test emulates two design days and two sizing weather file days periods
// calls code related to coincident plant sizing with HVAC sizing simulation
// this test runs 3 system timesteps for each zone timestep

	Environment.deallocate();
			// setup weather manager state needed
	NumOfEnvrn = 4;
	Environment.allocate( NumOfEnvrn );
	Environment( 1 ).KindOfEnvrn = ksDesignDay;
	Environment( 1 ).DesignDayNum = 1;

	Environment( 2 ).KindOfEnvrn = ksDesignDay;
	Environment( 2 ).DesignDayNum = 2;

	Environment( 3 ).KindOfEnvrn = ksRunPeriodDesign;
	Environment( 3 ).DesignDayNum = 0;
	Environment( 3 ).TotalDays = 4;

	Environment( 4 ).KindOfEnvrn = ksRunPeriodDesign;
	Environment( 4 ).DesignDayNum = 0;
	Environment( 4 ).TotalDays = 4;


	HVACSizingSimulationManager testSizeSimManagerObj;

	testSizeSimManagerObj.DetermineSizingAnalysesNeeded ();

	EXPECT_EQ( 1, testSizeSimManagerObj.plantCoincAnalyObjs[ 0 ].supplySideInletNodeNum );

	testSizeSimManagerObj.SetupSizingAnalyses();

	EXPECT_EQ( 4, NumOfEnvrn );
	AddDesignSetToEnvironmentStruct( 1 );
	EXPECT_EQ( 8, NumOfEnvrn );

	//now fill with three system timesteps for each zone timestep
	TimeStepZone = 15.0/60.0;
	NumOfSysTimeSteps = 3;
	TimeStepSys = TimeStepZone / NumOfSysTimeSteps;


	//first HVAC Sizing Simulation DD emulation
	KindOfSim = ksHVACSizeDesignDay;
	DayOfSim  = 1;
	Envrn = 5;
	Environment( Envrn ).DesignDayNum = 1;
	testSizeSimManagerObj.sizingLogger.SetupSizingLogsNewEnvironment();

	for (HourOfDay = 1; HourOfDay <= 24; ++HourOfDay) { // Begin hour loop ...
		TimeValue( 1 ).CurMinute = 0.0;
		TimeValue( 2 ).CurMinute = 0.0;
		for (TimeStep = 1; TimeStep <= NumOfTimeStepInHour; ++TimeStep) {
			for ( int SysTimestepLoop = 1; SysTimestepLoop <= NumOfSysTimeSteps; ++SysTimestepLoop ) {
				TimeValue( 2 ).CurMinute += TimeValue( 2 ).TimeStep * 60.0;

				Node(1).MassFlowRate = HourOfDay * 0.1;
				Node(1).Temp = 10.0;
				PlantReport(1).HeatingDemand = HourOfDay * 10.0;
				testSizeSimManagerObj.sizingLogger.UpdateSizingLogValuesSystemStep();
			}
			TimeValue( 1 ).CurMinute += TimeValue( 1 ).TimeStep * 60.0;
			testSizeSimManagerObj.sizingLogger.UpdateSizingLogValuesZoneStep();
		} // TimeStep loop
	} // ... End hour loop.

	//second HVAC Sizing Simulation DD emulation
	KindOfSim = ksHVACSizeDesignDay;
	DayOfSim  = 1;
	Envrn = 6;

	Environment( Envrn ).DesignDayNum = 2;
	testSizeSimManagerObj.sizingLogger.SetupSizingLogsNewEnvironment();
	for (HourOfDay = 1; HourOfDay <= 24; ++HourOfDay) { // Begin hour loop ...
		TimeValue( 1 ).CurMinute = 0.0;
		TimeValue( 2 ).CurMinute = 0.0;
		for (TimeStep = 1; TimeStep <= NumOfTimeStepInHour; ++TimeStep) {
			for ( int SysTimestepLoop = 1; SysTimestepLoop <= NumOfSysTimeSteps; ++SysTimestepLoop ) {
				TimeValue( 2 ).CurMinute += TimeValue( 2 ).TimeStep * 60.0;

				Node(1).MassFlowRate = HourOfDay * 0.1;
				Node(1).Temp = 10.0;
				PlantReport(1).HeatingDemand = HourOfDay * 10.0;

				testSizeSimManagerObj.sizingLogger.UpdateSizingLogValuesSystemStep();
			}
			TimeValue( 1 ).CurMinute += TimeValue( 1 ).TimeStep * 60.0;
			testSizeSimManagerObj.sizingLogger.UpdateSizingLogValuesZoneStep();
		} // TimeStep loop
	} // End hour loop.


	//first HVAC Sizing Simulation WEatherFileDAys emulation
	KindOfSim = ksHVACSizeRunPeriodDesign;
	DayOfSim  = 0;
	Envrn = 7;
	NumOfDayInEnvrn = 4;
	testSizeSimManagerObj.sizingLogger.SetupSizingLogsNewEnvironment();
	while  ( DayOfSim < NumOfDayInEnvrn ) {
		++DayOfSim;
		for (HourOfDay = 1; HourOfDay <= 24; ++HourOfDay) { // Begin hour loop ...
			TimeValue( 1 ).CurMinute = 0.0;
			TimeValue( 2 ).CurMinute = 0.0;
			for (TimeStep = 1; TimeStep <= NumOfTimeStepInHour; ++TimeStep) {
				for ( int SysTimestepLoop = 1; SysTimestepLoop <= NumOfSysTimeSteps; ++SysTimestepLoop ) {
					TimeValue( 2 ).CurMinute += TimeValue( 2 ).TimeStep * 60.0;

					Node(1).MassFlowRate = HourOfDay * 0.1;
					Node(1).Temp = 10.0;
					PlantReport(1).HeatingDemand = HourOfDay * 10.0;
					testSizeSimManagerObj.sizingLogger.UpdateSizingLogValuesSystemStep();
				}
				TimeValue( 1 ).CurMinute += TimeValue( 1 ).TimeStep * 60.0;
				testSizeSimManagerObj.sizingLogger.UpdateSizingLogValuesZoneStep();
			} // TimeStep loop
		} // ... End hour loop.
	} // day loop

	//second HVAC Sizing Simulation WEatherFileDAys emulation
	KindOfSim = ksHVACSizeRunPeriodDesign;
	DayOfSim  = 0;
	Envrn = 8;
	NumOfDayInEnvrn = 4;
	testSizeSimManagerObj.sizingLogger.SetupSizingLogsNewEnvironment();
	while  ( DayOfSim < NumOfDayInEnvrn ) {
		++DayOfSim;
		for (HourOfDay = 1; HourOfDay <= 24; ++HourOfDay) { // Begin hour loop ...
			TimeValue( 1 ).CurMinute = 0.0;
			TimeValue( 2 ).CurMinute = 0.0;
			for (TimeStep = 1; TimeStep <= NumOfTimeStepInHour; ++TimeStep) {
				for ( int SysTimestepLoop = 1; SysTimestepLoop <= NumOfSysTimeSteps; ++SysTimestepLoop ) {
					TimeValue( 2 ).CurMinute += TimeValue( 2 ).TimeStep * 60.0;

					Node(1).MassFlowRate = HourOfDay * 0.1;
					Node(1).Temp = 10.0;
					PlantReport(1).HeatingDemand = HourOfDay * 10.0;
					testSizeSimManagerObj.sizingLogger.UpdateSizingLogValuesSystemStep();
				}
				TimeValue( 1 ).CurMinute += TimeValue( 1 ).TimeStep * 60.0;
				testSizeSimManagerObj.sizingLogger.UpdateSizingLogValuesZoneStep();
			} // TimeStep loop
		} // ... End hour loop.
	} // day loop


	testSizeSimManagerObj.PostProcessLogs();

	// check plant resizing
	EXPECT_DOUBLE_EQ( 2.0 , PlantLoop( 1 ).MaxMassFlowRate ); // original size
	testSizeSimManagerObj.ProcessCoincidentPlantSizeAdjustments( 1 );
	EXPECT_DOUBLE_EQ( 2.4 , PlantLoop( 1 ).MaxMassFlowRate ); //resize check

	// check that the data are as expected in the logs
	// first timestep
	EXPECT_DOUBLE_EQ( 0.1 , testSizeSimManagerObj.sizingLogger
			.logObjs[ testSizeSimManagerObj.plantCoincAnalyObjs[ 0 ].supplyInletNodeFlow_LogIndex ]
				.ztStepObj[ 0 ].subSteps[ 0 ].LogDataValue );

	EXPECT_DOUBLE_EQ( 0.1 , testSizeSimManagerObj.sizingLogger
			.logObjs[ testSizeSimManagerObj.plantCoincAnalyObjs[ 0 ].supplyInletNodeFlow_LogIndex ]
				.ztStepObj[ 0 ].runningAvgDataValue );

	// last timestep of first hour
	EXPECT_DOUBLE_EQ( 0.1 , testSizeSimManagerObj.sizingLogger
			.logObjs[ testSizeSimManagerObj.plantCoincAnalyObjs[ 0 ].supplyInletNodeFlow_LogIndex ]
				.ztStepObj[ 3 ].subSteps[ 2 ].LogDataValue );

	EXPECT_DOUBLE_EQ( 0.1 , testSizeSimManagerObj.sizingLogger
			.logObjs[ testSizeSimManagerObj.plantCoincAnalyObjs[ 0 ].supplyInletNodeFlow_LogIndex ]
				.ztStepObj[ 3 ].runningAvgDataValue );

	// first timestep of second hour
	EXPECT_DOUBLE_EQ( 0.2 , testSizeSimManagerObj.sizingLogger
			.logObjs[ testSizeSimManagerObj.plantCoincAnalyObjs[ 0 ].supplyInletNodeFlow_LogIndex ]
				.ztStepObj[ 7 ].subSteps[ 0 ].LogDataValue );

	EXPECT_DOUBLE_EQ( 0.2 , testSizeSimManagerObj.sizingLogger
			.logObjs[ testSizeSimManagerObj.plantCoincAnalyObjs[ 0 ].supplyInletNodeFlow_LogIndex ]
				.ztStepObj[ 7 ].runningAvgDataValue );

	// last timestep of first DD, hour = 24
	EXPECT_DOUBLE_EQ( 2.4 , testSizeSimManagerObj.sizingLogger
			.logObjs[ testSizeSimManagerObj.plantCoincAnalyObjs[ 0 ].supplyInletNodeFlow_LogIndex ]
				.ztStepObj[ 95 ].subSteps[ 2 ].LogDataValue );

	EXPECT_DOUBLE_EQ( 2.4 , testSizeSimManagerObj.sizingLogger
			.logObjs[ testSizeSimManagerObj.plantCoincAnalyObjs[ 0 ].supplyInletNodeFlow_LogIndex ]
				.ztStepObj[ 95 ].runningAvgDataValue );

	// first timestep of second DD, hour = 1
	EXPECT_DOUBLE_EQ( 0.1 , testSizeSimManagerObj.sizingLogger
			.logObjs[ testSizeSimManagerObj.plantCoincAnalyObjs[ 0 ].supplyInletNodeFlow_LogIndex ]
				.ztStepObj[ 96 ].subSteps[ 0 ].LogDataValue );

	EXPECT_DOUBLE_EQ( 0.1 , testSizeSimManagerObj.sizingLogger
			.logObjs[ testSizeSimManagerObj.plantCoincAnalyObjs[ 0 ].supplyInletNodeFlow_LogIndex ]
				.ztStepObj[ 96 ].runningAvgDataValue );

	// first timestep of third sizing environment WeatherFileDays
	EXPECT_DOUBLE_EQ( 0.1 , testSizeSimManagerObj.sizingLogger
			.logObjs[ testSizeSimManagerObj.plantCoincAnalyObjs[ 0 ].supplyInletNodeFlow_LogIndex ]
				.ztStepObj[ 192 ].runningAvgDataValue);

	// first timestep of fourth sizing environment WeatherFileDays
	EXPECT_DOUBLE_EQ( 0.1 , testSizeSimManagerObj.sizingLogger
			.logObjs[ testSizeSimManagerObj.plantCoincAnalyObjs[ 0 ].supplyInletNodeFlow_LogIndex ]
				.ztStepObj[ 576 ].runningAvgDataValue);

}

TEST_F( HVACSizingSimulationManagerTest, TopDownTestSysTimestep3 )
{
// this test emulates two design days and calls nearly all the OO code related
// to coincident plant sizing with HVAC sizing simulation
// this test runs 3 system timesteps for each zone timestep

	HVACSizingSimulationManager testSizeSimManagerObj;

	testSizeSimManagerObj.DetermineSizingAnalysesNeeded ();

	EXPECT_EQ( 1, testSizeSimManagerObj.plantCoincAnalyObjs[ 0 ].supplySideInletNodeNum );

	testSizeSimManagerObj.SetupSizingAnalyses();

	EXPECT_EQ( 2, NumOfEnvrn );
	AddDesignSetToEnvironmentStruct( 1 );

	EXPECT_EQ( 4, NumOfEnvrn );

	//now fill with three system timesteps for each zone timestep
	TimeStepZone = 15.0/60.0;
	NumOfSysTimeSteps = 3;
	TimeStepSys = TimeStepZone / NumOfSysTimeSteps;


	//first HVAC Sizing Simulation DD emulation
	KindOfSim = 4;
	DayOfSim  = 1;
	Envrn = 3;
	Environment( Envrn ).DesignDayNum = 1;
	testSizeSimManagerObj.sizingLogger.SetupSizingLogsNewEnvironment();
	for ( HourOfDay = 1; HourOfDay <= 24; ++HourOfDay ) { // Begin hour loop ...
		TimeValue( 1 ).CurMinute = 0.0;
		TimeValue( 2 ).CurMinute = 0.0;
		for (TimeStep = 1; TimeStep <= NumOfTimeStepInHour; ++TimeStep) {
			for ( int SysTimestepLoop = 1; SysTimestepLoop <= NumOfSysTimeSteps; ++SysTimestepLoop ) {
				TimeValue( 2 ).CurMinute += TimeValue( 2 ).TimeStep * 60.0;

				Node(1).MassFlowRate = HourOfDay * 0.1;
				Node(1).Temp = 10.0;
				PlantReport(1).HeatingDemand = HourOfDay * 10.0;
				testSizeSimManagerObj.sizingLogger.UpdateSizingLogValuesSystemStep();
			}
			TimeValue( 1 ).CurMinute += TimeValue( 1 ).TimeStep * 60.0;
			testSizeSimManagerObj.sizingLogger.UpdateSizingLogValuesZoneStep();
		} // TimeStep loop
	} // ... End hour loop.

	//second HVAC Sizing Simulation DD emulation
	KindOfSim = 4;
	DayOfSim  = 1;
	Envrn = 4;
	Environment( Envrn ).DesignDayNum = 2;
	testSizeSimManagerObj.sizingLogger.SetupSizingLogsNewEnvironment();
	for ( HourOfDay = 1; HourOfDay <= 24; ++HourOfDay ) { // Begin hour loop ...
		TimeValue( 1 ).CurMinute = 0.0;
		TimeValue( 2 ).CurMinute = 0.0;
		for (TimeStep = 1; TimeStep <= NumOfTimeStepInHour; ++TimeStep) {
			for ( int SysTimestepLoop = 1; SysTimestepLoop <= NumOfSysTimeSteps; ++SysTimestepLoop ) {
				TimeValue( 2 ).CurMinute += TimeValue( 2 ).TimeStep * 60.0;

				Node(1).MassFlowRate = HourOfDay * 0.1;
				Node(1).Temp = 10.0;
				PlantReport(1).HeatingDemand = HourOfDay * 10.0;

				testSizeSimManagerObj.sizingLogger.UpdateSizingLogValuesSystemStep();
			}
			TimeValue( 1 ).CurMinute += TimeValue( 1 ).TimeStep * 60.0;
			testSizeSimManagerObj.sizingLogger.UpdateSizingLogValuesZoneStep();
		} // TimeStep loop
	} // End hour loop.


	testSizeSimManagerObj.PostProcessLogs();

	// check plant resizing
	EXPECT_DOUBLE_EQ( 2.0 , PlantLoop( 1 ).MaxMassFlowRate ); // original size
	testSizeSimManagerObj.ProcessCoincidentPlantSizeAdjustments( 1 );
	EXPECT_DOUBLE_EQ( 2.4 , PlantLoop( 1 ).MaxMassFlowRate ); //resize check

	// check that the data are as expected in the logs
	// first timestep
	EXPECT_DOUBLE_EQ( 0.1 , testSizeSimManagerObj.sizingLogger
			.logObjs[ testSizeSimManagerObj.plantCoincAnalyObjs[ 0 ].supplyInletNodeFlow_LogIndex ]
				.ztStepObj[ 0 ].subSteps[ 0 ].LogDataValue );

	EXPECT_DOUBLE_EQ( 0.1 , testSizeSimManagerObj.sizingLogger
			.logObjs[ testSizeSimManagerObj.plantCoincAnalyObjs[ 0 ].supplyInletNodeFlow_LogIndex ]
				.ztStepObj[ 0 ].runningAvgDataValue );

	// last timestep of first hour
	EXPECT_DOUBLE_EQ( 0.1 , testSizeSimManagerObj.sizingLogger
			.logObjs[ testSizeSimManagerObj.plantCoincAnalyObjs[ 0 ].supplyInletNodeFlow_LogIndex ]
				.ztStepObj[ 3 ].subSteps[ 2 ].LogDataValue );

	EXPECT_DOUBLE_EQ( 0.1 , testSizeSimManagerObj.sizingLogger
			.logObjs[ testSizeSimManagerObj.plantCoincAnalyObjs[ 0 ].supplyInletNodeFlow_LogIndex ]
				.ztStepObj[ 3 ].runningAvgDataValue );

	// first timestep of second hour
	EXPECT_DOUBLE_EQ( 0.2 , testSizeSimManagerObj.sizingLogger
			.logObjs[ testSizeSimManagerObj.plantCoincAnalyObjs[ 0 ].supplyInletNodeFlow_LogIndex ]
				.ztStepObj[ 7 ].subSteps[ 0 ].LogDataValue );

	EXPECT_DOUBLE_EQ( 0.2 , testSizeSimManagerObj.sizingLogger
			.logObjs[ testSizeSimManagerObj.plantCoincAnalyObjs[ 0 ].supplyInletNodeFlow_LogIndex ]
				.ztStepObj[ 7 ].runningAvgDataValue );

	// last timestep of first DD, hour = 24
	EXPECT_DOUBLE_EQ( 2.4 , testSizeSimManagerObj.sizingLogger
			.logObjs[ testSizeSimManagerObj.plantCoincAnalyObjs[ 0 ].supplyInletNodeFlow_LogIndex ]
				.ztStepObj[ 95 ].subSteps[ 2 ].LogDataValue );

	EXPECT_DOUBLE_EQ( 2.4 , testSizeSimManagerObj.sizingLogger
			.logObjs[ testSizeSimManagerObj.plantCoincAnalyObjs[ 0 ].supplyInletNodeFlow_LogIndex ]
				.ztStepObj[ 95 ].runningAvgDataValue );

	// first timestep of second DD, hour = 1
	EXPECT_DOUBLE_EQ( 0.1 , testSizeSimManagerObj.sizingLogger
			.logObjs[ testSizeSimManagerObj.plantCoincAnalyObjs[ 0 ].supplyInletNodeFlow_LogIndex ]
				.ztStepObj[ 96 ].subSteps[ 0 ].LogDataValue );

	EXPECT_DOUBLE_EQ( 0.1 , testSizeSimManagerObj.sizingLogger
			.logObjs[ testSizeSimManagerObj.plantCoincAnalyObjs[ 0 ].supplyInletNodeFlow_LogIndex ]
				.ztStepObj[ 96 ].runningAvgDataValue );

}

TEST_F( HVACSizingSimulationManagerTest, TopDownTestSysTimestep1 )
{
// this test emulates two design days and calls nearly all the OO code related
// to coincident plant sizing with HVAC sizing simulation
// this test runs 1 system timestep for each zone timestep

	GlobalCoolSizingFactor = 1.0;
	PlantSizData( NumPltSizInput ).SizingFactorOption = GlobalCoolingSizingFactorMode;

	HVACSizingSimulationManager testSizeSimManagerObj;

	testSizeSimManagerObj.DetermineSizingAnalysesNeeded ();

	EXPECT_EQ( 1, testSizeSimManagerObj.plantCoincAnalyObjs[ 0 ].supplySideInletNodeNum );

	testSizeSimManagerObj.SetupSizingAnalyses();

	EXPECT_EQ( 2, NumOfEnvrn );
	AddDesignSetToEnvironmentStruct( 1 );
	EXPECT_EQ( 4, NumOfEnvrn );

	//now fill with one system timesteps for each zone timestep
	TimeStepZone = 15.0/60.0;
	NumOfSysTimeSteps = 1;
	TimeStepSys = TimeStepZone / NumOfSysTimeSteps;

	//first HVAC Sizing Simulation DD emulation
	KindOfSim = 4;
	DayOfSim  = 1;
	Envrn = 3;
	Environment( Envrn ).DesignDayNum = 1;
	testSizeSimManagerObj.sizingLogger.SetupSizingLogsNewEnvironment();
	for ( HourOfDay = 1; HourOfDay <= 24; ++HourOfDay ) { // Begin hour loop ...
		TimeValue( 1 ).CurMinute = 0.0;
		TimeValue( 2 ).CurMinute = 0.0;
		for (TimeStep = 1; TimeStep <= NumOfTimeStepInHour; ++TimeStep) {

			for ( int SysTimestepLoop = 1; SysTimestepLoop <= NumOfSysTimeSteps; ++SysTimestepLoop ) {
				TimeValue( 2 ).CurMinute += TimeValue( 2 ).TimeStep * 60.0;

				Node(1).MassFlowRate = HourOfDay * 0.1;
				Node(1).Temp = 10.0;
				PlantReport(1).HeatingDemand = HourOfDay * 10.0;
				testSizeSimManagerObj.sizingLogger.UpdateSizingLogValuesSystemStep();
			}
			// E+ doesn't really update zone step data until system steps are done
			TimeValue( 1 ).CurMinute += TimeValue( 1 ).TimeStep * 60.0;
			testSizeSimManagerObj.sizingLogger.UpdateSizingLogValuesZoneStep();
		} // TimeStep loop
	} // ... End hour loop.

	//second HVAC Sizing Simulation DD emulation
	KindOfSim = 4;
	DayOfSim  = 1;
	Envrn = 4;
	Environment( Envrn ).DesignDayNum = 2;
	testSizeSimManagerObj.sizingLogger.SetupSizingLogsNewEnvironment();
	for ( HourOfDay = 1; HourOfDay <= 24; ++HourOfDay ) { // Begin hour loop ...
		TimeValue( 1 ).CurMinute = 0.0;
		TimeValue( 2 ).CurMinute = 0.0;
		for ( TimeStep = 1; TimeStep <= NumOfTimeStepInHour; ++TimeStep ) {

			for ( int SysTimestepLoop = 1; SysTimestepLoop <= NumOfSysTimeSteps; ++SysTimestepLoop ) {
				TimeValue( 2 ).CurMinute += TimeValue( 2 ).TimeStep * 60.0;

				Node(1).MassFlowRate = HourOfDay * 0.1;
				Node(1).Temp = 10.0;
				PlantReport(1).HeatingDemand = HourOfDay * 10.0;

				testSizeSimManagerObj.sizingLogger.UpdateSizingLogValuesSystemStep();
			}
			TimeValue( 1 ).CurMinute += TimeValue( 1 ).TimeStep * 60.0;
			testSizeSimManagerObj.sizingLogger.UpdateSizingLogValuesZoneStep();
		} // TimeStep loop
	} // End hour loop.


	testSizeSimManagerObj.PostProcessLogs();

	EXPECT_DOUBLE_EQ( 2.0 , PlantLoop( 1 ).MaxMassFlowRate ); // original size
	testSizeSimManagerObj.ProcessCoincidentPlantSizeAdjustments( 1 );
	EXPECT_DOUBLE_EQ( 2.4 , PlantLoop( 1 ).MaxMassFlowRate ); //resize check
}

TEST_F( HVACSizingSimulationManagerTest, VarySysTimesteps )
{
// this test emulates two design days and calls nearly all the OO code related
// to coincident plant sizing with HVAC sizing simulation
// this test run varies the system timestep some to test irregular

	PlantSizData( NumPltSizInput ).NumTimeStepsInAvg = 2;
	GlobalHeatSizingFactor = 1.0;
	PlantSizData( NumPltSizInput ).SizingFactorOption = GlobalHeatingSizingFactorMode;

	HVACSizingSimulationManager testSizeSimManagerObj;

	testSizeSimManagerObj.DetermineSizingAnalysesNeeded ();

	EXPECT_EQ( 1, testSizeSimManagerObj.plantCoincAnalyObjs[ 0 ].supplySideInletNodeNum );

	testSizeSimManagerObj.SetupSizingAnalyses();

	EXPECT_EQ( 2, NumOfEnvrn );
	AddDesignSetToEnvironmentStruct( 1 );
	EXPECT_EQ( 4, NumOfEnvrn );

	//now fill with one system timesteps for each zone timestep
	TimeStepZone = 15.0/60.0;
	NumOfSysTimeSteps = 1;
	TimeStepSys = TimeStepZone / NumOfSysTimeSteps;

	//first HVAC Sizing Simulation DD emulation
	KindOfSim = 4;
	DayOfSim  = 1;
	Envrn = 3;
	Environment( Envrn ).DesignDayNum = 1;
	testSizeSimManagerObj.sizingLogger.SetupSizingLogsNewEnvironment();
	for (HourOfDay = 1; HourOfDay <= 24; ++HourOfDay) { // Begin hour loop ...
		TimeValue( 1 ).CurMinute = 0.0;
		TimeValue( 2 ).CurMinute = 0.0;
		for (TimeStep = 1; TimeStep <= NumOfTimeStepInHour; ++TimeStep) {

			NumOfSysTimeSteps = TimeStep;
			TimeStepSys = TimeStepZone / NumOfSysTimeSteps;

			for ( int SysTimestepLoop = 1; SysTimestepLoop <= NumOfSysTimeSteps; ++SysTimestepLoop ) {
				TimeValue( 2 ).CurMinute += TimeValue( 2 ).TimeStep * 60.0;

				Node(1).MassFlowRate = HourOfDay * 0.1;
				Node(1).Temp = 10.0;
				PlantReport(1).HeatingDemand = HourOfDay * 10.0;
				testSizeSimManagerObj.sizingLogger.UpdateSizingLogValuesSystemStep();
			}
			// E+ doesn't really update zone step data until system steps are done
			TimeValue( 1 ).CurMinute += TimeValue( 1 ).TimeStep * 60.0;
			testSizeSimManagerObj.sizingLogger.UpdateSizingLogValuesZoneStep();
		} // TimeStep loop
	} // ... End hour loop.

	//second HVAC Sizing Simulation DD emulation
	KindOfSim = 4;
	DayOfSim  = 1;
	Envrn = 4;
	Environment( Envrn ).DesignDayNum = 2;
	testSizeSimManagerObj.sizingLogger.SetupSizingLogsNewEnvironment();
	for (HourOfDay = 1; HourOfDay <= 24; ++HourOfDay) { // Begin hour loop ...
		TimeValue( 1 ).CurMinute = 0.0;
		TimeValue( 2 ).CurMinute = 0.0;
		for (TimeStep = 1; TimeStep <= NumOfTimeStepInHour; ++TimeStep) {
			NumOfSysTimeSteps = TimeStep;
			TimeStepSys = TimeStepZone / NumOfSysTimeSteps;

			for ( int SysTimestepLoop = 1; SysTimestepLoop <= NumOfSysTimeSteps; ++SysTimestepLoop ) {
				TimeValue( 2 ).CurMinute += TimeValue( 2 ).TimeStep * 60.0;

				Node(1).MassFlowRate = HourOfDay * 0.1;
				Node(1).Temp = 10.0;
				PlantReport(1).HeatingDemand = HourOfDay * 10.0;

				testSizeSimManagerObj.sizingLogger.UpdateSizingLogValuesSystemStep();
			}
			TimeValue( 1 ).CurMinute += TimeValue( 1 ).TimeStep * 60.0;
			testSizeSimManagerObj.sizingLogger.UpdateSizingLogValuesZoneStep();
		} // TimeStep loop
	} // End hour loop.

	testSizeSimManagerObj.PostProcessLogs();

	EXPECT_DOUBLE_EQ( 2.0 , PlantLoop( 1 ).MaxMassFlowRate ); // original size
	testSizeSimManagerObj.ProcessCoincidentPlantSizeAdjustments( 1 );
	EXPECT_DOUBLE_EQ( 2.4 , PlantLoop( 1 ).MaxMassFlowRate ); //resize check

	testSizeSimManagerObj.ProcessCoincidentPlantSizeAdjustments( 1 );

	testSizeSimManagerObj.sizingLogger.IncrementSizingPeriodSet();
}
