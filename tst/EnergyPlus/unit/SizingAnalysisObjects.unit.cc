// EnergyPlus::DataPlant Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <EnergyPlus/SizingAnalysisObjects.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WeatherManager.hh>

using namespace EnergyPlus;
using namespace WeatherManager;
using namespace OutputProcessor;
using namespace DataGlobals;
using namespace DataPlant;
using namespace DataSizing;
using namespace OutputReportPredefined;

class SizingAnalysisObjectsTest : public :: testing::Test
{

public:

	Real64 lowLogVal;
	Real64 midLogVal;
	Real64 hiLogVal;
	Real64 LogVal;  // actual variable pointed to
	int averagingWindow;
	int logIndex;

	SizingLoggerFramework sizingLoggerFrameObj;

	// constructor for test fixture class
	SizingAnalysisObjectsTest()
	{
		// fill in test log data values
		lowLogVal = 50.0;
		midLogVal = 75.0;
		hiLogVal  = 100.0;

		NumOfTimeStepInHour = 4;  // in DataGlobals
		TimeStepZone = 0.25;

		// setup weather manager state needed
		NumOfEnvrn = 2;
		Environment.allocate( NumOfEnvrn );
		Environment(1).KindOfEnvrn = ksDesignDay;
		Environment(1).DesignDayNum = 1;

		Environment(2).KindOfEnvrn = ksDesignDay;
		Environment(2).DesignDayNum = 2;

		averagingWindow = 1;
		logIndex = sizingLoggerFrameObj.SetupVariableSizingLog(LogVal, averagingWindow );

		NumOfEnvrn = 4;
		Environment.redimension( NumOfEnvrn );

		Environment(3).KindOfEnvrn = ksHVACSizeDesignDay;
		Environment(3).DesignDayNum = 1;
		Environment(3).SeedEnvrnNum = 1;

		Environment(4).KindOfEnvrn = ksHVACSizeDesignDay;
		Environment(4).DesignDayNum = 2;
		Environment(4).SeedEnvrnNum = 2;

		TimeValue.allocate( 2 );
		TimeValue( 1 ).TimeStep >>= TimeStepZone;

		PlantSizData.allocate( 1 );

		PlantSizData( 1 ).SizingFactorOption = NoSizingFactorMode;
		PlantSizData( 1 ).DesVolFlowRate = 0.002;
		PlantSizData( 1 ).DeltaT = 10;

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

		SetPredefinedTables();

		int write_stat;
		// Open the Initialization Output File (lifted from SimulationManager.cc)
		OutputFileInits = GetNewUnitNumber();
		{ IOFlags flags; flags.ACTION( "write" ); flags.STATUS( "UNKNOWN" ); gio::open( OutputFileInits, "eplusout.eio", flags ); write_stat = flags.ios(); }
	}

	//destructor
	~SizingAnalysisObjectsTest()
	{
		TotNumLoops = 0;
		PlantLoop( 1 ).LoopSide.deallocate();
		PlantLoop.deallocate();
		Environment.deallocate();
		PlantSizData.deallocate();
		TimeValue.deallocate();

		// Close and delete eio output file
		{ IOFlags flags; flags.DISPOSE( "DELETE" ); gio::close( OutputFileInits, flags ); }
	}

};

TEST_F( SizingAnalysisObjectsTest, testZoneUpdateInLoggerFramework )
{
	ShowMessage( "Begin Test: SizingAnalysisObjectsTest, testZoneUpdateInLoggerFramework" );

	int const ZoneIndex ( 1 );

	// first step
	KindOfSim = 4;
	DayOfSim  = 1;
	HourOfDay = 1;
	Envrn = 3;
	Environment( Envrn ).DesignDayNum = 1;
	sizingLoggerFrameObj.SetupSizingLogsNewEnvironment();
	TimeValue( ZoneIndex ).CurMinute = 15;

	LogVal = lowLogVal;
	sizingLoggerFrameObj.UpdateSizingLogValuesZoneStep();

	EXPECT_DOUBLE_EQ( lowLogVal, sizingLoggerFrameObj.logObjs[ logIndex ].ztStepObj[ 0 ].logDataValue );

	//last step of first design day
	HourOfDay = 24;
	TimeValue( ZoneIndex ).CurMinute = 60;
	LogVal = hiLogVal;
	sizingLoggerFrameObj.UpdateSizingLogValuesZoneStep();

	EXPECT_DOUBLE_EQ( hiLogVal, sizingLoggerFrameObj.logObjs[ logIndex ].ztStepObj[ 95 ].logDataValue );

	//first step of second design day
	HourOfDay = 1;
	TimeValue( ZoneIndex ).CurMinute = 15;
	Envrn = 4;
	Environment( Envrn ).DesignDayNum = 2;
	sizingLoggerFrameObj.SetupSizingLogsNewEnvironment();
	LogVal = midLogVal;
	sizingLoggerFrameObj.UpdateSizingLogValuesZoneStep();

	EXPECT_DOUBLE_EQ( midLogVal, sizingLoggerFrameObj.logObjs[ logIndex ].ztStepObj[ 96 ].logDataValue );
}

TEST_F( SizingAnalysisObjectsTest, BasicLogging4stepsPerHour )
{
	ShowMessage( "Begin Test: SizingAnalysisObjectsTest, BasicLogging4stepsPerHour" );

// basic test of method FillZoneStep and zone time stamp constructor
// setup a log for 4 timesteps per hour and fill the first 4 steps, then check that values are there
	SizingLog TestLogObj( LogVal );

	TestLogObj.NumOfEnvironmentsInLogSet = 2;
	TestLogObj.NumOfDesignDaysInLogSet = 2;
	TestLogObj.NumberOfSizingPeriodsInLogSet = 0;

	TestLogObj.NumOfStepsInLogSet = 8; // test

	TestLogObj.ztStepCountByEnvrnMap[ 1 ]= 4;
	TestLogObj.ztStepCountByEnvrnMap[ 2 ]= 4;

	// as for 2 DDs and a run period
	TestLogObj.envrnStartZtStepIndexMap[ 1 ] = 0;
	TestLogObj.envrnStartZtStepIndexMap[ 2 ] = 4;

	TestLogObj.newEnvrnToSeedEnvrnMap[ 3 ] = 1;
	TestLogObj.newEnvrnToSeedEnvrnMap[ 4 ] = 2;

	TestLogObj.ztStepObj.resize( TestLogObj.NumOfStepsInLogSet );

// fill first step in log with zone step data
	int KindOfSim( 4 );
	int Envrn( 3 );
	int DayOfSim( 1 );
	int HourofDay( 1 );
	int CurMin( 15 );
	Real64 timeStepDuration( 0.25 );
	int numTimeStepsInHour ( 4 );
	LogVal = lowLogVal;
	ZoneTimestepObject tmpztStepStamp1( // call constructor
		KindOfSim,
		Envrn,
		DayOfSim,
		HourofDay,
		CurMin,
		timeStepDuration,
		numTimeStepsInHour
	);
	TestLogObj.FillZoneStep( tmpztStepStamp1 );

// fill second step log with zone step data

	CurMin = 30;
	LogVal = midLogVal;
	ZoneTimestepObject tmpztStepStamp2( // call constructor
		KindOfSim,
		Envrn,
		DayOfSim,
		HourofDay,
		CurMin,
		timeStepDuration,
		numTimeStepsInHour
	);
	TestLogObj.FillZoneStep( tmpztStepStamp2 );

// fill third step log with zone step data
	CurMin = 45 ;
	LogVal = midLogVal;
	ZoneTimestepObject tmpztStepStamp3( // call constructor
		KindOfSim,
		Envrn,
		DayOfSim,
		HourofDay,
		CurMin,
		timeStepDuration,
		numTimeStepsInHour
	);
	TestLogObj.FillZoneStep( tmpztStepStamp3 );

// fill fourth step log with zone step data
	CurMin = 60;
	LogVal = hiLogVal;
	ZoneTimestepObject tmpztStepStamp4( // call constructor
		KindOfSim,
		Envrn,
		DayOfSim,
		HourofDay,
		CurMin,
		timeStepDuration,
		numTimeStepsInHour
	);
	TestLogObj.FillZoneStep( tmpztStepStamp4 );

	// now check that the correct values were stored in the right spot
	EXPECT_DOUBLE_EQ(lowLogVal, TestLogObj.ztStepObj[ 0 ].logDataValue);
	EXPECT_DOUBLE_EQ(midLogVal, TestLogObj.ztStepObj[ 2 ].logDataValue);
	EXPECT_NE( lowLogVal, TestLogObj.ztStepObj[ 1 ].logDataValue );

	//store this in the logger framework
	sizingLoggerFrameObj.logObjs.push_back( TestLogObj );
}

TEST_F( SizingAnalysisObjectsTest, LoggingDDWrap1stepPerHour )
{
	ShowMessage( "Begin Test: SizingAnalysisObjectsTest, LoggingDDWrap1stepPerHour" );

// this test uses one timestep per hour and checks as for two design days

	SizingLog TestLogObj( LogVal );

	TestLogObj.NumOfEnvironmentsInLogSet = 2;
	TestLogObj.NumOfDesignDaysInLogSet = 2;
	TestLogObj.NumberOfSizingPeriodsInLogSet = 0;

	TestLogObj.NumOfStepsInLogSet = 48; // test
	TestLogObj.ztStepCountByEnvrnMap[ 1 ]= 24;
	TestLogObj.ztStepCountByEnvrnMap[ 2 ]= 24;

	TestLogObj.envrnStartZtStepIndexMap[ 1 ] = 0;
	TestLogObj.envrnStartZtStepIndexMap[ 2 ] = 24;

	TestLogObj.newEnvrnToSeedEnvrnMap[ 3 ] = 1;
	TestLogObj.newEnvrnToSeedEnvrnMap[ 4 ] = 2;

	TestLogObj.ztStepObj.resize( TestLogObj.NumOfStepsInLogSet );

// fill first step in log with zone step data
	int KindOfSim( 4 );
	int Envrn( 3 );
	int DDnum( 1 );
	int DayOfSim( 1 );
	int HourofDay( 1 );
	int CurMin( 60 );
	Real64 timeStepDuration( 1.0 );
	int numTimeStepsInHour ( 1 );

	LogVal = lowLogVal;
	for ( int hr = 1; hr <= 24; ++hr ) {
		HourofDay = hr;
		ZoneTimestepObject tmpztStepStamp1( // call constructor
			KindOfSim,Envrn,DayOfSim,HourofDay,CurMin,timeStepDuration,
			numTimeStepsInHour
		);
		TestLogObj.FillZoneStep( tmpztStepStamp1 );
	}

	Envrn = 4;
	DDnum = 2;
	LogVal = hiLogVal;
	for ( int hr = 1; hr <= 24; ++hr ) {
		HourofDay = hr;
		ZoneTimestepObject tmpztStepStamp1( // call constructor
			KindOfSim,Envrn,DayOfSim,HourofDay,CurMin,timeStepDuration,
			numTimeStepsInHour
		);
		TestLogObj.FillZoneStep( tmpztStepStamp1 );
	}

	// check values at wrap of environment change over
	EXPECT_DOUBLE_EQ(lowLogVal, TestLogObj.ztStepObj[ 23].logDataValue);
	EXPECT_DOUBLE_EQ( hiLogVal, TestLogObj.ztStepObj[ 24].logDataValue );

		//store this in the logger framework
	sizingLoggerFrameObj.logObjs.push_back(TestLogObj );
}

TEST_F( SizingAnalysisObjectsTest, PlantCoincidentAnalyObjTest )
{
	ShowMessage( "Begin Test: SizingAnalysisObjectsTest, PlantCoincidentAnalyObjTest" );

	std::string loopName;
	int loopNum;
	int nodeNum;
	Real64 density;
	Real64 cp;
	int timestepsInAvg;
	int plantSizingIndex;

	loopName = "Test Plant Loop 1";
	loopNum = 1;
	nodeNum = 1;
	density = 1000;
	cp = 1.0;
	timestepsInAvg = 1;
	plantSizingIndex = 1;

	PlantCoinicidentAnalysis TestAnalysisObj(
		loopName,
		loopNum,
		nodeNum,
		density,
		cp,
		timestepsInAvg,
		plantSizingIndex
		);

	// fill first step in log with zone step data
	int KindOfSim( 4 );
	int Envrn( 4 );
	int DayOfSim( 1 );
	int HourofDay( 1 );
	int CurMin( 15 );
	Real64 timeStepDuration( 0.25 );
	int numTimeStepsInHour ( 4 );

	ZoneTimestepObject tmpztStepStamp1( // call constructor
		KindOfSim,
		Envrn,
		DayOfSim,
		HourofDay,
		CurMin,
		timeStepDuration,
		numTimeStepsInHour
	);
	LogVal = 1.5; // kg/s
	tmpztStepStamp1.runningAvgDataValue = 1.5;
	sizingLoggerFrameObj.logObjs[logIndex].FillZoneStep( tmpztStepStamp1 );

	TestAnalysisObj.newFoundMassFlowRateTimeStamp = tmpztStepStamp1;
	TestAnalysisObj.peakMdotCoincidentDemand = 1000.0;
	TestAnalysisObj.peakMdotCoincidentReturnTemp = 10.0;
	TestAnalysisObj.NewFoundMaxDemandTimeStamp = tmpztStepStamp1;
	TestAnalysisObj.peakDemandMassFlow = 1.5;
	TestAnalysisObj.peakDemandReturnTemp = 10.0;

	EXPECT_DOUBLE_EQ( 0.002, PlantLoop( 1 ).MaxVolFlowRate ); //  m3/s

	TestAnalysisObj.ResolveDesignFlowRate( 1 );

	EXPECT_DOUBLE_EQ( 0.0015, PlantLoop( 1 ).MaxVolFlowRate ); //  m3/s
	EXPECT_DOUBLE_EQ( 1.5, PlantLoop( 1 ).MaxMassFlowRate ); //  m3/s
	EXPECT_TRUE( TestAnalysisObj.anotherIterationDesired );
}
