// EnergyPlus::DataPlant Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/SizingAnalysisObjects.hh>
#include <EnergyPlus/DataGlobals.hh>

using namespace EnergyPlus;
using namespace WeatherManager;
using namespace OutputProcessor;
using namespace DataGlobals;
using DataGlobals::NumOfTimeStepInHour;
using DataGlobals::KindOfSim;
using DataGlobals::TimeStepZone;



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
	SizingAnalysisObjectsTest ( )
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

		Environment(4).KindOfEnvrn = ksHVACSizeDesignDay;
		Environment(4).DesignDayNum = 2;

		TimeValue.allocate( 2 );
		TimeValue( 1 ).TimeStep >>= TimeStepZone;


	}




};



TEST_F(SizingAnalysisObjectsTest, testZoneUpdateInLoggerFramework )
{

// first add a log to the framework
//	int averagingWindow;
	//int logIndex;
	int const ZoneIndex ( 1 );
	Real64 checkVal;


	//setup globals, weather and output processor data

	// first step
	KindOfSim = 4;
	DayOfSim  = 1;
	HourOfDay = 1;
	Envrn = 3;
	Environment( Envrn ).DesignDayNum = 1;
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
	LogVal = midLogVal;
	sizingLoggerFrameObj.UpdateSizingLogValuesZoneStep();

	EXPECT_DOUBLE_EQ( midLogVal, sizingLoggerFrameObj.logObjs[ logIndex ].ztStepObj[ 96 ].logDataValue );

}

TEST_F( SizingAnalysisObjectsTest, BasicLogging4stepsPerHour)
{
// basic test of method FillZoneStep and zone time stamp constructor
// setup a log for 4 timesteps per hour and fill the first 4 steps, then check that values are there
	SizingLog TestLogObj;

	TestLogObj.NumOfEnvironmentsInLogSet = 2;
	TestLogObj.NumOfDesignDaysInLogSet = 2;
	TestLogObj.NumberOfSizingPeriodsInLogSet = 0;
	TestLogObj.ztStepCountByEnvrn.resize( TestLogObj.NumOfEnvironmentsInLogSet );
	TestLogObj.EnvrnIndexMapByEnvrn.resize ( TestLogObj.NumOfEnvironmentsInLogSet );
	TestLogObj.EnvrnStartZtStepIndex.resize ( TestLogObj.NumOfEnvironmentsInLogSet );

	TestLogObj.NumOfStepsInLogSet = 8; // test

	TestLogObj.ztStepCountByEnvrn[ 0 ] = 4;
	TestLogObj.ztStepCountByEnvrn[ 1 ] = 4; 

	// as for 2 DDs and a run period
	TestLogObj.EnvrnIndexMapByEnvrn[ 0 ] = 4; 
	TestLogObj.EnvrnIndexMapByEnvrn[ 1 ] = 5;

	TestLogObj.EnvrnStartZtStepIndex[ 0 ] = 0;
	TestLogObj.EnvrnStartZtStepIndex[ 1 ] = 4;

	TestLogObj.ztStepObj.resize( TestLogObj.NumOfStepsInLogSet );

	TestLogObj.p_rVariable = & LogVal; 

// fill first step in log with zone step data
	int KindOfSim( 4 );
	int Envrn( 4 );
	int DDnum( 1 );
	int DayOfSim( 1 );
	int HourofDay( 1 );
	int CurMin( 15 );
	Real64 timeStepDuration( 0.25 );
	int numTimeStepsInHour ( 4 );
	LogVal = lowLogVal;
	ZoneTimestepObject tmpztStepStamp1( // call constructor
		KindOfSim,
		Envrn,
		DDnum,
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
		DDnum,
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
		DDnum,
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
		DDnum,
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
// this test uses one timestep per hour and checks as for two design days

	SizingLog TestLogObj;

	TestLogObj.NumOfEnvironmentsInLogSet = 2;
	TestLogObj.NumOfDesignDaysInLogSet = 2;
	TestLogObj.NumberOfSizingPeriodsInLogSet = 0;
	TestLogObj.ztStepCountByEnvrn.resize( TestLogObj.NumOfEnvironmentsInLogSet );
	TestLogObj.EnvrnIndexMapByEnvrn.resize ( TestLogObj.NumOfEnvironmentsInLogSet );
	TestLogObj.EnvrnStartZtStepIndex.resize ( TestLogObj.NumOfEnvironmentsInLogSet );

	TestLogObj.NumOfStepsInLogSet = 48; // test

	TestLogObj.ztStepCountByEnvrn[ 0 ] = 24;
	TestLogObj.ztStepCountByEnvrn[ 1 ] = 24; 

	TestLogObj.EnvrnIndexMapByEnvrn[ 0 ] = 4;
	TestLogObj.EnvrnIndexMapByEnvrn[ 1 ] = 5;

	TestLogObj.EnvrnStartZtStepIndex[ 0 ] = 0;
	TestLogObj.EnvrnStartZtStepIndex[ 1 ] = 24;

	TestLogObj.ztStepObj.resize( TestLogObj.NumOfStepsInLogSet );

	TestLogObj.p_rVariable = & LogVal; 

// fill first step in log with zone step data
	int KindOfSim( 4 );
	int Envrn( 4 );
	int DDnum( 1 );
	int DayOfSim( 1 );
	int HourofDay( 1 );
	int CurMin( 60 );
	Real64 timeStepDuration( 1.0 );
	int numTimeStepsInHour ( 1 );

	LogVal = lowLogVal;
	for (int hr = 1; hr <= 24; hr++ ) {
		HourofDay = hr;
		ZoneTimestepObject tmpztStepStamp1( // call constructor
			KindOfSim,Envrn,DDnum,DayOfSim,HourofDay,CurMin,timeStepDuration,
			numTimeStepsInHour
		); 
		TestLogObj.FillZoneStep( tmpztStepStamp1 );
	}

	Envrn = 5;
	DDnum = 2;
	LogVal = hiLogVal;
	for (int hr = 1; hr <= 24; hr++ ) {
		HourofDay = hr;
		ZoneTimestepObject tmpztStepStamp1( // call constructor
			KindOfSim,Envrn,DDnum,DayOfSim,HourofDay,CurMin,timeStepDuration,
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

