#ifndef SizingAnalysisObjects_hh_INCLUDED
#define SizingAnalysisObjects_hh_INCLUDED

// C++ Headers
#include <string>
#include <vector>

// EnergyPlus Headers

#include <DataLoopNode.hh>
#include <WeatherManager.hh>
#include <OutputProcessor.hh>
#include <DataSizing.hh>
#include <DataHVACGlobals.hh>
#include <DataPlant.hh>
#include <OutputReportPredefined.hh>
#include <General.hh>

namespace EnergyPlus {


class SystemTimestepObject {
public: 

	Real64 CurMinuteStart		= 0.0; //minutes at beginning of system timestep
	Real64 CurMinuteEnd			= 0.0; //minutes at end of system timestep
	Real64 TimeStepDuration		= 0.0; //in fractional hours, length of timestep
	Real64 LogDataValue			= 0.0;
	int stStepsIntoZoneStep		= 0;
};


class ZoneTimestepObject {
public:

	int KindofSim			= 0;
	int EnvrnNum			= 0;
	int DesignDayNum		= 0;
	int DayOfSim			= 0;    // since start of simulation
	int HourOfDay			= 0; 
	int ztStepsIntoPeriod	= 0;    //count of zone timesteps into period
	Real64 stepStartMinute	= 0.0;  //minutes at beginning of zone timestep
	Real64 stepEndMinute	= 0.0;  //minutes at end of zone timestep
	Real64 TimeStepDuration = 0.0;  //in fractional hours, length of timestep

	Real64 LogDataValue		= 0.0;
	Real64 RunningAvgDataValue = 0.0;
	bool HasSystemSubSteps	= false;
	int NumSubSteps = 0;
	std::vector< SystemTimestepObject > subSteps; //nested object array for system timesteps inside here.
	
};

class SizingLog {
public:
	int NumOfEnvironmentsInLogSet;
	int NumOfDesignDaysInLogSet;
	int NumberOfSizingPeriodsInLogSet;
	std::vector <int> ztStepCountByEnvrn ; // array of how many zone timesteps are in each environment period, sized to number of environments in sizing set
	std::vector <int> EnvrnIndexMapByEnvrn ; //sized to number of environments in sizing set
	std::vector <int> EnvrnStartZtStepIndex ; //sized to number of environments in sizing set

	int NumOfStepsInLogSet; // sum of all zone timestep steps in log
	std::vector< ZoneTimestepObject > ztStepObj; //will be sized to the sum of all steps, eg. timesteps in hour * 24 hours * 2 design days.  


	int NodeNum; //temporary until pointers...

	int GetZtStepIndex(
		const ZoneTimestepObject tmpztStepStamp
	);

	void FillZoneStep(
		ZoneTimestepObject tmpztStepStamp
	);

	int GetSysStepZtStepIndex(
		ZoneTimestepObject tmpztStepStamp 
	);

	void FillSysStep( 
		ZoneTimestepObject tmpztStepStamp ,
		SystemTimestepObject tmpSysStepStamp
	 );

	void AverageSysTimeSteps();

	void ProcessRunningAverage( 
		int const TimeStepsInAverage
	);

	ZoneTimestepObject GetLogVariableDataMax( );

	void AdjustEnvrnIndexMapForIteration(
		int const HVACSizingIterCount
	);

	void ReInitLogForIteration();


};


class SizingLoggerFramework {
public:

	int NumOfLogs;
	std::vector <SizingLog> logObjs;


	int const SetupVariableSizingLog(
		int const SupplySideInletNodeNum  //change to pointers for generality later
	);

	ZoneTimestepObject PrepareZoneTimestepStamp ();

	void UpdateSizingLogValuesZoneStep();

	void UpdateSizingLogValuesSystemStep();

	void IncrementSizingPeriodSet(
		int const HVACSizingIterCount
	);

};


class  PlantCoinicidentAnalyis {
public:

	//this object collects data and methods for analyzing coincident sizing for a single plant loop

	// name of analysis object
	std::string name = "";
	int PlantLoopIndex = 0; // index in plant loop data structure.
	int PlantSizingIndex = 0;
	int SupplySideInletNodeNum = 0;
	int NumTimeStepsInAvg = 0;
	Real64 DensityForSizing = 0.0;
	Real64 PlantSizingFraction = 0.0;
	Real64 previousVolDesignFlowRate = 0.0;
	Real64 newVolDesignFlowRate = 0.0;
	Real64 newAdjustedMassFlowRate = 0.0; // with sizing factor included...
	Real64 newFoundMassFlowRate = 0.0;
	ZoneTimestepObject newFoundMassFlowRateTimeStamp;
	const Real64 SignificantNormalizedChange = 0.01 ;
	bool AnotherIterationDesired = false ;

	int LogIndex; //the index in the vector of log objects in the logger framework.

	void Initialize();

	void ResolveDesignFlowRate(
		int const HVACSizingIterCount
	);
	
};

}

#endif
