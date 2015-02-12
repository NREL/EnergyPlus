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

	int kindofSim			= 0;
	int envrnNum			= 0;
	int designDayNum		= 0;
	int dayOfSim			= 0;    // since start of simulation
	int hourOfDay			= 0; 
	int ztStepsIntoPeriod	= 0;    //count of zone timesteps into period
	Real64 stepStartMinute	= 0.0;  //minutes at beginning of zone timestep
	Real64 stepEndMinute	= 0.0;  //minutes at end of zone timestep
	Real64 timeStepDuration = 0.0;  //in fractional hours, length of timestep

	Real64 logDataValue		= 0.0;
	Real64 runningAvgDataValue = 0.0;
	bool hasSystemSubSteps	= false;
	int numSubSteps = 0;
	std::vector< SystemTimestepObject > subSteps; //nested object array for system timesteps inside here.
	
	ZoneTimestepObject (
		int kindOfSim,
		int environmentNum,
		int designDayNum,
		int dayOfSim,
		int hourOfDay,
		int stepEndMinute,
		Real64 timeStepDuration,
		int numOfTimeStepsPerHour
	);

	ZoneTimestepObject();
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
	int timeStepsInAverage;
	Real64 *p_rVariable;

	std::vector< ZoneTimestepObject > ztStepObj; //will be sized to the sum of all steps, eg. timesteps in hour * 24 hours * 2 design days.  



	void FillZoneStep(
		ZoneTimestepObject tmpztStepStamp
	);

	void FillSysStep( 
		ZoneTimestepObject tmpztStepStamp ,
		SystemTimestepObject tmpSysStepStamp
	 );

	void AverageSysTimeSteps();

	void ProcessRunningAverage();

	ZoneTimestepObject GetLogVariableDataMax( );

	Real64 GetLogVariableDataAtTimestamp( 
		ZoneTimestepObject tmpztStepStamp
		);

	void AdjustEnvrnIndexMapForIteration(
		int const HVACSizingIterCount
	);

	void ReInitLogForIteration();

private:

	int GetSysStepZtStepIndex(
		const ZoneTimestepObject tmpztStepStamp 
	);
	int GetZtStepIndex(
		const ZoneTimestepObject tmpztStepStamp
	);

};

class SizingLoggerFramework {
public:
	std::vector <SizingLog> logObjs;
	int const SetupVariableSizingLog(
		Real64 & rVariable,
		int stepsInAverage
	);

	ZoneTimestepObject PrepareZoneTimestepStamp ();

	void UpdateSizingLogValuesZoneStep();

	void UpdateSizingLogValuesSystemStep();

	void IncrementSizingPeriodSet(
		int const HVACSizingIterCount
	);
private:
	int NumOfLogs;


};


class  PlantCoinicidentAnalysis {
public:

	//this object collects data and methods for analyzing coincident sizing for a single plant loop
	int PlantLoopIndex = 0; // index in plant loop data structure.
	int SupplySideInletNodeNum = 0; // node index for supply side inlet node
	int PlantSizingIndex = 0;
	int NumTimeStepsInAvg = 0;
	ZoneTimestepObject newFoundMassFlowRateTimeStamp;
	Real64 PeakMdotCoincidentReturnTemp;
	Real64 PeakMdotCoincidentDemand;

	bool AnotherIterationDesired = false ;

	int supplyInletNodeFlow_LogIndex; // loop flow rate index for vector of log objects in the logger framework
	int supplyInletNodeTemp_LogIndex; // loop return temperature index for vector of log objects in the logger framework

	// variables related to loop demand
	int loopDemand_LogIndex; // Loop demand load index for vector of log objects in the logger framework
	bool PeakDemandAndFlowMismatch;
	ZoneTimestepObject newFoundMaxDemandTimeStamp;

	Real64 PeakDemandReturnTemp;
	Real64 PeakDemandMassFlow;


	PlantCoinicidentAnalysis (
		std::string plantLoopName,
		int loopIndex,
		int NodeNum,
		Real64 density,
			Real64 cp,
		int numTimeStepsInAvg,
		int sizingIndex
	);

	void ResolveDesignFlowRate(
		int const HVACSizingIterCount
	);

private:
	std::string name = ""; // name of analysis object


	Real64 newAdjustedMassFlowRate = 0.0; // with sizing factor included...
	Real64 newFoundMassFlowRate = 0.0;

	const Real64 SignificantNormalizedChange = 0.005 ;
	Real64 DensityForSizing = 0.0;
	Real64 SpecificHeatForSizing = 0.0;
	Real64 PlantSizingFraction = 0.0;
	Real64 previousVolDesignFlowRate = 0.0;
	Real64 newVolDesignFlowRate = 0.0;
	
};

}

#endif
