#ifndef SizingAnalysisObjects_hh_INCLUDED
#define SizingAnalysisObjects_hh_INCLUDED

// C++ Headers
#include <string>
#include <vector>
#include <map>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {


class SystemTimestepObject
{
public:
	Real64 CurMinuteStart = 0.0; //minutes at beginning of system timestep
	Real64 CurMinuteEnd = 0.0; //minutes at end of system timestep
	Real64 TimeStepDuration = 0.0; //in fractional hours, length of timestep
	Real64 LogDataValue = 0.0; // raw hard value, obtained from pointer
	int stStepsIntoZoneStep = 0;
};


class ZoneTimestepObject
{
public:

	int kindOfSim = 0;
	int envrnNum = 0;
	int dayOfSim = 0;    // since start of simulation
	int hourOfDay = 0;
	int ztStepsIntoPeriod = 0;    //count of zone timesteps into period
	Real64 stepStartMinute = 0.0;  //minutes at beginning of zone timestep
	Real64 stepEndMinute = 0.0;  //minutes at end of zone timestep
	Real64 timeStepDuration = 0.0;  //in fractional hours, length of timestep
	Real64 logDataValue = 0.0;
	Real64 runningAvgDataValue = 0.0;
	bool hasSystemSubSteps = false;
	int numSubSteps = 0;
	std::vector< SystemTimestepObject > subSteps; //nested object array for system timesteps inside here.

	ZoneTimestepObject (
		int kindSim,
		int environmentNum,
		int daySim,
		int hourDay,
		int stepEndMin,
		Real64 timeStepDurat,
		int numOfTimeStepsPerHour
	);

	ZoneTimestepObject();
};

class SizingLog
{
public:
	SizingLog( double & rVariable );

	int NumOfEnvironmentsInLogSet;
	int NumOfDesignDaysInLogSet;
	int NumberOfSizingPeriodsInLogSet;
	std::map < int, int > ztStepCountByEnvrnMap; // key is the seed original Environment number, or index in the Environment Structure
	std::map < int, int > envrnStartZtStepIndexMap; // key is the seed original Environment number, produces index in zone step vector where that period begins
	std::map < int, int > newEnvrnToSeedEnvrnMap; // key is the new HVAC sim envrionment number, produces Seed environment number
	int NumOfStepsInLogSet; // sum of all zone timestep steps in log
	int timeStepsInAverage; // breadth back in time for running average, zone timesteps
	Real64 &p_rVariable;    // reference to variable being loggged

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

	ZoneTimestepObject GetLogVariableDataMax();

	Real64 GetLogVariableDataAtTimestamp(
		ZoneTimestepObject tmpztStepStamp
	);

	void ReInitLogForIteration();

	void SetupNewEnvironment(
		int const seedEnvrnNum,
		int const newEnvrnNum
	);

private:

	int GetSysStepZtStepIndex(
		const ZoneTimestepObject tmpztStepStamp
	);
	int GetZtStepIndex(
		const ZoneTimestepObject tmpztStepStamp
	);

};

class SizingLoggerFramework
{
public:
	std::vector <SizingLog> logObjs;
	int SetupVariableSizingLog(
		Real64 & rVariable,
		int stepsInAverage
	);

	ZoneTimestepObject PrepareZoneTimestepStamp ();

	void UpdateSizingLogValuesZoneStep();

	void UpdateSizingLogValuesSystemStep();

	void SetupSizingLogsNewEnvironment();

	void IncrementSizingPeriodSet();
private:
	int NumOfLogs = 0;

};


class  PlantCoinicidentAnalysis
{
public:

	//this object collects data and methods for analyzing coincident sizing for a single plant loop
	int plantLoopIndex = 0; // index in plant loop data structure.
	int supplySideInletNodeNum = 0; // node index for supply side inlet node
	int plantSizingIndex = 0;
	int numTimeStepsInAvg = 0;
	ZoneTimestepObject newFoundMassFlowRateTimeStamp;  // result for max mass flow, as a timestamp object
	Real64 peakMdotCoincidentReturnTemp;
	Real64 peakMdotCoincidentDemand;
	bool anotherIterationDesired = false;
	int supplyInletNodeFlow_LogIndex; // loop flow rate index for vector of log objects in the logger framework
	int supplyInletNodeTemp_LogIndex; // loop return temperature index for vector of log objects in the logger framework
	// variables related to loop demand
	int loopDemand_LogIndex; // Loop demand load index for vector of log objects in the logger framework
	bool peakDemandAndFlowMismatch;
	ZoneTimestepObject NewFoundMaxDemandTimeStamp; // result for max loop demand, as a timestamp object

	Real64 peakDemandReturnTemp;
	Real64 peakDemandMassFlow;

	PlantCoinicidentAnalysis(
		std::string loopName,
		int loopIndex,
		int nodeNum,
		Real64 density,
		Real64 cp,
		int numStepsInAvg,
		int sizingIndex
	);

	void ResolveDesignFlowRate(
		int const HVACSizingIterCount
	);

private:
	std::string name = ""; // name of analysis object
	Real64 newAdjustedMassFlowRate = 0.0; // with sizing factor included...
	Real64 newFoundMassFlowRate = 0.0;
	Real64 significantNormalizedChange = 0.005; // criteria for if sizing algorithm yeild a change large enough worth making another pass.
	Real64 densityForSizing = 0.0;
	Real64 specificHeatForSizing = 0.0;
	Real64 previousVolDesignFlowRate = 0.0;
	Real64 newVolDesignFlowRate = 0.0;

	bool CheckTimeStampForNull(
		ZoneTimestepObject testStamp
	);
};

}

#endif
