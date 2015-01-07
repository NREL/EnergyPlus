#ifndef SizingAnalysisObjects_hh_INCLUDED
#define SizingAnalysisObjects_hh_INCLUDED

// C++ Headers
#include <string>
#include <vector>

// EnergyPlus Headers

#include <DataLoopNode.hh>
#include <WeatherManager.hh>
#include <OutputProcessor.hh>


namespace EnergyPlus {


class systemTimestepObject {
public: 

	Real64 CurMinuteStart = 0.0;  //minutes at beginning of system timestep
	Real64 CurMinuteEnd = 0.0;    //minutes at end of system timestep
	Real64 TimeStepDuration = 0.0; //in fractional hours, length of timestep

	Real64 LogDataValue;
};


class zoneTimestepObject {
public:

	int KindofSim = 0;  
	int EnvrnNum = 0;
	int DesignDayNum = 0;
	int DayOfSim =0; // since start of simulation
	int HourOfDay = 0; 
	Real64 stepStartMinute = 0.0;  //minutes at beginning of zone timestep
	Real64 stepEndMinute = 0.0;    //minutes at end of zone timestep
	Real64 TimeStepDuration = 0.0; //in fractional hours, length of timestep

	Real64 LogDataValue;
	int NumSubSteps;
	std::vector< systemTimestepObject > subSteps; //nested object array for system timesteps inside here.
	
};

class SizingLog {
public:
	int NumOfEnvironmentsInLogSet;
	std::vector <int> ztStepCountByEnvrn ; // array of how many zone timesteps are in each environment period, sized to number of environments in sizing set
	std::vector <int> EnvrnStartIndex ; //sized to number of environments in sizing set
	std::vector <int> EnvrnEndIndex ; //sized to number of environments in sizing set

	std::vector< zoneTimestepObject > ztStepObj; //will be sized to the sum of all steps, eg. timesteps in hour * 24 hours * 2 design days.  


	int NodeNum; //temporary until pointers... 
	int LastVectorIndexUsed;

	void fillZoneStep(zoneTimestepObject tmpztStepStamp );

	void fillSysStep( zoneTimestepObject tmpztStepStamp );
};


class SizingLoggerFramework {
public:

	int NumOfLogs;
	std::vector <SizingLog> logObjs;


	int const AddSizingLog(
		int const & SupplySideInletNodeNum  //change to pointers for generality later
	);

	void UpdateSizingLogValuesZoneStep();

};


class  PlantCoinicidentAnalyis {
public:

	// name of analysis object
	std::string name = "";
	int PlantLoop = 0;
	int SupplySideInletNodeNum = 0;

	Real64 previousDesignFlowRate = 0.0;

	int LogIndex;

	void initialize();
	
};

}

#endif
