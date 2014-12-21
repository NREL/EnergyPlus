// C++ Headers
#include <string>
#include <vector>

// EnergyPlus Headers
//#include <DataPrecisionGlobals.hh>
#include <SizingAnalysisObjects.hh>
#include <DataLoopNode.hh>
#include <WeatherManager.hh>
#include <OutputProcessor.hh>

namespace EnergyPlus {


	void PlantCoinicidentAnalyis::SetupPlantLogs() {
		//Real64 mdot;

		using DataLoopNode::Node;

		//eventually will use pointers, for now passing node num for hard coded to node mass flow rate
		LogIndex = SizingLogger.AddSizingLog(SupplySideInletNodeNum);

		//mdot = Node( SupplySideInletNodeNum ).MassFlowRate;
	


	}

	void SizingLog::fillZoneStep(
		zoneTimestepObject tmpztStepStamp 
	){

		using DataLoopNode::Node;
		int index;
		zoneTimestepObject lastztStepObj;

		//find array index
		if (LastVectorIndexUsed == 0) {
			index = 0;
		} else {
			//compare current stepStamp to last stepStamp

			index = LastVectorIndexUsed + 1;

		}
		ztStepObj[index] = tmpztStepStamp;


		//hardcode until pointers
		ztStepObj[index].LogDataValue = Node(NodeNum).MassFlowRate;

	
	}

	void SizingLog::fillSysStep(
		zoneTimestepObject tmpztStepStamp 
	){
	
	//TODO

	
	}

	int const SizingLoggerFramework::AddSizingLog(
		int const & SupplySideInletNodeNum
	){
		int VectorLength;
		using DataGlobals::NumOfTimeStepInHour;

		SizingLog tmpLog;

		tmpLog.NumOfEnvironmentsInLogSet =2; //temporary hard code for proof of concept

		tmpLog.NodeNum = SupplySideInletNodeNum; //temporary until pointers... 

		VectorLength = tmpLog.NumOfEnvironmentsInLogSet * 24 * NumOfTimeStepInHour; //temporary hard code for proof of concept
		tmpLog.ztStepCountByEnvrn.resize(tmpLog.NumOfEnvironmentsInLogSet);
		tmpLog.EnvrnEndIndex.resize(tmpLog.NumOfEnvironmentsInLogSet);
		tmpLog.EnvrnEndIndex.resize(tmpLog.NumOfEnvironmentsInLogSet);
		tmpLog.LastVectorIndexUsed = 0;
		tmpLog.ztStepObj.resize(VectorLength);

		logObjs.push_back(tmpLog);
		NumOfLogs++;
		return NumOfLogs;
		
	}

	void SizingLoggerFramework::UpdateSizingLogValuesZoneStep(){

		using DataGlobals::KindOfSim;
		using DataGlobals::DayOfSim;
		using DataGlobals::HourOfDay;
		using namespace WeatherManager;
		using namespace OutputProcessor;
		int const ZoneIndex (1);

		//prepare current timing data once and then pass into fill routines
		zoneTimestepObject tmpztStepStamp; 

		tmpztStepStamp.KindofSim       = KindOfSim;
		tmpztStepStamp.EnvrnNum        = Envrn;
		tmpztStepStamp.DayOfSim        = DayOfSim;
		tmpztStepStamp.HourOfDay       = HourOfDay;
		tmpztStepStamp.stepStartMinute = TimeValue( ZoneIndex ).CurMinute;
		tmpztStepStamp.stepEndMinute = tmpztStepStamp.stepStartMinute + TimeValue( ZoneIndex ).TimeStep * 60.0;
		tmpztStepStamp.TimeStepDuration = TimeValue( ZoneIndex ).TimeStep;

		tmpztStepStamp.NumSubSteps = 0;


		for (auto &L : this->logObjs) {
			L.fillZoneStep(tmpztStepStamp);
		}
	}
	

}



