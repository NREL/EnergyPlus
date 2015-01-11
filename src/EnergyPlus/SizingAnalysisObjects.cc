// C++ Headers
#include <string>
#include <vector>

// EnergyPlus Headers
#include <SizingAnalysisObjects.hh>
#include <DataLoopNode.hh>
#include <WeatherManager.hh>
#include <OutputProcessor.hh>
#include <DataSizing.hh>
#include <DataHVACGlobals.hh>

namespace EnergyPlus {

	int SizingLog::getZtStepIndex (
		const zoneTimestepObject tmpztStepStamp)
	{
	using DataGlobals::ksHVACSizeDesignDay;
	int VecIndex;
	int logEnvrnIndex( 0 );
	int logCounter( 0 );
	//find which sizing envrn index

	if (tmpztStepStamp.KindofSim == ksHVACSizeDesignDay) {
		for (int i = 0; i <= NumOfDesignDaysInLogSet; i++ ){
			if ( tmpztStepStamp.EnvrnNum == EnvrnIndexMapByEnvrn[ logCounter ] ){
				logEnvrnIndex = logCounter;
				logCounter++;
				break;
			}
		}
	}

	VecIndex = EnvrnStartZtStepIndex[ logEnvrnIndex ] + tmpztStepStamp.ztStepsIntoPeriod;
	return VecIndex;
	}

	void SizingLog::fillZoneStep(
		zoneTimestepObject tmpztStepStamp 
	){

		using DataLoopNode::Node;
		int index(0);
		
		index =  this -> getZtStepIndex( tmpztStepStamp ); 

		ztStepObj[index] = tmpztStepStamp;


		//hardcode until pointers
		ztStepObj[index].LogDataValue = Node( NodeNum ).MassFlowRate;

	
	}

	void SizingLog::fillSysStep(
		zoneTimestepObject tmpztStepStamp 
	){
	
	//TODO

	
	}

	zoneTimestepObject SizingLog::GetLogVariableDataMax( 

	){
	Real64 MaxVal;
	zoneTimestepObject tmpztStepStamp;
	MaxVal = 0.0;
		for ( auto &Zt : this -> ztStepObj ){
			
			if ( Zt.LogDataValue > MaxVal) {
				MaxVal = Zt.LogDataValue;
				tmpztStepStamp = Zt;
				}
		}
	return tmpztStepStamp;
	}

	int const SizingLoggerFramework::SetupVariableSizingLog(
		int const & SupplySideInletNodeNum  // change to pointer setup 
	){
		using DataGlobals::NumOfTimeStepInHour;
		using DataGlobals::ksDesignDay;
		using DataGlobals::ksRunPeriodDesign;
		using namespace WeatherManager;
		int VectorLength( 0 );
		int mapIndexCounter( 0 );
		int const HoursPerDay( 24 );
		SizingLog tmpLog;
		tmpLog.NumOfEnvironmentsInLogSet = 0;
		tmpLog.NumOfDesignDaysInLogSet   = 0;
		tmpLog.NumberOfSizingPeriodsInLogSet = 0;
		// search environments for sizing , this is coded to occur before the additions to Environment structure that will occur to run them
		for (int i = 1; i <= NumOfEnvrn; i++ ){
			if ( Environment( i ).KindOfEnvrn == ksDesignDay ) {
				tmpLog.NumOfEnvironmentsInLogSet++;
				tmpLog.NumOfDesignDaysInLogSet++;

			}
			if (Environment( i ).KindOfEnvrn == ksRunPeriodDesign ) {
				tmpLog.NumOfEnvironmentsInLogSet++;
				tmpLog.NumberOfSizingPeriodsInLogSet++;
			}
		}
		if (tmpLog.NumOfDesignDaysInLogSet > 0 && tmpLog.NumberOfSizingPeriodsInLogSet == 0 ) {
		// all design days, no sizing periods. first do the usual case TODO handle sizing periods

			VectorLength = tmpLog.NumOfEnvironmentsInLogSet * HoursPerDay * NumOfTimeStepInHour; 
			tmpLog.ztStepCountByEnvrn.resize(    tmpLog.NumOfEnvironmentsInLogSet );
			tmpLog.EnvrnStartZtStepIndex.resize( tmpLog.NumOfEnvironmentsInLogSet );
			tmpLog.EnvrnIndexMapByEnvrn.resize(  tmpLog.NumOfEnvironmentsInLogSet );
		//
			for (int i = 0; i<tmpLog.NumOfEnvironmentsInLogSet; i++) {

				tmpLog.ztStepCountByEnvrn[ i ] = HoursPerDay * NumOfTimeStepInHour;
				tmpLog.EnvrnStartZtStepIndex[ i ] = i * HoursPerDay * NumOfTimeStepInHour ;
			}
			mapIndexCounter = 0;
			for (int i = 1; i <= NumOfEnvrn; i++ ){
				if ( Environment( i ).KindOfEnvrn == ksDesignDay ) {
					tmpLog.EnvrnIndexMapByEnvrn[ mapIndexCounter ] = NumOfEnvrn + mapIndexCounter; //known offset for first iteration, what about subsequent iterations?
					mapIndexCounter++;

				}
			}
		}


		tmpLog.NodeNum = SupplySideInletNodeNum; //temporary until pointers... 

		tmpLog.ztStepObj.resize( VectorLength );

		logObjs.push_back( tmpLog );
		NumOfLogs++;
		return NumOfLogs - 1;
		
	}

	void SizingLoggerFramework::UpdateSizingLogValuesZoneStep(){

		using DataGlobals::KindOfSim;
		using DataGlobals::DayOfSim;
		using DataGlobals::HourOfDay;
		using DataGlobals::NumOfTimeStepInHour;
		using namespace WeatherManager;
		using namespace OutputProcessor;
		int const ZoneIndex ( 1 );
		Real64 const MinutesPerHour( 60.0 );
		int const HoursPerDay( 24 );


		//prepare current timing data once and then pass into fill routines
		zoneTimestepObject tmpztStepStamp; 

		tmpztStepStamp.KindofSim       = KindOfSim;
		tmpztStepStamp.EnvrnNum        = Envrn;
		tmpztStepStamp.DesignDayNum    = Environment( Envrn ).DesignDayNum;
		tmpztStepStamp.DayOfSim        = DayOfSim;
		tmpztStepStamp.HourOfDay       = HourOfDay;
		tmpztStepStamp.stepEndMinute = TimeValue( ZoneIndex ).CurMinute;
		if ( tmpztStepStamp.stepEndMinute == 0.0 ) {tmpztStepStamp.stepEndMinute = MinutesPerHour ; }
		tmpztStepStamp.stepStartMinute = tmpztStepStamp.stepEndMinute - TimeValue( ZoneIndex ).TimeStep * MinutesPerHour;
		tmpztStepStamp.TimeStepDuration = TimeValue( ZoneIndex ).TimeStep;

		tmpztStepStamp.ztStepsIntoPeriod = ((tmpztStepStamp.DayOfSim - 1) * (HoursPerDay * NumOfTimeStepInHour) ) + //multiple days
			((tmpztStepStamp.HourOfDay-1) * NumOfTimeStepInHour ) + //so far this day's hours
			round( (tmpztStepStamp.stepStartMinute / MinutesPerHour)/(tmpztStepStamp.TimeStepDuration) ); // into current hour



		tmpztStepStamp.NumSubSteps = 0;


		for ( auto &L : this -> logObjs ) {


			L.fillZoneStep(tmpztStepStamp);
		}
	}
	
	void PlantCoinicidentAnalyis::DetermineMaxFlowRate(){
	
		using DataSizing::PlantSizData;
		using DataHVACGlobals::SmallWaterVolFlow;

		previousVolDesignFlowRate = PlantSizData( PlantLoop ).DesVolFlowRate;
		newFoundMassFlowRate = newFoundMassFlowRateTimeStamp.LogDataValue;
		newVolDesignFlowRate = newFoundMassFlowRate / DensityForSizing;
		
		if (newVolDesignFlowRate > SmallWaterVolFlow && newVolDesignFlowRate < previousVolDesignFlowRate ) {
			PlantSizData( PlantLoop ).DesVolFlowRate = newVolDesignFlowRate;
		}
	
	}
}
