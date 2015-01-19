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
#include <DataPlant.hh>
#include <OutputReportPredefined.hh>

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
		for (int i = 0; i < NumOfDesignDaysInLogSet; i++ ){
			if ( tmpztStepStamp.EnvrnNum == EnvrnIndexMapByEnvrn[ logCounter ] ){
				logEnvrnIndex = logCounter;

				break;
			}
			logCounter++;

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
			for (int i = 0; i < tmpLog.NumOfEnvironmentsInLogSet; i++) {

				tmpLog.ztStepCountByEnvrn[ i ] = HoursPerDay * NumOfTimeStepInHour;
				tmpLog.EnvrnStartZtStepIndex[ i ] = i * HoursPerDay * NumOfTimeStepInHour ;
			}
			mapIndexCounter = 0;
			for (int i = 1; i <= NumOfEnvrn; i++ ){
				if ( Environment( i ).KindOfEnvrn == ksDesignDay ) {
					tmpLog.EnvrnIndexMapByEnvrn[ mapIndexCounter ] = NumOfEnvrn + mapIndexCounter + 1; //known offset for first iteration, what about subsequent iterations?
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
	
	void PlantCoinicidentAnalyis::ResolveDesignFlowRate(
		int const HVACSizingIterCount
	){
	
		using DataGlobals::TimeStepZone;
		using DataGlobals::SecInHour;
		using DataSizing::PlantSizData;
		using namespace DataPlant;
		using namespace OutputReportPredefined;
		using WeatherManager::Environment;
		using DataHVACGlobals::SmallWaterVolFlow;
		bool SetNewSizes;
		Real64 NormalizedChange;


		//this is for a loop level sizing factor which is assumed to not need to change for resizing
		PlantSizingFraction = PlantLoop( PlantLoopIndex ).LoopSide( SupplySide ).Branch( 1 ).PumpSizFac;

		previousVolDesignFlowRate	= PlantSizData( PlantLoopIndex ).DesVolFlowRate;

		if (newFoundMassFlowRateTimeStamp.LogDataValue > 0.0 ) {
			newFoundMassFlowRate		= newFoundMassFlowRateTimeStamp.LogDataValue;	
		} else {
			newFoundMassFlowRate		= 0.0;
		}
		
		newAdjustedMassFlowRate		= newFoundMassFlowRate * PlantSizingFraction; //indlude sizing fraction multiplier, often 1.0
		newVolDesignFlowRate		= newAdjustedMassFlowRate / DensityForSizing;



		//compare threshold, store TODO 
		SetNewSizes = false;
		if (newVolDesignFlowRate > SmallWaterVolFlow && newVolDesignFlowRate < previousVolDesignFlowRate ) {
			SetNewSizes = true;
			NormalizedChange = std::abs((newVolDesignFlowRate - previousVolDesignFlowRate) 
									/ previousVolDesignFlowRate);
			if (NormalizedChange > SignificantNormalizedChange ) {
				AnotherIterationDesired = true;
			} else {
				AnotherIterationDesired = false;
			}


		}

		if ( SetNewSizes ) {
		// set new size values for rest of simulation
			PlantSizData( PlantLoopIndex ).DesVolFlowRate = newVolDesignFlowRate;

			if (PlantLoop( PlantLoopIndex ).MaxVolFlowRateWasAutoSized ) {
				PlantLoop( PlantLoopIndex ).MaxVolFlowRate = newVolDesignFlowRate;
				PlantLoop( PlantLoopIndex ).MaxMassFlowRate =  newAdjustedMassFlowRate;
			}
			if ( PlantLoop( PlantLoopIndex ).VolumeWasAutoSized ) {
				PlantLoop( PlantLoopIndex ).Volume = PlantLoop( PlantLoopIndex ).MaxVolFlowRate * TimeStepZone * SecInHour / 0.8;
				PlantLoop( PlantLoopIndex ).Mass = PlantLoop( PlantLoopIndex ).Volume* DensityForSizing;
			}


		}
				//report to sizing summary table called Plant Loop Coincident Design Fluid Flow Rates
		PreDefTableEntry( pdchPlantSizPass, PlantLoop( PlantLoopIndex ).Name, HVACSizingIterCount );
		PreDefTableEntry( pdchPlantSizPrevVdot, PlantLoop( PlantLoopIndex ).Name, previousVolDesignFlowRate , 4 );
		PreDefTableEntry( pdchPlantSizCalcVdot, PlantLoop( PlantLoopIndex ).Name, newVolDesignFlowRate , 4 );

		if (SetNewSizes) {
			PreDefTableEntry( pdchPlantSizCoincYesNo, PlantLoop( PlantLoopIndex ).Name, "Yes" );
		} else {
			PreDefTableEntry( pdchPlantSizCoincYesNo, PlantLoop( PlantLoopIndex ).Name, "No" );
		}

		PreDefTableEntry( pdchPlantSizDesDay, PlantLoop( PlantLoopIndex ).Name, Environment(newFoundMassFlowRateTimeStamp.EnvrnNum).Title );
		PreDefTableEntry( pdchPlantSizPkTimeDayOfSim, PlantLoop( PlantLoopIndex ).Name, newFoundMassFlowRateTimeStamp.DayOfSim );
		PreDefTableEntry( pdchPlantSizPkTimeHour, PlantLoop( PlantLoopIndex ).Name, newFoundMassFlowRateTimeStamp.HourOfDay );
		PreDefTableEntry( pdchPlantSizPkTimeMin, PlantLoop( PlantLoopIndex ).Name, newFoundMassFlowRateTimeStamp.stepStartMinute );
	
	}
}
