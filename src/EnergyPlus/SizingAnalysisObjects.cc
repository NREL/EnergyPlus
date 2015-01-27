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
#include <General.hh>

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

	void SizingLog::ProcessRunningAverage (
		int const TimeStepsInAverage
	){

		Real64 RunningSum = 0.0;
		Real64 divisor = 0.0;
		divisor = double( TimeStepsInAverage );

		for (int k = 0; k < NumOfEnvironmentsInLogSet; k++) { // outer loop over environments in log set

			for (int i = 0; i < ztStepCountByEnvrn[ k ]; i++ ){ // next inner loop over zone timestep steps 

				if ( TimeStepsInAverage > 0 ) {
					RunningSum = 0.0;
					for ( int j = 0; j < TimeStepsInAverage ; j++ ) { //
						if ( (i - j) < 0) {
							RunningSum += ztStepObj[ EnvrnStartZtStepIndex[ k ] ].LogDataValue; //just use first value to fill early steps
						} else {
							RunningSum += ztStepObj[ ((i - j) + EnvrnStartZtStepIndex[ k ]) ].LogDataValue;
						}
					}
					ztStepObj[ (i + EnvrnStartZtStepIndex[ k ]) ].RunningAvgDataValue = RunningSum / divisor;
				}
			}
		}

	}

	zoneTimestepObject SizingLog::GetLogVariableDataMax( 

	){
		Real64 MaxVal;
		zoneTimestepObject tmpztStepStamp;
		MaxVal = 0.0;

		for ( auto &Zt : ztStepObj ){
			
			if ( Zt.RunningAvgDataValue > MaxVal) {
				MaxVal = Zt.RunningAvgDataValue;
				tmpztStepStamp = Zt;
				}
		}
	return tmpztStepStamp;
	}

	void SizingLog::AdjustEnvrnIndexMapForIteration(
		int const HVACSizingIterCount
	){
		for ( int i = 0; i < NumOfEnvironmentsInLogSet; i++ ) {
			EnvrnIndexMapByEnvrn[ i ] = EnvrnIndexMapByEnvrn[ i ] + NumOfEnvironmentsInLogSet;
		}
	}

	void SizingLog::ReInitLogForIteration(){
		zoneTimestepObject tmpNullztStepObj;

		for ( auto &Zt : this -> ztStepObj ){
			Zt = tmpNullztStepObj;
		}
	}

	int const SizingLoggerFramework::SetupVariableSizingLog(
		int const SupplySideInletNodeNum  // change to pointer setup 
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
		VectorLength = tmpLog.NumOfEnvironmentsInLogSet * HoursPerDay * NumOfTimeStepInHour;
		tmpLog.NumOfStepsInLogSet = VectorLength;
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

	void SizingLoggerFramework::IncrementSizingPeriodSet( 
		int const HVACSizingIterCount 
		){
	


		for ( auto &L : this -> logObjs ) {
			L.AdjustEnvrnIndexMapForIteration( HVACSizingIterCount );
			L.ReInitLogForIteration();
		}
	}
	
	void PlantCoinicidentAnalyis::ResolveDesignFlowRate(
		int const HVACSizingIterCount
	){
	
		using DataGlobals::TimeStepZone;
		using DataGlobals::SecInHour;
		using DataSizing::PlantSizData;
		using DataSizing::NoSizingFactorMode;
		using DataSizing::GlobalHeatingSizingFactorMode;
		using DataSizing::GlobalCoolingSizingFactorMode;
		using DataSizing::LoopComponentSizingFactorMode;
		using DataSizing::GlobalHeatSizingFactor;
		using DataSizing::GlobalCoolSizingFactor;
		using General::TrimSigDigits;
		using namespace DataPlant;
		using namespace OutputReportPredefined;
		using WeatherManager::Environment;
		using DataHVACGlobals::SmallWaterVolFlow;
		bool SetNewSizes;
		Real64 SizingFac;
		Real64 NormalizedChange;
		Real64 newFoundVolFlowRate;
		std::string chIteration;




		previousVolDesignFlowRate	= PlantSizData( PlantLoopIndex ).DesVolFlowRate;

		if (newFoundMassFlowRateTimeStamp.LogDataValue > 0.0 ) {
			newFoundMassFlowRate		= newFoundMassFlowRateTimeStamp.LogDataValue;
		} else {
			newFoundMassFlowRate		= 0.0;
		}

		newFoundVolFlowRate = newFoundMassFlowRate / DensityForSizing;

		// now apply the correct sizing factor depending on input option
		if ( PlantSizData( PlantLoopIndex ).SizingFactorOption == NoSizingFactorMode ) {
			SizingFac = 1.0;
		} else if ( PlantSizData( PlantLoopIndex ).SizingFactorOption == GlobalHeatingSizingFactorMode ) { 
			SizingFac = GlobalHeatSizingFactor;
		} else if ( PlantSizData( PlantLoopIndex ).SizingFactorOption == GlobalCoolingSizingFactorMode ) {
			SizingFac = GlobalCoolSizingFactor;
		} else if (  PlantSizData( PlantLoopIndex ).SizingFactorOption == LoopComponentSizingFactorMode ) {
			//multiplier used for pumps, often 1.0, from component level sizing fractions
			SizingFac = PlantLoop( PlantLoopIndex ).LoopSide( SupplySide ).Branch( 1 ).PumpSizFac;
		}

		newAdjustedMassFlowRate		= newFoundMassFlowRate * SizingFac; // apply overall heating or cooling sizing factor

		newVolDesignFlowRate		= newAdjustedMassFlowRate / DensityForSizing;

		//compare threshold, 
		SetNewSizes = false;
		if (   ( newVolDesignFlowRate > SmallWaterVolFlow ) // do not use zero size
//			&& ( newVolDesignFlowRate < previousVolDesignFlowRate )// assume only shrink size from noncoincident? nah
			)  { 

			NormalizedChange = std::abs((newVolDesignFlowRate - previousVolDesignFlowRate) 
									/ previousVolDesignFlowRate);
			if (NormalizedChange > SignificantNormalizedChange ) {
				AnotherIterationDesired = true;
				SetNewSizes = true;
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
//		PreDefTableEntry( pdchPlantSizPass, PlantLoop( PlantLoopIndex ).Name, HVACSizingIterCount );
		chIteration = TrimSigDigits(HVACSizingIterCount);
		PreDefTableEntry( pdchPlantSizPrevVdot, PlantLoop( PlantLoopIndex ).Name + " Sizing Pass " + chIteration , previousVolDesignFlowRate , 6 );
		PreDefTableEntry( pdchPlantSizMeasVdot, PlantLoop( PlantLoopIndex ).Name + " Sizing Pass " + chIteration , newFoundVolFlowRate , 6 );
		PreDefTableEntry( pdchPlantSizCalcVdot, PlantLoop( PlantLoopIndex ).Name + " Sizing Pass " + chIteration , newVolDesignFlowRate , 6 );

		if (SetNewSizes) {
			PreDefTableEntry( pdchPlantSizCoincYesNo, PlantLoop( PlantLoopIndex ).Name + " Sizing Pass " + chIteration , "Yes" );
		} else {
			PreDefTableEntry( pdchPlantSizCoincYesNo, PlantLoop( PlantLoopIndex ).Name + " Sizing Pass " + chIteration , "No" );
		}

		PreDefTableEntry( pdchPlantSizDesDay, PlantLoop( PlantLoopIndex ).Name + " Sizing Pass " + chIteration , Environment(newFoundMassFlowRateTimeStamp.EnvrnNum).Title );
		PreDefTableEntry( pdchPlantSizPkTimeDayOfSim, PlantLoop( PlantLoopIndex ).Name + " Sizing Pass " + chIteration , newFoundMassFlowRateTimeStamp.DayOfSim );
		PreDefTableEntry( pdchPlantSizPkTimeHour, PlantLoop( PlantLoopIndex ).Name + " Sizing Pass " + chIteration , newFoundMassFlowRateTimeStamp.HourOfDay - 1 );
		PreDefTableEntry( pdchPlantSizPkTimeMin, PlantLoop( PlantLoopIndex ).Name + " Sizing Pass " + chIteration , newFoundMassFlowRateTimeStamp.stepStartMinute, 0 );
	
	}
}
