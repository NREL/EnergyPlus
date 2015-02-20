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
	ZoneTimestepObject::ZoneTimestepObject( )
	{
		kindOfSim			= 0;
		envrnNum			= 0;
		designDayNum		= 0;
		dayOfSim			= 0;
		hourOfDay			= 0;
		ztStepsIntoPeriod	= 0;
		stepStartMinute		= 0.0;
		stepEndMinute		= 0.0;
		timeStepDuration	= 0.0;
	}


	ZoneTimestepObject::ZoneTimestepObject(
		int kindSim,
		int environmentNum,
		int desDayNum,
		int daySim,
		int hourDay,
		int stepEndMin,
		Real64 timeStepDurat,
		int numOfTimeStepsPerHour )
	{
		Real64 const minutesPerHour( 60.0 );
		int const hoursPerDay( 24 );

		kindOfSim		= kindSim;
		envrnNum		= environmentNum;
		designDayNum	= desDayNum;
		dayOfSim		= daySim;
		hourOfDay		= hourDay;
		stepEndMinute	= stepEndMin;
		timeStepDuration	= timeStepDurat;

		stepStartMinute = stepEndMinute - timeStepDuration * minutesPerHour;
		if ( stepStartMinute < 0.0 ){
		// then TimeValue(1).CurMinute has not been updated
			stepStartMinute = 0.0;
			stepEndMinute   = timeStepDuration * minutesPerHour;
		}

		ztStepsIntoPeriod = ((dayOfSim - 1) * (hoursPerDay * numOfTimeStepsPerHour) ) + //multiple days
			((hourOfDay-1) * numOfTimeStepsPerHour ) + //so far this day's hours
			round( (stepStartMinute / minutesPerHour)/(timeStepDuration) ); // into current hour

		if ( ztStepsIntoPeriod < 0 ) ztStepsIntoPeriod = 0;


		//We only expect this feature to be used with systems, so there will always be a system timestep update, at least one.  
		hasSystemSubSteps =  true;
		numSubSteps = 1;
		subSteps.resize( numSubSteps );

	}

	int SizingLog::GetZtStepIndex (
		const ZoneTimestepObject tmpztStepStamp )
	{
		using DataGlobals::ksHVACSizeDesignDay;
		int vecIndex;
		int logEnvrnIndex( 0 );
		int logCounter( 0 );
		//find which sizing envrn index

		if (tmpztStepStamp.kindOfSim == ksHVACSizeDesignDay) {
			for (int i = 0; i < NumOfDesignDaysInLogSet; i++ ) {
				if ( tmpztStepStamp.envrnNum == EnvrnIndexMapByEnvrn[ logCounter ] ) {
					logEnvrnIndex = logCounter;
					break;
				}
			logCounter++;
			}
		}

		if ( tmpztStepStamp.ztStepsIntoPeriod > 0 ) {
			vecIndex = EnvrnStartZtStepIndex[ logEnvrnIndex ] + tmpztStepStamp.ztStepsIntoPeriod;
		} else {
			vecIndex = EnvrnStartZtStepIndex[ logEnvrnIndex ];
		}

		// next constrain index to lie inside correct envronment
		if ( vecIndex < EnvrnStartZtStepIndex[ logEnvrnIndex ] ) { 
			vecIndex = EnvrnStartZtStepIndex[ logEnvrnIndex ]; // first step in environment
		}

		if (vecIndex > (EnvrnStartZtStepIndex[ logEnvrnIndex ] + ztStepCountByEnvrn[ logEnvrnIndex ]) ) {
			vecIndex = EnvrnStartZtStepIndex[ logEnvrnIndex ] + ztStepCountByEnvrn[ logEnvrnIndex ]; // last step in environment
		}
		return vecIndex;
	}

	void SizingLog::FillZoneStep(
		ZoneTimestepObject tmpztStepStamp )
	{
		int index =  GetZtStepIndex( tmpztStepStamp ); 

		ztStepObj[ index ].kindOfSim		= tmpztStepStamp.kindOfSim;
		ztStepObj[ index ].envrnNum			= tmpztStepStamp.envrnNum;
		ztStepObj[ index ].designDayNum		= tmpztStepStamp.designDayNum;
		ztStepObj[ index ].dayOfSim			= tmpztStepStamp.dayOfSim;
		ztStepObj[ index ].hourOfDay		= tmpztStepStamp.hourOfDay;
		ztStepObj[ index ].ztStepsIntoPeriod = tmpztStepStamp.ztStepsIntoPeriod;
		ztStepObj[ index ].stepStartMinute	= tmpztStepStamp.stepStartMinute;
		ztStepObj[ index ].stepEndMinute	= tmpztStepStamp.stepEndMinute;
		ztStepObj[ index ].timeStepDuration	= tmpztStepStamp.timeStepDuration;

		ztStepObj[ index ].logDataValue		= *p_rVariable;

	}

	int SizingLog::GetSysStepZtStepIndex(
		ZoneTimestepObject tmpztStepStamp )
	{
	// this method finds a zone timestep for the system timestep update to use
	// system timesteps are substeps inside a zone timestep, but are updated 
	// before the zone step has been called.
		int lastZnStepIndex( 0 );
		int znStepIndex( -1 );

		lastZnStepIndex =  GetZtStepIndex( tmpztStepStamp );
		znStepIndex = lastZnStepIndex + 1;

		for ( auto & E : EnvrnStartZtStepIndex) {
			//check if at the beginning of an environment
			if ( E == lastZnStepIndex ) { // don't advance yet
				znStepIndex = lastZnStepIndex;
			}
			//check if at the end of an environment
			if (E == znStepIndex ) { // don't kick over into the next environment
				znStepIndex = lastZnStepIndex;
			}
		}

		//safety checks, or assert?
		if ( znStepIndex >= NumOfStepsInLogSet ) znStepIndex = NumOfStepsInLogSet - 1;
		if ( znStepIndex < 0 ) znStepIndex = 0;

		return znStepIndex;
	}

	void SizingLog::FillSysStep(
		ZoneTimestepObject tmpztStepStamp ,
		SystemTimestepObject tmpSysStepStamp )
	{
		using DataLoopNode::Node;
		int lastZnStepIndex( 0 );
		int ztIndex( 0 );
		int systIndex ( 0 );
		int oldNumSubSteps;
		int newNumSubSteps;
		Real64 const MinutesPerHour( 60.0 );
		Real64 StartDiff; //in fractional hours
		Real64 ZoneStepStartMinutes;

		ztIndex = GetSysStepZtStepIndex(tmpztStepStamp);
		
		if (ztStepObj[ ztIndex ].hasSystemSubSteps ) {
		
			oldNumSubSteps  = ztStepObj[ ztIndex ].numSubSteps;
			newNumSubSteps = round ( tmpztStepStamp.timeStepDuration / tmpSysStepStamp.TimeStepDuration ) ;
			if ( newNumSubSteps != oldNumSubSteps ) {
				ztStepObj[ ztIndex ].subSteps.resize( newNumSubSteps );
				ztStepObj[ ztIndex ].numSubSteps = newNumSubSteps;
			}
		} else {
			newNumSubSteps = round ( tmpztStepStamp.timeStepDuration / tmpSysStepStamp.TimeStepDuration ) ;
			ztStepObj[ ztIndex ].subSteps.resize( newNumSubSteps );
			ztStepObj[ ztIndex ].numSubSteps = newNumSubSteps;
			ztStepObj[ ztIndex ].hasSystemSubSteps = true;
		}


			// figure out which index this substep needs to go into
			// the zone step level data are not yet available for minute, but we can get the previous zone step data...
		lastZnStepIndex = (ztIndex - 1);
		for ( auto & E : EnvrnStartZtStepIndex) {
			if (E == ztIndex ) { // don't drop back into the previous environment
					lastZnStepIndex = ztIndex ;
			}
		}

		ZoneStepStartMinutes = ztStepObj[lastZnStepIndex].stepEndMinute;
		if (ZoneStepStartMinutes < 0.0 ) ZoneStepStartMinutes = 0.0;

		tmpSysStepStamp.stStepsIntoZoneStep = round( 
			(( ( tmpSysStepStamp.CurMinuteStart - ZoneStepStartMinutes ) / MinutesPerHour) 
			/ tmpSysStepStamp.TimeStepDuration) ) ;

		ztStepObj[ ztIndex ].subSteps[ tmpSysStepStamp.stStepsIntoZoneStep ] = tmpSysStepStamp;
		ztStepObj[ ztIndex ].subSteps[ tmpSysStepStamp.stStepsIntoZoneStep ].LogDataValue = *p_rVariable;

	}

	void SizingLog::AverageSysTimeSteps( )
	{
		Real64 RunningSum;

		for ( auto &Zt : ztStepObj ){
			if ( Zt.numSubSteps > 0) {
				RunningSum = 0.0;
				for ( auto &SysT : Zt.subSteps ) {
					RunningSum += SysT.LogDataValue;
				}
				Zt.logDataValue = RunningSum / double( Zt.numSubSteps );
			}
		}
	}

	void SizingLog::ProcessRunningAverage ()
	{
		Real64 RunningSum = 0.0;
		Real64 divisor = 0.0;
		divisor = double( timeStepsInAverage );

		for (int k = 0; k < NumOfEnvironmentsInLogSet; k++) { // outer loop over environments in log set

			for (int i = 0; i < ztStepCountByEnvrn[ k ]; i++ ){ // next inner loop over zone timestep steps 

				if ( timeStepsInAverage > 0 ) {
					RunningSum = 0.0;
					for ( int j = 0; j < timeStepsInAverage ; j++ ) { //
						if ( (i - j) < 0) {
							RunningSum += ztStepObj[ EnvrnStartZtStepIndex[ k ] ].logDataValue; //just use first value to fill early steps
						} else {
							RunningSum += ztStepObj[ ((i - j) + EnvrnStartZtStepIndex[ k ]) ].logDataValue;
						}
					}
					ztStepObj[ (i + EnvrnStartZtStepIndex[ k ]) ].runningAvgDataValue = RunningSum / divisor;
				}
			}
		}
	}

	ZoneTimestepObject SizingLog::GetLogVariableDataMax( )
	{
		Real64 MaxVal;
		ZoneTimestepObject tmpztStepStamp;
		MaxVal = 0.0;

		for ( auto &Zt : ztStepObj ){
			
			if ( Zt.runningAvgDataValue > MaxVal) {
				MaxVal = Zt.runningAvgDataValue;
				tmpztStepStamp = Zt;
				}
		}
	return tmpztStepStamp;
	}

	Real64 SizingLog::GetLogVariableDataAtTimestamp (
		ZoneTimestepObject tmpztStepStamp
	){
		int index;
		Real64 val;

		index =  GetZtStepIndex( tmpztStepStamp ); 

		val = ztStepObj[index].runningAvgDataValue ;

		return val;
	}

	void SizingLog::AdjustEnvrnIndexMapForIteration(
		int const HVACSizingIterCount
	){
		for ( int i = 0; i < NumOfEnvironmentsInLogSet; i++ ) {
			EnvrnIndexMapByEnvrn[ i ] = EnvrnIndexMapByEnvrn[ i ] + NumOfEnvironmentsInLogSet;
		}
	}

	void SizingLog::ReInitLogForIteration(){
		ZoneTimestepObject tmpNullztStepObj;

		for ( auto &Zt : ztStepObj ){
			Zt = tmpNullztStepObj;
		}
	}

	int SizingLoggerFramework::SetupVariableSizingLog(
//		int const SupplySideInletNodeNum  // change to pointer setup 
		Real64 & rVariable,
		int stepsInAverage
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

		tmpLog.p_rVariable = & rVariable; 
		tmpLog.timeStepsInAverage = stepsInAverage;
		VectorLength = tmpLog.NumOfEnvironmentsInLogSet * HoursPerDay * NumOfTimeStepInHour;
		tmpLog.NumOfStepsInLogSet = VectorLength;
		tmpLog.ztStepObj.resize( VectorLength );

		logObjs.push_back( tmpLog );
		NumOfLogs++;
		return NumOfLogs - 1;
		
	}

	ZoneTimestepObject SizingLoggerFramework::PrepareZoneTimestepStamp() 
	{
		//prepare current timing data once and then pass into fill routines
		//function used by both zone and system frequency log updates

		using DataGlobals::KindOfSim;
		using DataGlobals::DayOfSim;
		using DataGlobals::HourOfDay;
		using DataGlobals::NumOfTimeStepInHour;
		using namespace WeatherManager;
		using namespace OutputProcessor;
		int const ZoneIndex ( 1 );

		ZoneTimestepObject tmpztStepStamp( // call constructor
			KindOfSim,
			Envrn,
			Environment( Envrn ).DesignDayNum,
			DayOfSim,
			HourOfDay,
			TimeValue( ZoneIndex ).CurMinute,
			TimeValue( ZoneIndex ).TimeStep,
			NumOfTimeStepInHour ); 

		return tmpztStepStamp;
	}

	void SizingLoggerFramework::UpdateSizingLogValuesZoneStep()
	{
		ZoneTimestepObject tmpztStepStamp; 

		tmpztStepStamp = PrepareZoneTimestepStamp();
		
		for ( auto &L : logObjs ) {
			L.FillZoneStep(tmpztStepStamp);
		}
	}

	void SizingLoggerFramework::UpdateSizingLogValuesSystemStep() 
	{
		int const SysIndex ( 2 );
		Real64 const MinutesPerHour( 60.0 );
		int const HoursPerDay( 24 );
		using namespace OutputProcessor;
		ZoneTimestepObject tmpztStepStamp; 
		SystemTimestepObject tmpSysStepStamp;

		tmpztStepStamp = PrepareZoneTimestepStamp();

		//pepare system timestep stamp
		tmpSysStepStamp.CurMinuteEnd = TimeValue( SysIndex ).CurMinute;
		if ( tmpSysStepStamp.CurMinuteEnd == 0.0 ) { tmpSysStepStamp.CurMinuteEnd = MinutesPerHour; }
		tmpSysStepStamp.CurMinuteStart = tmpSysStepStamp.CurMinuteEnd - TimeValue( SysIndex ).TimeStep * MinutesPerHour;
		tmpSysStepStamp.TimeStepDuration = TimeValue( SysIndex ).TimeStep;

		for ( auto &L : logObjs ) {
			L.FillSysStep(tmpztStepStamp, tmpSysStepStamp);
		}

	}

	void SizingLoggerFramework::IncrementSizingPeriodSet( 
		int const HVACSizingIterCount )
	{
		for ( auto &L : this -> logObjs ) {
			L.AdjustEnvrnIndexMapForIteration( HVACSizingIterCount );
			L.ReInitLogForIteration();
		}
	}
	
	PlantCoinicidentAnalysis::PlantCoinicidentAnalysis ( 
		std::string loopName,
		int loopIndex,
		int nodeNum,
		Real64 density,
			Real64 cp,
		int numStepsInAvg,
		int sizingIndex ) 
	{
		name= loopName;
		plantLoopIndex = loopIndex;
		supplySideInletNodeNum = nodeNum;
		densityForSizing = density;
		specificHeatForSizing = cp;
		numTimeStepsInAvg = numStepsInAvg;
		plantSizingIndex = sizingIndex;
	}
		

	void PlantCoinicidentAnalysis::ResolveDesignFlowRate(
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
		Real64 PeakDemandReturnTemp;
		Real64 PeakDemandMassFlow;
		bool SetNewSizes;
		Real64 SizingFac;
		Real64 NormalizedChange;
		Real64 newFoundVolFlowRate;
		Real64 peakLoadCalculatedMassFlow;
		std::string chIteration;

		previousVolDesignFlowRate	= PlantSizData( plantLoopIndex ).DesVolFlowRate;

		if (newFoundMassFlowRateTimeStamp.runningAvgDataValue > 0.0 ) {
			newFoundMassFlowRate		= newFoundMassFlowRateTimeStamp.runningAvgDataValue;
		} else {
			newFoundMassFlowRate		= 0.0;
		}

		//step 3 calculate mdot from max load and delta T
		if ( (NewFoundMaxDemandTimeStamp.runningAvgDataValue > 0.0) &&
			((specificHeatForSizing * PlantSizData( plantSizingIndex ).DeltaT) > 0.0))  {
				peakLoadCalculatedMassFlow = NewFoundMaxDemandTimeStamp.runningAvgDataValue / 
											(specificHeatForSizing * PlantSizData( plantSizingIndex ).DeltaT);
		} else {
			peakLoadCalculatedMassFlow = 0.0;
		}

		newFoundMassFlowRate = max( newFoundMassFlowRate, peakLoadCalculatedMassFlow ); //step 4, take larger of the two

		newFoundVolFlowRate = newFoundMassFlowRate / densityForSizing;

		// now apply the correct sizing factor depending on input option
		if ( PlantSizData( plantLoopIndex ).SizingFactorOption == NoSizingFactorMode ) {
			SizingFac = 1.0;
		} else if ( PlantSizData( plantLoopIndex ).SizingFactorOption == GlobalHeatingSizingFactorMode ) { 
			SizingFac = GlobalHeatSizingFactor;
		} else if ( PlantSizData( plantLoopIndex ).SizingFactorOption == GlobalCoolingSizingFactorMode ) {
			SizingFac = GlobalCoolSizingFactor;
		} else if (  PlantSizData( plantLoopIndex ).SizingFactorOption == LoopComponentSizingFactorMode ) {
			//multiplier used for pumps, often 1.0, from component level sizing fractions
			SizingFac = PlantLoop( plantLoopIndex ).LoopSide( SupplySide ).Branch( 1 ).PumpSizFac;
		}

		newAdjustedMassFlowRate		= newFoundMassFlowRate * SizingFac; // apply overall heating or cooling sizing factor

		newVolDesignFlowRate		= newAdjustedMassFlowRate / densityForSizing;

		//compare threshold, 
		SetNewSizes = false;
		if (   ( newVolDesignFlowRate > SmallWaterVolFlow ) // do not use zero size
//			&& ( newVolDesignFlowRate < previousVolDesignFlowRate )// assume only shrink size from noncoincident? nah
			)  { 

			NormalizedChange = std::abs((newVolDesignFlowRate - previousVolDesignFlowRate) 
									/ previousVolDesignFlowRate);
			if (NormalizedChange > significantNormalizedChange ) {
				anotherIterationDesired = true;
				SetNewSizes = true;
			} else {
				anotherIterationDesired = false;
			}
		}

		if ( SetNewSizes ) {
		// set new size values for rest of simulation
			PlantSizData( plantLoopIndex ).DesVolFlowRate = newVolDesignFlowRate;

			if (PlantLoop( plantLoopIndex ).MaxVolFlowRateWasAutoSized ) {
				PlantLoop( plantLoopIndex ).MaxVolFlowRate = newVolDesignFlowRate;
				PlantLoop( plantLoopIndex ).MaxMassFlowRate =  newAdjustedMassFlowRate;
			}
			if ( PlantLoop( plantLoopIndex ).VolumeWasAutoSized ) {
				PlantLoop( plantLoopIndex ).Volume = PlantLoop( plantLoopIndex ).MaxVolFlowRate * TimeStepZone * SecInHour / 0.8;
				PlantLoop( plantLoopIndex ).Mass = PlantLoop( plantLoopIndex ).Volume* densityForSizing;
			}


		}
				//report to sizing summary table called Plant Loop Coincident Design Fluid Flow Rates
//		PreDefTableEntry( pdchPlantSizPass, PlantLoop( PlantLoopIndex ).Name, HVACSizingIterCount );
		chIteration = TrimSigDigits(HVACSizingIterCount);
		PreDefTableEntry( pdchPlantSizPrevVdot, PlantLoop( plantLoopIndex ).Name + " Sizing Pass " + chIteration , previousVolDesignFlowRate , 6 );
		PreDefTableEntry( pdchPlantSizMeasVdot, PlantLoop( plantLoopIndex ).Name + " Sizing Pass " + chIteration , newFoundVolFlowRate , 6 );
		PreDefTableEntry( pdchPlantSizCalcVdot, PlantLoop( plantLoopIndex ).Name + " Sizing Pass " + chIteration , newVolDesignFlowRate , 6 );

		if (SetNewSizes) {
			PreDefTableEntry( pdchPlantSizCoincYesNo, PlantLoop( plantLoopIndex ).Name + " Sizing Pass " + chIteration , "Yes" );
		} else {
			PreDefTableEntry( pdchPlantSizCoincYesNo, PlantLoop( plantLoopIndex ).Name + " Sizing Pass " + chIteration , "No" );
		}

		PreDefTableEntry( pdchPlantSizDesDay, PlantLoop( plantLoopIndex ).Name + " Sizing Pass " + chIteration , Environment(newFoundMassFlowRateTimeStamp.envrnNum).Title );
		PreDefTableEntry( pdchPlantSizPkTimeDayOfSim, PlantLoop( plantLoopIndex ).Name + " Sizing Pass " + chIteration , newFoundMassFlowRateTimeStamp.dayOfSim );
		PreDefTableEntry( pdchPlantSizPkTimeHour, PlantLoop( plantLoopIndex ).Name + " Sizing Pass " + chIteration , newFoundMassFlowRateTimeStamp.hourOfDay - 1 );
		PreDefTableEntry( pdchPlantSizPkTimeMin, PlantLoop( plantLoopIndex ).Name + " Sizing Pass " + chIteration , newFoundMassFlowRateTimeStamp.stepStartMinute, 0 );
	
	}
}
