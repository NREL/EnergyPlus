// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

// C++ Headers
#include <string>
#include <vector>
#include <map>

// ObjexxFCL Headers
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <SizingAnalysisObjects.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataSizing.hh>
#include <General.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <WeatherManager.hh>

namespace EnergyPlus {
	ZoneTimestepObject::ZoneTimestepObject()
	{
		kindOfSim = 0;
		envrnNum = 0;
		dayOfSim = 0;
		hourOfDay = 0;
		ztStepsIntoPeriod = 0;
		stepStartMinute = 0.0;
		stepEndMinute = 0.0;
		timeStepDuration = 0.0;
	}


	ZoneTimestepObject::ZoneTimestepObject(
		int kindSim,
		int environmentNum,
		int daySim,
		int hourDay,
		int stepEndMin,
		Real64 timeStepDurat,
		int numOfTimeStepsPerHour
	)
		:	kindOfSim( kindSim ),
			envrnNum( environmentNum ),
			dayOfSim( daySim ),
			hourOfDay( hourDay ),
			stepEndMinute( stepEndMin ),
			timeStepDuration( timeStepDurat )
	{
		Real64 const minutesPerHour( 60.0 );
		int const hoursPerDay( 24 );

		stepStartMinute = stepEndMinute - timeStepDuration * minutesPerHour;
		if ( stepStartMinute < 0.0 ) {
		// then TimeValue(1).CurMinute has not been updated
			stepStartMinute = 0.0;
			stepEndMinute = timeStepDuration * minutesPerHour;
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

	SizingLog::SizingLog( double & rVariable ) :
	p_rVariable( rVariable )
	{}

	int SizingLog::GetZtStepIndex (
		const ZoneTimestepObject tmpztStepStamp )
	{

		int vecIndex;

		if ( tmpztStepStamp.ztStepsIntoPeriod > 0 ) { // discard any negative value for safety
			vecIndex = envrnStartZtStepIndexMap[ newEnvrnToSeedEnvrnMap[ tmpztStepStamp.envrnNum ] ] + tmpztStepStamp.ztStepsIntoPeriod;
		} else {
			vecIndex = envrnStartZtStepIndexMap[  newEnvrnToSeedEnvrnMap[ tmpztStepStamp.envrnNum ]  ];
		}

		// next for safety sake, constrain index to lie inside correct envronment
		if ( vecIndex < envrnStartZtStepIndexMap[  newEnvrnToSeedEnvrnMap[ tmpztStepStamp.envrnNum ]  ] ) {
			vecIndex = envrnStartZtStepIndexMap[  newEnvrnToSeedEnvrnMap[ tmpztStepStamp.envrnNum ]  ]; // first step in environment
		}
		if (vecIndex > ( envrnStartZtStepIndexMap[  newEnvrnToSeedEnvrnMap[ tmpztStepStamp.envrnNum ]  ]
				+ ztStepCountByEnvrnMap[  newEnvrnToSeedEnvrnMap[ tmpztStepStamp.envrnNum ]  ]) ) {
			vecIndex = envrnStartZtStepIndexMap[  newEnvrnToSeedEnvrnMap[ tmpztStepStamp.envrnNum ]  ]
				+ ztStepCountByEnvrnMap[  newEnvrnToSeedEnvrnMap[ tmpztStepStamp.envrnNum ]  ]; // last step in environment
		}
		return vecIndex;
	}

	void SizingLog::FillZoneStep(
		ZoneTimestepObject tmpztStepStamp )
	{
		int index =  GetZtStepIndex( tmpztStepStamp );

		ztStepObj[ index ].kindOfSim = tmpztStepStamp.kindOfSim;
		ztStepObj[ index ].envrnNum = tmpztStepStamp.envrnNum;
		ztStepObj[ index ].dayOfSim = tmpztStepStamp.dayOfSim;
		ztStepObj[ index ].hourOfDay = tmpztStepStamp.hourOfDay;
		ztStepObj[ index ].ztStepsIntoPeriod = tmpztStepStamp.ztStepsIntoPeriod;
		ztStepObj[ index ].stepStartMinute = tmpztStepStamp.stepStartMinute;
		ztStepObj[ index ].stepEndMinute = tmpztStepStamp.stepEndMinute;
		ztStepObj[ index ].timeStepDuration = tmpztStepStamp.timeStepDuration;

		ztStepObj[ index ].logDataValue = p_rVariable;

	}

	int SizingLog::GetSysStepZtStepIndex(
		ZoneTimestepObject tmpztStepStamp
	)
	{
	// this method finds a zone timestep for the system timestep update to use
	// system timesteps are substeps inside a zone timestep, but are updated
	// before the zone step has been called.
		int lastZnStepIndex =  GetZtStepIndex( tmpztStepStamp );
		int znStepIndex = lastZnStepIndex + 1;

		std::map< int, int >:: iterator end = envrnStartZtStepIndexMap.end();
		for (std::map< int, int >:: iterator itr = envrnStartZtStepIndexMap.begin(); itr != end; ++itr) {

			//check if at the beginning of an environment
			if ( itr->second == lastZnStepIndex ) { // don't advance yet
				znStepIndex = lastZnStepIndex;
			}
			//check if at the end of an environment
			if ( itr->second == znStepIndex ) { // don't kick over into the next environment
				znStepIndex = lastZnStepIndex;
			}
		}

		//last safety checks for range
		if ( znStepIndex >= NumOfStepsInLogSet ) znStepIndex = NumOfStepsInLogSet - 1;
		if ( znStepIndex < 0 ) znStepIndex = 0;

		return znStepIndex;
	}

	void SizingLog::FillSysStep(
		ZoneTimestepObject tmpztStepStamp ,
		SystemTimestepObject tmpSysStepStamp
	)
	{
		int lastZnStepIndex( 0 );
		int ztIndex( 0 );
		int oldNumSubSteps;
		int newNumSubSteps;
		Real64 const MinutesPerHour( 60.0 );
		Real64 ZoneStepStartMinutes;

		ztIndex = GetSysStepZtStepIndex(tmpztStepStamp);

		if (ztStepObj[ ztIndex ].hasSystemSubSteps ) {

			oldNumSubSteps = ztStepObj[ ztIndex ].numSubSteps;
			newNumSubSteps = round ( tmpztStepStamp.timeStepDuration / tmpSysStepStamp.TimeStepDuration );
			if ( newNumSubSteps != oldNumSubSteps ) {
				ztStepObj[ ztIndex ].subSteps.resize( newNumSubSteps );
				ztStepObj[ ztIndex ].numSubSteps = newNumSubSteps;
			}
		} else {
			newNumSubSteps = round ( tmpztStepStamp.timeStepDuration / tmpSysStepStamp.TimeStepDuration );
			ztStepObj[ ztIndex ].subSteps.resize( newNumSubSteps );
			ztStepObj[ ztIndex ].numSubSteps = newNumSubSteps;
			ztStepObj[ ztIndex ].hasSystemSubSteps = true;
		}


			// figure out which index this substep needs to go into
			// the zone step level data are not yet available for minute, but we can get the previous zone step data...
		lastZnStepIndex = (ztIndex - 1);
		std::map< int, int >:: iterator end = envrnStartZtStepIndexMap.end();
		for (std::map< int, int >:: iterator itr = envrnStartZtStepIndexMap.begin(); itr != end; ++itr) {
			if ( itr->second == ztIndex ) { // don't drop back into the previous environment
					lastZnStepIndex = ztIndex;
			}
		}

		ZoneStepStartMinutes = ztStepObj[lastZnStepIndex].stepEndMinute;
		if (ZoneStepStartMinutes < 0.0 ) ZoneStepStartMinutes = 0.0;

		tmpSysStepStamp.stStepsIntoZoneStep = round(
			(( ( tmpSysStepStamp.CurMinuteStart - ZoneStepStartMinutes ) / MinutesPerHour)
			/ tmpSysStepStamp.TimeStepDuration) );

		ztStepObj[ ztIndex ].subSteps[ tmpSysStepStamp.stStepsIntoZoneStep ] = tmpSysStepStamp;
		ztStepObj[ ztIndex ].subSteps[ tmpSysStepStamp.stStepsIntoZoneStep ].LogDataValue = p_rVariable;

	}

	void SizingLog::AverageSysTimeSteps()
	{
		Real64 RunningSum;

		for ( auto &Zt : ztStepObj ) {
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
		Real64 divisor = double( timeStepsInAverage );

		std::map< int, int >:: iterator end = ztStepCountByEnvrnMap.end();
		for (std::map< int, int >:: iterator itr = ztStepCountByEnvrnMap.begin(); itr != end; ++itr) {
//		for (int k = 0; k < NumOfEnvironmentsInLogSet; k++) { // outer loop over environments in log set

//			for ( int i = 0; i < ztStepCountByEnvrn[ k ]; ++i ) { // next inner loop over zone timestep steps
			for ( int i = 0; i < itr->second; ++i ) { // next inner loop over zone timestep steps

				if ( timeStepsInAverage > 0 ) {
					RunningSum = 0.0;
					for ( int j = 0; j < timeStepsInAverage; ++j ) { //
						if ( (i - j) < 0) {
							RunningSum += ztStepObj[ envrnStartZtStepIndexMap[ itr->first ] ].logDataValue; //just use first value to fill early steps
						} else {
							RunningSum += ztStepObj[ ( (i - j) + envrnStartZtStepIndexMap[ itr->first ] ) ].logDataValue;
						}
					}
					ztStepObj[ (i + envrnStartZtStepIndexMap[ itr->first ] ) ].runningAvgDataValue = RunningSum / divisor;
				}
			}
		}
	}

	ZoneTimestepObject SizingLog::GetLogVariableDataMax()
	{
		Real64 MaxVal;
		ZoneTimestepObject tmpztStepStamp;
		MaxVal = 0.0;

		if ( ! ztStepObj.empty() ) {
			tmpztStepStamp = ztStepObj[ 1 ];
		}

		for ( auto &Zt : ztStepObj ) {

			if ( Zt.runningAvgDataValue > MaxVal) {
				MaxVal = Zt.runningAvgDataValue;
				tmpztStepStamp = Zt;
				}
		}
	return tmpztStepStamp;
	}

	Real64 SizingLog::GetLogVariableDataAtTimestamp(
		ZoneTimestepObject tmpztStepStamp
	)
	{
		int const index =  GetZtStepIndex( tmpztStepStamp );

		Real64 const val = ztStepObj[index].runningAvgDataValue;

		return val;
	}

	void SizingLog::ReInitLogForIteration()
	{
		ZoneTimestepObject tmpNullztStepObj;

		for ( auto &Zt : ztStepObj ) {
			Zt = tmpNullztStepObj;
		}
	}

	void SizingLog::SetupNewEnvironment(
		int const seedEnvrnNum,
		int const newEnvrnNum
	)
	{
		newEnvrnToSeedEnvrnMap[ newEnvrnNum ] = seedEnvrnNum;
	}

	int SizingLoggerFramework::SetupVariableSizingLog(
//		int const SupplySideInletNodeNum  // change to pointer setup
		Real64 & rVariable,
		int stepsInAverage
	)
	{
		using DataGlobals::NumOfTimeStepInHour;
		using DataGlobals::ksDesignDay;
		using DataGlobals::ksRunPeriodDesign;
		using namespace WeatherManager;
		int VectorLength( 0 );
		int const HoursPerDay( 24 );

		SizingLog tmpLog( rVariable );
		tmpLog.NumOfEnvironmentsInLogSet = 0;
		tmpLog.NumOfDesignDaysInLogSet = 0;
		tmpLog.NumberOfSizingPeriodsInLogSet = 0;

		// search environment structure for sizing periods
		// this is coded to occur before the additions to Environment structure that will occur to run them as HVAC Sizing sims
		for ( int i = 1; i <= NumOfEnvrn; ++i ) {
			if ( Environment( i ).KindOfEnvrn == ksDesignDay ) {
				++tmpLog.NumOfEnvironmentsInLogSet;
				++tmpLog.NumOfDesignDaysInLogSet;
			}
			if ( Environment( i ).KindOfEnvrn == ksRunPeriodDesign ) {
				++tmpLog.NumOfEnvironmentsInLogSet;
				++tmpLog.NumberOfSizingPeriodsInLogSet;
			}
		}

		// next fill in the count of steps into map
		for ( int i = 1; i <= NumOfEnvrn; ++i ) {

			if ( Environment( i ).KindOfEnvrn == ksDesignDay ) {
				tmpLog.ztStepCountByEnvrnMap[ i ] = HoursPerDay * NumOfTimeStepInHour;
			}
			if ( Environment( i ).KindOfEnvrn == ksRunPeriodDesign ) {
				tmpLog.ztStepCountByEnvrnMap[ i ] = HoursPerDay * NumOfTimeStepInHour * Environment( i ).TotalDays;
			}
		}

		int stepSum = 0;
		std::map< int, int >:: iterator end = tmpLog.ztStepCountByEnvrnMap.end();
		for (std::map< int, int >:: iterator itr = tmpLog.ztStepCountByEnvrnMap.begin(); itr != end; ++itr) {

			tmpLog.envrnStartZtStepIndexMap [ itr->first ] = stepSum;
			stepSum += itr->second;
		}

		tmpLog.timeStepsInAverage = stepsInAverage;

		VectorLength = stepSum;

		tmpLog.NumOfStepsInLogSet = VectorLength;
		tmpLog.ztStepObj.resize( VectorLength );

		logObjs.push_back( tmpLog );
		++NumOfLogs;
		return NumOfLogs - 1;

	}

	void SizingLoggerFramework::SetupSizingLogsNewEnvironment ()
	{
		using namespace WeatherManager;

		for ( auto & L : logObjs ) {
			L.SetupNewEnvironment (Environment( Envrn ).SeedEnvrnNum, Envrn);
		}

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

		for ( auto & L : logObjs ) {
			L.FillZoneStep(tmpztStepStamp);
		}
	}

	void SizingLoggerFramework::UpdateSizingLogValuesSystemStep()
	{
		int const SysIndex ( 2 );
		Real64 const MinutesPerHour( 60.0 );
		using namespace OutputProcessor;
		ZoneTimestepObject tmpztStepStamp;
		SystemTimestepObject tmpSysStepStamp;

		tmpztStepStamp = PrepareZoneTimestepStamp();

		//pepare system timestep stamp
		tmpSysStepStamp.CurMinuteEnd = TimeValue( SysIndex ).CurMinute;
		if ( tmpSysStepStamp.CurMinuteEnd == 0.0 ) { tmpSysStepStamp.CurMinuteEnd = MinutesPerHour; }
		tmpSysStepStamp.CurMinuteStart = tmpSysStepStamp.CurMinuteEnd - TimeValue( SysIndex ).TimeStep * MinutesPerHour;
		tmpSysStepStamp.TimeStepDuration = TimeValue( SysIndex ).TimeStep;

		for ( auto & L : logObjs ) {
			L.FillSysStep(tmpztStepStamp, tmpSysStepStamp);
		}

	}

	void SizingLoggerFramework::IncrementSizingPeriodSet()
	{
		for ( auto &L : this -> logObjs ) {
			L.ReInitLogForIteration();
		}
	}

	PlantCoinicidentAnalysis::PlantCoinicidentAnalysis(
		std::string loopName,
		int loopIndex,
		int nodeNum,
		Real64 density,
		Real64 cp,
		int numStepsInAvg,
		int sizingIndex
	)
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
	)
	{
		using DataGlobals::OutputFileInits;
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
		using General::RoundSigDigits;
		using namespace DataPlant;
		using namespace OutputReportPredefined;
		using WeatherManager::Environment;
		using DataHVACGlobals::SmallWaterVolFlow;
		bool setNewSizes;
		Real64 sizingFac;
		Real64 normalizedChange;
		Real64 newFoundVolFlowRate;
		Real64 peakLoadCalculatedMassFlow;
		std::string chIteration;
		std::string chSetSizes;
		std::string chDemandTrapUsed;
		static gio::Fmt fmtA( "(A)" );
		bool changedByDemand( false );
		static bool eioHeaderDoneOnce( false );
		bool nullStampProblem;

		// first make sure we have valid time stamps to work with
		if ( CheckTimeStampForNull( newFoundMassFlowRateTimeStamp )
				|| CheckTimeStampForNull( NewFoundMaxDemandTimeStamp ) ) {
			// problem, don't have valid stamp, don't have any info to report either
			nullStampProblem =  true;
		} else {
			nullStampProblem =  false;
		}

		previousVolDesignFlowRate = PlantSizData( plantSizingIndex ).DesVolFlowRate;

		if (newFoundMassFlowRateTimeStamp.runningAvgDataValue > 0.0 ) {
			newFoundMassFlowRate = newFoundMassFlowRateTimeStamp.runningAvgDataValue;
		} else {
			newFoundMassFlowRate = 0.0;
		}

		//step 3 calculate mdot from max load and delta T
		if ( (NewFoundMaxDemandTimeStamp.runningAvgDataValue > 0.0) &&
			((specificHeatForSizing * PlantSizData( plantSizingIndex ).DeltaT) > 0.0))  {
				peakLoadCalculatedMassFlow = NewFoundMaxDemandTimeStamp.runningAvgDataValue /
											(specificHeatForSizing * PlantSizData( plantSizingIndex ).DeltaT);
		} else {
			peakLoadCalculatedMassFlow = 0.0;
		}

		if ( peakLoadCalculatedMassFlow > newFoundMassFlowRate ) {
			changedByDemand = true;
		} else {
			changedByDemand = false;
		}
		newFoundMassFlowRate = max( newFoundMassFlowRate, peakLoadCalculatedMassFlow ); // step 4, take larger of the two

		newFoundVolFlowRate = newFoundMassFlowRate / densityForSizing;

		// now apply the correct sizing factor depending on input option
		sizingFac = 1.0;
		if ( PlantSizData( plantSizingIndex ).SizingFactorOption == NoSizingFactorMode ) {
			sizingFac = 1.0;
		} else if ( PlantSizData( plantSizingIndex ).SizingFactorOption == GlobalHeatingSizingFactorMode ) {
			sizingFac = GlobalHeatSizingFactor;
		} else if ( PlantSizData( plantSizingIndex ).SizingFactorOption == GlobalCoolingSizingFactorMode ) {
			sizingFac = GlobalCoolSizingFactor;
		} else if ( PlantSizData( plantSizingIndex ).SizingFactorOption == LoopComponentSizingFactorMode ) {
			// multiplier used for pumps, often 1.0, from component level sizing fractions
			sizingFac = PlantLoop( plantLoopIndex ).LoopSide( SupplySide ).Branch( 1 ).PumpSizFac;
		}

		newAdjustedMassFlowRate = newFoundMassFlowRate * sizingFac; // apply overall heating or cooling sizing factor

		newVolDesignFlowRate = newAdjustedMassFlowRate / densityForSizing;

		//compare threshold,
		setNewSizes = false;
		normalizedChange = 0.0;
		if ( newVolDesignFlowRate > SmallWaterVolFlow && ! nullStampProblem ) {// do not use zero size or bad stamp data

			normalizedChange = std::abs((newVolDesignFlowRate - previousVolDesignFlowRate) / previousVolDesignFlowRate);
			if (normalizedChange > significantNormalizedChange ) {
				anotherIterationDesired = true;
				setNewSizes = true;
			} else {
				anotherIterationDesired = false;
			}
		}

		if ( setNewSizes ) {
		// set new size values for rest of simulation
			PlantSizData( plantSizingIndex ).DesVolFlowRate = newVolDesignFlowRate;

			if (PlantLoop( plantLoopIndex ).MaxVolFlowRateWasAutoSized ) {
				PlantLoop( plantLoopIndex ).MaxVolFlowRate = newVolDesignFlowRate;
				PlantLoop( plantLoopIndex ).MaxMassFlowRate =  newAdjustedMassFlowRate;
			}
			if ( PlantLoop( plantLoopIndex ).VolumeWasAutoSized ) {
				PlantLoop( plantLoopIndex ).Volume = PlantLoop( plantLoopIndex ).MaxVolFlowRate * TimeStepZone * SecInHour / 0.8;
				PlantLoop( plantLoopIndex ).Mass = PlantLoop( plantLoopIndex ).Volume* densityForSizing;
			}

		}

		// add a seperate eio summary report about what happened, did demand trap get used, what were the key values.
		if (! eioHeaderDoneOnce ) {
			gio::write( OutputFileInits, fmtA ) << "! <Plant Coincident Sizing Algorithm>,Plant Loop Name,Sizing Pass {#},Measured Mass Flow{kg/s},Measured Demand {W},Demand Calculated Mass Flow{kg/s},Sizes Changed {Yes/No},Previous Volume Flow Rate {m3/s},New Volume Flow Rate {m3/s},Demand Check Applied {Yes/No},Sizing Factor {},Normalized Change {},Specific Heat{J/kg-K},Density {kg/m3}";
			eioHeaderDoneOnce = true;
		}
		chIteration = TrimSigDigits(HVACSizingIterCount);
		if ( setNewSizes ) {
			chSetSizes = "Yes";
		} else {
			chSetSizes = "No";
		}
		if ( changedByDemand ) {
			chDemandTrapUsed = "Yes";
		} else {
			chDemandTrapUsed = "No";
		}

		gio::write( OutputFileInits, fmtA ) << "Plant Coincident Sizing Algorithm,"
				+ name + ","
				+ chIteration + ","
				+ RoundSigDigits( newFoundMassFlowRateTimeStamp.runningAvgDataValue, 7 ) + ","
				+ RoundSigDigits( NewFoundMaxDemandTimeStamp.runningAvgDataValue, 2 ) + ","
				+ RoundSigDigits( peakLoadCalculatedMassFlow, 7) + ","
				+ chSetSizes + ","
				+ RoundSigDigits( previousVolDesignFlowRate , 6 ) + ","
				+ RoundSigDigits( newVolDesignFlowRate, 6 ) + ","
				+ chDemandTrapUsed + ","
				+ RoundSigDigits( sizingFac, 4) + ","
				+ RoundSigDigits( normalizedChange, 6 ) + ","
				+ RoundSigDigits( specificHeatForSizing, 4 ) +","
				+ RoundSigDigits( densityForSizing, 4);

		//report to sizing summary table called Plant Loop Coincident Design Fluid Flow Rates

		PreDefTableEntry( pdchPlantSizPrevVdot, PlantLoop( plantLoopIndex ).Name + " Sizing Pass " + chIteration , previousVolDesignFlowRate , 6 );
		PreDefTableEntry( pdchPlantSizMeasVdot, PlantLoop( plantLoopIndex ).Name + " Sizing Pass " + chIteration , newFoundVolFlowRate , 6 );
		PreDefTableEntry( pdchPlantSizCalcVdot, PlantLoop( plantLoopIndex ).Name + " Sizing Pass " + chIteration , newVolDesignFlowRate , 6 );

		if (setNewSizes) {
			PreDefTableEntry( pdchPlantSizCoincYesNo, PlantLoop( plantLoopIndex ).Name + " Sizing Pass " + chIteration , "Yes" );
		} else {
			PreDefTableEntry( pdchPlantSizCoincYesNo, PlantLoop( plantLoopIndex ).Name + " Sizing Pass " + chIteration , "No" );
		}

		if ( ! nullStampProblem ) {
			if ( ! changedByDemand ) {
				if ( newFoundMassFlowRateTimeStamp.envrnNum > 0 ) { // protect against invalid index
					PreDefTableEntry( pdchPlantSizDesDay, PlantLoop( plantLoopIndex ).Name + " Sizing Pass " + chIteration , Environment(newFoundMassFlowRateTimeStamp.envrnNum).Title );
				}
				PreDefTableEntry( pdchPlantSizPkTimeDayOfSim, PlantLoop( plantLoopIndex ).Name + " Sizing Pass " + chIteration ,
					newFoundMassFlowRateTimeStamp.dayOfSim );
				PreDefTableEntry( pdchPlantSizPkTimeHour, PlantLoop( plantLoopIndex ).Name + " Sizing Pass " + chIteration ,
					newFoundMassFlowRateTimeStamp.hourOfDay - 1 );
				PreDefTableEntry( pdchPlantSizPkTimeMin, PlantLoop( plantLoopIndex ).Name + " Sizing Pass " + chIteration ,
					newFoundMassFlowRateTimeStamp.stepStartMinute, 0 );
			} else {
				if ( NewFoundMaxDemandTimeStamp.envrnNum > 0 ) { // protect against invalid index
					PreDefTableEntry( pdchPlantSizDesDay, PlantLoop( plantLoopIndex ).Name + " Sizing Pass " + chIteration , Environment(NewFoundMaxDemandTimeStamp.envrnNum).Title );
				}
				PreDefTableEntry( pdchPlantSizPkTimeDayOfSim, PlantLoop( plantLoopIndex ).Name + " Sizing Pass " + chIteration ,
					NewFoundMaxDemandTimeStamp.dayOfSim );
				PreDefTableEntry( pdchPlantSizPkTimeHour, PlantLoop( plantLoopIndex ).Name + " Sizing Pass " + chIteration ,
					NewFoundMaxDemandTimeStamp.hourOfDay - 1 );
				PreDefTableEntry( pdchPlantSizPkTimeMin, PlantLoop( plantLoopIndex ).Name + " Sizing Pass " + chIteration ,
					NewFoundMaxDemandTimeStamp.stepStartMinute, 0 );
			}
		}
	}

	bool PlantCoinicidentAnalysis::CheckTimeStampForNull(
		ZoneTimestepObject testStamp
	)
	{

		bool isNull = true;

		if ( testStamp.envrnNum != 0 ) {
			isNull = false;
		}
		if ( testStamp.kindOfSim != 0 ) {
			isNull = false;
		}

		return isNull;
	}
}
