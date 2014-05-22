// C++ Headers
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/FArray.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <HVACManager.hh>
#include <AirflowNetworkBalanceManager.hh>
#include <CoolTower.hh>
#include <DataAirflowNetwork.hh>
#include <DataAirLoop.hh>
#include <DataContaminantBalance.hh>
#include <DataConvergParams.hh>
#include <DataEnvironment.hh>
#include <DataErrorTracking.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataReportingFlags.hh>
#include <DataRoomAirModel.hh>
#include <DataSurfaces.hh>
#include <DataSystemVariables.hh>
#include <DataZoneEquipment.hh>
#include <DemandManager.hh>
#include <DisplayRoutines.hh>
#include <EarthTube.hh>
#include <EMSManager.hh>
#include <General.hh>
#include <HVACStandAloneERV.hh>
#include <IceThermalStorage.hh>
#include <InternalHeatGains.hh>
#include <ManageElectricPower.hh>
#include <NodeInputManager.hh>
#include <NonZoneEquipmentManager.hh>
#include <OutAirNodeManager.hh>
#include <OutputProcessor.hh>
#include <OutputReportTabular.hh>
#include <PlantCondLoopOperation.hh>
#include <PlantManager.hh>
#include <PlantUtilities.hh>
#include <PollutionModule.hh>
#include <Psychrometrics.hh>
#include <RefrigeratedCase.hh>
#include <ScheduleManager.hh>
#include <SetPointManager.hh>
#include <SimAirServingZones.hh>
#include <SystemAvailabilityManager.hh>
#include <SystemReports.hh>
#include <ThermalChimney.hh>
#include <UtilityRoutines.hh>
#include <WaterManager.hh>
#include <ZoneContaminantPredictorCorrector.hh>
#include <ZoneEquipmentManager.hh>
#include <ZoneTempPredictorCorrector.hh>

namespace EnergyPlus {

namespace HVACManager {

	// PURPOSE OF THIS MODULE:
	// This module contains the high level HVAC control
	// subroutines.  Subroutine ManageHVAC, which is called from the heat balance,
	// calls the HVAC simulation and is the most probable insertion point for
	// connections to other HVAC engines.  ManageHVAC also controls the system
	// timestep, automatically shortening the timestep to meet convergence criteria.

	// METHODOLOGY EMPLOYED:
	// The basic solution technique is iteration with lagging.
	// The timestep is shortened using a bisection method.

	// REFERENCES:

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::TimeStepZone;
	using DataGlobals::WarmupFlag;
	using DataGlobals::EndHourFlag;
	using DataGlobals::BeginDayFlag;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::SecInHour;
	using DataGlobals::BeginTimeStepFlag;
	using DataGlobals::KickOffSimulation;
	using DataGlobals::DayOfSim;
	using DataGlobals::HourOfDay;
	using DataGlobals::TimeStep;
	using DataGlobals::NumOfZones;
	using DataGlobals::OutputFileDebug;
	using DataGlobals::OutputFileStandard;
	using DataGlobals::OutputFileMeters;
	using DataGlobals::ZoneSizingCalc;
	using DataGlobals::DuringDay;
	using DataGlobals::DoOutputReporting;
	using DataGlobals::HVACTSReporting;
	using DataGlobals::SysSizingCalc;
	using DataGlobals::DisplayExtraWarnings;
	using DataGlobals::MetersHaveBeenInitialized;
	using DataGlobals::emsCallFromBeginTimestepBeforePredictor;
	using DataGlobals::emsCallFromEndSystemTimestepAfterHVACReporting;
	using DataGlobals::emsCallFromBeforeHVACManagers;
	using DataGlobals::emsCallFromAfterHVACManagers;
	using DataGlobals::emsCallFromHVACIterationLoop;
	using DataGlobals::AnyEnergyManagementSystemInModel;
	using DataGlobals::emsCallFromEndSystemTimestepBeforeHVACReporting;
	using DataGlobals::AnyIdealCondEntSetPointInModel;
	using DataGlobals::RunOptCondEntTemp;
	using DataGlobals::isPulseZoneSizing;
	using namespace DataEnvironment;

	using DataHeatBalFanSys::ZTAV;
	using DataHeatBalFanSys::ZT;
	using DataHeatBalFanSys::MAT;
	using DataHeatBalFanSys::ZoneAirHumRat;
	using DataHeatBalFanSys::ZoneAirHumRatAvg;
	using DataHeatBalFanSys::iGetZoneSetPoints;
	using DataHeatBalFanSys::iPredictStep;
	using DataHeatBalFanSys::iCorrectStep;
	using DataHeatBalFanSys::iPushZoneTimestepHistories;
	using DataHeatBalFanSys::iRevertZoneTimestepHistories;
	using DataHeatBalFanSys::iPushSystemTimestepHistories;
	using namespace DataHVACGlobals;
	using namespace DataLoopNode;
	using namespace DataAirLoop;
	using namespace DataConvergParams;
	using DataAirflowNetwork::SimulateAirflowNetwork;
	using DataAirflowNetwork::AirflowNetworkControlSimple;
	using namespace DataReportingFlags;

	// Data
	//MODULE PARAMETER DEFINITIONS:
	// na

	//MODULE VARIABLE DECLARATIONS:

	int HVACManageIteration( 0 ); // counts iterations to enforce maximum iteration limit
	int RepIterAir( 0 );

	FArray1D_bool CrossMixingReportFlag; // TRUE when Cross Mixing is active based on controls
	FArray1D_bool MixingReportFlag; // TRUE when Mixing is active based on controls
	FArray1D< Real64 > VentMCP; // product of mass rate and Cp for each Venitlation object

	//SUBROUTINE SPECIFICATIONS FOR MODULE PrimaryPlantLoops
	// and zone equipment simulations

	// MODULE SUBROUTINES:

	// Functions

	void
	ManageHVAC()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHORS:  Russ Taylor, Dan Fisher
		//       DATE WRITTEN:  Jan. 1998
		//       MODIFIED       Jul 2003 (CC) added a subroutine call for air models
		//       RE-ENGINEERED  May 2008, Brent Griffith, revised variable time step method and zone conditions history

		// PURPOSE OF THIS SUBROUTINE:
		// This routine effectively replaces the IBLAST
		// "SystemDriver" routine.  The main function of the routine
		// is to set the system timestep, "TimeStepSys", call the models related to zone
		// air temperatures, and .

		// METHODOLOGY EMPLOYED:
		//  manage calls to Predictor and Corrector and other updates in ZoneTempPredictorCorrector
		//  manage variable time step and when zone air histories are updated.

		// REFERENCES:

		// Using/Aliasing
		using DataConvergParams::MinTimeStepSys; // =0.0166667     != 1 minute | 0.3 C = (1% OF 300 C) =max allowable diff between ZoneAirTemp at Time=T & T-1
		using DataConvergParams::MaxZoneTempDiff;

		using ZoneTempPredictorCorrector::ManageZoneAirUpdates;
		using ZoneTempPredictorCorrector::DetectOscillatingZoneTemp;

		using NodeInputManager::CalcMoreNodeInfo;
		using ZoneEquipmentManager::UpdateZoneSizing;
		using OutputReportTabular::UpdateTabularReports; // added for writing tabular output reports
		using OutputReportTabular::GatherComponentLoadsHVAC;
		using DataGlobals::CompLoadReportIsReq;
		using SystemReports::InitEnergyReports;
		using SystemReports::ReportMaxVentilationLoads;
		using SystemReports::ReportSystemEnergyUse;
		using PollutionModule::CalculatePollution;
		using DemandManager::ManageDemand;
		using DemandManager::UpdateDemandManagers;
		using EMSManager::ManageEMS;
		using IceThermalStorage::UpdateIceFractions;
		using OutAirNodeManager::SetOutAirNodes;
		using AirflowNetworkBalanceManager::ManageAirflowNetworkBalance;
		using DataAirflowNetwork::RollBackFlag;
		using WaterManager::ManageWater;
		using WaterManager::ManageWaterInits;
		using RefrigeratedCase::ManageRefrigeratedCaseRacks;
		using SystemAvailabilityManager::ManageHybridVentilation;
		using DataHeatBalFanSys::SysDepZoneLoads;
		using DataHeatBalFanSys::SysDepZoneLoadsLagged;
		using DataHeatBalFanSys::ZTAVComf;
		using DataHeatBalFanSys::ZoneAirHumRatAvgComf;
		using DataSystemVariables::ReportDuringWarmup; // added for FMI
		using DataSystemVariables::UpdateDataDuringWarmupExternalInterface;
		using PlantManager::UpdateNodeThermalHistory;
		using ZoneContaminantPredictorCorrector::ManageZoneContaminanUpdates;
		using DataContaminantBalance::Contaminant;
		using DataContaminantBalance::ZoneAirCO2;
		using DataContaminantBalance::ZoneAirCO2Temp;
		using DataContaminantBalance::ZoneAirCO2Avg;
		using DataContaminantBalance::OutdoorCO2;
		using DataContaminantBalance::ZoneAirGC;
		using DataContaminantBalance::ZoneAirGCTemp;
		using DataContaminantBalance::ZoneAirGCAvg;
		using DataContaminantBalance::OutdoorGC;
		using ScheduleManager::GetCurrentScheduleValue;
		using ManageElectricPower::ManageElectricLoadCenters;
		using InternalHeatGains::UpdateInternalGainValues;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt const EndOfHeaderFormat( "('End of Data Dictionary')" ); // End of data dictionary marker
		static gio::Fmt const EnvironmentStampFormat( "(a,',',a,3(',',f7.2),',',f7.2)" ); // Format descriptor for environ stamp

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 PriorTimeStep; // magnitude of time step for previous history terms
		Real64 ZoneTempChange( 0.0 ); // change in zone air temperature from timestep t-1 to t
		int NodeNum;
		bool ReportDebug;
		static bool TriggerGetAFN( true );
		int ZoneNum;
		static bool PrintedWarmup( false );

		static bool MyEnvrnFlag( true );
		static bool InitVentReportFlag( true );
		static bool DebugNamesReported( false );

		static int ZTempTrendsNumSysSteps( 0 );
		static int SysTimestepLoop( 0 );
		bool DummyLogical;

		// Formats
		static gio::Fmt const Format_10( "('node #   Temp   MassMinAv  MassMaxAv TempSP      MassFlow       MassMin       ','MassMax        MassSP    Press        Enthal     HumRat Fluid Type')" );
		static gio::Fmt const Format_11( "('node #   Name')" );
		static gio::Fmt const Format_20( "(1x,I3,1x,F8.2,2(2x,F8.3),2x,F8.2,4(1x,F13.2),2x,F8.0,2x,F11.2,2x,F9.5,2x,A)" );
		static gio::Fmt const Format_30( "(1x,I3,5x,A)" );

		//SYSTEM INITIALIZATION
		if ( TriggerGetAFN ) {
			TriggerGetAFN = false;
			DisplayString( "Initializing HVAC" );
			ManageAirflowNetworkBalance(); // first call only gets input and returns.
		}

		ZT = MAT;
		// save for use with thermal comfort control models (Fang, Pierce, and KSU)
		ZTAVComf = ZTAV;
		ZoneAirHumRatAvgComf = ZoneAirHumRatAvg;
		ZTAV = 0.0;
		ZoneAirHumRatAvg = 0.0;
		PrintedWarmup = false;
		if ( Contaminant.CO2Simulation ) {
			OutdoorCO2 = GetCurrentScheduleValue( Contaminant.CO2OutdoorSchedPtr );
			ZoneAirCO2Avg = 0.0;
		}
		if ( Contaminant.GenericContamSimulation ) {
			OutdoorGC = GetCurrentScheduleValue( Contaminant.GenericContamOutdoorSchedPtr );
			if ( allocated( ZoneAirGCAvg ) ) ZoneAirGCAvg = 0.0;
		}

		if ( BeginEnvrnFlag && MyEnvrnFlag ) {
			ResetNodeData();
			AirLoopsSimOnce = false;
			MyEnvrnFlag = false;
			InitVentReportFlag = true;
			NumOfSysTimeStepsLastZoneTimeStep = 1;
			PreviousTimeStep = TimeStepZone;
		}
		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag = true;
		}

		SysTimeElapsed = 0.0;
		TimeStepSys = TimeStepZone;
		FirstTimeStepSysFlag = true;
		ShortenTimeStepSys = false;
		UseZoneTimeStepHistory = true;
		PriorTimeStep = TimeStepZone;
		NumOfSysTimeSteps = 1;
		FracTimeStepZone = TimeStepSys / TimeStepZone;

		ManageEMS( emsCallFromBeginTimestepBeforePredictor ); //calling point

		SetOutAirNodes();

		ManageRefrigeratedCaseRacks();

		//ZONE INITIALIZATION  'Get Zone Setpoints'
		ManageZoneAirUpdates( iGetZoneSetPoints, ZoneTempChange, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep );
		if ( Contaminant.SimulateContaminants ) ManageZoneContaminanUpdates( iGetZoneSetPoints, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep );

		ManageHybridVentilation();

		CalcAirFlowSimple();
		if ( SimulateAirflowNetwork > AirflowNetworkControlSimple ) {
			RollBackFlag = false;
			ManageAirflowNetworkBalance( false );
		}

		SetHeatToReturnAirFlag();

		SysDepZoneLoadsLagged = SysDepZoneLoads;

		UpdateInternalGainValues( true, true );

		ManageZoneAirUpdates( iPredictStep, ZoneTempChange, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep );

		if ( Contaminant.SimulateContaminants ) ManageZoneContaminanUpdates( iPredictStep, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep );

		SimHVAC();

		if ( AnyIdealCondEntSetPointInModel && MetersHaveBeenInitialized && ! WarmupFlag ) {
			RunOptCondEntTemp = true;
			while ( RunOptCondEntTemp ) {
				SimHVAC();
			}
		}

		ManageWaterInits();

		// Only simulate once per zone timestep; must be after SimHVAC
		if ( FirstTimeStepSysFlag && MetersHaveBeenInitialized ) {
			ManageDemand();
		}

		BeginTimeStepFlag = false; // At this point, we have been through the first pass through SimHVAC so this needs to be set

		ManageZoneAirUpdates( iCorrectStep, ZoneTempChange, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep );
		if ( Contaminant.SimulateContaminants ) ManageZoneContaminanUpdates( iCorrectStep, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep );

		if ( ZoneTempChange > MaxZoneTempDiff && ! KickOffSimulation ) {
			//determine value of adaptive system time step
			// model how many system timesteps we want in zone timestep
			ZTempTrendsNumSysSteps = int( ZoneTempChange / MaxZoneTempDiff + 1.0 ); // add 1 for truncation
			NumOfSysTimeSteps = min( ZTempTrendsNumSysSteps, LimitNumSysSteps );
			//then determine timestep length for even distribution, protect div by zero
			if ( NumOfSysTimeSteps > 0 ) TimeStepSys = TimeStepZone / NumOfSysTimeSteps;
			TimeStepSys = max( TimeStepSys, MinTimeStepSys );
			UseZoneTimeStepHistory = false;
			ShortenTimeStepSys = true;
		} else {
			NumOfSysTimeSteps = 1;
			UseZoneTimeStepHistory = true;
		}

		if ( UseZoneTimeStepHistory ) PreviousTimeStep = TimeStepZone;
		for ( SysTimestepLoop = 1; SysTimestepLoop <= NumOfSysTimeSteps; ++SysTimestepLoop ) {

			if ( TimeStepSys < TimeStepZone ) {

				ManageHybridVentilation();
				CalcAirFlowSimple( SysTimestepLoop );
				if ( SimulateAirflowNetwork > AirflowNetworkControlSimple ) {
					RollBackFlag = false;
					ManageAirflowNetworkBalance( false );
				}

				UpdateInternalGainValues( true, true );

				ManageZoneAirUpdates( iPredictStep, ZoneTempChange, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep );

				if ( Contaminant.SimulateContaminants ) ManageZoneContaminanUpdates( iPredictStep, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep );
				SimHVAC();

				if ( AnyIdealCondEntSetPointInModel && MetersHaveBeenInitialized && ! WarmupFlag ) {
					RunOptCondEntTemp = true;
					while ( RunOptCondEntTemp ) {
						SimHVAC();
					}
				}

				ManageWaterInits();

				//Need to set the flag back since we do not need to shift the temps back again in the correct step.
				ShortenTimeStepSys = false;

				ManageZoneAirUpdates( iCorrectStep, ZoneTempChange, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep );
				if ( Contaminant.SimulateContaminants ) ManageZoneContaminanUpdates( iCorrectStep, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep );

				ManageZoneAirUpdates( iPushSystemTimestepHistories, ZoneTempChange, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep );
				if ( Contaminant.SimulateContaminants ) ManageZoneContaminanUpdates( iPushSystemTimestepHistories, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep );
				PreviousTimeStep = TimeStepSys;
			}

			FracTimeStepZone = TimeStepSys / TimeStepZone;

			for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
				ZTAV( ZoneNum ) += ZT( ZoneNum ) * FracTimeStepZone;
				ZoneAirHumRatAvg( ZoneNum ) += ZoneAirHumRat( ZoneNum ) * FracTimeStepZone;
				if ( Contaminant.CO2Simulation ) ZoneAirCO2Avg( ZoneNum ) += ZoneAirCO2( ZoneNum ) * FracTimeStepZone;
				if ( Contaminant.GenericContamSimulation ) ZoneAirGCAvg( ZoneNum ) += ZoneAirGC( ZoneNum ) * FracTimeStepZone;
			}

			DetectOscillatingZoneTemp();
			UpdateZoneListAndGroupLoads(); // Must be called before UpdateDataandReport(HVACTSReporting)
			UpdateIceFractions(); // Update fraction of ice stored in TES
			ManageWater();
			// update electricity data for net, purchased, sold etc.
			DummyLogical = false;
			ManageElectricLoadCenters( false, DummyLogical, true );
			// Update the plant and condenser loop capacitance model temperature history.
			UpdateNodeThermalHistory();

			ManageEMS( emsCallFromEndSystemTimestepBeforeHVACReporting ); // EMS calling point

			// This is where output processor data is updated for System Timestep reporting
			if ( ! WarmupFlag ) {
				if ( DoOutputReporting ) {
					CalcMoreNodeInfo();
					CalculatePollution();
					InitEnergyReports();
					ReportSystemEnergyUse();
				}
				if ( DoOutputReporting || ( ZoneSizingCalc && CompLoadReportIsReq ) ) {
					ReportAirHeatBalance();
					if ( ZoneSizingCalc ) GatherComponentLoadsHVAC();
				}
				if ( DoOutputReporting ) {
					ReportMaxVentilationLoads();
					UpdateDataandReport( HVACTSReporting );
					UpdateTabularReports( HVACTSReporting );
				}
				if ( ZoneSizingCalc ) {
					UpdateZoneSizing( DuringDay );
				}
			} else if ( ! KickOffSimulation && DoOutputReporting && ReportDuringWarmup ) {
				if ( BeginDayFlag && ! PrintEnvrnStampWarmupPrinted ) {
					PrintEnvrnStampWarmup = true;
					PrintEnvrnStampWarmupPrinted = true;
				}
				if ( ! BeginDayFlag ) PrintEnvrnStampWarmupPrinted = false;
				if ( PrintEnvrnStampWarmup ) {
					if ( PrintEndDataDictionary && DoOutputReporting && ! PrintedWarmup ) {
						gio::write( OutputFileStandard, EndOfHeaderFormat );
						gio::write( OutputFileMeters, EndOfHeaderFormat );
						PrintEndDataDictionary = false;
					}
					if ( DoOutputReporting && ! PrintedWarmup ) {
						gio::write( OutputFileStandard, EnvironmentStampFormat ) << "1" << "Warmup {" + cWarmupDay + "} " + EnvironmentName << Latitude << Longitude << TimeZoneNumber << Elevation;
						gio::write( OutputFileMeters, EnvironmentStampFormat ) << "1" << "Warmup {" + cWarmupDay + "} " + EnvironmentName << Latitude << Longitude << TimeZoneNumber << Elevation;
						PrintEnvrnStampWarmup = false;
					}
					PrintedWarmup = true;
				}
				CalcMoreNodeInfo();
				UpdateDataandReport( HVACTSReporting );
			} else if ( UpdateDataDuringWarmupExternalInterface ) { // added for FMI
				if ( BeginDayFlag && ! PrintEnvrnStampWarmupPrinted ) {
					PrintEnvrnStampWarmup = true;
					PrintEnvrnStampWarmupPrinted = true;
				}
				if ( ! BeginDayFlag ) PrintEnvrnStampWarmupPrinted = false;
				if ( PrintEnvrnStampWarmup ) {
					if ( PrintEndDataDictionary && DoOutputReporting && ! PrintedWarmup ) {
						gio::write( OutputFileStandard, EndOfHeaderFormat );
						gio::write( OutputFileMeters, EndOfHeaderFormat );
						PrintEndDataDictionary = false;
					}
					if ( DoOutputReporting && ! PrintedWarmup ) {
						gio::write( OutputFileStandard, EnvironmentStampFormat ) << "1" << "Warmup {" + cWarmupDay + "} " + EnvironmentName << Latitude << Longitude << TimeZoneNumber << Elevation;
						gio::write( OutputFileMeters, EnvironmentStampFormat ) << "1" << "Warmup {" + cWarmupDay + "} " + EnvironmentName << Latitude << Longitude << TimeZoneNumber << Elevation;
						PrintEnvrnStampWarmup = false;
					}
					PrintedWarmup = true;
				}
				UpdateDataandReport( HVACTSReporting );
			}
			ManageEMS( emsCallFromEndSystemTimestepAfterHVACReporting ); // EMS calling point
			//UPDATE SYSTEM CLOCKS
			SysTimeElapsed += TimeStepSys;

			FirstTimeStepSysFlag = false;
		} //system time step  loop (loops once if no downstepping)

		ManageZoneAirUpdates( iPushZoneTimestepHistories, ZoneTempChange, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep );
		if ( Contaminant.SimulateContaminants ) ManageZoneContaminanUpdates( iPushZoneTimestepHistories, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep );

		NumOfSysTimeStepsLastZoneTimeStep = NumOfSysTimeSteps;

		UpdateDemandManagers();

		// DO FINAL UPDATE OF RECORD KEEPING VARIABLES
		// Report the Node Data to Aid in Debugging
		if ( DebugOutput ) {
			if ( EvenDuringWarmup ) {
				ReportDebug = true;
			} else {
				ReportDebug = ! WarmupFlag;
			}
			if ( ( ReportDebug ) && ( DayOfSim > 0 ) ) { // Report the node data
				if ( size( Node ) > 0 && ! DebugNamesReported ) {
					gio::write( OutputFileDebug, Format_11 );
					for ( NodeNum = 1; NodeNum <= isize( Node ); ++NodeNum ) {
						gio::write( OutputFileDebug, Format_30 ) << NodeNum << NodeID( NodeNum );
					}
					DebugNamesReported = true;
				}
				if ( size( Node ) > 0 ) {
					gio::write( OutputFileDebug, "*" );
					gio::write( OutputFileDebug, "*" );
					gio::write( OutputFileDebug, "*" ) << "Day of Sim     Hour of Day    Time";
					gio::write( OutputFileDebug, "*" ) << DayOfSim << HourOfDay << TimeStep * TimeStepZone;
					gio::write( OutputFileDebug, Format_10 );
				}
				for ( NodeNum = 1; NodeNum <= isize( Node ); ++NodeNum ) {
					gio::write( OutputFileDebug, Format_20 ) << NodeNum << Node( NodeNum ).Temp << Node( NodeNum ).MassFlowRateMinAvail << Node( NodeNum ).MassFlowRateMaxAvail << Node( NodeNum ).TempSetPoint << Node( NodeNum ).MassFlowRate << Node( NodeNum ).MassFlowRateMin << Node( NodeNum ).MassFlowRateMax << Node( NodeNum ).MassFlowRateSetPoint << Node( NodeNum ).Press << Node( NodeNum ).Enthalpy << Node( NodeNum ).HumRat << ValidNodeFluidTypes( Node( NodeNum ).FluidType );
				}
			}
		}

	}

	void
	SimHVAC()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Dan Fisher
		//       DATE WRITTEN:    April 1997
		//       DATE MODIFIED:   May 1998 (RKS,RDT)

		// PURPOSE OF THIS SUBROUTINE: Selects and calls the HVAC loop managers

		// METHODOLOGY EMPLOYED: Each loop manager is called or passed over
		// in succession based on the logical flags associated with the manager.
		// The logical flags are set in the manager routines and passed
		// as parameters to this routine.  Each loop manager potentially
		// affects a different set of other loop managers.

		// Future development could involve specifying any number of user
		// selectable control schemes based on the logical flags used in
		// this default control algorithm.

		// REFERENCES: none

		// Using/Aliasing
		using namespace DataConvergParams;
		using SetPointManager::ManageSetPoints;
		using SystemAvailabilityManager::ManageSystemAvailability;
		using ZoneEquipmentManager::ManageZoneEquipment;
		using NonZoneEquipmentManager::ManageNonZoneEquipment;
		using ManageElectricPower::ManageElectricLoadCenters;
		using DataEnvironment::EnvironmentName;
		using DataEnvironment::CurMnDy;
		using General::CreateSysTimeIntervalString;
		using General::RoundSigDigits;
		using EMSManager::ManageEMS;
		using PlantManager::GetPlantLoopData;
		using PlantManager::GetPlantInput;
		using PlantManager::SetupReports;
		using PlantManager::ManagePlantLoops;
		using PlantManager::SetupInitialPlantCallingOrder;
		using PlantManager::SetupBranchControlTypes;
		using PlantManager::ReInitPlantLoopsAtFirstHVACIteration;
		using PlantManager::InitOneTimePlantSizingInfo;
		using PlantCondLoopOperation::SetupPlantEMSActuators;
		using SimAirServingZones::ManageAirLoops;
		using DataPlant::SetAllPlantSimFlagsToValue;
		using DataPlant::TotNumLoops;
		using DataPlant::PlantManageSubIterations;
		using DataPlant::PlantManageHalfLoopCalls;
		using DataPlant::DemandSide;
		using DataPlant::SupplySide;
		using DataPlant::PlantLoop;
		using DataPlant::NumConvergenceHistoryTerms;
		using DataPlant::ConvergenceHistoryARR;
		using PlantUtilities::CheckPlantMixerSplitterConsistency;
		using PlantUtilities::CheckForRunawayPlantTemps;
		using PlantUtilities::AnyPlantSplitterMixerLacksContinuity;
		using DataGlobals::AnyPlantInModel;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		bool const IsPlantLoop( true );
		bool const NotPlantLoop( false );
		bool const SimWithPlantFlowUnlocked( false );
		bool const SimWithPlantFlowLocked( true );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool FirstHVACIteration; // True when solution technique on first iteration

		static bool IterSetup( false ); // Set to TRUE after the variable is setup for Output Reporting
		static int ErrCount( 0 ); // Number of times that the maximum iterations was exceeded
		static bool MySetPointInit( true );
		std::string CharErrOut; // a character string equivalent of ErrCount
		int que;
		static int MaxErrCount( 0 );
		static std::string ErrEnvironmentName;
		int LoopNum;
		int LoopSide;
		int ThisLoopSide;

		int AirSysNum;
		int StackDepth;
		std::string HistoryStack;
		std::string HistoryTrace;
		int ZoneInSysIndex;
		Real64 SlopeHumRat;
		Real64 SlopeMdot;
		Real64 SlopeTemps;
		Real64 AvgValue;
		bool FoundOscillationByDuplicate;
		int ZoneNum;
		int NodeIndex;
		bool MonotonicIncreaseFound;
		bool MonotonicDecreaseFound;

		// Initialize all of the simulation flags to true for the first iteration
		SimZoneEquipmentFlag = true;
		SimNonZoneEquipmentFlag = true;
		SimAirLoopsFlag = true;
		SimPlantLoopsFlag = true;
		SimElecCircuitsFlag = true;
		FirstHVACIteration = true;

		if ( AirLoopInputsFilled ) {
			// Reset air loop control info for cooling coil active flag (used in TU's for reheat air flow control)
			AirLoopControlInfo.CoolingActiveFlag() = false;
			// Reset air loop control info for heating coil active flag (used in OA controller for HX control)
			AirLoopControlInfo.HeatingActiveFlag() = false;
			// reset outside air system HX to off first time through
			AirLoopControlInfo.HeatRecoveryBypass() = true;
			// set HX check status flag to check for custom control in MixedAir.f90
			AirLoopControlInfo.CheckHeatRecoveryBypassStatus() = true;
			// set OA comp simulated flag to false
			AirLoopControlInfo.OASysComponentsSimulated() = false;
			// set economizer flow locked flag to false, will reset if custom HX control is used
			AirLoopControlInfo.EconomizerFlowLocked() = false;
			// set air loop resim flags for when heat recovery is used and air loop needs another iteration
			AirLoopControlInfo.HeatRecoveryResimFlag() = true;
			AirLoopControlInfo.HeatRecoveryResimFlag2() = false;
			AirLoopControlInfo.ResimAirLoopFlag() = false;
		}

		// This setups the reports for the Iteration variable that limits how many times
		//  it goes through all of the HVAC managers before moving on.
		// The plant loop 'get inputs' and initialization are also done here in order to allow plant loop connected components
		// simulated by managers other than the plant manager to run correctly.
		HVACManageIteration = 0;
		PlantManageSubIterations = 0;
		PlantManageHalfLoopCalls = 0;
		SetAllPlantSimFlagsToValue( true );
		if ( ! IterSetup ) {
			SetupOutputVariable( "HVAC System Solver Iteration Count []", HVACManageIteration, "HVAC", "Sum", "SimHVAC" );
			SetupOutputVariable( "Air System Solver Iteration Count []", RepIterAir, "HVAC", "Sum", "SimHVAC" );
			ManageSetPoints(); //need to call this before getting plant loop data so setpoint checks can complete okay
			GetPlantLoopData();
			GetPlantInput();
			SetupInitialPlantCallingOrder();
			SetupBranchControlTypes(); //new routine to do away with input for branch control type
			//    CALL CheckPlantLoopData
			SetupReports();
			if ( AnyEnergyManagementSystemInModel ) {
				SetupPlantEMSActuators();
			}

			if ( TotNumLoops > 0 ) {
				SetupOutputVariable( "Plant Solver Sub Iteration Count []", PlantManageSubIterations, "HVAC", "Sum", "SimHVAC" );
				SetupOutputVariable( "Plant Solver Half Loop Calls Count []", PlantManageHalfLoopCalls, "HVAC", "Sum", "SimHVAC" );
				for ( LoopNum = 1; LoopNum <= TotNumLoops; ++LoopNum ) {
					// init plant sizing numbers in main plant data structure
					InitOneTimePlantSizingInfo( LoopNum );
				}
			}
			IterSetup = true;
		}

		if ( ZoneSizingCalc ) {
			ManageZoneEquipment( FirstHVACIteration, SimZoneEquipmentFlag, SimAirLoopsFlag );
			// need to call non zone equipment so water use zone gains can be included in sizing calcs
			ManageNonZoneEquipment( FirstHVACIteration, SimNonZoneEquipmentFlag );
			ManageElectricLoadCenters( FirstHVACIteration, SimElecCircuitsFlag, false );
			return;
		}

		// Before the HVAC simulation, reset control flags and specified flow
		// rates that might have been set by the set point and availability
		// managers.

		ResetHVACControl();

		// Before the HVAC simulation, call ManageSetPoints to set all the HVAC
		// node setpoints

		ManageEMS( emsCallFromBeforeHVACManagers ); // calling point

		ManageSetPoints();

		// re-initialize plant loop and nodes.
		ReInitPlantLoopsAtFirstHVACIteration();

		// Before the HVAC simulation, call ManageSystemAvailability to set
		// the system on/off flags
		ManageSystemAvailability();

		ManageEMS( emsCallFromAfterHVACManagers ); // calling point

		// first explicitly call each system type with FirstHVACIteration,

		// Manages the various component simulations
		SimSelectedEquipment( SimAirLoopsFlag, SimZoneEquipmentFlag, SimNonZoneEquipmentFlag, SimPlantLoopsFlag, SimElecCircuitsFlag, FirstHVACIteration, SimWithPlantFlowUnlocked );

		// Eventually, when all of the flags are set to false, the
		// simulation has converged for this system time step.

		SimPlantLoopsFlag = true;
		SetAllPlantSimFlagsToValue( true ); //set so loop to simulate at least once on non-first hvac

		FirstHVACIteration = false;

		// then iterate among all systems after first HVAC iteration is over

		// Main iteration loop for HVAC.  If any of the simulation flags are
		// true, then specific components must be resimulated.
		while ( ( SimAirLoopsFlag || SimZoneEquipmentFlag || SimNonZoneEquipmentFlag || SimPlantLoopsFlag || SimElecCircuitsFlag ) && ( HVACManageIteration <= MaxIter ) ) {

			ManageEMS( emsCallFromHVACIterationLoop ); // calling point id

			// Manages the various component simulations
			SimSelectedEquipment( SimAirLoopsFlag, SimZoneEquipmentFlag, SimNonZoneEquipmentFlag, SimPlantLoopsFlag, SimElecCircuitsFlag, FirstHVACIteration, SimWithPlantFlowUnlocked );

			// Eventually, when all of the flags are set to false, the
			// simulation has converged for this system time step.

			UpdateZoneInletConvergenceLog();

			++HVACManageIteration; // Increment the iteration counter

		}
		if ( AnyPlantInModel ) {
			if ( AnyPlantSplitterMixerLacksContinuity() ) {
				// rerun systems in a "Final flow lock/last iteration" mode
				// now call for one second to last plant simulation
				SimAirLoopsFlag = false;
				SimZoneEquipmentFlag = false;
				SimNonZoneEquipmentFlag = false;
				SimPlantLoopsFlag = true;
				SimElecCircuitsFlag = false;
				SimSelectedEquipment( SimAirLoopsFlag, SimZoneEquipmentFlag, SimNonZoneEquipmentFlag, SimPlantLoopsFlag, SimElecCircuitsFlag, FirstHVACIteration, SimWithPlantFlowUnlocked );
				// now call for all non-plant simulation, but with plant flow lock on
				SimAirLoopsFlag = true;
				SimZoneEquipmentFlag = true;
				SimNonZoneEquipmentFlag = true;
				SimPlantLoopsFlag = false;
				SimElecCircuitsFlag = true;
				SimSelectedEquipment( SimAirLoopsFlag, SimZoneEquipmentFlag, SimNonZoneEquipmentFlag, SimPlantLoopsFlag, SimElecCircuitsFlag, FirstHVACIteration, SimWithPlantFlowLocked );
				UpdateZoneInletConvergenceLog();
				// now call for a last plant simulation
				SimAirLoopsFlag = false;
				SimZoneEquipmentFlag = false;
				SimNonZoneEquipmentFlag = false;
				SimPlantLoopsFlag = true;
				SimElecCircuitsFlag = false;
				SimSelectedEquipment( SimAirLoopsFlag, SimZoneEquipmentFlag, SimNonZoneEquipmentFlag, SimPlantLoopsFlag, SimElecCircuitsFlag, FirstHVACIteration, SimWithPlantFlowUnlocked );
				// now call for a last all non-plant simulation, but with plant flow lock on
				SimAirLoopsFlag = true;
				SimZoneEquipmentFlag = true;
				SimNonZoneEquipmentFlag = true;
				SimPlantLoopsFlag = false;
				SimElecCircuitsFlag = true;
				SimSelectedEquipment( SimAirLoopsFlag, SimZoneEquipmentFlag, SimNonZoneEquipmentFlag, SimPlantLoopsFlag, SimElecCircuitsFlag, FirstHVACIteration, SimWithPlantFlowLocked );
				UpdateZoneInletConvergenceLog();
			}
		}

		//DSU  Test plant loop for errors
		for ( LoopNum = 1; LoopNum <= TotNumLoops; ++LoopNum ) {
			for ( LoopSide = DemandSide; LoopSide <= SupplySide; ++LoopSide ) {
				CheckPlantMixerSplitterConsistency( LoopNum, LoopSide, 1, 1, FirstHVACIteration );
				CheckForRunawayPlantTemps( LoopNum, LoopSide );
			}
		}

		if ( ( HVACManageIteration > MaxIter ) && ( ! WarmupFlag ) ) {
			++ErrCount;
			if ( ErrCount < 15 ) {
				ErrEnvironmentName = EnvironmentName;
				gio::write( CharErrOut, "(I5)" ) << MaxIter;
				strip( CharErrOut );
				ShowWarningError( "SimHVAC: Maximum iterations (" + CharErrOut + ") exceeded for all HVAC loops, at " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString() );
				if ( SimAirLoopsFlag ) {
					ShowContinueError( "The solution for one or more of the Air Loop HVAC systems did not appear to converge" );
				}
				if ( SimZoneEquipmentFlag ) {
					ShowContinueError( "The solution for zone HVAC equipment did not appear to converge" );
				}
				if ( SimNonZoneEquipmentFlag ) {
					ShowContinueError( "The solution for non-zone equipment did not appear to converge" );
				}
				if ( SimPlantLoopsFlag ) {
					ShowContinueError( "The solution for one or more plant systems did not appear to converge" );
				}
				if ( SimElecCircuitsFlag ) {
					ShowContinueError( "The solution for on-site electric generators did not appear to converge" );
				}
				if ( ErrCount == 1 && ! DisplayExtraWarnings ) {
					ShowContinueError( "...use Output:Diagnostics,DisplayExtraWarnings; " "  to show more details on each max iteration exceeded." );
				}
				if ( DisplayExtraWarnings ) {

					for ( AirSysNum = 1; AirSysNum <= NumPrimaryAirSys; ++AirSysNum ) {

						if ( AirLoopConvergence( AirSysNum ).HVACMassFlowNotConverged ) {

							ShowContinueError( "Air System Named = " + AirToZoneNodeInfo( AirSysNum ).AirLoopName + " did not converge for mass flow rate" );
							ShowContinueError( "Check values should be zero. Most Recent values listed first." );
							HistoryTrace = "";
							for ( StackDepth = 1; StackDepth <= ConvergLogStackDepth; ++StackDepth ) {
								HistoryTrace += RoundSigDigits( AirLoopConvergence( AirSysNum ).HVACFlowDemandToSupplyTolValue( StackDepth ), 6 ) + ',';
							}

							ShowContinueError( "Demand-to-Supply interface mass flow rate check value iteration history trace: " + HistoryTrace );
							HistoryTrace = "";
							for ( StackDepth = 1; StackDepth <= ConvergLogStackDepth; ++StackDepth ) {
								HistoryTrace += RoundSigDigits( AirLoopConvergence( AirSysNum ).HVACFlowSupplyDeck1ToDemandTolValue( StackDepth ), 6 ) + ',';
							}
							ShowContinueError( "Supply-to-demand interface deck 1 mass flow rate check value iteration history trace: " + HistoryTrace );

							if ( AirToZoneNodeInfo( AirSysNum ).NumSupplyNodes >= 2 ) {
								HistoryTrace = "";
								for ( StackDepth = 1; StackDepth <= ConvergLogStackDepth; ++StackDepth ) {
									HistoryTrace += RoundSigDigits( AirLoopConvergence( AirSysNum ).HVACFlowSupplyDeck2ToDemandTolValue( StackDepth ), 6 ) + ',';
								}
								ShowContinueError( "Supply-to-demand interface deck 2 mass flow rate check value iteration history trace: " + HistoryTrace );
							}
						} // mass flow rate not converged

						if ( AirLoopConvergence( AirSysNum ).HVACHumRatNotConverged ) {

							ShowContinueError( "Air System Named = " + AirToZoneNodeInfo( AirSysNum ).AirLoopName + " did not converge for humidity ratio" );
							ShowContinueError( "Check values should be zero. Most Recent values listed first." );
							HistoryTrace = "";
							for ( StackDepth = 1; StackDepth <= ConvergLogStackDepth; ++StackDepth ) {
								HistoryTrace += RoundSigDigits( AirLoopConvergence( AirSysNum ).HVACHumDemandToSupplyTolValue( StackDepth ), 6 ) + ',';
							}
							ShowContinueError( "Demand-to-Supply interface humidity ratio check value iteration history trace: " + HistoryTrace );
							HistoryTrace = "";
							for ( StackDepth = 1; StackDepth <= ConvergLogStackDepth; ++StackDepth ) {
								HistoryTrace += RoundSigDigits( AirLoopConvergence( AirSysNum ).HVACHumSupplyDeck1ToDemandTolValue( StackDepth ), 6 ) + ',';
							}
							ShowContinueError( "Supply-to-demand interface deck 1 humidity ratio check value iteration history trace: " + HistoryTrace );

							if ( AirToZoneNodeInfo( AirSysNum ).NumSupplyNodes >= 2 ) {
								HistoryTrace = "";
								for ( StackDepth = 1; StackDepth <= ConvergLogStackDepth; ++StackDepth ) {
									HistoryTrace += RoundSigDigits( AirLoopConvergence( AirSysNum ).HVACHumSupplyDeck2ToDemandTolValue( StackDepth ), 6 ) + ',';
								}
								ShowContinueError( "Supply-to-demand interface deck 2 humidity ratio check value iteration history trace: " + HistoryTrace );
							}
						} // humidity ratio not converged

						if ( AirLoopConvergence( AirSysNum ).HVACTempNotConverged ) {

							ShowContinueError( "Air System Named = " + AirToZoneNodeInfo( AirSysNum ).AirLoopName + " did not converge for temperature" );
							ShowContinueError( "Check values should be zero. Most Recent values listed first." );
							HistoryTrace = "";
							for ( StackDepth = 1; StackDepth <= ConvergLogStackDepth; ++StackDepth ) {
								HistoryTrace += RoundSigDigits( AirLoopConvergence( AirSysNum ).HVACTempDemandToSupplyTolValue( StackDepth ), 6 ) + ',';
							}
							ShowContinueError( "Demand-to-Supply interface temperature check value iteration history trace: " + HistoryTrace );
							HistoryTrace = "";
							for ( StackDepth = 1; StackDepth <= ConvergLogStackDepth; ++StackDepth ) {
								HistoryTrace += RoundSigDigits( AirLoopConvergence( AirSysNum ).HVACTempSupplyDeck1ToDemandTolValue( StackDepth ), 6 ) + ',';
							}
							ShowContinueError( "Supply-to-demand interface deck 1 temperature check value iteration history trace: " + HistoryTrace );

							if ( AirToZoneNodeInfo( AirSysNum ).NumSupplyNodes >= 2 ) {
								HistoryTrace = "";
								for ( StackDepth = 1; StackDepth <= ConvergLogStackDepth; ++StackDepth ) {
									HistoryTrace += RoundSigDigits( AirLoopConvergence( AirSysNum ).HVACTempSupplyDeck1ToDemandTolValue( StackDepth ), 6 ) + ',';
								}
								ShowContinueError( "Supply-to-demand interface deck 2 temperature check value iteration history trace: " + HistoryTrace );
							}
						} // Temps not converged
						if ( AirLoopConvergence( AirSysNum ).HVACEnergyNotConverged ) {

							ShowContinueError( "Air System Named = " + AirToZoneNodeInfo( AirSysNum ).AirLoopName + " did not converge for energy" );
							ShowContinueError( "Check values should be zero. Most Recent values listed first." );
							HistoryTrace = "";
							for ( StackDepth = 1; StackDepth <= ConvergLogStackDepth; ++StackDepth ) {
								HistoryTrace += RoundSigDigits( AirLoopConvergence( AirSysNum ).HVACEnergyDemandToSupplyTolValue( StackDepth ), 6 ) + ',';
							}
							ShowContinueError( "Demand-to-Supply interface energy check value iteration history trace: " + HistoryTrace );
							HistoryTrace = "";
							for ( StackDepth = 1; StackDepth <= ConvergLogStackDepth; ++StackDepth ) {
								HistoryTrace += RoundSigDigits( AirLoopConvergence( AirSysNum ).HVACEnergySupplyDeck1ToDemandTolValue( StackDepth ), 6 ) + ',';
							}
							ShowContinueError( "Supply-to-demand interface deck 1 energy check value iteration history trace: " + HistoryTrace );

							if ( AirToZoneNodeInfo( AirSysNum ).NumSupplyNodes >= 2 ) {
								HistoryTrace = "";
								for ( StackDepth = 1; StackDepth <= ConvergLogStackDepth; ++StackDepth ) {
									HistoryTrace += RoundSigDigits( AirLoopConvergence( AirSysNum ).HVACEnergySupplyDeck2ToDemandTolValue( StackDepth ), 6 ) + ',';
								}
								ShowContinueError( "Supply-to-demand interface deck 2 energy check value iteration history trace: " + HistoryTrace );
							}
						} // energy not converged

					} // loop over air loop systems

					// loop over zones and check for issues with zone inlet nodes
					for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {

						for ( NodeIndex = 1; NodeIndex <= ZoneInletConvergence( ZoneNum ).NumInletNodes; ++NodeIndex ) {
							ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).NotConvergedHumRate = false;
							ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).NotConvergedMassFlow = false;
							ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).NotConvergedTemp = false;

							// Check humidity ratio
							FoundOscillationByDuplicate = false;
							MonotonicDecreaseFound = false;
							MonotonicIncreaseFound = false;
							// check for evidence of oscillation by indentify duplicates when latest value not equal to average
							AvgValue = sum( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).HumidityRatio ) / double( ConvergLogStackDepth );
							if ( std::abs( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).HumidityRatio( 1 ) - AvgValue ) > HVACHumRatOscillationToler ) { // last iterate differs from average
								FoundOscillationByDuplicate = false;
								for ( StackDepth = 2; StackDepth <= ConvergLogStackDepth; ++StackDepth ) {
									if ( std::abs( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).HumidityRatio( 1 ) - ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).HumidityRatio( StackDepth ) ) < HVACHumRatOscillationToler ) {
										FoundOscillationByDuplicate = true;
										ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).NotConvergedHumRate = true;
										ShowContinueError( "Node named " + NodeID( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).NodeNum ) + " shows oscillating humidity ratio across iterations with a repeated value of " + RoundSigDigits( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).HumidityRatio( 1 ), 6 ) );
										break;
									}
								}
								if ( ! FoundOscillationByDuplicate ) {
									SlopeHumRat = ( sum( ConvergLogStackARR ) * sum( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).HumidityRatio ) - double( ConvergLogStackDepth ) * sum( ( ConvergLogStackARR * ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).HumidityRatio ) ) ) / ( std::pow( sum( ConvergLogStackARR ), 2 ) - double( ConvergLogStackDepth ) * sum( pow( ConvergLogStackARR, 2 ) ) );
									if ( std::abs( SlopeHumRat ) > HVACHumRatSlopeToler ) {

										if ( SlopeHumRat < 0.0 ) { // check for monotic decrease
											MonotonicDecreaseFound = true;
											for ( StackDepth = 2; StackDepth <= ConvergLogStackDepth; ++StackDepth ) {
												if ( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).HumidityRatio( StackDepth - 1 ) > ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).HumidityRatio( StackDepth ) ) {
													MonotonicDecreaseFound = false;
													break;
												}
											}
											if ( MonotonicDecreaseFound ) {
												ShowContinueError( "Node named " + NodeID( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).NodeNum ) + " shows monotonically decreasing humidity ratio with a trend rate across iterations of " + RoundSigDigits( SlopeHumRat, 6 ) + " [ kg-water/kg-dryair/iteration]" );
											}
										} else { // check for monotic incrase
											MonotonicIncreaseFound = true;
											for ( StackDepth = 2; StackDepth <= ConvergLogStackDepth; ++StackDepth ) {
												if ( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).HumidityRatio( StackDepth - 1 ) < ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).HumidityRatio( StackDepth ) ) {
													MonotonicIncreaseFound = false;
													break;
												}
											}
											if ( MonotonicIncreaseFound ) {
												ShowContinueError( "Node named " + NodeID( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).NodeNum ) + " shows monotonically increasing humidity ratio with a trend rate across iterations of " + RoundSigDigits( SlopeHumRat, 6 ) + " [ kg-water/kg-dryair/iteration]" );
											}
										}
									} // significant slope in iterates
								} //no osciallation
							} // last value does not equal average of stack.

							if ( MonotonicDecreaseFound || MonotonicIncreaseFound || FoundOscillationByDuplicate ) {
								HistoryTrace = "";
								for ( StackDepth = 1; StackDepth <= ConvergLogStackDepth; ++StackDepth ) {
									HistoryTrace += RoundSigDigits( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).HumidityRatio( StackDepth ), 6 ) + ',';
								}
								ShowContinueError( "Node named " + NodeID( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).NodeNum ) + " humidity ratio [kg-water/kg-dryair] iteration history trace (most recent first): " + HistoryTrace );
							} // need to report trace
							// end humidity ratio

							// Check Mass flow rate
							FoundOscillationByDuplicate = false;
							MonotonicDecreaseFound = false;
							MonotonicIncreaseFound = false;
							// check for evidence of oscillation by indentify duplicates when latest value not equal to average
							AvgValue = sum( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).MassFlowRate ) / double( ConvergLogStackDepth );
							if ( std::abs( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).MassFlowRate( 1 ) - AvgValue ) > HVACFlowRateOscillationToler ) { // last iterate differs from average
								FoundOscillationByDuplicate = false;
								for ( StackDepth = 2; StackDepth <= ConvergLogStackDepth; ++StackDepth ) {
									if ( std::abs( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).MassFlowRate( 1 ) - ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).MassFlowRate( StackDepth ) ) < HVACFlowRateOscillationToler ) {
										FoundOscillationByDuplicate = true;
										ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).NotConvergedMassFlow = true;
										ShowContinueError( "Node named " + NodeID( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).NodeNum ) + " shows oscillating mass flow rate across iterations with a repeated value of " + RoundSigDigits( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).MassFlowRate( 1 ), 6 ) );
										break;
									}
								}
								if ( ! FoundOscillationByDuplicate ) {
									SlopeMdot = ( sum( ConvergLogStackARR ) * sum( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).MassFlowRate ) - double( ConvergLogStackDepth ) * sum( ( ConvergLogStackARR * ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).MassFlowRate ) ) ) / ( std::pow( sum( ConvergLogStackARR ), 2 ) - double( ConvergLogStackDepth ) * sum( pow( ConvergLogStackARR, 2 ) ) );
									if ( std::abs( SlopeMdot ) > HVACFlowRateSlopeToler ) {
										ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).NotConvergedMassFlow = true;
										if ( SlopeMdot < 0.0 ) { // check for monotic decrease
											MonotonicDecreaseFound = true;
											for ( StackDepth = 2; StackDepth <= ConvergLogStackDepth; ++StackDepth ) {
												if ( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).MassFlowRate( StackDepth - 1 ) > ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).MassFlowRate( StackDepth ) ) {
													MonotonicDecreaseFound = false;
													break;
												}
											}
											if ( MonotonicDecreaseFound ) {
												ShowContinueError( "Node named " + NodeID( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).NodeNum ) + " shows monotonically decreasing mass flow rate with a trend rate across iterations of " + RoundSigDigits( SlopeMdot, 6 ) + " [kg/s/iteration]" );
											}
										} else { // check for monotic incrase
											MonotonicIncreaseFound = true;
											for ( StackDepth = 2; StackDepth <= ConvergLogStackDepth; ++StackDepth ) {
												if ( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).MassFlowRate( StackDepth - 1 ) < ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).MassFlowRate( StackDepth ) ) {
													MonotonicIncreaseFound = false;
													break;
												}
											}
											if ( MonotonicIncreaseFound ) {
												ShowContinueError( "Node named " + NodeID( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).NodeNum ) + " shows monotonically increasing mass flow rate with a trend rate across iterations of " + RoundSigDigits( SlopeMdot, 6 ) + " [kg/s/iteration]" );
											}
										}
									} // significant slope in iterates
								} //no osciallation
							} // last value does not equal average of stack.

							if ( MonotonicDecreaseFound || MonotonicIncreaseFound || FoundOscillationByDuplicate ) {
								HistoryTrace = "";
								for ( StackDepth = 1; StackDepth <= ConvergLogStackDepth; ++StackDepth ) {
									HistoryTrace += RoundSigDigits( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).MassFlowRate( StackDepth ), 6 ) + ',';
								}
								ShowContinueError( "Node named " + NodeID( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).NodeNum ) + " mass flow rate [kg/s] iteration history trace (most recent first): " + HistoryTrace );
							} // need to report trace
							// end mass flow rate

							// Check Temperatures
							FoundOscillationByDuplicate = false;
							MonotonicDecreaseFound = false;
							MonotonicIncreaseFound = false;
							// check for evidence of oscillation by indentify duplicates when latest value not equal to average
							AvgValue = sum( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).Temperature ) / double( ConvergLogStackDepth );
							if ( std::abs( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).Temperature( 1 ) - AvgValue ) > HVACTemperatureOscillationToler ) { // last iterate differs from average
								FoundOscillationByDuplicate = false;
								for ( StackDepth = 2; StackDepth <= ConvergLogStackDepth; ++StackDepth ) {
									if ( std::abs( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).Temperature( 1 ) - ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).Temperature( StackDepth ) ) < HVACTemperatureOscillationToler ) {
										FoundOscillationByDuplicate = true;
										ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).NotConvergedTemp = true;
										ShowContinueError( "Node named " + NodeID( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).NodeNum ) + " shows oscillating temperatures across iterations with a repeated value of " + RoundSigDigits( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).Temperature( 1 ), 6 ) );
										break;
									}
								}
								if ( ! FoundOscillationByDuplicate ) {
									SlopeTemps = ( sum( ConvergLogStackARR ) * sum( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).Temperature ) - double( ConvergLogStackDepth ) * sum( ( ConvergLogStackARR * ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).Temperature ) ) ) / ( std::pow( sum( ConvergLogStackARR ), 2 ) - double( ConvergLogStackDepth ) * sum( pow( ConvergLogStackARR, 2 ) ) );
									if ( std::abs( SlopeTemps ) > HVACTemperatureSlopeToler ) {
										ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).NotConvergedTemp = true;
										if ( SlopeTemps < 0.0 ) { // check for monotic decrease
											MonotonicDecreaseFound = true;
											for ( StackDepth = 2; StackDepth <= ConvergLogStackDepth; ++StackDepth ) {
												if ( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).Temperature( StackDepth - 1 ) > ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).Temperature( StackDepth ) ) {
													MonotonicDecreaseFound = false;
													break;
												}
											}
											if ( MonotonicDecreaseFound ) {
												ShowContinueError( "Node named " + NodeID( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).NodeNum ) + " shows monotonically decreasing temperature with a trend rate across iterations of " + RoundSigDigits( SlopeTemps, 4 ) + " [C/iteration]" );
											}
										} else { // check for monotic incrase
											MonotonicIncreaseFound = true;
											for ( StackDepth = 2; StackDepth <= ConvergLogStackDepth; ++StackDepth ) {
												if ( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).Temperature( StackDepth - 1 ) < ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).Temperature( StackDepth ) ) {
													MonotonicIncreaseFound = false;
													break;
												}
											}
											if ( MonotonicIncreaseFound ) {
												ShowContinueError( "Node named " + NodeID( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).NodeNum ) + " shows monotonically increasing temperatures with a trend rate across iterations of " + RoundSigDigits( SlopeTemps, 4 ) + " [C/iteration]" );
											}
										}
									} // significant slope in iterates
								} //no osciallation
							} // last value does not equal average of stack.

							if ( MonotonicDecreaseFound || MonotonicIncreaseFound || FoundOscillationByDuplicate ) {
								HistoryTrace = "";
								for ( StackDepth = 1; StackDepth <= ConvergLogStackDepth; ++StackDepth ) {
									HistoryTrace += RoundSigDigits( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).Temperature( StackDepth ), 6 ) + ',';
								}
								ShowContinueError( "Node named " + NodeID( ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).NodeNum ) + " temperature [C] iteration history trace (most recent first): " + HistoryTrace );
							} // need to report trace
							// end Temperature checks

						} // loop over zone inlet nodes
					} // loop over zones

					for ( LoopNum = 1; LoopNum <= TotNumLoops; ++LoopNum ) {

						if ( PlantConvergence( LoopNum ).PlantMassFlowNotConverged ) {
							ShowContinueError( "Plant System Named = " + PlantLoop( LoopNum ).Name + " did not converge for mass flow rate" );
							ShowContinueError( "Check values should be zero. Most Recent values listed first." );
							HistoryTrace = "";
							for ( StackDepth = 1; StackDepth <= ConvergLogStackDepth; ++StackDepth ) {
								HistoryTrace += RoundSigDigits( PlantConvergence( LoopNum ).PlantFlowDemandToSupplyTolValue( StackDepth ), 6 ) + ',';
							}
							ShowContinueError( "Demand-to-Supply interface mass flow rate check value iteration history trace: " + HistoryTrace );
							HistoryTrace = "";
							for ( StackDepth = 1; StackDepth <= ConvergLogStackDepth; ++StackDepth ) {
								HistoryTrace += RoundSigDigits( PlantConvergence( LoopNum ).PlantFlowSupplyToDemandTolValue( StackDepth ), 6 ) + ',';
							}
							ShowContinueError( "Supply-to-Demand interface mass flow rate check value iteration history trace: " + HistoryTrace );

							// now work with history logs for mass flow to detect issues
							for ( ThisLoopSide = 1; ThisLoopSide <= isize( PlantLoop( LoopNum ).LoopSide ); ++ThisLoopSide ) {
								// loop side inlet node
								FoundOscillationByDuplicate = false;
								MonotonicDecreaseFound = false;
								MonotonicIncreaseFound = false;
								AvgValue = sum( PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).InletNode.MassFlowRateHistory ) / double( NumConvergenceHistoryTerms );
								if ( std::abs( PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).InletNode.MassFlowRateHistory( 1 ) - AvgValue ) > PlantFlowRateOscillationToler ) {
									FoundOscillationByDuplicate = false;
									for ( StackDepth = 2; StackDepth <= NumConvergenceHistoryTerms; ++StackDepth ) {
										if ( std::abs( PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).InletNode.MassFlowRateHistory( 1 ) - PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).InletNode.MassFlowRateHistory( StackDepth ) ) < PlantFlowRateOscillationToler ) {
											FoundOscillationByDuplicate = true;
											ShowContinueError( "Node named " + PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).NodeNameIn + " shows oscillating flow rates across iterations with a repeated value of " + RoundSigDigits( PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).InletNode.MassFlowRateHistory( 1 ), 7 ) );
											break;
										}
									}
								}
								if ( ! FoundOscillationByDuplicate ) {
									SlopeMdot = ( sum( ConvergenceHistoryARR ) * sum( PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).InletNode.MassFlowRateHistory ) - double( NumConvergenceHistoryTerms ) * sum( ( ConvergenceHistoryARR * PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).InletNode.MassFlowRateHistory ) ) ) / ( std::pow( sum( ConvergenceHistoryARR ), 2 ) - double( NumConvergenceHistoryTerms ) * sum( pow( ConvergenceHistoryARR, 2 ) ) );
									if ( std::abs( SlopeMdot ) > PlantFlowRateSlopeToler ) {
										if ( SlopeMdot < 0.0 ) { // check for monotonic decrease
											MonotonicDecreaseFound = true;
											for ( StackDepth = 2; StackDepth <= NumConvergenceHistoryTerms; ++StackDepth ) {
												if ( PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).InletNode.MassFlowRateHistory( StackDepth - 1 ) > PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).InletNode.MassFlowRateHistory( StackDepth ) ) {
													MonotonicDecreaseFound = false;
													break;
												}
											}
											if ( MonotonicDecreaseFound ) {
												ShowContinueError( "Node named " + PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).NodeNameIn + " shows monotonically decreasing mass flow rate with a trend rate across iterations of " + RoundSigDigits( SlopeMdot, 7 ) + " [kg/s/iteration]" );
											}
										} else { // check for monotonic incrase
											MonotonicIncreaseFound = true;
											for ( StackDepth = 2; StackDepth <= NumConvergenceHistoryTerms; ++StackDepth ) {
												if ( PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).InletNode.MassFlowRateHistory( StackDepth - 1 ) < PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).InletNode.MassFlowRateHistory( StackDepth ) ) {
													MonotonicIncreaseFound = false;
													break;
												}
											}
											if ( MonotonicIncreaseFound ) {
												ShowContinueError( "Node named " + PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).NodeNameIn + " shows monotonically increasing mass flow rate with a trend rate across iterations of " + RoundSigDigits( SlopeMdot, 7 ) + " [kg/s/iteration]" );
											}
										}
									} // significant slope found
								} // no oscillation found

								if ( MonotonicDecreaseFound || MonotonicIncreaseFound || FoundOscillationByDuplicate ) {
									HistoryTrace = "";
									for ( StackDepth = 1; StackDepth <= NumConvergenceHistoryTerms; ++StackDepth ) {
										HistoryTrace += RoundSigDigits( PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).InletNode.MassFlowRateHistory( StackDepth ), 7 ) + ',';
									}
									ShowContinueError( "Node named " + PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).NodeNameIn + " mass flow rate [kg/s] iteration history trace (most recent first): " + HistoryTrace );
								} // need to report trace
								// end of inlet node

								// loop side outlet node
								FoundOscillationByDuplicate = false;
								MonotonicDecreaseFound = false;
								MonotonicIncreaseFound = false;
								AvgValue = sum( PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).OutletNode.MassFlowRateHistory ) / double( NumConvergenceHistoryTerms );
								if ( std::abs( PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).OutletNode.MassFlowRateHistory( 1 ) - AvgValue ) > PlantFlowRateOscillationToler ) {
									FoundOscillationByDuplicate = false;
									for ( StackDepth = 2; StackDepth <= NumConvergenceHistoryTerms; ++StackDepth ) {
										if ( std::abs( PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).OutletNode.MassFlowRateHistory( 1 ) - PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).OutletNode.MassFlowRateHistory( StackDepth ) ) < PlantFlowRateOscillationToler ) {
											FoundOscillationByDuplicate = true;
											ShowContinueError( "Node named " + PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).NodeNameOut + " shows oscillating flow rates across iterations with a repeated value of " + RoundSigDigits( PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).OutletNode.MassFlowRateHistory( 1 ), 7 ) );
											break;
										}
									}
								}
								if ( ! FoundOscillationByDuplicate ) {
									SlopeMdot = ( sum( ConvergenceHistoryARR ) * sum( PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).OutletNode.MassFlowRateHistory ) - double( NumConvergenceHistoryTerms ) * sum( ( ConvergenceHistoryARR * PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).OutletNode.MassFlowRateHistory ) ) ) / ( std::pow( sum( ConvergenceHistoryARR ), 2 ) - double( NumConvergenceHistoryTerms ) * sum( pow( ConvergenceHistoryARR, 2 ) ) );
									if ( std::abs( SlopeMdot ) > PlantFlowRateSlopeToler ) {
										if ( SlopeMdot < 0.0 ) { // check for monotonic decrease
											MonotonicDecreaseFound = true;
											for ( StackDepth = 2; StackDepth <= NumConvergenceHistoryTerms; ++StackDepth ) {
												if ( PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).OutletNode.MassFlowRateHistory( StackDepth - 1 ) > PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).OutletNode.MassFlowRateHistory( StackDepth ) ) {
													MonotonicDecreaseFound = false;
													break;
												}
											}
											if ( MonotonicDecreaseFound ) {
												ShowContinueError( "Node named " + PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).NodeNameOut + " shows monotonically decreasing mass flow rate with a trend rate across iterations of " + RoundSigDigits( SlopeMdot, 7 ) + " [kg/s/iteration]" );
											}
										} else { // check for monotonic incrase
											MonotonicIncreaseFound = true;
											for ( StackDepth = 2; StackDepth <= NumConvergenceHistoryTerms; ++StackDepth ) {
												if ( PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).OutletNode.MassFlowRateHistory( StackDepth - 1 ) < PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).OutletNode.MassFlowRateHistory( StackDepth ) ) {
													MonotonicIncreaseFound = false;
													break;
												}
											}
											if ( MonotonicIncreaseFound ) {
												ShowContinueError( "Node named " + PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).NodeNameOut + " shows monotonically increasing mass flow rate with a trend rate across iterations of " + RoundSigDigits( SlopeMdot, 7 ) + " [kg/s/iteration]" );
											}
										}
									} // significant slope found
								} // no oscillation found

								if ( MonotonicDecreaseFound || MonotonicIncreaseFound || FoundOscillationByDuplicate ) {
									HistoryTrace = "";
									for ( StackDepth = 1; StackDepth <= NumConvergenceHistoryTerms; ++StackDepth ) {
										HistoryTrace += RoundSigDigits( PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).OutletNode.MassFlowRateHistory( StackDepth ), 7 ) + ',';
									}
									ShowContinueError( "Node named " + PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).NodeNameOut + " mass flow rate [kg/s] iteration history trace (most recent first): " + HistoryTrace );
								} // need to report trace
								// end of Outlet node

							} // plant loop sides

						} // mass flow not converged

						if ( PlantConvergence( LoopNum ).PlantTempNotConverged ) {
							ShowContinueError( "Plant System Named = " + PlantLoop( LoopNum ).Name + " did not converge for temperature" );
							ShowContinueError( "Check values should be zero. Most Recent values listed first." );
							HistoryTrace = "";
							for ( StackDepth = 1; StackDepth <= ConvergLogStackDepth; ++StackDepth ) {
								HistoryTrace += RoundSigDigits( PlantConvergence( LoopNum ).PlantTempDemandToSupplyTolValue( StackDepth ), 6 ) + ',';
							}
							ShowContinueError( "Demand-to-Supply interface temperature check value iteration history trace: " + HistoryTrace );
							HistoryTrace = "";
							for ( StackDepth = 1; StackDepth <= ConvergLogStackDepth; ++StackDepth ) {
								HistoryTrace += RoundSigDigits( PlantConvergence( LoopNum ).PlantTempSupplyToDemandTolValue( StackDepth ), 6 ) + ',';
							}
							ShowContinueError( "Supply-to-Demand interface temperature check value iteration history trace: " + HistoryTrace );

							// now work with history logs for mass flow to detect issues
							for ( ThisLoopSide = 1; ThisLoopSide <= isize( PlantLoop( LoopNum ).LoopSide ); ++ThisLoopSide ) {
								// loop side inlet node
								FoundOscillationByDuplicate = false;
								MonotonicDecreaseFound = false;
								MonotonicIncreaseFound = false;
								AvgValue = sum( PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).InletNode.TemperatureHistory ) / double( NumConvergenceHistoryTerms );
								if ( std::abs( PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).InletNode.TemperatureHistory( 1 ) - AvgValue ) > PlantTemperatureOscillationToler ) {
									FoundOscillationByDuplicate = false;
									for ( StackDepth = 2; StackDepth <= NumConvergenceHistoryTerms; ++StackDepth ) {
										if ( std::abs( PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).InletNode.TemperatureHistory( 1 ) - PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).InletNode.TemperatureHistory( StackDepth ) ) < PlantTemperatureOscillationToler ) {
											FoundOscillationByDuplicate = true;
											ShowContinueError( "Node named " + PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).NodeNameIn + " shows oscillating temperatures across iterations with a repeated value of " + RoundSigDigits( PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).InletNode.TemperatureHistory( 1 ), 5 ) );
											break;
										}
									}
								}
								if ( ! FoundOscillationByDuplicate ) {
									SlopeTemps = ( sum( ConvergenceHistoryARR ) * sum( PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).InletNode.TemperatureHistory ) - double( NumConvergenceHistoryTerms ) * sum( ( ConvergenceHistoryARR * PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).InletNode.TemperatureHistory ) ) ) / ( std::pow( sum( ConvergenceHistoryARR ), 2 ) - double( NumConvergenceHistoryTerms ) * sum( pow( ConvergenceHistoryARR, 2 ) ) );
									if ( std::abs( SlopeTemps ) > PlantTemperatureSlopeToler ) {
										if ( SlopeTemps < 0.0 ) { // check for monotic decrease
											MonotonicDecreaseFound = true;
											for ( StackDepth = 2; StackDepth <= NumConvergenceHistoryTerms; ++StackDepth ) {
												if ( PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).InletNode.TemperatureHistory( StackDepth - 1 ) > PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).InletNode.TemperatureHistory( StackDepth ) ) {
													MonotonicDecreaseFound = false;
													break;
												}
											}
											if ( MonotonicDecreaseFound ) {
												ShowContinueError( "Node named " + PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).NodeNameIn + " shows monotonically decreasing temperatures with a trend rate across iterations of " + RoundSigDigits( SlopeTemps, 5 ) + " [C/iteration]" );
											}
										} else { // check for monotic incrase
											MonotonicIncreaseFound = true;
											for ( StackDepth = 2; StackDepth <= NumConvergenceHistoryTerms; ++StackDepth ) {
												if ( PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).InletNode.TemperatureHistory( StackDepth - 1 ) < PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).InletNode.TemperatureHistory( StackDepth ) ) {
													MonotonicIncreaseFound = false;
													break;
												}
											}
											if ( MonotonicIncreaseFound ) {
												ShowContinueError( "Node named " + PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).NodeNameIn + " shows monotonically increasing temperatures with a trend rate across iterations of " + RoundSigDigits( SlopeTemps, 5 ) + " [C/iteration]" );
											}
										}
									} // significant slope found
								} // no oscillation found

								if ( MonotonicDecreaseFound || MonotonicIncreaseFound || FoundOscillationByDuplicate ) {
									HistoryTrace = "";
									for ( StackDepth = 1; StackDepth <= NumConvergenceHistoryTerms; ++StackDepth ) {
										HistoryTrace += RoundSigDigits( PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).InletNode.TemperatureHistory( StackDepth ), 5 ) + ',';
									}
									ShowContinueError( "Node named " + PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).NodeNameIn + " temperature [C] iteration history trace (most recent first): " + HistoryTrace );
								} // need to report trace
								// end of inlet node

								// loop side outlet node
								FoundOscillationByDuplicate = false;
								MonotonicDecreaseFound = false;
								MonotonicIncreaseFound = false;
								AvgValue = sum( PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).OutletNode.TemperatureHistory ) / double( NumConvergenceHistoryTerms );
								if ( std::abs( PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).OutletNode.TemperatureHistory( 1 ) - AvgValue ) > PlantTemperatureOscillationToler ) {
									FoundOscillationByDuplicate = false;
									for ( StackDepth = 2; StackDepth <= NumConvergenceHistoryTerms; ++StackDepth ) {
										if ( std::abs( PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).OutletNode.TemperatureHistory( 1 ) - PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).OutletNode.TemperatureHistory( StackDepth ) ) < PlantTemperatureOscillationToler ) {
											FoundOscillationByDuplicate = true;
											ShowContinueError( "Node named " + PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).NodeNameOut + " shows oscillating temperatures across iterations with a repeated value of " + RoundSigDigits( PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).OutletNode.TemperatureHistory( 1 ), 5 ) );
											break;
										}
									}
								}
								if ( ! FoundOscillationByDuplicate ) {
									SlopeTemps = ( sum( ConvergenceHistoryARR ) * sum( PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).OutletNode.TemperatureHistory ) - double( NumConvergenceHistoryTerms ) * sum( ( ConvergenceHistoryARR * PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).OutletNode.TemperatureHistory ) ) ) / ( std::pow( sum( ConvergenceHistoryARR ), 2 ) - double( NumConvergenceHistoryTerms ) * sum( pow( ConvergenceHistoryARR, 2 ) ) );
									if ( std::abs( SlopeTemps ) > PlantFlowRateSlopeToler ) {
										if ( SlopeTemps < 0.0 ) { // check for monotic decrease
											MonotonicDecreaseFound = true;
											for ( StackDepth = 2; StackDepth <= NumConvergenceHistoryTerms; ++StackDepth ) {
												if ( PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).OutletNode.TemperatureHistory( StackDepth - 1 ) > PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).OutletNode.TemperatureHistory( StackDepth ) ) {
													MonotonicDecreaseFound = false;
													break;
												}
											}
											if ( MonotonicDecreaseFound ) {
												ShowContinueError( "Node named " + PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).NodeNameOut + " shows monotonically decreasing temperatures with a trend rate across iterations of " + RoundSigDigits( SlopeTemps, 5 ) + " [C/iteration]" );
											}
										} else { // check for monotic incrase
											MonotonicIncreaseFound = true;
											for ( StackDepth = 2; StackDepth <= NumConvergenceHistoryTerms; ++StackDepth ) {
												if ( PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).OutletNode.TemperatureHistory( StackDepth - 1 ) < PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).OutletNode.TemperatureHistory( StackDepth ) ) {
													MonotonicIncreaseFound = false;
													break;
												}
											}
											if ( MonotonicIncreaseFound ) {
												ShowContinueError( "Node named " + PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).NodeNameOut + " shows monotonically increasing temperatures with a trend rate across iterations of " + RoundSigDigits( SlopeTemps, 5 ) + " [C/iteration]" );
											}
										}
									} // significant slope found
								} // no oscillation found

								if ( MonotonicDecreaseFound || MonotonicIncreaseFound || FoundOscillationByDuplicate ) {
									HistoryTrace = "";
									for ( StackDepth = 1; StackDepth <= NumConvergenceHistoryTerms; ++StackDepth ) {
										HistoryTrace += RoundSigDigits( PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).OutletNode.TemperatureHistory( StackDepth ), 5 ) + ',';
									}
									ShowContinueError( "Node named " + PlantLoop( LoopNum ).LoopSide( ThisLoopSide ).NodeNameOut + " temperature [C] iteration history trace (most recent first): " + HistoryTrace );
								} // need to report trace
								// end of Outlet node

							} // plant loop sides

						} //temperature not converged
					} // loop over plant loop systems
				}
			} else {
				if ( EnvironmentName == ErrEnvironmentName ) {
					ShowRecurringWarningErrorAtEnd( "SimHVAC: Exceeding Maximum iterations for all HVAC loops, during " + EnvironmentName + " continues", MaxErrCount );
				} else {
					MaxErrCount = 0;
					ErrEnvironmentName = EnvironmentName;
					ShowRecurringWarningErrorAtEnd( "SimHVAC: Exceeding Maximum iterations for all HVAC loops, during " + EnvironmentName + " continues", MaxErrCount );
				}
			}

		}
		// Set node setpoints to a flag value so that controllers can check whether their sensed nodes
		// have a setpoint
		if ( ! ZoneSizingCalc && ! SysSizingCalc ) {
			if ( MySetPointInit ) {
				if ( NumOfNodes > 0 ) {
					Node.TempSetPoint() = SensedNodeFlagValue;
					Node.HumRatSetPoint() = SensedNodeFlagValue;
					Node.HumRatMin() = SensedNodeFlagValue;
					Node.HumRatMax() = SensedNodeFlagValue;
					Node.MassFlowRateSetPoint() = SensedNodeFlagValue; // BG 5-26-2009 (being checked in HVACControllers.f90)
					DefaultNodeValues.TempSetPoint = SensedNodeFlagValue;
					DefaultNodeValues.HumRatSetPoint = SensedNodeFlagValue;
					DefaultNodeValues.HumRatMin = SensedNodeFlagValue;
					DefaultNodeValues.HumRatMax = SensedNodeFlagValue;
					DefaultNodeValues.MassFlowRateSetPoint = SensedNodeFlagValue; // BG 5-26-2009 (being checked in HVACControllers.f90)
				}
				MySetPointInit = false;
				DoSetPointTest = true;
			} else {
				DoSetPointTest = false;
			}
		}
		if ( SetPointErrorFlag ) {
			ShowFatalError( "Previous severe set point errors cause program termination" );
		}

	}

	void
	SimSelectedEquipment(
		bool & SimAirLoops, // True when the air loops need to be (re)simulated
		bool & SimZoneEquipment, // True when zone equipment components need to be (re)simulated
		bool & SimNonZoneEquipment, // True when non-zone equipment components need to be (re)simulated
		bool & SimPlantLoops, // True when the main plant loops need to be (re)simulated
		bool & SimElecCircuits, // True when electic circuits need to be (re)simulated
		bool & FirstHVACIteration, // True when solution technique on first iteration
		bool const LockPlantFlows
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor, Rick Strand
		//       DATE WRITTEN   May 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine receives the flags from SimHVAC which determines
		// which middle-level managers must be called.

		// METHODOLOGY EMPLOYED:
		// Each flag is checked and the appropriate manager is then called.

		// REFERENCES:
		// na

		// Using/Aliasing
		using ZoneEquipmentManager::ManageZoneEquipment;
		using NonZoneEquipmentManager::ManageNonZoneEquipment;
		using SimAirServingZones::ManageAirLoops;
		using PlantManager::ManagePlantLoops;
		using ManageElectricPower::ManageElectricLoadCenters;
		using AirflowNetworkBalanceManager::ManageAirflowNetworkBalance;
		using DataErrorTracking::AskForPlantCheckOnAbort;
		using PlantUtilities::SetAllFlowLocks;
		using PlantUtilities::ResetAllPlantInterConnectFlags;
		using DataPlant::FlowUnlocked;
		using DataPlant::FlowLocked;
		using DataPlant::AnyPlantLoopSidesNeedSim;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		bool ResimulateAirZone; // True when solution technique on third iteration used in AirflowNetwork

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxAir( 5 ); // Iteration Max for Air Simulation Iterations
		int const MaxPlant( 3 ); // Iteration Max for Plant Simulation Iteration
		int const MaxCond( 3 ); // Iteration Max for Plant Simulation Iteration

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int IterAir; // counts iterations to enforce maximum iteration limit
		static bool MyEnvrnFlag( true );
		static bool FlowMaxAvailAlreadyReset( false );
		static bool FlowResolutionNeeded( false );

		// FLOW:

		IterAir = 0;

		// Set all plant flow locks to UNLOCKED to allow air side components to operate properly
		// This requires that the plant flow resolver carefully set the min/max avail limits on
		//  air side components to ensure they request within bounds.
		if ( LockPlantFlows ) {
			SetAllFlowLocks( FlowLocked );
		} else {
			SetAllFlowLocks( FlowUnlocked );
		}
		ResetAllPlantInterConnectFlags();

		if ( BeginEnvrnFlag && MyEnvrnFlag ) {
			// Following comment is incorrect!  (LKL) Even the first time through this does more than read in data.
			// Zone equipment data needs to be read in before air loop data to allow the
			// determination of which zones are connected to which air loops.
			// This call of ManageZoneEquipment does nothing except force the
			// zone equipment data to be read in.
			ManageZoneEquipment( FirstHVACIteration, SimZoneEquipment, SimAirLoops );
			MyEnvrnFlag = false;
		}
		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag = true;
		}

		if ( FirstHVACIteration ) {
			RepIterAir = 0;
			// Call AirflowNetwork simulation to calculate air flows and pressures
			if ( SimulateAirflowNetwork > AirflowNetworkControlSimple ) {
				ManageAirflowNetworkBalance( FirstHVACIteration );
			}
			ManageAirLoops( FirstHVACIteration, SimAirLoops, SimZoneEquipment );
			AirLoopInputsFilled = true; // all air loop inputs have been read in
			SimAirLoops = true; //Need to make sure that SimAirLoop is simulated at min twice to calculate PLR in some air loop equipment
			AirLoopsSimOnce = true; // air loops simulated once for this environment
			ResetTerminalUnitFlowLimits();
			FlowMaxAvailAlreadyReset = true;
			ManageZoneEquipment( FirstHVACIteration, SimZoneEquipment, SimAirLoops );
			SimZoneEquipment = true; //needs to be simulated at least twice for flow resolution to propagate to this routine
			ManageNonZoneEquipment( FirstHVACIteration, SimNonZoneEquipment );

			ManageElectricLoadCenters( FirstHVACIteration, SimElecCircuits, false );

			ManagePlantLoops( FirstHVACIteration, SimAirLoops, SimZoneEquipment, SimNonZoneEquipment, SimPlantLoops, SimElecCircuits );

			AskForPlantCheckOnAbort = true; // need to make a first pass through plant calcs before this check make sense
			ManageElectricLoadCenters( FirstHVACIteration, SimElecCircuits, false );
		} else {
			FlowResolutionNeeded = false;
			while ( ( SimAirLoops || SimZoneEquipment ) && ( IterAir <= MaxAir ) ) {
				++IterAir; // Increment the iteration counter
				// Call AirflowNetwork simulation to calculate air flows and pressures
				ResimulateAirZone = false;
				if ( SimulateAirflowNetwork > AirflowNetworkControlSimple ) {
					ManageAirflowNetworkBalance( FirstHVACIteration, IterAir, ResimulateAirZone );
				}
				if ( SimAirLoops ) {
					ManageAirLoops( FirstHVACIteration, SimAirLoops, SimZoneEquipment );
					SimElecCircuits = true; //If this was simulated there are possible electric changes that need to be simulated
				}

				// make sure flow resolution gets done
				if ( FlowResolutionNeeded ) {
					SimZoneEquipment = true;
				}
				if ( SimZoneEquipment ) {
					if ( ( IterAir == 1 ) && ( ! FlowMaxAvailAlreadyReset ) ) { // don't do reset if already done in FirstHVACIteration
						ResetTerminalUnitFlowLimits();
						FlowResolutionNeeded = true;
					} else {
						ResolveAirLoopFlowLimits();
						FlowResolutionNeeded = false;
					}
					ManageZoneEquipment( FirstHVACIteration, SimZoneEquipment, SimAirLoops );
					SimElecCircuits = true; // If this was simulated there are possible electric changes that need to be simulated

				}
				FlowMaxAvailAlreadyReset = false;

				//      IterAir = IterAir + 1   ! Increment the iteration counter
				if ( SimulateAirflowNetwork > AirflowNetworkControlSimple ) {
					if ( ResimulateAirZone ) { // Need to make sure that SimAirLoop and SimZoneEquipment are simulated
						SimAirLoops = true; // at min three times using ONOFF fan with the AirflowNetwork model
						SimZoneEquipment = true;
					}
				}

			}

			RepIterAir += IterAir;
			if ( IterAir > MaxAir ) {
				AirLoopConvergFail = 1;
			} else {
				AirLoopConvergFail = 0;
			}
			// Check to see if any components have been locked out. If so, SimAirLoops will be reset to TRUE.
			ResolveLockoutFlags( SimAirLoops );

			if ( SimNonZoneEquipment ) {
				ManageNonZoneEquipment( FirstHVACIteration, SimNonZoneEquipment );
				SimElecCircuits = true; // If this was simulated there are possible electric changes that need to be simulated
			}

			if ( SimElecCircuits ) {
				ManageElectricLoadCenters( FirstHVACIteration, SimElecCircuits, false );
			}

			if ( ! SimPlantLoops ) {
				// check to see if any air side component may have requested plant resim
				if ( AnyPlantLoopSidesNeedSim() ) {
					SimPlantLoops = true;
				}

			}

			if ( SimPlantLoops ) {
				ManagePlantLoops( FirstHVACIteration, SimAirLoops, SimZoneEquipment, SimNonZoneEquipment, SimPlantLoops, SimElecCircuits );
			}

			if ( SimElecCircuits ) {
				ManageElectricLoadCenters( FirstHVACIteration, SimElecCircuits, false );
			}

		}

	}

	void
	ResetTerminalUnitFlowLimits()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   Feb 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Reset the max flow available limits at the inlet nodes of terminal units

		// METHODOLOGY EMPLOYED:
		// Loops through all air loops, finds the inlet nodes of the terminal units
		// served by each air loop, and resets the node MassFlowRateMaxAvail (and MinAvail) to
		// the hard max and mins.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataZoneEquipment::ZoneEquipConfig;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int AirLoopIndex;
		int ZonesCooledIndex;
		int ZonesHeatedIndex;
		int TermInletNode;

		for ( AirLoopIndex = 1; AirLoopIndex <= NumPrimaryAirSys; ++AirLoopIndex ) { // loop over the primary air loops
			for ( ZonesCooledIndex = 1; ZonesCooledIndex <= AirToZoneNodeInfo( AirLoopIndex ).NumZonesCooled; ++ZonesCooledIndex ) { // loop over the zones cooled by this air loop
				TermInletNode = AirToZoneNodeInfo( AirLoopIndex ).TermUnitCoolInletNodes( ZonesCooledIndex );
				// reset the max avail flow rate at the terminal unit cold air inlet to the max
				Node( TermInletNode ).MassFlowRateMaxAvail = Node( TermInletNode ).MassFlowRateMax;
				Node( TermInletNode ).MassFlowRateMinAvail = Node( TermInletNode ).MassFlowRateMin;
			}
			for ( ZonesHeatedIndex = 1; ZonesHeatedIndex <= AirToZoneNodeInfo( AirLoopIndex ).NumZonesHeated; ++ZonesHeatedIndex ) { // loop over the zones heated by this air loop
				TermInletNode = AirToZoneNodeInfo( AirLoopIndex ).TermUnitHeatInletNodes( ZonesHeatedIndex );
				// reset the max avail flow rate at the terminal unit hot air inlet to the max
				Node( TermInletNode ).MassFlowRateMaxAvail = Node( TermInletNode ).MassFlowRateMax;
				Node( TermInletNode ).MassFlowRateMinAvail = Node( TermInletNode ).MassFlowRateMin;
			}
		}

	}

	void
	ResolveAirLoopFlowLimits()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   August 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for resolving hard flow mismatches between zone equipment and
		// the primary air loop. Such a mismatch can occur when the air terminal units are
		// requsting more air than the central air system can supply.

		// METHODOLOGY EMPLOYED:
		// Sets the MassFlowRateMaxAvail on the terminal unit inlet nodes to match the
		// maximum available from the primary air loop.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataZoneEquipment::ZoneEquipConfig;
		using DataConvergParams::HVACFlowRateToler;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int AirLoopIndex;
		int ZonesCooledIndex;
		int ZonesHeatedIndex;
		int TermInletNode;
		int SupplyIndex;
		int SupplyNode;
		Real64 FlowRatio;

		for ( AirLoopIndex = 1; AirLoopIndex <= NumPrimaryAirSys; ++AirLoopIndex ) { // loop over the primary air loops
			for ( SupplyIndex = 1; SupplyIndex <= AirToZoneNodeInfo( AirLoopIndex ).NumSupplyNodes; ++SupplyIndex ) { // loop over the air loop supply outlets
				if ( AirToZoneNodeInfo( AirLoopIndex ).SupplyDuctType( SupplyIndex ) == Cooling ) { // check for cooling duct
					// check if terminal units requesting more air than air loop can supply; if so, set terminal unit inlet
					// node mass flow max avail to what air loop can supply
					SupplyNode = AirToZoneNodeInfo( AirLoopIndex ).AirLoopSupplyNodeNum( SupplyIndex );
					if ( Node( SupplyNode ).MassFlowRate > 0.0 ) {
						if ( ( Node( SupplyNode ).MassFlowRateSetPoint - Node( SupplyNode ).MassFlowRate ) > HVACFlowRateToler * 0.01 ) {
							FlowRatio = Node( SupplyNode ).MassFlowRate / Node( SupplyNode ).MassFlowRateSetPoint;
							for ( ZonesCooledIndex = 1; ZonesCooledIndex <= AirToZoneNodeInfo( AirLoopIndex ).NumZonesCooled; ++ZonesCooledIndex ) {
								TermInletNode = AirToZoneNodeInfo( AirLoopIndex ).TermUnitCoolInletNodes( ZonesCooledIndex );
								Node( TermInletNode ).MassFlowRateMaxAvail = Node( TermInletNode ).MassFlowRate * FlowRatio;
							}
						}
						if ( ( Node( SupplyNode ).MassFlowRateSetPoint - Node( SupplyNode ).MassFlowRate ) < -HVACFlowRateToler * 0.01 ) {
							if ( Node( SupplyNode ).MassFlowRateSetPoint == 0.0 ) {
								//               CALL ShowFatalError('ResolveAirLoopFlowLimits: Node MassFlowRateSetPoint = 0.0, Node='//  &
								//                                   TRIM(NodeID(SupplyNode))//  &
								//                                   ', check for Node Connection Errors in the following messages.')
								for ( ZonesCooledIndex = 1; ZonesCooledIndex <= AirToZoneNodeInfo( AirLoopIndex ).NumZonesCooled; ++ZonesCooledIndex ) {
									TermInletNode = AirToZoneNodeInfo( AirLoopIndex ).TermUnitCoolInletNodes( ZonesCooledIndex );
									Node( TermInletNode ).MassFlowRateMaxAvail = Node( TermInletNode ).MassFlowRateMax;
									Node( TermInletNode ).MassFlowRateMinAvail = Node( SupplyNode ).MassFlowRate / double( AirToZoneNodeInfo( AirLoopIndex ).NumZonesCooled );
								}
							} else {
								FlowRatio = Node( SupplyNode ).MassFlowRate / Node( SupplyNode ).MassFlowRateSetPoint;
								for ( ZonesCooledIndex = 1; ZonesCooledIndex <= AirToZoneNodeInfo( AirLoopIndex ).NumZonesCooled; ++ZonesCooledIndex ) {
									TermInletNode = AirToZoneNodeInfo( AirLoopIndex ).TermUnitCoolInletNodes( ZonesCooledIndex );
									Node( TermInletNode ).MassFlowRateMinAvail = Node( TermInletNode ).MassFlowRate * FlowRatio;
								}
							}
						}
					}
				}
			}
			for ( SupplyIndex = 1; SupplyIndex <= AirToZoneNodeInfo( AirLoopIndex ).NumSupplyNodes; ++SupplyIndex ) { // loop over the air loop supply outlets
				if ( AirToZoneNodeInfo( AirLoopIndex ).SupplyDuctType( SupplyIndex ) == Heating ) { // check for heating duct
					// check if terminal units requesting more air than air loop can supply; if so, set terminal unit inlet
					// node mass flow max avail to what air loop can supply
					SupplyNode = AirToZoneNodeInfo( AirLoopIndex ).AirLoopSupplyNodeNum( SupplyIndex );
					if ( Node( SupplyNode ).MassFlowRate > 0.0 ) {
						if ( ( Node( SupplyNode ).MassFlowRateSetPoint - Node( SupplyNode ).MassFlowRate ) > HVACFlowRateToler * 0.01 ) {
							FlowRatio = Node( SupplyNode ).MassFlowRate / Node( SupplyNode ).MassFlowRateSetPoint;
							for ( ZonesHeatedIndex = 1; ZonesHeatedIndex <= AirToZoneNodeInfo( AirLoopIndex ).NumZonesHeated; ++ZonesHeatedIndex ) {
								TermInletNode = AirToZoneNodeInfo( AirLoopIndex ).TermUnitHeatInletNodes( ZonesHeatedIndex );
								Node( TermInletNode ).MassFlowRateMaxAvail = Node( TermInletNode ).MassFlowRate * FlowRatio;
							}
						}
						if ( ( Node( SupplyNode ).MassFlowRateSetPoint - Node( SupplyNode ).MassFlowRate ) < -HVACFlowRateToler * 0.01 ) {
							if ( Node( SupplyNode ).MassFlowRateSetPoint == 0.0 ) {
								// CALL ShowFatalError('ResolveAirLoopFlowLimits: Node MassFlowRateSetPoint = 0.0, Node='//  &
								// TRIM(NodeID(SupplyNode))//  &
								// ', check for Node Connection Errors in the following messages.')
								for ( ZonesHeatedIndex = 1; ZonesHeatedIndex <= AirToZoneNodeInfo( AirLoopIndex ).NumZonesHeated; ++ZonesHeatedIndex ) {
									TermInletNode = AirToZoneNodeInfo( AirLoopIndex ).TermUnitHeatInletNodes( ZonesHeatedIndex );
									Node( TermInletNode ).MassFlowRateMaxAvail = Node( TermInletNode ).MassFlowRateMax;
									Node( TermInletNode ).MassFlowRateMinAvail = Node( SupplyNode ).MassFlowRate / double( AirToZoneNodeInfo( AirLoopIndex ).NumZonesCooled );
								}
							} else {
								FlowRatio = Node( SupplyNode ).MassFlowRate / Node( SupplyNode ).MassFlowRateSetPoint;
								for ( ZonesHeatedIndex = 1; ZonesHeatedIndex <= AirToZoneNodeInfo( AirLoopIndex ).NumZonesHeated; ++ZonesHeatedIndex ) {
									TermInletNode = AirToZoneNodeInfo( AirLoopIndex ).TermUnitHeatInletNodes( ZonesHeatedIndex );
									Node( TermInletNode ).MassFlowRateMinAvail = Node( TermInletNode ).MassFlowRate * FlowRatio;
								}
							}
						}
					}
				}
			}
		}

	}

	void
	ResolveLockoutFlags( bool & SimAir ) // TRUE means air loops must be (re)simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   December 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine checks for components lockout flags and asks for air loop resimulation
		// if any components have been locked out

		// METHODOLOGY EMPLOYED:
		// Checks if loop lockout flags are .TRUE.; if so, sets SimAirLoops to .TRUE.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int AirLoopIndex;

		for ( AirLoopIndex = 1; AirLoopIndex <= NumPrimaryAirSys; ++AirLoopIndex ) { // loop over the primary air loops
			// check if economizer ia active and if there is a request that it be locked out
			if ( AirLoopControlInfo( AirLoopIndex ).EconoActive && ( AirLoopControlInfo( AirLoopIndex ).ReqstEconoLockoutWithCompressor || AirLoopControlInfo( AirLoopIndex ).ReqstEconoLockoutWithHeating ) ) {
				AirLoopControlInfo( AirLoopIndex ).EconoLockout = true;
				SimAir = true;
			}
		}

	}

	void
	ResetHVACControl()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   December 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine resets loop control flags and specified flow rates that may
		// have been set by the set point and availability managers in the previous
		// time step

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		if ( NumPrimaryAirSys == 0 ) return;
		AirLoopControlInfo.NightVent() = false;
		AirLoopControlInfo.LoopFlowRateSet() = false;
		AirLoopFlow.ReqSupplyFrac() = 1.0;

	}

	void
	ResetNodeData()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   March 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine resets all node data to "initial" conditions.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na
		if ( NumOfNodes <= 0 ) return;

		Node.Temp() = DefaultNodeValues.Temp;
		Node.TempMin() = DefaultNodeValues.TempMin;
		Node.TempMax() = DefaultNodeValues.TempMax;
		Node.TempSetPoint() = DefaultNodeValues.TempSetPoint;
		Node.MassFlowRate() = DefaultNodeValues.MassFlowRate;
		Node.MassFlowRateMin() = DefaultNodeValues.MassFlowRateMin;
		Node.MassFlowRateMax() = DefaultNodeValues.MassFlowRateMax;
		Node.MassFlowRateMinAvail() = DefaultNodeValues.MassFlowRateMinAvail;
		Node.MassFlowRateMaxAvail() = DefaultNodeValues.MassFlowRateMaxAvail;
		Node.MassFlowRateSetPoint() = DefaultNodeValues.MassFlowRateSetPoint;
		Node.Quality() = DefaultNodeValues.Quality;
		Node.Press() = DefaultNodeValues.Press;
		Node.Enthalpy() = DefaultNodeValues.Enthalpy;
		Node.HumRat() = DefaultNodeValues.HumRat;
		Node.HumRatMin() = DefaultNodeValues.HumRatMin;
		Node.HumRatMax() = DefaultNodeValues.HumRatMax;
		Node.HumRatSetPoint() = DefaultNodeValues.HumRatSetPoint;
		Node.TempSetPointHi() = DefaultNodeValues.TempSetPointHi;
		Node.TempSetPointLo() = DefaultNodeValues.TempSetPointLo;

		if ( allocated( MoreNodeInfo ) ) {
			MoreNodeInfo.WetBulbTemp() = DefaultNodeValues.Temp;
			MoreNodeInfo.RelHumidity() = 0.0;
			MoreNodeInfo.ReportEnthalpy() = DefaultNodeValues.Enthalpy;
			MoreNodeInfo.VolFlowRateStdRho() = 0.0;
			MoreNodeInfo.VolFlowRateCrntRho() = 0.0;
			MoreNodeInfo.Density() = 0.0;
		}

	}

	void
	UpdateZoneListAndGroupLoads()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Apparently someone who doesn't believe in documenting.
		//       DATE WRITTEN   ???
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Don't know.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataHeatBalance;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ZoneNum;
		int ListNum;
		int GroupNum;
		int Mult;

		// FLOW:
		// Sum ZONE LIST and ZONE GROUP report variables
		ListSNLoadHeatEnergy = 0.0;
		ListSNLoadCoolEnergy = 0.0;
		ListSNLoadHeatRate = 0.0;
		ListSNLoadCoolRate = 0.0;

		for ( ListNum = 1; ListNum <= NumOfZoneLists; ++ListNum ) {
			for ( ZoneNum = 1; ZoneNum <= ZoneList( ListNum ).NumOfZones; ++ZoneNum ) {
				Mult = Zone( ZoneNum ).Multiplier;
				ListSNLoadHeatEnergy( ListNum ) += SNLoadHeatEnergy( ZoneList( ListNum ).Zone( ZoneNum ) ) * Mult;
				ListSNLoadCoolEnergy( ListNum ) += SNLoadCoolEnergy( ZoneList( ListNum ).Zone( ZoneNum ) ) * Mult;
				ListSNLoadHeatRate( ListNum ) += SNLoadHeatRate( ZoneList( ListNum ).Zone( ZoneNum ) ) * Mult;
				ListSNLoadCoolRate( ListNum ) += SNLoadCoolRate( ZoneList( ListNum ).Zone( ZoneNum ) ) * Mult;
			} // ZoneNum
		} // ListNum

		for ( GroupNum = 1; GroupNum <= NumOfZoneGroups; ++GroupNum ) {
			Mult = ZoneGroup( GroupNum ).Multiplier;
			GroupSNLoadHeatEnergy( GroupNum ) = ListSNLoadHeatEnergy( ZoneGroup( GroupNum ).ZoneList ) * Mult;
			GroupSNLoadCoolEnergy( GroupNum ) = ListSNLoadCoolEnergy( ZoneGroup( GroupNum ).ZoneList ) * Mult;
			GroupSNLoadHeatRate( GroupNum ) = ListSNLoadHeatRate( ZoneGroup( GroupNum ).ZoneList ) * Mult;
			GroupSNLoadCoolRate( GroupNum ) = ListSNLoadCoolRate( ZoneGroup( GroupNum ).ZoneList ) * Mult;
		} // GroupNum

	}

	void
	CalcAirFlowSimple( Optional_int_const SysTimestepLoop ) // System time step index
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Legacy Code
		//       DATE WRITTEN   na
		//       MODIFIED       Shirey, Jan 2008 (MIXING objects, use avg. conditions for Cp, Air Density and Hfg)
		//       MODIFIED       L. Lawrie and L. GU, Jan. 2008 (Allow multiple infiltration and ventilation objects)
		//                      B. Griffith. Jan 2009 add infiltration, residential basic/sherman-grimsrud and enhanced/AIM2
		//                      L. Lawrie - March 2009 - move ventilation electric calculation to this routine (for
		//                        Electricity Net.
		//                      L. Gu - Dec. 2009 - Added a new ventilation object to calculate flow rate based on wind and stack
		//                        effect through an opening.
		//       MODIFIED       Stovall - Aug 2011 (add refrigerator door mixing)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates the air component of the heat balance.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataEnvironment::OutBaroPress;
		using DataEnvironment::OutHumRat;
		using DataEnvironment::OutEnthalpy;
		using DataEnvironment::WindSpeed;
		using namespace DataHeatBalFanSys;
		using namespace DataHeatBalance;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyCpAirFnWTdb;
		using Psychrometrics::PsyTdbFnHW;
		using DataRoomAirModel::ZTJET;
		using DataRoomAirModel::AirModel;
		using DataRoomAirModel::RoomAirModel_UCSDDV;
		using DataRoomAirModel::RoomAirModel_UCSDCV;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataAirflowNetwork::SimulateAirflowNetwork;
		using DataAirflowNetwork::AirflowNetworkControlSimple;
		using DataAirflowNetwork::AirflowNetworkControlSimpleADS;
		using DataAirflowNetwork::AirflowNetworkZoneFlag;
		using EarthTube::ManageEarthTube;
		using CoolTower::ManageCoolTower;
		using ThermalChimney::ManageThermalChimney;
		using DataZoneEquipment::ZoneEquipAvail;
		using DataHVACGlobals::CycleOn;
		using DataHVACGlobals::CycleOnZoneFansOnly;
		using DataContaminantBalance::Contaminant;
		using DataContaminantBalance::ZoneAirCO2;
		using DataContaminantBalance::MixingMassFlowCO2;
		using DataContaminantBalance::ZoneAirGC;
		using DataContaminantBalance::MixingMassFlowGC;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const StdGravity( 9.80665 ); // The acceleration of gravity at the sea level (m/s2)

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 MCP;
		Real64 MCPxM;
		Real64 MCPxN;
		Real64 TZM; // Temperature of From Zone
		Real64 TZN; // Temperature of this zone
		Real64 TD; // Delta Temp limit of Mixing statement
		Real64 Tavg; // Average temperature in two zones exchanging air
		Real64 Wavg; // Average humidity ratio in two zones exchanging air
		int m; // Index to From Zone
		int n; // Index of this zone
		int j; // Loop Counter
		int NZ; // A pointer
		int I; // Ventilation master object index
		int NH; // Hybrid controlled zone number
		Real64 AirDensity; // Density of air (kg/m^3)
		Real64 CpAir; // Heat capacity of air (J/kg-C)
		Real64 OutletAirEnthalpy; // Enthlapy of outlet air (VENTILATION objects)
		Real64 TempExt;
		Real64 WindExt;
		bool MixingLimitFlag;
		Real64 MixingTmin;
		Real64 MixingTmax;

		Real64 IVF; // DESIGN INFILTRATION FLOW RATE (M**3/SEC)
		Real64 VVF; // DESIGN VENTILATION FLOW RATE (M**3/SEC)
		Real64 MCpI_temp;
		Real64 VAMFL_temp;
		static FArray1D< Real64 > ZMAT; // Zone air temperature
		static FArray1D< Real64 > ZHumRat; // Zone air humidity ratio
		Real64 Cw; // Opening effectivenss
		Real64 Cd; // Discharge coefficent
		Real64 angle; // Angle between wind direction and effective angle
		Real64 Qw; // Volumetric flow driven by wind
		Real64 Qst; // Volumetric flow driven by stack effect
		Real64 MassFlowDiff;
		//following variables used for refrigeration door mixing and all defined in EngRef
		int ZoneA;
		int ZoneB;
		Real64 TZoneA;
		Real64 TZoneB;
		Real64 HumRatZoneA;
		Real64 HumRatZoneB;
		Real64 AirDensityZoneA;
		Real64 CpAirZoneA;
		Real64 AirDensityZoneB;
		Real64 CpAirZoneB;
		Real64 AirDensityAvg;
		Real64 MassFlowDryAir;
		Real64 SchedDoorOpen;
		Real64 DoorHeight;
		Real64 DoorArea;
		Real64 DoorProt;
		Real64 FDens;
		Real64 Fb;
		Real64 FFlow;
		Real64 MassFlowToA;
		Real64 MassFlowToB;
		Real64 MassFlowXCpToA;
		Real64 MassFlowXCpToB;
		Real64 MassFlowXCpXTempToA;
		Real64 MassFlowXCpXTempToB;
		Real64 MassFlowXHumRatToA;
		Real64 MassFlowXHumRatToB;
		// Allocate the ZMAT and ZHumRat arrays

		if ( ! allocated( ZMAT ) ) ZMAT.allocate( NumOfZones );
		if ( ! allocated( ZHumRat ) ) ZHumRat.allocate( NumOfZones );
		if ( ! allocated( VentMCP ) ) VentMCP.allocate( TotVentilation );

		// Allocate module level logical arrays for MIXING and CROSS MIXING reporting
		if ( ! allocated( CrossMixingReportFlag ) ) CrossMixingReportFlag.allocate( TotCrossMixing );
		if ( ! allocated( MixingReportFlag ) ) MixingReportFlag.allocate( TotMixing );

		if ( ! allocated( MCPTThermChim ) ) MCPTThermChim.allocate( NumOfZones );
		if ( ! allocated( MCPThermChim ) ) MCPThermChim.allocate( NumOfZones );
		if ( ! allocated( ThermChimAMFL ) ) ThermChimAMFL.allocate( NumOfZones );

		//                                      COMPUTE ZONE AIR MIXINGS
		MCPM = 0.0;
		MCPTM = 0.0;
		MixingMassFlowZone = 0.0;
		MixingMassFlowXHumRat = 0.0;
		CrossMixingFlag = false;
		CrossMixingReportFlag = false;
		MixingReportFlag = false;
		if ( Contaminant.CO2Simulation && TotMixing + TotCrossMixing + TotRefDoorMixing > 0 ) MixingMassFlowCO2 = 0.0;
		if ( Contaminant.GenericContamSimulation && TotMixing + TotCrossMixing + TotRefDoorMixing > 0 ) MixingMassFlowGC = 0.0;

		IVF = 0.0;
		MCPTI = 0.0;
		MCPI = 0.0;
		OAMFL = 0.0;
		VVF = 0.0;
		MCPTV = 0.0;
		MCPV = 0.0;
		VAMFL = 0.0;
		VentMCP = 0.0;
		MDotCPOA = 0.0;
		MDotOA = 0.0;

		MCPThermChim = 0.0;
		ThermChimAMFL = 0.0;
		MCPTThermChim = 0.0;

		if ( AirFlowFlag != UseSimpleAirFlow ) return;
		// AirflowNetwork Multizone field /= SIMPLE
		if ( ! ( SimulateAirflowNetwork == AirflowNetworkControlSimple || SimulateAirflowNetwork == AirflowNetworkControlSimpleADS ) ) return;

		ManageEarthTube();
		ManageCoolTower();
		ManageThermalChimney();

		// Assign zone air temperature
		for ( j = 1; j <= NumOfZones; ++j ) {
			ZMAT( j ) = MAT( j );
			ZHumRat( j ) = ZoneAirHumRat( j );
			// This is only temperory fix for CR8867.  (L. Gu 8/12)
			if ( present( SysTimestepLoop ) && SysTimestepLoop == 1 ) {
				ZMAT( j ) = XMPT( j );
				ZHumRat( j ) = WZoneTimeMinusP( j );
			}
		}

		// Process the scheduled Ventilation for air heat balance
		if ( TotVentilation > 0 ) {
			ZnAirRpt.VentilFanElec() = 0.0;
		}

		// Initialization of ZoneAirBalance
		if ( TotZoneAirBalance > 0 ) {
			ZoneAirBalance.BalMassFlowRate() = 0.0;
			ZoneAirBalance.InfMassFlowRate() = 0.0;
			ZoneAirBalance.NatMassFlowRate() = 0.0;
			ZoneAirBalance.ExhMassFlowRate() = 0.0;
			ZoneAirBalance.IntMassFlowRate() = 0.0;
			ZoneAirBalance.ERVMassFlowRate() = 0.0;
		}

		for ( j = 1; j <= TotVentilation; ++j ) {
			NZ = Ventilation( j ).ZonePtr;
			Ventilation( j ).FanPower = 0.0;
			TempExt = Zone( NZ ).OutDryBulbTemp;
			WindExt = Zone( NZ ).WindSpeed;
			AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, TempExt, OutHumRat );
			CpAir = PsyCpAirFnWTdb( OutHumRat, TempExt );
			//CR7751 should maybe use code below, indoor conditions instead of outdoor conditions
			//   AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress, ZMAT(NZ), ZHumRat(NZ))
			//   CpAir = PsyCpAirFnWTdb(ZHumRat(NZ),ZMAT(NZ))
			// Hybrid ventilation global control
			if ( Ventilation( j ).HybridControlType == HybridControlTypeGlobal && Ventilation( j ).HybridControlMasterNum > 0 ) {
				I = Ventilation( j ).HybridControlMasterNum;
				NH = Ventilation( I ).ZonePtr;
				if ( j == I ) Ventilation( j ).HybridControlMasterStatus = false;
			} else {
				I = j;
				NH = NZ;
			}
			// Check scheduled temperatures
			if ( Ventilation( I ).MinIndoorTempSchedPtr > 0 ) {
				Ventilation( I ).MinIndoorTemperature = GetCurrentScheduleValue( Ventilation( I ).MinIndoorTempSchedPtr );
			}
			if ( Ventilation( I ).MaxIndoorTempSchedPtr > 0 ) {
				Ventilation( I ).MaxIndoorTemperature = GetCurrentScheduleValue( Ventilation( I ).MaxIndoorTempSchedPtr );
			}
			// Ensure the minimum indoor temperature <= the maximum indoor temperature
			if ( Ventilation( I ).MinIndoorTempSchedPtr > 0 || Ventilation( I ).MaxIndoorTempSchedPtr > 0 ) {
				if ( Ventilation( I ).MinIndoorTemperature > Ventilation( I ).MaxIndoorTemperature ) {
					++Ventilation( I ).IndoorTempErrCount;
					if ( Ventilation( I ).IndoorTempErrCount < 2 ) {
						ShowWarningError( "Ventilation indoor temperature control: The minimum indoor temperature is above " "the maximum indoor temperature in " + Ventilation( I ).Name );
						ShowContinueError( "The minimum indoor temperature is set to the maximum indoor temperature. " "Simulation continues." );
						ShowContinueErrorTimeStamp( " Occurrence info:" );
					} else {
						ShowRecurringWarningErrorAtEnd( "The minimum indoor temperature is still above " "the maximum indoor temperature", Ventilation( I ).IndoorTempErrIndex, Ventilation( I ).MinIndoorTemperature, Ventilation( I ).MinIndoorTemperature );
					}
					Ventilation( I ).MinIndoorTemperature = Ventilation( I ).MaxIndoorTemperature;
				}
			}
			if ( Ventilation( I ).MinOutdoorTempSchedPtr > 0 ) {
				Ventilation( I ).MinOutdoorTemperature = GetCurrentScheduleValue( Ventilation( I ).MinOutdoorTempSchedPtr );
			}
			if ( Ventilation( I ).MaxOutdoorTempSchedPtr > 0 ) {
				Ventilation( I ).MaxOutdoorTemperature = GetCurrentScheduleValue( Ventilation( I ).MaxOutdoorTempSchedPtr );
			}
			// Ensure the minimum outdoor temperature <= the maximum outdoor temperature
			if ( Ventilation( I ).MinOutdoorTempSchedPtr > 0 || Ventilation( I ).MaxOutdoorTempSchedPtr > 0 ) {
				if ( Ventilation( I ).MinOutdoorTemperature > Ventilation( I ).MaxOutdoorTemperature ) {
					++Ventilation( I ).OutdoorTempErrCount;
					if ( Ventilation( I ).OutdoorTempErrCount < 2 ) {
						ShowWarningError( "Ventilation outdoor temperature control: The minimum outdoor temperature is above " "the maximum outdoor temperature in " + Ventilation( I ).Name );
						ShowContinueError( "The minimum outdoor temperature is set to the maximum outdoor temperature. " "Simulation continues." );
						ShowContinueErrorTimeStamp( " Occurrence info:" );
					} else {
						ShowRecurringWarningErrorAtEnd( "The minimum outdoor temperature is still above " "the maximum outdoor temperature", Ventilation( I ).OutdoorTempErrIndex, Ventilation( I ).MinOutdoorTemperature, Ventilation( I ).MinOutdoorTemperature );
					}
					Ventilation( I ).MinIndoorTemperature = Ventilation( I ).MaxIndoorTemperature;
				}
			}
			if ( Ventilation( I ).DeltaTempSchedPtr > 0 ) {
				Ventilation( I ).DelTemperature = GetCurrentScheduleValue( Ventilation( I ).DeltaTempSchedPtr );
			}
			// Skip this if the zone is below the minimum indoor temperature limit
			if ( ( ZMAT( NH ) < Ventilation( I ).MinIndoorTemperature ) && ( ! Ventilation( j ).EMSSimpleVentOn ) ) continue;
			// Skip this if the zone is above the maximum indoor temperature limit
			if ( ( ZMAT( NH ) > Ventilation( I ).MaxIndoorTemperature ) && ( ! Ventilation( j ).EMSSimpleVentOn ) ) continue;
			// Skip if below the temperature difference limit (3/12/03 Negative DelTemperature allowed now)
			if ( ( ( ZMAT( NH ) - TempExt ) < Ventilation( I ).DelTemperature ) && ( ! Ventilation( j ).EMSSimpleVentOn ) ) continue;
			// Skip this if the outdoor temperature is below the minimum outdoor temperature limit
			if ( ( TempExt < Ventilation( I ).MinOutdoorTemperature ) && ( ! Ventilation( j ).EMSSimpleVentOn ) ) continue;
			// Skip this if the outdoor temperature is above the maximum outdoor temperature limit
			if ( ( TempExt > Ventilation( I ).MaxOutdoorTemperature ) && ( ! Ventilation( j ).EMSSimpleVentOn ) ) continue;
			// Skip this if the outdoor wind speed is above the maximum windspeed limit
			if ( ( WindExt > Ventilation( I ).MaxWindSpeed ) && ( ! Ventilation( j ).EMSSimpleVentOn ) ) continue;

			// Hybrid ventilation controls
			if ( ( Ventilation( j ).HybridControlType == HybridControlTypeClose ) && ( ! Ventilation( j ).EMSSimpleVentOn ) ) continue;
			if ( Ventilation( j ).HybridControlType == HybridControlTypeGlobal && Ventilation( j ).HybridControlMasterNum > 0 ) {
				if ( j == I ) Ventilation( j ).HybridControlMasterStatus = true;
			}

			if ( Ventilation( j ).ModelType == VentilationDesignFlowRate ) {
				// CR6845 if calculated < 0, don't propagate.
				VVF = Ventilation( j ).DesignLevel * GetCurrentScheduleValue( Ventilation( j ).SchedPtr );

				if ( Ventilation( j ).EMSSimpleVentOn ) VVF = Ventilation( j ).EMSimpleVentFlowRate;

				if ( VVF < 0.0 ) VVF = 0.0;
				VentMCP( j ) = VVF * AirDensity * CpAir * ( Ventilation( j ).ConstantTermCoef + std::abs( TempExt - ZMAT( NZ ) ) * Ventilation( j ).TemperatureTermCoef + WindExt * ( Ventilation( j ).VelocityTermCoef + WindExt * Ventilation( j ).VelocitySQTermCoef ) );
				if ( VentMCP( j ) < 0.0 ) VentMCP( j ) = 0.0;
				VAMFL_temp = VentMCP( j ) / CpAir;
				if ( Ventilation( j ).QuadratureSum ) {
					{ auto const SELECT_CASE_var( Ventilation( j ).FanType ); // ventilation type based calculation
					if ( SELECT_CASE_var == ExhaustVentilation ) {
						ZoneAirBalance( Ventilation( j ).OABalancePtr ).ExhMassFlowRate += VentMCP( j ) / CpAir;
					} else if ( SELECT_CASE_var == IntakeVentilation ) {
						ZoneAirBalance( Ventilation( j ).OABalancePtr ).IntMassFlowRate += VentMCP( j ) / CpAir;
					} else if ( SELECT_CASE_var == NaturalVentilation ) {
						ZoneAirBalance( Ventilation( j ).OABalancePtr ).NatMassFlowRate += VentMCP( j ) / CpAir;
					} else if ( SELECT_CASE_var == BalancedVentilation ) {
						ZoneAirBalance( Ventilation( j ).OABalancePtr ).BalMassFlowRate += VentMCP( j ) / CpAir;
					}}
				} else {
					MCPV( NZ ) += VentMCP( j );
					VAMFL( NZ ) += VAMFL_temp;
				}
				if ( Ventilation( j ).FanEfficiency > 0.0 ) {
					Ventilation( j ).FanPower = VAMFL_temp * Ventilation( j ).FanPressure / ( Ventilation( j ).FanEfficiency * AirDensity );
					if ( Ventilation( j ).FanType == BalancedVentilation ) Ventilation( j ).FanPower *= 2.0;
					// calc electric
					if ( SimulateAirflowNetwork == AirflowNetworkControlSimpleADS ) {
						// CR7608 IF (.not. TurnFansOn .or. .not. AirflowNetworkZoneFlag(NZ)) &
						if ( ! KickOffSimulation ) {
							if ( ! ( ZoneEquipAvail( NZ ) == CycleOn || ZoneEquipAvail( NZ ) == CycleOnZoneFansOnly ) || ! AirflowNetworkZoneFlag( NZ ) ) ZnAirRpt( NZ ).VentilFanElec += Ventilation( j ).FanPower * TimeStepSys * SecInHour;
						} else if ( ! AirflowNetworkZoneFlag( NZ ) ) {
							ZnAirRpt( NZ ).VentilFanElec += Ventilation( j ).FanPower * TimeStepSys * SecInHour;
						}
					} else {
						ZnAirRpt( NZ ).VentilFanElec += Ventilation( j ).FanPower * TimeStepSys * SecInHour;
					}
				}
				// Intake fans will add some heat to the air, raising the temperature for an intake fan...
				if ( Ventilation( j ).FanType == IntakeVentilation || Ventilation( j ).FanType == BalancedVentilation ) {
					if ( VAMFL_temp == 0.0 ) {
						OutletAirEnthalpy = OutEnthalpy;
					} else {
						if ( Ventilation( j ).FanPower > 0.0 ) {
							if ( Ventilation( j ).FanType == BalancedVentilation ) {
								OutletAirEnthalpy = OutEnthalpy + Ventilation( j ).FanPower / VAMFL_temp / 2.0; // Half fan power to calculate inlet T
							} else {
								OutletAirEnthalpy = OutEnthalpy + Ventilation( j ).FanPower / VAMFL_temp;
							}
						} else {
							OutletAirEnthalpy = OutEnthalpy;
						}
					}
					Ventilation( j ).AirTemp = PsyTdbFnHW( OutletAirEnthalpy, OutHumRat );
				} else {
					Ventilation( j ).AirTemp = TempExt;
				}
				if ( ! Ventilation( j ).QuadratureSum ) MCPTV( NZ ) += VentMCP( j ) * Ventilation( j ).AirTemp;
			}

			if ( Ventilation( j ).ModelType == VentilationWindAndStack ) {
				if ( Ventilation( j ).OpenEff != AutoCalculate ) {
					Cw = Ventilation( j ).OpenEff;
				} else {
					// linear interpolation between effective angle and wind direction
					angle = std::abs( WindDir - Ventilation( j ).EffAngle );
					if ( angle > 180.0 ) angle -= 180.0;
					Cw = 0.55 + angle / 180.0 * ( 0.3 - 0.55 );
				}
				if ( Ventilation( j ).DiscCoef != AutoCalculate ) {
					Cd = Ventilation( j ).DiscCoef;
				} else {
					Cd = 0.40 + 0.0045 * std::abs( TempExt - ZMAT( NZ ) );
				}
				Qw = Cw * Ventilation( j ).OpenArea * GetCurrentScheduleValue( Ventilation( j ).OpenAreaSchedPtr ) * WindExt;
				Qst = Cd * Ventilation( j ).OpenArea * GetCurrentScheduleValue( Ventilation( j ).OpenAreaSchedPtr ) * std::sqrt( 2.0 * 9.81 * Ventilation( j ).DH * std::abs( TempExt - ZMAT( NZ ) ) / ( ZMAT( NZ ) + 273.15 ) );
				VVF = std::sqrt( Qw * Qw + Qst * Qst );
				if ( Ventilation( j ).EMSSimpleVentOn ) VVF = Ventilation( j ).EMSimpleVentFlowRate;
				if ( VVF < 0.0 ) VVF = 0.0;
				VentMCP( j ) = VVF * AirDensity * CpAir;
				if ( VentMCP( j ) < 0.0 ) VentMCP( j ) = 0.0;
				if ( Ventilation( j ).QuadratureSum ) {
					ZoneAirBalance( Ventilation( j ).OABalancePtr ).NatMassFlowRate += VentMCP( j ) / CpAir;
				} else {
					MCPV( NZ ) += VentMCP( j );
					VAMFL_temp = VentMCP( j ) / CpAir;
					VAMFL( NZ ) += VAMFL_temp;
					Ventilation( j ).AirTemp = TempExt;
					MCPTV( NZ ) += VentMCP( j ) * Ventilation( j ).AirTemp;
				}
			}
		}

		// Process Mixing
		for ( j = 1; j <= TotMixing; ++j ) {
			n = Mixing( j ).ZonePtr;
			m = Mixing( j ).FromZone;
			TD = Mixing( j ).DeltaTemperature;
			// Get scheduled delta temperature
			if ( Mixing( j ).DeltaTempSchedPtr > 0 ) {
				TD = GetCurrentScheduleValue( Mixing( j ).DeltaTempSchedPtr );
			}
			TZN = ZMAT( n );
			TZM = ZMAT( m );

			// Hybrid ventilation controls
			if ( Mixing( j ).HybridControlType == HybridControlTypeClose ) continue;
			// Check temperature limit
			MixingLimitFlag = false;

			// Hybrid ventilation global control
			if ( Mixing( j ).HybridControlType == HybridControlTypeGlobal && Mixing( j ).HybridControlMasterNum > 0 ) {
				I = Mixing( j ).HybridControlMasterNum;
				if ( ! Ventilation( I ).HybridControlMasterStatus ) continue;
			} else {
				// Ensure the minimum indoor temperature <= the maximum indoor temperature
				if ( Mixing( j ).MinIndoorTempSchedPtr > 0 ) MixingTmin = GetCurrentScheduleValue( Mixing( j ).MinIndoorTempSchedPtr );
				if ( Mixing( j ).MaxIndoorTempSchedPtr > 0 ) MixingTmax = GetCurrentScheduleValue( Mixing( j ).MaxIndoorTempSchedPtr );
				if ( Mixing( j ).MinIndoorTempSchedPtr > 0 && Mixing( j ).MaxIndoorTempSchedPtr > 0 ) {
					if ( MixingTmin > MixingTmax ) {
						++Mixing( j ).IndoorTempErrCount;
						if ( Mixing( j ).IndoorTempErrCount < 2 ) {
							ShowWarningError( "Mixing zone temperature control: The minimum zone temperature is above " "the maximum zone temperature in " + Mixing( j ).Name );
							ShowContinueError( "The minimum zone temperature is set to the maximum zone temperature. " "Simulation continues." );
							ShowContinueErrorTimeStamp( " Occurrence info:" );
						} else {
							ShowRecurringWarningErrorAtEnd( "The minimum zone temperature is still above " "the maximum zone temperature", Mixing( j ).IndoorTempErrIndex, MixingTmin, MixingTmin );
						}
						MixingTmin = MixingTmax;
					}
				}
				if ( Mixing( j ).MinIndoorTempSchedPtr > 0 ) {
					if ( TZN < MixingTmin ) MixingLimitFlag = true;
				}
				if ( Mixing( j ).MaxIndoorTempSchedPtr > 0 ) {
					if ( TZN > MixingTmax ) MixingLimitFlag = true;
				}
				// Ensure the minimum source temperature <= the maximum source temperature
				if ( Mixing( j ).MinSourceTempSchedPtr > 0 ) MixingTmin = GetCurrentScheduleValue( Mixing( j ).MinSourceTempSchedPtr );
				if ( Mixing( j ).MaxSourceTempSchedPtr > 0 ) MixingTmax = GetCurrentScheduleValue( Mixing( j ).MaxSourceTempSchedPtr );
				if ( Mixing( j ).MinSourceTempSchedPtr > 0 && Mixing( j ).MaxSourceTempSchedPtr > 0 ) {
					if ( MixingTmin > MixingTmax ) {
						++Mixing( j ).SourceTempErrCount;
						if ( Mixing( j ).SourceTempErrCount < 2 ) {
							ShowWarningError( "Mixing source temperature control: The minimum source temperature is above " "the maximum source temperature in " + Mixing( j ).Name );
							ShowContinueError( "The minimum source temperature is set to the maximum source temperature. " "Simulation continues." );
							ShowContinueErrorTimeStamp( " Occurrence info:" );
						} else {
							ShowRecurringWarningErrorAtEnd( "The minimum source temperature is still above " "the maximum source temperature", Mixing( j ).SourceTempErrIndex, MixingTmin, MixingTmin );
						}
						MixingTmin = MixingTmax;
					}
				}
				if ( Mixing( j ).MinSourceTempSchedPtr > 0 ) {
					if ( TZM < MixingTmin ) MixingLimitFlag = true;
				}
				if ( Mixing( j ).MaxSourceTempSchedPtr > 0 ) {
					if ( TZM > MixingTmax ) MixingLimitFlag = true;
				}
				// Ensure the minimum outdoor temperature <= the maximum outdoor temperature
				TempExt = Zone( n ).OutDryBulbTemp;
				if ( Mixing( j ).MinOutdoorTempSchedPtr > 0 ) MixingTmin = GetCurrentScheduleValue( Mixing( j ).MinOutdoorTempSchedPtr );
				if ( Mixing( j ).MaxOutdoorTempSchedPtr > 0 ) MixingTmax = GetCurrentScheduleValue( Mixing( j ).MaxOutdoorTempSchedPtr );
				if ( Mixing( j ).MinOutdoorTempSchedPtr > 0 && Mixing( j ).MaxOutdoorTempSchedPtr > 0 ) {
					if ( MixingTmin > MixingTmax ) {
						++Mixing( j ).OutdoorTempErrCount;
						if ( Mixing( j ).OutdoorTempErrCount < 2 ) {
							ShowWarningError( "Mixing outdoor temperature control: The minimum outdoor temperature is above " "the maximum outdoor temperature in " + Mixing( j ).Name );
							ShowContinueError( "The minimum outdoor temperature is set to the maximum source temperature. " "Simulation continues." );
							ShowContinueErrorTimeStamp( " Occurrence info:" );
						} else {
							ShowRecurringWarningErrorAtEnd( "The minimum outdoor temperature is still above " "the maximum outdoor temperature", Mixing( j ).OutdoorTempErrIndex, MixingTmin, MixingTmin );
						}
						MixingTmin = MixingTmax;
					}
				}
				if ( Mixing( j ).MinOutdoorTempSchedPtr > 0 ) {
					if ( TempExt < MixingTmin ) MixingLimitFlag = true;
				}
				if ( Mixing( j ).MaxOutdoorTempSchedPtr > 0 ) {
					if ( TempExt > MixingTmax ) MixingLimitFlag = true;
				}
			}

			if ( Mixing( j ).HybridControlType != HybridControlTypeGlobal && MixingLimitFlag ) continue;
			if ( Mixing( j ).HybridControlType == HybridControlTypeGlobal ) TD = 0.0;

			//  If TD equals zero (default) set coefficients for full mixing otherwise test
			//    for mixing conditions if user input delta temp > 0, then from zone temp (TZM)
			//    must be td degrees warmer than zone temp (TZN).  If user input delta temp < 0,
			//    then from zone temp (TZM) must be TD degrees cooler than zone temp (TZN).
			if ( TD < 0.0 ) {
				if ( TZM < TZN + TD ) {
					//            Per Jan 17, 2008 conference call, agreed to use average conditions for Rho, Cp and Hfg
					//             RhoAirM = PsyRhoAirFnPbTdbW(OutBaroPress,tzm,ZHumRat(m))
					//             MCP=Mixing(J)%DesiredAirFlowRate * PsyCpAirFnWTdb(ZHumRat(m),tzm) * RhoAirM
					AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, ( TZN + TZM ) / 2.0, ( ZHumRat( n ) + ZHumRat( m ) ) / 2.0 );
					CpAir = PsyCpAirFnWTdb( ( ZHumRat( n ) + ZHumRat( m ) ) / 2.0, ( TZN + TZM ) / 2.0 ); // Use average conditions
					MCP = Mixing( j ).DesiredAirFlowRate * CpAir * AirDensity;
					MCPM( n ) += MCP;
					MCPTM( n ) += MCP * TZM;

					// Now to determine the moisture conditions
					MixingMassFlowZone( n ) += Mixing( j ).DesiredAirFlowRate * AirDensity;
					MixingMassFlowXHumRat( n ) += Mixing( j ).DesiredAirFlowRate * AirDensity * ZHumRat( m );
					if ( Contaminant.CO2Simulation ) {
						MixingMassFlowCO2( n ) += Mixing( j ).DesiredAirFlowRate * AirDensity * ZoneAirCO2( m );
					}
					if ( Contaminant.GenericContamSimulation ) {
						MixingMassFlowGC( n ) += Mixing( j ).DesiredAirFlowRate * AirDensity * ZoneAirGC( m );
					}
					MixingReportFlag( j ) = true;
				}
			}
			if ( TD > 0.0 ) {
				if ( TZM > TZN + TD ) {
					//             RhoAirM = PsyRhoAirFnPbTdbW(OutBaroPress,tzm,ZHumRat(m))
					//             MCP=Mixing(J)%DesiredAirFlowRate * PsyCpAirFnWTdb(ZHumRat(m),tzm) * RhoAirM
					AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, ( TZN + TZM ) / 2.0, ( ZHumRat( n ) + ZHumRat( m ) ) / 2.0 ); // Use avg conditions
					CpAir = PsyCpAirFnWTdb( ( ZHumRat( n ) + ZHumRat( m ) ) / 2.0, ( TZN + TZM ) / 2.0 ); // Use average conditions
					MCP = Mixing( j ).DesiredAirFlowRate * CpAir * AirDensity;
					MCPM( n ) += MCP;
					MCPTM( n ) += MCP * TZM;
					// Now to determine the moisture conditions
					MixingMassFlowZone( n ) += Mixing( j ).DesiredAirFlowRate * AirDensity;
					MixingMassFlowXHumRat( n ) += Mixing( j ).DesiredAirFlowRate * AirDensity * ZHumRat( m );
					if ( Contaminant.CO2Simulation ) {
						MixingMassFlowCO2( n ) += Mixing( j ).DesiredAirFlowRate * AirDensity * ZoneAirCO2( m );
					}
					if ( Contaminant.GenericContamSimulation ) {
						MixingMassFlowGC( n ) += Mixing( j ).DesiredAirFlowRate * AirDensity * ZoneAirGC( m );
					}
					MixingReportFlag( j ) = true;
				}
			}
			if ( TD == 0.0 ) {
				//          RhoAirM = PsyRhoAirFnPbTdbW(OutBaroPress,tzm,ZHumRat(m))
				//          MCP=Mixing(J)%DesiredAirFlowRate * PsyCpAirFnWTdb(ZHumRat(m),tzm) * RhoAirM
				AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, ( TZN + TZM ) / 2.0, ( ZHumRat( n ) + ZHumRat( m ) ) / 2.0, "CalcAirFlowSimple:Mixing" ); // Use avg conditions
				CpAir = PsyCpAirFnWTdb( ( ZHumRat( n ) + ZHumRat( m ) ) / 2.0, ( TZN + TZM ) / 2.0, "CalcAirFlowSimple:Mixing" ); // Use average conditions
				MCP = Mixing( j ).DesiredAirFlowRate * CpAir * AirDensity;
				MCPM( n ) += MCP;
				MCPTM( n ) += MCP * TZM;
				// Now to determine the moisture conditions
				MixingMassFlowZone( n ) += Mixing( j ).DesiredAirFlowRate * AirDensity;
				MixingMassFlowXHumRat( n ) += Mixing( j ).DesiredAirFlowRate * AirDensity * ZHumRat( m );
				if ( Contaminant.CO2Simulation ) {
					MixingMassFlowCO2( n ) += Mixing( j ).DesiredAirFlowRate * AirDensity * ZoneAirCO2( m );
				}
				if ( Contaminant.GenericContamSimulation ) {
					MixingMassFlowGC( n ) += Mixing( j ).DesiredAirFlowRate * AirDensity * ZoneAirGC( m );
				}
				MixingReportFlag( j ) = true;
			}
		}

		//                              COMPUTE CROSS ZONE
		//                              AIR MIXING
		for ( j = 1; j <= TotCrossMixing; ++j ) {
			n = CrossMixing( j ).ZonePtr;
			m = CrossMixing( j ).FromZone;
			TD = MTC( j );
			// Get scheduled delta temperature
			if ( CrossMixing( j ).DeltaTempSchedPtr > 0 ) {
				TD = GetCurrentScheduleValue( CrossMixing( j ).DeltaTempSchedPtr );
			}

			if ( TD >= 0.0 ) {
				TZN = ZMAT( n );
				TZM = ZMAT( m );
				// Check temperature limit
				MixingLimitFlag = false;
				// Ensure the minimum indoor temperature <= the maximum indoor temperature
				if ( CrossMixing( j ).MinIndoorTempSchedPtr > 0 ) MixingTmin = GetCurrentScheduleValue( CrossMixing( j ).MinIndoorTempSchedPtr );
				if ( CrossMixing( j ).MaxIndoorTempSchedPtr > 0 ) MixingTmax = GetCurrentScheduleValue( CrossMixing( j ).MaxIndoorTempSchedPtr );
				if ( CrossMixing( j ).MinIndoorTempSchedPtr > 0 && CrossMixing( j ).MaxIndoorTempSchedPtr > 0 ) {
					if ( MixingTmin > MixingTmax ) {
						++CrossMixing( j ).IndoorTempErrCount;
						if ( CrossMixing( j ).IndoorTempErrCount < 2 ) {
							ShowWarningError( "CrossMixing zone temperature control: The minimum zone temperature is above " "the maximum zone temperature in " + CrossMixing( j ).Name );
							ShowContinueError( "The minimum zone temperature is set to the maximum zone temperature. " "Simulation continues." );
							ShowContinueErrorTimeStamp( " Occurrence info:" );
						} else {
							ShowRecurringWarningErrorAtEnd( "The minimum zone temperature is still above " "the maximum zone temperature", CrossMixing( j ).IndoorTempErrIndex, MixingTmin, MixingTmin );
						}
						MixingTmin = MixingTmax;
					}
				}
				if ( CrossMixing( j ).MinIndoorTempSchedPtr > 0 ) {
					if ( TZN < MixingTmin ) MixingLimitFlag = true;
				}
				if ( CrossMixing( j ).MaxIndoorTempSchedPtr > 0 ) {
					if ( TZN > MixingTmax ) MixingLimitFlag = true;
				}
				// Ensure the minimum source temperature <= the maximum source temperature
				if ( CrossMixing( j ).MinSourceTempSchedPtr > 0 ) MixingTmin = GetCurrentScheduleValue( CrossMixing( j ).MinSourceTempSchedPtr );
				if ( CrossMixing( j ).MaxSourceTempSchedPtr > 0 ) MixingTmax = GetCurrentScheduleValue( CrossMixing( j ).MaxSourceTempSchedPtr );
				if ( CrossMixing( j ).MinSourceTempSchedPtr > 0 && CrossMixing( j ).MaxSourceTempSchedPtr > 0 ) {
					if ( MixingTmin > MixingTmax ) {
						++CrossMixing( j ).SourceTempErrCount;
						if ( CrossMixing( j ).SourceTempErrCount < 2 ) {
							ShowWarningError( "CrossMixing source temperature control: The minimum source temperature is above " "the maximum source temperature in " + CrossMixing( j ).Name );
							ShowContinueError( "The minimum source temperature is set to the maximum source temperature. " "Simulation continues." );
							ShowContinueErrorTimeStamp( " Occurrence info:" );
						} else {
							ShowRecurringWarningErrorAtEnd( "The minimum source temperature is still above " "the maximum source temperature", CrossMixing( j ).SourceTempErrIndex, MixingTmin, MixingTmin );
						}
						MixingTmin = MixingTmax;
					}
				}
				if ( CrossMixing( j ).MinSourceTempSchedPtr > 0 ) {
					if ( TZM < MixingTmin ) MixingLimitFlag = true;
				}
				if ( CrossMixing( j ).MaxSourceTempSchedPtr > 0 ) {
					if ( TZM > MixingTmax ) MixingLimitFlag = true;
				}
				// Ensure the minimum outdoor temperature <= the maximum outdoor temperature
				TempExt = Zone( n ).OutDryBulbTemp;
				if ( CrossMixing( j ).MinOutdoorTempSchedPtr > 0 ) MixingTmin = GetCurrentScheduleValue( CrossMixing( j ).MinOutdoorTempSchedPtr );
				if ( CrossMixing( j ).MaxOutdoorTempSchedPtr > 0 ) MixingTmax = GetCurrentScheduleValue( CrossMixing( j ).MaxOutdoorTempSchedPtr );
				if ( CrossMixing( j ).MinOutdoorTempSchedPtr > 0 && CrossMixing( j ).MaxOutdoorTempSchedPtr > 0 ) {
					if ( MixingTmin > MixingTmax ) {
						++CrossMixing( j ).OutdoorTempErrCount;
						if ( CrossMixing( j ).OutdoorTempErrCount < 2 ) {
							ShowWarningError( "CrossMixing outdoor temperature control: The minimum outdoor temperature is above " "the maximum outdoor temperature in " + Mixing( j ).Name );
							ShowContinueError( "The minimum outdoor temperature is set to the maximum source temperature. " "Simulation continues." );
							ShowContinueErrorTimeStamp( " Occurrence info:" );
						} else {
							ShowRecurringWarningErrorAtEnd( "The minimum outdoor temperature is still above " "the maximum outdoor temperature", CrossMixing( j ).OutdoorTempErrIndex, MixingTmin, MixingTmin );
						}
						MixingTmin = MixingTmax;
					}
				}
				if ( CrossMixing( j ).MinOutdoorTempSchedPtr > 0 ) {
					if ( TempExt < MixingTmin ) MixingLimitFlag = true;
				}
				if ( CrossMixing( j ).MaxOutdoorTempSchedPtr > 0 ) {
					if ( TempExt > MixingTmax ) MixingLimitFlag = true;
				}
				if ( MixingLimitFlag ) continue;

				if ( ( TD == 0.0 || ( TD > 0.0 && ( TZM - TZN ) >= TD ) ) ) {
					CrossMixingReportFlag( j ) = true; // set reporting flag
				}

				if ( ( ( TD <= 0.0 ) && ( ! CrossMixingFlag( n ) && ! CrossMixingFlag( m ) ) ) || ( ( TD > 0.0 ) && ( TZM - TZN >= TD ) ) ) {
					//                                      SET COEFFICIENTS .
					CrossMixingFlag( n ) = true;
					CrossMixingFlag( m ) = true;

					Tavg = ( TZN + TZM ) / 2.0;
					Wavg = ( ZHumRat( n ) + ZHumRat( m ) ) / 2.0;
					AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, Tavg, Wavg, "CalcAirFlowSimple:CrossMixing" );
					CpAir = PsyCpAirFnWTdb( Wavg, Tavg, "CalcAirFlowSimple:CrossMixing" );
					MCPxN = MVFC( j ) * CpAir * AirDensity;
					MCPM( n ) += MCPxN;

					MCPxM = MVFC( j ) * CpAir * AirDensity;
					MCPM( m ) += MCPxM;
					MCPTM( n ) += MCPxM * TZM;
					MCPTM( m ) += MCPxN * TZN;

					// Now to determine the moisture conditions
					MixingMassFlowZone( m ) += MVFC( j ) * AirDensity;
					MixingMassFlowXHumRat( m ) += MVFC( j ) * AirDensity * ZHumRat( n );

					MixingMassFlowZone( n ) += MVFC( j ) * AirDensity;
					MixingMassFlowXHumRat( n ) += MVFC( j ) * AirDensity * ZHumRat( m );
					if ( Contaminant.CO2Simulation ) {
						MixingMassFlowCO2( m ) += MVFC( j ) * AirDensity * ZoneAirCO2( n );
						MixingMassFlowCO2( n ) += MVFC( j ) * AirDensity * ZoneAirCO2( m );
					}
					if ( Contaminant.GenericContamSimulation ) {
						MixingMassFlowGC( m ) += MVFC( j ) * AirDensity * ZoneAirGC( n );
						MixingMassFlowGC( n ) += MVFC( j ) * AirDensity * ZoneAirGC( m );
					}
				}
			}
		}

		//                              COMPUTE REFRIGERATION DOOR
		//                              AIR MIXING
		if ( TotRefDoorMixing > 0 ) {
			//Zone loops structured in getinput so only do each pair of zones bounding door once, even if multiple doors in one zone
			for ( ZoneA = 1; ZoneA <= ( NumOfZones - 1 ); ++ZoneA ) {
				if ( ! RefDoorMixing( ZoneA ).RefDoorMixFlag ) continue;
				for ( j = 1; j <= RefDoorMixing( ZoneA ).NumRefDoorConnections; ++j ) {
					ZoneB = RefDoorMixing( ZoneA ).MateZonePtr( j );
					TZoneA = ZMAT( ZoneA );
					TZoneB = ZMAT( ZoneB );
					HumRatZoneA = ZHumRat( ZoneA );
					HumRatZoneB = ZHumRat( ZoneB );
					AirDensityZoneA = PsyRhoAirFnPbTdbW( OutBaroPress, TZoneA, HumRatZoneA, "CalcAirFlowSimple:RefrigerationDoorMixing" );
					CpAirZoneA = PsyCpAirFnWTdb( HumRatZoneA, TZoneA );
					AirDensityZoneB = PsyRhoAirFnPbTdbW( OutBaroPress, TZoneB, HumRatZoneB, "CalcAirFlowSimple:RefrigerationDoorMixing" );
					CpAirZoneB = PsyCpAirFnWTdb( HumRatZoneB, TZoneB, "CalcAirFlowSimple:RefrigerationDoorMixing" );
					Tavg = ( TZoneA + TZoneB ) / 2.0;
					Wavg = ( HumRatZoneA + HumRatZoneB ) / 2.0;
					AirDensityAvg = PsyRhoAirFnPbTdbW( OutBaroPress, Tavg, Wavg, "CalcAirFlowSimple:RefrigerationDoorMixing" );

					if ( RefDoorMixing( ZoneA ).EMSRefDoorMixingOn( j ) ) {
						MassFlowDryAir = RefDoorMixing( ZoneA ).VolRefDoorFlowRate( j ) * AirDensityAvg;
					} else {
						SchedDoorOpen = GetCurrentScheduleValue( RefDoorMixing( ZoneA ).OpenSchedPtr( j ) );
						if ( SchedDoorOpen == 0.0 ) continue;
						DoorHeight = RefDoorMixing( ZoneA ).DoorHeight( j );
						DoorArea = RefDoorMixing( ZoneA ).DoorArea( j );
						DoorProt = RefDoorMixing( ZoneA ).Protection( j );
						if ( AirDensityZoneA >= AirDensityZoneB ) {
							// Mass of dry air flow between zones is equal,
							// but have to calc directionally to avoid sqrt(neg number)
							FDens = std::pow( ( 2.0 / ( 1.0 + ( std::pow( ( AirDensityZoneA / AirDensityZoneB ), ( 1.0 / 3.0 ) ) ) ) ), 1.5 );
							Fb = 0.221 * DoorArea * AirDensityZoneA * FDens * std::sqrt( ( 1.0 - AirDensityZoneB / AirDensityZoneA ) * StdGravity * DoorHeight );
						} else { //ZoneADens < ZoneBDens
							FDens = std::pow( ( 2.0 / ( 1.0 + ( std::pow( ( AirDensityZoneB / AirDensityZoneA ), ( 1.0 / 3.0 ) ) ) ) ), 1.5 );
							Fb = 0.221 * DoorArea * AirDensityZoneB * FDens * std::sqrt( ( 1.0 - AirDensityZoneA / AirDensityZoneB ) * StdGravity * DoorHeight );
						} //ZoneADens .GE. ZoneBDens
						// FFlow = Doorway flow factor, is determined by temperature difference
						FFlow = 1.1;
						if ( std::abs( TZoneA - TZoneB ) > 11.0 ) FFlow = 0.8;
						MassFlowDryAir = Fb * SchedDoorOpen * FFlow * ( 1.0 - DoorProt );
						RefDoorMixing( ZoneA ).VolRefDoorFlowRate( j ) = MassFlowDryAir / AirDensityAvg;
						//Note - VolRefDoorFlowRate is used ONLY for reporting purposes, where it is
						//       used with the avg density to generate a reported mass flow
						//       Considering the small values typical for HumRat, this is not far off.
					} // EMSRefDoorMixingOn

					MassFlowToA = MassFlowDryAir * ( 1.0 + HumRatZoneB );
					MassFlowToB = MassFlowDryAir * ( 1.0 + HumRatZoneA );
					MassFlowXCpToA = MassFlowToA * CpAirZoneB;
					MassFlowXCpToB = MassFlowToB * CpAirZoneA;
					MassFlowXCpXTempToA = MassFlowXCpToA * TZoneB;
					MassFlowXCpXTempToB = MassFlowXCpToB * TZoneA;
					MassFlowXHumRatToA = MassFlowToA * HumRatZoneB;
					MassFlowXHumRatToB = MassFlowToB * HumRatZoneA;

					MCPM( ZoneA ) += MassFlowXCpToA;
					MCPM( ZoneB ) += MassFlowXCpToB;
					MCPTM( ZoneA ) += MassFlowXCpXTempToA;
					MCPTM( ZoneB ) += MassFlowXCpXTempToB;

					// Now to determine the moisture conditions
					MixingMassFlowZone( ZoneA ) += MassFlowToA;
					MixingMassFlowZone( ZoneB ) += MassFlowToB;
					MixingMassFlowXHumRat( ZoneA ) += MassFlowXHumRatToA;
					MixingMassFlowXHumRat( ZoneB ) += MassFlowXHumRatToB;

					// Now to determine the CO2 and generic contaminant conditions
					if ( Contaminant.CO2Simulation ) {
						MixingMassFlowCO2( ZoneA ) += MassFlowToA * ZoneAirCO2( ZoneB );
						MixingMassFlowCO2( ZoneB ) += MassFlowToB * ZoneAirCO2( ZoneA );
					}
					if ( Contaminant.GenericContamSimulation ) {
						MixingMassFlowCO2( ZoneA ) += MassFlowToA * ZoneAirGC( ZoneB );
						MixingMassFlowCO2( ZoneB ) += MassFlowToB * ZoneAirGC( ZoneA );
					}

				} // J=1,RefDoorMixing(ZoneA)%NumRefDoorConnections
			} //ZoneA=1,(NumOfZones - 1)
		} //(TotRefrigerationDoorMixing > 0) THEN

		// Process the scheduled Infiltration for air heat balance depending on model type
		for ( j = 1; j <= TotInfiltration; ++j ) {

			NZ = Infiltration( j ).ZonePtr;

			TempExt = Zone( NZ ).OutDryBulbTemp;
			WindExt = Zone( NZ ).WindSpeed;
			AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, TempExt, OutHumRat, "CalcAirFlowSimple:Infiltration" );
			CpAir = PsyCpAirFnWTdb( OutHumRat, TempExt );
			//CR7751  should maybe use code below, indoor conditions instead of outdoor conditions
			//   AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress, ZMAT(NZ), ZHumRat(NZ))
			//   CpAir = PsyCpAirFnWTdb(ZHumRat(NZ),ZMAT(NZ))
			{ auto const SELECT_CASE_var( Infiltration( j ).ModelType );

			if ( SELECT_CASE_var == InfiltrationDesignFlowRate ) {

				IVF = Infiltration( j ).DesignLevel * GetCurrentScheduleValue( Infiltration( j ).SchedPtr );
				// CR6845 if calculated < 0.0, don't propagate
				if ( IVF < 0.0 ) IVF = 0.0;
				MCpI_temp = IVF * AirDensity * CpAir * ( Infiltration( j ).ConstantTermCoef + std::abs( TempExt - ZMAT( NZ ) ) * Infiltration( j ).TemperatureTermCoef + WindExt * ( Infiltration( j ).VelocityTermCoef + WindExt * Infiltration( j ).VelocitySQTermCoef ) );

				if ( MCpI_temp < 0.0 ) MCpI_temp = 0.0;
			} else if ( SELECT_CASE_var == InfiltrationShermanGrimsrud ) {
				// Sherman Grimsrud model as formulated in ASHRAE HoF
				WindExt = WindSpeed; // formulated to use wind at Meterological Station rather than local
				IVF = GetCurrentScheduleValue( Infiltration( j ).SchedPtr ) * Infiltration( j ).LeakageArea / 1000.0 * std::sqrt( Infiltration( j ).BasicStackCoefficient * std::abs( TempExt - ZMAT( NZ ) ) + Infiltration( j ).BasicWindCoefficient * std::pow( WindExt, 2 ) );
				if ( IVF < 0.0 ) IVF = 0.0;
				MCpI_temp = IVF * AirDensity * CpAir;
				if ( MCpI_temp < 0.0 ) MCpI_temp = 0.0;
			} else if ( SELECT_CASE_var == InfiltrationAIM2 ) {
				// Walker Wilson model as formulated in ASHRAE HoF
				IVF = GetCurrentScheduleValue( Infiltration( j ).SchedPtr ) * std::sqrt( std::pow( ( Infiltration( j ).FlowCoefficient * Infiltration( j ).AIM2StackCoefficient * std::pow( ( std::abs( TempExt - ZMAT( NZ ) ) ), Infiltration( j ).PressureExponent ) ), 2 ) + std::pow( ( Infiltration( j ).FlowCoefficient * Infiltration( j ).AIM2WindCoefficient * std::pow( ( Infiltration( j ).ShelterFactor * WindExt ), ( 2.0 * Infiltration( j ).PressureExponent ) ) ), 2 ) );
				if ( IVF < 0.0 ) IVF = 0.0;
				MCpI_temp = IVF * AirDensity * CpAir;
				if ( MCpI_temp < 0.0 ) MCpI_temp = 0.0;
			}}

			if ( Infiltration( j ).EMSOverrideOn ) {
				IVF = Infiltration( j ).EMSAirFlowRateValue;
				if ( IVF < 0.0 ) IVF = 0.0;
				MCpI_temp = IVF * AirDensity * CpAir;
				if ( MCpI_temp < 0.0 ) MCpI_temp = 0.0;
			}

			if ( Infiltration( j ).QuadratureSum ) {
				ZoneAirBalance( Infiltration( j ).OABalancePtr ).InfMassFlowRate += MCpI_temp / CpAir;
			} else {
				MCPI( NZ ) += MCpI_temp;
				OAMFL( NZ ) += MCpI_temp / CpAir;
				MCPTI( NZ ) += MCpI_temp * TempExt;
			}

		}

		// Add infiltration rate enhanced by the existence of thermal chimney
		for ( NZ = 1; NZ <= NumOfZones; ++NZ ) {
			MCPI( NZ ) += MCPThermChim( NZ );
			OAMFL( NZ ) += ThermChimAMFL( NZ );
			MCPTI( NZ ) += MCPTThermChim( NZ );
		}

		// Calculate combined outdoor air flows
		for ( j = 1; j <= TotZoneAirBalance; ++j ) {
			if ( ZoneAirBalance( j ).BalanceMethod == AirBalanceQuadrature ) {
				if ( ! ZoneAirBalance( j ).OneTimeFlag ) GetStandAloneERVNodes( j );
				if ( ZoneAirBalance( j ).NumOfERVs > 0 ) {
					for ( I = 1; I <= ZoneAirBalance( j ).NumOfERVs; ++I ) {
						MassFlowDiff = Node( ZoneAirBalance( j ).ERVExhaustNode( I ) ).MassFlowRate - Node( ZoneAirBalance( j ).ERVInletNode( I ) ).MassFlowRate;
						if ( MassFlowDiff > 0.0 ) {
							ZoneAirBalance( j ).ERVMassFlowRate += MassFlowDiff;
						}
					}
				}
				NZ = ZoneAirBalance( j ).ZonePtr;
				AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, Zone( NZ ).OutDryBulbTemp, OutHumRat, "CalcAirFlowSimple:ZoneAirBalance" );
				CpAir = PsyCpAirFnWTdb( OutHumRat, Zone( NZ ).OutDryBulbTemp );
				ZoneAirBalance( j ).ERVMassFlowRate *= AirDensity;
				MDotOA( NZ ) = std::sqrt( std::pow( ( ZoneAirBalance( j ).NatMassFlowRate ), 2 ) + std::pow( ( ZoneAirBalance( j ).IntMassFlowRate ), 2 ) + std::pow( ( ZoneAirBalance( j ).ExhMassFlowRate ), 2 ) + std::pow( ( ZoneAirBalance( j ).ERVMassFlowRate ), 2 ) + std::pow( ( ZoneAirBalance( j ).InfMassFlowRate ), 2 ) + std::pow( ( AirDensity * ZoneAirBalance( j ).InducedAirRate * GetCurrentScheduleValue( ZoneAirBalance( j ).InducedAirSchedPtr ) ), 2 ) ) + ZoneAirBalance( j ).BalMassFlowRate;
				MDotCPOA( NZ ) = MDotOA( NZ ) * CpAir;
			}
		}

	}

	void
	ReportAirHeatBalance()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   July 2000
		//       MODIFIED       Shirey, Jan 2008 (MIXING/CROSS MIXING outputs)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates the report variables for the AirHeatBalance.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::SecInHour;
		using DataEnvironment::StdBaroPress;
		using DataEnvironment::OutBaroPress;
		using DataEnvironment::OutHumRat;
		using DataEnvironment::StdRhoAir;
		using DataHeatBalance::Zone;
		using DataHeatBalance::TotVentilation;
		using DataHeatBalance::Ventilation;
		using DataHeatBalance::ZnAirRpt;
		using DataHeatBalance::TotMixing;
		using DataHeatBalance::TotCrossMixing;
		using DataHeatBalance::Mixing;
		using DataHeatBalance::CrossMixing;
		using DataHeatBalance::MVFC;
		using DataHeatBalance::TotZoneAirBalance;
		using DataHeatBalance::ZoneAirBalance;
		using DataHeatBalance::AirBalanceQuadrature;
		using DataHeatBalance::TotRefDoorMixing;
		using DataHeatBalance::RefDoorMixing;
		using DataHVACGlobals::CycleOn;
		using DataHVACGlobals::CycleOnZoneFansOnly;
		using DataHeatBalFanSys::MCPI; // , MCPTI, MCPTV, MCPM, MCPTM, MixingMassFlowZone
		using DataHeatBalFanSys::MCPV;
		using DataHeatBalFanSys::MDotOA;
		using DataHeatBalFanSys::MDotCPOA;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyCpAirFnWTdb;
		using Psychrometrics::PsyHgAirFnWTdb;

		using AirflowNetworkBalanceManager::ReportAirflowNetwork;
		using DataAirflowNetwork::SimulateAirflowNetwork;
		using DataAirflowNetwork::AirflowNetworkZoneFlag;
		using DataAirflowNetwork::AirflowNetworkControlSimple;
		using DataAirflowNetwork::AirflowNetworkControlSimpleADS;
		using DataZoneEquipment::ZoneEquipAvail;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ZoneLoop; // Counter for the # of zones (nz)
		int ZoneA; // Mated zone number for pair pf zones sharing refrigeration door opening
		int ZoneB; // Mated zone number for pair pf zones sharing refrigeration door opening
		int VentNum; // Counter for ventilation statements
		Real64 AirDensity; // Density of air (kg/m^3)
		Real64 CpAir; // Heat capacity of air (J/kg-C)
		Real64 ADSCorrectionFactor; // Correction factor of air flow model values when ADS is simulated
		Real64 H2OHtOfVap; // Heat of vaporization of air
		Real64 TotalLoad; // Total loss or gain
		int MixNum; // Counter for MIXING and Cross Mixing statements
		static FArray1D< Real64 > MixSenLoad; // Mixing sensible loss or gain
		static FArray1D< Real64 > MixLatLoad; // Mixing latent loss or gain
		int j; // Index in a do-loop
		int VentZoneNum; // Number of ventilation object per zone
		Real64 VentZoneMassflow; // Total mass flow rate per zone
		Real64 VentZoneAirTemp; // Average Zone inlet temperature
		static bool firstTime( true );

		// Ensure no airflownetwork and simple calculations
		if ( SimulateAirflowNetwork == 0 ) return;

		if ( SimulateAirflowNetwork > AirflowNetworkControlSimple ) ReportAirflowNetwork();

		// Report results for SIMPLE option only
		if ( ! ( SimulateAirflowNetwork == AirflowNetworkControlSimple || SimulateAirflowNetwork == AirflowNetworkControlSimpleADS ) ) return;

		if ( firstTime ) {
			MixSenLoad.allocate( NumOfZones );
			MixLatLoad.allocate( NumOfZones );
			firstTime = false;
		}

		for ( ZoneLoop = 1; ZoneLoop <= NumOfZones; ++ZoneLoop ) { // Start of zone loads report variable update loop ...

			// Break the infiltration load into heat gain and loss components
			ADSCorrectionFactor = 1.0;

			if ( SimulateAirflowNetwork == AirflowNetworkControlSimpleADS ) {
				// CR7608 IF (TurnFansOn .AND. AirflowNetworkZoneFlag(ZoneLoop)) ADSCorrectionFactor=0
				if ( ( ZoneEquipAvail( ZoneLoop ) == CycleOn || ZoneEquipAvail( ZoneLoop ) == CycleOnZoneFansOnly ) && AirflowNetworkZoneFlag( ZoneLoop ) ) ADSCorrectionFactor = 0.0;
			}

			if ( MAT( ZoneLoop ) > Zone( ZoneLoop ).OutDryBulbTemp ) {

				ZnAirRpt( ZoneLoop ).InfilHeatLoss = 0.001 * MCPI( ZoneLoop ) * ( MAT( ZoneLoop ) - Zone( ZoneLoop ).OutDryBulbTemp ) * TimeStepSys * SecInHour * 1000.0 * ADSCorrectionFactor;
				ZnAirRpt( ZoneLoop ).InfilHeatGain = 0.0;

			} else if ( MAT( ZoneLoop ) <= Zone( ZoneLoop ).OutDryBulbTemp ) {

				ZnAirRpt( ZoneLoop ).InfilHeatGain = 0.001 * MCPI( ZoneLoop ) * ( Zone( ZoneLoop ).OutDryBulbTemp - MAT( ZoneLoop ) ) * TimeStepSys * SecInHour * 1000.0 * ADSCorrectionFactor;
				ZnAirRpt( ZoneLoop ).InfilHeatLoss = 0.0;

			}
			// Report infiltration latent gains and losses
			CpAir = PsyCpAirFnWTdb( OutHumRat, Zone( ZoneLoop ).OutDryBulbTemp, "ReportAirHeatBalance" );
			H2OHtOfVap = PsyHgAirFnWTdb( OutHumRat, Zone( ZoneLoop ).OutDryBulbTemp, "ReportAirHeatBalance:1" );
			if ( ZoneAirHumRat( ZoneLoop ) > OutHumRat ) {

				ZnAirRpt( ZoneLoop ).InfilLatentLoss = 0.001 * MCPI( ZoneLoop ) / CpAir * ( ZoneAirHumRat( ZoneLoop ) - OutHumRat ) * H2OHtOfVap * TimeStepSys * SecInHour * 1000.0 * ADSCorrectionFactor;
				ZnAirRpt( ZoneLoop ).InfilLatentGain = 0.0;

			} else if ( ZoneAirHumRat( ZoneLoop ) <= OutHumRat ) {

				ZnAirRpt( ZoneLoop ).InfilLatentGain = 0.001 * MCPI( ZoneLoop ) / CpAir * ( OutHumRat - ZoneAirHumRat( ZoneLoop ) ) * H2OHtOfVap * TimeStepSys * SecInHour * 1000.0 * ADSCorrectionFactor;
				ZnAirRpt( ZoneLoop ).InfilLatentLoss = 0.0;

			}
			// Total infiltration losses and gains
			TotalLoad = ZnAirRpt( ZoneLoop ).InfilHeatGain + ZnAirRpt( ZoneLoop ).InfilLatentGain - ZnAirRpt( ZoneLoop ).InfilHeatLoss - ZnAirRpt( ZoneLoop ).InfilLatentLoss;
			if ( TotalLoad > 0 ) {
				ZnAirRpt( ZoneLoop ).InfilTotalGain = TotalLoad * ADSCorrectionFactor;
				ZnAirRpt( ZoneLoop ).InfilTotalLoss = 0.0;
			} else {
				ZnAirRpt( ZoneLoop ).InfilTotalGain = 0.0;
				ZnAirRpt( ZoneLoop ).InfilTotalLoss = -TotalLoad * ADSCorrectionFactor;
			}

			// first calculate mass flows using outside air heat capacity for consistency with input to heat balance
			CpAir = PsyCpAirFnWTdb( OutHumRat, Zone( ZoneLoop ).OutDryBulbTemp, "ReportAirHeatBalance:2" );
			ZnAirRpt( ZoneLoop ).InfilMass = ( MCPI( ZoneLoop ) / CpAir ) * TimeStepSys * SecInHour * ADSCorrectionFactor;
			ZnAirRpt( ZoneLoop ).InfilMdot = ( MCPI( ZoneLoop ) / CpAir ) * ADSCorrectionFactor;
			ZnAirRpt( ZoneLoop ).VentilMass = ( MCPV( ZoneLoop ) / CpAir ) * TimeStepSys * SecInHour * ADSCorrectionFactor;
			ZnAirRpt( ZoneLoop ).VentilMdot = ( MCPV( ZoneLoop ) / CpAir ) * ADSCorrectionFactor;

			//CR7751  second, calculate using indoor conditions for density property
			AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, MAT( ZoneLoop ), ZoneAirHumRatAvg( ZoneLoop ), "ReportAirHeatBalance:3" );
			CpAir = PsyCpAirFnWTdb( ZoneAirHumRatAvg( ZoneLoop ), MAT( ZoneLoop ), "ReportAirHeatBalance:4" );
			ZnAirRpt( ZoneLoop ).InfilVolumeCurDensity = ( MCPI( ZoneLoop ) / CpAir / AirDensity ) * TimeStepSys * SecInHour * ADSCorrectionFactor;
			ZnAirRpt( ZoneLoop ).InfilAirChangeRate = ZnAirRpt( ZoneLoop ).InfilVolumeCurDensity / ( TimeStepSys * Zone( ZoneLoop ).Volume );
			ZnAirRpt( ZoneLoop ).InfilVdotCurDensity = ( MCPI( ZoneLoop ) / CpAir / AirDensity ) * ADSCorrectionFactor;
			ZnAirRpt( ZoneLoop ).VentilVolumeCurDensity = ( MCPV( ZoneLoop ) / CpAir / AirDensity ) * TimeStepSys * SecInHour * ADSCorrectionFactor;
			ZnAirRpt( ZoneLoop ).VentilAirChangeRate = ZnAirRpt( ZoneLoop ).VentilVolumeCurDensity / ( TimeStepSys * Zone( ZoneLoop ).Volume );
			ZnAirRpt( ZoneLoop ).VentilVdotCurDensity = ( MCPV( ZoneLoop ) / CpAir / AirDensity ) * ADSCorrectionFactor;

			//CR7751 third, calculate using standard dry air at nominal elevation
			AirDensity = StdRhoAir;
			ZnAirRpt( ZoneLoop ).InfilVolumeStdDensity = ( MCPI( ZoneLoop ) / CpAir / AirDensity ) * TimeStepSys * SecInHour * ADSCorrectionFactor;
			ZnAirRpt( ZoneLoop ).InfilVdotStdDensity = ( MCPI( ZoneLoop ) / CpAir / AirDensity ) * ADSCorrectionFactor;
			ZnAirRpt( ZoneLoop ).VentilVolumeStdDensity = ( MCPV( ZoneLoop ) / CpAir / AirDensity ) * TimeStepSys * SecInHour * ADSCorrectionFactor;
			ZnAirRpt( ZoneLoop ).VentilVdotStdDensity = ( MCPV( ZoneLoop ) / CpAir / AirDensity ) * ADSCorrectionFactor;

			//    ZnAirRpt(ZoneLoop)%VentilFanElec = 0.0
			ZnAirRpt( ZoneLoop ).VentilAirTemp = 0.0;
			ZnAirRpt( ZoneLoop ).VentilHeatLoss = 0.0;
			ZnAirRpt( ZoneLoop ).VentilHeatGain = 0.0;
			VentZoneNum = 0;
			VentZoneMassflow = 0.0;
			VentZoneAirTemp = 0.0;

			for ( VentNum = 1; VentNum <= TotVentilation; ++VentNum ) {
				if ( Ventilation( VentNum ).ZonePtr == ZoneLoop ) {
					// moved into CalcAirFlowSimple
					//        ZnAirRpt(ZoneLoop)%VentilFanElec  = ZnAirRpt(ZoneLoop)%VentilFanElec+Ventilation(VentNum)%FanPower*TimeStepSys*SecInHour &
					//          *ADSCorrectionFactor
					if ( ADSCorrectionFactor > 0 ) {
						ZnAirRpt( ZoneLoop ).VentilAirTemp += Ventilation( VentNum ).AirTemp * VentMCP( VentNum );
						VentZoneMassflow += VentMCP( VentNum );
						VentZoneAirTemp += Ventilation( VentNum ).AirTemp;
					} else {
						ZnAirRpt( ZoneLoop ).VentilAirTemp = Zone( ZoneLoop ).OutDryBulbTemp;
					}
					// Break the ventilation load into heat gain and loss components
					if ( MAT( ZoneLoop ) > Ventilation( VentNum ).AirTemp ) {
						ZnAirRpt( ZoneLoop ).VentilHeatLoss += VentMCP( VentNum ) * ( MAT( ZoneLoop ) - Ventilation( VentNum ).AirTemp ) * TimeStepSys * SecInHour * ADSCorrectionFactor;
					} else if ( MAT( ZoneLoop ) <= Ventilation( VentNum ).AirTemp ) {
						ZnAirRpt( ZoneLoop ).VentilHeatGain += VentMCP( VentNum ) * ( Ventilation( VentNum ).AirTemp - MAT( ZoneLoop ) ) * TimeStepSys * SecInHour * ADSCorrectionFactor;
					}

					++VentZoneNum;
					if ( VentZoneNum > 1 ) continue;

					// Report ventilation latent gains and losses
					H2OHtOfVap = PsyHgAirFnWTdb( OutHumRat, Zone( ZoneLoop ).OutDryBulbTemp, "ReportAirHeatBalance:5" );
					if ( ZoneAirHumRat( ZoneLoop ) > OutHumRat ) {
						ZnAirRpt( ZoneLoop ).VentilLatentLoss = 0.001 * MCPV( ZoneLoop ) / CpAir * ( ZoneAirHumRat( ZoneLoop ) - OutHumRat ) * H2OHtOfVap * TimeStepSys * SecInHour * 1000.0 * ADSCorrectionFactor;
						ZnAirRpt( ZoneLoop ).VentilLatentGain = 0.0;
					} else if ( ZoneAirHumRat( ZoneLoop ) <= OutHumRat ) {
						ZnAirRpt( ZoneLoop ).VentilLatentGain = 0.001 * MCPV( ZoneLoop ) / CpAir * ( OutHumRat - ZoneAirHumRat( ZoneLoop ) ) * H2OHtOfVap * TimeStepSys * SecInHour * 1000.0 * ADSCorrectionFactor;
						ZnAirRpt( ZoneLoop ).VentilLatentLoss = 0.0;
					}
					// Total ventilation losses and gains
					TotalLoad = ZnAirRpt( ZoneLoop ).VentilHeatGain + ZnAirRpt( ZoneLoop ).VentilLatentGain - ZnAirRpt( ZoneLoop ).VentilHeatLoss - ZnAirRpt( ZoneLoop ).VentilLatentLoss;
					if ( TotalLoad > 0 ) {
						ZnAirRpt( ZoneLoop ).VentilTotalGain = TotalLoad * ADSCorrectionFactor;
						ZnAirRpt( ZoneLoop ).VentilTotalLoss = 0.0;
					} else {
						ZnAirRpt( ZoneLoop ).VentilTotalGain = 0.0;
						ZnAirRpt( ZoneLoop ).VentilTotalLoss = -TotalLoad * ADSCorrectionFactor;
					}

				}
			}

			if ( ADSCorrectionFactor > 0 && VentZoneNum > 1 && VentZoneMassflow > 0.0 ) {
				ZnAirRpt( ZoneLoop ).VentilAirTemp /= VentZoneMassflow;
			} else if ( ADSCorrectionFactor > 0 && VentZoneNum == 1 ) {
				ZnAirRpt( ZoneLoop ).VentilAirTemp = VentZoneAirTemp;
			} else { // Just in case
				ZnAirRpt( ZoneLoop ).VentilAirTemp = Zone( ZoneLoop ).OutDryBulbTemp;
			}

			// Report mixing sensible and latent loads
			MixSenLoad = 0.0; // Initialize arrays to zero before starting to sum
			MixLatLoad = 0.0;
			ZnAirRpt( ZoneLoop ).MixVolume = 0.0; // zero reported volume prior to summations below
			ZnAirRpt( ZoneLoop ).MixVdotCurDensity = 0.0; // zero reported volume flow rate prior to summations below
			ZnAirRpt( ZoneLoop ).MixVdotStdDensity = 0.0; // zero reported volume flow rate prior to summations below
			ZnAirRpt( ZoneLoop ).MixMass = 0.0; // ! zero reported mass prior to summations below
			ZnAirRpt( ZoneLoop ).MixMdot = 0.0; // ! zero reported mass flow rate prior to summations below
			//    MixingLoad = 0.0d0

			for ( MixNum = 1; MixNum <= TotMixing; ++MixNum ) {
				if ( ( Mixing( MixNum ).ZonePtr == ZoneLoop ) && MixingReportFlag( MixNum ) ) {
					//        MixSenLoad(ZoneLoop) = MixSenLoad(ZoneLoop)+MCPM(ZoneLoop)*MAT(Mixing(MixNum)%FromZone)
					//        H2OHtOfVap = PsyHgAirFnWTdb(ZoneAirHumRat(ZoneLoop), MAT(ZoneLoop))
					//        Per Jan 17, 2008 conference call, agreed to use average conditions for Rho, Cp and Hfg
					//           and to recalculate the report variable using end of time step temps and humrats
					AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, ( MAT( ZoneLoop ) + MAT( Mixing( MixNum ).FromZone ) ) / 2.0, ( ZoneAirHumRat( ZoneLoop ) + ZoneAirHumRat( Mixing( MixNum ).FromZone ) ) / 2.0 );
					CpAir = PsyCpAirFnWTdb( ( ZoneAirHumRat( ZoneLoop ) + ZoneAirHumRat( Mixing( MixNum ).FromZone ) ) / 2.0, ( MAT( ZoneLoop ) + MAT( Mixing( MixNum ).FromZone ) ) / 2.0 );
					ZnAirRpt( ZoneLoop ).MixVolume += Mixing( MixNum ).DesiredAirFlowRate * TimeStepSys * SecInHour * ADSCorrectionFactor;
					ZnAirRpt( ZoneLoop ).MixVdotCurDensity += Mixing( MixNum ).DesiredAirFlowRate * ADSCorrectionFactor;
					ZnAirRpt( ZoneLoop ).MixMass += Mixing( MixNum ).DesiredAirFlowRate * AirDensity * TimeStepSys * SecInHour * ADSCorrectionFactor;
					ZnAirRpt( ZoneLoop ).MixMdot += Mixing( MixNum ).DesiredAirFlowRate * AirDensity * ADSCorrectionFactor;
					ZnAirRpt( ZoneLoop ).MixVdotStdDensity += Mixing( MixNum ).DesiredAirFlowRate * ( AirDensity / StdRhoAir ) * ADSCorrectionFactor;
					MixSenLoad( ZoneLoop ) += Mixing( MixNum ).DesiredAirFlowRate * AirDensity * CpAir * ( MAT( ZoneLoop ) - MAT( Mixing( MixNum ).FromZone ) );
					H2OHtOfVap = PsyHgAirFnWTdb( ( ZoneAirHumRat( ZoneLoop ) + ZoneAirHumRat( Mixing( MixNum ).FromZone ) ) / 2.0, ( MAT( ZoneLoop ) + MAT( Mixing( MixNum ).FromZone ) ) / 2.0, "ReportAirHeatBalance:Mixing" );
					//        MixLatLoad(ZoneLoop) = MixLatLoad(ZoneLoop)+MixingMassFlowZone(ZoneLoop)*(ZoneAirHumRat(ZoneLoop)- &
					//                     ZoneAirHumRat(Mixing(MixNum)%FromZone))*H2OHtOfVap
					MixLatLoad( ZoneLoop ) += Mixing( MixNum ).DesiredAirFlowRate * AirDensity * ( ZoneAirHumRat( ZoneLoop ) - ZoneAirHumRat( Mixing( MixNum ).FromZone ) ) * H2OHtOfVap;
				}
			}

			for ( MixNum = 1; MixNum <= TotCrossMixing; ++MixNum ) {
				if ( ( CrossMixing( MixNum ).ZonePtr == ZoneLoop ) && CrossMixingReportFlag( MixNum ) ) {
					//        MixSenLoad(ZoneLoop) = MixSenLoad(ZoneLoop)+MCPM(ZoneLoop)*MAT(CrossMixing(MixNum)%FromZone)
					//        Per Jan 17, 2008 conference call, agreed to use average conditions for Rho, Cp and Hfg
					//           and to recalculate the report variable using end of time step temps and humrats
					AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, ( MAT( ZoneLoop ) + MAT( CrossMixing( MixNum ).FromZone ) ) / 2.0, ( ZoneAirHumRat( ZoneLoop ) + ZoneAirHumRat( CrossMixing( MixNum ).FromZone ) ) / 2.0 );
					CpAir = PsyCpAirFnWTdb( ( ZoneAirHumRat( ZoneLoop ) + ZoneAirHumRat( CrossMixing( MixNum ).FromZone ) ) / 2.0, ( MAT( ZoneLoop ) + MAT( CrossMixing( MixNum ).FromZone ) ) / 2.0 );
					ZnAirRpt( ZoneLoop ).MixVolume += MVFC( MixNum ) * TimeStepSys * SecInHour * ADSCorrectionFactor;
					ZnAirRpt( ZoneLoop ).MixVdotCurDensity += MVFC( MixNum ) * ADSCorrectionFactor;
					ZnAirRpt( ZoneLoop ).MixMass += MVFC( MixNum ) * AirDensity * TimeStepSys * SecInHour * ADSCorrectionFactor;
					ZnAirRpt( ZoneLoop ).MixMdot += MVFC( MixNum ) * AirDensity * ADSCorrectionFactor;
					ZnAirRpt( ZoneLoop ).MixVdotStdDensity += MVFC( MixNum ) * ( AirDensity / StdRhoAir ) * ADSCorrectionFactor;
					MixSenLoad( ZoneLoop ) += MVFC( MixNum ) * AirDensity * CpAir * ( MAT( ZoneLoop ) - MAT( CrossMixing( MixNum ).FromZone ) );
					H2OHtOfVap = PsyHgAirFnWTdb( ( ZoneAirHumRat( ZoneLoop ) + ZoneAirHumRat( CrossMixing( MixNum ).FromZone ) ) / 2.0, ( MAT( ZoneLoop ) + MAT( CrossMixing( MixNum ).FromZone ) ) / 2.0, "ReportAirHeatBalance:XMixing" );
					//       MixLatLoad(ZoneLoop) = MixLatLoad(ZoneLoop)+MixingMassFlowZone(ZoneLoop)*(ZoneAirHumRat(ZoneLoop)- &
					//                     ZoneAirHumRat(CrossMixing(MixNum)%FromZone))*H2OHtOfVap
					MixLatLoad( ZoneLoop ) += MVFC( MixNum ) * AirDensity * ( ZoneAirHumRat( ZoneLoop ) - ZoneAirHumRat( CrossMixing( MixNum ).FromZone ) ) * H2OHtOfVap;

				}
			}

			if ( TotRefDoorMixing > 0 ) {
				//IF(ZoneLoop .NE. NumOfZones)THEN  !Refrigeration Door Mixing
				//Note - do each Pair a Single time, so must do increment reports for both zones
				//       Can't have a pair that has ZoneA zone number = NumOfZones because organized
				//       in input with lowest zone # first no matter how input in idf
				if ( RefDoorMixing( ZoneLoop ).RefDoorMixFlag ) { // .TRUE. for both zoneA and zoneB
					if ( RefDoorMixing( ZoneLoop ).ZonePtr == ZoneLoop ) {
						for ( j = 1; j <= RefDoorMixing( ZoneLoop ).NumRefDoorConnections; ++j ) {
							//    Capture impact when zoneloop is the 'primary zone'
							//    that is, the zone of a pair with the lower zone number
							if ( RefDoorMixing( ZoneLoop ).VolRefDoorFlowRate( j ) > 0.0 ) {
								ZoneB = RefDoorMixing( ZoneLoop ).MateZonePtr( j );
								AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, ( MAT( ZoneLoop ) + MAT( ZoneB ) ) / 2.0, ( ZoneAirHumRat( ZoneLoop ) + ZoneAirHumRat( ZoneB ) ) / 2.0 );
								CpAir = PsyCpAirFnWTdb( ( ZoneAirHumRat( ZoneLoop ) + ZoneAirHumRat( ZoneB ) ) / 2.0, ( MAT( ZoneLoop ) + MAT( ZoneB ) ) / 2.0 );
								H2OHtOfVap = PsyHgAirFnWTdb( ( ZoneAirHumRat( ZoneLoop ) + ZoneAirHumRat( ZoneB ) ) / 2.0, ( MAT( ZoneLoop ) + MAT( ZoneB ) ) / 2.0, "ReportAirHeatBalance:XMixing" );
								ZnAirRpt( ZoneLoop ).MixVolume += RefDoorMixing( ZoneLoop ).VolRefDoorFlowRate( j ) * TimeStepSys * SecInHour * ADSCorrectionFactor;
								ZnAirRpt( ZoneLoop ).MixVdotCurDensity += RefDoorMixing( ZoneLoop ).VolRefDoorFlowRate( j ) * ADSCorrectionFactor;
								ZnAirRpt( ZoneLoop ).MixMass += RefDoorMixing( ZoneLoop ).VolRefDoorFlowRate( j ) * AirDensity * TimeStepSys * SecInHour * ADSCorrectionFactor;
								ZnAirRpt( ZoneLoop ).MixMdot += RefDoorMixing( ZoneLoop ).VolRefDoorFlowRate( j ) * AirDensity * ADSCorrectionFactor;
								ZnAirRpt( ZoneLoop ).MixVdotStdDensity += RefDoorMixing( ZoneLoop ).VolRefDoorFlowRate( j ) * ( AirDensity / StdRhoAir ) * ADSCorrectionFactor;
								MixSenLoad( ZoneLoop ) += RefDoorMixing( ZoneLoop ).VolRefDoorFlowRate( j ) * AirDensity * CpAir * ( MAT( ZoneLoop ) - MAT( ZoneB ) );
								MixLatLoad( ZoneLoop ) += RefDoorMixing( ZoneLoop ).VolRefDoorFlowRate( j ) * AirDensity * ( ZoneAirHumRat( ZoneLoop ) - ZoneAirHumRat( ZoneB ) ) * H2OHtOfVap;
							} //flow > 0
						} // J-1, numref connections
					} // zone A (zoneptr = zoneloop)
					for ( ZoneA = 1; ZoneA <= ( ZoneLoop - 1 ); ++ZoneA ) {
						//    Capture impact when zoneloop is the 'mating zone'
						//    that is, the zone of a pair with the higher zone number(matezoneptr = zoneloop)
						if ( RefDoorMixing( ZoneA ).RefDoorMixFlag ) {
							for ( j = 1; j <= RefDoorMixing( ZoneA ).NumRefDoorConnections; ++j ) {
								if ( RefDoorMixing( ZoneA ).MateZonePtr( j ) == ZoneLoop ) {
									if ( RefDoorMixing( ZoneA ).VolRefDoorFlowRate( j ) > 0.0 ) {
										AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, ( MAT( ZoneLoop ) + MAT( ZoneA ) ) / 2.0, ( ZoneAirHumRat( ZoneLoop ) + ZoneAirHumRat( ZoneA ) ) / 2.0 );
										CpAir = PsyCpAirFnWTdb( ( ZoneAirHumRat( ZoneLoop ) + ZoneAirHumRat( ZoneA ) ) / 2.0, ( MAT( ZoneLoop ) + MAT( ZoneA ) ) / 2.0 );
										H2OHtOfVap = PsyHgAirFnWTdb( ( ZoneAirHumRat( ZoneLoop ) + ZoneAirHumRat( ZoneA ) ) / 2.0, ( MAT( ZoneLoop ) + MAT( ZoneA ) ) / 2.0, "ReportAirHeatBalance:XMixing" );
										ZnAirRpt( ZoneLoop ).MixVolume += RefDoorMixing( ZoneA ).VolRefDoorFlowRate( j ) * TimeStepSys * SecInHour * ADSCorrectionFactor;
										ZnAirRpt( ZoneLoop ).MixVdotCurDensity += RefDoorMixing( ZoneA ).VolRefDoorFlowRate( j ) * ADSCorrectionFactor;
										ZnAirRpt( ZoneLoop ).MixMass += RefDoorMixing( ZoneA ).VolRefDoorFlowRate( j ) * AirDensity * TimeStepSys * SecInHour * ADSCorrectionFactor;
										ZnAirRpt( ZoneLoop ).MixMdot += RefDoorMixing( ZoneA ).VolRefDoorFlowRate( j ) * AirDensity * ADSCorrectionFactor;
										ZnAirRpt( ZoneLoop ).MixVdotStdDensity += RefDoorMixing( ZoneA ).VolRefDoorFlowRate( j ) * ( AirDensity / StdRhoAir ) * ADSCorrectionFactor;
										MixSenLoad( ZoneLoop ) += RefDoorMixing( ZoneA ).VolRefDoorFlowRate( j ) * AirDensity * CpAir * ( MAT( ZoneLoop ) - MAT( ZoneA ) );
										MixLatLoad( ZoneLoop ) += RefDoorMixing( ZoneA ).VolRefDoorFlowRate( j ) * AirDensity * ( ZoneAirHumRat( ZoneLoop ) - ZoneAirHumRat( ZoneA ) ) * H2OHtOfVap;
									} // volflowrate > 0
								} // matezoneptr (zoneB) = Zonelooop
							} // NumRefDoorConnections
						} // Refdoormix flag on ZoneA
					} // zone A from 1 to (zoneloop - 1)
				} // Refdoormix flag on zoneloop
			} //(TotRefDoorMixing .GT. 0)
			//end refrigeration door mixing reports

			//    MixingLoad(ZoneLoop) = MCPM(ZoneLoop)*MAT(ZoneLoop) - MixSenLoad(ZoneLoop)
			if ( MixSenLoad( ZoneLoop ) > 0.0 ) {
				ZnAirRpt( ZoneLoop ).MixHeatLoss = MixSenLoad( ZoneLoop ) * TimeStepSys * SecInHour * ADSCorrectionFactor;
				ZnAirRpt( ZoneLoop ).MixHeatGain = 0.0;
			} else {
				ZnAirRpt( ZoneLoop ).MixHeatLoss = 0.0;
				ZnAirRpt( ZoneLoop ).MixHeatGain = -MixSenLoad( ZoneLoop ) * TimeStepSys * SecInHour * ADSCorrectionFactor;
			}
			// Report mixing latent loads
			//    MixingLoad(ZoneLoop) = MixLatLoad(ZoneLoop)
			if ( MixLatLoad( ZoneLoop ) > 0.0 ) {
				ZnAirRpt( ZoneLoop ).MixLatentLoss = MixLatLoad( ZoneLoop ) * TimeStepSys * SecInHour * ADSCorrectionFactor;
				ZnAirRpt( ZoneLoop ).MixLatentGain = 0.0;
			} else {
				ZnAirRpt( ZoneLoop ).MixLatentLoss = 0.0;
				ZnAirRpt( ZoneLoop ).MixLatentGain = -MixLatLoad( ZoneLoop ) * TimeStepSys * SecInHour * ADSCorrectionFactor;
			}
			// Total Mixing losses and gains
			TotalLoad = ZnAirRpt( ZoneLoop ).MixHeatGain + ZnAirRpt( ZoneLoop ).MixLatentGain - ZnAirRpt( ZoneLoop ).MixHeatLoss - ZnAirRpt( ZoneLoop ).MixLatentLoss;
			if ( TotalLoad > 0 ) {
				ZnAirRpt( ZoneLoop ).MixTotalGain = TotalLoad * ADSCorrectionFactor;
				ZnAirRpt( ZoneLoop ).MixTotalLoss = 0.0;
			} else {
				ZnAirRpt( ZoneLoop ).MixTotalGain = 0.0;
				ZnAirRpt( ZoneLoop ).MixTotalLoss = -TotalLoad * ADSCorrectionFactor;
			}

			// Reporting combined outdoor air flows
			for ( j = 1; j <= TotZoneAirBalance; ++j ) {
				if ( ZoneAirBalance( j ).BalanceMethod == AirBalanceQuadrature && ZoneLoop == ZoneAirBalance( j ).ZonePtr ) {
					if ( MAT( ZoneLoop ) > Zone( ZoneLoop ).OutDryBulbTemp ) {
						ZnAirRpt( ZoneLoop ).OABalanceHeatLoss = MDotCPOA( ZoneLoop ) * ( MAT( ZoneLoop ) - Zone( ZoneLoop ).OutDryBulbTemp ) * TimeStepSys * SecInHour * ADSCorrectionFactor;
						ZnAirRpt( ZoneLoop ).OABalanceHeatGain = 0.0;
					} else {
						ZnAirRpt( ZoneLoop ).OABalanceHeatLoss = 0.0;
						ZnAirRpt( ZoneLoop ).OABalanceHeatGain = -MDotCPOA( ZoneLoop ) * ( MAT( ZoneLoop ) - Zone( ZoneLoop ).OutDryBulbTemp ) * TimeStepSys * SecInHour * ADSCorrectionFactor;
					}
					H2OHtOfVap = PsyHgAirFnWTdb( OutHumRat, Zone( ZoneLoop ).OutDryBulbTemp, "ReportAirHeatBalance:2" );
					if ( ZoneAirHumRat( ZoneLoop ) > OutHumRat ) {
						ZnAirRpt( ZoneLoop ).OABalanceLatentLoss = 0.001 * MDotOA( ZoneLoop ) * ( ZoneAirHumRat( ZoneLoop ) - OutHumRat ) * H2OHtOfVap * TimeStepSys * SecInHour * 1000.0 * ADSCorrectionFactor;
						ZnAirRpt( ZoneLoop ).OABalanceLatentGain = 0.0;
					} else if ( ZoneAirHumRat( ZoneLoop ) <= OutHumRat ) {
						ZnAirRpt( ZoneLoop ).OABalanceLatentGain = 0.001 * MDotOA( ZoneLoop ) * ( OutHumRat - ZoneAirHumRat( ZoneLoop ) ) * H2OHtOfVap * TimeStepSys * SecInHour * 1000.0 * ADSCorrectionFactor;
						ZnAirRpt( ZoneLoop ).OABalanceLatentLoss = 0.0;
					}
					// Total ventilation losses and gains
					TotalLoad = ZnAirRpt( ZoneLoop ).OABalanceHeatGain + ZnAirRpt( ZoneLoop ).OABalanceLatentGain - ZnAirRpt( ZoneLoop ).OABalanceHeatLoss - ZnAirRpt( ZoneLoop ).OABalanceLatentLoss;
					if ( TotalLoad > 0 ) {
						ZnAirRpt( ZoneLoop ).OABalanceTotalGain = TotalLoad * ADSCorrectionFactor;
						ZnAirRpt( ZoneLoop ).OABalanceTotalLoss = 0.0;
					} else {
						ZnAirRpt( ZoneLoop ).OABalanceTotalGain = 0.0;
						ZnAirRpt( ZoneLoop ).OABalanceTotalLoss = -TotalLoad * ADSCorrectionFactor;
					}
					ZnAirRpt( ZoneLoop ).OABalanceMass = ( MDotOA( ZoneLoop ) ) * TimeStepSys * SecInHour * ADSCorrectionFactor;
					ZnAirRpt( ZoneLoop ).OABalanceMdot = ( MDotOA( ZoneLoop ) ) * ADSCorrectionFactor;
					AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, MAT( ZoneLoop ), ZoneAirHumRatAvg( ZoneLoop ) );
					ZnAirRpt( ZoneLoop ).OABalanceVolumeCurDensity = ( MDotOA( ZoneLoop ) / AirDensity ) * TimeStepSys * SecInHour * ADSCorrectionFactor;
					ZnAirRpt( ZoneLoop ).OABalanceAirChangeRate = ZnAirRpt( ZoneLoop ).OABalanceVolumeCurDensity / ( TimeStepSys * Zone( ZoneLoop ).Volume );
					ZnAirRpt( ZoneLoop ).OABalanceVdotCurDensity = ( MDotOA( ZoneLoop ) / AirDensity ) * ADSCorrectionFactor;
					AirDensity = StdRhoAir;
					ZnAirRpt( ZoneLoop ).OABalanceVolumeStdDensity = ( MDotOA( ZoneLoop ) / AirDensity ) * TimeStepSys * SecInHour * ADSCorrectionFactor;
					ZnAirRpt( ZoneLoop ).OABalanceVdotStdDensity = ( MDotOA( ZoneLoop ) / AirDensity ) * ADSCorrectionFactor;
					ZnAirRpt( ZoneLoop ).OABalanceFanElec = ZnAirRpt( ZoneLoop ).VentilFanElec;
				}
			}

		}

	}

	void
	SetHeatToReturnAirFlag()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   February 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This sets some flags at the air loop and zone level: these flags indicate
		// whether an air loop represents a "unitary" system, and whether the system is operating
		// in a on/off (cycling fan) mode. At the zone level flags are set to indicate whether
		// the zone is served by a zonal system only, and whether the air loop serving the zone (idf any)
		// is in cycling fan mode. Using this information, the subroutine sets a flag at the zone level
		// to tell ManageZoneAirUpdates (predict and correct) what to do with the heat to return air.

		// METHODOLOGY EMPLOYED:
		// Uses program data structures AirLoopControlInfo and ZoneEquipInfo

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::NumPrimaryAirSys;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataHeatBalance::Zone;
		using DataHeatBalance::Lights;
		using DataHeatBalance::TotLights;
		using ScheduleManager::CheckScheduleValue;
		using ScheduleManager::GetCurrentScheduleValue;
		using ScheduleManager::GetScheduleMaxValue;
		using DataSurfaces::SurfaceWindow;
		using DataSurfaces::AirFlowWindow_Destination_ReturnAir;

		// Locals
		// SUBROUTINE ARGUMENTS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int AirLoopNum( 0 ); // the air loop index
		int ControlledZoneNum; // controlled zone index
		static bool MyOneTimeFlag( true );
		static bool CyclingFan( false ); // TRUE means air loop operates in cycling fan mode at some point
		static int ZoneNum( 0 ); // zone index
		int LightNum; // Lights object index
		int SurfNum; // Surface index
		static Real64 CycFanMaxVal( 0.0 ); // max value of cycling fan schedule

		if ( ! AirLoopsSimOnce ) return;

		if ( MyOneTimeFlag ) {
			// set the air loop Any Continuous Fan flag
			for ( AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum ) {
				if ( AirLoopControlInfo( AirLoopNum ).UnitarySys ) { // for unitary systems check the cycling fan schedule
					if ( AirLoopControlInfo( AirLoopNum ).CycFanSchedPtr > 0 ) {
						CycFanMaxVal = GetScheduleMaxValue( AirLoopControlInfo( AirLoopNum ).CycFanSchedPtr );
						if ( CycFanMaxVal > 0.0 ) {
							AirLoopControlInfo( AirLoopNum ).AnyContFan = true;
						} else {
							AirLoopControlInfo( AirLoopNum ).AnyContFan = false;
						}
					} else { // no schedule means always cycling fan
						AirLoopControlInfo( AirLoopNum ).AnyContFan = false;
					}
				} else { // for nonunitary (central) all systems are continuous fan
					AirLoopControlInfo( AirLoopNum ).AnyContFan = true;
				}
			}
			// check to see if a controlled zone is served exclusively by a zonal system
			for ( ControlledZoneNum = 1; ControlledZoneNum <= NumOfZones; ++ControlledZoneNum ) {
				ZoneNum = ZoneEquipConfig( ControlledZoneNum ).ActualZoneNum;
				if ( ZoneEquipConfig( ControlledZoneNum ).AirLoopNum == 0 && ZoneEquipConfig( ControlledZoneNum ).NumInletNodes == ZoneEquipConfig( ControlledZoneNum ).NumExhaustNodes ) {
					ZoneEquipConfig( ControlledZoneNum ).ZonalSystemOnly = true;
				}
			}
			// issue warning messages if zone is served by a zonal system or a cycling system and the input calls for
			// heat gain to return air
			for ( ControlledZoneNum = 1; ControlledZoneNum <= NumOfZones; ++ControlledZoneNum ) {
				if ( ! ZoneEquipConfig( ControlledZoneNum ).IsControlled ) continue;
				ZoneNum = ZoneEquipConfig( ControlledZoneNum ).ActualZoneNum;
				CyclingFan = false;
				AirLoopNum = ZoneEquipConfig( ControlledZoneNum ).AirLoopNum;
				if ( AirLoopNum > 0 ) {
					if ( AirLoopControlInfo( AirLoopNum ).CycFanSchedPtr > 0 ) {
						CyclingFan = CheckScheduleValue( AirLoopControlInfo( AirLoopNum ).CycFanSchedPtr, 0.0 );
					}
				}
				if ( ZoneEquipConfig( ControlledZoneNum ).ZonalSystemOnly || CyclingFan ) {
					if ( Zone( ZoneNum ).RefrigCaseRA ) {
						ShowWarningError( "For zone=" + Zone( ZoneNum ).Name + " return air cooling by refrigerated cases will be" " applied to the zone air." );
						ShowContinueError( "  This zone has no return air or is served by an on/off HVAC system." );
					}
					for ( LightNum = 1; LightNum <= TotLights; ++LightNum ) {
						if ( Lights( LightNum ).ZonePtr != ZoneNum ) continue;
						if ( Lights( LightNum ).FractionReturnAir > 0.0 ) {
							ShowWarningError( "For zone=" + Zone( ZoneNum ).Name + " return air heat gain from lights will be" " applied to the zone air." );
							ShowContinueError( "  This zone has no return air or is served by an on/off HVAC system." );
							break;
						}
					}
					for ( SurfNum = Zone( ZoneNum ).SurfaceFirst; SurfNum <= Zone( ZoneNum ).SurfaceLast; ++SurfNum ) {
						if ( SurfaceWindow( SurfNum ).AirflowDestination == AirFlowWindow_Destination_ReturnAir ) {
							ShowWarningError( "For zone=" + Zone( ZoneNum ).Name + " return air heat gain from air flow windows will be applied to the zone air." );
							ShowContinueError( "  This zone has no return air or is served by an on/off HVAC system." );
						}
					}
				}
			}
			MyOneTimeFlag = false;
		}

		// set the air loop fan operation mode
		for ( AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum ) {
			if ( AirLoopControlInfo( AirLoopNum ).CycFanSchedPtr > 0 ) {
				if ( GetCurrentScheduleValue( AirLoopControlInfo( AirLoopNum ).CycFanSchedPtr ) == 0.0 ) {
					AirLoopControlInfo( AirLoopNum ).FanOpMode = CycFanCycCoil;
				} else {
					AirLoopControlInfo( AirLoopNum ).FanOpMode = ContFanCycCoil;
				}
			}
		}
		// set the zone level NoHeatToReturnAir flag and the ZoneEquip fan operation mode
		for ( ControlledZoneNum = 1; ControlledZoneNum <= NumOfZones; ++ControlledZoneNum ) {
			if ( ! ZoneEquipConfig( ControlledZoneNum ).IsControlled ) continue;
			ZoneNum = ZoneEquipConfig( ControlledZoneNum ).ActualZoneNum;
			AirLoopNum = ZoneEquipConfig( ControlledZoneNum ).AirLoopNum;
			if ( AirLoopNum > 0 ) {
				ZoneEquipConfig( ControlledZoneNum ).FanOpMode = AirLoopControlInfo( AirLoopNum ).FanOpMode;
			} else {
				ZoneEquipConfig( ControlledZoneNum ).FanOpMode = 0;
			}
			if ( ZoneEquipConfig( ControlledZoneNum ).FanOpMode == CycFanCycCoil || ZoneEquipConfig( ControlledZoneNum ).ZonalSystemOnly ) {
				Zone( ZoneNum ).NoHeatToReturnAir = true;
			} else {
				Zone( ZoneNum ).NoHeatToReturnAir = false;
			}
		}

	}

	void
	GetStandAloneERVNodes( int const OutdoorNum ) // Zone Air Balance Outdoor index
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Lixing Gu
		//       DATE WRITTEN   July 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets node numbers of stand alone ERVs to calculate combined outdoor air flows.

		// METHODOLOGY EMPLOYED:
		// Uses program data structures ZoneEquipInfo

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataZoneEquipment::ZoneEquipList;
		using DataZoneEquipment::ERVStandAlone_Num;
		using DataHeatBalance::ZoneAirBalance;
		using DataHeatBalance::AirBalanceQuadrature;
		using HVACStandAloneERV::GetStandAloneERVOutAirNode;
		using HVACStandAloneERV::GetStandAloneERVReturnAirNode;

		// Locals
		// SUBROUTINE ARGUMENTS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//  INTEGER      :: ERVNum=0                   ! the stand alone ERV index
		static int ZoneNum( 0 ); // zone index
		int j; // index
		int I; // index

		if ( allocated( ZoneEquipList ) ) {
			ZoneNum = ZoneAirBalance( OutdoorNum ).ZonePtr;
			ZoneAirBalance( OutdoorNum ).OneTimeFlag = true;
			if ( ZoneEquipList( ZoneNum ).NumOfEquipTypes > 0 ) {
				for ( I = 1; I <= ZoneEquipList( ZoneNum ).NumOfEquipTypes; ++I ) {
					if ( ZoneEquipList( ZoneNum ).EquipType_Num( I ) == ERVStandAlone_Num ) {
						++ZoneAirBalance( OutdoorNum ).NumOfERVs;
					}
				}
				if ( ZoneAirBalance( OutdoorNum ).NumOfERVs > 0 ) {
					ZoneAirBalance( OutdoorNum ).ERVInletNode.allocate( ZoneAirBalance( OutdoorNum ).NumOfERVs );
					ZoneAirBalance( OutdoorNum ).ERVExhaustNode.allocate( ZoneAirBalance( OutdoorNum ).NumOfERVs );
					j = 1;
					for ( I = 1; I <= ZoneEquipList( ZoneNum ).NumOfEquipTypes; ++I ) {
						if ( ZoneEquipList( ZoneNum ).EquipType_Num( I ) == ERVStandAlone_Num ) {
							ZoneAirBalance( OutdoorNum ).ERVInletNode( j ) = GetStandAloneERVOutAirNode( ZoneEquipList( ZoneNum ).EquipIndex( I ) );
							ZoneAirBalance( OutdoorNum ).ERVExhaustNode( j ) = GetStandAloneERVReturnAirNode( ZoneEquipList( ZoneNum ).EquipIndex( I ) );
							++j;
						}
					}
				}
			}
		}

	}

	void
	UpdateZoneInletConvergenceLog()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         <author>
		//       DATE WRITTEN   <date_written>
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataConvergParams::ZoneInletConvergence;
		using DataConvergParams::ConvergLogStackDepth;
		using DataLoopNode::Node;
		using DataGlobals::NumOfZones;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ZoneNum;
		int ZoneIndex;
		int NodeIndex;
		int NodeNum;
		FArray1D< Real64 > tmpRealARR( ConvergLogStackDepth );

		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {

			for ( NodeIndex = 1; NodeIndex <= ZoneInletConvergence( ZoneNum ).NumInletNodes; ++NodeIndex ) {
				NodeNum = ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).NodeNum;

				tmpRealARR = ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).HumidityRatio;
				ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).HumidityRatio( 1 ) = Node( NodeNum ).HumRat;
				ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).HumidityRatio( {2,ConvergLogStackDepth} ) = tmpRealARR( {1,ConvergLogStackDepth - 1} );

				tmpRealARR = ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).MassFlowRate;
				ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).MassFlowRate( 1 ) = Node( NodeNum ).MassFlowRate;
				ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).MassFlowRate( {2,ConvergLogStackDepth} ) = tmpRealARR( {1,ConvergLogStackDepth - 1} );

				tmpRealARR = ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).Temperature;
				ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).Temperature( 1 ) = Node( NodeNum ).Temp;
				ZoneInletConvergence( ZoneNum ).InletNode( NodeIndex ).Temperature( {2,ConvergLogStackDepth} ) = tmpRealARR( {1,ConvergLogStackDepth - 1} );
			}
		}

	}

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in EnergyPlus.f90.

	//     NOTICE: The U.S. Government is granted for itself and others acting on its
	//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
	//     reproduce, prepare derivative works, and perform publicly and display publicly.
	//     Beginning five (5) years after permission to assert copyright is granted,
	//     subject to two possible five year renewals, the U.S. Government is granted for
	//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
	//     worldwide license in this data to reproduce, prepare derivative works,
	//     distribute copies to the public, perform publicly and display publicly, and to
	//     permit others to do so.

	//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

} // HVACManager

} // EnergyPlus
