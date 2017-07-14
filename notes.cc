
// ManageSimulation --> SimulationManager.cc
//   Init stuff
//   ManageSizing()
//   while( next environment available ) {
//     GetNextEnvironment()
//     // Setup output reporting
//     while ( ( DayOfSim < NumOfDayInEnvrn ) || ( WarmupFlag ) ) {
//       for ( HourOfDay = 1; HourOfDay <= 24; ++HourOfDay ) {
//			   ManageWeather();
//			   ManageExteriorEnergyUse();
//			   ManageHeatBalance();
//          GetHeatBalanceInput()
//          InitHeatBalance()
//          ManageSurfaceHeatBalance()
//            InitSurfaceHeatBalance()
//	          CalcHeatBalanceOutsideSurf();
//	          CalcHeatBalanceInsideSurf();
//            ManageAirHeatBalance()
//              CalcHeatBalanceAir() // <-- Does almost nothing except call ManageHVAC()
//                ManageHVAC()
//                  // Some stuff for AFN
//                  // Some stuff for contaminant calcs
//                  // Reset nodes and set some flags on new environment
//                  ManageEMS()
//                  SetOutAirNodes();
//                  ManageRefrigeratedCaseRacks();
//	                ManageZoneAirUpdates( iGetZoneSetPoints, ZoneTempChange, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep );
//                  CalcAirFlowSimple // <-- This dude does a lot. Simple ventilation, infiltration, zone mixing, and probably more
//                  SetHeatToReturnAirFlag() // <-- Set some flags related to unitary systems and on/off operation
//                  UpdateInternalGainValues( true, true )
//                  ManageZoneAirUpdates( iPredictStep, ZoneTempChange, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep );
//                  SimHVAC()
//				          ManageWaterInits();
//              		ManageZoneAirUpdates( iCorrectStep, ZoneTempChange, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep );
//		              if ( ZoneTempChange > MaxZoneTempDiff && ! KickOffSimulation ) {
//		                ZTempTrendsNumSysSteps = int( ZoneTempChange / MaxZoneTempDiff + 1.0 ); // add 1 for truncation
//		                .... to compute a new shorter system timestep
//		              }
//                  for ( SysTimestepLoop = 1; SysTimestepLoop <= NumOfSysTimeSteps; ++SysTimestepLoop ) { // <-- this will always happen at least once
//		                if ( TimeStepSys < TimeStepZone ) {
//				              CalcAirFlowSimple( SysTimestepLoop );
//				              UpdateInternalGainValues( true, true );
//	  	                ManageZoneAirUpdates( iPredictStep, ZoneTempChange, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep );
//				              SimHVAC();
//                      ManageZoneAirUpdates( iCorrectStep, ZoneTempChange, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep );
//                      ManageZoneAirUpdates( iPushSystemTimestepHistories, ZoneTempChange, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep );
//                    }
//                    ManageWater();
//                    // Do output reporting
//                  }
//
//       }
//     }
//   }
//     ManageWeather --> WeatherManager.cc
//     ManageDemand --> DemandManager.cc
//   * ManageExteriorEnergyUse --> ExteriorEnergyUse.cc
//     ManageHeatBalance --> HeatBalanceManager.cc
//       * InitHeatBalance --> HeatBalanceManager.cc
//             PerformSolarCalculations
//         ManageSurfaceHeatBalance --> HeatBalanceSurfaceManager.cc
//           * InitSurfaceHeatBalance --> HeatBalanceSurfaceManager.cc
//                 ManageInternalHeatGains --> InternalHeatGains.cc
//           * CalcHeatBalanceOutsideSurf --> HeatBalanceSurfaceManager.cc
//           * CalcHeatBalanceInsideSurf --> HeatBalanceSurfaceManager.cc
//             ManageAirHeatBalance --> HeatBalanceAirManager.cc
//                *InitAirHeatBalance --> HeatBalanceAirManager.cc
//                 CalcHeatBalanceAir --> HeatBalanceAirManager.cc
//                   * CalcAirFlow --> ?? Might not exist anymore
//                   * ManageRefrigeratedCaseRacks --> RefrigeratedCase.cc
//                     ManageHVAC --> HVACManager.cc
//                       * ManageZoneAirUpdates 'GET ZONE SETPOINTS' --> ZoneTempPredictorCorrector.cc
//                       * ManageZoneAirUpdates 'PREDICT' --> ZoneTempPredictorCorrector.cc
//                       * SimHVAC --> HVACManager.cc
//                         UpdateDataandReport --> OutputProcessor.cc
//                 ReportAirHeatBalance --> HVACManager.cc
//             UpdateFinalSurfaceHeatBalance --> HeatBalanceSurfaceManager.cc
//             UpdateThermalHistories --> HeatBalanceSurfaceManager.cc
//             UpdateMoistureHistories --> ?? Might not exist anymore
//             ManageThermalComfort --> ThermalComfort.cc
//             ReportSurfaceHeatBalance --> HeatBalanceSurfaceManager.cc
//         RecKeepHeatBalance --> HeatBalanceManager.cc
//         ReportHeatBalance --> HeatBalanceManager.cc

