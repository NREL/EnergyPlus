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
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <CommandLineInterface.hh>
#include <SizingManager.hh>
#include <CostEstimateManager.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataStringGlobals.hh>
#include <DataZoneEquipment.hh>
#include <DisplayRoutines.hh>
#include <EMSManager.hh>
#include <General.hh>
#include <HeatBalanceManager.hh>
#include <InputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <OutputReportTabular.hh>
#include <ScheduleManager.hh>
#include <SimAirServingZones.hh>
#include <SQLiteProcedures.hh>
#include <UtilityRoutines.hh>
#include <WeatherManager.hh>
#include <ZoneEquipmentManager.hh>

namespace EnergyPlus {

namespace SizingManager {

	// MODULE INFORMATION:
	//       AUTHOR         Fred Buhl
	//       DATE WRITTEN   December 2000
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module contains the data and routines relating to managing the sizing
	// simulations.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES: none

	// OTHER NOTES: none

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGlobals;
	using namespace HeatBalanceManager;
	using namespace WeatherManager;
	using namespace DataSizing;
	using DataHVACGlobals::NumPrimaryAirSys;
	using DataZoneEquipment::ZoneEquipConfig;
	using DataStringGlobals::CharTab;
	using DataStringGlobals::CharComma;
	using DataStringGlobals::CharSpace;

	// Data
	// MODULE PARAMETER DEFINITIONS: none

	// DERIVED TYPE DEFINITIONS: none

	// INTERFACE BLOCK SPECIFICATIONS: none

	// MODULE VARIABLE DECLARATIONS:
	int NumAirLoops( 0 );

	// SUBROUTINE SPECIFICATIONS FOR MODULE SimulationManager

	// MODULE SUBROUTINES:

	// Functions
	void
	clear_state()
	{
	 NumAirLoops = 0;
	}


	void
	ManageSizing()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   December 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages the sizing simulations (using design day condiions)
		// for zones, central air systems, and central plants and zone heating and cooling

		// METHODOLOGY EMPLOYED:
		// Design day simulations are run with the zones supplied with "Ideal Loads",
		// yielding zone design supply air flow rates and zone heating and cooling capacities.
		// Design day simulations are run again with central air systems supplied by
		// purchased hot and cold water, yielding central heating and cooling capacities.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumRangeCheckErrorsFound;
		using ZoneEquipmentManager::UpdateZoneSizing;
		using ZoneEquipmentManager::ManageZoneEquipment;
		using ZoneEquipmentManager::RezeroZoneSizingArrays;
		using SimAirServingZones::ManageAirLoops;
		using SimAirServingZones::UpdateSysSizing;
		using DataEnvironment::OutDryBulbTemp;
		using DataEnvironment::OutHumRat;
		using DataEnvironment::OutBaroPress;
		using DataEnvironment::Month;
		using DataEnvironment::DayOfMonth;
		using DataEnvironment::EndMonthFlag;
		using DataEnvironment::EnvironmentName;
		using namespace OutputReportPredefined;
		using DataHeatBalance::Zone;
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using OutputReportTabular::isCompLoadRepReq;
		using OutputReportTabular::AllocateLoadComponentArrays;
		using OutputReportTabular::DeallocateLoadComponentArrays;
		using OutputReportTabular::ComputeLoadComponentDecayCurve;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "ManageSizing: " );
		static gio::Fmt fmtLD( "*" );

		// INTERFACE BLOCK SPECIFICATIONS: none

		// DERIVED TYPE DEFINITIONS: none

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool Available( false ); // an environment is available to process
		static bool ErrorsFound( false );
		static bool SimAir( false );
		static bool SimZoneEquip( false );
		static int TimeStepInDay( 0 ); // time step number
		static int LastMonth( 0 );
		static int LastDayOfMonth( 0 );
		static int CtrlZoneNum( 0 ); // controlled zone index
		static int ZoneNum( 0 ); // index into the Zone data array for the controlled zone
		static Real64 TempAtPeak( 0.0 ); // Outside temperature at peak cooling/heating for reporting
		static Real64 HumRatAtPeak( 0.0 ); // Outside humidity ratio at peak cooling/heating for reporting
		static int TimeStepAtPeak( 0 ); // time step number at heat or cool peak
		static int DDNum( 0 ); // Design Day index
		static int AirLoopNum( 0 ); // air loop index
		//  EXTERNAL            ReportZoneSizing
		//  EXTERNAL            ReportSysSizing
		std::string curName;
		int NumSizingPeriodsPerformed;
		int write_stat;
		int numZoneSizeIter; // number of times to repeat zone sizing calcs. 1 normal, 2 load component reporting
		int iZoneCalcIter; // index for repeating the zone sizing calcs
		static bool runZeroingOnce( true );
		bool isUserReqCompLoadReport;
		Real64 DOASHeatGainRateAtHtPk( 0.0 ); // zone heat gain rate from the DOAS at the heating peak [W]
		Real64 DOASHeatGainRateAtClPk( 0.0 ); // zone heat gain rate from the DOAS at the cooling peak [W]
		Real64 TStatSetPtAtPk( 0.0 ); // thermostat set point at peak

		// FLOW:

		OutputFileZoneSizing = 0;
		OutputFileSysSizing = 0;
		TimeStepInDay = 0;
		SysSizingRunDone = false;
		ZoneSizingRunDone = false;
		curName = "Unknown";
		GetOARequirements(); // get the OA requirements object
		GetZoneAirDistribution(); // get zone air distribution objects
		GetZoneHVACSizing(); // get zone HVAC sizing object
		GetSizingParams(); // get the building level sizing paramets
		GetZoneSizingInput(); // get the Zone Sizing input
		GetSystemSizingInput(); // get the System Sizing input
		GetPlantSizingInput(); // get the Plant Sizing input

		// okay, check sizing inputs vs desires vs requirements
		if ( DoZoneSizing || DoSystemSizing ) {
			if ( ( NumSysSizInput > 0 && NumZoneSizingInput == 0 ) || ( ! DoZoneSizing && DoSystemSizing && NumSysSizInput > 0 ) ) {
				ShowSevereError( RoutineName + "Requested System Sizing but did not request Zone Sizing." );
				ShowContinueError( "System Sizing cannot be done without Zone Sizing" );
				ShowFatalError( "Program terminates for preceding conditions." );
			}
		}

		// determine if the second set of zone sizing calculations should be performed
		// that include a pulse for the load component reporting
		isUserReqCompLoadReport = isCompLoadRepReq(); //check getinput structure if load component report is requested
		if ( DoZoneSizing && ( NumZoneSizingInput > 0 ) ) {
			CompLoadReportIsReq = isUserReqCompLoadReport;
		} else { // produce a warning if the user asked for the report but it will not be generated because sizing is not done
			if ( isUserReqCompLoadReport ) {
				ShowWarningError( RoutineName + "The ZoneComponentLoadSummary report was requested but no sizing objects were found so that report cannot be generated." );
			}
		}
		if ( CompLoadReportIsReq ) { //if that report is created then zone sizing calculations are repeated
			numZoneSizeIter = 2;
		} else {
			numZoneSizeIter = 1;
		}

		if ( ( DoZoneSizing ) && ( NumZoneSizingInput == 0 ) ) {
			ShowWarningError( RoutineName + "For a zone sizing run, there must be at least 1 Sizing:Zone input object. SimulationControl Zone Sizing option ignored." );
		}

		if ( ( NumZoneSizingInput > 0 ) && ( DoZoneSizing || DoSystemSizing || DoPlantSizing ) ) {

			if ( DoDesDaySim || DoWeathSim ) {
				DoOutputReporting = false;
			}
			DoOutputReporting = false;
			ZoneSizingCalc = true;
			Available = true;
			OutputFileZoneSizing = GetNewUnitNumber();
			if ( SizingFileColSep == CharComma ) {
				{ IOFlags flags; flags.ACTION( "write" ); gio::open( OutputFileZoneSizing, DataStringGlobals::outputZszCsvFileName, flags ); write_stat = flags.ios(); }
				if ( write_stat != 0 ) {
					ShowFatalError( RoutineName + "Could not open file "+ DataStringGlobals::outputZszCsvFileName +" for output (write)." );
				}
			} else if ( SizingFileColSep == CharTab ) {
				{ IOFlags flags; flags.ACTION( "write" ); gio::open( OutputFileZoneSizing, DataStringGlobals::outputZszTabFileName, flags ); write_stat = flags.ios(); }
				if ( write_stat != 0 ) {
					ShowFatalError( RoutineName + "Could not open file "+DataStringGlobals::outputZszTabFileName+" for output (write)." );
				}
			} else {
				{ IOFlags flags; flags.ACTION( "write" ); gio::open( OutputFileZoneSizing, DataStringGlobals::outputZszTxtFileName, flags ); write_stat = flags.ios(); }
				if ( write_stat != 0 ) {
					ShowFatalError( RoutineName + "Could not open file "+ DataStringGlobals::outputZszTxtFileName +" for output (write)." );
				}
			}

			ShowMessage( "Beginning Zone Sizing Calculations" );

			ResetEnvironmentCounter();
			KickOffSizing = true;
			SetupZoneSizing( ErrorsFound ); // Should only be done ONCE
			KickOffSizing = false;

			for ( iZoneCalcIter = 1; iZoneCalcIter <= numZoneSizeIter; ++iZoneCalcIter ) { //normally this is performed once but if load component
				//report is requested, these are repeated with a pulse in
				//each zone.

				//set flag if performing a "pulse" set of sizing calcs
				//the pulse simulation needs to be done first (the 1 in the following line) otherwise
				//the difference seen in the loads in the epluspls and epluszsz files are not
				//simple decreasing curves but appear as amost random fluctuations.
				isPulseZoneSizing = ( CompLoadReportIsReq && ( iZoneCalcIter == 1 ) );

				Available = true;

				ResetEnvironmentCounter();
				CurOverallSimDay = 0;
				NumSizingPeriodsPerformed = 0;
				while ( Available ) { // loop over environments

					GetNextEnvironment( Available, ErrorsFound ); // get an environment

					if ( ! Available ) break;
					if ( ErrorsFound ) break;

					// check that environment is one of the design days
					if ( KindOfSim == ksRunPeriodWeather ) {
						continue;
					}

					++NumSizingPeriodsPerformed;

					BeginEnvrnFlag = true;
					EndEnvrnFlag = false;
					EndMonthFlag = false;
					WarmupFlag = true;
					DayOfSim = 0;
					DayOfSimChr = "0";
					CurEnvirNumSimDay = 1;
					++CurOverallSimDay;
					while ( ( DayOfSim < NumOfDayInEnvrn ) || ( WarmupFlag ) ) { // Begin day loop ...

						++DayOfSim;
						if ( ! WarmupFlag && DayOfSim > 1 ) {
							++CurEnvirNumSimDay;
						}

						gio::write( DayOfSimChr, fmtLD ) << DayOfSim;
						strip( DayOfSimChr );
						BeginDayFlag = true;
						EndDayFlag = false;

						if ( WarmupFlag ) {
							DisplayString( "Warming up" );
						} else { // (.NOT.WarmupFlag)
							if ( DayOfSim == 1 ) {
								if ( ! isPulseZoneSizing ) {
									DisplayString( "Performing Zone Sizing Simulation" );
									DisplayString( "...for Sizing Period: #" + RoundSigDigits( NumSizingPeriodsPerformed ) + ' ' + EnvironmentName );
								} else {
									DisplayString( "Performing Zone Sizing Simulation for Load Component Report" );
									DisplayString( "...for Sizing Period: #" + RoundSigDigits( NumSizingPeriodsPerformed ) + ' ' + EnvironmentName );
								}
							}
							UpdateZoneSizing( BeginDay );
						}

						for ( HourOfDay = 1; HourOfDay <= 24; ++HourOfDay ) { // Begin hour loop ...

							BeginHourFlag = true;
							EndHourFlag = false;

							for ( TimeStep = 1; TimeStep <= NumOfTimeStepInHour; ++TimeStep ) { // Begin time step (TINC) loop ...

								BeginTimeStepFlag = true;

								// Set the End__Flag variables to true if necessary.  Note that
								// each flag builds on the previous level.  EndDayFlag cannot be
								// .TRUE. unless EndHourFlag is also .TRUE., etc.  Note that the
								// EndEnvrnFlag and the EndSimFlag cannot be set during warmup.
								// Note also that BeginTimeStepFlag, EndTimeStepFlag, and the
								// SubTimeStepFlags can/will be set/reset in the HVAC Manager.

								if ( TimeStep == NumOfTimeStepInHour ) {
									EndHourFlag = true;
									if ( HourOfDay == 24 ) {
										EndDayFlag = true;
										if ( ( ! WarmupFlag ) && ( DayOfSim == NumOfDayInEnvrn ) ) {
											EndEnvrnFlag = true;
										}
									}
								}

								//set flag for pulse used in load component reporting
								doLoadComponentPulseNow = false;
								if ( isPulseZoneSizing ) {
									if ( ! WarmupFlag ) {
										if ( DayOfSim == 1 ) { //first day of sizing period
											if ( HourOfDay == 10 ) { //at 10am
												if ( TimeStep == 1 ) { //first timestep in hour
													doLoadComponentPulseNow = true;
												}
											}
										}
									}
								}

								ManageWeather();

								if ( ! WarmupFlag ) {
									TimeStepInDay = ( HourOfDay - 1 ) * NumOfTimeStepInHour + TimeStep;
									if ( HourOfDay == 1 && TimeStep == 1 ) {
										DesDayWeath( CurOverallSimDay ).DateString = TrimSigDigits( Month ) + '/' + TrimSigDigits( DayOfMonth );
									}
									DesDayWeath( CurOverallSimDay ).Temp( TimeStepInDay ) = OutDryBulbTemp;
									DesDayWeath( CurOverallSimDay ).HumRat( TimeStepInDay ) = OutHumRat;
									DesDayWeath( CurOverallSimDay ).Press( TimeStepInDay ) = OutBaroPress;
								}

								ManageHeatBalance();

								//  After the first iteration of HeatBalance, all the "input" has been gotten
								if ( BeginSimFlag ) {
									if ( GetNumRangeCheckErrorsFound() > 0 ) {
										ShowFatalError( RoutineName + "Out of \"range\" values found in input" );
									}
								}

								BeginHourFlag = false;
								BeginDayFlag = false;
								BeginEnvrnFlag = false;
								BeginSimFlag = false;

							} // ... End time step (TINC) loop.

							PreviousHour = HourOfDay;

						} // ... End hour loop.

						if ( EndDayFlag ) UpdateZoneSizing( EndDay );

						if ( ! WarmupFlag && ( DayOfSim > 0 ) && ( DayOfSim < NumOfDayInEnvrn ) ) {
							++CurOverallSimDay;
						}

					} // ... End day loop.

					LastMonth = Month;
					LastDayOfMonth = DayOfMonth;

				} // ... End environment loop

				if ( NumSizingPeriodsPerformed > 0 ) {
					UpdateZoneSizing( EndZoneSizingCalc );
					ZoneSizingRunDone = true;
				} else {
					ShowSevereError( RoutineName + "No Sizing periods were performed for Zone Sizing. No Zone Sizing calculations saved." );
					ErrorsFound = true;
				}

				if ( isPulseZoneSizing && runZeroingOnce ) {
					RezeroZoneSizingArrays(); //zero all arrays related to zone sizing.
					runZeroingOnce = false;
				}
			} //loop that repeats the zone sizing calcs for the load component report, if requested

			// both the pulse and normal zone sizing is complete so now post processing of the results is performed
			if ( CompLoadReportIsReq ) {
				// call the routine that computes the decay curve
				ComputeLoadComponentDecayCurve();
				// remove some of the arrays used to derive the decay curves
				DeallocateLoadComponentArrays();
			}
		}

		ZoneSizingCalc = false;
		DoOutputReporting = false;
		Month = LastMonth;
		DayOfMonth = LastDayOfMonth;

		if ( ( DoSystemSizing ) && ( NumSysSizInput == 0 ) && ( NumAirLoops > 0 ) ) {
			ShowWarningError( RoutineName + "For a system sizing run, there must be at least 1 Sizing:System object input. SimulationControl System Sizing option ignored." );
		}

		if ( ( NumSysSizInput > 0 ) && ( DoSystemSizing || DoPlantSizing ) && ! ErrorsFound ) {

			ShowMessage( "Beginning System Sizing Calculations" );

			SysSizingCalc = true;
			Available = true;
			OutputFileSysSizing = GetNewUnitNumber();
			if ( SizingFileColSep == CharComma ) {
				{ IOFlags flags; flags.ACTION( "write" ); gio::open( OutputFileSysSizing, DataStringGlobals::outputSszCsvFileName, flags ); write_stat = flags.ios(); }
				if ( write_stat != 0 ) {
					ShowFatalError( RoutineName + "Could not open file "+ DataStringGlobals::outputSszCsvFileName +" for output (write)." );
				}
			} else if ( SizingFileColSep == CharTab ) {
				{ IOFlags flags; flags.ACTION( "write" ); gio::open( OutputFileSysSizing, DataStringGlobals::outputSszTabFileName, flags ); write_stat = flags.ios(); }
				if ( write_stat != 0 ) {
					ShowFatalError( RoutineName + "Could not open file "+ DataStringGlobals::outputSszTabFileName +" for output (write)." );
				}
			} else {
				{ IOFlags flags; flags.ACTION( "write" ); gio::open( OutputFileSysSizing, DataStringGlobals::outputSszTxtFileName, flags ); write_stat = flags.ios(); }
				if ( write_stat != 0 ) {
					ShowFatalError( RoutineName + "Could not open file "+DataStringGlobals::outputSszTxtFileName+ " for output (write)." );
				}
			}
			SimAir = true;
			SimZoneEquip = true;

			ManageZoneEquipment( true, SimZoneEquip, SimAir );
			ManageAirLoops( true, SimAir, SimZoneEquip );
			if ( GetNumRangeCheckErrorsFound() > 0 ) {
				ShowFatalError( RoutineName + "Out of \"range\" values found in input" );
			}

			ResetEnvironmentCounter();
			CurEnvirNumSimDay = 0;
			CurOverallSimDay = 0;
			NumSizingPeriodsPerformed = 0;
			while ( Available ) { // loop over environments

				GetNextEnvironment( Available, ErrorsFound ); // get an environment

				// check that environment is one of the design days
				if ( KindOfSim == ksRunPeriodWeather ) {
					continue;
				}

				if ( ! Available ) break;
				if ( ErrorsFound ) break;

				++NumSizingPeriodsPerformed;

				BeginEnvrnFlag = true;
				EndEnvrnFlag = false;
				WarmupFlag = false;
				DayOfSim = 0;
				DayOfSimChr = "0";
				CurEnvirNumSimDay = 1;
				++CurOverallSimDay;

				while ( ( DayOfSim < NumOfDayInEnvrn ) || ( WarmupFlag ) ) { // Begin day loop ...

					++DayOfSim;
					if ( ! WarmupFlag && DayOfSim > 1 ) {
						++CurEnvirNumSimDay;
					}
					gio::write( DayOfSimChr, fmtLD ) << DayOfSim;
					strip( DayOfSimChr );
					BeginDayFlag = true;
					EndDayFlag = false;

					if ( WarmupFlag ) {
						DisplayString( "Warming up" );
					} else { // (.NOT.WarmupFlag)
						if ( DayOfSim == 1 ) {
							DisplayString( "Calculating System sizing" );
							DisplayString( "...for Sizing Period: #" + RoundSigDigits( NumSizingPeriodsPerformed ) + ' ' + EnvironmentName );
						}
						UpdateSysSizing( BeginDay );
					}

					for ( HourOfDay = 1; HourOfDay <= 24; ++HourOfDay ) { // Begin hour loop ...

						BeginHourFlag = true;
						EndHourFlag = false;

						for ( TimeStep = 1; TimeStep <= NumOfTimeStepInHour; ++TimeStep ) { // Begin time step (TINC) loop ...

							BeginTimeStepFlag = true;

							// Set the End__Flag variables to true if necessary.  Note that
							// each flag builds on the previous level.  EndDayFlag cannot be
							// .TRUE. unless EndHourFlag is also .TRUE., etc.  Note that the
							// EndEnvrnFlag and the EndSimFlag cannot be set during warmup.

							if ( TimeStep == NumOfTimeStepInHour ) {
								EndHourFlag = true;
								if ( HourOfDay == 24 ) {
									EndDayFlag = true;
									if ( ( ! WarmupFlag ) && ( DayOfSim == NumOfDayInEnvrn ) ) {
										EndEnvrnFlag = true;
									}
								}
							}

							ManageWeather();

							UpdateSysSizing( DuringDay );

							BeginHourFlag = false;
							BeginDayFlag = false;
							BeginEnvrnFlag = false;

						} // ... End time step (TINC) loop.

						PreviousHour = HourOfDay;

					} // ... End hour loop.

					if ( EndDayFlag ) UpdateSysSizing( EndDay );

					if ( ! WarmupFlag && ( DayOfSim > 0 ) && ( DayOfSim < NumOfDayInEnvrn ) ) {
						++CurOverallSimDay;
					}

				} // ... End day loop.

			} // ... End environment loop

			if ( NumSizingPeriodsPerformed > 0 ) {
				UpdateSysSizing( EndSysSizingCalc );
				SysSizingRunDone = true;
			} else {
				ShowSevereError( RoutineName + "No Sizing periods were performed for System Sizing. No System Sizing calculations saved." );
				ErrorsFound = true;
			}
		}
		SysSizingCalc = false;

		// report sizing results to eio file
		if ( ZoneSizingRunDone ) {
			for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
				if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;
				ZoneNum = FinalZoneSizing( CtrlZoneNum ).ActualZoneNum;
				if ( FinalZoneSizing( CtrlZoneNum ).DesCoolVolFlow > 0.0 ) {
					TimeStepAtPeak = FinalZoneSizing( CtrlZoneNum ).TimeStepNumAtCoolMax;
					DDNum = FinalZoneSizing( CtrlZoneNum ).CoolDDNum;
					if ( DDNum > 0 && TimeStepAtPeak > 0 ) {
						TempAtPeak = DesDayWeath( DDNum ).Temp( TimeStepAtPeak );
						HumRatAtPeak = DesDayWeath( DDNum ).HumRat( TimeStepAtPeak );
						DOASHeatGainRateAtClPk = CalcZoneSizing( DDNum, CtrlZoneNum ).DOASHeatAddSeq( TimeStepAtPeak );
						TStatSetPtAtPk = ZoneSizing( DDNum, CtrlZoneNum ).CoolTstatTempSeq( TimeStepAtPeak );
					} else {
						TempAtPeak = 0.0;
						HumRatAtPeak = 0.0;
						DOASHeatGainRateAtClPk = 0.0;
						TStatSetPtAtPk = 0.0;
					}
					ReportZoneSizing( FinalZoneSizing( CtrlZoneNum ).ZoneName, "Cooling", CalcFinalZoneSizing( CtrlZoneNum ).DesCoolLoad,
						FinalZoneSizing( CtrlZoneNum ).DesCoolLoad, CalcFinalZoneSizing( CtrlZoneNum ).DesCoolVolFlow,
						FinalZoneSizing( CtrlZoneNum ).DesCoolVolFlow, FinalZoneSizing( CtrlZoneNum ).CoolDesDay, CoolPeakDateHrMin( CtrlZoneNum ),
						TempAtPeak, HumRatAtPeak, Zone( ZoneNum ).FloorArea, Zone( ZoneNum ).TotOccupants,
						FinalZoneSizing( CtrlZoneNum ).MinOA, DOASHeatGainRateAtClPk );
					curName = FinalZoneSizing( CtrlZoneNum ).ZoneName;
					PreDefTableEntry( pdchZnClCalcDesLd, curName, CalcFinalZoneSizing( CtrlZoneNum ).DesCoolLoad );
					PreDefTableEntry( pdchZnClUserDesLd, curName, FinalZoneSizing( CtrlZoneNum ).DesCoolLoad );
					if ( Zone( ZoneNum ).FloorArea != 0.0 ) {
						PreDefTableEntry( pdchZnClUserDesLdPerArea, curName, FinalZoneSizing( CtrlZoneNum ).DesCoolLoad / Zone( ZoneNum ).FloorArea );
					}
					PreDefTableEntry( pdchZnClCalcDesAirFlow, curName, CalcFinalZoneSizing( CtrlZoneNum ).DesCoolVolFlow, 3 );
					PreDefTableEntry( pdchZnClUserDesAirFlow, curName, FinalZoneSizing( CtrlZoneNum ).DesCoolVolFlow, 3 );
					PreDefTableEntry( pdchZnClDesDay, curName, FinalZoneSizing( CtrlZoneNum ).CoolDesDay );
					PreDefTableEntry( pdchZnClPkTime, curName, CoolPeakDateHrMin( CtrlZoneNum ) );
					PreDefTableEntry( pdchZnClPkTstatTemp, curName, TStatSetPtAtPk );
					PreDefTableEntry( pdchZnClPkIndTemp, curName, CalcFinalZoneSizing( CtrlZoneNum ).ZoneTempAtCoolPeak );
					PreDefTableEntry( pdchZnClPkIndHum, curName, CalcFinalZoneSizing( CtrlZoneNum ).ZoneHumRatAtCoolPeak, 5 );
					PreDefTableEntry( pdchZnClPkOATemp, curName, TempAtPeak );
					PreDefTableEntry( pdchZnClPkOAHum, curName, HumRatAtPeak, 5 );
					PreDefTableEntry(pdchZnClPkOAMinFlow, curName, FinalZoneSizing( CtrlZoneNum ).MinOA, 3 );
					PreDefTableEntry( pdchZnClPkDOASHeatGain, curName, DOASHeatGainRateAtClPk );
				}
				if ( FinalZoneSizing( CtrlZoneNum ).DesHeatVolFlow > 0.0 ) {
					TimeStepAtPeak = FinalZoneSizing( CtrlZoneNum ).TimeStepNumAtHeatMax;
					DDNum = FinalZoneSizing( CtrlZoneNum ).HeatDDNum;
					if ( DDNum > 0 && TimeStepAtPeak > 0 ) {
						TempAtPeak = DesDayWeath( DDNum ).Temp( TimeStepAtPeak );
						HumRatAtPeak = DesDayWeath( DDNum ).HumRat( TimeStepAtPeak );
						DOASHeatGainRateAtHtPk = CalcZoneSizing( DDNum, CtrlZoneNum ).DOASHeatAddSeq( TimeStepAtPeak );
						TStatSetPtAtPk = ZoneSizing( DDNum, CtrlZoneNum ).HeatTstatTempSeq( TimeStepAtPeak );
					} else {
						TempAtPeak = 0.0;
						HumRatAtPeak = 0.0;
						DOASHeatGainRateAtHtPk = 0.0;
						TStatSetPtAtPk = 0.0;
					}
					ReportZoneSizing( FinalZoneSizing( CtrlZoneNum ).ZoneName, "Heating", CalcFinalZoneSizing( CtrlZoneNum ).DesHeatLoad,
						FinalZoneSizing( CtrlZoneNum ).DesHeatLoad, CalcFinalZoneSizing( CtrlZoneNum ).DesHeatVolFlow,
						FinalZoneSizing( CtrlZoneNum ).DesHeatVolFlow, FinalZoneSizing( CtrlZoneNum ).HeatDesDay, HeatPeakDateHrMin( CtrlZoneNum ),
						TempAtPeak, HumRatAtPeak, Zone( ZoneNum ).FloorArea, Zone( ZoneNum ).TotOccupants,
						FinalZoneSizing( CtrlZoneNum ).MinOA, DOASHeatGainRateAtHtPk );
					curName = FinalZoneSizing( CtrlZoneNum ).ZoneName;
					PreDefTableEntry( pdchZnHtCalcDesLd, curName, CalcFinalZoneSizing( CtrlZoneNum ).DesHeatLoad );
					PreDefTableEntry( pdchZnHtUserDesLd, curName, FinalZoneSizing( CtrlZoneNum ).DesHeatLoad );
					if ( Zone( ZoneNum ).FloorArea != 0.0 ) {
						PreDefTableEntry( pdchZnHtUserDesLdPerArea, curName, FinalZoneSizing( CtrlZoneNum ).DesHeatLoad / Zone( ZoneNum ).FloorArea );
					}
					PreDefTableEntry( pdchZnHtCalcDesAirFlow, curName, CalcFinalZoneSizing( CtrlZoneNum ).DesHeatVolFlow, 3 );
					PreDefTableEntry( pdchZnHtUserDesAirFlow, curName, FinalZoneSizing( CtrlZoneNum ).DesHeatVolFlow, 3 );
					PreDefTableEntry( pdchZnHtDesDay, curName, FinalZoneSizing( CtrlZoneNum ).HeatDesDay );
					PreDefTableEntry( pdchZnHtPkTime, curName, HeatPeakDateHrMin( CtrlZoneNum ) );
					PreDefTableEntry( pdchZnHtPkTstatTemp, curName, TStatSetPtAtPk );
					PreDefTableEntry( pdchZnHtPkIndTemp, curName, CalcFinalZoneSizing( CtrlZoneNum ).ZoneTempAtHeatPeak );
					PreDefTableEntry( pdchZnHtPkIndHum, curName, CalcFinalZoneSizing( CtrlZoneNum ).ZoneHumRatAtHeatPeak, 5 );
					PreDefTableEntry( pdchZnHtPkOATemp, curName, TempAtPeak );
					PreDefTableEntry( pdchZnHtPkOAHum, curName, HumRatAtPeak, 5 );
					PreDefTableEntry( pdchZnHtPkOAMinFlow, curName, FinalZoneSizing( CtrlZoneNum ).MinOA, 3 );
					PreDefTableEntry( pdchZnHtPkDOASHeatGain, curName, DOASHeatGainRateAtHtPk );
				}
			}
			// Deallocate arrays no longer needed
			ZoneSizing.deallocate();
			// CalcZoneSizing.deallocate();
		}
		if ( SysSizingRunDone ) {
			for ( AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum ) {
				curName = FinalSysSizing( AirLoopNum ).AirPriLoopName;
				ReportSysSizing( curName, "Calculated Cooling Design Air Flow Rate [m3/s]", CalcSysSizing( AirLoopNum ).DesCoolVolFlow );
				PreDefTableEntry( pdchSysSizCalcClAir, curName, CalcSysSizing( AirLoopNum ).DesCoolVolFlow );
				if ( std::abs( CalcSysSizing( AirLoopNum ).DesCoolVolFlow ) <= 1.e-8 ) {
					ShowWarningError( RoutineName + "Calculated Cooling Design Air Flow Rate for System=" + FinalSysSizing( AirLoopNum ).AirPriLoopName + " is zero." );
					ShowContinueError( "Check Sizing:Zone and ZoneControl:Thermostat inputs." );
				}
				ReportSysSizing( curName, "User Cooling Design Air Flow Rate [m3/s]", FinalSysSizing( AirLoopNum ).DesCoolVolFlow );
				PreDefTableEntry( pdchSysSizUserClAir, curName, FinalSysSizing( AirLoopNum ).DesCoolVolFlow );
				ReportSysSizing( curName, "Calculated Heating Design Air Flow Rate [m3/s]", CalcSysSizing( AirLoopNum ).DesHeatVolFlow );
				PreDefTableEntry( pdchSysSizCalcHtAir, curName, CalcSysSizing( AirLoopNum ).DesHeatVolFlow );
				if ( std::abs( CalcSysSizing( AirLoopNum ).DesHeatVolFlow ) <= 1.e-8 ) {
					ShowWarningError( RoutineName + "Calculated Heating Design Air Flow Rate for System=" + FinalSysSizing( AirLoopNum ).AirPriLoopName + " is zero." );
					ShowContinueError( "Check Sizing:Zone and ZoneControl:Thermostat inputs." );
				}
				ReportSysSizing( curName, "User Heating Design Air Flow Rate [m3/s]", FinalSysSizing( AirLoopNum ).DesHeatVolFlow );
				PreDefTableEntry( pdchSysSizUserHtAir, curName, FinalSysSizing( AirLoopNum ).DesHeatVolFlow );
			}
			// Deallocate arrays no longer needed
			SysSizing.deallocate();
		}

		if ( ( DoPlantSizing ) && ( NumPltSizInput == 0 ) ) {
			ShowWarningError( RoutineName + "For a plant sizing run, there must be at least 1 Sizing:Plant object input. SimulationControl Plant Sizing option ignored." );
		}

		if ( ( NumPltSizInput > 0 ) && ( DoPlantSizing ) && ! ErrorsFound ) {

			ShowMessage( "Beginning Plant Sizing Calculations" );

		}

		if ( ErrorsFound ) {
			ShowFatalError( "Program terminates due to preceding conditions." );
		}

	}

	void
	GetOARequirements()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         R. Raustad - FSEC
		//       DATE WRITTEN   February 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains input data for the OA Requirements object and stores it in
		// appropriate data structure.

		// METHODOLOGY EMPLOYED:
		// Uses InputProcessor "Get" routines to obtain data.
		// This object requires only a name where the default values are assumed
		// if subsequent fields are not entered.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectDefMaxArgs;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using ScheduleManager::GetScheduleIndex;
		using ScheduleManager::CheckScheduleValueMinMax;
		using ScheduleManager::GetScheduleMaxValue;
		using namespace DataIPShortCuts;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetOARequirements: " ); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int TotalArgs; // Total number of alpha and numeric arguments (max) for a
		int IOStatus; // Used in GetObjectItem
		int OAIndex;
		static bool ErrorsFound( false ); // If errors detected in input
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		//  REAL(r64) :: CalcAmt

		std::string CurrentModuleObject; // for ease in getting objects
		Array1D_string Alphas; // Alpha input items for object
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D< Real64 > Numbers; // Numeric input items for object
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.

		CurrentModuleObject = "DesignSpecification:OutdoorAir";
		NumOARequirements = GetNumObjectsFound( CurrentModuleObject );
		GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );

		Alphas.allocate( NumAlphas );
		cAlphaFields.allocate( NumAlphas );
		cNumericFields.allocate( NumNumbers );
		Numbers.dimension( NumNumbers, 0.0 );
		lAlphaBlanks.dimension( NumAlphas, true );
		lNumericBlanks.dimension( NumNumbers, true );

		if ( NumOARequirements > 0 ) {
			OARequirements.allocate( NumOARequirements );

			//Start Loading the System Input
			for ( OAIndex = 1; OAIndex <= NumOARequirements; ++OAIndex ) {

				GetObjectItem( CurrentModuleObject, OAIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

				VerifyName( Alphas( 1 ), OARequirements, OAIndex - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) Alphas( 1 ) = "xxxxx";
				}
				OARequirements( OAIndex ).Name = Alphas( 1 );

				ProcessInputOARequirements( CurrentModuleObject, OAIndex, Alphas, NumAlphas, Numbers, NumNumbers, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields, ErrorsFound );

			}

			Alphas.deallocate();
			cAlphaFields.deallocate();
			cNumericFields.deallocate();
			Numbers.deallocate();
			lAlphaBlanks.deallocate();
			lNumericBlanks.deallocate();

			if ( ErrorsFound ) {
				ShowFatalError( RoutineName + "Errors found in input.  Preceding condition(s) cause termination." );
			}

		}

	}

	void
	ProcessInputOARequirements(
		std::string const & CurrentModuleObject,
		int const OAIndex,
		Array1_string const & Alphas,
		int & NumAlphas,
		Array1< Real64 > const & Numbers,
		int & NumNumbers,
		Array1_bool const & EP_UNUSED( lNumericBlanks ), //Unused
		Array1_bool const & lAlphaBlanks,
		Array1_string const & cAlphaFields,
		Array1_string const & EP_UNUSED( cNumericFields ), //Unused
		bool & ErrorsFound // If errors found in input
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         R. Raustad - FSEC
		//       DATE WRITTEN   February 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains input data for the OA Requirements object and stores it in
		// appropriate data structure.

		// METHODOLOGY EMPLOYED:
		// Uses InputProcessor "Get" routines to obtain data.
		// This object requires only a name where the default values are assumed
		// if subsequent fields are not entered.

		// REFERENCES:
		// na

		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using ScheduleManager::GetScheduleIndex;
		using ScheduleManager::CheckScheduleValueMinMax;
		using ScheduleManager::GetScheduleMaxValue;
		using namespace DataIPShortCuts;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetOARequirements: " ); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na



		if ( NumAlphas > 1 ) {
			if ( SameString( Alphas( 2 ), "Flow/Person" ) ) {
				OARequirements( OAIndex ).OAFlowMethod = OAFlowPPer;
			} else if ( SameString( Alphas( 2 ), "Flow/Zone" ) ) {
				OARequirements( OAIndex ).OAFlowMethod = OAFlow;
			} else if ( SameString( Alphas( 2 ), "Flow/Area" ) ) {
				OARequirements( OAIndex ).OAFlowMethod = OAFlowPerArea;
			} else if ( SameString( Alphas( 2 ), "AirChanges/Hour" ) ) {
				OARequirements( OAIndex ).OAFlowMethod = OAFlowACH;
			} else if ( SameString( Alphas( 2 ), "Sum" ) ) {
				OARequirements( OAIndex ).OAFlowMethod = OAFlowSum;
			} else if ( SameString( Alphas( 2 ), "Maximum" ) ) {
				OARequirements( OAIndex ).OAFlowMethod = OAFlowMax;
			} else {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + OARequirements( OAIndex ).Name + "\"," );
				ShowContinueError( "...Invalid " + cAlphaFields( 2 ) + "=\"" + Alphas( 2 ) + "\"," );
				ShowContinueError( "...Valid choices are Flow/Person, Flow/Zone, Flow/Area, AirChanges/Hour, Sum, Maximum." );
				ErrorsFound = true;
			}
		} else {
			// default value for Outdoor Air Method
			OARequirements( OAIndex ).OAFlowMethod = OAFlowPPer;
		}
		if ( NumNumbers > 0 ) {
			OARequirements( OAIndex ).OAFlowPerPerson = Numbers( 1 );
		} else {
			// default value for Outdoor Air Flow per Person when per person flow is counted
			OARequirements( OAIndex ).OAFlowPerPerson = 0.00944;
		}
		// if one of the methods that should not use the flow per person field is chosen then zero out the flow per person to avoid it
		// being counted later #4378
		if ( OARequirements( OAIndex ).OAFlowMethod != OAFlowPPer && OARequirements( OAIndex ).OAFlowMethod != OAFlowSum && OARequirements( OAIndex ).OAFlowMethod != OAFlowMax ) {
			OARequirements( OAIndex ).OAFlowPerPerson = 0.0;
		}
		// remaining fields default to 0
		if ( NumNumbers > 1 ) {
			if ( OARequirements( OAIndex ).OAFlowMethod == OAFlowPerArea || OARequirements( OAIndex ).OAFlowMethod == OAFlowSum || OARequirements( OAIndex ).OAFlowMethod == OAFlowMax ) {
				OARequirements( OAIndex ).OAFlowPerArea = Numbers( 2 );
			} else {
				OARequirements( OAIndex ).OAFlowPerArea = 0.0;
			}
		}
		if ( NumNumbers > 2 ) {
			if ( OARequirements( OAIndex ).OAFlowMethod == OAFlow || OARequirements( OAIndex ).OAFlowMethod == OAFlowSum || OARequirements( OAIndex ).OAFlowMethod == OAFlowMax ) {
				OARequirements( OAIndex ).OAFlowPerZone = Numbers( 3 );
			} else {
				OARequirements( OAIndex ).OAFlowPerZone = 0.0;
			}
		}
		if ( NumNumbers > 3 ) {
			if ( OARequirements( OAIndex ).OAFlowMethod == OAFlowACH || OARequirements( OAIndex ).OAFlowMethod == OAFlowSum || OARequirements( OAIndex ).OAFlowMethod == OAFlowMax ) {
				OARequirements( OAIndex ).OAFlowACH = Numbers( 4 );
			} else {
				OARequirements( OAIndex ).OAFlowACH = 0.0;
			}
		}
		if ( NumAlphas > 2 ) {
			if ( !lAlphaBlanks( 3 ) ) {
				OARequirements( OAIndex ).OAFlowFracSchPtr = GetScheduleIndex( Alphas( 3 ) );
				if ( OARequirements( OAIndex ).OAFlowFracSchPtr > 0 ) {
					if ( !CheckScheduleValueMinMax( OARequirements( OAIndex ).OAFlowFracSchPtr, ">=", 0.0, "<=", 1.0 ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + OARequirements( OAIndex ).Name + "\"," );
						ShowContinueError( "Error found in " + cAlphaFields( 3 ) + " = " + Alphas( 3 ) );
						ShowContinueError( "Schedule values must be (>=0., <=1.)" );
						ErrorsFound = true;
					} else {
						OARequirements( OAIndex ).MaxOAFractionSchValue = GetScheduleMaxValue( OARequirements( OAIndex ).OAFlowFracSchPtr );
					}
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + OARequirements( OAIndex ).Name + "\"," );
					ShowContinueError( "...Not Found " + cAlphaFields( 3 ) + "=\"" + Alphas( 3 ) + "\"." );
					ErrorsFound = true;
				}
			}
		}

	}


	void
	GetZoneAirDistribution()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         T. Hong - LBNL
		//       DATE WRITTEN   March 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains input data for the zone air distribution objects and stores it in
		// appropriate data structure.

		// METHODOLOGY EMPLOYED:
		// Uses InputProcessor "Get" routines to obtain data.
		// This object requires only a name where the default values are assumed
		// if subsequent fields are not entered.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectDefMaxArgs;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using ScheduleManager::GetScheduleIndex;
		using ScheduleManager::CheckScheduleValueMinMax;
		using namespace DataIPShortCuts;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetZoneAirDistribution: " ); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int TotalArgs; // Total number of alpha and numeric arguments (max) for a
		int IOStatus; // Used in GetObjectItem
		int ZADIndex;
		static bool ErrorsFound( false ); // If errors detected in input
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name

		std::string CurrentModuleObject; // for ease in getting objects
		Array1D_string Alphas; // Alpha input items for object
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D< Real64 > Numbers; // Numeric input items for object
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.

		CurrentModuleObject = "DesignSpecification:ZoneAirDistribution";
		NumZoneAirDistribution = GetNumObjectsFound( CurrentModuleObject );
		GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );

		Alphas.allocate( NumAlphas );
		cAlphaFields.allocate( NumAlphas );
		cNumericFields.allocate( NumNumbers );
		Numbers.dimension( NumNumbers, 0.0 );
		lAlphaBlanks.dimension( NumAlphas, true );
		lNumericBlanks.dimension( NumNumbers, true );

		if ( NumZoneAirDistribution > 0 ) {
			ZoneAirDistribution.allocate( NumZoneAirDistribution );

			//Start Loading the zone air distribution input
			for ( ZADIndex = 1; ZADIndex <= NumZoneAirDistribution; ++ZADIndex ) {

				GetObjectItem( CurrentModuleObject, ZADIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

				VerifyName( Alphas( 1 ), ZoneAirDistribution, ZADIndex - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) Alphas( 1 ) = "xxxxx";
				}

				ZoneAirDistribution( ZADIndex ).Name = Alphas( 1 );

				// Zone Air Distribution Effectiveness in Cooling Mode
				if ( NumNumbers > 0 ) {
					ZoneAirDistribution( ZADIndex ).ZoneADEffCooling = Numbers( 1 );
				} else {
					// default value
					ZoneAirDistribution( ZADIndex ).ZoneADEffCooling = 1.0;
				}

				// Zone Air Distribution Effectiveness in Heating Mode
				if ( NumNumbers > 1 ) {
					ZoneAirDistribution( ZADIndex ).ZoneADEffHeating = Numbers( 2 );
				} else {
					// default value
					ZoneAirDistribution( ZADIndex ).ZoneADEffHeating = 1.0;
				}

				// Zone Secondary Recirculation Fraction
				if ( NumNumbers > 2 ) {
					ZoneAirDistribution( ZADIndex ).ZoneSecondaryRecirculation = Numbers( 3 );
				} else {
					// default value
					ZoneAirDistribution( ZADIndex ).ZoneSecondaryRecirculation = 0.0;
				}

				if ( NumAlphas > 1 ) {
					if ( ! lAlphaBlanks( 2 ) ) {
						ZoneAirDistribution( ZADIndex ).ZoneADEffSchName = Alphas( 2 );
						ZoneAirDistribution( ZADIndex ).ZoneADEffSchPtr = GetScheduleIndex( Alphas( 2 ) );
						if ( ZoneAirDistribution( ZADIndex ).ZoneADEffSchPtr > 0 ) {
							if ( ! CheckScheduleValueMinMax( ZoneAirDistribution( ZADIndex ).ZoneADEffSchPtr, ">", 0.0 ) ) {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + ZoneAirDistribution( ZADIndex ).Name + "\"," );
								ShowContinueError( "Error found in " + cAlphaFields( 2 ) + " = " + Alphas( 2 ) );
								ShowContinueError( "Schedule values must be >0.0)" );
								ErrorsFound = true;
							}
						} else {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + ZoneAirDistribution( ZADIndex ).Name + "\"," );
							ShowContinueError( "...Not Found " + cAlphaFields( 2 ) + "=\"" + Alphas( 2 ) + "\"." );
							ErrorsFound = true;
						}
					}
				}

			}

			Alphas.deallocate();
			cAlphaFields.deallocate();
			cNumericFields.deallocate();
			Numbers.deallocate();
			lAlphaBlanks.deallocate();
			lNumericBlanks.deallocate();

			if ( ErrorsFound ) {
				ShowFatalError( RoutineName + "Errors found in input.  Preceding condition(s) cause termination." );
			}

		}

	}

	void
	GetSizingParams()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   January 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains input data for the Sizing Parameters object and stores it in
		// appropriate data structure.

		// METHODOLOGY EMPLOYED:
		// Uses InputProcessor "Get" routines to obtain data.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using namespace DataIPShortCuts;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt fmtA( "(A)" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int IOStatus; // Used in GetObjectItem
		int NumSizParams;
		int Temp;

		cCurrentModuleObject = "Sizing:Parameters";
		NumSizParams = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumSizParams == 1 ) {
			GetObjectItem( cCurrentModuleObject, 1, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			if ( lNumericFieldBlanks( 1 ) || rNumericArgs( 1 ) < 0.0 ) {
				GlobalHeatSizingFactor = 1.0;
			} else {
				GlobalHeatSizingFactor = rNumericArgs( 1 );
			}
			if ( lNumericFieldBlanks( 2 ) || rNumericArgs( 2 ) < 0.0 ) {
				GlobalCoolSizingFactor = 1.0;
			} else {
				GlobalCoolSizingFactor = rNumericArgs( 2 );
			}
			if ( lNumericFieldBlanks( 3 ) || rNumericArgs( 3 ) <= 0.0 ) {
				NumTimeStepsInAvg = NumOfTimeStepInHour;
			} else {
				NumTimeStepsInAvg = int( rNumericArgs( 3 ) );
			}
		} else if ( NumSizParams == 0 ) {
			GlobalHeatSizingFactor = 1.0;
			GlobalCoolSizingFactor = 1.0;
			NumTimeStepsInAvg = NumOfTimeStepInHour;
		} else {
			ShowFatalError( cCurrentModuleObject + ": More than 1 occurence of this object; only 1 allowed" );
		}

		if ( NumTimeStepsInAvg < NumOfTimeStepInHour ) {
			ShowWarningError( cCurrentModuleObject + ": note " + cNumericFieldNames( 3 ) + " entered value=[" + RoundSigDigits( NumTimeStepsInAvg ) + "] is less than 1 hour (i.e., " + RoundSigDigits( NumOfTimeStepInHour ) + " timesteps)." );
		}

		cCurrentModuleObject = "OutputControl:Sizing:Style";
		Temp = GetNumObjectsFound( cCurrentModuleObject );

		if ( Temp == 0 ) {
			cAlphaArgs( 1 ) = "Comma";
			SizingFileColSep = CharComma; //comma
		} else if ( Temp == 1 ) {
			GetObjectItem( cCurrentModuleObject, 1, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			if ( cAlphaArgs( 1 ) == "COMMA" ) {
				SizingFileColSep = CharComma; //comma
				cAlphaArgs( 1 ) = "Comma";
			} else if ( cAlphaArgs( 1 ) == "TAB" ) {
				SizingFileColSep = CharTab; //tab
				cAlphaArgs( 1 ) = "Tab";
			} else if ( cAlphaArgs( 1 ) == "FIXED" || cAlphaArgs( 1 ) == "SPACE" ) {
				SizingFileColSep = CharSpace; // space
				cAlphaArgs( 1 ) = "Space";
			} else {
				SizingFileColSep = CharComma; //comma
				ShowWarningError( cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 1 ) + " entered value=\"" + cAlphaArgs( 1 ) + "\", Commas will be used to separate fields." );
				cAlphaArgs( 1 ) = "Comma";
			}
			gio::write( OutputFileInits, fmtA ) << "! <Sizing Output Files>,Style";
			gio::write( OutputFileInits, "('Sizing Output Files,',A)" ) << cAlphaArgs( 1 );
		}

	}

	void
	GetZoneSizingInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   December 2000
		//       MODIFIED       Mangesh Basarkar, 06/2011: Specifying zone outside air based on design specification object
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains input data for zone sizing objects and stores it in
		// appropriate data structures.

		// METHODOLOGY EMPLOYED:
		// Uses InputProcessor "Get" routines to obtain data.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using namespace DataIPShortCuts;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ZoneSizIndex; // loop index
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int IOStatus; // Used in GetObjectItem
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		int NumDesDays; // Number of design days in input
		int NumSizingZoneStatements;
		int Item;
		int Item1;
		int ZLItem;
		bool errFlag;
		Array1D_string ZoneNames;
		int NumZones;
		int NumZoneLists;
		int OAIndex; // Index of design specification object
		int ObjIndex; // Index of zone air distribution effectiveness object name

		struct GlobalMiscObject
		{
			// Members
			std::string Name;
			int ZoneOrZoneListPtr;
			int NumOfZones;
			int StartPtr;
			bool ZoneListActive;

			// Default Constructor
			GlobalMiscObject() :
				ZoneOrZoneListPtr( 0 ),
				NumOfZones( 0 ),
				StartPtr( 0 ),
				ZoneListActive( false )
			{}

		};

		// Object Data
		Array1D< ZoneListData > ZoneListNames;
		Array1D< GlobalMiscObject > SizingZoneObjects;

		cCurrentModuleObject = "Sizing:Zone";
		NumSizingZoneStatements = GetNumObjectsFound( cCurrentModuleObject );
		SizingZoneObjects.allocate( NumSizingZoneStatements );

		if ( NumSizingZoneStatements > 0 ) {
			errFlag = false;
			GetZoneAndZoneListNames( errFlag, NumZones, ZoneNames, NumZoneLists, ZoneListNames );
		}

		cCurrentModuleObject = "Sizing:Zone";
		NumZoneSizingInput = 0;
		errFlag = false;
		for ( Item = 1; Item <= NumSizingZoneStatements; ++Item ) {
			GetObjectItem( cCurrentModuleObject, Item, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), SizingZoneObjects, Item - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				errFlag = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			SizingZoneObjects( Item ).Name = cAlphaArgs( 1 );

			Item1 = FindItemInList( cAlphaArgs( 1 ), ZoneNames, NumZones );
			ZLItem = 0;
			if ( Item1 == 0 && NumZoneLists > 0 ) ZLItem = FindItemInList( cAlphaArgs( 1 ), ZoneListNames );
			if ( Item1 > 0 ) {
				SizingZoneObjects( Item ).StartPtr = NumZoneSizingInput + 1;
				++NumZoneSizingInput;
				SizingZoneObjects( Item ).NumOfZones = 1;
				SizingZoneObjects( Item ).ZoneListActive = false;
				SizingZoneObjects( Item ).ZoneOrZoneListPtr = Item1;
			} else if ( ZLItem > 0 ) {
				SizingZoneObjects( Item ).StartPtr = NumZoneSizingInput + 1;
				NumZoneSizingInput += ZoneListNames( ZLItem ).NumOfZones;
				SizingZoneObjects( Item ).NumOfZones = ZoneListNames( ZLItem ).NumOfZones;
				SizingZoneObjects( Item ).ZoneListActive = true;
				SizingZoneObjects( Item ).ZoneOrZoneListPtr = ZLItem;
			} else {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 1 ) + " not found." );
				ErrorsFound = true;
				errFlag = true;
			}
		}

		if ( errFlag ) {
			ShowSevereError( "GetZoneSizingInput: Errors with invalid names in " + cCurrentModuleObject + " objects." );
			ShowContinueError( "...These will not be read in.  Other errors may occur." );
			NumZoneSizingInput = 0;
		}

		if ( NumZoneSizingInput > 0 ) {
			NumDesDays = GetNumObjectsFound( "SizingPeriod:DesignDay" ) + GetNumObjectsFound( "SizingPeriod:WeatherFileDays" ) + GetNumObjectsFound( "SizingPeriod:WeatherFileConditionType" );
			if ( NumDesDays == 0 && ( DoZoneSizing || DoSystemSizing || DoPlantSizing ) ) {
				ShowSevereError( "Zone Sizing calculations need SizingPeriod:* input. None found." );
				ErrorsFound = true;
			}
			ZoneSizingInput.allocate( NumZoneSizingInput );

			ZoneSizIndex = 0;
			for ( Item = 1; Item <= NumSizingZoneStatements; ++Item ) {

				GetObjectItem( cCurrentModuleObject, Item, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				for ( Item1 = 1; Item1 <= SizingZoneObjects( Item ).NumOfZones; ++Item1 ) {
					++ZoneSizIndex;
					if ( ! SizingZoneObjects( Item ).ZoneListActive ) {
						if ( SizingZoneObjects( Item ).ZoneOrZoneListPtr > 0 ) {
							ZoneSizingInput( ZoneSizIndex ).ZoneName = ZoneNames( SizingZoneObjects( Item ).ZoneOrZoneListPtr );
						} else {
							// Invalid zone, will be caught later
							ZoneSizingInput( ZoneSizIndex ).ZoneName = "Invalid Zone Name";
						}
					} else { // Zone list active
						if ( SizingZoneObjects( Item ).ZoneOrZoneListPtr > 0 && ZoneListNames( SizingZoneObjects( Item ).ZoneOrZoneListPtr ).Zones( Item1 ) > 0 ) {
							ZoneSizingInput( ZoneSizIndex ).ZoneName = ZoneNames( ZoneListNames( SizingZoneObjects( Item ).ZoneOrZoneListPtr ).Zones( Item1 ) );
						} else {
							// Invalid zone, will be caught later
							ZoneSizingInput( ZoneSizIndex ).ZoneName = "Invalid Zone Name";
						}
					}
					IsNotOK = false;
					IsBlank = false;
					VerifyName( ZoneSizingInput( ZoneSizIndex ).ZoneName, ZoneSizingInput, &ZoneSizingInputData::ZoneName, ZoneSizIndex - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
					if ( IsNotOK ) {
						ErrorsFound = true;
						if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
					}
					if ( IsNotOK && ! SizingZoneObjects( Item ).ZoneListActive ) {
						ShowContinueError( "Zone may have been entered in a ZoneList assignment." );
					}

					//  A2, \field Zone Cooling Design Supply Air Temperature Input Method
					//      \required-field
					//      \type choice
					//      \key SupplyAirTemperature
					//      \key TemperatureDifference
					//      \default SupplyAirTemperature
					{ auto const coolingSATMethod( cAlphaArgs( 2 ) );
					if ( coolingSATMethod == "SUPPLYAIRTEMPERATURE" ) {
						ZoneSizingInput( ZoneSizIndex ).ZnCoolDgnSAMethod = SupplyAirTemperature;
					} else if ( coolingSATMethod == "TEMPERATUREDIFFERENCE" ) {
						ZoneSizingInput( ZoneSizIndex ).ZnCoolDgnSAMethod = TemperatureDifference;
					} else {
						ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid data." );
						ShowContinueError( "... incorrect " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"" );
						ShowContinueError( "... valid values are SupplyAirTemperature or TemperatureDifference." );
						ErrorsFound = true;
					}}
					//  N1, \field Zone Cooling Design Supply Air Temperature
					//      \type real
					//      \units C
					//      \note Zone Cooling Design Supply Air Temperature is only used when Zone Cooling Design
					//      \note Supply Air Temperature Input Method = SupplyAirTemperature
					if ( lNumericFieldBlanks( 1 ) ) {
						ZoneSizingInput( ZoneSizIndex ).CoolDesTemp = 0.0;
					} else if ( rNumericArgs( 1 ) < 0.0 && ZoneSizingInput( ZoneSizIndex ).ZnCoolDgnSAMethod == SupplyAirTemperature ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid data." );
						ShowContinueError( "... incorrect " + cNumericFieldNames( 1 ) + "=[" + RoundSigDigits( rNumericArgs( 1 ), 2 ) + "],  value should not be negative." );
						ErrorsFound = true;
					} else if ( rNumericArgs( 1 ) >= 0.0 && ZoneSizingInput( ZoneSizIndex ).ZnCoolDgnSAMethod == SupplyAirTemperature ) {
						ZoneSizingInput( ZoneSizIndex ).CoolDesTemp = rNumericArgs( 1 );
					} else {
						ZoneSizingInput( ZoneSizIndex ).CoolDesTemp = 0.0;
					}
					//  N2, \field Zone Cooling Design Supply Air Temperature Difference
					//      \type real
					//      \units delta C
					//      \note Zone Cooling Design Supply Air Temperature is only used when Zone Cooling Design
					//      \note Supply Air Temperature Input Method = TemperatureDifference
					//      \note The absolute of this value is value will be subtracted from room temperature
					//      \note at peak load to calculate Zone Cooling Design Supply Air Temperature.
					if ( lNumericFieldBlanks( 2 ) ) {
						ZoneSizingInput( ZoneSizIndex ).CoolDesTempDiff = 0.0;
					} else if ( ZoneSizingInput( ZoneSizIndex ).ZnCoolDgnSAMethod == TemperatureDifference ) {
						ZoneSizingInput( ZoneSizIndex ).CoolDesTempDiff = rNumericArgs( 2 );
					} else {
						ZoneSizingInput( ZoneSizIndex ).CoolDesTempDiff = 0.0;
					}
					//  A3, \field Zone Heating Design Supply Air Temperature Input Method
					//      \required-field
					//      \type choice
					//      \key SupplyAirTemperature
					//      \key TemperatureDifference
					//      \default SupplyAirTemperature
					{ auto const heatingSATMethod( cAlphaArgs( 3 ) );
					if ( heatingSATMethod == "SUPPLYAIRTEMPERATURE" ) {
						ZoneSizingInput( ZoneSizIndex ).ZnHeatDgnSAMethod = SupplyAirTemperature;
					} else if ( heatingSATMethod == "TEMPERATUREDIFFERENCE" ) {
						ZoneSizingInput( ZoneSizIndex ).ZnHeatDgnSAMethod = TemperatureDifference;
					} else {
						ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid data." );
						ShowContinueError( "... incorrect " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"" );
						ShowContinueError( "... valid values are SupplyAirTemperature or TemperatureDifference." );
						ErrorsFound = true;
					}}
					//  N3, \field Zone Heating Design Supply Air Temperature
					//      \type real
					//      \units C
					//      \note Zone Heating Design Supply Air Temperature is only used when Zone Heating Design
					//      \note Supply Air Temperature Input Method = SupplyAirTemperature
					if ( lNumericFieldBlanks( 3 ) ) {
						ZoneSizingInput( ZoneSizIndex ).HeatDesTemp = 0.0;
					} else if ( rNumericArgs( 3 ) < 0.0 && ZoneSizingInput( ZoneSizIndex ).ZnHeatDgnSAMethod == SupplyAirTemperature ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid data." );
						ShowContinueError( "... incorrect " + cNumericFieldNames( 3 ) + "=[" + RoundSigDigits( rNumericArgs( 3 ), 2 ) + "],  value should not be negative." );
						ErrorsFound = true;
					} else if ( rNumericArgs( 3 ) >= 0.0 && ZoneSizingInput( ZoneSizIndex ).ZnHeatDgnSAMethod == SupplyAirTemperature ) {
						ZoneSizingInput( ZoneSizIndex ).HeatDesTemp = rNumericArgs( 3 );
					} else {
						ZoneSizingInput( ZoneSizIndex ).HeatDesTemp = 0.0;
					}
					//  N4, \field Zone Heating Design Supply Air Temperature Difference
					//      \type real
					//      \units deltaC
					//      \note Zone Heating Design Supply Air Temperature is only used when Zone Heating Design
					//      \note Supply Air Temperature Input Method = TemperatureDifference
					//      \note The absolute of this value is value will be added to room temperature
					//      \note at peak load to calculate Zone Heating Design Supply Air Temperature.
					if ( lNumericFieldBlanks( 4 ) ) {
						ZoneSizingInput( ZoneSizIndex ).HeatDesTempDiff = 0.0;
					} else if ( ZoneSizingInput( ZoneSizIndex ).ZnHeatDgnSAMethod == TemperatureDifference ) {
						ZoneSizingInput( ZoneSizIndex ).HeatDesTempDiff = rNumericArgs( 4 );
					} else {
						ZoneSizingInput( ZoneSizIndex ).HeatDesTempDiff = 0.0;
					}
					//  N5, \field Zone Cooling Design Supply Air Humidity Ratio
					//      \required-field
					//      \minimum 0.0
					//      \type real
					//      \units kg-H2O/kg-air
					if ( lNumericFieldBlanks( 5 ) ) {
						ZoneSizingInput( ZoneSizIndex ).CoolDesHumRat = 0.0;
					} else if ( rNumericArgs( 5 ) < 0.0 ) {
						ShowSevereError( cCurrentModuleObject + ": incorrect " + cNumericFieldNames( 5 ) + ": " + RoundSigDigits( rNumericArgs( 5 ), 2 ) );
						ShowContinueError( ".. value should not be negative. Occurs in Sizing Object=" + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					} else {
						ZoneSizingInput( ZoneSizIndex ).CoolDesHumRat = rNumericArgs( 5 );
					}
					//  N6, \field Zone Heating Design Supply Air Humidity Ratio
					//      \required-field
					//      \minimum 0.0
					//      \type real
					//      \units kg-H2O/kg-air
					if ( lNumericFieldBlanks( 6 ) ) {
						ZoneSizingInput( ZoneSizIndex ).HeatDesHumRat = 0.0;
					} else if ( rNumericArgs( 6 ) < 0.0 ) {
						ShowSevereError( cCurrentModuleObject + ": incorrect " + cNumericFieldNames( 6 ) + ": " + RoundSigDigits( rNumericArgs( 6 ), 2 ) );
						ShowContinueError( ".. value should not be negative. Occurs in Sizing Object=" + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					} else {
						ZoneSizingInput( ZoneSizIndex ).HeatDesHumRat = rNumericArgs( 6 );
					}
					//  A4, \field Design Specification Outdoor Air Object Name
					//      \type object-list
					//      \object-list DesignSpecificationOutdoorAirNames
					ZoneSizingInput( ZoneSizIndex ).DesignSpecOAObjName = cAlphaArgs( 4 );

					// Getting zone OA parameters from Design Specification object
					if ( ! lAlphaFieldBlanks( 4 ) ) {
						OAIndex = FindItemInList( ZoneSizingInput( ZoneSizIndex ).DesignSpecOAObjName, OARequirements );
						if ( OAIndex > 0 ) {
							ZoneSizingInput( ZoneSizIndex ).OADesMethod = OARequirements( OAIndex ).OAFlowMethod;
							ZoneSizingInput( ZoneSizIndex ).DesOAFlowPPer = OARequirements( OAIndex ).OAFlowPerPerson;
							ZoneSizingInput( ZoneSizIndex ).DesOAFlowPerArea = OARequirements( OAIndex ).OAFlowPerArea;
							ZoneSizingInput( ZoneSizIndex ).DesOAFlow = OARequirements( OAIndex ).OAFlowPerZone;
							ZoneSizingInput( ZoneSizIndex ).ZoneDesignSpecOAIndex = OAIndex;
						} else {
							ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid data." );
							ShowContinueError( "... incorrect " + cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\"." );
							ErrorsFound = true;
						}
					} else { // If no design spec object specified, i.e. no OA, then set OA method to None as default but flows to 0
						ZoneSizingInput( ZoneSizIndex ).OADesMethod = 0;
						ZoneSizingInput( ZoneSizIndex ).DesOAFlowPPer = 0.0;
						ZoneSizingInput( ZoneSizIndex ).DesOAFlowPerArea = 0.0;
						ZoneSizingInput( ZoneSizIndex ).DesOAFlow = 0.0;
					}

					//  N7, \field Zone Heating Sizing Factor
					//      \note if blank, global heating sizing factor from Sizing:Parameters is used.
					//      \minimum> 0
					if ( lNumericFieldBlanks( 7 ) || rNumericArgs( 7 ) == 0.0 ) {
						ZoneSizingInput( ZoneSizIndex ).HeatSizingFactor = GlobalHeatSizingFactor;
					} else if ( rNumericArgs( 7 ) < 0.0 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid data." );
						ShowContinueError( "... incorrect " + cNumericFieldNames( 7 ) + "=[" + RoundSigDigits( rNumericArgs( 7 ), 2 ) + "],  value should not be negative." );
						ErrorsFound = true;
					} else {
						ZoneSizingInput( ZoneSizIndex ).HeatSizingFactor = rNumericArgs( 7 );
					}
					//  N8, \field Zone Cooling Sizing Factor
					//      \note if blank, global cooling sizing factor from Sizing:Parameters is used.
					//      \minimum> 0
					if ( lNumericFieldBlanks( 8 ) || rNumericArgs( 8 ) == 0.0 ) {
						ZoneSizingInput( ZoneSizIndex ).CoolSizingFactor = GlobalCoolSizingFactor;
					} else if ( rNumericArgs( 8 ) < 0.0 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid data." );
						ShowContinueError( "... incorrect " + cNumericFieldNames( 8 ) + "=[" + RoundSigDigits( rNumericArgs( 8 ), 2 ) + "],  value should not be negative." );
						ErrorsFound = true;
					} else {
						ZoneSizingInput( ZoneSizIndex ).CoolSizingFactor = rNumericArgs( 8 );
					}
					//  N9, \field Cooling Design Air Flow Rate
					//      \type real
					//      \units m3/s
					//      \minimum 0
					//      \default 0
					//      \note This input is used if Cooling Design Air Flow Method is Flow/Zone
					//      \note This value will be multiplied by the global or zone sizing factor and
					//      \note by zone multipliers.
					if ( lNumericFieldBlanks( 9 ) ) {
						ZoneSizingInput( ZoneSizIndex ).DesCoolAirFlow = 0.0;
					} else if ( rNumericArgs( 9 ) < 0.0 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid data." );
						ShowContinueError( "... incorrect " + cNumericFieldNames( 9 ) + "=[" + RoundSigDigits( rNumericArgs( 9 ), 2 ) + "],  value should not be negative." );
						ErrorsFound = true;
					} else {
						ZoneSizingInput( ZoneSizIndex ).DesCoolAirFlow = rNumericArgs( 9 );
					}
					//  N10,\field Cooling Minimum Air Flow per Zone Floor Area
					//      \type real
					//      \units m3/s-m2
					//      \minimum 0
					//      \default .000762
					//      \note default is .15 cfm/ft2
					//      \note This input is used if Cooling Design Air Flow Method is design day with limit
					if ( lNumericFieldBlanks( 10 ) ) {
						if ( rNumericArgs( 10 ) <= 0.0 ) { // in case someone changes the default in the IDD
							ZoneSizingInput( ZoneSizIndex ).DesCoolMinAirFlowPerArea = 0.000762;
						} else {
							ZoneSizingInput( ZoneSizIndex ).DesCoolMinAirFlowPerArea = rNumericArgs( 10 );
						}
					} else if ( rNumericArgs( 10 ) < 0.0 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid data." );
						ShowContinueError( "... incorrect " + cNumericFieldNames( 108 ) + "=[" + RoundSigDigits( rNumericArgs( 10 ), 2 ) + "],  value should not be negative." );
						ErrorsFound = true;
					} else {
						ZoneSizingInput( ZoneSizIndex ).DesCoolMinAirFlowPerArea = rNumericArgs( 10 );
					}
					//  N11,\field Cooling Minimum Air Flow
					//      \type real
					//      \units m3/s
					//      \minimum 0
					//      \default 0
					//      \note This input is used if Cooling Design Air Flow Method is design day with limit
					if ( lNumericFieldBlanks( 11 ) ) {
						ZoneSizingInput( ZoneSizIndex ).DesCoolMinAirFlow = 0.0;
					} else if ( rNumericArgs( 11 ) < 0.0 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid data." );
						ShowContinueError( "... incorrect " + cNumericFieldNames( 11 ) + "=[" + RoundSigDigits( rNumericArgs( 11 ), 2 ) + "],  value should not be negative." );
						ErrorsFound = true;
					} else {
						ZoneSizingInput( ZoneSizIndex ).DesCoolMinAirFlow = rNumericArgs( 11 );
					}
					//  N12,\field Cooling Minimum Air Flow Fraction
					//      \note fraction of the Cooling design Air Flow Rate
					//      \type real
					//      \minimum 0
					//      \default 0
					//      \note This input is currently used in sizing the Fan minimum Flow Rate.
					//      \note It does not currently affect other component autosizing.
					if ( lNumericFieldBlanks( 12 ) ) {
						ZoneSizingInput( ZoneSizIndex ).DesCoolMinAirFlowFrac = 0.0;
					} else if ( rNumericArgs( 12 ) < 0.0 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid data." );
						ShowContinueError( "... incorrect " + cNumericFieldNames( 12 ) + "=[" + RoundSigDigits( rNumericArgs( 12 ), 2 ) + "],  value should not be negative." );
						ErrorsFound = true;
					} else {
						ZoneSizingInput( ZoneSizIndex ).DesCoolMinAirFlowFrac = rNumericArgs( 12 );
					}
					//  N13,\field Heating Design Air Flow Rate
					//      \type real
					//      \units m3/s
					//      \minimum 0
					//      \default 0
					//      \note This input is used if Heating Design Air Flow Method is Flow/Zone.
					//      \note This value will be multiplied by the global or zone sizing factor and
					//      \note by zone multipliers.
					if ( lNumericFieldBlanks( 13 ) ) {
						ZoneSizingInput( ZoneSizIndex ).DesHeatAirFlow = 0.0;
					} else if ( rNumericArgs( 13 ) < 0.0 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid data." );
						ShowContinueError( "... incorrect " + cNumericFieldNames( 13 ) + "=[" + RoundSigDigits( rNumericArgs( 13 ), 2 ) + "],  value should not be negative." );
						ErrorsFound = true;
					} else {
						ZoneSizingInput( ZoneSizIndex ).DesHeatAirFlow = rNumericArgs( 13 );
					}
					//  N14,\field Heating Maximum Air Flow per Zone Floor Area
					//      \type real
					//      \units m3/s-m2
					//      \minimum 0
					//      \default .002032
					//      \note default is .40 cfm/ft2
					//      \note This input is not currently used for autosizing any of the components.
					if ( lNumericFieldBlanks( 14 ) ) {
						if ( rNumericArgs( 14 ) <= 0.0 ) { // in case someone changes the default in the IDD
							ZoneSizingInput( ZoneSizIndex ).DesHeatMaxAirFlowPerArea = 0.002032;
						} else {
							ZoneSizingInput( ZoneSizIndex ).DesHeatMaxAirFlowPerArea = rNumericArgs( 14 );
						}
					} else if ( rNumericArgs( 14 ) < 0.0 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid data." );
						ShowContinueError( "... incorrect " + cNumericFieldNames( 14 ) + "=[" + RoundSigDigits( rNumericArgs( 14 ), 2 ) + "],  value should not be negative." );
						ErrorsFound = true;
					} else {
						ZoneSizingInput( ZoneSizIndex ).DesHeatMaxAirFlowPerArea = rNumericArgs( 14 );
					}
					//  N15,\field Heating Maximum Air Flow
					//      \type real
					//      \units m3/s
					//      \minimum 0
					//      \default .1415762
					//      \note default is 300 cfm
					//      \note This input is not currently used for autosizing any of the components.
					if ( lNumericFieldBlanks( 15 ) ) {
						if ( rNumericArgs( 15 ) <= 0.0 ) { // in case someone changes the default in the IDD
							ZoneSizingInput( ZoneSizIndex ).DesHeatMaxAirFlow = 0.1415762;
						} else {
							ZoneSizingInput( ZoneSizIndex ).DesHeatMaxAirFlow = rNumericArgs( 15 );
						}
					} else if ( rNumericArgs( 15 ) < 0.0 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid data." );
						ShowContinueError( "... incorrect " + cNumericFieldNames( 15 ) + "=[" + RoundSigDigits( rNumericArgs( 15 ), 2 ) + "],  value should not be negative." );
						ErrorsFound = true;
					} else {
						ZoneSizingInput( ZoneSizIndex ).DesHeatMaxAirFlow = rNumericArgs( 15 );
					}
					//  N16;\field Heating Maximum Air Flow Fraction
					//      \note fraction of the Heating Design Air Flow Rate
					//      \note This input is not currently used for autosizing any of the components.
					//      \type real
					//      \minimum 0
					//      \default 0.3
					if ( lNumericFieldBlanks( 16 ) ) {
						if ( rNumericArgs( 16 ) <= 0.0 ) { // in case someone changes the default in the IDD
							ZoneSizingInput( ZoneSizIndex ).DesHeatMaxAirFlowFrac = 0.3;
						} else {
							ZoneSizingInput( ZoneSizIndex ).DesHeatMaxAirFlowFrac = rNumericArgs( 16 );
						}
					} else if ( rNumericArgs( 16 ) < 0.0 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid data." );
						ShowContinueError( "... incorrect " + cNumericFieldNames( 16 ) + "=[" + RoundSigDigits( rNumericArgs( 16 ), 2 ) + "],  value should not be negative." );
						ErrorsFound = true;
					} else {
						ZoneSizingInput( ZoneSizIndex ).DesHeatMaxAirFlowFrac = rNumericArgs( 16 );
					}

					//  A7, \field Zone Air Distribution Object Name
					if ( ! lAlphaFieldBlanks( 7 ) ) {
						ZoneSizingInput( ZoneSizIndex ).ZoneAirDistEffObjName = cAlphaArgs( 7 );
						ObjIndex = FindItemInList( ZoneSizingInput( ZoneSizIndex ).ZoneAirDistEffObjName, ZoneAirDistribution );
						if ( ObjIndex > 0 ) {
							ZoneSizingInput( ZoneSizIndex ).ZoneADEffCooling = ZoneAirDistribution( ObjIndex ).ZoneADEffCooling;
							ZoneSizingInput( ZoneSizIndex ).ZoneADEffHeating = ZoneAirDistribution( ObjIndex ).ZoneADEffHeating;
							ZoneSizingInput( ZoneSizIndex ).ZoneSecondaryRecirculation = ZoneAirDistribution( ObjIndex ).ZoneSecondaryRecirculation;
							ZoneSizingInput( ZoneSizIndex ).ZoneAirDistributionIndex = ObjIndex;
						} else {
							// generate a warning message
							ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid data." );
							ShowContinueError( "... not found " + cAlphaFieldNames( 7 ) + "=\"" + cAlphaArgs( 7 ) + "\"." );
							ErrorsFound = true;
						}
					} else {
						// assume defaults
						ZoneSizingInput( ZoneSizIndex ).ZoneADEffCooling = 1.0;
						ZoneSizingInput( ZoneSizIndex ).ZoneADEffHeating = 1.0;
						ZoneSizingInput( ZoneSizIndex ).ZoneSecondaryRecirculation = 0.0;
					}

					{ auto const coolAirDesMethod( cAlphaArgs( 5 ) );
					if ( coolAirDesMethod == "DESIGNDAY" ) {
						ZoneSizingInput( ZoneSizIndex ).CoolAirDesMethod = FromDDCalc;
					} else if ( coolAirDesMethod == "FLOW/ZONE" ) {
						ZoneSizingInput( ZoneSizIndex ).CoolAirDesMethod = InpDesAirFlow;
					} else if ( coolAirDesMethod == "DESIGNDAYWITHLIMIT" ) {
						ZoneSizingInput( ZoneSizIndex ).CoolAirDesMethod = DesAirFlowWithLim;
					} else {
						ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid data." );
						ShowContinueError( "... incorrect " + cAlphaFieldNames( 5 ) + "=\"" + cAlphaArgs( 5 ) + "\"." );
						ShowContinueError( "... valid values are DesignDay, Flow/Zone or DesignDayWithLimit." );
						ErrorsFound = true;
					}}
					{ auto const heatAirDesMethod( cAlphaArgs( 6 ) );
					if ( heatAirDesMethod == "DESIGNDAY" ) {
						ZoneSizingInput( ZoneSizIndex ).HeatAirDesMethod = FromDDCalc;
					} else if ( heatAirDesMethod == "FLOW/ZONE" ) {
						ZoneSizingInput( ZoneSizIndex ).HeatAirDesMethod = InpDesAirFlow;
					} else if ( heatAirDesMethod == "DESIGNDAYWITHLIMIT" ) {
						ZoneSizingInput( ZoneSizIndex ).HeatAirDesMethod = DesAirFlowWithLim;
					} else {
						ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid data." );
						ShowContinueError( "... incorrect " + cAlphaFieldNames( 6 ) + "=\"" + cAlphaArgs( 6 ) + "\"." );
						ShowContinueError( "... valid values are DesignDay, Flow/Zone or DesignDayWithLimit." );
						ErrorsFound = true;
					}}
					if ( cAlphaArgs( 8 ) == "YES" ) {
						ZoneSizingInput( ZoneSizIndex ).AccountForDOAS = true;
					}
					else {
						ZoneSizingInput( ZoneSizIndex ).AccountForDOAS = false;
					}
					if ( ZoneSizingInput( ZoneSizIndex ).AccountForDOAS ) {
						{auto const DOASControlMethod( cAlphaArgs( 9 ) );
							if ( DOASControlMethod == "NEUTRALSUPPLYAIR" ) {
								ZoneSizingInput( ZoneSizIndex ).DOASControlStrategy = DOANeutralSup;
							}
							else if ( DOASControlMethod == "NEUTRALDEHUMIDIFIEDSUPPLYAIR" ) {
								ZoneSizingInput( ZoneSizIndex ).DOASControlStrategy = DOANeutralDehumSup;
							}
							else if ( DOASControlMethod == "COLDSUPPLYAIR" ) {
								ZoneSizingInput( ZoneSizIndex ).DOASControlStrategy = DOACoolSup;
							}
							else {
								ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid data." );
								ShowContinueError( "... incorrect " + cAlphaFieldNames( 9 ) + "=\"" + cAlphaArgs( 9 ) + "\"." );
								ShowContinueError( "... valid values are NeutralSupplyAir, NeutralDehumidifiedSupplyAir or ColdSupplyAir." );
								ErrorsFound = true;
							}
						}
						ZoneSizingInput( ZoneSizIndex ).DOASLowSetpoint = rNumericArgs( 17 );
						ZoneSizingInput( ZoneSizIndex ).DOASHighSetpoint = rNumericArgs( 18 );
						if ( rNumericArgs( 17 ) > 0.0 && rNumericArgs( 18 ) > 0.0 && rNumericArgs( 17 ) >= rNumericArgs( 18 ) ) {
							ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid data." );
							ShowContinueError( "... Dedicated Outside Air Low Setpoint for Design must be less than the High Setpoint" );
							ErrorsFound = true;
						}
					}
				}
			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( cCurrentModuleObject + ": Errors found in getting input. Program terminates." );
		}

	}

	void
	GetZoneAndZoneListNames(
		bool & ErrorsFound,
		int & NumZones,
		Array1D_string & ZoneNames,
		int & NumZoneLists,
		Array1D< ZoneListData > & ZoneListNames
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   October 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Get Zone and ZoneList Names so Sizing:Zone can use global ZoneList.
		// This is not a full validation of these objects -- only enough to fill
		// structures for the Sizing:Zone object.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::FindItemInList;
		using namespace DataIPShortCuts;

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Item;
		int Found;
		int Item1;
		int NumAlphas;
		int NumNumbers;
		int IOStatus;
		bool InErrFlag; // Preserve (no current use) the input status of ErrorsFound

		InErrFlag = ErrorsFound;
		cCurrentModuleObject = "Zone";
		NumZones = GetNumObjectsFound( cCurrentModuleObject );
		ZoneNames.allocate( NumZones );

		for ( Item = 1; Item <= NumZones; ++Item ) {
			GetObjectItem( cCurrentModuleObject, Item, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			// validation, but no error
			Found = FindItemInList( cAlphaArgs( 1 ), ZoneNames, Item - 1 );
			if ( Found == 0 ) {
				ZoneNames( Item ) = cAlphaArgs( 1 );
			} else {
				ZoneNames( Item ) = "xxxxx";
			}
		}

		cCurrentModuleObject = "ZoneList";
		NumZoneLists = GetNumObjectsFound( cCurrentModuleObject );
		ZoneListNames.allocate( NumZoneLists );

		for ( Item = 1; Item <= NumZoneLists; ++Item ) {
			GetObjectItem( cCurrentModuleObject, Item, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			// validation, but no error
			Found = FindItemInList( cAlphaArgs( 1 ), ZoneListNames, Item - 1 );
			if ( Found == 0 ) {
				ZoneListNames( Item ).Name = cAlphaArgs( 1 );
			} else {
				ZoneListNames( Item ).Name = "xxxxx";
			}
			ZoneListNames( Item ).Zones.allocate( NumAlphas - 1 );
			ZoneListNames( Item ).NumOfZones = NumAlphas - 1;
			for ( Item1 = 2; Item1 <= NumAlphas; ++Item1 ) {
				Found = FindItemInList( cAlphaArgs( Item1 ), ZoneNames, NumZones );
				ZoneListNames( Item ).Zones( Item1 - 1 ) = Found;
			}
		}

	}

	void
	GetSystemSizingInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   January 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains input data for System Sizing objects and stores it in
		// appropriate data structures.

		// METHODOLOGY EMPLOYED:
		// Uses InputProcessor "Get" routines to obtain data.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using General::RoundSigDigits;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:


		//Sizing:System;
		int const iNameAlphaNum = 1; // A1, \field AirLoop Name
		int const iLoadTypeSizeAlphaNum = 2; // A2, \field Type of Load to Size On
		int const iCoolCapControlAlphaNum = 11; // A11 \field Central Cooling Capacity Control Method
		int const iDesignOAVolFlowNumericNum = 1; // N1, \field Design Outdoor Air Flow Rate
		int const iMinSysAirFlowRatioNumericNum = 2; // N2, \field Minimum System Air Flow Ratio
		int const iPreheatDesignTempNumericNum = 3; // N3, \field Preheat Design Temperature
		int const iPreheatDesignHumRatNumericNum = 4; // N4, \field Preheat Design Humidity Ratio
		int const iPrecoolDesignTempNumericNum = 5; // N5, \field Precool Design Temperature
		int const iPrecoolDesignHumRatNumericNum = 6; // N6, \field Precool Design Humidity Ratio
		int const iCentralCoolDesignSATempNumericNum = 7; // N7, \field Central Cooling Design Supply Air Temperature
		int const iCentralHeatDesignSATempNumericNum = 8; // N8, \field Central Heating Design Supply Air Temperature
		int const iSizingOptionAlphaNum = 3; //  A3, \field Sizing Option
		int const i100PercentOACoolingAlphaNum = 4; //  A4, \field 100% Outdoor Air in Cooling
		int const i100PercentOAHeatingAlphaNum = 5; //  A5, \field 100% Outdoor Air in Heating
		int const iCentralCoolDesignSAHumRatNumericNum = 9; // N9, \field Central Cooling Design Supply Air Humidity Ratio
		int const iCentralHeatDesignSAHumRatNumericNum = 10; // N10, \field Central Heating Design Supply Air Humidity Ratio
		int const iCoolSAFMAlphaNum = 6; // A6, \field Cooling Design Air Flow Method
		int const iMaxCoolAirVolFlowNumericNum = 11; // N11, \field Cooling Design Air Flow Rate {m3/s}
		int const iCoolFlowPerFloorAreaNumericNum = 12; // N12, \field Supply Air Flow Rate Per Floor Area During Cooling Operation {m3/s-m2}
		int const iCoolFlowPerFracCoolNumericNum = 13; // N13, \field Fraction of Autosized Design Cooling Supply Air Flow Rate {-}
		int const iCoolFlowPerCoolCapNumericNum = 14; // N14, \field Design Supply Air Flow Rate Per Unit Cooling Capacity {m3/s-W}
		int const iHeatSAFMAlphaNum = 7; // A7, \field Heating Design Air Flow Method
		int const iMaxHeatAirVolFlowNumericNum = 15; // N15, \field Heating Design Air Flow Rate {m3/s}
		int const iHeatFlowPerFloorAreaNumericNum = 16; // N16, \field Supply Air Flow Rate Per Floor Area During Heating Operation {m3/s-m2}
		int const iHeatFlowPerFracHeatNumericNum = 17; // N17, \field Fraction of Autosized Design Heating Supply Air Flow Rate {-}
		int const iHeatFlowPerFracCoolNumericNum = 18; // N18, \field Fraction of Autosized Design Cooling Supply Air Flow Rate {-}
		int const iHeatFlowPerHeatCapNumericNum = 19; // N19, \field Design Supply Air Flow Rate Per Unit Heating Capacity {m3/s-W}
		int const iSystemOASMethodAlphaNum = 8; // A8,  \field System Outdoor Air Method
		int const iZoneMaxOAFractionNumericNum = 20; // N20, \field Zone Maximum Outdoor Air Fraction
		int const iCoolCAPMAlphaNum( 9 ); // A9, \field Cooling Design Capacity Method
		int const iCoolDesignCapacityNumericNum( 21 ); // N21, \field Cooling Design Capacity {W}
		int const iCoolCapacityPerFloorAreaNumericNum( 22 ); // N22, \field Cooling Design Capacity Per Floor Area {W/m2}
		int const iCoolFracOfAutosizedCapacityNumericNum( 23 ); // N23, \field Fraction of Autosized Cooling Design Capacity {-}
		int const iHeatCAPMAlphaNum( 10 ); // A10, \field Heating Design Capacity Method
		int const iHeatDesignCapacityNumericNum( 24 ); // N24, \field Heating Design Capacity {W}
		int const iHeatCapacityPerFloorAreaNumericNum( 25 ); // N25, \field Heating Design Capacity Per Floor Area {W/m2}
		int const iHeatFracOfAutosizedCapacityNumericNum( 26 ); // N26, \field Fraction of Autosized Cooling Design Capacity {-}



		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SysSizIndex; // loop index
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int IOStatus; // Used in GetObjectItem
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		int NumDesDays; // Number of design days in input

		NumAirLoops = GetNumObjectsFound( "AirLoopHVAC" );
		cCurrentModuleObject = "Sizing:System";
		NumSysSizInput = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumSysSizInput > 0 ) {
			NumDesDays = GetNumObjectsFound( "SizingPeriod:DesignDay" ) + GetNumObjectsFound( "SizingPeriod:WeatherFileDays" ) + GetNumObjectsFound( "SizingPeriod:WeatherFileConditionType" );
			if ( NumDesDays == 0 && ( DoSystemSizing || DoPlantSizing ) ) {
				ShowSevereError( "System Sizing calculations need SizingPeriod:* input. None found." );
				ErrorsFound = true;
			}
			SysSizInput.allocate( NumSysSizInput );
		}

		for ( SysSizIndex = 1; SysSizIndex <= NumSysSizInput; ++SysSizIndex ) {
			GetObjectItem( cCurrentModuleObject, SysSizIndex, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( iNameAlphaNum ), SysSizInput, &SystemSizingInputData::AirPriLoopName, SysSizIndex - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if (IsBlank) cAlphaArgs( iNameAlphaNum ) = "xxxxx";
			}

			SysSizInput( SysSizIndex ).AirPriLoopName = cAlphaArgs( iNameAlphaNum );
			{ auto const loadSizeType( cAlphaArgs( iLoadTypeSizeAlphaNum ) );
			if ( loadSizeType == "SENSIBLE" ) {
				SysSizInput( SysSizIndex ).LoadSizeType = Sensible;
			// } else if ( loadSizeType == "LATENT" ) {
				// SysSizInput( SysSizIndex ).LoadSizeType = Latent;
			} else if ( loadSizeType == "TOTAL" ) {
				SysSizInput( SysSizIndex ).LoadSizeType = Total;
			} else if ( loadSizeType == "VENTILATIONREQUIREMENT" ) {
				SysSizInput( SysSizIndex ).LoadSizeType = Ventilation;
			} else {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( iNameAlphaNum ) + "\", invalid data." );
				ShowContinueError( "... incorrect " + cAlphaFieldNames( iLoadTypeSizeAlphaNum ) + "=\"" + cAlphaArgs( iLoadTypeSizeAlphaNum ) + "\".");
				ShowContinueError( "... valid values are Sensible, Total, or VentilationRequirement." );
				ErrorsFound = true;
			}}
			// assign CoolingPeakLoadType based on LoadSizeType for now
			if ( SysSizInput( SysSizIndex ).LoadSizeType == Sensible ) {
				SysSizInput( SysSizIndex ).CoolingPeakLoadType = SensibleCoolingLoad;
			} else if ( SysSizInput( SysSizIndex ).LoadSizeType == Total ) {
				SysSizInput( SysSizIndex ).CoolingPeakLoadType = TotalCoolingLoad;
			} else {
				SysSizInput( SysSizIndex ).CoolingPeakLoadType = SensibleCoolingLoad;
			}
			// set the CoolCapControl input
			SysSizInput( SysSizIndex ).CoolCapControl = VAV;
			{ auto const CoolCapCtrl( cAlphaArgs( iCoolCapControlAlphaNum ) );
			if ( CoolCapCtrl == "VAV" ) {
				SysSizInput( SysSizIndex ).CoolCapControl = VAV;
			} else if ( CoolCapCtrl == "BYPASS" ) {
				SysSizInput( SysSizIndex ).CoolCapControl = Bypass;
			} else if ( CoolCapCtrl == "VT" ) {
				SysSizInput( SysSizIndex ).CoolCapControl = VT;
			} else if ( CoolCapCtrl == "ONOFF" ) {
				SysSizInput( SysSizIndex ).CoolCapControl = OnOff;
			} else {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( iNameAlphaNum ) + "\", invalid data." );
				ShowContinueError( "... incorrect " + cAlphaFieldNames( iCoolCapControlAlphaNum ) + "=\"" + cAlphaArgs( iCoolCapControlAlphaNum ) + "\"." );
				ShowContinueError( "... valid values are VAV, Bypass, VT, or OnOff." );
				ErrorsFound = true;
			}}
 			{ auto const sizingOption( cAlphaArgs( iSizingOptionAlphaNum ) );
			if ( sizingOption == "COINCIDENT" ) {
				SysSizInput( SysSizIndex ).SizingOption = Coincident;
			} else if ( sizingOption == "NONCOINCIDENT" ) {
				SysSizInput( SysSizIndex ).SizingOption = NonCoincident;
			} else {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( iNameAlphaNum ) + "\", invalid data." );
				ShowContinueError( "... incorrect " + cAlphaFieldNames( iSizingOptionAlphaNum ) + "=\"" + cAlphaArgs( iSizingOptionAlphaNum ) + "\".");
				ShowContinueError( "... valid values are Coincident or NonCoincident." );
				ErrorsFound = true;
			}}
			{ auto const coolOAOption( cAlphaArgs( i100PercentOACoolingAlphaNum ) );
			if ( coolOAOption == "YES" ) {
				SysSizInput( SysSizIndex ).CoolOAOption = 1;
			} else if ( coolOAOption == "NO" ) {
				SysSizInput( SysSizIndex ).CoolOAOption = 2;
			} else {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( iNameAlphaNum ) + "\", invalid data." );
				ShowContinueError("... incorrect " + cAlphaFieldNames( i100PercentOACoolingAlphaNum ) + "=\"" + cAlphaArgs( i100PercentOACoolingAlphaNum ) + "\".");
				ShowContinueError( "... valid values are Yes or No." );
				ErrorsFound = true;
			}}
			{ auto const heatOAOption( cAlphaArgs( i100PercentOAHeatingAlphaNum ) );
			if ( heatOAOption == "YES" ) {
				SysSizInput( SysSizIndex ).HeatOAOption = 1;
			} else if ( heatOAOption == "NO" ) {
				SysSizInput( SysSizIndex ).HeatOAOption = 2;
			} else {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( iNameAlphaNum ) + "\", invalid data." );
				ShowContinueError( "... incorrect " + cAlphaFieldNames( i100PercentOAHeatingAlphaNum ) + "=\"" + cAlphaArgs( i100PercentOAHeatingAlphaNum ) + "\".");
				ShowContinueError( "... valid values are Yes or No." );
				ErrorsFound = true;
			}}

			//  N1, \field Design Outdoor Air Flow Rate
			//      \type real
			//      \default autosize
			//      \minimum 0.0
			//int const  iDesignOAVolFlowNumericNum = 1;     // N1, \field Design Outdoor Air Flow Rate
			if ( lNumericFieldBlanks( iDesignOAVolFlowNumericNum ) ) {
				SysSizInput( SysSizIndex ).DesOutAirVolFlow = AutoSize;
			} else if ( rNumericArgs( iDesignOAVolFlowNumericNum ) < 0.0 && rNumericArgs( iDesignOAVolFlowNumericNum ) != AutoSize) {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( iNameAlphaNum ) + "\", invalid data." );
				ShowContinueError("... incorrect " + cNumericFieldNames( iDesignOAVolFlowNumericNum ) + "=[" + RoundSigDigits( rNumericArgs( iDesignOAVolFlowNumericNum ), 2) + "],  value should not be negative.");
				ErrorsFound = true;
			} else {
				SysSizInput(SysSizIndex).DesOutAirVolFlow = rNumericArgs( iDesignOAVolFlowNumericNum );
			}
			if ( SysSizInput( SysSizIndex ).DesOutAirVolFlow == AutoSize ) {
				SysSizInput( SysSizIndex ).OAAutoSized = true;
			}

			//  N2, \field Minimum System Air Flow Ratio
			//      \required-field
			//      \type real
			//      \minimum 0.0
			//      \maximum 1.0
			//int const iMinSysAirFlowRatioNumericNum = 2;  // N2, \field Minimum System Air Flow Ratio
			if ( lNumericFieldBlanks( iMinSysAirFlowRatioNumericNum ) ) {
				SysSizInput( SysSizIndex ).SysAirMinFlowRat = 0.0;
			} else if (rNumericArgs( iMinSysAirFlowRatioNumericNum ) < 0.0) {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( iMinSysAirFlowRatioNumericNum ) + "\", invalid data.");
				ShowContinueError( "... incorrect " + cNumericFieldNames( iMinSysAirFlowRatioNumericNum ) + "=[" + RoundSigDigits( rNumericArgs( iMinSysAirFlowRatioNumericNum ), 2) + "],  value should not be negative.");
				ErrorsFound = true;
			} else {
				SysSizInput( SysSizIndex ).SysAirMinFlowRat = rNumericArgs( iMinSysAirFlowRatioNumericNum );
			}
			//int const iPreheatDesignTempNumericNum = 3; // N3, \field Preheat Design Temperature
			//int const iPreheatDesignHumRatNumericNum = 4; // N4, \field Preheat Design Humidity Ratio
			//int const iPrecoolDesignTempNumericNum = 5; // N5, \field Precool Design Temperature
			//int const iPrecoolDesignHumRatNumericNum = 6; // N6, \field Precool Design Humidity Ratio
			//int const iCentralCoolDesignSATempNumericNum = 7; // N7, \field Central Cooling Design Supply Air Temperature
			//int const iCentralHeatDesignSATempNumericNum = 8; // N8, \field Central Heating Design Supply Air Temperature
			//int const iCentralCoolDesignSAHumRatNumericNum = 9; // N9, \field Central Cooling Design Supply Air Humidity Ratio
			//int const iCentralHeatDesignSAHumRatNumericNum = 10; // N10, \field Central Heating Design Supply Air Humidity Ratio
			SysSizInput( SysSizIndex ).PreheatTemp = rNumericArgs( iPreheatDesignTempNumericNum );
			SysSizInput( SysSizIndex ).PreheatHumRat = rNumericArgs( iPreheatDesignHumRatNumericNum );
			SysSizInput( SysSizIndex ).PrecoolTemp = rNumericArgs( iPrecoolDesignTempNumericNum );
			SysSizInput( SysSizIndex ).PrecoolHumRat = rNumericArgs( iPrecoolDesignHumRatNumericNum );
			SysSizInput( SysSizIndex ).CoolSupTemp = rNumericArgs( iCentralCoolDesignSATempNumericNum );
			SysSizInput( SysSizIndex ).HeatSupTemp = rNumericArgs( iCentralHeatDesignSATempNumericNum );
			SysSizInput( SysSizIndex ).CoolSupHumRat = rNumericArgs( iCentralCoolDesignSAHumRatNumericNum );
			SysSizInput( SysSizIndex ).HeatSupHumRat = rNumericArgs( iCentralHeatDesignSAHumRatNumericNum );
			//  N11, \field Cooling Design Air Flow Rate
			//      \note This input is used if Cooling Design Air Flow Method is Flow/System
			//      \note This value will *not* be multiplied by any sizing factor or by zone multipliers.
			//      \note If using zone multipliers, this value must be large enough to serve the multiplied zones.
			//      \type real
			//      \units m3/s
			//      \minimum 0
			//      \default 0
			//int const iCoolSAFMAlphaNum = 6; // A6, \field Cooling Design Air Flow Method
			//int const iMaxCoolAirVolFlowNumericNum = 11; // N11, \field Cooling Design Air Flow Rate {m3/s}
			//int const iCoolFlowPerFloorAreaNumericNum = 12; // N12, \field Supply Air Flow Rate Per Floor Area During Cooling Operation {m3/s-m2}
			//int const iCoolFlowPerFracCoolNumericNum = 13; // N13, \field Fraction of Autosized Design Cooling Supply Air Flow Rate {-}
			//int const iCoolFlowPerCoolCapNumericNum = 14; // N14, \field Design Supply Air Flow Rate Per Unit Cooling Capacity {m3/s-W}


			if ( lNumericFieldBlanks( iMaxCoolAirVolFlowNumericNum ) ) {
				SysSizInput( SysSizIndex ).DesCoolAirFlow = 0.0;
			} else if ( rNumericArgs( iMaxCoolAirVolFlowNumericNum ) < 0.0 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( iNameAlphaNum ) + "\", invalid data." );
				ShowContinueError( "... incorrect " + cNumericFieldNames( iMaxCoolAirVolFlowNumericNum ) + "=[" + RoundSigDigits( rNumericArgs( iMaxCoolAirVolFlowNumericNum ), 2 ) + "],  value should not be negative." );
				ErrorsFound = true;
			} else {
				SysSizInput( SysSizIndex ).DesCoolAirFlow = rNumericArgs( iMaxCoolAirVolFlowNumericNum );
			}
			//  N12;\field Heating Design Air Flow Rate
			//      \note This input is used if Heating Design Air Flow Method is Flow/System
			//      \note This value will *not* be multiplied by any sizing factor or by zone multipliers.
			//      \note If using zone multipliers, this value must be large enough to serve the multiplied zones.
			//      \type real
			//      \units m3/s
			//      \minimum 0
			//      \default 0
			//int const iHeatSAFMAlphaNum = 7; // A7, \field Heating Design Air Flow Method
			//int const iMaxHeatAirVolFlowNumericNum = 12; // N15, \field Heating Design Air Flow Rate {m3/s}
			//int const iHeatFlowPerFloorAreaNumericNum = 16; // N16, \field Supply Air Flow Rate Per Floor Area During Heating Operation {m3/s-m2}
			//int const iHeatFlowPerFracHeatNumericNum = 17; // N17, \field Fraction of Autosized Design Heating Supply Air Flow Rate {-}
			//int const iHeatFlowPerFracCoolNumericNum = 18; // N18, \field Fraction of Autosized Design Cooling Supply Air Flow Rate {-}
			//int const iHeatFlowPerHeatCapNumericNum = 19; // N19, \field Design Supply Air Flow Rate Per Unit Heating Capacity {m3/s-W}
			// add input fields for other cooling sizing methods
			if ( lNumericFieldBlanks( iMaxHeatAirVolFlowNumericNum ) ) {
				SysSizInput( SysSizIndex ).DesHeatAirFlow = 0.0;
			} else if ( rNumericArgs( iMaxHeatAirVolFlowNumericNum ) < 0.0 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( iNameAlphaNum ) + "\", invalid data." );
				ShowContinueError( "... incorrect " + cNumericFieldNames( iMaxHeatAirVolFlowNumericNum ) + "=[" + RoundSigDigits( rNumericArgs( iMaxHeatAirVolFlowNumericNum ), 2 ) + "],  value should not be negative." );
				ErrorsFound = true;
			} else {
				SysSizInput( SysSizIndex ).DesHeatAirFlow = rNumericArgs( iMaxHeatAirVolFlowNumericNum );
			}
			//  N13;\field Maximum Zone Outdoor Air Fraction
			//      \type real
			//      \default 1.0
			//      \minimum> 0.0
			//      \units dimensionless
			//int const iSystemOASMethodAlphaNum = 8; // A8,  \field System Outdoor Air Method
			//int const iZoneMaxOAFractionNumericNum = 13; // N20, \field Zone Maximum Outdoor Air Fraction

			// add input fields for other heating sizing methods
			if ( lNumericFieldBlanks ( iZoneMaxOAFractionNumericNum ) ) {
				SysSizInput( SysSizIndex ).MaxZoneOAFraction = 0.0;
			} else if ( rNumericArgs( iZoneMaxOAFractionNumericNum ) < 0.0 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( iNameAlphaNum ) + "\", invalid data." );
				ShowContinueError( "... incorrect " + cNumericFieldNames( iZoneMaxOAFractionNumericNum ) + "=[" + RoundSigDigits( rNumericArgs( iZoneMaxOAFractionNumericNum ), 2 ) + "],  value should not be negative.");
				ErrorsFound = true;
			} else {
				SysSizInput(SysSizIndex).MaxZoneOAFraction = rNumericArgs( iZoneMaxOAFractionNumericNum );
			}
			{ auto const coolAirDesMethod( cAlphaArgs( iCoolSAFMAlphaNum ) );
			if ( coolAirDesMethod == "DESIGNDAY" ) {
				SysSizInput( SysSizIndex ).CoolAirDesMethod = FromDDCalc;
			} else if ( coolAirDesMethod == "FLOW/SYSTEM" ) {
				SysSizInput( SysSizIndex ).CoolAirDesMethod = InpDesAirFlow;
			} else if ( coolAirDesMethod == "FLOWPERFLOORAREA" ) {
				SysSizInput(SysSizIndex).CoolAirDesMethod = InpDesAirFlow;
				SysSizInput(SysSizIndex).ScaleCoolSAFMethod = FlowPerFloorArea;
				SysSizInput(SysSizIndex).FlowPerFloorAreaCooled = rNumericArgs( iCoolFlowPerFloorAreaNumericNum );
			} else if ( coolAirDesMethod == "FRACTIONOFAUTOSIZEDCOOLINGAIRFLOW" ) {
				SysSizInput(SysSizIndex).CoolAirDesMethod = FromDDCalc;
				SysSizInput(SysSizIndex).ScaleCoolSAFMethod = FractionOfAutosizedCoolingAirflow;
				SysSizInput(SysSizIndex).FractionOfAutosizedCoolingAirflow = rNumericArgs( iCoolFlowPerFracCoolNumericNum );
			} else if ( coolAirDesMethod == "FLOWPERCOOLINGCAPACITY" ) {
				SysSizInput(SysSizIndex).CoolAirDesMethod = FromDDCalc;
				SysSizInput(SysSizIndex).ScaleCoolSAFMethod = FlowPerCoolingCapacity;
				SysSizInput(SysSizIndex).FlowPerCoolingCapacity = rNumericArgs( iCoolFlowPerCoolCapNumericNum );
			} else {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( iNameAlphaNum ) + "\", invalid data." );
				ShowContinueError( "... incorrect " + cAlphaFieldNames( iCoolSAFMAlphaNum ) + "=\"" + cAlphaArgs( iCoolSAFMAlphaNum ) + "\"." );
				ShowContinueError( "... valid values are DesignDay, Flow/System, FlowPerFloorArea, FractionOfAutosizedCoolingAirflow, or FlowPerCoolingCapacity." );
				ErrorsFound = true;
			}}
			{ auto const heatAirDesMethod( cAlphaArgs( iHeatSAFMAlphaNum ) );
			if ( heatAirDesMethod == "DESIGNDAY" ) {
				SysSizInput( SysSizIndex ).HeatAirDesMethod = FromDDCalc;
			} else if ( heatAirDesMethod == "FLOW/SYSTEM" ) {
				SysSizInput( SysSizIndex ).HeatAirDesMethod = InpDesAirFlow;
			} else if ( heatAirDesMethod == "FLOWPERFLOORAREA" ) {
				SysSizInput(SysSizIndex).HeatAirDesMethod = InpDesAirFlow;
				SysSizInput(SysSizIndex).ScaleHeatSAFMethod = FlowPerFloorArea;
				SysSizInput(SysSizIndex).FlowPerFloorAreaHeated = rNumericArgs( iHeatFlowPerFloorAreaNumericNum );
			} else if ( heatAirDesMethod == "FRACTIONOFAUTOSIZEDHEATINGAIRFLOW" ) {
				SysSizInput(SysSizIndex).HeatAirDesMethod = FromDDCalc;
				SysSizInput(SysSizIndex).ScaleHeatSAFMethod = FractionOfAutosizedHeatingAirflow;
				SysSizInput(SysSizIndex).FractionOfAutosizedHeatingAirflow = rNumericArgs( iHeatFlowPerFracHeatNumericNum );
			} else if ( heatAirDesMethod == "FRACTIONOFAUTOSIZEDCOOLINGAIRFLOW" ) {
				SysSizInput(SysSizIndex).HeatAirDesMethod = FromDDCalc;
				SysSizInput(SysSizIndex).ScaleHeatSAFMethod = FractionOfAutosizedCoolingAirflow;
				SysSizInput(SysSizIndex).FractionOfAutosizedCoolingAirflow = rNumericArgs( iHeatFlowPerFracCoolNumericNum );
			} else if ( heatAirDesMethod == "FLOWPERHEATINGCAPACITY" ) {
				SysSizInput(SysSizIndex).HeatAirDesMethod = FromDDCalc;
				SysSizInput(SysSizIndex).ScaleHeatSAFMethod = FlowPerHeatingCapacity;
				SysSizInput(SysSizIndex).FlowPerHeatingCapacity = rNumericArgs( iHeatFlowPerHeatCapNumericNum );
			} else {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( iNameAlphaNum ) + "\", invalid data." );
				ShowContinueError("... incorrect " + cAlphaFieldNames( iHeatSAFMAlphaNum ) + "=\"" + cAlphaArgs( iHeatSAFMAlphaNum ) + "\"." );
				ShowContinueError( "... valid values are DesignDay, Flow/System, FlowPerFloorArea, FractionOfAutosizedHeatingAirflow, or FlowPerHeatingCapacity." );
				ErrorsFound = true;
			}}
			{ auto const systemOAMethod(cAlphaArgs( iSystemOASMethodAlphaNum ));
			if ( systemOAMethod == "ZONESUM" ) {
				SysSizInput( SysSizIndex ).SystemOAMethod = SOAM_ZoneSum;
			} else if ( systemOAMethod == "VENTILATIONRATEPROCEDURE" ) {
				SysSizInput( SysSizIndex ).SystemOAMethod = SOAM_VRP;
				if ( SysSizInput( SysSizIndex ).LoadSizeType == Ventilation ) {
					ShowWarningError( cCurrentModuleObject + "=\"" + cAlphaArgs( iNameAlphaNum ) + "\", invalid combination of inputs." );
					ShowContinueError( cAlphaFieldNames( iLoadTypeSizeAlphaNum ) + " = " + cAlphaArgs( iLoadTypeSizeAlphaNum ) + " and " + cAlphaFieldNames( iSystemOASMethodAlphaNum ) + " = " + cAlphaArgs( iSystemOASMethodAlphaNum ) + "." );
					ShowContinueError( "Resetting System Outdoor Air Method to ZoneSum." );
					SysSizInput( SysSizIndex ).SystemOAMethod = SOAM_ZoneSum;
				} else {
					if ( SysSizInput( SysSizIndex ).DesOutAirVolFlow > 0 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( iNameAlphaNum ) + "\", invalid data." );
						ShowContinueError( "SystemOAMethod is set to VRP and " + cNumericFieldNames( iDesignOAVolFlowNumericNum ) + " > 0, user entry will be ignored." );
					}
				}
			} else {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( iNameAlphaNum ) + "\", invalid data." );
				ShowContinueError( "... incorrect " + cAlphaFieldNames( iSystemOASMethodAlphaNum ) + "=\"" + cAlphaArgs( iSystemOASMethodAlphaNum ) + "\".");
				ShowContinueError( "... valid values are ZoneSum or VentilationRateProcedure." );
				ErrorsFound = true;
			}}

			// Determine SysSizInput electric Cooling design capacity sizing method
			if ( SameString( cAlphaArgs( iCoolCAPMAlphaNum ), "COOLINGDESIGNCAPACITY" ) ) {
				SysSizInput( SysSizIndex ).CoolingCapMethod = CoolingDesignCapacity;
				SysSizInput( SysSizIndex ).ScaledCoolingCapacity = rNumericArgs( iCoolDesignCapacityNumericNum );
				if ( SysSizInput( SysSizIndex ).ScaledCoolingCapacity < 0.0 && SysSizInput( SysSizIndex ).ScaledCoolingCapacity != AutoSize ) {
					ShowSevereError( cCurrentModuleObject + " = " + SysSizInput( SysSizIndex ).AirPriLoopName );
					ShowContinueError( "Illegal " + cNumericFieldNames( iCoolDesignCapacityNumericNum ) + " = " + TrimSigDigits( rNumericArgs( iCoolDesignCapacityNumericNum ), 7 ) );
					ErrorsFound = true;
				}
			} else if ( SameString( cAlphaArgs( iCoolCAPMAlphaNum ), "CAPACITYPERFLOORAREA" ) ) {
				SysSizInput( SysSizIndex ).CoolingCapMethod = CapacityPerFloorArea;
				if ( !lNumericFieldBlanks( iCoolCapacityPerFloorAreaNumericNum ) ) {
					SysSizInput( SysSizIndex ).ScaledCoolingCapacity = rNumericArgs( iCoolCapacityPerFloorAreaNumericNum );
					if ( SysSizInput( SysSizIndex ).ScaledCoolingCapacity <= 0.0 ) {
						ShowSevereError( cCurrentModuleObject + " = " + SysSizInput( SysSizIndex ).AirPriLoopName );
						ShowContinueError( "Input for " + cAlphaFieldNames( iCoolCAPMAlphaNum ) + " = " + cAlphaArgs( iCoolCAPMAlphaNum ) );
						ShowContinueError( "Illegal " + cNumericFieldNames( iCoolCapacityPerFloorAreaNumericNum ) + " = " + TrimSigDigits( rNumericArgs( iCoolCapacityPerFloorAreaNumericNum ), 7 ) );
						ErrorsFound = true;
					} else if ( SysSizInput( SysSizIndex ).ScaledCoolingCapacity == AutoSize ) {
						ShowSevereError( cCurrentModuleObject + " = " + SysSizInput( SysSizIndex ).AirPriLoopName );
						ShowContinueError( "Input for " + cAlphaFieldNames( iCoolCAPMAlphaNum ) + " = " + cAlphaArgs( iCoolCAPMAlphaNum ) );
						ShowContinueError( "Illegal " + cNumericFieldNames( iCoolCapacityPerFloorAreaNumericNum ) + " = Autosize" );
						ErrorsFound = true;
					}
				} else {
					ShowSevereError( cCurrentModuleObject + " = " + SysSizInput( SysSizIndex ).AirPriLoopName );
					ShowContinueError( "Input for " + cAlphaFieldNames( iCoolCAPMAlphaNum ) + " = " + cAlphaArgs( iCoolCAPMAlphaNum ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFieldNames( iCoolCapacityPerFloorAreaNumericNum ) );
					ErrorsFound = true;
				}
			} else if ( SameString( cAlphaArgs( iCoolCAPMAlphaNum ), "FRACTIONOFAUTOSIZEDCOOLINGCAPACITY" ) ) {
				SysSizInput( SysSizIndex ).CoolingCapMethod = FractionOfAutosizedCoolingCapacity;
				if ( !lNumericFieldBlanks( iCoolFracOfAutosizedCapacityNumericNum ) ) {
					SysSizInput( SysSizIndex ).ScaledCoolingCapacity = rNumericArgs( iCoolFracOfAutosizedCapacityNumericNum );
					if ( SysSizInput( SysSizIndex ).ScaledCoolingCapacity < 0.0 ) {
						ShowSevereError( cCurrentModuleObject + " = " + SysSizInput( SysSizIndex ).AirPriLoopName );
						ShowContinueError( "Illegal " + cNumericFieldNames( iCoolFracOfAutosizedCapacityNumericNum ) + " = " + TrimSigDigits( rNumericArgs( iCoolFracOfAutosizedCapacityNumericNum ), 7 ) );
						ErrorsFound = true;
					}
				} else {
					ShowSevereError( cCurrentModuleObject + " = " + SysSizInput( SysSizIndex ).AirPriLoopName );
					ShowContinueError( "Input for " + cAlphaFieldNames( iCoolCAPMAlphaNum ) + " = " + cAlphaArgs( iCoolCAPMAlphaNum ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFieldNames( iCoolFracOfAutosizedCapacityNumericNum ) );
					ErrorsFound = true;
				}
			} else if ( SameString( cAlphaArgs( iCoolCAPMAlphaNum ), "NONE" ) ) {
				SysSizInput( SysSizIndex ).CoolingCapMethod = None;
				SysSizInput( SysSizIndex ).ScaledCoolingCapacity = 0.0;
			} else {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( iNameAlphaNum ) + "\", invalid data." );
				ShowContinueError( "... incorrect " + cAlphaFieldNames( iCoolCAPMAlphaNum ) + "=\"" + cAlphaArgs( iCoolCAPMAlphaNum ) + "\"." );
				ShowContinueError( "... valid values are CoolingDesignCapacity, CapacityPerFloorArea, FractionOfAutosizedCoolingCapacity, or None." );
				ErrorsFound = true;
			}

			// Determine SysSizInput electric heating design capacity sizing method
			if ( SameString( cAlphaArgs( iHeatCAPMAlphaNum ), "HEATINGDESIGNCAPACITY" ) ) {
				SysSizInput( SysSizIndex ).HeatingCapMethod = HeatingDesignCapacity;

				SysSizInput( SysSizIndex ).ScaledHeatingCapacity = rNumericArgs( iHeatDesignCapacityNumericNum );
				if ( SysSizInput( SysSizIndex ).ScaledHeatingCapacity < 0.0 && SysSizInput( SysSizIndex ).ScaledHeatingCapacity != AutoSize ) {
					ShowSevereError( cCurrentModuleObject + " = " + SysSizInput( SysSizIndex ).AirPriLoopName );
					ShowContinueError( "Illegal " + cNumericFieldNames( iHeatDesignCapacityNumericNum ) + " = " + TrimSigDigits( rNumericArgs( iHeatDesignCapacityNumericNum ), 7 ) );
					ErrorsFound = true;
				}
			} else if ( SameString( cAlphaArgs( iHeatCAPMAlphaNum ), "CAPACITYPERFLOORAREA" ) ) {
				SysSizInput( SysSizIndex ).HeatingCapMethod = CapacityPerFloorArea;
				if ( !lNumericFieldBlanks( iHeatCapacityPerFloorAreaNumericNum ) ) {
					SysSizInput( SysSizIndex ).ScaledHeatingCapacity = rNumericArgs( iHeatCapacityPerFloorAreaNumericNum );
					if ( SysSizInput( SysSizIndex ).ScaledHeatingCapacity <= 0.0 ) {
						ShowSevereError( cCurrentModuleObject + " = " + SysSizInput( SysSizIndex ).AirPriLoopName );
						ShowContinueError( "Input for " + cAlphaFieldNames( iHeatCAPMAlphaNum ) + " = " + cAlphaArgs( iHeatCAPMAlphaNum ) );
						ShowContinueError( "Illegal " + cNumericFieldNames( iHeatCapacityPerFloorAreaNumericNum ) + " = " + TrimSigDigits( rNumericArgs( iHeatCapacityPerFloorAreaNumericNum ), 7 ) );
						ErrorsFound = true;
					} else if ( SysSizInput( SysSizIndex ).ScaledHeatingCapacity == AutoSize ) {
						ShowSevereError( cCurrentModuleObject + " = " + SysSizInput( SysSizIndex ).AirPriLoopName );
						ShowContinueError( "Input for " + cAlphaFieldNames( iHeatCAPMAlphaNum ) + " = " + cAlphaArgs( iHeatCAPMAlphaNum ) );
						ShowContinueError( "Illegal " + cNumericFieldNames( iHeatCapacityPerFloorAreaNumericNum ) + " = Autosize" );
						ErrorsFound = true;
					}
				} else {
					ShowSevereError( cCurrentModuleObject + " = " + SysSizInput( SysSizIndex ).AirPriLoopName );
					ShowContinueError( "Input for " + cAlphaFieldNames( iHeatCAPMAlphaNum ) + " = " + cAlphaArgs( iHeatCAPMAlphaNum ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFieldNames( iHeatCapacityPerFloorAreaNumericNum ) );
					ErrorsFound = true;
				}
			} else if ( SameString( cAlphaArgs( iHeatCAPMAlphaNum ), "FRACTIONOFAUTOSIZEDHEATINGCAPACITY" ) ) {
				SysSizInput( SysSizIndex ).HeatingCapMethod = FractionOfAutosizedHeatingCapacity;
				if ( !lNumericFieldBlanks( iHeatFracOfAutosizedCapacityNumericNum ) ) {
					SysSizInput( SysSizIndex ).ScaledHeatingCapacity = rNumericArgs( iHeatFracOfAutosizedCapacityNumericNum );
					if ( SysSizInput( SysSizIndex ).ScaledHeatingCapacity < 0.0 ) {
						ShowSevereError( cCurrentModuleObject + " = " + SysSizInput( SysSizIndex ).AirPriLoopName );
						ShowContinueError( "Illegal " + cNumericFieldNames( iHeatFracOfAutosizedCapacityNumericNum ) + " = " + TrimSigDigits( rNumericArgs( iHeatFracOfAutosizedCapacityNumericNum ), 7 ) );
						ErrorsFound = true;
					}
				} else {
					ShowSevereError( cCurrentModuleObject + " = " + SysSizInput( SysSizIndex ).AirPriLoopName );
					ShowContinueError( "Input for " + cAlphaFieldNames( iHeatCAPMAlphaNum ) + " = " + cAlphaArgs( iHeatCAPMAlphaNum ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFieldNames( iHeatFracOfAutosizedCapacityNumericNum ) );
					ErrorsFound = true;
				}
			} else if ( SameString( cAlphaArgs( iHeatCAPMAlphaNum ), "NONE" ) ) {
				SysSizInput( SysSizIndex ).HeatingCapMethod = None;
				SysSizInput( SysSizIndex ).ScaledHeatingCapacity = 0.0;
			} else {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( iNameAlphaNum ) + "\", invalid data." );
				ShowContinueError( "... incorrect " + cAlphaFieldNames( iHeatCAPMAlphaNum ) + "=\"" + cAlphaArgs( iHeatCAPMAlphaNum ) + "\"." );
				ShowContinueError( "... valid values are HeatingDesignCapacity, CapacityPerFloorArea, FractionOfAutosizedHeatingCapacity, or None." );
				ErrorsFound = true;
			}

		}

		if ( ErrorsFound ) {
			ShowFatalError( cCurrentModuleObject + ": Errors found in getting input. Program terminates." );
		}

	}

	void
	GetPlantSizingInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   October 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains input data for Plant Sizing objects and stores it in
		// appropriate data structures.

		// METHODOLOGY EMPLOYED:
		// Uses InputProcessor "Get" routines to obtain data.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using namespace DataIPShortCuts;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PltSizIndex; // loop index
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int IOStatus; // Used in GetObjectItem
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		int NumDesDays; // Number of design days in input

		cCurrentModuleObject = "Sizing:Plant";
		NumPltSizInput = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumPltSizInput > 0 ) {
			NumDesDays = GetNumObjectsFound( "SizingPeriod:DesignDay" ) + GetNumObjectsFound( "SizingPeriod:WeatherFileDays" ) + GetNumObjectsFound( "SizingPeriod:WeatherFileConditionType" );
			if ( NumDesDays == 0 && DoPlantSizing ) {
				ShowSevereError( "Plant Sizing calculations need SizingPeriod:* input" );
				ErrorsFound = true;
			}
			PlantSizData.allocate( NumPltSizInput );
			for ( auto & e : PlantSizData ) {
				e.PlantLoopName.clear();
				e.ExitTemp = 0.0;
				e.DeltaT = 0.0;
				e.LoopType = 0;
				e.DesVolFlowRate = 0.0;
			}
			for ( int i=1; i<=NumPltSizInput; ++i ) {
				PlantSizData(i).ConcurrenceOption = NonCoincident;
				PlantSizData(i).NumTimeStepsInAvg = 1;
			}
		}

		for ( PltSizIndex = 1; PltSizIndex <= NumPltSizInput; ++PltSizIndex ) {
			GetObjectItem( cCurrentModuleObject, PltSizIndex, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), PlantSizData, &PlantSizingData::PlantLoopName, PltSizIndex - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			PlantSizData( PltSizIndex ).PlantLoopName = cAlphaArgs( 1 );
			PlantSizData( PltSizIndex ).ExitTemp = rNumericArgs( 1 );
			PlantSizData( PltSizIndex ).DeltaT = rNumericArgs( 2 );
			if (NumNumbers > 2) {
			  PlantSizData( PltSizIndex ).NumTimeStepsInAvg = rNumericArgs( 3 );
			} else {
			  PlantSizData( PltSizIndex ).NumTimeStepsInAvg = 1.0;
			}


			{ auto const loopType( cAlphaArgs( 2 ) );
			if ( loopType == "HEATING" ) {
				PlantSizData( PltSizIndex ).LoopType = HeatingLoop;
			} else if ( loopType == "COOLING" ) {
				PlantSizData( PltSizIndex ).LoopType = CoolingLoop;
			} else if ( loopType == "CONDENSER" ) {
				PlantSizData( PltSizIndex ).LoopType = CondenserLoop;
			} else if ( loopType == "STEAM" ) {
				PlantSizData( PltSizIndex ).LoopType = SteamLoop;
			} else {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid data." );
				ShowContinueError( "...incorrect " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
				ShowContinueError( "...Valid values are \"Heating\", \"Cooling\", \"Condenser\" or \"Steam\"." );
				ErrorsFound = true;
			}}

			if ( NumAlphas > 2 ) {
				{auto const concurrenceOption (cAlphaArgs(3) );
				if ( concurrenceOption == "NONCOINCIDENT" ) {
					PlantSizData( PltSizIndex ).ConcurrenceOption = NonCoincident;
				} else if ( concurrenceOption == "COINCIDENT" ) {
					PlantSizData( PltSizIndex ).ConcurrenceOption = Coincident;
				} else {
					ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid data." );
					ShowContinueError( "...incorrect " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"." );
					ShowContinueError( "...Valid values are \"NonCoincident\" or \"Coincident\"." );
					ErrorsFound = true;

				}}
			}
			if (NumAlphas > 3) {
				{auto const sizingFactorOption ( cAlphaArgs(4) );
					if ( sizingFactorOption == "NONE" ) {
						PlantSizData( PltSizIndex ).SizingFactorOption = NoSizingFactorMode;
					} else if ( sizingFactorOption == "GLOBALHEATINGSIZINGFACTOR" ) {
						PlantSizData( PltSizIndex ).SizingFactorOption = GlobalHeatingSizingFactorMode;
					} else if ( sizingFactorOption == "GLOBALCOOLINGSIZINGFACTOR" ) {
						PlantSizData( PltSizIndex ).SizingFactorOption = GlobalCoolingSizingFactorMode;
					} else if ( sizingFactorOption == "LOOPCOMPONENTSIZINGFACTOR" ) {
						PlantSizData( PltSizIndex ).SizingFactorOption = LoopComponentSizingFactorMode;
					}

				}

			}
			SetupEMSInternalVariable( "Plant Design Volume Flow Rate", PlantSizData( PltSizIndex ).PlantLoopName, "[m3/s]", PlantSizData( PltSizIndex ).DesVolFlowRate );
		}

		if ( ErrorsFound ) {
			ShowFatalError( cCurrentModuleObject + ": Errors found in getting input. Program terminates." );
		}

	}

	void
	SetupZoneSizing( bool & ErrorsFound )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         L. Lawrie/F. Buhl
		//       DATE WRITTEN   March 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//  execute a few (1) time steps of a simulation to facilitate setting up model for zone sizing
		//  developed to resolve reverse DD problems caused be the differences
		//  that stem from setup and information gathering that occurs during the first pass.

		// METHODOLOGY EMPLOYED:
		// Using global flag (kickoff sizing simulation), only a few time steps are executed.
		// global flag is used in other parts of simulation to terminate quickly.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataEnvironment::EndMonthFlag;
		using InputProcessor::GetNumRangeCheckErrorsFound;
		using CostEstimateManager::SimCostEstimate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool Available( false ); // an environment is available to process

		//  return  ! remove comment to do "old way"

		Available = true;

		CurOverallSimDay = 0;
		while ( Available ) { // do for each environment

			GetNextEnvironment( Available, ErrorsFound );

			if ( ! Available ) break;
			if ( ErrorsFound ) break;

			// check that environment is one of the design days
			if ( KindOfSim == ksRunPeriodWeather ) {
				continue;
			}

			BeginEnvrnFlag = true;
			EndEnvrnFlag = false;
			EndMonthFlag = false;
			WarmupFlag = true;
			DayOfSim = 0;

			CurEnvirNumSimDay = 1;
			++CurOverallSimDay;

			++DayOfSim;
			BeginDayFlag = true;
			EndDayFlag = false;

			HourOfDay = 1;

			BeginHourFlag = true;
			EndHourFlag = false;

			TimeStep = 1;

			BeginTimeStepFlag = true;

			ManageWeather();

			ManageHeatBalance();

			BeginHourFlag = false;
			BeginDayFlag = false;
			BeginEnvrnFlag = false;
			BeginSimFlag = false;
			BeginFullSimFlag = false;

			//          ! do another timestep=1
			ManageWeather();

			ManageHeatBalance();

			//         do an end of day, end of environment time step

			HourOfDay = 24;
			TimeStep = NumOfTimeStepInHour;
			EndEnvrnFlag = true;

			ManageWeather();

			ManageHeatBalance();

		} // ... End environment loop.

	}

	void
	ReportZoneSizing(
		std::string const & ZoneName, // the name of the zone
		std::string const & LoadType, // the description of the input variable
		Real64 const CalcDesLoad, // the value from the sizing calculation [W]
		Real64 const UserDesLoad, // the value from the sizing calculation modified by user input [W]
		Real64 const CalcDesFlow, // calculated design air flow rate [m3/s]
		Real64 const UserDesFlow, // user input or modified design air flow rate [m3/s]
		std::string const & DesDayName, // the name of the design day that produced the peak
		std::string const & PeakHrMin, // time stamp of the peak
		Real64 const PeakTemp, // temperature at peak [C]
		Real64 const PeakHumRat, // humidity ratio at peak [kg water/kg dry air]
		Real64 const FloorArea, // zone floor area [m2]
		Real64 const TotOccs, // design number of occupants for the zone
		Real64 const MinOAVolFlow, // zone design minimum outside air flow rate [m3/s]
		Real64 const DOASHeatAddRate // zone design heat addition rate from the DOAS [W]
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   Decenber 2001
		//       MODIFIED       August 2008, Greg Stark
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine writes one item of zone sizing data to the "eio" file..

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataPrecisionGlobals;
		using DataGlobals::OutputFileInits;
		using DataStringGlobals::VerString;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyOneTimeFlag( true );

		// Formats
		static gio::Fmt Format_990( "('! <Zone Sizing Information>, Zone Name, Load Type, Calc Des Load {W}, User Des Load {W}, ','Calc Des Air Flow Rate {m3/s}, ','User Des Air Flow Rate {m3/s}, Design Day Name, Date/Time of Peak, Temperature at Peak {C}, ','Humidity Ratio at Peak {kgWater/kgDryAir}, Floor Area {m2}, # Occupants, Calc Outdoor Air Flow Rate {m3/s}, Calc DOAS Heat Addition Rate {W}')" );
		static gio::Fmt Format_991( "(' Zone Sizing Information',14(', ',A))" );

		if ( MyOneTimeFlag ) {
			gio::write( OutputFileInits, Format_990 );
			MyOneTimeFlag = false;
		}

		gio::write( OutputFileInits, Format_991 ) << ZoneName << LoadType << RoundSigDigits( CalcDesLoad, 5 ) << RoundSigDigits( UserDesLoad, 5 ) << RoundSigDigits( CalcDesFlow, 5 ) << RoundSigDigits( UserDesFlow, 5 ) << DesDayName << PeakHrMin << RoundSigDigits( PeakTemp, 5 ) << RoundSigDigits( PeakHumRat, 5 ) << RoundSigDigits( FloorArea, 5 ) << RoundSigDigits( TotOccs, 5 ) << RoundSigDigits( MinOAVolFlow, 5 ) << RoundSigDigits( DOASHeatAddRate , 5 );

		// BSLLC Start
		if ( sqlite ) {
			sqlite->addSQLiteZoneSizingRecord( ZoneName, LoadType, CalcDesLoad, UserDesLoad, CalcDesFlow, UserDesFlow, DesDayName, PeakHrMin, PeakTemp, PeakHumRat, MinOAVolFlow, DOASHeatAddRate );
		}
		// BSLLC Finish

	}

	void
	ReportSysSizing(
		std::string const & SysName, // the name of the zone
		std::string const & VarDesc, // the description of the input variable
		Real64 const VarValue // the value from the sizing calculation
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   January 2003
		//       MODIFIED       August 2008, Greg Stark
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine writes one item of system sizing data to the "eio" file..

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataPrecisionGlobals;
		using DataGlobals::OutputFileInits;
		using DataStringGlobals::VerString;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyOneTimeFlag( true );

		// Formats
		static gio::Fmt Format_990( "('! <System Sizing Information>, System Name, ','Field Description, Value')" );
		static gio::Fmt Format_991( "(' System Sizing Information',3(', ',A))" );

		if ( MyOneTimeFlag ) {
			gio::write( OutputFileInits, Format_990 );
			MyOneTimeFlag = false;
		}

		gio::write( OutputFileInits, Format_991 ) << SysName << VarDesc << RoundSigDigits( VarValue, 5 );

		// BSLLC Start
		if ( sqlite ) sqlite->addSQLiteSystemSizingRecord( SysName, VarDesc, VarValue );
		// BSLLC Finish

	}


	void
	GetZoneHVACSizing()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Nigusse - FSEC
		//       DATE WRITTEN   July 2014
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains input data for the ZoneHVAC sizing methods object and stores it in
		// appropriate data structure.

		// METHODOLOGY EMPLOYED:
		// Uses InputProcessor "Get" routines to obtain data.
		// This object requires only a name where the default values are assumed
		// if subsequent fields are not entered.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectDefMaxArgs;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using namespace DataIPShortCuts;
		using General::RoundSigDigits;
		using General::TrimSigDigits;
		using DataSizing::NumZoneHVACSizing;
		using DataSizing::ZoneHVACSizing;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetZoneHVACSizing: " ); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int iHeatSAFMAlphaNum; // get input index to Zone HVAC sizing heat supp air flow method
		int iCoolSAFMAlphaNum; // get input index to Zone HVAC sizing cool supp air flow method
		int iMaxCoolAirVolFlowNumericNum; // get input index to Zone HVAC sizing cool supply air flow
		int iMaxHeatAirVolFlowNumericNum; // get input index to Zone HVAC sizing heat supply air flow
		int iNoCoolHeatSAFMAlphaNum; // get input index to Zone HVAC sizing no cool/heat supply air flow
		int iMaxNoCoolHeatAirVolFlowNumericNum; // get input index to Zone HVAC sizing no cool/heat supply air flow
		int iCoolFlowPerFloorAreaNumericNum; // get input index to Zone HVAC sizing cool flow per floor area
		int iCoolFlowPerFracCoolNumericNum; // get input index to Zone HVAC sizing cool flow per fraction cool
		int iCoolFlowPerCoolCapNumericNum; // get input index to Zone HVAC sizing cool flow per cooling cap
		int iHeatFlowPerFloorAreaNumericNum; // get input index to Zone HVAC sizing heat flow per floor area
		int iHeatFlowPerFracCoolNumericNum; // get input index to Zone HVAC sizing heat flow per fraction heat
		int iHeatFlowPerHeatCapNumericNum; // get input index to Zone HVAC sizing heat flow per heating cap
		int iNoCoolHeatFlowPerFloorAreaNumericNum; // get input index to Zone HVAC sizing no cool/heat FPA
		int iNoCoolHeatFlowPerFracCoolNumericNum; // get input index to Zone HVAC sizing no cool/heat FPFC
		int iNoCoolHeatFlowPerFracHeatNumericNum; // get input index to Zone HVAC sizing no cool/heat FPFH

		int iCoolCAPMAlphaNum; // get input index to Zone HVAC sizing chilled water flow method
		int iCoolDesignCapacityNumericNum; // get input index to Zone HVAC sizing chilled water flow
		int iCoolCapacityPerFloorAreaNumericNum; // get input index to Zone HVAC sizing cooling capacity per floor area
		int iCoolFracOfAutosizedCapacityNumericNum;  // get input index to Zone HVAC sizing capacity as fraction autozized cooling capacity

		int iHeatCAPMAlphaNum; // get input index to Zone HVAC sizing heating capacity
		int iHeatDesignCapacityNumericNum; // get input index to Zone HVAC sizing heating design capacity
		int iHeatCapacityPerFloorAreaNumericNum; // get input index to Zone HVAC sizing heating capacity per floor area
		int iHeatFracOfAutosizedCapacityNumericNum; // get input index to Zone HVAC sizing capacity as fraction autozized cooling capacity

		iCoolSAFMAlphaNum = 2; // get input index to Zone HVAC sizing heat supp air flow method
		iMaxCoolAirVolFlowNumericNum = 1; // get input index to Zone HVAC sizing cool supply air flow
		iCoolFlowPerFloorAreaNumericNum = 2; // get input index to Zone HVAC sizing cool flow per floor area
		iCoolFlowPerFracCoolNumericNum = 3; // get input index to Zone HVAC sizing cool flow per fraction cool
		iCoolFlowPerCoolCapNumericNum = 4; // get input index to Zone HVAC sizing cool flow per cooling cap


		iNoCoolHeatSAFMAlphaNum = 3; // get input index to Zone HVAC sizing no cool/heat supply air flow
		iMaxNoCoolHeatAirVolFlowNumericNum = 5; // get input index to Zone HVAC sizing no cool/heat supply air flow
		iNoCoolHeatFlowPerFloorAreaNumericNum = 6; // get input index to Zone HVAC sizing no cool/heat FPA
		iNoCoolHeatFlowPerFracCoolNumericNum = 7; // get input index to Zone HVAC sizing no cool/heat FPFC
		iNoCoolHeatFlowPerFracHeatNumericNum = 8; // get input index to Zone HVAC sizing no cool/heat FPFH

		iHeatSAFMAlphaNum = 4; // get input index to Zone HVAC sizing cool supp air flow method
		iMaxHeatAirVolFlowNumericNum = 9; // get input index to Zone HVAC sizing heat supply air flow
		iHeatFlowPerFloorAreaNumericNum = 10; // get input index to Zone HVAC sizing heat flow per floor area
		iHeatFlowPerFracCoolNumericNum = 11; // get input index to Zone HVAC sizing heat flow per fraction heat
		iHeatFlowPerHeatCapNumericNum = 12; // get input index to Zone HVAC sizing heat flow per heating cap

		iCoolCAPMAlphaNum = 5; // get input index to Zone HVAC sizing cooling design capacity method
		iCoolDesignCapacityNumericNum = 13; // get input index to Zone HVAC sizing cooling design capacity
		iCoolCapacityPerFloorAreaNumericNum = 14; // get input index to Zone HVAC sizing cooling design capacity per floor area
		iCoolFracOfAutosizedCapacityNumericNum = 15; // get input index to Zone HVAC sizing as a fraction of cooling design capacity


		iHeatCAPMAlphaNum = 6; // get input index to Zone HVAC sizing heating capacity
		iHeatDesignCapacityNumericNum = 16; // get input index to Zone HVAC sizing heating design capacity
		iHeatCapacityPerFloorAreaNumericNum = 17; // get input index to Zone HVAC sizing heating capacity per floor area
		iHeatFracOfAutosizedCapacityNumericNum = 18; // get input index to Zone HVAC sizing capacity as fraction autozized heating capacity


		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int TotalArgs; // Total number of alpha and numeric arguments (max) for a
		int IOStatus; // Used in GetObjectItem
		int zSIndex;  // index of "DesignSpecification:ZoneHVAC:Sizing" objects
		static bool ErrorsFound( false ); // If errors detected in input
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		//  REAL(r64) :: CalcAmt

		std::string CurrentModuleObject; // for ease in getting objects
		Array1D_string Alphas; // Alpha input items for object
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D< Real64 > Numbers; // Numeric input items for object
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.

		CurrentModuleObject = "DesignSpecification:ZoneHVAC:Sizing";
		NumZoneHVACSizing = GetNumObjectsFound( CurrentModuleObject );
		GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );

		Alphas.allocate( NumAlphas );
		cAlphaFields.allocate( NumAlphas );
		cNumericFields.allocate( NumNumbers );
		Numbers.dimension( NumNumbers, 0.0 );
		lAlphaBlanks.dimension( NumAlphas, true );
		lNumericBlanks.dimension( NumNumbers, true );

		if ( NumZoneHVACSizing > 0 ) {
			ZoneHVACSizing.allocate( NumZoneHVACSizing );

			//Start Loading the System Input
			for ( zSIndex = 1; zSIndex <= NumZoneHVACSizing; ++zSIndex ) {

				Alphas = "";
				cAlphaFields = "";
				cNumericFields = "";
				Numbers = 0;
				lAlphaBlanks = true;
				lNumericBlanks = true;

				GetObjectItem( CurrentModuleObject, zSIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

				VerifyName( Alphas( 1 ), ZoneHVACSizing, zSIndex - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) Alphas( 1 ) = "xxxxx";
				}

				ZoneHVACSizing( zSIndex ).Name = Alphas( 1 );

				// Determine supply air flow rate sizing method for cooling mode
				if ( SameString( Alphas( iCoolSAFMAlphaNum ), "SupplyAirFlowRate" ) ) {
					ZoneHVACSizing( zSIndex ).CoolingSAFMethod = SupplyAirFlowRate;;

					if ( !lNumericBlanks( iMaxCoolAirVolFlowNumericNum ) ) {
						ZoneHVACSizing( zSIndex ).MaxCoolAirVolFlow = Numbers( iMaxCoolAirVolFlowNumericNum );
						if ( ZoneHVACSizing( zSIndex ).MaxCoolAirVolFlow == AutoSize ) ZoneHVACSizing( zSIndex ).RequestAutoSize = true;
						if ( ZoneHVACSizing( zSIndex ).MaxCoolAirVolFlow <= 0.0 && ZoneHVACSizing( zSIndex ).MaxCoolAirVolFlow != AutoSize ) {
							ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
							ShowContinueError( "Illegal " + cNumericFields( iMaxCoolAirVolFlowNumericNum ) + " = " + TrimSigDigits( Numbers( iMaxCoolAirVolFlowNumericNum ), 7 ) );
							ErrorsFound = true;
						}
					} else {
						ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iCoolSAFMAlphaNum ) + " = " + Alphas( iCoolSAFMAlphaNum ) );
						ShowContinueError( "Blank field not allowed for " + cNumericFields( iMaxCoolAirVolFlowNumericNum ) );
						ErrorsFound = true;
					}
				} else if ( SameString( Alphas( iCoolSAFMAlphaNum ), "FlowPerFloorArea" ) ) {
					ZoneHVACSizing( zSIndex ).CoolingSAFMethod = FlowPerFloorArea;
					if ( !lNumericBlanks( iCoolFlowPerFloorAreaNumericNum ) ) {
						ZoneHVACSizing( zSIndex ).MaxCoolAirVolFlow = Numbers( iCoolFlowPerFloorAreaNumericNum );
						if ( ZoneHVACSizing( zSIndex ).MaxCoolAirVolFlow <= 0.0 && ZoneHVACSizing( zSIndex ).MaxCoolAirVolFlow != AutoSize ) {
							ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
							ShowContinueError( "Input for " + cAlphaFields( iCoolSAFMAlphaNum ) + " = " + Alphas( iCoolSAFMAlphaNum ) );
							ShowContinueError( "Illegal " + cNumericFields( iCoolFlowPerFloorAreaNumericNum ) + " = " + TrimSigDigits( Numbers( iCoolFlowPerFloorAreaNumericNum ), 7 ) );
							ErrorsFound = true;
							// Autosized input is not allowed
						} else if ( ZoneHVACSizing( zSIndex ).MaxCoolAirVolFlow == AutoSize ) {
							ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
							ShowContinueError( "Input for " + cAlphaFields( iCoolSAFMAlphaNum ) + " = " + Alphas( iCoolSAFMAlphaNum ) );
							ShowContinueError( "Illegal " + cNumericFields( iCoolFlowPerFloorAreaNumericNum ) + " = Autosize" );
							ErrorsFound = true;
						} else {
							// user input cooling supply air flow per unit conditioned area is saved in ZoneHVACSizing(zSIndex).MaxCoolAirVolFlow
							ZoneHVACSizing( zSIndex ).RequestAutoSize = true;
						}
					} else {
						ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iCoolSAFMAlphaNum ) + " = " + Alphas( iCoolSAFMAlphaNum ) );
						ShowContinueError( "Blank field not allowed for " + cNumericFields( iCoolFlowPerFloorAreaNumericNum ) );
						ErrorsFound = true;
					}
				} else if ( SameString( Alphas( iCoolSAFMAlphaNum ), "FractionOfAutosizedCoolingAirflow" ) ) {
					ZoneHVACSizing( zSIndex ).CoolingSAFMethod = FractionOfAutosizedCoolingAirflow;
					if ( !lNumericBlanks( iCoolFlowPerFracCoolNumericNum ) ) {
						ZoneHVACSizing( zSIndex ).MaxCoolAirVolFlow = Numbers( iCoolFlowPerFracCoolNumericNum );
						if ( ZoneHVACSizing( zSIndex ).MaxCoolAirVolFlow <= 0.0 && ZoneHVACSizing( zSIndex ).MaxCoolAirVolFlow != AutoSize ) {
							ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
							ShowContinueError( "Input for " + cAlphaFields( iCoolSAFMAlphaNum ) + " = " + Alphas( iCoolSAFMAlphaNum ) );
							ShowContinueError( "Illegal " + cNumericFields( iCoolFlowPerFracCoolNumericNum ) + " = " + TrimSigDigits( Numbers( iCoolFlowPerFracCoolNumericNum ), 7 ) );
							ErrorsFound = true;
							// Autosized input is not allowed
						} else if ( ZoneHVACSizing( zSIndex ).MaxCoolAirVolFlow == AutoSize ) {
							ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
							ShowContinueError( "Input for " + cAlphaFields( iCoolSAFMAlphaNum ) + " = " + Alphas( iCoolSAFMAlphaNum ) );
							ShowContinueError( "Illegal " + cNumericFields( iCoolFlowPerFracCoolNumericNum ) + " = Autosize" );
							ErrorsFound = true;
						} else {
							// user input fraction of cooling supply air flow rate is saved in ZoneHVACSizing(zSIndex).MaxCoolAirVolFlow
							ZoneHVACSizing( zSIndex ).RequestAutoSize = true;
						}
					} else {
						ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iCoolSAFMAlphaNum ) + " = " + Alphas( iCoolSAFMAlphaNum ) );
						ShowContinueError( "Blank field not allowed for " + cNumericFields( iCoolFlowPerFracCoolNumericNum ) );
						ErrorsFound = true;
					}
				} else if ( SameString( Alphas( iCoolSAFMAlphaNum ), "FlowPerCoolingCapacity" ) ) {

					ZoneHVACSizing( zSIndex ).CoolingSAFMethod = FlowPerCoolingCapacity;
					if ( !lNumericBlanks( iCoolFlowPerCoolCapNumericNum ) ) {
						ZoneHVACSizing( zSIndex ).MaxCoolAirVolFlow = Numbers( iCoolFlowPerCoolCapNumericNum );
						if ( ZoneHVACSizing( zSIndex ).MaxCoolAirVolFlow <= 0.0 && ZoneHVACSizing( zSIndex ).MaxCoolAirVolFlow != AutoSize ) {
							ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
							ShowContinueError( "Input for " + cAlphaFields( iCoolSAFMAlphaNum ) + " = " + Alphas( iCoolSAFMAlphaNum ) );
							ShowContinueError( "Illegal " + cNumericFields( iCoolFlowPerCoolCapNumericNum ) + " = " + TrimSigDigits( Numbers( iCoolFlowPerCoolCapNumericNum ), 7 ) );
							ErrorsFound = true;
							// Autosized input is not allowed
						} else if ( ZoneHVACSizing( zSIndex ).MaxCoolAirVolFlow == AutoSize ) {
							ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
							ShowContinueError( "Input for " + cAlphaFields( iCoolSAFMAlphaNum ) + " = " + Alphas( iCoolSAFMAlphaNum ) );
							ShowContinueError( "Illegal " + cNumericFields( iCoolFlowPerCoolCapNumericNum ) + " = Autosize" );
							ErrorsFound = true;
						} else {
							// user input cooling supply air flow per unit cooling capacity is saved in ZoneHVACSizing(zSIndex).MaxCoolAirVolFlow
							ZoneHVACSizing( zSIndex ).RequestAutoSize = true;
						}
					} else {
						ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iCoolSAFMAlphaNum ) + " = " + Alphas( iCoolSAFMAlphaNum ) );
						ShowContinueError( "Blank field not allowed for " + cNumericFields( iCoolFlowPerCoolCapNumericNum ) );
						ErrorsFound = true;
					}
				} else if ( SameString( Alphas( iCoolSAFMAlphaNum ), "None" ) || lAlphaBlanks( iCoolSAFMAlphaNum ) ) {
					ZoneHVACSizing( zSIndex ).CoolingSAFMethod = None;
					ZoneHVACSizing( zSIndex ).MaxCoolAirVolFlow = 0.0;
					// cooling supply air flow rate will not be sized, may be cooling coil does not exist
				} else {
					ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
					ShowContinueError( "Illegal " + cAlphaFields( iCoolSAFMAlphaNum ) + " = " + Alphas( iCoolSAFMAlphaNum ) );
					ErrorsFound = true;
				}
				// Determine supply air flow rate sizing method for heating mode
				if ( SameString( Alphas( iHeatSAFMAlphaNum ), "SupplyAirFlowRate" ) ) {
					ZoneHVACSizing( zSIndex ).HeatingSAFMethod = SupplyAirFlowRate;
					if ( !lNumericBlanks( iMaxHeatAirVolFlowNumericNum ) ) {
						ZoneHVACSizing( zSIndex ).MaxHeatAirVolFlow = Numbers( iMaxHeatAirVolFlowNumericNum );
						if ( ZoneHVACSizing( zSIndex ).MaxHeatAirVolFlow == AutoSize ) ZoneHVACSizing( zSIndex ).RequestAutoSize = true;

						if ( ZoneHVACSizing( zSIndex ).MaxHeatAirVolFlow <= 0.0 && ZoneHVACSizing( zSIndex ).MaxHeatAirVolFlow != AutoSize ) {
							ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
							ShowContinueError( "Illegal " + cNumericFields( iMaxHeatAirVolFlowNumericNum ) + " = " + TrimSigDigits( Numbers( iMaxHeatAirVolFlowNumericNum ), 7 ) );
							ErrorsFound = true;
						}
					} else {
						ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iHeatSAFMAlphaNum ) + " = " + Alphas( iHeatSAFMAlphaNum ) );
						ShowContinueError( "Blank field not allowed for " + cNumericFields( iMaxHeatAirVolFlowNumericNum ) );
						ErrorsFound = true;
					}
				} else if ( SameString( Alphas( iHeatSAFMAlphaNum ), "FlowPerFloorArea" ) ) {
					ZoneHVACSizing( zSIndex ).HeatingSAFMethod = FlowPerFloorArea;
					if ( !lNumericBlanks( iHeatFlowPerFloorAreaNumericNum ) ) {
						ZoneHVACSizing( zSIndex ).MaxHeatAirVolFlow = Numbers( iHeatFlowPerFloorAreaNumericNum );
						if ( ZoneHVACSizing( zSIndex ).MaxHeatAirVolFlow <= 0.0 && ZoneHVACSizing( zSIndex ).MaxHeatAirVolFlow != AutoSize ) {
							ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
							ShowContinueError( "Input for " + cAlphaFields( iHeatSAFMAlphaNum ) + " = " + Alphas( iHeatSAFMAlphaNum ) );
							ShowContinueError( "Illegal " + cNumericFields( iHeatFlowPerFloorAreaNumericNum ) + " = " + TrimSigDigits( Numbers( iHeatFlowPerFloorAreaNumericNum ), 7 ) );
							ErrorsFound = true;
							// Autosized input is not allowed
						} else if ( ZoneHVACSizing( zSIndex ).MaxHeatAirVolFlow == AutoSize ) {
							ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
							ShowContinueError( "Input for " + cAlphaFields( iHeatSAFMAlphaNum ) + " = " + Alphas( iHeatSAFMAlphaNum ) );
							ShowContinueError( "Illegal " + cNumericFields( iHeatFlowPerFloorAreaNumericNum ) + " = Autosize" );
							ErrorsFound = true;
						} else {
							// user input heating supply air flow per unit conditioned area is saved in ZoneHVACSizing(zSIndex).MaxHeatAirVolFlow
							ZoneHVACSizing( zSIndex ).RequestAutoSize = true;
						}
					} else {
						ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iHeatSAFMAlphaNum ) + " = " + Alphas( iHeatSAFMAlphaNum ) );
						ShowContinueError( "Blank field not allowed for " + cNumericFields( iHeatFlowPerFloorAreaNumericNum ) );
						ErrorsFound = true;
					}
				} else if ( SameString( Alphas( iHeatSAFMAlphaNum ), "FractionOfAutosizedHeatingAirflow" ) ) {
					ZoneHVACSizing( zSIndex ).HeatingSAFMethod = FractionOfAutosizedHeatingAirflow;
					if ( !lNumericBlanks( iHeatFlowPerFracCoolNumericNum ) ) {
						ZoneHVACSizing( zSIndex ).MaxHeatAirVolFlow = Numbers( iHeatFlowPerFracCoolNumericNum );
						if ( ZoneHVACSizing( zSIndex ).MaxHeatAirVolFlow <= 0.0 && ZoneHVACSizing( zSIndex ).MaxHeatAirVolFlow != AutoSize ) {
							ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
							ShowContinueError( "Input for " + cAlphaFields( iHeatSAFMAlphaNum ) + " = " + Alphas( iHeatSAFMAlphaNum ) );
							ShowContinueError( "Illegal " + cNumericFields( iHeatFlowPerFracCoolNumericNum ) + " = " + TrimSigDigits( Numbers( iHeatFlowPerFracCoolNumericNum ), 7 ) );
							ErrorsFound = true;
							// Autosized input is not allowed
						} else if ( ZoneHVACSizing( zSIndex ).MaxHeatAirVolFlow == AutoSize ) {
							ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
							ShowContinueError( "Input for " + cAlphaFields( iHeatSAFMAlphaNum ) + " = " + Alphas( iHeatSAFMAlphaNum ) );
							ShowContinueError( "Illegal " + cNumericFields( iHeatFlowPerFracCoolNumericNum ) + " = Autosize" );
							ErrorsFound = true;
						} else {
							// user input fraction of heating supply air flow rate is saved in ZoneHVACSizing(zSIndex).MaxHeatAirVolFlow
							ZoneHVACSizing( zSIndex ).RequestAutoSize = true;
						}
					} else {
						ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iHeatSAFMAlphaNum ) + " = " + Alphas( iHeatSAFMAlphaNum ) );
						ShowContinueError( "Blank field not allowed for " + cNumericFields( iHeatFlowPerFracCoolNumericNum ) );
						ErrorsFound = true;
					}
				} else if ( SameString( Alphas( iHeatSAFMAlphaNum ), "FlowPerHeatingCapacity" ) ) {
					ZoneHVACSizing( zSIndex ).HeatingSAFMethod = FlowPerHeatingCapacity;
					if ( !lNumericBlanks( iHeatFlowPerHeatCapNumericNum ) ) {
						ZoneHVACSizing( zSIndex ).MaxHeatAirVolFlow = Numbers( iHeatFlowPerHeatCapNumericNum );
						if ( ZoneHVACSizing( zSIndex ).MaxHeatAirVolFlow <= 0.0 && ZoneHVACSizing( zSIndex ).MaxHeatAirVolFlow != AutoSize ) {
							ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
							ShowContinueError( "Input for " + cAlphaFields( iHeatSAFMAlphaNum ) + " = " + Alphas( iHeatSAFMAlphaNum ) );
							ShowContinueError( "Illegal " + cNumericFields( iHeatFlowPerHeatCapNumericNum ) + " = " + TrimSigDigits( Numbers( iHeatFlowPerHeatCapNumericNum ), 7 ) );
							ErrorsFound = true;
							// Autosized input is not allowed
						} else if ( ZoneHVACSizing( zSIndex ).MaxHeatAirVolFlow == AutoSize ) {
							ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
							ShowContinueError( "Input for " + cAlphaFields( iHeatSAFMAlphaNum ) + " = " + Alphas( iHeatSAFMAlphaNum ) );
							ShowContinueError( "Illegal " + cNumericFields( iHeatFlowPerHeatCapNumericNum ) + " = Autosize" );
							ErrorsFound = true;
						} else {
							// user input heating supply air flow per unit heating capacity is saved in ZoneHVACSizing(zSIndex).MaxHeatAirVolFlow
							ZoneHVACSizing( zSIndex ).RequestAutoSize = true;
						}
					} else {
						ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iHeatSAFMAlphaNum ) + " = " + Alphas( iHeatSAFMAlphaNum ) );
						ShowContinueError( "Blank field not allowed for " + cNumericFields( iHeatFlowPerHeatCapNumericNum ) );
						ErrorsFound = true;
					}
				} else if ( SameString( Alphas( iHeatSAFMAlphaNum ), "None" ) || lAlphaBlanks( iHeatSAFMAlphaNum ) ) {
					ZoneHVACSizing( zSIndex ).HeatingSAFMethod = None;
					ZoneHVACSizing( zSIndex ).MaxHeatAirVolFlow = 0.0;
				} else {
					ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
					ShowContinueError( "Illegal " + cAlphaFields( iHeatSAFMAlphaNum ) + " = " + Alphas( iHeatSAFMAlphaNum ) );
					ErrorsFound = true;
				}

				// Determine supply air flow rate sizing method when cooling or heating is not needed
				if ( SameString( Alphas( iNoCoolHeatSAFMAlphaNum ), "SupplyAirFlowRate" ) ) {
					ZoneHVACSizing( zSIndex ).NoCoolHeatSAFMethod = SupplyAirFlowRate;
					if ( !lNumericBlanks( iMaxNoCoolHeatAirVolFlowNumericNum ) ) {
						ZoneHVACSizing( zSIndex ).MaxNoCoolHeatAirVolFlow = Numbers( iMaxNoCoolHeatAirVolFlowNumericNum );
						if ( ZoneHVACSizing( zSIndex ).MaxNoCoolHeatAirVolFlow == AutoSize ) ZoneHVACSizing( zSIndex ).RequestAutoSize = true;
						if ( ZoneHVACSizing( zSIndex ).MaxNoCoolHeatAirVolFlow < 0.0 && ZoneHVACSizing( zSIndex ).MaxNoCoolHeatAirVolFlow != AutoSize ) {
							ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
							ShowContinueError( "Illegal " + cNumericFields( iMaxNoCoolHeatAirVolFlowNumericNum ) + " = " + TrimSigDigits( Numbers( iMaxNoCoolHeatAirVolFlowNumericNum ), 7 ) );
							ErrorsFound = true;
						}
					} else {
						ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iNoCoolHeatSAFMAlphaNum ) + " = " + Alphas( iNoCoolHeatSAFMAlphaNum ) );
						ShowContinueError( "Blank field not allowed for " + cNumericFields( iMaxNoCoolHeatAirVolFlowNumericNum ) );
						ErrorsFound = true;
					}
				} else if ( SameString( Alphas( iNoCoolHeatSAFMAlphaNum ), "FlowPerFloorArea" ) ) {
					ZoneHVACSizing( zSIndex ).NoCoolHeatSAFMethod = FlowPerFloorArea;
					if ( !lNumericBlanks( iNoCoolHeatFlowPerFloorAreaNumericNum ) ) {
						ZoneHVACSizing( zSIndex ).MaxNoCoolHeatAirVolFlow = Numbers( iNoCoolHeatFlowPerFloorAreaNumericNum );
						if ( ZoneHVACSizing( zSIndex ).MaxNoCoolHeatAirVolFlow < 0.0 && ZoneHVACSizing( zSIndex ).MaxNoCoolHeatAirVolFlow != AutoSize ) {
							ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
							ShowContinueError( "Input for " + cAlphaFields( iNoCoolHeatSAFMAlphaNum ) + " = " + Alphas( iNoCoolHeatSAFMAlphaNum ) );
							ShowContinueError( "Illegal " + cNumericFields( iNoCoolHeatFlowPerFloorAreaNumericNum ) + " = " + TrimSigDigits( Numbers( iNoCoolHeatFlowPerFloorAreaNumericNum ), 7 ) );
							ErrorsFound = true;
							// Autosized input is not allowed
						} else if ( ZoneHVACSizing( zSIndex ).MaxNoCoolHeatAirVolFlow == AutoSize ) {
							ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
							ShowContinueError( "Input for " + cAlphaFields( iNoCoolHeatSAFMAlphaNum ) + " = " + Alphas( iNoCoolHeatSAFMAlphaNum ) );
							ShowContinueError( "Illegal " + cNumericFields( iNoCoolHeatFlowPerFloorAreaNumericNum ) + " = Autosize" );
							ErrorsFound = true;
						} else {
							// user input supply air flow per unit floor area during no cooling or heating area is saved in ZoneHVACSizing(zSIndex).MaxNoCoolHeatAirVolFlow
							ZoneHVACSizing( zSIndex ).RequestAutoSize = true;
						}
					} else {
						ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iNoCoolHeatSAFMAlphaNum ) + " = " + Alphas( iNoCoolHeatSAFMAlphaNum ) );
						ShowContinueError( "Blank field not allowed for " + cNumericFields( iNoCoolHeatFlowPerFloorAreaNumericNum ) );
						ErrorsFound = true;
					}
				} else if ( SameString( Alphas( iNoCoolHeatSAFMAlphaNum ), "FractionOfAutosizedCoolingAirflow" ) ) {
					ZoneHVACSizing( zSIndex ).NoCoolHeatSAFMethod = FractionOfAutosizedCoolingAirflow;
					if ( !lNumericBlanks( iNoCoolHeatFlowPerFracCoolNumericNum ) ) {
						ZoneHVACSizing( zSIndex ).MaxNoCoolHeatAirVolFlow = Numbers( iNoCoolHeatFlowPerFracCoolNumericNum );
						if ( ZoneHVACSizing( zSIndex ).MaxNoCoolHeatAirVolFlow < 0.0 && ZoneHVACSizing( zSIndex ).MaxNoCoolHeatAirVolFlow != AutoSize ) {
							ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
							ShowContinueError( "Input for " + cAlphaFields( iNoCoolHeatSAFMAlphaNum ) + " = " + Alphas( iNoCoolHeatSAFMAlphaNum ) );
							ShowContinueError( "Illegal " + cNumericFields( iNoCoolHeatFlowPerFracCoolNumericNum ) + " = " + TrimSigDigits( Numbers( iNoCoolHeatFlowPerFracCoolNumericNum ), 7 ) );
							ErrorsFound = true;
							// Autosized input is not allowed
						} else if ( ZoneHVACSizing( zSIndex ).MaxNoCoolHeatAirVolFlow == AutoSize ) {
							ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
							ShowContinueError( "Input for " + cAlphaFields( iNoCoolHeatSAFMAlphaNum ) + " = " + Alphas( iNoCoolHeatSAFMAlphaNum ) );
							ShowContinueError( "Illegal " + cNumericFields( iNoCoolHeatFlowPerFracCoolNumericNum ) + " = Autosize" );
							ErrorsFound = true;
						} else {
							// user input frcation of cooling supply air flow rate during no cooling or heating area is saved in ZoneHVACSizing(zSIndex).MaxNoCoolHeatAirVolFlow
							ZoneHVACSizing( zSIndex ).RequestAutoSize = true;
						}
					} else {
						ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iNoCoolHeatSAFMAlphaNum ) + " = " + Alphas( iNoCoolHeatSAFMAlphaNum ) );
						ShowContinueError( "Blank field not allowed for " + cNumericFields( iNoCoolHeatFlowPerFracCoolNumericNum ) );
						ErrorsFound = true;
					}
				} else if ( SameString( Alphas( iNoCoolHeatSAFMAlphaNum ), "FractionOfAutosizedHeatingAirflow" ) ) {
					ZoneHVACSizing( zSIndex ).NoCoolHeatSAFMethod = FractionOfAutosizedHeatingAirflow;
					if ( !lNumericBlanks( iNoCoolHeatFlowPerFracHeatNumericNum ) ) {
						ZoneHVACSizing( zSIndex ).MaxNoCoolHeatAirVolFlow = Numbers( iNoCoolHeatFlowPerFracHeatNumericNum );
						if ( ZoneHVACSizing( zSIndex ).MaxNoCoolHeatAirVolFlow < 0.0 && ZoneHVACSizing( zSIndex ).MaxNoCoolHeatAirVolFlow != AutoSize ) {
							ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
							ShowContinueError( "Input for " + cAlphaFields( iNoCoolHeatSAFMAlphaNum ) + " = " + Alphas( iNoCoolHeatSAFMAlphaNum ) );
							ShowContinueError( "Illegal " + cNumericFields( iNoCoolHeatFlowPerFracHeatNumericNum ) + " = " + TrimSigDigits( Numbers( iNoCoolHeatFlowPerFracHeatNumericNum ), 7 ) );
							ErrorsFound = true;
							// Autosized input is not allowed
						} else if ( ZoneHVACSizing( zSIndex ).MaxNoCoolHeatAirVolFlow == AutoSize ) {
							ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
							ShowContinueError( "Input for " + cAlphaFields( iNoCoolHeatSAFMAlphaNum ) + " = " + Alphas( iNoCoolHeatSAFMAlphaNum ) );
							ShowContinueError( "Illegal " + cNumericFields( iNoCoolHeatFlowPerFracHeatNumericNum ) + " = Autosize" );
							ErrorsFound = true;
						} else {
							// user input frcation of heating supply air flow rate during no cooling or heating area is saved in ZoneHVACSizing(zSIndex).MaxNoCoolHeatAirVolFlow
							ZoneHVACSizing( zSIndex ).RequestAutoSize = true;
						}
					} else {
						ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iNoCoolHeatSAFMAlphaNum ) + " = " + Alphas( iNoCoolHeatSAFMAlphaNum ) );
						ShowContinueError( "Blank field not allowed for " + cNumericFields( iNoCoolHeatFlowPerFracHeatNumericNum ) );
						ErrorsFound = true;
					}
				} else if ( SameString( Alphas( iNoCoolHeatSAFMAlphaNum ), "None" ) || lAlphaBlanks( iNoCoolHeatSAFMAlphaNum ) ) {
					ZoneHVACSizing( zSIndex ).NoCoolHeatSAFMethod = None;
					ZoneHVACSizing( zSIndex ).MaxNoCoolHeatAirVolFlow = 0.0;
				} else {
					ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
					ShowContinueError( "Illegal " + cAlphaFields( iNoCoolHeatSAFMAlphaNum ) + " = " + Alphas( iNoCoolHeatSAFMAlphaNum ) );
					ErrorsFound = true;
				}

				// Determine cooling design capacity of zoneHVAC equipment
				if ( SameString( Alphas( iCoolCAPMAlphaNum ), "CoolingDesignCapacity" ) ) {
					ZoneHVACSizing( zSIndex ).CoolingCapMethod = CoolingDesignCapacity;
					if ( !lNumericBlanks( iCoolDesignCapacityNumericNum ) ) {
						ZoneHVACSizing( zSIndex ).ScaledCoolingCapacity = Numbers( iCoolDesignCapacityNumericNum );
						if ( ZoneHVACSizing( zSIndex ).ScaledCoolingCapacity == AutoSize ) ZoneHVACSizing( zSIndex ).RequestAutoSize = true;
						if ( ZoneHVACSizing( zSIndex ).ScaledCoolingCapacity < 0.0 && ZoneHVACSizing( zSIndex ).ScaledCoolingCapacity != AutoSize ) {
							ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
							ShowContinueError( "Illegal " + cNumericFields( iCoolDesignCapacityNumericNum ) + " = " + TrimSigDigits( Numbers( iCoolDesignCapacityNumericNum ), 7 ) );
							ErrorsFound = true;
						}
					} else {
						ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iCoolCAPMAlphaNum ) + " = " + Alphas( iCoolCAPMAlphaNum ) );
						ShowContinueError( "Blank field not allowed for " + cNumericFields( iCoolDesignCapacityNumericNum ) );
						ErrorsFound = true;
					}
				} else if ( SameString( Alphas( iCoolCAPMAlphaNum ), "CapacityPerFloorArea" ) ) {
					ZoneHVACSizing( zSIndex ).CoolingCapMethod = CapacityPerFloorArea;
					if ( !lNumericBlanks( iCoolCapacityPerFloorAreaNumericNum ) ) {
						ZoneHVACSizing( zSIndex ).ScaledCoolingCapacity = Numbers( iCoolCapacityPerFloorAreaNumericNum );
						if ( ZoneHVACSizing( zSIndex ).ScaledCoolingCapacity <= 0.0 ) {
							ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
							ShowContinueError( "Input for " + cAlphaFields( iCoolCAPMAlphaNum ) + " = " + Alphas( iCoolCAPMAlphaNum ) );
							ShowContinueError( "Illegal " + cNumericFields( iCoolCapacityPerFloorAreaNumericNum ) + " = " + TrimSigDigits( Numbers( iCoolCapacityPerFloorAreaNumericNum ), 7 ) );
							ErrorsFound = true;
							// Autosized input is not allowed
						} else if ( ZoneHVACSizing( zSIndex ).ScaledCoolingCapacity == AutoSize ) {
							ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
							ShowContinueError( "Input for " + cAlphaFields( iCoolCAPMAlphaNum ) + " = " + Alphas( iCoolCAPMAlphaNum ) );
							ShowContinueError( "Illegal " + cNumericFields( iCoolCapacityPerFloorAreaNumericNum ) + " = Autosize" );
							ErrorsFound = true;
						}
					} else {
						ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iCoolCAPMAlphaNum ) + " = " + Alphas( iCoolCAPMAlphaNum ) );
						ShowContinueError( "Blank field not allowed for " + cNumericFields( iCoolCapacityPerFloorAreaNumericNum ) );
						ErrorsFound = true;
					}
				} else if ( SameString( Alphas( iCoolCAPMAlphaNum ), "FractionOfAutosizedCoolingCapacity" ) ) {
					ZoneHVACSizing( zSIndex ).CoolingCapMethod = FractionOfAutosizedCoolingCapacity;
					if ( !lNumericBlanks( iCoolFracOfAutosizedCapacityNumericNum ) ) {
						ZoneHVACSizing( zSIndex ).ScaledCoolingCapacity = Numbers( iCoolFracOfAutosizedCapacityNumericNum );
						if ( ZoneHVACSizing( zSIndex ).ScaledCoolingCapacity == AutoSize ) ZoneHVACSizing( zSIndex ).RequestAutoSize = true;
						if ( ZoneHVACSizing( zSIndex ).ScaledCoolingCapacity < 0.0 && ZoneHVACSizing( zSIndex ).ScaledCoolingCapacity != AutoSize ) {
							ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
							ShowContinueError( "Illegal " + cNumericFields( iCoolFracOfAutosizedCapacityNumericNum ) + " = " + TrimSigDigits( Numbers( iCoolFracOfAutosizedCapacityNumericNum ), 7 ) );
							ErrorsFound = true;
						}
					} else {
						ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iCoolCAPMAlphaNum ) + " = " + Alphas( iCoolCAPMAlphaNum ) );
						ShowContinueError( "Blank field not allowed for " + cNumericFields( iCoolFracOfAutosizedCapacityNumericNum ) );
						ErrorsFound = true;
					}
				} else if ( SameString( Alphas( iCoolCAPMAlphaNum ), "None" ) || lAlphaBlanks( iCoolCAPMAlphaNum ) ) {
					ZoneHVACSizing( zSIndex ).CoolingCapMethod = None;
				} else {
					ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
					ShowContinueError( "Illegal " + cAlphaFields( iCoolCAPMAlphaNum ) + " = " + Alphas( iCoolCAPMAlphaNum ) );
					ErrorsFound = true;
				}

				// Determine heating design capacity of zone HVAC equipment
				if ( SameString( Alphas( iHeatCAPMAlphaNum ), "HeatingDesignCapacity" ) ) {
					ZoneHVACSizing( zSIndex ).HeatingCapMethod = HeatingDesignCapacity;
					if ( !lNumericBlanks( iHeatDesignCapacityNumericNum ) ) {
						ZoneHVACSizing( zSIndex ).ScaledHeatingCapacity = Numbers( iHeatDesignCapacityNumericNum );
						if ( ZoneHVACSizing( zSIndex ).ScaledHeatingCapacity == AutoSize ) ZoneHVACSizing( zSIndex ).RequestAutoSize = true;
						if ( ZoneHVACSizing( zSIndex ).ScaledHeatingCapacity < 0.0 && ZoneHVACSizing( zSIndex ).ScaledHeatingCapacity != AutoSize ) {
							ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
							ShowContinueError( "Illegal " + cNumericFields( iHeatDesignCapacityNumericNum ) + " = " + TrimSigDigits( Numbers( iHeatDesignCapacityNumericNum ), 7 ) );
							ErrorsFound = true;
						}
					} else {
						ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iHeatCAPMAlphaNum ) + " = " + Alphas( iHeatCAPMAlphaNum ) );
						ShowContinueError( "Blank field not allowed for " + cNumericFields( iHeatDesignCapacityNumericNum ) );
						ErrorsFound = true;
					}
				} else if ( SameString( Alphas( iHeatCAPMAlphaNum ), "CapacityPerFloorArea" ) ) {
					ZoneHVACSizing( zSIndex ).HeatingCapMethod = CapacityPerFloorArea;
					if ( !lNumericBlanks( iHeatCapacityPerFloorAreaNumericNum ) ) {
						ZoneHVACSizing( zSIndex ).ScaledHeatingCapacity = Numbers( iHeatCapacityPerFloorAreaNumericNum );
						if ( ZoneHVACSizing( zSIndex ).ScaledHeatingCapacity <= 0.0 ) {
							ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
							ShowContinueError( "Input for " + cAlphaFields( iHeatCAPMAlphaNum ) + " = " + Alphas( iHeatCAPMAlphaNum ) );
							ShowContinueError( "Illegal " + cNumericFields( iHeatCapacityPerFloorAreaNumericNum ) + " = " + TrimSigDigits( Numbers( iHeatCapacityPerFloorAreaNumericNum ), 7 ) );
							ErrorsFound = true;
							// Autosized input is not allowed
						} else if ( ZoneHVACSizing( zSIndex ).ScaledHeatingCapacity == AutoSize ) {
							ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
							ShowContinueError( "Input for " + cAlphaFields( iHeatCAPMAlphaNum ) + " = " + Alphas( iHeatCAPMAlphaNum ) );
							ShowContinueError( "Illegal " + cNumericFields( iHeatCapacityPerFloorAreaNumericNum ) + " = Autosize" );
							ErrorsFound = true;
						}
					} else {
						ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iHeatCAPMAlphaNum ) + " = " + Alphas( iHeatCAPMAlphaNum ) );
						ShowContinueError( "Blank field not allowed for " + cNumericFields( iHeatCapacityPerFloorAreaNumericNum ) );
						ErrorsFound = true;
					}
				} else if ( SameString( Alphas( iHeatCAPMAlphaNum ), "FractionOfAutosizedHeatingCapacity" ) ) {
					ZoneHVACSizing( zSIndex ).HeatingCapMethod = FractionOfAutosizedHeatingCapacity;
					if ( !lNumericBlanks( iHeatFracOfAutosizedCapacityNumericNum ) ) {
						ZoneHVACSizing( zSIndex ).ScaledHeatingCapacity = Numbers( iHeatFracOfAutosizedCapacityNumericNum );
						if ( ZoneHVACSizing( zSIndex ).ScaledHeatingCapacity == AutoSize ) ZoneHVACSizing( zSIndex ).RequestAutoSize = true;
						if ( ZoneHVACSizing( zSIndex ).ScaledHeatingCapacity < 0.0 && ZoneHVACSizing( zSIndex ).ScaledCoolingCapacity != AutoSize ) {
							ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
							ShowContinueError( "Illegal " + cNumericFields( iHeatFracOfAutosizedCapacityNumericNum ) + " = " + TrimSigDigits( Numbers( iHeatFracOfAutosizedCapacityNumericNum ), 7 ) );
							ErrorsFound = true;
						}
					} else {
						ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iHeatCAPMAlphaNum ) + " = " + Alphas( iHeatCAPMAlphaNum ) );
						ShowContinueError( "Blank field not allowed for " + cNumericFields( iHeatFracOfAutosizedCapacityNumericNum ) );
						ErrorsFound = true;
					}
				} else if ( SameString( Alphas( iHeatCAPMAlphaNum ), "None" ) || lAlphaBlanks( iHeatCAPMAlphaNum ) ) {
					ZoneHVACSizing( zSIndex ).HeatingCapMethod = None;
				} else {
					ShowSevereError( CurrentModuleObject + " = " + ZoneHVACSizing( zSIndex ).Name );
					ShowContinueError( "Illegal " + cAlphaFields( iHeatCAPMAlphaNum ) + " = " + Alphas( iHeatCAPMAlphaNum ) );
					ErrorsFound = true;
				}

			}
		}

		Alphas.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		Numbers.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in input.  Preceding condition(s) cause termination." );
		}

	}

} // SizingManager

} // EnergyPlus
