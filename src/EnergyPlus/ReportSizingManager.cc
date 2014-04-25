// C++ Headers
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <ReportSizingManager.hh>
#include <CurveManager.hh>
#include <DataAirLoop.hh>
#include <DataAirSystems.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataHVACGlobals.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <InputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <Psychrometrics.hh>
#include <SQLiteProcedures.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace ReportSizingManager {

	// Module containing the routines dealing with the <module_name>

	// MODULE INFORMATION:
	//       AUTHOR         Linda Lawrie<author>
	//       DATE WRITTEN   November 2010
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// Provide module interface for ReportSizingOutput

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// USE STATEMENTS:
	// na

	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	// na

	// SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>:

	// Functions

	void
	ReportSizingOutput(
		std::string const & CompType, // the type of the component
		std::string const & CompName, // the name of the component
		std::string const & VarDesc, // the description of the input variable
		Real64 const VarValue, // the value from the sizing calculation
		Optional_string_const UsrDesc, // the description of a user-specified variable
		Optional< Real64 const > UsrValue // the value from the user for the desc item
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   Decenber 2001
		//       MODIFIED       August 2008, Greg Stark
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine writes one item of sizing data to the "eio" file..

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataPrecisionGlobals;
		using DataGlobals::OutputFileInits;
		using namespace OutputReportPredefined;
		using General::RoundSigDigits;
		using namespace SQLiteProcedures;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyOneTimeFlag( true );

		// Formats
		static gio::Fmt const Format_990( "('! <Component Sizing Information>, Component Type, Component Name, ','Input Field Description, Value')" );
		static gio::Fmt const Format_991( "(' Component Sizing Information, ',A,', ',A,', ',A,', ',A)" );

		if ( MyOneTimeFlag ) {
			gio::write( OutputFileInits, Format_990 );
			MyOneTimeFlag = false;
		}

		gio::write( OutputFileInits, Format_991 ) << CompType << CompName << VarDesc << RoundSigDigits( VarValue, 5 );
		//add to tabular output reports
		AddCompSizeTableEntry( CompType, CompName, VarDesc, VarValue );

		if ( present( UsrDesc ) && present( UsrValue ) ) {
			gio::write( OutputFileInits, Format_991 ) << CompType << CompName << UsrDesc << RoundSigDigits( UsrValue, 5 );
			AddCompSizeTableEntry( CompType, CompName, UsrDesc, UsrValue );
		} else if ( present( UsrDesc ) || present( UsrValue ) ) {
			ShowFatalError( "ReportSizingOutput: (Developer Error) - called with user-specified description or value but not both." );
		}

		// add to SQL output
		if ( WriteOutputToSQLite ) AddSQLiteComponentSizingRecord( CompType, CompName, VarDesc, VarValue );

	}

	void
	RequestSizing(
		std::string const & CompType, // type of component
		std::string const & CompName, // name of component
		int const SizingType, // integerized type of sizing requested (see DataHVACGlobals, e.g. CoolingCapacitySizing)
		std::string const & SizingString, // string containing info for eio report
		Real64 & SizingResult, // result of the sizing procedure
		bool const PrintWarningFlag, // TRUE when requesting output (eio) reporting
		std::string const & CallingRoutine // name of calling rotuine for warning messages
	)
	{
		// SUBROUTINE INFORMATION :
		// AUTHOR         Richard Raustad, FSEC
		// DATE WRITTEN   October 2013
		// MODIFIED       na
		// RE - ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE :
		// This function returns the load sizing result.

		// METHODOLOGY EMPLOYED :
		// Same as currently used.

		// REFERENCES :
		// na

		// USE STATEMENTS :
		using CurveManager::CurveValue;
		using DataEnvironment::StdRhoAir;
		using DataEnvironment::StdBaroPress;
		using DataAirLoop::AirLoopControlInfo;
		using DataAirSystems::PrimaryAirSystem;
		using DataGlobals::DisplayExtraWarnings;
		using namespace DataSizing; // Data added in zone eq component sizing routines
		using namespace DataHVACGlobals;
		using General::RoundSigDigits;
		using General::TrimSigDigits;
		using InputProcessor::SameString;
		using Psychrometrics::PsyCpAirFnWTdb;
		using Psychrometrics::PsyHFnTdbW;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyTwbFnTdbWPb;

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "RequestSizing: " ); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool IsAutoSize; // Indicator to autosize for reporting
		bool HardSizeNoDesRun; // Indicator to hardsize with no sizing runs for reporting
		Real64 AutosizeDes; // autosized value
		Real64 AutosizeUser; // user sized value
		Real64 OutTemp; // outdoor air temperature [C]
		Real64 CoilInTemp; // entering coil air temperature [C]
		Real64 CoilInHumRat; // entering coil air humidity ratio [kg/kg]
		Real64 CoilInWetBulb; // entering coil air wet-bulb temperature [C]
		Real64 TotCapTempModFac; // DX coil total capacity as a function of temperature curve pionter
		Real64 CoilInEnth; // entering coil air enthalpy [J/kg]
		Real64 CoilOutTemp; // coil outlet air temperature [C]
		Real64 CoilOutHumRat; // coil outlet air humidity ratio [kg/kg]
		Real64 CoilOutEnth; // coil outlet air enthalpy [J/kg]
		Real64 DesCoilLoad; // design coil load based on sizing inputs and entering air conditions [W]
		Real64 PeakCoilLoad; // adjusted coil size based on TotCapTempModFac [W]
		Real64 DesVolFlow; // coil design air volume flow rate [m3/s]
		Real64 DesMassFlow; // coil design air mass flow rate [kg/s]
		Real64 CpAir; // specific heat of air [J/kg-K]
		Real64 rhoair; // density of air [kg/m3]
		Real64 OutAirFrac; // outdoor air fraction
		Real64 MinPriFlowFrac; // minimum primary air flow fraction for induction units
		Real64 CpAirStd; // specific heat of air at standard conditions [J/kg-K]
		Real64 NominalCapacityDes; // Autosized nominal capacity for reporting [W]
		bool SizingDesRunThisAirSys; // true if a particular air system had a Sizing : System object and system sizing done
		bool SizingDesRunThisZone; // true if a particular zone had a Sizing : Zone object and zone sizing was done
		int TimeStepNumAtMax; // time step number at max load
		int DDNum; // design day number corresponding to TimeStepNumAtMax
		bool OASysFlag; // Logical flag determines if parent object set OA Sys coil property
		bool AirLoopSysFlag; // Logical flag determines if parent object set air loop coil property
		Real64 RatedVolFlowPerRatedTotCap; // ratio of volume flow rate to capacity [m3/W]

		AutosizeDes = 0.0;
		AutosizeUser = 0.0;
		IsAutoSize = false;
		OASysFlag = false;
		AirLoopSysFlag = false;

		if ( SysSizingRunDone || ZoneSizingRunDone ) {
			HardSizeNoDesRun = false;
		} else {
			HardSizeNoDesRun = true;
		}

		if ( CurSysNum > 0 ) {
			CheckThisAirSystemForSizing( CurSysNum, SizingDesRunThisAirSys );
			AirLoopSysFlag = UnitarySysEqSizing( CurSysNum ).Capacity; // *** change to CoolingCapacity and HeatingCapacity
			if ( CurOASysNum > 0 ) OASysFlag = OASysEqSizing( CurOASysNum ).Capacity; // logicals used when parent sizes coil *** change to CoolingCapacity and HeatingCapacity
		} else {
			SizingDesRunThisAirSys = false;
		}

		if ( CurZoneEqNum > 0 ) {
			CheckThisZoneForSizing( CurZoneEqNum, SizingDesRunThisZone );
		} else {
			SizingDesRunThisZone = false;
		}

		if ( SizingResult == AutoSize ) {
			IsAutoSize = true;
		}

		if ( CurZoneEqNum > 0 ) {
			if ( !IsAutoSize && !SizingDesRunThisZone ) {
				HardSizeNoDesRun = true;
				AutosizeUser = SizingResult;
				if ( PrintWarningFlag && SizingResult > 0.0 ) {
					ReportSizingOutput( CompType, CompName, "User-Specified " + SizingString, SizingResult );
				}
			} else { // TypeOfZoneSizing
//				if ( SizingType == ( CoolingAirflowSizing || HeatingAirflowSizing || SystemAirflowSizing ) ) { // Replaced by below: Pretty clear this is a bug
				if ( ( SizingType == CoolingAirflowSizing ) || ( SizingType == HeatingAirflowSizing ) || ( SizingType ==  SystemAirflowSizing ) ) {
				} else if ( SizingType == CoolingSHRSizing ) {
				} else if ( SizingType == CoolingCapacitySizing ) {
				} else if ( SizingType == HeatingCapacitySizing ) {
					if ( !IsAutoSize && !SizingDesRunThisZone ) {
						NominalCapacityDes = SizingResult;
					} else { //Autosize or hardsize with sizing data
						if ( ZoneEqSizing( CurZoneEqNum ).Capacity ) {   // *** change to HeatingCapacity
							NominalCapacityDes = ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad;
						} else if ( DataCoolCoilCap > 0.0 && DataFlowUsedForSizing > 0.0 ) {
							NominalCapacityDes = DataCoolCoilCap;
							DesVolFlow = DataFlowUsedForSizing;
						} else if ( FinalZoneSizing( CurZoneEqNum ).DesHeatMassFlow >= SmallMassFlow ) {
							if ( TermUnitPIU ) {
								MinPriFlowFrac = TermUnitSizing( CurZoneEqNum ).MinFlowFrac;
								if ( TermUnitSizing( CurZoneEqNum ).InducesPlenumAir ) {
									CoilInTemp = ( FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTempTU * MinPriFlowFrac ) + ( FinalZoneSizing( CurZoneEqNum ).ZoneRetTempAtHeatPeak * ( 1.0 - MinPriFlowFrac ) );
								} else {
									CoilInTemp = ( FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTempTU * MinPriFlowFrac ) + ( FinalZoneSizing( CurZoneEqNum ).ZoneTempAtHeatPeak * ( 1.0 - MinPriFlowFrac ) );
								}
							} else if ( TermUnitIU ) {
								CoilInTemp = FinalZoneSizing( CurZoneEqNum ).ZoneTempAtHeatPeak;
							} else if ( TermUnitSingDuct ) {
								CoilInTemp = FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTempTU;
							} else {
								CoilInTemp = FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTemp;
							}
							if ( TermUnitSingDuct || TermUnitPIU ) {
								CoilOutTemp = FinalZoneSizing( CurZoneEqNum ).HeatDesTemp;
								CoilOutHumRat = FinalZoneSizing( CurZoneEqNum ).HeatDesHumRat;
								CpAir = PsyCpAirFnWTdb( CoilOutHumRat, 0.5 * ( CoilInTemp + CoilOutTemp ) );
								DesCoilLoad = CpAir * StdRhoAir * TermUnitSizing( CurZoneEqNum ).AirVolFlow * ( CoilOutTemp - CoilInTemp );
							} else if ( TermUnitIU ) {
								if ( TermUnitSizing( CurZoneEqNum ).InducRat > 0.01 ) {
									DesVolFlow = TermUnitSizing( CurZoneEqNum ).AirVolFlow / TermUnitSizing( CurZoneEqNum ).InducRat;
									CpAir = PsyCpAirFnWTdb( FinalZoneSizing( CurZoneEqNum ).HeatDesHumRat, FinalZoneSizing( CurZoneEqNum ).HeatDesTemp );
									// the design heating coil load is the zone load minus whatever the central system does.Note that
									// DesHeatCoilInTempTU is really the primary air inlet temperature for the unit.
									DesCoilLoad = FinalZoneSizing( CurZoneEqNum ).DesHeatLoad - ( CpAir * StdRhoAir * DesVolFlow * ( FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTempTU - FinalZoneSizing( CurZoneEqNum ).ZoneTempAtHeatPeak ) );
								} else {
									DesCoilLoad = 0.0;
								}
							} else {
								CoilOutTemp = FinalZoneSizing( CurZoneEqNum ).HeatDesTemp;
								CoilOutHumRat = FinalZoneSizing( CurZoneEqNum ).HeatDesHumRat;
								CpAir = PsyCpAirFnWTdb( CoilOutHumRat, 0.5 * ( CoilInTemp + CoilOutTemp ) );
								DesCoilLoad = CpAir * FinalZoneSizing( CurZoneEqNum ).DesHeatMassFlow * ( CoilOutTemp - CoilInTemp );
							}
							NominalCapacityDes = max( 0.0, DesCoilLoad );
						} else {
							NominalCapacityDes = 0.0;
						}
					}
					AutosizeDes = NominalCapacityDes * DataHeatSizeRatio;
				} else { // from else if ( SizingType == HeatingCapacitySizing )
				}
			}
		} else {
			if ( !IsAutoSize && !SizingDesRunThisAirSys ) {
				HardSizeNoDesRun = true;
				AutosizeUser = SizingResult;
				if ( PrintWarningFlag && SizingResult > 0.0 ) {
					ReportSizingOutput( CompType, CompName, "User-Specified " + SizingString, SizingResult );
				}
			} else { // TypeOfAirLoopSysSizing
				if ( SizingType == CoolingAirflowSizing ) {
				} else if ( SizingType == HeatingAirflowSizing ) {
					if ( UnitarySysEqSizing( CurSysNum ).AirFlow ) {
						AutosizeDes = UnitarySysEqSizing( CurSysNum ).AirVolFlow;
					} else {
						AutosizeDes = FinalSysSizing( CurSysNum ).DesHeatVolFlow;
					}
				} else if ( SizingType == SystemAirflowSizing ) {
				} else if ( SizingType == CoolingSHRSizing ) {
				} else if ( SizingType == CoolingCapacitySizing ) {
				} else if ( SizingType == HeatingCapacitySizing ) {
					if ( CurOASysNum > 0 ) {
						if ( OASysEqSizing( CurOASysNum ).AirFlow ) {
							DesVolFlow = OASysEqSizing( CurOASysNum ).AirVolFlow;
						} else {
							DesVolFlow = FinalSysSizing( CurSysNum ).DesOutAirVolFlow;
						}
					} else {
						if ( DataFlowUsedForSizing > 0.0 ) {
							DesVolFlow = DataFlowUsedForSizing;
						} else if ( UnitarySysEqSizing( CurSysNum ).AirFlow ) {
							DesVolFlow = UnitarySysEqSizing( CurSysNum ).AirVolFlow;
						} else { // TypeOfAirLoopDuct
							if ( CurDuctType == Main ) {
								DesVolFlow = FinalSysSizing( CurSysNum ).SysAirMinFlowRat * FinalSysSizing( CurSysNum ).DesMainVolFlow;
							} else if ( CurDuctType == Cooling ) {
								DesVolFlow = FinalSysSizing( CurSysNum ).SysAirMinFlowRat * FinalSysSizing( CurSysNum ).DesCoolVolFlow;
							} else if ( CurDuctType == Heating ) {
								DesVolFlow = FinalSysSizing( CurSysNum ).DesHeatVolFlow;
							} else if ( CurDuctType == Other ) {
								DesVolFlow = FinalSysSizing( CurSysNum ).DesMainVolFlow;
							} else {
								DesVolFlow = FinalSysSizing( CurSysNum ).DesMainVolFlow;
							}
						}
					}
					DesMassFlow = StdRhoAir * DesVolFlow;
					// get the outside air fraction
					if ( CurOASysNum > 0 ) {
						OutAirFrac = 1.0;
					} else if ( FinalSysSizing( CurSysNum ).HeatOAOption == MinOA ) {
						if ( DesVolFlow > 0.0 ) {
							OutAirFrac = FinalSysSizing( CurSysNum ).DesOutAirVolFlow / DesVolFlow;
						} else {
							OutAirFrac = 1.0;
						}
						OutAirFrac = std::min( 1.0, std::max( 0.0, OutAirFrac ) );
					} else {
						OutAirFrac = 1.0;
					}
					// coil inlet temperature
					if ( CurOASysNum == 0 && PrimaryAirSystem( CurSysNum ).NumOAHeatCoils > 0 ) {
						CoilInTemp = OutAirFrac * FinalSysSizing( CurSysNum ).PreheatTemp + ( 1.0 - OutAirFrac ) * FinalSysSizing( CurSysNum ).HeatRetTemp;
					} else {
						CoilInTemp = OutAirFrac * FinalSysSizing( CurSysNum ).HeatOutTemp + ( 1.0 - OutAirFrac ) * FinalSysSizing( CurSysNum ).HeatRetTemp;
					}
					CpAirStd = PsyCpAirFnWTdb( 0.0, 20.0 );
					// coil load
					if ( CurOASysNum > 0 ) {
						if ( OASysEqSizing( CurOASysNum ).Capacity ) {
							DesCoilLoad = OASysEqSizing( CurOASysNum ).DesHeatingLoad;
						} else {
							DesCoilLoad = CpAirStd * DesMassFlow * ( FinalSysSizing( CurSysNum ).PreheatTemp - CoilInTemp );
						}
					} else {
						if ( UnitarySysEqSizing( CurSysNum ).Capacity ) {
							DesCoilLoad = UnitarySysEqSizing( CurSysNum ).DesHeatingLoad;
						} else {
							DesCoilLoad = CpAirStd * DesMassFlow * ( FinalSysSizing( CurSysNum ).HeatSupTemp - CoilInTemp );
						}
					}
					if ( AirLoopControlInfo( CurSysNum ).UnitarySys ) {
						if ( DataCoilIsSuppHeater ) {
							NominalCapacityDes = SuppHeatCap;
						} else {
							// TRUE for all air loop parent equipment except UnitarySystem where flag is reset to FALSE after simulating
							// This method allows downstream heating coils to size individually.Probably should do this for all air loop equipment
							// ChangoverBypass model always sets AirLoopControlInfo%UnitarySys to FALSE so heating coil can individually size
							if ( AirLoopControlInfo( CurSysNum ).UnitarySysSimulating ) {
								NominalCapacityDes = UnitaryHeatCap;
							} else {
								if ( DesCoilLoad >= SmallLoad ) {
									NominalCapacityDes = DesCoilLoad;
								} else {
									NominalCapacityDes = 0.0;
								}
							}
						}
					} else {
						if ( DesCoilLoad >= SmallLoad ) {
							NominalCapacityDes = DesCoilLoad;
						} else {
							NominalCapacityDes = 0.0;
						}
					}
				}
				AutosizeDes = NominalCapacityDes * DataHeatSizeRatio;
			}
		}

		if ( IsAutoSize || SizingDesRunThisAirSys || SizingDesRunThisZone ) {
			if ( AutosizeDes < SmallAirVolFlow ) {
				AutosizeDes = 0.0;
			}
		}

		if ( SizingResult == AutoSize ) {
			SizingResult = AutosizeDes;
		} else {
			AutosizeUser = SizingResult;
		}

		// put EMS override here?
		// EMS override code

		if ( PrintWarningFlag ) {
			if ( !HardSizeNoDesRun ) {
				if ( IsAutoSize ) { // Design Size values are available for both autosized and hard - sized
					// check capacity to make sure design volume flow per total capacity is within range
					if ( DataIsDXCoil && ( SizingType == CoolingCapacitySizing || SizingType == HeatingCapacitySizing ) ) {
						RatedVolFlowPerRatedTotCap = DesVolFlow / SizingResult;
						if ( RatedVolFlowPerRatedTotCap < MinRatedVolFlowPerRatedTotCap( DataDXCT ) ) {
							if ( !DataEMSOverride && DisplayExtraWarnings ) {
								ShowWarningError( RoutineName + ':' + CallingRoutine + ' ' + CompType + ' ' + CompName );
								ShowContinueError( "...Rated Total Cooling Capacity will be limited by the minimum rated volume flow per rated total capacity ratio." );
								ShowContinueError( "...DX coil volume flow rate (m3/s) = " + TrimSigDigits( DesVolFlow, 6 ) );
								ShowContinueError( "...Requested capacity (W) = " + TrimSigDigits( SizingResult, 3 ) );
								ShowContinueError( "...Requested flow/capacity ratio (m3/s/W) = " + TrimSigDigits( RatedVolFlowPerRatedTotCap, 3 ) );
								ShowContinueError( "...Minimum flow/capacity ratio (m3/s/W) = " + TrimSigDigits( MinRatedVolFlowPerRatedTotCap( DataDXCT ), 3 ) );
								SizingResult = DesVolFlow / MinRatedVolFlowPerRatedTotCap( DataDXCT );
								if ( !DataEMSOverride && DisplayExtraWarnings ) {
									ShowContinueError( "...Adjusted capacity (W) = " + TrimSigDigits( SizingResult, 3 ) );
								}
							}
						} else if ( RatedVolFlowPerRatedTotCap > MaxRatedVolFlowPerRatedTotCap( DataDXCT ) ) {
							if ( !DataEMSOverride && DisplayExtraWarnings ) {
								ShowWarningError( RoutineName + ':' + CallingRoutine + ' ' + CompType + ' ' + CompName );
								ShowContinueError( "...Rated Total Cooling Capacity will be limited by the maximum rated volume flow per rated total capacity ratio." );
								ShowContinueError( "...DX coil volume flow rate (m3/s) = " + TrimSigDigits( DesVolFlow, 6 ) );
								ShowContinueError( "...Requested capacity (W) = " + TrimSigDigits( SizingResult, 3 ) );
								ShowContinueError( "...Requested flow/capacity ratio (m3/s/W) = " + TrimSigDigits( RatedVolFlowPerRatedTotCap, 3 ) );
								ShowContinueError( "...Maximum flow/capacity ratio (m3/s/W) = " + TrimSigDigits( MaxRatedVolFlowPerRatedTotCap( DataDXCT ), 3 ) );
								SizingResult = DesVolFlow / MaxRatedVolFlowPerRatedTotCap( DataDXCT );
								if ( !DataEMSOverride && DisplayExtraWarnings ) {
									ShowContinueError( "...Adjusted capacity (W) = " + TrimSigDigits( SizingResult, 3 ) );
								}
								AutosizeDes = SizingResult;
							}
						}
					}
					if ( DataAutosizable && AutosizeUser > 0.0 && AutosizeDes > 0.0 ) {
						ReportSizingOutput( CompType, CompName, "Design Size " + SizingString, AutosizeDes, "User-Specified " + SizingString, AutosizeUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( AutosizeDes - AutosizeUser ) / AutosizeUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( RoutineName + ':' + CallingRoutine + ": Potential issue with equipment sizing for " + CompType + ' ' + CompName );
								ShowContinueError( "User-Specified " + SizingString + " = " + RoundSigDigits( AutosizeUser, 5 ) );
								ShowContinueError( "differs from Design Size " + SizingString + " = " + RoundSigDigits( AutosizeDes, 5 ) );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					} else {
						if ( DataAutosizable ) ReportSizingOutput( CompType, CompName, "Design Size " + SizingString, SizingResult );
					}
				} else {
					if ( DataAutosizable && AutosizeUser > 0.0 && AutosizeDes > 0.0 ) {
						ReportSizingOutput( CompType, CompName, "Design Size " + SizingString, AutosizeDes, "User-Specified " + SizingString, AutosizeUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( AutosizeDes - AutosizeUser ) / AutosizeUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( RoutineName + ':' + CallingRoutine + ": Potential issue with equipment sizing for " + CompType + ' ' + CompName );
								ShowContinueError( "User-Specified " + SizingString + " = " + RoundSigDigits( AutosizeUser, 5 ) );
								ShowContinueError( "differs from Design Size " + SizingString + " = " + RoundSigDigits( AutosizeDes, 5 ) );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					} else{
						if ( DataAutosizable ) ReportSizingOutput( CompType, CompName, "User-Specified " + SizingString, AutosizeUser );
					}
				}
//			} else {
//				if ( DataAutosizable ) ReportSizingOutput( CompType, CompName, "Design Size " + SizingString, AutosizeDes );
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

} // ReportSizingManager

} // EnergyPlus
