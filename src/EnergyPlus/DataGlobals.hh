#ifndef DataGlobals_hh_INCLUDED
#define DataGlobals_hh_INCLUDED

// C++ Headers
#include <iosfwd>
#include <string>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace DataGlobals {

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	extern int const BeginDay;
	extern int const DuringDay;
	extern int const EndDay;
	extern int const EndZoneSizingCalc;
	extern int const EndSysSizingCalc;

	// Parameters for KindOfSim
	extern int const ksDesignDay;
	extern int const ksRunPeriodDesign;
	extern int const ksRunPeriodWeather;

	extern int const ZoneTSReporting; // value for Zone Time Step Reporting (UpdateDataAndReport)
	extern int const HVACTSReporting; // value for HVAC Time Step Reporting (UpdateDataAndReport)

	extern Real64 const MaxEXPArg; // maximum exponent in EXP() function
	extern Real64 const Pi; // Pi 3.1415926535897932384626435
	extern Real64 const PiOvr2; // Pi/2
	extern Real64 const TwoPi; // 2*Pi 6.2831853071795864769252868
	extern Real64 const GravityConstant;
	extern Real64 const DegToRadians; // Conversion for Degrees to Radians
	extern Real64 const DegToRad; // Conversion for Degrees to Radians
	extern Real64 const RadToDeg; // Conversion for Radians to Degrees
	extern Real64 const SecInHour; // Conversion for hours to seconds
	extern Real64 const HoursInDay; // Number of Hourse in Day
	extern Real64 const SecsInDay; // Number of seconds in Day
	extern Real64 const BigNumber; // Max Number real used for initializations
	extern Real64 const rTinyValue; // Tiny value to replace use of TINY(x)
	extern std::string::size_type const MaxNameLength; // Maximum Name Length in Characters -- should be the same
	// as MaxAlphaArgLength in InputProcessor module

	extern Real64 const KelvinConv; // Conversion factor for C to K and K to C
	extern Real64 const InitConvTemp; // [deg C], standard init vol to mass flow conversion temp
	extern Real64 const AutoCalculate; // automatically calculate some fields.

	extern Real64 const StefanBoltzmann; // Stefan-Boltzmann constant in W/(m2*K4)
	extern Real64 const UniversalGasConst; // (J/mol*K)

	// Parameters for EMS Calling Points
	extern int const emsCallFromZoneSizing; // Identity where EMS called from
	extern int const emsCallFromSystemSizing; // Identity where EMS called from
	extern int const emsCallFromBeginNewEvironment; // Identity where EMS called from
	extern int const emsCallFromBeginNewEvironmentAfterWarmUp; // Identity where EMS called from
	extern int const emsCallFromBeginTimestepBeforePredictor; // Identity where EMS called from
	extern int const emsCallFromBeforeHVACManagers; // Identity where EMS called from
	extern int const emsCallFromAfterHVACManagers; // Identity where EMS called from
	extern int const emsCallFromHVACIterationLoop; // Identity where EMS called from
	extern int const emsCallFromEndSystemTimestepBeforeHVACReporting; // Identity where EMS called from
	extern int const emsCallFromEndSystemTimestepAfterHVACReporting; // Identity where EMS called from
	extern int const emsCallFromEndZoneTimestepBeforeZoneReporting; // Identity where EMS called from
	extern int const emsCallFromEndZoneTimestepAfterZoneReporting; // Identity where EMS called from
	extern int const emsCallFromSetupSimulation; // identify where EMS called from,
	// this is for input processing only
	extern int const emsCallFromExternalInterface; // Identity where EMS called from
	extern int const emsCallFromComponentGetInput; // EMS called from end of get input for a component
	extern int const emsCallFromUserDefinedComponentModel; // EMS called from inside a custom user component model
	extern int const emsCallFromUnitarySystemSizing; // EMS called from unitary system compound component

	extern int const ScheduleAlwaysOn; // Value when passed to schedule routines gives back 1.0 (on)

	// DERIVED TYPE DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS:
	// see DataOmterfaces fpr global interface statements

	// MODULE VARIABLE DECLARATIONS:

	extern bool BeginDayFlag; // True at the start of each day, False after first time step in day
	extern bool BeginEnvrnFlag; // True at the start of each environment, False after first time step in environ
	extern bool BeginHourFlag; // True at the start of each hour, False after first time step in hour
	extern bool BeginSimFlag; // True until any actual simulation (full or sizing) has begun, False after first time step
	extern bool BeginFullSimFlag; // True until full simulation has begun, False after first time step
	extern bool BeginTimeStepFlag; // True at the start of each time step, False after first subtime step of time step
	extern int DayOfSim; // Counter for days (during the simulation)
	extern std::string DayOfSimChr; // Counter for days (during the simulation) (character -- for reporting)
	extern bool EndEnvrnFlag; // True at the end of each environment (last time step of last hour of last day of environ)
	extern bool EndDesignDayEnvrnsFlag; // True at the end of the last design day environment
	// (last time step of last hour of last day of environ which is a design day)
	extern bool EndDayFlag; // True at the end of each day (last time step of last hour of day)
	extern bool EndHourFlag; // True at the end of each hour (last time step of hour)
	extern int PreviousHour; // Previous Hour Index
	extern int HourOfDay; // Counter for hours in a simulation day
	extern Real64 WeightPreviousHour; // Weighting of value for previous hour
	extern Real64 WeightNow; // Weighting of value for current hour
	extern int NumOfDayInEnvrn; // Number of days in the simulation for a particular environment
	extern int NumOfTimeStepInHour; // Number of time steps in each hour of the simulation
	extern int NumOfZones; // Total number of Zones for simulation
	extern int TimeStep; // Counter for time steps (fractional hours)
	extern Real64 TimeStepZone; // Zone time step in fractional hours
	extern bool WarmupFlag; // True during the warmup portion of a simulation
	extern int OutputFileStandard; // Unit number for the standard output file (hourly data only)
	extern std::ostream * eso_stream; // Internal stream used for eso output (used for performance)
	extern int StdOutputRecordCount; // Count of Standard output records
	extern int OutputFileInits; // Unit number for the standard Initialization output file
	extern int OutputFileDebug; // Unit number for debug outputs
	extern int OutputFileZoneSizing; // Unit number of zone sizing calc output file
	extern int OutputFileSysSizing; // Unit number of system sizing calc output file
	extern int OutputFileMeters; // Unit number for meters output
	extern std::ostream * mtr_stream; // Internal stream used for mtr output (used for performance)
	extern int StdMeterRecordCount; // Count of Meter output records
	extern int OutputFileBNDetails; // Unit number for Branch-Node Details
	extern bool ZoneSizingCalc; // TRUE if zone sizing calculation
	extern bool SysSizingCalc; // TRUE if system sizing calculation
	extern bool DoZoneSizing; // User input in SimulationControl object
	extern bool DoSystemSizing; // User input in SimulationControl object
	extern bool DoPlantSizing; // User input in SimulationControl object
	extern bool DoDesDaySim; // User input in SimulationControl object
	extern bool DoWeathSim; // User input in SimulationControl object
	extern bool WeathSimReq; // Input has a RunPeriod request
	extern int KindOfSim; // See parameters. (ksDesignDay, ksRunPeriodDesign, ksRunPeriodWeather)
	extern bool DoOutputReporting; // TRUE if variables to be written out
	extern bool DoingSizing; // TRUE when "sizing" is being performed (some error messages won't be displayed)
	extern bool DoingInputProcessing; // TRUE when "IP" is being performed (some error messages are cached)
	extern bool DisplayAllWarnings; // True when selection for  "DisplayAllWarnings" is entered (turns on other warning flags)
	extern bool DisplayExtraWarnings; // True when selection for  "DisplayExtraWarnings" is entered
	extern bool DisplayUnusedObjects; // True when selection for  "DisplayUnusedObjects" is entered
	extern bool DisplayUnusedSchedules; // True when selection for  "DisplayUnusedSchedules" is entered
	extern bool DisplayAdvancedReportVariables; // True when selection for  "DisplayAdvancedReportVariables" is entered
	extern bool DisplayZoneAirHeatBalanceOffBalance; // True when selection for  "DisplayZoneAirHeatBalanceOffBalance" is entered
	extern bool CreateMinimalSurfaceVariables; // True when selection for  "CreateMinimalSurfaceVariables" is entered
	extern Real64 CurrentTime; // CurrentTime, in fractional hours, from start of day. Uses Loads time step.
	extern int SimTimeSteps; // Number of (Loads) timesteps since beginning of run period (environment).
	extern int MinutesPerTimeStep; // Minutes per time step calculated from NumTimeStepInHour (number of minutes per load time step)
	extern bool MetersHaveBeenInitialized;
	extern bool KickOffSimulation; // Kick off simulation -- meaning run each environment for 1 or 2 time steps.
	extern bool KickOffSizing; // Kick off sizing -- meaning run each environment for 1 or 2 time steps.
	extern bool AnyEnergyManagementSystemInModel; // true if there is any EMS or Erl in model.  otherwise false
	extern bool AnyPlantInModel; // true if there are any plant or condenser loops in model, otherwise false
	extern int CacheIPErrorFile; // Cache IP errors until IDF processing done.
	extern bool AnyIdealCondEntSetPointInModel; // true if there is any ideal condenser entering set point manager in model.
	extern bool RunOptCondEntTemp; // true if the ideal condenser entering set point optimization is running
	extern bool CompLoadReportIsReq; // true if the extra sizing calcs are performed to create a "pulse" for the load component report
	extern bool isPulseZoneSizing; // true during the set of zone sizing calcs that include the "pulse" for the load component report
	extern int OutputFileZonePulse; // file handle for special zone sizing report that contains the result of the "pulse" for the load component report
	extern bool doLoadComponentPulseNow; // true for the time step that is the "pulse" for the load component report
	extern bool ShowDecayCurvesInEIO; // true if the Radiant to Convective Decay Curves should appear in the EIO file
	extern bool AnySlabsInModel; // true if there are any zone-coupled ground domains in the input file
	extern bool AnyBasementsInModel; // true if there are any basements in the input file

	extern int Progress; 
	extern void ( *fProgressPtr )( int );
	extern void ( *fMessagePtr )( std::string );

} // DataGlobals

} // EnergyPlus

#endif
