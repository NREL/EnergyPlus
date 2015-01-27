// C++ Headers
#include <ostream>

// ObjexxFCL Headers
#include <ObjexxFCL/numeric.hh>

// EnergyPlus Headers
#include <DataGlobals.hh>
#include <DataPrecisionGlobals.hh>

#include <string>

namespace EnergyPlus {

namespace DataGlobals {

	// MODULE INFORMATION:
	//       AUTHOR         Rick Strand
	//       DATE WRITTEN   January 1997
	//       MODIFIED       May 1997 (RKS) Added Weather Variables
	//       MODIFIED       December 1997 (RKS,DF,LKL) Split into DataGlobals and DataEnvironment
	//       MODIFIED       February 1999 (FW) Added NextHour, WGTNEXT, WGTNOW
	//       MODIFIED       September 1999 (LKL) Rename WGTNEXT,WGTNOW for clarity
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This data-only module is a repository for all variables which are considered
	// to be "global" in nature in EnergyPlus.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	int const BeginDay( 1 );
	int const DuringDay( 2 );
	int const EndDay( 3 );
	int const EndZoneSizingCalc( 4 );
	int const EndSysSizingCalc( 5 );

	// Parameters for KindOfSim
	int const ksDesignDay( 1 );
	int const ksRunPeriodDesign( 2 );
	int const ksRunPeriodWeather( 3 );

	int const ZoneTSReporting( 1 ); // value for Zone Time Step Reporting (UpdateDataAndReport)
	int const HVACTSReporting( 2 ); // value for HVAC Time Step Reporting (UpdateDataAndReport)

	Real64 const MaxEXPArg( 709.78 ); // maximum exponent in EXP() function
	Real64 const Pi( 3.14159265358979324 ); // Pi 3.1415926535897932384626435
	Real64 const PiOvr2( Pi / 2.0 ); // Pi/2
	Real64 const TwoPi( 2.0 * Pi ); // 2*Pi 6.2831853071795864769252868
	Real64 const GravityConstant( 9.807 );
	Real64 const DegToRadians( Pi / 180.0 ); // Conversion for Degrees to Radians
	Real64 const DegToRad( Pi / 180.0 ); // Conversion for Degrees to Radians
	Real64 const RadToDeg( 180.0 / Pi ); // Conversion for Radians to Degrees
	Real64 const SecInHour( 3600.0 ); // Conversion for hours to seconds
	Real64 const HoursInDay( 24.0 ); // Number of Hourse in Day
	Real64 const SecsInDay( SecInHour * HoursInDay ); // Number of seconds in Day
	Real64 const BigNumber( huge( 1.0 ) ); // Max Number real used for initializations
	Real64 const rTinyValue( epsilon( 1.0 ) ); // Tiny value to replace use of TINY(x)
	std::string::size_type const MaxNameLength( 100 ); // Maximum Name Length in Characters -- should be the same
	// as MaxAlphaArgLength in InputProcessor module

	Real64 const KelvinConv( 273.15 ); // Conversion factor for C to K and K to C
	Real64 const InitConvTemp( 5.05 ); // [deg C], standard init vol to mass flow conversion temp
	Real64 const AutoCalculate( -99999.0 ); // automatically calculate some fields.

	Real64 const StefanBoltzmann( 5.6697E-8 ); // Stefan-Boltzmann constant in W/(m2*K4)
	Real64 const UniversalGasConst( 8314.462175 ); // (J/mol*K)

	// Parameters for EMS Calling Points
	int const emsCallFromZoneSizing( 1 ); // Identity where EMS called from
	int const emsCallFromSystemSizing( 2 ); // Identity where EMS called from
	int const emsCallFromBeginNewEvironment( 3 ); // Identity where EMS called from
	int const emsCallFromBeginNewEvironmentAfterWarmUp( 4 ); // Identity where EMS called from
	int const emsCallFromBeginTimestepBeforePredictor( 5 ); // Identity where EMS called from
	int const emsCallFromBeforeHVACManagers( 6 ); // Identity where EMS called from
	int const emsCallFromAfterHVACManagers( 7 ); // Identity where EMS called from
	int const emsCallFromHVACIterationLoop( 8 ); // Identity where EMS called from
	int const emsCallFromEndSystemTimestepBeforeHVACReporting( 9 ); // Identity where EMS called from
	int const emsCallFromEndSystemTimestepAfterHVACReporting( 10 ); // Identity where EMS called from
	int const emsCallFromEndZoneTimestepBeforeZoneReporting( 11 ); // Identity where EMS called from
	int const emsCallFromEndZoneTimestepAfterZoneReporting( 12 ); // Identity where EMS called from
	int const emsCallFromSetupSimulation( 13 ); // identify where EMS called from,
	// this is for input processing only
	int const emsCallFromExternalInterface( 14 ); // Identity where EMS called from
	int const emsCallFromComponentGetInput( 15 ); // EMS called from end of get input for a component
	int const emsCallFromUserDefinedComponentModel( 16 ); // EMS called from inside a custom user component model
	int const emsCallFromUnitarySystemSizing( 17 ); // EMS called from unitary system compound component

	int const ScheduleAlwaysOn( -1 ); // Value when passed to schedule routines gives back 1.0 (on)

	// DERIVED TYPE DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS:
	// see DataOmterfaces fpr global interface statements

	// MODULE VARIABLE DECLARATIONS:

	bool BeginDayFlag( false ); // True at the start of each day, False after first time step in day
	bool BeginEnvrnFlag( false ); // True at the start of each environment, False after first time step in environ
	bool BeginHourFlag( false ); // True at the start of each hour, False after first time step in hour
	bool BeginSimFlag( false ); // True until any actual simulation (full or sizing) has begun, False after first time step
	bool BeginFullSimFlag( false ); // True until full simulation has begun, False after first time step
	bool BeginTimeStepFlag( false ); // True at the start of each time step, False after first subtime step of time step
	int DayOfSim( 0 ); // Counter for days (during the simulation)
	std::string DayOfSimChr( "0" ); // Counter for days (during the simulation) (character -- for reporting)
	bool EndEnvrnFlag( false ); // True at the end of each environment (last time step of last hour of last day of environ)
	bool EndDesignDayEnvrnsFlag( false ); // True at the end of the last design day environment
	// (last time step of last hour of last day of environ which is a design day)
	bool EndDayFlag( false ); // True at the end of each day (last time step of last hour of day)
	bool EndHourFlag( false ); // True at the end of each hour (last time step of hour)
	int PreviousHour( 0 ); // Previous Hour Index
	int HourOfDay( 0 ); // Counter for hours in a simulation day
	Real64 WeightPreviousHour( 0.0 ); // Weighting of value for previous hour
	Real64 WeightNow( 0.0 ); // Weighting of value for current hour
	int NumOfDayInEnvrn( 0 ); // Number of days in the simulation for a particular environment
	int NumOfTimeStepInHour( 0 ); // Number of time steps in each hour of the simulation
	int NumOfZones( 0 ); // Total number of Zones for simulation
	int TimeStep( 0 ); // Counter for time steps (fractional hours)
	Real64 TimeStepZone( 0.0 ); // Zone time step in fractional hours
	bool WarmupFlag( false ); // True during the warmup portion of a simulation
	int OutputFileStandard( 0 ); // Unit number for the standard output file (hourly data only)
	std::ostream * eso_stream( nullptr ); // Internal stream used for eso output (used for performance)
	int StdOutputRecordCount( 0 ); // Count of Standard output records
	int OutputFileInits( 0 ); // Unit number for the standard Initialization output file
	int OutputFileDebug( 0 ); // Unit number for debug outputs
	int OutputFileZoneSizing( 0 ); // Unit number of zone sizing calc output file
	int OutputFileSysSizing( 0 ); // Unit number of system sizing calc output file
	int OutputFileMeters( 0 ); // Unit number for meters output
	std::ostream * mtr_stream( nullptr ); // Internal stream used for mtr output (used for performance)
	int StdMeterRecordCount( 0 ); // Count of Meter output records
	int OutputFileBNDetails( 0 ); // Unit number for Branch-Node Details
	bool ZoneSizingCalc( false ); // TRUE if zone sizing calculation
	bool SysSizingCalc( false ); // TRUE if system sizing calculation
	bool DoZoneSizing( false ); // User input in SimulationControl object
	bool DoSystemSizing( false ); // User input in SimulationControl object
	bool DoPlantSizing( false ); // User input in SimulationControl object
	bool DoDesDaySim( false ); // User input in SimulationControl object
	bool DoWeathSim( false ); // User input in SimulationControl object
	bool WeathSimReq( false ); // Input has a RunPeriod request
	int KindOfSim( 0 ); // See parameters. (ksDesignDay, ksRunPeriodDesign, ksRunPeriodWeather)
	bool DoOutputReporting( false ); // TRUE if variables to be written out
	bool DoingSizing( false ); // TRUE when "sizing" is being performed (some error messages won't be displayed)
	bool DoingInputProcessing( false ); // TRUE when "IP" is being performed (some error messages are cached)
	bool DisplayAllWarnings( false ); // True when selection for  "DisplayAllWarnings" is entered (turns on other warning flags)
	bool DisplayExtraWarnings( false ); // True when selection for  "DisplayExtraWarnings" is entered
	bool DisplayUnusedObjects( false ); // True when selection for  "DisplayUnusedObjects" is entered
	bool DisplayUnusedSchedules( false ); // True when selection for  "DisplayUnusedSchedules" is entered
	bool DisplayAdvancedReportVariables( false ); // True when selection for  "DisplayAdvancedReportVariables" is entered
	bool DisplayZoneAirHeatBalanceOffBalance( false ); // True when selection for  "DisplayZoneAirHeatBalanceOffBalance" is entered
	bool CreateMinimalSurfaceVariables( false ); // True when selection for  "CreateMinimalSurfaceVariables" is entered
	Real64 CurrentTime( 0.0 ); // CurrentTime, in fractional hours, from start of day. Uses Loads time step.
	int SimTimeSteps( 0 ); // Number of (Loads) timesteps since beginning of run period (environment).
	int MinutesPerTimeStep; // Minutes per time step calculated from NumTimeStepInHour (number of minutes per load time step)
	bool MetersHaveBeenInitialized( false );
	bool KickOffSimulation( false ); // Kick off simulation -- meaning run each environment for 1 or 2 time steps.
	bool KickOffSizing( false ); // Kick off sizing -- meaning run each environment for 1 or 2 time steps.
	bool AnyEnergyManagementSystemInModel( false ); // true if there is any EMS or Erl in model.  otherwise false
	bool AnyPlantInModel( false ); // true if there are any plant or condenser loops in model, otherwise false
	int CacheIPErrorFile( 0 ); // Cache IP errors until IDF processing done.
	bool AnyIdealCondEntSetPointInModel( false ); // true if there is any ideal condenser entering set point manager in model.
	bool RunOptCondEntTemp( false ); // true if the ideal condenser entering set point optimization is running
	bool CompLoadReportIsReq( false ); // true if the extra sizing calcs are performed to create a "pulse" for the load component report
	bool isPulseZoneSizing( false ); // true during the set of zone sizing calcs that include the "pulse" for the load component report
	int OutputFileZonePulse( 0 ); // file handle for special zone sizing report that contains the result of the "pulse" for the load component report
	bool doLoadComponentPulseNow( false ); // true for the time step that is the "pulse" for the load component report
	bool ShowDecayCurvesInEIO( false ); // true if the Radiant to Convective Decay Curves should appear in the EIO file
	bool AnySlabsInModel ( false ); // true if there are any zone-coupled ground domains in the input file
	bool AnyBasementsInModel( false ); // true if there are any basements in the input file

	int Progress( 0 ); // current progress (0-100)
	void ( *fProgressPtr )( int );
	void ( *fMessagePtr )( std::string );

	//     NOTICE
	//     Copyright � 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.
	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in main.cc.
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

} // DataGlobals

} // EnergyPlus
