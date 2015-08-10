#ifndef DataZoneControls_hh_INCLUDED
#define DataZoneControls_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DataZoneControls {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	extern int NumTempControlledZones;
	extern int NumHumidityControlZones;
	extern int NumComfortControlledZones;
	extern int NumTStatStatements;
	extern int NumComfortTStatStatements;
	extern int NumOpTempControlledZones; // number of zones with operative temp control
	extern int NumTempAndHumidityControlledZones; // number of zones with over cool control
	extern bool AnyOpTempControl; // flag set true if any zones have op temp control
	extern bool AnyZoneTempAndHumidityControl; // flag set true if any zones have over cool control
	extern Array1D_bool StageZoneLogic; // Logical array, A zone with staged thermostat = .TRUE.
	extern Array1D< Real64 > OccRoomTSetPointHeat; // occupied heating set point for optimum start period
	extern Array1D< Real64 > OccRoomTSetPointCool; // occupied cooling set point for optimum start period
	extern bool GetZoneAirStatsInputFlag; // True when need to get input

	// Types

	// Clears the global data in DataZoneControls.
	// Needed for unit tests, should not be normally called.
	void
	clear_state();

	struct ZoneTempControls
	{
		// Members
		std::string Name; // Name of the thermostat
		std::string ZoneName; // Name of the zone
		int ActualZoneNum;
		std::string ControlTypeSchedName; // Name of the schedule which determines the zone temp setpoint
		int CTSchedIndex; // Index for this schedule
		int NumControlTypes;
		Array1D_string ControlType;
		Array1D_string ControlTypeName;
		Array1D_int ControlTypeSchIndx;
		int SchIndx_SingleHeatSetPoint;
		int SchIndx_SingleCoolSetPoint;
		int SchIndx_SingleHeatCoolSetPoint;
		int SchIndx_DualSetPointWDeadBand;
		bool ManageDemand; // Flag to indicate whether to use demand limiting
		Real64 HeatingResetLimit; // Lowest heating setpoint that can be set by demand manager [C]
		Real64 CoolingResetLimit; // Highest cooling setpoint that can be set by demand manager [C]
		bool EMSOverrideHeatingSetPointOn; // EMS is calling to override heating setpoint
		Real64 EMSOverrideHeatingSetPointValue; // value EMS is directing to use for heating setpoint [C]
		bool EMSOverrideCoolingSetPointOn; // EMS is calling to override cooling setpoint
		Real64 EMSOverrideCoolingSetPointValue; // value EMS is directing to use for cooling setpoint [C]
		bool OperativeTempControl; // flag to indicate whether control based on Operative Temp
		bool OpTempCntrlModeScheduled; // flag to indicate if radiative fraction is scheduled,
		// else constant
		Real64 FixedRadiativeFraction; // weighting factor for mean radiant temp for Operative temperature
		int OpTempRadiativeFractionSched; // index of schedule for when fraction is scheduled
		Real64 ZoneOvercoolRange; // Zone overcool temperature range (max), deg C
		bool ZoneOvercoolControl; // Flag to indicate whether control is based on overcool
		bool OvercoolCntrlModeScheduled; // Flag to indicate if zone overcool range is scheduled
		//   or constant
		Real64 ZoneOvercoolConstRange; // Overcool Range for Zone Air Setpoint Temperature [deltaC]
		int ZoneOvercoolRangeSchedIndex; // Index for Overcool Range Schedule
		Real64 ZoneOvercoolControlRatio; // Zone relative humidity shift per dry-bulb temperature overcooling
		//      below the original cooling setpoint, %RH/deltaC
		std::string DehumidifyingSched; // Name of the schedule to determine the zone dehumidifying setpoint
		int DehumidifyingSchedIndex; // Index for dehumidifying schedule

		// Default Constructor
		ZoneTempControls() :
			ActualZoneNum( 0 ),
			CTSchedIndex( 0 ),
			NumControlTypes( 0 ),
			SchIndx_SingleHeatSetPoint( 0 ),
			SchIndx_SingleCoolSetPoint( 0 ),
			SchIndx_SingleHeatCoolSetPoint( 0 ),
			SchIndx_DualSetPointWDeadBand( 0 ),
			ManageDemand( false ),
			HeatingResetLimit( 0.0 ),
			CoolingResetLimit( 0.0 ),
			EMSOverrideHeatingSetPointOn( false ),
			EMSOverrideHeatingSetPointValue( 0.0 ),
			EMSOverrideCoolingSetPointOn( false ),
			EMSOverrideCoolingSetPointValue( 0.0 ),
			OperativeTempControl( false ),
			OpTempCntrlModeScheduled( false ),
			FixedRadiativeFraction( 0.0 ),
			OpTempRadiativeFractionSched( 0 ),
			ZoneOvercoolRange( 0.0 ),
			ZoneOvercoolControl( false ),
			OvercoolCntrlModeScheduled( false ),
			ZoneOvercoolConstRange( 0.0 ),
			ZoneOvercoolRangeSchedIndex( 0 ),
			ZoneOvercoolControlRatio( 0.0 ),
			DehumidifyingSchedIndex( 0 )
		{}

		// Member Constructor
		ZoneTempControls(
			std::string const & Name, // Name of the thermostat
			std::string const & ZoneName, // Name of the zone
			int const ActualZoneNum,
			std::string const & ControlTypeSchedName, // Name of the schedule which determines the zone temp setpoint
			int const CTSchedIndex, // Index for this schedule
			int const NumControlTypes,
			Array1_string const & ControlType,
			Array1_string const & ControlTypeName,
			Array1_int const & ControlTypeSchIndx,
			int const SchIndx_SingleHeatSetPoint,
			int const SchIndx_SingleCoolSetPoint,
			int const SchIndx_SingleHeatCoolSetPoint,
			int const SchIndx_DualSetPointWDeadBand,
			bool const ManageDemand, // Flag to indicate whether to use demand limiting
			Real64 const HeatingResetLimit, // Lowest heating setpoint that can be set by demand manager [C]
			Real64 const CoolingResetLimit, // Highest cooling setpoint that can be set by demand manager [C]
			bool const EMSOverrideHeatingSetPointOn, // EMS is calling to override heating setpoint
			Real64 const EMSOverrideHeatingSetPointValue, // value EMS is directing to use for heating setpoint [C]
			bool const EMSOverrideCoolingSetPointOn, // EMS is calling to override cooling setpoint
			Real64 const EMSOverrideCoolingSetPointValue, // value EMS is directing to use for cooling setpoint [C]
			bool const OperativeTempControl, // flag to indicate whether control based on Operative Temp
			bool const OpTempCntrlModeScheduled, // flag to indicate if radiative fraction is scheduled,
			Real64 const FixedRadiativeFraction, // weighting factor for mean radiant temp for Operative temperature
			int const OpTempRadiativeFractionSched, // index of schedule for when fraction is scheduled
			Real64 const ZoneOvercoolRange, // Zone overcool temperature range (max), deg C
			bool const ZoneOvercoolControl, // Flag to indicate whether control is based on overcool
			bool const OvercoolCntrlModeScheduled, // Flag to indicate if zone overcool range is scheduled
			Real64 const ZoneOvercoolConstRange, // Overcool Range for Zone Air Setpoint Temperature [deltaC]
			int const ZoneOvercoolRangeSchedIndex, // Index for Overcool Range Schedule
			Real64 const ZoneOvercoolControlRatio, // Zone relative humidity shift per dry-bulb temperature overcooling
			std::string const & DehumidifyingSched, // Name of the schedule to determine the zone dehumidifying setpoint
			int const DehumidifyingSchedIndex // Index for dehumidifying schedule
		) :
			Name( Name ),
			ZoneName( ZoneName ),
			ActualZoneNum( ActualZoneNum ),
			ControlTypeSchedName( ControlTypeSchedName ),
			CTSchedIndex( CTSchedIndex ),
			NumControlTypes( NumControlTypes ),
			ControlType( ControlType ),
			ControlTypeName( ControlTypeName ),
			ControlTypeSchIndx( ControlTypeSchIndx ),
			SchIndx_SingleHeatSetPoint( SchIndx_SingleHeatSetPoint ),
			SchIndx_SingleCoolSetPoint( SchIndx_SingleCoolSetPoint ),
			SchIndx_SingleHeatCoolSetPoint( SchIndx_SingleHeatCoolSetPoint ),
			SchIndx_DualSetPointWDeadBand( SchIndx_DualSetPointWDeadBand ),
			ManageDemand( ManageDemand ),
			HeatingResetLimit( HeatingResetLimit ),
			CoolingResetLimit( CoolingResetLimit ),
			EMSOverrideHeatingSetPointOn( EMSOverrideHeatingSetPointOn ),
			EMSOverrideHeatingSetPointValue( EMSOverrideHeatingSetPointValue ),
			EMSOverrideCoolingSetPointOn( EMSOverrideCoolingSetPointOn ),
			EMSOverrideCoolingSetPointValue( EMSOverrideCoolingSetPointValue ),
			OperativeTempControl( OperativeTempControl ),
			OpTempCntrlModeScheduled( OpTempCntrlModeScheduled ),
			FixedRadiativeFraction( FixedRadiativeFraction ),
			OpTempRadiativeFractionSched( OpTempRadiativeFractionSched ),
			ZoneOvercoolRange( ZoneOvercoolRange ),
			ZoneOvercoolControl( ZoneOvercoolControl ),
			OvercoolCntrlModeScheduled( OvercoolCntrlModeScheduled ),
			ZoneOvercoolConstRange( ZoneOvercoolConstRange ),
			ZoneOvercoolRangeSchedIndex( ZoneOvercoolRangeSchedIndex ),
			ZoneOvercoolControlRatio( ZoneOvercoolControlRatio ),
			DehumidifyingSched( DehumidifyingSched ),
			DehumidifyingSchedIndex( DehumidifyingSchedIndex )
		{}

	};

	struct ZoneHumidityControls
	{
		// Members
		std::string ControlName; // Name of this humidity controller
		std::string ZoneName; // Name of the zone
		std::string HumidifyingSched; // Name of the schedule to determine the zone humidifying setpoint
		std::string DehumidifyingSched; // Name of the schedule to determine the zone dehumidifying setpoint
		int ActualZoneNum;
		int HumidifyingSchedIndex; // Index for humidifying schedule
		int DehumidifyingSchedIndex; // Index for dehumidifying schedule
		int ErrorIndex; // Error index when LowRH setpoint > HighRH setpoint
		bool EMSOverrideHumidifySetPointOn; // EMS is calling to override humidifying setpoint
		Real64 EMSOverrideHumidifySetPointValue; // value EMS is directing to use for humidifying setpoint
		bool EMSOverrideDehumidifySetPointOn; // EMS is calling to override dehumidifying setpoint
		Real64 EMSOverrideDehumidifySetPointValue; // value EMS is directing to use for dehumidifying setpoint

		// Default Constructor
		ZoneHumidityControls() :
			ActualZoneNum( 0 ),
			HumidifyingSchedIndex( 0 ),
			DehumidifyingSchedIndex( 0 ),
			ErrorIndex( 0 ),
			EMSOverrideHumidifySetPointOn( false ),
			EMSOverrideHumidifySetPointValue( 0.0 ),
			EMSOverrideDehumidifySetPointOn( false ),
			EMSOverrideDehumidifySetPointValue( 0.0 )
		{}

		// Member Constructor
		ZoneHumidityControls(
			std::string const & ControlName, // Name of this humidity controller
			std::string const & ZoneName, // Name of the zone
			std::string const & HumidifyingSched, // Name of the schedule to determine the zone humidifying setpoint
			std::string const & DehumidifyingSched, // Name of the schedule to determine the zone dehumidifying setpoint
			int const ActualZoneNum,
			int const HumidifyingSchedIndex, // Index for humidifying schedule
			int const DehumidifyingSchedIndex, // Index for dehumidifying schedule
			int const ErrorIndex, // Error index when LowRH setpoint > HighRH setpoint
			bool const EMSOverrideHumidifySetPointOn, // EMS is calling to override humidifying setpoint
			Real64 const EMSOverrideHumidifySetPointValue, // value EMS is directing to use for humidifying setpoint
			bool const EMSOverrideDehumidifySetPointOn, // EMS is calling to override dehumidifying setpoint
			Real64 const EMSOverrideDehumidifySetPointValue // value EMS is directing to use for dehumidifying setpoint
		) :
			ControlName( ControlName ),
			ZoneName( ZoneName ),
			HumidifyingSched( HumidifyingSched ),
			DehumidifyingSched( DehumidifyingSched ),
			ActualZoneNum( ActualZoneNum ),
			HumidifyingSchedIndex( HumidifyingSchedIndex ),
			DehumidifyingSchedIndex( DehumidifyingSchedIndex ),
			ErrorIndex( ErrorIndex ),
			EMSOverrideHumidifySetPointOn( EMSOverrideHumidifySetPointOn ),
			EMSOverrideHumidifySetPointValue( EMSOverrideHumidifySetPointValue ),
			EMSOverrideDehumidifySetPointOn( EMSOverrideDehumidifySetPointOn ),
			EMSOverrideDehumidifySetPointValue( EMSOverrideDehumidifySetPointValue )
		{}

	};

	struct ZoneComfortControls
	{
		// Members
		std::string Name; // Name of the thermostat
		std::string ZoneName; // Name of the zone
		int ActualZoneNum; // Index number of zone
		std::string ControlTypeSchedName; // Name of the schedule which determines the zone temp setpoint
		int ComfortSchedIndex; // Index for this schedule
		int NumControlTypes; // Number of control types in ZoneControl:ThermalComfort object
		Array1D_string ControlType; // Type of control
		Array1D_string ControlTypeName; // Name of control type
		Array1D_int ControlTypeSchIndx; // Index to control type schedule
		int SchIndx_SglHeatSetPointFanger; // Index to fanger single heating setpoint schedule
		int SchIndx_SglCoolSetPointFanger; // Index to fanger single cooling setpoint schedule
		int SchIndx_SglHCSetPointFanger; // Index to fanger single heating/cooling setpoint schedule
		int SchIndx_DualSetPointFanger; // Index to fanger dual setpoint schedule
		int SchIndx_SglHeatSetPointPierce; // Index to pierce single heating setpoint schedule
		int SchIndx_SglCoolSetPointPierce; // Index to pierce single cooling setpoint schedule
		int SchIndx_SglHCSetPointPierce; // Index to pierce single heating/cooling setpoint schedule
		int SchIndx_DualSetPointPierce; // Index to pierce dual setpoint schedule
		int SchIndx_SglHeatSetPointKSU; // Index to KSU single heating setpoint schedule
		int SchIndx_SglCoolSetPointKSU; // Index to KSU single cooling setpoint schedule
		int SchIndx_SglHCSetPointKSU; // Index to KSU single heating/cooling setpoint schedule
		int SchIndx_DualSetPointKSU; // Index to KSU dual setpoint schedule
		bool ManageDemand; // Flag to indicate whether to use demand limiting
		Real64 HeatingResetLimit; // Lowest heating setpoint that can be set by demand manager [C]
		Real64 CoolingResetLimit; // Highest cooling setpoint that can be set by demand manager [C]
		bool EMSOverrideHeatingSetPointOn; // EMS is calling to override heating setpoint
		Real64 EMSOverrideHeatingSetPointValue; // value EMS is directing to use for heating setpoint
		bool EMSOverrideCoolingSetPointOn; // EMS is calling to override cooling setpoint
		Real64 EMSOverrideCoolingSetPointValue; // value EMS is directing to use for cooling setpoint
		Real64 TdbMaxSetPoint; // Maximum dry-bulb temperature setpoint [C]
		Real64 TdbMinSetPoint; // Minimum dry-bulb temperature setpoint [C]
		std::string AverageMethodName; // Averaging Method for Zones with Multiple People Objects
		std::string AverageObjectName; // Object Name for Specific Object Average
		int AverageMethodNum; // Numerical value for averaging method
		int SpecificObjectNum; // People Object number used for Specific people object choice
		int PeopleAverageErrIndex; // People average error index
		int TdbMaxErrIndex; // Single cooling setpoint error index
		int TdbMinErrIndex; // Single heating setpoint error index
		int TdbHCErrIndex; // Single heating cooling setpoint error index
		int TdbDualMaxErrIndex; // Dual cooling setpoint error index
		int TdbDualMinErrIndex; // Dual heating setpoint error index

		// Default Constructor
		ZoneComfortControls() :
			ActualZoneNum( 0 ),
			ComfortSchedIndex( 0 ),
			NumControlTypes( 0 ),
			SchIndx_SglHeatSetPointFanger( 0 ),
			SchIndx_SglCoolSetPointFanger( 0 ),
			SchIndx_SglHCSetPointFanger( 0 ),
			SchIndx_DualSetPointFanger( 0 ),
			SchIndx_SglHeatSetPointPierce( 0 ),
			SchIndx_SglCoolSetPointPierce( 0 ),
			SchIndx_SglHCSetPointPierce( 0 ),
			SchIndx_DualSetPointPierce( 0 ),
			SchIndx_SglHeatSetPointKSU( 0 ),
			SchIndx_SglCoolSetPointKSU( 0 ),
			SchIndx_SglHCSetPointKSU( 0 ),
			SchIndx_DualSetPointKSU( 0 ),
			ManageDemand( false ),
			HeatingResetLimit( 0.0 ),
			CoolingResetLimit( 0.0 ),
			EMSOverrideHeatingSetPointOn( false ),
			EMSOverrideHeatingSetPointValue( 0.0 ),
			EMSOverrideCoolingSetPointOn( false ),
			EMSOverrideCoolingSetPointValue( 0.0 ),
			TdbMaxSetPoint( 50.0 ),
			TdbMinSetPoint( 0.0 ),
			AverageMethodName( "PEOPLE AVERGAE" ),
			AverageMethodNum( 0 ),
			SpecificObjectNum( 0 ),
			PeopleAverageErrIndex( 0 ),
			TdbMaxErrIndex( 0 ),
			TdbMinErrIndex( 0 ),
			TdbHCErrIndex( 0 ),
			TdbDualMaxErrIndex( 0 ),
			TdbDualMinErrIndex( 0 )
		{}

		// Member Constructor
		ZoneComfortControls(
			std::string const & Name, // Name of the thermostat
			std::string const & ZoneName, // Name of the zone
			int const ActualZoneNum, // Index number of zone
			std::string const & ControlTypeSchedName, // Name of the schedule which determines the zone temp setpoint
			int const ComfortSchedIndex, // Index for this schedule
			int const NumControlTypes, // Number of control types in ZoneControl:ThermalComfort object
			Array1_string const & ControlType, // Type of control
			Array1_string const & ControlTypeName, // Name of control type
			Array1_int const & ControlTypeSchIndx, // Index to control type schedule
			int const SchIndx_SglHeatSetPointFanger, // Index to fanger single heating setpoint schedule
			int const SchIndx_SglCoolSetPointFanger, // Index to fanger single cooling setpoint schedule
			int const SchIndx_SglHCSetPointFanger, // Index to fanger single heating/cooling setpoint schedule
			int const SchIndx_DualSetPointFanger, // Index to fanger dual setpoint schedule
			int const SchIndx_SglHeatSetPointPierce, // Index to pierce single heating setpoint schedule
			int const SchIndx_SglCoolSetPointPierce, // Index to pierce single cooling setpoint schedule
			int const SchIndx_SglHCSetPointPierce, // Index to pierce single heating/cooling setpoint schedule
			int const SchIndx_DualSetPointPierce, // Index to pierce dual setpoint schedule
			int const SchIndx_SglHeatSetPointKSU, // Index to KSU single heating setpoint schedule
			int const SchIndx_SglCoolSetPointKSU, // Index to KSU single cooling setpoint schedule
			int const SchIndx_SglHCSetPointKSU, // Index to KSU single heating/cooling setpoint schedule
			int const SchIndx_DualSetPointKSU, // Index to KSU dual setpoint schedule
			bool const ManageDemand, // Flag to indicate whether to use demand limiting
			Real64 const HeatingResetLimit, // Lowest heating setpoint that can be set by demand manager [C]
			Real64 const CoolingResetLimit, // Highest cooling setpoint that can be set by demand manager [C]
			bool const EMSOverrideHeatingSetPointOn, // EMS is calling to override heating setpoint
			Real64 const EMSOverrideHeatingSetPointValue, // value EMS is directing to use for heating setpoint
			bool const EMSOverrideCoolingSetPointOn, // EMS is calling to override cooling setpoint
			Real64 const EMSOverrideCoolingSetPointValue, // value EMS is directing to use for cooling setpoint
			Real64 const TdbMaxSetPoint, // Maximum dry-bulb temperature setpoint [C]
			Real64 const TdbMinSetPoint, // Minimum dry-bulb temperature setpoint [C]
			std::string const & AverageMethodName, // Averaging Method for Zones with Multiple People Objects
			std::string const & AverageObjectName, // Object Name for Specific Object Average
			int const AverageMethodNum, // Numerical value for averaging method
			int const SpecificObjectNum, // People Object number used for Specific people object choice
			int const PeopleAverageErrIndex, // People average error index
			int const TdbMaxErrIndex, // Single cooling setpoint error index
			int const TdbMinErrIndex, // Single heating setpoint error index
			int const TdbHCErrIndex, // Single heating cooling setpoint error index
			int const TdbDualMaxErrIndex, // Dual cooling setpoint error index
			int const TdbDualMinErrIndex // Dual heating setpoint error index
		) :
			Name( Name ),
			ZoneName( ZoneName ),
			ActualZoneNum( ActualZoneNum ),
			ControlTypeSchedName( ControlTypeSchedName ),
			ComfortSchedIndex( ComfortSchedIndex ),
			NumControlTypes( NumControlTypes ),
			ControlType( ControlType ),
			ControlTypeName( ControlTypeName ),
			ControlTypeSchIndx( ControlTypeSchIndx ),
			SchIndx_SglHeatSetPointFanger( SchIndx_SglHeatSetPointFanger ),
			SchIndx_SglCoolSetPointFanger( SchIndx_SglCoolSetPointFanger ),
			SchIndx_SglHCSetPointFanger( SchIndx_SglHCSetPointFanger ),
			SchIndx_DualSetPointFanger( SchIndx_DualSetPointFanger ),
			SchIndx_SglHeatSetPointPierce( SchIndx_SglHeatSetPointPierce ),
			SchIndx_SglCoolSetPointPierce( SchIndx_SglCoolSetPointPierce ),
			SchIndx_SglHCSetPointPierce( SchIndx_SglHCSetPointPierce ),
			SchIndx_DualSetPointPierce( SchIndx_DualSetPointPierce ),
			SchIndx_SglHeatSetPointKSU( SchIndx_SglHeatSetPointKSU ),
			SchIndx_SglCoolSetPointKSU( SchIndx_SglCoolSetPointKSU ),
			SchIndx_SglHCSetPointKSU( SchIndx_SglHCSetPointKSU ),
			SchIndx_DualSetPointKSU( SchIndx_DualSetPointKSU ),
			ManageDemand( ManageDemand ),
			HeatingResetLimit( HeatingResetLimit ),
			CoolingResetLimit( CoolingResetLimit ),
			EMSOverrideHeatingSetPointOn( EMSOverrideHeatingSetPointOn ),
			EMSOverrideHeatingSetPointValue( EMSOverrideHeatingSetPointValue ),
			EMSOverrideCoolingSetPointOn( EMSOverrideCoolingSetPointOn ),
			EMSOverrideCoolingSetPointValue( EMSOverrideCoolingSetPointValue ),
			TdbMaxSetPoint( TdbMaxSetPoint ),
			TdbMinSetPoint( TdbMinSetPoint ),
			AverageMethodName( AverageMethodName ),
			AverageObjectName( AverageObjectName ),
			AverageMethodNum( AverageMethodNum ),
			SpecificObjectNum( SpecificObjectNum ),
			PeopleAverageErrIndex( PeopleAverageErrIndex ),
			TdbMaxErrIndex( TdbMaxErrIndex ),
			TdbMinErrIndex( TdbMinErrIndex ),
			TdbHCErrIndex( TdbHCErrIndex ),
			TdbDualMaxErrIndex( TdbDualMaxErrIndex ),
			TdbDualMinErrIndex( TdbDualMinErrIndex )
		{}

	};

	struct ZoneStagedControls
	{
		// Members
		std::string Name; // Name of the thermostat
		std::string ZoneName; // Name of the zone
		int ActualZoneNum; // Index number of zone
		std::string HeatSetBaseSchedName; // Name of the schedule which provides zone heating setpoint base
		int HSBchedIndex; // Index for this schedule
		std::string CoolSetBaseSchedName; // Name of the schedule which provides zone cooling setpoint base
		int CSBchedIndex; // Index for this schedule
		int NumOfHeatStages; // Number of heating stages
		int NumOfCoolStages; // Number of cooling stages
		Real64 HeatThroRange; // Heating throttling tempeature range
		Real64 CoolThroRange; // Cooling throttling tempeature range
		Array1D< Real64 > HeatTOffset; // Heating temperature offset
		Array1D< Real64 > CoolTOffset; // Cooling temperature offset
		Real64 HeatSetPoint; // Heating throttling tempeature range
		Real64 CoolSetPoint; // Cooling throttling tempeature range
		int StageErrCount; // Staged setpoint erro count
		int StageErrIndex; // Staged setpoint erro index

		// Default Constructor
		ZoneStagedControls() :
			ActualZoneNum( 0 ),
			HSBchedIndex( 0 ),
			CSBchedIndex( 0 ),
			NumOfHeatStages( 0 ),
			NumOfCoolStages( 0 ),
			HeatThroRange( 0.0 ),
			CoolThroRange( 0.0 ),
			HeatSetPoint( 0.0 ),
			CoolSetPoint( 0.0 ),
			StageErrCount( 0 ),
			StageErrIndex( 0 )
		{}

		// Member Constructor
		ZoneStagedControls(
			std::string const & Name, // Name of the thermostat
			std::string const & ZoneName, // Name of the zone
			int const ActualZoneNum, // Index number of zone
			std::string const & HeatSetBaseSchedName, // Name of the schedule which provides zone heating setpoint base
			int const HSBchedIndex, // Index for this schedule
			std::string const & CoolSetBaseSchedName, // Name of the schedule which provides zone cooling setpoint base
			int const CSBchedIndex, // Index for this schedule
			int const NumOfHeatStages, // Number of heating stages
			int const NumOfCoolStages, // Number of cooling stages
			Real64 const HeatThroRange, // Heating throttling tempeature range
			Real64 const CoolThroRange, // Cooling throttling tempeature range
			Array1< Real64 > const & HeatTOffset, // Heating temperature offset
			Array1< Real64 > const & CoolTOffset, // Cooling temperature offset
			Real64 const HeatSetPoint, // Heating throttling tempeature range
			Real64 const CoolSetPoint, // Cooling throttling tempeature range
			int const StageErrCount, // Staged setpoint erro count
			int const StageErrIndex // Staged setpoint erro index
		) :
			Name( Name ),
			ZoneName( ZoneName ),
			ActualZoneNum( ActualZoneNum ),
			HeatSetBaseSchedName( HeatSetBaseSchedName ),
			HSBchedIndex( HSBchedIndex ),
			CoolSetBaseSchedName( CoolSetBaseSchedName ),
			CSBchedIndex( CSBchedIndex ),
			NumOfHeatStages( NumOfHeatStages ),
			NumOfCoolStages( NumOfCoolStages ),
			HeatThroRange( HeatThroRange ),
			CoolThroRange( CoolThroRange ),
			HeatTOffset( HeatTOffset ),
			CoolTOffset( CoolTOffset ),
			HeatSetPoint( HeatSetPoint ),
			CoolSetPoint( CoolSetPoint ),
			StageErrCount( StageErrCount ),
			StageErrIndex( StageErrIndex )
		{}

	};

	struct TStatObject
	{
		// Members
		std::string Name;
		int ZoneOrZoneListPtr;
		int NumOfZones;
		int TempControlledZoneStartPtr;
		int ComfortControlledZoneStartPtr;
		int StageControlledZoneStartPtr;
		bool ZoneListActive;

		// Default Constructor
		TStatObject() :
			ZoneOrZoneListPtr( 0 ),
			NumOfZones( 0 ),
			TempControlledZoneStartPtr( 0 ),
			ComfortControlledZoneStartPtr( 0 ),
			StageControlledZoneStartPtr( 0 ),
			ZoneListActive( false )
		{}

		// Member Constructor
		TStatObject(
			std::string const & Name,
			int const ZoneOrZoneListPtr,
			int const NumOfZones,
			int const TempControlledZoneStartPtr,
			int const ComfortControlledZoneStartPtr,
			int const StageControlledZoneStartPtr,
			bool const ZoneListActive
		) :
			Name( Name ),
			ZoneOrZoneListPtr( ZoneOrZoneListPtr ),
			NumOfZones( NumOfZones ),
			TempControlledZoneStartPtr( TempControlledZoneStartPtr ),
			ComfortControlledZoneStartPtr( ComfortControlledZoneStartPtr ),
			StageControlledZoneStartPtr( StageControlledZoneStartPtr ),
			ZoneListActive( ZoneListActive )
		{}

	};

	// Object Data
	extern Array1D< ZoneHumidityControls > HumidityControlZone;
	extern Array1D< ZoneTempControls > TempControlledZone;
	extern Array1D< ZoneComfortControls > ComfortControlledZone;
	extern Array1D< TStatObject > TStatObjects;
	extern Array1D< TStatObject > ComfortTStatObjects;
	extern Array1D< TStatObject > StagedTStatObjects;
	extern Array1D< ZoneStagedControls > StageControlledZone;

	//     NOTICE
	//     Copyright (c) 1996-2014 The Board of Trustees of the University of Illinois
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

} // DataZoneControls

} // EnergyPlus

#endif
