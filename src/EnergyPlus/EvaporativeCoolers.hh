#ifndef EvaporativeCoolers_hh_INCLUDED
#define EvaporativeCoolers_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/FArray1S.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <DataZoneEquipment.hh>

namespace EnergyPlus {

namespace EvaporativeCoolers {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS
	extern int const WaterSupplyFromMains;
	extern int const WaterSupplyFromTank;

	extern int const BlowThruFan;
	extern int const DrawThruFan;

	extern int const ZoneTemperatureDeadBandOnOffCycling;
	extern int const ZoneCoolingLoadOnOffCycling;
	extern int const ZoneCoolingLoadVariableSpeedFan;

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	extern bool GetInputEvapComponentsFlag; // Flag set to make sure you get input once
	extern int NumEvapCool; // The Number of Evap Coolers found in the Input
	extern FArray1D_bool MySizeFlag;
	extern FArray1D_bool CheckEquipName;

	extern int NumZoneEvapUnits;
	extern FArray1D_bool CheckZoneEvapUnitName;
	extern bool GetInputZoneEvapUnit;

	// SUBROUTINE SPECIFICATIONS FOR MODULE EvapCoolers

	// Types

	struct EvapConditions
	{
		// Members
		std::string EvapCoolerName; // Name of the EvapCooler
		int EquipIndex;
		int EvapCoolerType; // Type of the EvapCooler (parameters in DataGlobalConstants.cc
		std::string EvapControlType; // Type of Control for the EvapCooler
		std::string Schedule; // HeatingCoil Operation Schedule
		int SchedPtr; // Pointer to the correct schedule
		Real64 VolFlowRate; // Volume Flow Rate in Evap Cooler needed for calculating SatEff
		Real64 OutletTemp;
		Real64 OuletWetBulbTemp;
		Real64 OutletHumRat;
		Real64 OutletEnthalpy;
		Real64 OutletPressure;
		Real64 OutletMassFlowRate; // MassFlow through the EvapCooler being Simulated [kg/Sec]
		Real64 OutletMassFlowRateMaxAvail; // [kg/Sec]
		Real64 OutletMassFlowRateMinAvail; // [kg/Sec]
		bool InitFlag;
		int InletNode;
		int OutletNode;
		int SecondaryInletNode; // This is usually OA node feeding into the purge/secondary side
		int TertiaryInletNode; // This node is used to run building exhaust into purge side.
		Real64 InletMassFlowRate; // Inlet is primary process air node at inlet to cooler
		Real64 InletMassFlowRateMaxAvail;
		Real64 InletMassFlowRateMinAvail;
		Real64 InletTemp;
		Real64 InletWetBulbTemp;
		Real64 InletHumRat;
		Real64 InletEnthalpy;
		Real64 InletPressure;
		Real64 SecInletMassFlowRate; // Secondary inlet is for indirect coolers
		Real64 SecInletMassFlowRateMaxAvail;
		Real64 SecInletMassFlowRateMinAvail;
		Real64 SecInletTemp;
		Real64 SecInletWetBulbTemp;
		Real64 SecInletHumRat;
		Real64 SecInletEnthalpy;
		Real64 SecInletPressure;
		Real64 PadDepth;
		Real64 PadArea;
		Real64 RecircPumpPower;
		Real64 IndirectRecircPumpPower;
		Real64 IndirectPadDepth;
		Real64 IndirectPadArea;
		Real64 IndirectVolFlowRate;
		Real64 IndirectFanEff;
		Real64 IndirectFanDeltaPress;
		Real64 IndirectHXEffectiveness;
		Real64 DirectEffectiveness; // input saturation effectiveness for constant effectiveness model
		Real64 WetCoilMaxEfficiency;
		Real64 WetCoilFlowRatio;
		Real64 EvapCoolerEnergy;
		Real64 EvapCoolerPower;
		int EvapWaterSupplyMode; // where does water come from
		std::string EvapWaterSupplyName; // name of water source e.g. water storage tank
		int EvapWaterSupTankID;
		int EvapWaterTankDemandARRID;
		Real64 DriftFraction; // excess water from drift as fraction of Evap Water Consumption rate
		Real64 BlowDownRatio; // excess water use for blowdown as solids ratio to be maintained
		Real64 EvapWaterConsumpRate; // Evap Water Consumption rate in m3/sec
		Real64 EvapWaterConsump; // Evap Water Consumption in m3
		Real64 EvapWaterStarvMakupRate; // Evap water consumed but not really available from tank m3/s
		Real64 EvapWaterStarvMakup; // Evap water consumed but not really available from tank m3
		Real64 SatEff; // Reporting for Direct Stage and Ind Dry Saturation Efficiency
		Real64 StageEff; // Reporting for Indirect Total Stage Efficiency
		Real64 DPBoundFactor; // in RDDSpecial efficency w.r.t. dewpoint
		int EvapControlNodeNum; // need to control to avoid over cooling
		Real64 DesiredOutletTemp; // setpoint manager should set this
		Real64 PartLoadFract; // reduces cooling performance and associated fan power
		int DewPointBoundFlag; // report when indirect research special cooler is bound by dewpoint
		// rather than wetbulb-depression approach

		// Default Constructor
		EvapConditions() :
			EquipIndex( 0 ),
			EvapCoolerType( 0 ),
			SchedPtr( 0 ),
			VolFlowRate( 0.0 ),
			OutletTemp( 0.0 ),
			OuletWetBulbTemp( 0.0 ),
			OutletHumRat( 0.0 ),
			OutletEnthalpy( 0.0 ),
			OutletPressure( 0.0 ),
			OutletMassFlowRate( 0.0 ),
			OutletMassFlowRateMaxAvail( 0.0 ),
			OutletMassFlowRateMinAvail( 0.0 ),
			InitFlag( false ),
			InletNode( 0 ),
			OutletNode( 0 ),
			SecondaryInletNode( 0 ),
			TertiaryInletNode( 0 ),
			InletMassFlowRate( 0.0 ),
			InletMassFlowRateMaxAvail( 0.0 ),
			InletMassFlowRateMinAvail( 0.0 ),
			InletTemp( 0.0 ),
			InletWetBulbTemp( 0.0 ),
			InletHumRat( 0.0 ),
			InletEnthalpy( 0.0 ),
			InletPressure( 0.0 ),
			SecInletMassFlowRate( 0.0 ),
			SecInletMassFlowRateMaxAvail( 0.0 ),
			SecInletMassFlowRateMinAvail( 0.0 ),
			SecInletTemp( 0.0 ),
			SecInletWetBulbTemp( 0.0 ),
			SecInletHumRat( 0.0 ),
			SecInletEnthalpy( 0.0 ),
			SecInletPressure( 0.0 ),
			PadDepth( 0.0 ),
			PadArea( 0.0 ),
			RecircPumpPower( 0.0 ),
			IndirectRecircPumpPower( 0.0 ),
			IndirectPadDepth( 0.0 ),
			IndirectPadArea( 0.0 ),
			IndirectVolFlowRate( 0.0 ),
			IndirectFanEff( 0.0 ),
			IndirectFanDeltaPress( 0.0 ),
			IndirectHXEffectiveness( 0.0 ),
			DirectEffectiveness( 0.0 ),
			WetCoilMaxEfficiency( 0.0 ),
			WetCoilFlowRatio( 0.0 ),
			EvapCoolerEnergy( 0.0 ),
			EvapCoolerPower( 0.0 ),
			EvapWaterSupplyMode( WaterSupplyFromMains ),
			EvapWaterSupTankID( 0 ),
			EvapWaterTankDemandARRID( 0 ),
			DriftFraction( 0.0 ),
			BlowDownRatio( 0.0 ),
			EvapWaterConsumpRate( 0.0 ),
			EvapWaterConsump( 0.0 ),
			EvapWaterStarvMakupRate( 0.0 ),
			EvapWaterStarvMakup( 0.0 ),
			SatEff( 0.0 ),
			StageEff( 0.0 ),
			DPBoundFactor( 0.0 ),
			EvapControlNodeNum( 0 ),
			DesiredOutletTemp( 0.0 ),
			PartLoadFract( 0.0 ),
			DewPointBoundFlag( 0 )
		{}

		// Member Constructor
		EvapConditions(
			std::string const & EvapCoolerName, // Name of the EvapCooler
			int const EquipIndex,
			int const EvapCoolerType, // Type of the EvapCooler (parameters in DataGlobalConstants.cc
			std::string const & EvapControlType, // Type of Control for the EvapCooler
			std::string const & Schedule, // HeatingCoil Operation Schedule
			int const SchedPtr, // Pointer to the correct schedule
			Real64 const VolFlowRate, // Volume Flow Rate in Evap Cooler needed for calculating SatEff
			Real64 const OutletTemp,
			Real64 const OuletWetBulbTemp,
			Real64 const OutletHumRat,
			Real64 const OutletEnthalpy,
			Real64 const OutletPressure,
			Real64 const OutletMassFlowRate, // MassFlow through the EvapCooler being Simulated [kg/Sec]
			Real64 const OutletMassFlowRateMaxAvail, // [kg/Sec]
			Real64 const OutletMassFlowRateMinAvail, // [kg/Sec]
			bool const InitFlag,
			int const InletNode,
			int const OutletNode,
			int const SecondaryInletNode, // This is usually OA node feeding into the purge/secondary side
			int const TertiaryInletNode, // This node is used to run building exhaust into purge side.
			Real64 const InletMassFlowRate, // Inlet is primary process air node at inlet to cooler
			Real64 const InletMassFlowRateMaxAvail,
			Real64 const InletMassFlowRateMinAvail,
			Real64 const InletTemp,
			Real64 const InletWetBulbTemp,
			Real64 const InletHumRat,
			Real64 const InletEnthalpy,
			Real64 const InletPressure,
			Real64 const SecInletMassFlowRate, // Secondary inlet is for indirect coolers
			Real64 const SecInletMassFlowRateMaxAvail,
			Real64 const SecInletMassFlowRateMinAvail,
			Real64 const SecInletTemp,
			Real64 const SecInletWetBulbTemp,
			Real64 const SecInletHumRat,
			Real64 const SecInletEnthalpy,
			Real64 const SecInletPressure,
			Real64 const PadDepth,
			Real64 const PadArea,
			Real64 const RecircPumpPower,
			Real64 const IndirectRecircPumpPower,
			Real64 const IndirectPadDepth,
			Real64 const IndirectPadArea,
			Real64 const IndirectVolFlowRate,
			Real64 const IndirectFanEff,
			Real64 const IndirectFanDeltaPress,
			Real64 const IndirectHXEffectiveness,
			Real64 const DirectEffectiveness, // input saturation effectiveness for constant effectiveness model
			Real64 const WetCoilMaxEfficiency,
			Real64 const WetCoilFlowRatio,
			Real64 const EvapCoolerEnergy,
			Real64 const EvapCoolerPower,
			int const EvapWaterSupplyMode, // where does water come from
			std::string const & EvapWaterSupplyName, // name of water source e.g. water storage tank
			int const EvapWaterSupTankID,
			int const EvapWaterTankDemandARRID,
			Real64 const DriftFraction, // excess water from drift as fraction of Evap Water Consumption rate
			Real64 const BlowDownRatio, // excess water use for blowdown as solids ratio to be maintained
			Real64 const EvapWaterConsumpRate, // Evap Water Consumption rate in m3/sec
			Real64 const EvapWaterConsump, // Evap Water Consumption in m3
			Real64 const EvapWaterStarvMakupRate, // Evap water consumed but not really available from tank m3/s
			Real64 const EvapWaterStarvMakup, // Evap water consumed but not really available from tank m3
			Real64 const SatEff, // Reporting for Direct Stage and Ind Dry Saturation Efficiency
			Real64 const StageEff, // Reporting for Indirect Total Stage Efficiency
			Real64 const DPBoundFactor, // in RDDSpecial efficency w.r.t. dewpoint
			int const EvapControlNodeNum, // need to control to avoid over cooling
			Real64 const DesiredOutletTemp, // setpoint manager should set this
			Real64 const PartLoadFract, // reduces cooling performance and associated fan power
			int const DewPointBoundFlag // report when indirect research special cooler is bound by dewpoint
		) :
			EvapCoolerName( EvapCoolerName ),
			EquipIndex( EquipIndex ),
			EvapCoolerType( EvapCoolerType ),
			EvapControlType( EvapControlType ),
			Schedule( Schedule ),
			SchedPtr( SchedPtr ),
			VolFlowRate( VolFlowRate ),
			OutletTemp( OutletTemp ),
			OuletWetBulbTemp( OuletWetBulbTemp ),
			OutletHumRat( OutletHumRat ),
			OutletEnthalpy( OutletEnthalpy ),
			OutletPressure( OutletPressure ),
			OutletMassFlowRate( OutletMassFlowRate ),
			OutletMassFlowRateMaxAvail( OutletMassFlowRateMaxAvail ),
			OutletMassFlowRateMinAvail( OutletMassFlowRateMinAvail ),
			InitFlag( InitFlag ),
			InletNode( InletNode ),
			OutletNode( OutletNode ),
			SecondaryInletNode( SecondaryInletNode ),
			TertiaryInletNode( TertiaryInletNode ),
			InletMassFlowRate( InletMassFlowRate ),
			InletMassFlowRateMaxAvail( InletMassFlowRateMaxAvail ),
			InletMassFlowRateMinAvail( InletMassFlowRateMinAvail ),
			InletTemp( InletTemp ),
			InletWetBulbTemp( InletWetBulbTemp ),
			InletHumRat( InletHumRat ),
			InletEnthalpy( InletEnthalpy ),
			InletPressure( InletPressure ),
			SecInletMassFlowRate( SecInletMassFlowRate ),
			SecInletMassFlowRateMaxAvail( SecInletMassFlowRateMaxAvail ),
			SecInletMassFlowRateMinAvail( SecInletMassFlowRateMinAvail ),
			SecInletTemp( SecInletTemp ),
			SecInletWetBulbTemp( SecInletWetBulbTemp ),
			SecInletHumRat( SecInletHumRat ),
			SecInletEnthalpy( SecInletEnthalpy ),
			SecInletPressure( SecInletPressure ),
			PadDepth( PadDepth ),
			PadArea( PadArea ),
			RecircPumpPower( RecircPumpPower ),
			IndirectRecircPumpPower( IndirectRecircPumpPower ),
			IndirectPadDepth( IndirectPadDepth ),
			IndirectPadArea( IndirectPadArea ),
			IndirectVolFlowRate( IndirectVolFlowRate ),
			IndirectFanEff( IndirectFanEff ),
			IndirectFanDeltaPress( IndirectFanDeltaPress ),
			IndirectHXEffectiveness( IndirectHXEffectiveness ),
			DirectEffectiveness( DirectEffectiveness ),
			WetCoilMaxEfficiency( WetCoilMaxEfficiency ),
			WetCoilFlowRatio( WetCoilFlowRatio ),
			EvapCoolerEnergy( EvapCoolerEnergy ),
			EvapCoolerPower( EvapCoolerPower ),
			EvapWaterSupplyMode( EvapWaterSupplyMode ),
			EvapWaterSupplyName( EvapWaterSupplyName ),
			EvapWaterSupTankID( EvapWaterSupTankID ),
			EvapWaterTankDemandARRID( EvapWaterTankDemandARRID ),
			DriftFraction( DriftFraction ),
			BlowDownRatio( BlowDownRatio ),
			EvapWaterConsumpRate( EvapWaterConsumpRate ),
			EvapWaterConsump( EvapWaterConsump ),
			EvapWaterStarvMakupRate( EvapWaterStarvMakupRate ),
			EvapWaterStarvMakup( EvapWaterStarvMakup ),
			SatEff( SatEff ),
			StageEff( StageEff ),
			DPBoundFactor( DPBoundFactor ),
			EvapControlNodeNum( EvapControlNodeNum ),
			DesiredOutletTemp( DesiredOutletTemp ),
			PartLoadFract( PartLoadFract ),
			DewPointBoundFlag( DewPointBoundFlag )
		{}

	};

	struct ZoneEvapCoolerUnitStruct
	{
		// Members
		std::string Name; // user identifier
		int ZoneNodeNum;
		int AvailSchedIndex; // pointer to local availability schedule
		std::string AvailManagerListName; // Name of an availability manager list object
		bool UnitIsAvailable;
		int FanAvailStatus;
		int OAInletNodeNum; // outdoor air inlet node index
		int UnitOutletNodeNum; // Unit air outlet (to zone) node index
		int UnitReliefNodeNum; // Unit relief air (from zone) node index (optional)
		std::string FanObjectClassName;
		int FanType_Num;
		std::string FanName;
		int FanIndex;
		Real64 ActualFanVolFlowRate;
		int FanAvailSchedPtr;
		int FanInletNodeNum;
		int FanOutletNodeNum;
		Real64 DesignAirVolumeFlowRate;
		Real64 DesignAirMassFlowRate;
		Real64 DesignFanSpeedRatio;
		Real64 FanSpeedRatio;
		int FanLocation;
		int ControlSchemeType;
		Real64 TimeElapsed;
		Real64 ThrottlingRange; // temperature range for hystersis type tstat contorl [Delta C]
		bool IsOnThisTimestep;
		bool WasOnLastTimestep;
		Real64 ThresholdCoolingLoad;
		std::string EvapCooler_1_ObjectClassName;
		std::string EvapCooler_1_Name;
		int EvapCooler_1_Type_Num;
		int EvapCooler_1_Index;
		bool EvapCooler_1_AvailStatus;
		std::string EvapCooler_2_ObjectClassName;
		std::string EvapCooler_2_Name;
		int EvapCooler_2_Type_Num;
		int EvapCooler_2_Index;
		bool EvapCooler_2_AvailStatus;
		Real64 OAInletRho; // fills internal variable, current inlet air density [kg/m3]
		Real64 OAInletCp; // fills internal variable, current inlet air specific heat [J/kg-c]
		Real64 OAInletTemp; // fills internal variable, current inlet air temperature [C]
		Real64 OAInletHumRat; // fills internal variable, current inlet air humidity ratio [kg/kg]
		Real64 OAInletMassFlowRate; // fills internal variable, current inlet air mass flow rate [kg/s]
		Real64 UnitOutletTemp; // filled by actuator, component outlet temperature [C]
		Real64 UnitOutletHumRat; // filled by actuator, component outlet humidity ratio [kg/kg]
		Real64 UnitOutletMassFlowRate; // filled by actuator, component outlet mass flow rate [kg/s]
		Real64 UnitReliefTemp; // filled by actuator, component outlet temperature [C]
		Real64 UnitReliefHumRat; // filled by actuator, component outlet humidity ratio [kg/kg]
		Real64 UnitReliefMassFlowRate; // filled by actuator, component outlet mass flow rate [kg/s]
		Real64 UnitTotalCoolingRate; // unit output to zone, total cooling rate [W]
		Real64 UnitTotalCoolingEnergy; // unit output to zone, total cooling energy [J]
		Real64 UnitSensibleCoolingRate; // unit output to zone, sensible cooling rate [W]
		Real64 UnitSensibleCoolingEnergy; // unit output to zone, sensible cooling energy [J]
		Real64 UnitLatentHeatingRate; // unit output to zone, latent heating rate [W]
		Real64 UnitLatentHeatingEnergy; // unit output to zone, latent heating energy [J]
		Real64 UnitLatentCoolingRate; // unit output to zone, latent cooling rate [W]
		Real64 UnitLatentCoolingEnergy; // unit output to zone, latent cooling energy [J]
		Real64 UnitFanSpeedRatio; // unit fan speed ratio, dimensionless [ ]
		int UnitVSControlMaxIterErrorIndex; // regula falsi errors, fan speed iteration limits
		int UnitVSControlLimitsErrorIndex; // regula falsi errors, limits exceeded.
		int ZonePtr; // pointer to a zone served by an evaportive cooler unit
		int HVACSizingIndex; // index of a HVACSizing object for an evaportive cooler unit

		// Default Constructor
		ZoneEvapCoolerUnitStruct() :
			ZoneNodeNum( 0 ),
			AvailSchedIndex( 0 ),
			UnitIsAvailable( false ),
			FanAvailStatus( 0 ),
			OAInletNodeNum( 0 ),
			UnitOutletNodeNum( 0 ),
			UnitReliefNodeNum( 0 ),
			FanType_Num( 0 ),
			FanIndex( 0 ),
			ActualFanVolFlowRate( 0.0 ),
			FanAvailSchedPtr( 0 ),
			FanInletNodeNum( 0 ),
			FanOutletNodeNum( 0 ),
			DesignAirVolumeFlowRate( 0.0 ),
			DesignAirMassFlowRate( 0.0 ),
			DesignFanSpeedRatio( 0.0 ),
			FanSpeedRatio( 0.0 ),
			FanLocation( 0 ),
			ControlSchemeType( 0 ),
			TimeElapsed( 0.0 ),
			ThrottlingRange( 0.0 ),
			IsOnThisTimestep( false ),
			WasOnLastTimestep( false ),
			ThresholdCoolingLoad( 0.0 ),
			EvapCooler_1_Type_Num( 0 ),
			EvapCooler_1_Index( 0 ),
			EvapCooler_1_AvailStatus( false ),
			EvapCooler_2_Type_Num( 0 ),
			EvapCooler_2_Index( 0 ),
			EvapCooler_2_AvailStatus( false ),
			OAInletRho( 0.0 ),
			OAInletCp( 0.0 ),
			OAInletTemp( 0.0 ),
			OAInletHumRat( 0.0 ),
			OAInletMassFlowRate( 0.0 ),
			UnitOutletTemp( 0.0 ),
			UnitOutletHumRat( 0.0 ),
			UnitOutletMassFlowRate( 0.0 ),
			UnitReliefTemp( 0.0 ),
			UnitReliefHumRat( 0.0 ),
			UnitReliefMassFlowRate( 0.0 ),
			UnitTotalCoolingRate( 0.0 ),
			UnitTotalCoolingEnergy( 0.0 ),
			UnitSensibleCoolingRate( 0.0 ),
			UnitSensibleCoolingEnergy( 0.0 ),
			UnitLatentHeatingRate( 0.0 ),
			UnitLatentHeatingEnergy( 0.0 ),
			UnitLatentCoolingRate( 0.0 ),
			UnitLatentCoolingEnergy( 0.0 ),
			UnitFanSpeedRatio( 0.0 ),
			UnitVSControlMaxIterErrorIndex( 0 ),
			UnitVSControlLimitsErrorIndex( 0 ),
			ZonePtr( 0 ),
			HVACSizingIndex( 0 )
		{}

		// Member Constructor
		ZoneEvapCoolerUnitStruct(
			std::string const & Name, // user identifier
			int const ZoneNodeNum,
			int const AvailSchedIndex, // pointer to local availability schedule
			std::string const & AvailManagerListName, // Name of an availability manager list object
			bool const UnitIsAvailable,
			int const FanAvailStatus,
			int const OAInletNodeNum, // outdoor air inlet node index
			int const UnitOutletNodeNum, // Unit air outlet (to zone) node index
			int const UnitReliefNodeNum, // Unit relief air (from zone) node index (optional)
			std::string const & FanObjectClassName,
			int const FanType_Num,
			std::string const & FanName,
			int const FanIndex,
			Real64 const ActualFanVolFlowRate,
			int const FanAvailSchedPtr,
			int const FanInletNodeNum,
			int const FanOutletNodeNum,
			Real64 const DesignAirVolumeFlowRate,
			Real64 const DesignAirMassFlowRate,
			Real64 const DesignFanSpeedRatio,
			Real64 const FanSpeedRatio,
			int const FanLocation,
			int const ControlSchemeType,
			Real64 const TimeElapsed,
			Real64 const ThrottlingRange, // temperature range for hystersis type tstat contorl [Delta C]
			bool const IsOnThisTimestep,
			bool const WasOnLastTimestep,
			Real64 const ThresholdCoolingLoad,
			std::string const & EvapCooler_1_ObjectClassName,
			std::string const & EvapCooler_1_Name,
			int const EvapCooler_1_Type_Num,
			int const EvapCooler_1_Index,
			bool const EvapCooler_1_AvailStatus,
			std::string const & EvapCooler_2_ObjectClassName,
			std::string const & EvapCooler_2_Name,
			int const EvapCooler_2_Type_Num,
			int const EvapCooler_2_Index,
			bool const EvapCooler_2_AvailStatus,
			Real64 const OAInletRho, // fills internal variable, current inlet air density [kg/m3]
			Real64 const OAInletCp, // fills internal variable, current inlet air specific heat [J/kg-c]
			Real64 const OAInletTemp, // fills internal variable, current inlet air temperature [C]
			Real64 const OAInletHumRat, // fills internal variable, current inlet air humidity ratio [kg/kg]
			Real64 const OAInletMassFlowRate, // fills internal variable, current inlet air mass flow rate [kg/s]
			Real64 const UnitOutletTemp, // filled by actuator, component outlet temperature [C]
			Real64 const UnitOutletHumRat, // filled by actuator, component outlet humidity ratio [kg/kg]
			Real64 const UnitOutletMassFlowRate, // filled by actuator, component outlet mass flow rate [kg/s]
			Real64 const UnitReliefTemp, // filled by actuator, component outlet temperature [C]
			Real64 const UnitReliefHumRat, // filled by actuator, component outlet humidity ratio [kg/kg]
			Real64 const UnitReliefMassFlowRate, // filled by actuator, component outlet mass flow rate [kg/s]
			Real64 const UnitTotalCoolingRate, // unit output to zone, total cooling rate [W]
			Real64 const UnitTotalCoolingEnergy, // unit output to zone, total cooling energy [J]
			Real64 const UnitSensibleCoolingRate, // unit output to zone, sensible cooling rate [W]
			Real64 const UnitSensibleCoolingEnergy, // unit output to zone, sensible cooling energy [J]
			Real64 const UnitLatentHeatingRate, // unit output to zone, latent heating rate [W]
			Real64 const UnitLatentHeatingEnergy, // unit output to zone, latent heating energy [J]
			Real64 const UnitLatentCoolingRate, // unit output to zone, latent cooling rate [W]
			Real64 const UnitLatentCoolingEnergy, // unit output to zone, latent cooling energy [J]
			Real64 const UnitFanSpeedRatio, // unit fan speed ratio, dimensionless [ ]
			int const UnitVSControlMaxIterErrorIndex, // regula falsi errors, fan speed iteration limits
			int const UnitVSControlLimitsErrorIndex, // regula falsi errors, limits exceeded.
			int const ZonePtr, // pointer to a zone served by an evaportive cooler unit
		    int const HVACSizingIndex // index of a HVACSizing object for an evaportive cooler unit
		) :
			Name( Name ),
			ZoneNodeNum( ZoneNodeNum ),
			AvailSchedIndex( AvailSchedIndex ),
			AvailManagerListName( AvailManagerListName ),
			UnitIsAvailable( UnitIsAvailable ),
			FanAvailStatus( FanAvailStatus ),
			OAInletNodeNum( OAInletNodeNum ),
			UnitOutletNodeNum( UnitOutletNodeNum ),
			UnitReliefNodeNum( UnitReliefNodeNum ),
			FanObjectClassName( FanObjectClassName ),
			FanType_Num( FanType_Num ),
			FanName( FanName ),
			FanIndex( FanIndex ),
			ActualFanVolFlowRate( ActualFanVolFlowRate ),
			FanAvailSchedPtr( FanAvailSchedPtr ),
			FanInletNodeNum( FanInletNodeNum ),
			FanOutletNodeNum( FanOutletNodeNum ),
			DesignAirVolumeFlowRate( DesignAirVolumeFlowRate ),
			DesignAirMassFlowRate( DesignAirMassFlowRate ),
			DesignFanSpeedRatio( DesignFanSpeedRatio ),
			FanSpeedRatio( FanSpeedRatio ),
			FanLocation( FanLocation ),
			ControlSchemeType( ControlSchemeType ),
			TimeElapsed( TimeElapsed ),
			ThrottlingRange( ThrottlingRange ),
			IsOnThisTimestep( IsOnThisTimestep ),
			WasOnLastTimestep( WasOnLastTimestep ),
			ThresholdCoolingLoad( ThresholdCoolingLoad ),
			EvapCooler_1_ObjectClassName( EvapCooler_1_ObjectClassName ),
			EvapCooler_1_Name( EvapCooler_1_Name ),
			EvapCooler_1_Type_Num( EvapCooler_1_Type_Num ),
			EvapCooler_1_Index( EvapCooler_1_Index ),
			EvapCooler_1_AvailStatus( EvapCooler_1_AvailStatus ),
			EvapCooler_2_ObjectClassName( EvapCooler_2_ObjectClassName ),
			EvapCooler_2_Name( EvapCooler_2_Name ),
			EvapCooler_2_Type_Num( EvapCooler_2_Type_Num ),
			EvapCooler_2_Index( EvapCooler_2_Index ),
			EvapCooler_2_AvailStatus( EvapCooler_2_AvailStatus ),
			OAInletRho( OAInletRho ),
			OAInletCp( OAInletCp ),
			OAInletTemp( OAInletTemp ),
			OAInletHumRat( OAInletHumRat ),
			OAInletMassFlowRate( OAInletMassFlowRate ),
			UnitOutletTemp( UnitOutletTemp ),
			UnitOutletHumRat( UnitOutletHumRat ),
			UnitOutletMassFlowRate( UnitOutletMassFlowRate ),
			UnitReliefTemp( UnitReliefTemp ),
			UnitReliefHumRat( UnitReliefHumRat ),
			UnitReliefMassFlowRate( UnitReliefMassFlowRate ),
			UnitTotalCoolingRate( UnitTotalCoolingRate ),
			UnitTotalCoolingEnergy( UnitTotalCoolingEnergy ),
			UnitSensibleCoolingRate( UnitSensibleCoolingRate ),
			UnitSensibleCoolingEnergy( UnitSensibleCoolingEnergy ),
			UnitLatentHeatingRate( UnitLatentHeatingRate ),
			UnitLatentHeatingEnergy( UnitLatentHeatingEnergy ),
			UnitLatentCoolingRate( UnitLatentCoolingRate ),
			UnitLatentCoolingEnergy( UnitLatentCoolingEnergy ),
			UnitFanSpeedRatio( UnitFanSpeedRatio ),
			UnitVSControlMaxIterErrorIndex( UnitVSControlMaxIterErrorIndex ),
			UnitVSControlLimitsErrorIndex( UnitVSControlLimitsErrorIndex ),
			ZonePtr( ZonePtr ),
			HVACSizingIndex( HVACSizingIndex )
		{}

	};

	struct ZoneEvapCoolerUnitFieldData
	{
		// Members
		FArray1D_string FieldNames;

		// Default Constructor
		ZoneEvapCoolerUnitFieldData()
		{}

		// Member Constructor
		ZoneEvapCoolerUnitFieldData(
			FArray1_string const & FieldNames // Name of the HeatingCoil numeric field descriptions
			) :
			FieldNames(FieldNames)
		{}
	};

	// Object Data
	extern FArray1D< EvapConditions > EvapCond;
	extern FArray1D< ZoneEvapCoolerUnitStruct > ZoneEvapUnit;
	extern FArray1D< ZoneEvapCoolerUnitFieldData > ZoneEvapCoolerUnitFields;
	

	// Functions

	void
	SimEvapCooler(
		std::string const & CompName,
		int & CompIndex
	);

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetEvapInput();

	// End of Get Input subroutines for the HB Module
	//******************************************************************************

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitEvapCooler( int const EvapCoolNum );

	void
	SizeEvapCooler( int const EvapCoolNum );

	// End Initialization Section of the Module
	//******************************************************************************

	// Begin Algorithm Section of the Module
	//******************************************************************************

	void
	CalcDirectEvapCooler( int & EvapCoolNum );

	void
	CalcDryIndirectEvapCooler( int & EvapCoolNum );

	void
	CalcWetIndirectEvapCooler( int & EvapCoolNum );

	void
	CalcResearchSpecialPartLoad( int & EvapCoolNum );

	void
	CalcIndirectResearchSpecialEvapCooler( int const EvapCoolNum );

	void
	CalcDirectResearchSpecialEvapCooler( int const EvapCoolNum );

	// End Algorithm Section of the Module
	// *****************************************************************************

	// Beginning of Update subroutines for the EvapCooler Module
	// *****************************************************************************

	void
	UpdateEvapCooler( int const EvapCoolNum );

	//        End of Update subroutines for the EvapCooler Module
	// *****************************************************************************

	// Beginning of Reporting subroutines for the EvapCooler Module
	// *****************************************************************************

	void
	ReportEvapCooler( int const EvapCoolNum );

	//***************
	//Begin routines for zone HVAC Evaporative cooler unit
	//_______________________________________________________________________________________________________________________
	//***************

	void
	SimZoneEvaporativeCoolerUnit(
		std::string const & CompName, // name of the packaged terminal heat pump
		int const ZoneNum, // number of zone being served
		Real64 & SensibleOutputProvided, // sensible capacity delivered to zone
		Real64 & LatentOutputProvided, // Latent add/removal  (kg/s), dehumid = negative
		int & CompIndex // index to zone hvac unit
	);

	void
	GetInputZoneEvaporativeCoolerUnit();

	void
	InitZoneEvaporativeCoolerUnit(
		int const UnitNum, // unit number
		int const ZoneNum // number of zone being served
	);

	void
	SizeZoneEvaporativeCoolerUnit( int const UnitNum ); // unit number

	void
	CalcZoneEvaporativeCoolerUnit(
		int const UnitNum, // unit number
		int const ZoneNum, // number of zone being served
		Real64 & SensibleOutputProvided, // sensible capacity delivered to zone
		Real64 & LatentOutputProvided // Latent add/removal  (kg/s), dehumid = negative
	);

	void
	ControlVSEvapUnitToMeetLoad(
		int const UnitNum, // unit number
		int const ZoneNum, // number of zone being served
		Real64 const ZoneCoolingLoad // target cooling load
	);

	Real64
	VSEvapUnitLoadResidual(
		Real64 const FanSpeedRatio,
		Optional< FArray1S< Real64 > const > Par = _ // parameters
	);

	void
	ReportZoneEvaporativeCoolerUnit( int const UnitNum ); // unit number

	//        End of Reporting subroutines for the EvaporativeCoolers Module
	// *****************************************************************************

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
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

} // EvaporativeCoolers

} // EnergyPlus

#endif
