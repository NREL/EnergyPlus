#ifndef PurchasedAirManager_hh_INCLUDED
#define PurchasedAirManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace PurchasedAirManager {

	// Using/Aliasing

	// Data
	//MODULE PARAMETER DEFINITIONS:
	// MODULE PARAMETER DEFINITIONS:
	// Heating and Cooling Limit type parameters
	extern int const NoLimit;
	extern int const LimitFlowRate;
	extern int const LimitCapacity;
	extern int const LimitFlowRateAndCapacity;
	extern Array1D_string const cLimitType;
	// Dehumidification and Humidification control type parameters
	extern int const None;
	extern int const ConstantSensibleHeatRatio;
	extern int const Humidistat;
	extern int const ConstantSupplyHumidityRatio;
	// Demand controlled ventilation type parameters
	extern int const NoDCV;
	extern int const OccupancySchedule;
	extern int const CO2SetPoint;
	// Outdoor air economizer type parameters
	extern int const NoEconomizer;
	extern int const DifferentialDryBulb;
	extern int const DifferentialEnthalpy;
	// Heat recovery type parameters
	extern int const NoHeatRecovery;
	extern int const Sensible;
	extern int const Enthalpy;
	// Operating mode parameters
	extern int const Off;
	extern int const Heat;
	extern int const Cool;
	extern int const DeadBand;
	// Delta humidity ratio limit, 0.00025 equals delta between 45F dewpoint and 46F dewpoint
	// used to prevent dividing by near zero
	extern Real64 const SmallDeltaHumRat;

	// DERIVED TYPE DEFINITIONS:

	//MODULE VARIABLE DECLARATIONS:

	extern int NumPurchAir;
	extern bool GetPurchAirInputFlag;
	extern Array1D_bool CheckEquipName;
	//SUBROUTINE SPECIFICATIONS FOR MODULE PurchasedAir:

	// Types

	struct ZonePurchasedAir
	{
		// Members
		std::string cObjectName; // Name of the object from IDD
		std::string Name; // Name or identifier of this piece of equipment
		std::string AvailSched; // System availablity schedule
		int AvailSchedPtr; // Index to system availability schedule
		int ZoneSupplyAirNodeNum; // Node number of zone supply air node for purchased air
		int ZoneExhaustAirNodeNum; // Node number of zone exhaust air node for purchased air
		int ZoneRecircAirNodeNum; // Node number of recirculation air node for purchased air
		//   same as exhaust node if specified, otherwise zone return node
		Real64 MaxHeatSuppAirTemp; // Maximum supply air temperature for heating [C]
		Real64 MinCoolSuppAirTemp; // Minimum supply air temperature for cooling [C]
		Real64 MaxHeatSuppAirHumRat; // Maximum supply heating air humidity ratio [kg water/kg dry air]
		Real64 MinCoolSuppAirHumRat; // Minimum supply cooling air humidity ratio [kg water/kg dry air]
		int HeatingLimit; // Heating capacity limit type - NoLimit, LimitFlowRate, LimitCapacity,
		//       or LimitFlowRateAndCapacity
		Real64 MaxHeatVolFlowRate; // Maximum heating supply air flow[m3/s]
		Real64 MaxHeatSensCap; // Maximum heating sensible capacity [W]
		int CoolingLimit; // Cooling capacity limit type - NoLimit, LimitFlowRate, LimitCapacity,
		//       or LimitFlowRateAndCapacity
		Real64 MaxCoolVolFlowRate; // Maximum cooling supply air flow [m3/s]
		Real64 MaxCoolTotCap; // Maximum cooling total capacity [W]
		std::string HeatSched; // Heating availablity schedule
		int HeatSchedPtr; // Index to heating availability schedule
		std::string CoolSched; // Cooling availability schedule
		int CoolSchedPtr; // Index to the cooling availability schedule
		int DehumidCtrlType; // Dehumidification control type - ConstantSensibleHeatRatio,
		//      Humidistat, or ConstantSupplyHumidityRatio
		Real64 CoolSHR; // Cooling sensible heat ratio
		int HumidCtrlType; // Humidification control type - None,
		//      Humidistat, or ConstantSupplyHumidityRatio
		int OARequirementsPtr; // Index to DesignSpecification:OutdoorAir object
		int DCVType; // Demand controlled ventilation type - None,
		//      OccupancySchedule, or CO2SetPoint
		int EconomizerType; // Outdoor air economizer type - NoEconomizer,
		//      DifferentialDryBulb, or DifferentialEnthalpy
		bool OutdoorAir; // Is there outdoor air?
		int OutdoorAirNodeNum; // Node number of the outdoor air inlet node
		int HtRecType; // Outdoor air heat recovery type - None, Sensible, Enthalpy
		Real64 HtRecSenEff; // Sensible heat recovery effectiveness
		Real64 HtRecLatEff; // Latent heat recovery effectiveness
		int OAFlowFracSchPtr; // Fraction schedule applied to total OA requirement
		Real64 MaxHeatMassFlowRate; // The maximum heating air mass flow rate [kg/s]
		Real64 MaxCoolMassFlowRate; // The maximum cooling air mass flow rate [kg/s]
		bool EMSOverrideMdotOn; // if true, then EMS is calling to override supply mass flow rate
		Real64 EMSValueMassFlowRate; // Value EMS is directing to use for supply mass flow rate [kg/s]
		bool EMSOverrideOAMdotOn; // if true, then EMS is calling to override OA mass flow rate
		Real64 EMSValueOAMassFlowRate; // Value EMS is directing to use for OA mass flow rate [kg/s]
		bool EMSOverrideSupplyTempOn; // if true, then EMS is calling to override supply temperature
		Real64 EMSValueSupplyTemp; // Value EMS is directing to use for supply temperature [C]
		bool EMSOverrideSupplyHumRatOn; // if true, then EMS is calling to override supply humidity ratio
		Real64 EMSValueSupplyHumRat; // Value EMS is directing to use for supply humidity ratio [kg-H2O/kg-dryair]
		Real64 MinOAMassFlowRate; // The minimum required outdoor air mass flow rate [kg/s]
		Real64 OutdoorAirMassFlowRate; // The outdoor air mass flow rate [kg/s]
		// Intermediate results
		Real64 FinalMixedAirTemp; // Dry-bulb temperature of the mixed air, saved for system ventilation load reporting [C]
		Real64 FinalMixedAirHumRat; // Humidity ratio of the mixed air, saved for system ventilation load reporting [kg-H2O/kg-dryair]
		Real64 HtRecSenOutput; // Sensible heating/cooling rate from heat recovery (<0 means cooling) [W]
		Real64 HtRecLatOutput; // Latent heating/cooling rate from heat recovery (<0 means cooling or dehumidfying) [W]
		Real64 OASenOutput; // Outdoor air sensible output relative to zone conditions [W], <0 means OA is cooler than zone air
		Real64 OALatOutput; // Outdoor air latent output relative to zone conditions [W], <0 means OA is drier than zone air
		Real64 SenOutputToZone; // Ideal Loads System sensible output to zone [W], <0 means supply is cooler than zone air
		Real64 LatOutputToZone; // Ideal Loads System latent heat output to zone [W], <0 means supply is drier than zone air
		Real64 SenCoilLoad; // Ideal Loads System sensible load on "coils" (<0 means cooling) [W]
		Real64 LatCoilLoad; // Ideal Loads System latent load on "coils" (<0 means cooling or dehumidfying) [W]
		int OAFlowMaxCoolOutputError; // Counter for OAFlow > Max Cooling Flow error
		int OAFlowMaxHeatOutputError; // Counter for OAFlow > Max Heating Flow error
		int SaturationOutputError; // Counter for OAFlow > Max Heating Flow error
		int OAFlowMaxCoolOutputIndex; // Recurring warning index for OAFlow > Max Cooling Flow error
		int OAFlowMaxHeatOutputIndex; // Recurring warning index for OAFlow > Max Heating Flow error
		int SaturationOutputIndex; // Recurring warning index for OAFlow > Max Heating Flow error
		int AvailStatus;
		int CoolErrIndex; // Cooling setpoint error index (recurring errors)
		int HeatErrIndex; // Heating setpoint error index (recurring errors)
		// Output variables
		Real64 SenHeatEnergy; // Sensible heating energy consumed [J]
		Real64 LatHeatEnergy; // Latent   heating energy consumed [J]
		Real64 TotHeatEnergy; // Total    heating energy consumed [J]
		Real64 SenCoolEnergy; // Sensible cooling energy consumed [J]
		Real64 LatCoolEnergy; // Latent   cooling energy consumed [J]
		Real64 TotCoolEnergy; // Total    cooling energy consumed [J]
		Real64 ZoneSenHeatEnergy; // Sensible heating energy supplied to the zone [J]
		Real64 ZoneLatHeatEnergy; // Latent   heating energy supplied to the zone [J]
		Real64 ZoneTotHeatEnergy; // Total    heating energy supplied to the zone [J]
		Real64 ZoneSenCoolEnergy; // Sensible cooling energy supplied to the zone [J]
		Real64 ZoneLatCoolEnergy; // Latent   cooling energy supplied to the zone [J]
		Real64 ZoneTotCoolEnergy; // Total    cooling energy supplied to the zone [J]
		Real64 OASenHeatEnergy; // Sensible heating energy required for OA to equal zone air [J]
		Real64 OALatHeatEnergy; // Latent   heating energy required for OA to equal zone air [J]
		Real64 OATotHeatEnergy; // Total    heating energy required for OA to equal zone air [J]
		Real64 OASenCoolEnergy; // Sensible cooling energy required for OA to equal zone air [J]
		Real64 OALatCoolEnergy; // Latent   cooling energy required for OA to equal zone air [J]
		Real64 OATotCoolEnergy; // Total    cooling energy required for OA to equal zone air [J]
		Real64 HtRecSenHeatEnergy; // Sensible heating energy from heat reocovery [J]
		Real64 HtRecLatHeatEnergy; // Latent   heating energy from heat reocovery [J]
		Real64 HtRecTotHeatEnergy; // Total    heating energy from heat reocovery [J]
		Real64 HtRecSenCoolEnergy; // Sensible cooling energy from heat reocovery [J]
		Real64 HtRecLatCoolEnergy; // Latent   cooling energy from heat reocovery [J]
		Real64 HtRecTotCoolEnergy; // Total    cooling energy from heat reocovery [J]
		Real64 SenHeatRate; // Sensible heating rate consumed [W]
		Real64 LatHeatRate; // Latent   heating rate consumed [W]
		Real64 TotHeatRate; // Total    heating rate consumed [W]
		Real64 SenCoolRate; // Sensible cooling rate consumed [W]
		Real64 LatCoolRate; // Latent   cooling rate consumed [W]
		Real64 TotCoolRate; // Total    cooling rate consumed [W]
		Real64 ZoneSenHeatRate; // Sensible heating rate supplied to the zone [W]
		Real64 ZoneLatHeatRate; // Latent   heating rate supplied to the zone [W]
		Real64 ZoneTotHeatRate; // Total    heating rate supplied to the zone [W]
		Real64 ZoneSenCoolRate; // Sensible cooling rate supplied to the zone [W]
		Real64 ZoneLatCoolRate; // Latent   cooling rate supplied to the zone [W]
		Real64 ZoneTotCoolRate; // Total    cooling rate supplied to the zone [W]
		Real64 OASenHeatRate; // Sensible heating rate required for OA to equal zone air [W]
		Real64 OALatHeatRate; // Latent   heating rate required for OA to equal zone air [W]
		Real64 OATotHeatRate; // Total    heating rate required for OA to equal zone air [W]
		Real64 OASenCoolRate; // Sensible cooling rate required for OA to equal zone air [W]
		Real64 OALatCoolRate; // Latent   cooling rate required for OA to equal zone air [W]
		Real64 OATotCoolRate; // Total    cooling rate required for OA to equal zone air [W]
		Real64 HtRecSenHeatRate; // Sensible heating rate from heat reocovery [W]
		Real64 HtRecLatHeatRate; // Latent   heating rate from heat reocovery [W]
		Real64 HtRecTotHeatRate; // Total    heating rate from heat reocovery [W]
		Real64 HtRecSenCoolRate; // Sensible cooling rate from heat reocovery [W]
		Real64 HtRecLatCoolRate; // Latent   cooling rate from heat reocovery [W]
		Real64 HtRecTotCoolRate; // Total    cooling rate from heat reocovery [W]
		Real64 TimeEconoActive; // Time economizer is active [hrs]
		Real64 TimeHtRecActive; // Time heat reocovery is active [hrs]
		int ZonePtr; // pointer to a zone served by an Ideal load air system
		int HVACSizingIndex; // index of a HVAC Sizing object for an Ideal load air system


		// Default Constructor
		ZonePurchasedAir() :
			AvailSchedPtr( 0 ),
			ZoneSupplyAirNodeNum( 0 ),
			ZoneExhaustAirNodeNum( 0 ),
			ZoneRecircAirNodeNum( 0 ),
			MaxHeatSuppAirTemp( 0.0 ),
			MinCoolSuppAirTemp( 0.0 ),
			MaxHeatSuppAirHumRat( 0.0 ),
			MinCoolSuppAirHumRat( 0.0 ),
			HeatingLimit( 0 ),
			MaxHeatVolFlowRate( 0.0 ),
			MaxHeatSensCap( 0.0 ),
			CoolingLimit( 0 ),
			MaxCoolVolFlowRate( 0.0 ),
			MaxCoolTotCap( 0.0 ),
			HeatSchedPtr( 0 ),
			CoolSchedPtr( 0 ),
			DehumidCtrlType( 0 ),
			CoolSHR( 0.0 ),
			HumidCtrlType( 0 ),
			OARequirementsPtr( 0 ),
			DCVType( 0 ),
			EconomizerType( 0 ),
			OutdoorAir( false ),
			OutdoorAirNodeNum( 0 ),
			HtRecType( 0 ),
			HtRecSenEff( 0.0 ),
			HtRecLatEff( 0.0 ),
			OAFlowFracSchPtr( 0 ),
			MaxHeatMassFlowRate( 0.0 ),
			MaxCoolMassFlowRate( 0.0 ),
			EMSOverrideMdotOn( false ),
			EMSValueMassFlowRate( 0.0 ),
			EMSOverrideOAMdotOn( false ),
			EMSValueOAMassFlowRate( 0.0 ),
			EMSOverrideSupplyTempOn( false ),
			EMSValueSupplyTemp( 0.0 ),
			EMSOverrideSupplyHumRatOn( false ),
			EMSValueSupplyHumRat( 0.0 ),
			MinOAMassFlowRate( 0.0 ),
			OutdoorAirMassFlowRate( 0.0 ),
			FinalMixedAirTemp( 0.0 ),
			FinalMixedAirHumRat( 0.0 ),
			HtRecSenOutput( 0.0 ),
			HtRecLatOutput( 0.0 ),
			OASenOutput( 0.0 ),
			OALatOutput( 0.0 ),
			SenOutputToZone( 0.0 ),
			LatOutputToZone( 0.0 ),
			SenCoilLoad( 0.0 ),
			LatCoilLoad( 0.0 ),
			OAFlowMaxCoolOutputError( 0 ),
			OAFlowMaxHeatOutputError( 0 ),
			SaturationOutputError( 0 ),
			OAFlowMaxCoolOutputIndex( 0 ),
			OAFlowMaxHeatOutputIndex( 0 ),
			SaturationOutputIndex( 0 ),
			AvailStatus( 0 ),
			CoolErrIndex( 0 ),
			HeatErrIndex( 0 ),
			SenHeatEnergy( 0.0 ),
			LatHeatEnergy( 0.0 ),
			TotHeatEnergy( 0.0 ),
			SenCoolEnergy( 0.0 ),
			LatCoolEnergy( 0.0 ),
			TotCoolEnergy( 0.0 ),
			ZoneSenHeatEnergy( 0.0 ),
			ZoneLatHeatEnergy( 0.0 ),
			ZoneTotHeatEnergy( 0.0 ),
			ZoneSenCoolEnergy( 0.0 ),
			ZoneLatCoolEnergy( 0.0 ),
			ZoneTotCoolEnergy( 0.0 ),
			OASenHeatEnergy( 0.0 ),
			OALatHeatEnergy( 0.0 ),
			OATotHeatEnergy( 0.0 ),
			OASenCoolEnergy( 0.0 ),
			OALatCoolEnergy( 0.0 ),
			OATotCoolEnergy( 0.0 ),
			HtRecSenHeatEnergy( 0.0 ),
			HtRecLatHeatEnergy( 0.0 ),
			HtRecTotHeatEnergy( 0.0 ),
			HtRecSenCoolEnergy( 0.0 ),
			HtRecLatCoolEnergy( 0.0 ),
			HtRecTotCoolEnergy( 0.0 ),
			SenHeatRate( 0.0 ),
			LatHeatRate( 0.0 ),
			TotHeatRate( 0.0 ),
			SenCoolRate( 0.0 ),
			LatCoolRate( 0.0 ),
			TotCoolRate( 0.0 ),
			ZoneSenHeatRate( 0.0 ),
			ZoneLatHeatRate( 0.0 ),
			ZoneTotHeatRate( 0.0 ),
			ZoneSenCoolRate( 0.0 ),
			ZoneLatCoolRate( 0.0 ),
			ZoneTotCoolRate( 0.0 ),
			OASenHeatRate( 0.0 ),
			OALatHeatRate( 0.0 ),
			OATotHeatRate( 0.0 ),
			OASenCoolRate( 0.0 ),
			OALatCoolRate( 0.0 ),
			OATotCoolRate( 0.0 ),
			HtRecSenHeatRate( 0.0 ),
			HtRecLatHeatRate( 0.0 ),
			HtRecTotHeatRate( 0.0 ),
			HtRecSenCoolRate( 0.0 ),
			HtRecLatCoolRate( 0.0 ),
			HtRecTotCoolRate( 0.0 ),
			TimeEconoActive( 0.0 ),
			TimeHtRecActive( 0.0 ),
			ZonePtr( 0 ),
			HVACSizingIndex( 0 )
		{}

		// Member Constructor
		ZonePurchasedAir(
			std::string const & cObjectName, // Name of the object from IDD
			std::string const & Name, // Name or identifier of this piece of equipment
			std::string const & AvailSched, // System availablity schedule
			int const AvailSchedPtr, // Index to system availability schedule
			int const ZoneSupplyAirNodeNum, // Node number of zone supply air node for purchased air
			int const ZoneExhaustAirNodeNum, // Node number of zone exhaust air node for purchased air
			int const ZoneRecircAirNodeNum, // Node number of recirculation air node for purchased air
			Real64 const MaxHeatSuppAirTemp, // Maximum supply air temperature for heating [C]
			Real64 const MinCoolSuppAirTemp, // Minimum supply air temperature for cooling [C]
			Real64 const MaxHeatSuppAirHumRat, // Maximum supply heating air humidity ratio [kg water/kg dry air]
			Real64 const MinCoolSuppAirHumRat, // Minimum supply cooling air humidity ratio [kg water/kg dry air]
			int const HeatingLimit, // Heating capacity limit type - NoLimit, LimitFlowRate, LimitCapacity,
			Real64 const MaxHeatVolFlowRate, // Maximum heating supply air flow[m3/s]
			Real64 const MaxHeatSensCap, // Maximum heating sensible capacity [W]
			int const CoolingLimit, // Cooling capacity limit type - NoLimit, LimitFlowRate, LimitCapacity,
			Real64 const MaxCoolVolFlowRate, // Maximum cooling supply air flow [m3/s]
			Real64 const MaxCoolTotCap, // Maximum cooling total capacity [W]
			std::string const & HeatSched, // Heating availablity schedule
			int const HeatSchedPtr, // Index to heating availability schedule
			std::string const & CoolSched, // Cooling availability schedule
			int const CoolSchedPtr, // Index to the cooling availability schedule
			int const DehumidCtrlType, // Dehumidification control type - ConstantSensibleHeatRatio,
			Real64 const CoolSHR, // Cooling sensible heat ratio
			int const HumidCtrlType, // Humidification control type - None,
			int const OARequirementsPtr, // Index to DesignSpecification:OutdoorAir object
			int const DCVType, // Demand controlled ventilation type - None,
			int const EconomizerType, // Outdoor air economizer type - NoEconomizer,
			bool const OutdoorAir, // Is there outdoor air?
			int const OutdoorAirNodeNum, // Node number of the outdoor air inlet node
			int const HtRecType, // Outdoor air heat recovery type - None, Sensible, Enthalpy
			Real64 const HtRecSenEff, // Sensible heat recovery effectiveness
			Real64 const HtRecLatEff, // Latent heat recovery effectiveness
			int const OAFlowFracSchPtr, // Fraction schedule applied to total OA requirement
			Real64 const MaxHeatMassFlowRate, // The maximum heating air mass flow rate [kg/s]
			Real64 const MaxCoolMassFlowRate, // The maximum cooling air mass flow rate [kg/s]
			bool const EMSOverrideMdotOn, // if true, then EMS is calling to override supply mass flow rate
			Real64 const EMSValueMassFlowRate, // Value EMS is directing to use for supply mass flow rate [kg/s]
			bool const EMSOverrideOAMdotOn, // if true, then EMS is calling to override OA mass flow rate
			Real64 const EMSValueOAMassFlowRate, // Value EMS is directing to use for OA mass flow rate [kg/s]
			bool const EMSOverrideSupplyTempOn, // if true, then EMS is calling to override supply temperature
			Real64 const EMSValueSupplyTemp, // Value EMS is directing to use for supply temperature [C]
			bool const EMSOverrideSupplyHumRatOn, // if true, then EMS is calling to override supply humidity ratio
			Real64 const EMSValueSupplyHumRat, // Value EMS is directing to use for supply humidity ratio [kg-H2O/kg-dryair]
			Real64 const MinOAMassFlowRate, // The minimum required outdoor air mass flow rate [kg/s]
			Real64 const OutdoorAirMassFlowRate, // The outdoor air mass flow rate [kg/s]
			Real64 const FinalMixedAirTemp, // Dry-bulb temperature of the mixed air, saved for system ventilation load reporting [C]
			Real64 const FinalMixedAirHumRat, // Humidity ratio of the mixed air, saved for system ventilation load reporting [kg-H2O/kg-dryair]
			Real64 const HtRecSenOutput, // Sensible heating/cooling rate from heat recovery (<0 means cooling) [W]
			Real64 const HtRecLatOutput, // Latent heating/cooling rate from heat recovery (<0 means cooling or dehumidfying) [W]
			Real64 const OASenOutput, // Outdoor air sensible output relative to zone conditions [W], <0 means OA is cooler than zone air
			Real64 const OALatOutput, // Outdoor air latent output relative to zone conditions [W], <0 means OA is drier than zone air
			Real64 const SenOutputToZone, // Ideal Loads System sensible output to zone [W], <0 means supply is cooler than zone air
			Real64 const LatOutputToZone, // Ideal Loads System latent heat output to zone [W], <0 means supply is drier than zone air
			Real64 const SenCoilLoad, // Ideal Loads System sensible load on "coils" (<0 means cooling) [W]
			Real64 const LatCoilLoad, // Ideal Loads System latent load on "coils" (<0 means cooling or dehumidfying) [W]
			int const OAFlowMaxCoolOutputError, // Counter for OAFlow > Max Cooling Flow error
			int const OAFlowMaxHeatOutputError, // Counter for OAFlow > Max Heating Flow error
			int const SaturationOutputError, // Counter for OAFlow > Max Heating Flow error
			int const OAFlowMaxCoolOutputIndex, // Recurring warning index for OAFlow > Max Cooling Flow error
			int const OAFlowMaxHeatOutputIndex, // Recurring warning index for OAFlow > Max Heating Flow error
			int const SaturationOutputIndex, // Recurring warning index for OAFlow > Max Heating Flow error
			int const AvailStatus,
			int const CoolErrIndex, // Cooling setpoint error index (recurring errors)
			int const HeatErrIndex, // Heating setpoint error index (recurring errors)
			Real64 const SenHeatEnergy, // Sensible heating energy consumed [J]
			Real64 const LatHeatEnergy, // Latent   heating energy consumed [J]
			Real64 const TotHeatEnergy, // Total    heating energy consumed [J]
			Real64 const SenCoolEnergy, // Sensible cooling energy consumed [J]
			Real64 const LatCoolEnergy, // Latent   cooling energy consumed [J]
			Real64 const TotCoolEnergy, // Total    cooling energy consumed [J]
			Real64 const ZoneSenHeatEnergy, // Sensible heating energy supplied to the zone [J]
			Real64 const ZoneLatHeatEnergy, // Latent   heating energy supplied to the zone [J]
			Real64 const ZoneTotHeatEnergy, // Total    heating energy supplied to the zone [J]
			Real64 const ZoneSenCoolEnergy, // Sensible cooling energy supplied to the zone [J]
			Real64 const ZoneLatCoolEnergy, // Latent   cooling energy supplied to the zone [J]
			Real64 const ZoneTotCoolEnergy, // Total    cooling energy supplied to the zone [J]
			Real64 const OASenHeatEnergy, // Sensible heating energy required for OA to equal zone air [J]
			Real64 const OALatHeatEnergy, // Latent   heating energy required for OA to equal zone air [J]
			Real64 const OATotHeatEnergy, // Total    heating energy required for OA to equal zone air [J]
			Real64 const OASenCoolEnergy, // Sensible cooling energy required for OA to equal zone air [J]
			Real64 const OALatCoolEnergy, // Latent   cooling energy required for OA to equal zone air [J]
			Real64 const OATotCoolEnergy, // Total    cooling energy required for OA to equal zone air [J]
			Real64 const HtRecSenHeatEnergy, // Sensible heating energy from heat reocovery [J]
			Real64 const HtRecLatHeatEnergy, // Latent   heating energy from heat reocovery [J]
			Real64 const HtRecTotHeatEnergy, // Total    heating energy from heat reocovery [J]
			Real64 const HtRecSenCoolEnergy, // Sensible cooling energy from heat reocovery [J]
			Real64 const HtRecLatCoolEnergy, // Latent   cooling energy from heat reocovery [J]
			Real64 const HtRecTotCoolEnergy, // Total    cooling energy from heat reocovery [J]
			Real64 const SenHeatRate, // Sensible heating rate consumed [W]
			Real64 const LatHeatRate, // Latent   heating rate consumed [W]
			Real64 const TotHeatRate, // Total    heating rate consumed [W]
			Real64 const SenCoolRate, // Sensible cooling rate consumed [W]
			Real64 const LatCoolRate, // Latent   cooling rate consumed [W]
			Real64 const TotCoolRate, // Total    cooling rate consumed [W]
			Real64 const ZoneSenHeatRate, // Sensible heating rate supplied to the zone [W]
			Real64 const ZoneLatHeatRate, // Latent   heating rate supplied to the zone [W]
			Real64 const ZoneTotHeatRate, // Total    heating rate supplied to the zone [W]
			Real64 const ZoneSenCoolRate, // Sensible cooling rate supplied to the zone [W]
			Real64 const ZoneLatCoolRate, // Latent   cooling rate supplied to the zone [W]
			Real64 const ZoneTotCoolRate, // Total    cooling rate supplied to the zone [W]
			Real64 const OASenHeatRate, // Sensible heating rate required for OA to equal zone air [W]
			Real64 const OALatHeatRate, // Latent   heating rate required for OA to equal zone air [W]
			Real64 const OATotHeatRate, // Total    heating rate required for OA to equal zone air [W]
			Real64 const OASenCoolRate, // Sensible cooling rate required for OA to equal zone air [W]
			Real64 const OALatCoolRate, // Latent   cooling rate required for OA to equal zone air [W]
			Real64 const OATotCoolRate, // Total    cooling rate required for OA to equal zone air [W]
			Real64 const HtRecSenHeatRate, // Sensible heating rate from heat reocovery [W]
			Real64 const HtRecLatHeatRate, // Latent   heating rate from heat reocovery [W]
			Real64 const HtRecTotHeatRate, // Total    heating rate from heat reocovery [W]
			Real64 const HtRecSenCoolRate, // Sensible cooling rate from heat reocovery [W]
			Real64 const HtRecLatCoolRate, // Latent   cooling rate from heat reocovery [W]
			Real64 const HtRecTotCoolRate, // Total    cooling rate from heat reocovery [W]
			Real64 const TimeEconoActive, // Time economizer is active [hrs]
			Real64 const TimeHtRecActive, // Time heat reocovery is active [hrs]
			int const ZonePtr, // pointer to a zone served by an Ideal load air system
			int const HVACSizingIndex // index of a HVAC Sizing object for an Ideal load air system
		) :
			cObjectName( cObjectName ),
			Name( Name ),
			AvailSched( AvailSched ),
			AvailSchedPtr( AvailSchedPtr ),
			ZoneSupplyAirNodeNum( ZoneSupplyAirNodeNum ),
			ZoneExhaustAirNodeNum( ZoneExhaustAirNodeNum ),
			ZoneRecircAirNodeNum( ZoneRecircAirNodeNum ),
			MaxHeatSuppAirTemp( MaxHeatSuppAirTemp ),
			MinCoolSuppAirTemp( MinCoolSuppAirTemp ),
			MaxHeatSuppAirHumRat( MaxHeatSuppAirHumRat ),
			MinCoolSuppAirHumRat( MinCoolSuppAirHumRat ),
			HeatingLimit( HeatingLimit ),
			MaxHeatVolFlowRate( MaxHeatVolFlowRate ),
			MaxHeatSensCap( MaxHeatSensCap ),
			CoolingLimit( CoolingLimit ),
			MaxCoolVolFlowRate( MaxCoolVolFlowRate ),
			MaxCoolTotCap( MaxCoolTotCap ),
			HeatSched( HeatSched ),
			HeatSchedPtr( HeatSchedPtr ),
			CoolSched( CoolSched ),
			CoolSchedPtr( CoolSchedPtr ),
			DehumidCtrlType( DehumidCtrlType ),
			CoolSHR( CoolSHR ),
			HumidCtrlType( HumidCtrlType ),
			OARequirementsPtr( OARequirementsPtr ),
			DCVType( DCVType ),
			EconomizerType( EconomizerType ),
			OutdoorAir( OutdoorAir ),
			OutdoorAirNodeNum( OutdoorAirNodeNum ),
			HtRecType( HtRecType ),
			HtRecSenEff( HtRecSenEff ),
			HtRecLatEff( HtRecLatEff ),
			OAFlowFracSchPtr( OAFlowFracSchPtr ),
			MaxHeatMassFlowRate( MaxHeatMassFlowRate ),
			MaxCoolMassFlowRate( MaxCoolMassFlowRate ),
			EMSOverrideMdotOn( EMSOverrideMdotOn ),
			EMSValueMassFlowRate( EMSValueMassFlowRate ),
			EMSOverrideOAMdotOn( EMSOverrideOAMdotOn ),
			EMSValueOAMassFlowRate( EMSValueOAMassFlowRate ),
			EMSOverrideSupplyTempOn( EMSOverrideSupplyTempOn ),
			EMSValueSupplyTemp( EMSValueSupplyTemp ),
			EMSOverrideSupplyHumRatOn( EMSOverrideSupplyHumRatOn ),
			EMSValueSupplyHumRat( EMSValueSupplyHumRat ),
			MinOAMassFlowRate( MinOAMassFlowRate ),
			OutdoorAirMassFlowRate( OutdoorAirMassFlowRate ),
			FinalMixedAirTemp( FinalMixedAirTemp ),
			FinalMixedAirHumRat( FinalMixedAirHumRat ),
			HtRecSenOutput( HtRecSenOutput ),
			HtRecLatOutput( HtRecLatOutput ),
			OASenOutput( OASenOutput ),
			OALatOutput( OALatOutput ),
			SenOutputToZone( SenOutputToZone ),
			LatOutputToZone( LatOutputToZone ),
			SenCoilLoad( SenCoilLoad ),
			LatCoilLoad( LatCoilLoad ),
			OAFlowMaxCoolOutputError( OAFlowMaxCoolOutputError ),
			OAFlowMaxHeatOutputError( OAFlowMaxHeatOutputError ),
			SaturationOutputError( SaturationOutputError ),
			OAFlowMaxCoolOutputIndex( OAFlowMaxCoolOutputIndex ),
			OAFlowMaxHeatOutputIndex( OAFlowMaxHeatOutputIndex ),
			SaturationOutputIndex( SaturationOutputIndex ),
			AvailStatus( AvailStatus ),
			CoolErrIndex( CoolErrIndex ),
			HeatErrIndex( HeatErrIndex ),
			SenHeatEnergy( SenHeatEnergy ),
			LatHeatEnergy( LatHeatEnergy ),
			TotHeatEnergy( TotHeatEnergy ),
			SenCoolEnergy( SenCoolEnergy ),
			LatCoolEnergy( LatCoolEnergy ),
			TotCoolEnergy( TotCoolEnergy ),
			ZoneSenHeatEnergy( ZoneSenHeatEnergy ),
			ZoneLatHeatEnergy( ZoneLatHeatEnergy ),
			ZoneTotHeatEnergy( ZoneTotHeatEnergy ),
			ZoneSenCoolEnergy( ZoneSenCoolEnergy ),
			ZoneLatCoolEnergy( ZoneLatCoolEnergy ),
			ZoneTotCoolEnergy( ZoneTotCoolEnergy ),
			OASenHeatEnergy( OASenHeatEnergy ),
			OALatHeatEnergy( OALatHeatEnergy ),
			OATotHeatEnergy( OATotHeatEnergy ),
			OASenCoolEnergy( OASenCoolEnergy ),
			OALatCoolEnergy( OALatCoolEnergy ),
			OATotCoolEnergy( OATotCoolEnergy ),
			HtRecSenHeatEnergy( HtRecSenHeatEnergy ),
			HtRecLatHeatEnergy( HtRecLatHeatEnergy ),
			HtRecTotHeatEnergy( HtRecTotHeatEnergy ),
			HtRecSenCoolEnergy( HtRecSenCoolEnergy ),
			HtRecLatCoolEnergy( HtRecLatCoolEnergy ),
			HtRecTotCoolEnergy( HtRecTotCoolEnergy ),
			SenHeatRate( SenHeatRate ),
			LatHeatRate( LatHeatRate ),
			TotHeatRate( TotHeatRate ),
			SenCoolRate( SenCoolRate ),
			LatCoolRate( LatCoolRate ),
			TotCoolRate( TotCoolRate ),
			ZoneSenHeatRate( ZoneSenHeatRate ),
			ZoneLatHeatRate( ZoneLatHeatRate ),
			ZoneTotHeatRate( ZoneTotHeatRate ),
			ZoneSenCoolRate( ZoneSenCoolRate ),
			ZoneLatCoolRate( ZoneLatCoolRate ),
			ZoneTotCoolRate( ZoneTotCoolRate ),
			OASenHeatRate( OASenHeatRate ),
			OALatHeatRate( OALatHeatRate ),
			OATotHeatRate( OATotHeatRate ),
			OASenCoolRate( OASenCoolRate ),
			OALatCoolRate( OALatCoolRate ),
			OATotCoolRate( OATotCoolRate ),
			HtRecSenHeatRate( HtRecSenHeatRate ),
			HtRecLatHeatRate( HtRecLatHeatRate ),
			HtRecTotHeatRate( HtRecTotHeatRate ),
			HtRecSenCoolRate( HtRecSenCoolRate ),
			HtRecLatCoolRate( HtRecLatCoolRate ),
			HtRecTotCoolRate( HtRecTotCoolRate ),
			TimeEconoActive( TimeEconoActive ),
			TimeHtRecActive( TimeHtRecActive ),
			ZonePtr( ZonePtr ),
			HVACSizingIndex( HVACSizingIndex )
		{}

	};

	struct PurchAirNumericFieldData
	{
		// Members
		Array1D_string FieldNames;

		// Default Constructor
		PurchAirNumericFieldData()
		{}

		// Member Constructor
		PurchAirNumericFieldData(
			Array1_string const & FieldNames // Name of the HeatingCoil numeric field descriptions
			) :
			FieldNames(FieldNames)
		{}
	};

	// Object Data
	extern Array1D< ZonePurchasedAir > PurchAir; // Used to specify purchased air parameters
	extern Array1D< PurchAirNumericFieldData > PurchAirNumericFields; // Used to save the indecies of scalable sizing object for zone HVAC

	// Functions

	void
	SimPurchasedAir(
		std::string const & PurchAirName,
		Real64 & SysOutputProvided,
		Real64 & MoistOutputProvided, // Moisture output provided (kg/s), dehumidification = negative
		bool const FirstHVACIteration,
		int const ControlledZoneNum,
		int const ActualZoneNum,
		int & CompIndex
	);

	void
	GetPurchasedAir();

	void
	InitPurchasedAir(
		int const PurchAirNum,
		bool const FirstHVACIteration, // unused1208
		int const ControlledZoneNum,
		int const ActualZoneNum
	);

	void
	SizePurchasedAir( int const PurchAirNum );

	void
	CalcPurchAirLoads(
		int const PurchAirNum,
		Real64 & SysOutputProvided, // Sensible output provided [W] cooling = negative
		Real64 & MoistOutputProvided, // Moisture output provided [kg/s] dehumidification = negative
		int const ControlledZoneNum,
		int const ActualZoneNum
	);

	void
	CalcPurchAirMinOAMassFlow(
		int const PurchAirNum, // index to ideal loads unit
		int const ActualZoneNum, // index to actual zone number
		Real64 & OAMassFlowRate // outside air mass flow rate [kg/s] from volume flow using std density
	);

	void
	CalcPurchAirMixedAir(
		int const PurchAirNum, // index to ideal loads unit
		Real64 const OAMassFlowRate, // outside air mass flow rate [kg/s]
		Real64 const SupplyMassFlowRate, // supply air mass flow rate [kg/s]
		Real64 & MixedAirTemp, // Mixed air dry bulb temperature [C]
		Real64 & MixedAirHumRat, // Mixed air humidity ratio [kg H2O/kg Air]
		Real64 & MixedAirEnthalpy, // Mixed air enthalpy [J/kg]
		int const OperatingMode // current operating mode, Off, Heating, Cooling, or DeadBand
	);

	void
	UpdatePurchasedAir( int const PurchAirNum );

	void
	ReportPurchasedAir( int const PurchAirNum );

	Real64
	GetPurchasedAirOutAirMassFlow( int const PurchAirNum );

	int
	GetPurchasedAirZoneInletAirNode( int const PurchAirNum );

	int
	GetPurchasedAirReturnAirNode( int const PurchAirNum );

	Real64
	GetPurchasedAirMixedAirTemp( int const PurchAirNum );

	Real64
	GetPurchasedAirMixedAirHumRat( int const PurchAirNum );

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

} // PurchasedAirManager

} // EnergyPlus

#endif
