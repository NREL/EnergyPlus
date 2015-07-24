#ifndef RefrigeratedCase_hh_INCLUDED
#define RefrigeratedCase_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace RefrigeratedCase {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:

	// Anti-sweat heater control type
	extern int const ASNone;
	extern int const ASConstant;
	extern int const ASLinear;
	extern int const ASDewPoint;
	extern int const ASHeatBalance;
	// Refrigerated display case defrost type
	extern int const DefNone;
	extern int const DefOffCycle;
	extern int const DefHotFluid;
	extern int const DefHotFluidOnDemand;
	extern int const DefHotFluidTerm;
	extern int const DefElectric;
	extern int const DefElectricOnDemand;
	extern int const DefElectricTerm;

	// Refrigerated display case rack heat rejection location
	extern int const LocationOutdoors;
	extern int const LocationZone;
	// Condenser cooling type -- See DataHeatBalance - RefrigxxxTypexxx
	//INTEGER, PARAMETER :: CondenserCoolingAir   = 1
	//INTEGER, PARAMETER :: CondenserCoolingEvap  = 2
	//INTEGER, PARAMETER :: CondenserCoolingWater = 3
	//INTEGER, PARAMETER :: CondenserCascade      = 4
	// Air- and evap-cooled condenser fan speed control types
	extern int const FanVariableSpeed;
	extern int const FanConstantSpeedLinear;
	extern int const FanTwoSpeed;
	extern int const FanConstantSpeed;
	// Water-cooled condenser loop flow type
	extern int const VariableFlow;
	extern int const ConstantFlow;
	// Condenser evap cooling water supply
	extern int const WaterSupplyFromMains;
	extern int const WaterSupplyFromTank;
	// Cascade condenser temperature control types
	extern int const CascadeTempSet;
	extern int const CascadeTempFloat;
	// Refrigerated display case energy equation form
	extern int const None;
	extern int const CaseTemperatureMethod;
	extern int const RHCubic;
	extern int const DPCubic;
	// Secondary loop parameters
	extern int const SecFluidTypeAlwaysLiquid;
	extern int const SecFluidTypePhaseChange;
	extern int const SecPumpControlConstant;
	extern int const SecPumpControlVariable;
	// Walk In Cooler Defrost type
	extern int const WalkInDefrostFluid;
	extern int const WalkInDefrostElec;
	extern int const WalkInDefrostNone;
	extern int const WalkInDefrostOffCycle;
	// Walk In Cooler Defrost Control type
	extern int const DefrostControlSched;
	extern int const DefrostContTempTerm;
	// Walk In Cooler Stock Door Protection types
	extern int const WIStockDoorNone;
	extern int const WIStockDoorAirCurtain;
	extern int const WIStockDoorStripCurtain;
	// Subcooler type
	extern int const LiquidSuction;
	extern int const Mechanical;
	// Compressor suction pressure control
	extern int const FloatSuctionTemperature;
	extern int const ConstantSuctionTemperature;
	//Compressor rating types
	extern int const RatedSuperheat;
	extern int const RatedReturnGasTemperature;
	extern int const RatedSubcooling;
	extern int const RatedLiquidTemperature;
	// System service types (applies to system, cascade condenser, and secondary loops)
	//INTEGER, PARAMETER :: SupermarketService =1
	//INTEGER, PARAMETER :: WarehouseService   =2
	// Warehouse coil Defrost type
	extern int const DefrostFluid;
	extern int const DefrostElec;
	extern int const DefrostNone;
	extern int const DefrostOffCycle;

	extern int const RatedCapacityTotal;
	extern int const EuropeanSC1Std;
	extern int const EuropeanSC1Nom;
	extern int const EuropeanSC2Std;
	extern int const EuropeanSC2Nom;
	extern int const EuropeanSC3Std;
	extern int const EuropeanSC3Nom;
	extern int const EuropeanSC4Std;
	extern int const EuropeanSC4Nom;
	extern int const EuropeanSC5Std;
	extern int const EuropeanSC5Nom;
	extern int const UnitLoadFactorSens;
	extern int const SHR60;
	extern int const QuadraticSHR;
	extern int const European;
	extern int const TabularRH_DT1_TRoom;
	extern int const Ceiling;
	extern int const Middle;
	extern int const Floor;
	extern int const DetailedSystem;
	extern int const SecondarySystem;

	// Following constant approp for R22, future may make f(refrigerant)
	extern Real64 const CaseSuperheat; // case superheat used to control thermal expansion valve, ASHRAE 2006 p 44.6 (C)
	extern Real64 const TransCaseSuperheat; // case superheat for transcritical CO2 systems (C)
	// Next two constants used to autosize evap condenser
	extern Real64 const CondPumpRatePower; // evap condenser pump rated, Wpump/Wcapacity (15 W/ton)
	extern Real64 const AirVolRateEvapCond; // evap cond air flow rate for autosize, equiv 850 cfm/ton (m3/W-s)
	extern Real64 const EvapCutOutTdb; // shut off evap water flow if outdoor drybulb < evapcutOutTdb (C)
	// Miscellaneous constants
	extern Real64 const MyLargeNumber;
	extern Real64 const MySmallNumber;
	extern Real64 const Rair; // Air resistance used with Heat Balance anti-sweat (AS) heater
	extern Real64 const IceMeltEnthalpy; // heat of fusion of water J/kg
	extern Real64 const TempTooHotToFrost; // C, used to check for frosting conditions on evaporator coils
	extern Real64 const IcetoVaporEnthalpy; // J/kg to freeze water vapor to ice
	extern Real64 const WatertoVaporEnthalpy; // at 0C
	extern Real64 const SpecificHeatIce; // in the likely range (2040 at 0C and 1950 at -20C) (J/kg-C)
	extern Real64 const CondAirVolExponentDry; // exponent for forced air over a cylinder, = 1/.633
	//per ASHRAE 2005 (page 3.15)
	extern Real64 const CondAirVolExponentEvap; // exponent for evap condenser air vol flow, = 1/.76
	//per Manske, 1999
	extern Real64 const EvaporatorAirVolExponent; // exponent for evapaporator air vol flow, = 1/.65
	//per Manske, 1999, page 35

	extern Real64 const FanHalfSpeedRatio; // = 1/(2**2.5) for power step for two speed fan
	extern Real64 const CapFac60Percent; // = 60%, load served by half power 2-speed fan

	extern Array1D< Real64 > const EuropeanWetCoilFactor;
	extern Array1D< Real64 > const EuropeanAirInletTemp;
	extern Array1D< Real64 > const EuropeanEvapTemp;
	extern Array1D< Real64 > const EuropeanDT1;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:

	extern int NumSimulationCondAir; // Number of air-cooled condensers in simulation
	extern int NumSimulationCondEvap; // Number of evaporative condensers in simulation
	extern int NumSimulationCondWater; // Number of water-cooled condensers in simulation
	extern int NumSimulationCascadeCondensers; // Total number of Cascade condensers in IDF
	extern int NumSimulationGasCooler; // Number of gas coolers in simulation
	extern int NumSimulationSharedGasCoolers; // Total number of gas coolers that serve more than one system
	extern int NumTransRefrigSystems; // Total number of transcritical CO2 refrigeration systems
	extern int NumSimulationSharedCondensers; // Total number of condensers that serve more than one system
	extern int NumSimulationCases; // Number of refrigerated cases in simulation
	extern int NumSimulationCaseAndWalkInLists; // Total number of CaseAndWalkIn Lists in IDF
	extern int NumSimulationWalkIns; // Number of walk in coolers in simulation
	extern int NumSimulationCompressors; // Number of refrigeration compressors in simulation
	extern int NumSimulationSubcoolers; // Number of refrigeration subcoolers in simulation
	extern int NumSimulationMechSubcoolers; // Number of mechanical subcoolers in simulation
	extern int NumSimulationRefrigAirChillers; // Number of individual Air Chillers/coils in simulation
	extern int NumSimulationSecondarySystems; // Number of Secondary loops in simulation
	extern int NumSimulationTransferLoadLists; // Number of Secondary Lists in simulation
	extern int NumUnusedRefrigCases; // Number of refrigerated cases not connected to a rack or system
	extern int NumUnusedCoils; // Number of refrigeration air coils not connected to a rack or system
	extern int NumUnusedCondensers; // Number of refrigeration condensors not connected to a system
	extern int NumUnusedGasCoolers; // Number of refrigeration gas coolers not connected to a system
	extern int NumUnusedCompressors; // Number of refrigeration compressors not connected to a system
	extern int NumUnusedSecondarys; // Number of refrigeration secondarys not connected to a system
	extern int NumUnusedWalkIns; // Number of refrigeration compressors not connected to a system
	extern bool MyReferPlantScanFlag;

	// Refrigerated case variables
	extern Real64 CaseRAFactor; // Factor determining case credit allocation (e.g. % to zone or HVAC)
	extern Array1D_bool ShowStockingWarning; // Used for one-time warning message for possible case
	// input error regarding stocking
	extern Array1D_bool ShowFrostWarning; // Used for one-time warning message for possible case
	// input error regarding frost
	extern Array1D_bool ShowStoreEnergyWarning; // Used for one-time warning message for possible case
	// input error regarding defrost or stocking
	//  Walk In variables
	extern Array1D_bool ShowUnmetWIEnergyWarning; // Used for one-time warning message
	extern Array1D_bool ShowWIFrostWarning; // Used for one-time warning message

	// Refrigeration compressor rack variables
	extern Real64 TotalRackDeliveredCapacity; // Total capacity of all refrigerated cases attached to rack (W)
	extern Real64 TotalCompressorPower; // Total compressor electric power (W)
	extern Real64 CompressorCOPactual; // Compressor coefficient of performance at specific operating conditions (W/W)
	extern Real64 RackSenCreditToZone; // Amount of condenser heat applied to zone load (W)
	extern Real64 RackSenCreditToHVAC; // Amount of condenser heat applied to HVAC RA duct (W)
	extern int InletNode; // Water-cooled condenser inlet node number
	extern int OutletNode; // Water-cooled condenser outlet node number
	extern Array1D_bool ShowCOPWarning; // Used for one-time warning message for possible rack
	// input error regarding COP
	// Refrigeration condenser variables (used for both racks and detailed systems)
	extern Real64 TotalCondenserFanPower; // Total condenser fan electric power (W)
	extern Real64 TotalCondenserPumpPower; // Total condenser pump electric power (W)
	extern Real64 TotalCondenserHeat; // Total condenser heat from compressor rack (W)
	extern Real64 TotalBasinHeatPower; // Total condenser basin water heater power (W)
	extern Real64 TotalEvapWaterUseRate; // Total condenser water use rate (m3/s)

	// Refrigeration system variables
	extern Array1D_bool ShowUnmetEnergyWarning; // Used for one-time warning message for possible
	//compressor input error regarding total system compressor capacity
	extern Array1D_bool ShowHiStageUnmetEnergyWarning; // Used for one-time warning message for possible
	//high-stage compressor input error regarding high-stage compressor capacity

	// Transcritical refrigeration system variables
	extern bool TransCritSysFlag; // Used to indicate whether or not a transcritical refrigeration system has been defined.
	extern Array1D_bool ShowUnmetEnergyWarningTrans; // Used for one-time warning message for possible
	//compressor input error regarding total system compressor capacity

	// Refrigeration Secondary Loop variables
	extern Array1D_bool ShowUnmetSecondEnergyWarning; // Used for one-time warning message for possible
	//compressor input error regarding secondary loop heat exchanger capacity
	//Refrigerated warehouse coil variables
	extern Array1D_bool CheckChillerName; // used when simrefrigcoil called for a zone
	//LOGICAL, ALLOCATABLE,DIMENSION(:) :: CheckZoneNum  !used when simrefrigcoil called for a zone
	extern Array1D_bool ShowCoilFrostWarning; // Used for one-time warning message if defrost cycles insufficient to melt ice

	// Refrigeration Plant connections checks
	extern Array1D_bool CheckEquipNameRackWaterCondenser;
	extern Array1D_bool CheckEquipNameWaterCondenser;

	//Control variables
	extern Array1D_bool RefrigPresentInZone; // Used when translating rate to energy for reporting
	//  total refrigeration impact on a zone
	extern Array1D_bool CheckChillerSetName; // used when sim chiller set called form zone equip manager

	extern bool GetRefrigerationInputFlag; // Flag to show case input should be read
	extern bool HaveRefrigRacks; // Is initialized as TRUE and remains true when
	// refrigerated racks exist in the input deck
	extern bool HaveDetailedRefrig; // Is initialized as TRUE and remains true when
	// detailed refrigeration systems exist in the input deck
	extern bool HaveDetailedTransRefrig; // Is initialized as TRUE and remains true when
	// detailed transcritical CO2 refrigeration systems exist in the input deck
	extern bool ManageRefrigeration; // Is initialized as TRUE and remains true when
	// refrigerated racks or detailed systems exist in the input deck
	extern bool UseSysTimeStep; // Flag is true IF working on a system that includes a coil cooling a controlled zone on the system time step,
	// All other refrigeration calculations for case and walkin systems done on the load time step
	extern bool HaveCasesOrWalkins; // Is initialized as TRUE and remains true when
	// refrigerated cases or walkins exist in the input deck
	extern bool HaveChillers; // Is initialized as TRUE and remains true when
	// chillers exist in the input deck
	// SUBROUTINE SPECIFICATIONS FOR MODULE RefrigeratedCase:

	// Types

	struct RefrigCaseData
	{
		// Members
		std::string Name; // Name of refrigerated display case
		// CHARACTER(len=MaxNameLength) :: Schedule=' '               ! Display case availability schedule name
		std::string ZoneName; // Zone or Location of Display Case
		int NumSysAttach; // Number of systems attached to case, error if /=1
		int SchedPtr; // Index to the correct availability schedule
		int ZoneNodeNum; // Index to Zone Node
		int ActualZoneNum; // Index to Zone
		int ZoneRANode; // Node number of return node in zone
		Real64 RatedAmbientTemp; // Rated ambient (zone) temperature
		Real64 RatedAmbientRH; // Rated ambient (zone) relative humidity
		Real64 RatedAmbientDewPoint; // Rated ambient (zone) dew point temperature
		Real64 RateTotCapPerLength; // Gross total cooling capacity at rated conditions [W/m]
		Real64 RatedLHR; // Latent heat ratio (lat cap/total cap) at rated conditions
		Real64 RatedRTF; // Run time fraction at rated conditions
		int LatCapCurvePtr; // Index for latent case credit modifier curve
		int DefCapCurvePtr; // Index for defrost load modifier curve
		int LatentEnergyCurveType; // Type of latent case credit curve:
		// 1=Case Temperature Method, 2=RH cubic, 3=DP cubic
		int DefrostEnergyCurveType; // Type of defrost energy curve:
		// 1=Case Temperature Method, 2=RH cubic, 3=DP cubic
		Real64 STDFanPower; // Standard power of case fan [W/m] for case credit calc
		Real64 OperatingFanPower; // Operating power of refrigerated case fan [W/m]
		Real64 RatedLightingPower; // Rated (consis w RateTotCapPerLength) power of refrigerated case lights [W/m]
		Real64 LightingPower; // Installed power of refrigerated case lights [W/m]
		int LightingSchedPtr; // Index to the correct case lighting schedule
		Real64 AntiSweatPower; // Rated power of refrigerated case anti-sweat heaters [W/m]
		Real64 MinimumASPower; // Minimum power output of case anti-sweat heaters [W/m]
		int AntiSweatControlType; // Type of anti-sweat heater control:
		// 0=None,1=Constant,2=Linear,3=DewPoint,4=HeatBalance
		Real64 HumAtZeroAS; // Relative humidity for zero AS heater output using linear control
		Real64 Height; // case height for AS heater with heat balance control
		int DefrostType; // Case defrost control type, Off-cycle,Timed,Hot-gas,Electric
		Real64 DefrostPower; // Rated power of refrigerated case defrost [W/m]
		int DefrostSchedPtr; // Index to the correct defrost schedule
		int DefrostDripDownSchedPtr; // Index to the correct fail-safe schedule
		Real64 Length; // Length of refrigerated case [m]
		Real64 Temperature; // Rated case temperature [C]
		Real64 RAFrac; // HVAC under case return air fraction [0-1]
		int StockingSchedPtr; // Index to the correct product stocking schedule
		Real64 LightingFractionToCase; // Fraction of lighting energy that directly contributes to the
		// case cooling load. The remainder contributes to the zone load
		// (air heat balance).
		Real64 ASHeaterFractionToCase; // Fraction of anti-sweat heater energy that results in a direct
		// heat load to the case. The remainder is a heating load
		// to the zone where the refrigerated case is located.
		Real64 DesignSensCaseCredit; // Design sensible case credit applied to zone load
		Real64 EvapTempDesign; // Design evaporator temperature
		Real64 RefrigInventory; // Design refrigerant inventory [kg/m]
		Real64 DesignRefrigInventory; // Design refrigerant inventory [kg total for the case]
		Real64 DesignRatedCap; // Design total case capacity=RatedTotCap*Length [W]
		Real64 DesignLatentCap; // Design latent case capacity=DesignRAtedCap*LatentHeatRatio*RTF [W]
		Real64 DesignDefrostCap; // Design defrost case capacity=DefrostPower*Length [W]
		Real64 DesignLighting; // Design case lighting=LightingPower*Length [W]
		Real64 DesignFanPower; // Design power of case fan=Operatingpower*Length [W]
		Real64 StoredEnergy; // Cumulative Stored Energy not met by evaporator [J]
		Real64 StoredEnergySaved; // Cumulative Stored Energy not met by evaporator [J]
		int CaseCreditFracSchedPtr; // Index to the case credit reduction schedule
		// Report Variables
		Real64 TotalCoolingLoad; // Refrigerated case total cooling rate (W)
		Real64 TotalCoolingEnergy; // Refrigerated case total cooling energy (J)
		Real64 SensCoolingEnergyRate; // Refrigerated case sensible cooling rate (W)
		Real64 SensCoolingEnergy; // Refrigerated case sensible cooling energy (J)
		Real64 LatCoolingEnergyRate; // Refrigerated case latent cooling rate (W)
		Real64 LatCoolingEnergy; // Refrigerated case latent cooling energy (J)
		Real64 SensZoneCreditRate; // Refrigerated case sensible zone credit rate (W)
		Real64 SensZoneCreditCoolRate; // Refrigerated case sensible cooling zone credit rate (W)
		Real64 SensZoneCreditCool; // Refrigerated case sensible cooling zone credit energy (J)
		Real64 SensZoneCreditHeatRate; // Refrigerated case sensible heating zone credit rate (W)
		Real64 SensZoneCreditHeat; // Refrigerated case sensible heating zone credit energy (J)
		Real64 LatZoneCreditRate; // Refrigerated case latent zone credit rate (W)
		Real64 LatZoneCredit; // Refrigerated case latent zone credit energy (J)
		Real64 SensHVACCreditRate; // Refrigerated case sensible HVAC credit rate (W)
		Real64 SensHVACCreditCoolRate; // Refrigerated case sensible cooling HVAC credit rate (W)
		Real64 SensHVACCreditCool; // Refrigerated case sensible cooling HVAC credit energy (J)
		Real64 SensHVACCreditHeatRate; // Refrigerated case sensible heating HVAC credit rate (W)
		Real64 SensHVACCreditHeat; // Refrigerated case sensible heating HVAC credit energy (J)
		Real64 LatHVACCreditRate; // Refrigerated case latent HVAC credit rate (W)
		Real64 LatHVACCredit; // Refrigerated case latent HVAC credit energy (J)
		Real64 ElecAntiSweatPower; // Refrigerated case anti-sweat heater rate (W)
		Real64 ElecAntiSweatConsumption; // Refrigerated case anti-sweat heater energy (J)
		Real64 ElecFanPower; // Refrigerated case fan electric power (W)
		Real64 ElecFanConsumption; // Refrigerated case fan electric energy (J)
		Real64 ElecLightingPower; // Refrigerated case lighting electric power (W)
		Real64 ElecLightingConsumption; // Refrigerated case lighting electric energy (J)
		Real64 ElecDefrostPower; // Refrigerated case defrost rate (W)
		Real64 ElecDefrostConsumption; // Refrigerated case defrost energy (J)
		Real64 DefEnergyCurveValue; // Refrigerated case defrost capacity modifier
		Real64 LatEnergyCurveValue; // Refrigerated case latent capacity modifier
		Real64 MaxKgFrost; // Amount of frost formation to initiate defrost for On Demand
		Real64 Rcase; // Case wall resistance for AS heater calc (h-sqm-C/W)
		Real64 DefrostEnergy; // Refrigerated case defrost energy (J)
		Real64 StockingEnergy; // Refrigerated case product stocking energy (J)
		Real64 WarmEnvEnergy; // Refrigerated case extra sensible energy due to warm zone ambient (J)
		Real64 KgFrost; // Amount of frost on case evaporator (Kg)
		Real64 DefrostEnergySaved; // Refrigerated case defrost energy (J)
		Real64 StockingEnergySaved; // Refrigerated case product stocking energy (J)
		Real64 WarmEnvEnergySaved; // Refrigerated case extra sensible energy due to warm zone ambient (J)
		Real64 KgFrostSaved; // Amount of frost on case evaporator (Kg)
		Real64 HotDefrostCondCredit; // Used to credit condenser when heat reclaim used for hot gas/brine defrost (W)
		Real64 DeltaDefrostEnergy; // Used to reverse accumulation if the zone/load time step is repeated (J)

		// Default Constructor
		RefrigCaseData() :
			NumSysAttach( 0 ),
			SchedPtr( 0 ),
			ZoneNodeNum( 0 ),
			ActualZoneNum( 0 ),
			ZoneRANode( 0 ),
			RatedAmbientTemp( 0.0 ),
			RatedAmbientRH( 0.0 ),
			RatedAmbientDewPoint( 0.0 ),
			RateTotCapPerLength( 0.0 ),
			RatedLHR( 0.0 ),
			RatedRTF( 0.0 ),
			LatCapCurvePtr( 0 ),
			DefCapCurvePtr( 0 ),
			LatentEnergyCurveType( 0 ),
			DefrostEnergyCurveType( 0 ),
			STDFanPower( 0.0 ),
			OperatingFanPower( 0.0 ),
			RatedLightingPower( 0.0 ),
			LightingPower( 0.0 ),
			LightingSchedPtr( 0 ),
			AntiSweatPower( 0.0 ),
			MinimumASPower( 0.0 ),
			AntiSweatControlType( 0 ),
			HumAtZeroAS( 0.0 ),
			Height( 0.0 ),
			DefrostType( 0 ),
			DefrostPower( 0.0 ),
			DefrostSchedPtr( 0 ),
			DefrostDripDownSchedPtr( 0 ),
			Length( 0.0 ),
			Temperature( 0.0 ),
			RAFrac( 0.0 ),
			StockingSchedPtr( 0 ),
			LightingFractionToCase( 0.0 ),
			ASHeaterFractionToCase( 0.0 ),
			DesignSensCaseCredit( 0.0 ),
			EvapTempDesign( 0.0 ),
			RefrigInventory( 0.0 ),
			DesignRefrigInventory( 0.0 ),
			DesignRatedCap( 0.0 ),
			DesignLatentCap( 0.0 ),
			DesignDefrostCap( 0.0 ),
			DesignLighting( 0.0 ),
			DesignFanPower( 0.0 ),
			StoredEnergy( 0.0 ),
			StoredEnergySaved( 0.0 ),
			CaseCreditFracSchedPtr( 0 ),
			TotalCoolingLoad( 0.0 ),
			TotalCoolingEnergy( 0.0 ),
			SensCoolingEnergyRate( 0.0 ),
			SensCoolingEnergy( 0.0 ),
			LatCoolingEnergyRate( 0.0 ),
			LatCoolingEnergy( 0.0 ),
			SensZoneCreditRate( 0.0 ),
			SensZoneCreditCoolRate( 0.0 ),
			SensZoneCreditCool( 0.0 ),
			SensZoneCreditHeatRate( 0.0 ),
			SensZoneCreditHeat( 0.0 ),
			LatZoneCreditRate( 0.0 ),
			LatZoneCredit( 0.0 ),
			SensHVACCreditRate( 0.0 ),
			SensHVACCreditCoolRate( 0.0 ),
			SensHVACCreditCool( 0.0 ),
			SensHVACCreditHeatRate( 0.0 ),
			SensHVACCreditHeat( 0.0 ),
			LatHVACCreditRate( 0.0 ),
			LatHVACCredit( 0.0 ),
			ElecAntiSweatPower( 0.0 ),
			ElecAntiSweatConsumption( 0.0 ),
			ElecFanPower( 0.0 ),
			ElecFanConsumption( 0.0 ),
			ElecLightingPower( 0.0 ),
			ElecLightingConsumption( 0.0 ),
			ElecDefrostPower( 0.0 ),
			ElecDefrostConsumption( 0.0 ),
			DefEnergyCurveValue( 0.0 ),
			LatEnergyCurveValue( 0.0 ),
			MaxKgFrost( 0.0 ),
			Rcase( 0.0 ),
			DefrostEnergy( 0.0 ),
			StockingEnergy( 0.0 ),
			WarmEnvEnergy( 0.0 ),
			KgFrost( 0.0 ),
			DefrostEnergySaved( 0.0 ),
			StockingEnergySaved( 0.0 ),
			WarmEnvEnergySaved( 0.0 ),
			KgFrostSaved( 0.0 ),
			HotDefrostCondCredit( 0.0 ),
			DeltaDefrostEnergy( 0.0 )
		{}

		// Member Constructor
		RefrigCaseData(
			std::string const & Name, // Name of refrigerated display case
			std::string const & ZoneName, // Zone or Location of Display Case
			int const NumSysAttach, // Number of systems attached to case, error if /=1
			int const SchedPtr, // Index to the correct availability schedule
			int const ZoneNodeNum, // Index to Zone Node
			int const ActualZoneNum, // Index to Zone
			int const ZoneRANode, // Node number of return node in zone
			Real64 const RatedAmbientTemp, // Rated ambient (zone) temperature
			Real64 const RatedAmbientRH, // Rated ambient (zone) relative humidity
			Real64 const RatedAmbientDewPoint, // Rated ambient (zone) dew point temperature
			Real64 const RateTotCapPerLength, // Gross total cooling capacity at rated conditions [W/m]
			Real64 const RatedLHR, // Latent heat ratio (lat cap/total cap) at rated conditions
			Real64 const RatedRTF, // Run time fraction at rated conditions
			int const LatCapCurvePtr, // Index for latent case credit modifier curve
			int const DefCapCurvePtr, // Index for defrost load modifier curve
			int const LatentEnergyCurveType, // Type of latent case credit curve:
			int const DefrostEnergyCurveType, // Type of defrost energy curve:
			Real64 const STDFanPower, // Standard power of case fan [W/m] for case credit calc
			Real64 const OperatingFanPower, // Operating power of refrigerated case fan [W/m]
			Real64 const RatedLightingPower, // Rated (consis w RateTotCapPerLength) power of refrigerated case lights [W/m]
			Real64 const LightingPower, // Installed power of refrigerated case lights [W/m]
			int const LightingSchedPtr, // Index to the correct case lighting schedule
			Real64 const AntiSweatPower, // Rated power of refrigerated case anti-sweat heaters [W/m]
			Real64 const MinimumASPower, // Minimum power output of case anti-sweat heaters [W/m]
			int const AntiSweatControlType, // Type of anti-sweat heater control:
			Real64 const HumAtZeroAS, // Relative humidity for zero AS heater output using linear control
			Real64 const Height, // case height for AS heater with heat balance control
			int const DefrostType, // Case defrost control type, Off-cycle,Timed,Hot-gas,Electric
			Real64 const DefrostPower, // Rated power of refrigerated case defrost [W/m]
			int const DefrostSchedPtr, // Index to the correct defrost schedule
			int const DefrostDripDownSchedPtr, // Index to the correct fail-safe schedule
			Real64 const Length, // Length of refrigerated case [m]
			Real64 const Temperature, // Rated case temperature [C]
			Real64 const RAFrac, // HVAC under case return air fraction [0-1]
			int const StockingSchedPtr, // Index to the correct product stocking schedule
			Real64 const LightingFractionToCase, // Fraction of lighting energy that directly contributes to the
			Real64 const ASHeaterFractionToCase, // Fraction of anti-sweat heater energy that results in a direct
			Real64 const DesignSensCaseCredit, // Design sensible case credit applied to zone load
			Real64 const EvapTempDesign, // Design evaporator temperature
			Real64 const RefrigInventory, // Design refrigerant inventory [kg/m]
			Real64 const DesignRefrigInventory, // Design refrigerant inventory [kg total for the case]
			Real64 const DesignRatedCap, // Design total case capacity=RatedTotCap*Length [W]
			Real64 const DesignLatentCap, // Design latent case capacity=DesignRAtedCap*LatentHeatRatio*RTF [W]
			Real64 const DesignDefrostCap, // Design defrost case capacity=DefrostPower*Length [W]
			Real64 const DesignLighting, // Design case lighting=LightingPower*Length [W]
			Real64 const DesignFanPower, // Design power of case fan=Operatingpower*Length [W]
			Real64 const StoredEnergy, // Cumulative Stored Energy not met by evaporator [J]
			Real64 const StoredEnergySaved, // Cumulative Stored Energy not met by evaporator [J]
			int const CaseCreditFracSchedPtr, // Index to the case credit reduction schedule
			Real64 const TotalCoolingLoad, // Refrigerated case total cooling rate (W)
			Real64 const TotalCoolingEnergy, // Refrigerated case total cooling energy (J)
			Real64 const SensCoolingEnergyRate, // Refrigerated case sensible cooling rate (W)
			Real64 const SensCoolingEnergy, // Refrigerated case sensible cooling energy (J)
			Real64 const LatCoolingEnergyRate, // Refrigerated case latent cooling rate (W)
			Real64 const LatCoolingEnergy, // Refrigerated case latent cooling energy (J)
			Real64 const SensZoneCreditRate, // Refrigerated case sensible zone credit rate (W)
			Real64 const SensZoneCreditCoolRate, // Refrigerated case sensible cooling zone credit rate (W)
			Real64 const SensZoneCreditCool, // Refrigerated case sensible cooling zone credit energy (J)
			Real64 const SensZoneCreditHeatRate, // Refrigerated case sensible heating zone credit rate (W)
			Real64 const SensZoneCreditHeat, // Refrigerated case sensible heating zone credit energy (J)
			Real64 const LatZoneCreditRate, // Refrigerated case latent zone credit rate (W)
			Real64 const LatZoneCredit, // Refrigerated case latent zone credit energy (J)
			Real64 const SensHVACCreditRate, // Refrigerated case sensible HVAC credit rate (W)
			Real64 const SensHVACCreditCoolRate, // Refrigerated case sensible cooling HVAC credit rate (W)
			Real64 const SensHVACCreditCool, // Refrigerated case sensible cooling HVAC credit energy (J)
			Real64 const SensHVACCreditHeatRate, // Refrigerated case sensible heating HVAC credit rate (W)
			Real64 const SensHVACCreditHeat, // Refrigerated case sensible heating HVAC credit energy (J)
			Real64 const LatHVACCreditRate, // Refrigerated case latent HVAC credit rate (W)
			Real64 const LatHVACCredit, // Refrigerated case latent HVAC credit energy (J)
			Real64 const ElecAntiSweatPower, // Refrigerated case anti-sweat heater rate (W)
			Real64 const ElecAntiSweatConsumption, // Refrigerated case anti-sweat heater energy (J)
			Real64 const ElecFanPower, // Refrigerated case fan electric power (W)
			Real64 const ElecFanConsumption, // Refrigerated case fan electric energy (J)
			Real64 const ElecLightingPower, // Refrigerated case lighting electric power (W)
			Real64 const ElecLightingConsumption, // Refrigerated case lighting electric energy (J)
			Real64 const ElecDefrostPower, // Refrigerated case defrost rate (W)
			Real64 const ElecDefrostConsumption, // Refrigerated case defrost energy (J)
			Real64 const DefEnergyCurveValue, // Refrigerated case defrost capacity modifier
			Real64 const LatEnergyCurveValue, // Refrigerated case latent capacity modifier
			Real64 const MaxKgFrost, // Amount of frost formation to initiate defrost for On Demand
			Real64 const Rcase, // Case wall resistance for AS heater calc (h-sqm-C/W)
			Real64 const DefrostEnergy, // Refrigerated case defrost energy (J)
			Real64 const StockingEnergy, // Refrigerated case product stocking energy (J)
			Real64 const WarmEnvEnergy, // Refrigerated case extra sensible energy due to warm zone ambient (J)
			Real64 const KgFrost, // Amount of frost on case evaporator (Kg)
			Real64 const DefrostEnergySaved, // Refrigerated case defrost energy (J)
			Real64 const StockingEnergySaved, // Refrigerated case product stocking energy (J)
			Real64 const WarmEnvEnergySaved, // Refrigerated case extra sensible energy due to warm zone ambient (J)
			Real64 const KgFrostSaved, // Amount of frost on case evaporator (Kg)
			Real64 const HotDefrostCondCredit, // Used to credit condenser when heat reclaim used for hot gas/brine defrost (W)
			Real64 const DeltaDefrostEnergy // Used to reverse accumulation if the zone/load time step is repeated (J)
		) :
			Name( Name ),
			ZoneName( ZoneName ),
			NumSysAttach( NumSysAttach ),
			SchedPtr( SchedPtr ),
			ZoneNodeNum( ZoneNodeNum ),
			ActualZoneNum( ActualZoneNum ),
			ZoneRANode( ZoneRANode ),
			RatedAmbientTemp( RatedAmbientTemp ),
			RatedAmbientRH( RatedAmbientRH ),
			RatedAmbientDewPoint( RatedAmbientDewPoint ),
			RateTotCapPerLength( RateTotCapPerLength ),
			RatedLHR( RatedLHR ),
			RatedRTF( RatedRTF ),
			LatCapCurvePtr( LatCapCurvePtr ),
			DefCapCurvePtr( DefCapCurvePtr ),
			LatentEnergyCurveType( LatentEnergyCurveType ),
			DefrostEnergyCurveType( DefrostEnergyCurveType ),
			STDFanPower( STDFanPower ),
			OperatingFanPower( OperatingFanPower ),
			RatedLightingPower( RatedLightingPower ),
			LightingPower( LightingPower ),
			LightingSchedPtr( LightingSchedPtr ),
			AntiSweatPower( AntiSweatPower ),
			MinimumASPower( MinimumASPower ),
			AntiSweatControlType( AntiSweatControlType ),
			HumAtZeroAS( HumAtZeroAS ),
			Height( Height ),
			DefrostType( DefrostType ),
			DefrostPower( DefrostPower ),
			DefrostSchedPtr( DefrostSchedPtr ),
			DefrostDripDownSchedPtr( DefrostDripDownSchedPtr ),
			Length( Length ),
			Temperature( Temperature ),
			RAFrac( RAFrac ),
			StockingSchedPtr( StockingSchedPtr ),
			LightingFractionToCase( LightingFractionToCase ),
			ASHeaterFractionToCase( ASHeaterFractionToCase ),
			DesignSensCaseCredit( DesignSensCaseCredit ),
			EvapTempDesign( EvapTempDesign ),
			RefrigInventory( RefrigInventory ),
			DesignRefrigInventory( DesignRefrigInventory ),
			DesignRatedCap( DesignRatedCap ),
			DesignLatentCap( DesignLatentCap ),
			DesignDefrostCap( DesignDefrostCap ),
			DesignLighting( DesignLighting ),
			DesignFanPower( DesignFanPower ),
			StoredEnergy( StoredEnergy ),
			StoredEnergySaved( StoredEnergySaved ),
			CaseCreditFracSchedPtr( CaseCreditFracSchedPtr ),
			TotalCoolingLoad( TotalCoolingLoad ),
			TotalCoolingEnergy( TotalCoolingEnergy ),
			SensCoolingEnergyRate( SensCoolingEnergyRate ),
			SensCoolingEnergy( SensCoolingEnergy ),
			LatCoolingEnergyRate( LatCoolingEnergyRate ),
			LatCoolingEnergy( LatCoolingEnergy ),
			SensZoneCreditRate( SensZoneCreditRate ),
			SensZoneCreditCoolRate( SensZoneCreditCoolRate ),
			SensZoneCreditCool( SensZoneCreditCool ),
			SensZoneCreditHeatRate( SensZoneCreditHeatRate ),
			SensZoneCreditHeat( SensZoneCreditHeat ),
			LatZoneCreditRate( LatZoneCreditRate ),
			LatZoneCredit( LatZoneCredit ),
			SensHVACCreditRate( SensHVACCreditRate ),
			SensHVACCreditCoolRate( SensHVACCreditCoolRate ),
			SensHVACCreditCool( SensHVACCreditCool ),
			SensHVACCreditHeatRate( SensHVACCreditHeatRate ),
			SensHVACCreditHeat( SensHVACCreditHeat ),
			LatHVACCreditRate( LatHVACCreditRate ),
			LatHVACCredit( LatHVACCredit ),
			ElecAntiSweatPower( ElecAntiSweatPower ),
			ElecAntiSweatConsumption( ElecAntiSweatConsumption ),
			ElecFanPower( ElecFanPower ),
			ElecFanConsumption( ElecFanConsumption ),
			ElecLightingPower( ElecLightingPower ),
			ElecLightingConsumption( ElecLightingConsumption ),
			ElecDefrostPower( ElecDefrostPower ),
			ElecDefrostConsumption( ElecDefrostConsumption ),
			DefEnergyCurveValue( DefEnergyCurveValue ),
			LatEnergyCurveValue( LatEnergyCurveValue ),
			MaxKgFrost( MaxKgFrost ),
			Rcase( Rcase ),
			DefrostEnergy( DefrostEnergy ),
			StockingEnergy( StockingEnergy ),
			WarmEnvEnergy( WarmEnvEnergy ),
			KgFrost( KgFrost ),
			DefrostEnergySaved( DefrostEnergySaved ),
			StockingEnergySaved( StockingEnergySaved ),
			WarmEnvEnergySaved( WarmEnvEnergySaved ),
			KgFrostSaved( KgFrostSaved ),
			HotDefrostCondCredit( HotDefrostCondCredit ),
			DeltaDefrostEnergy( DeltaDefrostEnergy )
		{}

		// Reset Initialization Values to Zeros
		void
		reset_init()
		{
			TotalCoolingLoad = 0.0;
			TotalCoolingEnergy = 0.0;
			SensCoolingEnergyRate = 0.0;
			SensCoolingEnergy = 0.0;
			LatCoolingEnergyRate = 0.0;
			LatCoolingEnergy = 0.0;
			SensZoneCreditRate = 0.0;
			SensZoneCreditCoolRate = 0.0;
			SensZoneCreditCool = 0.0;
			SensZoneCreditHeatRate = 0.0;
			SensZoneCreditHeat = 0.0;
			LatZoneCreditRate = 0.0;
			LatZoneCredit = 0.0;
			SensHVACCreditRate = 0.0;
			SensHVACCreditCoolRate = 0.0;
			SensHVACCreditCool = 0.0;
			SensHVACCreditHeatRate = 0.0;
			SensHVACCreditHeat = 0.0;
			LatHVACCreditRate = 0.0;
			LatHVACCredit = 0.0;
			ElecFanPower = 0.0;
			ElecFanConsumption = 0.0;
			ElecAntiSweatPower = 0.0;
			ElecAntiSweatConsumption = 0.0;
			ElecLightingPower = 0.0;
			ElecLightingConsumption = 0.0;
			ElecDefrostPower = 0.0;
			ElecDefrostConsumption = 0.0;
			DefEnergyCurveValue = 0.0;
			LatEnergyCurveValue = 0.0;
			HotDefrostCondCredit = 0.0;
		}

		// Reset Accumulation and Carry-Over Values to Zeros
		void
		reset_init_accum()
		{
			DefrostEnergy = 0.0;
			StockingEnergy = 0.0;
			WarmEnvEnergy = 0.0;
			KgFrost = 0.0;
			StoredEnergy = 0.0;
		}

	};

	struct RefrigRackData
	{
		// Members
		bool CoilFlag; // Flag to show if coil type load on rack
		std::string Name; // Name of Refrigeration Compressor rack
		std::string SupplyTankName; // Evap water supply tank name
		std::string EndUseSubcategory; // Rack end-use subcategory
		// Index of refrigerated case (1 to NumCases) connected to rack #X
		Array1D_int CaseNum;
		Array1D_int CoilNum;
		Array1D_int WalkInNum;
		int HeatRejectionLocation; // Refrigeration Compressor Rack heat rejection location
		// (1=LocationOutdoors or 2=LocationZone)
		int CondenserType; // Specifies cooling mode for outdoor condenser
		// (1=Dry air, 2=Evap cooling, 3=Water-cooled)
		Real64 LaggedUsedWaterHeater; // Heat reclaim used to heat water in previous zone/load time step(W)
		Real64 LaggedUsedHVACCoil; // Heat reclaim used to heat HVAC coil in previous zone/load time step(W)
		Real64 EvapEffect; // Effectiveness of evaporative condenser
		Real64 CondenserAirFlowRate; // Evaporative condenser air volume flow rate (m3/s)
		Real64 EvapPumpPower; // Evaporative cooling water pump power (W)
		Real64 ActualEvapPumpPower; // Evaporative cooling water pump power, if adjusted (W)
		Real64 EvapPumpConsumption; // Evaporative cooling water pump electric consumption (J)
		Real64 EvapWaterConsumpRate; // Evaporative condenser water consumption rate (m3/s)
		Real64 EvapWaterConsumption; // Evaporative condenser water consumption (m3)
		int EvapSchedPtr; // Index to the correct evap condenser availability schedule
		Real64 BasinHeaterPowerFTempDiff; // Basin heater capacity per degree K below setpoint (W/K)
		Real64 BasinHeaterSetPointTemp; // Setpoint temperature for basin heater operation (C)
		Real64 BasinHeaterPower; // Power demand from basin heater (W)
		Real64 BasinHeaterConsumption; // Electric consumption from basin heater (J)
		Real64 RatedCOP; // Rated coefficient of performance for compressor rack (W/W)
		int COPFTempPtr; // Index to the correct COP curve object
		int NumCases; // Total number of refrigerated cases attached to each rack
		int NumCoils; // Total number of air chillers attached to each rack
		int NumWalkIns; // Total number of walk-ins attached to each rack
		int EvapWaterSupplyMode; // Source of water for evap condenser cooling
		int EvapWaterSupTankID; // TankID when evap condenser uses water from storage tank
		int EvapWaterTankDemandARRID; // Demand index when evap condenser uses water from storage tank
		int OutsideAirNodeNum; // Outside air node number
		int HeatRejectionZoneNum; // Heat rejection zone number used when walk-ins present and ht rej to zone
		int HeatRejectionZoneNodeNum; // Heat rejection zone node number used when walk-ins present and ht rej to zone
		Real64 TotalRackLoad; // Total capacity of all refrigerated cases on rack
		Real64 RackCompressorCOP; // Rack compressor COP at specific operating conditions
		Real64 RackCompressorPower; // Total rack compressor power (W)
		Real64 RackElecConsumption; // Total rack compressor electric consumption (J)
		Real64 RackCapacity; // Total rack delivered capacity (W)
		Real64 RackCoolingEnergy; // Total rack delivered energy (J)
		Real64 CondenserFanPower; // Condenser fan power (W)
		int TotCondFTempPtr; // Index for condenser fan power modifier curve
		// (function of outdoor temperature)
		Real64 ActualCondenserFanPower; // Rack condenser fan power (W)
		Real64 CondenserFanConsumption; // Rack condenser fan electric consumption (J)
		Real64 SensZoneCreditHeatRate; // Rack sensible heating zone credit rate (W)
		Real64 SensZoneCreditHeat; // Rack sensible heating zone credit energy (J)
		Real64 SensHVACCreditHeatRate; // Rack sensible heating HVAC credit rate (W)
		Real64 SensHVACCreditHeat; // Rack sensible heating HVAC credit energy (J)
		int EvapFreezeWarnIndex; // Recurring freeze warning index
		int NoFlowWarnIndex; // No cooling water when needed warning index
		int HighTempWarnIndex; // Water outlet high temp warning index
		int LowTempWarnIndex; // Water outlet low temp warning index
		int HighFlowWarnIndex; // Water outlet high flow warning index
		int HighInletWarnIndex; // Water inlet high temp warning index
		int InletNode; // Water-cooled condenser inlet node number
		Real64 InletTemp; // Water-cooling condenser inlet temperature (C)
		int OutletNode; // Water-cooled condenser outlet node number
		int PlantTypeOfNum; // Water-cooled condenser plant equipment type
		int PlantLoopNum; // Water-cooled condenser plant loop number
		int PlantLoopSideNum; // Water-cooled condenser plant loop side number
		int PlantBranchNum; // Water-cooled condenser plant branch number
		int PlantCompNum; // Water-cooled condenser plant component number
		Real64 OutletTemp; // Water-cooling condenser outlet temperature (C)
		int OutletTempSchedPtr; // Schedule pointer for condenser outlet temp setting
		Real64 VolFlowRate; // Water-cooled condenser volumetric flow rate (m3/s)
		Real64 DesVolFlowRate; // Water-cooled condenser design volumetric flow rate (m3/s)
		Real64 MassFlowRate; // Water-cooled condenser mass flow rate (kg/s)
		Real64 CondLoad; // Total condenser load (W)
		Real64 CondEnergy; // Condenser energy (J)
		int FlowType; // Water-cooled condenser loop flow type
		Real64 VolFlowRateMax; // Maximum condenser volumetric flow rate (m3/s)
		Real64 MassFlowRateMax; // Maximum condenser mass flow rate (kg/s)
		Real64 InletTempMin; // Minimum condenser water inlet temperature (C)
		Real64 OutletTempMax; // Maximum condenser water outlet temperature (C)
		Real64 TotalCoolingLoad;

		// Default Constructor
		RefrigRackData() :
			CoilFlag( false ),
			EndUseSubcategory( "General" ),
			HeatRejectionLocation( 0 ),
			CondenserType( 0 ),
			EvapEffect( 0.9 ),
			CondenserAirFlowRate( 0.0 ),
			EvapPumpPower( 0.0 ),
			ActualEvapPumpPower( 0.0 ),
			EvapPumpConsumption( 0.0 ),
			EvapWaterConsumpRate( 0.0 ),
			EvapWaterConsumption( 0.0 ),
			EvapSchedPtr( 0 ),
			BasinHeaterPowerFTempDiff( 0.0 ),
			BasinHeaterSetPointTemp( 2.0 ),
			BasinHeaterPower( 0.0 ),
			BasinHeaterConsumption( 0.0 ),
			RatedCOP( 0.0 ),
			COPFTempPtr( 0 ),
			NumCases( 0 ),
			NumCoils( 0 ),
			NumWalkIns( 0 ),
			EvapWaterSupplyMode( WaterSupplyFromMains ),
			EvapWaterSupTankID( 0 ),
			EvapWaterTankDemandARRID( 0 ),
			OutsideAirNodeNum( 0 ),
			HeatRejectionZoneNum( 0 ),
			HeatRejectionZoneNodeNum( 0 ),
			TotalRackLoad( 0.0 ),
			RackCompressorCOP( 0.0 ),
			RackCompressorPower( 0.0 ),
			RackElecConsumption( 0.0 ),
			RackCapacity( 0.0 ),
			RackCoolingEnergy( 0.0 ),
			CondenserFanPower( 0.0 ),
			TotCondFTempPtr( 0 ),
			ActualCondenserFanPower( 0.0 ),
			CondenserFanConsumption( 0.0 ),
			SensZoneCreditHeatRate( 0.0 ),
			SensZoneCreditHeat( 0.0 ),
			SensHVACCreditHeatRate( 0.0 ),
			SensHVACCreditHeat( 0.0 ),
			EvapFreezeWarnIndex( 0 ),
			NoFlowWarnIndex( 0 ),
			HighTempWarnIndex( 0 ),
			LowTempWarnIndex( 0 ),
			HighFlowWarnIndex( 0 ),
			HighInletWarnIndex( 0 ),
			InletNode( 0 ),
			InletTemp( 0.0 ),
			OutletNode( 0 ),
			PlantTypeOfNum( 0 ),
			PlantLoopNum( 0 ),
			PlantLoopSideNum( 0 ),
			PlantBranchNum( 0 ),
			PlantCompNum( 0 ),
			OutletTemp( 0.0 ),
			OutletTempSchedPtr( 0 ),
			VolFlowRate( 0.0 ),
			DesVolFlowRate( 0.0 ),
			MassFlowRate( 0.0 ),
			CondLoad( 0.0 ),
			CondEnergy( 0.0 ),
			FlowType( 1 ),
			VolFlowRateMax( 0.0 ),
			MassFlowRateMax( 0.0 ),
			InletTempMin( 10.0 ),
			OutletTempMax( 55.0 ),
			TotalCoolingLoad( 0.0 )
		{}

		// Member Constructor
		RefrigRackData(
			bool const CoilFlag, // Flag to show if coil type load on rack
			std::string const & Name, // Name of Refrigeration Compressor rack
			std::string const & SupplyTankName, // Evap water supply tank name
			std::string const & EndUseSubcategory, // Rack end-use subcategory
			Array1_int const & CaseNum,
			Array1_int const & CoilNum,
			Array1_int const & WalkInNum,
			int const HeatRejectionLocation, // Refrigeration Compressor Rack heat rejection location
			int const CondenserType, // Specifies cooling mode for outdoor condenser
			Real64 const LaggedUsedWaterHeater, // Heat reclaim used to heat water in previous zone/load time step(W)
			Real64 const LaggedUsedHVACCoil, // Heat reclaim used to heat HVAC coil in previous zone/load time step(W)
			Real64 const EvapEffect, // Effectiveness of evaporative condenser
			Real64 const CondenserAirFlowRate, // Evaporative condenser air volume flow rate (m3/s)
			Real64 const EvapPumpPower, // Evaporative cooling water pump power (W)
			Real64 const ActualEvapPumpPower, // Evaporative cooling water pump power, if adjusted (W)
			Real64 const EvapPumpConsumption, // Evaporative cooling water pump electric consumption (J)
			Real64 const EvapWaterConsumpRate, // Evaporative condenser water consumption rate (m3/s)
			Real64 const EvapWaterConsumption, // Evaporative condenser water consumption (m3)
			int const EvapSchedPtr, // Index to the correct evap condenser availability schedule
			Real64 const BasinHeaterPowerFTempDiff, // Basin heater capacity per degree K below setpoint (W/K)
			Real64 const BasinHeaterSetPointTemp, // Setpoint temperature for basin heater operation (C)
			Real64 const BasinHeaterPower, // Power demand from basin heater (W)
			Real64 const BasinHeaterConsumption, // Electric consumption from basin heater (J)
			Real64 const RatedCOP, // Rated coefficient of performance for compressor rack (W/W)
			int const COPFTempPtr, // Index to the correct COP curve object
			int const NumCases, // Total number of refrigerated cases attached to each rack
			int const NumCoils, // Total number of air chillers attached to each rack
			int const NumWalkIns, // Total number of walk-ins attached to each rack
			int const EvapWaterSupplyMode, // Source of water for evap condenser cooling
			int const EvapWaterSupTankID, // TankID when evap condenser uses water from storage tank
			int const EvapWaterTankDemandARRID, // Demand index when evap condenser uses water from storage tank
			int const OutsideAirNodeNum, // Outside air node number
			int const HeatRejectionZoneNum, // Heat rejection zone number used when walk-ins present and ht rej to zone
			int const HeatRejectionZoneNodeNum, // Heat rejection zone node number used when walk-ins present and ht rej to zone
			Real64 const TotalRackLoad, // Total capacity of all refrigerated cases on rack
			Real64 const RackCompressorCOP, // Rack compressor COP at specific operating conditions
			Real64 const RackCompressorPower, // Total rack compressor power (W)
			Real64 const RackElecConsumption, // Total rack compressor electric consumption (J)
			Real64 const RackCapacity, // Total rack delivered capacity (W)
			Real64 const RackCoolingEnergy, // Total rack delivered energy (J)
			Real64 const CondenserFanPower, // Condenser fan power (W)
			int const TotCondFTempPtr, // Index for condenser fan power modifier curve
			Real64 const ActualCondenserFanPower, // Rack condenser fan power (W)
			Real64 const CondenserFanConsumption, // Rack condenser fan electric consumption (J)
			Real64 const SensZoneCreditHeatRate, // Rack sensible heating zone credit rate (W)
			Real64 const SensZoneCreditHeat, // Rack sensible heating zone credit energy (J)
			Real64 const SensHVACCreditHeatRate, // Rack sensible heating HVAC credit rate (W)
			Real64 const SensHVACCreditHeat, // Rack sensible heating HVAC credit energy (J)
			int const EvapFreezeWarnIndex, // Recurring freeze warning index
			int const NoFlowWarnIndex, // No cooling water when needed warning index
			int const HighTempWarnIndex, // Water outlet high temp warning index
			int const LowTempWarnIndex, // Water outlet low temp warning index
			int const HighFlowWarnIndex, // Water outlet high flow warning index
			int const HighInletWarnIndex, // Water inlet high temp warning index
			int const InletNode, // Water-cooled condenser inlet node number
			Real64 const InletTemp, // Water-cooling condenser inlet temperature (C)
			int const OutletNode, // Water-cooled condenser outlet node number
			int const PlantTypeOfNum, // Water-cooled condenser plant equipment type
			int const PlantLoopNum, // Water-cooled condenser plant loop number
			int const PlantLoopSideNum, // Water-cooled condenser plant loop side number
			int const PlantBranchNum, // Water-cooled condenser plant branch number
			int const PlantCompNum, // Water-cooled condenser plant component number
			Real64 const OutletTemp, // Water-cooling condenser outlet temperature (C)
			int const OutletTempSchedPtr, // Schedule pointer for condenser outlet temp setting
			Real64 const VolFlowRate, // Water-cooled condenser volumetric flow rate (m3/s)
			Real64 const DesVolFlowRate, // Water-cooled condenser design volumetric flow rate (m3/s)
			Real64 const MassFlowRate, // Water-cooled condenser mass flow rate (kg/s)
			Real64 const CondLoad, // Total condenser load (W)
			Real64 const CondEnergy, // Condenser energy (J)
			int const FlowType, // Water-cooled condenser loop flow type
			Real64 const VolFlowRateMax, // Maximum condenser volumetric flow rate (m3/s)
			Real64 const MassFlowRateMax, // Maximum condenser mass flow rate (kg/s)
			Real64 const InletTempMin, // Minimum condenser water inlet temperature (C)
			Real64 const OutletTempMax, // Maximum condenser water outlet temperature (C)
			Real64 const TotalCoolingLoad
		) :
			CoilFlag( CoilFlag ),
			Name( Name ),
			SupplyTankName( SupplyTankName ),
			EndUseSubcategory( EndUseSubcategory ),
			CaseNum( CaseNum ),
			CoilNum( CoilNum ),
			WalkInNum( WalkInNum ),
			HeatRejectionLocation( HeatRejectionLocation ),
			CondenserType( CondenserType ),
			LaggedUsedWaterHeater( LaggedUsedWaterHeater ),
			LaggedUsedHVACCoil( LaggedUsedHVACCoil ),
			EvapEffect( EvapEffect ),
			CondenserAirFlowRate( CondenserAirFlowRate ),
			EvapPumpPower( EvapPumpPower ),
			ActualEvapPumpPower( ActualEvapPumpPower ),
			EvapPumpConsumption( EvapPumpConsumption ),
			EvapWaterConsumpRate( EvapWaterConsumpRate ),
			EvapWaterConsumption( EvapWaterConsumption ),
			EvapSchedPtr( EvapSchedPtr ),
			BasinHeaterPowerFTempDiff( BasinHeaterPowerFTempDiff ),
			BasinHeaterSetPointTemp( BasinHeaterSetPointTemp ),
			BasinHeaterPower( BasinHeaterPower ),
			BasinHeaterConsumption( BasinHeaterConsumption ),
			RatedCOP( RatedCOP ),
			COPFTempPtr( COPFTempPtr ),
			NumCases( NumCases ),
			NumCoils( NumCoils ),
			NumWalkIns( NumWalkIns ),
			EvapWaterSupplyMode( EvapWaterSupplyMode ),
			EvapWaterSupTankID( EvapWaterSupTankID ),
			EvapWaterTankDemandARRID( EvapWaterTankDemandARRID ),
			OutsideAirNodeNum( OutsideAirNodeNum ),
			HeatRejectionZoneNum( HeatRejectionZoneNum ),
			HeatRejectionZoneNodeNum( HeatRejectionZoneNodeNum ),
			TotalRackLoad( TotalRackLoad ),
			RackCompressorCOP( RackCompressorCOP ),
			RackCompressorPower( RackCompressorPower ),
			RackElecConsumption( RackElecConsumption ),
			RackCapacity( RackCapacity ),
			RackCoolingEnergy( RackCoolingEnergy ),
			CondenserFanPower( CondenserFanPower ),
			TotCondFTempPtr( TotCondFTempPtr ),
			ActualCondenserFanPower( ActualCondenserFanPower ),
			CondenserFanConsumption( CondenserFanConsumption ),
			SensZoneCreditHeatRate( SensZoneCreditHeatRate ),
			SensZoneCreditHeat( SensZoneCreditHeat ),
			SensHVACCreditHeatRate( SensHVACCreditHeatRate ),
			SensHVACCreditHeat( SensHVACCreditHeat ),
			EvapFreezeWarnIndex( EvapFreezeWarnIndex ),
			NoFlowWarnIndex( NoFlowWarnIndex ),
			HighTempWarnIndex( HighTempWarnIndex ),
			LowTempWarnIndex( LowTempWarnIndex ),
			HighFlowWarnIndex( HighFlowWarnIndex ),
			HighInletWarnIndex( HighInletWarnIndex ),
			InletNode( InletNode ),
			InletTemp( InletTemp ),
			OutletNode( OutletNode ),
			PlantTypeOfNum( PlantTypeOfNum ),
			PlantLoopNum( PlantLoopNum ),
			PlantLoopSideNum( PlantLoopSideNum ),
			PlantBranchNum( PlantBranchNum ),
			PlantCompNum( PlantCompNum ),
			OutletTemp( OutletTemp ),
			OutletTempSchedPtr( OutletTempSchedPtr ),
			VolFlowRate( VolFlowRate ),
			DesVolFlowRate( DesVolFlowRate ),
			MassFlowRate( MassFlowRate ),
			CondLoad( CondLoad ),
			CondEnergy( CondEnergy ),
			FlowType( FlowType ),
			VolFlowRateMax( VolFlowRateMax ),
			MassFlowRateMax( MassFlowRateMax ),
			InletTempMin( InletTempMin ),
			OutletTempMax( OutletTempMax ),
			TotalCoolingLoad( TotalCoolingLoad )
		{}

		// Reset Initialization Values to Zeros
		void
		reset_init()
		{
			SensHVACCreditHeatRate = 0.0;
			SensHVACCreditHeat = 0.0;
			SensZoneCreditHeatRate = 0.0;
			SensZoneCreditHeat = 0.0;
			CondLoad = 0.0;
			CondEnergy = 0.0;
			MassFlowRate = 0.0;
			RackElecConsumption = 0.0;
			CondenserFanConsumption = 0.0;
			EvapPumpConsumption = 0.0;
			RackCompressorPower = 0.0;
			ActualCondenserFanPower = 0.0;
			ActualEvapPumpPower = 0.0;
		}

	};

	struct RefrigSystemData
	{
		// Members
		std::string Name; // Name of refrigeration system
		std::string RefrigerantName; // Name of refrigerant, must match name in FluidName
		//    (see fluidpropertiesrefdata.idf)
		std::string EndUseSubcategory; // Used for reporting purposes
		bool SystemRejectHeatToZone; // Flag to show air-cooled condenser located inside zone
		bool CoilFlag; // Flag to show if coil type load on system (even if below in a secondary)
		Array1D_int CascadeLoadNum; // absolute index  of condensers placing load (allocated NumCondensers)
		Array1D_int CaseNum; // absolute Index of cases (allocated NumCases)
		Array1D_int CoilNum; // absolute Index of coils (allocated NumCoils)
		Array1D_int CompressorNum; // absolute Index of compressors (allocated NumCompressors)
		Array1D_int CondenserNum; // absolute Index of condensers removing load (allocated NumCondensers)
		Array1D_int GasCoolerNum; // absolute Index of gas cooler
		Array1D_int HiStageCompressorNum; // absolute Index of high-stage compressors (allocated NumHiStageCompressors)
		Array1D_int SecondaryNum; // absolute Index of seocndary loops (allocated NumSecondarys)
		Array1D_int SubcoolerNum; // Absolute Index of subcoolers (allocated NumSubcoolers)
		Array1D_int WalkInNum; // absolute Index of walk ins (allocated NumWalkIns)
		int CompSuctControl; // Index to suction control
		// 2 =fixed, 1=floating
		//INTEGER     :: ServiceType      = 1       ! Index to warehouse or supermarket
		// 1 = supermarket, 2=warehouse
		int HiStageWarnIndex1; // Recurring warning index when hi stage compressors unable to meet coil loads
		int HiStageWarnIndex2; // Recurring warning index when hi stage compressors unable to meet coil loads
		int InsuffCapWarn; // Recurring warning index when refrigeration system unable to meet coil loads
		int IntercoolerType; // Intercooler type (0=none, 1=flash intercooler, 2=shell-and-coil intercooler)
		int NumCases; // Number of cases on this system
		int NumCoils; // Number of cases on this system
		int NumCompressors; // Number of compressors on this system for single-stage systems
		// or number of low-stage compressors on this system for two-stage systems
		int NumCondensers; // Number of condensers on this system
		int NumGasCoolers; // Number of gas coolers on this system
		int NumHiStageCompressors; // Number of high-stage compressors on this system (two-stage systems only)
		int NumSecondarys; // Number of secondary loops on this system
		int NumStages; // Number of compressor stages
		int NumSubcoolers; // Number of subcoolers on this system
		int NumWalkIns; // Number of walk in coolers on this system
		int NumMechSCServed; // Number of mech subcoolers served/powered by compressor/cond on this system
		int NumNonCascadeLoads; // Sum of NumCases, NumWalk-Ins, NumCoils, and NumSecondarys
		int NumCascadeLoads; // Number of cascade condensers cooled by this system
		int NumTransferLoads; // Sum of NumCascadeLoads and NumSecondarys
		int RefIndex; // Index number of refrigerant, automatically assigned on first call to fluid property
		//   and used thereafter
		int SuctionPipeActualZoneNum; // ID number for zone where suction pipes gain heat
		int SuctionPipeZoneNodeNum; // ID number for zone node where suction pipes gain heat
		Array1D< Real64 > MechSCLoad; // Mechanical subcooler load on system from other systems(W)
		Real64 AverageCompressorCOP; // Average COP for compressers on this system (W)
		Real64 CpSatLiqCond; // Spec Heat of sat liquid at condensing pressure  (J/kg-C)
		Real64 CpSatVapEvap; // Spec Heat of saturated vapor exiting evaporator (J/kg-C)
		Real64 FlowRatioIntercooler; // Refrigerant mass flow ratio through coil-side of shell-and-coil intercooler
		Real64 HCaseIn; // Case inlet enthalpy (after subcoolers and pipe P drops) (J/kg)
		Real64 HCompIn; // Compressor inlet enthalpy  (J/kg)
		Real64 HCompOut; // Compressor outlet enthalpy (J/kg)
		Real64 HSatLiqCond; // Enthalpy of sat liquid at condensing pressure  (J/kg)
		Real64 HCaseOut; // Enthalpy of refrigerant leaving cases, after superheat (J/kg)
		Real64 IntercoolerEffectiveness; // Shell-and-coil intercooler effectiveness
		Real64 LSHXTrans; // Liquid suction subcooler load transferred within same suction group, W
		Real64 LSHXTransEnergy; // Liquid suction subcooler load transferred within same suction group, J
		Real64 NetHeatRejectLoad; // Portion of TotalCondenser load due to this system (after heat recovery) W
		Real64 NetHeatRejectEnergy; // Portion of TotalCondenser energy due to this system (after heat recovery) J
		Real64 PIntercooler; // Pressure in the intercooler (two-stage systems only) (Pa)
		Real64 PipeHeatLoad; // Total suction pipe heat gains, optional (W)
		Real64 PipeHeatEnergy; // Total suction pipe heat gains, optional (J)
		Real64 RefMassFlowtoLoads; // Total system refrigerant mass flow through cases(kg/s)
		Real64 RefMassFlowComps; // Total system refrigerant mass flow through compressors(kg/s)
		Real64 RefMassFlowHiStageComps; // Total system refrigerant mass flow through high-stage compressors(two-stage systems only) (kg/s)
		Real64 RefInventory; // Approximate refrigerant inventory entered by user (kg)
		Real64 SumMechSCLoad; // Total cooling load of all mech subcoolers served by suction group (W)
		Real64 SumMechSCBenefit; // Total cooling provided by mech subcoolers cooling liquid condensate in this system (W)
		Real64 SumCascadeCondCredit; // Sum of cond cred for hot brine/gas defrost on cases etc served by
		//    cascade condenser cooled by this system (W)
		Real64 SumCascadeLoad; // Total cooling load of all cascade condensers served by suction group (W)
		Real64 SumSecondaryLoopLoad; // Total cooling loads for all secondary loops served by this suction group (W)
		Real64 SumUASuctionPiping; // Sum of U*A for system suction piping (W/C)
		Real64 TCaseOut; // Case out temperature including case superheat (C)
		Real64 TCondense; // Condensing temperature (Tsat for P discharge) (C)
		Real64 TCompIn; // Compressor inlet temperature (after case and LSHX superheat and pipe delta P) (C)
		Real64 TCondenseMin; // Minimum allowed condensing temperature (C)
		Real64 TCondenseMinInput; // Minimum allowed condensing temperature, user's original input value (C)
		bool EMSOverrideOnTCondenseMin; // if true, EMS is calling to override minimum allowed condensing temperature
		Real64 EMSOverrideValueTCondenseMin; // value to use when EMS override is true [C]
		Real64 TEvapDesign; // Min (on sys) design case/walkin/secondary evap temp
		//  (also basis for floating evap T calc) (C)
		Real64 TEvapNeeded; // Max Case evap temperature to maintain lowest case T on system (C)
		Real64 TIntercooler; // Temperature in the intercooler (two-stage systems only) (Pa)
		Real64 TLiqInActual; // Actual liquid temperature entering TXV after subcooling (C)
		Real64 TotalCondDefrostCredit; // sum of heat reclaimed for hot gas and hot brine defrost for
		//    cases/WI/sec served directly [W]
		Real64 TotalCoolingEnergy; // Total energy of all refrigerated cases and walkins served directly (J)
		Real64 TotalCoolingLoad; // Total load of all refrigerated cases and walkins served directly (W)
		Real64 TotalSystemLoad; // Includes cases, walk-ins, and transfer loads (cascade, second, subcooler), W
		Real64 TotCompPower; // Total power for compressers on this system (for single-stage systems) or
		// total power for low-stage compressors on this system (for two-stage systems) (W)
		Real64 TotCompElecConsump; // Total Elec consump for compressers on this system (for single-stage systems) or
		// total elec consump for low-stage compressors on this system (for two-stage systems) (J)
		Real64 TotCompCapacity; // Total design capacity for compressers on this system (for single-stage systems) or
		// total design capacity for low-stage compressors on this system (for two-stage systems) (W)
		Real64 TotCompCoolingEnergy; // Total cooling energy from compressers on this system (for single-stage systems) or
		// total cooling energy from low-stage compressors on this system (for two-stage systems) (J)
		Real64 TotHiStageCompCapacity; // Total design capacity for high-stage compressers on this system (two-stage systems only) (W)
		Real64 TotHiStageCompCoolingEnergy; // Total cooling energy from high-stage compressers on this system (two-stage systems only) (J)
		Real64 TotHiStageCompElecConsump; // Total Elec consump for high-stage compressers on this system (two-stage systems only) (J)
		Real64 TotHiStageCompPower; // Total power for high-stage compressers on this system (two-stage systems only) (W)
		Real64 TotCompElecConsumpTwoStage; // Total Elec consump for the low- and high-stage compressors on this system (two-stage systems only) (J)
		Real64 TotRejectHeatRecovered; // Total reject heat recovered for hot gas or hot brine defrost or
		//     desuperheater coils (W)
		Real64 TotTransferLoad; // Total load from other systems transferred to this sytem, incl mech subcoolers,
		// cascade, and secondary loops (W)
		Real64 TotTransferEnergy; // Total energy from other systems transferred to this sytem, incl mech subcoolers,
		// cascade, and secondary loops (J)
		Real64 UnmetEnergy; // Accumulative loads unmet by total compressors (for single-stage systems) or
		// by low-stage compressors (for two-stage systems) on this system (J)
		Real64 UnmetHiStageEnergy; // Accumulative loads unmet by total high-stage compressors (two-stage systems only) on this system (J)
		Real64 UnmetEnergySaved; // Accumulative loads unmet by total compressors (for single-stage systems) on this system (J)

		// Default Constructor
		RefrigSystemData() :
			SystemRejectHeatToZone( false ),
			CoilFlag( false ),
			CompSuctControl( 2 ),
			HiStageWarnIndex1( 0 ),
			HiStageWarnIndex2( 0 ),
			InsuffCapWarn( 0 ),
			IntercoolerType( 0 ),
			NumCases( 0 ),
			NumCoils( 0 ),
			NumCompressors( 0 ),
			NumCondensers( 1 ),
			NumGasCoolers( 0 ),
			NumHiStageCompressors( 0 ),
			NumSecondarys( 0 ),
			NumStages( 1 ),
			NumSubcoolers( 0 ),
			NumWalkIns( 0 ),
			NumMechSCServed( 0 ),
			NumNonCascadeLoads( 0 ),
			NumCascadeLoads( 0 ),
			NumTransferLoads( 0 ),
			RefIndex( 0 ),
			SuctionPipeActualZoneNum( 0 ),
			SuctionPipeZoneNodeNum( 0 ),
			AverageCompressorCOP( 0.0 ),
			CpSatLiqCond( 0.0 ),
			CpSatVapEvap( 0.0 ),
			FlowRatioIntercooler( 0.0 ),
			HCaseIn( 0.0 ),
			HCompIn( 0.0 ),
			HCompOut( 0.0 ),
			HSatLiqCond( 0.0 ),
			HCaseOut( 0.0 ),
			IntercoolerEffectiveness( 0.0 ),
			LSHXTrans( 0.0 ),
			LSHXTransEnergy( 0.0 ),
			NetHeatRejectLoad( 0.0 ),
			NetHeatRejectEnergy( 0.0 ),
			PIntercooler( 0.0 ),
			PipeHeatLoad( 0.0 ),
			PipeHeatEnergy( 0.0 ),
			RefMassFlowtoLoads( 0.0 ),
			RefMassFlowComps( 0.0 ),
			RefMassFlowHiStageComps( 0.0 ),
			RefInventory( 0.0 ),
			SumMechSCLoad( 0.0 ),
			SumMechSCBenefit( 0.0 ),
			SumCascadeCondCredit( 0.0 ),
			SumCascadeLoad( 0.0 ),
			SumSecondaryLoopLoad( 0.0 ),
			SumUASuctionPiping( 0.0 ),
			TCaseOut( 0.0 ),
			TCondense( 0.0 ),
			TCompIn( 0.0 ),
			TCondenseMin( 0.0 ),
			TCondenseMinInput( 0.0 ),
			EMSOverrideOnTCondenseMin( false ),
			EMSOverrideValueTCondenseMin( 0.0 ),
			TEvapDesign( 0.0 ),
			TEvapNeeded( 0.0 ),
			TIntercooler( 0.0 ),
			TLiqInActual( 0.0 ),
			TotalCondDefrostCredit( 0.0 ),
			TotalCoolingEnergy( 0.0 ),
			TotalCoolingLoad( 0.0 ),
			TotalSystemLoad( 0.0 ),
			TotCompPower( 0.0 ),
			TotCompElecConsump( 0.0 ),
			TotCompCapacity( 0.0 ),
			TotCompCoolingEnergy( 0.0 ),
			TotHiStageCompCapacity( 0.0 ),
			TotHiStageCompCoolingEnergy( 0.0 ),
			TotHiStageCompElecConsump( 0.0 ),
			TotHiStageCompPower( 0.0 ),
			TotCompElecConsumpTwoStage( 0.0 ),
			TotRejectHeatRecovered( 0.0 ),
			TotTransferLoad( 0.0 ),
			TotTransferEnergy( 0.0 ),
			UnmetEnergy( 0.0 ),
			UnmetHiStageEnergy( 0.0 ),
			UnmetEnergySaved( 0.0 )
		{}

		// Member Constructor
		RefrigSystemData(
			std::string const & Name, // Name of refrigeration system
			std::string const & RefrigerantName, // Name of refrigerant, must match name in FluidName
			std::string const & EndUseSubcategory, // Used for reporting purposes
			bool const SystemRejectHeatToZone, // Flag to show air-cooled condenser located inside zone
			bool const CoilFlag, // Flag to show if coil type load on system (even if below in a secondary)
			Array1_int const & CascadeLoadNum, // absolute index  of condensers placing load (allocated NumCondensers)
			Array1_int const & CaseNum, // absolute Index of cases (allocated NumCases)
			Array1_int const & CoilNum, // absolute Index of coils (allocated NumCoils)
			Array1_int const & CompressorNum, // absolute Index of compressors (allocated NumCompressors)
			Array1_int const & CondenserNum, // absolute Index of condensers removing load (allocated NumCondensers)
			Array1_int const & GasCoolerNum, // absolute Index of gas cooler
			Array1_int const & HiStageCompressorNum, // absolute Index of high-stage compressors (allocated NumHiStageCompressors)
			Array1_int const & SecondaryNum, // absolute Index of seocndary loops (allocated NumSecondarys)
			Array1_int const & SubcoolerNum, // Absolute Index of subcoolers (allocated NumSubcoolers)
			Array1_int const & WalkInNum, // absolute Index of walk ins (allocated NumWalkIns)
			int const CompSuctControl, // Index to suction control
			int const HiStageWarnIndex1, // Recurring warning index when hi stage compressors unable to meet coil loads
			int const HiStageWarnIndex2, // Recurring warning index when hi stage compressors unable to meet coil loads
			int const InsuffCapWarn, // Recurring warning index when refrigeration system unable to meet coil loads
			int const IntercoolerType, // Intercooler type (0=none, 1=flash intercooler, 2=shell-and-coil intercooler)
			int const NumCases, // Number of cases on this system
			int const NumCoils, // Number of cases on this system
			int const NumCompressors, // Number of compressors on this system for single-stage systems
			int const NumCondensers, // Number of condensers on this system
			int const NumGasCoolers, // Number of gas coolers on this system
			int const NumHiStageCompressors, // Number of high-stage compressors on this system (two-stage systems only)
			int const NumSecondarys, // Number of secondary loops on this system
			int const NumStages, // Number of compressor stages
			int const NumSubcoolers, // Number of subcoolers on this system
			int const NumWalkIns, // Number of walk in coolers on this system
			int const NumMechSCServed, // Number of mech subcoolers served/powered by compressor/cond on this system
			int const NumNonCascadeLoads, // Sum of NumCases, NumWalk-Ins, NumCoils, and NumSecondarys
			int const NumCascadeLoads, // Number of cascade condensers cooled by this system
			int const NumTransferLoads, // Sum of NumCascadeLoads and NumSecondarys
			int const RefIndex, // Index number of refrigerant, automatically assigned on first call to fluid property
			int const SuctionPipeActualZoneNum, // ID number for zone where suction pipes gain heat
			int const SuctionPipeZoneNodeNum, // ID number for zone node where suction pipes gain heat
			Array1< Real64 > const & MechSCLoad, // Mechanical subcooler load on system from other systems(W)
			Real64 const AverageCompressorCOP, // Average COP for compressers on this system (W)
			Real64 const CpSatLiqCond, // Spec Heat of sat liquid at condensing pressure  (J/kg-C)
			Real64 const CpSatVapEvap, // Spec Heat of saturated vapor exiting evaporator (J/kg-C)
			Real64 const FlowRatioIntercooler, // Refrigerant mass flow ratio through coil-side of shell-and-coil intercooler
			Real64 const HCaseIn, // Case inlet enthalpy (after subcoolers and pipe P drops) (J/kg)
			Real64 const HCompIn, // Compressor inlet enthalpy  (J/kg)
			Real64 const HCompOut, // Compressor outlet enthalpy (J/kg)
			Real64 const HSatLiqCond, // Enthalpy of sat liquid at condensing pressure  (J/kg)
			Real64 const HCaseOut, // Enthalpy of refrigerant leaving cases, after superheat (J/kg)
			Real64 const IntercoolerEffectiveness, // Shell-and-coil intercooler effectiveness
			Real64 const LSHXTrans, // Liquid suction subcooler load transferred within same suction group, W
			Real64 const LSHXTransEnergy, // Liquid suction subcooler load transferred within same suction group, J
			Real64 const NetHeatRejectLoad, // Portion of TotalCondenser load due to this system (after heat recovery) W
			Real64 const NetHeatRejectEnergy, // Portion of TotalCondenser energy due to this system (after heat recovery) J
			Real64 const PIntercooler, // Pressure in the intercooler (two-stage systems only) (Pa)
			Real64 const PipeHeatLoad, // Total suction pipe heat gains, optional (W)
			Real64 const PipeHeatEnergy, // Total suction pipe heat gains, optional (J)
			Real64 const RefMassFlowtoLoads, // Total system refrigerant mass flow through cases(kg/s)
			Real64 const RefMassFlowComps, // Total system refrigerant mass flow through compressors(kg/s)
			Real64 const RefMassFlowHiStageComps, // Total system refrigerant mass flow through high-stage compressors(two-stage systems only) (kg/s)
			Real64 const RefInventory, // Approximate refrigerant inventory entered by user (kg)
			Real64 const SumMechSCLoad, // Total cooling load of all mech subcoolers served by suction group (W)
			Real64 const SumMechSCBenefit, // Total cooling provided by mech subcoolers cooling liquid condensate in this system (W)
			Real64 const SumCascadeCondCredit, // Sum of cond cred for hot brine/gas defrost on cases etc served by
			Real64 const SumCascadeLoad, // Total cooling load of all cascade condensers served by suction group (W)
			Real64 const SumSecondaryLoopLoad, // Total cooling loads for all secondary loops served by this suction group (W)
			Real64 const SumUASuctionPiping, // Sum of U*A for system suction piping (W/C)
			Real64 const TCaseOut, // Case out temperature including case superheat (C)
			Real64 const TCondense, // Condensing temperature (Tsat for P discharge) (C)
			Real64 const TCompIn, // Compressor inlet temperature (after case and LSHX superheat and pipe delta P) (C)
			Real64 const TCondenseMin, // Minimum allowed condensing temperature (C)
			Real64 const TCondenseMinInput, // Minimum allowed condensing temperature, user's original input value (C)
			bool const EMSOverrideOnTCondenseMin, // if true, EMS is calling to override minimum allowed condensing temperature
			Real64 const EMSOverrideValueTCondenseMin, // value to use when EMS override is true [C]
			Real64 const TEvapDesign, // Min (on sys) design case/walkin/secondary evap temp
			Real64 const TEvapNeeded, // Max Case evap temperature to maintain lowest case T on system (C)
			Real64 const TIntercooler, // Temperature in the intercooler (two-stage systems only) (Pa)
			Real64 const TLiqInActual, // Actual liquid temperature entering TXV after subcooling (C)
			Real64 const TotalCondDefrostCredit, // sum of heat reclaimed for hot gas and hot brine defrost for
			Real64 const TotalCoolingEnergy, // Total energy of all refrigerated cases and walkins served directly (J)
			Real64 const TotalCoolingLoad, // Total load of all refrigerated cases and walkins served directly (W)
			Real64 const TotalSystemLoad, // Includes cases, walk-ins, and transfer loads (cascade, second, subcooler), W
			Real64 const TotCompPower, // Total power for compressers on this system (for single-stage systems) or
			Real64 const TotCompElecConsump, // Total Elec consump for compressers on this system (for single-stage systems) or
			Real64 const TotCompCapacity, // Total design capacity for compressers on this system (for single-stage systems) or
			Real64 const TotCompCoolingEnergy, // Total cooling energy from compressers on this system (for single-stage systems) or
			Real64 const TotHiStageCompCapacity, // Total design capacity for high-stage compressers on this system (two-stage systems only) (W)
			Real64 const TotHiStageCompCoolingEnergy, // Total cooling energy from high-stage compressers on this system (two-stage systems only) (J)
			Real64 const TotHiStageCompElecConsump, // Total Elec consump for high-stage compressers on this system (two-stage systems only) (J)
			Real64 const TotHiStageCompPower, // Total power for high-stage compressers on this system (two-stage systems only) (W)
			Real64 const TotCompElecConsumpTwoStage, // Total Elec consump for the low- and high-stage compressors on this system (two-stage systems only) (J)
			Real64 const TotRejectHeatRecovered, // Total reject heat recovered for hot gas or hot brine defrost or
			Real64 const TotTransferLoad, // Total load from other systems transferred to this sytem, incl mech subcoolers,
			Real64 const TotTransferEnergy, // Total energy from other systems transferred to this sytem, incl mech subcoolers,
			Real64 const UnmetEnergy, // Accumulative loads unmet by total compressors (for single-stage systems) or
			Real64 const UnmetHiStageEnergy, // Accumulative loads unmet by total high-stage compressors (two-stage systems only) on this system (J)
			Real64 const UnmetEnergySaved // Accumulative loads unmet by total compressors (for single-stage systems) on this system (J)
		) :
			Name( Name ),
			RefrigerantName( RefrigerantName ),
			EndUseSubcategory( EndUseSubcategory ),
			SystemRejectHeatToZone( SystemRejectHeatToZone ),
			CoilFlag( CoilFlag ),
			CascadeLoadNum( CascadeLoadNum ),
			CaseNum( CaseNum ),
			CoilNum( CoilNum ),
			CompressorNum( CompressorNum ),
			CondenserNum( CondenserNum ),
			GasCoolerNum( GasCoolerNum ),
			HiStageCompressorNum( HiStageCompressorNum ),
			SecondaryNum( SecondaryNum ),
			SubcoolerNum( SubcoolerNum ),
			WalkInNum( WalkInNum ),
			CompSuctControl( CompSuctControl ),
			HiStageWarnIndex1( HiStageWarnIndex1 ),
			HiStageWarnIndex2( HiStageWarnIndex2 ),
			InsuffCapWarn( InsuffCapWarn ),
			IntercoolerType( IntercoolerType ),
			NumCases( NumCases ),
			NumCoils( NumCoils ),
			NumCompressors( NumCompressors ),
			NumCondensers( NumCondensers ),
			NumGasCoolers( NumGasCoolers ),
			NumHiStageCompressors( NumHiStageCompressors ),
			NumSecondarys( NumSecondarys ),
			NumStages( NumStages ),
			NumSubcoolers( NumSubcoolers ),
			NumWalkIns( NumWalkIns ),
			NumMechSCServed( NumMechSCServed ),
			NumNonCascadeLoads( NumNonCascadeLoads ),
			NumCascadeLoads( NumCascadeLoads ),
			NumTransferLoads( NumTransferLoads ),
			RefIndex( RefIndex ),
			SuctionPipeActualZoneNum( SuctionPipeActualZoneNum ),
			SuctionPipeZoneNodeNum( SuctionPipeZoneNodeNum ),
			MechSCLoad( MechSCLoad ),
			AverageCompressorCOP( AverageCompressorCOP ),
			CpSatLiqCond( CpSatLiqCond ),
			CpSatVapEvap( CpSatVapEvap ),
			FlowRatioIntercooler( FlowRatioIntercooler ),
			HCaseIn( HCaseIn ),
			HCompIn( HCompIn ),
			HCompOut( HCompOut ),
			HSatLiqCond( HSatLiqCond ),
			HCaseOut( HCaseOut ),
			IntercoolerEffectiveness( IntercoolerEffectiveness ),
			LSHXTrans( LSHXTrans ),
			LSHXTransEnergy( LSHXTransEnergy ),
			NetHeatRejectLoad( NetHeatRejectLoad ),
			NetHeatRejectEnergy( NetHeatRejectEnergy ),
			PIntercooler( PIntercooler ),
			PipeHeatLoad( PipeHeatLoad ),
			PipeHeatEnergy( PipeHeatEnergy ),
			RefMassFlowtoLoads( RefMassFlowtoLoads ),
			RefMassFlowComps( RefMassFlowComps ),
			RefMassFlowHiStageComps( RefMassFlowHiStageComps ),
			RefInventory( RefInventory ),
			SumMechSCLoad( SumMechSCLoad ),
			SumMechSCBenefit( SumMechSCBenefit ),
			SumCascadeCondCredit( SumCascadeCondCredit ),
			SumCascadeLoad( SumCascadeLoad ),
			SumSecondaryLoopLoad( SumSecondaryLoopLoad ),
			SumUASuctionPiping( SumUASuctionPiping ),
			TCaseOut( TCaseOut ),
			TCondense( TCondense ),
			TCompIn( TCompIn ),
			TCondenseMin( TCondenseMin ),
			TCondenseMinInput( TCondenseMinInput ),
			EMSOverrideOnTCondenseMin( EMSOverrideOnTCondenseMin ),
			EMSOverrideValueTCondenseMin( EMSOverrideValueTCondenseMin ),
			TEvapDesign( TEvapDesign ),
			TEvapNeeded( TEvapNeeded ),
			TIntercooler( TIntercooler ),
			TLiqInActual( TLiqInActual ),
			TotalCondDefrostCredit( TotalCondDefrostCredit ),
			TotalCoolingEnergy( TotalCoolingEnergy ),
			TotalCoolingLoad( TotalCoolingLoad ),
			TotalSystemLoad( TotalSystemLoad ),
			TotCompPower( TotCompPower ),
			TotCompElecConsump( TotCompElecConsump ),
			TotCompCapacity( TotCompCapacity ),
			TotCompCoolingEnergy( TotCompCoolingEnergy ),
			TotHiStageCompCapacity( TotHiStageCompCapacity ),
			TotHiStageCompCoolingEnergy( TotHiStageCompCoolingEnergy ),
			TotHiStageCompElecConsump( TotHiStageCompElecConsump ),
			TotHiStageCompPower( TotHiStageCompPower ),
			TotCompElecConsumpTwoStage( TotCompElecConsumpTwoStage ),
			TotRejectHeatRecovered( TotRejectHeatRecovered ),
			TotTransferLoad( TotTransferLoad ),
			TotTransferEnergy( TotTransferEnergy ),
			UnmetEnergy( UnmetEnergy ),
			UnmetHiStageEnergy( UnmetHiStageEnergy ),
			UnmetEnergySaved( UnmetEnergySaved )
		{}

		// Reset Initialization Values to Zeros
		void
		reset_init()
		{
			TotalCoolingLoad = 0.0;
			TotalCondDefrostCredit = 0.0;
			SumSecondaryLoopLoad = 0.0;
			SumMechSCBenefit = 0.0;
			NetHeatRejectLoad = 0.0;
			NetHeatRejectEnergy = 0.0;
			AverageCompressorCOP = 0.0;
			TotCompCapacity = 0.0;
			TotHiStageCompCapacity = 0.0;
			TotCompElecConsump = 0.0;
			TotHiStageCompElecConsump = 0.0;
			TotCompElecConsumpTwoStage = 0.0;
			TotCompPower = 0.0;
			TotHiStageCompPower = 0.0;
			TotCompCoolingEnergy = 0.0;
			TotHiStageCompCoolingEnergy = 0.0;
		}

	};

	struct TransRefrigSystemData
	{
		// Members
		std::string Name; // Name of transcritical CO2 refrigeration system
		std::string RefrigerantName; // Name of refrigerant, must match name in FluidName
		//    (see fluidpropertiesrefdata.idf)
		std::string EndUseSubcategory; // Used for reporting purposes
		bool SystemRejectHeatToZone; // Flag to show air-cooled gas cooler located inside zone
		Array1D_int CaseNumMT; // absolute Index of medium temperature cases (allocated NumCasesMT)
		Array1D_int CaseNumLT; // absolute Index of low temperature cases (allocated NumCasesLT)
		Array1D_int CompressorNumHP; // absolute Index of high pressure compressors (allocated NumCompressorsHP)
		Array1D_int CompressorNumLP; // absolute Index of low pressure compressors (allocated NumCompressorsLP)
		Array1D_int GasCoolerNum; // absolute Index of gas cooler
		Array1D_int WalkInNumMT; // absolute Index of medium temperature walk ins (allocated NumWalkInsMT)
		Array1D_int WalkInNumLT; // absolute Index of low temperature walk ins (allocated NumWalkInsLT)
		int NumCasesLT; // Number of low temperature cases on this system
		int NumCasesMT; // Number of medium temperature cases on this system
		int NumCompressorsHP; // Number of high pressure compressors on this system
		int NumCompressorsLP; // Number of low pressure compressors on this system
		int NumGasCoolers; // Number of gas coolers on this system
		int NumWalkInsLT; // Number of low temperature walk in coolers on this system
		int NumWalkInsMT; // Number of medium temperature walk in coolers on this system
		int RefIndex; // Index number of refrigerant, automatically assigned on first call to fluid property
		//   and used thereafter
		int SuctionPipeActualZoneNumMT; // ID number for zone where medium temperature suction pipes gain heat
		int SuctionPipeZoneNodeNumMT; // ID number for zone node where medium temperature suction pipes gain heat
		int SuctionPipeActualZoneNumLT; // ID number for zone where medium temperature suction pipes gain heat
		int SuctionPipeZoneNodeNumLT; // ID number for zone node where medium temperature suction pipes gain heat
		int TransSysType; // Transcritical refrigeration system type: SingleStage = 1, TwoStage=2
		Real64 AverageCompressorCOP; // Average COP for compressers on this system (W)
		Real64 CpSatLiqCond; // Spec Heat of sat liquid at condensing pressure  (J/kg-C)
		Real64 CpSatVapEvapMT; // Spec Heat of saturated vapor exiting medium temperature evaporator (J/kg-C)
		Real64 CpSatVapEvapLT; // Spec Heat of saturated vapor exiting low temperature evaporator (J/kg-C)
		Real64 CpSatLiqReceiver; // Spec Heat of saturated liquid in receiver (J/kg-C)
		Real64 DelHSubcoolerDis; // Change in enthalpy across subcooler, hot side (J/kg)
		Real64 DelHSubcoolerSuc; // Change in enthalpy across subcooler, cold side (J/kg)
		Real64 HCaseInMT; // Medium temperature case inlet enthalpy (after subcoolers and pipe P drops) (J/kg)
		Real64 HCaseInLT; // Low temperature case inlet enthalpy (after pipe P drops) (J/kg)
		Real64 HCompInHP; // High pressure compressor inlet enthalpy  (J/kg)
		Real64 HCompInLP; // Low pressure compressor inlet enthalpy  (J/kg)
		Real64 HCompOutHP; // High pressure compressor outlet enthalpy (J/kg)
		Real64 HCompOutLP; // Low pressure compressor outlet enthalpy (J/kg)
		Real64 HSatLiqCond; // Enthalpy of sat liquid at condensing pressure  (J/kg)
		Real64 HSatLiqReceiver; // Enthalpy of sat liquid in receiver (J/kg)
		Real64 HCaseOutMT; // Enthalpy of refrigerant leaving medium temperature cases, after superheat (J/kg)
		Real64 HCaseOutLT; // Enthalpy of refrigerant leaving low temperature cases, after superheat (J/kg)
		Real64 NetHeatRejectLoad; // Portion of TotalCondenser load due to this system (after heat recovery) W
		Real64 NetHeatRejectEnergy; // Portion of TotalCondenser energy due to this system (after heat recovery) J
		Real64 PipeHeatLoadMT; // Total medium temperature suction pipe heat gains, optional (W)
		Real64 PipeHeatLoadLT; // Total low temperature suction pipe heat gains, optional (W)
		Real64 PipeHeatEnergy; // Total suction pipe heat gains, optional (J)
		Real64 PipeHeatEnergyMT; // Total medium temperature suction pipe heat gains, optional (J)
		Real64 PipeHeatEnergyLT; // Total low temperature suction pipe heat gains, optional (J)
		Real64 RefMassFlowtoMTLoads; // Refrigerant mass flow through medium temperature cases(kg/s)
		Real64 RefMassFlowtoLTLoads; // Refrigerant mass flow through low temperature cases(kg/s)
		Real64 RefMassFlowCompsHP; // Total system refrigerant mass flow through high pressue compressors(kg/s)
		Real64 RefMassFlowCompsLP; // Total system refrigerant mass flow through low pressue compressors(kg/s)
		Real64 RefMassFlowComps; // Total system refrigerant mass flow through all compressors (kg/s)
		Real64 RefMassFlowReceiverBypass; // Refrigerant mass flow through receiver bypass (kg/s)
		Real64 RefInventory; // Approximate refrigerant inventory entered by user (kg)
		Real64 SCEffectiveness; // Heat exchanger effectiveness of the subcooler
		Real64 SumUASuctionPipingMT; // Sum of U*A for medium temperature suction piping (W/C)
		Real64 SumUASuctionPipingLT; // Sum of U*A for low temperature suction piping (W/C)
		Real64 TCaseOutMT; // Medium temperature case out temperature including case superheat (C)
		Real64 TCaseOutLT; // Low temperature case out temperature including case superheat (C)
		Real64 TCondense; // Condensing temperature (Tsat for P discharge) (C)
		Real64 TReceiver; // Temperature in receiver (Tsat for P receiver) (C)
		Real64 PReceiver; // Pressure in receiver (Psat for T receiver) (C)
		Real64 TCompInHP; // High pressure compressor inlet temperature (after case and LSHX superheat and pipe delta P) (C)
		Real64 TCompInLP; // Low pressure compressor inlet temperature (after case and pipe delta P) (C)
		Real64 TCondenseMin; // Minimum allowed condensing temperature (C)
		Real64 TEvapDesignMT; // Min (on sys) design medium temperature case/walkin/secondary evap temp
		Real64 TEvapDesignLT; // Min (on sys) design low temperature case/walkin/secondary evap temp
		Real64 TEvapNeededMT; // Max MT Case evap temperature to maintain lowest case T on system (C)
		Real64 TEvapNeededLT; // Max LT Case evap temperature to maintain lowest case T on system (C)
		Real64 TLiqInActual; // Actual liquid temperature entering TXV after subcooling (C)
		Real64 TotalCondDefrostCredit; // sum of heat reclaimed for hot gas and hot brine defrost for cases/WI served directly [W]
		Real64 TotalCoolingEnergy; // Total energy of all refrigerated cases and walkins served directly (J)
		Real64 TotalCoolingEnergyMT; // Total energy of all medium temperature refrigerated cases and walkins served directly (J)
		Real64 TotalCoolingEnergyLT; // Total energy of all low temperature refrigerated cases and walkins served directly (J)
		Real64 TotalCoolingLoadMT; // Total medium temperature load of all refrigerated cases and walkins served directly (W)
		Real64 TotalCoolingLoadLT; // Total low temperature load of all refrigerated cases and walkins served directly (W)
		Real64 TotalSystemLoad; // Sum of MT and LT loads, W
		Real64 TotalSystemLoadMT; // Includes medium temperature cases and walk-ins, W
		Real64 TotalSystemLoadLT; // Includes low temperature cases and walk-ins, W
		Real64 TotCompPowerHP; // Total power for high pressure compressers on this system (W)
		Real64 TotCompPowerLP; // Total power for low pressure compressers on this system (W)
		Real64 TotCompElecConsump; // Total Elec consump for compressers on this system (J)
		Real64 TotCompElecConsumpHP; // Total Elec consumption for high pressure compressors on this system (J)
		Real64 TotCompElecConsumpLP; // Total Elec consumption for low pressure compressors on this system (J)
		Real64 TotCompCapacity; // Sum of HP and LP compressor capacity (W)
		Real64 TotCompCapacityHP; // Total design capacity for high pressure compressers on this system (W)
		Real64 TotCompCapacityLP; // Total design capacity for low pressure compressers on this system (W)
		Real64 TotCompCoolingEnergy; // Total cooling energy from compressers on this system (J)
		Real64 TotCompCoolingEnergyHP; // Total cooling energy from high pressure compressers on this system (J)
		Real64 TotCompCoolingEnergyLP; // Total cooling energy from low pressure compressers on this system (J)
		Real64 TotRejectHeatRecovered; // Total reject heat recovered for hot gas or hot brine defrost (W)
		Real64 UnmetEnergy; // Accumulative loads unmet by the LP and HP compressors on this system (J)
		Real64 UnmetEnergyMT; // Accumulative loads unmet by total HP compressors on this system (J)
		Real64 UnmetEnergyLT; // Accumulative loads unmet by total LP compressors on this system (J)
		Real64 UnmetEnergySaved; // Accumulative loads unmet by the LP and HP compressors on this system (J)
		Real64 UnmetEnergySavedMT; // Accumulative loads unmet by total HP compressors on this system (J)
		Real64 UnmetEnergySavedLT; // Accumulative loads unmet by total LP compressors on this system (J)

		// Default Constructor
		TransRefrigSystemData() :
			SystemRejectHeatToZone( false ),
			NumCasesLT( 0 ),
			NumCasesMT( 0 ),
			NumCompressorsHP( 0 ),
			NumCompressorsLP( 0 ),
			NumGasCoolers( 1 ),
			NumWalkInsLT( 0 ),
			NumWalkInsMT( 0 ),
			RefIndex( 0 ),
			SuctionPipeActualZoneNumMT( 0 ),
			SuctionPipeZoneNodeNumMT( 0 ),
			SuctionPipeActualZoneNumLT( 0 ),
			SuctionPipeZoneNodeNumLT( 0 ),
			TransSysType( 0 ),
			AverageCompressorCOP( 0.0 ),
			CpSatLiqCond( 0.0 ),
			CpSatVapEvapMT( 0.0 ),
			CpSatVapEvapLT( 0.0 ),
			CpSatLiqReceiver( 0.0 ),
			DelHSubcoolerDis( 0.0 ),
			DelHSubcoolerSuc( 0.0 ),
			HCaseInMT( 0.0 ),
			HCaseInLT( 0.0 ),
			HCompInHP( 0.0 ),
			HCompInLP( 0.0 ),
			HCompOutHP( 0.0 ),
			HCompOutLP( 0.0 ),
			HSatLiqCond( 0.0 ),
			HSatLiqReceiver( 0.0 ),
			HCaseOutMT( 0.0 ),
			HCaseOutLT( 0.0 ),
			NetHeatRejectLoad( 0.0 ),
			NetHeatRejectEnergy( 0.0 ),
			PipeHeatLoadMT( 0.0 ),
			PipeHeatLoadLT( 0.0 ),
			PipeHeatEnergy( 0.0 ),
			PipeHeatEnergyMT( 0.0 ),
			PipeHeatEnergyLT( 0.0 ),
			RefMassFlowtoMTLoads( 0.0 ),
			RefMassFlowtoLTLoads( 0.0 ),
			RefMassFlowCompsHP( 0.0 ),
			RefMassFlowCompsLP( 0.0 ),
			RefMassFlowComps( 0.0 ),
			RefMassFlowReceiverBypass( 0.0 ),
			RefInventory( 0.0 ),
			SCEffectiveness( 0.0 ),
			SumUASuctionPipingMT( 0.0 ),
			SumUASuctionPipingLT( 0.0 ),
			TCaseOutMT( 0.0 ),
			TCaseOutLT( 0.0 ),
			TCondense( 0.0 ),
			TReceiver( 0.0 ),
			PReceiver( 0.0 ),
			TCompInHP( 0.0 ),
			TCompInLP( 0.0 ),
			TCondenseMin( 0.0 ),
			TEvapDesignMT( 0.0 ),
			TEvapDesignLT( 0.0 ),
			TEvapNeededMT( 0.0 ),
			TEvapNeededLT( 0.0 ),
			TLiqInActual( 0.0 ),
			TotalCondDefrostCredit( 0.0 ),
			TotalCoolingEnergy( 0.0 ),
			TotalCoolingEnergyMT( 0.0 ),
			TotalCoolingEnergyLT( 0.0 ),
			TotalCoolingLoadMT( 0.0 ),
			TotalCoolingLoadLT( 0.0 ),
			TotalSystemLoad( 0.0 ),
			TotalSystemLoadMT( 0.0 ),
			TotalSystemLoadLT( 0.0 ),
			TotCompPowerHP( 0.0 ),
			TotCompPowerLP( 0.0 ),
			TotCompElecConsump( 0.0 ),
			TotCompElecConsumpHP( 0.0 ),
			TotCompElecConsumpLP( 0.0 ),
			TotCompCapacity( 0.0 ),
			TotCompCapacityHP( 0.0 ),
			TotCompCapacityLP( 0.0 ),
			TotCompCoolingEnergy( 0.0 ),
			TotCompCoolingEnergyHP( 0.0 ),
			TotCompCoolingEnergyLP( 0.0 ),
			TotRejectHeatRecovered( 0.0 ),
			UnmetEnergy( 0.0 ),
			UnmetEnergyMT( 0.0 ),
			UnmetEnergyLT( 0.0 ),
			UnmetEnergySaved( 0.0 ),
			UnmetEnergySavedMT( 0.0 ),
			UnmetEnergySavedLT( 0.0 )
		{}

		// Member Constructor
		TransRefrigSystemData(
			std::string const & Name, // Name of transcritical CO2 refrigeration system
			std::string const & RefrigerantName, // Name of refrigerant, must match name in FluidName
			std::string const & EndUseSubcategory, // Used for reporting purposes
			bool const SystemRejectHeatToZone, // Flag to show air-cooled gas cooler located inside zone
			Array1_int const & CaseNumMT, // absolute Index of medium temperature cases (allocated NumCasesMT)
			Array1_int const & CaseNumLT, // absolute Index of low temperature cases (allocated NumCasesLT)
			Array1_int const & CompressorNumHP, // absolute Index of high pressure compressors (allocated NumCompressorsHP)
			Array1_int const & CompressorNumLP, // absolute Index of low pressure compressors (allocated NumCompressorsLP)
			Array1_int const & GasCoolerNum, // absolute Index of gas cooler
			Array1_int const & WalkInNumMT, // absolute Index of medium temperature walk ins (allocated NumWalkInsMT)
			Array1_int const & WalkInNumLT, // absolute Index of low temperature walk ins (allocated NumWalkInsLT)
			int const NumCasesLT, // Number of low temperature cases on this system
			int const NumCasesMT, // Number of medium temperature cases on this system
			int const NumCompressorsHP, // Number of high pressure compressors on this system
			int const NumCompressorsLP, // Number of low pressure compressors on this system
			int const NumGasCoolers, // Number of gas coolers on this system
			int const NumWalkInsLT, // Number of low temperature walk in coolers on this system
			int const NumWalkInsMT, // Number of medium temperature walk in coolers on this system
			int const RefIndex, // Index number of refrigerant, automatically assigned on first call to fluid property
			int const SuctionPipeActualZoneNumMT, // ID number for zone where medium temperature suction pipes gain heat
			int const SuctionPipeZoneNodeNumMT, // ID number for zone node where medium temperature suction pipes gain heat
			int const SuctionPipeActualZoneNumLT, // ID number for zone where medium temperature suction pipes gain heat
			int const SuctionPipeZoneNodeNumLT, // ID number for zone node where medium temperature suction pipes gain heat
			int const TransSysType, // Transcritical refrigeration system type: SingleStage = 1, TwoStage=2
			Real64 const AverageCompressorCOP, // Average COP for compressers on this system (W)
			Real64 const CpSatLiqCond, // Spec Heat of sat liquid at condensing pressure  (J/kg-C)
			Real64 const CpSatVapEvapMT, // Spec Heat of saturated vapor exiting medium temperature evaporator (J/kg-C)
			Real64 const CpSatVapEvapLT, // Spec Heat of saturated vapor exiting low temperature evaporator (J/kg-C)
			Real64 const CpSatLiqReceiver, // Spec Heat of saturated liquid in receiver (J/kg-C)
			Real64 const DelHSubcoolerDis, // Change in enthalpy across subcooler, hot side (J/kg)
			Real64 const DelHSubcoolerSuc, // Change in enthalpy across subcooler, cold side (J/kg)
			Real64 const HCaseInMT, // Medium temperature case inlet enthalpy (after subcoolers and pipe P drops) (J/kg)
			Real64 const HCaseInLT, // Low temperature case inlet enthalpy (after pipe P drops) (J/kg)
			Real64 const HCompInHP, // High pressure compressor inlet enthalpy  (J/kg)
			Real64 const HCompInLP, // Low pressure compressor inlet enthalpy  (J/kg)
			Real64 const HCompOutHP, // High pressure compressor outlet enthalpy (J/kg)
			Real64 const HCompOutLP, // Low pressure compressor outlet enthalpy (J/kg)
			Real64 const HSatLiqCond, // Enthalpy of sat liquid at condensing pressure  (J/kg)
			Real64 const HSatLiqReceiver, // Enthalpy of sat liquid in receiver (J/kg)
			Real64 const HCaseOutMT, // Enthalpy of refrigerant leaving medium temperature cases, after superheat (J/kg)
			Real64 const HCaseOutLT, // Enthalpy of refrigerant leaving low temperature cases, after superheat (J/kg)
			Real64 const NetHeatRejectLoad, // Portion of TotalCondenser load due to this system (after heat recovery) W
			Real64 const NetHeatRejectEnergy, // Portion of TotalCondenser energy due to this system (after heat recovery) J
			Real64 const PipeHeatLoadMT, // Total medium temperature suction pipe heat gains, optional (W)
			Real64 const PipeHeatLoadLT, // Total low temperature suction pipe heat gains, optional (W)
			Real64 const PipeHeatEnergy, // Total suction pipe heat gains, optional (J)
			Real64 const PipeHeatEnergyMT, // Total medium temperature suction pipe heat gains, optional (J)
			Real64 const PipeHeatEnergyLT, // Total low temperature suction pipe heat gains, optional (J)
			Real64 const RefMassFlowtoMTLoads, // Refrigerant mass flow through medium temperature cases(kg/s)
			Real64 const RefMassFlowtoLTLoads, // Refrigerant mass flow through low temperature cases(kg/s)
			Real64 const RefMassFlowCompsHP, // Total system refrigerant mass flow through high pressue compressors(kg/s)
			Real64 const RefMassFlowCompsLP, // Total system refrigerant mass flow through low pressue compressors(kg/s)
			Real64 const RefMassFlowComps, // Total system refrigerant mass flow through all compressors (kg/s)
			Real64 const RefMassFlowReceiverBypass, // Refrigerant mass flow through receiver bypass (kg/s)
			Real64 const RefInventory, // Approximate refrigerant inventory entered by user (kg)
			Real64 const SCEffectiveness, // Heat exchanger effectiveness of the subcooler
			Real64 const SumUASuctionPipingMT, // Sum of U*A for medium temperature suction piping (W/C)
			Real64 const SumUASuctionPipingLT, // Sum of U*A for low temperature suction piping (W/C)
			Real64 const TCaseOutMT, // Medium temperature case out temperature including case superheat (C)
			Real64 const TCaseOutLT, // Low temperature case out temperature including case superheat (C)
			Real64 const TCondense, // Condensing temperature (Tsat for P discharge) (C)
			Real64 const TReceiver, // Temperature in receiver (Tsat for P receiver) (C)
			Real64 const PReceiver, // Pressure in receiver (Psat for T receiver) (C)
			Real64 const TCompInHP, // High pressure compressor inlet temperature (after case and LSHX superheat and pipe delta P) (C)
			Real64 const TCompInLP, // Low pressure compressor inlet temperature (after case and pipe delta P) (C)
			Real64 const TCondenseMin, // Minimum allowed condensing temperature (C)
			Real64 const TEvapDesignMT, // Min (on sys) design medium temperature case/walkin/secondary evap temp
			Real64 const TEvapDesignLT, // Min (on sys) design low temperature case/walkin/secondary evap temp
			Real64 const TEvapNeededMT, // Max MT Case evap temperature to maintain lowest case T on system (C)
			Real64 const TEvapNeededLT, // Max LT Case evap temperature to maintain lowest case T on system (C)
			Real64 const TLiqInActual, // Actual liquid temperature entering TXV after subcooling (C)
			Real64 const TotalCondDefrostCredit, // sum of heat reclaimed for hot gas and hot brine defrost for cases/WI served directly [W]
			Real64 const TotalCoolingEnergy, // Total energy of all refrigerated cases and walkins served directly (J)
			Real64 const TotalCoolingEnergyMT, // Total energy of all medium temperature refrigerated cases and walkins served directly (J)
			Real64 const TotalCoolingEnergyLT, // Total energy of all low temperature refrigerated cases and walkins served directly (J)
			Real64 const TotalCoolingLoadMT, // Total medium temperature load of all refrigerated cases and walkins served directly (W)
			Real64 const TotalCoolingLoadLT, // Total low temperature load of all refrigerated cases and walkins served directly (W)
			Real64 const TotalSystemLoad, // Sum of MT and LT loads, W
			Real64 const TotalSystemLoadMT, // Includes medium temperature cases and walk-ins, W
			Real64 const TotalSystemLoadLT, // Includes low temperature cases and walk-ins, W
			Real64 const TotCompPowerHP, // Total power for high pressure compressers on this system (W)
			Real64 const TotCompPowerLP, // Total power for low pressure compressers on this system (W)
			Real64 const TotCompElecConsump, // Total Elec consump for compressers on this system (J)
			Real64 const TotCompElecConsumpHP, // Total Elec consumption for high pressure compressors on this system (J)
			Real64 const TotCompElecConsumpLP, // Total Elec consumption for low pressure compressors on this system (J)
			Real64 const TotCompCapacity, // Sum of HP and LP compressor capacity (W)
			Real64 const TotCompCapacityHP, // Total design capacity for high pressure compressers on this system (W)
			Real64 const TotCompCapacityLP, // Total design capacity for low pressure compressers on this system (W)
			Real64 const TotCompCoolingEnergy, // Total cooling energy from compressers on this system (J)
			Real64 const TotCompCoolingEnergyHP, // Total cooling energy from high pressure compressers on this system (J)
			Real64 const TotCompCoolingEnergyLP, // Total cooling energy from low pressure compressers on this system (J)
			Real64 const TotRejectHeatRecovered, // Total reject heat recovered for hot gas or hot brine defrost (W)
			Real64 const UnmetEnergy, // Accumulative loads unmet by the LP and HP compressors on this system (J)
			Real64 const UnmetEnergyMT, // Accumulative loads unmet by total HP compressors on this system (J)
			Real64 const UnmetEnergyLT, // Accumulative loads unmet by total LP compressors on this system (J)
			Real64 const UnmetEnergySaved, // Accumulative loads unmet by the LP and HP compressors on this system (J)
			Real64 const UnmetEnergySavedMT, // Accumulative loads unmet by total HP compressors on this system (J)
			Real64 const UnmetEnergySavedLT // Accumulative loads unmet by total LP compressors on this system (J)
		) :
			Name( Name ),
			RefrigerantName( RefrigerantName ),
			EndUseSubcategory( EndUseSubcategory ),
			SystemRejectHeatToZone( SystemRejectHeatToZone ),
			CaseNumMT( CaseNumMT ),
			CaseNumLT( CaseNumLT ),
			CompressorNumHP( CompressorNumHP ),
			CompressorNumLP( CompressorNumLP ),
			GasCoolerNum( GasCoolerNum ),
			WalkInNumMT( WalkInNumMT ),
			WalkInNumLT( WalkInNumLT ),
			NumCasesLT( NumCasesLT ),
			NumCasesMT( NumCasesMT ),
			NumCompressorsHP( NumCompressorsHP ),
			NumCompressorsLP( NumCompressorsLP ),
			NumGasCoolers( NumGasCoolers ),
			NumWalkInsLT( NumWalkInsLT ),
			NumWalkInsMT( NumWalkInsMT ),
			RefIndex( RefIndex ),
			SuctionPipeActualZoneNumMT( SuctionPipeActualZoneNumMT ),
			SuctionPipeZoneNodeNumMT( SuctionPipeZoneNodeNumMT ),
			SuctionPipeActualZoneNumLT( SuctionPipeActualZoneNumLT ),
			SuctionPipeZoneNodeNumLT( SuctionPipeZoneNodeNumLT ),
			TransSysType( TransSysType ),
			AverageCompressorCOP( AverageCompressorCOP ),
			CpSatLiqCond( CpSatLiqCond ),
			CpSatVapEvapMT( CpSatVapEvapMT ),
			CpSatVapEvapLT( CpSatVapEvapLT ),
			CpSatLiqReceiver( CpSatLiqReceiver ),
			DelHSubcoolerDis( DelHSubcoolerDis ),
			DelHSubcoolerSuc( DelHSubcoolerSuc ),
			HCaseInMT( HCaseInMT ),
			HCaseInLT( HCaseInLT ),
			HCompInHP( HCompInHP ),
			HCompInLP( HCompInLP ),
			HCompOutHP( HCompOutHP ),
			HCompOutLP( HCompOutLP ),
			HSatLiqCond( HSatLiqCond ),
			HSatLiqReceiver( HSatLiqReceiver ),
			HCaseOutMT( HCaseOutMT ),
			HCaseOutLT( HCaseOutLT ),
			NetHeatRejectLoad( NetHeatRejectLoad ),
			NetHeatRejectEnergy( NetHeatRejectEnergy ),
			PipeHeatLoadMT( PipeHeatLoadMT ),
			PipeHeatLoadLT( PipeHeatLoadLT ),
			PipeHeatEnergy( PipeHeatEnergy ),
			PipeHeatEnergyMT( PipeHeatEnergyMT ),
			PipeHeatEnergyLT( PipeHeatEnergyLT ),
			RefMassFlowtoMTLoads( RefMassFlowtoMTLoads ),
			RefMassFlowtoLTLoads( RefMassFlowtoLTLoads ),
			RefMassFlowCompsHP( RefMassFlowCompsHP ),
			RefMassFlowCompsLP( RefMassFlowCompsLP ),
			RefMassFlowComps( RefMassFlowComps ),
			RefMassFlowReceiverBypass( RefMassFlowReceiverBypass ),
			RefInventory( RefInventory ),
			SCEffectiveness( SCEffectiveness ),
			SumUASuctionPipingMT( SumUASuctionPipingMT ),
			SumUASuctionPipingLT( SumUASuctionPipingLT ),
			TCaseOutMT( TCaseOutMT ),
			TCaseOutLT( TCaseOutLT ),
			TCondense( TCondense ),
			TReceiver( TReceiver ),
			PReceiver( PReceiver ),
			TCompInHP( TCompInHP ),
			TCompInLP( TCompInLP ),
			TCondenseMin( TCondenseMin ),
			TEvapDesignMT( TEvapDesignMT ),
			TEvapDesignLT( TEvapDesignLT ),
			TEvapNeededMT( TEvapNeededMT ),
			TEvapNeededLT( TEvapNeededLT ),
			TLiqInActual( TLiqInActual ),
			TotalCondDefrostCredit( TotalCondDefrostCredit ),
			TotalCoolingEnergy( TotalCoolingEnergy ),
			TotalCoolingEnergyMT( TotalCoolingEnergyMT ),
			TotalCoolingEnergyLT( TotalCoolingEnergyLT ),
			TotalCoolingLoadMT( TotalCoolingLoadMT ),
			TotalCoolingLoadLT( TotalCoolingLoadLT ),
			TotalSystemLoad( TotalSystemLoad ),
			TotalSystemLoadMT( TotalSystemLoadMT ),
			TotalSystemLoadLT( TotalSystemLoadLT ),
			TotCompPowerHP( TotCompPowerHP ),
			TotCompPowerLP( TotCompPowerLP ),
			TotCompElecConsump( TotCompElecConsump ),
			TotCompElecConsumpHP( TotCompElecConsumpHP ),
			TotCompElecConsumpLP( TotCompElecConsumpLP ),
			TotCompCapacity( TotCompCapacity ),
			TotCompCapacityHP( TotCompCapacityHP ),
			TotCompCapacityLP( TotCompCapacityLP ),
			TotCompCoolingEnergy( TotCompCoolingEnergy ),
			TotCompCoolingEnergyHP( TotCompCoolingEnergyHP ),
			TotCompCoolingEnergyLP( TotCompCoolingEnergyLP ),
			TotRejectHeatRecovered( TotRejectHeatRecovered ),
			UnmetEnergy( UnmetEnergy ),
			UnmetEnergyMT( UnmetEnergyMT ),
			UnmetEnergyLT( UnmetEnergyLT ),
			UnmetEnergySaved( UnmetEnergySaved ),
			UnmetEnergySavedMT( UnmetEnergySavedMT ),
			UnmetEnergySavedLT( UnmetEnergySavedLT )
		{}

		// Reset Initialization Values to Zeros
		void
		reset_init()
		{
			TotalCoolingLoadMT = 0.0;
			TotalCoolingLoadLT = 0.0;
			TotalCondDefrostCredit = 0.0;
			NetHeatRejectLoad = 0.0;
			NetHeatRejectEnergy = 0.0;
			AverageCompressorCOP = 0.0;
			TotCompCapacityHP = 0.0;
			TotCompCapacityLP = 0.0;
			TotCompElecConsump = 0.0;
			TotCompPowerHP = 0.0;
			TotCompPowerLP = 0.0;
			TotCompCoolingEnergy = 0.0;
		}

	};

	struct CaseAndWalkInListDef // Derived Type for CaseAndWalkIn Lists
	{
		// Members
		std::string Name; // Name of this CaseAndWalkIn List
		int NumCases; // Number of Cases in this CaseAndWalkIn List
		int NumCoils; // Number of Coils in this CaseAndWalkIn List
		int NumWalkIns; // Number of WalkIns in this CaseAndWalkIn List
		Array1D_int CaseItemNum; // List of Item numbers that correspond to each Case
		Array1D_int CoilItemNum; // List of Item numbers that correspond to each Coil
		Array1D_int WalkInItemNum; // List of Item numbers that correspond to each WalkIn

		// Default Constructor
		CaseAndWalkInListDef() :
			NumCases( 0 ),
			NumCoils( 0 ),
			NumWalkIns( 0 )
		{}

		// Member Constructor
		CaseAndWalkInListDef(
			std::string const & Name, // Name of this CaseAndWalkIn List
			int const NumCases, // Number of Cases in this CaseAndWalkIn List
			int const NumCoils, // Number of Coils in this CaseAndWalkIn List
			int const NumWalkIns, // Number of WalkIns in this CaseAndWalkIn List
			Array1_int const & CaseItemNum, // List of Item numbers that correspond to each Case
			Array1_int const & CoilItemNum, // List of Item numbers that correspond to each Coil
			Array1_int const & WalkInItemNum // List of Item numbers that correspond to each WalkIn
		) :
			Name( Name ),
			NumCases( NumCases ),
			NumCoils( NumCoils ),
			NumWalkIns( NumWalkIns ),
			CaseItemNum( CaseItemNum ),
			CoilItemNum( CoilItemNum ),
			WalkInItemNum( WalkInItemNum )
		{}

	};

	struct CompressorListDef // Derived Type for Compressor Lists
	{
		// Members
		std::string Name; // Name of this Compressor List
		int NumCompressors; // Number of Compressors in this Node List
		Array1D_int CompItemNum; // List of Item numbers that correspond to the compressors

		// Default Constructor
		CompressorListDef() :
			NumCompressors( 0 )
		{}

		// Member Constructor
		CompressorListDef(
			std::string const & Name, // Name of this Compressor List
			int const NumCompressors, // Number of Compressors in this Node List
			Array1_int const & CompItemNum // List of Item numbers that correspond to the compressors
		) :
			Name( Name ),
			NumCompressors( NumCompressors ),
			CompItemNum( CompItemNum )
		{}

	};

	struct RefrigCondenserData
	{
		// Members
		std::string Name; // Name of condenser
		std::string SupplyTankName; // Evap water supply tank name
		std::string EndUseSubcategory; // Rack end-use subcategory
		bool CondenserRejectHeatToZone; // Flag to show air-cooled condenser located inside zone
		bool CoilFlag; // Flag to show if coil type load on system served by condenser
		Array1D_int SysNum; // absolute Index of system placing load (allocated NumRefrigSystems)
		int NumSysAttach; // Number of systems attached to condenser, error if /=1
		int CondenserType; // Specifies cooling mode for condenser
		// (1=Dry air, 2=Evap cooling, 3=Water-cooled, 4=Cascade)
		int EvapFreezeWarnIndex; // Recurring freeze warning index
		int FlowType; // Water-cooled condenser loop flow type
		int CondCreditWarnIndex1; // Used to count warnings
		int CondCreditWarnIndex2; // Used to count warnings
		int CondCreditWarnIndex3; // Used to count warnings
		int CondCreditWarnIndex4; // Used to count warnings
		int CondCreditWarnIndex5; // Used to count warnings
		int CondCreditWarnIndex6; // Used to count warnings
		int CondCreditWarnIndex7; // Used to count warnings
		int NoFlowWarnIndex; // No cooling water when needed warning index
		int HighTempWarnIndex; // Water outlet high temp warning index
		int LowTempWarnIndex; // Water outlet low temp warning index
		int HighFlowWarnIndex; // Water outlet high flow warning index
		int HighInletWarnIndex; // Water inlet high temp warning index
		int InletNode; // Water-cooled condenser inlet node number
		int EvapSchedPtr; // Index to the correct evap condenser availability schedule
		int EvapWaterSupplyMode; // Source of water for evap condenser cooling
		int EvapWaterSupTankID; // TankID when evap condenser uses water from storage tank
		int EvapWaterTankDemandARRID; // Demand index when evap condenser uses water from storage tank
		int OutletNode; // Water-cooled condenser outlet node number
		int PlantTypeOfNum; // Water-cooled condenser plant equipment type
		int PlantLoopNum; // Water-cooled condenser plant loop number
		int PlantLoopSideNum; // Water-cooled condenser plant loop side number
		int PlantBranchNum; // Water-cooled condenser plant branch number
		int PlantCompNum; // Water-cooled condenser plant component number
		int OutletTempSchedPtr; // Schedule pointer for condenser outlet temp setting
		int InletAirNodeNum; // Inlet air node number, can be outside or in a zone
		int InletAirZoneNum; // Inlet air zone number, if located in a zone
		int FanSpeedControlType; // fixed, two-speed, or variable
		int CapCurvePtr; // capcity curve pointer for air-cooled condensers
		int CascadeSysID; // System ID number for system rejecting heat to cascade condenser
		int CascadeTempControl; // Determines whether cascade condenser evaporating temperature set by
		// Tevap for other loads on system (=2) or set at a constant (= 1)
		int CascadeSinkSystemID; // System ID number for system absorbing condenser heat
		//INTEGER     :: ServiceType      = 1       ! Index to warehouse or supermarket (only applies to cascade condensers)
		// 1 = supermarket, 2=warehouse
		Real64 CascadeRatedEvapTemp; // Rated evaporating temperature in cascade condenser
		Real64 MinCondLoad; // minimun condenser load for air-cooled cond (W)
		Real64 TempSlope; // slope for deltaT as function of heat rej for air-cooled cond (C/W)
		Real64 EvapEffect; // Effectiveness of evaporative condenser
		Real64 RatedAirFlowRate; // Evaporative condenser air volume flow rate (m3/s)
		Real64 EvapPumpPower; // Evaporative cooling water pump power (W)
		Real64 ActualEvapPumpPower; // Evaporative cooling water pump power, if adjusted (W)
		Real64 EvapPumpConsumption; // Evaporative cooling water pump electric consumption (J)
		Real64 EvapWaterConsumpRate; // Evaporative condenser water consumption rate (m3/s)
		Real64 EvapWaterConsumption; // Evaporative condenser water consumption (m3)
		Real64 BasinHeaterPowerFTempDiff; // Basin heater capacity per degree K below setpoint (W/K)
		Real64 BasinHeaterSetPointTemp; // Setpoint temperature for basin heater operation (C)
		Real64 BasinHeaterPower; // Power demand from basin heater (W)
		Real64 BasinHeaterConsumption; // Electric consumption from basin heater (J)
		Real64 FanMinAirFlowRatio; // Minimum power fraction for fan (dimensionless between 0 and 1.0)
		Real64 RatedFanPower; // Rated Condenser fan power (W)
		Real64 ActualFanPower; // Condenser fan power (W)
		Real64 FanElecEnergy; // Condenser fan electric consumption (J)
		Real64 InletTemp; // Water-cooling condenser inlet temperature (C)
		Real64 OutletTemp; // Water-cooling condenser outlet temperature (C)
		Real64 VolFlowRate; // Water-cooled condenser volumetric flow rate (m3/s)
		Real64 DesVolFlowRate; // Water-cooled condenser design volumetric flow rate (m3/s)
		Real64 MassFlowRate; // Water-cooled condenser water mass flow rate (kg/s)
		Real64 RatedTCondense; // Condenser rated saturated condensing Temperature (C)
		Real64 CondLoad; // Total condenser load (W)
		Real64 CondEnergy; // Condenser energy (J)
		Real64 VolFlowRateMax; // Maximum condenser volumetric flow rate (m3/s)
		Real64 MassFlowRateMax; // Maximum condenser mass flow rate (kg/s)
		Real64 InletTempMin; // Minimum condenser water inlet temperature (C)
		Real64 OutletTempMax; // Maximum condenser water outlet temperature (C)
		Real64 RatedSubcool; // Subcooling included in capacity rating curves (C)
		Real64 RatedDelT; // Rated difference between Tcondense and Tdrybulb for air-cooled (C)
		// Rated difference between Tcondense and Twetbulb for evap-cooled (C)
		Real64 RatedCapacity; // Rated heat rejection capacity (W)
		Real64 RatedWaterInletT; // Rated water inlet temperature (C)
		Real64 RatedApproachT; // Rated approach temperature difference for water-cooled or cascade condenser(C)
		Real64 MinCapFacEvap; // HRCF equation limit
		Real64 MaxCapFacEvap; // HRCF equation limit
		Real64 EvapCoeff1; // First coefficienct in evap condenser approach T difference equn (C)
		Real64 EvapCoeff2; // Second coefficienct in evap condenser approach T difference equn (C)
		Real64 EvapCoeff3; // Third coefficienct in evap condenser approach T difference equn (C)
		Real64 EvapCoeff4; // Fourth coefficienct in evap condenser approach T difference equn (dimensionless)
		Real64 EvapElevFact; // Elevation correction factor for evap condensers
		Real64 RefOpCharge; // Condenser refrigerant operating charge, kg
		Real64 RefReceiverInventory; // Condensate receiver refrigerant inventory, kg
		Real64 RefPipingInventory; // Condensate piping refrigerant inventory, kg
		Real64 TotalHeatRecoveredEnergy; // All recovered heat for external loads and defrost purposes, J
		Real64 TotalHeatRecoveredLoad; // All recovered heat for external loads and defrost purposes [W]
		Real64 ExternalEnergyRecovered; // ExternalHeatRecovered, J
		Real64 InternalEnergyRecovered; // InternalHeatRecovered, J
		Real64 ExternalHeatRecoveredLoad; // Sum of LaggedUsedWaterHeater and LaggedUsedHVACCoil [W]
		Real64 InternalHeatRecoveredLoad; // Sum of all heat recovered for defrost purposes [W]
		Real64 LaggedUsedWaterHeater; // Heat reclaim used to heat water in previous zone/load time step(W)
		Real64 LaggedUsedHVACCoil; // Heat reclaim used to heat HVAC coil in previous zone/load time step(W)

		// Default Constructor
		RefrigCondenserData() :
			EndUseSubcategory( "General" ),
			CondenserRejectHeatToZone( false ),
			CoilFlag( false ),
			NumSysAttach( 0 ),
			CondenserType( 0 ),
			EvapFreezeWarnIndex( 0 ),
			FlowType( 1 ),
			CondCreditWarnIndex1( 0 ),
			CondCreditWarnIndex2( 0 ),
			CondCreditWarnIndex3( 0 ),
			CondCreditWarnIndex4( 0 ),
			CondCreditWarnIndex5( 0 ),
			CondCreditWarnIndex6( 0 ),
			CondCreditWarnIndex7( 0 ),
			NoFlowWarnIndex( 0 ),
			HighTempWarnIndex( 0 ),
			LowTempWarnIndex( 0 ),
			HighFlowWarnIndex( 0 ),
			HighInletWarnIndex( 0 ),
			InletNode( 0 ),
			EvapSchedPtr( 0 ),
			EvapWaterSupplyMode( WaterSupplyFromMains ),
			EvapWaterSupTankID( 0 ),
			EvapWaterTankDemandARRID( 0 ),
			OutletNode( 0 ),
			PlantTypeOfNum( 0 ),
			PlantLoopNum( 0 ),
			PlantLoopSideNum( 0 ),
			PlantBranchNum( 0 ),
			PlantCompNum( 0 ),
			OutletTempSchedPtr( 0 ),
			InletAirNodeNum( 0 ),
			InletAirZoneNum( 0 ),
			FanSpeedControlType( 0 ),
			CapCurvePtr( 0 ),
			CascadeSysID( 0 ),
			CascadeTempControl( 0 ),
			CascadeSinkSystemID( 0 ),
			CascadeRatedEvapTemp( 0.0 ),
			MinCondLoad( 0.0 ),
			TempSlope( 0.0 ),
			EvapEffect( 0.9 ),
			RatedAirFlowRate( 0.0 ),
			EvapPumpPower( 0.0 ),
			ActualEvapPumpPower( 0.0 ),
			EvapPumpConsumption( 0.0 ),
			EvapWaterConsumpRate( 0.0 ),
			EvapWaterConsumption( 0.0 ),
			BasinHeaterPowerFTempDiff( 0.0 ),
			BasinHeaterSetPointTemp( 2.0 ),
			BasinHeaterPower( 0.0 ),
			BasinHeaterConsumption( 0.0 ),
			FanMinAirFlowRatio( 0.0 ),
			RatedFanPower( 0.0 ),
			ActualFanPower( 0.0 ),
			FanElecEnergy( 0.0 ),
			InletTemp( 0.0 ),
			OutletTemp( 0.0 ),
			VolFlowRate( 0.0 ),
			DesVolFlowRate( 0.0 ),
			MassFlowRate( 0.0 ),
			RatedTCondense( 0.0 ),
			CondLoad( 0.0 ),
			CondEnergy( 0.0 ),
			VolFlowRateMax( 0.0 ),
			MassFlowRateMax( 0.0 ),
			InletTempMin( 10.0 ),
			OutletTempMax( 55.0 ),
			RatedSubcool( 0.0 ),
			RatedDelT( 0.0 ),
			RatedCapacity( 0.0 ),
			RatedWaterInletT( 0.0 ),
			RatedApproachT( 0.0 ),
			MinCapFacEvap( 0.0 ),
			MaxCapFacEvap( 0.0 ),
			EvapCoeff1( 0.0 ),
			EvapCoeff2( 0.0 ),
			EvapCoeff3( 0.0 ),
			EvapCoeff4( 0.0 ),
			EvapElevFact( 1.0 ),
			RefOpCharge( 0.0 ),
			RefReceiverInventory( 0.0 ),
			RefPipingInventory( 0.0 ),
			TotalHeatRecoveredEnergy( 0.0 ),
			TotalHeatRecoveredLoad( 0.0 ),
			ExternalEnergyRecovered( 0.0 ),
			InternalEnergyRecovered( 0.0 ),
			ExternalHeatRecoveredLoad( 0.0 ),
			InternalHeatRecoveredLoad( 0.0 ),
			LaggedUsedWaterHeater( 0.0 ),
			LaggedUsedHVACCoil( 0.0 )
		{}

		// Member Constructor
		RefrigCondenserData(
			std::string const & Name, // Name of condenser
			std::string const & SupplyTankName, // Evap water supply tank name
			std::string const & EndUseSubcategory, // Rack end-use subcategory
			bool const CondenserRejectHeatToZone, // Flag to show air-cooled condenser located inside zone
			bool const CoilFlag, // Flag to show if coil type load on system served by condenser
			Array1_int const & SysNum, // absolute Index of system placing load (allocated NumRefrigSystems)
			int const NumSysAttach, // Number of systems attached to condenser, error if /=1
			int const CondenserType, // Specifies cooling mode for condenser
			int const EvapFreezeWarnIndex, // Recurring freeze warning index
			int const FlowType, // Water-cooled condenser loop flow type
			int const CondCreditWarnIndex1, // Used to count warnings
			int const CondCreditWarnIndex2, // Used to count warnings
			int const CondCreditWarnIndex3, // Used to count warnings
			int const CondCreditWarnIndex4, // Used to count warnings
			int const CondCreditWarnIndex5, // Used to count warnings
			int const CondCreditWarnIndex6, // Used to count warnings
			int const CondCreditWarnIndex7, // Used to count warnings
			int const NoFlowWarnIndex, // No cooling water when needed warning index
			int const HighTempWarnIndex, // Water outlet high temp warning index
			int const LowTempWarnIndex, // Water outlet low temp warning index
			int const HighFlowWarnIndex, // Water outlet high flow warning index
			int const HighInletWarnIndex, // Water inlet high temp warning index
			int const InletNode, // Water-cooled condenser inlet node number
			int const EvapSchedPtr, // Index to the correct evap condenser availability schedule
			int const EvapWaterSupplyMode, // Source of water for evap condenser cooling
			int const EvapWaterSupTankID, // TankID when evap condenser uses water from storage tank
			int const EvapWaterTankDemandARRID, // Demand index when evap condenser uses water from storage tank
			int const OutletNode, // Water-cooled condenser outlet node number
			int const PlantTypeOfNum, // Water-cooled condenser plant equipment type
			int const PlantLoopNum, // Water-cooled condenser plant loop number
			int const PlantLoopSideNum, // Water-cooled condenser plant loop side number
			int const PlantBranchNum, // Water-cooled condenser plant branch number
			int const PlantCompNum, // Water-cooled condenser plant component number
			int const OutletTempSchedPtr, // Schedule pointer for condenser outlet temp setting
			int const InletAirNodeNum, // Inlet air node number, can be outside or in a zone
			int const InletAirZoneNum, // Inlet air zone number, if located in a zone
			int const FanSpeedControlType, // fixed, two-speed, or variable
			int const CapCurvePtr, // capcity curve pointer for air-cooled condensers
			int const CascadeSysID, // System ID number for system rejecting heat to cascade condenser
			int const CascadeTempControl, // Determines whether cascade condenser evaporating temperature set by
			int const CascadeSinkSystemID, // System ID number for system absorbing condenser heat
			Real64 const CascadeRatedEvapTemp, // Rated evaporating temperature in cascade condenser
			Real64 const MinCondLoad, // minimun condenser load for air-cooled cond (W)
			Real64 const TempSlope, // slope for deltaT as function of heat rej for air-cooled cond (C/W)
			Real64 const EvapEffect, // Effectiveness of evaporative condenser
			Real64 const RatedAirFlowRate, // Evaporative condenser air volume flow rate (m3/s)
			Real64 const EvapPumpPower, // Evaporative cooling water pump power (W)
			Real64 const ActualEvapPumpPower, // Evaporative cooling water pump power, if adjusted (W)
			Real64 const EvapPumpConsumption, // Evaporative cooling water pump electric consumption (J)
			Real64 const EvapWaterConsumpRate, // Evaporative condenser water consumption rate (m3/s)
			Real64 const EvapWaterConsumption, // Evaporative condenser water consumption (m3)
			Real64 const BasinHeaterPowerFTempDiff, // Basin heater capacity per degree K below setpoint (W/K)
			Real64 const BasinHeaterSetPointTemp, // Setpoint temperature for basin heater operation (C)
			Real64 const BasinHeaterPower, // Power demand from basin heater (W)
			Real64 const BasinHeaterConsumption, // Electric consumption from basin heater (J)
			Real64 const FanMinAirFlowRatio, // Minimum power fraction for fan (dimensionless between 0 and 1.0)
			Real64 const RatedFanPower, // Rated Condenser fan power (W)
			Real64 const ActualFanPower, // Condenser fan power (W)
			Real64 const FanElecEnergy, // Condenser fan electric consumption (J)
			Real64 const InletTemp, // Water-cooling condenser inlet temperature (C)
			Real64 const OutletTemp, // Water-cooling condenser outlet temperature (C)
			Real64 const VolFlowRate, // Water-cooled condenser volumetric flow rate (m3/s)
			Real64 const DesVolFlowRate, // Water-cooled condenser design volumetric flow rate (m3/s)
			Real64 const MassFlowRate, // Water-cooled condenser water mass flow rate (kg/s)
			Real64 const RatedTCondense, // Condenser rated saturated condensing Temperature (C)
			Real64 const CondLoad, // Total condenser load (W)
			Real64 const CondEnergy, // Condenser energy (J)
			Real64 const VolFlowRateMax, // Maximum condenser volumetric flow rate (m3/s)
			Real64 const MassFlowRateMax, // Maximum condenser mass flow rate (kg/s)
			Real64 const InletTempMin, // Minimum condenser water inlet temperature (C)
			Real64 const OutletTempMax, // Maximum condenser water outlet temperature (C)
			Real64 const RatedSubcool, // Subcooling included in capacity rating curves (C)
			Real64 const RatedDelT, // Rated difference between Tcondense and Tdrybulb for air-cooled (C)
			Real64 const RatedCapacity, // Rated heat rejection capacity (W)
			Real64 const RatedWaterInletT, // Rated water inlet temperature (C)
			Real64 const RatedApproachT, // Rated approach temperature difference for water-cooled or cascade condenser(C)
			Real64 const MinCapFacEvap, // HRCF equation limit
			Real64 const MaxCapFacEvap, // HRCF equation limit
			Real64 const EvapCoeff1, // First coefficienct in evap condenser approach T difference equn (C)
			Real64 const EvapCoeff2, // Second coefficienct in evap condenser approach T difference equn (C)
			Real64 const EvapCoeff3, // Third coefficienct in evap condenser approach T difference equn (C)
			Real64 const EvapCoeff4, // Fourth coefficienct in evap condenser approach T difference equn (dimensionless)
			Real64 const EvapElevFact, // Elevation correction factor for evap condensers
			Real64 const RefOpCharge, // Condenser refrigerant operating charge, kg
			Real64 const RefReceiverInventory, // Condensate receiver refrigerant inventory, kg
			Real64 const RefPipingInventory, // Condensate piping refrigerant inventory, kg
			Real64 const TotalHeatRecoveredEnergy, // All recovered heat for external loads and defrost purposes, J
			Real64 const TotalHeatRecoveredLoad, // All recovered heat for external loads and defrost purposes [W]
			Real64 const ExternalEnergyRecovered, // ExternalHeatRecovered, J
			Real64 const InternalEnergyRecovered, // InternalHeatRecovered, J
			Real64 const ExternalHeatRecoveredLoad, // Sum of LaggedUsedWaterHeater and LaggedUsedHVACCoil [W]
			Real64 const InternalHeatRecoveredLoad, // Sum of all heat recovered for defrost purposes [W]
			Real64 const LaggedUsedWaterHeater, // Heat reclaim used to heat water in previous zone/load time step(W)
			Real64 const LaggedUsedHVACCoil // Heat reclaim used to heat HVAC coil in previous zone/load time step(W)
		) :
			Name( Name ),
			SupplyTankName( SupplyTankName ),
			EndUseSubcategory( EndUseSubcategory ),
			CondenserRejectHeatToZone( CondenserRejectHeatToZone ),
			CoilFlag( CoilFlag ),
			SysNum( SysNum ),
			NumSysAttach( NumSysAttach ),
			CondenserType( CondenserType ),
			EvapFreezeWarnIndex( EvapFreezeWarnIndex ),
			FlowType( FlowType ),
			CondCreditWarnIndex1( CondCreditWarnIndex1 ),
			CondCreditWarnIndex2( CondCreditWarnIndex2 ),
			CondCreditWarnIndex3( CondCreditWarnIndex3 ),
			CondCreditWarnIndex4( CondCreditWarnIndex4 ),
			CondCreditWarnIndex5( CondCreditWarnIndex5 ),
			CondCreditWarnIndex6( CondCreditWarnIndex6 ),
			CondCreditWarnIndex7( CondCreditWarnIndex7 ),
			NoFlowWarnIndex( NoFlowWarnIndex ),
			HighTempWarnIndex( HighTempWarnIndex ),
			LowTempWarnIndex( LowTempWarnIndex ),
			HighFlowWarnIndex( HighFlowWarnIndex ),
			HighInletWarnIndex( HighInletWarnIndex ),
			InletNode( InletNode ),
			EvapSchedPtr( EvapSchedPtr ),
			EvapWaterSupplyMode( EvapWaterSupplyMode ),
			EvapWaterSupTankID( EvapWaterSupTankID ),
			EvapWaterTankDemandARRID( EvapWaterTankDemandARRID ),
			OutletNode( OutletNode ),
			PlantTypeOfNum( PlantTypeOfNum ),
			PlantLoopNum( PlantLoopNum ),
			PlantLoopSideNum( PlantLoopSideNum ),
			PlantBranchNum( PlantBranchNum ),
			PlantCompNum( PlantCompNum ),
			OutletTempSchedPtr( OutletTempSchedPtr ),
			InletAirNodeNum( InletAirNodeNum ),
			InletAirZoneNum( InletAirZoneNum ),
			FanSpeedControlType( FanSpeedControlType ),
			CapCurvePtr( CapCurvePtr ),
			CascadeSysID( CascadeSysID ),
			CascadeTempControl( CascadeTempControl ),
			CascadeSinkSystemID( CascadeSinkSystemID ),
			CascadeRatedEvapTemp( CascadeRatedEvapTemp ),
			MinCondLoad( MinCondLoad ),
			TempSlope( TempSlope ),
			EvapEffect( EvapEffect ),
			RatedAirFlowRate( RatedAirFlowRate ),
			EvapPumpPower( EvapPumpPower ),
			ActualEvapPumpPower( ActualEvapPumpPower ),
			EvapPumpConsumption( EvapPumpConsumption ),
			EvapWaterConsumpRate( EvapWaterConsumpRate ),
			EvapWaterConsumption( EvapWaterConsumption ),
			BasinHeaterPowerFTempDiff( BasinHeaterPowerFTempDiff ),
			BasinHeaterSetPointTemp( BasinHeaterSetPointTemp ),
			BasinHeaterPower( BasinHeaterPower ),
			BasinHeaterConsumption( BasinHeaterConsumption ),
			FanMinAirFlowRatio( FanMinAirFlowRatio ),
			RatedFanPower( RatedFanPower ),
			ActualFanPower( ActualFanPower ),
			FanElecEnergy( FanElecEnergy ),
			InletTemp( InletTemp ),
			OutletTemp( OutletTemp ),
			VolFlowRate( VolFlowRate ),
			DesVolFlowRate( DesVolFlowRate ),
			MassFlowRate( MassFlowRate ),
			RatedTCondense( RatedTCondense ),
			CondLoad( CondLoad ),
			CondEnergy( CondEnergy ),
			VolFlowRateMax( VolFlowRateMax ),
			MassFlowRateMax( MassFlowRateMax ),
			InletTempMin( InletTempMin ),
			OutletTempMax( OutletTempMax ),
			RatedSubcool( RatedSubcool ),
			RatedDelT( RatedDelT ),
			RatedCapacity( RatedCapacity ),
			RatedWaterInletT( RatedWaterInletT ),
			RatedApproachT( RatedApproachT ),
			MinCapFacEvap( MinCapFacEvap ),
			MaxCapFacEvap( MaxCapFacEvap ),
			EvapCoeff1( EvapCoeff1 ),
			EvapCoeff2( EvapCoeff2 ),
			EvapCoeff3( EvapCoeff3 ),
			EvapCoeff4( EvapCoeff4 ),
			EvapElevFact( EvapElevFact ),
			RefOpCharge( RefOpCharge ),
			RefReceiverInventory( RefReceiverInventory ),
			RefPipingInventory( RefPipingInventory ),
			TotalHeatRecoveredEnergy( TotalHeatRecoveredEnergy ),
			TotalHeatRecoveredLoad( TotalHeatRecoveredLoad ),
			ExternalEnergyRecovered( ExternalEnergyRecovered ),
			InternalEnergyRecovered( InternalEnergyRecovered ),
			ExternalHeatRecoveredLoad( ExternalHeatRecoveredLoad ),
			InternalHeatRecoveredLoad( InternalHeatRecoveredLoad ),
			LaggedUsedWaterHeater( LaggedUsedWaterHeater ),
			LaggedUsedHVACCoil( LaggedUsedHVACCoil )
		{}

		// Reset Initialization Values to Zeros
		void
		reset_init()
		{
			CondLoad = 0.0;
			CondEnergy = 0.0;
			MassFlowRate = 0.0;
			ActualFanPower = 0.0;
			FanElecEnergy = 0.0;
			EvapWaterConsumpRate = 0.0;
			EvapWaterConsumption = 0.0;
			ActualEvapPumpPower = 0.0;
			EvapPumpConsumption = 0.0;
			ExternalHeatRecoveredLoad = 0.0;
			ExternalEnergyRecovered = 0.0;
			InternalHeatRecoveredLoad = 0.0;
			InternalEnergyRecovered = 0.0;
			TotalHeatRecoveredLoad = 0.0;
			TotalHeatRecoveredEnergy = 0.0;
			// LowTempWarn = 0
		}

	};

	struct RefrigGasCoolerData
	{
		// Members
		std::string Name; // Name of gas cooler
		std::string EndUseSubcategory; // Gas cooler end-use subcategory
		bool GasCoolerRejectHeatToZone; // Flag to show gas cooler located inside zone
		bool TransOpFlag; // Flag to show transcritical (vs subcritical) operation of the refrigeration system
		Array1D_int SysNum; // absolute Index of system placing load (allocated NumRefrigSystems)
		int CapCurvePtr; // capcity curve pointer for gas cooler
		int FanSpeedControlType; // fixed, two-speed, or variable
		int GasCoolerCreditWarnIndex; // Used to count warnings
		int InletAirNodeNum; // Inlet air node number, can be outside or in a zone
		int InletAirZoneNum; // Inlet air zone number, if located in a zone
		int NumSysAttach; // Number of systems attached to gas cooler
		Real64 ActualFanPower; // Actual gas cooler fan power (W)
		Real64 CpGasCoolerOut; // Saturated liquid specific heat at gas cooler outlet (J/kg-C)
		Real64 FanElecEnergy; // Gas cooler fan electric consumption (J)
		Real64 FanMinAirFlowRatio; // Minimum power fraction for fan (dimensionless between 0 and 1.0)
		Real64 GasCoolerApproachT; // Gas cooler approach temperature (C)
		Real64 GasCoolerEnergy; // Gas cooler energy (J)
		Real64 GasCoolerLoad; // Total gas cooler load (W)
		Real64 HGasCoolerOut; // Specific enthalpy at the gas cooler outlet (C)
		Real64 InternalEnergyRecovered; // InternalHeatRecovered, J
		Real64 InternalHeatRecoveredLoad; // Sum of all heat recovered for defrost purposes [W]
		Real64 MinCondLoad; // minimun gas cooler load for air-cooled gas cooler (W)
		Real64 MinCondTemp; // Minimum condensing temperature during subcritical operation (C)
		Real64 PGasCoolerOut; // Optimum pressure at the gas cooler outlet (C)
		Real64 RatedApproachT; // Rated approach temperature difference(C)
		Real64 RatedCapacity; // Rated heat rejection capacity (W)
		Real64 RatedFanPower; // Rated gas cooler fan power (W)
		Real64 RatedOutletP; // Rated gas cooler outlet pressure (Pa)
		Real64 RatedOutletT; // Rated gas cooler outlet temperature (C)
		Real64 RefOpCharge; // Gas cooler refrigerant operating charge, kg
		Real64 RefPipingInventory; // Gas cooler outlet piping refrigerant inventory, kg
		Real64 RefReceiverInventory; // Gas cooler receiver refrigerant inventory, kg
		Real64 SubcriticalTempDiff; // Temperature difference for subcritical operation (C)
		Real64 TempSlope; // slope for deltaT as function of heat rej for gas cooler (C/W)
		Real64 TGasCoolerOut; // Temperature at the gas cooler outlet (C)
		Real64 TotalHeatRecoveredEnergy; // All recovered heat for defrost purposes, J
		Real64 TotalHeatRecoveredLoad; // All recovered heat for defrost purposes [W]
		Real64 TransitionTemperature; // Transition temperature between subcritical and transcritical operation (C)
		//  REAL(r64)   :: ExternalEnergyRecovered = 0.0d0   ! ExternalHeatRecovered, J
		//  REAL(r64)   :: ExternalHeatRecoveredLoad = 0.0d0 ! Sum of LaggedUsedWaterHeater and LaggedUsedHVACCoil [W]
		//  REAL(r64)   :: LaggedUsedWaterHeater =0.0d0      ! Heat reclaim used to heat water in previous zone/load time step(W)
		//  REAL(r64)   :: LaggedUsedHVACCoil =0.0d0         ! Heat reclaim used to heat HVAC coil in previous zone/load time step(W)

		// Default Constructor
		RefrigGasCoolerData() :
			EndUseSubcategory( "General" ),
			GasCoolerRejectHeatToZone( false ),
			TransOpFlag( false ),
			CapCurvePtr( 0 ),
			FanSpeedControlType( 0 ),
			GasCoolerCreditWarnIndex( 0 ),
			InletAirNodeNum( 0 ),
			InletAirZoneNum( 0 ),
			NumSysAttach( 0 ),
			ActualFanPower( 0.0 ),
			CpGasCoolerOut( 0.0 ),
			FanElecEnergy( 0.0 ),
			FanMinAirFlowRatio( 0.0 ),
			GasCoolerApproachT( 3.0 ),
			GasCoolerEnergy( 0.0 ),
			GasCoolerLoad( 0.0 ),
			HGasCoolerOut( 0.0 ),
			InternalEnergyRecovered( 0.0 ),
			InternalHeatRecoveredLoad( 0.0 ),
			MinCondLoad( 0.0 ),
			MinCondTemp( 1.0e1 ),
			PGasCoolerOut( 0.0 ),
			RatedApproachT( 3.0 ),
			RatedCapacity( 0.0 ),
			RatedFanPower( 0.0 ),
			RatedOutletP( 9.0e6 ),
			RatedOutletT( 38.0 ),
			RefOpCharge( 0.0 ),
			RefPipingInventory( 0.0 ),
			RefReceiverInventory( 0.0 ),
			SubcriticalTempDiff( 1.0e1 ),
			TempSlope( 0.0 ),
			TGasCoolerOut( 0.0 ),
			TotalHeatRecoveredEnergy( 0.0 ),
			TotalHeatRecoveredLoad( 0.0 ),
			TransitionTemperature( 0.0 )
		{}

		// Member Constructor
		RefrigGasCoolerData(
			std::string const & Name, // Name of gas cooler
			std::string const & EndUseSubcategory, // Gas cooler end-use subcategory
			bool const GasCoolerRejectHeatToZone, // Flag to show gas cooler located inside zone
			bool const TransOpFlag, // Flag to show transcritical (vs subcritical) operation of the refrigeration system
			Array1_int const & SysNum, // absolute Index of system placing load (allocated NumRefrigSystems)
			int const CapCurvePtr, // capcity curve pointer for gas cooler
			int const FanSpeedControlType, // fixed, two-speed, or variable
			int const GasCoolerCreditWarnIndex, // Used to count warnings
			int const InletAirNodeNum, // Inlet air node number, can be outside or in a zone
			int const InletAirZoneNum, // Inlet air zone number, if located in a zone
			int const NumSysAttach, // Number of systems attached to gas cooler
			Real64 const ActualFanPower, // Actual gas cooler fan power (W)
			Real64 const CpGasCoolerOut, // Saturated liquid specific heat at gas cooler outlet (J/kg-C)
			Real64 const FanElecEnergy, // Gas cooler fan electric consumption (J)
			Real64 const FanMinAirFlowRatio, // Minimum power fraction for fan (dimensionless between 0 and 1.0)
			Real64 const GasCoolerApproachT, // Gas cooler approach temperature (C)
			Real64 const GasCoolerEnergy, // Gas cooler energy (J)
			Real64 const GasCoolerLoad, // Total gas cooler load (W)
			Real64 const HGasCoolerOut, // Specific enthalpy at the gas cooler outlet (C)
			Real64 const InternalEnergyRecovered, // InternalHeatRecovered, J
			Real64 const InternalHeatRecoveredLoad, // Sum of all heat recovered for defrost purposes [W]
			Real64 const MinCondLoad, // minimun gas cooler load for air-cooled gas cooler (W)
			Real64 const MinCondTemp, // Minimum condensing temperature during subcritical operation (C)
			Real64 const PGasCoolerOut, // Optimum pressure at the gas cooler outlet (C)
			Real64 const RatedApproachT, // Rated approach temperature difference(C)
			Real64 const RatedCapacity, // Rated heat rejection capacity (W)
			Real64 const RatedFanPower, // Rated gas cooler fan power (W)
			Real64 const RatedOutletP, // Rated gas cooler outlet pressure (Pa)
			Real64 const RatedOutletT, // Rated gas cooler outlet temperature (C)
			Real64 const RefOpCharge, // Gas cooler refrigerant operating charge, kg
			Real64 const RefPipingInventory, // Gas cooler outlet piping refrigerant inventory, kg
			Real64 const RefReceiverInventory, // Gas cooler receiver refrigerant inventory, kg
			Real64 const SubcriticalTempDiff, // Temperature difference for subcritical operation (C)
			Real64 const TempSlope, // slope for deltaT as function of heat rej for gas cooler (C/W)
			Real64 const TGasCoolerOut, // Temperature at the gas cooler outlet (C)
			Real64 const TotalHeatRecoveredEnergy, // All recovered heat for defrost purposes, J
			Real64 const TotalHeatRecoveredLoad, // All recovered heat for defrost purposes [W]
			Real64 const TransitionTemperature // Transition temperature between subcritical and transcritical operation (C)
		) :
			Name( Name ),
			EndUseSubcategory( EndUseSubcategory ),
			GasCoolerRejectHeatToZone( GasCoolerRejectHeatToZone ),
			TransOpFlag( TransOpFlag ),
			SysNum( SysNum ),
			CapCurvePtr( CapCurvePtr ),
			FanSpeedControlType( FanSpeedControlType ),
			GasCoolerCreditWarnIndex( GasCoolerCreditWarnIndex ),
			InletAirNodeNum( InletAirNodeNum ),
			InletAirZoneNum( InletAirZoneNum ),
			NumSysAttach( NumSysAttach ),
			ActualFanPower( ActualFanPower ),
			CpGasCoolerOut( CpGasCoolerOut ),
			FanElecEnergy( FanElecEnergy ),
			FanMinAirFlowRatio( FanMinAirFlowRatio ),
			GasCoolerApproachT( GasCoolerApproachT ),
			GasCoolerEnergy( GasCoolerEnergy ),
			GasCoolerLoad( GasCoolerLoad ),
			HGasCoolerOut( HGasCoolerOut ),
			InternalEnergyRecovered( InternalEnergyRecovered ),
			InternalHeatRecoveredLoad( InternalHeatRecoveredLoad ),
			MinCondLoad( MinCondLoad ),
			MinCondTemp( MinCondTemp ),
			PGasCoolerOut( PGasCoolerOut ),
			RatedApproachT( RatedApproachT ),
			RatedCapacity( RatedCapacity ),
			RatedFanPower( RatedFanPower ),
			RatedOutletP( RatedOutletP ),
			RatedOutletT( RatedOutletT ),
			RefOpCharge( RefOpCharge ),
			RefPipingInventory( RefPipingInventory ),
			RefReceiverInventory( RefReceiverInventory ),
			SubcriticalTempDiff( SubcriticalTempDiff ),
			TempSlope( TempSlope ),
			TGasCoolerOut( TGasCoolerOut ),
			TotalHeatRecoveredEnergy( TotalHeatRecoveredEnergy ),
			TotalHeatRecoveredLoad( TotalHeatRecoveredLoad ),
			TransitionTemperature( TransitionTemperature )
		{}

		// Reset Initialization Values to Zeros
		void
		reset_init()
		{
			GasCoolerLoad = 0.0;
			GasCoolerEnergy = 0.0;
			ActualFanPower = 0.0;
			FanElecEnergy = 0.0;
			InternalHeatRecoveredLoad = 0.0;
			InternalEnergyRecovered = 0.0;
			TotalHeatRecoveredLoad = 0.0;
			TotalHeatRecoveredEnergy = 0.0;
		}

	};

	struct RefrigCompressorData
	{
		// Members
		bool CoilFlag; // Flag to show if coil type load on system served by compressor
		std::string Name; // Name of compressor
		int CapacityCurvePtr; // Index to the capacity curve object
		int ElecPowerCurvePtr; // Index to the electrical power curve object
		int MassFlowCurvePtr; // Index to the mass flow curve object
		int TransElecPowerCurvePtr; // Index to the transcritical electrical power curve object
		int TransCapacityCurvePtr; // Index to the transcritical capacity curve object
		int NumSysAttach; // Number of systems attached to compressor, error if /=1
		int SuperheatRatingType; // Type of manufacturer's rating info re superheat
		int SubcoolRatingType; // Type of manufacturer's rating info re subcooling
		Real64 Capacity; // Comprssor delivered capacity (W)
		Real64 CoolingEnergy; // Compressor delivered energy (J)
		Real64 Efficiency; // Compressor efficiency (0 to 1)
		Real64 ElecConsumption; // Compressor electric consumption (J)
		Real64 LoadFactor; // Fraction of the time the compressor runs to meet the load (0 to 1)
		Real64 MassFlow; // Compressor mass flow (kg/s)
		Real64 NomCap; // Nominal compressor capacity at ARI 540 rating conditions
		Real64 Power; // Compressor power (W)
		Real64 RatedSuperheat; // Rated Superheat at compressor suction (C)
		Real64 RatedSubcool; // Rated Subcooling, note may not match condenser rating (C)
		std::string EndUseSubcategory; // Compressor end-use subcategory
		bool TransFlag; // Flag to indicate if compressor can operate in transcritical region

		// Default Constructor
		RefrigCompressorData() :
			CoilFlag( false ),
			CapacityCurvePtr( 0 ),
			ElecPowerCurvePtr( 0 ),
			MassFlowCurvePtr( 0 ),
			TransElecPowerCurvePtr( 0 ),
			TransCapacityCurvePtr( 0 ),
			NumSysAttach( 0 ),
			SuperheatRatingType( 0 ),
			SubcoolRatingType( 0 ),
			Capacity( 0.0 ),
			CoolingEnergy( 0.0 ),
			Efficiency( 0.0 ),
			ElecConsumption( 0.0 ),
			LoadFactor( 0.0 ),
			MassFlow( 0.0 ),
			NomCap( 0.0 ),
			Power( 0.0 ),
			RatedSuperheat( 0.0 ),
			RatedSubcool( 0.0 ),
			EndUseSubcategory( "General" ),
			TransFlag( false )
		{}

		// Member Constructor
		RefrigCompressorData(
			bool const CoilFlag, // Flag to show if coil type load on system served by compressor
			std::string const & Name, // Name of compressor
			int const CapacityCurvePtr, // Index to the capacity curve object
			int const ElecPowerCurvePtr, // Index to the electrical power curve object
			int const MassFlowCurvePtr, // Index to the mass flow curve object
			int const TransElecPowerCurvePtr, // Index to the transcritical electrical power curve object
			int const TransCapacityCurvePtr, // Index to the transcritical capacity curve object
			int const NumSysAttach, // Number of systems attached to compressor, error if /=1
			int const SuperheatRatingType, // Type of manufacturer's rating info re superheat
			int const SubcoolRatingType, // Type of manufacturer's rating info re subcooling
			Real64 const Capacity, // Comprssor delivered capacity (W)
			Real64 const CoolingEnergy, // Compressor delivered energy (J)
			Real64 const Efficiency, // Compressor efficiency (0 to 1)
			Real64 const ElecConsumption, // Compressor electric consumption (J)
			Real64 const LoadFactor, // Fraction of the time the compressor runs to meet the load (0 to 1)
			Real64 const MassFlow, // Compressor mass flow (kg/s)
			Real64 const NomCap, // Nominal compressor capacity at ARI 540 rating conditions
			Real64 const Power, // Compressor power (W)
			Real64 const RatedSuperheat, // Rated Superheat at compressor suction (C)
			Real64 const RatedSubcool, // Rated Subcooling, note may not match condenser rating (C)
			std::string const & EndUseSubcategory, // Compressor end-use subcategory
			bool const TransFlag // Flag to indicate if compressor can operate in transcritical region
		) :
			CoilFlag( CoilFlag ),
			Name( Name ),
			CapacityCurvePtr( CapacityCurvePtr ),
			ElecPowerCurvePtr( ElecPowerCurvePtr ),
			MassFlowCurvePtr( MassFlowCurvePtr ),
			TransElecPowerCurvePtr( TransElecPowerCurvePtr ),
			TransCapacityCurvePtr( TransCapacityCurvePtr ),
			NumSysAttach( NumSysAttach ),
			SuperheatRatingType( SuperheatRatingType ),
			SubcoolRatingType( SubcoolRatingType ),
			Capacity( Capacity ),
			CoolingEnergy( CoolingEnergy ),
			Efficiency( Efficiency ),
			ElecConsumption( ElecConsumption ),
			LoadFactor( LoadFactor ),
			MassFlow( MassFlow ),
			NomCap( NomCap ),
			Power( Power ),
			RatedSuperheat( RatedSuperheat ),
			RatedSubcool( RatedSubcool ),
			EndUseSubcategory( EndUseSubcategory ),
			TransFlag( TransFlag )
		{}

		// Reset Initialization Values to Zeros
		void
		reset_init()
		{
			ElecConsumption = 0.0;
			Power = 0.0;
		}

	};

	struct CaseRAFractionData
	{
		// Members
		Real64 TotalCaseRAFraction; // Sum case return air fraction for error checking
		std::string ZoneName; // Zone or Location of Refrigerated Case

		// Default Constructor
		CaseRAFractionData() :
			TotalCaseRAFraction( 0.0 )
		{}

		// Member Constructor
		CaseRAFractionData(
			Real64 const TotalCaseRAFraction, // Sum case return air fraction for error checking
			std::string const & ZoneName // Zone or Location of Refrigerated Case
		) :
			TotalCaseRAFraction( TotalCaseRAFraction ),
			ZoneName( ZoneName )
		{}

	};

	struct SubcoolerData
	{
		// Members
		bool CoilFlag; // Flag to show if coil type load on system served by subcooler
		std::string Name; // Name of Subcooler
		std::string MechSourceSys; // Name of refrigeration system providing
		// cool liquid to mechanical, needed for character comparison after systems read
		int SubcoolerType; // Specifies subcooler type(0=liquid suction heat exchanger,1=mechanical)
		int MechSourceSysID; // ID number of refrigeration system providing cool liquid to mechanical
		Real64 MechSCTransLoad; // Mechanical subcooler load transferred between suction groups, W
		Real64 MechSCTransEnergy; // Mechanical subcooler energy transferred between suction groups, W
		Real64 LiqSuctDesignDelT; // Liquid suction subcooler design subcooling, C
		Real64 LiqSuctDesignTliqIn; // Liquid suction subcooler design inlet temperature liquid, C
		Real64 LiqSuctDesignTvapIn; // Liquid suction subcooler design inlet temperature vapor, C
		Real64 MechControlTliqOut; // Mechanical subcooler design outlet temperature subcooled liquid, C

		// Default Constructor
		SubcoolerData() :
			CoilFlag( false ),
			SubcoolerType( 0 ),
			MechSourceSysID( 0 ),
			MechSCTransLoad( 0.0 ),
			MechSCTransEnergy( 0.0 ),
			LiqSuctDesignDelT( 0.0 ),
			LiqSuctDesignTliqIn( 0.0 ),
			LiqSuctDesignTvapIn( 0.0 ),
			MechControlTliqOut( 0.0 )
		{}

		// Member Constructor
		SubcoolerData(
			bool const CoilFlag, // Flag to show if coil type load on system served by subcooler
			std::string const & Name, // Name of Subcooler
			std::string const & MechSourceSys, // Name of refrigeration system providing
			int const SubcoolerType, // Specifies subcooler type(0=liquid suction heat exchanger,1=mechanical)
			int const MechSourceSysID, // ID number of refrigeration system providing cool liquid to mechanical
			Real64 const MechSCTransLoad, // Mechanical subcooler load transferred between suction groups, W
			Real64 const MechSCTransEnergy, // Mechanical subcooler energy transferred between suction groups, W
			Real64 const LiqSuctDesignDelT, // Liquid suction subcooler design subcooling, C
			Real64 const LiqSuctDesignTliqIn, // Liquid suction subcooler design inlet temperature liquid, C
			Real64 const LiqSuctDesignTvapIn, // Liquid suction subcooler design inlet temperature vapor, C
			Real64 const MechControlTliqOut // Mechanical subcooler design outlet temperature subcooled liquid, C
		) :
			CoilFlag( CoilFlag ),
			Name( Name ),
			MechSourceSys( MechSourceSys ),
			SubcoolerType( SubcoolerType ),
			MechSourceSysID( MechSourceSysID ),
			MechSCTransLoad( MechSCTransLoad ),
			MechSCTransEnergy( MechSCTransEnergy ),
			LiqSuctDesignDelT( LiqSuctDesignDelT ),
			LiqSuctDesignTliqIn( LiqSuctDesignTliqIn ),
			LiqSuctDesignTvapIn( LiqSuctDesignTvapIn ),
			MechControlTliqOut( MechControlTliqOut )
		{}

	};

	struct SecondaryLoopData
	{
		// Members
		bool CoilFlag; // Flag to show if coil type load on secondary system
		std::string Name; // Name of refrigeration system
		std::string FluidName; // Name of circulating fluid
		std::string EndUseSubcategory; // Used for reporting purposes
		Array1D_int CaseNum; // Absolute Index of cases (dimensioned 1 to NumCases)
		Array1D_int CoilNum; // Absolute Index of coils (dimensioned 1 to NumCoils)
		Array1D_int WalkInNum; // Absolute Index of walk-ins (dimensioned 1 to NumWalkIns)
		int DistPipeZoneNum; // ID number for zone where distribution pipe gain heat
		int DistPipeZoneNodeNum; // ID number for zone node where distribution pipe gain heat
		Real64 DistPipeZoneHeatGain; // ! sensible heat gain rate to zone with pipe
		int FluidType; // Indicates whether fluid always liquid or undergoes phase change
		int FluidID; // Numerical ID used for calls to properties subroutine
		int NumSysAttach; // Used to check for non-unique and unused secondary loops
		int NumPumps; // Number of pumps (or pump stages) serving this system
		int NumCases; // Number of Cases served by this secondary loop
		int NumCoils; // Number of Cases served by this secondary loop
		int NumWalkIns; // Number of Walk-Ins served by this secondary loop
		int PumpControlType; // Constant speed or variable speed
		int ReceiverZoneNum; // ID number for zone where receiver gains heat
		int ReceiverZoneNodeNum; // ID number for zone node where receiver gains heat
		Real64 ReceiverZoneHeatGain; // sensible heat gain rate to zone with receiver
		//INTEGER     :: ServiceType        = 1   ! Index to warehouse or supermarket
		// 1 = supermarket, 2=warehouse
		int VarSpeedCurvePtr; // Pointer for variable speed pump power curve
		Real64 AvailLoadCoils; // Used to determine amount of avail heat for warehouse coils
		Real64 CpBrineRated; // Specific heat of secondary loop fluid at rated average
		//    brine temperature (J/kg-C)
		Real64 ChillerRefInventory; // Refrigerant inventory on cold side of loop heat exchanger
		Real64 CircRate; // For PhaseChange loop = mass flow at pump/mass gas out load (dimensionless)
		Real64 CoolingLoadRated; // Rated capacity of heat exchanger serving secondary loop (W)
		Real64 DensityBrineRated; // Density of secondary loop fluid at
		//    rated average brine temperature (J/kg-C)
		Real64 DistPipeHeatGain; // Secondary fluid distribution piping heat gain (W)
		Real64 DistPipeHeatGainEnergy; // Secondary fluid distribution piping heat gain (J)
		Real64 FlowVolActual; // Actual Mass flow rate of circ fluid(kg/s)
		Real64 HotDefrostCondCredit; // Used to credit condenser when heat reclaim used for hot gas/brine defrost (W)
		Real64 HeatExchangeEta; // Heat exchanger effectiveness (dimensionless)
		Real64 MaxVolFlow; // Defined by minimum of chiller or pump ratings (m3/s)
		Real64 MaxLoad; // Defined by minimum of chiller rating or loat at MaxVolFlow (W)
		Real64 PumpTotRatedPower; // Total pump rated power on loop (W)
		Real64 PumpPowerToHeat; // Fraction of pump power converted to heat in circ fluid (dimensionless)
		Real64 PumpIncrementFlowVol; // Circ fluid flow for each pump or pump stage (m3/s)
		Real64 PumpIncrementPower; // Pump power for each pump or pump stage (W)
		Real64 PumpPowerTotal; // Total Pump Power Secondary Loop (report variable)(W)
		Real64 PumpElecEnergyTotal; // Total pump energy secondary loop (report variable)(W)
		Real64 ReceiverHeatGain; // Secondary fluid Receiver heat gain (W)
		Real64 ReceiverHeatGainEnergy; // Secondary fluid Receiver heat gain (J)
		Real64 RefInventory; // Approximate refrigerant inventory entered by user (kg)
		Real64 SumUADistPiping; // Sum of U*A for secondary fluid dist piping (W/C)
		Real64 SumUAReceiver; // Sum of U*A for secondary fluid receiver (W/C)
		Real64 TBrineAverage; // (C)
		Real64 TBrineInRated; // Entering brine temperature based upon rated range,approach,
		//    and evap Temp (C)
		Real64 TCondense; // Rated condensing temperature for heat exchanger serving
		//    secondary loop with phase change(C)
		Real64 TEvapDesign; // Rated evaporating temperature for heat exchanger serving
		//    secondary loop (C)
		Real64 TApproachDifRated; // Rated approach temperature diff for heat exchanger serving
		//    secondary loop (C)
		Real64 TRangeDifRated; // Rated range temperature diff for heat exchanger serving
		//    secondary loop (C)
		Real64 TMinNeeded; // Lowest Tbrine to case or walk-in needed on loop (C)
		Real64 TotalCoolingLoad; // Total load (cases + walk-ins + pump heat + distribution pipe heat gain)
		//     on this system (W)
		Real64 TotalCoolingEnergy; // Total energy (cases + walk-ins + pump heat + distribution pipe heat gain)
		//    on this system (J)
		Real64 TotalRefrigLoad; // Total load (cases + walk-ins) on this system (W)
		Real64 TotalRefrigEnergy; // Total energy (cases + walk-ins) on this system (J)
		Real64 UnmetEnergy; // Load that is greater than capacity of loop heat exchanger, accumulates (J)
		Real64 UnmetEnergySaved; // Load that is greater than capacity of loop heat exchanger, accumulates (J)

		// Default Constructor
		SecondaryLoopData() :
			CoilFlag( false ),
			DistPipeZoneNum( 0 ),
			DistPipeZoneNodeNum( 0 ),
			DistPipeZoneHeatGain( 0.0 ),
			FluidType( 0 ),
			FluidID( 0 ),
			NumSysAttach( 0 ),
			NumPumps( 0 ),
			NumCases( 0 ),
			NumCoils( 0 ),
			NumWalkIns( 0 ),
			PumpControlType( 0 ),
			ReceiverZoneNum( 0 ),
			ReceiverZoneNodeNum( 0 ),
			ReceiverZoneHeatGain( 0.0 ),
			VarSpeedCurvePtr( 0 ),
			AvailLoadCoils( 0.0 ),
			CpBrineRated( 0.0 ),
			ChillerRefInventory( 0.0 ),
			CircRate( 0.0 ),
			CoolingLoadRated( 0.0 ),
			DensityBrineRated( 0.0 ),
			DistPipeHeatGain( 0.0 ),
			DistPipeHeatGainEnergy( 0.0 ),
			FlowVolActual( 0.0 ),
			HotDefrostCondCredit( 0.0 ),
			HeatExchangeEta( 0.0 ),
			MaxVolFlow( 0.0 ),
			MaxLoad( 0.0 ),
			PumpTotRatedPower( 0.0 ),
			PumpPowerToHeat( 0.0 ),
			PumpIncrementFlowVol( 0.0 ),
			PumpIncrementPower( 0.0 ),
			PumpPowerTotal( 0.0 ),
			PumpElecEnergyTotal( 0.0 ),
			ReceiverHeatGain( 0.0 ),
			ReceiverHeatGainEnergy( 0.0 ),
			RefInventory( 0.0 ),
			SumUADistPiping( 0.0 ),
			SumUAReceiver( 0.0 ),
			TBrineAverage( 0.0 ),
			TBrineInRated( 0.0 ),
			TCondense( 0.0 ),
			TEvapDesign( 0.0 ),
			TApproachDifRated( 0.0 ),
			TRangeDifRated( 0.0 ),
			TMinNeeded( 0.0 ),
			TotalCoolingLoad( 0.0 ),
			TotalCoolingEnergy( 0.0 ),
			TotalRefrigLoad( 0.0 ),
			TotalRefrigEnergy( 0.0 ),
			UnmetEnergy( 0.0 ),
			UnmetEnergySaved( 0.0 )
		{}

		// Member Constructor
		SecondaryLoopData(
			bool const CoilFlag, // Flag to show if coil type load on secondary system
			std::string const & Name, // Name of refrigeration system
			std::string const & FluidName, // Name of circulating fluid
			std::string const & EndUseSubcategory, // Used for reporting purposes
			Array1_int const & CaseNum, // Absolute Index of cases (dimensioned 1 to NumCases)
			Array1_int const & CoilNum, // Absolute Index of coils (dimensioned 1 to NumCoils)
			Array1_int const & WalkInNum, // Absolute Index of walk-ins (dimensioned 1 to NumWalkIns)
			int const DistPipeZoneNum, // ID number for zone where distribution pipe gain heat
			int const DistPipeZoneNodeNum, // ID number for zone node where distribution pipe gain heat
			Real64 const DistPipeZoneHeatGain, // ! sensible heat gain rate to zone with pipe
			int const FluidType, // Indicates whether fluid always liquid or undergoes phase change
			int const FluidID, // Numerical ID used for calls to properties subroutine
			int const NumSysAttach, // Used to check for non-unique and unused secondary loops
			int const NumPumps, // Number of pumps (or pump stages) serving this system
			int const NumCases, // Number of Cases served by this secondary loop
			int const NumCoils, // Number of Cases served by this secondary loop
			int const NumWalkIns, // Number of Walk-Ins served by this secondary loop
			int const PumpControlType, // Constant speed or variable speed
			int const ReceiverZoneNum, // ID number for zone where receiver gains heat
			int const ReceiverZoneNodeNum, // ID number for zone node where receiver gains heat
			Real64 const ReceiverZoneHeatGain, // sensible heat gain rate to zone with receiver
			int const VarSpeedCurvePtr, // Pointer for variable speed pump power curve
			Real64 const AvailLoadCoils, // Used to determine amount of avail heat for warehouse coils
			Real64 const CpBrineRated, // Specific heat of secondary loop fluid at rated average
			Real64 const ChillerRefInventory, // Refrigerant inventory on cold side of loop heat exchanger
			Real64 const CircRate, // For PhaseChange loop = mass flow at pump/mass gas out load (dimensionless)
			Real64 const CoolingLoadRated, // Rated capacity of heat exchanger serving secondary loop (W)
			Real64 const DensityBrineRated, // Density of secondary loop fluid at
			Real64 const DistPipeHeatGain, // Secondary fluid distribution piping heat gain (W)
			Real64 const DistPipeHeatGainEnergy, // Secondary fluid distribution piping heat gain (J)
			Real64 const FlowVolActual, // Actual Mass flow rate of circ fluid(kg/s)
			Real64 const HotDefrostCondCredit, // Used to credit condenser when heat reclaim used for hot gas/brine defrost (W)
			Real64 const HeatExchangeEta, // Heat exchanger effectiveness (dimensionless)
			Real64 const MaxVolFlow, // Defined by minimum of chiller or pump ratings (m3/s)
			Real64 const MaxLoad, // Defined by minimum of chiller rating or loat at MaxVolFlow (W)
			Real64 const PumpTotRatedPower, // Total pump rated power on loop (W)
			Real64 const PumpPowerToHeat, // Fraction of pump power converted to heat in circ fluid (dimensionless)
			Real64 const PumpIncrementFlowVol, // Circ fluid flow for each pump or pump stage (m3/s)
			Real64 const PumpIncrementPower, // Pump power for each pump or pump stage (W)
			Real64 const PumpPowerTotal, // Total Pump Power Secondary Loop (report variable)(W)
			Real64 const PumpElecEnergyTotal, // Total pump energy secondary loop (report variable)(W)
			Real64 const ReceiverHeatGain, // Secondary fluid Receiver heat gain (W)
			Real64 const ReceiverHeatGainEnergy, // Secondary fluid Receiver heat gain (J)
			Real64 const RefInventory, // Approximate refrigerant inventory entered by user (kg)
			Real64 const SumUADistPiping, // Sum of U*A for secondary fluid dist piping (W/C)
			Real64 const SumUAReceiver, // Sum of U*A for secondary fluid receiver (W/C)
			Real64 const TBrineAverage, // (C)
			Real64 const TBrineInRated, // Entering brine temperature based upon rated range,approach,
			Real64 const TCondense, // Rated condensing temperature for heat exchanger serving
			Real64 const TEvapDesign, // Rated evaporating temperature for heat exchanger serving
			Real64 const TApproachDifRated, // Rated approach temperature diff for heat exchanger serving
			Real64 const TRangeDifRated, // Rated range temperature diff for heat exchanger serving
			Real64 const TMinNeeded, // Lowest Tbrine to case or walk-in needed on loop (C)
			Real64 const TotalCoolingLoad, // Total load (cases + walk-ins + pump heat + distribution pipe heat gain)
			Real64 const TotalCoolingEnergy, // Total energy (cases + walk-ins + pump heat + distribution pipe heat gain)
			Real64 const TotalRefrigLoad, // Total load (cases + walk-ins) on this system (W)
			Real64 const TotalRefrigEnergy, // Total energy (cases + walk-ins) on this system (J)
			Real64 const UnmetEnergy, // Load that is greater than capacity of loop heat exchanger, accumulates (J)
			Real64 const UnmetEnergySaved // Load that is greater than capacity of loop heat exchanger, accumulates (J)
		) :
			CoilFlag( CoilFlag ),
			Name( Name ),
			FluidName( FluidName ),
			EndUseSubcategory( EndUseSubcategory ),
			CaseNum( CaseNum ),
			CoilNum( CoilNum ),
			WalkInNum( WalkInNum ),
			DistPipeZoneNum( DistPipeZoneNum ),
			DistPipeZoneNodeNum( DistPipeZoneNodeNum ),
			DistPipeZoneHeatGain( DistPipeZoneHeatGain ),
			FluidType( FluidType ),
			FluidID( FluidID ),
			NumSysAttach( NumSysAttach ),
			NumPumps( NumPumps ),
			NumCases( NumCases ),
			NumCoils( NumCoils ),
			NumWalkIns( NumWalkIns ),
			PumpControlType( PumpControlType ),
			ReceiverZoneNum( ReceiverZoneNum ),
			ReceiverZoneNodeNum( ReceiverZoneNodeNum ),
			ReceiverZoneHeatGain( ReceiverZoneHeatGain ),
			VarSpeedCurvePtr( VarSpeedCurvePtr ),
			AvailLoadCoils( AvailLoadCoils ),
			CpBrineRated( CpBrineRated ),
			ChillerRefInventory( ChillerRefInventory ),
			CircRate( CircRate ),
			CoolingLoadRated( CoolingLoadRated ),
			DensityBrineRated( DensityBrineRated ),
			DistPipeHeatGain( DistPipeHeatGain ),
			DistPipeHeatGainEnergy( DistPipeHeatGainEnergy ),
			FlowVolActual( FlowVolActual ),
			HotDefrostCondCredit( HotDefrostCondCredit ),
			HeatExchangeEta( HeatExchangeEta ),
			MaxVolFlow( MaxVolFlow ),
			MaxLoad( MaxLoad ),
			PumpTotRatedPower( PumpTotRatedPower ),
			PumpPowerToHeat( PumpPowerToHeat ),
			PumpIncrementFlowVol( PumpIncrementFlowVol ),
			PumpIncrementPower( PumpIncrementPower ),
			PumpPowerTotal( PumpPowerTotal ),
			PumpElecEnergyTotal( PumpElecEnergyTotal ),
			ReceiverHeatGain( ReceiverHeatGain ),
			ReceiverHeatGainEnergy( ReceiverHeatGainEnergy ),
			RefInventory( RefInventory ),
			SumUADistPiping( SumUADistPiping ),
			SumUAReceiver( SumUAReceiver ),
			TBrineAverage( TBrineAverage ),
			TBrineInRated( TBrineInRated ),
			TCondense( TCondense ),
			TEvapDesign( TEvapDesign ),
			TApproachDifRated( TApproachDifRated ),
			TRangeDifRated( TRangeDifRated ),
			TMinNeeded( TMinNeeded ),
			TotalCoolingLoad( TotalCoolingLoad ),
			TotalCoolingEnergy( TotalCoolingEnergy ),
			TotalRefrigLoad( TotalRefrigLoad ),
			TotalRefrigEnergy( TotalRefrigEnergy ),
			UnmetEnergy( UnmetEnergy ),
			UnmetEnergySaved( UnmetEnergySaved )
		{}

		// Reset Initialization Values to Zeros
		void
		reset_init()
		{
			TotalCoolingLoad = 0.0;
			PumpPowerTotal = 0.0;
			PumpElecEnergyTotal = 0.0;
			ReceiverZoneHeatGain = 0.0;
			DistPipeZoneHeatGain = 0.0;
		}

	};

	struct TransferLoadListDef // Derived Type for Transfer Load (Secondary and Cascade) Lists
	{
		// Members
		std::string Name; // Name of this TransferLoad List
		int NumSecondarys; // Number of Secondary Loops in this TransferLoad List
		int NumCascadeLoads; // Number of Cascade condenser loads in this TransferLoad List
		Array1D_int CascadeLoadItemNum; // List of Item numbers that correspond to the Cascade Condenser
		Array1D_int SecondaryItemNum; // List of Item numbers that correspond to the Secondary

		// Default Constructor
		TransferLoadListDef() :
			NumSecondarys( 0 ),
			NumCascadeLoads( 0 )
		{}

		// Member Constructor
		TransferLoadListDef(
			std::string const & Name, // Name of this TransferLoad List
			int const NumSecondarys, // Number of Secondary Loops in this TransferLoad List
			int const NumCascadeLoads, // Number of Cascade condenser loads in this TransferLoad List
			Array1_int const & CascadeLoadItemNum, // List of Item numbers that correspond to the Cascade Condenser
			Array1_int const & SecondaryItemNum // List of Item numbers that correspond to the Secondary
		) :
			Name( Name ),
			NumSecondarys( NumSecondarys ),
			NumCascadeLoads( NumCascadeLoads ),
			CascadeLoadItemNum( CascadeLoadItemNum ),
			SecondaryItemNum( SecondaryItemNum )
		{}

	};

	struct WalkInData
	{
		// Members
		std::string Name; // Name of walk in cooler
		Array1D_string ZoneName;
		// Names of zones exchanging energy with cooler
		int CircFanSchedPtr; // Index to the correct availability schedule
		int DefrostDripDownSchedPtr; // Index to the correct fail-safe schedule
		int DefrostSchedPtr; // Index to the correct defrost schedule
		int DefrostControlType; // WalkIn defrost control type, Timed,Frost level
		int DefrostType; // WalkIn defrost type, Hot-gas,Electric, Hot-brine
		int HeaterSchedPtr; // Index to the correct availability schedule
		int LightingSchedPtr; // Index to the correct WalkIn lighting schedule
		int NumSysAttach; // Number of systems attached to WalkIn, error if /=1
		int NumZones; // Number of zones exchanging energy with WalkIn
		int SchedPtr; // Index to the correct availability schedule
		int StockingSchedPtr; // Index to the correct product stocking schedule
		Array1D_int GlassDoorOpenSchedPtr; // Index to the door opening schedule
		Array1D_int StockDoorOpenSchedPtr; // Index to the door opening schedule
		Array1D_int StockDoorProtectType; // Index to door protection type
		Array1D_int ZoneNodeNum; // Index to Zone Node
		Array1D_int ZoneNum; // Index to Zone
		Real64 CircFanConsumption; // Operating energy of  Walk In fan [J]
		Real64 CircFanPower; // Operating power of  Walk In fan [W]
		Real64 CoilFanPower; // Operating power of  Walk In evap coil fan [W]
		Real64 IceTemp; // Temperature of Ice Mass [C]
		Real64 IceTempSaved; // Temperature of Ice Mass [C]
		Real64 DefrostCapacity; // Design defrost WalkIn capacity [W]
		Real64 DeltaFreezeKgFrost; // Used to reverse accumulation if the zone/load time step is repeated (kg)
		Real64 DefEnergyFraction; // Portion of defrost energy available to melt ice,
		//    used with fluid defrost with temp termination (dimensionless)
		Real64 DesignFanPower; // Design power of fans [W]
		Real64 DesignLighting; // Design  lighting (includes task and display lights)[W]
		Real64 DesignRatedCap; // Design total capacity [W]
		Real64 DesignRefrigInventory; // Design refrigerant inventory [kg]
		Real64 FloorArea; // Floor area of  Walk In [m2]
		Real64 FloorUValue; // U-value of Walk In floor [W/m2-C]
		Real64 HeaterPower; // Rated power of  Walk In   heaters [W/m]
		Real64 HotDefrostCondCredit; // Used to credit condenser when heat reclaim used for hot gas/brine defrost (W)
		Real64 KgFrost; // Amount of frost on WalkIn evaporator (Kg)
		Real64 StoredEnergy; // Cumulative Stored Energy not met by evaporator [J]
		Real64 KgFrostSaved; // Amount of frost on WalkIn evaporator (Kg)
		Real64 StoredEnergySaved; // Cumulative Stored Energy not met by evaporator [J]
		Real64 Temperature; // Rated temperature [C]
		Real64 TEvapDesign; // Design evaporator temperature (or brine inlet T) [C]
		Real64 TotalFanPower; // Sum of coil and circ fan power  [W]
		Array1D< Real64 > AreaGlassDr;
		Array1D< Real64 > UValueGlassDr;
		Array1D< Real64 > HeightGlassDr;
		Array1D< Real64 > AreaStockDr;
		Array1D< Real64 > UValueStockDr;
		Array1D< Real64 > HeightStockDr;
		Array1D< Real64 > SurfaceArea;
		Array1D< Real64 > UValue;
		// Report Variables
		Real64 ElecHeaterPower; // Walk In heater rate (W)
		Real64 ElecHeaterConsumption; // Walk In heater energy (J)
		Real64 ElecFanPower; // Walk In fan electric power (W)
		Real64 ElecFanConsumption; // Walk In fan electric energy (J)
		Real64 ElecLightingPower; // Walk In lighting electric power (W)
		Real64 ElecLightingConsumption; // Walk In lighting electric energy (J)
		Real64 ElecDefrostPower; // Walk In defrost rate (W)
		Real64 ElecDefrostConsumption; // Walk In defrost energy (J)
		Real64 TotalCoolingLoad; // Walk In total cooling rate (W)
		Real64 TotalCoolingEnergy; // Walk In total cooling energy (J)
		Real64 TotalElecPower; // Walk In total electric
		//   (fans, heaters, lighting, and elec defrost) rate (W)
		Real64 TotalElecConsumption; // Walk In total electric energy (J)
		Real64 TotLatCoolingEnergyRate; // Walk In latent cooling rate (W)
		Real64 TotLatCoolingEnergy; // Walk In latent cooling energy (J)
		Real64 TotSensCoolingEnergyRate; // Walk In sensible cooling rate (W)
		Real64 TotSensCoolingEnergy; // Walk In sensible cooling energy (J)
		Array1D< Real64 > LatZoneCreditRate; // Amount of latent energy provided to zone(W)
		Array1D< Real64 > LatZoneCredit; // Amount of latent energy provided to zone(J)
		Array1D< Real64 > SensZoneCreditRate; // Amount of sensible heat gain to zone, pos and neg (W)
		Array1D< Real64 > SensZoneCreditCoolRate; // Amount of sensible cooling provided to the zone (W)
		Array1D< Real64 > SensZoneCreditCool; // Amount of sensible cooling provided to the zone (J)
		Array1D< Real64 > SensZoneCreditHeatRate; // Amount of sensible heat provided to the zone (W)
		Array1D< Real64 > SensZoneCreditHeat; // Amount of sensible heat provided to the zone (J)

		// Default Constructor
		WalkInData() :
			CircFanSchedPtr( 0 ),
			DefrostDripDownSchedPtr( 0 ),
			DefrostSchedPtr( 0 ),
			DefrostControlType( 0 ),
			DefrostType( 0 ),
			HeaterSchedPtr( 0 ),
			LightingSchedPtr( 0 ),
			NumSysAttach( 0 ),
			NumZones( 0 ),
			SchedPtr( 0 ),
			StockingSchedPtr( 0 ),
			CircFanPower( 0.0 ),
			CoilFanPower( 0.0 ),
			IceTemp( 0.0 ),
			IceTempSaved( 0.0 ),
			DefrostCapacity( 0.0 ),
			DeltaFreezeKgFrost( 0.0 ),
			DefEnergyFraction( 0.0 ),
			DesignFanPower( 0.0 ),
			DesignLighting( 0.0 ),
			DesignRatedCap( 0.0 ),
			DesignRefrigInventory( 0.0 ),
			FloorArea( 0.0 ),
			FloorUValue( 0.0 ),
			HeaterPower( 0.0 ),
			HotDefrostCondCredit( 0.0 ),
			KgFrost( 0.0 ),
			StoredEnergy( 0.0 ),
			KgFrostSaved( 0.0 ),
			StoredEnergySaved( 0.0 ),
			Temperature( 0.0 ),
			TEvapDesign( 0.0 ),
			TotalFanPower( 0.0 ),
			ElecHeaterPower( 0.0 ),
			ElecHeaterConsumption( 0.0 ),
			ElecFanPower( 0.0 ),
			ElecFanConsumption( 0.0 ),
			ElecLightingPower( 0.0 ),
			ElecLightingConsumption( 0.0 ),
			ElecDefrostPower( 0.0 ),
			ElecDefrostConsumption( 0.0 ),
			TotalCoolingLoad( 0.0 ),
			TotalCoolingEnergy( 0.0 ),
			TotalElecPower( 0.0 ),
			TotalElecConsumption( 0.0 ),
			TotLatCoolingEnergyRate( 0.0 ),
			TotLatCoolingEnergy( 0.0 ),
			TotSensCoolingEnergyRate( 0.0 ),
			TotSensCoolingEnergy( 0.0 )
		{}

		// Member Constructor
		WalkInData(
			std::string const & Name, // Name of walk in cooler
			Array1_string const & ZoneName,
			int const CircFanSchedPtr, // Index to the correct availability schedule
			int const DefrostDripDownSchedPtr, // Index to the correct fail-safe schedule
			int const DefrostSchedPtr, // Index to the correct defrost schedule
			int const DefrostControlType, // WalkIn defrost control type, Timed,Frost level
			int const DefrostType, // WalkIn defrost type, Hot-gas,Electric, Hot-brine
			int const HeaterSchedPtr, // Index to the correct availability schedule
			int const LightingSchedPtr, // Index to the correct WalkIn lighting schedule
			int const NumSysAttach, // Number of systems attached to WalkIn, error if /=1
			int const NumZones, // Number of zones exchanging energy with WalkIn
			int const SchedPtr, // Index to the correct availability schedule
			int const StockingSchedPtr, // Index to the correct product stocking schedule
			Array1_int const & GlassDoorOpenSchedPtr, // Index to the door opening schedule
			Array1_int const & StockDoorOpenSchedPtr, // Index to the door opening schedule
			Array1_int const & StockDoorProtectType, // Index to door protection type
			Array1_int const & ZoneNodeNum, // Index to Zone Node
			Array1_int const & ZoneNum, // Index to Zone
			Real64 const CircFanConsumption, // Operating energy of  Walk In fan [J]
			Real64 const CircFanPower, // Operating power of  Walk In fan [W]
			Real64 const CoilFanPower, // Operating power of  Walk In evap coil fan [W]
			Real64 const IceTemp, // Temperature of Ice Mass [C]
			Real64 const IceTempSaved, // Temperature of Ice Mass [C]
			Real64 const DefrostCapacity, // Design defrost WalkIn capacity [W]
			Real64 const DeltaFreezeKgFrost, // Used to reverse accumulation if the zone/load time step is repeated (kg)
			Real64 const DefEnergyFraction, // Portion of defrost energy available to melt ice,
			Real64 const DesignFanPower, // Design power of fans [W]
			Real64 const DesignLighting, // Design  lighting (includes task and display lights)[W]
			Real64 const DesignRatedCap, // Design total capacity [W]
			Real64 const DesignRefrigInventory, // Design refrigerant inventory [kg]
			Real64 const FloorArea, // Floor area of  Walk In [m2]
			Real64 const FloorUValue, // U-value of Walk In floor [W/m2-C]
			Real64 const HeaterPower, // Rated power of  Walk In   heaters [W/m]
			Real64 const HotDefrostCondCredit, // Used to credit condenser when heat reclaim used for hot gas/brine defrost (W)
			Real64 const KgFrost, // Amount of frost on WalkIn evaporator (Kg)
			Real64 const StoredEnergy, // Cumulative Stored Energy not met by evaporator [J]
			Real64 const KgFrostSaved, // Amount of frost on WalkIn evaporator (Kg)
			Real64 const StoredEnergySaved, // Cumulative Stored Energy not met by evaporator [J]
			Real64 const Temperature, // Rated temperature [C]
			Real64 const TEvapDesign, // Design evaporator temperature (or brine inlet T) [C]
			Real64 const TotalFanPower, // Sum of coil and circ fan power  [W]
			Array1< Real64 > const & AreaGlassDr,
			Array1< Real64 > const & UValueGlassDr,
			Array1< Real64 > const & HeightGlassDr,
			Array1< Real64 > const & AreaStockDr,
			Array1< Real64 > const & UValueStockDr,
			Array1< Real64 > const & HeightStockDr,
			Array1< Real64 > const & SurfaceArea,
			Array1< Real64 > const & UValue,
			Real64 const ElecHeaterPower, // Walk In heater rate (W)
			Real64 const ElecHeaterConsumption, // Walk In heater energy (J)
			Real64 const ElecFanPower, // Walk In fan electric power (W)
			Real64 const ElecFanConsumption, // Walk In fan electric energy (J)
			Real64 const ElecLightingPower, // Walk In lighting electric power (W)
			Real64 const ElecLightingConsumption, // Walk In lighting electric energy (J)
			Real64 const ElecDefrostPower, // Walk In defrost rate (W)
			Real64 const ElecDefrostConsumption, // Walk In defrost energy (J)
			Real64 const TotalCoolingLoad, // Walk In total cooling rate (W)
			Real64 const TotalCoolingEnergy, // Walk In total cooling energy (J)
			Real64 const TotalElecPower, // Walk In total electric
			Real64 const TotalElecConsumption, // Walk In total electric energy (J)
			Real64 const TotLatCoolingEnergyRate, // Walk In latent cooling rate (W)
			Real64 const TotLatCoolingEnergy, // Walk In latent cooling energy (J)
			Real64 const TotSensCoolingEnergyRate, // Walk In sensible cooling rate (W)
			Real64 const TotSensCoolingEnergy, // Walk In sensible cooling energy (J)
			Array1< Real64 > const & LatZoneCreditRate, // Amount of latent energy provided to zone(W)
			Array1< Real64 > const & LatZoneCredit, // Amount of latent energy provided to zone(J)
			Array1< Real64 > const & SensZoneCreditRate, // Amount of sensible heat gain to zone, pos and neg (W)
			Array1< Real64 > const & SensZoneCreditCoolRate, // Amount of sensible cooling provided to the zone (W)
			Array1< Real64 > const & SensZoneCreditCool, // Amount of sensible cooling provided to the zone (J)
			Array1< Real64 > const & SensZoneCreditHeatRate, // Amount of sensible heat provided to the zone (W)
			Array1< Real64 > const & SensZoneCreditHeat // Amount of sensible heat provided to the zone (J)
		) :
			Name( Name ),
			ZoneName( ZoneName ),
			CircFanSchedPtr( CircFanSchedPtr ),
			DefrostDripDownSchedPtr( DefrostDripDownSchedPtr ),
			DefrostSchedPtr( DefrostSchedPtr ),
			DefrostControlType( DefrostControlType ),
			DefrostType( DefrostType ),
			HeaterSchedPtr( HeaterSchedPtr ),
			LightingSchedPtr( LightingSchedPtr ),
			NumSysAttach( NumSysAttach ),
			NumZones( NumZones ),
			SchedPtr( SchedPtr ),
			StockingSchedPtr( StockingSchedPtr ),
			GlassDoorOpenSchedPtr( GlassDoorOpenSchedPtr ),
			StockDoorOpenSchedPtr( StockDoorOpenSchedPtr ),
			StockDoorProtectType( StockDoorProtectType ),
			ZoneNodeNum( ZoneNodeNum ),
			ZoneNum( ZoneNum ),
			CircFanConsumption( CircFanConsumption ),
			CircFanPower( CircFanPower ),
			CoilFanPower( CoilFanPower ),
			IceTemp( IceTemp ),
			IceTempSaved( IceTempSaved ),
			DefrostCapacity( DefrostCapacity ),
			DeltaFreezeKgFrost( DeltaFreezeKgFrost ),
			DefEnergyFraction( DefEnergyFraction ),
			DesignFanPower( DesignFanPower ),
			DesignLighting( DesignLighting ),
			DesignRatedCap( DesignRatedCap ),
			DesignRefrigInventory( DesignRefrigInventory ),
			FloorArea( FloorArea ),
			FloorUValue( FloorUValue ),
			HeaterPower( HeaterPower ),
			HotDefrostCondCredit( HotDefrostCondCredit ),
			KgFrost( KgFrost ),
			StoredEnergy( StoredEnergy ),
			KgFrostSaved( KgFrostSaved ),
			StoredEnergySaved( StoredEnergySaved ),
			Temperature( Temperature ),
			TEvapDesign( TEvapDesign ),
			TotalFanPower( TotalFanPower ),
			AreaGlassDr( AreaGlassDr ),
			UValueGlassDr( UValueGlassDr ),
			HeightGlassDr( HeightGlassDr ),
			AreaStockDr( AreaStockDr ),
			UValueStockDr( UValueStockDr ),
			HeightStockDr( HeightStockDr ),
			SurfaceArea( SurfaceArea ),
			UValue( UValue ),
			ElecHeaterPower( ElecHeaterPower ),
			ElecHeaterConsumption( ElecHeaterConsumption ),
			ElecFanPower( ElecFanPower ),
			ElecFanConsumption( ElecFanConsumption ),
			ElecLightingPower( ElecLightingPower ),
			ElecLightingConsumption( ElecLightingConsumption ),
			ElecDefrostPower( ElecDefrostPower ),
			ElecDefrostConsumption( ElecDefrostConsumption ),
			TotalCoolingLoad( TotalCoolingLoad ),
			TotalCoolingEnergy( TotalCoolingEnergy ),
			TotalElecPower( TotalElecPower ),
			TotalElecConsumption( TotalElecConsumption ),
			TotLatCoolingEnergyRate( TotLatCoolingEnergyRate ),
			TotLatCoolingEnergy( TotLatCoolingEnergy ),
			TotSensCoolingEnergyRate( TotSensCoolingEnergyRate ),
			TotSensCoolingEnergy( TotSensCoolingEnergy ),
			LatZoneCreditRate( LatZoneCreditRate ),
			LatZoneCredit( LatZoneCredit ),
			SensZoneCreditRate( SensZoneCreditRate ),
			SensZoneCreditCoolRate( SensZoneCreditCoolRate ),
			SensZoneCreditCool( SensZoneCreditCool ),
			SensZoneCreditHeatRate( SensZoneCreditHeatRate ),
			SensZoneCreditHeat( SensZoneCreditHeat )
		{}

		// Reset Initialization Values to Zeros
		void
		reset_init()
		{
			HotDefrostCondCredit = 0.0;
			TotalCoolingLoad = 0.0;
			TotalCoolingEnergy = 0.0;
			TotSensCoolingEnergyRate = 0.0;
			TotSensCoolingEnergy = 0.0;
			TotLatCoolingEnergyRate = 0.0;
			TotLatCoolingEnergy = 0.0;
			ElecFanPower = 0.0;
			ElecFanConsumption = 0.0;
			ElecHeaterPower = 0.0;
			ElecHeaterConsumption = 0.0;
			ElecLightingPower = 0.0;
			ElecLightingConsumption = 0.0;
			TotalElecPower = 0.0;
			TotalElecConsumption = 0.0;
			ElecDefrostPower = 0.0;
			ElecDefrostConsumption = 0.0;
		}

	};

	struct CaseWIZoneReportData
	{
		// Members
		Real64 LatCoolingToZoneRate; // Positive for reporting Net latent credit to zone on sys time step from cases/walkins (W)
		Real64 LatCoolingToZoneEnergy;
		Real64 SenCoolingToZoneRate; // Positive for reporting Net sensible cooling to zone on sys time step from cases/walkins (W)
		Real64 SenCoolingToZoneEnergy;
		Real64 HeatingToZoneRate; // Positive for reporting Net sensible credit to zone on sys time step from cases/walkins (W)
		Real64 HeatingToZoneEnergy;
		Real64 TotCoolingToZoneRate; // Positive for reporting Net total cooling credit to zone from cases/walkins (W)
		Real64 TotCoolingToZoneEnergy;
		Real64 TotHtXferToZoneRate; // Gives negative for cooling, positive for heating net to zone from cases/walkins (W)
		Real64 TotHtXferToZoneEnergy;
		Real64 SenCaseCreditToZoneEnergy; // Negative (heat out zone) positive (heat into zone) (rate found in CaseCreditData) (J)

		// Default Constructor
		CaseWIZoneReportData() :
			LatCoolingToZoneRate( 0.0 ),
			LatCoolingToZoneEnergy( 0.0 ),
			SenCoolingToZoneRate( 0.0 ),
			SenCoolingToZoneEnergy( 0.0 ),
			HeatingToZoneRate( 0.0 ),
			HeatingToZoneEnergy( 0.0 ),
			TotCoolingToZoneRate( 0.0 ),
			TotCoolingToZoneEnergy( 0.0 ),
			TotHtXferToZoneRate( 0.0 ),
			TotHtXferToZoneEnergy( 0.0 ),
			SenCaseCreditToZoneEnergy( 0.0 )
		{}

		// Member Constructor
		CaseWIZoneReportData(
			Real64 const LatCoolingToZoneRate, // Positive for reporting Net latent credit to zone on sys time step from cases/walkins (W)
			Real64 const LatCoolingToZoneEnergy,
			Real64 const SenCoolingToZoneRate, // Positive for reporting Net sensible cooling to zone on sys time step from cases/walkins (W)
			Real64 const SenCoolingToZoneEnergy,
			Real64 const HeatingToZoneRate, // Positive for reporting Net sensible credit to zone on sys time step from cases/walkins (W)
			Real64 const HeatingToZoneEnergy,
			Real64 const TotCoolingToZoneRate, // Positive for reporting Net total cooling credit to zone from cases/walkins (W)
			Real64 const TotCoolingToZoneEnergy,
			Real64 const TotHtXferToZoneRate, // Gives negative for cooling, positive for heating net to zone from cases/walkins (W)
			Real64 const TotHtXferToZoneEnergy,
			Real64 const SenCaseCreditToZoneEnergy // Negative (heat out zone) positive (heat into zone) (rate found in CaseCreditData) (J)
		) :
			LatCoolingToZoneRate( LatCoolingToZoneRate ),
			LatCoolingToZoneEnergy( LatCoolingToZoneEnergy ),
			SenCoolingToZoneRate( SenCoolingToZoneRate ),
			SenCoolingToZoneEnergy( SenCoolingToZoneEnergy ),
			HeatingToZoneRate( HeatingToZoneRate ),
			HeatingToZoneEnergy( HeatingToZoneEnergy ),
			TotCoolingToZoneRate( TotCoolingToZoneRate ),
			TotCoolingToZoneEnergy( TotCoolingToZoneEnergy ),
			TotHtXferToZoneRate( TotHtXferToZoneRate ),
			TotHtXferToZoneEnergy( TotHtXferToZoneEnergy ),
			SenCaseCreditToZoneEnergy( SenCaseCreditToZoneEnergy )
		{}

		// Reset to Zeros
		void
		reset()
		{
			LatCoolingToZoneRate = 0.0;
			LatCoolingToZoneEnergy = 0.0;
			SenCoolingToZoneRate = 0.0;
			SenCoolingToZoneEnergy = 0.0;
			HeatingToZoneRate = 0.0;
			HeatingToZoneEnergy = 0.0;
			TotCoolingToZoneRate = 0.0;
			TotCoolingToZoneEnergy = 0.0;
			TotHtXferToZoneRate = 0.0;
			TotHtXferToZoneEnergy = 0.0;
			SenCaseCreditToZoneEnergy = 0.0;
		}

	};

	struct WarehouseCoilData
	{
		// Members
		std::string Name; // Name of Warehouse Coil
		std::string ZoneName; // Names of zone cooled by coil
		bool SecStatusFirst; // Flag to show if this is the first coil on a particular secondary
		bool SecStatusLast; // Flag to show if this is the last coil on a particular secondary
		bool SysStatusFirst; // Flag to show if this is the first coil on a particular primary
		bool SysStatusLast; // Flag to show if this is the last coil on a particular primary
		int CoilFanSchedPtr; // Index to the correct availability schedule
		int DefrostDripDownSchedPtr; // Index to the correct fail-safe schedule
		int DefrostSchedPtr; // Index to the correct defrost schedule
		int DefrostControlType; // Coil defrost control type, Timed,Frost level
		int DefrostType; // Coil defrost type, Hot-gas,Electric, Hot-brine
		int FanType; // Index to coil fan type (fixed, two-speed, etc.)
		int HeaterSchedPtr; // Index to the correct availability schedule
		//INTEGER          :: NodeNumInlet=0            ! Node number for inlet to coil
		//INTEGER          :: NodeNumOutlet=0           ! Node number for outlet from coil
		int NumSysAttach; // Number of refrigerating systems cooling this coil (error check purpose)
		int RatingType; // Indicates which type of manufacturer's rating is used
		int SchedPtr; // Index to the correct availability schedule
		int SCIndex; // IDs which of European standard conditions is used for rating
		int SecServeID; // Index to the refrigeration system serving this coil
		int SHRCorrectionType; // Index to type of correction for sensible heat ratio
		int SHRCorrectionCurvePtr; // Index to Sensible heat ratio correction curve
		int SysServeID; // Index to the secondary system serving this coil
		int VerticalLocation; // Index to coil location, floor, ceiling, or middle
		int ZoneNodeNum; // Index to the zone node for the zone served by this coil
		int ZoneNum; // Index to the zone served by this coil
		Real64 CorrMaterial; // Correction factor from manufacturer's rating for coil material, default 1.0
		Real64 CorrRefrigerant; // Correction factor from manufacturer's rating for refrigerant, default 1.0
		Real64 DefrostCapacity; // Design defrost Coil capacity [W]
		Real64 DefrostPower; // Defrost power for electric defrost (W)
		Real64 DeltaFreezeKgFrost; // Used to reverse accumulation if the zone/load time step is repeated (kg)
		Real64 DefEnergyFraction; // Portion of defrost energy available to melt ice,
		//    used with fluid defrost with temp termination (dimensionless)
		Real64 DesignRefrigInventory; // Design refrigerant inventory [kg]
		Real64 FanMinAirFlowRatio; // Minimum air flow ratio set to preserve fan motor, dimensionless
		Real64 HeaterPower; // Rated power of  coil heaters [W/m]
		Real64 HotDefrostCondCredit; // Used to credit condenser when heat reclaim used for hot gas/brine defrost (W)
		Real64 IceTemp; // Temperature of Ice Mass [C]
		Real64 IceTempSaved; // Temperature of Ice Mass [C]
		Real64 KgFrost; // Amount of frost on coil evaporator (Kg)
		Real64 KgFrostSaved; // Amount of frost on coil evaporator (Kg)
		Real64 MaxTemperatureDif; // Maximum difference between Tevap and Tair inlet, limits capacity during initial pull-down (deltaC)
		Real64 RatedAirVolumeFlow; // Rated air flow through coil (m3/s)
		Real64 RatedCapTotal; // Rated total heat capacity, both latent and sensible [W]
		Real64 RatedFanPower; // Rated power of fans [W]
		Real64 RatedRH; // Rated RH corresponding to RatedCapacityTotal [decimal 0 to 1]
		Real64 RatedSensibleCap; // Rated total capacity at sensible heat ratio of 1.0 [W]
		Real64 RatedTemperatureDif; // Rated temperature difference DT1, T air in minus evaporating temperature [W]
		Real64 ReqLoad; // Load requested to meet zone load [W]
		Real64 SensHeatRatio; // Sensible heat ratio (sensible/total), dimensionless
		Real64 SHRCorrection60; // Correction factor corresponding to sensible heat ratio of 0.6 [ dimensionless]
		Real64 Temperature; // Rated temperature [C]
		Real64 TEvapDesign; // Design evaporator temperature (or brine inlet T) [C]
		Real64 ThermalDefrostPower; // Thermal defrost load used to communicate with derate routine even if not electric defrost [W]
		Real64 UnitLoadFactorSens; // Rated sensible capacity [W/C]
		// Report Variables
		Real64 ElecHeaterPower; // Coil heater rate (W)
		Real64 ElecHeaterConsumption; // Coil heater energy (J)
		Real64 ElecFanPower; // Coil fan electric power (W)
		Real64 ElecFanConsumption; // Coil fan electric energy (J)
		Real64 ElecDefrostPower; // Coil defrost rate (W)
		Real64 ElecDefrostConsumption; // Coil defrost energy (J)
		Real64 LatCreditRate; // Latent heat removed from the zone [W]
		Real64 LatLoadServed; // Latent load met by coil (J)
		Real64 LatKgPerS_ToZone; // Latent load met by coil (kg/s)
		Real64 LatCreditEnergy; // Latent heat removed from the zone [J]
		Real64 ReportSensCoolCreditRate; // Coil cooling credit to zone (net) [W]
		Real64 ReportHeatingCreditRate; // Coil heating credit to zone (net) [J]
		Real64 ReportSensCoolCreditEnergy; // Coil cooling credit to zone (net) [W]
		Real64 ReportHeatingCreditEnergy; // Coil heating credit to zone (net) [J]
		Real64 ReportTotalCoolCreditRate; // Coil cooling sens + latent credit to zone[W]
		Real64 ReportTotalCoolCreditEnergy; // Coil cooling sens + latent credit to zone[J]
		Real64 SensCreditRate; // Net Sensible heat removed from the zone [W]
		Real64 SensCreditEnergy; // Net Sensible heat removed from the zone [J]
		Real64 SensCoolingEnergyRate; // Gross Coil sensible cooling rate (W)
		Real64 SensCoolingEnergy; // Gross Coil sensible cooling energy (J)
		Real64 TotalCoolingLoad; // Gross total cooling rate (W)
		Real64 TotalCoolingEnergy; // Gross total cooling energy (J)
		Real64 TotalElecPower; // Coil total electric
		//   (fans, heaters, and elec defrost) rate (W)
		Real64 TotalElecConsumption; // Coil total electric energy (J)

		// Default Constructor
		WarehouseCoilData() :
			SecStatusFirst( false ),
			SecStatusLast( false ),
			SysStatusFirst( false ),
			SysStatusLast( false ),
			CoilFanSchedPtr( 0 ),
			DefrostDripDownSchedPtr( 0 ),
			DefrostSchedPtr( 0 ),
			DefrostControlType( 0 ),
			DefrostType( 0 ),
			FanType( 0 ),
			HeaterSchedPtr( 0 ),
			NumSysAttach( 0 ),
			RatingType( 0 ),
			SchedPtr( 0 ),
			SCIndex( 0 ),
			SecServeID( 0 ),
			SysServeID( 0 ),
			VerticalLocation( 0 ),
			ZoneNodeNum( 0 ),
			ZoneNum( 0 ),
			CorrMaterial( 0.0 ),
			CorrRefrigerant( 0.0 ),
			DefrostCapacity( 0.0 ),
			DefrostPower( 0.0 ),
			DeltaFreezeKgFrost( 0.0 ),
			DefEnergyFraction( 0.0 ),
			DesignRefrigInventory( 0.0 ),
			FanMinAirFlowRatio( 0.0 ),
			HeaterPower( 0.0 ),
			HotDefrostCondCredit( 0.0 ),
			IceTemp( 0.0 ),
			IceTempSaved( 0.0 ),
			KgFrost( 0.0 ),
			KgFrostSaved( 0.0 ),
			MaxTemperatureDif( 0.0 ),
			RatedAirVolumeFlow( 0.0 ),
			RatedCapTotal( 0.0 ),
			RatedFanPower( 0.0 ),
			RatedRH( 0.0 ),
			RatedSensibleCap( 0.0 ),
			RatedTemperatureDif( 0.0 ),
			ReqLoad( 0.0 ),
			SensHeatRatio( 0.0 ),
			SHRCorrection60( 0.0 ),
			Temperature( 0.0 ),
			TEvapDesign( 0.0 ),
			ThermalDefrostPower( 0.0 ),
			UnitLoadFactorSens( 0.0 ),
			ElecHeaterPower( 0.0 ),
			ElecHeaterConsumption( 0.0 ),
			ElecFanPower( 0.0 ),
			ElecFanConsumption( 0.0 ),
			ElecDefrostPower( 0.0 ),
			ElecDefrostConsumption( 0.0 ),
			LatCreditRate( 0.0 ),
			LatLoadServed( 0.0 ),
			LatKgPerS_ToZone( 0.0 ),
			LatCreditEnergy( 0.0 ),
			ReportSensCoolCreditRate( 0.0 ),
			ReportHeatingCreditRate( 0.0 ),
			ReportSensCoolCreditEnergy( 0.0 ),
			ReportHeatingCreditEnergy( 0.0 ),
			ReportTotalCoolCreditRate( 0.0 ),
			ReportTotalCoolCreditEnergy( 0.0 ),
			SensCreditRate( 0.0 ),
			SensCreditEnergy( 0.0 ),
			SensCoolingEnergyRate( 0.0 ),
			SensCoolingEnergy( 0.0 ),
			TotalCoolingLoad( 0.0 ),
			TotalCoolingEnergy( 0.0 ),
			TotalElecPower( 0.0 ),
			TotalElecConsumption( 0.0 )
		{}

		// Member Constructor
		WarehouseCoilData(
			std::string const & Name, // Name of Warehouse Coil
			std::string const & ZoneName, // Names of zone cooled by coil
			bool const SecStatusFirst, // Flag to show if this is the first coil on a particular secondary
			bool const SecStatusLast, // Flag to show if this is the last coil on a particular secondary
			bool const SysStatusFirst, // Flag to show if this is the first coil on a particular primary
			bool const SysStatusLast, // Flag to show if this is the last coil on a particular primary
			int const CoilFanSchedPtr, // Index to the correct availability schedule
			int const DefrostDripDownSchedPtr, // Index to the correct fail-safe schedule
			int const DefrostSchedPtr, // Index to the correct defrost schedule
			int const DefrostControlType, // Coil defrost control type, Timed,Frost level
			int const DefrostType, // Coil defrost type, Hot-gas,Electric, Hot-brine
			int const FanType, // Index to coil fan type (fixed, two-speed, etc.)
			int const HeaterSchedPtr, // Index to the correct availability schedule
			int const NumSysAttach, // Number of refrigerating systems cooling this coil (error check purpose)
			int const RatingType, // Indicates which type of manufacturer's rating is used
			int const SchedPtr, // Index to the correct availability schedule
			int const SCIndex, // IDs which of European standard conditions is used for rating
			int const SecServeID, // Index to the refrigeration system serving this coil
			int const SHRCorrectionType, // Index to type of correction for sensible heat ratio
			int const SHRCorrectionCurvePtr, // Index to Sensible heat ratio correction curve
			int const SysServeID, // Index to the secondary system serving this coil
			int const VerticalLocation, // Index to coil location, floor, ceiling, or middle
			int const ZoneNodeNum, // Index to the zone node for the zone served by this coil
			int const ZoneNum, // Index to the zone served by this coil
			Real64 const CorrMaterial, // Correction factor from manufacturer's rating for coil material, default 1.0
			Real64 const CorrRefrigerant, // Correction factor from manufacturer's rating for refrigerant, default 1.0
			Real64 const DefrostCapacity, // Design defrost Coil capacity [W]
			Real64 const DefrostPower, // Defrost power for electric defrost (W)
			Real64 const DeltaFreezeKgFrost, // Used to reverse accumulation if the zone/load time step is repeated (kg)
			Real64 const DefEnergyFraction, // Portion of defrost energy available to melt ice,
			Real64 const DesignRefrigInventory, // Design refrigerant inventory [kg]
			Real64 const FanMinAirFlowRatio, // Minimum air flow ratio set to preserve fan motor, dimensionless
			Real64 const HeaterPower, // Rated power of  coil heaters [W/m]
			Real64 const HotDefrostCondCredit, // Used to credit condenser when heat reclaim used for hot gas/brine defrost (W)
			Real64 const IceTemp, // Temperature of Ice Mass [C]
			Real64 const IceTempSaved, // Temperature of Ice Mass [C]
			Real64 const KgFrost, // Amount of frost on coil evaporator (Kg)
			Real64 const KgFrostSaved, // Amount of frost on coil evaporator (Kg)
			Real64 const MaxTemperatureDif, // Maximum difference between Tevap and Tair inlet, limits capacity during initial pull-down (deltaC)
			Real64 const RatedAirVolumeFlow, // Rated air flow through coil (m3/s)
			Real64 const RatedCapTotal, // Rated total heat capacity, both latent and sensible [W]
			Real64 const RatedFanPower, // Rated power of fans [W]
			Real64 const RatedRH, // Rated RH corresponding to RatedCapacityTotal [decimal 0 to 1]
			Real64 const RatedSensibleCap, // Rated total capacity at sensible heat ratio of 1.0 [W]
			Real64 const RatedTemperatureDif, // Rated temperature difference DT1, T air in minus evaporating temperature [W]
			Real64 const ReqLoad, // Load requested to meet zone load [W]
			Real64 const SensHeatRatio, // Sensible heat ratio (sensible/total), dimensionless
			Real64 const SHRCorrection60, // Correction factor corresponding to sensible heat ratio of 0.6 [ dimensionless]
			Real64 const Temperature, // Rated temperature [C]
			Real64 const TEvapDesign, // Design evaporator temperature (or brine inlet T) [C]
			Real64 const ThermalDefrostPower, // Thermal defrost load used to communicate with derate routine even if not electric defrost [W]
			Real64 const UnitLoadFactorSens, // Rated sensible capacity [W/C]
			Real64 const ElecHeaterPower, // Coil heater rate (W)
			Real64 const ElecHeaterConsumption, // Coil heater energy (J)
			Real64 const ElecFanPower, // Coil fan electric power (W)
			Real64 const ElecFanConsumption, // Coil fan electric energy (J)
			Real64 const ElecDefrostPower, // Coil defrost rate (W)
			Real64 const ElecDefrostConsumption, // Coil defrost energy (J)
			Real64 const LatCreditRate, // Latent heat removed from the zone [W]
			Real64 const LatLoadServed, // Latent load met by coil (J)
			Real64 const LatKgPerS_ToZone, // Latent load met by coil (kg/s)
			Real64 const LatCreditEnergy, // Latent heat removed from the zone [J]
			Real64 const ReportSensCoolCreditRate, // Coil cooling credit to zone (net) [W]
			Real64 const ReportHeatingCreditRate, // Coil heating credit to zone (net) [J]
			Real64 const ReportSensCoolCreditEnergy, // Coil cooling credit to zone (net) [W]
			Real64 const ReportHeatingCreditEnergy, // Coil heating credit to zone (net) [J]
			Real64 const ReportTotalCoolCreditRate, // Coil cooling sens + latent credit to zone[W]
			Real64 const ReportTotalCoolCreditEnergy, // Coil cooling sens + latent credit to zone[J]
			Real64 const SensCreditRate, // Net Sensible heat removed from the zone [W]
			Real64 const SensCreditEnergy, // Net Sensible heat removed from the zone [J]
			Real64 const SensCoolingEnergyRate, // Gross Coil sensible cooling rate (W)
			Real64 const SensCoolingEnergy, // Gross Coil sensible cooling energy (J)
			Real64 const TotalCoolingLoad, // Gross total cooling rate (W)
			Real64 const TotalCoolingEnergy, // Gross total cooling energy (J)
			Real64 const TotalElecPower, // Coil total electric
			Real64 const TotalElecConsumption // Coil total electric energy (J)
		) :
			Name( Name ),
			ZoneName( ZoneName ),
			SecStatusFirst( SecStatusFirst ),
			SecStatusLast( SecStatusLast ),
			SysStatusFirst( SysStatusFirst ),
			SysStatusLast( SysStatusLast ),
			CoilFanSchedPtr( CoilFanSchedPtr ),
			DefrostDripDownSchedPtr( DefrostDripDownSchedPtr ),
			DefrostSchedPtr( DefrostSchedPtr ),
			DefrostControlType( DefrostControlType ),
			DefrostType( DefrostType ),
			FanType( FanType ),
			HeaterSchedPtr( HeaterSchedPtr ),
			NumSysAttach( NumSysAttach ),
			RatingType( RatingType ),
			SchedPtr( SchedPtr ),
			SCIndex( SCIndex ),
			SecServeID( SecServeID ),
			SHRCorrectionType( SHRCorrectionType ),
			SHRCorrectionCurvePtr( SHRCorrectionCurvePtr ),
			SysServeID( SysServeID ),
			VerticalLocation( VerticalLocation ),
			ZoneNodeNum( ZoneNodeNum ),
			ZoneNum( ZoneNum ),
			CorrMaterial( CorrMaterial ),
			CorrRefrigerant( CorrRefrigerant ),
			DefrostCapacity( DefrostCapacity ),
			DefrostPower( DefrostPower ),
			DeltaFreezeKgFrost( DeltaFreezeKgFrost ),
			DefEnergyFraction( DefEnergyFraction ),
			DesignRefrigInventory( DesignRefrigInventory ),
			FanMinAirFlowRatio( FanMinAirFlowRatio ),
			HeaterPower( HeaterPower ),
			HotDefrostCondCredit( HotDefrostCondCredit ),
			IceTemp( IceTemp ),
			IceTempSaved( IceTempSaved ),
			KgFrost( KgFrost ),
			KgFrostSaved( KgFrostSaved ),
			MaxTemperatureDif( MaxTemperatureDif ),
			RatedAirVolumeFlow( RatedAirVolumeFlow ),
			RatedCapTotal( RatedCapTotal ),
			RatedFanPower( RatedFanPower ),
			RatedRH( RatedRH ),
			RatedSensibleCap( RatedSensibleCap ),
			RatedTemperatureDif( RatedTemperatureDif ),
			ReqLoad( ReqLoad ),
			SensHeatRatio( SensHeatRatio ),
			SHRCorrection60( SHRCorrection60 ),
			Temperature( Temperature ),
			TEvapDesign( TEvapDesign ),
			ThermalDefrostPower( ThermalDefrostPower ),
			UnitLoadFactorSens( UnitLoadFactorSens ),
			ElecHeaterPower( ElecHeaterPower ),
			ElecHeaterConsumption( ElecHeaterConsumption ),
			ElecFanPower( ElecFanPower ),
			ElecFanConsumption( ElecFanConsumption ),
			ElecDefrostPower( ElecDefrostPower ),
			ElecDefrostConsumption( ElecDefrostConsumption ),
			LatCreditRate( LatCreditRate ),
			LatLoadServed( LatLoadServed ),
			LatKgPerS_ToZone( LatKgPerS_ToZone ),
			LatCreditEnergy( LatCreditEnergy ),
			ReportSensCoolCreditRate( ReportSensCoolCreditRate ),
			ReportHeatingCreditRate( ReportHeatingCreditRate ),
			ReportSensCoolCreditEnergy( ReportSensCoolCreditEnergy ),
			ReportHeatingCreditEnergy( ReportHeatingCreditEnergy ),
			ReportTotalCoolCreditRate( ReportTotalCoolCreditRate ),
			ReportTotalCoolCreditEnergy( ReportTotalCoolCreditEnergy ),
			SensCreditRate( SensCreditRate ),
			SensCreditEnergy( SensCreditEnergy ),
			SensCoolingEnergyRate( SensCoolingEnergyRate ),
			SensCoolingEnergy( SensCoolingEnergy ),
			TotalCoolingLoad( TotalCoolingLoad ),
			TotalCoolingEnergy( TotalCoolingEnergy ),
			TotalElecPower( TotalElecPower ),
			TotalElecConsumption( TotalElecConsumption )
		{}

		// Reset Initialization Values to Zeros
		void
		reset_init()
		{
			HotDefrostCondCredit = 0.0;
			TotalCoolingLoad = 0.0;
			TotalCoolingEnergy = 0.0;
			SensCoolingEnergyRate = 0.0;
			SensCoolingEnergy = 0.0;
			SensCreditRate = 0.0;
			LatKgPerS_ToZone = 0.0;
			SensHeatRatio = 0.0;
			LatCreditEnergy = 0.0;
			LatCreditRate = 0.0;
			ElecFanPower = 0.0;
			ElecFanConsumption = 0.0;
			ElecHeaterPower = 0.0;
			ElecHeaterConsumption = 0.0;
			TotalElecPower = 0.0;
			TotalElecConsumption = 0.0;
			ElecDefrostPower = 0.0;
			ElecDefrostConsumption = 0.0;
			ReportTotalCoolCreditRate = 0.0;
			ReportTotalCoolCreditEnergy = 0.0;
			ReportSensCoolCreditRate = 0.0;
			ReportHeatingCreditRate = 0.0;
			ReportSensCoolCreditEnergy = 0.0;
			ReportHeatingCreditEnergy = 0.0;
		}

	};

	struct AirChillerSetData
	{
		// Members
		std::string Name; // Name of Chiller Set
		//CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:)  :: CoilName   ! Name of Individual Chiller in set
		std::string ZoneName; // Name of zone where chiller set is located
		Array1D_int CoilNum; // ID number of Individual Chiller in set
		int ChillerSetID; // ID number for this set of chillers (all serving one zone,
		//                       but can be chilled by mult systems)
		int SchedPtr; // Schedule to take whole set off-line if needed
		int NodeNumInlet; // Node ID Number of inlet for chiller set as a whole, not identified for specific coils
		int NodeNumOutlet; // Node ID Number of outlet for chiller set as a whole, not identified for specific coils
		int NumCoils; // Number of individual chillers in set
		int ZoneNum; // ID number of zone where chiller set is located
		int ZoneNodeNum; // ID number of zone node giving mixed conditions of zone where chiller set is located
		Real64 QZnReqSens; // Sensible heat needed by the zone to reach setpoint [W]

		// Default Constructor
		AirChillerSetData() :
			ChillerSetID( 0 ),
			SchedPtr( 0 ),
			NodeNumInlet( 0 ),
			NodeNumOutlet( 0 ),
			NumCoils( 0 ),
			ZoneNum( 0 ),
			ZoneNodeNum( 0 ),
			QZnReqSens( 0.0 )
		{}

		// Member Constructor
		AirChillerSetData(
			std::string const & Name, // Name of Chiller Set
			std::string const & ZoneName, // Name of zone where chiller set is located
			Array1_int const & CoilNum, // ID number of Individual Chiller in set
			int const ChillerSetID, // ID number for this set of chillers (all serving one zone,
			int const SchedPtr, // Schedule to take whole set off-line if needed
			int const NodeNumInlet, // Node ID Number of inlet for chiller set as a whole, not identified for specific coils
			int const NodeNumOutlet, // Node ID Number of outlet for chiller set as a whole, not identified for specific coils
			int const NumCoils, // Number of individual chillers in set
			int const ZoneNum, // ID number of zone where chiller set is located
			int const ZoneNodeNum, // ID number of zone node giving mixed conditions of zone where chiller set is located
			Real64 const QZnReqSens // Sensible heat needed by the zone to reach setpoint [W]
		) :
			Name( Name ),
			ZoneName( ZoneName ),
			CoilNum( CoilNum ),
			ChillerSetID( ChillerSetID ),
			SchedPtr( SchedPtr ),
			NodeNumInlet( NodeNumInlet ),
			NodeNumOutlet( NodeNumOutlet ),
			NumCoils( NumCoils ),
			ZoneNum( ZoneNum ),
			ZoneNodeNum( ZoneNodeNum ),
			QZnReqSens( QZnReqSens )
		{}

	};

	struct CoilCreditData // used to sum impact of all coils within a zone
	{
		// Members
		Real64 LatCreditToZoneRate; // Net latent credit to zone on sys time step from coil (W)
		Real64 LatCreditToZoneEnergy;
		Real64 LatKgPerS_ToZoneRate; // Latent water to zone on sys time step from coils, neg when water removed (kg/s)
		Real64 SenCreditToZoneRate; // Net sensible credit to zone on sys time step from coil (W)
		Real64 SenCreditToZoneEnergy;
		Real64 ReportH20RemovedKgPerS_FromZoneRate; // same but positive for reporting purposes (kg/s)
		Real64 ReportLatCreditToZoneRate; // Positive for reporting Net latent credit to zone on sys time step from coil (W)
		Real64 ReportLatCreditToZoneEnergy;
		Real64 ReportHeatingToZoneRate; // Positive for reporting Net sensible credit to zone on sys time step from coil (W)
		Real64 ReportHeatingToZoneEnergy;
		Real64 ReportSenCoolingToZoneRate; // Positive for reporting Net sensible credit to zone on sys time step from coil (W)
		Real64 ReportSenCoolingToZoneEnergy;
		Real64 ReportTotCoolingToZoneRate; // Positive for reporting Net total cooling credit to zone from chillers (W)
		Real64 ReportTotCoolingToZoneEnergy;

		// Default Constructor
		CoilCreditData() :
			LatCreditToZoneRate( 0.0 ),
			LatCreditToZoneEnergy( 0.0 ),
			LatKgPerS_ToZoneRate( 0.0 ),
			SenCreditToZoneRate( 0.0 ),
			SenCreditToZoneEnergy( 0.0 ),
			ReportH20RemovedKgPerS_FromZoneRate( 0.0 ),
			ReportLatCreditToZoneRate( 0.0 ),
			ReportLatCreditToZoneEnergy( 0.0 ),
			ReportHeatingToZoneRate( 0.0 ),
			ReportHeatingToZoneEnergy( 0.0 ),
			ReportSenCoolingToZoneRate( 0.0 ),
			ReportSenCoolingToZoneEnergy( 0.0 ),
			ReportTotCoolingToZoneRate( 0.0 ),
			ReportTotCoolingToZoneEnergy( 0.0 )
		{}

		// Member Constructor
		CoilCreditData(
			Real64 const LatCreditToZoneRate, // Net latent credit to zone on sys time step from coil (W)
			Real64 const LatCreditToZoneEnergy,
			Real64 const LatKgPerS_ToZoneRate, // Latent water to zone on sys time step from coils, neg when water removed (kg/s)
			Real64 const SenCreditToZoneRate, // Net sensible credit to zone on sys time step from coil (W)
			Real64 const SenCreditToZoneEnergy,
			Real64 const ReportH20RemovedKgPerS_FromZoneRate, // same but positive for reporting purposes (kg/s)
			Real64 const ReportLatCreditToZoneRate, // Positive for reporting Net latent credit to zone on sys time step from coil (W)
			Real64 const ReportLatCreditToZoneEnergy,
			Real64 const ReportHeatingToZoneRate, // Positive for reporting Net sensible credit to zone on sys time step from coil (W)
			Real64 const ReportHeatingToZoneEnergy,
			Real64 const ReportSenCoolingToZoneRate, // Positive for reporting Net sensible credit to zone on sys time step from coil (W)
			Real64 const ReportSenCoolingToZoneEnergy,
			Real64 const ReportTotCoolingToZoneRate, // Positive for reporting Net total cooling credit to zone from chillers (W)
			Real64 const ReportTotCoolingToZoneEnergy
		) :
			LatCreditToZoneRate( LatCreditToZoneRate ),
			LatCreditToZoneEnergy( LatCreditToZoneEnergy ),
			LatKgPerS_ToZoneRate( LatKgPerS_ToZoneRate ),
			SenCreditToZoneRate( SenCreditToZoneRate ),
			SenCreditToZoneEnergy( SenCreditToZoneEnergy ),
			ReportH20RemovedKgPerS_FromZoneRate( ReportH20RemovedKgPerS_FromZoneRate ),
			ReportLatCreditToZoneRate( ReportLatCreditToZoneRate ),
			ReportLatCreditToZoneEnergy( ReportLatCreditToZoneEnergy ),
			ReportHeatingToZoneRate( ReportHeatingToZoneRate ),
			ReportHeatingToZoneEnergy( ReportHeatingToZoneEnergy ),
			ReportSenCoolingToZoneRate( ReportSenCoolingToZoneRate ),
			ReportSenCoolingToZoneEnergy( ReportSenCoolingToZoneEnergy ),
			ReportTotCoolingToZoneRate( ReportTotCoolingToZoneRate ),
			ReportTotCoolingToZoneEnergy( ReportTotCoolingToZoneEnergy )
		{}

		// Reset to Zeros
		void
		reset()
		{
			LatCreditToZoneRate = 0.0;
			LatCreditToZoneEnergy = 0.0;
			LatKgPerS_ToZoneRate = 0.0;
			SenCreditToZoneRate = 0.0;
			SenCreditToZoneEnergy = 0.0;
			ReportH20RemovedKgPerS_FromZoneRate = 0.0;
			ReportLatCreditToZoneRate = 0.0;
			ReportLatCreditToZoneEnergy = 0.0;
			ReportHeatingToZoneRate = 0.0;
			ReportHeatingToZoneEnergy = 0.0;
			ReportSenCoolingToZoneRate = 0.0;
			ReportSenCoolingToZoneEnergy = 0.0;
			ReportTotCoolingToZoneRate = 0.0;
			ReportTotCoolingToZoneEnergy = 0.0;
		}

	};

	// Object Data
	extern Array1D< RefrigCaseData > RefrigCase;
	extern Array1D< RefrigRackData > RefrigRack;
	extern Array1D< CaseRAFractionData > CaseRAFraction;
	extern Array1D< RefrigSystemData > System;
	extern Array1D< TransRefrigSystemData > TransSystem;
	extern Array1D< RefrigCondenserData > Condenser;
	extern Array1D< RefrigCompressorData > Compressor;
	extern Array1D< RefrigGasCoolerData > GasCooler;
	extern Array1D< SubcoolerData > Subcooler;
	extern Array1D< CaseAndWalkInListDef > CaseAndWalkInList;
	extern Array1D< CompressorListDef > CompressorLists;
	extern Array1D< SecondaryLoopData > Secondary;
	extern Array1D< TransferLoadListDef > TransferLoadList;
	extern Array1D< WalkInData > WalkIn;
	extern Array1D< WarehouseCoilData > WarehouseCoil;
	extern Array1D< AirChillerSetData > AirChillerSet;
	extern Array1D< CoilCreditData > CoilSysCredit;
	extern Array1D< CaseWIZoneReportData > CaseWIZoneReport;

	// Functions

	void
	ManageRefrigeratedCaseRacks();

	//***************************************************************************************************

	void
	GetRefrigerationInput();

	//***************************************************************************************************
	//***************************************************************************************************

	void
	SetupReportInput();

	//***************************************************************************************************
	//***************************************************************************************************

	void
	InitRefrigeration();

	void
	InitRefrigerationPlantConnections();

	//***************************************************************************************************
	//***************************************************************************************************

	void
	CalcRackSystem( int const RackNum );

	//***************************************************************************************************

	void
	ReportRackSystem( int const RackNum );

	//***************************************************************************************************

	void
	CalculateCase( int const CaseID ); // Absolute pointer to refrigerated case

	//***************************************************************************************************
	//***************************************************************************************************

	void
	SimRefrigCondenser(
		int const SysType,
		std::string const & CompName,
		int & CompIndex,
		bool const FirstHVACIteration,
		bool const InitLoopEquip
	);

	//***************************************************************************************************
	//***************************************************************************************************

	void
	UpdateRefrigCondenser(
		int const Num,
		int const SysType
	);

	//***************************************************************************************************
	//***************************************************************************************************

	void
	SimulateDetailedRefrigerationSystems();

	//***************************************************************************************************
	//***************************************************************************************************

	void
	SimulateDetailedTransRefrigSystems();

	//***************************************************************************************************
	//***************************************************************************************************

	void
	CalcDetailedSystem( int const SysNum );

	//***************************************************************************************************
	//***************************************************************************************************

	void
	CalcDetailedTransSystem( int const SysNum );

	//***************************************************************************************************
	//***************************************************************************************************

	void
	CalculateCondensers( int const SysNum );

	//***************************************************************************************************
	//***************************************************************************************************

	void
	CalcGasCooler( int const SysNum );

	//***************************************************************************************************
	//***************************************************************************************************

	void
	CalculateCompressors( int const SysNum );

	//***************************************************************************************************
	//***************************************************************************************************

	void
	CalculateTransCompressors( int const SysNum );

	//***************************************************************************************************
	//***************************************************************************************************

	void
	CalculateSubcoolers( int const SysNum );

	//***************************************************************************************************

	//***************************************************************************************************

	void
	GetRefrigeratedRackIndex(
		std::string const & Name,
		int & IndexPtr,
		int const SysType,
		bool & ErrorsFound,
		Optional_string_const ThisObjectType = _,
		Optional_bool_const SuppressWarning = _
	);

	void
	ReportRefrigerationComponents();

	//***************************************************************************************************
	//***************************************************************************************************

	void
	CalculateWalkIn( int const WalkInID ); // Absolute pointer to  Walk In

	//***************************************************************************************************
	//***************************************************************************************************

	void
	CalculateSecondary( int const SecondaryNum );

	void
	SumZoneImpacts();

	void
	CheckRefrigerationInput();

	//***************************************************************************************************
	//***************************************************************************************************

	void
	SimAirChillerSet(
		std::string const & AirChillerSetName,
		int const ZoneNum,
		bool const FirstHVACIteration,
		Real64 & SysOutputProvided,
		Real64 & LatOutputProvided,
		int & AirChillerSetPtr // from ZoneEquipList(CurZoneEqNum)%EquipIndex(EquipPtr)
	);

	//***************************************************************************************************

	void
	CalculateAirChillerSets( int const AirChillerSetID );

	//***************************************************************************************************

	void
	FinalRateCoils(
		bool const DeRate, // True if compressor rack or secondary ht exchanger unable to provide capacity
		int const SystemSourceType, // Secondarysystem or DetailedSystem
		int const SystemID, // ID for Secondary loop or detailed system calling for derate
		Real64 const InitialTotalLoad, // Load on system or secondary loop as initially calculated [W]
		Real64 const AvailableTotalLoad // Load that system or secondary loop is able to serve [W]
	);

	//***************************************************************************************************

	//***************************************************************************************************

	void
	CalculateCoil(
		int const CoilID,
		Real64 const QZnReq // sensible load required
	);

	//***************************************************************************************************

	void
	FigureRefrigerationZoneGains();

	//***************************************************************************************************

	void
	ZeroHVACValues();

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

} // RefrigeratedCase

} // EnergyPlus

#endif
