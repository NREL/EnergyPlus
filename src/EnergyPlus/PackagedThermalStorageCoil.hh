#ifndef PackagedThermalStorageCoil_hh_INCLUDED
#define PackagedThermalStorageCoil_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <DataHVACGlobals.hh>

namespace EnergyPlus {

namespace PackagedThermalStorageCoil {

	// Using/Aliasing
	using namespace DataHVACGlobals;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// control types
	extern int const ScheduledOpModes; // control over TES modes is via local schedule
	extern int const EMSActuatedOpModes; // control over TES modes is via EMS

	// Control Modes
	extern int const OffMode;
	extern int const CoolingOnlyMode;
	extern int const CoolingAndChargeMode;
	extern int const CoolingAndDischargeMode;
	extern int const ChargeOnlyMode;
	extern int const DischargeOnlyMode;

	// storage media
	extern int const FluidBased;
	extern int const IceBased;
	//INTEGER, PARAMETER :: UserDefinedFluid = 103

	//Water Systems
	extern int const CondensateDiscarded; // default mode where water is "lost"
	extern int const CondensateToTank; // collect coil condensate from air and store in water storage tank

	extern int const WaterSupplyFromMains;
	extern int const WaterSupplyFromTank;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:

	extern int NumTESCoils;
	extern Array1D_bool CheckEquipName;
	extern bool GetTESInputFlag;
	// SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>:

	// Types

	struct PackagedTESCoolingCoilStruct
	{
		// Members
		std::string Name; // Name of TES cooling package
		int AvailSchedNum; // pointer to availability schedule
		int ModeControlType; // how are operation modes controlled
		int ControlModeSchedNum; // pointer to control schedule if used
		bool EMSControlModeOn; // if true, then EMS actuator has been used
		Real64 EMSControlModeValue; // value to use from EMS actuator for control mode
		int CurControlMode;
		int ControlModeErrorIndex;
		Real64 RatedEvapAirVolFlowRate; // [m3/s]
		Real64 RatedEvapAirMassFlowRate; // [kg/s]
		int EvapAirInletNodeNum; // evaporator inlet node pointer
		int EvapAirOutletNodeNum; // evaporator outlet node pointer
		// Cooling Only Mode
		bool CoolingOnlyModeIsAvailable;
		Real64 CoolingOnlyRatedTotCap; // gross total cooling capacity at rating conditions [W]
		Real64 CoolingOnlyRatedSHR; // Sensible heat ratio (sens cap/total cap) at rating conditions [W/W]
		Real64 CoolingOnlyRatedCOP; // Coefficient of performance at rating conditions [W/W]
		int CoolingOnlyCapFTempCurve; // curve index for total cooling capacity modifier curve
		// (function of entering wetbulb, outside drybulb)
		int CoolingOnlyCapFTempObjectNum; // type of object used for curve input
		int CoolingOnlyCapFFlowCurve; // curve index for total cooling capacity modifier curve
		// (function of actual supply air flow vs rated air flow)
		int CoolingOnlyCapFFlowObjectNum; // type of object used for curve input
		int CoolingOnlyEIRFTempCurve; // curve index for energy input ratio modifier curve
		// (function of entering wetbulb, outside drybulb)
		int CoolingOnlyEIRFTempObjectNum; // type of object used for curve input
		int CoolingOnlyEIRFFlowCurve; // curve index for energy input ratio modifier curve
		// (function of actual supply air flow vs rated air flow)
		int CoolingOnlyEIRFFlowObjectNum; // type of object used for curve input
		int CoolingOnlyPLFFPLRCurve; // curve index for part-load fact vs part load ratio,EIR modifier
		int CoolingOnlyPLFFPLRObjectNum; // type of object used for curve input
		int CoolingOnlySHRFTempCurve; // curve index for sensible heat ratio modifier curve
		// (function of entering wetbulb and drybulb)
		int CoolingOnlySHRFTempObjectNum; // type of object used for curve input
		int CoolingOnlySHRFFlowCurve; // curve index for sensible heat ratio modifer curve
		// (function of actual supply air flow vs rated air flow)
		int CoolingOnlySHRFFlowObjectNum;
		// cooling and charge mode
		bool CoolingAndChargeModeAvailable;
		Real64 CoolingAndChargeRatedTotCap; // gross total evaporator cooling capacity at rating conditions [W]
		Real64 CoolingAndChargeRatedTotCapSizingFactor; // sizing factor for gross total evaporator [ ]
		Real64 CoolingAndChargeRatedChargeCap; // net storage charging capacity at rating conditions [W]
		Real64 CoolingAndChargeRatedChargeCapSizingFactor; // sizing factor for charging capacity [ ]
		Real64 CoolingAndChargeRatedSHR; // Sensible heat ratio (sens cap/total cap) at rating conditions [W/W]
		Real64 CoolingAndChargeCoolingRatedCOP; // Coefficient of performance at rating conditions, for cooling [W/W]
		Real64 CoolingAndChargeChargingRatedCOP; // Coefficient of performance at rating conditions, for charging [W/W]
		int CoolingAndChargeCoolingCapFTempCurve; // curve index for total cooling capacity modifier curve
		// (function of entering wetbulb, outside drybulb, state of TES)
		int CoolingAndChargeCoolingCapFTempObjectNum;
		int CoolingAndChargeCoolingCapFFlowCurve; // curve index for total cooling capacity modifier curve
		// (function of actual supply air flow vs rated air flow)
		int CoolingAndChargeCoolingCapFFlowObjectNum;
		int CoolingAndChargeCoolingEIRFTempCurve; // curve index for cooling energy input ratio modifier curve
		// (function of entering wetbulb, outside drybulb, state of TES)
		int CoolingAndChargeCoolingEIRFTempObjectNum;
		int CoolingAndChargeCoolingEIRFFlowCurve; // curve index for cooling energy input ratio modifier curve
		// (function of actual supply air flow vs rated air flow)
		int CoolingAndChargeCoolingEIRFFlowObjectNum;
		int CoolingAndChargeCoolingPLFFPLRCurve; // curve index for cooling part-load fact vs part load ratio, EIR modifier
		// (function of evaporator part load)
		int CoolingAndChargeCoolingPLFFPLRObjectNum;
		int CoolingAndChargeChargingCapFTempCurve; // curve index for charging capacity modifier curve
		// (function of entering wetbulb, outside drybulb, state of TES)
		int CoolingAndChargeChargingCapFTempObjectNum;
		int CoolingAndChargeChargingCapFEvapPLRCurve; // curve index for charging capacity modifier curve
		// function of evaporator part load ratio
		int CoolingAndChargeChargingCapFEvapPLRObjectNum;
		int CoolingAndChargeChargingEIRFTempCurve; // curve index for charging energy input ratio modifier curve
		// (function of entering wetbulb, outside drybulb, state of TES)
		int CoolingAndChargeChargingEIRFTempObjectNum;
		int CoolingAndChargeChargingEIRFFLowCurve; // curve index for charging energy input ratio modifier curve
		// (function of actual supply air flow vs rated air flow)
		int CoolingAndChargeChargingEIRFFLowObjectNum;
		int CoolingAndChargeChargingPLFFPLRCurve; // curve index for chargine part-load fact vs part load ratio, EIR modif
		// (function of evaporator part load)
		int CoolingAndChargeChargingPLFFPLRObjectNum;
		int CoolingAndChargeSHRFTempCurve; // curve index for sensible heat ratio modifier curve
		// (function of entering wetbulb and drybulb)
		int CoolingAndChargeSHRFTempObjectNum;
		int CoolingAndChargeSHRFFlowCurve; // curve index for sensible heat ratio modifer curve
		// (function of actual supply air flow vs rated air flow)
		int CoolingAndChargeSHRFFlowObjectNum;
		//cooling and discharge mode
		bool CoolingAndDischargeModeAvailable;
		Real64 CoolingAndDischargeRatedTotCap; // gross total evaporator cooling capacity at rating conditions [W]
		Real64 CoolingAndDischargeRatedTotCapSizingFactor; // sizing factor gross total cooling capacity []
		Real64 CoolingAndDischargeRatedDischargeCap; // net storage discharging capacity at rating conditions [W]
		Real64 CoolingAndDischargeRatedDischargeCapSizingFactor; // sizing factor discharging capacity []
		Real64 CoolingAndDischargeRatedSHR; // Sensible heat ratio (sens cap/total cap) at rating conditions [W/W]
		Real64 CoolingAndDischargeCoolingRatedCOP; // Coefficient of performance at rating conditions, for cooling [W/W]
		Real64 CoolingAndDischargeDischargingRatedCOP; // Coefficient of performance at rating conditions, for charging [W/W]
		int CoolingAndDischargeCoolingCapFTempCurve; // curve index for total cooling capacity modifier curve
		// (function of entering wetbulb, outside drybulb, state of TES)
		int CoolingAndDischargeCoolingCapFTempObjectNum;
		int CoolingAndDischargeCoolingCapFFlowCurve; // curve index for total cooling capacity modifier curve
		// (function of actual supply air flow vs rated air flow)
		int CoolingAndDischargeCoolingCapFFlowObjectNum;
		int CoolingAndDischargeCoolingEIRFTempCurve; // curve index for cooling energy input ratio modifier curve
		// (function of entering wetbulb, outside drybulb, state of TES)
		int CoolingAndDischargeCoolingEIRFTempObjectNum;
		int CoolingAndDischargeCoolingEIRFFlowCurve; // curve index for cooling energy input ratio modifier curve
		// (function of actual supply air flow vs rated air flow)
		int CoolingAndDischargeCoolingEIRFFlowObjectNum;
		int CoolingAndDischargeCoolingPLFFPLRCurve; // curve index for cooling part-load fact vs part load ratio,
		// EIR modifier (function of evaporator part load)
		int CoolingAndDischargeCoolingPLFFPLRObjectNum;
		int CoolingAndDischargeDischargingCapFTempCurve; // curve index for discharging capacity modifier curve
		// (function of entering wetbulb, outside drybulb, state of TES)
		int CoolingAndDischargeDischargingCapFTempObjectNum;
		int CoolingAndDischargeDischargingCapFFlowCurve; // curve index for discharging capacity modifier curve
		// (function of actual supply air flow vs rated air flow)
		int CoolingAndDischargeDischargingCapFFlowObjectNum;
		int CoolingAndDischargeDischargingCapFEvapPLRCurve; // curve index for discharging capacity modifier curve
		// function of evaporator part load ratio
		int CoolingAndDischargeDischargingCapFEvapPLRObjectNum;
		int CoolingAndDischargeDischargingEIRFTempCurve; // curve index for discharging energy input ratio modifier curve
		// (function of entering wetbulb, outside drybulb, state of TES)
		int CoolingAndDischargeDischargingEIRFTempObjectNum;
		int CoolingAndDischargeDischargingEIRFFLowCurve; // curve index for discharging energy input ratio modifier curve
		// (function of actual supply air flow vs rated air flow)
		int CoolingAndDischargeDischargingEIRFFLowObjectNum;
		int CoolingAndDischargeDischargingPLFFPLRCurve; // curve index for discharging part-load fact vs part load ratio
		//  EIR modifier (function of evaporator part load)
		int CoolingAndDischargeDischargingPLFFPLRObjectNum;
		int CoolingAndDischargeSHRFTempCurve; // curve index for sensible heat ratio modifier curve
		// (function of entering wetbulb and drybulb)
		int CoolingAndDischargeSHRFTempObjectNum;
		int CoolingAndDischargeSHRFFlowCurve; // curve index for sensible heat ratio modifer curve
		// (function of actual supply air flow vs rated air flow)
		int CoolingAndDischargeSHRFFlowObjectNum;
		// Charge Only Mode
		bool ChargeOnlyModeAvailable;
		Real64 ChargeOnlyRatedCapacity; // net storage charging capacity at rating conditions [W]
		Real64 ChargeOnlyRatedCapacitySizingFactor; // sizing factor for charging capacity []
		Real64 ChargeOnlyRatedCOP; // coefficient of performance at rating conditions [W/W]
		int ChargeOnlyChargingCapFTempCurve; // curve index for charging capacity modifier curve
		// function of outside drybulb and state of TES
		int ChargeOnlyChargingCapFTempObjectNum;
		int ChargeOnlyChargingEIRFTempCurve; // curve index for charging energy input ratio modifier curve
		// function of outside drybulb and state of TES
		int ChargeOnlyChargingEIRFTempObjectNum;
		// Discharge Only mode
		bool DischargeOnlyModeAvailable;
		Real64 DischargeOnlyRatedDischargeCap; // gross total evaporator cooling capacity at rating conditions [W]
		Real64 DischargeOnlyRatedDischargeCapSizingFactor; // sizing factor for cooling capacity []
		Real64 DischargeOnlyRatedSHR; // sensible heat ratio (sens cap/total cap) at rating conditions
		Real64 DischargeOnlyRatedCOP; // coefficient of performance at rating conditions for discharging [W/W]
		int DischargeOnlyCapFTempCurve; // curve index for total cooling capacity modifier curve
		// function of entering wetbulb and state of TES
		int DischargeOnlyCapFTempObjectNum;
		int DischargeOnlyCapFFlowCurve; // curve index for tot cooling capacity modifier curve
		// (function of actual supply air flow vs rated air flow)
		int DischargeOnlyCapFFlowObjectNum;
		int DischargeOnlyEIRFTempCurve; // curve index for energy input ratio modifier curve
		// function of entering wetbulb and state of TES
		int DischargeOnlyEIRFTempObjectNum;
		int DischargeOnlyEIRFFlowCurve; // curve index for energy input ratio modifier curve
		// (function of actual supply air flow vs rated air flow)
		int DischargeOnlyEIRFFlowObjectNum;
		int DischargeOnlyPLFFPLRCurve; // curve index for part-load fact vs evaporator part load ratio
		int DischargeOnlyPLFFPLRObjectNum;
		int DischargeOnlySHRFTempCurve; // curve index for sensible heat ratio modifier curve
		// (function of entering wetbulb and drybulb)
		int DischargeOnlySHRFTempObjectNum;
		int DischargeOnlySHRFFLowCurve; // curve index for
		int DischargeOnlySHRFFLowObjectNum;
		// other inputs
		Real64 AncillaryControlsPower; // standby and controls electric power, draws when available [W]
		Real64 ColdWeatherMinimumTempLimit; // temperature limit for cold weather operation mode [C]
		Real64 ColdWeatherAncillaryPower; // electrical power draw during cold weather [W]
		int CondAirInletNodeNum; // Condenser air inlet node num pointer
		int CondAirOutletNodeNum; // condenser air outlet node num pointer
		int CondenserType; // Type of condenser for DX cooling coil: AIR COOLED or EVAP COOLED
		Real64 CondenserAirVolumeFlow; // design air flow rate thru condenser [m3/s]
		Real64 CondenserAirFlowSizingFactor; // scale condenser air flow relative to evap air flow when autosizing
		Real64 CondenserAirMassFlow; // design air flow rate thru condenser [kg/s]
		Real64 EvapCondEffect; // effectiveness of the evaporatively cooled condenser
		Real64 CondInletTemp; // air temperature drybulb entering condenser section after evap cooling [C]
		Real64 EvapCondPumpElecNomPower; // Nominal power input to the evap condenser water circulation pump [W]
		Real64 EvapCondPumpElecEnergy; // Electric energy used by condenser water circulation pump [J]
		Real64 BasinHeaterPowerFTempDiff; // Basin heater power for evaporatively cooled condensers [W/K]
		int BasinHeaterAvailSchedNum; // basin heater availability schedule pointer num
		Real64 BasinHeaterSetpointTemp; // evap water basin temperature setpoint [C]
		int EvapWaterSupplyMode; // where does evap water come from
		std::string EvapWaterSupplyName; // name of water source e.g. water storage tank
		int EvapWaterSupTankID; // supply tank index, if any
		int EvapWaterTankDemandARRID; // evap water demand array index
		int CondensateCollectMode; // where does condensate  water go to
		std::string CondensateCollectName; // name of water source e.g. water storage tank
		int CondensateTankID;
		int CondensateTankSupplyARRID;
		// TES tank
		int StorageMedia; // water/fluid or ice based TES
		std::string StorageFluidName; // if user defined, name of fluid type
		int StorageFluidIndex; // if user defined, index of fluid type
		Real64 FluidStorageVolume; // volume of water in storage tank for water systems [m3/s]
		Real64 IceStorageCapacity; // capacity of storage in J
		Real64 StorageCapacitySizingFactor; // storage time used to autocalculate capacity [hr]
		Real64 MinimumFluidTankTempLimit; // optional inputs [C]
		Real64 MaximumFluidTankTempLimit; // optional inputs [C]
		Real64 RatedFluidTankTemp; // rating point condition for fluid storage tanks [C]
		int StorageAmbientNodeNum; // node "pointer" for ambient conditions exposed to TES
		Real64 StorageUA; // overall heat transfer coefficient for TES to ambient [W/k]
		bool TESPlantConnectionAvailable;
		int TESPlantInletNodeNum; // plant loop inlet node index
		int TESPlantOutletNodeNum; // plant loop outlet node index
		int TESPlantLoopNum; // plant loop connection index
		int TESPlantLoopSideNum; // plant loop side connection index
		int TESPlantBranchNum; // plant loop branch connection index
		int TESPlantCompNum; // plant loop component connection index
		Real64 TESPlantDesignVolumeFlowRate; // plant connection design mass flow rate [m3/s]
		Real64 TESPlantDesignMassFlowRate; // [kg/s]
		Real64 TESPlantEffectiveness;
		Real64 TimeElapsed;
		Real64 IceFracRemain; // state of storage for current time step [0..1.0]
		Real64 IceFracRemainLastTimestep; // state of storage for previous time step [0..1.0]
		Real64 FluidTankTempFinal;
		Real64 FluidTankTempFinalLastTimestep;
		// dynamic calculated data
		Real64 QdotPlant; // heat exchange rate for plant connection to TES tank [W]
		Real64 Q_Plant; // heat exchange energy for plant connection to TES tank [J]
		Real64 QdotAmbient; // heat exchange rate for skin losses/gains for TES tank to surroundings [W]
		Real64 Q_Ambient; // heat exchange enegy for skin losses/gains for TES tank to surroundings [J]
		Real64 QdotTES; // heat exchange rate by mechanical systems to charge or discharge TES [W]
		Real64 Q_TES; // heat exchange energy by mechanical systems to charge or discharge TES [J]
		Real64 ElecCoolingPower; // electric power for cooling [W]
		Real64 ElecCoolingEnergy; // electric energy for cooling [J], metered
		Real64 EvapTotCoolingRate; // evaporator coil total cooling rate [W]
		Real64 EvapTotCoolingEnergy; // evaporatory coil total cooling energy [J], metered
		Real64 EvapSensCoolingRate;
		Real64 EvapSensCoolingEnergy;
		Real64 EvapLatCoolingRate;
		Real64 EvapLatCoolingEnergy;
		Real64 RuntimeFraction;
		Real64 CondenserRuntimeFraction;
		Real64 ElectColdWeatherPower; // electric power for cold weather protection [W]
		Real64 ElectColdWeatherEnergy; // electric energy for cold weather protection [J], metered
		Real64 ElectEvapCondBasinHeaterPower;
		Real64 ElectEvapCondBasinHeaterEnergy;
		Real64 EvapWaterConsumpRate; // Evap Water Consumption rate in m3/sec
		Real64 EvapWaterConsump; // Evap Water Consumption in m3
		Real64 EvapWaterStarvMakupRate; // Evap water consumed but not really available from tank m3/s
		Real64 EvapWaterStarvMakup; // Evap water consumed but not really available from tank m3
		Real64 EvapCondPumpElecPower;
		Real64 EvapCondPumpElecConsumption;

		// Default Constructor
		PackagedTESCoolingCoilStruct() :
			AvailSchedNum( 0 ),
			ModeControlType( 0 ),
			ControlModeSchedNum( 0 ),
			EMSControlModeOn( false ),
			EMSControlModeValue( 0.0 ),
			CurControlMode( OffMode ),
			ControlModeErrorIndex( 0 ),
			RatedEvapAirVolFlowRate( 0.0 ),
			RatedEvapAirMassFlowRate( 0.0 ),
			EvapAirInletNodeNum( 0 ),
			EvapAirOutletNodeNum( 0 ),
			CoolingOnlyModeIsAvailable( false ),
			CoolingOnlyRatedTotCap( 0.0 ),
			CoolingOnlyRatedSHR( 0.0 ),
			CoolingOnlyRatedCOP( 0.0 ),
			CoolingOnlyCapFTempCurve( 0 ),
			CoolingOnlyCapFTempObjectNum( 0 ),
			CoolingOnlyCapFFlowCurve( 0 ),
			CoolingOnlyCapFFlowObjectNum( 0 ),
			CoolingOnlyEIRFTempCurve( 0 ),
			CoolingOnlyEIRFTempObjectNum( 0 ),
			CoolingOnlyEIRFFlowCurve( 0 ),
			CoolingOnlyEIRFFlowObjectNum( 0 ),
			CoolingOnlyPLFFPLRCurve( 0 ),
			CoolingOnlyPLFFPLRObjectNum( 0 ),
			CoolingOnlySHRFTempCurve( 0 ),
			CoolingOnlySHRFTempObjectNum( 0 ),
			CoolingOnlySHRFFlowCurve( 0 ),
			CoolingOnlySHRFFlowObjectNum( 0 ),
			CoolingAndChargeModeAvailable( false ),
			CoolingAndChargeRatedTotCap( 0.0 ),
			CoolingAndChargeRatedTotCapSizingFactor( 0.0 ),
			CoolingAndChargeRatedChargeCap( 0.0 ),
			CoolingAndChargeRatedChargeCapSizingFactor( 0.0 ),
			CoolingAndChargeRatedSHR( 0.0 ),
			CoolingAndChargeCoolingRatedCOP( 0.0 ),
			CoolingAndChargeChargingRatedCOP( 0.0 ),
			CoolingAndChargeCoolingCapFTempCurve( 0 ),
			CoolingAndChargeCoolingCapFTempObjectNum( 0 ),
			CoolingAndChargeCoolingCapFFlowCurve( 0 ),
			CoolingAndChargeCoolingCapFFlowObjectNum( 0 ),
			CoolingAndChargeCoolingEIRFTempCurve( 0 ),
			CoolingAndChargeCoolingEIRFTempObjectNum( 0 ),
			CoolingAndChargeCoolingEIRFFlowCurve( 0 ),
			CoolingAndChargeCoolingEIRFFlowObjectNum( 0 ),
			CoolingAndChargeCoolingPLFFPLRCurve( 0 ),
			CoolingAndChargeCoolingPLFFPLRObjectNum( 0 ),
			CoolingAndChargeChargingCapFTempCurve( 0 ),
			CoolingAndChargeChargingCapFTempObjectNum( 0 ),
			CoolingAndChargeChargingCapFEvapPLRCurve( 0 ),
			CoolingAndChargeChargingCapFEvapPLRObjectNum( 0 ),
			CoolingAndChargeChargingEIRFTempCurve( 0 ),
			CoolingAndChargeChargingEIRFTempObjectNum( 0 ),
			CoolingAndChargeChargingEIRFFLowCurve( 0 ),
			CoolingAndChargeChargingEIRFFLowObjectNum( 0 ),
			CoolingAndChargeChargingPLFFPLRCurve( 0 ),
			CoolingAndChargeChargingPLFFPLRObjectNum( 0 ),
			CoolingAndChargeSHRFTempCurve( 0 ),
			CoolingAndChargeSHRFTempObjectNum( 0 ),
			CoolingAndChargeSHRFFlowCurve( 0 ),
			CoolingAndChargeSHRFFlowObjectNum( 0 ),
			CoolingAndDischargeModeAvailable( false ),
			CoolingAndDischargeRatedTotCap( 0.0 ),
			CoolingAndDischargeRatedTotCapSizingFactor( 0.0 ),
			CoolingAndDischargeRatedDischargeCap( 0.0 ),
			CoolingAndDischargeRatedDischargeCapSizingFactor( 0.0 ),
			CoolingAndDischargeRatedSHR( 0.0 ),
			CoolingAndDischargeCoolingRatedCOP( 0.0 ),
			CoolingAndDischargeDischargingRatedCOP( 0.0 ),
			CoolingAndDischargeCoolingCapFTempCurve( 0 ),
			CoolingAndDischargeCoolingCapFTempObjectNum( 0 ),
			CoolingAndDischargeCoolingCapFFlowCurve( 0 ),
			CoolingAndDischargeCoolingCapFFlowObjectNum( 0 ),
			CoolingAndDischargeCoolingEIRFTempCurve( 0 ),
			CoolingAndDischargeCoolingEIRFTempObjectNum( 0 ),
			CoolingAndDischargeCoolingEIRFFlowCurve( 0 ),
			CoolingAndDischargeCoolingEIRFFlowObjectNum( 0 ),
			CoolingAndDischargeCoolingPLFFPLRCurve( 0 ),
			CoolingAndDischargeCoolingPLFFPLRObjectNum( 0 ),
			CoolingAndDischargeDischargingCapFTempCurve( 0 ),
			CoolingAndDischargeDischargingCapFTempObjectNum( 0 ),
			CoolingAndDischargeDischargingCapFFlowCurve( 0 ),
			CoolingAndDischargeDischargingCapFFlowObjectNum( 0 ),
			CoolingAndDischargeDischargingCapFEvapPLRCurve( 0 ),
			CoolingAndDischargeDischargingCapFEvapPLRObjectNum( 0 ),
			CoolingAndDischargeDischargingEIRFTempCurve( 0 ),
			CoolingAndDischargeDischargingEIRFTempObjectNum( 0 ),
			CoolingAndDischargeDischargingEIRFFLowCurve( 0 ),
			CoolingAndDischargeDischargingEIRFFLowObjectNum( 0 ),
			CoolingAndDischargeDischargingPLFFPLRCurve( 0 ),
			CoolingAndDischargeDischargingPLFFPLRObjectNum( 0 ),
			CoolingAndDischargeSHRFTempCurve( 0 ),
			CoolingAndDischargeSHRFTempObjectNum( 0 ),
			CoolingAndDischargeSHRFFlowCurve( 0 ),
			CoolingAndDischargeSHRFFlowObjectNum( 0 ),
			ChargeOnlyModeAvailable( false ),
			ChargeOnlyRatedCapacity( 0.0 ),
			ChargeOnlyRatedCapacitySizingFactor( 0.0 ),
			ChargeOnlyRatedCOP( 0.0 ),
			ChargeOnlyChargingCapFTempCurve( 0 ),
			ChargeOnlyChargingCapFTempObjectNum( 0 ),
			ChargeOnlyChargingEIRFTempCurve( 0 ),
			ChargeOnlyChargingEIRFTempObjectNum( 0 ),
			DischargeOnlyModeAvailable( false ),
			DischargeOnlyRatedDischargeCap( 0.0 ),
			DischargeOnlyRatedDischargeCapSizingFactor( 0.0 ),
			DischargeOnlyRatedSHR( 0.0 ),
			DischargeOnlyRatedCOP( 0.0 ),
			DischargeOnlyCapFTempCurve( 0 ),
			DischargeOnlyCapFTempObjectNum( 0 ),
			DischargeOnlyCapFFlowCurve( 0 ),
			DischargeOnlyCapFFlowObjectNum( 0 ),
			DischargeOnlyEIRFTempCurve( 0 ),
			DischargeOnlyEIRFTempObjectNum( 0 ),
			DischargeOnlyEIRFFlowCurve( 0 ),
			DischargeOnlyEIRFFlowObjectNum( 0 ),
			DischargeOnlyPLFFPLRCurve( 0 ),
			DischargeOnlyPLFFPLRObjectNum( 0 ),
			DischargeOnlySHRFTempCurve( 0 ),
			DischargeOnlySHRFTempObjectNum( 0 ),
			DischargeOnlySHRFFLowCurve( 0 ),
			DischargeOnlySHRFFLowObjectNum( 0 ),
			AncillaryControlsPower( 0.0 ),
			ColdWeatherMinimumTempLimit( 0.0 ),
			ColdWeatherAncillaryPower( 0.0 ),
			CondAirInletNodeNum( 0 ),
			CondAirOutletNodeNum( 0 ),
			CondenserType( AirCooled ),
			CondenserAirVolumeFlow( 0.0 ),
			CondenserAirFlowSizingFactor( 0.0 ),
			CondenserAirMassFlow( 0.0 ),
			EvapCondEffect( 0.0 ),
			CondInletTemp( 0.0 ),
			EvapCondPumpElecNomPower( 0.0 ),
			EvapCondPumpElecEnergy( 0.0 ),
			BasinHeaterPowerFTempDiff( 0.0 ),
			BasinHeaterAvailSchedNum( 0 ),
			BasinHeaterSetpointTemp( 0.0 ),
			EvapWaterSupplyMode( WaterSupplyFromMains ),
			EvapWaterSupTankID( 0 ),
			EvapWaterTankDemandARRID( 0 ),
			CondensateCollectMode( CondensateDiscarded ),
			CondensateTankID( 0 ),
			CondensateTankSupplyARRID( 0 ),
			StorageMedia( 0 ),
			StorageFluidIndex( 0 ),
			FluidStorageVolume( 0.0 ),
			IceStorageCapacity( 0.0 ),
			StorageCapacitySizingFactor( 0.0 ),
			MinimumFluidTankTempLimit( 0.0 ),
			MaximumFluidTankTempLimit( 100.0 ),
			RatedFluidTankTemp( 0.0 ),
			StorageAmbientNodeNum( 0 ),
			StorageUA( 0.0 ),
			TESPlantConnectionAvailable( false ),
			TESPlantInletNodeNum( 0 ),
			TESPlantOutletNodeNum( 0 ),
			TESPlantLoopNum( 0 ),
			TESPlantLoopSideNum( 0 ),
			TESPlantBranchNum( 0 ),
			TESPlantCompNum( 0 ),
			TESPlantDesignVolumeFlowRate( 0.0 ),
			TESPlantDesignMassFlowRate( 0.0 ),
			TESPlantEffectiveness( 0.0 ),
			TimeElapsed( 0.0 ),
			IceFracRemain( 0.0 ),
			IceFracRemainLastTimestep( 0.0 ),
			FluidTankTempFinal( 0.0 ),
			FluidTankTempFinalLastTimestep( 0.0 ),
			QdotPlant( 0.0 ),
			Q_Plant( 0.0 ),
			QdotAmbient( 0.0 ),
			Q_Ambient( 0.0 ),
			QdotTES( 0.0 ),
			Q_TES( 0.0 ),
			ElecCoolingPower( 0.0 ),
			ElecCoolingEnergy( 0.0 ),
			EvapTotCoolingRate( 0.0 ),
			EvapTotCoolingEnergy( 0.0 ),
			EvapSensCoolingRate( 0.0 ),
			EvapSensCoolingEnergy( 0.0 ),
			EvapLatCoolingRate( 0.0 ),
			EvapLatCoolingEnergy( 0.0 ),
			RuntimeFraction( 0.0 ),
			CondenserRuntimeFraction( 0.0 ),
			ElectColdWeatherPower( 0.0 ),
			ElectColdWeatherEnergy( 0.0 ),
			ElectEvapCondBasinHeaterPower( 0.0 ),
			ElectEvapCondBasinHeaterEnergy( 0.0 ),
			EvapWaterConsumpRate( 0.0 ),
			EvapWaterConsump( 0.0 ),
			EvapWaterStarvMakupRate( 0.0 ),
			EvapWaterStarvMakup( 0.0 ),
			EvapCondPumpElecPower( 0.0 ),
			EvapCondPumpElecConsumption( 0.0 )
		{}

		// Member Constructor
		PackagedTESCoolingCoilStruct(
			std::string const & Name, // Name of TES cooling package
			int const AvailSchedNum, // pointer to availability schedule
			int const ModeControlType, // how are operation modes controlled
			int const ControlModeSchedNum, // pointer to control schedule if used
			bool const EMSControlModeOn, // if true, then EMS actuator has been used
			Real64 const EMSControlModeValue, // value to use from EMS actuator for control mode
			int const CurControlMode,
			int const ControlModeErrorIndex,
			Real64 const RatedEvapAirVolFlowRate, // [m3/s]
			Real64 const RatedEvapAirMassFlowRate, // [kg/s]
			int const EvapAirInletNodeNum, // evaporator inlet node pointer
			int const EvapAirOutletNodeNum, // evaporator outlet node pointer
			bool const CoolingOnlyModeIsAvailable,
			Real64 const CoolingOnlyRatedTotCap, // gross total cooling capacity at rating conditions [W]
			Real64 const CoolingOnlyRatedSHR, // Sensible heat ratio (sens cap/total cap) at rating conditions [W/W]
			Real64 const CoolingOnlyRatedCOP, // Coefficient of performance at rating conditions [W/W]
			int const CoolingOnlyCapFTempCurve, // curve index for total cooling capacity modifier curve
			int const CoolingOnlyCapFTempObjectNum, // type of object used for curve input
			int const CoolingOnlyCapFFlowCurve, // curve index for total cooling capacity modifier curve
			int const CoolingOnlyCapFFlowObjectNum, // type of object used for curve input
			int const CoolingOnlyEIRFTempCurve, // curve index for energy input ratio modifier curve
			int const CoolingOnlyEIRFTempObjectNum, // type of object used for curve input
			int const CoolingOnlyEIRFFlowCurve, // curve index for energy input ratio modifier curve
			int const CoolingOnlyEIRFFlowObjectNum, // type of object used for curve input
			int const CoolingOnlyPLFFPLRCurve, // curve index for part-load fact vs part load ratio,EIR modifier
			int const CoolingOnlyPLFFPLRObjectNum, // type of object used for curve input
			int const CoolingOnlySHRFTempCurve, // curve index for sensible heat ratio modifier curve
			int const CoolingOnlySHRFTempObjectNum, // type of object used for curve input
			int const CoolingOnlySHRFFlowCurve, // curve index for sensible heat ratio modifer curve
			int const CoolingOnlySHRFFlowObjectNum,
			bool const CoolingAndChargeModeAvailable,
			Real64 const CoolingAndChargeRatedTotCap, // gross total evaporator cooling capacity at rating conditions [W]
			Real64 const CoolingAndChargeRatedTotCapSizingFactor, // sizing factor for gross total evaporator [ ]
			Real64 const CoolingAndChargeRatedChargeCap, // net storage charging capacity at rating conditions [W]
			Real64 const CoolingAndChargeRatedChargeCapSizingFactor, // sizing factor for charging capacity [ ]
			Real64 const CoolingAndChargeRatedSHR, // Sensible heat ratio (sens cap/total cap) at rating conditions [W/W]
			Real64 const CoolingAndChargeCoolingRatedCOP, // Coefficient of performance at rating conditions, for cooling [W/W]
			Real64 const CoolingAndChargeChargingRatedCOP, // Coefficient of performance at rating conditions, for charging [W/W]
			int const CoolingAndChargeCoolingCapFTempCurve, // curve index for total cooling capacity modifier curve
			int const CoolingAndChargeCoolingCapFTempObjectNum,
			int const CoolingAndChargeCoolingCapFFlowCurve, // curve index for total cooling capacity modifier curve
			int const CoolingAndChargeCoolingCapFFlowObjectNum,
			int const CoolingAndChargeCoolingEIRFTempCurve, // curve index for cooling energy input ratio modifier curve
			int const CoolingAndChargeCoolingEIRFTempObjectNum,
			int const CoolingAndChargeCoolingEIRFFlowCurve, // curve index for cooling energy input ratio modifier curve
			int const CoolingAndChargeCoolingEIRFFlowObjectNum,
			int const CoolingAndChargeCoolingPLFFPLRCurve, // curve index for cooling part-load fact vs part load ratio, EIR modifier
			int const CoolingAndChargeCoolingPLFFPLRObjectNum,
			int const CoolingAndChargeChargingCapFTempCurve, // curve index for charging capacity modifier curve
			int const CoolingAndChargeChargingCapFTempObjectNum,
			int const CoolingAndChargeChargingCapFEvapPLRCurve, // curve index for charging capacity modifier curve
			int const CoolingAndChargeChargingCapFEvapPLRObjectNum,
			int const CoolingAndChargeChargingEIRFTempCurve, // curve index for charging energy input ratio modifier curve
			int const CoolingAndChargeChargingEIRFTempObjectNum,
			int const CoolingAndChargeChargingEIRFFLowCurve, // curve index for charging energy input ratio modifier curve
			int const CoolingAndChargeChargingEIRFFLowObjectNum,
			int const CoolingAndChargeChargingPLFFPLRCurve, // curve index for chargine part-load fact vs part load ratio, EIR modif
			int const CoolingAndChargeChargingPLFFPLRObjectNum,
			int const CoolingAndChargeSHRFTempCurve, // curve index for sensible heat ratio modifier curve
			int const CoolingAndChargeSHRFTempObjectNum,
			int const CoolingAndChargeSHRFFlowCurve, // curve index for sensible heat ratio modifer curve
			int const CoolingAndChargeSHRFFlowObjectNum,
			bool const CoolingAndDischargeModeAvailable,
			Real64 const CoolingAndDischargeRatedTotCap, // gross total evaporator cooling capacity at rating conditions [W]
			Real64 const CoolingAndDischargeRatedTotCapSizingFactor, // sizing factor gross total cooling capacity []
			Real64 const CoolingAndDischargeRatedDischargeCap, // net storage discharging capacity at rating conditions [W]
			Real64 const CoolingAndDischargeRatedDischargeCapSizingFactor, // sizing factor discharging capacity []
			Real64 const CoolingAndDischargeRatedSHR, // Sensible heat ratio (sens cap/total cap) at rating conditions [W/W]
			Real64 const CoolingAndDischargeCoolingRatedCOP, // Coefficient of performance at rating conditions, for cooling [W/W]
			Real64 const CoolingAndDischargeDischargingRatedCOP, // Coefficient of performance at rating conditions, for charging [W/W]
			int const CoolingAndDischargeCoolingCapFTempCurve, // curve index for total cooling capacity modifier curve
			int const CoolingAndDischargeCoolingCapFTempObjectNum,
			int const CoolingAndDischargeCoolingCapFFlowCurve, // curve index for total cooling capacity modifier curve
			int const CoolingAndDischargeCoolingCapFFlowObjectNum,
			int const CoolingAndDischargeCoolingEIRFTempCurve, // curve index for cooling energy input ratio modifier curve
			int const CoolingAndDischargeCoolingEIRFTempObjectNum,
			int const CoolingAndDischargeCoolingEIRFFlowCurve, // curve index for cooling energy input ratio modifier curve
			int const CoolingAndDischargeCoolingEIRFFlowObjectNum,
			int const CoolingAndDischargeCoolingPLFFPLRCurve, // curve index for cooling part-load fact vs part load ratio,
			int const CoolingAndDischargeCoolingPLFFPLRObjectNum,
			int const CoolingAndDischargeDischargingCapFTempCurve, // curve index for discharging capacity modifier curve
			int const CoolingAndDischargeDischargingCapFTempObjectNum,
			int const CoolingAndDischargeDischargingCapFFlowCurve, // curve index for discharging capacity modifier curve
			int const CoolingAndDischargeDischargingCapFFlowObjectNum,
			int const CoolingAndDischargeDischargingCapFEvapPLRCurve, // curve index for discharging capacity modifier curve
			int const CoolingAndDischargeDischargingCapFEvapPLRObjectNum,
			int const CoolingAndDischargeDischargingEIRFTempCurve, // curve index for discharging energy input ratio modifier curve
			int const CoolingAndDischargeDischargingEIRFTempObjectNum,
			int const CoolingAndDischargeDischargingEIRFFLowCurve, // curve index for discharging energy input ratio modifier curve
			int const CoolingAndDischargeDischargingEIRFFLowObjectNum,
			int const CoolingAndDischargeDischargingPLFFPLRCurve, // curve index for discharging part-load fact vs part load ratio
			int const CoolingAndDischargeDischargingPLFFPLRObjectNum,
			int const CoolingAndDischargeSHRFTempCurve, // curve index for sensible heat ratio modifier curve
			int const CoolingAndDischargeSHRFTempObjectNum,
			int const CoolingAndDischargeSHRFFlowCurve, // curve index for sensible heat ratio modifer curve
			int const CoolingAndDischargeSHRFFlowObjectNum,
			bool const ChargeOnlyModeAvailable,
			Real64 const ChargeOnlyRatedCapacity, // net storage charging capacity at rating conditions [W]
			Real64 const ChargeOnlyRatedCapacitySizingFactor, // sizing factor for charging capacity []
			Real64 const ChargeOnlyRatedCOP, // coefficient of performance at rating conditions [W/W]
			int const ChargeOnlyChargingCapFTempCurve, // curve index for charging capacity modifier curve
			int const ChargeOnlyChargingCapFTempObjectNum,
			int const ChargeOnlyChargingEIRFTempCurve, // curve index for charging energy input ratio modifier curve
			int const ChargeOnlyChargingEIRFTempObjectNum,
			bool const DischargeOnlyModeAvailable,
			Real64 const DischargeOnlyRatedDischargeCap, // gross total evaporator cooling capacity at rating conditions [W]
			Real64 const DischargeOnlyRatedDischargeCapSizingFactor, // sizing factor for cooling capacity []
			Real64 const DischargeOnlyRatedSHR, // sensible heat ratio (sens cap/total cap) at rating conditions
			Real64 const DischargeOnlyRatedCOP, // coefficient of performance at rating conditions for discharging [W/W]
			int const DischargeOnlyCapFTempCurve, // curve index for total cooling capacity modifier curve
			int const DischargeOnlyCapFTempObjectNum,
			int const DischargeOnlyCapFFlowCurve, // curve index for tot cooling capacity modifier curve
			int const DischargeOnlyCapFFlowObjectNum,
			int const DischargeOnlyEIRFTempCurve, // curve index for energy input ratio modifier curve
			int const DischargeOnlyEIRFTempObjectNum,
			int const DischargeOnlyEIRFFlowCurve, // curve index for energy input ratio modifier curve
			int const DischargeOnlyEIRFFlowObjectNum,
			int const DischargeOnlyPLFFPLRCurve, // curve index for part-load fact vs evaporator part load ratio
			int const DischargeOnlyPLFFPLRObjectNum,
			int const DischargeOnlySHRFTempCurve, // curve index for sensible heat ratio modifier curve
			int const DischargeOnlySHRFTempObjectNum,
			int const DischargeOnlySHRFFLowCurve, // curve index for
			int const DischargeOnlySHRFFLowObjectNum,
			Real64 const AncillaryControlsPower, // standby and controls electric power, draws when available [W]
			Real64 const ColdWeatherMinimumTempLimit, // temperature limit for cold weather operation mode [C]
			Real64 const ColdWeatherAncillaryPower, // electrical power draw during cold weather [W]
			int const CondAirInletNodeNum, // Condenser air inlet node num pointer
			int const CondAirOutletNodeNum, // condenser air outlet node num pointer
			int const CondenserType, // Type of condenser for DX cooling coil: AIR COOLED or EVAP COOLED
			Real64 const CondenserAirVolumeFlow, // design air flow rate thru condenser [m3/s]
			Real64 const CondenserAirFlowSizingFactor, // scale condenser air flow relative to evap air flow when autosizing
			Real64 const CondenserAirMassFlow, // design air flow rate thru condenser [kg/s]
			Real64 const EvapCondEffect, // effectiveness of the evaporatively cooled condenser
			Real64 const CondInletTemp, // air temperature drybulb entering condenser section after evap cooling [C]
			Real64 const EvapCondPumpElecNomPower, // Nominal power input to the evap condenser water circulation pump [W]
			Real64 const EvapCondPumpElecEnergy, // Electric energy used by condenser water circulation pump [J]
			Real64 const BasinHeaterPowerFTempDiff, // Basin heater power for evaporatively cooled condensers [W/K]
			int const BasinHeaterAvailSchedNum, // basin heater availability schedule pointer num
			Real64 const BasinHeaterSetpointTemp, // evap water basin temperature setpoint [C]
			int const EvapWaterSupplyMode, // where does evap water come from
			std::string const & EvapWaterSupplyName, // name of water source e.g. water storage tank
			int const EvapWaterSupTankID, // supply tank index, if any
			int const EvapWaterTankDemandARRID, // evap water demand array index
			int const CondensateCollectMode, // where does condensate  water go to
			std::string const & CondensateCollectName, // name of water source e.g. water storage tank
			int const CondensateTankID,
			int const CondensateTankSupplyARRID,
			int const StorageMedia, // water/fluid or ice based TES
			std::string const & StorageFluidName, // if user defined, name of fluid type
			int const StorageFluidIndex, // if user defined, index of fluid type
			Real64 const FluidStorageVolume, // volume of water in storage tank for water systems [m3/s]
			Real64 const IceStorageCapacity, // capacity of storage in J
			Real64 const StorageCapacitySizingFactor, // storage time used to autocalculate capacity [hr]
			Real64 const MinimumFluidTankTempLimit, // optional inputs [C]
			Real64 const MaximumFluidTankTempLimit, // optional inputs [C]
			Real64 const RatedFluidTankTemp, // rating point condition for fluid storage tanks [C]
			int const StorageAmbientNodeNum, // node "pointer" for ambient conditions exposed to TES
			Real64 const StorageUA, // overall heat transfer coefficient for TES to ambient [W/k]
			bool const TESPlantConnectionAvailable,
			int const TESPlantInletNodeNum, // plant loop inlet node index
			int const TESPlantOutletNodeNum, // plant loop outlet node index
			int const TESPlantLoopNum, // plant loop connection index
			int const TESPlantLoopSideNum, // plant loop side connection index
			int const TESPlantBranchNum, // plant loop branch connection index
			int const TESPlantCompNum, // plant loop component connection index
			Real64 const TESPlantDesignVolumeFlowRate, // plant connection design mass flow rate [m3/s]
			Real64 const TESPlantDesignMassFlowRate, // [kg/s]
			Real64 const TESPlantEffectiveness,
			Real64 const TimeElapsed,
			Real64 const IceFracRemain, // state of storage for current time step [0..1.0]
			Real64 const IceFracRemainLastTimestep, // state of storage for previous time step [0..1.0]
			Real64 const FluidTankTempFinal,
			Real64 const FluidTankTempFinalLastTimestep,
			Real64 const QdotPlant, // heat exchange rate for plant connection to TES tank [W]
			Real64 const Q_Plant, // heat exchange energy for plant connection to TES tank [J]
			Real64 const QdotAmbient, // heat exchange rate for skin losses/gains for TES tank to surroundings [W]
			Real64 const Q_Ambient, // heat exchange enegy for skin losses/gains for TES tank to surroundings [J]
			Real64 const QdotTES, // heat exchange rate by mechanical systems to charge or discharge TES [W]
			Real64 const Q_TES, // heat exchange energy by mechanical systems to charge or discharge TES [J]
			Real64 const ElecCoolingPower, // electric power for cooling [W]
			Real64 const ElecCoolingEnergy, // electric energy for cooling [J], metered
			Real64 const EvapTotCoolingRate, // evaporator coil total cooling rate [W]
			Real64 const EvapTotCoolingEnergy, // evaporatory coil total cooling energy [J], metered
			Real64 const EvapSensCoolingRate,
			Real64 const EvapSensCoolingEnergy,
			Real64 const EvapLatCoolingRate,
			Real64 const EvapLatCoolingEnergy,
			Real64 const RuntimeFraction,
			Real64 const CondenserRuntimeFraction,
			Real64 const ElectColdWeatherPower, // electric power for cold weather protection [W]
			Real64 const ElectColdWeatherEnergy, // electric energy for cold weather protection [J], metered
			Real64 const ElectEvapCondBasinHeaterPower,
			Real64 const ElectEvapCondBasinHeaterEnergy,
			Real64 const EvapWaterConsumpRate, // Evap Water Consumption rate in m3/sec
			Real64 const EvapWaterConsump, // Evap Water Consumption in m3
			Real64 const EvapWaterStarvMakupRate, // Evap water consumed but not really available from tank m3/s
			Real64 const EvapWaterStarvMakup, // Evap water consumed but not really available from tank m3
			Real64 const EvapCondPumpElecPower,
			Real64 const EvapCondPumpElecConsumption
		) :
			Name( Name ),
			AvailSchedNum( AvailSchedNum ),
			ModeControlType( ModeControlType ),
			ControlModeSchedNum( ControlModeSchedNum ),
			EMSControlModeOn( EMSControlModeOn ),
			EMSControlModeValue( EMSControlModeValue ),
			CurControlMode( CurControlMode ),
			ControlModeErrorIndex( ControlModeErrorIndex ),
			RatedEvapAirVolFlowRate( RatedEvapAirVolFlowRate ),
			RatedEvapAirMassFlowRate( RatedEvapAirMassFlowRate ),
			EvapAirInletNodeNum( EvapAirInletNodeNum ),
			EvapAirOutletNodeNum( EvapAirOutletNodeNum ),
			CoolingOnlyModeIsAvailable( CoolingOnlyModeIsAvailable ),
			CoolingOnlyRatedTotCap( CoolingOnlyRatedTotCap ),
			CoolingOnlyRatedSHR( CoolingOnlyRatedSHR ),
			CoolingOnlyRatedCOP( CoolingOnlyRatedCOP ),
			CoolingOnlyCapFTempCurve( CoolingOnlyCapFTempCurve ),
			CoolingOnlyCapFTempObjectNum( CoolingOnlyCapFTempObjectNum ),
			CoolingOnlyCapFFlowCurve( CoolingOnlyCapFFlowCurve ),
			CoolingOnlyCapFFlowObjectNum( CoolingOnlyCapFFlowObjectNum ),
			CoolingOnlyEIRFTempCurve( CoolingOnlyEIRFTempCurve ),
			CoolingOnlyEIRFTempObjectNum( CoolingOnlyEIRFTempObjectNum ),
			CoolingOnlyEIRFFlowCurve( CoolingOnlyEIRFFlowCurve ),
			CoolingOnlyEIRFFlowObjectNum( CoolingOnlyEIRFFlowObjectNum ),
			CoolingOnlyPLFFPLRCurve( CoolingOnlyPLFFPLRCurve ),
			CoolingOnlyPLFFPLRObjectNum( CoolingOnlyPLFFPLRObjectNum ),
			CoolingOnlySHRFTempCurve( CoolingOnlySHRFTempCurve ),
			CoolingOnlySHRFTempObjectNum( CoolingOnlySHRFTempObjectNum ),
			CoolingOnlySHRFFlowCurve( CoolingOnlySHRFFlowCurve ),
			CoolingOnlySHRFFlowObjectNum( CoolingOnlySHRFFlowObjectNum ),
			CoolingAndChargeModeAvailable( CoolingAndChargeModeAvailable ),
			CoolingAndChargeRatedTotCap( CoolingAndChargeRatedTotCap ),
			CoolingAndChargeRatedTotCapSizingFactor( CoolingAndChargeRatedTotCapSizingFactor ),
			CoolingAndChargeRatedChargeCap( CoolingAndChargeRatedChargeCap ),
			CoolingAndChargeRatedChargeCapSizingFactor( CoolingAndChargeRatedChargeCapSizingFactor ),
			CoolingAndChargeRatedSHR( CoolingAndChargeRatedSHR ),
			CoolingAndChargeCoolingRatedCOP( CoolingAndChargeCoolingRatedCOP ),
			CoolingAndChargeChargingRatedCOP( CoolingAndChargeChargingRatedCOP ),
			CoolingAndChargeCoolingCapFTempCurve( CoolingAndChargeCoolingCapFTempCurve ),
			CoolingAndChargeCoolingCapFTempObjectNum( CoolingAndChargeCoolingCapFTempObjectNum ),
			CoolingAndChargeCoolingCapFFlowCurve( CoolingAndChargeCoolingCapFFlowCurve ),
			CoolingAndChargeCoolingCapFFlowObjectNum( CoolingAndChargeCoolingCapFFlowObjectNum ),
			CoolingAndChargeCoolingEIRFTempCurve( CoolingAndChargeCoolingEIRFTempCurve ),
			CoolingAndChargeCoolingEIRFTempObjectNum( CoolingAndChargeCoolingEIRFTempObjectNum ),
			CoolingAndChargeCoolingEIRFFlowCurve( CoolingAndChargeCoolingEIRFFlowCurve ),
			CoolingAndChargeCoolingEIRFFlowObjectNum( CoolingAndChargeCoolingEIRFFlowObjectNum ),
			CoolingAndChargeCoolingPLFFPLRCurve( CoolingAndChargeCoolingPLFFPLRCurve ),
			CoolingAndChargeCoolingPLFFPLRObjectNum( CoolingAndChargeCoolingPLFFPLRObjectNum ),
			CoolingAndChargeChargingCapFTempCurve( CoolingAndChargeChargingCapFTempCurve ),
			CoolingAndChargeChargingCapFTempObjectNum( CoolingAndChargeChargingCapFTempObjectNum ),
			CoolingAndChargeChargingCapFEvapPLRCurve( CoolingAndChargeChargingCapFEvapPLRCurve ),
			CoolingAndChargeChargingCapFEvapPLRObjectNum( CoolingAndChargeChargingCapFEvapPLRObjectNum ),
			CoolingAndChargeChargingEIRFTempCurve( CoolingAndChargeChargingEIRFTempCurve ),
			CoolingAndChargeChargingEIRFTempObjectNum( CoolingAndChargeChargingEIRFTempObjectNum ),
			CoolingAndChargeChargingEIRFFLowCurve( CoolingAndChargeChargingEIRFFLowCurve ),
			CoolingAndChargeChargingEIRFFLowObjectNum( CoolingAndChargeChargingEIRFFLowObjectNum ),
			CoolingAndChargeChargingPLFFPLRCurve( CoolingAndChargeChargingPLFFPLRCurve ),
			CoolingAndChargeChargingPLFFPLRObjectNum( CoolingAndChargeChargingPLFFPLRObjectNum ),
			CoolingAndChargeSHRFTempCurve( CoolingAndChargeSHRFTempCurve ),
			CoolingAndChargeSHRFTempObjectNum( CoolingAndChargeSHRFTempObjectNum ),
			CoolingAndChargeSHRFFlowCurve( CoolingAndChargeSHRFFlowCurve ),
			CoolingAndChargeSHRFFlowObjectNum( CoolingAndChargeSHRFFlowObjectNum ),
			CoolingAndDischargeModeAvailable( CoolingAndDischargeModeAvailable ),
			CoolingAndDischargeRatedTotCap( CoolingAndDischargeRatedTotCap ),
			CoolingAndDischargeRatedTotCapSizingFactor( CoolingAndDischargeRatedTotCapSizingFactor ),
			CoolingAndDischargeRatedDischargeCap( CoolingAndDischargeRatedDischargeCap ),
			CoolingAndDischargeRatedDischargeCapSizingFactor( CoolingAndDischargeRatedDischargeCapSizingFactor ),
			CoolingAndDischargeRatedSHR( CoolingAndDischargeRatedSHR ),
			CoolingAndDischargeCoolingRatedCOP( CoolingAndDischargeCoolingRatedCOP ),
			CoolingAndDischargeDischargingRatedCOP( CoolingAndDischargeDischargingRatedCOP ),
			CoolingAndDischargeCoolingCapFTempCurve( CoolingAndDischargeCoolingCapFTempCurve ),
			CoolingAndDischargeCoolingCapFTempObjectNum( CoolingAndDischargeCoolingCapFTempObjectNum ),
			CoolingAndDischargeCoolingCapFFlowCurve( CoolingAndDischargeCoolingCapFFlowCurve ),
			CoolingAndDischargeCoolingCapFFlowObjectNum( CoolingAndDischargeCoolingCapFFlowObjectNum ),
			CoolingAndDischargeCoolingEIRFTempCurve( CoolingAndDischargeCoolingEIRFTempCurve ),
			CoolingAndDischargeCoolingEIRFTempObjectNum( CoolingAndDischargeCoolingEIRFTempObjectNum ),
			CoolingAndDischargeCoolingEIRFFlowCurve( CoolingAndDischargeCoolingEIRFFlowCurve ),
			CoolingAndDischargeCoolingEIRFFlowObjectNum( CoolingAndDischargeCoolingEIRFFlowObjectNum ),
			CoolingAndDischargeCoolingPLFFPLRCurve( CoolingAndDischargeCoolingPLFFPLRCurve ),
			CoolingAndDischargeCoolingPLFFPLRObjectNum( CoolingAndDischargeCoolingPLFFPLRObjectNum ),
			CoolingAndDischargeDischargingCapFTempCurve( CoolingAndDischargeDischargingCapFTempCurve ),
			CoolingAndDischargeDischargingCapFTempObjectNum( CoolingAndDischargeDischargingCapFTempObjectNum ),
			CoolingAndDischargeDischargingCapFFlowCurve( CoolingAndDischargeDischargingCapFFlowCurve ),
			CoolingAndDischargeDischargingCapFFlowObjectNum( CoolingAndDischargeDischargingCapFFlowObjectNum ),
			CoolingAndDischargeDischargingCapFEvapPLRCurve( CoolingAndDischargeDischargingCapFEvapPLRCurve ),
			CoolingAndDischargeDischargingCapFEvapPLRObjectNum( CoolingAndDischargeDischargingCapFEvapPLRObjectNum ),
			CoolingAndDischargeDischargingEIRFTempCurve( CoolingAndDischargeDischargingEIRFTempCurve ),
			CoolingAndDischargeDischargingEIRFTempObjectNum( CoolingAndDischargeDischargingEIRFTempObjectNum ),
			CoolingAndDischargeDischargingEIRFFLowCurve( CoolingAndDischargeDischargingEIRFFLowCurve ),
			CoolingAndDischargeDischargingEIRFFLowObjectNum( CoolingAndDischargeDischargingEIRFFLowObjectNum ),
			CoolingAndDischargeDischargingPLFFPLRCurve( CoolingAndDischargeDischargingPLFFPLRCurve ),
			CoolingAndDischargeDischargingPLFFPLRObjectNum( CoolingAndDischargeDischargingPLFFPLRObjectNum ),
			CoolingAndDischargeSHRFTempCurve( CoolingAndDischargeSHRFTempCurve ),
			CoolingAndDischargeSHRFTempObjectNum( CoolingAndDischargeSHRFTempObjectNum ),
			CoolingAndDischargeSHRFFlowCurve( CoolingAndDischargeSHRFFlowCurve ),
			CoolingAndDischargeSHRFFlowObjectNum( CoolingAndDischargeSHRFFlowObjectNum ),
			ChargeOnlyModeAvailable( ChargeOnlyModeAvailable ),
			ChargeOnlyRatedCapacity( ChargeOnlyRatedCapacity ),
			ChargeOnlyRatedCapacitySizingFactor( ChargeOnlyRatedCapacitySizingFactor ),
			ChargeOnlyRatedCOP( ChargeOnlyRatedCOP ),
			ChargeOnlyChargingCapFTempCurve( ChargeOnlyChargingCapFTempCurve ),
			ChargeOnlyChargingCapFTempObjectNum( ChargeOnlyChargingCapFTempObjectNum ),
			ChargeOnlyChargingEIRFTempCurve( ChargeOnlyChargingEIRFTempCurve ),
			ChargeOnlyChargingEIRFTempObjectNum( ChargeOnlyChargingEIRFTempObjectNum ),
			DischargeOnlyModeAvailable( DischargeOnlyModeAvailable ),
			DischargeOnlyRatedDischargeCap( DischargeOnlyRatedDischargeCap ),
			DischargeOnlyRatedDischargeCapSizingFactor( DischargeOnlyRatedDischargeCapSizingFactor ),
			DischargeOnlyRatedSHR( DischargeOnlyRatedSHR ),
			DischargeOnlyRatedCOP( DischargeOnlyRatedCOP ),
			DischargeOnlyCapFTempCurve( DischargeOnlyCapFTempCurve ),
			DischargeOnlyCapFTempObjectNum( DischargeOnlyCapFTempObjectNum ),
			DischargeOnlyCapFFlowCurve( DischargeOnlyCapFFlowCurve ),
			DischargeOnlyCapFFlowObjectNum( DischargeOnlyCapFFlowObjectNum ),
			DischargeOnlyEIRFTempCurve( DischargeOnlyEIRFTempCurve ),
			DischargeOnlyEIRFTempObjectNum( DischargeOnlyEIRFTempObjectNum ),
			DischargeOnlyEIRFFlowCurve( DischargeOnlyEIRFFlowCurve ),
			DischargeOnlyEIRFFlowObjectNum( DischargeOnlyEIRFFlowObjectNum ),
			DischargeOnlyPLFFPLRCurve( DischargeOnlyPLFFPLRCurve ),
			DischargeOnlyPLFFPLRObjectNum( DischargeOnlyPLFFPLRObjectNum ),
			DischargeOnlySHRFTempCurve( DischargeOnlySHRFTempCurve ),
			DischargeOnlySHRFTempObjectNum( DischargeOnlySHRFTempObjectNum ),
			DischargeOnlySHRFFLowCurve( DischargeOnlySHRFFLowCurve ),
			DischargeOnlySHRFFLowObjectNum( DischargeOnlySHRFFLowObjectNum ),
			AncillaryControlsPower( AncillaryControlsPower ),
			ColdWeatherMinimumTempLimit( ColdWeatherMinimumTempLimit ),
			ColdWeatherAncillaryPower( ColdWeatherAncillaryPower ),
			CondAirInletNodeNum( CondAirInletNodeNum ),
			CondAirOutletNodeNum( CondAirOutletNodeNum ),
			CondenserType( CondenserType ),
			CondenserAirVolumeFlow( CondenserAirVolumeFlow ),
			CondenserAirFlowSizingFactor( CondenserAirFlowSizingFactor ),
			CondenserAirMassFlow( CondenserAirMassFlow ),
			EvapCondEffect( EvapCondEffect ),
			CondInletTemp( CondInletTemp ),
			EvapCondPumpElecNomPower( EvapCondPumpElecNomPower ),
			EvapCondPumpElecEnergy( EvapCondPumpElecEnergy ),
			BasinHeaterPowerFTempDiff( BasinHeaterPowerFTempDiff ),
			BasinHeaterAvailSchedNum( BasinHeaterAvailSchedNum ),
			BasinHeaterSetpointTemp( BasinHeaterSetpointTemp ),
			EvapWaterSupplyMode( EvapWaterSupplyMode ),
			EvapWaterSupplyName( EvapWaterSupplyName ),
			EvapWaterSupTankID( EvapWaterSupTankID ),
			EvapWaterTankDemandARRID( EvapWaterTankDemandARRID ),
			CondensateCollectMode( CondensateCollectMode ),
			CondensateCollectName( CondensateCollectName ),
			CondensateTankID( CondensateTankID ),
			CondensateTankSupplyARRID( CondensateTankSupplyARRID ),
			StorageMedia( StorageMedia ),
			StorageFluidName( StorageFluidName ),
			StorageFluidIndex( StorageFluidIndex ),
			FluidStorageVolume( FluidStorageVolume ),
			IceStorageCapacity( IceStorageCapacity ),
			StorageCapacitySizingFactor( StorageCapacitySizingFactor ),
			MinimumFluidTankTempLimit( MinimumFluidTankTempLimit ),
			MaximumFluidTankTempLimit( MaximumFluidTankTempLimit ),
			RatedFluidTankTemp( RatedFluidTankTemp ),
			StorageAmbientNodeNum( StorageAmbientNodeNum ),
			StorageUA( StorageUA ),
			TESPlantConnectionAvailable( TESPlantConnectionAvailable ),
			TESPlantInletNodeNum( TESPlantInletNodeNum ),
			TESPlantOutletNodeNum( TESPlantOutletNodeNum ),
			TESPlantLoopNum( TESPlantLoopNum ),
			TESPlantLoopSideNum( TESPlantLoopSideNum ),
			TESPlantBranchNum( TESPlantBranchNum ),
			TESPlantCompNum( TESPlantCompNum ),
			TESPlantDesignVolumeFlowRate( TESPlantDesignVolumeFlowRate ),
			TESPlantDesignMassFlowRate( TESPlantDesignMassFlowRate ),
			TESPlantEffectiveness( TESPlantEffectiveness ),
			TimeElapsed( TimeElapsed ),
			IceFracRemain( IceFracRemain ),
			IceFracRemainLastTimestep( IceFracRemainLastTimestep ),
			FluidTankTempFinal( FluidTankTempFinal ),
			FluidTankTempFinalLastTimestep( FluidTankTempFinalLastTimestep ),
			QdotPlant( QdotPlant ),
			Q_Plant( Q_Plant ),
			QdotAmbient( QdotAmbient ),
			Q_Ambient( Q_Ambient ),
			QdotTES( QdotTES ),
			Q_TES( Q_TES ),
			ElecCoolingPower( ElecCoolingPower ),
			ElecCoolingEnergy( ElecCoolingEnergy ),
			EvapTotCoolingRate( EvapTotCoolingRate ),
			EvapTotCoolingEnergy( EvapTotCoolingEnergy ),
			EvapSensCoolingRate( EvapSensCoolingRate ),
			EvapSensCoolingEnergy( EvapSensCoolingEnergy ),
			EvapLatCoolingRate( EvapLatCoolingRate ),
			EvapLatCoolingEnergy( EvapLatCoolingEnergy ),
			RuntimeFraction( RuntimeFraction ),
			CondenserRuntimeFraction( CondenserRuntimeFraction ),
			ElectColdWeatherPower( ElectColdWeatherPower ),
			ElectColdWeatherEnergy( ElectColdWeatherEnergy ),
			ElectEvapCondBasinHeaterPower( ElectEvapCondBasinHeaterPower ),
			ElectEvapCondBasinHeaterEnergy( ElectEvapCondBasinHeaterEnergy ),
			EvapWaterConsumpRate( EvapWaterConsumpRate ),
			EvapWaterConsump( EvapWaterConsump ),
			EvapWaterStarvMakupRate( EvapWaterStarvMakupRate ),
			EvapWaterStarvMakup( EvapWaterStarvMakup ),
			EvapCondPumpElecPower( EvapCondPumpElecPower ),
			EvapCondPumpElecConsumption( EvapCondPumpElecConsumption )
		{}

	};

	// Object Data
	extern Array1D< PackagedTESCoolingCoilStruct > TESCoil;

	// Functions

	void
	SimTESCoil(
		std::string const & CompName, // name of the fan coil unit
		int & CompIndex,
		int const FanOpMode, // allows parent object to control fan mode
		int & TESOpMode,
		Optional< Real64 const > PartLoadRatio = _ // part load ratio (for single speed cycling unit)
	);

	void
	GetTESCoilInput();

	void
	InitTESCoil( int & TESCoilNum );

	void
	SizeTESCoil( int & TESCoilNum );

	void
	CalcTESCoilOffMode( int const TESCoilNum );

	void
	CalcTESCoilCoolingOnlyMode(
		int const TESCoilNum,
		int const FanOpMode,
		Real64 const PartLoadRatio
	);

	void
	CalcTESCoilCoolingAndChargeMode(
		int const TESCoilNum,
		int const FanOpMode,
		Real64 const PartLoadRatio
	);

	void
	CalcTESCoilCoolingAndDischargeMode(
		int const TESCoilNum,
		int const FanOpMode,
		Real64 const PartLoadRatio
	);

	void
	CalcTESCoilChargeOnlyMode( int const TESCoilNum );

	void
	CalcTESCoilDischargeOnlyMode(
		int const TESCoilNum,
		Real64 const PartLoadRatio
	);

	void
	UpdateTEStorage( int const TESCoilNum );

	void
	CalcTESWaterStorageTank( int const TESCoilNum );

	void
	CalcTESIceStorageTank( int const TESCoilNum );

	void
	UpdateColdWeatherProtection( int const TESCoilNum );

	void
	UpdateEvaporativeCondenserBasinHeater( int const TESCoilNum );

	void
	UpdateEvaporativeCondenserWaterUse(
		int const TESCoilNum,
		Real64 const HumRatAfterEvap,
		int const InletNodeNum
	);

	void
	GetTESCoilIndex(
		std::string const & CoilName,
		int & CoilIndex,
		bool & ErrorsFound,
		Optional_string_const CurrentModuleObject = _
	);

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

} // PackagedThermalStorageCoil

} // EnergyPlus

#endif
