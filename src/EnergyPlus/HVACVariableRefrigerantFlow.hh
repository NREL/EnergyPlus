#ifndef HVACVariableRefrigerantFlow_hh_INCLUDED
#define HVACVariableRefrigerantFlow_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace HVACVariableRefrigerantFlow {

	// Using/Aliasing

	// Data
	//MODULE PARAMETER DEFINITIONS
	// Compressor operation
	extern int const On; // normal compressor operation
	extern int const Off; // signal DXCoil that compressor shouldn't run

	//Heat Recovery System used
	extern int const No; // Heat Pump mode only
	extern int const Yes; // Heat Pump or Heat Recovery Mode (not available at this time)

	// Defrost strategy
	extern int const ReverseCycle; // uses reverse cycle defrost strategy
	extern int const Resistive; // uses electric resistance heater for defrost

	// Defrost control
	extern int const Timed; // defrost cycle is timed
	extern int const OnDemand; // defrost cycle occurs only when required

	// Thermostat Priority Control Type
	extern int const LoadPriority; // total of zone loads dictate operation in cooling or heating
	extern int const ZonePriority; // # of zones requireing cooling or heating dictate operation in cooling or heating
	extern int const ThermostatOffsetPriority; // zone with largest deviation from setpoint dictates operation
	extern int const ScheduledPriority; // cooling and heating modes are scheduled
	extern int const MasterThermostatPriority; // Master zone thermostat dictates operation
	extern int const FirstOnPriority; // first unit to respond dictates operation (not used at this time)

	//Water Systems
	extern int const CondensateDiscarded; // default mode where water is "lost"
	extern int const CondensateToTank; // collect coil condensate from air and store in water storage tank

	extern int const WaterSupplyFromMains; // mains water line used as water source
	extern int const WaterSupplyFromTank; // storage tank used as water source

	extern Real64 const MaxCap; // limit of zone terminal unit capacity

	// VRF System Types (strings used in integer conversions)
	extern int const NumVRFSystemTypes;
	extern int const VRF_HeatPump;
	extern Array1D_string const cVRFTypes;

	extern int const NumValidFuelTypes;
	extern Array1D_string const cValidFuelTypes;

	// Fuel Types
	extern int const FuelTypeElectric; // Fuel type for electricity
	extern int const FuelTypeNaturalGas; // Fuel type for natural gas
	extern int const FuelTypePropaneGas; // Fuel type for propane gas
	extern int const FuelTypeDiesel; // Fuel type for diesel
	extern int const FuelTypeGasoline; // Fuel type for gasoline
	extern int const FuelTypeFuelOil1; // Fuel type for fuel oil #1
	extern int const FuelTypeFuelOil2; // Fuel type for fuel oil #2
	extern int const FuelTypeOtherFuel1; // Fuel type for other fuel #1
	extern int const FuelTypeOtherFuel2; // Fuel type for other fuel #2

	// curve type for equivalent piping losses (not necessarily the same value used in CurveManager)
	extern int const BiQuadratic;

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	extern bool GetVRFInputFlag; // Flag set to make sure you get input once
	extern Array1D_bool CheckEquipName; // Flag set to check equipment connections once
	extern int NumVRFCond; // total number of VRF condensers
	extern int NumVRFTU; // total number of VRF terminal units
	extern int NumVRFTULists; // The number of VRF TU lists
	extern Real64 CompOnMassFlow; // Supply air mass flow rate w/ compressor ON
	extern Real64 OACompOnMassFlow; // OA mass flow rate w/ compressor ON
	extern Real64 CompOffMassFlow; // Supply air mass flow rate w/ compressor OFF
	extern Real64 OACompOffMassFlow; // OA mass flow rate w/ compressor OFF
	extern Real64 CompOnFlowRatio; // fan flow ratio when coil on
	extern Real64 CompOffFlowRatio; // fan flow ratio when coil off
	extern Real64 FanSpeedRatio; // ratio of air flow ratio passed to fan object
	extern Array1D_bool HeatingLoad; // defines a heating load on VRFTerminalUnits
	extern Array1D_bool CoolingLoad; // defines a cooling load on VRFTerminalUnits
	extern Array1D_bool LastModeHeating; // defines last mode was heating mode
	extern Array1D_bool LastModeCooling; // defines last mode was cooling mode
	extern Array1D< Real64 > MaxCoolingCapacity; // maximum capacity of any terminal unit
	extern Array1D< Real64 > MaxHeatingCapacity; // maximum capacity of any terminal unit
	extern Array1D< Real64 > CoolCombinationRatio; // ratio of terminal unit capacity to VRF condenser capacity
	extern Array1D< Real64 > HeatCombinationRatio; // ratio of terminal unit capacity to VRF condenser capacity
	extern Real64 LoopDXCoolCoilRTF; // holds value of DX cooling coil RTF
	extern Real64 LoopDXHeatCoilRTF; // holds value of DX heating coil RTF
	extern Real64 CondenserWaterMassFlowRate; // VRF water-cooled condenser mass flow rate (kg/s)
	extern Array1D_int NumCoolingLoads; // number of TU's requesting cooling
	extern Array1D_int NumHeatingLoads; // number of TU's requesting heating
	extern Array1D< Real64 > MaxDeltaT; // maximum zone temperature difference from setpoint
	extern Array1D< Real64 > MinDeltaT; // minimum zone temperature difference from setpoint
	extern Array1D< Real64 > SumCoolingLoads; // sum of cooling loads
	extern Array1D< Real64 > SumHeatingLoads; // sum of heating loads

	// Subroutine Specifications for the Module
	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Algorithms for the module

	// Update routine to check convergence and update nodes
	//Private UpdateVRF

	// Reporting routines for module

	// Types

	struct VRFCondenserEquipment
	{
		// Members
		std::string Name; // Name of the VRF Terminal Unit
		int VRFSystemTypeNum; // integer equivalent of system type
		int VRFPlantTypeOfNum; // integer equivalent of index to DataPlant type
		int SourceLoopNum; // plant data for water-coole only
		int SourceLoopSideNum; // plant data for water-coole only
		int SourceBranchNum; // plant data for water-coole only
		int SourceCompNum; // plant data for water-coole only
		Real64 WaterCondenserDesignMassFlow; // plant data for water-coole only
		Real64 WaterCondenserMassFlow; // Water condenser flow rate (kg/s)
		Real64 QCondenser; // Water condenser heat rejection/absorption (W)
		Real64 QCondEnergy; // Water condenser heat rejection/aborption energy (J)
		Real64 CondenserSideOutletTemp; // Water condenser outlet temp (C)
		int SchedPtr; // Pointer to the correct schedule
		Real64 CoolingCapacity; // Nominal VRF heat pump cooling capacity (W)
		Real64 TotalCoolingCapacity; // Nominal VRF heat pump cooling capacity (W)
		Real64 CoolingCombinationRatio; // Ratio or terminal unit cooling capacity to VRF condenser capacity
		Real64 VRFCondPLR; // Condenser part-load ratio wrt total capacity
		Real64 VRFCondRTF; // Condenser runtime fraction
		Real64 VRFCondCyclingRatio; // Condenser cycling ratio below MinPLR
		Real64 CondenserInletTemp; // Condenser entering air temperature (C)
		Real64 CoolingCOP; // Nominal VRF heat pump cooling COP (W/W)
		Real64 OperatingCoolingCOP; // Operating VRF heat pump cooling COP (W/W)
		Real64 RatedCoolingPower; // Rated cooling power = Rated Cooling Capacity / Rated COP (W)
		Real64 HeatingCapacity; // Nominal VRF heat pump heating capacity (W)
		Real64 HeatingCapacitySizeRatio; // Ratio of heating to cooling when autosizing
		bool LockHeatingCapacity; // used in sizing to size VRF heat cap to VRF cool cap
		Real64 TotalHeatingCapacity; // Nominal VRF heat pump heating capacity (W)
		Real64 HeatingCombinationRatio; // Ratio or terminal unit heating capacity to VRF condenser capacity
		Real64 HeatingCOP; // Nominal VRF heat pump heating COP
		Real64 OperatingHeatingCOP; // Operating VRF heat pump heating COP
		Real64 RatedHeatingPower; // Rated heating power = Rated Heating Capacity / Rated COP (W)
		Real64 MinOATCooling; // Minimum outdoor air dry-bulb temp in cooling mode (C)
		Real64 MaxOATCooling; // Maximum outdoor air dry-bulb temp in cooling mode (C)
		Real64 MinOATHeating; // Minimum outdoor air dry-bulb temp in heating mode (C)
		Real64 MaxOATHeating; // Maximum outdoor air dry-bulb temp in heating mode (C)
		int CoolCapFT; // index to cooling capacity function of temperature curve
		int CoolEIRFT; // index to cooling EIR function of temperature curve
		int HeatCapFT; // index to heating capacity function of temperature curve
		int HeatEIRFT; // index to heating EIR function of temperature curve
		int CoolBoundaryCurvePtr; // index to cooling capacity boundary curve
		int HeatBoundaryCurvePtr; // index to cooling capacity boundary curve
		int EIRCoolBoundaryCurvePtr; // index to cooling EIR boundary curve
		int CoolEIRFPLR1; // index to cooling EIR function of PLR curve < 1
		int CoolEIRFPLR2; // index to cooling EIR function of PLR curve >= 1
		int CoolCapFTHi; // index to cooling capacity function of temperature curve
		int CoolEIRFTHi; // index to cooling EIR function of temperature curve
		int HeatCapFTHi; // index to heating capacity function of temperature curve
		int HeatEIRFTHi; // index to heating EIR function of temperature curve
		int EIRHeatBoundaryCurvePtr; // index to heating EIR boundary curve
		int HeatEIRFPLR1; // index to heating EIR function of PLR curve < 1
		int HeatEIRFPLR2; // index to heating EIR function of PLR curve >= 1
		int CoolPLFFPLR; // index to cooling PLF function of PLR curve
		int HeatPLFFPLR; // index to heating PLF function of PLR curve
		int HeatingPerformanceOATType; // Temperature type for heating performance curves
		Real64 MinPLR; // minimum PLR before cycling occurs
		int MasterZonePtr; // index to master thermostat zone
		int MasterZoneTUIndex; // index to TU in master thermostat zone
		int ThermostatPriority; // VRF priority control (1=LoadPriority, 2=ZonePriority, etc)
		int SchedPriorityPtr; // VRF priority control schedule pointer
		int ZoneTUListPtr; // index to zone terminal unit list
		bool HeatRecoveryUsed; // .TRUE. = heat recovery used
		Real64 VertPipeLngth; // vertical piping length (m)
		int PCFLengthCoolPtr; // piping correction factor for length in cooling mode curve index
		int PCFLengthCoolPtrType; // PCF for length curve type
		Real64 PCFHeightCool; // piping correction factor for height in cooling mode
		Real64 EquivPipeLngthCool; // equivalent piping length for cooling
		Real64 PipingCorrectionCooling; // piping correction factor for cooling
		int PCFLengthHeatPtr; // piping correction factor for length in heating mode curve index
		int PCFLengthHeatPtrType; // PCF for length curve type
		Real64 PCFHeightHeat; // piping correction factor for height in heating mode
		Real64 EquivPipeLngthHeat; // equivalent piping length for heating
		Real64 PipingCorrectionHeating; // piping correction factor for heating
		Real64 CCHeaterPower; // crankcase heater power per compressor (W)
		Real64 CompressorSizeRatio; // ratio of min compressor size to total capacity
		int NumCompressors; // number of compressors in VRF condenser
		Real64 MaxOATCCHeater; // maximum outdoor air dry-bulb temp for crankcase heater operation (C)
		int DefrostEIRPtr; // index to defrost EIR curve
		Real64 DefrostFraction; // defrost time period fraction (hr)
		int DefrostStrategy; // Type of defrost (reversecycle or resistive)
		int DefrostControl; // type of defrost control (timed or ondemand)
		Real64 DefrostCapacity; // capacity of resistive defrost heating element (W)
		Real64 DefrostPower; // power used during defrost (W)
		Real64 DefrostConsumption; // energy used during defrost (J)
		Real64 MaxOATDefrost; // maximum outdoor air dry-bulb temp for defrost operation (C)
		int CondenserType; // condenser type, evap- or air-cooled
		int CondenserNodeNum; // condenser inlet node number
		bool SkipCondenserNodeNumCheck; // used to check for duplicate node names
		int CondenserOutletNodeNum; // condenser outlet node number
		Real64 WaterCondVolFlowRate; // water condenser volume flow rate (m3/s)
		Real64 EvapCondEffectiveness; // evaporative condenser effectiveness
		Real64 EvapCondAirVolFlowRate; // air volume flow rate through condenser (m3/s)
		Real64 EvapCondPumpPower; // evaporative condenser water pump power (W)
		int CoolCombRatioPTR; // index to cooling combination ratio curve pointer
		int HeatCombRatioPTR; // index to heating combination ratio curve pointer
		int OperatingMode; // VRF Condenser operating mode, 0=off, 1=cooling, 2=heating, 3=HR
		Real64 ElecPower; // VRF Condenser power (W)
		Real64 ElecCoolingPower; // VRF Condenser power in cooling mode (W)
		Real64 ElecHeatingPower; // VRF Condenser power in heating mode (W)
		Real64 CoolElecConsumption; // VRF Condenser cooling energy (J)
		Real64 HeatElecConsumption; // VRF Condenser heating energy (J)
		Real64 CrankCaseHeaterPower; // VRF Condenser crankcase heater power (W)
		Real64 CrankCaseHeaterElecConsumption; // VRF Condenser crankcase heater energy (J)
		Real64 EvapCondPumpElecPower; // VRF Condenser evaporatively cooled condenser pump power (W)
		Real64 EvapCondPumpElecConsumption; // VRF Condenser evaporatively cooled condenser pump elec consumption (J)
		Real64 EvapWaterConsumpRate; // VRF Condenser evaporatively cooled condenser water consumption (m3/s)
		int HRMaxTempLimitIndex; // Warning message recurring error index
		int CoolingMaxTempLimitIndex; // Warning message recurring error index
		int HeatingMaxTempLimitIndex; // Warning message recurring error index
		int FuelType; // Fuel type
		Real64 SUMultiplier; // exponential timer for mode changes
		Real64 TUCoolingLoad; // total TU cooling load for each VRF system
		Real64 TUHeatingLoad; // total TU heating load for each VRF system
		bool SwitchedMode; // used to derate capacity/power when system changes operating mode
		// begin variables used for heat recovery mode
		Real64 OperatingCOP; // Operating VRF heat pump COP (total TU capacity/total power)
		Real64 MinOATHeatRecovery; // Minimum outdoor air temperature for heat recovery operation (C)
		Real64 MaxOATHeatRecovery; // Maximum outdoor air temperature for heat recovery operation (C)
		int HRCAPFTCool; // Index to cool capacity as a function of temperature curve for heat recovery
		Real64 HRCAPFTCoolConst; // constant used if curve is blank
		int HRCAPFTCoolType; // Curve type for HRCAPFTCool
		Real64 HRInitialCoolCapFrac; // Fractional cooling degradation at the start of heat recovery from cooling mode
		Real64 HRCoolCapTC; // Time constant used to recover from intial degratation in cooling heat recovery
		int HREIRFTCool; // Index to cool EIR as a function of temperature curve for heat recovery
		Real64 HREIRFTCoolConst; // constant used if curve is blank
		int HREIRFTCoolType; // Curve type for HREIRFTCool
		Real64 HRInitialCoolEIRFrac; // Fractional EIR degradation at the start of heat recovery from cooling mode
		Real64 HRCoolEIRTC; // Time constant used to recover from intial degratation in cooling heat recovery
		int HRCAPFTHeat; // Index to heat capacity as a function of temperature curve for heat recovery
		Real64 HRCAPFTHeatConst; // constant used if curve is blank
		int HRCAPFTHeatType; // Curve type for HRCAPFTHeat
		Real64 HRInitialHeatCapFrac; // Fractional heating degradation at the start of heat recovery from heating mode
		Real64 HRHeatCapTC; // Time constant used to recover from intial degratation in heating heat recovery
		int HREIRFTHeat; // Index to heat EIR as a function of temperature curve for heat recovery
		Real64 HREIRFTHeatConst; // constant used if curve is blank
		int HREIRFTHeatType; // Curve type for HREIRFTHeat
		Real64 HRInitialHeatEIRFrac; // Fractional EIR degradation at the start of heat recovery from heating mode
		Real64 HRHeatEIRTC; // Time constant used to recover from intial degratation in heating heat recovery
		bool HRCoolingActive; // heat recovery mode active in cooling mode
		bool HRHeatingActive; // heat recovery mode active in heating mode
		bool ModeChange; // tracks changes in operating mode
		bool HRModeChange; // tracks changes in heat recovery operating mode
		Real64 HRTimer; // timer used to model changes in system performance as mode changes
		Real64 HRTime; // length of time system has been in same mode (hr)
		int EIRFTempCoolErrorIndex; // warning message index for recurring warnings
		int EIRFTempHeatErrorIndex; // warning message index for recurring warnings
		int DefrostHeatErrorIndex; // warning message index for recurring warnings
		// end variables used for heat recovery mode
		// begin variables for Water System interactions
		int EvapWaterSupplyMode; // where does water come from
		std::string EvapWaterSupplyName; // name of water source e.g. water storage tank
		int EvapWaterSupTankID;
		int EvapWaterTankDemandARRID;
		std::string CondensateCollectName; // name of water source e.g. water storage tank
		int CondensateTankID;
		int CondensateTankSupplyARRID;
		Real64 CondensateVdot; // rate of water condensation from air stream [m3/s]
		Real64 CondensateVol; // amount of water condensed from air stream [m3]
		//end variables for water system interactions
		// begin variables for Basin Heater interactions
		Real64 BasinHeaterPowerFTempDiff; // Basin heater capacity per degree C below setpoint (W/C)
		Real64 BasinHeaterSetPointTemp; // setpoint temperature for basin heater operation (C)
		Real64 BasinHeaterPower; // Basin heater power (W)
		Real64 BasinHeaterConsumption; // Basin heater energy consumption (J)
		int BasinHeaterSchedulePtr; // Pointer to basin heater schedule
		//end variables for Basin Heater interactions
		bool EMSOverrideHPOperatingMode;
		Real64 EMSValueForHPOperatingMode;
		int HPOperatingModeErrorIndex;

		// Default Constructor
		VRFCondenserEquipment() :
			VRFSystemTypeNum( 0 ),
			VRFPlantTypeOfNum( 0 ),
			SourceLoopNum( 0 ),
			SourceLoopSideNum( 0 ),
			SourceBranchNum( 0 ),
			SourceCompNum( 0 ),
			WaterCondenserDesignMassFlow( 0.0 ),
			WaterCondenserMassFlow( 0.0 ),
			QCondenser( 0.0 ),
			QCondEnergy( 0.0 ),
			CondenserSideOutletTemp( 0.0 ),
			SchedPtr( -1 ),
			CoolingCapacity( 0.0 ),
			TotalCoolingCapacity( 0.0 ),
			CoolingCombinationRatio( 1.0 ),
			VRFCondPLR( 0.0 ),
			VRFCondRTF( 0.0 ),
			VRFCondCyclingRatio( 0.0 ),
			CondenserInletTemp( 0.0 ),
			CoolingCOP( 0.0 ),
			OperatingCoolingCOP( 0.0 ),
			RatedCoolingPower( 0.0 ),
			HeatingCapacity( 0.0 ),
			HeatingCapacitySizeRatio( 1.0 ),
			LockHeatingCapacity( false ),
			TotalHeatingCapacity( 0.0 ),
			HeatingCombinationRatio( 1.0 ),
			HeatingCOP( 0.0 ),
			OperatingHeatingCOP( 0.0 ),
			RatedHeatingPower( 0.0 ),
			MinOATCooling( 0.0 ),
			MaxOATCooling( 0.0 ),
			MinOATHeating( 0.0 ),
			MaxOATHeating( 0.0 ),
			CoolCapFT( 0 ),
			CoolEIRFT( 0 ),
			HeatCapFT( 0 ),
			HeatEIRFT( 0 ),
			CoolBoundaryCurvePtr( 0 ),
			HeatBoundaryCurvePtr( 0 ),
			EIRCoolBoundaryCurvePtr( 0 ),
			CoolEIRFPLR1( 0 ),
			CoolEIRFPLR2( 0 ),
			CoolCapFTHi( 0 ),
			CoolEIRFTHi( 0 ),
			HeatCapFTHi( 0 ),
			HeatEIRFTHi( 0 ),
			EIRHeatBoundaryCurvePtr( 0 ),
			HeatEIRFPLR1( 0 ),
			HeatEIRFPLR2( 0 ),
			CoolPLFFPLR( 0 ),
			HeatPLFFPLR( 0 ),
			HeatingPerformanceOATType( 0 ),
			MinPLR( 0.0 ),
			MasterZonePtr( 0 ),
			MasterZoneTUIndex( 0 ),
			ThermostatPriority( 0 ),
			SchedPriorityPtr( 0 ),
			ZoneTUListPtr( 0 ),
			HeatRecoveryUsed( false ),
			VertPipeLngth( 0.0 ),
			PCFLengthCoolPtr( 0 ),
			PCFLengthCoolPtrType( 0 ),
			PCFHeightCool( 0.0 ),
			EquivPipeLngthCool( 0.0 ),
			PipingCorrectionCooling( 1.0 ),
			PCFLengthHeatPtr( 0 ),
			PCFLengthHeatPtrType( 0 ),
			PCFHeightHeat( 0.0 ),
			EquivPipeLngthHeat( 0.0 ),
			PipingCorrectionHeating( 1.0 ),
			CCHeaterPower( 0.0 ),
			CompressorSizeRatio( 0.0 ),
			NumCompressors( 0 ),
			MaxOATCCHeater( 0.0 ),
			DefrostEIRPtr( 0 ),
			DefrostFraction( 0.0 ),
			DefrostStrategy( 0 ),
			DefrostControl( 0 ),
			DefrostCapacity( 0.0 ),
			DefrostPower( 0.0 ),
			DefrostConsumption( 0.0 ),
			MaxOATDefrost( 0.0 ),
			CondenserType( 0 ),
			CondenserNodeNum( 0 ),
			SkipCondenserNodeNumCheck( false ),
			CondenserOutletNodeNum( 0 ),
			WaterCondVolFlowRate( 0.0 ),
			EvapCondEffectiveness( 0.0 ),
			EvapCondAirVolFlowRate( 0.0 ),
			EvapCondPumpPower( 0.0 ),
			CoolCombRatioPTR( 0 ),
			HeatCombRatioPTR( 0 ),
			OperatingMode( 0 ),
			ElecPower( 0.0 ),
			ElecCoolingPower( 0.0 ),
			ElecHeatingPower( 0.0 ),
			CoolElecConsumption( 0.0 ),
			HeatElecConsumption( 0.0 ),
			CrankCaseHeaterPower( 0.0 ),
			CrankCaseHeaterElecConsumption( 0.0 ),
			EvapCondPumpElecPower( 0.0 ),
			EvapCondPumpElecConsumption( 0.0 ),
			EvapWaterConsumpRate( 0.0 ),
			HRMaxTempLimitIndex( 0 ),
			CoolingMaxTempLimitIndex( 0 ),
			HeatingMaxTempLimitIndex( 0 ),
			FuelType( 0 ),
			SUMultiplier( 0.0 ),
			TUCoolingLoad( 0.0 ),
			TUHeatingLoad( 0.0 ),
			SwitchedMode( false ),
			OperatingCOP( 0.0 ),
			MinOATHeatRecovery( 0.0 ),
			MaxOATHeatRecovery( 0.0 ),
			HRCAPFTCool( 0 ),
			HRCAPFTCoolConst( 0.9 ),
			HRCAPFTCoolType( 0 ),
			HRInitialCoolCapFrac( 0.5 ),
			HRCoolCapTC( 0.15 ),
			HREIRFTCool( 0 ),
			HREIRFTCoolConst( 1.1 ),
			HREIRFTCoolType( 0 ),
			HRInitialCoolEIRFrac( 1.0 ),
			HRCoolEIRTC( 0.0 ),
			HRCAPFTHeat( 0 ),
			HRCAPFTHeatConst( 1.1 ),
			HRCAPFTHeatType( 0 ),
			HRInitialHeatCapFrac( 1.0 ),
			HRHeatCapTC( 0.0 ),
			HREIRFTHeat( 0 ),
			HREIRFTHeatConst( 1.1 ),
			HREIRFTHeatType( 0 ),
			HRInitialHeatEIRFrac( 1.0 ),
			HRHeatEIRTC( 0.0 ),
			HRCoolingActive( false ),
			HRHeatingActive( false ),
			ModeChange( false ),
			HRModeChange( false ),
			HRTimer( 0.0 ),
			HRTime( 0.0 ),
			EIRFTempCoolErrorIndex( 0 ),
			EIRFTempHeatErrorIndex( 0 ),
			DefrostHeatErrorIndex( 0 ),
			EvapWaterSupplyMode( WaterSupplyFromMains ),
			EvapWaterSupTankID( 0 ),
			EvapWaterTankDemandARRID( 0 ),
			CondensateTankID( 0 ),
			CondensateTankSupplyARRID( 0 ),
			CondensateVdot( 0.0 ),
			CondensateVol( 0.0 ),
			BasinHeaterPowerFTempDiff( 0.0 ),
			BasinHeaterSetPointTemp( 0.0 ),
			BasinHeaterPower( 0.0 ),
			BasinHeaterConsumption( 0.0 ),
			BasinHeaterSchedulePtr( 0 ),
			EMSOverrideHPOperatingMode( false ),
			EMSValueForHPOperatingMode( 0.0 ),
			HPOperatingModeErrorIndex( 0 )
		{}

		// Member Constructor
		VRFCondenserEquipment(
			std::string const & Name, // Name of the VRF Terminal Unit
			int const VRFSystemTypeNum, // integer equivalent of system type
			int const VRFPlantTypeOfNum, // integer equivalent of index to DataPlant type
			int const SourceLoopNum, // plant data for water-coole only
			int const SourceLoopSideNum, // plant data for water-coole only
			int const SourceBranchNum, // plant data for water-coole only
			int const SourceCompNum, // plant data for water-coole only
			Real64 const WaterCondenserDesignMassFlow, // plant data for water-coole only
			Real64 const WaterCondenserMassFlow, // Water condenser flow rate (kg/s)
			Real64 const QCondenser, // Water condenser heat rejection/absorption (W)
			Real64 const QCondEnergy, // Water condenser heat rejection/aborption energy (J)
			Real64 const CondenserSideOutletTemp, // Water condenser outlet temp (C)
			int const SchedPtr, // Pointer to the correct schedule
			Real64 const CoolingCapacity, // Nominal VRF heat pump cooling capacity (W)
			Real64 const TotalCoolingCapacity, // Nominal VRF heat pump cooling capacity (W)
			Real64 const CoolingCombinationRatio, // Ratio or terminal unit cooling capacity to VRF condenser capacity
			Real64 const VRFCondPLR, // Condenser part-load ratio wrt total capacity
			Real64 const VRFCondRTF, // Condenser runtime fraction
			Real64 const VRFCondCyclingRatio, // Condenser cycling ratio below MinPLR
			Real64 const CondenserInletTemp, // Condenser entering air temperature (C)
			Real64 const CoolingCOP, // Nominal VRF heat pump cooling COP (W/W)
			Real64 const OperatingCoolingCOP, // Operating VRF heat pump cooling COP (W/W)
			Real64 const RatedCoolingPower, // Rated cooling power = Rated Cooling Capacity / Rated COP (W)
			Real64 const HeatingCapacity, // Nominal VRF heat pump heating capacity (W)
			Real64 const HeatingCapacitySizeRatio, // Ratio of heating to cooling when autosizing
			bool const LockHeatingCapacity, // used in sizing to size VRF heat cap to VRF cool cap
			Real64 const TotalHeatingCapacity, // Nominal VRF heat pump heating capacity (W)
			Real64 const HeatingCombinationRatio, // Ratio or terminal unit heating capacity to VRF condenser capacity
			Real64 const HeatingCOP, // Nominal VRF heat pump heating COP
			Real64 const OperatingHeatingCOP, // Operating VRF heat pump heating COP
			Real64 const RatedHeatingPower, // Rated heating power = Rated Heating Capacity / Rated COP (W)
			Real64 const MinOATCooling, // Minimum outdoor air dry-bulb temp in cooling mode (C)
			Real64 const MaxOATCooling, // Maximum outdoor air dry-bulb temp in cooling mode (C)
			Real64 const MinOATHeating, // Minimum outdoor air dry-bulb temp in heating mode (C)
			Real64 const MaxOATHeating, // Maximum outdoor air dry-bulb temp in heating mode (C)
			int const CoolCapFT, // index to cooling capacity function of temperature curve
			int const CoolEIRFT, // index to cooling EIR function of temperature curve
			int const HeatCapFT, // index to heating capacity function of temperature curve
			int const HeatEIRFT, // index to heating EIR function of temperature curve
			int const CoolBoundaryCurvePtr, // index to cooling capacity boundary curve
			int const HeatBoundaryCurvePtr, // index to cooling capacity boundary curve
			int const EIRCoolBoundaryCurvePtr, // index to cooling EIR boundary curve
			int const CoolEIRFPLR1, // index to cooling EIR function of PLR curve < 1
			int const CoolEIRFPLR2, // index to cooling EIR function of PLR curve >= 1
			int const CoolCapFTHi, // index to cooling capacity function of temperature curve
			int const CoolEIRFTHi, // index to cooling EIR function of temperature curve
			int const HeatCapFTHi, // index to heating capacity function of temperature curve
			int const HeatEIRFTHi, // index to heating EIR function of temperature curve
			int const EIRHeatBoundaryCurvePtr, // index to heating EIR boundary curve
			int const HeatEIRFPLR1, // index to heating EIR function of PLR curve < 1
			int const HeatEIRFPLR2, // index to heating EIR function of PLR curve >= 1
			int const CoolPLFFPLR, // index to cooling PLF function of PLR curve
			int const HeatPLFFPLR, // index to heating PLF function of PLR curve
			int const HeatingPerformanceOATType, // Temperature type for heating performance curves
			Real64 const MinPLR, // minimum PLR before cycling occurs
			int const MasterZonePtr, // index to master thermostat zone
			int const MasterZoneTUIndex, // index to TU in master thermostat zone
			int const ThermostatPriority, // VRF priority control (1=LoadPriority, 2=ZonePriority, etc)
			int const SchedPriorityPtr, // VRF priority control schedule pointer
			int const ZoneTUListPtr, // index to zone terminal unit list
			bool const HeatRecoveryUsed, // .TRUE. = heat recovery used
			Real64 const VertPipeLngth, // vertical piping length (m)
			int const PCFLengthCoolPtr, // piping correction factor for length in cooling mode curve index
			int const PCFLengthCoolPtrType, // PCF for length curve type
			Real64 const PCFHeightCool, // piping correction factor for height in cooling mode
			Real64 const EquivPipeLngthCool, // equivalent piping length for cooling
			Real64 const PipingCorrectionCooling, // piping correction factor for cooling
			int const PCFLengthHeatPtr, // piping correction factor for length in heating mode curve index
			int const PCFLengthHeatPtrType, // PCF for length curve type
			Real64 const PCFHeightHeat, // piping correction factor for height in heating mode
			Real64 const EquivPipeLngthHeat, // equivalent piping length for heating
			Real64 const PipingCorrectionHeating, // piping correction factor for heating
			Real64 const CCHeaterPower, // crankcase heater power per compressor (W)
			Real64 const CompressorSizeRatio, // ratio of min compressor size to total capacity
			int const NumCompressors, // number of compressors in VRF condenser
			Real64 const MaxOATCCHeater, // maximum outdoor air dry-bulb temp for crankcase heater operation (C)
			int const DefrostEIRPtr, // index to defrost EIR curve
			Real64 const DefrostFraction, // defrost time period fraction (hr)
			int const DefrostStrategy, // Type of defrost (reversecycle or resistive)
			int const DefrostControl, // type of defrost control (timed or ondemand)
			Real64 const DefrostCapacity, // capacity of resistive defrost heating element (W)
			Real64 const DefrostPower, // power used during defrost (W)
			Real64 const DefrostConsumption, // energy used during defrost (J)
			Real64 const MaxOATDefrost, // maximum outdoor air dry-bulb temp for defrost operation (C)
			int const CondenserType, // condenser type, evap- or air-cooled
			int const CondenserNodeNum, // condenser inlet node number
			bool const SkipCondenserNodeNumCheck, // used to check for duplicate node names
			int const CondenserOutletNodeNum, // condenser outlet node number
			Real64 const WaterCondVolFlowRate, // water condenser volume flow rate (m3/s)
			Real64 const EvapCondEffectiveness, // evaporative condenser effectiveness
			Real64 const EvapCondAirVolFlowRate, // air volume flow rate through condenser (m3/s)
			Real64 const EvapCondPumpPower, // evaporative condenser water pump power (W)
			int const CoolCombRatioPTR, // index to cooling combination ratio curve pointer
			int const HeatCombRatioPTR, // index to heating combination ratio curve pointer
			int const OperatingMode, // VRF Condenser operating mode, 0=off, 1=cooling, 2=heating, 3=HR
			Real64 const ElecPower, // VRF Condenser power (W)
			Real64 const ElecCoolingPower, // VRF Condenser power in cooling mode (W)
			Real64 const ElecHeatingPower, // VRF Condenser power in heating mode (W)
			Real64 const CoolElecConsumption, // VRF Condenser cooling energy (J)
			Real64 const HeatElecConsumption, // VRF Condenser heating energy (J)
			Real64 const CrankCaseHeaterPower, // VRF Condenser crankcase heater power (W)
			Real64 const CrankCaseHeaterElecConsumption, // VRF Condenser crankcase heater energy (J)
			Real64 const EvapCondPumpElecPower, // VRF Condenser evaporatively cooled condenser pump power (W)
			Real64 const EvapCondPumpElecConsumption, // VRF Condenser evaporatively cooled condenser pump elec consumption (J)
			Real64 const EvapWaterConsumpRate, // VRF Condenser evaporatively cooled condenser water consumption (m3/s)
			int const HRMaxTempLimitIndex, // Warning message recurring error index
			int const CoolingMaxTempLimitIndex, // Warning message recurring error index
			int const HeatingMaxTempLimitIndex, // Warning message recurring error index
			int const FuelType, // Fuel type
			Real64 const SUMultiplier, // exponential timer for mode changes
			Real64 const TUCoolingLoad, // total TU cooling load for each VRF system
			Real64 const TUHeatingLoad, // total TU heating load for each VRF system
			bool const SwitchedMode, // used to derate capacity/power when system changes operating mode
			Real64 const OperatingCOP, // Operating VRF heat pump COP (total TU capacity/total power)
			Real64 const MinOATHeatRecovery, // Minimum outdoor air temperature for heat recovery operation (C)
			Real64 const MaxOATHeatRecovery, // Maximum outdoor air temperature for heat recovery operation (C)
			int const HRCAPFTCool, // Index to cool capacity as a function of temperature curve for heat recovery
			Real64 const HRCAPFTCoolConst, // constant used if curve is blank
			int const HRCAPFTCoolType, // Curve type for HRCAPFTCool
			Real64 const HRInitialCoolCapFrac, // Fractional cooling degradation at the start of heat recovery from cooling mode
			Real64 const HRCoolCapTC, // Time constant used to recover from intial degratation in cooling heat recovery
			int const HREIRFTCool, // Index to cool EIR as a function of temperature curve for heat recovery
			Real64 const HREIRFTCoolConst, // constant used if curve is blank
			int const HREIRFTCoolType, // Curve type for HREIRFTCool
			Real64 const HRInitialCoolEIRFrac, // Fractional EIR degradation at the start of heat recovery from cooling mode
			Real64 const HRCoolEIRTC, // Time constant used to recover from intial degratation in cooling heat recovery
			int const HRCAPFTHeat, // Index to heat capacity as a function of temperature curve for heat recovery
			Real64 const HRCAPFTHeatConst, // constant used if curve is blank
			int const HRCAPFTHeatType, // Curve type for HRCAPFTHeat
			Real64 const HRInitialHeatCapFrac, // Fractional heating degradation at the start of heat recovery from heating mode
			Real64 const HRHeatCapTC, // Time constant used to recover from intial degratation in heating heat recovery
			int const HREIRFTHeat, // Index to heat EIR as a function of temperature curve for heat recovery
			Real64 const HREIRFTHeatConst, // constant used if curve is blank
			int const HREIRFTHeatType, // Curve type for HREIRFTHeat
			Real64 const HRInitialHeatEIRFrac, // Fractional EIR degradation at the start of heat recovery from heating mode
			Real64 const HRHeatEIRTC, // Time constant used to recover from intial degratation in heating heat recovery
			bool const HRCoolingActive, // heat recovery mode active in cooling mode
			bool const HRHeatingActive, // heat recovery mode active in heating mode
			bool const ModeChange, // tracks changes in operating mode
			bool const HRModeChange, // tracks changes in heat recovery operating mode
			Real64 const HRTimer, // timer used to model changes in system performance as mode changes
			Real64 const HRTime, // length of time system has been in same mode (hr)
			int const EIRFTempCoolErrorIndex, // warning message index for recurring warnings
			int const EIRFTempHeatErrorIndex, // warning message index for recurring warnings
			int const DefrostHeatErrorIndex, // warning message index for recurring warnings
			int const EvapWaterSupplyMode, // where does water come from
			std::string const & EvapWaterSupplyName, // name of water source e.g. water storage tank
			int const EvapWaterSupTankID,
			int const EvapWaterTankDemandARRID,
			std::string const & CondensateCollectName, // name of water source e.g. water storage tank
			int const CondensateTankID,
			int const CondensateTankSupplyARRID,
			Real64 const CondensateVdot, // rate of water condensation from air stream [m3/s]
			Real64 const CondensateVol, // amount of water condensed from air stream [m3]
			Real64 const BasinHeaterPowerFTempDiff, // Basin heater capacity per degree C below setpoint (W/C)
			Real64 const BasinHeaterSetPointTemp, // setpoint temperature for basin heater operation (C)
			Real64 const BasinHeaterPower, // Basin heater power (W)
			Real64 const BasinHeaterConsumption, // Basin heater energy consumption (J)
			int const BasinHeaterSchedulePtr, // Pointer to basin heater schedule
			bool const EMSOverrideHPOperatingMode,
			Real64 const EMSValueForHPOperatingMode,
			int const HPOperatingModeErrorIndex
		) :
			Name( Name ),
			VRFSystemTypeNum( VRFSystemTypeNum ),
			VRFPlantTypeOfNum( VRFPlantTypeOfNum ),
			SourceLoopNum( SourceLoopNum ),
			SourceLoopSideNum( SourceLoopSideNum ),
			SourceBranchNum( SourceBranchNum ),
			SourceCompNum( SourceCompNum ),
			WaterCondenserDesignMassFlow( WaterCondenserDesignMassFlow ),
			WaterCondenserMassFlow( WaterCondenserMassFlow ),
			QCondenser( QCondenser ),
			QCondEnergy( QCondEnergy ),
			CondenserSideOutletTemp( CondenserSideOutletTemp ),
			SchedPtr( SchedPtr ),
			CoolingCapacity( CoolingCapacity ),
			TotalCoolingCapacity( TotalCoolingCapacity ),
			CoolingCombinationRatio( CoolingCombinationRatio ),
			VRFCondPLR( VRFCondPLR ),
			VRFCondRTF( VRFCondRTF ),
			VRFCondCyclingRatio( VRFCondCyclingRatio ),
			CondenserInletTemp( CondenserInletTemp ),
			CoolingCOP( CoolingCOP ),
			OperatingCoolingCOP( OperatingCoolingCOP ),
			RatedCoolingPower( RatedCoolingPower ),
			HeatingCapacity( HeatingCapacity ),
			HeatingCapacitySizeRatio( HeatingCapacitySizeRatio ),
			LockHeatingCapacity( LockHeatingCapacity ),
			TotalHeatingCapacity( TotalHeatingCapacity ),
			HeatingCombinationRatio( HeatingCombinationRatio ),
			HeatingCOP( HeatingCOP ),
			OperatingHeatingCOP( OperatingHeatingCOP ),
			RatedHeatingPower( RatedHeatingPower ),
			MinOATCooling( MinOATCooling ),
			MaxOATCooling( MaxOATCooling ),
			MinOATHeating( MinOATHeating ),
			MaxOATHeating( MaxOATHeating ),
			CoolCapFT( CoolCapFT ),
			CoolEIRFT( CoolEIRFT ),
			HeatCapFT( HeatCapFT ),
			HeatEIRFT( HeatEIRFT ),
			CoolBoundaryCurvePtr( CoolBoundaryCurvePtr ),
			HeatBoundaryCurvePtr( HeatBoundaryCurvePtr ),
			EIRCoolBoundaryCurvePtr( EIRCoolBoundaryCurvePtr ),
			CoolEIRFPLR1( CoolEIRFPLR1 ),
			CoolEIRFPLR2( CoolEIRFPLR2 ),
			CoolCapFTHi( CoolCapFTHi ),
			CoolEIRFTHi( CoolEIRFTHi ),
			HeatCapFTHi( HeatCapFTHi ),
			HeatEIRFTHi( HeatEIRFTHi ),
			EIRHeatBoundaryCurvePtr( EIRHeatBoundaryCurvePtr ),
			HeatEIRFPLR1( HeatEIRFPLR1 ),
			HeatEIRFPLR2( HeatEIRFPLR2 ),
			CoolPLFFPLR( CoolPLFFPLR ),
			HeatPLFFPLR( HeatPLFFPLR ),
			HeatingPerformanceOATType( HeatingPerformanceOATType ),
			MinPLR( MinPLR ),
			MasterZonePtr( MasterZonePtr ),
			MasterZoneTUIndex( MasterZoneTUIndex ),
			ThermostatPriority( ThermostatPriority ),
			SchedPriorityPtr( SchedPriorityPtr ),
			ZoneTUListPtr( ZoneTUListPtr ),
			HeatRecoveryUsed( HeatRecoveryUsed ),
			VertPipeLngth( VertPipeLngth ),
			PCFLengthCoolPtr( PCFLengthCoolPtr ),
			PCFLengthCoolPtrType( PCFLengthCoolPtrType ),
			PCFHeightCool( PCFHeightCool ),
			EquivPipeLngthCool( EquivPipeLngthCool ),
			PipingCorrectionCooling( PipingCorrectionCooling ),
			PCFLengthHeatPtr( PCFLengthHeatPtr ),
			PCFLengthHeatPtrType( PCFLengthHeatPtrType ),
			PCFHeightHeat( PCFHeightHeat ),
			EquivPipeLngthHeat( EquivPipeLngthHeat ),
			PipingCorrectionHeating( PipingCorrectionHeating ),
			CCHeaterPower( CCHeaterPower ),
			CompressorSizeRatio( CompressorSizeRatio ),
			NumCompressors( NumCompressors ),
			MaxOATCCHeater( MaxOATCCHeater ),
			DefrostEIRPtr( DefrostEIRPtr ),
			DefrostFraction( DefrostFraction ),
			DefrostStrategy( DefrostStrategy ),
			DefrostControl( DefrostControl ),
			DefrostCapacity( DefrostCapacity ),
			DefrostPower( DefrostPower ),
			DefrostConsumption( DefrostConsumption ),
			MaxOATDefrost( MaxOATDefrost ),
			CondenserType( CondenserType ),
			CondenserNodeNum( CondenserNodeNum ),
			SkipCondenserNodeNumCheck( SkipCondenserNodeNumCheck ),
			CondenserOutletNodeNum( CondenserOutletNodeNum ),
			WaterCondVolFlowRate( WaterCondVolFlowRate ),
			EvapCondEffectiveness( EvapCondEffectiveness ),
			EvapCondAirVolFlowRate( EvapCondAirVolFlowRate ),
			EvapCondPumpPower( EvapCondPumpPower ),
			CoolCombRatioPTR( CoolCombRatioPTR ),
			HeatCombRatioPTR( HeatCombRatioPTR ),
			OperatingMode( OperatingMode ),
			ElecPower( ElecPower ),
			ElecCoolingPower( ElecCoolingPower ),
			ElecHeatingPower( ElecHeatingPower ),
			CoolElecConsumption( CoolElecConsumption ),
			HeatElecConsumption( HeatElecConsumption ),
			CrankCaseHeaterPower( CrankCaseHeaterPower ),
			CrankCaseHeaterElecConsumption( CrankCaseHeaterElecConsumption ),
			EvapCondPumpElecPower( EvapCondPumpElecPower ),
			EvapCondPumpElecConsumption( EvapCondPumpElecConsumption ),
			EvapWaterConsumpRate( EvapWaterConsumpRate ),
			HRMaxTempLimitIndex( HRMaxTempLimitIndex ),
			CoolingMaxTempLimitIndex( CoolingMaxTempLimitIndex ),
			HeatingMaxTempLimitIndex( HeatingMaxTempLimitIndex ),
			FuelType( FuelType ),
			SUMultiplier( SUMultiplier ),
			TUCoolingLoad( TUCoolingLoad ),
			TUHeatingLoad( TUHeatingLoad ),
			SwitchedMode( SwitchedMode ),
			OperatingCOP( OperatingCOP ),
			MinOATHeatRecovery( MinOATHeatRecovery ),
			MaxOATHeatRecovery( MaxOATHeatRecovery ),
			HRCAPFTCool( HRCAPFTCool ),
			HRCAPFTCoolConst( HRCAPFTCoolConst ),
			HRCAPFTCoolType( HRCAPFTCoolType ),
			HRInitialCoolCapFrac( HRInitialCoolCapFrac ),
			HRCoolCapTC( HRCoolCapTC ),
			HREIRFTCool( HREIRFTCool ),
			HREIRFTCoolConst( HREIRFTCoolConst ),
			HREIRFTCoolType( HREIRFTCoolType ),
			HRInitialCoolEIRFrac( HRInitialCoolEIRFrac ),
			HRCoolEIRTC( HRCoolEIRTC ),
			HRCAPFTHeat( HRCAPFTHeat ),
			HRCAPFTHeatConst( HRCAPFTHeatConst ),
			HRCAPFTHeatType( HRCAPFTHeatType ),
			HRInitialHeatCapFrac( HRInitialHeatCapFrac ),
			HRHeatCapTC( HRHeatCapTC ),
			HREIRFTHeat( HREIRFTHeat ),
			HREIRFTHeatConst( HREIRFTHeatConst ),
			HREIRFTHeatType( HREIRFTHeatType ),
			HRInitialHeatEIRFrac( HRInitialHeatEIRFrac ),
			HRHeatEIRTC( HRHeatEIRTC ),
			HRCoolingActive( HRCoolingActive ),
			HRHeatingActive( HRHeatingActive ),
			ModeChange( ModeChange ),
			HRModeChange( HRModeChange ),
			HRTimer( HRTimer ),
			HRTime( HRTime ),
			EIRFTempCoolErrorIndex( EIRFTempCoolErrorIndex ),
			EIRFTempHeatErrorIndex( EIRFTempHeatErrorIndex ),
			DefrostHeatErrorIndex( DefrostHeatErrorIndex ),
			EvapWaterSupplyMode( EvapWaterSupplyMode ),
			EvapWaterSupplyName( EvapWaterSupplyName ),
			EvapWaterSupTankID( EvapWaterSupTankID ),
			EvapWaterTankDemandARRID( EvapWaterTankDemandARRID ),
			CondensateCollectName( CondensateCollectName ),
			CondensateTankID( CondensateTankID ),
			CondensateTankSupplyARRID( CondensateTankSupplyARRID ),
			CondensateVdot( CondensateVdot ),
			CondensateVol( CondensateVol ),
			BasinHeaterPowerFTempDiff( BasinHeaterPowerFTempDiff ),
			BasinHeaterSetPointTemp( BasinHeaterSetPointTemp ),
			BasinHeaterPower( BasinHeaterPower ),
			BasinHeaterConsumption( BasinHeaterConsumption ),
			BasinHeaterSchedulePtr( BasinHeaterSchedulePtr ),
			EMSOverrideHPOperatingMode( EMSOverrideHPOperatingMode ),
			EMSValueForHPOperatingMode( EMSValueForHPOperatingMode ),
			HPOperatingModeErrorIndex( HPOperatingModeErrorIndex )
		{}

	};

	struct TerminalUnitListData
	{
		// Members
		std::string Name; // Name of the VRF Terminal Unit List
		int NumTUInList; // Number of VRF Terminal Units in List
		Array1D_int ZoneTUPtr; // index to VRF Terminal Unit
		Array1D_string ZoneTUName; // Name of the VRF Terminal Unit
		Array1D_bool IsSimulated; // TRUE if TU has been simulated
		Array1D< Real64 > TotalCoolLoad; // Total zone cooling coil load met by TU
		Array1D< Real64 > TotalHeatLoad; // Total zone heating coil load met by TU
		Array1D_bool CoolingCoilPresent; // FALSE if coil not present
		Array1D_bool HeatingCoilPresent; // FALSE if coil not present
		Array1D_bool TerminalUnitNotSizedYet; // TRUE if terminal unit not sized
		Array1D_bool HRHeatRequest; // defines a heating load on VRFTerminalUnits when QZnReq < 0
		Array1D_bool HRCoolRequest; // defines a cooling load on VRFTerminalUnits when QZnReq > 0
		Array1D_bool CoolingCoilAvailable; // cooling coil availability scheduled on
		Array1D_bool HeatingCoilAvailable; // cooling coil availability scheduled on
		Array1D_int CoolingCoilAvailSchPtr; // cooilng coil availability schedule index
		Array1D_int HeatingCoilAvailSchPtr; // heating coil availability schedule index

		// Default Constructor
		TerminalUnitListData() :
			NumTUInList( 0 )
		{}

		// Member Constructor
		TerminalUnitListData(
			std::string const & Name, // Name of the VRF Terminal Unit List
			int const NumTUInList, // Number of VRF Terminal Units in List
			Array1_int const & ZoneTUPtr, // index to VRF Terminal Unit
			Array1_string const & ZoneTUName, // Name of the VRF Terminal Unit
			Array1_bool const & IsSimulated, // TRUE if TU has been simulated
			Array1< Real64 > const & TotalCoolLoad, // Total zone cooling coil load met by TU
			Array1< Real64 > const & TotalHeatLoad, // Total zone heating coil load met by TU
			Array1_bool const & CoolingCoilPresent, // FALSE if coil not present
			Array1_bool const & HeatingCoilPresent, // FALSE if coil not present
			Array1_bool const & TerminalUnitNotSizedYet, // TRUE if terminal unit not sized
			Array1_bool const & HRHeatRequest, // defines a heating load on VRFTerminalUnits when QZnReq < 0
			Array1_bool const & HRCoolRequest, // defines a cooling load on VRFTerminalUnits when QZnReq > 0
			Array1_bool const & CoolingCoilAvailable, // cooling coil availability scheduled on
			Array1_bool const & HeatingCoilAvailable, // cooling coil availability scheduled on
			Array1_int const & CoolingCoilAvailSchPtr, // cooilng coil availability schedule index
			Array1_int const & HeatingCoilAvailSchPtr // heating coil availability schedule index
		) :
			Name( Name ),
			NumTUInList( NumTUInList ),
			ZoneTUPtr( ZoneTUPtr ),
			ZoneTUName( ZoneTUName ),
			IsSimulated( IsSimulated ),
			TotalCoolLoad( TotalCoolLoad ),
			TotalHeatLoad( TotalHeatLoad ),
			CoolingCoilPresent( CoolingCoilPresent ),
			HeatingCoilPresent( HeatingCoilPresent ),
			TerminalUnitNotSizedYet( TerminalUnitNotSizedYet ),
			HRHeatRequest( HRHeatRequest ),
			HRCoolRequest( HRCoolRequest ),
			CoolingCoilAvailable( CoolingCoilAvailable ),
			HeatingCoilAvailable( HeatingCoilAvailable ),
			CoolingCoilAvailSchPtr( CoolingCoilAvailSchPtr ),
			HeatingCoilAvailSchPtr( HeatingCoilAvailSchPtr )
		{}

	};

	struct VRFTerminalUnitEquipment
	{
		// Members
		std::string Name; // Name of the VRF Terminal Unit
		int VRFTUType_Num; // DataHVACGlobals VRF Terminal Unit type
		int SchedPtr; // Pointer to the correct schedule
		int VRFSysNum; // index to VRF Condenser
		int TUListIndex; // index to VRF Terminal Unit List
		int IndexToTUInTUList; // index to TU in VRF Terminal Unit List
		int ZoneNum; // index to zone where VRF Terminal Unit resides
		int VRFTUInletNodeNum; // VRF Terminal Unit inlet node number
		int VRFTUOutletNodeNum; // VRF Terminal Unit outlet node number
		int VRFTUOAMixerOANodeNum; // OA node number for this TU's OA mixer
		int VRFTUOAMixerRelNodeNum; // Relief node number for this TU's OA mixer
		int VRFTUOAMixerRetNodeNum; // Return node number for this TU's OA mixer
		Real64 MaxCoolAirVolFlow; // supply air volumetric flow rate during cooling operation [m3/s]
		Real64 MaxHeatAirVolFlow; // supply air volumetric flow rate during heating operation [m3/s]
		Real64 MaxNoCoolAirVolFlow; // supply air volumetric flow rate when no cooling [m3/s]
		Real64 MaxNoHeatAirVolFlow; // supply air volumetric flow rate when no heating [m3/s]
		Real64 MaxCoolAirMassFlow; // supply air mass flow rate during cooling operation [kg/s]
		Real64 MaxHeatAirMassFlow; // supply air mass flow rate during heating operation [kg/s]
		Real64 MaxNoCoolAirMassFlow; // supply air mass flow rate when no cooling [kg/s]
		Real64 MaxNoHeatAirMassFlow; // supply air mass flow rate when no heating [kg/s]
		Real64 CoolOutAirVolFlow; // OA volumetric flow rate during cooling operation [m3/s]
		Real64 HeatOutAirVolFlow; // OA volumetric flow rate during heating operation [m3/s]
		Real64 NoCoolHeatOutAirVolFlow; // OA volumetric flow rate when no cooling or heating [m3/s]
		Real64 CoolOutAirMassFlow; // OA mass flow rate during cooling operation [kg/s]
		Real64 HeatOutAirMassFlow; // OA mass flow rate during heating operation [kg/s]
		Real64 NoCoolHeatOutAirMassFlow; // OA mass flow rate when no cooling or heating [kg/s]
		int FanOpModeSchedPtr; // Pointer to the correct fan operating mode schedule
		int FanAvailSchedPtr; // Pointer to the correct fan availability schedule
		int FanIndex; // Index to fan object
		Real64 FanPower; // power reported by fan component
		int OpMode; // operation mode: 1 = cycling fan, cycling coil 2 = constant fan, cycling coil
		int FanPlace; // fan placement; 1=blow through, 2=draw through
		Real64 ActualFanVolFlowRate; // volumetric flow rate from fan object
		std::string OAMixerName; // name of outside air mixer
		int OAMixerIndex; // index to outside air mixer
		bool OAMixerUsed; // true if OA Mixer object is used
		int CoolCoilIndex; // index to terminal unit cooling coil
		int HeatCoilIndex; // index to terminal unit heating coil
		int DXCoolCoilType_Num; // type of VRF cooling coil
		int DXHeatCoilType_Num; // type of VRF cooling coil
		Real64 ParasiticElec; // parasitic electric for VRF terminal unit
		Real64 ParasiticOffElec; // parasitic electric for VRF terminal unit when off
		Real64 HeatingSpeedRatio; // Fan speed ratio in heating mode
		Real64 HeatingCapacitySizeRatio; // Ratio of heating to cooling when autosizing
		Real64 CoolingSpeedRatio; // Fan speed ratio in cooling mode
		Real64 ParasiticCoolElecPower; // Terminal unit cooling parasitic electric power [W]
		Real64 ParasiticHeatElecPower; // Terminal unit heating parasitic electric power [W]
		Real64 ParasiticElecCoolConsumption; // Terminal unit parasitic electric consumption in cooling [J]
		Real64 ParasiticElecHeatConsumption; // Terminal unit parasitic electric consumption in heating [J]
		bool CoolingCoilPresent; // FALSE if coil not present
		bool HeatingCoilPresent; // FALSE if coil not present
		std::string AvailManagerListName; // Name of an availability manager list object
		int AvailStatus;
		Real64 TerminalUnitSensibleRate; // sensible cooling/heating rate of VRF terminal unit (W)
		Real64 TerminalUnitLatentRate; // latent dehumidificatino/humidification rate of VRF terminal unit (W)
		Real64 TotalCoolingRate; // report variable for total cooling rate (W)
		Real64 TotalHeatingRate; // report variable for total heating rate (W)
		Real64 SensibleCoolingRate; // report variable for sensible cooling rate (W)
		Real64 SensibleHeatingRate; // report variable for sensible heating rate (W)
		Real64 LatentCoolingRate; // report variable for latent cooling rate (W)
		Real64 LatentHeatingRate; // report variable for latent heating rate (W)
		Real64 TotalCoolingEnergy; // report variable for total cooling energy (J)
		Real64 TotalHeatingEnergy; // report variable for total heating energy (J)
		Real64 SensibleCoolingEnergy; // report variable for sensible cooling energy (J)
		Real64 SensibleHeatingEnergy; // report variable for sensible heating energy (J)
		Real64 LatentCoolingEnergy; // report variable for latent cooling energy (J)
		Real64 LatentHeatingEnergy; // report variable for latent heating energy (J)
		bool EMSOverridePartLoadFrac; // User defined EMS function
		Real64 EMSValueForPartLoadFrac; // user defined value for EMS function
		int IterLimitExceeded; // index used for warning messages
		int FirstIterfailed; // index used for warning messages
		int ZonePtr; // pointer to a zone served by a VRF terminal unit
		int HVACSizingIndex; // index of a HVACSizing object for a VRF terminal
		// Default Constructor
		VRFTerminalUnitEquipment() :
			VRFTUType_Num( 0 ),
			SchedPtr( -1 ),
			VRFSysNum( 0 ),
			TUListIndex( 0 ),
			IndexToTUInTUList( 0 ),
			ZoneNum( 0 ),
			VRFTUInletNodeNum( 0 ),
			VRFTUOutletNodeNum( 0 ),
			VRFTUOAMixerOANodeNum( 0 ),
			VRFTUOAMixerRelNodeNum( 0 ),
			VRFTUOAMixerRetNodeNum( 0 ),
			MaxCoolAirVolFlow( 0.0 ),
			MaxHeatAirVolFlow( 0.0 ),
			MaxNoCoolAirVolFlow( 0.0 ),
			MaxNoHeatAirVolFlow( 0.0 ),
			MaxCoolAirMassFlow( 0.0 ),
			MaxHeatAirMassFlow( 0.0 ),
			MaxNoCoolAirMassFlow( 0.0 ),
			MaxNoHeatAirMassFlow( 0.0 ),
			CoolOutAirVolFlow( 0.0 ),
			HeatOutAirVolFlow( 0.0 ),
			NoCoolHeatOutAirVolFlow( 0.0 ),
			CoolOutAirMassFlow( 0.0 ),
			HeatOutAirMassFlow( 0.0 ),
			NoCoolHeatOutAirMassFlow( 0.0 ),
			FanOpModeSchedPtr( 0 ),
			FanAvailSchedPtr( 0 ),
			FanIndex( 0 ),
			FanPower( 0.0 ),
			OpMode( 0 ),
			FanPlace( 0 ),
			ActualFanVolFlowRate( 0.0 ),
			OAMixerIndex( 0 ),
			OAMixerUsed( false ),
			CoolCoilIndex( 0 ),
			HeatCoilIndex( 0 ),
			DXCoolCoilType_Num( 0 ),
			DXHeatCoilType_Num( 0 ),
			ParasiticElec( 0.0 ),
			ParasiticOffElec( 0.0 ),
			HeatingSpeedRatio( 1.0 ),
			HeatingCapacitySizeRatio( 1.0 ),
			CoolingSpeedRatio( 1.0 ),
			ParasiticCoolElecPower( 0.0 ),
			ParasiticHeatElecPower( 0.0 ),
			ParasiticElecCoolConsumption( 0.0 ),
			ParasiticElecHeatConsumption( 0.0 ),
			CoolingCoilPresent( true ),
			HeatingCoilPresent( true ),
			AvailStatus( 0 ),
			TerminalUnitSensibleRate( 0.0 ),
			TerminalUnitLatentRate( 0.0 ),
			TotalCoolingRate( 0.0 ),
			TotalHeatingRate( 0.0 ),
			SensibleCoolingRate( 0.0 ),
			SensibleHeatingRate( 0.0 ),
			LatentCoolingRate( 0.0 ),
			LatentHeatingRate( 0.0 ),
			TotalCoolingEnergy( 0.0 ),
			TotalHeatingEnergy( 0.0 ),
			SensibleCoolingEnergy( 0.0 ),
			SensibleHeatingEnergy( 0.0 ),
			LatentCoolingEnergy( 0.0 ),
			LatentHeatingEnergy( 0.0 ),
			EMSOverridePartLoadFrac( false ),
			EMSValueForPartLoadFrac( 0.0 ),
			IterLimitExceeded( 0 ),
			FirstIterfailed( 0 ),
			ZonePtr( 0 ),
			HVACSizingIndex( 0 )
		{}

		// Member Constructor
		VRFTerminalUnitEquipment(
			std::string const & Name, // Name of the VRF Terminal Unit
			int const VRFTUType_Num, // DataHVACGlobals VRF Terminal Unit type
			int const SchedPtr, // Pointer to the correct schedule
			int const VRFSysNum, // index to VRF Condenser
			int const TUListIndex, // index to VRF Terminal Unit List
			int const IndexToTUInTUList, // index to TU in VRF Terminal Unit List
			int const ZoneNum, // index to zone where VRF Terminal Unit resides
			int const VRFTUInletNodeNum, // VRF Terminal Unit inlet node number
			int const VRFTUOutletNodeNum, // VRF Terminal Unit outlet node number
			int const VRFTUOAMixerOANodeNum, // OA node number for this TU's OA mixer
			int const VRFTUOAMixerRelNodeNum, // Relief node number for this TU's OA mixer
			int const VRFTUOAMixerRetNodeNum, // Return node number for this TU's OA mixer
			Real64 const MaxCoolAirVolFlow, // supply air volumetric flow rate during cooling operation [m3/s]
			Real64 const MaxHeatAirVolFlow, // supply air volumetric flow rate during heating operation [m3/s]
			Real64 const MaxNoCoolAirVolFlow, // supply air volumetric flow rate when no cooling [m3/s]
			Real64 const MaxNoHeatAirVolFlow, // supply air volumetric flow rate when no heating [m3/s]
			Real64 const MaxCoolAirMassFlow, // supply air mass flow rate during cooling operation [kg/s]
			Real64 const MaxHeatAirMassFlow, // supply air mass flow rate during heating operation [kg/s]
			Real64 const MaxNoCoolAirMassFlow, // supply air mass flow rate when no cooling [kg/s]
			Real64 const MaxNoHeatAirMassFlow, // supply air mass flow rate when no heating [kg/s]
			Real64 const CoolOutAirVolFlow, // OA volumetric flow rate during cooling operation [m3/s]
			Real64 const HeatOutAirVolFlow, // OA volumetric flow rate during heating operation [m3/s]
			Real64 const NoCoolHeatOutAirVolFlow, // OA volumetric flow rate when no cooling or heating [m3/s]
			Real64 const CoolOutAirMassFlow, // OA mass flow rate during cooling operation [kg/s]
			Real64 const HeatOutAirMassFlow, // OA mass flow rate during heating operation [kg/s]
			Real64 const NoCoolHeatOutAirMassFlow, // OA mass flow rate when no cooling or heating [kg/s]
			int const FanOpModeSchedPtr, // Pointer to the correct fan operating mode schedule
			int const FanAvailSchedPtr, // Pointer to the correct fan availability schedule
			int const FanIndex, // Index to fan object
			Real64 const FanPower, // power reported by fan component
			int const OpMode, // operation mode: 1 = cycling fan, cycling coil 2 = constant fan, cycling coil
			int const FanPlace, // fan placement; 1=blow through, 2=draw through
			Real64 const ActualFanVolFlowRate, // volumetric flow rate from fan object
			std::string const & OAMixerName, // name of outside air mixer
			int const OAMixerIndex, // index to outside air mixer
			bool const OAMixerUsed, // true if OA Mixer object is used
			int const CoolCoilIndex, // index to terminal unit cooling coil
			int const HeatCoilIndex, // index to terminal unit heating coil
			int const DXCoolCoilType_Num, // type of VRF cooling coil
			int const DXHeatCoilType_Num, // type of VRF cooling coil
			Real64 const ParasiticElec, // parasitic electric for VRF terminal unit
			Real64 const ParasiticOffElec, // parasitic electric for VRF terminal unit when off
			Real64 const HeatingSpeedRatio, // Fan speed ratio in heating mode
			Real64 const HeatingCapacitySizeRatio, // Ratio of heating to cooling when autosizing
			Real64 const CoolingSpeedRatio, // Fan speed ratio in cooling mode
			Real64 const ParasiticCoolElecPower, // Terminal unit cooling parasitic electric power [W]
			Real64 const ParasiticHeatElecPower, // Terminal unit heating parasitic electric power [W]
			Real64 const ParasiticElecCoolConsumption, // Terminal unit parasitic electric consumption in cooling [J]
			Real64 const ParasiticElecHeatConsumption, // Terminal unit parasitic electric consumption in heating [J]
			bool const CoolingCoilPresent, // FALSE if coil not present
			bool const HeatingCoilPresent, // FALSE if coil not present
			std::string const & AvailManagerListName, // Name of an availability manager list object
			int const AvailStatus,
			Real64 const TerminalUnitSensibleRate, // sensible cooling/heating rate of VRF terminal unit (W)
			Real64 const TerminalUnitLatentRate, // latent dehumidificatino/humidification rate of VRF terminal unit (W)
			Real64 const TotalCoolingRate, // report variable for total cooling rate (W)
			Real64 const TotalHeatingRate, // report variable for total heating rate (W)
			Real64 const SensibleCoolingRate, // report variable for sensible cooling rate (W)
			Real64 const SensibleHeatingRate, // report variable for sensible heating rate (W)
			Real64 const LatentCoolingRate, // report variable for latent cooling rate (W)
			Real64 const LatentHeatingRate, // report variable for latent heating rate (W)
			Real64 const TotalCoolingEnergy, // report variable for total cooling energy (J)
			Real64 const TotalHeatingEnergy, // report variable for total heating energy (J)
			Real64 const SensibleCoolingEnergy, // report variable for sensible cooling energy (J)
			Real64 const SensibleHeatingEnergy, // report variable for sensible heating energy (J)
			Real64 const LatentCoolingEnergy, // report variable for latent cooling energy (J)
			Real64 const LatentHeatingEnergy, // report variable for latent heating energy (J)
			bool const EMSOverridePartLoadFrac, // User defined EMS function
			Real64 const EMSValueForPartLoadFrac, // user defined value for EMS function
			int const IterLimitExceeded, // index used for warning messages
			int const FirstIterfailed, // index used for warning messages
			int const ZonePtr, // pointer to a zone served by a VRF terminal
			int const HVACSizingIndex // index of a HVACSizing object for a VRF terminal
		) :
			Name( Name ),
			VRFTUType_Num( VRFTUType_Num ),
			SchedPtr( SchedPtr ),
			VRFSysNum( VRFSysNum ),
			TUListIndex( TUListIndex ),
			IndexToTUInTUList( IndexToTUInTUList ),
			ZoneNum( ZoneNum ),
			VRFTUInletNodeNum( VRFTUInletNodeNum ),
			VRFTUOutletNodeNum( VRFTUOutletNodeNum ),
			VRFTUOAMixerOANodeNum( VRFTUOAMixerOANodeNum ),
			VRFTUOAMixerRelNodeNum( VRFTUOAMixerRelNodeNum ),
			VRFTUOAMixerRetNodeNum( VRFTUOAMixerRetNodeNum ),
			MaxCoolAirVolFlow( MaxCoolAirVolFlow ),
			MaxHeatAirVolFlow( MaxHeatAirVolFlow ),
			MaxNoCoolAirVolFlow( MaxNoCoolAirVolFlow ),
			MaxNoHeatAirVolFlow( MaxNoHeatAirVolFlow ),
			MaxCoolAirMassFlow( MaxCoolAirMassFlow ),
			MaxHeatAirMassFlow( MaxHeatAirMassFlow ),
			MaxNoCoolAirMassFlow( MaxNoCoolAirMassFlow ),
			MaxNoHeatAirMassFlow( MaxNoHeatAirMassFlow ),
			CoolOutAirVolFlow( CoolOutAirVolFlow ),
			HeatOutAirVolFlow( HeatOutAirVolFlow ),
			NoCoolHeatOutAirVolFlow( NoCoolHeatOutAirVolFlow ),
			CoolOutAirMassFlow( CoolOutAirMassFlow ),
			HeatOutAirMassFlow( HeatOutAirMassFlow ),
			NoCoolHeatOutAirMassFlow( NoCoolHeatOutAirMassFlow ),
			FanOpModeSchedPtr( FanOpModeSchedPtr ),
			FanAvailSchedPtr( FanAvailSchedPtr ),
			FanIndex( FanIndex ),
			FanPower( FanPower ),
			OpMode( OpMode ),
			FanPlace( FanPlace ),
			ActualFanVolFlowRate( ActualFanVolFlowRate ),
			OAMixerName( OAMixerName ),
			OAMixerIndex( OAMixerIndex ),
			OAMixerUsed( OAMixerUsed ),
			CoolCoilIndex( CoolCoilIndex ),
			HeatCoilIndex( HeatCoilIndex ),
			DXCoolCoilType_Num( DXCoolCoilType_Num ),
			DXHeatCoilType_Num( DXHeatCoilType_Num ),
			ParasiticElec( ParasiticElec ),
			ParasiticOffElec( ParasiticOffElec ),
			HeatingSpeedRatio( HeatingSpeedRatio ),
			HeatingCapacitySizeRatio( HeatingCapacitySizeRatio ),
			CoolingSpeedRatio( CoolingSpeedRatio ),
			ParasiticCoolElecPower( ParasiticCoolElecPower ),
			ParasiticHeatElecPower( ParasiticHeatElecPower ),
			ParasiticElecCoolConsumption( ParasiticElecCoolConsumption ),
			ParasiticElecHeatConsumption( ParasiticElecHeatConsumption ),
			CoolingCoilPresent( CoolingCoilPresent ),
			HeatingCoilPresent( HeatingCoilPresent ),
			AvailManagerListName( AvailManagerListName ),
			AvailStatus( AvailStatus ),
			TerminalUnitSensibleRate( TerminalUnitSensibleRate ),
			TerminalUnitLatentRate( TerminalUnitLatentRate ),
			TotalCoolingRate( TotalCoolingRate ),
			TotalHeatingRate( TotalHeatingRate ),
			SensibleCoolingRate( SensibleCoolingRate ),
			SensibleHeatingRate( SensibleHeatingRate ),
			LatentCoolingRate( LatentCoolingRate ),
			LatentHeatingRate( LatentHeatingRate ),
			TotalCoolingEnergy( TotalCoolingEnergy ),
			TotalHeatingEnergy( TotalHeatingEnergy ),
			SensibleCoolingEnergy( SensibleCoolingEnergy ),
			SensibleHeatingEnergy( SensibleHeatingEnergy ),
			LatentCoolingEnergy( LatentCoolingEnergy ),
			LatentHeatingEnergy( LatentHeatingEnergy ),
			EMSOverridePartLoadFrac( EMSOverridePartLoadFrac ),
			EMSValueForPartLoadFrac( EMSValueForPartLoadFrac ),
			IterLimitExceeded( IterLimitExceeded ),
			FirstIterfailed( FirstIterfailed ),
			ZonePtr( ZonePtr ),
			HVACSizingIndex( HVACSizingIndex )
		{}

	};


	struct VRFTUNumericFieldData
	{
		// Members
		Array1D_string FieldNames;

		// Default Constructor
		VRFTUNumericFieldData()
		{}

		// Member Constructor
		VRFTUNumericFieldData(
			Array1_string const & FieldNames // Name of the HeatingCoil numeric field descriptions
			) :
			FieldNames( FieldNames )
		{}
	};


	// Object Data
	extern Array1D< VRFCondenserEquipment > VRF; // AirConditioner:VariableRefrigerantFlow object
	extern Array1D< VRFTerminalUnitEquipment > VRFTU; // ZoneHVAC:TerminalUnit:VariableRefrigerantFlow object
	extern Array1D< TerminalUnitListData > TerminalUnitList; // zoneTerminalUnitList object
	extern Array1D< VRFTUNumericFieldData > VRFTUNumericFields; // holds VRF TU numeric input fields character field name

	// Functions

	void
	SimulateVRF(
		std::string const & CompName,
		int const ZoneNum,
		bool const FirstHVACIteration,
		Real64 & SysOutputProvided,
		Real64 & LatOutputProvided,
		int & CompIndex
	);

	void
	SimVRFCondenserPlant(
		std::string const & VRFType, // Type of VRF
		int const VRFTypeNum, // Type of VRF in Plant equipment
		std::string const & VRFName, // User Specified Name of VRF
		int & VRFNum, // Index of Equipment
		bool const FirstHVACIteration, // Flag for first time through HVAC simulation
		bool & InitLoopEquip, // If not zero, calculate the max load for operating conditions
		Real64 const MyLoad, // Loop demand component will meet
		Real64 & MaxCap, // Maximum operating capacity of GSHP [W]
		Real64 & MinCap, // Minimum operating capacity of GSHP [W]
		Real64 & OptCap, // Optimal operating capacity of GSHP [W]
		int const LoopNum // The calling loop number
	);

	void
	CalcVRFCondenser(
		int const VRFCond, // index to VRF condenser
		bool const FirstHVACIteration // flag for first time through HVAC system simulation
	);

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetVRFInput();

	// End of Get Input subroutines for the Module
	//******************************************************************************

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitVRF(
		int const VRFTUNum,
		int const ZoneNum,
		bool const FirstHVACIteration,
		Real64 & OnOffAirFlowRatio,
		Real64 & QZnReq
	);

	void
	SetCompFlowRate(
		int const VRFTUNum,
		int const VRFCond,
		Optional_bool_const UseCurrentMode = _
	);

	void
	SizeVRF( int const VRFTUNum );

	void
	SizeVRFCondenser( int const VRFCond );

	// End Initialization Section of the Module
	//******************************************************************************

	// Begin Algorithm Section of the Module
	//******************************************************************************

	void
	SimVRF(
		int const VRFTUNum,
		bool const FirstHVACIteration,
		Real64 & OnOffAirFlowRatio,
		Real64 & SysOutputProvided,
		Real64 & LatOutputProvided,
		Real64 const QZnReq
	);

	void
	ControlVRF(
		int const VRFTUNum, // Index to VRF terminal unit
		Real64 const QZnReq, // Index to zone number
		bool const FirstHVACIteration, // flag for 1st HVAC iteration in the time step
		Real64 & PartLoadRatio, // unit part load ratio
		Real64 & OnOffAirFlowRatio // ratio of compressor ON airflow to AVERAGE airflow over timestep
	);

	void
	CalcVRF(
		int const VRFTUNum, // Unit index in VRF terminal unit array
		bool const FirstHVACIteration, // flag for 1st HVAC iteration in the time step
		Real64 const PartLoadRatio, // compressor part load fraction
		Real64 & LoadMet, // load met by unit (W)
		Real64 & OnOffAirFlowRatio, // ratio of ON air flow to average air flow
		Optional< Real64 > LatOutputProvided = _ // delivered latent capacity (W)
	);

	int
	GetVRFTUOutAirNode( int const VRFTUNum );

	int
	GetVRFTUZoneInletAirNode( int const VRFTUNum );

	int
	GetVRFTUMixedAirNode( int const VRFTUNum );

	int
	GetVRFTUReturnAirNode( int const VRFTUNum );

	void
	ReportVRFTerminalUnit( int const VRFTUNum ); // index to VRF terminal unit

	void
	ReportVRFCondenser( int const VRFCond ); // index to VRF condensing unit

	void
	UpdateVRFCondenser( int const VRFCond ); // index to VRF condensing unit

	Real64
	PLRResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = VRFTUNum
	);

	void
	SetAverageAirFlow(
		int const VRFTUNum, // Unit index
		Real64 const PartLoadRatio, // unit part load ratio
		Real64 & OnOffAirFlowRatio // ratio of compressor ON airflow to average airflow over timestep
	);

	void
	InitializeOperatingMode(
		bool const FirstHVACIteration, // flag for first time through HVAC systems
		int const VRFCond, // Condenser Unit index
		int const TUListNum, // Condenser Unit terminal unit list
		Real64 & OnOffAirFlowRatio // ratio of on to off flow rate
	);

	void
	LimitTUCapacity(
		int const VRFCond, // Condenser Unit index
		int const NumTUInList, // Number of terminal units in list
		Real64 const StartingCapacity, // temporary variable holding condenser capacity [W]
		Array1S< Real64 > const CapArray, // Array of coil capacities in either cooling or heating mode [W]
		Real64 & MaxLimit, // Maximum terminal unit capacity for coils in same operating mode [W]
		Real64 const AltCapacity, // temporary variable holding heat recovery capacity [W]
		Array1S< Real64 > const AltArray, // Array of coil capacities of heat recovery [W]
		Real64 & AltLimit // Maximum terminal unit capacity of heat recovery coils [W]
	);

	void
	LimitCoilCapacity(
		int const NumTUInList, // Number of terminal units in list
		Real64 const TotalCapacity, // temporary variable holding condenser capacity [W]
		Array1S< Real64 > const CapArray, // Array of coil capacities in either cooling or heating mode [W]
		Real64 & MaxLimit // Maximum terminal unit capacity for coils in same operating mode [W]
	);

	// End of Utility subroutines for the Module
	// *****************************************************************************

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

} // HVACVariableRefrigerantFlow

} // EnergyPlus

#endif
