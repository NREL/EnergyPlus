#ifndef DXCoils_hh_INCLUDED
#define DXCoils_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <DataHVACGlobals.hh>

namespace EnergyPlus {

namespace DXCoils {

	// Using/Aliasing
	using DataHVACGlobals::AirCooled;
	using DataHVACGlobals::DryBulbIndicator;

	// Data
	//MODULE PARAMETER DEFINITIONS
	// Defrost strategy (heat pump only)
	extern int const ReverseCycle; // uses reverse cycle defrost strategy
	extern int const Resistive; // uses electric resistance heater for defrost
	// Defrost control  (heat pump only)
	extern int const Timed; // defrost cycle is timed
	extern int const OnDemand; // defrost cycle occurs only when required
	// Compressor operation
	extern int const On; // normal compressor operation
	extern int const Off; // signal DXCoil that compressor shouldn't run

	extern Real64 const RatedInletAirTemp; // 26.6667C or 80F
	extern Real64 const RatedInletWetBulbTemp; // 19.44 or 67F
	extern Real64 const RatedInletAirHumRat; // Humidity ratio corresponding to 80F dry bulb/67F wet bulb
	extern Real64 const RatedOutdoorAirTemp; // 35 C or 95F
	extern Real64 const RatedInletAirTempHeat; // 21.11C or 70F
	extern Real64 const RatedOutdoorAirTempHeat; // 8.33 C or 47F
	extern Real64 const RatedInletWetBulbTempHeat; // 15.55 or 60F

	extern Real64 const DryCoilOutletHumRatioMin; // dry coil outlet minimum hum ratio kgH2O/kgdry air

	// Curve Types
	extern int const Linear;
	extern int const BiLinear;
	extern int const Quadratic;
	extern int const BiQuadratic;
	extern int const Cubic;

	// Multimode DX Coil
	extern int const MaxCapacityStages; // Maximum number of capacity stages supported
	extern int const MaxDehumidModes; // Maximum number of enhanced dehumidification modes supported
	extern int const MaxModes; // Maximum number of performance modes

	//Water Systems
	extern int const CondensateDiscarded; // default mode where water is "lost"
	extern int const CondensateToTank; // collect coil condensate from air and store in water storage tank

	extern int const WaterSupplyFromMains;
	extern int const WaterSupplyFromTank;

	extern int const NumValidOutputFuelTypes;
	extern Array1D_string const cValidOutputFuelTypes;

	// Fuel Types
	extern int const FuelTypeElectricity; // Fuel type for electricity
	extern int const FuelTypeNaturalGas; // Fuel type for natural gas
	extern int const FuelTypePropaneGas; // Fuel type for propane gas
	extern int const FuelTypeDiesel; // Fuel type for diesel
	extern int const FuelTypeGasoline; // Fuel type for gasoline
	extern int const FuelTypeFuelOil1; // Fuel type for fuel oil #1
	extern int const FuelTypeFuelOil2; // Fuel type for fuel oil #2
	extern int const FuelTypeOtherFuel1; // Fuel type for other fuel #1
	extern int const FuelTypeOtherFuel2; // Fuel type for other fuel #2

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	extern Array1D< Real64 > DXCoilOutletTemp; // DX coil outlet dry bulb temperature [C]
	extern Array1D< Real64 > DXCoilOutletHumRat; // DX coil outlet humidity ratio [kgWater/kgDryAir]
	extern Array1D< Real64 > DXCoilPartLoadRatio; // DX coil part-load ratio
	extern Array1D_int DXCoilFanOpMode; // supply air fan operating mode
	extern Array1D< Real64 > DXCoilFullLoadOutAirTemp; // DX coil full load outlet dry bulb temperature [C]
	extern Array1D< Real64 > DXCoilFullLoadOutAirHumRat; // DX coil full load outlet humidity ratio [kgWater/kgDryAir]
	extern Array1D< Real64 > DXCoilTotalCooling; // DX cooling coil total cooling output [W]
	extern Array1D< Real64 > DXCoilTotalHeating; // DX heating coil total heating output [W]
	extern Array1D< Real64 > DXCoilCoolInletAirWBTemp; // DX cooling coil inlet air wet-bulb temp [C]
	extern Array1D< Real64 > DXCoilHeatInletAirDBTemp; // DX heating coil inlet air dry-bulb temp [C]
	extern Array1D< Real64 > DXCoilHeatInletAirWBTemp; // DX heating coil inlet air wet-bulb temp [C]
	extern int CurDXCoilNum;

	extern int NumDXCoils; // Total number of DX coils
	extern Real64 HPWHHeatingCapacity; // Used by Heat Pump:Water Heater object as total water heating capacity [W]
	extern Real64 HPWHHeatingCOP; // Used by Heat Pump:Water Heater object as water heating COP [W/W]
	extern bool GetCoilsInputFlag; // First time, input is "gotten"
	extern int NumVRFHeatingCoils; // number of VRF heat pump heating coils
	extern int NumVRFCoolingCoils; // number of VRF heat pump cooling coils
	extern int NumDXHeatingCoils; // number of DX heat pump heating coils
	extern int NumDoe2DXCoils; // number of doe2 DX  coils
	extern int NumDXHeatPumpWaterHeaterPumpedCoils; // number of DX  water heater coils, pumped
	extern int NumDXHeatPumpWaterHeaterWrappedCoils; // number of wrapped tank HPWH coils
	extern int NumDXMulSpeedCoils; // number of DX coils with multi-speed compressor
	extern int NumDXMulModeCoils; // number of DX coils with multi-mode performance

	extern int NumDXMulSpeedCoolCoils; // number of multispeed DX cooling coils
	extern int NumDXMulSpeedHeatCoils; // number of multispeed DX heating coils
	extern Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Update routines to check convergence and update nodes

	// Common routines

	// External function calls

	// Types

	struct DXCoilData
	{
		// Members
		//          Some variables in this type are arrays (dimension=MaxModes) to support coil type
		//          COIL:DX:MultiMode:CoolingEmpirical.  Other coil types only use the first element.
		std::string Name; // Name of the DX Coil
		std::string DXCoilType; // type of coil
		int DXCoilType_Num; // Integer equivalent to DXCoilType
		std::string Schedule; // WaterCoil Operation Schedule
		int SchedPtr; // Pointer to the correct schedule
		//          RatedCoolCap, RatedSHR and RatedCOP do not include the thermal or electrical
		//          effects due to the supply air fan
		Array1D< Real64 > RatedTotCap; // Gross total cooling capacity at rated conditions [watts]
		Real64 HeatSizeRatio; // heat pump heating to cooling sizing ratio when autosized
		Array1D_bool RatedTotCapEMSOverrideOn; // if true, then EMS is calling to override rated total capacity
		Array1D< Real64 > RatedTotCapEMSOverrideValue; // value to use for EMS override
		Array1D< Real64 > RatedSHR; // Sensible heat ratio (sens cap/total cap) at rated conditions
		Array1D_bool RatedSHREMSOverrideOn; // if true, then EMS is calling to override Sensible heat ratio
		Array1D< Real64 > RatedSHREMSOverrideValue; // value to use for EMS override forSensible heat ratio
		Array1D< Real64 > RatedCOP; // Coefficient of performance at rated conditions
		Array1D< Real64 > RatedAirVolFlowRate; // Air volume flow rate through coil at rated conditions [m3/s]
		// This is adjusted for bypassed air if any (see BypassedFlowFrac)
		Array1D_bool RatedAirVolFlowRateEMSOverrideON; // if true, then EMS is calling to override Air volume flow rate
		Array1D< Real64 > RatedAirVolFlowRateEMSOverrideValue; // value to use for EMS override Air volume flow rate
		Array1D< Real64 > FanPowerPerEvapAirFlowRate; // Fan Power Per Air volume flow rate through the
		// Evaporator coil at rated conditions [W/(m3/s)]
		Array1D< Real64 > RatedAirMassFlowRate; // Air mass flow rate through coil at rated conditions [kg/s]
		// This is adjusted for bypassed air if any (see BypassedFlowFrac)
		Array1D< Real64 > BypassedFlowFrac; // Fraction of air flow bypassed around coil
		Array1D< Real64 > RatedCBF; // rated coil bypass factor, determined using RatedTotCap and RatedSHR
		int AirInNode; // Air inlet node number
		int AirOutNode; // Air outlet node number
		Array1D_int CCapFTemp; // index of total cooling capacity modifier curve
		// (function of entering wetbulb, outside drybulb)
		int CCapFTempErrorIndex; // Used for warning messages when output of CCapFTemp is negative
		Array1D_int TotCapTempModFacCurveType; // type of curve for CCapFTemp (cubic,quadratic,bi-quadratic)
		Array1D_int CCapFFlow; // index of total cooling capacity modifier curve
		// (function of actual supply air flow vs rated air flow)
		int CCapFFlowErrorIndex; // Used for warning messages when output of CCapFFlow is negative
		Array1D_int EIRFTemp; // index of energy input ratio modifier curve
		// (function of entering wetbulb, outside drybulb)
		int EIRFTempErrorIndex; // Used for warning messages when output of EIRFTemp is negative
		Array1D_int EIRTempModFacCurveType; // type of curve for EIRFTemp (cubic,quadratic,bi-quadratic)
		Array1D_int EIRFFlow; // index of energy input ratio modifier curve
		// (function of actual supply air flow vs rated air flow)
		int EIRFFlowErrorIndex; // Used for warning messages when output of EIRFFlow is negative
		Array1D_int PLFFPLR; // index of part-load factor vs part-load ratio curve
		bool ReportCoolingCoilCrankcasePower; // logical determines if the cooling coil crankcase heater power is reported
		Real64 CrankcaseHeaterCapacity; // total crankcase heater capacity [W]
		Real64 CrankcaseHeaterPower; // report variable for average crankcase heater power [W]
		Real64 MaxOATCrankcaseHeater; // maximum OAT for crankcase heater operation [C]
		Real64 CrankcaseHeaterConsumption; // report variable for total crankcase heater energy consumption [J]
		Real64 BasinHeaterPowerFTempDiff; // Basin heater capacity per degree C below setpoint (W/C)
		Real64 BasinHeaterSetPointTemp; // setpoint temperature for basin heater operation (C)
		int CompanionUpstreamDXCoil; // index number of the DX coil that is "upstream" of this DX coil. Currently used for
		// UnitarySystem:HeatPump:AirToAir for proper calculation of crankcase heater energy
		// consumption
		bool FindCompanionUpStreamCoil; // Flag to get the companion coil in Init.
		Array1D_int CondenserInletNodeNum; // Node number of outdoor condenser(s) (actually an evaporator for heating coils)
		int LowOutletTempIndex; // used for low outlet temperature warnings
		Real64 FullLoadOutAirTempLast; // used for low outlet temperature warnings
		Real64 FullLoadInletAirTempLast; // used for low outlet temperature warnings
		bool PrintLowOutTempMessage; // used to print warning message for low outlet air dry-bulb conditions
		std::string LowOutTempBuffer1; // holds warning message until next iteration (only prints 1 message/iteration)
		std::string LowOutTempBuffer2; // holds warning message until next iteration (only prints 1 message/iteration)
		int HeatingCoilPLFCurvePTR; // PLF curve index to gas or electric heating coil (used in latent degradation model)
		int BasinHeaterSchedulePtr; // Pointer to basin heater schedule
		// start of multi-speed compressor variables
		Real64 RatedTotCap2; // Gross total cooling capacity at rated conditions, low speed [watts]
		// Note: For HPWHs, RatedTotCap2   = Water Heating Capacity for Coil:DX:HPWH and
		//                  RatedTotCap(1) = Air Cooling Coil Capacity for Coil:DX:HPWH
		Real64 RatedSHR2; // Sensible heat ratio (sens cap/total cap) at rated conditions, low speed
		Real64 RatedCOP2; // Coefficient of performance at rated conditions, low speed
		Real64 RatedAirVolFlowRate2; // Air volume flow rate through unit at rated conditions, low speed [m3/s]
		Real64 RatedAirMassFlowRate2; // Air mass flow rate through unit at rated conditions, low speed [kg/s]
		Real64 RatedCBF2; // rated coil bypass factor (low speed), determined using RatedTotCap2 and RatedSHR2
		int CCapFTemp2; // index of total cooling capacity modifier curve (low speed)
		int EIRFTemp2; // index of energy input ratio modifier curve (low speed)
		// (function of entering wetbulb, outside drybulb)
		Real64 RatedEIR2; // rated energy input ratio (low speed, inverse of COP2)
		Real64 InternalStaticPressureDrop; // for rating VAV system
		bool RateWithInternalStaticAndFanObject;
		int SupplyFanIndex;
		std::string SupplyFanName;
		std::string CoilSystemName;
		// end of multi-speed compressor variables
		Array1D< Real64 > RatedEIR; // rated energy input ratio (inverse of COP)
		Real64 InletAirMassFlowRate;
		Real64 InletAirMassFlowRateMax;
		Real64 InletAirTemp;
		Real64 InletAirHumRat;
		Real64 InletAirEnthalpy;
		//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
		//  REAL(r64) :: InletAirPressure       =0.0d0
		Real64 OutletAirTemp;
		Real64 OutletAirHumRat;
		Real64 OutletAirEnthalpy;
		Real64 PartLoadRatio; // Ratio of actual sensible cooling load to steady-state sensible cooling capacity
		Real64 TotalCoolingEnergy;
		Real64 SensCoolingEnergy;
		Real64 LatCoolingEnergy;
		Real64 TotalCoolingEnergyRate;
		Real64 SensCoolingEnergyRate;
		Real64 LatCoolingEnergyRate;
		Real64 ElecCoolingConsumption;
		Real64 ElecCoolingPower;
		Real64 CoolingCoilRuntimeFraction; // Run time fraction of the DX cooling unit
		// start of variables used in heat pump heating coils only
		Real64 TotalHeatingEnergy;
		Real64 TotalHeatingEnergyRate;
		Real64 ElecHeatingConsumption;
		Real64 ElecHeatingPower;
		Real64 HeatingCoilRuntimeFraction; // Run time fraction of the DX heating unit
		int DefrostStrategy; // defrost strategy; 1=reverse-cycle, 2=resistive
		int DefrostControl; // defrost control; 1=timed, 2=on-demand
		int EIRFPLR; // index of energy input ratio vs part-load ratio curve
		int DefrostEIRFT; // index of defrost mode total cooling capacity for reverse cycle heat pump
		int RegionNum; // Region number for calculating HSPF of single speed DX heating coil
		Real64 MinOATCompressor; // Minimum OAT for heat pump compressor operation
		Real64 OATempCompressorOn; // The outdoor tempearture when the compressor is automatically turned back on,
		// if applicable, following automatic shut off. This field is used only for
		// HSPF calculation.
		Real64 MaxOATCompressor; // Maximum OAT for VRF heat pump compressor operation
		Real64 MaxOATDefrost; // Maximum OAT for defrost operation
		Real64 DefrostTime; // Defrost time period in hours
		Real64 DefrostCapacity; // Resistive defrost to nominal capacity (at 21.11C/8.33C) ratio
		Real64 HPCompressorRuntime; // keep track of compressor runtime
		Real64 HPCompressorRuntimeLast; // keep track of last time step compressor runtime (if simulation downshifts)
		Real64 TimeLeftToDefrost; // keep track of time left to defrost heat pump
		Real64 DefrostPower; // power used during defrost
		Real64 DefrostConsumption; // energy used during defrost
		int HeatingPerformanceOATType; // Heating performance curve OAT type (1-wetbulb, 2-drybulb)
		bool HPCoilIsInCoilSystemHeatingDX;
		bool OATempCompressorOnOffBlank;
		// end of variables used in heat pump heating coils only
		// start of variables for DX cooling coil latent degradation model
		Array1D< Real64 > Twet_Rated; // Nominal time for condensate to begin leaving the coil's
		// condensate drain line (sec)
		Array1D< Real64 > Gamma_Rated; // Initial moisture evaporation rate divided by steady-state
		// AC latent capacity (dimensionless)
		Array1D< Real64 > MaxONOFFCyclesperHour; // Maximum ON/OFF cycles per hour for the compressor (cycles/hour)
		Array1D< Real64 > LatentCapacityTimeConstant; // Time constant for latent capacity to reach steady state
		// after startup (sec)
		// end of variables for DX cooling coil latent degradation model
		Array1D_int CondenserType; // Type of condenser for DX cooling coil: AIR COOLED or EVAP COOLED
		// start of variables for DX cooling coil evaporative condenser option
		bool ReportEvapCondVars; // true if any performance mode includes an evap condenser
		Array1D< Real64 > EvapCondEffect; // effectiveness of the evaporatively cooled condenser
		// [high speed for multi-speed unit] (-)
		Real64 CondInletTemp; // Evap condenser inlet temperature [C], report variable
		Array1D< Real64 > EvapCondAirFlow; // Air flow rate through the evap condenser at high speed,
		// for water use calcs [m3/s]
		Array1D< Real64 > EvapCondPumpElecNomPower; // Nominal power input to the evap condenser water circulation pump
		// at high speed [W]
		Real64 EvapCondPumpElecPower; // Average power consumed by the evap condenser water circulation pump over
		// the time step [W]
		Real64 EvapCondPumpElecConsumption; // Electric energy consumed by the evap condenser water circulation pump [J]
		Real64 EvapWaterConsumpRate; // Evap condenser water consumption rate [m3/s]
		Real64 EvapWaterConsump; // Evap condenser water consumption [m3]
		Real64 EvapCondAirFlow2; // Air flow rate through the evap condenser at low speed, for water use calcs [m3/s]
		Real64 EvapCondEffect2; // effectiveness of the evaporatively cooled condenser at low speed (-)
		Real64 EvapCondPumpElecNomPower2; // Nominal power input to the evap condenser water circulation pump at low speed [W]
		Real64 BasinHeaterPower; // Basin heater power (W)
		Real64 BasinHeaterConsumption; // Basin heater energy consumption (J)
		// end of variables for DX cooling coil evaporative condenser option
		// start of variables for Multimode DX cooling coil
		int NumCapacityStages; // number of capacity stages, up to MaxCapacityStages for Multimode DX coil,
		// always 1 for other coils
		int NumDehumidModes; // number of enhanced dehumidification modes, up to MaxDehumidModes for Multimode DX coil,
		// always 0 for other coils)
		Array1D_string CoilPerformanceType; // Coil Performance object type
		Array1D_int CoilPerformanceType_Num; // Coil Performance object type number
		Array1D_string CoilPerformanceName; // Coil Performance object names
		Real64 CoolingCoilStg2RuntimeFrac; // Run time fraction of stage 2
		int DehumidificationMode; // Dehumidification mode for multimode coil,
		// 0=normal, 1+=enhanced dehumidification mode
		// end of variables for Multimode DX cooling coil
		// start of variables for heat pump water heater DX coil
		int WaterInNode; // Condenser water inlet node number for HPWH DX coil
		int WaterOutNode; // Condenser water outlet node number for HPWH DX coil
		int HCOPFTemp; // COP as a function of temperature curve index
		int HCOPFTempErrorIndex; // Used for warning messages when output of HCOPFTemp is negative
		int HCOPFTempCurveType; // COP as a function of temperature curve type
		int HCOPFAirFlow; // COP as a function of air flow rate ratio curve index
		int HCOPFAirFlowErrorIndex; // Used for warning messages when output of HCOPFAirFlow is negative
		int HCOPFWaterFlow; // COP as a function of water flow rate ratio curve index
		int HCOPFWaterFlowErrorIndex; // Used for warning messages when output of HCOPFWaterFlow is negative
		int HCapFTemp; // Heating capacity as a function of temperature curve index
		int HCapFTempErrorIndex; // Used for warning messages when output of HCapFTemp is negative
		int HCapFTempCurveType; // Heating capacity as a function of temperature curve type
		int HCapFAirFlow; // Heating capacity as a function of air flow rate ratio curve index
		int HCapFAirFlowErrorIndex; // Used for warning messages when output of HCapFAirFlow is negative
		int HCapFWaterFlow; // Heating capacity as a function of water flow rate ratio curve index
		int HCapFWaterFlowErrorIndex; // Used for warning messages when output of HCapFWaterFlow is negative
		int InletAirTemperatureType; // Specifies to use either air wet-bulb or dry-bulb temp for curve objects
		Real64 RatedInletDBTemp; // Rated inlet air dry-bulb temperature [C]
		Real64 RatedInletWBTemp; // Rated inlet air wet-bulb temperature [C]
		Real64 RatedInletWaterTemp; // Rated condenser water inlet temperature [C]
		//  REAL(r64) :: CondenserInletWaterTemp     = 0.0d0     ! Actual inlet water temperature to condenser of the HPWH DX coil [C]
		Real64 HPWHCondPumpElecNomPower; // Nominal power input to the condenser water circulation pump [W]
		Real64 HPWHCondPumpFracToWater; // Nominal power fraction to water for the condenser water circulation pump
		Real64 RatedHPWHCondWaterFlow; // Rated water flow rate through the condenser of the HPWH DX coil [m3/s]
		Real64 ElecWaterHeatingPower; // Total electric power consumed by compressor and condenser pump [W]
		Real64 ElecWaterHeatingConsumption; // Total electric consumption by compressor and condenser pump [J]
		bool FanPowerIncludedInCOP; // Indicates that fan heat is included in heating capacity and COP
		bool CondPumpHeatInCapacity; // Indicates that condenser pump heat is included in heating capacity
		bool CondPumpPowerInCOP; // Indicates that condenser pump power is included in heating COP
		// end of variables for heat pump water heater DX coil
		// Error tracking
		Real64 LowTempLast; // low ambient temp entering condenser when warning message occurred
		Real64 HighTempLast; // high ambient temp entering condenser when warning message occurred
		int ErrIndex1; // index/pointer to recurring error structure for Air volume flow rate per watt of
		// rated total cooling capacity error
		int ErrIndex2; // index/pointer to recurring error structure for PLF curve values must be >= 0.7. error
		int ErrIndex3; // index/pointer to recurring error structure for DX cooling coil runtime fraction > 1.0 warning
		int ErrIndex4; // index/pointer to recurring error structure for DX heating coil runtime fraction > 1.0 warning
		int LowAmbErrIndex; // index/pointer to recurring error structure for low ambient temp entering condenser
		int HighAmbErrIndex; // index/pointer to recurring error structure for high ambient temp entering condenser
		int PLFErrIndex; // index/pointer to recurring error structure for PLF <> 1 at speed 1 for a multiple speed coil
		int PLRErrIndex; // index/pointer to recurring error structure for PLR < .7
		bool PrintLowAmbMessage; // used to print warning message for low ambient conditions
		std::string LowAmbBuffer1; // holds warning message until next iteration (only prints 1 message/iteration)
		std::string LowAmbBuffer2; // holds warning message until next iteration (only prints 1 message/iteration)
		bool PrintHighAmbMessage; // used to print warning message for high ambient conditions
		std::string HighAmbBuffer1; // holds warning message until next iteration (only prints 1 message/iteration)
		std::string HighAmbBuffer2; // holds warning message until next iteration (only prints 1 message/iteration)
		//begin variables for Water System interactions
		int EvapWaterSupplyMode; // where does water come from
		std::string EvapWaterSupplyName; // name of water source e.g. water storage tank
		int EvapWaterSupTankID;
		int EvapWaterTankDemandARRID;
		int CondensateCollectMode; // where does water come from
		std::string CondensateCollectName; // name of water source e.g. water storage tank
		int CondensateTankID;
		int CondensateTankSupplyARRID;
		Real64 CondensateVdot; // rate of water condensation from air stream [m3/s]
		Real64 CondensateVol; // amount of water condensed from air stream [m3]
		//end variables for water system interactions
		// used to print low ambient warning message for DOE2 coil only after time step has incremented
		Real64 CurrentEndTimeLast; // end time of time step for last simulation time step
		Real64 TimeStepSysLast; // last system time step (used to check for downshifting)
		// for multispeed DX coil type
		int FuelType; // Fuel type
		int NumOfSpeeds; // Number of speeds
		bool PLRImpact; // Part load fraction applied to Speed Number > 1
		bool LatentImpact; // Latent degradation applied to Speed Number > 1
		Array1D_int MSErrIndex; // index flag for num speeds/recurring messages
		Array1D< Real64 > MSRatedTotCap; // Rated cooling capacity for MS heat pump [W]
		Array1D< Real64 > MSRatedSHR; // Rated SHR for MS heat pump [dimensionless]
		Array1D< Real64 > MSRatedCOP; // Rated COP for MS heat pump [dimensionless]
		Array1D< Real64 > MSRatedAirVolFlowRate; // Air volume flow rate through unit at rated conditions [m3/s]
		Array1D< Real64 > MSRatedAirMassFlowRate; // Air mass flow rate through unit at rated conditions [m3/s]
		Array1D< Real64 > MSRatedCBF; // rated coil bypass factor
		Array1D_int MSCCapFTemp; // index of total cooling capacity modifier curve
		Array1D_int MSCCapFFlow; // index of total cooling capacity modifier curve
		Array1D_int MSEIRFTemp; // index of energy input ratio modifier curve as a function of temperature
		Array1D_int MSEIRFFlow; // index of energy input ratio modifier curve as a function of flow fraction
		Array1D_int MSPLFFPLR; // index of part load factor as a function of part load ratio
		Array1D_int MSWasteHeat; // index of waste heat as a function of temperature
		Array1D< Real64 > MSWasteHeatFrac; // Waste heat fraction
		Array1D< Real64 > MSEvapCondEffect; // effectiveness of the evaporatively cooled condenser
		Array1D< Real64 > MSEvapCondAirFlow; // Air flow rate through the evap condenser for water use calcs [m3/s]
		Array1D< Real64 > MSEvapCondPumpElecNomPower; // Nominal power input to the evap condenser
		// water circulation pump
		Array1D_int MSTotCapTempModFacCurveType; // type of curve for CCapFTemp (cubic,quadratic,bi-quadratic)
		Array1D_int MSEIRTempModFacCurveType; // type of curve for EIRFTemp (cubic,quadratic,bi-quadratic)
		Array1D< Real64 > MSTwet_Rated; // Nominal time for condensate to begin leaving the coil's
		// condensate drain line (sec)
		Array1D< Real64 > MSGamma_Rated; // Initial moisture evaporation rate divided by steady-state
		// AC latent capacity (dimensionless)
		Array1D< Real64 > MSMaxONOFFCyclesperHour; // Maximum ON/OFF cycles per hour for the compressor (cycles/hour)
		Array1D< Real64 > MSLatentCapacityTimeConstant; // Time constant for latent capacity to reach steady state
		Array1D< Real64 > MSFanPowerPerEvapAirFlowRate;
		Real64 FuelUsed; // Energy used, in addition to electricity [W]
		Real64 FuelConsumed; // Energy consumed, in addition to electricity [J]
		// End of multispeed DX coil input
		// VRF system variables used for sizing
		bool CoolingCoilPresent; // FALSE if coil not present
		bool HeatingCoilPresent; // FALSE if coil not present
		bool ISHundredPercentDOASDXCoil; // FALSE if coil is regular dx coil
		Array1D_int SHRFTemp; // index of sensible heat ratio modifier curve
		// (function of entering wetbulb and drybulb)
		int SHRFTempErrorIndex; // Used for warning messages when output of SHRFTemp is negative
		Array1D_int SHRFTempCurveType; // type of curve for SHRFTemp (cubic,quadratic,bi-quadratic)
		Array1D_int SHRFFlow; // index of sensible heat ratio modifier curve
		// (function of actual supply air flow vs rated air flow)
		int SHRFFlowErrorIndex; // Used for warning messages when output of SHRFFlow is negative
		int SHRFTemp2; // index of sensible heat ratio modifier curve
		// (function of entering wetbulb and drybulb)
		int SHRFFlow2; // index of sensible heat ratio modifier curve
		// (function of actual supply air flow vs rated air flow)
		int SHRFTempCurveType2; // type of curve for SHRFTemp (cubic,quadratic,bi-quadratic)
		bool UserSHRCurveExists; // TRUE if user specified SHR modifier curve exists
		bool ASHRAE127StdRprt; // TRUE if user wishes to report ASHRAE 127 standard ratings

		int SecZonePtr; // index to the zone where the secondary coil is placed
		int SecCoilSHRFT; // index to the secondary coil sensible heat ratio temperature modifier curve
		int SecCoilSHRFF; // index to the secondary coil sensible heat ratio flor fraction modifier curve
		Real64 SecCoilAirFlow; // secondary coil air flow rate
		Real64 SecCoilAirFlowScalingFactor; // secondary coil air flow rate autosize scaling factor
		Real64 SecCoilRatedSHR; // secondary coil nominal or rated sensible heat ratio
		Real64 SecCoilSHR; // secondary coil current sensible heat ratio
		int SecZoneAirNodeNum; // secondary zone air node number
		Real64 EvapInletWetBulb; // secondary DX coil inlet wet bulb temperature (zone air node wet bulb temp.)
		Real64 SecCoilSensibleHeatGainRate; // secondary zone sensible heat gain rate [W]
		Real64 SecCoilTotalHeatRemovalRate; // secondary zone total heat removal rate [W]
		Real64 SecCoilSensibleHeatRemovalRate; // secondary zone sensible heat removal rate [W]
		Real64 SecCoilLatentHeatRemovalRate; // secondary zone latent heat removal rate [W]
		bool IsSecondaryDXCoilInZone; // true means secondary dx coil is zone instead of outside
		Real64 CompressorPartLoadRatio; // compressor part load ratio of the primary DX coil
		Array1D_int MSSecCoilSHRFT; // index to the multi speed secondary coil sensible heat ratio temperature modifier curve
		Array1D_int MSSecCoilSHRFF; //  index to the multi speed secondary coil sensible heat ratio flow fraction modifier curve
		Array1D< Real64 > MSSecCoilAirFlow; // multispeed secondary coil air flow rate
		Array1D< Real64 > MSSecCoilAirFlowScalingFactor; //multispeed secondary coil air flow rate autosize scaling factor
		Array1D< Real64 > MSSecCoilRatedSHR; // multispeed secondary coil nominal or rated sensible heat ratio
		int MSSpeedNumLS; // current low speed number of multspeed HP
		int MSSpeedNumHS; // current high speed number of multspeed HP
		Real64 MSSpeedRatio; // current speed ratio of multspeed HP
		Real64 MSCycRatio; // current cycling ratio of multspeed HP

		// Default Constructor
		DXCoilData() :
			DXCoilType_Num( 0 ),
			SchedPtr( 0 ),
			RatedTotCap( MaxModes, 0.0 ),
			HeatSizeRatio( 1.0 ),
			RatedTotCapEMSOverrideOn( MaxModes, false ),
			RatedTotCapEMSOverrideValue( MaxModes, 0.0 ),
			RatedSHR( MaxModes, 0.0 ),
			RatedSHREMSOverrideOn( MaxModes, false ),
			RatedSHREMSOverrideValue( MaxModes, 0.0 ),
			RatedCOP( MaxModes, 0.0 ),
			RatedAirVolFlowRate( MaxModes, 0.0 ),
			RatedAirVolFlowRateEMSOverrideON( MaxModes, false ),
			RatedAirVolFlowRateEMSOverrideValue( MaxModes, 0.0 ),
			FanPowerPerEvapAirFlowRate( MaxModes, 0.0 ),
			RatedAirMassFlowRate( MaxModes, 0.0 ),
			BypassedFlowFrac( MaxModes, 0.0 ),
			RatedCBF( MaxModes, 0.0 ),
			AirInNode( 0 ),
			AirOutNode( 0 ),
			CCapFTemp( MaxModes, 0 ),
			CCapFTempErrorIndex( 0 ),
			TotCapTempModFacCurveType( MaxModes, 0 ),
			CCapFFlow( MaxModes, 0 ),
			CCapFFlowErrorIndex( 0 ),
			EIRFTemp( MaxModes, 0 ),
			EIRFTempErrorIndex( 0 ),
			EIRTempModFacCurveType( MaxModes, 0 ),
			EIRFFlow( MaxModes, 0 ),
			EIRFFlowErrorIndex( 0 ),
			PLFFPLR( MaxModes, 0 ),
			ReportCoolingCoilCrankcasePower( true ),
			CrankcaseHeaterCapacity( 0.0 ),
			CrankcaseHeaterPower( 0.0 ),
			MaxOATCrankcaseHeater( 0.0 ),
			CrankcaseHeaterConsumption( 0.0 ),
			BasinHeaterPowerFTempDiff( 0.0 ),
			BasinHeaterSetPointTemp( 0.0 ),
			CompanionUpstreamDXCoil( 0 ),
			FindCompanionUpStreamCoil( true ),
			CondenserInletNodeNum( MaxModes, 0 ),
			LowOutletTempIndex( 0 ),
			FullLoadOutAirTempLast( 0.0 ),
			FullLoadInletAirTempLast( 0.0 ),
			PrintLowOutTempMessage( false ),
			HeatingCoilPLFCurvePTR( 0 ),
			BasinHeaterSchedulePtr( 0 ),
			RatedTotCap2( 0.0 ),
			RatedSHR2( 0.0 ),
			RatedCOP2( 0.0 ),
			RatedAirVolFlowRate2( 0.0 ),
			RatedAirMassFlowRate2( 0.0 ),
			RatedCBF2( 0.0 ),
			CCapFTemp2( 0 ),
			EIRFTemp2( 0 ),
			RatedEIR2( 0.0 ),
			InternalStaticPressureDrop( 0.0 ),
			RateWithInternalStaticAndFanObject( false ),
			SupplyFanIndex( 0 ),
			RatedEIR( MaxModes, 0.0 ),
			InletAirMassFlowRate( 0.0 ),
			InletAirMassFlowRateMax( 0.0 ),
			InletAirTemp( 0.0 ),
			InletAirHumRat( 0.0 ),
			InletAirEnthalpy( 0.0 ),
			OutletAirTemp( 0.0 ),
			OutletAirHumRat( 0.0 ),
			OutletAirEnthalpy( 0.0 ),
			PartLoadRatio( 0.0 ),
			TotalCoolingEnergy( 0.0 ),
			SensCoolingEnergy( 0.0 ),
			LatCoolingEnergy( 0.0 ),
			TotalCoolingEnergyRate( 0.0 ),
			SensCoolingEnergyRate( 0.0 ),
			LatCoolingEnergyRate( 0.0 ),
			ElecCoolingConsumption( 0.0 ),
			ElecCoolingPower( 0.0 ),
			CoolingCoilRuntimeFraction( 0.0 ),
			TotalHeatingEnergy( 0.0 ),
			TotalHeatingEnergyRate( 0.0 ),
			ElecHeatingConsumption( 0.0 ),
			ElecHeatingPower( 0.0 ),
			HeatingCoilRuntimeFraction( 0.0 ),
			DefrostStrategy( 0 ),
			DefrostControl( 0 ),
			EIRFPLR( 0 ),
			DefrostEIRFT( 0 ),
			RegionNum( 0 ),
			MinOATCompressor( 0.0 ),
			OATempCompressorOn( 0.0 ),
			MaxOATCompressor( 0.0 ),
			MaxOATDefrost( 0.0 ),
			DefrostTime( 0.0 ),
			DefrostCapacity( 0.0 ),
			HPCompressorRuntime( 0.0 ),
			HPCompressorRuntimeLast( 0.0 ),
			TimeLeftToDefrost( 0.0 ),
			DefrostPower( 0.0 ),
			DefrostConsumption( 0.0 ),
			HeatingPerformanceOATType( DryBulbIndicator ),
			HPCoilIsInCoilSystemHeatingDX( false ),
			OATempCompressorOnOffBlank( false ),
			Twet_Rated( MaxModes, 0.0 ),
			Gamma_Rated( MaxModes, 0.0 ),
			MaxONOFFCyclesperHour( MaxModes, 0.0 ),
			LatentCapacityTimeConstant( MaxModes, 0.0 ),
			CondenserType( MaxModes, AirCooled ),
			ReportEvapCondVars( false ),
			EvapCondEffect( MaxModes, 0.0 ),
			CondInletTemp( 0.0 ),
			EvapCondAirFlow( MaxModes, 0.0 ),
			EvapCondPumpElecNomPower( MaxModes, 0.0 ),
			EvapCondPumpElecPower( 0.0 ),
			EvapCondPumpElecConsumption( 0.0 ),
			EvapWaterConsumpRate( 0.0 ),
			EvapWaterConsump( 0.0 ),
			EvapCondAirFlow2( 0.0 ),
			EvapCondEffect2( 0.0 ),
			EvapCondPumpElecNomPower2( 0.0 ),
			BasinHeaterPower( 0.0 ),
			BasinHeaterConsumption( 0.0 ),
			NumCapacityStages( 1 ),
			NumDehumidModes( 0 ),
			CoilPerformanceType( MaxModes ),
			CoilPerformanceType_Num( MaxModes, 0 ),
			CoilPerformanceName( MaxModes ),
			CoolingCoilStg2RuntimeFrac( 0.0 ),
			DehumidificationMode( 0 ),
			WaterInNode( 0 ),
			WaterOutNode( 0 ),
			HCOPFTemp( 0 ),
			HCOPFTempErrorIndex( 0 ),
			HCOPFTempCurveType( 0 ),
			HCOPFAirFlow( 0 ),
			HCOPFAirFlowErrorIndex( 0 ),
			HCOPFWaterFlow( 0 ),
			HCOPFWaterFlowErrorIndex( 0 ),
			HCapFTemp( 0 ),
			HCapFTempErrorIndex( 0 ),
			HCapFTempCurveType( 0 ),
			HCapFAirFlow( 0 ),
			HCapFAirFlowErrorIndex( 0 ),
			HCapFWaterFlow( 0 ),
			HCapFWaterFlowErrorIndex( 0 ),
			InletAirTemperatureType( 0 ),
			RatedInletDBTemp( 0.0 ),
			RatedInletWBTemp( 0.0 ),
			RatedInletWaterTemp( 0.0 ),
			HPWHCondPumpElecNomPower( 0.0 ),
			HPWHCondPumpFracToWater( 0.0 ),
			RatedHPWHCondWaterFlow( 0.0 ),
			ElecWaterHeatingPower( 0.0 ),
			ElecWaterHeatingConsumption( 0.0 ),
			FanPowerIncludedInCOP( true ),
			CondPumpHeatInCapacity( false ),
			CondPumpPowerInCOP( false ),
			LowTempLast( 0.0 ),
			HighTempLast( 0.0 ),
			ErrIndex1( 0 ),
			ErrIndex2( 0 ),
			ErrIndex3( 0 ),
			ErrIndex4( 0 ),
			LowAmbErrIndex( 0 ),
			HighAmbErrIndex( 0 ),
			PLFErrIndex( 0 ),
			PLRErrIndex( 0 ),
			PrintLowAmbMessage( false ),
			PrintHighAmbMessage( false ),
			EvapWaterSupplyMode( WaterSupplyFromMains ),
			EvapWaterSupTankID( 0 ),
			EvapWaterTankDemandARRID( 0 ),
			CondensateCollectMode( CondensateDiscarded ),
			CondensateTankID( 0 ),
			CondensateTankSupplyARRID( 0 ),
			CondensateVdot( 0.0 ),
			CondensateVol( 0.0 ),
			CurrentEndTimeLast( 0.0 ),
			TimeStepSysLast( 0.0 ),
			FuelType( 0 ),
			NumOfSpeeds( 0 ),
			PLRImpact( false ),
			LatentImpact( false ),
			CoolingCoilPresent( true ),
			HeatingCoilPresent( true ),
			ISHundredPercentDOASDXCoil( false ),
			SHRFTemp( MaxModes, 0 ),
			SHRFTempErrorIndex( 0 ),
			SHRFTempCurveType( MaxModes, 0 ),
			SHRFFlow( MaxModes, 0 ),
			SHRFFlowErrorIndex( 0 ),
			SHRFTemp2( 0 ),
			SHRFFlow2( 0 ),
			SHRFTempCurveType2( 0 ),
			UserSHRCurveExists( false ),
			ASHRAE127StdRprt( false ),
			SecZonePtr( 0 ),
			SecCoilSHRFT( 0 ),
			SecCoilSHRFF( 0 ),
			SecCoilAirFlow( 0.0 ),
			SecCoilAirFlowScalingFactor( 1.0 ),
			SecCoilRatedSHR( 1.0 ),
			SecCoilSHR( 1.0 ),
			SecZoneAirNodeNum( 0 ),
			EvapInletWetBulb( 0.0 ),
			SecCoilSensibleHeatGainRate( 0.0 ),
			SecCoilTotalHeatRemovalRate( 0.0 ),
			SecCoilSensibleHeatRemovalRate( 0.0 ),
			SecCoilLatentHeatRemovalRate( 0.0 ),
			IsSecondaryDXCoilInZone( false ),
			CompressorPartLoadRatio( 0.0 ),
			//MSSecCoilSHRFT( 0 ),
			//MSSecCoilSHRFF( 0 ),
			//MSSecCoilAirFlow( 0.0 ),
			//MSSecCoilAirFlowScalingFactor( 0.0 ),
			//MSSecCoilRatedSHR( 0.0 )
			MSSpeedNumLS( 1 ),
			MSSpeedNumHS( 2 ),
			MSSpeedRatio( 0.0 ),
			MSCycRatio( 0.0 )


		{}

		// Member Constructor
		DXCoilData(
			std::string const & Name, // Name of the DX Coil
			std::string const & DXCoilType, // type of coil
			int const DXCoilType_Num, // Integer equivalent to DXCoilType
			std::string const & Schedule, // WaterCoil Operation Schedule
			int const SchedPtr, // Pointer to the correct schedule
			Array1< Real64 > const & RatedTotCap, // Gross total cooling capacity at rated conditions [watts]
			Real64 const HeatSizeRatio, // heat pump heating to cooling sizing ratio when autosized
			Array1_bool const & RatedTotCapEMSOverrideOn, // if true, then EMS is calling to override rated total capacity
			Array1< Real64 > const & RatedTotCapEMSOverrideValue, // value to use for EMS override
			Array1< Real64 > const & RatedSHR, // Sensible heat ratio (sens cap/total cap) at rated conditions
			Array1_bool const & RatedSHREMSOverrideOn, // if true, then EMS is calling to override Sensible heat ratio
			Array1< Real64 > const & RatedSHREMSOverrideValue, // value to use for EMS override forSensible heat ratio
			Array1< Real64 > const & RatedCOP, // Coefficient of performance at rated conditions
			Array1< Real64 > const & RatedAirVolFlowRate, // Air volume flow rate through coil at rated conditions [m3/s]
			Array1_bool const & RatedAirVolFlowRateEMSOverrideON, // if true, then EMS is calling to override Air volume flow rate
			Array1< Real64 > const & RatedAirVolFlowRateEMSOverrideValue, // value to use for EMS override Air volume flow rate
			Array1< Real64 > const & FanPowerPerEvapAirFlowRate, // Fan Power Per Air volume flow rate through the
			Array1< Real64 > const & RatedAirMassFlowRate, // Air mass flow rate through coil at rated conditions [kg/s]
			Array1< Real64 > const & BypassedFlowFrac, // Fraction of air flow bypassed around coil
			Array1< Real64 > const & RatedCBF, // rated coil bypass factor, determined using RatedTotCap and RatedSHR
			int const AirInNode, // Air inlet node number
			int const AirOutNode, // Air outlet node number
			Array1_int const & CCapFTemp, // index of total cooling capacity modifier curve
			int const CCapFTempErrorIndex, // Used for warning messages when output of CCapFTemp is negative
			Array1_int const & TotCapTempModFacCurveType, // type of curve for CCapFTemp (cubic,quadratic,bi-quadratic)
			Array1_int const & CCapFFlow, // index of total cooling capacity modifier curve
			int const CCapFFlowErrorIndex, // Used for warning messages when output of CCapFFlow is negative
			Array1_int const & EIRFTemp, // index of energy input ratio modifier curve
			int const EIRFTempErrorIndex, // Used for warning messages when output of EIRFTemp is negative
			Array1_int const & EIRTempModFacCurveType, // type of curve for EIRFTemp (cubic,quadratic,bi-quadratic)
			Array1_int const & EIRFFlow, // index of energy input ratio modifier curve
			int const EIRFFlowErrorIndex, // Used for warning messages when output of EIRFFlow is negative
			Array1_int const & PLFFPLR, // index of part-load factor vs part-load ratio curve
			bool const ReportCoolingCoilCrankcasePower, // logical determines if the cooling coil crankcase heater power is reported
			Real64 const CrankcaseHeaterCapacity, // total crankcase heater capacity [W]
			Real64 const CrankcaseHeaterPower, // report variable for average crankcase heater power [W]
			Real64 const MaxOATCrankcaseHeater, // maximum OAT for crankcase heater operation [C]
			Real64 const CrankcaseHeaterConsumption, // report variable for total crankcase heater energy consumption [J]
			Real64 const BasinHeaterPowerFTempDiff, // Basin heater capacity per degree C below setpoint (W/C)
			Real64 const BasinHeaterSetPointTemp, // setpoint temperature for basin heater operation (C)
			int const CompanionUpstreamDXCoil, // index number of the DX coil that is "upstream" of this DX coil. Currently used for
			bool const FindCompanionUpStreamCoil, // Flag to get the companion coil in Init.
			Array1_int const & CondenserInletNodeNum, // Node number of outdoor condenser(s) (actually an evaporator for heating coils)
			int const LowOutletTempIndex, // used for low outlet temperature warnings
			Real64 const FullLoadOutAirTempLast, // used for low outlet temperature warnings
			Real64 const FullLoadInletAirTempLast, // used for low outlet temperature warnings
			bool const PrintLowOutTempMessage, // used to print warning message for low outlet air dry-bulb conditions
			std::string const & LowOutTempBuffer1, // holds warning message until next iteration (only prints 1 message/iteration)
			std::string const & LowOutTempBuffer2, // holds warning message until next iteration (only prints 1 message/iteration)
			int const HeatingCoilPLFCurvePTR, // PLF curve index to gas or electric heating coil (used in latent degradation model)
			int const BasinHeaterSchedulePtr, // Pointer to basin heater schedule
			Real64 const RatedTotCap2, // Gross total cooling capacity at rated conditions, low speed [watts]
			Real64 const RatedSHR2, // Sensible heat ratio (sens cap/total cap) at rated conditions, low speed
			Real64 const RatedCOP2, // Coefficient of performance at rated conditions, low speed
			Real64 const RatedAirVolFlowRate2, // Air volume flow rate through unit at rated conditions, low speed [m3/s]
			Real64 const RatedAirMassFlowRate2, // Air mass flow rate through unit at rated conditions, low speed [kg/s]
			Real64 const RatedCBF2, // rated coil bypass factor (low speed), determined using RatedTotCap2 and RatedSHR2
			int const CCapFTemp2, // index of total cooling capacity modifier curve (low speed)
			int const EIRFTemp2, // index of energy input ratio modifier curve (low speed)
			Real64 const RatedEIR2, // rated energy input ratio (low speed, inverse of COP2)
			Real64 const InternalStaticPressureDrop, // for rating VAV system
			bool const RateWithInternalStaticAndFanObject,
			int const SupplyFanIndex,
			std::string const & SupplyFanName,
			std::string const & CoilSystemName,
			Array1< Real64 > const & RatedEIR, // rated energy input ratio (inverse of COP)
			Real64 const InletAirMassFlowRate,
			Real64 const InletAirMassFlowRateMax,
			Real64 const InletAirTemp,
			Real64 const InletAirHumRat,
			Real64 const InletAirEnthalpy,
			Real64 const OutletAirTemp,
			Real64 const OutletAirHumRat,
			Real64 const OutletAirEnthalpy,
			Real64 const PartLoadRatio, // Ratio of actual sensible cooling load to steady-state sensible cooling capacity
			Real64 const TotalCoolingEnergy,
			Real64 const SensCoolingEnergy,
			Real64 const LatCoolingEnergy,
			Real64 const TotalCoolingEnergyRate,
			Real64 const SensCoolingEnergyRate,
			Real64 const LatCoolingEnergyRate,
			Real64 const ElecCoolingConsumption,
			Real64 const ElecCoolingPower,
			Real64 const CoolingCoilRuntimeFraction, // Run time fraction of the DX cooling unit
			Real64 const TotalHeatingEnergy,
			Real64 const TotalHeatingEnergyRate,
			Real64 const ElecHeatingConsumption,
			Real64 const ElecHeatingPower,
			Real64 const HeatingCoilRuntimeFraction, // Run time fraction of the DX heating unit
			int const DefrostStrategy, // defrost strategy; 1=reverse-cycle, 2=resistive
			int const DefrostControl, // defrost control; 1=timed, 2=on-demand
			int const EIRFPLR, // index of energy input ratio vs part-load ratio curve
			int const DefrostEIRFT, // index of defrost mode total cooling capacity for reverse cycle heat pump
			int const RegionNum, // Region number for calculating HSPF of single speed DX heating coil
			Real64 const MinOATCompressor, // Minimum OAT for heat pump compressor operation
			Real64 const OATempCompressorOn, // The outdoor tempearture when the compressor is automatically turned back on,
			Real64 const MaxOATCompressor, // Maximum OAT for VRF heat pump compressor operation
			Real64 const MaxOATDefrost, // Maximum OAT for defrost operation
			Real64 const DefrostTime, // Defrost time period in hours
			Real64 const DefrostCapacity, // Resistive defrost to nominal capacity (at 21.11C/8.33C) ratio
			Real64 const HPCompressorRuntime, // keep track of compressor runtime
			Real64 const HPCompressorRuntimeLast, // keep track of last time step compressor runtime (if simulation downshifts)
			Real64 const TimeLeftToDefrost, // keep track of time left to defrost heat pump
			Real64 const DefrostPower, // power used during defrost
			Real64 const DefrostConsumption, // energy used during defrost
			int const HeatingPerformanceOATType, // Heating performance curve OAT type (1-wetbulb, 2-drybulb)
			bool const HPCoilIsInCoilSystemHeatingDX,
			bool const OATempCompressorOnOffBlank,
			Array1< Real64 > const & Twet_Rated, // Nominal time for condensate to begin leaving the coil's
			Array1< Real64 > const & Gamma_Rated, // Initial moisture evaporation rate divided by steady-state
			Array1< Real64 > const & MaxONOFFCyclesperHour, // Maximum ON/OFF cycles per hour for the compressor (cycles/hour)
			Array1< Real64 > const & LatentCapacityTimeConstant, // Time constant for latent capacity to reach steady state
			Array1_int const & CondenserType, // Type of condenser for DX cooling coil: AIR COOLED or EVAP COOLED
			bool const ReportEvapCondVars, // true if any performance mode includes an evap condenser
			Array1< Real64 > const & EvapCondEffect, // effectiveness of the evaporatively cooled condenser
			Real64 const CondInletTemp, // Evap condenser inlet temperature [C], report variable
			Array1< Real64 > const & EvapCondAirFlow, // Air flow rate through the evap condenser at high speed,
			Array1< Real64 > const & EvapCondPumpElecNomPower, // Nominal power input to the evap condenser water circulation pump
			Real64 const EvapCondPumpElecPower, // Average power consumed by the evap condenser water circulation pump over
			Real64 const EvapCondPumpElecConsumption, // Electric energy consumed by the evap condenser water circulation pump [J]
			Real64 const EvapWaterConsumpRate, // Evap condenser water consumption rate [m3/s]
			Real64 const EvapWaterConsump, // Evap condenser water consumption [m3]
			Real64 const EvapCondAirFlow2, // Air flow rate through the evap condenser at low speed, for water use calcs [m3/s]
			Real64 const EvapCondEffect2, // effectiveness of the evaporatively cooled condenser at low speed (-)
			Real64 const EvapCondPumpElecNomPower2, // Nominal power input to the evap condenser water circulation pump at low speed [W]
			Real64 const BasinHeaterPower, // Basin heater power (W)
			Real64 const BasinHeaterConsumption, // Basin heater energy consumption (J)
			int const NumCapacityStages, // number of capacity stages, up to MaxCapacityStages for Multimode DX coil,
			int const NumDehumidModes, // number of enhanced dehumidification modes, up to MaxDehumidModes for Multimode DX coil,
			Array1_string const & CoilPerformanceType, // Coil Performance object type
			Array1_int const & CoilPerformanceType_Num, // Coil Performance object type number
			Array1_string const & CoilPerformanceName, // Coil Performance object names
			Real64 const CoolingCoilStg2RuntimeFrac, // Run time fraction of stage 2
			int const DehumidificationMode, // Dehumidification mode for multimode coil,
			int const WaterInNode, // Condenser water inlet node number for HPWH DX coil
			int const WaterOutNode, // Condenser water outlet node number for HPWH DX coil
			int const HCOPFTemp, // COP as a function of temperature curve index
			int const HCOPFTempErrorIndex, // Used for warning messages when output of HCOPFTemp is negative
			int const HCOPFTempCurveType, // COP as a function of temperature curve type
			int const HCOPFAirFlow, // COP as a function of air flow rate ratio curve index
			int const HCOPFAirFlowErrorIndex, // Used for warning messages when output of HCOPFAirFlow is negative
			int const HCOPFWaterFlow, // COP as a function of water flow rate ratio curve index
			int const HCOPFWaterFlowErrorIndex, // Used for warning messages when output of HCOPFWaterFlow is negative
			int const HCapFTemp, // Heating capacity as a function of temperature curve index
			int const HCapFTempErrorIndex, // Used for warning messages when output of HCapFTemp is negative
			int const HCapFTempCurveType, // Heating capacity as a function of temperature curve type
			int const HCapFAirFlow, // Heating capacity as a function of air flow rate ratio curve index
			int const HCapFAirFlowErrorIndex, // Used for warning messages when output of HCapFAirFlow is negative
			int const HCapFWaterFlow, // Heating capacity as a function of water flow rate ratio curve index
			int const HCapFWaterFlowErrorIndex, // Used for warning messages when output of HCapFWaterFlow is negative
			int const InletAirTemperatureType, // Specifies to use either air wet-bulb or dry-bulb temp for curve objects
			Real64 const RatedInletDBTemp, // Rated inlet air dry-bulb temperature [C]
			Real64 const RatedInletWBTemp, // Rated inlet air wet-bulb temperature [C]
			Real64 const RatedInletWaterTemp, // Rated condenser water inlet temperature [C]
			Real64 const HPWHCondPumpElecNomPower, // Nominal power input to the condenser water circulation pump [W]
			Real64 const HPWHCondPumpFracToWater, // Nominal power fraction to water for the condenser water circulation pump
			Real64 const RatedHPWHCondWaterFlow, // Rated water flow rate through the condenser of the HPWH DX coil [m3/s]
			Real64 const ElecWaterHeatingPower, // Total electric power consumed by compressor and condenser pump [W]
			Real64 const ElecWaterHeatingConsumption, // Total electric consumption by compressor and condenser pump [J]
			bool const FanPowerIncludedInCOP, // Indicates that fan heat is included in heating capacity and COP
			bool const CondPumpHeatInCapacity, // Indicates that condenser pump heat is included in heating capacity
			bool const CondPumpPowerInCOP, // Indicates that condenser pump power is included in heating COP
			Real64 const LowTempLast, // low ambient temp entering condenser when warning message occurred
			Real64 const HighTempLast, // high ambient temp entering condenser when warning message occurred
			int const ErrIndex1, // index/pointer to recurring error structure for Air volume flow rate per watt of
			int const ErrIndex2, // index/pointer to recurring error structure for PLF curve values must be >= 0.7. error
			int const ErrIndex3, // index/pointer to recurring error structure for DX cooling coil runtime fraction > 1.0 warning
			int const ErrIndex4, // index/pointer to recurring error structure for DX heating coil runtime fraction > 1.0 warning
			int const LowAmbErrIndex, // index/pointer to recurring error structure for low ambient temp entering condenser
			int const HighAmbErrIndex, // index/pointer to recurring error structure for high ambient temp entering condenser
			int const PLFErrIndex, // index/pointer to recurring error structure for PLF <> 1 at speed 1 for a multiple speed coil
			int const PLRErrIndex, // index/pointer to recurring error structure for PLR < .7
			bool const PrintLowAmbMessage, // used to print warning message for low ambient conditions
			std::string const & LowAmbBuffer1, // holds warning message until next iteration (only prints 1 message/iteration)
			std::string const & LowAmbBuffer2, // holds warning message until next iteration (only prints 1 message/iteration)
			bool const PrintHighAmbMessage, // used to print warning message for high ambient conditions
			std::string const & HighAmbBuffer1, // holds warning message until next iteration (only prints 1 message/iteration)
			std::string const & HighAmbBuffer2, // holds warning message until next iteration (only prints 1 message/iteration)
			int const EvapWaterSupplyMode, // where does water come from
			std::string const & EvapWaterSupplyName, // name of water source e.g. water storage tank
			int const EvapWaterSupTankID,
			int const EvapWaterTankDemandARRID,
			int const CondensateCollectMode, // where does water come from
			std::string const & CondensateCollectName, // name of water source e.g. water storage tank
			int const CondensateTankID,
			int const CondensateTankSupplyARRID,
			Real64 const CondensateVdot, // rate of water condensation from air stream [m3/s]
			Real64 const CondensateVol, // amount of water condensed from air stream [m3]
			Real64 const CurrentEndTimeLast, // end time of time step for last simulation time step
			Real64 const TimeStepSysLast, // last system time step (used to check for downshifting)
			int const FuelType, // Fuel type
			int const NumOfSpeeds, // Number of speeds
			bool const PLRImpact, // Part load fraction applied to Speed Number > 1
			bool const LatentImpact, // Latent degradation applied to Speed Number > 1
			Array1_int const & MSErrIndex, // index flag for num speeds/recurring messages
			Array1< Real64 > const & MSRatedTotCap, // Rated cooling capacity for MS heat pump [W]
			Array1< Real64 > const & MSRatedSHR, // Rated SHR for MS heat pump [dimensionless]
			Array1< Real64 > const & MSRatedCOP, // Rated COP for MS heat pump [dimensionless]
			Array1< Real64 > const & MSRatedAirVolFlowRate, // Air volume flow rate through unit at rated conditions [m3/s]
			Array1< Real64 > const & MSRatedAirMassFlowRate, // Air mass flow rate through unit at rated conditions [m3/s]
			Array1< Real64 > const & MSRatedCBF, // rated coil bypass factor
			Array1_int const & MSCCapFTemp, // index of total cooling capacity modifier curve
			Array1_int const & MSCCapFFlow, // index of total cooling capacity modifier curve
			Array1_int const & MSEIRFTemp, // index of energy input ratio modifier curve as a function of temperature
			Array1_int const & MSEIRFFlow, // index of energy input ratio modifier curve as a function of flow fraction
			Array1_int const & MSPLFFPLR, // index of part load factor as a function of part load ratio
			Array1_int const & MSWasteHeat, // index of waste heat as a function of temperature
			Array1< Real64 > const & MSWasteHeatFrac, // Waste heat fraction
			Array1< Real64 > const & MSEvapCondEffect, // effectiveness of the evaporatively cooled condenser
			Array1< Real64 > const & MSEvapCondAirFlow, // Air flow rate through the evap condenser for water use calcs [m3/s]
			Array1< Real64 > const & MSEvapCondPumpElecNomPower, // Nominal power input to the evap condenser
			Array1_int const & MSTotCapTempModFacCurveType, // type of curve for CCapFTemp (cubic,quadratic,bi-quadratic)
			Array1_int const & MSEIRTempModFacCurveType, // type of curve for EIRFTemp (cubic,quadratic,bi-quadratic)
			Array1< Real64 > const & MSTwet_Rated, // Nominal time for condensate to begin leaving the coil's
			Array1< Real64 > const & MSGamma_Rated, // Initial moisture evaporation rate divided by steady-state
			Array1< Real64 > const & MSMaxONOFFCyclesperHour, // Maximum ON/OFF cycles per hour for the compressor (cycles/hour)
			Array1< Real64 > const & MSLatentCapacityTimeConstant, // Time constant for latent capacity to reach steady state
			Array1< Real64 > const & MSFanPowerPerEvapAirFlowRate,
			Real64 const FuelUsed, // Energy used, in addition to electricity [W]
			Real64 const FuelConsumed, // Energy consumed, in addition to electricity [J]
			bool const CoolingCoilPresent, // FALSE if coil not present
			bool const HeatingCoilPresent, // FALSE if coil not present
			bool const ISHundredPercentDOASDXCoil, // FALSE if coil is regular dx coil
			Array1_int const & SHRFTemp, // index of sensible heat ratio modifier curve
			int const SHRFTempErrorIndex, // Used for warning messages when output of SHRFTemp is negative
			Array1_int const & SHRFTempCurveType, // type of curve for SHRFTemp (cubic,quadratic,bi-quadratic)
			Array1_int const & SHRFFlow, // index of sensible heat ratio modifier curve
			int const SHRFFlowErrorIndex, // Used for warning messages when output of SHRFFlow is negative
			int const SHRFTemp2, // index of sensible heat ratio modifier curve
			int const SHRFFlow2, // index of sensible heat ratio modifier curve
			int const SHRFTempCurveType2, // type of curve for SHRFTemp (cubic,quadratic,bi-quadratic)
			bool const UserSHRCurveExists, // TRUE if user specified SHR modifier curve exists
			bool const ASHRAE127StdRprt, // TRUE if user wishes to report ASHRAE 127 standard ratings
			int const SecZonePtr, // index to the zone where the secondary coil is placed
			int const SecCoilSHRFT, // index to the secondary coil sensible heat ratio temperature modifier curve
			int const SecCoilSHRFF, // index to the secondary coil sensible heat ratio flor fraction modifier curve
			Real64 const SecCoilAirFlow, // secondary coil air flow rate
			Real64 const SecCoilAirFlowScalingFactor, // secondary coil air flow rate autosize scaling factor
			Real64 const SecCoilRatedSHR, // secondary coil nominal or rated sensible heat ratio
			Real64 const SecCoilSHR, // secondary coil current sensible heat ratio
			int const SecZoneAirNodeNum, // secondary zone air node number
			Real64 const EvapInletWetBulb, // secondary DX coil inlet wet bulb temperature (zone air node wet bulb temp.)
			Real64 const SecCoilSensibleHeatGainRate, // secondary zone sensible heat gain rate [W]
			Real64 const SecCoilTotalHeatRemovalRate, // secondary zone total heat removal rate [W]
			Real64 const SecCoilSensibleHeatRemovalRate, // secondary zone sensible heat removal rate [W]
			Real64 const SecCoilLatentHeatRemovalRate, // secondary zone latent heat removal rate [W]
			bool const IsSecondaryDXCoilInZone, // true means secondary dx coil is zone instead of outside
			Real64 const CompressorPartLoadRatio, // compressor part load ratio of the primary DX coil
			Array1_int const & MSSecCoilSHRFT, // index to the multi speed secondary coil sensible heat ratio temperature modifier curve
			Array1_int const & MSSecCoilSHRFF, //  index to the multi speed secondary coil sensible heat ratio flow fraction modifier curve
			Array1< Real64 > const & MSSecCoilAirFlow, // multispeed secondary coil air flow rate
			Array1< Real64 > const & MSSecCoilAirFlowScalingFactor, //multispeed secondary coil air flow rate autosize scaling factor
			Array1< Real64 > const & MSSecCoilRatedSHR, // multispeed secondary coil nominal or rated sensible heat ratio
			int const MSSpeedNumLS, // current low speed number of multspeed HP
			int const MSSpeedNumHS, // current high speed number of multspeed HP
			Real64 const MSSpeedRatio, // current speed ratio of multspeed HP
			Real64 const MSCycRatio // current cycling ratio of multspeed HP
		) :
			Name( Name ),
			DXCoilType( DXCoilType ),
			DXCoilType_Num( DXCoilType_Num ),
			Schedule( Schedule ),
			SchedPtr( SchedPtr ),
			RatedTotCap( MaxModes, RatedTotCap ),
			HeatSizeRatio( HeatSizeRatio ),
			RatedTotCapEMSOverrideOn( MaxModes, RatedTotCapEMSOverrideOn ),
			RatedTotCapEMSOverrideValue( MaxModes, RatedTotCapEMSOverrideValue ),
			RatedSHR( MaxModes, RatedSHR ),
			RatedSHREMSOverrideOn( MaxModes, RatedSHREMSOverrideOn ),
			RatedSHREMSOverrideValue( MaxModes, RatedSHREMSOverrideValue ),
			RatedCOP( MaxModes, RatedCOP ),
			RatedAirVolFlowRate( MaxModes, RatedAirVolFlowRate ),
			RatedAirVolFlowRateEMSOverrideON( MaxModes, RatedAirVolFlowRateEMSOverrideON ),
			RatedAirVolFlowRateEMSOverrideValue( MaxModes, RatedAirVolFlowRateEMSOverrideValue ),
			FanPowerPerEvapAirFlowRate( MaxModes, FanPowerPerEvapAirFlowRate ),
			RatedAirMassFlowRate( MaxModes, RatedAirMassFlowRate ),
			BypassedFlowFrac( MaxModes, BypassedFlowFrac ),
			RatedCBF( MaxModes, RatedCBF ),
			AirInNode( AirInNode ),
			AirOutNode( AirOutNode ),
			CCapFTemp( MaxModes, CCapFTemp ),
			CCapFTempErrorIndex( CCapFTempErrorIndex ),
			TotCapTempModFacCurveType( MaxModes, TotCapTempModFacCurveType ),
			CCapFFlow( MaxModes, CCapFFlow ),
			CCapFFlowErrorIndex( CCapFFlowErrorIndex ),
			EIRFTemp( MaxModes, EIRFTemp ),
			EIRFTempErrorIndex( EIRFTempErrorIndex ),
			EIRTempModFacCurveType( MaxModes, EIRTempModFacCurveType ),
			EIRFFlow( MaxModes, EIRFFlow ),
			EIRFFlowErrorIndex( EIRFFlowErrorIndex ),
			PLFFPLR( MaxModes, PLFFPLR ),
			ReportCoolingCoilCrankcasePower( ReportCoolingCoilCrankcasePower ),
			CrankcaseHeaterCapacity( CrankcaseHeaterCapacity ),
			CrankcaseHeaterPower( CrankcaseHeaterPower ),
			MaxOATCrankcaseHeater( MaxOATCrankcaseHeater ),
			CrankcaseHeaterConsumption( CrankcaseHeaterConsumption ),
			BasinHeaterPowerFTempDiff( BasinHeaterPowerFTempDiff ),
			BasinHeaterSetPointTemp( BasinHeaterSetPointTemp ),
			CompanionUpstreamDXCoil( CompanionUpstreamDXCoil ),
			FindCompanionUpStreamCoil( FindCompanionUpStreamCoil ),
			CondenserInletNodeNum( MaxModes, CondenserInletNodeNum ),
			LowOutletTempIndex( LowOutletTempIndex ),
			FullLoadOutAirTempLast( FullLoadOutAirTempLast ),
			FullLoadInletAirTempLast( FullLoadInletAirTempLast ),
			PrintLowOutTempMessage( PrintLowOutTempMessage ),
			LowOutTempBuffer1( LowOutTempBuffer1 ),
			LowOutTempBuffer2( LowOutTempBuffer2 ),
			HeatingCoilPLFCurvePTR( HeatingCoilPLFCurvePTR ),
			BasinHeaterSchedulePtr( BasinHeaterSchedulePtr ),
			RatedTotCap2( RatedTotCap2 ),
			RatedSHR2( RatedSHR2 ),
			RatedCOP2( RatedCOP2 ),
			RatedAirVolFlowRate2( RatedAirVolFlowRate2 ),
			RatedAirMassFlowRate2( RatedAirMassFlowRate2 ),
			RatedCBF2( RatedCBF2 ),
			CCapFTemp2( CCapFTemp2 ),
			EIRFTemp2( EIRFTemp2 ),
			RatedEIR2( RatedEIR2 ),
			InternalStaticPressureDrop( InternalStaticPressureDrop ),
			RateWithInternalStaticAndFanObject( RateWithInternalStaticAndFanObject ),
			SupplyFanIndex( SupplyFanIndex ),
			SupplyFanName( SupplyFanName ),
			CoilSystemName( CoilSystemName ),
			RatedEIR( MaxModes, RatedEIR ),
			InletAirMassFlowRate( InletAirMassFlowRate ),
			InletAirMassFlowRateMax( InletAirMassFlowRateMax ),
			InletAirTemp( InletAirTemp ),
			InletAirHumRat( InletAirHumRat ),
			InletAirEnthalpy( InletAirEnthalpy ),
			OutletAirTemp( OutletAirTemp ),
			OutletAirHumRat( OutletAirHumRat ),
			OutletAirEnthalpy( OutletAirEnthalpy ),
			PartLoadRatio( PartLoadRatio ),
			TotalCoolingEnergy( TotalCoolingEnergy ),
			SensCoolingEnergy( SensCoolingEnergy ),
			LatCoolingEnergy( LatCoolingEnergy ),
			TotalCoolingEnergyRate( TotalCoolingEnergyRate ),
			SensCoolingEnergyRate( SensCoolingEnergyRate ),
			LatCoolingEnergyRate( LatCoolingEnergyRate ),
			ElecCoolingConsumption( ElecCoolingConsumption ),
			ElecCoolingPower( ElecCoolingPower ),
			CoolingCoilRuntimeFraction( CoolingCoilRuntimeFraction ),
			TotalHeatingEnergy( TotalHeatingEnergy ),
			TotalHeatingEnergyRate( TotalHeatingEnergyRate ),
			ElecHeatingConsumption( ElecHeatingConsumption ),
			ElecHeatingPower( ElecHeatingPower ),
			HeatingCoilRuntimeFraction( HeatingCoilRuntimeFraction ),
			DefrostStrategy( DefrostStrategy ),
			DefrostControl( DefrostControl ),
			EIRFPLR( EIRFPLR ),
			DefrostEIRFT( DefrostEIRFT ),
			RegionNum( RegionNum ),
			MinOATCompressor( MinOATCompressor ),
			OATempCompressorOn( OATempCompressorOn ),
			MaxOATCompressor( MaxOATCompressor ),
			MaxOATDefrost( MaxOATDefrost ),
			DefrostTime( DefrostTime ),
			DefrostCapacity( DefrostCapacity ),
			HPCompressorRuntime( HPCompressorRuntime ),
			HPCompressorRuntimeLast( HPCompressorRuntimeLast ),
			TimeLeftToDefrost( TimeLeftToDefrost ),
			DefrostPower( DefrostPower ),
			DefrostConsumption( DefrostConsumption ),
			HeatingPerformanceOATType( HeatingPerformanceOATType ),
			HPCoilIsInCoilSystemHeatingDX( HPCoilIsInCoilSystemHeatingDX ),
			OATempCompressorOnOffBlank( OATempCompressorOnOffBlank ),
			Twet_Rated( MaxModes, Twet_Rated ),
			Gamma_Rated( MaxModes, Gamma_Rated ),
			MaxONOFFCyclesperHour( MaxModes, MaxONOFFCyclesperHour ),
			LatentCapacityTimeConstant( MaxModes, LatentCapacityTimeConstant ),
			CondenserType( MaxModes, CondenserType ),
			ReportEvapCondVars( ReportEvapCondVars ),
			EvapCondEffect( MaxModes, EvapCondEffect ),
			CondInletTemp( CondInletTemp ),
			EvapCondAirFlow( MaxModes, EvapCondAirFlow ),
			EvapCondPumpElecNomPower( MaxModes, EvapCondPumpElecNomPower ),
			EvapCondPumpElecPower( EvapCondPumpElecPower ),
			EvapCondPumpElecConsumption( EvapCondPumpElecConsumption ),
			EvapWaterConsumpRate( EvapWaterConsumpRate ),
			EvapWaterConsump( EvapWaterConsump ),
			EvapCondAirFlow2( EvapCondAirFlow2 ),
			EvapCondEffect2( EvapCondEffect2 ),
			EvapCondPumpElecNomPower2( EvapCondPumpElecNomPower2 ),
			BasinHeaterPower( BasinHeaterPower ),
			BasinHeaterConsumption( BasinHeaterConsumption ),
			NumCapacityStages( NumCapacityStages ),
			NumDehumidModes( NumDehumidModes ),
			CoilPerformanceType( MaxModes, CoilPerformanceType ),
			CoilPerformanceType_Num( MaxModes, CoilPerformanceType_Num ),
			CoilPerformanceName( MaxModes, CoilPerformanceName ),
			CoolingCoilStg2RuntimeFrac( CoolingCoilStg2RuntimeFrac ),
			DehumidificationMode( DehumidificationMode ),
			WaterInNode( WaterInNode ),
			WaterOutNode( WaterOutNode ),
			HCOPFTemp( HCOPFTemp ),
			HCOPFTempErrorIndex( HCOPFTempErrorIndex ),
			HCOPFTempCurveType( HCOPFTempCurveType ),
			HCOPFAirFlow( HCOPFAirFlow ),
			HCOPFAirFlowErrorIndex( HCOPFAirFlowErrorIndex ),
			HCOPFWaterFlow( HCOPFWaterFlow ),
			HCOPFWaterFlowErrorIndex( HCOPFWaterFlowErrorIndex ),
			HCapFTemp( HCapFTemp ),
			HCapFTempErrorIndex( HCapFTempErrorIndex ),
			HCapFTempCurveType( HCapFTempCurveType ),
			HCapFAirFlow( HCapFAirFlow ),
			HCapFAirFlowErrorIndex( HCapFAirFlowErrorIndex ),
			HCapFWaterFlow( HCapFWaterFlow ),
			HCapFWaterFlowErrorIndex( HCapFWaterFlowErrorIndex ),
			InletAirTemperatureType( InletAirTemperatureType ),
			RatedInletDBTemp( RatedInletDBTemp ),
			RatedInletWBTemp( RatedInletWBTemp ),
			RatedInletWaterTemp( RatedInletWaterTemp ),
			HPWHCondPumpElecNomPower( HPWHCondPumpElecNomPower ),
			HPWHCondPumpFracToWater( HPWHCondPumpFracToWater ),
			RatedHPWHCondWaterFlow( RatedHPWHCondWaterFlow ),
			ElecWaterHeatingPower( ElecWaterHeatingPower ),
			ElecWaterHeatingConsumption( ElecWaterHeatingConsumption ),
			FanPowerIncludedInCOP( FanPowerIncludedInCOP ),
			CondPumpHeatInCapacity( CondPumpHeatInCapacity ),
			CondPumpPowerInCOP( CondPumpPowerInCOP ),
			LowTempLast( LowTempLast ),
			HighTempLast( HighTempLast ),
			ErrIndex1( ErrIndex1 ),
			ErrIndex2( ErrIndex2 ),
			ErrIndex3( ErrIndex3 ),
			ErrIndex4( ErrIndex4 ),
			LowAmbErrIndex( LowAmbErrIndex ),
			HighAmbErrIndex( HighAmbErrIndex ),
			PLFErrIndex( PLFErrIndex ),
			PLRErrIndex( PLRErrIndex ),
			PrintLowAmbMessage( PrintLowAmbMessage ),
			LowAmbBuffer1( LowAmbBuffer1 ),
			LowAmbBuffer2( LowAmbBuffer2 ),
			PrintHighAmbMessage( PrintHighAmbMessage ),
			HighAmbBuffer1( HighAmbBuffer1 ),
			HighAmbBuffer2( HighAmbBuffer2 ),
			EvapWaterSupplyMode( EvapWaterSupplyMode ),
			EvapWaterSupplyName( EvapWaterSupplyName ),
			EvapWaterSupTankID( EvapWaterSupTankID ),
			EvapWaterTankDemandARRID( EvapWaterTankDemandARRID ),
			CondensateCollectMode( CondensateCollectMode ),
			CondensateCollectName( CondensateCollectName ),
			CondensateTankID( CondensateTankID ),
			CondensateTankSupplyARRID( CondensateTankSupplyARRID ),
			CondensateVdot( CondensateVdot ),
			CondensateVol( CondensateVol ),
			CurrentEndTimeLast( CurrentEndTimeLast ),
			TimeStepSysLast( TimeStepSysLast ),
			FuelType( FuelType ),
			NumOfSpeeds( NumOfSpeeds ),
			PLRImpact( PLRImpact ),
			LatentImpact( LatentImpact ),
			MSErrIndex( MSErrIndex ),
			MSRatedTotCap( MSRatedTotCap ),
			MSRatedSHR( MSRatedSHR ),
			MSRatedCOP( MSRatedCOP ),
			MSRatedAirVolFlowRate( MSRatedAirVolFlowRate ),
			MSRatedAirMassFlowRate( MSRatedAirMassFlowRate ),
			MSRatedCBF( MSRatedCBF ),
			MSCCapFTemp( MSCCapFTemp ),
			MSCCapFFlow( MSCCapFFlow ),
			MSEIRFTemp( MSEIRFTemp ),
			MSEIRFFlow( MSEIRFFlow ),
			MSPLFFPLR( MSPLFFPLR ),
			MSWasteHeat( MSWasteHeat ),
			MSWasteHeatFrac( MSWasteHeatFrac ),
			MSEvapCondEffect( MSEvapCondEffect ),
			MSEvapCondAirFlow( MSEvapCondAirFlow ),
			MSEvapCondPumpElecNomPower( MSEvapCondPumpElecNomPower ),
			MSTotCapTempModFacCurveType( MSTotCapTempModFacCurveType ),
			MSEIRTempModFacCurveType( MSEIRTempModFacCurveType ),
			MSTwet_Rated( MSTwet_Rated ),
			MSGamma_Rated( MSGamma_Rated ),
			MSMaxONOFFCyclesperHour( MSMaxONOFFCyclesperHour ),
			MSLatentCapacityTimeConstant( MSLatentCapacityTimeConstant ),
			MSFanPowerPerEvapAirFlowRate( MSFanPowerPerEvapAirFlowRate ),
			FuelUsed( FuelUsed ),
			FuelConsumed( FuelConsumed ),
			CoolingCoilPresent( CoolingCoilPresent ),
			HeatingCoilPresent( HeatingCoilPresent ),
			ISHundredPercentDOASDXCoil( ISHundredPercentDOASDXCoil ),
			SHRFTemp( MaxModes, SHRFTemp ),
			SHRFTempErrorIndex( SHRFTempErrorIndex ),
			SHRFTempCurveType( MaxModes, SHRFTempCurveType ),
			SHRFFlow( MaxModes, SHRFFlow ),
			SHRFFlowErrorIndex( SHRFFlowErrorIndex ),
			SHRFTemp2( SHRFTemp2 ),
			SHRFFlow2( SHRFFlow2 ),
			SHRFTempCurveType2( SHRFTempCurveType2 ),
			UserSHRCurveExists( UserSHRCurveExists ),
			ASHRAE127StdRprt( ASHRAE127StdRprt ),
			SecZonePtr( SecZonePtr ),
			SecCoilSHRFT( SecCoilSHRFT ),
			SecCoilSHRFF( SecCoilSHRFF ),
			SecCoilAirFlow( SecCoilAirFlow ),
			SecCoilAirFlowScalingFactor( SecCoilAirFlowScalingFactor ),
			SecCoilRatedSHR( SecCoilRatedSHR ),
			SecCoilSHR( SecCoilSHR ),
			SecZoneAirNodeNum( SecZoneAirNodeNum ),
			EvapInletWetBulb( EvapInletWetBulb ),
			SecCoilSensibleHeatGainRate( SecCoilSensibleHeatGainRate ),
			SecCoilTotalHeatRemovalRate( SecCoilTotalHeatRemovalRate ),
			SecCoilSensibleHeatRemovalRate( SecCoilSensibleHeatRemovalRate ),
			SecCoilLatentHeatRemovalRate( SecCoilLatentHeatRemovalRate ),
			IsSecondaryDXCoilInZone( IsSecondaryDXCoilInZone ),
			CompressorPartLoadRatio( CompressorPartLoadRatio ),
			MSSecCoilSHRFT( MSSecCoilSHRFT ),
			MSSecCoilSHRFF( MSSecCoilSHRFF ),
			MSSecCoilAirFlow( MSSecCoilAirFlow ),
			MSSecCoilAirFlowScalingFactor( MSSecCoilAirFlowScalingFactor ),
			MSSecCoilRatedSHR( MSSecCoilRatedSHR ),
			MSSpeedNumLS( MSSpeedNumLS ),
			MSSpeedNumHS( MSSpeedNumHS ),
			MSSpeedRatio( MSSpeedRatio ),
			MSCycRatio( MSCycRatio )

		{}

	};

	struct PerfModeData
	{
		// Members
		Array1D< std::string > FieldNames;

		// Default Constructor
		PerfModeData () :
			FieldNames()
		{}

		// Member Constructor
		PerfModeData(
			Array1D< std::string > const & FieldNames // Name of the HeatingCoil numeric field descriptions
		) :
			FieldNames( FieldNames )
		{}
	};

	struct DXCoilNumericFieldData
	{
		// Members
		Array1D < PerfModeData > PerfMode; // Coil Performance object type

		// Default Constructor
		DXCoilNumericFieldData ():
			PerfMode( 0 )
		{}

		// Member Constructor
		DXCoilNumericFieldData(
			Array1_int const & EP_UNUSED( PerfMode ) // Name of the HeatingCoil numeric field descriptions
	) :
		PerfMode( 0 )
		{}
	};

	// Object Data
	extern Array1D< DXCoilData > DXCoil;
	extern Array1D< DXCoilNumericFieldData > DXCoilNumericFields;

	// Functions

	void
	SimDXCoil(
		std::string const & CompName, // name of the fan coil unit
		int const CompOp, // compressor operation; 1=on, 0=off
		bool const FirstHVACIteration, // True when first HVAC iteration
		int & CompIndex,
		int const FanOpMode, // allows parent object to control fan mode
		Optional< Real64 const > PartLoadRatio = _, // part load ratio (for single speed cycling unit)
		Optional< Real64 const > OnOffAFR = _, // ratio of compressor on airflow to compressor off airflow
		Optional< Real64 const > CoilCoolingHeatingPLRRatio = _, // used for cycling fan RH control
		Optional< Real64 const > MaxCap = _, // maximum cooling capacity of VRF terminal units
		Optional< Real64 const > CompCyclingRatio = _ // cycling ratio of VRF condenser connected to this TU
	);

	void
	SimDXCoilMultiSpeed(
		std::string const & CompName, // name of the fan coil unit
		Real64 const SpeedRatio, // = (CompressorSpeed - CompressorSpeedMin) /
		Real64 const CycRatio, // cycling part load ratio for variable speed
		int & CompIndex,
		Optional_int_const SpeedNum = _, // Speed number for multispeed cooling coil onlyn
		Optional_int_const FanOpMode = _, // Fan operation mode
		Optional_int_const CompOp = _ // Compressor on/off; 1=on, 0=off
	);

	void
	SimDXCoilMultiMode(
		std::string const & CompName, // name of the fan coil unit
		int const CompOp, // compressor operation; 1=on, 0=off !unused1208
		bool const FirstHVACIteration, // true if first hvac iteration
		Real64 const PartLoadRatio, // part load ratio
		int const DehumidMode, // dehumidification mode (0=normal, 1=enhanced)
		int & CompIndex,
		int const FanOpMode // allows parent object to control fan mode
	);

	void
	GetDXCoils();

	void
	InitDXCoil( int const DXCoilNum ); // number of the current DX coil unit being simulated

	void
	SizeDXCoil( int const DXCoilNum );

	void
	CalcHPWHDXCoil(
		int const DXCoilNum, // the number of the DX coil to be simulated
		Real64 const PartLoadRatio // sensible water heating load / full load sensible water heating capacity
	);

	void
	CalcDoe2DXCoil(
		int const DXCoilNum, // the number of the DX coil to be simulated
		int const CompOp, // compressor operation; 1=on, 0=off
		bool const FirstHVACIteration, // true if this is the first iteration of HVAC
		Real64 const PartLoadRatio, // sensible cooling load / full load sensible cooling capacity
		int const FanOpMode, // Allows parent object to control fan operation
		Optional_int_const PerfMode = _, // Performance mode for MultiMode DX coil; Always 1 for other coil types
		Optional< Real64 const > OnOffAirFlowRatio = _, // ratio of compressor on airflow to compressor off airflow
		Optional< Real64 const > CoolingHeatingPLR = _ // used for cycling fan RH control
	);

	void
	CalcVRFCoolingCoil(
		int const DXCoilNum, // the number of the DX coil to be simulated
		int const CompOp, // compressor operation; 1=on, 0=off
		bool const FirstHVACIteration, // true if this is the first iteration of HVAC
		Real64 const PartLoadRatio, // sensible cooling load / full load sensible cooling capacity
		int const FanOpMode, // Allows parent object to control fan operation
		Real64 const CompCycRatio, // cycling ratio of VRF condenser
		Optional_int_const PerfMode = _, // Performance mode for MultiMode DX coil; Always 1 for other coil types
		Optional< Real64 const > OnOffAirFlowRatio = _, // ratio of compressor on airflow to compressor off airflow
		Optional< Real64 const > MaxCoolCap = _ // maximum capacity of DX coil
	);

	void
	CalcDXHeatingCoil(
		int const DXCoilNum, // the number of the DX heating coil to be simulated
		Real64 const PartLoadRatio, // sensible cooling load / full load sensible cooling capacity
		int const FanOpMode, // Allows parent object to control fan mode
		Optional< Real64 const > OnOffAirFlowRatio = _, // ratio of compressor on airflow to compressor off airflow
		Optional< Real64 const > MaxHeatCap = _ // maximum allowed heating capacity
	);

	void
	CalcMultiSpeedDXCoil(
		int const DXCoilNum, // the number of the DX heating coil to be simulated
		Real64 const SpeedRatio, // = (CompressorSpeed - CompressorSpeedMin) / (CompressorSpeedMax - CompressorSpeedMin)
		Real64 const CycRatio, // cycling part load ratio
		Optional_bool_const ForceOn = _
	);

	void
	CalcBasinHeaterPowerForMultiModeDXCoil(
		int const DXCoilNum, // Index of coil being simulated
		int const DehumidMode // Dehumidification mode (0=normal, 1=enhanced)
	);

	Real64
	AdjustCBF(
		Real64 const CBFNom, // nominal coil bypass factor
		Real64 const AirMassFlowRateNom, // nominal air mass flow rate [kg/s]
		Real64 const AirMassFlowRate // actual air mass flow rate [kg/s]
	);

	Real64
	CalcCBF(
		std::string const & UnitType,
		std::string const & UnitName,
		Real64 const InletAirTemp, // inlet air temperature [C]
		Real64 const InletAirHumRat, // inlet air humidity ratio [kg water / kg dry air]
		Real64 const TotCap, // total cooling  capacity [Watts]
		Real64 const AirMassFlowRate, // the air mass flow rate at the given capacity [kg/s]
		Real64 const SHR // sensible heat ratio at the given capacity and flow rate
	);

	Real64
	CalcEffectiveSHR(
		int const DXCoilNum, // Index number for cooling coil
		Real64 const SHRss, // Steady-state sensible heat ratio
		Real64 const RTF, // Compressor run-time fraction
		Real64 const QLatRated, // Rated latent capacity
		Real64 const QLatActual, // Actual latent capacity
		Real64 const EnteringDB, // Entering air dry-bulb temperature
		Real64 const EnteringWB, // Entering air wet-bulb temperature
		Optional_int_const Mode = _, // Performance mode for MultiMode DX coil; Always 1 for other coil types
		Optional< Real64 const > HeatingRTF = _ // Used to recalculate Toff for cycling fan systems
	);

	void
	CalcTotCapSHR(
		Real64 const InletDryBulb, // inlet air dry bulb temperature [C]
		Real64 const InletHumRat, // inlet air humidity ratio [kg water / kg dry air]
		Real64 const InletEnthalpy, // inlet air specific enthalpy [J/kg]
		Real64 const InletWetBulb, // inlet air wet bulb temperature [C]
		Real64 const AirMassFlowRatio, // Ratio of actual air mass flow to nominal air mass flow
		Real64 const AirMassFlow, // actual mass flow for capacity and SHR calculation
		Real64 const TotCapNom, // nominal total capacity [W]
		Real64 const CBF, // coil bypass factor
		int const CCapFTemp, // capacity modifier curve index, function of entering wetbulb
		int const CCapFFlow, // capacity modifier curve, function of actual flow vs rated flow
		Real64 & TotCap, // total capacity at the given conditions [W]
		Real64 & SHR, // sensible heat ratio at the given conditions
		Real64 const CondInletTemp, // Condenser inlet temperature [C]
		Real64 const Pressure // air pressure [Pa]
	);

	void
	CalcMultiSpeedDXCoilCooling(
		int const DXCoilNum, // the number of the DX heating coil to be simulated
		Real64 const SpeedRatio, // = (CompressorSpeed - CompressorSpeedMin) / (CompressorSpeedMax - CompressorSpeedMin)
		Real64 const CycRatio, // cycling part load ratio
		int const SpeedNum, // Speed number
		int const FanOpMode, // Sets fan control to CycFanCycCoil or ContFanCycCoil
		int const CompOp // Compressor on/off; 1=on, 0=off
	);

	void
	CalcMultiSpeedDXCoilHeating(
		int const DXCoilNum, // the number of the DX heating coil to be simulated
		Real64 const SpeedRatio, // = (CompressorSpeed - CompressorSpeedMin) / (CompressorSpeedMax - CompressorSpeedMin)
		Real64 const CycRatio, // cycling part load ratio
		int const SpeedNum, // Speed number
		int const FanOpMode // Fan operation mode
	);

	void
	UpdateDXCoil( int const DXCoilNum ); // number of the current fan coil unit being simulated

	void
	ReportDXCoil( int const DXCoilNum ); // number of the current fan coil unit being simulated

	void
	CalcTwoSpeedDXCoilStandardRating( int const DXCoilNum );

	void
	GetFanIndexForTwoSpeedCoil(
		int const CoolingCoilIndex,
		int & SupplyFanIndex,
		std::string & SupplyFanName
	);

	Real64
	CalcTwoSpeedDXCoilIEERResidual(
		Real64 const SupplyAirMassFlowRate, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	// ======================  Utility routines ======================================

	void
	GetDXCoilIndex(
		std::string const & DXCoilName,
		int & DXCoilIndex,
		bool & ErrorsFound,
		Optional_string_const ThisObjectType = _,
		Optional_bool_const SuppressWarning = _
	);

	Real64
	GetCoilCapacity(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	Real64
	GetCoilCapacityByIndexType(
		int const CoilIndex, // must match coil index for the coil type
		int const CoilType_Num, // must match coil types in this module
		bool & ErrorsFound // set to true if problem
	);

	int
	GetCoilTypeNum(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound, // set to true if problem
		Optional_bool_const PrintWarning = _ // prints warning when true
	);

	Real64
	GetMinOATCompressor(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetCoilInletNode(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetCoilOutletNode(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetCoilCondenserInletNode(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	Real64
	GetDXCoilBypassedFlowFrac(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetHPCoolingCoilIndex(
		std::string const & HeatingCoilType, // Type of DX heating coil used in HP
		std::string const & HeatingCoilName, // Name of DX heating coil used in HP
		int const HeatingCoilIndex // Index of DX heating coil used in HP
	);

	int
	GetDXCoilNumberOfSpeeds(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetDXCoilAvailSchPtr(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound, // set to true if problem
		Optional_int_const CoilIndex = _ // Coil index number
	);

	Real64
	GetDXCoilAirFlow(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetDXCoilCapFTCurveIndex(
		int const CoilIndex, // coil index pointer
		bool & ErrorsFound // set to true if problem
	);

	void
	SetDXCoolingCoilData(
		int const DXCoilNum, // Number of DX Cooling Coil
		bool & ErrorsFound, // Set to true if certain errors found
		Optional_int HeatingCoilPLFCurvePTR = _, // Parameter equivalent of heating coil PLR curve index
		Optional_int CondenserType = _, // Parameter equivalent of condenser type parameter
		Optional_int CondenserInletNodeNum = _, // Parameter equivalent of condenser inlet node number
		Optional< Real64 > MaxOATCrankcaseHeater = _, // Parameter equivalent of condenser Max OAT for Crank Case Heater temp
		Optional< Real64 > MinOATCooling = _, // Parameter equivalent of condenser Min OAT for compressor cooling operation
		Optional< Real64 > MaxOATCooling = _, // Parameter equivalent of condenser Max OAT for compressor cooling operation
		Optional< Real64 > MinOATHeating = _, // Parameter equivalent of condenser Min OAT for compressor heating operation
		Optional< Real64 > MaxOATHeating = _, // Parameter equivalent of condenser Max OAT for compressor heating operation
		Optional_int HeatingPerformanceOATType = _, // Parameter equivalent to condenser entering air temp type (1-db, 2=wb)
		Optional_int DefrostStrategy = _,
		Optional_int DefrostControl = _,
		Optional_int DefrostEIRPtr = _,
		Optional< Real64 > DefrostFraction = _,
		Optional< Real64 > DefrostCapacity = _,
		Optional< Real64 > MaxOATDefrost = _,
		Optional_bool CoolingCoilPresent = _,
		Optional_bool HeatingCoilPresent = _,
		Optional< Real64 > HeatSizeRatio = _,
		Optional< Real64 > TotCap = _,
		Optional_int SupplyFanIndex = _,
		Optional_string SupplyFanName = _
	);

	void
	SetCoilSystemHeatingDXFlag(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName // must match coil names for the coil type
	);

	void
	SetCoilSystemCoolingData(
		std::string const & CoilName, // must match coil names for the coil type
		std::string const & CoilSystemName
	);

	Real64
	CalcSHRUserDefinedCurves(
		Real64 const InletDryBulb, // inlet air dry bulb temperature [C]
		Real64 const InletWetBulb, // inlet air wet bulb temperature [C]
		Real64 const AirMassFlowRatio, // ratio of actual air mass flow to rated air mass flow
		int const SHRFTempCurveIndex, // SHR modifier curve index
		int const SHRFFlowCurveIndex, // SHR modifier curve index
		Real64 const SHRRated // rated sensible heat ratio, user input
	);

	void
	SetDXCoilTypeData( std::string const & CoilName ); // must match coil names for the coil type

	void
	CalcSecondaryDXCoils( int const DXCoilNum );

	Real64
	CalcSecondaryDXCoilsSHR(
		int const DXCoilNum,
		Real64 const EvapAirMassFlow,
		Real64 const TotalHeatRemovalRate,
		Real64 const PartLoadRatio,
		Real64 const SecCoilRatedSHR,
		Real64 const EvapInletDryBulb,
		Real64 const EvapInletHumRat,
		Real64 const EvapInletWetBulb,
		Real64 const EvapInletEnthalpy,
		Real64 const CondInletDryBulb,
		Real64 const SecCoilFlowFraction,
		int const SecCoilSHRFT,
		int const SecCoilSHRFF
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

} // DXCoils

} // EnergyPlus

#endif
