// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

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
	extern bool MyOneTimeFlag; // One time flag used to allocate MyEnvrnFlag and MySizeFlag
	extern bool MyOneTimeSizeFlag; // One time flag used to allocate MyEnvrnFlag and MySizeFlag
	extern Array1D_bool CheckEquipName; // Flag set to check equipment connections once
	extern bool ZoneEquipmentListNotChecked; // False after the Zone Equipment List has been checked for items
	extern Array1D_bool MyEnvrnFlag; // Flag for initializing at beginning of each new environment
	extern Array1D_bool MySizeFlag; // False after TU has been sized
	extern Array1D_bool MyBeginTimeStepFlag; // Flag to sense beginning of time step
	extern Array1D_bool MyVRFFlag; // used for sizing VRF inputs one time
	extern Array1D_bool MyVRFCondFlag; // used to reset timer counter
	extern Array1D_bool MyZoneEqFlag; // used to set up zone equipment availability managers
	extern int NumVRFCond; // total number of VRF condensers (All VRF Algorithm Types)
	extern int NumVRFCond_SysCurve; // total number of VRF condensers with VRF Algorithm Type 1 
	extern int NumVRFCond_FluidTCtrl; // total number of VRF condensers with VRF Algorithm Type 2 
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
		int VRFAlgorithmTypeNum; // Algorithm type: 1_system curve based model; 2_physics based model (FluidTCtrl)
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
		//begin variables used for Defrost 
		int DefrostEIRPtr; // index to defrost EIR curve
		Real64 DefrostFraction; // defrost time period fraction (hr)
		int DefrostStrategy; // Type of defrost (reversecycle or resistive)
		int DefrostControl; // type of defrost control (timed or ondemand)
		Real64 DefrostCapacity; // capacity of resistive defrost heating element (W)
		Real64 DefrostPower; // power used during defrost (W)
		Real64 DefrostConsumption; // energy used during defrost (J)
		Real64 MaxOATDefrost; // maximum outdoor air dry-bulb temp for defrost operation (C)
		//end variables used for Defrost 
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
		
		//The following are for the Algorithm Type: VRF model based on physics, applicable for Fluid Temperature Control
		std::string RefrigerantName; // Name of refrigerant, must match name in FluidName (see fluidpropertiesrefdata.idf)
		Real64 RatedEvapCapacity; // Rated Evaporative Capacity [W]
		Real64 RatedCompPower; // Rated Evaporative Capacity [W]
		Real64 CondensingTemp; // VRV system outdoor unit condensing temperature [C]
		Real64 EvaporatingTemp; // VRV system outdoor unit evaporating temperature [C]
		Real64 IUEvaporatingTemp; // VRV system indoor unit evaporating temperature, min among all indoor units [C]
		Real64 IUCondensingTemp; // VRV system indoor unit condensing temperature, max among all indoor units [C]
		Real64 IUEvapTempLow; // VRV system indoor unit evaporating temperature, lower bound[C]
		Real64 IUEvapTempHigh; // VRV system indoor unit evaporating temperature, higher bound [C]
		Real64 IUCondTempLow; // VRV system indoor unit condensing temperature, lower bound [C]
		Real64 IUCondTempHigh; // VRV system indoor unit condensing temperature, higher bound [C]
		Real64 OUEvapTempLow; // VRV system outdoor unit evaporating temperature, lower bound[C]
		Real64 OUEvapTempHigh; // VRV system outdoor unit evaporating temperature, higher bound [C]
		Real64 OUCondTempLow; // VRV system outdoor unit condensing temperature, lower bound [C]
		Real64 OUCondTempHigh; // VRV system outdoor unit condensing temperature, higher bound [C]
		Real64 OUAirFlowRate; // Max condenser air flow rate  [m3/s]
		Real64 SH; // VRF outdoor unit superheating degrees [C]
		Real64 SC; // VRF outdoor unit subcooling degrees [C]
		Real64 C1Te; // VRF Outdoor Unit Coefficient 1 to calculate Te,req [--]
		Real64 C2Te; // VRF Outdoor Unit Coefficient 2 to calculate Te,req [--]
		Real64 C3Te; // VRF Outdoor Unit Coefficient 3 to calculate Te,req [--]
		Real64 C1Tc; // VRF Outdoor Unit Coefficient 1 to calculate Tc,req [--]
		Real64 C2Tc; // VRF Outdoor Unit Coefficient 2 to calculate Tc,req [--]
		Real64 C3Tc; // VRF Outdoor Unit Coefficient 3 to calculate Tc,req [--]
		int AlgorithmIUCtrl; //VRF indoor unit contrl algorithm, 1-High sensible, 2-Te/Tc constant
		Real64 EvapTempFixed; // Indoor unit evaporating temperature, fixed, for AlgorithmIUCtrl is 2-Te/Tc constant [C]
		Real64 CondTempFixed; // Inddor unit condensing temperature, fixed, for AlgorithmIUCtrl is 2-Te/Tc constant [C]
		Real64 RatedCondFanPower; // Outdoor unit fan power at rated conditions [W]
		Real64 CondFanPower; // Outdoor unit fan power at real conditions[W]
		Real64 CompActSpeed; // Compressor speed [rps]
		Real64 NcompCooling; // compressor electric power at cooling mode [W]
		Real64 NcompHeating; // compressor electric power at heating mode [W]
		Array1D< Real64 > CompressorSpeed; // compressor speed array [rps]
		Array1D_int OUCoolingCAPFT; // index to outdoor unit cooling capacity function of temperature at different compressor speed
		Array1D_int OUCoolingPWRFT; // index to outdoor unit cooling power function of temperature at different compressor speed
		Real64 CompMaxDeltaP; // maximum compressor pressure rise [Pa]
		Real64 RefPipDia;  // diameter of refrigerant pipe that links the outdoor unit to the indoor units [m]
		Real64 RefPipLen;  // length of refrigerant pipe that links the outdoor unit to the indoor units [m]
		Real64 RefPipEquLen; // Equivilent length of refrigerant pipe for pressure drop calculations [m]
		Real64 RefPipHei; // height of refrigerant pipe that links the outdoor unit to the indoor units [m]
		Real64 RefPipInsThi; // thickness of refrigerant pipe insulation [m]
		Real64 RefPipInsCon; // thermal conductivity of refrigerant pipe insulation [W/mk]
		Real64 VRFOperationSimPath; // simulation path indicating the VRF operation mode [--]

		// Default Constructor
		VRFCondenserEquipment() :
			VRFSystemTypeNum( 0 ),
			VRFAlgorithmTypeNum( 0 ),
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
			HPOperatingModeErrorIndex( 0 ),
			RatedEvapCapacity( 40000.0 ),
			RatedCompPower( 14000.0 ),
			CondensingTemp( 44.0 ),
			EvaporatingTemp( 6.0 ),
			IUEvaporatingTemp( 6.0 ), 
			IUCondensingTemp( 44.0 ),
			IUEvapTempLow( 4.0 ),
			IUEvapTempHigh( 13.0 ),
			IUCondTempLow( 42.0 ), 
			IUCondTempHigh( 46.0 ),
			OUEvapTempLow( -30.0 ),
			OUEvapTempHigh( 20.0 ),
			OUCondTempLow( 30.0 ), 
			OUCondTempHigh( 96.0 ),
			OUAirFlowRate( 0.0 ),	             
			SH( 0.0 ),                       			         
			SC( 0.0 ),                       			         
			C1Te( 0.0 ),	                   			         
			C2Te( 0.0 ),                     			         
			C3Te( 0.0 ),                     			         
			C1Tc( 0.0 ),	                   			         
			C2Tc( 0.0 ),                     			         
			C3Tc( 0.0 ),                     			         
			AlgorithmIUCtrl( 1 ),            
			EvapTempFixed( 0.0 ),            			         
			CondTempFixed( 0.0 ),            			         
			RatedCondFanPower( 0.0 ),   
			CondFanPower( 0.0 ),  
			CompActSpeed( 0.0 ),   
			NcompCooling( 0.0 ),   
			NcompHeating( 0.0 ),   		 	      		 
			CompMaxDeltaP( 0.0 ),         	                     
			RefPipDia( 0.0 ),         	                         
			RefPipLen( 0.0 ),         	           
			RefPipEquLen( 0.0 ),
			RefPipHei( 0.0 ),       	                         
			RefPipInsThi( 0.0 ),
			RefPipInsCon( 0.0 ),
			VRFOperationSimPath( 0.0 )
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

	};


	struct VRFTUNumericFieldData
	{
		// Members
		Array1D_string FieldNames;

		// Default Constructor
		VRFTUNumericFieldData()
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

	void
	GetVRFInputData(
		bool & ErrorsFound // flag for errors in GetInput
	);

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

	// Clears the global data in CurveManager.
	// Needed for unit tests, should not be normally called.
	void
	clear_state();

	// End of Utility subroutines for the Module
	// *****************************************************************************

	
	// Begin of Methods for New VRF Model: Fluid Temperature Control
	//******************************************************************************
	void
	CalcVRFIUTeTc_FluidTCtrl(
		int const IndexVRFCondenser // index to VRF Outdoor Unit
	);
	
	void
	CalcVRFIUVariableTeTc(
		int const VRFTUNum, // the number of the VRF TU to be simulated
		Real64 & EvapTemp, // evaporating temperature
		Real64 & CondTemp  // condensing temperature 
	);
		
	void
	ControlVRF_FluidTCtrl(
		int const VRFTUNum, // Index to VRF terminal unit
		Real64 const QZnReq, // Index to zone number
		bool const FirstHVACIteration, // flag for 1st HVAC iteration in the time step
		Real64 & PartLoadRatio, // unit part load ratio
		Real64 & OnOffAirFlowRatio // ratio of compressor ON airflow to AVERAGE airflow over timestep
	);
	
	void
	CalcVRF_FluidTCtrl(
		int const VRFTUNum, // Unit index in VRF terminal unit array
		bool const FirstHVACIteration, // flag for 1st HVAC iteration in the time step
		Real64 const PartLoadRatio, // compressor part load fraction
		Real64 & LoadMet, // load met by unit (W)
		Real64 & OnOffAirFlowRatio, // ratio of ON air flow to average air flow
		Optional< Real64 > LatOutputProvided = _ // delivered latent capacity (W)
	);

	void
	CalcVRFCondenser_FluidTCtrl(
		int const VRFCond, // index to VRF condenser
		bool const FirstHVACIteration // flag for first time through HVAC system simulation
	);
	
	Real64
	CalVRFTUAirFlowRate_FluidTCtrl(
		int VRFTUNum, // TU index
		Real64 PartLoadRatio, // part load ratio of the coil 
		bool FirstHVACIteration // FirstHVACIteration flag
	);
	
	Real64
	VRFTUAirFlowResidual_FluidTCtrl(
		Real64 const FanSpdRatio, // fan speed ratio of VRF VAV TU 
		Array1< Real64 > const & Par // par(1) = VRFTUNum
	);
	
	Real64 
	CompResidual_FluidTCtrl( 
		Real64 const Te, // Outdoor unit evaporating temperature
		Array1< Real64 > const & Par // Array of parameters
	);

	// End of Methods for New VRF Model: Fluid Temperature Control
	// *****************************************************************************

} // HVACVariableRefrigerantFlow

} // EnergyPlus

#endif
