#ifndef HVACUnitarySystem_hh_INCLUDED
#define HVACUnitarySystem_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <DataSizing.hh>

namespace EnergyPlus {

namespace HVACUnitarySystem {

	// Using/Aliasing
	using DataSizing::AutoSize;

	// Data
	//MODULE PARAMETER DEFINITIONS
	extern Real64 const MinAirMassFlow;

	// Last mode of operation
	extern int const CoolingMode; // last compressor operating mode was in cooling
	extern int const HeatingMode; // last compressor operating mode was in heating

	// Compressor operation
	extern int const On; // normal compressor operation
	extern int const Off; // signal DXCoil that compressor shouldn't run

	// Dehumidification control modes (DehumidControlMode)
	extern int const DehumidControl_None;
	extern int const DehumidControl_Multimode;
	extern int const DehumidControl_CoolReheat;

	// Coil type for SimWater and SimSteamCoil
	extern int const CoolingCoil;
	extern int const HeatingCoil;
	extern int const SuppHeatCoil;

	// Supply Air Sizing Option
	extern int const None;
	extern int const SupplyAirFlowRate;
	extern int const FlowPerFloorArea;
	extern int const FractionOfAutoSizedCoolingValue;
	extern int const FractionOfAutoSizedHeatingValue;
	extern int const FlowPerCoolingCapacity;
	extern int const FlowPerHeatingCapacity;

	// Airflow control for contant fan mode
	extern int const UseCompressorOnFlow; // set compressor OFF air flow rate equal to compressor ON air flow rate
	extern int const UseCompressorOffFlow; // set compressor OFF air flow rate equal to user defined value

	// System Control Type
	extern int const LoadBased; // control system based on zone load
	extern int const SetPointBased; // control system based on coil set point manager

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	extern bool GetInputFlag; // Flag to get input only once
	extern bool EconomizerFlag; // holds air loop economizer status
	extern bool HeatingLoad; // True when zone needs heating
	extern bool CoolingLoad; // True when zone needs cooling
	extern Real64 MoistureLoad; // Dehumidification Load (W)
	extern bool SuppHeatingCoilFlag; // set to TRUE when simulating supplemental heating coil
	extern int NumUnitarySystem; // The Number of Unitary Systems found in the Input
	extern int NumDesignSpecMultiSpeedHP; // The number of design specification objects for MSHP
	extern Real64 CompOnMassFlow; // Supply air mass flow rate w/ compressor ON [kg/s]
	extern Real64 CompOffMassFlow; // Supply air mass flow rate w/ compressor OFF [kg/s]
	extern Real64 CompOnFlowRatio; // fan flow ratio when coil on
	extern Real64 CompOffFlowRatio; // fan flow ratio when coil off
	extern Real64 FanSpeedRatio; // ratio of air flow ratio passed to fan object
	extern Real64 CoolHeatPLRRat; // ratio of cooling to heating PLR, used for cycling fan RH control
	extern Real64 OnOffAirFlowRatioSave; // Saves the OnOffAirFlowRatio calculated in RegulaFalsi calls.
	extern Real64 QToCoolSetPt; // load to cooling set point {W}
	extern Real64 QToHeatSetPt; // load to heating set point {W}
	extern Real64 TempSteamIn; // steam coil steam inlet temperature

	// Allocatable types
	extern Array1D_bool CheckEquipName;
	extern Array1D_bool MultiOrVarSpeedHeatCoil;
	extern Array1D_bool MultiOrVarSpeedCoolCoil;

	// Subroutine Specifications for the Module
	// Driver/Manager Routines

	// Initialization routines

	// Get Input routines

	// Control routines to find PLR, check convergence and update nodes

	// Calc routines to simulate each child component in order

	// set point based calc routine
	// Load based calc routine

	// Airflow control routines

	// Verify set point exists for SetPointBased control
	// Heat recovery subroutine
	// Reporting routines for module

	// RegulaFalsi routines
	// ** RAR I'd rather see a SELECT CASE in 1 or 2 generic routines instead of one for each coil type

	// Types

	struct DesignSpecMSHPData
	{
		// Members
		std::string Name; // Name of the design specification MSHP
		int NumOfSpeedCooling; // The number of speeds for cooling
		int NumOfSpeedHeating; // The number of speeds for heating
		Array1D< Real64 > CoolingVolFlowRatio; // The ratio of flow to max for this speed
		Array1D< Real64 > HeatingVolFlowRatio; // The ratio of flow to max for this speed

		// Default Constructor
		DesignSpecMSHPData() :
			NumOfSpeedCooling( 0 ),
			NumOfSpeedHeating( 0 )
		{}

		// Member Constructor
		DesignSpecMSHPData(
			std::string const & Name, // Name of the design specification MSHP
			int const NumOfSpeedCooling, // The number of speeds for cooling
			int const NumOfSpeedHeating, // The number of speeds for heating
			Array1< Real64 > const & CoolingVolFlowRatio, // The ratio of flow to max for this speed
			Array1< Real64 > const & HeatingVolFlowRatio // The ratio of flow to max for this speed
		) :
			Name( Name ),
			NumOfSpeedCooling( NumOfSpeedCooling ),
			NumOfSpeedHeating( NumOfSpeedHeating ),
			CoolingVolFlowRatio( CoolingVolFlowRatio ),
			HeatingVolFlowRatio( HeatingVolFlowRatio )
		{}

	};

	struct UnitarySystemData
	{
		// Members
		// HVAC system specific data
		std::string UnitarySystemType; // Type of Unitary System
		int UnitarySystemType_Num; // integer type of Unitary System
		std::string Name; // Name of the Unitary System
		bool HeatPump; // TRUE if both cooling and heating coils are DX
		int SysAvailSchedPtr; // System Availability schedule
		int UnitarySystemInletNodeNum; // Parent inlet node number
		int UnitarySystemOutletNodeNum; // Parent outlet node number
		int CondenserType; // type of condenser (AirCooled, EvapCooled, WaterCooled)
		bool AirLoopEquipment; // identifies that this system is part of an air loop
		int ControlZoneNum; // Index to controlled zone
		int ZoneSequenceCoolingNum; // Index to cooling sequence/priority for this zone
		int ZoneSequenceHeatingNum; // Index to heating sequence/priority for this zone
		int NodeNumOfControlledZone; // Node number of controlled zone
		int ZoneInletNode; // Zone inlet node number in the controlled zone
		Real64 ControlZoneMassFlowFrac; // Fraction of flow to control zone
		bool Humidistat; // Set to True if dehumidification control mode is set to
		Real64 DesignMaxOutletTemp; // Maximum supply air temperature from heater [C]
		int CondenserNodeNum; // index to condenser air inlet node
		int DehumidControlType_Num; // Set to Dehumid Control None, CoolReheat or MultiMode
		int AirFlowControl; // UseCompressorOnFlow or UseCompressorOffFlow
		int ControlType; // Setpoint or Load based control
		bool RequestAutoSize; // determines if inputs need autosizing
		bool RunOnSensibleLoad; // logical determines if this system will run to
		bool RunOnLatentLoad; // logical determines if this system will run to
		bool RunOnLatentOnlyWithSensible; // allow latent dehumidification only if sensible load exists
		int DehumidificationMode; // Dehumidification mode for multimode coil,
		// 0=normal, 1+=enhanced dehumidification mode
		int FanOpMode; // Fan operating mode (see parameter above)
		int LastMode; // last mode of operation, coolingmode or heatingmode
		Real64 AncillaryOnPower; // Ancillary On-Cycle Electric Power [W]
		Real64 AncillaryOffPower; // Ancillary Off-Cycle Electric Power [W]
		std::string DesignSpecMultispeedHPType; // Object type for specifying multispeed flow rates
		std::string DesignSpecMultispeedHPName; // Object name for specifying multispeed flow rates
		// Cooling coil specific data
		std::string CoolingCoilName; // coil name (eliminate after blank is accepted in CALL)
		int CoolingCoilType_Num; // numeric coil type
		int CoolingCoilIndex; // index to specific cooling coil
		Real64 DesignCoolingCapacity; // cooling coil capacity (W)
		int CoolingCoilAvailSchPtr; // cooling coil availability schedule index
		int ActualDXCoilIndexForHXAssisted; // index to DX coil used in HX assisted object
		Real64 DOASDXCoolingCoilMinTout; // DOAS DX Cooling coil outlet air minimum temperature
		bool ISHundredPercentDOASDXCoil; // logical determines if this system will run as 100% DOAS
		bool CoolCoilExists; // True if a cooling coil is specified in the unitary system
		int FrostControlStatus; // DOAS coil system frost control status
		int CoolCoilInletNodeNum; // Cooling coil air inlet node number
		int CoolCoilOutletNodeNum; // Cooling coil air outlet node number
		int CoolCoilFluidOutletNodeNum; // Cooling coil fluid outlet node number (from Plant Loop data)
		int CoolCoilLoopNum; // Plant loop num of chilled water coil
		int CoolCoilLoopSide; // Supply side or demand side
		int CoolCoilBranchNum; // Branch of number of the cooling coil in the plant loop
		int CoolCoilCompNum; // Comp num of the cooling coil in the plant loop
		int CoolCoilFluidInletNode; // Cooling coil fluid inlet node
		Real64 MaxCoolCoilFluidFlow; // Maximum cooling coil fluid flow for chilled water coil
		bool CoolingCoilUpstream; // Set to true when coolign coil is upstream in the unitary system
		Real64 CoolCompPartLoadRatio; // Unitary system compressor part load ratio in cooling
		// Heating coil specific data
		std::string HeatingCoilName; // coil name (eliminate after blank is accepted in CALL)
		int HeatingCoilType_Num; // numeric coil type
		int HeatingCoilIndex; // index to specific heating coil
		Real64 DesignHeatingCapacity; // heating coil capacity (W)
		Real64 HeatingSizingRatio; // ratio of heating coil to cooling coil size
		bool DXHeatingCoil; // specifies if heating coil is DX
		int HeatCoilInletNodeNum; // Heating coil air inlet node number
		int HeatCoilOutletNodeNum; // Heating coil air outlet node number
		int HeatCoilFluidOutletNodeNum; // Heating coil fluid outlet node number (from Plant Loop data)
		int HeatingCoilPLFCurveIndex; // PLF curve index (not used yet?)
		int HeatingCoilAvailSchPtr; // heating coil availability schedule index
		bool HeatCoilExists; // True if a heating coil is specified in the unitary system
		int HeatCoilLoopNum; // Plant loop num of hot water or steam coil
		int HeatCoilLoopSide; // Supply side or demand side
		int HeatCoilBranchNum; // Branch of number of the heating coil in the plant loop
		int HeatCoilCompNum; // Comp num of the heating coil in the plant loop
		int HeatCoilFluidInletNode; // Heating coil fluid inlet node
		Real64 MaxHeatCoilFluidFlow; // Maximum heating coil fluid flow for hot water or steam coil
		Real64 HeatCompPartLoadRatio; // Unitary system compressor part load ratio in heating
		// Supplemental heating coil specific data
		std::string SuppHeatCoilName; // coil name (eliminate after blank is accepted in CALL)
		int SuppHeatCoilType_Num; // numeric coil type
		int SuppHeatCoilIndex; // index to specific supplemental heating coil
		Real64 DesignSuppHeatingCapacity; // supplemental heating coil capacity (W)
		int SuppCoilFluidInletNode; // supplemental heating coil water/steam inlet node
		int SuppCoilFluidOutletNodeNum; // Supplemental coil fluid outlet node number (from Plant Loop data)
		int SuppCoilAirInletNode; // supplemental heating coil air inlet node
		int SuppCoilAirOutletNode; // supplemental heating coil air outlet node
		int SuppCoilAvailSchPtr; // supplemental heating coil availability schedule index
		Real64 MaxSuppCoilFluidFlow; // supplemental heating coil maximum water/steam flow rate (m3/s)
		Real64 MaxOATSuppHeat; // Maximum outdoor dry-bulb temperature for supplemental coil [C]
		bool SuppCoilExists; // True if a supp coil is specified in the unitary system
		int SuppCoilLoopNum; // Plant loop num of supplemental coil
		int SuppCoilLoopSide; // Supply side or demand side
		int SuppCoilBranchNum; // Branch of number of the supplemental coil in the plant loop
		int SuppCoilCompNum; // Comp num of the supplemental coil in the plant loop
		// fan specific data
		int FanType_Num; // Fan type num i.e. OnOff, ConstVol, VAV
		int FanIndex; // index of fan of a particular type
		Real64 ActualFanVolFlowRate; // Actual or design fan volume flow rate
		int FanOpModeSchedPtr; // fan operating mode schedule pointer
		int FanAvailSchedPtr; // fan availability schedule pointer
		int FanPlace; // Blow through or DrawThrough Fan
		bool FanExists; // True if a fan is specified in the unitary system
		Real64 FanDelayTime; // Fan delay time, time delay for the HP's fan to
		// shut off after compressor cycle off [s]
		// air flow variables
		Real64 MaxCoolAirVolFlow; // Maximum coil air volumetric flow for cooling [m3/s]
		Real64 MaxHeatAirVolFlow; // Maximum coil air volumetric flow for heating [m3/s]
		Real64 MaxNoCoolHeatAirVolFlow; // Maximum coil air volumetric flow for no cooling or heating [m3/s]
		Real64 DesignFanVolFlowRate; // Design fan volume flow rate [m3/s]
		Real64 DesignMassFlowRate; // Design mass flow rate [m3/s]
		Real64 MaxCoolAirMassFlow; // Maximum coil air mass flow for cooling [kg/s]
		Real64 MaxHeatAirMassFlow; // Maximum coil air mass flow for heating [kg/s]
		Real64 MaxNoCoolHeatAirMassFlow; // Maximum coil air mass flow for no cooling or heating [kg/s]
		int CoolingSAFMethod; // Supply air flow method for cooling
		int HeatingSAFMethod; // Supply air flow method for heating
		int NoCoolHeatSAFMethod; // Supply air flow method for no cooling or heating
		// Heat pump related specific data
		Real64 MinOATCompressor; // Minimum outdoor temperature below which compressor if off
		Real64 MaxONOFFCyclesperHour; // Maximum cycling rate of unitary system [cycles/hr]
		Real64 HPTimeConstant; // Heat pump time constant [s]
		Real64 OnCyclePowerFraction; // Fraction of on-cycle power use [~]
		Real64 DesignHRWaterVolumeFlow; // Design water volume flow rate through heat recovery loop [m3/s]
		Real64 WSHPRuntimeFrac; // Runtime fraction of water source heat pump
		Real64 HeatingCoilSensDemand; // Sensible demand on Heating Coil [W]
		Real64 CoolingCoilSensDemand; // Sensible demand on Cooling Coil [W]
		Real64 CoolingCoilLatentDemand; // Latent demand on Cooling Coil [W]
		// Heat recovery related specific data
		int HeatRecoveryInletNodeNum; // Node number on heat recovery water inlet
		int HeatRecoveryOutletNodeNum; // Node number on heat recovery water outlet
		bool HeatRecActive; // True when entered Heat Rec Vol Flow Rate > 0
		Real64 DesignHeatRecMassFlowRate; // Design water mass flow rate through heat recovery loop [kg/s]
		Real64 MaxHROutletWaterTemp; // Maximum outlet water temperature for heat recovery [C]
		int HRLoopNum; // plant loop number for heat recovery
		int HRLoopSideNum; // Plant loop side (supply or demand) for heat recovery
		int HRBranchNum; // plant loop branch for heat recovery
		int HRCompNum; // plant loop component for heat recovery
		// set point based control varibles
		int SystemHeatControlNodeNum; // the node number of the node with the setpoint
		int SystemCoolControlNodeNum; // the node number of the node with the setpoint
		int SuppHeatControlNodeNum; // the node number of the node with the setpoint
		Real64 DesiredOutletTemp; // the setpoint temperature at the unit outlet node
		Real64 DesiredOutletHumRat; // the setpoint humidity ratio at the unit outlet node
		// operational system variables
		Real64 CoolingPartLoadFrac; // part load cooling fraction for current timestep
		Real64 HeatingPartLoadFrac; // part load heating fraction for current timestep
		Real64 SuppHeatPartLoadFrac; // part load supp heating fraction for current timestep
		Real64 SupHeaterLoad; // Supplemental Heat Load for current timestep
		Real64 SenLoadLoss; // Air distribution system sensible loss [W]
		Real64 LatLoadLoss; // Air distribution system latent loss [W]
		Real64 SensibleLoadMet; // System sensible load [W]
		Real64 LatentLoadMet; // System latent load [W]
		bool InitHeatPump; // Heat pump initialization flag (for error reporting)
		int WaterCyclingMode; // Heat Pump Coil water flow mode; See def in DataHVACGlobals,
		// 1=water cycling, 2=water constant, 3=water constant on demand
		// start of additional varibles for variable speed water source heat pump
		int HeatCoolMode; // System operating mode (0 = floating, 1 = cooling, 2 = heating)
		int NumOfSpeedCooling; // The number of speeds for cooling
		int NumOfSpeedHeating; // The number of speeds for heating
		Real64 IdleSpeedRatio; // idle air fan ratio
		Real64 IdleVolumeAirRate; // idle air flow rate [m3/s]
		Real64 IdleMassFlowRate; // idle air flow rate [kg/s]
		bool CheckFanFlow; // Supply airflow check
		Array1D< Real64 > HeatVolumeFlowRate; // Supply air volume flow rate during heating operation
		Array1D< Real64 > HeatMassFlowRate; // Supply air mass flow rate during heating operation
		Array1D< Real64 > CoolVolumeFlowRate; // Supply air volume flow rate during cooling operation
		Array1D< Real64 > CoolMassFlowRate; // Supply air mass flow rate during cooling operation
		Array1D< Real64 > MSHeatingSpeedRatio; // Fan speed ratio in heating mode
		Array1D< Real64 > MSCoolingSpeedRatio; // Fan speed ratio in cooling mode
		Real64 NoHeatCoolSpeedRatio; // Fan speed ratio when no cooling or heating
		int DesignSpecMSHPIndex; // Index to design specification multispeed heat pump object
		bool MultiSpeedCoolingCoil; // TRUE when cooling coil multispeed
		bool MultiSpeedHeatingCoil; // TRUE when heating coil multispeed
		bool VarSpeedCoolingCoil; // TRUE when cooling coil variable speed
		bool VarSpeedHeatingCoil; // TRUE when heating coil variable speed
		int CoolingSpeedNum; // speed number for multispeed cooling coils types
		int HeatingSpeedNum; // speed number for multispeed heating coils types
		Real64 CoolingSpeedRatio; // current compressor speed ratio (variable speed)
		Real64 CoolingFanSpeedRatio; // current fan speed ratio
		Real64 HeatingSpeedRatio; // current compressor speed ratio (variable speed)
		Real64 HeatingFanSpeedRatio; // current fan speed ratio
		Real64 CoolingCycRatio; // cycling part load ratio (variable speed)
		Real64 HeatingCycRatio; // cycling part load ratio (variable speed)
		// end of additional variables for variable speed water source heat pump
		// Report Varibles
		Real64 PartLoadFrac; // part load fraction for current time step (single speed)
		Real64 FanPartLoadRatio; // Unitary system fan part load ratio
		Real64 CompPartLoadRatio; // Unitary system compressor part load ratio
		Real64 ElecPower; // Unitary System Electric Power
		Real64 ElecPowerConsumption; // Electricity power comsumption: CondenserFan+CCHeater+Defrost+aux
		Real64 TotCoolEnergyRate; // Unitary System Total Cooling Rate [W]
		Real64 SensCoolEnergyRate; // Unitary System Sensible Cooling Rate [W]
		Real64 LatCoolEnergyRate; // Unitary System Latent Cooling Rate [W]
		Real64 TotHeatEnergyRate; // Unitary System Total Heating Rate [W]
		Real64 SensHeatEnergyRate; // Unitary System Sensible Heating Rate [W]
		Real64 LatHeatEnergyRate; // Unitary System Latent Heating Rate [W]
		Real64 TotalAuxElecPower; // Unitary System Ancillary Electric Power [W]
		Real64 HeatingAuxElecConsumption; // Unitary System Heating Ancillary Electric Energy [J]
		Real64 CoolingAuxElecConsumption; // Unitary System Cooling Ancillary Electric Energy [J]
		Real64 HeatRecoveryRate; // Unitary System Heat Recovery Rate [W]
		Real64 HeatRecoveryEnergy; // Unitary System Heat Recovery Energy [J]
		Real64 HeatRecoveryInletTemp; // Unitary System Heat Recovery Inlet Temperature [C]
		Real64 HeatRecoveryOutletTemp; // Unitary System Heat Recovery Outlet Temperature [C]
		Real64 HeatRecoveryMassFlowRate; // Unitary System Heat Recovery Fluid Mass Flow Rate [kg/s]
		Real64 DehumidInducedHeatingDemandRate; // Unitary System
		Real64 EMSSensibleZoneLoadValue; // Value EMS is directing to use
		Real64 EMSMoistureZoneLoadValue; // Value EMS is directing to use
		int SpeedNum; // speed number of active multi- or variable-speed coil
		Real64 SpeedRatio; // current compressor speed ratio (variable speed)
		Real64 CycRatio; // cycling part load ratio (variable speed)
		// Warning message variables
		int HXAssistedSensPLRIter; // used in HX Assisted calculations
		int HXAssistedSensPLRIterIndex; // used in HX Assisted calculations
		int HXAssistedSensPLRFail; // used in HX Assisted calculations
		int HXAssistedSensPLRFailIndex; // used in HX Assisted calculations
		int HXAssistedSensPLRFail2; // used in HX Assisted calculations
		int HXAssistedSensPLRFailIndex2; // used in HX Assisted calculations
		int HXAssistedLatPLRIter; // used in HX Assisted calculations
		int HXAssistedLatPLRIterIndex; // used in HX Assisted calculations
		int HXAssistedLatPLRFail; // used in HX Assisted calculations
		int HXAssistedLatPLRFailIndex; // used in HX Assisted calculations
		int HXAssistedCRLatPLRIter; // used in HX Assisted calculations
		int HXAssistedCRLatPLRIterIndex; // used in HX Assisted calculations
		int HXAssistedCRLatPLRFail; // used in HX Assisted calculations
		int HXAssistedCRLatPLRFailIndex; // used in HX Assisted calculations
		int HXAssistedCRLatPLRFail2; // used in HX Assisted calculations
		int HXAssistedCRLatPLRFailIndex2; // used in HX Assisted calculations
		int SensPLRIter; // used in cool coil calculations
		int SensPLRIterIndex; // used in cool coil calculations
		int SensPLRFail; // used in cool coil calculations
		int SensPLRFailIndex; // used in cool coil calculations
		int LatPLRIter; // used in cool coil calculations
		int LatPLRIterIndex; // used in cool coil calculations
		int LatPLRFail; // used in cool coil calculations
		int LatPLRFailIndex; // used in cool coil calculations
		int HeatCoilSensPLRIter; // used in heat coil calculations
		int HeatCoilSensPLRIterIndex; // used in heat coil calculations
		int HeatCoilSensPLRFail; // used in heat coil calculations
		int HeatCoilSensPLRFailIndex; // used in heat coil calculations
		int SuppHeatCoilSensPLRIter; // used in supp heat coil calculations
		int SuppHeatCoilSensPLRIterIndex; // used in supp heat coil calculations
		int SuppHeatCoilSensPLRFail; // used in supp heat coil calculations
		int SuppHeatCoilSensPLRFailIndex; // used in supp heat coil calculations
		int DXCoilSensPLRIter; // used in DXCoil calculations
		int DXCoilSensPLRIterIndex; // used in DXCoil calculations
		int DXCoilSensPLRFail; // used in DXCoil calculations
		int DXCoilSensPLRFailIndex; // used in DXCoil calculations
		int MSpdSensPLRIter; // used in MultiSpeed calculations
		int MSpdSensPLRIterIndex; // used in MultiSpeed calculations
		int MSpdCycSensPLRIter; // used in MultiSpeed calculations
		int MSpdCycSensPLRIterIndex; // used in MultiSpeed calculations
		int MSpdLatPLRIter; // used in MultiSpeed calculations
		int MSpdLatPLRIterIndex; // used in MultiSpeed calculations
		int MSpdCycLatPLRIter; // used in MultiSpeed calculations
		int MSpdCycLatPLRIterIndex; // used in MultiSpeed calculations
		int MaxIterIndex; // used in PLR calculations for sensible load
		int RegulaFalsIFailedIndex; // used in PLR calculations for sensible load
		int LatMaxIterIndex; // used in PLR calculations for moisture load
		int LatRegulaFalsIFailedIndex; // used in PLR calculations for moisture load
		// EMS variables
		bool DesignFanVolFlowRateEMSOverrideOn; // If true, then EMS is calling to override autosize fan flow
		bool MaxHeatAirVolFlowEMSOverrideOn; // If true, then EMS is calling to override autosize fan flow
		bool MaxCoolAirVolFlowEMSOverrideOn; // If true, then EMS is calling to override autosize fan flow
		bool MaxNoCoolHeatAirVolFlowEMSOverrideOn; // If true, then EMS is calling to override autosize fan flow
		Real64 DesignFanVolFlowRateEMSOverrideValue; // EMS value for override of fan flow rate autosize [m3/s]
		Real64 MaxHeatAirVolFlowEMSOverrideValue; // EMS value for override of fan flow rate autosize [m3/s]
		Real64 MaxCoolAirVolFlowEMSOverrideValue; // EMS value for override of fan flow rate autosize [m3/s]
		Real64 MaxNoCoolHeatAirVolFlowEMSOverrideValue; // EMS value for override of fan flow rate autosize [m3/s]
		bool EMSOverrideSensZoneLoadRequest; // If true, then EMS is calling to override zone load
		bool EMSOverrideMoistZoneLoadRequest; // If true, then EMS is calling to override zone load
		// Staged thermostat control
		int StageNum; // Stage number specified by staged thermostat
		bool Staged; // Using Staged thermostat
		int CoolCountAvail; // Counter used to minimize the occurrence of output warnings
		int CoolIndexAvail; // Index used to minimize the occurrence of output warnings
		int HeatCountAvail; // Counter used to minimize the occurrence of output warnings
		int HeatIndexAvail; // Index used to minimize the occurrence of output warnings

		// Default Constructor
		UnitarySystemData() :
			UnitarySystemType_Num( 0 ),
			HeatPump( false ),
			SysAvailSchedPtr( 0 ),
			UnitarySystemInletNodeNum( 0 ),
			UnitarySystemOutletNodeNum( 0 ),
			CondenserType( 0 ),
			AirLoopEquipment( true ),
			ControlZoneNum( 0 ),
			ZoneSequenceCoolingNum( 0 ),
			ZoneSequenceHeatingNum( 0 ),
			NodeNumOfControlledZone( 0 ),
			ZoneInletNode( 0 ),
			ControlZoneMassFlowFrac( 0.0 ),
			Humidistat( false ),
			DesignMaxOutletTemp( 80.0 ),
			CondenserNodeNum( 0 ),
			DehumidControlType_Num( 0 ),
			AirFlowControl( 1 ),
			ControlType( 0 ),
			RequestAutoSize( false ),
			RunOnSensibleLoad( true ),
			RunOnLatentLoad( false ),
			RunOnLatentOnlyWithSensible( false ),
			DehumidificationMode( 0 ),
			FanOpMode( 0 ),
			LastMode( 0 ),
			AncillaryOnPower( 0.0 ),
			AncillaryOffPower( 0.0 ),
			CoolingCoilType_Num( 0 ),
			CoolingCoilIndex( 0 ),
			DesignCoolingCapacity( 0.0 ),
			CoolingCoilAvailSchPtr( 0 ),
			ActualDXCoilIndexForHXAssisted( 0 ),
			DOASDXCoolingCoilMinTout( 0.0 ),
			ISHundredPercentDOASDXCoil( false ),
			CoolCoilExists( false ),
			FrostControlStatus( 0 ),
			CoolCoilInletNodeNum( 0 ),
			CoolCoilOutletNodeNum( 0 ),
			CoolCoilFluidOutletNodeNum( 0 ),
			CoolCoilLoopNum( 0 ),
			CoolCoilLoopSide( 0 ),
			CoolCoilBranchNum( 0 ),
			CoolCoilCompNum( 0 ),
			CoolCoilFluidInletNode( 0 ),
			MaxCoolCoilFluidFlow( AutoSize ),
			CoolingCoilUpstream( true ),
			CoolCompPartLoadRatio( 0.0 ),
			HeatingCoilType_Num( 0 ),
			HeatingCoilIndex( 0 ),
			DesignHeatingCapacity( 0.0 ),
			HeatingSizingRatio( 1.0 ),
			DXHeatingCoil( false ),
			HeatCoilInletNodeNum( 0 ),
			HeatCoilOutletNodeNum( 0 ),
			HeatCoilFluidOutletNodeNum( 0 ),
			HeatingCoilPLFCurveIndex( 0 ),
			HeatingCoilAvailSchPtr( 0 ),
			HeatCoilExists( false ),
			HeatCoilLoopNum( 0 ),
			HeatCoilLoopSide( 0 ),
			HeatCoilBranchNum( 0 ),
			HeatCoilCompNum( 0 ),
			HeatCoilFluidInletNode( 0 ),
			MaxHeatCoilFluidFlow( AutoSize ),
			HeatCompPartLoadRatio( 0.0 ),
			SuppHeatCoilType_Num( 0 ),
			SuppHeatCoilIndex( 0 ),
			DesignSuppHeatingCapacity( 0.0 ),
			SuppCoilFluidInletNode( 0 ),
			SuppCoilFluidOutletNodeNum( 0 ),
			SuppCoilAirInletNode( 0 ),
			SuppCoilAirOutletNode( 0 ),
			SuppCoilAvailSchPtr( 0 ),
			MaxSuppCoilFluidFlow( AutoSize ),
			MaxOATSuppHeat( 21.0 ),
			SuppCoilExists( false ),
			SuppCoilLoopNum( 0 ),
			SuppCoilLoopSide( 0 ),
			SuppCoilBranchNum( 0 ),
			SuppCoilCompNum( 0 ),
			FanType_Num( 0 ),
			FanIndex( 0 ),
			ActualFanVolFlowRate( 0.0 ),
			FanOpModeSchedPtr( 0 ),
			FanAvailSchedPtr( 0 ),
			FanPlace( 0 ),
			FanExists( false ),
			FanDelayTime( 0.0 ),
			MaxCoolAirVolFlow( 0.0 ),
			MaxHeatAirVolFlow( 0.0 ),
			MaxNoCoolHeatAirVolFlow( 0.0 ),
			DesignFanVolFlowRate( 0.0 ),
			DesignMassFlowRate( 0.0 ),
			MaxCoolAirMassFlow( 0.0 ),
			MaxHeatAirMassFlow( 0.0 ),
			MaxNoCoolHeatAirMassFlow( 0.0 ),
			CoolingSAFMethod( 0 ),
			HeatingSAFMethod( 0 ),
			NoCoolHeatSAFMethod( 0 ),
			MinOATCompressor( 0.0 ),
			MaxONOFFCyclesperHour( 0.0 ),
			HPTimeConstant( 0.0 ),
			OnCyclePowerFraction( 0.0 ),
			DesignHRWaterVolumeFlow( 0.0 ),
			WSHPRuntimeFrac( 0.0 ),
			HeatingCoilSensDemand( 0.0 ),
			CoolingCoilSensDemand( 0.0 ),
			CoolingCoilLatentDemand( 0.0 ),
			HeatRecoveryInletNodeNum( 0 ),
			HeatRecoveryOutletNodeNum( 0 ),
			HeatRecActive( false ),
			DesignHeatRecMassFlowRate( 0.0 ),
			MaxHROutletWaterTemp( 0.0 ),
			HRLoopNum( 0 ),
			HRLoopSideNum( 0 ),
			HRBranchNum( 0 ),
			HRCompNum( 0 ),
			SystemHeatControlNodeNum( 0 ),
			SystemCoolControlNodeNum( 0 ),
			SuppHeatControlNodeNum( 0 ),
			DesiredOutletTemp( 0.0 ),
			DesiredOutletHumRat( 1.0 ),
			CoolingPartLoadFrac( 0.0 ),
			HeatingPartLoadFrac( 0.0 ),
			SuppHeatPartLoadFrac( 0.0 ),
			SupHeaterLoad( 0.0 ),
			SenLoadLoss( 0.0 ),
			LatLoadLoss( 0.0 ),
			SensibleLoadMet( 0.0 ),
			LatentLoadMet( 0.0 ),
			InitHeatPump( true ),
			WaterCyclingMode( 0 ),
			HeatCoolMode( 0 ),
			NumOfSpeedCooling( 0 ),
			NumOfSpeedHeating( 0 ),
			IdleSpeedRatio( 0 ),
			IdleVolumeAirRate( 0 ),
			IdleMassFlowRate( 0 ),
			CheckFanFlow( true ),
			NoHeatCoolSpeedRatio( 1.0 ),
			DesignSpecMSHPIndex( 0 ),
			MultiSpeedCoolingCoil( false ),
			MultiSpeedHeatingCoil( false ),
			VarSpeedCoolingCoil( false ),
			VarSpeedHeatingCoil( false ),
			CoolingSpeedNum( 0 ),
			HeatingSpeedNum( 0 ),
			CoolingSpeedRatio( 1.0 ),
			CoolingFanSpeedRatio( 1.0 ),
			HeatingSpeedRatio( 1.0 ),
			HeatingFanSpeedRatio( 1.0 ),
			CoolingCycRatio( 0.0 ),
			HeatingCycRatio( 0.0 ),
			PartLoadFrac( 0.0 ),
			FanPartLoadRatio( 0.0 ),
			CompPartLoadRatio( 0.0 ),
			ElecPower( 0.0 ),
			ElecPowerConsumption( 0.0 ),
			TotCoolEnergyRate( 0.0 ),
			SensCoolEnergyRate( 0.0 ),
			LatCoolEnergyRate( 0.0 ),
			TotHeatEnergyRate( 0.0 ),
			SensHeatEnergyRate( 0.0 ),
			LatHeatEnergyRate( 0.0 ),
			TotalAuxElecPower( 0.0 ),
			HeatingAuxElecConsumption( 0.0 ),
			CoolingAuxElecConsumption( 0.0 ),
			HeatRecoveryRate( 0.0 ),
			HeatRecoveryEnergy( 0.0 ),
			HeatRecoveryInletTemp( 0.0 ),
			HeatRecoveryOutletTemp( 0.0 ),
			HeatRecoveryMassFlowRate( 0.0 ),
			DehumidInducedHeatingDemandRate( 0.0 ),
			EMSSensibleZoneLoadValue( 0.0 ),
			EMSMoistureZoneLoadValue( 0.0 ),
			SpeedNum( 0 ),
			SpeedRatio( 0.0 ),
			CycRatio( 0.0 ),
			HXAssistedSensPLRIter( 0 ),
			HXAssistedSensPLRIterIndex( 0 ),
			HXAssistedSensPLRFail( 0 ),
			HXAssistedSensPLRFailIndex( 0 ),
			HXAssistedSensPLRFail2( 0 ),
			HXAssistedSensPLRFailIndex2( 0 ),
			HXAssistedLatPLRIter( 0 ),
			HXAssistedLatPLRIterIndex( 0 ),
			HXAssistedLatPLRFail( 0 ),
			HXAssistedLatPLRFailIndex( 0 ),
			HXAssistedCRLatPLRIter( 0 ),
			HXAssistedCRLatPLRIterIndex( 0 ),
			HXAssistedCRLatPLRFail( 0 ),
			HXAssistedCRLatPLRFailIndex( 0 ),
			HXAssistedCRLatPLRFail2( 0 ),
			HXAssistedCRLatPLRFailIndex2( 0 ),
			SensPLRIter( 0 ),
			SensPLRIterIndex( 0 ),
			SensPLRFail( 0 ),
			SensPLRFailIndex( 0 ),
			LatPLRIter( 0 ),
			LatPLRIterIndex( 0 ),
			LatPLRFail( 0 ),
			LatPLRFailIndex( 0 ),
			HeatCoilSensPLRIter( 0 ),
			HeatCoilSensPLRIterIndex( 0 ),
			HeatCoilSensPLRFail( 0 ),
			HeatCoilSensPLRFailIndex( 0 ),
			SuppHeatCoilSensPLRIter( 0 ),
			SuppHeatCoilSensPLRIterIndex( 0 ),
			SuppHeatCoilSensPLRFail( 0 ),
			SuppHeatCoilSensPLRFailIndex( 0 ),
			DXCoilSensPLRIter( 0 ),
			DXCoilSensPLRIterIndex( 0 ),
			DXCoilSensPLRFail( 0 ),
			DXCoilSensPLRFailIndex( 0 ),
			MSpdSensPLRIter( 0 ),
			MSpdSensPLRIterIndex( 0 ),
			MSpdCycSensPLRIter( 0 ),
			MSpdCycSensPLRIterIndex( 0 ),
			MSpdLatPLRIter( 0 ),
			MSpdLatPLRIterIndex( 0 ),
			MSpdCycLatPLRIter( 0 ),
			MSpdCycLatPLRIterIndex( 0 ),
			MaxIterIndex( 0 ),
			RegulaFalsIFailedIndex( 0 ),
			LatMaxIterIndex( 0 ),
			LatRegulaFalsIFailedIndex( 0 ),
			DesignFanVolFlowRateEMSOverrideOn( false ),
			MaxHeatAirVolFlowEMSOverrideOn( false ),
			MaxCoolAirVolFlowEMSOverrideOn( false ),
			MaxNoCoolHeatAirVolFlowEMSOverrideOn( false ),
			DesignFanVolFlowRateEMSOverrideValue( 0.0 ),
			MaxHeatAirVolFlowEMSOverrideValue( 0.0 ),
			MaxCoolAirVolFlowEMSOverrideValue( 0.0 ),
			MaxNoCoolHeatAirVolFlowEMSOverrideValue( 0.0 ),
			EMSOverrideSensZoneLoadRequest( false ),
			EMSOverrideMoistZoneLoadRequest( false ),
			StageNum( 0 ),
			Staged( false ),
			CoolCountAvail( 0 ),
			CoolIndexAvail( 0 ),
			HeatCountAvail( 0 ),
			HeatIndexAvail( 0 )
		{}

		// Member Constructor
		UnitarySystemData(
			std::string const & UnitarySystemType, // Type of Unitary System
			int const UnitarySystemType_Num, // integer type of Unitary System
			std::string const & Name, // Name of the Unitary System
			bool const HeatPump, // TRUE if both cooling and heating coils are DX
			int const SysAvailSchedPtr, // System Availability schedule
			int const UnitarySystemInletNodeNum, // Parent inlet node number
			int const UnitarySystemOutletNodeNum, // Parent outlet node number
			int const CondenserType, // type of condenser (AirCooled, EvapCooled, WaterCooled)
			bool const AirLoopEquipment, // identifies that this system is part of an air loop
			int const ControlZoneNum, // Index to controlled zone
			int const ZoneSequenceCoolingNum, // Index to cooling sequence/priority for this zone
			int const ZoneSequenceHeatingNum, // Index to heating sequence/priority for this zone
			int const NodeNumOfControlledZone, // Node number of controlled zone
			int const ZoneInletNode, // Zone inlet node number in the controlled zone
			Real64 const ControlZoneMassFlowFrac, // Fraction of flow to control zone
			bool const Humidistat, // Set to True if dehumidification control mode is set to
			Real64 const DesignMaxOutletTemp, // Maximum supply air temperature from heater [C]
			int const CondenserNodeNum, // index to condenser air inlet node
			int const DehumidControlType_Num, // Set to Dehumid Control None, CoolReheat or MultiMode
			int const AirFlowControl, // UseCompressorOnFlow or UseCompressorOffFlow
			int const ControlType, // Setpoint or Load based control
			bool const RequestAutoSize, // determines if inputs need autosizing
			bool const RunOnSensibleLoad, // logical determines if this system will run to
			bool const RunOnLatentLoad, // logical determines if this system will run to
			bool const RunOnLatentOnlyWithSensible, // allow latent dehumidification only if sensible load exists
			int const DehumidificationMode, // Dehumidification mode for multimode coil,
			int const FanOpMode, // Fan operating mode (see parameter above)
			int const LastMode, // last mode of operation, coolingmode or heatingmode
			Real64 const AncillaryOnPower, // Ancillary On-Cycle Electric Power [W]
			Real64 const AncillaryOffPower, // Ancillary Off-Cycle Electric Power [W]
			std::string const & DesignSpecMultispeedHPType, // Object type for specifying multispeed flow rates
			std::string const & DesignSpecMultispeedHPName, // Object name for specifying multispeed flow rates
			std::string const & CoolingCoilName, // coil name (eliminate after blank is accepted in CALL)
			int const CoolingCoilType_Num, // numeric coil type
			int const CoolingCoilIndex, // index to specific cooling coil
			Real64 const DesignCoolingCapacity, // cooling coil capacity (W)
			int const CoolingCoilAvailSchPtr, // cooling coil availability schedule index
			int const ActualDXCoilIndexForHXAssisted, // index to DX coil used in HX assisted object
			Real64 const DOASDXCoolingCoilMinTout, // DOAS DX Cooling coil outlet air minimum temperature
			bool const ISHundredPercentDOASDXCoil, // logical determines if this system will run as 100% DOAS
			bool const CoolCoilExists, // True if a cooling coil is specified in the unitary system
			int const FrostControlStatus, // DOAS coil system frost control status
			int const CoolCoilInletNodeNum, // Cooling coil air inlet node number
			int const CoolCoilOutletNodeNum, // Cooling coil air outlet node number
			int const CoolCoilFluidOutletNodeNum, // Cooling coil fluid outlet node number (from Plant Loop data)
			int const CoolCoilLoopNum, // Plant loop num of chilled water coil
			int const CoolCoilLoopSide, // Supply side or demand side
			int const CoolCoilBranchNum, // Branch of number of the cooling coil in the plant loop
			int const CoolCoilCompNum, // Comp num of the cooling coil in the plant loop
			int const CoolCoilFluidInletNode, // Cooling coil fluid inlet node
			Real64 const MaxCoolCoilFluidFlow, // Maximum cooling coil fluid flow for chilled water coil
			bool const CoolingCoilUpstream, // Set to true when coolign coil is upstream in the unitary system
			Real64 const CoolCompPartLoadRatio, // Unitary system compressor part load ratio in cooling
			std::string const & HeatingCoilName, // coil name (eliminate after blank is accepted in CALL)
			int const HeatingCoilType_Num, // numeric coil type
			int const HeatingCoilIndex, // index to specific heating coil
			Real64 const DesignHeatingCapacity, // heating coil capacity (W)
			Real64 const HeatingSizingRatio, // ratio of heating coil to cooling coil size
			bool const DXHeatingCoil, // specifies if heating coil is DX
			int const HeatCoilInletNodeNum, // Heating coil air inlet node number
			int const HeatCoilOutletNodeNum, // Heating coil air outlet node number
			int const HeatCoilFluidOutletNodeNum, // Heating coil fluid outlet node number (from Plant Loop data)
			int const HeatingCoilPLFCurveIndex, // PLF curve index (not used yet?)
			int const HeatingCoilAvailSchPtr, // heating coil availability schedule index
			bool const HeatCoilExists, // True if a heating coil is specified in the unitary system
			int const HeatCoilLoopNum, // Plant loop num of hot water or steam coil
			int const HeatCoilLoopSide, // Supply side or demand side
			int const HeatCoilBranchNum, // Branch of number of the heating coil in the plant loop
			int const HeatCoilCompNum, // Comp num of the heating coil in the plant loop
			int const HeatCoilFluidInletNode, // Heating coil fluid inlet node
			Real64 const MaxHeatCoilFluidFlow, // Maximum heating coil fluid flow for hot water or steam coil
			Real64 const HeatCompPartLoadRatio, // Unitary system compressor part load ratio in heating
			std::string const & SuppHeatCoilName, // coil name (eliminate after blank is accepted in CALL)
			int const SuppHeatCoilType_Num, // numeric coil type
			int const SuppHeatCoilIndex, // index to specific supplemental heating coil
			Real64 const DesignSuppHeatingCapacity, // supplemental heating coil capacity (W)
			int const SuppCoilFluidInletNode, // supplemental heating coil water/steam inlet node
			int const SuppCoilFluidOutletNodeNum, // Supplemental coil fluid outlet node number (from Plant Loop data)
			int const SuppCoilAirInletNode, // supplemental heating coil air inlet node
			int const SuppCoilAirOutletNode, // supplemental heating coil air outlet node
			int const SuppCoilAvailSchPtr, // supplemental heating coil availability schedule index
			Real64 const MaxSuppCoilFluidFlow, // supplemental heating coil maximum water/steam flow rate (m3/s)
			Real64 const MaxOATSuppHeat, // Maximum outdoor dry-bulb temperature for supplemental coil [C]
			bool const SuppCoilExists, // True if a supp coil is specified in the unitary system
			int const SuppCoilLoopNum, // Plant loop num of supplemental coil
			int const SuppCoilLoopSide, // Supply side or demand side
			int const SuppCoilBranchNum, // Branch of number of the supplemental coil in the plant loop
			int const SuppCoilCompNum, // Comp num of the supplemental coil in the plant loop
			int const FanType_Num, // Fan type num i.e. OnOff, ConstVol, VAV
			int const FanIndex, // index of fan of a particular type
			Real64 const ActualFanVolFlowRate, // Actual or design fan volume flow rate
			int const FanOpModeSchedPtr, // fan operating mode schedule pointer
			int const FanAvailSchedPtr, // fan availability schedule pointer
			int const FanPlace, // Blow through or DrawThrough Fan
			bool const FanExists, // True if a fan is specified in the unitary system
			Real64 const FanDelayTime, // Fan delay time, time delay for the HP's fan to
			Real64 const MaxCoolAirVolFlow, // Maximum coil air volumetric flow for cooling [m3/s]
			Real64 const MaxHeatAirVolFlow, // Maximum coil air volumetric flow for heating [m3/s]
			Real64 const MaxNoCoolHeatAirVolFlow, // Maximum coil air volumetric flow for no cooling or heating [m3/s]
			Real64 const DesignFanVolFlowRate, // Design fan volume flow rate [m3/s]
			Real64 const DesignMassFlowRate, // Design mass flow rate [m3/s]
			Real64 const MaxCoolAirMassFlow, // Maximum coil air mass flow for cooling [kg/s]
			Real64 const MaxHeatAirMassFlow, // Maximum coil air mass flow for heating [kg/s]
			Real64 const MaxNoCoolHeatAirMassFlow, // Maximum coil air mass flow for no cooling or heating [kg/s]
			int const CoolingSAFMethod, // Supply air flow method for cooling
			int const HeatingSAFMethod, // Supply air flow method for heating
			int const NoCoolHeatSAFMethod, // Supply air flow method for no cooling or heating
			Real64 const MinOATCompressor, // Minimum outdoor temperature below which compressor if off
			Real64 const MaxONOFFCyclesperHour, // Maximum cycling rate of unitary system [cycles/hr]
			Real64 const HPTimeConstant, // Heat pump time constant [s]
			Real64 const OnCyclePowerFraction, // Fraction of on-cycle power use [~]
			Real64 const DesignHRWaterVolumeFlow, // Design water volume flow rate through heat recovery loop [m3/s]
			Real64 const WSHPRuntimeFrac, // Runtime fraction of water source heat pump
			Real64 const HeatingCoilSensDemand, // Sensible demand on Heating Coil [W]
			Real64 const CoolingCoilSensDemand, // Sensible demand on Cooling Coil [W]
			Real64 const CoolingCoilLatentDemand, // Latent demand on Cooling Coil [W]
			int const HeatRecoveryInletNodeNum, // Node number on heat recovery water inlet
			int const HeatRecoveryOutletNodeNum, // Node number on heat recovery water outlet
			bool const HeatRecActive, // True when entered Heat Rec Vol Flow Rate > 0
			Real64 const DesignHeatRecMassFlowRate, // Design water mass flow rate through heat recovery loop [kg/s]
			Real64 const MaxHROutletWaterTemp, // Maximum outlet water temperature for heat recovery [C]
			int const HRLoopNum, // plant loop number for heat recovery
			int const HRLoopSideNum, // Plant loop side (supply or demand) for heat recovery
			int const HRBranchNum, // plant loop branch for heat recovery
			int const HRCompNum, // plant loop component for heat recovery
			int const SystemHeatControlNodeNum, // the node number of the node with the setpoint
			int const SystemCoolControlNodeNum, // the node number of the node with the setpoint
			int const SuppHeatControlNodeNum, // the node number of the node with the setpoint
			Real64 const DesiredOutletTemp, // the setpoint temperature at the unit outlet node
			Real64 const DesiredOutletHumRat, // the setpoint humidity ratio at the unit outlet node
			Real64 const CoolingPartLoadFrac, // part load cooling fraction for current timestep
			Real64 const HeatingPartLoadFrac, // part load heating fraction for current timestep
			Real64 const SuppHeatPartLoadFrac, // part load supp heating fraction for current timestep
			Real64 const SupHeaterLoad, // Supplemental Heat Load for current timestep
			Real64 const SenLoadLoss, // Air distribution system sensible loss [W]
			Real64 const LatLoadLoss, // Air distribution system latent loss [W]
			Real64 const SensibleLoadMet, // System sensible load [W]
			Real64 const LatentLoadMet, // System latent load [W]
			bool const InitHeatPump, // Heat pump initialization flag (for error reporting)
			int const WaterCyclingMode, // Heat Pump Coil water flow mode; See def in DataHVACGlobals,
			int const HeatCoolMode, // System operating mode (0 = floating, 1 = cooling, 2 = heating)
			int const NumOfSpeedCooling, // The number of speeds for cooling
			int const NumOfSpeedHeating, // The number of speeds for heating
			Real64 const IdleSpeedRatio, // idle air fan ratio
			Real64 const IdleVolumeAirRate, // idle air flow rate [m3/s]
			Real64 const IdleMassFlowRate, // idle air flow rate [kg/s]
			bool const CheckFanFlow, // Supply airflow check
			Array1< Real64 > const & HeatVolumeFlowRate, // Supply air volume flow rate during heating operation
			Array1< Real64 > const & HeatMassFlowRate, // Supply air mass flow rate during heating operation
			Array1< Real64 > const & CoolVolumeFlowRate, // Supply air volume flow rate during cooling operation
			Array1< Real64 > const & CoolMassFlowRate, // Supply air mass flow rate during cooling operation
			Array1< Real64 > const & MSHeatingSpeedRatio, // Fan speed ratio in heating mode
			Array1< Real64 > const & MSCoolingSpeedRatio, // Fan speed ratio in cooling mode
			Real64 const NoHeatCoolSpeedRatio, // Fan speed ratio when no cooling or heating
			int const DesignSpecMSHPIndex, // Index to design specification multispeed heat pump object
			bool const MultiSpeedCoolingCoil, // TRUE when cooling coil multispeed
			bool const MultiSpeedHeatingCoil, // TRUE when heating coil multispeed
			bool const VarSpeedCoolingCoil, // TRUE when cooling coil variable speed
			bool const VarSpeedHeatingCoil, // TRUE when heating coil variable speed
			int const CoolingSpeedNum, // speed number for multispeed cooling coils types
			int const HeatingSpeedNum, // speed number for multispeed heating coils types
			Real64 const CoolingSpeedRatio, // current compressor speed ratio (variable speed)
			Real64 const CoolingFanSpeedRatio, // current fan speed ratio
			Real64 const HeatingSpeedRatio, // current compressor speed ratio (variable speed)
			Real64 const HeatingFanSpeedRatio, // current fan speed ratio
			Real64 const CoolingCycRatio, // cycling part load ratio (variable speed)
			Real64 const HeatingCycRatio, // cycling part load ratio (variable speed)
			Real64 const PartLoadFrac, // part load fraction for current time step (single speed)
			Real64 const FanPartLoadRatio, // Unitary system fan part load ratio
			Real64 const CompPartLoadRatio, // Unitary system compressor part load ratio
			Real64 const ElecPower, // Unitary System Electric Power
			Real64 const ElecPowerConsumption, // Electricity power comsumption: CondenserFan+CCHeater+Defrost+aux
			Real64 const TotCoolEnergyRate, // Unitary System Total Cooling Rate [W]
			Real64 const SensCoolEnergyRate, // Unitary System Sensible Cooling Rate [W]
			Real64 const LatCoolEnergyRate, // Unitary System Latent Cooling Rate [W]
			Real64 const TotHeatEnergyRate, // Unitary System Total Heating Rate [W]
			Real64 const SensHeatEnergyRate, // Unitary System Sensible Heating Rate [W]
			Real64 const LatHeatEnergyRate, // Unitary System Latent Heating Rate [W]
			Real64 const TotalAuxElecPower, // Unitary System Ancillary Electric Power [W]
			Real64 const HeatingAuxElecConsumption, // Unitary System Heating Ancillary Electric Energy [J]
			Real64 const CoolingAuxElecConsumption, // Unitary System Cooling Ancillary Electric Energy [J]
			Real64 const HeatRecoveryRate, // Unitary System Heat Recovery Rate [W]
			Real64 const HeatRecoveryEnergy, // Unitary System Heat Recovery Energy [J]
			Real64 const HeatRecoveryInletTemp, // Unitary System Heat Recovery Inlet Temperature [C]
			Real64 const HeatRecoveryOutletTemp, // Unitary System Heat Recovery Outlet Temperature [C]
			Real64 const HeatRecoveryMassFlowRate, // Unitary System Heat Recovery Fluid Mass Flow Rate [kg/s]
			Real64 const DehumidInducedHeatingDemandRate, // Unitary System
			Real64 const EMSSensibleZoneLoadValue, // Value EMS is directing to use
			Real64 const EMSMoistureZoneLoadValue, // Value EMS is directing to use
			int const SpeedNum, // speed number of active multi- or variable-speed coil
			Real64 const SpeedRatio, // current compressor speed ratio (variable speed)
			Real64 const CycRatio, // cycling part load ratio (variable speed)
			int const HXAssistedSensPLRIter, // used in HX Assisted calculations
			int const HXAssistedSensPLRIterIndex, // used in HX Assisted calculations
			int const HXAssistedSensPLRFail, // used in HX Assisted calculations
			int const HXAssistedSensPLRFailIndex, // used in HX Assisted calculations
			int const HXAssistedSensPLRFail2, // used in HX Assisted calculations
			int const HXAssistedSensPLRFailIndex2, // used in HX Assisted calculations
			int const HXAssistedLatPLRIter, // used in HX Assisted calculations
			int const HXAssistedLatPLRIterIndex, // used in HX Assisted calculations
			int const HXAssistedLatPLRFail, // used in HX Assisted calculations
			int const HXAssistedLatPLRFailIndex, // used in HX Assisted calculations
			int const HXAssistedCRLatPLRIter, // used in HX Assisted calculations
			int const HXAssistedCRLatPLRIterIndex, // used in HX Assisted calculations
			int const HXAssistedCRLatPLRFail, // used in HX Assisted calculations
			int const HXAssistedCRLatPLRFailIndex, // used in HX Assisted calculations
			int const HXAssistedCRLatPLRFail2, // used in HX Assisted calculations
			int const HXAssistedCRLatPLRFailIndex2, // used in HX Assisted calculations
			int const SensPLRIter, // used in cool coil calculations
			int const SensPLRIterIndex, // used in cool coil calculations
			int const SensPLRFail, // used in cool coil calculations
			int const SensPLRFailIndex, // used in cool coil calculations
			int const LatPLRIter, // used in cool coil calculations
			int const LatPLRIterIndex, // used in cool coil calculations
			int const LatPLRFail, // used in cool coil calculations
			int const LatPLRFailIndex, // used in cool coil calculations
			int const HeatCoilSensPLRIter, // used in heat coil calculations
			int const HeatCoilSensPLRIterIndex, // used in heat coil calculations
			int const HeatCoilSensPLRFail, // used in heat coil calculations
			int const HeatCoilSensPLRFailIndex, // used in heat coil calculations
			int const SuppHeatCoilSensPLRIter, // used in supp heat coil calculations
			int const SuppHeatCoilSensPLRIterIndex, // used in supp heat coil calculations
			int const SuppHeatCoilSensPLRFail, // used in supp heat coil calculations
			int const SuppHeatCoilSensPLRFailIndex, // used in supp heat coil calculations
			int const DXCoilSensPLRIter, // used in DXCoil calculations
			int const DXCoilSensPLRIterIndex, // used in DXCoil calculations
			int const DXCoilSensPLRFail, // used in DXCoil calculations
			int const DXCoilSensPLRFailIndex, // used in DXCoil calculations
			int const MSpdSensPLRIter, // used in MultiSpeed calculations
			int const MSpdSensPLRIterIndex, // used in MultiSpeed calculations
			int const MSpdCycSensPLRIter, // used in MultiSpeed calculations
			int const MSpdCycSensPLRIterIndex, // used in MultiSpeed calculations
			int const MSpdLatPLRIter, // used in MultiSpeed calculations
			int const MSpdLatPLRIterIndex, // used in MultiSpeed calculations
			int const MSpdCycLatPLRIter, // used in MultiSpeed calculations
			int const MSpdCycLatPLRIterIndex, // used in MultiSpeed calculations
			int const MaxIterIndex, // used in PLR calculations for sensible load
			int const RegulaFalsIFailedIndex, // used in PLR calculations for sensible load
			int const LatMaxIterIndex, // used in PLR calculations for moisture load
			int const LatRegulaFalsIFailedIndex, // used in PLR calculations for moisture load
			bool const DesignFanVolFlowRateEMSOverrideOn, // If true, then EMS is calling to override autosize fan flow
			bool const MaxHeatAirVolFlowEMSOverrideOn, // If true, then EMS is calling to override autosize fan flow
			bool const MaxCoolAirVolFlowEMSOverrideOn, // If true, then EMS is calling to override autosize fan flow
			bool const MaxNoCoolHeatAirVolFlowEMSOverrideOn, // If true, then EMS is calling to override autosize fan flow
			Real64 const DesignFanVolFlowRateEMSOverrideValue, // EMS value for override of fan flow rate autosize [m3/s]
			Real64 const MaxHeatAirVolFlowEMSOverrideValue, // EMS value for override of fan flow rate autosize [m3/s]
			Real64 const MaxCoolAirVolFlowEMSOverrideValue, // EMS value for override of fan flow rate autosize [m3/s]
			Real64 const MaxNoCoolHeatAirVolFlowEMSOverrideValue, // EMS value for override of fan flow rate autosize [m3/s]
			bool const EMSOverrideSensZoneLoadRequest, // If true, then EMS is calling to override zone load
			bool const EMSOverrideMoistZoneLoadRequest, // If true, then EMS is calling to override zone load
			int const StageNum, // Stage number specified by staged thermostat
			bool const Staged, // Using Staged thermostat
			int const CoolCountAvail, // Counter used to minimize the occurrence of output warnings
			int const CoolIndexAvail, // Index used to minimize the occurrence of output warnings
			int const HeatCountAvail, // Counter used to minimize the occurrence of output warnings
			int const HeatIndexAvail // Index used to minimize the occurrence of output warnings
		) :
			UnitarySystemType( UnitarySystemType ),
			UnitarySystemType_Num( UnitarySystemType_Num ),
			Name( Name ),
			HeatPump( HeatPump ),
			SysAvailSchedPtr( SysAvailSchedPtr ),
			UnitarySystemInletNodeNum( UnitarySystemInletNodeNum ),
			UnitarySystemOutletNodeNum( UnitarySystemOutletNodeNum ),
			CondenserType( CondenserType ),
			AirLoopEquipment( AirLoopEquipment ),
			ControlZoneNum( ControlZoneNum ),
			ZoneSequenceCoolingNum( ZoneSequenceCoolingNum ),
			ZoneSequenceHeatingNum( ZoneSequenceHeatingNum ),
			NodeNumOfControlledZone( NodeNumOfControlledZone ),
			ZoneInletNode( ZoneInletNode ),
			ControlZoneMassFlowFrac( ControlZoneMassFlowFrac ),
			Humidistat( Humidistat ),
			DesignMaxOutletTemp( DesignMaxOutletTemp ),
			CondenserNodeNum( CondenserNodeNum ),
			DehumidControlType_Num( DehumidControlType_Num ),
			AirFlowControl( AirFlowControl ),
			ControlType( ControlType ),
			RequestAutoSize( RequestAutoSize ),
			RunOnSensibleLoad( RunOnSensibleLoad ),
			RunOnLatentLoad( RunOnLatentLoad ),
			RunOnLatentOnlyWithSensible( RunOnLatentOnlyWithSensible ),
			DehumidificationMode( DehumidificationMode ),
			FanOpMode( FanOpMode ),
			LastMode( LastMode ),
			AncillaryOnPower( AncillaryOnPower ),
			AncillaryOffPower( AncillaryOffPower ),
			DesignSpecMultispeedHPType( DesignSpecMultispeedHPType ),
			DesignSpecMultispeedHPName( DesignSpecMultispeedHPName ),
			CoolingCoilName( CoolingCoilName ),
			CoolingCoilType_Num( CoolingCoilType_Num ),
			CoolingCoilIndex( CoolingCoilIndex ),
			DesignCoolingCapacity( DesignCoolingCapacity ),
			CoolingCoilAvailSchPtr( CoolingCoilAvailSchPtr ),
			ActualDXCoilIndexForHXAssisted( ActualDXCoilIndexForHXAssisted ),
			DOASDXCoolingCoilMinTout( DOASDXCoolingCoilMinTout ),
			ISHundredPercentDOASDXCoil( ISHundredPercentDOASDXCoil ),
			CoolCoilExists( CoolCoilExists ),
			FrostControlStatus( FrostControlStatus ),
			CoolCoilInletNodeNum( CoolCoilInletNodeNum ),
			CoolCoilOutletNodeNum( CoolCoilOutletNodeNum ),
			CoolCoilFluidOutletNodeNum( CoolCoilFluidOutletNodeNum ),
			CoolCoilLoopNum( CoolCoilLoopNum ),
			CoolCoilLoopSide( CoolCoilLoopSide ),
			CoolCoilBranchNum( CoolCoilBranchNum ),
			CoolCoilCompNum( CoolCoilCompNum ),
			CoolCoilFluidInletNode( CoolCoilFluidInletNode ),
			MaxCoolCoilFluidFlow( MaxCoolCoilFluidFlow ),
			CoolingCoilUpstream( CoolingCoilUpstream ),
			CoolCompPartLoadRatio( CoolCompPartLoadRatio ),
			HeatingCoilName( HeatingCoilName ),
			HeatingCoilType_Num( HeatingCoilType_Num ),
			HeatingCoilIndex( HeatingCoilIndex ),
			DesignHeatingCapacity( DesignHeatingCapacity ),
			HeatingSizingRatio( HeatingSizingRatio ),
			DXHeatingCoil( DXHeatingCoil ),
			HeatCoilInletNodeNum( HeatCoilInletNodeNum ),
			HeatCoilOutletNodeNum( HeatCoilOutletNodeNum ),
			HeatCoilFluidOutletNodeNum( HeatCoilFluidOutletNodeNum ),
			HeatingCoilPLFCurveIndex( HeatingCoilPLFCurveIndex ),
			HeatingCoilAvailSchPtr( HeatingCoilAvailSchPtr ),
			HeatCoilExists( HeatCoilExists ),
			HeatCoilLoopNum( HeatCoilLoopNum ),
			HeatCoilLoopSide( HeatCoilLoopSide ),
			HeatCoilBranchNum( HeatCoilBranchNum ),
			HeatCoilCompNum( HeatCoilCompNum ),
			HeatCoilFluidInletNode( HeatCoilFluidInletNode ),
			MaxHeatCoilFluidFlow( MaxHeatCoilFluidFlow ),
			HeatCompPartLoadRatio( HeatCompPartLoadRatio ),
			SuppHeatCoilName( SuppHeatCoilName ),
			SuppHeatCoilType_Num( SuppHeatCoilType_Num ),
			SuppHeatCoilIndex( SuppHeatCoilIndex ),
			DesignSuppHeatingCapacity( DesignSuppHeatingCapacity ),
			SuppCoilFluidInletNode( SuppCoilFluidInletNode ),
			SuppCoilFluidOutletNodeNum( SuppCoilFluidOutletNodeNum ),
			SuppCoilAirInletNode( SuppCoilAirInletNode ),
			SuppCoilAirOutletNode( SuppCoilAirOutletNode ),
			SuppCoilAvailSchPtr( SuppCoilAvailSchPtr ),
			MaxSuppCoilFluidFlow( MaxSuppCoilFluidFlow ),
			MaxOATSuppHeat( MaxOATSuppHeat ),
			SuppCoilExists( SuppCoilExists ),
			SuppCoilLoopNum( SuppCoilLoopNum ),
			SuppCoilLoopSide( SuppCoilLoopSide ),
			SuppCoilBranchNum( SuppCoilBranchNum ),
			SuppCoilCompNum( SuppCoilCompNum ),
			FanType_Num( FanType_Num ),
			FanIndex( FanIndex ),
			ActualFanVolFlowRate( ActualFanVolFlowRate ),
			FanOpModeSchedPtr( FanOpModeSchedPtr ),
			FanAvailSchedPtr( FanAvailSchedPtr ),
			FanPlace( FanPlace ),
			FanExists( FanExists ),
			FanDelayTime( FanDelayTime ),
			MaxCoolAirVolFlow( MaxCoolAirVolFlow ),
			MaxHeatAirVolFlow( MaxHeatAirVolFlow ),
			MaxNoCoolHeatAirVolFlow( MaxNoCoolHeatAirVolFlow ),
			DesignFanVolFlowRate( DesignFanVolFlowRate ),
			DesignMassFlowRate( DesignMassFlowRate ),
			MaxCoolAirMassFlow( MaxCoolAirMassFlow ),
			MaxHeatAirMassFlow( MaxHeatAirMassFlow ),
			MaxNoCoolHeatAirMassFlow( MaxNoCoolHeatAirMassFlow ),
			CoolingSAFMethod( CoolingSAFMethod ),
			HeatingSAFMethod( HeatingSAFMethod ),
			NoCoolHeatSAFMethod( NoCoolHeatSAFMethod ),
			MinOATCompressor( MinOATCompressor ),
			MaxONOFFCyclesperHour( MaxONOFFCyclesperHour ),
			HPTimeConstant( HPTimeConstant ),
			OnCyclePowerFraction( OnCyclePowerFraction ),
			DesignHRWaterVolumeFlow( DesignHRWaterVolumeFlow ),
			WSHPRuntimeFrac( WSHPRuntimeFrac ),
			HeatingCoilSensDemand( HeatingCoilSensDemand ),
			CoolingCoilSensDemand( CoolingCoilSensDemand ),
			CoolingCoilLatentDemand( CoolingCoilLatentDemand ),
			HeatRecoveryInletNodeNum( HeatRecoveryInletNodeNum ),
			HeatRecoveryOutletNodeNum( HeatRecoveryOutletNodeNum ),
			HeatRecActive( HeatRecActive ),
			DesignHeatRecMassFlowRate( DesignHeatRecMassFlowRate ),
			MaxHROutletWaterTemp( MaxHROutletWaterTemp ),
			HRLoopNum( HRLoopNum ),
			HRLoopSideNum( HRLoopSideNum ),
			HRBranchNum( HRBranchNum ),
			HRCompNum( HRCompNum ),
			SystemHeatControlNodeNum( SystemHeatControlNodeNum ),
			SystemCoolControlNodeNum( SystemCoolControlNodeNum ),
			SuppHeatControlNodeNum( SuppHeatControlNodeNum ),
			DesiredOutletTemp( DesiredOutletTemp ),
			DesiredOutletHumRat( DesiredOutletHumRat ),
			CoolingPartLoadFrac( CoolingPartLoadFrac ),
			HeatingPartLoadFrac( HeatingPartLoadFrac ),
			SuppHeatPartLoadFrac( SuppHeatPartLoadFrac ),
			SupHeaterLoad( SupHeaterLoad ),
			SenLoadLoss( SenLoadLoss ),
			LatLoadLoss( LatLoadLoss ),
			SensibleLoadMet( SensibleLoadMet ),
			LatentLoadMet( LatentLoadMet ),
			InitHeatPump( InitHeatPump ),
			WaterCyclingMode( WaterCyclingMode ),
			HeatCoolMode( HeatCoolMode ),
			NumOfSpeedCooling( NumOfSpeedCooling ),
			NumOfSpeedHeating( NumOfSpeedHeating ),
			IdleSpeedRatio( IdleSpeedRatio ),
			IdleVolumeAirRate( IdleVolumeAirRate ),
			IdleMassFlowRate( IdleMassFlowRate ),
			CheckFanFlow( CheckFanFlow ),
			HeatVolumeFlowRate( HeatVolumeFlowRate ),
			HeatMassFlowRate( HeatMassFlowRate ),
			CoolVolumeFlowRate( CoolVolumeFlowRate ),
			CoolMassFlowRate( CoolMassFlowRate ),
			MSHeatingSpeedRatio( MSHeatingSpeedRatio ),
			MSCoolingSpeedRatio( MSCoolingSpeedRatio ),
			NoHeatCoolSpeedRatio( NoHeatCoolSpeedRatio ),
			DesignSpecMSHPIndex( DesignSpecMSHPIndex ),
			MultiSpeedCoolingCoil( MultiSpeedCoolingCoil ),
			MultiSpeedHeatingCoil( MultiSpeedHeatingCoil ),
			VarSpeedCoolingCoil( VarSpeedCoolingCoil ),
			VarSpeedHeatingCoil( VarSpeedHeatingCoil ),
			CoolingSpeedNum( CoolingSpeedNum ),
			HeatingSpeedNum( HeatingSpeedNum ),
			CoolingSpeedRatio( CoolingSpeedRatio ),
			CoolingFanSpeedRatio( CoolingFanSpeedRatio ),
			HeatingSpeedRatio( HeatingSpeedRatio ),
			HeatingFanSpeedRatio( HeatingFanSpeedRatio ),
			CoolingCycRatio( CoolingCycRatio ),
			HeatingCycRatio( HeatingCycRatio ),
			PartLoadFrac( PartLoadFrac ),
			FanPartLoadRatio( FanPartLoadRatio ),
			CompPartLoadRatio( CompPartLoadRatio ),
			ElecPower( ElecPower ),
			ElecPowerConsumption( ElecPowerConsumption ),
			TotCoolEnergyRate( TotCoolEnergyRate ),
			SensCoolEnergyRate( SensCoolEnergyRate ),
			LatCoolEnergyRate( LatCoolEnergyRate ),
			TotHeatEnergyRate( TotHeatEnergyRate ),
			SensHeatEnergyRate( SensHeatEnergyRate ),
			LatHeatEnergyRate( LatHeatEnergyRate ),
			TotalAuxElecPower( TotalAuxElecPower ),
			HeatingAuxElecConsumption( HeatingAuxElecConsumption ),
			CoolingAuxElecConsumption( CoolingAuxElecConsumption ),
			HeatRecoveryRate( HeatRecoveryRate ),
			HeatRecoveryEnergy( HeatRecoveryEnergy ),
			HeatRecoveryInletTemp( HeatRecoveryInletTemp ),
			HeatRecoveryOutletTemp( HeatRecoveryOutletTemp ),
			HeatRecoveryMassFlowRate( HeatRecoveryMassFlowRate ),
			DehumidInducedHeatingDemandRate( DehumidInducedHeatingDemandRate ),
			EMSSensibleZoneLoadValue( EMSSensibleZoneLoadValue ),
			EMSMoistureZoneLoadValue( EMSMoistureZoneLoadValue ),
			SpeedNum( SpeedNum ),
			SpeedRatio( SpeedRatio ),
			CycRatio( CycRatio ),
			HXAssistedSensPLRIter( HXAssistedSensPLRIter ),
			HXAssistedSensPLRIterIndex( HXAssistedSensPLRIterIndex ),
			HXAssistedSensPLRFail( HXAssistedSensPLRFail ),
			HXAssistedSensPLRFailIndex( HXAssistedSensPLRFailIndex ),
			HXAssistedSensPLRFail2( HXAssistedSensPLRFail2 ),
			HXAssistedSensPLRFailIndex2( HXAssistedSensPLRFailIndex2 ),
			HXAssistedLatPLRIter( HXAssistedLatPLRIter ),
			HXAssistedLatPLRIterIndex( HXAssistedLatPLRIterIndex ),
			HXAssistedLatPLRFail( HXAssistedLatPLRFail ),
			HXAssistedLatPLRFailIndex( HXAssistedLatPLRFailIndex ),
			HXAssistedCRLatPLRIter( HXAssistedCRLatPLRIter ),
			HXAssistedCRLatPLRIterIndex( HXAssistedCRLatPLRIterIndex ),
			HXAssistedCRLatPLRFail( HXAssistedCRLatPLRFail ),
			HXAssistedCRLatPLRFailIndex( HXAssistedCRLatPLRFailIndex ),
			HXAssistedCRLatPLRFail2( HXAssistedCRLatPLRFail2 ),
			HXAssistedCRLatPLRFailIndex2( HXAssistedCRLatPLRFailIndex2 ),
			SensPLRIter( SensPLRIter ),
			SensPLRIterIndex( SensPLRIterIndex ),
			SensPLRFail( SensPLRFail ),
			SensPLRFailIndex( SensPLRFailIndex ),
			LatPLRIter( LatPLRIter ),
			LatPLRIterIndex( LatPLRIterIndex ),
			LatPLRFail( LatPLRFail ),
			LatPLRFailIndex( LatPLRFailIndex ),
			HeatCoilSensPLRIter( HeatCoilSensPLRIter ),
			HeatCoilSensPLRIterIndex( HeatCoilSensPLRIterIndex ),
			HeatCoilSensPLRFail( HeatCoilSensPLRFail ),
			HeatCoilSensPLRFailIndex( HeatCoilSensPLRFailIndex ),
			SuppHeatCoilSensPLRIter( SuppHeatCoilSensPLRIter ),
			SuppHeatCoilSensPLRIterIndex( SuppHeatCoilSensPLRIterIndex ),
			SuppHeatCoilSensPLRFail( SuppHeatCoilSensPLRFail ),
			SuppHeatCoilSensPLRFailIndex( SuppHeatCoilSensPLRFailIndex ),
			DXCoilSensPLRIter( DXCoilSensPLRIter ),
			DXCoilSensPLRIterIndex( DXCoilSensPLRIterIndex ),
			DXCoilSensPLRFail( DXCoilSensPLRFail ),
			DXCoilSensPLRFailIndex( DXCoilSensPLRFailIndex ),
			MSpdSensPLRIter( MSpdSensPLRIter ),
			MSpdSensPLRIterIndex( MSpdSensPLRIterIndex ),
			MSpdCycSensPLRIter( MSpdCycSensPLRIter ),
			MSpdCycSensPLRIterIndex( MSpdCycSensPLRIterIndex ),
			MSpdLatPLRIter( MSpdLatPLRIter ),
			MSpdLatPLRIterIndex( MSpdLatPLRIterIndex ),
			MSpdCycLatPLRIter( MSpdCycLatPLRIter ),
			MSpdCycLatPLRIterIndex( MSpdCycLatPLRIterIndex ),
			MaxIterIndex( MaxIterIndex ),
			RegulaFalsIFailedIndex( RegulaFalsIFailedIndex ),
			LatMaxIterIndex( LatMaxIterIndex ),
			LatRegulaFalsIFailedIndex( LatRegulaFalsIFailedIndex ),
			DesignFanVolFlowRateEMSOverrideOn( DesignFanVolFlowRateEMSOverrideOn ),
			MaxHeatAirVolFlowEMSOverrideOn( MaxHeatAirVolFlowEMSOverrideOn ),
			MaxCoolAirVolFlowEMSOverrideOn( MaxCoolAirVolFlowEMSOverrideOn ),
			MaxNoCoolHeatAirVolFlowEMSOverrideOn( MaxNoCoolHeatAirVolFlowEMSOverrideOn ),
			DesignFanVolFlowRateEMSOverrideValue( DesignFanVolFlowRateEMSOverrideValue ),
			MaxHeatAirVolFlowEMSOverrideValue( MaxHeatAirVolFlowEMSOverrideValue ),
			MaxCoolAirVolFlowEMSOverrideValue( MaxCoolAirVolFlowEMSOverrideValue ),
			MaxNoCoolHeatAirVolFlowEMSOverrideValue( MaxNoCoolHeatAirVolFlowEMSOverrideValue ),
			EMSOverrideSensZoneLoadRequest( EMSOverrideSensZoneLoadRequest ),
			EMSOverrideMoistZoneLoadRequest( EMSOverrideMoistZoneLoadRequest ),
			StageNum( StageNum ),
			Staged( Staged ),
			CoolCountAvail( CoolCountAvail ),
			CoolIndexAvail( CoolIndexAvail ),
			HeatCountAvail( HeatCountAvail ),
			HeatIndexAvail( HeatIndexAvail )
		{}

	};

	struct UnitarySystemNumericFieldData
	{
		// Members
		Array1D_string FieldNames;

		// Default Constructor
		UnitarySystemNumericFieldData()
		{}

		// Member Constructor
		UnitarySystemNumericFieldData(
			Array1_string const & FieldNames // Name of the UnitarySystem numeric field descriptions
		) :
			FieldNames( FieldNames )
		{}
	};

	// Object Data
	extern Array1D< DesignSpecMSHPData > DesignSpecMSHP;
	extern Array1D< UnitarySystemData > UnitarySystem;
	extern Array1D< UnitarySystemNumericFieldData > UnitarySystemNumericFields;

	// Functions

	void
	SimUnitarySystem(
		std::string const & UnitarySystemName, // Name of Unitary System object
		bool const FirstHVACIteration, // True when first HVAC iteration
		int const AirLoopNum, // Primary air loop number
		int & CompIndex, // Index to Unitary System object
		Optional_bool HeatActive = _, // True if heat coil active
		Optional_bool CoolActive = _, // True if cool coil active
		Optional_int_const OAUnitNum = _, // If the system is an equipment of OutdoorAirUnit
		Optional< Real64 const > OAUCoilOutTemp = _, // the coil inlet temperature of OutdoorAirUnit
		Optional_bool_const ZoneEquipment = _ // TRUE if called as zone equipment
	);

	// Beginning of Initialization subroutines for the Module
	// *****************************************************************************

	void
	InitUnitarySystems(
		int const UnitarySysNum, // number of the current DX Sys being simulated
		int const AirLoopNum, // number of the current air loop being simulated
		bool const FirstHVACIteration, // True when first HVAC iteration
		Optional_int_const OAUnitNum = _, // number of the current Outdoor air unit being simulated
		Optional< Real64 const > OAUCoilOutTemp = _ // the coil inlet temperature of OutdoorAirUnit
	);

	void
	CheckNodeSetPoint(
		int const UnitarySysNum, // number of the current DX Sys being simulated
		int const AirLoopNum, // number of the current air loop being simulated
		int const ControlNode, // Node to test for set point
		int const CoilType, // True if cooling coil, then test for HumRatMax set point
		Optional< Real64 const > OAUCoilOutTemp = _ // the coil inlet temperature of OutdoorAirUnit
	);

	void
	UpdateUnitarySystemControl(
		int const UnitarySysNum, // number of the current DX Sys being simulated
		int const AirLoopNum, // number of the current air loop being simulated
		int const OutNode, // coil outlet node number
		int const ControlNode, // control node number
		Real64 & OnOffAirFlowRatio,
		bool const FirstHVACIteration,
		Optional< Real64 const > OAUCoilOutletTemp = _, // "ONLY" for zoneHVAC:OutdoorAirUnit
		Optional< Real64 > ZoneLoad = _,
		Optional< Real64 const > MaxOutletTemp = _ // limits heating coil outlet temp [C]
	);

	void
	InitLoadBasedControl(
		int const UnitarySysNum, // number of the current DX Sys being simulated
		int const AirLoopNum, // number of the current air loop being simulated
		bool const FirstHVACIteration,
		Real64 & OnOffAirFlowRatio,
		Real64 & ZoneLoad
	);

	// End of Initialization subroutines for the Module
	// *****************************************************************************

	void
	SizeUnitarySystem(
		int const UnitarySysNum,
		bool const FirstHVACIteration,
		int const AirLoopNum // does this need to be optional?
	);

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetUnitarySystemInput();

	// End of Get Input subroutines for the Module
	//******************************************************************************

	// Beginning of Calculation subroutines for the DXCoolingSystem Module
	// *****************************************************************************

	void
	ControlUnitarySystemtoSP(
		int const UnitarySysNum, // Index of AirloopHVAC:UnitarySystem object
		bool const FirstHVACIteration, // True when first HVAC iteration
		int const AirLoopNum, // Primary air loop number
		Optional< Real64 const > OAUCoilOutTemp = _, // the coil inlet temperature of OutdoorAirUnit
		Optional_bool HXUnitOn = _ // Flag to control HX for HXAssisted Cooling Coil
	);

	void
	ControlUnitarySystemtoLoad(
		int const UnitarySysNum, // Index of AirloopHVAC:UnitarySystem object
		bool const FirstHVACIteration, // True when first HVAC iteration
		int const AirLoopNum, // Primary air loop number
		int & CompOn, // Determines if compressor is on or off
		Optional< Real64 const > OAUCoilOutTemp = _, // the coil inlet temperature of OutdoorAirUnit
		Optional_bool HXUnitOn = _ // Flag to control HX for HXAssisted Cooling Coil
	);

	void
	ControlUnitarySystemOutput(
		int const UnitarySysNum, // Index of AirloopHVAC:UnitarySystem object
		bool const FirstHVACIteration, // True when first HVAC iteration
		Real64 & OnOffAirFlowRatio, // ratio of heating PLR to cooling PLR (is this correct?)
		Real64 const ZoneLoad,
		Real64 & FullSensibleOutput,
		Optional_bool HXUnitOn = _, // Flag to control HX for HXAssisted Cooling Coil
		Optional_int CompOn = _
	);

	void
	SetSpeedVariables(
		int const UnitarySysNum, // Index of AirloopHVAC:UnitarySystem object
		bool const SensibleLoad, // True when meeting a sensible load (not a moisture load)
		Real64 const PartLoadRatio // operating PLR
	);

	Real64
	CalcUnitarySystemLoadResidual(
		Real64 const PartLoadRatio, // DX cooling coil part load ratio
		Array1< Real64 > const & Par // Function parameters
	);

	void
	CalcUnitarySystemToLoad(
		int const UnitarySysNum, // Index of AirloopHVAC:UnitarySystem object
		bool const FirstHVACIteration, // True when first HVAC iteration
		Real64 const CoolPLR, // operating cooling part-load ratio []
		Real64 const HeatPLR, // operating cooling part-load ratio []
		Real64 & OnOffAirFlowRatio, // ratio of heating PLR to cooling PLR (is this correct?)
		Real64 & SensOutput, // sensible capacity (W)
		Real64 & LatOutput, // latent capacity (W)
		Optional_bool HXUnitOn = _, // Flag to control HX for HXAssisted Cooling Coil
		Optional< Real64 > HeatCoilLoad = _, // Adjusted load to heating coil when SAT exceeds max limit (W)
		Optional< Real64 > SuppCoilLoad = _, // Adjusted load to supp heating coil when SAT exceeds max limit (W)
		Optional_int_const CompOn = _ // Determines if compressor is on or off
	);

	void
	CalcUnitaryCoolingSystem(
		int const UnitarySysNum, // Index of AirloopHVAC:UnitarySystem object
		bool const FirstHVACIteration, // True when first HVAC iteration
		Real64 const PartLoadRatio, // coil operating part-load ratio
		int const CompOn, // compressor control (0=off, 1=on)
		Real64 const OnOffAirFlowRatio,
		Real64 const CoilCoolHeatRat, // ratio of cooling to heating PLR for cycling fan RH control
		Optional_bool HXUnitOn = _ // Flag to control HX for HXAssisted Cooling Coil
	);

	void
	CalcUnitaryHeatingSystem(
		int const UnitarySysNum, // Index of AirloopHVAC:UnitarySystem object
		bool const FirstHVACIteration, // True when first HVAC iteration
		Real64 const PartLoadRatio, // coil operating part-load ratio
		int const CompOn, // comrpressor control (0=off, 1=on)
		Real64 const OnOffAirFlowRatio, // ratio of on to off flow rate
		Optional< Real64 const > HeatCoilLoad = _ // adjusted heating coil load if outlet temp exceeds max (W)
	);

	void
	CalcUnitarySuppHeatingSystem(
		int const UnitarySysNum, // Index of AirloopHVAC:UnitarySystem object
		bool const FirstHVACIteration, // True when first HVAC iteration
		Real64 const PartLoadRatio, // coil operating part-load ratio
		Optional< Real64 const > SuppCoilLoad = _ // adjusted supp coil load when outlet temp exceeds max (W)
	);

	void
	CalcUnitarySuppSystemToSP(
		int const UnitarySysNum, // Index of AirloopHVAC:UnitarySystem object
		bool const FirstHVACIteration // True when first HVAC iteration
	);

	void
	ControlCoolingSystem(
		int const UnitarySysNum, // index to Unitary System
		bool const FirstHVACIteration, // First HVAC iteration flag
		bool & HXUnitOn // flag to enable heat exchanger heat recovery
	);

	void
	ControlHeatingSystem(
		int const UnitarySysNum, // index to Unitary System
		bool const FirstHVACIteration // First HVAC iteration flag
	);

	void
	ControlSuppHeatSystem(
		int const UnitarySysNum, // index to Unitary System
		bool const FirstHVACIteration // First HVAC iteration flag
	);

	void
	SimWaterCoils(
		int const UnitarySysNum, // Index of AirloopHVAC:UnitarySystem object
		bool const FirstHVACIteration, // True when first HVAC iteration
		Real64 const PartLoadFrac,
		int const CoilType
	);

	void
	SimSteamCoils(
		int const UnitarySysNum, // Index of AirloopHVAC:UnitarySystem object
		bool const FirstHVACIteration, // True when first HVAC iteration
		Real64 const PartLoadFrac,
		int const CoilType
	);

	void
	SimMultiSpeedCoils(
		int const UnitarySysNum, // Index of AirloopHVAC:UnitarySystem object
		bool const FirstHVACIteration, // True when first HVAC iteration
		bool const SensibleLoad,
		bool const LatentLoad,
		Real64 const PartLoadFrac,
		int const CoilType,
		Optional_int_const SpeedNumber = _
	);

	void
	CalcPassiveSystem(
		int const UnitarySysNum, // Index of AirloopHVAC:UnitarySystem object
		bool const FirstHVACIteration // True when first HVAC iteration
	);

	void
	SetOnOffMassFlowRate(
		int const UnitarySysNum, // index to unitary system
		Real64 & OnOffAirFlowRatio, // ratio of coil on to coil off air flow rate
		Real64 const PartLoadRatio // coil part-load ratio
	);

	void
	SetAverageAirFlow(
		int const UnitarySysNum, // Unit index
		Real64 const PartLoadRatio, // unit part load ratio
		Real64 & OnOffAirFlowRatio // ratio of compressor ON airflow to AVERAGE airflow over timestep
	);

	void
	ReportUnitarySystem(
		int const UnitarySysNum,
		int const AirLoopNum
	);

	void
	UnitarySystemHeatRecovery( int const UnitarySysNum ); // Number of the current electric UnitarySystem being simulated

	Real64
	DXHeatingCoilResidual(
		Real64 const PartLoadFrac, // Compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	DXCoilVarSpeedResidual(
		Real64 const SpeedRatio, // compressor speed ratio (1.0 is max, 0.0 is min)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	HeatingCoilVarSpeedResidual(
		Real64 const SpeedRatio, // compressor speed ratio (1.0 is max, 0.0 is min)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	DXCoilVarSpeedHumRatResidual(
		Real64 const SpeedRatio, // compressor speed ratio (1.0 is max, 0.0 is min)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	DXCoilCyclingResidual(
		Real64 const CycRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	HeatingCoilVarSpeedCycResidual(
		Real64 const CycRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	DXCoilCyclingHumRatResidual(
		Real64 const CycRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	DOE2DXCoilResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	DOE2DXCoilHumRatResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	CoolWaterHumRatResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = CoolWater coil number
	);

	Real64
	CoolWaterTempResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = CoolWater coil number
	);

	Real64
	CoolWatertoAirHPHumRatResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = CoolWatertoAirHP coil number
	);

	Real64
	CoolWatertoAirHPTempResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = CoolWatertoAirHP coil number
	);

	Real64
	HeatWatertoAirHPTempResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = HeatWatertoAirHP coil number
	);

	void
	HeatPumpRunFrac(
		int const UnitarySysNum, // UnitarySystem Index Number
		Real64 const PLR, // part load ratio
		bool & errFlag, // part load factor out of range flag
		Real64 & RuntimeFrac // the required run time fraction to meet part load
	);

	Real64
	MultiModeDXCoilResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	MultiModeDXCoilHumRatResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	HXAssistedCoolCoilTempResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	HXAssistedCoolCoilHRResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	GasElecHeatingCoilResidual(
		Real64 const PartLoadFrac, // Compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	HotWaterHeatingCoilResidual(
		Real64 const PartLoadFrac, // Compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	SteamHeatingCoilResidual(
		Real64 const PartLoadFrac, // Compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	void
	FrostControlSetPointLimit(
		int const UnitarySysNum, // dx cooling coil system index
		Real64 & TempSetPoint, // temperature setpoint of the sensor node
		Real64 & HumRatSetPoint, // humidity ratio setpoint of the sensor node
		Real64 const BaroPress, // baromtric pressure, Pa [N/m^2]
		Real64 const TfrostControl, // minimum temperature limit for forst control
		int const ControlMode // temperature or humidity control mode
	);

	void
	CheckUnitarySysCoilInOASysExists( std::string const & UnitarySysName );

	void
	GetUnitarySystemOAHeatCoolCoil(
		std::string const & UnitarySystemName, // Name of Unitary System object
		Optional_bool OACoolingCoil = _, // Cooling coil in OA stream
		Optional_bool OAHeatingCoil = _ // Heating coil in OA stream
	);

	int
	GetUnitarySystemDXCoolingCoilIndex( std::string const & UnitarySystemName ); // Name of Unitary System object

} // HVACUnitarySystem

	// *****************************************************************************
	//     NOTICE

	//     Copyright (c) 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of CalIFornia through Ernest OrlanDO Lawrence
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
	//     permit others to DO so.
	//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.


} // EnergyPlus

#endif
