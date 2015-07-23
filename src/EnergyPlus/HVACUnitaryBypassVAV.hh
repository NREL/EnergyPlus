#ifndef HVACUnitaryBypassVAV_hh_INCLUDED
#define HVACUnitaryBypassVAV_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace HVACUnitaryBypassVAV {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS
	// Compressor operation
	extern int const On; // Normal compressor operation
	extern int const Off; // Signal DXCoil that compressor should not run

	// DX Coils supported in this module
	extern int const CoilDX_CoolingSingleSpeed; // Coil:DX:CoolingBypassFactorEmpirical
	extern int const CoilDX_CoolingHXAssisted; // Coil:DX:HeatExchangerAssisted
	extern int const CoilDX_CoolingTwoStageWHumControl; // Coil:Cooling:DX:TwoStageWithHumidityControlMode
	// formerly (v3 and beyond) Coil:DX:MultiMode:CoolingEmpirical
	extern int const CoilDX_HeatingEmpirical; // Coil:DX:HeatingEmpirical
	extern int const Coil_HeatingGas; // Coil:Gas:Heating
	extern int const Coil_HeatingElectric; // Coil:Electric:Heating

	// Dehumidification control modes (DehumidControlMode) for Multimode units only
	extern int const DehumidControl_None;
	extern int const DehumidControl_Multimode;
	extern int const DehumidControl_CoolReheat;

	// Mode of operation
	extern int const CoolingMode; // System operating mode is cooling
	extern int const HeatingMode; // System operating mode is heating

	// Priority control mode (prioritized thermostat signal)
	extern int const CoolingPriority; // Controls CBVAV system based on cooling priority
	extern int const HeatingPriority; // Controls CBVAV system based on heating priority
	extern int const ZonePriority; // Controls CBVAV system based on zone priority

	// Airflow control for contant fan mode
	extern int const UseCompressorOnFlow; // Set compressor OFF air flow rate equal to compressor ON air flow rate
	extern int const UseCompressorOffFlow; // Set compressor OFF air flow rate equal to user defined value

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:

	extern int NumCBVAV; // Number of CBVAV systems in input file
	extern Real64 CompOnMassFlow; // System air mass flow rate w/ compressor ON
	extern Real64 OACompOnMassFlow; // OA mass flow rate w/ compressor ON
	extern Real64 CompOffMassFlow; // System air mass flow rate w/ compressor OFF
	extern Real64 OACompOffMassFlow; // OA mass flow rate w/ compressor OFF
	extern Real64 CompOnFlowRatio; // fan flow ratio when coil on
	extern Real64 CompOffFlowRatio; // fan flow ratio when coil off
	extern Real64 FanSpeedRatio; // ratio of air flow ratio passed to fan object
	extern Real64 BypassDuctFlowFraction; // Fraction of unit mass flow that returns to inlet of CBVAV unit through bypass duct
	extern Real64 PartLoadFrac; // Compressor part-load fraction
	extern Real64 SaveCompressorPLR; // Holds DX compressor PLR from active DX coil
	extern Real64 TempSteamIn; // steam coil steam inlet temperature
	extern Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Types

	struct CBVAVData
	{
		// Members
		// input data
		std::string Name; // Name of unit
		std::string UnitType; // Type of unit
		std::string Sched; // Availability schedule name
		int SchedPtr; // Index number to availability schedule
		Real64 MaxCoolAirVolFlow; // System air volumetric flow rate during cooling operation [m3/s]
		Real64 MaxHeatAirVolFlow; // System air volumetric flow rate during heating operation [m3/s]
		Real64 MaxNoCoolHeatAirVolFlow; // System air volumetric flow rate when no cooling or heating [m3/s]
		Real64 MaxCoolAirMassFlow; // System air mass flow rate during cooling operation [kg/s]
		Real64 MaxHeatAirMassFlow; // System air mass flow rate during heating operation [kg/s]
		Real64 MaxNoCoolHeatAirMassFlow; // System air mass flow rate when no cooling or heating [kg/s]
		Real64 CoolOutAirVolFlow; // OA volumetric flow rate during cooling operation [m3/s]
		Real64 HeatOutAirVolFlow; // OA volumetric flow rate during heating operation [m3/s]
		Real64 NoCoolHeatOutAirVolFlow; // OA volumetric flow rate when no cooling or heating [m3/s]
		Real64 CoolOutAirMassFlow; // OA mass flow rate during cooling operation [kg/s]
		Real64 HeatOutAirMassFlow; // OA mass flow rate during heating operation [kg/s]
		Real64 NoCoolHeatOutAirMassFlow; // OA mass flow rate when no cooling or heating [kg/s]
		int OutAirSchPtr; // Index number to outside air multiplier schedule
		int AirInNode; // Inlet air node number for CBVAV unit
		int AirOutNode; // Outlet air node number for CBVAV unit
		int CondenserNodeNum; // DX Coil condenser air inlet node number
		int MixerOutsideAirNode; // Outside air node number for OA mixer
		int MixerMixedAirNode; // Mixed air node number for OA mixer
		int MixerReliefAirNode; // Relief air node number for OA mixer
		int MixerInletAirNode; // Return air node number for OA mixer
		int SplitterOutletAirNode; // Air node number for splitter (last component outlet node)
		std::string OAMixType; // type of outside air mixer
		std::string OAMixName; // Name of OA mixer
		int OAMixIndex; // Index to OA mixer
		std::string FanName; // Name of fan
		std::string FanType; // Type of fan
		int FanPlace; // Fan placement is either blowthru (1) or drawthru (2)
		int FanType_Num; // Fan type number (see DataHVACGlobals)
		int FanIndex; // Index number to fan
		int FanOpModeSchedPtr; // Fan operating mode schedule pointer
		Real64 FanVolFlow; // Volumetric flow rate of system supply air fan [m3/s]
		Real64 HeatingSpeedRatio; // Fan speed ratio in heating mode
		Real64 CoolingSpeedRatio; // Fan speed ratio in cooling mode
		Real64 NoHeatCoolSpeedRatio; // Fan speed ratio when no cooling or heating
		bool CheckFanFlow; // Check fan volumetric flow versus system flow in init routine.
		std::string DXCoolCoilName; // Name of DX cooling coil
		std::string DXCoolCoilType; // Type of DX cooling coil,Coil:DX:CoolingBypassFactorEmpirical or
		//               CoilSystem:Cooling:DX:HeatExchangerAssisted
		int DXCoolCoilType_Num; // Numeric equivalent for DX cooling coil type
		int CoolCoilCompIndex; // cooling coil component index number
		int DXCoolCoilIndexNum; // actual DX cooling coil index number
		int DXHeatCoilIndexNum; // actual DX heating coil index number
		std::string HeatCoilName; // Name of heating coil
		std::string HeatCoilType; // Type of heating coil,Coil:DX:HeatingEmpirical
		// Coil:Heater:Gas, Coil:Heater:Electric, Coil:Heater:Water
		// Coil:Heater:Steam
		int HeatCoilType_Num; // Numeric equivalent for DX heating coil type
		int HeatCoilIndex; // DX heating coil index number
		int OpMode; // mode of operation; 1=cycling fan, cycling compressor
		//                    2=continuous fan, cycling compresor
		int CoilControlNode; // heating coil hot water or steam inlet node
		int CoilOutletNode; // outlet node for hot water and steam coil
		int LoopNum; // plant loop index for water heating coil
		int LoopSide; // plant loop side  index for water heating coil
		int BranchNum; // plant loop branch index for water heating coil
		int CompNum; // plant loop component index for water heating coil
		int HotWaterCoilMaxIterIndex; // Index to recurring warning message
		int HotWaterCoilMaxIterIndex2; // Index to recurring warning message
		Real64 MaxHeatCoilFluidFlow; // water or steam mass flow rate for heating coil [kg/s]
		Real64 DesignHeatingCapacity; // design heating capacity of the heating coil
		Real64 DesignSuppHeatingCapacity; // Operating capacity of supplemental Heating Coil [W]
		Real64 MinOATCompressor; // Minimum OAT for compressor operation [C]
		Real64 MinLATCooling; // Minimum leaving air temp for compressor cooling operation [C]
		Real64 MaxLATHeating; // Maximum leaving air temp for heating operation [C]
		// Report data
		Real64 TotHeatEnergyRate; // Total heating output [W]
		Real64 TotHeatEnergy; // Total heating output [J]
		Real64 TotCoolEnergyRate; // Total cooling output [W]
		Real64 TotCoolEnergy; // Total cooling output [J]
		Real64 SensHeatEnergyRate; // Sensible heating output [W]
		Real64 SensHeatEnergy; // Sensible heating output [J]
		Real64 SensCoolEnergyRate; // Sensible cooling output [W]
		Real64 SensCoolEnergy; // Sensible cooling output [J]
		Real64 LatHeatEnergyRate; // Latent heating output [W]
		Real64 LatHeatEnergy; // Latent heating output [J]
		Real64 LatCoolEnergyRate; // Latent cooling output [W]
		Real64 LatCoolEnergy; // Latent cooling output [J]
		Real64 ElecPower; // Electricity consumed [W]
		Real64 ElecConsumption; // Electricity consumed [J]
		Real64 FanPartLoadRatio; // Fan part-load ratio for time step
		Real64 CompPartLoadRatio; // Compressor part-load ratio for time step
		int LastMode; // Last mode of operation, coolingmode or heatingmode
		int AirFlowControl; // Fan control mode, UseCompressorOnFlow or UseCompressorOffFlow
		Real64 CompPartLoadFrac; // Compressor part load ratio
		int AirLoopNumber; // Air loop served by the CBVAV system
		int NumControlledZones;
		Array1D_int ControlledZoneNum; // Index to controlled zones
		Array1D_int ActualZoneNum; // Actual zone number of controlled zone
		Array1D_int ActualZoneNodeNum; // Actual zone node num of controlled zone
		Array1D_int CBVAVBoxOutletNode; // Outlet node of CBVAV Box in controlled zone
		Array1D_int ZoneSequenceCoolingNum; // Index to cooling sequence/priority for this zone
		Array1D_int ZoneSequenceHeatingNum; // Index to heating sequence/priority for this zone
		int PriorityControl; // Control mode - CoolingPriority, HeatingPriority, or ZonePriority
		int NumZonesCooled; // Number of zones requesting cooling
		int NumZonesHeated; // Number of zones requesting heating
		int PLRMaxIter; // Counter for recurring warning message
		int PLRMaxIterIndex; // Index to recurring warning message
		int DXCoilInletNode; // Inlet node number of DX cooling coil
		int DXCoilOutletNode; // Outlet node number of DX cooling coil
		int HeatingCoilInletNode; // Inlet node of heating coil
		int HeatingCoilOutletNode; // Outlet node of heating coil
		int FanInletNodeNum; // fan inlet node number
		Real64 OutletTempSetPoint; // Oulet node temperature setpoint [C]
		Real64 CoilTempSetPoint; // Coil oulet node temperature setpoint (inc. fan heat) [C]
		int HeatCoolMode; // System operating mode (0 = floating, 1 = cooling, 2 = heating)
		Real64 BypassMassFlowRate; // Bypass mass flow rate report variable [m3/s]
		int DehumidificationMode; // Dehumidification mode (0=normal, 1=enhanced)
		int DehumidControlType; // Dehumidification control type (currently only for multimode coil)
		bool HumRatMaxCheck; // Used in Init for warning messages
		int DXIterationExceeded; // Counter for DX coil messages
		int DXIterationExceededIndex; // Counter for DX coil messages
		int DXIterationFailed; // Counter for DX coil messages
		int DXIterationFailedIndex; // Counter for DX coil messages
		int HXDXIterationExceeded; // Counter for HX assisted DX coil messages
		int HXDXIterationExceededIndex; // Counter for HX assisted DX coil messages
		int HXDXIterationFailed; // Counter for HX assisted DX coil messages
		int HXDXIterationFailedIndex; // Counter for HX assisted DX coil messages
		int MMDXIterationExceeded; // Counter for multimode DX coil messages
		int MMDXIterationExceededIndex; // Counter for multimode DX coil messages
		int MMDXIterationFailed; // Counter for multimode DX coil messages
		int MMDXIterationFailedIndex; // Counter for multimode DX coil messages
		int DMDXIterationExceeded; // Counter for dehumidifying multimode DX coil messages
		int DMDXIterationExceededIndex; // Counter for dehumidifying multimode DX coil messages
		int DMDXIterationFailed; // Counter for dehumidifying multimode DX coil messages
		int DMDXIterationFailedIndex; // Counter for dehumidifying multimode DX coil messages
		int CRDXIterationExceeded; // Counter for cool reheat multimode DX coil messages
		int CRDXIterationExceededIndex; // Counter for cool reheat multimode DX coil messages
		int CRDXIterationFailed; // Counter for cool reheat multimode DX coil messages
		int CRDXIterationFailedIndex; // Counter for cool reheat multimode DX coil messages

		// Default Constructor
		CBVAVData() :
			SchedPtr( 0 ),
			MaxCoolAirVolFlow( 0.0 ),
			MaxHeatAirVolFlow( 0.0 ),
			MaxNoCoolHeatAirVolFlow( 0.0 ),
			MaxCoolAirMassFlow( 0.0 ),
			MaxHeatAirMassFlow( 0.0 ),
			MaxNoCoolHeatAirMassFlow( 0.0 ),
			CoolOutAirVolFlow( 0.0 ),
			HeatOutAirVolFlow( 0.0 ),
			NoCoolHeatOutAirVolFlow( 0.0 ),
			CoolOutAirMassFlow( 0.0 ),
			HeatOutAirMassFlow( 0.0 ),
			NoCoolHeatOutAirMassFlow( 0.0 ),
			OutAirSchPtr( 0 ),
			AirInNode( 0 ),
			AirOutNode( 0 ),
			CondenserNodeNum( 0 ),
			MixerOutsideAirNode( 0 ),
			MixerMixedAirNode( 0 ),
			MixerReliefAirNode( 0 ),
			MixerInletAirNode( 0 ),
			SplitterOutletAirNode( 0 ),
			OAMixIndex( 0 ),
			FanPlace( 0 ),
			FanType_Num( 0 ),
			FanIndex( 0 ),
			FanOpModeSchedPtr( 0 ),
			FanVolFlow( 0.0 ),
			HeatingSpeedRatio( 1.0 ),
			CoolingSpeedRatio( 1.0 ),
			NoHeatCoolSpeedRatio( 1.0 ),
			CheckFanFlow( true ),
			DXCoolCoilType_Num( 0 ),
			CoolCoilCompIndex( 0 ),
			DXCoolCoilIndexNum( 0 ),
			DXHeatCoilIndexNum( 0 ),
			HeatCoilType_Num( 0 ),
			HeatCoilIndex( 0 ),
			OpMode( 0 ),
			CoilControlNode( 0 ),
			CoilOutletNode( 0 ),
			LoopNum( 0 ),
			LoopSide( 0 ),
			BranchNum( 0 ),
			CompNum( 0 ),
			HotWaterCoilMaxIterIndex( 0 ),
			HotWaterCoilMaxIterIndex2( 0 ),
			MaxHeatCoilFluidFlow( 0.0 ),
			DesignHeatingCapacity( 0.0 ),
			DesignSuppHeatingCapacity( 0.0 ),
			MinOATCompressor( 0.0 ),
			MinLATCooling( 0.0 ),
			MaxLATHeating( 0.0 ),
			TotHeatEnergyRate( 0.0 ),
			TotHeatEnergy( 0.0 ),
			TotCoolEnergyRate( 0.0 ),
			TotCoolEnergy( 0.0 ),
			SensHeatEnergyRate( 0.0 ),
			SensHeatEnergy( 0.0 ),
			SensCoolEnergyRate( 0.0 ),
			SensCoolEnergy( 0.0 ),
			LatHeatEnergyRate( 0.0 ),
			LatHeatEnergy( 0.0 ),
			LatCoolEnergyRate( 0.0 ),
			LatCoolEnergy( 0.0 ),
			ElecPower( 0.0 ),
			ElecConsumption( 0.0 ),
			FanPartLoadRatio( 0.0 ),
			CompPartLoadRatio( 0.0 ),
			LastMode( 0 ),
			AirFlowControl( 0 ),
			CompPartLoadFrac( 0.0 ),
			AirLoopNumber( 0 ),
			NumControlledZones( 0 ),
			PriorityControl( 0 ),
			NumZonesCooled( 0 ),
			NumZonesHeated( 0 ),
			PLRMaxIter( 0 ),
			PLRMaxIterIndex( 0 ),
			DXCoilInletNode( 0 ),
			DXCoilOutletNode( 0 ),
			HeatingCoilInletNode( 0 ),
			HeatingCoilOutletNode( 0 ),
			FanInletNodeNum( 0 ),
			OutletTempSetPoint( 0.0 ),
			CoilTempSetPoint( 0.0 ),
			HeatCoolMode( 0 ),
			BypassMassFlowRate( 0.0 ),
			DehumidificationMode( 0 ),
			DehumidControlType( 0 ),
			HumRatMaxCheck( true ),
			DXIterationExceeded( 0 ),
			DXIterationExceededIndex( 0 ),
			DXIterationFailed( 0 ),
			DXIterationFailedIndex( 0 ),
			HXDXIterationExceeded( 0 ),
			HXDXIterationExceededIndex( 0 ),
			HXDXIterationFailed( 0 ),
			HXDXIterationFailedIndex( 0 ),
			MMDXIterationExceeded( 0 ),
			MMDXIterationExceededIndex( 0 ),
			MMDXIterationFailed( 0 ),
			MMDXIterationFailedIndex( 0 ),
			DMDXIterationExceeded( 0 ),
			DMDXIterationExceededIndex( 0 ),
			DMDXIterationFailed( 0 ),
			DMDXIterationFailedIndex( 0 ),
			CRDXIterationExceeded( 0 ),
			CRDXIterationExceededIndex( 0 ),
			CRDXIterationFailed( 0 ),
			CRDXIterationFailedIndex( 0 )
		{}

		// Member Constructor
		CBVAVData(
			std::string const & Name, // Name of unit
			std::string const & UnitType, // Type of unit
			std::string const & Sched, // Availability schedule name
			int const SchedPtr, // Index number to availability schedule
			Real64 const MaxCoolAirVolFlow, // System air volumetric flow rate during cooling operation [m3/s]
			Real64 const MaxHeatAirVolFlow, // System air volumetric flow rate during heating operation [m3/s]
			Real64 const MaxNoCoolHeatAirVolFlow, // System air volumetric flow rate when no cooling or heating [m3/s]
			Real64 const MaxCoolAirMassFlow, // System air mass flow rate during cooling operation [kg/s]
			Real64 const MaxHeatAirMassFlow, // System air mass flow rate during heating operation [kg/s]
			Real64 const MaxNoCoolHeatAirMassFlow, // System air mass flow rate when no cooling or heating [kg/s]
			Real64 const CoolOutAirVolFlow, // OA volumetric flow rate during cooling operation [m3/s]
			Real64 const HeatOutAirVolFlow, // OA volumetric flow rate during heating operation [m3/s]
			Real64 const NoCoolHeatOutAirVolFlow, // OA volumetric flow rate when no cooling or heating [m3/s]
			Real64 const CoolOutAirMassFlow, // OA mass flow rate during cooling operation [kg/s]
			Real64 const HeatOutAirMassFlow, // OA mass flow rate during heating operation [kg/s]
			Real64 const NoCoolHeatOutAirMassFlow, // OA mass flow rate when no cooling or heating [kg/s]
			int const OutAirSchPtr, // Index number to outside air multiplier schedule
			int const AirInNode, // Inlet air node number for CBVAV unit
			int const AirOutNode, // Outlet air node number for CBVAV unit
			int const CondenserNodeNum, // DX Coil condenser air inlet node number
			int const MixerOutsideAirNode, // Outside air node number for OA mixer
			int const MixerMixedAirNode, // Mixed air node number for OA mixer
			int const MixerReliefAirNode, // Relief air node number for OA mixer
			int const MixerInletAirNode, // Return air node number for OA mixer
			int const SplitterOutletAirNode, // Air node number for splitter (last component outlet node)
			std::string const & OAMixType, // type of outside air mixer
			std::string const & OAMixName, // Name of OA mixer
			int const OAMixIndex, // Index to OA mixer
			std::string const & FanName, // Name of fan
			std::string const & FanType, // Type of fan
			int const FanPlace, // Fan placement is either blowthru (1) or drawthru (2)
			int const FanType_Num, // Fan type number (see DataHVACGlobals)
			int const FanIndex, // Index number to fan
			int const FanOpModeSchedPtr, // Fan operating mode schedule pointer
			Real64 const FanVolFlow, // Volumetric flow rate of system supply air fan [m3/s]
			Real64 const HeatingSpeedRatio, // Fan speed ratio in heating mode
			Real64 const CoolingSpeedRatio, // Fan speed ratio in cooling mode
			Real64 const NoHeatCoolSpeedRatio, // Fan speed ratio when no cooling or heating
			bool const CheckFanFlow, // Check fan volumetric flow versus system flow in init routine.
			std::string const & DXCoolCoilName, // Name of DX cooling coil
			std::string const & DXCoolCoilType, // Type of DX cooling coil,Coil:DX:CoolingBypassFactorEmpirical or
			int const DXCoolCoilType_Num, // Numeric equivalent for DX cooling coil type
			int const CoolCoilCompIndex, // cooling coil component index number
			int const DXCoolCoilIndexNum, // actual DX cooling coil index number
			int const DXHeatCoilIndexNum, // actual DX heating coil index number
			std::string const & HeatCoilName, // Name of heating coil
			std::string const & HeatCoilType, // Type of heating coil,Coil:DX:HeatingEmpirical
			int const HeatCoilType_Num, // Numeric equivalent for DX heating coil type
			int const HeatCoilIndex, // DX heating coil index number
			int const OpMode, // mode of operation; 1=cycling fan, cycling compressor
			int const CoilControlNode, // heating coil hot water or steam inlet node
			int const CoilOutletNode, // outlet node for hot water and steam coil
			int const LoopNum, // plant loop index for water heating coil
			int const LoopSide, // plant loop side  index for water heating coil
			int const BranchNum, // plant loop branch index for water heating coil
			int const CompNum, // plant loop component index for water heating coil
			int const HotWaterCoilMaxIterIndex, // Index to recurring warning message
			int const HotWaterCoilMaxIterIndex2, // Index to recurring warning message
			Real64 const MaxHeatCoilFluidFlow, // water or steam mass flow rate for heating coil [kg/s]
			Real64 const DesignHeatingCapacity, // design heating capacity of the heating coil
			Real64 const DesignSuppHeatingCapacity, // Operating capacity of supplemental Heating Coil [W]
			Real64 const MinOATCompressor, // Minimum OAT for compressor operation [C]
			Real64 const MinLATCooling, // Minimum leaving air temp for compressor cooling operation [C]
			Real64 const MaxLATHeating, // Maximum leaving air temp for heating operation [C]
			Real64 const TotHeatEnergyRate, // Total heating output [W]
			Real64 const TotHeatEnergy, // Total heating output [J]
			Real64 const TotCoolEnergyRate, // Total cooling output [W]
			Real64 const TotCoolEnergy, // Total cooling output [J]
			Real64 const SensHeatEnergyRate, // Sensible heating output [W]
			Real64 const SensHeatEnergy, // Sensible heating output [J]
			Real64 const SensCoolEnergyRate, // Sensible cooling output [W]
			Real64 const SensCoolEnergy, // Sensible cooling output [J]
			Real64 const LatHeatEnergyRate, // Latent heating output [W]
			Real64 const LatHeatEnergy, // Latent heating output [J]
			Real64 const LatCoolEnergyRate, // Latent cooling output [W]
			Real64 const LatCoolEnergy, // Latent cooling output [J]
			Real64 const ElecPower, // Electricity consumed [W]
			Real64 const ElecConsumption, // Electricity consumed [J]
			Real64 const FanPartLoadRatio, // Fan part-load ratio for time step
			Real64 const CompPartLoadRatio, // Compressor part-load ratio for time step
			int const LastMode, // Last mode of operation, coolingmode or heatingmode
			int const AirFlowControl, // Fan control mode, UseCompressorOnFlow or UseCompressorOffFlow
			Real64 const CompPartLoadFrac, // Compressor part load ratio
			int const AirLoopNumber, // Air loop served by the CBVAV system
			int const NumControlledZones,
			Array1_int const & ControlledZoneNum, // Index to controlled zones
			Array1_int const & ActualZoneNum, // Actual zone number of controlled zone
			Array1_int const & ActualZoneNodeNum, // Actual zone node num of controlled zone
			Array1_int const & CBVAVBoxOutletNode, // Outlet node of CBVAV Box in controlled zone
			Array1_int const & ZoneSequenceCoolingNum, // Index to cooling sequence/priority for this zone
			Array1_int const & ZoneSequenceHeatingNum, // Index to heating sequence/priority for this zone
			int const PriorityControl, // Control mode - CoolingPriority, HeatingPriority, or ZonePriority
			int const NumZonesCooled, // Number of zones requesting cooling
			int const NumZonesHeated, // Number of zones requesting heating
			int const PLRMaxIter, // Counter for recurring warning message
			int const PLRMaxIterIndex, // Index to recurring warning message
			int const DXCoilInletNode, // Inlet node number of DX cooling coil
			int const DXCoilOutletNode, // Outlet node number of DX cooling coil
			int const HeatingCoilInletNode, // Inlet node of heating coil
			int const HeatingCoilOutletNode, // Outlet node of heating coil
			int const FanInletNodeNum, // fan inlet node number
			Real64 const OutletTempSetPoint, // Oulet node temperature setpoint [C]
			Real64 const CoilTempSetPoint, // Coil oulet node temperature setpoint (inc. fan heat) [C]
			int const HeatCoolMode, // System operating mode (0 = floating, 1 = cooling, 2 = heating)
			Real64 const BypassMassFlowRate, // Bypass mass flow rate report variable [m3/s]
			int const DehumidificationMode, // Dehumidification mode (0=normal, 1=enhanced)
			int const DehumidControlType, // Dehumidification control type (currently only for multimode coil)
			bool const HumRatMaxCheck, // Used in Init for warning messages
			int const DXIterationExceeded, // Counter for DX coil messages
			int const DXIterationExceededIndex, // Counter for DX coil messages
			int const DXIterationFailed, // Counter for DX coil messages
			int const DXIterationFailedIndex, // Counter for DX coil messages
			int const HXDXIterationExceeded, // Counter for HX assisted DX coil messages
			int const HXDXIterationExceededIndex, // Counter for HX assisted DX coil messages
			int const HXDXIterationFailed, // Counter for HX assisted DX coil messages
			int const HXDXIterationFailedIndex, // Counter for HX assisted DX coil messages
			int const MMDXIterationExceeded, // Counter for multimode DX coil messages
			int const MMDXIterationExceededIndex, // Counter for multimode DX coil messages
			int const MMDXIterationFailed, // Counter for multimode DX coil messages
			int const MMDXIterationFailedIndex, // Counter for multimode DX coil messages
			int const DMDXIterationExceeded, // Counter for dehumidifying multimode DX coil messages
			int const DMDXIterationExceededIndex, // Counter for dehumidifying multimode DX coil messages
			int const DMDXIterationFailed, // Counter for dehumidifying multimode DX coil messages
			int const DMDXIterationFailedIndex, // Counter for dehumidifying multimode DX coil messages
			int const CRDXIterationExceeded, // Counter for cool reheat multimode DX coil messages
			int const CRDXIterationExceededIndex, // Counter for cool reheat multimode DX coil messages
			int const CRDXIterationFailed, // Counter for cool reheat multimode DX coil messages
			int const CRDXIterationFailedIndex // Counter for cool reheat multimode DX coil messages
		) :
			Name( Name ),
			UnitType( UnitType ),
			Sched( Sched ),
			SchedPtr( SchedPtr ),
			MaxCoolAirVolFlow( MaxCoolAirVolFlow ),
			MaxHeatAirVolFlow( MaxHeatAirVolFlow ),
			MaxNoCoolHeatAirVolFlow( MaxNoCoolHeatAirVolFlow ),
			MaxCoolAirMassFlow( MaxCoolAirMassFlow ),
			MaxHeatAirMassFlow( MaxHeatAirMassFlow ),
			MaxNoCoolHeatAirMassFlow( MaxNoCoolHeatAirMassFlow ),
			CoolOutAirVolFlow( CoolOutAirVolFlow ),
			HeatOutAirVolFlow( HeatOutAirVolFlow ),
			NoCoolHeatOutAirVolFlow( NoCoolHeatOutAirVolFlow ),
			CoolOutAirMassFlow( CoolOutAirMassFlow ),
			HeatOutAirMassFlow( HeatOutAirMassFlow ),
			NoCoolHeatOutAirMassFlow( NoCoolHeatOutAirMassFlow ),
			OutAirSchPtr( OutAirSchPtr ),
			AirInNode( AirInNode ),
			AirOutNode( AirOutNode ),
			CondenserNodeNum( CondenserNodeNum ),
			MixerOutsideAirNode( MixerOutsideAirNode ),
			MixerMixedAirNode( MixerMixedAirNode ),
			MixerReliefAirNode( MixerReliefAirNode ),
			MixerInletAirNode( MixerInletAirNode ),
			SplitterOutletAirNode( SplitterOutletAirNode ),
			OAMixType( OAMixType ),
			OAMixName( OAMixName ),
			OAMixIndex( OAMixIndex ),
			FanName( FanName ),
			FanType( FanType ),
			FanPlace( FanPlace ),
			FanType_Num( FanType_Num ),
			FanIndex( FanIndex ),
			FanOpModeSchedPtr( FanOpModeSchedPtr ),
			FanVolFlow( FanVolFlow ),
			HeatingSpeedRatio( HeatingSpeedRatio ),
			CoolingSpeedRatio( CoolingSpeedRatio ),
			NoHeatCoolSpeedRatio( NoHeatCoolSpeedRatio ),
			CheckFanFlow( CheckFanFlow ),
			DXCoolCoilName( DXCoolCoilName ),
			DXCoolCoilType( DXCoolCoilType ),
			DXCoolCoilType_Num( DXCoolCoilType_Num ),
			CoolCoilCompIndex( CoolCoilCompIndex ),
			DXCoolCoilIndexNum( DXCoolCoilIndexNum ),
			DXHeatCoilIndexNum( DXHeatCoilIndexNum ),
			HeatCoilName( HeatCoilName ),
			HeatCoilType( HeatCoilType ),
			HeatCoilType_Num( HeatCoilType_Num ),
			HeatCoilIndex( HeatCoilIndex ),
			OpMode( OpMode ),
			CoilControlNode( CoilControlNode ),
			CoilOutletNode( CoilOutletNode ),
			LoopNum( LoopNum ),
			LoopSide( LoopSide ),
			BranchNum( BranchNum ),
			CompNum( CompNum ),
			HotWaterCoilMaxIterIndex( HotWaterCoilMaxIterIndex ),
			HotWaterCoilMaxIterIndex2( HotWaterCoilMaxIterIndex2 ),
			MaxHeatCoilFluidFlow( MaxHeatCoilFluidFlow ),
			DesignHeatingCapacity( DesignHeatingCapacity ),
			DesignSuppHeatingCapacity( DesignSuppHeatingCapacity ),
			MinOATCompressor( MinOATCompressor ),
			MinLATCooling( MinLATCooling ),
			MaxLATHeating( MaxLATHeating ),
			TotHeatEnergyRate( TotHeatEnergyRate ),
			TotHeatEnergy( TotHeatEnergy ),
			TotCoolEnergyRate( TotCoolEnergyRate ),
			TotCoolEnergy( TotCoolEnergy ),
			SensHeatEnergyRate( SensHeatEnergyRate ),
			SensHeatEnergy( SensHeatEnergy ),
			SensCoolEnergyRate( SensCoolEnergyRate ),
			SensCoolEnergy( SensCoolEnergy ),
			LatHeatEnergyRate( LatHeatEnergyRate ),
			LatHeatEnergy( LatHeatEnergy ),
			LatCoolEnergyRate( LatCoolEnergyRate ),
			LatCoolEnergy( LatCoolEnergy ),
			ElecPower( ElecPower ),
			ElecConsumption( ElecConsumption ),
			FanPartLoadRatio( FanPartLoadRatio ),
			CompPartLoadRatio( CompPartLoadRatio ),
			LastMode( LastMode ),
			AirFlowControl( AirFlowControl ),
			CompPartLoadFrac( CompPartLoadFrac ),
			AirLoopNumber( AirLoopNumber ),
			NumControlledZones( NumControlledZones ),
			ControlledZoneNum( ControlledZoneNum ),
			ActualZoneNum( ActualZoneNum ),
			ActualZoneNodeNum( ActualZoneNodeNum ),
			CBVAVBoxOutletNode( CBVAVBoxOutletNode ),
			ZoneSequenceCoolingNum( ZoneSequenceCoolingNum ),
			ZoneSequenceHeatingNum( ZoneSequenceHeatingNum ),
			PriorityControl( PriorityControl ),
			NumZonesCooled( NumZonesCooled ),
			NumZonesHeated( NumZonesHeated ),
			PLRMaxIter( PLRMaxIter ),
			PLRMaxIterIndex( PLRMaxIterIndex ),
			DXCoilInletNode( DXCoilInletNode ),
			DXCoilOutletNode( DXCoilOutletNode ),
			HeatingCoilInletNode( HeatingCoilInletNode ),
			HeatingCoilOutletNode( HeatingCoilOutletNode ),
			FanInletNodeNum( FanInletNodeNum ),
			OutletTempSetPoint( OutletTempSetPoint ),
			CoilTempSetPoint( CoilTempSetPoint ),
			HeatCoolMode( HeatCoolMode ),
			BypassMassFlowRate( BypassMassFlowRate ),
			DehumidificationMode( DehumidificationMode ),
			DehumidControlType( DehumidControlType ),
			HumRatMaxCheck( HumRatMaxCheck ),
			DXIterationExceeded( DXIterationExceeded ),
			DXIterationExceededIndex( DXIterationExceededIndex ),
			DXIterationFailed( DXIterationFailed ),
			DXIterationFailedIndex( DXIterationFailedIndex ),
			HXDXIterationExceeded( HXDXIterationExceeded ),
			HXDXIterationExceededIndex( HXDXIterationExceededIndex ),
			HXDXIterationFailed( HXDXIterationFailed ),
			HXDXIterationFailedIndex( HXDXIterationFailedIndex ),
			MMDXIterationExceeded( MMDXIterationExceeded ),
			MMDXIterationExceededIndex( MMDXIterationExceededIndex ),
			MMDXIterationFailed( MMDXIterationFailed ),
			MMDXIterationFailedIndex( MMDXIterationFailedIndex ),
			DMDXIterationExceeded( DMDXIterationExceeded ),
			DMDXIterationExceededIndex( DMDXIterationExceededIndex ),
			DMDXIterationFailed( DMDXIterationFailed ),
			DMDXIterationFailedIndex( DMDXIterationFailedIndex ),
			CRDXIterationExceeded( CRDXIterationExceeded ),
			CRDXIterationExceededIndex( CRDXIterationExceededIndex ),
			CRDXIterationFailed( CRDXIterationFailed ),
			CRDXIterationFailedIndex( CRDXIterationFailedIndex )
		{}

	};

	// Object Data
	extern Array1D< CBVAVData > CBVAV;

	// Functions

	void
	SimUnitaryBypassVAV(
		std::string const & CompName, // Name of the CBVAV system
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system time step
		int const AirLoopNum, // air loop index
		int & CompIndex // Index to changeover-bypass VAV system
	);

	void
	SimCBVAV(
		int const CBVAVNum, // Index of the current CBVAV system being simulated
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & QZnReq, // Zone load for all zones served by this air loop system
		Real64 & QSensUnitOut, // Sensible delivered capacity [W]
		Real64 & OnOffAirFlowRatio, // Ratio of compressor ON airflow to AVERAGE airflow over timestep
		bool & HXUnitOn // flag to enable heat exchanger
	);

	void
	GetCBVAV();

	void
	InitCBVAV(
		int const CBVAVNum, // Index of the current CBVAV unit being simulated
		bool const FirstHVACIteration, // TRUE if first HVAC iteration
		int const AirLoopNum, // air loop index
		Real64 & QZnReq, // Heating/Cooling load for all zones
		Real64 & OnOffAirFlowRatio, // Ratio of compressor ON airflow to average airflow over timestep
		bool & HXUnitOn // flag to enable heat exchanger
	);

	void
	SizeCBVAV( int const CBVAVNum ); // Index to CBVAV system

	void
	ControlCBVAVOutput(
		int const CBVAVNum, // Index to CBVAV system
		bool const FirstHVACIteration, // Flag for 1st HVAC iteration
		Real64 & QZnReq, // Cooling or heating output needed by zone [W]
		Real64 & PartLoadFrac, // Unit part load fraction
		Real64 & OnOffAirFlowRatio, // Ratio of compressor ON airflow to AVERAGE airflow over timestep
		bool & HXUnitOn // flag to enable heat exchanger
	);

	void
	CalcCBVAV(
		int const CBVAVNum, // Unit index in fan coil array
		bool const FirstHVACIteration, // Flag for 1st HVAC iteration
		Real64 & PartLoadFrac, // Compressor part load fraction
		Real64 & LoadMet, // Load met by unit (W)
		Real64 & QZnReq, // Zone load (W)
		Real64 & OnOffAirFlowRatio, // Ratio of compressor ON airflow to AVERAGE airflow over timestep
		bool const HXUnitOn // flag to enable heat exchanger
	);

	void
	GetZoneLoads(
		int const CBVAVNum, // Index to CBVAV unit being simulated
		Real64 & QZoneReq // Total zone load served by this air loop
	);

	Real64
	CalcSetPointTempTarget( int const CBVAVNumber ); // Index to changeover-bypass VAV system

	Real64
	DOE2DXCoilResidual(
		Real64 const PartLoadFrac, // Compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // Par(1) = DX coil number
	);

	Real64
	HXAssistDXCoilResidual(
		Real64 const PartLoadFrac, // Compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // Par(1) = DX coil number
	);

	Real64
	DXHeatingCoilResidual(
		Real64 const PartLoadFrac, // Compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // Par(1) = DX coil number
	);

	Real64
	MultiModeDXCoilResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // Par(1) = DX coil number
	);

	void
	SetAverageAirFlow(
		int const CBVAVNum, // Index to CBVAV system
		Real64 & OnOffAirFlowRatio, // Ratio of compressor ON airflow to average airflow over timestep
		bool const FirstHVACIteration // Flag denoting the first pass on the air loop simulation
	);

	void
	ReportCBVAV( int const CBVAVNum ); // Index of the current CBVAV unit being simulated

	void
	CalcNonDXHeatingCoils(
		int const CBVAVNum, // Changeover bypass VAV unit index
		bool const FirstHVACIteration, // flag for first HVAC iteration in the time step
		Real64 & HeatCoilLoad, // heating coil load to be met (Watts)
		int const FanMode, // fan operation mode
		Real64 & HeatCoilLoadmet // coil heating load met
	);

	Real64
	HotWaterCoilResidual(
		Real64 const HWFlow, // hot water flow rate in kg/s
		Array1< Real64 > const & Par // Par(5) is the requested coil load
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

} // HVACUnitaryBypassVAV

} // EnergyPlus

#endif
