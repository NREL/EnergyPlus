#ifndef HeatRecovery_hh_INCLUDED
#define HeatRecovery_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace HeatRecovery {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern Real64 const KELVZERO;
	extern Real64 const SMALL;

	// Heat exchanger performance data type
	extern int const BALANCEDHX_PERFDATATYPE1;

	// Heat exchanger configurations
	extern int const Counter_Flow;
	extern int const Parallel_Flow;
	extern int const Cross_Flow_Both_Unmixed;
	extern int const Cross_Flow_Other;

	// Heat exchanger configuration types
	extern int const Plate;
	extern int const Rotary;

	// Economizer lockout operation
	extern int const EconoLockOut_No;
	extern int const EconoLockOut_Yes;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	extern int NumHeatExchangers; // number of heat exchangers
	extern int NumAirToAirPlateExchs; // number of air to air plate heat exchangers
	extern int NumAirToAirGenericExchs; // number of air to air generic heat exchangers
	extern int NumDesiccantBalancedExchs; // number of desiccant balanced heat exchangers
	extern int NumDesBalExchsPerfDataType1; // number of desiccant balanced heat exchanger performance data maps
	extern Real64 FullLoadOutAirTemp; // Used with desiccant HX empirical model, water coils use inlet node condition
	// DX coils use DXCoilFullLoadOutAirTemp when coil is ON otherwise inlet node
	extern Real64 FullLoadOutAirHumRat; // Used with desiccant HX empirical model, water coils use inlet node condition
	// DX coils use DXCoilFullLoadOutAirHumRat when coil is ON otherwise inlet node
	extern bool GetInputFlag; // First time, input is "gotten"
	extern bool CalledFromParentObject; // Indicates that HX is called from parent object (this object is not on a branch)
	extern Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE:

	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Sizing routine for the module

	// Update routines to check convergence and update nodes

	// Common routines

	// External function calls

	// Types

	struct HeatExchCond
	{
		// Members
		std::string Name; // name of component
		int ExchTypeNum; // Integer equivalent to ExchType
		int HeatExchPerfTypeNum; // Desiccant balanced heat exchanger performance data type num
		std::string HeatExchPerfName; // Desiccant balanced heat exchanger performance data name
		int SchedPtr; // index of schedule
		int FlowArr; // flow Arrangement:
		// 1: COUNTER_FLOW
		// 2: PARALLEL_FLOW
		// 3: CROSS_FLOW_BOTH_UNMIXED
		int EconoLockOut; // 1: Yes;  0: No
		Real64 hARatio; // ratio of supply side h*A to secondary side h*A
		Real64 NomSupAirVolFlow; // nominal supply air volume flow rate (m3/s)
		Real64 NomSupAirInTemp; // nominal supply air inlet temperature (C)
		Real64 NomSupAirOutTemp; // nominal supply air outlet temperature (C)
		Real64 NomSecAirVolFlow; // nominal secondary air volume flow rate (m3/s)
		Real64 NomSecAirInTemp; // nominal secondary air inlet temperature (C)
		Real64 NomElecPower; // nominal electric power consumption [W]
		// values describing nominal condition (derived from input parameters)
		Real64 UA0; // (Uavg*A) at nominal condition
		Real64 mTSup0; // product mDot*Tabs, supply  air, nominal cond.
		Real64 mTSec0; // product mDot*Tabs, exhaust air, nominal cond
		Real64 NomSupAirMassFlow; // nominal supply air mass flow rate (kg/s)
		Real64 NomSecAirMassFlow; // nominal secondary air mass flow rate (kg/s)
		// Nodes
		int SupInletNode; // supply air inlet node number
		int SupOutletNode; // supply air outlet node number
		int SecInletNode; // secondary air inlet node number
		int SecOutletNode; // secondary air outlet node number
		// inlet conditions
		Real64 SupInTemp; // supply air inlet temperature (C)
		Real64 SupInHumRat; // supply air inlet humidity ratio (kg water/kg dry air)
		Real64 SupInEnth; // supply air inlet enthalpy (J/kg)
		Real64 SupInMassFlow; // supply air inlet mass flow rate (kg/s)
		Real64 SecInTemp; // secondary air inlet temperature (C)
		Real64 SecInHumRat; // secondary air inlet humidity ratio (kg water/kg dry air)
		Real64 SecInEnth; // secondary air inlet enthalpy (J/kg)
		Real64 SecInMassFlow; // secondary air inlet mass flow rate (kg/s)
		// balanced desiccant inputs
		int PerfDataIndex; // Performance data index allocating performance data number to heat exchanger
		Real64 FaceArea; // face area of balanced desiccant heat exchangers to determine face velocity [m2]
		bool UnbalancedWarningFlag; // Used to print one-time warning when unbalanced flow exists (then set to FALSE)
		// generic hx performance inputs
		Real64 HeatEffectSensible100; // heating sensible effectiveness at 100% rated air flow
		Real64 HeatEffectSensible75; // heating sensible effectiveness at 75% rated air flow
		Real64 HeatEffectLatent100; // heating latent effectiveness at 100% rated air flow
		Real64 HeatEffectLatent75; // heating latent effectiveness at 75% rated air flow
		Real64 CoolEffectSensible100; // cooling sensible effectiveness at 100% rated air flow
		Real64 CoolEffectSensible75; // cooling sensible effectiveness at 75% rated air flow
		Real64 CoolEffectLatent100; // cooling latent effectiveness at 100% rated air flow
		Real64 CoolEffectLatent75; // cooling latent effectiveness at 75% rated air flow
		int HeatExchEconoMode; // generic heat exchanger economize mode option
		// 1 = None, 2 = Bypass, 3 = Stop Rotary HX Rotation
		int ExchConfigNum; // parameter equivalent of HX configuration, plate or rotary
		// frost control parameters
		std::string FrostControlType; // type of frost control used if any
		Real64 ThresholdTemperature; // threshold temperature for frost control
		Real64 InitialDefrostTime; // initial defrost time
		Real64 RateofDefrostTimeIncrease; // rate of change of defrost time
		Real64 DefrostFraction; // fraction of time HX is in frost control mode
		bool ControlToTemperatureSetPoint; // temperature control flag for generic HX
		// outlet conditions
		Real64 SupOutTemp; // supply air outlet temperature (C)
		Real64 SupOutHumRat; // supply air outlet humidity ratio (kg water/kg dry air)
		Real64 SupOutEnth; // supply air outlet enthalpy (J/kg)
		Real64 SupOutMassFlow; // supply air outlet mass flow rate (kg/s)
		Real64 SecOutTemp; // secondary air outlet temperature (C)
		Real64 SecOutHumRat; // secondary air outlet humidity ratio (kg water/kg dry air)
		Real64 SecOutEnth; // secondary air outlet enthalpy (J/kg)
		Real64 SecOutMassFlow; // secondary air outlet mass flow rate (kg/s)
		// report values
		Real64 SensHeatingRate; // rate of sensible heat being added to the supply (primary) air [W]
		Real64 SensHeatingEnergy; // sensible heat added to the supply (primary) air [J]
		Real64 LatHeatingRate; // rate of latent heat being added to the supply (primary) air [W]
		Real64 LatHeatingEnergy; // latent heat added to the supply (primary) air [J]
		Real64 TotHeatingRate; // rate of total heat being added to the supply (primary) air [W]
		Real64 TotHeatingEnergy; // total heat added to the supply (primary) air [J]
		Real64 SensCoolingRate; // rate of sensible heat being removed from the supply (primary) air [W]
		Real64 SensCoolingEnergy; // sensible heat removed from the supply (primary) air [J]
		Real64 LatCoolingRate; // rate of latent heat being removed from the supply (primary) air [W]
		Real64 LatCoolingEnergy; // latent heat removed from the supply (primary) air [J]
		Real64 TotCoolingRate; // rate of total heat being removed from the supply (primary) air [W]
		Real64 TotCoolingEnergy; // total heat removed from the supply (primary) air [J]
		Real64 ElecUseEnergy; // electricity consumption [J]
		Real64 ElecUseRate; // electricity consumption rate [W]
		Real64 SensEffectiveness; // heat exchanger sensible effectiveness [-]
		Real64 LatEffectiveness; // heat exchanger latent effectiveness [-]
		Real64 SupBypassMassFlow; // supply air mass flow rate bypassing the heat exchanger [kg/s]
		Real64 SecBypassMassFlow; // secondary air mass flow rate bypassing the heat exchanger [kg/s]
		int LowFlowErrCount; // Counter for recurring warning message
		int LowFlowErrIndex; // Index to recurring warning message
		int UnBalancedErrCount; // Counter for recurring warning message
		int UnBalancedErrIndex; // Index to recurring warning message

		// Default Constructor
		HeatExchCond() :
			ExchTypeNum( 0 ),
			HeatExchPerfTypeNum( 0 ),
			SchedPtr( 0 ),
			FlowArr( 0 ),
			EconoLockOut( 0 ),
			hARatio( 0.0 ),
			NomSupAirVolFlow( 0.0 ),
			NomSupAirInTemp( 0.0 ),
			NomSupAirOutTemp( 0.0 ),
			NomSecAirVolFlow( 0.0 ),
			NomSecAirInTemp( 0.0 ),
			NomElecPower( 0.0 ),
			UA0( 0.0 ),
			mTSup0( 0.0 ),
			mTSec0( 0.0 ),
			NomSupAirMassFlow( 0.0 ),
			NomSecAirMassFlow( 0.0 ),
			SupInletNode( 0 ),
			SupOutletNode( 0 ),
			SecInletNode( 0 ),
			SecOutletNode( 0 ),
			SupInTemp( 0.0 ),
			SupInHumRat( 0.0 ),
			SupInEnth( 0.0 ),
			SupInMassFlow( 0.0 ),
			SecInTemp( 0.0 ),
			SecInHumRat( 0.0 ),
			SecInEnth( 0.0 ),
			SecInMassFlow( 0.0 ),
			PerfDataIndex( 0 ),
			FaceArea( 0.0 ),
			UnbalancedWarningFlag( true ),
			HeatEffectSensible100( 0.0 ),
			HeatEffectSensible75( 0.0 ),
			HeatEffectLatent100( 0.0 ),
			HeatEffectLatent75( 0.0 ),
			CoolEffectSensible100( 0.0 ),
			CoolEffectSensible75( 0.0 ),
			CoolEffectLatent100( 0.0 ),
			CoolEffectLatent75( 0.0 ),
			HeatExchEconoMode( 0 ),
			ExchConfigNum( 0 ),
			ThresholdTemperature( 0.0 ),
			InitialDefrostTime( 0.0 ),
			RateofDefrostTimeIncrease( 0.0 ),
			DefrostFraction( 0.0 ),
			ControlToTemperatureSetPoint( false ),
			SupOutTemp( 0.0 ),
			SupOutHumRat( 0.0 ),
			SupOutEnth( 0.0 ),
			SupOutMassFlow( 0.0 ),
			SecOutTemp( 0.0 ),
			SecOutHumRat( 0.0 ),
			SecOutEnth( 0.0 ),
			SecOutMassFlow( 0.0 ),
			SensHeatingRate( 0.0 ),
			SensHeatingEnergy( 0.0 ),
			LatHeatingRate( 0.0 ),
			LatHeatingEnergy( 0.0 ),
			TotHeatingRate( 0.0 ),
			TotHeatingEnergy( 0.0 ),
			SensCoolingRate( 0.0 ),
			SensCoolingEnergy( 0.0 ),
			LatCoolingRate( 0.0 ),
			LatCoolingEnergy( 0.0 ),
			TotCoolingRate( 0.0 ),
			TotCoolingEnergy( 0.0 ),
			ElecUseEnergy( 0.0 ),
			ElecUseRate( 0.0 ),
			SensEffectiveness( 0.0 ),
			LatEffectiveness( 0.0 ),
			SupBypassMassFlow( 0.0 ),
			SecBypassMassFlow( 0.0 ),
			LowFlowErrCount( 0 ),
			LowFlowErrIndex( 0 ),
			UnBalancedErrCount( 0 ),
			UnBalancedErrIndex( 0 )
		{}

		// Member Constructor
		HeatExchCond(
			std::string const & Name, // name of component
			int const ExchTypeNum, // Integer equivalent to ExchType
			int const HeatExchPerfTypeNum, // Desiccant balanced heat exchanger performance data type num
			std::string const & HeatExchPerfName, // Desiccant balanced heat exchanger performance data name
			int const SchedPtr, // index of schedule
			int const FlowArr, // flow Arrangement:
			int const EconoLockOut, // 1: Yes;  0: No
			Real64 const hARatio, // ratio of supply side h*A to secondary side h*A
			Real64 const NomSupAirVolFlow, // nominal supply air volume flow rate (m3/s)
			Real64 const NomSupAirInTemp, // nominal supply air inlet temperature (C)
			Real64 const NomSupAirOutTemp, // nominal supply air outlet temperature (C)
			Real64 const NomSecAirVolFlow, // nominal secondary air volume flow rate (m3/s)
			Real64 const NomSecAirInTemp, // nominal secondary air inlet temperature (C)
			Real64 const NomElecPower, // nominal electric power consumption [W]
			Real64 const UA0, // (Uavg*A) at nominal condition
			Real64 const mTSup0, // product mDot*Tabs, supply  air, nominal cond.
			Real64 const mTSec0, // product mDot*Tabs, exhaust air, nominal cond
			Real64 const NomSupAirMassFlow, // nominal supply air mass flow rate (kg/s)
			Real64 const NomSecAirMassFlow, // nominal secondary air mass flow rate (kg/s)
			int const SupInletNode, // supply air inlet node number
			int const SupOutletNode, // supply air outlet node number
			int const SecInletNode, // secondary air inlet node number
			int const SecOutletNode, // secondary air outlet node number
			Real64 const SupInTemp, // supply air inlet temperature (C)
			Real64 const SupInHumRat, // supply air inlet humidity ratio (kg water/kg dry air)
			Real64 const SupInEnth, // supply air inlet enthalpy (J/kg)
			Real64 const SupInMassFlow, // supply air inlet mass flow rate (kg/s)
			Real64 const SecInTemp, // secondary air inlet temperature (C)
			Real64 const SecInHumRat, // secondary air inlet humidity ratio (kg water/kg dry air)
			Real64 const SecInEnth, // secondary air inlet enthalpy (J/kg)
			Real64 const SecInMassFlow, // secondary air inlet mass flow rate (kg/s)
			int const PerfDataIndex, // Performance data index allocating performance data number to heat exchanger
			Real64 const FaceArea, // face area of balanced desiccant heat exchangers to determine face velocity [m2]
			bool const UnbalancedWarningFlag, // Used to print one-time warning when unbalanced flow exists (then set to FALSE)
			Real64 const HeatEffectSensible100, // heating sensible effectiveness at 100% rated air flow
			Real64 const HeatEffectSensible75, // heating sensible effectiveness at 75% rated air flow
			Real64 const HeatEffectLatent100, // heating latent effectiveness at 100% rated air flow
			Real64 const HeatEffectLatent75, // heating latent effectiveness at 75% rated air flow
			Real64 const CoolEffectSensible100, // cooling sensible effectiveness at 100% rated air flow
			Real64 const CoolEffectSensible75, // cooling sensible effectiveness at 75% rated air flow
			Real64 const CoolEffectLatent100, // cooling latent effectiveness at 100% rated air flow
			Real64 const CoolEffectLatent75, // cooling latent effectiveness at 75% rated air flow
			int const HeatExchEconoMode, // generic heat exchanger economize mode option
			int const ExchConfigNum, // parameter equivalent of HX configuration, plate or rotary
			std::string const & FrostControlType, // type of frost control used if any
			Real64 const ThresholdTemperature, // threshold temperature for frost control
			Real64 const InitialDefrostTime, // initial defrost time
			Real64 const RateofDefrostTimeIncrease, // rate of change of defrost time
			Real64 const DefrostFraction, // fraction of time HX is in frost control mode
			bool const ControlToTemperatureSetPoint, // temperature control flag for generic HX
			Real64 const SupOutTemp, // supply air outlet temperature (C)
			Real64 const SupOutHumRat, // supply air outlet humidity ratio (kg water/kg dry air)
			Real64 const SupOutEnth, // supply air outlet enthalpy (J/kg)
			Real64 const SupOutMassFlow, // supply air outlet mass flow rate (kg/s)
			Real64 const SecOutTemp, // secondary air outlet temperature (C)
			Real64 const SecOutHumRat, // secondary air outlet humidity ratio (kg water/kg dry air)
			Real64 const SecOutEnth, // secondary air outlet enthalpy (J/kg)
			Real64 const SecOutMassFlow, // secondary air outlet mass flow rate (kg/s)
			Real64 const SensHeatingRate, // rate of sensible heat being added to the supply (primary) air [W]
			Real64 const SensHeatingEnergy, // sensible heat added to the supply (primary) air [J]
			Real64 const LatHeatingRate, // rate of latent heat being added to the supply (primary) air [W]
			Real64 const LatHeatingEnergy, // latent heat added to the supply (primary) air [J]
			Real64 const TotHeatingRate, // rate of total heat being added to the supply (primary) air [W]
			Real64 const TotHeatingEnergy, // total heat added to the supply (primary) air [J]
			Real64 const SensCoolingRate, // rate of sensible heat being removed from the supply (primary) air [W]
			Real64 const SensCoolingEnergy, // sensible heat removed from the supply (primary) air [J]
			Real64 const LatCoolingRate, // rate of latent heat being removed from the supply (primary) air [W]
			Real64 const LatCoolingEnergy, // latent heat removed from the supply (primary) air [J]
			Real64 const TotCoolingRate, // rate of total heat being removed from the supply (primary) air [W]
			Real64 const TotCoolingEnergy, // total heat removed from the supply (primary) air [J]
			Real64 const ElecUseEnergy, // electricity consumption [J]
			Real64 const ElecUseRate, // electricity consumption rate [W]
			Real64 const SensEffectiveness, // heat exchanger sensible effectiveness [-]
			Real64 const LatEffectiveness, // heat exchanger latent effectiveness [-]
			Real64 const SupBypassMassFlow, // supply air mass flow rate bypassing the heat exchanger [kg/s]
			Real64 const SecBypassMassFlow, // secondary air mass flow rate bypassing the heat exchanger [kg/s]
			int const LowFlowErrCount, // Counter for recurring warning message
			int const LowFlowErrIndex, // Index to recurring warning message
			int const UnBalancedErrCount, // Counter for recurring warning message
			int const UnBalancedErrIndex // Index to recurring warning message
		) :
			Name( Name ),
			ExchTypeNum( ExchTypeNum ),
			HeatExchPerfTypeNum( HeatExchPerfTypeNum ),
			HeatExchPerfName( HeatExchPerfName ),
			SchedPtr( SchedPtr ),
			FlowArr( FlowArr ),
			EconoLockOut( EconoLockOut ),
			hARatio( hARatio ),
			NomSupAirVolFlow( NomSupAirVolFlow ),
			NomSupAirInTemp( NomSupAirInTemp ),
			NomSupAirOutTemp( NomSupAirOutTemp ),
			NomSecAirVolFlow( NomSecAirVolFlow ),
			NomSecAirInTemp( NomSecAirInTemp ),
			NomElecPower( NomElecPower ),
			UA0( UA0 ),
			mTSup0( mTSup0 ),
			mTSec0( mTSec0 ),
			NomSupAirMassFlow( NomSupAirMassFlow ),
			NomSecAirMassFlow( NomSecAirMassFlow ),
			SupInletNode( SupInletNode ),
			SupOutletNode( SupOutletNode ),
			SecInletNode( SecInletNode ),
			SecOutletNode( SecOutletNode ),
			SupInTemp( SupInTemp ),
			SupInHumRat( SupInHumRat ),
			SupInEnth( SupInEnth ),
			SupInMassFlow( SupInMassFlow ),
			SecInTemp( SecInTemp ),
			SecInHumRat( SecInHumRat ),
			SecInEnth( SecInEnth ),
			SecInMassFlow( SecInMassFlow ),
			PerfDataIndex( PerfDataIndex ),
			FaceArea( FaceArea ),
			UnbalancedWarningFlag( UnbalancedWarningFlag ),
			HeatEffectSensible100( HeatEffectSensible100 ),
			HeatEffectSensible75( HeatEffectSensible75 ),
			HeatEffectLatent100( HeatEffectLatent100 ),
			HeatEffectLatent75( HeatEffectLatent75 ),
			CoolEffectSensible100( CoolEffectSensible100 ),
			CoolEffectSensible75( CoolEffectSensible75 ),
			CoolEffectLatent100( CoolEffectLatent100 ),
			CoolEffectLatent75( CoolEffectLatent75 ),
			HeatExchEconoMode( HeatExchEconoMode ),
			ExchConfigNum( ExchConfigNum ),
			FrostControlType( FrostControlType ),
			ThresholdTemperature( ThresholdTemperature ),
			InitialDefrostTime( InitialDefrostTime ),
			RateofDefrostTimeIncrease( RateofDefrostTimeIncrease ),
			DefrostFraction( DefrostFraction ),
			ControlToTemperatureSetPoint( ControlToTemperatureSetPoint ),
			SupOutTemp( SupOutTemp ),
			SupOutHumRat( SupOutHumRat ),
			SupOutEnth( SupOutEnth ),
			SupOutMassFlow( SupOutMassFlow ),
			SecOutTemp( SecOutTemp ),
			SecOutHumRat( SecOutHumRat ),
			SecOutEnth( SecOutEnth ),
			SecOutMassFlow( SecOutMassFlow ),
			SensHeatingRate( SensHeatingRate ),
			SensHeatingEnergy( SensHeatingEnergy ),
			LatHeatingRate( LatHeatingRate ),
			LatHeatingEnergy( LatHeatingEnergy ),
			TotHeatingRate( TotHeatingRate ),
			TotHeatingEnergy( TotHeatingEnergy ),
			SensCoolingRate( SensCoolingRate ),
			SensCoolingEnergy( SensCoolingEnergy ),
			LatCoolingRate( LatCoolingRate ),
			LatCoolingEnergy( LatCoolingEnergy ),
			TotCoolingRate( TotCoolingRate ),
			TotCoolingEnergy( TotCoolingEnergy ),
			ElecUseEnergy( ElecUseEnergy ),
			ElecUseRate( ElecUseRate ),
			SensEffectiveness( SensEffectiveness ),
			LatEffectiveness( LatEffectiveness ),
			SupBypassMassFlow( SupBypassMassFlow ),
			SecBypassMassFlow( SecBypassMassFlow ),
			LowFlowErrCount( LowFlowErrCount ),
			LowFlowErrIndex( LowFlowErrIndex ),
			UnBalancedErrCount( UnBalancedErrCount ),
			UnBalancedErrIndex( UnBalancedErrIndex )
		{}

	};

	struct BalancedDesDehumPerfData
	{
		// Members
		// User Input data
		std::string Name; // unique name of balanced desiccant performance data type object
		std::string PerfType; // Type of performance data set
		Real64 NomSupAirVolFlow; // nominal supply air volumetric flow rate m^3/s
		Real64 NomProcAirFaceVel; // nominal process air face velocity m/s
		Real64 NomElecPower; // nominal electric power consumption [W]
		// regeneration outlet temperature equation coefficients and limits
		Real64 B1; // constant coefficient for outlet regeneration temprature equation
		Real64 B2; // regen inlet humrat coeff for outlet regen temperature equation
		Real64 B3; // regen inlet temp coeff for outlet regen temprature equation
		Real64 B4; // (regen in humrat/regen in temp) coeff for outlet regen temp eq
		Real64 B5; // process inlet humrat coeff for outlet regen temp equation
		Real64 B6; // process inlet temp coeff for outlet regen temp equation
		Real64 B7; // (process in humrat/proc in temp) coeff for outlet regen temp eq
		Real64 B8; // process, regen face velocity coeff for outlet regen temp eq
		Real64 T_MinRegenAirInTemp; // min allowable regen inlet air temperature [C]
		Real64 T_MaxRegenAirInTemp; // max allowable regen inlet air temperature [C]
		Real64 T_MinRegenAirInHumRat; // min allowable regen inlet air humidity ratio [kg water / kg air]
		Real64 T_MaxRegenAirInHumRat; // max allowable regen inlet air humidity ratio [kg water / kg air]
		Real64 T_MinProcAirInTemp; // min allowable process inlet air temperature [C]
		Real64 T_MaxProcAirInTemp; // max allowable process inlet air temperature [C]
		Real64 T_MinProcAirInHumRat; // min allowable process inlet air humidity ratio [kg water/kg air]
		Real64 T_MaxProcAirInHumRat; // max allowable process inlet air humidity ratio [kg water/kg air]
		Real64 T_MinFaceVel; // min allowable process, regen face velocity [m/s]
		Real64 T_MaxFaceVel; // max allowable process, regen face velocity [m/s]
		Real64 MinRegenAirOutTemp; // min allowable regen outlet air temperature [C]
		Real64 MaxRegenAirOutTemp; // max allowable regen outlet air temperature [C]
		Real64 T_MinRegenAirInRelHum; // min allowable regen inlet air relative humidity [%]
		Real64 T_MaxRegenAirInRelHum; // max allowable regen inlet air relative humidity [%]
		Real64 T_MinProcAirInRelHum; // min allowable process inlet air relative humidity [%]
		Real64 T_MaxProcAirInRelHum; // max allowable process inlet air relative humidity [%]
		// regeneration outlet humidity ratio equation coefficients and limits
		Real64 C1; // constant coeff for outlet regen humidity ratio equation
		Real64 C2; // regen inlet humrat coeff for outlet regen humidity ratio eq
		Real64 C3; // regen inlet temp coeff for outlet regen humidity ratio equation
		Real64 C4; // (regen in humrat/regen in temp) coeff for outlet regen humrat eq
		Real64 C5; // process inlet humrat coeff for outlet regen humidity ratio eq
		Real64 C6; // process inlet temp coeff for outlet regen humidity ratio eq
		Real64 C7; // (proc in humrat/proc in temp) coeff for outlet regen humrat eq
		Real64 C8; // process, regen face velocity coeff for outlet regen humrat eq
		Real64 H_MinRegenAirInTemp; // min allowable regen inlet air temperature [C]
		Real64 H_MaxRegenAirInTemp; // max allowable regen inlet air temperature [C]
		Real64 H_MinRegenAirInHumRat; // min allowable regen inlet air humidity ratio [kg water / kg air]
		Real64 H_MaxRegenAirInHumRat; // max allowable regen inlet air humidity ratio [kg water / kg air]
		Real64 H_MinProcAirInTemp; // min allowable process inlet air temperature [C]
		Real64 H_MaxProcAirInTemp; // max allowable process inlet air temperature [C]
		Real64 H_MinProcAirInHumRat; // min allowable process inlet air humidity ratio [kg water/kg air]
		Real64 H_MaxProcAirInHumRat; // max allowable process inlet air humidity ratio [kg water/kg air]
		Real64 H_MinFaceVel; // min allowable process, regen face velocity [m/s]
		Real64 H_MaxFaceVel; // max allowable process, regen face velocity [m/s]
		Real64 MinRegenAirOutHumRat; // min allowable regen outlet air temperature [C]
		Real64 MaxRegenAirOutHumRat; // max allowable regen outlet air temperature [C]
		Real64 H_MinRegenAirInRelHum; // min allowable regen inlet air relative humidity [%]
		Real64 H_MaxRegenAirInRelHum; // max allowable regen inlet air relative humidity [%]
		Real64 H_MinProcAirInRelHum; // min allowable process inlet air relative humidity [%]
		Real64 H_MaxProcAirInRelHum; // max allowable process inlet air relative humidity [%]
		// for model bound checking
		// regen inlet relative humidity for temperature equation
		bool PrintRegenInRelHumTempMess; // - flag to print regen in RH error message for temp eq
		int RegenInRelHumTempErrIndex; // - index to recurring error struc for regen outlet hum rat
		int RegenInRelHumTempErrorCount; // - counter if regen outlet temp limits are exceeded
		std::string RegenInRelHumTempBuffer1; // - buffer for RegenOutHumRat warn mess on following timstep
		std::string RegenInRelHumTempBuffer2; // - buffer for RegenOutHumRat warn mess on following timstep
		std::string RegenInRelHumTempBuffer3; // - buffer for RegenOutHumRat warn mess on following timstep
		Real64 RegenInRelHumTempLast; // - last value of regen outlet humidity ratio
		// process inlet relative humidity for temperature equation
		bool PrintProcInRelHumTempMess; // - flag to print regen in RH error message for temp eq
		int ProcInRelHumTempErrIndex; // - index to recurring error struc for regen outlet hum rat
		int ProcInRelHumTempErrorCount; // - counter if regen outlet temp limits are exceeded
		std::string ProcInRelHumTempBuffer1; // - buffer for RegenOutHumRat warn mess on following timstep
		std::string ProcInRelHumTempBuffer2; // - buffer for RegenOutHumRat warn mess on following timstep
		std::string ProcInRelHumTempBuffer3; // - buffer for RegenOutHumRat warn mess on following timstep
		Real64 ProcInRelHumTempLast; // - last value of regen outlet humidity ratio
		// regen inlet relative humidity for humidity ratio equation
		bool PrintRegenInRelHumHumRatMess; // - flag to print regen in RH error message for temp eq
		int RegenInRelHumHumRatErrIndex; // - index to recurring error struc for regen outlet hum rat
		int RegenInRelHumHumRatErrorCount; // - counter if regen outlet temp limits are exceeded
		std::string RegenInRelHumHumRatBuffer1; // - buffer for RegenOutHumRat warn mess on following timstep
		std::string RegenInRelHumHumRatBuffer2; // - buffer for RegenOutHumRat warn mess on following timstep
		std::string RegenInRelHumHumRatBuffer3; // - buffer for RegenOutHumRat warn mess on following timstep
		Real64 RegenInRelHumHumRatLast; // - last value of regen outlet humidity ratio
		// process inlet relative humidity for humidity ratio equation
		bool PrintProcInRelHumHumRatMess; // - flag to print regen in RH error message for temp eq
		int ProcInRelHumHumRatErrIndex; // - index to recurring error struc for regen outlet hum rat
		int ProcInRelHumHumRatErrorCount; // - counter if regen outlet temp limits are exceeded
		std::string ProcInRelHumHumRatBuffer1; // - buffer for RegenOutHumRat warn mess on following timstep
		std::string ProcInRelHumHumRatBuffer2; // - buffer for RegenOutHumRat warn mess on following timstep
		std::string ProcInRelHumHumRatBuffer3; // - buffer for RegenOutHumRat warn mess on following timstep
		Real64 ProcInRelHumHumRatLast; // - last value of regen outlet humidity ratio
		// regen outlet temp variables
		bool PrintT_RegenInTempMessage; // - flag to print regen in temp error message for temp eq
		bool PrintT_RegenInHumRatMessage; // - flag to print regen in humrat err message for temp eq
		bool PrintT_ProcInTempMessage; // - flag to print proc inlet temp err message for temp eq
		bool PrintT_ProcInHumRatMessage; // - flag to print process hum rat err message for temp eq
		bool PrintT_FaceVelMessage; // - flag to print face velocity error message
		bool PrintRegenOutTempMessage; // - flag to print regen outlet temp error message
		bool PrintRegenOutTempFailedMessage; // - flag to print regen outlet temp error message
		// regen outlet hum rat variables
		bool PrintH_RegenInTempMessage; // - flag to print regen in temp err message for humrat eq
		bool PrintH_RegenInHumRatMessage; // - flag for regen in humrat err message for humrat eq
		bool PrintH_ProcInTempMessage; // - flag for process inlet temp err message for humrat eq
		bool PrintH_ProcInHumRatMessage; // - flag for process hum rat error message for hum rat eq
		bool PrintH_FaceVelMessage; // - flag for face velocity error message
		bool PrintRegenOutHumRatMessage; // - flag for regen outlet hum rat error message
		bool PrintRegenInHumRatMessage; // - flag for regen outlet hum rat error message
		// used when regen outlet humrat is below regen inlet humrat, verify coefficients warning issued
		bool PrintRegenOutHumRatFailedMess; // - flag for regen outlet hum rat error message
		int RegenOutHumRatFailedErrIndex; // - index to recurring error struc for regen outlet hum rat
		int RegenOutHumRatFailedErrorCount; // - counter if regen outlet temp limits are exceeded
		std::string RegenOutHumRatFailedBuffer1; // - buffer for RegenOutHumRat warn mess on following timstep
		std::string RegenOutHumRatFailedBuffer2; // - buffer for RegenOutHumRat warn mess on following timstep
		std::string RegenOutHumRatFailedBuffer3; // - buffer for RegenOutHumRat warn mess on following timstep
		Real64 RegenOutHumRatFailedLast; // - last value of regen outlet humidity ratio
		// used when regen and process mass flow rates are not equal to within 2%
		bool PrintImbalancedMassFlowMess; // - flag for imbalanced regen and process mass flow rate
		int ImbalancedFlowErrIndex; // - index to recurring error struc for imbalanced flow
		int ImbalancedMassFlowErrorCount; // - counter for imbalanced regen and process mass flow rate
		std::string ImbalancedMassFlowBuffer1; // - buffer for imbalanced regen and process mass flow rate
		std::string ImbalancedMassFlowBuffer2; // - buffer for imbalanced regen and process mass flow rate
		std::string ImbalancedMassFlowBuffer3; // - buffer for imbalanced regen and process mass flow rate
		Real64 ABSImbalancedFlow; // - last value of heat exchanger mass flow rate imbalance
		// regen outlet temp eqn
		int T_RegenInTempErrorCount; // - counter if regen inlet temp limits are exceeded
		int T_RegenInHumRatErrorCount; // - counter if regen inlet hum rat limits are exceeded
		int T_ProcInTempErrorCount; // - counter if process inlet temperature limits are exceeded
		int T_ProcInHumRatErrorCount; // - counter if process inlet hum rat limits are exceeded
		int T_FaceVelErrorCount; // - counter if regen and proc face vel limits are exceeded
		int T_RegenInTempErrIndex; // - index to recurring error structure for regen inlet temp
		int T_RegenInHumRatErrIndex; // - index to recurring error structure for regen in humrat
		int T_ProcInTempErrIndex; // - index to recurring error structure for process in temp
		int T_ProcInHumRatErrIndex; // - index to recurring error structure for process in humrat
		int T_FaceVelocityErrIndex; // - index to recurring err struc for proc and regen face vel
		int RegenOutTempErrorCount; // - counter if regen outlet temp limits are exceeded
		int RegenOutTempErrIndex; // - index to recurring error structure for regen outlet temp
		// used when regen outlet temperature is above regen inlet temperature, verify coefficients warning issued
		int RegenOutTempFailedErrorCount; // - counter if regen outlet temp limits are exceeded
		int RegenOutTempFailedErrIndex; // - index to recurring error structure for regen outlet temp
		std::string RegenOutTempFailedBuffer1; // - buffer for RegenOutTemp warn messages on following timestep
		std::string RegenOutTempFailedBuffer2; // - buffer for RegenOutTemp warn messages on following timestep
		std::string RegenOutTempFailedBuffer3; // - buffer for RegenOutTemp warn messages on following timestep
		Real64 RegenOutTempFailedLast; // - last value of regen outlet temp
		// regen outlet hum rat eqn
		int H_RegenInTempErrorCount; // - counter if regen inlet temp limits are exceeded
		int H_RegenInHumRatErrorCount; // - counter if regen inlet hum rat limits are exceeded
		int H_ProcInTempErrorCount; // - counter if process inlet temperature limits are exceeded
		int H_ProcInHumRatErrorCount; // - counter if process inlet hum rat limits are exceeded
		int H_FaceVelErrorCount; // - counter if regen and proc face vel limits are exceeded
		int H_RegenInTempErrIndex; // - index to recurring error structure for regen inlet temp
		int H_RegenInHumRatErrIndex; // - index to recurring error struc for regen inlet humrat
		int H_ProcInTempErrIndex; // - index to recurring error struc for process inlet temp
		int H_ProcInHumRatErrIndex; // - index to recurring error struc for process inlet hum rat
		int H_FaceVelocityErrIndex; // - index to recurring err struc for proc and regen face vel
		int RegenOutHumRatErrorCount; // - counter if regen outlet temp limits are exceeded
		int RegenOutHumRatErrIndex; // - index to recurring error struc for regen outlet hum rat
		int RegenInHumRatErrorCount; // - counter if regen outlet temp limits are exceeded
		int RegenInHumRatErrIndex; // - index to recurring error struc for regen outlet hum rat
		// regen outlet temp variables                                   !- T_RegenInTemp = Regen inlet temperature
		std::string T_RegenInTempBuffer1; // - buffer for T_RegenInTemp warn message on following timestep
		std::string T_RegenInTempBuffer2; // - buffer for T_RegenInTemp warn message on following timestep
		std::string T_RegenInTempBuffer3; // - buffer for T_RegenInTemp warn message on following timestep
		Real64 T_RegenInTempLast; // - last value of regen inlet temp
		//- T_RegenInHumRat = Regen inlet humidity ratio
		std::string T_RegenInHumRatBuffer1; // - buffer for T_RegenInHumRat warn messag on following timestep
		std::string T_RegenInHumRatBuffer2; // - buffer for T_RegenInHumRat warn messag on following timestep
		std::string T_RegenInHumRatBuffer3; // - buffer for T_RegenInHumRat warn messag on following timestep
		Real64 T_RegenInHumRatLast; // - last value of regen inlet humidity ratio
		//- T_ProcInTemp = Process inlet temperature
		std::string T_ProcInTempBuffer1; // - buffer for T_ProcInTemp warning messag on following timestep
		std::string T_ProcInTempBuffer2; // - buffer for T_ProcInTemp warning messag on following timestep
		std::string T_ProcInTempBuffer3; // - buffer for T_ProcInTemp warning messag on following timestep
		Real64 T_ProcInTempLast; // - last value of process inlet temp
		//- T_ProcInHumRat = Process inlet humidity ratio
		std::string T_ProcInHumRatBuffer1; // - buffer for T_ProcInHumRat warn message on following timestep
		std::string T_ProcInHumRatBuffer2; // - buffer for T_ProcInHumRat warn message on following timestep
		std::string T_ProcInHumRatBuffer3; // - buffer for T_ProcInHumRat warn message on following timestep
		Real64 T_ProcInHumRatLast; // - last value of process inlet humidity ratio
		//- T_FaceVel = Process and regen face velocity
		std::string T_FaceVelBuffer1; // - buffer for T_FaceVel warning messages on following time step
		std::string T_FaceVelBuffer2; // - buffer for T_FaceVel warning messages on following time step
		std::string T_FaceVelBuffer3; // - buffer for T_FaceVel warning messages on following time step
		Real64 T_FaceVelLast; // - last value of process and regen face velocity
		//- T_RegenOutTemp = Regen outlet temperature
		std::string RegenOutTempBuffer1; // - buffer for RegenOutTemp warn messages on following timestep
		std::string RegenOutTempBuffer2; // - buffer for RegenOutTemp warn messages on following timestep
		std::string RegenOutTempBuffer3; // - buffer for RegenOutTemp warn messages on following timestep
		Real64 RegenOutTempLast; // - last value of regen outlet temp
		// regen outlet humidity ratio variables                         !- H_RegenInTemp = Regen inlet temperature
		std::string H_RegenInTempBuffer1; // - buffer for H_RegenInTemp warn message on following time step
		std::string H_RegenInTempBuffer2; // - buffer for H_RegenInTemp warn message on following time step
		std::string H_RegenInTempBuffer3; // - buffer for H_RegenInTemp warn message on following time step
		Real64 H_RegenInTempLast; // - last value of regen inlet temp
		//- H_RegenInHumRat = Regen inlet humidity ratio
		std::string H_RegenInHumRatBuffer1; // - buffer for H_RegenInHumRat warn messag on following timestep
		std::string H_RegenInHumRatBuffer2; // - buffer for H_RegenInHumRat warn messag on following timestep
		std::string H_RegenInHumRatBuffer3; // - buffer for H_RegenInHumRat warn messag on following timestep
		Real64 H_RegenInHumRatLast; // - last value of regen inlet humidity ratio
		//- H_ProcInTemp = Process inlet temperature
		std::string H_ProcInTempBuffer1; // - buffer for H_ProcInTemp warn messages on following time step
		std::string H_ProcInTempBuffer2; // - buffer for H_ProcInTemp warn messages on following time step
		std::string H_ProcInTempBuffer3; // - buffer for H_ProcInTemp warn messages on following time step
		Real64 H_ProcInTempLast; // - last value of process inlet temp
		//- H_ProcInHumRat = Process inlet humidity ratio
		std::string H_ProcInHumRatBuffer1; // - buffer for H_ProcInHumRat warn message on following timestep
		std::string H_ProcInHumRatBuffer2; // - buffer for H_ProcInHumRat warn message on following timestep
		std::string H_ProcInHumRatBuffer3; // - buffer for H_ProcInHumRat warn message on following timestep
		Real64 H_ProcInHumRatLast; // - last value of process inlet humidity ratio
		//- H_FaceVel = Process and regen face velocity
		std::string H_FaceVelBuffer1; // - buffer for H_FaceVel warning messages on following time step
		std::string H_FaceVelBuffer2; // - buffer for H_FaceVel warning messages on following time step
		std::string H_FaceVelBuffer3; // - buffer for H_FaceVel warning messages on following time step
		Real64 H_FaceVelLast; // - last value of process and regen face velocity
		//- H_RegenOutTemp = Regen outlet temperature
		std::string RegenOutHumRatBuffer1; // - buffer for RegenOutHumRat warn message on following timestep
		std::string RegenOutHumRatBuffer2; // - buffer for RegenOutHumRat warn message on following timestep
		std::string RegenOutHumRatBuffer3; // - buffer for RegenOutHumRat warn message on following timestep
		Real64 RegenOutHumRatLast; // - last value of regen outlet humidity ratio

		// Default Constructor
		BalancedDesDehumPerfData() :
			NomSupAirVolFlow( 0.0 ),
			NomProcAirFaceVel( 0.0 ),
			NomElecPower( 0.0 ),
			B1( 0.0 ),
			B2( 0.0 ),
			B3( 0.0 ),
			B4( 0.0 ),
			B5( 0.0 ),
			B6( 0.0 ),
			B7( 0.0 ),
			B8( 0.0 ),
			T_MinRegenAirInTemp( 0.0 ),
			T_MaxRegenAirInTemp( 0.0 ),
			T_MinRegenAirInHumRat( 0.0 ),
			T_MaxRegenAirInHumRat( 0.0 ),
			T_MinProcAirInTemp( 0.0 ),
			T_MaxProcAirInTemp( 0.0 ),
			T_MinProcAirInHumRat( 0.0 ),
			T_MaxProcAirInHumRat( 0.0 ),
			T_MinFaceVel( 0.0 ),
			T_MaxFaceVel( 0.0 ),
			MinRegenAirOutTemp( 0.0 ),
			MaxRegenAirOutTemp( 0.0 ),
			T_MinRegenAirInRelHum( 0.0 ),
			T_MaxRegenAirInRelHum( 0.0 ),
			T_MinProcAirInRelHum( 0.0 ),
			T_MaxProcAirInRelHum( 0.0 ),
			C1( 0.0 ),
			C2( 0.0 ),
			C3( 0.0 ),
			C4( 0.0 ),
			C5( 0.0 ),
			C6( 0.0 ),
			C7( 0.0 ),
			C8( 0.0 ),
			H_MinRegenAirInTemp( 0.0 ),
			H_MaxRegenAirInTemp( 0.0 ),
			H_MinRegenAirInHumRat( 0.0 ),
			H_MaxRegenAirInHumRat( 0.0 ),
			H_MinProcAirInTemp( 0.0 ),
			H_MaxProcAirInTemp( 0.0 ),
			H_MinProcAirInHumRat( 0.0 ),
			H_MaxProcAirInHumRat( 0.0 ),
			H_MinFaceVel( 0.0 ),
			H_MaxFaceVel( 0.0 ),
			MinRegenAirOutHumRat( 0.0 ),
			MaxRegenAirOutHumRat( 0.0 ),
			H_MinRegenAirInRelHum( 0.0 ),
			H_MaxRegenAirInRelHum( 0.0 ),
			H_MinProcAirInRelHum( 0.0 ),
			H_MaxProcAirInRelHum( 0.0 ),
			PrintRegenInRelHumTempMess( false ),
			RegenInRelHumTempErrIndex( 0 ),
			RegenInRelHumTempErrorCount( 0 ),
			RegenInRelHumTempLast( 0.0 ),
			PrintProcInRelHumTempMess( false ),
			ProcInRelHumTempErrIndex( 0 ),
			ProcInRelHumTempErrorCount( 0 ),
			ProcInRelHumTempLast( 0.0 ),
			PrintRegenInRelHumHumRatMess( false ),
			RegenInRelHumHumRatErrIndex( 0 ),
			RegenInRelHumHumRatErrorCount( 0 ),
			RegenInRelHumHumRatLast( 0.0 ),
			PrintProcInRelHumHumRatMess( false ),
			ProcInRelHumHumRatErrIndex( 0 ),
			ProcInRelHumHumRatErrorCount( 0 ),
			ProcInRelHumHumRatLast( 0.0 ),
			PrintT_RegenInTempMessage( false ),
			PrintT_RegenInHumRatMessage( false ),
			PrintT_ProcInTempMessage( false ),
			PrintT_ProcInHumRatMessage( false ),
			PrintT_FaceVelMessage( false ),
			PrintRegenOutTempMessage( false ),
			PrintRegenOutTempFailedMessage( false ),
			PrintH_RegenInTempMessage( false ),
			PrintH_RegenInHumRatMessage( false ),
			PrintH_ProcInTempMessage( false ),
			PrintH_ProcInHumRatMessage( false ),
			PrintH_FaceVelMessage( false ),
			PrintRegenOutHumRatMessage( false ),
			PrintRegenInHumRatMessage( false ),
			PrintRegenOutHumRatFailedMess( false ),
			RegenOutHumRatFailedErrIndex( 0 ),
			RegenOutHumRatFailedErrorCount( 0 ),
			RegenOutHumRatFailedLast( 0.0 ),
			PrintImbalancedMassFlowMess( false ),
			ImbalancedFlowErrIndex( 0 ),
			ImbalancedMassFlowErrorCount( 0 ),
			ABSImbalancedFlow( 0.0 ),
			T_RegenInTempErrorCount( 0 ),
			T_RegenInHumRatErrorCount( 0 ),
			T_ProcInTempErrorCount( 0 ),
			T_ProcInHumRatErrorCount( 0 ),
			T_FaceVelErrorCount( 0 ),
			T_RegenInTempErrIndex( 0 ),
			T_RegenInHumRatErrIndex( 0 ),
			T_ProcInTempErrIndex( 0 ),
			T_ProcInHumRatErrIndex( 0 ),
			T_FaceVelocityErrIndex( 0 ),
			RegenOutTempErrorCount( 0 ),
			RegenOutTempErrIndex( 0 ),
			RegenOutTempFailedErrorCount( 0 ),
			RegenOutTempFailedErrIndex( 0 ),
			RegenOutTempFailedLast( 0.0 ),
			H_RegenInTempErrorCount( 0 ),
			H_RegenInHumRatErrorCount( 0 ),
			H_ProcInTempErrorCount( 0 ),
			H_ProcInHumRatErrorCount( 0 ),
			H_FaceVelErrorCount( 0 ),
			H_RegenInTempErrIndex( 0 ),
			H_RegenInHumRatErrIndex( 0 ),
			H_ProcInTempErrIndex( 0 ),
			H_ProcInHumRatErrIndex( 0 ),
			H_FaceVelocityErrIndex( 0 ),
			RegenOutHumRatErrorCount( 0 ),
			RegenOutHumRatErrIndex( 0 ),
			RegenInHumRatErrorCount( 0 ),
			RegenInHumRatErrIndex( 0 ),
			T_RegenInTempLast( 0.0 ),
			T_RegenInHumRatLast( 0.0 ),
			T_ProcInTempLast( 0.0 ),
			T_ProcInHumRatLast( 0.0 ),
			T_FaceVelLast( 0.0 ),
			RegenOutTempLast( 0.0 ),
			H_RegenInTempLast( 0.0 ),
			H_RegenInHumRatLast( 0.0 ),
			H_ProcInTempLast( 0.0 ),
			H_ProcInHumRatLast( 0.0 ),
			H_FaceVelLast( 0.0 ),
			RegenOutHumRatLast( 0.0 )
		{}

		// Member Constructor
		BalancedDesDehumPerfData(
			std::string const & Name, // unique name of balanced desiccant performance data type object
			std::string const & PerfType, // Type of performance data set
			Real64 const NomSupAirVolFlow, // nominal supply air volumetric flow rate m^3/s
			Real64 const NomProcAirFaceVel, // nominal process air face velocity m/s
			Real64 const NomElecPower, // nominal electric power consumption [W]
			Real64 const B1, // constant coefficient for outlet regeneration temprature equation
			Real64 const B2, // regen inlet humrat coeff for outlet regen temperature equation
			Real64 const B3, // regen inlet temp coeff for outlet regen temprature equation
			Real64 const B4, // (regen in humrat/regen in temp) coeff for outlet regen temp eq
			Real64 const B5, // process inlet humrat coeff for outlet regen temp equation
			Real64 const B6, // process inlet temp coeff for outlet regen temp equation
			Real64 const B7, // (process in humrat/proc in temp) coeff for outlet regen temp eq
			Real64 const B8, // process, regen face velocity coeff for outlet regen temp eq
			Real64 const T_MinRegenAirInTemp, // min allowable regen inlet air temperature [C]
			Real64 const T_MaxRegenAirInTemp, // max allowable regen inlet air temperature [C]
			Real64 const T_MinRegenAirInHumRat, // min allowable regen inlet air humidity ratio [kg water / kg air]
			Real64 const T_MaxRegenAirInHumRat, // max allowable regen inlet air humidity ratio [kg water / kg air]
			Real64 const T_MinProcAirInTemp, // min allowable process inlet air temperature [C]
			Real64 const T_MaxProcAirInTemp, // max allowable process inlet air temperature [C]
			Real64 const T_MinProcAirInHumRat, // min allowable process inlet air humidity ratio [kg water/kg air]
			Real64 const T_MaxProcAirInHumRat, // max allowable process inlet air humidity ratio [kg water/kg air]
			Real64 const T_MinFaceVel, // min allowable process, regen face velocity [m/s]
			Real64 const T_MaxFaceVel, // max allowable process, regen face velocity [m/s]
			Real64 const MinRegenAirOutTemp, // min allowable regen outlet air temperature [C]
			Real64 const MaxRegenAirOutTemp, // max allowable regen outlet air temperature [C]
			Real64 const T_MinRegenAirInRelHum, // min allowable regen inlet air relative humidity [%]
			Real64 const T_MaxRegenAirInRelHum, // max allowable regen inlet air relative humidity [%]
			Real64 const T_MinProcAirInRelHum, // min allowable process inlet air relative humidity [%]
			Real64 const T_MaxProcAirInRelHum, // max allowable process inlet air relative humidity [%]
			Real64 const C1, // constant coeff for outlet regen humidity ratio equation
			Real64 const C2, // regen inlet humrat coeff for outlet regen humidity ratio eq
			Real64 const C3, // regen inlet temp coeff for outlet regen humidity ratio equation
			Real64 const C4, // (regen in humrat/regen in temp) coeff for outlet regen humrat eq
			Real64 const C5, // process inlet humrat coeff for outlet regen humidity ratio eq
			Real64 const C6, // process inlet temp coeff for outlet regen humidity ratio eq
			Real64 const C7, // (proc in humrat/proc in temp) coeff for outlet regen humrat eq
			Real64 const C8, // process, regen face velocity coeff for outlet regen humrat eq
			Real64 const H_MinRegenAirInTemp, // min allowable regen inlet air temperature [C]
			Real64 const H_MaxRegenAirInTemp, // max allowable regen inlet air temperature [C]
			Real64 const H_MinRegenAirInHumRat, // min allowable regen inlet air humidity ratio [kg water / kg air]
			Real64 const H_MaxRegenAirInHumRat, // max allowable regen inlet air humidity ratio [kg water / kg air]
			Real64 const H_MinProcAirInTemp, // min allowable process inlet air temperature [C]
			Real64 const H_MaxProcAirInTemp, // max allowable process inlet air temperature [C]
			Real64 const H_MinProcAirInHumRat, // min allowable process inlet air humidity ratio [kg water/kg air]
			Real64 const H_MaxProcAirInHumRat, // max allowable process inlet air humidity ratio [kg water/kg air]
			Real64 const H_MinFaceVel, // min allowable process, regen face velocity [m/s]
			Real64 const H_MaxFaceVel, // max allowable process, regen face velocity [m/s]
			Real64 const MinRegenAirOutHumRat, // min allowable regen outlet air temperature [C]
			Real64 const MaxRegenAirOutHumRat, // max allowable regen outlet air temperature [C]
			Real64 const H_MinRegenAirInRelHum, // min allowable regen inlet air relative humidity [%]
			Real64 const H_MaxRegenAirInRelHum, // max allowable regen inlet air relative humidity [%]
			Real64 const H_MinProcAirInRelHum, // min allowable process inlet air relative humidity [%]
			Real64 const H_MaxProcAirInRelHum, // max allowable process inlet air relative humidity [%]
			bool const PrintRegenInRelHumTempMess, // - flag to print regen in RH error message for temp eq
			int const RegenInRelHumTempErrIndex, // - index to recurring error struc for regen outlet hum rat
			int const RegenInRelHumTempErrorCount, // - counter if regen outlet temp limits are exceeded
			std::string const & RegenInRelHumTempBuffer1, // - buffer for RegenOutHumRat warn mess on following timstep
			std::string const & RegenInRelHumTempBuffer2, // - buffer for RegenOutHumRat warn mess on following timstep
			std::string const & RegenInRelHumTempBuffer3, // - buffer for RegenOutHumRat warn mess on following timstep
			Real64 const RegenInRelHumTempLast, // - last value of regen outlet humidity ratio
			bool const PrintProcInRelHumTempMess, // - flag to print regen in RH error message for temp eq
			int const ProcInRelHumTempErrIndex, // - index to recurring error struc for regen outlet hum rat
			int const ProcInRelHumTempErrorCount, // - counter if regen outlet temp limits are exceeded
			std::string const & ProcInRelHumTempBuffer1, // - buffer for RegenOutHumRat warn mess on following timstep
			std::string const & ProcInRelHumTempBuffer2, // - buffer for RegenOutHumRat warn mess on following timstep
			std::string const & ProcInRelHumTempBuffer3, // - buffer for RegenOutHumRat warn mess on following timstep
			Real64 const ProcInRelHumTempLast, // - last value of regen outlet humidity ratio
			bool const PrintRegenInRelHumHumRatMess, // - flag to print regen in RH error message for temp eq
			int const RegenInRelHumHumRatErrIndex, // - index to recurring error struc for regen outlet hum rat
			int const RegenInRelHumHumRatErrorCount, // - counter if regen outlet temp limits are exceeded
			std::string const & RegenInRelHumHumRatBuffer1, // - buffer for RegenOutHumRat warn mess on following timstep
			std::string const & RegenInRelHumHumRatBuffer2, // - buffer for RegenOutHumRat warn mess on following timstep
			std::string const & RegenInRelHumHumRatBuffer3, // - buffer for RegenOutHumRat warn mess on following timstep
			Real64 const RegenInRelHumHumRatLast, // - last value of regen outlet humidity ratio
			bool const PrintProcInRelHumHumRatMess, // - flag to print regen in RH error message for temp eq
			int const ProcInRelHumHumRatErrIndex, // - index to recurring error struc for regen outlet hum rat
			int const ProcInRelHumHumRatErrorCount, // - counter if regen outlet temp limits are exceeded
			std::string const & ProcInRelHumHumRatBuffer1, // - buffer for RegenOutHumRat warn mess on following timstep
			std::string const & ProcInRelHumHumRatBuffer2, // - buffer for RegenOutHumRat warn mess on following timstep
			std::string const & ProcInRelHumHumRatBuffer3, // - buffer for RegenOutHumRat warn mess on following timstep
			Real64 const ProcInRelHumHumRatLast, // - last value of regen outlet humidity ratio
			bool const PrintT_RegenInTempMessage, // - flag to print regen in temp error message for temp eq
			bool const PrintT_RegenInHumRatMessage, // - flag to print regen in humrat err message for temp eq
			bool const PrintT_ProcInTempMessage, // - flag to print proc inlet temp err message for temp eq
			bool const PrintT_ProcInHumRatMessage, // - flag to print process hum rat err message for temp eq
			bool const PrintT_FaceVelMessage, // - flag to print face velocity error message
			bool const PrintRegenOutTempMessage, // - flag to print regen outlet temp error message
			bool const PrintRegenOutTempFailedMessage, // - flag to print regen outlet temp error message
			bool const PrintH_RegenInTempMessage, // - flag to print regen in temp err message for humrat eq
			bool const PrintH_RegenInHumRatMessage, // - flag for regen in humrat err message for humrat eq
			bool const PrintH_ProcInTempMessage, // - flag for process inlet temp err message for humrat eq
			bool const PrintH_ProcInHumRatMessage, // - flag for process hum rat error message for hum rat eq
			bool const PrintH_FaceVelMessage, // - flag for face velocity error message
			bool const PrintRegenOutHumRatMessage, // - flag for regen outlet hum rat error message
			bool const PrintRegenInHumRatMessage, // - flag for regen outlet hum rat error message
			bool const PrintRegenOutHumRatFailedMess, // - flag for regen outlet hum rat error message
			int const RegenOutHumRatFailedErrIndex, // - index to recurring error struc for regen outlet hum rat
			int const RegenOutHumRatFailedErrorCount, // - counter if regen outlet temp limits are exceeded
			std::string const & RegenOutHumRatFailedBuffer1, // - buffer for RegenOutHumRat warn mess on following timstep
			std::string const & RegenOutHumRatFailedBuffer2, // - buffer for RegenOutHumRat warn mess on following timstep
			std::string const & RegenOutHumRatFailedBuffer3, // - buffer for RegenOutHumRat warn mess on following timstep
			Real64 const RegenOutHumRatFailedLast, // - last value of regen outlet humidity ratio
			bool const PrintImbalancedMassFlowMess, // - flag for imbalanced regen and process mass flow rate
			int const ImbalancedFlowErrIndex, // - index to recurring error struc for imbalanced flow
			int const ImbalancedMassFlowErrorCount, // - counter for imbalanced regen and process mass flow rate
			std::string const & ImbalancedMassFlowBuffer1, // - buffer for imbalanced regen and process mass flow rate
			std::string const & ImbalancedMassFlowBuffer2, // - buffer for imbalanced regen and process mass flow rate
			std::string const & ImbalancedMassFlowBuffer3, // - buffer for imbalanced regen and process mass flow rate
			Real64 const ABSImbalancedFlow, // - last value of heat exchanger mass flow rate imbalance
			int const T_RegenInTempErrorCount, // - counter if regen inlet temp limits are exceeded
			int const T_RegenInHumRatErrorCount, // - counter if regen inlet hum rat limits are exceeded
			int const T_ProcInTempErrorCount, // - counter if process inlet temperature limits are exceeded
			int const T_ProcInHumRatErrorCount, // - counter if process inlet hum rat limits are exceeded
			int const T_FaceVelErrorCount, // - counter if regen and proc face vel limits are exceeded
			int const T_RegenInTempErrIndex, // - index to recurring error structure for regen inlet temp
			int const T_RegenInHumRatErrIndex, // - index to recurring error structure for regen in humrat
			int const T_ProcInTempErrIndex, // - index to recurring error structure for process in temp
			int const T_ProcInHumRatErrIndex, // - index to recurring error structure for process in humrat
			int const T_FaceVelocityErrIndex, // - index to recurring err struc for proc and regen face vel
			int const RegenOutTempErrorCount, // - counter if regen outlet temp limits are exceeded
			int const RegenOutTempErrIndex, // - index to recurring error structure for regen outlet temp
			int const RegenOutTempFailedErrorCount, // - counter if regen outlet temp limits are exceeded
			int const RegenOutTempFailedErrIndex, // - index to recurring error structure for regen outlet temp
			std::string const & RegenOutTempFailedBuffer1, // - buffer for RegenOutTemp warn messages on following timestep
			std::string const & RegenOutTempFailedBuffer2, // - buffer for RegenOutTemp warn messages on following timestep
			std::string const & RegenOutTempFailedBuffer3, // - buffer for RegenOutTemp warn messages on following timestep
			Real64 const RegenOutTempFailedLast, // - last value of regen outlet temp
			int const H_RegenInTempErrorCount, // - counter if regen inlet temp limits are exceeded
			int const H_RegenInHumRatErrorCount, // - counter if regen inlet hum rat limits are exceeded
			int const H_ProcInTempErrorCount, // - counter if process inlet temperature limits are exceeded
			int const H_ProcInHumRatErrorCount, // - counter if process inlet hum rat limits are exceeded
			int const H_FaceVelErrorCount, // - counter if regen and proc face vel limits are exceeded
			int const H_RegenInTempErrIndex, // - index to recurring error structure for regen inlet temp
			int const H_RegenInHumRatErrIndex, // - index to recurring error struc for regen inlet humrat
			int const H_ProcInTempErrIndex, // - index to recurring error struc for process inlet temp
			int const H_ProcInHumRatErrIndex, // - index to recurring error struc for process inlet hum rat
			int const H_FaceVelocityErrIndex, // - index to recurring err struc for proc and regen face vel
			int const RegenOutHumRatErrorCount, // - counter if regen outlet temp limits are exceeded
			int const RegenOutHumRatErrIndex, // - index to recurring error struc for regen outlet hum rat
			int const RegenInHumRatErrorCount, // - counter if regen outlet temp limits are exceeded
			int const RegenInHumRatErrIndex, // - index to recurring error struc for regen outlet hum rat
			std::string const & T_RegenInTempBuffer1, // - buffer for T_RegenInTemp warn message on following timestep
			std::string const & T_RegenInTempBuffer2, // - buffer for T_RegenInTemp warn message on following timestep
			std::string const & T_RegenInTempBuffer3, // - buffer for T_RegenInTemp warn message on following timestep
			Real64 const T_RegenInTempLast, // - last value of regen inlet temp
			std::string const & T_RegenInHumRatBuffer1, // - buffer for T_RegenInHumRat warn messag on following timestep
			std::string const & T_RegenInHumRatBuffer2, // - buffer for T_RegenInHumRat warn messag on following timestep
			std::string const & T_RegenInHumRatBuffer3, // - buffer for T_RegenInHumRat warn messag on following timestep
			Real64 const T_RegenInHumRatLast, // - last value of regen inlet humidity ratio
			std::string const & T_ProcInTempBuffer1, // - buffer for T_ProcInTemp warning messag on following timestep
			std::string const & T_ProcInTempBuffer2, // - buffer for T_ProcInTemp warning messag on following timestep
			std::string const & T_ProcInTempBuffer3, // - buffer for T_ProcInTemp warning messag on following timestep
			Real64 const T_ProcInTempLast, // - last value of process inlet temp
			std::string const & T_ProcInHumRatBuffer1, // - buffer for T_ProcInHumRat warn message on following timestep
			std::string const & T_ProcInHumRatBuffer2, // - buffer for T_ProcInHumRat warn message on following timestep
			std::string const & T_ProcInHumRatBuffer3, // - buffer for T_ProcInHumRat warn message on following timestep
			Real64 const T_ProcInHumRatLast, // - last value of process inlet humidity ratio
			std::string const & T_FaceVelBuffer1, // - buffer for T_FaceVel warning messages on following time step
			std::string const & T_FaceVelBuffer2, // - buffer for T_FaceVel warning messages on following time step
			std::string const & T_FaceVelBuffer3, // - buffer for T_FaceVel warning messages on following time step
			Real64 const T_FaceVelLast, // - last value of process and regen face velocity
			std::string const & RegenOutTempBuffer1, // - buffer for RegenOutTemp warn messages on following timestep
			std::string const & RegenOutTempBuffer2, // - buffer for RegenOutTemp warn messages on following timestep
			std::string const & RegenOutTempBuffer3, // - buffer for RegenOutTemp warn messages on following timestep
			Real64 const RegenOutTempLast, // - last value of regen outlet temp
			std::string const & H_RegenInTempBuffer1, // - buffer for H_RegenInTemp warn message on following time step
			std::string const & H_RegenInTempBuffer2, // - buffer for H_RegenInTemp warn message on following time step
			std::string const & H_RegenInTempBuffer3, // - buffer for H_RegenInTemp warn message on following time step
			Real64 const H_RegenInTempLast, // - last value of regen inlet temp
			std::string const & H_RegenInHumRatBuffer1, // - buffer for H_RegenInHumRat warn messag on following timestep
			std::string const & H_RegenInHumRatBuffer2, // - buffer for H_RegenInHumRat warn messag on following timestep
			std::string const & H_RegenInHumRatBuffer3, // - buffer for H_RegenInHumRat warn messag on following timestep
			Real64 const H_RegenInHumRatLast, // - last value of regen inlet humidity ratio
			std::string const & H_ProcInTempBuffer1, // - buffer for H_ProcInTemp warn messages on following time step
			std::string const & H_ProcInTempBuffer2, // - buffer for H_ProcInTemp warn messages on following time step
			std::string const & H_ProcInTempBuffer3, // - buffer for H_ProcInTemp warn messages on following time step
			Real64 const H_ProcInTempLast, // - last value of process inlet temp
			std::string const & H_ProcInHumRatBuffer1, // - buffer for H_ProcInHumRat warn message on following timestep
			std::string const & H_ProcInHumRatBuffer2, // - buffer for H_ProcInHumRat warn message on following timestep
			std::string const & H_ProcInHumRatBuffer3, // - buffer for H_ProcInHumRat warn message on following timestep
			Real64 const H_ProcInHumRatLast, // - last value of process inlet humidity ratio
			std::string const & H_FaceVelBuffer1, // - buffer for H_FaceVel warning messages on following time step
			std::string const & H_FaceVelBuffer2, // - buffer for H_FaceVel warning messages on following time step
			std::string const & H_FaceVelBuffer3, // - buffer for H_FaceVel warning messages on following time step
			Real64 const H_FaceVelLast, // - last value of process and regen face velocity
			std::string const & RegenOutHumRatBuffer1, // - buffer for RegenOutHumRat warn message on following timestep
			std::string const & RegenOutHumRatBuffer2, // - buffer for RegenOutHumRat warn message on following timestep
			std::string const & RegenOutHumRatBuffer3, // - buffer for RegenOutHumRat warn message on following timestep
			Real64 const RegenOutHumRatLast // - last value of regen outlet humidity ratio
		) :
			Name( Name ),
			PerfType( PerfType ),
			NomSupAirVolFlow( NomSupAirVolFlow ),
			NomProcAirFaceVel( NomProcAirFaceVel ),
			NomElecPower( NomElecPower ),
			B1( B1 ),
			B2( B2 ),
			B3( B3 ),
			B4( B4 ),
			B5( B5 ),
			B6( B6 ),
			B7( B7 ),
			B8( B8 ),
			T_MinRegenAirInTemp( T_MinRegenAirInTemp ),
			T_MaxRegenAirInTemp( T_MaxRegenAirInTemp ),
			T_MinRegenAirInHumRat( T_MinRegenAirInHumRat ),
			T_MaxRegenAirInHumRat( T_MaxRegenAirInHumRat ),
			T_MinProcAirInTemp( T_MinProcAirInTemp ),
			T_MaxProcAirInTemp( T_MaxProcAirInTemp ),
			T_MinProcAirInHumRat( T_MinProcAirInHumRat ),
			T_MaxProcAirInHumRat( T_MaxProcAirInHumRat ),
			T_MinFaceVel( T_MinFaceVel ),
			T_MaxFaceVel( T_MaxFaceVel ),
			MinRegenAirOutTemp( MinRegenAirOutTemp ),
			MaxRegenAirOutTemp( MaxRegenAirOutTemp ),
			T_MinRegenAirInRelHum( T_MinRegenAirInRelHum ),
			T_MaxRegenAirInRelHum( T_MaxRegenAirInRelHum ),
			T_MinProcAirInRelHum( T_MinProcAirInRelHum ),
			T_MaxProcAirInRelHum( T_MaxProcAirInRelHum ),
			C1( C1 ),
			C2( C2 ),
			C3( C3 ),
			C4( C4 ),
			C5( C5 ),
			C6( C6 ),
			C7( C7 ),
			C8( C8 ),
			H_MinRegenAirInTemp( H_MinRegenAirInTemp ),
			H_MaxRegenAirInTemp( H_MaxRegenAirInTemp ),
			H_MinRegenAirInHumRat( H_MinRegenAirInHumRat ),
			H_MaxRegenAirInHumRat( H_MaxRegenAirInHumRat ),
			H_MinProcAirInTemp( H_MinProcAirInTemp ),
			H_MaxProcAirInTemp( H_MaxProcAirInTemp ),
			H_MinProcAirInHumRat( H_MinProcAirInHumRat ),
			H_MaxProcAirInHumRat( H_MaxProcAirInHumRat ),
			H_MinFaceVel( H_MinFaceVel ),
			H_MaxFaceVel( H_MaxFaceVel ),
			MinRegenAirOutHumRat( MinRegenAirOutHumRat ),
			MaxRegenAirOutHumRat( MaxRegenAirOutHumRat ),
			H_MinRegenAirInRelHum( H_MinRegenAirInRelHum ),
			H_MaxRegenAirInRelHum( H_MaxRegenAirInRelHum ),
			H_MinProcAirInRelHum( H_MinProcAirInRelHum ),
			H_MaxProcAirInRelHum( H_MaxProcAirInRelHum ),
			PrintRegenInRelHumTempMess( PrintRegenInRelHumTempMess ),
			RegenInRelHumTempErrIndex( RegenInRelHumTempErrIndex ),
			RegenInRelHumTempErrorCount( RegenInRelHumTempErrorCount ),
			RegenInRelHumTempBuffer1( RegenInRelHumTempBuffer1 ),
			RegenInRelHumTempBuffer2( RegenInRelHumTempBuffer2 ),
			RegenInRelHumTempBuffer3( RegenInRelHumTempBuffer3 ),
			RegenInRelHumTempLast( RegenInRelHumTempLast ),
			PrintProcInRelHumTempMess( PrintProcInRelHumTempMess ),
			ProcInRelHumTempErrIndex( ProcInRelHumTempErrIndex ),
			ProcInRelHumTempErrorCount( ProcInRelHumTempErrorCount ),
			ProcInRelHumTempBuffer1( ProcInRelHumTempBuffer1 ),
			ProcInRelHumTempBuffer2( ProcInRelHumTempBuffer2 ),
			ProcInRelHumTempBuffer3( ProcInRelHumTempBuffer3 ),
			ProcInRelHumTempLast( ProcInRelHumTempLast ),
			PrintRegenInRelHumHumRatMess( PrintRegenInRelHumHumRatMess ),
			RegenInRelHumHumRatErrIndex( RegenInRelHumHumRatErrIndex ),
			RegenInRelHumHumRatErrorCount( RegenInRelHumHumRatErrorCount ),
			RegenInRelHumHumRatBuffer1( RegenInRelHumHumRatBuffer1 ),
			RegenInRelHumHumRatBuffer2( RegenInRelHumHumRatBuffer2 ),
			RegenInRelHumHumRatBuffer3( RegenInRelHumHumRatBuffer3 ),
			RegenInRelHumHumRatLast( RegenInRelHumHumRatLast ),
			PrintProcInRelHumHumRatMess( PrintProcInRelHumHumRatMess ),
			ProcInRelHumHumRatErrIndex( ProcInRelHumHumRatErrIndex ),
			ProcInRelHumHumRatErrorCount( ProcInRelHumHumRatErrorCount ),
			ProcInRelHumHumRatBuffer1( ProcInRelHumHumRatBuffer1 ),
			ProcInRelHumHumRatBuffer2( ProcInRelHumHumRatBuffer2 ),
			ProcInRelHumHumRatBuffer3( ProcInRelHumHumRatBuffer3 ),
			ProcInRelHumHumRatLast( ProcInRelHumHumRatLast ),
			PrintT_RegenInTempMessage( PrintT_RegenInTempMessage ),
			PrintT_RegenInHumRatMessage( PrintT_RegenInHumRatMessage ),
			PrintT_ProcInTempMessage( PrintT_ProcInTempMessage ),
			PrintT_ProcInHumRatMessage( PrintT_ProcInHumRatMessage ),
			PrintT_FaceVelMessage( PrintT_FaceVelMessage ),
			PrintRegenOutTempMessage( PrintRegenOutTempMessage ),
			PrintRegenOutTempFailedMessage( PrintRegenOutTempFailedMessage ),
			PrintH_RegenInTempMessage( PrintH_RegenInTempMessage ),
			PrintH_RegenInHumRatMessage( PrintH_RegenInHumRatMessage ),
			PrintH_ProcInTempMessage( PrintH_ProcInTempMessage ),
			PrintH_ProcInHumRatMessage( PrintH_ProcInHumRatMessage ),
			PrintH_FaceVelMessage( PrintH_FaceVelMessage ),
			PrintRegenOutHumRatMessage( PrintRegenOutHumRatMessage ),
			PrintRegenInHumRatMessage( PrintRegenInHumRatMessage ),
			PrintRegenOutHumRatFailedMess( PrintRegenOutHumRatFailedMess ),
			RegenOutHumRatFailedErrIndex( RegenOutHumRatFailedErrIndex ),
			RegenOutHumRatFailedErrorCount( RegenOutHumRatFailedErrorCount ),
			RegenOutHumRatFailedBuffer1( RegenOutHumRatFailedBuffer1 ),
			RegenOutHumRatFailedBuffer2( RegenOutHumRatFailedBuffer2 ),
			RegenOutHumRatFailedBuffer3( RegenOutHumRatFailedBuffer3 ),
			RegenOutHumRatFailedLast( RegenOutHumRatFailedLast ),
			PrintImbalancedMassFlowMess( PrintImbalancedMassFlowMess ),
			ImbalancedFlowErrIndex( ImbalancedFlowErrIndex ),
			ImbalancedMassFlowErrorCount( ImbalancedMassFlowErrorCount ),
			ImbalancedMassFlowBuffer1( ImbalancedMassFlowBuffer1 ),
			ImbalancedMassFlowBuffer2( ImbalancedMassFlowBuffer2 ),
			ImbalancedMassFlowBuffer3( ImbalancedMassFlowBuffer3 ),
			ABSImbalancedFlow( ABSImbalancedFlow ),
			T_RegenInTempErrorCount( T_RegenInTempErrorCount ),
			T_RegenInHumRatErrorCount( T_RegenInHumRatErrorCount ),
			T_ProcInTempErrorCount( T_ProcInTempErrorCount ),
			T_ProcInHumRatErrorCount( T_ProcInHumRatErrorCount ),
			T_FaceVelErrorCount( T_FaceVelErrorCount ),
			T_RegenInTempErrIndex( T_RegenInTempErrIndex ),
			T_RegenInHumRatErrIndex( T_RegenInHumRatErrIndex ),
			T_ProcInTempErrIndex( T_ProcInTempErrIndex ),
			T_ProcInHumRatErrIndex( T_ProcInHumRatErrIndex ),
			T_FaceVelocityErrIndex( T_FaceVelocityErrIndex ),
			RegenOutTempErrorCount( RegenOutTempErrorCount ),
			RegenOutTempErrIndex( RegenOutTempErrIndex ),
			RegenOutTempFailedErrorCount( RegenOutTempFailedErrorCount ),
			RegenOutTempFailedErrIndex( RegenOutTempFailedErrIndex ),
			RegenOutTempFailedBuffer1( RegenOutTempFailedBuffer1 ),
			RegenOutTempFailedBuffer2( RegenOutTempFailedBuffer2 ),
			RegenOutTempFailedBuffer3( RegenOutTempFailedBuffer3 ),
			RegenOutTempFailedLast( RegenOutTempFailedLast ),
			H_RegenInTempErrorCount( H_RegenInTempErrorCount ),
			H_RegenInHumRatErrorCount( H_RegenInHumRatErrorCount ),
			H_ProcInTempErrorCount( H_ProcInTempErrorCount ),
			H_ProcInHumRatErrorCount( H_ProcInHumRatErrorCount ),
			H_FaceVelErrorCount( H_FaceVelErrorCount ),
			H_RegenInTempErrIndex( H_RegenInTempErrIndex ),
			H_RegenInHumRatErrIndex( H_RegenInHumRatErrIndex ),
			H_ProcInTempErrIndex( H_ProcInTempErrIndex ),
			H_ProcInHumRatErrIndex( H_ProcInHumRatErrIndex ),
			H_FaceVelocityErrIndex( H_FaceVelocityErrIndex ),
			RegenOutHumRatErrorCount( RegenOutHumRatErrorCount ),
			RegenOutHumRatErrIndex( RegenOutHumRatErrIndex ),
			RegenInHumRatErrorCount( RegenInHumRatErrorCount ),
			RegenInHumRatErrIndex( RegenInHumRatErrIndex ),
			T_RegenInTempBuffer1( T_RegenInTempBuffer1 ),
			T_RegenInTempBuffer2( T_RegenInTempBuffer2 ),
			T_RegenInTempBuffer3( T_RegenInTempBuffer3 ),
			T_RegenInTempLast( T_RegenInTempLast ),
			T_RegenInHumRatBuffer1( T_RegenInHumRatBuffer1 ),
			T_RegenInHumRatBuffer2( T_RegenInHumRatBuffer2 ),
			T_RegenInHumRatBuffer3( T_RegenInHumRatBuffer3 ),
			T_RegenInHumRatLast( T_RegenInHumRatLast ),
			T_ProcInTempBuffer1( T_ProcInTempBuffer1 ),
			T_ProcInTempBuffer2( T_ProcInTempBuffer2 ),
			T_ProcInTempBuffer3( T_ProcInTempBuffer3 ),
			T_ProcInTempLast( T_ProcInTempLast ),
			T_ProcInHumRatBuffer1( T_ProcInHumRatBuffer1 ),
			T_ProcInHumRatBuffer2( T_ProcInHumRatBuffer2 ),
			T_ProcInHumRatBuffer3( T_ProcInHumRatBuffer3 ),
			T_ProcInHumRatLast( T_ProcInHumRatLast ),
			T_FaceVelBuffer1( T_FaceVelBuffer1 ),
			T_FaceVelBuffer2( T_FaceVelBuffer2 ),
			T_FaceVelBuffer3( T_FaceVelBuffer3 ),
			T_FaceVelLast( T_FaceVelLast ),
			RegenOutTempBuffer1( RegenOutTempBuffer1 ),
			RegenOutTempBuffer2( RegenOutTempBuffer2 ),
			RegenOutTempBuffer3( RegenOutTempBuffer3 ),
			RegenOutTempLast( RegenOutTempLast ),
			H_RegenInTempBuffer1( H_RegenInTempBuffer1 ),
			H_RegenInTempBuffer2( H_RegenInTempBuffer2 ),
			H_RegenInTempBuffer3( H_RegenInTempBuffer3 ),
			H_RegenInTempLast( H_RegenInTempLast ),
			H_RegenInHumRatBuffer1( H_RegenInHumRatBuffer1 ),
			H_RegenInHumRatBuffer2( H_RegenInHumRatBuffer2 ),
			H_RegenInHumRatBuffer3( H_RegenInHumRatBuffer3 ),
			H_RegenInHumRatLast( H_RegenInHumRatLast ),
			H_ProcInTempBuffer1( H_ProcInTempBuffer1 ),
			H_ProcInTempBuffer2( H_ProcInTempBuffer2 ),
			H_ProcInTempBuffer3( H_ProcInTempBuffer3 ),
			H_ProcInTempLast( H_ProcInTempLast ),
			H_ProcInHumRatBuffer1( H_ProcInHumRatBuffer1 ),
			H_ProcInHumRatBuffer2( H_ProcInHumRatBuffer2 ),
			H_ProcInHumRatBuffer3( H_ProcInHumRatBuffer3 ),
			H_ProcInHumRatLast( H_ProcInHumRatLast ),
			H_FaceVelBuffer1( H_FaceVelBuffer1 ),
			H_FaceVelBuffer2( H_FaceVelBuffer2 ),
			H_FaceVelBuffer3( H_FaceVelBuffer3 ),
			H_FaceVelLast( H_FaceVelLast ),
			RegenOutHumRatBuffer1( RegenOutHumRatBuffer1 ),
			RegenOutHumRatBuffer2( RegenOutHumRatBuffer2 ),
			RegenOutHumRatBuffer3( RegenOutHumRatBuffer3 ),
			RegenOutHumRatLast( RegenOutHumRatLast )
		{}

	};

	// Object Data
	extern Array1D< HeatExchCond > ExchCond;
	extern Array1D< BalancedDesDehumPerfData > BalDesDehumPerfData;

	// Functions

	void
	SimHeatRecovery(
		std::string const & CompName, // name of the heat exchanger unit
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		int & CompIndex, // Pointer to Component
		int const FanOpMode, // Supply air fan operating mode
		Optional< Real64 const > HXPartLoadRatio = _, // Part load ratio requested of DX compressor
		Optional_bool_const HXUnitEnable = _, // Flag to operate heat exchanger
		Optional_int_const CompanionCoilIndex = _, // index of companion cooling coil
		Optional_bool_const RegenInletIsOANode = _, // flag to determine if supply inlet is OA node, if so air flow cycles
		Optional_bool_const EconomizerFlag = _, // economizer operation flag passed by airloop or OA sys
		Optional_bool_const HighHumCtrlFlag = _ // high humidity control flag passed by airloop or OA sys
	);

	void
	GetHeatRecoveryInput();

	void
	InitHeatRecovery(
		int const ExchNum, // number of the current heat exchanger being simulated
		int const CompanionCoilIndex
	);

	void
	SizeHeatRecovery( int const ExchNum );

	void
	CalcAirToAirPlateHeatExch(
		int const ExNum, // number of the current heat exchanger being simulated
		bool const HXUnitOn, // flag to simulate heat exchager heat recovery
		Optional_bool_const EconomizerFlag = _, // economizer flag pass by air loop or OA sys
		Optional_bool_const HighHumCtrlFlag = _ // high humidity control flag passed by airloop or OA sys
	);

	void
	CalcAirToAirGenericHeatExch(
		int const ExNum, // number of the current heat exchanger being simulated
		bool const HXUnitOn, // flag to simulate heat exchanger heat recovery
		bool const FirstHVACIteration, // first HVAC iteration flag
		Optional_bool_const EconomizerFlag = _, // economizer flag pass by air loop or OA sys
		Optional_bool_const HighHumCtrlFlag = _ // high humidity control flag passed by airloop or OA sys
	);

	void
	CalcDesiccantBalancedHeatExch(
		int const ExNum, // number of the current heat exchanger being simulated
		bool const HXUnitOn, // flag to simulate heat exchager heat recovery
		bool const FirstHVACIteration, // First HVAC iteration flag
		int const FanOpMode, // Supply air fan operating mode (1=cycling, 2=constant)
		Real64 const PartLoadRatio, // Part load ratio requested of DX compressor
		int const CompanionCoilIndex, // index of companion cooling coil
		bool const RegenInletIsOANode, // Flag to determine if regen side inlet is OANode, if so this air stream cycles
		Optional_bool_const EconomizerFlag = _, // economizer flag pass by air loop or OA sys
		Optional_bool_const HighHumCtrlFlag = _ // high humidity control flag passed by airloop or OA sys
	);

	void
	FrostControl( int const ExNum ); // number of the current heat exchanger being simulated

	void
	UpdateHeatRecovery( int const ExNum ); // number of the current heat exchanger being simulated

	void
	ReportHeatRecovery( int const ExNum ); // number of the current heat exchanger being simulated

	Real64
	SafeDiv(
		Real64 const a,
		Real64 const b
	);

	void
	CalculateEpsFromNTUandZ(
		Real64 const NTU, // number of transfer units
		Real64 const Z, // capacity rate ratio
		int const FlowArr, // flow arrangement
		Real64 & Eps // heat exchanger effectiveness
	);

	void
	CalculateNTUfromEpsAndZ(
		Real64 & NTU, // number of transfer units
		int & Err, // error indicator
		Real64 const Z, // capacity rate ratio
		int const FlowArr, // flow arrangement
		Real64 const Eps // heat exchanger effectiveness
	);

	Real64
	GetNTUforCrossFlowBothUnmixed(
		Real64 const Eps, // heat exchanger effectiveness
		Real64 const Z // capacity rate ratio
	);

	Real64
	GetResidCrossFlowBothUnmixed(
		Real64 const NTU, // number of transfer units
		Array1< Real64 > const & Par // par(1) = Eps, par(2) = Z
	);

	void
	CheckModelBoundsTempEq(
		int const ExchNum, // number of the current heat exchanger being simulated
		Real64 & T_RegenInTemp, // current regen inlet temperature (C) for regen outlet temp eqn
		Real64 & T_RegenInHumRat, // current regen inlet hum rat for regen outlet temp eqn
		Real64 & T_ProcInTemp, // current process inlet temperature (C) for regen outlet temp eqn
		Real64 & T_ProcInHumRat, // current process inlet hum rat for regen outlet temp eqn
		Real64 & T_FaceVel, // current process and regen face velocity (m/s)
		bool const FirstHVACIteration // First HVAC iteration flag
	);

	void
	CheckModelBoundsHumRatEq(
		int const ExchNum, // number of the current heat exchanger being simulated
		Real64 & H_RegenInTemp, // current regen inlet temperature (C) for regen outlet hum rat eqn
		Real64 & H_RegenInHumRat, // current regen inlet hum rat for regen outlet hum rat eqn
		Real64 & H_ProcInTemp, // current process inlet temperature (C) for regen outlet hum rat eqn
		Real64 & H_ProcInHumRat, // current process inlet hum rat for regen outlet hum rat eqn
		Real64 & H_FaceVel, // current process and regen face velocity (m/s)
		bool const FirstHVACIteration // First HVAC iteration flag
	);

	void
	CheckModelBoundOutput_Temp(
		int const ExchNum, // number of the current heat exchanger being simulated
		Real64 const RegenInTemp, // current regen inlet temp passed to eqn
		Real64 & RegenOutTemp, // current regen outlet temp from eqn
		bool const FirstHVACIteration // First HVAC iteration flag
	);

	void
	CheckModelBoundOutput_HumRat(
		int const ExchNum, // number of the current heat exchanger being simulated
		Real64 const RegenInHumRat, // current regen inlet hum rat passed to eqn
		Real64 & RegenOutHumRat, // current regen outlet hum rat from eqn
		bool const FirstHVACIteration // First HVAC iteration flag
	);

	void
	CheckModelBoundsRH_TempEq(
		int const ExchNum, // number of the current heat exchanger being simulated
		Real64 const T_RegenInTemp, // current regen inlet temperature passed to eqn
		Real64 const T_RegenInHumRat, // current regen inlet hum rat passed to eqn
		Real64 const T_ProcInTemp, // current process inlet temperature passed to eqn
		Real64 const T_ProcInHumRat, // current regen outlet hum rat from eqn
		bool const FirstHVACIteration // first HVAC iteration flag
	);

	void
	CheckModelBoundsRH_HumRatEq(
		int const ExchNum, // number of the current heat exchanger being simulated
		Real64 const H_RegenInTemp, // current regen inlet temperature passed to eqn
		Real64 const H_RegenInHumRat, // current regen inlet hum rat passed to eqn
		Real64 const H_ProcInTemp, // current process inlet temperature passed to eqn
		Real64 const H_ProcInHumRat, // current process inlet hum rat passed to eqn
		bool const FirstHVACIteration // first HVAC iteration flag
	);

	void
	CheckForBalancedFlow(
		int const ExchNum, // number of the current heat exchanger being simulated
		Real64 const ProcessInMassFlow, // current process inlet air mass flow rate (m3/s)
		Real64 const RegenInMassFlow, // current regeneration inlet air mass flow rate (m3/s)
		bool const FirstHVACIteration // first HVAC iteration flag
	);

	int
	GetSupplyInletNode(
		std::string const & HXName, // must match HX names for the ExchCond type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetSupplyOutletNode(
		std::string const & HXName, // must match HX names for the ExchCond type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetSecondaryInletNode(
		std::string const & HXName, // must match HX names for the ExchCond type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetSecondaryOutletNode(
		std::string const & HXName, // must match HX names for the ExchCond type
		bool & ErrorsFound // set to true if problem
	);

	Real64
	GetSupplyAirFlowRate(
		std::string const & HXName, // must match HX names for the ExchCond type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetHeatExchangerObjectTypeNum(
		std::string const & HXName, // must match HX names for the ExchCond type
		bool & ErrorsFound // set to true if problem
	);

	void
	SetHeatExchangerData(
		int const HXNum, // Index of HX
		bool & ErrorsFound, // Set to true if certain errors found
		std::string const & HXName, // Name of HX
		Optional< Real64 > SupplyAirVolFlow = _, // HX supply air flow rate    [m3/s]
		Optional< Real64 > SecondaryAirVolFlow = _ // HX secondary air flow rate [m3/s]
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

} // HeatRecovery

} // EnergyPlus

#endif
