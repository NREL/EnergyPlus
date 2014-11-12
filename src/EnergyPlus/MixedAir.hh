#ifndef MixedAir_hh_INCLUDED
#define MixedAir_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/FArray1S.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <DataHVACGlobals.hh>

namespace EnergyPlus {

namespace MixedAir {

	// Using/Aliasing
	using DataHVACGlobals::BypassWhenWithinEconomizerLimits;

	// Data
	// MODULE PARAMETER DEFINITIONS
	extern int const NoLockoutPossible;
	extern int const LockoutWithHeatingPossible;
	extern int const LockoutWithCompressorPossible;

	extern int const NoEconomizer;
	// Changed by Amit as a part on New Feature Proposal
	extern int const FixedDryBulb;
	extern int const FixedEnthalpy;
	extern int const DifferentialDryBulb;
	extern int const DifferentialEnthalpy;
	extern int const FixedDewPointAndDryBulb;
	extern int const ElectronicEnthalpy;
	extern int const DifferentialDryBulbAndEnthalpy;
	// coil operation
	extern int const On; // normal coil operation
	extern int const Off; // signal coil shouldn't run
	// component types addressed by this module
	extern int const OAMixer_Num;
	extern int const Fan_Simple_CV;
	extern int const Fan_Simple_VAV;
	extern int const WaterCoil_SimpleCool;
	extern int const WaterCoil_Cooling;
	extern int const WaterCoil_SimpleHeat;
	extern int const SteamCoil_AirHeat;
	extern int const WaterCoil_DetailedCool;
	extern int const Coil_ElectricHeat;
	extern int const Coil_GasHeat;
	extern int const WaterCoil_CoolingHXAsst;
	extern int const DXSystem;
	extern int const HeatXchngr;
	extern int const Desiccant;
	extern int const Unglazed_SolarCollector;
	extern int const EvapCooler;
	extern int const PVT_AirBased;
	extern int const Fan_ComponentModel; // cpw22Aug2010 (new)
	extern int const DXHeatPumpSystem;
	extern int const Coil_UserDefined;
	extern int const UnitarySystem;

	extern int const ControllerSimple;
	extern int const ControllerOutsideAir;
	extern int const ControllerStandAloneERV;

	//Zone Outdoor Air Method
	//INTEGER, PARAMETER :: ZOAM_FlowPerPerson = 1  ! set the outdoor air flow rate based on number of people in the zone
	//INTEGER, PARAMETER :: ZOAM_FlowPerZone = 2    ! sum the outdoor air flow rate per zone based on user input
	//INTEGER, PARAMETER :: ZOAM_FlowPerArea = 3    ! sum the outdoor air flow rate based on zone area
	//INTEGER, PARAMETER :: ZOAM_FlowPerACH = 4     ! sum the outdoor air flow rate based on number of air changes for the zone
	//INTEGER, PARAMETER :: ZOAM_Sum = 5            ! sum the outdoor air flow rate of the people component and the space floor area component
	//INTEGER, PARAMETER :: ZOAM_Max = 6            ! use the maximum of the outdoor air flow rate of the people component and
	//                                              ! the space floor area component
	//System Outdoor Air Method
	//INTEGER, PARAMETER :: SOAM_ZoneSum = 1  ! Sum the outdoor air flow rates of all zones
	//INTEGER, PARAMETER :: SOAM_VRP = 2      ! Use ASHRAE Standard 62.1-2007 to calculate the system level outdoor air flow rates
	//                                        !  considering the zone air distribution effectiveness and the system ventilation efficiency
	//INTEGER, PARAMETER :: SOAM_IAQP = 3     ! Use ASHRAE Standard 62.1-2007 IAQP to calculate the system level outdoor air flow rates
	//                                        ! based on the CO2 setpoint
	//INTEGER, PARAMETER :: SOAM_ProportionalControl = 4     ! Use ASHRAE Standard 62.1-2004 or Trane Engineer's newsletter (volume 34-5)
	//                                                       ! to calculate the system level outdoor air flow rates
	//INTEGER, PARAMETER :: SOAM_IAQPGC = 5   ! Use ASHRAE Standard 62.1-2004 IAQP to calculate the system level outdoor air flow rates
	//                                        ! based on the generic contaminant setpoint
	//INTEGER, PARAMETER :: SOAM_IAQPCOM = 6  ! Take the maximum outdoor air rate from both CO2 and generic contaminant controls
	//                                        ! based on the generic contaminant setpoint

	extern FArray1D_string const CurrentModuleObjects;

	// Parameters below (CMO - Current Module Object.  used primarily in Get Inputs)
	// Multiple Get Input routines in this module or these would be in individual routines.
	extern int const CMO_OASystem;
	extern int const CMO_AirLoopEqList;
	extern int const CMO_ControllerList;
	extern int const CMO_SysAvailMgrList;
	extern int const CMO_OAController;
	extern int const CMO_ERVController;
	extern int const CMO_MechVentilation;
	extern int const CMO_OAMixer;

	//Type declarations in MixedAir module

	//MODULE VARIABLE DECLARATIONS:
	extern int NumControllerLists; // Number of Controller Lists
	extern int NumOAControllers; // Number of OA Controllers (includes ERV controllers)
	extern int NumERVControllers; // Number of ERV Controllers
	extern int NumOAMixers; // Number of Outdoor Air Mixers
	extern int NumVentMechControllers; // Number of Controller:MechanicalVentilation objects in input deck

	extern FArray1D_bool MyOneTimeErrorFlag;
	extern FArray1D_bool MyOneTimeCheckUnitarySysFlag;
	extern bool GetOASysInputFlag; // Flag set to make sure you get input once
	extern bool GetOAMixerInputFlag; // Flag set to make sure you get input once
	extern bool GetOAControllerInputFlag; // Flag set to make sure you get input once

	//SUBROUTINE SPECIFICATIONS FOR MODULE MixedAir
	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Algorithms/Calculation routines for the module

	// Sizing routine for the module

	// Update routines to check convergence and update nodes

	// Utility routines for the module

	// Types

	struct ControllerListProps
	{
		// Members
		std::string Name;
		int NumControllers; // number of controllers on list
		FArray1D_string ControllerType;
		FArray1D_string ControllerName;

		// Default Constructor
		ControllerListProps() :
			NumControllers( 0 )
		{}

		// Member Constructor
		ControllerListProps(
			std::string const & Name,
			int const NumControllers, // number of controllers on list
			FArray1_string const & ControllerType,
			FArray1_string const & ControllerName
		) :
			Name( Name ),
			NumControllers( NumControllers ),
			ControllerType( ControllerType ),
			ControllerName( ControllerName )
		{}

	};

	struct OAControllerProps // Derived type for Outside Air Controller data
	{
		// Members
		std::string Name;
		std::string ControllerType;
		int ControllerType_Num; // Parameter equivalent of controller type
		int OACtrlIndex;
		int Lockout; // 0=NoLockoutPossible; 1=LockoutWithHeatingPossible;
		// 2=LockoutWithCompressorPossible;
		bool FixedMin; // Fixed Minimum or Proportional Minimum
		Real64 TempLim; // Temperature Limit
		Real64 TempLowLim; // Temperature Lower Limit
		Real64 EnthLim; // Enthalpy Limit
		Real64 DPTempLim; // Dew Point Temperature Limit
		int EnthalpyCurvePtr; // Electronic Enthalpy Curve Index (max HumRat = f[OAT])
		Real64 MinOA; // Minimum outside air flow (m3/sec)
		Real64 MaxOA; // Maximum outside air flow (m3/sec)
		int Econo; // 0 = NoEconomizer, 1 = FixedDryBulb, 2 = FixedEnthalpy, 3=DifferentialDryBulb,
		// 4=DifferentialEnthalpy, 5=FixedDewPointAndDryBulb, 6 = ElectronicEnthalpy,
		// 7 =DifferentialDryBulbAndEnthalpy
		bool EconBypass; // ModulateFlow =FALSE , MinimumFlowWithBypass =TRUE
		int MixNode; // Controlled node (mixed air node)
		int OANode; // Actuated node (outside air node)
		int InletNode; // Inlet Air Node for into Mixer  (BTG Nov 2004)
		int RelNode; // Relief Air Node Number
		int RetNode; // Return Air Node Number
		std::string MinOASch; // Name of the minimum outside air schedule
		int MinOASchPtr; // Index to the minimum outside air schedule
		Real64 RelMassFlow;
		Real64 OAMassFlow;
		Real64 ExhMassFlow;
		Real64 MixMassFlow;
		Real64 InletTemp;
		Real64 InletEnth;
		Real64 InletPress;
		Real64 InletHumRat;
		Real64 OATemp;
		Real64 OAEnth;
		Real64 OAPress;
		Real64 OAHumRat;
		Real64 RetTemp;
		Real64 RetEnth;
		Real64 MixSetTemp;
		Real64 MinOAMassFlowRate; // Minimum outside air flow (kg/s)
		Real64 MaxOAMassFlowRate; // Maximum outside air flow (kg/s)
		int ZoneEquipZoneNum;
		std::string VentilationMechanicalName; // Name of ventilation:mechanical object used for DCV
		int VentMechObjectNum; // Index to VENTILATION:MECHANICAL object for this controller
		int HumidistatZoneNum; // zone number where humidistat is located
		int NodeNumofHumidistatZone; // node number of zone where humidistat is located
		Real64 HighRHOAFlowRatio; // Modify ratio with respect to maximum outdoor air flow rate (high RH)
		bool ModifyDuringHighOAMoisture; // flag to Modify outdoor air flow, TRUE when modify any time
		// FALSE when modify only when indoor air humrat is less than outdoor HR
		int EconomizerOASchedPtr; // schedule to modify outdoor air flow
		std::string MinOAflowSch; // Name of the Minimum fraction of Design/Mixed Mass of air
		std::string MaxOAflowSch; // Name of the Maximum fraction of Design/Mixed Mass of air
		int MinOAflowSchPtr; // Index to the minimum outside air schedule
		int MaxOAflowSchPtr; // Index to the minimum outside air schedule
		//   Economizer Status, which is currently following the EconomizerOperationFlag, might be something like "Economizer status
		//   indicates when the conditions are favorable for the economizer to operate (i.e., none of the control limits have been exceeded).
		//   While this status signal indicates favorable conditions for economizer operation, it does not guarantee that the air-side
		//   economizer has increased outdoor air flow above the minimum level since the actual outdoor air flow rate is also governed
		//   by other controls (e.g., mixed air setpoint tempeature, time of day economizer control, etc.).
		int EconomizerStatus; // Air Economizer status (1 = on, 0 = off or economizer not exists)
		int HeatRecoveryBypassStatus; // OA Sys Heat Recovery Bypass status (1 = on, 0 = off or economizer not exists)
		int HRHeatingCoilActive; // OA Sys Heat Recovery Heating Coil Was Active status (1 = on, 0 = off)
		Real64 MixedAirTempAtMinOAFlow; // calculated mixed air temp when using special HX bypass control
		int HighHumCtrlStatus; // High Humidity Control status (1 = on, 0 = off or high hum ctrl not used)
		Real64 OAFractionRpt; // Actual outdoor air fraction for reporting (based on mixed air flow rate),
		// 0 to 1 (normally)
		Real64 MinOAFracLimit; // Minimum OA fraction limit
		bool EMSOverrideOARate; // if true, EMS is calling to override OA rate
		Real64 EMSOARateValue; // Value EMS is directing to use. [kg/s]
		int HeatRecoveryBypassControlType; // User input selects type of heat recovery optimization

		// Default Constructor
		OAControllerProps() :
			ControllerType_Num( 0 ),
			OACtrlIndex( 0 ),
			Lockout( 0 ),
			FixedMin( true ),
			TempLim( 0.0 ),
			TempLowLim( 0.0 ),
			EnthLim( 0.0 ),
			DPTempLim( 0.0 ),
			EnthalpyCurvePtr( 0 ),
			MinOA( 0.0 ),
			MaxOA( 0.0 ),
			Econo( 0 ),
			EconBypass( false ),
			MixNode( 0 ),
			OANode( 0 ),
			InletNode( 0 ),
			RelNode( 0 ),
			RetNode( 0 ),
			MinOASchPtr( 0 ),
			RelMassFlow( 0.0 ),
			OAMassFlow( 0.0 ),
			ExhMassFlow( 0.0 ),
			MixMassFlow( 0.0 ),
			InletTemp( 0.0 ),
			InletEnth( 0.0 ),
			InletPress( 0.0 ),
			InletHumRat( 0.0 ),
			OATemp( 0.0 ),
			OAEnth( 0.0 ),
			OAPress( 0.0 ),
			OAHumRat( 0.0 ),
			RetTemp( 0.0 ),
			RetEnth( 0.0 ),
			MixSetTemp( 0.0 ),
			MinOAMassFlowRate( 0.0 ),
			MaxOAMassFlowRate( 0.0 ),
			ZoneEquipZoneNum( 0 ),
			VentMechObjectNum( 0 ),
			HumidistatZoneNum( 0 ),
			NodeNumofHumidistatZone( 0 ),
			HighRHOAFlowRatio( 1.0 ),
			ModifyDuringHighOAMoisture( false ),
			EconomizerOASchedPtr( 0 ),
			MinOAflowSchPtr( 0 ),
			MaxOAflowSchPtr( 0 ),
			EconomizerStatus( 0 ),
			HeatRecoveryBypassStatus( 0 ),
			HRHeatingCoilActive( 0 ),
			MixedAirTempAtMinOAFlow( 0.0 ),
			HighHumCtrlStatus( 0 ),
			OAFractionRpt( 0.0 ),
			MinOAFracLimit( 0.0 ),
			EMSOverrideOARate( false ),
			EMSOARateValue( 0.0 ),
			HeatRecoveryBypassControlType( BypassWhenWithinEconomizerLimits )
		{}

		// Member Constructor
		OAControllerProps(
			std::string const & Name,
			std::string const & ControllerType,
			int const ControllerType_Num, // Parameter equivalent of controller type
			int const OACtrlIndex,
			int const Lockout, // 0=NoLockoutPossible; 1=LockoutWithHeatingPossible;
			bool const FixedMin, // Fixed Minimum or Proportional Minimum
			Real64 const TempLim, // Temperature Limit
			Real64 const TempLowLim, // Temperature Lower Limit
			Real64 const EnthLim, // Enthalpy Limit
			Real64 const DPTempLim, // Dew Point Temperature Limit
			int const EnthalpyCurvePtr, // Electronic Enthalpy Curve Index (max HumRat = f[OAT])
			Real64 const MinOA, // Minimum outside air flow (m3/sec)
			Real64 const MaxOA, // Maximum outside air flow (m3/sec)
			int const Econo, // 0 = NoEconomizer, 1 = FixedDryBulb, 2 = FixedEnthalpy, 3=DifferentialDryBulb,
			bool const EconBypass, // ModulateFlow =FALSE , MinimumFlowWithBypass =TRUE
			int const MixNode, // Controlled node (mixed air node)
			int const OANode, // Actuated node (outside air node)
			int const InletNode, // Inlet Air Node for into Mixer  (BTG Nov 2004)
			int const RelNode, // Relief Air Node Number
			int const RetNode, // Return Air Node Number
			std::string const & MinOASch, // Name of the minimum outside air schedule
			int const MinOASchPtr, // Index to the minimum outside air schedule
			Real64 const RelMassFlow,
			Real64 const OAMassFlow,
			Real64 const ExhMassFlow,
			Real64 const MixMassFlow,
			Real64 const InletTemp,
			Real64 const InletEnth,
			Real64 const InletPress,
			Real64 const InletHumRat,
			Real64 const OATemp,
			Real64 const OAEnth,
			Real64 const OAPress,
			Real64 const OAHumRat,
			Real64 const RetTemp,
			Real64 const RetEnth,
			Real64 const MixSetTemp,
			Real64 const MinOAMassFlowRate, // Minimum outside air flow (kg/s)
			Real64 const MaxOAMassFlowRate, // Maximum outside air flow (kg/s)
			int const ZoneEquipZoneNum,
			std::string const & VentilationMechanicalName, // Name of ventilation:mechanical object used for DCV
			int const VentMechObjectNum, // Index to VENTILATION:MECHANICAL object for this controller
			int const HumidistatZoneNum, // zone number where humidistat is located
			int const NodeNumofHumidistatZone, // node number of zone where humidistat is located
			Real64 const HighRHOAFlowRatio, // Modify ratio with respect to maximum outdoor air flow rate (high RH)
			bool const ModifyDuringHighOAMoisture, // flag to Modify outdoor air flow, TRUE when modify any time
			int const EconomizerOASchedPtr, // schedule to modify outdoor air flow
			std::string const & MinOAflowSch, // Name of the Minimum fraction of Design/Mixed Mass of air
			std::string const & MaxOAflowSch, // Name of the Maximum fraction of Design/Mixed Mass of air
			int const MinOAflowSchPtr, // Index to the minimum outside air schedule
			int const MaxOAflowSchPtr, // Index to the minimum outside air schedule
			int const EconomizerStatus, // Air Economizer status (1 = on, 0 = off or economizer not exists)
			int const HeatRecoveryBypassStatus, // OA Sys Heat Recovery Bypass status (1 = on, 0 = off or economizer not exists)
			int const HRHeatingCoilActive, // OA Sys Heat Recovery Heating Coil Was Active status (1 = on, 0 = off)
			Real64 const MixedAirTempAtMinOAFlow, // calculated mixed air temp when using special HX bypass control
			int const HighHumCtrlStatus, // High Humidity Control status (1 = on, 0 = off or high hum ctrl not used)
			Real64 const OAFractionRpt, // Actual outdoor air fraction for reporting (based on mixed air flow rate),
			Real64 const MinOAFracLimit, // Minimum OA fraction limit
			bool const EMSOverrideOARate, // if true, EMS is calling to override OA rate
			Real64 const EMSOARateValue, // Value EMS is directing to use. [kg/s]
			int const HeatRecoveryBypassControlType // User input selects type of heat recovery optimization
		) :
			Name( Name ),
			ControllerType( ControllerType ),
			ControllerType_Num( ControllerType_Num ),
			OACtrlIndex( OACtrlIndex ),
			Lockout( Lockout ),
			FixedMin( FixedMin ),
			TempLim( TempLim ),
			TempLowLim( TempLowLim ),
			EnthLim( EnthLim ),
			DPTempLim( DPTempLim ),
			EnthalpyCurvePtr( EnthalpyCurvePtr ),
			MinOA( MinOA ),
			MaxOA( MaxOA ),
			Econo( Econo ),
			EconBypass( EconBypass ),
			MixNode( MixNode ),
			OANode( OANode ),
			InletNode( InletNode ),
			RelNode( RelNode ),
			RetNode( RetNode ),
			MinOASch( MinOASch ),
			MinOASchPtr( MinOASchPtr ),
			RelMassFlow( RelMassFlow ),
			OAMassFlow( OAMassFlow ),
			ExhMassFlow( ExhMassFlow ),
			MixMassFlow( MixMassFlow ),
			InletTemp( InletTemp ),
			InletEnth( InletEnth ),
			InletPress( InletPress ),
			InletHumRat( InletHumRat ),
			OATemp( OATemp ),
			OAEnth( OAEnth ),
			OAPress( OAPress ),
			OAHumRat( OAHumRat ),
			RetTemp( RetTemp ),
			RetEnth( RetEnth ),
			MixSetTemp( MixSetTemp ),
			MinOAMassFlowRate( MinOAMassFlowRate ),
			MaxOAMassFlowRate( MaxOAMassFlowRate ),
			ZoneEquipZoneNum( ZoneEquipZoneNum ),
			VentilationMechanicalName( VentilationMechanicalName ),
			VentMechObjectNum( VentMechObjectNum ),
			HumidistatZoneNum( HumidistatZoneNum ),
			NodeNumofHumidistatZone( NodeNumofHumidistatZone ),
			HighRHOAFlowRatio( HighRHOAFlowRatio ),
			ModifyDuringHighOAMoisture( ModifyDuringHighOAMoisture ),
			EconomizerOASchedPtr( EconomizerOASchedPtr ),
			MinOAflowSch( MinOAflowSch ),
			MaxOAflowSch( MaxOAflowSch ),
			MinOAflowSchPtr( MinOAflowSchPtr ),
			MaxOAflowSchPtr( MaxOAflowSchPtr ),
			EconomizerStatus( EconomizerStatus ),
			HeatRecoveryBypassStatus( HeatRecoveryBypassStatus ),
			HRHeatingCoilActive( HRHeatingCoilActive ),
			MixedAirTempAtMinOAFlow( MixedAirTempAtMinOAFlow ),
			HighHumCtrlStatus( HighHumCtrlStatus ),
			OAFractionRpt( OAFractionRpt ),
			MinOAFracLimit( MinOAFracLimit ),
			EMSOverrideOARate( EMSOverrideOARate ),
			EMSOARateValue( EMSOARateValue ),
			HeatRecoveryBypassControlType( HeatRecoveryBypassControlType )
		{}

	};

	struct VentilationMechanicalProps // Derived type for Ventilation:Mechanical data
	{
		// Members
		std::string Name; // Name of Ventilation:Mechanical object
		std::string SchName; // Name of the mechanical ventilation schedule
		int SchPtr; // Index to the mechanical ventilation schedule
		bool DCVFlag; // if true, implement OA based on demand controlled ventilation
		int NumofVentMechZones; // Number of zones with mechanical ventilation
		Real64 TotAreaOAFlow; // Total outdoor air flow rate for all zones per area (m3/s/m2)
		Real64 TotPeopleOAFlow; // Total outdoor air flow rate for all PEOPLE objects in zones (m3/s)
		Real64 TotZoneOAFlow; // Total outdoor air flow rate for all zones (m3/s)
		Real64 TotZoneOAACH; // Total outdoor air flow rate for all zones Air Changes per hour (m3/s/m3)
		int SystemOAMethod; // System Outdoor Air Method - SOAM_ZoneSum, SOAM_VRP
		Real64 ZoneMaxOAFraction; // Zone maximum outdoor air fraction
		FArray1D< Real64 > ZoneOAAreaRate; // Mechanical ventilation rate (m3/s/m2) for each zone
		FArray1D< Real64 > ZoneOAPeopleRate; // Mechanical ventilation rate (m3/s/person) for each zone
		FArray1D< Real64 > ZoneOAFlow; // OA Flow Rate (m3/s/zone) for each zone
		FArray1D< Real64 > ZoneOAACH; // OA ACH (m3/s/volume) for each zone
		FArray1D_int Zone; // Zones requiring mechanical ventilation
		FArray1D_int ZoneDesignSpecOAObjIndex; // index of the design specification outdoor air object
		// for each zone in zone list
		FArray1D_string ZoneDesignSpecOAObjName; // name of the design specification outdoor air object
		// for each zone in zone list
		int CO2MaxMinLimitErrorCount; // Counter when max CO2 concentration < min CO2 concentration
		// For SOAM_ProportionalControl
		int CO2MaxMinLimitErrorIndex; // Index for max CO2 concentration < min CO2 concentration recurring error message
		// For SOAM_ProportionalControl
		int CO2GainErrorCount; // Counter when CO2 generation from people is zero for SOAM_ProportionalControl
		int CO2GainErrorIndex; // Index for recurring error message when CO2 generation from people is zero
		// For SOAM_ProportionalControl
		FArray1D< Real64 > ZoneADEffCooling; // Zone air distribution effectiveness in cooling mode
		// for each zone
		FArray1D< Real64 > ZoneADEffHeating; // Zone air distribution effectiveness in heating mode
		// for each zone
		FArray1D_int ZoneADEffSchPtr; // Pointer to the zone air distribution effectiveness schedule
		// for each zone
		FArray1D_string ZoneADEffSchName; // Zone air distribution effectiveness schedule name
		// for each zone
		FArray1D_int ZoneDesignSpecADObjIndex; // index of the design specification zone air
		//  distribution object for each zone in the zone list
		FArray1D_string ZoneDesignSpecADObjName; // name of the design specification zone air
		// distribution object for each zone in the zone list
		FArray1D< Real64 > ZoneSecondaryRecirculation; // zone air secondary recirculation ratio

		// Default Constructor
		VentilationMechanicalProps() :
			SchPtr( 0 ),
			DCVFlag( false ),
			NumofVentMechZones( 0 ),
			TotAreaOAFlow( 0.0 ),
			TotPeopleOAFlow( 0.0 ),
			TotZoneOAFlow( 0.0 ),
			TotZoneOAACH( 0.0 ),
			SystemOAMethod( 0 ),
			ZoneMaxOAFraction( 1.0 ),
			CO2MaxMinLimitErrorCount( 0 ),
			CO2MaxMinLimitErrorIndex( 0 ),
			CO2GainErrorCount( 0 ),
			CO2GainErrorIndex( 0 )
		{}

		// Member Constructor
		VentilationMechanicalProps(
			std::string const & Name, // Name of Ventilation:Mechanical object
			std::string const & SchName, // Name of the mechanical ventilation schedule
			int const SchPtr, // Index to the mechanical ventilation schedule
			bool const DCVFlag, // if true, implement OA based on demand controlled ventilation
			int const NumofVentMechZones, // Number of zones with mechanical ventilation
			Real64 const TotAreaOAFlow, // Total outdoor air flow rate for all zones per area (m3/s/m2)
			Real64 const TotPeopleOAFlow, // Total outdoor air flow rate for all PEOPLE objects in zones (m3/s)
			Real64 const TotZoneOAFlow, // Total outdoor air flow rate for all zones (m3/s)
			Real64 const TotZoneOAACH, // Total outdoor air flow rate for all zones Air Changes per hour (m3/s/m3)
			int const SystemOAMethod, // System Outdoor Air Method - SOAM_ZoneSum, SOAM_VRP
			Real64 const ZoneMaxOAFraction, // Zone maximum outdoor air fraction
			FArray1< Real64 > const & ZoneOAAreaRate, // Mechanical ventilation rate (m3/s/m2) for each zone
			FArray1< Real64 > const & ZoneOAPeopleRate, // Mechanical ventilation rate (m3/s/person) for each zone
			FArray1< Real64 > const & ZoneOAFlow, // OA Flow Rate (m3/s/zone) for each zone
			FArray1< Real64 > const & ZoneOAACH, // OA ACH (m3/s/volume) for each zone
			FArray1_int const & Zone, // Zones requiring mechanical ventilation
			FArray1_int const & ZoneDesignSpecOAObjIndex, // index of the design specification outdoor air object
			FArray1_string const & ZoneDesignSpecOAObjName, // name of the design specification outdoor air object
			int const CO2MaxMinLimitErrorCount, // Counter when max CO2 concentration < min CO2 concentration
			int const CO2MaxMinLimitErrorIndex, // Index for max CO2 concentration < min CO2 concentration recurring error message
			int const CO2GainErrorCount, // Counter when CO2 generation from people is zero for SOAM_ProportionalControl
			int const CO2GainErrorIndex, // Index for recurring error message when CO2 generation from people is zero
			FArray1< Real64 > const & ZoneADEffCooling, // Zone air distribution effectiveness in cooling mode
			FArray1< Real64 > const & ZoneADEffHeating, // Zone air distribution effectiveness in heating mode
			FArray1_int const & ZoneADEffSchPtr, // Pointer to the zone air distribution effectiveness schedule
			FArray1_string const & ZoneADEffSchName, // Zone air distribution effectiveness schedule name
			FArray1_int const & ZoneDesignSpecADObjIndex, // index of the design specification zone air
			FArray1_string const & ZoneDesignSpecADObjName, // name of the design specification zone air
			FArray1< Real64 > const & ZoneSecondaryRecirculation // zone air secondary recirculation ratio
		) :
			Name( Name ),
			SchName( SchName ),
			SchPtr( SchPtr ),
			DCVFlag( DCVFlag ),
			NumofVentMechZones( NumofVentMechZones ),
			TotAreaOAFlow( TotAreaOAFlow ),
			TotPeopleOAFlow( TotPeopleOAFlow ),
			TotZoneOAFlow( TotZoneOAFlow ),
			TotZoneOAACH( TotZoneOAACH ),
			SystemOAMethod( SystemOAMethod ),
			ZoneMaxOAFraction( ZoneMaxOAFraction ),
			ZoneOAAreaRate( ZoneOAAreaRate ),
			ZoneOAPeopleRate( ZoneOAPeopleRate ),
			ZoneOAFlow( ZoneOAFlow ),
			ZoneOAACH( ZoneOAACH ),
			Zone( Zone ),
			ZoneDesignSpecOAObjIndex( ZoneDesignSpecOAObjIndex ),
			ZoneDesignSpecOAObjName( ZoneDesignSpecOAObjName ),
			CO2MaxMinLimitErrorCount( CO2MaxMinLimitErrorCount ),
			CO2MaxMinLimitErrorIndex( CO2MaxMinLimitErrorIndex ),
			CO2GainErrorCount( CO2GainErrorCount ),
			CO2GainErrorIndex( CO2GainErrorIndex ),
			ZoneADEffCooling( ZoneADEffCooling ),
			ZoneADEffHeating( ZoneADEffHeating ),
			ZoneADEffSchPtr( ZoneADEffSchPtr ),
			ZoneADEffSchName( ZoneADEffSchName ),
			ZoneDesignSpecADObjIndex( ZoneDesignSpecADObjIndex ),
			ZoneDesignSpecADObjName( ZoneDesignSpecADObjName ),
			ZoneSecondaryRecirculation( ZoneSecondaryRecirculation )
		{}

	};

	struct OAMixerProps // Derived type for Outside Air Mixing Component
	{
		// Members
		std::string Name;
		int MixerIndex; // Set on first call...
		int MixNode; // Outlet node - mixed air
		int InletNode; // Inlet node for outside air stream (Nov. 2004 BTG was OANode )
		int RelNode; // Outlet node - relief air
		int RetNode; // Inlet node - return air
		Real64 MixTemp;
		Real64 MixHumRat;
		Real64 MixEnthalpy;
		Real64 MixPressure;
		Real64 MixMassFlowRate;
		Real64 OATemp;
		Real64 OAHumRat;
		Real64 OAEnthalpy;
		Real64 OAPressure;
		Real64 OAMassFlowRate;
		Real64 RelTemp;
		Real64 RelHumRat;
		Real64 RelEnthalpy;
		Real64 RelPressure;
		Real64 RelMassFlowRate;
		Real64 RetTemp;
		Real64 RetHumRat;
		Real64 RetEnthalpy;
		Real64 RetPressure;
		Real64 RetMassFlowRate;

		// Default Constructor
		OAMixerProps() :
			MixerIndex( 0 ),
			MixNode( 0 ),
			InletNode( 0 ),
			RelNode( 0 ),
			RetNode( 0 ),
			MixTemp( 0.0 ),
			MixHumRat( 0.0 ),
			MixEnthalpy( 0.0 ),
			MixPressure( 0.0 ),
			MixMassFlowRate( 0.0 ),
			OATemp( 0.0 ),
			OAHumRat( 0.0 ),
			OAEnthalpy( 0.0 ),
			OAPressure( 0.0 ),
			OAMassFlowRate( 0.0 ),
			RelTemp( 0.0 ),
			RelHumRat( 0.0 ),
			RelEnthalpy( 0.0 ),
			RelPressure( 0.0 ),
			RelMassFlowRate( 0.0 ),
			RetTemp( 0.0 ),
			RetHumRat( 0.0 ),
			RetEnthalpy( 0.0 ),
			RetPressure( 0.0 ),
			RetMassFlowRate( 0.0 )
		{}

		// Member Constructor
		OAMixerProps(
			std::string const & Name,
			int const MixerIndex, // Set on first call...
			int const MixNode, // Outlet node - mixed air
			int const InletNode, // Inlet node for outside air stream (Nov. 2004 BTG was OANode )
			int const RelNode, // Outlet node - relief air
			int const RetNode, // Inlet node - return air
			Real64 const MixTemp,
			Real64 const MixHumRat,
			Real64 const MixEnthalpy,
			Real64 const MixPressure,
			Real64 const MixMassFlowRate,
			Real64 const OATemp,
			Real64 const OAHumRat,
			Real64 const OAEnthalpy,
			Real64 const OAPressure,
			Real64 const OAMassFlowRate,
			Real64 const RelTemp,
			Real64 const RelHumRat,
			Real64 const RelEnthalpy,
			Real64 const RelPressure,
			Real64 const RelMassFlowRate,
			Real64 const RetTemp,
			Real64 const RetHumRat,
			Real64 const RetEnthalpy,
			Real64 const RetPressure,
			Real64 const RetMassFlowRate
		) :
			Name( Name ),
			MixerIndex( MixerIndex ),
			MixNode( MixNode ),
			InletNode( InletNode ),
			RelNode( RelNode ),
			RetNode( RetNode ),
			MixTemp( MixTemp ),
			MixHumRat( MixHumRat ),
			MixEnthalpy( MixEnthalpy ),
			MixPressure( MixPressure ),
			MixMassFlowRate( MixMassFlowRate ),
			OATemp( OATemp ),
			OAHumRat( OAHumRat ),
			OAEnthalpy( OAEnthalpy ),
			OAPressure( OAPressure ),
			OAMassFlowRate( OAMassFlowRate ),
			RelTemp( RelTemp ),
			RelHumRat( RelHumRat ),
			RelEnthalpy( RelEnthalpy ),
			RelPressure( RelPressure ),
			RelMassFlowRate( RelMassFlowRate ),
			RetTemp( RetTemp ),
			RetHumRat( RetHumRat ),
			RetEnthalpy( RetEnthalpy ),
			RetPressure( RetPressure ),
			RetMassFlowRate( RetMassFlowRate )
		{}

	};

	// Object Data
	extern FArray1D< ControllerListProps > ControllerLists;
	extern FArray1D< OAControllerProps > OAController;
	extern FArray1D< OAMixerProps > OAMixer;
	extern FArray1D< VentilationMechanicalProps > VentilationMechanical;

	// Functions

	void
	ManageOutsideAirSystem(
		std::string const & OASysName,
		bool const FirstHVACIteration,
		int const AirLoopNum,
		int & OASysNum
	);

	void
	SimOutsideAirSys(
		int const OASysNum,
		bool const FirstHVACIteration,
		int const AirLoopNum
	);

	void
	SimOAComponent(
		std::string const & CompType, // the component type
		std::string const & CompName, // the component Name
		int const CompTypeNum, // Component Type -- Integerized for this module
		bool const FirstHVACIteration,
		int & CompIndex,
		int const AirLoopNum, // air loop index for economizer lockout coordination
		bool const Sim, // if TRUE, simulate component; if FALSE, just set the coil exisitence flags
		int const OASysNum, // index to outside air system
		Optional_bool OAHeatingCoil = _, // TRUE indicates a heating coil has been found
		Optional_bool OACoolingCoil = _, // TRUE indicates a cooling coil has been found
		Optional_bool OAHX = _ // TRUE indicates a heat exchanger has been found
	);

	void
	SimOAMixer(
		std::string const & CompName,
		bool const FirstHVACIteration,
		int & CompIndex
	);

	void
	SimOAController(
		std::string const & CtrlName,
		int & CtrlIndex,
		bool const FirstHVACIteration,
		int const AirLoopNum
	);

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetOutsideAirSysInputs();

	void
	GetOAControllerInputs();

	void
	GetOAMixerInputs();

	// End of Get Input subroutines for the Module
	//******************************************************************************

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitOutsideAirSys(
		int const OASysNum, // unused1208
		bool const FirstHVACIteration
	);

	void
	InitOAController(
		int const OAControllerNum,
		bool const FirstHVACIteration,
		int const AirLoopNum
	);

	void
	InitOAMixer(
		int const OAMixerNum,
		bool const FirstHVACIteration
	);

	// End of Initialization Section of the Module
	//******************************************************************************

	// Beginning Calculation Section of the Module
	//******************************************************************************

	void
	CalcOAController(
		int const OAControllerNum,
		int const AirLoopNum
	);

	void
	CalcOAMixer( int const OAMixerNum );

	// End of Calculation/Simulation Section of the Module
	//******************************************************************************

	// Beginning Sizing Section of the Module
	//******************************************************************************

	void
	SizeOAController( int const OAControllerNum );

	// End of Sizing Section of the Module
	//******************************************************************************

	// Beginning Update/Reporting Section of the Module
	//******************************************************************************

	void
	UpdateOAController( int const OAControllerNum );

	void
	UpdateOAMixer( int const OAMixerNum );

	void
	ReportOAMixer( int const OAMixerNum ); // unused1208

	void
	ReportOAController( int const OAControllerNum ); // unused1208

	// End of Sizing Section of the Module
	//******************************************************************************

	// Beginning Utility Section of the Module
	//******************************************************************************

	Real64
	MixedAirControlTempResidual(
		Real64 const OASignal, // Relative outside air flow rate (0 to 1)
		Optional< FArray1S< Real64 > const > Par = _ // par(1) = mixed node number
	);

	FArray1D_int
	GetOAMixerNodeNumbers(
		std::string const & OAMixerName, // must match OA mixer names for the OA mixer type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetNumOAMixers();

	int
	GetNumOAControllers();

	int
	GetOAMixerReliefNodeNumber( int const OAMixerNum ); // Which Mixer

	int
	GetOASysControllerListIndex( int const OASysNumber ); // OA Sys Number

	int
	GetOASysNumSimpControllers( int const OASysNumber ); // OA Sys Number

	int
	GetOASysNumHeatingCoils( int const OASysNumber ); // OA Sys Number

	int
	GetOASysNumCoolingCoils( int const OASysNumber ); // OA Sys Number

	int
	GetOASystemNumber( std::string const & OASysName ); // OA Sys Name

	int
	FindOAMixerMatchForOASystem( int const OASysNumber ); // Which OA System

	int
	GetOAMixerIndex( std::string const & OAMixerName ); // Which Mixer

	int
	GetOAMixerInletNodeNumber( int const OAMixerNumber ); // Which Mixer

	int
	GetOAMixerReturnNodeNumber( int const OAMixerNumber ); // Which Mixer

	int
	GetOAMixerMixedNodeNumber( int const OAMixerNumber ); // Which Mixer

	bool
	CheckForControllerWaterCoil(
		std::string const & ControllerType, // should be passed in as UPPERCASE
		std::string const & ControllerName // should be passed in as UPPERCASE
	);

	void
	CheckControllerLists( bool & ErrFound );

	void
	SetOAControllerData(
		int const OACtrlNum, // Number of OA Controller
		bool & ErrorsFound, // Set to true if certain errors found
		Optional_string Name = _, // Name of Controller
		Optional_string ControllerType = _, // Controller Type
		Optional_int ControllerType_Num = _, // Parameter equivalent of Controller Type
		Optional_string LockoutType = _, // Lock out type
		Optional_bool FixedMin = _, // Fixed Minimum or Proportional Minimum
		Optional< Real64 > TempLim = _, // Temperature Limit
		Optional< Real64 > TempLowLim = _, // Temperature Lower Limit
		Optional< Real64 > EnthLim = _, // Enthalpy Limit
		Optional< Real64 > DPTempLim = _, // Dew Point Temperature Limit
		Optional_int EnthalpyCurvePtr = _, // Electronic Enthalpy Limit Curve Index
		Optional< Real64 > MaxOA = _, // Maximum outside air flow (m3/sec)
		Optional< Real64 > MinOA = _, // Minimum outside air flow (m3/sec)
		Optional_string EconoType = _, // EconoType = No Economizer,Differential Enthalpy, Differential Dry bulb,
		Optional_int MixNode = _, // Controlled node (mixed air node)
		Optional_int OANode = _, // Actuated node (outside air node)
		Optional_int InletNode = _, // Inlet Air Node for into Mixer  (BTG Nov 2004)
		Optional_int RelNode = _, // Relief Air Node Number
		Optional_int RetNode = _, // Return Air Node Number
		Optional_int HumidistatZoneNum = _, // Zone number where humidistat is located
		Optional< Real64 > HighRHOAFlowRatio = _, // Ratio of outside air flow to maximum outside air flow rate for high RH
		Optional_bool ModifyDuringHighOAMoisture = _, // TRUE if modify air flow is allowed during high OA humrat conditions
		Optional_int NodeNumofHumidistatZone = _, // actual node number of controlled zone
		Optional_int EconomizerOASchedPtr = _, // Time of day schedule for increasing outdoor air
		Optional_string BypassType = _ // ActivateBypassAtMinOAFlow, SetOAFlowRate
	);

	void
	CheckOAControllerName(
		std::string const & OAControllerName, // proposed name
		int const NumCurrentOAControllers, // Count on number of controllers
		bool & IsNotOK, // Pass through to VerifyName
		bool & IsBlank, // Pass through to VerifyName
		std::string const & SourceID // Pass through to VerifyName
	);

	void
	Checksetpoints(
		int const OAControllerNum, // index to OA controller
		Real64 const OutAirMinFrac, // Local variable used to calculate min OA fraction
		Real64 & OutAirSignal, // Used to set OA mass flow rate
		bool & EconomizerOperationFlag // logical used to show economizer status
	);

	int
	GetNumOASystems();

	int
	GetOACompListNumber( int const OASysNum ); // OA Sys Number

	std::string
	GetOACompName(
		int const OASysNum, // OA Sys Number
		int const InListNum // In-list Number
	);

	std::string
	GetOACompType(
		int const OASysNum, // OA Sys Number
		int const InListNum // In-list Number
	);

	int
	GetOACompTypeNum(
		int const OASysNum, // OA Sys Number
		int const InListNum // In-list Number
	);

	// End of Utility Section of the Module
	//******************************************************************************

	//     NOTICE

	//     Copyright � 1996-2014 The Board of Trustees of the University of Illinois
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

} // MixedAir

} // EnergyPlus

#endif
