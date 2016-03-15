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

#ifndef MixedAir_hh_INCLUDED
#define MixedAir_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
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
	extern int const Humidifier;

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
	//INTEGER, PARAMETER :: SOAM_ProportionalControlSchOcc = 4     ! Use ASHRAE Standard 62.1-2004 or Trane Engineer's newsletter (volume 34-5)
	//                                                       ! to calculate the system level outdoor air flow rates based on scheduled occupancy
	//INTEGER, PARAMETER :: SOAM_IAQPGC = 5   ! Use ASHRAE Standard 62.1-2004 IAQP to calculate the system level outdoor air flow rates
	//                                        ! based on the generic contaminant setpoint
	//INTEGER, PARAMETER :: SOAM_IAQPCOM = 6  ! Take the maximum outdoor air rate from both CO2 and generic contaminant controls
	//                                        ! based on the generic contaminant setpoint
	//INTEGER, PARAMETER :: SOAM_ProportionalControlDesOcc = 7     ! Use ASHRAE Standard 62.1-2004 or Trane Engineer's newsletter (volume 34-5)
	//                                                       ! to calculate the system level outdoor air flow rates based on design occupancy

	extern Array1D_string const CurrentModuleObjects;

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

	extern Array1D_bool MyOneTimeErrorFlag;
	extern Array1D_bool MyOneTimeCheckUnitarySysFlag;
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
		Array1D_string ControllerType;
		Array1D_string ControllerName;

		// Default Constructor
		ControllerListProps() :
			NumControllers( 0 )
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
		bool ManageDemand; // Used by demand manager to manage ventilation
		Real64 DemandLimitFlowRate; //Current demand limit if demand manager is ON
		Real64 MaxOAFracBySetPoint; // The maximum OA fraction due to freezing cooling coil check 
		int MixedAirSPMNum; // index of mixed air setpoint manager
		bool CoolCoilFreezeCheck; // if true, cooling coil freezing is prevented by recalculating the amount of OA

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
			HeatRecoveryBypassControlType( BypassWhenWithinEconomizerLimits ),
			ManageDemand( false ),
			DemandLimitFlowRate( 0.0 ),
			MaxOAFracBySetPoint( 0 ),
			MixedAirSPMNum( 0 ),
			CoolCoilFreezeCheck( false )
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
		Array1D< Real64 > ZoneOAAreaRate; // Mechanical ventilation rate (m3/s/m2) for each zone
		Array1D< Real64 > ZoneOAPeopleRate; // Mechanical ventilation rate (m3/s/person) for each zone
		Array1D< Real64 > ZoneOAFlow; // OA Flow Rate (m3/s/zone) for each zone
		Array1D< Real64 > ZoneOAACH; // OA ACH (m3/s/volume) for each zone
		Array1D_int Zone; // Zones requiring mechanical ventilation
		Array1D_int ZoneDesignSpecOAObjIndex; // index of the design specification outdoor air object
		// for each zone in zone list
		Array1D_string ZoneDesignSpecOAObjName; // name of the design specification outdoor air object
		// for each zone in zone list
		int CO2MaxMinLimitErrorCount; // Counter when max CO2 concentration < min CO2 concentration
		// For SOAM_ProportionalControlSchOcc
		int CO2MaxMinLimitErrorIndex; // Index for max CO2 concentration < min CO2 concentration recurring error message
		// For SOAM_ProportionalControlSchOcc
		int CO2GainErrorCount; // Counter when CO2 generation from people is zero for SOAM_ProportionalControlSchOcc
		int CO2GainErrorIndex; // Index for recurring error message when CO2 generation from people is zero
		// For SOAM_ProportionalControlSchOcc
		Array1D< Real64 > ZoneADEffCooling; // Zone air distribution effectiveness in cooling mode
		// for each zone
		Array1D< Real64 > ZoneADEffHeating; // Zone air distribution effectiveness in heating mode
		// for each zone
		Array1D_int ZoneADEffSchPtr; // Pointer to the zone air distribution effectiveness schedule
		// for each zone
		Array1D_string ZoneADEffSchName; // Zone air distribution effectiveness schedule name
		// for each zone
		Array1D_int ZoneDesignSpecADObjIndex; // index of the design specification zone air
		//  distribution object for each zone in the zone list
		Array1D_string ZoneDesignSpecADObjName; // name of the design specification zone air
		// distribution object for each zone in the zone list
		Array1D< Real64 > ZoneSecondaryRecirculation; // zone air secondary recirculation ratio

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

	};

	// Object Data
	extern Array1D< ControllerListProps > ControllerLists;
	extern Array1D< OAControllerProps > OAController;
	extern Array1D< OAMixerProps > OAMixer;
	extern Array1D< VentilationMechanicalProps > VentilationMechanical;

	// Functions

	Real64 OAGetFlowRate(int OAPtr);

	Real64 OAGetMinFlowRate(int OAPtr);

	void OASetDemandManagerVentilationState(int OAPtr, bool aState);

	void OASetDemandManagerVentilationFlow(int OAPtr, Real64 aFlow);

	int GetOAController(std::string const & OAName);

	// Clears the global data in MixedAir.
	// Needed for unit tests, should not be normally called.
	void
	clear_state();

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

	void
	ProcessOAControllerInputs(
		std::string const & CurrentModuleObject,
		int const OutAirNum,
		Array1_string const & AlphArray,
		int & NumAlphas,
		Array1< Real64 > const & NumArray,
		int & NumNums,
		Array1_bool const & lNumericBlanks, //Unused
		Array1_bool const & lAlphaBlanks,
		Array1_string const & cAlphaFields,
		Array1_string const & cNumericFields, //Unused
		bool & ErrorsFound // If errors found in input
	);

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
		Array1< Real64 > const & Par // par(1) = mixed node number
	);

	Array1D_int
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
	GetOASysNumHXs( int const OASysNumber ); // OA Sys Number

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

} // MixedAir

} // EnergyPlus

#endif
