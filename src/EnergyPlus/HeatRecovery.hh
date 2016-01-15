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
		bool myEnvrnFlag; // one-time-init flag

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
			UnBalancedErrIndex( 0 ),
			myEnvrnFlag( true )
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

	};

	// Object Data
	extern Array1D< HeatExchCond > ExchCond;
	extern Array1D< BalancedDesDehumPerfData > BalDesDehumPerfData;

	// Functions

	void
	clear_state();

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
		int const FanOpMode, // Supply air fan operating mode (1=cycling, 2=constant)
		Optional_bool_const EconomizerFlag = _, // economizer flag pass by air loop or OA sys
		Optional_bool_const HighHumCtrlFlag = _, // high humidity control flag passed by airloop or OA sys
		Optional< Real64 const > HXPartLoadRatio = _ //
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

} // HeatRecovery

} // EnergyPlus

#endif
