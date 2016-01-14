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

#ifndef DataAirLoop_hh_INCLUDED
#define DataAirLoop_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DataAirLoop {

	// Using/Aliasing

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:

	// DERIVED TYPE DEFINITIONS:

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:

	extern int NumOASystems; // Number of Outdoor Air Systems
	extern bool AirLoopInputsFilled; // Set to TRUE after first pass through air loop

	// Variables specific to AirflowNetwork simulations.
	// Avoid using these for other purposes since these variables are only reset to 0 within AirflowNetworkBalanceManager, line 322.
	// Non-AFN simulations may have multiple air loops and use of these variables may yield unintended results.
	extern int LoopFanOperationMode; // OnOff fan operation mode
	extern Real64 LoopSystemOnMassFlowrate; // Loop mass flow rate during on cycle using an OnOff fan
	extern Real64 LoopSystemOffMassFlowrate; // Loop mass flow rate during off cycle using an OnOff fan
	extern Real64 LoopOnOffFanPartLoadRatio; // OnOff fan part load ratio
	extern Real64 LoopHeatingCoilMaxRTF; // Maximum run time fraction for electric or gas heating coil in an HVAC Air Loop
	extern Real64 LoopOnOffFanRTF; // OnOff fan run time fraction in an HVAC Air Loop
	extern Real64 LoopDXCoilRTF; // OnOff fan run time fraction in an HVAC Air Loop
	extern Real64 LoopCompCycRatio; // Loop compressor cycling ratio for multispeed heat pump

	// Types

	struct AirLoopZoneEquipConnectData
	{
		// Members
		std::string AirLoopName; // Name of Primary Air System
		int NumReturnNodes; // Number of return nodes connected to system
		int NumSupplyNodes; // number of supply nodes exiting primary air system
		int NumZonesCooled; // number of zones cooled by this primary air system
		int NumZonesHeated; // number of zones heated by this primary air system
		Array1D_int ZoneEquipReturnNodeNum; // Zone Equip side return air node numbers
		Array1D_int ZoneEquipSupplyNodeNum; // Zone equip side supply air node numbers
		Array1D_int AirLoopReturnNodeNum; // Air loop side return air node numbers
		Array1D_int AirLoopSupplyNodeNum; // Air loop side supply air node numbers
		Array1D_int CoolCtrlZoneNums; // Controlled zone numbers of zones cooled by this air loop
		Array1D_int HeatCtrlZoneNums; // Controlled zone numbers of zones heated by this air loop
		Array1D_int CoolZoneInletNodes; // Zone inlet node numbers of zones cooled by this air loop
		Array1D_int HeatZoneInletNodes; // Zone inlet node numbers of zones heated by this air loop
		Array1D_int TermUnitCoolInletNodes; // Air terminal unit cooling inlet node numbers for this air loop
		Array1D_int TermUnitHeatInletNodes; // Air terminal unit heating inlet node numbers for this air loop
		Array1D_int SupplyDuctType; // 1=main, 2=cooling, 3=heating, 4=other

		// Default Constructor
		AirLoopZoneEquipConnectData() :
			NumReturnNodes( 0 ),
			NumSupplyNodes( 0 ),
			NumZonesCooled( 0 ),
			NumZonesHeated( 0 )
		{}

	};

	struct AirLoopOutsideAirConnectData
	{
		// Members
		bool OASysExists; // true if there is an Outside Air Sys
		int OASysInletNodeNum; // node number of return air inlet to OA sys
		int OASysOutletNodeNum; // node number of mixed air outlet of OA sys

		// Default Constructor
		AirLoopOutsideAirConnectData() :
			OASysExists( false ),
			OASysInletNodeNum( 0 ),
			OASysOutletNodeNum( 0 )
		{}

	};

	struct DefinePriAirSysAvailMgrs
	{
		// Members
		int NumAvailManagers; // number of availability managers for this system
		int AvailStatus; // system availability status
		int StartTime; // cycle on time (in SimTimeSteps)
		int StopTime; // cycle off time (in SimTimeSteps)
		Real64 ReqSupplyFrac; // required system flow rate (as a fraction)
		Array1D_string AvailManagerName; // name of each availability manager
		Array1D_int AvailManagerType; // type of availability manager
		Array1D_int AvailManagerNum; // index for availability manager

		// Default Constructor
		DefinePriAirSysAvailMgrs() :
			NumAvailManagers( 0 ),
			AvailStatus( 0 ),
			StartTime( 0 ),
			StopTime( 0 ),
			ReqSupplyFrac( 0.0 )
		{}

	};

	struct AirLooptoZoneData // Derived type for air loop connection to zones on air loop
	{
		// Members
		int NumZones;
		Array1D_int Zone;
		Array1D_int ActualZoneNumber;

		// Default Constructor
		AirLooptoZoneData() :
			NumZones( 0 )
		{}

	};

	struct AirLoopControlData // Derived type for air control information
	{
		// Members
		std::string OACtrlName; // name of OA controller
		int OACtrlNum; // index of OA controller
		bool CyclingFan; // TRUE if currently the air loop supply fan is cycling
		bool AnyContFan; // TRUE if at any time supply fan is continuous
		int CycFanSchedPtr; // index of schedule indicating whether fan is cycling or continuous in a unitary system
		int FanOpMode; // 1=cycling fan cycling compressor; 2=constant fan cycling comptressor
		bool UnitarySys; // TRUE if a unitary system
		bool UnitarySysSimulating; // set FALSE for AirloopUnitarySystem after simulating to downstream coils can size independently
		bool Simple; // TRUE if system has 1 branch and 1 component
		bool CanNotLockoutEcono; // user input says econo lockout not allowed
		bool CanLockoutEconoWithHeating; // user input says econo lockout with heating is allowed
		bool CanLockoutEconoWithCompressor; // user input says econo lockout with compressor is allowed
		bool ReqstEconoLockoutWithHeating; // there is a request to lockout the economizer due to heating
		bool ReqstEconoLockoutWithCompressor; // there is a request to lockout the economizer due to compressor operation
		bool EconoActive; // if true economizer is active
		bool HeatRecoveryBypass; // if true heat recovery is bypassed (not active)
		bool ResimAirLoopFlag; // Same as SimAir, will trigger re-sim of air loops
		bool HeatRecoveryResimFlag; // Used to trigger new air loop sim when HX is used in OA system
		bool HeatRecoveryResimFlag2; // Used to trigger new air loop sim when HX is used in OA system
		bool CheckHeatRecoveryBypassStatus; // determines when heat recovery bypass is set
		bool EconomizerFlowLocked; // locks economizer flow for custon ERV operation
		bool HighHumCtrlActive; // if true high humidity control is active
		bool EconoLockout; // if true the economizer will be locked out (OA flow set to minimum)
		bool LoopFlowRateSet; // if true then the air loop flow rate should be set using ReqSupplyFrac
		bool NightVent; // if true then air loop is in night ventilation mode
		bool AllowWarmRestartFlag; // if true then speculative warm restart is attempted after first HVAC iteration
		bool NewFlowRateFlag; // true whenever the air mass flow rates have changed since last air loop sim
		bool ConvergedFlag; // true whenever the air loop sim was converged overall
		bool CoolingActiveFlag; // true whenever the air loop cooling coil is operating
		bool HeatingActiveFlag; // true whenever the air loop heating coil is operating
		bool OASysComponentsSimulated; // - true after OA components have been simulated
		bool AirLoopDCVFlag; // TRUE if the air loop has OA Controller specifying a Mechanical controller with DCV
		// - internal flag only

		// Default Constructor
		AirLoopControlData() :
			OACtrlNum( 0 ),
			CyclingFan( false ),
			AnyContFan( false ),
			CycFanSchedPtr( 0 ),
			FanOpMode( 0 ),
			UnitarySys( false ),
			UnitarySysSimulating( true ),
			Simple( false ),
			CanNotLockoutEcono( false ),
			CanLockoutEconoWithHeating( false ),
			CanLockoutEconoWithCompressor( false ),
			ReqstEconoLockoutWithHeating( false ),
			ReqstEconoLockoutWithCompressor( false ),
			EconoActive( false ),
			HeatRecoveryBypass( false ),
			ResimAirLoopFlag( false ),
			HeatRecoveryResimFlag( true ),
			HeatRecoveryResimFlag2( false ),
			CheckHeatRecoveryBypassStatus( false ),
			EconomizerFlowLocked( false ),
			HighHumCtrlActive( false ),
			EconoLockout( false ),
			LoopFlowRateSet( false ),
			NightVent( false ),
			AllowWarmRestartFlag( false ),
			NewFlowRateFlag( false ),
			ConvergedFlag( false ),
			CoolingActiveFlag( false ),
			HeatingActiveFlag( false ),
			OASysComponentsSimulated( false ),
			AirLoopDCVFlag( true )
		{}

	};

	struct AirLoopFlowData // Derived type for air loop flow information
	{
		// Members
		Real64 ZoneExhaust; // total of zone exhaust air mass flow rate for this loop [kg/s]
		Real64 ZoneExhaustBalanced; // zone exhaust air that is balanced by simple air flow for loop [kg/s]
		Real64 DesSupply; // design supply air mass flow rate for loop [kg/s]
		Real64 SysToZoneDesFlowRatio; // System design flow divided by the sum of the zone design flows
		Real64 ReqSupplyFrac; // required flow (as a fraction of DesSupply) set by a manager
		Real64 MinOutAir; // minimum outside air mass flow rate [kg/s]
		Real64 MaxOutAir; // maximum outside air mass flow rate [kg/s]
		Real64 OAMinFrac; // minimum outside air flow fraction this time step
		Real64 Previous; // Previous mass air flow rate for this loop [kg/s]
		Real64 SupFlow; // supply air flow rate [kg/s]
		Real64 RetFlow; // return air flow rate [kg/s]
		Real64 RetFlow0; // sum of zone return flows before adjusting for total loop exhaust
		Real64 RecircFlow; // sum of zone plenum recirculated flows
		Real64 FanPLR; // Operating PLR of air loop fan
		Real64 OAFrac; // fraction of outside air to mixed air mass flow rate
		Real64 ZoneMixingFlow; // total zone mixing net flow used to cap the return flow
		Real64 RetFlowAdjustment; // difference between user-specified return flow and default return flow
		bool FlowError; // error flag for flow error message

		// Default Constructor
		AirLoopFlowData() :
			ZoneExhaust( 0.0 ),
			ZoneExhaustBalanced( 0.0 ),
			DesSupply( 0.0 ),
			SysToZoneDesFlowRatio( 0.0 ),
			ReqSupplyFrac( 1.0 ),
			MinOutAir( 0.0 ),
			MaxOutAir( 0.0 ),
			OAMinFrac( 0.0 ),
			Previous( 0.0 ),
			SupFlow( 0.0 ),
			RetFlow( 0.0 ),
			RetFlow0( 0.0 ),
			RecircFlow( 0.0 ),
			FanPLR( 0.0 ),
			OAFrac( 0.0 ),
			ZoneMixingFlow( 0.0 ),
			RetFlowAdjustment( 0.0 ),
			FlowError( false )
		{}

	};

	struct OAControllerData
	{
		// Members
		bool EconoActive; // if true economizer is active
		bool HighHumCtrlActive; // if true high humidity control is active

		// Default Constructor
		OAControllerData() :
			EconoActive( false ),
			HighHumCtrlActive( false )
		{}

	};

	struct OutsideAirSysProps
	{
		// Members
		std::string Name;
		std::string ControllerListName;
		std::string ComponentListName;
		int ControllerListNum; // index of the Controller List
		int NumComponents;
		int NumControllers;
		int NumSimpleControllers; // number of CONTROLLER:SIMPLE objects in OA Sys controller list
		Array1D_string ComponentName;
		Array1D_string ComponentType;
		Array1D_int ComponentType_Num; // Parameterized (see above) Component Types this
		// module can address
		Array1D_int ComponentIndex; // Which one in list -- updated by routines called from here
		Array1D_string ControllerName;
		Array1D_string ControllerType;
		Array1D_int ControllerIndex; // Which one in list -- updated by routines called from here

		// Default Constructor
		OutsideAirSysProps() :
			ControllerListNum( 0 ),
			NumComponents( 0 ),
			NumControllers( 0 ),
			NumSimpleControllers( 0 )
		{}

	};

	// Object Data
	extern Array1D< AirLoopZoneEquipConnectData > AirToZoneNodeInfo;
	extern Array1D< AirLoopOutsideAirConnectData > AirToOANodeInfo;
	extern Array1D< DefinePriAirSysAvailMgrs > PriAirSysAvailMgr;
	extern Array1D< AirLooptoZoneData > AirLoopZoneInfo;
	extern Array1D< AirLoopControlData > AirLoopControlInfo;
	extern Array1D< AirLoopFlowData > AirLoopFlow;
	extern Array1D< OAControllerData > OAControllerInfo;
	extern Array1D< OutsideAirSysProps > OutsideAirSys;

	// Clears the global data in DataAirLoop.
	// Needed for unit tests, should not be normally called.
	void
	clear_state();

} // DataAirLoop

} // EnergyPlus

#endif
