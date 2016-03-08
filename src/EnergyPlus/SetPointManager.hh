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

#ifndef SetPointManager_hh_INCLUDED
#define SetPointManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <DataLoopNode.hh>

namespace EnergyPlus {

namespace SetPointManager {

	// Using/Aliasing

	// Data
	//MODULE PARAMETER DEFINITIONS:
	extern int const MaxTemp;
	extern int const MinTemp;
	extern int const TempFirst;
	extern int const FlowFirst;
	extern int const iRefTempType_WetBulb;
	extern int const iRefTempType_DryBulb;
	extern int const iRefGroundTempObjType_BuildingSurface;
	extern int const iRefGroundTempObjType_Shallow;
	extern int const iRefGroundTempObjType_Deep;
	extern int const iRefGroundTempObjType_FCfactorMethod;

	// following are used to reduce string comparisons related to CtrlVarType
	extern int const iCtrlVarType_Temp; // control type 'Temperature'
	extern int const iCtrlVarType_MaxTemp; // control type 'MaximumTemperature'
	extern int const iCtrlVarType_MinTemp; // control type 'MinimumTemperature'
	extern int const iCtrlVarType_HumRat; // control Type 'HumidityRatio'
	extern int const iCtrlVarType_MaxHumRat; // control Type 'MaximumHumidityRatio'
	extern int const iCtrlVarType_MinHumRat; // control Type 'MinimumHumidityRatio'
	extern int const iCtrlVarType_MassFlow; // control type 'MassFlowRate'
	extern int const iCtrlVarType_MaxMassFlow; // control Type 'MaximumMassFlowRate'
	extern int const iCtrlVarType_MinMassFlow; // control Type 'MinimumMassFlowRate'

	extern int const NumValidCtrlTypes;
	extern Array1D_string const cValidCtrlTypes;

	// following are used to reduce string comparisons related to CtrlVarType
	extern int const iSPMType_Scheduled;
	extern int const iSPMType_ScheduledDual;
	extern int const iSPMType_OutsideAir;
	extern int const iSPMType_SZReheat;
	extern int const iSPMType_SZHeating;
	extern int const iSPMType_SZCooling;
	extern int const iSPMType_SZMinHum;
	extern int const iSPMType_SZMaxHum;
	extern int const iSPMType_MixedAir;
	extern int const iSPMType_OutsideAirPretreat;
	extern int const iSPMType_Warmest;
	extern int const iSPMType_Coldest;
	extern int const iSPMType_WarmestTempFlow;
	extern int const iSPMType_RAB;
	extern int const iSPMType_MZCoolingAverage;
	extern int const iSPMType_MZHeatingAverage;
	extern int const iSPMType_MZMinHumAverage;
	extern int const iSPMType_MZMaxHumAverage;
	extern int const iSPMType_MZMinHum;
	extern int const iSPMType_MZMaxHum;
	extern int const iSPMType_FollowOATemp;
	extern int const iSPMType_FollowSysNodeTemp;
	extern int const iSPMType_GroundTemp;
	extern int const iSPMType_CondEntReset;
	extern int const iSPMType_IdealCondEntReset;
	extern int const iSPMType_SZOneStageCooling;
	extern int const iSPMType_SZOneStageHeating;
	extern int const iSPMType_ReturnWaterResetChW;
	extern int const iSPMType_ReturnWaterResetHW;

	extern int const NumValidSPMTypes;
	extern Array1D_string const cValidSPMTypes;

	//Type declarations in SetPointManager module

	// This one is used for conflicting node checks and is DEALLOCATED at the end of VerifySetPointManagers

	//MODULE VARIABLE DECLARATIONS:
	extern int NumAllSetPtMgrs; // Number of all Setpoint Managers found in input
	extern int NumSchSetPtMgrs; // Number of Scheduled Setpoint Managers found in input
	extern int NumDualSchSetPtMgrs; // Number of Scheduled Dual Setpoint Managers found in input
	extern int NumOutAirSetPtMgrs; // Number of Outside Air Setpoint Managers found in input
	extern int NumSZRhSetPtMgrs; // number of single zone reheat setpoint managers
	extern int NumSZHtSetPtMgrs; // number of single zone heating setpoint managers
	extern int NumSZClSetPtMgrs; // number of single zone cooling setpoint managers
	extern int NumSZMinHumSetPtMgrs; // number of Single Zone Minimum Humidity Setpoint Managers
	extern int NumSZMaxHumSetPtMgrs; // number of Single Zone Maximum Humidity Setpoint Managers
	extern int NumMixedAirSetPtMgrs; // number of mixed air setpoint managers
	extern int NumOAPretreatSetPtMgrs; // number of outside air pretreat setpoint managers
	extern int NumWarmestSetPtMgrs; // number of Warmest setpoint managers
	extern int NumColdestSetPtMgrs; // number of Coldest setpoint managers
	extern int NumWarmestSetPtMgrsTempFlow; // number of Warmest Temp Flow setpoint managers
	extern int NumRABFlowSetPtMgrs; // number of return air bypass temperature-based flow setpoint manager
	extern int NumMZClgAverageSetPtMgrs; // number of Multizone:Cooling:Average setpoint managers
	extern int NumMZHtgAverageSetPtMgrs; // number of Multizone:Heating:Average setpoint managers
	extern int NumMZAverageMinHumSetPtMgrs; // number of MultiZone:MinimumHumidity:Average setpoint managers
	extern int NumMZAverageMaxHumSetPtMgrs; // number of MultiZone:MaximumHumidity:Average setpoint managers
	extern int NumMZMinHumSetPtMgrs; // number of MultiZone:Humidity:Minimum setpoint managers
	extern int NumMZMaxHumSetPtMgrs; // number of MultiZone:Humidity:Maximum setpoint managers
	extern int NumFollowOATempSetPtMgrs; // number of SetpointManager:FollowOutdoorAirTemperature setpoint managers
	extern int NumFollowSysNodeTempSetPtMgrs; // number of SetpointManager:FollowSystemNodeTemperature setpoint managers
	extern int NumGroundTempSetPtMgrs; // number of SetpointManager:FollowGroundTemperature setpoint managers
	extern int NumCondEntSetPtMgrs; // number of Condenser Entering Reset setpoint managers
	extern int NumIdealCondEntSetPtMgrs; // number of Ideal Condenser Entering Temperature setpoint managers
	extern int NumSZOneStageCoolingSetPtMgrs; // number of single zone one stage cooling setpoint managers
	extern int NumSZOneStageHeatingSetPtMgrs; // number of singel zone one stage heating setpoint managers
	extern int NumReturnWaterResetChWSetPtMgrs; // number of chilled-water return water reset setpoint managers
	extern int NumReturnWaterResetHWSetPtMgrs; // number of hot-water return water reset setpoint managers
	extern int NumSchTESSetPtMgrs; // Number of TES Scheduled Setpoint Managers found in input

	extern bool ManagerOn;
	extern bool GetInputFlag; // First time, input is "gotten"

	// temperature-based flow control manager
	// Average Cooling Set Pt Mgr
	// Average Heating Set Pt Mgr
	// Average Minimum humidity ratio Set Pt Mgr
	// Average Maximum humidity ratio Set Pt Mgr

	// Temperature Setpoint Manager data
	// Node Temp Setpoint Manager data
	// Manager data

	//SUBROUTINE SPECIFICATIONS FOR MODULE SetPointManager

	// Types

	struct DataSetPointManager // Derived type for all Setpoint Managers
	{
		// Members
		std::string Name; // name of setpoint manager
		int SPMType; // integer representing type of setpoint manager
		int CtrlTypeMode; // set to iCtrlVarType_xxxx
		int NumCtrlNodes; // number of control nodes
		Array1D_int CtrlNodes; // index to control node
		int AirLoopNum; // index to air loop
		std::string AirLoopName; // name of air loop

		// Default Constructor
		DataSetPointManager() :
			SPMType( 0 ),
			CtrlTypeMode( 0 ),
			NumCtrlNodes( 0 ),
			AirLoopNum( 0 )
		{}

	};

	struct DefineScheduledSetPointManager // Derived type for Scheduled Setpoint Manager data
	{
		// Members
		std::string Name;
		std::string CtrlVarType;
		int CtrlTypeMode; // set to iCtrlVarType_xxxx
		std::string Sched;
		int SchedPtr;
		int NumCtrlNodes;
		std::string CtrlNodeListName;
		Array1D_int CtrlNodes;
		Real64 SetPt;

		// Default Constructor
		DefineScheduledSetPointManager() :
			CtrlTypeMode( 0 ),
			SchedPtr( 0 ),
			NumCtrlNodes( 0 ),
			SetPt( 0.0 )
		{}

		void calculate();

	};

	struct DefineSchedDualSetPointManager // Derived type for Scheduled Dual Setpoint Manager
	{
		// Members
		std::string Name;
		std::string CtrlVarType;
		int CtrlTypeMode; // set to iCtrlVarType_xxxx
		std::string SchedHi;
		std::string SchedLo;
		int SchedPtrHi;
		int SchedPtrLo;
		int NumCtrlNodes;
		std::string CtrlNodeListName;
		Array1D_int CtrlNodes;
		Real64 SetPtHi;
		Real64 SetPtLo;

		// Default Constructor
		DefineSchedDualSetPointManager() :
			CtrlTypeMode( 0 ),
			SchedPtrHi( 0 ),
			SchedPtrLo( 0 ),
			NumCtrlNodes( 0 ),
			SetPtHi( 0.0 ),
			SetPtLo( 0.0 )
		{}

		void calculate();

	};

	struct DefineOutsideAirSetPointManager // Derived type for Outside Air Setpoint Manager Data
	{
		// Members
		std::string Name;
		std::string CtrlVarType; // type of variable to be set
		int CtrlTypeMode; // set to iCtrlVarType_xxxx
		Real64 OutLowSetPt1; // 1st setpoint at outside low
		Real64 OutLow1; // 1st Outside low
		Real64 OutHighSetPt1; // 1st setpoint at outside high
		Real64 OutHigh1; // 1st Outside high
		std::string Sched; // Optional schedule
		int SchedPtr; // Schedule index
		Real64 OutLowSetPt2; // 2nd setpoint at outside low (optional)
		Real64 OutLow2; // 2nd Outside low (optional)
		Real64 OutHighSetPt2; // 2nd setpoint at outside high (optional)
		Real64 OutHigh2; // 2nd Outside high (optional)
		int NumCtrlNodes;
		std::string CtrlNodeListName;
		Array1D_int CtrlNodes;
		Real64 SetPt;

		// Default Constructor
		DefineOutsideAirSetPointManager() :
			CtrlTypeMode( 0 ),
			OutLowSetPt1( 0.0 ),
			OutLow1( 0.0 ),
			OutHighSetPt1( 0.0 ),
			OutHigh1( 0.0 ),
			SchedPtr( 0 ),
			OutLowSetPt2( 0.0 ),
			OutLow2( 0.0 ),
			OutHighSetPt2( 0.0 ),
			OutHigh2( 0.0 ),
			NumCtrlNodes( 0 ),
			SetPt( 0.0 )
		{}

		void calculate(
			Optional_int_const NodeNum = _, // When Init Calls this routine, it passes the cur node number
			Optional_bool_const InitFlag = _ // When Init Calls this routine, it passes True
		);

		Real64
		CalcSetPoint(
			Real64 OutLowTemp,
			Real64 OutHighTemp,
			Real64 OutDryBulbTemp,
			Real64 SetTempAtOutLow,
			Real64 SetTempAtOutHigh
		);
	};

	struct DefineSZReheatSetPointManager // Derived type for the Single Zone Reheat Setpoint Manager data
	{
		// Members
		std::string Name;
		std::string CtrlVarType; // type of variable to be set
		int CtrlTypeMode; // set to iCtrlVarType_xxxx
		std::string ControlZoneName; // name of the control zone (zone with main thermostat)
		int ControlZoneNum; // number (index into Zone array) of control zone
		int ZoneNodeNum; // zone node number
		int ZoneInletNodeNum; // inlet node number for the SZRH air
		Real64 MinSetTemp; // minimum supply air setpoint temperature
		Real64 MaxSetTemp; // maximum supply air setpoint temperature
		int MixedAirNode; // mixed air node number
		int FanNodeIn; // fan inlet node number
		int FanNodeOut; // fan outlet node number
		int AirLoopNum; // air loop index of air loop associated with this setpoint manager
		int OAInNode; // outside airstream inlet node to the OA mixer
		int RetNode; // return node inlet to OA mixer
		int LoopInNode; // Primary Air System inlet node
		int NumCtrlNodes;
		Array1D_int CtrlNodes; // node numbers of nodes where setpoint is to be set
		Real64 SetPt; // the setpoint

		// Default Constructor
		DefineSZReheatSetPointManager() :
			CtrlTypeMode( 0 ),
			ControlZoneNum( 0 ),
			ZoneNodeNum( 0 ),
			ZoneInletNodeNum( 0 ),
			MinSetTemp( 0.0 ),
			MaxSetTemp( 0.0 ),
			MixedAirNode( 0 ),
			FanNodeIn( 0 ),
			FanNodeOut( 0 ),
			AirLoopNum( 0 ),
			OAInNode( 0 ),
			RetNode( 0 ),
			LoopInNode( 0 ),
			NumCtrlNodes( 0 ),
			SetPt( 0.0 )
		{}

		void calculate();

	};

	struct DefineSZHeatingSetPointManager // Derived type for the Single Zone Heating Setpoint Manager data
	{
		// Members
		std::string Name;
		std::string CtrlVarType; // type of variable to be set
		int CtrlTypeMode; // set to iCtrlVarType_xxxx
		std::string ControlZoneName; // name of the control zone (zone with main thermostat)
		int ControlZoneNum; // number (index into Zone array) of control zone
		int ZoneNodeNum; // zone node number
		int ZoneInletNodeNum; // inlet node number for the supply air
		Real64 MinSetTemp; // minimum supply air setpoint temperature
		Real64 MaxSetTemp; // maximum supply air setpoint temperature
		int NumCtrlNodes;
		Array1D_int CtrlNodes; // node numbers of nodes where setpoint is to be set
		Real64 SetPt; // the setpoint

		// Default Constructor
		DefineSZHeatingSetPointManager() :
			CtrlTypeMode( 0 ),
			ControlZoneNum( 0 ),
			ZoneNodeNum( 0 ),
			ZoneInletNodeNum( 0 ),
			MinSetTemp( 0.0 ),
			MaxSetTemp( 0.0 ),
			NumCtrlNodes( 0 ),
			SetPt( 0.0 )
		{}

		void calculate();

	};

	struct DefineSZCoolingSetPointManager // Derived type for the Single Zone Cooling Setpoint Manager data
	{
		// Members
		std::string Name;
		std::string CtrlVarType; // type of variable to be set
		int CtrlTypeMode; // set to iCtrlVarType_xxxx
		std::string ControlZoneName; // name of the control zone (zone with main thermostat)
		int ControlZoneNum; // number (index into Zone array) of control zone
		int ZoneNodeNum; // zone node number
		int ZoneInletNodeNum; // inlet node number for the supply air
		Real64 MinSetTemp; // minimum supply air setpoint temperature
		Real64 MaxSetTemp; // maximum supply air setpoint temperature
		int NumCtrlNodes;
		Array1D_int CtrlNodes; // node numbers of nodes where setpoint is to be set
		Real64 SetPt; // the setpoint

		// Default Constructor
		DefineSZCoolingSetPointManager() :
			CtrlTypeMode( 0 ),
			ControlZoneNum( 0 ),
			ZoneNodeNum( 0 ),
			ZoneInletNodeNum( 0 ),
			MinSetTemp( 0.0 ),
			MaxSetTemp( 0.0 ),
			NumCtrlNodes( 0 ),
			SetPt( 0.0 )
		{}

		void calculate();

	};

	struct DefineSZMinHumSetPointManager // Derived Type for Single Zone Minimum Humidity Setpoint Manager data
	{
		// Members
		std::string Name;
		std::string CtrlVarType; // type of variable to be set
		int CtrlTypeMode; // set to iCtrlVarType_xxxx
		int NumZones; // number of zones whose humidity is being controlled
		int NumCtrlNodes; // number of nodes whose humidity ratio is being set
		Array1D_int ZoneNodes; // zone node numbers of zones being controlled
		Array1D_int ZoneNum; // actual zone number ( index into Zone array)
		Array1D_int CtrlZoneNum; // index into ZoneEquipConfig
		Array1D_int CtrlNodes; // nodes where humidity ratio is being set
		Real64 SetPt; // the setpoint

		// Default Constructor
		DefineSZMinHumSetPointManager() :
			CtrlTypeMode( 0 ),
			NumZones( 0 ),
			NumCtrlNodes( 0 ),
			SetPt( 0.0 )
		{}

		void calculate();

	};

	struct DefineSZMaxHumSetPointManager // Derived Type for Single Zone Maximum Humidity Setpoint Manager data
	{
		// Members
		std::string Name;
		std::string CtrlVarType; // type of variable to be set
		int CtrlTypeMode; // set to iCtrlVarType_xxxx
		int NumZones; // number of zones whose humidity is being controlled
		int NumCtrlNodes; // number of nodes whose humidity ratio is being set
		Array1D_int ZoneNodes; // zone node numbers of zones being controlled
		Array1D_int ZoneNum; // actual zone number (index into Zone array)
		Array1D_int CtrlZoneNum; // index into ZoneEquipConfig
		Array1D_int CtrlNodes; // nodes where humidity ratio is being set
		Real64 SetPt; // the setpoint

		// Default Constructor
		DefineSZMaxHumSetPointManager() :
			CtrlTypeMode( 0 ),
			NumZones( 0 ),
			NumCtrlNodes( 0 ),
			SetPt( 0.0 )
		{}

		void calculate();

	};

	struct DefineMixedAirSetPointManager
	{
		// Members
		std::string Name;
		std::string CtrlVarType; // type of variable to be set
		int CtrlTypeMode; // set to iCtrlVarType_xxxx
		int RefNode; // reference node number
		int FanInNode; // supply fan inlet node number
		int FanOutNode; // Supplt fan outlet node number
		int NumCtrlNodes; // number of nodes whose humidity ratio is being set
		Array1D_int CtrlNodes; // node numbers of nodes where setpoint is to be set
		Real64 SetPt; // the setpoint
		bool MySetPointCheckFlag; // used for mixed air SPM test for missing SP
		bool FreezeCheckEnable; // Enable freezing check
		int CoolCoilInNode; // Cooling coil inlet node number
		int CoolCoilOutNode; // Cooling coil outlet node number
		Real64 MinCoolCoilOutTemp; // The minimum temperature at cooling coil outlet node

		// Default Constructor
		DefineMixedAirSetPointManager() :
			CtrlTypeMode( 0 ),
			RefNode( 0 ),
			FanInNode( 0 ),
			FanOutNode( 0 ),
			NumCtrlNodes( 0 ),
			SetPt( 0.0 ),
			MySetPointCheckFlag( true ),
			FreezeCheckEnable( false ),
			CoolCoilInNode( 0 ),
			CoolCoilOutNode( 0 ),
		    MinCoolCoilOutTemp( 7.2 )

		{}

		void calculate();

	};

	struct DefineOAPretreatSetPointManager
	{
		// Members
		std::string Name;
		std::string CtrlVarType; // type of variable to be set
		int CtrlTypeMode; // set to iCtrlVarType_xxxx
		int RefNode; // reference node number
		int MixedOutNode; // mixed air outlet node number
		int OAInNode; // outside air inlet node number
		int ReturnInNode; // return air inlet node number
		Real64 MinSetTemp; // minimum supply air setpoint temperature [C]
		Real64 MaxSetTemp; // maximum supply air setpoint temperature [C]
		Real64 MinSetHumRat; // minimum supply air setpoint humidity ratio [kg/kg]
		Real64 MaxSetHumRat; // maximum supply air setpoint humidity ratio [kg/kg]
		int NumCtrlNodes; // number of nodes whose humidity ratio is being set
		Array1D_int CtrlNodes; // node numbers of nodes where setpoint is to be set
		Real64 SetPt; // the setpoint
		bool MySetPointCheckFlag; // used for DOAS SPM test for missing SP

		// Default Constructor
		DefineOAPretreatSetPointManager() :
			CtrlTypeMode( 0 ),
			RefNode( 0 ),
			MixedOutNode( 0 ),
			OAInNode( 0 ),
			ReturnInNode( 0 ),
			MinSetTemp( 0.0 ),
			MaxSetTemp( 0.0 ),
			MinSetHumRat( 0.0 ),
			MaxSetHumRat( 0.0 ),
			NumCtrlNodes( 0 ),
			SetPt( 0.0 ),
			MySetPointCheckFlag( true )
		{}

		void calculate();

	};

	struct DefineWarmestSetPointManager
	{
		// Members
		std::string Name;
		std::string CtrlVarType; // type of variable to be set
		int CtrlTypeMode; // set to iCtrlVarType_xxxx
		std::string AirLoopName; // name of air loop that will use "warmest zone" strategy
		int AirLoopNum; // index of named air loop
		Real64 MinSetTemp; // minimum supply air setpoint temperature
		Real64 MaxSetTemp; // maximum supply air setpoint temperature
		int Strategy; // supply flow and temperature set strategy
		// 1 = MaxTemp
		int NumCtrlNodes; // number of nodes whose temperature is being set
		Array1D_int CtrlNodes; // nodes where temperature is being set
		Real64 SetPt; // the setpoint

		// Default Constructor
		DefineWarmestSetPointManager() :
			CtrlTypeMode( 0 ),
			AirLoopNum( 0 ),
			MinSetTemp( 0.0 ),
			MaxSetTemp( 0.0 ),
			Strategy( 0 ),
			NumCtrlNodes( 0 ),
			SetPt( 0.0 )
		{}

		void calculate();

	};

	struct DefineColdestSetPointManager
	{
		// Members
		std::string Name;
		std::string CtrlVarType; // type of variable to be set
		int CtrlTypeMode; // set to iCtrlVarType_xxxx
		std::string AirLoopName; // name of air loop that will use "coldest zone" strategy
		int AirLoopNum; // index of named air loop
		Real64 MinSetTemp; // minimum supply air setpoint temperature
		Real64 MaxSetTemp; // maximum supply air setpoint temperature
		int Strategy; // supply flow and temperature set strategy
		// 2 = MinTemp
		int NumCtrlNodes; // number of nodes whose temperature is being set
		Array1D_int CtrlNodes; // nodes where temperature is being set
		Real64 SetPt; // the setpoint

		// Default Constructor
		DefineColdestSetPointManager() :
			CtrlTypeMode( 0 ),
			AirLoopNum( 0 ),
			MinSetTemp( 0.0 ),
			MaxSetTemp( 0.0 ),
			Strategy( 0 ),
			NumCtrlNodes( 0 ),
			SetPt( 0.0 )
		{}

		void calculate();

	};

	struct DefWarmestSetPtManagerTempFlow
	{
		// Members
		std::string Name;
		std::string CtrlVarType; // type of variable to be set
		int CtrlTypeMode; // set to iCtrlVarType_xxxx
		std::string AirLoopName; // name of air loop that will use "warmest zone" strategy
		int AirLoopNum; // index of named air loop
		Real64 MinSetTemp; // minimum supply air setpoint temperature
		Real64 MaxSetTemp; // maximum supply air setpoint temperature
		int Strategy; // supply flow and temperature set strategy
		// 1 = TempFirst, 2 = FlowFirst
		int NumCtrlNodes; // number of nodes whose temperature is being set
		Array1D_int CtrlNodes; // nodes where temperature is being set
		Real64 SetPt; // the setpoint
		Real64 MinTurndown; // minimum fractional flow rate
		Real64 Turndown; // fractional flow rate
		int CritZoneNum;
		bool SimReady;

		// Default Constructor
		DefWarmestSetPtManagerTempFlow() :
			CtrlTypeMode( 0 ),
			AirLoopNum( 0 ),
			MinSetTemp( 0.0 ),
			MaxSetTemp( 0.0 ),
			Strategy( 0 ),
			NumCtrlNodes( 0 ),
			SetPt( 0.0 ),
			MinTurndown( 0.0 ),
			Turndown( 0.0 ),
			CritZoneNum( 0 ),
			SimReady( false )
		{}

		void calculate();

	};

	struct DefRABFlowSetPointManager
	{
		// Members
		std::string Name;
		std::string CtrlVarType; // type of variable to be set
		int CtrlTypeMode; // set to iCtrlVarType_xxxx
		int NumCtrlNodes; // number of nodes whose temperature is being set
		Array1D_int CtrlNodes; // nodes where temperature is being set
		std::string AirLoopName; // name of air loop that will use "warmest zone" strategy
		int AirLoopNum; // index of named air loop
		std::string Sched; // name of a schedule of supply air setpoint temperatures
		int SchedPtr; // index of the above schedule
		Real64 FlowSetPt; // mass flow rate setpoint (kg/s)
		int RABMixInNode;
		int SupMixInNode;
		int MixOutNode;
		int RABSplitOutNode;
		int SysOutNode;
		int AllSetPtMgrIndex; // index of RAB SP manager in AllSetPtMgr structure

		// Default Constructor
		DefRABFlowSetPointManager() :
			CtrlTypeMode( 0 ),
			NumCtrlNodes( 0 ),
			AirLoopNum( 0 ),
			SchedPtr( 0 ),
			FlowSetPt( 0.0 ),
			RABMixInNode( 0 ),
			SupMixInNode( 0 ),
			MixOutNode( 0 ),
			RABSplitOutNode( 0 ),
			SysOutNode( 0 ),
			AllSetPtMgrIndex( 0 )
		{}

		void calculate();

	};

	struct DefMultiZoneAverageCoolingSetPointManager // derived type for SetpointManager:Multizone:Cooling:Average data
	{
		// Members
		std::string Name;
		std::string CtrlVarType; // type of variable to be set
		int CtrlTypeMode; // set to iCtrlVarType_xxxx
		std::string AirLoopName; // name of air loop that will use "MultiZone:Cooling:Average" strategy
		int AirLoopNum; // index of named air loop
		Real64 MinSetTemp; // minimum supply air setpoint temperature [C]
		Real64 MaxSetTemp; // maximum supply air setpoint temperature [C]
		int NumCtrlNodes; // number of nodes whose temperature is being set
		Array1D_int CtrlNodes; // nodes where temperature is being set
		Real64 SetPt; // the temperature setpoint [C]

		// Default Constructor
		DefMultiZoneAverageCoolingSetPointManager() :
			CtrlTypeMode( 0 ),
			AirLoopNum( 0 ),
			MinSetTemp( 0.0 ),
			MaxSetTemp( 0.0 ),
			NumCtrlNodes( 0 ),
			SetPt( 0.0 )
		{}

		void calculate();

	};

	struct DefMultiZoneAverageHeatingSetPointManager // derived type for SetpointManager:Multizone:Heating:Average data
	{
		// Members
		std::string Name;
		std::string CtrlVarType; // type of variable to be set
		int CtrlTypeMode; // set to iCtrlVarType_xxxx
		std::string AirLoopName; // name of air loop that will use "MultiZone:Heating:Average" strategy
		int AirLoopNum; // index of named air loop
		Real64 MinSetTemp; // minimum supply air setpoint temperature [C]
		Real64 MaxSetTemp; // maximum supply air setpoint temperature [C]
		int NumCtrlNodes; // number of nodes whose temperature is being set
		Array1D_int CtrlNodes; // nodes where temperature is being set
		Real64 SetPt; // the temperature setpoint [C]

		// Default Constructor
		DefMultiZoneAverageHeatingSetPointManager() :
			CtrlTypeMode( 0 ),
			AirLoopNum( 0 ),
			MinSetTemp( 0.0 ),
			MaxSetTemp( 0.0 ),
			NumCtrlNodes( 0 ),
			SetPt( 0.0 )
		{}

		void calculate();

	};

	struct DefMultiZoneAverageMinHumSetPointManager // derived type for SetpointManager:MultiZone:MinimumHumidity:Average data
	{
		// Members
		std::string Name;
		std::string CtrlVarType; // type of variable to be set
		int CtrlTypeMode; // set to iCtrlVarType_xxxx
		std::string AirLoopName; // name of air loop using MultiZone:MinimumHumidity:Average strategy
		int AirLoopNum; // index of named air loop
		Real64 MinSetHum; // minimum supply air humidity ratio [kg/kg]
		Real64 MaxSetHum; // maximum supply air humidity ratio [kg/kg]
		int NumCtrlNodes; // number of nodes whose humidity ratio is being set
		Array1D_int CtrlNodes; // nodes where humidity ratio is being set
		Real64 SetPt; // the humidity ratio setpoint [kg/kg]

		// Default Constructor
		DefMultiZoneAverageMinHumSetPointManager() :
			CtrlTypeMode( 0 ),
			AirLoopNum( 0 ),
			MinSetHum( 0.0 ),
			MaxSetHum( 0.0 ),
			NumCtrlNodes( 0 ),
			SetPt( 0.0 )
		{}

		void calculate();

	};

	struct DefMultiZoneAverageMaxHumSetPointManager // derived type for SetpointManager:MultiZone:MaximumHumidity:Average data
	{
		// Members
		std::string Name;
		std::string CtrlVarType; // type of variable to be set
		int CtrlTypeMode; // set to iCtrlVarType_xxxx
		std::string AirLoopName; // name of air loop using MultiZone:MaximumHumidity:Average strategy
		int AirLoopNum; // index of named air loop
		Real64 MinSetHum; // minimum supply air humidity ratio [kg/kg]
		Real64 MaxSetHum; // maximum supply air humidity ratio [kg/kg]
		int NumCtrlNodes; // number of nodes whose humidity ratio is being set
		Array1D_int CtrlNodes; // nodes where humidity ratio is being set
		Real64 SetPt; // the humidity ratio setpoint [kg/kg]

		// Default Constructor
		DefMultiZoneAverageMaxHumSetPointManager() :
			CtrlTypeMode( 0 ),
			AirLoopNum( 0 ),
			MinSetHum( 0.0 ),
			MaxSetHum( 0.0 ),
			NumCtrlNodes( 0 ),
			SetPt( 0.0 )
		{}

		void calculate();

	};

	struct DefMultiZoneMinHumSetPointManager // derived type for SetpointManager:MultiZone:Humidity:Minimum data
	{
		// Members
		std::string Name;
		std::string CtrlVarType; // type of variable to be set
		int CtrlTypeMode; // set to iCtrlVarType_xxxx
		std::string AirLoopName; // name of air loop using SetpointManager:MultiZone:Humidity:Minimum
		int AirLoopNum; // index of named air loop
		Real64 MinSetHum; // minimum supply air humidity ratio [kg/kg]
		Real64 MaxSetHum; // maximum supply air humidity ratio [kg/kg]
		int NumCtrlNodes; // number of nodes whose humidity ratio is being set
		Array1D_int CtrlNodes; // nodes where humidity ratio is being set
		Real64 SetPt; // the humidity ratio setpoint [kg/kg]

		// Default Constructor
		DefMultiZoneMinHumSetPointManager() :
			CtrlTypeMode( 0 ),
			AirLoopNum( 0 ),
			MinSetHum( 0.0 ),
			MaxSetHum( 0.0 ),
			NumCtrlNodes( 0 ),
			SetPt( 0.0 )
		{}

		void calculate();

	};

	struct DefMultiZoneMaxHumSetPointManager // derived type for SetpointManager:MultiZone:Humidity:Maximum data
	{
		// Members
		std::string Name;
		std::string CtrlVarType; // type of variable to be set
		int CtrlTypeMode; // set to iCtrlVarType_xxxx
		std::string AirLoopName; // name of air loop using SetpointManager:MultiZone:Humidity:Maximum
		int AirLoopNum; // index of named air loop
		Real64 MinSetHum; // minimum supply air humidity ratio [kg/kg]
		Real64 MaxSetHum; // maximum supply air humidity ratio [kg/kg]
		int NumCtrlNodes; // number of nodes whose humidity ratio is being set
		Array1D_int CtrlNodes; // nodes where humidity ratio is being set
		Real64 SetPt; // the humidity ratio setpoint [kg/kg]

		// Default Constructor
		DefMultiZoneMaxHumSetPointManager() :
			CtrlTypeMode( 0 ),
			AirLoopNum( 0 ),
			MinSetHum( 0.0 ),
			MaxSetHum( 0.0 ),
			NumCtrlNodes( 0 ),
			SetPt( 0.0 )
		{}

		void calculate();

	};

	struct DefineFollowOATempSetPointManager
	{
		// Members
		std::string Name;
		std::string CtrlVarType; // type of variable to be set
		int CtrlTypeMode; // set to iCtrlVarType_xxxx
		std::string RefTempType; // Reference Temperature type (choice OutdoorAirWetBulb/OutdoorAirDryBulb)
		int RefTypeMode; // set to iRefTempType_WetBulb or iRefTempType_DryBulb
		Real64 Offset; // Offset temperature difference
		Real64 MinSetTemp; // Minimum supply air setpoint temperature
		Real64 MaxSetTemp; // Maximum supply air setpoint temperature
		int NumCtrlNodes; // number of nodes whose temperature is being set
		Array1D_int CtrlNodes; // nodes where temperature is being set
		Real64 SetPt; // the setpoint

		// Default Constructor
		DefineFollowOATempSetPointManager() :
			CtrlTypeMode( 0 ),
			RefTypeMode( 0 ),
			Offset( 0.0 ),
			MinSetTemp( 0.0 ),
			MaxSetTemp( 0.0 ),
			NumCtrlNodes( 0 ),
			SetPt( 0.0 )
		{}

		void calculate();

	};

	struct DefineFollowSysNodeTempSetPointManager
	{
		// Members
		std::string Name;
		std::string CtrlVarType; // type of variable to be set
		int CtrlTypeMode; // set to iCtrlVarType_xxxx
		int RefNodeNum; // reference node number
		std::string RefTempType; // Reference Temperature type (choice OutdoorAirWetBulb/OutdoorAirDryBulb)
		int RefTypeMode; // set to iRefTempType_WetBulb or iRefTempType_DryBulb
		Real64 Offset; // Offset temperature difference
		Real64 MinSetTemp; // Minimum supply air setpoint temperature
		Real64 MaxSetTemp; // Maximum supply air setpoint temperature
		int NumCtrlNodes; // number of nodes whose temperature is being set
		Array1D_int CtrlNodes; // nodes where temperature is being set
		Real64 SetPt; // the setpoint

		// Default Constructor
		DefineFollowSysNodeTempSetPointManager() :
			CtrlTypeMode( 0 ),
			RefNodeNum( 0 ),
			RefTypeMode( 0 ),
			Offset( 0.0 ),
			MinSetTemp( 0.0 ),
			MaxSetTemp( 0.0 ),
			NumCtrlNodes( 0 ),
			SetPt( 0.0 )
		{}

		void calculate();

	};

	struct DefineGroundTempSetPointManager
	{
		// Members
		std::string Name;
		std::string CtrlVarType; // type of variable to be set
		int CtrlTypeMode; // set to iCtrlVarType_xxxx
		std::string RefGroundTempObjType; // Reference Temperature type (Available choices are listed below)
		// Site:GroundTemperature:BuildingSurface
		// Site:GroundTemperature:Shallow
		// Site:GroundTemperature:Deep
		// Site:GroundTemperature:FCfactorMethod
		int RefTypeMode; // set to iRefGroundTempObjType_xxxx based on RefGroundTempObjType
		Real64 Offset; // Offset temperature difference
		Real64 MinSetTemp; // Minimum supply air setpoint temperature
		Real64 MaxSetTemp; // Maximum supply air setpoint temperature
		int NumCtrlNodes; // number of nodes whose temperature is being set
		Array1D_int CtrlNodes; // nodes where temperature is being set
		Real64 SetPt; // the setpoint

		// Default Constructor
		DefineGroundTempSetPointManager() :
			CtrlTypeMode( 0 ),
			RefTypeMode( 0 ),
			Offset( 0.0 ),
			MinSetTemp( 0.0 ),
			MaxSetTemp( 0.0 ),
			NumCtrlNodes( 0 ),
			SetPt( 0.0 )
		{}

		void calculate();

	};

	struct DefineCondEntSetPointManager // derived type for SetpointManager:CondenserEnteringReset data
	{
		// Members
		std::string Name;
		std::string CtrlVarType; // type of variable to be set
		int CtrlTypeMode; // set to iCtrlVarType_xxxx
		std::string CondEntTempSched; // Optional schedule
		int CondEntTempSchedPtr; // default condenser entering water temperature schedule Index
		Real64 TowerDsnInletAirWetBulb; // cooling tower design inlet air wetbulb temperature
		int MinTwrWbCurve; // minimum design wetbulb temperature curve name
		int MinOaWbCurve; // minimum outside air wetbulb temperature curve name
		int OptCondEntCurve; // optimized condenser entering water temperature curve name
		Real64 MinimumLiftTD; // minimum lift
		Real64 MaxCondEntTemp; // maximum condenser entering water temp
		int NumCtrlNodes; // number of nodes whose temperature is being set
		std::string CtrlNodeListName;
		Array1D_int CtrlNodes; // nodes where temperature is being set
		Real64 SetPt; // the temperature set point [C]
		int ChillerIndexPlantSide; // plant side chiller index
		int ChillerIndexDemandSide; // demand side chiller index
		int BranchIndexPlantSide; // plant side branch index
		int BranchIndexDemandSide; // demand side branch index
		int LoopIndexPlantSide; // plant side loop index
		int LoopIndexDemandSide; // deand side loop index
		int TypeNum; // chiller type number

		// Default Constructor
		DefineCondEntSetPointManager() :
			CtrlTypeMode( 0 ),
			CondEntTempSchedPtr( 0 ),
			TowerDsnInletAirWetBulb( 0.0 ),
			MinTwrWbCurve( 0 ),
			MinOaWbCurve( 0 ),
			OptCondEntCurve( 0 ),
			MinimumLiftTD( 0.0 ),
			MaxCondEntTemp( 0.0 ),
			NumCtrlNodes( 0 ),
			SetPt( 0.0 ),
			ChillerIndexPlantSide( 0 ),
			ChillerIndexDemandSide( 0 ),
			BranchIndexPlantSide( 0 ),
			BranchIndexDemandSide( 0 ),
			LoopIndexPlantSide( 0 ),
			LoopIndexDemandSide( 0 ),
			TypeNum( 0 )
		{}

		void calculate();

	};

	struct DefineIdealCondEntSetPointManager // derived type for SetpointManager:CondenserEnteringReset:Ideal data
	{
		// Members
		std::string Name;
		std::string CtrlVarType; // type of variable to be set
		int CtrlTypeMode; // set to iCtrlVarType_xxxx
		Real64 MinimumLiftTD; // minimum lift
		Real64 MaxCondEntTemp; // maximum condenser entering water temp
		int NumCtrlNodes; // number of nodes whose temperature is being set
		std::string CtrlNodeListName;
		Array1D_int CtrlNodes; // nodes where temperature is being set
		Real64 SetPt; // the temperature set point [C]
		int ChillerIndexPlantSide; // plant side chiller index
		int BranchIndexPlantSide; // plant side branch index
		int LoopIndexPlantSide; // plant side loop index
		int ChllrVarType; // report variable type
		int ChllrVarIndex; // report variable index
		int ChlPumpVarType; // report variable type
		int ChlPumpVarIndex; // report variable index
		Array1D_int ClTowerVarType; // report variable type
		Array1D_int ClTowerVarIndex; // report variable index
		int CndPumpVarType; // report variable type
		int CndPumpVarIndex; // report variable index
		int TypeNum; // chiller type number
		Array1D_int TowerNum; // cooling tower number
		int CondLoopNum; // condenser loop number
		Array1D_int CondTowerBranchNum; // condenser branch number
		int numTowers; // number of towers to query
		int CondPumpNum; // condenser pump number
		int CondPumpBranchNum; // condenser branch number for pump
		int ChilledPumpNum; // chilled water pump number
		int ChilledPumpBranchNum; // chilled water branch number for pump
		bool SetupIdealCondEntSetPtVars; // flag for initialization of meters and such

		// Default Constructor
		DefineIdealCondEntSetPointManager() :
			CtrlTypeMode( 0 ),
			MinimumLiftTD( 0.0 ),
			MaxCondEntTemp( 0.0 ),
			NumCtrlNodes( 0 ),
			SetPt( 0.0 ),
			ChillerIndexPlantSide( 0 ),
			BranchIndexPlantSide( 0 ),
			LoopIndexPlantSide( 0 ),
			ChllrVarType( 0 ),
			ChllrVarIndex( 0 ),
			ChlPumpVarType( 0 ),
			ChlPumpVarIndex( 0 ),
			CndPumpVarType( 0 ),
			CndPumpVarIndex( 0 ),
			TypeNum( 0 ),
			CondLoopNum( 0 ),
			numTowers( 0 ),
			CondPumpNum( 0 ),
			CondPumpBranchNum( 0 ),
			ChilledPumpNum( 0 ),
			ChilledPumpBranchNum( 0 ),
			SetupIdealCondEntSetPtVars( true )
		{}

		void calculate();

		void SetupMeteredVarsForSetPt();

		Real64 calculateCurrentEnergyUsage();

		void
		setupSetPointAndFlags(
			Real64 & TotEnergy,
			Real64 & TotEnergyPre, 
			Real64 & CondWaterSetPoint, 
			Real64 & CondTempLimit, 
			bool & RunOptCondEntTemp, 
			bool & RunSubOptCondEntTemp,
			bool & RunFinalOptCondEntTemp
		);

	};

	struct DefineSZOneStageCoolinggSetPointManager // Derived type for the Single Zone One Stage Cooling Setpoint Manager data
	{
		// Members
		std::string Name;
		std::string CtrlVarType; // type of variable to be set
		int CtrlTypeMode; // set to iCtrlVarType_xxxx
		std::string ControlZoneName; // name of the control zone (zone with main thermostat)
		int ControlZoneNum; // number (index into Zone array) of control zone
		int ZoneNodeNum; // zone node number
		Real64 CoolingOnTemp; // minimum supply air setpoint temperature
		Real64 CoolingOffTemp; // maximum supply air setpoint temperature
		int NumCtrlNodes;
		Array1D_int CtrlNodes; // node numbers of nodes where setpoint is to be set
		Real64 SetPt; // the setpoint

		// Default Constructor
		DefineSZOneStageCoolinggSetPointManager() :
			CtrlTypeMode( 0 ),
			ControlZoneNum( 0 ),
			ZoneNodeNum( 0 ),
			CoolingOnTemp( 0.0 ),
			CoolingOffTemp( 0.0 ),
			NumCtrlNodes( 0 ),
			SetPt( 0.0 )
		{}

		void calculate();

	};

	struct DefineSZOneStageHeatingSetPointManager // Derived type for the Single Zone One Stage Heating Setpoint Manager data
	{
		// Members
		std::string Name;
		std::string CtrlVarType; // type of variable to be set
		int CtrlTypeMode; // set to iCtrlVarType_xxxx
		std::string ControlZoneName; // name of the control zone (zone with main thermostat)
		int ControlZoneNum; // number (index into Zone array) of control zone
		int ZoneNodeNum; // zone node number
		Real64 HeatingOnTemp; // minimum supply air setpoint temperature
		Real64 HeatingOffTemp; // maximum supply air setpoint temperature
		int NumCtrlNodes;
		Array1D_int CtrlNodes; // node numbers of nodes where setpoint is to be set
		Real64 SetPt; // the setpoint

		// Default Constructor
		DefineSZOneStageHeatingSetPointManager() :
			CtrlTypeMode( 0 ),
			ControlZoneNum( 0 ),
			ZoneNodeNum( 0 ),
			HeatingOnTemp( 0.0 ),
			HeatingOffTemp( 0.0 ),
			NumCtrlNodes( 0 ),
			SetPt( 0.0 )
		{}

		void calculate();

	};

	struct DefineReturnWaterChWSetPointManager // derived type for SetpointManager:SupplyResetForReturnTemperature:ChilledWater data
	{
		// Members
		std::string Name;
		int returnNodeIndex; // node ID for the plant supply-side return node
		int supplyNodeIndex; // node ID for the plant supply-side supply node
		Real64 minimumChilledWaterSetpoint; // the minimum reset temperature for the chilled water setpoint
		Real64 maximumChilledWaterSetpoint; // the maximum reset temperature for the chilled water setpoint
		int returnTemperatureScheduleIndex; // the index in Schedules array for the scheduled return temperature; zero if not used
		Real64 returnTemperatureConstantTarget; // the constant value used as the return temperature target; used if schedule index is zero
		Real64 currentSupplySetPt; // the current supply setpoint temperature
		int plantLoopIndex; // the index for the plant loop for this manager, zero if not initialized
		int plantSetpointNodeIndex; // the index for the node where the plant setpoint is set, need to look up after Plant is established
		bool useReturnTempSetpoint; // only true if the target return temperature should be looked up as the Node(returnNode).TempSetPoint

		// Default Constructor
		DefineReturnWaterChWSetPointManager() :
			Name( "" ),
			returnNodeIndex( 0 ),
			supplyNodeIndex( 0 ),
			minimumChilledWaterSetpoint( 0.0 ),
			maximumChilledWaterSetpoint( 0.0 ),
			returnTemperatureScheduleIndex( 0 ),
			returnTemperatureConstantTarget( 0.0 ),
			currentSupplySetPt( 0.0 ),
			plantLoopIndex( 0 ),
			plantSetpointNodeIndex( 0 ),
			useReturnTempSetpoint( false )
		{}

		// Calculation method
		void calculate( DataLoopNode::NodeData & returnNode, DataLoopNode::NodeData & supplyNode );

	};

	struct DefineReturnWaterHWSetPointManager // derived type for SetpointManager:SupplyResetForReturnTemperature:HotWater data
	{
		// Members
		std::string Name;
		int returnNodeIndex; // node ID for the plant supply-side return node
		int supplyNodeIndex; // node ID for the plant supply-side supply node
		Real64 maximumHotWaterSetpoint; // the maximum reset temperature for the hot water setpoint
		Real64 minimumHotWaterSetpoint; // the minimum reset temperature for the hot water setpoint
		int returnTemperatureScheduleIndex; // the index in Schedules array for the scheduled return temperature; zero if not used
		Real64 returnTemperatureConstantTarget; // the constant value used as the return temperature target; used if schedule index is zero
		Real64 currentSupplySetPt; // the current supply setpoint temperature
		int plantLoopIndex; // the index for the plant loop for this manager, zero if not initialized
		int plantSetpointNodeIndex; // the index for the node where the plant setpoint is set, need to look up after Plant is established
		bool useReturnTempSetpoint; // only true if the target return temperature should be looked up as the Node(returnNode).TempSetPoint

		// Default Constructor
		DefineReturnWaterHWSetPointManager() :
			Name( "" ),
			returnNodeIndex( 0 ),
			supplyNodeIndex( 0 ),
			maximumHotWaterSetpoint( 0.0 ),
			minimumHotWaterSetpoint( 0.0 ),
			returnTemperatureScheduleIndex( 0 ),
			returnTemperatureConstantTarget( 0.0 ),
			currentSupplySetPt( 0.0 ),
			plantLoopIndex( 0 ),
			plantSetpointNodeIndex( 0 ),
			useReturnTempSetpoint( false )
		{}

		// Calculation method
		void calculate( DataLoopNode::NodeData & returnNode, DataLoopNode::NodeData & supplyNode );

	};

	struct DefineScheduledTESSetPointManager // Derived type for Scheduled TES Setpoint Manager data
	{
		// Members
		int SchedPtr;
		int SchedPtrCharge;
		int CtrlNodeNum;
		Real64 NonChargeCHWTemp;
		Real64 ChargeCHWTemp;
		int CompOpType;
		Real64 SetPt;
		
		// Default Constructor
		DefineScheduledTESSetPointManager() :
		SchedPtr( 0 ),
		SchedPtrCharge( 0 ),
		CtrlNodeNum( 0 ),
		NonChargeCHWTemp( 0.0 ),
		ChargeCHWTemp( 0.0 ),
		CompOpType( 0 ),
		SetPt( 0.0 )
		{}

		// Calculation method
		void calculate();

	};

	// Object Data
	extern Array1D< DataSetPointManager > AllSetPtMgr; // Array for all Setpoint Manager data(warnings)
	extern Array1D< DefineScheduledSetPointManager > SchSetPtMgr; // Array for Scheduled Setpoint Manager data
	extern Array1D< DefineSchedDualSetPointManager > DualSchSetPtMgr; // Dual Scheduled Setpoint Manager data
	extern Array1D< DefineOutsideAirSetPointManager > OutAirSetPtMgr; // Array for Outside Air Setpoint Manager data
	extern Array1D< DefineSZReheatSetPointManager > SingZoneRhSetPtMgr; // Array for SZRH Set Pt Mgr
	extern Array1D< DefineSZHeatingSetPointManager > SingZoneHtSetPtMgr; // Array for SZ Heating Set Pt Mgr
	extern Array1D< DefineSZCoolingSetPointManager > SingZoneClSetPtMgr; // Array for SZ Cooling Set Pt Mgr
	extern Array1D< DefineSZMinHumSetPointManager > SZMinHumSetPtMgr; // Array for SZ Min Hum Set Pt Mgr
	extern Array1D< DefineSZMaxHumSetPointManager > SZMaxHumSetPtMgr; // Array for SZ Max Hum Set Pt Mgr
	extern Array1D< DefineMixedAirSetPointManager > MixedAirSetPtMgr; // Array for Mixed Air Set Pt Mgr
	extern Array1D< DefineOAPretreatSetPointManager > OAPretreatSetPtMgr; // Array for OA Pretreat Set Pt Mgr
	extern Array1D< DefineWarmestSetPointManager > WarmestSetPtMgr; // Array for Warmest Set Pt Mgr
	extern Array1D< DefineColdestSetPointManager > ColdestSetPtMgr; // Array for Coldest Set Pt Mgr
	extern Array1D< DefWarmestSetPtManagerTempFlow > WarmestSetPtMgrTempFlow; // Array for Warmest Set Pt Mgr
	extern Array1D< DefRABFlowSetPointManager > RABFlowSetPtMgr; // Array for return air bypass
	extern Array1D< DefMultiZoneAverageCoolingSetPointManager > MZAverageCoolingSetPtMgr; // Array for MultiZone
	extern Array1D< DefMultiZoneAverageHeatingSetPointManager > MZAverageHeatingSetPtMgr; // Array for MultiZone
	extern Array1D< DefMultiZoneAverageMinHumSetPointManager > MZAverageMinHumSetPtMgr; // Array for MultiZone
	extern Array1D< DefMultiZoneAverageMaxHumSetPointManager > MZAverageMaxHumSetPtMgr; // Array for MultiZone
	extern Array1D< DefMultiZoneMinHumSetPointManager > MZMinHumSetPtMgr; // Multizone min humidity rat Set Pt Mgr
	extern Array1D< DefMultiZoneMaxHumSetPointManager > MZMaxHumSetPtMgr; // Multizone max humidity rat Set Pt Mgr
	extern Array1D< DefineFollowOATempSetPointManager > FollowOATempSetPtMgr; // Array for Follow Outdoor Air
	extern Array1D< DefineFollowSysNodeTempSetPointManager > FollowSysNodeTempSetPtMgr; // Array for Follow System
	extern Array1D< DefineGroundTempSetPointManager > GroundTempSetPtMgr; // Array for Ground Temp Setpoint
	extern Array1D< DefineCondEntSetPointManager > CondEntSetPtMgr; // Condenser Entering Water Set Pt Mgr
	extern Array1D< DefineIdealCondEntSetPointManager > IdealCondEntSetPtMgr; // Ideal Condenser Entering Set Pt Mgr
	extern Array1D< DefineSZOneStageCoolinggSetPointManager > SZOneStageCoolingSetPtMgr; // single zone 1 stage cool
	extern Array1D< DefineSZOneStageHeatingSetPointManager > SZOneStageHeatingSetPtMgr; // single zone 1 stage heat
	extern Array1D< DefineReturnWaterChWSetPointManager > ReturnWaterResetChWSetPtMgr; // return water reset
	extern Array1D< DefineReturnWaterHWSetPointManager > ReturnWaterResetHWSetPtMgr; // hot-water return reset
	extern Array1D< DefineScheduledTESSetPointManager > SchTESSetPtMgr; // Array for Scheduled Setpoint Manager data

	// Functions

	void
	clear_state();

	void
	ManageSetPoints();

	void
	GetSetPointManagerInputs();

	void
	VerifySetPointManagers( bool & ErrorsFound ); // flag to denote node conflicts in input. !unused1208

	void
	InitSetPointManagers();

	void
	SimSetPointManagers();

	void
	UpdateSetPointManagers();

	void
	UpdateMixedAirSetPoints();

	void
	UpdateOAPretreatSetPoints();

	bool
	IsNodeOnSetPtManager(
		int const NodeNum,
		int const SetPtType
	);

	bool
	NodeHasSPMCtrlVarType(
		int const NodeNum,
		int const iCtrlVarType
	);

	void
	ResetHumidityRatioCtrlVarType(
		int const NodeNum
		);

	void
	CheckIfAnyIdealCondEntSetPoint();

	int
	GetHumidityRatioVariableType( int const CntrlNodeNum );

	void
	SetUpNewScheduledTESSetPtMgr(
								 int const SchedPtr,
								 int const SchedPtrCharge,
								 Real64 NonChargeCHWTemp,
								 Real64 ChargeCHWTemp,
								 int const CompOpType,
								 int const ControlNodeNum
								 );

	bool
	GetCoilFreezingCheckFlag( int const MixedAirSPMNum );

	int
	GetMixedAirNumWithCoilFreezingCheck( int const MixedAirNode );
	

} // SetPointManager

} // EnergyPlus

#endif
