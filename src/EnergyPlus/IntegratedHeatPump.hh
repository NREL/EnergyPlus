#ifndef IntegratedHeatPumps_hh_INCLUDED
#define IntegratedHeatPumps_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <VariableSpeedCoils.hh>
#include <DataSizing.hh>
#include <DataGlobals.hh>
#include <DataHVACGlobals.hh>

namespace EnergyPlus {

namespace IntegratedHeatPumps {

	// Using/Aliasing
	// Using/Aliasing
	using VariableSpeedCoils::MaxSpedLevels;

	// Data
	//MODULE PARAMETER DEFINITIONS

	// Identifier is VarSpeedCoil
	extern int NumIHPs; //counter for all integrated heat pumps including air-source and water-source
	extern bool GetCoilsInputFlag; // Flag set to make sure you get input once

	// operation mode
	int const IdleMode(0);
	int const SCMode(1);
	int const SHMode(2);
	int const DWHMode(3);
	int const SCWHMatchSCMode(4);
	int const SCWHMatchWHMode(5);
	int const SCDWHMode(6);
	int const SHDWHElecHeatOffMode(7);
	int const SHDWHElecHeatOnMode(8);

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Update routines to check convergence and update nodes

	// Update routine

	// Utility routines
	//SHR, bypass factor routines

	// Types

	struct IntegratedHeatPumpData // variable speed coil
	{
		// Members
		std::string Name; // Name of the  Coil
		std::string IHPtype; // type of coil

		std::string SCCoilType; // Numeric Equivalent for SC Coil Type
		std::string SCCoilName;
		int SCCoilIndex; // Index to SC coil

		std::string SHCoilType; // Numeric Equivalent for SH Coil Type
		std::string SHCoilName;
		int SHCoilIndex; // Index to SH coil

		std::string SCWHCoilType; // Numeric Equivalent for SCWH Coil Type
		std::string SCWHCoilName; 
		int SCWHCoilIndex; // Index to SCWH coil

		std::string DWHCoilType; // Numeric Equivalent for DWH Coil Type
		std::string DWHCoilName;
		int DWHCoilIndex; // Index to DWH coil

		std::string SCDWHCoolCoilType; // Numeric Equivalent for SCDWH Coil Type, cooling part
		std::string SCDWHCoolCoilName;
		int SCDWHCoolCoilIndex; // Index to SCDWH coil, cooling part

		std::string SCDWHWHCoilType; // Numeric Equivalent for SCDWH Coil Type, water heating part
		std::string SCDWHWHCoilName;
		int SCDWHWHCoilIndex; // Index to SCDWH coil, water heating part

		std::string SHDWHHeatCoilType; // Numeric Equivalent for SHDWH Coil Type, heating part
		std::string SHDWHHeatCoilName;
		int SHDWHHeatCoilIndex; // Index to SHDWH coil, heating part

		std::string SHDWHWHCoilType; // Numeric Equivalent for SHDWH Coil Type, water heating part
		std::string SHDWHWHCoilName;
		int SHDWHWHCoilIndex; // Index to SHDWH coil, water heating part

		int AirCoolInletNodeNum; // Node Number of the Air cooling coil Inlet
		int AirHeatInletNodeNum; // Node Number of the Air cooling coil Inlet
		int AirOutletNodeNum; // Node Number of the Air Outlet
		int WaterInletNodeNum; // Node Number of the Water Onlet
		int WaterOutletNodeNum; // Node Number of the Water Outlet
		int WaterTankoutNod; // water node to monitor the supply water flow amount

		int ModeMatchSCWH;//- 0: match cooling load, 1 : match water heating load in SCWH mode
		int MinSpedSCWH; //-minimum speed level for SCWH mode
		int MinSpedSCDWH; //- minimum speed level for SCDWH mode
		int MinSpedSHDWH;//- minimum speed level for SHDWH mode
		Real64 TindoorOverCoolAllow;  //- [C], indoor temperature above which indoor overcooling is allowed
		Real64 TambientOverCoolAllow; //- [C], ambient temperature above which indoor overcooling is allowed
		Real64 TindoorWHHighPriority;  //- [C], indoor temperature above which water heating has the higher priority
		Real64 TambientWHHighPriority; //ambient temperature above which water heating has the higher priority

		Real64 WaterVolSCDWH;// limit of water volume before switching from SCDWH to SCWH
		Real64 TimeLimitSHDWH; //time limit before turning from SHDWH to electric heating 

		int WHtankType;
		std::string WHtankName; 
		int WHtankID; 
		bool IsWHCallAvail;//whether water heating call available
		bool CheckWHCall; 
		int CurMode; //current working mode
		Real64 ControlledZoneTemp; 
		Real64 WaterFlowAccumVol;// water flow accumulated volume
		Real64 SHDWHRunTime; 
		bool NodeConnected; 
		Real64 TotalHeatingEnergyRate; 
		Real64 CoolVolFlowScale;// max fan cooling volumetric flow rate
		Real64 HeatVolFlowScale;// max fan heating volumetric flow rate
		Real64 MaxHeatAirMassFlow;//maximum air mass flow rate for heating mode
		Real64 MaxHeatAirVolFlow;//maximum air volume flow rate for heating mode
		Real64 MaxCoolAirMassFlow;//maximum air mass flow rate for heating mode
		Real64 MaxCoolAirVolFlow;//maximum air volume flow rate for heating mode
		bool IHPCoilsSized;//whether IHP coils have been sized

		std::string IDFanName;//IHP indoor fan name
		int IDFanID;//IHP indoor fan index
		int IDFanPlace;//indoor fan placement
		
		int ODAirInletNodeNum; // oudoor coil inlet Nod
		int ODAirOutletNodeNum; // oudoor coil outlet Nod

		// Default Constructor
		IntegratedHeatPumpData() :
			SCCoilIndex(0),
			SHCoilIndex(0),
			SCWHCoilIndex(0),
			DWHCoilIndex(0),
			SCDWHCoolCoilIndex(0),
			SCDWHWHCoilIndex(0),
			SHDWHHeatCoilIndex(0),
			SHDWHWHCoilIndex(0),
			AirCoolInletNodeNum(0),
			AirHeatInletNodeNum(0),
			AirOutletNodeNum(0),
			WaterInletNodeNum(0),
			WaterOutletNodeNum(0),
			WaterTankoutNod(0),
			ODAirInletNodeNum(0), // oudoor coil inlet Nod
			ODAirOutletNodeNum(0), // oudoor coil outlet Nod
			ModeMatchSCWH(0),
			MinSpedSCWH(1),
			MinSpedSCDWH(1),
			MinSpedSHDWH(1),
			TindoorOverCoolAllow(0.0),
			TambientOverCoolAllow(0.0),
			TindoorWHHighPriority(0.0),
			TambientWHHighPriority(0.0),
			WaterVolSCDWH(0.0),
			TimeLimitSHDWH(0.0),
			WHtankType(0),
			WHtankID(0),
			IsWHCallAvail(false),
			CheckWHCall(false),
			CurMode(0),
			ControlledZoneTemp(0),
			WaterFlowAccumVol(0),
			SHDWHRunTime(0),
			NodeConnected(false),
			TotalHeatingEnergyRate(0),
			CoolVolFlowScale(0),
			HeatVolFlowScale(0),
			MaxHeatAirMassFlow(0),
			MaxHeatAirVolFlow(0),
			MaxCoolAirMassFlow(0),
			MaxCoolAirVolFlow(0),
			IHPCoilsSized(false),
			IDFanID(0),
			IDFanPlace(0)
		{}

	};

	// Object Data
	extern Array1D< IntegratedHeatPumpData > IntegratedHeatPumpUnits;

	// Functions
	void
		clear_state();

	void
	SimIHP(
		std::string const & CompName, // Coil Name
		int & CompIndex, // Index for Component name
		int const CyclingScheme, // Continuous fan OR cycling compressor
		Real64 & MaxONOFFCyclesperHour, // Maximum cycling rate of heat pump [cycles/hr]
		Real64 & HPTimeConstant, // Heat pump time constant [s]
		Real64 & FanDelayTime, // Fan delay time, time delay for the HP's fan to
		int const CompOp, // compressor on/off. 0 = off; 1= on
		Real64 const PartLoadFrac,
		int const SpeedNum, // compressor speed number
		Real64 const SpeedRatio, // compressor speed ratio
		Real64 const SensLoad, // Sensible demand load [W]
		Real64 const LatentLoad, // Latent demand load [W]
		bool const IsCallbyWH, //whether the call from the water heating loop or air loop, true = from water heating loop
		bool const FirstHVACIteration, // TRUE if First iteration of simulation
		Optional< Real64 const > OnOffAirFlowRat = _ // ratio of comp on to comp off air flow rate
	);

	void
	GetIHPInput();


	void
	SizeIHP( int const CoilNum );
	
	void
	InitializeIHP( int const DXCoilNum); 

	void
	UpdateIHP( int const DXCoilNum );

	void
		DecideWorkMode(int const DXCoilNum,
		Real64 const SensLoad, // Sensible demand load [W]
		Real64 const LatentLoad // Latent demand load [W]
		);

	int 
		GetCurWorkMode(int const DXCoilNum);

	int
		GetLowSpeedNumIHP(int const DXCoilNum);
	int
		GetMaxSpeedNumIHP(int const DXCoilNum);

	Real64
		GetAirVolFlowRateIHP(int const DXCoilNum, int const SpeedNum, Real64 const SpeedRatio, 
		bool const IsCallbyWH //whether the call from the water heating loop or air loop, true = from water heating loop
		);

	Real64
		GetWaterVolFlowRateIHP(int const DXCoilNum, int const SpeedNum, Real64 const SpeedRatio,
		bool const IsCallbyWH //whether the call from the water heating loop or air loop, true = from water heating loop
		);

	Real64
		GetAirMassFlowRateIHP(int const DXCoilNum, int const SpeedNum, Real64 const SpeedRatio,
		bool const IsCallbyWH //whether the call from the water heating loop or air loop, true = from water heating loop
		);

	int
		GetCoilIndexIHP(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
		);

	int
		GetCoilInletNodeIHP(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
		);

	Real64
		GetDWHCoilCapacityIHP(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		int const Mode,//mode coil type
		bool & ErrorsFound // set to true if problem
		);

	int
		GetIHPDWHCoilPLFFPLR(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		int const Mode,//mode coil type
		bool & ErrorsFound // set to true if problem
		);

	void ClearCoils
		(int const DXCoilNum // coil ID
		);

	void
		ConnectIHP(
		int const WhichCoil // must match coil names for the coil type
		);

	void
		DisconnectIHP(
		int const WhichCoil // must match coil names for the coil type
		);

	
	//     NOTICE

	//     Copyright © 1996-2016 The Board of Trustees of the University of Illinois
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

} // IntegratedHeatPumps

} // EnergyPlus

#endif
