#ifndef SwimmingPool_hh_INCLUDED
#define SwimmingPool_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace SwimmingPool {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	//na

	// MODULE VARIABLE DECLARATIONS:
	// Standard, run-of-the-mill variables...
	extern int NumSwimmingPools; // Number of swimming pools
	extern Array1D< int > SurfaceToPoolIndex; // Average source over the time step for a particular radiant surface
	extern Array1D< Real64 > QPoolSrcAvg; // Average source over the time step for a particular pool
	extern Array1D< Real64 > HeatTransCoefsAvg; // Average denominator term over the time step for a particular pool
	extern Array1D< Real64 > ZeroSourceSumHATsurf; // Equal to SumHATsurf for all the walls in a zone with no source
	// Record keeping variables used to calculate QPoolSrcAvg locally
	extern Array1D< Real64 > LastQPoolSrc; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > LastHeatTransCoefs; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > LastSysTimeElapsed; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > LastTimeStepSys; // Need to keep the last value in case we are still iterating

	// Types

	struct SwimmingPoolData
	{
		// Members
		// Input data
		std::string Name; // name of swimming pool
		std::string SurfaceName; // surface name of pool
		int SurfacePtr; // index to surface array
		std::string ZoneName; // Name of zone the pool is in
		int ZonePtr; // Pointer to this zone in the Zone derived type
		std::string WaterInletNodeName; // water inlet node name
		int WaterInletNode; // water inlet node number
		std::string WaterOutletNodeName; // water outlet node name
		int WaterOutletNode; // water outlet node number
		int HWLoopNum;
		int HWLoopSide;
		int HWBranchNum;
		int HWCompNum;
		Real64 WaterVolFlowMax; // maximum water flow rate for pool, m3/s
		Real64 WaterMassFlowRateMax; // maximum water mass flow rate for pool, kg/s
		Real64 AvgDepth; // average depth of the pool, m
		Real64 ActivityFactor; // Activity factor for the pool
		std::string ActivityFactorSchedName; // Activity factor schedule name
		int ActivityFactorSchedPtr; // Activity factor schedule pointer
		Real64 CurActivityFactor; // Current activity factor value
		std::string MakeupWaterSupplyName; // Name of make-up water source
		std::string MakeupWaterSupplySchedName; //Name of make-up water supply schedule
		int MakeupWaterSupplySchedPtr; // Index to schedule for make-up water
		Real64 CurMakeupWaterTemp; // Current makeup water temperature
		std::string CoverSchedName; // Pool cover schedule name
		int CoverSchedPtr; // Index to pool cover schedule
		Real64 CurCoverSchedVal; // Current cover schedule value based on schedule
		Real64 CoverEvapFactor; // Pool cover evaporation factor
		Real64 CoverConvFactor; // Pool cover convective factor
		Real64 CoverSWRadFactor; // Pool cover short-wavelength radiation factor
		Real64 CoverLWRadFactor; // Pool cover long-wavelength radiation factor
		Real64 CurCoverEvapFac; // Current pool cover evaporation factor
		Real64 CurCoverConvFac; // Current pool cover convective factor
		Real64 CurCoverSWRadFac; // Current pool cover short-wavelength radiation factor
		Real64 CurCoverLWRadFac; // Current pool cover long-wavelength radiation factor
		Real64 RadConvertToConvect; // LW and SW radiation converted to convective gain by pool cover in W/m2
		Real64 MiscPowerFactor; // Pool miscellaneous power equipment consumption coefficient in W/(kg/s)
		std::string SetPtTempSchedName; // Schedule name for water setpoint temperature
		int SetPtTempSchedPtr; // Schedule pointer for water setpoint temperature
		Real64 CurSetPtTemp; // Current water setpoint temperature
		Real64 MaxNumOfPeople; // Number of people in the pool as defined by user input
		std::string PeopleSchedName; // Name of people schedule
		int PeopleSchedPtr; // People schedule index
		std::string PeopleHeatGainSchedName; // Name of people heat gain schedule
		int PeopleHeatGainSchedPtr; // People heat gain schedule index
		Real64 PeopleHeatGain; // Current heat gain from people
		int GlycolIndex; // index in fluid property routines for water
		Real64 WaterMass; // pool water mass
		// Report data
		Real64 PoolWaterTemp; // Average pool water temperature
		Real64 WaterInletTemp; // water inlet temperature
		Real64 WaterOutletTemp; // water outlet temperature
		Real64 WaterMassFlowRate; // water mass flow rate
		Real64 MakeUpWaterMassFlowRate; // makeup water flow rate (addition to the pool)
		Real64 MakeUpWaterMass; // makeup water mass added to pool
		Real64 MakeUpWaterVolFlowRate; // makeup water volume flow rate
		Real64 MakeUpWaterVol; // makeup water volume added to pool
		Real64 HeatPower; // heating sent to pool in Watts
		Real64 HeatEnergy; // heating sent to pool in Joules
		Real64 MiscEquipPower; // power for miscellaneous pool equipment in Watts
		Real64 MiscEquipEnergy; // energy for miscellaneous pool equipment in Joules
		Real64 RadConvertToConvectRep; //LW and SW radiation converted to convective gain by pool cover (reporting) in W
		Real64 EvapHeatLossRate; // Heat lost due to evaporation of pool water as a rate in Watts
		Real64 EvapEnergyLoss; // Energy lost due to evaporation in Joules

		// Default Constructor
		SwimmingPoolData() :
			SurfacePtr( 0 ),
			ZonePtr( 0 ),
			WaterInletNode( 0 ),
			WaterOutletNode( 0 ),
			HWLoopNum( 0 ),
			HWLoopSide( 0 ),
			HWBranchNum( 0 ),
			HWCompNum( 0 ),
			WaterVolFlowMax( 0.0 ),
			WaterMassFlowRateMax( 0.0 ),
			AvgDepth( 0.0 ),
			ActivityFactor( 0.0 ),
			ActivityFactorSchedPtr( 0 ),
			CurActivityFactor( 0.0 ),
			MakeupWaterSupplySchedPtr( 0 ),
			CurMakeupWaterTemp( 0.0 ),
			CoverSchedPtr( 0 ),
			CurCoverSchedVal( 0.0 ),
			CoverEvapFactor( 0.0 ),
			CoverConvFactor( 0.0 ),
			CoverSWRadFactor( 0.0 ),
			CoverLWRadFactor( 0.0 ),
			CurCoverEvapFac( 0.0 ),
			CurCoverConvFac( 0.0 ),
			CurCoverSWRadFac( 0.0 ),
			CurCoverLWRadFac( 0.0 ),
			RadConvertToConvect( 0.0 ),
			MiscPowerFactor( 0.0 ),
			SetPtTempSchedPtr( 0 ),
			CurSetPtTemp( 23.0 ),
			MaxNumOfPeople( 0.0 ),
			PeopleSchedPtr( 0 ),
			PeopleHeatGainSchedPtr( 0 ),
			PeopleHeatGain( 0.0 ),
			GlycolIndex( 0 ),
			WaterMass( 0.0 ),
			PoolWaterTemp( 23.0 ),
			WaterInletTemp( 0.0 ),
			WaterOutletTemp( 0.0 ),
			WaterMassFlowRate( 0.0 ),
			MakeUpWaterMassFlowRate( 0.0 ),
			MakeUpWaterMass( 0.0 ),
			MakeUpWaterVolFlowRate( 0.0 ),
			MakeUpWaterVol( 0.0 ),
			HeatPower( 0.0 ),
			HeatEnergy( 0.0 ),
			MiscEquipPower( 0.0 ),
			MiscEquipEnergy( 0.0 ),
			RadConvertToConvectRep( 0.0 ),
			EvapHeatLossRate( 0.0 ),
			EvapEnergyLoss( 0.0 )
		{}

		// Member Constructor
		SwimmingPoolData(
			std::string const & Name, // name of swimming pool
			std::string const & SurfaceName, // surface name of pool
			int const SurfacePtr, // index to surface array
			std::string const & ZoneName, // Name of zone the pool is in
			int const ZonePtr, // Pointer to this zone in the Zone derived type
			std::string WaterInletNodeName, // water inlet node name
			int const WaterInletNode, // water inlet node number
			std::string WaterOutletNodeName, // water outlet node name
			int const WaterOutletNode, // water outlet node number
			int const HWLoopNum,
			int const EP_UNUSED( HWLoopSide ),
			int const HWBranchNum,
			int const HWCompNum,
			Real64 const WaterVolFlowMax, // maximum water flow rate for pool, m3/s
			Real64 const WaterMassFlowRateMax, // maximum water mass flow rate for pool, kg/s
			Real64 const AvgDepth, // average depth of the pool, m
			Real64 const ActivityFactor, // Activity factor for the pool
			std::string const & ActivityFactorSchedName, // Activity factor schedule name
			int const ActivityFactorSchedPtr, // Activity factor schedule pointer
			Real64 const CurActivityFactor, // Current activity factor
			std::string const & MakeupWaterSupplyName, // Name of make-up water source
			std::string const & MakeupWaterSupplySchedName, // Name of make-up water supply schedule
			int const MakeupWaterSupplySchedPtr, // Index to schedule for make-up water
			Real64 const CurMakeupWaterTemp, // Current makeup water temperature
			std::string const & CoverSchedName, // Pool cover schedule name
			int const CoverSchedPtr, // Index to pool cover schedule
			Real64 const CurCoverSchedVal, // Current cover schedule value
			Real64 const CoverEvapFactor, // Pool cover evaporation factor
			Real64 const CoverConvFactor, // Pool cover convective factor
			Real64 const CoverSWRadFactor, // Pool cover short-wavelength radiation factor
			Real64 const CoverLWRadFactor, // Pool cover long-wavelength radiation factor
			Real64 const CurCoverEvapFac, // Current pool cover evaporation factor
			Real64 const CurCoverConvFac, // Current pool cover convective factor
			Real64 const CurCoverSWRadFac, // Current pool cover short-wavelength radiation factor
			Real64 const CurCoverLWRadFac, // Current pool cover long-wavelength radiation factor
			Real64 const RadConvertToConvect, // LW and SW radiation converted to convective gain by pool cover
			Real64 const MiscPowerFactor, // Pool miscellaneous power equipment consumption coefficient in W/(kg/s)
			std::string const & SetPtTempSchedName, // Schedule name for water setpoint temperature
			int const SetPtTempSchedPtr, // Schedule pointer for water setpoint temperature
			Real64 const CurSetPtTemp, // Current water setpoint temperature
			Real64 const MaxNumOfPeople, // Number of people in the pool as defined by user input
			std::string const & PeopleSchedName, // Name of people schedule
			int const PeopleSchedPtr, // People schedule index
			std::string const & PeopleHeatGainSchedName, // Name of people heat gain schedule
			int const PeopleHeatGainSchedPtr, // People heat gain schedule index
			Real64 const PeopleHeatGain, // Current people heat gain for the pool
			int const GlycolIndex, // index in fluid property routines for water
			Real64 const WaterMass, // pool water mass
			// Report data
			Real64 const PoolWaterTemp, // Average pool water temperature
			Real64 const WaterInletTemp, // water inlet temperature
			Real64 const WaterOutletTemp, // water outlet temperature
			Real64 const WaterMassFlowRate, // water mass flow rate from loop
			Real64 const MakeUpWaterMassFlowRate, // makeup water flow rate (addition to the pool)
			Real64 const MakeUpWaterMass, // makeup water mass added to pool
			Real64 const MakeUpWaterVolFlowRate, //makeup water volume flow rate
			Real64 const MakeUpWaterVol, //makeup water volume added to pool
			Real64 const HeatPower, // heating sent to pool in Watts
			Real64 const HeatEnergy, // heating sent to pool in Joules
			Real64 const MiscEquipPower, // power for miscellaneous pool equipment in Watts
			Real64 const MiscEquipEnergy, // energy for miscellaneous pool equipment in Joules
			Real64 const RadConvertToConvectRep, //LW and SW radiation converted to convective gain by pool cover (reporting) in W
			Real64 const EvapHeatLossRate, // Heat lost due to evaporation of pool water
			Real64 const EvapEnergyLoss // Energy lost due to evaporation in Joules
		) :
			Name( Name ),
			SurfaceName( SurfaceName ),
			SurfacePtr( SurfacePtr ),
			ZoneName( ZoneName ),
			ZonePtr( ZonePtr ),
			WaterInletNodeName( WaterInletNodeName ),
			WaterInletNode( WaterInletNode ),
			WaterOutletNodeName( WaterOutletNodeName ),
			WaterOutletNode( WaterOutletNode ),
			HWLoopNum( HWLoopNum ),
			HWBranchNum( HWBranchNum ),
			HWCompNum( HWCompNum ),
			WaterVolFlowMax( WaterVolFlowMax ),
			WaterMassFlowRateMax( WaterMassFlowRateMax ),
			AvgDepth( AvgDepth ),
			ActivityFactor( ActivityFactor ),
			ActivityFactorSchedName( ActivityFactorSchedName ),
			ActivityFactorSchedPtr( ActivityFactorSchedPtr ),
			CurActivityFactor( CurActivityFactor ),
			MakeupWaterSupplyName( MakeupWaterSupplyName ),
			MakeupWaterSupplySchedName( MakeupWaterSupplySchedName ),
			MakeupWaterSupplySchedPtr( MakeupWaterSupplySchedPtr ),
			CurMakeupWaterTemp( CurMakeupWaterTemp ),
			CoverSchedName( CoverSchedName ),
			CoverSchedPtr( CoverSchedPtr ),
			CurCoverSchedVal( CurCoverSchedVal ),
			CoverEvapFactor( CoverEvapFactor ),
			CoverConvFactor( CoverConvFactor ),
			CoverSWRadFactor( CoverSWRadFactor ),
			CoverLWRadFactor( CoverLWRadFactor ),
			CurCoverEvapFac( CurCoverEvapFac ),
			CurCoverConvFac( CurCoverConvFac ),
			CurCoverSWRadFac( CurCoverSWRadFac ),
			CurCoverLWRadFac( CurCoverLWRadFac ),
			RadConvertToConvect( RadConvertToConvect ),
			MiscPowerFactor( MiscPowerFactor ),
			SetPtTempSchedName( SetPtTempSchedName ),
			SetPtTempSchedPtr( SetPtTempSchedPtr ),
			CurSetPtTemp( CurSetPtTemp ),
			MaxNumOfPeople( MaxNumOfPeople ),
			PeopleSchedName( PeopleSchedName ),
			PeopleSchedPtr( PeopleSchedPtr ),
			PeopleHeatGainSchedName( PeopleHeatGainSchedName ),
			PeopleHeatGainSchedPtr( PeopleHeatGainSchedPtr ),
			PeopleHeatGain( PeopleHeatGain ),
			GlycolIndex( GlycolIndex ),
			WaterMass( WaterMass ),
			PoolWaterTemp( PoolWaterTemp ),
			WaterInletTemp( WaterInletTemp ),
			WaterOutletTemp( WaterOutletTemp ),
			WaterMassFlowRate( WaterMassFlowRate ),
			MakeUpWaterMassFlowRate( MakeUpWaterMassFlowRate ),
			MakeUpWaterMass( MakeUpWaterMass ),
			MakeUpWaterVolFlowRate( MakeUpWaterVolFlowRate ),
			MakeUpWaterVol( MakeUpWaterVol ),
			HeatPower( HeatPower ),
			HeatEnergy( HeatEnergy ),
			MiscEquipPower( MiscEquipPower ),
			MiscEquipEnergy( MiscEquipEnergy ),
			RadConvertToConvectRep( RadConvertToConvectRep ),
			EvapHeatLossRate( EvapHeatLossRate ),
			EvapEnergyLoss( EvapEnergyLoss )
		{}
	};

	// Object Data
	extern Array1D< SwimmingPoolData > Pool;

	// Functions

	void
	SimSwimmingPool(
		bool const FirstHVACIteration
	);

	void
	GetSwimmingPool();

	void
	InitSwimmingPool(
		bool const FirstHVACIteration, // true during the first HVAC iteration
		int const PoolNum // Index of the swimming pool under consideration within the derived types
	);

	void
	CalcSwimmingPool(
		int const PoolNum // Index of the swimming pool under consideration within the derived types
	);

	void
	UpdateSwimmingPool(
		int const PoolNum // Index of the swimming pool under consideration within the derived types
	);

	void
	UpdatePoolSourceValAvg( bool & SwimmingPoolOn ); // .TRUE. if the swimming pool has "run" this zone time step

	Real64
	SumHATsurf( int const ZoneNum ); // Zone number

	void
	ReportSwimmingPool();

	Real64
	MakeUpWaterVolFlowFunct( Real64 MakeUpWaterMassFlowRate, Real64 Density );
	
	Real64
	MakeUpWaterVolFunct( Real64 MakeUpWaterMass, Real64 Density );

	//     NOTICE

	//     Copyright (c) 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in EnergyPlus.cc.

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

} // SwimmingPool

} // EnergyPlus

#endif
