#ifndef SwimmingPool_hh_INCLUDED
#define SwimmingPool_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>

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
	extern FArray1D< int > SurfaceToPoolIndex; // Average source over the time step for a particular radiant surface

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
		Real64 RadConvertToConvect; // LW and SW radiation converted to convective gain by pool cover
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
		
		// Report data
		Real64 PoolWaterTemp; // Average pool water temperature
		Real64 WaterInletTemp; // water inlet temperature
		Real64 WaterOutletTemp; // water outlet temperature
		Real64 WaterMassFlowRate; // water mass flow rate
		Real64 HeatPower; // heating sent to pool in Watts
		Real64 HeatEnergy; // heating sent to pool in Joules
		Real64 MiscEquipPower; // power for miscellaneous pool equipment in Watts
		Real64 MiscEquipEnergy; // energy for miscellaneous pool equipment in Joules

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
			PoolWaterTemp( 23.0 ),
			WaterInletTemp( 0.0 ),
			WaterOutletTemp( 0.0 ),
			WaterMassFlowRate( 0.0 ),
			HeatPower( 0.0 ),
			HeatEnergy( 0.0 ),
			MiscEquipPower( 0.0 ),
			MiscEquipEnergy( 0.0 )

		{}

		// Member Constructor
		SwimmingPoolData(
			std::string const Name, // name of swimming pool
			std::string const SurfaceName, // surface name of pool
			int const SurfacePtr, // index to surface array
			std::string const ZoneName, // Name of zone the pool is in
			int const ZonePtr, // Pointer to this zone in the Zone derived type
			std::string WaterInletNodeName, // water inlet node name
			int const WaterInletNode, // water inlet node number
			std::string WaterOutletNodeName, // water outlet node name
			int const WaterOutletNode, // water outlet node number
			int const HWLoopNum,
			int const HWLoopSide,
			int const HWBranchNum,
			int const HWCompNum,
			Real64 const WaterVolFlowMax, // maximum water flow rate for pool, m3/s
			Real64 const AvgDepth, // average depth of the pool, m
			Real64 const ActivityFactor, // Activity factor for the pool
			std::string const ActivityFactorSchedName, // Activity factor schedule name
			int const ActivityFactorSchedPtr, // Activity factor schedule pointer
			Real64 const CurActivityFactor, // Current activity factor
			std::string const MakeupWaterSupplyName, // Name of make-up water source
			std::string const MakeupWaterSupplySchedName, //Name of make-up water supply schedule
			int const MakeupWaterSupplySchedPtr, // Index to schedule for make-up water
			Real64 const CurMakeupWaterTemp, // Current makeup water temperature
			std::string const CoverSchedName, // Pool cover schedule name
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
			std::string const SetPtTempSchedName, // Schedule name for water setpoint temperature
			int const SetPtTempSchedPtr, // Schedule pointer for water setpoint temperature
			Real64 const CurSetPtTemp, // Current water setpoint temperature
			Real64 const MaxNumOfPeople, // Number of people in the pool as defined by user input
			std::string const PeopleSchedName, // Name of people schedule
			int const PeopleSchedPtr, // People schedule index
			std::string const PeopleHeatGainSchedName, // Name of people heat gain schedule
			int const PeopleHeatGainSchedPtr, // People heat gain schedule index
			Real64 const PeopleHeatGain, // Current people heat gain for the pool
			// Report data
			Real64 const PoolWaterTemp, // Average pool water temperature
			Real64 const WaterInletTemp, // water inlet temperature
			Real64 const WaterOutletTemp, // water outlet temperature
			Real64 const WaterMassFlowRate, // water mass flow rate
			Real64 const HeatPower, // heating sent to pool in Watts
			Real64 const HeatEnergy, // heating sent to pool in Joules
			Real64 const MiscEquipPower, // power for miscellaneous pool equipment in Watts
			Real64 const MiscEquipEnergy // energy for miscellaneous pool equipment in Joules
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
			PoolWaterTemp( PoolWaterTemp ),
			WaterInletTemp( WaterInletTemp ),
			WaterOutletTemp( WaterOutletTemp ),
			WaterMassFlowRate( WaterMassFlowRate ),
			HeatPower( HeatPower ),
			HeatEnergy( HeatEnergy ),
			MiscEquipPower( MiscEquipPower ),
			MiscEquipEnergy( MiscEquipEnergy )
		
		{}
	};

	// Object Data
	extern FArray1D< SwimmingPoolData > Pool;

	// Functions

	void
	SimSwimmingPool(
		int const SurfNum,
		Real64 & TempSurfIn,
		Real64 const RefAirTemp,
		Real64 const IterDampConst,
		Real64 const TempInsOld
	);

	void
	GetSwimmingPool();

	void
	InitSwimmingPool(
		int const PoolNum // Index of the swimming pool under consideration within the derived types
	);

	void
	CalcSwimmingPool(
		int const PoolNum, // Index of the swimming pool under consideration within the derived types
		int const SurfNum,
		Real64 & TempSurfIn,
		Real64 const RefAirTemp,
		Real64 const IterDampConst,
		Real64 const TempInsOld
	);

	void
	UpdateSwimmingPool(
		int const PoolNum // Index of the swimming pool under consideration within the derived types
	);

	void
	ReportSwimmingPool(
		int const PoolNum // Index of the swimming pool under consideration within the derived types
	);

	//     NOTICE

	//     Copyright � 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in EnergyPlus.f90.

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
