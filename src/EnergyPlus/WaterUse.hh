#ifndef WaterUse_hh_INCLUDED
#define WaterUse_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace WaterUse {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern int const HeatRecoveryHXIdeal;
	extern int const HeatRecoveryHXCounterFlow;
	extern int const HeatRecoveryHXCrossFlow;

	extern int const HeatRecoveryConfigPlant;
	extern int const HeatRecoveryConfigEquipment;
	extern int const HeatRecoveryConfigPlantAndEquip;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE TYPE DECLARATIONS:

	// MODULE VARIABLE DECLARATIONS:
	extern int NumWaterEquipment;
	extern int NumWaterConnections;
	//INTEGER :: MaxIterationsErrorCount =0
	extern bool GetWaterUseInputFlag;

	extern Array1D_bool CheckEquipName;
	extern Array1D_bool CheckPlantLoop;

	// SUBROUTINE SPECIFICATIONS:

	// Types

	struct WaterEquipmentType
	{
		// Members
		std::string Name; // Name of DHW
		std::string EndUseSubcatName;
		int Connections; // Index for WATER USE CONNECTIONS object
		Real64 PeakVolFlowRate; // Peak volumetric flow rate, also water consumption rate (m3/s)
		int FlowRateFracSchedule; // Pointer to schedule object
		Real64 ColdVolFlowRate;
		Real64 HotVolFlowRate;
		Real64 TotalVolFlowRate; // Volumetric flow rate, also water consumption rate (m3/s)
		Real64 ColdMassFlowRate;
		Real64 HotMassFlowRate;
		Real64 TotalMassFlowRate; // Mass flow rate (kg/s)
		Real64 DrainMassFlowRate;
		int ColdTempSchedule; // Index for schedule object
		int HotTempSchedule; // Index for schedule object
		int TargetTempSchedule; // Index for schedule object
		Real64 ColdTemp; // Cold supply water temperature (C)
		Real64 HotTemp; // Hot supply water temperature (C)
		Real64 TargetTemp; // Target (mixed) water temperature (C)
		Real64 MixedTemp; // Actual outlet (mixed) water temperature (C)
		Real64 DrainTemp;
		int Zone; // Index for zone object
		int SensibleFracSchedule; // Pointer to schedule object
		Real64 SensibleRate;
		Real64 SensibleEnergy;
		Real64 SensibleRateNoMultiplier;
		int LatentFracSchedule; // Pointer to schedule object
		Real64 LatentRate;
		Real64 LatentEnergy;
		Real64 LatentRateNoMultiplier;
		Real64 MoistureRate;
		Real64 MoistureMass;
		Real64 ColdVolume; // Water consumption (m3)
		Real64 HotVolume; // Water consumption (m3)
		Real64 TotalVolume; // Water consumption (m3)
		Real64 Power; // Heating rate required to meet the mixed water temperature (W)
		Real64 Energy; // Heating energy required to meet the mixed water temperature (J)

		// Default Constructor
		WaterEquipmentType() :
			Connections( 0 ),
			PeakVolFlowRate( 0.0 ),
			FlowRateFracSchedule( 0 ),
			ColdVolFlowRate( 0.0 ),
			HotVolFlowRate( 0.0 ),
			TotalVolFlowRate( 0.0 ),
			ColdMassFlowRate( 0.0 ),
			HotMassFlowRate( 0.0 ),
			TotalMassFlowRate( 0.0 ),
			DrainMassFlowRate( 0.0 ),
			ColdTempSchedule( 0 ),
			HotTempSchedule( 0 ),
			TargetTempSchedule( 0 ),
			ColdTemp( 0.0 ),
			HotTemp( 0.0 ),
			TargetTemp( 0.0 ),
			MixedTemp( 0.0 ),
			DrainTemp( 0.0 ),
			Zone( 0 ),
			SensibleFracSchedule( 0 ),
			SensibleRate( 0.0 ),
			SensibleEnergy( 0.0 ),
			SensibleRateNoMultiplier( 0.0 ),
			LatentFracSchedule( 0 ),
			LatentRate( 0.0 ),
			LatentEnergy( 0.0 ),
			LatentRateNoMultiplier( 0.0 ),
			MoistureRate( 0.0 ),
			MoistureMass( 0.0 ),
			ColdVolume( 0.0 ),
			HotVolume( 0.0 ),
			TotalVolume( 0.0 ),
			Power( 0.0 ),
			Energy( 0.0 )
		{}

		// Member Constructor
		WaterEquipmentType(
			std::string const & Name, // Name of DHW
			std::string const & EndUseSubcatName,
			int const Connections, // Index for WATER USE CONNECTIONS object
			Real64 const PeakVolFlowRate, // Peak volumetric flow rate, also water consumption rate (m3/s)
			int const FlowRateFracSchedule, // Pointer to schedule object
			Real64 const ColdVolFlowRate,
			Real64 const HotVolFlowRate,
			Real64 const TotalVolFlowRate, // Volumetric flow rate, also water consumption rate (m3/s)
			Real64 const ColdMassFlowRate,
			Real64 const HotMassFlowRate,
			Real64 const TotalMassFlowRate, // Mass flow rate (kg/s)
			Real64 const DrainMassFlowRate,
			int const ColdTempSchedule, // Index for schedule object
			int const HotTempSchedule, // Index for schedule object
			int const TargetTempSchedule, // Index for schedule object
			Real64 const ColdTemp, // Cold supply water temperature (C)
			Real64 const HotTemp, // Hot supply water temperature (C)
			Real64 const TargetTemp, // Target (mixed) water temperature (C)
			Real64 const MixedTemp, // Actual outlet (mixed) water temperature (C)
			Real64 const DrainTemp,
			int const Zone, // Index for zone object
			int const SensibleFracSchedule, // Pointer to schedule object
			Real64 const SensibleRate,
			Real64 const SensibleEnergy,
			Real64 const SensibleRateNoMultiplier,
			int const LatentFracSchedule, // Pointer to schedule object
			Real64 const LatentRate,
			Real64 const LatentEnergy,
			Real64 const LatentRateNoMultiplier,
			Real64 const MoistureRate,
			Real64 const MoistureMass,
			Real64 const ColdVolume, // Water consumption (m3)
			Real64 const HotVolume, // Water consumption (m3)
			Real64 const TotalVolume, // Water consumption (m3)
			Real64 const Power, // Heating rate required to meet the mixed water temperature (W)
			Real64 const Energy // Heating energy required to meet the mixed water temperature (J)
		) :
			Name( Name ),
			EndUseSubcatName( EndUseSubcatName ),
			Connections( Connections ),
			PeakVolFlowRate( PeakVolFlowRate ),
			FlowRateFracSchedule( FlowRateFracSchedule ),
			ColdVolFlowRate( ColdVolFlowRate ),
			HotVolFlowRate( HotVolFlowRate ),
			TotalVolFlowRate( TotalVolFlowRate ),
			ColdMassFlowRate( ColdMassFlowRate ),
			HotMassFlowRate( HotMassFlowRate ),
			TotalMassFlowRate( TotalMassFlowRate ),
			DrainMassFlowRate( DrainMassFlowRate ),
			ColdTempSchedule( ColdTempSchedule ),
			HotTempSchedule( HotTempSchedule ),
			TargetTempSchedule( TargetTempSchedule ),
			ColdTemp( ColdTemp ),
			HotTemp( HotTemp ),
			TargetTemp( TargetTemp ),
			MixedTemp( MixedTemp ),
			DrainTemp( DrainTemp ),
			Zone( Zone ),
			SensibleFracSchedule( SensibleFracSchedule ),
			SensibleRate( SensibleRate ),
			SensibleEnergy( SensibleEnergy ),
			SensibleRateNoMultiplier( SensibleRateNoMultiplier ),
			LatentFracSchedule( LatentFracSchedule ),
			LatentRate( LatentRate ),
			LatentEnergy( LatentEnergy ),
			LatentRateNoMultiplier( LatentRateNoMultiplier ),
			MoistureRate( MoistureRate ),
			MoistureMass( MoistureMass ),
			ColdVolume( ColdVolume ),
			HotVolume( HotVolume ),
			TotalVolume( TotalVolume ),
			Power( Power ),
			Energy( Energy )
		{}

		// Reset Some Values to Zeros
		void
		reset()
		{
			SensibleRate = 0.0;
			SensibleEnergy = 0.0;
			LatentRate = 0.0;
			LatentEnergy = 0.0;
			MixedTemp = 0.0;
			TotalMassFlowRate = 0.0;
			DrainTemp = 0.0;
		}

	};

	struct WaterConnectionsType
	{
		// Members
		std::string Name; // Name of DHW
		bool Init; // Flag for initialization:  TRUE means do the init
		bool InitSizing; // Flag for initialization of plant sizing
		bool StandAlone; // Flag for operation with no plant connections
		int InletNode; // Hot water demand node
		int OutletNode; // Cold water supply node
		int SupplyTankNum;
		int RecoveryTankNum;
		int TankDemandID; // array to request flow from supply tank
		int TankSupplyID; // array to send flow to recovery tank
		bool HeatRecovery;
		int HeatRecoveryHX;
		int HeatRecoveryConfig;
		Real64 HXUA;
		Real64 Effectiveness;
		Real64 RecoveryRate;
		Real64 RecoveryEnergy;
		Real64 MainsMassFlowRate; // Mass flow rate (kg/s)
		Real64 TankMassFlowRate; // Mass flow rate (kg/s)
		Real64 ColdMassFlowRate; // Mass flow rate (kg/s)  cold = mains + tank
		Real64 HotMassFlowRate; // Mass flow rate (kg/s)
		Real64 TotalMassFlowRate; // Mass flow rate (kg/s) total = cold + hot
		Real64 DrainMassFlowRate;
		Real64 RecoveryMassFlowRate;
		Real64 PeakVolFlowRate; // Volumetric flow rate, also water consumption rate (m3/s)
		Real64 MainsVolFlowRate; // Volumetric flow rate, also water consumption rate (m3/s)
		Real64 TankVolFlowRate; // Volumetric flow rate, also water consumption rate (m3/s)
		Real64 ColdVolFlowRate; // Volumetric flow rate, also water consumption rate (m3/s)
		Real64 HotVolFlowRate; // Volumetric flow rate, also water consumption rate (m3/s)
		Real64 TotalVolFlowRate; // Volumetric flow rate, also water consumption rate (m3/s)
		Real64 DrainVolFlowRate;
		Real64 PeakMassFlowRate; // Peak Mass flow rate for MassFlowRateMax
		int ColdTempSchedule; // Index for schedule object
		int HotTempSchedule; // Index for schedule object
		Real64 MainsTemp; // Cold supply water temperature (C)
		Real64 TankTemp; // Cold supply water temperature (C)
		Real64 ColdSupplyTemp; // cold from mains, schedule, or tank, depending
		Real64 ColdTemp; // Cold supply water temperature (C)  actual cold (could be reheated)
		Real64 HotTemp; // Hot supply water temperature (C)
		Real64 DrainTemp;
		Real64 RecoveryTemp;
		Real64 ReturnTemp;
		Real64 WasteTemp;
		Real64 TempError;
		Real64 MainsVolume; // Water consumption (m3)
		Real64 TankVolume; // Water consumption (m3)
		Real64 ColdVolume; // Water consumption (m3)
		Real64 HotVolume; // Water consumption (m3)
		Real64 TotalVolume; // Water consumption (m3)
		Real64 Power; // Heating rate required to raise temperature from cold to hot (W)
		Real64 Energy; // Heating energy required to raise temperature from cold to hot (J)
		int NumWaterEquipment;
		int MaxIterationsErrorIndex; // recurring error index
		Array1D_int WaterEquipment;
		int PlantLoopNum;
		int PlantLoopSide;
		int PlantLoopBranchNum;
		int PlantLoopCompNum;

		// Default Constructor
		WaterConnectionsType() :
			Init( true ),
			InitSizing( true ),
			StandAlone( false ),
			InletNode( 0 ),
			OutletNode( 0 ),
			SupplyTankNum( 0 ),
			RecoveryTankNum( 0 ),
			TankDemandID( 0 ),
			TankSupplyID( 0 ),
			HeatRecovery( false ),
			HeatRecoveryHX( HeatRecoveryHXIdeal ),
			HeatRecoveryConfig( HeatRecoveryConfigPlant ),
			HXUA( 0.0 ),
			Effectiveness( 0.0 ),
			RecoveryRate( 0.0 ),
			RecoveryEnergy( 0.0 ),
			MainsMassFlowRate( 0.0 ),
			TankMassFlowRate( 0.0 ),
			ColdMassFlowRate( 0.0 ),
			HotMassFlowRate( 0.0 ),
			TotalMassFlowRate( 0.0 ),
			DrainMassFlowRate( 0.0 ),
			RecoveryMassFlowRate( 0.0 ),
			PeakVolFlowRate( 0.0 ),
			MainsVolFlowRate( 0.0 ),
			TankVolFlowRate( 0.0 ),
			ColdVolFlowRate( 0.0 ),
			HotVolFlowRate( 0.0 ),
			TotalVolFlowRate( 0.0 ),
			DrainVolFlowRate( 0.0 ),
			PeakMassFlowRate( 0.0 ),
			ColdTempSchedule( 0 ),
			HotTempSchedule( 0 ),
			MainsTemp( 0.0 ),
			TankTemp( 0.0 ),
			ColdSupplyTemp( 0.0 ),
			ColdTemp( 0.0 ),
			HotTemp( 0.0 ),
			DrainTemp( 0.0 ),
			RecoveryTemp( 0.0 ),
			ReturnTemp( 0.0 ),
			WasteTemp( 0.0 ),
			TempError( 0.0 ),
			MainsVolume( 0.0 ),
			TankVolume( 0.0 ),
			ColdVolume( 0.0 ),
			HotVolume( 0.0 ),
			TotalVolume( 0.0 ),
			Power( 0.0 ),
			Energy( 0.0 ),
			NumWaterEquipment( 0 ),
			MaxIterationsErrorIndex( 0 ),
			PlantLoopNum( 0 ),
			PlantLoopSide( 0 ),
			PlantLoopBranchNum( 0 ),
			PlantLoopCompNum( 0 )
		{}

		// Member Constructor
		WaterConnectionsType(
			std::string const & Name, // Name of DHW
			bool const Init, // Flag for initialization:  TRUE means do the init
			bool const InitSizing, // Flag for initialization of plant sizing
			bool const StandAlone, // Flag for operation with no plant connections
			int const InletNode, // Hot water demand node
			int const OutletNode, // Cold water supply node
			int const SupplyTankNum,
			int const RecoveryTankNum,
			int const TankDemandID, // array to request flow from supply tank
			int const TankSupplyID, // array to send flow to recovery tank
			bool const HeatRecovery,
			int const HeatRecoveryHX,
			int const HeatRecoveryConfig,
			Real64 const HXUA,
			Real64 const Effectiveness,
			Real64 const RecoveryRate,
			Real64 const RecoveryEnergy,
			Real64 const MainsMassFlowRate, // Mass flow rate (kg/s)
			Real64 const TankMassFlowRate, // Mass flow rate (kg/s)
			Real64 const ColdMassFlowRate, // Mass flow rate (kg/s)  cold = mains + tank
			Real64 const HotMassFlowRate, // Mass flow rate (kg/s)
			Real64 const TotalMassFlowRate, // Mass flow rate (kg/s) total = cold + hot
			Real64 const DrainMassFlowRate,
			Real64 const RecoveryMassFlowRate,
			Real64 const PeakVolFlowRate, // Volumetric flow rate, also water consumption rate (m3/s)
			Real64 const MainsVolFlowRate, // Volumetric flow rate, also water consumption rate (m3/s)
			Real64 const TankVolFlowRate, // Volumetric flow rate, also water consumption rate (m3/s)
			Real64 const ColdVolFlowRate, // Volumetric flow rate, also water consumption rate (m3/s)
			Real64 const HotVolFlowRate, // Volumetric flow rate, also water consumption rate (m3/s)
			Real64 const TotalVolFlowRate, // Volumetric flow rate, also water consumption rate (m3/s)
			Real64 const DrainVolFlowRate,
			Real64 const PeakMassFlowRate, // Peak Mass flow rate for MassFlowRateMax
			int const ColdTempSchedule, // Index for schedule object
			int const HotTempSchedule, // Index for schedule object
			Real64 const MainsTemp, // Cold supply water temperature (C)
			Real64 const TankTemp, // Cold supply water temperature (C)
			Real64 const ColdSupplyTemp, // cold from mains, schedule, or tank, depending
			Real64 const ColdTemp, // Cold supply water temperature (C)  actual cold (could be reheated)
			Real64 const HotTemp, // Hot supply water temperature (C)
			Real64 const DrainTemp,
			Real64 const RecoveryTemp,
			Real64 const ReturnTemp,
			Real64 const WasteTemp,
			Real64 const TempError,
			Real64 const MainsVolume, // Water consumption (m3)
			Real64 const TankVolume, // Water consumption (m3)
			Real64 const ColdVolume, // Water consumption (m3)
			Real64 const HotVolume, // Water consumption (m3)
			Real64 const TotalVolume, // Water consumption (m3)
			Real64 const Power, // Heating rate required to raise temperature from cold to hot (W)
			Real64 const Energy, // Heating energy required to raise temperature from cold to hot (J)
			int const NumWaterEquipment,
			int const MaxIterationsErrorIndex, // recurring error index
			Array1_int const & WaterEquipment,
			int const PlantLoopNum,
			int const PlantLoopSide,
			int const PlantLoopBranchNum,
			int const PlantLoopCompNum
		) :
			Name( Name ),
			Init( Init ),
			InitSizing( InitSizing ),
			StandAlone( StandAlone ),
			InletNode( InletNode ),
			OutletNode( OutletNode ),
			SupplyTankNum( SupplyTankNum ),
			RecoveryTankNum( RecoveryTankNum ),
			TankDemandID( TankDemandID ),
			TankSupplyID( TankSupplyID ),
			HeatRecovery( HeatRecovery ),
			HeatRecoveryHX( HeatRecoveryHX ),
			HeatRecoveryConfig( HeatRecoveryConfig ),
			HXUA( HXUA ),
			Effectiveness( Effectiveness ),
			RecoveryRate( RecoveryRate ),
			RecoveryEnergy( RecoveryEnergy ),
			MainsMassFlowRate( MainsMassFlowRate ),
			TankMassFlowRate( TankMassFlowRate ),
			ColdMassFlowRate( ColdMassFlowRate ),
			HotMassFlowRate( HotMassFlowRate ),
			TotalMassFlowRate( TotalMassFlowRate ),
			DrainMassFlowRate( DrainMassFlowRate ),
			RecoveryMassFlowRate( RecoveryMassFlowRate ),
			PeakVolFlowRate( PeakVolFlowRate ),
			MainsVolFlowRate( MainsVolFlowRate ),
			TankVolFlowRate( TankVolFlowRate ),
			ColdVolFlowRate( ColdVolFlowRate ),
			HotVolFlowRate( HotVolFlowRate ),
			TotalVolFlowRate( TotalVolFlowRate ),
			DrainVolFlowRate( DrainVolFlowRate ),
			PeakMassFlowRate( PeakMassFlowRate ),
			ColdTempSchedule( ColdTempSchedule ),
			HotTempSchedule( HotTempSchedule ),
			MainsTemp( MainsTemp ),
			TankTemp( TankTemp ),
			ColdSupplyTemp( ColdSupplyTemp ),
			ColdTemp( ColdTemp ),
			HotTemp( HotTemp ),
			DrainTemp( DrainTemp ),
			RecoveryTemp( RecoveryTemp ),
			ReturnTemp( ReturnTemp ),
			WasteTemp( WasteTemp ),
			TempError( TempError ),
			MainsVolume( MainsVolume ),
			TankVolume( TankVolume ),
			ColdVolume( ColdVolume ),
			HotVolume( HotVolume ),
			TotalVolume( TotalVolume ),
			Power( Power ),
			Energy( Energy ),
			NumWaterEquipment( NumWaterEquipment ),
			MaxIterationsErrorIndex( MaxIterationsErrorIndex ),
			WaterEquipment( WaterEquipment ),
			PlantLoopNum( PlantLoopNum ),
			PlantLoopSide( PlantLoopSide ),
			PlantLoopBranchNum( PlantLoopBranchNum ),
			PlantLoopCompNum( PlantLoopCompNum )
		{}

	};

	// Object Data
	extern Array1D< WaterEquipmentType > WaterEquipment;
	extern Array1D< WaterConnectionsType > WaterConnections;

	// Functions

	void
	SimulateWaterUse( bool const FirstHVACIteration );

	void
	SimulateWaterUseConnection(
		int const EquipTypeNum,
		std::string const & CompName,
		int & CompIndex,
		bool const InitLoopEquip,
		bool const FirstHVACIteration
	);

	void
	GetWaterUseInput();

	void
	CalcEquipmentFlowRates( int const WaterEquipNum );

	void
	CalcEquipmentDrainTemp( int const WaterEquipNum );

	void
	InitConnections( int const WaterConnNum );

	void
	CalcConnectionsFlowRates(
		int const WaterConnNum,
		bool const FirstHVACIteration
	);

	void
	CalcConnectionsDrainTemp( int const WaterConnNum );

	void
	CalcConnectionsHeatRecovery( int const WaterConnNum );

	void
	UpdateWaterConnections( int const WaterConnNum );

	void
	ReportStandAloneWaterUse();

	void
	ReportWaterUse( int const WaterConnNum );

	void
	CalcWaterUseZoneGains();

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
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

} // WaterUse

} // EnergyPlus

#endif
