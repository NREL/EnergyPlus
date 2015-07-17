#ifndef CoolTower_hh_INCLUDED
#define CoolTower_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

// (ref: Object: COOLTOWER:SHOWER)

namespace CoolTower {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS
	extern int const WaterSupplyFromMains;
	extern int const WaterSupplyFromTank;
	extern int const WaterFlowSchedule;
	extern int const WindDrivenFlow;

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLES DECLARATIONS:
	extern int NumCoolTowers; // Total cooltower statements in inputs

	// Subroutine Specifications for the Heat Balance Module

	// Types

	struct CoolTowerParams
	{
		// Members
		std::string Name; // The component name
		std::string CompType; // Type of component
		std::string Schedule; // Available schedule
		std::string ZoneName; // Name of zone the component is serving
		std::string PumpSchedName; // Available schedule of the water pump
		int SchedPtr; // Index to schedule
		int ZonePtr; // Point to this zone
		int PumpSchedPtr; // Index to schedule for water pump
		int FlowCtrlType; // Type of cooltower operation
		int CoolTWaterSupplyMode; // Type of water source
		std::string CoolTWaterSupplyName; // Name of water source
		int CoolTWaterSupTankID; // Index to water storage tank
		int CoolTWaterTankDemandARRID; // Index to water storage demand
		Real64 TowerHeight; // Effective cooltower height in m
		Real64 OutletArea; // Outlet area where conditioned air comes in m2
		Real64 OutletVelocity; // Outlet velocity of the cooltower in m/s
		Real64 MaxAirVolFlowRate; // Maximum allowable airflow in m3/s
		Real64 AirMassFlowRate; // Air mass flow rate in kg/s
		Real64 CoolTAirMass; // Air mass in kg
		Real64 MinZoneTemp; // Lower temperature limit to prevent over cooling in C
		Real64 FracWaterLoss; // Fraction of estimated blowdown and drift water
		Real64 FracFlowSched; // Fraction of airflow loss
		Real64 MaxWaterFlowRate; // Maximum limit of water flow rate in m3/s
		Real64 ActualWaterFlowRate; // Actual water mass flow rate in m3/s
		Real64 RatedPumpPower; // Rated power consumption for water pump serving the cooltower in watts
		Real64 SenHeatLoss; // Sensible heat loss in Joules
		Real64 SenHeatPower; // Sensible heat loss rate in watts
		Real64 LatHeatLoss; // Latent heat loss in Joules
		Real64 LatHeatPower; // Latent heat loss rate in watts
		Real64 AirVolFlowRate; // Air flow rate in m3/s
		Real64 AirVolFlowRateStd; // Air flow rate in m3/s at standard conditions
		Real64 CoolTAirVol; // Air volume in m3
		Real64 ActualAirVolFlowRate; // Actual air flow rate in m3/s
		Real64 InletDBTemp; // Outdoor dry bulb temperature in C
		Real64 InletWBTemp; // Outdoor wet bulb temperature in C
		Real64 InletHumRat; // Outdoor humidity ratio
		Real64 OutletTemp; // Dry bulb temperature at cooltower exit in C
		Real64 OutletHumRat; // Humidity ratio at cooltower exit
		Real64 CoolTWaterConsumpRate; // Total water consumption during the processes in m3/s
		Real64 CoolTWaterStarvMakeupRate; // Water provided from the mains (m3/s)
		Real64 CoolTWaterStarvMakeup; // Water provided from the mains
		Real64 CoolTWaterConsump; // Total water consumption in m3
		Real64 PumpElecPower; // Pump power in watts
		Real64 PumpElecConsump; // Pump energy consumption in Joules

		// Default Constructor
		CoolTowerParams() :
			SchedPtr( 0 ),
			ZonePtr( 0 ),
			PumpSchedPtr( 0 ),
			FlowCtrlType( 0 ),
			CoolTWaterSupplyMode( WaterSupplyFromMains ),
			CoolTWaterSupTankID( 0 ),
			CoolTWaterTankDemandARRID( 0 ),
			TowerHeight( 0.0 ),
			OutletArea( 0.0 ),
			OutletVelocity( 0.0 ),
			MaxAirVolFlowRate( 0.0 ),
			AirMassFlowRate( 0.0 ),
			CoolTAirMass( 0.0 ),
			MinZoneTemp( 0.0 ),
			FracWaterLoss( 0.0 ),
			FracFlowSched( 0.0 ),
			MaxWaterFlowRate( 0.0 ),
			ActualWaterFlowRate( 0.0 ),
			RatedPumpPower( 0.0 ),
			SenHeatLoss( 0.0 ),
			SenHeatPower( 0.0 ),
			LatHeatLoss( 0.0 ),
			LatHeatPower( 0.0 ),
			AirVolFlowRate( 0.0 ),
			AirVolFlowRateStd( 0.0 ),
			CoolTAirVol( 0.0 ),
			ActualAirVolFlowRate( 0.0 ),
			InletDBTemp( 0.0 ),
			InletWBTemp( 0.0 ),
			InletHumRat( 0.0 ),
			OutletTemp( 0.0 ),
			OutletHumRat( 0.0 ),
			CoolTWaterConsumpRate( 0.0 ),
			CoolTWaterStarvMakeupRate( 0.0 ),
			CoolTWaterStarvMakeup( 0.0 ),
			CoolTWaterConsump( 0.0 ),
			PumpElecPower( 0.0 ),
			PumpElecConsump( 0.0 )
		{}

		// Member Constructor
		CoolTowerParams(
			std::string const & Name, // The component name
			std::string const & CompType, // Type of component
			std::string const & Schedule, // Available schedule
			std::string const & ZoneName, // Name of zone the component is serving
			std::string const & PumpSchedName, // Available schedule of the water pump
			int const SchedPtr, // Index to schedule
			int const ZonePtr, // Point to this zone
			int const PumpSchedPtr, // Index to schedule for water pump
			int const FlowCtrlType, // Type of cooltower operation
			int const CoolTWaterSupplyMode, // Type of water source
			std::string const & CoolTWaterSupplyName, // Name of water source
			int const CoolTWaterSupTankID, // Index to water storage tank
			int const CoolTWaterTankDemandARRID, // Index to water storage demand
			Real64 const TowerHeight, // Effective cooltower height in m
			Real64 const OutletArea, // Outlet area where conditioned air comes in m2
			Real64 const OutletVelocity, // Outlet velocity of the cooltower in m/s
			Real64 const MaxAirVolFlowRate, // Maximum allowable airflow in m3/s
			Real64 const AirMassFlowRate, // Air mass flow rate in kg/s
			Real64 const CoolTAirMass, // Air mass in kg
			Real64 const MinZoneTemp, // Lower temperature limit to prevent over cooling in C
			Real64 const FracWaterLoss, // Fraction of estimated blowdown and drift water
			Real64 const FracFlowSched, // Fraction of airflow loss
			Real64 const MaxWaterFlowRate, // Maximum limit of water flow rate in m3/s
			Real64 const ActualWaterFlowRate, // Actual water mass flow rate in m3/s
			Real64 const RatedPumpPower, // Rated power consumption for water pump serving the cooltower in watts
			Real64 const SenHeatLoss, // Sensible heat loss in Joules
			Real64 const SenHeatPower, // Sensible heat loss rate in watts
			Real64 const LatHeatLoss, // Latent heat loss in Joules
			Real64 const LatHeatPower, // Latent heat loss rate in watts
			Real64 const AirVolFlowRate, // Air flow rate in m3/s
			Real64 const AirVolFlowRateStd, // Air flow rate in m3/s at standard conditions
			Real64 const CoolTAirVol, // Air volume in m3
			Real64 const ActualAirVolFlowRate, // Actual air flow rate in m3/s
			Real64 const InletDBTemp, // Outdoor dry bulb temperature in C
			Real64 const InletWBTemp, // Outdoor wet bulb temperature in C
			Real64 const InletHumRat, // Outdoor humidity ratio
			Real64 const OutletTemp, // Dry bulb temperature at cooltower exit in C
			Real64 const OutletHumRat, // Humidity ratio at cooltower exit
			Real64 const CoolTWaterConsumpRate, // Total water consumption during the processes in m3/s
			Real64 const CoolTWaterStarvMakeupRate, // Water provided from the mains (m3/s)
			Real64 const CoolTWaterStarvMakeup, // Water provided from the mains
			Real64 const CoolTWaterConsump, // Total water consumption in m3
			Real64 const PumpElecPower, // Pump power in watts
			Real64 const PumpElecConsump // Pump energy consumption in Joules
		) :
			Name( Name ),
			CompType( CompType ),
			Schedule( Schedule ),
			ZoneName( ZoneName ),
			PumpSchedName( PumpSchedName ),
			SchedPtr( SchedPtr ),
			ZonePtr( ZonePtr ),
			PumpSchedPtr( PumpSchedPtr ),
			FlowCtrlType( FlowCtrlType ),
			CoolTWaterSupplyMode( CoolTWaterSupplyMode ),
			CoolTWaterSupplyName( CoolTWaterSupplyName ),
			CoolTWaterSupTankID( CoolTWaterSupTankID ),
			CoolTWaterTankDemandARRID( CoolTWaterTankDemandARRID ),
			TowerHeight( TowerHeight ),
			OutletArea( OutletArea ),
			OutletVelocity( OutletVelocity ),
			MaxAirVolFlowRate( MaxAirVolFlowRate ),
			AirMassFlowRate( AirMassFlowRate ),
			CoolTAirMass( CoolTAirMass ),
			MinZoneTemp( MinZoneTemp ),
			FracWaterLoss( FracWaterLoss ),
			FracFlowSched( FracFlowSched ),
			MaxWaterFlowRate( MaxWaterFlowRate ),
			ActualWaterFlowRate( ActualWaterFlowRate ),
			RatedPumpPower( RatedPumpPower ),
			SenHeatLoss( SenHeatLoss ),
			SenHeatPower( SenHeatPower ),
			LatHeatLoss( LatHeatLoss ),
			LatHeatPower( LatHeatPower ),
			AirVolFlowRate( AirVolFlowRate ),
			AirVolFlowRateStd( AirVolFlowRateStd ),
			CoolTAirVol( CoolTAirVol ),
			ActualAirVolFlowRate( ActualAirVolFlowRate ),
			InletDBTemp( InletDBTemp ),
			InletWBTemp( InletWBTemp ),
			InletHumRat( InletHumRat ),
			OutletTemp( OutletTemp ),
			OutletHumRat( OutletHumRat ),
			CoolTWaterConsumpRate( CoolTWaterConsumpRate ),
			CoolTWaterStarvMakeupRate( CoolTWaterStarvMakeupRate ),
			CoolTWaterStarvMakeup( CoolTWaterStarvMakeup ),
			CoolTWaterConsump( CoolTWaterConsump ),
			PumpElecPower( PumpElecPower ),
			PumpElecConsump( PumpElecConsump )
		{}

	};

	// Object Data
	extern Array1D< CoolTowerParams > CoolTowerSys;

	// Functions

	void
	ManageCoolTower();

	void
	GetCoolTower();

	void
	CalcCoolTower();

	void
	UpdateCoolTower();

	void
	ReportCoolTower();

	//*****************************************************************************************
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

} // CoolTower

} // EnergyPlus

#endif
