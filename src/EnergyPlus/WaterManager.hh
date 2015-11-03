#ifndef WaterManager_hh_INCLUDED
#define WaterManager_hh_INCLUDED

// C++ Headers
#include <string>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace WaterManager {

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	// na

	// SUBROUTINE SPECIFICATIONS FOR MODULE WaterManager:
	// pointers for water storage tanks and their supply arrays
	// pointers for water storage tanks and their demand arrays

	// Functions

	void
	ManageWater();

	void
	ManageWaterInits();

	void
	GetWaterManagerInput();

	void
	UpdatePrecipitation();

	void
	UpdateIrrigation();

	void
	SizeWaterManager();

	void
	CalcWaterStorageTank( int const TankNum ); // Index of storage tank

	void
	SetupTankSupplyComponent(
		std::string const & CompName,
		std::string const & CompType,
		std::string const & TankName,
		bool & ErrorsFound,
		int & TankIndex,
		int & WaterSupplyIndex
	);

	void
	InternalSetupTankSupplyComponent(
		std::string const & CompName,
		std::string const & CompType,
		std::string const & TankName,
		bool & ErrorsFound,
		int & TankIndex,
		int & WaterSupplyIndex
	);

	void
	SetupTankDemandComponent(
		std::string const & CompName,
		std::string const & CompType,
		std::string const & TankName,
		bool & ErrorsFound,
		int & TankIndex,
		int & WaterDemandIndex
	);

	void
	InternalSetupTankDemandComponent(
		std::string const & CompName,
		std::string const & CompType,
		std::string const & TankName,
		bool & ErrorsFound,
		int & TankIndex,
		int & WaterDemandIndex
	);

	void
	CalcRainCollector( int const RainColNum ); // Index of rain collector

	void
	CalcGroundwaterWell( int const WellNum ); // Index of well

	void
	UpdateWaterManager();

	void
	ReportWaterManager();

} // WaterManager

} // EnergyPlus

#endif
