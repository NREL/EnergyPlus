#ifndef ExteriorEnergyUse_hh_INCLUDED
#define ExteriorEnergyUse_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace ExteriorEnergyUse {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern int const ElecUse; // Electricity
	extern int const GasUse; // Gas (Natural)
	extern int const WaterUse; // Water
	extern int const CoalUse; // Coal
	extern int const FuelOil1Use; // FuelOil#1
	extern int const FuelOil2Use; // FuelOil#2
	extern int const LPGUse; // PropaneGas
	extern int const GasolineUse; // Gasoline
	extern int const DieselUse; // Diesel
	extern int const SteamUse; // Steam
	extern int const DistrictCoolUse; // Purchased Cooling
	extern int const DistrictHeatUse; // Purchased Heating
	extern int const OtherFuel1Use; // OtherFuel1
	extern int const OtherFuel2Use; // OtherFuel2

	extern int const ScheduleOnly; // exterior lights only on schedule
	extern int const AstroClockOverride; // exterior lights controlled to turn off during day.

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	extern int NumExteriorLights; // Number of Exterior Light Inputs
	extern int NumExteriorEqs; // Number of Exterior Equipment Inputs

	// SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>

	// Name Public routines, optionally name Private routines within this module

	// Types

	struct ExteriorLightUsage
	{
		// Members
		std::string Name; // Descriptive name -- will show on reporting
		int SchedPtr; // Can be scheduled
		Real64 DesignLevel; // Consumption in Watts
		Real64 Power; // Power = DesignLevel * ScheduleValue
		Real64 CurrentUse; // Use for this time step
		int ControlMode; // Control mode Schedule Only or Astronomical Clock plus schedule
		bool ManageDemand; // Flag to indicate whether to use demand limiting
		Real64 DemandLimit; // Demand limit set by demand manager [W]
		bool PowerActuatorOn; // EMS flag
		Real64 PowerActuatorValue; // EMS value
		Real64 SumConsumption; // sum of electric consumption [J] for reporting
		Real64 SumTimeNotZeroCons; // sum of time of positive electric consumption [hr]

		// Default Constructor
		ExteriorLightUsage() :
			SchedPtr( 0 ),
			DesignLevel( 0.0 ),
			Power( 0.0 ),
			CurrentUse( 0.0 ),
			ControlMode( 1 ),
			ManageDemand( false ),
			DemandLimit( 0.0 ),
			PowerActuatorOn( false ),
			SumConsumption( 0.0 ),
			SumTimeNotZeroCons( 0.0 )
		{}

		// Member Constructor
		ExteriorLightUsage(
			std::string const & Name, // Descriptive name -- will show on reporting
			int const SchedPtr, // Can be scheduled
			Real64 const DesignLevel, // Consumption in Watts
			Real64 const Power, // Power = DesignLevel * ScheduleValue
			Real64 const CurrentUse, // Use for this time step
			int const ControlMode, // Control mode Schedule Only or Astronomical Clock plus schedule
			bool const ManageDemand, // Flag to indicate whether to use demand limiting
			Real64 const DemandLimit, // Demand limit set by demand manager [W]
			bool const PowerActuatorOn, // EMS flag
			Real64 const PowerActuatorValue, // EMS value
			Real64 const SumConsumption, // sum of electric consumption [J] for reporting
			Real64 const SumTimeNotZeroCons // sum of time of positive electric consumption [hr]
		) :
			Name( Name ),
			SchedPtr( SchedPtr ),
			DesignLevel( DesignLevel ),
			Power( Power ),
			CurrentUse( CurrentUse ),
			ControlMode( ControlMode ),
			ManageDemand( ManageDemand ),
			DemandLimit( DemandLimit ),
			PowerActuatorOn( PowerActuatorOn ),
			PowerActuatorValue( PowerActuatorValue ),
			SumConsumption( SumConsumption ),
			SumTimeNotZeroCons( SumTimeNotZeroCons )
		{}

	};

	struct ExteriorEquipmentUsage
	{
		// Members
		std::string Name; // Descriptive name -- will show on reporting
		int FuelType;
		int SchedPtr; // Can be scheduled
		Real64 DesignLevel; // Design Consumption (Watts, except for Water Equipment)
		Real64 Power; // Power = DesignLevel * ScheduleValue
		Real64 CurrentUse; // Use for this time step
		bool ManageDemand; // Flag to indicate whether to use demand limiting
		Real64 DemandLimit; // Demand limit set by demand manager [W]

		// Default Constructor
		ExteriorEquipmentUsage() :
			FuelType( 0 ),
			SchedPtr( 0 ),
			DesignLevel( 0.0 ),
			Power( 0.0 ),
			CurrentUse( 0.0 ),
			ManageDemand( false ),
			DemandLimit( 0.0 )
		{}

		// Member Constructor
		ExteriorEquipmentUsage(
			std::string const & Name, // Descriptive name -- will show on reporting
			int const FuelType,
			int const SchedPtr, // Can be scheduled
			Real64 const DesignLevel, // Design Consumption (Watts, except for Water Equipment)
			Real64 const Power, // Power = DesignLevel * ScheduleValue
			Real64 const CurrentUse, // Use for this time step
			bool const ManageDemand, // Flag to indicate whether to use demand limiting
			Real64 const DemandLimit // Demand limit set by demand manager [W]
		) :
			Name( Name ),
			FuelType( FuelType ),
			SchedPtr( SchedPtr ),
			DesignLevel( DesignLevel ),
			Power( Power ),
			CurrentUse( CurrentUse ),
			ManageDemand( ManageDemand ),
			DemandLimit( DemandLimit )
		{}

	};

	// Object Data
	extern FArray1D< ExteriorLightUsage > ExteriorLights; // Structure for Exterior Light reporting
	extern FArray1D< ExteriorEquipmentUsage > ExteriorEquipment; // Structure for Exterior Equipment Reporting

	// Functions

	void
	ManageExteriorEnergyUse();

	void
	GetExteriorEnergyUseInput();

	void
	ValidateFuelType(
		int & FuelTypeNumber, // Fuel Type to be set in structure.
		std::string const & FuelTypeAlpha, // Fuel Type String
		std::string & FuelTypeString, // Standardized Fuel Type String (for variable naming)
		std::string const & CurrentModuleObject, // object being parsed
		std::string const & CurrentField, // current field being parsed
		std::string const & CurrentName // current object name being parsed
	);

	void
	ReportExteriorEnergyUse();

	//     NOTICE

	//     Copyright � 1996-2014 The Board of Trustees of the University of Illinois
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

} // ExteriorEnergyUse

} // EnergyPlus

#endif
