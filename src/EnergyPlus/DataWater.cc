// EnergyPlus Headers
#include <DataWater.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataWater {

	// Module containing the routines dealing with the DataWater

	// MODULE INFORMATION:
	//       AUTHOR         B. Griffith
	//       DATE WRITTEN   August 2006
	//       MODIFIED       D. Sailor -- to add ecoroof irrigation
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This data-only module is a repository for the variables that relate specifically
	// to the management of water in the simulation

	// METHODOLOGY EMPLOYED:
	// <description>

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLI

	// MODULE PARAMETER DEFINITION

	int const ScheduledTankTemp( 101 ); // tank water temperature is user input via schedule
	int const TankZoneThermalCoupled( 102 ); // tank water temperature is modeled using simple UA

	int const RainSchedDesign( 201 ); // mode of Rainfall determination is Scheduled Design
	int const IrrSchedDesign( 202 ); // mode of Irrigation determination is Scheduled Design (DJS -PSU)
	int const IrrSmartSched( 203 ); // mode of irrigation DJS - PSU

	int const ConstantRainLossFactor( 301 );
	int const ScheduledRainLossFactor( 302 );

	int const AmbientTempSchedule( 1 ); // ambient temperature around tank (or HPWH inlet air) is scheduled
	int const AmbientTempZone( 2 ); // tank is located in a zone or HPWH inlet air is zone air only
	int const AmbientTempExterior( 3 ); // tank is located outdoors or HPWH inlet air is outdoor air only

	int const ConstantWaterTable( 401 );
	int const ScheduledWaterTable( 402 );

	int const NoControlLevel( 501 );
	int const MainsFloatValve( 502 );
	int const WellFloatValve( 503 );
	int const WellFloatMainsBackup( 504 );
	int const OtherTankFloatValve( 505 );
	int const TankMainsBackup( 506 );

	int const OverflowDiscarded( 601 );
	int const OverflowToTank( 602 );

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	int NumWaterStorageTanks( 0 ); // number of water Storage tanks in model
	int NumRainCollectors( 0 ); // number of rainfall collectors in model
	int NumGroundWaterWells( 0 ); // number of
	int NumSiteRainFall( 0 );
	int NumIrrigation( 0 ); // DJS PSU Dec 2006 number of irrigation descriptions (1 allowed)
	bool AnyWaterSystemsInModel( false ); // control flag set true if any water systems
	bool WaterSystemGetInputCalled( false ); // set true once input data gotten.
	bool AnyIrrigationInModel( false ); // control flag set true if irrigation input for ecoroof DJS PSU Dec 2006

	// Object Data
	SiteRainFallDataStruct RainFall; // type of rainfall modeling | design annual rain | rain sched id | nominal annual rain | current rate | current amount
	IrrigationDataStruct Irrigation; // type of irrigation modeling | Irrigation schedule id | scheduled amount | actual amount | irrigation threshold
	Array1D< StorageTankDataStruct > WaterStorage;
	Array1D< RainfallCollectorDataStruct > RainCollector;
	Array1D< GroundwaterWellDataStruct > GroundwaterWell;

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

} // DataWater

} // EnergyPlus
