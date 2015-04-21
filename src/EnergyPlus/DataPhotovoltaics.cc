// EnergyPlus Headers
#include <DataPhotovoltaics.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataPhotovoltaics {

	// MODULE INFORMATION:
	//       AUTHOR         D. Bradley
	//       DATE WRITTEN   May 2003
	//       MODIFIED       B. Griffith, Dec. 2003, heavy changes, moved derived types here from Photovoltaics.cc
	//                      B. Griffith, Feb 2008, added BIPV and inverter to one-diode model
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This data-only module is a repository for the variables that relate specifically
	// to the native EnergyPlus photovoltaics simulation.

	// METHODOLOGY EMPLOYED:

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	std::string const cPVGeneratorObjectName( "Generator:Photovoltaic" );
	std::string const cPVSimplePerfObjectName( "PhotovoltaicPerformance:Simple" );
	std::string const cPVEquiv1DiodePerfObjectName( "PhotovoltaicPerformance:EquivalentOne-Diode" );
	std::string const cPVSandiaPerfObjectName( "PhotovoltaicPerformance:Sandia" );

	int const iNotYetSetPVModel( 0 );
	int const iSimplePVModel( 1001 );
	int const iTRNSYSPVModel( 1002 );
	int const iSandiaPVModel( 1003 );

	int const iNotYetSetCellIntegration( 0 ); // cell temp method not set
	int const iDecoupledCellIntegration( 1 ); // cell temp method based on energy balance
	int const iDecoupledUllebergDynamicCellIntegration( 2 ); // cell temp method based on energy bal with capacity
	int const iSurfaceOutsideFaceCellIntegration( 3 ); // cell temp method based on coupling to E+'s heat balance
	int const iTranspiredCollectorCellIntegration( 4 ); // cell temp method based on coupling to unglazed transpired co
	int const iExteriorVentedCavityCellIntegration( 5 ); // cell temp method based on coupling to nat vent exterior cavi
	int const iPVTSolarCollectorCellIntegration( 6 ); // cell temp method based on coupling to PVT model

	int const FixedEfficiency( 10 ); // simple PV, constant efficiency
	int const ScheduledEfficiency( 11 ); // simpel PV, scheduled efficiency

	int const CrystallineSiPVCells( 1 );
	int const AmorphousSiPVCells( 2 );

	Real64 const MinIrradiance( 0.3 ); // [W/m2] Assume no operation if Ic is below this number (W/m2)
	// DERIVED TYPE DEFINITIONS

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:
	int NumPVs( 0 ); // count of number of PV generators
	int Num1DiodePVModuleTypes( 0 ); // count for Equivalent one-diode model
	int NumSimplePVModuleTypes( 0 ); // count of number of input objs for simple model
	int NumSNLPVModuleTypes( 0 ); // count of number of input objs for Sandia model

	Real64 ShuntResistance( 0.0 ); // old "RSH" in common block of trnsys code

	// Object Data
	Array1D< PVArrayStruct > PVarray;

	// ___________________________________________________________________________

	//     EnergyPlus V1.2 and beyond include models for photovoltaic calculations called
	//     Generator:Photovoltaic:Simple and Generator:PV:Sandia implemented by the Center for
	//     Buildings and Thermal Systems, National Renewable Energy Laboratory, 1617 Cole Blvd
	//     MS 2722, Golden, CO, 80401
	//     EnergyPlus v1.1.1 and beyond includes model for Photovoltaic calculations, now
	//     referred to as the Generator:PV:Equivalent One-Diode model developed by Thermal Energy
	//     System Specialists, 2916 Marketplace Drive, Suite 104, Madison, WI 53719;
	//     Tel: (608) 274-2577

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

} // DataPhotovoltaics

} // EnergyPlus
