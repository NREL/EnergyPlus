// EnergyPlus Headers
#include <DataPlantPipingSystems.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataPlantPipingSystems {

	// Module containing the data structures dealing with the PlantPipingSystems

	// MODULE INFORMATION:
	//       AUTHOR         Edwin Lee
	//       DATE WRITTEN   Summer 2011
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// Contains all the data structures for PlantPipingSystems

	// METHODOLOGY EMPLOYED:
	// A pseudo-object-oriented approach is taken to use inheritance in the structure.
	// For example, an abstract base cell class is defined with temperatures and other
	//  generic properties, then different cell types inherit from this by including it
	//  as a MyBase field within its own structure.  Not exactly OO inheritance, but
	//  it's close, and it increases code reuse, that's for sure!
	// Enumerations are defined first with an EnumClassName_InstanceName format

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using DataGlobals::Pi;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	int const PartitionType_BasementWall( -1 );
	int const PartitionType_BasementFloor( -2 );
	int const PartitionType_Pipe( -3 );
	int const PartitionType_Slab( -4 );
	int const PartitionType_XSide( -5 );
	int const PartitionType_XSideWall( -6 );
	int const PartitionType_ZSide( -7 );
	int const PartitionType_ZSideWall( -8 );
	int const PartitionType_FloorInside( -9 );
	int const PartitionType_UnderFloor( -10 );
	int const PartitionType_HorizInsXSide( 11 );
	int const PartitionType_VertInsLowerEdge( -12 );
	int const PartitionType_HorizInsZSide( -13 );

	int const RegionType_Pipe( -1 );
	int const RegionType_BasementWall( -2 );
	int const RegionType_BasementFloor( -3 );
	int const RegionType_XDirection( -4 );
	int const RegionType_YDirection( -5 );
	int const RegionType_ZDirection( -6 );
	int const RegionType_XSide( -7 );
	int const RegionType_XSideWall( -8 );
	int const RegionType_ZSide( -9 );
	int const RegionType_ZSideWall( -10 );
	int const RegionType_FloorInside( -11 );
	int const RegionType_UnderFloor( -12 );
	int const RegionType_HorizInsXSide( -13 );
	int const RegionType_HorizInsZSide( -14 );
	int const RegionType_VertInsLowerEdge( -15 );

	int const MeshDistribution_Uniform( -1 );
	int const MeshDistribution_SymmetricGeometric( -2 );

	int const SegmentFlow_IncreasingZ( -1 );
	int const SegmentFlow_DecreasingZ( -2 );

	int const Direction_PositiveY( -1 );
	int const Direction_NegativeY( -2 );
	int const Direction_PositiveX( -3 );
	int const Direction_NegativeX( -4 );
	int const Direction_PositiveZ( -5 );
	int const Direction_NegativeZ( -6 );

	int const CellType_Unknown( -1 );
	int const CellType_Pipe( -2 );
	int const CellType_GeneralField( -3 );
	int const CellType_GroundSurface( -4 );
	int const CellType_FarfieldBoundary( -5 );
	int const CellType_AdiabaticWall( -6 );
	int const CellType_BasementWall( -7 );
	int const CellType_BasementFloor( -8 );
	int const CellType_BasementCorner( -9 );
	int const CellType_BasementCutaway( -10 );
	int const CellType_Slab( -11 );
	int const CellType_HorizInsulation( -12 );
	int const CellType_VertInsulation( -13 );
	int const CellType_ZoneGroundInterface( -14 );
	int const CellType_BasementWallInsu( -15 );
	int const CellType_BasementFloorInsu( -16 );

	// DERIVED TYPE DEFINITIONS:

	//Input data structure

	// Internal structure

	//Simulation data structures

	// 'Current' data structure for variables, this is one-per-domain

	// MODULE VARIABLE DECLARATIONS:

	// Object Data
	Array1D< FullDomainStructureInfo > PipingSystemDomains;
	Array1D< PipeCircuitInfo > PipingSystemCircuits;
	Array1D< PipeSegmentInfo > PipingSystemSegments;

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

} // DataPlantPipingSystems

} // EnergyPlus
