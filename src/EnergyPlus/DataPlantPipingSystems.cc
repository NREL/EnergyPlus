// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

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
	int const PartitionType_HorizInsXSide( -11 );
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

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	clear_state()
	{
		PipingSystemDomains.deallocate();
		PipingSystemCircuits.deallocate();
		PipingSystemSegments.deallocate();
	}


} // DataPlantPipingSystems

} // EnergyPlus
