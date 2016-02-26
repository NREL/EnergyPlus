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

#ifndef DataPlantPipingSystems_hh_INCLUDED
#define DataPlantPipingSystems_hh_INCLUDED

// C++ Headers
#include <memory>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array3D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <GroundTemperatureModeling/GroundTemperatureModelManager.hh>

namespace EnergyPlus {

namespace DataPlantPipingSystems {

	// Using/Aliasing
	using namespace GroundTemperatureManager;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern int const PartitionType_BasementWall;
	extern int const PartitionType_BasementFloor;
	extern int const PartitionType_Pipe;
	extern int const PartitionType_Slab;
	extern int const PartitionType_XSide;
	extern int const PartitionType_XSideWall;
	extern int const PartitionType_ZSide;
	extern int const PartitionType_ZSideWall;
	extern int const PartitionType_FloorInside;
	extern int const PartitionType_UnderFloor;
	extern int const PartitionType_HorizInsXSide;
	extern int const PartitionType_HorizInsZSide;
	extern int const PartitionType_VertInsLowerEdge;


	extern int const RegionType_Pipe;
	extern int const RegionType_BasementWall;
	extern int const RegionType_BasementFloor;
	extern int const RegionType_XDirection;
	extern int const RegionType_YDirection;
	extern int const RegionType_ZDirection;
	extern int const RegionType_XSide;
	extern int const RegionType_XSideWall;
	extern int const RegionType_ZSide;
	extern int const RegionType_ZSideWall;
	extern int const RegionType_FloorInside;
	extern int const RegionType_UnderFloor;
	extern int const RegionType_HorizInsXSide;
	extern int const RegionType_HorizInsZSide;
	extern int const RegionType_VertInsLowerEdge;

	extern int const MeshDistribution_Uniform;
	extern int const MeshDistribution_SymmetricGeometric;

	extern int const SegmentFlow_IncreasingZ;
	extern int const SegmentFlow_DecreasingZ;

	extern int const Direction_PositiveY;
	extern int const Direction_NegativeY;
	extern int const Direction_PositiveX;
	extern int const Direction_NegativeX;
	extern int const Direction_PositiveZ;
	extern int const Direction_NegativeZ;

	extern int const CellType_Unknown;
	extern int const CellType_Pipe;
	extern int const CellType_GeneralField;
	extern int const CellType_GroundSurface;
	extern int const CellType_FarfieldBoundary;
	extern int const CellType_AdiabaticWall;
	extern int const CellType_BasementWall;
	extern int const CellType_BasementFloor;
	extern int const CellType_BasementCorner;
	extern int const CellType_BasementCutaway;
	extern int const CellType_Slab;
	extern int const CellType_HorizInsulation;
	extern int const CellType_VertInsulation;
	extern int const CellType_ZoneGroundInterface;
	extern int const CellType_BasementWallInsu;
	extern int const CellType_BasementFloorInsu;

	// DERIVED TYPE DEFINITIONS:

	//Input data structure

	// Internal structure

	//Simulation data structures

	// 'Current' data structure for variables, this is one-per-domain

	// MODULE VARIABLE DECLARATIONS:

	// Types

	struct BaseThermalPropertySet
	{
		// Members
		Real64 Conductivity; // W/mK
		Real64 Density; // kg/m3
		Real64 SpecificHeat; // J/kgK

		// Default Constructor
		BaseThermalPropertySet() :
			Conductivity( 0.0 ),
			Density( 0.0 ),
			SpecificHeat( 0.0 )
		{}

		// Member Constructor
		BaseThermalPropertySet(
			Real64 const Conductivity, // W/mK
			Real64 const Density, // kg/m3
			Real64 const SpecificHeat // J/kgK
		) :
			Conductivity( Conductivity ),
			Density( Density ),
			SpecificHeat( SpecificHeat )
		{}

	};

	struct ExtendedFluidProperties // : Inherits BaseThermalPropertySet
	{
		// Members
		BaseThermalPropertySet MyBase;
		Real64 Viscosity; // kg/m-s
		Real64 Prandtl; // -

		// Default Constructor
		ExtendedFluidProperties()
		{}

		// Member Constructor
		ExtendedFluidProperties(
			BaseThermalPropertySet const & MyBase,
			Real64 const Viscosity, // kg/m-s
			Real64 const Prandtl // -
		) :
			MyBase( MyBase ),
			Viscosity( Viscosity ),
			Prandtl( Prandtl )
		{}

	};

	struct BaseCell
	{
		// Members
		Real64 Temperature; // C
		Real64 Temperature_PrevIteration; // C
		Real64 Temperature_PrevTimeStep; // C
		Real64 Beta; // K/W
		BaseThermalPropertySet Properties;

		// Default Constructor
		BaseCell() :
			Temperature( 0.0 ),
			Temperature_PrevIteration( 0.0 ),
			Temperature_PrevTimeStep( 0.0 ),
			Beta( 0.0 )
		{}

	};

	struct RadialCellInformation // : Inherits BaseCell
	{
		// Members
		BaseCell MyBase;
		Real64 RadialCentroid;
		Real64 InnerRadius;
		Real64 OuterRadius;

		// Default Constructor
		RadialCellInformation()
		{}

	};

	struct FluidCellInformation // : Inherits BaseCell
	{
		// Members
		BaseCell MyBase;
		Real64 PipeInnerRadius;
		Real64 Volume;
		ExtendedFluidProperties Properties;

		// Default Constructor
		FluidCellInformation()
		{}

	};

	struct CartesianPipeCellInformation // Specialized cell information only used by cells which contain pipes
	{
		// Members
		Array1D< RadialCellInformation > Soil;
		RadialCellInformation Insulation;
		RadialCellInformation Pipe;
		FluidCellInformation Fluid;
		Real64 RadialSliceWidth;
		Real64 InterfaceVolume;

		// Default Constructor
		CartesianPipeCellInformation()
		{}

	};

	struct Point
	{
		// Members
		int X;
		int Y;

		// Default Constructor
		Point()
		{}

		// Member Constructor
		Point(
			int const X,
			int const Y
		) :
			X( X ),
			Y( Y )
		{}

	};

	struct PointF
	{
		// Members
		Real64 X;
		Real64 Y;

		// Default Constructor
		PointF()
		{}

		// Member Constructor
		PointF(
			Real64 const X,
			Real64 const Y
		) :
			X( X ),
			Y( Y )
		{}

	};

	struct Point3DInteger
	{
		// Members
		int X;
		int Y;
		int Z;

		// Default Constructor
		Point3DInteger()
		{}

		// Member Constructor
		Point3DInteger(
			int const X,
			int const Y,
			int const Z
		) :
			X( X ),
			Y( Y ),
			Z( Z )
		{}

	};

	struct Point3DReal
	{
		// Members
		Real64 X;
		Real64 Y;
		Real64 Z;

		// Default Constructor
		Point3DReal()
		{}

		// Member Constructor
		Point3DReal(
			Real64 const X,
			Real64 const Y,
			Real64 const Z
		) :
			X( X ),
			Y( Y ),
			Z( Z )
		{}

	};

	struct DomainRectangle
	{
		// Members
		int XMin;
		int XMax;
		int YMin;
		int YMax;

		// Default Constructor
		DomainRectangle()
		{}

		// Member Constructor
		DomainRectangle(
			int const XMin,
			int const XMax,
			int const YMin,
			int const YMax
		) :
			XMin( XMin ),
			XMax( XMax ),
			YMin( YMin ),
			YMax( YMax )
		{}

	};

	struct MeshPartition
	{
		// Members
		Real64 rDimension;
		int PartitionType; // From Enum: ParitionType
		Real64 TotalWidth;

		// Default Constructor
		MeshPartition()
		{}

		// Member Constructor
		MeshPartition(
			Real64 const rDimension,
			int const PartitionType, // From Enum: ParitionType
			Real64 const TotalWidth
		) :
			rDimension( rDimension ),
			PartitionType( PartitionType ),
			TotalWidth( TotalWidth )
		{}

	};

	struct GridRegion
	{
		// Members
		Real64 Min;
		Real64 Max;
		int RegionType; // From Enum: RegionType
		Array1D< Real64 > CellWidths;

		// Default Constructor
		GridRegion()
		{}

	};

	struct TempGridRegionData
	{
		// Members
		Real64 Min;
		Real64 Max;
		int RegionType; // From Enum: RegionType

		// Default Constructor
		TempGridRegionData()
		{}

		// Member Constructor
		TempGridRegionData(
			Real64 const Min,
			Real64 const Max,
			int const RegionType // From Enum: RegionType
		) :
			Min( Min ),
			Max( Max ),
			RegionType( RegionType )
		{}

	};

	struct RectangleF
	{
		// Members
		Real64 X_min;
		Real64 Y_min;
		Real64 Width;
		Real64 Height;

		// Default Constructor
		RectangleF()
		{}

		// Member Constructor
		RectangleF(
			Real64 const X_min,
			Real64 const Y_min,
			Real64 const Width,
			Real64 const Height
		) :
			X_min( X_min ),
			Y_min( Y_min ),
			Width( Width ),
			Height( Height )
		{}

	};

	struct NeighborInformation
	{
		// Members
		Real64 ThisCentroidToNeighborCentroid;
		Real64 ThisCentroidToNeighborWall;
		Real64 ThisWallToNeighborCentroid;
		Real64 ConductionResistance;
		Point3DInteger NeighborCellIndeces;

		// Default Constructor
		NeighborInformation()
		{}

	};

	struct RadialSizing
	{
		// Members
		Real64 InnerDia;
		Real64 OuterDia;

		// Default Constructor
		RadialSizing()
		{}

	};

	struct DirectionNeighbor_Dictionary
	{
		// Members
		int Direction; // From Enum: Direction
		NeighborInformation Value;

		// Default Constructor
		DirectionNeighbor_Dictionary()
		{}

	};

	struct CartesianCell
	{
		// Members
		BaseCell MyBase;
		int X_index;
		int Y_index;
		int Z_index;
		Real64 X_min;
		Real64 X_max;
		Real64 Y_min;
		Real64 Y_max;
		Real64 Z_min;
		Real64 Z_max;
		Point3DReal Centroid;
		int CellType; // From Enum: CellType
		int PipeIndex;
		Array1D< DirectionNeighbor_Dictionary > NeighborInformation;
		CartesianPipeCellInformation PipeCellData;

		// Default Constructor
		CartesianCell()
		{}

	};

	struct MeshExtents
	{
		// Members
		Real64 Xmax;
		Real64 Ymax;
		Real64 Zmax;

		// Default Constructor
		MeshExtents() :
			Xmax( 0.0 ),
			Ymax( 0.0 ),
			Zmax( 0.0 )
		{}

		// Member Constructor
		MeshExtents(
			Real64 const Xmax,
			Real64 const Ymax,
			Real64 const Zmax
		) :
			Xmax( Xmax ),
			Ymax( Ymax ),
			Zmax( Zmax )
		{}

	};

	struct DistributionStructure
	{
		// Members
		int MeshDistribution; // From Enum: MeshDistribution
		int RegionMeshCount;
		Real64 GeometricSeriesCoefficient;

		// Default Constructor
		DistributionStructure() :
			MeshDistribution( 0 ),
			RegionMeshCount( 0 ),
			GeometricSeriesCoefficient( 0.0 )
		{}

	};

	struct MeshProperties
	{
		// Members
		DistributionStructure X;
		DistributionStructure Y;
		DistributionStructure Z;

		// Default Constructor
		MeshProperties()
		{}

	};

	struct SimulationControl
	{
		// Members
		Real64 MinimumTemperatureLimit;
		Real64 MaximumTemperatureLimit;
		Real64 Convergence_CurrentToPrevIteration;
		int MaxIterationsPerTS;

		// Default Constructor
		SimulationControl() :
			MinimumTemperatureLimit( -1000.0 ),
			MaximumTemperatureLimit( 1000.0 ),
			Convergence_CurrentToPrevIteration( 0.0 ),
			MaxIterationsPerTS( 0 )
		{}

	};

	struct FarfieldInfo
	{
		// Members
		std::shared_ptr< BaseGroundTempsModel > groundTempModel;
	};

	struct BasementZoneInfo
	{
		// Members
		Real64 Depth; // m
		Real64 Width; // m
		Real64 Length; // m
		bool ShiftPipesByWidth;
		std::string WallBoundaryOSCMName;
		int WallBoundaryOSCMIndex;
		std::string FloorBoundaryOSCMName;
		int FloorBoundaryOSCMIndex;
		Array1D_int WallSurfacePointers;
		Array1D_int FloorSurfacePointers;
		int BasementWallXIndex;
		int BasementFloorYIndex;

		// Default Constructor
		BasementZoneInfo() :
			Depth( 0.0 ),
			Width( 0.0 ),
			Length( 0.0 ),
			ShiftPipesByWidth( false ),
			WallBoundaryOSCMIndex( 0 ),
			FloorBoundaryOSCMIndex( 0 ),
			BasementWallXIndex( -1 ),
			BasementFloorYIndex( -1 )
		{}

	};

	struct DirectionReal_Dictionary
	{
		// Members
		int Direction; // From Enum: Direction
		Real64 Value;

		// Default Constructor
		DirectionReal_Dictionary() :
			Direction( 0 ),
			Value( 0.0 )
		{}

	};

	struct ReportingInformation
	{
		// Members
		Array1D< DirectionReal_Dictionary > SurfaceHeatTransfer;
		Real64 TotalBoundaryHeatTransfer;
		Real64 EnergyStoredInCells;
		Real64 AverageSurfaceTemperature;
		Real64 PipeCircuitHeatTransferMCpDT;
		Real64 PipeCircuitHeatTransferUADT;
		Real64 BasementWallHeatTransfer;
		Real64 BasementFloorHeatTransfer;
		Real64 AverageBasementFloorTemperature;
		Real64 AverageBasementWallTemperature;

		// Default Constructor
		ReportingInformation() :
			TotalBoundaryHeatTransfer( 0.0 ),
			EnergyStoredInCells( 0.0 ),
			AverageSurfaceTemperature( 0.0 ),
			PipeCircuitHeatTransferMCpDT( 0.0 ),
			PipeCircuitHeatTransferUADT( 0.0 ),
			BasementWallHeatTransfer( 0.0 ),
			BasementFloorHeatTransfer( 0.0 ),
			AverageBasementFloorTemperature( 0.0 ),
			AverageBasementWallTemperature( 0.0 )
		{}

	};

	struct MeshPartitions
	{
		// Members
		Array1D< MeshPartition > X;
		Array1D< MeshPartition > Y;
		Array1D< MeshPartition > Z;

		// Default Constructor
		MeshPartitions()
		{}

	};

	struct MoistureInfo
	{
		// Members
		Real64 Theta_liq; // volumetric moisture content of the soil
		Real64 Theta_sat; // volumetric moisture content of soil at saturation
		Real64 GroundCoverCoefficient;

		// Default Constructor
		MoistureInfo() :
			Theta_liq( 0.3 ),
			Theta_sat( 0.5 ),
			GroundCoverCoefficient( 0.408 )
		{}

	};

	struct CurSimConditionsInfo
	{
		// Members
		//Simulation conditions
		Real64 PrevSimTimeSeconds;
		Real64 CurSimTimeSeconds;
		Real64 CurSimTimeStepSize;
		//Environmental conditions
		Real64 CurAirTemp;
		Real64 CurWindSpeed;
		Real64 CurIncidentSolar;
		Real64 CurRelativeHumidity;

		// Default Constructor
		CurSimConditionsInfo() :
			PrevSimTimeSeconds( -1.0 ),
			CurSimTimeSeconds( 0.0 ),
			CurSimTimeStepSize( 0.0 ),
			CurAirTemp( 10.0 ),
			CurWindSpeed( 2.6 ),
			CurIncidentSolar( 0.0 ),
			CurRelativeHumidity( 100.0 )
		{}

	};

	struct PipeSegmentInfo
	{
		// Members
		// ID
		std::string Name;
		// Misc inputs
		PointF PipeLocation;
		Point PipeCellCoordinates;
		int FlowDirection; // From Enum: SegmentFlow
		// Pointer to parent pipe circuit
		int ParentCircuitIndex;
		// Reporting variables
		Real64 InletTemperature;
		Real64 OutletTemperature;
		Real64 FluidHeatLoss;
		// Error handling flags
		bool PipeCellCoordinatesSet;
		// Other flags
		bool IsActuallyPartOfAHorizontalTrench;

		// Default Constructor
		PipeSegmentInfo() :
			FlowDirection( 0 ),
			ParentCircuitIndex( 0 ),
			InletTemperature( 0.0 ),
			OutletTemperature( 0.0 ),
			FluidHeatLoss( 0.0 ),
			PipeCellCoordinatesSet( false ),
			IsActuallyPartOfAHorizontalTrench( false )
		{}

	};

	struct PipeCircuitInfo
	{
		// Members
		// ID
		std::string Name;
		// Inlet and outlet information
		std::string InletNodeName;
		std::string OutletNodeName;
		int InletNodeNum;
		int OutletNodeNum;
		Point3DInteger CircuitInletCell;
		Point3DInteger CircuitOutletCell;
		// Names and pointers to pipe segments found in this pipe circuit
		Array1D_string PipeSegmentNames;
		Array1D_int PipeSegmentIndeces;
		// Pointer to the domain which contains this pipe circuit
		int ParentDomainIndex;
		// Misc inputs
		RadialSizing PipeSize;
		RadialSizing InsulationSize;
		Real64 RadialMeshThickness;
		bool HasInsulation;
		Real64 DesignVolumeFlowRate;
		Real64 DesignMassFlowRate;
		Real64 Convergence_CurrentToPrevIteration;
		int MaxIterationsPerTS;
		int NumRadialCells;
		BaseThermalPropertySet PipeProperties;
		BaseThermalPropertySet InsulationProperties;
		// A list of 3d cell indices that span the entire length of this pipe circuit (useful for reporting)
		Array1D< Point3DInteger > ListOfCircuitPoints;
		// Flags
		bool CheckEquipName;
		bool NeedToFindOnPlantLoop;
		bool IsActuallyPartOfAHorizontalTrench;
		// Location of this pipe circuit in the PlantLoop topology
		int LoopNum;
		int LoopSideNum;
		int BranchNum;
		int CompNum;
		// Current fluid property values
		Real64 CurFluidDensity;
		Real64 CurFluidViscosity;
		Real64 CurFluidConductivity;
		Real64 CurFluidPrandtl;
		Real64 CurFluidSpecificHeat;
		ExtendedFluidProperties CurFluidPropertySet;
		// Variables used to pass information from INIT-type routines to CALC-type routines
		Real64 CurCircuitInletTemp;
		Real64 CurCircuitFlowRate;
		Real64 CurCircuitConvectionCoefficient;
		// Reporting variables
		Real64 InletTemperature;
		Real64 OutletTemperature;
		Real64 FluidHeatLoss;

		// Default Constructor
		PipeCircuitInfo() :
			InletNodeNum( 0 ),
			OutletNodeNum( 0 ),
			ParentDomainIndex( 0 ),
			RadialMeshThickness( 0.0 ),
			HasInsulation( false ),
			DesignVolumeFlowRate( 0.0 ),
			DesignMassFlowRate( 0.0 ),
			Convergence_CurrentToPrevIteration( 0.0 ),
			MaxIterationsPerTS( 0 ),
			NumRadialCells( 0 ),
			CheckEquipName( true ),
			NeedToFindOnPlantLoop( true ),
			IsActuallyPartOfAHorizontalTrench( false ),
			LoopNum( 0 ),
			LoopSideNum( 0 ),
			BranchNum( 0 ),
			CompNum( 0 ),
			CurFluidDensity( 998.0 ),
			CurFluidViscosity( 0.0015 ),
			CurFluidConductivity( 0.58 ),
			CurFluidPrandtl( 7.0 ),
			CurFluidSpecificHeat( 4190.0 ),
			CurCircuitInletTemp( 23.0 ),
			CurCircuitFlowRate( 0.1321 ),
			CurCircuitConvectionCoefficient( 0.0 ),
			InletTemperature( 0.0 ),
			OutletTemperature( 0.0 ),
			FluidHeatLoss( 0.0 )
		{}

	};

	struct ZoneCoupledSurfaceData
	{
		// Members
		// ID
		std::string Name;
		// Surface data
		int IndexInSurfaceArray;
		Real64 SurfaceArea;
		Real64 Width;
		Real64 Length;
		Real64 Depth;
		Real64 Conductivity;
		Real64 Density;
		Real64 InsulationConductivity;
		Real64 InsulationDensity;

		// Default Constructor
		ZoneCoupledSurfaceData() :
			IndexInSurfaceArray( 0 ),
			SurfaceArea( 0.0 ),
			Width( 0.0 ),
			Length( 0.0 ),
			Depth( 0.0 ),
			Conductivity( 0.0 ),
			Density( 0.0 ),
			InsulationConductivity( 0.0 ),
			InsulationDensity( 0.0 )

		{}

	};

	struct FullDomainStructureInfo
	{
		// Members
		// ID
		std::string Name;
		// Names and pointers to circuits found in this domain
		Array1D_string CircuitNames;
		Array1D_int CircuitIndeces;
		int MaxIterationsPerTS;
		// Flag variables
		bool OneTimeInit;
		bool BeginSimInit;
		bool BeginSimEnvrn;
		bool DomainNeedsSimulation;
		bool DomainNeedsToBeMeshed;
		bool IsActuallyPartOfAHorizontalTrench;
		bool HasAPipeCircuit;
		bool IsZoneCoupledSlab;
		bool HasCoupledBasement;
		// "Input" data structure variables
		MeshExtents Extents;
		MeshProperties Mesh;
		BaseThermalPropertySet GroundProperties;
		BaseThermalPropertySet SlabProperties;
		BaseThermalPropertySet BasementInterfaceProperties;
		BaseThermalPropertySet HorizInsProperties;
		BaseThermalPropertySet VertInsProperties;
		SimulationControl SimControls;
		FarfieldInfo Farfield;
		BasementZoneInfo BasementZone;
		MoistureInfo Moisture;
		// "Internal" data structure variables
		MeshPartitions Partitions;
		CurSimConditionsInfo Cur;
		ReportingInformation Reporting;
		bool HasBasement;
		// Zone coupled variables
		Array1D <ZoneCoupledSurfaceData> ZoneCoupledSurfaces;
		int ZoneCoupledOSCMIndex;
		Real64 PerimeterOffset;
		bool SlabInGradeFlag;
		int SlabMaterialNum;
		Real64 SlabWidth;
		Real64 SlabLength;
		Real64 SlabThickness;
		Real64 XIndex;
		Real64 YIndex;
		Real64 ZIndex;
		bool HorizInsPresentFlag;
		int HorizInsMaterialNum;
		Real64 HorizInsThickness;
		Real64 HorizInsWidth;
		Real64 HeatFlux;
		Real64 WallHeatFlux;
		Real64 FloorHeatFlux;
		Real64 AggregateHeatFlux;
		Real64 AggregateWallHeatFlux;
		Real64 AggregateFloorHeatFlux;
		int NumHeatFlux;
		bool ResetHeatFluxFlag;
		Real64 ConvCoeff;
		bool FullHorizInsPresent;
		bool VertInsPresentFlag;
		int VertInsMaterialNum;
		Real64 VertInsThickness;
		Real64 VertInsDepth;
		int XWallIndex;
		int YFloorIndex;
		int ZWallIndex;
		int InsulationXIndex;
		int InsulationYIndex;
		int InsulationZIndex;
		bool SimTimestepFlag;
		bool SimHourlyFlag;
		Real64 ZoneCoupledSurfaceTemp;
		Real64 BasementWallTemp;
		Real64 BasementFloorTemp;
		int NumDomainCells;
		int NumGroundSurfCells;
		int NumInsulationCells;

		// Main 3D cells array
		Array3D< CartesianCell > Cells;

		// Default Constructor
		FullDomainStructureInfo() :
			MaxIterationsPerTS( 10 ),
			OneTimeInit( true ),
			BeginSimInit( true ),
			BeginSimEnvrn( true ),
			DomainNeedsSimulation( true ),
			DomainNeedsToBeMeshed( true ),
			IsActuallyPartOfAHorizontalTrench( false ),
			HasAPipeCircuit( true ),
			IsZoneCoupledSlab( false ),
			HasCoupledBasement( false ),
			HasBasement( false ),
			ZoneCoupledOSCMIndex( 0 ),
			PerimeterOffset( 0.0 ),
			SlabInGradeFlag( false ),
			SlabMaterialNum( 0 ),
			SlabWidth( 0.0 ),
			SlabLength( 0.0 ),
			SlabThickness( 0.0 ),
			XIndex( 0 ),
			YIndex( 0 ),
			ZIndex( 0 ),
			HorizInsPresentFlag( false ),
			HorizInsMaterialNum( 0 ),
			HorizInsThickness( 0.0254 ),
			HorizInsWidth( 0.0 ),
			HeatFlux( 0.0 ),
			WallHeatFlux( 0.0 ),
			FloorHeatFlux( 0.0 ),
			AggregateHeatFlux( 0.0 ),
			AggregateWallHeatFlux( 0.0 ),
			AggregateFloorHeatFlux( 0.0 ),
			NumHeatFlux ( 0 ),
			ResetHeatFluxFlag( true ),
			ConvCoeff( 0.0 ),
			FullHorizInsPresent( false ),
			VertInsPresentFlag( false ),
			VertInsMaterialNum( 0 ),
			VertInsThickness( 0.0254 ),
			VertInsDepth( 0.0 ),
			XWallIndex( 0 ),
			YFloorIndex( 0 ),
			ZWallIndex( 0 ),
			InsulationXIndex( 0 ),
			InsulationYIndex( 0 ),
			InsulationZIndex( 0 ),
			SimTimestepFlag( false ),
			SimHourlyFlag( false ),
			ZoneCoupledSurfaceTemp( 0.0 ),
			BasementWallTemp( 0.0 ),
			BasementFloorTemp( 0.0 ),
			NumDomainCells ( 0 ),
			NumGroundSurfCells( 0 ),
			NumInsulationCells( 0 )

		{}

	};

	// Object Data
	extern Array1D< FullDomainStructureInfo > PipingSystemDomains;
	extern Array1D< PipeCircuitInfo > PipingSystemCircuits;
	extern Array1D< PipeSegmentInfo > PipingSystemSegments;

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	clear_state();

} // DataPlantPipingSystems

} // EnergyPlus

#endif
