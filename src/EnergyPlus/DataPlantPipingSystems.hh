#ifndef DataPlantPipingSystems_hh_INCLUDED
#define DataPlantPipingSystems_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array3D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DataPlantPipingSystems {

	// Using/Aliasing

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

		// Member Constructor
		BaseCell(
			Real64 const Temperature, // C
			Real64 const Temperature_PrevIteration, // C
			Real64 const Temperature_PrevTimeStep, // C
			Real64 const Beta, // K/W
			BaseThermalPropertySet const & Properties
		) :
			Temperature( Temperature ),
			Temperature_PrevIteration( Temperature_PrevIteration ),
			Temperature_PrevTimeStep( Temperature_PrevTimeStep ),
			Beta( Beta ),
			Properties( Properties )
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

		// Member Constructor
		RadialCellInformation(
			BaseCell const & MyBase,
			Real64 const RadialCentroid,
			Real64 const InnerRadius,
			Real64 const OuterRadius
		) :
			MyBase( MyBase ),
			RadialCentroid( RadialCentroid ),
			InnerRadius( InnerRadius ),
			OuterRadius( OuterRadius )
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

		// Member Constructor
		FluidCellInformation(
			BaseCell const & MyBase,
			Real64 const PipeInnerRadius,
			Real64 const Volume,
			ExtendedFluidProperties const & Properties
		) :
			MyBase( MyBase ),
			PipeInnerRadius( PipeInnerRadius ),
			Volume( Volume ),
			Properties( Properties )
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

		// Member Constructor
		CartesianPipeCellInformation(
			Array1< RadialCellInformation > const & Soil,
			RadialCellInformation const & Insulation,
			RadialCellInformation const & Pipe,
			FluidCellInformation const & Fluid,
			Real64 const RadialSliceWidth,
			Real64 const InterfaceVolume
		) :
			Soil( Soil ),
			Insulation( Insulation ),
			Pipe( Pipe ),
			Fluid( Fluid ),
			RadialSliceWidth( RadialSliceWidth ),
			InterfaceVolume( InterfaceVolume )
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

		// Member Constructor
		GridRegion(
			Real64 const Min,
			Real64 const Max,
			int const RegionType, // From Enum: RegionType
			Array1< Real64 > const & CellWidths
		) :
			Min( Min ),
			Max( Max ),
			RegionType( RegionType ),
			CellWidths( CellWidths )
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

		// Member Constructor
		NeighborInformation(
			Real64 const ThisCentroidToNeighborCentroid,
			Real64 const ThisCentroidToNeighborWall,
			Real64 const ThisWallToNeighborCentroid,
			Real64 const ConductionResistance,
			Point3DInteger const & NeighborCellIndeces
		) :
			ThisCentroidToNeighborCentroid( ThisCentroidToNeighborCentroid ),
			ThisCentroidToNeighborWall( ThisCentroidToNeighborWall ),
			ThisWallToNeighborCentroid( ThisWallToNeighborCentroid ),
			ConductionResistance( ConductionResistance ),
			NeighborCellIndeces( NeighborCellIndeces )
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

		// Member Constructor
		RadialSizing(
			Real64 const InnerDia,
			Real64 const OuterDia
		) :
			InnerDia( InnerDia ),
			OuterDia( OuterDia )
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

		// Member Constructor
		DirectionNeighbor_Dictionary(
			int const Direction, // From Enum: Direction
			NeighborInformation const & Value
		) :
			Direction( Direction ),
			Value( Value )
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

		// Member Constructor
		CartesianCell(
			BaseCell const & MyBase,
			int const X_index,
			int const Y_index,
			int const Z_index,
			Real64 const X_min,
			Real64 const X_max,
			Real64 const Y_min,
			Real64 const Y_max,
			Real64 const Z_min,
			Real64 const Z_max,
			Point3DReal const & Centroid,
			int const CellType, // From Enum: CellType
			int const PipeIndex,
			Array1< DirectionNeighbor_Dictionary > const & NeighborInformation,
			CartesianPipeCellInformation const & PipeCellData
		) :
			MyBase( MyBase ),
			X_index( X_index ),
			Y_index( Y_index ),
			Z_index( Z_index ),
			X_min( X_min ),
			X_max( X_max ),
			Y_min( Y_min ),
			Y_max( Y_max ),
			Z_min( Z_min ),
			Z_max( Z_max ),
			Centroid( Centroid ),
			CellType( CellType ),
			PipeIndex( PipeIndex ),
			NeighborInformation( NeighborInformation ),
			PipeCellData( PipeCellData )
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

		// Member Constructor
		DistributionStructure(
			int const MeshDistribution, // From Enum: MeshDistribution
			int const RegionMeshCount,
			Real64 const GeometricSeriesCoefficient
		) :
			MeshDistribution( MeshDistribution ),
			RegionMeshCount( RegionMeshCount ),
			GeometricSeriesCoefficient( GeometricSeriesCoefficient )
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

		// Member Constructor
		MeshProperties(
			DistributionStructure const & X,
			DistributionStructure const & Y,
			DistributionStructure const & Z
		) :
			X( X ),
			Y( Y ),
			Z( Z )
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

		// Member Constructor
		SimulationControl(
			Real64 const MinimumTemperatureLimit,
			Real64 const MaximumTemperatureLimit,
			Real64 const Convergence_CurrentToPrevIteration,
			int const MaxIterationsPerTS
		) :
			MinimumTemperatureLimit( MinimumTemperatureLimit ),
			MaximumTemperatureLimit( MaximumTemperatureLimit ),
			Convergence_CurrentToPrevIteration( Convergence_CurrentToPrevIteration ),
			MaxIterationsPerTS( MaxIterationsPerTS )
		{}

	};

	struct FarfieldInfo
	{
		// Members
		Real64 AverageGroundTemperature; // C
		Real64 AverageGroundTemperatureAmplitude; // C
		Real64 PhaseShiftOfMinGroundTempDays; // days
		Real64 PhaseShiftOfMinGroundTemp; // seconds

		// Default Constructor
		FarfieldInfo() :
			AverageGroundTemperature( 0.0 ),
			AverageGroundTemperatureAmplitude( 0.0 ),
			PhaseShiftOfMinGroundTempDays( 0.0 ),
			PhaseShiftOfMinGroundTemp( 0.0 )
		{}

		// Member Constructor
		FarfieldInfo(
			Real64 const AverageGroundTemperature, // C
			Real64 const AverageGroundTemperatureAmplitude, // C
			Real64 const PhaseShiftOfMinGroundTempDays, // days
			Real64 const PhaseShiftOfMinGroundTemp // seconds
		) :
			AverageGroundTemperature( AverageGroundTemperature ),
			AverageGroundTemperatureAmplitude( AverageGroundTemperatureAmplitude ),
			PhaseShiftOfMinGroundTempDays( PhaseShiftOfMinGroundTempDays ),
			PhaseShiftOfMinGroundTemp( PhaseShiftOfMinGroundTemp )
		{}

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

		// Member Constructor
		BasementZoneInfo(
			Real64 const Depth, // m
			Real64 const Width, // m
			Real64 const Length, // m
			bool const ShiftPipesByWidth,
			std::string const & WallBoundaryOSCMName,
			int const WallBoundaryOSCMIndex,
			std::string const & FloorBoundaryOSCMName,
			int const FloorBoundaryOSCMIndex,
			Array1_int const & WallSurfacePointers,
			Array1_int const & FloorSurfacePointers,
			int const BasementWallXIndex,
			int const BasementFloorYIndex
		) :
			Depth( Depth ),
			Width( Width ),
			Length( Length ),
			ShiftPipesByWidth( ShiftPipesByWidth ),
			WallBoundaryOSCMName( WallBoundaryOSCMName ),
			WallBoundaryOSCMIndex( WallBoundaryOSCMIndex ),
			FloorBoundaryOSCMName( FloorBoundaryOSCMName ),
			FloorBoundaryOSCMIndex( FloorBoundaryOSCMIndex ),
			WallSurfacePointers( WallSurfacePointers ),
			FloorSurfacePointers( FloorSurfacePointers ),
			BasementWallXIndex( BasementWallXIndex ),
			BasementFloorYIndex( BasementFloorYIndex )
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

		// Member Constructor
		DirectionReal_Dictionary(
			int const Direction, // From Enum: Direction
			Real64 const Value
		) :
			Direction( Direction ),
			Value( Value )
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

		// Member Constructor
		ReportingInformation(
			Array1< DirectionReal_Dictionary > const & SurfaceHeatTransfer,
			Real64 const TotalBoundaryHeatTransfer,
			Real64 const EnergyStoredInCells,
			Real64 const AverageSurfaceTemperature,
			Real64 const PipeCircuitHeatTransferMCpDT,
			Real64 const PipeCircuitHeatTransferUADT,
			Real64 const BasementWallHeatTransfer,
			Real64 const BasementFloorHeatTransfer,
			Real64 const AverageBasementFloorTemperature,
			Real64 const AverageBasementWallTemperature
		) :
			SurfaceHeatTransfer( SurfaceHeatTransfer ),
			TotalBoundaryHeatTransfer( TotalBoundaryHeatTransfer ),
			EnergyStoredInCells( EnergyStoredInCells ),
			AverageSurfaceTemperature( AverageSurfaceTemperature ),
			PipeCircuitHeatTransferMCpDT( PipeCircuitHeatTransferMCpDT ),
			PipeCircuitHeatTransferUADT( PipeCircuitHeatTransferUADT ),
			BasementWallHeatTransfer( BasementWallHeatTransfer ),
			BasementFloorHeatTransfer( BasementFloorHeatTransfer ),
			AverageBasementFloorTemperature( AverageBasementFloorTemperature ),
			AverageBasementWallTemperature( AverageBasementWallTemperature )
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

		// Member Constructor
		MeshPartitions(
			Array1< MeshPartition > const & X,
			Array1< MeshPartition > const & Y,
			Array1< MeshPartition > const & Z
		) :
			X( X ),
			Y( Y ),
			Z( Z )
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

		// Member Constructor
		MoistureInfo(
			Real64 const Theta_liq, // volumetric moisture content of the soil
			Real64 const Theta_sat, // volumetric moisture content of soil at saturation
			Real64 const GroundCoverCoefficient
		) :
			Theta_liq( Theta_liq ),
			Theta_sat( Theta_sat ),
			GroundCoverCoefficient( GroundCoverCoefficient )
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

		// Member Constructor
		CurSimConditionsInfo(
			Real64 const PrevSimTimeSeconds,
			Real64 const CurSimTimeSeconds,
			Real64 const CurSimTimeStepSize,
			Real64 const CurAirTemp,
			Real64 const CurWindSpeed,
			Real64 const CurIncidentSolar,
			Real64 const CurRelativeHumidity
		) :
			PrevSimTimeSeconds( PrevSimTimeSeconds ),
			CurSimTimeSeconds( CurSimTimeSeconds ),
			CurSimTimeStepSize( CurSimTimeStepSize ),
			CurAirTemp( CurAirTemp ),
			CurWindSpeed( CurWindSpeed ),
			CurIncidentSolar( CurIncidentSolar ),
			CurRelativeHumidity( CurRelativeHumidity )
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

		// Member Constructor
		PipeSegmentInfo(
			std::string const & Name,
			PointF const & PipeLocation,
			Point const & PipeCellCoordinates,
			int const FlowDirection, // From Enum: SegmentFlow
			int const ParentCircuitIndex,
			Real64 const InletTemperature,
			Real64 const OutletTemperature,
			Real64 const FluidHeatLoss,
			bool const PipeCellCoordinatesSet,
			bool const IsActuallyPartOfAHorizontalTrench
		) :
			Name( Name ),
			PipeLocation( PipeLocation ),
			PipeCellCoordinates( PipeCellCoordinates ),
			FlowDirection( FlowDirection ),
			ParentCircuitIndex( ParentCircuitIndex ),
			InletTemperature( InletTemperature ),
			OutletTemperature( OutletTemperature ),
			FluidHeatLoss( FluidHeatLoss ),
			PipeCellCoordinatesSet( PipeCellCoordinatesSet ),
			IsActuallyPartOfAHorizontalTrench( IsActuallyPartOfAHorizontalTrench )
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

		// Member Constructor
		PipeCircuitInfo(
			std::string const & Name,
			std::string const & InletNodeName,
			std::string const & OutletNodeName,
			int const InletNodeNum,
			int const OutletNodeNum,
			Point3DInteger const & CircuitInletCell,
			Point3DInteger const & CircuitOutletCell,
			Array1_string const & PipeSegmentNames,
			Array1_int const & PipeSegmentIndeces,
			int const ParentDomainIndex,
			RadialSizing const & PipeSize,
			RadialSizing const & InsulationSize,
			Real64 const RadialMeshThickness,
			bool const HasInsulation,
			Real64 const DesignVolumeFlowRate,
			Real64 const DesignMassFlowRate,
			Real64 const Convergence_CurrentToPrevIteration,
			int const MaxIterationsPerTS,
			int const NumRadialCells,
			BaseThermalPropertySet const & PipeProperties,
			BaseThermalPropertySet const & InsulationProperties,
			Array1< Point3DInteger > const & ListOfCircuitPoints,
			bool const CheckEquipName,
			bool const NeedToFindOnPlantLoop,
			bool const IsActuallyPartOfAHorizontalTrench,
			int const LoopNum,
			int const LoopSideNum,
			int const BranchNum,
			int const CompNum,
			Real64 const CurFluidDensity,
			Real64 const CurFluidViscosity,
			Real64 const CurFluidConductivity,
			Real64 const CurFluidPrandtl,
			Real64 const CurFluidSpecificHeat,
			ExtendedFluidProperties const & CurFluidPropertySet,
			Real64 const CurCircuitInletTemp,
			Real64 const CurCircuitFlowRate,
			Real64 const CurCircuitConvectionCoefficient,
			Real64 const InletTemperature,
			Real64 const OutletTemperature,
			Real64 const FluidHeatLoss
		) :
			Name( Name ),
			InletNodeName( InletNodeName ),
			OutletNodeName( OutletNodeName ),
			InletNodeNum( InletNodeNum ),
			OutletNodeNum( OutletNodeNum ),
			CircuitInletCell( CircuitInletCell ),
			CircuitOutletCell( CircuitOutletCell ),
			PipeSegmentNames( PipeSegmentNames ),
			PipeSegmentIndeces( PipeSegmentIndeces ),
			ParentDomainIndex( ParentDomainIndex ),
			PipeSize( PipeSize ),
			InsulationSize( InsulationSize ),
			RadialMeshThickness( RadialMeshThickness ),
			HasInsulation( HasInsulation ),
			DesignVolumeFlowRate( DesignVolumeFlowRate ),
			DesignMassFlowRate( DesignMassFlowRate ),
			Convergence_CurrentToPrevIteration( Convergence_CurrentToPrevIteration ),
			MaxIterationsPerTS( MaxIterationsPerTS ),
			NumRadialCells( NumRadialCells ),
			PipeProperties( PipeProperties ),
			InsulationProperties( InsulationProperties ),
			ListOfCircuitPoints( ListOfCircuitPoints ),
			CheckEquipName( CheckEquipName ),
			NeedToFindOnPlantLoop( NeedToFindOnPlantLoop ),
			IsActuallyPartOfAHorizontalTrench( IsActuallyPartOfAHorizontalTrench ),
			LoopNum( LoopNum ),
			LoopSideNum( LoopSideNum ),
			BranchNum( BranchNum ),
			CompNum( CompNum ),
			CurFluidDensity( CurFluidDensity ),
			CurFluidViscosity( CurFluidViscosity ),
			CurFluidConductivity( CurFluidConductivity ),
			CurFluidPrandtl( CurFluidPrandtl ),
			CurFluidSpecificHeat( CurFluidSpecificHeat ),
			CurFluidPropertySet( CurFluidPropertySet ),
			CurCircuitInletTemp( CurCircuitInletTemp ),
			CurCircuitFlowRate( CurCircuitFlowRate ),
			CurCircuitConvectionCoefficient( CurCircuitConvectionCoefficient ),
			InletTemperature( InletTemperature ),
			OutletTemperature( OutletTemperature ),
			FluidHeatLoss( FluidHeatLoss )
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

		// Member Constructor
		ZoneCoupledSurfaceData(
			std::string Name,
			int const IndexInSurfaceArray,
			Real64 const SurfaceArea,
			Real64 const Width,
			Real64 const Length,
			Real64 const Depth,
			Real64 const Conductivity,
			Real64 const Density,
			Real64 const InsulationConductivity,
			Real64 const InsulationDensity
		) :
			Name( Name ),
			IndexInSurfaceArray( IndexInSurfaceArray ),
			SurfaceArea( SurfaceArea ),
			Width( Width ),
			Length( Length ),
			Depth( Depth ),
			Conductivity( Conductivity ),
			Density( Density ),
			InsulationConductivity( InsulationConductivity ),
			InsulationDensity( InsulationDensity )
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

		// Member Constructor
		FullDomainStructureInfo(
			std::string const & Name,
			Array1_string const & CircuitNames,
			Array1_int const & CircuitIndeces,
			int const MaxIterationsPerTS,
			bool const OneTimeInit,
			bool const BeginSimInit,
			bool const BeginSimEnvrn,
			bool const DomainNeedsSimulation,
			bool const DomainNeedsToBeMeshed,
			bool const IsActuallyPartOfAHorizontalTrench,
			bool const HasAPipeCircuit,
			bool const IsZoneCoupledSlab,
			bool const HasCoupledBasement,
			MeshExtents const & Extents,
			MeshProperties const & Mesh,
			BaseThermalPropertySet const & GroundProperties,
			BaseThermalPropertySet const & SlabProperties,
			BaseThermalPropertySet const & BasementInterfaceProperties,
			BaseThermalPropertySet const & HorizInsProperties,
			BaseThermalPropertySet const & VertInsProperties,
			SimulationControl const & SimControls,
			FarfieldInfo const & Farfield,
			BasementZoneInfo const & BasementZone,
			MoistureInfo const & Moisture,
			MeshPartitions const & Partitions,
			CurSimConditionsInfo const & Cur,
			ReportingInformation const & Reporting,
			bool const HasBasement,
			Array1 <ZoneCoupledSurfaceData> const & ZoneCoupledSurfaces,
			int const ZoneCoupledOSCMIndex,
			Real64 const PerimeterOffset,
			bool const SlabInGradeFlag,
			int const SlabMaterialNum,
			Real64 const SlabWidth,
			Real64 const SlabLength,
			Real64 const SlabThickness,
			Real64 const XIndex,
			Real64 const YIndex,
			Real64 const ZIndex,
			bool const HorizInsPresentFlag,
			int const HorizInsMaterialNum,
			Real64 const HorizInsThickness,
			Real64 const HorizInsWidth,
			Real64 const HeatFlux,
			Real64 const WallHeatFlux,
			Real64 const FloorHeatFlux,
			Real64 const AggregateHeatFlux,
			Real64 const AggregateWallHeatFlux,
			Real64 const AggregateFloorHeatFlux,
			int const NumHeatFlux,
			bool const ResetHeatFluxFlag,
			Real64 const ConvCoeff,
			bool const FullHorizInsPresent,
			bool const VertInsPresentFlag,
			int const VertInsMaterialNum,
			Real64 const VertInsThickness,
			Real64 const VertInsDepth,
			int const XWallIndex,
			int const YFloorIndex,
			int const ZWallIndex,
			int const InsulationXIndex,
			int const InsulationYIndex,
			int const InsulationZIndex,
			bool const SimTimestepFlag,
			bool const SimHourlyFlag,
			Real64 ZoneCoupledSurfaceTemp,
			Real64 BasementWallTemp,
			Real64 BasementFloorTemp,
			int const NumDomainCells,
			int const NumGroundSurfCells,
			int const NumInsulationCells,

			Array3< CartesianCell > const & Cells
		) :
			Name( Name ),
			CircuitNames( CircuitNames ),
			CircuitIndeces( CircuitIndeces ),
			MaxIterationsPerTS( MaxIterationsPerTS ),
			OneTimeInit( OneTimeInit ),
			BeginSimInit( BeginSimInit ),
			BeginSimEnvrn( BeginSimEnvrn ),
			DomainNeedsSimulation( DomainNeedsSimulation ),
			DomainNeedsToBeMeshed( DomainNeedsToBeMeshed ),
			IsActuallyPartOfAHorizontalTrench( IsActuallyPartOfAHorizontalTrench ),
			HasAPipeCircuit( HasAPipeCircuit ),
			IsZoneCoupledSlab( IsZoneCoupledSlab ),
			HasCoupledBasement( HasCoupledBasement ),
			Extents( Extents ),
			Mesh( Mesh ),
			GroundProperties( GroundProperties ),
			SlabProperties( SlabProperties ),
			BasementInterfaceProperties( BasementInterfaceProperties ),
			HorizInsProperties( HorizInsProperties ),
			VertInsProperties( VertInsProperties ),
			SimControls( SimControls ),
			Farfield( Farfield ),
			BasementZone( BasementZone ),
			Moisture( Moisture ),
			Partitions( Partitions ),
			Cur( Cur ),
			Reporting( Reporting ),
			HasBasement( HasBasement ),
			ZoneCoupledSurfaces( ZoneCoupledSurfaces ),
			ZoneCoupledOSCMIndex( ZoneCoupledOSCMIndex ),
			PerimeterOffset( PerimeterOffset ),
			SlabInGradeFlag( SlabInGradeFlag ),
			SlabMaterialNum( SlabMaterialNum ),
			SlabWidth( SlabWidth ),
			SlabLength( SlabLength ),
			SlabThickness( SlabThickness ),
			XIndex( XIndex ),
			YIndex( YIndex ),
			ZIndex( ZIndex ),
			HorizInsPresentFlag( HorizInsPresentFlag ),
			HorizInsMaterialNum( HorizInsMaterialNum ),
			HorizInsThickness( HorizInsThickness ),
			HorizInsWidth( HorizInsWidth ),
			HeatFlux( HeatFlux ),
			WallHeatFlux( WallHeatFlux ),
			FloorHeatFlux( FloorHeatFlux ),
			AggregateHeatFlux( AggregateHeatFlux ),
			AggregateWallHeatFlux( AggregateWallHeatFlux ),
			AggregateFloorHeatFlux( AggregateFloorHeatFlux ),
			NumHeatFlux( NumHeatFlux ),
			ResetHeatFluxFlag( ResetHeatFluxFlag ),
			ConvCoeff( ConvCoeff ),
			FullHorizInsPresent( FullHorizInsPresent ),
			VertInsPresentFlag( VertInsPresentFlag ),
			VertInsMaterialNum( VertInsMaterialNum ),
			VertInsThickness( VertInsThickness ),
			VertInsDepth( VertInsDepth ),
			XWallIndex( XWallIndex ),
			YFloorIndex( YFloorIndex ),
			ZWallIndex( ZWallIndex ),
			InsulationXIndex( InsulationXIndex ),
			InsulationYIndex( InsulationYIndex ),
			InsulationZIndex( InsulationZIndex ),
			SimTimestepFlag( SimTimestepFlag ),
			SimHourlyFlag( SimHourlyFlag ),
			ZoneCoupledSurfaceTemp( ZoneCoupledSurfaceTemp ),
			BasementWallTemp( BasementWallTemp ),
			BasementFloorTemp( BasementFloorTemp ),
			NumDomainCells( NumDomainCells ),
			NumGroundSurfCells( NumGroundSurfCells ),
			NumInsulationCells( NumInsulationCells ),
			Cells( Cells )
		{}

	};

	// Object Data
	extern Array1D< FullDomainStructureInfo > PipingSystemDomains;
	extern Array1D< PipeCircuitInfo > PipingSystemCircuits;
	extern Array1D< PipeSegmentInfo > PipingSystemSegments;

} // DataPlantPipingSystems

} // EnergyPlus

#endif
