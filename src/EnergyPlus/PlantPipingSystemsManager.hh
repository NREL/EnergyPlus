// EnergyPlus, Copyright (c) 1996-2017, The Board of Trustees of the University of Illinois,
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
// National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other
// contributors. All rights reserved.
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
//     similar designation, without the U.S. Department of Energy's prior written consent.
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

#ifndef PlantPipingSystemsManager_hh_INCLUDED
#define PlantPipingSystemsManager_hh_INCLUDED

// C++ Headers
#include <map>
#include <memory>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Array3D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <GroundTemperatureModeling/GroundTemperatureModelManager.hh>

namespace EnergyPlus {

namespace PlantPipingSystemsManager {

	// MODULE PARAMETER DEFINITIONS:
	extern std::string const ObjName_ug_GeneralDomain;
	extern std::string const ObjName_Circuit;
	extern std::string const ObjName_Segment;
	extern std::string const ObjName_HorizTrench;
	extern std::string const ObjName_ZoneCoupled_Slab;
	extern std::string const ObjName_ZoneCoupled_Basement;

	// Using/Aliasing
	using namespace GroundTemperatureManager;

	enum class SegmentFlow { IncreasingZ, DecreasingZ };
	enum class MeshDistribution { Uniform, SymmetricGeometric, Geometric };
	enum class RegionType { Pipe, BasementWall, BasementFloor, XDirection, YDirection, ZDirection, XSide, XSideWall, ZSide, ZSideWall, FloorInside, UnderFloor, HorizInsXSide, HorizInsZSide, VertInsLowerEdge };
	enum class Direction { PositiveY, NegativeY, PositiveX, NegativeX, PositiveZ, NegativeZ };
	enum class PartitionType { BasementWall, BasementFloor, Pipe, Slab, XSide, XSideWall, ZSide, ZSideWall, FloorInside, UnderFloor, HorizInsXSide, VertInsLowerEdge, HorizInsZSide };
	enum class CellType { Unknown, Pipe, GeneralField, GroundSurface, FarfieldBoundary, BasementWall, BasementFloor, BasementCorner, BasementCutaway, Slab, HorizInsulation, VertInsulation, ZoneGroundInterface };

	extern Array1D< Direction > NeighborFieldCells;
	extern Array1D< Direction > NeighborBoundaryCells;

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

		Real64 inline diffusivity(){return this->Conductivity / ( this->Density * this->SpecificHeat );}

	};

	struct ExtendedFluidProperties : BaseThermalPropertySet
	{
		// Members
		Real64 Viscosity; // kg/m-s
		Real64 Prandtl; // -

		// Default Constructor
		ExtendedFluidProperties()
		{}

		// Member Constructor
		ExtendedFluidProperties(
			Real64 const Conductivity, // W/mK
			Real64 const Density, // kg/m3
			Real64 const SpecificHeat, // J/kgK
			Real64 const Viscosity, // kg/m-s
			Real64 const Prandtl // -
		) :
			BaseThermalPropertySet( Conductivity, Density, SpecificHeat ),
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

	struct RadialSizing
	{
		// Members
		Real64 InnerDia;
		Real64 OuterDia;

		// Default Constructor
		RadialSizing()
		{}

		Real64 inline thickness(){return ( this->OuterDia - this->InnerDia ) / 2.0;}
	};

	struct RadialCellInformation : BaseCell
	{
		// Members
		Real64 RadialCentroid;
		Real64 InnerRadius;
		Real64 OuterRadius;

		// Default Constructor
		RadialCellInformation()
		{}

		// Member Constructor
		RadialCellInformation(
			Real64 const m_RadialCentroid,
			Real64 const m_MinRadius,
			Real64 const m_MaxRadius
		) {
			RadialCentroid = m_RadialCentroid;
			InnerRadius = m_MinRadius;
			OuterRadius = m_MaxRadius;
		}

		// Get the XY cross sectional area of the radial cell
		Real64 inline XY_CrossSectArea(){return DataGlobals::Pi * ( pow_2( this->OuterRadius ) - pow_2( this->InnerRadius ) );}

	};

	struct FluidCellInformation : BaseCell
	{
		// Members
		Real64 PipeInnerRadius;
		Real64 Volume;
		ExtendedFluidProperties Properties;

		// Default Constructor
		FluidCellInformation()
		{}

		// Member Constructor
		FluidCellInformation(
			Real64 const m_PipeInnerRadius,
			Real64 const m_CellDepth
		) {
			this->PipeInnerRadius = m_PipeInnerRadius;
			this->Volume = DataGlobals::Pi * pow_2( m_PipeInnerRadius ) * m_CellDepth;
		}
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

		// Eventually this should be the real constructor
		static void ctor(
			CartesianPipeCellInformation & c,
			Real64 const GridCellWidth,
			RadialSizing const & PipeSizes,
			int const NumRadialNodes,
			Real64 const CellDepth,
			Real64 const InsulationThickness,
			Real64 const RadialGridExtent,
			bool const SimHasInsulation
		);

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
		PartitionType partitionType; // From Enum: ParitionType
		Real64 TotalWidth;

		// Default Constructor
		MeshPartition()
		{}

		// Member Constructor
		MeshPartition(
			Real64 const rDimension,
			PartitionType const partitionType, // From Enum: ParitionType
			Real64 const TotalWidth
		) :
			rDimension( rDimension ),
			partitionType( partitionType ),
			TotalWidth( TotalWidth )
		{}

	};

	struct GridRegion
	{
		// Members
		Real64 Min;
		Real64 Max;
		RegionType thisRegionType; // From Enum: RegionType
		Array1D< Real64 > CellWidths;

		// Default Constructor
		GridRegion()
		{}

		// Member Constructor
		GridRegion(
			Real64 Min,
			Real64 Max,
			RegionType thisRegionType,
			Array1D< Real64 > CellWidths
		) :
			Min( Min ),
			Max( Max ),
			thisRegionType( thisRegionType ),
			CellWidths( CellWidths )
		{}
	};

	struct TempGridRegionData
	{
		// Members
		Real64 Min;
		Real64 Max;
		RegionType thisRegionType;

		// Default Constructor
		TempGridRegionData()
		{}

		// Member Constructor
		TempGridRegionData(
			Real64 const Min,
			Real64 const Max,
			RegionType const thisRegionType
		) :
			Min( Min ),
			Max( Max ),
			thisRegionType( thisRegionType )
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

		bool inline contains(PointF const & p){return ( ( this->X_min <= p.X ) && ( p.X < ( this->X_min + this->Width ) ) && ( this->Y_min <= p.Y ) && ( p.Y < ( this->Y_min + this->Height ) ) );}

	};

	struct NeighborInformation
	{
		// Members
		Real64 ThisCentroidToNeighborCentroid;
		Real64 ThisCentroidToNeighborWall;
		Real64 ThisWallToNeighborCentroid;
		Real64 ConductionResistance;
		Real64 adiabaticMultiplier;
		Direction direction;
		Point3DInteger NeighborCellIndeces;

		// Default Constructor
		NeighborInformation() : adiabaticMultiplier( 1.0 )
		{}

	};

	struct CartesianCell : BaseCell
	{
		// Members
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
		CellType cellType;
		int PipeIndex;
		std::map< Direction, NeighborInformation > NeighborInfo;
		CartesianPipeCellInformation PipeCellData;

		// Default Constructor
		CartesianCell()
		{}

		Real64 inline width() const {return this->X_max - this->X_min;}

		Real64 inline height() const {return this->Y_max - this->Y_min;}

		Real64 inline depth() const {return this->Z_max - this->Z_min;}

		Real64 inline XNormalArea() const {return this->depth() * this->height();}

		Real64 inline YNormalArea() const {return this->depth() * this->width();}

		Real64 inline ZNormalArea() const {return this->width() * this->height();}

		Real64 inline volume() const {return this->width() * this->depth() * this->height();}

		Real64 normalArea(Direction const direction) const;

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
		MeshDistribution thisMeshDistribution;
		int RegionMeshCount;
		Real64 GeometricSeriesCoefficient;

		// Default Constructor
		DistributionStructure() :
			thisMeshDistribution( MeshDistribution::Uniform ),
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
		Real64 rhoCP_soil_liq;
		Real64 rhoCP_soil_transient;
		Real64 rhoCP_soil_ice;
		Real64 rhoCp_soil_liq_1;

		// Default Constructor
		MoistureInfo() :
			Theta_liq( 0.3 ),
			Theta_sat( 0.5 ),
			GroundCoverCoefficient( 0.408 ),
			rhoCP_soil_liq( 0.0 ),
			rhoCP_soil_transient( 0.0 ),
			rhoCP_soil_ice( 0.0 ),
			rhoCp_soil_liq_1( 0.0 )
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
		SegmentFlow FlowDirection;
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
			FlowDirection( SegmentFlow::IncreasingZ ),
			ParentCircuitIndex( 0 ),
			InletTemperature( 0.0 ),
			OutletTemperature( 0.0 ),
			FluidHeatLoss( 0.0 ),
			PipeCellCoordinatesSet( false ),
			IsActuallyPartOfAHorizontalTrench( false )
		{}

		void
		initPipeCells(
			int const x,
			int const y
		);

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


		void
		initInOutCells(
			CartesianCell const & in,
			CartesianCell const & out
		);


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
		int Zone;

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
			InsulationDensity( 0.0 ),
			Zone( 0 )
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
		bool HasZoneCoupledSlab;
		bool HasZoneCoupledBasement;
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
		Array1D < ZoneCoupledSurfaceData > ZoneCoupledSurfaces;
		int ZoneCoupledOSCMIndex;
		Real64 PerimeterOffset;
		bool SlabInGradeFlag;
		int SlabMaterialNum;
		Real64 SlabArea;
		Real64 SlabWidth;
		Real64 SlabLength;
		Real64 SlabThickness;
		Real64 XIndex;
		Real64 YIndex;
		Real64 ZIndex;
		int x_max_index;
		int y_max_index;
		int z_max_index;
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
		bool SimDailyFlag;
		Real64 ZoneCoupledSurfaceTemp;
		Real64 BasementWallTemp;
		Real64 BasementFloorTemp;
		int NumDomainCells;
		int NumGroundSurfCells;
		int NumInsulationCells;
		int NumSlabCells;
		Array2D< Real64 > WeightingFactor;
		Array2D< Real64 > WeightedHeatFlux;
		Real64 TotalEnergyUniformHeatFlux = 0.0;
		Real64 TotalEnergyWeightedHeatFlux = 0.0;
		Real64 HeatFluxWeightingFactor = 0.0;
		Array1D< GridRegion > XRegions;
		Array1D< GridRegion > YRegions;
		Array1D< GridRegion > ZRegions;

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
			HasZoneCoupledSlab( false ),
			HasZoneCoupledBasement( false ),
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
			x_max_index( 0 ),
			y_max_index( 0 ),
			z_max_index( 0 ),
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
			SimDailyFlag( false ),
			ZoneCoupledSurfaceTemp( 0.0 ),
			BasementWallTemp( 0.0 ),
			BasementFloorTemp( 0.0 ),
			NumDomainCells ( 0 ),
			NumGroundSurfCells( 0 ),
			NumInsulationCells( 0 ),
			NumSlabCells( 0 ),
			XRegions( { 0, -1 } ),
			YRegions( { 0, -1 } ),
			ZRegions( { 0, -1 } )
		{}


		void
		developMesh();

		void
		createPartitionCenterList();

		Array1D< GridRegion >
		createPartitionRegionList(
			Array1D< MeshPartition > const & ThesePartitionCenters,
			bool const PartitionsExist,
			Real64 const DirExtentMax,
			int const PartitionsUBound
		);

		void
		createRegionList(
			Array1D< GridRegion > & Regions,
			Array1D< GridRegion > const & ThesePartitionRegions,
			Real64 const DirExtentMax,
			RegionType const DirDirection,
			bool const PartitionsExist,
			Optional_int BasementWallXIndex = _,
			Optional_int BasementFloorYIndex = _,
			Optional_int XIndex = _,
			Optional_int XWallIndex = _,
			Optional_int InsulationXIndex = _,
			Optional_int YIndex = _,
			Optional_int YFloorIndex = _,
			Optional_int InsulationYIndex = _,
			Optional_int ZIndex = _,
			Optional_int ZWallIndex = _,
			Optional_int InsulationZIndex = _
		);

		void
		createCellArray(
			Array1D< Real64 > const & XBoundaryPoints,
			Array1D< Real64 > const & YBoundaryPoints,
			Array1D< Real64 > const & ZBoundaryPoints
		);


		void
		setupCellNeighbors();

		void
		setupPipeCircuitInOutCells();

		int
		getCellWidthsCount(
			RegionType const dir
		);

		void
		getCellWidths(
			GridRegion & g,
			RegionType const direction
		);

		void
		addNeighborInformation(
			int const X,
			int const Y,
			int const Z,
			Direction const direction,
			Real64 const ThisCentroidToNeighborCentroid,
			Real64 const ThisCentroidToNeighborWall,
			Real64 const ThisWallToNeighborCentroid,
			Real64 const ThisAdiabaticMultiplier
		);


		Real64
		GetBasementWallHeatFlux();

		Real64
		GetBasementFloorHeatFlux();

		void
		UpdateBasementSurfaceTemperatures();

		Real64
		GetZoneInterfaceHeatFlux();

		void
		UpdateZoneSurfaceTemperatures();

		Real64
		GetAverageTempByType(
			CellType const cellType
		);

		void InitializeSoilMoistureCalcs();

		void
		EvaluateSoilRhoCp(
			Real64 const CellTemp,
			Real64 & rhoCp
		);

		void
		ShiftTemperaturesForNewTimeStep();

		void
		ShiftTemperaturesForNewIteration();

		bool
		IsConverged_CurrentToPrevIteration();

		bool
		CheckForOutOfRangeTemps();

	};

	// Object Data
	extern Array1D< FullDomainStructureInfo > PipingSystemDomains;
	extern Array1D< PipeCircuitInfo > PipingSystemCircuits;
	extern Array1D< PipeSegmentInfo > PipingSystemSegments;

	void
	clear_state();

	void
	SimPipingSystemCircuit(
		std::string const & EquipName, // name of the Pipe Heat Transfer.
		int & EqNum, // index in local derived types for external calling
		bool const FirstHVACIteration, // component number
		bool const InitLoopEquip
	);

	void
	SimulateGroundDomains(
		bool initOnly
	);

	void
	CheckIfAnySlabs();

	void
	CheckIfAnyBasements();

	void
	GetPipingSystemsAndGroundDomainsInput();

	int
	GetNumSegmentsForHorizontalTrenches( int const NumHorizontalTrenches );

	void
	ReadGeneralDomainInputs(
		int const IndexStart,
		int const NumGeneralizedDomains,
		bool & ErrorsFound
	);

	void
	ReadZoneCoupledDomainInputs(
		int const StartingDomainNumForZone,
		int const NumZoneCoupledDomains,
		bool & ErrorsFound
	);

	void
	ReadBasementInputs(
		int const StartingDomainNumForBasement,
		int const NumBasements,
		bool & ErrorsFound
	);

	void
	ReadPipeCircuitInputs(
		int const NumPipeCircuits,
		bool & ErrorsFound
	);

	void
	ReadPipeSegmentInputs(
		int const NumPipeSegmentsInInput,
		bool & ErrorsFound
	);

	void
	ReadHorizontalTrenchInputs(
		int const StartingDomainNumForHorizontal,
		int const StartingCircuitNumForHorizontal,
		int const StartingSegmentNumForHorizontal,
		int const NumHorizontalTrenchesInInput,
		bool & ErrorsFound
	);

	void
	SetupPipingSystemOutputVariables(
		int const TotalNumSegments,
		int const TotalNumCircuits
	);

	void
	SetupZoneCoupledOutputVariables(
		int const DomainNum
	);

	void
	InitPipingSystems(
		int const DomainNum,
		int const CircuitNum
	);

	void
	UpdatePipingSystems(
		int const DomainNum,
		int const CircuitNum
	);

	void
	IssueSevereInputFieldErrorStringEntry(
		std::string const & RoutineName,
		std::string const & ObjectName,
		std::string const & InstanceName,
		std::string const & FieldName,
		std::string const & FieldEntry,
		std::string const & Condition,
		bool & ErrorsFound
	);

	void
	IssueSevereInputFieldErrorRealEntry(
		std::string const & RoutineName,
		std::string const & ObjectName,
		std::string const & InstanceName,
		std::string const & FieldName,
		Real64 const FieldEntry,
		std::string const & Condition,
		bool & ErrorsFound
	);

	int
	GetSurfaceCountForOSCM( int const OSCMIndex );

	Array1D_int
	GetSurfaceIndecesForOSCM(
		int const OSCMIndex,
		int const SurfCount
	);

	Array1D< ZoneCoupledSurfaceData >
	GetSurfaceDataForOSCM(
		int const OSCMIndex,
		int const SurfCount
	);

	bool
	inline
	IsInRangeReal(
		Real64 const r,
		Real64 const lower,
		Real64 const upper
	)
	{
		return ( ( r >= lower ) && ( r <= upper ) );
	}

	bool
	inline
	IsInRange_BasementModel(
		Real64 const r,
		Real64 const lower,
		Real64 const upper
	)
	{
		return ( ( r >= lower ) && ( r < upper ) );
	}

	Real64
	inline
	Real_ConstrainTo(
		Real64 const r,
		Real64 const MinVal,
		Real64 const MaxVal
	)
	{
		return min( max( r, MinVal ), MaxVal );
	}

	bool
	MeshPartitionArray_Contains(
		Array1D< MeshPartition > const & meshes,
		Real64 const value
	);

	bool
	DomainRectangle_Contains(
		DomainRectangle const & Rect,
		Point const & p
	);

	void
	MeshPartition_SelectionSort( Array1< MeshPartition > & X );

	bool
	IsConverged_PipeCurrentToPrevIteration(
		int const CircuitNum,
		CartesianCell const & CellToCheck
	);

	void
	ShiftPipeTemperaturesForNewIteration( CartesianCell & ThisPipeCell );

	int
	CreateRegionListCount(
		Array1D< GridRegion > const & ThesePartitionRegions,
		Real64 const DirExtentMax,
		bool const PartitionsExist
	);

	int
	CreateBoundaryListCount(
		Array1D< GridRegion > const & RegionList,
		RegionType const DirDirection
	);

	Array1D< Real64 >
	CreateBoundaryList(
		Array1D< GridRegion > const & RegionList,
		Real64 const DirExtentMax,
		RegionType const DirDirection,
		int const RetValLbound,
		int const RetValUBound
	);

	void
	PerformIterationLoop(
		int const DomainNum,
		Optional < int const > CircuitNum
	);

	void
	PerformTemperatureFieldUpdate( int const DomainNum );

	Real64
	EvaluateFieldCellTemperature(
		int const DomainNum,
		CartesianCell & ThisCell
	);

	Real64
	EvaluateGroundSurfaceTemperature(
		int const DomainNum,
		CartesianCell & cell
	);

	Real64
	EvaluateBasementCellTemperature(
		int const DomainNum,
		CartesianCell & cell
	);

	Real64
	EvaluateZoneInterfaceTemperature(
		int const DomainNum,
		CartesianCell & cell
	);

	Real64
	GetAverageInterfaceTemp(
		int const DomainNum,
		int const CellType,
		int const CellType2
	);

	Real64
	EvaluateFarfieldBoundaryTemperature(
		int const DomainNum,
		CartesianCell & cell
	);

	void
	EvaluateFarfieldCharacteristics(
		int const DomainNum,
		CartesianCell & cell,
		Direction const direction,
		Real64 & neighbortemp,
		Real64 & resistance,
		Real64 & adiabaticMultiplier
	);

	Real64
	GetFarfieldTemp(
		int const DomainNum,
		CartesianCell const & cell
	);

	void
	PreparePipeCircuitSimulation(
		int const DomainNum,
		int const CircuitNum
	);

	void
	PerformPipeCircuitSimulation(
		int const DomainNum,
		int const CircuitNum
	);

	void
	PerformPipeCellSimulation(
		int const DomainNum,
		int const CircuitNum,
		CartesianCell & ThisCell,
		Real64 const FlowRate,
		Real64 const EnteringTemp
	);

	void
	SimulateRadialToCartesianInterface(
		int const DomainNum,
		CartesianCell & ThisCell
	);

	void
	SimulateOuterMostRadialSoilSlice(
		int const CircuitNum,
		CartesianCell & ThisCell
	);

	void
	SimulateAllInteriorRadialSoilSlices( CartesianCell & ThisCell );

	void
	SimulateInnerMostRadialSoilSlice(
		int const CircuitNum,
		CartesianCell & ThisCell
	);

	void
	SimulateRadialInsulationCell( CartesianCell & ThisCell );

	void
	SimulateRadialPipeCell(
		int const CircuitNum,
		CartesianCell & ThisCell,
		Real64 const ConvectionCoefficient
	);

	void
	SimulateFluidCell(
		CartesianCell & ThisCell,
		Real64 const FlowRate,
		Real64 const ConvectionCoefficient,
		Real64 const EnteringFluidTemp
	);

	void
	DoOneTimeInitializations(
		int const DomainNum,
		Optional < int const > CircuitNum
	);

	void
	DoStartOfTimeStepInitializations(
		int const DomainNum,
		Optional < int const > CircuitNum
	);

	void
	DoEndOfIterationOperations(
		int const DomainNum,
		bool & Finished
	);

	void
	SetAdditionalNeighborData(
		int const DomainNum,
		int const X,
		int const Y,
		int const Z,
		Direction const direction,
		Real64 const Resistance,
		CartesianCell const & NeighborCell
	);

	void
	EvaluateNeighborCoordinates(
		CartesianCell const & ThisCell,
		Direction const CurDirection,
		int & NX,
		int & NY,
		int & NZ
	);

	void
	EvaluateNeighborCharacteristics(
		int const DomainNum,
		CartesianCell & ThisCell,
		Direction const CurDirection,
		Real64 & NeighborTemp,
		Real64 & Resistance,
		Real64 & AdiabaticMultiplier
	);

	void
	EvaluateCellNeighborDirections(
		int const DomainNum,
		CartesianCell const & cell,
		int & NumFieldCells,
		int & NumBoundaryCells
	);

} // PlantPipingSystemsManager

} // EnergyPlus

#endif
