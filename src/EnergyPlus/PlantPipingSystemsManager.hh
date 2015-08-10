#ifndef PlantPipingSystemsManager_hh_INCLUDED
#define PlantPipingSystemsManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataPlantPipingSystems.hh>

namespace EnergyPlus {

namespace PlantPipingSystemsManager {

	// Using/Aliasing
	using DataPlantPipingSystems::BaseThermalPropertySet;
	using DataPlantPipingSystems::CartesianCell;
	using DataPlantPipingSystems::CartesianPipeCellInformation;
	using DataPlantPipingSystems::DirectionNeighbor_Dictionary;
	using DataPlantPipingSystems::DomainRectangle;
	using DataPlantPipingSystems::FluidCellInformation;
	using DataPlantPipingSystems::GridRegion;
	using DataPlantPipingSystems::MeshPartition;
	using DataPlantPipingSystems::NeighborInformation;
	using DataPlantPipingSystems::PipeCircuitInfo;
	using DataPlantPipingSystems::PipeSegmentInfo;
	using DataPlantPipingSystems::Point;
	using DataPlantPipingSystems::PointF;
	using DataPlantPipingSystems::RadialCellInformation;
	using DataPlantPipingSystems::RadialSizing;
	using DataPlantPipingSystems::RectangleF;
	using DataPlantPipingSystems::ZoneCoupledSurfaceData;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern std::string const ObjName_ug_GeneralDomain;
	extern std::string const ObjName_Circuit;
	extern std::string const ObjName_Segment;
	extern std::string const ObjName_HorizTrench;
	extern std::string const ObjName_ZoneCoupled_Slab;
	extern std::string const ObjName_ZoneCoupled_Basement;

	// MODULE INTERFACE DEFINITIONS:

	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	extern Array1D_int NeighborFieldCells;
	extern Array1D_int NeighborBoundaryCells;

	// SUBROUTINE SPECIFICATIONS FOR MODULE:
	// ************************************* !
	// Driver/Manager Routines               !
	// ************************************* !
	//   Public Entry Point                  !
	//   Other Management                    !
	//   Management/Input workers
	// ************************************* !
	// ************************************* !

	// ******************************************** !
	// Utility Routines                             !
	// ******************************************** !
	//   Useful numeric routines                    !
	//   Extensions for data classes                !
	//   Convergence checks                         !
	//   Array shifting                             !
	//   Error checking                             !
	//   Other utilities                            !
	//   Cartesian cell property routines           !
	//   Class "constructors"                       !
	// ******************************************** !
	// ******************************************** !

	// ***************************************** !
	// Simulation Algorithms                     !
	// ***************************************** !
	//   Mesh Development routines               !
	//   Simulation algorithms                   !
	// ***************************************** !
	// ***************************************** !

	// Functions

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	SimPipingSystemCircuit(
		std::string const & EquipName, // name of the Pipe Heat Transfer.
		int & EqNum, // index in local derived types for external calling
		bool const FirstHVACIteration, // component number
		bool const InitLoopEquip
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	InitAndSimGroundDomains();

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	CheckIfAnySlabs();

	//*********************************************************************************************!

	//*********************************************************************************************!
	void
	CheckIfAnyBasements();

	//*********************************************************************************************!

	//*********************************************************************************************!
	void
	GetPipingSystemsInput();

	//*********************************************************************************************!

	//*********************************************************************************************!

	int
	GetNumSegmentsForHorizontalTrenches( int const NumHorizontalTrenches );

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	ReadGeneralDomainInputs(
		int const IndexStart,
		int const NumGeneralizedDomains,
		bool & ErrorsFound
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	ReadZoneCoupledDomainInputs(
		int const StartingDomainNumForZone,
		int const NumZoneCoupledDomains,
		bool & ErrorsFound
	);

	//*********************************************************************************************!

	//*********************
	void
	ReadBasementInputs(
		int const StartingDomainNumForBasement,
		int const NumBasements,
		bool & ErrorsFound
	);

	//*********************************************************************************************!

	//*********************************************************************************************!
	void
	ReadPipeCircuitInputs(
		int const NumPipeCircuits,
		bool & ErrorsFound
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	ReadPipeSegmentInputs(
		int const NumPipeSegmentsInInput,
		bool & ErrorsFound
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	ReadHorizontalTrenchInputs(
		int const StartingDomainNumForHorizontal,
		int const StartingCircuitNumForHorizontal,
		int const StartingSegmentNumForHorizontal,
		int const NumHorizontalTrenchesInInput,
		bool & ErrorsFound
	);

	//*********************************************************************************************!

	void
	SetupPipingSystemOutputVariables(
		int const TotalNumSegments,
		int const TotalNumCircuits
	);

	//*********************************************************************************************!

	void
	SetupZoneCoupledOutputVariables(
		int const DomainNum
	);

	//*********************************************************************************************!

	void
	InitPipingSystems(
		int const DomainNum,
		int const CircuitNum
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	UpdatePipingSystems(
		int const DomainNum,
		int const CircuitNum
	);

	//*********************************************************************************************!

	//=====================  Utility/Other routines for module.

	//*********************************************************************************************!

	void
	IssueSevereInputFieldError(
		std::string const & RoutineName,
		std::string const & ObjectName,
		std::string const & InstanceName,
		std::string const & FieldName,
		std::string const & FieldEntry,
		std::string const & Condition,
		bool & ErrorsFound
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	IssueSevereInputFieldError(
		std::string const & RoutineName,
		std::string const & ObjectName,
		std::string const & InstanceName,
		std::string const & FieldName,
		Real64 const FieldEntry,
		std::string const & Condition,
		bool & ErrorsFound
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	int
	GetSurfaceCountForOSCM( int const OSCMIndex );

	//*********************************************************************************************!

	//*********************************************************************************************!

	Array1D_int
	GetSurfaceIndecesForOSCM(
		int const OSCMIndex,
		int const SurfCount
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	Array1D< ZoneCoupledSurfaceData >
	GetSurfaceDataForOSCM(
		int const OSCMIndex,
		int const SurfCount
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	bool
	IsInRange(
		int const i,
		int const lower,
		int const upper
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	bool
	IsInRange(
		Real64 const r,
		Real64 const lower,
		Real64 const upper
	);

	//*********************************************************************************************!

	//*********************************************************************************************!
	bool
	IsInRange_BasementModel(
		Real64 const r,
		Real64 const lower,
		Real64 const upper
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	Real64
	Real_ConstrainTo(
		Real64 const r,
		Real64 const MinVal,
		Real64 const MaxVal
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	bool
	CellType_IsFieldCell( int const CellType ); // From Enum: CellType

	//*********************************************************************************************!

	//*********************************************************************************************!

	bool
	MeshPartitionArray_Contains(
		Array1D< MeshPartition > const & meshes,
		Real64 const value
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	Real64
	RadialCellInfo_XY_CrossSectArea( RadialCellInformation const & r );

	//*********************************************************************************************!

	//*********************************************************************************************!

	bool
	DomainRectangle_Contains(
		DomainRectangle const & Rect,
		Point const & p
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	MeshPartition_SelectionSort( Array1< MeshPartition > & X );

	//*********************************************************************************************!

	//*********************************************************************************************!

	int
	MeshPartition_CompareByDimension(
		MeshPartition const & x,
		MeshPartition const & y
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	Real64
	BaseThermalPropertySet_Diffusivity( BaseThermalPropertySet const & p );

	//*********************************************************************************************!

	//*********************************************************************************************!

	bool
	RectangleF_Contains(
		RectangleF const & rect,
		PointF const & p
	);

	//*********************************************************************************************!

	//*********************************************************************************************!
	//Extension methods for Sim classes
	//*********************************************************************************************!

	//*********************************************************************************************!

	Real64
	RadialSizing_Thickness( RadialSizing const & r );

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	PipeSegmentInfo_InitPipeCells(
		PipeSegmentInfo & s,
		int const x,
		int const y
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	PipeCircuitInfo_InitInOutCells(
		PipeCircuitInfo & c,
		CartesianCell const & in,
		CartesianCell const & out
	);

	//*********************************************************************************************!

	//*********************************************************************************************!
	// Convergence checking
	//*********************************************************************************************!

	//*********************************************************************************************!

	bool
	IsConverged_CurrentToPrevIteration( int const DomainNum );

	//*********************************************************************************************!

	//*********************************************************************************************!

	bool
	IsConverged_PipeCurrentToPrevIteration(
		int const CircuitNum,
		CartesianCell const & CellToCheck,
		Real64 & MaxDivAmount
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	ShiftTemperaturesForNewTimeStep( int const DomainNum );

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	ShiftTemperaturesForNewIteration( int const DomainNum );

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	ShiftPipeTemperaturesForNewIteration( CartesianCell & ThisPipeCell );

	//*********************************************************************************************!

	//*********************************************************************************************!

	bool
	CheckForOutOfRangeTemps( int const DomainNum );

	//*********************************************************************************************!

	//*********************************************************************************************!

	Real64
	Width( CartesianCell const & c );

	//*********************************************************************************************!

	//*********************************************************************************************!

	Real64
	Height( CartesianCell const & c );

	//*********************************************************************************************!

	//*********************************************************************************************!

	Real64
	Depth( CartesianCell const & c );

	//*********************************************************************************************!

	//*********************************************************************************************!

	Real64
	XNormalArea( CartesianCell const & c );

	//*********************************************************************************************!

	//*********************************************************************************************!

	Real64
	YNormalArea( CartesianCell const & c );

	//*********************************************************************************************!

	//*********************************************************************************************!

	Real64
	ZNormalArea( CartesianCell const & c );

	//*********************************************************************************************!

	//*********************************************************************************************!

	Real64
	Volume( CartesianCell const & c );

	//*********************************************************************************************!

	//*********************************************************************************************!

	RectangleF
	XYRectangle( CartesianCell const & c );

	//*********************************************************************************************!

	//*********************************************************************************************!

	RectangleF
	XZRectangle( CartesianCell const & c );

	//*********************************************************************************************!

	//*********************************************************************************************!

	RectangleF
	YZRectangle( CartesianCell const & c );

	//*********************************************************************************************!

	//*********************************************************************************************!

	Real64
	NormalArea(
		CartesianCell const & c,
		int const Direction // From Enum: Direction
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	NeighborInformation
	NeighborInformationArray_Value(
		Array1D< DirectionNeighbor_Dictionary > const & dict,
		int const Direction // From Enum: Direction
	);

	//*********************************************************************************************!

	//*********************************************************************************************!
	// Constructors for generic classes
	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	CartesianPipeCellInformation_ctor(
		CartesianPipeCellInformation & c,
		Real64 const GridCellWidth,
		RadialSizing const & PipeSizes,
		int const NumRadialNodes,
		Real64 const CellDepth,
		Real64 const InsulationThickness,
		Real64 const RadialGridExtent,
		bool const SimHasInsulation
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	RadialCellInformation_ctor(
		RadialCellInformation & c,
		Real64 const m_RadialCentroid,
		Real64 const m_MinRadius,
		Real64 const m_MaxRadius
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	FluidCellInformation_ctor(
		FluidCellInformation & c,
		Real64 const m_PipeInnerRadius,
		Real64 const m_CellDepth
	);

	//*********************************************************************************************!

	// ==================================================
	// =========== Mesh Development routines ============
	// ==================================================

	//*********************************************************************************************!

	void
	DevelopMesh( int const DomainNum );

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	CreatePartitionCenterList( int const DomainNum );

	//*********************************************************************************************!

	//*********************************************************************************************!

	Array1D< GridRegion >
	CreatePartitionRegionList(
		int const DomainNum,
		Array1D< MeshPartition > const & ThesePartitionCenters,
		bool const PartitionsExist,
		Real64 const DirExtentMax,
		int const PartitionsUBound
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	int
	CreateRegionListCount(
		Array1D< GridRegion > const & ThesePartitionRegions,
		Real64 const DirExtentMax,
		bool const PartitionsExist
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	Array1D< GridRegion >
	CreateRegionList(
		int const DomainNum,
		Array1D< GridRegion > const & ThesePartitionRegions,
		Real64 const DirExtentMax,
		int const DirDirection,
		int const RetValUBound,
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

	//*********************************************************************************************!

	//*********************************************************************************************!

	int
	CreateBoundaryListCount(
		Array1D< GridRegion > const & RegionList,
		int const DirDirection
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	Array1D< Real64 >
	CreateBoundaryList(
		Array1D< GridRegion > const & RegionList,
		Real64 const DirExtentMax,
		int const DirDirection,
		int const RetValLbound,
		int const RetValUBound
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	CreateCellArray(
		int const DomainNum,
		Array1D< Real64 > const & XBoundaryPoints,
		Array1D< Real64 > const & YBoundaryPoints,
		Array1D< Real64 > const & ZBoundaryPoints
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	SetupCellNeighbors( int const DomainNum );

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	AddNeighborInformation(
		int const DomainNum,
		int const X,
		int const Y,
		int const Z,
		int const Direction, // From Enum: Direction
		Real64 const ThisCentroidToNeighborCentroid,
		Real64 const ThisCentroidToNeighborWall,
		Real64 const ThisWallToNeighborCentroid
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	SetupPipeCircuitInOutCells( int const DomainNum );

	//*********************************************************************************************!

	//*********************************************************************************************!

	int
	GetCellWidthsCount(
		int const DomainNum,
		int const dir // From Enum: RegionType
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	GetCellWidths(
		int const DomainNum,
		GridRegion & g
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	PerformIterationLoop(
		int const DomainNum,
		Optional < int const > CircuitNum
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	PerformTemperatureFieldUpdate( int const DomainNum );

	//*********************************************************************************************!

	//*********************************************************************************************!

	Real64
	EvaluateFieldCellTemperature(
		int const DomainNum,
		CartesianCell const & ThisCell
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	Real64
	EvaluateGroundSurfaceTemperature(
		int const DomainNum,
		CartesianCell const & cell
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	Real64
	EvaluateAdiabaticSurfaceTemperature(
		int const DomainNum,
		CartesianCell const & cell
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	Real64
	EvaluateBasementCellTemperature(
		int const DomainNum,
		CartesianCell const & cell
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	Real64
	GetBasementWallHeatFlux( int const DomainNum );

	//*********************************************************************************************!

	//*********************************************************************************************!

	Real64
	GetBasementFloorHeatFlux( int const DomainNum );

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	UpdateBasementSurfaceTemperatures( int const DomainNum );

	//*********************************************************************************************!

	//*********************************************************************************************!

	Real64
	EvaluateZoneInterfaceTemperature(
		int const DomainNum,
		CartesianCell const & cell
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	Real64
	GetZoneInterfaceHeatFlux( int const DomainNum );

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	UpdateZoneSurfaceTemperatures( int const DomainNum );

	//*********************************************************************************************!

	//*********************************************************************************************!

	Real64
	GetAverageTempByType(
		int const DomainNum,
		int const CellType
	);

	Real64
	GetAverageInterfaceTemp(
		int const DomainNum,
		int const CellType,
		int const CellType2
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	Real64
	EvaluateFarfieldBoundaryTemperature(
		int const DomainNum,
		CartesianCell const & cell
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	EvaluateFarfieldCharacteristics(
		int const DomainNum,
		CartesianCell const & cell,
		int const direction,
		Real64 & neighbortemp,
		Real64 & resistance
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	Real64
	GetFarfieldTemp(
		int const DomainNum,
		CartesianCell const & cell
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	PreparePipeCircuitSimulation(
		int const DomainNum,
		int const CircuitNum
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	PerformPipeCircuitSimulation(
		int const DomainNum,
		int const CircuitNum
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	PerformPipeCellSimulation(
		int const DomainNum,
		int const CircuitNum,
		CartesianCell & ThisCell,
		Real64 const FlowRate,
		Real64 const EnteringTemp
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	SimulateRadialToCartesianInterface(
		int const DomainNum,
		CartesianCell & ThisCell
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	SimulateOuterMostRadialSoilSlice(
		int const DomainNum,
		int const CircuitNum,
		CartesianCell & ThisCell
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	SimulateAllInteriorRadialSoilSlices( CartesianCell & ThisCell );

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	SimulateInnerMostRadialSoilSlice(
		int const DomainNum,
		int const CircuitNum,
		CartesianCell & ThisCell
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	SimulateRadialInsulationCell( CartesianCell & ThisCell );

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	SimulateRadialPipeCell(
		int const DomainNum,
		int const CircuitNum,
		CartesianCell & ThisCell,
		Real64 const ConvectionCoefficient
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	SimulateFluidCell(
		CartesianCell & ThisCell,
		Real64 const FlowRate,
		Real64 const ConvectionCoefficient,
		Real64 const EnteringFluidTemp
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	DoOneTimeInitializations(
		int const DomainNum,
		Optional < int const > CircuitNum
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	DoStartOfTimeStepInitializations(
		int const DomainNum,
		Optional < int const > CircuitNum
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	DoEndOfIterationOperations(
		int const DomainNum,
		bool & Finished
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	EvaluateSoilRhoCp(
		int const DomainNum,
		Optional< Real64 const > CellTemp = _,
		Optional< Real64 > rhoCp = _,
		Optional_bool_const InitOnly = _
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	SetAdditionalNeighborData(
		int const DomainNum,
		int const X,
		int const Y,
		int const Z,
		int const Direction,
		Real64 const Resistance,
		CartesianCell const & NeighborCell
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	EvaluateNeighborCharacteristics(
		int const DomainNum,
		CartesianCell const & ThisCell,
		int const CurDirection,
		Real64 & NeighborTemp,
		Real64 & Resistance,
		Optional_int NeighborX = _,
		Optional_int NeighborY = _,
		Optional_int NeighborZ = _
	);

	//*********************************************************************************************!

	//*********************************************************************************************!

	void
	EvaluateCellNeighborDirections(
		int const DomainNum,
		CartesianCell const & cell
	);

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

} // PlantPipingSystemsManager

} // EnergyPlus

#endif
