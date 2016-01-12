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

#ifndef SurfaceGeometry_hh_INCLUDED
#define SurfaceGeometry_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Array1S.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataSurfaces.hh>
#include <DataVectorTypes.hh>

namespace EnergyPlus {

namespace SurfaceGeometry {

	// Using/Aliasing
	using DataSurfaces::SurfaceData;
	using DataVectorTypes::Vector;

	// Data
	//MODULE PARAMETER DEFINITIONS
	extern Array1D_string const BaseSurfCls;
	extern Array1D_string const SubSurfCls;
	extern Array1D_int const BaseSurfIDs;

	extern Array1D_int const SubSurfIDs;

	extern int const UnenteredAdjacentZoneSurface; // allows users to enter one zone surface ("Zone")
	// referencing another in adjacent zone
	extern int const UnreconciledZoneSurface; // interim value between entering surfaces ("Surface") and reconciling
	// surface names in other zones

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	// Following are used only during getting vertices, so are module variables here.
	extern Real64 CosBldgRelNorth; // Cosine of the building rotation (relative north) (includes appendix G rotation)
	extern Real64 SinBldgRelNorth; // Sine of the building rotation (relative north)   (includes appendix G rotation)
	extern Real64 CosBldgRotAppGonly; // Cosine of the building rotation for appendix G only(relative north)
	extern Real64 SinBldgRotAppGonly; // Sine of the building rotation for appendix G only (relative north)
	extern Array1D< Real64 > CosZoneRelNorth; // Cosine of the zone rotation (relative north)
	extern Array1D< Real64 > SinZoneRelNorth; // Sine of the zone rotation (relative north)

	extern bool NoGroundTempObjWarning; // This will cause a warning to be issued if surfaces with "Ground"
	// outside environment are used but no ground temperature object was input.
	extern bool NoFCGroundTempObjWarning; // This will cause a warning to be issued if surfaces with "GroundFCfactorMethod"
	// outside environment are used but no FC ground temperatures was input.
	extern bool RectSurfRefWorldCoordSystem; // GlobalGeometryRules=World (true) or Relative (false)
	extern int Warning1Count; // counts of Modify Window 5/6 windows
	extern int Warning2Count; // counts of overriding exterior windows with Window 5/6 glazing systems
	extern int Warning3Count; // counts of overriding interior windows with Window 5/6 glazing systems

	//SUBROUTINE SPECIFICATIONS FOR MODULE SurfaceGeometry

	// Object Data
	extern Array1D< SurfaceData > SurfaceTmp; // Allocated/Deallocated during input processing

	// Functions

	// Clears the global data in HeatBalanceManager.
	// Needed for unit tests, should not be normally called.
	void
	clear_state();

	void
	SetupZoneGeometry( bool & ErrorsFound );

	void
	AllocateModuleArrays();

	void
	GetSurfaceData( bool & ErrorsFound ); // If errors found in input


	void
	checkSubSurfAzTiltNorm(
		SurfaceData & baseSurface, // Base surface data (in)
		SurfaceData & subSurface, // Subsurface data (in)
		bool & surfaceError // True if there is subsurface error that requires a fatal
	);

	void
	GetGeometryParameters( bool & ErrorsFound ); // set to true if errors found during input

	void
	GetDetShdSurfaceData(
		bool & ErrorsFound, // Error flag indicator (true if errors found)
		int & SurfNum, // Count of Current SurfaceNumber
		int const TotDetachedFixed, // Number of Fixed Detached Shading Surfaces to obtain
		int const TotDetachedBldg // Number of Building Detached Shading Surfaces to obtain
	);

	void
	GetRectDetShdSurfaceData(
		bool & ErrorsFound, // Error flag indicator (true if errors found)
		int & SurfNum, // Count of Current SurfaceNumber
		int const TotRectDetachedFixed, // Number of Fixed Detached Shading Surfaces to obtain
		int const TotRectDetachedBldg // Number of Building Detached Shading Surfaces to obtain
	);

	void
	GetHTSurfaceData(
		bool & ErrorsFound, // Error flag indicator (true if errors found)
		int & SurfNum, // Count of Current SurfaceNumber
		int const TotHTSurfs, // Number of Heat Transfer Base Surfaces to obtain
		int const TotDetailedWalls, // Number of Wall:Detailed items to obtain
		int const TotDetailedRoofs, // Number of RoofCeiling:Detailed items to obtain
		int const TotDetailedFloors, // Number of Floor:Detailed items to obtain
		Array1S_string const BaseSurfCls, // Valid Classes for Base Surfaces
		Array1S_int const BaseSurfIDs,
		int & NeedToAddSurfaces // Number of surfaces to add, based on unentered IZ surfaces
	);

	void
	GetRectSurfaces(
		bool & ErrorsFound, // Error flag indicator (true if errors found)
		int & SurfNum, // Count of Current SurfaceNumber
		int const TotRectExtWalls, // Number of Exterior Walls to obtain
		int const TotRectIntWalls, // Number of Adiabatic Walls to obtain
		int const TotRectIZWalls, // Number of Interzone Walls to obtain
		int const TotRectUGWalls, // Number of Underground to obtain
		int const TotRectRoofs, // Number of Roofs to obtain
		int const TotRectCeilings, // Number of Adiabatic Ceilings to obtain
		int const TotRectIZCeilings, // Number of Interzone Ceilings to obtain
		int const TotRectGCFloors, // Number of Floors with Ground Contact to obtain
		int const TotRectIntFloors, // Number of Adiabatic Walls to obtain
		int const TotRectIZFloors, // Number of Interzone Floors to obtain
		Array1S_int const BaseSurfIDs, // ID Assignments for valid surface classes
		int & NeedToAddSurfaces // Number of surfaces to add, based on unentered IZ surfaces
	);

	void
	MakeRectangularVertices(
		int const SurfNum,
		Real64 const XCoord,
		Real64 const YCoord,
		Real64 const ZCoord,
		Real64 const Length,
		Real64 const Height,
		bool const SurfWorldCoordSystem
	);

	void
	GetHTSubSurfaceData(
		bool & ErrorsFound, // Error flag indicator (true if errors found)
		int & SurfNum, // Count of Current SurfaceNumber
		int const TotHTSubs, // Number of Heat Transfer SubSurfaces to obtain
		Array1S_string const SubSurfCls, // Valid Classes for Sub Surfaces
		Array1S_int const SubSurfIDs, // ID Assignments for valid sub surface classes
		int & AddedSubSurfaces, // Subsurfaces added when windows reference Window5
		int & NeedToAddSurfaces // Number of surfaces to add, based on unentered IZ surfaces
	);

	void
	GetRectSubSurfaces(
		bool & ErrorsFound, // Error flag indicator (true if errors found)
		int & SurfNum, // Count of Current SurfaceNumber
		int const TotWindows, // Number of Window SubSurfaces to obtain
		int const TotDoors, // Number of Door SubSurfaces to obtain
		int const TotGlazedDoors, // Number of Glass Door SubSurfaces to obtain
		int const TotIZWindows, // Number of Interzone Window SubSurfaces to obtain
		int const TotIZDoors, // Number of Interzone Door SubSurfaces to obtain
		int const TotIZGlazedDoors, // Number of Interzone Glass Door SubSurfaces to obtain
		Array1S_int const SubSurfIDs, // ID Assignments for valid sub surface classes
		int & AddedSubSurfaces, // Subsurfaces added when windows reference Window5
		int & NeedToAddSubSurfaces // Number of surfaces to add, based on unentered IZ surfaces
	);

	void
	CheckWindowShadingControlFrameDivider(
		std::string const & cRoutineName, // routine name calling this one (for error messages)
		bool & ErrorsFound, // true if errors have been found or are found here
		int const SurfNum, // current surface number
		int const FrameField // field number for frame/divider
	);

	void
	CheckSubSurfaceMiscellaneous(
		std::string const & cRoutineName, // routine name calling this one (for error messages)
		bool & ErrorsFound, // true if errors have been found or are found here
		int const SurfNum, // current surface number
		std::string const & SubSurfaceName, // name of the surface
		std::string const & SubSurfaceConstruction, // name of the construction
		int & AddedSubSurfaces
	);

	void
	MakeRelativeRectangularVertices(
		int const BaseSurfNum, // Base surface
		int const SurfNum,
		Real64 const XCoord,
		Real64 const ZCoord,
		Real64 const Length,
		Real64 const Height
	);

	void
	GetAttShdSurfaceData(
		bool & ErrorsFound, // Error flag indicator (true if errors found)
		int & SurfNum, // Count of Current SurfaceNumber
		int const TotShdSubs // Number of Attached Shading SubSurfaces to obtain
	);

	void
	GetSimpleShdSurfaceData(
		bool & ErrorsFound, // Error flag indicator (true if errors found)
		int & SurfNum, // Count of Current SurfaceNumber
		int const TotOverhangs, // Number of Overhangs to obtain
		int const TotOverhangsProjection, // Number of Overhangs (projection) to obtain
		int const TotFins, // Number of Fins to obtain
		int const TotFinsProjection // Number of Fins (projection) to obtain
	);

	void
	GetIntMassSurfaceData(
		bool & ErrorsFound, // Error flag indicator (true if errors found)
		int & SurfNum, // Count of Current SurfaceNumber
		int const TotIntMass // Number of Internal Mass Surfaces to obtain
	);

	void
	GetShadingSurfReflectanceData( bool & ErrorsFound ); // If errors found in input

	void
	GetHTSurfExtVentedCavityData( bool & ErrorsFound ); // Error flag indicator (true if errors found)

	void
	GetSurfaceHeatTransferAlgorithmOverrides( bool & ErrorsFound );

	void
	GetVertices(
		int const SurfNum, // Current surface number
		int const NSides, // Number of sides to figure
		Array1S< Real64 > const Vertices // Vertices, in specified order
	);

	void
	ReverseAndRecalculate(
		int const SurfNum, // Surface number for the surface
		int const NSides, // number of sides to surface
		Real64 & SurfAzimuth, // Surface Facing angle (will be 0 for roofs/floors)
		Real64 & SurfTilt // Surface tilt (
	);

	void
	MakeMirrorSurface( int & SurfNum ); // In=>Surface to Mirror, Out=>new Surface index

	void
	GetWindowShadingControlData( bool & ErrorsFound ); // If errors found in input

	void
	GetStormWindowData( bool & ErrorsFound ); // If errors found in input

	void
	GetWindowGapAirflowControlData( bool & ErrorsFound ); // If errors found in input

	void
	GetOSCData( bool & ErrorsFound );

	void
	GetOSCMData( bool & ErrorsFound );

	void
	GetMovableInsulationData( bool & ErrorsFound ); // If errors found in input

	void
	CalculateZoneVolume(
		bool & ErrorsFound, // If errors found in input
		Array1S_bool const CeilingHeightEntered
	);

	void
	ProcessSurfaceVertices(
		int const ThisSurf, // Surface Number
		bool & ErrorsFound
	);

	void
	CalcCoordinateTransformation(
		int const SurfNum, // Surface Number
		Vector & CompCoordTranslVector // Coordinate Translation Vector
	);

	void
	CreateShadedWindowConstruction(
		int const SurfNum, // Surface number
		int const WSCPtr, // Pointer to WindowShadingControl for SurfNum
		int const ShDevNum // Shading device material number for WSCptr
	);

	void
	CreateStormWindowConstructions();

	void
	ModifyWindow(
		int const SurfNum, // SurfNum has construction of glazing system from Window5 Data File;
		bool & ErrorsFound, // Set to true if errors found
		int & AddedSubSurfaces // Subsurfaces added when window references a
	);

	void
	AddWindow(
		int const SurfNum, // SurfNum has construction of glazing system from Window5 Data File;
		bool & ErrorsFound, // Set to true if errors found
		int & AddedSubSurfaces // Subsurfaces added when window references a
	);

	void
	TransformVertsByAspect(
		int const SurfNum, // Current surface number
		int const NSides // Number of sides to figure
	);

	void
	CalcSurfaceCentroid();

	void
	SetupShadeSurfacesForSolarCalcs();

	void
	CheckConvexity(
		int const SurfNum, // Current surface number
		int const NSides // Number of sides to figure
	);

	bool
	isRectangle(
		int const ThisSurf // Current surface number
	);
		
} // SurfaceGeometry

} // EnergyPlus

#endif
