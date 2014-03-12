#ifndef DataSurfaceLists_hh_INCLUDED
#define DataSurfaceLists_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/Fstring.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DataSurfaceLists {

	// Using/Aliasing
	using DataGlobals::MaxNameLength;

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:

	// DERIVED TYPE DEFINITIONS:

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:

	extern int NumOfSurfaceLists; // Number of surface lists in the user input file
	extern int NumOfSurfListVentSlab; // Number of surface lists in the user input file
	extern bool SurfaceListInputsFilled; // Set to TRUE after first pass through air loop

	//  CHARACTER(len=*), PARAMETER :: CurrentModuleObject = ' '
	// SUBROUTINE SPECIFICATIONS FOR MODULE DataSurfaceLists

	// Types

	struct SurfaceListData
	{
		// Members
		Fstring Name; // Name of the surface list
		int NumOfSurfaces; // Number of surfaces in the list
		FArray1D_Fstring SurfName; // Surfaces named in the list
		FArray1D_int SurfPtr; // Location of surfaces in Surface derived type
		FArray1D< Real64 > SurfFlowFrac; // Fraction of mass flow/length for a surface

		// Default Constructor
		SurfaceListData() :
			Name( MaxNameLength ),
			NumOfSurfaces( 0 ),
			SurfName( sFstring( MaxNameLength ) )
		{}

		// Member Constructor
		SurfaceListData(
			Fstring const & Name, // Name of the surface list
			int const NumOfSurfaces, // Number of surfaces in the list
			FArray1_Fstring const & SurfName, // Surfaces named in the list
			FArray1_int const & SurfPtr, // Location of surfaces in Surface derived type
			FArray1< Real64 > const & SurfFlowFrac // Fraction of mass flow/length for a surface
		) :
			Name( MaxNameLength, Name ),
			NumOfSurfaces( NumOfSurfaces ),
			SurfName( SurfName ),
			SurfPtr( SurfPtr ),
			SurfFlowFrac( SurfFlowFrac )
		{}

	};

	struct SlabListData
	{
		// Members
		Fstring Name; // Name of the surface list
		int NumOfSurfaces; // Number of surfaces in the list
		FArray1D_Fstring SurfName; // Surfaces named in the list
		FArray1D_int SurfPtr; // Location of surfaces in Surface derived type
		FArray1D_Fstring ZoneName; // Zone named in the list
		FArray1D_int ZonePtr; // Location of Zone in Surface derived type
		FArray1D< Real64 > CoreDiameter; // Fraction of mass flow/length for a surface
		FArray1D< Real64 > CoreLength; // Fraction of mass flow/length for a surface
		FArray1D< Real64 > CoreNumbers; // Fraction of mass flow/length for a surface
		FArray1D_Fstring SlabInNodeName; // Zone named in the list
		FArray1D_Fstring SlabOutNodeName; // Zone named in the list

		// Default Constructor
		SlabListData() :
			Name( MaxNameLength ),
			NumOfSurfaces( 0 ),
			SurfName( sFstring( MaxNameLength ) ),
			ZoneName( sFstring( MaxNameLength ) ),
			SlabInNodeName( sFstring( MaxNameLength ) ),
			SlabOutNodeName( sFstring( MaxNameLength ) )
		{}

		// Member Constructor
		SlabListData(
			Fstring const & Name, // Name of the surface list
			int const NumOfSurfaces, // Number of surfaces in the list
			FArray1_Fstring const & SurfName, // Surfaces named in the list
			FArray1_int const & SurfPtr, // Location of surfaces in Surface derived type
			FArray1_Fstring const & ZoneName, // Zone named in the list
			FArray1_int const & ZonePtr, // Location of Zone in Surface derived type
			FArray1< Real64 > const & CoreDiameter, // Fraction of mass flow/length for a surface
			FArray1< Real64 > const & CoreLength, // Fraction of mass flow/length for a surface
			FArray1< Real64 > const & CoreNumbers, // Fraction of mass flow/length for a surface
			FArray1_Fstring const & SlabInNodeName, // Zone named in the list
			FArray1_Fstring const & SlabOutNodeName // Zone named in the list
		) :
			Name( MaxNameLength, Name ),
			NumOfSurfaces( NumOfSurfaces ),
			SurfName( SurfName ),
			SurfPtr( SurfPtr ),
			ZoneName( ZoneName ),
			ZonePtr( ZonePtr ),
			CoreDiameter( CoreDiameter ),
			CoreLength( CoreLength ),
			CoreNumbers( CoreNumbers ),
			SlabInNodeName( SlabInNodeName ),
			SlabOutNodeName( SlabOutNodeName )
		{}

	};

	// Object Data
	extern FArray1D< SurfaceListData > SurfList;
	extern FArray1D< SlabListData > SlabList;

	// Functions

	void
	GetSurfaceListsInputs();

	int
	GetNumberOfSurfaceLists();

	int
	GetNumberOfSurfListVentSlab();

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in EnergyPlus.f90.

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

} // DataSurfaceLists

} // EnergyPlus

#endif
