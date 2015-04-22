#ifndef DataSurfaceLists_hh_INCLUDED
#define DataSurfaceLists_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DataSurfaceLists {

	// Using/Aliasing

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
		std::string Name; // Name of the surface list
		int NumOfSurfaces; // Number of surfaces in the list
		Array1D_string SurfName; // Surfaces named in the list
		Array1D_int SurfPtr; // Location of surfaces in Surface derived type
		Array1D< Real64 > SurfFlowFrac; // Fraction of mass flow/length for a surface

		// Default Constructor
		SurfaceListData() :
			NumOfSurfaces( 0 )
		{}

		// Member Constructor
		SurfaceListData(
			std::string const & Name, // Name of the surface list
			int const NumOfSurfaces, // Number of surfaces in the list
			Array1_string const & SurfName, // Surfaces named in the list
			Array1_int const & SurfPtr, // Location of surfaces in Surface derived type
			Array1< Real64 > const & SurfFlowFrac // Fraction of mass flow/length for a surface
		) :
			Name( Name ),
			NumOfSurfaces( NumOfSurfaces ),
			SurfName( SurfName ),
			SurfPtr( SurfPtr ),
			SurfFlowFrac( SurfFlowFrac )
		{}

	};

	struct SlabListData
	{
		// Members
		std::string Name; // Name of the surface list
		int NumOfSurfaces; // Number of surfaces in the list
		Array1D_string SurfName; // Surfaces named in the list
		Array1D_int SurfPtr; // Location of surfaces in Surface derived type
		Array1D_string ZoneName; // Zone named in the list
		Array1D_int ZonePtr; // Location of Zone in Surface derived type
		Array1D< Real64 > CoreDiameter; // Fraction of mass flow/length for a surface
		Array1D< Real64 > CoreLength; // Fraction of mass flow/length for a surface
		Array1D< Real64 > CoreNumbers; // Fraction of mass flow/length for a surface
		Array1D_string SlabInNodeName; // Zone named in the list
		Array1D_string SlabOutNodeName; // Zone named in the list

		// Default Constructor
		SlabListData() :
			NumOfSurfaces( 0 )
		{}

		// Member Constructor
		SlabListData(
			std::string const & Name, // Name of the surface list
			int const NumOfSurfaces, // Number of surfaces in the list
			Array1_string const & SurfName, // Surfaces named in the list
			Array1_int const & SurfPtr, // Location of surfaces in Surface derived type
			Array1_string const & ZoneName, // Zone named in the list
			Array1_int const & ZonePtr, // Location of Zone in Surface derived type
			Array1< Real64 > const & CoreDiameter, // Fraction of mass flow/length for a surface
			Array1< Real64 > const & CoreLength, // Fraction of mass flow/length for a surface
			Array1< Real64 > const & CoreNumbers, // Fraction of mass flow/length for a surface
			Array1_string const & SlabInNodeName, // Zone named in the list
			Array1_string const & SlabOutNodeName // Zone named in the list
		) :
			Name( Name ),
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
	extern Array1D< SurfaceListData > SurfList;
	extern Array1D< SlabListData > SlabList;

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

} // DataSurfaceLists

} // EnergyPlus

#endif
