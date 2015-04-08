#ifndef MatrixDataManager_hh_INCLUDED
#define MatrixDataManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Array2S.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace MatrixDataManager {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	//INTEGER, PARAMETER :: OneDimensional = 1
	extern int const TwoDimensional;
	//INTEGER, PARAMETER :: ThreeDimensional = 3

	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:

	extern int NumMats; // number of matracies in input file

	// SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>:

	// todo, flush out the following routines, see CurveManager for patterns
	//PUBLIC GetMatrixValue
	//PUBLIC GetMatrixCheck
	//PUBLIC GetMatrixType
	//PUBLIC GetMatrixMinMaxValues
	//PUBLIC SetMatrixOutputMinMaxValues
	//PUBLIC GetMatrixName

	// Types

	struct MatrixDataStruct
	{
		// Members
		std::string Name; // Matrix Name
		int MatrixType;
		//REAL(r64), DIMENSION(:), ALLOCATABLE     :: Mat1D ! hold data if one dimensional
		Array2D< Real64 > Mat2D; // hold data if two dimensional
		//REAL(r64), DIMENSION(:,:,:), Allocatable :: Mat3D ! hold data if three dimensional

		// Default Constructor
		MatrixDataStruct() :
			MatrixType( 0 )
		{}

		// Member Constructor
		MatrixDataStruct(
			std::string const & Name, // Matrix Name
			int const MatrixType,
			Array2< Real64 > const & Mat2D // hold data if two dimensional
		) :
			Name( Name ),
			MatrixType( MatrixType ),
			Mat2D( Mat2D )
		{}

	};

	// Object Data
	extern Array1D< MatrixDataStruct > MatData;

	// Functions

	void
	GetMatrixInput();

	int
	MatrixIndex( std::string const & MatrixName );

	void
	Get2DMatrix(
		int const Idx, // pointer index to location in MatData
		Array2S< Real64 > Mat2D
	);

	void
	Get2DMatrixDimensions(
		int const Idx, // pointer index to location in MatData
		int & NumRows,
		int & NumCols
	);

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

} // MatrixDataManager

} // EnergyPlus

#endif
