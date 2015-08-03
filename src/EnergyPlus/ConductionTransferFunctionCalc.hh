#ifndef ConductionTransferFunctionCalc_hh_INCLUDED
#define ConductionTransferFunctionCalc_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Array3D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace ConductionTransferFunctionCalc {

	// Data
	// MODULE PARAMETER DEFINITIONS:
	//INTEGER, PRIVATE, PARAMETER :: MaxTotNodes = 75   ! Maximum total number of
	// nodes per construction.  This limit is a compromise between faster,
	// less accurate solutions and slower, possibly more accurate answers.

	extern int const NumOfPerpendNodes; // Number of nodes in the direction
	// perpendicular to the main direction of heat transfer.  This is only used
	// when a two-dimensional solution has been requested for a construction
	// with a heat source/sink.

	// DERIVED TYPE DEFINITIONS
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:
	extern Array2D< Real64 > AExp; // Exponential of AMat
	extern Array2D< Real64 > AInv; // Inverse of AMat
	extern Array2D< Real64 > AMat; // "A" matrix from Seem's dissertation
	// (constant coefficients of linear system)
	extern Array1D< Real64 > BMat; // "B" matrix of state space method (non-zero elements)
	extern Array1D< Real64 > CMat; // "C" matrix of state space method (non-zero elements)
	extern Array1D< Real64 > DMat; // "D" matrix of state space method (non-zero elements)
	extern Array1D< Real64 > e; // Coefficients for the surface flux history term
	extern Array2D< Real64 > Gamma1; // Intermediate calculation array corresponding to a term
	// in Seem's dissertation
	extern Array2D< Real64 > Gamma2; // Intermediate calculation array corresponding to a term
	// in Seem's dissertation
	extern int NodeSource; // Node at which a source or sink is present
	extern int NodeUserTemp; // Node where user wishes to calculate a temperature
	// (for constructions with sources/sinks only)
	extern int rcmax; // Total number of nodes in the construct (<= MaxTotNodes)
	extern Array3D< Real64 > s; // Coefficients for the surface temperature history terms
	extern Array2D< Real64 > s0; // Coefficients for the current surface temperature terms
	extern Real64 TinyLimit;
	extern Array2D< Real64 > IdenMatrix; // Identity Matrix

	// SUBROUTINE SPECIFICATIONS FOR MODULE ConductionTransferFunctionCalc

	// Functions

	void
	InitConductionTransferFunctions();

	void
	CalculateExponentialMatrix( Real64 & delt ); // Time step of the resulting CTFs

	void
	CalculateInverseMatrix();

	void
	CalculateGammas(
		Real64 const delt, // Time increment in fraction of an hour
		int const SolutionDimensions // Integer relating whether a 1- or 2-D solution is required
	);

	void
	CalculateCTFs(
		int & nrf, // Number of response factor terms
		int const SolutionDimensions // Integer relating whether a 1- or 2-D solution is required
	);

	void
	ReportCTFs( bool const DoReportBecauseError );

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

} // ConductionTransferFunctionCalc

} // EnergyPlus

#endif
