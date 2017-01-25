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

} // ConductionTransferFunctionCalc

} // EnergyPlus

#endif
