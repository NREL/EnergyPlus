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

#ifndef AirflowNetworkSolver_hh_INCLUDED
#define AirflowNetworkSolver_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Array2D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

// define this variable to get new code, commenting should yield original
#define SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS

namespace AirflowNetworkSolver {

	// Data
	extern int NetworkNumOfLinks;
	extern int NetworkNumOfNodes;

	extern int const NrInt; // Number of intervals for a large opening

	// Common block AFEDAT
	extern Array1D< Real64 > AFECTL;
	extern Array1D< Real64 > AFLOW2;
	extern Array1D< Real64 > AFLOW;
	extern Array1D< Real64 > PS;
	extern Array1D< Real64 > PW;

	// Common block CONTRL
	extern Real64 PB;
	extern int LIST;

	// Common block ZONL
	extern Array1D< Real64 > RHOZ;
	extern Array1D< Real64 > SQRTDZ;
	extern Array1D< Real64 > VISCZ;
	extern Array1D< Real64 > SUMAF;
	extern Array1D< Real64 > TZ; // Temperature [C]
	extern Array1D< Real64 > WZ; // Humidity ratio [kg/kg]
	extern Array1D< Real64 > PZ; // Pressure [Pa]

	// Other array variables
	extern Array1D_int ID;
	extern Array1D_int IK;
	extern Array1D< Real64 > AD;
	extern Array1D< Real64 > AU;

#ifdef SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS
	extern Array1D_int newIK; // noel
	extern Array1D< Real64 > newAU; // noel
#endif

	//REAL(r64), ALLOCATABLE, DIMENSION(:) :: AL
	extern Array1D< Real64 > SUMF;
	extern int Unit11;
	extern int Unit21;

	// Large opening variables
	extern Array1D< Real64 > DpProf; // Differential pressure profile for Large Openings [Pa]
	extern Array1D< Real64 > RhoProfF; // Density profile in FROM zone [kg/m3]
	extern Array1D< Real64 > RhoProfT; // Density profile in TO zone [kg/m3]
	extern Array2D< Real64 > DpL; // Array of stack pressures in link

	// Functions

	void
	AllocateAirflowNetworkData();

	void
	InitAirflowNetworkData();

	void
	SETSKY();

	void
	AIRMOV();

	void
	SOLVZP(
		Array1A_int IK, // pointer to the top of column/row "K"
		Array1A< Real64 > AD, // the main diagonal of [A] before and after factoring
		Array1A< Real64 > AU, // the upper triangle of [A] before and after factoring
		int & ITER // number of iterations
	);

	void
	FILJAC(
		int const NNZE, // number of nonzero entries in the "AU" array.
		int const LFLAG // if = 1, use laminar relationship (initialization).
	);

	void
	AFEPLR(
		int const j, // Component number
		int const LFLAG, // Initialization flag.If = 1, use laminar relationship
		Real64 const PDROP, // Total pressure drop across a component (P1 - P2) [Pa]
		int const i, // Linkage number
		int const n, // Node 1 number
		int const M, // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	);

	void
	AFESCR(
		int const j, // Component number
		int const LFLAG, // Initialization flag.If = 1, use laminar relationship
		Real64 const PDROP, // Total pressure drop across a component (P1 - P2) [Pa]
		int const i, // Linkage number
		int const n, // Node 1 number
		int const M, // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	);

	void
	AFEDWC(
		int const j, // Component number
		int const LFLAG, // Initialization flag.If = 1, use laminar relationship
		Real64 const PDROP, // Total pressure drop across a component (P1 - P2) [Pa]
		int const i, // Linkage number
		int const n, // Node 1 number
		int const M, // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	);

	void
	AFESOP(
		int const j, // Component number
		int const LFLAG, // Initialization flag.If = 1, use laminar relationship
		Real64 const PDROP, // Total pressure drop across a component (P1 - P2) [Pa]
		int const i, // Linkage number
		int const n, // Node 1 number
		int const M, // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	);

	void
	AFECFR(
		int const j, // Component number
		int const LFLAG, // Initialization flag.If = 1, use laminar relationship
		Real64 const PDROP, // Total pressure drop across a component (P1 - P2) [Pa]
		int const i, // Linkage number
		int const n, // Node 1 number
		int const M, // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	);

	void
	AFEFAN(
		int const JA, // Component number
		int const LFLAG, // Initialization flag.If = 1, use laminar relationship
		Real64 const PDROP, // Total pressure drop across a component (P1 - P2) [Pa]
		int const i, // Linkage number
		int const n, // Node 1 number
		int const M, // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	);

	// The above subroutine is not used. Leave it for the time being and revise later.

	void
	AFECPF(
		int const j, // Component number
		int const LFLAG, // Initialization flag.If = 1, use laminar relationship
		Real64 const PDROP, // Total pressure drop across a component (P1 - P2) [Pa]
		int const i, // Linkage number
		int const n, // Node 1 number
		int const M, // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	);

	// Leave it for the time being and revise later. Or drop this component ???????????

	void
	AFEDMP(
		int const j, // Component number
		int const LFLAG, // Initialization flag.If = 1, use laminar relationship
		Real64 const PDROP, // Total pressure drop across a component (P1 - P2) [Pa]
		int const i, // Linkage number
		int const n, // Node 1 number
		int const M, // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	);

	void
	AFESEL(
		int const j, // Component number
		int const LFLAG, // Initialization flag.If = 1, use laminar relationship
		Real64 const PDROP, // Total pressure drop across a component (P1 - P2) [Pa]
		int const i, // Linkage number
		int const n, // Node 1 number
		int const M, // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	);

	void
	AFEELR(
		int const j, // Component number
		int const LFLAG, // Initialization flag.If = 1, use laminar relationship
		Real64 const PDROP, // Total pressure drop across a component (P1 - P2) [Pa]
		int const i, // Linkage number
		int const n, // Node 1 number
		int const M, // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	);

	void
	AFECPD(
		int const j, // Component number
		int const LFLAG, // Initialization flag.If = 1, use laminar relationship
		Real64 & PDROP, // Total pressure drop across a component (P1 - P2) [Pa]
		int const i, // Linkage number
		int const n, // Node 1 number
		int const M, // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	);

	void
	AFECOI(
		int const j, // Component number
		int const LFLAG, // Initialization flag.If = 1, use laminar relationship
		Real64 const PDROP, // Total pressure drop across a component (P1 - P2) [Pa]
		int const i, // Linkage number
		int const n, // Node 1 number
		int const M, // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	);

	void
	AFETMU(
		int const j, // Component number
		int const LFLAG, // Initialization flag.If = 1, use laminar relationship
		Real64 const PDROP, // Total pressure drop across a component (P1 - P2) [Pa]
		int const i, // Linkage number
		int const n, // Node 1 number
		int const M, // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	);

	void
	AFEEXF(
		int const j, // Component number
		int const LFLAG, // Initialization flag.If = 1, use laminar relationship
		Real64 const PDROP, // Total pressure drop across a component (P1 - P2) [Pa]
		int const i, // Linkage number
		int const n, // Node 1 number
		int const M, // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	);

	void
	AFEHEX(
		int const j, // Component number
		int const LFLAG, // Initialization flag.If = 1, use laminar relationship
		Real64 const PDROP, // Total pressure drop across a component (P1 - P2) [Pa]
		int const i, // Linkage number
		int const n, // Node 1 number
		int const M, // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	);

	void
	AFEHOP(
		int const j, // Component number
		int const LFLAG, // Initialization flag.If = 1, use laminar relationship
		Real64 const PDROP, // Total pressure drop across a component (P1 - P2) [Pa]
		int const i, // Linkage number
		int const n, // Node 1 number
		int const M, // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	);

	void
	GenericCrack(
		Real64 & coef, // Flow coefficient
		Real64 const expn, // Flow exponent
		int const LFLAG, // Initialization flag.If = 1, use laminar relationship
		Real64 const PDROP, // Total pressure drop across a component (P1 - P2) [Pa]
		int const n, // Node 1 number
		int const M, // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	);

	void
	FACSKY(
		Array1A< Real64 > AU, // the upper triangle of [A] before and after factoring
		Array1A< Real64 > AD, // the main diagonal of [A] before and after factoring
		Array1A< Real64 > AL, // the lower triangle of [A] before and after factoring
		Array1A_int const IK, // pointer to the top of column/row "K"
		int const NEQ, // number of equations
		int const NSYM // symmetry:  0 = symmetric matrix, 1 = non-symmetric
	);

	void
	SLVSKY(
		Array1A< Real64 > const AU, // the upper triangle of [A] before and after factoring
		Array1A< Real64 > const AD, // the main diagonal of [A] before and after factoring
		Array1A< Real64 > const AL, // the lower triangle of [A] before and after factoring
		Array1A< Real64 > B, // "B" vector (input); "X" vector (output).
		Array1A_int const IK, // pointer to the top of column/row "K"
		int const NEQ, // number of equations
		int const NSYM // symmetry:  0 = symmetric matrix, 1 = non-symmetric
	);

	void
	FILSKY(
		Array1A< Real64 > const X, // element array (row-wise sequence)
		Array1A_int const LM, // location matrix
		Array1A_int const IK, // pointer to the top of column/row "K"
		Array1A< Real64 > AU, // the upper triangle of [A] before and after factoring
		Array1A< Real64 > AD, // the main diagonal of [A] before and after factoring
		int const FLAG // mode of operation
	);

	void
	DUMPVD(
		std::string const & S, // Description
		Array1A< Real64 > const V, // Output values
		int const n, // Array size
		int const UOUT // Output file unit
	);

	void
	DUMPVR(
		std::string const & S, // Description
		Array1A< Real64 > const V, // Output values
		int const n, // Array size
		int const UOUT // Output file unit
	);

	void
	AFEDOP(
		int const j, // Component number
		int const LFLAG, // Initialization flag.If = 1, use laminar relationship
		Real64 const PDROP, // Total pressure drop across a component (P1 - P2) [Pa]
		int const IL, // Linkage number
		int const n, // Node 1 number
		int const M, // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	);

	void
	PresProfile(
		int const il, // Linkage number
		int const Pprof, // Opening number
		Real64 const G, // gravitation field strength [N/kg]
		Array1A< Real64 > const DpF, // Stack pressures at start heights of Layers
		Array1A< Real64 > const DpT, // Stack pressures at start heights of Layers
		Array1A< Real64 > const BetaF, // Density gradients in the FROM zone (starting at linkheight) [Kg/m3/m]
		Array1A< Real64 > const BetaT, // Density gradients in the TO zone (starting at linkheight) [Kg/m3/m]
		Array1A< Real64 > const RhoStF, // Density at the start heights of Layers in the FROM zone
		Array1A< Real64 > const RhoStT, // Density at the start heights of Layers in the TO zone
		int const From, // Number of FROM zone
		int const To, // Number of To zone
		Real64 const ActLh, // Actual height of opening [m]
		Real64 const OwnHeightFactor // Cosine of deviation angle of the opening plane from the vertical direction
	);

	void
	PStack();

	Real64
	psz(
		Real64 const Pz0, // Pressure at altitude z0 [Pa]
		Real64 const Rho0, // density at altitude z0 [kg/m3]
		Real64 const beta, // density gradient [kg/m4]
		Real64 const z0, // reference altitude [m]
		Real64 const z, // altitude[m]
		Real64 const g // gravity field strength [N/kg]
	);

	void
	LClimb(
		Real64 const G, // gravity field strength [N/kg]
		Real64 & Rho, // Density link level (initialized with rho zone) [kg/m3]
		Real64 const Z, // Height of the link above the zone reference [m]
		Real64 & T, // temperature at link level [C]
		Real64 & X, // absolute humidity at link level [kg/kg]
		Real64 & Dp, // Stackpressure to the linklevel [Pa]
		int const zone, // Zone number
		Real64 const PZ, // Zone Pressure (reflevel) [Pa]
		Real64 const Pbz, // Barometric pressure at entrance level [Pa]
		Real64 & RhoDr // Air density of dry air on the link level used
	);

	//*****************************************************************************************

} // AirflowNetworkSolver

} // EnergyPlus

#endif
