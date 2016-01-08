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

// C++ Headers
#include <cassert>
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <ConductionTransferFunctionCalc.hh>
#include <DataConversions.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataPrecisionGlobals.hh>
#include <DisplayRoutines.hh>
#include <General.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace ConductionTransferFunctionCalc {

	// MODULE INFORMATION:
	//       AUTHOR         Rick Strand
	//       DATE WRITTEN   January 1997
	//       MODIFIED       June-July 2000, RKS
	//       RE-ENGINEERED  June 1996, February 1997, August 1997, RKS
	//       RE-ENGINEERED  November 1999, LKL

	// PURPOSE OF THIS MODULE:
	// This module calculates the conduction transfer functions (CTFs) for
	// all building constructions.

	// METHODOLOGY EMPLOYED:
	// This subroutine uses the state space method of calculating CTFs.
	// The state space method involves imposing a finite difference grid
	// to a solution space (i.e., a building construction, inside to
	// outside surface).  The finite difference grid is only used to
	// derive a system of differential equations.  This first order
	// system can then be solved using matrix algebra.  In this
	// implementation of the state space method, a conversion from the
	// internal units of EnergyPlus (SI) to English units is used, This
	// is done due to observations made by Russ Taylor that the solution
	// method was not as stable numerically when SI units were used.

	// REFERENCES:
	// While there are several important references on the state space
	// method, the most definitive reference which includes details on
	// implementing the state space method is:
	// Seem, J.E.  1987.  Modeling of Heat Transfer in Buildings, Ph.D.
	// Dissertation, Department of Mechanical Engineering, University of
	// Wisconsin-Madison.

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGlobals;
	using namespace DataHeatBalance; // This is the Heat balance super block data-only module

	// Data
	// MODULE PARAMETER DEFINITIONS:
	//INTEGER, PRIVATE, PARAMETER :: MaxTotNodes = 75   ! Maximum total number of
	// nodes per construction.  This limit is a compromise between faster,
	// less accurate solutions and slower, possibly more accurate answers.

	int const NumOfPerpendNodes( 7 ); // Number of nodes in the direction
	// perpendicular to the main direction of heat transfer.  This is only used
	// when a two-dimensional solution has been requested for a construction
	// with a heat source/sink.

	// DERIVED TYPE DEFINITIONS
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:
	Array2D< Real64 > AExp; // Exponential of AMat
	Array2D< Real64 > AInv; // Inverse of AMat
	Array2D< Real64 > AMat; // "A" matrix from Seem's dissertation
	// (constant coefficients of linear system)
	Array1D< Real64 > BMat( 3 ); // "B" matrix of state space method (non-zero elements)
	Array1D< Real64 > CMat( 2 ); // "C" matrix of state space method (non-zero elements)
	Array1D< Real64 > DMat( 2 ); // "D" matrix of state space method (non-zero elements)
	Array1D< Real64 > e; // Coefficients for the surface flux history term
	Array2D< Real64 > Gamma1; // Intermediate calculation array corresponding to a term
	// in Seem's dissertation
	Array2D< Real64 > Gamma2; // Intermediate calculation array corresponding to a term
	// in Seem's dissertation
	int NodeSource; // Node at which a source or sink is present
	int NodeUserTemp; // Node where user wishes to calculate a temperature
	// (for constructions with sources/sinks only)
	int rcmax; // Total number of nodes in the construct (<= MaxTotNodes)
	Array3D< Real64 > s; // Coefficients for the surface temperature history terms
	Array2D< Real64 > s0( 3, 4 ); // Coefficients for the current surface temperature terms
	Real64 TinyLimit;
	Array2D< Real64 > IdenMatrix; // Identity Matrix

	// SUBROUTINE SPECIFICATIONS FOR MODULE ConductionTransferFunctionCalc

	// MODULE SUBROUTINES:

	// Functions

	void
	InitConductionTransferFunctions()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   June 1990
		//       MODIFIED       July 1994, LKL, cosmetic and improve execution time
		//                      Dec 1995, Apr 1996, RKS, cosmetic and clean-up changes, changes to allow proper
		//                       handling of resistive layers
		//                      June 2000, RKS, addition of QTFs (both 1- and 2-D solutions for constructions
		//                       with embedded/internal heat sources/sinks)
		//                      July 2010-August 2011, RKS, R-value only layer enhancement
		//       RE-ENGINEERED  June 1996, February 1997, August-October 1997, RKS; Nov 1999, LKL

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine serves as the main drive for the
		// calculation of Conduction Transfer Functions (CTFs)
		// using the state space method.

		// METHODOLOGY EMPLOYED:
		// The basic steps of this routine (which may be a little difficult
		// to decipher until another major revision is done) are:
		//   1. Determine if enough material info has been entered
		//   2. Determine whether construct is (a) all resistive,
		//      (b) the reverse of a previously calculated construct, or
		//      (c) neither (a) nor (b), i.e. a layer for which CTFs must
		//      be calculated.
		//   3. If the answer to 2 is (a), calculate the overall resistance
		//      and use this as the CTF (steady state conduction).
		//   4. If the answer to 2 is (b), transfer the CTFs for the reverse
		//      construction to the CTF arrays for this construct (reversing
		//      the inside and outside terms).
		//   5. If the answer to 2 is (c), calculate the CTFs using the state
		//      space method described below.
		// The state space method of calculating CTFs involves
		// applying a finite difference grid to a multilayered
		// building element and performing linear algebra on the
		// resulting system of equations (in matrix form).
		// CTFs must be calculated for non-reversed layers which
		// have an appreciable thermal mass.  A conversion from
		// SI units to English units is made due to concerns
		// about round off problems noted in earlier version of
		// this subroutine.

		// REFERENCES:
		// Seem, J.E.  "Modeling of Heat Transfer in Buildings",
		//  Department of Mechanical Engineering, University of
		//  Wisconsin-Madison, 1987.
		// Strand, R.K. "Testing Design Description for the CTF
		//  Calculation Code in BEST", BSO internal document,
		//  May/June 1996.
		// Strand, R.K. "Heat Source Transfer Functions and Their
		//  Applicatoin to Low Temperature Radiant Heating System",
		//  Ph.D. Dissertation, Department of Mechanical and
		//  Industrial Engineering, University of Illinois at
		//  Urbana-Champaign, 1995.

		// Using/Aliasing
		using namespace DataConversions;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const PhysPropLimit( 1.0e-6 ); // Physical properties limit.
		// This is more or less the traditional value from BLAST.

		Real64 const RValueLowLimit( 1.0e-3 ); // Physical properties limit for R-value only layers
		// This value was based on trial and error related to CR 7791 where a
		// user had entered a "no insulation" layer with an R-value of 1.0E-05.
		// Some trial and error established this as a potential value though
		// there is no guarantee that this is a good value.

		int const MinNodes( 6 ); // Minimum number of state space nodes
		// per layer.  This value was chosen based on experience with IBLAST.

		Real64 const MaxAllowedCTFSumError( 0.01 ); // Allow a 1 percent
		// difference between the CTF series summations.  If the difference is
		// greater than this, then the coefficients will not yield a valid steady
		// state solution.

		Real64 const MaxAllowedTimeStep( 4.0 ); // Sets the maximum allowed time step
		// for CTF calculations to be 4 hours.  This is done in response to some
		// rare situations where odd or faulty input will cause the routine to
		// go off and get some huge time step (in excess of 20 hours).  This value
		// is a compromise that does not really solve any input problems.  One run
		// indicated that 2 meters of concrete will result in a time step of slightly
		// more than 3 hours.  So, 4 hours was arbitrarily picked as a ceiling for
		// time steps so that an error message can be produced to warn the user
		// that something isn't right.  Note that the 4 hour limit does not guarantee
		// that problems won't exist and it does not necessarily avoid any problems
		// that interpolated temperature histories might cause.

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Array1D_int AdjacentResLayerNum( MaxLayersInConstruct ); // Layers that are adjacent to each other which are resistive
		// only can and should be combined
		int AdjLayer; // Loop counter for adjacent resistance-only layers
		Real64 amatx; // Intermediate calculation variable
		Real64 amatxx; // Intermediate calculation variable
		Real64 amaty; // Intermediate calculation variable
		Real64 BiggestSum; // Largest CTF series summation (maximum of SumXi, SumYi, and SumZi)
		Real64 cap; // Thermal capacitance of a node (intermediate calculation)
		Real64 capavg; // Thermal capacitance of a node (average value for a node at an interface)
		Real64 cnd; // Total thermal conductance (1/Rtot) of the bldg element
		int Constr; // Loop counter
		int ConstrNum; // Loop counter (construct number)
		Array1D< Real64 > cp( MaxLayersInConstruct ); // Specific heat of a material layer
		bool CTFConvrg; // Set after CTFs are calculated, based on whether there are too
		// many CTF terms
		int CurrentLayer; // Pointer to material number in Material derived type (current layer)
		Array1D< Real64 > dl( MaxLayersInConstruct ); // Thickness of a material layer
		Real64 dtn; // Intermediate calculation of the time step
		Array1D< Real64 > dx( MaxLayersInConstruct ); // Distance between nodes in a particular material layer
		Real64 dxn; // Intermediate calculation of nodal spacing
		Real64 dxtmp; // Intermediate calculation variable ( = 1/dx/cap)
		Real64 dyn; // Nodal spacing in the direction perpendicular to the main direction
		// of heat transfer (only valid for a 2-D solution)
		static bool ErrorsFound( false ); // Flag for input error condition
		int HistTerm; // Loop counter
		int ipts1; // Intermediate calculation for number of nodes per layer
		int ir; // Loop control for constructing Identity Matrix
		int Layer; // Loop counter
		int Layer1; // Loop counter
		int LayersInConstruct; // Array containing the number of layers for each construct
		// Different from TotLayers because shades are not include in local var
		Array1D< Real64 > lr( MaxLayersInConstruct ); // R value of a material layer
		int Node; // Loop counter
		int Node2; // Node number (modification of Node and NodeInRow)
		int NodeInLayer; // Loop counter
		int NodeInRow; // Loop counter
		Array1D_int Nodes( MaxLayersInConstruct ); // Array containing the number of nodes per layer
		int NumResLayers; // Number of resistive layers in the construction
		// property allowable, traditional value from BLAST
		int NumAdjResLayers; // Number of resistive layers that are adjacent
		int OppositeLayer; // Used for comparing constructions (to see if one is the reverse of another)
		Array1D_bool ResLayer( MaxLayersInConstruct ); // Set true if the layer must be handled as a resistive
		bool RevConst; // Set true if one construct is the reverse of another (CTFs already
		// available)
		Array1D< Real64 > rho( MaxLayersInConstruct ); // Density of a material layer
		Array1D< Real64 > rk( MaxLayersInConstruct ); // Thermal conductivity of a material layer
		Real64 rs; // Total thermal resistance of the building element
		Real64 SumXi; // Summation of all of the Xi terms (inside CTFs) for a construction
		Real64 SumYi; // Summation of all of the Xi terms (cross CTFs) for a construction
		Real64 SumZi; // Summation of all of the Xi terms (outside CTFs) for a construction
		bool DoCTFErrorReport;
		Real64 Alpha; // thermal diffusivity in m2/s, for local check of properties
		Real64 DeltaTimestep; // zone timestep in seconds, for local check of properties
		Real64 ThicknessThreshold; // min thickness consistent with other thermal properties, for local check

		// FLOW:
		// Subroutine initializations
		TinyLimit = rTinyValue;
		DoCTFErrorReport = false;

		for ( ConstrNum = 1; ConstrNum <= TotConstructs; ++ConstrNum ) { // Begin construction loop ...

			Construct( ConstrNum ).CTFCross = 0.0;
			Construct( ConstrNum ).CTFFlux = 0.0;
			Construct( ConstrNum ).CTFInside = 0.0;
			Construct( ConstrNum ).CTFOutside = 0.0;
			Construct( ConstrNum ).CTFSourceIn = 0.0;
			Construct( ConstrNum ).CTFSourceOut = 0.0;
			Construct( ConstrNum ).CTFTimeStep = 0.0;
			Construct( ConstrNum ).CTFTSourceOut = 0.0;
			Construct( ConstrNum ).CTFTSourceIn = 0.0;
			Construct( ConstrNum ).CTFTSourceQ = 0.0;
			Construct( ConstrNum ).CTFTUserOut = 0.0;
			Construct( ConstrNum ).CTFTUserIn = 0.0;
			Construct( ConstrNum ).CTFTUserSource = 0.0;
			Construct( ConstrNum ).NumHistories = 0;
			Construct( ConstrNum ).NumCTFTerms = 0;
			Construct( ConstrNum ).UValue = 0.0;

			AdjacentResLayerNum = 0; // Zero this out for each construct

			if ( Construct( ConstrNum ).TypeIsWindow ) continue;

			// Initialize construct parameters

			Construct( ConstrNum ).CTFTimeStep = TimeStepZone;
			rs = 0.0;
			LayersInConstruct = 0;
			NumResLayers = 0;
			ResLayer = false;

			for ( Layer = 1; Layer <= Construct( ConstrNum ).TotLayers; ++Layer ) { // Begin layer loop ...

				// Loop through all of the layers in the current construct. The purpose
				// of this loop is to define the thermal properties necessary to
				// calculate the CTFs.

				CurrentLayer = Construct( ConstrNum ).LayerPoint( Layer );

				++LayersInConstruct;

				// Obtain thermal properties from the Material derived type

				dl( Layer ) = Material( CurrentLayer ).Thickness;
				rk( Layer ) = Material( CurrentLayer ).Conductivity;
				rho( Layer ) = Material( CurrentLayer ).Density;
				cp( Layer ) = Material( CurrentLayer ).SpecHeat; // Must convert
				// from kJ/kg-K to J/kg-k due to rk units

				if ( Construct( ConstrNum ).SourceSinkPresent && ! Material( CurrentLayer ).WarnedForHighDiffusivity ) {
					// check for materials that are too conductive or thin
					if ( ( rho( Layer ) * cp( Layer ) ) > 0.0 ) {
						Alpha = rk( Layer ) / ( rho( Layer ) * cp( Layer ) );
						if ( Alpha > HighDiffusivityThreshold ) {
							DeltaTimestep = TimeStepZoneSec;
							ThicknessThreshold = std::sqrt( Alpha * DeltaTimestep * 3.0 );
							if ( Material( CurrentLayer ).Thickness < ThicknessThreshold ) {
								ShowSevereError( "InitConductionTransferFunctions: Found Material that is too thin and/or too highly conductive, material name = " + Material( CurrentLayer ).Name );
								ShowContinueError( "High conductivity Material layers are not well supported for internal source constructions, material conductivity = " + RoundSigDigits( Material( CurrentLayer ).Conductivity, 3 ) + " [W/m-K]" );
								ShowContinueError( "Material thermal diffusivity = " + RoundSigDigits( Alpha, 3 ) + " [m2/s]" );
								ShowContinueError( "Material with this thermal diffusivity should have thickness > " + RoundSigDigits( ThicknessThreshold, 5 ) + " [m]" );
								if ( Material( CurrentLayer ).Thickness < ThinMaterialLayerThreshold ) {
									ShowContinueError( "Material may be too thin to be modeled well, thickness = " + RoundSigDigits( Material( CurrentLayer ).Thickness, 5 ) + " [m]" );
									ShowContinueError( "Material with this thermal diffusivity should have thickness > " + RoundSigDigits( ThinMaterialLayerThreshold, 5 ) + " [m]" );
								}
								Material( CurrentLayer ).WarnedForHighDiffusivity = true;
							}
						}
					}
				}

				if ( rk( Layer ) <= PhysPropLimit ) { // Thermal conductivity too small,
					// thus this must be handled as a resistive layer

					ResLayer( Layer ) = true;

				} else {

					lr( Layer ) = dl( Layer ) / rk( Layer );
					if ( ( dl( Layer ) * std::sqrt( rho( Layer ) * cp( Layer ) / rk( Layer ) ) ) < PhysPropLimit ) {

						// If the time constant is smaller than some reasonable
						// limit, this should be treated as a resistive layer.

						ResLayer( Layer ) = true;

					} else { // Layer has significant thermal mass (non-resistive)

						ResLayer( Layer ) = false;

					}
				}

				// If not a resistive layer, nothing further is required
				// for this layer.

				if ( ResLayer( Layer ) ) { // Resistive layer-check for R-value, etc.
					++NumResLayers; // Increment number of resistive layers
					lr( Layer ) = Material( CurrentLayer ).Resistance; // User defined thermal resistivity
					if ( lr( Layer ) < RValueLowLimit ) { // User didn't define enough
						// parameters to calculate CTFs for a building element
						// containing this layer.

						ShowSevereError( "InitConductionTransferFunctions: Material=" + Material( CurrentLayer ).Name + "R Value below lowest allowed value" );
						ShowContinueError( "Lowest allowed value=[" + RoundSigDigits( RValueLowLimit, 3 ) + "], Material R Value=[" + RoundSigDigits( lr( Layer ), 3 ) + "]." );
						ErrorsFound = true;

					} else { // A valid user defined R-value is available.
						// If this is either the first or last layer in the construction,
						// then assign other properties based on air at 1 atm, 300K.
						// Reference for air properties:  Incropera and DeWitt,
						// Introduction to Heat Transfer, Appendix A, Table A.4,
						// John Wiley & Sons, New York, 1985.
						// If this is not the first or last layer in the construction,
						// then use the "exact" approach to model a massless layer
						// based on the node equations for the state space method.

						if ( ( Layer == 1 ) || ( Layer == Construct( ConstrNum ).TotLayers ) || ( ! Material( Construct( ConstrNum ).LayerPoint( Layer ) ).ROnly ) ) {
							cp( Layer ) = 1.007;
							rho( Layer ) = 1.1614;
							rk( Layer ) = 0.0263;
							dl( Layer ) = rk( Layer ) * lr( Layer );
						} else {
							cp( Layer ) = 0.0;
							rho( Layer ) = 0.0;
							rk( Layer ) = 1.0;
							dl( Layer ) = lr( Layer );
						}

					}
				} // ... end of resistive layer determination IF-THEN block.
			} // ... end of layer loop.

			// If errors have been found, just cycle

			if ( ErrorsFound ) continue;

			// Combine any adjacent resistive-only (no mass) layers together
			// to avoid a divide by zero error in the CTF calculations below.
			// Since the inner and outer layers cannot be resistive layers
			// (inner and outer layer still converted to equivalent air layer)
			// there can only be resistive layers adjacent to one another if
			// there are more than three total layers and more than one
			// resistive layer.
			if ( ( LayersInConstruct > 3 ) && ( NumResLayers > 1 ) ) {
				NumAdjResLayers = 0;
				for ( Layer = 2; Layer <= LayersInConstruct - 2; ++Layer ) {
					if ( ( ResLayer( Layer ) ) && ( ResLayer( Layer + 1 ) ) ) {
						++NumAdjResLayers;
						// There is method to the next assignment statement.  As the layers get shifted, the layer
						// numbers will also shift.  Thus, we have to also shift which layer we are dealing with.
						AdjacentResLayerNum( NumAdjResLayers ) = Layer + 1 - NumAdjResLayers;
					}
				}
				for ( AdjLayer = 1; AdjLayer <= NumAdjResLayers; ++AdjLayer ) {
					Layer = AdjacentResLayerNum( AdjLayer );
					// Double check to make sure we are in the right place...
					if ( ( ResLayer( Layer ) ) && ( ResLayer( Layer + 1 ) ) ) {
						// Shift layers forward after combining two adjacent layers.  Then
						// restart the do loop.
						cp( Layer ) = 0.0;
						rho( Layer ) = 0.0;
						rk( Layer ) = 1.0;
						lr( Layer ) += lr( Layer + 1 );
						dl( Layer ) = lr( Layer );
						--NumResLayers; // Combining layers so decrease number of resistive layers
						for ( Layer1 = Layer + 1; Layer1 <= LayersInConstruct - 1; ++Layer1 ) {
							lr( Layer1 ) = lr( Layer1 + 1 );
							dl( Layer1 ) = dl( Layer1 + 1 );
							rk( Layer1 ) = rk( Layer1 + 1 );
							rho( Layer1 ) = rho( Layer1 + 1 );
							cp( Layer1 ) = cp( Layer1 + 1 );
							ResLayer( Layer1 ) = ResLayer( Layer1 + 1 );
						}
						// Then zero out the layer that got shifted forward
						cp( LayersInConstruct ) = 0.0;
						rho( LayersInConstruct ) = 0.0;
						rk( LayersInConstruct ) = 0.0;
						lr( LayersInConstruct ) = 0.0;
						dl( LayersInConstruct ) = 0.0;
						// Now reduce the number of layers in construct since merger is complete
						--LayersInConstruct;
						// Also adjust layers with source/sinks if two layers are merged
						if ( Construct( ConstrNum ).SourceSinkPresent ) {
							--Construct( ConstrNum ).SourceAfterLayer;
							--Construct( ConstrNum ).TempAfterLayer;
						}
					} else { // These are not adjacent layers and there is a logic flaw here (should not happen)
						ShowFatalError( "Combining resistance layers failed for " + Construct( ConstrNum ).Name );
						ShowContinueError( "This should never happen.  Contact EnergyPlus Support for further assistance." );
					}
				}
			}

			// Convert SI units to English.  In theory, conversion to English
			// units is not necessary; however, Russ Taylor noted that some
			// numerical problems when SI units were used and decided to continue
			// calculating CTFs in English units.

			for ( Layer = 1; Layer <= LayersInConstruct; ++Layer ) { // Begin units conversion loop ...

				lr( Layer ) *= CFU;
				dl( Layer ) /= CFL;
				rk( Layer ) /= CFK;
				rho( Layer ) /= CFD;
				cp( Layer ) /= ( CFC * 1000.0 );

			} // ... end of layer loop for units conversion.

			if ( Construct( ConstrNum ).SolutionDimensions == 1 ) {
				dyn = 0.0;
			} else {
				dyn = ( Construct( ConstrNum ).ThicknessPerpend / CFL ) / double( NumOfPerpendNodes - 1 );
			}

			// Compute total construct conductivity and resistivity.

			for ( Layer = 1; Layer <= LayersInConstruct; ++Layer ) {
				rs += lr( Layer ); // Resistances in series sum algebraically
			}

			cnd = 1.0 / rs; // Conductivity is the inverse of resistivity

			if ( LayersInConstruct > NumResLayers ) {

				// One or more are not simple resistive layers so CTFs will have to be
				// calculated unless this is a reverse of a previously defined
				// construction.

				// Check for reversed construction of interzone surfaces by checking
				// previous constructions for same number of layers as first indicator.

				RevConst = false;

				for ( Constr = 1; Constr <= ConstrNum - 1; ++Constr ) { // Constructions loop (check reversed) ...

					// If a source or sink is present in this construction, do not allow any
					// checks for reversed constructions, i.e., always force EnergyPlus to
					// calculate CTF/QTFs.  So, don't even check for reversed constructions.
					if ( Construct( ConstrNum ).SourceSinkPresent ) break; // Constr DO loop

					if ( Construct( ConstrNum ).TotLayers == Construct( Constr ).TotLayers ) { // Same number of layers--now | check for reversed construct.

						RevConst = true;

						for ( Layer = 1; Layer <= Construct( ConstrNum ).TotLayers; ++Layer ) { // Begin layers loop ...

							// RevConst is set to FALSE anytime a mismatch in materials is found.
							// This will exit this DO immediately and go on to the next construct
							// (if any remain).

							OppositeLayer = Construct( ConstrNum ).TotLayers - Layer + 1;

							if ( Construct( ConstrNum ).LayerPoint( Layer ) != Construct( Constr ).LayerPoint( OppositeLayer ) ) {

								RevConst = false;
								break; // Layer DO loop

							}

						} // ... end of layers loop.

						if ( RevConst ) { // Curent construction is a reverse of
							// construction Constr.  Thus, CTFs do not need to be re-
							// calculated.  Copy CTF info for construction Constr to
							// construction ConstrNum.

							Construct( ConstrNum ).CTFTimeStep = Construct( Constr ).CTFTimeStep;
							Construct( ConstrNum ).NumHistories = Construct( Constr ).NumHistories;
							Construct( ConstrNum ).NumCTFTerms = Construct( Constr ).NumCTFTerms;

							// Transfer the temperature and flux history terms to CTF arrays.
							// Loop through the number of CTF history terms ...
							for ( HistTerm = 0; HistTerm <= Construct( ConstrNum ).NumCTFTerms; ++HistTerm ) {

								Construct( ConstrNum ).CTFInside( HistTerm ) = Construct( Constr ).CTFOutside( HistTerm );
								Construct( ConstrNum ).CTFCross( HistTerm ) = Construct( Constr ).CTFCross( HistTerm );
								Construct( ConstrNum ).CTFOutside( HistTerm ) = Construct( Constr ).CTFInside( HistTerm );
								if ( HistTerm != 0 ) Construct( ConstrNum ).CTFFlux( HistTerm ) = Construct( Constr ).CTFFlux( HistTerm );

							} // ... end of CTF history terms loop.

							break; // Constr DO loop

						} // ... end of reversed construction found block

					} // ... end of reversed construct (same number of layers) block.

				} // ... end of construct loop (check reversed--Constr)

				if ( ! RevConst ) { // Calculate CTFs (non-reversed constr)

					// Estimate number of nodes each layer of the construct will require
					// and calculate the nodal spacing from that

					for ( Layer = 1; Layer <= LayersInConstruct; ++Layer ) { // Begin loop thru layers ...

						// The calculation of dxn used here is based on a standard stability
						// criteria for explicit finite difference solutions.  This criteria
						// was chosen not because it is viewed to be correct, but rather for
						// lack of any better criteria at this time.  The use of a Fourier
						// number based criteria such as this is probably physically correct,
						// though the coefficient (2.0) may not be.

						// If this is a "resistive" layer, only need a single node
						if ( ( ResLayer( Layer ) ) && ( Layer > 1 ) && ( Layer < LayersInConstruct ) ) {
							Nodes( Layer ) = 1;
							dx( Layer ) = dl( Layer );
						} else {
							dxn = std::sqrt( 2.0 * ( rk( Layer ) / rho( Layer ) / cp( Layer ) ) * Construct( ConstrNum ).CTFTimeStep );

							ipts1 = int( dl( Layer ) / dxn ); // number of nodes=thickness/spacing

							// Limit the upper and lower bounds of the number of
							// nodes to MaxCTFTerms and MinNodes respectively.

							if ( ipts1 > MaxCTFTerms ) { // Too many nodes
								Nodes( Layer ) = MaxCTFTerms;
							} else if ( ipts1 < MinNodes ) { // Too few nodes
								Nodes( Layer ) = MinNodes;
							} else { // Calculated number of nodes ok
								Nodes( Layer ) = ipts1;
							}

							if ( Construct( ConstrNum ).SolutionDimensions > 1 ) {
								if ( ipts1 > MaxCTFTerms / 2 ) ipts1 = MaxCTFTerms / 2;
							}

							dx( Layer ) = dl( Layer ) / double( Nodes( Layer ) ); // calc node spacing

						}

					} // . .. end of layers in construction loop (calculating #nodes per layer)

					// Determine the total number of nodes (rcmax)

					rcmax = 0;
					for ( Layer = 1; Layer <= LayersInConstruct; ++Layer ) {
						rcmax += Nodes( Layer );
					}

					// Nodes are placed throughout layers and at the interface between
					// layers.  As a result, the end layers share a node with the adjacent
					// layer-leaving one less node total for all layers.

					--rcmax;
					if ( Construct( ConstrNum ).SolutionDimensions > 1 ) rcmax *= NumOfPerpendNodes;

					// This section no longer needed as rcmax/number of total nodes is allowed to float.
					// If reinstated, this node reduction section would have to be modified to account for
					// the possibility that a 2-D solution is potentially being performed.
					// Check to see if the maximum number of nodes for the construct has
					// been exceeded.  Reduce the nodes per layer if necessary, but only
					// if the number of nodes in a particular layer is greater than the
					// minimum node limit.

					//        DO WHILE (rcmax > MaxTotNodes)     ! Begin total node reduction loop ...

					//          rcmax = 0

					//          DO Layer = 1, LayersInConstruct   ! Begin layer node reduction ...

					//          ! If more nodes than the minimum limit for a layer, reduce the
					//          ! number of nodes.

					//            IF (Nodes(Layer) > MinNodes) THEN
					//              Nodes(Layer) = Nodes(Layer)-1
					//              dx(Layer) = dl(Layer)/DBLE(Nodes(Layer)) ! Recalc node spacing
					//            END IF

					//            rcmax = rcmax + Nodes(Layer) ! Recalculate total number of nodes

					//          END DO        ! ... end of layer loop for node reduction.

					//          rcmax = rcmax-1 ! See note above on counting rcmax

					//        END DO      ! ... end of total node reduction loop.

					// For constructions that have sources or sinks present, determine which
					// node the source/sink is applied at and also where the temperature
					// calculation has been requested.
					NodeSource = 0;
					NodeUserTemp = 0;
					if ( Construct( ConstrNum ).SourceSinkPresent ) {

						for ( Layer = 1; Layer <= Construct( ConstrNum ).SourceAfterLayer; ++Layer ) {
							NodeSource += Nodes( Layer );
						}
						if ( ( NodeSource > 0 ) && ( Construct( ConstrNum ).SolutionDimensions > 1 ) ) NodeSource = ( ( NodeSource - 1 ) * NumOfPerpendNodes ) + 1;

						for ( Layer = 1; Layer <= Construct( ConstrNum ).TempAfterLayer; ++Layer ) {
							NodeUserTemp += Nodes( Layer );
						}
						if ( ( NodeUserTemp > 0 ) && ( Construct( ConstrNum ).SolutionDimensions > 1 ) ) NodeUserTemp = ( ( NodeUserTemp - 1 ) * NumOfPerpendNodes ) + 1;

					}

					// "Adjust time step to ensure stability."  If the time step is too
					// small, it will result in too many history terms which can lead to
					// solution instability.  The method used here to determine whether or
					// not the time step will produce a stable solution is based on a pure
					// Fourier number calculation (Fo = 1) and has not proven to be
					// completely effective.  If too many history terms are calculated,
					// the time step is adjusted and the CTFs end up being recalculated
					// (see later code in this routine).

					dtn = 0.0;
					Construct( ConstrNum ).CTFTimeStep = 0.0;
					for ( Layer = 1; Layer <= LayersInConstruct; ++Layer ) {
						if ( Nodes( Layer ) >= MaxCTFTerms ) {
							if ( Construct( ConstrNum ).SolutionDimensions == 1 ) {
								dtn = rho( Layer ) * cp( Layer ) * pow_2( dx( Layer ) ) / rk( Layer );
							} else { // 2-D solution requested-->this changes length parameter in Fourier number calculation
								dtn = rho( Layer ) * cp( Layer ) * ( pow_2( dx( Layer ) ) + pow_2( dyn ) ) / rk( Layer );
							}
							if ( dtn > Construct( ConstrNum ).CTFTimeStep ) Construct( ConstrNum ).CTFTimeStep = dtn;
						}
					}

					// If the user defined time step is significantly different than the
					// calculated time step for this construct, then CTFTimeStep must be
					// revised.

					if ( std::abs( ( TimeStepZone - Construct( ConstrNum ).CTFTimeStep ) / TimeStepZone ) > 0.1 ) {

						if ( Construct( ConstrNum ).CTFTimeStep > TimeStepZone ) {

							// CTFTimeStep larger than TimeStepZone:  Make sure TimeStepZone
							// divides evenly into CTFTimeStep
							Construct( ConstrNum ).NumHistories = int( ( Construct( ConstrNum ).CTFTimeStep / TimeStepZone ) + 0.5 );
							Construct( ConstrNum ).CTFTimeStep = TimeStepZone * double( Construct( ConstrNum ).NumHistories );

						} else {

							// CTFTimeStep smaller than TimeStepZone:  Set to TimeStepZone
							Construct( ConstrNum ).CTFTimeStep = TimeStepZone;
							Construct( ConstrNum ).NumHistories = 1;

						}

					}

					// Calculate the CTFs using the state space method
					// outlined in Seem's dissertation.  The main matrices
					// AMat, BMat, CMat, and DMat must be derived from
					// applying a finite difference network to the layers of
					// each bldg element.

					// This section must continue looping until the CTFs
					// calculated here will produce a stable solution (less
					// history terms than MaxCTFTerms).

					// This first subsection calculates the elements of AMat
					// which characterizes the heat transfer inside the
					// building element.

					CTFConvrg = false; // Initialize loop control logical

					AExp.allocate( rcmax, rcmax );
					AExp = 0.0;
					AMat.allocate( rcmax, rcmax );
					AMat = 0.0;
					AInv.allocate( rcmax, rcmax );
					AInv = 0.0;
					IdenMatrix.allocate( rcmax, rcmax );
					IdenMatrix = 0.0;
					for ( ir = 1; ir <= rcmax; ++ir ) {
						IdenMatrix( ir, ir ) = 1.0;
					}
					e.dimension( rcmax, 0.0 );
					Gamma1.allocate( 3, rcmax );
					Gamma1 = 0.0;
					Gamma2.allocate( 3, rcmax );
					Gamma2 = 0.0;
					s.allocate( 3, 4, rcmax );
					s = 0.0;

					while ( ! CTFConvrg ) { // Begin CTF calculation loop ...

						BMat( 3 ) = 0.0;

						if ( Construct( ConstrNum ).SolutionDimensions == 1 ) {

							// Set up intermediate calculations for the first layer.
							cap = rho( 1 ) * cp( 1 ) * dx( 1 );
							cap *= 1.5; // For the first node, account for the fact that the
							// half-node at the surface results in a "loss" of some
							// thermal mass.  Therefore, for simplicity, include it
							// at this node.  Same thing done at the last node...
							dxtmp = 1.0 / dx( 1 ) / cap;

							AMat( 1, 1 ) = -2.0 * rk( 1 ) * dxtmp; // Assign the matrix values for the
							AMat( 2, 1 ) = rk( 1 ) * dxtmp; // first node.
							BMat( 1 ) = rk( 1 ) * dxtmp; // Assign non-zero value of BMat.

							Layer = 1; // Initialize the "layer" counter

							NodeInLayer = 2; // Initialize the node (in a layer) counter (already
							// on the second node for the first layer

							for ( Node = 2; Node <= rcmax - 1; ++Node ) { // Begin nodes loop (includes all nodes except the
								// first/last which have special equations) ...

								if ( ( NodeInLayer == Nodes( Layer ) ) && ( LayersInConstruct != 1 ) ) { // For a node at
									// the interface between two adjacent layers, the
									// capacitance of the node must be calculated from the 2
									// halves which may be made up of 2 different materials.

									cap = ( rho( Layer ) * cp( Layer ) * dx( Layer ) + rho( Layer + 1 ) * cp( Layer + 1 ) * dx( Layer + 1 ) ) * 0.5;

									AMat( Node - 1, Node ) = rk( Layer ) / dx( Layer ) / cap; // Assign matrix
									AMat( Node, Node ) = -1.0 * ( rk( Layer ) / dx( Layer ) + rk( Layer + 1 ) / dx( Layer + 1 ) ) / cap; // values for | the current
									AMat( Node + 1, Node ) = rk( Layer + 1 ) / dx( Layer + 1 ) / cap; // node.

									NodeInLayer = 0; // At an interface, reset nodes in layer counter
									++Layer; // Also increment the layer counter

								} else { // Standard node within any layer

									cap = rho( Layer ) * cp( Layer ) * dx( Layer ); // Intermediate
									dxtmp = 1.0 / dx( Layer ) / cap; // calculations.
									AMat( Node - 1, Node ) = rk( Layer ) * dxtmp; // Assign matrix
									AMat( Node, Node ) = -2.0 * rk( Layer ) * dxtmp; // values for the
									AMat( Node + 1, Node ) = rk( Layer ) * dxtmp; // current node.

								}

								++NodeInLayer; // Increment nodes in layer counter
								if ( Node == NodeSource ) BMat( 3 ) = 1.0 / cap;

							} // ... end of nodes loop.

							// Intermediate calculations for the last node.
							cap = rho( LayersInConstruct ) * cp( LayersInConstruct ) * dx( LayersInConstruct );
							cap *= 1.5; // For the last node, account for the fact that the
							// half-node at the surface results in a "loss" of some
							// thermal mass.  Therefore, for simplicity, include it
							// at this node.  Same thing done at the first node...
							dxtmp = 1.0 / dx( LayersInConstruct ) / cap;

							AMat( rcmax, rcmax ) = -2.0 * rk( LayersInConstruct ) * dxtmp; // Assign matrix
							AMat( rcmax - 1, rcmax ) = rk( LayersInConstruct ) * dxtmp; // values for the
							BMat( 2 ) = rk( LayersInConstruct ) * dxtmp; // last node.

							CMat( 1 ) = -rk( 1 ) / dx( 1 ); // Compute the necessary elements
							CMat( 2 ) = rk( LayersInConstruct ) / dx( LayersInConstruct ); // of all other
							DMat( 1 ) = rk( 1 ) / dx( 1 ); // matrices for the state
							DMat( 2 ) = -rk( LayersInConstruct ) / dx( LayersInConstruct ); // space method

						} else { // 2-D solution requested (assign matrices appropriately)

							// As with the 1-D solution, we are accounting for the thermal mass
							// of the half-node at the surface by adding it to the first row
							// of interior nodes at both sides of the construction.  This is not
							// exact, but it does take all of the thermal mass into account.
							amatx = rk( 1 ) / ( 1.5 * rho( 1 ) * cp( 1 ) * dx( 1 ) * dx( 1 ) );
							amaty = rk( 1 ) / ( 1.5 * rho( 1 ) * cp( 1 ) * dyn * dyn );

							// FIRST ROW OF NODES: This first row within the first material layer
							// is special in that it is exposed to a boundary condition.  Thus,
							// the equations are slightly different.
							// Note also that the first and last nodes in a row are slightly
							// different from the rest since they are on an adiabatic plane in
							// the direction perpendicular to the main direction of heat transfer.
							AMat( 1, 1 ) = -2.0 * ( amatx + amaty );
							AMat( 2, 1 ) = 2.0 * amaty;
							AMat( NumOfPerpendNodes + 1, 1 ) = amatx;

							for ( Node = 2; Node <= NumOfPerpendNodes - 1; ++Node ) {
								AMat( Node - 1, Node ) = amaty;
								AMat( Node, Node ) = -2.0 * ( amatx + amaty );
								AMat( Node + 1, Node ) = amaty;
								AMat( Node + NumOfPerpendNodes, Node ) = amatx;
							}

							AMat( NumOfPerpendNodes, NumOfPerpendNodes ) = -2.0 * ( amatx + amaty );
							AMat( NumOfPerpendNodes - 1, NumOfPerpendNodes ) = 2.0 * amaty;
							AMat( NumOfPerpendNodes + NumOfPerpendNodes, NumOfPerpendNodes ) = amatx;

							BMat( 1 ) = amatx;

							Layer = 1;
							NodeInLayer = 2;
							amatx = rk( 1 ) / ( rho( 1 ) * cp( 1 ) * dx( 1 ) * dx( 1 ) ); // Reset these to the normal capacitance
							amaty = rk( 1 ) / ( rho( 1 ) * cp( 1 ) * dyn * dyn ); // Reset these to the normal capacitance
							assert( NumOfPerpendNodes > 0 ); //Autodesk:F2C++ Loop setup assumption
							int const Node_stop( rcmax + 1 - 2 * NumOfPerpendNodes );
							for ( Node = NumOfPerpendNodes + 1; Node <= Node_stop; Node += NumOfPerpendNodes ) {
								// INTERNAL ROWS OF NODES: This is the majority of nodes which are all within
								// a solid layer and not exposed to a boundary condition.
								if ( ( LayersInConstruct == 1 ) || ( NodeInLayer != Nodes( Layer ) ) ) {
									// Single material row: This row of nodes are all contained within a material
									// and thus there is no special considerations necessary.
									if ( NodeInLayer == 1 ) {
										// These intermediate variables only need to be reassigned when a new layer is started.
										// When this is simply another row of the same material, these have already been assigned correctly.
										amatx = rk( Layer ) / ( rho( Layer ) * cp( Layer ) * dx( Layer ) * dx( Layer ) );
										amaty = rk( Layer ) / ( rho( Layer ) * cp( Layer ) * dyn * dyn );
									}

									// Note that the first and last layers in a row are slightly different
									// from the rest since they are on an adiabatic plane in the direction
									// perpendicular to the main direction of heat transfer.
									AMat( Node, Node ) = -2.0 * ( amatx + amaty );
									AMat( Node + 1, Node ) = 2.0 * amaty;
									AMat( Node - NumOfPerpendNodes, Node ) = amatx;
									AMat( Node + NumOfPerpendNodes, Node ) = amatx;

									for ( NodeInRow = 2; NodeInRow <= NumOfPerpendNodes - 1; ++NodeInRow ) {
										Node2 = Node + NodeInRow - 1;
										AMat( Node2 - 1, Node2 ) = amaty;
										AMat( Node2, Node2 ) = -2.0 * ( amatx + amaty );
										AMat( Node2 + 1, Node2 ) = amaty;
										AMat( Node2 - NumOfPerpendNodes, Node2 ) = amatx;
										AMat( Node2 + NumOfPerpendNodes, Node2 ) = amatx;
									}

									Node2 = Node - 1 + NumOfPerpendNodes;
									AMat( Node2, Node2 ) = -2.0 * ( amatx + amaty );
									AMat( Node2 - 1, Node2 ) = 2.0 * amaty;
									AMat( Node2 - NumOfPerpendNodes, Node2 ) = amatx;
									AMat( Node2 + NumOfPerpendNodes, Node2 ) = amatx;

								} else { // Row at a two-layer interface (half of node consists of one layer's materials
									// and the other half consist of the next layer's materials)
									capavg = 0.5 * ( rho( Layer ) * cp( Layer ) * dx( Layer ) + rho( Layer + 1 ) * cp( Layer + 1 ) * dx( Layer + 1 ) );
									amatx = rk( Layer ) / ( capavg * dx( Layer ) );
									amatxx = rk( Layer + 1 ) / ( capavg * dx( Layer + 1 ) );
									amaty = ( rk( Layer ) * dx( Layer ) + rk( Layer + 1 ) * dx( Layer + 1 ) ) / ( capavg * dyn * dyn );

									AMat( Node, Node ) = -amatx - amatxx - 2.0 * amaty;
									AMat( Node + 1, Node ) = 2.0 * amaty;
									AMat( Node - NumOfPerpendNodes, Node ) = amatx;
									AMat( Node + NumOfPerpendNodes, Node ) = amatxx;

									for ( NodeInRow = 2; NodeInRow <= NumOfPerpendNodes - 1; ++NodeInRow ) {
										Node2 = Node + NodeInRow - 1;
										AMat( Node2 - 1, Node2 ) = amaty;
										AMat( Node2, Node2 ) = -amatx - amatxx - 2.0 * amaty;
										AMat( Node2 + 1, Node2 ) = amaty;
										AMat( Node2 - NumOfPerpendNodes, Node2 ) = amatx;
										AMat( Node2 + NumOfPerpendNodes, Node2 ) = amatxx;
									}

									Node2 = Node - 1 + NumOfPerpendNodes;
									AMat( Node2, Node2 ) = -amatx - amatxx - 2.0 * amaty;
									AMat( Node2 - 1, Node2 ) = 2.0 * amaty;
									AMat( Node2 - NumOfPerpendNodes, Node2 ) = amatx;
									AMat( Node2 + NumOfPerpendNodes, Node2 ) = amatxx;

									if ( Node == NodeSource ) BMat( 3 ) = 2.0 * double( NumOfPerpendNodes - 1 ) / capavg;
									NodeInLayer = 0;
									++Layer;

								}
								++NodeInLayer;

							}

							// LAST ROW OF NODES: Like the first row of nodes, this row is exposed to a boundary
							// condition and thus has slightly modified nodal equations.

							// As with the 1-D solution, we are accounting for the thermal mass
							// of the half-node at the surface by adding it to the first row
							// of interior nodes at both sides of the construction.  This is not
							// exact, but it does take all of the thermal mass into account.
							amatx /= 1.5;
							amaty /= 1.5;

							Node = rcmax + 1 - NumOfPerpendNodes;
							AMat( Node, Node ) = -2.0 * ( amatx + amaty );
							AMat( Node + 1, Node ) = 2.0 * amaty;
							AMat( Node - NumOfPerpendNodes, Node ) = amatx;

							for ( Node = rcmax + 2 - NumOfPerpendNodes; Node <= rcmax - 1; ++Node ) {
								AMat( Node - 1, Node ) = amaty;
								AMat( Node, Node ) = -2.0 * ( amatx + amaty );
								AMat( Node + 1, Node ) = amaty;
								AMat( Node - NumOfPerpendNodes, Node ) = amatx;
							}

							AMat( rcmax, rcmax ) = -2.0 * ( amatx + amaty );
							AMat( rcmax - 1, rcmax ) = 2.0 * amaty;
							AMat( rcmax - NumOfPerpendNodes, rcmax ) = amatx;

							BMat( 2 ) = amatx;

							CMat( 1 ) = -rk( 1 ) / dx( 1 ) / double( NumOfPerpendNodes - 1 );
							CMat( 2 ) = rk( LayersInConstruct ) / dx( LayersInConstruct ) / double( NumOfPerpendNodes - 1 );

							DMat( 1 ) = rk( 1 ) / dx( 1 ) / double( NumOfPerpendNodes - 1 );
							DMat( 2 ) = -rk( LayersInConstruct ) / dx( LayersInConstruct ) / double( NumOfPerpendNodes - 1 );

						}

						// Calculation of the CTFs based on the state space
						// method.  This process involves finding the exponential
						// and inverse of AMat and using these results to
						// determine the CTFs.  The Gammas are an intermediate
						// calculations which are necessary before the CTFs can
						// be computed in TransFuncCoeffs.
						DisplayNumberAndString( ConstrNum, "Calculating CTFs for \"" + Construct( ConstrNum ).Name + "\", Construction #" );

						//          CALL DisplayNumberAndString(ConstrNum,'Matrix exponential for Construction #')
						CalculateExponentialMatrix( Construct( ConstrNum ).CTFTimeStep ); // Compute exponential of AMat

						//          CALL DisplayNumberAndString(ConstrNum,'Invert Matrix for Construction #')
						CalculateInverseMatrix(); // Compute inverse of AMat

						//          CALL DisplayNumberAndString(ConstrNum,'Gamma calculation for Construction #')
						CalculateGammas( Construct( ConstrNum ).CTFTimeStep, Construct( ConstrNum ).SolutionDimensions );
						// Compute "gamma"s from AMat, AExp, and AInv

						//          CALL DisplayNumberAndString(ConstrNum,'Compute CTFs for Construction #')
						CalculateCTFs( Construct( ConstrNum ).NumCTFTerms, Construct( ConstrNum ).SolutionDimensions ); // Compute CTFs

						// Now check to see if the number of transfer functions
						// is greater than MaxCTFTerms.  If it is, then increase the
						// time step and the number of history terms and
						// recalculate.  Whether or not it will be necessary to
						// recalculate the CTFs is controlled by this DO WHILE
						// loop and the logical CTFConvrg.

						CTFConvrg = true; // Assume solution convergence

						// If too many terms, then solution did not converge.  Increase the
						// number of histories and the time step.  Reset CTFConvrg to continue
						// the DO loop.
						if ( Construct( ConstrNum ).NumCTFTerms > ( MaxCTFTerms - 1 ) ) {
							++Construct( ConstrNum ).NumHistories;
							Construct( ConstrNum ).CTFTimeStep += TimeStepZone;
							CTFConvrg = false;
						}

						// If the number of terms is okay, then do a further check on the summation of
						// the various series summations.  In theory, Sum(Xi) = Sum(Yi) = Sum(Zi).  If
						// this is not the case, then the terms have not reached a valid solution, and
						// we need to increase the number of histories and the time step as above.
						if ( CTFConvrg ) {
							SumXi = s0( 2, 2 );
							SumYi = s0( 1, 2 );
							SumZi = s0( 1, 1 );
							for ( HistTerm = 1; HistTerm <= Construct( ConstrNum ).NumCTFTerms; ++HistTerm ) {
								SumXi += s( 2, 2, HistTerm );
								SumYi += s( 1, 2, HistTerm );
								SumZi += s( 1, 1, HistTerm );
							}
							SumXi = std::abs( SumXi );
							SumYi = std::abs( SumYi );
							SumZi = std::abs( SumZi );
							BiggestSum = max( SumXi, SumYi, SumZi );
							if ( BiggestSum > 0.0 ) {
								if ( ( ( std::abs( SumXi - SumYi ) / BiggestSum ) > MaxAllowedCTFSumError ) || ( ( std::abs( SumZi - SumYi ) / BiggestSum ) > MaxAllowedCTFSumError ) ) {
									++Construct( ConstrNum ).NumHistories;
									Construct( ConstrNum ).CTFTimeStep += TimeStepZone;
									CTFConvrg = false;
								}
							} else { // Something terribly wrong--the surface has no CTFs, not even an R-value
								ShowFatalError( "Illegal construction definition, no CTFs calculated for " + Construct( ConstrNum ).Name );
							}
						}

						// Once the time step has reached a certain point, it is highly likely that
						// there is either a problem with the input or the solution.  This should
						// be extremely rare since other checks should flag most bad user input.
						// Thus, if the time step reaches a certain point, error out and let the
						// user know that something needs to be checked in the input file.
						if ( Construct( ConstrNum ).CTFTimeStep >= MaxAllowedTimeStep ) {
							ShowSevereError( "CTF calculation convergence problem for Construction=\"" + Construct( ConstrNum ).Name + "\"." );
							ShowContinueError( "...with Materials (outside layer to inside)" );
							ShowContinueError( "(outside)=\"" + Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Name + "\"" );
							for ( Layer = 2; Layer <= Construct( ConstrNum ).TotLayers; ++Layer ) {
								if ( Layer != Construct( ConstrNum ).TotLayers ) {
									ShowContinueError( "(next)=\"" + Material( Construct( ConstrNum ).LayerPoint( Layer ) ).Name + "\"" );
								} else {
									ShowContinueError( "(inside)=\"" + Material( Construct( ConstrNum ).LayerPoint( Layer ) ).Name + "\"" );
								}
							}
							ShowContinueError( "The Construction report will be produced. This will show more details on Constructions and their materials." );
							ShowContinueError( "Attempts will be made to complete the CTF process but the report may be incomplete." );
							ShowContinueError( "Constructs reported after this construction may appear to have all 0 CTFs." );
							ShowContinueError( "The potential causes of this problem are related to the input for the construction" );
							ShowContinueError( "listed in the severe error above.  The CTF calculate routine is unable to come up" );
							ShowContinueError( "with a series of CTF terms that have a reasonable time step and this indicates an" );
							ShowContinueError( "error.  Check the definition of this construction and the materials that make up" );
							ShowContinueError( "the construction.  Very thin, highly conductive materials may cause problems." );
							ShowContinueError( "This may be avoided by ignoring the presence of those materials since they probably" );
							ShowContinueError( "do not effect the heat transfer characteristics of the construction.  Highly" );
							ShowContinueError( "conductive or highly resistive layers that are alternated with high mass layers" );
							ShowContinueError( "may also result in problems.  After confirming that the input is correct and" );
							ShowContinueError( "realistic, the user should contact the EnergyPlus support team." );
							DoCTFErrorReport = true;
							ErrorsFound = true;
							break;
							//            CALL ShowFatalError('Program terminated for reasons listed (InitConductionTransferFunctions) ')
						}

					} // ... end of CTF calculation loop.

				} // ... end of IF block for non-reversed constructs.

			} else { // Construct has only resistive layers (no thermal mass).
				// CTF calculation not necessary, overall resistance
				// (R-value) is all that is needed.

				// Set time step for construct to user time step and the number of
				// inter-time step interpolations to 1
				Construct( ConstrNum ).CTFTimeStep = TimeStepZone;
				Construct( ConstrNum ).NumHistories = 1;
				Construct( ConstrNum ).NumCTFTerms = 1;

				s0( 1, 1 ) = cnd; // CTFs for current time
				s0( 2, 1 ) = -cnd; // step are set to the
				s0( 1, 2 ) = cnd; // overall conductance
				s0( 2, 2 ) = -cnd; // of the construction.

				e.allocate( 1 );
				e = 0.0;
				s.allocate( 2, 2, 1 );
				s = 0.0;
				s( 1, 1, 1 ) = 0.0; // CTF temperature
				s( 2, 1, 1 ) = 0.0; // and flux
				s( 1, 2, 1 ) = 0.0; // history terms
				s( 2, 2, 1 ) = 0.0; // are all
				e( 1 ) = 0.0; // zero.

				if ( Construct( ConstrNum ).SourceSinkPresent ) {
					ShowSevereError( "Sources/sinks not allowed in purely resistive constructions --> " + Construct( ConstrNum ).Name );
					ErrorsFound = true;
				}

				RevConst = false; // In the code that follows, handle a resistive
				// layer as a non-reversed construction.

			} // ... end of resistive construction IF block.

			// Transfer the CTFs to the storage arrays for all non-reversed
			// constructions.  This transfer was done earlier in the routine for
			// reversed constructions.

			if ( ! RevConst ) { // If this is either a new construction or a non-
				// reversed construction, the CTFs must be stored
				// in the proper arrays.  If this is a reversed
				// construction, nothing further needs to be done.

				// Copy the CTFs into the storage arrays, converting them back to SI
				// units in the process.  First the "zero" terms and then the history terms...
				Construct( ConstrNum ).CTFOutside( 0 ) = s0( 1, 1 ) * CFU;
				Construct( ConstrNum ).CTFCross( 0 ) = s0( 1, 2 ) * CFU;
				Construct( ConstrNum ).CTFInside( 0 ) = -s0( 2, 2 ) * CFU;
				if ( Construct( ConstrNum ).SourceSinkPresent ) {
					// QTFs...
					Construct( ConstrNum ).CTFSourceOut( 0 ) = s0( 3, 1 );
					Construct( ConstrNum ).CTFSourceIn( 0 ) = s0( 3, 2 );
					// QTFs for temperature calculation at source/sink location
					Construct( ConstrNum ).CTFTSourceOut( 0 ) = s0( 1, 3 );
					Construct( ConstrNum ).CTFTSourceIn( 0 ) = s0( 2, 3 );
					Construct( ConstrNum ).CTFTSourceQ( 0 ) = s0( 3, 3 ) / CFU;
					if ( Construct( ConstrNum ).TempAfterLayer != 0 ) {
						// QTFs for user specified interior temperature calculations...
						Construct( ConstrNum ).CTFTUserOut( 0 ) = s0( 1, 4 );
						Construct( ConstrNum ).CTFTUserIn( 0 ) = s0( 2, 4 );
						Construct( ConstrNum ).CTFTUserSource( 0 ) = s0( 3, 4 ) / CFU;
					}
				}

				for ( HistTerm = 1; HistTerm <= Construct( ConstrNum ).NumCTFTerms; ++HistTerm ) {
					// "REGULAR" CTFs...
					Construct( ConstrNum ).CTFOutside( HistTerm ) = s( 1, 1, HistTerm ) * CFU;
					Construct( ConstrNum ).CTFCross( HistTerm ) = s( 1, 2, HistTerm ) * CFU;
					Construct( ConstrNum ).CTFInside( HistTerm ) = -s( 2, 2, HistTerm ) * CFU;
					if ( HistTerm != 0 ) Construct( ConstrNum ).CTFFlux( HistTerm ) = -e( HistTerm );
					if ( Construct( ConstrNum ).SourceSinkPresent ) {
						// QTFs...
						Construct( ConstrNum ).CTFSourceOut( HistTerm ) = s( 3, 1, HistTerm );
						Construct( ConstrNum ).CTFSourceIn( HistTerm ) = s( 3, 2, HistTerm );
						// QTFs for temperature calculation at source/sink location
						Construct( ConstrNum ).CTFTSourceOut( HistTerm ) = s( 1, 3, HistTerm );
						Construct( ConstrNum ).CTFTSourceIn( HistTerm ) = s( 2, 3, HistTerm );
						Construct( ConstrNum ).CTFTSourceQ( HistTerm ) = s( 3, 3, HistTerm ) / CFU;
						if ( Construct( ConstrNum ).TempAfterLayer != 0 ) {
							// QTFs for user specified interior temperature calculations...
							Construct( ConstrNum ).CTFTUserOut( HistTerm ) = s( 1, 4, HistTerm );
							Construct( ConstrNum ).CTFTUserIn( HistTerm ) = s( 2, 4, HistTerm );
							Construct( ConstrNum ).CTFTUserSource( HistTerm ) = s( 3, 4, HistTerm ) / CFU;
						}
					}
				}

			} // ... end of the reversed construction IF block.

			Construct( ConstrNum ).UValue = cnd * CFU;

			if ( allocated( AExp ) ) AExp.deallocate();
			if ( allocated( AMat ) ) AMat.deallocate();
			if ( allocated( AInv ) ) AInv.deallocate();
			if ( allocated( IdenMatrix ) ) IdenMatrix.deallocate();
			if ( allocated( e ) ) e.deallocate();
			if ( allocated( Gamma1 ) ) Gamma1.deallocate();
			if ( allocated( Gamma2 ) ) Gamma2.deallocate();
			if ( allocated( s ) ) s.deallocate();

		} // ... end of construction loop.

		ReportCTFs( DoCTFErrorReport );

		if ( ErrorsFound ) {
			ShowFatalError( "Program terminated for reasons listed (InitConductionTransferFunctions)" );
		}

	}

	void
	CalculateExponentialMatrix( Real64 & delt ) // Time step of the resulting CTFs
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   June 1990
		//       MODIFIED       Dec 1995, Apr 1996, RKS; June 2000 RKS
		//       RE-ENGINEERED  June 1996, RKS; Nov 1999, LKL;

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine computes the exponential matrix exp(AMat*delt) for
		// use in the state space method for the calculation of CTFs.

		// METHODOLOGY EMPLOYED:
		// Uses the method of Taylor expansion combined with scaling and
		// squaring to most efficiently compute the exponential matrix.  The
		// steps in the procedure are outlined in Seem's dissertation in
		// Appendix A, page 128.  Exponential matrix multiplication modified
		// to take advantage of the characteristic form of AMat.  AMat starts
		// out as a tri-diagonal matrix.  Each time AMat is raised to a higher
		// power two extra non-zero diagonals are added.  ExponMatrix now
		// recognizes this.  This should speed up the calcs somewhat.  Also, a
		// new cut-off criteria based on the significant figures of double-
		// precision variables has been added.  The main loop for higher powers
		// of AMat is now stopped whenever these powers of AMat will no longer
		// add to the summation (AExp) instead ofstopping potentially at the
		// artifical limit of AMat**100.

		// REFERENCES:
		// Seem, J.E.  "Modeling of Heat Transfer in Buildings",
		//  Department of Mechanical Engineering, University of
		//  Wisconsin-Madison, 1987.
		// Strand, R.K. "Testing Design Description for the CTF
		//  Calculation Code in BEST", BSO internal document,
		//  May/June 1996.

		// USE STATEMENTS:
		// none

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		Real64 const DPLimit( 1.0e-20 );
		// This argument is nice, but not sure it's accurate -- LKL Nov 1999.
		// Parameter set to the significant figures limit of double
		// precision variables plus a safety factor.- The argument for setting this parameter to 1E-20 involves the
		// number of significant figures for REAL(r64) variables which is 16 and the largest power to which
		// AMat will be raised which is 100.  This would be a factor of 1E-18.  A factor of "safety" of another 100
		// arrives at the value chosen.  It is argued that if one number is 1E-16 larger than a second number, then
		// adding the second to the first will not effect the first.  However, on the conservative side, there could
		// be up to 100 numbers which might, added together, still could effect the original number.  Each
		// successive power of AMat will have terms smaller than the previous power.  Thus, when the ratio between
		// the terms of the latest power of AMat and the total (AExp) is less than DPLim, all further powers of
		// AMat will have absolutely no effect on the REAL(r64) value of AExp.  Thus, there is no need to
		// continue the calculation.  In effect, AExp has "converged".  In REAL(r64)ity, 1E-16 would probably guarantee
		// convergence since AMat terms drop off quickly, but the extra powers allows for differences between
		// computer platforms.

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AMatRowNorm; // Row norm for AMat
		Real64 AMatRowNormMax; // Largest row norm for AMat
		Array2D< Real64 > AMat1; // AMat factored by (delt/2^k)
		Array2D< Real64 > AMato; // AMat raised to the previous power (power of AMat1-1)
		Array2D< Real64 > AMatN; // Current value of AMat raised to power n (n = 1,2...)
		bool Backup; // Used when numerics get to small in Exponentiation
		Real64 CheckVal; // Used to avoid possible overflow from Double->REAL(r64)->Integer
		Real64 fact; // Intermediate calculation variable (delt/2^k)
		int i; // Loop counter
		int ic; // Loop counter
		int ict; // Loop counter
		int idm; // Loop counter
		int ir; // Loop counter
		int isq; // Loop counter
		int j; // Loop counter
		int k; // Power of 2 which is used to factor AMat
		int l; // Theoretical power to which the A matrix must be
		// raised to accurately calculate the exponential matrix
		bool SigFigLimit; // Significant figure limit logical, true
		// when exponential calculation loop can be exited (i.e.
		// the significant figure limit for REAL(r64)
		// variables reached)

		// FLOW:
		AMat1.allocate( rcmax, rcmax );
		AMato.allocate( rcmax, rcmax );
		AMatN.allocate( rcmax, rcmax );

		// Subroutine initializations.  AMat is assigned to local variable AMat1 to
		// avoid the corruption of the original AMat 2-d array.
		AMat1 = AMat;

		//  Other arrays are initialized to zero.
		AExp = 0.0;
		AMato = 0.0;
		AMatN = 0.0;

		// Step 1, page 128 (Seem's thesis):  Compute the matrix row norm.
		// See equation (A.3) which states that the matrix row norm is the
		// maximum summation of the elements in a row of AMat multiplied by
		// the time step.

		//Note With change to row-major arrays "row" here now means "column"

		AMatRowNormMax = 0.0; // Start of Step 1 ...

		for ( i = 1; i <= rcmax; ++i ) {

			AMatRowNorm = 0.0;
			for ( j = 1; j <= rcmax; ++j ) {
				AMatRowNorm += std::abs( AMat1( j, i ) );
			}

			AMatRowNorm *= delt;

			AMatRowNormMax = max( AMatRowNormMax, AMatRowNorm );

		} // ... end of Step 1.

		// Step 2, page 128:  Find smallest integer k such that
		// AMatRowNormMax< = 2^k

		k = int( std::log( AMatRowNormMax ) / std::log( 2.0 ) ) + 1; //Autodesk:Num Handle AMatRowNormMax=0

		// Step 3, page 128:  Divide (AMat*delt) by 2^k.  This section of code
		// takes advantage of the fact that AMat is tridiagonal.  Thus, it
		// only factors the elements of the AMat that are known to be non-zero.

		fact = delt / std::pow( 2.0, k ); // Start of Step 3 ...
		AMat1 *= fact; // ... end of Step 3.

		// Step 4, page 128:  Calculate l, the highest power to which AMat
		// must be taken theoretically to accurately calculate its exponential.
		// This is based on a paper by Cadzow and Martens ("Discrete-Time and
		// Computer Control Systems",Prentice-Hall, pp. 389-390, 1970).  This
		// number is now used as the maximum power to which AMat must be
		// raised in order to calculate the exponential matrix.  A new cut-off
		// criteria based on the number of significant figures in a double-
		// precision variable is used as a more practical limit on the
		// exponentiation algorithm.

		CheckVal = min( 3.0 * AMatRowNormMax + 6.0, 100.0 );
		l = int( CheckVal );

		// Step 5, page 128:  Calculate the exponential.  First, add the
		// linear term to the identity matrix.
		AExp = AMat1 + IdenMatrix; // Start of Step 5 ...

		// Now, add successive terms to the expansion as per the standard
		// exponential formula.  AMato contains the last "power" of AMat
		// which saves the program from having to remultiply the entire power
		// of AMat each time.  Since this is still the linear power of AMat,
		// AMat1 is still tridiagonal in nature.
		AMato = AMat1;

		i = 1; // Initialize the counter for the following DO loop

		// The following DO WHILE loop continues to raise AMat to successive
		// powers and add it to the exponential matrix (AExp).
		while ( i < l ) { // Begin power raising loop ...

			++i; // Increment the loop counter
			SigFigLimit = true; // Set the significant factor limit flag

			for ( ir = 1; ir <= rcmax; ++ir ) { // Begin matrix multiplication loop ...
				// The following matrix multiplication could be "optimized" since
				// for one-dimensional heat transfer AMat is 3-diagonal, AMat squared
				// is 5-diagonal, etc.  However, the code can be much simpler if we
				// ignore this fact and just do a generic matrix multiplication.
				// For 2-D heat transfer, the number of off-diagonal non-zero terms
				// is slightly more complicated as well.
				for ( ic = 1; ic <= rcmax; ++ic ) {
					AMatN( ic, ir ) = 0.0;
					for ( ict = 1; ict <= rcmax; ++ict ) {
						// Make sure the next term won't cause an underflow.  If it will end up being
						// so small as to go below TinyLimit, then ignore it since it won't add anything
						// to AMatN anyway.
						if ( std::abs( AMat1( ic, ict ) ) > TinyLimit ) {
							if ( std::abs( AMato( ict, ir ) ) > std::abs( double( i ) * TinyLimit / AMat1( ic, ict ) ) ) AMatN( ic, ir ) += AMato( ict, ir ) * AMat1( ic, ict ) / double( i );
						}
					}
				}
			} // ... end of matrix multiplication loop.

			// Update AMato and AExp matrices
			AMato = AMatN;
			AExp += AMato;

			// The next DO loop tests the significant figures limit criteria to
			// see if any values in AExp are still changing appreciably.
			for ( ir = 1; ir <= rcmax; ++ir ) {
				for ( ic = 1; ic <= rcmax; ++ic ) {
					// Test of limit criteria:
					if ( std::abs( AExp( ic, ir ) ) > TinyLimit ) { // Next line divides by AExp entry so it
						// must be checked to avoid dividing by zero.
						// If the ratio between any current element in the power
						// of AMat and its corresponding element in AExp is
						// greater than the number which might effect the overall
						// exponential matrix based on stability criteria, then
						// continue raising AMat to another power (SigFigLimit = false).

						if ( std::abs( AMato( ic, ir ) / AExp( ic, ir ) ) > DPLimit ) {
							SigFigLimit = false;
							break; // DO loop (anytime SigFigLimit is false, AMat must continue to be raised another power)
						}

					} else { // There are still elements of AExp which are zero, so
						// the raising of AMat to higher powers should continue.

						SigFigLimit = false;
						break; // DO loop (anytime SigFigLimit is false, AMat must continue to be raised another power)

					}
				}
				if ( ! SigFigLimit ) break; // DO loop (anytime SigFigLimit is false, AMat must continue to be raised another power)
			}

			// Compute next term, only if necessary.  If SigFigLimit is still true,
			// then all of the new terms being added to AExp are too small to
			// affect it.  Thus, there is no need to continue this do loop further.

			if ( SigFigLimit ) i = 100; // SigFigLimit is still true, set i to maximum possible
			// value of l (100).

		} // ... end of power raising loop and Step 5.

		// Step 6, page 128:
		// Square AExp "k times" to obtain the actual exponential matrix
		// (remember that AExp was scaled earlier in this routine).

		for ( isq = 1; isq <= k; ++isq ) { // Begin squaring DO loop and Step 6 ...

			// Use AMato to store the old values of AExp
			AMato = AExp;
			Backup = true;
			AExp = 0.0;

			// Multiply the old value of AExp (AMato) by itself and store in AExp.
			for ( ir = 1; ir <= rcmax; ++ir ) {
				for ( ic = 1; ic <= rcmax; ++ic ) {
					for ( idm = 1; idm <= rcmax; ++idm ) {
						if ( std::abs( AMato( idm, ir ) * AMato( ic, idm ) ) > TinyLimit ) {
							AExp( ic, ir ) += AMato( idm, ir ) * AMato( ic, idm );
							Backup = false;
						}
					}
				}
			}
			// Backup is true when every item of AExp didnt pass the TinyLimit test
			if ( Backup ) {
				AExp = AMato;
				break;
			}

		} // ... end of squaring loop and Step 6.

		AMat1.deallocate();
		AMato.deallocate();
		AMatN.deallocate();

	}

	void
	CalculateInverseMatrix()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   Dec 1995
		//       MODIFIED       June 2000 RKS (made routine generic to allow for 2-D solutions)
		//       RE-ENGINEERED  June 1996, February 1997 RKS

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine computes the inverse of AMat for use
		// in the calculation of the CTFs.

		// METHODOLOGY EMPLOYED:
		// Uses row elimination to zero the off-diagonal terms of
		// AMat while performing the same operations on another
		// matrix which starts as the identity matrix.  Once AMat
		// has been converted to an identity matrix(I), the other
		// matrix which started as the I will then be the inverse
		// of A.  This algorithm has been customized for a
		// tri-diagonal matrix.

		// REFERENCES:
		// Any linear algebra test (this is a generic routine).

		// USE STATEMENTS:
		// none

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Array2D< Real64 > AMat1; // Intermediate calculation matrix equivalent at first to AMat
		int ic; // Loop counter
		int ir; // Loop counter
		int irr; // Loop counter

		// FLOW:

		// Subroutine initializations ...
		AMat1.allocate( rcmax, rcmax );

		AMat1 = AMat; // Set AMat1 = AMat to avoid AMat changes
		AInv = IdenMatrix; // Set AInv to Identity Matrix

		// Use Gaussian elimination to zero all of the elements of AMat left
		// of the diagonal.
		// This DO loop will cycle through each of the rows in AMat except the
		// last row which is handled later because it does not have to be used
		// to eliminate any other rows.  The index ir is the current row
		// number and also the column of the current diagonal element.

		for ( ir = 1; ir <= rcmax - 1; ++ir ) { // Begin forward elimination loop ...

			// Factor all of the elements of the row being used to zero the next
			// row in both AMat and AInv by the diagonal element of this row.
			// We should only need to factor the elements to the right of the
			// diagonal since those to the right of it should be zero.
			for ( ic = ir + 1; ic <= rcmax; ++ic ) {
				AMat1( ic, ir ) /= AMat1( ir, ir );
			}

			// In the forward elimination process, all the elements in AInv to the
			// right of the diagonal are zero so they do not need to be factored.
			for ( ic = 1; ic <= ir; ++ic ) {
				AInv( ic, ir ) /= AMat1( ir, ir );
			}

			AMat1( ir, ir ) = 1.0; // By definition, the diagonal of AMat is now 1.

			// Use this factored row to eliminate the off-diagonal element of the
			// rows below the current one (ir)...

			for ( irr = ir + 1; irr <= rcmax; ++irr ) { // Start of row reduction loop...

				for ( ic = ir + 1; ic <= rcmax; ++ic ) {
					AMat1( ic, irr ) -= AMat1( ir, irr ) * AMat1( ic, ir );
				}

				// Now, determine the effect on the next row of AInv.  Again, all of
				// the elements in AInv to the right of the diagonal are zero, so they
				// can be ignored.

				for ( ic = 1; ic <= ir; ++ic ) {
					AInv( ic, irr ) -= AMat1( ir, irr ) * AInv( ic, ir );
				}

				AMat1( ir, irr ) = 0.0; // By definition, the element to the left of the
				// diagonal in the next row of AMat is now zero.

			} // ...end of row reduction loop

		} // ... end of the forward elimination loop.

		// Factor the last row of AInv by the current value of the last
		// diagonal element of AMat. After this is done, all of the diagonal
		// elements of AMat are unity and all of the elements in AMat left of
		// the diagonal are zero.

		for ( ic = 1; ic <= rcmax; ++ic ) {
			AInv( ic, rcmax ) /= AMat1( rcmax, rcmax );
		}
		AMat1( rcmax, rcmax ) = 1.0;

		// Now, use back substitution to eliminate the elements to the right
		// of the diagonal in AMat.  The procedure is similar to the forward
		// elimination process except that we only have to operate on AInv,
		// though now all of the columns of AInv may be non-zero.

		// This DO loop will cycle through the remaining rows which are not
		// yet diagonalized in reverse order.  Note that the only effect on
		// AMat is that the off-diagonal element is zeroed.  The diagonal
		// (which has already been set to unity) is not effected by this row
		// elimination process.
		// In the following code ir is the column being zeroed and irr is the
		// row being worked on

		for ( ir = rcmax; ir >= 2; --ir ) { // Begin reverse elimination loop ...
			for ( irr = 1; irr <= ir - 1; ++irr ) {
				for ( ic = 1; ic <= rcmax; ++ic ) {
					AInv( ic, irr ) -= AMat1( ir, irr ) * AInv( ic, ir );
				}
				AMat1( ir, irr ) = 0.0;
			}
		} // ... end of reverse elimination loop.

		// At this point, AMat1 is equal to the identity matrix (I)
		// and AInv is equal to the inverse of AMat.

		AMat1.deallocate();

	}

	void
	CalculateGammas(
		Real64 const delt, // Time increment in fraction of an hour
		int const SolutionDimensions // Integer relating whether a 1- or 2-D solution is required
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   June 1990
		//       MODIFIED       na
		//       RE-ENGINEERED  July 1996, RKS

		// PURPOSE OF THIS SUBROUTINE:
		// Compute gammas as defined in Seem's dissertation.
		// Runs as a subroutine of the conduction transfer
		// function solver (InitializeCTFs).

		// METHODOLOGY EMPLOYED:
		// Determine the Gamma1 and Gamma2 based on the results
		// from the ExponMatrix and InvertMatrix subroutines.
		// This routine is specialized to take advantage of the
		// fact that most of BMat consists of zeroes.

		// REFERENCES:
		// The state space method of calculating CTFs is
		// outlined in the doctoral dissertation of John Seem,
		// "Modeling of Heat Transfer in Buildings", Department
		// of Mechanical Engineering, University of Wisconsin-
		// Madison, 1987.

		// USE STATEMENTS:
		// none

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Array2D< Real64 > ATemp; // Intermediate variable equal to AExp - I
		int i; // Loop counter
		int is1; // Loop counter
		int j; // Loop counter
		int SurfNode; // Loop counter

		// FLOW:

		// Compute Gamma1 from equation (2.1.12) in Seem's dissertation which
		// states that:  Gamma1  =  [AInv] * ([AExp]-[I]) * [BMat]
		// noting that BMat contains only the non-zero values of the B Matrix.

		ATemp.allocate( rcmax, rcmax );
		ATemp = AExp - IdenMatrix;
		Gamma1 = 0.0;

		for ( i = 1; i <= rcmax; ++i ) {

			for ( is1 = 1; is1 <= rcmax; ++is1 ) {

				if ( SolutionDimensions == 1 ) {
					Gamma1( 1, i ) += AInv( is1, i ) * ATemp( 1, is1 ) * BMat( 1 );
					Gamma1( 2, i ) += AInv( is1, i ) * ATemp( rcmax, is1 ) * BMat( 2 );
				} else { // SolutionDimensions = 2
					for ( SurfNode = 1; SurfNode <= NumOfPerpendNodes; ++SurfNode ) {
						Gamma1( 1, i ) += AInv( is1, i ) * ATemp( SurfNode, is1 ) * BMat( 1 );
						Gamma1( 2, i ) += AInv( is1, i ) * ATemp( rcmax + 1 - SurfNode, is1 ) * BMat( 2 );
					}
				}

				if ( NodeSource > 0 ) {
					Gamma1( 3, i ) += AInv( is1, i ) * ATemp( NodeSource, is1 ) * BMat( 3 );
				}

			}

		}

		ATemp.deallocate();
		// Compute Gamma2 from equation (2.1.13) in Seem's dissertation which
		// states that:  Gamma2  =  [AInv] * ([Gamma1]/delt - [BMat])
		// again noting that BMat contains only the non-zero values of B.
		Gamma2 = 0.0;

		for ( i = 1; i <= rcmax; ++i ) {

			for ( j = 1; j <= 3; ++j ) {

				for ( is1 = 1; is1 <= rcmax; ++is1 ) {

					if ( SolutionDimensions == 1 ) {
						if ( ( j == 1 ) && ( is1 == 1 ) ) {
							Gamma2( j, i ) += AInv( is1, i ) * ( Gamma1( j, is1 ) / delt - BMat( 1 ) );
						} else if ( ( j == 2 ) && ( is1 == rcmax ) ) {
							Gamma2( j, i ) += AInv( is1, i ) * ( Gamma1( j, is1 ) / delt - BMat( 2 ) );
						} else if ( ( j == 3 ) && ( is1 == NodeSource ) ) {
							Gamma2( j, i ) += AInv( is1, i ) * ( Gamma1( j, is1 ) / delt - BMat( 3 ) );
						} else { // the element of the actual BMat is zero
							Gamma2( j, i ) += AInv( is1, i ) * ( Gamma1( j, is1 ) / delt );
						}
					} else { // SolutionDimensions = 2
						if ( ( j == 1 ) && ( ( is1 >= 1 ) && ( is1 <= NumOfPerpendNodes ) ) ) {
							Gamma2( j, i ) += AInv( is1, i ) * ( Gamma1( j, is1 ) / delt - BMat( 1 ) );
						} else if ( ( j == 2 ) && ( ( is1 <= rcmax ) && ( is1 >= rcmax + 1 - NumOfPerpendNodes ) ) ) {
							Gamma2( j, i ) += AInv( is1, i ) * ( Gamma1( j, is1 ) / delt - BMat( 2 ) );
						} else if ( ( j == 3 ) && ( is1 == NodeSource ) ) {
							Gamma2( j, i ) += AInv( is1, i ) * ( Gamma1( j, is1 ) / delt - BMat( 3 ) );
						} else { // the element of the actual BMat is zero
							Gamma2( j, i ) += AInv( is1, i ) * ( Gamma1( j, is1 ) / delt );
						}
					}

				}

			}

		}

	}

	void
	CalculateCTFs(
		int & nrf, // Number of response factor terms
		int const SolutionDimensions // Integer relating whether a 1- or 2-D solution is required
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   June 1990
		//       MODIFIED       Apr 96, RKS, cosmetic, algorithm neutral changes
		//       RE-ENGINEERED  July 1996, RKS; Nov 1999, LKL (allocatable arrays)

		// PURPOSE OF THIS SUBROUTINE:
		// Subprogram to calculate the Sj and ej coefficients of Seem's
		// dissertation.  Follows Seem's technique to compute the coefficients
		// in order with minimum storage requirements.

		// METHODOLOGY EMPLOYED:
		// Combine the results of the ExponMatrix, InvertMatrix, and
		// CalculateGammas routines together to arrive at the temperature
		// coefficients (s, s0) and the heat flux history coefficients (e) of
		// the CTFs.  The outline of this subroutine is based on step 5 of
		// Seem's suggested implementation of the state space method found on
		// pages 26+27 of his dissertation.

		// REFERENCES:
		// The state space method of calculating CTFs is outlined in the
		// doctoral dissertation of John Seem, "Modeling of Heat Transfer in
		// Buildings", Department of Mechanical Engineering, University of
		// Wisconsin-Madison, 1987.  In particular, the equations used for
		// these calculations are equations (2.1.24) through (2.1.26) in Seem's
		// dissertation.

		// USE STATEMENTS:
		// none

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const ConvrgLim( 1.0e-13 ); // Convergence limit (ratio) for cutting off the calculation of further
		// CTFs.  This value was found to give suitable accuracy in IBLAST.

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE DUMMY VARIABLE DECLARATIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Real64 avg; // Intermediate calculation variable (average)
		bool CTFConvrg; // Set after CTFs are calculated, based on whether there are
		// too many CTFs terms
		int i; // Loop counter
		int ic; // Loop counter
		int inum; // Loop counter
		int ir; // Loop counter
		int is; // Loop counter
		int is2; // Loop counter
		int j; // Loop counter
		Array2D< Real64 > PhiR0; // Product of Phi( = AExp) and R0 matrices from the state
		// space method
		Real64 rat; // Intermediate calculation variable (ratio of flux history
		// terms)
		Array2D< Real64 > Rnew; // Current R matrix
		Array2D< Real64 > Rold; // R matrix from the last iteration
		int SurfNode; // Loop counter (for nodes at a surface)
		Real64 SurfNodeFac; // Multiplying factor applied to various surface nodes
		Real64 trace; // Trace of the product of Phi( = AExp) and R0

		// FLOW:

		// Subroutine initializations
		PhiR0.allocate( rcmax, rcmax );
		Rnew.allocate( rcmax, rcmax );
		Rold.allocate( rcmax, rcmax );
		PhiR0 = 0.0;
		Rold = 0.0;

		s0 = 0.0;
		s = 0.0;
		e = 0.0;
		Rnew = IdenMatrix; // Rnew initialized to the identity matrix

		// Calculate Gamma1-Gamma2.  Gamma1 is not used by itself in the
		// equations, only Gamma1-Gamma2.  Thus, reset Gamma1 to:
		// Gamma1-Gamma2
		for ( i = 1; i <= rcmax; ++i ) {
			for ( j = 1; j <= 3; ++j ) {
				Gamma1( j, i ) -= Gamma2( j, i );
			}
		}

		// Compute s0.  See Seem's thesis equation (2.1.24) which states that:
		// s0  =  (CMat*R0*Gamma2) + (DMat)
		// Note that for a two-dimensional solution, there is more than one
		// node at the surface and the effect of each of these must be added
		// together.
		if ( SolutionDimensions == 1 ) {
			s0( 1, 1 ) = CMat( 1 ) * Gamma2( 1, 1 ) + DMat( 1 );
			s0( 2, 1 ) = CMat( 1 ) * Gamma2( 2, 1 );
			s0( 3, 1 ) = CMat( 1 ) * Gamma2( 3, 1 );
			s0( 1, 2 ) = CMat( 2 ) * Gamma2( 1, rcmax );
			s0( 2, 2 ) = CMat( 2 ) * Gamma2( 2, rcmax ) + DMat( 2 );
			s0( 3, 2 ) = CMat( 2 ) * Gamma2( 3, rcmax );
		} else { // SolutionDimensions = 2
			for ( SurfNode = 1; SurfNode <= NumOfPerpendNodes; ++SurfNode ) {
				if ( ( SurfNode == 1 ) || ( SurfNode == NumOfPerpendNodes ) ) {
					SurfNodeFac = 0.5;
				} else {
					SurfNodeFac = 1.0;
				}
				s0( 1, 1 ) += SurfNodeFac * CMat( 1 ) * Gamma2( 1, SurfNode );
				s0( 2, 1 ) += SurfNodeFac * CMat( 1 ) * Gamma2( 2, SurfNode );
				s0( 3, 1 ) += SurfNodeFac * CMat( 1 ) * Gamma2( 3, SurfNode );
				s0( 1, 2 ) += SurfNodeFac * CMat( 2 ) * Gamma2( 1, rcmax + 1 - SurfNode );
				s0( 2, 2 ) += SurfNodeFac * CMat( 2 ) * Gamma2( 2, rcmax + 1 - SurfNode );
				s0( 3, 2 ) += SurfNodeFac * CMat( 2 ) * Gamma2( 3, rcmax + 1 - SurfNode );
			}
			s0( 1, 1 ) += double( NumOfPerpendNodes - 1 ) * DMat( 1 );
			s0( 2, 2 ) += double( NumOfPerpendNodes - 1 ) * DMat( 2 );
		}

		if ( NodeSource > 0 ) {
			s0( 1, 3 ) = Gamma2( 1, NodeSource );
			s0( 2, 3 ) = Gamma2( 2, NodeSource );
			s0( 3, 3 ) = Gamma2( 3, NodeSource );
		}
		if ( NodeUserTemp > 0 ) {
			s0( 1, 4 ) = Gamma2( 1, NodeUserTemp );
			s0( 2, 4 ) = Gamma2( 2, NodeUserTemp );
			s0( 3, 4 ) = Gamma2( 3, NodeUserTemp );
		}

		// Check for and enforce symmetry in the cross term (Y)
		if ( std::abs( s0( 2, 1 ) ) != std::abs( s0( 1, 2 ) ) ) {
			avg = ( std::abs( s0( 2, 1 ) ) + std::abs( s0( 1, 2 ) ) ) * 0.5;
			s0( 2, 1 ) *= avg / std::abs( s0( 2, 1 ) );
			s0( 1, 2 ) *= avg / std::abs( s0( 1, 2 ) );
		}

		// Compute S's and e's from 1 to n-1.  See equations (2.1.25) and
		// (2.1.26) and Appendix C.
		inum = 1; // Set history term counter
		CTFConvrg = false; // Set the convergence logical to false

		// The following DO WHILE loop calculates each successive set of time
		// history terms until there are rcmax number of history terms or the
		// latest flux history term is negligibly small compared to the first
		// flux history term.
		while ( ( ! CTFConvrg ) && ( inum < rcmax ) ) { // Begin CTF calculation loop ...

			// Compute e(inum) based on Appendix C (Seem's dissertation). First,
			// compute the new PhiR0 and its trace.

			trace = 0.0;

			for ( ir = 1; ir <= rcmax; ++ir ) {

				for ( ic = 1; ic <= rcmax; ++ic ) {
					PhiR0( ic, ir ) = 0.0;
					for ( is = 1; is <= rcmax; ++is ) {
						// Make sure the next term won't cause an underflow.  If it will end up being
						// so small as to go below TinyLimit, then ignore it since it won't add anything
						// to PhiR0 anyway.
						if ( std::abs( Rnew( ic, is ) ) > TinyLimit ) {
							if ( std::abs( AExp( is, ir ) ) > std::abs( TinyLimit / Rnew( ic, is ) ) ) PhiR0( ic, ir ) += AExp( is, ir ) * Rnew( ic, is );
						}
					}
				}

				trace += PhiR0( ir, ir );

			}

			// Now calculate ej from the trace.  According to Appendix C:
			// e(j) = -Trace[AExp*R(j-1)]/j

			e( inum ) = -trace / double( inum );

			// Update Rold and compute Rnew.  Note:  PhiR0 = AExp*R(j-1) here.
			// According to Appendix C:  R(j) = AExp*R(j-1) + e(j-1)

			for ( ir = 1; ir <= rcmax; ++ir ) {
				for ( ic = 1; ic <= rcmax; ++ic ) {
					Rold( ic, ir ) = Rnew( ic, ir );
					Rnew( ic, ir ) = PhiR0( ic, ir );
				}
				Rnew( ir, ir ) += e( inum );
			}

			// Compute S(inum) based on eq.(2.1.25) which states:
			// S(j)  =  CMat*[R(j-1)*(Gamma1-Gamma2)+R(j)*Gamma2]
			//          + e(j)*DMat
			if ( SolutionDimensions == 1 ) {
				for ( j = 1; j <= 3; ++j ) {
					for ( is2 = 1; is2 <= rcmax; ++is2 ) {
						s( j, 1, inum ) += CMat( 1 ) * ( Rold( is2, 1 ) * Gamma1( j, is2 ) + Rnew( is2, 1 ) * Gamma2( j, is2 ) );
						s( j, 2, inum ) += CMat( 2 ) * ( Rold( is2, rcmax ) * Gamma1( j, is2 ) + Rnew( is2, rcmax ) * Gamma2( j, is2 ) );
						if ( NodeSource > 0 ) {
							s( j, 3, inum ) += ( Rold( is2, NodeSource ) * Gamma1( j, is2 ) + Rnew( is2, NodeSource ) * Gamma2( j, is2 ) );
						}
						if ( NodeUserTemp > 0 ) {
							s( j, 4, inum ) += ( Rold( is2, NodeUserTemp ) * Gamma1( j, is2 ) + Rnew( is2, NodeUserTemp ) * Gamma2( j, is2 ) );
						}

					}
					if ( j != 3 ) s( j, j, inum ) += e( inum ) * DMat( j );
				}
			} else { // SolutionDimensions = 2
				for ( j = 1; j <= 3; ++j ) {
					for ( is2 = 1; is2 <= rcmax; ++is2 ) {
						for ( SurfNode = 1; SurfNode <= NumOfPerpendNodes; ++SurfNode ) {
							if ( ( SurfNode == 1 ) || ( SurfNode == NumOfPerpendNodes ) ) {
								SurfNodeFac = 0.5;
							} else {
								SurfNodeFac = 1.0;
							}
							s( j, 1, inum ) += SurfNodeFac * CMat( 1 ) * ( Rold( is2, SurfNode ) * Gamma1( j, is2 ) + Rnew( is2, SurfNode ) * Gamma2( j, is2 ) );
							s( j, 2, inum ) += SurfNodeFac * CMat( 2 ) * ( Rold( is2, rcmax + 1 - SurfNode ) * Gamma1( j, is2 ) + Rnew( is2, rcmax + 1 - SurfNode ) * Gamma2( j, is2 ) );
						}
						if ( NodeSource > 0 ) {
							s( j, 3, inum ) += ( Rold( is2, NodeSource ) * Gamma1( j, is2 ) + Rnew( is2, NodeSource ) * Gamma2( j, is2 ) );
						}
						if ( NodeUserTemp > 0 ) {
							s( j, 4, inum ) += ( Rold( is2, NodeUserTemp ) * Gamma1( j, is2 ) + Rnew( is2, NodeUserTemp ) * Gamma2( j, is2 ) );
						}
					}
				}
				s( 1, 1, inum ) += e( inum ) * DMat( 1 ) * double( NumOfPerpendNodes - 1 );
				s( 2, 2, inum ) += e( inum ) * DMat( 2 ) * double( NumOfPerpendNodes - 1 );
			}

			// Check for and enforce symmetry in the cross term (Y)
			if ( std::abs( s( 2, 1, inum ) ) != std::abs( s( 1, 2, inum ) ) ) {
				avg = ( std::abs( s( 2, 1, inum ) ) + std::abs( s( 1, 2, inum ) ) ) * 0.5;
				s( 2, 1, inum ) *= avg / std::abs( s( 2, 1, inum ) );
				s( 1, 2, inum ) *= avg / std::abs( s( 1, 2, inum ) );
			}

			// Check for convergence of the CTFs.
			if ( e( 1 ) == 0.0 ) {

				nrf = 1; // e(1) is zero, so there are no history terms.
				CTFConvrg = true; // CTF calculations have converged--set logical.

			} else {
				// e(1) is non-zero -- Calculate and compare the ratio of the flux
				// terms to the convergence limit.
				rat = std::abs( e( inum ) / e( 1 ) );

				if ( rat < ConvrgLim ) {

					// If the ratio is less than the convergence limit, then any other
					// terms would have a neglible impact on the CTF-based energy balances.
					nrf = inum;
					CTFConvrg = true; // CTF calculations have converged--set logical.

				}
			} // ... end of convergence check block.

			++inum;

		} // ... end of CTF calculation loop.
		// Continue to the next coefficient if the solution has not converged
		if ( ! CTFConvrg ) { // Compute last e and S, if still unconverged.

			// Compute e(inum) based on Appendix C (Seem's dissertation) or see
			// equation above.  First compute the new PhiR0 and its trace.

			trace = 0.0;

			for ( ir = 1; ir <= rcmax; ++ir ) {
				for ( is = 1; is <= rcmax; ++is ) {
					trace += AExp( is, ir ) * Rnew( ir, is );
				}
			}

			e( rcmax ) = -trace / double( rcmax ); // Now calculate ej from the trace.

			// Compute S(inum) based on eq.(2.1.25) which states:
			//   S(last) = CMat*R(last-1)*(Gamma1-Gamma2)+e(last)*DMat

			if ( SolutionDimensions == 1 ) {
				for ( j = 1; j <= 3; ++j ) {
					for ( is2 = 1; is2 <= rcmax; ++is2 ) {
						s( j, 1, rcmax ) += CMat( 1 ) * Rnew( is2, 1 ) * Gamma1( j, is2 );
						s( j, 2, rcmax ) += CMat( 2 ) * Rnew( is2, rcmax ) * Gamma1( j, is2 );
						if ( NodeSource > 0 ) {
							s( j, 3, rcmax ) += Rnew( is2, NodeSource ) * Gamma1( j, is2 );
						}
						if ( NodeUserTemp > 0 ) {
							s( j, 4, rcmax ) += Rnew( is2, NodeUserTemp ) * Gamma1( j, is2 );
						}
					}
				}
				s( 1, 1, rcmax ) += e( rcmax ) * DMat( 1 );
				s( 2, 2, rcmax ) += e( rcmax ) * DMat( 2 );
				nrf = rcmax;
			} else { // SolutionDimensions = 2
				for ( j = 1; j <= 3; ++j ) {
					for ( is2 = 1; is2 <= rcmax; ++is2 ) {
						for ( SurfNode = 1; SurfNode <= NumOfPerpendNodes; ++SurfNode ) {
							if ( ( SurfNode == 1 ) || ( SurfNode == NumOfPerpendNodes ) ) {
								SurfNodeFac = 0.5;
							} else {
								SurfNodeFac = 1.0;
							}
							s( j, 1, rcmax ) += SurfNodeFac * CMat( 1 ) * Rnew( is2, SurfNode ) * Gamma1( j, is2 );
							s( j, 2, rcmax ) += SurfNodeFac * CMat( 2 ) * Rnew( is2, rcmax + 1 - SurfNode ) * Gamma1( j, is2 );
						}
						if ( NodeSource > 0 ) {
							s( j, 3, rcmax ) += Rnew( is2, NodeSource ) * Gamma1( j, is2 );
						}
						if ( NodeUserTemp > 0 ) {
							s( j, 4, rcmax ) += Rnew( is2, NodeUserTemp ) * Gamma1( j, is2 );
						}
					}
				}
				s( 1, 1, rcmax ) += e( rcmax ) * DMat( 1 ) * double( NumOfPerpendNodes - 1 );
				s( 2, 2, rcmax ) += e( rcmax ) * DMat( 2 ) * double( NumOfPerpendNodes - 1 );
			}

			// Check for and enforce symmetry in the cross term (Y)

			if ( std::abs( s( 2, 1, rcmax ) ) != std::abs( s( 1, 2, rcmax ) ) ) {
				avg = ( std::abs( s( 2, 1, rcmax ) ) + std::abs( s( 1, 2, rcmax ) ) ) * 0.5;
				s( 2, 1, rcmax ) *= avg / std::abs( s( 2, 1, rcmax ) );
				s( 1, 2, rcmax ) *= avg / std::abs( s( 1, 2, rcmax ) );
			}

		} // ... end of IF block for calculation of last e and S.

		PhiR0.deallocate();
		Rnew.deallocate();
		Rold.deallocate();

		return; // The array e and the matrices s and s0 now contain the conduction
		// transfer functions for this construction.

	}

	void
	ReportCTFs( bool const DoReportBecauseError )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   July 1999
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine gives a detailed report to the user about
		// the conduction transfer functions and other thermal data
		// of each construction.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::ScanForReports;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//  CHARACTER(len=12),DIMENSION(6)  :: Roughness = (/'VeryRough   ','Rough       ','MediumRough ', &
		//                                                   'MediumSmooth','Smooth      ','VerySmooth  '/)
		bool DoReport;

		int ThisNum;
		int Layer;
		int I;

		// Formats
		static gio::Fmt fmtA( "(A)" );
		static gio::Fmt Format_700( "(' Construction CTF,',A,3(',',I4),',',F8.3,',',G15.4,4(',',F8.3),',',A)" );
		static gio::Fmt Format_701( "(' Material CTF Summary,',A,',',F8.4,',',F14.3,',',F11.3,',',F13.3,',',G12.4)" );
		static gio::Fmt Format_702( "(' Material:Air,',A,',',G12.4)" );
		static gio::Fmt Format_703( "(' CTF,',I4,4(',',G20.8))" );
		static gio::Fmt Format_704( "(' CTF,',I4,3(',',G20.8))" );
		static gio::Fmt Format_705( "(' QTF,',I4,2(',',G20.8))" );
		static gio::Fmt Format_706( "(' Source/Sink Loc Internal Temp QTF,',I4,3(',',G20.8))" );
		static gio::Fmt Format_707( "(' User Loc Internal Temp QTF,',I4,3(',',G20.8))" );

		ScanForReports( "Constructions", DoReport, "Constructions" );

		if ( DoReport || DoReportBecauseError ) {
			//                                      Write Descriptions
			gio::write( OutputFileInits, fmtA ) << "! <Construction CTF>,Construction Name,Index,#Layers,#CTFs,Time Step {hours},ThermalConductance {w/m2-K},OuterThermalAbsorptance,InnerThermalAbsorptance,OuterSolarAbsorptance,InnerSolarAbsorptance,Roughness";
			gio::write( OutputFileInits, fmtA ) << "! <Material CTF Summary>,Material Name,Thickness {m},Conductivity {w/m-K},Density {kg/m3},Specific Heat {J/kg-K},ThermalResistance {m2-K/w}";
			gio::write( OutputFileInits, fmtA ) << "! <Material:Air>,Material Name,ThermalResistance {m2-K/w}";
			gio::write( OutputFileInits, fmtA ) << "! <CTF>,Time,Outside,Cross,Inside,Flux (except final one)";

			for ( ThisNum = 1; ThisNum <= TotConstructs; ++ThisNum ) {

				if ( Construct( ThisNum ).TypeIsWindow ) continue;

				gio::write( OutputFileInits, Format_700 ) << Construct( ThisNum ).Name << ThisNum << Construct( ThisNum ).TotLayers << Construct( ThisNum ).NumCTFTerms << Construct( ThisNum ).CTFTimeStep << Construct( ThisNum ).UValue << Construct( ThisNum ).OutsideAbsorpThermal << Construct( ThisNum ).InsideAbsorpThermal << Construct( ThisNum ).OutsideAbsorpSolar << Construct( ThisNum ).InsideAbsorpSolar << DisplayMaterialRoughness( Construct( ThisNum ).OutsideRoughness );

				for ( I = 1; I <= Construct( ThisNum ).TotLayers; ++I ) {
					Layer = Construct( ThisNum ).LayerPoint( I );
					{ auto const SELECT_CASE_var( Material( Layer ).Group );
					if ( SELECT_CASE_var == Air ) {
						gio::write( OutputFileInits, Format_702 ) << Material( Layer ).Name << Material( Layer ).Resistance;
					} else {
						gio::write( OutputFileInits, Format_701 ) << Material( Layer ).Name << Material( Layer ).Thickness << Material( Layer ).Conductivity << Material( Layer ).Density << Material( Layer ).SpecHeat << Material( Layer ).Resistance;
					}}
				}

				for ( I = Construct( ThisNum ).NumCTFTerms; I >= 0; --I ) {
					if ( I != 0 ) {
						gio::write( OutputFileInits, Format_703 ) << I << Construct( ThisNum ).CTFOutside( I ) << Construct( ThisNum ).CTFCross( I ) << Construct( ThisNum ).CTFInside( I ) << Construct( ThisNum ).CTFFlux( I );
					} else {
						gio::write( OutputFileInits, Format_704 ) << I << Construct( ThisNum ).CTFOutside( I ) << Construct( ThisNum ).CTFCross( I ) << Construct( ThisNum ).CTFInside( I );
					}
				}

				if ( Construct( ThisNum ).SourceSinkPresent ) {
					// QTFs...
					for ( I = Construct( ThisNum ).NumCTFTerms; I >= 0; --I ) {
						gio::write( OutputFileInits, Format_705 ) << I << Construct( ThisNum ).CTFSourceOut( I ) << Construct( ThisNum ).CTFSourceIn( I );
					}
					// QTFs for source/sink location temperature calculation...
					for ( I = Construct( ThisNum ).NumCTFTerms; I >= 0; --I ) {
						gio::write( OutputFileInits, Format_706 ) << I << Construct( ThisNum ).CTFTSourceOut( I ) << Construct( ThisNum ).CTFTSourceIn( I ) << Construct( ThisNum ).CTFTSourceQ( I );
					}
					if ( Construct( ThisNum ).TempAfterLayer != 0 ) {
						// QTFs for user specified interior temperature calculation...
						for ( I = Construct( ThisNum ).NumCTFTerms; I >= 0; --I ) {
							gio::write( OutputFileInits, Format_707 ) << I << Construct( ThisNum ).CTFTUserOut( I ) << Construct( ThisNum ).CTFTUserIn( I ) << Construct( ThisNum ).CTFTUserSource( I );
						}
					}
				}

			}

		}

	}

} // ConductionTransferFunctionCalc

} // EnergyPlus
