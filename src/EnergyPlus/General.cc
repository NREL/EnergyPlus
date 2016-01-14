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
#include <cstdlib>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <General.hh>
#include <DataGlobals.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataPrecisionGlobals.hh>
#include <DataRuntimeLanguage.hh>
#include <DataStringGlobals.hh>
#include <DataSurfaces.hh>
#include <InputProcessor.hh>
#include <UtilityRoutines.hh>

#if defined( _WIN32 ) && _MSC_VER < 1900
#define snprintf _snprintf
#endif

namespace EnergyPlus {

namespace General {

	// Module containing routines for general use

	// MODULE INFORMATION:
	//       AUTHOR         Fred Buhl, Linda Lawrie
	//       DATE WRITTEN   December 2001
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// contains routines (most likely numeric) that may be needed in several parts
	// of EnergyPlus

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES: none

	// OTHER NOTES: none

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	// This module should not contain variables in the module sense as it is
	// intended strictly to provide "interfaces" to routines used by other
	// parts of the simulation.

	// MODULE PARAMETER DEFINITIONS
	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS
	// na

	// INTERFACE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	// na

	//SUBROUTINE SPECIFICATIONS FOR MODULE General
	//PUBLIC  SaveCompDesWaterFlow
	//PUBLIC  ErfFunction

	// Functions

	void
	SolveRegulaFalsi(
		Real64 const Eps, // required absolute accuracy
		int const MaxIte, // maximum number of allowed iterations
		int & Flag, // integer storing exit status
		Real64 & XRes, // value of x that solves f(x,Par) = 0
		std::function< Real64( Real64 const, Array1< Real64 > const & ) > f,
		Real64 const X_0, // 1st bound of interval that contains the solution
		Real64 const X_1, // 2nd bound of interval that contains the solution
		Array1< Real64 > const & Par // array with additional parameters used for function evaluation
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Michael Wetter
		//       DATE WRITTEN   March 1999
		//       MODIFIED       Fred Buhl November 2000, R. Raustad October 2006 - made subroutine RECURSIVE
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Find the value of x between x0 and x1 such that f(x,Par)
		// is equal to zero.

		// METHODOLOGY EMPLOYED:
		// Uses the Regula Falsi (false position) method (similar to secant method)

		// REFERENCES:
		// See Press et al., Numerical Recipes in Fortran, Cambridge University Press,
		// 2nd edition, 1992. Page 347 ff.

		// USE STATEMENTS:
		// na

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// = -2: f(x0) and f(x1) have the same sign
		// = -1: no convergence
		// >  0: number of iterations performed
		// optional
		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const SMALL( 1.e-10 );

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 X0; // present 1st bound
		Real64 X1; // present 2nd bound
		Real64 XTemp; // new estimate
		Real64 Y0; // f at X0
		Real64 Y1; // f at X1
		Real64 YTemp; // f at XTemp
		Real64 DY; // DY = Y0 - Y1
		bool Conv; // flag, true if convergence is achieved
		bool StopMaxIte; // stop due to exceeding of maximum # of iterations
		bool Cont; // flag, if true, continue searching
		int NIte; // number of interations

		X0 = X_0;
		X1 = X_1;
		Conv = false;
		StopMaxIte = false;
		Cont = true;
		NIte = 0;

		Y0 = f( X0, Par );
		Y1 = f( X1, Par );
		// check initial values
		if ( Y0 * Y1 > 0 ) {
			Flag = -2;
			XRes = X0;
			return;
		}

		while ( Cont ) {

			DY = Y0 - Y1;
			if ( std::abs( DY ) < SMALL ) DY = SMALL;
			// new estimation
			XTemp = ( Y0 * X1 - Y1 * X0 ) / DY;
			YTemp = f( XTemp, Par );

			++NIte;

			// check convergence
			if ( std::abs( YTemp ) < Eps ) Conv = true;

			if ( NIte > MaxIte ) StopMaxIte = true;

			if ( ( ! Conv ) && ( ! StopMaxIte ) ) {
				Cont = true;
			} else {
				Cont = false;
			}

			if ( Cont ) {

				// reassign values (only if further iteration required)
				if ( Y0 < 0.0 ) {
					if ( YTemp < 0.0 ) {
						X0 = XTemp;
						Y0 = YTemp;
					} else {
						X1 = XTemp;
						Y1 = YTemp;
					}
				} else {
					if ( YTemp < 0.0 ) {
						X1 = XTemp;
						Y1 = YTemp;
					} else {
						X0 = XTemp;
						Y0 = YTemp;
					}
				} // ( Y0 < 0 )

			} // (Cont)

		} // Cont

		if ( Conv ) {
			Flag = NIte;
		} else {
			Flag = -1;
		}
		XRes = XTemp;

	}

	void
	SolveRegulaFalsi(
		Real64 const Eps, // required absolute accuracy
		int const MaxIte, // maximum number of allowed iterations
		int & Flag, // integer storing exit status
		Real64 & XRes, // value of x that solves f(x) = 0
		std::function< Real64( Real64 const ) > f,
		Real64 const X_0, // 1st bound of interval that contains the solution
		Real64 const X_1 // 2nd bound of interval that contains the solution
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Michael Wetter
		//       DATE WRITTEN   March 1999
		//       MODIFIED       Fred Buhl November 2000, R. Raustad October 2006 - made subroutine RECURSIVE
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Find the value of x between x0 and x1 such that f(x)
		// is equal to zero.

		// METHODOLOGY EMPLOYED:
		// Uses the Regula Falsi (false position) method (similar to secant method)

		// REFERENCES:
		// See Press et al., Numerical Recipes in Fortran, Cambridge University Press,
		// 2nd edition, 1992. Page 347 ff.

		// USE STATEMENTS:
		// na

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// = -2: f(x0) and f(x1) have the same sign
		// = -1: no convergence
		// >  0: number of iterations performed
		// optional
		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const SMALL( 1.e-10 );

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 X0; // present 1st bound
		Real64 X1; // present 2nd bound
		Real64 XTemp; // new estimate
		Real64 Y0; // f at X0
		Real64 Y1; // f at X1
		Real64 YTemp; // f at XTemp
		Real64 DY; // DY = Y0 - Y1
		bool Conv; // flag, true if convergence is achieved
		bool StopMaxIte; // stop due to exceeding of maximum # of iterations
		bool Cont; // flag, if true, continue searching
		int NIte; // number of interations

		X0 = X_0;
		X1 = X_1;
		Conv = false;
		StopMaxIte = false;
		Cont = true;
		NIte = 0;

		Y0 = f( X0 );
		Y1 = f( X1 );
		// check initial values
		if ( Y0 * Y1 > 0 ) {
			Flag = -2;
			XRes = X0;
			return;
		}

		while ( Cont ) {

			DY = Y0 - Y1;
			if ( std::abs( DY ) < SMALL ) DY = SMALL;
			// new estimation
			XTemp = ( Y0 * X1 - Y1 * X0 ) / DY;
			YTemp = f( XTemp );

			++NIte;

			// check convergence
			if ( std::abs( YTemp ) < Eps ) Conv = true;

			if ( NIte > MaxIte ) StopMaxIte = true;

			if ( ( ! Conv ) && ( ! StopMaxIte ) ) {
				Cont = true;
			} else {
				Cont = false;
			}

			if ( Cont ) {

				// reassign values (only if further iteration required)
				if ( Y0 < 0.0 ) {
					if ( YTemp < 0.0 ) {
						X0 = XTemp;
						Y0 = YTemp;
					} else {
						X1 = XTemp;
						Y1 = YTemp;
					}
				} else {
					if ( YTemp < 0.0 ) {
						X1 = XTemp;
						Y1 = YTemp;
					} else {
						X0 = XTemp;
						Y0 = YTemp;
					}
				} // ( Y0 < 0 )

			} // (Cont)

		} // Cont

		if ( Conv ) {
			Flag = NIte;
		} else {
			Flag = -1;
		}
		XRes = XTemp;

	}

	Real64
	InterpSw(
		Real64 const SwitchFac, // Switching factor: 0.0 if glazing is unswitched, = 1.0 if fully switched
		Real64 const A, // Glazing property in unswitched state
		Real64 const B // Glazing property in fully switched state
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   February 1999

		// PURPOSE OF THIS FUNCTION:
		// For switchable glazing, calculates a weighted average of properties
		// A and B

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 InterpSw;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 locSwitchFac;
		// bound SwitchFac

		locSwitchFac = min( SwitchFac, 1.0 );
		locSwitchFac = max( locSwitchFac, 0.0 );

		InterpSw = ( 1.0 - locSwitchFac ) * A + locSwitchFac * B;
		return InterpSw;
	}

	Real64
	InterpBlind(
		Real64 const ProfAng, // Profile angle (rad)
		Array1A< Real64 > const PropArray // Array of blind properties
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   May 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Does profile-angle interpolation of window blind solar-thermal properties

		// METHODOLOGY EMPLOYED:
		// Linear interpolation.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::Pi;
		using DataGlobals::PiOvr2;

		// Return value
		Real64 InterpBlind;

		// Argument array dimensioning
		PropArray.dim( 37 );

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		Real64 const DeltaAngRad( Pi / 36.0 ); // Profile angle increment (rad)

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 InterpFac; // Interpolation factor
		int IAlpha; // Profile angle index

		if ( ProfAng > PiOvr2 || ProfAng < -PiOvr2 ) {
			InterpBlind = 0.0;
		} else {
			IAlpha = 1 + int( ( ProfAng + PiOvr2 ) / DeltaAngRad );
			InterpFac = ( ProfAng - ( -PiOvr2 + DeltaAngRad * ( IAlpha - 1 ) ) ) / DeltaAngRad;
			InterpBlind = ( 1.0 - InterpFac ) * PropArray( IAlpha ) + InterpFac * PropArray( IAlpha + 1 );
		}
		return InterpBlind;
	}

	Real64
	InterpProfAng(
		Real64 const ProfAng, // Profile angle (rad)
		Array1S< Real64 > const PropArray // Array of blind properties
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   May 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Does profile-angle interpolation of window blind solar-thermal properties

		// METHODOLOGY EMPLOYED:
		// Linear interpolation.

		// REFERENCES:na

		// Using/Aliasing
		using DataGlobals::Pi;
		using DataGlobals::PiOvr2;

		// Return value
		Real64 InterpProfAng;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		Real64 const DeltaAngRad( Pi / 36.0 ); // Profile angle increment (rad)

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 InterpFac; // Interpolation factor
		int IAlpha; // Profile angle index

		// DeltaAng = Pi/36
		if ( ProfAng > PiOvr2 || ProfAng < -PiOvr2 ) {
			InterpProfAng = 0.0;
		} else {
			IAlpha = 1 + int( ( ProfAng + PiOvr2 ) / DeltaAngRad );
			InterpFac = ( ProfAng - ( -PiOvr2 + DeltaAngRad * ( IAlpha - 1 ) ) ) / DeltaAngRad;
			InterpProfAng = ( 1.0 - InterpFac ) * PropArray( IAlpha ) + InterpFac * PropArray( IAlpha + 1 );
		}
		return InterpProfAng;
	}

//	Real64
//	InterpSlatAng(
//		Real64 const SlatAng, // Slat angle (rad)
//		bool const VarSlats, // True if slat angle is variable
//		Array1A< Real64 > const PropArray // Array of blind properties as function of slat angle
//	)
//	{
//
//		// SUBROUTINE INFORMATION:
//		//       AUTHOR         Fred Winkelmann
//		//       DATE WRITTEN   Dec 2001
//		//       MODIFIED       na
//		//       RE-ENGINEERED  na
//
//		// PURPOSE OF THIS SUBROUTINE:
//		// Does slat-angle interpolation of window blind solar-thermal properties that
//		// do not depend on profile angle
//
//		// METHODOLOGY EMPLOYED:
//		// Linear interpolation.
//
//		// REFERENCES:na
//
//		// USE STATEMENTS:
//		// Using/Aliasing
//		using DataGlobals::Pi;
//		using DataGlobals::PiOvr2;
//		using DataSurfaces::MaxSlatAngs;
//
//		// Return value
//		Real64 InterpSlatAng;
//
//		// Argument array dimensioning
//		PropArray.dim( MaxSlatAngs );
//
//		// Locals
//		// FUNCTION ARGUMENT DEFINITIONS:
//
//		// FUNCTION PARAMETER DEFINITIONS:
//		Real64 const DeltaAng( Pi / ( double( MaxSlatAngs ) - 1.0 ) );
//
//		// FUNCTION LOCAL VARIABLE DECLARATIONS:
//		Real64 InterpFac; // Interpolation factor
//		int IBeta; // Slat angle index
//		Real64 SlatAng1;
//
//		if ( SlatAng > Pi || SlatAng < 0.0 ) {
//			//  InterpSlatAng = 0.0
//			//  RETURN
//			//END IF
//			SlatAng1 = min( max( SlatAng, 0.0 ), Pi );
//		} else {
//			SlatAng1 = SlatAng;
//		}
//
//		if ( VarSlats ) { // Variable-angle slats
//			IBeta = 1 + int( SlatAng1 / DeltaAng );
//			InterpFac = ( SlatAng1 - DeltaAng * ( IBeta - 1 ) ) / DeltaAng;
//			InterpSlatAng = PropArray( IBeta ) + InterpFac * ( PropArray( min( MaxSlatAngs, IBeta + 1 ) ) - PropArray( IBeta ) );
//		} else { // Fixed-angle slats or shade
//			InterpSlatAng = PropArray( 1 );
//		}
//
//		return InterpSlatAng;
//	}

	Real64
	InterpSlatAng(
		Real64 const SlatAng, // Slat angle (rad)
		bool const VarSlats, // True if slat angle is variable
		Array1S< Real64 > const PropArray // Array of blind properties as function of slat angle
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   Dec 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Does slat-angle interpolation of window blind solar-thermal properties that
		// do not depend on profile angle

		// METHODOLOGY EMPLOYED:
		// Linear interpolation.

		// REFERENCES:na

		// Using/Aliasing
		using DataGlobals::Pi;
		using DataSurfaces::MaxSlatAngs;

		// Return value
		Real64 InterpSlatAng;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		static Real64 const DeltaAng( Pi / ( double( MaxSlatAngs ) - 1.0 ) );
		static Real64 const DeltaAng_inv( ( double( MaxSlatAngs ) - 1.0 ) / Pi );

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 InterpFac; // Interpolation factor
		int IBeta; // Slat angle index
		Real64 SlatAng1;

		if ( SlatAng > Pi || SlatAng < 0.0 ) {
			//  InterpSlatAng = 0.0
			//  RETURN
			//END IF
			SlatAng1 = min( max( SlatAng, 0.0 ), Pi );
		} else {
			SlatAng1 = SlatAng;
		}

		if ( VarSlats ) { // Variable-angle slats
			IBeta = 1 + int( SlatAng1 * DeltaAng_inv );
			InterpFac = ( SlatAng1 - DeltaAng * ( IBeta - 1 ) ) * DeltaAng_inv;
			InterpSlatAng = PropArray( IBeta ) + InterpFac * ( PropArray( min( MaxSlatAngs, IBeta + 1 ) ) - PropArray( IBeta ) );
		} else { // Fixed-angle slats or shade
			InterpSlatAng = PropArray( 1 );
		}

		return InterpSlatAng;
	}

	Real64
	InterpProfSlatAng(
		Real64 const ProfAng, // Profile angle (rad)
		Real64 const SlatAng, // Slat angle (rad)
		bool const VarSlats, // True if variable-angle slats
		Array2A< Real64 > const PropArray // Array of blind properties
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   Dec 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Does simultaneous profile-angle and slat-angle interpolation of window
		// blind solar-thermal properties that depend on profile angle and slat angle

		// METHODOLOGY EMPLOYED:
		// Linear interpolation.

		// REFERENCES:na

		// Using/Aliasing
		using DataGlobals::Pi;
		using DataGlobals::PiOvr2;
		using DataSurfaces::MaxSlatAngs;

		// Return value
		Real64 InterpProfSlatAng;

		// Argument array dimensioning
		PropArray.dim( MaxSlatAngs, 37 );

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		Real64 const DeltaProfAng( Pi / 36.0 );
		Real64 const DeltaSlatAng( Pi / ( double( MaxSlatAngs ) - 1.0 ) );

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 ProfAngRatio; // Profile angle interpolation factor
		Real64 SlatAngRatio; // Slat angle interpolation factor
		int IAlpha; // Profile angle index
		int IBeta; // Slat angle index
		Real64 Val1; // Property values at points enclosing the given ProfAngle and SlatAngle
		Real64 Val2;
		Real64 Val3;
		Real64 Val4;
		Real64 ValA; // Property values at given SlatAngle to be interpolated in profile angle
		Real64 ValB;
		Real64 SlatAng1;
		Real64 ProfAng1;

		if ( SlatAng > Pi || SlatAng < 0.0 || ProfAng > PiOvr2 || ProfAng < -PiOvr2 ) {
			//  InterpProfSlatAng = 0.0
			//  RETURN
			SlatAng1 = min( max( SlatAng, 0.0 ), Pi );

			// This is not correct, fixed 2/17/2010
			//ProfAng1 = MIN(MAX(SlatAng,-PiOvr2),PiOvr2)
			ProfAng1 = min( max( ProfAng, -PiOvr2 ), PiOvr2 );
		} else {
			SlatAng1 = SlatAng;
			ProfAng1 = ProfAng;
		}

		IAlpha = int( ( ProfAng1 + PiOvr2 ) / DeltaProfAng ) + 1;
		ProfAngRatio = ( ProfAng1 + PiOvr2 - ( IAlpha - 1 ) * DeltaProfAng ) / DeltaProfAng;

		if ( VarSlats ) { // Variable-angle slats: interpolate in profile angle and slat angle
			IBeta = int( SlatAng1 / DeltaSlatAng ) + 1;
			SlatAngRatio = ( SlatAng1 - ( IBeta - 1 ) * DeltaSlatAng ) / DeltaSlatAng;
			Val1 = PropArray( IBeta, IAlpha );
			Val2 = PropArray( min( MaxSlatAngs, IBeta + 1 ), IAlpha );
			Val3 = PropArray( IBeta, min( 37, IAlpha + 1 ) );
			Val4 = PropArray( min( MaxSlatAngs, IBeta + 1 ), min( 37, IAlpha + 1 ) );
			ValA = Val1 + SlatAngRatio * ( Val2 - Val1 );
			ValB = Val3 + SlatAngRatio * ( Val4 - Val3 );
			InterpProfSlatAng = ValA + ProfAngRatio * ( ValB - ValA );
		} else { // Fixed-angle slats: interpolate only in profile angle
			Val1 = PropArray( 1, IAlpha );
			Val2 = PropArray( 1, min( 37, IAlpha + 1 ) );
			InterpProfSlatAng = Val1 + ProfAngRatio * ( Val2 - Val1 );
		}

		return InterpProfSlatAng;
	}

	Real64
	BlindBeamBeamTrans(
		Real64 const ProfAng, // Solar profile angle (rad)
		Real64 const SlatAng, // Slat angle (rad)
		Real64 const SlatWidth, // Slat width (m)
		Real64 const SlatSeparation, // Slat separation (distance between surfaces of adjacent slats) (m)
		Real64 const SlatThickness // Slat thickness (m)
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   Jan 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates beam-to-beam transmittance of a window blind

		// METHODOLOGY EMPLOYED:
		// Based on solar profile angle and slat geometry

		// REFERENCES:na

		// Using/Aliasing
		using DataGlobals::Pi;
		using DataGlobals::PiOvr2;

		// Return value
		Real64 BlindBeamBeamTrans;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 fEdge; // Slat edge correction factor
		Real64 wbar; // Intermediate variable
		Real64 gamma; // Intermediate variable
		Real64 fEdge1; // Intermediate variable
		Real64 CosProfAng; // Cosine of profile angle

		CosProfAng = std::cos( ProfAng );
		gamma = SlatAng - ProfAng;
		wbar = SlatSeparation;
		if ( CosProfAng != 0.0 ) wbar = SlatWidth * std::cos( gamma ) / CosProfAng;
		BlindBeamBeamTrans = max( 0.0, 1.0 - std::abs( wbar / SlatSeparation ) );

		if ( BlindBeamBeamTrans > 0.0 ) {

			// Correction factor that accounts for finite thickness of slats. It is used to modify the
			// blind transmittance to account for reflection and absorption by the slat edges.
			// fEdge is ratio of area subtended by edge of slat to area between tops of adjacent slats.

			fEdge = 0.0;
			fEdge1 = 0.0;
			if ( std::abs( std::sin( gamma ) ) > 0.01 ) {
				if ( ( SlatAng > 0.0 && SlatAng <= PiOvr2 && ProfAng <= SlatAng ) || ( SlatAng > PiOvr2 && SlatAng <= Pi && ProfAng > -( Pi - SlatAng ) ) ) fEdge1 = SlatThickness * std::abs( std::sin( gamma ) ) / ( ( SlatSeparation + SlatThickness / std::abs( std::sin( SlatAng ) ) ) * CosProfAng );
				fEdge = min( 1.0, std::abs( fEdge1 ) );
			}
			BlindBeamBeamTrans *= ( 1.0 - fEdge );

		}

		return BlindBeamBeamTrans;
	}

	Real64
	POLYF(
		Real64 const X, // Cosine of angle of incidence
		Array1A< Real64 > const A // Polynomial coefficients
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   February 1999
		//       DATE MODIFIED  October 1999, FW: change to 6th order polynomial over
		//                        entire incidence angle range

		// PURPOSE OF THIS FUNCTION:
		// Evaluates glazing beam transmittance or absorptance of the form
		// A(1)*X + A(2)*X^2 + A(3)*X^3 + A(4)*X^4 + A(5)*X^5 + A(6)*X^6
		// where X is the cosine of the angle of incidence (0.0 to 1.0)

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 POLYF;

		// Argument array dimensioning
		A.dim( 6 );

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		if ( X < 0.0 || X > 1.0 ) {
			POLYF = 0.0;
		} else {
			POLYF = X * ( A( 1 ) + X * ( A( 2 ) + X * ( A( 3 ) + X * ( A( 4 ) + X * ( A( 5 ) + X * A( 6 ) ) ) ) ) );
		}
		return POLYF;
	}

	Real64
	POLYF(
		Real64 const X, // Cosine of angle of incidence
		Array1< Real64 > const & A // Polynomial coefficients
	)
	{
		// Return value
		Real64 POLYF;

		if ( X < 0.0 || X > 1.0 ) {
			POLYF = 0.0;
		} else {
			POLYF = X * ( A( 1 ) + X * ( A( 2 ) + X * ( A( 3 ) + X * ( A( 4 ) + X * ( A( 5 ) + X * A( 6 ) ) ) ) ) );
		}
		return POLYF;
	}

	Real64
	POLYF(
		Real64 const X, // Cosine of angle of incidence
		Array1S< Real64 > const & A // Polynomial coefficients
	)
	{
		// Return value
		Real64 POLYF;

		if ( X < 0.0 || X > 1.0 ) {
			POLYF = 0.0;
		} else {
			POLYF = X * ( A( 1 ) + X * ( A( 2 ) + X * ( A( 3 ) + X * ( A( 4 ) + X * ( A( 5 ) + X * A( 6 ) ) ) ) ) );
		}
		return POLYF;
	}

	Real64
	POLY1F(
		Real64 & X, // independent variable
		Array1A< Real64 > A, // array of polynomial coefficients
		int & N // number of terms in polynomial
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         George N. Walton
		//       DATE WRITTEN   May 1977
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function evaluates a polynomial of the form:
		// POLY = A(1) + A(2)*X + A(3)*X**2 + ... + A(N)*X**(N-1)

		// METHODOLOGY EMPLOYED:
		// Uses Horner's Rule.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 POLY1F;

		// Argument array dimensioning
		A.dim( N );

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int I; // Loop parameter
		Real64 SUM; // Temporary summation variable

		SUM = A( N );
		for ( I = 2; I <= N; ++I ) {
			SUM = SUM * X + A( N - I + 1 );
		}

		POLY1F = SUM;

		return POLY1F;

	}

	Real64
	POLY2F(
		Real64 & X, // independent variable
		Array1A< Real64 > A, // array of polynomial coefficients
		int & N // number of terms in polynomial
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         George N. Walton
		//       DATE WRITTEN   May 1977
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function evaluates a polynomial of the form:
		// POLY = A(1)*X + A(2)*X**2 + ... + A(N)*X**N

		// METHODOLOGY EMPLOYED:
		// Uses Horner's Rule.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 POLY2F;

		// Argument array dimensioning
		A.dim( N );

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int I; // Loop parameter
		Real64 SUM; // Temporary summation variable

		SUM = A( N ) * X;
		for ( I = 2; I <= N; ++I ) {
			SUM = X * ( SUM + A( N - I + 1 ) );
		}

		POLY2F = SUM;

		return POLY2F;

	}

	std::string
	TrimSigDigits(
		Real64 const RealValue,
		int const SigDigits
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   March 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function accepts a number as parameter as well as the number of
		// significant digits after the decimal point to report and returns a string
		// that is appropriate.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		//USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY : IEEE_IS_NAN ! Use IEEE_IS_NAN when GFortran supports it

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		static std::string const NAN_string( "NAN" );
		static std::string const ZEROOOO( "0.000000000000000000000000000" );
		static gio::Fmt fmtLD( "*" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		if ( std::isnan( RealValue ) ) return NAN_string;

		std::string String; // Working string
		if ( RealValue != 0.0 ) {
			gio::write( String, fmtLD ) << RealValue;
		} else {
			String = ZEROOOO;
		}
		std::string::size_type const EPos = index( String, 'E' ); // Position of E in original string format xxEyy
		std::string EString; // E string retained from original string
		if ( EPos != std::string::npos ) {
			EString = String.substr( EPos );
			String.erase( EPos );
		}
		std::string::size_type const DotPos = index( String, '.' ); // Position of decimal point in original string
		std::string::size_type const SLen = len( String ); // Length of String (w/o E part)
		bool IncludeDot; // True when decimal point output
		if ( SigDigits > 0 || EString != "" ) {
			IncludeDot = true;
		} else {
			IncludeDot = false;
		}
		if ( IncludeDot ) {
			String.erase( min( DotPos + SigDigits + 1, SLen ) );
			String += EString;
		} else {
			String.erase( DotPos );
		}
		return stripped( String );

	}

	std::string
	TrimSigDigits(
		int const IntegerValue,
		Optional_int_const EP_UNUSED( SigDigits ) // ignored
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   March 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function accepts a number as parameter as well as the number of
		// significant digits after the decimal point to report and returns a string
		// that is appropriate.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		static gio::Fmt fmtLD( "*" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		std::string String; // Working string

		gio::write( String, fmtLD ) << IntegerValue;
		return stripped( String );
	}

	std::string
	RoundSigDigits(
		Real64 const RealValue,
		int const SigDigits
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   March 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function accepts a number as parameter as well as the number of
		// significant digits after the decimal point to report and returns a string
		// that is appropriate.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		//USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY : IEEE_IS_NAN ! Use IEEE_IS_NAN when GFortran supports it

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		static std::string const DigitChar( "01234567890" );
		static std::string const NAN_string( "NAN" );
		static std::string const ZEROOOO( "0.000000000000000000000000000" );
		static gio::Fmt fmtLD( "*" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		if ( std::isnan( RealValue ) ) return NAN_string;

		std::string String; // Working string
		if ( RealValue != 0.0 ) {
			gio::write( String, fmtLD ) << RealValue;
		} else {
			String = ZEROOOO;
		}

		std::string::size_type const EPos = index( String, 'E' ); // Position of E in original string format xxEyy
		std::string EString; // E string retained from original string
		if ( EPos != std::string::npos ) {
			EString = String.substr( EPos );
			String.erase( EPos );
		}

		std::string::size_type const DotPos = index( String, '.' ); // Position of decimal point in original string
		assert( DotPos != std::string::npos );
		assert( DotPos > 0 ); // Or SPos will not be valid
		char TestChar( DotPos + SigDigits + 1 < String.length() ? String[ DotPos + SigDigits + 1 ] : ' ' ); // Test character (digit) for rounding, if position in digit string >= 5 (digit is 5 or greater) then will round
		std::string::size_type const TPos = index( DigitChar, TestChar ); // Position of Testchar in Digit string

		std::string::size_type SPos; // Actual string position being replaced
		if ( SigDigits == 0 ) {
			SPos = DotPos - 1;
		} else {
			SPos = DotPos + SigDigits;
		}

		if ( ( TPos != std::string::npos ) && ( TPos >= 5 ) ) { // Must round to next Digit
			char const Char2Rep = String[ SPos ]; // Character (digit) to be replaced
			std::string::size_type NPos = index( DigitChar, Char2Rep ); // Position of "next" char in Digit String
			std::string::size_type TPos1;
			assert( NPos != std::string::npos );
			String[ SPos ] = DigitChar[ NPos + 1 ];
			while ( NPos == 9 ) { // Must change other char too
				if ( SigDigits == 1 ) {
					assert( SPos >= 2u );
					TestChar = String[ SPos - 2 ];
					if ( TestChar == '.' ) {
						assert( SPos >= 3u );
						TestChar = String[ SPos - 3 ];
						SPos -= 2;
					}
					if ( TestChar == ' ' ) {
						TestChar = '0'; // all 999s
					} else if ( TestChar == '-' ) { //Autodesk Added to fix bug for values like -9.9999
						assert( SPos >= 3u );
						String[ SPos - 3 ] = TestChar; // Shift sign left to avoid overwriting it
						TestChar = '0'; // all 999s
					}
					TPos1 = index( DigitChar, TestChar );
					assert( TPos1 != std::string::npos );
					assert( SPos >= 2u );
					String[ SPos - 2 ] = DigitChar[ TPos1 + 1 ];
				} else {
					assert( SPos >= 1u );
					TestChar = String[ SPos - 1 ];
					if ( TestChar == '.' ) {
						assert( SPos >= 2u );
						TestChar = String[ SPos - 2 ];
						--SPos;
					}
					if ( TestChar == ' ' ) {
						TestChar = '0'; // all 999s
					} else if ( TestChar == '-' ) { //Autodesk Added to fix bug for values like -9.9999
						assert( SPos >= 2u );
						String[ SPos - 2 ] = TestChar; // Shift sign left to avoid overwriting it
						TestChar = '0'; // all 999s
					}
					TPos1 = index( DigitChar, TestChar );
					assert( TPos1 != std::string::npos );
					assert( SPos >= 1u );
					String[ SPos - 1 ] = DigitChar[ TPos1 + 1 ];
				}
				--SPos;
				NPos = TPos1;
			}
		}

		bool IncludeDot; // True when decimal point output
		if ( SigDigits > 0 || EString != "" ) {
			IncludeDot = true;
		} else {
			IncludeDot = false;
		}
		if ( IncludeDot ) {
			String.erase( min( DotPos + SigDigits + 1, len( String ) ) );
			String += EString;
		} else {
			String.erase( DotPos );
		}

		return stripped( String );

	}

	std::string
	RoundSigDigits(
		int const IntegerValue,
		Optional_int_const EP_UNUSED( SigDigits ) // ignored
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   March 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function accepts a number as parameter as well as the number of
		// significant digits after the decimal point to report and returns a string
		// that is appropriate.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		static gio::Fmt fmtLD( "*" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		std::string String; // Working string

		gio::write( String, fmtLD ) << IntegerValue;
		return stripped( String );
	}

	std::string
	RemoveTrailingZeros( std::string const & InputString )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   September 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  July 2014, Performance and refinements, Stuart Mentzer

		// PURPOSE OF THIS FUNCTION:
		// Remove trailing zeroes from output strings.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		// na

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		static std::string const ED( "ED" );
		static std::string const zero_string( "0." );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		assert( ! has_any_of( InputString, "ed" ) ); //Pre Not using lowercase exponent letter
		assert( InputString == stripped( InputString ) ); //Pre Already stripped surrounding spaces

		if ( has( InputString, '.' ) && ( ! has_any_of( InputString, ED ) ) ) { // In +/-<digits>.<digits> format
			std::string::size_type const pos( InputString.find_last_not_of( '0' ) );
			if ( pos + 1 < InputString.length() ) {
				switch ( pos ) { // Handle [+/-].000... format
				case 0u: // .0*
					return zero_string;
				case 1u:
					if ( InputString[ 1 ] == '.' ) {
						char const c0( InputString[ 0 ] );
						if ( ( c0 == '+' ) || ( c0 == '-' ) ) {
							return zero_string;
						}
					}
				default:
					return InputString.substr( 0, InputString.find_last_not_of( '0' ) + 1 );
				}
			} else { // No trailing zeros
				return InputString;
			}
		} else { // Not in +/-<digits>.<digits> format
			return InputString;
		}
	}

	std::string &
	strip_trailing_zeros( std::string & InputString )
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Stuart Mentzer (in-place version of RemoveTrailingZeros by Linda Lawrie)
		//       DATE WRITTEN   July 2014
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Remove trailing fractional zeros from floating point representation strings in place.

		static std::string const ED( "ED" );
		static std::string const zero_string( "0." );

		assert( ! has_any_of( InputString, "ed" ) ); //Pre Not using lowercase exponent letter
		assert( InputString == stripped( InputString ) ); //Pre Already stripped surrounding spaces

		if ( has( InputString, '.' ) && ( ! has_any_of( InputString, ED ) ) ) { // Has decimal point and no exponent part
			std::string::size_type const pos( InputString.find_last_not_of( '0' ) );
			if ( pos + 1 < InputString.length() ) {
				switch ( pos ) { // Handle [+/-].000... format
				case 0u: // .0*
					InputString = zero_string;
					break;
				case 1u:
					if ( InputString[ 1 ] == '.' ) {
						char const c0( InputString[ 0 ] );
						if ( ( c0 == '+' ) || ( c0 == '-' ) ) {
							InputString = zero_string;
							break;
						}
					}
				default:
					InputString.erase( pos + 1 );
				}
			}
		}
		return InputString; // Allows chaining
	}

	void
	MovingAvg(
		Array1A< Real64 > const DataIn, // input data that needs smoothing
		int const NumDataItems, // number of values in DataIn
		int const NumItemsInAvg, // number of items in the averaging window
		Array1A< Real64 > SmoothedData // output data after smoothing
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   January 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Smooth the data in the 1-d array DataIn by averaging over a window NumItemsInAvg
		// wide. Return the results in the 1-d array SmoothedData

		// METHODOLOGY EMPLOYED:
		// Note that DataIn and SmoothedData should have the same size. This is the reponsibility
		// of the calling routine. NumItemsInAvg should be no bigger than the size of DataIn.

		// REFERENCES:
		// na.

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		DataIn.dim( NumDataItems );
		SmoothedData.dim( NumDataItems );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Array1D< Real64 > TempData( 3 * NumDataItems ); // a scratch array

		for ( int i = 1; i <= NumDataItems; ++i ) {
			TempData( i ) = TempData( NumDataItems + i ) = TempData( 2 * NumDataItems + i ) = DataIn( i );
			SmoothedData( i ) = 0.0;
		}

		for ( int i = 1; i <= NumDataItems; ++i ) {
			for ( int j = 1; j <= NumItemsInAvg; ++j ) {
				SmoothedData( i ) += TempData( NumDataItems + i - NumItemsInAvg + j );
			}
			SmoothedData( i ) /= double( NumItemsInAvg );
		}
	}

	void
	ProcessDateString(
		std::string const & String,
		int & PMonth,
		int & PDay,
		int & PWeekDay,
		int & DateType, // DateType found (-1=invalid, 1=month/day, 2=nth day in month, 3=last day in month)
		bool & ErrorsFound,
		Optional_int PYear
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   December 1999
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine will process a date from a string and determine
		// the proper month and day for that date string.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::ProcessNumber;
		using namespace DataStringGlobals;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int FstNum;
		bool errFlag;
		int NumTokens;
		int TokenDay;
		int TokenMonth;
		int TokenWeekday;

		FstNum = int( ProcessNumber( String, errFlag ) );
		DateType = -1;
		if ( ! errFlag ) {
			// Entered single number, do inverse JDay
			if ( FstNum == 0 ) {
				PMonth = 0;
				PDay = 0;
				DateType = 1;
			} else if ( FstNum < 0 || FstNum > 366 ) {
				ShowSevereError( "Invalid Julian date Entered=" + String );
				ErrorsFound = true;
			} else {
				InvJulianDay( FstNum, PMonth, PDay, 0 );
				DateType = 1;
			}
		} else {
			// Error when processing as number, try x/x
			if ( ! present( PYear ) ) {
				DetermineDateTokens( String, NumTokens, TokenDay, TokenMonth, TokenWeekday, DateType, ErrorsFound );
			} else {
				int TokenYear = 0;
				DetermineDateTokens( String, NumTokens, TokenDay, TokenMonth, TokenWeekday, DateType, ErrorsFound, TokenYear );
				PYear = TokenYear;
			}
			if ( DateType == 1 ) {
				PDay = TokenDay;
				PMonth = TokenMonth;
			} else if ( DateType == 2 || DateType == 3 ) {
				// interpret as TokenDay TokenWeekday in TokenMonth
				PDay = TokenDay;
				PMonth = TokenMonth;
				PWeekDay = TokenWeekday;
			}
		}

	}

	void
	DetermineDateTokens(
		std::string const & String,
		int & NumTokens, // Number of tokens found in string
		int & TokenDay, // Value of numeric field found
		int & TokenMonth, // Value of Month field found (1=Jan, 2=Feb, etc)
		int & TokenWeekday, // Value of Weekday field found (1=Sunday, 2=Monday, etc), 0 if none
		int & DateType, // DateType found (-1=invalid, 1=month/day, 2=nth day in month, 3=last day in month)
		bool & ErrorsFound, // Set to true if cannot process this string as a date
		Optional_int TokenYear // Value of Year if one appears to be present and this argument is present
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   August 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is invoked for date fields that appear to be strings (give
		// error when ProcessNumber is used).

		// METHODOLOGY EMPLOYED:
		// Delete everything that is extraneous to the date information needed.  Process what
		// is left.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using InputProcessor::ProcessNumber;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static int const NumSingleChars( 3 );
		static Array1D_string const SingleChars( NumSingleChars, { "/", ":", "-" } );
		static int const NumDoubleChars( 6 );
		static Array1D_string const DoubleChars( NumDoubleChars, { "ST ", "ND ", "RD ", "TH ", "OF ", "IN " } ); // Need trailing spaces: Want thse only at end of words
		static Array1D_string const Months( 12, { "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC" } );
		static Array1D_string const Weekdays( 7, { "SUN", "MON", "TUE", "WED", "THU", "FRI", "SAT" } );
		static std::string const Numbers( "0123456789" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string CurrentString;
		std::string::size_type Pos;
		int Loop;
		Array1D_string Fields( 3 );
		int NumField1;
		int NumField2;
		int NumField3;
		bool errFlag;
		bool InternalError;
		bool WkDayInMonth;

		CurrentString = String;
		NumTokens = 0;
		TokenDay = 0;
		TokenMonth = 0;
		TokenWeekday = 0;
		DateType = -1;
		InternalError = false;
		WkDayInMonth = false;
		if ( present( TokenYear ) ) TokenYear = 0;
		// Take out separator characters, other extraneous stuff

		for ( Loop = 1; Loop <= NumSingleChars; ++Loop ) {
			Pos = index( CurrentString, SingleChars( Loop ) );
			while ( Pos != std::string::npos ) {
				CurrentString[ Pos ] = ' ';
				Pos = index( CurrentString, SingleChars( Loop ) );
			}
		}

		for ( Loop = 1; Loop <= NumDoubleChars; ++Loop ) {
			Pos = index( CurrentString, DoubleChars( Loop ) );
			while ( Pos != std::string::npos ) {
				CurrentString.replace( Pos, 2, "  " );
				Pos = index( CurrentString, DoubleChars( Loop ) );
				WkDayInMonth = true;
			}
		}

		strip( CurrentString );
		if ( CurrentString == BlankString ) {
			ShowSevereError( "Invalid date field=" + String );
			ErrorsFound = true;
		} else {
			Loop = 0;
			while ( Loop < 3 ) { // Max of 3 fields
				if ( CurrentString == BlankString ) break;
				Pos = index( CurrentString, ' ' );
				++Loop;
				if ( Pos == std::string::npos ) Pos = CurrentString.length();
				Fields( Loop ) = CurrentString.substr( 0, Pos );
				CurrentString.erase( 0, Pos );
				strip( CurrentString );
			}
			if ( not_blank( CurrentString ) ) {
				ShowSevereError( "Invalid date field=" + String );
				ErrorsFound = true;
			} else if ( Loop == 2 ) {
				// Field must be Day Month or Month Day (if both numeric, mon / day)
				InternalError = false;
				NumField1 = int( ProcessNumber( Fields( 1 ), errFlag ) );
				if ( errFlag ) {
					// Month day, but first field is not numeric, 2nd must be
					NumField2 = int( ProcessNumber( Fields( 2 ), errFlag ) );
					if ( errFlag ) {
						ShowSevereError( "Invalid date field=" + String );
						InternalError = true;
					} else {
						TokenDay = NumField2;
					}
					TokenMonth = FindItemInList( Fields( 1 ).substr( 0, 3 ), Months, 12 );
					ValidateMonthDay( String, TokenDay, TokenMonth, InternalError );
					if ( ! InternalError ) {
						DateType = 1;
					} else {
						ErrorsFound = true;
					}
				} else {
					// Month Day, first field was numeric, if 2nd is, then it's month<num> day<num>
					NumField2 = int( ProcessNumber( Fields( 2 ), errFlag ) );
					if ( ! errFlag ) {
						TokenMonth = NumField1;
						TokenDay = NumField2;
						ValidateMonthDay( String, TokenDay, TokenMonth, InternalError );
						if ( ! InternalError ) {
							DateType = 1;
						} else {
							ErrorsFound = true;
						}
					} else { // 2nd field was not numeric.  Must be Month
						TokenDay = NumField1;
						TokenMonth = FindItemInList( Fields( 2 ).substr( 0, 3 ), Months, 12 );
						ValidateMonthDay( String, TokenDay, TokenMonth, InternalError );
						if ( ! InternalError ) {
							DateType = 1;
							NumTokens = 2;
						} else {
							ErrorsFound = true;
						}
					}
				}
			} else if ( Loop == 3 ) {
				// Field must be some combination of <num> Weekday Month (if WkDayInMonth true)
				if ( WkDayInMonth ) {
					NumField1 = int( ProcessNumber( Fields( 1 ), errFlag ) );
					if ( ! errFlag ) { // the expected result
						TokenDay = NumField1;
						TokenWeekday = FindItemInList( Fields( 2 ).substr( 0, 3 ), Weekdays, 7 );
						if ( TokenWeekday == 0 ) {
							TokenMonth = FindItemInList( Fields( 2 ).substr( 0, 3 ), Months, 12 );
							TokenWeekday = FindItemInList( Fields( 3 ).substr( 0, 3 ), Weekdays, 7 );
							if ( TokenMonth == 0 || TokenWeekday == 0 ) InternalError = true;
						} else {
							TokenMonth = FindItemInList( Fields( 3 ).substr( 0, 3 ), Months, 12 );
							if ( TokenMonth == 0 ) InternalError = true;
						}
						DateType = 2;
						NumTokens = 3;
						if ( TokenDay < 0 || TokenDay > 5 ) InternalError = true;
					} else { // first field was not numeric....
						if ( Fields( 1 ) == "LA" ) {
							DateType = 3;
							NumTokens = 3;
							TokenWeekday = FindItemInList( Fields( 2 ).substr( 0, 3 ), Weekdays, 7 );
							if ( TokenWeekday == 0 ) {
								TokenMonth = FindItemInList( Fields( 2 ).substr( 0, 3 ), Months, 12 );
								TokenWeekday = FindItemInList( Fields( 3 ).substr( 0, 3 ), Weekdays, 7 );
								if ( TokenMonth == 0 || TokenWeekday == 0 ) InternalError = true;
							} else {
								TokenMonth = FindItemInList( Fields( 3 ).substr( 0, 3 ), Months, 12 );
								if ( TokenMonth == 0 ) InternalError = true;
							}
						} else { // error....
							ShowSevereError( "First date field not numeric, field=" + String );
						}
					}
				} else { // mm/dd/yyyy or yyyy/mm/dd
					NumField1 = int( ProcessNumber( Fields( 1 ), errFlag ) );
					NumField2 = int( ProcessNumber( Fields( 2 ), errFlag ) );
					NumField3 = int( ProcessNumber( Fields( 3 ), errFlag ) );
					DateType = 1;
					// error detection later..
					if ( NumField1 > 100 ) {
						if ( present( TokenYear ) ) {
							TokenYear = NumField1;
						}
						TokenMonth = NumField2;
						TokenDay = NumField3;
					} else if ( NumField3 > 100 ) {
						if ( present( TokenYear ) ) {
							TokenYear = NumField3;
						}
						TokenMonth = NumField1;
						TokenDay = NumField2;
					}
				}
			} else {
				// Not enough or too many fields
				ShowSevereError( "Invalid date field=" + String );
				ErrorsFound = true;
			}
		}

		if ( InternalError ) {
			DateType = -1;
			ErrorsFound = true;
		}

	}

	void
	ValidateMonthDay(
		std::string const & String, // REAL(r64) string being processed
		int const Day,
		int const Month,
		bool & ErrorsFound
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   August 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine validates a potential Day, Month values, produces an error
		// message when not valid, and sets error flag.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static Array1D_int const EndMonthDay( 12, { 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 } );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool InternalError;

		InternalError = false;
		if ( Month < 1 || Month > 12 ) InternalError = true;
		if ( ! InternalError ) {
			if ( Day < 1 || Day > EndMonthDay( Month ) ) InternalError = true;
		}
		if ( InternalError ) {
			ShowSevereError( "Invalid Month Day date format=" + String );
			ErrorsFound = true;
		} else {
			ErrorsFound = false;
		}

	}

	int
	JulianDay(
		int const Month, // Month, 1..12
		int const Day, // Day of Month, not validated by month
		int const LeapYearValue // 1 if leap year indicated, 0 if not
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  from JDAYF in BLAST/IBLAST

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine returns the appropriate Julian Day value for the input
		// Month and Day.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int JulianDay;

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
		static Array1D_int EndDayofMonth( 12, { 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 } );
		// End day numbers of each month (without Leap Year)

		if ( Month == 1 ) {
			//                                       CASE 1: JANUARY
			JulianDay = Day;

		} else if ( Month == 2 ) {
			//                                       CASE 2: FEBRUARY
			JulianDay = Day + EndDayofMonth( 1 );

		} else if ( ( Month >= 3 ) && ( Month <= 12 ) ) {
			//                                       CASE 3: REMAINING MONTHS
			JulianDay = Day + EndDayofMonth( Month - 1 ) + LeapYearValue;

		} else {
			JulianDay = 0;

		}

		return JulianDay;

	}

	void
	InvJulianDay(
		int const Number,
		int & PMonth,
		int & PDay,
		int const LeapYr
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   December 1999
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine performs and inverse Julian Day
		// calculation, using an input JulianDay and returning
		// appropriate Month and Day.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static Array1D_int const EndOfMonth( {0,12}, { 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 } );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int WMonth;
		int LeapAddPrev;
		int LeapAddCur;

		if ( Number < 0 || Number > 366 ) return;
		for ( WMonth = 1; WMonth <= 12; ++WMonth ) {
			if ( WMonth == 1 ) {
				LeapAddPrev = 0;
				LeapAddCur = 0;
			} else if ( WMonth == 2 ) {
				LeapAddPrev = 0;
				LeapAddCur = LeapYr;
			} else {
				LeapAddPrev = LeapYr;
				LeapAddCur = LeapYr;
			}
			if ( Number > ( EndOfMonth( WMonth - 1 ) + LeapAddPrev ) && Number <= ( EndOfMonth( WMonth ) + LeapAddCur ) ) break;
		}
		PMonth = WMonth;
		PDay = Number - ( EndOfMonth( WMonth - 1 ) + LeapAddCur );

	}

	bool
	BetweenDates(
		int const TestDate, // Date to test
		int const StartDate, // Start date in sequence
		int const EndDate // End date in sequence
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   June 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function returns true if the TestDate is between
		// (StartDate <= TestDate <= EndDate).

		// METHODOLOGY EMPLOYED:
		// The input dates are Julian Day format, year is irrelevant.
		// Thus, if StartDate > EndDate (i.e. StartDate = 1Dec and EndDate = 31Jan),
		// this routine accomodates.

		// REFERENCES:
		// Adapted from BLAST BTWEEN function.

		// USE STATEMENTS:
		// na

		// Return value
		bool BetweenDates;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		BetweenDates = false; // Default case

		if ( StartDate <= EndDate ) { // Start Date <= End Date
			if ( TestDate >= StartDate && TestDate <= EndDate ) BetweenDates = true;
		} else { // EndDate <= StartDate
			if ( TestDate <= EndDate || TestDate >= StartDate ) BetweenDates = true;
		}

		return BetweenDates;

	}

	std::string
	CreateSysTimeIntervalString()
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   April 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function creates the current time interval of the system
		// time step.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::CurrentTime;
		using DataGlobals::TimeStepZone;
		using DataHVACGlobals::TimeStepSys;
		using DataHVACGlobals::SysTimeElapsed;

		// Return value
		std::string OutputString;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		static gio::Fmt TStmpFmt( "(I2.2,':',F3.0)" );
		static gio::Fmt TStmpFmti( "(I2.2,':',I2.2)" );
		Real64 const FracToMin( 60.0 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 ActualTimeS; // Start of current interval (HVAC time step)
		Real64 ActualTimeE; // End of current interval (HVAC time step)
		int ActualTimeHrS;
		//  INTEGER ActualTimeHrE
		std::string TimeStmpS; // Character representation of start of interval
		std::string TimeStmpE; // Character representation of end of interval
		int ActualTimeMinS;

		//  ActualTimeS=INT(CurrentTime)+(SysTimeElapsed+(CurrentTime - INT(CurrentTime)))
		// CR6902  ActualTimeS=INT(CurrentTime-TimeStepZone)+SysTimeElapsed
		// [DC] TODO: Improve display accuracy up to fractional seconds using hh:mm:ss.0 format
		ActualTimeS = CurrentTime - TimeStepZone + SysTimeElapsed;
		ActualTimeE = ActualTimeS + TimeStepSys;
		ActualTimeHrS = int( ActualTimeS );
		//  ActualTimeHrE=INT(ActualTimeE)
		ActualTimeMinS = nint( ( ActualTimeS - ActualTimeHrS ) * FracToMin );

		if ( ActualTimeMinS == 60 ) {
			++ActualTimeHrS;
			ActualTimeMinS = 0;
		}
		gio::write( TimeStmpS, TStmpFmti ) << ActualTimeHrS << ActualTimeMinS;

		gio::write( TimeStmpE, TStmpFmt ) << int( ActualTimeE ) << ( ActualTimeE - int( ActualTimeE ) ) * FracToMin;
		if ( TimeStmpE[ 3 ] == ' ' ) TimeStmpE[ 3 ] = '0';
		TimeStmpE[ 5 ] = ' ';
		strip( TimeStmpE );

		OutputString = TimeStmpS + " - " + TimeStmpE;

		return OutputString;

	}

	Real64
	SafeDivide(
		Real64 const a,
		Real64 const b
	)
	{

		// returns a / b while preventing division by zero

		// Return value
		Real64 c;

		// Locals
		Real64 const SMALL( 1.E-10 );

		if ( std::abs( b ) >= SMALL ) {
			c = a / b;
		} else {
			c = a / sign( SMALL, b );
		}
		return c;
	}

	//SUBROUTINE SaveCompDesWaterFlow(WaterInletNodeNum,DesWaterFlow)

	//          ! SUBROUTINE INFORMATION:
	//          !       AUTHOR         Fred Buhl
	//          !       DATE WRITTEN   January 2004
	//          !       MODIFIED       na
	//          !       RE-ENGINEERED  na

	//          ! PURPOSE OF THIS SUBROUTINE:
	//          ! Save the design water flow rates of those components using water as an energy source
	//          ! or sink in an array that can be accessed by the water loop managers for sizing calculations.

	//          ! METHODOLOGY EMPLOYED:
	//          ! The design flow rate is stored in a dynamic array along with the water inlet node number
	//          ! (which is used by the water loops as a component identifier instead if name and type).

	//          ! REFERENCES:
	//          ! na

	//          ! USE STATEMENTS:
	//  USE DataSizing

	//  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

	//          ! SUBROUTINE ARGUMENT DEFINITIONS:
	//  INTEGER :: WaterInletNodeNum ! the component's water inlet node number (condenser side for water / water compoennts)
	//  REAL(r64)    :: DesWaterFlow      ! the component's design water flow rate [m3/s]

	//          ! SUBROUTINE PARAMETER DEFINITIONS:
	//          ! na

	//          ! INTERFACE BLOCK SPECIFICATIONS:
	//          ! na

	//          ! DERIVED TYPE DEFINITIONS:
	//          ! na

	//          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	//  TYPE (CompDesWaterFlowData), ALLOCATABLE, DIMENSION(:) :: CompDesWaterFlow0 ! scratch array to store components'
	//                                                                            ! design water flow rate
	//  INTEGER :: WaterCompNum ! component do loop index

	//  NumWaterComps = NumWaterComps + 1 ! increment the number of components that use water as a source of heat or coolth
	//  ! save the existing data in a scratch array
	//  IF (NumWaterComps > 1) THEN
	//    ALLOCATE(CompDesWaterFlow0(NumWaterComps-1))
	//    DO WaterCompNum=1,NumWaterComps-1
	//      CompDesWaterFlow0(WaterCompNum)%SupNode = CompDesWaterFlow(WaterCompNum)%SupNode
	//      CompDesWaterFlow0(WaterCompNum)%DesVolFlowRate = CompDesWaterFlow(WaterCompNum)%DesVolFlowRate
	//    END DO
	//    ! get rid of the old array
	//    DEALLOCATE(CompDesWaterFlow)
	//  END IF
	//  ! allocate a new array
	//  ALLOCATE(CompDesWaterFlow(NumWaterComps))
	//  ! save the new data
	//  CompDesWaterFlow(NumWaterComps)%SupNode = WaterInletNodeNum
	//  CompDesWaterFlow(NumWaterComps)%DesVolFlowRate = DesWaterFlow
	//  ! move the old data back from the scratch array
	//  IF (NumWaterComps > 1) THEN
	//    DO WaterCompNum=1,NumWaterComps-1
	//      CompDesWaterFlow(WaterCompNum)%SupNode = CompDesWaterFlow0(WaterCompNum)%SupNode
	//      CompDesWaterFlow(WaterCompNum)%DesVolFlowRate = CompDesWaterFlow0(WaterCompNum)%DesVolFlowRate
	//    END DO
	//    DEALLOCATE(CompDesWaterFlow0)
	//  END IF

	//  RETURN

	//END SUBROUTINE SaveCompDesWaterFlow

	void
	Invert3By3Matrix(
		Array2A< Real64 > const A, // Input 3X3 Matrix
		Array2A< Real64 > InverseA // Output 3X3 Matrix - Inverse Of A
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         George Walton
		//       DATE WRITTEN   August 1976
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine computes the inverse of a 3x3 matrix by the
		// cofactor method.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::OutputFileStandard;

		// Argument array dimensioning
		A.dim( 3, 3 );
		InverseA.dim( 3, 3 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 Determinant; // Determinant of Matrix A

		// Compute Determinant

		Determinant = A( 1, 1 ) * A( 2, 2 ) * A( 3, 3 ) + A( 2, 1 ) * A( 3, 2 ) * A( 1, 3 ) + A( 3, 1 ) * A( 1, 2 ) * A( 2, 3 ) - A( 1, 1 ) * A( 2, 3 ) * A( 3, 2 ) - A( 1, 2 ) * A( 2, 1 ) * A( 3, 3 ) - A( 1, 3 ) * A( 2, 2 ) * A( 3, 1 );

		if ( std::abs( Determinant ) < .1E-12 ) {
			ShowFatalError( "Determinant = [Zero] in Invert3By3Matrix", OutputFileStandard );
		}

		// Compute Inverse

		InverseA( 1, 1 ) = ( A( 2, 2 ) * A( 3, 3 ) - A( 2, 3 ) * A( 3, 2 ) ) / Determinant;
		InverseA( 1, 2 ) = ( A( 1, 3 ) * A( 3, 2 ) - A( 1, 2 ) * A( 3, 3 ) ) / Determinant;
		InverseA( 1, 3 ) = ( A( 1, 2 ) * A( 2, 3 ) - A( 1, 3 ) * A( 2, 2 ) ) / Determinant;
		InverseA( 2, 1 ) = ( A( 2, 3 ) * A( 3, 1 ) - A( 2, 1 ) * A( 3, 3 ) ) / Determinant;
		InverseA( 2, 2 ) = ( A( 1, 1 ) * A( 3, 3 ) - A( 1, 3 ) * A( 3, 1 ) ) / Determinant;
		InverseA( 2, 3 ) = ( A( 1, 3 ) * A( 2, 1 ) - A( 1, 1 ) * A( 2, 3 ) ) / Determinant;
		InverseA( 3, 1 ) = ( A( 2, 1 ) * A( 3, 2 ) - A( 2, 2 ) * A( 3, 1 ) ) / Determinant;
		InverseA( 3, 2 ) = ( A( 1, 2 ) * A( 3, 1 ) - A( 1, 1 ) * A( 3, 2 ) ) / Determinant;
		InverseA( 3, 3 ) = ( A( 1, 1 ) * A( 2, 2 ) - A( 1, 2 ) * A( 2, 1 ) ) / Determinant;

	}

	void
	Iterate(
		Real64 & ResultX, // ResultX is the final Iteration result passed back to the calling routine
		Real64 const Tol, // Tolerance for Convergence
		Real64 const X0, // Current value of X
		Real64 const Y0, // Current value of the function Y(X)
		Real64 & X1, // First Previous values of X
		Real64 & Y1, // First Previous values of Y(X1)
		int const Iter, // Number of iterations
		int & Cnvg // Convergence flag  Cnvg = 0:  Not converged
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   March 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Iterately solves for the value of X which satisfies Y(X)=0.
		// The subroutine tests for convergence and provides a new guess for the value of the
		// independent variable X.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// Linear Correction based on the RegulaFalsi routine in EnergyPlus

		// Using/Aliasing
		using namespace DataPrecisionGlobals;
		//unused0909  use dataglobals, only: outputfiledebug

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		//                  Cnvg = 1:  Converged

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const small( 1.e-9 ); // Small Number used to approximate zero
		Real64 const Perturb( 0.1 ); // Perturbation applied to X to initialize iteration

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Real64 DY; // Linear fit result

		// FLOW:

		// Check for convergence by comparing change in X
		if ( Iter != 1 ) {
			if ( std::abs( X0 - X1 ) < Tol || Y0 == 0.0 ) {
				ResultX = X0;
				Cnvg = 1;
				return;
			}
		}

		// Not converged.
		Cnvg = 0;
		if ( Iter == 1 ) {

			// New guess is specified by Perturb
			if ( std::abs( X0 ) > small ) {
				ResultX = X0 * ( 1.0 + Perturb );
			} else {
				ResultX = Perturb;
			}

		} else {

			// New guess calculated from LINEAR FIT of most recent two points
			DY = Y0 - Y1;
			if ( std::abs( DY ) < small ) DY = small;
			// new estimation

			ResultX = ( Y0 * X1 - Y1 * X0 ) / DY;

		}

		X1 = X0;
		Y1 = Y0;

	}

	int
	FindNumberInList(
		int const WhichNumber,
		Array1A_int const ListOfItems,
		int const NumItems
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up a number(integer) in a similar list of
		// items and returns the index of the item in the list, if
		// found.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int FindNumberInList;

		// Argument array dimensioning
		ListOfItems.dim( _ );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Count;

		FindNumberInList = 0;

		for ( Count = 1; Count <= NumItems; ++Count ) {
			if ( WhichNumber == ListOfItems( Count ) ) {
				FindNumberInList = Count;
				break;
			}
		}

		return FindNumberInList;

	}

	void
	DecodeMonDayHrMin(
		int const Item, // word containing encoded month, day, hour, minute
		int & Month, // month in integer format (1-12)
		int & Day, // day in integer format (1-31)
		int & Hour, // hour in integer format (1-24)
		int & Minute // minute in integer format (0:59)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   March 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine decodes the "packed" integer representation of
		// the Month, Day, Hour, and Minute.  Packed integers are used to
		// save memory allocation.  Original idea for this routine is contained
		// in DECMDH, BLAST code, by Jean Baugh.

		// METHODOLOGY EMPLOYED:
		// Using maximum integer concept the original date can be decoded
		// from the packed single word.  This relies on 4 byte integer representation
		// as a minimum (capable of representing up to 2,147,483,647).

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// ((month*100 + day)*100 + hour)*100 + minute

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const DecMon( 100 * 100 * 100 );
		int const DecDay( 100 * 100 );
		int const DecHr( 100 );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int TmpItem;

		TmpItem = Item;
		Month = TmpItem / DecMon;
		TmpItem = ( TmpItem - Month * DecMon );
		Day = TmpItem / DecDay;
		TmpItem -= Day * DecDay;
		Hour = TmpItem / DecHr;
		Minute = mod( TmpItem, DecHr );

	}

	int
	DetermineMinuteForReporting( int const IndexTypeKey ) // kind of reporting, Zone Timestep or System
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   January 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// When reporting peaks, minutes are used but not necessarily easily calculated.

		// METHODOLOGY EMPLOYED:
		// Could use the access to the minute as OP (OutputProcessor) does but uses
		// external calculation.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataPrecisionGlobals;
		using DataGlobals::HVACTSReporting;
		using DataGlobals::TimeStepZone;
		using DataGlobals::CurrentTime;
		using DataHVACGlobals::TimeStepSys;
		using DataHVACGlobals::SysTimeElapsed;

		// Return value
		int ActualTimeMin; // calculated Minute for reporting

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		Real64 const FracToMin( 60.0 );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 ActualTimeS; // Start of current interval (HVAC time step)
		Real64 ActualTimeE; // End of current interval (HVAC time step)
		int ActualTimeHrS;

		if ( IndexTypeKey == HVACTSReporting ) {
			ActualTimeS = CurrentTime - TimeStepZone + SysTimeElapsed;
			ActualTimeE = ActualTimeS + TimeStepSys;
			ActualTimeHrS = int( ActualTimeS );
			ActualTimeMin = nint( ( ActualTimeE - ActualTimeHrS ) * FracToMin );
		} else {
			ActualTimeMin = ( CurrentTime - int( CurrentTime ) ) * FracToMin;
		}

		return ActualTimeMin;

	}

	void
	EncodeMonDayHrMin(
		int & Item, // word containing encoded month, day, hour, minute
		int const Month, // month in integer format (1:12)
		int const Day, // day in integer format (1:31)
		int const Hour, // hour in integer format (1:24)
		int const Minute // minute in integer format (0:59)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   March 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine encodes the "packed" integer representation of
		// the Month, Day, Hour, and Minute.  Packed integers are used to
		// save memory allocation.  Original idea for this routine is contained
		// in DECMDH, BLAST code, by Jean Baugh.

		// METHODOLOGY EMPLOYED:
		// Using maximum integer concept the original date can be decoded
		// from the packed single word.  This relies on 4 byte integer representation
		// as a minimum (capable of representing up to 2,147,483,647).

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// ((month*100 + day)*100 + hour)*100 + minute

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		Item = ( ( Month * 100 + Day ) * 100 + Hour ) * 100 + Minute;

	}

	int
	LogicalToInteger( bool const Flag )
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dimitri Curtil
		//       DATE WRITTEN   November 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This subroutine uses an input logical and makes
		// an integer (true=1, false=0)

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int LogicalToInteger;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		if ( Flag ) {
			LogicalToInteger = 1;
		} else {
			LogicalToInteger = 0;
		}

		return LogicalToInteger;

	}

	Real64
	GetCurrentHVACTime()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dimitri Curtil
		//       DATE WRITTEN   November 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This routine returns the time in seconds at the end of the current HVAC step.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::CurrentTime;
		using DataGlobals::TimeStepZone;
		using DataGlobals::SecInHour;
		using DataHVACGlobals::SysTimeElapsed;
		using DataHVACGlobals::TimeStepSys;

		// Return value
		Real64 GetCurrentHVACTime;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:
		// na

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 CurrentHVACTime;

		// This is the correct formula that does not use MinutesPerSystemTimeStep, which would
		// erronously truncate all sub-minute system time steps down to the closest full minute.
		// Maybe later TimeStepZone, TimeStepSys and SysTimeElapsed could also be specified
		// as real.
		CurrentHVACTime = ( CurrentTime - TimeStepZone ) + SysTimeElapsed + TimeStepSys;
		GetCurrentHVACTime = CurrentHVACTime * SecInHour;

		return GetCurrentHVACTime;

	}

	Real64
	GetPreviousHVACTime()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dimitri Curtil
		//       DATE WRITTEN   November 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This routine returns the time in seconds at the beginning of the current HVAC step.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::CurrentTime;
		using DataGlobals::TimeStepZone;
		using DataGlobals::SecInHour;
		using DataHVACGlobals::SysTimeElapsed;

		// Return value
		Real64 GetPreviousHVACTime;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:
		// na

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 PreviousHVACTime;

		// This is the correct formula that does not use MinutesPerSystemTimeStep, which would
		// erronously truncate all sub-minute system time steps down to the closest full minute.
		PreviousHVACTime = ( CurrentTime - TimeStepZone ) + SysTimeElapsed;
		GetPreviousHVACTime = PreviousHVACTime * SecInHour;

		return GetPreviousHVACTime;

	}

	std::string
	CreateHVACTimeIntervalString()
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Dimitri Curtil
		//       DATE WRITTEN   January 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function creates the time stamp with the current time interval for the HVAC
		// time step.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Return value
		std::string OutputString;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		OutputString = CreateTimeIntervalString( GetPreviousHVACTime(), GetCurrentHVACTime() );

		return OutputString;

	}

	std::string
	CreateTimeString( Real64 const Time ) // Time in seconds
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Dimitri Curtil
		//       DATE WRITTEN   January 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function creates the time stamp string from the time value specified in seconds.
		// Inspired by similar function CreateSysTimeIntervalString() in General.cc
		// However, this function provides better accuracy for sub-minute time steps
		// by also showing information down to the 10th of a second.
		// Note that Time is expected to be specified in REAL(r64).

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int Hours; // Number of hours <= 24
		int Minutes; // Remaining minutes < 60
		Real64 Seconds; // Remaining seconds < 60

		ParseTime( Time, Hours, Minutes, Seconds );

		// TimeStamp written with formatting
		// "hh:mm:ss.s"
		// 10 chars + null terminator = 11
		// This approach should not normally be used due to the fixed width c-style
		// string but in this case the output string is a fixed size so this is more
		// clear for formatting and faster. If formatted string changes, make sure to
		// add more to buffer.
		static char buffer[ 11 ];
		int cx = snprintf( buffer, 11, "%02d:%02d:%04.1f", Hours, Minutes, Seconds );

		// Make sure output string is only between 0 and 10 characters so string is
		// not out of bounds of the buffer.
		assert( cx >= 0 && cx < 11 );
		// Only done to quiet release compiler warning for unused variable.
		(void) cx;

		return std::string( buffer );
	}

	std::string
	CreateTimeIntervalString(
		Real64 const StartTime, // Start of current interval in seconds
		Real64 const EndTime // End of current interval in seconds
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Dimitri Curtil
		//       DATE WRITTEN   January 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function creates the time stamp with the current time interval from start and end
		// time values specified in seconds.
		// Inspired by similar function CreateSysTimeIntervalString() in General.cc

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		std::string TimeStmpS; // Character representation of start of interval
		std::string TimeStmpE; // Character representation of end of interval

		TimeStmpS = CreateTimeString( StartTime );
		TimeStmpE = CreateTimeString( EndTime );

		return TimeStmpS + " - " + TimeStmpE;

	}

	void
	ParseTime(
		Real64 const Time, // Time value in seconds
		int & Hours, // Number of hours
		int & Minutes, // Number of minutes < 60
		Real64 & Seconds // Number of seconds < 60
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Dimitri Curtil
		//       DATE WRITTEN   January 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This subroutine decomposes a time value specified in seconds
		// into a triplet { hours : minutes : seconds } such that
		// - minutes < 60
		// - seconds < 60

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int const MinToSec( 60 );
		int const HourToSec( MinToSec * 60 );
		Real64 Remainder( 0.0 );

		// Get number of hours
		// This might undershoot the actual number of hours. See DO WHILE loop.
		Hours = int( Time ) / HourToSec;

		// Compute remainder in seconds
		Remainder = ( Time - Hours * HourToSec );

		// Compute minutes
		Minutes = int( Remainder ) / MinToSec;

		// Compute remainder in seconds
		Remainder -= Minutes * MinToSec;

		// Compute seconds
		Seconds = Remainder;

	}

	void
	ScanForReports(
		std::string const & reportName,
		bool & DoReport,
		Optional_string_const ReportKey,
		Optional_string Option1,
		Optional_string Option2
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   March 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine scans for the global "reports" settings, such as Variable Dictionary,
		// Surfaces (and options), Constructions, etc.

		// METHODOLOGY EMPLOYED:
		// First time routine is called, all the viable combinations/settings for the reports are
		// stored in SAVEd variables.  Later callings will retrieve those.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::MakeUPPERCase;
		using InputProcessor::FindItemInList;
		using InputProcessor::GetNumSectionsFound;
		using InputProcessor::SameString;
		using DataRuntimeLanguage::OutputFullEMSTrace;
		using DataRuntimeLanguage::OutputEMSErrors;
		using DataRuntimeLanguage::OutputEMSActuatorAvailFull;
		using DataRuntimeLanguage::OutputEMSActuatorAvailSmall;
		using DataRuntimeLanguage::OutputEMSInternalVarsFull;
		using DataRuntimeLanguage::OutputEMSInternalVarsSmall;
		using DataGlobals::ShowDecayCurvesInEIO;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumReports;
		int RepNum;
		int NumNames;
		int NumNumbers;
		int IOStat;
		static bool SurfVert( false );
		static bool SurfDet( false );
		static bool SurfDetWVert( false );
		static bool DXFReport( false );
		static std::string DXFOption1;
		static std::string DXFOption2;
		static bool DXFWFReport( false );
		static std::string DXFWFOption1;
		static std::string DXFWFOption2;
		static bool VRMLReport( false );
		static std::string VRMLOption1;
		static std::string VRMLOption2;
		static bool CostInfo( false );
		static bool ViewFactorInfo( false );
		static std::string ViewRptOption1;
		static bool Constructions( false );
		static bool Materials( false );
		static bool LineRpt( false );
		static std::string LineRptOption1;
		static bool VarDict( false );
		static bool EMSoutput( false );
		static std::string VarDictOption1;
		static std::string VarDictOption2;
		//  LOGICAL,SAVE :: SchRpt = .FALSE.
		//  CHARACTER(len=MaxNameLength) :: SchRptOption
		static bool GetReportInput( true );

		if ( GetReportInput ) {

			cCurrentModuleObject = "Output:Surfaces:List";

			NumReports = GetNumObjectsFound( cCurrentModuleObject );
			for ( RepNum = 1; RepNum <= NumReports; ++RepNum ) {
				GetObjectItem( cCurrentModuleObject, RepNum, cAlphaArgs, NumNames, rNumericArgs, NumNumbers, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				{ auto const SELECT_CASE_var( cAlphaArgs( 1 ) );

				if ( SELECT_CASE_var == "LINES" ) {
					LineRpt = true;
					LineRptOption1 = cAlphaArgs( 2 );

				} else if ( SELECT_CASE_var == "VERTICES" ) {
					SurfVert = true;

				} else if ( ( SELECT_CASE_var == "DETAILS" ) || ( SELECT_CASE_var == "DETAILED" ) || ( SELECT_CASE_var == "DETAIL" ) ) {
					SurfDet = true;

				} else if ( ( SELECT_CASE_var == "DETAILSWITHVERTICES" ) || ( SELECT_CASE_var == "DETAILVERTICES" ) ) {
					SurfDetWVert = true;

				} else if ( SELECT_CASE_var == "COSTINFO" ) {
					//   Custom case for reporting surface info for cost estimates (for first costs in opitimzing)
					CostInfo = true;

				} else if ( SELECT_CASE_var == "VIEWFACTORINFO" ) { // actual reporting is in HeatBalanceIntRadExchange
					ViewFactorInfo = true;
					ViewRptOption1 = cAlphaArgs( 2 );

				} else if ( SELECT_CASE_var == "DECAYCURVESFROMZONECOMPONENTLOADS" ) { //Should the Radiant to Convective Decay Curves from the load component report appear in the EIO file
					ShowDecayCurvesInEIO = true;

				} else if ( SELECT_CASE_var == "" ) {
					ShowWarningError( cCurrentModuleObject + ": No " + cAlphaFieldNames( 1 ) + " supplied." );
					ShowContinueError( " Legal values are: \"Lines\", \"Vertices\", \"Details\", \"DetailsWithVertices\", \"CostInfo\", \"ViewFactorIinfo\"." );

				} else {
					ShowWarningError( cCurrentModuleObject + ": Invalid " + cAlphaFieldNames( 1 ) + "=\"" + cAlphaArgs( 1 ) + "\" supplied." );
					ShowContinueError( " Legal values are: \"Lines\", \"Vertices\", \"Details\", \"DetailsWithVertices\", \"CostInfo\", \"ViewFactorIinfo\"." );

				}}
			}

			cCurrentModuleObject = "Output:Surfaces:Drawing";

			NumReports = GetNumObjectsFound( cCurrentModuleObject );
			for ( RepNum = 1; RepNum <= NumReports; ++RepNum ) {
				GetObjectItem( cCurrentModuleObject, RepNum, cAlphaArgs, NumNames, rNumericArgs, NumNumbers, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				{ auto const SELECT_CASE_var( cAlphaArgs( 1 ) );

				if ( SELECT_CASE_var == "DXF" ) {
					DXFReport = true;
					DXFOption1 = cAlphaArgs( 2 );
					DXFOption2 = cAlphaArgs( 3 );

				} else if ( SELECT_CASE_var == "DXF:WIREFRAME" ) {
					DXFWFReport = true;
					DXFWFOption1 = cAlphaArgs( 2 );
					DXFWFOption2 = cAlphaArgs( 3 );

				} else if ( SELECT_CASE_var == "VRML" ) {
					VRMLReport = true;
					VRMLOption1 = cAlphaArgs( 2 );
					VRMLOption2 = cAlphaArgs( 3 );

				} else if ( SELECT_CASE_var == "" ) {
					ShowWarningError( cCurrentModuleObject + ": No " + cAlphaFieldNames( 1 ) + " supplied." );
					ShowContinueError( " Legal values are: \"DXF\", \"DXF:WireFrame\", \"VRML\"." );

				} else {
					ShowWarningError( cCurrentModuleObject + ": Invalid " + cAlphaFieldNames( 1 ) + "=\"" + cAlphaArgs( 1 ) + "\" supplied." );
					ShowContinueError( " Legal values are: \"DXF\", \"DXF:WireFrame\", \"VRML\"." );

				}}
			}

			RepNum = GetNumSectionsFound( "Report Variable Dictionary" );
			if ( RepNum > 0 ) {
				VarDict = true;
				VarDictOption1 = "REGULAR";
				VarDictOption2 = "";
			}

			cCurrentModuleObject = "Output:VariableDictionary";

			NumReports = GetNumObjectsFound( cCurrentModuleObject );
			for ( RepNum = 1; RepNum <= NumReports; ++RepNum ) {
				GetObjectItem( cCurrentModuleObject, RepNum, cAlphaArgs, NumNames, rNumericArgs, NumNumbers, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				VarDict = true;
				VarDictOption1 = cAlphaArgs( 1 );
				VarDictOption2 = cAlphaArgs( 2 );

			}

			cCurrentModuleObject = "Output:Constructions";
			NumReports = GetNumObjectsFound( cCurrentModuleObject );
			for ( RepNum = 1; RepNum <= NumReports; ++RepNum ) {
				GetObjectItem( cCurrentModuleObject, RepNum, cAlphaArgs, NumNames, rNumericArgs, NumNumbers, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				if ( has_prefix( cAlphaArgs( 1 ), "CONSTRUCT" ) ) {
					Constructions = true;
				} else if ( has_prefix( cAlphaArgs( 1 ), "MAT" ) ) {
					Materials = true;
				}
				if ( NumNames > 1 ) {
					if ( has_prefix( cAlphaArgs( 2 ), "CONSTRUCT" ) ) {
						Constructions = true;
					} else if ( has_prefix( cAlphaArgs( 2 ), "MAT" ) ) {
						Materials = true;
					}
				}
			}

			cCurrentModuleObject = "Output:EnergyManagementSystem";
			NumReports = GetNumObjectsFound( cCurrentModuleObject );
			for ( RepNum = 1; RepNum <= NumReports; ++RepNum ) {
				GetObjectItem( cCurrentModuleObject, RepNum, cAlphaArgs, NumNames, rNumericArgs, NumNumbers, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				EMSoutput = true;

				{ auto const SELECT_CASE_var( cAlphaArgs( 1 ) );

				if ( SELECT_CASE_var == "NONE" ) {
					OutputEMSActuatorAvailSmall = false;
					OutputEMSActuatorAvailFull = false;
				} else if ( SELECT_CASE_var == "NOTBYUNIQUEKEYNAMES" ) {
					OutputEMSActuatorAvailSmall = true;
					OutputEMSActuatorAvailFull = false;
				} else if ( SELECT_CASE_var == "VERBOSE" ) {
					OutputEMSActuatorAvailSmall = false;
					OutputEMSActuatorAvailFull = true;

				} else if ( SELECT_CASE_var == "" ) {
					ShowWarningError( cCurrentModuleObject + ": Blank " + cAlphaFieldNames( 1 ) + " supplied." );
					ShowContinueError( " Legal values are: \"None\", \"NotByUniqueKeyNames\", \"Verbose\". \"None\" will be used." );
					OutputEMSActuatorAvailSmall = false;
					OutputEMSActuatorAvailFull = false;
				} else {
					ShowWarningError( cCurrentModuleObject + ": Invalid " + cAlphaFieldNames( 1 ) + "=\"" + cAlphaArgs( 1 ) + "\" supplied." );
					ShowContinueError( " Legal values are: \"None\", \"NotByUniqueKeyNames\", \"Verbose\". \"None\" will be used." );
					OutputEMSActuatorAvailSmall = false;
					OutputEMSActuatorAvailFull = false;
				}}

				{ auto const SELECT_CASE_var( cAlphaArgs( 2 ) );

				if ( SELECT_CASE_var == "NONE" ) {
					OutputEMSInternalVarsFull = false;
					OutputEMSInternalVarsSmall = false;
				} else if ( SELECT_CASE_var == "NOTBYUNIQUEKEYNAMES" ) {
					OutputEMSInternalVarsFull = false;
					OutputEMSInternalVarsSmall = true;
				} else if ( SELECT_CASE_var == "VERBOSE" ) {
					OutputEMSInternalVarsFull = true;
					OutputEMSInternalVarsSmall = false;
				} else if ( SELECT_CASE_var == "" ) {
					ShowWarningError( cCurrentModuleObject + ": Blank " + cAlphaFieldNames( 2 ) + " supplied." );
					ShowContinueError( " Legal values are: \"None\", \"NotByUniqueKeyNames\", \"Verbose\". \"None\" will be used." );
					OutputEMSInternalVarsFull = false;
					OutputEMSInternalVarsSmall = false;
				} else {
					ShowWarningError( cCurrentModuleObject + ": Invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 1 ) + "\" supplied." );
					ShowContinueError( " Legal values are: \"None\", \"NotByUniqueKeyNames\", \"Verbose\". \"None\" will be used." );
					OutputEMSInternalVarsFull = false;
					OutputEMSInternalVarsSmall = false;
				}}

				{ auto const SELECT_CASE_var( cAlphaArgs( 3 ) );

				if ( SELECT_CASE_var == "NONE" ) {
					OutputEMSErrors = false;
					OutputFullEMSTrace = false;
				} else if ( SELECT_CASE_var == "ERRORSONLY" ) {
					OutputEMSErrors = true;
					OutputFullEMSTrace = false;
				} else if ( SELECT_CASE_var == "VERBOSE" ) {
					OutputFullEMSTrace = true;
					OutputEMSErrors = true;
				} else if ( SELECT_CASE_var == "" ) {
					ShowWarningError( cCurrentModuleObject + ": Blank " + cAlphaFieldNames( 3 ) + " supplied." );
					ShowContinueError( " Legal values are: \"None\", \"ErrorsOnly\", \"Verbose\". \"None\" will be used." );
					OutputEMSErrors = false;
					OutputFullEMSTrace = false;
				} else {
					ShowWarningError( cCurrentModuleObject + ": Invalid " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 1 ) + "\" supplied." );
					ShowContinueError( " Legal values are: \"None\", \"ErrorsOnly\", \"Verbose\". \"None\" will be used." );
					OutputEMSErrors = false;
					OutputFullEMSTrace = false;
				}}

			}

			//    cCurrentModuleObject='Output:Schedules'
			//    NumReports=GetNumObjectsFound(cCurrentModuleObject)
			//    DO RepNum=1,NumReports
			//      CALL GetObjectItem(cCurrentModuleObject,RepNum,cAlphaArgs,NumNames,rNumericArgs,NumNumbers,IOStat,  &
			//                     AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
			//                     AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
			//      SchRpt=.TRUE.
			//      SchRptOption=cAlphaArgs(1)
			//    ENDDO

			GetReportInput = false;

		}

		// Process the Scan Request
		DoReport = false;

		{ auto const SELECT_CASE_var( MakeUPPERCase( reportName ) );
		if ( SELECT_CASE_var == "CONSTRUCTIONS" ) {
			if ( present( ReportKey ) ) {
				if ( SameString( ReportKey, "Constructions" ) ) DoReport = Constructions;
				if ( SameString( ReportKey, "Materials" ) ) DoReport = Materials;
			}
		} else if ( SELECT_CASE_var == "VIEWFACTORINFO" ) {
			DoReport = ViewFactorInfo;
			if ( present( Option1 ) ) Option1 = ViewRptOption1;
		} else if ( SELECT_CASE_var == "VARIABLEDICTIONARY" ) {
			DoReport = VarDict;
			if ( present( Option1 ) ) Option1 = VarDictOption1;
			if ( present( Option2 ) ) Option2 = VarDictOption2;
			//    CASE ('SCHEDULES')
			//     DoReport=SchRpt
			//      IF (PRESENT(Option1)) Option1=SchRptOption
		} else if ( SELECT_CASE_var == "SURFACES" ) {
			{ auto const SELECT_CASE_var1( MakeUPPERCase( ReportKey ) ); //Autodesk:OPTIONAL ReportKey used without PRESENT check
			if ( SELECT_CASE_var1 == "COSTINFO" ) {
				DoReport = CostInfo;
			} else if ( SELECT_CASE_var1 == "DXF" ) {
				DoReport = DXFReport;
				if ( present( Option1 ) ) Option1 = DXFOption1;
				if ( present( Option2 ) ) Option2 = DXFOption2;
			} else if ( SELECT_CASE_var1 == "DXF:WIREFRAME" ) {
				DoReport = DXFWFReport;
				if ( present( Option1 ) ) Option1 = DXFWFOption1;
				if ( present( Option2 ) ) Option2 = DXFWFOption2;
			} else if ( SELECT_CASE_var1 == "VRML" ) {
				DoReport = VRMLReport;
				if ( present( Option1 ) ) Option1 = VRMLOption1;
				if ( present( Option2 ) ) Option2 = VRMLOption2;
			} else if ( SELECT_CASE_var1 == "VERTICES" ) {
				DoReport = SurfVert;
			} else if ( SELECT_CASE_var1 == "DETAILS" ) {
				DoReport = SurfDet;
			} else if ( SELECT_CASE_var1 == "DETAILSWITHVERTICES" ) {
				DoReport = SurfDetWVert;
			} else if ( SELECT_CASE_var1 == "LINES" ) {
				DoReport = LineRpt;
				if ( present( Option1 ) ) Option1 = LineRptOption1;
			} else {
			}}
		} else if ( SELECT_CASE_var == "ENERGYMANAGEMENTSYSTEM" ) {
			DoReport = EMSoutput;
		} else {
		}}

	}

	void
	CheckCreatedZoneItemName(
		std::string const & calledFrom, // routine called from
		std::string const & CurrentObject, // object being parsed
		std::string const & ZoneName, // Zone Name associated
		std::string::size_type const MaxZoneNameLength, // maximum length of zonelist zone names
		std::string const & ItemName, // Item name (People, Lights, etc object)
		Array1_string const & ItemNames, // Item Names to check for duplication
		int const NumItems, // Number of items in ItemNames array
		std::string & ResultName, // Resultant name
		bool & errFlag // Error flag set to true if error found here.
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   December 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine checks "global" objects (that is, ones with ZoneList used in the name
		// specification) along with a specific name for the current object for length and duplication
		// with previous objects of that class.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::MaxNameLength;
		using InputProcessor::FindItemInList;

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		errFlag = false;
		std::string::size_type const ItemNameLength = len( ItemName );
		std::string::size_type const ItemLength = len( ZoneName ) + ItemNameLength;
		ResultName = ZoneName + ' ' + ItemName;
		bool TooLong = false;
		if ( ItemLength > MaxNameLength ) {
			ShowWarningError( calledFrom + CurrentObject + " Combination of ZoneList and Object Name generate a name too long." );
			ShowContinueError( "Object Name=\"" + ItemName + "\"." );
			ShowContinueError( "ZoneList/Zone Name=\"" + ZoneName + "\"." );
			ShowContinueError( "Item length=[" + RoundSigDigits( int( ItemLength ) ) + "] > Maximum Length=[" + RoundSigDigits( MaxNameLength ) + "]. You may need to shorten the names." );
			ShowContinueError( "Shortening the Object Name by [" + RoundSigDigits( int( MaxZoneNameLength + 1 + ItemNameLength - MaxNameLength ) ) + "] characters will assure uniqueness for this ZoneList." );
			ShowContinueError( "name that will be used (may be needed in reporting)=\"" + ResultName + "\"." );
			TooLong = true;
		}

		int FoundItem = FindItemInList( ResultName, ItemNames, NumItems );

		if ( FoundItem != 0 ) {
			ShowSevereError( calledFrom + CurrentObject + "=\"" + ItemName + "\", Duplicate Generated name encountered." );
			ShowContinueError( "name=\"" + ResultName + "\" has already been generated or entered as " + CurrentObject + " item=[" + RoundSigDigits( FoundItem ) + "]." );
			if ( TooLong ) ShowContinueError( "Duplicate name likely caused by the previous \"too long\" warning." );
			ResultName = "xxxxxxx";
			errFlag = true;
		}

	}

} // General

} // EnergyPlus
