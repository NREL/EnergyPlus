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

#ifndef WindowEquivalentLayer_hh_INCLUDED
#define WindowEquivalentLayer_hh_INCLUDED

// C++ Headers
#include <functional>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Array2A.hh>
#include <ObjexxFCL/Array2S.hh>
#include <ObjexxFCL/Array3D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataWindowEquivalentLayer.hh>

namespace EnergyPlus {

namespace WindowEquivalentLayer {

	// Using/Aliasing
	using namespace DataWindowEquivalentLayer;

	// Data
	extern Real64 const RadiansToDeg; // Conversion for Radians to Degrees
	extern Real64 const PAtmSeaLevel; // Standard atmospheric pressure at sea level (Pa)
	extern int const hipRHO; // return reflectance
	extern int const hipTAU; // return transmittance
	extern Real64 const SMALL_ERROR; // small number
	// CFSGAP: space between layers (gap types)
	extern int const gtySEALED; // sealed
	extern int const gtyOPENin; // open to indoor air  (re Open Channel Flow (OCF))
	extern int const gtyOPENout; // open to outdoor air (re Open Channel Flow (OCF))
	// shade control options
	extern int const lscNONE; // no control
	extern int const lscVBPROF; // VB slatA = ProfA (max gain)
	extern int const lscVBNOBM; // VB slatA just exclude beam
	// Constants
	extern int const hipRHO_BT0;
	extern int const hipTAU_BT0;
	extern int const hipTAU_BB0;
	extern int const hipDIM; // dimension of parameter array

	extern Array3D< Real64 > CFSDiffAbsTrans;
	extern Array1D_bool EQLDiffPropFlag;

	// MODULE SUBROUTINES:
	// Initialization routines for module

	// Standard Ratings calculation routines

	// Calculation routines for the module

	// Functions

	void
	clear_state();

	void
	InitEquivalentLayerWindowCalculations();

	void
	SetEquivalentLayerWindowProperties( int const ConstrNum );

	void
	CalcEQLWindowUvalue(
		CFSTY const & FS, // CFS to be calculated
		Real64 & UNFRC // NFRC U-factor, W/m2-K
	);

	void
	CalcEQLWindowSHGCAndTransNormal(
		CFSTY & FS, // fenestration system
		Real64 & SHGCSummer, // solar heat gain coefficient
		Real64 & TransNormal // transmittance at normal incidence
	);

	void
	CalcEQLWindowOpticalProperty(
		CFSTY & FS, // fenestration system
		int const DiffBeamFlag, // isDIFF: calc diffuse properties
		Array2A< Real64 > Abs1,
		Real64 const IncA, // angle of incidence, radians
		Real64 const VProfA, // inc solar vertical profile angle, radians
		Real64 const HProfA // inc solar horizontal profile angle, radians
	);

	void
	EQLWindowSurfaceHeatBalance(
		int const SurfNum, // Surface number
		Real64 const HcOut, // outside convection coeficient at this timestep, W/m2K
		Real64 & SurfInsideTemp, // Inside window surface temperature (innermost face) [C]
		Real64 & SurfOutsideTemp, // Outside surface temperature (C)
		Real64 & SurfOutsideEmiss,
		int const CalcCondition // Calucation condition (summer, winter or no condition)
	);

	void
	OPENNESS_LW(
		Real64 const OPENNESS, // shade openness (=tausbb at normal incidence)
		Real64 const EPSLW0, // apparent LW emittance of shade at 0 openness
		Real64 const TAULW0, // apparent LW transmittance of shade at 0 openness
		Real64 & EPSLW, // returned: effective LW emittance of shade
		Real64 & TAULW // returned: effective LW transmittance of shade
	);

	Real64
	P01(
		Real64 const P, // property
		std::string const & WHAT // identifier for err msg
	);

	Real64
	HEMINT(
		std::function< Real64( Real64 const THETA, int const OPT, Array1A< Real64 > const ) > F, // property integrand function
		int const F_Opt, // options passed to F() (hipRHO, hipTAU)
		Array1A< Real64 > const F_P // parameters passed to F()
	);

	void
	RB_DIFF(
		Real64 const RHO_BT0, // normal incidence beam-total reflectance
		Real64 const TAU_BT0, // normal incidence beam-total transmittance
		Real64 const TAU_BB0, // normal incidence beam-beam transmittance
		Real64 & RHO_DD, // returned: diffuse-diffuse reflectance
		Real64 & TAU_DD // returned: diffuse-diffuse transmittance
	);

	Real64
	RB_F(
		Real64 const THETA, // incidence angle, radians
		int const OPT, // options (unused)
		Array1A< Real64 > const P // parameters
	);

	void
	RB_BEAM(
		Real64 const xTHETA, // angle of incidence, radians (0 - PI/2)
		Real64 const RHO_BT0, // normal incidence beam-total front reflectance
		Real64 const TAU_BT0, // normal incidence beam-total transmittance
		Real64 const TAU_BB0, // normal incidence beam-beam transmittance
		Real64 & RHO_BD, // returned: beam-diffuse front reflectance
		Real64 & TAU_BB, // returned: beam-beam transmittance
		Real64 & TAU_BD // returned: beam-diffuse transmittance
	);

	void
	IS_DIFF(
		Real64 const RHO_BT0, // normal incidence beam-total reflectance
		Real64 const TAU_BT0, // normal incidence beam-total transmittance
		Real64 const TAU_BB0, // normal incidence beam-beam transmittance
		Real64 & RHO_DD, // returned: diffuse-diffuse reflectance
		Real64 & TAU_DD // returned: diffuse-diffuse transmittance
	);

	Real64
	IS_F(
		Real64 const THETA, // incidence angle, radians
		int const OPT, // options (1=reflectance, 2=transmittance)
		Array1A< Real64 > const P // parameters
	);

	void
	IS_BEAM(
		Real64 const xTHETA, // incidence angle, radians (0 - PI/2)
		Real64 const RHO_BT0, // beam-total reflectance
		Real64 const TAU_BT0, // beam-total transmittance at normal incidence
		Real64 const TAU_BB0, // beam-beam transmittance at normal incidence
		Real64 & RHO_BD, // returned: beam-diffuse reflectance
		Real64 & TAU_BB, // returned: beam-beam transmittance
		Real64 & TAU_BD // returned: beam-diffuse transmittance
	);

	Real64
	IS_OPENNESS(
		Real64 const D, // wire diameter
		Real64 const S // wire spacing
	);

	Real64
	IS_DSRATIO( Real64 const OPENNESS ); // openness

	void
	FM_DIFF(
		Real64 const RHO_BT0, // fabric beam-total reflectance at normal incidence
		Real64 const TAU_BT0, // fabric beam-total transmittance at normal incidence
		Real64 const TAU_BB0, // forward facing fabric beam-beam transmittance at normal incidence
		Real64 & RHO_DD, // returned: fabric diffuse-diffuse reflectance
		Real64 & TAU_DD // returned: fabric diffuse-diffuse transmittance
	);

	Real64
	FM_F(
		Real64 const THETA, // incidence angle, radians
		int const Opt, // options (hipRHO, hipTAU)
		Array1A< Real64 > const P // parameters
	);

	void
	FM_BEAM(
		Real64 const xTHETA, // incidence angle, radians (0 - PI/2)
		Real64 const RHO_BT0, // fabric beam-total reflectance
		Real64 const TAU_BT0, // fabric beam-total transmittance at normal incidence
		Real64 const TAU_BB0, // fabric beam-beam transmittance at normal incidence
		Real64 & RHO_BD, // returned: fabric beam-diffuse reflectance
		Real64 & TAU_BB, // returned: fabric beam-beam transmittance
		Real64 & TAU_BD // returned: fabric beam-diffuse transmittance
	);

	void
	PD_LW(
		Real64 const S, // pleat spacing (> 0)
		Real64 const W, // pleat depth (>=0, same units as S)
		Real64 const OPENNESS_FABRIC, // fabric openness, 0-1 (=tausbb at normal incidence)
		Real64 const EPSLWF0_FABRIC, // fabric LW front emittance at 0 openness
		Real64 const EPSLWB0_FABRIC, // fabric LW back emittance at 0 openness
		Real64 const TAULW0_FABRIC, // fabric LW transmittance at 0 openness
		Real64 & EPSLWF_PD, // returned: drape front effective LW emittance
		Real64 & TAULW_PD // returned: drape effective LW transmittance
	);

	void
	PD_DIFF(
		Real64 const S, // pleat spacing (> 0)
		Real64 const W, // pleat depth (>=0, same units as S)
		Real64 const RHOFF_DD, // fabric front diffuse-diffuse reflectance
		Real64 const RHOBF_DD, // fabric back diffuse-diffuse reflectance
		Real64 const TAUF_DD, // fabric diffuse-diffuse transmittance
		Real64 & RHOFDD, // returned: drape diffuse-diffuse reflectance
		Real64 & TAUFDD // returned: drape diffuse-diffuse transmittance
	);

	void
	PD_BEAM(
		Real64 const S, // pleat spacing (> 0)
		Real64 const W, // pleat depth (>=0, same units as S)
		Real64 const OHM_V_RAD, // vertical profile angle, radians +=above horiz
		Real64 const OHM_H_RAD, // horizontal profile angle, radians=clockwise when viewed from above
		Real64 const RHOFF_BT0, // beam total reflectance front (outside)
		Real64 const TAUFF_BB0, // beam beam transmittance front (outside)
		Real64 const TAUFF_BD0, // beam diffuse transmittance front (outside)
		Real64 const RHOFF_DD, // diffuse-diffuse reflectance front (outside)
		Real64 const TAUFF_DD, // diffuse-diffuse transmittance front (outside)
		Real64 const RHOBF_BT0, // beam total reflectance back (inside)
		Real64 const TAUBF_BB0, // beam beam total transmittance back (inside)
		Real64 const TAUBF_BD0, // beam diffuse transmittance back (inside)
		Real64 const RHOBF_DD, // diffuse-diffuse reflectance front (outside)
		Real64 const TAUBF_DD, // diffuse-diffuse transmittance front (outside)
		Real64 & RHO_BD, // returned: drape front beam-diffuse reflectance
		Real64 & TAU_BB, // returned: drape beam-beam transmittance
		Real64 & TAU_BD // returned: drape beam-diffuse transmittance
	);

	void
	PD_BEAM_CASE_I(
		Real64 const S, // pleat spacing (> 0)
		Real64 const W, // pleat depth (>=0, same units as S)
		Real64 const OMEGA_H, // horizontal profile angle, radians
		Real64 const DE, // width of illumination on pleat bottom (same units as S)
		Real64 const RHOFF_BT_PARL,
		Real64 const TAUFF_BB_PARL,
		Real64 const TAUFF_BD_PARL,
		Real64 const RHOBF_BT_PARL,
		Real64 const TAUBF_BB_PARL,
		Real64 const TAUBF_BD_PARL,
		Real64 const RHOFF_BT_PERP,
		Real64 const TAUFF_BB_PERP,
		Real64 const TAUFF_BD_PERP,
		Real64 const RHOBF_BT_PERP,
		Real64 const TAUBF_BB_PERP,
		Real64 const TAUBF_BD_PERP,
		Real64 const RHOBF_DD, // fabric back diffuse-diffuse reflectance
		Real64 const RHOFF_DD, // fabric front diffuse-diffuse reflectance
		Real64 const TAUFF_DD, // fabric front diffuse-diffuse transmittance
		Real64 const TAUBF_DD, // fabric back diffuse-diffuse transmittance
		Real64 & RHO_BD, // returned: drape front beam-diffuse reflectance
		Real64 & TAU_BD, // returned: drape front beam-diffuse transmittance
		Real64 & TAU_BB // returned: drape front beam-beam transmittance
	);

	void
	PD_BEAM_CASE_II(
		Real64 const S, // pleat spacing (> 0)
		Real64 const W, // pleat depth (>=0, same units as S)
		Real64 const OMEGA_H, // horizontal profile angle, radians
		Real64 const DE, // width of illumination on pleat bottom (same units as S)
		Real64 const RHOFF_BT_PARL,
		Real64 const TAUFF_BB_PARL,
		Real64 const TAUFF_BD_PARL,
		Real64 const RHOBF_BT_PARL,
		Real64 const TAUBF_BB_PARL,
		Real64 const TAUBF_BD_PARL,
		Real64 const RHOFF_BT_PERP,
		Real64 const TAUFF_BB_PERP,
		Real64 const TAUFF_BD_PERP,
		Real64 const RHOBF_BT_PERP,
		Real64 const TAUBF_BB_PERP,
		Real64 const TAUBF_BD_PERP,
		Real64 const RHOBF_DD, // fabric back diffuse-diffuse reflectance
		Real64 const RHOFF_DD, // fabric front diffuse-diffuse reflectance
		Real64 const TAUFF_DD, // fabric front diffuse-diffuse transmittance
		Real64 const TAUBF_DD, // fabric back diffuse-diffuse transmittance
		Real64 & RHO_BD, // returned: drape front beam-diffuse reflectance
		Real64 & TAU_BD, // returned: drape front beam-diffuse transmittance
		Real64 & TAU_BB // returned: drape front beam-beam transmittance
	);

	void
	PD_BEAM_CASE_III(
		Real64 const S, // pleat spacing (> 0)
		Real64 const W, // pleat depth (>=0, same units as S)
		Real64 const OMEGA_H, // horizontal profile angle, radians
		Real64 const DE, // width of illumination on pleat bottom (same units as S)
		Real64 const RHOFF_BT_PARL,
		Real64 const TAUFF_BB_PARL,
		Real64 const TAUFF_BD_PARL,
		Real64 const RHOBF_BT_PARL,
		Real64 const TAUBF_BB_PARL,
		Real64 const TAUBF_BD_PARL,
		Real64 const RHOFF_BT_PERP,
		Real64 const TAUFF_BB_PERP,
		Real64 const TAUFF_BD_PERP,
		Real64 const RHOBF_BT_PERP,
		Real64 const TAUBF_BB_PERP,
		Real64 const TAUBF_BD_PERP,
		Real64 const RHOBF_DD, // fabric back diffuse-diffuse reflectance
		Real64 const RHOFF_DD, // fabric front diffuse-diffuse reflectance
		Real64 const TAUFF_DD, // fabric front diffuse-diffuse transmittance
		Real64 const TAUBF_DD, // fabric back diffuse-diffuse transmittance
		Real64 & RHO_BD, // returned: drape front beam-diffuse reflectance
		Real64 & TAU_BD, // returned: drape front beam-diffuse transmittance
		Real64 & TAU_BB // returned: drape front beam-beam transmittance
	);

	void
	PD_BEAM_CASE_IV(
		Real64 const S, // pleat spacing (> 0)
		Real64 const W, // pleat depth (>=0, same units as S)
		Real64 const OMEGA_H, // horizontal profile angle, radians
		Real64 const DE, // width of illumination on pleat bottom (same units as S)
		Real64 const RHOFF_BT_PARL,
		Real64 const TAUFF_BB_PARL,
		Real64 const TAUFF_BD_PARL,
		Real64 const RHOBF_BT_PARL,
		Real64 const TAUBF_BB_PARL,
		Real64 const TAUBF_BD_PARL,
		Real64 const RHOFF_BT_PERP,
		Real64 const TAUFF_BB_PERP,
		Real64 const TAUFF_BD_PERP,
		Real64 const RHOBF_BT_PERP,
		Real64 const TAUBF_BB_PERP,
		Real64 const TAUBF_BD_PERP,
		Real64 const RHOBF_DD, // fabric back diffuse-diffuse reflectance
		Real64 const RHOFF_DD, // fabric front diffuse-diffuse reflectance
		Real64 const TAUFF_DD, // fabric front diffuse-diffuse transmittance
		Real64 const TAUBF_DD, // fabric back diffuse-diffuse transmittance
		Real64 & RHO_BD, // returned: drape front beam-diffuse reflectance
		Real64 & TAU_BD, // returned: drape front beam-diffuse transmittance
		Real64 & TAU_BB // returned: drape front beam-beam transmittance
	);

	void
	PD_BEAM_CASE_V(
		Real64 const S, // pleat spacing (> 0)
		Real64 const W, // pleat depth (>=0, same units as S)
		Real64 const OMEGA_H, // horizontal profile angle, radians
		Real64 const DE, // width of illumination on pleat bottom (same units as S)
		Real64 const RHOFF_BT_PARL,
		Real64 const TAUFF_BB_PARL,
		Real64 const TAUFF_BD_PARL,
		Real64 const RHOBF_BT_PARL,
		Real64 const TAUBF_BB_PARL,
		Real64 const TAUBF_BD_PARL,
		Real64 const RHOFF_BT_PERP,
		Real64 const TAUFF_BB_PERP,
		Real64 const TAUFF_BD_PERP,
		Real64 const RHOBF_BT_PERP,
		Real64 const TAUBF_BB_PERP,
		Real64 const TAUBF_BD_PERP,
		Real64 const RHOBF_DD, // fabric back diffuse-diffuse reflectance
		Real64 const RHOFF_DD, // fabric front diffuse-diffuse reflectance
		Real64 const TAUFF_DD, // fabric front diffuse-diffuse transmittance
		Real64 const TAUBF_DD, // fabric back diffuse-diffuse transmittance
		Real64 & RHO_BD, // returned: drape front beam-diffuse reflectance
		Real64 & TAU_BD, // returned: drape front beam-diffuse transmittance
		Real64 & TAU_BB // returned: drape front beam-beam transmittance
	);

	void
	PD_BEAM_CASE_VI(
		Real64 const S, // pleat spacing (> 0)
		Real64 const W, // pleat depth (>=0, same units as S)
		Real64 const OMEGA_H, // horizontal profile angle, radians
		Real64 const DE, // width of illumination on pleat bottom (same units as S)
		Real64 const RHOFF_BT_PARL,
		Real64 const TAUFF_BB_PARL,
		Real64 const TAUFF_BD_PARL,
		Real64 const RHOBF_BT_PARL,
		Real64 const TAUBF_BB_PARL,
		Real64 const TAUBF_BD_PARL,
		Real64 const RHOFF_BT_PERP,
		Real64 const TAUFF_BB_PERP,
		Real64 const TAUFF_BD_PERP,
		Real64 const RHOBF_BT_PERP,
		Real64 const TAUBF_BB_PERP,
		Real64 const TAUBF_BD_PERP,
		Real64 const RHOBF_DD, // fabric back diffuse-diffuse reflectance
		Real64 const RHOFF_DD, // fabric front diffuse-diffuse reflectance
		Real64 const TAUFF_DD, // fabric front diffuse-diffuse transmittance
		Real64 const TAUBF_DD, // fabric back diffuse-diffuse transmittance
		Real64 & RHO_BD, // returned: drape front beam-diffuse reflectance
		Real64 & TAU_BD, // returned: drape front beam-diffuse transmittance
		Real64 & TAU_BB // returned: drape front beam-beam transmittance
	);

	void
	VB_DIFF(
		Real64 const S, // slat spacing (any length units; same units as W)
		Real64 const W, // slat tip-to-tip width (any length units; same units as S)
		Real64 const PHI, // slat angle, radians (-PI/2 <= PHI <= PI/2)
		Real64 const RHODFS_SLAT, // reflectance of downward-facing slat surfaces (concave?)
		Real64 const RHOUFS_SLAT, // reflectance of upward-facing slat surfaces (convex?)
		Real64 const TAU_SLAT, // diffuse transmitance of slats
		Real64 & RHOFVB, // returned: front side effective diffuse reflectance of venetian blind
		Real64 & TAUVB // returned: effective diffuse transmittance of venetian blind
	);

	Real64
	VB_SLAT_RADIUS_RATIO(
		Real64 const W, // slat tip-to-tip (chord) width (any units; same units as C) must be > 0
		Real64 const C // slat crown height (any units, same units as W) must be >= 0
	);

	void
	VB_SOL46_CURVE(
		Real64 const S, // slat spacing (any length units; same units as W)
		Real64 const W, // slat tip-to-tip (chord) width (any length units; same units as S)
		Real64 const SL_WR, // slat curvature radius ratio (= W/R)
		Real64 const PHIx, // slat angle, radians (-PI/2 <= PHI <= PI/2)
		Real64 const OMEGAx, // incident beam profile angle (radians)
		Real64 const RHODFS_SLAT, // SW (solar) reflectance downward-facing slat surfaces (concave?)
		Real64 const RHOUFS_SLAT, // SW (solar) reflectance upward-facing slat surfaces (convex?)
		Real64 const TAU_SLAT, // SW (solar) transmittance of slats
		Real64 & RHO_BD, // returned: effective SW (solar) beam-to-diffuse reflectance front side
		Real64 & TAU_BB, // returned: effective SW (solar) beam-to-beam transmittance front side
		Real64 & TAU_BD // returned: effective SW (solar) beam-to-diffuse transmittance front side
	);

	void
	VB_SOL4(
		Real64 const S, // slat spacing (any length units; same units as W)
		Real64 const W, // slat tip-to-tip width (any length units; same units as S)
		Real64 const OMEGA, // incident beam profile angle (radians)
		Real64 const DE, // distance from front tip of any slat to shadow (caused by the adjacent slat) on
		Real64 const PHI, // slat angle, radians (-PI/2 <= PHI <= PI/2)
		Real64 const RHODFS_SLAT, // solar reflectance downward-facing slat surfaces (concave?)
		Real64 const RHOUFS_SLAT, // solar reflectance upward-facing slat surfaces (convex?)
		Real64 const TAU_SLAT, // solar transmittance of slat
		Real64 & RHO_BD, // returned: solar beam-to-diffuse reflectance the venetian blind (front side)
		Real64 & TAU_BD // returned: solar beam-to-diffuse transmittance of the venetian blind (front side)
	);

	void
	VB_SOL6(
		Real64 const S, // slat spacing (any length units; same units as W)
		Real64 const W, // slat tip-to-tip width (any length units; same units as S)
		Real64 const OMEGA, // incident beam profile angle (radians)
		Real64 const DE, // distance from front tip of any slat to shadow (caused by the adjacent slat) on
		Real64 const PHI, // slat angle, radians (-PI/2 <= PHI <= PI/2)
		Real64 const RHODFS_SLAT, // solar reflectance downward-facing slat surfaces (concave)
		Real64 const RHOUFS_SLAT, // solar reflectance upward-facing slat surfaces (convex)
		Real64 const TAU_SLAT, // solar transmittance of slat
		Real64 & RHO_BD, // returned: solar beam-to-diffuse reflectance the venetian blind (front side)
		Real64 & TAU_BD // returned: solar beam-to-diffuse transmittance of the venetian blind (front side)
	);

	void
	SOLMATS(
		int const N, // # of active rows in A
		Array2S< Real64 > A, // matrix, minimum required dimensions: A( N, N+2)
		Array1S< Real64 > XSOL // returned: solution vector, min req dimension: XSOL( N)
	);

	bool
	ASHWAT_Thermal(
		CFSTY const & FS, // fenestration system
		Real64 const TIN, // indoor / outdoor air temperature, K
		Real64 const TOUT,
		Real64 const HCIN, // indoor / outdoor convective heat transfer
		Real64 const HCOUT,
		Real64 const TRMOUT,
		Real64 const TRMIN, // indoor / outdoor mean radiant temp, K
		Real64 const ISOL, // total incident solar, W/m2 (values used for SOURCE derivation)
		Array1S< Real64 > const SOURCE, // absorbed solar by layer,  W/m2
		Real64 const TOL, // convergence tolerance, usually
		Array1A< Real64 > QOCF, // returned: heat flux to layer i from gaps i-1 and i
		Real64 & QOCFRoom, // returned: open channel heat gain to room, W/m2
		Array1A< Real64 > T, // returned: layer temperatures, 1=outside-most layer, K
		Array1< Real64 > & Q, // returned: heat flux at ith gap (betw layers i and i+1), W/m2
		Array1A< Real64 > JF, // returned: front (outside facing) radiosity of surfaces, W/m2
		Array1A< Real64 > JB, // returned: back (inside facing) radiosity, W/m2
		Array1A< Real64 > HC, // returned: gap convective heat transfer coefficient, W/m2K
		Real64 & UCG, // returned: center-glass U-factor, W/m2-K
		Real64 & SHGC, // returned: center-glass SHGC (Solar Heat Gain Coefficient)
		Optional_bool_const HCInFlag = _ // If true uses ISO Std 150099 routine for HCIn calc
	);

	void
	DL_RES_r2(
		Real64 const Tg, // mean glass layer temperature, {K}
		Real64 const Td, // mean diathermanous layer temperature, {K}
		Real64 const Tm, // mean radiant room temperature, {K}
		Real64 const rhog, // reflectance of glass layer, {-}
		Real64 const rhodf, // front reflectance of diathermanous layer, {-}
		Real64 const rhodb, // back reflectance of diathermanous layer, {-}
		Real64 const taud, // transmittance of diathermanous layer, {-}
		Real64 const rhom, // reflectance of the room, {-}
		Real64 & hr_gm, // heat transfer coefficient between left and right surface {W/m2K}
		Real64 & hr_gd, // heat transfer coefficient between left and middle surface {W/m2K}
		Real64 & hr_md // heat transfer coefficient between right and middle surface {W/m2K}
	);

	void
	SETUP4x4_A(
		Real64 const rhog,
		Real64 const rhodf,
		Real64 const rhodb,
		Real64 const taud,
		Real64 const rhom,
		Array2A< Real64 > A
	);

	Real64
	FRA(
		Real64 const TM, // mean gas temp, K
		Real64 const T, // gas layer thickness, m
		Real64 const DT, // temp difference across layer, K
		Real64 const AK, // gas conductance coeffs, K = AK + BK*TM + CK*TM*TM
		Real64 const BK,
		Real64 const CK,
		Real64 const ACP, // gas specific heat coeffs, CP = ACP + BCP*TM + CCP*TM*TM
		Real64 const BCP,
		Real64 const CCP,
		Real64 const AVISC, // gas viscosity coeffs, VISC = AVISC + BVISC*TM + CVISC*TM*TM
		Real64 const BVISC,
		Real64 const CVISC,
		Real64 const RHOGAS // gas density, kg/m3
	);

	Real64
	FNU( Real64 const RA ); // Rayleigh number

	Real64
	HConvGap(
		CFSGAP const & G, // gap
		Real64 const T1, // bounding surface temps (K)
		Real64 const T2
	);

	Real64
	HRadPar(
		Real64 const T1, // bounding surface temps [K]
		Real64 const T2,
		Real64 const E1, // bounding surface emissivities
		Real64 const E2
	);

	Real64
	HIC_ASHRAE(
		Real64 const L, // glazing height, m
		Real64 const TG, // glazing inside surf temp, C or K
		Real64 const TI // inside air temp, C or K
	);

	void
	SLtoGL(
		Real64 const breal, // distance from shade to glass (m)
		Real64 const Ts, // shade temperature (K)
		Real64 const Tg, // glass temperature (K)
		Real64 & hsg, // the heat transfer coefficient, shade-to-glass, {W/m2K}
		int const scheme
	);

	Real64
	SLtoAMB(
		Real64 const b, // distance from shade to glass (m) where air flow takes place
		Real64 const L, // window height, m (usually taken as 1 m)
		Real64 const Ts, // shade temperature, K
		Real64 const Tamb, // room air temperature, K
		Real64 const hc_in, // indoor (room) convective transfer coeff, W/m2K)
		int const scheme // flag to select model, scheme=2 has problems
	);

	void
	GLtoAMB(
		Real64 const b, // distance from shade to glass {m}
		Real64 const L, // window height {m}, usually taken as 1 meter
		Real64 const Tg, // glass temperature {K}
		Real64 const Tamb, // room air temperature, {K}
		Real64 const hc_in, // inside convection coefficient, {W/m2K}
		Real64 & hgamb, // glass to room air heat transfer coefficient
		int const scheme
	);

	Real64
	ConvectionFactor( CFSLAYER const & L ); // window layer

	bool
	CFSUFactor(
		CFSTY const & FS, // fenestration system
		Real64 const TOUT, // outdoor temperature, C (air and MRT)
		Real64 const HCOUT, // outdoor convective coefficient, W/m2-K
		Real64 const TIN, // indoor air temperature, C
		Real64 const HCIN, // indoor convective coefficient, W/m2-K
		Real64 & U // returned: U factor, W/m2-K
	);

	void
	ASHWAT_Solar(
		int const NL, // # of layers
		Array1S< CFSSWP > const LSWP_ON, // layer SW (solar) properties (off-normal adjusted)
		CFSSWP const & SWP_ROOM, // effective SW (solar) properties of room
		Real64 const IBEAM, // incident beam insolation (W/m2 aperture)
		Real64 const IDIFF, // incident diffuse insolation (W/m2 aperture)
		Real64 const ILIGHTS, // incident diffuse insolation (W/m2 aperture)
		Array1S< Real64 > SOURCE, // returned: layer-by-layer flux of absorbed
		Optional< Array1S< Real64 > > SourceBD = _ // returned: layer-by-layer flux of absorbed
	);

	void
	NETRAD(
		int const NL, // # of layers, 1=outside .. NL=inside
		Array1S< CFSSWP > const LSWP_ON, // layer SW (solar) properties (off-normal adjusted)
		Real64 const RHO_room, // effective solar reflectance of room (at inside)
		Real64 const ISOL, // incident flux (W/m2)
		Array1< Real64 > & QPLUS, // returned: see Edwards paper
		Array1< Real64 > & QMINUS // returned: see Edwards paper
	);

	void
	TDMA_R(
		Array1S< Real64 > X,
		Array1S< Real64 > const AP,
		Array1S< Real64 > const AE,
		Array1S< Real64 > const AW,
		Array1S< Real64 > const BP,
		int const N
	);

	void
	TDMA(
		Array1S< Real64 > X,
		Array1S< Real64 > const AP,
		Array1S< Real64 > const AE,
		Array1S< Real64 > const AW,
		Array1S< Real64 > const BP,
		int const N
	);

	void
	AUTOTDMA(
		Array1S< Real64 > X,
		Array1S< Real64 > AP,
		Array1S< Real64 > const AE,
		Array1S< Real64 > const AW,
		Array1S< Real64 > const BP,
		int & N
	);

	void
	ASHWAT_OffNormalProperties(
		CFSLAYER const & L, // layer for which to derive off-normal properties
		Real64 const THETA, // solar beam angle of incidence, from normal, radians
		Real64 const OMEGA_V, // solar beam vertical profile angle, +=above horizontal, radians
		Real64 const OMEGA_H, // solar beam horizontal profile angle, +=clockwise when viewed
		CFSSWP & LSWP_ON // returned: off-normal properties
	);

	bool
	Specular_OffNormal(
		Real64 const THETA, // solar beam angle of incidence, from normal radians
		Real64 & RAT_1MR, // returned: ratio of off-normal to normal solar (1-reflectance)
		Real64 & RAT_TAU // returned: ratio of off-normal to normal solar transmittance
	);

	void
	Specular_SWP(
		CFSSWP & SWP, // short wave properties (adjusted in place)
		Real64 const OMEGA // incident angle, radians
	);

	void
	Specular_Adjust(
		CFSSWP & SWP, // short wave properties (adjusted in place)
		Real64 const RAT_1MR, // adjustment factors, see Specular_OffNormal()
		Real64 const RAT_TAU // adjustment factors, see Specular_OffNormal()
	);

	void
	Specular_RATDiff(
		Real64 & RAT_1MRDiff,
		Real64 & RAT_TAUDiff
	);

	Real64
	Specular_F(
		Real64 const THETA, // incidence angle, radians
		int const OPT, // options (unused)
		Array1A< Real64 > const P // parameters (none defined)
	);

	void
	Specular_EstimateDiffuseProps( CFSSWP & SWP ); // short wave properties

	bool
	RB_LWP(
		CFSLAYER const & L, // RB layer
		CFSLWP & LLWP // returned: equivalent layer long wave properties
	);

	bool
	RB_SWP(
		CFSLAYER const & L, // RB layer
		CFSSWP & LSWP, // returned: equivalent layer properties set
		Optional< Real64 const > THETA = _ // incident angle, 0 <= theta <= PI/2
	);

	bool
	IS_LWP(
		CFSLAYER const & L, // IS layer
		CFSLWP & LLWP // returned: equivalent layer long wave properties
	);

	bool
	IS_SWP(
		CFSLAYER const & L, // PD layer
		CFSSWP & LSWP, // returned: equivalent layer properties set
		Optional< Real64 const > THETA = _ // incident angle, 0 <= theta <= PI/2
	);

	void
	Fabric_EstimateDiffuseProps( CFSSWP & SWP ); // fabric short wave properties

	bool
	PD_LWP(
		CFSLAYER const & L, // PD layer
		CFSLWP & LLWP // returned: equivalent layer long wave properties
	);

	bool
	PD_SWP(
		CFSLAYER const & L, // PD layer
		CFSSWP & LSWP, // returned: equivalent layer properties set
		Optional< Real64 const > OHM_V_RAD = _, // vertical VB profile angles, radians
		Optional< Real64 const > OHM_H_RAD = _ // horizonatl VB profile angles, radians
	);

	bool
	VB_LWP(
		CFSLAYER const & L, // VB layer
		CFSLWP & LLWP // returned: equivalent layer long wave properties
	);

	bool
	VB_SWP(
		CFSLAYER const & L, // VB layer
		CFSSWP & LSWP, // returned: equivalent off-normal properties
		Optional< Real64 const > OMEGA = _ // incident profile angle (radians)
	);

	bool
	VB_ShadeControl(
		CFSLAYER & L, // VB layer
		Real64 const OMEGA_DEG // incident profile angle (degrees)
	);

	Real64
	VB_CriticalSlatAngle(
		CFSLAYER const & L, // VB layer
		Real64 const OMEGA_DEG // incident profile angle (degrees)
	);

	bool
	DoShadeControl(
		CFSLAYER & L, // layer (returned updated)
		Real64 const THETA, // solar beam angle of incidence, from normal, (radians)
		Real64 const OMEGA_V, // solar beam vertical profile angle, +=above horizontal (radians)
		Real64 const OMEGA_H // solar beam horizontal profile angle, +=clockwise when viewed
	);

	void
	FinalizeCFSLAYER( CFSLAYER & L ); // layer, input: LTYPE, LWP_MAT, SWP_MAT

	bool
	IsGZSLayer( CFSLAYER const & L );

	bool
	IsGlazeLayerX( CFSLAYER const & L );

	bool
	IsControlledShade( CFSLAYER const & L );

	bool
	IsVBLayer( CFSLAYER const & L );

	void
	BuildGap(
		CFSGAP & G, // returned
		int const GType, // gap type (gtyOPENin, gtyOPENout or gtySEALED)
		Real64 & TAS, // gap thickness, m
		Optional< Real64 const > xTMan = _, // re density calc -- temp (C) and pressure (Pa)
		Optional< Real64 const > xPMan = _ // re density calc -- temp (C) and pressure (Pa)
	);

	void
	AdjustVBGap(
		CFSGAP & G, // gap, returned updated
		CFSLAYER const & L // adjacent layer
	);

	float
	DensityCFSFillGas(
		CFSFILLGAS const & FG, // gas properties
		Real64 const P, // pressure, Pa
		Real64 const T // temperature, K
	);

	int
	CFSNGlz( CFSTY const & FS ); // CFS

	int
	CFSHasControlledShade( CFSTY const & FS );

	void
	CheckAndFixCFSLayer( CFSLAYER & Layer );

	void
	FillDefaultsSWP(
		CFSLAYER const & L, // CFSLayer (input properties must be set)
		CFSSWP & SWP // properties to fill
	);

	void
	FinalizeCFS( CFSTY & FS );

	Real64
	EffectiveEPSLF( CFSTY const & FS ); // Complex Fenestration

	Real64
	EffectiveEPSLB( CFSTY const & FS ); // Complex Fenestration

	bool
	FEQX(
		Real64 const a, // values to compare, fractional tolerance
		Real64 const b,
		Real64 const tolF,
		Optional< Real64 > tolAbs = _ // absolute tolerance
	);

	Real64
	TRadC(
		Real64 const J, // radiosity, W/m2
		Real64 const Emiss // surface emissivity
	);

	void
	CalcEQLOpticalProperty(
		int const SurfNum,
		int const BeamDIffFlag, // identifier index of diffuse and beam SW radiation
		Array2A< Real64 > CFSAbs // absorbed beam solar radiation by layers fraction
	);

	void
	CalcEQLWindowStandardRatings( int const ConstrNum ); // construction index

	Real64
	EQLWindowInsideEffectiveEmiss( int const ConstrNum );

	Real64
	EQLWindowOutsideEffectiveEmiss( int const ConstrNum );

	Real64
	HCInWindowStandardRatings(
		Real64 const Height, // Window height, 1.0 m
		Real64 const TSurfIn, // Inside surface temperature
		Real64 const TAirIn // Zone Air Temperature
	);

} // WindowEquivalentLayer

} // EnergyPlus

#endif
