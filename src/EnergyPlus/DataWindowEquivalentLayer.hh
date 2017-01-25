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

#ifndef DataWindowEquivalentLayer_hh_INCLUDED
#define DataWindowEquivalentLayer_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DataWindowEquivalentLayer {

	// Using/Aliasing

	// Data
	// CFSTY: Complex Fenestration System
	extern int const CFSMAXNL; // max # of glaze or shade layers
	// Long-wave (aka LW or thermal) layer properties
	// Short wave (aka SW or solar) layer properties
	// "black" room (no reflection)
	// Layer information

	// Gap Gas Properties
	// Gap information
	// Equivalent Layer Window Constructon
	// CFSLAYER: layer types
	extern int const ltyNONE; // unused / empty layer
	extern int const ltyGLAZE; // glazing layer i.e, purely specular
	extern int const ltyDRAPE; // pleated drapes/curtains
	extern int const ltyROLLB; // roller blind
	extern int const ltyVBHOR; // venetian blinds - horizontal
	extern int const ltyVBVER; // venetian blinds - vertical
	extern int const ltyINSCRN; // insect screen
	extern int const ltyROOM; // indoor space and/or make no adjustment
	extern int const ltyGZS; // glazing with spectral data (read from aux file)
	// index for solar arrays
	extern int const isDIFF;
	extern int const isBEAM;
	// Defined CFSLayers and CFSs
	extern int TotWinEquivLayerConstructs; // Number of constructions with Window equivalent Layer

	// Types

	struct CFSLWP
	{
		// Members
		Real64 EPSLF; // thermal emittance, front (outside) side
		Real64 EPSLB; // thermal emittance, back (inside) side
		Real64 TAUL; // thermal transmittance (same value for front or back)

		// Default Constructor
		CFSLWP() :
			EPSLF( 0.0 ),
			EPSLB( 0.0 ),
			TAUL( 0.0 )
		{}

	};

	struct CFSSWP
	{
		// Members
		Real64 RHOSFBB; // Solar reflectance, BEAM-BEAM, front (outside) side (any angle of incidence)
		Real64 RHOSBBB; // Solar reflectance, BEAM-BEAM, back (inside) side (any angle of incidence)
		Real64 TAUSFBB; // Solar transmittance, BEAM-BEAM, any angle of incidence
		// radiation incident from front (outside)
		Real64 TAUSBBB; // Solar transmittance, BEAM-BEAM, any angle of incidence
		//    radiation incident from back (inside)
		Real64 RHOSFBD; // Solar reflectance, BEAM-DIFFUSE, front (outside) side
		//    BEAM-DIFFUSE, any angle of incidence
		Real64 RHOSBBD; // Solar reflectance, BEAM-DIFFUSE, back (inside) side
		//    any angle of incidence
		Real64 TAUSFBD; // Solar transmittance, BEAM-DIFFUSE, front (outside) side
		//    any angle of incidence
		Real64 TAUSBBD; // Solar transmittance, BEAM-DIFFUSE, any angle of incidence
		Real64 RHOSFDD; // Solar reflectance, DIFFUSE-DIFFUSE, front (outside) side
		Real64 RHOSBDD; // Solar reflectance, DIFFUSE-DIFFUSE, back (inside) side
		Real64 TAUS_DD; // Solar transmittance, DIFFUSE-DIFFUSE
		//    (same value for radiation incident from front or back)

		// Default Constructor
		CFSSWP() :
			RHOSFBB( 0.0 ),
			RHOSBBB( 0.0 ),
			TAUSFBB( 0.0 ),
			TAUSBBB( 0.0 ),
			RHOSFBD( 0.0 ),
			RHOSBBD( 0.0 ),
			TAUSFBD( 0.0 ),
			TAUSBBD( 0.0 ),
			RHOSFDD( 0.0 ),
			RHOSBDD( 0.0 ),
			TAUS_DD( 0.0 )
		{}

	};

	struct CFSLAYER
	{
		// Members
		std::string Name; // ID of layer
		int LTYPE; // layer type (see ltyXXX above)
		int iGZS; // re spectral glazing
		//   = GZSTbl idx of LTYPE=ltyGZS (spectral glazing)
		//   else 0
		// material properties
		//  ltyGLAZE, ltyROLLB: as measured
		CFSSWP SWP_MAT; // ltyGZS: derived from GSZ file data
		CFSLWP LWP_MAT; // ltyVBxxx = slat properties (diffuse only)
		//   short wave (solar)
		//   long wave (thermal)
		// equivalent layer properties (see FinalizeCFSLAYER())
		//  = diff + direct-normal properties for pseudo flat layer
		CFSSWP SWP_EL; // ltyGLAZE, ltyGZS, ltyROLLB: same as _MAT
		CFSLWP LWP_EL; // ltyVBxxx: see VB_xxx()
		//   short wave (solar)
		//   long wave (thermal)
		// Shade Geometry (Slat, Drape, Insect Screen)
		Real64 S; // spacing
		//    VB: slat spacing, m, >0
		//    PD: rectangular pleat spacing, m >0
		//    IS: wire center-to-center spacing (pitch), m, >0
		//    else unused
		Real64 W; // width
		//    VB: slat tip-to-tip (chord width), m, >0
		//        if crown > 0, W < slat flattened width
		//    PD: pleat depth, m >= 0
		//    IS: wire diameter, m, >0, <S
		Real64 C; // crown
		//    VB: slat crown, m >=0 if used
		//    crown assume upward for ltyVBHOR else unused
		Real64 PHI_DEG; // Angle
		//    VB: slat angle, degrees (-90 <= PHI_DEG <= 90)
		//        ltyVBHOR: + = front-side slat tip below horizontal
		//        ltyVBVER: + = front-side slat tip is counter-
		//                     clockwise from normal (viewed from above)
		//    else unused
		// shade control method
		int CNTRL; // VB: lscNONE:   PHI_DEG not changed
		//        lscVBPROF: PHI_DEG = profile angle (max gain)
		//        lscVBNOBM: exclude beam (max visibility w/o beam)
		//                   PHI_DEG altered to just exclude beam
		//                   PHI_DEG = 20 if diffuse only

		// Default Constructor
		CFSLAYER() :
			LTYPE( 0 ),
			iGZS( 0 ),
			S( 0.0 ),
			W( 0.0 ),
			C( 0.0 ),
			PHI_DEG( 0.0 ),
			CNTRL( 0 )
		{}

	};

	struct CFSFILLGAS
	{
		// Members
		std::string Name; // Gas Type (AIR, ARGON, XENON, KRYPTON, CUSTOM)
		//Gas Conductivity: K = AK + BK*T + CK*T*T
		Real64 AK; // conductivity coeff constant term,  (W/m-K)
		Real64 BK; // conductivity coeff of T term, (W/m-K2)
		Real64 CK; // conductivity coeff of T^2 term, (W/m-K^3)
		// Gas Specific heat: CP = ACP + BCP*T + CCP*T*T
		Real64 ACP; // specific heat constant term, (J/kg-K)
		Real64 BCP; // specific heat coeff of T term, (J/kg-K^2)
		Real64 CCP; // specific heat coeff of T^2 term, (J/kg-K^3)
		//Gas Viscosity: Visc = AVISC + BVISC*T + CVISC*T*T
		Real64 AVISC; // viscosity constant term, (N-sec/m2)
		Real64 BVISC; // viscosity coeff of T term, (N-sec/m2-K)
		Real64 CVISC; // viscosity coeff of T^2 term, (N-sec/m2-K^2)
		Real64 MHAT; // apparent molecular weight of gas

		// Default Constructor
		CFSFILLGAS() :
			AK( 0.0 ),
			BK( 0.0 ),
			CK( 0.0 ),
			ACP( 0.0 ),
			BCP( 0.0 ),
			CCP( 0.0 ),
			AVISC( 0.0 ),
			BVISC( 0.0 ),
			CVISC( 0.0 ),
			MHAT( 0.0 )
		{}

	};

	struct CFSGAP
	{
		// Members
		std::string Name; // Gap layer name
		int GTYPE; // gap type (gtyXXX above)
		Real64 TAS; // actual surface-surface gap thickness, mm (always > 0)
		//   VB: minimum tip-surface distance (slats normal to CFS plane)
		Real64 TAS_EFF; // effective gap thickness, mm (always > 0)
		//   if either adjacent layer is VB adjusted
		//   slat angle and convective behavior
		//   else = TAS
		CFSFILLGAS FG; // fill gas properties (see above)
		Real64 RHOGAS; // fill gas density (kg/m3)

		// Default Constructor
		CFSGAP() :
			GTYPE( 0 ),
			TAS( 0.0 ),
			TAS_EFF( 0.0 ),
			RHOGAS( 0.0 )
		{}

	};

	struct CFSTY
	{
		// Members
		std::string Name; // ID (Fenestration Name)
		int NL; // number of layers
		Array1D< CFSLAYER > L; // layer array, L(1) is outside layer
		Array1D< CFSGAP > G; // gap array, G(1) is outside-most, betw L(1) and L(2)
		bool ISControlled; // CFS is not controlled, or has no controlled VB layer

		// Default Constructor
		CFSTY() :
			NL( 0 ),
			L( CFSMAXNL ),
			G( CFSMAXNL-1 ),
			ISControlled( false )
		{}

	};

	// Object Data
	extern CFSSWP SWP_ROOMBLK; // Solar reflectance, BEAM-BEAM, front | Solar reflectance, BEAM-BEAM, back | Solar transmittance, BEAM-BEAM, front | Solar transmittance, BEAM-BEAM, back | Solar reflectance, BEAM-DIFFUSE, front | Solar reflectance, BEAM-DIFFUSE, back | Solar transmittance, BEAM-DIFFUSE, front | Solar transmittance, BEAM-DIFFUSE, back | Solar reflectance, DIFFUSE-DIFFUSE, front | Solar reflectance, DIFFUSE-DIFFUSE, back | Solar transmittance, DIFFUSE-DIFFUSE
	extern Array1D< CFSLAYER > CFSLayers;
	extern Array1D< CFSTY > CFS;
	extern Array1D< CFSGAP > CFSGaps;

} // DataWindowEquivalentLayer

} // EnergyPlus

#endif
