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
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <ThermalISO15099Calc.hh>
#include <DataGlobals.hh>
#include <DataPrecisionGlobals.hh>
#include <TARCOGArgs.hh>
#include <TARCOGCommon.hh>
#include <TARCOGGasses90.hh>
#include <TARCOGGassesParams.hh>
#include <TARCOGOutput.hh>
#include <TARCOGParams.hh>
#include <TarcogShading.hh>

namespace EnergyPlus {

namespace ThermalISO15099Calc {
	//***********************************************************************
	// ThermalISO15099Calc: a TARCOG module
	//    module For Calculation of Thermal Performance Indices For Center
	//     of Glass According to ISO 15099
	// History of Revisions:
	//  Revision: 6.0.36  (June/22/2010)
	//   - Initial setup, extracted and refactored from TARCOG.for
	//***********************************************************************

	// MODULE INFORMATION:
	//       AUTHOR         D. Charlie Curcija
	//       DATE WRITTEN   July/2000
	//       MODIFIED       na
	//       RE-ENGINEERED  March/27/2012, Simon Vidanovic

	//  Revision: 7.0.13  (March/27/2012), Simon Vidanovic
	//   - feature: New set of equaitons is set instead of hhat coefficents and new approach to solution which improves
	//               speed and stability.  Note that this solution does not include laminates

	// PURPOSE OF THIS MODULE:
	// Module For Calculation of Thermal Performance Indices For Center
	//  of Glass According to ISO 15099

	// METHODOLOGY EMPLOYED:
	// <description>

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// USE STATEMENTS:

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace TARCOGGassesParams;
	using namespace TARCOGParams;
	using namespace TARCOGArgs;
	using namespace TARCOGCommon;
	using namespace TARCOGOutput;
	using namespace TARCOGGasses90;
	using namespace TarcogShading;

	// Data
	//private picard

	// Functions

	void
	film(
		Real64 const tex,
		Real64 const tw,
		Real64 const ws,
		int const iwd,
		Real64 & hcout,
		int const ibc
	)
	{
		//***********************************************************************
		// purpose - to find outdoor film coeff
		//***********************************************************************
		// Inputs -
		//   tex - outdoor air temp [k]
		//   tw - outside surface temp
		//   ws - wind speed [m/s]
		//   iwd - wind direction [0 - windward; 1 - leeward]
		// Outputs
		//   hcout - convective film coeff [w m-2 k-1]

		// Locals
		Real64 const conv( 5.6783 );

		Real64 vc;
		Real64 acoef;
		Real64 bexp;

		// calculation of convection component of exterior film coefficient using the :
		{ auto const SELECT_CASE_var( ibc );
		if ( SELECT_CASE_var == 0 ) { //ISO 15099
			hcout = 4.0 + 4.0 * ws;
		} else if ( SELECT_CASE_var == -1 ) { // old ASHRAE SPC142 correlation
			if ( iwd == 0 ) { // windward
				if ( ws > 2.0 ) {
					vc = 0.25 * ws;
				} else {
					vc = 0.5;
				}
			} else { // leeward
				vc = 0.3 + 0.05 * ws;
			}
			hcout = 3.28 * std::pow( vc, 0.605 );
			hcout *= conv; // convert to metric
		} else if ( SELECT_CASE_var == -2 ) { // Yazdanian-Klems correlation:
			if ( iwd == 0 ) { // windward
				acoef = 2.38;
				bexp = 0.89;
			} else { // leeward
				acoef = 2.86;
				bexp = 0.617;
			}
			hcout = std::sqrt( pow_2( 0.84 * std::pow( tw - tex, 0.33 ) ) + pow_2( acoef * std::pow( ws, bexp ) ) );
		} else if ( SELECT_CASE_var == -3 ) { // Kimura correlation (Section 8.4.2.3 in ISO 15099-2001):
			if ( iwd == 0 ) { // windward
				if ( ws > 2.0 ) {
					vc = 0.25 * ws;
				} else {
					vc = 0.5 * ws;
				}
			} else { // leeward
				vc = 0.3 + 0.05 * ws;
			}
			hcout = 4.7 + 7.6 * vc;
		}}

	}

	void
	Calc_ISO15099(
		int const nlayer,
		int const iwd,
		Real64 & tout,
		Real64 & tind,
		Real64 & trmin,
		Real64 const wso,
		Real64 const wsi,
		Real64 const dir,
		Real64 const outir,
		int const isky,
		Real64 const tsky,
		Real64 & esky,
		Real64 const fclr,
		Real64 const VacuumPressure,
		Real64 const VacuumMaxGapThickness,
		Array1A< Real64 > gap,
		Array1A< Real64 > thick,
		Array1A< Real64 > scon,
		Array1A< Real64 > const tir,
		Array1A< Real64 > const emis,
		Real64 const totsol,
		Real64 const tilt,
		Array1A< Real64 > const asol,
		Real64 const height,
		Real64 const heightt,
		Real64 const width,
		Array1A< Real64 > const presure,
		Array2A_int const iprop,
		Array2A< Real64 > const frct,
		Array2A< Real64 > const xgcon,
		Array2A< Real64 > const xgvis,
		Array2A< Real64 > const xgcp,
		Array1A< Real64 > const xwght,
		Array1A< Real64 > const gama,
		Array1A_int const nmix,
		Array1A_int const SupportPillar, // Shows whether or not gap have support pillar
		Array1A< Real64 > const PillarSpacing, // Pillar spacing for each gap (used in case there is support pillar)
		Array1A< Real64 > const PillarRadius, // Pillar radius for each gap (used in case there is support pillar)
		Array1A< Real64 > theta,
		Array1A< Real64 > q,
		Array1A< Real64 > qv,
		Real64 & ufactor,
		Real64 & sc,
		Real64 & hflux,
		Real64 & hcin,
		Real64 & hcout,
		Real64 & hrin,
		Real64 & hrout,
		Real64 & hin,
		Real64 & hout,
		Array1A< Real64 > hcgas,
		Array1A< Real64 > hrgas,
		Real64 & shgc,
		int & nperr,
		std::string & ErrorMessage,
		Real64 & shgct,
		Real64 & tamb,
		Real64 & troom,
		Array1A_int const ibc,
		Array1A< Real64 > const Atop,
		Array1A< Real64 > const Abot,
		Array1A< Real64 > const Al,
		Array1A< Real64 > const Ar,
		Array1A< Real64 > const Ah,
		Array1A< Real64 > const SlatThick,
		Array1A< Real64 > const SlatWidth,
		Array1A< Real64 > const SlatAngle,
		Array1A< Real64 > const SlatCond,
		Array1A< Real64 > const SlatSpacing,
		Array1A< Real64 > const SlatCurve,
		Array1A< Real64 > const vvent,
		Array1A< Real64 > const tvent,
		Array1A_int const LayerType,
		Array1A_int const nslice,
		Array1A< Real64 > const LaminateA,
		Array1A< Real64 > const LaminateB,
		Array1A< Real64 > const sumsol,
		Array1A< Real64 > Ra,
		Array1A< Real64 > Nu,
		int const ThermalMod,
		int const Debug_mode, // Switch for debug output files:
		Real64 & ShadeEmisRatioOut,
		Real64 & ShadeEmisRatioIn,
		Real64 & ShadeHcRatioOut,
		Real64 & ShadeHcRatioIn,
		Real64 & HcUnshadedOut,
		Real64 & HcUnshadedIn,
		Array1A< Real64 > Keff,
		Array1A< Real64 > ShadeGapKeffConv,
		Real64 const SDScalar,
		int const SHGCCalc, // SHGC calculation switch:
		int & NumOfIterations
	)
	{

		/// function attributes:

		/// INPUTS:

		/// General:

		// Argument array dimensioning
		gap.dim( MaxGap );
		thick.dim( maxlay );
		scon.dim( maxlay );
		tir.dim( maxlay2 );
		emis.dim( maxlay2 );
		asol.dim( maxlay );
		presure.dim( maxlay1 );
		iprop.dim( maxgas, maxlay1 );
		frct.dim( maxgas, maxlay1 );
		xgcon.dim( 3, maxgas );
		xgvis.dim( 3, maxgas );
		xgcp.dim( 3, maxgas );
		xwght.dim( maxgas );
		gama.dim( maxgas );
		nmix.dim( maxlay1 );
		SupportPillar.dim( maxlay );
		PillarSpacing.dim( maxlay );
		PillarRadius.dim( maxlay );
		theta.dim( maxlay2 );
		q.dim( maxlay3 );
		qv.dim( maxlay1 );
		hcgas.dim( maxlay1 );
		hrgas.dim( maxlay1 );
		ibc.dim( 2 );
		Atop.dim( maxlay );
		Abot.dim( maxlay );
		Al.dim( maxlay );
		Ar.dim( maxlay );
		Ah.dim( maxlay );
		SlatThick.dim( maxlay );
		SlatWidth.dim( maxlay );
		SlatAngle.dim( maxlay );
		SlatCond.dim( maxlay );
		SlatSpacing.dim( maxlay );
		SlatCurve.dim( maxlay );
		vvent.dim( maxlay1 );
		tvent.dim( maxlay1 );
		LayerType.dim( maxlay );
		nslice.dim( maxlay );
		LaminateA.dim( maxlay );
		LaminateB.dim( maxlay );
		sumsol.dim( maxlay );
		Ra.dim( maxlay );
		Nu.dim( maxlay );
		Keff.dim( maxlay );
		ShadeGapKeffConv.dim( MaxGap );

		// Locals
		//    0 - do not perform SHGC calculations
		//    1 - perform SHGC calculations
		//    0 - don't create debug output files
		//    1 - append results to existing debug output file
		//    2 - store results in new debug output file
		//   3 - save in-between results (in all iterations) to existing debug file

		/// Environment related:

		/// Layers:

		/// Venetians:

		/// Laminates:

		/// Gaps:

		// 0 - does not have support pillar
		// 1 - have support pillar

		//// INPUTS/OUTPUTS:

		/// OUTPUTS:
		/// Overall:

		/// Layers:

		/// Gaps:

		/// Shading related:

		// Variables

		static Array1D< Real64 > thetas( maxlay2 );
		static Array1D< Real64 > rir( maxlay2 );
		static Array1D< Real64 > hcgass( maxlay1 );
		static Array1D< Real64 > hrgass( maxlay1 );
		static Array1D< Real64 > rs( maxlay3, 0.0 );

		//  REAL(r64) :: grho(maxgas,3)
		static Array1D< Real64 > qs( maxlay3 );
		static Array1D< Real64 > qvs( maxlay1 );
		static Array1D< Real64 > LaminateAU( maxlay );
		static Array1D< Real64 > sumsolU( maxlay );
		static Array1D< Real64 > sol0( maxlay );
		Real64 shgct_NOSD;
		Real64 trmout;

		Real64 Gout;
		Real64 Gin;
		Real64 AchievedErrorTolerance;
		Real64 AchievedErrorToleranceSolar;
		int NumOfIter;
		int NumOfIterSolar;

		Real64 tgg;
		Real64 qc1;
		Real64 qc2;
		Real64 qcgg;
		static Array1D< Real64 > qcgas( maxlay1 );
		static Array1D< Real64 > qcgaps( maxlay1 );
		static Array1D< Real64 > qrgas( maxlay1 );
		static Array1D< Real64 > qrgaps( maxlay1 );

		Real64 ShadeHcModifiedOut;
		Real64 ShadeHcModifiedIn;

		//REAL(r64) :: xgrho(maxgas, 3)   !!!!!!!!!!!!!!!!!1

		//cbi...Variables for "unshaded" run:

		bool NeedUnshadedRun;
		int nlayer_NOSD;
		Real64 AchievedErrorTolerance_NOSD;
		int NumOfIter_NOSD;
		static Array1D< Real64 > Atop_NOSD( maxlay );
		static Array1D< Real64 > Abot_NOSD( maxlay );
		static Array1D< Real64 > Al_NOSD( maxlay );
		static Array1D< Real64 > Ar_NOSD( maxlay );
		static Array1D< Real64 > Ah_NOSD( maxlay );
		static Array1D< Real64 > SlatThick_NOSD( maxlay );
		static Array1D< Real64 > SlatWidth_NOSD( maxlay );
		static Array1D< Real64 > SlatAngle_NOSD( maxlay );
		static Array1D< Real64 > SlatCond_NOSD( maxlay );
		static Array1D< Real64 > SlatSpacing_NOSD( maxlay );
		static Array1D< Real64 > SlatCurve_NOSD( maxlay );
		static Array1D< Real64 > vvent_NOSD( maxlay1 );
		static Array1D< Real64 > tvent_NOSD( maxlay1 );
		static Array1D< Real64 > qv_NOSD( maxlay1 );
		static Array1D< Real64 > q_NOSD( maxlay3 );
		Real64 hin_NOSD;
		Real64 flux_NOSD;
		Real64 hcin_NOSD;
		Real64 hrin_NOSD;
		Real64 hcout_NOSD;
		Real64 hrout_NOSD;
		Real64 tamb_NOSD;
		Real64 troom_NOSD;
		static Array1D_int LayerType_NOSD( maxlay );
		Real64 ufactor_NOSD;
		Real64 sc_NOSD;
		Real64 hflux_NOSD;
		Real64 shgc_NOSD;
		Real64 hout_NOSD;
		static Array1D< Real64 > gap_NOSD( maxlay );
		static Array1D< Real64 > thick_NOSD( maxlay );
		static Array1D< Real64 > scon_NOSD( maxlay );
		static Array1D< Real64 > emis_NOSD( maxlay2 );
		static Array1D< Real64 > rir_NOSD( maxlay2 );
		static Array1D< Real64 > tir_NOSD( maxlay2 );
		static Array1D< Real64 > theta_NOSD( maxlay2 );
		static Array2D< Real64 > frct_NOSD( maxgas, maxlay1 );
		static Array2D_int iprop_NOSD( maxgas, maxlay1 );
		static Array1D_int nmix_NOSD( maxlay1 );
		static Array1D< Real64 > presure_NOSD( maxlay1 );
		static Array1D< Real64 > hcgas_NOSD( maxlay1 );
		static Array1D< Real64 > hrgas_NOSD( maxlay1 );
		//REAL(r64) ::  rs_NOSD(maxlay3)!,sol(maxlay)
		static Array1D< Real64 > LaminateA_NOSD( maxlay );
		static Array1D< Real64 > LaminateB_NOSD( maxlay );
		static Array1D< Real64 > sumsol_NOSD( maxlay );
		static Array1D< Real64 > Ra_NOSD( maxlay );
		static Array1D< Real64 > Nu_NOSD( maxlay );
		Real64 ShadeEmisRatioOut_NOSD;
		Real64 ShadeEmisRatioIn_NOSD;
		Real64 ShadeHcRatioOut_NOSD;
		Real64 ShadeHcRatioIn_NOSD;
		Real64 ShadeHcModifiedOut_NOSD;
		Real64 ShadeHcModifiedIn_NOSD;
		static Array1D< Real64 > Ebb( maxlay );
		static Array1D< Real64 > Ebf( maxlay );
		static Array1D< Real64 > Rb( maxlay );
		static Array1D< Real64 > Rf( maxlay );
		static Array1D< Real64 > Ebbs( maxlay );
		static Array1D< Real64 > Ebfs( maxlay );
		static Array1D< Real64 > Rbs( maxlay );
		static Array1D< Real64 > Rfs( maxlay );
		static Array1D< Real64 > Ebb_NOSD( maxlay );
		static Array1D< Real64 > Ebf_NOSD( maxlay );
		static Array1D< Real64 > Rb_NOSD( maxlay );
		static Array1D< Real64 > Rf_NOSD( maxlay );

		static Array1D< Real64 > ShadeGapKeffConv_NOSD( MaxGap );
		static Array1D< Real64 > qcgas_NOSD( maxlay1 );
		static Array1D< Real64 > Keff_NOSD( maxlay1 );
		static Array1D< Real64 > qrgas_NOSD( maxlay1 );
		static Array1D_int nslice_NOSD( maxlay );
		static Array1D< Real64 > vfreevent_NOSD( maxlay1 );

		int FirstSpecularLayer;
		int LastSpecularLayer;

		static Array1D< Real64 > vfreevent( maxlay1 );

		//cbi...Other variables:
		Real64 flux;
		Real64 hint;
		Real64 houtt;
		Real64 ebsky;
		Real64 ebroom;
		int i;
		int j;
		int OriginalIndex;
		int UnshadedDebug;
		static Real64 rtot( 0.0 );
		static Real64 sft( 0.0 );
		static Real64 hcins( 0.0 );
		static Real64 hrins( 0.0 );
		static Real64 hins( 0.0 );
		static Real64 hcouts( 0.0 );
		static Real64 hrouts( 0.0 );
		static Real64 houts( 0.0 );
		static Real64 ufactors( 0.0 );
		static Real64 fluxs( 0.0 );
		static Real64 qeff( 0.0 );
		static Real64 flux_nonsolar( 0.0 );
		static gio::Fmt fmtLD( "*" );

		//Autodesk:Uninit Initialize variables used uninitialized
		shgc_NOSD = 0.0; //Autodesk:Uninit Force default initialization
		sc_NOSD = 0.0; //Autodesk:Uninit Force default initialization
		hflux_NOSD = 0.0; //Autodesk:Uninit Force default initialization
		ShadeHcRatioIn_NOSD = 0.0; //Autodesk:Uninit Force default initialization
		ShadeHcRatioOut_NOSD = 0.0; //Autodesk:Uninit Force default initialization

		AchievedErrorTolerance = 0.0;
		AchievedErrorToleranceSolar = 0.0;
		AchievedErrorTolerance_NOSD = 0.0;

		PrepVariablesISO15099( nlayer, tout, tind, trmin, isky, outir, tsky, esky, fclr, gap, thick, scon, tir, emis, tilt, hin, hout, ibc, SlatThick, SlatWidth, SlatAngle, SlatCond, LayerType, ThermalMod, SDScalar, ShadeEmisRatioOut, ShadeEmisRatioIn, ShadeHcRatioOut, ShadeHcRatioIn, Keff, ShadeGapKeffConv, sc, shgc, ufactor, flux, LaminateAU, sumsolU, sol0, hint, houtt, trmout, ebsky, ebroom, Gout, Gin, rir, vfreevent, nperr, ErrorMessage );

		//No option to take hardcoded variables.  All gas coefficients are now passed from outside.
		//if (GoAhead(nperr)) call propcon90(ISO15099,mgas,xgcon,xgvis,xgcp,xgrho,xwght,nperr)

		// exit on error
		if ( ! ( GoAhead( nperr ) ) ) return;

		//bi...Write intermediate results to output file:
		if ( WriteDebugOutput ) {
			WriteModifiedArguments( InArgumentsFile, DBGD, esky, trmout, trmin, ebsky, ebroom, Gout, Gin, nlayer, LayerType, nmix, frct, thick, scon, gap, xgcon, xgvis, xgcp, xwght );
		}

		//cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
		//     This is "solar radiation" pass
		//cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

		//This is main calculation in case UFactor calculations will not be performed
		if ( ( dir > 0.0 ) || ( SHGCCalc == 0 ) ) {
			// call therm1d to calculate heat flux with solar radiation

			therm1d( nlayer, iwd, tout, tind, wso, wsi, VacuumPressure, VacuumMaxGapThickness, dir, ebsky, Gout, trmout, trmin, ebroom, Gin, tir, rir, emis, gap, thick, scon, tilt, asol, height, heightt, width, iprop, frct, presure, nmix, xwght, xgcon, xgvis, xgcp, gama, SupportPillar, PillarSpacing, PillarRadius, theta, q, qv, flux, hcin, hrin, hcout, hrout, hin, hout, hcgas, hrgas, ufactor, nperr, ErrorMessage, tamb, troom, ibc, Atop, Abot, Al, Ar, Ah, vvent, tvent, LayerType, Ra, Nu, vfreevent, qcgas, qrgas, Ebf, Ebb, Rf, Rb, ShadeEmisRatioOut, ShadeEmisRatioIn, ShadeHcModifiedOut, ShadeHcModifiedIn, ThermalMod, Debug_mode, AchievedErrorToleranceSolar, NumOfIterSolar );

			NumOfIterations = NumOfIterSolar;
			//exit on error:

			if ( nlayer > 1 ) {
				for ( i = 1; i <= nlayer - 1; ++i ) {
					Keff( i ) = gap( i ) * q( 2 * i + 1 ) / ( theta( 2 * i + 1 ) - theta( 2 * i ) );
					if ( IsShadingLayer( LayerType( i ) ) ) {
						Keff( i ) = gap( i ) * q( 2 * i + 1 ) / ( theta( 2 * i + 1 ) - theta( 2 * i ) );
					}
					if ( IsShadingLayer( LayerType( i + 1 ) ) ) {
						Keff( i ) = gap( i ) * q( 2 * i + 1 ) / ( theta( 2 * i + 1 ) - theta( 2 * i ) );
					}
				}
			}

			if ( ! ( GoAhead( nperr ) ) ) return;

			//No need to store results in case of non-ufactor run
			if ( ( SHGCCalc > 0 ) && ( dir > 0.0 ) ) {
				solarISO15099( totsol, rtot, rs, nlayer, asol, sft );
				shgct = sft;
				shgct_NOSD = 0.0;
				hcins = hcin;
				hrins = hrin;
				hins = hin;
				hcouts = hcout;
				hrouts = hrout;
				houts = hout;
				ufactors = ufactor;
				fluxs = flux;
				for ( i = 1; i <= nlayer; ++i ) {
					thetas( 2 * i - 1 ) = theta( 2 * i - 1 );
					thetas( 2 * i ) = theta( 2 * i );
					Ebbs( i ) = Ebb( i );
					Ebfs( i ) = Ebf( i );
					Rbs( i ) = Rb( i );
					Rfs( i ) = Rf( i );
					qs( 2 * i - 1 ) = q( 2 * i - 1 );
					qs( 2 * i ) = q( 2 * i );
					//qprims(2*i - 1) = qprim(2*i - 1)
					//qprims(2*i) = qprim(2*i)
					qvs( 2 * i - 1 ) = qv( 2 * i - 1 );
					qvs( 2 * i ) = qv( 2 * i );
					hcgass( i ) = hcgas( i );
					hrgass( i ) = hrgas( i );
					qrgaps( i ) = qrgas( i );
					qcgaps( i ) = qcgas( i );
				}
				//    CHECK THIS!
				qs( 2 * nlayer + 1 ) = q( 2 * nlayer + 1 );
			} //if (UFactorCalc.gt.0) then

		}

		//No solar radiation pass is not needed to be calculated
		//if ((SHGCCalc.gt.0).or.(dir.eq.0)) then
		if ( SHGCCalc > 0 ) {

			//cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
			//      This is "no solar radiation" pass
			//cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

			hin = hint;
			hout = houtt;

			// call therm1d to calculate heat flux without solar radiation
			therm1d( nlayer, iwd, tout, tind, wso, wsi, VacuumPressure, VacuumMaxGapThickness, 0.0, ebsky, Gout, trmout, trmin, ebroom, Gin, tir, rir, emis, gap, thick, scon, tilt, sol0, height, heightt, width, iprop, frct, presure, nmix, xwght, xgcon, xgvis, xgcp, gama, SupportPillar, PillarSpacing, PillarRadius, theta, q, qv, flux, hcin, hrin, hcout, hrout, hin, hout, hcgas, hrgas, ufactor, nperr, ErrorMessage, tamb, troom, ibc, Atop, Abot, Al, Ar, Ah, vvent, tvent, LayerType, Ra, Nu, vfreevent, qcgas, qrgas, Ebf, Ebb, Rf, Rb, ShadeEmisRatioOut, ShadeEmisRatioIn, ShadeHcModifiedOut, ShadeHcModifiedIn, ThermalMod, Debug_mode, AchievedErrorTolerance, NumOfIter );

			NumOfIterations = NumOfIter;

			//exit on error:
			if ( ! ( GoAhead( nperr ) ) ) return;

			//bi...Keep hcout, hcin in case this is an unshaded system:
			HcUnshadedOut = hcout;
			HcUnshadedIn = hcin;

			//bi...do an Unshaded run if necessary (Uvalue/Winter conditions):
			//bi...Prepare variables for UNSHADED (NO SD) run:

			NeedUnshadedRun = false;
			FirstSpecularLayer = 1;
			LastSpecularLayer = nlayer;
			nlayer_NOSD = nlayer;
			if ( IsShadingLayer( LayerType( 1 ) ) ) {
				--nlayer_NOSD;
				FirstSpecularLayer = 2;
				NeedUnshadedRun = true;
			}

			//  if (LayerType(nlayer).eq.VENETBLIND) then
			if ( IsShadingLayer( LayerType( nlayer ) ) ) {
				--nlayer_NOSD;
				LastSpecularLayer = nlayer - 1;
				NeedUnshadedRun = true;
			}

			// no unshaded run for now
			NeedUnshadedRun = false;
			//bi...Set outdoor & indoor gas properties:
			if ( NeedUnshadedRun ) {
				nmix_NOSD( 1 ) = nmix( 1 );
				presure_NOSD( 1 ) = presure( 1 );
				nmix_NOSD( nlayer_NOSD + 1 ) = nmix( nlayer + 1 );
				presure_NOSD( nlayer_NOSD + 1 ) = presure( nlayer + 1 );
				for ( j = 1; j <= nmix( 1 ); ++j ) {
					iprop_NOSD( j, 1 ) = iprop( j, 1 );
					frct_NOSD( j, 1 ) = frct( j, 1 );
				}
				for ( j = 1; j <= nmix( nlayer_NOSD + 1 ); ++j ) {
					iprop_NOSD( j, nlayer_NOSD + 1 ) = iprop( j, nlayer + 1 );
					frct_NOSD( j, nlayer_NOSD + 1 ) = frct( j, nlayer + 1 );
				}
				for ( i = 1; i <= nlayer_NOSD; ++i ) {
					OriginalIndex = FirstSpecularLayer + i - 1;
					Atop_NOSD( i ) = Atop( OriginalIndex );
					Abot_NOSD( i ) = Abot( OriginalIndex );
					Al_NOSD( i ) = Al( OriginalIndex );
					Ar_NOSD( i ) = Ar( OriginalIndex );
					Ah_NOSD( i ) = Ah( OriginalIndex );

					SlatThick_NOSD( i ) = SlatThick( OriginalIndex );
					SlatWidth_NOSD( i ) = SlatWidth( OriginalIndex );
					SlatAngle_NOSD( i ) = SlatAngle( OriginalIndex );
					SlatCond_NOSD( i ) = SlatCond( OriginalIndex );
					SlatSpacing_NOSD( i ) = SlatSpacing( OriginalIndex );
					SlatCurve_NOSD( i ) = SlatCurve( OriginalIndex );

					//cbi...    TO do when Forced Ventilation is implemented: take care of appropriate arguments!!!
					//      vvent_NOSD
					//      tvent_NOSD

					LayerType_NOSD( i ) = LayerType( OriginalIndex );

					thick_NOSD( i ) = thick( OriginalIndex );
					scon_NOSD( i ) = scon( OriginalIndex );
					tir_NOSD( 2 * i - 1 ) = tir( 2 * OriginalIndex - 1 );
					emis_NOSD( 2 * i - 1 ) = emis( 2 * OriginalIndex - 1 );
					emis_NOSD( 2 * i ) = emis( 2 * OriginalIndex );
					rir_NOSD( 2 * i - 1 ) = rir( 2 * OriginalIndex - 1 );
					rir_NOSD( 2 * i ) = rir( 2 * OriginalIndex );

					gap_NOSD( i ) = gap( OriginalIndex );

					if ( i < nlayer_NOSD ) {
						nmix_NOSD( i + 1 ) = nmix( OriginalIndex + 1 );
						presure_NOSD( i + 1 ) = presure( OriginalIndex + 1 );
						for ( j = 1; j <= nmix_NOSD( i + 1 ); ++j ) {
							iprop_NOSD( j, i + 1 ) = iprop( j, OriginalIndex + 1 );
							frct_NOSD( j, i + 1 ) = frct( j, OriginalIndex + 1 );
						}
					}

					LaminateA_NOSD( i ) = LaminateA( OriginalIndex );
					LaminateB_NOSD( i ) = LaminateB( OriginalIndex );
					sumsol_NOSD( i ) = sumsol( OriginalIndex );

					nslice_NOSD( i ) = nslice( OriginalIndex );

				}

				//    This is UNSHADED pass - no solar radiation:
				hin_NOSD = hint;
				hout_NOSD = houtt;

				//Simon: Removed unshaded debug output for now
				UnshadedDebug = 0;
				if ( WriteDebugOutput && ( UnshadedDebug == 1 ) ) {
					FilePosition = "APPEND";
					// InArgumentsFile should already be open
					//open(unit=InArgumentsFile,  file=TRIM(DBGD)//DebugOutputFileName,  status='unknown', position=FilePosition,  &
					//     form='formatted', iostat=nperr)

					//if (nperr.ne.0)  open(unit=InArgumentsFile,  file=DebugOutputFileName,  status='unknown', position=FilePosition, &
					//    form='formatted', iostat=nperr)
					gio::write( InArgumentsFile, fmtLD );
					gio::write( InArgumentsFile, fmtLD ) << "UNSHADED RUN:";
					gio::write( InArgumentsFile, fmtLD );
					//close(InArgumentsFile)

					WriteInputArguments( tout, tind, trmin, wso, iwd, wsi, dir, outir, isky, tsky, esky, fclr, VacuumPressure, VacuumMaxGapThickness, ibc, hout_NOSD, hin_NOSD, ISO15099, ThermalMod, SDScalar, height, heightt, width, tilt, totsol, nlayer_NOSD, LayerType_NOSD, thick_NOSD, scon_NOSD, asol, tir_NOSD, emis_NOSD, Atop_NOSD, Abot_NOSD, Al_NOSD, Ar_NOSD, Ah_NOSD, SlatThick_NOSD, SlatWidth_NOSD, SlatAngle_NOSD, SlatCond_NOSD, SlatSpacing_NOSD, SlatCurve_NOSD, nslice_NOSD, LaminateA_NOSD, LaminateB_NOSD, sumsol_NOSD, gap_NOSD, vvent_NOSD, tvent_NOSD, presure_NOSD, nmix_NOSD, iprop_NOSD, frct_NOSD, xgcon, xgvis, xgcp, xwght );

				} // end if UnshadedDebug = 1

				//cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
				//      This is "Unshaded, No solar radiation" pass
				//cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
				// call therm1d to calculate heat flux with solar radiation
				therm1d( nlayer_NOSD, iwd, tout, tind, wso, wsi, VacuumPressure, VacuumMaxGapThickness, 0.0, ebsky, Gout, trmout, trmin, ebroom, Gin, tir_NOSD, rir_NOSD, emis_NOSD, gap_NOSD, thick_NOSD, scon_NOSD, tilt, sol0, height, heightt, width, iprop_NOSD, frct_NOSD, presure_NOSD, nmix_NOSD, xwght, xgcon, xgvis, xgcp, gama, SupportPillar, PillarSpacing, PillarRadius, theta_NOSD, q_NOSD, qv_NOSD, flux_NOSD, hcin_NOSD, hrin_NOSD, hcout_NOSD, hrout_NOSD, hin_NOSD, hout_NOSD, hcgas_NOSD, hrgas_NOSD, ufactor_NOSD, nperr, ErrorMessage, tamb_NOSD, troom_NOSD, ibc, Atop_NOSD, Abot_NOSD, Al_NOSD, Ar_NOSD, Ah_NOSD, vvent_NOSD, tvent_NOSD, LayerType_NOSD, Ra_NOSD, Nu_NOSD, vfreevent_NOSD, qcgas_NOSD, qrgas_NOSD, Ebf_NOSD, Ebb_NOSD, Rf_NOSD, Rb_NOSD, ShadeEmisRatioOut_NOSD, ShadeEmisRatioIn_NOSD, ShadeHcModifiedOut_NOSD, ShadeHcModifiedIn_NOSD, ThermalMod, Debug_mode, AchievedErrorTolerance_NOSD, NumOfIter_NOSD );

				NumOfIterations = NumOfIter_NOSD;
				// exit on error
				if ( ! ( GoAhead( nperr ) ) ) return;

				//bi...  Keep these values:
				HcUnshadedOut = hcout_NOSD;
				HcUnshadedIn = hcin_NOSD;

				ShadeHcRatioOut = ShadeHcModifiedOut / HcUnshadedOut;
				ShadeHcRatioIn = ShadeHcModifiedIn / HcUnshadedIn;

				//bi...unshaded results:
				if ( WriteDebugOutput && ( UnshadedDebug == 1 ) ) {
					WriteOutputArguments( OutArgumentsFile, DBGD, nlayer_NOSD, tamb, q_NOSD, qv_NOSD, qcgas_NOSD, qrgas_NOSD, theta_NOSD, vfreevent_NOSD, vvent_NOSD, Keff_NOSD, ShadeGapKeffConv_NOSD, troom_NOSD, ufactor_NOSD, shgc_NOSD, sc_NOSD, hflux_NOSD, shgct_NOSD, hcin_NOSD, hrin_NOSD, hcout_NOSD, hrout_NOSD, Ra_NOSD, Nu_NOSD, LayerType_NOSD, Ebf_NOSD, Ebb_NOSD, Rf_NOSD, Rb_NOSD, ebsky, Gout, ebroom, Gin, ShadeEmisRatioIn_NOSD, ShadeEmisRatioOut_NOSD, ShadeHcRatioIn_NOSD, ShadeHcRatioOut_NOSD, hcin_NOSD, hcout_NOSD, hcgas_NOSD, hrgas_NOSD, AchievedErrorTolerance_NOSD, NumOfIter_NOSD ); //Autodesk:Uninit shgc_NOSD, sc_NOSD, hflux_NOSD, ShadeHcRatioIn_NOSD, ShadeHcRatioOut_NOSD were uninitialized
				} // end if UnshadedDebug = 1
			} // end if NeedUnshadedRun...

			//bi Set T6-related quantities keff, keffc: (using non-solar pass results)
			if ( nlayer > 1 ) {
				for ( i = 1; i <= nlayer - 1; ++i ) {
					Keff( i ) = gap( i ) * q( 2 * i + 1 ) / ( theta( 2 * i + 1 ) - theta( 2 * i ) );
					if ( IsShadingLayer( LayerType( i ) ) ) {
						Keff( i ) = gap( i ) * q( 2 * i + 1 ) / ( theta( 2 * i + 1 ) - theta( 2 * i ) );
					}
					if ( IsShadingLayer( LayerType( i + 1 ) ) ) {
						Keff( i ) = gap( i ) * q( 2 * i + 1 ) / ( theta( 2 * i + 1 ) - theta( 2 * i ) );
					}
					if ( IsShadingLayer( LayerType( i ) ) ) {
						//Keff(i)   = gap(i)   * qprim(2*i+1) / (theta(2*i+1) - theta(2*i))
						if ( ( i > 1 ) && ( i < nlayer ) ) {
							tgg = gap( i - 1 ) + gap( i ) + thick( i );
							qc1 = qcgas( i - 1 );
							qc2 = qcgas( i );
							qcgg = ( qc1 + qc2 ) / 2.0;
							ShadeGapKeffConv( i ) = tgg * qcgg / ( theta( 2 * i + 1 ) - theta( 2 * i - 2 ) );
						}
					}
				}
			}

		} //if (UFactorCalc.ne.0) then

		//bi...  For debugging purposes:
		qeff = ufactor * std::abs( tout - tind );
		flux_nonsolar = flux;

		if ( ( SHGCCalc > 0 ) && ( dir > 0.0 ) ) {
			shgc = totsol - ( fluxs - flux ) / dir;
			sc = shgc / 0.87;
			hcin = hcins;
			hrin = hrins;
			hin = hins;
			hcout = hcouts;
			hrout = hrouts;
			hout = houts;
			flux = fluxs; // <--- ???
			for ( i = 1; i <= nlayer; ++i ) {
				theta( 2 * i - 1 ) = thetas( 2 * i - 1 );
				theta( 2 * i ) = thetas( 2 * i );
				Ebb( i ) = Ebbs( i );
				Ebf( i ) = Ebfs( i );
				Rb( i ) = Rbs( i );
				Rf( i ) = Rfs( i );
				q( 2 * i - 1 ) = qs( 2 * i - 1 );
				q( 2 * i ) = qs( 2 * i );
				//qprim(2*i - 1) = qprims(2*i - 1)
				//qprim(2*i) = qprims(2*i)
				qv( 2 * i - 1 ) = qvs( 2 * i - 1 );
				qv( 2 * i ) = qvs( 2 * i );
				hcgas( i ) = hcgass( i );
				hrgas( i ) = hrgass( i );
				qcgas( i ) = qcgaps( i );
				qrgas( i ) = qrgaps( i );
				AchievedErrorTolerance = AchievedErrorToleranceSolar;
				NumOfIter = NumOfIterSolar;
			}

			// bi    CHECK THIS!
			q( 2 * nlayer + 1 ) = qs( 2 * nlayer + 1 );
		}

		hflux = flux; // save flux value for output table

		//bi...  Write results to debug output file:
		if ( WriteDebugOutput ) {
			WriteOutputArguments( OutArgumentsFile, DBGD, nlayer, tamb, q, qv, qcgas, qrgas, theta, vfreevent, vvent, Keff, ShadeGapKeffConv, troom, ufactor, shgc, sc, hflux, shgct, hcin, hrin, hcout, hrout, Ra, Nu, LayerType, Ebf, Ebb, Rf, Rb, ebsky, Gout, ebroom, Gin, ShadeEmisRatioIn, ShadeEmisRatioOut, ShadeHcRatioIn, ShadeHcRatioOut, HcUnshadedIn, HcUnshadedOut, hcgas, hrgas, AchievedErrorTolerance, NumOfIter );
		} // if WriteDebugOutput.eq.true - writing output file

	}

	void
	therm1d(
		int const nlayer,
		int const iwd,
		Real64 & tout,
		Real64 & tind,
		Real64 const wso,
		Real64 const wsi,
		Real64 const VacuumPressure,
		Real64 const VacuumMaxGapThickness,
		Real64 const dir,
		Real64 & ebsky,
		Real64 const Gout,
		Real64 const trmout,
		Real64 const trmin,
		Real64 & ebroom,
		Real64 const Gin,
		Array1< Real64 > const & tir,
		Array1< Real64 > const & rir,
		Array1< Real64 > const & emis,
		Array1< Real64 > const & gap,
		Array1< Real64 > const & thick,
		Array1< Real64 > const & scon,
		Real64 const tilt,
		Array1< Real64 > const & asol,
		Real64 const height,
		Real64 const heightt,
		Real64 const width,
		Array2_int const & iprop,
		Array2< Real64 > const & frct,
		Array1< Real64 > const & presure,
		Array1_int const & nmix,
		Array1< Real64 > const & wght,
		Array2< Real64 > const & gcon,
		Array2< Real64 > const & gvis,
		Array2< Real64 > const & gcp,
		Array1< Real64 > const & gama,
		Array1_int const & SupportPillar,
		Array1< Real64 > const & PillarSpacing,
		Array1< Real64 > const & PillarRadius,
		Array1< Real64 > & theta,
		Array1< Real64 > & q,
		Array1< Real64 > & qv,
		Real64 & flux,
		Real64 & hcin,
		Real64 & hrin,
		Real64 & hcout,
		Real64 & hrout,
		Real64 & hin,
		Real64 & hout,
		Array1< Real64 > & hcgas,
		Array1< Real64 > & hrgas,
		Real64 & ufactor,
		int & nperr,
		std::string & ErrorMessage,
		Real64 & tamb,
		Real64 & troom,
		Array1_int const & ibc,
		Array1< Real64 > const & Atop,
		Array1< Real64 > const & Abot,
		Array1< Real64 > const & Al,
		Array1< Real64 > const & Ar,
		Array1< Real64 > const & Ah,
		Array1< Real64 > const & vvent,
		Array1< Real64 > const & tvent,
		Array1_int const & LayerType,
		Array1< Real64 > & Ra,
		Array1< Real64 > & Nu,
		Array1< Real64 > & vfreevent,
		Array1< Real64 > & qcgas,
		Array1< Real64 > & qrgas,
		Array1< Real64 > & Ebf,
		Array1< Real64 > & Ebb,
		Array1< Real64 > & Rf,
		Array1< Real64 > & Rb,
		Real64 & ShadeEmisRatioOut,
		Real64 & ShadeEmisRatioIn,
		Real64 & ShadeHcModifiedOut,
		Real64 & ShadeHcModifiedIn,
		int const ThermalMod,
		int const Debug_mode, // Switch for debug output files:
		Real64 & AchievedErrorTolerance,
		int & TotalIndex
	)
	{
		//********************************************************************************
		// Main subroutine for calculation of 1-D heat transfer in the center of glazing.
		//********************************************************************************
		// Inputs
		//   nlayer    number of solid layers
		//   iwd   wind direction
		//   tout  outside temp in k
		//   tind  inside temp in k
		//   wso   wind speed in m/s
		//   wsi   inside forced air speed m/s
		//   Ebsky     ir flux from outside
		//   Gout  back facing radiosity from outside
		//   Trmout    Mean outdoor radiant temperature
		//   Trmin     Mean indoor radiant temperature
		//   Ebroom    ir flux from room
		//   Gin   front facing radiosity from room
		//   tir   ir transmittance of each layer
		//   rir   ir reflectance of each surface
		//   emis  ir emittances of each surface
		//   gap   array of gap widths in meters
		//   thick     thickness of glazing layers (m)
		//   scon  Vector of conductivities of 'glazing' layers
		//   tilt  Window tilt (deg). vert: tilt=90, hor out up: tilt=0, hor out down: tilt=180
		//   sol   absorbed solar energy for each layer in w/m2
		//   height    glazing cavity height
		//   heightt
		//   iprop
		//   frct
		//   presure
		//   nmix  vector of number of gasses in a mixture for each gap
		//   hin  convective indoor film coefficient (if non-zero hin input)
		//   hout     convective outdoor film coeff. (if non-zero hout input)
		// outputs
		//   theta     temp distribution in k
		//   flux  net heat flux between room and window
		//   rtot  overall thermal resistance
		//   rs    ?
		//   hcin  convective indoor film coeff.
		//   hrin  radiative part of indoor film coeff.
		//   hcout     convective outdoor film coeff.
		//   hrout     radiative part of outdoor film coeff.
		//   hin   convective indoor film coefficient
		//   hout  convective outdoor film coeff.
		//   ufactor   overall u-factor
		//   qcgap     vector of convective/conductive parts of flux in gaps
		//   qrgap     vector of radiative parts of flux in gaps
		//   nperr
		// *Inactives**
		//   wa - window azimuth (degrees, clockwise from south)
		//   hgas  matrix of gap film coefficients
		// Locals
		//   Ebb   Vector
		//   Ebf   Vector
		//   Rb    Vector
		//   Rf    Vector
		//   a     Array
		//   b     Array
		//   hhat  Vector
		//   err   iteration tolerance
		//   dtmax     max temp dfference after iteration
		//   index     iteration step

		// Using
		using DataGlobals::StefanBoltzmann;

		// Locals
		//    0 - don't create debug output files
		//    1 - append results to existing debug output file
		//    2 - store results in new debug output file
		//   3 - save in-between results (in all iterations) to existing debug file


		Array2D< Real64 > a( 4*nlayer, 4*nlayer );
		Array1D< Real64 > b( 4*nlayer );
		static Array1D< Real64 > hgas( maxlay1 );
		//REAL(r64) :: hhatv(maxlay3),hcv(maxlay3), Ebgap(maxlay3), Tgap(maxlay1)
		static Array1D< Real64 > Tgap( maxlay1 );

		//REAL(r64) ::  alpha
		int maxiter;

		Real64 qr_gap_out;
		Real64 qr_gap_in;

		Array1D< Real64 > told( 2*nlayer );

		// Simon: parameters used in case of JCFN iteration method
		//REAL(r64) :: Dvector(maxlay4) ! store diagonal matrix used in JCFN iterations
		Array1D< Real64 > FRes( {1,4*nlayer} ); // store function results from current iteration
		Array1D< Real64 > FResOld( {1,4*nlayer} ); // store function results from previous iteration
		Array1D< Real64 > FResDiff( {1,4*nlayer} ); // save difference in results between iterations
		Array1D< Real64 > Radiation( {1,2*nlayer} ); // radiation on layer surfaces.  used as temporary storage during iterations

		Array1D< Real64 > x( {1,4*nlayer} ); // temporary vector for storing results (theta and Radiation).  used for easier handling
		Array1D< Real64 > dX( {1,4*nlayer}, 0.0 ); // difference in results
		Array2D< Real64 > Jacobian( {1,4*nlayer}, {1,4*nlayer} ); // diagonal vector for jacobian comuptation-free newton method
		Array1D< Real64 > DRes( {1,4*nlayer} ); // used in jacobian forward-difference approximation

		// This is used to store matrix before equation solver.  It is important because solver destroys
		// content of matrices
		Array2D< Real64 > LeftHandSide( {1,4*nlayer}, {1,4*nlayer} );
		Array1D< Real64 > RightHandSide( {1,4*nlayer} );

		// Simon: Keep best achieved convergence
		Real64 prevDifference;
		Real64 Relaxation;
		Array1D< Real64 > RadiationSave( {1,2*nlayer} );
		Array1D< Real64 > thetaSave( {1,2*nlayer} );
		int currentTry;

//		static Array1D_int LayerTypeSpec( maxlay ); //Unused

		int CSMFlag;
		int i;
		int j;
		int k;
		Real64 curDifference;
		int index;
		int curTempCorrection;

		Real64 qc_gap_in;
		Real64 hc_modified_in;

		int CalcOutcome;

		bool iterationsFinished; // To mark whether or not iterations are finished
		bool saveIterationResults;
		bool updateGapTemperature;
		//logical :: TurnOnNewton

		// Formats
		static gio::Fmt Format_1111( "('Outdoor: ',F9.6,' ;  alt2: ',F9.6,' ; alt3: ',F9.6,' ; alt4: ',F9.6)" );
		static gio::Fmt Format_1112( "('Indoor:  ',F9.6,' ;  alt2: ',F9.6,' ; alt3: ',F9.6,' ; alt4: ',F9.6)" );

		int SDLayerIndex = -1;

		// Simon: This is set to zero until it is resolved what to do with modifier
		ShadeHcModifiedOut = 0.0;
		//BuffIndex = 0
		CSMFlag = 0;
		CalcOutcome = CALC_UNKNOWN;
		curTempCorrection = 0;
		AchievedErrorTolerance = 0.0;
		curDifference = 0.0;
		//TurnOnNewton = .TRUE.
		currentTry = 0;
		index = 0;
		TotalIndex = 0;
		iterationsFinished = false;
		qv = 0.0;
		Ebb = 0.0;
		Ebf = 0.0;
		Rb = 0.0;
		Rf = 0.0;
		a = 0.0;
		b = 0.0;

		//Dvector = 0.0
		FRes = 0.0;
		FResOld = 0.0;
		FResDiff = 0.0;
		Radiation = 0.0;
		Relaxation = RelaxationStart;
		//alpha = PicardRelaxation

		maxiter = NumOfIterations;

		//call MarkVentilatedGaps(nlayer, isVentilated, LayerType, vvent)

		if ( Debug_mode == saveIntermediateResults ) {
			saveIterationResults = true;
		} else {
			saveIterationResults = false;
		}

		//call guess(tout, tind, nlayer, gap, thick, glsyswidth, theta, Ebb, Ebf, Tgap)

		for ( i = 1; i <= nlayer; ++i ) {
			k = 2 * i;
			Radiation( k ) = Ebb( i );
			Radiation( k - 1 ) = Ebf( i );
			told( k - 1 ) = 0.0;
			told( k ) = 0.0;
			//told(k-1) = theta(k-1)
			//told(k) = theta(k)
		}

		//bi...Set LayerTypeSpec array - need to treat venetians AND woven shades as glass:
		if ( ThermalMod == THERM_MOD_CSM ) {
			for ( i = 1; i <= nlayer; ++i ) {
				if ( IsShadingLayer( LayerType( i ) ) ) {
//					LayerTypeSpec( i ) = 0; //Unused
					SDLayerIndex = i;
				} else {
//					LayerTypeSpec( i ) = LayerType( i ); //Unused
				}
			}
		}

		//first store results before iterations begin
		if ( saveIterationResults ) {
			storeIterationResults( nlayer, index, theta, trmout, tamb, trmin, troom, ebsky, ebroom, hcin, hcout, hrin, hrout, hin, hout, Ebb, Ebf, Rb, Rf, nperr );
		}

		Tgap( 1 ) = tout;
		Tgap( nlayer + 1 ) = tind;
		for ( i = 2; i <= nlayer; ++i ) {
			Tgap( i ) = ( theta( 2 * i - 1 ) + theta( 2 * i - 2 ) ) / 2;
		}
		//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		//!!! MAIN ITERATION LOOP
		//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		while ( ! ( iterationsFinished ) ) {

			for ( i = 1; i <= 2 * nlayer; ++i ) {
				if ( theta( i ) < 0 ) {
					theta( i ) = 1.0 * i;
				}
			}

			//do i=1,nlayer+1
			//  if (i == 1) then
			//    Tgap(i) = tout
			//  else if (i == nlayer+1) then
			//    Tgap(i) = tind
			//  else
			//    Tgap(i) = (theta(2*i-2) + theta(2*i-1)) / 2.0d0
			//  end if
			//end do

			// skip updating gap temperatures for shading devices. Gap temperature in that case is not simply average
			// between two layer temperatures
			for ( i = 2; i <= nlayer; ++i ) {
				updateGapTemperature = false;
				if ( ( ! ( IsShadingLayer( LayerType( i - 1 ) ) ) ) && ( ! ( IsShadingLayer( LayerType( i ) ) ) ) ) {
					updateGapTemperature = true;
				}
				if ( updateGapTemperature ) {
					Tgap( i ) = ( theta( 2 * i - 1 ) + theta( 2 * i - 2 ) ) / 2;
				}
			}

			// evaluate convective/conductive components of gap
			hatter( nlayer, iwd, tout, tind, wso, wsi, VacuumPressure, VacuumMaxGapThickness, ebsky, tamb, ebroom, troom, gap, height, heightt, scon, tilt, theta, Tgap, Radiation, trmout, trmin, iprop, frct, presure, nmix, wght, gcon, gvis, gcp, gama, SupportPillar, PillarSpacing, PillarRadius, hgas, hcgas, hrgas, hcin, hcout, hin, hout, index, ibc, nperr, ErrorMessage, hrin, hrout, Ra, Nu );

			// exit on error
			if ( ! ( GoAhead( nperr ) ) ) return;

			//bi...Override hhat values near SHADING DEVICE layer(s), but only for CSM thermal model:
			if ( ( ThermalMod == THERM_MOD_CSM ) && ( SDLayerIndex > 0 ) ) {
				// adjust hhat values
				//call adjusthhat(SDLayerIndex, ibc, tout, tind, nlayer, theta, wso, wsi, iwd, height, heightt, tilt,  &
				//               &  thick, gap, hout, hrout, hin, hrin, iprop, frct, presure, nmix, wght, gcon, gvis, gcp, &
				//               index, SDScalar, Ebf, Ebb, hgas, hhat, nperr, ErrorMessage)
				//do i = 1, maxlay3
				//hhatv(i) = 0.0d0
				//Ebgap(i) = 0.0d0
				//qv(i)    = 0.0d0
				//hcv(i)   = 0.0d0
				//end do
				matrixQBalance( nlayer, a, b, scon, thick, hcgas, hcout, hcin, asol, qv, tind, tout, Gin, Gout, theta, tir, rir, emis );
			} else {
				//bi...There are no Venetian layers, or ThermalMod is not CSM, so carry on as usual:
				shading( theta, gap, hgas, hcgas, hrgas, frct, iprop, presure, nmix, wght, gcon, gvis, gcp, nlayer, width, height, tilt, tout, tind, Atop, Abot, Al, Ar, Ah, vvent, tvent, LayerType, Tgap, qv, nperr, ErrorMessage, vfreevent );

				// exit on error
				if ( ! ( GoAhead( nperr ) ) ) return;

				matrixQBalance( nlayer, a, b, scon, thick, hcgas, hcout, hcin, asol, qv, tind, tout, Gin, Gout, theta, tir, rir, emis );

			} //  end if

			FResOld = FRes;

			// Pack results in one array
			for ( i = 1; i <= nlayer; ++i ) {
				k = 4 * i - 3;
				j = 2 * i - 1;

				x( k ) = theta( j );
				x( k + 1 ) = theta( j + 1 );
				x( k + 2 ) = Radiation( j );
				x( k + 3 ) = Radiation( j + 1 );
			}

			CalculateFuncResults( nlayer, a, b, x, FRes );

			FResDiff = FRes - FResOld;

			//if (TurnOnNewton) then
			//  do i = 1, 4*nlayer
			//    temp = x(i)
			//    h = ConvergenceTolerance * ABS(x(i))
			//    if (h == 0) then
			//      h = ConvergenceTolerance
			//    end if
			//    x(i) = temp + h
			//    h = x(i) - temp ! trick to reduce finite precision error
			//    call CalculateFuncResults(nlayer, a, b, x, DRes, nperr, ErrorMessage)
			//    do j = 1, 4*nlayer
			//      Jacobian(j,i) = (DRes(j) - FRes(j)) / h
			//    end do
			//    x(i) = temp
			//  end do
			//end if

			//if (TurnOnNewton) then
			//  LeftHandSide = Jacobian
			//  RightHandSide = -FRes
			//else
			LeftHandSide = a;
			RightHandSide = b;
			//end if
			EquationsSolver( LeftHandSide, RightHandSide, 4 * nlayer, nperr, ErrorMessage );

			//if (TurnOnNewton) then
			//  dx = RightHandSide
			//end if

			// Simon: This is much better, but also much slower convergence criteria.  Think of how to make this flexible and allow
			// user to change this from outside (through argument passing)
			//curDifference = ABS(FRes(1))
			//do i = 2, 4*nlayer
			//curDifference = MAX(curDifference, ABS(FRes(i)))
			//curDifference = curDifference + ABS(FRes(i))
			//end do

			curDifference = std::abs( theta( 1 ) - told( 1 ) );
			//curDifference = ABS(FRes(1))
			for ( i = 2; i <= 2 * nlayer; ++i ) {
				//do i = 2, 4*nlayer
				curDifference = max( curDifference, std::abs( theta( i ) - told( i ) ) );
				//curDifference = MAX(ABS(FRes(i)), curDifference)
			}

			for ( i = 1; i <= nlayer; ++i ) {
				k = 4 * i - 3;
				j = 2 * i - 1;
				//if (TurnOnNewton) then
				//  theta(j) = theta(j) + Relaxation*dx(k)
				//  theta(j+1) = theta(j+1) + Relaxation*dx(k+1)
				//  Radiation(j) = Radiation(j) + Relaxation*dx(k+2)
				//  Radiation(j+1) = Radiation(j+1) + Relaxation*dx(k+3)
				//else
				//  dX(k) = RightHandSide(k) - theta(j)
				//  dX(k+1) = RightHandSide(k + 1) - theta(j+1)
				//  dX(k+2) = RightHandSide(k + 2) - Radiation(j)
				//  dX(k+3) = RightHandSide(k + 3) - Radiation(j+1)
				told( j ) = theta( j );
				told( j + 1 ) = theta( j + 1 );
				theta( j ) = ( 1 - Relaxation ) * theta( j ) + Relaxation * RightHandSide( k );
				theta( j + 1 ) = ( 1 - Relaxation ) * theta( j + 1 ) + Relaxation * RightHandSide( k + 1 );
				Radiation( j ) = ( 1 - Relaxation ) * Radiation( j ) + Relaxation * RightHandSide( k + 2 );
				Radiation( j + 1 ) = ( 1 - Relaxation ) * Radiation( j + 1 ) + Relaxation * RightHandSide( k + 3 );
				//end if
			}

			// it is important not to update gaps around shading layers since that is already calculated by
			// shading routines
			for ( i = 1; i <= nlayer + 1; ++i ) {
				updateGapTemperature = true;
				if ( ( i == 1 ) || ( i == nlayer + 1 ) ) {
					// update gap array with interior and exterior temperature
					updateGapTemperature = true;
				} else {
					// update gap temperature only if gap on both sides
					updateGapTemperature = false;
					if ( ( ! ( IsShadingLayer( LayerType( i - 1 ) ) ) ) && ( ! ( IsShadingLayer( LayerType( i ) ) ) ) ) {
						updateGapTemperature = true;
					}
				}
				j = 2 * ( i - 1 );
				if ( updateGapTemperature ) {
					if ( i == 1 ) {
						Tgap( 1 ) = tout;
					} else if ( i == ( nlayer + 1 ) ) {
						Tgap( i ) = tind;
					} else {
						Tgap( i ) = ( theta( j ) + theta( j + 1 ) ) / 2;
					}
				}
			}

			//and store results during iterations
			if ( saveIterationResults ) {
				storeIterationResults( nlayer, index + 1, theta, trmout, tamb, trmin, troom, ebsky, ebroom, hcin, hcout, hrin, hrout, hin, hout, Ebb, Ebf, Rb, Rf, nperr );
			}

			if ( ! ( GoAhead( nperr ) ) ) return;

			prevDifference = curDifference;

			if ( ( index == 0 ) || ( curDifference < AchievedErrorTolerance ) ) {
				AchievedErrorTolerance = curDifference;
				currentTry = 0;
				for ( i = 1; i <= 2 * nlayer; ++i ) {
					RadiationSave( i ) = Radiation( i );
					thetaSave( i ) = theta( i );
				}
			} else {
				// This is case when program solution diverged
				++currentTry;
				if ( currentTry >= NumOfTries ) {
					currentTry = 0;
					for ( i = 1; i <= 2 * nlayer; ++i ) {
						Radiation( i ) = RadiationSave( i );
						theta( i ) = thetaSave( i );
					}
					//if (.not.TurnOnNewton) then
					//  TurnOnNewton = .TRUE.
					//else
					Relaxation -= RelaxationDecrease;
					TotalIndex += index;
					index = 0;
					// Start from best achieved convergence
					if ( Relaxation <= 0.0 ) { // cannot continue with relaxation equal to zero
						iterationsFinished = true;
					}
					// TurnOnNewton = .TRUE.
					//end if ! if (.not.TurnOnNewton) then
				} // f (currentTry == NumOfTries) then
			}

			// Chek if results were found:
			if ( curDifference < ConvergenceTolerance ) {
				CalcOutcome = CALC_OK;
				TotalIndex += index;
				iterationsFinished = true;
			}

			if ( index >= maxiter ) {
				Relaxation -= RelaxationDecrease;
				TotalIndex += index;
				index = 0;
				//TurnOnNewton = .TRUE.

				// Start from best achieved convergence
				for ( i = 1; i <= 2 * nlayer; ++i ) {
					Radiation( i ) = RadiationSave( i );
					theta( i ) = thetaSave( i );
				}
				if ( Relaxation <= 0.0 ) { // cannot continue with relaxation equal to zero
					iterationsFinished = true;
				}
			}

			++index;
		}

		// Get results from closest iteration and store it
		if ( CalcOutcome == CALC_OK ) {
			for ( i = 1; i <= 2 * nlayer; ++i ) {
				Radiation( i ) = RadiationSave( i );
				theta( i ) = thetaSave( i );
			}

			for ( i = 2; i <= nlayer; ++i ) {
				updateGapTemperature = false;
				if ( ( ! ( IsShadingLayer( LayerType( i - 1 ) ) ) ) && ( ! ( IsShadingLayer( LayerType( i ) ) ) ) ) {
					updateGapTemperature = true;
				}

				if ( updateGapTemperature ) {
					Tgap( i ) = ( theta( 2 * i - 1 ) + theta( 2 * i - 2 ) ) / 2;
				}
			}

			// Simon: It is important to recalculate coefficients from most accurate run
			hatter( nlayer, iwd, tout, tind, wso, wsi, VacuumPressure, VacuumMaxGapThickness, ebsky, tamb, ebroom, troom, gap, height, heightt, scon, tilt, theta, Tgap, Radiation, trmout, trmin, iprop, frct, presure, nmix, wght, gcon, gvis, gcp, gama, SupportPillar, PillarSpacing, PillarRadius, hgas, hcgas, hrgas, hcin, hcout, hin, hout, index, ibc, nperr, ErrorMessage, hrin, hrout, Ra, Nu );

			shading( theta, gap, hgas, hcgas, hrgas, frct, iprop, presure, nmix, wght, gcon, gvis, gcp, nlayer, width, height, tilt, tout, tind, Atop, Abot, Al, Ar, Ah, vvent, tvent, LayerType, Tgap, qv, nperr, ErrorMessage, vfreevent );
		}

		if ( CalcOutcome == CALC_UNKNOWN ) {
			ErrorMessage = "Tarcog failed to converge";
			nperr = 2; // error 2: failed to converge...
		}

		// Get radiation results first
		//if (curEquationsApproach.eq.eaQBalance) then
		for ( i = 1; i <= nlayer; ++i ) {
			k = 2 * i - 1;
			Rf( i ) = Radiation( k );
			Rb( i ) = Radiation( k + 1 );
			Ebf( i ) = StefanBoltzmann * pow_4( theta( k ) );
			Ebb( i ) = StefanBoltzmann * pow_4( theta( k + 1 ) );
		}
		//end if

		// Finishing calcs:
		resist( nlayer, trmout, tout, trmin, tind, hcgas, hrgas, theta, q, qv, LayerType, thick, scon, ufactor, flux, qcgas, qrgas );

		//bi...  Set T6-related quantities - ratios for modified epsilon, hc for modelling external SDs:
		//    (using non-solar pass results)
		if ( ( dir == 0.0 ) && ( nlayer > 1 ) ) {

			qr_gap_out = Rf( 2 ) - Rb( 1 );
			qr_gap_in = Rf( nlayer ) - Rb( nlayer - 1 );

			if ( IsShadingLayer( LayerType( 1 ) ) ) {
				ShadeEmisRatioOut = qr_gap_out / ( emis( 3 ) * StefanBoltzmann * ( pow_4( theta( 3 ) ) - pow_4( trmout ) ) );
				//qc_gap_out = qprim(3) - qr_gap_out
				//qcgapout2 = qcgas(1)
				//Hc_modified_out = (qc_gap_out / (theta(3) - tout))
				//ShadeHcModifiedOut = Hc_modified_out
			}

			if ( IsShadingLayer( LayerType( nlayer ) ) ) {
				ShadeEmisRatioIn = qr_gap_in / ( emis( 2 * nlayer - 2 ) * StefanBoltzmann * ( pow_4( trmin ) - pow_4( theta( 2 * nlayer - 2 ) ) ) );
				qc_gap_in = q( 2 * nlayer - 1 ) - qr_gap_in;
				hc_modified_in = ( qc_gap_in / ( tind - theta( 2 * nlayer - 2 ) ) );
				ShadeHcModifiedIn = hc_modified_in;
			}
		} // IF dir = 0

		//do i=1, nlayer-1
		//  if (((LayerType(i).eq.VENETBLIND)  &
		//      &  .and.(ThermalMod.ne.THERM_MOD_CSM))  &
		//      &  .or.(LayerType(i).eq.WOVSHADE)) then
		//    !hcgas(i+1)=hcv(2*i+1)
		//  else
		//    !hcgas(i+1)=hgas(i+1)
		//  end if
		//end do

		//110   format(' Theta(',I1,') = ',F12.6)
		//111   format(' T(',I1,')=',F15.9)
		//112  format(' ',A3,' =',F15.9)

	}

	void
	guess(
		Real64 const tout,
		Real64 const tind,
		int const nlayer,
		Array1A< Real64 > const gap,
		Array1A< Real64 > const thick,
		Real64 & width,
		Array1A< Real64 > theta,
		Array1A< Real64 > Ebb,
		Array1A< Real64 > Ebf,
		Array1A< Real64 > Tgap
	)
	{
		//***********************************************************************
		// purpose - initializes temperature distribution assuming
		//   a constant temperature gradient across the window
		//***********************************************************************
		// Input
		//   tout    outdoor air temperature (k)
		//   tind     indoor air temperature (k)
		//   nlayer  number of solid layers in window output
		//   gap     thickness of gas gaps (m)
		//   thick   thickness of glazing layers (m)
		// Output
		//   width   total width of the glazing system
		//   theta   array of surface temps starting from outdoor layer (k)
		//   Ebb     vector of emissive power (?) of the back surface (# of layers)
		//   Ebf     vector of emissive power (?) of the front surface (# of layers)
		// Locals
		//   x   Vector of running width
		//   delta   delta T per unit length

		// Using
		using DataGlobals::StefanBoltzmann;

		// Argument array dimensioning
		gap.dim( MaxGap );
		thick.dim( maxlay );
		theta.dim( maxlay2 );
		Ebb.dim( maxlay );
		Ebf.dim( maxlay );
		Tgap.dim( maxlay1 );

		// Locals
		Array1D< Real64 > x( maxlay2 );
		Real64 delta;
		int i;
		int j;
		int k;

		x( 1 ) = 0.001;
		x( 2 ) = x( 1 ) + thick( 1 );

		for ( i = 2; i <= nlayer; ++i ) {
			j = 2 * i - 1;
			k = 2 * i;
			x( j ) = x( j - 1 ) + gap( i - 1 );
			x( k ) = x( k - 1 ) + thick( i );
		}

		width = x( nlayer * 2 ) + 0.01;
		delta = ( tind - tout ) / width;

		if ( delta == 0.0 ) {
			delta = TemperatureQuessDiff / width;
		}

		for ( i = 1; i <= nlayer; ++i ) {
			j = 2 * i;
			theta( j - 1 ) = tout + x( j - 1 ) * delta;
			theta( j ) = tout + x( j ) * delta;
			Ebf( i ) = StefanBoltzmann * pow_4( theta( j - 1 ) );
			Ebb( i ) = StefanBoltzmann * pow_4( theta( j ) );
		}

		for ( i = 1; i <= nlayer + 1; ++i ) {
			if ( i == 1 ) {
				Tgap( 1 ) = tout;
			} else if ( i == ( nlayer + 1 ) ) {
				Tgap( nlayer + 1 ) = tind;
			} else {
				Tgap( i ) = ( theta( 2 * i - 1 ) + theta( 2 * i - 2 ) ) / 2;
			}
		}

	}

	void
	TemperaturesFromEnergy(
		Array1A< Real64 > theta,
		Array1A< Real64 > Tgap,
		Array1A< Real64 > const Ebf,
		Array1A< Real64 > const Ebb,
		int const nlayer,
		int & nperr,
		std::string & ErrorMessage
	)
	{
		//***********************************************************************
		// this subroutine computes the new temperature distribution
		//***********************************************************************

		// Using
		using DataGlobals::StefanBoltzmann;

		// Argument array dimensioning
		theta.dim( maxlay2 );
		Tgap.dim( maxlay1 );
		Ebf.dim( maxlay );
		Ebb.dim( maxlay );

		// Locals
		//REAL(r64), intent(out) :: dtmax
		//integer, intent(out) :: MaxIndex

		Array1D< Real64 > told( maxlay2 );
		int i;
		int j;

		//dtmax = 0.0d0
		//MaxIndex = 0

		// first check for energy values. They cannot be negative because power to 0.25
		// will crash application
		for ( i = 1; i <= nlayer; ++i ) {
			if ( ( Ebf( i ) < 0 ) && ( Ebb( i ) < 0 ) ) {
				nperr = 2; //this is flag for convergence error
				ErrorMessage = "Tarcog failed to converge.";
				return; // stop execution
			}
		}

		for ( i = 1; i <= nlayer; ++i ) {
			j = 2 * i;
			told( j ) = theta( j );
			told( j - 1 ) = theta( j - 1 );
			theta( j - 1 ) = root_4( Ebf( i ) / StefanBoltzmann );
			theta( j ) = root_4( Ebb( i ) / StefanBoltzmann );
			if ( i != 1 ) {
				Tgap( i ) = ( theta( j - 1 ) + theta( j - 2 ) ) / 2;
			}
		}

	}

	void
	solarISO15099(
		Real64 const totsol,
		Real64 const rtot,
		Array1A< Real64 > const rs,
		int const nlayer,
		Array1A< Real64 > const absol,
		Real64 & sf
	)
	{
		//***********************************************************************
		//   This subroutine calculates the shading coefficient for a window.
		//***********************************************************************
		//  Inputs:
		//    absol     array of absorped fraction of solar radiation in lites
		//    totsol    total solar transmittance
		//    rtot  total thermal resistance of window
		//    rs    array of thermal resistances of each gap and layer
		//    layer     number of layers
		//     dir  direct solar radiation
		//  Outputs:
		//    sf    solar gain of space

		// Argument array dimensioning
		rs.dim( maxlay3 );
		absol.dim( maxlay );

		// Locals
		Real64 flowin;
		Real64 fract;
		int i;
		int j;

		fract = 0.0;
		flowin = 0.0;
		sf = 0.0;

		if ( rtot == 0.0 ) {
			return;
		}

		// evaluate inward flowing fraction of absorbed radiation:
		flowin = ( rs( 1 ) + 0.5 * rs( 2 ) ) / rtot;
		fract = absol( 1 ) * flowin;

		for ( i = 2; i <= nlayer; ++i ) {
			j = 2 * i;
			flowin += ( 0.5 * ( rs( j - 2 ) + rs( j ) ) + rs( j - 1 ) ) / rtot;
			fract += absol( i ) * flowin;
		}
		sf = totsol + fract; // add inward fraction to directly transmitted fraction

	}

	void
	resist(
		int const nlayer,
		Real64 const trmout,
		Real64 const Tout,
		Real64 const trmin,
		Real64 const tind,
		Array1< Real64 > const & hcgas,
		Array1< Real64 > const & hrgas,
		Array1< Real64 > & Theta,
		Array1< Real64 > & qlayer,
		Array1< Real64 > const & qv,
		Array1_int const & LayerType,
		Array1< Real64 > const & thick,
		Array1< Real64 > const & scon,
		Real64 & ufactor,
		Real64 & flux,
		Array1< Real64 > & qcgas,
		Array1< Real64 > & qrgas
	)
	{
		//***********************************************************************
		// subroutine to calculate total thermal resistance of the glazing system
		//***********************************************************************

		// Locals
		int i;

		//R_tot = 0.0d0

		// Simon: calculation of heat flow through gaps and layers as well as ventilation speed and heat flow
		// are kept just for reporting purposes.  U-factor calculation is performed by calculating heat flow transfer
		// at indoor layer

		//calculate heat flow for external and internal environments and gaps
		for ( i = 1; i <= nlayer + 1; ++i ) {
			if ( i == 1 ) {
				qcgas( i ) = hcgas( i ) * ( Theta( 2 * i - 1 ) - Tout );
				qrgas( i ) = hrgas( i ) * ( Theta( 2 * i - 1 ) - trmout );
				qlayer( 2 * i - 1 ) = qcgas( i ) + qrgas( i );
				//    rs(2*i-1) = 1/hgas(i)
			} else if ( i == ( nlayer + 1 ) ) {
				qcgas( i ) = hcgas( i ) * ( tind - Theta( 2 * i - 2 ) );
				qrgas( i ) = hrgas( i ) * ( trmin - Theta( 2 * i - 2 ) );
				qlayer( 2 * i - 1 ) = qcgas( i ) + qrgas( i );
				//    rs(2*i-1) = 1/hgas(i)
			} else {
				qcgas( i ) = hcgas( i ) * ( Theta( 2 * i - 1 ) - Theta( 2 * i - 2 ) );
				qrgas( i ) = hrgas( i ) * ( Theta( 2 * i - 1 ) - Theta( 2 * i - 2 ) );
				qlayer( 2 * i - 1 ) = qcgas( i ) + qrgas( i );
				//    rs(2*i-1) = 1/hgas(i)
			}
		}

		//.....Calculate thermal resistances for glazing layers:
		for ( i = 1; i <= nlayer; ++i ) {
			//  rs(2*i) = thick(i)/scon(i)
			qlayer( 2 * i ) = scon( i ) / thick( i ) * ( Theta( 2 * i ) - Theta( 2 * i - 1 ) );
		}

		//R_tot = 0.0d0

		//do i = 1, 2*nlayer+1
		//  R_tot = R_tot + rs(i)
		//end do

		// U factor:
		//ufactor = 1.0d0/R_tot

		flux = qlayer( 2 * nlayer + 1 );
		if ( IsShadingLayer( LayerType( nlayer ) ) ) {
			flux += qv( nlayer );
		}

		ufactor = 0.0;
		if ( tind != Tout ) {
			ufactor = flux / ( tind - Tout );
		}

	}

	void
	hatter(
		int const nlayer,
		int const iwd,
		Real64 const tout,
		Real64 const tind,
		Real64 const wso,
		Real64 const wsi,
		Real64 const VacuumPressure,
		Real64 const VacuumMaxGapThickness,
		Real64 & ebsky,
		Real64 & tamb,
		Real64 & ebroom,
		Real64 & troom,
		Array1< Real64 > const & gap,
		Real64 const height,
		Real64 const heightt,
		Array1< Real64 > const & scon,
		Real64 const tilt,
		Array1< Real64 > & theta,
		Array1< Real64 > const & Tgap,
		Array1< Real64 > & Radiation,
		Real64 const trmout,
		Real64 const trmin,
		Array2_int const & iprop,
		Array2< Real64 > const & frct,
		Array1< Real64 > const & presure,
		Array1_int const & nmix,
		Array1< Real64 > const & wght,
		Array2< Real64 > const & gcon,
		Array2< Real64 > const & gvis,
		Array2< Real64 > const & gcp,
		Array1< Real64 > const & gama,
		Array1_int const & SupportPillar,
		Array1< Real64 > const & PillarSpacing,
		Array1< Real64 > const & PillarRadius,
		Array1< Real64 > & hgas,
		Array1< Real64 > & hcgas,
		Array1< Real64 > & hrgas,
		Real64 & hcin,
		Real64 & hcout,
		Real64 const hin,
		Real64 const hout,
		int const index,
		Array1_int const & ibc,
		int & nperr,
		std::string & ErrorMessage,
		Real64 & hrin,
		Real64 & hrout,
		Array1< Real64 > & Ra,
		Array1< Real64 > & Nu
	)
	{
		//***********************************************************************
		//  This subroutine calculates the array of conductances/film coefficients used to model convection.  The conductances/film
		//  coefficients are calculated as functions of temperature defined with the usual variable h and THEN are converted into an
		//  equivalent value interms of the black body emittance based on the surface
		//***********************************************************************
		// Inputs
		//   nlayer   number of solid layers
		//   iwd  wind direction
		//   tout     outside temp in k
		//   tind  inside temp in k
		//   wso  wind speed in m/s
		//   wsi  inside forced air speed m/s
		//   Ebsky    ir flux from outside
		//   Ebroom   ir flux from room
		//   Gout     radiosity (ir flux) of the combined environment (sky+ground)
		//   Gin
		//   gap  vector of gap widths in meters
		//   height   IGU cavity height
		//   heightt
		//   thick    glazing layer thickness
		//   scon   Vector of conductivities of each glazing layer
		//   tilt   Window tilt (in degrees)
		//   theta  Vector of average temperatures
		//   Ebb
		//   Ebf
		//   iprop    array of gap mixtures
		//   frct     vector of mixture fractions
		//   presure
		//   hin   Indoor Indoor combined film coefficient (if non-zero)
		//   hout  Outdoor combined film coefficient (if non-zero)
		//   nmix  vector of number of gasses in a mixture for each gap
		// Ouputs
		//   hhat     vector of all film coefficients (maxlay3)
		//   hgas     vector of gap 'film' coeff.
		//   hcin  Indoor convective surface heat transfer coefficient
		//   hcout     Outdoor convective heat transfer coeff
		//   hrin    Indoor radiative surface heat transfer coefficient
		//   hrout   Outdoor radiative surface heat transfer coefficient
		//   hin   Indoor combined film coefficient
		//   hout  Outdoor combined film coefficient
		//   index    iteration step
		//   ibc
		// Inactives**
		//   wa - window azimuth (degrees, clockwise from south)

		// Locals
		int i;
		int k;
		int nface;
		//character(len=3) :: a

		//  common    /props/ gcon(maxgas,3),gvis(maxgas,3),gcp(maxgas,3),grho(maxgas,3),wght(maxgas)

		// evaluate convective/conductive components of gap grashof number, thermal conductivity and their derivatives:
		nface = 2 * nlayer;

		//do i = 1, nlayer
		//  j=2*i

		//  if ((Ebb(i)-Ebf(i)).eq.0) then
		//    theta(j) = theta(j) + tempCorrection
		//    Ebb(i) = sigma * (theta(j) ** 4)
		//  end if
		//  hhat(j) = scon(i)/thick(i) * (theta(j)-theta(j-1))/(Ebb(i)-Ebf(i))

		//dr.....caluclate for laminate procedure
		//  if (nslice(i).gt.1) then
		//    if ((LaminateB(i).ne.0).and.((Ebb(i)-Ebf(i)).ne.0)) then
		//      hhat(j) = (theta(j)-theta(j-1))/(LaminateB(i) * (Ebb(i)-Ebf(i)))
		//    end if
		//  end if
		//if (hhat(j).lt.0) then
		//  nperr = 6
		//  write(a, '(i3)') i
		//  ErrorMessage = 'Heat transfer coefficient based on emissive power in glazing layer is less than zero. Layer #'//TRIM(a)
		//  return
		//end if
		//end do

		filmg( tilt, theta, Tgap, nlayer, height, gap, iprop, frct, VacuumPressure, presure, nmix, wght, gcon, gvis, gcp, gama, hcgas, Ra, Nu, nperr, ErrorMessage );

		if ( ! ( GoAhead( nperr ) ) ) {
			return;
		}

		//this is adding influence of pillar to hgas
		filmPillar( SupportPillar, scon, PillarSpacing, PillarRadius, nlayer, gap, hcgas, VacuumMaxGapThickness, nperr, ErrorMessage );

		if ( ! ( GoAhead( nperr ) ) ) {
			return;
		}

		// adjust radiation coefficients
		//hrgas = 0.0d0
		for ( i = 2; i <= nlayer; ++i ) {
			k = 2 * i - 1;
			//if ((theta(k)-theta(k-1)) == 0) then
			//  theta(k-1) = theta(k-1) + tempCorrection
			//end if
			if ( ( theta( k ) - theta( k - 1 ) ) != 0 ) {
				hrgas( i ) = ( Radiation( k ) - Radiation( k - 1 ) ) / ( theta( k ) - theta( k - 1 ) );
			}

			hgas( i ) = hcgas( i ) + hrgas( i );
		}

		// convective indoor film coeff:
		if ( ibc( 2 ) <= 0 ) {
			filmi( tind, theta( nface ), nlayer, tilt, wsi, heightt, iprop, frct, presure, nmix, wght, gcon, gvis, gcp, hcin, ibc( 2 ), nperr, ErrorMessage );
		} else if ( ibc( 2 ) == 1 ) {
			hcin = hin - hrin;
			//Simon: First iteration is with index = 0 and that means it should reenter iteration with whatever is provided as input
			//else if (ibc(2).eq.2.and.index.eq.1) then
		} else if ( ( ibc( 2 ) == 2 ) && ( index == 0 ) ) {
			hcin = hin;
		}
		if ( hcin < 0 ) {
			nperr = 8;
			ErrorMessage = "Hcin is less then zero.";
			return;
		}

		hcgas( nlayer + 1 ) = hcin;
		//hrin = 0.95d0*(Ebroom - Radiation(2*nlayer))/(Trmin-theta(2*nlayer))+0.05d0*hrin
		hrin = ( ebroom - Radiation( 2 * nlayer ) ) / ( trmin - theta( 2 * nlayer ) );
		//if ((Theta(2*nlayer) - Trmin).ne.0) then
		//  hrin =  sigma * emis(2*nlayer) * (Theta(2*nlayer)**4 - Trmin**4)/(Theta(2*nlayer) - Trmin)
		//else
		//  Theta(2*nlayer) = Theta(2*nlayer) + tempCorrection
		//  hrin =  sigma * emis(2*nlayer) * (Theta(2*nlayer)**4 - Trmin**4)/(Theta(2*nlayer) - Trmin)
		//end if
		hrgas( nlayer + 1 ) = hrin;
		//hgas(nlayer+1)  = hcgas(nlayer+1) + hrgas(nlayer+1)
		troom = ( hcin * tind + hrin * trmin ) / ( hcin + hrin );

		// convective outdoor film coeff:
		if ( ibc( 1 ) <= 0 ) {
			film( tout, theta( 1 ), wso, iwd, hcout, ibc( 1 ) );
		} else if ( ibc( 1 ) == 1 ) {
			hcout = hout - hrout;
			//Simon: First iteration is with index = 0 and that means it should reenter iteration with whatever is provided as input
			//else if (ibc(1).eq.2.and.index.eq.1) then
		} else if ( ( ibc( 1 ) == 2 ) && ( index == 0 ) ) {
			hcout = hout;
		}
		if ( hcout < 0 ) {
			nperr = 9;
			ErrorMessage = "Hcout is less than zero.";
			return;
		}

		hcgas( 1 ) = hcout;
		hrout = ( Radiation( 1 ) - ebsky ) / ( theta( 1 ) - trmout );
		//if ((Theta(1) - Trmout).ne.0) then
		//  hrout = sigma * emis(1) * (Theta(1)**4 - Trmout**4)/(Theta(1) - Trmout)
		//else
		//  Theta(1) = Theta(1) + tempCorrection
		//  hrout = sigma * emis(1) * (Theta(1)**4 - Trmout**4)/(Theta(1) - Trmout)
		//end if
		hrgas( 1 ) = hrout;
		//hgas(1)  = hrout + hcout
		tamb = ( hcout * tout + hrout * trmout ) / ( hcout + hrout );

	}

	void
	filmi(
		Real64 const tair,
		Real64 const t,
		int const nlayer,
		Real64 const tilt,
		Real64 const wsi,
		Real64 const height,
		Array2A_int const iprop,
		Array2A< Real64 > const frct,
		Array1A< Real64 > const presure,
		Array1A_int const nmix,
		Array1A< Real64 > const wght,
		Array2A< Real64 > const gcon,
		Array2A< Real64 > const gvis,
		Array2A< Real64 > const gcp,
		Real64 & hcin,
		int const ibc,
		int & nperr,
		std::string & ErrorMessage
	)
	{
		//***********************************************************************
		//  purpose to evaluate heat flux at indoor surface of window using still air correlations (Curcija and Goss 1993)
		//  found in SPC142 equations 5.43 - 5.48.
		//***********************************************************************
		// Input
		//   tair - room air temperature
		//   t - inside surface temperature
		//   nlayer  number of glazing layers
		//   tilt - the tilt of the glazing in degrees
		//   wsi - room wind speed (m/s)
		//   height - window height
		//   iprop
		//   frct
		//   presure
		//   nmix  vector of number of gasses in a mixture for each gap
		// Output
		//   hcin - indoor convecive heat transfer coeff

		// If there is forced air in the room than use SPC142 corelation 5.49 to calculate the room side film coefficient.

		// Using
		using DataGlobals::GravityConstant;
		using DataGlobals::Pi;

		// Argument array dimensioning
		iprop.dim( maxgas, maxlay1 );
		frct.dim( maxgas, maxlay1 );
		presure.dim( maxlay1 );
		nmix.dim( maxlay1 );
		wght.dim( maxgas );
		gcon.dim( 3, maxgas );
		gvis.dim( 3, maxgas );
		gcp.dim( 3, maxgas );

		// Locals
		static Array1D< Real64 > frcti( maxgas );
		int j;
		static Array1D_int ipropi( maxgas );
		Real64 tiltr;
		Real64 tmean;
		Real64 delt;
		Real64 con;
		Real64 visc;
		Real64 dens;
		Real64 cp;
		Real64 pr;
		Real64 gr;
		Real64 RaCrit;
		Real64 RaL;
		Real64 Gnui( 0.0 );

		if ( wsi > 0.0 ) { // main IF
			{ auto const SELECT_CASE_var( ibc );
			if ( SELECT_CASE_var == 0 ) {
				hcin = 4.0 + 4.0 * wsi;
			} else if ( SELECT_CASE_var == -1 ) {
				hcin = 5.6 + 3.8 * wsi; // SPC142 correlation
				return;
			}}
		} else { // main IF - else
			tiltr = tilt * 2.0 * Pi / 360.0; // convert tilt in degrees to radians
			tmean = tair + 0.25 * ( t - tair );
			delt = std::abs( tair - t );

			for ( j = 1; j <= nmix( nlayer + 1 ); ++j ) {
				ipropi( j ) = iprop( j, nlayer + 1 );
				frcti( j ) = frct( j, nlayer + 1 );
			}

			GASSES90( tmean, ipropi, frcti, presure( nlayer + 1 ), nmix( nlayer + 1 ), wght, gcon, gvis, gcp, con, visc, dens, cp, pr, ISO15099, nperr, ErrorMessage );

			//   Calculate grashoff number:
			//   The grashoff number is the Rayleigh Number (equation 5.29) in SPC142 divided by the Prandtl Number (prand):
			gr = GravityConstant * pow_3( height ) * delt * pow_2( dens ) / ( tmean * pow_2( visc ) );

			RaL = gr * pr;
			//   write(*,*)' RaCrit,RaL,gr,pr '
			//   write(*,*) RaCrit,RaL,gr,pr

			if ( ( 0.0 <= tilt ) && ( tilt < 15.0 ) ) { // IF no. 1
				Gnui = 0.13 * std::pow( RaL, 1.0 / 3.0 );
			} else if ( ( 15.0 <= tilt ) && ( tilt <= 90.0 ) ) {
				//   if the room air is still THEN use equations 5.43 - 5.48:
				RaCrit = 2.5e5 * std::pow( std::exp( 0.72 * tilt ) / std::sin( tiltr ), 0.2 );
				if ( RaL <= RaCrit ) { // IF no. 2
					Gnui = 0.56 * root_4( RaL * std::sin( tiltr ) );
					// write(*,*) ' Nu ', Gnui
				} else {
					//Gnui = 0.13d0*(RaL**0.3333d0 - RaCrit**0.3333d0) + 0.56d0*(RaCrit*sin(tiltr))**0.25d0
					Gnui = 0.13 * ( std::pow( RaL, 1.0 / 3.0 ) - std::pow( RaCrit, 1.0 / 3.0 ) ) + 0.56 * root_4( RaCrit * std::sin( tiltr ) );
				} // end if no. 2
			} else if ( ( 90.0 < tilt ) && ( tilt <= 179.0 ) ) {
				Gnui = 0.56 * root_4( RaL * std::sin( tiltr ) );
			} else if ( ( 179.0 < tilt ) && ( tilt <= 180.0 ) ) {
				Gnui = 0.58 * std::pow( RaL, 1 / 3.0 );
			} else {
				assert( false );
			} // end if no. 1
			//   write(*,*) ' RaL   ', RaL, '   RaCrit', RaCrit
			//   write(*,*)'   Nusselt Number   ',Gnui

			hcin = Gnui * ( con / height );
			//   hin = 1.77d0*(ABS(t-tair))**0.25d0

		} // end main IF

	}

	void
	filmg(
		Real64 const tilt,
		Array1A< Real64 > const theta,
		Array1A< Real64 > const Tgap,
		int const nlayer,
		Real64 const height,
		Array1A< Real64 > const gap,
		Array2A_int const iprop,
		Array2A< Real64 > const frct,
		Real64 const VacuumPressure,
		Array1A< Real64 > const presure,
		Array1A_int const nmix,
		Array1A< Real64 > const wght,
		Array2A< Real64 > const gcon,
		Array2A< Real64 > const gvis,
		Array2A< Real64 > const gcp,
		Array1A< Real64 > const gama,
		Array1A< Real64 > hcgas,
		Array1A< Real64 > Rayleigh,
		Array1A< Real64 > Nu,
		int & nperr,
		std::string & ErrorMessage
	)
	{
		//***********************************************************************
		// sobroutine to calculate effective conductance of gaps
		//***********************************************************************
		// Inputs:
		//   tilt  window angle (deg)
		//   theta     vector of surface temperatures [K]
		//   nlayer    total number of glazing layers
		//   height    glazing cavity height
		//   gap   vector of gap widths [m]
		//   iprop
		//   frct
		//   presure
		//   nmix  vector of number of gasses in a mixture for each gap
		// Output:
		//   hgas  vector of gap coefficients
		//   nperr     error code
		// Locals:
		//   gr    gap grashof number
		//   con   gap gas conductivity
		//   visc  dynamic viscosity @ mean temperature [g/m*s]
		//   dens  density @ mean temperature [kg/m^3]
		//   cp    specific heat @ mean temperature [J/g*K]
		//   pr    gap gas Prandtl number
		//   tmean     average film temperature
		//   delt  temperature difference

		// Using
		using DataGlobals::GravityConstant;

		// Argument array dimensioning
		theta.dim( maxlay2 );
		Tgap.dim( maxlay1 );
		gap.dim( MaxGap );
		iprop.dim( maxgas, maxlay1 );
		frct.dim( maxgas, maxlay1 );
		presure.dim( maxlay1 );
		nmix.dim( maxlay1 );
		wght.dim( maxgas );
		gcon.dim( 3, maxgas );
		gvis.dim( 3, maxgas );
		gcp.dim( 3, maxgas );
		gama.dim( maxgas );
		hcgas.dim( maxlay1 );
		Rayleigh.dim( maxlay );
		Nu.dim( maxlay );

		// Locals
		Real64 con;
		Real64 visc;
		Real64 dens;
		Real64 cp;
		Real64 pr;
		Real64 delt;
		Real64 tmean;
		Real64 ra;
		Real64 asp;
		Real64 gnu;
		static Array1D< Real64 > frctg( maxgas );
		static Array1D_int ipropg( maxgas );
		int i;
		int j;
		int k;
		int l;

		hcgas = 0.0;

		for ( i = 1; i <= nlayer - 1; ++i ) {
			j = 2 * i;
			k = j + 1;
			// determine the gas properties of each gap:
			//tmean = (theta(j)+theta(k))/2.0d0
			tmean = Tgap( i + 1 ); // Tgap(1) is exterior environment
			delt = std::abs( theta( j ) - theta( k ) );
			// Temperatures should not be equal. This can happen in initial temperature guess before iterations started
			if ( delt == 0.0 ) delt = 1.0e-6;
			for ( l = 1; l <= nmix( i + 1 ); ++l ) {
				ipropg( l ) = iprop( l, i + 1 );
				frctg( l ) = frct( l, i + 1 );
			}

			if ( presure( i + 1 ) > VacuumPressure ) {
				GASSES90( tmean, ipropg, frctg, presure( i + 1 ), nmix( i + 1 ), wght, gcon, gvis, gcp, con, visc, dens, cp, pr, ISO15099, nperr, ErrorMessage );

				// Calculate grashoff number:
				// The grashoff number is the Rayleigh Number (equation 5.29) in SPC142 divided by the Prandtl Number (prand):
				ra = GravityConstant * pow_3( gap( i ) ) * delt * cp * pow_2( dens ) / ( tmean * visc * con );
				Rayleigh( i ) = ra;
				// write(*,*) 'height,gap(i),asp',height,gap(i),asp
				//asp = 1
				//if (gap(i).ne.0) then
				asp = height / gap( i );
				//end if
				// determine the Nusselt number:
				nusselt( tilt, ra, asp, gnu, nperr, ErrorMessage );

				Nu( i ) = gnu;
				// calculate effective conductance of the gap
				hcgas( i + 1 ) = con / gap( i ) * gnu;

				// write(*,*)'theta(j),theta(k),j,k',j,theta(j),k,theta(k)
				// write(*,*)'Nusselt,Rayleigh,Prandtl,hgas(k),k'
				// write(*,*) gnu,gr*pr,pr,hgas(k),k
			} else { //low pressure calculations
				GassesLow( tmean, wght( iprop( 1, i + 1 ) ), presure( i + 1 ), gama( iprop( 1, i + 1 ) ), con, nperr, ErrorMessage );
				hcgas( i + 1 ) = con;
			} //if (pressure(i+1).gt.VacuumPressure) then
		}
	}

	void
	filmPillar(
		Array1A_int const SupportPillar, // Shows whether or not gap have support pillar
		Array1A< Real64 > const scon, // Conductivity of glass layers
		Array1A< Real64 > const PillarSpacing, // Pillar spacing for each gap (used in case there is support pillar)
		Array1A< Real64 > const PillarRadius, // Pillar radius for each gap (used in case there is support pillar)
		int const nlayer,
		Array1A< Real64 > const gap,
		Array1A< Real64 > hcgas,
		Real64 const EP_UNUSED( VacuumMaxGapThickness ),
		int & EP_UNUSED( nperr ),
		std::string & EP_UNUSED( ErrorMessage )
	)
	{
		//***********************************************************************
		// subroutine to calculate effective conductance of support pillars
		//***********************************************************************

		// Using
		using DataGlobals::Pi;

		// Argument array dimensioning
		SupportPillar.dim( maxlay );
		scon.dim( maxlay );
		PillarSpacing.dim( maxlay );
		PillarRadius.dim( maxlay );
		gap.dim( MaxGap );
		hcgas.dim( maxlay1 );

		// Locals
		//   0 - does not have support pillar
		//   1 - have support pillar

		static Real64 cpa( 0.0 );
		static Real64 aveGlassConductivity( 0.0 );
		static int i( 0 );
		static int k( 0 );

		for ( i = 1; i <= nlayer - 1; ++i ) {
			k = 2 * i + 1;
			if ( SupportPillar( i ) == YES_SupportPillar ) {
				//lkl        if (gap(i).gt.(VacuumMaxGapThickness + InputDataTolerance)) then
				//lkl          nperr = 1007 !support pillar is not necessary for wide gaps (calculation will continue)
				//lkl          write(a, '(f12.6)') VacuumMaxGapThickness
				//lkl          write(b, '(i3)') i
				//lkl          ErrorMessage = 'Gap width is more than '//TRIM(a)//' and it contains support pillar. Gap #'//TRIM(b)
				//lkl        end if  !if (gap(i).gt.VacuumMaxGapThickness) then

				//Average glass conductivity is taken as average from both glass surrounding gap
				aveGlassConductivity = ( scon( i ) + scon( i + 1 ) ) / 2;

				cpa = 2.0 * aveGlassConductivity * PillarRadius( i ) / ( pow_2( PillarSpacing( i ) ) * ( 1.0 + 2.0 * gap( i ) / ( Pi * PillarRadius( i ) ) ) );

				//It is important to add on prevoius values caluculated for gas
				hcgas( i + 1 ) += cpa;
			} //if (SupportPillar(i).eq.YES_SupportPillar) then

		}

	}

	void
	nusselt(
		Real64 const tilt,
		Real64 const ra,
		Real64 const asp,
		Real64 & gnu,
		int & nperr,
		std::string & ErrorMessage
	)
	{
		//***********************************************************************
		// purpose to calculate nusselt modulus for air gaps (ISO15099)
		//***********************************************************************
		// Input
		//   tilt   tilt in degrees
		//   ra     rayleigh number
		//   asp    Aspect ratio
		// Output
		//   gnu    nusselt number
		//   nperr

		// Using
		using DataGlobals::Pi;

		// Locals
		Real64 subNu1;
		Real64 subNu2;
		Real64 subNu3;
		Real64 Nu1;
		Real64 Nu2;
		Real64 G;
		Real64 Nu60;
		Real64 Nu90;
		Real64 tiltr;

		subNu1 = 0.0;
		subNu2 = 0.0;
		subNu3 = 0.0;
		Nu1 = 0.0;
		Nu2 = 0.0;
		Nu90 = 0.0;
		Nu60 = 0.0;
		G = 0.0;
		tiltr = tilt * 2.0 * Pi / 360.0; // convert tilt in degrees to radians
		if ( ( tilt >= 0.0 ) && ( tilt < 60.0 ) ) { //ISO/DIS 15099 - chapter 5.3.3.1
			subNu1 = 1.0 - 1708.0 / ( ra * std::cos( tiltr ) );
			subNu1 = pos( subNu1 );
			subNu2 = 1.0 - ( 1708.0 * std::pow( std::sin( 1.8 * tiltr ), 1.6 ) ) / ( ra * std::cos( tiltr ) );
			subNu3 = std::pow( ra * std::cos( tiltr ) / 5830.0, 1.0 / 3.0 ) - 1.0;
			subNu3 = pos( subNu3 );
			gnu = 1.0 + 1.44 * subNu1 * subNu2 + subNu3; //equation 42
			if ( ra >= 1.0e5 ) {
				nperr = 1001; // Rayleigh number is out of range
				ErrorMessage = "Rayleigh number out of range in Nusselt num. calc. for gaps (angle between 0 and 60 deg).";
			}
			if ( asp <= 20.0 ) {
				nperr = 1002; // Aspect Ratio is out of range
				ErrorMessage = "Aspect Ratio out of range in Nusselt num. calc. for gaps (angle between 0 and 60 deg).";
			}
		} else if ( tilt == 60.0 ) { //ISO/DIS 15099 - chapter 5.3.3.2
			G = 0.5 / std::pow( 1.0 + std::pow( ra / 3160.0, 20.6 ), 0.1 ); //equation 47
			Nu1 = std::pow( 1.0 + pow_7( ( 0.0936 * std::pow( ra, 0.314 ) ) / ( 1.0 + G ) ), 0.1428571 ); //equation 45
			Nu2 = ( 0.104 + 0.175 / asp ) * std::pow( ra, 0.283 ); //equation 46
			gnu = max( Nu1, Nu2 ); //equation 44
		} else if ( ( tilt > 60.0 ) && ( tilt < 90.0 ) ) { //ISO/DIS 15099 - chapter 5.3.3.3
			if ( ( ra > 100.0 ) && ( ra < 2.0e7 ) && ( asp > 5.0 ) && ( asp < 100.0 ) ) {
				G = 0.5 / std::pow( 1.0 + std::pow( ra / 3160.0, 20.6 ), 0.1 ); //equation 47
				Nu1 = std::pow( 1.0 + pow_7( ( 0.0936 * std::pow( ra, 0.314 ) ) / ( 1.0 + G ) ), 0.1428571 ); //equation 45
				Nu2 = ( 0.104 + 0.175 / asp ) * std::pow( ra, 0.283 ); //equation 46
				Nu60 = max( Nu1, Nu2 ); //equation 44
				Nu2 = 0.242 * std::pow( ra / asp, 0.272 ); //equation 52
				if ( ra > 5.0e4 ) {
					Nu1 = 0.0673838 * std::pow( ra, 1.0 / 3.0 ); //equation 49
				} else if ( ( ra > 1.0e4 ) && ( ra <= 5.0e4 ) ) {
					Nu1 = 0.028154 * std::pow( ra, 0.4134 ); //equation 50
				} else if ( ra <= 1.0e4 ) {
					Nu1 = 1.0 + 1.7596678e-10 * std::pow( ra, 2.2984755 ); //equation 51
				}
			} else if ( ra <= 100.0 ) {
				G = 0.5 / std::pow( 1.0 + std::pow( ra / 3160.0, 20.6 ), 0.1 ); //equation 47
				Nu1 = std::pow( 1.0 + pow_7( ( 0.0936 * std::pow( ra, 0.314 ) ) / ( 1.0 + G ) ), 0.1428571 ); //equation 45
				Nu2 = ( 0.104 + 0.175 / asp ) * std::pow( ra, 0.283 ); //equation 46
				Nu60 = max( Nu1, Nu2 ); //equation 44
				Nu2 = 0.242 * std::pow( ra / asp, 0.272 ); //equation 52
				Nu1 = 1.0 + 1.7596678e-10 * std::pow( ra, 2.2984755 ); //equation 51
				nperr = 1003; // Rayleigh number is less than 100
				ErrorMessage = "Rayleigh number is less than 100 in Nusselt number calculations for gaps (angle between 60 and 90 degrees).";
			} else if ( ra > 2.0e7 ) {
				G = 0.5 / std::pow( 1.0 + std::pow( ra / 3160.0, 20.6 ), 0.1 ); //equation 47
				Nu1 = std::pow( 1.0 + pow_7( ( 0.0936 * std::pow( ra, 0.314 ) ) / ( 1.0 + G ) ), 0.1428571 ); //equation 45
				Nu2 = ( 0.104 + 0.175 / asp ) * std::pow( ra, 0.283 ); //equation 46
				Nu60 = max( Nu1, Nu2 ); //equation 44
				Nu2 = 0.242 * std::pow( ra / asp, 0.272 ); //equation 52
				Nu1 = 0.0673838 * std::pow( ra, 1.0 / 3.0 ); //equation 49
				nperr = 1004; // Rayleigh number is great from 2e7
				ErrorMessage = "Rayleigh number is greater than 2e7 in Nusselt number calculations for gaps (angle between 60 and 90 degrees).";
			} else if ( ( asp <= 5.0 ) || ( asp >= 100.0 ) ) {
				G = 0.5 / std::pow( 1.0 + std::pow( ra / 3160.0, 20.6 ), 0.1 ); //equation 47
				Nu1 = std::pow( 1.0 + pow_7( ( 0.0936 * std::pow( ra, 0.314 ) ) / ( 1.0 + G ) ), 0.1428571 ); //equation 45
				Nu2 = ( 0.104 + 0.175 / asp ) * std::pow( ra, 0.283 ); //equation 46
				Nu60 = max( Nu1, Nu2 ); //equation 44
				Nu2 = 0.242 * std::pow( ra / asp, 0.272 ); //equation 52
				if ( ra > 5.0e4 ) {
					Nu1 = 0.0673838 * std::pow( ra, 1.0 / 3.0 ); //equation 49
				} else if ( ( ra > 1.0e4 ) && ( ra <= 5.0e4 ) ) {
					Nu1 = 0.028154 * std::pow( ra, 0.4134 ); //equation 50
				} else if ( ra <= 1.0e4 ) {
					Nu1 = 1.0 + 1.7596678e-10 * std::pow( ra, 2.2984755 ); //equation 51
				}
				nperr = 1005; // Aspect Ratio is out of range
				ErrorMessage = "Aspect Ratio is out of range in Nusselt number calculations for gaps (angle between 60 and 90 degrees).";
			}
			Nu90 = max( Nu1, Nu2 ); //equation 48
			gnu = ( ( Nu90 - Nu60 ) / ( 90.0 - 60.0 ) ) * ( tilt - 60.0 ) + Nu60; //linear interpolation between 60 and 90 degrees
		} else if ( tilt == 90.0 ) { //ISO/DIS 15099 - chapter 5.3.3.4
			Nu2 = 0.242 * std::pow( ra / asp, 0.272 ); //equation 52
			if ( ra > 5.0e4 ) {
				Nu1 = 0.0673838 * std::pow( ra, 1.0 / 3.0 ); //equation 49
			} else if ( ( ra > 1.0e4 ) && ( ra <= 5.0e4 ) ) {
				Nu1 = 0.028154 * std::pow( ra, 0.4134 ); //equation 50
				//Nu1 = 0.028154d0 * ra ** 0.414d0                       !equation 50 - DISCONTINUITY CORRECTED
			} else if ( ra <= 1.0e4 ) {
				Nu1 = 1.0 + 1.7596678e-10 * std::pow( ra, 2.2984755 ); //equation 51
			}
			gnu = max( Nu1, Nu2 ); //equation 48
		} else if ( ( tilt > 90.0 ) && ( tilt <= 180.0 ) ) {
			Nu2 = 0.242 * std::pow( ra / asp, 0.272 ); //equation 52
			if ( ra > 5.0e4 ) {
				Nu1 = 0.0673838 * std::pow( ra, 1.0 / 3.0 ); //equation 49
			} else if ( ( ra > 1.0e4 ) && ( ra <= 5.0e4 ) ) {
				Nu1 = 0.028154 * std::pow( ra, 0.4134 ); //equation 50
			} else if ( ra <= 1.0e4 ) {
				Nu1 = 1.0 + 1.7596678e-10 * std::pow( ra, 2.2984755 ); //equation 51
			}
			gnu = max( Nu1, Nu2 ); //equation 48
			gnu = 1.0 + ( gnu - 1.0 ) * std::sin( tiltr ); //equation 53
		} else {
			nperr = 10; //error flag: angle is out of range
			ErrorMessage = "Window tilt angle is out of range.";
			return;
		}

	}

	//  subroutine picard(nlayer, alpha, Ebb, Ebf, Rf, Rb, Ebbold, Ebfold, Rfold, Rbold)

	//    integer, intent(in) :: nlayer
	//    REAL(r64), intent(in) :: alpha
	//    REAL(r64), intent(in) :: Ebbold(maxlay), Ebfold(maxlay), Rbold(maxlay), Rfold(maxlay)
	//    REAL(r64), intent(inout) :: Ebb(maxlay), Ebf(maxlay), Rb(maxlay), Rf(maxlay)

	//    integer :: i

	//    do i=1,nlayer
	//      Ebb(i) = alpha * Ebb(i) + (1.0d0-alpha) * Ebbold(i)
	//      Ebf(i) = alpha * Ebf(i) + (1.0d0-alpha) * Ebfold(i)
	//      Rb(i) = alpha * Rb(i) + (1.0d0-alpha) * Rbold(i)
	//      Rf(i) = alpha * Rf(i) + (1.0d0-alpha) * Rfold(i)
	//    end do

	//    return
	//  end subroutine picard

	void
	adjusthhat(
		int const SDLayerIndex,
		Array1A_int const ibc,
		Real64 const tout,
		Real64 const tind,
		int const nlayer,
		Array1A< Real64 > const theta,
		Real64 const wso,
		Real64 const wsi,
		int const iwd,
		Real64 const height,
		Real64 const heightt,
		Real64 const tilt,
		Array1A< Real64 > const thick,
		Array1A< Real64 > const gap,
		Real64 const hout,
		Real64 const hrout,
		Real64 const hin,
		Real64 const hrin,
		Array2A_int const iprop,
		Array2A< Real64 > const frct,
		Array1A< Real64 > const presure,
		Array1A_int const nmix,
		Array1A< Real64 > const wght,
		Array2A< Real64 > const gcon,
		Array2A< Real64 > const gvis,
		Array2A< Real64 > const gcp,
		int const index,
		Real64 const SDScalar,
		Array1A< Real64 > const Ebf,
		Array1A< Real64 > const Ebb,
		Array1A< Real64 > hgas,
		Array1A< Real64 > hhat,
		int & nperr,
		std::string & ErrorMessage
	)
	{

		//********************************************************************
		//  Modifies hhat, hgas coefficients around SD layers
		//********************************************************************

		// Using
		using DataGlobals::GravityConstant;

		// Argument array dimensioning
		ibc.dim( 2 );
		theta.dim( maxlay2 );
		thick.dim( maxlay );
		gap.dim( MaxGap );
		iprop.dim( maxgas, maxlay1 );
		frct.dim( maxgas, maxlay1 );
		presure.dim( maxlay1 );
		nmix.dim( maxlay1 );
		wght.dim( maxgas );
		gcon.dim( 3, maxgas );
		gvis.dim( 3, maxgas );
		gcp.dim( 3, maxgas );
		Ebf.dim( maxlay );
		Ebb.dim( maxlay );
		hgas.dim( maxlay );
		hhat.dim( maxlay3 );

		// Locals
		Real64 hc_NOSD( 0.0 );
		Real64 hc_0;
		Real64 hc_1;
		Real64 hc_alpha;
		Real64 hhat_alpha;
		Real64 hc_1_1;
		Real64 hc_1_2;
		Real64 hc_alpha1;
		Real64 hc_alpha2;
		Real64 hhat_alpha1;
		Real64 hhat_alpha2;
		Array1D< Real64 > frctg( maxgas );
		int i;
		int j;
		int k;
		int l;
		Array1D_int ipropg( maxgas );
		Real64 tmean;
		Real64 con;
		Real64 visc;
		Real64 dens;
		Real64 cp;
		Real64 pr;
		Real64 delt;
		Real64 gap_NOSD;
		Real64 rayl;
		Real64 asp;
		Real64 gnu;

		//bi...  Step 1: Calculate hc as if there was no SD here
		if ( SDLayerIndex == 1 ) {
			//car    SD is the first layer (outdoor)
			// calc hc_0 as hcout:
			// convective outdoor film coeff:
			if ( ibc( 1 ) <= 0 ) {
				film( tout, theta( 3 ), wso, iwd, hc_NOSD, ibc( 1 ) );
			} else if ( ibc( 1 ) == 1 ) {
				hc_NOSD = hout - hrout;
			} else if ( ( ibc( 1 ) == 2 ) && ( index == 1 ) ) {
				hc_NOSD = hout;
			} else {
				assert( false );
			}
			if ( hc_NOSD < 0 ) {
				nperr = 9;
				ErrorMessage = "Hcout is out of range.";
				return;
			}
			hc_0 = hc_NOSD * ( theta( 3 ) - tout ) / ( theta( 3 ) - theta( 2 ) );
			hc_1 = hgas( 2 );
			hc_alpha = SDScalar * ( hc_1 - hc_0 ) + hc_0;
			hhat_alpha = hc_alpha * ( theta( 3 ) - theta( 2 ) ) / ( Ebf( 2 ) - Ebb( 1 ) );

			hgas( 2 ) = hc_alpha;
			hhat( 3 ) = hhat_alpha;
		} else if ( SDLayerIndex == nlayer ) {
			//car    SD is the last layer (indoor)
			// calc hc_0 as hcin:
			// convective indoor film coeff:
			if ( ibc( 2 ) <= 0 ) {
				filmi( tind, theta( 2 * nlayer - 2 ), nlayer, tilt, wsi, heightt, iprop, frct, presure, nmix, wght, gcon, gvis, gcp, hc_NOSD, ibc( 2 ), nperr, ErrorMessage );
			} else if ( ibc( 2 ) == 1 ) {
				hc_NOSD = hin - hrin;
			} else if ( ibc( 2 ) == 2 && index == 1 ) {
				hc_NOSD = hin;
			}
			if ( hc_NOSD < 0 ) {
				nperr = 8;
				ErrorMessage = "Hcin is out of range.";
				return;
			}
			//    hgas(2*nlayer+1) = hcin
			hc_0 = hc_NOSD * ( tind - theta( 2 * nlayer - 2 ) ) / ( theta( 2 * nlayer - 1 ) - theta( 2 * nlayer - 2 ) );
			hc_1 = hgas( nlayer + 1 );
			hc_alpha = SDScalar * ( hc_1 - hc_0 ) + hc_0;
			hhat_alpha = hc_alpha * ( theta( 2 * nlayer - 1 ) - theta( 2 * nlayer - 2 ) ) / ( Ebf( nlayer ) - Ebb( nlayer - 1 ) );

			hgas( nlayer + 1 ) = hc_alpha;
			hhat( 2 * nlayer - 1 ) = hhat_alpha;
		} else {
			//car     SD is in between glazing
			//  calc hc_NOSD as hcgas:
			j = 2 * SDLayerIndex - 2;
			k = j + 3;
			// determine the gas properties for this "gap":
			tmean = ( theta( j ) + theta( k ) ) / 2.0;
			delt = std::abs( theta( j ) - theta( k ) );
			i = SDLayerIndex;
			for ( l = 1; l <= nmix( i + 1 ); ++l ) {
				ipropg( l ) = iprop( l, i + 1 );
				frctg( l ) = frct( l, i + 1 );
			}
			GASSES90( tmean, ipropg, frctg, presure( i + 1 ), nmix( i + 1 ), wght, gcon, gvis, gcp, con, visc, dens, cp, pr, ISO15099, nperr, ErrorMessage );
			gap_NOSD = gap( SDLayerIndex - 1 ) + gap( SDLayerIndex ) + thick( SDLayerIndex );
			// determine the Rayleigh number:
			rayl = GravityConstant * pow_3( gap_NOSD ) * delt * cp * pow_2( dens ) / ( tmean * visc * con );
			asp = height / gap_NOSD;
			// determine the Nusselt number:
			nusselt( tilt, rayl, asp, gnu, nperr, ErrorMessage );
			// calculate effective conductance of the gap
			hc_NOSD = con / gap_NOSD * gnu;
			i = SDLayerIndex;
			j = 2 * i;
			//car     changed hc interpolation with temperatures by inverting ratios
			//car  hc_0_1 = hc_NOSD*ABS((theta(2*i-1) - theta(2*i-2))/(theta(2*i+1) - theta(2*i-2)))
			//car  hc_0_2 = hc_NOSD*ABS((theta(2*i+1) - theta(2*i))/(theta(2*i+1) - theta(2*i-2)))
			hc_1_1 = hgas( i );
			hc_1_2 = hgas( i + 1 );

			//car    changed how SDscalar interpolation is done.  Instead of hc_0_1 and hc_0_2, hc_NOSD is used
			hc_alpha1 = SDScalar * ( hc_1_1 - hc_NOSD ) + hc_NOSD;
			hc_alpha2 = SDScalar * ( hc_1_2 - hc_NOSD ) + hc_NOSD;

			hhat_alpha1 = hc_alpha1 * ( theta( j - 1 ) - theta( j - 2 ) ) / ( Ebf( i ) - Ebb( i - 1 ) );
			hhat_alpha2 = hc_alpha2 * ( theta( j + 1 ) - theta( j ) ) / ( Ebf( i + 1 ) - Ebb( i ) );

			hgas( i ) = hc_alpha1;
			hgas( i + 1 ) = hc_alpha2;
			hhat( j - 1 ) = hhat_alpha1;
			hhat( j + 1 ) = hhat_alpha2;
		}

	}

	void
	storeIterationResults(
		int const nlayer,
		int const index,
		Array1< Real64 > const & theta,
		Real64 const trmout,
		Real64 const tamb,
		Real64 const trmin,
		Real64 const troom,
		Real64 const ebsky,
		Real64 const ebroom,
		Real64 const hcin,
		Real64 const hcout,
		Real64 const hrin,
		Real64 const hrout,
		Real64 const hin,
		Real64 const hout,
		Array1< Real64 > const & Ebb,
		Array1< Real64 > const & Ebf,
		Array1< Real64 > const & Rb,
		Array1< Real64 > const & Rf,
		int & EP_UNUSED( nperr )
	)
	{

		// Using/Aliasing
		using DataGlobals::KelvinConv;

		// Locals
		//character(len=*), intent(inout) :: ErrorMessage

		//localy used
		static std::string dynFormat;
		std::string a;
		int i;

		// Formats
		static gio::Fmt Format_1000( "(I3)" );

		//open(unit=InArgumentsFile,  file=TRIM(DBGD)//'TarcogIterations.dbg',  status='unknown', position='APPEND',  &
		//          &  form='formatted', iostat=nperr)

		//if (nperr.ne.0)  open(unit=InArgumentsFile,  file='TarcogIterations.dbg',  status='unknown', position='APPEND',  &
		//          &  form='formatted', iostat=nperr)

		//open(unit=IterationCSV,  file=TRIM(DBGD)//TRIM(IterationCSVName),  status='unknown', position='APPEND',  &
		//          &  form='formatted', iostat=nperr)

		//if (nperr.ne.0)  open(unit=IterationCSV,  file=TRIM(IterationCSVName),  status='unknown', position='APPEND',  &
		//          &  form='formatted', iostat=nperr)

		//open(unit=IterationHHAT,  file=TRIM(DBGD)//TRIM(IterationHHATName),  status='unknown', position='APPEND',  &
		//          &  form='formatted', iostat=nperr)

		//if (nperr.ne.0)  open(unit=IterationHHAT,  file=TRIM(IterationHHATName),  status='unknown', position='APPEND',  &
		//          &  form='formatted', iostat=nperr)

		//write(a,1000) index
		gio::write( TarcogIterationsFileNumber, "('*************************************************************************************************')" );
		gio::write( TarcogIterationsFileNumber, "('Iteration number: ', i5)" ) << index;

		gio::write( TarcogIterationsFileNumber, "('Trmin = ', f8.4)" ) << trmin - KelvinConv;
		gio::write( TarcogIterationsFileNumber, "('Troom = ', f12.6)" ) << troom - KelvinConv;
		gio::write( TarcogIterationsFileNumber, "('Trmout = ', f8.4)" ) << trmout - KelvinConv;
		gio::write( TarcogIterationsFileNumber, "('Tamb = ', f12.6)" ) << tamb - KelvinConv;

		gio::write( TarcogIterationsFileNumber, "('Ebsky = ', f8.4)" ) << ebsky;
		gio::write( TarcogIterationsFileNumber, "('Ebroom = ', f8.4)" ) << ebroom;

		gio::write( TarcogIterationsFileNumber, "('hcin = ', f8.4)" ) << hcin;
		gio::write( TarcogIterationsFileNumber, "('hcout = ', f8.4)" ) << hcout;
		gio::write( TarcogIterationsFileNumber, "('hrin = ', f8.4)" ) << hrin;
		gio::write( TarcogIterationsFileNumber, "('hrout = ', f8.4)" ) << hrout;
		gio::write( TarcogIterationsFileNumber, "('hin = ', f8.4)" ) << hin;
		gio::write( TarcogIterationsFileNumber, "('hout = ', f8.4)" ) << hout;

		//Write headers for Ebb and Ebf
		for ( i = 1; i <= 2 * nlayer; ++i ) {

			gio::write( a, Format_1000 ) << ( i + 1 ) / 2; //this is just to simulate correct integer in brackets
			if ( i == 1 ) {
				dynFormat = "('";
			}
			if ( mod( i, 2 ) == 1 ) {
				dynFormat += "Ebf(" + a + ')';
			} else {
				dynFormat += "Ebb(" + a + ')';
			}
			if ( i == 2 * nlayer ) {
				dynFormat += "')";
			} else {
				dynFormat += "===";
			}
		}
		gio::write( TarcogIterationsFileNumber, dynFormat );

		//write Ebb and Ebf
		gio::write( TarcogIterationsFileNumber, "(f16.8,'   ',f16.8,$)" )
			<< Ebf( 1 ) << Ebb( 1 );
		for ( i = 2; i <= nlayer; ++i ) {
			gio::write( TarcogIterationsFileNumber, "('   ',f16.8,'   ',f16.8,$)" ) << Ebf( i ) << Ebb( i );
		}
		gio::write( TarcogIterationsFileNumber );

		//Write headers for Rb and Rf
		for ( i = 1; i <= 2 * nlayer; ++i ) {

			gio::write( a, Format_1000 ) << ( i + 1 ) / 2; //this is just to simulate correct integer in brackets
			if ( i == 1 ) {
				dynFormat = "('";
			}
			if ( mod( i, 2 ) == 1 ) {
				dynFormat += "Rf(" + a + ')';
			} else {
				dynFormat += "Rb(" + a + ')';
			}
			if ( i == 2 * nlayer ) {
				dynFormat += "')";
			} else {
				dynFormat += "===";
			}
		}
		gio::write( TarcogIterationsFileNumber, dynFormat );

		//write Rb and Rf
		gio::write( TarcogIterationsFileNumber, "(f16.8,'   ',f16.8,$)" ) << Rf( 1 ) << Rb( 1 );
		for ( i = 1; i <= nlayer; ++i ) {
			gio::write( TarcogIterationsFileNumber, "('   ',f16.8,'   ',f16.8,$)" ) << Rf( i ) << Rb( i );
		}
		gio::write( TarcogIterationsFileNumber );

		//Write header for temperatures
		for ( i = 1; i <= 2 * nlayer; ++i ) {

			gio::write( a, Format_1000 ) << i;
			if ( i == 1 ) {
				dynFormat = "('";
			}
			dynFormat += "theta(" + a + ')';
			if ( i == ( 2 * nlayer ) ) {
				dynFormat += "')";
			} else {
				dynFormat += "==";
			}
		}
		gio::write( TarcogIterationsFileNumber, dynFormat );

		//write temperatures
		gio::write( TarcogIterationsFileNumber, "(f16.8,'   ',f16.8,$)" ) << theta( 1 ) - KelvinConv;
		for ( i = 2; i <= 2 * nlayer; ++i ) {
			gio::write( TarcogIterationsFileNumber, "('   ',f16.8,'   ',f16.8,$)" ) << theta( i ) - KelvinConv;
		}
		gio::write( TarcogIterationsFileNumber );

		//close(TarcogIterationsFileNumber)

		//write results in csv file
		if ( index == 0 ) {
			dynFormat = "('  ";
			for ( i = 1; i <= 2 * nlayer; ++i ) {
				gio::write( a, Format_1000 ) << i; //this is just to simulate correct integer in brackets
				if ( i != 2 * nlayer ) {
					dynFormat += "theta(" + a + "),";
				} else {
					dynFormat += "theta(" + a + ')';
				}
			}
			dynFormat += "')";
			gio::write( IterationCSVFileNumber, dynFormat );
		}
		gio::write( IterationCSVFileNumber, "(f16.8,'   ',f16.8,$)" ) << theta( 1 ) - KelvinConv;
		for ( i = 2; i <= 2 * nlayer; ++i ) {
			gio::write( IterationCSVFileNumber, "('   ',f16.8,'   ',f16.8,$)" ) << theta( i ) - KelvinConv;
		}
		gio::write( IterationCSVFileNumber );

		//close(IterationCSVFileNumber)

	}

	void
	CalculateFuncResults(
		int const nlayer,
		Array2< Real64 > const & a,
		Array1< Real64 > const & b,
		Array1< Real64 > const & x,
		Array1< Real64 > & FRes
	)
	{
		//calculate balance equations by using temperature solution and estimates stores error in FRes
		//REAL(r64), intent(in) :: theta(maxlay2)
		//REAL(r64), intent(in) :: R(maxlay2)  ! Radiation on layer surfaces

		// Locals
		//integer, intent(out) :: nperr
		//character(len=*), intent(out) :: ErrorMessage

		//Local variables

		//Tuned Rewritten to traverse a in unit stride order
		int const nlayer4( 4 * nlayer );
		for ( int i = 1; i <= nlayer4; ++i ) {
			FRes( i ) = -b( i );
		}
		for ( int j = 1; j <= nlayer4; ++j ) {
			Real64 const x_j( x( j ) );
			for ( int i = 1; i <= nlayer4; ++i ) {
				FRes( i ) += a( j, i ) * x_j;
			}
		}

	}

} // ThermalISO15099Calc

} // EnergyPlus
