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
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <ThermalEN673Calc.hh>
#include <DataGlobals.hh>
#include <TARCOGArgs.hh>
#include <TARCOGGasses90.hh>
#include <TARCOGGassesParams.hh>
#include <TARCOGCommon.hh>
#include <TARCOGOutput.hh>
#include <TARCOGParams.hh>

namespace EnergyPlus {

namespace ThermalEN673Calc {

	// MODULE INFORMATION:
	//       AUTHOR         D. Charlie Curcija
	//       DATE WRITTEN   July/2000
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// Calculate thermal properties of IGU according to EN673 standard

	// METHODOLOGY EMPLOYED:
	// <description>

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// USE STATEMENTS:

	// Using/Aliasing
	using namespace TARCOGCommon;
	using namespace TARCOGGassesParams;
	using namespace TARCOGGasses90;
	using namespace TARCOGParams;

	// Functions

	void
	Calc_EN673(
		int const standard,
		int const nlayer,
		Real64 const tout,
		Real64 const tind,
		Array1A< Real64 > gap,
		Array1A< Real64 > thick,
		Array1A< Real64 > scon,
		Array1A< Real64 > const emis,
		Real64 const totsol,
		Real64 const tilt,
		Real64 const dir,
		Array1A< Real64 > const asol,
		Array1A< Real64 > const presure,
		Array2A_int const iprop,
		Array2A< Real64 > const frct,
		Array1A_int const nmix,
		Array2A< Real64 > const xgcon,
		Array2A< Real64 > const xgvis,
		Array2A< Real64 > const xgcp,
		Array1A< Real64 > const xwght,
		Array1A< Real64 > theta,
		Real64 & ufactor,
		Real64 & hcin,
		Real64 & hin,
		Real64 & hout,
		Real64 & shgc,
		int & nperr,
		std::string & ErrorMessage,
		Array1A_int const ibc,
		Array1A< Real64 > hg,
		Array1A< Real64 > hr,
		Array1A< Real64 > hs,
		Array1A< Real64 > Ra,
		Array1A< Real64 > Nu
	)
	{

		// Using/Aliasing
		using TARCOGArgs::GoAhead;
		using namespace TARCOGOutput;

		/// function attributes:

		/// INPUTS:

		/// General:

		// Argument array dimensioning
		gap.dim( MaxGap );
		thick.dim( maxlay );
		scon.dim( maxlay );
		emis.dim( maxlay2 );
		asol.dim( maxlay );
		presure.dim( maxlay1 );
		iprop.dim( maxgas, maxlay1 );
		frct.dim( maxgas, maxlay1 );
		nmix.dim( maxlay1 );
		xgcon.dim( 3, maxgas );
		xgvis.dim( 3, maxgas );
		xgcp.dim( 3, maxgas );
		xwght.dim( maxgas );
		theta.dim( maxlay2 );
		ibc.dim( 2 );
		hg.dim( maxlay );
		hr.dim( maxlay );
		hs.dim( maxlay );
		Ra.dim( maxlay );
		Nu.dim( maxlay );

		// Locals
		/// Environment related:

		/// Layers:

		/// Gaps:

		//// INPUTS/OUTPUTS:

		/// OUTPUTS:
		/// Overall:

		/// Layers:

		/// Gaps:

		Array1D< Real64 > rs( maxlay3 );
		Real64 rtot;
		Real64 sft;

		//call  propcon90(standard, mgas, gcon, gvis, gcp, grho, wght, nperr)
		rtot = 0.0;
		sft = 0.0;
		if ( GoAhead( nperr ) ) {
			EN673ISO10292( nlayer, tout, tind, emis, gap, thick, scon, tilt, iprop, frct, xgcon, xgvis, xgcp, xwght, presure, nmix, theta, standard, hg, hr, hs, hin, hout, hcin, ibc, rs, ufactor, Ra, Nu, nperr, ErrorMessage );

			if ( GoAhead( nperr ) ) {
				rtot = 1.0 / ufactor;
				solar_EN673( dir, totsol, rtot, rs, nlayer, asol, sft, standard, nperr, ErrorMessage );
				if ( GoAhead( nperr ) ) {
					shgc = sft;
					if ( WriteDebugOutput ) WriteOutputEN673( OutArgumentsFile, DBGD, nlayer, ufactor, hout, hin, Ra, Nu, hg, hr, hs, nperr );
				} // GoAhead after solar
			} // GoAhead after EN673ISO10292
		} // GopAhead after propcon90

	}

	void
	EN673ISO10292(
		int const nlayer,
		Real64 const tout,
		Real64 const tind,
		Array1A< Real64 > const emis,
		Array1A< Real64 > const gap,
		Array1A< Real64 > const thick,
		Array1A< Real64 > const scon,
		Real64 const tilt,
		Array2A_int const iprop,
		Array2A< Real64 > const frct,
		Array2A< Real64 > const xgcon,
		Array2A< Real64 > const xgvis,
		Array2A< Real64 > const xgcp,
		Array1A< Real64 > const xwght,
		Array1A< Real64 > const presure,
		Array1A_int const nmix,
		Array1A< Real64 > theta,
		int const standard,
		Array1A< Real64 > hg,
		Array1A< Real64 > hr,
		Array1A< Real64 > hs,
		Real64 & hin,
		Real64 const hout,
		Real64 & hcin,
		Array1A_int const ibc,
		Array1A< Real64 > rs,
		Real64 & ufactor,
		Array1A< Real64 > Ra,
		Array1A< Real64 > Nu,
		int & nperr,
		std::string & ErrorMessage
	)
	{
		// Using
		using DataGlobals::GravityConstant;
		using DataGlobals::StefanBoltzmann;

		// Argument array dimensioning
		emis.dim( maxlay2 );
		gap.dim( MaxGap );
		thick.dim( maxlay );
		scon.dim( maxlay );
		iprop.dim( maxgas, maxlay1 );
		frct.dim( maxgas, maxlay1 );
		xgcon.dim( 3, maxgas );
		xgvis.dim( 3, maxgas );
		xgcp.dim( 3, maxgas );
		xwght.dim( maxgas );
		presure.dim( maxlay1 );
		nmix.dim( maxlay1 );
		theta.dim( maxlay2 );
		hg.dim( maxlay );
		hr.dim( maxlay );
		hs.dim( maxlay );
		ibc.dim( 2 );
		rs.dim( maxlay3 );
		Ra.dim( maxlay );
		Nu.dim( maxlay );

		// Locals
		//dr...internal variables
		Real64 Tm;
		Real64 diff;
		Real64 Rg;
		Array1D< Real64 > dT( maxlay1 );
		int i;
		int j;
		int iter;
		Real64 dens;
		Real64 visc;
		Real64 con;
		Real64 cp;
		Real64 pr;
		Array1D< Real64 > Gr( maxlay );
		Real64 A;
		Real64 n;
		Real64 hrin;
		Real64 sumRs;
		Real64 sumRsold;

		Real64 const eps( 1.0e-4 ); // set iteration accuracy

		Array1D< Real64 > frctg( maxgas );
		Array1D_int ipropg( maxgas );

		//jel..hrin is 4.4 for standard clear glass:
		if ( ( emis( 2 * nlayer ) < 0.85 ) && ( emis( 2 * nlayer ) > 0.83 ) ) {
			hrin = 4.4;
		} else {
			hrin = 4.4 * emis( 2 * nlayer ) / 0.837;
			//       hrin = 4.4 * emis(2*nlayer) / 0.84  !old formula
		}

		if ( ibc( 1 ) != 1 ) {
			nperr = 38;
			ErrorMessage = "Boundary conditions for EN673 can be combined hout for outdoor and either convective (hcin) or combined (hin) for indoor.  Others are not supported currently.";
			return;
		}

		if ( ibc( 2 ) == 1 ) {

		} else if ( ibc( 2 ) == 2 ) {
			hcin = hin;
			hin = hcin + hrin;
		} else {
			nperr = 39;
			ErrorMessage = "CSM and SCW thermal models cannot be used for outdoor and indoor SD layers.";
			return;
		}

		rs( 1 ) = 1.0 / hout;
		rs( 2 * nlayer + 1 ) = 1.0 / hin;

		Tm = 283.0;
		iter = 1;
		sumRs = 0.0;
		Rg = 0.0;

		//bi Init vectors:
		Gr = 0.0;
		Nu = 0.0;
		Ra = 0.0;
		con = 0.0;

		for ( i = 1; i <= nlayer; ++i ) {
			rs( 2 * i ) = thick( i ) / scon( i ); // thermal resistance of each glazing layer
			Rg += rs( 2 * i ); // cumulative thermal resistance of glazing layers
		}

		if ( nlayer == 1 ) { // Calc U-Factor and glazing temperature for simgle glazing and return
			ufactor = 1.0 / ( 1.0 / hin + 1.0 / hout + Rg );
			theta( 1 ) = ufactor * ( tind - tout ) / hout + tout;
			theta( 2 ) = tind - ufactor * ( tind - tout ) / hin;
			return;
		} else {
			if ( tind > tout ) {
				//dr...linear interpolation for gas conductance coefficients
				if ( tilt == 0.0 ) {
					A = 0.16;
					n = 0.28;
				} else if ( ( tilt > 0.0 ) && ( tilt < 45.0 ) ) {
					linint( 0.0, 45.0, 0.16, 0.1, tilt, A );
					linint( 0.0, 45.0, 0.28, 0.31, tilt, n );
				} else if ( tilt == 45.0 ) {
					A = 0.10;
					n = 0.31;
				} else if ( ( tilt > 45.0 ) && ( tilt < 90.0 ) ) {
					linint( 45.0, 90.0, 0.1, 0.035, tilt, A );
					linint( 45.0, 90.0, 0.31, 0.38, tilt, n );
				} else if ( tilt == 90 ) {
					A = 0.035;
					n = 0.38;
				} // tilt

				//c   gas constants
				//    open(unit=18,  file='gas.dbg',  status='unknown', position='APPEND',
				//  2            form='formatted', iostat=nperr)
				//    write(18,*) 'New calc'
				for ( i = 1; i <= nlayer - 1; ++i ) {
					//22222  format('Gas #', I3, ' : Dens=', F9.7, ' Visc=', F12.9, ' Cond=', F9.7, ' Cp=', F9.7)
					//   write(18, 22222) iprop(i+1, j), tempDens, gvis(iprop(i+1,j), 1), gcon(iprop(i+1,j), 1), gcp(iprop(i+1,j), 1)
					dT( i ) = 15.0 / ( nlayer - 1 ); // set initial temperature distribution
					for ( j = 1; j <= nmix( i + 1 ); ++j ) {
						ipropg( j ) = iprop( j, i + 1 );
						frctg( j ) = frct( j, i + 1 );
					}
					GASSES90( Tm, ipropg, frctg, presure( i + 1 ), nmix( i + 1 ), xwght, xgcon, xgvis, xgcp, con, visc, dens, cp, pr, standard, nperr, ErrorMessage );
					Gr( i ) = ( GravityConstant * pow_3( gap( i ) ) * dT( i ) * pow_2( dens ) ) / ( Tm * pow_2( visc ) );
					Ra( i ) = Gr( i ) * pr;
					Nu( i ) = A * std::pow( Ra( i ), n );
					if ( Nu( i ) < 1.0 ) {
						Nu( i ) = 1.0;
					}
					hg( i ) = Nu( i ) * con / gap( i );
				} // gaps
			} else {
				for ( i = 1; i <= nlayer - 1; ++i ) {
					Nu( i ) = 1.0;
					hg( i ) = Nu( i ) * con / gap( i ); //Autodesk:Uninit con was uninitialized
				}
			}
			for ( i = 1; i <= nlayer - 1; ++i ) {
				hr( i ) = 4.0 * StefanBoltzmann * std::pow( 1.0 / emis( 2 * i ) + 1.0 / emis( 2 * i + 1 ) - 1.0, -1.0 ) * pow_3( Tm );
				hs( i ) = hg( i ) + hr( i );
				rs( 2 * i + 1 ) = 1.0 / hs( i ); // Thermal resistance of each gap
				sumRs += rs( 2 * i + 1 );
			}
			//    write(18,*) '------'
			//    close(18)

			ufactor = 1.0 / ( 1.0 / hin + 1.0 / hout + sumRs + Rg );
			theta( 1 ) = ufactor * ( tind - tout ) / hout + tout;
			theta( 2 * nlayer ) = tind - ufactor * ( tind - tout ) / hin;
			for ( i = 2; i <= nlayer; ++i ) {
				theta( 2 * i - 2 ) = ufactor * ( tind - tout ) * thick( 1 ) / scon( 1 ) + theta( 2 * i - 3 );
				theta( 2 * i - 1 ) = ufactor * ( tind - tout ) / hs( i - 1 ) + theta( 2 * i - 2 );
			} // end of first iteration

			//bi More iterations:
			while ( true ) {
				sumRsold = sumRs;
				sumRs = 0.0;

				if ( ( standard == EN673 ) && ( nlayer == 2 ) ) {
					return; // If EN673 declared values path and glazing has 2 layers, end claculations and return
				} else {
					if ( tind > tout ) {
						for ( i = 1; i <= nlayer - 1; ++i ) {
							dT( i ) = 15.0 * ( 1.0 / hs( i ) ) / sumRsold; // updated temperature distribution
							if ( standard == EN673 ) {
								Tm = 283.0;
							} else {
								Tm = ( theta( 2 * i ) + theta( 2 * i + 1 ) ) / 2.0;
							}
							for ( j = 1; j <= nmix( i + 1 ); ++j ) {
								ipropg( j ) = iprop( j, i + 1 );
								frctg( j ) = frct( j, i + 1 );
							} // j, gas mix
							GASSES90( Tm, ipropg, frctg, presure( i + 1 ), nmix( i + 1 ), xwght, xgcon, xgvis, xgcp, con, visc, dens, cp, pr, standard, nperr, ErrorMessage );
							Gr( i ) = ( GravityConstant * pow_3( gap( i ) ) * dT( i ) * pow_2( dens ) ) / ( Tm * pow_2( visc ) );
							Ra( i ) = Gr( i ) * pr;
							Nu( i ) = A * std::pow( Ra( i ), n );
							if ( Nu( i ) < 1.0 ) {
								Nu( i ) = 1.0;
							}
							hg( i ) = Nu( i ) * con / gap( i );
						} // i, gaps
					} else {
						for ( i = 1; i <= nlayer - 1; ++i ) {
							Nu( i ) = 1.0;
							hg( i ) = Nu( i ) * con / gap( i ); //Autodesk:Uninit con was possibly uninitialized
						}
					} // tind > tout
				}

				for ( i = 1; i <= nlayer - 1; ++i ) {
					//      hr(i) = 4 * sigma * (1/emis(2*i) + 1/emis(2*i+1) - 1)**(-1) * Tm**3
					hs( i ) = hg( i ) + hr( i );
					rs( 2 * i + 1 ) = 1.0 / hs( i ); // Thermal resistance of each gap
					sumRs += rs( 2 * i + 1 );
				}
				ufactor = 1.0 / ( 1.0 / hin + 1.0 / hout + sumRs + Rg );
				theta( 1 ) = ufactor * ( tind - tout ) / hout + tout;
				theta( 2 * nlayer ) = tind - ufactor * ( tind - tout ) / hin;
				for ( i = 2; i <= nlayer; ++i ) {
					theta( 2 * i - 2 ) = ufactor * ( tind - tout ) * thick( 1 ) / scon( 1 ) + theta( 2 * i - 3 );
					theta( 2 * i - 1 ) = ufactor * ( tind - tout ) / hs( i - 1 ) + theta( 2 * i - 2 );
				}
				++iter; // end of next iteration
				diff = std::abs( sumRs - sumRsold );
				//bi: perhaps we should also limit No. of iterations?
				if ( diff < eps ) break; // tolerance was met - exit loop
			} // remaining iterations
		}

		//dr...END OF ITERATIONS

	}

	void
	linint(
		Real64 const x1,
		Real64 const x2,
		Real64 const y1,
		Real64 const y2,
		Real64 const x,
		Real64 & y
	)
	{

		y = ( y2 - y1 ) / ( x2 - x1 ) * ( x - x1 ) + y1; //Autodesk:DivZero Should protect against divide by zero

	}

	void
	solar_EN673(
		Real64 const dir,
		Real64 const totsol,
		Real64 const rtot,
		Array1A< Real64 > const rs,
		int const nlayer,
		Array1A< Real64 > const absol,
		Real64 & sf,
		int const standard,
		int & nperr,
		std::string & ErrorMessage
	)
	{
		//***********************************************************************
		//   This subroutine calculates the shading coefficient for a window.
		//***********************************************************************
		//  Inputs:
		//    absol     array of absorped fraction of solar radiation in lites
		//    totsol    total solar transmittance
		//    rtot      total thermal resistance of window
		//    rs        array of thermal resistances of each gap and layer
		//    layer     number of layers
		//  Outputs:
		//    sf        solar gain of space

		// Argument array dimensioning
		rs.dim( maxlay3 );
		absol.dim( maxlay );

		// Locals
		int i;
		int j;
		Real64 fract;
		Real64 flowin;

		fract = 0.0;
		flowin = 0.0;
		sf = 0.0;

		// evaluate inward flowing fraction of absorbed radiation:
		if ( ( standard == EN673 ) || ( standard == EN673Design ) ) {
			if ( nlayer == 1 ) {
				fract = dir * absol( 1 ) * ( rs( 1 ) * rs( 3 ) ) / ( rs( 1 ) * ( rs( 1 ) + rs( 3 ) ) );
			} else {
				flowin = ( rs( 1 ) + 0.5 * rs( 2 ) ) / rtot;
				fract = dir * absol( 1 ) * rs( 10 );
				for ( i = 2; i <= nlayer; ++i ) {
					j = 2 * i;
					flowin += ( 0.5 * ( rs( j - 2 ) + 0.5 * rs( j ) ) + rs( j - 1 ) ) / rtot;
					fract += absol( i ) * flowin;
				}
				fract += dir * absol( nlayer ) * rs( 2 * nlayer ) / 2.0;
			}
		} else {
			nperr = 28;
			ErrorMessage = "Invalid code for standard.";
			return;
		}

		sf = totsol + fract; // add inward fraction to directly transmitted fraction

	}

} // ThermalEN673Calc

} // EnergyPlus
