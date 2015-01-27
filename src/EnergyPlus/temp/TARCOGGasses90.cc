// C++ Headers
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <TARCOGGasses90.hh>
#include <DataGlobals.hh>
#include <TARCOGGassesParams.hh>

namespace EnergyPlus {

namespace TARCOGGasses90 {

	// MODULE INFORMATION:
	//       AUTHOR         D. Charlie Curcija
	//       DATE WRITTEN   June/2000
	//       MODIFIED       (see revision history bellow)
	//       RE-ENGINEERED  na
	//  Revision: 7.0.02  (November/8/2011), Simon Vidanovic
	//   - feature: Error message (string) return from gasses
	//  Revision: 7.0.00  (September/6/2011), Simon Vidanovic
	//   - Introduction of vacuum coefficients and routine to calculate low gas pressure conductance
	//  Revision: 6.3.09  (August/23/2011), Simon Vidanovic
	//   - Removed GetGasIndex function which could cause a double usage of gas coefficients
	//     and therefore introducing new bugs.

	// PURPOSE OF THIS MODULE:
	// A module containing functions for gas properties calculation

	// METHODOLOGY EMPLOYED:
	// <description>

	// REFERENCES:
	// ISO15099, EN673

	// OTHER NOTES:
	// na

	// USE STATEMENTS:

	// Using/Aliasing
	using namespace DataGlobals;
	using namespace TARCOGGassesParams;

	// Data
	//private doe2gas90

	// Functions

	void
	GASSES90(
		Real64 const tmean,
		FArray1A_int const iprop,
		FArray1A< Real64 > const frct,
		Real64 const pres,
		int const nmix,
		FArray1A< Real64 > const xwght,
		FArray2A< Real64 > const xgcon,
		FArray2A< Real64 > const xgvis,
		FArray2A< Real64 > const xgcp,
		Real64 & con,
		Real64 & visc,
		Real64 & dens,
		Real64 & cp,
		Real64 & pr,
		int const standard,
		int & nperr,
		std::string & ErrorMessage
	)
	{

		// Variables

		// Argument array dimensioning
		iprop.dim( maxgas );
		frct.dim( maxgas );
		xwght.dim( maxgas );
		xgcon.dim( maxgas, 3 );
		xgvis.dim( maxgas, 3 );
		xgcp.dim( maxgas, 3 );

		// Locals

		static Real64 const sqrt_2( std::sqrt( 2.0 ) );

		int i;
		int j;

		FArray1D< Real64 > fvis( maxgas );
		FArray1D< Real64 > fcon( maxgas );
		FArray1D< Real64 > fdens( maxgas );
		FArray1D< Real64 > fcp( maxgas );
		FArray1D< Real64 > kprime( maxgas );
		FArray1D< Real64 > kdblprm( maxgas );
		FArray1D< Real64 > mukpdwn( maxgas );
		FArray1D< Real64 > kpdown( maxgas );
		FArray1D< Real64 > kdpdown( maxgas );
		Real64 molmix;
		Real64 cpmixm;
		Real64 kpmix;
		Real64 kdpmix;
		Real64 kmix;
		Real64 mumix;
		Real64 phimup;
		Real64 downer;
		Real64 psiup;
		Real64 psiterm;
		Real64 phikup;
		Real64 rhomix;

		//Simon: TODO: this is used for EN673 calculations and it is not assigned properly. Check this
		//REAL(r64), dimension(maxgas, 3) :: xgrho //Autodesk:Unused
		FArray2D< Real64 > grho( maxgas, 3 );

		//REAL(r64) gaslaw
		//DATA gaslaw /8314.51d0/   ! Molar gas constant in Joules/(kmol*K)

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const ENpressure( 1.0e5 ); // Gap gas pressure (Pa)
		Real64 const gaslaw( 8314.51 ); // Molar gas constant (J/kMol-K)

		//!! Body of GASSES90

		//Autodesk:Uninit Initialize variables used uninitialized
		//xgrho = 0.0d0 //Autodesk:Uninit Force default initialization

		con = 0.0;
		visc = 0.0;
		dens = 0.0;
		cp = 0.0;

		//Simon: remove this when assigned properly
		grho = 0.0;

		Real64 const tmean_2( pow_2( tmean ) );
		fcon( 1 ) = xgcon( iprop( 1 ), 1 ) + xgcon( iprop( 1 ), 2 ) * tmean + xgcon( iprop( 1 ), 3 ) * tmean_2;
		fvis( 1 ) = xgvis( iprop( 1 ), 1 ) + xgvis( iprop( 1 ), 2 ) * tmean + xgvis( iprop( 1 ), 3 ) * tmean_2;
		fcp( 1 ) = xgcp( iprop( 1 ), 1 ) + xgcp( iprop( 1 ), 2 ) * tmean + xgcp( iprop( 1 ), 3 ) * tmean_2;
		// Density using ideal gas law: rho=(presure*mol. weight)/(gas const*Tmean)
		fdens( 1 ) = pres * xwght( iprop( 1 ) ) / ( UniversalGasConst * tmean );
		// Mollecular weights in kg/kmol
		if ( ( standard == EN673 ) || ( standard == EN673Design ) ) {
			//fdens( 1 ) = xgrho( iprop( 1 ), 1 ) + xgrho( iprop( 1 ), 2 ) * tmean + xgrho( iprop( 1 ), 3 ) * pow_2( tmean ); //Autodesk:Uninit xgrho was uninitialized
//			fdens( i ) = ENpressure * xwght( iprop( i ) ) / ( gaslaw * tmean );
			fdens( 1 ) = ENpressure * xwght( iprop( 1 ) ) / ( gaslaw * tmean ); //Autodesk:Init Guess patch for line above (i is uninitialized)
		}

		if ( frct( 1 ) == 1.0 ) { // Single gas properties
			visc = fvis( 1 ); // viscosity in kg/(m*s)
			con = fcon( 1 ); // conductivity in W/(m*K)
			cp = fcp( 1 ); // SpecIFic heat in J/(kg*K)
			dens = fdens( 1 ); // density in kg/m^3
		} else { // Mixture properties
			molmix = frct( 1 ) * xwght( iprop( 1 ) ); // initialize equation 56
			cpmixm = molmix * fcp( 1 ); // initialize equation 58
			kprime( 1 ) = 3.75 * UniversalGasConst / xwght( iprop( 1 ) ) * fvis( 1 ); // equation 67
			kdblprm( 1 ) = fcon( 1 ) - kprime( 1 ); // equation 67
			// initialize sumations for eqns 60-66:
			mumix = 0.0;
			kpmix = 0.0;
			kdpmix = 0.0;
			mukpdwn( 1 ) = 1.0;
			kpdown( 1 ) = 1.0;
			kdpdown( 1 ) = 1.0;
			for ( i = 2; i <= nmix; ++i ) {
				if ( frct( i ) == 0.0 ) {
					nperr = 2011; // error 2011: component fraction in a mixture is 0%
					ErrorMessage = "Component fraction in mixture is 0%";
					return;
				}
				// calculate properties of mixture constituents:
				fcon( i ) = xgcon( iprop( i ), 1 ) + xgcon( iprop( i ), 2 ) * tmean + xgcon( iprop( i ), 3 ) * tmean_2;
				fvis( i ) = xgvis( iprop( i ), 1 ) + xgvis( iprop( i ), 2 ) * tmean + xgvis( iprop( i ), 3 ) * tmean_2;
				fcp( i ) = xgcp( iprop( i ), 1 ) + xgcp( iprop( i ), 2 ) * tmean + xgcp( iprop( i ), 3 ) * tmean_2;
				fdens( i ) = pres * xwght( iprop( i ) ) / ( UniversalGasConst * tmean );
				if ( ( standard == EN673 ) || ( standard == EN673Design ) ) {
					//fdens( i ) = grho( iprop( i ), 1 ) + grho( iprop( i ), 2 ) * tmean + grho( iprop( i ), 3 ) * pow_2( tmean );
					fdens( 1 ) = ENpressure * xwght( iprop( 1 ) ) / ( gaslaw * tmean ); // Density using ideal gas law: rho=(presure*mol. weight)/(gas const*Tmean)
				}
				molmix += frct( i ) * xwght( iprop( i ) ); // equation 56
				cpmixm += frct( i ) * fcp( i ) * xwght( iprop( i ) ); // equation 58-59
				kprime( i ) = 3.75 * UniversalGasConst / xwght( iprop( i ) ) * fvis( i ); // equation 67
				kdblprm( i ) = fcon( i ) - kprime( i ); // equation 68
				mukpdwn( i ) = 1.0; // initialize denominator of equation 60
				kpdown( i ) = 1.0; // initialize denominator of equation 63
				kdpdown( i ) = 1.0; // initialize denominator of equation 65
			}

			if ( standard == ISO15099 ) {
				for ( i = 1; i <= nmix; ++i ) {
					Real64 const kprime_i( kprime( i ) );
					Real64 const xwght_i( xwght( iprop( i ) ) );
					for ( j = 1; j <= nmix; ++j ) {
						Real64 const xwght_j( xwght( iprop( j ) ) );

						// numerator of equation 61
						Real64 const x_pow( root_4( xwght_j / xwght_i ) );
						phimup = pow_2( 1.0 + std::sqrt( fvis( i ) / fvis( j ) ) * x_pow );

						// denominator of equation 61, 64 and 66
						downer = 2.0 * sqrt_2 * std::sqrt( 1.0 + ( xwght_i / xwght_j ) );

						// calculate the denominator of equation 60
						if ( i != j ) mukpdwn( i ) += phimup / downer * frct( j ) / frct( i );

						// numerator of equation 64, psiterm is the multiplied term in brackets
						psiup = pow_2( 1.0 + std::sqrt( kprime_i / kprime( j ) ) / x_pow );

						psiterm = 1.0 + 2.41 * ( xwght_i - xwght_j ) * ( xwght_i - 0.142 * xwght_j ) / pow_2( xwght_i + xwght_j );

						// using the common denominator downer calculate the denominator for equation 63
						if ( i != j ) kpdown( i ) += psiup * psiterm / downer * frct( j ) / frct( i );

						// calculate the numerator of equation 66
						phikup = psiup; //Tuned Was pow_2( 1.0 + std::sqrt( kprime_i / kprime( j ) ) * std::pow( xwght_i / xwght_j, 0.25 ) );

						// using the common denominator downer calculate the denominator for equation 65
						if ( i != j ) kdpdown( i ) += phikup / downer * frct( j ) / frct( i );
					}
					mumix += fvis( i ) / mukpdwn( i ); // equation 60
					kpmix += kprime( i ) / kpdown( i ); // equation 63
					kdpmix += kdblprm( i ) / kdpdown( i ); // equation 65
				}

				// calculate the density of the mixture assuming an ideal gas:
				rhomix = pres * molmix / ( UniversalGasConst * tmean ); // equation 57
				kmix = kpmix + kdpmix; // equation 68-a

				// final mixture properties:
				visc = mumix;
				con = kmix;
				dens = rhomix;
				cp = cpmixm / molmix;
			} else if ( ( standard == EN673 ) || ( standard == EN673Design ) ) {
				for ( i = 1; i <= nmix; ++i ) {
					con += fcon( i ) * frct( i );
					visc += fvis( i ) * frct( i );
					dens += fdens( i ) * frct( i );
					cp += fcp( i ) * frct( i );
				}
			} else {
				assert( false ); // should never come here - unsupported standard
			}

		}

		pr = cp * visc / con; // calculate the Prandtl number

	}

	void
	GassesLow(
		Real64 const tmean,
		Real64 const mwght,
		Real64 const pressure,
		Real64 const gama,
		Real64 & cond,
		int & nperr,
		std::string & ErrorMessage
	)
	{

		// Locals
		static Real64 alpha( 0.0 );
		static Real64 B( 0.0 );

		alpha = alpha1 * alpha2 / ( alpha2 + alpha1 * ( 1 - alpha2 ) );

		if ( ( gama ) == 1 ) {
			nperr = 40; //supplied gamma coefficient is incorrect
			ErrorMessage = "Supplied gamma coefficient is incorrect.";
			return;
		}

		B = alpha * ( gama + 1 ) / ( gama - 1 ) * std::sqrt( UniversalGasConst / ( 8 * Pi * mwght * tmean ) );

		cond = B * pressure;

	}

	//  subroutine doe2gas90 (standard, iprop, frct, pres, nmix, con0, dcon, visc0, dvisc, dens0, ddens, pr0, dpr)
	//    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
	//    !  calculate gas properties using old doe2 format                              !
	//    !------------------------------------------------------------------------------!
	//    !  iprop(i)  vector of gas identifiers (for max of w5cog.fi::maxgas gasses)
	//    !  frct(i)   vector of fraction of gasses in a mixture (for max of w5cog.fi::maxgas gasses)
	//    !  pres(i)   pressure (default: pres = 1e5)[N/m^2]
	//    !  nmix(i)   number of gasses in a mixture
	//    !  con0(o)   thermal conductivity @ mean temperature of 0 C[W/m-K]
	//    !  dcon(o)   derivative of thermal conductivity wrt temperature x 10^5 [W/m-K^2 x 10^5]
	//    !  visc0(o)  dynamic viscosity @ mean temperature of 0 C x 10^5 [kg/m-s x 10^5]
	//    !  dvisc(o)  derivative of dynamic viscosity wrt temperature x 10^8 [kg/m-s-K x 10^8]
	//    !  dens0(o)  density @ mean temperature of 0 C [kg/m^3]
	//    !  ddens(o)  derivative of density wrt temperature [kg/m^3-K]
	//    !  pr0(o)    Prandl number @ mean temperature of 0 C [ - ]
	//    !  dpr(o)    derivative of Prandl number wrt temperature [ 1/K ]
	//    !  nperr(o)  error flag (if component fraction in a mixture is 0%)
	//    !
	//    !**import:
	//    !  w5cog.fi::maxgas
	//    !
	//    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

	//    ! Variables

	//    integer, intent(in) :: nmix, iprop(maxgas)
	//    REAL(r64), intent(in) :: pres, frct(maxgas)

	//    REAL(r64), intent(out) :: con0, dcon, visc0, dvisc, dens0, ddens, pr0, dpr

	//    REAL(r64) :: con, visc, dens, cp, pr
	//    integer :: standard, nperr
	//    character(len=2000) :: ErrMsg

	//    call GASSES90(273.15d0, iprop, frct, pres,nmix, wght, gcon, gvis, gcp, con, visc, dens, cp, pr, standard, nperr, ErrMsg)

	//    con0=con
	//    visc0=visc*10**5
	//    dens0=dens
	//    pr0=pr

	//    call GASSES90(283.15d0,iprop, frct, pres, nmix, wght, gcon, gvis, gcp, con, visc, dens, cp, pr, standard, nperr, ErrMsg)

	//    dcon=(con-con0)/10*10**5
	//    dvisc=(visc*10**5-visc0)/10*10**3
	//    ddens=(dens-dens0)/10
	//    dpr=(pr-pr0)/10

	//  end subroutine doe2gas90

	//     NOTICE

	//     Copyright � 1996-2014 The Board of Trustees of the University of Illinois
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

} // TARCOGGasses90

} // EnergyPlus
