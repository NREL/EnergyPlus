// C++ Headers
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <TarcogShading.hh>
#include <DataGlobals.hh>
#include <TARCOGCommon.hh>
#include <TARCOGGasses90.hh>
#include <TARCOGGassesParams.hh>
#include <TARCOGParams.hh>

namespace EnergyPlus {

namespace TarcogShading {

	// MODULE INFORMATION:
	//       AUTHOR         Simon Vidanovic
	//       DATE WRITTEN   June/22/2010
	//       MODIFIED       na
	//       RE-ENGINEERED  na
	//  Revision: 6.0.36  (June/22/2010)
	//   - Initial setup, extracted from TARCOG.for

	// PURPOSE OF THIS MODULE:
	// Module which contains subroutines used for handling shading
	// device layers according to ISO15099

	// METHODOLOGY EMPLOYED:
	// <description>

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// USE STATEMENTS:

	// Using/Aliasing
	using namespace TARCOGGassesParams;
	using namespace TARCOGGasses90;
	using namespace TARCOGParams;

	// Functions

	void
	shading(
		FArray1A< Real64 > const theta,
		FArray1A< Real64 > const gap,
		FArray1A< Real64 > hgas,
		FArray1A< Real64 > hcgas,
		FArray1A< Real64 > hrgas,
		FArray2A< Real64 > const frct,
		FArray2A_int const iprop,
		FArray1A< Real64 > const pressure,
		FArray1A_int const nmix,
		FArray1A< Real64 > const xwght,
		FArray2A< Real64 > const xgcon,
		FArray2A< Real64 > const xgvis,
		FArray2A< Real64 > const xgcp,
		int const nlayer,
		Real64 const width,
		Real64 const height,
		Real64 const angle,
		Real64 const Tout,
		Real64 const Tin,
		FArray1A< Real64 > const Atop,
		FArray1A< Real64 > const Abot,
		FArray1A< Real64 > const Al,
		FArray1A< Real64 > const Ar,
		FArray1A< Real64 > const Ah,
		FArray1A< Real64 > const vvent,
		FArray1A< Real64 > const tvent,
		FArray1A_int const LayerType,
		FArray1A< Real64 > Tgaps,
		FArray1A< Real64 > qv,
		int & nperr,
		std::string & ErrorMessage,
		FArray1A< Real64 > vfreevent
	)
	{
		//**************************************************************************************************************
		//  Input:
		// theta   Vector of average temperatures
		//  gap      Vector of gap widths (maxlay) [m]
		//  hgas    Convective part of gap effective conductivity
		//  frct    Fraction of gasses in a mixture (maxlay1,maxgas)
		//  iprop    Vector of gas identifers (maxlay1,maxgas)
		//  pressure  Vector of gas pressures [N/m^2]
		//  nmix    Vector of number of gasses for each mixture (maxgas=10)
		//  nlayer  Number of glazing layers
		//  width    IGU cavity width [m]
		//  height  IGU cavity height [m]
		//  angle    Window angle [degrees]
		//  Tout    Outdoor temperature [K]
		//  Tin      Indoor temperature [K]
		//  Atop    Opening between top of shading device and top of glazing cavity [m^2]
		//  Abot    Opening between bottom of shading device and bottom of glazing cavity [m^2]
		//  Al      Opening between left of shading device and left end of glazing cavity [m^2]
		//  Ar      Opening between right of shading device and right end of glazing cavity [m^2]
		//  Ah      Total area holes in the shading device [m^2]
		//  LayerType    Vector of layer types (0 - glazing; 1 - shading)
		//  Ebf      Vector of emissive power of the front surface (# of layers)
		//  Input/Output:
		//  Tgaps    Vector of gap temperatures [K]
		//  Output:
		//  qv      Vector of heat transfer to the gap by vetilation [W/m^2]
		//  hhatv    Vector of all film coefficients for vented cavities (maxlay3)
		//  hcv      Vector of surface-to-air heat transfer coefficients by condction/convection for vented cavities [W/(m^2*K)]
		//  Ebgap    Vector of emissive power of the vented cavities (maxlay3)
		//  nperr    Error flag
		// vfreevent   Vector of free ventilation velocities in gaps
		//**************************************************************************************************************

		// Using/Aliasing
		using namespace TARCOGCommon;

		// Argument array dimensioning
		theta.dim( maxlay2 );
		gap.dim( MaxGap );
		hgas.dim( maxlay1 );
		hcgas.dim( maxlay1 );
		hrgas.dim( maxlay1 );
		frct.dim( maxlay1, maxgas );
		iprop.dim( maxlay1, maxgas );
		pressure.dim( maxlay1 );
		nmix.dim( maxlay1 );
		xwght.dim( maxgas );
		xgcon.dim( maxgas, 3 );
		xgvis.dim( maxgas, 3 );
		xgcp.dim( maxgas, 3 );
		Atop.dim( maxlay );
		Abot.dim( maxlay );
		Al.dim( maxlay );
		Ar.dim( maxlay );
		Ah.dim( maxlay );
		vvent.dim( maxlay1 );
		tvent.dim( maxlay1 );
		LayerType.dim( maxlay );
		Tgaps.dim( maxlay1 );
		qv.dim( maxlay1 );
		vfreevent.dim( maxlay1 );

		// Locals
		//REAL(r64), intent(in) :: Ebf(maxlay)

		Real64 Atops;
		Real64 Abots;
		Real64 Als;
		Real64 Ars;
		Real64 Ahs;
		Real64 press1;
		Real64 press2;
		Real64 s1;
		Real64 s2;
		Real64 s;
		Real64 hcvs;
		Real64 qvs;
		Real64 hc;
		Real64 hc1;
		Real64 hc2;
		FArray1D< Real64 > frct1( maxgas );
		FArray1D< Real64 > frct2( maxgas );
		Real64 speed;
		Real64 Tav;
		Real64 Tgap;
		Real64 Temp;
		Real64 speed1;
		Real64 speed2;
		Real64 Tav1;
		Real64 Tav2;
		Real64 Tgap1;
		Real64 Tgap2;
		Real64 hcv1;
		Real64 hcv2;
		Real64 qv1;
		Real64 qv2;

		int i;
		int j;
		int k;
		int nmix1;
		int nmix2;
		FArray1D_int iprop1( maxgas );
		FArray1D_int iprop2( maxgas );

		// init vectors:
		qv = 0.0;
		//hhatv = 0.0d0
		//Ebgap = 0.0d0
		//hcv = 0.0d0

		//main loop:
		for ( i = 1; i <= nlayer; ++i ) {
			k = 2 * i + 1;
			//if (LayerType(i).eq.VENETBLIND) then
			if ( IsShadingLayer( LayerType( i ) ) ) {
				//dr.........set Shading device geometry
				Atops = Atop( i );
				Abots = Abot( i );
				Als = Al( i );
				Ars = Ar( i );
				Ahs = Ah( i );

				//dr.....setting gas properies for two adjacent gaps (or enviroment)
				nmix1 = nmix( i );
				nmix2 = nmix( i + 1 );
				press1 = pressure( i );
				press2 = pressure( i + 1 );
				for ( j = 1; j <= maxgas; ++j ) {
					iprop1( j ) = iprop( i, j );
					iprop2( j ) = iprop( i + 1, j );
					frct1( j ) = frct( i, j );
					frct2( j ) = frct( i + 1, j );
				} // j

				//dr.......shading on outdoor side
				if ( i == 1 ) {
					s = gap( 1 );
					hc = hcgas( 2 );
					//Tenv = tvent(1)
					Tav = ( theta( 2 ) + theta( 3 ) ) / 2.0;
					Tgap = Tgaps( 2 );

					//bi......use Tout as temp of the air at inlet
					shadingedge( iprop1, frct1, press1, nmix1, iprop2, frct2, press2, nmix2, xwght, xgcon, xgvis, xgcp, Atops, Abots, Als, Ars, Ahs, s, height, width, angle, vvent( 2 ), hc, Tout, Tav, Tgap, hcvs, qvs, nperr, ErrorMessage, speed );

					// exit on error
					if ( ( nperr > 0 ) && ( nperr < 1000 ) ) return;

					Tgaps( 2 ) = Tgap;
					//Ebgap(3) = sigma * Tgap ** 4

					hcgas( 2 ) = hcvs / 2.0;
					hgas( 2 ) = hcgas( 2 ) + hrgas( 2 );
					qv( 2 ) = qvs;

					//bi.........Add free ventilation velocity
					vfreevent( 2 ) = speed;
				} //if (i.eq.1) then

				//dr.......shading on indoor side
				if ( i == nlayer ) {
					if ( nlayer > 1 ) {
						s = gap( nlayer - 1 ); //Autodesk:BoundsViolation gap(nlayer - 1) @ nlayer=1: Fixed with if block
						Tav = ( theta( 2 * nlayer - 1 ) + theta( 2 * nlayer - 2 ) ) / 2.0; //Autodesk:BoundsViolation theta(2 * nlayer - 2) @ nlayer=1: Fixed with if block in 8.2
					} else {
						s = 0.0;
						Tav = 273.15;
					}
					hc = hcgas( nlayer );
					//Tenv = tvent(nlayer + 1)

					Tgap = Tgaps( nlayer );

					//bi.........use Tin as temp of the air at inlet
					shadingedge( iprop2, frct2, press2, nmix2, iprop1, frct1, press1, nmix1, xwght, xgcon, xgvis, xgcp, Atops, Abots, Als, Ars, Ahs, s, height, width, angle, vvent( nlayer ), hc, Tin, Tav, Tgap, hcvs, qvs, nperr, ErrorMessage, speed );

					// exit on error
					if ( ( nperr > 0 ) && ( nperr < 1000 ) ) return;

					Tgaps( nlayer ) = Tgap;
					hcgas( nlayer ) = hcvs / 2.0;
					hgas( nlayer ) = hcgas( nlayer ) + hrgas( nlayer );
					qv( nlayer ) = qvs;

					//bi.........Add free ventilation velocity
					vfreevent( i ) = speed;
				} //if (i.eq.nlayer) then

				//dr.......shading between glass layers
				if ( ( i > 1 ) && ( i < nlayer ) ) {
					//dr.........average temperatures
					Tav1 = ( theta( 2 * i - 2 ) + theta( 2 * i - 1 ) ) / 2.0;
					Tav2 = ( theta( 2 * i ) + theta( 2 * i + 1 ) ) / 2.0;
					Tgap1 = Tgaps( i );
					Tgap2 = Tgaps( i + 1 );

					hc1 = hcgas( i );
					hc2 = hcgas( i + 1 );
					if ( i > 1 ) s1 = gap( i - 1 );
					s2 = gap( i );

					//speed1 = vvent(i)
					//speed2 = vvent(i+1)

					if ( ( CalcForcedVentilation != 0 ) && ( ( vvent( i ) != 0 ) || ( vvent( i + 1 ) != 0 ) ) ) {
						forcedventilation( iprop1, frct1, press1, nmix1, xwght, xgcon, xgvis, xgcp, s1, height, hc1, vvent( i ), tvent( i ), Temp, Tav1, hcv1, qv1, nperr, ErrorMessage );
						forcedventilation( iprop2, frct2, press2, nmix1, xwght, xgcon, xgvis, xgcp, s2, height, hc1, vvent( i + 1 ), tvent( i + 1 ), Temp, Tav2, hcv2, qv2, nperr, ErrorMessage );
					} else {
						shadingin( iprop1, frct1, press1, nmix1, iprop2, frct2, press2, nmix2, xwght, xgcon, xgvis, xgcp, Atops, Abots, Als, Ars, Ahs, s1, s2, height, width, angle, hc1, hc2, speed1, speed2, Tgap1, Tgap2, Tav1, Tav2, hcv1, hcv2, qv1, qv2, nperr, ErrorMessage );
					}

					// exit on error
					if ( ( nperr > 0 ) && ( nperr < 1000 ) ) return;

					//if (vvent(i).gt.0) then !not implemented for inside shadin yet
					//  nperr = 1006
					//  ErrorMessage = 'Forced ventilation not implemented for internal SD layers.'
					//  return
					//end if

					hcgas( i ) = hcv1 / 2.0;
					hcgas( i + 1 ) = hcv2 / 2.0;
					hgas( i ) = hcgas( i ) + hrgas( i );
					hgas( i + 1 ) = hcgas( i + 1 ) + hrgas( i + 1 );
					qv( i ) = qv1;
					qv( i + 1 ) = qv2;
					Tgaps( i ) = Tgap1;
					Tgaps( i + 1 ) = Tgap2;
					//bi.........Add free ventilation velocity
					vfreevent( i ) = speed1;
					vfreevent( i + 1 ) = speed2;
				} //if ((i.gt.1).and.(i.lt.nlayer)) then
			} //if (LayerType(i).eq.SHADING) then
		}

	}

	void
	forcedventilation(
		FArray1A_int const iprop,
		FArray1A< Real64 > const frct,
		Real64 const press,
		int const nmix,
		FArray1A< Real64 > const xwght,
		FArray2A< Real64 > const xgcon,
		FArray2A< Real64 > const xgvis,
		FArray2A< Real64 > const xgcp,
		Real64 const s,
		Real64 const H,
		Real64 const hc,
		Real64 const forcedspeed,
		Real64 const Tinlet,
		Real64 & Toutlet,
		Real64 const Tav,
		Real64 & hcv,
		Real64 & qv,
		int & nperr,
		std::string & ErrorMessage
	)
	{
		//**************************************************************************************************************
		//  Input:
		//  iprop      Vector of gas identifiers
		//  frct      Fraction of gasses in a mixture
		//  nmix      Number of gasses in a mixture
		//  press      Pressure in mixture
		//  s1        Gap width [m]
		//  H          IGU cavity height [m]
		//  L          IGU cavity width [m]
		//  hc        Convective/conductive coefficient for non-vented gap
		//  Tav        Average temperature of gap surfaces
		// Tinlet    Temperature of inlet air
		//  Output:
		//  hcv    Convective/conductive coefficient for vented gap
		//  qv    Heat transfer to the gap by vetilation [W/m^2]
		//  nperr      Error flag
		// ErrorMessage string containing error message
		//**************************************************************************************************************

		// Argument array dimensioning
		iprop.dim( maxgas );
		frct.dim( maxgas );
		xwght.dim( maxgas );
		xgcon.dim( maxgas, 3 );
		xgvis.dim( maxgas, 3 );
		xgcp.dim( maxgas, 3 );

		// Locals
		Real64 H0;
		Real64 dens;
		Real64 cp;
		Real64 pr;
		Real64 con;
		Real64 visc;

		GASSES90( Tav, iprop, frct, press, nmix, xwght, xgcon, xgvis, xgcp, con, visc, dens, cp, pr, 1, nperr, ErrorMessage );

		H0 = ( dens * cp * s * forcedspeed ) / ( 4.0 * hc + 8.0 * forcedspeed );

		Toutlet = Tav - ( Tav - Tinlet ) * std::pow( e, -H / H0 );

		qv = -dens * cp * forcedspeed * s * ( Toutlet - Tinlet ) / H;

		//Need to calculate surface-to-air convection heat transfer coefficient.  This is needed later to calculate layer
		//to gap thermal resistance
		hcv = 2.0 * hc + 4.0 * forcedspeed;

	}

	void
	shadingin(
		FArray1A_int const iprop1,
		FArray1A< Real64 > const frct1,
		Real64 const press1,
		int const nmix1,
		FArray1A_int const iprop2,
		FArray1A< Real64 > const frct2,
		Real64 const press2,
		int const nmix2,
		FArray1A< Real64 > const xwght,
		FArray2A< Real64 > const xgcon,
		FArray2A< Real64 > const xgvis,
		FArray2A< Real64 > const xgcp,
		Real64 & Atop,
		Real64 & Abot,
		Real64 const Al,
		Real64 const Ar,
		Real64 const Ah,
		Real64 const s1,
		Real64 const s2,
		Real64 const H,
		Real64 const L,
		Real64 const angle,
		Real64 const hc1,
		Real64 const hc2,
		Real64 & speed1,
		Real64 & speed2,
		Real64 & Tgap1,
		Real64 & Tgap2,
		Real64 const Tav1,
		Real64 const Tav2,
		Real64 & hcv1,
		Real64 & hcv2,
		Real64 & qv1,
		Real64 & qv2,
		int & nperr,
		std::string & ErrorMessage
	)
	{
		//**************************************************************************************************************
		//  Input:
		//  iprop1      Vector of gas identifiers
		//  frct1      Fraction of gasses in a mixture
		//  nmix1      Number of gasses in a mixture
		//  press1      Pressure in mixture
		//  iprop2      Vector of gas identifiers
		//  frct2      Fraction of gasses in a mixture
		//  nmix2      Number of gasses in a mixture
		//  press2      Pressure in mixture
		//  Atop      Opening between top of shading device and top of glazing cavity [m^2]
		//  Abot      Opening between bottom of shading device and bottom of glazing cavity [m^2]
		//  Al        Opening between left of shading device and left end of glazing cavity [m^2]
		//  Ar        Opening between right of shading device and right end of glazing cavity [m^2]
		//  Ah        Total area holes in the shading device [m^2]
		//  s1, s2      Gap width [m]
		//  H        IGU cavity height [m]
		//  L        IGU cavity width [m]
		//  angle      Window angle [degrees]
		//  hc1, hc2    Convective/conductive coefficient for non-vented gap
		//  Tav1, Tav2    Average temperature of gap surfaces
		//  Output:
		//  Tgap1, Tgap2  Temperature of vented gap
		//  hcv1, hcv2    Convective/conductive coefficient for vented gap
		//  qv1, qv2    Heat transfer to the gap by vetilation [W/m^2]
		//  speed1, speed2  Air/gas velocities in gaps around SD layer
		//  nperr      Error flag
		//**************************************************************************************************************

		// Using/Aliasing
		using DataGlobals::GravityConstant;
		using DataGlobals::KelvinConv;
		using DataGlobals::Pi;

		// Argument array dimensioning
		iprop1.dim( maxgas );
		frct1.dim( maxgas );
		iprop2.dim( maxgas );
		frct2.dim( maxgas );
		xwght.dim( maxgas );
		xgcon.dim( maxgas, 3 );
		xgvis.dim( maxgas, 3 );
		xgcp.dim( maxgas, 3 );

		// Locals
		Real64 A;
		Real64 A1;
		Real64 A2;
		Real64 B1;
		Real64 B2;
		Real64 C1;
		Real64 C2;
		Real64 D1;
		Real64 D2;
		Real64 Zin1;
		Real64 Zin2;
		Real64 Zout1;
		Real64 Zout2;
		Real64 A1eqin;
		Real64 A1eqout;
		Real64 A2eqin;
		Real64 A2eqout;
		Real64 T0;
		Real64 tilt;
		Real64 dens0;
		Real64 visc0;
		Real64 con0;
		Real64 pr0;
		Real64 cp0;
		Real64 dens1;
		Real64 visc1;
		Real64 con1;
		Real64 pr1;
		Real64 cp1;
		Real64 dens2;
		Real64 visc2;
		Real64 con2;
		Real64 pr2;
		Real64 cp2;
		Real64 Tup;
		Real64 Tdown;
		Real64 H01;
		Real64 H02;
		Real64 beta1;
		Real64 beta2;
		Real64 alpha1;
		Real64 alpha2;
		Real64 P1;
		Real64 P2;
		Real64 qsmooth;

		// iteration parameters
		int iter;
		Real64 TGapOld1;
		Real64 TGapOld2;
		Real64 Temp1;
		Real64 Temp2;
		bool converged;

		TGapOld1 = 0.0;
		TGapOld2 = 0.0;
		tilt = Pi / 180 * ( angle - 90 );
		T0 = 0.0 + KelvinConv;
		A1eqin = 0.0;
		A2eqout = 0.0;
		A1eqout = 0.0;
		A2eqin = 0.0;
		P1 = 0.0;
		P2 = 0.0;

		GASSES90( T0, iprop1, frct1, press1, nmix1, xwght, xgcon, xgvis, xgcp, con0, visc0, dens0, cp0, pr0, 1, nperr, ErrorMessage );

		// exit on error:
		if ( ( nperr > 0 ) && ( nperr < 1000 ) ) return;

		//dr...check for error messages
		if ( ( Tgap1 * Tgap2 ) == 0 ) {
			nperr = 15;
			ErrorMessage = "Temperature of vented gap must be greater than 0 [K].";
			return;
		}

		if ( ( Atop + Abot ) == 0 ) {
			//    nperr = 16
			//    return
			Atop = 0.000001;
			Abot = 0.000001;
		}

		converged = false;
		iter = 0;
		Real64 const s1_2( pow_2( s1 ) );
		Real64 const s2_2( pow_2( s2 ) );
		Real64 const s1_s2_2( pow_2( s1 / s2 ) );
		while ( ! converged ) {
			++iter;
			GASSES90( Tgap1, iprop1, frct1, press1, nmix1, xwght, xgcon, xgvis, xgcp, con1, visc1, dens1, cp1, pr1, 1, nperr, ErrorMessage );
			GASSES90( Tgap2, iprop2, frct2, press2, nmix2, xwght, xgcon, xgvis, xgcp, con2, visc2, dens2, cp2, pr2, 1, nperr, ErrorMessage );

			//  A = dens0 * T0 * GravityConstant * ABS(cos(tilt)) * ABS(Tgap1 - Tgap2) / (Tgap1 * Tgap2)

			//bi...Bug fix #00005:
			A = dens0 * T0 * GravityConstant * H * std::abs( std::cos( tilt ) ) * std::abs( Tgap1 - Tgap2 ) / ( Tgap1 * Tgap2 );

			if ( A == 0.0 ) {
				qv1 = 0.0;
				qv2 = 0.0;
				speed1 = 0.0;
				speed2 = 0.0;
				hcv1 = 2.0 * hc1;
				hcv2 = 2.0 * hc2;
				return;
			}

			B1 = dens1 / 2;
			B2 = ( dens2 / 2 ) * s1_s2_2;

			C1 = 12 * visc1 * H / s1_2;
			C2 = 12 * visc2 * ( H / s2_2 ) * ( s1 / s2 );

			if ( Tgap1 >= Tgap2 ) {
				A1eqin = Abot + 0.5 * Atop * ( Al + Ar + Ah ) / ( Abot + Atop );
				A2eqout = Abot + 0.5 * Atop * ( Al + Ar + Ah ) / ( Abot + Atop );
				A1eqout = Atop + 0.5 * Abot * ( Al + Ar + Ah ) / ( Abot + Atop );
				A2eqin = Atop + 0.5 * Abot * ( Al + Ar + Ah ) / ( Abot + Atop );
			} else if ( Tgap1 < Tgap2 ) {
				A1eqout = Abot + 0.5 * Atop * ( Al + Ar + Ah ) / ( Abot + Atop );
				A2eqin = Abot + 0.5 * Atop * ( Al + Ar + Ah ) / ( Abot + Atop );
				A1eqin = Atop + 0.5 * Abot * ( Al + Ar + Ah ) / ( Abot + Atop );
				A2eqout = Atop + 0.5 * Abot * ( Al + Ar + Ah ) / ( Abot + Atop );
			}

			Zin1 = pow_2( ( s1 * L / ( 0.6 * A1eqin ) ) - 1.0 );
			Zin2 = pow_2( ( s2 * L / ( 0.6 * A2eqin ) ) - 1.0 );
			Zout1 = pow_2( ( s1 * L / ( 0.6 * A1eqout ) ) - 1.0 );
			Zout2 = pow_2( ( s2 * L / ( 0.6 * A2eqout ) ) - 1.0 );

			D1 = ( dens1 / 2.0 ) * ( Zin1 + Zout1 );
			D2 = ( dens2 / 2.0 ) * s1_s2_2 * ( Zin2 + Zout2 );

			A1 = B1 + D1 + B2 + D2;
			A2 = C1 + C2;

			speed1 = ( std::sqrt( pow_2( A2 ) + std::abs( 4.0 * A * A1 ) ) - A2 ) / ( 2.0 * A1 );
			speed2 = speed1 * s1 / s2;

			H01 = ( dens1 * cp1 * s1 * speed1 ) / ( 4.0 * hc1 + 8.0 * speed1 );
			H02 = ( dens2 * cp2 * s2 * speed2 ) / ( 4.0 * hc2 + 8.0 * speed2 );

			if ( ( H01 != 0.0 ) && ( H02 != 0.0 ) ) {
				P1 = -H / H01;
				P2 = -H / H02;
			}

			beta1 = std::pow( e, P1 );
			beta2 = std::pow( e, P2 );

			alpha1 = 1.0 - beta1;
			alpha2 = 1.0 - beta2;

			if ( Tgap1 > Tgap2 ) {
				Tup = ( alpha1 * Tav1 + beta1 * alpha2 * Tav2 ) / ( 1.0 - beta1 * beta2 );
				Tdown = alpha2 * Tav2 + beta2 * Tup;
			} else if ( Tgap2 >= Tgap1 ) {
				Tdown = ( alpha1 * Tav1 + beta1 * alpha2 * Tav2 ) / ( 1.0 - beta1 * beta2 );
				Tup = alpha2 * Tav2 + beta2 * Tdown;
			}

			TGapOld1 = Tgap1;
			TGapOld2 = Tgap2;

			if ( Tgap1 > Tgap2 ) {
				Temp1 = Tav1 - ( H01 / H ) * ( Tup - Tdown );
				Temp2 = Tav2 - ( H02 / H ) * ( Tdown - Tup );
			} else if ( Tgap2 >= Tgap1 ) {
				Temp1 = Tav1 - ( H01 / H ) * ( Tdown - Tup );
				Temp2 = Tav2 - ( H02 / H ) * ( Tup - Tdown );
			}

			Tgap1 = AirflowRelaxationParameter * Temp1 + ( 1.0 - AirflowRelaxationParameter ) * TGapOld1;
			Tgap2 = AirflowRelaxationParameter * Temp2 + ( 1.0 - AirflowRelaxationParameter ) * TGapOld2;

			converged = false;
			if ( ( std::abs( Tgap1 - TGapOld1 ) < AirflowConvergenceTolerance ) || ( iter >= NumOfIterations ) ) {
				if ( std::abs( Tgap2 - TGapOld2 ) < AirflowConvergenceTolerance ) {
					converged = true;
				}
			}

		}

		hcv1 = 2.0 * hc1 + 4.0 * speed1;
		hcv2 = 2.0 * hc2 + 4.0 * speed2;

		if ( Tgap2 >= Tgap1 ) {
			qv1 = -dens1 * cp1 * speed1 * s1 * L * ( Tdown - Tup ) / ( H * L );
			qv2 = -dens2 * cp2 * speed2 * s2 * L * ( Tup - Tdown ) / ( H * L );
		} else if ( Tgap2 < Tgap1 ) {
			qv1 = dens1 * cp1 * speed1 * s1 * L * ( Tdown - Tup ) / ( H * L );
			qv2 = dens2 * cp2 * speed2 * s2 * L * ( Tup - Tdown ) / ( H * L );
		}

		//  write(*, *) Tup-Tdown
		//  write(*, 998) Tup - Tdown, qv1, qv2

		//998  format(f15.9, f15.9, f15.9)

		//bi..  testing - velocities output file
		//bi      open(unit = 33, file = 'velocities.out', status='unknown', form='formatted', iostat = er)

		//bi  write(33, 987) speed1

		qsmooth = ( std::abs( qv1 ) + std::abs( qv2 ) ) / 2.0;

		if ( qv1 > 0.0 ) {
			qv1 = qsmooth;
			qv2 = -qsmooth;
		} else {
			qv1 = -qsmooth;
			qv2 = qsmooth;
		}

	}

	void
	shadingedge(
		FArray1A_int const iprop1,
		FArray1A< Real64 > const frct1,
		Real64 const press1,
		int const nmix1,
		FArray1A_int const iprop2,
		FArray1A< Real64 > const frct2,
		Real64 const press2,
		int const nmix2,
		FArray1A< Real64 > const xwght,
		FArray2A< Real64 > const xgcon,
		FArray2A< Real64 > const xgvis,
		FArray2A< Real64 > const xgcp,
		Real64 & Atop,
		Real64 & Abot,
		Real64 const Al,
		Real64 const Ar,
		Real64 & Ah,
		Real64 const s,
		Real64 const H,
		Real64 const L,
		Real64 const angle,
		Real64 const forcedspeed,
		Real64 const hc,
		Real64 const Tenv,
		Real64 const Tav,
		Real64 & Tgap,
		Real64 & hcv,
		Real64 & qv,
		int & nperr,
		std::string & ErrorMessage,
		Real64 & speed
	)
	{
		//**************************************************************************************************************
		//  Input:
		//  iprop1      Vector of gas identifiers
		//  frct1      Fraction of gasses in a mixture
		//  nmix1      Number of gasses in a mixture
		//  press1      Pressure in mixture
		//  iprop2      Vector of gas identifiers
		//  frct2      Fraction of gasses in a mixture
		//  nmix2      Number of gasses in a mixture
		//  press2      Pressure in mixture
		//  Atop      Opening between top of shading device and top of glazing cavity [m^2]
		//  Abot      Opening between bottom of shading device and bottom of glazing cavity [m^2]
		//  Al        Opening between left of shading device and left end of glazing cavity [m^2]
		//  Ar        Opening between right of shading device and right end of glazing cavity [m^2]
		//  Ah        Total area holes in the shading device [m^2]
		//  s        Gap width [m]
		//  H        IGU cavity height [m]
		//  L        IGU cavity width [m]
		//  angle      Window angle [degrees]
		//  forcedspeed    Speed of forced ventilation [m/s]
		//  hc        Convective/conductive coefficient for non-vented gap
		//  Tenv      Enviromental temperature
		//  Tav        Average temperature of gap surfaces
		//  Output:
		//  Tgap      Temperature of vented gap
		//  hcv        Convective/conductive coefficient for vented gap
		//  qv        Heat transfer to the gap by vetilation [W/m^2]
		//  nperr      Error flag
		//  speed      Air velocity
		//**************************************************************************************************************

		// Using/Aliasing
		using DataGlobals::GravityConstant;
		using DataGlobals::KelvinConv;
		using DataGlobals::Pi;

		// Argument array dimensioning
		iprop1.dim( maxgas );
		frct1.dim( maxgas );
		iprop2.dim( maxgas );
		frct2.dim( maxgas );
		xwght.dim( maxgas );
		xgcon.dim( maxgas, 3 );
		xgvis.dim( maxgas, 3 );
		xgcp.dim( maxgas, 3 );

		// Locals
		Real64 A;
		Real64 A1;
		Real64 A2;
		Real64 B1;
		Real64 C1;
		Real64 D1;
		Real64 Zin1;
		Real64 Zout1;
		Real64 A1eqin;
		Real64 A1eqout;
		Real64 T0;
		Real64 tilt;
		Real64 dens0;
		Real64 visc0;
		Real64 con0;
		Real64 pr0;
		Real64 cp0;
		//REAL(r64) :: dens1, visc1, con1, pr1, cp1
		Real64 dens2;
		Real64 visc2;
		Real64 con2;
		Real64 pr2;
		Real64 cp2;
		Real64 Tgapout;
		Real64 H0;
		Real64 P;
		Real64 beta;

		// iteration parameters
		int iter;
		Real64 TGapOld;
		bool converged;

		tilt = Pi / 180.0 * ( angle - 90.0 );
		T0 = 0.0 + KelvinConv;

		GASSES90( T0, iprop1, frct1, press1, nmix1, xwght, xgcon, xgvis, xgcp, con0, visc0, dens0, cp0, pr0, 1, nperr, ErrorMessage );
		//call gasses90(Tenv, iprop1, frct1, press1, nmix1, xwght, xgcon, xgvis, xgcp, con1, visc1, dens1, cp1, pr1, 1, &
		//                nperr, ErrorMessage)

		// exit on error:
		if ( ( nperr > 0 ) && ( nperr < 1000 ) ) return;

		//dr...check for error messages
		if ( ( Tgap * Tenv ) == 0.0 ) {
			nperr = 15;
			ErrorMessage = "Temperature of vented air must be greater then 0 [K].";
			return;
		}

		if ( ( Atop + Abot ) == 0 ) {
			//    nperr = 16
			//    return
			Atop = 0.000001;
			Abot = 0.000001;
		}
		if ( ( Ah + Al + Ar ) == 0.0 ) {
			Ah = 0.000001;
		}

		converged = false;
		iter = 0;
		Real64 const s_2( pow_2( s ) );
		while ( ! converged ) {
			++iter;
			GASSES90( Tgap, iprop2, frct2, press2, nmix2, xwght, xgcon, xgvis, xgcp, con2, visc2, dens2, cp2, pr2, 1, nperr, ErrorMessage );

			if ( ( nperr > 0 ) && ( nperr < 1000 ) ) return;

			//  A = dens0 * T0 * gravity * ABS(cos(tilt)) * ABS(Tgap - Tenv) / (Tgap * Tenv)

			//bi...Bug fix #00005:
			A = dens0 * T0 * GravityConstant * H * std::abs( std::cos( tilt ) ) * std::abs( Tgap - Tenv ) / ( Tgap * Tenv );
			//  A = dens0 * T0 * GravityConstant * H * ABS(cos(tilt)) * (Tgap - Tenv) / (Tgap * Tenv)

			B1 = dens2 / 2;
			C1 = 12.0 * visc2 * H / s_2;

			if ( Tgap > Tenv ) {
				A1eqin = Abot + 0.5 * Atop * ( Al + Ar + Ah ) / ( Abot + Atop );
				A1eqout = Atop + 0.5 * Abot * ( Al + Ar + Ah ) / ( Abot + Atop );
			} else if ( Tgap <= Tenv ) {
				A1eqout = Abot + 0.5 * Atop * ( Al + Ar + Ah ) / ( Abot + Atop );
				A1eqin = Atop + 0.5 * Abot * ( Al + Ar + Ah ) / ( Abot + Atop );
			}

			Zin1 = pow_2( ( s * L / ( 0.6 * A1eqin ) ) - 1 );
			Zout1 = pow_2( ( s * L / ( 0.6 * A1eqout ) ) - 1 );

			D1 = ( dens2 / 2.0 ) * ( Zin1 + Zout1 );

			A1 = B1 + D1;
			A2 = C1;

			//dr...recalculate speed if forced speed exist
			//bi...skip forced vent for now
			//  if (forcedspeed.ne.0) then
			if ( ( forcedspeed != 0.0 ) && ( CalcForcedVentilation != 0 ) ) {
				speed = forcedspeed;
			} else {
				speed = ( std::sqrt( pow_2( A2 ) + std::abs( 4.0 * A * A1 ) ) - A2 ) / ( 2.0 * A1 );
				//  speed = ABS((SQRT((A2 ** 2) + (4 * A * A1)) - A2) / (2 * A1))
			}

			TGapOld = Tgap;

			// Speed is zero when environment temperature is equal to average layer temperatures
			// For example, this can happen when inside and outside temperatures are equal
			if ( speed != 0.0 ) {
				H0 = ( dens2 * cp2 * s * speed ) / ( 4.0 * hc + 8.0 * speed );

				P = -H / H0;
				beta = std::pow( e, P );
				Tgapout = Tav - ( Tav - Tenv ) * beta;
				Tgap = Tav - ( H0 / H ) * ( Tgapout - Tenv );
			} else {
				Tgapout = Tav;
				Tgap = Tav;
			}

			converged = false;
			if ( ( std::abs( Tgap - TGapOld ) < AirflowConvergenceTolerance ) || ( iter >= NumOfIterations ) ) {
				converged = true;
			}

			//if (iter > NumOfIterations) then
			//  converged = .TRUE.
			//end if

		}

		//bi...Test output:
		//  write(*,101) tenv, tgap, tgapout
		//101  format(f15.9, f15.9, f15.9)

		hcv = 2.0 * hc + 4.0 * speed;

		qv = dens2 * cp2 * speed * s * L * ( Tenv - Tgapout ) / ( H * L );

	}

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

} // TarcogShading

} // EnergyPlus
