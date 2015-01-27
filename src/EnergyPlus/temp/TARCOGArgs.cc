// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <TARCOGArgs.hh>
#include <DataGlobals.hh>
#include <TARCOGCommon.hh>
#include <TARCOGGassesParams.hh>
#include <TARCOGOutput.hh>
#include <TARCOGParams.hh>

namespace EnergyPlus {

namespace TARCOGArgs {

	// MODULE INFORMATION:
	//       AUTHOR         Simon Vidanovic
	//       DATE WRITTEN   June/22/2010
	//       MODIFIED       na
	//       RE-ENGINEERED  na
	//  Revision: 6.0.36  (June/22/2010)
	//   - Initial setup, extracted from TARCOG.for

	// PURPOSE OF THIS MODULE:
	// A module which contains common functions for error checking and
	//    preparation of arguments and intermediate variables

	// METHODOLOGY EMPLOYED:
	// <description>

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// USE STATEMENTS:

	// Using/Aliasing
	using DataGlobals::Pi;
	using DataGlobals::StefanBoltzmann;
	using namespace TARCOGCommon;
	using namespace TARCOGGassesParams;
	using namespace TARCOGOutput;
	using namespace TARCOGParams;

	static gio::Fmt fmtI3( "(I3)" );

	// Functions

	int
	ArgCheck(
		int const nlayer,
		int const iwd,
		Real64 const tout,
		Real64 const tind,
		Real64 const trmin,
		Real64 const wso,
		Real64 const wsi,
		Real64 const dir,
		Real64 const outir,
		int const isky,
		Real64 const tsky,
		Real64 const esky,
		Real64 const fclr,
		Real64 const VacuumPressure,
		Real64 const VacuumMaxGapThickness,
		int const CalcDeflection,
		Real64 const Pa,
		Real64 const Pini,
		Real64 const Tini,
		FArray1A< Real64 > const gap,
		FArray1A< Real64 > const GapDef,
		FArray1A< Real64 > const thick,
		FArray1A< Real64 > const scon,
		FArray1A< Real64 > const YoungsMod,
		FArray1A< Real64 > const PoissonsRat,
		FArray1A< Real64 > const tir,
		FArray1A< Real64 > const emis,
		Real64 const totsol,
		Real64 const tilt,
		FArray1A< Real64 > const asol,
		Real64 const height,
		Real64 const heightt,
		Real64 const width,
		FArray1A< Real64 > const presure,
		FArray2A_int const iprop,
		FArray2A< Real64 > const frct,
		FArray2A< Real64 > const xgcon,
		FArray2A< Real64 > const xgvis,
		FArray2A< Real64 > const xgcp,
		FArray1A< Real64 > const xwght,
		FArray1A< Real64 > const gama,
		FArray1A_int const nmix,
		FArray1A_int const SupportPillar, // Shows whether or not gap have support pillar
		FArray1A< Real64 > const PillarSpacing, // Pillar spacing for each gap (used in case there is support pillar)
		FArray1A< Real64 > const PillarRadius, // Pillar radius for each gap (used in case there is support pillar)
		Real64 & hin,
		Real64 & hout,
		FArray1A_int const ibc,
		FArray1A< Real64 > const Atop,
		FArray1A< Real64 > const Abot,
		FArray1A< Real64 > const Al,
		FArray1A< Real64 > const Ar,
		FArray1A< Real64 > const Ah,
		FArray1A< Real64 > const SlatThick,
		FArray1A< Real64 > const SlatWidth,
		FArray1A< Real64 > const SlatAngle,
		FArray1A< Real64 > const SlatCond,
		FArray1A< Real64 > const SlatSpacing,
		FArray1A< Real64 > const SlatCurve,
		FArray1A< Real64 > const vvent,
		FArray1A< Real64 > const tvent,
		FArray1A_int const LayerType,
		FArray1A_int const nslice,
		FArray1A< Real64 > const LaminateA,
		FArray1A< Real64 > const LaminateB,
		FArray1A< Real64 > const sumsol,
		int const standard,
		int const ThermalMod,
		Real64 const SDScalar,
		std::string & ErrorMessage
	)
	{

		/// INPUTS:

		/// General:

		// Return value
		int ArgCheck;

		// Argument array dimensioning
		gap.dim( maxlay );
		GapDef.dim( MaxGap );
		thick.dim( maxlay );
		scon.dim( maxlay );
		YoungsMod.dim( maxlay );
		PoissonsRat.dim( maxlay );
		tir.dim( maxlay2 );
		emis.dim( maxlay2 );
		asol.dim( maxlay );
		presure.dim( maxlay1 );
		iprop.dim( maxlay1, maxgas );
		frct.dim( maxlay1, maxgas );
		xgcon.dim( maxgas, 3 );
		xgvis.dim( maxgas, 3 );
		xgcp.dim( maxgas, 3 );
		xwght.dim( maxgas );
		gama.dim( maxgas );
		nmix.dim( maxlay1 );
		SupportPillar.dim( maxlay );
		PillarSpacing.dim( maxlay );
		PillarRadius.dim( maxlay );
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

		// Locals
		/// Environment related:

		/// Layers:
		/// Venetians:

		/// Laminates:

		/// Gaps:

		//Deflection

		//Support Pillars
		//   0 - does not have support pillar
		//   1 - have support pillar

		//// INPUTS/OUTPUTS:

		int i;
		std::string a;

		//bi...Write debug output files - if debug flag = 1:

		if ( WriteDebugOutput ) {

			WriteInputArguments( tout, tind, trmin, wso, iwd, wsi, dir, outir, isky, tsky, esky, fclr, VacuumPressure, VacuumMaxGapThickness, ibc, hout, hin, standard, ThermalMod, SDScalar, height, heightt, width, tilt, totsol, nlayer, LayerType, thick, scon, asol, tir, emis, Atop, Abot, Al, Ar, Ah, SlatThick, SlatWidth, SlatAngle, SlatCond, SlatSpacing, SlatCurve, nslice, LaminateA, LaminateB, sumsol, gap, vvent, tvent, presure, nmix, iprop, frct, xgcon, xgvis, xgcp, xwght );

			WriteTARCOGInputFile( VersionNumber, tout, tind, trmin, wso, iwd, wsi, dir, outir, isky, tsky, esky, fclr, VacuumPressure, VacuumMaxGapThickness, CalcDeflection, Pa, Pini, Tini, ibc, hout, hin, standard, ThermalMod, SDScalar, height, heightt, width, tilt, totsol, nlayer, LayerType, thick, scon, YoungsMod, PoissonsRat, asol, tir, emis, Atop, Abot, Al, Ar, Ah, SupportPillar, PillarSpacing, PillarRadius, SlatThick, SlatWidth, SlatAngle, SlatCond, SlatSpacing, SlatCurve, nslice, gap, GapDef, vvent, tvent, presure, nmix, iprop, frct, xgcon, xgvis, xgcp, xwght, gama );

		} // if debug=1 - write dbg output file

		//bi...assume All OK
		ArgCheck = 0;

		//dr...check for error messages
		if ( nlayer < 1 ) {
			ArgCheck = 17;
			ErrorMessage = "Number of layers must be >0.";
			return ArgCheck;
		}

		if ( ( standard < MinStandard ) || ( standard > MaxStandard ) ) {
			ArgCheck = 28;
			ErrorMessage = "Invalid code for standard.";
			return ArgCheck;
		}

		if ( ( ThermalMod < MinThermalMode ) || ( ThermalMod > MaxThermalMode ) ) {
			ArgCheck = 29;
			ErrorMessage = "Invalid code for thermal mode.";
			return ArgCheck;
		}

		if ( ( iwd != 0 ) && ( iwd != 1 ) ) {
			ArgCheck = 18;
			ErrorMessage = "Wind direction can be windward (=0) or leeward (=1).";
			return ArgCheck;
		}

		if ( ( fclr < 0.0 ) || ( fclr > 1.0 ) ) {
			ArgCheck = 19;
			ErrorMessage = "Fraction of sky that is clear can be in range between 0 and 1.";
			return ArgCheck;
		}

		for ( i = 1; i <= nlayer - 1; ++i ) {
			if ( gap( i ) <= 0.0 ) {
				ArgCheck = 20;
				gio::write( a, fmtI3 ) << i;
				ErrorMessage = "Gap width is less than (or equal to) zero. Gap #" + a;
				return ArgCheck;
			}
		}

		for ( i = 1; i <= nlayer; ++i ) {
			if ( thick( i ) <= 0.0 ) {
				ArgCheck = 21;
				gio::write( a, fmtI3 ) << i;
				ErrorMessage = "Layer width is less than (or equal to) zero. Layer #" + a;
				return ArgCheck;
			}
			if ( ( i < nlayer ) && ( LayerType( i ) == VENETBLIND ) && ( LayerType( i + 1 ) == VENETBLIND ) ) {
				ArgCheck = 37;
				ErrorMessage = "Cannot handle two consecutive venetian blinds.";
				return ArgCheck;
			}
			if ( ( i < nlayer ) && ( LayerType( i ) == WOVSHADE ) && ( LayerType( i + 1 ) == WOVSHADE ) ) {
				ArgCheck = 43;
				ErrorMessage = "Cannot handle two consecutive woven shades.";
				return ArgCheck;
			}
			if ( ( i < nlayer ) && ( LayerType( i ) == VENETBLIND ) && ( LayerType( i + 1 ) == WOVSHADE ) ) {
				ArgCheck = 44;
				ErrorMessage = "Cannot handle consecutive venetian blind and woven shade.";
				return ArgCheck;
			}
			if ( ( i < nlayer ) && ( LayerType( i ) == WOVSHADE ) && ( LayerType( i + 1 ) == VENETBLIND ) ) {
				ArgCheck = 44;
				ErrorMessage = "Cannot handle consecutive venetian blind and woven shade.";
				return ArgCheck;
			}
			//Deflection cannot be calculated with IGU containing shading layer. This error check is to be
			//removed once that extension is programmed
			if ( ( CalcDeflection > 0.0 ) && ( LayerType( i ) != SPECULAR ) ) {
				ArgCheck = 42;
				ErrorMessage = "Cannot calculate deflection with IGU containing shading devices.";
				return ArgCheck;
			}
		}

		if ( height <= 0.0 ) {
			ArgCheck = 23;
			ErrorMessage = "IGU cavity height must be greater than zero.";
			return ArgCheck;
		}

		if ( heightt <= 0.0 ) {
			ArgCheck = 24;
			ErrorMessage = "Total window height must be greater than zero.";
			return ArgCheck;
		}

		if ( width <= 0.0 ) {
			ArgCheck = 25;
			ErrorMessage = "Window width must be greater than zero.";
			return ArgCheck;
		}

		if ( ( SDScalar < 0.0 ) || ( SDScalar > 1.0 ) ) {
			ArgCheck = 30;
			ErrorMessage = "SDscalar is out of range (<0.0 or >1.0).";
			return ArgCheck;
		}

		//bi...Check layers and update Venetian blinds properties:
		for ( i = 1; i <= nlayer; ++i ) {
			if ( scon( i ) <= 0.0 ) {
				ArgCheck = 26;
				gio::write( a, fmtI3 ) << i;
				ErrorMessage = "Layer " + a + " has conductivity whcih is less or equal to zero.";
				return ArgCheck;
			}

			if ( ( LayerType( i ) < MinLayType ) || ( LayerType( i ) > MaxLayType ) ) {
				ArgCheck = 22;
				gio::write( a, fmtI3 ) << i;
				ErrorMessage = "Incorrect layer type for layer #" + a + ".  Layer type can either be 0 (glazing layer)," "1 (Venetian blind), 2 (woven shade), 3 (perforated), 4 (diffuse shade) or 5 (bsdf).";
				return ArgCheck;
			}

			//bi...TEMPORARY! Don't allow CSW and CSM method for outdoor and indoor SD layers
			if ( ( IsShadingLayer( LayerType( 1 ) ) ) && ( ( ThermalMod == THERM_MOD_SCW ) || ( ThermalMod == THERM_MOD_CSM ) ) ) {
				ArgCheck = 39;
				ErrorMessage = "CSM and SCW thermal models cannot be used for outdoor and indoor SD layers.";
				return ArgCheck;
			}
			if ( ( IsShadingLayer( LayerType( nlayer ) ) ) && ( ( ThermalMod == THERM_MOD_SCW ) || ( ThermalMod == THERM_MOD_CSM ) ) ) {
				ArgCheck = 39;
				ErrorMessage = "CSM and SCW thermal models cannot be used for outdoor and indoor SD layers.";
				return ArgCheck;
			}

			if ( LayerType( i ) == VENETBLIND ) { // Venetian blind specific:
				if ( SlatThick( i ) <= 0 ) {
					ArgCheck = 31;
					gio::write( a, fmtI3 ) << i;
					ErrorMessage = "Invalid slat thickness (must be >0). Layer #" + a;
					return ArgCheck;
				}
				if ( SlatWidth( i ) <= 0.0 ) {
					ArgCheck = 32;
					gio::write( a, fmtI3 ) << i;
					ErrorMessage = "Invalid slat width (must be >0). Layer #" + a;
					return ArgCheck;
				}
				if ( ( SlatAngle( i ) < -90.0 ) || ( SlatAngle( i ) > 90.0 ) ) {
					ArgCheck = 33;
					gio::write( a, fmtI3 ) << i;
					ErrorMessage = "Invalid slat angle (must be between -90 and 90). Layer #" + a;
					return ArgCheck;
				}
				if ( SlatCond( i ) <= 0.0 ) {
					ArgCheck = 34;
					gio::write( a, fmtI3 ) << i;
					ErrorMessage = "Invalid conductivity of slat material (must be >0). Layer #" + a;
					return ArgCheck;
				}
				if ( SlatSpacing( i ) <= 0.0 ) {
					ArgCheck = 35;
					gio::write( a, fmtI3 ) << i;
					ErrorMessage = "Invalid slat spacing (must be >0). Layer #" + a;
					return ArgCheck;
				}
				if ( ( SlatCurve( i ) != 0.0 ) && ( std::abs( SlatCurve( i ) ) <= ( SlatWidth( i ) / 2.0 ) ) ) {
					ArgCheck = 36;
					gio::write( a, fmtI3 ) << i;
					ErrorMessage = "Invalid curvature radius (absolute value must be >SlatWidth/2, or 0 for flat slats). Layer #" + a;
					return ArgCheck;
				}

			} //  LayerType is Venetian

		} // Layers...

		for ( i = 1; i <= nlayer + 1; ++i ) {
			if ( presure( i ) < 0.0 ) {
				ArgCheck = 27;
				gio::write( a, fmtI3 ) << i;
				if ( ( i == 1 ) || ( i == ( nlayer + 1 ) ) ) {
					ErrorMessage = "One of enviroments (inside or outside) has pressure which is less than zero.";
				} else {
					ErrorMessage = "One of gaps has pressure which is less than zero. Gap #" + a;
				}
				return ArgCheck;
			}
		}

		//bi...Debug output:
		//      open(unit=18,  file='iprop.dbg',  status='unknown', position='APPEND',
		//  2            form='formatted', iostat=nperr)
		//    write(18,5555) 'Iprop1:', iprop(1, 1), iprop(1, 2), iprop (1, 3)
		//    write(18,5555) 'Iprop2:', iprop(2, 1), iprop(2, 2), iprop (2, 3)
		//    write(18,5555) 'Iprop3:', iprop(3, 1), iprop(3, 2), iprop (3, 3)
		//5555  format(A, I3, I3, I3)
		//    close(18)

		return ArgCheck;
	}

	void
	PrepVariablesISO15099(
		int const nlayer,
		Real64 const tout,
		Real64 const tind,
		Real64 & trmin,
		int const isky,
		Real64 const outir, // IR radiance of window's exterior/interior surround (W/m2)
		Real64 const tsky,
		Real64 & esky,
		Real64 const fclr,
		FArray1A< Real64 > gap,
		FArray1A< Real64 > thick,
		FArray1A< Real64 > scon,
		FArray1A< Real64 > const tir,
		FArray1A< Real64 > const emis,
		Real64 const tilt,
		Real64 & hin,
		Real64 & hout,
		FArray1A_int const ibc,
		FArray1A< Real64 > const SlatThick,
		FArray1A< Real64 > const SlatWidth,
		FArray1A< Real64 > const SlatAngle,
		FArray1A< Real64 > const SlatCond,
		FArray1A_int const LayerType,
		int const ThermalMod,
		Real64 const SDScalar,
		Real64 & ShadeEmisRatioOut,
		Real64 & ShadeEmisRatioIn,
		Real64 & ShadeHcRatioOut,
		Real64 & ShadeHcRatioIn,
		FArray1A< Real64 > Keff,
		FArray1A< Real64 > ShadeGapKeffConv,
		Real64 & sc,
		Real64 & shgc,
		Real64 & ufactor,
		Real64 & flux,
		FArray1A< Real64 > LaminateAU,
		FArray1A< Real64 > sumsolU,
		FArray1A< Real64 > sol0,
		Real64 & hint,
		Real64 & houtt,
		Real64 & trmout,
		Real64 & ebsky,
		Real64 & ebroom,
		Real64 & Gout,
		Real64 & Gin,
		FArray1A< Real64 > rir,
		FArray1A< Real64 > vfreevent,
		int & nperr,
		std::string & ErrorMessage
	)
	{

		// Argument array dimensioning
		gap.dim( MaxGap );
		thick.dim( maxlay );
		scon.dim( maxlay );
		tir.dim( maxlay2 );
		emis.dim( maxlay2 );
		ibc.dim( 2 );
		SlatThick.dim( maxlay );
		SlatWidth.dim( maxlay );
		SlatAngle.dim( maxlay );
		SlatCond.dim( maxlay );
		LayerType.dim( maxlay );
		Keff.dim( maxlay );
		ShadeGapKeffConv.dim( MaxGap );
		LaminateAU.dim( maxlay );
		sumsolU.dim( maxlay );
		sol0.dim( maxlay );
		rir.dim( maxlay2 );
		vfreevent.dim( maxlay1 );

		// Locals
		/// Environment related:

		/// Layers:

		/// Venetians:

		//// INPUTS/OUTPUTS:

		/// OUTPUTS:

		int i;
		int k;
		int k1;
		Real64 tiltr;
		Real64 Rsky;
		Real64 Fsky;
		Real64 Fground;
		Real64 e0;
		std::string a;

		//! Initialize variables:

		//! Scalars:
		ShadeEmisRatioOut = 1.0;
		ShadeEmisRatioIn = 1.0;
		ShadeHcRatioOut = 1.0;
		ShadeHcRatioIn = 1.0;

		//! re-initialize iteration parameters:
		sc = 0.0;
		shgc = 0.0;
		ufactor = 0.0;
		flux = 0.0;

		//! Vectors:
		LaminateAU = 0.0;
		sumsolU = 0.0;
		vfreevent = 0.0;
		sol0 = 0.0;
		//bi...    Clear keff, keffc elements:
		Keff = 0.0;
		ShadeGapKeffConv = 0.0;

		// Adjust shading layer properties
		for ( i = 1; i <= nlayer; ++i ) {
			if ( LayerType( i ) == VENETBLIND ) {
				scon( i ) = SlatCond( i );
				if ( ThermalMod == THERM_MOD_SCW ) {
					//bi...the idea here is to have glass-to-glass width the same as before scaling
					//bi...TODO: check for outdoor and indoor blinds! SCW model is only applicable to in-between SDs!!!
					thick( i ) = SlatWidth( i ) * std::cos( SlatAngle( i ) * Pi / 180.0 );
					if ( i > 1 ) gap( i - 1 ) += ( 1.0 - SDScalar ) / 2.0 * thick( i ); //Autodesk:BoundsViolation gap(i-1) @ i=1: Added if condition
					gap( i ) += ( 1.0 - SDScalar ) / 2.0 * thick( i );
					thick( i ) *= SDScalar;
					if ( thick( i ) < SlatThick( i ) ) thick( i ) = SlatThick( i );
				} else if ( ( ThermalMod == THERM_MOD_ISO15099 ) || ( ThermalMod == THERM_MOD_CSM ) ) {
					thick( i ) = SlatThick( i );
				}
			} // Venetian
		}

		hint = hin;
		houtt = hout;
		tiltr = tilt * 2.0 * Pi / 360.0; // convert tilt in degrees to radians

		// external radiation term
		{ auto const SELECT_CASE_var( isky );
		if ( SELECT_CASE_var == 3 ) {
			Gout = outir;
			trmout = root_4( Gout / StefanBoltzmann );
		} else if ( SELECT_CASE_var == 2 ) { // effective clear sky emittance from swinbank (SPC142/ISO15099 equations 131, 132, ...)
			Rsky = 5.31e-13 * pow_6( tout );
			esky = Rsky / ( StefanBoltzmann * pow_4( tout ) ); // check esky const, also check what esky to use when tsky input...
		} else if ( SELECT_CASE_var == 1 ) {
			esky = pow_4( tsky ) / pow_4( tout );
		} else if ( SELECT_CASE_var == 0 ) { // for isky=0 it is assumed that actual values for esky and Tsky are specified
			esky *= pow_4( tsky ) / pow_4( tout );
		} else {
			nperr = 1; // error 2010: isky can be: 0(esky,Tsky input), 1(Tsky input), or 2(Swinbank model)
			return;
		}}

		//Simon: In this case we do not need to recalculate Gout and Trmout again
		if ( isky != 3 ) {
			Fsky = ( 1.0 + std::cos( tiltr ) ) / 2.0;
			Fground = 1.0 - Fsky;
			e0 = Fground + ( 1.0 - fclr ) * Fsky + Fsky * fclr * esky;
			//  Trmout = Tout * e0**0.25d0

			//bi   Set mean radiant temps for fixed combined film coef. case:

			if ( ibc( 1 ) == 1 ) { // outside BC - fixed combined film coef.
				trmout = tout;
			} else {
				trmout = tout * root_4( e0 );
			}

			Gout = StefanBoltzmann * pow_4( trmout );
		} //if (isky.ne.3) then

		ebsky = Gout;

		//     Ebsky=sigma*Tout**4.0d0
		// As of 6/1/01 The expression for Ebsky is different in the current ISO 15099
		// (Ebsky=sigma*Tout**4) because equations 32 and 33 specify Tout and Tind as reference
		// outdoor and indoor temperatures, but I think that they should be Tne and Tni
		// (environmental temps).  Therefore, Ebsky becomes the same as Gout.
		// Inside (room) radiation
		//     Ebroom = sigma*tind**4.0d0
		// See comment above about Ebsky

		if ( ibc( 2 ) == 1 ) { // inside BC - fixed combined film coef.
			trmin = tind;
		}

		Gin = StefanBoltzmann * pow_4( trmin );
		ebroom = Gin;

		// calculate ir reflectance:
		for ( k = 1; k <= nlayer; ++k ) {
			k1 = 2 * k - 1;
			rir( k1 ) = 1 - tir( k1 ) - emis( k1 );
			rir( k1 + 1 ) = 1 - tir( k1 ) - emis( k1 + 1 );
			if ( ( tir( k1 ) < 0.0 ) || ( tir( k1 ) > 1.0 ) || ( tir( k1 + 1 ) < 0.0 ) || ( tir( k1 + 1 ) > 1.0 ) ) {
				nperr = 4;
				gio::write( a, fmtI3 ) << k;
				ErrorMessage = "Layer transmissivity is our of range (<0 or >1). Layer #" + a;
				return;
			}
			if ( ( emis( k1 ) < 0.0 ) || ( emis( k1 ) > 1.0 ) || ( emis( k1 + 1 ) < 0.0 ) || ( emis( k1 + 1 ) > 1.0 ) ) {
				nperr = 14;
				gio::write( a, fmtI3 ) << k;
				ErrorMessage = "Layer emissivity is our of range (<0 or >1). Layer #" + a;
				return;
			}
			if ( ( rir( k1 ) < 0.0 ) || ( rir( k1 ) > 1.0 ) || ( rir( k1 + 1 ) < 0.0 ) || ( rir( k1 + 1 ) > 1.0 ) ) {
				nperr = 3;
				gio::write( a, fmtI3 ) << k;
				ErrorMessage = "Layer reflectivity is our of range (<0 or >1). Layer #" + a;
				return;
			}
		}

	}

	bool
	GoAhead( int const nperr )
	{

		// Return value
		bool GoAhead;

		if ( ( ( nperr > 0 ) && ( nperr < 1000 ) ) || ( ( nperr > 2000 ) && ( nperr < 3000 ) ) ) {
			GoAhead = false; // error
		} else {
			GoAhead = true; // all OK, or a warning
		}

		return GoAhead;

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

} // TARCOGArgs

} // EnergyPlus
