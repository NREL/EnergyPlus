// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <TARCOGDeflection.hh>
#include <DataGlobals.hh>
#include <TARCOGCommon.hh>
#include <TARCOGParams.hh>

namespace EnergyPlus {

namespace TARCOGDeflection {

	// MODULE INFORMATION:
	//       AUTHOR         Simon Vidanovic
	//       DATE WRITTEN   October/22/2011
	//       MODIFIED       na
	//       RE-ENGINEERED  na
	//  Revision: 7.0.02  (October 22, 2011)
	//   - Initial setup

	// PURPOSE OF THIS MODULE:
	// A module which contains functions for deflection calculation

	// METHODOLOGY EMPLOYED:
	// <description>

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// USE STATEMENTS:

	// Using/Aliasing
	using namespace DataGlobals;
	//use TARCOGGassesParams
	using namespace TARCOGParams;
	using namespace TARCOGCommon;

	// Functions

	void
	PanesDeflection(
		int const DeflectionStandard,
		Real64 const W,
		Real64 const H,
		int const nlayer,
		Real64 const Pa,
		Real64 const Pini,
		Real64 const Tini,
		FArray1A< Real64 > const PaneThickness,
		FArray1A< Real64 > const NonDeflectedGapWidth,
		FArray1A< Real64 > DeflectedGapWidthMax,
		FArray1A< Real64 > DeflectedGapWidthMean,
		FArray1A< Real64 > const PanelTemps,
		FArray1A< Real64 > const YoungsMod,
		FArray1A< Real64 > const PoissonsRat,
		FArray1A< Real64 > LayerDeflection,
		int & nperr,
		std::string & ErrorMessage
	)
	{
		//***********************************************************************
		// PanesDeflection - calculates deflection of panes and recalculate gap
		//                   widths at maximal point of deflection
		//***********************************************************************

		//INPUT

		// Argument array dimensioning
		PaneThickness.dim( maxlay );
		NonDeflectedGapWidth.dim( MaxGap );
		DeflectedGapWidthMax.dim( MaxGap );
		DeflectedGapWidthMean.dim( MaxGap );
		PanelTemps.dim( maxlay2 );
		YoungsMod.dim( maxlay );
		PoissonsRat.dim( maxlay );
		LayerDeflection.dim( maxlay );

		// Locals
		//OUTPUT

		//Localy used
		FArray1D< Real64 > DCoeff( maxlay );
		int i;

		i = 0;
		// first calculate D coefficients since that will be necessary for any of selected standards
		for ( i = 1; i <= nlayer; ++i ) {
			DCoeff( i ) = YoungsMod( i ) * pow_3( PaneThickness( i ) ) / ( 12 * ( 1 - pow_2( PoissonsRat( i ) ) ) );
		}

		{ auto const SELECT_CASE_var( DeflectionStandard );
		if ( SELECT_CASE_var == NO_DEFLECTION_CALCULATION ) {
			return;
		} else if ( SELECT_CASE_var == DEFLECTION_CALC_TEMPERATURE ) {
			DeflectionTemperatures( nlayer, W, H, Pa, Pini, Tini, NonDeflectedGapWidth, DeflectedGapWidthMax, DeflectedGapWidthMean, PanelTemps, DCoeff, LayerDeflection, nperr, ErrorMessage );
		} else if ( SELECT_CASE_var == DEFLECTION_CALC_GAP_WIDTHS ) {
			DeflectionWidths( nlayer, W, H, DCoeff, NonDeflectedGapWidth, DeflectedGapWidthMax, DeflectedGapWidthMean, LayerDeflection );
		} else {
			return;
		}}

	}

	void
	DeflectionTemperatures(
		int const nlayer,
		Real64 const W,
		Real64 const H,
		Real64 const Pa,
		Real64 const Pini,
		Real64 const Tini,
		FArray1A< Real64 > const NonDeflectedGapWidth,
		FArray1A< Real64 > DeflectedGapWidthMax,
		FArray1A< Real64 > DeflectedGapWidthMean,
		FArray1A< Real64 > const PanelTemps,
		FArray1A< Real64 > DCoeff,
		FArray1A< Real64 > LayerDeflection,
		int & nperr,
		std::string & ErrorMessage
	)
	{
		//***********************************************************************************************************
		// DeflectionTemperatures - calculates deflection of panes and recalculate gap
		//                          widths at maximal point of deflection based on gap pressures and temperatures
		//***********************************************************************************************************
		//INPUT

		// Argument array dimensioning
		NonDeflectedGapWidth.dim( MaxGap );
		DeflectedGapWidthMax.dim( MaxGap );
		DeflectedGapWidthMean.dim( MaxGap );
		PanelTemps.dim( maxlay2 );
		DCoeff.dim( maxlay );
		LayerDeflection.dim( maxlay );

		// Locals
		//OUTPUT

		// Static constants
		static Real64 const Pi_6( pow_6( Pi ) );

		//localy used
		FArray1D< Real64 > DPressure( maxlay ); // delta pressure at each glazing layer
		FArray1D< Real64 > Vini( MaxGap );
		FArray1D< Real64 > Vgap( MaxGap );
		FArray1D< Real64 > Pgap( MaxGap );
		FArray1D< Real64 > Tgap( MaxGap );
		Real64 MaxLDSum;
		Real64 MeanLDSum;
		Real64 Ratio;
		int i;
		int j;

		i = 0;
		j = 0;
		Ratio = 0.0;
		MeanLDSum = 0.0;
		MaxLDSum = 0.0;

		//calculate Vini for each gap
		for ( i = 1; i <= nlayer - 1; ++i ) {
			Vini( i ) = NonDeflectedGapWidth( i ) * W * H;
		} //do i = 1, nlayer

		MaxLDSum = LDSumMax( W, H );
		MeanLDSum = LDSumMean( W, H );
		Ratio = MeanLDSum / MaxLDSum;

		//calculate Vgap for each gap
		Real64 const W_H_Ratio( W * H * Ratio );
		for ( i = 1; i <= nlayer - 1; ++i ) {
			Vgap( i ) = Vini( i ) + W_H_Ratio * ( LayerDeflection( i ) - LayerDeflection( i + 1 ) );
		} //do i = 1, nlayer

		//calculate Tgap for each gap
		for ( i = 1; i <= nlayer - 1; ++i ) {
			j = 2 * i;
			Tgap( i ) = ( PanelTemps( j ) + PanelTemps( j + 1 ) ) / 2;
		} //do i = 1, nlayer

		for ( i = 1; i <= nlayer - 1; ++i ) {
			Pgap( i ) = Pini * Vini( i ) * Tgap( i ) / ( Tini * Vgap( i ) );
		} //do i = 1, nlayer

		DPressure( 1 ) = Pgap( 1 ) - Pa;
		if ( nlayer > 1 ) {
			DPressure( nlayer ) = Pa - Pgap( nlayer - 1 );
		}

		for ( i = 2; i <= nlayer - 1; ++i ) {
			DPressure( i ) = Pgap( i ) - Pgap( i - 1 );
		} //do i = 1, nlayer

		Real64 const deflection_fac( DeflectionRelaxation * MaxLDSum * 16 );
		for ( i = 1; i <= nlayer; ++i ) {
			LayerDeflection( i ) += deflection_fac * DPressure( i ) / ( Pi_6 * DCoeff( i ) );
		}

		for ( i = 1; i <= nlayer - 1; ++i ) {
			DeflectedGapWidthMax( i ) = NonDeflectedGapWidth( i ) + LayerDeflection( i ) - LayerDeflection( i + 1 );
			if ( DeflectedGapWidthMax( i ) < 0.0 ) {
				nperr = 2001; //glazing panes collapsed
				ErrorMessage = "Glazing panes collapsed";
			}
		}

		for ( i = 1; i <= nlayer - 1; ++i ) {
			DeflectedGapWidthMean( i ) = NonDeflectedGapWidth( i ) + Ratio * ( DeflectedGapWidthMax( i ) - NonDeflectedGapWidth( i ) );
		}

	}

	void
	DeflectionWidths(
		int const nlayer,
		Real64 const W,
		Real64 const H,
		FArray1A< Real64 > DCoeff,
		FArray1A< Real64 > const NonDeflectedGapWidth,
		FArray1A< Real64 > const DeflectedGapWidthMax,
		FArray1A< Real64 > DeflectedGapWidthMean,
		FArray1A< Real64 > LayerDeflection
	)
	{
		//INPUT

		// Argument array dimensioning
		DCoeff.dim( maxlay );
		NonDeflectedGapWidth.dim( MaxGap );
		DeflectedGapWidthMax.dim( MaxGap );
		DeflectedGapWidthMean.dim( MaxGap );
		LayerDeflection.dim( maxlay );

		// Locals
		//OUTPUT
		//integer, intent(inout) :: nperr
		//character(len=*) :: ErrorMessage

		//LOCALS
		int i;
		int j;
		Real64 nominator;
		Real64 denominator;
		Real64 SumL;
		Real64 MaxLDSum;
		Real64 MeanLDSum;
		Real64 Ratio;

		i = 0;
		j = 0;
		Ratio = 0.0;
		MeanLDSum = 0.0;
		MaxLDSum = 0.0;

		nominator = 0.0;
		for ( i = 1; i <= nlayer - 1; ++i ) {
			SumL = 0.0;
			for ( j = i; j <= nlayer - 1; ++j ) {
				SumL += NonDeflectedGapWidth( j ) - DeflectedGapWidthMax( j );
			}
			nominator += SumL * DCoeff( i );
		}

		denominator = 0.0;
		for ( i = 1; i <= nlayer; ++i ) {
			denominator += DCoeff( i );
		}

		LayerDeflection( nlayer ) = nominator / denominator;

		for ( i = nlayer - 1; i >= 1; --i ) {
			LayerDeflection( i ) = DeflectedGapWidthMax( i ) - NonDeflectedGapWidth( i ) + LayerDeflection( i + 1 );
		}

		MaxLDSum = LDSumMax( W, H );
		MeanLDSum = LDSumMean( W, H );
		Ratio = MeanLDSum / MaxLDSum;

		for ( i = 1; i <= nlayer - 1; ++i ) {
			DeflectedGapWidthMean( i ) = NonDeflectedGapWidth( i ) + Ratio * ( DeflectedGapWidthMax( i ) - NonDeflectedGapWidth( i ) );
		}

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

} // TARCOGDeflection

} // EnergyPlus
