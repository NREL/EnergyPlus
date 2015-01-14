#ifndef ThermalISO15099Calc_hh_INCLUDED
#define ThermalISO15099Calc_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1A.hh>
#include <ObjexxFCL/FArray2A.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace ThermalISO15099Calc {

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
	);

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
		FArray1A< Real64 > gap,
		FArray1A< Real64 > thick,
		FArray1A< Real64 > scon,
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
		FArray1A< Real64 > theta,
		FArray1A< Real64 > q,
		FArray1A< Real64 > qv,
		Real64 & ufactor,
		Real64 & sc,
		Real64 & hflux,
		Real64 & hcin,
		Real64 & hcout,
		Real64 & hrin,
		Real64 & hrout,
		Real64 & hin,
		Real64 & hout,
		FArray1A< Real64 > hcgas,
		FArray1A< Real64 > hrgas,
		Real64 & shgc,
		int & nperr,
		std::string & ErrorMessage,
		Real64 & shgct,
		Real64 & tamb,
		Real64 & troom,
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
		FArray1A< Real64 > Ra,
		FArray1A< Real64 > Nu,
		int const ThermalMod,
		int const Debug_mode, // Switch for debug output files:
		Real64 & ShadeEmisRatioOut,
		Real64 & ShadeEmisRatioIn,
		Real64 & ShadeHcRatioOut,
		Real64 & ShadeHcRatioIn,
		Real64 & HcUnshadedOut,
		Real64 & HcUnshadedIn,
		FArray1A< Real64 > Keff,
		FArray1A< Real64 > ShadeGapKeffConv,
		Real64 const SDScalar,
		int const SHGCCalc, // SHGC calculation switch:
		int & NumOfIterations
	);

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
		FArray1A< Real64 > const tir,
		FArray1A< Real64 > const rir,
		FArray1A< Real64 > const emis,
		FArray1A< Real64 > const gap,
		FArray1A< Real64 > const thick,
		FArray1A< Real64 > const scon,
		Real64 const tilt,
		FArray1A< Real64 > const asol,
		Real64 const height,
		Real64 const heightt,
		Real64 const width,
		FArray2A_int const iprop,
		FArray2A< Real64 > const frct,
		FArray1A< Real64 > const presure,
		FArray1A_int const nmix,
		FArray1A< Real64 > const wght,
		FArray2A< Real64 > const gcon,
		FArray2A< Real64 > const gvis,
		FArray2A< Real64 > const gcp,
		FArray1A< Real64 > const gama,
		FArray1A_int const SupportPillar,
		FArray1A< Real64 > const PillarSpacing,
		FArray1A< Real64 > const PillarRadius,
		FArray1A< Real64 > theta,
		FArray1A< Real64 > q,
		FArray1A< Real64 > qv,
		Real64 & flux,
		Real64 & hcin,
		Real64 & hrin,
		Real64 & hcout,
		Real64 & hrout,
		Real64 & hin,
		Real64 & hout,
		FArray1A< Real64 > hcgas,
		FArray1A< Real64 > hrgas,
		Real64 & ufactor,
		int & nperr,
		std::string & ErrorMessage,
		Real64 & tamb,
		Real64 & troom,
		FArray1A_int const ibc,
		FArray1A< Real64 > const Atop,
		FArray1A< Real64 > const Abot,
		FArray1A< Real64 > const Al,
		FArray1A< Real64 > const Ar,
		FArray1A< Real64 > const Ah,
		FArray1A< Real64 > const vvent,
		FArray1A< Real64 > const tvent,
		FArray1A_int const LayerType,
		FArray1A< Real64 > Ra,
		FArray1A< Real64 > Nu,
		FArray1A< Real64 > vfreevent,
		FArray1A< Real64 > qcgas,
		FArray1A< Real64 > qrgas,
		FArray1A< Real64 > Ebf,
		FArray1A< Real64 > Ebb,
		FArray1A< Real64 > Rf,
		FArray1A< Real64 > Rb,
		Real64 & ShadeEmisRatioOut,
		Real64 & ShadeEmisRatioIn,
		Real64 & ShadeHcModifiedOut,
		Real64 & ShadeHcModifiedIn,
		int const ThermalMod,
		int const Debug_mode, // Switch for debug output files:
		Real64 & AchievedErrorTolerance,
		int & TotalIndex
	);

	void
	guess(
		Real64 const tout,
		Real64 const tind,
		int const nlayer,
		FArray1A< Real64 > const gap,
		FArray1A< Real64 > const thick,
		Real64 & width,
		FArray1A< Real64 > theta,
		FArray1A< Real64 > Ebb,
		FArray1A< Real64 > Ebf,
		FArray1A< Real64 > Tgap
	);

	void
	TemperaturesFromEnergy(
		FArray1A< Real64 > theta,
		FArray1A< Real64 > Tgap,
		FArray1A< Real64 > const Ebf,
		FArray1A< Real64 > const Ebb,
		int const nlayer,
		int & nperr,
		std::string & ErrorMessage
	);

	void
	solarISO15099(
		Real64 const totsol,
		Real64 const rtot,
		FArray1A< Real64 > const rs,
		int const nlayer,
		FArray1A< Real64 > const absol,
		Real64 & sf
	);

	void
	resist(
		int const nlayer,
		Real64 const trmout,
		Real64 const Tout,
		Real64 const trmin,
		Real64 const tind,
		FArray1A< Real64 > const hcgas,
		FArray1A< Real64 > const hrgas,
		FArray1A< Real64 > Theta,
		FArray1A< Real64 > qlayer,
		FArray1A< Real64 > const qv,
		FArray1A_int const LayerType,
		FArray1A< Real64 > const thick,
		FArray1A< Real64 > const scon,
		Real64 & ufactor,
		Real64 & flux,
		FArray1A< Real64 > qcgas,
		FArray1A< Real64 > qrgas
	);

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
		FArray1A< Real64 > const gap,
		Real64 const height,
		Real64 const heightt,
		FArray1A< Real64 > const scon,
		Real64 const tilt,
		FArray1A< Real64 > theta,
		FArray1A< Real64 > const Tgap,
		FArray1A< Real64 > Radiation,
		Real64 const trmout,
		Real64 const trmin,
		FArray2A_int const iprop,
		FArray2A< Real64 > const frct,
		FArray1A< Real64 > const presure,
		FArray1A_int const nmix,
		FArray1A< Real64 > const wght,
		FArray2A< Real64 > const gcon,
		FArray2A< Real64 > const gvis,
		FArray2A< Real64 > const gcp,
		FArray1A< Real64 > const gama,
		FArray1A_int const SupportPillar,
		FArray1A< Real64 > const PillarSpacing,
		FArray1A< Real64 > const PillarRadius,
		FArray1A< Real64 > hgas,
		FArray1A< Real64 > hcgas,
		FArray1A< Real64 > hrgas,
		Real64 & hcin,
		Real64 & hcout,
		Real64 const hin,
		Real64 const hout,
		int const index,
		FArray1A_int const ibc,
		int & nperr,
		std::string & ErrorMessage,
		Real64 & hrin,
		Real64 & hrout,
		FArray1A< Real64 > Ra,
		FArray1A< Real64 > Nu
	);

	void
	filmi(
		Real64 const tair,
		Real64 const t,
		int const nlayer,
		Real64 const tilt,
		Real64 const wsi,
		Real64 const height,
		FArray2A_int const iprop,
		FArray2A< Real64 > const frct,
		FArray1A< Real64 > const presure,
		FArray1A_int const nmix,
		FArray1A< Real64 > const wght,
		FArray2A< Real64 > const gcon,
		FArray2A< Real64 > const gvis,
		FArray2A< Real64 > const gcp,
		Real64 & hcin,
		int const ibc,
		int & nperr,
		std::string & ErrorMessage
	);

	void
	filmg(
		Real64 const tilt,
		FArray1A< Real64 > const theta,
		FArray1A< Real64 > const Tgap,
		int const nlayer,
		Real64 const height,
		FArray1A< Real64 > const gap,
		FArray2A_int const iprop,
		FArray2A< Real64 > const frct,
		Real64 const VacuumPressure,
		FArray1A< Real64 > const presure,
		FArray1A_int const nmix,
		FArray1A< Real64 > const wght,
		FArray2A< Real64 > const gcon,
		FArray2A< Real64 > const gvis,
		FArray2A< Real64 > const gcp,
		FArray1A< Real64 > const gama,
		FArray1A< Real64 > hcgas,
		FArray1A< Real64 > Rayleigh,
		FArray1A< Real64 > Nu,
		int & nperr,
		std::string & ErrorMessage
	);

	void
	filmPillar(
		FArray1A_int const SupportPillar, // Shows whether or not gap have support pillar
		FArray1A< Real64 > const scon, // Conductivity of glass layers
		FArray1A< Real64 > const PillarSpacing, // Pillar spacing for each gap (used in case there is support pillar)
		FArray1A< Real64 > const PillarRadius, // Pillar radius for each gap (used in case there is support pillar)
		int const nlayer,
		FArray1A< Real64 > const gap,
		FArray1A< Real64 > hcgas,
		Real64 const VacuumMaxGapThickness,
		int & nperr,
		std::string & ErrorMessage
	);

	void
	nusselt(
		Real64 const tilt,
		Real64 const ra,
		Real64 const asp,
		Real64 & gnu,
		int & nperr,
		std::string & ErrorMessage
	);

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
		FArray1A_int const ibc,
		Real64 const tout,
		Real64 const tind,
		int const nlayer,
		FArray1A< Real64 > const theta,
		Real64 const wso,
		Real64 const wsi,
		int const iwd,
		Real64 const height,
		Real64 const heightt,
		Real64 const tilt,
		FArray1A< Real64 > const thick,
		FArray1A< Real64 > const gap,
		Real64 const hout,
		Real64 const hrout,
		Real64 const hin,
		Real64 const hrin,
		FArray2A_int const iprop,
		FArray2A< Real64 > const frct,
		FArray1A< Real64 > const presure,
		FArray1A_int const nmix,
		FArray1A< Real64 > const wght,
		FArray2A< Real64 > const gcon,
		FArray2A< Real64 > const gvis,
		FArray2A< Real64 > const gcp,
		int const index,
		Real64 const SDScalar,
		FArray1A< Real64 > const Ebf,
		FArray1A< Real64 > const Ebb,
		FArray1A< Real64 > hgas,
		FArray1A< Real64 > hhat,
		int & nperr,
		std::string & ErrorMessage
	);

	void
	storeIterationResults(
		int const nlayer,
		int const index,
		FArray1A< Real64 > const theta,
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
		FArray1A< Real64 > const Ebb,
		FArray1A< Real64 > const Ebf,
		FArray1A< Real64 > const Rb,
		FArray1A< Real64 > const Rf,
		int & nperr
	);

	void
	CalculateFuncResults(
		int const nlayer,
		FArray2A< Real64 > const a,
		FArray1A< Real64 > const b,
		FArray1A< Real64 > const x,
		FArray1A< Real64 > FRes
	);

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
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

} // ThermalISO15099Calc

} // EnergyPlus

#endif
