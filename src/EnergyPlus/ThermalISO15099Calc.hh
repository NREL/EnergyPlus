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

#ifndef ThermalISO15099Calc_hh_INCLUDED
#define ThermalISO15099Calc_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Array2A.hh>

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
	);

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
	);

	void
	TemperaturesFromEnergy(
		Array1A< Real64 > theta,
		Array1A< Real64 > Tgap,
		Array1A< Real64 > const Ebf,
		Array1A< Real64 > const Ebb,
		int const nlayer,
		int & nperr,
		std::string & ErrorMessage
	);

	void
	solarISO15099(
		Real64 const totsol,
		Real64 const rtot,
		Array1A< Real64 > const rs,
		int const nlayer,
		Array1A< Real64 > const absol,
		Real64 & sf
	);

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
	);

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
	);

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
	);

	void
	filmPillar(
		Array1A_int const SupportPillar, // Shows whether or not gap have support pillar
		Array1A< Real64 > const scon, // Conductivity of glass layers
		Array1A< Real64 > const PillarSpacing, // Pillar spacing for each gap (used in case there is support pillar)
		Array1A< Real64 > const PillarRadius, // Pillar radius for each gap (used in case there is support pillar)
		int const nlayer,
		Array1A< Real64 > const gap,
		Array1A< Real64 > hcgas,
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
	);

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
	);

	void
	CalculateFuncResults(
		int const nlayer,
		Array2< Real64 > const & a,
		Array1< Real64 > const & b,
		Array1< Real64 > const & x,
		Array1< Real64 > & FRes
	);

} // ThermalISO15099Calc

} // EnergyPlus

#endif
