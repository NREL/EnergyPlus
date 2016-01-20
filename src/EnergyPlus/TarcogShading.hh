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

#ifndef TarcogShading_hh_INCLUDED
#define TarcogShading_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Array2A.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace TarcogShading {

	// Functions

	void
	shading(
		Array1< Real64 > const & theta,
		Array1< Real64 > const & gap,
		Array1< Real64 > & hgas,
		Array1< Real64 > & hcgas,
		Array1< Real64 > & hrgas,
		Array2< Real64 > const & frct,
		Array2_int const & iprop,
		Array1< Real64 > const & pressure,
		Array1_int const & nmix,
		Array1< Real64 > const & xwght,
		Array2< Real64 > const & xgcon,
		Array2< Real64 > const & xgvis,
		Array2< Real64 > const & xgcp,
		int const nlayer,
		Real64 const width,
		Real64 const height,
		Real64 const angle,
		Real64 const Tout,
		Real64 const Tin,
		Array1< Real64 > const & Atop,
		Array1< Real64 > const & Abot,
		Array1< Real64 > const & Al,
		Array1< Real64 > const & Ar,
		Array1< Real64 > const & Ah,
		Array1< Real64 > const & vvent,
		Array1< Real64 > const & tvent,
		Array1_int const & LayerType,
		Array1< Real64 > & Tgaps,
		Array1< Real64 > & qv,
		int & nperr,
		std::string & ErrorMessage,
		Array1< Real64 > & vfreevent
	);

	void
	forcedventilation(
		Array1A_int const iprop,
		Array1A< Real64 > const frct,
		Real64 const press,
		int const nmix,
		Array1A< Real64 > const xwght,
		Array2A< Real64 > const xgcon,
		Array2A< Real64 > const xgvis,
		Array2A< Real64 > const xgcp,
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
	);

	void
	shadingin(
		Array1A_int const iprop1,
		Array1A< Real64 > const frct1,
		Real64 const press1,
		int const nmix1,
		Array1A_int const iprop2,
		Array1A< Real64 > const frct2,
		Real64 const press2,
		int const nmix2,
		Array1A< Real64 > const xwght,
		Array2A< Real64 > const xgcon,
		Array2A< Real64 > const xgvis,
		Array2A< Real64 > const xgcp,
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
	);

	void
	shadingedge(
		Array1A_int const iprop1,
		Array1A< Real64 > const frct1,
		Real64 const press1,
		int const nmix1,
		Array1A_int const iprop2,
		Array1A< Real64 > const frct2,
		Real64 const press2,
		int const nmix2,
		Array1A< Real64 > const xwght,
		Array2A< Real64 > const xgcon,
		Array2A< Real64 > const xgvis,
		Array2A< Real64 > const xgcp,
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
	);

} // TarcogShading

} // EnergyPlus

#endif
