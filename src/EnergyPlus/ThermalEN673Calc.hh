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

#ifndef ThermalEN673Calc_hh_INCLUDED
#define ThermalEN673Calc_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Array2A.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace ThermalEN673Calc {

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
	);

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
	);

	void
	linint(
		Real64 const x1,
		Real64 const x2,
		Real64 const y1,
		Real64 const y2,
		Real64 const x,
		Real64 & y
	);

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
	);

} // ThermalEN673Calc

} // EnergyPlus

#endif
