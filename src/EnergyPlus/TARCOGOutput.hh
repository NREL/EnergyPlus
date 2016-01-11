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

#ifndef TARCOGOutput_hh_INCLUDED
#define TARCOGOutput_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Array2A.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace TARCOGOutput {

	// Data
	// variables:
	//bi...Debug files handles:
	//character(len=1000) :: DebugDir
	extern std::string DBGD;
	extern std::string FileMode;
	extern std::string FilePosition;
	extern bool WriteDebugOutput;
	extern int DebugMode;
	extern int winID;
	extern int iguID;

	extern int InArgumentsFile;
	extern int OutArgumentsFile;
	extern int WINCogFile;

	//Intermediate debug files
	extern int IterationCSVFileNumber;
	extern int TarcogIterationsFileNumber;

	extern std::string IterationCSVName;

	//integer, parameter :: IterationHHAT = 102
	//character(len=1000)    :: IterationHHATName = 'IterationHHAT.csv'

	extern std::string WinCogFileName;
	//character(len=1000)    :: SHGCFileName = 'test.w7'
	extern std::string DebugOutputFileName;

	extern std::string const VersionNumber;
	extern std::string const VersionCompileDateCC;

	// Functions

	void
	WriteInputArguments(
		Real64 const tout,
		Real64 const tind,
		Real64 const trmin,
		Real64 const wso,
		int const iwd,
		Real64 const wsi,
		Real64 const dir,
		Real64 const outir,
		int const isky,
		Real64 const tsky,
		Real64 const esky,
		Real64 const fclr,
		Real64 const VacuumPressure,
		Real64 const VacuumMaxGapThickness,
		Array1A_int const ibc,
		Real64 const hout,
		Real64 const hin,
		int const standard,
		int const ThermalMod,
		Real64 const SDScalar,
		Real64 const height,
		Real64 const heightt,
		Real64 const width,
		Real64 const tilt,
		Real64 const totsol,
		int const nlayer,
		Array1A_int const LayerType,
		Array1A< Real64 > const thick,
		Array1A< Real64 > const scon,
		Array1A< Real64 > const asol,
		Array1A< Real64 > const tir,
		Array1A< Real64 > const emis,
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
		Array1A_int const nslice,
		Array1A< Real64 > const LaminateA,
		Array1A< Real64 > const LaminateB,
		Array1A< Real64 > const sumsol,
		Array1A< Real64 > const gap,
		Array1A< Real64 > const vvent,
		Array1A< Real64 > const tvent,
		Array1A< Real64 > const presure,
		Array1A_int const nmix,
		Array2A_int const iprop,
		Array2A< Real64 > const frct,
		Array2A< Real64 > const xgcon,
		Array2A< Real64 > const xgvis,
		Array2A< Real64 > const xgcp,
		Array1A< Real64 > const xwght
	);

	void
	WriteModifiedArguments(
		int const InArgumentsFile,
		std::string const & DBGD,
		Real64 const esky,
		Real64 const trmout,
		Real64 const trmin,
		Real64 const ebsky,
		Real64 const ebroom,
		Real64 const Gout,
		Real64 const Gin,
		int const nlayer,
		Array1A_int const LayerType,
		Array1A_int const nmix,
		Array2A< Real64 > const frct,
		Array1A< Real64 > const thick,
		Array1A< Real64 > const scon,
		Array1A< Real64 > const gap,
		Array2A< Real64 > const xgcon,
		Array2A< Real64 > const xgvis,
		Array2A< Real64 > const xgcp,
		Array1A< Real64 > const xwght
	);

	void
	WriteOutputArguments(
		int & OutArgumentsFile,
		std::string const & DBGD,
		int const nlayer,
		Real64 const tamb,
		Array1A< Real64 > const q,
		Array1A< Real64 > const qv,
		Array1A< Real64 > const qcgas,
		Array1A< Real64 > const qrgas,
		Array1A< Real64 > const theta,
		Array1A< Real64 > const vfreevent,
		Array1A< Real64 > const vvent,
		Array1A< Real64 > const Keff,
		Array1A< Real64 > const ShadeGapKeffConv,
		Real64 const troom,
		Real64 const ufactor,
		Real64 const shgc,
		Real64 const sc,
		Real64 const hflux,
		Real64 const shgct,
		Real64 const hcin,
		Real64 const hrin,
		Real64 const hcout,
		Real64 const hrout,
		Array1A< Real64 > const Ra,
		Array1A< Real64 > const Nu,
		Array1A_int const LayerType,
		Array1A< Real64 > const Ebf,
		Array1A< Real64 > const Ebb,
		Array1A< Real64 > const Rf,
		Array1A< Real64 > const Rb,
		Real64 const ebsky,
		Real64 const Gout,
		Real64 const ebroom,
		Real64 const Gin,
		Real64 const ShadeEmisRatioIn,
		Real64 const ShadeEmisRatioOut,
		Real64 const ShadeHcRatioIn,
		Real64 const ShadeHcRatioOut,
		Real64 const HcUnshadedIn,
		Real64 const HcUnshadedOut,
		Array1A< Real64 > const hcgas,
		Array1A< Real64 > const hrgas,
		Real64 const AchievedErrorTolerance,
		int const NumOfIter
	);

	void
	WriteOutputEN673(
		int & OutArgumentsFile,
		std::string const & DBGD,
		int const nlayer,
		Real64 const ufactor,
		Real64 const hout,
		Real64 const hin,
		Array1A< Real64 > const Ra,
		Array1A< Real64 > const Nu,
		Array1A< Real64 > const hg,
		Array1A< Real64 > const hr,
		Array1A< Real64 > const hs,
		int & nperr
	);

	void
	WriteTARCOGInputFile(
		std::string const & VerNum,
		Real64 const tout,
		Real64 const tind,
		Real64 const trmin,
		Real64 const wso,
		int const iwd,
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
		Array1A_int const ibc,
		Real64 const hout,
		Real64 const hin,
		int const standard,
		int const ThermalMod,
		Real64 const SDScalar,
		Real64 const height,
		Real64 const heightt,
		Real64 const width,
		Real64 const tilt,
		Real64 const totsol,
		int const nlayer,
		Array1A_int const LayerType,
		Array1A< Real64 > const thick,
		Array1A< Real64 > const scon,
		Array1A< Real64 > const YoungsMod,
		Array1A< Real64 > const PoissonsRat,
		Array1A< Real64 > const asol,
		Array1A< Real64 > const tir,
		Array1A< Real64 > const emis,
		Array1A< Real64 > const Atop,
		Array1A< Real64 > const Abot,
		Array1A< Real64 > const Al,
		Array1A< Real64 > const Ar,
		Array1A< Real64 > const Ah,
		Array1A_int const SupportPillar, // Shows whether or not gap have support pillar
		Array1A< Real64 > const PillarSpacing, // Pillar spacing for each gap (used in case there is support pillar)
		Array1A< Real64 > const PillarRadius, // Pillar radius for each gap (used in case there is support pillar)
		Array1A< Real64 > const SlatThick,
		Array1A< Real64 > const SlatWidth,
		Array1A< Real64 > const SlatAngle,
		Array1A< Real64 > const SlatCond,
		Array1A< Real64 > const SlatSpacing,
		Array1A< Real64 > const SlatCurve,
		Array1A_int const nslice,
		Array1A< Real64 > const gap,
		Array1A< Real64 > const GapDef,
		Array1A< Real64 > const vvent,
		Array1A< Real64 > const tvent,
		Array1A< Real64 > const presure,
		Array1A_int const nmix,
		Array2A_int const iprop,
		Array2A< Real64 > const frct,
		Array2A< Real64 > const xgcon,
		Array2A< Real64 > const xgvis,
		Array2A< Real64 > const xgcp,
		Array1A< Real64 > const xwght,
		Array1A< Real64 > const gama
	);

	void
	FinishDebugOutputFiles( int const nperr );

	void
	PrepDebugFilesAndVariables(
		std::string const & Debug_dir,
		std::string const & Debug_file,
		int const Debug_mode,
		int const win_ID,
		int const igu_ID,
		int & nperr
	);

} // TARCOGOutput

} // EnergyPlus

#endif
