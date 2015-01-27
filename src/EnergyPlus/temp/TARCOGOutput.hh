#ifndef TARCOGOutput_hh_INCLUDED
#define TARCOGOutput_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1A.hh>
#include <ObjexxFCL/FArray2A.hh>

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
		FArray1A_int const ibc,
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
		FArray1A_int const LayerType,
		FArray1A< Real64 > const thick,
		FArray1A< Real64 > const scon,
		FArray1A< Real64 > const asol,
		FArray1A< Real64 > const tir,
		FArray1A< Real64 > const emis,
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
		FArray1A_int const nslice,
		FArray1A< Real64 > const LaminateA,
		FArray1A< Real64 > const LaminateB,
		FArray1A< Real64 > const sumsol,
		FArray1A< Real64 > const gap,
		FArray1A< Real64 > const vvent,
		FArray1A< Real64 > const tvent,
		FArray1A< Real64 > const presure,
		FArray1A_int const nmix,
		FArray2A_int const iprop,
		FArray2A< Real64 > const frct,
		FArray2A< Real64 > const xgcon,
		FArray2A< Real64 > const xgvis,
		FArray2A< Real64 > const xgcp,
		FArray1A< Real64 > const xwght
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
		FArray1A_int const LayerType,
		FArray1A_int const nmix,
		FArray2A< Real64 > const frct,
		FArray1A< Real64 > const thick,
		FArray1A< Real64 > const scon,
		FArray1A< Real64 > const gap,
		FArray2A< Real64 > const xgcon,
		FArray2A< Real64 > const xgvis,
		FArray2A< Real64 > const xgcp,
		FArray1A< Real64 > const xwght
	);

	void
	WriteOutputArguments(
		int & OutArgumentsFile,
		std::string const & DBGD,
		int const nlayer,
		Real64 const tamb,
		FArray1A< Real64 > const q,
		FArray1A< Real64 > const qv,
		FArray1A< Real64 > const qcgas,
		FArray1A< Real64 > const qrgas,
		FArray1A< Real64 > const theta,
		FArray1A< Real64 > const vfreevent,
		FArray1A< Real64 > const vvent,
		FArray1A< Real64 > const Keff,
		FArray1A< Real64 > const ShadeGapKeffConv,
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
		FArray1A< Real64 > const Ra,
		FArray1A< Real64 > const Nu,
		FArray1A_int const LayerType,
		FArray1A< Real64 > const Ebf,
		FArray1A< Real64 > const Ebb,
		FArray1A< Real64 > const Rf,
		FArray1A< Real64 > const Rb,
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
		FArray1A< Real64 > const hcgas,
		FArray1A< Real64 > const hrgas,
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
		FArray1A< Real64 > const Ra,
		FArray1A< Real64 > const Nu,
		FArray1A< Real64 > const hg,
		FArray1A< Real64 > const hr,
		FArray1A< Real64 > const hs,
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
		FArray1A_int const ibc,
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
		FArray1A_int const LayerType,
		FArray1A< Real64 > const thick,
		FArray1A< Real64 > const scon,
		FArray1A< Real64 > const YoungsMod,
		FArray1A< Real64 > const PoissonsRat,
		FArray1A< Real64 > const asol,
		FArray1A< Real64 > const tir,
		FArray1A< Real64 > const emis,
		FArray1A< Real64 > const Atop,
		FArray1A< Real64 > const Abot,
		FArray1A< Real64 > const Al,
		FArray1A< Real64 > const Ar,
		FArray1A< Real64 > const Ah,
		FArray1A_int const SupportPillar, // Shows whether or not gap have support pillar
		FArray1A< Real64 > const PillarSpacing, // Pillar spacing for each gap (used in case there is support pillar)
		FArray1A< Real64 > const PillarRadius, // Pillar radius for each gap (used in case there is support pillar)
		FArray1A< Real64 > const SlatThick,
		FArray1A< Real64 > const SlatWidth,
		FArray1A< Real64 > const SlatAngle,
		FArray1A< Real64 > const SlatCond,
		FArray1A< Real64 > const SlatSpacing,
		FArray1A< Real64 > const SlatCurve,
		FArray1A_int const nslice,
		FArray1A< Real64 > const gap,
		FArray1A< Real64 > const GapDef,
		FArray1A< Real64 > const vvent,
		FArray1A< Real64 > const tvent,
		FArray1A< Real64 > const presure,
		FArray1A_int const nmix,
		FArray2A_int const iprop,
		FArray2A< Real64 > const frct,
		FArray2A< Real64 > const xgcon,
		FArray2A< Real64 > const xgvis,
		FArray2A< Real64 > const xgcp,
		FArray1A< Real64 > const xwght,
		FArray1A< Real64 > const gama
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

} // TARCOGOutput

} // EnergyPlus

#endif
