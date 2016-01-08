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

// C++ Headers
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>
#include <ObjexxFCL/Time_Date.hh>

// EnergyPlus Headers
#include <TARCOGOutput.hh>
#include <CommandLineInterface.hh>
#include <DataGlobals.hh>
#include <DataStringGlobals.hh>
#include <DataPrecisionGlobals.hh>
#include <TARCOGCommon.hh>
#include <TARCOGGassesParams.hh>
#include <TARCOGParams.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace TARCOGOutput {

	// MODULE INFORMATION:
	//       AUTHOR         Simon Vidanovic
	//       DATE WRITTEN   June/22/2010
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	//  Revision: 6.0.36  (June/22/2010)
	//   - Initial setup, extracted from TARCOG.for

	// PURPOSE OF THIS MODULE:
	// A module which contains debug dump subroutines

	// METHODOLOGY EMPLOYED:
	// <description>

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// USE STATEMENTS:

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace TARCOGCommon;
	using namespace TARCOGGassesParams;
	using namespace TARCOGParams;

	// Data
	// variables:
	//bi...Debug files handles:
	//character(len=1000) :: DebugDir
	std::string DBGD;
	std::string FileMode;
	std::string FilePosition;
	bool WriteDebugOutput;
	int DebugMode;
	int winID;
	int iguID;

	int InArgumentsFile( statusClosed );
	int OutArgumentsFile( statusClosed );
	int WINCogFile( statusClosed );

	//Intermediate debug files
	int IterationCSVFileNumber( statusClosed );
	int TarcogIterationsFileNumber( statusClosed );

	std::string IterationCSVName( "IterationResults.csv" );

	//integer, parameter :: IterationHHAT = 102
	//character(len=1000)    :: IterationHHATName = 'IterationHHAT.csv'

	std::string WinCogFileName( "test.w7" );
	//character(len=1000)    :: SHGCFileName = 'test.w7'
	std::string DebugOutputFileName( "Tarcog.dbg" );

	std::string const VersionNumber( " 7.0.15.00 " );
	std::string const VersionCompileDateCC( " August 02, 2012" );

	static gio::Fmt fmtLD( "*" );

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
	)
	{

		// Using/Aliasing
		using DataGlobals::KelvinConv;

		// Argument array dimensioning
		ibc.dim( 2 );
		LayerType.dim( maxlay );
		thick.dim( maxlay );
		scon.dim( maxlay );
		asol.dim( maxlay );
		tir.dim( maxlay2 );
		emis.dim( maxlay2 );
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
		nslice.dim( maxlay );
		LaminateA.dim( maxlay );
		LaminateB.dim( maxlay );
		sumsol.dim( maxlay );
		gap.dim( maxlay );
		vvent.dim( maxlay1 );
		tvent.dim( maxlay1 );
		presure.dim( maxlay1 );
		nmix.dim( maxlay1 );
		iprop.dim( maxgas, maxlay1 );
		frct.dim( maxgas, maxlay1 );
		xgcon.dim( 3, maxgas );
		xgvis.dim( 3, maxgas );
		xgcp.dim( 3, maxgas );
		xwght.dim( maxgas );

		// Locals
		Array1D_int DATE_TIME( 8 );
		Array1D_string real_CLOCK( 3 );

		int i;
		int j;

		// Formats
		static gio::Fmt Format_10001( "('TARCOG v.',A,'compiled ',A)" );
		static gio::Fmt Format_1000( "('TARCOG input arguments:')" );
		static gio::Fmt Format_1001( "('TARCOG debug output, ',I4,'-',I2.2,'-',I2.2,', ',I2.2,':',I2.2,':',I2.2)" );
		static gio::Fmt Format_1002( "('     WindowID:',I8,'  - Not specified')" );
		static gio::Fmt Format_1003( "('     WindowID:',I8,' ')" );
		static gio::Fmt Format_1006( "('     IGUID:   ',I8,'  - Not specified')" );
		static gio::Fmt Format_1007( "('     IGUID:   ',I8,' ')" );
		static gio::Fmt Format_1005( "('Simulation parameters:')" );
		static gio::Fmt Format_1010( "('  Tout       =  ',F10.6,' K ( ',F7.3,' deg C) - Outdoor temperature')" );
		static gio::Fmt Format_1015( "('  Tint       =  ',F10.6,' K ( ',F7.3,' deg C) - Indoor temperature')" );
		static gio::Fmt Format_1014( "('Adjusted input arguments:')" );
		static gio::Fmt Format_1013( "(' Gass coefficients:')" );
		static gio::Fmt Format_1016( "('  Trmout     =  ',F10.6,' K ( ',F7.3,' deg C) - Outdoor mean radiant temp.')" );
		static gio::Fmt Format_1017( "('  Gout       =  ',F10.6,' ')" );
		static gio::Fmt Format_1018( "('  Gin        =  ',F10.6,' ')" );
		static gio::Fmt Format_1019( "('  Ebsky      =  ',F10.6,' ')" );
		static gio::Fmt Format_10191( "('  Ebroom     =  ',F10.6,' ')" );
		static gio::Fmt Format_1020( "('  Trmin      =  ',F10.6,' K ( ',F7.3,' deg C) - Indoor mean radiant temp.')" );
		static gio::Fmt Format_1030( "('  wso        =  ',F7.3,'    - Outdoor wind speed [m/s]')" );
		static gio::Fmt Format_1032( "('  iwd        =    0        - Wind direction - windward')" );
		static gio::Fmt Format_1033( "('  iwd        =    1        - Wind direction - leeward')" );
		static gio::Fmt Format_1035( "('  wsi        =  ',F7.3,'    - Indoor forced air speed [m/s]')" );
		static gio::Fmt Format_1040( "('  dir        = ',F8.3,'    - Direct solar radiation [W/m^2]')" );
		static gio::Fmt Format_1041( "('  outir       = ',F8.3,'    - IR radiation [W/m^2]')" );
		static gio::Fmt Format_1045( "('  isky       =  ',I3,'        - Flag for handling tsky, esky')" );
		static gio::Fmt Format_1050( "('  tsky           =  ',F10.6,' K ( ',F7.3,' deg C) - Night sky temperature')" );
		static gio::Fmt Format_1055( "('  esky           =  ',F7.3,'    - Effective night sky emmitance')" );
		static gio::Fmt Format_1060( "('  fclr           =  ',F7.3,'    - Fraction of sky that is clear')" );
		static gio::Fmt Format_1061( "('  VacuumPressure =  ',F7.3,'    - maximum allowed gas pressure to be considered as vacuum')" );
		static gio::Fmt Format_1062( "('  VacuumMaxGapThickness =  ',F7.3,'    - maximum allowed vacuum gap thickness with support pillar')" );
		static gio::Fmt Format_1063( "('  ibc(1)         =  ',I3,'        - Outdoor BC switch')" );
		static gio::Fmt Format_1065( "('  hout           =  ',F9.5,'  - Outdoor film coeff. [W/m^2-K]')" );
		static gio::Fmt Format_1066( "('  ibc(2)         =  ',I3,'        - Indoor BC switch')" );
		static gio::Fmt Format_1068( "('  hin            =  ',F9.5,'  - Indoor film coeff. [W/m^2-K]')" );
		static gio::Fmt Format_1070( "('  standard   =  ',I3,'        - ISO 15099 calc. standard')" );
		static gio::Fmt Format_1071( "('  standard   =  ',I3,'        - EN 673/ISO 10292 Declared calc. standard')" );
		static gio::Fmt Format_1072( "('  standard   =  ',I3,'        - EN 673/ISO 10292 Design calc. standard')" );
		static gio::Fmt Format_10731( "('  ThermalMod =  ',I3,'        - ISO15099 thermal model')" );
		static gio::Fmt Format_10732( "('  ThermalMod =  ',I3,'        - Scaled Cavity Width (SCW) thermal model')" );
		static gio::Fmt Format_10733( "('  ThermalMod =  ',I3,'        - Convective Scalar Model (CSM) thermal model')" );
		static gio::Fmt Format_10740( "('  SDScalar =  ',F7.5,'      - Factor of Venetian SD layer contribution to convection',/,/,' (only if ThermalModel = 2, otherwise ignored)')" );
		static gio::Fmt Format_1075( "('IGU parameters:')" );
		static gio::Fmt Format_1076( "('  height     =  ',F10.6,' - IGU cavity height [m]')" );
		static gio::Fmt Format_1077( "('  heightt    =  ',F10.6,' - Total window height [m]')" );
		static gio::Fmt Format_1078( "('  width      =  ',F10.6,' - Window width [m]')" );
		static gio::Fmt Format_1079( "('  tilt       =  ',F7.3,'    - Window tilt [deg]')" );
		static gio::Fmt Format_1080( "('  totsol     =  ',F10.6,' - Total solar transmittance of IGU')" );
		static gio::Fmt Format_1081( "('  nlayer     =  ',I3,'        - Number of glazing layers')" );
		static gio::Fmt Format_1089( "('IGU layers list:')" );
		static gio::Fmt Format_10802( "(' Layer',I3,' : ',I1,'              - Specular layer - Monolyhtic Glass')" );
		static gio::Fmt Format_10803( "(' Layer',I3,' : ',I1,'              - Laminated Glass')" );
		static gio::Fmt Format_10804( "(' Layer',I3,' : ',I1,'              - Venetian Blind')" );
		static gio::Fmt Format_10805( "(' Layer',I3,' : ',I1,'              - Woven Shade')" );
		static gio::Fmt Format_10806( "(' Layer',I3,' : ',I1,'              - Diffuse Shade')" );
		static gio::Fmt Format_10809( "(' Layer',I3,' : ',I1,'              - UNKNOWN TYPE!')" );
		static gio::Fmt Format_1085( "('    nslice     = ',I3,'          - Number of slices')" );
		static gio::Fmt Format_1086( "('    LaminateA  = ',F12.8,' - A coeff.')" );
		static gio::Fmt Format_1087( "('    LaminateB  = ',F12.8,' - B coeff.')" );
		static gio::Fmt Format_1088( "('    sumsol     = ',F12.8,' - Absorbed solar energy [W/m^2]')" );
		static gio::Fmt Format_1090( "('    thick   = ',F10.6,'   - Thickness [m]')" );
		static gio::Fmt Format_1091( "('    scon    = ',F10.6,'   - Thermal conductivity [W/m-K]')" );
		static gio::Fmt Format_1092( "('    asol    = ',F12.8,' - Absorbed solar energy [W/m^2]')" );
		static gio::Fmt Format_1093( "('    tir     = ',F12.8,' - IR transmittance')" );
		static gio::Fmt Format_1094( "('    emis1   = ',F10.6,'   - IR outdoor emissivity')" );
		static gio::Fmt Format_1095( "('    emis2   = ',F10.6,'   - IR indoor emissivity')" );
		static gio::Fmt Format_1100( "('    Atop    = ',F10.6,'   - Top opening area [m^2]')" );
		static gio::Fmt Format_1101( "('    Abot    = ',F10.6,'   - Bottom opening area [m^2]')" );
		static gio::Fmt Format_1102( "('    Al      = ',F10.6,'   - Left opening area [m^2]')" );
		static gio::Fmt Format_1103( "('    Ar      = ',F10.6,'   - Right opening area [m^2]')" );
		static gio::Fmt Format_1105( "('    Ah      = ',F10.6,'   - Total area of holes [m^2]')" );
		static gio::Fmt Format_11051( "('    SlatThick   = ',F10.6,'   - Slat thickness [m]')" );
		static gio::Fmt Format_11052( "('    SlatWidth   = ',F10.6,'   - Slat width [m]')" );
		static gio::Fmt Format_11053( "('    SlatAngle   = ',F10.6,'   - Slat tilt angle [deg]')" );
		static gio::Fmt Format_11054( "('    SlatCond    = ',F10.6,'   - Conductivity of the slat material [W/m.K]')" );
		static gio::Fmt Format_11055( "('    SlatSpacing = ',F10.6,'   - Distance between slats [m]')" );
		static gio::Fmt Format_11056( "('    SlatCurve   = ',F10.6,'   - Curvature radius of the slat [m]')" );
		static gio::Fmt Format_1110( "('IGU Gaps:')" );
		static gio::Fmt Format_1111( "(' Gap ',I2,':')" );
		static gio::Fmt Format_11110( "(' Outdoor space:')" );
		static gio::Fmt Format_11111( "(' Indoor space:')" );
		static gio::Fmt Format_1112( "('    gap        = ',F12.5,' - Gap width [m]')" );
		static gio::Fmt Format_1113( "('    presure    = ',F12.5,' - Gas pressure [N/m^2]')" );
		static gio::Fmt Format_1114( "('    nmix       = ',I6,'       - Num. of gasses in a gas mix')" );
		static gio::Fmt Format_1115( "('      Gas ',I1,':     ',A,'     ',F6.2,' %')" );
		static gio::Fmt Format_1120( "('    vvent      = ',F12.5,' - Forced ventilation speed [m/s]')" );
		static gio::Fmt Format_1121( "('    tvent      = ',F12.5,' - Temperature in connected gap [K]')" );
		static gio::Fmt Format_1130( "('      Gas mix coefficients - gas ',i1,', ',F6.2,' %')" );
		static gio::Fmt Format_1131( "('        gcon   = ',F11.6,', ',F11.6,', ',F11.6,' - Conductivity')" );
		static gio::Fmt Format_1132( "('        gvis   = ',F11.6,', ',F11.6,', ',F11.6,' - Dynamic viscosity')" );
		static gio::Fmt Format_1133( "('        gcp    = ',F11.6,', ',F11.6,', ',F11.6,' - Spec.heat @ const.P')" );
		static gio::Fmt Format_1134( "('        wght   = ',F11.6,'                           - Molecular weight')" );
		static gio::Fmt Format_1198( "('=====  =====  =====  =====  =====  =====  =====  =====  =====  =====  =====')" );

		//bi...Create debug file w/ Tarcog's input arguments:

		// File is not open and nothing cannot be written
		if ( InArgumentsFile == statusClosed ) return;

		date_and_time( real_CLOCK( 1 ), real_CLOCK( 2 ), real_CLOCK( 3 ), DATE_TIME );

		gio::write( InArgumentsFile, fmtLD );
		//  write(InArgumentsFile, 10001) VersionNumber, VersionCompileDateCC
		gio::write( InArgumentsFile, Format_1001 ) << DATE_TIME( 1 ) << DATE_TIME( 2 ) << DATE_TIME( 3 ) << DATE_TIME( 5 ) << DATE_TIME( 6 ) << DATE_TIME( 7 );
		gio::write( InArgumentsFile, fmtLD );

		if ( winID == -1 ) {
			gio::write( InArgumentsFile, Format_1002 ) << winID;
		} else {
			gio::write( InArgumentsFile, Format_1003 ) << winID;
		}

		if ( iguID == -1 ) {
			gio::write( InArgumentsFile, Format_1006 ) << iguID;
		} else {
			gio::write( InArgumentsFile, Format_1007 ) << iguID;
		}

		gio::write( InArgumentsFile, fmtLD ) << "    Debug dir:     " + DBGD;

		gio::write( InArgumentsFile, fmtLD );
		gio::write( InArgumentsFile, Format_1000 );
		gio::write( InArgumentsFile, fmtLD );
		gio::write( InArgumentsFile, Format_1005 );
		gio::write( InArgumentsFile, Format_1010 ) << tout << tout - KelvinConv;
		gio::write( InArgumentsFile, Format_1015 ) << tind << tind - KelvinConv;
		gio::write( InArgumentsFile, Format_1020 ) << trmin << trmin - KelvinConv;
		gio::write( InArgumentsFile, Format_1030 ) << wso;
		if ( iwd == 0 ) gio::write( InArgumentsFile, Format_1032 ); // windward
		if ( iwd == 1 ) gio::write( InArgumentsFile, Format_1033 ); // leeward
		gio::write( InArgumentsFile, Format_1035 ) << wsi;
		gio::write( InArgumentsFile, Format_1040 ) << dir;
		gio::write( InArgumentsFile, Format_1041 ) << outir;
		gio::write( InArgumentsFile, Format_1045 ) << isky;
		gio::write( InArgumentsFile, Format_1050 ) << tsky << tsky - KelvinConv;
		gio::write( InArgumentsFile, Format_1055 ) << esky;
		gio::write( InArgumentsFile, Format_1060 ) << fclr;
		gio::write( InArgumentsFile, Format_1061 ) << VacuumPressure;
		gio::write( InArgumentsFile, Format_1062 ) << VacuumMaxGapThickness;
		gio::write( InArgumentsFile, Format_1063 ) << ibc( 1 );
		gio::write( InArgumentsFile, Format_1065 ) << hout;
		gio::write( InArgumentsFile, Format_1066 ) << ibc( 2 );
		gio::write( InArgumentsFile, Format_1068 ) << hin;

		if ( standard == ISO15099 ) gio::write( InArgumentsFile, Format_1070 ) << standard;
		if ( standard == EN673 ) gio::write( InArgumentsFile, Format_1071 ) << standard;
		if ( standard == EN673Design ) gio::write( InArgumentsFile, Format_1072 ) << standard;

		if ( ThermalMod == THERM_MOD_ISO15099 ) {
			gio::write( InArgumentsFile, Format_10731 ) << ThermalMod;
			gio::write( InArgumentsFile, Format_10740 ) << SDScalar;
		}

		if ( ThermalMod == THERM_MOD_SCW ) {
			gio::write( InArgumentsFile, Format_10732 ) << ThermalMod;
			gio::write( InArgumentsFile, Format_10740 ) << SDScalar;
		}

		if ( ThermalMod == THERM_MOD_CSM ) {
			gio::write( InArgumentsFile, Format_10733 ) << ThermalMod;
			gio::write( InArgumentsFile, Format_10740 ) << SDScalar;
		}

		//    if (ThermalMod.eq.THERM_MOD_CSM)
		//        write(InArgumentsFile, 10740) SDScalar

		gio::write( InArgumentsFile, fmtLD );

		gio::write( InArgumentsFile, Format_1075 );
		gio::write( InArgumentsFile, Format_1076 ) << height;
		gio::write( InArgumentsFile, Format_1077 ) << heightt;
		gio::write( InArgumentsFile, Format_1078 ) << width;
		gio::write( InArgumentsFile, Format_1079 ) << tilt;
		gio::write( InArgumentsFile, Format_1080 ) << totsol;
		gio::write( InArgumentsFile, Format_1081 ) << nlayer;
		gio::write( InArgumentsFile, fmtLD );

		gio::write( InArgumentsFile, Format_1089 );
		for ( i = 1; i <= nlayer; ++i ) {
			{ auto const SELECT_CASE_var( LayerType( i ) );
			if ( SELECT_CASE_var == DIFFSHADE ) { // Diffuse Shade
				gio::write( InArgumentsFile, Format_10806 ) << i << LayerType( i );
			} else if ( SELECT_CASE_var == WOVSHADE ) { // Woven Shade
				gio::write( InArgumentsFile, Format_10805 ) << i << LayerType( i );
			} else if ( SELECT_CASE_var == VENETBLIND ) { // Venetian blind
				gio::write( InArgumentsFile, Format_10804 ) << i << LayerType( i );
			} else if ( SELECT_CASE_var == SPECULAR ) { // Specular layer
				if ( nslice( i ) <= 1 ) {
					gio::write( InArgumentsFile, Format_10802 ) << i << LayerType( i ); // Monolithic glass
				} else {
					gio::write( InArgumentsFile, Format_10803 ) << i << LayerType( i ); // Laminated layer
				}
			} else {
				gio::write( InArgumentsFile, Format_10809 ) << i << LayerType( i );
			}}

			gio::write( InArgumentsFile, Format_1090 ) << thick( i );
			gio::write( InArgumentsFile, Format_1091 ) << scon( i );
			gio::write( InArgumentsFile, Format_1092 ) << asol( i );
			gio::write( InArgumentsFile, Format_1093 ) << tir( 2 * i - 1 );
			gio::write( InArgumentsFile, Format_1094 ) << emis( 2 * i - 1 );
			gio::write( InArgumentsFile, Format_1095 ) << emis( 2 * i );

			if ( LayerType( i ) == VENETBLIND ) { // SD layer
				gio::write( InArgumentsFile, Format_1100 ) << Atop( i );
				gio::write( InArgumentsFile, Format_1101 ) << Abot( i );
				gio::write( InArgumentsFile, Format_1102 ) << Al( i );
				gio::write( InArgumentsFile, Format_1103 ) << Ar( i );
				gio::write( InArgumentsFile, Format_1105 ) << Ah( i );

				gio::write( InArgumentsFile, Format_11051 ) << SlatThick( i );
				gio::write( InArgumentsFile, Format_11052 ) << SlatWidth( i );
				gio::write( InArgumentsFile, Format_11053 ) << SlatAngle( i );
				gio::write( InArgumentsFile, Format_11054 ) << SlatCond( i );
				gio::write( InArgumentsFile, Format_11055 ) << SlatSpacing( i );
				gio::write( InArgumentsFile, Format_11056 ) << SlatCurve( i );

				//bi...Input arguments correction patch:

				//     if (ApplyVenetianPatch.eq..TRUE.) then
				//      SlatThick(i) = Thick(i)
				//      SlatWidth(i) = SlatWidth(i) / 1000.0d0
				//      SlatCurve(i) = SlatCurve(i) / 1000.0d0
				//      SlatSpacing(i) = SlatSpacing(i) / 1000.0d0
				//      write(InArgumentsFile, *) 'After applying the patch:'
				//        write(InArgumentsFile, 11051) SlatThick(i)
				//        write(InArgumentsFile, 11052) SlatWidth(i)
				//        write(InArgumentsFile, 11053) SlatAngle(i)
				//        write(InArgumentsFile, 11054) SlatCond(i)
				//        write(InArgumentsFile, 11055) SlatSpacing(i)
				//        write(InArgumentsFile, 11056) SlatCurve(i)
				//     end if

				//bi...end Input arguments correction patch

			}

			if ( nslice( i ) > 1 ) { // SD layer
				gio::write( InArgumentsFile, Format_1085 ) << nslice( i );
				gio::write( InArgumentsFile, Format_1085 ) << LaminateA( i );
				gio::write( InArgumentsFile, Format_1085 ) << LaminateB( i );
				gio::write( InArgumentsFile, Format_1085 ) << sumsol( i );
			}
		} // i - layers

		gio::write( InArgumentsFile, fmtLD );

		gio::write( InArgumentsFile, Format_1110 );

		for ( i = 1; i <= nlayer + 1; ++i ) { // loop through gaps:
			if ( ( i > 1 ) && ( i <= nlayer ) ) gio::write( InArgumentsFile, Format_1111 ) << i - 1;
			if ( i == 1 ) gio::write( InArgumentsFile, Format_11110 );
			if ( i == nlayer + 1 ) gio::write( InArgumentsFile, Format_11111 );
			if ( ( i > 1 ) && ( i <= nlayer ) ) gio::write( InArgumentsFile, Format_1112 ) << gap( i - 1 );
			gio::write( InArgumentsFile, Format_1113 ) << presure( i );
			if ( ( i > 1 ) && ( i <= nlayer ) ) {
				gio::write( InArgumentsFile, Format_1120 ) << vvent( i );
			}
			if ( ( i > 1 ) && ( i <= nlayer ) ) {
				gio::write( InArgumentsFile, Format_1121 ) << tvent( i );
			}
			gio::write( InArgumentsFile, Format_1114 ) << nmix( i );

			//if (mgas.eq.1) then ! call gasses by names:
			//  do  j = 1, nmix(i)
			//    if (iprop(i, j).eq.1) write(InArgumentsFile, 1115) iprop(i, j), 'Air,     ', 100*frct(i, j) ! Air
			//    if (iprop(i, j).eq.2) write(InArgumentsFile, 1115) iprop(i, j), 'Argon,   ', 100*frct(i, j) ! Argon
			//    if (iprop(i, j).eq.3) write(InArgumentsFile, 1115) iprop(i, j), 'Krypton, ', 100*frct(i, j) ! Krypton
			//    if (iprop(i, j).eq.4) write(InArgumentsFile, 1115) iprop(i, j), 'Xenon,   ', 100*frct(i, j) ! Xenon
			//  end do  ! j - mix loop
			//end if

			//if (mgas.eq.0) then ! show received gass properties:
			for ( j = 1; j <= nmix( i ); ++j ) {
				//if (iprop(i, j).eq.1) write(InArgumentsFile, 1115) iprop(i, j), ' ' 100*frct(i, j) ! Air
				gio::write( InArgumentsFile, Format_1115 ) << iprop( j, i ) << ' ' << 100 * frct( j, i ); // gas
				//if (iprop(i, j).eq.2) write(InArgumentsFile, 1116) iprop(i, j), 100*frct(i, j) ! Argon
				//if (iprop(i, j).eq.3) write(InArgumentsFile, 1117) iprop(i, j), 100*frct(i, j) ! Krypton
				//if (iprop(i, j).eq.4) write(InArgumentsFile, 1118) iprop(i, j), 100*frct(i, j) ! Xenon
				gio::write( InArgumentsFile, Format_1130 ) << iprop( j, i ) << 100 * frct( j, i );
				gio::write( InArgumentsFile, Format_1131 ) << xgcon( 1, iprop( j, i ) ) << xgcon( 2, iprop( j, i ) ) << xgcon( 3, iprop( j, i ) );
				gio::write( InArgumentsFile, Format_1132 ) << xgvis( 1, iprop( j, i ) ) << xgvis( 2, iprop( j, i ) ) << xgvis( 3, iprop( j, i ) );
				gio::write( InArgumentsFile, Format_1133 ) << xgcp( 1, iprop( j, i ) ) << xgcp( 2, iprop( j, i ) ) << xgcp( 3, iprop( j, i ) );
				gio::write( InArgumentsFile, Format_1134 ) << xwght( iprop( j, i ) );
			} // - j - one mix
			//end if  ! MGAS = 1 - "table" gasses
		} // i - gas loop

		gio::write( InArgumentsFile, fmtLD );
		gio::write( InArgumentsFile, Format_1198 );

		//close(InArgumentsFile)

		//!!!!!!!!!!!!!!!!!!
		//!!
		//!! Formats:
		//!!
		//!!!!!!!!!!!!!!!!!!

		//1000  format('TARCOG input arguments list - ',I4,'-',I2.2,'-',I2.2, ', ', I2.2,':',I2.2,':',I2.2)

		//1010  format('  Tout       =  ', F10.6,  ' - Outdoor temperature [K]')
		//1015  format('  Tin        =  ', F10.6,  ' - Indoor temperature [K]')

		//1016  format('  Trmout     =  ', F10.6,  ' - Outdoor mean radiant temperature [K]')

		//1020  format('  Trmin      =  ', F10.6,  ' - Indoor mean radiant temperature [K]')
		//1050  format('  tsky       =  ', F10.6,  ' - Night sky temperature [K]')

		//1115  format('      Gas ', I1, ':     Air,     ', F6.2,' %')
		//1116  format('      Gas ', I1, ':     Argon,   ', F6.2,' %')
		//1117  format('      Gas ', I1, ':     Krypron, ', F6.2,' %')
		//1118  format('      Gas ', I1, ':     Xenon,   ', F6.2,' %')

		//1199  format('-----  *****  -----  *****  -----  *****  -----  *****  -----  *****  -----')

	}

	void
	WriteModifiedArguments(
		int const InArgumentsFile,
		std::string const & EP_UNUSED( DBGD ),
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
	)
	{

		// Using/Aliasing
		using DataGlobals::KelvinConv;

		// Argument array dimensioning
		LayerType.dim( maxlay );
		nmix.dim( maxlay1 );
		frct.dim( maxgas, maxlay1 );
		thick.dim( maxlay );
		scon.dim( maxlay );
		gap.dim( MaxGap );
		xgcon.dim( 3, maxgas );
		xgvis.dim( 3, maxgas );
		xgcp.dim( 3, maxgas );
		xwght.dim( maxgas );

		// Locals
		int i;
		int j;

		// Formats
		static gio::Fmt Format_1014( "('Adjusted input arguments:')" );
		static gio::Fmt Format_1013( "(' Gass coefficients:')" );
		static gio::Fmt Format_1016( "('  Trmout     =  ',F10.6,' K ( ',F7.3,' deg C) - Outdoor mean radiant temp.')" );
		static gio::Fmt Format_1017( "('  Gout       =  ',F10.6,' ')" );
		static gio::Fmt Format_1018( "('  Gin        =  ',F10.6,' ')" );
		static gio::Fmt Format_1019( "('  Ebsky      =  ',F10.6,' ')" );
		static gio::Fmt Format_10191( "('  Ebroom     =  ',F10.6,' ')" );
		static gio::Fmt Format_1020( "('  Trmin      =  ',F10.6,' K ( ',F7.3,' deg C) - Indoor mean radiant temp.')" );
		static gio::Fmt Format_1055( "('  esky       =  ',F7.3,'    - Effective night sky emmitance')" );
		static gio::Fmt Format_1084( "(' Layer',I3,' : ',I1,'              - Venetian Blind')" );
		static gio::Fmt Format_1090( "('    thick   = ',F10.6,'   - Thickness [m]')" );
		static gio::Fmt Format_1091( "('    scon    = ',F10.6,'   - Thermal conductivity [W/m-K]')" );
		static gio::Fmt Format_1130( "('      Gas mix coefficients - gas ',i1,', ',F6.2,' %')" );
		static gio::Fmt Format_1131( "('        gcon   = ',F11.6,', ',F11.6,', ',F11.6,' - Conductivity')" );
		static gio::Fmt Format_1132( "('        gvis   = ',F11.6,', ',F11.6,', ',F11.6,' - Dynamic viscosity')" );
		static gio::Fmt Format_1133( "('        gcp    = ',F11.6,', ',F11.6,', ',F11.6,' - Spec.heat @ const.P')" );
		static gio::Fmt Format_1134( "('        wght   = ',F11.6,'                           - Molecular weight')" );
		static gio::Fmt Format_1110( "('IGU Gaps:')" );
		static gio::Fmt Format_1111( "(' Gap ',I2,':')" );
		static gio::Fmt Format_1112( "(' Gap width: ',F11.8)" );
		static gio::Fmt Format_11110( "(' Outdoor space:')" );
		static gio::Fmt Format_11111( "(' Indoor space:')" );
		static gio::Fmt Format_1198( "('=====  =====  =====  =====  =====  =====  =====  =====  =====  =====  =====')" );

		//open(unit=InArgumentsFile,  file=TRIM(DBGD)//DebugOutputFileName,  status='unknown', access=FileMode, &
		//        position=FilePosition, form='formatted', iostat=nperr)
		//if (nperr.ne.0)  open(unit=InArgumentsFile,  file=DebugOutputFileName,  status='unknown', access=FileMode, &
		//        position=FilePosition, form='formatted', iostat=nperr)
		gio::write( InArgumentsFile, fmtLD );
		gio::write( InArgumentsFile, Format_1014 );
		gio::write( InArgumentsFile, fmtLD );
		gio::write( InArgumentsFile, Format_1055 ) << esky;
		gio::write( InArgumentsFile, Format_1016 ) << trmout << trmout - KelvinConv;
		gio::write( InArgumentsFile, Format_1020 ) << trmin << trmin - KelvinConv;
		gio::write( InArgumentsFile, Format_1019 ) << ebsky;
		gio::write( InArgumentsFile, Format_10191 ) << ebroom;
		gio::write( InArgumentsFile, Format_1017 ) << Gout;
		gio::write( InArgumentsFile, Format_1018 ) << Gin;
		gio::write( InArgumentsFile, fmtLD );

		for ( i = 1; i <= nlayer; ++i ) {
			if ( LayerType( i ) == VENETBLIND ) { // SD layer
				gio::write( InArgumentsFile, Format_1084 ) << i << LayerType( i );
				gio::write( InArgumentsFile, Format_1090 ) << thick( i );
				gio::write( InArgumentsFile, Format_1091 ) << scon( i );
			}
		}
		gio::write( InArgumentsFile, fmtLD );

		gio::write( InArgumentsFile, Format_1013 );
		for ( i = 1; i <= nlayer + 1; ++i ) { // loop through gaps:
			if ( ( i > 1 ) && ( i <= nlayer ) ) gio::write( InArgumentsFile, Format_1111 ) << i - 1;
			if ( ( i > 1 ) && ( i <= nlayer ) ) gio::write( InArgumentsFile, Format_1112 ) << gap( i - 1 );
			if ( i == 1 ) gio::write( InArgumentsFile, Format_11110 );
			if ( i == nlayer + 1 ) gio::write( InArgumentsFile, Format_11111 );
			//    write(InArgumentsFile, 1111) i-1
			for ( j = 1; j <= nmix( i ); ++j ) {
				gio::write( InArgumentsFile, Format_1130 ) << j << 100 * frct( j, i );
				gio::write( InArgumentsFile, Format_1131 ) << xgcon( 1, j ) << xgcon( 2, j ) << xgcon( 3, j );
				gio::write( InArgumentsFile, Format_1132 ) << xgvis( 1, j ) << xgvis( 2, j ) << xgvis( 3, j );
				gio::write( InArgumentsFile, Format_1133 ) << xgcp( 1, j ) << xgcp( 2, j ) << xgcp( 3, j );
				gio::write( InArgumentsFile, Format_1134 ) << xwght( j );
			} // j - gas mix
		} // i - gaps
		gio::write( InArgumentsFile, fmtLD );
		gio::write( InArgumentsFile, Format_1198 );
		//close(InArgumentsFile)

		//!!!!!!!!!!!!!!!!!!
		//!!
		//!! Formats:
		//!!
		//!!!!!!!!!!!!!!!!!!

	}

	void
	WriteOutputArguments(
		int & OutArgumentsFile,
		std::string const & EP_UNUSED( DBGD ),
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
	)
	{

		// Using/Aliasing
		using DataGlobals::KelvinConv;

		// Argument array dimensioning
		q.dim( maxlay3 );
		qv.dim( maxlay1 );
		qcgas.dim( maxlay1 );
		qrgas.dim( maxlay1 );
		theta.dim( maxlay2 );
		vfreevent.dim( maxlay1 );
		vvent.dim( maxlay1 );
		Keff.dim( maxlay );
		ShadeGapKeffConv.dim( MaxGap );
		Ra.dim( maxlay );
		Nu.dim( maxlay );
		LayerType.dim( maxlay );
		Ebf.dim( maxlay );
		Ebb.dim( maxlay );
		Rf.dim( maxlay );
		Rb.dim( maxlay );
		hcgas.dim( maxlay );
		hrgas.dim( maxlay );

		// Locals
		Array1D_int DATE_TIME( 8 );
		Array1D_string real_CLOCK( 3 );

		int i;

		// Formats
		static gio::Fmt Format_2000( "('TARCOG calculation results - ',I4,'-',I2.2,'-',I2.2,', ',I2.2,':',I2.2,':',I2.2)" );
		static gio::Fmt Format_2120( "('  Ufactor  = ',F12.6)" );
		static gio::Fmt Format_2130( "('  SHGC     = ',F12.6)" );
		static gio::Fmt Format_2131( "('  SHGC_OLD = ',F12.6)" );
		static gio::Fmt Format_2132( "('  SC       = ',F12.6)" );
		static gio::Fmt Format_2140( "('  hcin  = ',F10.6,3x,'hrin  = ',F10.6,3x,'hin  = ',F10.6)" );
		static gio::Fmt Format_2150( "('  hcout = ',F10.6,3x,'hrout = ',F10.6,3x,'hout = ',F10.6)" );
		static gio::Fmt Format_2155( "('  Ra(',I1,') =',F15.6,'        Nu(',I1,') =',F12.6)" );
		static gio::Fmt Format_2160( "('  hcgas(',I1,') =',F15.6,'      hrgas(',I1,') =',F24.6)" );
		static gio::Fmt Format_2165( "('  rhum  =',F15.6,'        rhout =',F12.6)" );
		static gio::Fmt Format_2170( "('  hflux    = ',F12.6)" );
		static gio::Fmt Format_2105( "('                                            Tamb =',F11.6,' K ( ',F7.3,' deg C)')" );
		static gio::Fmt Format_2110( "('  ----------------- ------------------   Theta',I2,' =',F11.6,' K ( ',F7.3,' deg C)')" );
		static gio::Fmt Format_2111( "('  \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\   Theta',I2,' =',F11.6,' K ( ',F7.3,' deg C)')" );
		static gio::Fmt Format_2112( "('  +++++++++++++++++ ++++++++++++++++++   Theta',I2,' =',F11.6,' K ( ',F7.3,' deg C)')" );
		static gio::Fmt Format_2113( "('  ooooooooooooooooo oooooooooooooooooo   Theta',I2,' =',F11.6,' K ( ',F7.3,' deg C)')" );
		static gio::Fmt Format_2115( "('                                           Troom =',F11.6,' K ( ',F7.3,' deg C)')" );
		static gio::Fmt Format_2180( "('           qout =',F12.5)" );
		static gio::Fmt Format_2190( "('  |     qpane',i2,' =',F12.5,'        |')" );
		static gio::Fmt Format_2195( "('  |     qpane',i2,' =',F12.5,'        |         keffc',i2,' =',F11.6)" );
		static gio::Fmt Format_2199( "('  |      qlayer',i2,' =',F12.5,'       |')" );
		static gio::Fmt Format_2210( "('            qin =',F11.6)" );
		static gio::Fmt Format_2300( "('            q',i2,' =',F12.5)" );
		static gio::Fmt Format_2310( "('        qprim',i2,' =',F12.5)" );
		static gio::Fmt Format_2320( "('           qv',i2,' =',F12.5)" );
		static gio::Fmt Format_2321( "('       airspd',i2,' =',F12.5,'    keff',i2,' =',F12.5)" );
		static gio::Fmt Format_2322( "('           qc',i2,' =',F12.5,'      qr',i2,' =',F12.5)" );
		static gio::Fmt Format_2330( "('  ShadeEmisRatioIn  =',F11.6,'        ShadeEmisRatioOut =',F11.6)" );
		static gio::Fmt Format_2331( "('  ShadeHcRatioIn    =',F11.6,'        ShadeHcRatioOut   =',F11.6)" );
		static gio::Fmt Format_2332( "('  HcUnshadedIn      =',F11.6,'        HcUnshadedOut     =',F11.6)" );
		static gio::Fmt Format_2340( "('  ')" );
		static gio::Fmt Format_2350( "('Heat Flux Flow and Temperatures of Layer Surfaces:')" );
		static gio::Fmt Format_2351( "('Basic IGU properties:')" );
		static gio::Fmt Format_2220( "('  he = ',F8.4,',',3x,'hi = ',F8.4)" );
		static gio::Fmt Format_2230( "('  hg',I2,' =',E15.6,'      hr',I2,' =',E15.6,'      hs',I2,' =',E15.6)" );
		static gio::Fmt Format_3333( "('Flux (non-solar pass): ',F12.6,' ; Flux per W7: ',F12.6)" );
		static gio::Fmt Format_4205( "('  Ebsky =',F11.6,' [W/m2], Gout =',F11.6,' [W/m2]')" );
		static gio::Fmt Format_4215( "('  Ebroom =',F11.6,' [W/m2], Gin  =',F11.6,' [W/m2]')" );
		static gio::Fmt Format_4110( "('  Ef',I2,' =',F11.6,' [W/m2], Rf',I2,' =',F11.6,' [W/m2]')" );
		static gio::Fmt Format_4111( "('  ----------------- ------------------')" );
		static gio::Fmt Format_4112( "('  Ef',I2,' =',F11.6,' [W/m2], Rf',I2,' =',F11.6,' [W/m2]')" );
		static gio::Fmt Format_4113( "('  \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\')" );
		static gio::Fmt Format_4114( "('  Ef',I2,' =',F11.6,' [W/m2], Rf',I2,' =',F11.6,' [W/m2]')" );
		static gio::Fmt Format_4115( "('  +++++++++++++++++ ++++++++++++++++++')" );
		static gio::Fmt Format_4116( "('  Ef',I2,' =',F11.6,' [W/m2], Rf',I2,' =',F11.6,' [W/m2]')" );
		static gio::Fmt Format_4117( "('  ooooooooooooooooo oooooooooooooooooo')" );
		static gio::Fmt Format_4120( "('  Eb',I2,' =',F11.6,' [W/m2], Rb',I2,' =',F11.6,' [W/m2]')" );
		static gio::Fmt Format_4121( "('  ----------------- ------------------')" );
		static gio::Fmt Format_4122( "('  Eb',I2,' =',F11.6,' [W/m2], Rb',I2,' =',F11.6,' [W/m2]')" );
		static gio::Fmt Format_4123( "('  \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\')" );
		static gio::Fmt Format_4124( "('  Eb',I2,' =',F11.6,' [W/m2], Rb',I2,' =',F11.6,' [W/m2]')" );
		static gio::Fmt Format_4125( "('  +++++++++++++++++ ++++++++++++++++++')" );
		static gio::Fmt Format_4126( "('  Eb',I2,' =',F11.6,' [W/m2], Rb',I2,' =',F11.6,' [W/m2]')" );
		static gio::Fmt Format_4127( "('  ooooooooooooooooo oooooooooooooooooo')" );
		static gio::Fmt Format_4190( "('  |                     |')" );
		static gio::Fmt Format_4350( "('Energy balances on Layer Surfaces:')" );

		//open(unit=OutArgumentsFile,  file=TRIM(DBGD)//DebugOutputFileName,  status='unknown', access=FileMode, &
		//      position=FilePosition, form='formatted', iostat=nperr)
		//if (nperr.ne.0)  open(unit=OutArgumentsFile,  file=DebugOutputFileName,  status='unknown', access=FileMode, &
		//      position=FilePosition, form='formatted', iostat=nperr)
		date_and_time( real_CLOCK( 1 ), real_CLOCK( 2 ), real_CLOCK( 3 ), DATE_TIME );
		gio::write( OutArgumentsFile, fmtLD );
		gio::write( OutArgumentsFile, Format_2000 ) << DATE_TIME( 1 ) << DATE_TIME( 2 ) << DATE_TIME( 3 ) << DATE_TIME( 5 ) << DATE_TIME( 6 ) << DATE_TIME( 7 );
		gio::write( OutArgumentsFile, fmtLD );
		gio::write( OutArgumentsFile, Format_2350 );
		gio::write( OutArgumentsFile, fmtLD );
		gio::write( OutArgumentsFile, Format_2105 ) << tamb << tamb - KelvinConv;
		gio::write( OutArgumentsFile, Format_2180 ) << q( 1 );

		//bi  Write out layer properties:
		for ( i = 1; i <= nlayer; ++i ) {
			//        write(OutArgumentsFile, 2110) 2*i-1, theta(2*i-1), theta(2*i-1)-273.15d0
			{ auto const SELECT_CASE_var( LayerType( i ) );
			if ( SELECT_CASE_var == SPECULAR ) { // Specular layer
				gio::write( OutArgumentsFile, Format_2110 ) << 2 * i - 1 << theta( 2 * i - 1 ) << theta( 2 * i - 1 ) - KelvinConv;
				gio::write( OutArgumentsFile, Format_2190 ) << i << q( 2 * i );
				gio::write( OutArgumentsFile, Format_2110 ) << 2 * i << theta( 2 * i ) << theta( 2 * i ) - KelvinConv;
			} else if ( SELECT_CASE_var == VENETBLIND ) { // Venetian blind
				gio::write( OutArgumentsFile, Format_2111 ) << 2 * i - 1 << theta( 2 * i - 1 ) << theta( 2 * i - 1 ) - KelvinConv;
				gio::write( OutArgumentsFile, Format_2195 ) << i << q( 2 * i ) << i << ShadeGapKeffConv( i );
				gio::write( OutArgumentsFile, Format_2111 ) << 2 * i << theta( 2 * i ) << theta( 2 * i ) - KelvinConv;
			} else if ( SELECT_CASE_var == WOVSHADE ) { // Venetian blind
				gio::write( OutArgumentsFile, Format_2112 ) << 2 * i - 1 << theta( 2 * i - 1 ) << theta( 2 * i - 1 ) - KelvinConv;
				gio::write( OutArgumentsFile, Format_2195 ) << i << q( 2 * i ) << i << ShadeGapKeffConv( i );
				gio::write( OutArgumentsFile, Format_2112 ) << 2 * i << theta( 2 * i ) << theta( 2 * i ) - KelvinConv;
			} else if ( SELECT_CASE_var == DIFFSHADE ) { // Venetian blind
				gio::write( OutArgumentsFile, Format_2110 ) << 2 * i - 1 << theta( 2 * i - 1 ) << theta( 2 * i - 1 ) - KelvinConv;
				gio::write( OutArgumentsFile, Format_2190 ) << i << q( 2 * i );
				gio::write( OutArgumentsFile, Format_2110 ) << 2 * i << theta( 2 * i ) << theta( 2 * i ) - KelvinConv;
			} else {
				gio::write( OutArgumentsFile, Format_2110 ) << 2 * i - 1 << theta( 2 * i - 1 ) << theta( 2 * i - 1 ) - KelvinConv;
				gio::write( OutArgumentsFile, Format_2199 ) << i << q( 2 * i );
				gio::write( OutArgumentsFile, Format_2110 ) << 2 * i << theta( 2 * i ) << theta( 2 * i ) - KelvinConv;
			}}

			//    write(OutArgumentsFile, 2110) 2*i, theta(2*i), theta(2*i)-273.15d0

			//bi  Write out gap properties:
			if ( i != nlayer ) {
				gio::write( OutArgumentsFile, Format_2300 ) << i << q( 2 * i + 1 );
				gio::write( OutArgumentsFile, Format_2320 ) << i << qv( i + 1 );
				if ( vvent( i + 1 ) == 0 ) {
					gio::write( OutArgumentsFile, Format_2321 ) << i << vfreevent( i + 1 ) << i << Keff( i );
				} else {
					if ( i > 1 ) {
						gio::write( OutArgumentsFile, Format_2321 ) << i << vvent( i + 1 ) << i << Keff( i - 1 ); //Autodesk:BoundsViolation Keff(i-1) @ i=1: Fixed in 8.2 by surrounding if block
					}
				}
				gio::write( OutArgumentsFile, Format_2322 ) << i << qcgas( i + 1 ) << i << qrgas( i + 1 );
				//      write(OutArgumentsFile, 2323) i, Keff(i)
				//write(OutArgumentsFile, 2310) i, qprim(2*i + 1)
			} else {
				gio::write( OutArgumentsFile, Format_2210 ) << q( 2 * i + 1 );
			}
		} // i - layers

		gio::write( OutArgumentsFile, Format_2115 ) << troom << troom - KelvinConv;

		gio::write( OutArgumentsFile, fmtLD );

		//Simon: Write energy balances on layer surfaces
		gio::write( OutArgumentsFile, Format_4350 );
		gio::write( OutArgumentsFile, fmtLD );
		gio::write( OutArgumentsFile, Format_4205 ) << ebsky << Gout;
		gio::write( OutArgumentsFile, fmtLD );

		for ( i = 1; i <= nlayer; ++i ) {
			{ auto const SELECT_CASE_var( LayerType( i ) );
			if ( SELECT_CASE_var == SPECULAR ) { // Specular layer
				gio::write( OutArgumentsFile, Format_4110 ) << i << Ebf( i ) << i << Rf( i );
				gio::write( OutArgumentsFile, Format_4111 );
				gio::write( OutArgumentsFile, Format_4190 );
				gio::write( OutArgumentsFile, Format_4121 );
				gio::write( OutArgumentsFile, Format_4120 ) << i << Ebb( i ) << i << Rb( i );
			} else if ( SELECT_CASE_var == VENETBLIND ) { // Venetian blind
				gio::write( OutArgumentsFile, Format_4112 ) << i << Ebf( i ) << i << Rf( i );
				gio::write( OutArgumentsFile, Format_4113 );
				gio::write( OutArgumentsFile, Format_4190 );
				gio::write( OutArgumentsFile, Format_4123 );
				gio::write( OutArgumentsFile, Format_4122 ) << i << Ebb( i ) << i << Rb( i );
			} else if ( SELECT_CASE_var == WOVSHADE ) { // Venetian blind
				gio::write( OutArgumentsFile, Format_4114 ) << i << Ebf( i ) << i << Rf( i );
				gio::write( OutArgumentsFile, Format_4115 );
				gio::write( OutArgumentsFile, Format_4190 );
				gio::write( OutArgumentsFile, Format_4125 );
				gio::write( OutArgumentsFile, Format_4124 ) << i << Ebb( i ) << i << Rb( i );
			} else if ( SELECT_CASE_var == DIFFSHADE ) {
				gio::write( OutArgumentsFile, Format_4116 ) << i << Ebf( i ) << i << Rf( i );
				gio::write( OutArgumentsFile, Format_4117 );
				gio::write( OutArgumentsFile, Format_4190 );
				gio::write( OutArgumentsFile, Format_4127 );
				gio::write( OutArgumentsFile, Format_4126 ) << i << Ebb( i ) << i << Rb( i );
			} else {
				gio::write( OutArgumentsFile, Format_4110 ) << i << Ebf( i ) << i << Rf( i );
				gio::write( OutArgumentsFile, Format_4111 );
				gio::write( OutArgumentsFile, Format_4190 );
				gio::write( OutArgumentsFile, Format_4121 );
				gio::write( OutArgumentsFile, Format_4120 ) << i << Ebb( i ) << i << Rb( i );
			}}
			gio::write( OutArgumentsFile, fmtLD );
		}

		gio::write( OutArgumentsFile, Format_4215 ) << ebroom << Gin;

		gio::write( OutArgumentsFile, fmtLD );
		gio::write( OutArgumentsFile, Format_2351 );
		gio::write( OutArgumentsFile, fmtLD );
		gio::write( OutArgumentsFile, Format_2120 ) << ufactor;
		gio::write( OutArgumentsFile, Format_2130 ) << shgc;
		gio::write( OutArgumentsFile, fmtLD );
		gio::write( OutArgumentsFile, Format_2132 ) << sc;
		gio::write( OutArgumentsFile, Format_2170 ) << hflux;
		gio::write( OutArgumentsFile, fmtLD );
		gio::write( OutArgumentsFile, Format_2131 ) << shgct;
		gio::write( OutArgumentsFile, fmtLD );
		gio::write( OutArgumentsFile, Format_2140 ) << hcin << hrin << hcin + hrin;
		gio::write( OutArgumentsFile, Format_2150 ) << hcout << hrout << hcout + hrout;

		gio::write( OutArgumentsFile, fmtLD );
		for ( i = 1; i <= nlayer - 1; ++i ) {
			gio::write( OutArgumentsFile, Format_2155 ) << i << Ra( i ) << i << Nu( i );
		}
		gio::write( OutArgumentsFile, fmtLD );
		gio::write( OutArgumentsFile, Format_2330 ) << ShadeEmisRatioIn << ShadeEmisRatioOut;
		gio::write( OutArgumentsFile, Format_2331 ) << ShadeHcRatioIn << ShadeHcRatioOut;
		gio::write( OutArgumentsFile, Format_2332 ) << HcUnshadedIn << HcUnshadedOut;

		gio::write( OutArgumentsFile, fmtLD );
		for ( i = 2; i <= nlayer; ++i ) {
			gio::write( OutArgumentsFile, Format_2160 ) << i << hcgas( i ) << i << hrgas( i );
		}

		gio::write( OutArgumentsFile, fmtLD );
		gio::write( OutArgumentsFile, "('  Error Tolerance = ', e12.6)" ) << AchievedErrorTolerance;

		gio::write( OutArgumentsFile, fmtLD );
		gio::write( OutArgumentsFile, "('  Number of Iterations = ', i6)" ) << NumOfIter;

		//  write(OutArgumentsFile, *)
		//  write(OutArgumentsFile, 3333) flux_nonsolar, qeff

		//close(OutArgumentsFile)

		//!!!!!!!!!!!!!!!!!!
		//!!
		//!! Formats:
		//!!
		//!!!!!!!!!!!!!!!!!!

		//2101  format(' SHGC =   ',F8.6,6x,' SHGC_OLD = ',F8.6,2x,' SC = ',F8.6)
		//2110  format(' Theta(',I3,') = ',F12.6)
		//2111  format(/'Pane #:', I3/)
		//2112  format('Number of panes: ',I2)
		//2113  format('    Thetaslice(',I3,',',I3') = ',F12.6)

		//2105  format('                                            Tamb =',F11.6)
		//2110  format('  ----------------- ------------------   Theta',I2,' =',F11.6)
		//2115  format('                                           Troom =',F11.6)

		//2190  format('  |       qpane', i2,' =',  F11.6,'       |         keffc', i2,' =',  F11.6)
		//2195  format('  |////   qpane', i2,' =',  F11.6,'   ////|')

		//2323  format('         keff', i2,' =',  F12.5)

	}

	void
	WriteOutputEN673(
		int & OutArgumentsFile,
		std::string const & EP_UNUSED( DBGD ),
		int const nlayer,
		Real64 const ufactor,
		Real64 const hout,
		Real64 const hin,
		Array1A< Real64 > const Ra,
		Array1A< Real64 > const Nu,
		Array1A< Real64 > const hg,
		Array1A< Real64 > const hr,
		Array1A< Real64 > const hs,
		int & EP_UNUSED( nperr )
	)
	{

		// Argument array dimensioning
		Ra.dim( maxlay );
		Nu.dim( maxlay );
		hg.dim( maxlay );
		hr.dim( maxlay );
		hs.dim( maxlay );

		// Locals
		//character(len=*), intent (inout) :: ErrorMessage

		Array1D_int DATE_TIME( 8 );
		Array1D_string real_CLOCK( 3 );

		int i;

		// Formats
		static gio::Fmt Format_2000( "('TARCOG calculation results - ',I4,'-',I2.2,'-',I2.2,', ',I2.2,':',I2.2,':',I2.2)" );
		static gio::Fmt Format_2351( "('Basic IGU properties:')" );
		static gio::Fmt Format_2120( "('  Ufactor  = ',F12.6)" );
		static gio::Fmt Format_2220( "('  he = ',F8.4,',',3x,'hi = ',F8.4)" );
		static gio::Fmt Format_2155( "('  Ra(',I1,') =',F15.6,'        Nu(',I1,') =',F12.6)" );
		static gio::Fmt Format_2230( "('  hg',I2,' =',E15.6,'      hr',I2,' =',E15.6,'      hs',I2,' =',E15.6)" );

		//open(unit=OutArgumentsFile,  file=TRIM(DBGD)//DebugOutputFileName,  status='unknown', access=FileMode,  &
		//      position=FilePosition, form='formatted', iostat=nperr)
		//if (nperr.ne.0)  open(unit=OutArgumentsFile,  file=DebugOutputFileName,  status='unknown', access=FileMode,  &
		//      position=FilePosition, form='formatted', iostat=nperr)
		date_and_time( real_CLOCK( 1 ), real_CLOCK( 2 ), real_CLOCK( 3 ), DATE_TIME );
		gio::write( OutArgumentsFile, fmtLD );
		gio::write( OutArgumentsFile, Format_2000 ) << DATE_TIME( 1 ) << DATE_TIME( 2 ) << DATE_TIME( 3 ) << DATE_TIME( 5 ) << DATE_TIME( 6 ) << DATE_TIME( 7 );
		gio::write( OutArgumentsFile, fmtLD );

		gio::write( OutArgumentsFile, fmtLD );
		gio::write( OutArgumentsFile, Format_2351 );
		gio::write( OutArgumentsFile, fmtLD );
		gio::write( OutArgumentsFile, Format_2120 ) << ufactor;
		gio::write( OutArgumentsFile, fmtLD );
		gio::write( OutArgumentsFile, Format_2220 ) << hout << hin;
		gio::write( OutArgumentsFile, fmtLD );
		for ( i = 1; i <= nlayer - 1; ++i ) {
			gio::write( OutArgumentsFile, Format_2155 ) << i << Ra( i ) << i << Nu( i );
		}
		gio::write( OutArgumentsFile, fmtLD );
		for ( i = 1; i <= nlayer - 1; ++i ) {
			gio::write( OutArgumentsFile, Format_2230 ) << i << hg( i ) << i << hr( i ) << i << hs( i );
		}
		//close(OutArgumentsFile)

		//!!!!!!!!!!!!!!!!!!
		//!!
		//!! Formats:
		//!!
		//!!!!!!!!!!!!!!!!!!

	}

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
	)
	{

		// Using/Aliasing
		using namespace TARCOGGassesParams;

		// Argument array dimensioning
		ibc.dim( 2 );
		LayerType.dim( maxlay );
		thick.dim( maxlay );
		scon.dim( maxlay );
		YoungsMod.dim( maxlay );
		PoissonsRat.dim( maxlay );
		asol.dim( maxlay );
		tir.dim( maxlay2 );
		emis.dim( maxlay2 );
		Atop.dim( maxlay );
		Abot.dim( maxlay );
		Al.dim( maxlay );
		Ar.dim( maxlay );
		Ah.dim( maxlay );
		SupportPillar.dim( maxlay );
		PillarSpacing.dim( maxlay );
		PillarRadius.dim( maxlay );
		SlatThick.dim( maxlay );
		SlatWidth.dim( maxlay );
		SlatAngle.dim( maxlay );
		SlatCond.dim( maxlay );
		SlatSpacing.dim( maxlay );
		SlatCurve.dim( maxlay );
		nslice.dim( maxlay );
		gap.dim( maxlay );
		GapDef.dim( MaxGap );
		vvent.dim( maxlay1 );
		tvent.dim( maxlay1 );
		presure.dim( maxlay1 );
		nmix.dim( maxlay1 );
		iprop.dim( maxgas, maxlay1 );
		frct.dim( maxgas, maxlay1 );
		xgcon.dim( 3, maxgas );
		xgvis.dim( 3, maxgas );
		xgcp.dim( 3, maxgas );
		xwght.dim( maxgas );
		gama.dim( maxgas );

		// Locals
		//Support Pillars
		//   0 - does not have support pillar
		//   1 - have support pillar

		int i;
		int j;
		int NumOfProvGasses;

		Array1D_int DATE_TIME( 8 );
		Array1D_string real_CLOCK( 3 );


		static std::string dynFormat;

		// Formats
		static gio::Fmt Format_111( "('*')" );
		static gio::Fmt Format_112( "('* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *')" );
		static gio::Fmt Format_113( "('*------------------------------------------------------------')" );
		static gio::Fmt Format_200( "('* General options:')" );
		static gio::Fmt Format_210( "('* <nlayer, debug, standard, ThermalMod, CalcDeflection, SDScalar, VacuumPressure, VacuumMaxGapThickness>')" );
		static gio::Fmt Format_300( "('* Environmental settings:')" );
		static gio::Fmt Format_310( "('* <tout, tind, wso, iwd, wsi, dir, outir, isky, tsky, esky, fclr, trmin, Pa, Pini, Tini>')" );
		static gio::Fmt Format_400( "('* Overall IGU properties:')" );
		static gio::Fmt Format_410( "('* <totsol, tilt, height, heightt, width>')" );
		static gio::Fmt Format_600( "('* Outdoor environment:')" );
		static gio::Fmt Format_610( "('* <ibc(1), hout, presure(1), 1, 1, 1.0, vvent(1), tvent(1)>')" );
		static gio::Fmt Format_700( "('* IGU definition:')" );
		static gio::Fmt Format_800( "('* Indoor environment:')" );
		static gio::Fmt Format_810( "('* <ibc(2), hin, presure(nlayer+1), 1, 1, 1.0, vvent(nlayer+1), tvent(nlayer+1)>')" );
		static gio::Fmt Format_900( "('* End file')" );
		static gio::Fmt Format_10001( "('* created by TARCOG v. ',A)" );
		static gio::Fmt Format_1001( "('* TARCOG debug output for WinCOG, ',I4,'-',I2.2,'-',I2.2,', ',I2.2,':',I2.2,':',I2.2)" );
		static gio::Fmt Format_1002( "('*     WindowID:   ',I8,'  - Not specified')" );
		static gio::Fmt Format_1003( "('*     WindowID:   ',I8,' ')" );
		static gio::Fmt Format_1006( "('*     IGUID:      ',I8,'  - Not specified')" );
		static gio::Fmt Format_1007( "('*     IGUID:      ',I8,' ')" );
		static gio::Fmt Format_1008( "('*     Num Layers: ',I8,' ')" );
		static gio::Fmt Format_1010( "('    ',I1,', ',I1,', ',I1,', ',I1,', ',I1,', ',F24.12,', ',F24.12,', ',F24.12)" );
		static gio::Fmt Format_1020( "('    ',F24.12,', ',F24.12,', ',F24.12,', ',I1,', ',F24.12,', ',F24.12,', ',F24.12,', ',I1,', ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12)" );
		static gio::Fmt Format_1030( "('    ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12)" );
		static gio::Fmt Format_1031( "('    ',F24.12,', ',F24.12,', ',I3,', ',F24.12,', ',I3,', ',I3,', ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12,', ',I2,', ',I2,', ',I2,', ',I2)" );
		static gio::Fmt Format_1034( "('* <PillarSpacing(i), PillarRadius(i)')" );
		static gio::Fmt Format_1035( "('    ',F24.12,', ',F24.12)" );
		static gio::Fmt Format_1040( "('    ',I1,', ',F24.12,', ',F24.12,', ',I1,', ',I1,', ',F24.12,', ',F24.12,', ',F24.12)" );
		static gio::Fmt Format_1048( "('* <gap(i), GapDef(i), presure(i+1), nmix(i+1), (iprop(i+1, j), j=1,nmix(i+1)), (frct(i+1, j), ',/,/,'j=1,nmix(i+1)), vvent(i), tvent(i), SupportPillar(i)>')" );
		static gio::Fmt Format_1049( "('* Gap ',I1,':')" );
		static gio::Fmt Format_1041( "('    ',F24.12,', ',F24.12,', ',I1,', ',I1,', ',F24.12,', ',F24.12,', ',F24.12)" );
		static gio::Fmt Format_1042( "('    ',F24.12,', ',F24.12,', ',I1,', ',I1,', ',I1,', ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12)" );
		static gio::Fmt Format_1043( "('    ',F24.12,', ',F24.12,', ',I1,', ',I1,', ',I1,', ',I1,', ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12)" );
		static gio::Fmt Format_1050( "('* <scon(i), asol(i), thick(i), emis(2*i-1), emis(2*i), tir(2*i-1), YoungsMod(i),',/,/,' PoissonsRat(i), LayerType(i), nslice(i)>')" );
		static gio::Fmt Format_1051( "('    ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12,', ',I1,', ',I1)" );
		static gio::Fmt Format_1052( "('* <Atop(i), Abot(i), Al(i), Ar(i), Ah(i)>')" );
		static gio::Fmt Format_1053( "('    ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12)" );
		static gio::Fmt Format_1054( "('* <SlatThick(i), SlatWidth(i), SlatAngle(i), SlatCond(i), SlatSpacing(i), SlatCurve(i)>')" );
		static gio::Fmt Format_1055( "('    ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12)" );
		static gio::Fmt Format_1060( "('* Layer ',I1,' - specular-glass:')" );
		static gio::Fmt Format_1061( "('* Layer ',I1,' - venetian blind:')" );
		static gio::Fmt Format_1062( "('* Layer ',I1,' - woven shade:')" );
		static gio::Fmt Format_1063( "('* Layer ',I1,' - diffuse shade:')" );
		static gio::Fmt Format_1064( "('* Layer ',I1,' - ???:')" );
		static gio::Fmt Format_2000( "('* Gas coefficients information')" );
		static gio::Fmt Format_2010( "('    ',I2)" );
		static gio::Fmt Format_2011( "('* <NumberOfGasses>')" );
		static gio::Fmt Format_2020( "('    ',ES12.6)" );
		static gio::Fmt Format_2021( "('* <MolecularWeight>')" );
		static gio::Fmt Format_2030( "(', ',ES12.6,$)" );
		static gio::Fmt Format_2031( "('* <gconA, gconB, gconC>')" );
		static gio::Fmt Format_2032( "('* <gvisA, gvisB, gvisC>')" );
		static gio::Fmt Format_2033( "('* <gcpA, gcpB, gcpC>')" );
		static gio::Fmt Format_2034( "('* <Gamma>')" );
		static gio::Fmt Format_1198( "(' *************************************************')" );

		//open(unit=WINCogFile,  file=TRIM(DBGD)//TRIM(WinCogFileName),  status='unknown', access=FileMode, &
		//       position=FilePosition, form='formatted', iostat=nperr)
		//if (nperr.ne.0) open(unit=WINCogFile,  file=TRIM(WinCogFileName),  status='unknown', access=FileMode, &
		//                      position=FilePosition, form='formatted', iostat=nperr)
		//else
		//  open(unit=WINCogFile,  file=TRIM(DBGD)//TRIM(SHGCFileName),  status='unknown', access=FileMode, &
		//         position=FilePosition, form='formatted', iostat=nperr)
		//  if (nperr.ne.0) open(unit=WINCogFile,  file=TRIM(SHGCFileName),  status='unknown', access=FileMode, &
		//                        position=FilePosition, form='formatted', iostat=nperr)
		//end if

		//bi...Create WINCOG input file using Tarcog's input arguments:

		//bi...Write the header:

		date_and_time( real_CLOCK( 1 ), real_CLOCK( 2 ), real_CLOCK( 3 ), DATE_TIME );

		gio::write( WINCogFile, Format_112 );
		gio::write( WINCogFile, Format_111 );
		gio::write( WINCogFile, Format_1001 ) << DATE_TIME( 1 ) << DATE_TIME( 2 ) << DATE_TIME( 3 ) << DATE_TIME( 5 ) << DATE_TIME( 6 ) << DATE_TIME( 7 );
		gio::write( WINCogFile, Format_10001 ) << VerNum; //, VerDat
		gio::write( WINCogFile, Format_111 );

		if ( winID == -1 ) {
			gio::write( WINCogFile, Format_1002 ) << winID;
		} else {
			gio::write( WINCogFile, Format_1003 ) << winID;
		}
		if ( iguID == -1 ) {
			gio::write( WINCogFile, Format_1006 ) << iguID;
		} else {
			gio::write( WINCogFile, Format_1007 ) << iguID;
		}

		gio::write( WINCogFile, Format_1008 ) << nlayer;
		gio::write( WINCogFile, Format_111 );
		gio::write( WINCogFile, Format_112 );

		//bi...Write main body:

		gio::write( WINCogFile, Format_113 );
		gio::write( WINCogFile, Format_200 );
		gio::write( WINCogFile, Format_113 );
		gio::write( WINCogFile, Format_210 );
		gio::write( WINCogFile, Format_1010 ) << nlayer << 2 << standard << ThermalMod << CalcDeflection << SDScalar << VacuumPressure << VacuumMaxGapThickness;

		gio::write( WINCogFile, Format_113 );
		gio::write( WINCogFile, Format_300 );
		gio::write( WINCogFile, Format_113 );
		gio::write( WINCogFile, Format_310 );
		gio::write( WINCogFile, Format_1020 ) << tout << tind << wso << iwd << wsi << dir << outir << isky << tsky << esky << fclr << trmin << Pa << Pini << Tini;

		//if (mgas.eq.0) then
		NumOfProvGasses = 0;
		while ( xwght( NumOfProvGasses + 1 ) != 0 ) {
			++NumOfProvGasses;
		}
		gio::write( WINCogFile, Format_113 );
		gio::write( WINCogFile, Format_2000 );
		gio::write( WINCogFile, Format_113 );
		gio::write( WINCogFile, Format_2011 );
		gio::write( WINCogFile, Format_2010 ) << NumOfProvGasses;
		for ( i = 1; i <= NumOfProvGasses; ++i ) {
			gio::write( WINCogFile, Format_2021 );
			gio::write( WINCogFile, Format_2020 ) << xwght( i );
			gio::write( WINCogFile, Format_2031 );
			gio::write( Format_2020 ) << xgcon( 1, 1 );
			for ( j = 2; i <= 3; ++j ) {
				gio::write( WINCogFile, Format_2030 ) << xgcon( j, i );
			}
			gio::write( WINCogFile );
			gio::write( WINCogFile, Format_2032 );
			gio::write( Format_2020 ) << xgvis( 1, 1 );
			for ( j = 2; i <= 3; ++j ) {
				gio::write( WINCogFile, Format_2030 ) << xgvis( j, i );
			}
			gio::write( WINCogFile );
			gio::write( WINCogFile, Format_2033 );
			gio::write( Format_2020 ) << xgcp( 1, 1 );
			for ( j = 2; i <= 3; ++j ) {
				gio::write( WINCogFile, Format_2030 ) << xgcp( j, i );
			}
			gio::write( WINCogFile );
			gio::write( WINCogFile, Format_2034 );
			gio::write( WINCogFile, Format_2020 ) << gama( i );
		} //i = 1, NumProvGasses
		//end if

		gio::write( WINCogFile, Format_113 );
		gio::write( WINCogFile, Format_400 );
		gio::write( WINCogFile, Format_113 );
		gio::write( WINCogFile, Format_410 );
		gio::write( WINCogFile, Format_1030 ) << totsol << tilt << height << heightt << width;

		//write(WINCogFile,500)
		//write(WINCogFile,*) SlatLength, SlatSpacing, SlatAngle, &
		//    CurvatureRadius, SubdivisionNumber, Rf, Rb, T, Ef_IR, Eb_IR, T_IR, &
		//    NumThetas, SOLMethod, FIRMethod, SOLhemCalc, SOLdifCalc
		//write(WINCogFile,*) '    0.016, 0.012, 45, 0.0, 5, 0, 0.70, 0.40, 0.00, 0.90, 0.90, 0.00, 1, 1, 1, 1'
		//write(WINCogFile,117)
		//write(WINCogFile,*) '    0.003, 0.01, 0.8, 0.6, 1, 1, 1'
		//write(WINCogFile,118)
		//write(WINCogFile,*) '    1'
		//write(WINCogFile,*) '    0'
		//write(WINCogFile,*) '    1'
		//write(WINCogFile,*) '    0'

		gio::write( WINCogFile, Format_113 );
		gio::write( WINCogFile, Format_600 );
		gio::write( WINCogFile, Format_113 );
		gio::write( WINCogFile, Format_610 );
		gio::write( WINCogFile, Format_1040 ) << ibc( 1 ) << hout << presure( 1 ) << 1 << 1 << 1.0 << vvent( 1 ) << tvent( 1 );

		gio::write( WINCogFile, Format_700 );

		for ( i = 1; i <= nlayer; ++i ) {
			gio::write( WINCogFile, Format_113 );
			if ( LayerType( i ) == SPECULAR ) {
				gio::write( WINCogFile, Format_1060 ) << i;
			} else if ( LayerType( i ) == VENETBLIND ) {
				gio::write( WINCogFile, Format_1061 ) << i;
			} else if ( LayerType( i ) == WOVSHADE ) {
				gio::write( WINCogFile, Format_1062 ) << i;
			} else if ( LayerType( i ) == DIFFSHADE ) {
				gio::write( WINCogFile, Format_1063 ) << i;
			} else {
				gio::write( WINCogFile, Format_1064 ) << i;
			}
			gio::write( WINCogFile, Format_113 );

			gio::write( WINCogFile, Format_1050 );
			gio::write( WINCogFile, Format_1051 ) << scon( i ) << asol( i ) << thick( i ) << emis( 2 * i - 1 ) << emis( 2 * i ) << tir( 2 * i - 1 ) << YoungsMod( i ) << PoissonsRat( i ) << LayerType( i ) << nslice( i );

			if ( IsShadingLayer( LayerType( i ) ) ) {
				gio::write( WINCogFile, Format_1052 );
				gio::write( WINCogFile, Format_1053 ) << Atop( i ) << Abot( i ) << Al( i ) << Ar( i ) << Ah( i );
			}

			if ( LayerType( i ) == VENETBLIND ) {
				gio::write( WINCogFile, Format_1054 );
				gio::write( WINCogFile, Format_1055 ) << SlatThick( i ) << SlatWidth( i ) << SlatAngle( i ) << SlatCond( i ) << SlatSpacing( i ) << SlatCurve( i );
			}

			if ( i < nlayer ) {
				gio::write( WINCogFile, Format_113 );
				gio::write( WINCogFile, Format_1049 ) << i;
				gio::write( WINCogFile, Format_113 );
				gio::write( WINCogFile, Format_1048 );
				gio::write( WINCogFile, "('    ',F24.12, ', ', F24.12,', ',F24.12,', ',I1,', ',$)" ) << gap( i ) << GapDef( i ) << presure( i + 1 ) << nmix( i + 1 );
				for ( j = 1; j <= nmix( i + 1 ); ++ j ) {
					gio::write( WINCogFile, "(I1,', ',$)" ) << iprop( j, i + 1 );
				}
				for ( j = 1; j <= nmix( i + 1 ); ++ j ) {
					gio::write( WINCogFile, "(F24.12,', ',$)" ) << frct( j, i + 1 );
				}
				gio::write( WINCogFile, "('    ',F24.12,', ', F24.12,', ',F24.12,', ',I1)" ) << vvent( i + 1 ) << tvent( i + 1 ) << SupportPillar( i );
				if ( SupportPillar( i ) == YES_SupportPillar ) {
					gio::write( WINCogFile, Format_1034 );
					gio::write( WINCogFile, Format_1035 ) << PillarSpacing( i ) << PillarRadius( i );
				}
			}
		} //  i - layers

		gio::write( WINCogFile, Format_113 );
		gio::write( WINCogFile, Format_800 );
		gio::write( WINCogFile, Format_113 );

		gio::write( WINCogFile, Format_810 );
		gio::write( WINCogFile, Format_1040 ) << ibc( 2 ) << hin << presure( nlayer + 1 ) << 1 << 1 << 1.0 << vvent( nlayer + 1 ) << tvent( nlayer + 1 );

		gio::write( WINCogFile, Format_113 );
		gio::write( WINCogFile, Format_900 );
		gio::write( WINCogFile, Format_113 );
		//  write(WINCogFile, 1198)
		gio::write( WINCogFile, fmtLD );

		//close(WINCogFile)

		//!!!!!!!!!!!!!!!!!!
		//!!
		//!! Formats:
		//!!
		//!!!!!!!!!!!!!!!!!!

		//  General:

		//  Gaps/environment:

		//  Layers:

		//2020  format('    ',F12.6)

	}

	void
	FinishDebugOutputFiles( int const nperr )
	{

		// Locals

		// Formats
		static gio::Fmt Format_2360( "('TARCOG status: ',I3,' - Normal termination.')" );
		static gio::Fmt Format_2361( "('TARCOG status: ',I3,' - Warning!')" );
		static gio::Fmt Format_2362( "('TARCOG status: ',I3,' - Error!')" );
		static gio::Fmt Format_1199( "('#####  #####  #####  #####  #####  #####  #####  #####  #####  #####  #####')" );

		if ( WriteDebugOutput ) {
			//open(unit=OutArgumentsFile,  file=TRIM(DBGD)//DebugOutputFileName,  status='unknown', position='APPEND',  &
			//      &  form='formatted', iostat=ferr)
			//if (ferr.ne.0) open(unit=OutArgumentsFile,  file=DebugOutputFileName,  status='unknown', position='APPEND',  &
			//      &  form='formatted', iostat=ferr)

			gio::write( OutArgumentsFile, fmtLD );
			if ( ( nperr > 0 ) && ( nperr < 1000 ) ) {
				gio::write( OutArgumentsFile, Format_2362 ) << nperr;
			} else if ( ( nperr >= 1000 ) ) {
				gio::write( OutArgumentsFile, Format_2361 ) << nperr;
			} else {
				gio::write( OutArgumentsFile, Format_2360 ) << nperr;
			}

			gio::write( OutArgumentsFile, fmtLD );
			gio::write( OutArgumentsFile, Format_1199 );
			gio::write( OutArgumentsFile, Format_1199 );

			//close(OutArgumentsFile)
		} // debug

		// Close debug files
		if ( InArgumentsFile != statusClosed ) {
			gio::close( InArgumentsFile );
			InArgumentsFile = statusClosed;
			OutArgumentsFile = statusClosed; // This is same is InArgumentsFile
		}

		if ( WINCogFile != statusClosed ) {
			gio::close( WINCogFile );
			WINCogFile = statusClosed;
		}

		if ( IterationCSVFileNumber != statusClosed ) {
			gio::close( IterationCSVFileNumber );
			IterationCSVFileNumber = statusClosed;
		}

		if ( TarcogIterationsFileNumber != statusClosed ) {
			gio::close( TarcogIterationsFileNumber );
			TarcogIterationsFileNumber = statusClosed;
		}

		//!!!!!!!!!!!!!!!!!!
		//!!
		//!! Formats:
		//!!
		//!!!!!!!!!!!!!!!!!!

	}

	void
	PrepDebugFilesAndVariables(
		std::string const & Debug_dir,
		std::string const & Debug_file,
		int const Debug_mode,
		int const win_ID,
		int const igu_ID,
		int & nperr
	)
	{

		// Locals
		char LastPathChar;
		std::string::size_type LastPathCharIndex;

		DBGD = Debug_dir;

		LastPathCharIndex = len( Debug_dir );
		if ( LastPathCharIndex > 0 ) {
			LastPathChar = Debug_dir[ LastPathCharIndex - 1 ];
			if ( LastPathChar != '/' ) DBGD = Debug_dir + '/';
			if ( ( LastPathChar == '/' ) && ( LastPathCharIndex == 1 ) ) DBGD = "";
		}

		//DebugDir = Debug_dir
		DebugMode = Debug_mode;
		winID = win_ID;
		iguID = igu_ID;

		//setup file names if file name is provided, otherwise keep default
		if ( Debug_file != "" ) {
			WinCogFileName = Debug_file + ".w7";
			//SHGCFileName = TRIM(Debug_file)//'_SHGC.w7'
			DebugOutputFileName = Debug_file + ".dbg";
		}

		//bi...Write debug output files - if debug flag > 0:

		WriteDebugOutput = false;
		if ( ( Debug_mode > minDebugFlag ) && ( Debug_mode <= maxDebugFlag ) ) {

			WriteDebugOutput = true;
			if ( Debug_mode == appendResultsToFile ) FilePosition = "APPEND";
			if ( ( Debug_mode == resultsToNewFile ) || ( Debug_mode == saveIntermediateResults ) ) FileMode = "SEQUENTIAL";

			InArgumentsFile = GetNewUnitNumber();
			//      open(newunit=InArgumentsFile,  file=TRIM(DBGD)//DebugOutputFileName,  status='unknown', access=FileMode,  &
			//              position=FilePosition, form='formatted', iostat=nperr)
			{ IOFlags flags; flags.ACCESS( FileMode ); flags.FORM( "formatted" ); flags.STATUS( "unknown" ); flags.POSITION( FilePosition ); gio::open( InArgumentsFile, DBGD + DebugOutputFileName, flags ); nperr = flags.ios(); }

			//      if (nperr.ne.0)  open(newunit=InArgumentsFile,  file=DebugOutputFileName,  status='unknown', access=FileMode,  &
			//              position=FilePosition, form='formatted', iostat=nperr)
			if ( nperr != 0 ) { IOFlags flags; flags.ACCESS( FileMode ); flags.FORM( "formatted" ); flags.STATUS( "unknown" ); flags.POSITION( FilePosition ); gio::open( InArgumentsFile, DebugOutputFileName, flags ); nperr = flags.ios(); }

			OutArgumentsFile = InArgumentsFile;

			WINCogFile = GetNewUnitNumber();
			//      open(newunit=WINCogFile,  file=TRIM(DBGD)//TRIM(WinCogFileName),  status='unknown', access=FileMode, &
			//             position=FilePosition, form='formatted', iostat=nperr)
			{ IOFlags flags; flags.ACCESS( FileMode ); flags.FORM( "formatted" ); flags.STATUS( "unknown" ); flags.POSITION( FilePosition ); gio::open( WINCogFile, DBGD + WinCogFileName, flags ); nperr = flags.ios(); }
			//      if (nperr.ne.0) open(newunit=WINCogFile,  file=TRIM(WinCogFileName),  status='unknown', access=FileMode, &
			//                            position=FilePosition, form='formatted', iostat=nperr)
			if ( nperr != 0 ) { IOFlags flags; flags.ACCESS( FileMode ); flags.FORM( "formatted" ); flags.STATUS( "unknown" ); flags.POSITION( FilePosition ); gio::open( WINCogFile, WinCogFileName, flags ); nperr = flags.ios(); }

			if ( Debug_mode == saveIntermediateResults ) {
				TarcogIterationsFileNumber = GetNewUnitNumber();
				//        open(newunit=TarcogIterationsFileNumber,  file=TRIM(DBGD)//'TarcogIterations.dbg',  status='unknown', position='APPEND',  &
				//              form='formatted', iostat=nperr)
				{ IOFlags flags; flags.FORM( "formatted" ); flags.STATUS( "unknown" ); flags.POSITION( "APPEND" ); gio::open( TarcogIterationsFileNumber, DBGD + DataStringGlobals::TarcogIterationsFileName, flags ); nperr = flags.ios(); }

				//        if (nperr.ne.0)  open(newunit=TarcogIterationsFileNumber, file='TarcogIterations.dbg',status='unknown', position='APPEND',  &
				//              &  form='formatted', iostat=nperr)
				if ( nperr != 0 ) { IOFlags flags; flags.FORM( "formatted" ); flags.STATUS( "unknown" ); flags.POSITION( "APPEND" ); gio::open( TarcogIterationsFileNumber, DataStringGlobals::TarcogIterationsFileName, flags ); nperr = flags.ios(); }

				IterationCSVFileNumber = GetNewUnitNumber();
				//        open(newunit=IterationCSVFileNumber,  file=TRIM(DBGD)//TRIM(IterationCSVName),  status='unknown', position='APPEND',  &
				//              form='formatted', iostat=nperr)
				{ IOFlags flags; flags.FORM( "formatted" ); flags.STATUS( "unknown" ); flags.POSITION( "APPEND" ); gio::open( IterationCSVFileNumber, DBGD + IterationCSVName, flags ); nperr = flags.ios(); }

				//        if (nperr.ne.0)  open(newunit=IterationCSVFileNumber,  file=TRIM(IterationCSVName),  status='unknown', position='APPEND',  &
				//              form='formatted', iostat=nperr)
				if ( nperr != 0 ) { IOFlags flags; flags.FORM( "formatted" ); flags.STATUS( "unknown" ); flags.POSITION( "APPEND" ); gio::open( IterationCSVFileNumber, IterationCSVName, flags ); nperr = flags.ios(); }
			}
		}

	}

} // TARCOGOutput

} // EnergyPlus
