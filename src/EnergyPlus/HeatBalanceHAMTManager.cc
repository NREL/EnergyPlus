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
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/member.functions.hh>

// EnergyPlus Headers
#include <HeatBalanceHAMTManager.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalSurface.hh>
#include <DataMoistureBalance.hh>
#include <DataSurfaces.hh>
#include <DisplayRoutines.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <OutputProcessor.hh>
#include <Psychrometrics.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace HeatBalanceHAMTManager {

	// MODULE INFORMATION:
	//       AUTHOR      Phillip Biddulph
	//       DATE WRITTEN   June 2008
	//       MODIFIED
	//       Bug fixes to make sure HAMT can cope with data limits  ! PDB August 2009
	//       RE-ENGINEERED

	// PURPOSE OF THIS MODULE:
	// Calculate, record and report the one dimentional heat and moisture transfer
	// through a surface given the material composition of the building surface and
	// the external and internal Temperatures and Relative Humidities.

	// METHODOLOGY EMPLOYED:
	// Each surface is split into "cells", where all characteristics are initiallised.
	// Cells are matched and links created in the initialisation routine.
	// The internal and external "surfaces" of the surface are virtual cells to allow for the
	// input of heat and vapor via heat transfer coefficients, radiation,
	// and vapor transfer coefficients
	// Uses Forward (implicit) finite difference alogorithm. Heat transfer is caclulated first,
	// with the option of including the latent heat, then liquid and vapor transfer. The process is ittereated.
	// Once the temperatures have converged the internal surface
	// temperature and vapor densities are passed back to EnergyPlus.

	// Temperatures and relative humidities are updated once EnergyPlus has checked that
	// the zone temperatures have converged.

	// REFERENCES:
	// K?zel, H.M. (1995) Simultaneous Heat and Moisture Transport in Building Components.
	// One- and two-dimensional calculation using simple parameters. IRB Verlag 1995
	// Holman, J.P. (2002) Heat Transfer, Ninth Edition. McGraw-Hill
	// Winterton, R.H.S. (1997) Heat Transfer. (Oxford Chemistry Primers; 50) Oxford University Press
	// Kumar Kumaran, M. (1996) IEA ANNEX 24, Final Report, Volume 3

	// USE STATEMENTS:

	// Using/Aliasing
	using namespace DataGlobals;
	using namespace DataMoistureBalance;
	using namespace DataSurfaces;
	using DataHeatBalSurface::QRadSWOutAbs;
	using DataHeatBalSurface::QRadSWInAbs;
	using DataHeatBalSurface::NetLWRadToSurf;
	using DataHeatBalSurface::MaxSurfaceTempLimit;
	using DataHeatBalSurface::MaxSurfaceTempLimitBeforeFatal;
	using DataHeatBalSurface::MinSurfaceTempLimit;
	using DataHeatBalSurface::MinSurfaceTempLimitBeforeFatal;
	using namespace DataHeatBalance;
	using namespace Psychrometrics;
	using DataHeatBalFanSys::MAT;
	using DataHeatBalFanSys::QHTRadSysSurf;
	using DataHeatBalFanSys::QHWBaseboardSurf;
	using DataHeatBalFanSys::QSteamBaseboardSurf;
	using DataHeatBalFanSys::QElecBaseboardSurf;
	using DataEnvironment::SkyTemp;
	using DataEnvironment::SunIsUp;
	using DataEnvironment::OutBaroPress;
	using DataEnvironment::OutEnthalpy;
	using DataEnvironment::IsRain;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	int const ittermax( 150 ); // Maximum Number of itterations
	int const adjmax( 6 ); // Maximum Number of Adjacent Cells

	Real64 const wdensity( 1000.0 ); // Density of water kg.m-3
	Real64 const wspech( 4180.0 ); // Specific Heat Capacity of Water J.kg-1.K-1 (at 20C)
	Real64 const whv( 2489000.0 ); // Evaporation enthalpy of water J.kg-1
	Real64 const convt( 0.002 ); // Temperature convergence limit
	Real64 const qvplim( 100000.0 ); // Maximum latent heat W
	Real64 const rhmax( 1.01 ); // Maximum RH value

	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	Array1D_int firstcell;
	Array1D_int lastcell;
	Array1D_int Extcell;
	Array1D_int ExtRadcell;
	Array1D_int ExtConcell;
	Array1D_int ExtSkycell;
	Array1D_int ExtGrncell;
	Array1D_int Intcell;
	Array1D_int IntConcell;

	Array1D< Real64 > watertot;
	Array1D< Real64 > surfrh;
	Array1D< Real64 > surfextrh;
	Array1D< Real64 > surftemp;
	Array1D< Real64 > surfexttemp;
	Array1D< Real64 > surfvp;

	Array1D< Real64 > extvtc; // External Surface vapor transfer coefficient
	Array1D< Real64 > intvtc; // Internal Surface Vapor Transfer Coefficient
	Array1D_bool extvtcflag; // External Surface vapor transfer coefficient flag
	Array1D_bool intvtcflag; // Internal Surface Vapor Transfer Coefficient flag
	Array1D_bool MyEnvrnFlag; // Flag to reset surface properties.

	Real64 deltat( 0.0 ); // time step in seconds

	int TotCellsMax( 0 ); // Maximum number of cells per material

	bool latswitch( false ); // latent heat switch,
	bool rainswitch( false ); // rain switch,

	// SUBROUTINE SPECIFICATIONS FOR MODULE HeatBalanceHAMTManager:

	// Object Data
	Array1D< subcell > cells;

	// Functions

	void
	ManageHeatBalHAMT(
		int const SurfNum,
		Real64 & TempSurfInTmp,
		Real64 & TempSurfOutTmp
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Phillip Biddulph
		//       DATE WRITTEN   June 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Manages the Heat and Moisture Transfer calculations.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		static bool OneTimeFlag( true );

		if ( OneTimeFlag ) {
			OneTimeFlag = false;
			DisplayString( "Initialising Heat and Moisture Transfer Model" );
			GetHeatBalHAMTInput();
			InitHeatBalHAMT();
		}

		CalcHeatBalHAMT( SurfNum, TempSurfInTmp, TempSurfOutTmp );

	}

	void
	GetHeatBalHAMTInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Phillip Biddulph
		//       DATE WRITTEN   June 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// gets input for the HAMT model

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace InputProcessor;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const cHAMTObject1( "MaterialProperty:HeatAndMoistureTransfer:Settings" );
		static std::string const cHAMTObject2( "MaterialProperty:HeatAndMoistureTransfer:SorptionIsotherm" );
		static std::string const cHAMTObject3( "MaterialProperty:HeatAndMoistureTransfer:Suction" );
		static std::string const cHAMTObject4( "MaterialProperty:HeatAndMoistureTransfer:Redistribution" );
		static std::string const cHAMTObject5( "MaterialProperty:HeatAndMoistureTransfer:Diffusion" );
		static std::string const cHAMTObject6( "MaterialProperty:HeatAndMoistureTransfer:ThermalConductivity" );
		static std::string const cHAMTObject7( "SurfaceProperties:VaporCoefficients" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Array1D_string AlphaArray;
		Array1D_string cAlphaFieldNames;
		Array1D_string cNumericFieldNames;

		Array1D_bool lAlphaBlanks;
		Array1D_bool lNumericBlanks;

		Array1D< Real64 > NumArray;

		Real64 dumrh;
		Real64 dumdata;
		Real64 avdata;

		int MaxNums;
		int MaxAlphas;
		int NumParams;
		int NumNums;
		int NumAlphas;
		int status;
		int matid;
		int iso;
		int Numid;
		int suc;
		int red;
		int mu;
		int tc;
		//unused1208    INTEGER :: sid
		int HAMTitems;
		int item;
		int ii;
		int jj;
		int vtcsid;

		bool avflag;
		bool isoerrrise;
		bool ErrorsFound;

		watertot.allocate( TotSurfaces );
		surfrh.allocate( TotSurfaces );
		surfextrh.allocate( TotSurfaces );
		surftemp.allocate( TotSurfaces );
		surfexttemp.allocate( TotSurfaces );
		surfvp.allocate( TotSurfaces );

		firstcell.allocate( TotSurfaces );
		lastcell.allocate( TotSurfaces );
		Extcell.allocate( TotSurfaces );
		ExtRadcell.allocate( TotSurfaces );
		ExtConcell.allocate( TotSurfaces );
		ExtSkycell.allocate( TotSurfaces );
		ExtGrncell.allocate( TotSurfaces );
		Intcell.allocate( TotSurfaces );
		IntConcell.allocate( TotSurfaces );

		extvtc.allocate( TotSurfaces );
		intvtc.allocate( TotSurfaces );
		extvtcflag.allocate( TotSurfaces );
		intvtcflag.allocate( TotSurfaces );
		MyEnvrnFlag.allocate( TotSurfaces );

		extvtc = -1.0;
		intvtc = -1.0;
		extvtcflag = false;
		intvtcflag = false;
		MyEnvrnFlag = true;

		latswitch = true;
		rainswitch = true;

		MaxAlphas = 0;
		MaxNums = 0;
		GetObjectDefMaxArgs( cHAMTObject1, NumParams, NumAlphas, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		MaxNums = max( MaxNums, NumNums );
		GetObjectDefMaxArgs( cHAMTObject2, NumParams, NumAlphas, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		MaxNums = max( MaxNums, NumNums );
		GetObjectDefMaxArgs( cHAMTObject3, NumParams, NumAlphas, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		MaxNums = max( MaxNums, NumNums );
		GetObjectDefMaxArgs( cHAMTObject4, NumParams, NumAlphas, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		MaxNums = max( MaxNums, NumNums );
		GetObjectDefMaxArgs( cHAMTObject5, NumParams, NumAlphas, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		MaxNums = max( MaxNums, NumNums );
		GetObjectDefMaxArgs( cHAMTObject6, NumParams, NumAlphas, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		MaxNums = max( MaxNums, NumNums );
		GetObjectDefMaxArgs( cHAMTObject7, NumParams, NumAlphas, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		MaxNums = max( MaxNums, NumNums );

		ErrorsFound = false;

		AlphaArray.allocate( MaxAlphas );
		cAlphaFieldNames.allocate( MaxAlphas );
		cNumericFieldNames.allocate( MaxNums );
		NumArray.dimension( MaxNums, 0.0 );
		lAlphaBlanks.dimension( MaxAlphas, false );
		lNumericBlanks.dimension( MaxNums, false );

		HAMTitems = GetNumObjectsFound( cHAMTObject1 ); // MaterialProperty:HeatAndMoistureTransfer:Settings
		for ( item = 1; item <= HAMTitems; ++item ) {
			GetObjectItem( cHAMTObject1, item, AlphaArray, NumAlphas, NumArray, NumNums, status, lNumericBlanks, lAlphaBlanks, cAlphaFieldNames, cNumericFieldNames );

			matid = FindItemInList( AlphaArray( 1 ), Material );

			if ( matid == 0 ) {
				ShowSevereError( cHAMTObject1 + ' ' + cAlphaFieldNames( 1 ) + "=\"" + AlphaArray( 1 ) + "\" is invalid (undefined)." );
				ShowContinueError( "The basic material must be defined in addition to specifying HeatAndMoistureTransfer properties." );
				ErrorsFound = true;
				continue;
			}
			if ( Material( matid ).ROnly ) {
				ShowWarningError( cHAMTObject1 + ' ' + cAlphaFieldNames( 1 ) + "=\"" + AlphaArray( 1 ) + "\" is defined as an R-only value material." );
				continue;
			}

			Material( matid ).Porosity = NumArray( 1 );
			Material( matid ).iwater = NumArray( 2 );

		}

		HAMTitems = GetNumObjectsFound( cHAMTObject2 ); // MaterialProperty:HeatAndMoistureTransfer:SorptionIsotherm
		for ( item = 1; item <= HAMTitems; ++item ) {
			GetObjectItem( cHAMTObject2, item, AlphaArray, NumAlphas, NumArray, NumNums, status, lNumericBlanks, lAlphaBlanks, cAlphaFieldNames, cNumericFieldNames );

			matid = FindItemInList( AlphaArray( 1 ), Material );

			if ( matid == 0 ) {
				ShowSevereError( cHAMTObject2 + ' ' + cAlphaFieldNames( 1 ) + "=\"" + AlphaArray( 1 ) + "\" is invalid (undefined)." );
				ShowContinueError( "The basic material must be defined in addition to specifying HeatAndMoistureTransfer properties." );
				ErrorsFound = true;
				continue;
			}
			if ( Material( matid ).ROnly ) {
				ShowWarningError( cHAMTObject2 + ' ' + cAlphaFieldNames( 1 ) + "=\"" + AlphaArray( 1 ) + "\" is defined as an R-only value material." );
				continue;
			}

			Numid = 1;

			Material( matid ).niso = int( NumArray( Numid ) );

			for ( iso = 1; iso <= Material( matid ).niso; ++iso ) {
				++Numid;
				Material( matid ).isorh( iso ) = NumArray( Numid );
				++Numid;
				Material( matid ).isodata( iso ) = NumArray( Numid );
			}

			++Material( matid ).niso;
			Material( matid ).isorh( Material( matid ).niso ) = rhmax;
			Material( matid ).isodata( Material( matid ).niso ) = Material( matid ).Porosity * wdensity;

			++Material( matid ).niso;
			Material( matid ).isorh( Material( matid ).niso ) = 0.0;
			Material( matid ).isodata( Material( matid ).niso ) = 0.0;
		}

		// check the isotherm
		for ( matid = 1; matid <= TotMaterials; ++matid ) {
			if ( Material( matid ).niso > 0 ) {
				// - First sort
				for ( jj = 1; jj <= Material( matid ).niso - 1; ++jj ) {
					for ( ii = jj + 1; ii <= Material( matid ).niso; ++ii ) {
						if ( Material( matid ).isorh( jj ) > Material( matid ).isorh( ii ) ) {

							dumrh = Material( matid ).isorh( jj );
							dumdata = Material( matid ).isodata( jj );

							Material( matid ).isorh( jj ) = Material( matid ).isorh( ii );
							Material( matid ).isodata( jj ) = Material( matid ).isodata( ii );

							Material( matid ).isorh( ii ) = dumrh;
							Material( matid ).isodata( ii ) = dumdata;

						}
					}
				}
				//- Now make sure the data rises
				isoerrrise = false;
				for ( ii = 1; ii <= 100; ++ii ) {
					avflag = true;
					for ( jj = 1; jj <= Material( matid ).niso - 1; ++jj ) {
						if ( Material( matid ).isodata( jj ) > Material( matid ).isodata( jj + 1 ) ) {
							isoerrrise = true;
							avdata = ( Material( matid ).isodata( jj ) + Material( matid ).isodata( jj + 1 ) ) / 2.0;
							Material( matid ).isodata( jj ) = avdata;
							Material( matid ).isodata( jj + 1 ) = avdata;
							avflag = false;
						}
					}
					if ( avflag ) break;
				}
				if ( isoerrrise ) {
					ShowWarningError( cHAMTObject2 + " data not rising - Check material " + Material( matid ).Name );
					ShowContinueError( "Isotherm data has been fixed, and the simulation continues." );
				}
			}
		}

		HAMTitems = GetNumObjectsFound( cHAMTObject3 ); // MaterialProperty:HeatAndMoistureTransfer:Suction
		for ( item = 1; item <= HAMTitems; ++item ) {
			GetObjectItem( cHAMTObject3, item, AlphaArray, NumAlphas, NumArray, NumNums, status, lNumericBlanks, lAlphaBlanks, cAlphaFieldNames, cNumericFieldNames );

			matid = FindItemInList( AlphaArray( 1 ), Material );

			if ( matid == 0 ) {
				ShowSevereError( cHAMTObject3 + ' ' + cAlphaFieldNames( 1 ) + "=\"" + AlphaArray( 1 ) + "\" is invalid (undefined)." );
				ShowContinueError( "The basic material must be defined in addition to specifying HeatAndMoistureTransfer properties." );
				ErrorsFound = true;
				continue;
			}
			if ( Material( matid ).ROnly ) {
				ShowWarningError( cHAMTObject3 + ' ' + cAlphaFieldNames( 1 ) + "=\"" + AlphaArray( 1 ) + "\" is defined as an R-only value material." );
				continue;
			}

			Numid = 1;

			Material( matid ).nsuc = NumArray( Numid );
			for ( suc = 1; suc <= Material( matid ).nsuc; ++suc ) {
				++Numid;
				Material( matid ).sucwater( suc ) = NumArray( Numid );
				++Numid;
				Material( matid ).sucdata( suc ) = NumArray( Numid );
			}

			++Material( matid ).nsuc;
			Material( matid ).sucwater( Material( matid ).nsuc ) = Material( matid ).isodata( Material( matid ).niso );
			Material( matid ).sucdata( Material( matid ).nsuc ) = Material( matid ).sucdata( Material( matid ).nsuc - 1 );

		}

		HAMTitems = GetNumObjectsFound( cHAMTObject4 ); // MaterialProperty:HeatAndMoistureTransfer:Redistribution
		for ( item = 1; item <= HAMTitems; ++item ) {
			GetObjectItem( cHAMTObject4, item, AlphaArray, NumAlphas, NumArray, NumNums, status, lNumericBlanks, lAlphaBlanks, cAlphaFieldNames, cNumericFieldNames );

			matid = FindItemInList( AlphaArray( 1 ), Material );
			if ( matid == 0 ) {
				ShowSevereError( cHAMTObject4 + ' ' + cAlphaFieldNames( 1 ) + "=\"" + AlphaArray( 1 ) + "\" is invalid (undefined)." );
				ShowContinueError( "The basic material must be defined in addition to specifying HeatAndMoistureTransfer properties." );
				ErrorsFound = true;
				continue;
			}
			if ( Material( matid ).ROnly ) {
				ShowWarningError( cHAMTObject4 + ' ' + cAlphaFieldNames( 1 ) + "=\"" + AlphaArray( 1 ) + "\" is defined as an R-only value material." );
				continue;
			}
			Numid = 1;

			Material( matid ).nred = NumArray( Numid );
			for ( red = 1; red <= Material( matid ).nred; ++red ) {
				++Numid;
				Material( matid ).redwater( red ) = NumArray( Numid );
				++Numid;
				Material( matid ).reddata( red ) = NumArray( Numid );
			}

			++Material( matid ).nred;
			Material( matid ).redwater( Material( matid ).nred ) = Material( matid ).isodata( Material( matid ).niso );
			Material( matid ).reddata( Material( matid ).nred ) = Material( matid ).reddata( Material( matid ).nred - 1 );

		}

		HAMTitems = GetNumObjectsFound( cHAMTObject5 ); // MaterialProperty:HeatAndMoistureTransfer:Diffusion
		for ( item = 1; item <= HAMTitems; ++item ) {
			GetObjectItem( cHAMTObject5, item, AlphaArray, NumAlphas, NumArray, NumNums, status, lNumericBlanks, lAlphaBlanks, cAlphaFieldNames, cNumericFieldNames );

			matid = FindItemInList( AlphaArray( 1 ), Material );
			if ( matid == 0 ) {
				ShowSevereError( cHAMTObject5 + ' ' + cAlphaFieldNames( 1 ) + "=\"" + AlphaArray( 1 ) + "\" is invalid (undefined)." );
				ShowContinueError( "The basic material must be defined in addition to specifying HeatAndMoistureTransfer properties." );
				ErrorsFound = true;
				continue;
			}
			if ( Material( matid ).ROnly ) {
				ShowWarningError( cHAMTObject5 + ' ' + cAlphaFieldNames( 1 ) + "=\"" + AlphaArray( 1 ) + "\" is defined as an R-only value material." );
				continue;
			}

			Numid = 1;

			Material( matid ).nmu = NumArray( Numid );
			if ( Material( matid ).nmu > 0 ) {
				for ( mu = 1; mu <= Material( matid ).nmu; ++mu ) {
					++Numid;
					Material( matid ).murh( mu ) = NumArray( Numid );
					++Numid;
					Material( matid ).mudata( mu ) = NumArray( Numid );
				}

				++Material( matid ).nmu;
				Material( matid ).murh( Material( matid ).nmu ) = Material( matid ).isorh( Material( matid ).niso );
				Material( matid ).mudata( Material( matid ).nmu ) = Material( matid ).mudata( Material( matid ).nmu - 1 );

			}
		}

		HAMTitems = GetNumObjectsFound( cHAMTObject6 ); // MaterialProperty:HeatAndMoistureTransfer:ThermalConductivity
		for ( item = 1; item <= HAMTitems; ++item ) {
			GetObjectItem( cHAMTObject6, item, AlphaArray, NumAlphas, NumArray, NumNums, status, lNumericBlanks, lAlphaBlanks, cAlphaFieldNames, cNumericFieldNames );

			matid = FindItemInList( AlphaArray( 1 ), Material );
			if ( matid == 0 ) {
				ShowSevereError( cHAMTObject6 + ' ' + cAlphaFieldNames( 1 ) + "=\"" + AlphaArray( 1 ) + "\" is invalid (undefined)." );
				ShowContinueError( "The basic material must be defined in addition to specifying HeatAndMoistureTransfer properties." );
				ErrorsFound = true;
				continue;
			}
			if ( Material( matid ).ROnly ) {
				ShowWarningError( cHAMTObject6 + ' ' + cAlphaFieldNames( 1 ) + "=\"" + AlphaArray( 1 ) + "\" is defined as an R-only value material." );
				continue;
			}
			Numid = 1;

			Material( matid ).ntc = NumArray( Numid );
			if ( Material( matid ).ntc > 0 ) {
				for ( tc = 1; tc <= Material( matid ).ntc; ++tc ) {
					++Numid;
					Material( matid ).tcwater( tc ) = NumArray( Numid );
					++Numid;
					Material( matid ).tcdata( tc ) = NumArray( Numid );
				}

				++Material( matid ).ntc;
				Material( matid ).tcwater( Material( matid ).ntc ) = Material( matid ).isodata( Material( matid ).niso );
				Material( matid ).tcdata( Material( matid ).ntc ) = Material( matid ).tcdata( Material( matid ).ntc - 1 );

			}
		}

		// Vapor Transfer coefficients
		HAMTitems = GetNumObjectsFound( cHAMTObject7 ); // SurfaceProperties:VaporCoefficients
		for ( item = 1; item <= HAMTitems; ++item ) {
			GetObjectItem( cHAMTObject7, item, AlphaArray, NumAlphas, NumArray, NumNums, status, lNumericBlanks, lAlphaBlanks, cAlphaFieldNames, cNumericFieldNames );

			vtcsid = FindItemInList( AlphaArray( 1 ), Surface );
			if ( vtcsid == 0 ) {
				ShowSevereError( cHAMTObject7 + ' ' + cAlphaFieldNames( 1 ) + "=\"" + AlphaArray( 1 ) + "\" is invalid (undefined)." );
				ShowContinueError( "The basic material must be defined in addition to specifying HeatAndMoistureTransfer properties." );
				ErrorsFound = true;
				continue;
			}

			if ( AlphaArray( 2 ) == "YES" ) {
				extvtcflag( vtcsid ) = true;
				extvtc( vtcsid ) = NumArray( 1 );
			}

			if ( AlphaArray( 3 ) == "YES" ) {
				intvtcflag( vtcsid ) = true;
				intvtc( vtcsid ) = NumArray( 2 );
			}

		}

		AlphaArray.deallocate();
		cAlphaFieldNames.deallocate();
		cNumericFieldNames.deallocate();
		NumArray.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

		if ( ErrorsFound ) {
			ShowFatalError( "GetHeatBalHAMTInput: Errors found getting input.  Program terminates." );
		}

	}

	void
	InitHeatBalHAMT()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Phillip Biddulph
		//       DATE WRITTEN   June 2008
		//       MODIFIED       B. Griffith, Aug 2012 for surface-specific algorithms
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::TrimSigDigits;
		using General::ScanForReports;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const adjdist( 0.00005 ); // Allowable distance between two cells, also used as limit on cell length
		static std::string const RoutineName( "InitCombinedHeatAndMoistureFiniteElement: " );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int ii;
		int cid;
		int cid1;
		int cid2;
		int sid;
		int conid;
		int lid;
		int matid;
		int did;
		int adj1;
		int adj2;
		int errorCount;
		int MaterNum;

		Real64 runor;
		Real64 high1;
		Real64 low2;
		Real64 testlen;
		Real64 waterd; // water density
		bool DoReport;

		// Formats
		static gio::Fmt Format_1966( "('! <HAMT cells>, Surface Name, Construction Name, Cell Numbers')" );
		static gio::Fmt Format_1965( "('! <HAMT origins>, Surface Name, Construction Name, Cell origins (m) ')" );
		static gio::Fmt Format_1968( "('HAMT cells, ',A,',',A,400(,:,',',i4))" );
		static gio::Fmt Format_1967( "('HAMT origins,',A,',',A,400(,:,',',f10.7))" );
		static gio::Fmt Format_108( "('! <Material Nominal Resistance>, Material Name,  Nominal R')" );
		static gio::Fmt Format_111( "('Material Nominal Resistance's,2(',',A))" );

		deltat = TimeStepZone * 3600.0;

		// Check the materials information and work out how many cells are required.
		errorCount = 0;
		TotCellsMax = 0;
		for ( sid = 1; sid <= TotSurfaces; ++sid ) {
			if ( Surface( sid ).Class == SurfaceClass_Window ) continue;
			if ( Surface( sid ).HeatTransferAlgorithm != HeatTransferModel_HAMT ) continue;
			conid = Surface( sid ).Construction;
			if ( conid == 0 ) continue;
			for ( lid = 1; lid <= Construct( conid ).TotLayers; ++lid ) {
				matid = Construct( conid ).LayerPoint( lid );
				if ( Material( matid ).ROnly ) {
					ShowSevereError( RoutineName + "Construction=" + Construct( conid ).Name + " cannot contain R-only value materials." );
					ShowContinueError( "Reference Material=\"" + Material( matid ).Name + "\"." );
					++errorCount;
					continue;
				}

				if ( Material( matid ).nmu < 0 ) {
					ShowSevereError( RoutineName + "Construction=" + Construct( conid ).Name );
					ShowContinueError( "Reference Material=\"" + Material( matid ).Name + "\" does not have required Water Vapor Diffusion Resistance Factor (mu) data." );
					++errorCount;
				}

				if ( Material( matid ).niso < 0 ) {
					ShowSevereError( RoutineName + "Construction=" + Construct( conid ).Name );
					ShowContinueError( "Reference Material=\"" + Material( matid ).Name + "\" does not have required isotherm data." );
					++errorCount;
				}
				if ( Material( matid ).nsuc < 0 ) {
					ShowSevereError( RoutineName + "Construction=" + Construct( conid ).Name );
					ShowContinueError( "Reference Material=\"" + Material( matid ).Name + "\" does not have required liquid transport coefficient (suction) data." );
					++errorCount;
				}
				if ( Material( matid ).nred < 0 ) {
					ShowSevereError( RoutineName + "Construction=" + Construct( conid ).Name );
					ShowContinueError( "Reference Material=\"" + Material( matid ).Name + "\" does not have required liquid transport coefficient (redistribution) data." );
					++errorCount;
				}
				if ( Material( matid ).ntc < 0 ) {
					if ( Material( matid ).Conductivity > 0 ) {
						ShowWarningError( RoutineName + "Construction=" + Construct( conid ).Name );
						ShowContinueError( "Reference Material=\"" + Material( matid ).Name + "\" does not have thermal conductivity data. Using fixed value." );
						Material( matid ).ntc = 2;
						Material( matid ).tcwater( 1 ) = 0.0;
						Material( matid ).tcdata( 1 ) = Material( matid ).Conductivity;
						Material( matid ).tcwater( 2 ) = Material( matid ).isodata( Material( matid ).niso );
						Material( matid ).tcdata( 2 ) = Material( matid ).Conductivity;
					} else {
						ShowSevereError( RoutineName + "Construction=" + Construct( conid ).Name );
						ShowContinueError( "Reference Material=\"" + Material( matid ).Name + "\" does not have required thermal conductivity data." );
						++errorCount;
					}
				}

				// convert material water content to RH

				waterd = Material( matid ).iwater * Material( matid ).Density;
				interp( Material( matid ).niso, Material( matid ).isodata, Material( matid ).isorh, waterd, Material( matid ).irh );

				Material( matid ).divs = int( Material( matid ).Thickness / Material( matid ).divsize ) + Material( matid ).divmin;
				if ( Material( matid ).divs > Material( matid ).divmax ) {
					Material( matid ).divs = Material( matid ).divmax;
				}
				// Check length of cell - reduce number of divisions if neccessary
				while ( true ) {
					testlen = Material( matid ).Thickness * ( ( std::sin( Pi * ( -1.0 / double( Material( matid ).divs ) ) - Pi / 2.0 ) / 2.0 ) - ( std::sin( -Pi / 2.0 ) / 2.0 ) );
					if ( testlen > adjdist ) break;
					--Material( matid ).divs;
					if ( Material( matid ).divs < 1 ) {
						ShowSevereError( RoutineName + "Construction=" + Construct( conid ).Name );
						ShowContinueError( "Reference Material=\"" + Material( matid ).Name + "\" is too thin." );
						++errorCount;
						break;
					}
				}
				TotCellsMax += Material( matid ).divs;
			}
			TotCellsMax += 7;

		}

		if ( errorCount > 0 ) {
			ShowFatalError( "CombinedHeatAndMoistureFiniteElement: Incomplete data to start solution, program terminates." );
		}

		// Make the cells and initialise
		cells.allocate( TotCellsMax );
		for ( auto & e : cells ) {
			e.adjs = -1;
			e.adjsl = -1;
		}

		cid = 0;

		// Set up surface cell structure
		for ( sid = 1; sid <= TotSurfaces; ++sid ) {
			if ( ! Surface( sid ).HeatTransSurf ) continue;
			if ( Surface( sid ).Class == SurfaceClass_Window ) continue;
			if ( Surface( sid ).HeatTransferAlgorithm != HeatTransferModel_HAMT ) continue;
			// Boundary Cells
			runor = -0.02;
			// Air Convection Cell
			++cid;
			firstcell( sid ) = cid;
			ExtConcell( sid ) = cid;
			cells( cid ).rh = 0.0;
			cells( cid ).sid = sid;
			cells( cid ).length( 1 ) = 0.01;
			cells( cid ).origin( 1 ) = cells( cid ).length( 1 ) / 2.0 + runor;

			// Air Radiation Cell
			++cid;
			ExtRadcell( sid ) = cid;
			cells( cid ).rh = 0.0;
			cells( cid ).sid = sid;
			cells( cid ).length( 1 ) = 0.01;
			cells( cid ).origin( 1 ) = cells( cid ).length( 1 ) / 2.0 + runor;

			// Sky Cell
			++cid;
			ExtSkycell( sid ) = cid;
			cells( cid ).rh = 0.0;
			cells( cid ).sid = sid;
			cells( cid ).length( 1 ) = 0.01;
			cells( cid ).origin( 1 ) = cells( cid ).length( 1 ) / 2.0 + runor;

			// Ground Cell
			++cid;
			ExtGrncell( sid ) = cid;
			cells( cid ).rh = 0.0;
			cells( cid ).sid = sid;
			cells( cid ).length( 1 ) = 0.01;
			cells( cid ).origin( 1 ) = cells( cid ).length( 1 ) / 2.0 + runor;
			runor += cells( cid ).length( 1 );

			// External Virtual Cell
			++cid;
			Extcell( sid ) = cid;
			cells( cid ).rh = 0.0;
			cells( cid ).sid = sid;
			cells( cid ).length( 1 ) = 0.01;
			cells( cid ).origin( 1 ) = cells( cid ).length( 1 ) / 2.0 + runor;
			runor += cells( cid ).length( 1 );

			// Material Cells
			conid = Surface( sid ).Construction;
			for ( lid = 1; lid <= Construct( conid ).TotLayers; ++lid ) {
				matid = Construct( conid ).LayerPoint( lid );

				for ( did = 1; did <= Material( matid ).divs; ++did ) {
					++cid;

					cells( cid ).matid = matid;
					cells( cid ).sid = sid;

					cells( cid ).temp = Material( matid ).itemp;
					cells( cid ).tempp1 = Material( matid ).itemp;
					cells( cid ).tempp2 = Material( matid ).itemp;

					cells( cid ).rh = Material( matid ).irh;
					cells( cid ).rhp1 = Material( matid ).irh;
					cells( cid ).rhp2 = Material( matid ).irh;

					cells( cid ).density = Material( matid ).Density;
					cells( cid ).spech = Material( matid ).SpecHeat;

					// Make cells smaller near the surface
					cells( cid ).length( 1 ) = Material( matid ).Thickness * ( ( std::sin( Pi * ( -double( did ) / double( Material( matid ).divs ) ) - Pi / 2.0 ) / 2.0 ) - ( std::sin( Pi * ( -double( did - 1 ) / double( Material( matid ).divs ) ) - Pi / 2.0 ) / 2.0 ) );

					cells( cid ).origin( 1 ) = runor + cells( cid ).length( 1 ) / 2.0;
					runor += cells( cid ).length( 1 );

					cells( cid ).volume = cells( cid ).length( 1 ) * Surface( sid ).Area;

				}
			}

			// Interior Virtual Cell
			++cid;
			Intcell( sid ) = cid;
			cells( cid ).sid = sid;
			cells( cid ).rh = 0.0;
			cells( cid ).length( 1 ) = 0.01;
			cells( cid ).origin( 1 ) = cells( cid ).length( 1 ) / 2.0 + runor;
			runor += cells( cid ).length( 1 );

			// Air Convection Cell
			++cid;
			lastcell( sid ) = cid;
			IntConcell( sid ) = cid;
			cells( cid ).rh = 0.0;
			cells( cid ).sid = sid;
			cells( cid ).length( 1 ) = 0.01;
			cells( cid ).origin( 1 ) = cells( cid ).length( 1 ) / 2.0 + runor;

		}

		// Find adjacent cells.
		for ( cid1 = 1; cid1 <= TotCellsMax; ++cid1 ) {
			for ( cid2 = 1; cid2 <= TotCellsMax; ++cid2 ) {
				if ( ( cid1 != cid2 ) && ( cells( cid1 ).sid == cells( cid2 ).sid ) ) {
					high1 = cells( cid1 ).origin( 1 ) + cells( cid1 ).length( 1 ) / 2.0;
					low2 = cells( cid2 ).origin( 1 ) - cells( cid2 ).length( 1 ) / 2.0;
					if ( std::abs( low2 - high1 ) < adjdist ) {
						adj1 = 0;
						for ( ii = 1; ii <= adjmax; ++ii ) {
							++adj1;
							if ( cells( cid1 ).adjs( adj1 ) == -1 ) break;
						}
						adj2 = 0;
						for ( ii = 1; ii <= adjmax; ++ii ) {
							++adj2;
							if ( cells( cid2 ).adjs( adj2 ) == -1 ) break;
						}
						cells( cid1 ).adjs( adj1 ) = cid2;
						cells( cid2 ).adjs( adj2 ) = cid1;

						cells( cid1 ).adjsl( adj1 ) = adj2;
						cells( cid2 ).adjsl( adj2 ) = adj1;

						sid = cells( cid1 ).sid;
						cells( cid1 ).overlap( adj1 ) = Surface( sid ).Area;
						cells( cid2 ).overlap( adj2 ) = Surface( sid ).Area;
						cells( cid1 ).dist( adj1 ) = cells( cid1 ).length( 1 ) / 2.0;
						cells( cid2 ).dist( adj2 ) = cells( cid2 ).length( 1 ) / 2.0;
					}
				}
			}
		}

		// Reset surface virtual cell origins and volumes. Initialise report variables.
		gio::write( OutputFileInits, Format_1966 );
		gio::write( OutputFileInits, Format_1965 );
		//cCurrentModuleObject='MaterialProperty:HeatAndMoistureTransfer:*'
		for ( sid = 1; sid <= TotSurfaces; ++sid ) {
			if ( ! Surface( sid ).HeatTransSurf ) continue;
			if ( Surface( sid ).Class == SurfaceClass_Window ) continue;
			if ( Surface( sid ).HeatTransferAlgorithm != HeatTransferModel_HAMT ) continue;
			cells( Extcell( sid ) ).origin( 1 ) += cells( Extcell( sid ) ).length( 1 ) / 2.0;
			cells( Intcell( sid ) ).origin( 1 ) -= cells( Intcell( sid ) ).length( 1 ) / 2.0;
			cells( Extcell( sid ) ).volume = 0.0;
			cells( Intcell( sid ) ).volume = 0.0;
			watertot( sid ) = 0.0;
			surfrh( sid ) = 0.0;
			surfextrh( sid ) = 0.0;
			surftemp( sid ) = 0.0;
			surfexttemp( sid ) = 0.0;
			surfvp( sid ) = 0.0;
			SetupOutputVariable( "HAMT Surface Average Water Content Ratio [kg/kg]", watertot( sid ), "Zone", "State", Surface( sid ).Name );
			SetupOutputVariable( "HAMT Surface Inside Face Temperature [C]", surftemp( sid ), "Zone", "State", Surface( sid ).Name );
			SetupOutputVariable( "HAMT Surface Inside Face Relative Humidity [%]", surfrh( sid ), "Zone", "State", Surface( sid ).Name );
			SetupOutputVariable( "HAMT Surface Inside Face Vapor Pressure [Pa]", surfvp( sid ), "Zone", "State", Surface( sid ).Name );
			SetupOutputVariable( "HAMT Surface Outside Face Temperature [C]", surfexttemp( sid ), "Zone", "State", Surface( sid ).Name );
			SetupOutputVariable( "HAMT Surface Outside Face Relative Humidity [%]", surfextrh( sid ), "Zone", "State", Surface( sid ).Name );

			// write cell origins to initilisation output file
			conid = Surface( sid ).Construction;
			gio::write( OutputFileInits, "('HAMT cells, ',A,',',A,$)" ) << Surface( sid ).Name << Construct( conid ).Name;
			for ( int concell = 1, concell_end = Intcell( sid ) - Extcell( sid ) + 1; concell <= concell_end; ++concell ) {
				gio::write( OutputFileInits, "(',',i4,$)" ) << concell;
			}
			gio::write( OutputFileInits );
			gio::write( OutputFileInits, "('HAMT origins,',A,',',A,$)" ) << Surface( sid ).Name << Construct( conid ).Name;
			for ( int cellid = Extcell( sid ); cellid <= Intcell( sid ); ++cellid ) {
				gio::write( OutputFileInits, "(','f10.7,$)" ) << cells( cellid ).origin( 1 );
			}
			gio::write( OutputFileInits );

			for ( int cellid = Extcell( sid ), concell = 1; cellid <= Intcell( sid ); ++cellid, ++concell ) {
				SetupOutputVariable( "HAMT Surface Temperature Cell " + TrimSigDigits( concell ) + " [C]", cells( cellid ).temp, "Zone", "State", Surface( sid ).Name );
			}
			for ( int cellid = Extcell( sid ), concell = 1; cellid <= Intcell( sid ); ++cellid, ++concell ) {
				SetupOutputVariable( "HAMT Surface Water Content Cell " + TrimSigDigits( concell ) + " [kg/kg]", cells( cellid ).wreport, "Zone", "State", Surface( sid ).Name );
			}
			for ( int cellid = Extcell( sid ), concell = 1; cellid <= Intcell( sid ); ++cellid, ++concell ) {
				SetupOutputVariable( "HAMT Surface Relative Humidity Cell " + TrimSigDigits( concell ) + " [%]", cells( cellid ).rhp, "Zone", "State", Surface( sid ).Name );
			}
		}

		ScanForReports( "Constructions", DoReport, "Constructions" );
		if ( DoReport ) {

			gio::write( OutputFileInits, Format_108 );

			for ( MaterNum = 1; MaterNum <= TotMaterials; ++MaterNum ) {

				gio::write( OutputFileInits, Format_111 ) << Material( MaterNum ).Name << RoundSigDigits( NominalR( MaterNum ), 4 );

			}

		}

	}

	void
	CalcHeatBalHAMT(
		int const sid,
		Real64 & TempSurfInTmp,
		Real64 & TempSurfOutTmp
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Phillip Biddulph
		//       DATE WRITTEN   June 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To calculate the heat and moisture transfer through the surface

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::RoundSigDigits;
		using DataSurfaces::OtherSideCondModeledExt;
		using DataSurfaces::OSCM;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const HAMTExt( "HAMT-Ext" );
		static std::string const HAMTInt( "HAMT-Int" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 TempSurfInP;
		Real64 RhoIn;
		Real64 RhoOut;
		Real64 torsum;
		Real64 oorsum;
		Real64 phioosum;
		Real64 phiorsum;
		Real64 vpoosum;
		Real64 vporsum;
		Real64 rhr1;
		Real64 rhr2;
		Real64 wcap;
		Real64 thermr1;
		Real64 thermr2;
		Real64 tcap;
		Real64 qvp;
		Real64 vaporr1;
		Real64 vaporr2;
		Real64 vpdiff;
		Real64 sumtp1;
		Real64 tempmax;
		Real64 tempmin;

		int ii;
		int matid;
		int itter;
		int cid;
		//unused1208    INTEGER :: cid1
		int adj;
		int adjl;

		//    INTEGER, SAVE :: tempErrCount=0
		static int qvpErrCount( 0 );
		//    INTEGER, SAVE :: tempErrReport=0
		static int qvpErrReport( 0 );
		Real64 denominator;

		if ( BeginEnvrnFlag && MyEnvrnFlag( sid ) ) {
			cells( Extcell( sid ) ).rh = 0.0;
			cells( Extcell( sid ) ).rhp1 = 0.0;
			cells( Extcell( sid ) ).rhp2 = 0.0;

			cells( Extcell( sid ) ).temp = 10.0;
			cells( Extcell( sid ) ).tempp1 = 10.0;
			cells( Extcell( sid ) ).tempp2 = 10.0;

			cells( Intcell( sid ) ).rh = 0.0;
			cells( Intcell( sid ) ).rhp1 = 0.0;
			cells( Intcell( sid ) ).rhp2 = 0.0;

			cells( Intcell( sid ) ).temp = 10.0;
			cells( Intcell( sid ) ).tempp1 = 10.0;
			cells( Intcell( sid ) ).tempp2 = 10.0;

			for ( cid = Extcell( sid ) + 1; cid <= Intcell( sid ) - 1; ++cid ) {
				matid = cells( cid ).matid;

				cells( cid ).temp = Material( matid ).itemp;
				cells( cid ).tempp1 = Material( matid ).itemp;
				cells( cid ).tempp2 = Material( matid ).itemp;

				cells( cid ).rh = Material( matid ).irh;
				cells( cid ).rhp1 = Material( matid ).irh;
				cells( cid ).rhp2 = Material( matid ).irh;
			}
			MyEnvrnFlag( sid ) = false;
		}
		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( sid ) = true;
		}

		// Set all the boundary values
		cells( ExtRadcell( sid ) ).temp = TempOutsideAirFD( sid );
		cells( ExtConcell( sid ) ).temp = TempOutsideAirFD( sid );
		if ( Surface( sid ).ExtBoundCond == OtherSideCondModeledExt ) {
			//CR8046 switch modeled rad temp for sky temp.
			cells( ExtSkycell( sid ) ).temp = OSCM( Surface( sid ).OSCMPtr ).TRad;
			cells( Extcell( sid ) ).Qadds = 0.0; // eliminate incident shortwave on underlying surface
		} else {
			cells( ExtSkycell( sid ) ).temp = SkyTemp;

			cells( Extcell( sid ) ).Qadds = Surface( sid ).Area * QRadSWOutAbs( sid );

		}

		cells( ExtGrncell( sid ) ).temp = TempOutsideAirFD( sid );
		RhoOut = RhoVaporAirOut( sid );

		// Special case when the surface is an internal mass
		if ( Surface( sid ).ExtBoundCond == sid ) {
			cells( ExtConcell( sid ) ).temp = MAT( Surface( sid ).Zone );
			RhoOut = RhoVaporAirIn( sid );
		}

		RhoIn = RhoVaporAirIn( sid );

		cells( ExtRadcell( sid ) ).htc = HAirFD( sid );
		cells( ExtConcell( sid ) ).htc = HConvExtFD( sid );
		cells( ExtSkycell( sid ) ).htc = HSkyFD( sid );
		cells( ExtGrncell( sid ) ).htc = HGrndFD( sid );

		cells( IntConcell( sid ) ).temp = MAT( Surface( sid ).Zone );

		cells( IntConcell( sid ) ).htc = HConvInFD( sid );

		cells( Intcell( sid ) ).Qadds = Surface( sid ).Area * ( QRadSWInAbs( sid ) + NetLWRadToSurf( sid ) + QHTRadSysSurf( sid ) + QHWBaseboardSurf( sid ) + QSteamBaseboardSurf( sid ) + QElecBaseboardSurf( sid ) + QRadThermInAbs( sid ) );
		// Check, Is this per unit area or for the whole wall.
		//    cells(Intcell(sid))%Qadds=QRadSWInAbs(sid)+NetLWRadToSurf(sid)+QHtRadSysSurf(sid)+QRadThermInAbs(sid)

		cells( ExtConcell( sid ) ).rh = PsyRhFnTdbRhov( cells( ExtConcell( sid ) ).temp, RhoOut, HAMTExt );
		cells( IntConcell( sid ) ).rh = PsyRhFnTdbRhov( cells( IntConcell( sid ) ).temp, RhoIn, HAMTInt );

		if ( cells( ExtConcell( sid ) ).rh > rhmax ) {
			cells( ExtConcell( sid ) ).rh = rhmax;
		}
		if ( cells( IntConcell( sid ) ).rh > rhmax ) {
			cells( IntConcell( sid ) ).rh = rhmax;
		}

		// PDB August 2009 Start! Correction for when no vapour transfer coefficient have been defined.
		if ( extvtcflag( sid ) ) {
			cells( ExtConcell( sid ) ).vtc = extvtc( sid );
		} else {
			if ( cells( ExtConcell( sid ) ).rh > 0 ) {
				cells( ExtConcell( sid ) ).vtc = HMassConvExtFD( sid ) * RhoOut / ( PsyPsatFnTemp( TempOutsideAirFD( sid ) ) * cells( ExtConcell( sid ) ).rh );
			} else {
				cells( ExtConcell( sid ) ).vtc = 10000.0;
			}
		}

		if ( intvtcflag( sid ) ) {
			cells( IntConcell( sid ) ).vtc = intvtc( sid );
			HMassConvInFD( sid ) = cells( IntConcell( sid ) ).vtc * PsyPsatFnTemp( MAT( Surface( sid ).Zone ) ) * cells( IntConcell( sid ) ).rh / RhoIn;
		} else {
			if ( cells( IntConcell( sid ) ).rh > 0 ) {
				cells( IntConcell( sid ) ).vtc = HMassConvInFD( sid ) * RhoIn / ( PsyPsatFnTemp( MAT( Surface( sid ).Zone ) ) * cells( IntConcell( sid ) ).rh );
			} else {
				cells( IntConcell( sid ) ).vtc = 10000.0;
			}
		}
		// PDB August 2009 End

		// Initialise
		for ( cid = firstcell( sid ); cid <= Extcell( sid ) - 1; ++cid ) {
			cells( cid ).tempp1 = cells( cid ).temp;
			cells( cid ).tempp2 = cells( cid ).temp;
			cells( cid ).rhp1 = cells( cid ).rh;
			cells( cid ).rhp2 = cells( cid ).rh;
		}
		for ( cid = Intcell( sid ) + 1; cid <= lastcell( sid ); ++cid ) {
			cells( cid ).tempp1 = cells( cid ).temp;
			cells( cid ).tempp2 = cells( cid ).temp;
			cells( cid ).rhp1 = cells( cid ).rh;
			cells( cid ).rhp2 = cells( cid ).rh;
		}

		itter = 0;
		while ( true ) {
			++itter;
			// Update Moisture values

			for ( cid = firstcell( sid ); cid <= lastcell( sid ); ++cid ) {
				matid = cells( cid ).matid;
				cells( cid ).vp = RHtoVP( cells( cid ).rh, cells( cid ).temp );
				cells( cid ).vpp1 = RHtoVP( cells( cid ).rhp1, cells( cid ).tempp1 );
				cells( cid ).vpsat = PsyPsatFnTemp( cells( cid ).tempp1 );
				if ( matid > 0 ) {
					interp( Material( matid ).niso, Material( matid ).isorh, Material( matid ).isodata, cells( cid ).rhp1, cells( cid ).water, cells( cid ).dwdphi );
					if ( IsRain && rainswitch ) {
						interp( Material( matid ).nsuc, Material( matid ).sucwater, Material( matid ).sucdata, cells( cid ).water, cells( cid ).dw );
					} else {
						interp( Material( matid ).nred, Material( matid ).redwater, Material( matid ).reddata, cells( cid ).water, cells( cid ).dw );
					}
					interp( Material( matid ).nmu, Material( matid ).murh, Material( matid ).mudata, cells( cid ).rhp1, cells( cid ).mu );
					interp( Material( matid ).ntc, Material( matid ).tcwater, Material( matid ).tcdata, cells( cid ).water, cells( cid ).wthermalc );
				}
			}

			//Calculate Heat and Vapor resistances,
			for ( cid = Extcell( sid ); cid <= Intcell( sid ); ++cid ) {
				torsum = 0.0;
				oorsum = 0.0;
				vpdiff = 0.0;
				for ( ii = 1; ii <= adjmax; ++ii ) {
					adj = cells( cid ).adjs( ii );
					adjl = cells( cid ).adjsl( ii );
					if ( adj == -1 ) break;

					if ( cells( cid ).htc > 0 ) {
						thermr1 = 1.0 / ( cells( cid ).overlap( ii ) * cells( cid ).htc );
					} else if ( cells( cid ).matid > 0 ) {
						thermr1 = cells( cid ).dist( ii ) / ( cells( cid ).overlap( ii ) * cells( cid ).wthermalc );
					} else {
						thermr1 = 0.0;
					}

					if ( cells( cid ).vtc > 0 ) {
						vaporr1 = 1.0 / ( cells( cid ).overlap( ii ) * cells( cid ).vtc );
					} else if ( cells( cid ).matid > 0 ) {
						vaporr1 = ( cells( cid ).dist( ii ) * cells( cid ).mu ) / ( cells( cid ).overlap( ii ) * WVDC( cells( cid ).tempp1, OutBaroPress ) );
					} else {
						vaporr1 = 0.0;
					}

					if ( cells( adj ).htc > 0 ) {
						thermr2 = 1.0 / ( cells( cid ).overlap( ii ) * cells( adj ).htc );
					} else if ( cells( adj ).matid > 0 ) {
						thermr2 = cells( adj ).dist( adjl ) / ( cells( cid ).overlap( ii ) * cells( adj ).wthermalc );
					} else {
						thermr2 = 0.0;
					}

					if ( cells( adj ).vtc > 0 ) {
						vaporr2 = 1.0 / ( cells( cid ).overlap( ii ) * cells( adj ).vtc );
					} else if ( cells( adj ).matid > 0 ) {
						vaporr2 = cells( adj ).mu * cells( adj ).dist( adjl ) / ( WVDC( cells( adj ).tempp1, OutBaroPress ) * cells( cid ).overlap( ii ) );
					} else {
						vaporr2 = 0.0;
					}

					if ( thermr1 + thermr2 > 0 ) {
						oorsum += 1.0 / ( thermr1 + thermr2 );
						torsum += cells( adj ).tempp1 / ( thermr1 + thermr2 );
					}
					if ( vaporr1 + vaporr2 > 0 ) {
						vpdiff += ( cells( adj ).vp - cells( cid ).vp ) / ( vaporr1 + vaporr2 );
					}

				}

				// Calculate Heat Capacitance
				tcap = ( ( cells( cid ).density * cells( cid ).spech + cells( cid ).water * wspech ) * cells( cid ).volume );

				// calculate the latent heat if wanted and check for divergence
				qvp = 0.0;
				if ( ( cells( cid ).matid > 0 ) && ( latswitch ) ) {
					qvp = vpdiff * whv;
				}
				if ( std::abs( qvp ) > qvplim ) {
					if ( ! WarmupFlag ) {
						++qvpErrCount;
						if ( qvpErrCount < 16 ) {
							ShowWarningError( "HeatAndMoistureTransfer: Large Latent Heat for Surface " + Surface( sid ).Name );
						} else {
							ShowRecurringWarningErrorAtEnd( "HeatAndMoistureTransfer: Large Latent Heat Errors ", qvpErrReport );
						}
					}
					qvp = 0.0;
				}

				// Calculate the temperature for the next time step
				cells( cid ).tempp1 = ( torsum + qvp + cells( cid ).Qadds + ( tcap * cells( cid ).temp / deltat ) ) / ( oorsum + ( tcap / deltat ) );
			}

			// Check for silly temperatures
			tempmax = maxval( cells, &subcell::tempp1 );
			tempmin = minval( cells, &subcell::tempp1 );
			if ( tempmax > MaxSurfaceTempLimit ) {
				if ( ! WarmupFlag ) {
					if ( Surface( sid ).HighTempErrCount == 0 ) {
						ShowSevereMessage( "HAMT: Temperature (high) out of bounds (" + RoundSigDigits( tempmax, 2 ) + ") for surface=" + Surface( sid ).Name );
						ShowContinueErrorTimeStamp( "" );
					}
					ShowRecurringWarningErrorAtEnd( "HAMT: Temperature Temperature (high) out of bounds; Surface=" + Surface( sid ).Name, Surface( sid ).HighTempErrCount, tempmax, tempmax, _, "C", "C" );
				}
			}
			if ( tempmax > MaxSurfaceTempLimitBeforeFatal ) {
				if ( ! WarmupFlag ) {
					ShowSevereError( "HAMT: HAMT: Temperature (high) out of bounds ( " + RoundSigDigits( tempmax, 2 ) + ") for surface=" + Surface( sid ).Name );
					ShowContinueErrorTimeStamp( "" );
					ShowFatalError( "Program terminates due to preceding condition." );
				}
			}
			if ( tempmin < MinSurfaceTempLimit ) {
				if ( ! WarmupFlag ) {
					if ( Surface( sid ).HighTempErrCount == 0 ) {
						ShowSevereMessage( "HAMT: Temperature (low) out of bounds (" + RoundSigDigits( tempmin, 2 ) + ") for surface=" + Surface( sid ).Name );
						ShowContinueErrorTimeStamp( "" );
					}
					ShowRecurringWarningErrorAtEnd( "HAMT: Temperature Temperature (high) out of bounds; Surface=" + Surface( sid ).Name, Surface( sid ).HighTempErrCount, tempmin, tempmin, _, "C", "C" );
				}
			}
			if ( tempmin < MinSurfaceTempLimitBeforeFatal ) {
				if ( ! WarmupFlag ) {
					ShowSevereError( "HAMT: HAMT: Temperature (low) out of bounds ( " + RoundSigDigits( tempmin, 2 ) + ") for surface=" + Surface( sid ).Name );
					ShowContinueErrorTimeStamp( "" );
					ShowFatalError( "Program terminates due to preceding condition." );
				}
			}

			// Calculate the liquid and vapor resisitances
			for ( cid = Extcell( sid ); cid <= Intcell( sid ); ++cid ) {
				phioosum = 0.0;
				phiorsum = 0.0;
				vpoosum = 0.0;
				vporsum = 0.0;

				for ( ii = 1; ii <= adjmax; ++ii ) {
					adj = cells( cid ).adjs( ii );
					adjl = cells( cid ).adjsl( ii );
					if ( adj == -1 ) break;

					if ( cells( cid ).vtc > 0 ) {
						vaporr1 = 1.0 / ( cells( cid ).overlap( ii ) * cells( cid ).vtc );
					} else if ( cells( cid ).matid > 0 ) {
						vaporr1 = ( cells( cid ).dist( ii ) * cells( cid ).mu ) / ( cells( cid ).overlap( ii ) * WVDC( cells( cid ).tempp1, OutBaroPress ) );
					} else {
						vaporr1 = 0.0;
					}

					if ( cells( adj ).vtc > 0 ) {
						vaporr2 = 1.0 / ( cells( cid ).overlap( ii ) * cells( adj ).vtc );
					} else if ( cells( adj ).matid > 0 ) {
						vaporr2 = ( cells( adj ).dist( adjl ) * cells( adj ).mu ) / ( cells( cid ).overlap( ii ) * WVDC( cells( adj ).tempp1, OutBaroPress ) );
					} else {
						vaporr2 = 0.0;
					}
					if ( vaporr1 + vaporr2 > 0 ) {
						vpoosum += 1.0 / ( vaporr1 + vaporr2 );
						vporsum += ( cells( adj ).vpp1 / ( vaporr1 + vaporr2 ) );
					}

					if ( ( cells( cid ).dw > 0 ) && ( cells( cid ).dwdphi > 0 ) ) {
						rhr1 = cells( cid ).dist( ii ) / ( cells( cid ).overlap( ii ) * cells( cid ).dw * cells( cid ).dwdphi );
					} else {
						rhr1 = 0.0;
					}
					if ( ( cells( adj ).dw > 0 ) && ( cells( adj ).dwdphi > 0 ) ) {
						rhr2 = cells( adj ).dist( adjl ) / ( cells( cid ).overlap( ii ) * cells( adj ).dw * cells( adj ).dwdphi );
					} else {
						rhr2 = 0.0;
					}

					//             IF(rhr1+rhr2>0)THEN
					if ( rhr1 * rhr2 > 0 ) {
						phioosum += 1.0 / ( rhr1 + rhr2 );
						phiorsum += ( cells( adj ).rhp1 / ( rhr1 + rhr2 ) );
					}

				}

				// Moisture Capacitance
				if ( cells( cid ).dwdphi > 0.0 ) {
					wcap = cells( cid ).dwdphi * cells( cid ).volume;
				} else {
					wcap = 0.0;
				}

				// Calculate the RH for the next time step
				denominator = ( phioosum + vpoosum * cells( cid ).vpsat + wcap / deltat );
				if ( denominator != 0.0 ) {
					cells( cid ).rhp1 = ( phiorsum + vporsum + ( wcap * cells( cid ).rh ) / deltat ) / denominator;
				} else {
					ShowSevereError( "CalcHeatBalHAMT: demoninator in calculating RH is zero.  Check material properties for accuracy." );
					ShowContinueError( "...Problem occurs in Material=\"" + Material( cells( cid ).matid ).Name + "\"." );
					ShowFatalError( "Program terminates due to preceding condition." );
				}

				if ( cells( cid ).rhp1 > rhmax ) {
					cells( cid ).rhp1 = rhmax;
				}
			}

			//Check for convergence or too many itterations
			sumtp1 = 0.0;
			for ( cid = Extcell( sid ); cid <= Intcell( sid ); ++cid ) {
				if ( sumtp1 < std::abs( cells( cid ).tempp2 - cells( cid ).tempp1 ) ) {
					sumtp1 = std::abs( cells( cid ).tempp2 - cells( cid ).tempp1 );
				}
			}
			if ( sumtp1 < convt ) {
				break;
			}
			if ( itter > ittermax ) {
				break;
			}
			for ( cid = firstcell( sid ); cid <= lastcell( sid ); ++cid ) {
				cells( cid ).tempp2 = cells( cid ).tempp1;
				cells( cid ).rhp2 = cells( cid ).rhp1;
			}
		}

		// report back to CalcHeatBalanceInsideSurf
		TempSurfOutTmp = cells( Extcell( sid ) ).tempp1;
		TempSurfInTmp = cells( Intcell( sid ) ).tempp1;

		TempSurfInP = cells( Intcell( sid ) ).rhp1 * PsyPsatFnTemp( cells( Intcell( sid ) ).tempp1 );

		RhoVaporSurfIn( sid ) = TempSurfInP / ( 461.52 * ( MAT( Surface( sid ).Zone ) + KelvinConv ) );

	}

	void
	UpdateHeatBalHAMT( int const sid )
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Phillip Biddulph
		//       DATE WRITTEN   June 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// The zone heat balance equation has converged, so now the HAMT values are to be fixed
		// ready for the next itteration.
		// Fill all the report variables

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int cid;
		Real64 watermass;
		Real64 matmass;
		//unused1208    REAL(r64), SAVE :: InOld=0.0D0
		//unused1208    REAL(r64), SAVE :: OutOld=0.0D0

		//Update Temperatures and RHs. Calculate report variables
		matmass = 0.0;
		watermass = 0.0;
		for ( cid = firstcell( sid ); cid <= lastcell( sid ); ++cid ) {
			// fix HAMT values for this surface
			cells( cid ).temp = cells( cid ).tempp1;
			cells( cid ).rh = cells( cid ).rhp1;
			cells( cid ).rhp = cells( cid ).rh * 100.0;
			if ( cells( cid ).density > 0.0 ) {
				cells( cid ).wreport = cells( cid ).water / cells( cid ).density;
				watermass += ( cells( cid ).water * cells( cid ).volume );
				matmass += ( cells( cid ).density * cells( cid ).volume );
			}
		}

		watertot( sid ) = 0.0;
		if ( matmass > 0 ) watertot( sid ) = watermass / matmass;

		surfrh( sid ) = 100.0 * cells( Intcell( sid ) ).rh;
		surfextrh( sid ) = 100.0 * cells( Extcell( sid ) ).rh;
		surftemp( sid ) = cells( Intcell( sid ) ).temp;
		surfexttemp( sid ) = cells( Extcell( sid ) ).temp;
		surfvp( sid ) = RHtoVP( cells( Intcell( sid ) ).rh, cells( Intcell( sid ) ).temp );

	}

	void
	interp(
		int const ndata,
		Array1A< Real64 > const xx,
		Array1A< Real64 > const yy,
		Real64 const invalue,
		Real64 & outvalue,
		Optional< Real64 > outgrad
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Phillip Biddulph
		//       DATE WRITTEN   June 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To find a value by searching an array and interpolating between two coordinates
		// Also returns the gradient if required.

		// METHODOLOGY EMPLOYED:
		// Simple search

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		xx.dim( ndata );
		yy.dim( ndata );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Real64 xxlow;
		Real64 xxhigh;
		Real64 yylow;
		Real64 yyhigh;
		Real64 mygrad;
		int step;

		mygrad = 0.0;
		outvalue = 0.0;

		if ( ndata > 1 ) {
			xxlow = xx( 1 );
			yylow = yy( 1 );
			for ( step = 2; step <= ndata; ++step ) {
				xxhigh = xx( step );
				yyhigh = yy( step );
				if ( invalue <= xxhigh ) break;
				xxlow = xxhigh;
				yylow = yyhigh;
			}

			if ( xxhigh > xxlow ) {
				mygrad = ( yyhigh - yylow ) / ( xxhigh - xxlow );
				outvalue = ( invalue - xxlow ) * mygrad + yylow;
				// PDB August 2009 bug fix
			} else if ( std::abs( xxhigh - xxlow ) < 0.0000000001 ) {
				outvalue = yylow;
			}
		}

		if ( present( outgrad ) ) {
			// return gradient if required
			outgrad = mygrad;
		}
	}

	Real64
	RHtoVP(
		Real64 const RH,
		Real64 const Temperature
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Phillip Biddulph
		//       DATE WRITTEN   June 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Convert Relative Humidity and Temperature to Vapor Pressure

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 RHtoVP;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		Real64 VPSat;

		VPSat = PsyPsatFnTemp( Temperature );

		RHtoVP = RH * VPSat;

		return RHtoVP;
	}

	Real64
	WVDC(
		Real64 const Temperature,
		Real64 const ambp
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Phillip Biddulph
		//       DATE WRITTEN   June 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// To calculate the Water Vapor Diffusion Coefficient in air
		// using the temperature and ambient atmospheric pressor

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// K?zel, H.M. (1995) Simultaneous Heat and Moisture Transport in Building Components.
		// One- and two-dimensional calculation using simple parameters. IRB Verlag 1995

		// USE STATEMENTS:
		// na

		// Return value
		Real64 WVDC;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		WVDC = ( 2.e-7 * std::pow( Temperature + KelvinConv, 0.81 ) ) / ambp;

		return WVDC;
	}

	//                                 COPYRIGHT NOTICE

	//     Portions Copyright (c) University College London 2007.  All rights
	//     reserved.

	//     UCL LEGAL NOTICE
	//     Neither UCL, members of UCL nor any person or organisation acting on
	//     behalf of either:

	//     A. Makes any warranty of representation, express or implied with
	//        respect to the accuracy, completeness, or usefulness of the
	//        information contained in this program, including any warranty of
	//        merchantability or fitness of any purpose with respect to the
	//        program, or that the use of any information disclosed in this
	//        program may not infringe privately-owned rights, or

	//     B. Assumes any liability with respect to the use of, or for any and
	//        all damages resulting from the use of the program or any portion
	//        thereof or any information disclosed therein.

} // HeatBalanceHAMTManager

} // EnergyPlus
