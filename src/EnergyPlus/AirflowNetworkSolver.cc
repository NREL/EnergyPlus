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

// EnergyPlus Headers
#include <AirflowNetworkSolver.hh>
#include <CommandLineInterface.hh>
#include <DataAirflowNetwork.hh>
#include <DataAirLoop.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataStringGlobals.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSurfaces.hh>
#include <General.hh>
#include <OutputProcessor.hh>
#include <Psychrometrics.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

// define this variable to get new code, commenting should yield original
#define SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS

namespace AirflowNetworkSolver {

	// MODULE INFORMATION:
	//       AUTHOR         Lixing Gu, Don Shirey, and Muthusamy V. Swami
	//       DATE WRITTEN   Jul. 2005
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module is used to simulate airflows and pressures. The module is modified to
	// meet requirements of EnergyPLus based on AIRNET, developed by
	// National Institute of Standards and Technology (NIST).

	// METHODOLOGY EMPLOYED:
	// An airflow network approach is used. It consists of nodes connected by airflow elements.
	// The Newton's method is applied to solve a sparse matrix. When a new solver is available, this
	// module will be replaced or updated.

	// REFERENCES:
	// Walton, G. N., 1989, "AIRNET - A Computer Program for Building Airflow Network Modeling,"
	// NISTIR 89-4072, National Institute of Standards and Technology, Gaithersburg, Maryland

	// OTHER NOTES: none

	// USE STATEMENTS:

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::Pi;
	using DataGlobals::DegToRadians;
	using DataGlobals::KelvinConv;
	using DataGlobals::rTinyValue;
	using DataEnvironment::StdBaroPress;
	using DataEnvironment::OutBaroPress;
	using DataEnvironment::OutDryBulbTemp;
	using DataEnvironment::OutHumRat;
	using DataEnvironment::Latitude;
	using DataSurfaces::Surface;
	using Psychrometrics::PsyRhoAirFnPbTdbW;
	using Psychrometrics::PsyCpAirFnWTdb;
	using Psychrometrics::PsyHFnTdbW;
	using Psychrometrics::PsyRhoAirFnPbTdbW;
	using namespace DataAirflowNetwork;

	// Data
	int NetworkNumOfLinks( 0 );
	int NetworkNumOfNodes( 0 );

	int const NrInt( 20 ); // Number of intervals for a large opening

	static std::string const BlankString;

	// Common block AFEDAT
	Array1D< Real64 > AFECTL;
	Array1D< Real64 > AFLOW2;
	Array1D< Real64 > AFLOW;
	Array1D< Real64 > PS;
	Array1D< Real64 > PW;

	// Common block CONTRL
	Real64 PB( 0.0 );
	int LIST( 0 );

	// Common block ZONL
	Array1D< Real64 > RHOZ;
	Array1D< Real64 > SQRTDZ;
	Array1D< Real64 > VISCZ;
	Array1D< Real64 > SUMAF;
	Array1D< Real64 > TZ; // Temperature [C]
	Array1D< Real64 > WZ; // Humidity ratio [kg/kg]
	Array1D< Real64 > PZ; // Pressure [Pa]

	// Other array variables
	Array1D_int ID;
	Array1D_int IK;
	Array1D< Real64 > AD;
	Array1D< Real64 > AU;

#ifdef SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS
	Array1D_int newIK; // noel
	Array1D< Real64 > newAU; // noel
#endif

	//REAL(r64), ALLOCATABLE, DIMENSION(:) :: AL
	Array1D< Real64 > SUMF;
	int Unit11( 0 );
	int Unit21( 0 );

	// Large opening variables
	Array1D< Real64 > DpProf; // Differential pressure profile for Large Openings [Pa]
	Array1D< Real64 > RhoProfF; // Density profile in FROM zone [kg/m3]
	Array1D< Real64 > RhoProfT; // Density profile in TO zone [kg/m3]
	Array2D< Real64 > DpL; // Array of stack pressures in link

	// Functions

	void
	AllocateAirflowNetworkData()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Lixing Gu
		//       DATE WRITTEN   Aug. 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine allocates dynamic arrays for AirflowNetworkSolver.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int i;
		int j;
		int n;

		// Formats
		static gio::Fmt Format_900( "(1X,i2)" );
		static gio::Fmt Format_901( "(1X,2I4,4F9.4)" );
		static gio::Fmt Format_902( "(1X,2I4,4F9.4)" );
		static gio::Fmt Format_903( "(9X,4F9.4)" );
		static gio::Fmt Format_904( "(1X,2I4,1F9.4)" );
		static gio::Fmt Format_910( "(1X,I4,2(I4,F9.4),I4,2F4.1)" );

		// Assume a network to simulate multizone airflow is a subset of the network to simulate air distribution system.
		// Network array size is allocated based on the network of air distribution system.
		// If multizone airflow is simulated only, the array size is allocated based on the multizone network.
		// FLOW:
		NetworkNumOfLinks = AirflowNetworkNumOfLinks;
		NetworkNumOfNodes = AirflowNetworkNumOfNodes;

		AFECTL.allocate( NetworkNumOfLinks );
		AFLOW2.allocate( NetworkNumOfLinks );
		AFLOW.allocate( NetworkNumOfLinks );
		PW.allocate( NetworkNumOfLinks );
		PS.allocate( NetworkNumOfLinks );

		TZ.allocate( NetworkNumOfNodes );
		WZ.allocate( NetworkNumOfNodes );
		PZ.allocate( NetworkNumOfNodes );
		RHOZ.allocate( NetworkNumOfNodes );
		SQRTDZ.allocate( NetworkNumOfNodes );
		VISCZ.allocate( NetworkNumOfNodes );
		SUMAF.allocate( NetworkNumOfNodes );

		ID.allocate( NetworkNumOfNodes );
		IK.allocate( NetworkNumOfNodes + 1 );
#ifdef SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS
		newIK.allocate( NetworkNumOfNodes + 1 );
#endif
		AD.allocate( NetworkNumOfNodes );
		SUMF.allocate( NetworkNumOfNodes );

		n = 0;
		for ( i = 1; i <= AirflowNetworkNumOfLinks; ++i ) {
			j = AirflowNetworkCompData( AirflowNetworkLinkageData( i ).CompNum ).CompTypeNum;
			if ( j == CompTypeNum_DOP ) {
				++n;
			}
		}

		DpProf.allocate( n * ( NrInt + 2 ) );
		RhoProfF.allocate( n * ( NrInt + 2 ) );
		RhoProfT.allocate( n * ( NrInt + 2 ) );
		DpL.allocate( AirflowNetworkNumOfLinks, 2 );

		PB = 101325.0;
		//   LIST = 5
		LIST = 0;
		if ( LIST >= 1 ) {
			Unit21 = GetNewUnitNumber();
			gio::open( Unit21, DataStringGlobals::outputAdsFileName );
		}

		for ( n = 1; n <= NetworkNumOfNodes; ++n ) {
			ID( n ) = n;
		}
		for ( i = 1; i <= NetworkNumOfLinks; ++i ) {
			AFECTL( i ) = 1.0;
			AFLOW( i ) = 0.0;
			AFLOW2( i ) = 0.0;
		}

		for ( i = 1; i <= NetworkNumOfNodes; ++i ) {
			TZ( i ) = AirflowNetworkNodeSimu( i ).TZ;
			WZ( i ) = AirflowNetworkNodeSimu( i ).WZ;
			PZ( i ) = AirflowNetworkNodeSimu( i ).PZ;
		}

		// Assign linkage values
		for ( i = 1; i <= NetworkNumOfLinks; ++i ) {
			PW( i ) = 0.0;
		}
		// Write an ouput file used for AIRNET input
		if ( LIST >= 5 ) {
			Unit11 = GetNewUnitNumber();
			gio::open( Unit11, DataStringGlobals::eplusADSFileName );
			for ( i = 1; i <= NetworkNumOfNodes; ++i ) {
				gio::write( Unit11, Format_901 ) << i << AirflowNetworkNodeData( i ).NodeTypeNum << AirflowNetworkNodeData( i ).NodeHeight << TZ( i ) << PZ( i );
			}
			gio::write( Unit11, Format_900 ) << 0;
			for ( i = 1; i <= AirflowNetworkNumOfComps; ++i ) {
				j = AirflowNetworkCompData( i ).TypeNum;
				{ auto const SELECT_CASE_var( AirflowNetworkCompData( i ).CompTypeNum );
				if ( SELECT_CASE_var == CompTypeNum_PLR ) { //'PLR'  Power law component
					//              WRITE(Unit11,902) AirflowNetworkCompData(i)%CompNum,1,DisSysCompLeakData(j)%FlowCoef, &
					//                  DisSysCompLeakData(j)%FlowCoef,DisSysCompLeakData(j)%FlowCoef,DisSysCompLeakData(j)%FlowExpo
				} else if ( SELECT_CASE_var == CompTypeNum_SCR ) { //'SCR'  Surface crack component
					gio::write( Unit11, Format_902 ) << AirflowNetworkCompData( i ).CompNum << 1 << MultizoneSurfaceCrackData( j ).FlowCoef << MultizoneSurfaceCrackData( j ).FlowCoef << MultizoneSurfaceCrackData( j ).FlowCoef << MultizoneSurfaceCrackData( j ).FlowExpo;
				} else if ( SELECT_CASE_var == CompTypeNum_DWC ) { //'DWC' Duct component
					//              WRITE(Unit11,902) AirflowNetworkCompData(i)%CompNum,2,DisSysCompDuctData(j)%L,DisSysCompDuctData(j)%D, &
					//                               DisSysCompDuctData(j)%A,DisSysCompDuctData(j)%Rough
					//              WRITE(Unit11,903) DisSysCompDuctData(i)%TurDynCoef,DisSysCompDuctData(j)%LamFriCoef, &
					//                               DisSysCompDuctData(j)%LamFriCoef,DisSysCompDuctData(j)%InitLamCoef
					//           CASE (CompTypeNum_CVF) ! 'CVF' Constant volume fan component
					//              WRITE(Unit11,904) AirflowNetworkCompData(i)%CompNum,4,DisSysCompCVFData(j)%FlowRate
				} else if ( SELECT_CASE_var == CompTypeNum_EXF ) { // 'EXF' Zone exhaust fan
					gio::write( Unit11, Format_904 ) << AirflowNetworkCompData( i ).CompNum << 4 << MultizoneCompExhaustFanData( j ).FlowRate;
				} else {
				}}
			}
			gio::write( Unit11, Format_900 ) << 0;
			for ( i = 1; i <= NetworkNumOfLinks; ++i ) {
				gio::write( Unit11, Format_910 ) << i << AirflowNetworkLinkageData( i ).NodeNums( 1 ) << AirflowNetworkLinkageData( i ).NodeHeights( 1 ) << AirflowNetworkLinkageData( i ).NodeNums( 2 ) << AirflowNetworkLinkageData( i ).NodeHeights( 2 ) << AirflowNetworkLinkageData( i ).CompNum << 0 << 0;
			}
			gio::write( Unit11, Format_900 ) << 0;
		}

		SETSKY();

		//SETSKY figures out the IK stuff -- which is why E+ doesn't allocate AU until here
#ifdef SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS
		//   ! only printing to screen, can be commented
		//   print*, "SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS is defined"
		//   write(*,'(2(a,i8))') "AllocateAirflowNetworkData: after SETSKY, allocating AU.  NetworkNumOfNodes=", &
		//        NetworkNumOfNodes, " IK(NetworkNumOfNodes+1)= NNZE=", IK(NetworkNumOfNodes+1)
		//   print*, " NetworkNumOfLinks=", NetworkNumOfLinks
		// allocate same size as others -- this will be maximum  !noel
		newAU.allocate( IK( NetworkNumOfNodes + 1 ) );
#endif

		// noel, GNU says the AU is indexed above its upper bound
		//ALLOCATE(AU(IK(NetworkNumOfNodes+1)-1))
		AU.allocate( IK( NetworkNumOfNodes + 1 ) );

	}

	void
	InitAirflowNetworkData()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Lixing Gu
		//       DATE WRITTEN   Aug. 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine initializes variables for AirflowNetworkSolver.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int i;
		int n;

		// FLOW:
		for ( n = 1; n <= NetworkNumOfNodes; ++n ) {
			ID( n ) = n;
		}
		for ( i = 1; i <= NetworkNumOfLinks; ++i ) {
			AFECTL( i ) = 1.0;
			AFLOW( i ) = 0.0;
			AFLOW2( i ) = 0.0;
		}

		for ( i = 1; i <= NetworkNumOfNodes; ++i ) {
			TZ( i ) = AirflowNetworkNodeSimu( i ).TZ;
			WZ( i ) = AirflowNetworkNodeSimu( i ).WZ;
			PZ( i ) = AirflowNetworkNodeSimu( i ).PZ;
		}

	}

	void
	SETSKY()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         George Walton
		//       DATE WRITTEN   1998
		//       MODIFIED       Feb. 2006 (L. Gu) to meet requirements of AirflowNetwork
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine sets up the "IK" array describing the sparse matrix [A] in skyline
		//     form by using the location matrix.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// AIRNET

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// IK(K) - pointer to the top of column/row "K".
		int i;
		int j;
		int k;
		int L;
		int M;
		int N1;
		int N2;

		// FLOW:
		// Initialize "IK".
		for ( i = 1; i <= NetworkNumOfNodes + 1; ++i ) {
			IK( i ) = 0;
		}
		// Determine column heights.
		for ( M = 1; M <= NetworkNumOfLinks; ++M ) {
			j = AirflowNetworkLinkageData( M ).NodeNums( 2 );
			if ( j == 0 ) continue;
			L = ID( j );
			i = AirflowNetworkLinkageData( M ).NodeNums( 1 );
			k = ID( i );
			N1 = std::abs( L - k );
			N2 = max( k, L );
			IK( N2 ) = max( IK( N2 ), N1 );
		}
		// Convert heights to column addresses.
		j = IK( 1 );
		IK( 1 ) = 1;
		for ( k = 1; k <= NetworkNumOfNodes; ++k ) {
			i = IK( k + 1 );
			IK( k + 1 ) = IK( k ) + j;
			j = i;
		}

	}

	void
	AIRMOV()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         George Walton
		//       DATE WRITTEN   Extracted from AIRNET
		//       MODIFIED       Lixing Gu, 2/1/04
		//                      Revised the subroutine to meet E+ needs
		//       MODIFIED       Lixing Gu, 6/8/05
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is a driver for AIRNET to calculate nodal pressures and linkage airflows

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int i;
		int M;
		int n;
		int ITER;

		// Formats
		static gio::Fmt Format_900( "(,/,11X,'i    n    m       DP',12x,'F1',12X,'F2')" );
		static gio::Fmt Format_901( "(1X,A6,3I5,3F14.6)" );
		static gio::Fmt Format_902( "(,/,11X,'n       P',12x,'sumF')" );
		static gio::Fmt Format_903( "(1X,A6,I5,3F14.6)" );
		static gio::Fmt Format_907( "(,/,' CPU seconds for ',A,F12.3)" );

		// FLOW:
		// Compute zone air properties.
		for ( n = 1; n <= NetworkNumOfNodes; ++n ) {
			RHOZ( n ) = PsyRhoAirFnPbTdbW( StdBaroPress + PZ( n ), TZ( n ), WZ( n ) );
			if ( AirflowNetworkNodeData( n ).ExtNodeNum > 0 ) {
				RHOZ( n ) = PsyRhoAirFnPbTdbW( StdBaroPress + PZ( n ), OutDryBulbTemp, OutHumRat );
				TZ( n ) = OutDryBulbTemp;
				WZ( n ) = OutHumRat;
			}
			SQRTDZ( n ) = std::sqrt( RHOZ( n ) );
			VISCZ( n ) = 1.71432e-5 + 4.828e-8 * TZ( n );
			if ( LIST >= 2 ) gio::write( Unit21, Format_903 ) << "D,V:" << n << RHOZ( n ) << VISCZ( n );
		}
		if ( AirflowNetworkSimu.InitFlag == 0 ) {
			for ( n = 1; n <= NetworkNumOfNodes; ++n ) {
				if ( AirflowNetworkNodeData( n ).NodeTypeNum == 0 ) PZ( n ) = 0.0;
			}
		}
		// Compute stack pressures.
		for ( i = 1; i <= NetworkNumOfLinks; ++i ) {
			n = AirflowNetworkLinkageData( i ).NodeNums( 1 );
			M = AirflowNetworkLinkageData( i ).NodeNums( 2 );
			if ( AFLOW( i ) > 0.0 ) {
				PS( i ) = 9.80 * ( RHOZ( n ) * ( AirflowNetworkNodeData( n ).NodeHeight - AirflowNetworkNodeData( M ).NodeHeight ) + AirflowNetworkLinkageData( i ).NodeHeights( 2 ) * ( RHOZ( M ) - RHOZ( n ) ) );
			} else if ( AFLOW( i ) < 0.0 ) {
				PS( i ) = 9.80 * ( RHOZ( M ) * ( AirflowNetworkNodeData( n ).NodeHeight - AirflowNetworkNodeData( M ).NodeHeight ) + AirflowNetworkLinkageData( i ).NodeHeights( 1 ) * ( RHOZ( M ) - RHOZ( n ) ) );
			} else {
				PS( i ) = 4.90 * ( ( RHOZ( n ) + RHOZ( M ) ) * ( AirflowNetworkNodeData( n ).NodeHeight - AirflowNetworkNodeData( M ).NodeHeight ) + ( AirflowNetworkLinkageData( i ).NodeHeights( 1 ) + AirflowNetworkLinkageData( i ).NodeHeights( 2 ) ) * ( RHOZ( M ) - RHOZ( n ) ) );
			}
		}

		// Calculate pressure field in a large opening
		PStack();

		SOLVZP( IK, AD, AU, ITER );

		// Report element flows and zone pressures.
		for ( n = 1; n <= NetworkNumOfNodes; ++n ) {
			SUMAF( n ) = 0.0;
		}
		if ( LIST >= 1 ) gio::write( Unit21, Format_900 );
		for ( i = 1; i <= NetworkNumOfLinks; ++i ) {
			n = AirflowNetworkLinkageData( i ).NodeNums( 1 );
			M = AirflowNetworkLinkageData( i ).NodeNums( 2 );
			if ( LIST >= 1 ) {
				gio::write( Unit21, Format_901 ) << "Flow: " << i << n << M << AirflowNetworkLinkSimu( i ).DP << AFLOW( i ) << AFLOW2( i );
			}
			if ( AirflowNetworkCompData( AirflowNetworkLinkageData( i ).CompNum ).CompTypeNum == CompTypeNum_HOP ) {
				SUMAF( n ) = SUMAF( n ) - AFLOW( i );
				SUMAF( M ) += AFLOW( i );
			} else {
				SUMAF( n ) = SUMAF( n ) - AFLOW( i ) - AFLOW2( i );
				SUMAF( M ) += AFLOW( i ) + AFLOW2( i );
			}
		}
		for ( n = 1; n <= NetworkNumOfNodes; ++n ) {
			if ( LIST >= 1 ) gio::write( Unit21, Format_903 ) << "Room: " << n << PZ( n ) << SUMAF( n ) << TZ( n );
		}

		for ( i = 1; i <= NetworkNumOfLinks; ++i ) {
			if ( AFLOW2( i ) != 0.0 ) {

			}
			if ( AFLOW( i ) > 0.0 ) {
				AirflowNetworkLinkSimu( i ).FLOW = AFLOW( i );
				AirflowNetworkLinkSimu( i ).FLOW2 = 0.0;
			} else {
				AirflowNetworkLinkSimu( i ).FLOW = 0.0;
				AirflowNetworkLinkSimu( i ).FLOW2 = -AFLOW( i );
			}
			if ( AirflowNetworkCompData( AirflowNetworkLinkageData( i ).CompNum ).CompTypeNum == CompTypeNum_HOP ) {
				if ( AFLOW( i ) > 0.0 ) {
					AirflowNetworkLinkSimu( i ).FLOW = AFLOW( i ) + AFLOW2( i );
					AirflowNetworkLinkSimu( i ).FLOW2 = AFLOW2( i );
				} else {
					AirflowNetworkLinkSimu( i ).FLOW = AFLOW2( i );
					AirflowNetworkLinkSimu( i ).FLOW2 = -AFLOW( i ) + AFLOW2( i );
				}
			}
			if ( AirflowNetworkLinkageData( i ).DetOpenNum > 0 ) {
				if ( AFLOW2( i ) != 0.0 ) {
					AirflowNetworkLinkSimu( i ).FLOW = AFLOW( i ) + AFLOW2( i );
					AirflowNetworkLinkSimu( i ).FLOW2 = AFLOW2( i );
				}
			}
			if ( AirflowNetworkCompData( AirflowNetworkLinkageData( i ).CompNum ).CompTypeNum == CompTypeNum_SOP && AFLOW2( i ) != 0.0 ) {
				if ( AFLOW( i ) >= 0.0 ) {
					AirflowNetworkLinkSimu( i ).FLOW = AFLOW( i );
					AirflowNetworkLinkSimu( i ).FLOW2 = std::abs( AFLOW2( i ) );
				} else {
					AirflowNetworkLinkSimu( i ).FLOW = std::abs( AFLOW2( i ) );
					AirflowNetworkLinkSimu( i ).FLOW2 = -AFLOW( i );
				}
			}
		}

		for ( i = 1; i <= NetworkNumOfNodes; ++i ) {
			AirflowNetworkNodeSimu( i ).PZ = PZ( i );
		}

	}

	void
	SOLVZP(
		Array1A_int IK, // pointer to the top of column/row "K"
		Array1A< Real64 > AD, // the main diagonal of [A] before and after factoring
		Array1A< Real64 > AU, // the upper triangle of [A] before and after factoring
		int & ITER // number of iterations
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         George Walton
		//       DATE WRITTEN   Extracted from AIRNET
		//       MODIFIED       Lixing Gu, 2/1/04
		//                      Revised the subroutine to meet E+ needs
		//       MODIFIED       Lixing Gu, 6/8/05
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine solves zone pressures by modified Newton-Raphson iteration

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::RoundSigDigits;

		// Argument array dimensioning
		IK.dim( NetworkNumOfNodes+1 );
		AD.dim( NetworkNumOfNodes );
		AU.dim( IK(NetworkNumOfNodes+1) );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//noel GNU says AU is being indexed beyound bounds
		//REAL(r64), INTENT(INOUT) :: AU(IK(NetworkNumOfNodes+1)-1) ! the upper triangle of [A] before and after factoring

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt fmtLD( "*" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		//     NNZE   - number of nonzero entries in the "AU" array.
		//     LFLAG   - if = 1, use laminar relationship (initialization).
		//     I       - element number.
		//     N       - number of node/zone 1.
		//     M       - number of node/zone 2.
		//     F       - flows through the element (kg/s).
		//     DF      - partial derivatives:  DF/DP.
		//     NF      - number of flows, 1 or 2.
		//     SUMF    - sum of flows into node/zone.
		//     CCF     - current pressure correction (Pa).
		//     PCF     - previous pressure correction (Pa).
		//     CEF     - convergence enhancement factor.
		int n;
		int NNZE;
		int NSYM;
		int LFLAG;
		int CONVG;
		int ACCEL;
		Array1D< Real64 > PCF( NetworkNumOfNodes );
		Array1D< Real64 > CEF( NetworkNumOfNodes );
		Real64 C;
		Real64 SSUMF;
		Real64 SSUMAF;
		Real64 ACC0;
		Real64 ACC1;
		Array1D< Real64 > CCF( NetworkNumOfNodes );

		// Formats
		static gio::Fmt Format_901( "(A5,I3,2E14.6,0P,F8.4,F24.14)" );

		// FLOW:
		ACC1 = 0.0;
		ACCEL = 0;
		NSYM = 0;
		NNZE = IK( NetworkNumOfNodes + 1 ) - 1;
		if ( LIST >= 2 ) gio::write( Unit21, fmtLD ) << "Initialization" << NetworkNumOfNodes << NetworkNumOfLinks << NNZE;
		ITER = 0;

		for ( n = 1; n <= NetworkNumOfNodes; ++n ) {
			PCF( n ) = 0.0;
			CEF( n ) = 0.0;
		}

		if ( AirflowNetworkSimu.InitFlag != 1 ) {
			// Initialize node/zone pressure values by assuming only linear relationship between
			// airflows and pressure drops.
			LFLAG = 1;
			FILJAC( NNZE, LFLAG );
			for ( n = 1; n <= NetworkNumOfNodes; ++n ) {
				if ( AirflowNetworkNodeData( n ).NodeTypeNum == 0 ) PZ( n ) = SUMF( n );
			}
			// Data dump.
			if ( LIST >= 3 ) {
				DUMPVD( "AD:", AD, NetworkNumOfNodes, Unit21 );
				DUMPVD( "AU:", AU, NNZE, Unit21 );
				DUMPVR( "AF:", SUMF, NetworkNumOfNodes, Unit21 );
			}
			// Solve linear system for approximate PZ.
#ifdef SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS
			FACSKY( newAU, AD, newAU, newIK, NetworkNumOfNodes, NSYM ); //noel
			SLVSKY( newAU, AD, newAU, PZ, newIK, NetworkNumOfNodes, NSYM ); //noel
#else
			FACSKY( AU, AD, AU, IK, NetworkNumOfNodes, NSYM );
			SLVSKY( AU, AD, AU, PZ, IK, NetworkNumOfNodes, NSYM );
#endif
			if ( LIST >= 2 ) DUMPVD( "PZ:", PZ, NetworkNumOfNodes, Unit21 );
		}
		// Solve nonlinear airflow network equations by modified Newton's method.

		while ( ITER < AirflowNetworkSimu.MaxIteration ) {
			LFLAG = 0;
			++ITER;
			if ( LIST >= 2 ) gio::write( Unit21, fmtLD ) << "Begin iteration " << ITER;
			// Set up the Jacobian matrix.
			FILJAC( NNZE, LFLAG );
			// Data dump.
			if ( LIST >= 3 ) {
				DUMPVR( "SUMF:", SUMF, NetworkNumOfNodes, Unit21 );
				DUMPVR( "SUMAF:", SUMAF, NetworkNumOfNodes, Unit21 );
			}
			// Check convergence.
			CONVG = 1;
			SSUMF = 0.0;
			SSUMAF = 0.0;
			for ( n = 1; n <= NetworkNumOfNodes; ++n ) {
				SSUMF += std::abs( SUMF( n ) );
				SSUMAF += SUMAF( n );
				if ( CONVG == 1 ) {
					if ( std::abs( SUMF( n ) ) <= AirflowNetworkSimu.AbsTol ) continue;
					if ( std::abs( SUMF( n ) / SUMAF( n ) ) > AirflowNetworkSimu.RelTol ) CONVG = 0;
				}
			}
			ACC0 = ACC1;
			if ( SSUMAF > 0.0 ) ACC1 = SSUMF / SSUMAF;
			if ( CONVG == 1 && ITER > 1 ) return;
			if ( ITER >= AirflowNetworkSimu.MaxIteration ) break;
			// Data dump.
			if ( LIST >= 3 ) {
				DUMPVD( "AD:", AD, NetworkNumOfNodes, Unit21 );
				DUMPVD( "AU:", AU, NNZE, Unit21 );
			}
			// Solve AA * CCF = SUMF.
			for ( n = 1; n <= NetworkNumOfNodes; ++n ) {
				CCF( n ) = SUMF( n );
			}
#ifdef SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS
			FACSKY( newAU, AD, newAU, newIK, NetworkNumOfNodes, NSYM ); //noel
			SLVSKY( newAU, AD, newAU, CCF, newIK, NetworkNumOfNodes, NSYM ); //noel
#else
			FACSKY( AU, AD, AU, IK, NetworkNumOfNodes, NSYM );
			SLVSKY( AU, AD, AU, CCF, IK, NetworkNumOfNodes, NSYM );
#endif
			// Revise PZ (Steffensen iteration on the N-R correction factors to handle oscillating corrections).
			if ( ACCEL == 1 ) {
				ACCEL = 0;
			} else {
				if ( ITER > 2 && ACC1 > 0.5 * ACC0 ) ACCEL = 1;
			}
			for ( n = 1; n <= NetworkNumOfNodes; ++n ) {
				if ( AirflowNetworkNodeData( n ).NodeTypeNum == 1 ) continue;
				CEF( n ) = 1.0;
				if ( ACCEL == 1 ) {
					C = CCF( n ) / PCF( n );
					if ( C < AirflowNetworkSimu.ConvLimit ) CEF( n ) = 1.0 / ( 1.0 - C );
					C = CCF( n ) * CEF( n );
				} else {
					//            IF (CCF(N) .EQ. 0.0d0) CCF(N)=TINY(CCF(N))  ! 1.0E-40
					if ( CCF( n ) == 0.0 ) CCF( n ) = rTinyValue; // 1.0E-40 (Epsilon)
					PCF( n ) = CCF( n );
					C = CCF( n );
				}
				if ( std::abs( C ) > AirflowNetworkSimu.MaxPressure ) {
					CEF( n ) *= AirflowNetworkSimu.MaxPressure / std::abs( C );
					PZ( n ) -= CCF( n ) * CEF( n );
				} else {
					PZ( n ) -= C;
				}
			}
			// Data revision dump.
			if ( LIST >= 2 ) {
				for ( n = 1; n <= NetworkNumOfNodes; ++n ) {
					if ( AirflowNetworkNodeData( n ).NodeTypeNum == 0 ) gio::write( Unit21, Format_901 ) << " Rev:" << n << SUMF( n ) << CCF( n ) << CEF( n ) << PZ( n );
				}
			}
		}

		// Error termination.
		ShowSevereError( "Too many iterations (SOLVZP) in Airflow Network simulation" );
		++AirflowNetworkSimu.ExtLargeOpeningErrCount;
		if ( AirflowNetworkSimu.ExtLargeOpeningErrCount < 2 ) {
			ShowWarningError( "AirflowNetwork: SOLVER, Changing values for initialization flag, Relative airflow convergence, Absolute airflow convergence, Convergence acceleration limit or Maximum Iteration Number may solve the problem." );
			ShowContinueErrorTimeStamp( "" );
			ShowContinueError( "..Iterations=" + RoundSigDigits( ITER ) + ", Max allowed=" + RoundSigDigits( AirflowNetworkSimu.MaxIteration ) );
			ShowFatalError( "AirflowNetwork: SOLVER, The previous error causes termination." );
		} else {
			ShowRecurringWarningErrorAtEnd( "AirFlowNetwork: Too many iterations (SOLVZP) in AirflowNetwork simulation continues.", AirflowNetworkSimu.ExtLargeOpeningErrIndex );
		}

	}

	void
	FILJAC(
		int const NNZE, // number of nonzero entries in the "AU" array.
		int const LFLAG // if = 1, use laminar relationship (initialization).
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         George Walton
		//       DATE WRITTEN   Extracted from AIRNET
		//       MODIFIED       Lixing Gu, 2/1/04
		//                      Revised the subroutine to meet E+ needs
		//       MODIFIED       Lixing Gu, 6/8/05
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine creates matrices for solution of flows

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

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// I       - component number.
		// N       - number of node/zone 1.
		// M       - number of node/zone 2.
		// F       - flows through the element (kg/s).
		// DF      - partial derivatives:  DF/DP.
		// NF      - number of flows, 1 or 2.
		int i;
		int j;
		int M;
		int n;
		int FLAG;
		int NF;
#ifdef SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS
		int LHK; // noel
		int JHK;
		int JHK1;
		int newsum;
		int newh;
		int ispan;
		int thisIK;
		bool allZero; // noel
#endif
		Array1D< Real64 > X( 4 );
		Real64 DP;
		Array1D< Real64 > F( 2 );
		Array1D< Real64 > DF( 2 );

		// Formats
		static gio::Fmt Format_901( "(A5,3I3,4E16.7)" );

		// FLOW:
		for ( n = 1; n <= NetworkNumOfNodes; ++n ) {
			SUMF( n ) = 0.0;
			SUMAF( n ) = 0.0;
			if ( AirflowNetworkNodeData( n ).NodeTypeNum == 1 ) {
				AD( n ) = 1.0;
			} else {
				AD( n ) = 0.0;
			}
		}
		for ( n = 1; n <= NNZE; ++n ) {
			AU( n ) = 0.0;
		}
		//                              Set up the Jacobian matrix.
		for ( i = 1; i <= NetworkNumOfLinks; ++i ) {
			n = AirflowNetworkLinkageData( i ).NodeNums( 1 );
			M = AirflowNetworkLinkageData( i ).NodeNums( 2 );
			//!!! Check array of DP. DpL is used for multizone air flow calculation only
			//!!! and is not for forced air calculation
			if ( i > NumOfLinksMultiZone ) {
				DP = PZ( n ) - PZ( M ) + PS( i ) + PW( i );
			} else {
				DP = PZ( n ) - PZ( M ) + DpL( i, 1 ) + PW( i );
			}
			if ( LIST >= 4 ) gio::write( Unit21, Format_901 ) << "PS:" << i << n << M << PS( i ) << PW( i ) << AirflowNetworkLinkSimu( i ).DP;
			j = AirflowNetworkLinkageData( i ).CompNum;
			{ auto const SELECT_CASE_var( AirflowNetworkCompData( j ).CompTypeNum );
			if ( SELECT_CASE_var == CompTypeNum_PLR ) { // Distribution system crack component
				AFEPLR( j, LFLAG, DP, i, n, M, F, DF, NF );
			} else if ( SELECT_CASE_var == CompTypeNum_DWC ) { // Distribution system duct component
				AFEDWC( j, LFLAG, DP, i, n, M, F, DF, NF );
			} else if ( SELECT_CASE_var == CompTypeNum_CVF ) { // Distribution system constant volume fan component
				AFECFR( j, LFLAG, DP, i, n, M, F, DF, NF );
			} else if ( SELECT_CASE_var == CompTypeNum_FAN ) { // Distribution system detailed fan component
				AFEFAN( j, LFLAG, DP, i, n, M, F, DF, NF );
				//           Case (CompTypeNum_CPF) ! not currently used in EnergyPlus code -- left for compatibility with AirNet
				//              CALL AFECPF(J,LFLAG,DP,I,N,M,F,DF,NF)
			} else if ( SELECT_CASE_var == CompTypeNum_DMP ) { // Distribution system damper component
				AFEDMP( j, LFLAG, DP, i, n, M, F, DF, NF );
			} else if ( SELECT_CASE_var == CompTypeNum_ELR ) { // Distribution system effective leakage ratio component
				AFEELR( j, LFLAG, DP, i, n, M, F, DF, NF );
			} else if ( SELECT_CASE_var == CompTypeNum_CPD ) { // Distribution system constant pressure drop component
				AFECPD( j, LFLAG, DP, i, n, M, F, DF, NF );
			} else if ( SELECT_CASE_var == CompTypeNum_DOP ) { // Detailed opening
				AFEDOP( j, LFLAG, DP, i, n, M, F, DF, NF );
			} else if ( SELECT_CASE_var == CompTypeNum_SOP ) { // Simple opening
				AFESOP( j, LFLAG, DP, i, n, M, F, DF, NF );
			} else if ( SELECT_CASE_var == CompTypeNum_SCR ) { // Surface crack component
				AFESCR( j, LFLAG, DP, i, n, M, F, DF, NF );
			} else if ( SELECT_CASE_var == CompTypeNum_SEL ) { // Surface effective leakage ratio component
				AFESEL( j, LFLAG, DP, i, n, M, F, DF, NF );
			} else if ( SELECT_CASE_var == CompTypeNum_COI ) { // Distribution system coil component
				AFECOI( j, LFLAG, DP, i, n, M, F, DF, NF );
			} else if ( SELECT_CASE_var == CompTypeNum_TMU ) { // Distribution system terminal unit component
				AFETMU( j, LFLAG, DP, i, n, M, F, DF, NF );
			} else if ( SELECT_CASE_var == CompTypeNum_EXF ) { // Exhaust fan component
				AFEEXF( j, LFLAG, DP, i, n, M, F, DF, NF );
			} else if ( SELECT_CASE_var == CompTypeNum_HEX ) { // Distribution system heat exchanger component
				AFEHEX( j, LFLAG, DP, i, n, M, F, DF, NF );
			} else if ( SELECT_CASE_var == CompTypeNum_HOP ) { // Horizontal opening
				AFEHOP( j, LFLAG, DP, i, n, M, F, DF, NF );
			} else {
				continue;
			}}
			AirflowNetworkLinkSimu( i ).DP = DP;
			AFLOW( i ) = F( 1 );
			AFLOW2( i ) = 0.0;
			if ( AirflowNetworkCompData( j ).CompTypeNum == CompTypeNum_DOP ) {
				AFLOW2( i ) = F( 2 );
			}
			if ( LIST >= 3 ) gio::write( Unit21, Format_901 ) << " NRi:" << i << n << M << AirflowNetworkLinkSimu( i ).DP << F( 1 ) << DF( 1 );
			FLAG = 1;
			if ( AirflowNetworkNodeData( n ).NodeTypeNum == 0 ) {
				++FLAG;
				X( 1 ) = DF( 1 );
				X( 2 ) = -DF( 1 );
				SUMF( n ) += F( 1 );
				SUMAF( n ) += std::abs( F( 1 ) );
			}
			if ( AirflowNetworkNodeData( M ).NodeTypeNum == 0 ) {
				FLAG += 2;
				X( 4 ) = DF( 1 );
				X( 3 ) = -DF( 1 );
				SUMF( M ) -= F( 1 );
				SUMAF( M ) += std::abs( F( 1 ) );
			}
			if ( FLAG != 1 ) FILSKY( X, AirflowNetworkLinkageData( i ).NodeNums, IK, AU, AD, FLAG );
			if ( NF == 1 ) continue;
			AFLOW2( i ) = F( 2 );
			if ( LIST >= 3 ) gio::write( Unit21, Format_901 ) << " NRj:" << i << n << M << AirflowNetworkLinkSimu( i ).DP << F( 2 ) << DF( 2 );
			FLAG = 1;
			if ( AirflowNetworkNodeData( n ).NodeTypeNum == 0 ) {
				++FLAG;
				X( 1 ) = DF( 2 );
				X( 2 ) = -DF( 2 );
				SUMF( n ) += F( 2 );
				SUMAF( n ) += std::abs( F( 2 ) );
			}
			if ( AirflowNetworkNodeData( M ).NodeTypeNum == 0 ) {
				FLAG += 2;
				X( 4 ) = DF( 2 );
				X( 3 ) = -DF( 2 );
				SUMF( M ) -= F( 2 );
				SUMAF( M ) += std::abs( F( 2 ) );
			}
			if ( FLAG != 1 ) FILSKY( X, AirflowNetworkLinkageData( i ).NodeNums, IK, AU, AD, FLAG );
		}

#ifdef SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS

		// After the matrix values have been set, we can look at them and see if any columns are filled with zeros.
		// If they are, let's remove them from the matrix -- but only for the purposes of doing the solve.
		// They way I do this is building a separate IK array (newIK) that simply changes the column heights.
		// So the affected SOLVEs would use this newIK and nothing else changes.
		for ( n = 1; n <= NetworkNumOfNodes + 1; ++n ) {
			newIK( n ) = IK( n );
			//print*, " NetworkNumOfNodes  n=", n, " IK(n)=", IK(n)
		}

		newsum = IK( 2 ) - IK( 1 ); // always 0?

		JHK = 1;
		for ( n = 2; n <= NetworkNumOfNodes; ++n ) {
			JHK1 = IK( n + 1 ); // starts at IK(3)-IK(2)
			LHK = JHK1 - JHK;
			if ( LHK <= 0 ) {
				newIK( n + 1 ) = newIK( n );
				continue;
			}
			//write(*,'(4(a,i8))') "n=", n, " ik=", ik(n), " JHK=", JHK, " LHK=", LHK

			// is the entire column zero?  noel
			allZero = true;
			for ( i = 0; i <= LHK - 1; ++i ) {
				if ( AU( JHK + i ) != 0.0 ) {
					allZero = false;
					break;
				}
			}

			newh = LHK;
			if ( allZero ) {
				//print*, "allzero n=", n
				newh = 0;
			} else {
				//DO i=0,LHK-1
				//   write(*, '(2(a,i8),a, f15.3)') "  n=", n, " i=", i, " AU(JHK+i)=", AU(JHK+i)
				//enddo
			}
			newIK( n + 1 ) = newIK( n ) + newh;
			newsum += newh;

			//do i = LHK-1,0, -1
			//   write(*, '(2(a,i8),a, f15.3)') "  n=", n, " i=", i, " AU(JHK+i)=", AU(JHK+i)
			//enddo
			JHK = JHK1;
		}

		// this is just a print to screen, is not necessary
		//     if (firstTime) then
		//        write(*, '(2(a,i8))') " After SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS: newsum=", newsum, " oldsum=", IK(NetworkNumOfNodes+1)
		//        firstTime=.FALSE.
		//     endif

		// Now fill newAU from AU, using newIK
		thisIK = 1;
		for ( n = 2; n <= NetworkNumOfNodes; ++n ) {
			thisIK = newIK( n );
			ispan = newIK( n + 1 ) - thisIK;

			if ( ispan <= 0 ) continue;
			for ( i = 0; i <= ispan - 1; ++i ) {
				newAU( thisIK + i ) = AU( IK( n ) + i );
			}

		}
#endif

	}

	void
	AFEPLR(
		int const j, // Component number
		int const LFLAG, // Initialization flag.If = 1, use laminar relationship
		Real64 const PDROP, // Total pressure drop across a component (P1 - P2) [Pa]
		int const i, // Linkage number
		int const n, // Node 1 number
		int const M, // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         George Walton
		//       DATE WRITTEN   Extracted from AIRNET
		//       MODIFIED       Lixing Gu, 2/1/04
		//                      Revised the subroutine to meet E+ needs
		//       MODIFIED       Lixing Gu, 6/8/05
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine solves airflow for a power law resistance airflow component

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		F.dim( 2 );
		DF.dim( 2 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 CDM;
		Real64 FL;
		Real64 FT;
		Real64 RhozNorm;
		Real64 VisczNorm;
		Real64 expn;
		Real64 Ctl;
		Real64 coef;
		int CompNum;

		// Formats
		static gio::Fmt Format_901( "(A5,I3,6X,4E16.7)" );

		// FLOW:
		// Crack standard condition: T=20C, p=101325 Pa and 0 g/kg
		CompNum = AirflowNetworkCompData( j ).TypeNum;
		RhozNorm = PsyRhoAirFnPbTdbW( 101325.0, 20.0, 0.0 );
		VisczNorm = 1.71432e-5 + 4.828e-8 * 20.0;
		expn = DisSysCompLeakData( CompNum ).FlowExpo;
		coef = DisSysCompLeakData( CompNum ).FlowCoef;

		if ( PDROP >= 0.0 ) {
			coef /= SQRTDZ( n );
		} else {
			coef /= SQRTDZ( M );
		}

		NF = 1;
		if ( LFLAG == 1 ) {
			// Initialization by linear relation.
			if ( PDROP >= 0.0 ) {
				Ctl = std::pow( RhozNorm / RHOZ( n ), expn - 1.0 ) * std::pow( VisczNorm / VISCZ( n ), 2.0 * expn - 1.0 );
				DF( 1 ) = coef * RHOZ( n ) / VISCZ( n ) * Ctl;
			} else {
				Ctl = std::pow( RhozNorm / RHOZ( M ), expn - 1.0 ) * std::pow( VisczNorm / VISCZ( M ), 2.0 * expn - 1.0 );
				DF( 1 ) = coef * RHOZ( M ) / VISCZ( M ) * Ctl;
			}
			F( 1 ) = -DF( 1 ) * PDROP;
		} else {
			// Standard calculation.
			if ( PDROP >= 0.0 ) {
				// Flow in positive direction for laminar flow.
				Ctl = std::pow( RhozNorm / RHOZ( n ), expn - 1.0 ) * std::pow( VisczNorm / VISCZ( n ), 2.0 * expn - 1.0 );
				CDM = coef * RHOZ( n ) / VISCZ( n ) * Ctl;
				FL = CDM * PDROP;
				// Flow in positive direction for turbulent flow.
				if ( expn == 0.5 ) {
					FT = coef * SQRTDZ( n ) * std::sqrt( PDROP );
				} else {
					FT = coef * SQRTDZ( n ) * std::pow( PDROP, expn );
				}
			} else {
				// Flow in negative direction for laminar flow
				CDM = coef * RHOZ( M ) / VISCZ( M );
				FL = CDM * PDROP;
				// Flow in negative direction for turbulent flow
				if ( expn == 0.5 ) {
					FT = -coef * SQRTDZ( M ) * std::sqrt( -PDROP );
				} else {
					FT = -coef * SQRTDZ( M ) * std::pow( -PDROP, expn );
				}
			}
			// Select laminar or turbulent flow.
			if ( LIST >= 4 ) gio::write( Unit21, Format_901 ) << " plr: " << i << PDROP << FL << FT;
			if ( std::abs( FL ) <= std::abs( FT ) ) {
				F( 1 ) = FL;
				DF( 1 ) = CDM;
			} else {
				F( 1 ) = FT;
				DF( 1 ) = FT * expn / PDROP;
			}
		}
	}

	void
	AFESCR(
		int const j, // Component number
		int const LFLAG, // Initialization flag.If = 1, use laminar relationship
		Real64 const PDROP, // Total pressure drop across a component (P1 - P2) [Pa]
		int const i, // Linkage number
		int const n, // Node 1 number
		int const M, // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         George Walton
		//       DATE WRITTEN   Extracted from AIRNET
		//       MODIFIED       Lixing Gu, 2/1/04
		//                      Revised the subroutine to meet E+ needs
		//       MODIFIED       Lixing Gu, 6/8/05
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine solves airflow for a surface crack component

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		F.dim( 2 );
		DF.dim( 2 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 CDM;
		Real64 FL;
		Real64 FT;
		Real64 RhozNorm;
		Real64 VisczNorm;
		Real64 expn;
		Real64 Ctl;
		Real64 coef;
		Real64 Corr;
		Real64 VisAve;
		Real64 Tave;
		Real64 RhoCor;
		int CompNum;

		// Formats
		static gio::Fmt Format_901( "(A5,I3,6X,4E16.7)" );

		// FLOW:
		// Crack standard condition from given inputs
		if ( i > NetworkNumOfLinks - NumOfLinksIntraZone ) {
			Corr = 1.0;
		} else {
			Corr = MultizoneSurfaceData( i ).Factor;
		}
		CompNum = AirflowNetworkCompData( j ).TypeNum;
		RhozNorm = PsyRhoAirFnPbTdbW( MultizoneSurfaceCrackData( CompNum ).StandardP, MultizoneSurfaceCrackData( CompNum ).StandardT, MultizoneSurfaceCrackData( CompNum ).StandardW );
		VisczNorm = 1.71432e-5 + 4.828e-8 * MultizoneSurfaceCrackData( CompNum ).StandardT;

		expn = MultizoneSurfaceCrackData( CompNum ).FlowExpo;
		VisAve = ( VISCZ( n ) + VISCZ( M ) ) / 2.0;
		Tave = ( TZ( n ) + TZ( M ) ) / 2.0;
		if ( PDROP >= 0.0 ) {
			coef = MultizoneSurfaceCrackData( CompNum ).FlowCoef / SQRTDZ( n ) * Corr;
		} else {
			coef = MultizoneSurfaceCrackData( CompNum ).FlowCoef / SQRTDZ( M ) * Corr;
		}

		NF = 1;
		if ( LFLAG == 1 ) {
			// Initialization by linear relation.
			if ( PDROP >= 0.0 ) {
				RhoCor = ( TZ( n ) + KelvinConv ) / ( Tave + KelvinConv );
				Ctl = std::pow( RhozNorm / RHOZ( n ) / RhoCor, expn - 1.0 ) * std::pow( VisczNorm / VisAve, 2.0 * expn - 1.0 );
				DF( 1 ) = coef * RHOZ( n ) / VISCZ( n ) * Ctl;
			} else {
				RhoCor = ( TZ( M ) + KelvinConv ) / ( Tave + KelvinConv );
				Ctl = std::pow( RhozNorm / RHOZ( M ) / RhoCor, expn - 1.0 ) * std::pow( VisczNorm / VisAve, 2.0 * expn - 1.0 );
				DF( 1 ) = coef * RHOZ( M ) / VISCZ( M ) * Ctl;
			}
			F( 1 ) = -DF( 1 ) * PDROP;
		} else {
			// Standard calculation.
			if ( PDROP >= 0.0 ) {
				// Flow in positive direction.
				// Laminar flow.
				RhoCor = ( TZ( n ) + KelvinConv ) / ( Tave + KelvinConv );
				Ctl = std::pow( RhozNorm / RHOZ( n ) / RhoCor, expn - 1.0 ) * std::pow( VisczNorm / VisAve, 2.0 * expn - 1.0 );
				CDM = coef * RHOZ( n ) / VISCZ( n ) * Ctl;
				FL = CDM * PDROP;
				// Turbulent flow.
				if ( expn == 0.5 ) {
					FT = coef * SQRTDZ( n ) * std::sqrt( PDROP ) * Ctl;
				} else {
					FT = coef * SQRTDZ( n ) * std::pow( PDROP, expn ) * Ctl;
				}
			} else {
				// Flow in negative direction.
				// Laminar flow.
				RhoCor = ( TZ( M ) + KelvinConv ) / ( Tave + KelvinConv );
				Ctl = std::pow( RhozNorm / RHOZ( M ) / RhoCor, expn - 1.0 ) * std::pow( VisczNorm / VisAve, 2.0 * expn - 1.0 );
				CDM = coef * RHOZ( M ) / VISCZ( M ) * Ctl;
				FL = CDM * PDROP;
				// Turbulent flow.
				if ( expn == 0.5 ) {
					FT = -coef * SQRTDZ( M ) * std::sqrt( -PDROP ) * Ctl;
				} else {
					FT = -coef * SQRTDZ( M ) * std::pow( -PDROP, expn ) * Ctl;
				}
			}
			// Select laminar or turbulent flow.
			if ( LIST >= 4 ) gio::write( Unit21, Format_901 ) << " scr: " << i << PDROP << FL << FT;
			if ( std::abs( FL ) <= std::abs( FT ) ) {
				F( 1 ) = FL;
				DF( 1 ) = CDM;
			} else {
				F( 1 ) = FT;
				DF( 1 ) = FT * expn / PDROP;
			}
		}
	}

	void
	AFEDWC(
		int const j, // Component number
		int const LFLAG, // Initialization flag.If = 1, use laminar relationship
		Real64 const PDROP, // Total pressure drop across a component (P1 - P2) [Pa]
		int const i, // Linkage number
		int const n, // Node 1 number
		int const M, // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         George Walton
		//       DATE WRITTEN   Extracted from AIRNET
		//       MODIFIED       Lixing Gu, 2/1/04
		//                      Revised the subroutine to meet E+ needs
		//       MODIFIED       Lixing Gu, 6/8/05
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine solves airflow for a duct/pipe component using Colebrook equation for the
		// turbulent friction factor

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		F.dim( 2 );
		DF.dim( 2 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const C( 0.868589 );
		Real64 const EPS( 0.001 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//     RE      - Reynolds number.
		//     FL      - friction factor for laminar flow.
		//     FT      - friction factor for turbulent flow.

		Real64 A0;
		Real64 A1;
		Real64 A2;
		Real64 B;
		Real64 D;
		Real64 S2;
		Real64 CDM;
		Real64 FL;
		Real64 FT;
		Real64 FTT;
		Real64 RE;
		int CompNum;
		Real64 ed;
		Real64 ld;
		Real64 g;
		Real64 AA1;

		// Formats
		static gio::Fmt Format_901( "(A5,I3,6X,4E16.7)" );

		// FLOW:
		CompNum = AirflowNetworkCompData( j ).TypeNum;
		ed = DisSysCompDuctData( CompNum ).Rough / DisSysCompDuctData( CompNum ).D;
		ld = DisSysCompDuctData( CompNum ).L / DisSysCompDuctData( CompNum ).D;
		g = 1.14 - 0.868589 * std::log( ed );
		AA1 = g;

		NF = 1;
		if ( LFLAG == 1 ) {
			// Initialization by linear relation.
			if ( PDROP >= 0.0 ) {
				DF( 1 ) = ( 2.0 * RHOZ( n ) * DisSysCompDuctData( CompNum ).A * DisSysCompDuctData( CompNum ).D ) / ( VISCZ( n ) * DisSysCompDuctData( CompNum ).InitLamCoef * ld );
			} else {
				DF( 1 ) = ( 2.0 * RHOZ( M ) * DisSysCompDuctData( CompNum ).A * DisSysCompDuctData( CompNum ).D ) / ( VISCZ( M ) * DisSysCompDuctData( CompNum ).InitLamCoef * ld );
			}
			F( 1 ) = -DF( 1 ) * PDROP;
			if ( LIST >= 4 ) gio::write( Unit21, Format_901 ) << " dwi:" << i << DisSysCompDuctData( CompNum ).InitLamCoef << F( 1 ) << DF( 1 );
		} else {
			// Standard calculation.
			if ( PDROP >= 0.0 ) {
				// Flow in positive direction.
				// Laminar flow coefficient !=0
				if ( DisSysCompDuctData( CompNum ).LamFriCoef >= 0.001 ) {
					A2 = DisSysCompDuctData( CompNum ).LamFriCoef / ( 2.0 * RHOZ( n ) * DisSysCompDuctData( CompNum ).A * DisSysCompDuctData( CompNum ).A );
					A1 = ( VISCZ( n ) * DisSysCompDuctData( CompNum ).LamDynCoef * ld ) / ( 2.0 * RHOZ( n ) * DisSysCompDuctData( CompNum ).A * DisSysCompDuctData( CompNum ).D );
					A0 = -PDROP;
					CDM = std::sqrt( A1 * A1 - 4.0 * A2 * A0 );
					FL = ( CDM - A1 ) / ( 2.0 * A2 );
					CDM = 1.0 / CDM;
				} else {
					CDM = ( 2.0 * RHOZ( n ) * DisSysCompDuctData( CompNum ).A * DisSysCompDuctData( CompNum ).D ) / ( VISCZ( n ) * DisSysCompDuctData( CompNum ).LamDynCoef * ld );
					FL = CDM * PDROP;
				}
				RE = FL * DisSysCompDuctData( CompNum ).D / ( VISCZ( n ) * DisSysCompDuctData( CompNum ).A );
				if ( LIST >= 4 ) gio::write( Unit21, Format_901 ) << " dwl:" << i << PDROP << FL << CDM << RE;
				// Turbulent flow; test when Re>10.
				if ( RE >= 10.0 ) {
					S2 = std::sqrt( 2.0 * RHOZ( n ) * PDROP ) * DisSysCompDuctData( CompNum ).A;
					FTT = S2 / std::sqrt( ld / pow_2( g ) + DisSysCompDuctData( CompNum ).TurDynCoef );
					if ( LIST >= 4 ) gio::write( Unit21, Format_901 ) << " dwt:" << i << S2 << FTT << g;
					while ( true ) {
						FT = FTT;
						B = ( 9.3 * VISCZ( n ) * DisSysCompDuctData( CompNum ).A ) / ( FT * DisSysCompDuctData( CompNum ).Rough );
						D = 1.0 + g * B;
						g -= ( g - AA1 + C * std::log( D ) ) / ( 1.0 + C * B / D );
						FTT = S2 / std::sqrt( ld / pow_2( g ) + DisSysCompDuctData( CompNum ).TurDynCoef );
						if ( LIST >= 4 ) gio::write( Unit21, Format_901 ) << " dwt:" << i << B << FTT << g;
						if ( std::abs( FTT - FT ) / FTT < EPS ) break;
					}
					FT = FTT;
				} else {
					FT = FL;
				}
			} else {
				// Flow in negative direction.
				// Laminar flow coefficient !=0
				if ( DisSysCompDuctData( CompNum ).LamFriCoef >= 0.001 ) {
					A2 = DisSysCompDuctData( CompNum ).LamFriCoef / ( 2.0 * RHOZ( M ) * DisSysCompDuctData( CompNum ).A * DisSysCompDuctData( CompNum ).A );
					A1 = ( VISCZ( M ) * DisSysCompDuctData( CompNum ).LamDynCoef * ld ) / ( 2.0 * RHOZ( M ) * DisSysCompDuctData( CompNum ).A * DisSysCompDuctData( CompNum ).D );
					A0 = PDROP;
					CDM = std::sqrt( A1 * A1 - 4.0 * A2 * A0 );
					FL = -( CDM - A1 ) / ( 2.0 * A2 );
					CDM = 1.0 / CDM;
				} else {
					CDM = ( 2.0 * RHOZ( M ) * DisSysCompDuctData( CompNum ).A * DisSysCompDuctData( CompNum ).D ) / ( VISCZ( M ) * DisSysCompDuctData( CompNum ).LamDynCoef * ld );
					FL = CDM * PDROP;
				}
				RE = -FL * DisSysCompDuctData( CompNum ).D / ( VISCZ( M ) * DisSysCompDuctData( CompNum ).A );
				if ( LIST >= 4 ) gio::write( Unit21, Format_901 ) << " dwl:" << i << PDROP << FL << CDM << RE;
				// Turbulent flow; test when Re>10.
				if ( RE >= 10.0 ) {
					S2 = std::sqrt( -2.0 * RHOZ( M ) * PDROP ) * DisSysCompDuctData( CompNum ).A;
					FTT = S2 / std::sqrt( ld / pow_2( g ) + DisSysCompDuctData( CompNum ).TurDynCoef );
					if ( LIST >= 4 ) gio::write( Unit21, Format_901 ) << " dwt:" << i << S2 << FTT << g;
					while ( true ) {
						FT = FTT;
						B = ( 9.3 * VISCZ( M ) * DisSysCompDuctData( CompNum ).A ) / ( FT * DisSysCompDuctData( CompNum ).Rough );
						D = 1.0 + g * B;
						g -= ( g - AA1 + C * std::log( D ) ) / ( 1.0 + C * B / D );
						FTT = S2 / std::sqrt( ld / pow_2( g ) + DisSysCompDuctData( CompNum ).TurDynCoef );
						if ( LIST >= 4 ) gio::write( Unit21, Format_901 ) << " dwt:" << i << B << FTT << g;
						if ( std::abs( FTT - FT ) / FTT < EPS ) break;
					}
					FT = -FTT;
				} else {
					FT = FL;
				}
			}
			// Select laminar or turbulent flow.
			if ( std::abs( FL ) <= std::abs( FT ) ) {
				F( 1 ) = FL;
				DF( 1 ) = CDM;
			} else {
				F( 1 ) = FT;
				DF( 1 ) = 0.5 * FT / PDROP;
			}
		}

	}

	void
	AFESOP(
		int const j, // Component number
		int const LFLAG, // Initialization flag.If = 1, use laminar relationship
		Real64 const PDROP, // Total pressure drop across a component (P1 - P2) [Pa]
		int const i, // Linkage number
		int const n, // Node 1 number
		int const M, // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         George Walton
		//       DATE WRITTEN   Extracted from AIRNET
		//       MODIFIED       Lixing Gu, 2/1/04
		//                      Revised the subroutine to meet E+ needs
		//       MODIFIED       Lixing Gu, 6/8/05
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine solves airflow for a Doorway airflow component using standard interface.
		// A doorway may have two-way airflows. Heights measured relative to the bottom of the door.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		F.dim( 2 );
		DF.dim( 2 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const SQRT2( 1.414213562373095 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//     DPMID   - pressure drop at mid-height of doorway.
		//     DRHO    - difference in air densities between rooms.
		//     Y       - height of neutral plane rel. to bottom of door (m).
		//     F0      - flow factor at the bottom of the door.
		//     FH      - flow factor at the top of the door.
		//     DF0     - derivative factor at the bottom of the door.
		//     DFH     - derivative factor at the top of the door.
		Real64 DPMID;
		Real64 C;
		Real64 DF0;
		Real64 DFH;
		Real64 DRHO;
		Real64 GDRHO;
		Real64 F0;
		Real64 FH;
		Real64 Y;
		int CompNum;
		Real64 FlowCoef;
		Real64 FlowExpo;
		Real64 MinRhoDiff;
		Real64 DischCoeff;
		Real64 Width;
		Real64 Height;
		Real64 OpenFactor;

		// Formats
		static gio::Fmt Format_900( "(A5,9X,4E16.7)" );
		static gio::Fmt Format_903( "(A5,3I3,4E16.7)" );

		// FLOW:
		CompNum = AirflowNetworkCompData( j ).TypeNum;
		MinRhoDiff = MultizoneCompSimpleOpeningData( CompNum ).MinRhoDiff;
		DischCoeff = MultizoneCompSimpleOpeningData( CompNum ).DischCoeff;
		Width = MultizoneSurfaceData( i ).Width;
		Height = MultizoneSurfaceData( i ).Height;
		FlowCoef = MultizoneCompSimpleOpeningData( CompNum ).FlowCoef * 2.0 * ( Width + Height );
		FlowExpo = MultizoneCompSimpleOpeningData( CompNum ).FlowExpo;
		OpenFactor = MultizoneSurfaceData( i ).OpenFactor;
		if ( OpenFactor > 0.0 ) {
			Width *= OpenFactor;
			if ( Surface( MultizoneSurfaceData( i ).SurfNum ).Tilt < 90.0 ) {
				Height *= Surface( MultizoneSurfaceData( i ).SurfNum ).SinTilt;
			}
		}

		if ( PDROP >= 0.0 ) {
			FlowCoef /= SQRTDZ( n );
		} else {
			FlowCoef /= SQRTDZ( M );
		}

		// Add window multiplier with window close
		if ( MultizoneSurfaceData( i ).Multiplier > 1.0 ) FlowCoef *= MultizoneSurfaceData( i ).Multiplier;
		// Add window multiplier with window open
		if ( OpenFactor > 0.0 ) {
			if ( MultizoneSurfaceData( i ).Multiplier > 1.0 ) Width *= MultizoneSurfaceData( i ).Multiplier;
		}

		NF = 1;
		DRHO = RHOZ( n ) - RHOZ( M );
		GDRHO = 9.8 * DRHO;
		if ( LIST >= 4 ) gio::write( Unit21, Format_903 ) << " DOR:" << i << n << M << PDROP << std::abs( DRHO ) << MinRhoDiff;
		if ( OpenFactor == 0.0 ) {
			GenericCrack( FlowCoef, FlowExpo, LFLAG, PDROP, n, M, F, DF, NF );
			return;
		}
		if ( std::abs( DRHO ) < MinRhoDiff || LFLAG == 1 ) {
			DPMID = PDROP - 0.5 * Height * GDRHO;
			// Initialization or identical temps: treat as one-way flow.
			GenericCrack( FlowCoef, FlowExpo, LFLAG, DPMID, n, M, F, DF, NF );
			if ( LIST >= 4 ) gio::write( Unit21, Format_900 ) << " Drs:" << DPMID << F( 1 ) << DF( 1 );
		} else {
			// Possible two-way flow:
			Y = PDROP / GDRHO;
			if ( LIST >= 4 ) gio::write( Unit21, Format_900 ) << " DrY:" << PDROP << GDRHO << Y;
			// F0 = lower flow, FH = upper flow.
			C = SQRT2 * Width * DischCoeff;
			DF0 = C * std::sqrt( std::abs( PDROP ) ) / std::abs( GDRHO );
			//        F0 = 0.666667d0*C*SQRT(ABS(GDRHO*Y))*ABS(Y)
			F0 = ( 2.0 / 3.0 ) * C * std::sqrt( std::abs( GDRHO * Y ) ) * std::abs( Y );
			DFH = C * std::sqrt( std::abs( ( Height - Y ) / GDRHO ) );
			//        FH = 0.666667d0*DFH*ABS(GDRHO*(Height-Y))
			FH = ( 2.0 / 3.0 ) * DFH * std::abs( GDRHO * ( Height - Y ) );
			if ( LIST >= 4 ) gio::write( Unit21, Format_900 ) << " DrF:" << F0 << DF0 << FH << DFH;
			if ( Y <= 0.0 ) {
				// One-way flow (negative).
				if ( DRHO >= 0.0 ) {
					F( 1 ) = -SQRTDZ( M ) * std::abs( FH - F0 );
					DF( 1 ) = SQRTDZ( M ) * std::abs( DFH - DF0 );
				} else {
					F( 1 ) = SQRTDZ( n ) * std::abs( FH - F0 );
					DF( 1 ) = SQRTDZ( n ) * std::abs( DFH - DF0 );
				}
				if ( LIST >= 4 ) gio::write( Unit21, Format_900 ) << " Dr1:" << C << F( 1 ) << DF( 1 );
			} else if ( Y >= Height ) {
				// One-way flow (positive).
				if ( DRHO >= 0.0 ) {
					F( 1 ) = SQRTDZ( n ) * std::abs( FH - F0 );
					DF( 1 ) = SQRTDZ( n ) * std::abs( DFH - DF0 );
				} else {
					F( 1 ) = -SQRTDZ( M ) * std::abs( FH - F0 );
					DF( 1 ) = SQRTDZ( M ) * std::abs( DFH - DF0 );
				}
				if ( LIST >= 4 ) gio::write( Unit21, Format_900 ) << " Dr2:" << C << F( 1 ) << DF( 1 );
			} else {
				// Two-way flow.
				NF = 2;
				if ( DRHO >= 0.0 ) {
					F( 1 ) = -SQRTDZ( M ) * FH;
					DF( 1 ) = SQRTDZ( M ) * DFH;
					F( 2 ) = SQRTDZ( n ) * F0;
					DF( 2 ) = SQRTDZ( n ) * DF0;
				} else {
					F( 1 ) = SQRTDZ( n ) * FH;
					DF( 1 ) = SQRTDZ( n ) * DFH;
					F( 2 ) = -SQRTDZ( M ) * F0;
					DF( 2 ) = SQRTDZ( M ) * DF0;
				}
				if ( LIST >= 4 ) gio::write( Unit21, Format_900 ) << " Dr3:" << C << F( 1 ) << DF( 1 );
				if ( LIST >= 4 ) gio::write( Unit21, Format_900 ) << " Dr4:" << C << F( 2 ) << DF( 2 );
			}
		}

	}

	void
	AFECFR(
		int const j, // Component number
		int const EP_UNUSED( LFLAG ), // Initialization flag.If = 1, use laminar relationship
		Real64 const EP_UNUSED( PDROP ), // Total pressure drop across a component (P1 - P2) [Pa]
		int const EP_UNUSED( i ), // Linkage number
		int const EP_UNUSED( n ), // Node 1 number
		int const EP_UNUSED( M ), // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         George Walton
		//       DATE WRITTEN   Extracted from AIRNET
		//       MODIFIED       Lixing Gu, 2/1/04
		//                      Revised the subroutine to meet E+ needs
		//       MODIFIED       Lixing Gu, 6/8/05
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine solves airflow for a constant flow rate airflow component -- using standard interface.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataLoopNode::Node;
		using DataAirLoop::LoopSystemOnMassFlowrate;
		using DataAirLoop::LoopSystemOffMassFlowrate;
		using DataAirLoop::LoopFanOperationMode;
		using DataAirLoop::LoopCompCycRatio;
		using DataHVACGlobals::FanType_SimpleOnOff;
		using DataHVACGlobals::FanType_SimpleConstVolume;
		using DataHVACGlobals::FanType_SimpleVAV;

		// Argument array dimensioning
		F.dim( 2 );
		DF.dim( 2 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const CycFanCycComp( 1 ); // fan cycles with compressor operation

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CompNum;
		int k;
		int k1;
		Real64 SumTermFlow; // Sum of all Terminal flows [kg/s]
		Real64 SumFracSuppLeak; // Sum of all supply leaks as a fraction of supply fan flow rate
		int Node1;
		int Node2;

		// FLOW:
		CompNum = AirflowNetworkCompData( j ).TypeNum;

		NF = 1;
		if ( DisSysCompCVFData( CompNum ).FanTypeNum == FanType_SimpleOnOff ) {
			if ( LoopFanOperationMode == CycFanCycComp && LoopSystemOnMassFlowrate > 0.0 ) {
				F( 1 ) = LoopSystemOnMassFlowrate;
			} else {
				F( 1 ) = Node( DisSysCompCVFData( CompNum ).InletNode ).MassFlowRate * DisSysCompCVFData( CompNum ).Ctrl;
				if ( MultiSpeedHPIndicator == 2 ) {
					F( 1 ) = LoopSystemOnMassFlowrate * LoopCompCycRatio + LoopSystemOffMassFlowrate * ( 1.0 - LoopCompCycRatio );
				}
			}
		} else if ( DisSysCompCVFData( CompNum ).FanTypeNum == FanType_SimpleConstVolume ) {
			if ( DisSysCompCVFData( CompNum ).FlowRate > 0 ) {
				F( 1 ) = DisSysCompCVFData( CompNum ).FlowRate * DisSysCompCVFData( CompNum ).Ctrl;
			} else {
				F( 1 ) = Node( DisSysCompCVFData( CompNum ).InletNode ).MassFlowRate * DisSysCompCVFData( CompNum ).Ctrl;
			}
			if ( MultiSpeedHPIndicator == 2 ) {
				F( 1 ) = LoopSystemOnMassFlowrate;
			}
		} else if ( DisSysCompCVFData( CompNum ).FanTypeNum == FanType_SimpleVAV ) {
			// Check VAV termals with a damper
			SumTermFlow = 0.0;
			SumFracSuppLeak = 0.0;
			for ( k = 1; k <= NetworkNumOfLinks; ++k ) {
				if ( AirflowNetworkLinkageData( k ).VAVTermDamper ) {
					k1 = AirflowNetworkNodeData( AirflowNetworkLinkageData( k ).NodeNums( 1 ) ).EPlusNodeNum;
					if ( Node( k1 ).MassFlowRate > 0.0 ) {
						SumTermFlow += Node( k1 ).MassFlowRate;
					}
				}
				if ( AirflowNetworkCompData( AirflowNetworkLinkageData( k ).CompNum ).CompTypeNum == CompTypeNum_ELR ) {
					// Calculate supply leak sensible losses
					Node1 = AirflowNetworkLinkageData( k ).NodeNums( 1 );
					Node2 = AirflowNetworkLinkageData( k ).NodeNums( 2 );
					if ( ( AirflowNetworkNodeData( Node2 ).EPlusZoneNum > 0 ) && ( AirflowNetworkNodeData( Node1 ).EPlusNodeNum == 0 ) ) {
						SumFracSuppLeak += DisSysCompELRData( AirflowNetworkCompData( AirflowNetworkLinkageData( k ).CompNum ).TypeNum ).ELR;
					}
				}
			}
			F( 1 ) = SumTermFlow / ( 1.0 - SumFracSuppLeak );
			VAVTerminalRatio = 0.0;
			if ( F( 1 ) > DisSysCompCVFData( CompNum ).MaxAirMassFlowRate ) {
				VAVTerminalRatio = DisSysCompCVFData( CompNum ).MaxAirMassFlowRate / F( 1 );
				F( 1 ) = DisSysCompCVFData( CompNum ).MaxAirMassFlowRate;
			}
		}
		DF( 1 ) = 0.0;

	}

	void
	AFEFAN(
		int const JA, // Component number
		int const LFLAG, // Initialization flag.If = 1, use laminar relationship
		Real64 const PDROP, // Total pressure drop across a component (P1 - P2) [Pa]
		int const i, // Linkage number
		int const n, // Node 1 number
		int const M, // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         George Walton
		//       DATE WRITTEN   Extracted from AIRNET
		//       MODIFIED       Lixing Gu, 2/1/04
		//                      Revised the subroutine to meet E+ needs
		//       MODIFIED       Lixing Gu, 6/8/05
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine solves airflow for a detailed fan component -- using standard interface.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		F.dim( 2 );
		DF.dim( 2 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const TOL( 0.00001 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//     PRISE   - pressure rise (negative of pressure drop) (Pa).
		int j;
		int k;
		int L;
		Real64 DPDF;
		Real64 PRISE;
		Real64 BX;
		Real64 BY;
		Real64 CX;
		Real64 CY;
		Real64 CCY;
		Real64 DX;
		Real64 DY;
		int CompNum;
		int NumCur;
		Real64 FlowCoef;
		Real64 FlowExpo;

		// Formats
		static gio::Fmt Format_901( "(A5,I3,5E14.6)" );

		// FLOW:
		CompNum = AirflowNetworkCompData( JA ).TypeNum;
		NumCur = DisSysCompDetFanData( CompNum ).n;
		FlowCoef = DisSysCompDetFanData( CompNum ).FlowCoef;
		FlowExpo = DisSysCompDetFanData( CompNum ).FlowExpo;

		NF = 1;
		if ( AFECTL( i ) <= 0.0 ) {
			// Speed = 0; treat fan as resistance.
			GenericCrack( FlowCoef, FlowExpo, LFLAG, PDROP, n, M, F, DF, NF );
			return;
		}
		// Pressure rise at reference fan speed.
		if ( AFECTL( i ) >= DisSysCompDetFanData( CompNum ).TranRat ) {
			PRISE = -PDROP * ( DisSysCompDetFanData( CompNum ).RhoAir / RHOZ( n ) ) / pow_2( AFECTL( i ) );
		} else {
			PRISE = -PDROP * ( DisSysCompDetFanData( CompNum ).RhoAir / RHOZ( n ) ) / ( DisSysCompDetFanData( CompNum ).TranRat * AFECTL( i ) );
		}
		if ( LIST >= 4 ) gio::write( Unit21, Format_901 ) << " fan:" << i << PDROP << PRISE << AFECTL( i ) << DisSysCompDetFanData( CompNum ).TranRat;
		if ( LFLAG == 1 ) {
			// Initialization by linear approximation.
			F( 1 ) = -DisSysCompDetFanData( CompNum ).Qfree * AFECTL( i ) * ( 1.0 - PRISE / DisSysCompDetFanData( CompNum ).Pshut );
			DPDF = -DisSysCompDetFanData( CompNum ).Pshut / DisSysCompDetFanData( CompNum ).Qfree;
			if ( LIST >= 4 ) gio::write( Unit21, Format_901 ) << " fni:" << JA << DisSysCompDetFanData( CompNum ).Qfree << DisSysCompDetFanData( CompNum ).Pshut;
		} else {
			// Solution of the fan performance curve.
			// Determine curve fit range.
			j = 1;
			k = 5 * ( j - 1 ) + 1;
			BX = DisSysCompDetFanData( CompNum ).Coeff( k );
			BY = DisSysCompDetFanData( CompNum ).Coeff( k + 1 ) + BX * ( DisSysCompDetFanData( CompNum ).Coeff( k + 2 ) + BX * ( DisSysCompDetFanData( CompNum ).Coeff( k + 3 ) + BX * DisSysCompDetFanData( CompNum ).Coeff( k + 4 ) ) ) - PRISE;
			if ( BY < 0.0 ) ShowFatalError( "Out of range, too low in an AirflowNetwork detailed Fan" );

			while ( true ) {
				DX = DisSysCompDetFanData( CompNum ).Coeff( k + 5 );
				DY = DisSysCompDetFanData( CompNum ).Coeff( k + 1 ) + DX * ( DisSysCompDetFanData( CompNum ).Coeff( k + 2 ) + DX * ( DisSysCompDetFanData( CompNum ).Coeff( k + 3 ) + DX * DisSysCompDetFanData( CompNum ).Coeff( k + 5 ) ) ) - PRISE;
				if ( LIST >= 4 ) gio::write( Unit21, Format_901 ) << " fp0:" << j << BX << BY << DX << DY;
				if ( BY * DY <= 0.0 ) break;
				++j;
				if ( j > NumCur ) ShowFatalError( "Out of range, too high (FAN) in ADS simulation" );
				k += 5;
				BX = DX;
				BY = DY;
			}
			// Determine reference mass flow rate by false position method.
			L = 0;
			CY = 0.0;
Label40: ;
			++L;
			if ( L > 100 ) ShowFatalError( "Too many iterations (FAN) in AirflowNtework simulation" );
			CCY = CY;
			CX = BX - BY * ( ( DX - BX ) / ( DY - BY ) );
			CY = DisSysCompDetFanData( CompNum ).Coeff( k + 1 ) + CX * ( DisSysCompDetFanData( CompNum ).Coeff( k + 2 ) + CX * ( DisSysCompDetFanData( CompNum ).Coeff( k + 3 ) + CX * DisSysCompDetFanData( CompNum ).Coeff( k + 4 ) ) ) - PRISE;
			if ( BY * CY == 0.0 ) goto Label90;
			if ( BY * CY > 0.0 ) goto Label60;
			DX = CX;
			DY = CY;
			if ( CY * CCY > 0.0 ) BY *= 0.5;
			goto Label70;
Label60: ;
			BX = CX;
			BY = CY;
			if ( CY * CCY > 0.0 ) DY *= 0.5;
Label70: ;
			if ( LIST >= 4 ) gio::write( Unit21, Format_901 ) << " fpi:" << j << BX << CX << DX << BY << DY;
			if ( DX - BX < TOL * CX ) goto Label80;
			if ( DX - BX < TOL ) goto Label80;
			goto Label40;
Label80: ;
			CX = 0.5 * ( BX + DX );
Label90: ;
			F( 1 ) = CX;
			DPDF = DisSysCompDetFanData( CompNum ).Coeff( k + 2 ) + CX * ( 2.0 * DisSysCompDetFanData( CompNum ).Coeff( k + 3 ) + CX * 3.0 * DisSysCompDetFanData( CompNum ).Coeff( k + 4 ) );
		}
		// Convert to flow at given speed.
		F( 1 ) *= ( RHOZ( n ) / DisSysCompDetFanData( CompNum ).RhoAir ) * AFECTL( i );
		// Set derivative w/r pressure drop (-).
		if ( AFECTL( i ) >= DisSysCompDetFanData( CompNum ).TranRat ) {
			DF( 1 ) = -AFECTL( i ) / DPDF;
		} else {
			DF( 1 ) = -1.0 / DPDF;
		}
	}

	// The above subroutine is not used. Leave it for the time being and revise later.

	void
	AFECPF(
		int const EP_UNUSED( j ), // Component number
		int const LFLAG, // Initialization flag.If = 1, use laminar relationship
		Real64 const PDROP, // Total pressure drop across a component (P1 - P2) [Pa]
		int const i, // Linkage number
		int const EP_UNUSED( n ), // Node 1 number
		int const EP_UNUSED( M ), // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         George Walton
		//       DATE WRITTEN   Extracted from AIRNET
		//       MODIFIED       Lixing Gu, 2/1/04
		//                      Revised the subroutine to meet E+ needs
		//       MODIFIED       Lixing Gu, 6/8/05
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine solves airflow for a constant power simple fan component -- using standard interface.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		F.dim( 2 );
		DF.dim( 2 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		// FLOW:
		NF = 1;
		if ( LFLAG == 1 ) {
			F( 1 ) = AFECTL( i );
			DF( 1 ) = F( 1 );
		} else {
			F( 1 ) = -AFECTL( i ) / PDROP;
			DF( 1 ) = -F( 1 ) / PDROP;
		}

	}

	// Leave it for the time being and revise later. Or drop this component ???????????

	void
	AFEDMP(
		int const j, // Component number
		int const LFLAG, // Initialization flag.If = 1, use laminar relationship
		Real64 const PDROP, // Total pressure drop across a component (P1 - P2) [Pa]
		int const i, // Linkage number
		int const n, // Node 1 number
		int const M, // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         George Walton
		//       DATE WRITTEN   Extracted from AIRNET
		//       MODIFIED       Lixing Gu, 2/1/04
		//                      Revised the subroutine to meet E+ needs
		//       MODIFIED       Lixing Gu, 6/8/05
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine solves airflow for a Controlled power law resistance airflow component (damper)

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		F.dim( 2 );
		DF.dim( 2 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 C;
		int CompNum;

		// Formats
		static gio::Fmt Format_901( "(A5,I3,6X,4E16.7)" );

		// FLOW:
		// Get component number
		CompNum = AirflowNetworkCompData( j ).TypeNum;

		NF = 1;
		C = AFECTL( i );
		if ( C < DisSysCompDamperData( CompNum ).FlowMin ) C = DisSysCompDamperData( CompNum ).FlowMin;
		if ( C > DisSysCompDamperData( CompNum ).FlowMax ) C = DisSysCompDamperData( CompNum ).FlowMax;
		C = DisSysCompDamperData( CompNum ).A0 + C * ( DisSysCompDamperData( CompNum ).A1 + C * ( DisSysCompDamperData( CompNum ).A2 + C * DisSysCompDamperData( CompNum ).A3 ) );
		if ( LIST >= 4 ) gio::write( Unit21, Format_901 ) << " Dmp:" << i << AFECTL( i ) << DisSysCompDamperData( CompNum ).FlowMin << DisSysCompDamperData( CompNum ).FlowMax << C;
		if ( LFLAG == 1 || std::abs( PDROP ) <= DisSysCompDamperData( CompNum ).LTP ) {
			//                              Laminar flow.
			if ( PDROP >= 0.0 ) {
				DF( 1 ) = C * DisSysCompDamperData( CompNum ).LamFlow * RHOZ( n ) / VISCZ( n );
			} else {
				DF( 1 ) = C * DisSysCompDamperData( CompNum ).LamFlow * RHOZ( M ) / VISCZ( M );
			}
			F( 1 ) = DF( 1 ) * PDROP;
		} else {
			//                              Turbulent flow.
			if ( PDROP >= 0.0 ) {
				F( 1 ) = C * DisSysCompDamperData( CompNum ).TurFlow * SQRTDZ( n ) * std::pow( PDROP, DisSysCompDamperData( CompNum ).FlowExpo );
			} else {
				F( 1 ) = -C * DisSysCompDamperData( CompNum ).TurFlow * SQRTDZ( M ) * std::pow( -PDROP, DisSysCompDamperData( CompNum ).FlowExpo );
			}
			DF( 1 ) = F( 1 ) * DisSysCompDamperData( CompNum ).FlowExpo / PDROP;
		}

	}

	void
	AFESEL(
		int const j, // Component number
		int const LFLAG, // Initialization flag.If = 1, use laminar relationship
		Real64 const PDROP, // Total pressure drop across a component (P1 - P2) [Pa]
		int const i, // Linkage number
		int const n, // Node 1 number
		int const M, // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         George Walton
		//       DATE WRITTEN   Extracted from AIRNET
		//       MODIFIED       Lixing Gu, 2/1/04
		//                      Revised the subroutine to meet E+ needs
		//       MODIFIED       Lixing Gu, 6/8/05
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine solves airflow for a Surface effective leakage area component

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		F.dim( 2 );
		DF.dim( 2 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static Real64 const sqrt_2( std::sqrt( 2.0 ) );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 CDM;
		Real64 FL;
		Real64 FT;
		Real64 FlowCoef;
		Real64 FlowExpo;
		int CompNum;

		// Formats
		static gio::Fmt Format_901( "(A5,I3,6X,4E16.7)" );

		// FLOW:
		// Get component properties
		CompNum = AirflowNetworkCompData( j ).TypeNum;
		FlowExpo = MultizoneSurfaceELAData( CompNum ).FlowExpo;
		FlowCoef = MultizoneSurfaceELAData( CompNum ).ELA * MultizoneSurfaceELAData( CompNum ).DischCoeff * sqrt_2 * std::pow( MultizoneSurfaceELAData( CompNum ).RefDeltaP, 0.5 - FlowExpo );

		NF = 1;
		if ( LFLAG == 1 ) {
			// Initialization by linear relation.
			if ( PDROP >= 0.0 ) {
				DF( 1 ) = FlowCoef * RHOZ( n ) / VISCZ( n );
			} else {
				DF( 1 ) = FlowCoef * RHOZ( M ) / VISCZ( M );
			}
			F( 1 ) = -DF( 1 ) * PDROP;
		} else {
			// Standard calculation.
			if ( PDROP >= 0.0 ) {
				// Flow in positive direction.
				// Laminar flow.
				CDM = FlowCoef * RHOZ( n ) / VISCZ( n );
				FL = CDM * PDROP;
				// Turbulent flow.
				if ( MultizoneSurfaceELAData( CompNum ).FlowExpo == 0.5 ) {
					FT = FlowCoef * SQRTDZ( n ) * std::sqrt( PDROP );
				} else {
					FT = FlowCoef * SQRTDZ( n ) * std::pow( PDROP, FlowExpo );
				}
			} else {
				// Flow in negative direction.
				// Laminar flow.
				CDM = FlowCoef * RHOZ( M ) / VISCZ( M );
				FL = CDM * PDROP;
				// Turbulent flow.
				if ( FlowExpo == 0.5 ) {
					FT = -FlowCoef * SQRTDZ( M ) * std::sqrt( -PDROP );
				} else {
					FT = -FlowCoef * SQRTDZ( M ) * std::pow( -PDROP, FlowExpo );
				}
			}
			// Select laminar or turbulent flow.
			if ( LIST >= 4 ) gio::write( Unit21, Format_901 ) << " plr: " << i << PDROP << FL << FT;
			if ( std::abs( FL ) <= std::abs( FT ) ) {
				F( 1 ) = FL;
				DF( 1 ) = CDM;
			} else {
				F( 1 ) = FT;
				DF( 1 ) = FT * FlowExpo / PDROP;
			}
		}

	}

	void
	AFEELR(
		int const j, // Component number
		int const LFLAG, // Initialization flag.If = 1, use laminar relationship
		Real64 const PDROP, // Total pressure drop across a component (P1 - P2) [Pa]
		int const i, // Linkage number
		int const n, // Node 1 number
		int const M, // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         George Walton
		//       DATE WRITTEN   Extracted from AIRNET
		//       MODIFIED       Lixing Gu, 2/1/04
		//                      Revised the subroutine to meet E+ needs
		//       MODIFIED       Lixing Gu, 6/8/05
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine solves airflow for a Effective leakage ratio component

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		F.dim( 2 );
		DF.dim( 2 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 CDM;
		Real64 FL;
		Real64 FT;
		Real64 FlowCoef;
		int CompNum;

		// Formats
		static gio::Fmt Format_901( "(A5,I3,6X,4E16.7)" );

		// FLOW:
		// Get component properties
		CompNum = AirflowNetworkCompData( j ).TypeNum;
		FlowCoef = DisSysCompELRData( CompNum ).ELR * DisSysCompELRData( CompNum ).FlowRate / RHOZ( n ) * std::pow( DisSysCompELRData( CompNum ).RefPres, -DisSysCompELRData( CompNum ).FlowExpo );

		NF = 1;
		if ( LFLAG == 1 ) {
			// Initialization by linear relation.
			if ( PDROP >= 0.0 ) {
				DF( 1 ) = FlowCoef * RHOZ( n ) / VISCZ( n );
			} else {
				DF( 1 ) = FlowCoef * RHOZ( M ) / VISCZ( M );
			}
			F( 1 ) = -DF( 1 ) * PDROP;
		} else {
			// Standard calculation.
			if ( PDROP >= 0.0 ) {
				// Flow in positive direction.
				// Laminar flow.
				CDM = FlowCoef * RHOZ( n ) / VISCZ( n );
				FL = CDM * PDROP;
				// Turbulent flow.
				if ( DisSysCompELRData( CompNum ).FlowExpo == 0.5 ) {
					FT = FlowCoef * SQRTDZ( n ) * std::sqrt( PDROP );
				} else {
					FT = FlowCoef * SQRTDZ( n ) * std::pow( PDROP, DisSysCompELRData( CompNum ).FlowExpo );
				}
			} else {
				// Flow in negative direction.
				// Laminar flow.
				CDM = FlowCoef * RHOZ( M ) / VISCZ( M );
				FL = CDM * PDROP;
				// Turbulent flow.
				if ( DisSysCompELRData( CompNum ).FlowExpo == 0.5 ) {
					FT = -FlowCoef * SQRTDZ( M ) * std::sqrt( -PDROP );
				} else {
					FT = -FlowCoef * SQRTDZ( M ) * std::pow( -PDROP, DisSysCompELRData( CompNum ).FlowExpo );
				}
			}
			// Select laminar or turbulent flow.
			if ( LIST >= 4 ) gio::write( Unit21, Format_901 ) << " plr: " << i << PDROP << FL << FT;
			if ( std::abs( FL ) <= std::abs( FT ) ) {
				F( 1 ) = FL;
				DF( 1 ) = CDM;
			} else {
				F( 1 ) = FT;
				DF( 1 ) = FT * DisSysCompELRData( CompNum ).FlowExpo / PDROP;
			}
		}

	}

	void
	AFECPD(
		int const j, // Component number
		int const EP_UNUSED( LFLAG ), // Initialization flag.If = 1, use laminar relationship
		Real64 & PDROP, // Total pressure drop across a component (P1 - P2) [Pa]
		int const EP_UNUSED( i ), // Linkage number
		int const n, // Node 1 number
		int const M, // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         George Walton
		//       DATE WRITTEN   Extracted from AIRNET
		//       MODIFIED       Lixing Gu, 2/1/04
		//                      Revised the subroutine to meet E+ needs
		//       MODIFIED       Lixing Gu, 6/8/05
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine solves airflow for a Constant pressure drop component

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		F.dim( 2 );
		DF.dim( 2 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 Co;
		int CompNum;
		int k;

		// FLOW:
		// Get component properties
		// A  = Cross section area [m2]
		// DP = Pressure difference across the element [Pa]
		CompNum = AirflowNetworkCompData( j ).TypeNum;

		NF = 1;
		if ( PDROP == 0.0 ) {
			F( 1 ) = std::sqrt( 2.0 * RHOZ( n ) ) * DisSysCompCPDData( CompNum ).A * std::sqrt( DisSysCompCPDData( CompNum ).DP );
			DF( 1 ) = 0.5 * F( 1 ) / DisSysCompCPDData( CompNum ).DP;
		} else {
			for ( k = 1; k <= NetworkNumOfLinks; ++k ) {
				if ( AirflowNetworkLinkageData( k ).NodeNums( 2 ) == n ) {
					F( 1 ) = AFLOW( k );
					break;
				}
			}
			PDROP = DisSysCompCPDData( CompNum ).DP;
			PZ( M ) = PZ( n ) - PDROP;
			Co = F( 1 ) / DisSysCompCPDData( CompNum ).DP;
			DF( 1 ) = 10.e10;
		}

	}

	void
	AFECOI(
		int const j, // Component number
		int const LFLAG, // Initialization flag.If = 1, use laminar relationship
		Real64 const PDROP, // Total pressure drop across a component (P1 - P2) [Pa]
		int const i, // Linkage number
		int const n, // Node 1 number
		int const M, // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         George Walton
		//       DATE WRITTEN   Extracted from AIRNET
		//       MODIFIED       Lixing Gu, 2/1/04
		//                      Revised the subroutine to meet E+ needs
		//       MODIFIED       Lixing Gu, 6/8/05
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine solves airflow for a coil component

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		F.dim( 2 );
		DF.dim( 2 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const C( 0.868589 );
		Real64 const EPS( 0.001 );
		Real64 const Rough( 0.0001 );
		Real64 const InitLamCoef( 128.0 );
		Real64 const LamDynCoef( 64.0 );
		Real64 const LamFriCoef( 0.0001 );
		Real64 const TurDynCoef( 0.0001 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 A0;
		Real64 A1;
		Real64 A2;
		Real64 B;
		Real64 D;
		Real64 S2;
		Real64 CDM;
		Real64 FL;
		Real64 FT;
		Real64 FTT;
		Real64 RE;
		int CompNum;
		Real64 ed;
		Real64 ld;
		Real64 g;
		Real64 AA1;
		Real64 area;

		// Formats
		static gio::Fmt Format_901( "(A5,I3,6X,4E16.7)" );

		// FLOW:
		// Get component properties
		CompNum = AirflowNetworkCompData( j ).TypeNum;
		ed = Rough / DisSysCompDuctData( CompNum ).D;
		area = pow_2( DisSysCompCoilData( CompNum ).D ) * Pi;
		ld = DisSysCompCoilData( CompNum ).L / DisSysCompCoilData( CompNum ).D;
		g = 1.14 - 0.868589 * std::log( ed );
		AA1 = g;

		NF = 1;
		if ( LFLAG == 1 ) {
			// Initialization by linear relation.
			if ( PDROP >= 0.0 ) {
				DF( 1 ) = ( 2.0 * RHOZ( n ) * area * DisSysCompCoilData( CompNum ).D ) / ( VISCZ( n ) * InitLamCoef * ld );
			} else {
				DF( 1 ) = ( 2.0 * RHOZ( M ) * area * DisSysCompCoilData( CompNum ).D ) / ( VISCZ( M ) * InitLamCoef * ld );
			}
			F( 1 ) = -DF( 1 ) * PDROP;
			if ( LIST >= 4 ) gio::write( Unit21, Format_901 ) << " dwi:" << i << InitLamCoef << F( 1 ) << DF( 1 );
		} else {
			// Standard calculation.
			if ( PDROP >= 0.0 ) {
				// Flow in positive direction.
				// Laminar flow coefficient !=0
				if ( LamFriCoef >= 0.001 ) {
					A2 = LamFriCoef / ( 2.0 * RHOZ( n ) * area * area );
					A1 = ( VISCZ( n ) * LamDynCoef * ld ) / ( 2.0 * RHOZ( n ) * area * DisSysCompCoilData( CompNum ).D );
					A0 = -PDROP;
					CDM = std::sqrt( A1 * A1 - 4.0 * A2 * A0 );
					FL = ( CDM - A1 ) / ( 2.0 * A2 );
					CDM = 1.0 / CDM;
				} else {
					CDM = ( 2.0 * RHOZ( n ) * area * DisSysCompCoilData( CompNum ).D ) / ( VISCZ( n ) * LamDynCoef * ld );
					FL = CDM * PDROP;
				}
				RE = FL * DisSysCompCoilData( CompNum ).D / ( VISCZ( n ) * area );
				if ( LIST >= 4 ) gio::write( Unit21, Format_901 ) << " dwl:" << i << PDROP << FL << CDM << RE;
				// Turbulent flow; test when Re>10.
				if ( RE >= 10.0 ) {
					S2 = std::sqrt( 2.0 * RHOZ( n ) * PDROP ) * area;
					FTT = S2 / std::sqrt( ld / pow_2( g ) + TurDynCoef );
					if ( LIST >= 4 ) gio::write( Unit21, Format_901 ) << " dwt:" << i << S2 << FTT << g;
					while ( true ) {
						FT = FTT;
						B = ( 9.3 * VISCZ( n ) * area ) / ( FT * Rough );
						D = 1.0 + g * B;
						g -= ( g - AA1 + C * std::log( D ) ) / ( 1.0 + C * B / D );
						FTT = S2 / std::sqrt( ld / pow_2( g ) + TurDynCoef );
						if ( LIST >= 4 ) gio::write( Unit21, Format_901 ) << " dwt:" << i << B << FTT << g;
						if ( std::abs( FTT - FT ) / FTT < EPS ) break;
					}
					FT = FTT;
				} else {
					FT = FL;
				}
			} else {
				// Flow in negative direction.
				// Laminar flow coefficient !=0
				if ( LamFriCoef >= 0.001 ) {
					A2 = LamFriCoef / ( 2.0 * RHOZ( M ) * area * area );
					A1 = ( VISCZ( M ) * LamDynCoef * ld ) / ( 2.0 * RHOZ( M ) * area * DisSysCompCoilData( CompNum ).D );
					A0 = PDROP;
					CDM = std::sqrt( A1 * A1 - 4.0 * A2 * A0 );
					FL = -( CDM - A1 ) / ( 2.0 * A2 );
					CDM = 1.0 / CDM;
				} else {
					CDM = ( 2.0 * RHOZ( M ) * area * DisSysCompCoilData( CompNum ).D ) / ( VISCZ( M ) * LamDynCoef * ld );
					FL = CDM * PDROP;
				}
				RE = -FL * DisSysCompCoilData( CompNum ).D / ( VISCZ( M ) * area );
				if ( LIST >= 4 ) gio::write( Unit21, Format_901 ) << " dwl:" << i << PDROP << FL << CDM << RE;
				// Turbulent flow; test when Re>10.
				if ( RE >= 10.0 ) {
					S2 = std::sqrt( -2.0 * RHOZ( M ) * PDROP ) * area;
					FTT = S2 / std::sqrt( ld / pow_2( g ) + TurDynCoef );
					if ( LIST >= 4 ) gio::write( Unit21, Format_901 ) << " dwt:" << i << S2 << FTT << g;
					while ( true ) {
						FT = FTT;
						B = ( 9.3 * VISCZ( M ) * area ) / ( FT * Rough );
						D = 1.0 + g * B;
						g -= ( g - AA1 + C * std::log( D ) ) / ( 1.0 + C * B / D );
						FTT = S2 / std::sqrt( ld / pow_2( g ) + TurDynCoef );
						if ( LIST >= 4 ) gio::write( Unit21, Format_901 ) << " dwt:" << i << B << FTT << g;
						if ( std::abs( FTT - FT ) / FTT < EPS ) break;
					}
					FT = -FTT;
				} else {
					FT = FL;
				}
			}
			// Select laminar or turbulent flow.
			if ( std::abs( FL ) <= std::abs( FT ) ) {
				F( 1 ) = FL;
				DF( 1 ) = CDM;
			} else {
				F( 1 ) = FT;
				DF( 1 ) = 0.5 * FT / PDROP;
			}
		}

	}

	void
	AFETMU(
		int const j, // Component number
		int const LFLAG, // Initialization flag.If = 1, use laminar relationship
		Real64 const PDROP, // Total pressure drop across a component (P1 - P2) [Pa]
		int const i, // Linkage number
		int const n, // Node 1 number
		int const M, // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         George Walton
		//       DATE WRITTEN   Extracted from AIRNET
		//       MODIFIED       Lixing Gu, 2/1/04
		//                      Revised the subroutine to meet E+ needs
		//       MODIFIED       Lixing Gu, 6/8/05
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine solves airflow for a terminal unit component

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataLoopNode::Node;

		// Argument array dimensioning
		F.dim( 2 );
		DF.dim( 2 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const C( 0.868589 );
		Real64 const EPS( 0.001 );
		Real64 const Rough( 0.0001 );
		Real64 const InitLamCoef( 128.0 );
		Real64 const LamDynCoef( 64.0 );
		Real64 const LamFriCoef( 0.0001 );
		Real64 const TurDynCoef( 0.0001 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 A0;
		Real64 A1;
		Real64 A2;
		Real64 B;
		Real64 D;
		Real64 S2;
		Real64 CDM;
		Real64 FL;
		Real64 FT;
		Real64 FTT;
		Real64 RE;
		int CompNum;
		Real64 ed;
		Real64 ld;
		Real64 g;
		Real64 AA1;
		Real64 area;

		// Formats
		static gio::Fmt Format_901( "(A5,I3,6X,4E16.7)" );

		// FLOW:
		// Get component properties
		CompNum = AirflowNetworkCompData( j ).TypeNum;
		ed = Rough / DisSysCompTermUnitData( CompNum ).D;
		area = pow_2( DisSysCompTermUnitData( CompNum ).D ) * Pi;
		ld = DisSysCompTermUnitData( CompNum ).L / DisSysCompTermUnitData( CompNum ).D;
		g = 1.14 - 0.868589 * std::log( ed );
		AA1 = g;

		NF = 1;
		if ( LFLAG == 1 ) {
			// Initialization by linear relation.
			if ( PDROP >= 0.0 ) {
				DF( 1 ) = ( 2.0 * RHOZ( n ) * area * DisSysCompTermUnitData( CompNum ).D ) / ( VISCZ( n ) * InitLamCoef * ld );
			} else {
				DF( 1 ) = ( 2.0 * RHOZ( M ) * area * DisSysCompTermUnitData( CompNum ).D ) / ( VISCZ( M ) * InitLamCoef * ld );
			}
			F( 1 ) = -DF( 1 ) * PDROP;
			if ( LIST >= 4 ) gio::write( Unit21, Format_901 ) << " dwi:" << i << InitLamCoef << F( 1 ) << DF( 1 );
		} else {
			// Standard calculation.
			if ( PDROP >= 0.0 ) {
				// Flow in positive direction.
				// Laminar flow coefficient !=0
				if ( LamFriCoef >= 0.001 ) {
					A2 = LamFriCoef / ( 2.0 * RHOZ( n ) * area * area );
					A1 = ( VISCZ( n ) * LamDynCoef * ld ) / ( 2.0 * RHOZ( n ) * area * DisSysCompTermUnitData( CompNum ).D );
					A0 = -PDROP;
					CDM = std::sqrt( A1 * A1 - 4.0 * A2 * A0 );
					FL = ( CDM - A1 ) / ( 2.0 * A2 );
					CDM = 1.0 / CDM;
				} else {
					CDM = ( 2.0 * RHOZ( n ) * area * DisSysCompTermUnitData( CompNum ).D ) / ( VISCZ( n ) * LamDynCoef * ld );
					FL = CDM * PDROP;
				}
				RE = FL * DisSysCompTermUnitData( CompNum ).D / ( VISCZ( n ) * area );
				if ( LIST >= 4 ) gio::write( Unit21, Format_901 ) << " dwl:" << i << PDROP << FL << CDM << RE;
				// Turbulent flow; test when Re>10.
				if ( RE >= 10.0 ) {
					S2 = std::sqrt( 2.0 * RHOZ( n ) * PDROP ) * area;
					FTT = S2 / std::sqrt( ld / pow_2( g ) + TurDynCoef );
					if ( LIST >= 4 ) gio::write( Unit21, Format_901 ) << " dwt:" << i << S2 << FTT << g;
					while ( true ) {
						FT = FTT;
						B = ( 9.3 * VISCZ( n ) * area ) / ( FT * Rough );
						D = 1.0 + g * B;
						g -= ( g - AA1 + C * std::log( D ) ) / ( 1.0 + C * B / D );
						FTT = S2 / std::sqrt( ld / pow_2( g ) + TurDynCoef );
						if ( LIST >= 4 ) gio::write( Unit21, Format_901 ) << " dwt:" << i << B << FTT << g;
						if ( std::abs( FTT - FT ) / FTT < EPS ) break;
					}
					FT = FTT;
				} else {
					FT = FL;
				}
			} else {
				// Flow in negative direction.
				// Laminar flow coefficient !=0
				if ( LamFriCoef >= 0.001 ) {
					A2 = LamFriCoef / ( 2.0 * RHOZ( M ) * area * area );
					A1 = ( VISCZ( M ) * LamDynCoef * ld ) / ( 2.0 * RHOZ( M ) * area * DisSysCompTermUnitData( CompNum ).D );
					A0 = PDROP;
					CDM = std::sqrt( A1 * A1 - 4.0 * A2 * A0 );
					FL = -( CDM - A1 ) / ( 2.0 * A2 );
					CDM = 1.0 / CDM;
				} else {
					CDM = ( 2.0 * RHOZ( M ) * area * DisSysCompTermUnitData( CompNum ).D ) / ( VISCZ( M ) * LamDynCoef * ld );
					FL = CDM * PDROP;
				}
				RE = -FL * DisSysCompTermUnitData( CompNum ).D / ( VISCZ( M ) * area );
				if ( LIST >= 4 ) gio::write( Unit21, Format_901 ) << " dwl:" << i << PDROP << FL << CDM << RE;
				// Turbulent flow; test when Re>10.
				if ( RE >= 10.0 ) {
					S2 = std::sqrt( -2.0 * RHOZ( M ) * PDROP ) * area;
					FTT = S2 / std::sqrt( ld / pow_2( g ) + TurDynCoef );
					if ( LIST >= 4 ) gio::write( Unit21, Format_901 ) << " dwt:" << i << S2 << FTT << g;
					while ( true ) {
						FT = FTT;
						B = ( 9.3 * VISCZ( M ) * area ) / ( FT * Rough );
						D = 1.0 + g * B;
						g -= ( g - AA1 + C * std::log( D ) ) / ( 1.0 + C * B / D );
						FTT = S2 / std::sqrt( ld / pow_2( g ) + TurDynCoef );
						if ( LIST >= 4 ) gio::write( Unit21, Format_901 ) << " dwt:" << i << B << FTT << g;
						if ( std::abs( FTT - FT ) / FTT < EPS ) break;
					}
					FT = -FTT;
				} else {
					FT = FL;
				}
			}
			// Select laminar or turbulent flow.
			if ( std::abs( FL ) <= std::abs( FT ) ) {
				F( 1 ) = FL;
				DF( 1 ) = CDM;
			} else {
				F( 1 ) = FT;
				DF( 1 ) = 0.5 * FT / PDROP;
			}
		}
		// If damper, setup the airflows from nodal values calculated from teminal
		if ( AirflowNetworkLinkageData( i ).VAVTermDamper ) {
			F( 1 ) = Node( DisSysCompTermUnitData( CompNum ).DamperInletNode ).MassFlowRate;
			if ( VAVTerminalRatio > 0.0 ) {
				F( 1 ) *= VAVTerminalRatio;
			}
			DF( 1 ) = 0.0;
		}

	}

	void
	AFEEXF(
		int const j, // Component number
		int const LFLAG, // Initialization flag.If = 1, use laminar relationship
		Real64 const PDROP, // Total pressure drop across a component (P1 - P2) [Pa]
		int const i, // Linkage number
		int const n, // Node 1 number
		int const M, // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         George Walton
		//       DATE WRITTEN   Extracted from AIRNET
		//       MODIFIED       Lixing Gu, 12/17/06
		//                      Revised for zone exhaust fan
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine solves airflow for a surface crack component

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataLoopNode::Node;
		using DataHVACGlobals::VerySmallMassFlow;

		// Argument array dimensioning
		F.dim( 2 );
		DF.dim( 2 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 CDM;
		Real64 FL;
		Real64 FT;
		Real64 RhozNorm;
		Real64 VisczNorm;
		Real64 expn;
		Real64 Ctl;
		Real64 coef;
		Real64 Corr;
		Real64 VisAve;
		Real64 Tave;
		Real64 RhoCor;
		int CompNum;
		int InletNode;

		// Formats
		static gio::Fmt Format_901( "(A5,I3,6X,4E16.7)" );

		// FLOW:
		CompNum = AirflowNetworkCompData( j ).TypeNum;
		InletNode = MultizoneCompExhaustFanData( CompNum ).InletNode;
		if ( Node( InletNode ).MassFlowRate > VerySmallMassFlow ) {
			// Treat the component as an exhaust fan
			NF = 1;
			F( 1 ) = Node( InletNode ).MassFlowRate;
			DF( 1 ) = 0.0;
			return;
		} else {
			// Treat the component as a surface crack
			// Crack standard condition from given inputs
			Corr = MultizoneSurfaceData( i ).Factor;
			RhozNorm = PsyRhoAirFnPbTdbW( MultizoneCompExhaustFanData( CompNum ).StandardP, MultizoneCompExhaustFanData( CompNum ).StandardT, MultizoneCompExhaustFanData( CompNum ).StandardW );
			VisczNorm = 1.71432e-5 + 4.828e-8 * MultizoneCompExhaustFanData( CompNum ).StandardT;

			expn = MultizoneCompExhaustFanData( CompNum ).FlowExpo;
			VisAve = ( VISCZ( n ) + VISCZ( M ) ) / 2.0;
			Tave = ( TZ( n ) + TZ( M ) ) / 2.0;
			if ( PDROP >= 0.0 ) {
				coef = MultizoneCompExhaustFanData( CompNum ).FlowCoef / SQRTDZ( n ) * Corr;
			} else {
				coef = MultizoneCompExhaustFanData( CompNum ).FlowCoef / SQRTDZ( M ) * Corr;
			}

			NF = 1;
			if ( LFLAG == 1 ) {
				// Initialization by linear relation.
				if ( PDROP >= 0.0 ) {
					RhoCor = ( TZ( n ) + KelvinConv ) / ( Tave + KelvinConv );
					Ctl = std::pow( RhozNorm / RHOZ( n ) / RhoCor, expn - 1.0 ) * std::pow( VisczNorm / VisAve, 2.0 * expn - 1.0 );
					DF( 1 ) = coef * RHOZ( n ) / VISCZ( n ) * Ctl;
				} else {
					RhoCor = ( TZ( M ) + KelvinConv ) / ( Tave + KelvinConv );
					Ctl = std::pow( RhozNorm / RHOZ( M ) / RhoCor, expn - 1.0 ) * std::pow( VisczNorm / VisAve, 2.0 * expn - 1.0 );
					DF( 1 ) = coef * RHOZ( M ) / VISCZ( M ) * Ctl;
				}
				F( 1 ) = -DF( 1 ) * PDROP;
			} else {
				// Standard calculation.
				if ( PDROP >= 0.0 ) {
					// Flow in positive direction.
					// Laminar flow.
					RhoCor = ( TZ( n ) + KelvinConv ) / ( Tave + KelvinConv );
					Ctl = std::pow( RhozNorm / RHOZ( n ) / RhoCor, expn - 1.0 ) * std::pow( VisczNorm / VisAve, 2.0 * expn - 1.0 );
					CDM = coef * RHOZ( n ) / VISCZ( n ) * Ctl;
					FL = CDM * PDROP;
					// Turbulent flow.
					if ( expn == 0.5 ) {
						FT = coef * SQRTDZ( n ) * std::sqrt( PDROP ) * Ctl;
					} else {
						FT = coef * SQRTDZ( n ) * std::pow( PDROP, expn ) * Ctl;
					}
				} else {
					// Flow in negative direction.
					// Laminar flow.
					RhoCor = ( TZ( M ) + KelvinConv ) / ( Tave + KelvinConv );
					Ctl = std::pow( RhozNorm / RHOZ( M ) / RhoCor, expn - 1.0 ) * std::pow( VisczNorm / VisAve, 2.0 * expn - 1.0 );
					CDM = coef * RHOZ( M ) / VISCZ( M ) * Ctl;
					FL = CDM * PDROP;
					// Turbulent flow.
					if ( expn == 0.5 ) {
						FT = -coef * SQRTDZ( M ) * std::sqrt( -PDROP ) * Ctl;
					} else {
						FT = -coef * SQRTDZ( M ) * std::pow( -PDROP, expn ) * Ctl;
					}
				}
				// Select laminar or turbulent flow.
				if ( LIST >= 4 ) gio::write( Unit21, Format_901 ) << " scr: " << i << PDROP << FL << FT;
				if ( std::abs( FL ) <= std::abs( FT ) ) {
					F( 1 ) = FL;
					DF( 1 ) = CDM;
				} else {
					F( 1 ) = FT;
					DF( 1 ) = FT * expn / PDROP;
				}
			}
		}
	}

	void
	AFEHEX(
		int const j, // Component number
		int const LFLAG, // Initialization flag.If = 1, use laminar relationship
		Real64 const PDROP, // Total pressure drop across a component (P1 - P2) [Pa]
		int const EP_UNUSED( i ), // Linkage number
		int const n, // Node 1 number
		int const M, // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         George Walton
		//       DATE WRITTEN   Extracted from AIRNET
		//       MODIFIED       Lixing Gu, 2/1/04
		//                      Revised the subroutine to meet E+ needs
		//       MODIFIED       Lixing Gu, 1/18/09
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine solves airflow for a heat exchanger component

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		F.dim( 2 );
		DF.dim( 2 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const C( 0.868589 );
		Real64 const EPS( 0.001 );
		Real64 const Rough( 0.0001 );
		Real64 const InitLamCoef( 128.0 );
		Real64 const LamDynCoef( 64.0 );
		Real64 const LamFriCoef( 0.0001 );
		Real64 const TurDynCoef( 0.0001 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 A0;
		Real64 A1;
		Real64 A2;
		Real64 B;
		Real64 D;
		Real64 S2;
		Real64 CDM;
		Real64 FL;
		Real64 FT;
		Real64 FTT;
		Real64 RE;
		int CompNum;
		Real64 ed;
		Real64 ld;
		Real64 g;
		Real64 AA1;
		Real64 area;

		// FLOW:
		// Get component properties
		CompNum = AirflowNetworkCompData( j ).TypeNum;
		ed = Rough / DisSysCompHXData( CompNum ).D;
		area = pow_2( DisSysCompHXData( CompNum ).D ) * Pi;
		ld = DisSysCompHXData( CompNum ).L / DisSysCompHXData( CompNum ).D;
		g = 1.14 - 0.868589 * std::log( ed );
		AA1 = g;

		NF = 1;
		if ( LFLAG == 1 ) {
			// Initialization by linear relation.
			if ( PDROP >= 0.0 ) {
				DF( 1 ) = ( 2.0 * RHOZ( n ) * area * DisSysCompHXData( CompNum ).D ) / ( VISCZ( n ) * InitLamCoef * ld );
			} else {
				DF( 1 ) = ( 2.0 * RHOZ( M ) * area * DisSysCompHXData( CompNum ).D ) / ( VISCZ( M ) * InitLamCoef * ld );
			}
			F( 1 ) = -DF( 1 ) * PDROP;
		} else {
			// Standard calculation.
			if ( PDROP >= 0.0 ) {
				// Flow in positive direction.
				// Laminar flow coefficient !=0
				if ( LamFriCoef >= 0.001 ) {
					A2 = LamFriCoef / ( 2.0 * RHOZ( n ) * area * area );
					A1 = ( VISCZ( n ) * LamDynCoef * ld ) / ( 2.0 * RHOZ( n ) * area * DisSysCompHXData( CompNum ).D );
					A0 = -PDROP;
					CDM = std::sqrt( A1 * A1 - 4.0 * A2 * A0 );
					FL = ( CDM - A1 ) / ( 2.0 * A2 );
					CDM = 1.0 / CDM;
				} else {
					CDM = ( 2.0 * RHOZ( n ) * area * DisSysCompHXData( CompNum ).D ) / ( VISCZ( n ) * LamDynCoef * ld );
					FL = CDM * PDROP;
				}
				RE = FL * DisSysCompHXData( CompNum ).D / ( VISCZ( n ) * area );
				// Turbulent flow; test when Re>10.
				if ( RE >= 10.0 ) {
					S2 = std::sqrt( 2.0 * RHOZ( n ) * PDROP ) * area;
					FTT = S2 / std::sqrt( ld / pow_2( g ) + TurDynCoef );
					while ( true ) {
						FT = FTT;
						B = ( 9.3 * VISCZ( n ) * area ) / ( FT * Rough );
						D = 1.0 + g * B;
						g -= ( g - AA1 + C * std::log( D ) ) / ( 1.0 + C * B / D );
						FTT = S2 / std::sqrt( ld / pow_2( g ) + TurDynCoef );
						if ( std::abs( FTT - FT ) / FTT < EPS ) break;
					}
					FT = FTT;
				} else {
					FT = FL;
				}
			} else {
				// Flow in negative direction.
				// Laminar flow coefficient !=0
				if ( LamFriCoef >= 0.001 ) {
					A2 = LamFriCoef / ( 2.0 * RHOZ( M ) * area * area );
					A1 = ( VISCZ( M ) * LamDynCoef * ld ) / ( 2.0 * RHOZ( M ) * area * DisSysCompHXData( CompNum ).D );
					A0 = PDROP;
					CDM = std::sqrt( A1 * A1 - 4.0 * A2 * A0 );
					FL = -( CDM - A1 ) / ( 2.0 * A2 );
					CDM = 1.0 / CDM;
				} else {
					CDM = ( 2.0 * RHOZ( M ) * area * DisSysCompHXData( CompNum ).D ) / ( VISCZ( M ) * LamDynCoef * ld );
					FL = CDM * PDROP;
				}
				RE = -FL * DisSysCompHXData( CompNum ).D / ( VISCZ( M ) * area );
				// Turbulent flow; test when Re>10.
				if ( RE >= 10.0 ) {
					S2 = std::sqrt( -2.0 * RHOZ( M ) * PDROP ) * area;
					FTT = S2 / std::sqrt( ld / pow_2( g ) + TurDynCoef );
					while ( true ) {
						FT = FTT;
						B = ( 9.3 * VISCZ( M ) * area ) / ( FT * Rough );
						D = 1.0 + g * B;
						g -= ( g - AA1 + C * std::log( D ) ) / ( 1.0 + C * B / D );
						FTT = S2 / std::sqrt( ld / pow_2( g ) + TurDynCoef );
						if ( std::abs( FTT - FT ) / FTT < EPS ) break;
					}
					FT = -FTT;
				} else {
					FT = FL;
				}
			}
			// Select laminar or turbulent flow.
			if ( std::abs( FL ) <= std::abs( FT ) ) {
				F( 1 ) = FL;
				DF( 1 ) = CDM;
			} else {
				F( 1 ) = FT;
				DF( 1 ) = 0.5 * FT / PDROP;
			}
		}

	}

	void
	AFEHOP(
		int const j, // Component number
		int const LFLAG, // Initialization flag.If = 1, use laminar relationship
		Real64 const PDROP, // Total pressure drop across a component (P1 - P2) [Pa]
		int const i, // Linkage number
		int const n, // Node 1 number
		int const M, // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Lixing Gu
		//       DATE WRITTEN   Apr. 2009
		//       MODIFIED       na
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine solves airflow for a horizontal opening component. The subroutine was
		// developed based on the subroutine AFEPLR of AIRNET.

		// METHODOLOGY EMPLOYED:
		// Combine forced and buyancy airflows together with a cap

		// REFERENCES:
		// Cooper, L., 1989, "Calculation of the Flow Through a Horizontal Ceiling/Floor Vent,"
		// NISTIR 89-4052, National Institute of Standards and Technology, Gaithersburg, MD

		// USE STATEMENTS:
		using DataGlobals::Pi;

		// Argument array dimensioning
		F.dim( 2 );
		DF.dim( 2 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 RhozAver;
		Real64 expn;
		Real64 coef;
		int CompNum;
		Real64 Width; // Opening width
		Real64 Height; // Opening height
		Real64 Fact; // Opening factor
		Real64 Slope; // Opening slope
		Real64 DischCoeff; // Discharge coefficient
		Real64 fma12; // massflow in direction "from-to" [kg/s]
		Real64 fma21; // massflow in direction "to-from" [kg/s]
		Real64 dp1fma12; // derivative d fma12 / d Dp [kg/s/Pa]
		Real64 dp1fma21; // derivative d fma21 / d Dp [kg/s/Pa]
		Real64 PurgedP; // Purge pressure [Pa]
		Real64 BuoFlow; // Buoyancy flow rate [Pa]
		Real64 BuoFlowMax; // Maximum buoyancy flow rate [Pa]
		Real64 dPBuoFlow; // Derivative of buoyancy flow rate [kg/s/Pa]
		Real64 DH; // Hydraulic diameter [m]
		Real64 Cshape; // Shape factor [dimensionless]
		Real64 OpenArea; // Opening area [m2]

		// FLOW:
		// Get information on the horizontal opening
		CompNum = AirflowNetworkCompData( j ).TypeNum;
		RhozAver = ( RHOZ( n ) + RHOZ( M ) ) / 2.0;
		Width = MultizoneSurfaceData( i ).Width;
		Height = MultizoneSurfaceData( i ).Height;
		Fact = MultizoneSurfaceData( i ).OpenFactor;
		expn = MultizoneCompHorOpeningData( CompNum ).FlowExpo;
		coef = MultizoneCompHorOpeningData( CompNum ).FlowCoef;
		Slope = MultizoneCompHorOpeningData( CompNum ).Slope;
		DischCoeff = MultizoneCompHorOpeningData( CompNum ).DischCoeff;
		Cshape = 0.942 * Width / Height;
		OpenArea = Width * Height * Fact * std::sin( Slope * Pi / 180.0 ) * ( 1.0 + std::cos( Slope * Pi / 180.0 ) );
		DH = 4.0 * ( Width * Height ) / 2.0 / ( Width + Height ) * Fact;

		// Check which zone is higher
		NF = 1;
		if ( Fact == 0.0 ) {
			GenericCrack( coef, expn, LFLAG, PDROP, n, M, F, DF, NF );
			return;
		}

		fma12 = 0.0;
		fma21 = 0.0;
		dp1fma12 = 0.0;
		dp1fma21 = 0.0;
		BuoFlow = 0.0;
		dPBuoFlow = 0.0;

		if ( AirflowNetworkLinkageData( i ).NodeHeights( 1 ) > AirflowNetworkLinkageData( i ).NodeHeights( 2 ) ) {
			// Node N is upper zone
			if ( RHOZ( n ) > RHOZ( M ) ) {
				BuoFlowMax = RhozAver * 0.055 * std::sqrt( 9.81 * std::abs( RHOZ( n ) - RHOZ( M ) ) * pow_5( DH ) / RhozAver );
				PurgedP = Cshape * Cshape * 9.81 * std::abs( RHOZ( n ) - RHOZ( M ) ) * pow_5( DH ) / ( 2.0 * pow_2( OpenArea ) );
				if ( std::abs( PDROP ) <= PurgedP ) {
					BuoFlow = BuoFlowMax * ( 1.0 - std::abs( PDROP ) / PurgedP );
					dPBuoFlow = BuoFlowMax / PurgedP;
				}
			}
		} else {
			// Node M is upper zone
			if ( RHOZ( n ) < RHOZ( M ) ) {
				BuoFlowMax = RhozAver * 0.055 * std::sqrt( 9.81 * std::abs( RHOZ( n ) - RHOZ( M ) ) * pow_5( DH ) / RhozAver );
				PurgedP = Cshape * Cshape * 9.81 * std::abs( RHOZ( n ) - RHOZ( M ) ) * pow_5( DH ) / ( 2.0 * pow_2( OpenArea ) );
				if ( std::abs( PDROP ) <= PurgedP ) {
					BuoFlow = BuoFlowMax * ( 1.0 - std::abs( PDROP ) / PurgedP );
					dPBuoFlow = BuoFlowMax / PurgedP;
				}
			}
		}

		if ( PDROP == 0.0 ) {
			fma12 = BuoFlow;
			fma21 = BuoFlow;
			dp1fma12 = 0.0;
			dp1fma21 = 0.0;
		} else if ( PDROP > 0.0 ) {
			fma12 = RHOZ( n ) * OpenArea * Fact * DischCoeff * std::sqrt( 2.0 * PDROP / RhozAver ) + BuoFlow;
			dp1fma12 = RHOZ( n ) * OpenArea * DischCoeff / std::sqrt( 2.0 * PDROP * RhozAver ) + dPBuoFlow;
			if ( BuoFlow > 0.0 ) {
				fma21 = BuoFlow;
				dp1fma21 = dPBuoFlow;
			}
		} else { // PDROP.LT.0.0
			fma21 = RHOZ( M ) * OpenArea * Fact * DischCoeff * std::sqrt( 2.0 * std::abs( PDROP ) / RhozAver ) + BuoFlow;
			dp1fma21 = -RHOZ( M ) * OpenArea * DischCoeff / std::sqrt( 2.0 * std::abs( PDROP ) * RhozAver ) + dPBuoFlow;
			if ( BuoFlow > 0.0 ) {
				fma12 = BuoFlow;
				dp1fma12 = dPBuoFlow;
			}
		}

		F( 1 ) = fma12 - fma21;
		DF( 1 ) = dp1fma12 - dp1fma21;
		F( 2 ) = 0.0;
		if ( fma12 != 0.0 && fma21 != 0.0 ) {
			F( 2 ) = BuoFlow;
		}
		DF( 2 ) = 0.0;

	}

	void
	GenericCrack(
		Real64 & coef, // Flow coefficient
		Real64 const expn, // Flow exponent
		int const LFLAG, // Initialization flag.If = 1, use laminar relationship
		Real64 const PDROP, // Total pressure drop across a component (P1 - P2) [Pa]
		int const n, // Node 1 number
		int const M, // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         George Walton
		//       DATE WRITTEN   Extracted from AIRNET
		//       MODIFIED       Lixing Gu, 2/1/04
		//                      Revised the subroutine to meet E+ needs
		//       MODIFIED       Lixing Gu, 6/8/05
		//       RE-ENGINEERED  This subroutine is revised from AFEPLR developed by George Walton, NIST

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine solves airflow for a power law component

		// METHODOLOGY EMPLOYED:
		// Using Q=C(dP)^n

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		F.dim( 2 );
		DF.dim( 2 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 CDM;
		Real64 FL;
		Real64 FT;
		Real64 RhozNorm;
		Real64 VisczNorm;
		Real64 Ctl;
		Real64 VisAve;
		Real64 Tave;
		Real64 RhoCor;

		// Formats
		static gio::Fmt Format_901( "(A5,6X,4E16.7)" );

		// FLOW:
		// Calculate normal density and viscocity at Crack standard condition: T=20C, p=101325 Pa and 0 g/kg
		RhozNorm = PsyRhoAirFnPbTdbW( 101325.0, 20.0, 0.0 );
		VisczNorm = 1.71432e-5 + 4.828e-8 * 20.0;
		VisAve = ( VISCZ( n ) + VISCZ( M ) ) / 2.0;
		Tave = ( TZ( n ) + TZ( M ) ) / 2.0;
		if ( PDROP >= 0.0 ) {
			coef /= SQRTDZ( n );
		} else {
			coef /= SQRTDZ( M );
		}

		NF = 1;
		if ( LFLAG == 1 ) {
			// Initialization by linear relation.
			if ( PDROP >= 0.0 ) {
				RhoCor = ( TZ( n ) + KelvinConv ) / ( Tave + KelvinConv );
				Ctl = std::pow( RhozNorm / RHOZ( n ) / RhoCor, expn - 1.0 ) * std::pow( VisczNorm / VisAve, 2.0 * expn - 1.0 );
				DF( 1 ) = coef * RHOZ( n ) / VISCZ( n ) * Ctl;
			} else {
				RhoCor = ( TZ( M ) + KelvinConv ) / ( Tave + KelvinConv );
				Ctl = std::pow( RhozNorm / RHOZ( M ) / RhoCor, expn - 1.0 ) * std::pow( VisczNorm / VisAve, 2.0 * expn - 1.0 );
				DF( 1 ) = coef * RHOZ( M ) / VISCZ( M ) * Ctl;
			}
			F( 1 ) = -DF( 1 ) * PDROP;
		} else {
			// Standard calculation.
			if ( PDROP >= 0.0 ) {
				// Flow in positive direction.
				// Laminar flow.
				RhoCor = ( TZ( n ) + KelvinConv ) / ( Tave + KelvinConv );
				Ctl = std::pow( RhozNorm / RHOZ( n ) / RhoCor, expn - 1.0 ) * std::pow( VisczNorm / VisAve, 2.0 * expn - 1.0 );
				CDM = coef * RHOZ( n ) / VISCZ( n ) * Ctl;
				FL = CDM * PDROP;
				// Turbulent flow.
				if ( expn == 0.5 ) {
					FT = coef * SQRTDZ( n ) * std::sqrt( PDROP ) * Ctl;
				} else {
					FT = coef * SQRTDZ( n ) * std::pow( PDROP, expn ) * Ctl;
				}
			} else {
				// Flow in negative direction.
				// Laminar flow.
				RhoCor = ( TZ( M ) + KelvinConv ) / ( Tave + KelvinConv );
				Ctl = std::pow( RhozNorm / RHOZ( M ) / RhoCor, 2.0 * expn - 1.0 ) * std::pow( VisczNorm / VisAve, 2.0 * expn - 1.0 );
				CDM = coef * RHOZ( M ) / VISCZ( M ) * Ctl;
				FL = CDM * PDROP;
				// Turbulent flow.
				if ( expn == 0.5 ) {
					FT = -coef * SQRTDZ( M ) * std::sqrt( -PDROP ) * Ctl;
				} else {
					FT = -coef * SQRTDZ( M ) * std::pow( -PDROP, expn ) * Ctl;
				}
			}
			// Select laminar or turbulent flow.
			if ( LIST >= 4 ) gio::write( Unit21, Format_901 ) << " generic crack: " << PDROP << FL << FT;
			if ( std::abs( FL ) <= std::abs( FT ) ) {
				F( 1 ) = FL;
				DF( 1 ) = CDM;
			} else {
				F( 1 ) = FT;
				DF( 1 ) = FT * expn / PDROP;
			}
		}
	}

	void
	FACSKY(
		Array1A< Real64 > AU, // the upper triangle of [A] before and after factoring
		Array1A< Real64 > AD, // the main diagonal of [A] before and after factoring
		Array1A< Real64 > AL, // the lower triangle of [A] before and after factoring
		Array1A_int const IK, // pointer to the top of column/row "K"
		int const NEQ, // number of equations
		int const NSYM // symmetry:  0 = symmetric matrix, 1 = non-symmetric
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         George Walton
		//       DATE WRITTEN   Extracted from AIRNET
		//       MODIFIED       Lixing Gu, 2/1/04
		//                      Revised the subroutine to meet E+ needs
		//       MODIFIED       Lixing Gu, 6/8/05
		//       RE-ENGINEERED  This subroutine is revised from FACSKY developed by George Walton, NIST

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine performs L-U factorization of a skyline ordered matrix, [A]
		// The algorithm has been restructured for clarity.
		// Note dependence on compiler for optimizing the inner do loops.

		// METHODOLOGY EMPLOYED:
		//     L-U factorization of a skyline ordered matrix, [A], used for
		//     solution of simultaneous linear algebraic equations [A] * X = B.
		//     No pivoting!  No scaling!  No warnings!!!
		//     Related routines:  SLVSKY, SETSKY, FILSKY.

		// REFERENCES:
		//     Algorithm is described in "The Finite Element Method Displayed",
		//     by G. Dhatt and G. Touzot, John Wiley & Sons, New York, 1984.

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		AU.dim( IK(NetworkNumOfNodes+1) );
		AD.dim( NetworkNumOfNodes );
		AL.dim( IK(NetworkNumOfNodes+1)-1 );
		IK.dim( NetworkNumOfNodes+1 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// noel, GNU says the AU is indexed above its upper bound
		//REAL(r64), INTENT(INOUT) :: AU(IK(NetworkNumOfNodes+1)-1) ! the upper triangle of [A] before and after factoring

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int JHK;
		int JHK1;
		int LHK;
		int LHK1;
		int IMIN;
		int IMIN1;
		int JHJ;
		int JHJ1;
		int IC;
		int i;
		int j;
		int k;
		Real64 T1;
		Real64 T2;
		Real64 SDOT;
		Real64 SUMD;

		// FLOW:
		AD( 1 ) = 1.0 / AD( 1 );
		JHK = 1;
		for ( k = 2; k <= NEQ; ++k ) {
			SUMD = 0.0;
			JHK1 = IK( k + 1 );
			LHK = JHK1 - JHK;
			if ( LHK > 0 ) {
				LHK1 = LHK - 1;
				IMIN = k - LHK1;
				IMIN1 = IMIN - 1;
				if ( NSYM == 1 ) AL( JHK ) *= AD( IMIN1 );
				if ( LHK1 != 0 ) {
					JHJ = IK( IMIN );
					if ( NSYM == 0 ) {
						for ( j = 1; j <= LHK1; ++j ) {
							JHJ1 = IK( IMIN + j );
							IC = min( j, JHJ1 - JHJ );
							if ( IC > 0 ) {
								SDOT = 0.0;
								for ( i = 0; i <= IC - 1; ++i ) {
									SDOT += AU( JHJ1 - IC + i ) * AU( JHK + j - IC + i );
								}
								AU( JHK + j ) -= SDOT;
							}
							JHJ = JHJ1;
						}
					} else {
						for ( j = 1; j <= LHK1; ++j ) {
							JHJ1 = IK( IMIN + j );
							IC = min( j, JHJ1 - JHJ );
							SDOT = 0.0;
							if ( IC > 0 ) {
								for ( i = 0; i <= IC - 1; ++i ) {
									SDOT += AL( JHJ1 - IC + i ) * AU( JHK + j - IC + i );
								}
								AU( JHK + j ) -= SDOT;
								SDOT = 0.0;
								for ( i = 0; i <= IC - 1; ++i ) {
									SDOT += AU( JHJ1 - IC + i ) * AL( JHK + j - IC + i );
								}
							}
							AL( JHK + j ) = ( AL( JHK + j ) - SDOT ) * AD( IMIN1 + j );
							JHJ = JHJ1;
						}
					}
				}
				if ( NSYM == 0 ) {
					for ( i = 0; i <= LHK1; ++i ) {
						T1 = AU( JHK + i );
						T2 = T1 * AD( IMIN1 + i );
						AU( JHK + i ) = T2;
						SUMD += T1 * T2;
					}
				} else {
					for ( i = 0; i <= LHK1; ++i ) {
						SUMD += AU( JHK + i ) * AL( JHK + i );
					}
				}
			}
			if ( AD( k ) - SUMD == 0.0 ) {
				ShowSevereError( "AirflowNetworkSolver: L-U factorization in Subroutine FACSKY." );
				ShowContinueError( "The denominator used in L-U factorizationis equal to 0.0 at node = " + AirflowNetworkNodeData( k ).Name + '.' );
				ShowContinueError( "One possible cause is that this node may not be connected directly, or indirectly via airflow network connections " );
				ShowContinueError( "(e.g., AirflowNetwork:Multizone:SurfaceCrack, AirflowNetwork:Multizone:Component:SimpleOpening, etc.), to an external" );
				ShowContinueError( "node (AirflowNetwork:MultiZone:Surface)." );
				ShowContinueError( "Please send your input file and weather file to EnergyPlus support/development team for further investigation." );
				ShowFatalError( "Preceding condition causes termination." );
			}
			AD( k ) = 1.0 / ( AD( k ) - SUMD );
			JHK = JHK1;
		}
	}

	void
	SLVSKY(
		Array1A< Real64 > const AU, // the upper triangle of [A] before and after factoring
		Array1A< Real64 > const AD, // the main diagonal of [A] before and after factoring
		Array1A< Real64 > const AL, // the lower triangle of [A] before and after factoring
		Array1A< Real64 > B, // "B" vector (input); "X" vector (output).
		Array1A_int const IK, // pointer to the top of column/row "K"
		int const NEQ, // number of equations
		int const NSYM // symmetry:  0 = symmetric matrix, 1 = non-symmetric
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         George Walton
		//       DATE WRITTEN   Extracted from AIRNET
		//       MODIFIED       Lixing Gu, 2/1/04
		//                      Revised the subroutine to meet E+ needs
		//       MODIFIED       Lixing Gu, 6/8/05
		//       RE-ENGINEERED  This subroutine is revised from CLVSKY developed by George Walton, NIST

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine solves simultaneous linear algebraic equations [A] * X = B
		// using L-U factored skyline form of [A] from "FACSKY"

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		AU.dim( IK(NetworkNumOfNodes+1) );
		AD.dim( NetworkNumOfNodes );
		AL.dim( IK(NetworkNumOfNodes+1)-1 );
		B.dim( NetworkNumOfNodes );
		IK.dim( NetworkNumOfNodes+1 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// noel, GNU says the AU is indexed above its upper bound
		//REAL(r64), INTENT(INOUT) :: AU(IK(NetworkNumOfNodes+1)-1) ! the upper triangle of [A] before and after factoring

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int i;
		int JHK;
		int JHK1;
		int k;
		int LHK;
		Real64 SDOT;
		Real64 T1;

		// FLOW:
		JHK = 1;
		for ( k = 2; k <= NEQ; ++k ) {
			JHK1 = IK( k + 1 );
			LHK = JHK1 - JHK;
			if ( LHK <= 0 ) continue;
			SDOT = 0.0;
			if ( NSYM == 0 ) {
				for ( i = 0; i <= LHK - 1; ++i ) {
					SDOT += AU( JHK + i ) * B( k - LHK + i );
				}
			} else {
				for ( i = 0; i <= LHK - 1; ++i ) {
					SDOT += AL( JHK + i ) * B( k - LHK + i );
				}
			}
			B( k ) -= SDOT;
			JHK = JHK1;
		}
		if ( NSYM == 0 ) {
			for ( k = 1; k <= NEQ; ++k ) {
				B( k ) *= AD( k );
			}
		}
		k = NEQ + 1;
		JHK1 = IK( k );
		while ( k != 1 ) {
			--k;
			if ( NSYM == 1 ) B( k ) *= AD( k );
			if ( k == 1 ) break;
			//        IF(K.EQ.1) RETURN
			JHK = IK( k );
			T1 = B( k );
			for ( i = 0; i <= JHK1 - JHK - 1; ++i ) {
				B( k - JHK1 + JHK + i ) -= AU( JHK + i ) * T1;
			}
			JHK1 = JHK;
		}

	}

	void
	FILSKY(
		Array1A< Real64 > const X, // element array (row-wise sequence)
		Array1A_int const LM, // location matrix
		Array1A_int const IK, // pointer to the top of column/row "K"
		Array1A< Real64 > AU, // the upper triangle of [A] before and after factoring
		Array1A< Real64 > AD, // the main diagonal of [A] before and after factoring
		int const FLAG // mode of operation
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         George Walton
		//       DATE WRITTEN   Extracted from AIRNET
		//       MODIFIED       Lixing Gu, 2/1/04
		//                      Revised the subroutine to meet E+ needs
		//       MODIFIED       Lixing Gu, 6/8/05
		//       RE-ENGINEERED  This subroutine is revised from FILSKY developed by George Walton, NIST

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine adds element array "X" to the sparse skyline matrix [A]

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		X.dim( 4 );
		LM.dim( 2 );
		IK.dim( NetworkNumOfNodes+1 );
		AU.dim( IK(NetworkNumOfNodes+1) );
		AD.dim( NetworkNumOfNodes );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// noel, GNU says the AU is indexed above its upper bound
		//REAL(r64), INTENT(INOUT) :: AU(IK(NetworkNumOfNodes+1)-1) ! the upper triangle of [A] before and after factoring

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int j;
		int k;
		int L;

		// FLOW:
		// K = row number, L = column number.
		if ( FLAG > 1 ) {
			k = LM( 1 );
			L = LM( 2 );
			if ( FLAG == 4 ) {
				AD( k ) += X( 1 );
				if ( k < L ) {
					j = IK( L + 1 ) - L + k;
					AU( j ) += X( 2 );
				} else {
					j = IK( k + 1 ) - k + L;
					AU( j ) += X( 3 );
				}
				AD( L ) += X( 4 );
			} else if ( FLAG == 3 ) {
				AD( L ) += X( 4 );
			} else if ( FLAG == 2 ) {
				AD( k ) += X( 1 );
			}
		}
	}

	void
	DUMPVD(
		std::string const & S, // Description
		Array1A< Real64 > const V, // Output values
		int const n, // Array size
		int const UOUT // Output file unit
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         George Walton
		//       DATE WRITTEN   Extracted from AIRNET
		//       MODIFIED       Lixing Gu, 2/1/04
		//                      Revised the subroutine to meet E+ needs
		//       MODIFIED       Lixing Gu, 6/8/05
		//       RE-ENGINEERED  This subroutine is revised from DUMPVD developed by George Walton, NIST

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine prints the contents of the REAL(r64) "V" vector

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		V.dim( _ );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int i;

		// Formats
		static gio::Fmt Format_901( "(1X,A,$)" );
		static gio::Fmt Format_902( "(1X,5E15.07,$)" );

		// FLOW:
		// Write values for debug
		gio::write( UOUT, Format_901 ) << S;
		for ( i = 1; i <= n; ++i ) {
			gio::write( UOUT, Format_902 ) << V( i );
		}
		gio::write( UOUT );
	}

	void
	DUMPVR(
		std::string const & S, // Description
		Array1A< Real64 > const V, // Output values
		int const n, // Array size
		int const UOUT // Output file unit
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         George Walton
		//       DATE WRITTEN   Extracted from AIRNET
		//       MODIFIED       Lixing Gu, 2/1/04
		//                      Revised the subroutine to meet E+ needs
		//       MODIFIED       Lixing Gu, 6/8/05
		//       RE-ENGINEERED  This subroutine is revised from DUMPVR developed by George Walton, NIST

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine prints the contents of the REAL(r64) "V" vector

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		V.dim( _ );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int i;

		// Formats
		static gio::Fmt Format_901( "(1X,A,$)" );
		static gio::Fmt Format_902( "(1X,5E15.07,$)" );

		// FLOW:
		gio::write( UOUT, Format_901 ) << S;
		for ( i = 1; i <= n; ++i ) {
			gio::write( UOUT, Format_902 ) << V( i );
		}
		gio::write( UOUT );
	}

	void
	AFEDOP(
		int const j, // Component number
		int const EP_UNUSED( LFLAG ), // Initialization flag.If = 1, use laminar relationship
		Real64 const PDROP, // Total pressure drop across a component (P1 - P2) [Pa]
		int const IL, // Linkage number
		int const EP_UNUSED( n ), // Node 1 number
		int const EP_UNUSED( M ), // Node 2 number
		Array1A< Real64 > F, // Airflow through the component [kg/s]
		Array1A< Real64 > DF, // Partial derivative:  DF/DP
		int & NF // Number of flows, either 1 or 2
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Lixing Gu
		//       DATE WRITTEN   Oct. 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  This subroutine is revised based on a vertical large opening subroutine from COMIS

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simulates airflow and pressure of a detailed large opening component.

		// METHODOLOGY EMPLOYED:
		// Purpose:  This routine calculates the massflow and its derivative
		//       through a large opening in both flow directions. As input
		//       the density profiles RhoProfF/T are required aswell as the
		//       effective pressure difference profile DpProfNew, which is the
		//       sum of the stack pressure difference profile DpProf and the
		//       difference of the actual pressures at reference height. The
		//       profiles are calculated in the routine PresProfile.
		//       The massflow and its derivative are calculated for each
		//       interval representing a step of the pressure difference
		//       profile. The total flow and derivative are obtained by
		//       summation over the whole opening.
		//       The calculation is split into different cases representing
		//       different situations of the opening:
		//       - closed opening (opening factor = 0): summation of top and
		//         bottom crack (crack length = lwmax) plus "integration" over
		//         a vertically distributed crack of length (2*lhmax+lextra).
		//       - type 1: normal rectangular opening: "integration" over NrInt
		//         openings with width actlw and height actlh/NrInt
		//       - type 2: horizontally pivoted window: flow direction assumed
		//         strictly perpendicular to the plane of the opening
		//         -> "integration" over normal rectangular openings at top
		//         and bottom of LO plus a rectangular opening in series with two
		//         triangular openings in the middle of the LO (most general
		//         situation). The geometry is defined by the input parameters
		//         actlw(=lwmax), actlh, axisheight, opening angle.
		//       Assuming the massflow perpendicular to the opening plane in all
		//       cases the ownheightfactor has no influence on the massflow.

		// REFERENCES:
		// Helmut E. Feustel and Alison Rayner-Hooson, "COMIS Fundamentals," LBL-28560,
		// Lawrence Berkeley National Laboratory, Berkeley, CA, May 1990

		// USE STATEMENTS:
		using DataGlobals::PiOvr2;

		// Argument array dimensioning
		F.dim( 2 );
		DF.dim( 2 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const RealMin( 1e-37 );
		static Real64 const sqrt_1_2( std::sqrt( 1.2 ) );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		static Real64 const sqrt_2( std::sqrt( 2.0 ) );

		int CompNum;
		Real64 Width;
		Real64 Height;

		Real64 fma12; // massflow in direction "from-to" [kg/s]
		Real64 fma21; // massflow in direction "to-from" [kg/s]
		Real64 dp1fma12; // derivative d fma12 / d Dp [kg/s/Pa]
		Real64 dp1fma21; // derivative d fma21 / d Dp [kg/s/Pa]
		Array1D< Real64 > DpProfNew( NrInt+2 ); // Differential pressure profile for Large Openings, taking into account fixed
		// pressures and actual zone pressures at reference height
		Real64 Fact; // Actual opening factor
		Real64 DifLim; // Limit for the pressure difference where laminarization takes place [Pa]
		Real64 Cfact;
		Real64 FvVeloc;

		Real64 ActLh;
		Real64 ActLw;
		Real64 Lextra;
		Real64 Axishght;
		Real64 ActCD;
		Real64 Cs;
		Real64 expn;
		Real64 Type;
		Real64 Interval;
		Real64 fmasum;
		Real64 dfmasum;
		Real64 Prefact;
		Array1D< Real64 > EvalHghts( NrInt+2 );
		Real64 h2;
		Real64 h4;
		Real64 alpha;
		Real64 rholink;
		Real64 c1;
		Real64 c2;
		Real64 DpZeroOffset;
		Real64 area;
		Real64 WFact;
		Real64 HFact;
		int i;
		int Loc;
		int iNum;

		// FLOW:
		// Get component properties
		DifLim = 1.0e-4;
		CompNum = AirflowNetworkCompData( j ).TypeNum;
		Width = MultizoneSurfaceData( IL ).Width;
		Height = MultizoneSurfaceData( IL ).Height;
		Fact = MultizoneSurfaceData( IL ).OpenFactor;
		Loc = ( AirflowNetworkLinkageData( IL ).DetOpenNum - 1 ) * ( NrInt + 2 );
		iNum = MultizoneCompDetOpeningData( CompNum ).NumFac;
		ActCD = 0.0;

		if ( iNum == 2 ) {
			if ( Fact <= MultizoneCompDetOpeningData( CompNum ).OpenFac2 ) {
				WFact = MultizoneCompDetOpeningData( CompNum ).WidthFac1 + ( Fact - MultizoneCompDetOpeningData( CompNum ).OpenFac1 ) / ( MultizoneCompDetOpeningData( CompNum ).OpenFac2 - MultizoneCompDetOpeningData( CompNum ).OpenFac1 ) * ( MultizoneCompDetOpeningData( CompNum ).WidthFac2 - MultizoneCompDetOpeningData( CompNum ).WidthFac1 );
				HFact = MultizoneCompDetOpeningData( CompNum ).HeightFac1 + ( Fact - MultizoneCompDetOpeningData( CompNum ).OpenFac1 ) / ( MultizoneCompDetOpeningData( CompNum ).OpenFac2 - MultizoneCompDetOpeningData( CompNum ).OpenFac1 ) * ( MultizoneCompDetOpeningData( CompNum ).HeightFac2 - MultizoneCompDetOpeningData( CompNum ).HeightFac1 );
				Cfact = MultizoneCompDetOpeningData( CompNum ).DischCoeff1 + ( Fact - MultizoneCompDetOpeningData( CompNum ).OpenFac1 ) / ( MultizoneCompDetOpeningData( CompNum ).OpenFac2 - MultizoneCompDetOpeningData( CompNum ).OpenFac1 ) * ( MultizoneCompDetOpeningData( CompNum ).DischCoeff2 - MultizoneCompDetOpeningData( CompNum ).DischCoeff1 );
			} else {
				ShowFatalError( "Open Factor is above the maximum input range for opening factors in AirflowNetwork:MultiZone:Component:DetailedOpening = " + MultizoneCompDetOpeningData( CompNum ).Name );
			}
		}

		if ( iNum == 3 ) {
			if ( Fact <= MultizoneCompDetOpeningData( CompNum ).OpenFac2 ) {
				WFact = MultizoneCompDetOpeningData( CompNum ).WidthFac1 + ( Fact - MultizoneCompDetOpeningData( CompNum ).OpenFac1 ) / ( MultizoneCompDetOpeningData( CompNum ).OpenFac2 - MultizoneCompDetOpeningData( CompNum ).OpenFac1 ) * ( MultizoneCompDetOpeningData( CompNum ).WidthFac2 - MultizoneCompDetOpeningData( CompNum ).WidthFac1 );
				HFact = MultizoneCompDetOpeningData( CompNum ).HeightFac1 + ( Fact - MultizoneCompDetOpeningData( CompNum ).OpenFac1 ) / ( MultizoneCompDetOpeningData( CompNum ).OpenFac2 - MultizoneCompDetOpeningData( CompNum ).OpenFac1 ) * ( MultizoneCompDetOpeningData( CompNum ).HeightFac2 - MultizoneCompDetOpeningData( CompNum ).HeightFac1 );
				Cfact = MultizoneCompDetOpeningData( CompNum ).DischCoeff1 + ( Fact - MultizoneCompDetOpeningData( CompNum ).OpenFac1 ) / ( MultizoneCompDetOpeningData( CompNum ).OpenFac2 - MultizoneCompDetOpeningData( CompNum ).OpenFac1 ) * ( MultizoneCompDetOpeningData( CompNum ).DischCoeff2 - MultizoneCompDetOpeningData( CompNum ).DischCoeff1 );
			} else if ( Fact <= MultizoneCompDetOpeningData( CompNum ).OpenFac3 ) {
				WFact = MultizoneCompDetOpeningData( CompNum ).WidthFac2 + ( Fact - MultizoneCompDetOpeningData( CompNum ).OpenFac2 ) / ( MultizoneCompDetOpeningData( CompNum ).OpenFac3 - MultizoneCompDetOpeningData( CompNum ).OpenFac2 ) * ( MultizoneCompDetOpeningData( CompNum ).WidthFac3 - MultizoneCompDetOpeningData( CompNum ).WidthFac2 );
				HFact = MultizoneCompDetOpeningData( CompNum ).HeightFac2 + ( Fact - MultizoneCompDetOpeningData( CompNum ).OpenFac2 ) / ( MultizoneCompDetOpeningData( CompNum ).OpenFac3 - MultizoneCompDetOpeningData( CompNum ).OpenFac2 ) * ( MultizoneCompDetOpeningData( CompNum ).HeightFac3 - MultizoneCompDetOpeningData( CompNum ).HeightFac2 );
				Cfact = MultizoneCompDetOpeningData( CompNum ).DischCoeff2 + ( Fact - MultizoneCompDetOpeningData( CompNum ).OpenFac2 ) / ( MultizoneCompDetOpeningData( CompNum ).OpenFac3 - MultizoneCompDetOpeningData( CompNum ).OpenFac2 ) * ( MultizoneCompDetOpeningData( CompNum ).DischCoeff3 - MultizoneCompDetOpeningData( CompNum ).DischCoeff2 );
			} else {
				ShowFatalError( "Open Factor is above the maximum input range for opening factors in AirflowNetwork:MultiZone:Component:DetailedOpening = " + MultizoneCompDetOpeningData( CompNum ).Name );
			}
		}

		if ( iNum == 4 ) {
			if ( Fact <= MultizoneCompDetOpeningData( CompNum ).OpenFac2 ) {
				WFact = MultizoneCompDetOpeningData( CompNum ).WidthFac1 + ( Fact - MultizoneCompDetOpeningData( CompNum ).OpenFac1 ) / ( MultizoneCompDetOpeningData( CompNum ).OpenFac2 - MultizoneCompDetOpeningData( CompNum ).OpenFac1 ) * ( MultizoneCompDetOpeningData( CompNum ).WidthFac2 - MultizoneCompDetOpeningData( CompNum ).WidthFac1 );
				HFact = MultizoneCompDetOpeningData( CompNum ).HeightFac1 + ( Fact - MultizoneCompDetOpeningData( CompNum ).OpenFac1 ) / ( MultizoneCompDetOpeningData( CompNum ).OpenFac2 - MultizoneCompDetOpeningData( CompNum ).OpenFac1 ) * ( MultizoneCompDetOpeningData( CompNum ).HeightFac2 - MultizoneCompDetOpeningData( CompNum ).HeightFac1 );
				Cfact = MultizoneCompDetOpeningData( CompNum ).DischCoeff1 + ( Fact - MultizoneCompDetOpeningData( CompNum ).OpenFac1 ) / ( MultizoneCompDetOpeningData( CompNum ).OpenFac2 - MultizoneCompDetOpeningData( CompNum ).OpenFac1 ) * ( MultizoneCompDetOpeningData( CompNum ).DischCoeff2 - MultizoneCompDetOpeningData( CompNum ).DischCoeff1 );
			} else if ( Fact <= MultizoneCompDetOpeningData( CompNum ).OpenFac3 ) {
				WFact = MultizoneCompDetOpeningData( CompNum ).WidthFac2 + ( Fact - MultizoneCompDetOpeningData( CompNum ).OpenFac2 ) / ( MultizoneCompDetOpeningData( CompNum ).OpenFac3 - MultizoneCompDetOpeningData( CompNum ).OpenFac2 ) * ( MultizoneCompDetOpeningData( CompNum ).WidthFac3 - MultizoneCompDetOpeningData( CompNum ).WidthFac2 );
				HFact = MultizoneCompDetOpeningData( CompNum ).HeightFac2 + ( Fact - MultizoneCompDetOpeningData( CompNum ).OpenFac2 ) / ( MultizoneCompDetOpeningData( CompNum ).OpenFac3 - MultizoneCompDetOpeningData( CompNum ).OpenFac2 ) * ( MultizoneCompDetOpeningData( CompNum ).HeightFac3 - MultizoneCompDetOpeningData( CompNum ).HeightFac2 );
				Cfact = MultizoneCompDetOpeningData( CompNum ).DischCoeff2 + ( Fact - MultizoneCompDetOpeningData( CompNum ).OpenFac2 ) / ( MultizoneCompDetOpeningData( CompNum ).OpenFac3 - MultizoneCompDetOpeningData( CompNum ).OpenFac2 ) * ( MultizoneCompDetOpeningData( CompNum ).DischCoeff3 - MultizoneCompDetOpeningData( CompNum ).DischCoeff2 );
			} else if ( Fact <= MultizoneCompDetOpeningData( CompNum ).OpenFac4 ) {
				WFact = MultizoneCompDetOpeningData( CompNum ).WidthFac3 + ( Fact - MultizoneCompDetOpeningData( CompNum ).OpenFac3 ) / ( MultizoneCompDetOpeningData( CompNum ).OpenFac4 - MultizoneCompDetOpeningData( CompNum ).OpenFac3 ) * ( MultizoneCompDetOpeningData( CompNum ).WidthFac4 - MultizoneCompDetOpeningData( CompNum ).WidthFac3 );
				HFact = MultizoneCompDetOpeningData( CompNum ).HeightFac3 + ( Fact - MultizoneCompDetOpeningData( CompNum ).OpenFac3 ) / ( MultizoneCompDetOpeningData( CompNum ).OpenFac4 - MultizoneCompDetOpeningData( CompNum ).OpenFac3 ) * ( MultizoneCompDetOpeningData( CompNum ).HeightFac4 - MultizoneCompDetOpeningData( CompNum ).HeightFac3 );
				Cfact = MultizoneCompDetOpeningData( CompNum ).DischCoeff3 + ( Fact - MultizoneCompDetOpeningData( CompNum ).OpenFac3 ) / ( MultizoneCompDetOpeningData( CompNum ).OpenFac4 - MultizoneCompDetOpeningData( CompNum ).OpenFac3 ) * ( MultizoneCompDetOpeningData( CompNum ).DischCoeff4 - MultizoneCompDetOpeningData( CompNum ).DischCoeff3 );
			} else {
				ShowFatalError( "Open Factor is above the maximum input range for opening factors in AirflowNetwork:MultiZone:Component:DetailedOpening = " + MultizoneCompDetOpeningData( CompNum ).Name );
			}
		}

		// calculate DpProfNew
		for ( i = 1; i <= NrInt + 2; ++i ) {
			DpProfNew( i ) = PDROP + DpProf( Loc + i ) - DpL( IL, 1 );
		}

		// Get opening data based on the opening factor
		if ( Fact == 0 ) {
			ActLw = MultizoneSurfaceData( IL ).Width;
			ActLh = MultizoneSurfaceData( IL ).Height;
			Cfact = 0.0;
		} else {
			ActLw = MultizoneSurfaceData( IL ).Width * WFact;
			ActLh = MultizoneSurfaceData( IL ).Height * HFact;
			ActCD = Cfact;
		}

		Cs = MultizoneCompDetOpeningData( CompNum ).FlowCoef;
		expn = MultizoneCompDetOpeningData( CompNum ).FlowExpo;
		Type = MultizoneCompDetOpeningData( CompNum ).LVOType;
		if ( Type == 1 ) {
			Lextra = MultizoneCompDetOpeningData( CompNum ).LVOValue;
			Axishght = 0.0;
		} else if ( Type == 2 ) {
			Lextra = 0.0;
			Axishght = MultizoneCompDetOpeningData( CompNum ).LVOValue;
			ActLw = MultizoneSurfaceData( IL ).Width;
			ActLh = MultizoneSurfaceData( IL ).Height;
		}

		// Add window multiplier with window close
		if ( MultizoneSurfaceData( IL ).Multiplier > 1.0 ) Cs *= MultizoneSurfaceData( IL ).Multiplier;
		// Add window multiplier with window open
		if ( Fact > 0.0 ) {
			if ( MultizoneSurfaceData( IL ).Multiplier > 1.0 ) ActLw *= MultizoneSurfaceData( IL ).Multiplier;
		}

		// Add recurring warnings
		if ( Fact > 0.0 ) {
			if ( ActLw == 0.0 ) {
				++MultizoneCompDetOpeningData( CompNum ).WidthErrCount;
				if ( MultizoneCompDetOpeningData( CompNum ).WidthErrCount < 2 ) {
					ShowWarningError( "The actual width of the AirflowNetwork:MultiZone:Component:DetailedOpening of " + MultizoneCompDetOpeningData( CompNum ).Name + " is 0." );
					ShowContinueError( "The actual width is set to 1.0E-6 m." );
					ShowContinueErrorTimeStamp( "Occurrence info:" );
				} else {
					ShowRecurringWarningErrorAtEnd( "The actual width of the AirflowNetwork:MultiZone:Component:DetailedOpening of " + MultizoneCompDetOpeningData( CompNum ).Name + " is 0 error continues.", MultizoneCompDetOpeningData( CompNum ).WidthErrIndex, ActLw, ActLw );
				}
				ActLw = 1.0e-6;
			}
			if ( ActLh == 0.0 ) {
				++MultizoneCompDetOpeningData( CompNum ).HeightErrCount;
				if ( MultizoneCompDetOpeningData( CompNum ).HeightErrCount < 2 ) {
					ShowWarningError( "The actual height of the AirflowNetwork:MultiZone:Component:DetailedOpening of " + MultizoneCompDetOpeningData( CompNum ).Name + " is 0." );
					ShowContinueError( "The actual height is set to 1.0E-6 m." );
					ShowContinueErrorTimeStamp( "Occurrence info:" );
				} else {
					ShowRecurringWarningErrorAtEnd( "The actual width of the AirflowNetwork:MultiZone:Component:DetailedOpening of " + MultizoneCompDetOpeningData( CompNum ).Name + " is 0 error continues.", MultizoneCompDetOpeningData( CompNum ).HeightErrIndex, ActLh, ActLh );
				}
				ActLh = 1.0e-6;
			}
		}
		// Initialization:
		NF = 1;
		Interval = ActLh / NrInt;
		fma12 = 0.0;
		fma21 = 0.0;
		dp1fma12 = 0.0;
		dp1fma21 = 0.0;

		// Closed LO
		if ( Cfact == 0 ) {
			DpZeroOffset = DifLim;
			// bottom crack
			if ( DpProfNew( 1 ) > 0 ) {
				if ( std::abs( DpProfNew( 1 ) ) <= DpZeroOffset ) {
					dfmasum = Cs * ActLw * std::pow( DpZeroOffset, expn ) / DpZeroOffset;
					fmasum = DpProfNew( 1 ) * dfmasum;
				} else {
					fmasum = Cs * ActLw * std::pow( DpProfNew( 1 ), expn );
					dfmasum = fmasum * expn / DpProfNew( 1 );
				}
				fma12 += fmasum;
				dp1fma12 += dfmasum;
			} else {
				if ( std::abs( DpProfNew( 1 ) ) <= DpZeroOffset ) {
					dfmasum = -Cs * ActLw * std::pow( DpZeroOffset, expn ) / DpZeroOffset;
					fmasum = DpProfNew( 1 ) * dfmasum;
				} else {
					fmasum = Cs * ActLw * std::pow( -DpProfNew( 1 ), expn );
					dfmasum = fmasum * expn / DpProfNew( 1 );
				}
				fma21 += fmasum;
				dp1fma21 += dfmasum;
			}
			// top crack
			if ( DpProfNew( NrInt + 2 ) > 0 ) {
				if ( std::abs( DpProfNew( NrInt + 2 ) ) <= DpZeroOffset ) {
					dfmasum = Cs * ActLw * std::pow( DpZeroOffset, expn ) / DpZeroOffset;
					fmasum = DpProfNew( NrInt + 2 ) * dfmasum;
				} else {
					fmasum = Cs * ActLw * std::pow( DpProfNew( NrInt + 2 ), expn );
					dfmasum = fmasum * expn / DpProfNew( NrInt + 2 );
				}
				fma12 += fmasum;
				dp1fma12 += dfmasum;
			} else {
				if ( std::abs( DpProfNew( NrInt + 2 ) ) <= DpZeroOffset ) {
					dfmasum = -Cs * ActLw * std::pow( DpZeroOffset, expn ) / DpZeroOffset;
					fmasum = DpProfNew( NrInt + 2 ) * dfmasum;
				} else {
					fmasum = Cs * ActLw * std::pow( -DpProfNew( NrInt + 2 ), expn );
					dfmasum = fmasum * expn / DpProfNew( NrInt + 2 );
				}
				fma21 += fmasum;
				dp1fma21 += dfmasum;
			}
			// side and extra cracks
			Prefact = Interval * ( 2 + Lextra / ActLh ) * Cs;
			for ( i = 2; i <= NrInt + 1; ++i ) {
				if ( DpProfNew( i ) > 0 ) {
					if ( std::abs( DpProfNew( i ) ) <= DpZeroOffset ) {
						dfmasum = Prefact * std::pow( DpZeroOffset, expn ) / DpZeroOffset;
						fmasum = DpProfNew( i ) * dfmasum;
					} else {
						fmasum = Prefact * std::pow( DpProfNew( i ), expn );
						dfmasum = fmasum * expn / DpProfNew( i );
					}
					fma12 += fmasum;
					dp1fma12 += dfmasum;
				} else {
					if ( std::abs( DpProfNew( i ) ) <= DpZeroOffset ) {
						dfmasum = -Prefact * std::pow( DpZeroOffset, expn ) / DpZeroOffset;
						fmasum = DpProfNew( i ) * dfmasum;
					} else {
						fmasum = Prefact * std::pow( -DpProfNew( i ), expn );
						dfmasum = fmasum * expn / DpProfNew( i );
					}
					fma21 += fmasum;
					dp1fma21 += dfmasum;
				}
			}
		}

		// Open LO, type 1
		if ( ( Cfact != 0 ) && ( Type == 1 ) ) {
			DpZeroOffset = DifLim * 1e-3;
			Prefact = ActLw * ActCD * Interval * sqrt_2;
			for ( i = 2; i <= NrInt + 1; ++i ) {
				if ( DpProfNew( i ) > 0 ) {
					if ( std::abs( DpProfNew( i ) ) <= DpZeroOffset ) {
						dfmasum = std::sqrt( RhoProfF( Loc + i ) * DpZeroOffset ) / DpZeroOffset;
						fmasum = DpProfNew( i ) * dfmasum;
					} else {
						fmasum = std::sqrt( RhoProfF( Loc + i ) * DpProfNew( i ) );
						dfmasum = 0.5 * fmasum / DpProfNew( i );
					}
					fma12 += fmasum;
					dp1fma12 += dfmasum;
				} else {
					if ( std::abs( DpProfNew( i ) ) <= DpZeroOffset ) {
						dfmasum = -std::sqrt( RhoProfT( Loc + i ) * DpZeroOffset ) / DpZeroOffset;
						fmasum = DpProfNew( i ) * dfmasum;
					} else {
						fmasum = std::sqrt( -RhoProfT( Loc + i ) * DpProfNew( i ) );
						dfmasum = 0.5 * fmasum / DpProfNew( i );
					}
					fma21 += fmasum;
					dp1fma21 += dfmasum;
				}
			}

			fma12 *= Prefact;
			fma21 *= Prefact;
			dp1fma12 *= Prefact;
			dp1fma21 *= Prefact;

		}

		// Open LO, type 2
		if ( ( Cfact != 0 ) && ( Type == 2 ) ) {
			// Initialization
			DpZeroOffset = DifLim * 1e-3;
			// New definition for opening factors for LVO type 2: opening angle = 90 degrees --> opening factor = 1.0
			// should be PIOvr2 in below?
			alpha = Fact * PiOvr2;
			Real64 const cos_alpha( std::cos( alpha ) );
			Real64 const tan_alpha( std::tan( alpha ) );
			h2 = Axishght * ( 1.0 - cos_alpha );
			h4 = Axishght + ( ActLh - Axishght ) * cos_alpha;
			EvalHghts( 1 ) = 0.0;
			EvalHghts( NrInt + 2 ) = ActLh;
			// New definition for opening factors for LVO type 2: pening angle = 90 degrees --> opening factor = 1.0
			if ( Fact == 1.0 ) {
				h2 = Axishght;
				h4 = Axishght;
			}

			for ( i = 2; i <= NrInt + 1; ++i ) {
				EvalHghts( i ) = Interval * ( i - 1.5 );
			}

			// Calculation of massflow and its derivative
			for ( i = 2; i <= NrInt + 1; ++i ) {
				if ( DpProfNew( i ) > 0 ) {
					rholink = RhoProfF( Loc + i );
				} else {
					rholink = RhoProfT( Loc + i );
				}

				if ( ( EvalHghts( i ) <= h2 ) || ( EvalHghts( i ) >= h4 ) ) {
					if ( std::abs( DpProfNew( i ) ) <= DpZeroOffset ) {
						dfmasum = ActCD * ActLw * Interval * std::sqrt( 2.0 * rholink * DpZeroOffset ) / DpZeroOffset * sign( 1, DpProfNew( i ) );
						fmasum = DpProfNew( i ) * dfmasum;
					} else {
						fmasum = ActCD * ActLw * Interval * std::sqrt( 2.0 * rholink * std::abs( DpProfNew( i ) ) );
						dfmasum = 0.5 * fmasum / DpProfNew( i );
					}
				} else {
					// triangular opening at the side of LO
					c1 = ActCD * ActLw * Interval * std::sqrt( 2.0 * rholink );
					c2 = 2 * ActCD * std::abs( Axishght - EvalHghts( i ) ) * tan_alpha * Interval * std::sqrt( 2.0 * rholink );
					if ( ( c1 != 0 ) && ( c2 != 0 ) ) {
						if ( std::abs( DpProfNew( i ) ) <= DpZeroOffset ) {
							dfmasum = std::sqrt( DpZeroOffset / ( 1 / c1 / c1 + 1 / c2 / c2 ) ) / DpZeroOffset * sign( 1, DpProfNew( i ) );
							fmasum = DpProfNew( i ) * dfmasum;
						} else {
							fmasum = std::sqrt( std::abs( DpProfNew( i ) ) / ( 1 / c1 / c1 + 1 / c2 / c2 ) );
							dfmasum = 0.5 * fmasum / DpProfNew( i );
						}
					} else {
						fmasum = 0.0;
						dfmasum = 0.0;
					}
				}

				if ( DpProfNew( i ) > 0 ) {
					fma12 += fmasum;
					dp1fma12 += dfmasum;
				} else {
					fma21 += fmasum;
					dp1fma21 += dfmasum;
				}
			}

		}

		// Calculate some velocity in the large opening
		area = ActLh * ActLw * ActCD;
		if ( area > ( Cs + RealMin ) ) {
			if ( area > RealMin ) {
				FvVeloc = ( fma21 + fma12 ) / area;
			} else {
				FvVeloc = 0.0;
			}
		} else {
			// here the average velocity over the full area, may blow half in half out.
			// velocity= Fva/Nett area=Fma/Rho/(Cm/( (2**N)* SQRT(1.2) ) )
			if ( Cs > 0.0 ) {
				// get the average Rho for this closed window
				for ( i = 2; i <= NrInt + 1; ++i ) {
					rholink = 0.0;
					if ( DpProfNew( i ) > 0 ) {
						rholink = RhoProfF( Loc + i );
					} else {
						rholink = RhoProfT( Loc + i );
					}
					rholink /= NrInt;
					rholink = 1.2;
				}
				FvVeloc = ( fma21 + fma12 ) * std::pow( 2.0, expn ) * sqrt_1_2 / ( rholink * Cs );
			} else {
				FvVeloc = 0.0;
			}
		}

		// Output mass flow rates and associated derivatives
		F( 1 ) = fma12 - fma21;
		DF( 1 ) = dp1fma12 - dp1fma21;
		F( 2 ) = 0.0;
		if ( fma12 != 0.0 && fma21 != 0.0 ) {
			F( 2 ) = fma21;
		}
		DF( 2 ) = 0.0;

	}

	void
	PresProfile(
		int const il, // Linkage number
		int const Pprof, // Opening number
		Real64 const G, // gravitation field strength [N/kg]
		Array1A< Real64 > const DpF, // Stack pressures at start heights of Layers
		Array1A< Real64 > const DpT, // Stack pressures at start heights of Layers
		Array1A< Real64 > const BetaF, // Density gradients in the FROM zone (starting at linkheight) [Kg/m3/m]
		Array1A< Real64 > const BetaT, // Density gradients in the TO zone (starting at linkheight) [Kg/m3/m]
		Array1A< Real64 > const RhoStF, // Density at the start heights of Layers in the FROM zone
		Array1A< Real64 > const RhoStT, // Density at the start heights of Layers in the TO zone
		int const From, // Number of FROM zone
		int const To, // Number of To zone
		Real64 const ActLh, // Actual height of opening [m]
		Real64 const OwnHeightFactor // Cosine of deviation angle of the opening plane from the vertical direction
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Lixing Gu
		//       DATE WRITTEN   Oct. 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  This subroutine is revised based on PresProfile subroutine from COMIS

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates for a large opening profiles of stack pressure difference and
		// densities in the zones linked by the a detailed opening cmponent.

		// METHODOLOGY EMPLOYED:
		// The profiles are obtained in the following
		// way:    - the opening is divided into NrInt vertical intervals
		//         - the stack pressure difference and densities in From-
		//           and To-zone are calculated at the centre of each
		//           interval aswell as at the top and bottom of the LO
		//          - these values are stored in the (NrInt+2)-dimensional
		//             arrays DpProf, RhoProfF, RhoProfT.
		// The calculation of stack pressure and density in the two zones
		// is based on the arrays DpF/T, RhoStF/T, BetaF/T. These arrays
		// are calculated in the COMIS routine Lclimb. They contain the
		// values of stack pressure and density at the startheight of the
		// opening and at startheights of all layers lying inside the
		// opening, and the density gradients across the layers.
		// The effective startheight zl(1/2) in the From/To zone and the
		// effective length actLh of the LO take into account the
		// startheightfactor, heightfactor and ownheightfactor. Thus for
		// slanted windows the range of the profiles is the vertical
		// projection of the actual opening.

		// REFERENCES:
		// Helmut E. Feustel and Alison Rayner-Hooson, "COMIS Fundamentals," LBL-28560,
		// Lawrence Berkeley National Laboratory, Berkeley, CA, May 1990

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		DpF.dim( 2 );
		DpT.dim( 2 );
		BetaF.dim( 2 );
		BetaT.dim( 2 );
		RhoStF.dim( 2 );
		RhoStT.dim( 2 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// in the FROM zone (starting at linkheight) [Pa]
		// (starting at linkheight) [Kg/m3]
		// (starting at linkheight) [Kg/m3]

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Array1D< Real64 > zF( 2 ); // Startheights of layers in FROM-, TO-zone
		Array1D< Real64 > zT( 2 );
		Array1D< Real64 > zStF( 2 ); // Startheights of layers within the LO, starting with the actual startheight of the LO.
		Array1D< Real64 > zStT( 2 );
		// The values in the arrays DpF, DpT, BetaF, BetaT, RhoStF, RhoStT are calculated at these heights.
		Real64 hghtsFR;
		Real64 hghtsTR;
		Array1D< Real64 > hghtsF( NrInt+2 ); // Heights of evaluation points for pressure and density profiles
		Array1D< Real64 > hghtsT( NrInt+2 );
		Real64 Interval; // Distance between two evaluation points
		Real64 delzF; // Interval between actual evaluation point and startheight of actual layer in FROM-, TO-zone
		Real64 delzT;
		int AnzLayF; // Number of layers in FROM-, TO-zone
		int AnzLayT;
		int lF; // Actual index for DpF/T, BetaF/T, RhoStF/T, zStF/T
		int lT;
		int n;
		int i;
		int k;

		// FLOW:
		// Initialization
		delzF = 0.0;
		delzT = 0.0;
		Interval = ActLh * OwnHeightFactor / NrInt;

		for ( n = 1; n <= NrInt; ++n ) {
			hghtsF( n + 1 ) = AirflowNetworkLinkageData( il ).NodeHeights( 1 ) + Interval * ( n - 0.5 );
			hghtsT( n + 1 ) = AirflowNetworkLinkageData( il ).NodeHeights( 2 ) + Interval * ( n - 0.5 );
		}
		hghtsF( 1 ) = AirflowNetworkLinkageData( il ).NodeHeights( 1 );
		hghtsT( 1 ) = AirflowNetworkLinkageData( il ).NodeHeights( 2 );
		hghtsF( NrInt + 2 ) = AirflowNetworkLinkageData( il ).NodeHeights( 1 ) + ActLh * OwnHeightFactor;
		hghtsT( NrInt + 2 ) = AirflowNetworkLinkageData( il ).NodeHeights( 2 ) + ActLh * OwnHeightFactor;

		lF = 1;
		lT = 1;
		if ( From == 0 ) {
			AnzLayF = 1;
		} else {
			AnzLayF = 0;
		}
		if ( To == 0 ) {
			AnzLayT = 1;
		} else {
			AnzLayT = 0;
		}

		if ( AnzLayF > 0 ) {
			for ( n = 1; n <= AnzLayF; ++n ) {
				zF( n ) = 0.0;
				if ( hghtsF( 1 ) < 0.0 ) zF( n ) = hghtsF( 1 );
			}
		}

		if ( AnzLayT > 0 ) {
			for ( n = 1; n <= AnzLayT; ++n ) {
				zT( n ) = 0.0;
				if ( hghtsT( 1 ) < 0.0 ) zT( n ) = hghtsT( 1 );
			}
		}

		zStF( 1 ) = AirflowNetworkLinkageData( il ).NodeHeights( 1 );
		i = 2;
		k = 1;

		while ( k <= AnzLayF ) {
			if ( zF( k ) > zStF( 1 ) ) break;
			++k;
		}

		while ( k <= AnzLayF ) {
			if ( zF( k ) > hghtsF( NrInt ) ) break;
			zStF( i ) = zF( k ); //Autodesk:BoundsViolation zStF(i) @ i>2 and zF(k) @ k>2
			++i;
			++k;
		}

		zStF( i ) = AirflowNetworkLinkageData( il ).NodeHeights( 1 ) + ActLh * OwnHeightFactor; //Autodesk:BoundsViolation zStF(i) @ i>2
		zStT( 1 ) = AirflowNetworkLinkageData( il ).NodeHeights( 2 );
		i = 2;
		k = 1;

		while ( k <= AnzLayT ) {
			if ( zT( k ) > zStT( 1 ) ) break;
			++k;
		}

		while ( k <= AnzLayT ) {
			if ( zT( k ) > hghtsT( NrInt ) ) break; //Autodesk:BoundsViolation zT(k) @ k>2
			zStT( i ) = zT( k ); //Autodesk:BoundsViolation zStF(i) @ i>2 and zT(k) @ k>2
			++i;
			++k;
		}

		zStT( i ) = AirflowNetworkLinkageData( il ).NodeHeights( 2 ) + ActLh * OwnHeightFactor; //Autodesk:BoundsViolation zStT(i) @ i>2

		// Calculation of DpProf, RhoProfF, RhoProfT
		for ( i = 1; i <= NrInt + 2; ++i ) {
			hghtsFR = hghtsF( i );
			hghtsTR = hghtsT( i );

			while ( true ) {
				if ( hghtsFR > zStF( lF + 1 ) ) {
					if ( lF > 2 ) break;
					++lF;
				}
				if ( hghtsFR <= zStF( lF + 1 ) ) break;
			}

			while ( true ) {
				if ( hghtsTR > zStT( lT + 1 ) ) {
					++lT;
				}
				if ( hghtsTR <= zStT( lT + 1 ) ) break;
			}

			delzF = hghtsF( i ) - zStF( lF );
			delzT = hghtsT( i ) - zStT( lT );

			RhoProfF( i + Pprof ) = RhoStF( lF ) + BetaF( lF ) * delzF;
			RhoProfT( i + Pprof ) = RhoStT( lT ) + BetaT( lT ) * delzT;

			DpProf( i + Pprof ) = DpF( lF ) - DpT( lT ) - G * ( RhoStF( lF ) * delzF + BetaF( lF ) * pow_2( delzF ) / 2.0 ) + G * ( RhoStT( lT ) * delzT + BetaT( lT ) * pow_2( delzT ) / 2.0 );
		}

	}

	void
	PStack()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Lixing Gu
		//       DATE WRITTEN   Oct. 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  This subroutine is revised based on PresProfile subroutine from COMIS

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates the stack pressures for a link between two zones

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// Helmut E. Feustel and Alison Rayner-Hooson, "COMIS Fundamentals," LBL-28560,
		// Lawrence Berkeley National Laboratory, Berkeley, CA, May 1990

		// USE STATEMENTS:
		using DataGlobals::Pi;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const PSea( 101325.0 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//      REAL(r64) RhoOut ! air density outside [kg/m3]
		Real64 G; // gravity field strength [N/kg]
		Real64 RhoL1; // Air density [kg/m3]
		Real64 RhoL2;
		Real64 Pbz; // Pbarom at entrance level [Pa]
		Array2D< Real64 > RhoDrL( NumOfLinksMultiZone, 2 ); // dry air density on both sides of the link [kg/m3]
		Real64 TempL1; // Temp in From and To zone at link level [C]
		Real64 TempL2;
		//      REAL(r64) Tout ! outside temperature [C]
		Real64 Xhl1; // Humidity in From and To zone at link level [kg/kg]
		Real64 Xhl2;
		//      REAL(r64) Xhout ! outside humidity [kg/kg]
		Array1D< Real64 > Hfl( NumOfLinksMultiZone ); // Own height factor for large (slanted) openings
		int Nl; // number of links

		Array1D< Real64 > DpF( 2 );
		Real64 DpP;
		Array1D< Real64 > DpT( 2 );
		Real64 H;
		Array1D< Real64 > RhoStF( 2 );
		Array1D< Real64 > RhoStT( 2 );
		Real64 RhoDrDummi;
		Array1D< Real64 > BetaStF( 2 );
		Array1D< Real64 > BetaStT( 2 );
		Real64 T;
		Real64 X;
		Array1D< Real64 > HSt( 2 );
		Real64 TzFrom;
		Real64 XhzFrom;
		Real64 TzTo;
		Real64 XhzTo;
		Real64 ActLh;
		Real64 ActLOwnh;
		Real64 Pref;
		Real64 PzFrom;
		Real64 PzTo;
		Array1D< Real64 > RhoLd( 2 );
		Real64 RhoStd;
		int From;
		int To;
		int Fromz;
		int Toz;
		int Ltyp;
		int i;
		int ll;
		int j;
		int k;
		int Pprof;
		int ilayptr;
		int OpenNum;

		Real64 RhoREF;
		Real64 CONV;

		// FLOW:
		RhoREF = PsyRhoAirFnPbTdbW( PSea, OutDryBulbTemp, OutHumRat );

		CONV = Latitude * 2.0 * Pi / 360.0;
		G = 9.780373 * ( 1.0 + 0.0052891 * pow_2( std::sin( CONV ) ) - 0.0000059 * pow_2( std::sin( 2.0 * CONV ) ) );

		Hfl = 1.0;
		Pbz = OutBaroPress;
		Nl = NumOfLinksMultiZone;
		OpenNum = 0;
		RhoLd( 1 ) = 1.2;
		RhoLd( 2 ) = 1.2;
		RhoStd = 1.2;

		for ( i = 1; i <= Nl; ++i ) {
			// Check surface tilt
			if ( i <= Nl - NumOfLinksIntraZone ) { //Revised by L.Gu, on 9 / 29 / 10
				if ( AirflowNetworkLinkageData( i ).DetOpenNum > 0 && Surface( MultizoneSurfaceData( i ).SurfNum ).Tilt < 90 ) {
					Hfl( i ) = Surface( MultizoneSurfaceData( i ).SurfNum ).SinTilt;
				}
			}
			// Initialisation
			From = AirflowNetworkLinkageData( i ).NodeNums( 1 );
			To = AirflowNetworkLinkageData( i ).NodeNums( 2 );
			if ( AirflowNetworkNodeData( From ).EPlusZoneNum > 0 && AirflowNetworkNodeData( To ).EPlusZoneNum > 0 ) {
				ll = 0;
			} else if ( AirflowNetworkNodeData( From ).EPlusZoneNum == 0 && AirflowNetworkNodeData( To ).EPlusZoneNum > 0 ) {
				ll = 1;
			} else {
				ll = 3;
			}

			Ltyp = AirflowNetworkCompData( AirflowNetworkLinkageData( i ).CompNum ).CompTypeNum;
			if ( Ltyp == CompTypeNum_DOP ) {
				ActLh = MultizoneSurfaceData( i ).Height;
				ActLOwnh = ActLh * 1.0;
			} else {
				ActLh = 0.0;
				ActLOwnh = 0.0;
			}

			TempL1 = TZ( From );
			Xhl1 = WZ( From );
			TzFrom = TZ( From );
			XhzFrom = WZ( From );
			RhoL1 = RHOZ( From );
			if ( ll == 0 || ll == 3 ) {
				PzFrom = PZ( From );
			} else {
				PzFrom = 0.0;
				From = 0;
			}

			ilayptr = 0;
			if ( From == 0 ) ilayptr = 1;
			if ( ilayptr == 0 ) {
				Fromz = 0;
			} else {
				Fromz = From;
			}

			TempL2 = TZ( To );
			Xhl2 = WZ( To );
			TzTo = TZ( To );
			XhzTo = WZ( To );
			RhoL2 = RHOZ( To );

			if ( ll < 3 ) {
				PzTo = PZ( To );
			} else {
				PzTo = 0.0;
				To = 0;
			}
			ilayptr = 0;
			if ( To == 0 ) ilayptr = 1;
			if ( ilayptr == 0 ) {
				Toz = 0;
			} else {
				Toz = To;
			}

			// RhoDrL is Rho at link level without pollutant but with humidity
			RhoDrL( i, 1 ) = PsyRhoAirFnPbTdbW( OutBaroPress + PzFrom, TempL1, Xhl1 );
			RhoDrL( i, 2 ) = PsyRhoAirFnPbTdbW( OutBaroPress + PzTo, TempL2, Xhl2 );

			// End initialisation

			// calculate DpF the difference between Pz and P at Node 1 height
			ilayptr = 0;
			if ( Fromz == 0 ) ilayptr = 1;
			j = ilayptr;
			k = 1;
			LClimb( G, RhoLd( 1 ), AirflowNetworkLinkageData( i ).NodeHeights( 1 ), TempL1, Xhl1, DpF( k ), Toz, PzTo, Pbz, RhoDrL( i, 1 ) );
			RhoL1 = RhoLd( 1 );
			// For large openings calculate the stack pressure difference profile and the
			// density profile within the the top- and the bottom- height of the large opening
			if ( ActLOwnh > 0.0 ) {
				HSt( k ) = AirflowNetworkLinkageData( i ).NodeHeights( 1 );
				RhoStF( k ) = RhoL1;
				++k;
				HSt( k ) = 0.0;
				if ( HSt( k - 1 ) < 0.0 ) HSt( k ) = HSt( k - 1 );

				// Search for the first startheight of a layer which is within the top- and the
				// bottom- height of the large opening.
				while ( true ) {
					ilayptr = 0;
					if ( Fromz == 0 ) ilayptr = 9;
					if ( ( j > ilayptr ) || ( HSt( k ) > AirflowNetworkLinkageData( i ).NodeHeights( 1 ) ) ) break;
					j += 9;
					HSt( k ) = 0.0;
					if ( HSt( k - 1 ) < 0.0 ) HSt( k ) = HSt( k - 1 );
				}

				// Calculate Rho and stack pressure for every StartHeight of a layer which is
				// within the top- and the bottom-height of the  large opening.
				while ( true ) {
					ilayptr = 0;
					if ( Fromz == 0 ) ilayptr = 9;
					if ( ( j > ilayptr ) || ( HSt( k ) >= ( AirflowNetworkLinkageData( i ).NodeHeights( 1 ) + ActLOwnh ) ) ) break; //Autodesk:BoundsViolation HSt(k) @ k>2
					T = TzFrom;
					X = XhzFrom;
					LClimb( G, RhoStd, HSt( k ), T, X, DpF( k ), Fromz, PzFrom, Pbz, RhoDrDummi ); //Autodesk:BoundsViolation HSt(k) and DpF(k) @ k>2
					RhoStF( k ) = RhoStd; //Autodesk:BoundsViolation RhoStF(k) @ k>2
					j += 9;
					++k; //Autodesk:Note k>2 now
					HSt( k ) = 0.0; //Autodesk:BoundsViolation @ k>2
					if ( HSt( k - 1 ) < 0.0 ) HSt( k ) = HSt( k - 1 ); //Autodesk:BoundsViolation @ k>2
				}
				// Stack pressure difference and rho for top-height of the large opening
				HSt( k ) = AirflowNetworkLinkageData( i ).NodeHeights( 1 ) + ActLOwnh; //Autodesk:BoundsViolation k>2 poss
				T = TzFrom;
				X = XhzFrom;
				LClimb( G, RhoStd, HSt( k ), T, X, DpF( k ), Fromz, PzFrom, Pbz, RhoDrDummi ); //Autodesk:BoundsViolation k>2 poss
				RhoStF( k ) = RhoStd; //Autodesk:BoundsViolation k >= 3 poss

				for ( j = 1; j <= ( k - 1 ); ++j ) {
					BetaStF( j ) = ( RhoStF( j + 1 ) - RhoStF( j ) ) / ( HSt( j + 1 ) - HSt( j ) );
				}
			}

			// repeat procedure for the "To" node, DpT
			ilayptr = 0;
			if ( Toz == 0 ) ilayptr = 1;
			j = ilayptr;
			// Calculate Rho at link height only if we have large openings or layered zones.
			k = 1;
			LClimb( G, RhoLd( 2 ), AirflowNetworkLinkageData( i ).NodeHeights( 2 ), TempL2, Xhl2, DpT( k ), Toz, PzTo, Pbz, RhoDrL( i, 2 ) );
			RhoL2 = RhoLd( 2 );

			// For large openings calculate the stack pressure difference profile and the
			// density profile within the the top- and the bottom- height of the large opening
			if ( ActLOwnh > 0.0 ) {
				HSt( k ) = AirflowNetworkLinkageData( i ).NodeHeights( 2 );
				RhoStT( k ) = RhoL2;
				++k;
				HSt( k ) = 0.0;
				if ( HSt( k - 1 ) < 0.0 ) HSt( k ) = HSt( k - 1 );
				while ( true ) {
					ilayptr = 0;
					if ( Toz == 0 ) ilayptr = 9;
					if ( ( j > ilayptr ) || ( HSt( k ) > AirflowNetworkLinkageData( i ).NodeHeights( 2 ) ) ) break;
					j += 9;
					HSt( k ) = 0.0;
					if ( HSt( k - 1 ) < 0.0 ) HSt( k ) = HSt( k - 1 );
				}
				// Calculate Rho and stack pressure for every StartHeight of a layer which is
				// within the top- and the bottom-height of the  large opening.
				while ( true ) {
					ilayptr = 0;
					if ( Toz == 0 ) ilayptr = 9;
					if ( ( j > ilayptr ) || ( HSt( k ) >= ( AirflowNetworkLinkageData( i ).NodeHeights( 2 ) + ActLOwnh ) ) ) break; //Autodesk:BoundsViolation Hst(k) @ k>2
					T = TzTo;
					X = XhzTo;
					LClimb( G, RhoStd, HSt( k ), T, X, DpT( k ), Toz, PzTo, Pbz, RhoDrDummi ); //Autodesk:BoundsViolation HSt(k) and DpT(k) @ k>2
					RhoStT( k ) = RhoStd; //Autodesk:BoundsViolation RhoStT(k) @ k>2
					j += 9;
					++k; //Autodesk:Note k>2 now
					HSt( k ) = 0.0; //Autodesk:BoundsViolation @ k>2
					if ( HSt( k - 1 ) < 0.0 ) HSt( k ) = HSt( k - 1 ); //Autodesk:BoundsViolation @ k>2
				}
				// Stack pressure difference and rho for top-height of the large opening
				HSt( k ) = AirflowNetworkLinkageData( i ).NodeHeights( 2 ) + ActLOwnh; //Autodesk:BoundsViolation k>2 poss
				T = TzTo;
				X = XhzTo;
				LClimb( G, RhoStd, HSt( k ), T, X, DpT( k ), Toz, PzTo, Pbz, RhoDrDummi ); //Autodesk:BoundsViolation k>2 poss
				RhoStT( k ) = RhoStd; //Autodesk:BoundsViolation k>2 poss

				for ( j = 1; j <= ( k - 1 ); ++j ) {
					BetaStT( j ) = ( RhoStT( j + 1 ) - RhoStT( j ) ) / ( HSt( j + 1 ) - HSt( j ) );
				}
			}

			// CALCULATE STACK PRESSURE FOR THE PATH ITSELF for different flow directions
			H = double( AirflowNetworkLinkageData( i ).NodeHeights( 2 ) ) - double( AirflowNetworkLinkageData( i ).NodeHeights( 1 ) );
			if ( ll == 0 || ll == 3 || ll == 6 ) {
				H -= AirflowNetworkNodeData( From ).NodeHeight;
			}
			if ( ll < 3 ) {
				H += AirflowNetworkNodeData( To ).NodeHeight;
			}

			// IF AIR FLOWS from "From" to "To"
			Pref = Pbz + PzFrom + DpF( 1 );
			DpP = psz( Pref, RhoLd( 1 ), 0.0, 0.0, H, G );
			DpL( i, 1 ) = ( DpF( 1 ) - DpT( 1 ) + DpP );

			// IF AIR FLOWS from "To" to "From"
			Pref = Pbz + PzTo + DpT( 1 );
			DpP = -psz( Pref, RhoLd( 2 ), 0.0, 0.0, -H, G );
			DpL( i, 2 ) = ( DpF( 1 ) - DpT( 1 ) + DpP );

			if ( Ltyp == CompTypeNum_DOP ) {
				Pprof = OpenNum * ( NrInt + 2 );
				PresProfile( i, Pprof, G, DpF, DpT, BetaStF, BetaStT, RhoStF, RhoStT, From, To, ActLh, Hfl( i ) );
				++OpenNum;
			}

		}

	}

	Real64
	psz(
		Real64 const Pz0, // Pressure at altitude z0 [Pa]
		Real64 const Rho0, // density at altitude z0 [kg/m3]
		Real64 const beta, // density gradient [kg/m4]
		Real64 const z0, // reference altitude [m]
		Real64 const z, // altitude[m]
		Real64 const g // gravity field strength [N/kg]
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Lixing Gu
		//       DATE WRITTEN   Oct. 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  This subroutine is revised based on psz function from COMIS

		// PURPOSE OF THIS SUBROUTINE:
		// This function determines the pressure due to buoyancy in a stratified density environment

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 psz;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 dz;
		Real64 rho;

		// FLOW:
		dz = z - z0;
		rho = ( Rho0 + beta * dz / 2.0 );
		psz = -Pz0 * ( 1.0 - std::exp( -dz * rho * g / Pz0 ) ); // Differential pressure from z to z0 [Pa]

		return psz;
	}

	void
	LClimb(
		Real64 const G, // gravity field strength [N/kg]
		Real64 & Rho, // Density link level (initialized with rho zone) [kg/m3]
		Real64 const Z, // Height of the link above the zone reference [m]
		Real64 & T, // temperature at link level [C]
		Real64 & X, // absolute humidity at link level [kg/kg]
		Real64 & Dp, // Stackpressure to the linklevel [Pa]
		int const zone, // Zone number
		Real64 const PZ, // Zone Pressure (reflevel) [Pa]
		Real64 const Pbz, // Barometric pressure at entrance level [Pa]
		Real64 & RhoDr // Air density of dry air on the link level used
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Lixing Gu
		//       DATE WRITTEN   Oct. 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  This subroutine is revised based on subroutine IClimb from COMIS

		// PURPOSE OF THIS SUBROUTINE:
		// This function the differential pressure from the reflevel in a zone To Z, the level of a link

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// for the concentration routine [kg/m3]

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 H; // Start Height of the layer
		Real64 BetaT; // Temperature gradient of this layer
		Real64 BetaXfct; // Humidity gradient factor of this layer
		Real64 BetaCfct; // Concentration 1 gradient factor of this layer
		Real64 X0;
		Real64 P;
		Real64 Htop;
		Real64 Hbot;
		Real64 Rho0;
		Real64 Rho1;
		Real64 BetaRho;
		static int L( 0 );
		static int ilayptr( 0 );

		// FLOW:
		Dp = 0.0;
		Rho0 = Rho;
		X0 = X;
		if ( Z > 0.0 ) {
			// initialize start values
			H = 0.0;
			BetaT = 0.0;
			BetaXfct = 0.0;
			BetaCfct = 0.0;
			BetaRho = 0.0;
			Hbot = 0.0;

			while ( H < 0.0 ) {
				// loop until H>0 ; The start of the layer is above 0
				BetaT = 0.0;
				BetaXfct = 0.0;
				BetaCfct = 0.0;
				L += 9;
				ilayptr = 0;
				if ( zone == 0 ) ilayptr = 9;
				if ( L >= ilayptr ) {
					H = Z + 1.0;
				} else {
					H = 0.0;
				}
			}

			// The link is in this layer also if it is on the top of it.

			while ( true ) {
				if ( H >= Z ) {
					// the link ends in this layer , we reached the final Dp and BetaRho
					Htop = Z;
					P = PZ + Dp;
					if ( Htop != Hbot ) {
						Rho0 = PsyRhoAirFnPbTdbW( Pbz + P, T, X );
						T += ( Htop - Hbot ) * BetaT;
						X += ( Htop - Hbot ) * BetaXfct * X0;
						Rho1 = PsyRhoAirFnPbTdbW( Pbz + P, T, X );
						BetaRho = ( Rho1 - Rho0 ) / ( Htop - Hbot );
						Dp += psz( Pbz + P, Rho0, BetaRho, Hbot, Htop, G );
					}
					RhoDr = PsyRhoAirFnPbTdbW( Pbz + PZ + Dp, T, X );
					Rho = PsyRhoAirFnPbTdbW( Pbz + PZ + Dp, T, X );
					return;

				} else {
					// bottom of the layer is below Z  (Z above ref)
					Htop = H;
					// P is the pressure up to the start height of the layer we just reached
					P = PZ + Dp;
					if ( Htop != Hbot ) {
						Rho0 = PsyRhoAirFnPbTdbW( Pbz + P, T, X );
						T += ( Htop - Hbot ) * BetaT;
						X += ( Htop - Hbot ) * BetaXfct * X0;
						Rho1 = PsyRhoAirFnPbTdbW( Pbz + P, T, X );
						BetaRho = ( Rho1 - Rho0 ) / ( Htop - Hbot );
						Dp += psz( Pbz + P, Rho0, BetaRho, Hbot, Htop, G );
					}

					RhoDr = PsyRhoAirFnPbTdbW( Pbz + PZ + Dp, T, X );
					Rho = PsyRhoAirFnPbTdbW( Pbz + PZ + Dp, T, X );

					// place current values Hbot and Beta's
					Hbot = H;
					BetaT = 0.0;
					BetaXfct = 0.0;
					BetaCfct = 0.0;
					L += 9;
					ilayptr = 0;
					if ( zone == 0 ) ilayptr = 9;
					if ( L >= ilayptr ) {
						H = Z + 1.0;
					} else {
						H = 0.0;
					}
				}
			}

		} else {
			// This is the ELSE for negative linkheights Z below the refplane
			H = 0.0;
			BetaT = 0.0;
			BetaXfct = 0.0;
			BetaCfct = 0.0;
			BetaRho = 0.0;
			Htop = 0.0;
			while ( H > 0.0 ) {
				// loop until H<0 ; The start of the layer is below the zone refplane
				L -= 9;
				ilayptr = 0;
				if ( zone == 0 ) ilayptr = 1;
				if ( L < ilayptr ) {
					// with H=Z (negative) this loop will exit, no data for interval Z-refplane
					H = Z;
					BetaT = 0.0;
					BetaXfct = 0.0;
					BetaCfct = 0.0;
					BetaRho = 0.0;
				} else {
					H = 0.0;
					BetaT = 0.0;
					BetaXfct = 0.0;
					BetaCfct = 0.0;
				}
			}

			// The link is in this layer also if it is on the bottom of it.
			while ( true ) {
				if ( H <= Z ) {
					Hbot = Z;
					P = PZ + Dp;
					if ( Htop != Hbot ) {
						Rho1 = PsyRhoAirFnPbTdbW( Pbz + P, T, X );
						T += ( Hbot - Htop ) * BetaT;
						X += ( Hbot - Htop ) * BetaXfct * X0;
						Rho0 = PsyRhoAirFnPbTdbW( Pbz + P, T, X );
						BetaRho = ( Rho1 - Rho0 ) / ( Htop - Hbot );
						Dp -= psz( Pbz + P, Rho0, BetaRho, Hbot, Htop, G );
					}
					RhoDr = PsyRhoAirFnPbTdbW( Pbz + PZ + Dp, T, X );
					Rho = PsyRhoAirFnPbTdbW( Pbz + PZ + Dp, T, X );
					return;
				} else {
					// bottom of the layer is below Z  (Z below ref)
					Hbot = H;
					P = PZ + Dp;
					if ( Htop != Hbot ) {
						Rho1 = PsyRhoAirFnPbTdbW( Pbz + P, T, X );
						// T,X,C calculated for the lower height
						T += ( Hbot - Htop ) * BetaT;
						X += ( Hbot - Htop ) * BetaXfct * X0;
						Rho0 = PsyRhoAirFnPbTdbW( Pbz + P, T, X );
						BetaRho = ( Rho1 - Rho0 ) / ( Htop - Hbot );
						Dp -= psz( Pbz + P, Rho0, BetaRho, Hbot, Htop, G );
					}
					RhoDr = PsyRhoAirFnPbTdbW( Pbz + PZ + Dp, T, X );
					Rho = PsyRhoAirFnPbTdbW( Pbz + PZ + Dp, T, X );

					// place current values Hbot and Beta's
					Htop = H;
					L -= 9;
					ilayptr = 0;
					if ( zone == 0 ) ilayptr = 1;
					if ( L < ilayptr ) {
						H = Z - 1.0;
						BetaT = 0.0;
						BetaXfct = 0.0;
						BetaCfct = 0.0;
					} else {
						H = 0.0;
						BetaT = 0.0;
						BetaXfct = 0.0;
						BetaCfct = 0.0;
					}
				}
				// ENDIF H<Z
			}
		}

	}

	//*****************************************************************************************

} // AirflowNetworkSolver

} // EnergyPlus
