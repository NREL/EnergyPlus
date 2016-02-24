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
#include <cassert>
#include <cmath>
#include <cstddef>
#include <cstdint>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/ArrayS.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <WindowComplexManager.hh>
#include <DataComplexFenestration.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalSurface.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataShadowingCombinations.hh>
#include <DataSurfaces.hh>
#include <DataSystemVariables.hh>
#include <DataZoneEquipment.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <PierceSurface.hh>
#include <Psychrometrics.hh>
#include <TARCOGGassesParams.hh>
#include <TARCOGMain.hh>
#include <TARCOGParams.hh>
#include <UtilityRoutines.hh>
#include <Vectors.hh>

namespace EnergyPlus {

namespace WindowComplexManager {

	// Module containing the routines dealing with complex fenestration

	// MODULE INFORMATION:
	//       AUTHOR         Joe Klems
	//       DATE WRITTEN   ???
	//       MODIFIED       November 2011, Simon Vidanovic
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	//  Initialize data for solar and thermal calculations and also performs thermal calculations for BSDF window

	// METHODOLOGY EMPLOYED:
	// <description>

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataComplexFenestration;
	using namespace DataPrecisionGlobals;
	using namespace DataVectorTypes;
	using namespace DataBSDFWindow;
	using DataGlobals::Pi;
	using DataGlobals::DegToRadians;
	using DataGlobals::NumOfTimeStepInHour;
	using DataGlobals::NumOfZones;
	using DataGlobals::rTinyValue;
	using DataGlobals::KelvinConv;
	using DataGlobals::TimeStepZoneSec;
	using namespace DataSurfaces; // , ONLY: TotSurfaces,TotWindows,Surface,SurfaceWindow   !update this later
	using DataEnvironment::SunIsUpValue;
	using DataEnvironment::SkyTempKelvin;
	using DataEnvironment::IsRain;
	using DataEnvironment::SunIsUp;
	using DataEnvironment::CloudFraction;
	using DataEnvironment::OutBaroPress;
	using DataEnvironment::OutHumRat;
	using namespace DataHeatBalance;
	using namespace DataShadowingCombinations;
	using namespace Vectors;
	using namespace DataHeatBalFanSys;

	// Data
	// MODULE PARAMETER DEFINITIONS:

	Real64 const sigma( 5.6697e-8 ); // Stefan-Boltzmann constant
	Real64 const PressureDefault( 101325.0 );

	int const Calculate_Geometry( 1 );
	int const Copy_Geometry( 2 );

	int const TmpLen( 20 ); // Length increment of temporary arrays

	int const Front_Incident( 1 ); // Ray identification types
	int const Front_Transmitted( 2 );
	int const Front_Reflected( 3 );
	int const Back_Incident( 4 );
	int const Back_Transmitted( 5 );
	int const Back_Reflected( 6 );

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:

	int NumComplexWind( 0 ); // Total number of complex windows

	// SUBROUTINE SPECIFICATIONS FOR MODULE WindowComplexManager:

	// Object Data
	Array1D< BasisStruct > BasisList;
	Array1D< WindowIndex > WindowList;
	Array2D< WindowStateIndex > WindowStateList;

	// Functions

	void
	clear_state()
	{
		NumComplexWind = 0;
		BasisList.deallocate();
		WindowList.deallocate();
		WindowStateList.deallocate();
	}

	void
	InitBSDFWindows()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Joe Klems
		//       DATE WRITTEN   August 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Set up the overall optical geometry for a BSDF window

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace Vectors;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool Once( true ); // Flag for insuring things happen once
		static int IBasis( 0 ); // Index for identifying basis in BasisList
		static int ISurf( 0 ); // Index for sorting thru Surface array
		static int IConst( 0 ); // Index for accessing Construct array
		static int IState( 0 ); // Index identifying the window state for a particular window
		static int IWind( 0 ); // Index identifying a window in the WindowList
		static int I( 0 ); // general purpose index
		static int J( 0 ); // general purpose index
		static int JSurf( 0 ); // back surface number
		int BaseSurf; // base surface number (used in finding back surface)
		static int K( 0 ); // general purpose index
		static int KBkSurf( 0 ); // back surface index
		static int KBasis( 0 ); // secondary reference to a basis index
		static int NumBasis( 0 ); // Number of unique bases (No. in BasisList)
		static int NBkSurf( 0 ); // Local variable for the number of back surfaces
		int NumStates; // Local variable for the number of states
		static int MatrixNo( 0 ); // Index of Basis matrix
		Array1D< Real64 > Thetas; // temp array holding theta values
		Array1D_int NPhis; // temp array holding number of phis for a given theta
		Array1D< Real64 > V( 3 ); // vector array
		Real64 VLen; // Length of vector array
		int NHold; // No. values in the Temporary array

		struct TempBasisIdx
		{
			// Members
			int Basis; // Basis no in basis table
			int State; // State in which basis first occurs

			// Default Constructor
			TempBasisIdx() :
			Basis( 0 ),
			State( 0 )
			{}

		};

		// Object Data
		Array1D< TempBasisIdx > IHold; // Temporary array

		if ( TotComplexFenStates <= 0 ) return; //Nothing to do if no complex fenestration states
		//Construct Basis List
		BasisList.allocate( TotComplexFenStates );

		//Note:  Construction of the basis list contains the assumption of identical incoming and outgoing bases in
		//            that the complex fenestration state definition contains only one basis description, hence
		//            assumes square property matrices.  If this assumption were relaxed through change of the
		//            definition or additional definition of a state type with non-square matrices, then the loop
		//            below should be modified to enter both of the bases into the basis list.

		for ( IConst = FirstBSDF; IConst <= FirstBSDF + TotComplexFenStates - 1; ++IConst ) {
			MatrixNo = Construct( IConst ).BSDFInput.BasisMatIndex;
			if ( NumBasis == 0 ) {
				NumBasis = 1;
				ConstructBasis( IConst, BasisList( 1 ) );
			} else {
				for ( IBasis = 1; IBasis <= NumBasis; ++IBasis ) {
					if ( MatrixNo == BasisList( IBasis ).BasisMatIndex ) goto BsLoop_loop;
				}
				++NumBasis;
				ConstructBasis( IConst, BasisList( NumBasis ) );
			}
			BsLoop_loop: ;
		}
		BasisList.redimension( NumBasis );
		//  Proceed to set up geometry for complex fenestration states
		ComplexWind.allocate( TotSurfaces ); //Set up companion array to SurfaceWindow to hold window
		//     geometry for each state.  This is an allocatable array of
		//     geometries for the window states but only the complex
		//     fenestration surfaces will have the arrays allocated
		//  Search Thru Surfaces for Complex Fenestration State references
		//  This will define the first complex fenestration state for that window, others will follow if there are
		//     control specifications
		WindowList.allocate( TotSurfaces ); //Temporary allocation
		WindowStateList.allocate( TotComplexFenStates, TotSurfaces ); //Temporary allocation
		for ( ISurf = 1; ISurf <= TotSurfaces; ++ISurf ) {
			IConst = Surface( ISurf ).Construction;
			if ( IConst == 0 ) continue; // This is true for overhangs (Shading:Zone:Detailed)
			if ( ! ( Construct( IConst ).TypeIsWindow && ( Construct( IConst ).WindowTypeBSDF ) ) ) continue; //Only BSDF windows
			//Simon Check: Thermal construction removed
			//ThConst = Construct(IConst)%BSDFInput%ThermalConstruction
			SurfaceWindow( ISurf ).WindowModelType = WindowBSDFModel;
			++NumComplexWind;
			NumStates = 1;
			WindowList( NumComplexWind ).NumStates = 1; //Having found the construction reference in
			// the Surface array defines the first state for this window
			WindowList( NumComplexWind ).SurfNo = ISurf;
			//WindowList( NumComplexWind ).Azimuth = DegToRadians * Surface( ISurf ).Azimuth;
			//WindowList( NumComplexWind ).Tilt = DegToRadians * Surface( ISurf ).Tilt;
			WindowStateList( NumStates, NumComplexWind ).InitInc = Calculate_Geometry;
			WindowStateList( NumStates, NumComplexWind ).InitTrn = Calculate_Geometry;
			WindowStateList( NumStates, NumComplexWind ).CopyIncState = 0;
			WindowStateList( NumStates, NumComplexWind ).CopyTrnState = 0;
			WindowStateList( NumStates, NumComplexWind ).Konst = IConst;
			//Simon Check: ThermalConstruction assigned to current construction
			//WindowStateList(NumComplexWind, NumStates)%ThermConst = ThConst
			for ( I = 1; I <= NumBasis; ++I ) { //Find basis in Basis List
				if ( Construct( IConst ).BSDFInput.BasisMatIndex == BasisList( I ).BasisMatIndex ) {
					WindowStateList( NumStates, NumComplexWind ).IncBasisIndx = I; //Note: square property matrices
					WindowStateList( NumStates, NumComplexWind ).TrnBasisIndx = I; //   assumption
				}
			}
			if ( WindowStateList( NumStates, NumComplexWind ).IncBasisIndx <= 0 ) {
				ShowFatalError( "Complex Window Init: Window Basis not in BasisList." );
			}
		}
		//  Should now have a WindowList with NumComplexWind entries containing all the complex fenestrations
		//    with a first state defined for each.
		//  *  *  *
		//  Here a search should be made for control specifications, which will give additional states for
		//    controlled complex fenestrations.  These should be added to the WindowStateList, and
		//     WindowList( )%NumStates incremented for each window for which states are added.
		//      Added states should have WindowStateList ( , )%InitInc set to Calculate_Geometry
		//  *  *  *

		// At this point, we have a complete WindowList and WindowStateList, with NumComplexWind
		//   defined, and NumStates for each complex window defined
		// Now sort through the window list to see that geometry will only be done once for each
		//  window, basis combination
		// Note:  code below assumes identical incoming and outgoing bases; following code will
		//   need revision if this assumption relaxed

		for ( IWind = 1; IWind <= NumComplexWind; ++IWind ) { //Search window list for repeated bases
			if ( WindowList( IWind ).NumStates > 1 ) {
				IHold.allocate( WindowList( IWind ).NumStates );
				NHold = 1;
				IHold( 1 ).State = 1;
				IHold( 1 ).Basis = WindowStateList( 1, IWind ).IncBasisIndx;
				// If the Mth new basis found is basis B in the basis list, and it
				// first occurs in the WindowStateList  in state N, then IHold(M)%Basis=B
				// and IHold(M)%State=N
				for ( K = 1; K <= NumBasis; ++K ) {
					if ( K > NHold ) break;
					KBasis = IHold( K ).Basis;
					J = IHold( K ).State;
					Once = true;
					for ( I = J + 1; I <= WindowList( IWind ).NumStates; ++I ) { //See if subsequent states have the same basis
						if ( ( WindowStateList( I, NumComplexWind ).InitInc == Calculate_Geometry ) && ( WindowStateList( I, NumComplexWind ).IncBasisIndx == KBasis ) ) {
							//Note:  square property matrices (same inc & trn bases) assumption
							//If same incident and outgoing basis assumption removed, following code will need to
							//  be extended to treat the two bases separately
							WindowStateList( I, NumComplexWind ).InitInc = Copy_Geometry;
							WindowStateList( I, NumComplexWind ).InitTrn = Copy_Geometry;
							WindowStateList( I, NumComplexWind ).CopyIncState = J;
							WindowStateList( I, NumComplexWind ).CopyTrnState = J;
						} else if ( Once ) {
							Once = false; //First occurrence of a different basis
							++NHold;
							IHold( NHold ).State = I;
							IHold( NHold ).Basis = WindowStateList( I, IWind ).IncBasisIndx;
							WindowStateList( I, NumComplexWind ).InitTrn = Calculate_Geometry;
							WindowStateList( I, NumComplexWind ).CopyIncState = 0;
							WindowStateList( I, NumComplexWind ).CopyTrnState = 0;
						}
					}
				}
				IHold.deallocate();
			}
		}

		//  Now go through window list and window state list and calculate or copy the
		//   geometry information for each window, state
		for ( IWind = 1; IWind <= NumComplexWind; ++IWind ) {
			ISurf = WindowList( IWind ).SurfNo;
			NumStates = WindowList( IWind ).NumStates;
			//ALLOCATE(SurfaceWindow( ISurf )%ComplexFen)    !activate the BSDF window description
			//  for this surface
			SurfaceWindow( ISurf ).ComplexFen.NumStates = NumStates;
			SurfaceWindow( ISurf ).ComplexFen.State.allocate( NumStates ); //Allocate space for the states
			ComplexWind( ISurf ).NumStates = NumStates;
			ComplexWind( ISurf ).Geom.allocate( NumStates ); //Allocate space for the geometries
			//Azimuth = WindowList( IWind ).Azimuth;
			//Tilt = WindowList( IWind ).Tilt;
			// Get the number of back surfaces for this window
			BaseSurf = Surface( ISurf ).BaseSurf; //ShadowComb is organized by base surface
			NBkSurf = ShadowComb( BaseSurf ).NumBackSurf;
			ComplexWind( ISurf ).NBkSurf = NBkSurf;
			// Define the back surface directions
			ComplexWind( ISurf ).sWinSurf.allocate( NBkSurf );
			ComplexWind( ISurf ).sdotN.allocate( NBkSurf );
			//Define the unit vectors pointing from the window center to the back surface centers
			for ( KBkSurf = 1; KBkSurf <= NBkSurf; ++KBkSurf ) {
				BaseSurf = Surface( ISurf ).BaseSurf; //ShadowComb is organized by base surface
				JSurf = ShadowComb( BaseSurf ).BackSurf( KBkSurf ); //these are all proper back surfaces
				V = Surface( JSurf ).Centroid - Surface( ISurf ).Centroid;
				VLen = magnitude( V );
				//Define the unit vector from the window center to the back
				ComplexWind( ISurf ).sWinSurf( KBkSurf ) = V / VLen;
				//surface center
				//Define the back surface cosine(incident angle)
				ComplexWind( ISurf ).sdotN( KBkSurf ) = dot( V, Surface( JSurf ).OutNormVec ) / VLen;
			}
			for ( IState = 1; IState <= NumStates; ++IState ) {
				//The following assumes identical incoming and outgoing bases.  The logic will need to be
				//  redesigned if this assumption is relaxed
				IConst = WindowStateList( IState, IWind ).Konst;
				//ThConst = WindowStateList ( IWind , IState )%ThermConst
				SurfaceWindow( ISurf ).ComplexFen.State( IState ).Konst = IConst;
				//SurfaceWindow(ISurf)%ComplexFen%State(IState)%ThermConst = ThConst
				if ( WindowStateList( IState, IWind ).InitInc == Calculate_Geometry ) {
					ComplexWind( ISurf ).Geom( IState ).Inc = BasisList( WindowStateList( IState, IWind ).IncBasisIndx ); //Put in the basis structure from the BasisList
					ComplexWind( ISurf ).Geom( IState ).Trn = BasisList( WindowStateList( IState, IWind ).TrnBasisIndx );

					SetupComplexWindowStateGeometry( ISurf, IState, IConst, ComplexWind( ISurf ), ComplexWind( ISurf ).Geom( IState ), SurfaceWindow( ISurf ).ComplexFen.State( IState ) );
					//Note--setting up the state geometry will include constructing outgoing basis/surface
					//  maps and those incoming maps that will not depend on shading.
				} else {
					SurfaceWindow( ISurf ).ComplexFen.State( IState ) = SurfaceWindow( ISurf ).ComplexFen.State( WindowStateList( IState, IWind ).CopyIncState ); //Note this overwrites Konst
					SurfaceWindow( ISurf ).ComplexFen.State( IState ).Konst = IConst; //  so it has to be put back
					//SurfaceWindow (ISurf )%ComplexFen%State(IState)%ThermConst = ThConst  !same for ThermConst
					ComplexWind( ISurf ).Geom( IState ) = ComplexWind( ISurf ).Geom( WindowStateList( IState, IWind ).CopyIncState );
				}

			} //State loop
		} //Complex Window loop
		//  Allocate all beam-dependent complex fenestration quantities
		for ( IWind = 1; IWind <= NumComplexWind; ++IWind ) {
			ISurf = WindowList( IWind ).SurfNo;
			NumStates = WindowList( IWind ).NumStates;
			for ( IState = 1; IState <= NumStates; ++IState ) {
				AllocateCFSStateHourlyData( ISurf, IState );
			} //State loop
		} //Complex Window loop

	}

	void
	AllocateCFSStateHourlyData(
		int const iSurf, // Surface number
		int const iState // Complex fenestration state number
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Vidanovic
		//       DATE WRITTEN   May 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Allocate hourly data arrays for complex fenestration state

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
		int NLayers; // Number of complex fenestration layers
		int NBkSurf; // Number of back surfaces
		int KBkSurf; // Back surfaces counter

		NLayers = SurfaceWindow( iSurf ).ComplexFen.State( iState ).NLayers;
		NBkSurf = ComplexWind( iSurf ).NBkSurf;

		ComplexWind( iSurf ).Geom( iState ).SolBmGndWt.allocate( 24, NumOfTimeStepInHour, ComplexWind( iSurf ).Geom( iState ).NGnd );
		ComplexWind( iSurf ).Geom( iState ).SolBmIndex.allocate( 24, NumOfTimeStepInHour );
		ComplexWind( iSurf ).Geom( iState ).ThetaBm.allocate( 24, NumOfTimeStepInHour );
		ComplexWind( iSurf ).Geom( iState ).PhiBm.allocate( 24, NumOfTimeStepInHour );
		SurfaceWindow( iSurf ).ComplexFen.State( iState ).WinDirHemiTrans.allocate( 24, NumOfTimeStepInHour );
		SurfaceWindow( iSurf ).ComplexFen.State( iState ).WinDirSpecTrans.allocate( 24, NumOfTimeStepInHour );
		SurfaceWindow( iSurf ).ComplexFen.State( iState ).WinBmGndTrans.allocate( 24, NumOfTimeStepInHour );
		SurfaceWindow( iSurf ).ComplexFen.State( iState ).WinBmFtAbs.allocate( 24, NumOfTimeStepInHour, NLayers );
		SurfaceWindow( iSurf ).ComplexFen.State( iState ).WinBmGndAbs.allocate( 24, NumOfTimeStepInHour, NLayers );
		SurfaceWindow( iSurf ).ComplexFen.State( iState ).WinToSurfBmTrans.allocate( 24, NumOfTimeStepInHour, NBkSurf );
		SurfaceWindow( iSurf ).ComplexFen.State( iState ).BkSurf.allocate( NBkSurf );
		for ( KBkSurf = 1; KBkSurf <= NBkSurf; ++KBkSurf ) {
			SurfaceWindow( iSurf ).ComplexFen.State( iState ).BkSurf( KBkSurf ).WinDHBkRefl.allocate( 24, NumOfTimeStepInHour );
			SurfaceWindow( iSurf ).ComplexFen.State( iState ).BkSurf( KBkSurf ).WinDirBkAbs.allocate( 24, NumOfTimeStepInHour, NLayers );
		}

	}

	void
	ExpandComplexState(
		int const iSurf, // Surface number
		int const iConst // Construction number
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Vidanovic
		//       DATE WRITTEN   May 2013
		//       MODIFIED       Simon Vidanovic (July 2013)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// When complex fenestration is controlled by EMS, program does not know in advance how many states are assigned to
		// ceratin surface. This information can be obtain only at runtime. Purpose of this routine is to extend number of states
		// used by complex fenestration in case that is necessary.

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

		// Expands states by one
		int NumOfStates = SurfaceWindow( iSurf ).ComplexFen.NumStates;

		ComplexWind( iSurf ).Geom.redimension( NumOfStates + 1 );
		SurfaceWindow( iSurf ).ComplexFen.State.redimension( NumOfStates + 1 );

		// Do daylighting geometry only in case it is initialized. If daylighting is not used then no need to expand state for that
		if ( ComplexWind( iSurf ).DaylightingInitialized ) {
			ComplexWind( iSurf ).DaylghtGeom.redimension( NumOfStates + 1 );
			ComplexWind( iSurf ).DaylightingInitialized = false;
		} else {
			ComplexWind( iSurf ).DaylghtGeom.allocate( NumOfStates + 1 );
		}

		// Increase number of states and insert new state
		++NumOfStates;
		SurfaceWindow( iSurf ).ComplexFen.NumStates = NumOfStates;
		ComplexWind( iSurf ).NumStates = NumOfStates;

		SurfaceWindow( iSurf ).ComplexFen.State( NumOfStates ).Konst = iConst;

		// load basis and setup window state geometry
		ConstructBasis( iConst, ComplexWind( iSurf ).Geom( NumOfStates ).Inc );
		ConstructBasis( iConst, ComplexWind( iSurf ).Geom( NumOfStates ).Trn );

		SetupComplexWindowStateGeometry( iSurf, NumOfStates, iConst, ComplexWind( iSurf ), ComplexWind( iSurf ).Geom( NumOfStates ), SurfaceWindow( iSurf ).ComplexFen.State( NumOfStates ) );

		// allocation of memory for hourly data can be performed only after window state geometry has been setup
		AllocateCFSStateHourlyData( iSurf, NumOfStates );

		// calculate static properties for complex fenestration
		CalcWindowStaticProperties( iSurf, NumOfStates, ComplexWind( iSurf ), ComplexWind( iSurf ).Geom( NumOfStates ), SurfaceWindow( iSurf ).ComplexFen.State( NumOfStates ) );

		// calculate hourly data from complex fenestration
		CFSShadeAndBeamInitialization( iSurf, NumOfStates );

	}

	void
	CheckCFSStates( int const iSurf ) // Surface number
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Vidanovic
		//       DATE WRITTEN   May 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Check if there are new states available for complex fenestration and performs proper initialization

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
		int NumOfStates; // number of states for current surface
		bool StateFound; // variable to indicate if state has been found
		int i; // Local counter
		int CurrentCFSState;

		StateFound = false;
		CurrentCFSState = SurfaceWindow( iSurf ).ComplexFen.CurrentState;

		// Check if EMS changed construction number
		if ( Surface( iSurf ).Construction != SurfaceWindow( iSurf ).ComplexFen.State( CurrentCFSState ).Konst ) {

			// If construction number changed then take new state
			// First search for existing states. Maybe state is already added in previous timestep
			NumOfStates = SurfaceWindow( iSurf ).ComplexFen.NumStates;
			for ( i = 1; i <= NumOfStates; ++i ) {
				if ( Surface( iSurf ).Construction == SurfaceWindow( iSurf ).ComplexFen.State( i ).Konst ) {
					StateFound = true;
					CurrentCFSState = i;
					SurfaceWindow( iSurf ).ComplexFen.CurrentState = i;
				}
			}
		} else {
			StateFound = true;
		}

		// If new state is not found in the list of current states, then create new one, initialize and make it active
		if ( ! StateFound ) {
			ExpandComplexState( iSurf, Surface( iSurf ).Construction );
			CurrentCFSState = SurfaceWindow( iSurf ).ComplexFen.NumStates;
			SurfaceWindow( iSurf ).ComplexFen.CurrentState = CurrentCFSState;
		}

	}

	void
	InitComplexWindows()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   November 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Extract simple init for Complex Windows

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

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool Once( true ); // Flag for insuring things happen once

		//One-time initialization
		if ( Once ) {
			Once = false;
			InitBSDFWindows();
			CalcStaticProperties();
		}

	}

	void
	UpdateComplexWindows()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Joe Klems
		//       DATE WRITTEN   August 2011
		//       MODIFIED       B. Griffith, Nov. 2012 revised for detailed timestep integration mode
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Performs the shading-dependent initialization of the Complex Fenestration data;
		// On first call, calls the one-time initializition

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::KickOffSizing;
		using DataGlobals::KickOffSimulation;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// LOGICAL,SAVE    ::  Once  =.TRUE.  !Flag for insuring things happen once
		int NumStates; // Number of states for a given complex fen
		int ISurf; // Index for sorting thru Surface array
		int IState; // Index identifying the window state for a particular window
		int IWind; // Index identifying a window in the WindowList

		// !One-time initialization
		//  IF (Once) THEN
		//    ONCE = .FALSE.
		//    CALL InitBSDFWindows
		//    CALL CalcStaticProperties
		//  ENDIF

		if ( NumComplexWind == 0 ) return;

		if ( KickOffSizing || KickOffSimulation ) return;

		//Shading-dependent initialization; performed once for each shading period

		// Initialize the geometric quantities

		for ( IWind = 1; IWind <= NumComplexWind; ++IWind ) {
			ISurf = WindowList( IWind ).SurfNo;
			NumStates = ComplexWind( ISurf ).NumStates;
			for ( IState = 1; IState <= NumStates; ++IState ) {
				CFSShadeAndBeamInitialization( ISurf, IState );
			} //State loop
		} //window loop

	}

	void
	CFSShadeAndBeamInitialization(
		int const iSurf, // Window surface number
		int const iState // Window state number
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Vidanovic
		//       DATE WRITTEN   May 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates shading properties of complex fenestration
		// Refactoring from Klems code

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace Vectors;
		using DataGlobals::HourOfDay;
		using DataGlobals::TimeStep;
		using DataGlobals::KickOffSizing;
		using DataGlobals::KickOffSimulation;
		using DataSystemVariables::DetailedSolarTimestepIntegration;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// Object Data
		static Vector SunDir( 0.0, 0.0, 1.0 ); // unit vector pointing toward sun (world CS)
		static Vector Posit( 0.0, 0.0, 1.0 ); // vector location of current ground point
		static Vector HitPt( 0.0, 0.0, 1.0 ); // vector location of ray intersection with a surface

		if ( KickOffSizing || KickOffSimulation ) return;

		int IncRay; // Index of incident ray corresponding to beam direction
		Real64 Theta; // Theta angle of incident ray correspongind to beam direction
		Real64 Phi; // Phi angle of incident ray correspongind to beam direction
		bool hit; // hit flag
		int TotHits; // hit counter
		auto & complexWindow( ComplexWind( iSurf ) );
		auto & complexWindowGeom( complexWindow.Geom( iState ) );
		auto & surfaceWindowState( SurfaceWindow( iSurf ).ComplexFen.State( iState ) );

		if ( ! DetailedSolarTimestepIntegration ) {
			std::size_t lHT( 0 ); // Linear index for ( Hour, TS )
			std::size_t lHTI( 0 ); // Linear index for ( Hour, TS, I )
			for ( int Hour = 1; Hour <= 24; ++Hour ) {
				for ( int TS = 1; TS <= NumOfTimeStepInHour; ++TS, ++lHT ) { // [ lHT ] == ( Hour, TS )
					SunDir = SUNCOSTS( TS, Hour, {1,3} );
					Theta = 0.0;
					Phi = 0.0;
					if ( SUNCOSTS( TS, Hour, 3 ) > SunIsUpValue ) {
						IncRay = FindInBasis( SunDir, Front_Incident, iSurf, iState, complexWindowGeom.Inc, Theta, Phi );
						complexWindowGeom.ThetaBm[ lHT ] = Theta;
						complexWindowGeom.PhiBm[ lHT ] = Phi;
					} else {
						complexWindowGeom.ThetaBm[ lHT ] = 0.0;
						complexWindowGeom.PhiBm[ lHT ] = 0.0;
						IncRay = 0; // sundown can't have ray incident on window
					}
					if ( IncRay > 0 ) { // Sun may be incident on the window
						complexWindowGeom.SolBmIndex[ lHT ] = IncRay;
					} else { // Window can't be sunlit, set front incidence ray index to zero
						complexWindowGeom.SolBmIndex[ lHT ] = 0;
					}
					for ( int I = 1, nGnd = complexWindowGeom.NGnd; I <= nGnd; ++I, ++lHTI ) { // Gnd pt loop
						TotHits = 0;
						Vector const gndPt( complexWindowGeom.GndPt( I ) );
						for ( int JSurf = 1, eSurf = TotSurfaces; JSurf <= eSurf; ++JSurf ) {
							// the following test will cycle on anything except exterior surfaces and shading surfaces
							if ( Surface( JSurf ).HeatTransSurf && Surface( JSurf ).ExtBoundCond != ExternalEnvironment ) continue;
							// skip surfaces that face away from the ground point
							if ( dot( SunDir, Surface( JSurf ).NewellSurfaceNormalVector ) >= 0.0 ) continue;
							// Looking for surfaces between GndPt and sun
							PierceSurface( JSurf, gndPt, SunDir, HitPt, hit );
							if ( hit ) {
								// Are not going into the details of whether a hit surface is transparent
								// Since this is ultimately simply weighting the transmittance, so great
								// detail is not warranted
								++TotHits;
								break;
							}
						}
						if ( TotHits > 0 ) {
							complexWindowGeom.SolBmGndWt[ lHTI ] = 0.0; // [ lHTI ] == ( Hour, TS, I )
						} else {
							complexWindowGeom.SolBmGndWt[ lHTI ] = 1.0; // [ lHTI ] == ( Hour, TS, I )
						}
					} // Gnd pt loop

					// update window beam properties
					CalculateWindowBeamProperties( iSurf, iState, complexWindow, complexWindowGeom, surfaceWindowState, Hour, TS );
				} // Timestep loop
			} // Hour loop
		} else { // detailed timestep integration
			std::size_t const lHT( complexWindowGeom.ThetaBm.index( HourOfDay, TimeStep ) ); // [ lHT ] == ( HourOfDay, TimeStep )
			SunDir = SUNCOSTS( TimeStep, HourOfDay, {1,3} );
			Theta = 0.0;
			Phi = 0.0;
			if ( SUNCOSTS( TimeStep, HourOfDay, 3 ) > SunIsUpValue ) {
				IncRay = FindInBasis( SunDir, Front_Incident, iSurf, iState, complexWindowGeom.Inc, Theta, Phi );
				complexWindowGeom.ThetaBm[ lHT ] = Theta;
				complexWindowGeom.PhiBm[ lHT ] = Phi;
			} else {
				complexWindowGeom.ThetaBm[ lHT ] = 0.0;
				complexWindowGeom.PhiBm[ lHT ] = 0.0;
				IncRay = 0; // sundown can't have ray incident on window
			}

			if ( IncRay > 0 ) { // Sun may be incident on the window
				complexWindowGeom.SolBmIndex[ lHT ] = IncRay;
			} else { // Window can't be sunlit, set front incidence ray index to zero
				complexWindowGeom.SolBmIndex[ lHT ] = 0.0;
			}
			std::size_t lHTI( complexWindowGeom.SolBmGndWt.index( HourOfDay, TimeStep, 1 ) ); // Linear index for ( HourOfDay, TimeStep, I )
			for ( int I = 1, nGnd = complexWindowGeom.NGnd; I <= nGnd; ++I, ++lHTI ) { // Gnd pt loop
				TotHits = 0;
				Vector const gndPt( complexWindowGeom.GndPt( I ) );
				for ( int JSurf = 1; JSurf <= TotSurfaces; ++JSurf ) {
					// the following test will cycle on anything except exterior surfaces and shading surfaces
					if ( Surface( JSurf ).HeatTransSurf && Surface( JSurf ).ExtBoundCond != ExternalEnvironment ) continue;
					// skip surfaces that face away from the ground point
					if ( dot( SunDir, Surface( JSurf ).NewellSurfaceNormalVector ) >= 0.0 ) continue;
					// Looking for surfaces between GndPt and sun
					PierceSurface( JSurf, gndPt, SunDir, HitPt, hit );
					if ( hit ) {
						// Are not going into the details of whether a hit surface is transparent
						// Since this is ultimately simply weighting the transmittance, so great
						// detail is not warranted
						++TotHits;
						break;
					}
				}
				if ( TotHits > 0 ) {
					complexWindowGeom.SolBmGndWt[ lHTI ] = 0.0; // [ lHTI ] == ( HourOfDay, TimeStep, I )
				} else {
					complexWindowGeom.SolBmGndWt[ lHTI ] = 1.0; // [ lHTI ] == ( HourOfDay, TimeStep, I )
				}
			} // Gnd pt loop

			// Update window beam properties
			CalculateWindowBeamProperties( iSurf, iState, complexWindow, complexWindowGeom, surfaceWindowState, HourOfDay, TimeStep );
		} // solar calculation mode, average over days or detailed

	}

	void
	CalculateWindowBeamProperties(
		int const ISurf, // Window surface number
		int const IState, // Window state number
		BSDFWindowGeomDescr const & Window, // Window Geometry
		BSDFGeomDescr const & Geom, // State Geometry
		BSDFStateDescr & State, // State Description
		int const Hour, // Hour number
		int const TS // Timestep number
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Joe Klems
		//       DATE WRITTEN   August 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates those optical properties of all the Complex Fenestrations that
		//  depend on the beam direction (hence, on hour and time step)

		// METHODOLOGY EMPLOYED:
		// Locate the bidirectional property matrices in the BSDFInput structure
		// and use them to calculate the desired average properties.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace Vectors;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// INTEGER, INTENT(IN)     ::  IWind    !Window number (in WindowList)

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int IConst; // State construction number
		int I; // general purpose index--Back surface
		int J; // general purpose index--ray
		int JRay; // ray index number
		Real64 Theta;
		Real64 Phi;
		int JSurf; // gen purpose surface no
		int BaseSurf; // base surface no
		int M; // general purpose index--ray
		int L; // general purpose index--layer
		int KBkSurf; // general purpose index--back surface
		Real64 Sum1; // general purpose sum
		Real64 Sum2; // general purpose sum
		int IBm; // index of beam ray in incoming basis
		int BkIncRay; // index of sun dir in back incidence basis
		bool RegWindFnd; // flag for regular exterior back surf window
		Array1D_int RegWinIndex; // bk surf nos of reg windows
		static int NRegWin( 0 ); // no reg windows found as back surfaces
		static int KRegWin( 0 ); // index of reg window as back surface
		Real64 Refl; // temporary reflectance
		Array1D< Real64 > Absorb; // temporary layer absorptance

		// Object Data
		Vector SunDir; // current sun direction

		IConst = SurfaceWindow( ISurf ).ComplexFen.State( IState ).Konst;

		//  Begin calculation
		//  Calculate the Transmittance from a given beam direction to a given zone surface

		IBm = Geom.SolBmIndex( Hour, TS );
		if ( IBm <= 0.0 ) { //Beam cannot be incident on window for this Hour, TS
			State.WinToSurfBmTrans( Hour, TS, {1,Window.NBkSurf} ) = 0.0;
			State.WinDirHemiTrans( Hour, TS ) = 0.0;
			State.WinDirSpecTrans( Hour, TS ) = 0.0;
			State.WinBmFtAbs( Hour, TS, {1,State.NLayers} ) = 0.0;
		} else {
			for ( I = 1; I <= Window.NBkSurf; ++I ) { //Back surface loop
				Sum1 = 0.0;
				for ( J = 1; J <= Geom.NSurfInt( I ); ++J ) { //Ray loop
					Sum1 += Geom.Trn.Lamda( Geom.SurfInt( J, I ) ) * Construct( IConst ).BSDFInput.SolFrtTrans( IBm, Geom.SurfInt( J, I ) );
				} //Ray loop
				State.WinToSurfBmTrans( Hour, TS, I ) = Sum1;
			} //Back surface loop
			//Calculate the directional-hemispherical transmittance
			Sum1 = 0.0;
			for ( J = 1; J <= Geom.Trn.NBasis; ++J ) {
				Sum1 += Geom.Trn.Lamda( J ) * Construct( IConst ).BSDFInput.SolFrtTrans( IBm, J );
			}
			State.WinDirHemiTrans( Hour, TS ) = Sum1;
			//Calculate the directional specular transmittance
			//Note:  again using assumption that Inc and Trn basis have same structure
			State.WinDirSpecTrans( Hour, TS ) = Geom.Trn.Lamda( IBm ) * Construct( IConst ).BSDFInput.SolFrtTrans( IBm, IBm );
			//Calculate the layer front absorptance for beam radiation
			for ( L = 1; L <= State.NLayers; ++L ) {
				State.WinBmFtAbs( Hour, TS, L ) = Construct( IConst ).BSDFInput.Layer( L ).FrtAbs( IBm, 1 );
			}
		}
		//Calculate,  for a given beam direction, the transmittance into the zone
		// for ground-reflected radiation (transmitted radiation assumed uniformly diffuse)

		Sum1 = 0.0;
		Sum2 = 0.0;
		for ( J = 1; J <= Geom.NGnd; ++J ) { //Incident ray loop
			JRay = Geom.GndIndex( J );
			if ( Geom.SolBmGndWt( Hour, TS, J ) > 0.0 ) {
				Sum2 += Geom.SolBmGndWt( Hour, TS, J ) * Geom.Inc.Lamda( JRay );
				for ( M = 1; M <= Geom.Trn.NBasis; ++M ) { //Outgoing ray loop
					Sum1 += Geom.SolBmGndWt( Hour, TS, J ) * Geom.Inc.Lamda( JRay ) * Geom.Trn.Lamda( M ) * Construct( IConst ).BSDFInput.SolFrtTrans( JRay, M );
				} //Outgoing ray loop
			}
		} //Indcident ray loop
		if ( Sum2 > 0.0 ) {
			State.WinBmGndTrans( Hour, TS ) = Sum1 / Sum2;
		} else {
			State.WinBmGndTrans( Hour, TS ) = 0.0; //No unshaded ground => no transmittance
		}

		//Calculate,  for a given beam direction, the layer front absorptance
		// for ground-reflected radiation

		for ( L = 1; L <= State.NLayers; ++L ) { //layer loop
			Sum1 = 0.0;
			Sum2 = 0.0;
			for ( J = 1; J <= Geom.NGnd; ++J ) { //Incident ray loop
				JRay = Geom.GndIndex( J );
				if ( Geom.SolBmGndWt( Hour, TS, J ) > 0.0 ) {
					Sum2 += Geom.SolBmGndWt( Hour, TS, J ) * Geom.Inc.Lamda( JRay );
					Sum1 += Geom.SolBmGndWt( Hour, TS, J ) * Geom.Inc.Lamda( JRay ) * Construct( IConst ).BSDFInput.Layer( L ).FrtAbs( JRay, 1 );
				}
			} //Incident ray loop
			if ( Sum2 > 0.0 ) {
				State.WinBmGndAbs( Hour, TS, L ) = Sum1 / Sum2;
			} else {
				State.WinBmGndAbs( Hour, TS, L ) = 0.0; //No unshaded ground => no absorptance
			}
		} //layer loop

		//Check the back surfaces for exterior windows
		RegWindFnd = false;
		NRegWin = 0.0;
		RegWinIndex.allocate( Window.NBkSurf );
		for ( KBkSurf = 1; KBkSurf <= Window.NBkSurf; ++KBkSurf ) {
			BaseSurf = Surface( ISurf ).BaseSurf; //ShadowComb is organized by base surface
			JSurf = ShadowComb( BaseSurf ).BackSurf( KBkSurf );
			if ( SurfaceWindow( JSurf ).WindowModelType == WindowBSDFModel ) continue;
			if ( ! ( Surface( JSurf ).Class == SurfaceClass_Window || Surface( JSurf ).Class == SurfaceClass_GlassDoor ) ) continue;
			if ( ! ( Surface( JSurf ).HeatTransSurf && Surface( JSurf ).ExtBoundCond == ExternalEnvironment && Surface( JSurf ).ExtSolar ) ) continue;
			// Back surface is an exterior window or door
			RegWindFnd = true;
			++NRegWin;
			RegWinIndex( NRegWin ) = KBkSurf;
		}
		if ( RegWindFnd ) {
			Absorb.allocate( State.NLayers );
			SunDir = SUNCOSTS( TS, Hour, {1,3} );
			BkIncRay = FindInBasis( SunDir, Back_Incident, ISurf, IState, ComplexWind( ISurf ).Geom( IState ).Trn, Theta, Phi );
			if ( BkIncRay > 0 ) {
				//Here calculate the back incidence properties for the solar ray
				//this does not say whether or not the ray can pass through the
				//back surface window and hit this one!
				Sum1 = 0.0;
				for ( J = 1; J <= Geom.Trn.NBasis; ++J ) {
					Sum1 += Geom.Trn.Lamda( J ) * Construct( IConst ).BSDFInput.SolBkRefl( BkIncRay, J );
				}
				Refl = Sum1;
				for ( L = 1; L <= State.NLayers; ++L ) {
					Absorb( L ) = Construct( IConst ).BSDFInput.Layer( L ).BkAbs( BkIncRay, 1 );
				}
			} else {
				//solar ray can't be incident on back, so set properties equal to zero
				Refl = 0.0;
				for ( L = 1; L <= State.NLayers; ++L ) {
					Absorb( L ) = 0.0;
				}
			}
			for ( KRegWin = 1; KRegWin <= NRegWin; ++KRegWin ) {
				KBkSurf = RegWinIndex( KRegWin );
				State.BkSurf( KBkSurf ).WinDHBkRefl( Hour, TS ) = Refl;
				for ( L = 1; L <= State.NLayers; ++L ) {
					State.BkSurf( KBkSurf ).WinDirBkAbs( Hour, TS, L ) = Absorb( L );
				}
			}
		}
		if ( allocated( Absorb ) ) Absorb.deallocate();
		RegWinIndex.deallocate();

	}

	void
	CalcStaticProperties()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Joe Klems
		//       DATE WRITTEN   <date_written>
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates those optical properties of all the Complex Fenestrations that
		// do not depend on the beam direction (hence, on hour and time step)

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace Vectors;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int ISurf( 0 ); // Index for sorting thru Surface array
//		static int IConst( 0 ); // Index for accessing Construct array
		static int IState( 0 ); // Index identifying the window state for a particular window
		static int IWind( 0 ); // Index identifying a window in the WindowList
		static int NumStates( 0 ); // local copy of no of states

		for ( IWind = 1; IWind <= NumComplexWind; ++IWind ) {
			ISurf = WindowList( IWind ).SurfNo;
			NumStates = WindowList( IWind ).NumStates;
			for ( IState = 1; IState <= NumStates; ++IState ) {
				// IConst = WindowStateList ( IWind , IState )%Konst
				SurfaceWindow( ISurf ).ComplexFen.State( IState ).Konst = WindowStateList( IState, IWind ).Konst;
				CalcWindowStaticProperties( ISurf, IState, ComplexWind( ISurf ), ComplexWind( ISurf ).Geom( IState ), SurfaceWindow( ISurf ).ComplexFen.State( IState ) );
			}
		}

	}

	void
	CalculateBasisLength(
		BSDFWindowInputStruct const & Input, // BSDF data input struct for this construction
		int const IConst, // Construction number of input
		int & NBasis // Calculated Basis length
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Joe Klems
		//       DATE WRITTEN   August 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the basis length for a Window6 Non-Symmetric or Axisymmetric basis
		// from the input basis matrix

		// METHODOLOGY EMPLOYED:
		// <description>

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

		if ( Input.BasisMatNcols == 1 ) {
			//Axisymmetric basis, No. rows is no. of thetas = basis length
			NBasis = Input.BasisMatNrows;
			return;
		}
		NBasis = 1;
		for ( int I = 2; I <= Input.BasisMatNrows; ++I ) {
			NBasis += std::floor( Construct( IConst ).BSDFInput.BasisMat( 2, I ) + 0.001 );
		}

	}

	void
	DetermineMaxBackSurfaces()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Joe Klems
		//       DATE WRITTEN   September 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the basis length for a Window6 Non-Symmetric or Axisymmetric basis
		// from the input basis matrix

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ZoneNum; // Zone Number
		int SurfNum; // Surface Number
		static int NumSurfInZone( 0 ); // Number of zone surfaces
		static bool ComplexFenInZone( false );

		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
			ComplexFenInZone = false;
			for ( SurfNum = Zone( ZoneNum ).SurfaceFirst; SurfNum <= Zone( ZoneNum ).SurfaceLast; ++SurfNum ) {
				if ( SurfaceWindow( SurfNum ).WindowModelType == WindowBSDFModel ) ComplexFenInZone = true;
			}
			if ( ComplexFenInZone ) {
				NumSurfInZone = Zone( ZoneNum ).SurfaceLast - Zone( ZoneNum ).SurfaceFirst + 1;
				if ( MaxBkSurf < NumSurfInZone ) MaxBkSurf = NumSurfInZone;
			}
		}

	}

	void
	ConstructBasis(
		int const IConst, // Index for accessing Construct array
		BasisStruct & Basis
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Joe Klems
		//       DATE WRITTEN  June 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Set up a basis from the matrix information pointed to in Construction by ICons

		// METHODOLOGY EMPLOYED:
		// <description>

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

		static int I( 0 ); // general purpose index
		static int J( 0 ); // general purpose index
		static int NThetas( 0 ); // Current number of theta values
		static int NumElem( 0 ); // Number of elements in current basis
		static int ElemNo( 0 ); // Current basis element number
		int MaxNPhis; // Max no of NPhis for any theta
		static Real64 Theta( 0.0 ); // Current theta value
		static Real64 Phi( 0.0 ); // Current phi value
		static Real64 DTheta( 0.0 ); // Increment for theta value (Window6 type input)
		static Real64 DPhi( 0.0 ); // Increment for phi value (Window6 type input)
		static Real64 HalfDTheta( 0.0 ); // Half-width of all theta bins except first and last (W6 input)
		static Real64 Lamda( 0.0 ); // Current 'Lamda' value (element weight)
		static Real64 SolAng( 0.0 ); // Current element solid angle
		static Real64 NextTheta( 0.0 ); // Next theta in the W6 basis after current
		static Real64 LastTheta( 0.0 ); // Previous theta in the W6 basis before current
		static Real64 LowerTheta( 0.0 ); // Lower theta boundary of the element
		static Real64 UpperTheta( 0.0 ); // Upper theta boundary of the element
		Array1D< Real64 > Thetas; // temp array holding theta values
		Array1D_int NPhis; // temp array holding number of phis for a given theta

		NThetas = Construct( IConst ).BSDFInput.BasisMatNrows; //Note here assuming row by row input
		Basis.NThetas = NThetas;
		Basis.BasisMatIndex = Construct( IConst ).BSDFInput.BasisMatIndex;
		Basis.NBasis = Construct( IConst ).BSDFInput.NBasis;
		Basis.Grid.allocate( Basis.NBasis );
		Thetas.allocate( NThetas + 1 ); //Temp array
		//By convention the Thetas array contains a final point at Pi/2 which is not a basis element
		NPhis.allocate( NThetas + 1 ); //Temp array
		Basis.Thetas.allocate( NThetas + 1 );
		Basis.NPhis.allocate( NThetas + 1 );

		Basis.Lamda.allocate( Construct( IConst ).BSDFInput.NBasis );
		Basis.SolAng.allocate( Construct( IConst ).BSDFInput.NBasis );
		if ( Construct( IConst ).BSDFInput.BasisType == BasisType_WINDOW ) {
			//   WINDOW6 Basis
			Basis.BasisType = BasisType_WINDOW;
			if ( Construct( IConst ).BSDFInput.BasisSymmetryType == BasisSymmetry_None ) {
				// No basis symmetry
				Basis.BasisSymmetryType = BasisSymmetry_None;
				Thetas( 1 ) = 0.0; //By convention, the first basis point is at the center (theta=0,phi=0)
				Thetas( NThetas + 1 ) = 0.5 * Pi; //and there is an N+1st point (not a basis element) at Pi/2
				NPhis( 1 ) = 1;
				NumElem = 1;
				for ( I = 2; I <= NThetas; ++I ) {
					Thetas( I ) = Construct( IConst ).BSDFInput.BasisMat( 1, I ) * DegToRadians;
					NPhis( I ) = std::floor( Construct( IConst ).BSDFInput.BasisMat( 2, I ) + 0.001 );
					if ( NPhis( I ) <= 0 ) ShowFatalError( "WindowComplexManager: incorrect input, no. phis must be positive." );
					NumElem += NPhis( I );
				}
				MaxNPhis = maxval( NPhis( {1,NThetas} ) );
				Basis.Phis.allocate( NThetas + 1, MaxNPhis + 1 ); //N+1st Phi point (not basis element) at 2Pi
				Basis.BasisIndex.allocate( MaxNPhis, NThetas + 1 );
				Basis.Phis = 0.0; // Initialize so undefined elements will contain zero
				Basis.BasisIndex = 0; //Initialize so undefined elements will contain zero
				if ( NumElem != Construct( IConst ).BSDFInput.NBasis ) { //Constructed Basis must match property matrices
					ShowFatalError( "WindowComplexManager: Constructed basis length does not match property matrices." );
				}
				Basis.Thetas = Thetas;
				Basis.NPhis = NPhis;
				ElemNo = 0;
				for ( I = 1; I <= NThetas; ++I ) {
					Theta = Thetas( I );
					if ( I == 1 ) { //First theta value must always be zero
						HalfDTheta = 0.5 * Thetas( I + 1 );
						LastTheta = 0.0;
						NextTheta = Thetas( I + 1 );
						LowerTheta = 0.0;
						UpperTheta = HalfDTheta;
					} else if ( I > 1 && I < NThetas ) {
						LastTheta = Thetas( I - 1 );
						NextTheta = Thetas( I + 1 );
						LowerTheta = UpperTheta;
						HalfDTheta = Theta - LowerTheta;
						UpperTheta = Theta + HalfDTheta;
					} else if ( I == NThetas ) {
						LastTheta = Thetas( I - 1 );
						NextTheta = 0.5 * Pi;
						LowerTheta = UpperTheta; //It is assumed that Thetas(N) is the mean between the previous
						//UpperTheta and pi/2.
						UpperTheta = 0.5 * Pi;
					}
					DPhi = 2.0 * Pi / NPhis( I );
					if ( I == 1 ) {
						Lamda = Pi * pow_2( std::sin( UpperTheta ) );
						SolAng = 2.0 * Pi * ( 1.0 - std::cos( UpperTheta ) );
					} else {
						Lamda = 0.5 * DPhi * ( pow_2( std::sin( UpperTheta ) ) - pow_2( std::sin( LowerTheta ) ) ); //For W6 basis, lamda is funct of Theta and
						// NPhis, not individual Phi
						SolAng = DPhi * ( std::cos( LowerTheta ) - std::cos( UpperTheta ) );
					}
					DTheta = UpperTheta - LowerTheta;
					Basis.Phis( I, NPhis( I ) + 1 ) = 2.0 * Pi; //Non-basis-element Phi point for table searching in Phi
					for ( J = 1; J <= NPhis( I ); ++J ) {
						++ElemNo;
						Basis.BasisIndex( J, I ) = ElemNo;
						Phi = ( J - 1 ) * DPhi;
						Basis.Phis( I, J ) = Phi; //Note: this ordering of I & J are necessary to allow Phis(Theta) to
						//  be searched as a one-dimensional table
						FillBasisElement( Theta, Phi, ElemNo, Basis.Grid( ElemNo ), LowerTheta, UpperTheta, DPhi, BasisType_WINDOW ); //This gets all the simple grid characteristics
						Basis.Lamda( ElemNo ) = Lamda;
						Basis.SolAng( ElemNo ) = SolAng;
					}
				}
			} else { // BST
				//  Axisymmetric basis symmetry (Note this only useful specular systems, where it allows shorter data input)
				Basis.BasisSymmetryType = BasisSymmetry_Axisymmetric;
				Thetas( 1 ) = 0.0; //By convention, the first basis point is at the center (theta=0,phi=0)
				Thetas( NThetas + 1 ) = 0.5 * Pi; //and there is an N+1st point (not a basis element) at Pi/2
				NPhis = 1; //As insurance, define one phi for each theta
				NumElem = 1;
				for ( I = 2; I <= NThetas; ++I ) {
					Thetas( I ) = Construct( IConst ).BSDFInput.BasisMat( 1, I ) * DegToRadians;
					++NumElem;
				}
				Basis.Phis.allocate( 1, NThetas );
				Basis.BasisIndex.allocate( 1, NThetas );
				Basis.Phis = 0.0; // Initialize so undefined elements will contain zero
				Basis.BasisIndex = 0; //Initialize so undefined elements will contain zero
				if ( NumElem != Construct( IConst ).BSDFInput.NBasis ) { //Constructed Basis must match property matrices
					ShowFatalError( "WindowComplexManager: Constructed basis length does not match property matrices." );
				}
				Basis.Thetas = Thetas;
				Basis.NPhis = NPhis;
				ElemNo = 0;
				DPhi = 2.0 * Pi;
				for ( I = 1; I <= NThetas; ++I ) {
					Theta = Thetas( I );
					if ( I == 1 ) { //First theta value must always be zero
						HalfDTheta = 0.5 * Thetas( I + 1 );
						LastTheta = 0.0;
						NextTheta = Thetas( I + 1 );
						LowerTheta = 0.0;
						UpperTheta = HalfDTheta;
					} else if ( I > 1 && I < NThetas ) {
						LastTheta = Thetas( I - 1 );
						NextTheta = Thetas( I + 1 );
						LowerTheta = UpperTheta;
						HalfDTheta = Theta - LowerTheta;
						UpperTheta = Theta + HalfDTheta;
					} else if ( I == NThetas ) {
						LastTheta = Thetas( I - 1 );
						NextTheta = 0.5 * Pi;
						LowerTheta = UpperTheta; //It is assumed that Thetas(N) is the mean between the previous
						//UpperTheta and pi/2.
						UpperTheta = 0.5 * Pi;
					}
					if ( I == 1 ) {
						Lamda = Pi * pow_2( std::sin( UpperTheta ) );
						SolAng = 2.0 * Pi * ( 1.0 - std::cos( UpperTheta ) );
					} else {
						Lamda = 0.5 * DPhi * ( pow_2( std::sin( UpperTheta ) ) - pow_2( std::sin( LowerTheta ) ) ); //For W6 basis, lamda is funct of Theta and
						// NPhis, not individual Phi
						SolAng = DPhi * ( std::cos( LowerTheta ) - std::cos( UpperTheta ) );
					}
					DTheta = UpperTheta - LowerTheta;
					++ElemNo;
					Basis.BasisIndex( 1, I ) = ElemNo;
					Phi = 0.0;
					Basis.Phis( I, 1 ) = Phi; //Note: this ordering of I & J are necessary to allow Phis(Theta) to
					//  be searched as a one-dimensional table
					FillBasisElement( Theta, Phi, ElemNo, Basis.Grid( ElemNo ), LowerTheta, UpperTheta, DPhi, BasisType_WINDOW ); //This gets all the simple grid characteristics
					Basis.Lamda( ElemNo ) = Lamda;
					Basis.SolAng( ElemNo ) = SolAng;
				}
			} // BST
		} else { // BTW
			ShowFatalError( "WindowComplexManager: Non-Window6 basis type not yet implemented." );
		} // BTW
		Thetas.deallocate();
		NPhis.deallocate();

	}

	void
	FillBasisElement(
		Real64 const Theta, // Central polar angle of element
		Real64 const Phi, // Central azimuthal angle of element
		int const Elem, // Index number of element in basis
		BasisElemDescr & BasisElem,
		Real64 const LowerTheta, // Lower edge of element (polar angle)
		Real64 const UpperTheta, // Upper edge of element (polar angle)
		Real64 const DPhi, // Width of element (azimuthal angle)
		int const InputType // Basis type
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Joe Klems
		//       DATE WRITTEN   August 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// fill in values for all the components of a basis element

		// METHODOLOGY EMPLOYED:
		// <n/a>

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

		if ( InputType == BasisType_WINDOW ) {
			//WINDOW6 Type BASIS
			if ( Elem == 1 ) {
				//first element, theta=0, is special case
				BasisElem.Theta = Theta;
				BasisElem.Phi = 0.0;
				BasisElem.dPhi = 2.0 * Pi;
				BasisElem.UpprTheta = UpperTheta;
				BasisElem.dTheta = BasisElem.UpprTheta - Theta;
				BasisElem.LwrTheta = Theta;
				BasisElem.LwrPhi = 0.0;
				BasisElem.UpprPhi = 2.0 * Pi;
			} else {
				BasisElem.Theta = Theta;
				BasisElem.Phi = Phi;
				BasisElem.dPhi = DPhi;
				BasisElem.LwrPhi = Phi - DPhi / 2.0;
				BasisElem.UpprPhi = Phi + DPhi / 2.0;
				BasisElem.LwrTheta = LowerTheta;
				BasisElem.UpprTheta = UpperTheta;
				BasisElem.dTheta = BasisElem.UpprTheta - BasisElem.LwrTheta;
			}
		} else {
			//Non-WINDOW6 Type Basis
			//Currently not implemented
			ShowFatalError( "WindowComplexManager: Custom basis type not yet implemented." );
		}

	}

	void
	SetupComplexWindowStateGeometry(
		int const ISurf, // Surface number of the complex fenestration
		int const IState, // State number of the complex fenestration state
		int const IConst, // Pointer to construction for this state
		BSDFWindowGeomDescr & Window, // Window Geometry
		BSDFGeomDescr & Geom, // State Geometry
		BSDFStateDescr & EP_UNUSED( State ) // State Description
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         J. Klems
		//       DATE WRITTEN   June 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Define all the geometric quantites for a complex fenestration state

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace Vectors;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//INTEGER, INTENT(IN)      ::  IWind            !Complex fenestration number (in window list)

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 Azimuth; // Complex fenestration azimuth
		Real64 Tilt; // Complex fenestration tilt
		int ElemNo; // Grid index variable
		bool hit; // Surface intersection flag
		int I; // Temp Indices
		int J;
		int IRay; // Ray index variable
		int IZone; // Zone containing the complex window
		int JSurf; // Secondary Surface index
		int BaseSurf; // base surface index
		int KBkSurf; // Back surface index
		int MaxHits; // Max no of hits found
		int MaxInt; // Max no of intersections found
		int NSky; // No of sky rays
		int NGnd; // No of gnd rays
		int NReflSurf; // No of rays striking ext surfaces
		int NBkSurf; // No of back surfaces
		int TotHits; // Current number of surface intersections
		Real64 Theta; // Basis theta angle
		Real64 Phi; // Basis phi angle
		Real64 HitDsq; // Squared distance to current hit pt
		Real64 LeastHitDsq; // Squared distance to closest hit pt
		Array1D< Real64 > V( 3 ); // vector array
		Array1D_int TmpRfSfInd; // Temporary RefSurfIndex
		Array1D_int TmpRfRyNH; // Temporary RefRayNHits
		Array2D_int TmpHSurfNo; // Temporary HitSurfNo
		Array2D< Real64 > TmpHSurfDSq; // Temporary HitSurfDSq
		Array1D_int TmpSkyInd; // Temporary sky index list
		Array1D_int TmpGndInd; // Temporary gnd index list
		Array2D_int TmpSurfInt; // Temporary index of ray intersecing back surf
		Array2D< Real64 > TmpSjdotN; // Temporary dot prod of ray angle w bk surf norm
		Array1D_int ITemp1D; // Temporary INT 1D array
		Array2D< Real64 > Temp2D; // Temporary real 2D array
		Real64 TransRSurf; // Norminal transmittance of shading surface
		Real64 WtSum; // Sum for normalizing various weights
		Real64 DotProd; // Temporary variable for manipulating dot product .dot.

		struct BackHitList
		{
			// Members
			int KBkSurf; // Back surface index of the hit surface
			int HitSurf; // Surface number of the hit surface
			Vector HitPt; // coords of hit pt (world syst)
			Real64 HitDsq; // Squared distance to the current hit pt

			// Default Constructor
			BackHitList() :
			KBkSurf( 0 ),
			HitSurf( 0 ),
			HitDsq( 0.0 )
			{}

		};

		// Object Data
		Vector HitPt; // coords of hit pt (world syst)
		Vector X; // position vector
		Vector VecNorm; // outer normal vector
		Array1D< Vector > TmpGndPt; // Temporary ground intersection list
		Array2D< Vector > TempV2D; // Temporary vector 2D array
		Array2D< Vector > TmpHitPt; // Temporary HitPt
		BackHitList BSHit; // Temp list of back surface hit quantities for a ray

		//This routine primarily fills in the BSDFGeomDescr type for a given window and state
		//Note that on call the incoming and outgoing basis structure types have already been filled in
		//  Define the central ray directions (in world coordinate system)

		SurfaceWindow( ISurf ).ComplexFen.State( IState ).NLayers = Construct( IConst ).BSDFInput.NumLayers;
		Azimuth = DegToRadians * Surface( ISurf ).Azimuth;
		Tilt = DegToRadians * Surface( ISurf ).Tilt;

		//For incoming grid

		Geom.sInc.allocate( Geom.Inc.NBasis );
		Geom.sInc = Vector( 0.0, 0.0, 0.0 );
		Geom.pInc.allocate( Geom.Inc.NBasis );
		Geom.CosInc.allocate( Geom.Inc.NBasis );
		Geom.DAInc.allocate( Geom.Inc.NBasis );
		Geom.pInc = BSDFDaylghtPosition( 0.0, 0.0 );
		for ( ElemNo = 1; ElemNo <= Geom.Inc.NBasis; ++ElemNo ) {
			Theta = Geom.Inc.Grid( ElemNo ).Theta;
			Phi = Geom.Inc.Grid( ElemNo ).Phi;
			//The following puts in the vectors depending on
			// window orientation
			Geom.sInc( ElemNo ) = WorldVectFromW6( Theta, Phi, Front_Incident, Tilt, Azimuth );
			Geom.pInc( ElemNo ) = DaylghtAltAndAzimuth( Geom.sInc( ElemNo ) );

			Geom.CosInc( ElemNo ) = std::cos( Geom.Inc.Grid( ElemNo ).Theta );
			//Geom%DAInc(ElemNo) = COS(Geom%pInc(ElemNo)%Altitude) * Geom%Inc%Grid(ElemNo)%dTheta * Geom%Inc%Grid(ElemNo)%dPhi
			// Geom%DAInc(ElemNo) = Geom%Inc%Grid(ElemNo)%dTheta * Geom%Inc%Grid(ElemNo)%dPhi
			Geom.DAInc( ElemNo ) = std::cos( Geom.Inc.Grid( ElemNo ).Theta ) * Geom.Inc.Grid( ElemNo ).dTheta * Geom.Inc.Grid( ElemNo ).dPhi;
		}
		//  For outgoing grid
		Geom.sTrn.allocate( Geom.Trn.NBasis );
		Geom.sTrn = Vector( 0.0, 0.0, 0.0 );
		Geom.pTrn.allocate( Geom.Trn.NBasis );
		Geom.pTrn = BSDFDaylghtPosition( 0.0, 0.0 );
		for ( ElemNo = 1; ElemNo <= Geom.Trn.NBasis; ++ElemNo ) {
			Theta = Geom.Trn.Grid( ElemNo ).Theta;
			Phi = Geom.Trn.Grid( ElemNo ).Phi;
			//The following puts in the vectors depending on
			// window orientation
			Geom.sTrn( ElemNo ) = WorldVectFromW6( Theta, Phi, Front_Transmitted, Tilt, Azimuth );
			Geom.pTrn( ElemNo ) = DaylghtAltAndAzimuth( Geom.sTrn( ElemNo ) );
		}
		//  Incident Basis:
		//  Construct sky and ground ray index maps, and list of rays intersecting exterior surfaces
		//Sky, and ground ray index maps, and rays that are potentially beam radiation reflected from exterior surfaces
		TmpRfSfInd.allocate( Geom.Inc.NBasis );
		TmpRfRyNH.allocate( Geom.Inc.NBasis );
		TmpHSurfNo.allocate( TotSurfaces, Geom.Inc.NBasis );
		TmpHSurfDSq.allocate( TotSurfaces, Geom.Inc.NBasis );
		TmpHitPt.allocate( TotSurfaces, Geom.Inc.NBasis );
		TmpSkyInd.allocate( Geom.Inc.NBasis );
		TmpGndInd.allocate( Geom.Inc.NBasis );
		TmpGndPt.allocate( Geom.Inc.NBasis );
		NSky = 0;
		NGnd = 0;
		NReflSurf = 0;
		TmpRfRyNH = 0;
		Geom.NSkyUnobs = 0;
		Geom.NGndUnobs = 0;
		//  Note--this loop could be repeated for different positions in the window plane (as for detailed reflection
		//  calculations, varying the origin in the call to PierceSurface.  Essentially, have set NsubV =1.
		for ( IRay = 1; IRay <= Geom.Inc.NBasis; ++IRay ) {
			if ( Geom.sInc( IRay ).z < 0.0 ) {
				// A ground ray
				++Geom.NGndUnobs;
			} else {
				// A sky ray
				++Geom.NSkyUnobs;
			}
			// Exterior reveal shadowing/reflection treatment should be inserted here
			TotHits = 0;
			for ( JSurf = 1; JSurf <= TotSurfaces; ++JSurf ) {
				// the following test will cycle on anything except exterior surfaces and shading surfaces
				if ( Surface( JSurf ).HeatTransSurf && Surface( JSurf ).ExtBoundCond != ExternalEnvironment ) continue;
				//  skip the base surface containing the window and any other subsurfaces of that surface
				if ( JSurf == Surface( ISurf ).BaseSurf || Surface( JSurf ).BaseSurf == Surface( ISurf ).BaseSurf ) continue;
				//  skip surfaces that face away from the window
				DotProd = dot( Geom.sInc( IRay ), Surface( JSurf ).NewellSurfaceNormalVector );
				if ( DotProd >= 0.0 ) continue;
				PierceSurface( JSurf, Surface( ISurf ).Centroid, Geom.sInc( IRay ), HitPt, hit );
				if ( !hit ) continue; // Miss: Try next surface
				if ( TotHits == 0 ) {
					//  First hit for this ray
					TotHits = 1;
					++NReflSurf;
					TmpRfSfInd( NReflSurf ) = IRay;
					TmpRfRyNH( NReflSurf ) = 1;
					TmpHSurfNo( 1, NReflSurf ) = JSurf;
					TmpHitPt( 1, NReflSurf ) = HitPt;
					V = HitPt - Surface( ISurf ).Centroid; //vector array from window ctr to hit pt
					LeastHitDsq = magnitude_squared( V ); //dist^2 window ctr to hit pt
					TmpHSurfDSq( 1, NReflSurf ) = LeastHitDsq;
					if ( ! Surface( JSurf ).HeatTransSurf && Surface( JSurf ).SchedShadowSurfIndex != 0 ) {
						TransRSurf = 1.0; //If a shadowing surface may have a scheduled transmittance,
						//   treat it here as completely transparent
					} else {
						TransRSurf = 0.0;
					}
				} else {
					V = HitPt - Surface( ISurf ).Centroid;
					HitDsq = magnitude_squared( V );
					if ( HitDsq >= LeastHitDsq ) {
						if ( TransRSurf > 0.0 ) { //forget the new hit if the closer hit is opaque
							J = TotHits + 1;
							if ( TotHits > 1 ) {
								for ( I = 2; I <= TotHits; ++I ) {
									if ( HitDsq < TmpHSurfDSq( I, NReflSurf ) ) {
										J = I;
										break;
									}
								}
								if ( ! Surface( JSurf ).HeatTransSurf && Surface( JSurf ).SchedShadowSurfIndex == 0 ) {
									//  The new hit is opaque, so we can drop all the hits further away
									TmpHSurfNo( J, NReflSurf ) = JSurf;
									TmpHitPt( J, NReflSurf ) = HitPt;
									TmpHSurfDSq( J, NReflSurf ) = HitDsq;
									TotHits = J;
								} else {
									//  The new hit is scheduled (presumed transparent), so keep the more distant hits
									//     Note that all the hists in the list will be transparent except the last,
									//       which may be either transparent or opaque
									if ( TotHits >= J ) {
										for ( I = TotHits; I >= J; --I ) {
											TmpHSurfNo( I + 1, NReflSurf ) = TmpHSurfNo( I, NReflSurf );
											TmpHitPt( I + 1, NReflSurf ) = TmpHitPt( I, NReflSurf );
											TmpHSurfDSq( I + 1, NReflSurf ) = TmpHSurfDSq( I, NReflSurf );
										}
										TmpHSurfNo( J, NReflSurf ) = JSurf;
										TmpHitPt( J, NReflSurf ) = HitPt;
										TmpHSurfDSq( J, NReflSurf ) = HitDsq;
										++TotHits;
									}
								}
							}
						}
					} else {
						//  A new closest hit.  If it is opaque, drop the current hit list,
						//    otherwise add it at the front
						LeastHitDsq = HitDsq;
						if ( ! Surface( JSurf ).HeatTransSurf && Surface( JSurf ).SchedShadowSurfIndex != 0 ) {
							TransRSurf = 1.0; // New closest hit is transparent, keep the existing hit list
							for ( I = TotHits; I >= 1; --I ) {
								TmpHSurfNo( I + 1, NReflSurf ) = TmpHSurfNo( I, NReflSurf );
								TmpHitPt( I + 1, NReflSurf ) = TmpHitPt( I, NReflSurf );
								TmpHSurfDSq( I + 1, NReflSurf ) = TmpHSurfDSq( I, NReflSurf );
								++TotHits;
							}
						} else {
							TransRSurf = 0.0; //New closest hit is opaque, drop the existing hit list
							TotHits = 1;
						}
						TmpHSurfNo( 1, NReflSurf ) = JSurf; // In either case the new hit is put in position 1
						TmpHitPt( 1, NReflSurf ) = HitPt;
						TmpHSurfDSq( 1, NReflSurf ) = LeastHitDsq;
					}
				}
			} //End of loop over surfaces
			if ( TotHits <= 0 ) {
				//This ray reached the sky or ground unobstructed
				if ( Geom.sInc( IRay ).z < 0.0 ) {
					//A ground ray
					++NGnd;
					TmpGndInd( NGnd ) = IRay;
					TmpGndPt( NGnd ).x = Surface( ISurf ).Centroid.x - ( Geom.sInc( IRay ).x / Geom.sInc( IRay ).z ) * Surface( ISurf ).Centroid.z;
					TmpGndPt( NGnd ).y = Surface( ISurf ).Centroid.y - ( Geom.sInc( IRay ).y / Geom.sInc( IRay ).z ) * Surface( ISurf ).Centroid.z;
					TmpGndPt( NGnd ).z = 0.0;
				} else {
					//A sky ray
					++NSky;
					TmpSkyInd( NSky ) = IRay;
				}
			} else {
				//Save the number of hits for this ray
				TmpRfRyNH( NReflSurf ) = TotHits;
			}
		} //End of loop over basis rays
		//Store results of indexing the incident basis for this window
		Geom.NSky = NSky;
		Geom.NGnd = NGnd;
		Geom.NReflSurf = NReflSurf;
		Geom.SkyIndex.allocate( NSky );
		Geom.SkyIndex = TmpSkyInd( {1,NSky} );
		TmpSkyInd.deallocate();
		Geom.GndIndex.allocate( NGnd );
		Geom.GndPt.allocate( NGnd );
		Geom.GndIndex = TmpGndInd( {1,NGnd} );
		Geom.GndPt = TmpGndPt( {1,NGnd} );
		TmpGndInd.deallocate();
		TmpGndPt.deallocate();
		MaxHits = maxval( TmpRfRyNH );
		Geom.RefSurfIndex.allocate( NReflSurf );
		Geom.RefRayNHits.allocate( NReflSurf );
		Geom.HitSurfNo.allocate( MaxHits, NReflSurf );
		Geom.HitSurfDSq.allocate( MaxHits, NReflSurf );
		Geom.HitPt.allocate( MaxHits, NReflSurf );
		Geom.RefSurfIndex = TmpRfSfInd( {1,NReflSurf} );
		Geom.RefRayNHits = TmpRfRyNH( {1,NReflSurf} );
		Geom.HitSurfNo = 0;
		Geom.HitSurfDSq = 0.0;
		Geom.HitPt = Vector( 0.0, 0.0, 0.0 );
		for ( I = 1; I <= NReflSurf; ++I ) {
			TotHits = TmpRfRyNH( I );
			Geom.HitSurfNo( {1,TotHits}, I ) = TmpHSurfNo( {1,TotHits}, I );
			Geom.HitSurfDSq( {1,TotHits}, I ) = TmpHSurfDSq( {1,TotHits}, I );
			Geom.HitPt( {1,TotHits}, I ) = TmpHitPt( {1,TotHits}, I );
		}
		TmpRfRyNH.deallocate();
		TmpRfSfInd.deallocate();
		TmpHSurfNo.deallocate();
		TmpHSurfDSq.deallocate();
		TmpHitPt.deallocate();
		//In above scheme sky and ground rays are those that intesect no exterior surfaces.
		//  The list of hit points is compiled for later (future?) calculation
		//  of reflections from these surfaces.  The hit list for each ray includes all
		//   surfaces with schedulable transmittance intersected by the ray,
		//   in order of increasing distance, up to the first opaque surface.
		//  Rays that intesect one or more schedulable transmittance but no opaque
		//  surfaces (therefore may reach the sky or ground) are left out of the sky/ground
		//  calcuation.  A correction for these rays could/should be made after the
		//  shading calculation.
		// Now calculate weights for averaging the transmittance matrix
		//Sky Weights
		Geom.SolSkyWt.allocate( NSky );
		for ( I = 1; I <= NSky; ++I ) {
			J = Geom.SkyIndex( I );
			Geom.SolSkyWt( I ) = SkyWeight( Geom.sInc( J ) );
		}
		WtSum = sum( Geom.SolSkyWt( {1,NSky} ) );
		Geom.SolSkyWt( {1,NSky} ) /= WtSum;
		//SkyGround Weights
		Geom.SolSkyGndWt.allocate( NGnd );
		for ( I = 1; I <= NGnd; ++I ) {
			Geom.SolSkyGndWt( I ) = SkyGndWeight( Geom.GndPt( I ) );
		}
		WtSum = sum( Geom.SolSkyGndWt( {1,NGnd} ) );
		Geom.SolSkyGndWt( {1,NGnd} ) /= WtSum;
		//  Weights for beam reflected from ground are calculated after shading
		//  interval is determined
		//Transmitted Basis:
		//  Construct back surface intersection maps
		IZone = Surface( ISurf ).Zone;
		NBkSurf = Window.NBkSurf;
		Geom.NSurfInt.allocate( NBkSurf );
		Geom.NSurfInt = 0; //Initialize the number of intersections to zero
		TmpSurfInt.allocate( Geom.Trn.NBasis, NBkSurf );
		TmpSjdotN.allocate( Geom.Trn.NBasis, NBkSurf );
		//Find the intersections of the basis rays with the back surfaces
		for ( IRay = 1; IRay <= Geom.Trn.NBasis; ++IRay ) { //ray loop
			TotHits = 0;
			//  Insert treatment of intersection & reflection from interior reveals here
			for ( KBkSurf = 1; KBkSurf <= NBkSurf; ++KBkSurf ) { //back surf loop
				BaseSurf = Surface( ISurf ).BaseSurf; //ShadowComb is organized by base surface
				JSurf = ShadowComb( BaseSurf ).BackSurf( KBkSurf ); //these are all proper back surfaces
				PierceSurface( JSurf, Surface( ISurf ).Centroid, Geom.sTrn( IRay ), HitPt, hit );
				if ( !hit ) continue; // Miss: Try next surface
				if ( TotHits == 0 ) {
					//  First hit for this ray
					TotHits = 1;
					BSHit.KBkSurf = KBkSurf;
					BSHit.HitSurf = JSurf;
					BSHit.HitPt = HitPt;
					V = HitPt - Surface( ISurf ).Centroid;
					BSHit.HitDsq = magnitude_squared( V );
				} else if ( BSHit.HitSurf == Surface( JSurf ).BaseSurf ) {
					//  another hit, check whether this is a subsurface of a previously hit base surface
					//  (which would be listed first in the Surface array)
					//  if so, replace the previous hit with this one
					++TotHits;
					BSHit.KBkSurf = KBkSurf;
					BSHit.HitSurf = JSurf;
					BSHit.HitPt = HitPt;
					V = HitPt - Surface( ISurf ).Centroid;
					BSHit.HitDsq = magnitude_squared( V );
				} else {
					++TotHits;
					// is the new hit closer than the previous one (i.e., zone not strictly convex)?
					// if so, take the closer hit
					V = HitPt - Surface( ISurf ).Centroid;
					HitDsq = magnitude_squared( V );
					if ( HitDsq < BSHit.HitDsq ) {
						BSHit.KBkSurf = KBkSurf;
						BSHit.HitSurf = JSurf;
						BSHit.HitPt = HitPt;
						BSHit.HitDsq = HitDsq;
					}
				}
			} //back surf loop
			if ( TotHits == 0 ) { //this should not happen--means a ray has gotten lost
				//    CALL ShowWarningError('BSDF--Zone surfaces do not completely enclose zone--transmitted ray lost')
			} else {
				KBkSurf = BSHit.KBkSurf;
				JSurf = BSHit.HitSurf;
				++Geom.NSurfInt( KBkSurf );
				TmpSurfInt( Geom.NSurfInt( KBkSurf ), KBkSurf ) = IRay;
				VecNorm = Surface( JSurf ).OutNormVec;
				TmpSjdotN( Geom.NSurfInt( KBkSurf ), KBkSurf ) = dot( Geom.sTrn( IRay ), VecNorm );
			}
		} //ray loop
		//  All rays traced, now put away the results in the temporary arrays
		MaxInt = maxval( Geom.NSurfInt );
		Geom.SurfInt.allocate( MaxInt, Window.NBkSurf );
		Geom.SjdotN.allocate( MaxInt, Window.NBkSurf );
		Geom.SurfInt = 0;
		for ( I = 1; I <= Window.NBkSurf; ++I ) {
			Geom.SurfInt( {1,Geom.NSurfInt( I )}, I ) = TmpSurfInt( {1,Geom.NSurfInt( I )}, I );
			Geom.SjdotN( {1,Geom.NSurfInt( I )}, I ) = TmpSjdotN( {1,Geom.NSurfInt( I )}, I );
		}

		TmpSurfInt.deallocate();
		TmpSjdotN.deallocate();

	}

	void
	CalcWindowStaticProperties(
		int const ISurf, // Surface number of the complex fenestration
		int const IState, // State number of the complex fenestration state
		BSDFWindowGeomDescr & Window, // Window Geometry
		BSDFGeomDescr & Geom, // State Geometry
		BSDFStateDescr & State // State Description
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Joe Klems
		//       DATE WRITTEN   <date_written>
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates those optical properties of all the Complex Fenestrations that
		// do not depend on the beam direction (hence, on hour and time step)

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace Vectors;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int IConst; // Pointer to construction for this fenestration
		static int I( 0 ); // general purpose index
		static int J( 0 ); // general purpose index
		static int JJ( 0 ); // general purpose index--ray
		static int L( 0 ); // general purpose index--layer
		static int M( 0 ); // general purpose index--ray
		int KBkSurf; // back surface index
		int JSurf; // surface number (used for back surface)
		int BaseSurf; // base surface number (used for finding back surface)
		Real64 Sum1; // general purpose temporary sum
		Real64 Sum2; // general purpose temporary sum
		Real64 Sum3; // general purpose temporary sum
		Real64 Hold; // temp variable

		IConst = SurfaceWindow( ISurf ).ComplexFen.State( IState ).Konst;

		//Calculate the hemispherical-hemispherical transmittance

		Sum1 = 0.0;
		Sum2 = 0.0;
		for ( J = 1; J <= Geom.Inc.NBasis; ++J ) { //Incident ray loop
			Sum2 += Geom.Inc.Lamda( J );
			for ( M = 1; M <= Geom.Trn.NBasis; ++M ) { //Outgoing ray loop
				Sum1 += Geom.Inc.Lamda( J ) * Geom.Trn.Lamda( M ) * Construct( IConst ).BSDFInput.SolFrtTrans( M, J );
			} //Outgoing ray loop
		} //Incident ray loop
		if ( Sum2 > 0 ) {
			State.WinDiffTrans = Sum1 / Sum2;
		} else {
			State.WinDiffTrans = 0.0;
			ShowWarningError( "BSDF--Inc basis has zero projected solid angle" );
		}

		//Calculate the hemispherical-hemispherical transmittance for visible spetrum

		Sum1 = 0.0;
		Sum2 = 0.0;
		for ( J = 1; J <= Geom.Inc.NBasis; ++J ) { //Incident ray loop
			Sum2 += Geom.Inc.Lamda( J );
			for ( M = 1; M <= Geom.Trn.NBasis; ++M ) { //Outgoing ray loop
				Sum1 += Geom.Inc.Lamda( J ) * Geom.Trn.Lamda( M ) * Construct( IConst ).BSDFInput.VisFrtTrans( M, J );
			} //Outgoing ray loop
		} //Incident ray loop
		if ( Sum2 > 0.0 ) {
			State.WinDiffVisTrans = Sum1 / Sum2;
		} else {
			State.WinDiffVisTrans = 0.0;
			ShowWarningError( "BSDF--Inc basis has zero projected solid angle" );
		}

		//Set the nominal diffuse transmittance so the surface isn't mistaken as opaque
		Construct( IConst ).TransDiff = SurfaceWindow( ISurf ).ComplexFen.State( IState ).WinDiffTrans;
		//Calculate Window Sky Transmittance (transmitted radiation assumed diffuse)
		//and Sky Absorptance (by layer)
		Sum1 = 0.0;
		Sum2 = 0.0;
		Sum3 = 0.0;
		for ( JJ = 1; JJ <= Geom.NSky; ++JJ ) {
			for ( M = 1; M <= Geom.Trn.NBasis; ++M ) {
				J = Geom.SkyIndex( JJ );
				Sum1 += Geom.SolSkyWt( JJ ) * Construct( IConst ).BSDFInput.SolFrtTrans( M, J ) * Geom.Inc.Lamda( J ) * Geom.Trn.Lamda( M );
			}
		}
		for ( JJ = 1; JJ <= Geom.NSky; ++JJ ) {
			J = Geom.SkyIndex( JJ );
			Sum2 += Geom.SolSkyWt( JJ ) * Geom.Inc.Lamda( J );
		}

		if ( Sum2 != 0.0 ) {
			State.WinSkyTrans = Sum1 / Sum2;
		} else {
			State.WinSkyTrans = 0.0;
		}

		State.WinSkyFtAbs.allocate( State.NLayers );
		//Also allocate the beam quantities for this state
		for ( L = 1; L <= State.NLayers; ++L ) {
			Sum3 = 0.0;
			for ( JJ = 1; JJ <= Geom.NSky; ++JJ ) {
				J = Geom.SkyIndex( JJ );
				Sum3 += Geom.SolSkyWt( JJ ) * Geom.Inc.Lamda( J ) * Construct( IConst ).BSDFInput.Layer( L ).FrtAbs( J, 1 );
			}

			if ( Sum2 != 0.0 ) {
				State.WinSkyFtAbs( L ) = Sum3 / Sum2;
			} else {
				State.WinSkyFtAbs( L ) = 0.0;
			}

		}

		//Calculate Window Sky/Ground Transmittance
		//(applies to ground-reflected sky radiation, transmitted radiation assumed diffuse)
		//This is the same calculation as the sky transmittance, except that the set of incident
		//rays and the ray weights are different
		//Also calculate Window Sky/Ground Absorptance (by layer)
		Sum1 = 0.0;
		Sum2 = 0.0;
		Sum3 = 0.0;

		for ( JJ = 1; JJ <= Geom.NGnd; ++JJ ) {
			for ( M = 1; M <= Geom.Trn.NBasis; ++M ) {
				J = Geom.GndIndex( JJ );
				Sum1 += Geom.SolSkyGndWt( JJ ) * Construct( IConst ).BSDFInput.SolFrtTrans( M, J ) * Geom.Inc.Lamda( J ) * Geom.Trn.Lamda( M );
			}
		}

		for ( JJ = 1; JJ <= Geom.NGnd; ++JJ ) {
			J = Geom.GndIndex( JJ );
			Sum2 += Geom.SolSkyGndWt( JJ ) * Geom.Inc.Lamda( J );
		}

		if ( Sum2 != 0.0 ) {
			State.WinSkyGndTrans = Sum1 / Sum2;
		} else {
			State.WinSkyGndTrans = 0.0;
		}

		State.WinSkyGndAbs.allocate( State.NLayers );
		for ( L = 1; L <= State.NLayers; ++L ) {
			Sum3 = 0.0;
			for ( JJ = 1; JJ <= Geom.NGnd; ++JJ ) {
				J = Geom.GndIndex( JJ );
				Sum3 += Geom.SolSkyGndWt( JJ ) * Geom.Inc.Lamda( J ) * Construct( IConst ).BSDFInput.Layer( L ).FrtAbs( J, 1 );
			}

			if ( Sum2 != 0.0 ) {
				State.WinSkyGndAbs( L ) = Sum3 / Sum2;
			} else {
				State.WinSkyGndAbs( L ) = 0.0;
			}
		}

		//Calculate Window Back Hemispherical Reflectance and Layer Back Hemispherical Absorptance
		Sum1 = 0.0;
		Sum2 = 0.0;
		Sum3 = 0.0;
		//Note this again assumes the equivalence Inc basis = transmission basis for back incidence and
		// Trn basis = incident basis for back incidence
		for ( J = 1; J <= Geom.Trn.NBasis; ++J ) {
			for ( M = 1; M <= Geom.Inc.NBasis; ++M ) {
				Sum1 += Construct( IConst ).BSDFInput.SolBkRefl( M, J ) * Geom.Trn.Lamda( J ) * Geom.Inc.Lamda( M );
			}
		}
		for ( J = 1; J <= Geom.Trn.NBasis; ++J ) {
			Sum2 += Geom.Trn.Lamda( J );
		}

		if ( Sum2 != 0.0 ) {
			State.WinBkHemRefl = Sum1 / Sum2;
		} else {
			State.WinBkHemRefl = 0.0;
		}

		Construct( IConst ).ReflectSolDiffBack = State.WinBkHemRefl;

		State.WinBkHemAbs.allocate( State.NLayers );
		for ( L = 1; L <= State.NLayers; ++L ) {
			for ( J = 1; J <= Geom.Trn.NBasis; ++J ) {
				Sum3 += Geom.Trn.Lamda( J ) * Construct( IConst ).BSDFInput.Layer( L ).BkAbs( J, 1 );
			}

			if ( Sum2 != 0.0 ) {
				State.WinBkHemAbs( L ) = Sum3 / Sum2;
			} else {
				State.WinBkHemAbs( L ) = 0.0;
			}

			//Put this into the construction for use in non-detailed optical calculations
			Construct( IConst ).AbsDiffBack( L ) = State.WinBkHemAbs( L );
		}

		//Calculate Window Layer Front Hemispherical Absorptance
		Sum1 = 0.0;
		Sum2 = 0.0;
		for ( J = 1; J <= Geom.Inc.NBasis; ++J ) {
			Sum2 += Geom.Inc.Lamda( J );
		}
		State.WinFtHemAbs.allocate( State.NLayers );
		for ( L = 1; L <= State.NLayers; ++L ) {
			Sum1 = 0.0;
			for ( J = 1; J <= Geom.Inc.NBasis; ++J ) {
				Sum1 += Geom.Inc.Lamda( J ) * Construct( IConst ).BSDFInput.Layer( L ).FrtAbs( J, 1 );
			}

			if ( Sum2 != 0.0 ) {
				State.WinFtHemAbs( L ) = Sum1 / Sum2;
			} else {
				State.WinFtHemAbs( L ) = 0.0;
			}

			//Put this into the construction for use in non-detailed optical calculations
			Construct( IConst ).AbsDiff( L ) = State.WinFtHemAbs( L );
		}

		//Calculate Window Back Hemispherical Visible Reflectance
		Sum1 = 0.0;
		Sum2 = 0.0;
		//Note this again assumes the equivalence Inc basis = transmission basis for back incidence and
		// Trn basis = incident basis for back incidence
		for ( J = 1; J <= Geom.Trn.NBasis; ++J ) {
			for ( M = 1; M <= Geom.Inc.NBasis; ++M ) {
				Sum1 += Construct( IConst ).BSDFInput.VisBkRefl( M, J ) * Geom.Trn.Lamda( J ) * Geom.Inc.Lamda( M );
			}
		}
		for ( J = 1; J <= Geom.Trn.NBasis; ++J ) {
			Sum2 += Geom.Trn.Lamda( J );
		}

		if ( Sum2 != 0.0 ) {
			State.WinBkHemVisRefl = Sum1 / Sum2;
		} else {
			State.WinBkHemVisRefl = 0.0;
		}

		Construct( IConst ).ReflectVisDiffBack = State.WinBkHemVisRefl;

		//     *     *     *     *
		//Note potential problem if one relaxes the assumption that Inc and Trn basis have same structure:
		//  The following calculations are made for the set of ray numbers defined in the Trn basis that
		//   were determined to connect the center of the window to a particular back surface.
		//   Here it is assumed that one can reverse these rays and get an equivalent set in the Trn
		//   basis for back-incidence quantities: back transmittance and back layer absorptance
		//   This assumption may fail if the Inc and Trn bases are allowed to have different structure.
		//   Note also that in this case one would need to rethink the relationship of the basis
		//   definitions to back-incidence quantities:  possibly this would
		//   also require that the basis for back incident quantities be
		//   different from the Trn basis, and similarly the basis for backward outgoing rays
		//   be different from the Inc basis.

		//     *     *     *     *
		//  Note that we are assuming that for back incidence the layer numberings are the same
		//  as for front incidence, i.e., from outside to inside when incidence is from inside
		//     *     *     *     *
		//For back surfaces that are complex fenestrations, calculate the directional-hemispherical back
		//  reflectance and the directional back absorptance by layer for this fenestration receiving
		//  radiation via the back surface
		//  Make this calculation only for cases where the back surface is a Complex Fenestration
		//First allocate the back surface section of the state properties
		if ( ! allocated( State.BkSurf ) ) State.BkSurf.allocate( Window.NBkSurf );
		for ( KBkSurf = 1; KBkSurf <= Window.NBkSurf; ++KBkSurf ) { //back surface loop
			BaseSurf = Surface( ISurf ).BaseSurf; //ShadowComb is organized by base surface
			JSurf = ShadowComb( BaseSurf ).BackSurf( KBkSurf );
			if ( SurfaceWindow( JSurf ).WindowModelType != WindowBSDFModel ) continue;

			//  Directional-hemispherical back reflectance
			Sum1 = 0.0;
			Sum2 = 0.0;
			for ( J = 1; J <= Geom.NSurfInt( KBkSurf ); ++J ) { //Inc Ray loop
				Sum2 += Geom.Trn.Lamda( Geom.SurfInt( J, KBkSurf ) );
				for ( M = 1; M <= Geom.Inc.NBasis; ++M ) { //Outgoing Ray loop
					Sum1 += Geom.Trn.Lamda( Geom.SurfInt( J, KBkSurf ) ) * Geom.Inc.Lamda( M ) * Construct( IConst ).BSDFInput.SolBkRefl( Geom.SurfInt( J, KBkSurf ), M );
				} //Outgoing Ray loop
			} //Inc Ray loop
			if ( Sum2 > 0.0 ) {
				Hold = Sum1 / Sum2;
				for ( I = 1; I <= 24; ++I ) {
					for ( J = 1; J <= NumOfTimeStepInHour; ++J ) {
						State.BkSurf( KBkSurf ).WinDHBkRefl( I, J ) = Hold;
					}
				}
			} else {
				for ( I = 1; I <= 24; ++I ) {
					for ( J = 1; J <= NumOfTimeStepInHour; ++J ) {
						State.BkSurf( KBkSurf ).WinDHBkRefl( I, J ) = 0.0;
					}
				}
			}

			//  Directional layer  back absorption
			for ( L = 1; L <= State.NLayers; ++L ) { //layer loop
				Sum1 = 0.0;
				Sum2 = 0.0;
				for ( J = 1; J <= Geom.NSurfInt( KBkSurf ); ++J ) { //Inc Ray loop
					Sum2 += Geom.Trn.Lamda( Geom.SurfInt( J, KBkSurf ) );
					Sum1 += Geom.Trn.Lamda( Geom.SurfInt( J, KBkSurf ) ) * Construct( IConst ).BSDFInput.Layer( L ).BkAbs( Geom.SurfInt( J, KBkSurf ), 1 );
				} //Inc Ray loop
				if ( Sum2 > 0.0 ) {
					Hold = Sum1 / Sum2;
					for ( I = 1; I <= 24; ++I ) {
						for ( J = 1; J <= NumOfTimeStepInHour; ++J ) {
							State.BkSurf( KBkSurf ).WinDirBkAbs( I, J, L ) = Hold;
						}
					}
				} else {
					for ( I = 1; I <= 24; ++I ) {
						for ( J = 1; J <= NumOfTimeStepInHour; ++J ) {
							State.BkSurf( KBkSurf ).WinDirBkAbs( I, J, L ) = 0.0;
						}
					}
				}

			} //layer loop
		} //back surface loop

		// ********************************************************************************
		// Allocation and calculation of integrated values for front of window surface
		// ********************************************************************************

		// Sum of front absorptances for each incident direction (integration of absorptances)
		if ( ! allocated( State.IntegratedFtAbs ) ) State.IntegratedFtAbs.allocate( Geom.Inc.NBasis );
		for ( J = 1; J <= Geom.Inc.NBasis; ++J ) {
			Sum1 = 0.0;
			for ( L = 1; L <= State.NLayers; ++L ) { //layer loop
				Sum1 += Construct( IConst ).BSDFInput.Layer( L ).FrtAbs( J, 1 );
			}
			State.IntegratedFtAbs( J ) = Sum1;
		}

		// Integrating front transmittance
		if ( ! allocated( State.IntegratedFtTrans ) ) State.IntegratedFtTrans.allocate( Geom.Inc.NBasis );
		for ( J = 1; J <= Geom.Inc.NBasis; ++J ) { // Incident ray loop
			Sum1 = 0.0;
			for ( M = 1; M <= Geom.Trn.NBasis; ++M ) { // Outgoing ray loop
				Sum1 += Geom.Trn.Lamda( J ) * Construct( IConst ).BSDFInput.SolFrtTrans( J, M );
			} // Outgoing ray loop
			State.IntegratedFtTrans( J ) = Sum1;
		} // Incident ray loop

		if ( ! allocated( State.IntegratedFtRefl ) ) State.IntegratedFtRefl.allocate( Geom.Inc.NBasis );
		// Integrating front reflectance
		for ( J = 1; J <= Geom.Inc.NBasis; ++J ) { // Incoming ray loop
			State.IntegratedFtRefl( J ) = 1 - State.IntegratedFtTrans( J ) - State.IntegratedFtAbs( J );
		} //Incoming ray loop

		// ********************************************************************************
		// Allocation and calculation of integrated values for back of window surface
		// ********************************************************************************

		// Sum of back absorptances for each incident direction (integration of absorptances)
		if ( ! allocated( State.IntegratedBkAbs ) ) State.IntegratedBkAbs.allocate( Geom.Trn.NBasis );
		for ( J = 1; J <= Geom.Trn.NBasis; ++J ) {
			Sum1 = 0.0;
			for ( L = 1; L <= State.NLayers; ++L ) { //layer loop
				Sum1 += Construct( IConst ).BSDFInput.Layer( L ).BkAbs( J, 1 );
			}
			State.IntegratedBkAbs( J ) = Sum1;
		}

		// Integrating back reflectance
		if ( ! allocated( State.IntegratedBkRefl ) ) State.IntegratedBkRefl.allocate( Geom.Trn.NBasis );
		for ( J = 1; J <= Geom.Trn.NBasis; ++J ) { // Outgoing ray loop
			Sum1 = 0.0;
			for ( M = 1; M <= Geom.Inc.NBasis; ++M ) { // Incident ray loop
				Sum1 += Geom.Inc.Lamda( J ) * Construct( IConst ).BSDFInput.SolBkRefl( J, M );
			} //Incident ray loop
			State.IntegratedBkRefl( J ) = Sum1;
		} //Outgoing ray loop

		if ( ! allocated( State.IntegratedBkTrans ) ) State.IntegratedBkTrans.allocate( Geom.Trn.NBasis );
		// Integrating back transmittance
		for ( J = 1; J <= Geom.Trn.NBasis; ++J ) { // Outgoing ray loop
			State.IntegratedBkTrans( J ) = 1 - State.IntegratedBkRefl( J ) - State.IntegratedBkAbs( J );
		} //Outgoing ray loop

	}

	Real64
	SkyWeight( Vector const & EP_UNUSED( DirVec ) ) // Direction of the element to be weighted
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Joe Klems
		//       DATE WRITTEN   June 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Search a one-dimensional array for a given value, returning the index of the element equal to the value, if
		//   found, or zero

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace Vectors;

		// Return value
		Real64 Wt; // Weight

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS
		//na

		//Flow:

		Wt = 1.0;

		// To do:  figure out how to weight sky elements to reproduce the current E+ assumptions
		//  Possibly one will need to calculated average DH transmittance for isotropic sky and
		//  horizon separately and then later average according to sky conditions.  Then a completely
		//  different scheme for daylight.  For now: rays that reach sky equally weighted in calculating
		//  transmittance, rays passing through surfaces with scheduled transmittance are neglected.

		return Wt;

	}

	Real64
	SkyGndWeight( Vector const & EP_UNUSED( PosVec ) ) // x,y,z(=0) of ground intersection pt
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Joe Klems
		//       DATE WRITTEN   June 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Search a one-dimensional array for a given value, returning the index of the element equal to the value, if
		//   found, or zero

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace Vectors;

		// Return value
		Real64 Wt; // Weight

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS
		//na

		//Flow:

		Wt = 1.0;

		//  At present, equally weights all ground rays for calculation of the complex window transmittance for
		//  sky radiation reflected from ground.  This does not take into account shading of the ground.
		//  The correct procedure would be to generate a set of rays to the sky and see which do not intersect
		//  surfaces, as is done in the reflection manager.  However, this would increase computational load.
		//  Given that equal weighting, by averaging the transmittance only over rays that come from the ground,
		//  already produces a more accurate ground transmittance than the existing method, it is at least questionable
		//  whether the more detailed procedure would produce enough improvement in accuracy to make up for
		//  the additional calculation time.  Therefore a more detailed treatment is deferred until there is some
		//  experience with the new method to determine whether further detail is warranted.

		return Wt;

	}

	BSDFDaylghtPosition
	DaylghtAltAndAzimuth( Vector const & UnitVect ) // vector which needs to be converted
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Vidanovic
		//       DATE WRITTEN   April 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Transform unit vector (given in world coordinates) into altitude and azimuth.  Azimuth is measured from positive x-axe.
		// Altitude range is from -pi/2 to pi/2. Vector laying in horizontal plane will have altitude equal to zero and vector
		// pointing upward will have altitude equal to pi/2. Range for azimuth is calculated from -pi to +pi.

		// METHODOLOGY EMPLOYED:
		// <n/a>

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataBSDFWindow;
		using namespace Vectors;
		using namespace DataGlobals;

		// Return value
		BSDFDaylghtPosition DayPos; // altitude and azimuth in world coordinates

		if ( UnitVect.x != 0.0 ) {
			if ( UnitVect.x >= 0.0 ) {
				DayPos.Azimuth = std::atan( UnitVect.y / UnitVect.x );
			} else {
				if ( UnitVect.y >= 0.0 ) {
					DayPos.Azimuth = Pi + std::atan( UnitVect.y / UnitVect.x );
				} else {
					DayPos.Azimuth = -Pi + std::atan( UnitVect.y / UnitVect.x );
				}
			}
		} else {
			if ( UnitVect.y >= 0.0 ) {
				DayPos.Azimuth = PiOvr2;
			} else {
				DayPos.Azimuth = -PiOvr2;
			}
		}

		DayPos.Altitude = std::asin( UnitVect.z );

		return DayPos;

	}

	Vector
	WorldVectFromW6(
		Real64 const Theta, // Polar angle in W6 Coords
		Real64 const Phi, // Azimuthal angle in W6 Coords
		int const RadType, // Type of radiation: Front_Incident, etc.
		Real64 const Gamma, // Surface tilt angle, radians, world coordinate system
		Real64 const Alpha // Surface azimuth, radians, world coordinate system
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Joe Klems
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Transform angular coordinates in the WINDOW6 coordinate system for
		// a given surface into a unit vector in the world coordinate system,
		// pointing to the radiation source (for incident radiation) or in
		// the direction of propagation (for outgoing radiation)

		// METHODOLOGY EMPLOYED:
		// <n/a>

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace Vectors;

		// Return value
		Vector UnitVect; // unit vector direction in world CS

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// Error tolerance is used to make small numbers equal to zero.  Due to precision of pi constant used in E+, performing
		// trigonometric operations on those constant will not cause absolutely accurate results
		Real64 const ErrorTolerance( 1.e-10 );

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		UnitVect = Vector( 0.0, 0.0, 0.0 );
		{ auto const SELECT_CASE_var( RadType );
		if ( SELECT_CASE_var == Front_Incident ) { //W6 vector will point in direction of propagation, must reverse to get world vector
			//  after the W6 vector has been rotated into the world CS
			UnitVect.x = std::sin( Theta ) * std::sin( Phi ) * std::cos( Gamma ) * std::sin( Alpha ) - std::sin( Theta ) * std::cos( Phi ) * std::cos( Alpha ) + std::cos( Theta ) * std::sin( Gamma ) * std::sin( Alpha );
			UnitVect.y = std::sin( Theta ) * std::cos( Phi ) * std::sin( Alpha ) + std::sin( Theta ) * std::sin( Phi ) * std::cos( Gamma ) * std::cos( Alpha ) + std::cos( Theta ) * std::sin( Gamma ) * std::cos( Alpha );
			UnitVect.z = -( std::sin( Theta ) * std::sin( Phi ) * std::sin( Gamma ) - std::cos( Theta ) * std::cos( Gamma ) );
		} else if ( SELECT_CASE_var == Front_Transmitted ) {
			UnitVect.x = std::sin( Theta ) * std::cos( Phi ) * std::cos( Alpha ) - std::sin( Theta ) * std::sin( Phi ) * std::cos( Gamma ) * std::sin( Alpha ) - std::cos( Theta ) * std::sin( Gamma ) * std::sin( Alpha );
			UnitVect.y = -( std::sin( Theta ) * std::cos( Phi ) * std::sin( Alpha ) + std::sin( Theta ) * std::sin( Phi ) * std::cos( Gamma ) * std::cos( Alpha ) + std::cos( Theta ) * std::sin( Gamma ) * std::cos( Alpha ) );
			UnitVect.z = std::sin( Theta ) * std::sin( Phi ) * std::sin( Gamma ) - std::cos( Theta ) * std::cos( Gamma );
		} else if ( SELECT_CASE_var == Front_Reflected ) {
			UnitVect.x = std::sin( Theta ) * std::cos( Phi ) * std::cos( Alpha ) - std::sin( Theta ) * std::sin( Phi ) * std::cos( Gamma ) * std::sin( Alpha ) + std::cos( Theta ) * std::sin( Gamma ) * std::sin( Alpha );
			UnitVect.y = std::cos( Theta ) * std::sin( Gamma ) * std::cos( Alpha ) - std::sin( Theta ) * std::cos( Phi ) * std::sin( Alpha ) - std::sin( Theta ) * std::sin( Phi ) * std::cos( Gamma ) * std::cos( Alpha );
			UnitVect.z = std::sin( Theta ) * std::sin( Phi ) * std::sin( Gamma ) + std::cos( Theta ) * std::cos( Gamma );
		} else if ( SELECT_CASE_var == Back_Incident ) {
			UnitVect.x = std::sin( Theta ) * std::sin( Phi ) * std::cos( Gamma ) * std::sin( Alpha ) - std::sin( Theta ) * std::cos( Phi ) * std::cos( Alpha ) - std::cos( Theta ) * std::sin( Gamma ) * std::sin( Alpha );
			UnitVect.y = std::sin( Theta ) * std::cos( Phi ) * std::sin( Alpha ) + std::sin( Theta ) * std::sin( Phi ) * std::cos( Gamma ) * std::cos( Alpha ) - std::cos( Theta ) * std::sin( Gamma ) * std::cos( Alpha );
			UnitVect.z = -std::cos( Theta ) * std::cos( Gamma ) - std::sin( Theta ) * std::sin( Phi ) * std::sin( Gamma );
		} else if ( SELECT_CASE_var == Back_Transmitted ) { //This is same as front reflected
			UnitVect.x = std::sin( Theta ) * std::cos( Phi ) * std::cos( Alpha ) - std::sin( Theta ) * std::sin( Phi ) * std::cos( Gamma ) * std::sin( Alpha ) + std::cos( Theta ) * std::sin( Gamma ) * std::sin( Alpha );
			UnitVect.y = std::cos( Theta ) * std::sin( Gamma ) * std::cos( Alpha ) - std::sin( Theta ) * std::cos( Phi ) * std::sin( Alpha ) - std::sin( Theta ) * std::sin( Phi ) * std::cos( Gamma ) * std::cos( Alpha );
			UnitVect.z = std::sin( Theta ) * std::sin( Phi ) * std::sin( Gamma ) + std::cos( Theta ) * std::cos( Gamma );
		} else if ( SELECT_CASE_var == Back_Reflected ) { //This is same as front transmitted
			UnitVect.x = std::sin( Theta ) * std::cos( Phi ) * std::cos( Alpha ) - std::sin( Theta ) * std::sin( Phi ) * std::cos( Gamma ) * std::cos( Alpha ) - std::cos( Theta ) * std::sin( Gamma ) * std::sin( Alpha );
			UnitVect.y = -( std::sin( Theta ) * std::cos( Phi ) * std::sin( Alpha ) + std::sin( Theta ) * std::sin( Phi ) * std::cos( Gamma ) * std::cos( Alpha ) + std::cos( Theta ) * std::sin( Gamma ) * std::cos( Alpha ) );
			UnitVect.z = std::sin( Theta ) * std::sin( Phi ) * std::sin( Gamma ) - std::cos( Theta ) * std::cos( Gamma );
		}}

		// Remove small numbers from evaluation (due to limited decimal points for pi)
		if ( std::abs( UnitVect.x ) <= ErrorTolerance ) UnitVect.x = 0.0;
		if ( std::abs( UnitVect.y ) <= ErrorTolerance ) UnitVect.y = 0.0;
		if ( std::abs( UnitVect.z ) <= ErrorTolerance ) UnitVect.z = 0.0;

		return UnitVect;

	}

	int
	FindInBasis(
		Vector const & RayToFind, // Ray vector direction in world CS
		int const RadType, // Type of radiation: Front_Incident, etc.
		int const ISurf, // Window Surface number
		int const EP_UNUSED( IState ), // Complex Fenestration state number
		BasisStruct const & Basis, // Complex Fenestration basis root
		Real64 & Theta, // Theta value for ray
		Real64 & Phi // Phi value for ray
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Joe Klems
		//       DATE WRITTEN August 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:

		// METHODOLOGY EMPLOYED:
		// <n/a>

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace Vectors;

		// Return value
		int RayIndex; // Index of ray in basis, zero if ray not in hemisphere

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// INTEGER, INTENT(IN)      ::  IWind  !window index in window list

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ITheta; // Table index of Theta
		int IPhi; // Table index of Phi, given ITheta
		int IThDn; // Theta lower table index
		int IThUp; // Theta upper table index
		int IPhDn; // Phi lower table index
		int IPhUp; // Phi upper table index
		Real64 Gamma; // Gamma (tilt) angle of window
		Real64 Alpha; // Alpha (azimuth) angle of window
		Real64 DotProd;

		Theta = 0.0;
		Phi = 0.0;

		// Check if surface and vector are pointing in different directions
		DotProd = dot( RayToFind, Surface( ISurf ).NewellSurfaceNormalVector );
		if ( DotProd <= 0.0 ) {
			RayIndex = 0;
			return RayIndex;
		}

		//get window tilt and azimuth
		Gamma = DegToRadians * Surface( ISurf ).Tilt;
		Alpha = DegToRadians * Surface( ISurf ).Azimuth;
		//get the corresponding local Theta, Phi for ray
		W6CoordsFromWorldVect( RayToFind, RadType, Gamma, Alpha, Theta, Phi );

		if ( Theta >= 0.5 * Pi ) { //Ray was in not in correct hemisphere
			RayIndex = 0;
			return RayIndex;
		}
		if ( Basis.BasisSymmetryType == BasisSymmetry_None ) {
			//Search the basis thetas
			if ( Theta <= 0.0 ) {
				//Special case, Theta = 0.; this is always the first basis element
				RayIndex = 1;
				return RayIndex;
			}
			//So here Theta > 0
			//Note the table searches always go to the limit point, which is not itself a basis element
			IThUp = SearchAscTable( Theta, Basis.NThetas + 1, Basis.Thetas );
			IThDn = IThUp - 1;
			//Determine which of the theta basis points is closer to the Theta value
			if ( Theta <= Basis.Grid( Basis.BasisIndex( 1, IThDn ) ).UpprTheta ) {
				//Note this will take care of both the special cases IThUp=2 and IThUp=NThetas +1
				ITheta = IThDn;
			} else {
				ITheta = IThUp;
			}
			//Now determine the Phi index
			if ( Basis.NPhis( ITheta ) == 1 ) {
				//Note that for W6 basis this can only happen for the first basis element
				//If later bases are introduced this logic may have to be redesigned
				RayIndex = Basis.BasisIndex( 1, ITheta );
				return RayIndex;
			}
			IPhUp = SearchAscTable( Phi, Basis.NPhis( ITheta ) + 1, Basis.Phis( ITheta, _ ) );
			IPhDn = IPhUp - 1;
			if ( Phi <= Basis.Grid( Basis.BasisIndex( IPhDn, ITheta ) ).UpprPhi ) {
				IPhi = IPhDn;
			} else {
				if ( IPhUp == Basis.NPhis( ITheta ) + 1 ) {
					// Phi is above upper limit for highest Phi basis element, meaning it is closer to 2Pi,
					// i.e., the first element
					IPhi = 1;
				} else {
					IPhi = IPhUp;
				}
			}
			RayIndex = Basis.BasisIndex( IPhi, ITheta );
			return RayIndex;
		} else if ( Basis.BasisSymmetryType == BasisSymmetry_Axisymmetric ) {
			//Search the basis thetas
			if ( Theta <= 0.0 ) {
				//Special case, Theta = 0.; this is always the first basis element
				RayIndex = 1;
				return RayIndex;
			}
			//So here Theta > 0
			//Note the table searches always go to the limit point, which is not itself a basis element
			IThUp = SearchAscTable( Theta, Basis.NThetas + 1, Basis.Thetas );
			IThDn = IThUp - 1;
			//Determine which of the theta basis points is closer to the Theta value
			if ( Theta <= Basis.Grid( Basis.BasisIndex( 1, IThDn ) ).UpprTheta ) {
				//Note this will take care of both the special cases IThUp=2 and IThUp=NThetas +1
				ITheta = IThDn;
			} else {
				ITheta = IThUp;
			}
			RayIndex = Basis.BasisIndex( 1, ITheta );
			return RayIndex;
		}
		//No other type is implemented
		RayIndex = 0;

		return RayIndex;

	}

	void
	W6CoordsFromWorldVect(
		Vector const & RayVect, // Ray vector direction in world CS
		int const RadType, // Type of radiation: Front_Incident, etc.
		Real64 const Gamma, // Surface tilt angle, world coordinate system
		Real64 const Alpha, // Surface azimuth, world coordinate system
		Real64 & Theta, // Polar angle in W6 Coords
		Real64 & Phi // Azimuthal angle in W6 Coords
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Joe Klems
		//       DATE WRITTEN   August 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Invert the transformation from W6 to world coordinates to
		// calculate the theta, phi corresponding to a given ray direction
		// in the world coordinate system, for a window with a
		// given rotation and tilt (Gamma and Alpha)
		//  (needed for locating the sun direction in the local coordinate system)

		// METHODOLOGY EMPLOYED:
		// <n/a>

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace Vectors;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 Cost( 0.0 ); // Temp for cos theta
		Real64 Sint; // Temp for sin theta
		Real64 Psi; // Temp for phi before rotation adjustment
		Real64 RdotX; // Temp variable for manipulating .dot. produt
		Real64 RdotY; // Temp variable for manipulating .dot. produt
		Real64 RdotZ; // Temp variable for manipulating .dot. produt

		// Object Data
		Vector W6x; // W6 x coordinate unit vector
		Vector W6y; // W6 y coordinate unit vector
		Vector W6z; // W6 z coordinate unit vector

		// define the local W6 coordinate vectors
		W6x.x = std::cos( Alpha );
		W6x.y = -std::sin( Alpha );
		W6x.z = 0.0;
		W6y.x = -std::cos( Gamma ) * std::sin( Alpha );
		W6y.y = -std::cos( Gamma ) * std::cos( Alpha );
		W6y.z = std::sin( Gamma );
		W6z.x = -std::sin( Gamma ) * std::sin( Alpha );
		W6z.y = -std::sin( Gamma ) * std::cos( Alpha );
		W6z.z = -std::cos( Gamma );
		{ auto const SELECT_CASE_var( RadType );
		if ( SELECT_CASE_var == Front_Incident ) {
			RdotZ = dot( W6z, RayVect );
			Cost = -RdotZ;
			Sint = std::sqrt( 1.0 - pow_2( Cost ) );
			Theta = std::acos( Cost );
			RdotY = dot( W6y, RayVect );
			RdotX = dot( W6x, RayVect );
			Psi = std::atan2( -RdotY / Sint, - RdotX / Sint );
			if ( Psi < 0.0 ) {
				Phi = 2.0 * Pi + Psi;
			} else {
				Phi = Psi;
			}
		} else if ( SELECT_CASE_var == Front_Transmitted ) {
			Cost = dot( W6z, RayVect );
			Sint = std::sqrt( 1.0 - pow_2( Cost ) );
			Theta = std::acos( Cost );
			RdotY = dot( W6y, RayVect );
			RdotX = dot( W6x, RayVect );
			Psi = std::atan2( RdotY / Sint, RdotX / Sint );
			if ( Psi < 0.0 ) {
				Phi = 2.0 * Pi + Psi;
			} else {
				Phi = Psi;
			}
		} else if ( SELECT_CASE_var == Front_Reflected ) {
			RdotZ = dot( W6z, RayVect );
			Cost = -RdotZ;
			Sint = std::sqrt( 1.0 - pow_2( Cost ) );
			Theta = std::acos( Cost );
			RdotY = dot( W6y, RayVect );
			RdotX = dot( W6x, RayVect );
			Psi = std::atan2( RdotY / Sint, RdotX / Sint );
			if ( Psi < 0.0 ) {
				Phi = 2.0 * Pi + Psi;
			} else {
				Phi = Psi;
			}
		} else if ( SELECT_CASE_var == Back_Incident ) {
			Cost = dot( W6z, RayVect );
			Sint = std::sqrt( 1.0 - pow_2( Cost ) );
			Theta = std::acos( Cost );
			RdotY = dot( W6y, RayVect );
			RdotX = dot( W6x, RayVect );
			Psi = std::atan2( -RdotY / Sint, - RdotX / Sint );
			if ( Psi < 0.0 ) {
				Phi = 2 * Pi + Psi;
			} else {
				Phi = Psi;
			}
		} else if ( SELECT_CASE_var == Back_Transmitted ) { //This is same as front reflected
			RdotZ = dot( W6z, RayVect );
			Cost = -RdotZ;
			Sint = std::sqrt( 1.0 - pow_2( Cost ) );
			Theta = std::acos( Cost );
			RdotY = dot( W6y, RayVect );
			RdotX = dot( W6x, RayVect );
			Psi = std::atan2( RdotY / Sint, RdotX / Sint );
			if ( Psi < 0.0 ) {
				Phi = 2.0 * Pi + Psi;
			} else {
				Phi = Psi;
			}
		} else if ( SELECT_CASE_var == Back_Reflected ) { //This is same as front transmitted
			Cost = dot( W6z, RayVect );
			Sint = std::sqrt( 1.0 - pow_2( Cost ) );
			Theta = std::acos( Cost );
			RdotY = dot( W6y, RayVect );
			RdotX = dot( W6x, RayVect );
			Psi = std::atan2( RdotY / Sint, RdotX / Sint );
			if ( Psi < 0.0 ) {
				Phi = 2.0 * Pi + Psi;
			} else {
				Phi = Psi;
			}
		} else {
			assert( false );
		}}
		if ( std::abs( Cost ) < rTinyValue ) Cost = 0.0;
		if ( Cost < 0.0 ) Theta = Pi - Theta; //This signals ray out of hemisphere

	}

	void
	CalcComplexWindowThermal(
		int const SurfNum, // Surface number
		int & ConstrNum, // Construction number
		Real64 const HextConvCoeff, // Outside air film conductance coefficient
		Real64 & SurfInsideTemp, // Inside window surface temperature
		Real64 & SurfOutsideTemp, // Outside surface temperature (C)
		Real64 & SurfOutsideEmiss,
		int const CalcCondition // Calucation condition (summer, winter or no condition)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   October 2009
		//       MODIFIED       Simon Vidanovic
		//       RE-ENGINEERED  September 2011

		// PURPOSE OF THIS SUBROUTINE:
		// wrapper between E+ and TARCOG

		// METHODOLOGY EMPLOYED:
		// draft out an attempt for proof-of-concept, to reuse native TARCOG implementation
		// based off of 1-26-2009 version of WinCOG/TARCOG solution from Carli, Inc.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		//USE WindowTARCOGManager, ONLY: tarcog
		// Using/Aliasing
		using namespace DataBSDFWindow;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataLoopNode::Node;
		using Psychrometrics::PsyCpAirFnWTdb;
		using Psychrometrics::PsyTdpFnWPb;
		using General::InterpSlatAng; // Function for slat angle interpolation
		using General::InterpSw;
		using InputProcessor::SameString;
		using DataHeatBalSurface::HcExtSurf;
		using DataGlobals::StefanBoltzmann;
		using TARCOGGassesParams::maxgas;
		using TARCOGParams::maxlay;
		using TARCOGParams::maxlay1;
		using DataHeatBalance::GasCoeffsAir;
		using DataHeatBalance::SupportPillar;
		using TARCOGMain::TARCOG90;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// (temperature of innermost face) [C]
		//INTEGER, INTENT(IN)        :: CurrentThermalModelNumber

		// SUBROUTINE PARAMETER DEFINITIONS:
		//INTEGER,  PARAMETER :: maxlay = 100 ! maximum number of layers (including laminates)
		//INTEGER,  PARAMETER :: maxgas = 10  ! maximum number of individual gasses
		//INTEGER, PARAMETER :: maxlay1  = maxlay+1     ! maximum number of 'gaps', including in and out (maxlay+1)
		//REAL(r64), PARAMETER :: StefanBoltzmannConst = 5.6697d-8   ! Stefan-Boltzmann constant in W/(m2*K4)

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// TARCOG Inputs:
		static int nlayer( 0 ); // Number of glazing layers
		static int iwd( 0 ); // Wind direction:  0 - windward, 1 - leeward
		static Real64 tout( 0.0 ); // Outdoor temperature [K]
		static Real64 tind( 0.0 ); // Indoor temperature [K]
		static Real64 trmin( 0.0 ); // Indoor mean radiant temperature [K]
		static Real64 wso( 0.0 ); // Outdoor wind speed [m/s]
		static Real64 wsi( 0.0 ); // Inside forced air speed [m/s]
		static Real64 dir( 0.0 ); // Direct solar radiation [W/m^2]
		static int isky( 0 ); // Flag for sky temperature (Tsky) and sky emittance (esky)
		//                      0 - both tsky and esky are specified
		//                      1 - tsky specified, esky = 1
		//                      2 - Swinbank model for effective sky emittance
		static Real64 tsky( 0.0 ); // Night sky temperature [K]
		static Real64 esky( 0.0 ); // Effective night sky emittance
		static Real64 fclr( 0.0 ); // Fraction of sky that is clear
		Real64 VacuumPressure; // maximal pressure for gas to be considered as vacuum [Pa]
		Real64 VacuumMaxGapThickness; // maximal gap thickness for which vacuum calculation will work without issuing
		// warning message
		static Array1D< Real64 > gap( maxlay, 0.0 ); // Vector of gap widths [m] {maxlay}
		static Array1D< Real64 > thick( maxlay, 0.0 ); // Vector of glass thicknesses [m] {maxlay}
		static Array1D< Real64 > scon( maxlay, 0.0 ); // Vector of conductivities of each glazing layer  [W/m.K] {maxlay}
		static Array1D< Real64 > tir( maxlay*2, 0.0 ); // Vector of IR transmittances of each layer {2*maxlay - 2 surfaces per layer}
		static Array1D< Real64 > emis( maxlay*2, 0.0 ); // Vector of IR emittances of each surface {2*maxlay - 2 surfaces per layer}
		static Array1D_int SupportPlr( maxlay, 0 ); // Shows whether or not gap have support pillar
		// 0 - does not have support pillar
		// 1 - have support pillar
		static Array1D< Real64 > PillarSpacing( maxlay, 0.0 ); // Pillar spacing for each gap (used in case there is support pillar)
		static Array1D< Real64 > PillarRadius( maxlay, 0.0 ); // Pillar radius for each gap (used in case there is support pillar)

		static Real64 totsol( 0.0 ); // Total solar transmittance of the IGU
		static Real64 tilt( 0.0 ); // Window tilt [degrees]
		static Array1D< Real64 > asol( maxlay, 0.0 ); // Vector of Absorbed solar energy fractions for each layer {maxlay}
		static Real64 height( 0.0 ); // IGU cavity height [m]
		static Real64 heightt( 0.0 ); // Total window height [m]
		static Real64 width( 0.0 ); // Window width [m]
		static Array1D< Real64 > presure( maxlay+1, 0.0 ); // Vector of gas pressures in gaps [N/m^2] {maxlay+1}

		//Deflection
		//Tarcog requires deflection as input parameters.  Deflection is NOT used in EnergyPlus simulations
		int CalcDeflection; // Deflection calculation flag:
		//    0 - no deflection calculations
		//    1 - perform deflection calculation (input is Pressure/Temp)
		//    2 - perform deflection calculation (input is measured deflection)
		Real64 Pa; // Atmospheric (outside/inside) pressure (used onlu if CalcDeflection = 1)
		Real64 Pini; // Initial presssure at time of fabrication (used only if CalcDeflection = 1)
		Real64 Tini; // Initial temperature at time of fabrication (used only if CalcDeflection = 1)
		static Array1D< Real64 > GapDefMax( maxlay-1, 0.0 ); // Vector of gap widths in deflected state.  It will be used as input
		// if CalcDeflection = 2. In case CalcDeflection = 1 it will return recalculated
		// gap widths. [m]
		static Array1D< Real64 > YoungsMod( maxlay, 0.0 ); // Vector of Young's modulus. [m]
		static Array1D< Real64 > PoissonsRat( maxlay, 0.0 ); // Vector of Poisson's Ratios. [m]
		static Array1D< Real64 > LayerDef( maxlay, 0.0 ); // Vector of layers deflection. [m]

		static Array2D_int iprop( maxgas, maxlay+1, 1 ); // Matrix of gas codes - see above {maxgap x maxgas}
		static Array2D< Real64 > frct( maxgas, maxlay+1, 0.0 ); // Matrix of mass percentages in gap mixtures  {maxgap x maxgas}
		static Array2D< Real64 > gcon( 3, maxgas, 0.0 ); // Matrix of constants for gas conductivity calc
		//     (A, B, C for max of 10 gasses) {maxgas x 3}
		static Array2D< Real64 > gvis( 3, maxgas, 0.0 ); // Matrix of constants for gas dynamic viscosity calc
		//     (A, B, C for max of 10 gasses) {maxgas x 3}
		static Array2D< Real64 > gcp( 3, maxgas, 0.0 ); // Matrix of constants for gas specific heat calc at constant pressure
		//     (A, B, C for max of 10 gasses) {maxgas x 3}
		static Array1D< Real64 > wght( maxgas, 0.0 ); // Vector of Molecular weights for gasses {maxgas}
		static Array1D< Real64 > gama( maxgas, 0.0 ); // Vector of spefic heat ration for low pressure calc {maxgas}
		static bool feedData( false ); // flag to notify if data needs to be feed into gas arrays
		static Array1D_int nmix( maxlay+1, 0 ); // Vector of number of gasses in gas mixture of each gap {maxlay+1}
		static Real64 hin( 0.0 ); // Indoor combined film coefficient (if non-zero) [W/m^2.K]
		static Real64 hout( 0.0 ); // Outdoor combined film coefficient (if non-zero) [W/m^2.K]
		static Array1D_int ibc( 2, 0 ); // Vector of boundary condition flags (ibc(1) - outdoor, ibc(2) - indoor)
		//             0 - h to be calculated;
		//             1 - combined film coefficient (h) prescribed;
		//             2 - convective film coefficient (hc) prescribed.
		//           Also used in old algorithms for calculating h, accessible through
		//           negative values for flags:
		//             -1  - old SPC142 correlation
		//             -2  - Klems-Yazdanian correlation (applicable to outdoor only)
		//             -3  - Kimura correlation (applicable to outdoor only)
		static Array1D< Real64 > Atop( maxlay, 0.0 ); // Vector with areas of top openings - between SD layers and top of
		//               glazing cavity, for each layer [m^2] {maxlay} *
		static Array1D< Real64 > Abot( maxlay, 0.0 ); // Vector with areas of bottom openings - between SD layers
		//               and bottom of glazing cavity [m^2] {maxlay}
		static Array1D< Real64 > Al( maxlay, 0.0 ); // Vector with areas of left-hand side openings - between SD layers
		//               and left end of glazing cavity [m^2] {maxlay}
		static Array1D< Real64 > Ar( maxlay, 0.0 ); // Vector of areas of right-hand side openings - between SD layers
		//               and right end of glazing cavity [m^2] {maxlay}
		static Array1D< Real64 > Ah( maxlay, 0.0 ); // Vector of total areas of holes for each SD [m^2] {maxlay}
		static Array1D< Real64 > SlatThick( maxlay, 0.0 ); // Thickness of the slat material [m] {maxlay} **
		static Array1D< Real64 > SlatWidth( maxlay, 0.0 ); // Slat width [m] {maxlay}
		static Array1D< Real64 > SlatAngle( maxlay, 0.0 ); // Slat tilt angle [deg] {maxlay}
		static Array1D< Real64 > SlatCond( maxlay, 0.0 ); // Conductivity of the slat material [W/m.K] {maxlay}
		static Array1D< Real64 > SlatSpacing( maxlay, 0.0 ); // Distance between slats [m] {maxlay}
		static Array1D< Real64 > SlatCurve( maxlay, 0.0 ); // Curvature radius of the slat [m] {maxlay}
		static Array1D< Real64 > vvent( maxlay+1, 0.0 ); // Vector of velocities for forced ventilation, for each gap, and for
		//               outdoor and indoor environment [m/s] {maxlay+1} ***
		static Array1D< Real64 > tvent( maxlay+1, 0.0 ); // Vector of temperatures of ventilation gas for forced ventilation, for each
		//  gap, and for outdoor and indoor environment [K] {maxlay+1}
		static Array1D_int LayerType( maxlay, 0 ); // Glazing layer type flag {maxlay}:
		//                 0 - Specular layer,
		//                 1 - Venetian blind (SD)
		//                 2 - Woven shade (SD) (not implemented)
		//                 3 - Diffuse shade (not implemented)
		static Array1D_int nslice( maxlay, 0 ); // Vector of numbers of slices in a laminated glazing layers
		//   (0 - monolithic layer) {maxlay}
		static Array1D< Real64 > LaminateA( maxlay, 0.0 ); // Left-hand side array for creating slice equations {maxlay}
		static Array1D< Real64 > LaminateB( maxlay, 0.0 ); // Right-hand side array for creating slice equations {maxlay}
		static Array1D< Real64 > sumsol( maxlay, 0.0 ); // Array of absorbed solar energy fractions for each laminated
		//               glazing layer [W/m^2] {maxlay}
		static int standard( 1 ); // Calculation standard switch:
		//                 1 - ISO 15099,
		//                 2 - EN673 / ISO 10292 Declared,
		//                 3 - EN673 / ISO 10292 Design.
		static int ThermalMod( 0 ); // Thermal model:
		//                 0 - ISO15099
		//                 1 - Scaled Cavity Width (SCW)
		//                 2 - Convective Scalar Model (CSM)
		static int Debug_mode( 0 ); // Switch for debug output files:
		//                 0 - don't create debug output files
		//                 1 - append results to existing debug output file (where applicable)
		//                 2 - store results in a new debug output file
		static std::string Debug_dir; // Target directory for debug files (pointer to a character array)
		static std::string Debug_file( "Test" ); // Template file name used to create debug output files
		static std::int32_t Window_ID( -1 ); // ID of the window (long integer value, passed by W6)
		static std::int32_t IGU_ID( -1 ); // ID of the IGU (long integer value, passed by W6)
		static Real64 SDScalar( 0.0 ); // SD convection factor (value between 0 and 1)
		//                 0.0 - No SD layer
		//                 1.0 - Closed SD
		//               Notes:   * vvent, tvent, Atop, Abot, Al, Ar and Ah are considered for SD layers only.
		//                       ** SlatThick, SlatWidth, SlatAngle, SlatCond, SlatSpacing, SlatCurve
		//                          are used for Venetian blind layers only.
		//                      *** For vvent & tvent: vvent(1) - exterior, vvent(nlayer+1) - interior.
		//                     **** Forced ventilation calculation is not active at this time.
		// TARCOG Output:

		static Array1D< Real64 > theta( maxlay*2, 0.0 ); // Vector of average temperatures of glazing surfaces [K] {2*maxlay}
		static Array1D< Real64 > q( maxlay*2+1, 0.0 ); // Vector of various heat fluxes [W/m^2] {2*maxlay+1},
		//    depending on element index:
		//    1  = qout (heat flux from outer-most glazing surface to outdoor space)
		//   2*i = qpane(i) (heat flux through i-th glazing layer)
		// 2*i-1 = qgap(i) (heat flux from i-th glazing cavity to indoor-faced
		//          surface of the adjacent glazing layer)
		// 2*nlayer+1 = qin (heat flux from indoor space to inner-most glazing
		//              surface)
		static Array1D< Real64 > qprim( maxlay1, 0.0 ); // Vector of heat fluxes from the outdoor-faced surfaces of glazing layers
		//    towards the adjacent glazing cavity [W/m2]
		static Array1D< Real64 > qv( maxlay1, 0.0 ); // Vector of heat fluxes to each gap by ventillation [W/m^2]
		static Real64 ufactor( 0.0 ); // Center of glass U-value [W/m^2.K]
		static Real64 sc( 0.0 ); // Shading Coefficient
		static Real64 hflux( 0.0 ); // Net heat flux between room and window [W/m^2]
		static Real64 hcin( 0.0 ); // Indoor convective surface heat transfer coefficient  [W/m^2.K]
		static Real64 hcout( 0.0 ); // Outdoor convective surface heat transfer coefficient [W/m^2.K]
		static Real64 hrin( 0.0 ); // Indoor radiative surface heat transfer coefficient [W/m^2.K]
		static Real64 hrout( 0.0 ); // Outdoor radiative surface heat transfer coefficient [W/m^2.K]
		static Array1D< Real64 > hcgap( maxlay1, 0.0 ); // Convective part of gap effective conductivity {maxlay}
		static Array1D< Real64 > hrgap( maxlay1, 0.0 ); // Radiative part of gap effective conductivity (including in and out)
		static Real64 shgc( 0.0 ); // Solar heat gain coefficient - per ISO 15099
		static Real64 shgct( 0.0 ); // Solar heat gain coefficient - per old procedure
		static Real64 tamb( 0.0 ); // Outdoor environmental temperature [K]
		static Real64 troom( 0.0 ); // Indoor environmental temperature [K]
		static Array1D< Real64 > hg( maxlay, 0.0 ); // Gas conductance of the glazing cavity
		//         [W/m^2.K] - EN673 and ISO 10292 procedure
		static Array1D< Real64 > hr( maxlay, 0.0 ); // Radiation conductance of the glazing cavity
		//         [W/m^2.K] - EN673 and ISO 10292 procedure
		static Array1D< Real64 > hs( maxlay, 0.0 ); // Thermal conductance of the glazing cavity
		//         [W/m^2.K] - EN673 and ISO 10292 procedure
		static Real64 he( 0.0 ); // External heat transfer coefficient [W/m^2.K] - EN673 and ISO 10292 procedure
		static Real64 hi( 0.0 ); // Internal heat transfer coefficient [W/m^2.K] - EN673 and ISO 10292 procedure
		static Array1D< Real64 > Ra( maxlay+1, 0.0 ); // Vector of Rayleigh numbers, for each gap {maxlay}
		static Array1D< Real64 > Nu( maxlay+1, 0.0 ); // Vector of Nusselt numbers, for each gap {maxlay}
		static int nperr( 0 ); // Error code
		static Real64 ShadeEmisRatioOut( 0.0 ); // Ratio of modified to glass emissivity at the outermost glazing surface
		static Real64 ShadeEmisRatioIn( 0.0 ); // Ratio of modified to glass emissivity at the innermost glazing surface
		static Real64 ShadeHcRatioOut( 0.0 ); // Ratio of modified to unshaded Hc at the outermost glazing surface
		static Real64 ShadeHcRatioIn( 0.0 ); // Ratio of modified to unshaded Hc at the innermost glazing surface
		static Real64 HcUnshadedOut( 0.0 ); // Hc value at outdoor surface of an unshaded subsystem [W/m^2.K]
		static Real64 HcUnshadedIn( 0.0 ); // Hc value at indoor surface of an unshaded subsystem [W/m^2.K]
		static Array1D< Real64 > Keff( maxlay, 0.0 ); // Vector of keff values for gaps [W/m.K] {maxlay}
		static Array1D< Real64 > ShadeGapKeffConv( maxlay-1, 0.0 ); // Vector of convective keff values for areas above/below
		// SD layers [W/m.K] {maxlay-1}

		int ZoneNum; // Zone number corresponding to SurfNum

		static Array1D< Real64 > deltaTemp( 100, 0.0 );
		int i;
		static Array1D_int iMinDT( 1, 0 );
		static Array1D_int IDConst( 100, 0 );

		int TotLay; // Total number of layers in a construction
		//   (sum of solid layers and gap layers)
		int Lay; // Layer number
		int LayPtr; // Material number for a layer
		int ShadingLayPtr; // Shading layer pointer for effective temperature calculations
		int GlassLayPtr; // Glass layer pointer for effective temperature calculations
		Real64 EpsGlassIR;
		Real64 RhoGlassIR;
		Real64 TauShadeIR;
		Real64 EpsShadeIR;
		Real64 RhoShadeIR;
		int IGlass; // glass layer number (1,2,3,...)
		int IGap; // Gap layer number (1,2,...)
		int TotGlassLay; // Total number of glass layers in a construction
		int ZoneEquipConfigNum;
		int NodeNum;
		Real64 SumSysMCp; // Zone sum of air system MassFlowRate*Cp
		Real64 SumSysMCpT; // Zone sum of air system MassFlowRate*Cp*T
		Real64 MassFlowRate;
		Real64 NodeTemp;
		Real64 CpAir;
		Real64 RefAirTemp; // reference air temperatures
		int k; // Layer counter
		int SurfNumAdj; // An interzone surface's number in the adjacent zone
		int ZoneNumAdj; // An interzone surface's adjacent zone number
		int ShadeFlag; // Flag indicating whether shade or blind is on, and shade/blind position
		int IMix;
		Real64 EffShBlEmiss; // Effective interior shade or blind emissivity
		Real64 EffGlEmiss; // Effective inside glass emissivity when interior shade or blind

		Real64 IncidentSolar; // Solar incident on outside of window (W)
		Real64 ConvHeatFlowNatural; // Convective heat flow from gap between glass and interior shade or blind (W)
		Real64 ShadeArea; // shade/blind area (m2)
		Real64 sconsh; // shade/blind conductance (W/m2-K)
		Real64 CondHeatGainShade; // Conduction through shade/blind, outside to inside (W)

		Real64 ShGlReflFacIR; // Factor for long-wave inter-reflection between shade/blind and adjacent glass
//		Real64 RhoGlIR1; // Long-wave reflectance of glass surface facing shade/blind; 1=exterior shade/blind,
		Real64 RhoGlIR2;
		//  2=interior shade/blind
		Real64 RhoShIR1; // Long-wave reflectance of shade/blind surface facing glass; 1=interior shade/blind,
		Real64 RhoShIR2;
		//  2=exterior shade/blind
		Real64 EpsShIR1; // Long-wave emissivity of shade/blind surface facing glass; 1=interior shade/blind,
		Real64 EpsShIR2;
		//  2=exterior shade/blind
		Real64 TauShIR; // Long-wave transmittance of isolated shade/blind
		Real64 NetIRHeatGainShade; // Net IR heat gain to zone from interior shade/blind (W)
		Real64 NetIRHeatGainGlass; // Net IR heat gain to zone from shade/blind side of glass when interior
		//  shade/blind is present. Zero if shade/blind has zero IR transmittance (W)
		Real64 ConvHeatGainFrZoneSideOfShade; // Convective heat gain to zone from side of interior shade facing zone (W)
		Real64 ConvHeatGainFrZoneSideOfGlass; // Convective heat gain to zone from side of glass facing zone when
		//  no interior shade/blind is present (W)
		Real64 CondHeatGainGlass; // Conduction through inner glass layer, outside to inside (W)
		Real64 TotAirflowGap; // Total volumetric airflow through window gap (m3/s)
		Real64 TAirflowGapOutlet; // Temperature of air leaving airflow gap between glass panes (K)
		Real64 TAirflowGapOutletC; // Temperature of air leaving airflow gap between glass panes (C)
		Real64 ConvHeatFlowForced; // Convective heat flow from forced airflow gap (W)
		Real64 InletAirHumRat; // Humidity ratio of air from window gap entering fan
		Real64 ZoneTemp; // Zone air temperature (C)
		Real64 CpAirOutlet; // Heat capacity of air from window gap (J/kg-K)
		Real64 CpAirZone; // Heat capacity of zone air (J/kg-K)
		Real64 ConvHeatGainToZoneAir; // Convective heat gain to zone air from window gap airflow (W)
//		int ConstrNumSh; // Construction number with shading device
		Real64 TransDiff; // Diffuse shortwave transmittance
		static int CalcSHGC( 0 ); // SHGC calculations are not necessary for E+ run
		static int NumOfIterations( 0 );

		int GasType; // locally used coefficent to point at correct gas type
		int ICoeff;

		std::string tarcogErrorMessage; // store error text from tarcog

		//Simon: locally used variables
		int ngllayer;
		int nglface;
		int nglfacep;
		int TempInt;
		int PillarPtr;
		int DeflectionPtr;
		int GasPointer;
		int ThermalModelNum;
		Real64 rmir; // IR radiance of window's interior surround (W/m2)
		Real64 outir;
		Real64 Ebout;
		Real64 dominantGapWidth; // store value for dominant gap width.  Used for airflow calculations

		// fill local vars

		CalcDeflection = 0;
		CalcSHGC = 0;

		if ( CalcCondition == noCondition ) {
			ConstrNum = Surface( SurfNum ).Construction;
			SurfNumAdj = Surface( SurfNum ).ExtBoundCond;
			ShadeFlag = SurfaceWindow( SurfNum ).ShadingFlag;
		}

		TotGlassLay = Construct( ConstrNum ).TotGlassLayers;
		ngllayer = Construct( ConstrNum ).TotGlassLayers;
		nglface = 2 * ngllayer;
		nglfacep = nglface;
		hrin = 0.0;
		hcin = 0.0;
		hrout = 0.0;
		hcout = 0.0;

		Pa = OutBaroPress;

		ThermalModelNum = Construct( ConstrNum ).BSDFInput.ThermalModel;
		standard = WindowThermalModel( ThermalModelNum ).CalculationStandard;
		ThermalMod = WindowThermalModel( ThermalModelNum ).ThermalModel;
		CalcDeflection = WindowThermalModel( ThermalModelNum ).DeflectionModel;
		SDScalar = WindowThermalModel( ThermalModelNum ).SDScalar;
		VacuumPressure = WindowThermalModel( ThermalModelNum ).VacuumPressureLimit;
		Tini = WindowThermalModel( ThermalModelNum ).InitialTemperature - KelvinConv;
		Pini = WindowThermalModel( ThermalModelNum ).InitialPressure;

		nlayer = Construct( ConstrNum ).TotSolidLayers;
		isky = 3; // IR radiation is provided from external source
		iwd = 0; // assume windward for now.  TODO compare surface normal with wind direction

		if ( CalcCondition == noCondition ) {
			ZoneNum = Surface( SurfNum ).Zone;

			// determine reference air temperature for this surface
			{ auto const SELECT_CASE_var( Surface( SurfNum ).TAirRef );
			if ( SELECT_CASE_var == ZoneMeanAirTemp ) {
				RefAirTemp = MAT( ZoneNum );
			} else if ( SELECT_CASE_var == AdjacentAirTemp ) {
				RefAirTemp = TempEffBulkAir( SurfNum );
			} else if ( SELECT_CASE_var == ZoneSupplyAirTemp ) {
				// determine ZoneEquipConfigNum for this zone
				//            ControlledZoneAirFlag = .FALSE.
				ZoneEquipConfigNum = ZoneNum;
				//            DO ZoneEquipConfigNum = 1, NumOfControlledZones
				//                IF (ZoneEquipConfig(ZoneEquipConfigNum)%ActualZoneNum /= ZoneNum) CYCLE
				//                ControlledZoneAirFlag = .TRUE.
				//                EXIT
				//            END DO ! ZoneEquipConfigNum
				// check whether this zone is a controlled zone or not
				if ( ! Zone( ZoneNum ).IsControlled ) {
					ShowFatalError( "Zones must be controlled for Ceiling-Diffuser Convection model. No system serves zone " + Zone( ZoneNum ).Name );
					return;
				}
				// determine supply air conditions
				SumSysMCp = 0.0;
				SumSysMCpT = 0.0;
				for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( ZoneEquipConfigNum ).NumInletNodes; ++NodeNum ) {
					NodeTemp = Node( ZoneEquipConfig( ZoneEquipConfigNum ).InletNode( NodeNum ) ).Temp;
					MassFlowRate = Node( ZoneEquipConfig( ZoneEquipConfigNum ).InletNode( NodeNum ) ).MassFlowRate;
					CpAir = PsyCpAirFnWTdb( ZoneAirHumRat( ZoneNum ), NodeTemp );
					SumSysMCp += MassFlowRate * CpAir;
					SumSysMCpT += MassFlowRate * CpAir * NodeTemp;
				}
				// a weighted average of the inlet temperatures.
				RefAirTemp = SumSysMCpT / SumSysMCp;
			} else {
				// currently set to mean air temp but should add error warning here
				RefAirTemp = MAT( ZoneNum );
			}}

			tind = RefAirTemp + KelvinConv; // Inside air temperature

			// now get "outside" air temperature
			if ( SurfNumAdj > 0 ) { // Interzone window

				ZoneNumAdj = Surface( SurfNumAdj ).Zone;

				// determine reference air temperature for this surface
				{ auto const SELECT_CASE_var( Surface( SurfNumAdj ).TAirRef );
				if ( SELECT_CASE_var == ZoneMeanAirTemp ) {
					RefAirTemp = MAT( ZoneNumAdj );
				} else if ( SELECT_CASE_var == AdjacentAirTemp ) {
					RefAirTemp = TempEffBulkAir( SurfNumAdj );
				} else if ( SELECT_CASE_var == ZoneSupplyAirTemp ) {
					// determine ZoneEquipConfigNum for this zone
					ZoneEquipConfigNum = ZoneNum;
					// check whether this zone is a controlled zone or not
					if ( ! Zone( ZoneNum ).IsControlled ) {
						ShowFatalError( "Zones must be controlled for Ceiling-Diffuser Convection model. No system serves zone " + Zone( ZoneNum ).Name );
						return;
					}
					// determine supply air conditions
					SumSysMCp = 0.0;
					SumSysMCpT = 0.0;
					for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( ZoneEquipConfigNum ).NumInletNodes; ++NodeNum ) {
						NodeTemp = Node( ZoneEquipConfig( ZoneEquipConfigNum ).InletNode( NodeNum ) ).Temp;
						MassFlowRate = Node( ZoneEquipConfig( ZoneEquipConfigNum ).InletNode( NodeNum ) ).MassFlowRate;
						CpAir = PsyCpAirFnWTdb( ZoneAirHumRat( ZoneNumAdj ), NodeTemp );
						SumSysMCp += MassFlowRate * CpAir;
						SumSysMCpT += MassFlowRate * CpAir * NodeTemp;
					}
					// a weighted average of the inlet temperatures.
					RefAirTemp = SumSysMCpT / SumSysMCp;
				} else {
					// currently set to mean air temp but should add error warning here
					RefAirTemp = MAT( ZoneNumAdj );
				}}

				tout = RefAirTemp + KelvinConv; // outside air temperature

				tsky = MRT( ZoneNumAdj ) + KelvinConv; // TODO this misses IR from sources such as high temp radiant and baseboards

				//  ! Add long-wave radiation from adjacent zone absorbed by glass layer closest to the adjacent zone.
				//  AbsRadGlassFace(1) = AbsRadGlassFace(1) + QRadThermInAbs(SurfNumAdj)
				//  ! The IR radiance of this window's "exterior" surround is the IR radiance
				//  ! from surfaces and high-temp radiant sources in the adjacent zone
				outir = SurfaceWindow( SurfNumAdj ).IRfromParentZone + QHTRadSysSurf( SurfNumAdj ) + QHWBaseboardSurf( SurfNumAdj );

			} else { // Exterior window (ExtBoundCond = 0)

				if ( Surface( SurfNum ).ExtWind ) { // Window is exposed to wind (and possibly rain)
					if ( IsRain ) { // Raining: since wind exposed, outside window surface gets wet
						tout = Surface( SurfNum ).OutWetBulbTemp + KelvinConv;
					} else { // Dry
						tout = Surface( SurfNum ).OutDryBulbTemp + KelvinConv;
					}
				} else { // Window not exposed to wind
					tout = Surface( SurfNum ).OutDryBulbTemp + KelvinConv;
				}
				//tsky = SkyTemp + TKelvin
				tsky = SkyTempKelvin;
				Ebout = sigma * pow_4( tout );
				outir = Surface( SurfNum ).ViewFactorSkyIR * ( AirSkyRadSplit( SurfNum ) * sigma * pow_4( tsky ) + ( 1.0 - AirSkyRadSplit( SurfNum ) ) * Ebout ) + Surface( SurfNum ).ViewFactorGroundIR * Ebout;

			}

			hin = HConvIn( SurfNum ); // Room-side surface convective film conductance
			ibc( 2 ) = 0; // convective coefficient on indoor side will be recalculated (like in Winkelmann routines)

			//hcout=HextConvCoeff  ! Exterior convection coefficient is passed in from outer routine
			hout = HextConvCoeff; // Exterior convection coefficient is passed in from outer routine
			ibc( 1 ) = 2; // prescribed convective film coeff on outdoor side
			tilt = Surface( SurfNum ).Tilt;
			height = Surface( SurfNum ).Height;
			heightt = height; // for now put same window and glazing pocket hights
			width = Surface( SurfNum ).Width;

			//indoor mean radiant temperature.
			// IR incident on window from zone surfaces and high-temp radiant sources
			rmir = SurfaceWindow( SurfNum ).IRfromParentZone + QHTRadSysSurf( SurfNum ) + QHWBaseboardSurf( SurfNum ) + QSteamBaseboardSurf( SurfNum ) + QElecBaseboardSurf( SurfNum );
			trmin = root_4( rmir / StefanBoltzmann ); // TODO check model equation.

			// outdoor wind speed
			if ( ! Surface( SurfNum ).ExtWind ) {
				wso = 0.0; // No wind exposure
				//ELSE IF (Surface(SurfNum)%Class == SurfaceClass_Window .AND. SurfaceWindow(SurfNum)%ShadingFlag == ExtShadeOn) THEN
				//  wso =  0.0  ! Assume zero wind speed at outside glass surface of window with exterior shade
			} else {
				wso = Surface( SurfNum ).WindSpeed;
			}

			// indoor wind speed
			wsi = 0.0; // assumuption (TODO, what to use for inside air velocity?)

			fclr = 1.0 - CloudFraction;
		}

		//now fill layer data
		//IConst = ConstrNum
		//IF(ShadeFlag==IntShadeOn.OR.ShadeFlag==ExtShadeOn.OR.ShadeFlag==IntBlindOn.OR.ShadeFlag==ExtBlindOn &
		//    .OR.ShadeFlag==BGShadeOn.OR.ShadeFlag==BGBlindOn.OR.ShadeFlag==ExtScreenOn) THEN
		//  IConst = Surface(SurfNum)%ShadedConstruction
		//  IF(Surfacewindow(SurfNum)%StormWinFlag > 0) IConst = Surface(SurfNum)%StormWinShadedConstruction
		//END IF
		//TotLay = Construct(IConst)%TotLayers
		TotLay = Construct( ConstrNum ).TotLayers;
		IGap = 0;

		//****************************************************************************************************
		// Inside and outside gas coefficients
		//****************************************************************************************************
		iprop( 1, 1 ) = 1; // air on outdoor side
		frct( 1, 1 ) = 1.0; // pure air on outdoor side
		nmix( 1 ) = 1; // pure air on outdoor side

		iprop( 1, nlayer + 1 ) = 1; // air on indoor side
		frct( 1, nlayer + 1 ) = 1.0; // pure air on indoor side
		nmix( nlayer + 1 ) = 1; // pure air on indoor side

		//Simon: feed gas coefficients with air.  This is necessary for tarcog because it is used on indoor and outdoor sides
		GasType = GasCoeffsAir;
		wght( iprop( 1, 1 ) ) = GasWght( GasType );
		gama( iprop( 1, 1 ) ) = GasSpecificHeatRatio( GasType );
		for ( ICoeff = 1; ICoeff <= 3; ++ICoeff ) {
			gcon( ICoeff, iprop( 1, 1 ) ) = GasCoeffsCon( ICoeff, GasType );
			gvis( ICoeff, iprop( 1, 1 ) ) = GasCoeffsVis( ICoeff, GasType );
			gcp( ICoeff, iprop( 1, 1 ) ) = GasCoeffsCp( ICoeff, GasType );
		}

		// Fill window layer properties needed for window layer heat balance calculation
		IGlass = 0;
		IGap = 0;
		for ( Lay = 1; Lay <= TotLay; ++Lay ) {
			LayPtr = Construct( ConstrNum ).LayerPoint( Lay );

			if ( ( Material( LayPtr ).Group == WindowGlass ) || ( Material( LayPtr ).Group == WindowSimpleGlazing ) ) {
				++IGlass;
				LayerType( IGlass ) = 0; // this marks specular layer type
				thick( IGlass ) = Material( LayPtr ).Thickness;
				scon( IGlass ) = Material( LayPtr ).Conductivity;
				emis( 2 * IGlass - 1 ) = Material( LayPtr ).AbsorpThermalFront;
				emis( 2 * IGlass ) = Material( LayPtr ).AbsorpThermalBack;
				tir( 2 * IGlass - 1 ) = Material( LayPtr ).TransThermal;
				tir( 2 * IGlass ) = Material( LayPtr ).TransThermal;
				YoungsMod( IGlass ) = Material( LayPtr ).YoungModulus;
				PoissonsRat( IGlass ) = Material( LayPtr ).PoissonsRatio;
			} else if ( Material( LayPtr ).Group == ComplexWindowShade ) {
				if ( CalcCondition == noCondition ) {
					if ( Lay == 1 ) SurfaceWindow( SurfNum ).ShadingFlag = ExtShadeOn;
					if ( Lay == TotLay ) SurfaceWindow( SurfNum ).ShadingFlag = IntShadeOn;
				}
				++IGlass;
				TempInt = Material( LayPtr ).ComplexShadePtr;
				LayerType( IGlass ) = ComplexShade( TempInt ).LayerType;

				thick( IGlass ) = ComplexShade( TempInt ).Thickness;
				scon( IGlass ) = ComplexShade( TempInt ).Conductivity;
				emis( 2 * IGlass - 1 ) = ComplexShade( TempInt ).FrontEmissivity;
				emis( 2 * IGlass ) = ComplexShade( TempInt ).BackEmissivity;
				tir( 2 * IGlass - 1 ) = ComplexShade( TempInt ).IRTransmittance;
				tir( 2 * IGlass ) = ComplexShade( TempInt ).IRTransmittance;

				// This needs to be converted into correct areas. That can be done only after loading complete window data
				Atop( IGlass ) = ComplexShade( TempInt ).TopOpeningMultiplier;
				Abot( IGlass ) = ComplexShade( TempInt ).BottomOpeningMultiplier;
				Al( IGlass ) = ComplexShade( TempInt ).LeftOpeningMultiplier;
				Ar( IGlass ) = ComplexShade( TempInt ).RightOpeningMultiplier;
				Ah( IGlass ) = ComplexShade( TempInt ).FrontOpeningMultiplier;

				SlatThick( IGlass ) = ComplexShade( TempInt ).SlatThickness;
				SlatWidth( IGlass ) = ComplexShade( TempInt ).SlatWidth;
				SlatAngle( IGlass ) = ComplexShade( TempInt ).SlatAngle;
				SlatCond( IGlass ) = ComplexShade( TempInt ).SlatConductivity;
				SlatSpacing( IGlass ) = ComplexShade( TempInt ).SlatSpacing;
				SlatCurve( IGlass ) = ComplexShade( TempInt ).SlatCurve;
			} else if ( Material( LayPtr ).Group == ComplexWindowGap ) {
				++IGap;
				gap( IGap ) = Material( LayPtr ).Thickness;
				presure( IGap ) = Material( LayPtr ).Pressure;

				DeflectionPtr = Material( LayPtr ).DeflectionStatePtr;
				if ( DeflectionPtr != 0 ) {
					GapDefMax( IGap ) = DeflectionState( DeflectionPtr ).DeflectedThickness;
				} else {
					GapDefMax( IGap ) = gap( IGap );
				}

				PillarPtr = Material( LayPtr ).SupportPillarPtr;

				if ( PillarPtr != 0 ) {
					SupportPlr( IGap ) = 1;
					PillarSpacing( IGap ) = SupportPillar( PillarPtr ).Spacing;
					PillarRadius( IGap ) = SupportPillar( PillarPtr ).Radius;
				}

				GasPointer = Material( LayPtr ).GasPointer;

				nmix( IGap + 1 ) = Material( GasPointer ).NumberOfGasesInMixture;
				for ( IMix = 1; IMix <= nmix( IGap + 1 ); ++IMix ) {
					//iprop(IGap+1, IMix) = Material(LayPtr)%GasType(IMix)
					//iprop(IGap+1, IMix) = GetGasIndex(Material(LayPtr)%GasWght(IMix))
					frct( IMix, IGap + 1 ) = Material( GasPointer ).GasFract( IMix );

					//Now has to build-up gas coefficients arrays. All used gasses should be stored into these arrays and
					//to be correctly referenced by gap arrays

					//First check if gas coefficients are already part of array.  Duplicates are not necessary
					CheckGasCoefs( Material( GasPointer ).GasWght( IMix ), iprop( IMix, IGap + 1 ), wght, feedData );
					if ( feedData ) {
						wght( iprop( IMix, IGap + 1 ) ) = Material( GasPointer ).GasWght( IMix );
						gama( iprop( IMix, IGap + 1 ) ) = Material( GasPointer ).GasSpecHeatRatio( IMix );
						for ( i = 1; i <= 3; ++i ) {
							gcon( i, iprop( IMix, IGap + 1 ) ) = Material( GasPointer ).GasCon( i, IMix );
							gvis( i, iprop( IMix, IGap + 1 ) ) = Material( GasPointer ).GasVis( i, IMix );
							gcp( i, iprop( IMix, IGap + 1 ) ) = Material( GasPointer ).GasCp( i, IMix );
						}
					} //IF feedData THEN
				}
			} else {
				ShowContinueError( "Illegal layer type in Construction:ComplexFenestrationState." );
				ShowContinueError( "Allowed object are:" );
				ShowContinueError( "   - WindowMaterial:Glazing" );
				ShowContinueError( "   - WindowMaterial:ComplexShade" );
				ShowContinueError( "   - WindowMaterial:Gap" );
				ShowFatalError( "halting because of error in layer definition for Construction:ComplexFenestrationState" );
			}

		} // End of loop over glass, gap and blind/shade layers in a window construction

		if ( CalcCondition == noCondition ) {
			// now calculate correct areas for multipliers
			for ( Lay = 1; Lay <= nlayer; ++Lay ) {
				if ( LayerType( Lay ) != 0 ) { // Layer is shading
					// before changing multipliers, need to determine which one is dominant gap width
					if ( Lay == 1 ) { // Exterior shading device
						dominantGapWidth = gap( Lay );
					} else if ( Lay == nlayer ) { // Interior shading device
						dominantGapWidth = gap( Lay - 1 );
					} else { // In-between shading device
						dominantGapWidth = min( gap( Lay - 1 ), gap( Lay ) );
					}
					Atop( Lay ) *= dominantGapWidth * width;
					Abot( Lay ) *= dominantGapWidth * width;
					Al( Lay ) *= dominantGapWidth * height;
					Ar( Lay ) *= dominantGapWidth * height;
					Ah( Lay ) *= width * height;
				}
			}
		}

		//ThermalMod = 0
		Debug_mode = 0;
		CalcSHGC = 0;

		Window_ID = ConstrNum;

		//vector of absorbed solar energy fractions for each layer.
		asol = 0.0;
		// direct solar radiation
		if ( CalcCondition == noCondition ) {
			ShadeFlag = SurfaceWindow( SurfNum ).ShadingFlag;
			dir = QRadSWOutIncident( SurfNum ) + QS( Surface( SurfNum ).Zone ); // TODO, check , !
			//                  currently using Exterior beam plus diffuse solar incident on surface
			//                  plus zone short wave.  CHECK
			//if (dir.ne.0.0d0) then
			for ( IGlass = 1; IGlass <= nlayer; ++IGlass ) {
				//IF (dir > 0.0D0 ) THEN
				asol( IGlass ) = QRadSWwinAbs( IGlass, SurfNum );
				//ELSE
				//  asol(IGLASS) = 0.0D0
				//ENDIF
			}
			//end if

			// Add contribution of IR from zone internal gains (lights, equipment and people). This is absorbed in zone-side layer and it
			// is assumed that nothing is transmitted through
			asol( nlayer ) += QRadThermInAbs( SurfNum );

			presure = OutBaroPress;

			// Instead of doing temperature guess get solution from previous iteration.  That should be much better than guess
			for ( k = 1; k <= 2 * nlayer; ++k ) {
				theta( k ) = SurfaceWindow( SurfNum ).ThetaFace( k );
			}

		}

		// Standard conditions run (winter and summer)
		if ( CalcCondition == winterCondition ) {
			tind = 294.15;
			tout = 255.15;
			hcout = 26.0;
			wso = 5.5;
			dir = 0.0;
		} else if ( CalcCondition == summerCondition ) {
			tind = 297.15;
			tout = 305.15;
			hcout = 15.0;
			wso = 2.75;
			dir = 783.0;
			CalcSHGC = 1;
		}

		// Common condition data
		if ( CalcCondition != noCondition ) {
			trmin = tind;
			outir = 0.0;
			tsky = tout;
			wsi = 0.0;
			fclr = 1.0;
			ibc( 1 ) = 0;
			ibc( 2 ) = 0;
			presure = 101325.0;
			iwd = 0; // Windward wind direction
			isky = 0;
			esky = 1.0;
			height = 1.0;
			heightt = 1.0;
			width = 1.0;
			tilt = 90.0;
			// Just to make initial quess different from absolute zero
			theta = 273.15;
		}

		//  call TARCOG
		TARCOG90( nlayer, iwd, tout, tind, trmin, wso, wsi, dir, outir, isky, tsky, esky, fclr, VacuumPressure, VacuumMaxGapThickness, CalcDeflection, Pa, Pini, Tini, gap, GapDefMax, thick, scon, YoungsMod, PoissonsRat, tir, emis, totsol, tilt, asol, height, heightt, width, presure, iprop, frct, gcon, gvis, gcp, wght, gama, nmix, SupportPlr, PillarSpacing, PillarRadius, theta, LayerDef, q, qv, ufactor, sc, hflux, hcin, hcout, hrin, hrout, hin, hout, hcgap, hrgap, shgc, nperr, tarcogErrorMessage, shgct, tamb, troom, ibc, Atop, Abot, Al, Ar, Ah, SlatThick, SlatWidth, SlatAngle, SlatCond, SlatSpacing, SlatCurve, vvent, tvent, LayerType, nslice, LaminateA, LaminateB, sumsol, hg, hr, hs, he, hi, Ra, Nu, standard, ThermalMod, Debug_mode, Debug_dir, Debug_file, Window_ID, IGU_ID, ShadeEmisRatioOut, ShadeEmisRatioIn, ShadeHcRatioOut, ShadeHcRatioIn, HcUnshadedOut, HcUnshadedIn, Keff, ShadeGapKeffConv, SDScalar, CalcSHGC, NumOfIterations );

		// process results from TARCOG
		if ( ( nperr > 0 ) && ( nperr < 1000 ) ) { // process error signal from tarcog

			ShowSevereError( "Window tarcog returned an error" );
			tarcogErrorMessage = "message = \"" + tarcogErrorMessage + "\"";
			ShowContinueErrorTimeStamp( tarcogErrorMessage );
			if ( CalcCondition == noCondition ) {
				ShowContinueError( "surface name = " + Surface( SurfNum ).Name );
			}
			ShowContinueError( "construction name = " + Construct( ConstrNum ).Name );
			ShowFatalError( "halting because of error in tarcog" );
		} else if ( CalcCondition == winterCondition ) {
			NominalU( ConstrNum ) = ufactor;
		} else if ( CalcCondition == summerCondition ) {
			//tempInt = SurfaceWindow(SurfNum)%ComplexFen%CurrentState
			//tempReal = SurfaceWindow(SurfNum)%ComplexFen%State(tempInt)%WinDiffTrans

			//Sum1 = 0.0d0
			//Sum2 = 0.0d0
			//do  j = 1 , ComplexWind(SurfNum)%Geom%Inc%NBasis     !Incident ray loop
			//  Sum2 = Sum2 + ComplexWind(SurfNum)%Geom%Inc%Lamda (j)
			//  do  m = 1 , ComplexWind(SurfNum)%Geom%Trn%NBasis     !Outgoing ray loop
			//    Sum1 =Sum1 + ComplexWind(SurfNum)%Geom%Inc%Lamda(j) * ComplexWind(SurfNum)%Geom%Trn%Lamda(m) * &
			//      & Construct(ConstrNum)%BSDFInput%SolFrtTrans ( j , m )
			//  end do      !Outgoing ray loop
			//end do      !Incident ray loop
			//if (Sum2 > 0.0d0) THEN
			//  tempReal = Sum1/Sum2
			//else
			//  tempReal = 0.0d0
			//  CALL ShowWarningError('BSDF--Inc basis has zero projected solid angle')
			//endif

			Construct( ConstrNum ).SummerSHGC = shgc;

			//Construct(SurfNum)%VisTransNorm = SurfaceWindow(SurfNum)%ComplexFen%State(tempInt)%WinDiffVisTrans
		} else if ( CalcCondition == noCondition ) { // expect converged results...
			// Window heat balance solution has converged.

			SurfaceWindow( SurfNum ).WindowCalcIterationsRep = NumOfIterations;
			HConvIn( SurfNum ) = hcin;

			// For interior shade, add convective gain from glass/shade gap air flow to zone convective gain;
			// For all cases, get total window heat gain for reporting. See CalcWinFrameAndDividerTemps for
			// contribution of frame and divider.

			SurfInsideTemp = theta( 2 * nlayer ) - KelvinConv;
			SurfOutsideTemp = theta( 1 ) - KelvinConv;
			SurfOutsideEmiss = emis( 1 );

			IncidentSolar = Surface( SurfNum ).Area * QRadSWOutIncident( SurfNum );
			if ( ShadeFlag == IntShadeOn || ShadeFlag == IntBlindOn ) {
				// Interior shade or blind
				ConvHeatFlowNatural = -qv( nlayer ) * height * width;

				SurfaceWindow( SurfNum ).ConvHeatFlowNatural = ConvHeatFlowNatural;
				WinGapConvHtFlowRep( SurfNum ) = ConvHeatFlowNatural;
				WinGapConvHtFlowRepEnergy( SurfNum ) = WinGapConvHtFlowRep( SurfNum ) * TimeStepZoneSec;
				// Window heat gain from glazing and shade/blind to zone. Consists of transmitted solar, convection
				//   from air exiting gap, convection from zone-side of shade/blind, net IR to zone from shade and net IR to
				//   zone from the glass adjacent to the shade/blind (zero if shade/blind IR transmittance is zero).
				// Following assumes glazed area = window area (i.e., dividers ignored) in calculating
				//   IR to zone from glass when interior shade/blind is present.
				ShadeArea = Surface( SurfNum ).Area + SurfaceWindow( SurfNum ).DividerArea;
				sconsh = scon( ngllayer + 1 ) / thick( ngllayer + 1 );
				nglfacep = nglface + 2;
				CondHeatGainShade = ShadeArea * sconsh * ( theta( nglfacep - 1 ) - theta( nglfacep ) );
				EpsShIR1 = emis( nglface + 1 );
				EpsShIR2 = emis( nglface + 2 );
				TauShIR = tir( nglface + 1 );
				RhoShIR1 = max( 0.0, 1.0 - TauShIR - EpsShIR1 );
				RhoShIR2 = max( 0.0, 1.0 - TauShIR - EpsShIR2 );
				RhoGlIR2 = 1.0 - emis( 2 * ngllayer );
				ShGlReflFacIR = 1.0 - RhoGlIR2 * RhoShIR1;
				NetIRHeatGainShade = ShadeArea * EpsShIR2 * ( sigma * pow_4( theta( nglfacep ) ) - rmir ) + EpsShIR1 * ( sigma * pow_4( theta( nglfacep - 1 ) ) - rmir ) * RhoGlIR2 * TauShIR / ShGlReflFacIR;
				NetIRHeatGainGlass = ShadeArea * ( emis( 2 * ngllayer ) * TauShIR / ShGlReflFacIR ) * ( sigma * pow_4( theta( 2 * ngllayer ) ) - rmir );
				ConvHeatGainFrZoneSideOfShade = ShadeArea * hcin * ( theta( nglfacep ) - tind );
				WinHeatGain( SurfNum ) = WinTransSolar( SurfNum ) + ConvHeatFlowNatural + ConvHeatGainFrZoneSideOfShade + NetIRHeatGainGlass + NetIRHeatGainShade;
				// store components for reporting
				WinGainConvGlazShadGapToZoneRep( SurfNum ) = ConvHeatFlowNatural; // result is in [W]
				WinGainConvShadeToZoneRep( SurfNum ) = ConvHeatGainFrZoneSideOfShade;
				WinGainIRGlazToZoneRep( SurfNum ) = NetIRHeatGainGlass;
				WinGainIRShadeToZoneRep( SurfNum ) = NetIRHeatGainShade;
			} else {
				// Interior shade or blind not present; innermost layer is glass
				CondHeatGainGlass = Surface( SurfNum ).Area * scon( nlayer ) / thick( nlayer ) * ( theta( 2 * nlayer - 1 ) - theta( 2 * nlayer ) );
				NetIRHeatGainGlass = Surface( SurfNum ).Area * emis( 2 * nlayer ) * ( sigma * pow_4( theta( 2 * nlayer ) ) - rmir );
				ConvHeatGainFrZoneSideOfGlass = Surface( SurfNum ).Area * hcin * ( theta( 2 * nlayer ) - tind );
				WinHeatGain( SurfNum ) = WinTransSolar( SurfNum ) + ConvHeatGainFrZoneSideOfGlass + NetIRHeatGainGlass;
				// store components for reporting
				WinGainConvGlazToZoneRep( SurfNum ) = ConvHeatGainFrZoneSideOfGlass;
				WinGainIRGlazToZoneRep( SurfNum ) = NetIRHeatGainGlass;
				// need to report convective heat flow from the gap in case of exterior shade
				if ( ShadeFlag == ExtShadeOn ) {
					ConvHeatFlowNatural = -qv( 2 ) * height * width; // qv(1) is exterior environment

					WinGapConvHtFlowRep( SurfNum ) = ConvHeatFlowNatural;
					WinGapConvHtFlowRepEnergy( SurfNum ) = WinGapConvHtFlowRep( SurfNum ) * TimeStepZoneSec;
				}
			}

			// Add convective heat gain from airflow window
			// Note: effect of fan heat on gap outlet temperature is neglected since fan power (based
			// on pressure drop through the gap) is extremely small

			//WinGapConvHtFlowRep(SurfNum) = 0.0d0
			//WinGapConvHtFlowRepEnergy(SurfNum) = 0.0d0
			TotAirflowGap = SurfaceWindow( SurfNum ).AirflowThisTS * Surface( SurfNum ).Width;
			TAirflowGapOutlet = KelvinConv; // TODO Need to calculate this
			TAirflowGapOutletC = TAirflowGapOutlet - KelvinConv;
			SurfaceWindow( SurfNum ).TAirflowGapOutlet = TAirflowGapOutletC;
			if ( SurfaceWindow( SurfNum ).AirflowThisTS > 0.0 ) {
				ConvHeatFlowForced = sum( qv ); // TODO.  figure forced ventilation heat flow in Watts

				WinGapConvHtFlowRep( SurfNum ) = ConvHeatFlowForced;
				WinGapConvHtFlowRepEnergy( SurfNum ) = WinGapConvHtFlowRep( SurfNum ) * TimeStepZoneSec;
				// Add heat from gap airflow to zone air if destination is inside air; save the heat gain to return
				// air in case it needs to be sent to the zone (due to no return air determined in HVAC simulation)
				if ( SurfaceWindow( SurfNum ).AirflowDestination == AirFlowWindow_Destination_IndoorAir || SurfaceWindow( SurfNum ).AirflowDestination == AirFlowWindow_Destination_ReturnAir ) {
					if ( SurfaceWindow( SurfNum ).AirflowSource == AirFlowWindow_Source_IndoorAir ) {
						InletAirHumRat = ZoneAirHumRat( ZoneNum );
					} else { // AirflowSource = outside air
						InletAirHumRat = OutHumRat;
					}
					ZoneTemp = MAT( ZoneNum ); // this should be Tin (account for different reference temps)
					CpAirOutlet = PsyCpAirFnWTdb( InletAirHumRat, TAirflowGapOutletC );
					CpAirZone = PsyCpAirFnWTdb( ZoneAirHumRat( ZoneNum ), ZoneTemp );
					ConvHeatGainToZoneAir = TotAirflowGap * ( CpAirOutlet * ( TAirflowGapOutletC ) - CpAirZone * ZoneTemp );
					if ( SurfaceWindow( SurfNum ).AirflowDestination == AirFlowWindow_Destination_IndoorAir ) {
						SurfaceWindow( SurfNum ).ConvHeatGainToZoneAir = ConvHeatGainToZoneAir;
						WinHeatGain( SurfNum ) += ConvHeatGainToZoneAir;
					} else {
						SurfaceWindow( SurfNum ).RetHeatGainToZoneAir = ConvHeatGainToZoneAir;
					}
				}
				// For AirflowDestination = ReturnAir in a controlled (i.e., conditioned) zone with return air, see CalcZoneLeavingConditions
				// for calculation of modification of return-air temperature due to airflow from window gaps into return air.
			}

			// Correct WinHeatGain for interior diffuse shortwave (solar and shortwave from lights) transmitted
			// back out window
			ConstrNum = Surface( SurfNum ).Construction;
			//ConstrNumSh = Surface(SurfNum)%ShadedConstruction
			//IF(SurfaceWindow(SurfNum)%StormWinFlag==1) THEN
			//  ConstrNum = Surface(SurfNum)%StormWinConstruction
			//  ConstrNumSh = Surface(SurfNum)%StormWinShadedConstruction
			//END IF
			//IF(ShadeFlag <= 0) THEN
			TransDiff = Construct( ConstrNum ).TransDiff;
			//ELSE IF(ShadeFlag==IntShadeOn .OR. ShadeFlag==ExtShadeOn) THEN
			//  TransDiff = Construct(ConstrNum)%TransDiff
			//ELSE IF(ShadeFlag==IntBlindOn .OR. ShadeFlag==ExtBlindOn .OR.ShadeFlag==BGBlindOn) THEN
			//  TransDiff = InterpSlatAng(SurfaceWindow(SurfNum)%SlatAngThisTS,SurfaceWindow(SurfNum)%MovableSlats, &
			//                             Construct(ConstrNumSh)%BlTransDiff)
			//ELSE IF(ShadeFlag == SwitchableGlazing) THEN
			//  TransDiff = InterpSW(SurfaceWindow(SurfNum)%SwitchingFactor,Construct(ConstrNum)%TransDiff, &
			//                             Construct(ConstrNumSh)%TransDiff)
			//END IF
			WinHeatGain( SurfNum ) -= QS( Surface( SurfNum ).Zone ) * Surface( SurfNum ).Area * TransDiff;
			WinLossSWZoneToOutWinRep( SurfNum ) = QS( Surface( SurfNum ).Zone ) * Surface( SurfNum ).Area * TransDiff;

			if ( ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn ) {
				WinShadingAbsorbedSolar( SurfNum ) = ( SurfaceWindow( SurfNum ).ExtBeamAbsByShade + SurfaceWindow( SurfNum ).ExtDiffAbsByShade ) * ( Surface( SurfNum ).Area + SurfaceWindow( SurfNum ).DividerArea );
				WinShadingAbsorbedSolarEnergy( SurfNum ) = WinShadingAbsorbedSolar( SurfNum ) * TimeStepZoneSec;
			}
			if ( SunIsUp ) {
				WinSysSolTransmittance( SurfNum ) = WinTransSolar( SurfNum ) / ( QRadSWOutIncident( SurfNum ) * ( Surface( SurfNum ).Area + SurfaceWindow( SurfNum ).DividerArea ) + 0.0001 );
				WinSysSolAbsorptance( SurfNum ) = ( QRadSWwinAbsTot( SurfNum ) + WinShadingAbsorbedSolar( SurfNum ) ) / ( QRadSWOutIncident( SurfNum ) * ( Surface( SurfNum ).Area + SurfaceWindow( SurfNum ).DividerArea ) + 0.0001 );
				WinSysSolReflectance( SurfNum ) = 1.0 - WinSysSolTransmittance( SurfNum ) - WinSysSolAbsorptance( SurfNum );
			} else {
				WinSysSolTransmittance( SurfNum ) = 0.0;
				WinSysSolAbsorptance( SurfNum ) = 0.0;
				WinSysSolReflectance( SurfNum ) = 0.0;
			}

			// Save hcv for use in divider calc with interior or exterior shade (see CalcWinFrameAndDividerTemps)
			if ( ShadeFlag == IntShadeOn ) SurfaceWindow( SurfNum ).ConvCoeffWithShade = 0.0;

			if ( ShadeFlag == IntShadeOn ) {
				SurfInsideTemp = theta( 2 * ngllayer + 2 ) - KelvinConv;

				// Get properties of inside shading layer
				ShadingLayPtr = Construct( ConstrNum ).LayerPoint( TotLay );
				ShadingLayPtr = Material( ShadingLayPtr ).ComplexShadePtr;
				TauShadeIR = ComplexShade( ShadingLayPtr ).IRTransmittance;
				EpsShadeIR = ComplexShade( ShadingLayPtr ).FrontEmissivity;
				RhoShadeIR = max( 0.0, 1.0 - TauShadeIR - EpsShadeIR );

				// Get properties of glass next to inside shading layer
				GlassLayPtr = Construct( ConstrNum ).LayerPoint( TotLay - 2 );
				EpsGlassIR = Material( GlassLayPtr ).AbsorpThermalBack;
				RhoGlassIR = 1 - EpsGlassIR;

				EffShBlEmiss = EpsShadeIR * ( 1.0 + RhoGlassIR * TauShadeIR / ( 1.0 - RhoGlassIR * RhoShadeIR ) );
				SurfaceWindow( SurfNum ).EffShBlindEmiss = EffShBlEmiss;
				EffGlEmiss = EpsGlassIR * TauShadeIR / ( 1.0 - RhoGlassIR * RhoShadeIR );
				SurfaceWindow( SurfNum ).EffGlassEmiss = EffGlEmiss;
				//  EffShBlEmiss = InterpSlatAng(SurfaceWindow(SurfNum)%SlatAngThisTS,SurfaceWindow(SurfNum)%MovableSlats, &
				//                    SurfaceWindow(SurfNum)%EffShBlindEmiss)
				//  EffGlEmiss   = InterpSlatAng(SurfaceWindow(SurfNum)%SlatAngThisTS,SurfaceWindow(SurfNum)%MovableSlats, &
				//                    SurfaceWindow(SurfNum)%EffGlassEmiss)
				SurfaceWindow( SurfNum ).EffInsSurfTemp = ( EffShBlEmiss * SurfInsideTemp + EffGlEmiss * ( theta( 2 * ngllayer ) - KelvinConv ) ) / ( EffShBlEmiss + EffGlEmiss );
				//ELSE
				//  SurfInsideTemp = theta(2*ngllayer) - TKelvin
				//END IF
				//IF(ShadeFlag == ExtShadeOn .OR. ShadeFlag == ExtBlindOn .OR. ShadeFlag == ExtScreenOn) THEN
				//SurfOutsideTemp = theta(2*ngllayer+1) - TKelvin  !this looks wrong.
			} else {
				SurfOutsideTemp = theta( 1 ) - KelvinConv;
			}

			for ( k = 1; k <= nlayer; ++k ) {
				SurfaceWindow( SurfNum ).ThetaFace( 2 * k - 1 ) = theta( 2 * k - 1 );
				SurfaceWindow( SurfNum ).ThetaFace( 2 * k ) = theta( 2 * k );

				// temperatures for reporting
				FenLaySurfTempFront( k, SurfNum ) = theta( 2 * k - 1 ) - KelvinConv;
				FenLaySurfTempBack( k, SurfNum ) = theta( 2 * k ) - KelvinConv;
				//thetas(k) = theta(k)
			}

		}

	}

	// This function check if gas with molecular weight has already been feed into coefficients and
	// feed arrays

	void
	CheckGasCoefs(
		Real64 const currentWeight,
		int & indexNumber,
		Array1A< Real64 > wght,
		bool & feedData
	)
	{

		// Using/Aliasing
		using namespace DataPrecisionGlobals;
		using TARCOGGassesParams::maxgas;

		// Argument array dimensioning
		wght.dim( maxgas );

		// Locals
		//Local variables
		static int counter( 1 );
		static bool coeffFound( false );

		feedData = false;
		coeffFound = false;
		counter = 1;
		while ( ( counter <= maxgas ) && ( wght( counter ) != 0 ) && ( ! coeffFound ) ) {
			if ( std::abs( currentWeight - wght( counter ) ) < 1.0e-5 ) {
				coeffFound = true;
			} else {
				++counter;
			}
		} //DO WHILE((counter.LE.maxgas).AND.(wght(couner).NE.0).AND.(.NOT.coeffFound))

		// In case coefficient is not found data needs to be stored in gas coefficients arrays
		if ( ( ! coeffFound ) && ( counter < maxgas ) ) {
			feedData = true;
		}

		indexNumber = counter;

	}

	int
	SearchAscTable(
		Real64 const y, // Value to be found in the table
		int const n, // Number of values in the table
		Array1S< Real64 > const ytab // Table of values, monotonic, ascending order
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Joe Klems
		//       DATE WRITTEN   Feb 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Given an ascending monotonic table with n entries, find  an index i
		// such that ytab(i-1) < y <= ytab(i)

		// METHODOLOGY EMPLOYED:
		// binary search

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int SearchAscTable;

		// Argument array dimensioning

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int Ih; // Intex for upper end of interval
		int Il; // Index for lower end of interval
		int Im; // Index for midpoint of interval
		Real64 Yh; // Table value for upper end of interval
		Real64 Yl; // Table value for lower end of interval
		Real64 Ym; // Table value for midpoint of interval

		Yh = ytab( n );
		Yl = ytab( 1 );
		Ih = n;
		Il = 1;
		if ( y < Yl ) {
			SearchAscTable = 1;
			return SearchAscTable;
		} else if ( y > Yh ) {
			SearchAscTable = n;
			return SearchAscTable;
		}
		while ( true ) {
			if ( Ih - Il <= 1 ) break;
			Im = ( Ih + Il ) / 2;
			Ym = ytab( Im );
			if ( y <= Ym ) {
				Yh = Ym;
				Ih = Im;
			} else {
				Yl = Ym;
				Il = Im;
			}

		}

		SearchAscTable = Ih;

		return SearchAscTable;

	}

} // WindowComplexManager

} // EnergyPlus
