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

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/member.functions.hh>
#include <ObjexxFCL/string.functions.hh>
#include <ObjexxFCL/Vector3.hh>

// EnergyPlus Headers
#include <CommandLineInterface.hh>
#include <SolarShading.hh>
#include <DataDaylighting.hh>
#include <DataDaylightingDevices.hh>
#include <DataEnvironment.hh>
#include <DataErrorTracking.hh>
#include <DataGlobals.hh>
#include <DataStringGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalSurface.hh>
#include <DataIPShortCuts.hh>
#include <DataPrecisionGlobals.hh>
#include <DataReportingFlags.hh>
#include <DataShadowingCombinations.hh>
#include <DataSurfaces.hh>
#include <DataSystemVariables.hh>
#include <DataTimings.hh>
#include <DataViewFactorInformation.hh>
#include <DataWindowEquivalentLayer.hh>
#include <DaylightingDevices.hh>
#include <DaylightingManager.hh>
#include <DisplayRoutines.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <ScheduleManager.hh>
#include <SolarReflectionManager.hh>
#include <UtilityRoutines.hh>
#include <Vectors.hh>
#include <WindowComplexManager.hh>
#include <WindowEquivalentLayer.hh>

namespace EnergyPlus {

namespace SolarShading {

	// MODULE INFORMATION:
	//       AUTHOR         Rick Strand
	//       DATE WRITTEN   March 1997
	//       MODIFIED       December 1998, FCW
	//       MODIFIED       July 1999, Linda Lawrie, eliminate shadefl.scr,
	//                      do shadowing calculations during simulation
	//       MODIFIED       June 2001, FCW, handle window blinds
	//       MODIFIED       May 2004, LKL, Polygons > 4 sides (not subsurfaces)
	//       MODIFIED       January 2007, LKL, Taking parameters back to original integer (HC)
	//       MODIFIED       August 2011, JHK, Including Complex Fenestration optical calculations
	//       MODIFIED       November 2012, BG, Timestep solar and daylighting calculations
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// The purpose of this module is to encompass the routines and data
	// which are need to perform the solar calculations in EnergyPlus.
	// This also requires that shading and geometry routines and data
	// which are used by the solar calculations be included in this module.

	// METHODOLOGY EMPLOYED:
	// Many of the methods used in this module have been carried over from the
	// (I)BLAST program.  As such, there is not much documentation on the
	// methodology used.  The original code was written mainly by George
	// Walton and requires coordinate transformations.  It calculates
	// shading using an overlapping polygon approach.

	// REFERENCES:
	// TARP Manual, NIST Publication.
	// Passive Solar Extension of the BLAST Program, CERL/UIUC Publication.

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGlobals;
	using namespace DataEnvironment;
	using namespace DataHeatBalance;
	using namespace DataSurfaces;
	using namespace DataShadowingCombinations;
	using DaylightingManager::ProfileAngle;
	using namespace SolarReflectionManager;
	using namespace DataReportingFlags;
	using DataBSDFWindow::SUNCOSTS;
	using DataBSDFWindow::MaxBkSurf;
	using DataBSDFWindow::ComplexWind;
	using namespace DataVectorTypes;
	using namespace DataTimings;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// General Parameters...
	Real64 const SmallIncrement( 1.0e-10 ); // Small increment added for shading/sunlit area calculations.
	Real64 const HCMULT( 100000.0 ); // Multiplier used to change meters to .01 millimeters for homogeneous coordinates.
	// Homogeneous Coordinates are represented in integers (64 bit). This changes the surface coordinates from meters
	// to .01 millimeters -- making that the resolution for shadowing, polygon clipping, etc.
	Real64 const sqHCMULT( HCMULT * HCMULT ); // Square of HCMult used in Homogeneous coordinates
	Real64 const sqHCMULT_fac( 0.5 / sqHCMULT ); // ( 0.5 / sqHCMULT ) factor
	Real64 const kHCMULT( 1.0 / ( HCMULT * HCMULT ) ); // half of inverse square of HCMult used in Homogeneous coordinates

	// Parameters for use with the variable OverlapStatus...
	int const NoOverlap( 1 );
	int const FirstSurfWithinSecond( 2 );
	int const SecondSurfWithinFirst( 3 );
	int const PartialOverlap( 4 );
	int const TooManyVertices( 5 );
	int const TooManyFigures( 6 );
	Array1D_string const cOverLapStatus( 6, { "No-Overlap", "1st-Surf-within-2nd", "2nd-Surf-within-1st", "Partial-Overlap", "Too-Many-Vertices", "Too-Many-Figures" } );

	// DERIVED TYPE DEFINITIONS:
	// INTERFACE BLOCK SPECIFICATIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	int MaxHCV( 15 ); // Maximum number of HC vertices
	// (needs to be based on maxnumvertices)
	int MaxHCS( 15000 ); // 200      ! Maximum number of HC surfaces (was 56)
	// Following are initially set in AllocateModuleArrays
	int MAXHCArrayBounds( 0 ); // Bounds based on Max Number of Vertices in surfaces
	int MAXHCArrayIncrement( 0 ); // Increment based on Max Number of Vertices in surfaces
	// The following variable should be re-engineered to lower in module hierarchy but need more analysis
	int NVS; // Number of vertices of the shadow/clipped surface
	int NumVertInShadowOrClippedSurface;
	int CurrentSurfaceBeingShadowed;
	int CurrentShadowingSurface;
	int OverlapStatus; // Results of overlap calculation:
	// 1=No overlap; 2=NS1 completely within NS2
	// 3=NS2 completely within NS1; 4=Partial overlap

	Array1D< Real64 > CTHETA; // Cosine of angle of incidence of sun's rays on surface NS
	int FBKSHC; // HC location of first back surface
	int FGSSHC; // HC location of first general shadowing surface
	int FINSHC; // HC location of first back surface overlap
	int FRVLHC; // HC location of first reveal surface
	int FSBSHC; // HC location of first subsurface
	int LOCHCA( 0 ); // Location of highest data in the HC arrays
	int NBKSHC; // Number of back surfaces in the HC arrays
	int NGSSHC; // Number of general shadowing surfaces in the HC arrays
	int NINSHC; // Number of back surface overlaps in the HC arrays
	int NRVLHC; // Number of reveal surfaces in HC array
	int NSBSHC; // Number of subsurfaces in the HC arrays
	bool CalcSkyDifShading; // True when sky diffuse solar shading is
	int ShadowingCalcFrequency( 0 ); // Frequency for Shadowing Calculations
	int ShadowingDaysLeft( 0 ); // Days left in current shadowing period
	bool debugging( false );
	namespace {
	// These were static variables within different functions. They were pulled out into the namespace
	// to facilitate easier unit testing of those functions.
	// These are purposefully not in the header file as an extern variable. No one outside of this should
	// use these. They are cleared by clear_state() for use by unit tests, but normal simulations should be unaffected.
	// This is purposefully in an anonymous namespace so nothing outside this implementation file can use it.
		bool MustAllocSolarShading( true );
		bool GetInputFlag( true );
		bool firstTime( true );
	}

	std::ofstream shd_stream; // Shading file stream
	Array1D_int HCNS; // Surface number of back surface HC figures
	Array1D_int HCNV; // Number of vertices of each HC figure
	Array2D< Int64 > HCA; // 'A' homogeneous coordinates of sides
	Array2D< Int64 > HCB; // 'B' homogeneous coordinates of sides
	Array2D< Int64 > HCC; // 'C' homogeneous coordinates of sides
	Array2D< Int64 > HCX; // 'X' homogeneous coordinates of vertices of figure.
	Array2D< Int64 > HCY; // 'Y' homogeneous coordinates of vertices of figure.
	Array3D_int WindowRevealStatus;
	Array1D< Real64 > HCAREA; // Area of each HC figure.  Sign Convention:  Base Surface
	// - Positive, Shadow - Negative, Overlap between two shadows
	// - positive, etc., so that sum of HC areas=base sunlit area
	Array1D< Real64 > HCT; // Transmittance of each HC figure
	Array1D< Real64 > ISABSF; // For simple interior solar distribution (in which all beam
	// radiation entering zone is assumed to strike the floor),
	// fraction of beam radiation absorbed by each floor surface
	Array1D< Real64 > SAREA; // Sunlit area of heat transfer surface HTS
	// Excludes multiplier for windows
	// Shadowing combinations data structure...See ShadowingCombinations type
	int NumTooManyFigures( 0 );
	int NumTooManyVertices( 0 );
	int NumBaseSubSurround( 0 );
	Array1D< Real64 > SUNCOS( 3 ); // Direction cosines of solar position
	Real64 XShadowProjection; // X projection of a shadow (formerly called C)
	Real64 YShadowProjection; // Y projection of a shadow (formerly called S)
	Array1D< Real64 > XTEMP; // Temporary 'X' values for HC vertices of the overlap
	Array1D< Real64 > XVC; // X-vertices of the clipped figure
	Array1D< Real64 > XVS; // X-vertices of the shadow
	Array1D< Real64 > YTEMP; // Temporary 'Y' values for HC vertices of the overlap
	Array1D< Real64 > YVC; // Y-vertices of the clipped figure
	Array1D< Real64 > YVS; // Y-vertices of the shadow
	Array1D< Real64 > ZVC; // Z-vertices of the clipped figure
	// Used in Sutherland Hodman poly clipping
	Array1D< Real64 > ATEMP; // Temporary 'A' values for HC vertices of the overlap
	Array1D< Real64 > BTEMP; // Temporary 'B' values for HC vertices of the overlap
	Array1D< Real64 > CTEMP; // Temporary 'C' values for HC vertices of the overlap
	Array1D< Real64 > XTEMP1; // Temporary 'X' values for HC vertices of the overlap
	Array1D< Real64 > YTEMP1; // Temporary 'Y' values for HC vertices of the overlap
	int maxNumberOfFigures( 0 );

	// SUBROUTINE SPECIFICATIONS FOR MODULE SolarShading

	// Object Data
	Array1D< SurfaceErrorTracking > TrackTooManyFigures;
	Array1D< SurfaceErrorTracking > TrackTooManyVertices;
	Array1D< SurfaceErrorTracking > TrackBaseSubSurround;

	static gio::Fmt fmtLD( "*" );

	// MODULE SUBROUTINES:

	// Functions
	void
	clear_state()
	{
		MaxHCV= 15;
		MaxHCS= 1500;
		MAXHCArrayBounds = 0;
		MAXHCArrayIncrement = 0;
		NVS = 0;
		NumVertInShadowOrClippedSurface = 0;
		CurrentSurfaceBeingShadowed = 0;
		CurrentShadowingSurface = 0;
		OverlapStatus = 0;
		CTHETA.deallocate();
		FBKSHC = 0;
		FGSSHC = 0;
		FINSHC = 0;
		FRVLHC = 0;
		FSBSHC = 0;
		LOCHCA = 0;
		NBKSHC = 0;
		NGSSHC = 0;
		NINSHC = 0;
		NRVLHC = 0;
		NSBSHC = 0;
		CalcSkyDifShading = false;
		ShadowingCalcFrequency = 0; // Frequency for Shadowing Calculations
		ShadowingDaysLeft =0; // Days left in current shadowing period
		debugging = false;
		MustAllocSolarShading = true;
		GetInputFlag = true;
		firstTime = true;
		HCNS.deallocate();
		HCNV.deallocate();
		HCA.deallocate();
		HCB.deallocate();
		HCC.deallocate();
		HCX.deallocate();
		HCY.deallocate();
		WindowRevealStatus.deallocate();
		HCAREA.deallocate();
		HCT.deallocate();
		ISABSF.deallocate();
		SAREA.deallocate();
		NumTooManyFigures = 0;
		NumTooManyVertices = 0;
		NumBaseSubSurround = 0;
		XShadowProjection = 0.0;
		YShadowProjection = 0.0;
		XTEMP.deallocate();
		XVC.deallocate();
		XVS.deallocate();
		YTEMP.deallocate();
		YVC.deallocate();
		YVS.deallocate();
		ZVC.deallocate();
		ATEMP.deallocate();
		BTEMP.deallocate();
		CTEMP.deallocate();
		XTEMP1.deallocate();
		YTEMP1.deallocate();
		maxNumberOfFigures = 0;
		TrackTooManyFigures.deallocate();
		TrackTooManyVertices.deallocate();
		TrackBaseSubSurround.deallocate();
		DBZoneIntWin.deallocate();
		ISABSF.deallocate();
	}

	void
	InitSolarCalculations()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         George Walton
		//       DATE WRITTEN   September 1977
		//       MODIFIED       na
		//       RE-ENGINEERED  Mar97, RKS, Initial EnergyPlus Version

		// PURPOSE OF THIS SUBROUTINE:
		// This routine controls the computation of the solar flux multipliers.

		// METHODOLOGY EMPLOYED:
		// All shadowing calculations have been grouped under this routine to
		// allow segmentation separating it from the hourly loads calculation.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// FLOW:
#ifdef EP_Count_Calls
		++NumInitSolar_Calls;
#endif
		if ( BeginSimFlag ) {

			shd_stream.open( DataStringGlobals::outputShdFileName );
			if ( ! shd_stream ) {
				ShowFatalError( "InitSolarCalculations: Could not open file \"" + DataStringGlobals::outputShdFileName + "\" for output (write)." );
			}

			if ( GetInputFlag ) {
				GetShadowingInput();
				GetInputFlag = false;
				MaxHCV = ( ( ( max( 15, MaxVerticesPerSurface ) + 16 ) / 16 ) * 16 ) - 1; // Assure MaxHCV+1 is multiple of 16 for 128 B alignment
				assert( ( MaxHCV + 1 ) % 16 == 0 );
			}

			if ( firstTime ) DisplayString( "Allocate Solar Module Arrays" );
			AllocateModuleArrays();

			if ( SolarDistribution != FullInteriorExterior ) {
				if ( firstTime ) DisplayString( "Computing Interior Solar Absorption Factors" );
				ComputeIntSolarAbsorpFactors();
			}

			if ( firstTime ) DisplayString( "Determining Shadowing Combinations" );
			DetermineShadowingCombinations();
			shd_stream.close(); // Done writing to shd file

			if ( firstTime ) DisplayString( "Computing Window Shade Absorption Factors" );
			ComputeWinShadeAbsorpFactors();

			if ( CalcSolRefl ) {
				DisplayString( "Initializing Solar Reflection Factors" );
				InitSolReflRecSurf();
			}

			if ( firstTime ) DisplayString( "Proceeding with Initializing Solar Calculations" );

		}

		if ( BeginEnvrnFlag ) {
			CTHETA = 0.0;
			SAREA = 0.0;
			SurfSunlitArea = 0.0;
			SurfSunlitFrac = 0.0;
			SunlitFracHR = 0.0;
			SunlitFrac = 0.0;
			SunlitFracWithoutReveal = 0.0;
			BackSurfaces = 0;
			OverlapAreas = 0.0;
			CosIncAngHR = 0.0;
			CosIncAng = 0.0;
			AnisoSkyMult = 1.0; //For isotropic sky; recalculated in AnisoSkyViewFactors if anisotropic radiance
			//    WithShdgIsoSky=0.0
			//    WoShdgIsoSky=0.0
			//    WithShdgHoriz=0.0
			//    WoShdgHoriz=0.0
			//    DifShdgRatioIsoSky=0.0
			//    DifShdgRatioHoriz=0.0
			MultIsoSky = 0.0;
			MultCircumSolar = 0.0;
			MultHorizonZenith = 0.0;
			WinTransSolar = 0.0;
			WinBmSolar = 0.0;
			WinBmBmSolar = 0.0;
			WinBmDifSolar = 0.0;

			WinDifSolar = 0.0;
			WinDirSolTransAtIncAngle = 0.0;
			WinHeatGain = 0.0;
			WinHeatGainRep = 0.0;
			WinHeatLossRep = 0.0;
			WinGainConvGlazToZoneRep = 0.0;
			WinGainIRGlazToZoneRep = 0.0;
			WinLossSWZoneToOutWinRep = 0.0;
			WinGainFrameDividerToZoneRep = 0.0;
			WinGainConvGlazShadGapToZoneRep = 0.0;
			WinGainConvShadeToZoneRep = 0.0;
			OtherConvGainInsideFaceToZoneRep = 0.0;
			WinGainIRShadeToZoneRep = 0.0;
			WinGapConvHtFlowRep = 0.0;
			WinShadingAbsorbedSolar = 0.0;
			WinSysSolTransmittance = 0.0;
			WinSysSolReflectance = 0.0;
			WinSysSolAbsorptance = 0.0;
			InsideGlassCondensationFlag = 0;
			InsideFrameCondensationFlag = 0;
			InsideDividerCondensationFlag = 0;
			ZoneTransSolar = 0.0;
			ZoneBmSolFrExtWinsRep = 0.0;
			ZoneBmSolFrIntWinsRep = 0.0;
			InitialZoneDifSolReflW = 0.0;
			ZoneDifSolFrExtWinsRep = 0.0;
			ZoneDifSolFrIntWinsRep = 0.0;
			ZoneWinHeatGain = 0.0;
			ZoneWinHeatGainRep = 0.0;
			ZoneWinHeatLossRep = 0.0;
			ZoneOpaqSurfInsFaceCond = 0.0;
			ZoneOpaqSurfInsFaceCondGainRep = 0.0;
			ZoneOpaqSurfInsFaceCondLossRep = 0.0;
			QRadSWOutIncident = 0.0;
			QRadSWOutIncidentBeam = 0.0;
			BmIncInsSurfIntensRep = 0.0;
			BmIncInsSurfAmountRep = 0.0;
			//    DifIncInsSurfIntensRep=0.0
			//    DifIncInsSurfAmountRep=0.0
			IntBmIncInsSurfIntensRep = 0.0;
			IntBmIncInsSurfAmountRep = 0.0;
			//    IntDifIncInsSurfIntensRep=0.0
			//    IntDifIncInsSurfAmountRep=0.0
			QRadSWOutIncidentSkyDiffuse = 0.0;
			QRadSWOutIncidentGndDiffuse = 0.0;
			QRadSWOutIncBmToDiffReflGnd = 0.0;
			QRadSWOutIncSkyDiffReflGnd = 0.0;
			QRadSWOutIncBmToBmReflObs = 0.0;
			QRadSWOutIncBmToDiffReflObs = 0.0;
			QRadSWOutIncSkyDiffReflObs = 0.0;
			CosIncidenceAngle = 0.0;
			QRadSWwinAbsTot = 0.0;
			SWwinAbsTotalReport = 0.0;
			InitialDifSolInAbsReport = 0.0;
			InitialDifSolInTransReport = 0.0;
			SWInAbsTotalReport = 0.0;
			WindowRevealStatus = 0;
			//energy
			WinTransSolarEnergy = 0.0;
			WinBmSolarEnergy = 0.0;
			WinBmBmSolarEnergy = 0.0;
			WinBmDifSolarEnergy = 0.0;

			WinDifSolarEnergy = 0.0;
			WinHeatGainRepEnergy = 0.0;
			WinHeatLossRepEnergy = 0.0;
			WinGapConvHtFlowRepEnergy = 0.0;
			WinShadingAbsorbedSolarEnergy = 0.0;
			ZoneTransSolarEnergy = 0.0;
			ZoneBmSolFrExtWinsRepEnergy = 0.0;
			ZoneBmSolFrIntWinsRepEnergy = 0.0;
			ZoneDifSolFrExtWinsRepEnergy = 0.0;
			ZoneDifSolFrIntWinsRepEnergy = 0.0;
			ZoneWinHeatGainRepEnergy = 0.0;
			ZoneWinHeatLossRepEnergy = 0.0;
			ZnOpqSurfInsFaceCondGnRepEnrg = 0.0;
			ZnOpqSurfInsFaceCondLsRepEnrg = 0.0;
			BmIncInsSurfAmountRepEnergy = 0.0;
			//    DifIncInsSurfAmountRepEnergy=0.0
			IntBmIncInsSurfAmountRepEnergy = 0.0;
			//    IntDifIncInsSurfAmountRepEnergy=0.0
			QRadSWwinAbsTotEnergy = 0.0;

		}

		firstTime = false;

	}

	void
	GetShadowingInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   July 1999
		//       MODIFIED       B. Griffith, Nov 2012, add calculaton method
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the Shadowing Calculation object.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::SameString;
		using General::RoundSigDigits;
		using namespace DataIPShortCuts;
		using DataSystemVariables::SutherlandHodgman;
		using DataSystemVariables::DetailedSkyDiffuseAlgorithm;
		using DataSystemVariables::DetailedSolarTimestepIntegration;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt fmtA( "(A)" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumItems;
		int NumNumbers;
		int NumAlphas;
		int IOStat;

		rNumericArgs( {1,4} ) = 0.0; // so if nothing gotten, defaults will be maintained.
		cAlphaArgs( 1 ) = "";
		cAlphaArgs( 2 ) = "";
		cCurrentModuleObject = "ShadowCalculation";
		NumItems = GetNumObjectsFound( cCurrentModuleObject );
		NumAlphas = 0;
		NumNumbers = 0;
		if ( NumItems > 1 ) {
			ShowWarningError( cCurrentModuleObject + ": More than 1 occurence of this object found, only first will be used." );
		}

		if ( NumItems != 0 ) {
			GetObjectItem( cCurrentModuleObject, 1, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			ShadowingCalcFrequency = rNumericArgs( 1 );
		}

		if ( ShadowingCalcFrequency <= 0 ) {
			//  Set to default value
			ShadowingCalcFrequency = 20;
		}
		if ( ShadowingCalcFrequency > 31 ) {
			ShowWarningError( cCurrentModuleObject + ": suspect " + cNumericFieldNames( 1 ) );
			ShowContinueError( "Value entered=[" + RoundSigDigits( rNumericArgs( 1 ), 0 ) + "], Shadowing Calculations will be inaccurate." );
		}

		if ( rNumericArgs( 2 ) > 199.0 ) {
			MaxHCS = rNumericArgs( 2 );
		} else {
			MaxHCS = 15000;
		}

		if ( NumAlphas >= 1 ) {
			if ( SameString( cAlphaArgs( 1 ), "AverageOverDaysInFrequency" ) ) {
				DetailedSolarTimestepIntegration = false;
				cAlphaArgs( 1 ) = "AverageOverDaysInFrequency";
			} else if ( SameString( cAlphaArgs( 1 ), "TimestepFrequency" ) ) {
				DetailedSolarTimestepIntegration = true;
				cAlphaArgs( 1 ) = "TimestepFrequency";
			} else {
				ShowWarningError( cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 1 ) );
				ShowContinueError( "Value entered=\"" + cAlphaArgs( 1 ) + "\", AverageOverDaysInFrequency will be used." );
				DetailedSolarTimestepIntegration = false;
				cAlphaArgs( 1 ) = "AverageOverDaysInFrequency";
			}
		} else {
			DetailedSolarTimestepIntegration = false;
			cAlphaArgs( 1 ) = "AverageOverDaysInFrequency";
		}

		if ( NumAlphas >= 2 ) {
			if ( SameString( cAlphaArgs( 2 ), "SutherlandHodgman" ) ) {
				SutherlandHodgman = true;
				cAlphaArgs( 2 ) = "SutherlandHodgman";
			} else if ( SameString( cAlphaArgs( 2 ), "ConvexWeilerAtherton" ) ) {
				SutherlandHodgman = false;
				cAlphaArgs( 2 ) = "ConvexWeilerAtherton";
			} else if ( lAlphaFieldBlanks( 2 ) ) {
				if ( ! SutherlandHodgman ) { // if already set.
					cAlphaArgs( 2 ) = "ConvexWeilerAtherton";
				} else {
					cAlphaArgs( 2 ) = "SutherlandHodgman";
				}
			} else {
				ShowWarningError( cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 2 ) );
				if ( ! SutherlandHodgman ) {
					ShowContinueError( "Value entered=\"" + cAlphaArgs( 2 ) + "\", ConvexWeilerAtherton will be used." );
				} else {
					ShowContinueError( "Value entered=\"" + cAlphaArgs( 2 ) + "\", SutherlandHodgman will be used." );
				}
			}
		} else {
			if ( ! SutherlandHodgman ) {
				cAlphaArgs( 2 ) = "ConvexWeilerAtherton";
			} else {
				cAlphaArgs( 2 ) = "SutherlandHodgman";
			}
		}

		if ( NumAlphas >= 3 ) {
			if ( SameString( cAlphaArgs( 3 ), "SimpleSkyDiffuseModeling" ) ) {
				DetailedSkyDiffuseAlgorithm = false;
				cAlphaArgs( 3 ) = "SimpleSkyDiffuseModeling";
			} else if ( SameString( cAlphaArgs( 3 ), "DetailedSkyDiffuseModeling" ) ) {
				DetailedSkyDiffuseAlgorithm = true;
				cAlphaArgs( 3 ) = "DetailedSkyDiffuseModeling";
			} else if ( lAlphaFieldBlanks( 3 ) ) {
				DetailedSkyDiffuseAlgorithm = false;
				cAlphaArgs( 3 ) = "SimpleSkyDiffuseModeling";
			} else {
				ShowWarningError( cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 3 ) );
				ShowContinueError( "Value entered=\"" + cAlphaArgs( 3 ) + "\", SimpleSkyDiffuseModeling will be used." );
			}
		} else {
			cAlphaArgs( 3 ) = "SimpleSkyDiffuseModeling";
			DetailedSkyDiffuseAlgorithm = false;
		}

		if ( ! DetailedSkyDiffuseAlgorithm && ShadingTransmittanceVaries && SolarDistribution != MinimalShadowing ) {
			ShowWarningError( "GetShadowingInput: The shading transmittance for shading devices changes throughout the year. Choose DetailedSkyDiffuseModeling in the " + cCurrentModuleObject + " object to remove this warning." );
			ShowContinueError( "Simulation has been reset to use DetailedSkyDiffuseModeling. Simulation continues." );
			DetailedSkyDiffuseAlgorithm = true;
			cAlphaArgs( 2 ) = "DetailedSkyDiffuseModeling";
			if ( ShadowingCalcFrequency > 1 ) {
				ShowContinueError( "Better accuracy may be gained by setting the " + cNumericFieldNames( 1 ) + " to 1 in the " + cCurrentModuleObject + " object." );
			}
		} else if ( DetailedSkyDiffuseAlgorithm ) {
			if ( ! ShadingTransmittanceVaries || SolarDistribution == MinimalShadowing ) {
				ShowWarningError( "GetShadowingInput: DetailedSkyDiffuseModeling is chosen but not needed as either the shading transmittance for shading devices does not change throughout the year" );
				ShowContinueError( " or MinimalShadowing has been chosen." );
				ShowContinueError( "Simulation should be set to use SimpleSkyDiffuseModeling, but is left at Detailed for simulation." );
				ShowContinueError( "Choose SimpleSkyDiffuseModeling in the " + cCurrentModuleObject + " object to reduce computation time." );
			}
		}

		gio::write( OutputFileInits, fmtA ) << "! <Shadowing/Sun Position Calculations> [Annual Simulations], Calculation Method, Value {days}, Allowable Number Figures in Shadow Overlap {}, Polygon Clipping Algorithm, Sky Diffuse Modeling Algorithm";
		gio::write( OutputFileInits, fmtA ) << "Shadowing/Sun Position Calculations," + cAlphaArgs( 1 ) + ',' + RoundSigDigits( ShadowingCalcFrequency ) + ',' + RoundSigDigits( MaxHCS ) + ',' + cAlphaArgs( 2 ) + ',' + cAlphaArgs( 3 );

	}

	void
	AllocateModuleArrays()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   February 1998
		//       MODIFIED       August 2005 JG - Added output variables for energy in J
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine allocates all of the arrays at the module level which
		// require allocation.

		// METHODOLOGY EMPLOYED:
		// Allocation is dependent on the user input file.

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SurfLoop;
		int ZoneLoop;
		int I;
		int NumOfLayers;

		// FLOW:

		CTHETA.dimension( TotSurfaces, 0.0 );
		SAREA.dimension( TotSurfaces, 0.0 );
		SurfSunlitArea.dimension( TotSurfaces, 0.0 );
		SurfSunlitFrac.dimension( TotSurfaces, 0.0 );
		SunlitFracHR.dimension( 24, TotSurfaces, 0.0 );
		SunlitFrac.dimension( NumOfTimeStepInHour, 24, TotSurfaces, 0.0 );
		SunlitFracWithoutReveal.dimension( NumOfTimeStepInHour, 24, TotSurfaces, 0.0 );
		BackSurfaces.dimension( NumOfTimeStepInHour, 24, MaxBkSurf, TotSurfaces, 0 );
		OverlapAreas.dimension( NumOfTimeStepInHour, 24, MaxBkSurf, TotSurfaces, 0.0 );
		CosIncAngHR.dimension( 24, TotSurfaces, 0.0 );
		CosIncAng.dimension( NumOfTimeStepInHour, 24, TotSurfaces, 0.0 );
		AnisoSkyMult.dimension( TotSurfaces, 1.0 ); // For isotropic sky: recalculated in AnisoSkyViewFactors if anisotropic radiance
		//  ALLOCATE(WithShdgIsoSky(TotSurfaces))
		//  WithShdgIsoSky=0.0
		//  ALLOCATE(WoShdgIsoSky(TotSurfaces))
		//  WoShdgIsoSky=0.0
		//  ALLOCATE(WithShdgHoriz(TotSurfaces))
		//  WithShdgHoriz=0.0
		//  ALLOCATE(WoShdgHoriz(TotSurfaces))
		//  WoShdgHoriz=0.0
		//  ALLOCATE(DifShdgRatioIsoSky(TotSurfaces))
		//  DifShdgRatioIsoSky=0.0
		//  ALLOCATE(DifShdgRatioHoriz(TotSurfaces))
		//  DifShdgRatioHoriz=0.0
		MultIsoSky.dimension( TotSurfaces, 0.0 );
		MultCircumSolar.dimension( TotSurfaces, 0.0 );
		MultHorizonZenith.dimension( TotSurfaces, 0.0 );
		WinTransSolar.dimension( TotSurfaces, 0.0 );
		WinBmSolar.dimension( TotSurfaces, 0.0 );
		WinBmBmSolar.dimension( TotSurfaces, 0.0 );
		WinBmDifSolar.dimension( TotSurfaces, 0.0 );

		WinDifSolar.dimension( TotSurfaces, 0.0 );
		WinDirSolTransAtIncAngle.dimension( TotSurfaces, 0.0 );
		WinHeatGain.dimension( TotSurfaces, 0.0 );
		WinHeatGainRep.dimension( TotSurfaces, 0.0 );
		WinHeatLossRep.dimension( TotSurfaces, 0.0 );
		WinGainConvGlazToZoneRep.dimension( TotSurfaces, 0.0 );
		WinGainIRGlazToZoneRep.dimension( TotSurfaces, 0.0 );
		WinLossSWZoneToOutWinRep.dimension( TotSurfaces, 0.0 );
		WinGainFrameDividerToZoneRep.dimension( TotSurfaces, 0.0 );
		WinGainConvGlazShadGapToZoneRep.dimension( TotSurfaces, 0.0 );
		WinGainConvShadeToZoneRep.dimension( TotSurfaces, 0.0 );
		OtherConvGainInsideFaceToZoneRep.dimension( TotSurfaces, 0.0 );
		WinGainIRShadeToZoneRep.dimension( TotSurfaces, 0.0 );
		WinGapConvHtFlowRep.dimension( TotSurfaces, 0.0 );
		WinShadingAbsorbedSolar.dimension( TotSurfaces, 0.0 );
		WinSysSolTransmittance.dimension( TotSurfaces, 0.0 );
		WinSysSolReflectance.dimension( TotSurfaces, 0.0 );
		WinSysSolAbsorptance.dimension( TotSurfaces, 0.0 );
		InsideGlassCondensationFlag.dimension( TotSurfaces, 0 );
		InsideFrameCondensationFlag.dimension( TotSurfaces, 0 );
		InsideDividerCondensationFlag.dimension( TotSurfaces, 0 );
		ZoneTransSolar.dimension( NumOfZones, 0.0 );
		ZoneBmSolFrExtWinsRep.dimension( NumOfZones, 0.0 );
		ZoneBmSolFrIntWinsRep.dimension( NumOfZones, 0.0 );
		InitialZoneDifSolReflW.dimension( NumOfZones, 0.0 );
		ZoneDifSolFrExtWinsRep.dimension( NumOfZones, 0.0 );
		ZoneDifSolFrIntWinsRep.dimension( NumOfZones, 0.0 );
		ZoneWinHeatGain.dimension( NumOfZones, 0.0 );
		ZoneWinHeatGainRep.dimension( NumOfZones, 0.0 );
		ZoneWinHeatLossRep.dimension( NumOfZones, 0.0 );
		ZoneOpaqSurfInsFaceCond.dimension( NumOfZones, 0.0 );
		ZoneOpaqSurfInsFaceCondGainRep.dimension( NumOfZones, 0.0 );
		ZoneOpaqSurfInsFaceCondLossRep.dimension( NumOfZones, 0.0 );
		ZoneOpaqSurfExtFaceCond.dimension( NumOfZones, 0.0 );
		ZoneOpaqSurfExtFaceCondGainRep.dimension( NumOfZones, 0.0 );
		ZoneOpaqSurfExtFaceCondLossRep.dimension( NumOfZones, 0.0 );

		QRadSWOutIncident.dimension( TotSurfaces, 0.0 );
		QRadSWOutIncidentBeam.dimension( TotSurfaces, 0.0 );
		BmIncInsSurfIntensRep.dimension( TotSurfaces, 0.0 );
		BmIncInsSurfAmountRep.dimension( TotSurfaces, 0.0 );
		//  ALLOCATE(DifIncInsSurfIntensRep(TotSurfaces))
		//  DifIncInsSurfIntensRep=0.0
		//  ALLOCATE(DifIncInsSurfAmountRep(TotSurfaces))
		//  DifIncInsSurfAmountRep=0.0
		IntBmIncInsSurfIntensRep.dimension( TotSurfaces, 0.0 );
		IntBmIncInsSurfAmountRep.dimension( TotSurfaces, 0.0 );
		//  ALLOCATE(IntDifIncInsSurfIntensRep(TotSurfaces))
		//  IntDifIncInsSurfIntensRep=0.0
		//  ALLOCATE(IntDifIncInsSurfAmountRep(TotSurfaces))
		//  IntDifIncInsSurfAmountRep=0.0
		QRadSWOutIncidentSkyDiffuse.dimension( TotSurfaces, 0.0 );
		QRadSWOutIncidentGndDiffuse.dimension( TotSurfaces, 0.0 );
		QRadSWOutIncBmToDiffReflGnd.dimension( TotSurfaces, 0.0 );
		QRadSWOutIncSkyDiffReflGnd.dimension( TotSurfaces, 0.0 );
		QRadSWOutIncBmToBmReflObs.dimension( TotSurfaces, 0.0 );
		QRadSWOutIncBmToDiffReflObs.dimension( TotSurfaces, 0.0 );
		QRadSWOutIncSkyDiffReflObs.dimension( TotSurfaces, 0.0 );
		CosIncidenceAngle.dimension( TotSurfaces, 0.0 );
		BSDFBeamDirectionRep.dimension( TotSurfaces, 0 );
		BSDFBeamThetaRep.dimension( TotSurfaces, 0.0 );
		BSDFBeamPhiRep.dimension( TotSurfaces, 0.0 );
		QRadSWwinAbsTot.dimension( TotSurfaces, 0.0 );

		QRadSWwinAbsLayer.dimension( MaxSolidWinLayers, TotSurfaces, 0.0 );

		FenLaySurfTempFront.dimension( MaxSolidWinLayers, TotSurfaces, 0.0 );
		FenLaySurfTempBack.dimension( MaxSolidWinLayers, TotSurfaces, 0.0 );

		SWwinAbsTotalReport.dimension( TotSurfaces, 0.0 );
		InitialDifSolInAbsReport.dimension( TotSurfaces, 0.0 );
		InitialDifSolInTransReport.dimension( TotSurfaces, 0.0 );
		SWInAbsTotalReport.dimension( TotSurfaces, 0.0 );
		WindowRevealStatus.dimension( NumOfTimeStepInHour, 24, TotSurfaces, 0 );

		// Weiler-Atherton
		MAXHCArrayBounds = 2 * ( MaxVerticesPerSurface + 1 );
		MAXHCArrayIncrement = MaxVerticesPerSurface + 1;
		XTEMP.dimension( 2 * ( MaxVerticesPerSurface + 1 ), 0.0 );
		YTEMP.dimension( 2 * ( MaxVerticesPerSurface + 1 ), 0.0 );
		XVC.dimension( MaxVerticesPerSurface + 1, 0.0 );
		XVS.dimension( MaxVerticesPerSurface + 1, 0.0 );
		YVC.dimension( MaxVerticesPerSurface + 1, 0.0 );
		YVS.dimension( MaxVerticesPerSurface + 1, 0.0 );
		ZVC.dimension( MaxVerticesPerSurface + 1, 0.0 );

		// Sutherland-Hodgman
		ATEMP.dimension( 2 * ( MaxVerticesPerSurface + 1 ), 0.0 );
		BTEMP.dimension( 2 * ( MaxVerticesPerSurface + 1 ), 0.0 );
		CTEMP.dimension( 2 * ( MaxVerticesPerSurface + 1 ), 0.0 );
		XTEMP1.dimension( 2 * ( MaxVerticesPerSurface + 1 ), 0.0 );
		YTEMP1.dimension( 2 * ( MaxVerticesPerSurface + 1 ), 0.0 );

		//energy
		WinTransSolarEnergy.dimension( TotSurfaces, 0.0 );
		WinBmSolarEnergy.dimension( TotSurfaces, 0.0 );

		WinBmBmSolarEnergy.dimension( TotSurfaces, 0.0 );
		WinBmDifSolarEnergy.dimension( TotSurfaces, 0.0 );

		WinDifSolarEnergy.dimension( TotSurfaces, 0.0 );
		WinHeatGainRepEnergy.dimension( TotSurfaces, 0.0 );
		WinHeatLossRepEnergy.dimension( TotSurfaces, 0.0 );
		WinGapConvHtFlowRepEnergy.dimension( TotSurfaces, 0.0 );
		ZoneTransSolarEnergy.dimension( NumOfZones, 0.0 );
		ZoneBmSolFrExtWinsRepEnergy.dimension( NumOfZones, 0.0 );
		ZoneBmSolFrIntWinsRepEnergy.dimension( NumOfZones, 0.0 );
		ZoneDifSolFrExtWinsRepEnergy.dimension( NumOfZones, 0.0 );
		ZoneDifSolFrIntWinsRepEnergy.dimension( NumOfZones, 0.0 );
		ZoneWinHeatGainRepEnergy.dimension( NumOfZones, 0.0 );
		ZoneWinHeatLossRepEnergy.dimension( NumOfZones, 0.0 );
		BmIncInsSurfAmountRepEnergy.dimension( TotSurfaces, 0.0 );
		ZnOpqSurfInsFaceCondGnRepEnrg.dimension( NumOfZones, 0.0 );
		ZnOpqSurfInsFaceCondLsRepEnrg.dimension( NumOfZones, 0.0 );
		ZnOpqSurfExtFaceCondGnRepEnrg.dimension( NumOfZones, 0.0 );
		ZnOpqSurfExtFaceCondLsRepEnrg.dimension( NumOfZones, 0.0 );
		//  ALLOCATE(DifIncInsSurfAmountRepEnergy(TotSurfaces))
		//  DifIncInsSurfAmountRepEnergy=0.0
		IntBmIncInsSurfAmountRepEnergy.dimension( TotSurfaces, 0.0 );
		//  ALLOCATE(IntDifIncInsSurfAmountRepEnergy(TotSurfaces))
		//  IntDifIncInsSurfAmountRepEnergy=0.0
		QRadSWwinAbsTotEnergy.dimension( TotSurfaces, 0.0 );
		WinShadingAbsorbedSolarEnergy.dimension( TotSurfaces, 0.0 );
		for ( auto & e : SurfaceWindow ) {
			e.BmSolAbsdOutsReveal = 0.0;
			e.BmSolRefldOutsRevealReport = 0.0;
			e.BmSolAbsdInsReveal = 0.0;
			e.BmSolRefldInsReveal = 0.0;
			e.BmSolRefldInsRevealReport = 0.0;
			e.OutsRevealDiffOntoGlazing = 0.0;
			e.InsRevealDiffOntoGlazing = 0.0;
			e.InsRevealDiffIntoZone = 0.0;
			e.OutsRevealDiffOntoFrame = 0.0;
			e.InsRevealDiffOntoFrame = 0.0;
		}

		// Added report variables for inside reveal to debug CR 7596. TH 5/26/2009
		for ( auto & e : SurfaceWindow ) {
			e.InsRevealDiffOntoGlazingReport = 0.0;
			e.InsRevealDiffIntoZoneReport = 0.0;
			e.InsRevealDiffOntoFrameReport = 0.0;
			e.BmSolAbsdInsRevealReport = 0.0;
		}

		DisplayString( "Initializing Zone Report Variables" );
		// CurrentModuleObject='Zone'
		for ( ZoneLoop = 1; ZoneLoop <= NumOfZones; ++ZoneLoop ) {
			SetupOutputVariable( "Zone Windows Total Transmitted Solar Radiation Rate [W]", ZoneTransSolar( ZoneLoop ), "Zone", "Average", Zone( ZoneLoop ).Name );
			SetupOutputVariable( "Zone Exterior Windows Total Transmitted Beam Solar Radiation Rate [W]", ZoneBmSolFrExtWinsRep( ZoneLoop ), "Zone", "Average", Zone( ZoneLoop ).Name );
			SetupOutputVariable( "Zone Interior Windows Total Transmitted Beam Solar Radiation Rate [W]", ZoneBmSolFrIntWinsRep( ZoneLoop ), "Zone", "Average", Zone( ZoneLoop ).Name );
			SetupOutputVariable( "Zone Exterior Windows Total Transmitted Diffuse Solar Radiation Rate [W]", ZoneDifSolFrExtWinsRep( ZoneLoop ), "Zone", "Average", Zone( ZoneLoop ).Name );
			SetupOutputVariable( "Zone Interior Windows Total Transmitted Diffuse Solar Radiation Rate [W]", ZoneDifSolFrIntWinsRep( ZoneLoop ), "Zone", "Average", Zone( ZoneLoop ).Name );
			SetupOutputVariable( "Zone Windows Total Heat Gain Rate [W]", ZoneWinHeatGainRep( ZoneLoop ), "Zone", "Average", Zone( ZoneLoop ).Name );
			SetupOutputVariable( "Zone Windows Total Heat Loss Rate [W]", ZoneWinHeatLossRep( ZoneLoop ), "Zone", "Average", Zone( ZoneLoop ).Name );
			// Energy variables
			SetupOutputVariable( "Zone Windows Total Transmitted Solar Radiation Energy [J]", ZoneTransSolarEnergy( ZoneLoop ), "Zone", "Sum", Zone( ZoneLoop ).Name );
			SetupOutputVariable( "Zone Exterior Windows Total Transmitted Beam Solar Radiation Energy [J]", ZoneBmSolFrExtWinsRepEnergy( ZoneLoop ), "Zone", "Sum", Zone( ZoneLoop ).Name );
			SetupOutputVariable( "Zone Interior Windows Total Transmitted Beam Solar Radiation Energy [J]", ZoneBmSolFrIntWinsRepEnergy( ZoneLoop ), "Zone", "Sum", Zone( ZoneLoop ).Name );
			SetupOutputVariable( "Zone Exterior Windows Total Transmitted Diffuse Solar Radiation Energy [J]", ZoneDifSolFrExtWinsRepEnergy( ZoneLoop ), "Zone", "Sum", Zone( ZoneLoop ).Name );
			SetupOutputVariable( "Zone Interior Windows Total Transmitted Diffuse Solar Radiation Energy [J]", ZoneDifSolFrIntWinsRepEnergy( ZoneLoop ), "Zone", "Sum", Zone( ZoneLoop ).Name );
			SetupOutputVariable( "Zone Windows Total Heat Gain Energy [J]", ZoneWinHeatGainRepEnergy( ZoneLoop ), "Zone", "Sum", Zone( ZoneLoop ).Name );
			SetupOutputVariable( "Zone Windows Total Heat Loss Energy [J]", ZoneWinHeatLossRepEnergy( ZoneLoop ), "Zone", "Sum", Zone( ZoneLoop ).Name );

			if ( DisplayAdvancedReportVariables ) {
				// CurrentModuleObject='Zone(Advanced)'
				SetupOutputVariable( "Zone Opaque Surface Inside Faces Total Conduction Heat Gain Rate [W]", ZoneOpaqSurfInsFaceCondGainRep( ZoneLoop ), "Zone", "Average", Zone( ZoneLoop ).Name );
				SetupOutputVariable( "Zone Opaque Surface Inside Faces Total Conduction Heat Loss Rate [W]", ZoneOpaqSurfInsFaceCondLossRep( ZoneLoop ), "Zone", "Average", Zone( ZoneLoop ).Name );
				// Energy variables
				SetupOutputVariable( "Zone Opaque Surface Inside Faces Total Conduction Heat Gain Energy [J]", ZnOpqSurfInsFaceCondGnRepEnrg( ZoneLoop ), "Zone", "Sum", Zone( ZoneLoop ).Name );
				SetupOutputVariable( "Zone Opaque Surface Inside Faces Total Conduction Heat Loss Energy [J]", ZnOpqSurfInsFaceCondLsRepEnrg( ZoneLoop ), "Zone", "Sum", Zone( ZoneLoop ).Name );
			}
		}

		DisplayString( "Initializing Surface (Shading) Report Variables" );
		// CurrentModuleObject='Surfaces'
		for ( SurfLoop = 1; SurfLoop <= TotSurfaces; ++SurfLoop ) {
			if ( Surface( SurfLoop ).ExtSolar ) {
				SetupOutputVariable( "Surface Outside Face Sunlit Area [m2]", SurfSunlitArea( SurfLoop ), "Zone", "State", Surface( SurfLoop ).Name );
				SetupOutputVariable( "Surface Outside Face Sunlit Fraction []", SurfSunlitFrac( SurfLoop ), "Zone", "State", Surface( SurfLoop ).Name );
				SetupOutputVariable( "Surface Outside Face Incident Solar Radiation Rate per Area [W/m2]", QRadSWOutIncident( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
				SetupOutputVariable( "Surface Outside Face Incident Beam Solar Radiation Rate per Area [W/m2]", QRadSWOutIncidentBeam( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
				SetupOutputVariable( "Surface Outside Face Incident Sky Diffuse Solar Radiation Rate per Area [W/m2]", QRadSWOutIncidentSkyDiffuse( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
				SetupOutputVariable( "Surface Outside Face Incident Ground Diffuse Solar Radiation Rate per Area [W/m2]", QRadSWOutIncidentGndDiffuse( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
				SetupOutputVariable( "Surface Outside Face Beam Solar Incident Angle Cosine Value []", CosIncidenceAngle( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
				SetupOutputVariable( "Surface Outside Face Incident Sky Diffuse Ground Reflected Solar Radiation Rate per Area [W/m2]", QRadSWOutIncSkyDiffReflGnd( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
				SetupOutputVariable( "Surface Outside Face Incident Sky Diffuse Surface Reflected Solar Radiation Rate per Area [W/m2]", QRadSWOutIncSkyDiffReflObs( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
				SetupOutputVariable( "Surface Outside Face Incident Beam To Beam Surface Reflected Solar Radiation Rate per Area [W/m2]", QRadSWOutIncBmToBmReflObs( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
				SetupOutputVariable( "Surface Outside Face Incident Beam To Diffuse Surface Reflected Solar Radiation Rate per Area [W/m2]", QRadSWOutIncBmToDiffReflObs( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
				SetupOutputVariable( "Surface Outside Face Incident Beam To Diffuse Ground Reflected Solar Radiation Rate per Area [W/m2]", QRadSWOutIncBmToDiffReflGnd( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
				SetupOutputVariable( "Surface Anisotropic Sky Multiplier []", AnisoSkyMult( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
				SetupOutputVariable( "Surface Window BSDF Beam Direction Number []", BSDFBeamDirectionRep( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
				SetupOutputVariable( "Surface Window BSDF Beam Theta Angle [rad]", BSDFBeamThetaRep( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
				SetupOutputVariable( "Surface Window BSDF Beam Phi Angle [rad]", BSDFBeamPhiRep( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
			}
			if ( ! Surface( SurfLoop ).HeatTransSurf ) continue;

			if ( Surface( SurfLoop ).Class == SurfaceClass_Window ) {
				// CurrentModuleObject='Windows/GlassDoors'
				if ( Surface( SurfLoop ).ExtSolar ) {
					SetupOutputVariable( "Surface Window Total Glazing Layers Absorbed Solar Radiation Rate [W]", QRadSWwinAbsTot( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
					SetupOutputVariable( "Surface Window Total Glazing Layers Absorbed Shortwave Radiation Rate [W]", SWwinAbsTotalReport( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );

					if ( Construct( Surface( SurfLoop ).Construction ).WindowTypeBSDF ) {
						NumOfLayers = Construct( Surface( SurfLoop ).Construction ).TotSolidLayers;
					} else {
						NumOfLayers = Construct( Surface( SurfLoop ).Construction ).TotLayers;
					}
					for ( I = 1; I <= NumOfLayers; ++I ) {
						SetupOutputVariable( "Surface Window Total Absorbed Shortwave Radiation Rate Layer " + RoundSigDigits( I ) + " [W]", QRadSWwinAbsLayer( I, SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Front Face Temperature Layer " + RoundSigDigits( I ) + " [C]", FenLaySurfTempFront( I, SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Back Face Temperature Layer " + RoundSigDigits( I ) + " [C]", FenLaySurfTempBack( I, SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
					}

					SetupOutputVariable( "Surface Window Transmitted Solar Radiation Rate [W]", WinTransSolar( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
					SetupOutputVariable( "Surface Window Transmitted Beam Solar Radiation Rate [W]", WinBmSolar( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );

					//added TH 12/9/2009
					SetupOutputVariable( "Surface Window Transmitted Beam To Beam Solar Radiation Rate [W]", WinBmBmSolar( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
					SetupOutputVariable( "Surface Window Transmitted Beam To Diffuse Solar Radiation Rate [W]", WinBmDifSolar( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );

					SetupOutputVariable( "Surface Window Transmitted Diffuse Solar Radiation Rate [W]", WinDifSolar( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
					SetupOutputVariable( "Surface Window Heat Gain Rate [W]", WinHeatGainRep( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
					SetupOutputVariable( "Surface Window Heat Loss Rate [W]", WinHeatLossRep( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
					SetupOutputVariable( "Surface Window Gap Convective Heat Transfer Rate [W]", WinGapConvHtFlowRep( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
					SetupOutputVariable( "Surface Window Shading Device Absorbed Solar Radiation Rate [W]", WinShadingAbsorbedSolar( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );

					if ( DisplayAdvancedReportVariables ) {
						// CurrentModuleObject='Windows/GlassDoors(Advanced)'
						SetupOutputVariable( "Surface Window Inside Face Glazing Zone Convection Heat Gain Rate [W]", WinGainConvGlazToZoneRep( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Inside Face Glazing Net Infrared Heat Transfer Rate [W]", WinGainIRGlazToZoneRep( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Shortwave from Zone Back Out Window Heat Transfer Rate [W]", WinLossSWZoneToOutWinRep( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Inside Face Frame and Divider Zone Heat Gain Rate [W]", WinGainFrameDividerToZoneRep( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Inside Face Gap between Shade and Glazing Zone Convection Heat Gain Rate [W]", WinGainConvGlazShadGapToZoneRep( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Inside Face Shade Zone Convection Heat Gain Rate [W]", WinGainConvShadeToZoneRep( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Inside Face Shade Net Infrared Heat Transfer Rate [W]", WinGainIRShadeToZoneRep( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
						if ( Construct( Surface( SurfLoop ).Construction ).WindowTypeEQL ) {
							SetupOutputVariable( "Surface Window Inside Face Other Convection Heat Gain Rate [W]", OtherConvGainInsideFaceToZoneRep( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
						}
					}

					// Added TH 12/23/2008 for thermochromic windows
					// CurrentModuleObject='Thermochromic Windows'
					if ( Construct( Surface( SurfLoop ).Construction ).TCFlag == 1 ) {
						SetupOutputVariable( "Surface Window Thermochromic Layer Temperature [C]", SurfaceWindow( SurfLoop ).TCLayerTemp, "Zone", "Average", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Thermochromic Layer Property Specification Temperature [C]", SurfaceWindow( SurfLoop ).SpecTemp, "Zone", "Average", Surface( SurfLoop ).Name );
					}

					// Added TH 5/26/2009 for switchable windows to report switching factor (tinted level)
					// CurrentModuleObject='Switchable Windows'
					if ( Surface( SurfLoop ).WindowShadingControlPtr > 0 ) {
						if ( WindowShadingControl( Surface( SurfLoop ).WindowShadingControlPtr ).ShadingType == WSC_ST_SwitchableGlazing ) {
							//IF (SurfaceWindow(SurfLoop)%ShadingFlag == SwitchableGlazing) THEN  !ShadingFlag is not set to SwitchableGlazing yet!
							SetupOutputVariable( "Surface Window Switchable Glazing Switching Factor []", SurfaceWindow( SurfLoop ).SwitchingFactor, "Zone", "Average", Surface( SurfLoop ).Name );
							SetupOutputVariable( "Surface Window Switchable Glazing Visible Transmittance []", SurfaceWindow( SurfLoop ).VisTransSelected, "Zone", "Average", Surface( SurfLoop ).Name );
						}
					}

					if ( SurfaceWindow( SurfLoop ).FrameArea > 0.0 ) {
						// CurrentModuleObject='Window Frames'
						SetupOutputVariable( "Surface Window Frame Heat Gain Rate [W]", SurfaceWindow( SurfLoop ).FrameHeatGain, "Zone", "Average", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Frame Heat Loss Rate [W]", SurfaceWindow( SurfLoop ).FrameHeatLoss, "Zone", "Average", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Frame Inside Temperature [C]", SurfaceWindow( SurfLoop ).FrameTempSurfIn, "Zone", "Average", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Frame Outside Temperature [C]", SurfaceWindow( SurfLoop ).FrameTempSurfOut, "Zone", "Average", Surface( SurfLoop ).Name );
					}
					if ( SurfaceWindow( SurfLoop ).DividerArea > 0.0 ) {
						// CurrentModuleObject='Window Dividers'
						SetupOutputVariable( "Surface Window Divider Heat Gain Rate [W]", SurfaceWindow( SurfLoop ).DividerHeatGain, "Zone", "Average", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Divider Heat Loss Rate [W]", SurfaceWindow( SurfLoop ).DividerHeatLoss, "Zone", "Average", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Divider Inside Temperature [C]", SurfaceWindow( SurfLoop ).DividerTempSurfIn, "Zone", "Average", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Divider Outside Temperature [C]", SurfaceWindow( SurfLoop ).DividerTempSurfOut, "Zone", "Average", Surface( SurfLoop ).Name );
					}

					// CurrentModuleObject='Windows'
					// Energy
					SetupOutputVariable( "Surface Window Total Glazing Layers Absorbed Solar Radiation Energy [J]", QRadSWwinAbsTotEnergy( SurfLoop ), "Zone", "Sum", Surface( SurfLoop ).Name );
					SetupOutputVariable( "Surface Window Transmitted Solar Radiation Energy [J]", WinTransSolarEnergy( SurfLoop ), "Zone", "Sum", Surface( SurfLoop ).Name );
					SetupOutputVariable( "Surface Window Transmitted Beam Solar Radiation Energy [J]", WinBmSolarEnergy( SurfLoop ), "Zone", "Sum", Surface( SurfLoop ).Name );

					//added TH 12/9/2009
					SetupOutputVariable( "Surface Window Transmitted Beam To Beam Solar Radiation Energy [J]", WinBmBmSolarEnergy( SurfLoop ), "Zone", "Sum", Surface( SurfLoop ).Name );
					SetupOutputVariable( "Surface Window Transmitted Beam To Diffuse Solar Radiation Energy [J]", WinBmDifSolarEnergy( SurfLoop ), "Zone", "Sum", Surface( SurfLoop ).Name );

					SetupOutputVariable( "Surface Window Transmitted Diffuse Solar Radiation Energy [J]", WinDifSolarEnergy( SurfLoop ), "Zone", "Sum", Surface( SurfLoop ).Name );
					SetupOutputVariable( "Surface Window Heat Gain Energy [J]", WinHeatGainRepEnergy( SurfLoop ), "Zone", "Sum", Surface( SurfLoop ).Name );
					SetupOutputVariable( "Surface Window Heat Loss Energy [J]", WinHeatLossRepEnergy( SurfLoop ), "Zone", "Sum", Surface( SurfLoop ).Name );
					SetupOutputVariable( "Surface Window Gap Convective Heat Transfer Energy [J]", WinGapConvHtFlowRepEnergy( SurfLoop ), "Zone", "Sum", Surface( SurfLoop ).Name );
					SetupOutputVariable( "Surface Window Shading Device Absorbed Solar Radiation Energy [J]", WinShadingAbsorbedSolarEnergy( SurfLoop ), "Zone", "Sum", Surface( SurfLoop ).Name );

					SetupOutputVariable( "Surface Window System Solar Transmittance []", WinSysSolTransmittance( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
					SetupOutputVariable( "Surface Window System Solar Reflectance []", WinSysSolReflectance( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
					SetupOutputVariable( "Surface Window System Solar Absorptance []", WinSysSolAbsorptance( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
					SetupOutputVariable( "Surface Window Inside Face Glazing Condensation Status []", InsideGlassCondensationFlag( SurfLoop ), "Zone", "State", Surface( SurfLoop ).Name );
					SetupOutputVariable( "Surface Window Inside Face Frame Condensation Status []", InsideFrameCondensationFlag( SurfLoop ), "Zone", "State", Surface( SurfLoop ).Name );
					SetupOutputVariable( "Surface Window Inside Face Divider Condensation Status []", InsideDividerCondensationFlag( SurfLoop ), "Zone", "State", Surface( SurfLoop ).Name );

					// Outside reveal report variables
					//IF (Surface(SurfLoop)%Reveal > 0.0) THEN
					SetupOutputVariable( "Surface Window Outside Reveal Reflected Beam Solar Radiation Rate [W]", SurfaceWindow( SurfLoop ).BmSolRefldOutsRevealReport, "Zone", "State", Surface( SurfLoop ).Name );
					// Energy
					SetupOutputVariable( "Surface Window Outside Reveal Reflected Beam Solar Radiation Energy [J]", SurfaceWindow( SurfLoop ).BmSolRefldOutsRevealRepEnergy, "Zone", "Sum", Surface( SurfLoop ).Name );
					//ENDIF

					// Inside reveal report variables
					if ( SurfaceWindow( SurfLoop ).InsideReveal > 0.0 || SurfaceWindow( SurfLoop ).InsideSillDepth > 0.0 ) {
						SetupOutputVariable( "Surface Window Inside Reveal Reflected Beam Solar Radiation Rate [W]", SurfaceWindow( SurfLoop ).BmSolRefldInsRevealReport, "Zone", "State", Surface( SurfLoop ).Name );
						// Energy
						SetupOutputVariable( "Surface Window Inside Reveal Reflected Beam Solar Radiation Energy [J]", SurfaceWindow( SurfLoop ).BmSolRefldInsRevealRepEnergy, "Zone", "Sum", Surface( SurfLoop ).Name );

						// Added report variables for inside reveal to debug CR 7596. TH 5/26/2009
						// All reflected solar by the inside reveal is turned into diffuse
						SetupOutputVariable( "Surface Window Inside Reveal Absorbed Beam Solar Radiation Rate [W]", SurfaceWindow( SurfLoop ).BmSolAbsdInsRevealReport, "Zone", "State", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Inside Reveal Reflected Diffuse Zone Solar Radiation Rate [W]", SurfaceWindow( SurfLoop ).InsRevealDiffIntoZoneReport, "Zone", "State", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Inside Reveal Reflected Diffuse Frame Solar Radiation Rate [W]", SurfaceWindow( SurfLoop ).InsRevealDiffOntoFrameReport, "Zone", "State", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Inside Reveal Reflected Diffuse Glazing Solar Radiation Rate [W]", SurfaceWindow( SurfLoop ).InsRevealDiffOntoGlazingReport, "Zone", "State", Surface( SurfLoop ).Name );
					}

					//     Output blind report variables only when blinds are used
					if ( SurfaceWindow( SurfLoop ).BlindNumber > 0 ) {
						// CurrentModuleObject='Window Blinds'
						SetupOutputVariable( "Surface Window Blind Beam to Beam Solar Transmittance []", SurfaceWindow( SurfLoop ).BlTsolBmBm, "Zone", "State", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Blind Beam to Diffuse Solar Transmittance []", SurfaceWindow( SurfLoop ).BlTsolBmDif, "Zone", "State", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Blind Diffuse to Diffuse Solar Transmittance []", SurfaceWindow( SurfLoop ).BlTsolDifDif, "Zone", "State", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Blind and Glazing System Beam Solar Transmittance []", SurfaceWindow( SurfLoop ).BlGlSysTsolBmBm, "Zone", "State", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Blind and Glazing System Diffuse Solar Transmittance []", SurfaceWindow( SurfLoop ).BlGlSysTsolDifDif, "Zone", "State", Surface( SurfLoop ).Name );
					}

					//     Output screen report variables only when screens are used
					if ( SurfaceWindow( SurfLoop ).ScreenNumber > 0 ) {
						// CurrentModuleObject='Window Screens'
						SetupOutputVariable( "Surface Window Screen Beam to Beam Solar Transmittance []", SurfaceWindow( SurfLoop ).ScTsolBmBm, "Zone", "State", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Screen Beam to Diffuse Solar Transmittance []", SurfaceWindow( SurfLoop ).ScTsolBmDif, "Zone", "State", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Screen Diffuse to Diffuse Solar Transmittance []", SurfaceWindow( SurfLoop ).ScTsolDifDif, "Zone", "State", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Screen and Glazing System Beam Solar Transmittance []", SurfaceWindow( SurfLoop ).ScGlSysTsolBmBm, "Zone", "State", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Screen and Glazing System Diffuse Solar Transmittance []", SurfaceWindow( SurfLoop ).ScGlSysTsolDifDif, "Zone", "State", Surface( SurfLoop ).Name );
					}

					// CurrentModuleObject='Windows'
					SetupOutputVariable( "Surface Window Solar Horizontal Profile Angle [deg]", SurfaceWindow( SurfLoop ).ProfileAngHor, "Zone", "State", Surface( SurfLoop ).Name );
					SetupOutputVariable( "Surface Window Solar Vertical Profile Angle [deg]", SurfaceWindow( SurfLoop ).ProfileAngVert, "Zone", "State", Surface( SurfLoop ).Name );
					SetupOutputVariable( "Surface Window Glazing Beam to Beam Solar Transmittance []", SurfaceWindow( SurfLoop ).GlTsolBmBm, "Zone", "State", Surface( SurfLoop ).Name );
					SetupOutputVariable( "Surface Window Glazing Beam to Diffuse Solar Transmittance []", SurfaceWindow( SurfLoop ).GlTsolBmDif, "Zone", "State", Surface( SurfLoop ).Name );
					SetupOutputVariable( "Surface Window Glazing Diffuse to Diffuse Solar Transmittance []", SurfaceWindow( SurfLoop ).GlTsolDifDif, "Zone", "State", Surface( SurfLoop ).Name );
					SetupOutputVariable( "Surface Window Model Solver Iteration Count []", SurfaceWindow( SurfLoop ).WindowCalcIterationsRep, "Zone", "State", Surface( SurfLoop ).Name );
				} else if ( ! Surface( SurfLoop ).ExtSolar ) { // Not ExtSolar
					if ( DisplayAdvancedReportVariables ) {
						// CurrentModuleObject='InteriorWindows(Advanced)'
						if ( SurfaceWindow( SurfLoop ).OriginalClass != SurfaceClass_TDD_Diffuser ) SetupOutputVariable( "Surface Window Total Glazing Layers Absorbed Solar Radiation Rate [W]", QRadSWwinAbsTot( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Total Glazing Layers Absorbed Shortwave Radiation Rate [W]", SWwinAbsTotalReport( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
						if ( SurfaceWindow( SurfLoop ).OriginalClass != SurfaceClass_TDD_Diffuser ) SetupOutputVariable( "Surface Window Transmitted Solar Radiation Rate [W]", WinTransSolar( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Transmitted Beam Solar Radiation Rate [W]", WinBmSolar( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );

						//added TH 12/9/2009
						SetupOutputVariable( "Surface Window Transmitted Beam To Beam Solar Radiation Rate [W]", WinBmBmSolar( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Transmitted Beam To Diffuse Solar Radiation Rate [W]", WinBmDifSolar( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );

						SetupOutputVariable( "Surface Window Transmitted Diffuse Solar Radiation Rate [W]", WinDifSolar( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Heat Gain Rate [W]", WinHeatGainRep( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Heat Loss Rate [W]", WinHeatLossRep( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Gap Convective Heat Transfer Rate [W]", WinGapConvHtFlowRep( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Shading Device Absorbed Solar Radiation Rate [W]", WinShadingAbsorbedSolar( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
						if ( SurfaceWindow( SurfLoop ).FrameArea > 0.0 ) {
							SetupOutputVariable( "Surface Window Frame Heat Gain Rate [W]", SurfaceWindow( SurfLoop ).FrameHeatGain, "Zone", "Average", Surface( SurfLoop ).Name );
							SetupOutputVariable( "Surface Window Frame Heat Loss Rate [W]", SurfaceWindow( SurfLoop ).FrameHeatLoss, "Zone", "Average", Surface( SurfLoop ).Name );
							SetupOutputVariable( "Surface Window Frame Inside Temperature [C]", SurfaceWindow( SurfLoop ).FrameTempSurfIn, "Zone", "Average", Surface( SurfLoop ).Name );
							SetupOutputVariable( "Surface Window Frame Outside Temperature [C]", SurfaceWindow( SurfLoop ).FrameTempSurfOut, "Zone", "Average", Surface( SurfLoop ).Name );
						}
						if ( SurfaceWindow( SurfLoop ).DividerArea > 0.0 ) {
							SetupOutputVariable( "Surface Window Divider Heat Gain Rate [W]", SurfaceWindow( SurfLoop ).DividerHeatGain, "Zone", "Average", Surface( SurfLoop ).Name );
							SetupOutputVariable( "Surface Window Divider Heat Loss Rate [W]", SurfaceWindow( SurfLoop ).DividerHeatLoss, "Zone", "Average", Surface( SurfLoop ).Name );
							SetupOutputVariable( "Surface Window Divider Inside Temperature [C]", SurfaceWindow( SurfLoop ).DividerTempSurfIn, "Zone", "Average", Surface( SurfLoop ).Name );
							SetupOutputVariable( "Surface Window Divider Outside Temperature [C]", SurfaceWindow( SurfLoop ).DividerTempSurfOut, "Zone", "Average", Surface( SurfLoop ).Name );
						}
						// Energy
						if ( SurfaceWindow( SurfLoop ).OriginalClass != SurfaceClass_TDD_Diffuser ) SetupOutputVariable( "Surface Window Total Glazing Layers Absorbed Solar Radiation Energy [J]", QRadSWwinAbsTotEnergy( SurfLoop ), "Zone", "Sum", Surface( SurfLoop ).Name );
						if ( SurfaceWindow( SurfLoop ).OriginalClass != SurfaceClass_TDD_Diffuser ) SetupOutputVariable( "Surface Window Transmitted Solar Radiation Energy [J]", WinTransSolarEnergy( SurfLoop ), "Zone", "Sum", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Transmitted Beam Solar Radiation Energy [J]", WinBmSolarEnergy( SurfLoop ), "Zone", "Sum", Surface( SurfLoop ).Name );

						SetupOutputVariable( "Surface Window Transmitted Beam To Beam Solar Radiation Energy [J]", WinBmBmSolarEnergy( SurfLoop ), "Zone", "Sum", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Transmitted Beam To Diffuse Solar Radiation Energy [J]", WinBmDifSolarEnergy( SurfLoop ), "Zone", "Sum", Surface( SurfLoop ).Name );

						SetupOutputVariable( "Surface Window Transmitted Diffuse Solar Radiation Energy [J]", WinDifSolarEnergy( SurfLoop ), "Zone", "Sum", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Heat Gain Energy [J]", WinHeatGainRepEnergy( SurfLoop ), "Zone", "Sum", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Heat Loss Energy [J]", WinHeatLossRepEnergy( SurfLoop ), "Zone", "Sum", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Gap Convective Heat Transfer Energy [J]", WinGapConvHtFlowRepEnergy( SurfLoop ), "Zone", "Sum", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Shading Device Absorbed Solar Radiation Energy [J]", WinShadingAbsorbedSolarEnergy( SurfLoop ), "Zone", "Sum", Surface( SurfLoop ).Name );

						SetupOutputVariable( "Surface Window System Solar Transmittance []", WinSysSolTransmittance( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window System Solar Reflectance []", WinSysSolReflectance( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window System Solar Absorptance []", WinSysSolAbsorptance( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Inside Face Glazing Condensation Status []", InsideGlassCondensationFlag( SurfLoop ), "Zone", "State", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Inside Face Frame Condensation Status []", InsideFrameCondensationFlag( SurfLoop ), "Zone", "State", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Inside Face Divider Condensation Status []", InsideDividerCondensationFlag( SurfLoop ), "Zone", "State", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Outside Reveal Reflected Beam Solar Radiation Rate [W]", SurfaceWindow( SurfLoop ).BmSolRefldOutsRevealReport, "Zone", "State", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Inside Reveal Reflected Beam Solar Radiation Rate [W]", SurfaceWindow( SurfLoop ).BmSolRefldInsRevealReport, "Zone", "State", Surface( SurfLoop ).Name );
						// Energy
						SetupOutputVariable( "Surface Window Outside Reveal Reflected Beam Solar Radiation Energy [J]", SurfaceWindow( SurfLoop ).BmSolRefldOutsRevealRepEnergy, "Zone", "Sum", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Inside Reveal Reflected Beam Solar Radiation Energy [J]", SurfaceWindow( SurfLoop ).BmSolRefldInsRevealRepEnergy, "Zone", "Sum", Surface( SurfLoop ).Name );

						//     Output blind report variables only when blinds are used
						if ( SurfaceWindow( SurfLoop ).BlindNumber > 0 ) {
							SetupOutputVariable( "Surface Window Blind Beam to Beam Solar Transmittance []", SurfaceWindow( SurfLoop ).BlTsolBmBm, "Zone", "State", Surface( SurfLoop ).Name );
							SetupOutputVariable( "Surface Window Blind Beam to Diffuse Solar Transmittance []", SurfaceWindow( SurfLoop ).BlTsolBmDif, "Zone", "State", Surface( SurfLoop ).Name );
							SetupOutputVariable( "Surface Window Blind Diffuse to Diffuse Solar Transmittance []", SurfaceWindow( SurfLoop ).BlTsolDifDif, "Zone", "State", Surface( SurfLoop ).Name );
							SetupOutputVariable( "Surface Window Blind and Glazing System Beam Solar Transmittance []", SurfaceWindow( SurfLoop ).BlGlSysTsolBmBm, "Zone", "State", Surface( SurfLoop ).Name );
							SetupOutputVariable( "Surface Window Blind and Glazing System Diffuse Solar Transmittance []", SurfaceWindow( SurfLoop ).BlGlSysTsolDifDif, "Zone", "State", Surface( SurfLoop ).Name );
						}

						//     Output screen report variables only when screens are used
						if ( SurfaceWindow( SurfLoop ).ScreenNumber > 0 ) {
							SetupOutputVariable( "Surface Window Screen Beam to Beam Solar Transmittance []", SurfaceWindow( SurfLoop ).ScTsolBmBm, "Zone", "State", Surface( SurfLoop ).Name );
							SetupOutputVariable( "Surface Window Screen Beam to Diffuse Solar Transmittance []", SurfaceWindow( SurfLoop ).ScTsolBmDif, "Zone", "State", Surface( SurfLoop ).Name );
							SetupOutputVariable( "Surface Window Screen Diffuse to Diffuse Solar Transmittance []", SurfaceWindow( SurfLoop ).ScTsolDifDif, "Zone", "State", Surface( SurfLoop ).Name );
							SetupOutputVariable( "Surface Window Screen and Glazing System Beam Solar Transmittance []", SurfaceWindow( SurfLoop ).ScGlSysTsolBmBm, "Zone", "State", Surface( SurfLoop ).Name );
							SetupOutputVariable( "Surface Window Screen and Glazing System Diffuse Solar Transmittance []", SurfaceWindow( SurfLoop ).ScGlSysTsolDifDif, "Zone", "State", Surface( SurfLoop ).Name );
						}

						SetupOutputVariable( "Surface Window Solar Horizontal Profile Angle [deg]", SurfaceWindow( SurfLoop ).ProfileAngHor, "Zone", "State", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Solar Vertical Profile Angle [deg]", SurfaceWindow( SurfLoop ).ProfileAngVert, "Zone", "State", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Glazing Beam to Beam Solar Transmittance []", SurfaceWindow( SurfLoop ).GlTsolBmBm, "Zone", "State", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Glazing Beam to Diffuse Solar Transmittance []", SurfaceWindow( SurfLoop ).GlTsolBmDif, "Zone", "State", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Glazing Diffuse to Diffuse Solar Transmittance []", SurfaceWindow( SurfLoop ).GlTsolDifDif, "Zone", "State", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Surface Window Model Solver Iteration Count []", SurfaceWindow( SurfLoop ).WindowCalcIterationsRep, "Zone", "State", Surface( SurfLoop ).Name );
					}
				} // end non extsolar reporting as advanced variables
			} // Window Reporting
			if ( Surface( SurfLoop ).Class == SurfaceClass_Window && Surface( SurfLoop ).ExtBoundCond > 0 && Surface( SurfLoop ).ExtBoundCond != SurfLoop ) { //Interzone window
				// CurrentModuleObject='InterzoneWindows'
				SetupOutputVariable( "Surface Window Transmitted Beam Solar Radiation Rate [W]", SurfaceWindow( SurfLoop ).BmSolTransThruIntWinRep, "Zone", "State", Surface( SurfLoop ).Name );
				//energy
				SetupOutputVariable( "Surface Window Transmitted Beam Solar Radiation Energy [J]", SurfaceWindow( SurfLoop ).BmSolTransThruIntWinRepEnergy, "Zone", "Sum", Surface( SurfLoop ).Name );
			}
			if ( Surface( SurfLoop ).Class == SurfaceClass_TDD_Dome && Surface( SurfLoop ).ExtSolar ) {
				// CurrentModuleObject='TDD Domes'
				SetupOutputVariable( "Surface Window Total Glazing Layers Absorbed Solar Radiation Rate [W]", QRadSWwinAbsTot( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
				SetupOutputVariable( "Surface Window Transmitted Solar Radiation Rate [W]", WinTransSolar( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
				//energy
				SetupOutputVariable( "Surface Window Total Glazing Layers Absorbed Solar Radiation Energy [J]", QRadSWwinAbsTotEnergy( SurfLoop ), "Zone", "Sum", Surface( SurfLoop ).Name );
				SetupOutputVariable( "Surface Window Transmitted Solar Radiation Energy [J]", WinTransSolarEnergy( SurfLoop ), "Zone", "Sum", Surface( SurfLoop ).Name );
			}
			if ( SurfaceWindow( SurfLoop ).OriginalClass == SurfaceClass_TDD_Diffuser ) {
				// CurrentModuleObject='TDD Diffusers'
				SetupOutputVariable( "Surface Outside Face Incident Solar Radiation Rate per Area [W/m2]", QRadSWOutIncident( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
				SetupOutputVariable( "Surface Window Total Glazing Layers Absorbed Solar Radiation Rate [W]", QRadSWwinAbsTot( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
				SetupOutputVariable( "Surface Window Transmitted Solar Radiation Rate [W]", WinTransSolar( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
				//energy
				SetupOutputVariable( "Surface Window Total Glazing Layers Absorbed Solar Radiation Energy [J]", QRadSWwinAbsTotEnergy( SurfLoop ), "Zone", "Sum", Surface( SurfLoop ).Name );
				SetupOutputVariable( "Surface Window Transmitted Solar Radiation Energy [J]", WinTransSolarEnergy( SurfLoop ), "Zone", "Sum", Surface( SurfLoop ).Name );
			}
		}

		for ( SurfLoop = 1; SurfLoop <= TotSurfaces; ++SurfLoop ) {
			if ( ! Surface( SurfLoop ).HeatTransSurf ) continue;
			// CurrentModuleObject='Surfaces'
			SetupOutputVariable( "Surface Inside Face Exterior Windows Incident Beam Solar Radiation Rate per Area [W/m2]", BmIncInsSurfIntensRep( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
			SetupOutputVariable( "Surface Inside Face Exterior Windows Incident Beam Solar Radiation Rate [W]", BmIncInsSurfAmountRep( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
			SetupOutputVariable( "Surface Inside Face Interior Windows Incident Beam Solar Radiation Rate per Area [W/m2]", IntBmIncInsSurfIntensRep( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
			SetupOutputVariable( "Surface Inside Face Interior Windows Incident Beam Solar Radiation Rate [W]", IntBmIncInsSurfAmountRep( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
			SetupOutputVariable( "Surface Inside Face Initial Transmitted Diffuse Absorbed Solar Radiation Rate [W]", InitialDifSolInAbsReport( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
			SetupOutputVariable( "Surface Inside Face Initial Transmitted Diffuse Transmitted Out Window Solar Radiation Rate [W]", InitialDifSolInTransReport( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
			SetupOutputVariable( "Surface Inside Face Absorbed Shortwave Radiation Rate [W]", SWInAbsTotalReport( SurfLoop ), "Zone", "Average", Surface( SurfLoop ).Name );
			//energy
			SetupOutputVariable( "Surface Inside Face Exterior Windows Incident Beam Solar Radiation Energy [J]", BmIncInsSurfAmountRepEnergy( SurfLoop ), "Zone", "Sum", Surface( SurfLoop ).Name );
			SetupOutputVariable( "Surface Inside Face Interior Windows Incident Beam Solar Radiation Energy [J]", IntBmIncInsSurfAmountRepEnergy( SurfLoop ), "Zone", "Sum", Surface( SurfLoop ).Name );
		}

	}

	void
	AnisoSkyViewFactors()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   April 1999
		//       MODIFIED       LKL; Dec 2002 -- Anisotropic is only sky radiance option
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates view factor multiplier, AnisoSkyMult, for diffuse
		// sky irradiance on exterior surfaces taking into account
		// anisotropic radiance of the sky. Called by InitSurfaceHeatBalance
		// In this case the diffuse sky irradiance on a surface is given by
		//  AnisoSkyMult(SurfNum) * DifSolarRad
		// AnisoSkyMult accounts not only for the sky radiance distribution but
		// also for the effects of shading of sky diffuse radiation by
		// shadowing surfaces such as overhangs. It does not account for reflection
		// of sky diffuse radiation from shadowing surfaces.
		// Based on an empirical model described in
		// R. Perez, P. Ineichen, R. Seals, J. Michalsky and R. Stewart,
		// "Modeling Daylight Availability and Irradiance Components from Direct
		// and Global Irradiance," Solar Energy 44, 271-289, 1990.
		// In this model the radiance of the sky consists of three distributions
		// that are superimposed:

		// (1) An isotropic distribution that covers the entire sky dome;
		// (2) A circumsolar brightening centered around the position of the sun;
		// (3) A horizon brightening
		// The circumsolar brightening is assumed to be concentrated at a point
		// source at the center of the sun although this region actually begins at the
		// periphery of the solar disk and falls off in intensity with increasing
		// angular distance from the periphery.
		// The horizon brightening is assumed to be concentrated at the horizon and
		// to be independent of azimuth. In actuality, for clear skies, the horizon
		// brightening is highest at the horizon and decreases in intensity away from
		// the horizon. For overcast skies the horizon brightening has a negative value
		// since for such skies the sky radiance increases rather than decreases away
		// from the horizon.
		// The F11R, F12R, etc. values were provided by R. Perez, private communication,
		// 5/21/99. These values have higher precision than those listed in the above
		// paper.

		// Using/Aliasing
		using DataSystemVariables::DetailedSkyDiffuseAlgorithm;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		static Array1D< Real64 > const EpsilonLimit( 7, { 1.065, 1.23, 1.5, 1.95, 2.8, 4.5, 6.2 } ); // Upper limit of bins of the sky clearness parameter, Epsilon
		// Circumsolar brightening coefficients; index corresponds to range of Epsilon, the sky clearness parameter
		static Array1D< Real64 > const F11R( 8, { -0.0083117, 0.1299457, 0.3296958, 0.5682053, 0.8730280, 1.1326077, 1.0601591, 0.6777470 } );
		static Array1D< Real64 > const F12R( 8, { 0.5877285, 0.6825954, 0.4868735, 0.1874525, -0.3920403, -1.2367284, -1.5999137, -0.3272588 } );
		static Array1D< Real64 > const F13R( 8, { -0.0620636, -0.1513752, -0.2210958, -0.2951290, -0.3616149, -0.4118494, -0.3589221, -0.2504286 } );
		// Horizon/zenith brightening coefficient array; index corresponds to range of Epsilon, the sky clearness parameter
		static Array1D< Real64 > const F21R( 8, { -0.0596012, -0.0189325, 0.0554140, 0.1088631, 0.2255647, 0.2877813, 0.2642124, 0.1561313 } );
		static Array1D< Real64 > const F22R( 8, { 0.0721249, 0.0659650, -0.0639588, -0.1519229, -0.4620442, -0.8230357, -1.1272340, -1.3765031 } );
		static Array1D< Real64 > const F23R( 8, { -0.0220216, -0.0288748, -0.0260542, -0.0139754, 0.0012448, 0.0558651, 0.1310694, 0.2506212 } );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Real64 CosZenithAng; // Cosine of solar zenith angle
		Real64 ZenithAng; // Solar zenith angle (radians)
		Real64 ZenithAngDeg; // Solar zenith angle (degrees)
		Real64 F1; // Circumsolar brightening coefficient
		Real64 F2; // Horizon/zenith brightening coefficient
		Real64 Epsilon; // Sky clearness parameter
		Real64 Delta; // Sky brightness parameter
		Real64 CosIncAngBeamOnSurface; // Cosine of incidence angle of beam solar on surface
		Real64 IncAng; // Incidence angle of beam solar on surface (radians)
		int SurfNum; // Surface number
		int EpsilonBin; // Sky clearness (Epsilon) bin index
		Real64 AirMass; // Relative air mass
		Real64 AirMassH; // Intermediate variable for relative air mass calculation
		Real64 CircumSolarFac; // Ratio of cosine of incidence angle to cosine of zenith angle
		Real64 KappaZ3; // Intermediate variable
		Real64 ViewFactorSkyGeom; // Geometrical sky view factor
		Real64 const cosine_tolerance( 0.0001 );

		// FLOW:
#ifdef EP_Count_Calls
		++NumAnisoSky_Calls;
#endif

		CosZenithAng = SOLCOS( 3 );
		ZenithAng = std::acos( CosZenithAng );
		ZenithAngDeg = ZenithAng / DegToRadians;

		AnisoSkyMult = 0.0;

		//           Relative air mass
		AirMassH = ( 1.0 - 0.1 * Elevation / 1000.0 );
		if ( ZenithAngDeg <= 75.0 ) {
			AirMass = AirMassH / CosZenithAng;
		} else {
			AirMass = AirMassH / ( CosZenithAng + 0.15 * std::pow( 93.9 - ZenithAngDeg, -1.253 ) );
		}
		KappaZ3 = 1.041 * pow_3( ZenithAng );
		Epsilon = ( ( BeamSolarRad + DifSolarRad ) / DifSolarRad + KappaZ3 ) / ( 1.0 + KappaZ3 );
		Delta = DifSolarRad * AirMass / 1353.0; // 1353 is average extraterrestrial irradiance (W/m2)
		//           Circumsolar (F1) and horizon/zenith (F2) brightening coefficients
		for ( EpsilonBin = 1; EpsilonBin <= 8; ++EpsilonBin ) {
			if ( EpsilonBin == 8 ) break;
			if ( Epsilon < EpsilonLimit( EpsilonBin ) ) break;
		}
		F1 = max( 0.0, F11R( EpsilonBin ) + F12R( EpsilonBin ) * Delta + F13R( EpsilonBin ) * ZenithAng );
		F2 = F21R( EpsilonBin ) + F22R( EpsilonBin ) * Delta + F23R( EpsilonBin ) * ZenithAng;

		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			if ( ! Surface( SurfNum ).ExtSolar ) continue;

			CosIncAngBeamOnSurface = SOLCOS( 1 ) * Surface( SurfNum ).OutNormVec( 1 ) + SOLCOS( 2 ) * Surface( SurfNum ).OutNormVec( 2 ) + SOLCOS( 3 ) * Surface( SurfNum ).OutNormVec( 3 );

			// So I believe this should only be a diagnostic error...the calcs should always be within -1,+1; it's just round-off that we need to trap for
			if ( CosIncAngBeamOnSurface > 1.0 ) {
				if ( CosIncAngBeamOnSurface > ( 1.0 + cosine_tolerance ) ) {
					ShowSevereError( "Cosine of incident angle of beam solar on surface out of range...too high" );
					ShowContinueError("This is a diagnostic error that should not be encountered under normal circumstances");
					ShowContinueError( "Occurs on surface: " + Surface ( SurfNum ).Name );
					ShowContinueError( "Current value = " + TrimSigDigits( CosIncAngBeamOnSurface ) + " ... should be within [-1, +1]" );
					ShowFatalError( "Anisotropic solar calculation causes fatal error" );
				}
				CosIncAngBeamOnSurface = 1.0;
			} else if ( CosIncAngBeamOnSurface < -1.0 ) {
				if ( CosIncAngBeamOnSurface < ( -1.0 - cosine_tolerance ) ) {
					ShowSevereError( "Cosine of incident angle of beam solar on surface out of range...too low" );
					ShowContinueError("This is a diagnostic error that should not be encountered under normal circumstances");
					ShowContinueError( "Occurs on surface: " + Surface ( SurfNum ).Name );
					ShowContinueError( "Current value = " + TrimSigDigits( CosIncAngBeamOnSurface ) + " ... should be within [-1, +1]" );
					ShowFatalError( "Anisotropic solar calculation causes fatal error" );
				}
				CosIncAngBeamOnSurface = -1.0;
			}

			IncAng = std::acos( CosIncAngBeamOnSurface );

			ViewFactorSkyGeom = Surface( SurfNum ).ViewFactorSky;
			MultIsoSky( SurfNum ) = ViewFactorSkyGeom * ( 1.0 - F1 );
			//           0.0871557 below corresponds to a zenith angle of 85 deg
			CircumSolarFac = max( 0.0, CosIncAngBeamOnSurface ) / max( 0.0871557, CosZenithAng );
			//           For near-horizontal roofs, model has an inconsistency that gives sky diffuse
			//           irradiance significantly different from DifSolarRad when zenith angle is
			//           above 85 deg. The following forces irradiance to be very close to DifSolarRad
			//           in this case.
			if ( CircumSolarFac > 0.0 && CosZenithAng < 0.0871557 && Surface( SurfNum ).Tilt < 2.0 ) CircumSolarFac = 1.0;
			MultCircumSolar( SurfNum ) = F1 * CircumSolarFac;
			MultHorizonZenith( SurfNum ) = F2 * Surface( SurfNum ).SinTilt;
			if ( ! DetailedSkyDiffuseAlgorithm || ! ShadingTransmittanceVaries || SolarDistribution == MinimalShadowing ) {
				AnisoSkyMult( SurfNum ) = MultIsoSky( SurfNum ) * DifShdgRatioIsoSky( SurfNum ) + MultCircumSolar( SurfNum ) * SunlitFrac( TimeStep, HourOfDay, SurfNum ) + MultHorizonZenith( SurfNum ) * DifShdgRatioHoriz( SurfNum );
			} else {
				AnisoSkyMult( SurfNum ) = MultIsoSky( SurfNum ) * DifShdgRatioIsoSkyHRTS( TimeStep, HourOfDay, SurfNum ) + MultCircumSolar( SurfNum ) * SunlitFrac( TimeStep, HourOfDay, SurfNum ) + MultHorizonZenith( SurfNum ) * DifShdgRatioHorizHRTS( TimeStep, HourOfDay, SurfNum );
				curDifShdgRatioIsoSky( SurfNum ) = DifShdgRatioIsoSkyHRTS( TimeStep, HourOfDay, SurfNum );
			}
			AnisoSkyMult( SurfNum ) = max( 0.0, AnisoSkyMult( SurfNum ) ); // make sure not negative.
		}

	}

	void
	CHKBKS(
		int const NBS, // Surface Number of the potential back surface
		int const NRS // Surface Number of the potential shadow receiving surface
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Legacy Code
		//       DATE WRITTEN
		//       MODIFIED       Nov 2001, FW: Reverse subroutine arguments NRS and NBS to
		//                                    correspond to how CHKBKS is called
		//                      Jan 2002, FW: change error message
		//       RE-ENGINEERED  Lawrie, Oct 2000

		// PURPOSE OF THIS SUBROUTINE:
		// Determines whether a any vertices of the back surface are in front of the receiving surface;
		// if so, gives severe error.  Only base heat transfer surfaces are checked.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// BLAST/IBLAST code, original author George Walton

		// Using/Aliasing
		using namespace Vectors;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt ValFmt( "(F20.4)" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int N; // Loop Control (vertex counter)
		int NVRS; // Number of vertices of the receiving surface
		int NVBS; // Number of vertices of the back surface
		Real64 DOTP; // Dot product of C and D
		std::string CharDotP; // for error messages
		std::string VTString;

		// Object Data
		Vector AVec; // Vector from vertex 2 to vertex 1, both same surface
		Vector BVec; // Vector from vertex 2 to vertex 3, both same surface
		Vector CVec; // Vector perpendicular to surface at vertex 2
		Vector DVec; // Vector from vertex 2 of first surface to vertex 'n' of second surface

		NVRS = Surface( NRS ).Sides;
		NVBS = Surface( NBS ).Sides;

		// SEE IF ANY VERTICES OF THE back surface ARE IN FRONT OF THE receiving surface

		AVec = Surface( NRS ).Vertex( 1 ) - Surface( NRS ).Vertex( 2 );
		BVec = Surface( NRS ).Vertex( 3 ) - Surface( NRS ).Vertex( 2 );

		CVec = cross( BVec, AVec );

		for ( N = 1; N <= NVBS; ++N ) {
			DVec = Surface( NBS ).Vertex( N ) - Surface( NRS ).Vertex( 2 );
			DOTP = dot( CVec, DVec );
			if ( DOTP > 0.0009 ) {
				ShowSevereError( "Problem in interior solar distribution calculation (CHKBKS)" );
				ShowContinueError( "   Solar Distribution = FullInteriorExterior will not work in Zone=" + Surface( NRS ).ZoneName );
				gio::write( VTString, "(I4)" ) << N;
				strip( VTString );
				ShowContinueError( "   because vertex " + VTString + " of back surface=" + Surface( NBS ).Name + " is in front of receiving surface=" + Surface( NRS ).Name );
				gio::write( CharDotP, ValFmt ) << DOTP;
				strip( CharDotP );
				ShowContinueError( "   (Dot Product indicator=" + CharDotP + ')' );
				ShowContinueError( "   Check surface geometry; if OK, use Solar Distribution = FullExterior instead." );
			}
		}

	}

	void
	CHKGSS(
		int const NRS, // Surface number of the potential shadow receiving surface
		int const NSS, // Surface number of the potential shadow casting surface
		Real64 const ZMIN, // Lowest point of the receiving surface
		bool & CannotShade // TRUE if shadow casting surface cannot shade receiving surface.
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Legacy Code
		//       DATE WRITTEN
		//       MODIFIED       na
		//       RE-ENGINEERED  Lawrie, Oct 2000

		// PURPOSE OF THIS SUBROUTINE:
		// Determines the possible shadowing combinations.  The
		// routine checks detached shadowing or base heat transfer surfaces
		// for the possibility that they cannot shade a given base heat transfer surface.

		// METHODOLOGY EMPLOYED:
		// Shadowing is not possible if:
		// 1.  The lowest point of the shadow receiving surface (receiving surface)
		//     Is higher than the highest point of the shadow casting surface (s.s.)
		// 2.  The shadow casting surface Faces up (e.g. A flat roof)
		// 3.  The shadow casting surface Is behind the receiving surface
		// 4.  The receiving surface is behind the shadow casting surface

		// REFERENCES:
		// BLAST/IBLAST code, original author George Walton

		// Using/Aliasing
		using namespace Vectors;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static Real64 TolValue( 0.0003 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// Object Data

		CannotShade = true;

		// see if no point of shadow casting surface is above low point of receiving surface

		auto const & surface_C( Surface( NSS ) );
		if ( surface_C.OutNormVec( 3 ) > 0.9999 ) return; // Shadow Casting Surface is horizontal and facing upward
		auto const & vertex_C( surface_C.Vertex );
		Real64 ZMAX( vertex_C( 1 ).z );
		for ( int i = 2, e = surface_C.Sides; i <= e; ++i ) {
			ZMAX = std::max( ZMAX, vertex_C( i ).z );
		}
		if ( ZMAX <= ZMIN ) return;

		// SEE IF ANY VERTICES OF THE Shadow Casting Surface ARE ABOVE THE PLANE OF THE receiving surface

		auto const & surface_R( Surface( NRS ) );
		auto const & vertex_R( surface_R.Vertex );
		auto const vertex_R_2( vertex_R( 2 ) );
		Vector const AVec( vertex_R( 1 ) - vertex_R_2 ); // Vector from vertex 2 to vertex 1 of receiving surface
		Vector const BVec( vertex_R( 3 ) - vertex_R_2 ); // Vector from vertex 2 to vertex 3 of receiving surface

		Vector const CVec( cross( BVec, AVec ) ); // Vector perpendicular to surface at vertex 2

		int const NVSS = surface_C.Sides; // Number of vertices of the shadow casting surface
		Real64 DOTP( 0.0 ); // Dot Product
		for ( int I = 1; I <= NVSS; ++I ) {
			DOTP = dot( CVec, vertex_C( I ) - vertex_R_2 );
			if ( DOTP > TolValue ) break; // DO loop
		}

		// SEE IF ANY VERTICES OF THE receiving surface ARE ABOVE THE PLANE OF THE S.S.

		if ( DOTP > TolValue ) {

			auto const vertex_C_2( vertex_C( 2 ) );
			Vector const AVec( vertex_C( 1 ) - vertex_C_2 );
			Vector const BVec( vertex_C( 3 ) - vertex_C_2 );

			Vector const CVec( cross( BVec, AVec ) );

			int const NVRS = surface_R.Sides; // Number of vertices of the receiving surface
			for ( int I = 1; I <= NVRS; ++I ) {
				DOTP = dot( CVec, vertex_R( I ) - vertex_C_2 );
				if ( DOTP > TolValue ) {
					CannotShade = false;
					break; // DO loop
				}
			}

		}

	}

	void
	CHKSBS(
		int const HTS, // Heat transfer surface number of the general receiving surf
		int const GRSNR, // Surface number of general receiving surface
		int const SBSNR // Surface number of subsurface
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Legacy Code
		//       DATE WRITTEN
		//       MODIFIED       na
		//       RE-ENGINEERED  Lawrie, Oct 2000

		// PURPOSE OF THIS SUBROUTINE:
		// Checks that a subsurface is completely
		// enclosed by its base surface.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// BLAST/IBLAST code, original author George Walton

		// 3D Planar Polygons
		// In 3D applications, one sometimes wants to test a point and polygon that are in the same plane.
		// For example, one may have the intersection point of a ray with the plane of a polyhedron's face,
		// and want to test if it is inside the face.  Or one may want to know if the base of a 3D perpendicular
		// dropped from a point is inside a planar polygon.

		// 3D inclusion is easily determined by projecting the point and polygon into 2D.  To do this, one simply
		// ignores one of the 3D coordinates and uses the other two.  To optimally select the coordinate to ignore,
		// compute a normal vector to the plane, and select the coordinate with the largest absolute value [Snyder & Barr, 1987].
		// This gives the projection of the polygon with maximum area, and results in robust computations.
		// John M. Snyder & Alan H. Barr, "Ray Tracing Complex Models Containing Surface Tessellations",
		// Computer Graphics 21(4), 119-126 (1987) [also in the Proceedings of SIGGRAPH 1987]
		//--- using adapted routine from Triangulation code -- EnergyPlus.

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// MSG - for error message
		static Array1D_string const MSG( 4, { "misses", "", "within", "overlaps" } );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int N; // Loop Control
		int NVT; // Number of vertices
		static Array1D< Real64 > XVT; // X Vertices of
		static Array1D< Real64 > YVT; // Y vertices of
		static Array1D< Real64 > ZVT; // Z vertices of

		int NS1; // Number of the figure being overlapped
		int NS2; // Number of the figure doing overlapping
		int NS3; // Location to place results of overlap

		static bool OneTimeFlag( true );
		bool inside;

		bool Out;
		Real64 X1; // ,SX,SY,SZ
		Real64 Y1;
		Real64 Z1;
		Real64 X2;
		Real64 Y2;
		Real64 Z2;
		Real64 BX;
		Real64 BY;
		Real64 BZ;
		Real64 BMAX;
		//  INTEGER M

		if ( OneTimeFlag ) {
			XVT.allocate( MaxVerticesPerSurface + 1 );
			YVT.allocate( MaxVerticesPerSurface + 1 );
			ZVT.allocate( MaxVerticesPerSurface + 1 );
			XVT = 0.0;
			YVT = 0.0;
			ZVT = 0.0;
			OneTimeFlag = false;
		}

		NS1 = 1;
		NS2 = 2;
		NS3 = 3;
		HCT( 1 ) = 0.0;
		HCT( 2 ) = 0.0;

		// Put coordinates of base surface into clockwise sequence on the x'-y' plane.

		XVT = 0.0;
		YVT = 0.0;
		ZVT = 0.0;
		XVS = 0.0;
		YVS = 0.0;
		CTRANS( GRSNR, HTS, NVT, XVT, YVT, ZVT );
		for ( N = 1; N <= NVT; ++N ) {
			XVS( N ) = XVT( NVT + 1 - N );
			YVS( N ) = YVT( NVT + 1 - N );
		}

		HTRANS1( NS2, NVT );

		// Put coordinates of the subsurface into clockwise sequence.

		NVS = Surface( SBSNR ).Sides;
		for ( N = 1; N <= NVS; ++N ) {
			XVS( N ) = ShadeV( SBSNR ).XV( NVS + 1 - N );
			YVS( N ) = ShadeV( SBSNR ).YV( NVS + 1 - N );
		}
		HTRANS1( NS1, NVS );

		// Determine the overlap condition.

		DeterminePolygonOverlap( NS1, NS2, NS3 );

		// Print error condition if necessary.

		if ( OverlapStatus != FirstSurfWithinSecond ) {
			Out = false;
			//C                            COMPUTE COMPONENTS OF VECTOR
			//C                            NORMAL TO BASE SURFACE.
			X1 = Surface( GRSNR ).Vertex( 1 ).x - Surface( GRSNR ).Vertex( 2 ).x; //XV(1,GRSNR)-XV(2,GRSNR)
			Y1 = Surface( GRSNR ).Vertex( 1 ).y - Surface( GRSNR ).Vertex( 2 ).y; //YV(1,GRSNR)-YV(2,GRSNR)
			Z1 = Surface( GRSNR ).Vertex( 1 ).z - Surface( GRSNR ).Vertex( 2 ).z; //ZV(1,GRSNR)-ZV(2,GRSNR)
			X2 = Surface( GRSNR ).Vertex( 3 ).x - Surface( GRSNR ).Vertex( 2 ).x; //XV(3,GRSNR)-XV(2,GRSNR)
			Y2 = Surface( GRSNR ).Vertex( 3 ).y - Surface( GRSNR ).Vertex( 2 ).y; //YV(3,GRSNR)-YV(2,GRSNR)
			Z2 = Surface( GRSNR ).Vertex( 3 ).z - Surface( GRSNR ).Vertex( 2 ).z; //ZV(3,GRSNR)-ZV(2,GRSNR)
			BX = Y1 * Z2 - Y2 * Z1;
			BY = Z1 * X2 - Z2 * X1;
			BZ = X1 * Y2 - X2 * Y1;
			//C                            FIND LARGEST COMPONENT.
			BMAX = max( std::abs( BX ), std::abs( BY ), std::abs( BZ ) );
			//C
			if ( std::abs( BX ) == BMAX ) {
				//        write(outputfiledebug,*) ' looking bx-bmax',bmax
				for ( N = 1; N <= Surface( SBSNR ).Sides; ++N ) { //NV(SBSNR)
					inside = polygon_contains_point( Surface( GRSNR ).Sides, Surface( GRSNR ).Vertex, Surface( SBSNR ).Vertex( N ), true, false, false );
					if ( ! inside ) {
						Out = true;
						//            do m=1,surface(grsnr)%sides
						//            write(outputfiledebug,*) 'grsnr,side=',m,surface(grsnr)%vertex
						//            write(outputfiledebug,*) 'point outside=',surface(sbsnr)%vertex(n)
						//            enddo
						//            EXIT
					}
					//          Y1 = Surface(GRSNR)%Vertex(Surface(GRSNR)%Sides)%Y-Surface(SBSNR)%Vertex(N)%Y !YV(NV(GRSNR),GRSNR)-YV(N,SBSNR)
					//          Z1 = Surface(GRSNR)%Vertex(Surface(GRSNR)%Sides)%Z-Surface(SBSNR)%Vertex(N)%Z !ZV(NV(GRSNR),GRSNR)-ZV(N,SBSNR)
					//          DO M=1,Surface(GRSNR)%Sides !NV(GRSNR)
					//            Y2 = Y1
					//            Z2 = Z1
					//            Y1 = Surface(GRSNR)%Vertex(M)%Y-Surface(SBSNR)%Vertex(N)%Y !YV(M,GRSNR)-YV(N,SBSNR)
					//            Z1 = Surface(GRSNR)%Vertex(M)%Z-Surface(SBSNR)%Vertex(N)%Z !ZV(M,GRSNR)-ZV(N,SBSNR)
					//            SX = Y1*Z2-Y2*Z1
					//            IF(SX*BX.LT.-1.0d-6) THEN
					//              OUT=.TRUE.
					//              write(outputfiledebug,*) 'sx*bx=',sx*bx
					//              write(outputfiledebug,*) 'grsnr=',surface(grsnr)%vertex(m)
					//              write(outputfiledebug,*) 'sbsnr=',surface(sbsnr)%vertex(n)
					//            endif
					//          ENDDO
					//          IF (OUT) EXIT
				}
			} else if ( std::abs( BY ) == BMAX ) {
				//        write(outputfiledebug,*) ' looking by-bmax',bmax
				for ( N = 1; N <= Surface( SBSNR ).Sides; ++N ) { //NV(SBSNR)
					inside = polygon_contains_point( Surface( GRSNR ).Sides, Surface( GRSNR ).Vertex, Surface( SBSNR ).Vertex( N ), false, true, false );
					if ( ! inside ) {
						Out = true;
						//            do m=1,surface(grsnr)%sides
						//            write(outputfiledebug,*) 'grsnr,side=',m,surface(grsnr)%vertex
						//            write(outputfiledebug,*) 'point outside=',surface(sbsnr)%vertex(n)
						//            enddo
						//            EXIT
					}
					//          Z1 = Surface(GRSNR)%Vertex(Surface(GRSNR)%Sides)%Z-Surface(SBSNR)%Vertex(N)%Z !ZV(NV(GRSNR),GRSNR)-ZV(N,SBSNR)
					//          X1 = Surface(GRSNR)%Vertex(Surface(GRSNR)%Sides)%X-Surface(SBSNR)%Vertex(N)%X !XV(NV(GRSNR),GRSNR)-XV(N,SBSNR)
					//          DO M=1,Surface(GRSNR)%Sides !NV(GRSNR)
					//            Z2 = Z1
					//            X2 = X1
					//            Z1 = Surface(GRSNR)%Vertex(M)%Z-Surface(SBSNR)%Vertex(N)%Z !ZV(M,GRSNR)-ZV(N,SBSNR)
					//            X1 = Surface(GRSNR)%Vertex(M)%X-Surface(SBSNR)%Vertex(N)%X !XV(M,GRSNR)-XV(N,SBSNR)
					//            SY = Z1*X2-Z2*X1
					//            IF(SY*BY.LT.-1.0d-6) THEN
					//              OUT=.TRUE.
					//              write(outputfiledebug,*) 'sy*by=',sy*by
					//              write(outputfiledebug,*) 'grsnr=',surface(grsnr)%vertex(m)
					//              write(outputfiledebug,*) 'sbsnr=',surface(sbsnr)%vertex(n)
					//            ENDIF
					//          ENDDO
					//          IF (OUT) EXIT
				}
			} else {
				//        write(outputfiledebug,*) ' looking bz-bmax',bmax
				for ( N = 1; N <= Surface( SBSNR ).Sides; ++N ) { //NV(SBSNR)
					inside = polygon_contains_point( Surface( GRSNR ).Sides, Surface( GRSNR ).Vertex, Surface( SBSNR ).Vertex( N ), false, false, true );
					if ( ! inside ) {
						Out = true;
						//            do m=1,surface(grsnr)%sides
						//            write(outputfiledebug,*) 'grsnr,side=',m,surface(grsnr)%vertex
						//            write(outputfiledebug,*) 'point outside=',surface(sbsnr)%vertex(n)
						//            enddo
						//            EXIT
					}
					//          X1 = Surface(GRSNR)%Vertex(Surface(GRSNR)%Sides)%X-Surface(SBSNR)%Vertex(N)%X !XV(NV(GRSNR),GRSNR)-XV(N,SBSNR)
					//          Y1 = Surface(GRSNR)%Vertex(Surface(GRSNR)%Sides)%Y-Surface(SBSNR)%Vertex(N)%Y !YV(NV(GRSNR),GRSNR)-YV(N,SBSNR)
					//          DO M=1,Surface(GRSNR)%Sides !NV(GRSNR)
					//            X2 = X1
					//            Y2 = Y1
					//            X1 = Surface(GRSNR)%Vertex(M)%X-Surface(SBSNR)%Vertex(N)%X !XV(M,GRSNR)-XV(N,SBSNR)
					//            Y1 = Surface(GRSNR)%Vertex(M)%Y-Surface(SBSNR)%Vertex(N)%Y !YV(M,GRSNR)-YV(N,SBSNR)
					//            SZ = X1*Y2-X2*Y1
					//            IF(SZ*BZ.LT.-1.0d-6) THEN
					//              OUT=.TRUE.
					//              write(outputfiledebug,*) 'sz*bz=',sz*bz
					//              write(outputfiledebug,*) 'grsnr=',surface(grsnr)%vertex(m)
					//              write(outputfiledebug,*) 'sbsnr=',surface(sbsnr)%vertex(n)
					//            ENDIF
					//          ENDDO
					//          IF (OUT) EXIT
				}
			}
			//    CALL ShowWarningError('Base surface does not surround subsurface (CHKSBS), Overlap Status='//  &
			//                           TRIM(cOverLapStatus(OverlapStatus)))
			//    CALL ShowContinueError('Surface "'//TRIM(Surface(GRSNR)%Name)//'" '//TRIM(MSG(OverlapStatus))//  &
			//                     ' SubSurface "'//TRIM(Surface(SBSNR)%Name)//'"')
			//    IF (FirstSurroundError) THEN
			//      CALL ShowWarningError('Base Surface does not surround subsurface errors occuring...'//  &
			//                     'Check that the SurfaceGeometry object is expressing the proper starting corner and '//  &
			//                     'direction [CounterClockwise/Clockwise]')
			//      FirstSurroundError=.FALSE.
			//    ENDIF
			if ( Out ) {
				TrackBaseSubSurround.redimension( ++NumBaseSubSurround );
				TrackBaseSubSurround( NumBaseSubSurround ).SurfIndex1 = GRSNR;
				TrackBaseSubSurround( NumBaseSubSurround ).SurfIndex2 = SBSNR;
				TrackBaseSubSurround( NumBaseSubSurround ).MiscIndex = OverlapStatus;
				//    CALL ShowRecurringWarningErrorAtEnd('Base surface does not surround subsurface (CHKSBS), Overlap Status='//  &
				//                       TRIM(cOverLapStatus(OverlapStatus)), &
				//                       TrackBaseSubSurround(GRSNR)%ErrIndex1)
				//    CALL ShowRecurringContinueErrorAtEnd('Surface "'//TRIM(Surface(GRSNR)%Name)//'" '//TRIM(MSG(OverlapStatus))//  &
				//                       ' SubSurface "'//TRIM(Surface(SBSNR)%Name)//'"',  &
				//                      TrackBaseSubSurround(SBSNR)%ErrIndex2)

				shd_stream << "==== Base does not Surround subsurface details ====\n";
				shd_stream << "Surface=" << Surface( GRSNR ).Name << ' ' << cOverLapStatus( OverlapStatus ) << '\n';
				shd_stream << "Surface#=" << std::setw( 5 ) << GRSNR << " NSides=" << std::setw( 5 ) << Surface( GRSNR ).Sides << '\n';
				shd_stream << std::fixed << std::setprecision( 2 );
				for ( N = 1; N <= Surface( GRSNR ).Sides; ++N ) {
					Vector const & v( Surface( GRSNR ).Vertex( N ) );
					shd_stream << "Vertex " << std::setw( 5 ) << N << "=(" << std::setw( 15 ) << v.x << ',' << std::setw( 15 ) << v.y << ',' << std::setw( 15 ) << v.z << ")\n";
				}
				shd_stream << "SubSurface=" << Surface( SBSNR ).Name << '\n';
				shd_stream << "Surface#=" << std::setw( 5 ) << SBSNR << " NSides=" << std::setw( 5 ) << Surface( SBSNR ).Sides << '\n';
				for ( N = 1; N <= Surface( SBSNR ).Sides; ++N ) {
					Vector const & v( Surface( SBSNR ).Vertex( N ) );
					shd_stream << "Vertex " << std::setw( 5 ) << N << "=(" << std::setw( 15 ) << v.x << ',' << std::setw( 15 ) << v.y << ',' << std::setw( 15 ) << v.z << ")\n";
				}
				shd_stream << "================================\n";
			}
		}

	}

	bool
	polygon_contains_point(
		int const nsides, // number of sides (vertices)
		Array1A< Vector > polygon_3d, // points of polygon
		Vector const & point_3d, // point to be tested
		bool const ignorex,
		bool const ignorey,
		bool const ignorez
	)
	{

		// Function information:
		//       Author         Linda Lawrie
		//       Date written   October 2005
		//       Modified       na
		//       Re-engineered  na

		// Purpose of this function:
		// Determine if a point is inside a simple 2d polygon.  For a simple polygon (one whose
		// boundary never crosses itself).  The polygon does not need to be convex.

		// Methodology employed:
		// <Description>

		// References:
		// M Shimrat, Position of Point Relative to Polygon, ACM Algorithm 112,
		// Communications of the ACM, Volume 5, Number 8, page 434, August 1962.

		// Use statements:
		// Using/Aliasing
		using namespace DataVectorTypes;

		// Return value
		bool inside; // return value, true=inside, false = not inside

		// Argument array dimensioning
		polygon_3d.dim( nsides );

		// Locals
		// Function argument definitions:

		// Function parameter definitions:

		// Interface block specifications:
		// na

		// Derived type definitions:
		// na

		// Function local variable declarations:
		int i;
		int ip1;

		// Object Data
		Array1D< Vector_2d > polygon( nsides );
		Vector_2d point;

		inside = false;
		if ( ignorex ) {
			for ( int i = 1; i <= nsides; ++i ) {
				polygon( i ).x = polygon_3d( i ).y;
				polygon( i ).y = polygon_3d( i ).z;
			}
			point.x = point_3d.y;
			point.y = point_3d.z;
		} else if ( ignorey ) {
			for ( int i = 1; i <= nsides; ++i ) {
				polygon( i ).x = polygon_3d( i ).x;
				polygon( i ).y = polygon_3d( i ).z;
			}
			point.x = point_3d.x;
			point.y = point_3d.z;
		} else if ( ignorez ) {
			for ( int i = 1; i <= nsides; ++i ) {
				polygon( i ).x = polygon_3d( i ).x;
				polygon( i ).y = polygon_3d( i ).y;
			}
			point.x = point_3d.x;
			point.y = point_3d.y;
		} else { // Illegal
			assert( false );
			point.x = point.y = 0.0; // Elim possibly used uninitialized warnings
		}

		for ( i = 1; i <= nsides; ++i ) {

			if ( i < nsides ) {
				ip1 = i + 1;
			} else {
				ip1 = 1;
			}

			if ( ( polygon( i ).y < point.y && point.y <= polygon( ip1 ).y ) || ( point.y <= polygon( i ).y && polygon( ip1 ).y < point.y ) ) {
				if ( ( point.x - polygon( i ).x ) - ( point.y - polygon( i ).y ) * ( polygon( ip1 ).x - polygon( i ).x ) / ( polygon( ip1 ).y - polygon( i ).y ) < 0 ) {
					inside = ! inside;
				}
			}

		}

		return inside;
	}

	void
	ComputeIntSolarAbsorpFactors()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Legacy Code
		//       DATE WRITTEN
		//       MODIFIED       B. Griffith, Oct 2010, deal with no floor case
		//                      L. Lawrie, Mar 2012, relax >154 tilt even further (>120 considered non-wall by ASHRAE)
		//       RE-ENGINEERED  Lawrie, Oct 2000

		// PURPOSE OF THIS SUBROUTINE:
		// This routine computes the fractions of diffusely transmitted
		// solar energy absorbed by each zone surface.

		// METHODOLOGY EMPLOYED:
		// It is assumed that all transmitted solar energy is incident
		// on the floors of the zone.  The fraction directly absorbed in
		// the floor is given by 'ISABSF'.  It is proportional to the
		// area * solar absorptance.  The remaining solar energy is then
		// distributed uniformly around the room according to
		// area*absorptance product

		// REFERENCES:
		// BLAST/IBLAST code, original author George Walton

		// Using/Aliasing
		using General::RoundSigDigits;
		using namespace DataWindowEquivalentLayer;

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
		int ConstrNum; // Index for constructions
		int FirstZoneSurf; // Index of first surface in current zone
		int LastZoneSurf; // Index of last surface in current zone
		Real64 AreaSum; // Intermediate calculation value
		int SurfNum; // DO loop counter for zone surfaces
		int ZoneNum; // Loop Counter for Zones
		int Lay; // Window glass layer number
		Real64 AbsDiffTotWin; // Sum of a window's glass layer solar absorptances
		Real64 TestFractSum;
		Real64 HorizAreaSum;

		// FLOW:

		if ( ! allocated( ISABSF ) ) {
			ISABSF.allocate( TotSurfaces );
		}
		ISABSF = 0.0;

		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {

			FirstZoneSurf = Zone( ZoneNum ).SurfaceFirst;
			LastZoneSurf = Zone( ZoneNum ).SurfaceLast;
			AreaSum = 0.0;

			for ( SurfNum = FirstZoneSurf; SurfNum <= LastZoneSurf; ++SurfNum ) {

				if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
				//CR 8229, relaxed from -0.99 to -0.5  (Tilt > 154)
				// CR8769   !use ASHRAE std of >120, -0.9 to -0.5  (Tilt > 120)
				//      IF (Surface(SurfNum)%Class == SurfaceClass_Floor) THEN
				//        write(outputfiledebug,*) 'surf=',TRIM(surface(SurfNum)%name),Surface(SurfNum)%CosTilt
				//      endif
				if ( Zone( ZoneNum ).OfType == StandardZone && Surface( SurfNum ).CosTilt < -0.5 ) AreaSum += Surface( SurfNum ).Area;
				//  Next is not implemented but would be:
				// IF ((Zone(ZoneNum)%OfType .eq. SolarWallZone .or Zone(ZoneNum)%OfType .eq. RoofPondZone) .and.     &
				//      Surface(SurfNum)%ExtBoundCond > 0)    AreaSum = AreaSum + Surface(SurfNum)%Area

			}

			HorizAreaSum = AreaSum;

			if ( ( ! Zone( ZoneNum ).HasFloor ) && ( HorizAreaSum > 0.0 ) ) {
				//fill floor area even though surfs not called "Floor", they are roughly horizontal and face upwards.
				Zone( ZoneNum ).FloorArea = HorizAreaSum;
				ShowWarningError( "ComputeIntSolarAbsorpFactors: Solar distribution model is set to place solar gains on the zone floor," );
				ShowContinueError( "...Zone=\"" + Zone( ZoneNum ).Name + "\" has no floor, but has approximate horizontal surfaces." );
				ShowContinueError( "...these Tilt > 120 degrees, (area=[" + RoundSigDigits( HorizAreaSum, 2 ) + "] m2) will be used." );
			}

			// Compute ISABSF

			for ( SurfNum = FirstZoneSurf; SurfNum <= LastZoneSurf; ++SurfNum ) {

				if ( ! Surface( SurfNum ).HeatTransSurf ) continue;

				// only horizontal surfaces. !      !CR 8229, relaxed from -0.99 to -0.5  (Tilt > 154)
				// only horizontal surfaces. !      !CR8769 use ASHRAE std of >120, -0.9 to -0.5  (Tilt > 120)
				if ( ( Zone( ZoneNum ).OfType != StandardZone || Surface( SurfNum ).CosTilt < -0.5 ) && ( Zone( ZoneNum ).OfType == StandardZone || Surface( SurfNum ).ExtBoundCond > 0 ) ) {

					ConstrNum = Surface( SurfNum ).Construction;
					// last minute V3.1
					if ( Construct( ConstrNum ).TransDiff <= 0.0 ) { //Opaque surface
						if ( AreaSum > 0.0 ) ISABSF( SurfNum ) = Surface( SurfNum ).Area * Construct( ConstrNum ).InsideAbsorpSolar / AreaSum;
					} else { //Window (floor windows are assumed to have no shading device and no divider,
						//and assumed to be non-switchable)
						if ( SurfaceWindow( SurfNum ).StormWinFlag == 1 ) ConstrNum = Surface( SurfNum ).StormWinConstruction;
						AbsDiffTotWin = 0.0;
						if ( ! Construct( Surface( SurfNum ).Construction ).WindowTypeEQL ) {
							for ( Lay = 1; Lay <= Construct( ConstrNum ).TotGlassLayers; ++Lay ) {
								AbsDiffTotWin += Construct( ConstrNum ).AbsDiffBack( Lay );
							}
						} else {
							for ( Lay = 1; Lay <= CFS( Construct( ConstrNum ).EQLConsPtr ).NL; ++Lay ) {
								AbsDiffTotWin += Construct( ConstrNum ).AbsDiffBackEQL( Lay );
							}
						}
						if ( AreaSum > 0.0 ) ISABSF( SurfNum ) = Surface( SurfNum ).Area * AbsDiffTotWin / AreaSum;
					}

				}

			}

			//CR 8229  test ISABSF for problems
			TestFractSum = sum( ISABSF( {FirstZoneSurf,LastZoneSurf} ) );

			if ( TestFractSum <= 0.0 ) {
				if ( Zone( ZoneNum ).ExtWindowArea > 0.0 ) { // we have a problem, the sun has no floor to go to
					if ( Zone( ZoneNum ).FloorArea <= 0.0 ) {
						ShowSevereError( "ComputeIntSolarAbsorpFactors: Solar distribution model is set to place solar gains on the zone floor," );
						ShowContinueError( "but Zone =\"" + Zone( ZoneNum ).Name + "\" does not appear to have any floor surfaces." );
						ShowContinueError( "Solar gains will be spread evenly on all surfaces in the zone, and the simulation continues..." );
					} else { // Floor Area > 0 but still can't absorb
						ShowSevereError( "ComputeIntSolarAbsorpFactors: Solar distribution model is set to place solar gains on the zone floor," );
						ShowContinueError( "but Zone =\"" + Zone( ZoneNum ).Name + "\" floor cannot absorb any solar gains. " );
						ShowContinueError( "Check the solar absorptance of the inside layer of the floor surface construction/material." );
						ShowContinueError( "Solar gains will be spread evenly on all surfaces in the zone, and the simulation continues..." );
					}

					// try again but use an even spread across all the surfaces in the zone, regardless of horizontal
					//  so as to not lose solar energy
					AreaSum = 0.0;
					for ( SurfNum = FirstZoneSurf; SurfNum <= LastZoneSurf; ++SurfNum ) {
						if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
						AreaSum += Surface( SurfNum ).Area;
					}

					for ( SurfNum = FirstZoneSurf; SurfNum <= LastZoneSurf; ++SurfNum ) {

						if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
						ConstrNum = Surface( SurfNum ).Construction;
						if ( Construct( ConstrNum ).TransDiff <= 0.0 ) { //Opaque surface
							if ( AreaSum > 0.0 ) ISABSF( SurfNum ) = Surface( SurfNum ).Area * Construct( ConstrNum ).InsideAbsorpSolar / AreaSum;
						} else { //Window (floor windows are assumed to have no shading device and no divider,
							//and assumed to be non-switchable)
							if ( SurfaceWindow( SurfNum ).StormWinFlag == 1 ) ConstrNum = Surface( SurfNum ).StormWinConstruction;
							AbsDiffTotWin = 0.0;
							if ( ! Construct( Surface( SurfNum ).Construction ).WindowTypeEQL ) {
								for ( Lay = 1; Lay <= Construct( ConstrNum ).TotGlassLayers; ++Lay ) {
									AbsDiffTotWin += Construct( ConstrNum ).AbsDiffBack( Lay );
								}
							} else {
								for ( Lay = 1; Lay <= CFS( Construct( ConstrNum ).EQLConsPtr ).NL; ++Lay ) {
									AbsDiffTotWin += Construct( ConstrNum ).AbsDiffBackEQL( Lay );
								}
							}

							if ( AreaSum > 0.0 ) ISABSF( SurfNum ) = Surface( SurfNum ).Area * AbsDiffTotWin / AreaSum;
						}
					}
				}

			}

		} // zone loop

	}

	void
	CLIP(
		int const NVT,
		Array1< Real64 > & XVT,
		Array1< Real64 > & YVT,
		Array1< Real64 > & ZVT
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Legacy Code
		//       DATE WRITTEN
		//       MODIFIED       na
		//       RE-ENGINEERED  Lawrie, Oct 2000

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine 'clips' the shadow casting surface polygon so that
		// none of it lies below the plane of the receiving surface polygon.  This
		// prevents the casting of 'false' shadows.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// BLAST/IBLAST code, original author George Walton

		// USE STATEMENTS:
		// na

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NABOVE( 0 ); // Number of vertices of shadow casting surface. above the plane of receiving surface
		int NEXT( 0 ); // First vertex above plane of receiving surface
		int NON( 0 ); // Number of vertices of shadow casting surface. on plane of receiving surface
		Real64 XIN( 0.0 ); // X of entry point of shadow casting surface. into plane of receiving surface
		Real64 XOUT( 0.0 ); // X of exit point of shadow casting surface. from plane of receiving surface
		Real64 YIN( 0.0 ); // Y of entry point of shadow casting surface. into plane of receiving surface
		Real64 YOUT( 0.0 ); // Y of exit point of shadow casting surface. from plane of receiving surface
		//  INTEGER NVS      ! Number of vertices of the shadow/clipped surface

		// Determine if the shadow casting surface. is above, below, or intersects with the plane of the receiving surface

		NumVertInShadowOrClippedSurface = NVS;
		for ( int N = 1; N <= NVT; ++N ) {
			Real64 const ZVT_N( ZVT( N ) );
			if ( ZVT_N > 0.0 ) {
				++NABOVE;
			} else if ( ZVT_N == 0.0 ) {
				++NON;
			}
		}

		if ( NABOVE + NON == NVT ) { // Rename the unclipped shadow casting surface.

			NVS = NVT;
			NumVertInShadowOrClippedSurface = NVT;
			for ( int N = 1; N <= NVT; ++N ) {
				XVC( N ) = XVT( N );
				YVC( N ) = YVT( N );
				ZVC( N ) = ZVT( N );
			}

		} else if ( NABOVE == 0 ) { // Totally submerged shadow casting surface.

			NVS = 0;
			NumVertInShadowOrClippedSurface = 0;

		} else { // Remove (clip) that portion of the shadow casting surface. which is below the receiving surface

			NVS = NABOVE + 2;
			NumVertInShadowOrClippedSurface = NABOVE + 2;
			Real64 ZVT_N, ZVT_P( ZVT( 1 ) );
			XVT( NVT + 1 ) = XVT( 1 );
			YVT( NVT + 1 ) = YVT( 1 );
			ZVT( NVT + 1 ) = ZVT_P;
			for ( int N = 1, P = 2; N <= NVT; ++N, ++P ) {
				ZVT_N = ZVT_P;
				ZVT_P = ZVT( P );
				if ( ZVT_N >= 0.0 && ZVT_P < 0.0 ) { // Line enters plane of receiving surface
					Real64 const ZVT_fac( 1.0 / ( ZVT_P - ZVT_N ) );
					XIN = ( ZVT_P * XVT( N ) - ZVT_N * XVT( P ) ) * ZVT_fac;
					YIN = ( ZVT_P * YVT( N ) - ZVT_N * YVT( P ) ) * ZVT_fac;
				} else if ( ZVT_N <= 0.0 && ZVT_P > 0.0 ) { // Line exits plane of receiving surface
					NEXT = N + 1;
					Real64 const ZVT_fac( 1.0 / ( ZVT_P - ZVT_N ) );
					XOUT = ( ZVT_P * XVT( N ) - ZVT_N * XVT( P ) ) * ZVT_fac;
					YOUT = ( ZVT_P * YVT( N ) - ZVT_N * YVT( P ) ) * ZVT_fac;
				}
			}

			// Renumber the vertices of the clipped shadow casting surface. so they are still counter-clockwise sequential.

			XVC( 1 ) = XOUT; //? Verify that the IN and OUT values were ever set?
			YVC( 1 ) = YOUT;
			ZVC( 1 ) = 0.0;
			XVC( NVS ) = XIN;
			YVC( NVS ) = YIN;
			ZVC( NVS ) = 0.0;
			for ( int N = 1; N <= NABOVE; ++N ) {
				if ( NEXT > NVT ) NEXT = 1;
				XVC( N + 1 ) = XVT( NEXT );
				YVC( N + 1 ) = YVT( NEXT );
				ZVC( N + 1 ) = ZVT( NEXT );
				++NEXT;
			}

		}

	}

	void
	CTRANS(
		int const NS, // Surface number whose vertex coordinates are being transformed
		int const NGRS, // Base surface number for surface NS
		int & NVT, // Number of vertices for surface NS
		Array1< Real64 > & XVT, // XYZ coordinates of vertices of NS in plane of NGRS
		Array1< Real64 > & YVT,
		Array1< Real64 > & ZVT
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Legacy Code
		//       DATE WRITTEN
		//       MODIFIED       na
		//       RE-ENGINEERED  Lawrie, Oct 2000

		// PURPOSE OF THIS SUBROUTINE:
		// Transforms the general coordinates of the vertices
		// of surface NS to coordinates in the plane of the receiving surface NGRS.
		// See subroutine 'CalcCoordinateTransformation' SurfaceGeometry Module.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// BLAST/IBLAST code, original author George Walton
		// NECAP subroutine 'SHADOW'

		// USE STATEMENTS:
		// na

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 Xdif; // Intermediate Result
		Real64 Ydif; // Intermediate Result
		Real64 Zdif; // Intermediate Result

		//Tuned
		auto const & surface( Surface( NS ) );
		auto const & base_surface( Surface( NGRS ) );
		auto const & base_lcsx( base_surface.lcsx );
		auto const & base_lcsy( base_surface.lcsy );
		auto const & base_lcsz( base_surface.lcsz );
		Real64 const base_X0( X0( NGRS ) );
		Real64 const base_Y0( Y0( NGRS ) );
		Real64 const base_Z0( Z0( NGRS ) );

		NVT = surface.Sides;

		// Perform transformation
		for ( int N = 1; N <= NVT; ++N ) {
			auto const & vertex( surface.Vertex( N ) );

			Xdif = vertex.x - base_X0;
			Ydif = vertex.y - base_Y0;
			Zdif = vertex.z - base_Z0;

			if ( std::abs( Xdif ) <= 1.E-15 ) Xdif = 0.0;
			if ( std::abs( Ydif ) <= 1.E-15 ) Ydif = 0.0;
			if ( std::abs( Zdif ) <= 1.E-15 ) Zdif = 0.0;

			XVT( N ) = base_lcsx.x * Xdif + base_lcsx.y * Ydif + base_lcsx.z * Zdif;
			YVT( N ) = base_lcsy.x * Xdif + base_lcsy.y * Ydif + base_lcsy.z * Zdif;
			ZVT( N ) = base_lcsz.x * Xdif + base_lcsz.y * Ydif + base_lcsz.z * Zdif;
		}

	}

	void
	HTRANS(
		int const I, // Mode selector: 0 - Compute H.C. of sides
		int const NS, // Figure Number
		int const NumVertices // Number of vertices
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Legacy Code
		//       DATE WRITTEN
		//       MODIFIED       na
		//       RE-ENGINEERED  Lawrie, Oct 2000

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine sets up the homogeneous coordinates.
		// This routine converts the cartesian coordinates of a surface
		// or shadow polygon to homogeneous coordinates.  It also
		// computes the area of the polygon.

		// METHODOLOGY EMPLOYED:
		// Note: Original legacy code used integer arithmetic (tests in subroutines
		// INCLOS and INTCPT are sensitive to round-off error).  However, porting to Fortran 77
		// (BLAST/IBLAST) required some variables to become REAL(r64) instead.

		// Notes on homogeneous coordinates:
		// A point (X,Y) is represented by a 3-element vector
		// (W*X,W*Y,W), where W may be any REAL(r64) number except 0.  a line
		// is also represented by a 3-element vector (A,B,C).  The
		// directed line (A,B,C) from point (W*X1,W*Y1,W) to point
		// (V*X2,V*Y2,V) is given by (A,B,C) = (W*X1,W*Y1,W) cross
		// (V*X2,V*Y2,V).  The sequence of the cross product is a
		// convention to determine sign.  The condition that a point lie
		// on a line is that (A,B,C) dot (W*X,W*Y,W) = 0.  'Normalize'
		// the representation of a point by setting W to 1.  Then if
		// (A,B,C) dot (X,Y,1) > 0.0, The point is to the left of the
		// line, and if it is less than zero, the point is to the right
		// of the line.  The intercept of two lines is given by
		// (W*X,W*Y,W) = (A1,B1,C1) cross (A2,B2,C3).

		// REFERENCES:
		// BLAST/IBLAST code, original author George Walton
		// W. M. Newman & R. F. Sproull, 'Principles of Interactive Computer Graphics', Appendix II, McGraw-Hill, 1973.
		// 'CRC Math Tables', 22 ED, 'Analytic Geometry', P.369

		// Using/Aliasing
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//                1 - Compute H.C. of vertices & sides

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		if ( NS > 2 * MaxHCS ) {
			ShowFatalError( "Solar Shading: HTrans: Too many Figures (>" + TrimSigDigits( MaxHCS ) + ')' );
		}

		HCNV( NS ) = NumVertices;

		//Tuned Linear indexing

		assert( equal_dimensions( HCX, HCY ) );
		assert( equal_dimensions( HCX, HCA ) );
		assert( equal_dimensions( HCX, HCB ) );
		assert( equal_dimensions( HCX, HCC ) );
		auto const l1( HCX.index( NS, 1 ) );
		if ( I != 0 ) { // Transform vertices of figure ns.

			// See comment at top of module regarding HCMULT
			auto l( l1 );
			for ( int N = 1; N <= NumVertices; ++N, ++l ) { // [ l ] == ( NS, N )
				HCX[ l ] = nint64( XVS( N ) * HCMULT );
				HCY[ l ] = nint64( YVS( N ) * HCMULT );
			}

		}

		// Establish extra point for finding lines between points.

		auto l( HCX.index( NS, NumVertices + 1 ) );
		Int64 HCX_m( HCX[ l ] = HCX[ l1 ] ); // [ l ] == ( NS, NumVertices + 1 ), [ l1 ] == ( NS, 1 )
		Int64 HCY_m( HCY[ l ] = HCY[ l1 ] ); // [ l ] == ( NS, NumVertices + 1 ), [ l1 ] == ( NS, 1 )

		// Determine lines between points.
		l = l1;
		auto m( l1 + 1u );
		Int64 HCX_l;
		Int64 HCY_l;
		Real64 SUM( 0.0 ); // Sum variable
		for ( int N = 1; N <= NumVertices; ++N, ++l, ++m ) { // [ l ] == ( NS, N ), [ m ] == ( NS, N + 1 )
			HCX_l = HCX_m;
			HCY_l = HCY_m;
			HCX_m = HCX[ m ];
			HCY_m = HCY[ m ];
			HCA[ l ] = HCY_l - HCY_m;
			HCB[ l ] = HCX_m - HCX_l;
			SUM += HCC[ l ] = ( HCY_m * HCX_l ) - ( HCX_m * HCY_l );
		}

		// Compute area of polygon.
		//  SUM=0.0D0
		//  DO N = 1, NumVertices
		//    SUM = SUM + HCX(N,NS)*HCY(N+1,NS) - HCY(N,NS)*HCX(N+1,NS) ! Since HCX and HCY integerized, value of SUM should be ok
		//  END DO
		HCAREA( NS ) = SUM * sqHCMULT_fac;
		//  HCAREA(NS)=0.5d0*SUM*(kHCMULT)

	}

	void
	HTRANS0(
		int const NS, // Figure Number
		int const NumVertices // Number of vertices
	)
	{
		// Using/Aliasing
		using General::TrimSigDigits;

		// Locals

		if ( NS > 2 * MaxHCS ) {
			ShowFatalError( "Solar Shading: HTrans0: Too many Figures (>" + TrimSigDigits( MaxHCS ) + ')' );
		}

		HCNV( NS ) = NumVertices;

		//Tuned Linear indexing

		assert( equal_dimensions( HCX, HCY ) );
		assert( equal_dimensions( HCX, HCA ) );
		assert( equal_dimensions( HCX, HCB ) );
		assert( equal_dimensions( HCX, HCC ) );

		auto const l1( HCX.index( NS, 1 ) );

		auto l( HCX.index( NS, NumVertices + 1 ) );
		Int64 HCX_m( HCX[ l ] = HCX[ l1 ] ); // [ l1 ] == ( NS, 1 )
		Int64 HCY_m( HCY[ l ] = HCY[ l1 ] ); // [ l1 ] == ( NS, 1 )

		l = l1;
		auto m( l1 + 1u );
		Int64 HCX_l;
		Int64 HCY_l;
		Real64 SUM( 0.0 );
		for ( int N = 1; N <= NumVertices; ++N, ++l, ++m ) { // [ l ] == ( NS, N ), [ m ] == ( NS, N + 1 )
			HCX_l = HCX_m;
			HCY_l = HCY_m;
			HCX_m = HCX[ m ];
			HCY_m = HCY[ m ];
			HCA[ l ] = HCY_l - HCY_m;
			HCB[ l ] = HCX_m - HCX_l;
			SUM += HCC[ l ] = ( HCY_m * HCX_l ) - ( HCX_m * HCY_l );
		}

		HCAREA( NS ) = SUM * sqHCMULT_fac;

	}

	void
	HTRANS1(
		int const NS, // Figure Number
		int const NumVertices // Number of vertices
	)
	{
		// Using/Aliasing
		using General::TrimSigDigits;

		if ( NS > 2 * MaxHCS ) {
			ShowFatalError( "Solar Shading: HTrans1: Too many Figures (>" + TrimSigDigits( MaxHCS ) + ')' );
		}

		HCNV( NS ) = NumVertices;

		//Tuned Linear indexing

		assert( equal_dimensions( HCX, HCY ) );
		assert( equal_dimensions( HCX, HCA ) );
		assert( equal_dimensions( HCX, HCB ) );
		assert( equal_dimensions( HCX, HCC ) );

		auto const l1( HCX.index( NS, 1 ) );

		// only in HTRANS1
		auto l( l1 );
		for ( int N = 1; N <= NumVertices; ++N, ++l ) { // [ l ] == ( NS, N )
			HCX[ l ] = nint64( XVS( N ) * HCMULT );
			HCY[ l ] = nint64( YVS( N ) * HCMULT );
		}

		l = HCX.index( NS, NumVertices + 1 );
		Int64 HCX_m( HCX[ l ] = HCX[ l1 ] ); // [ l1 ] == ( NS, 1 )
		Int64 HCY_m( HCY[ l ] = HCY[ l1 ] );

		l = l1;
		auto m( l1 + 1u );
		Int64 HCX_l;
		Int64 HCY_l;
		Real64 SUM( 0.0 );
		for ( int N = 1; N <= NumVertices; ++N, ++l, ++m ) { // [ l ] == ( NS, N ), [ m ] == ( NS, N + 1 )
			HCX_l = HCX_m;
			HCY_l = HCY_m;
			HCX_m = HCX[ m ];
			HCY_m = HCY[ m ];
			HCA[ l ] = HCY_l - HCY_m;
			HCB[ l ] = HCX_m - HCX_l;
			SUM += HCC[ l ] = ( HCY_m * HCX_l ) - ( HCX_m * HCY_l );
		}

		HCAREA( NS ) = SUM * sqHCMULT_fac;

	}

	void
	INCLOS(
		int const N1, // Figure number of figure 1
		int const N1NumVert, // Number of vertices of figure 1
		int const N2, // Figure number of figure 2
		int const N2NumVert, // Number of vertices of figure 2
		int & NumVerticesOverlap, // Number of vertices which overlap
		int & NIN // Number of vertices of figure 1 within figure 2
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Legacy Code
		//       DATE WRITTEN
		//       MODIFIED       na
		//       RE-ENGINEERED  Lawrie, Oct 2000

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine determines which vertices of figure N1 lie within figure N2.

		// METHODOLOGY EMPLOYED:
		// For vertex N of figure N1 to lie within figure N2, it must be
		// on or to the right of all sides of figure N2, assuming
		// figure N2 is convex.

		// REFERENCES:
		// BLAST/IBLAST code, original author George Walton

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
		int K; // Vertex number of the overlap
		int M; // Side number of figure N2
		int N; // Vertex number of figure N1
		bool CycleMainLoop; // Sets when to cycle main loop
		Real64 HFunct;

		NIN = 0;

		for ( N = 1; N <= N1NumVert; ++N ) {

			CycleMainLoop = false;

			// Eliminate cases where vertex N is to the left of side M.

			for ( M = 1; M <= N2NumVert; ++M ) {
				HFunct = HCX( N1, N ) * HCA( N2, M ) + HCY( N1, N ) * HCB( N2, M ) + HCC( N2, M );
				if ( HFunct > 0.0 ) {
					CycleMainLoop = true; // Set to cycle to the next value of N
					break; // M DO loop
				}
			}

			if ( CycleMainLoop ) continue;
			++NIN;

			// Check for duplication of previously determined points.

			if ( NumVerticesOverlap != 0 ) {
				for ( K = 1; K <= NumVerticesOverlap; ++K ) {
					if ( ( XTEMP( K ) == HCX( N1, N ) ) && ( YTEMP( K ) == HCY( N1, N ) ) ) {
						CycleMainLoop = true; // Set to cycle to the next value of N
						break; // K DO loop
					}
				}
				if ( CycleMainLoop ) continue;
			}

			// Record enclosed vertices in temporary arrays.

			++NumVerticesOverlap;
			XTEMP( NumVerticesOverlap ) = HCX( N1, N );
			YTEMP( NumVerticesOverlap ) = HCY( N1, N );

		}

	}

	void
	INTCPT(
		int const NV1, // Number of vertices of figure NS1
		int const NV2, // Number of vertices of figure NS2
		int & NV3, // Number of vertices of figure NS3
		int const NS1, // Number of the figure being overlapped
		int const NS2 // Number of the figure doing overlapping
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Legacy Code
		//       DATE WRITTEN
		//       MODIFIED       na
		//       RE-ENGINEERED  Lawrie, Oct 2000

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine determines all intercepts between the sides of figure NS1
		// and the sides of figure NS2.

		// METHODOLOGY EMPLOYED:
		// The requirements for intersection are that the end points of
		// line N lie on both sides of line M and vice versa.  Also
		// eliminate cases where the end point of one line lies exactly
		// on the other to reduce duplication with the enclosed points.

		// REFERENCES:
		// BLAST/IBLAST code, original author George Walton

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
		Real64 W; // Normalization factor
		Real64 XUntrunc; // Untruncated X coordinate
		Real64 YUntrunc; // Untruncated Y coordinate
		Int64 I1; // Intermediate result for testing intersection
		Int64 I2; // Intermediate result for testing intersection
		int K;
		int KK;
		int M; // Side number of figure NS2
		int N; // Side number of figure NS1

		for ( N = 1; N <= NV1; ++N ) {
			for ( M = 1; M <= NV2; ++M ) {

				// Eliminate cases where sides N and M do not intersect.

				I1 = HCA( NS1, N ) * HCX( NS2, M ) + HCB( NS1, N ) * HCY( NS2, M ) + HCC( NS1, N );
				I2 = HCA( NS1, N ) * HCX( NS2, M + 1 ) + HCB( NS1, N ) * HCY( NS2, M + 1 ) + HCC( NS1, N );
				if ( I1 >= 0 && I2 >= 0 ) continue;
				if ( I1 <= 0 && I2 <= 0 ) continue;

				I1 = HCA( NS2, M ) * HCX( NS1, N ) + HCB( NS2, M ) * HCY( NS1, N ) + HCC( NS2, M );
				I2 = HCA( NS2, M ) * HCX( NS1, N + 1 ) + HCB( NS2, M ) * HCY( NS1, N + 1 ) + HCC( NS2, M );
				if ( I1 >= 0 && I2 >= 0 ) continue;
				if ( I1 <= 0 && I2 <= 0 ) continue;

				// Determine the point of intersection and record in the temporary array.

				KK = NV3;
				++NV3;
				W = HCB( NS2, M ) * HCA( NS1, N ) - HCA( NS2, M ) * HCB( NS1, N );
				XUntrunc = ( HCC( NS2, M ) * HCB( NS1, N ) - HCB( NS2, M ) * HCC( NS1, N ) ) / W;
				YUntrunc = ( HCA( NS2, M ) * HCC( NS1, N ) - HCC( NS2, M ) * HCA( NS1, N ) ) / W;
				if ( NV3 > isize( XTEMP ) ) {
					//        write(outputfiledebug,*) 'nv3=',nv3,' SIZE(xtemp)=',SIZE(xtemp)
					XTEMP.redimension( isize( XTEMP ) + 10, 0.0 );
					YTEMP.redimension( isize( YTEMP ) + 10, 0.0 );
				}
				XTEMP( NV3 ) = nint64( XUntrunc );
				YTEMP( NV3 ) = nint64( YUntrunc );

				// Eliminate near-duplicate points.

				if ( KK != 0 ) {
					auto const x( XTEMP( NV3 ) );
					auto const y( YTEMP( NV3 ) );
					for ( K = 1; K <= KK; ++K ) {
						if ( std::abs( x - XTEMP( K ) ) > 2.0 ) continue;
						if ( std::abs( y - YTEMP( K ) ) > 2.0 ) continue;
						NV3 = KK;
						break; // K DO loop
					}
				}

			}

		}

	}

	void
	CLIPPOLY(
		int const NS1, // Figure number of figure 1 (The subject polygon)
		int const NS2, // Figure number of figure 2 (The clipping polygon)
		int const NV1, // Number of vertices of figure 1
		int const NV2, // Number of vertices of figure 2
		int & NV3 // Number of vertices of figure 3
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Tyler Hoyt
		//       DATE WRITTEN   May 4, 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Populate global arrays XTEMP and YTEMP with the vertices
		// of the overlap between NS1 and NS2, and determine relevant
		// overlap status.

		// METHODOLOGY EMPLOYED:
		// The Sutherland-Hodgman algorithm for polygon clipping is employed.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using General::ReallocateRealArray;
		using General::SafeDivide;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		typedef  Array2D< Int64 >::size_type  size_type;
		bool INTFLAG; // For overlap status
		int S; // Test vertex
		int KK; // Duplicate test index
		int NVOUT; // Current output length for loops
		int NVTEMP;

		Real64 W; // Normalization factor
		Real64 HFunct;

#ifdef EP_Count_Calls
		++NumClipPoly_Calls;
#endif
		//Tuned Linear indexing

		assert( equal_dimensions( HCX, HCY ) );
		assert( equal_dimensions( HCX, HCA ) );
		assert( equal_dimensions( HCX, HCB ) );
		assert( equal_dimensions( HCX, HCC ) );

		// Populate the arrays with the original polygon
		for ( size_type j = 0, l = HCX.index( NS1, 1 ), e = NV1; j < e; ++j, ++l ) {
			XTEMP[ j ] = HCX[ l ]; // [ l ] == ( NS1, j+1 )
			YTEMP[ j ] = HCY[ l ];
			ATEMP[ j ] = HCA[ l ];
			BTEMP[ j ] = HCB[ l ];
			CTEMP[ j ] = HCC[ l ];
		}

		NVOUT = NV1; // First point-loop is the length of the subject polygon.
		INTFLAG = false;
		NVTEMP = 0;
		KK = 0;

		auto l( HCA.index( NS2, 1 ) );
		for ( int E = 1; E <= NV2; ++E, ++l ) { // Loop over edges of the clipping polygon
			for ( int P = 1; P <= NVOUT; ++P ) {
				XTEMP1( P ) = XTEMP( P );
				YTEMP1( P ) = YTEMP( P );
			}
			S = NVOUT;
			Real64 const HCA_E( HCA[ l ] );
			Real64 const HCB_E( HCB[ l ] );
			Real64 const HCC_E( HCC[ l ] );
			Real64 XTEMP1_S( XTEMP1( S ) );
			Real64 YTEMP1_S( YTEMP1( S ) );
			for ( int P = 1; P <= NVOUT; ++P ) {
				Real64 const XTEMP1_P( XTEMP1( P ) );
				Real64 const YTEMP1_P( YTEMP1( P ) );
				HFunct = XTEMP1_P * HCA_E + YTEMP1_P * HCB_E + HCC_E;
				// S is constant within this block
				if ( HFunct <= 0.0 ) { // Vertex is not in the clipping plane
					HFunct = XTEMP1_S * HCA_E + YTEMP1_S * HCB_E + HCC_E;
					if ( HFunct > 0.0 ) { // Test vertex is in the clipping plane

						// Find/store the intersection of the clip edge and the line connecting S and P
						KK = NVTEMP;
						++NVTEMP;
						Real64 const ATEMP_S( ATEMP( S ) );
						Real64 const BTEMP_S( BTEMP( S ) );
						Real64 const CTEMP_S( CTEMP( S ) );
						W = HCB_E * ATEMP_S - HCA_E * BTEMP_S;
						if ( W != 0.0 ) {
							Real64 const W_inv( 1.0 / W );
							XTEMP( NVTEMP ) = nint64( ( HCC_E * BTEMP_S - HCB_E * CTEMP_S ) * W_inv );
							YTEMP( NVTEMP ) = nint64( ( HCA_E * CTEMP_S - HCC_E * ATEMP_S ) * W_inv );
						} else {
							XTEMP( NVTEMP ) = SafeDivide( HCC_E * BTEMP_S - HCB_E * CTEMP_S, W );
							YTEMP( NVTEMP ) = SafeDivide( HCA_E * CTEMP_S - HCC_E * ATEMP_S, W );
						}
						INTFLAG = true;

						if ( E == NV2 ) { // Remove near-duplicates on last edge
							if ( KK != 0 ) {
								auto const x( XTEMP( NVTEMP ) );
								auto const y( YTEMP( NVTEMP ) );
								for ( int K = 1; K <= KK; ++K ) {
									if ( std::abs( x - XTEMP( K ) ) > 2.0 ) continue;
									if ( std::abs( y - YTEMP( K ) ) > 2.0 ) continue;
									NVTEMP = KK;
									break; // K loop
								}
							}
						}

					}

					KK = NVTEMP;
					++NVTEMP;
					if ( NVTEMP > MAXHCArrayBounds ) {
						int const NewArrayBounds( MAXHCArrayBounds + MAXHCArrayIncrement );
						XTEMP.redimension( NewArrayBounds, 0.0 );
						YTEMP.redimension( NewArrayBounds, 0.0 );
						XTEMP1.redimension( NewArrayBounds, 0.0 );
						YTEMP1.redimension( NewArrayBounds, 0.0 );
						ATEMP.redimension( NewArrayBounds, 0.0 );
						BTEMP.redimension( NewArrayBounds, 0.0 );
						CTEMP.redimension( NewArrayBounds, 0.0 );
						MAXHCArrayBounds = NewArrayBounds;
					}

					XTEMP( NVTEMP ) = XTEMP1_P;
					YTEMP( NVTEMP ) = YTEMP1_P;

					if ( E == NV2 ) { // Remove near-duplicates on last edge
						if ( KK != 0 ) {
							auto const x( XTEMP( NVTEMP ) );
							auto const y( YTEMP( NVTEMP ) );
							for ( int K = 1; K <= KK; ++K ) {
								if ( std::abs( x - XTEMP( K ) ) > 2.0 ) continue;
								if ( std::abs( y - YTEMP( K ) ) > 2.0 ) continue;
								NVTEMP = KK;
								break; // K loop
							}
						}
					}

				} else {
					HFunct = XTEMP1_S * HCA_E + YTEMP1_S * HCB_E + HCC_E;
					if ( HFunct <= 0.0 ) { // Test vertex is not in the clipping plane
						if ( NVTEMP < 2 * ( MaxVerticesPerSurface + 1 )){  // avoid assigning to element outside of XTEMP array size
							KK = NVTEMP;
							++NVTEMP;
							Real64 const ATEMP_S( ATEMP( S ) );
							Real64 const BTEMP_S( BTEMP( S ) );
							Real64 const CTEMP_S( CTEMP( S ) );
							W = HCB_E * ATEMP_S - HCA_E * BTEMP_S;
							if ( W != 0.0 ) {
								Real64 const W_inv( 1.0 / W );
								XTEMP( NVTEMP ) = nint64( ( HCC_E * BTEMP_S - HCB_E * CTEMP_S ) * W_inv );
								YTEMP( NVTEMP ) = nint64( ( HCA_E * CTEMP_S - HCC_E * ATEMP_S ) * W_inv );
							}
							else {
								XTEMP( NVTEMP ) = SafeDivide( HCC_E * BTEMP_S - HCB_E * CTEMP_S, W );
								YTEMP( NVTEMP ) = SafeDivide( HCA_E * CTEMP_S - HCC_E * ATEMP_S, W );
							}
							INTFLAG = true;

							if ( E == NV2 ) { // Remove near-duplicates on last edge
								if ( KK != 0 ) {
									auto const x( XTEMP( NVTEMP ) );
									auto const y( YTEMP( NVTEMP ) );
									for ( int K = 1; K <= KK; ++K ) {
										if ( std::abs( x - XTEMP( K ) ) > 2.0 ) continue;
										if ( std::abs( y - YTEMP( K ) ) > 2.0 ) continue;
										NVTEMP = KK;
										break; // K loop
									}
								}
							}
						}
					}
				}
				S = P;
				XTEMP1_S = XTEMP1_P;
				YTEMP1_S = YTEMP1_P;
			} // end loop over points of subject polygon

			NVOUT = NVTEMP;
			if ( NVOUT == 0 ) break; // Added to avoid array bounds violation of XTEMP1 and YTEMP1 and wasted looping
			NVTEMP = 0;

			if ( E != NV2 ) {
				if ( NVOUT > 2 ) { // Compute HC values for edges of output polygon
					Real64 const X_1( XTEMP( 1 ) );
					Real64 const Y_1( YTEMP( 1 ) );
					Real64 X_P( X_1 ), X_P1;
					Real64 Y_P( Y_1 ), Y_P1;
					for ( int P = 1; P < NVOUT; ++P ) {
						X_P1 = XTEMP( P + 1 );
						Y_P1 = YTEMP( P + 1 );
						ATEMP( P ) = Y_P - Y_P1;
						BTEMP( P ) = X_P1 - X_P;
						CTEMP( P ) = X_P * Y_P1 - Y_P * X_P1;
						X_P = X_P1;
						Y_P = Y_P1;
					}
					ATEMP( NVOUT ) = Y_P1 - Y_1;
					BTEMP( NVOUT ) = X_1 - X_P1;
					CTEMP( NVOUT ) = X_P1 * Y_1 - Y_P1 * X_1;
				}
			}

		} // end loop over edges in NS2

		NV3 = NVOUT;

		if ( NV3 < 3 ) { // Determine overlap status
			OverlapStatus = NoOverlap;
		} else if ( ! INTFLAG ) {
			OverlapStatus = FirstSurfWithinSecond;
		}

	}

	void
	MULTOL(
		int const NNN, // argument
		int const LOC0, // Location in the homogeneous coordinate array
		int const NRFIGS // Number of figures overlapped
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Legacy Code
		//       DATE WRITTEN
		//       MODIFIED       na
		//       RE-ENGINEERED  Lawrie, Oct 2000

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine determines the overlaps of figure 'NS2' with previous figures
		// 'LOC0+1' through 'LOC0+NRFIGS'.  For example, if NS2
		// is a shadow, overlap with previous shadows.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// BLAST/IBLAST code, original author George Walton

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// first figure overlapped (minus 1)

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int I; // Loop Control
		int NS1; // Number of the figure being overlapped
		int NS2; // Number of the figure doing overlapping
		int NS3; // Location to place results of overlap

		maxNumberOfFigures = max( maxNumberOfFigures, NRFIGS );

		NS2 = NNN;
		for ( I = 1; I <= NRFIGS; ++I ) {
			NS1 = LOC0 + I;
			NS3 = LOCHCA + 1;

			DeterminePolygonOverlap( NS1, NS2, NS3 ); // Find overlap of figure NS2 on figure NS1.

			// Process overlap cases:

			if ( OverlapStatus == NoOverlap ) continue;

			if ( ( OverlapStatus == TooManyVertices ) || ( OverlapStatus == TooManyFigures ) ) break;

			LOCHCA = NS3; // Increment h.c. arrays pointer.

		}

	}

	void
	ORDER(
		int const NV3, // Number of vertices of figure NS3
		int const NS3 // Location to place results of overlap
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Legacy Code
		//       DATE WRITTEN
		//       MODIFIED       na
		//       RE-ENGINEERED  Lawrie, Oct 2000

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine sorts the vertices found by inclosure and
		// intercept in to clockwise order so that the overlap polygon
		// may be used in computing subsequent overlaps.

		// METHODOLOGY EMPLOYED:
		// The slopes of the lines from the left-most vertex to all
		// others are found.  The slopes are sorted into descending
		// sequence.  This sequence puts the vertices in clockwise order.

		// REFERENCES:
		// BLAST/IBLAST code, original author George Walton

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
		static Array1D< Real64 > SLOPE; // Slopes from left-most vertex to others
		Real64 DELTAX; // Difference between X coordinates of two vertices
		Real64 DELTAY; // Difference between Y coordinates of two vertices
		Real64 SAVES; // Temporary location for exchange of variables
		Real64 SAVEX; // Temporary location for exchange of variables
		Real64 SAVEY; // Temporary location for exchange of variables
		Real64 XMIN; // X coordinate of left-most vertex
		Real64 YXMIN;
		int I; // Sort index
		int IM1; // Sort control
		int J; // Sort index
		int M; // Number of slopes to be sorted
		int N; // Vertex number
		int P; // Location of first slope to be sorted
		static bool FirstTimeFlag( true );

		if ( FirstTimeFlag ) {
			SLOPE.allocate( max( 10, MaxVerticesPerSurface + 1 ) );
			FirstTimeFlag = false;
		}
		// Determine left-most vertex.

		XMIN = XTEMP( 1 );
		YXMIN = YTEMP( 1 );
		for ( N = 2; N <= NV3; ++N ) {
			if ( XTEMP( N ) >= XMIN ) continue;
			XMIN = XTEMP( N );
			YXMIN = YTEMP( N );
		}

		// Determine slopes from left-most vertex to all others.  Identify
		// first and second or last points as they occur.

		P = 1;
		M = 0;
		for ( N = 1; N <= NV3; ++N ) {

			DELTAX = XTEMP( N ) - XMIN;
			DELTAY = YTEMP( N ) - YXMIN;

			if ( std::abs( DELTAX ) > 0.5 ) {

				++M;
				SLOPE( M ) = DELTAY / DELTAX;
				XTEMP( M ) = XTEMP( N );
				YTEMP( M ) = YTEMP( N );

			} else if ( DELTAY > 0.5 ) {

				P = 2;
				HCX( NS3, 2 ) = nint64( XTEMP( N ) );
				HCY( NS3, 2 ) = nint64( YTEMP( N ) );

			} else if ( DELTAY < -0.5 ) {

				HCX( NS3, NV3 ) = nint64( XTEMP( N ) );
				HCY( NS3, NV3 ) = nint64( YTEMP( N ) );

			} else {

				HCX( NS3, 1 ) = nint64( XMIN );
				HCY( NS3, 1 ) = nint64( YXMIN );

			}

		}

		// Sequence the temporary arrays in order of decreasing slopes.(bubble sort)

		if ( M != 1 ) {

			for ( I = 2; I <= M; ++I ) {
				IM1 = I - 1;
				for ( J = 1; J <= IM1; ++J ) {
					if ( SLOPE( I ) <= SLOPE( J ) ) continue;
					SAVEX = XTEMP( I );
					SAVEY = YTEMP( I );
					SAVES = SLOPE( I );
					XTEMP( I ) = XTEMP( J );
					YTEMP( I ) = YTEMP( J );
					SLOPE( I ) = SLOPE( J );
					XTEMP( J ) = SAVEX;
					YTEMP( J ) = SAVEY;
					SLOPE( J ) = SAVES;
				}
			}

		}

		// Place sequenced points in the homogeneous coordinate arrays.

		for ( N = 1; N <= M; ++N ) {
			HCX( NS3, N + P ) = nint64( XTEMP( N ) );
			HCY( NS3, N + P ) = nint64( YTEMP( N ) );
		}

	}

	void
	DeterminePolygonOverlap(
		int const NS1, // Number of the figure being overlapped
		int const NS2, // Number of the figure doing overlapping
		int const NS3 // Location to place results of overlap
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Legacy Code
		//       DATE WRITTEN
		//       MODIFIED       na
		//       RE-ENGINEERED  Lawrie, Oct 2000

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine computes the possible overlap of two polygons.
		// It uses homogeneous coordinate techniques to determine the overlap area
		// between two convex polygons.  Results are stored in the homogeneous coordinate (HC) arrays.

		// METHODOLOGY EMPLOYED:
		// The vertices defining the overlap between fig.1 and fig.2
		// consist of: the vertices of fig.1 enclosed by fig.2 (A)
		// plus the vertices of fig.2 enclosed by fig.1 (B)
		// plus the intercepts of fig.1 and fig.2 (C & D)

		//                               +----------------------+
		//                               !                      !
		//                               !         FIG.2        !
		//                               !                      !
		//                +--------------C----------A           !
		//                !              !         /            !
		//                !              !        /             !
		//                !              B-------D--------------+
		//                !    FIG.1            /
		//                !                    /
		//                +-------------------+

		// REFERENCES:
		// BLAST/IBLAST code, original author George Walton

		// Using/Aliasing
		using General::RoundSigDigits;
		using DataSystemVariables::SutherlandHodgman;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int N; // Loop index
		int NV1; // Number of vertices of figure NS1
		int NV2; // Number of vertices of figure NS2
		int NV3; // Number of vertices of figure NS3 (the overlap of NS1 and NS2)
		int NIN1; // Number of vertices of NS1 within NS2
		int NIN2; // Number of vertices of NS2 within NS1
		static bool TooManyFiguresMessage( false );
		static bool TooManyVerticesMessage( false );

		// Check for exceeding array limits.
#ifdef EP_Count_Calls
		++NumDetPolyOverlap_Calls;
#endif

		if ( NS3 > MaxHCS ) {

			OverlapStatus = TooManyFigures;

			if ( ! TooManyFiguresMessage && ! DisplayExtraWarnings ) {
				ShowWarningError( "DeterminePolygonOverlap: Too many figures [>" + RoundSigDigits( MaxHCS ) + "]  detected in an overlap calculation. Use Output:Diagnostics,DisplayExtraWarnings; for more details." );
				TooManyFiguresMessage = true;
			}

			if ( DisplayExtraWarnings ) {
				TrackTooManyFigures.redimension( ++NumTooManyFigures );
				TrackTooManyFigures( NumTooManyFigures ).SurfIndex1 = CurrentShadowingSurface;
				TrackTooManyFigures( NumTooManyFigures ).SurfIndex2 = CurrentSurfaceBeingShadowed;
			}

			return;

		}

		OverlapStatus = PartialOverlap;
		NV1 = HCNV( NS1 );
		NV2 = HCNV( NS2 );
		NV3 = 0;

		if ( ! SutherlandHodgman ) {
			INCLOS( NS1, NV1, NS2, NV2, NV3, NIN1 ); // Find vertices of NS1 within NS2.

			if ( NIN1 >= NV1 ) {

				OverlapStatus = FirstSurfWithinSecond;

			} else {

				INCLOS( NS2, NV2, NS1, NV1, NV3, NIN2 ); // Find vertices of NS2 within NS1.

				if ( NIN2 >= NV2 ) {

					OverlapStatus = SecondSurfWithinFirst;

				} else {

					INTCPT( NV1, NV2, NV3, NS1, NS2 ); // Find intercepts of NS1 & NS2.

					if ( NV3 < 3 ) { // Overlap must have 3 or more vertices
						OverlapStatus = NoOverlap;
						return;
					}

				}

			}

		} else {
			// simple polygon clipping
			CLIPPOLY( NS1, NS2, NV1, NV2, NV3 );
		}

		if ( NV3 < MaxHCV && NS3 <= MaxHCS ) {

			if ( ! SutherlandHodgman ) {
				ORDER( NV3, NS3 ); // Put vertices in clockwise order.
			} else {
				assert( equal_dimensions( HCX, HCY ) );
				auto l( HCX.index( NS3, 1 ) );
				for ( N = 1; N <= NV3; ++N, ++l ) {
					HCX[ l ] = nint64( XTEMP( N ) ); // [ l ] == ( N, NS3 )
					HCY[ l ] = nint64( YTEMP( N ) );
				}
			}

			HTRANS0( NS3, NV3 ); // Determine h.c. values of sides.
			// Skip overlaps of negligible area.

			if ( std::abs( HCAREA( NS3 ) ) * HCMULT < std::abs( HCAREA( NS1 ) ) ) {
				OverlapStatus = NoOverlap;
			} else {
				if ( HCAREA( NS1 ) * HCAREA( NS2 ) > 0.0 ) HCAREA( NS3 ) = -HCAREA( NS3 ); // Determine sign of area of overlap
				Real64 const HCT_1( HCT( NS1 ) );
				Real64 const HCT_2( HCT( NS2 ) );
				Real64 HCT_3( HCT_2 * HCT_1 ); // Determine transmission of overlap
				if ( HCT_2 >= 0.5 && HCT_1 >= 0.5 ) {
					if ( HCT_2 != 1.0 && HCT_1 != 1.0 ) {
						HCT_3 = 1.0 - HCT_3;
					}
				}
				HCT( NS3 ) = HCT_3;
			}

		} else if ( NV3 > MaxHCV ) {

			OverlapStatus = TooManyVertices;

			if ( ! TooManyVerticesMessage && ! DisplayExtraWarnings ) {
				ShowWarningError( "DeterminePolygonOverlap: Too many vertices [>" + RoundSigDigits( MaxHCV ) + "] detected in an overlap calculation. Use Output:Diagnostics,DisplayExtraWarnings; for more details." );
				TooManyVerticesMessage = true;
			}

			if ( DisplayExtraWarnings ) {
				TrackTooManyVertices.redimension( ++NumTooManyVertices );
				TrackTooManyVertices( NumTooManyVertices ).SurfIndex1 = CurrentShadowingSurface;
				TrackTooManyVertices( NumTooManyVertices ).SurfIndex2 = CurrentSurfaceBeingShadowed;
			}

		} else if ( NS3 > MaxHCS ) {

			OverlapStatus = TooManyFigures;

			if ( ! TooManyFiguresMessage && ! DisplayExtraWarnings ) {
				ShowWarningError( "DeterminePolygonOverlap: Too many figures [>" + RoundSigDigits( MaxHCS ) + "]  detected in an overlap calculation. Use Output:Diagnostics,DisplayExtraWarnings; for more details." );
				TooManyFiguresMessage = true;
			}

			if ( DisplayExtraWarnings ) {
				TrackTooManyFigures.redimension( ++NumTooManyFigures );
				TrackTooManyFigures( NumTooManyFigures ).SurfIndex1 = CurrentShadowingSurface;
				TrackTooManyFigures( NumTooManyFigures ).SurfIndex2 = CurrentSurfaceBeingShadowed;
			}

		}

	}

	void
	CalcPerSolarBeam(
		Real64 const AvgEqOfTime, // Average value of Equation of Time for period
		Real64 const AvgSinSolarDeclin, // Average value of Sine of Solar Declination for period
		Real64 const AvgCosSolarDeclin // Average value of Cosine of Solar Declination for period
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Legacy Code
		//       DATE WRITTEN
		//       MODIFIED       BG, Nov 2012 - Timestep solar.  DetailedSolarTimestepIntegration
		//       RE-ENGINEERED  Lawrie, Oct 2000

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages computation of solar gain multipliers for beam radiation.  These
		// are calculated for a period of days depending on the input "Shadowing Calculations".

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// BLAST/IBLAST code, original author George Walton

		// Using/Aliasing
		using WindowComplexManager::InitComplexWindows;
		using WindowComplexManager::UpdateComplexWindows;
		using DataSystemVariables::DetailedSkyDiffuseAlgorithm;
		using DataSystemVariables::DetailedSolarTimestepIntegration;
		using DataGlobals::HourOfDay;
		using DataGlobals::TimeStep;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int iHour; // Hour index number
		int TS; // TimeStep Loop Counter
		static bool Once( true );

		if ( Once ) InitComplexWindows();
		Once = false;

		if ( KickOffSizing || KickOffSimulation ) return; // Skip solar calcs for these Initialization steps.

#ifdef EP_Count_Calls
		++NumCalcPerSolBeam_Calls;
#endif

		// Intialize some values for the appropriate period
		if ( ! DetailedSolarTimestepIntegration ) {
			SunlitFracHR = 0.0;
			SunlitFrac = 0.0;
			SunlitFracWithoutReveal = 0.0;
			CTHETA = 0.0;
			CosIncAngHR = 0.0;
			CosIncAng = 0.0;
			AOSurf = 0.0;
			BackSurfaces = 0;
			OverlapAreas = 0.0;
			for ( auto & e : SurfaceWindow ) {
				e.OutProjSLFracMult = 1.0;
				e.InOutProjSLFracMult = 1.0;
			}
		} else {
			SunlitFracHR( HourOfDay, {1,TotSurfaces} ) = 0.0;
			SunlitFrac( TimeStep, HourOfDay, {1,TotSurfaces} ) = 0.0;
			SunlitFracWithoutReveal( TimeStep, HourOfDay, {1,TotSurfaces} ) = 0.0;
			CTHETA( {1,TotSurfaces} ) = 0.0;
			CosIncAngHR( HourOfDay, {1,TotSurfaces} ) = 0.0;
			CosIncAng( TimeStep, HourOfDay, {1,TotSurfaces} ) = 0.0;
			AOSurf( {1,TotSurfaces} ) = 0.0;
			BackSurfaces( TimeStep, HourOfDay, {1,MaxBkSurf}, {1,TotSurfaces} ) = 0;
			OverlapAreas( TimeStep, HourOfDay, {1,MaxBkSurf}, {1,TotSurfaces} ) = 0.0;
			for ( int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
				SurfaceWindow( SurfNum ).OutProjSLFracMult( HourOfDay ) = 1.0;
				SurfaceWindow( SurfNum ).InOutProjSLFracMult( HourOfDay ) = 1.0;
			}
		}

		if ( ! DetailedSolarTimestepIntegration ) {
			for ( iHour = 1; iHour <= 24; ++iHour ) { // Do for all hours
				for ( TS = 1; TS <= NumOfTimeStepInHour; ++TS ) {
					FigureSunCosines( iHour, TS, AvgEqOfTime, AvgSinSolarDeclin, AvgCosSolarDeclin );
				}
			}
		} else {
			FigureSunCosines( HourOfDay, TimeStep, AvgEqOfTime, AvgSinSolarDeclin, AvgCosSolarDeclin );
		}
		// Initialize/update the Complex Fenestration geometry and optical properties
		UpdateComplexWindows();
		if ( ! DetailedSolarTimestepIntegration ) {
			for ( iHour = 1; iHour <= 24; ++iHour ) { // Do for all hours.
				for ( TS = 1; TS <= NumOfTimeStepInHour; ++TS ) {
					FigureSolarBeamAtTimestep( iHour, TS );
				} // TimeStep Loop
			} // Hour Loop
		} else {
			FigureSolarBeamAtTimestep( HourOfDay, TimeStep );
		}

	}

	void
	FigureSunCosines(
		int const iHour,
		int const iTimeStep,
		Real64 const EqOfTime, // value of Equation of Time for period
		Real64 const SinSolarDeclin, // value of Sine of Solar Declination for period
		Real64 const CosSolarDeclin // value of Cosine of Solar Declination for period
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   October 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Determine solar position.  Default for sun below horizon.

		// METHODOLOGY EMPLOYED:
		// Given hour, timestep, equation of time, solar declination sine, and solar declination cosine,
		// determine sun directions for use elsewhere

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::TimeStepZone;
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
		Real64 CurrentTime; // Current Time for passing to Solar Position Routine

		if ( NumOfTimeStepInHour != 1 ) {
			CurrentTime = double( iHour - 1 ) + double( iTimeStep ) * ( TimeStepZone );
		} else {
			CurrentTime = double( iHour ) + TS1TimeOffset;
		}
		SUN4( CurrentTime, EqOfTime, SinSolarDeclin, CosSolarDeclin );

		// Save hourly values for use in DaylightingManager
		if ( ! DetailedSolarTimestepIntegration ) {
			if ( iTimeStep == NumOfTimeStepInHour ) SUNCOSHR( iHour, {1,3} ) = SUNCOS;
		} else {
			SUNCOSHR( iHour, {1,3} ) = SUNCOS;
		}
		// Save timestep values for use in WindowComplexManager
		SUNCOSTS( iTimeStep, iHour, {1,3} ) = SUNCOS;

	}

	void
	FigureSolarBeamAtTimestep(
		int const iHour,
		int const iTimeStep
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B.Griffith, derived from CalcPerSolarBeam, Legacy and Lawrie.
		//       DATE WRITTEN   October 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine computes solar gain multipliers for beam solar

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataSystemVariables::DetailedSkyDiffuseAlgorithm;
		using DataSystemVariables::DetailedSolarTimestepIntegration;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const NPhi( 6 ); // Number of altitude angle steps for sky integration
		int const NTheta( 24 ); // Number of azimuth angle steps for sky integration
		Real64 const Eps( 1.e-10 ); // Small number
		Real64 const DPhi( PiOvr2 / NPhi ); // Altitude step size, 15 deg for NPhi = 6
		Real64 const DTheta( 2.0 * Pi / NTheta ); // Azimuth step size, 15 deg for NTheta = 24
		Real64 const DThetaDPhi( DTheta * DPhi ); // Product of DTheta and DPhi
		Real64 const PhiMin( 0.5 * DPhi ); // Minimum altitude

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS
		Real64 SurfArea; // Surface area. For walls, includes all window frame areas.
		Real64 CosPhi; // Cosine of Phi
		int SurfNum; // Surface Loop index
		Real64 Fac1WoShdg; // Intermediate calculation factor, without shading
		Real64 Fac1WithShdg; // Intermediate calculation factor, with shading

		// Recover the sun direction from the array stored in previous loop
		SUNCOS = SUNCOSTS( iTimeStep, iHour, {1,3} );

		CTHETA = 0.0;

		if ( SUNCOS( 3 ) < SunIsUpValue ) return;

		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			CTHETA( SurfNum ) = SUNCOS( 1 ) * Surface( SurfNum ).OutNormVec( 1 ) + SUNCOS( 2 ) * Surface( SurfNum ).OutNormVec( 2 ) + SUNCOS( 3 ) * Surface( SurfNum ).OutNormVec( 3 );
			if ( ! DetailedSolarTimestepIntegration ) {
				if ( iTimeStep == NumOfTimeStepInHour ) CosIncAngHR( iHour, SurfNum ) = CTHETA( SurfNum );
			} else {
				CosIncAngHR( iHour, SurfNum ) = CTHETA( SurfNum );
			}
			CosIncAng( iTimeStep, iHour, SurfNum ) = CTHETA( SurfNum );
		}

		SHADOW( iHour, iTimeStep ); // Determine sunlit areas and solar multipliers for all surfaces.

		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			if ( Surface( SurfNum ).Area >= 1.e-10 ) {
				SurfArea = Surface( SurfNum ).NetAreaShadowCalc;
				if ( ! DetailedSolarTimestepIntegration ) {
					if ( iTimeStep == NumOfTimeStepInHour ) SunlitFracHR( iHour, SurfNum ) = SAREA( SurfNum ) / SurfArea;
				} else {
					SunlitFracHR( iHour, SurfNum ) = SAREA( SurfNum ) / SurfArea;
				}
				SunlitFrac( iTimeStep, iHour, SurfNum ) = SAREA( SurfNum ) / SurfArea;
				if ( SunlitFrac( iTimeStep, iHour, SurfNum ) < 1.e-5 ) SunlitFrac( iTimeStep, iHour, SurfNum ) = 0.0;
			}

			//Added check
			if ( SunlitFrac( iTimeStep, iHour, SurfNum ) > 1.0 ) {
				SunlitFrac( iTimeStep, iHour, SurfNum ) = 1.0;
			}
		}

		//   Note -- if not the below, values are set in SkyDifSolarShading routine (constant for simulation)
		if ( DetailedSkyDiffuseAlgorithm && ShadingTransmittanceVaries && SolarDistribution != MinimalShadowing ) {
			CosPhi = 1.0 - SUNCOS( 3 );

			for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {

				if ( ! Surface( SurfNum ).ShadowingSurf && ( ! Surface( SurfNum ).HeatTransSurf || ! Surface( SurfNum ).ExtSolar || ( Surface( SurfNum ).ExtBoundCond != ExternalEnvironment && Surface( SurfNum ).ExtBoundCond != OtherSideCondModeledExt ) ) ) continue;

				if ( CTHETA( SurfNum ) < 0.0 ) continue;

				Fac1WoShdg = CosPhi * DThetaDPhi * CTHETA( SurfNum );
				Fac1WithShdg = Fac1WoShdg * SunlitFrac( iTimeStep, iHour, SurfNum );
				WithShdgIsoSky( SurfNum ) = Fac1WithShdg;
				WoShdgIsoSky( SurfNum ) = Fac1WoShdg;

				// Horizon region
				if ( SUNCOS( 3 ) <= PhiMin ) {
					WithShdgHoriz( SurfNum ) = Fac1WithShdg;
					WoShdgHoriz( SurfNum ) = Fac1WoShdg;
				}
			} // End of surface loop

			for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {

				if ( ! Surface( SurfNum ).ShadowingSurf && ( ! Surface( SurfNum ).HeatTransSurf || ! Surface( SurfNum ).ExtSolar || ( Surface( SurfNum ).ExtBoundCond != ExternalEnvironment && Surface( SurfNum ).ExtBoundCond != OtherSideCondModeledExt ) ) ) continue;

				if ( std::abs( WoShdgIsoSky( SurfNum ) ) > Eps ) {
					DifShdgRatioIsoSkyHRTS( iTimeStep, iHour, SurfNum ) = ( WithShdgIsoSky( SurfNum ) ) / ( WoShdgIsoSky( SurfNum ) );
				} else {
					DifShdgRatioIsoSkyHRTS( iTimeStep, iHour, SurfNum ) = ( WithShdgIsoSky( SurfNum ) ) / ( WoShdgIsoSky( SurfNum ) + Eps );
				}
				if ( std::abs( WoShdgHoriz( SurfNum ) ) > Eps ) {
					DifShdgRatioHorizHRTS( iTimeStep, iHour, SurfNum ) = ( WithShdgHoriz( SurfNum ) ) / ( WoShdgHoriz( SurfNum ) );
				} else {
					DifShdgRatioHorizHRTS( iTimeStep, iHour, SurfNum ) = ( WithShdgHoriz( SurfNum ) ) / ( WoShdgHoriz( SurfNum ) + Eps );
				}
			}

			//  ! Get IR view factors. An exterior surface can receive IR radiation from
			//  ! sky, ground or shadowing surfaces. Assume shadowing surfaces have same
			//  ! temperature as outside air (and therefore same temperature as ground),
			//  ! so that the view factor to these shadowing surfaces can be included in
			//  ! the ground view factor. Sky IR is assumed to be isotropic and shadowing
			//  ! surfaces are assumed to be opaque to IR so they totally "shade" IR from
			//  ! sky or ground.

			//  DO SurfNum = 1,TotSurfaces
			//    Surface(SurfNum)%ViewFactorSkyIR = Surface(SurfNum)%ViewFactorSkyIR * DifShdgRatioIsoSky(SurfNum,IHOUR,TS)
			//    Surface(SurfNum)%ViewFactorGroundIR = 1.0 - Surface(SurfNum)%ViewFactorSkyIR
			//  END DO

		} // test for shading surfaces

		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			// For exterior windows with frame/divider that are partially or fully sunlit,
			// correct SunlitFrac due to shadowing of frame and divider projections onto window glass.
			// Note: if SunlitFrac = 0.0 the window is either completely shaded or the sun is in back
			// of the window; in either case, frame/divider shadowing doesn't have to be done.

			if ( Surface( SurfNum ).Class == SurfaceClass_Window && Surface( SurfNum ).ExtBoundCond == ExternalEnvironment && SunlitFrac( iTimeStep, iHour, SurfNum ) > 0.0 && Surface( SurfNum ).FrameDivider > 0 ) CalcFrameDividerShadow( SurfNum, Surface( SurfNum ).FrameDivider, iHour );
		}

	}

	void
	DetermineShadowingCombinations()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         From Legacy Code
		//       DATE WRITTEN
		//       MODIFIED       LKL; March 2002 -- another missing translation from BLAST's routine
		//                      FCW; Jan 2003 -- removed line that prevented beam solar through interior windows
		//       RE-ENGINEERED  Rick Strand; 1998
		//                      Linda Lawrie; Oct 2000

		// PURPOSE OF THIS SUBROUTINE:
		// This routine prepares a list of heat transfer surfaces and
		// their possible shadowers which is used to direct the hourly
		// calculation of shadows and sunlit areas.

		// METHODOLOGY EMPLOYED:
		// As appropriate surfaces are identified, they are placed into the
		// ShadowComb data structure (module level) with the accompanying lists
		// of other surface numbers.

		// REFERENCES:
		// BLAST/IBLAST code, original author George Walton

		// Using/Aliasing
		using namespace DataErrorTracking;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt fmtA( "(A)" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Array1D_int GSS; // List of shadowing surfaces numbers for a receiving surface
		Array1D_int BKS; // List of back surface numbers for a receiving surface
		Array1D_int SBS; // List of subsurfaces for a receiving surface
		static int MaxGSS( 50 ); // Current Max for GSS array
		static int MaxBKS( 50 ); // Current Max for BKS array
		static int MaxSBS( 50 ); // Current Max for SBS array
		bool CannotShade; // TRUE if subsurface cannot shade receiving surface
		bool HasWindow; // TRUE if a window is present on receiving surface
		Real64 ZMIN; // Lowest point on the receiving surface
		int BackSurfaceNumber; // Back surface number
		int HTS; // Heat transfer surface number for a receiving surface
		int GRSNR; // Receiving surface number
		int GSSNR; // Shadowing surface number
		int SBSNR; // Subsurface number
		int NBKS; // Number of back surfaces for a receiving surface
		int NGSS; // Number of shadowing surfaces for a receiving surface
		int NSBS; // Number of subsurfaces for a receiving surface
		bool ShadowingSurf; // True if a receiving surface is a shadowing surface
		Array1D_bool CastingSurface; // tracking during setup of ShadowComb

		static int MaxDim( 0 );

#ifdef EP_Count_Calls
		++NumDetShadowCombs_Calls;
#endif

		ShadowComb.dimension( TotSurfaces, ShadowingCombinations{} ); // Set all elements to default constructed state

		CastingSurface.dimension( TotSurfaces, false );

		HCA.dimension( 2 * MaxHCS, MaxHCV + 1, 0 );
		HCB.dimension( 2 * MaxHCS, MaxHCV + 1, 0 );
		HCC.dimension( 2 * MaxHCS, MaxHCV + 1, 0 );
		HCX.dimension( 2 * MaxHCS, MaxHCV + 1, 0 );
		HCY.dimension( 2 * MaxHCS, MaxHCV + 1, 0 );
		HCAREA.dimension( 2 * MaxHCS, 0.0 );
		HCNS.dimension( 2 * MaxHCS, 0 );
		HCNV.dimension( 2 * MaxHCS, 0 );
		HCT.dimension( 2 * MaxHCS, 0.0 );

		GSS.dimension( MaxGSS, 0 );
		BKS.dimension( MaxGSS, 0 );
		SBS.dimension( MaxGSS, 0 );

		HTS = 0;

		// Check every surface as a possible shadow receiving surface ("RS" = receiving surface).
		if ( IgnoreSolarRadiation ) {
			return;
		}

		for ( GRSNR = 1; GRSNR <= TotSurfaces; ++GRSNR ) { // Loop through all surfaces (looking for potential receiving surfaces)...

			ShadowingSurf = Surface( GRSNR ).ShadowingSurf;
			NGSS = 0;
			NSBS = 0;
			NBKS = 0;

			if ( ! ShadowingSurf && ! Surface( GRSNR ).HeatTransSurf ) continue;
			HTS = GRSNR;
			if ( ! ShadowingSurf && ! Surface( GRSNR ).ExtSolar ) continue; // Skip surfaces with no external solar

			if ( ! ShadowingSurf && Surface( GRSNR ).BaseSurf != GRSNR ) continue; // Skip subsurfaces (SBS)

			// Get the lowest point of receiving surface
			ZMIN = minval( Surface( GRSNR ).Vertex, &Vector::z );

			// Check every surface as a possible shadow casting surface ("SS" = shadow sending)
			NGSS = 0;
			if ( SolarDistribution != MinimalShadowing ) { // Except when doing simplified exterior shadowing.

				for ( GSSNR = 1; GSSNR <= TotSurfaces; ++GSSNR ) { // Loop through all surfaces, looking for ones that could shade GRSNR

					if ( GSSNR == GRSNR ) continue; // Receiving surface cannot shade itself
					if ( ( Surface( GSSNR ).HeatTransSurf ) && ( Surface( GSSNR ).BaseSurf == GRSNR ) ) continue; // A heat transfer subsurface of a receiving surface
					// cannot shade the receiving surface
					if ( ShadowingSurf ) {
						// If receiving surf is a shadowing surface exclude matching shadow surface as sending surface
						//IF((GSSNR == GRSNR+1 .AND. Surface(GSSNR)%Name(1:3) == 'Mir').OR. &
						//   (GSSNR == GRSNR-1 .AND. Surface(GRSNR)%Name(1:3) == 'Mir')) CYCLE
						if ( ( ( GSSNR == GRSNR + 1 ) && Surface( GSSNR ).MirroredSurf ) || ( ( GSSNR == GRSNR - 1 ) && Surface( GRSNR ).MirroredSurf ) ) continue;
					}

					if ( Surface( GSSNR ).BaseSurf == GRSNR ) { // Shadowing subsurface of receiving surface

						++NGSS;
						if ( NGSS > MaxGSS ) {
							GSS.redimension( MaxGSS *= 2, 0 );
						}
						GSS( NGSS ) = GSSNR;

					} else if ( ( Surface( GSSNR ).BaseSurf == 0 ) || ( ( Surface( GSSNR ).BaseSurf == GSSNR ) && ( ( Surface( GSSNR ).ExtBoundCond == ExternalEnvironment ) || Surface( GSSNR ).ExtBoundCond == OtherSideCondModeledExt ) ) ) { // Detached shadowing surface or | any other base surface exposed to outside environment

						CHKGSS( GRSNR, GSSNR, ZMIN, CannotShade ); // Check to see if this can shade the receiving surface
						if ( ! CannotShade ) { // Update the shadowing surface data if shading is possible
							++NGSS;
							if ( NGSS > MaxGSS ) {
								GSS.redimension( MaxGSS *= 2, 0 );
							}
							GSS( NGSS ) = GSSNR;
						}

					}

				} // ...end of surfaces DO loop (GSSNR)
			} else { // Simplified Distribution -- still check for Shading Subsurfaces

				for ( GSSNR = 1; GSSNR <= TotSurfaces; ++GSSNR ) { // Loop through all surfaces (looking for surfaces which could shade GRSNR) ...

					if ( GSSNR == GRSNR ) continue; // Receiving surface cannot shade itself
					if ( ( Surface( GSSNR ).HeatTransSurf ) && ( Surface( GSSNR ).BaseSurf == GRSNR ) ) continue; // Skip heat transfer subsurfaces of receiving surface
					if ( Surface( GSSNR ).BaseSurf == GRSNR ) { // Shadowing subsurface of receiving surface
						++NGSS;
						if ( NGSS > MaxGSS ) {
							GSS.redimension( MaxGSS *= 2, 0 );
						}
						GSS( NGSS ) = GSSNR;
					}
				}

			} // ...end of check for simplified solar distribution

			// Check every surface as a receiving subsurface of the receiving surface
			NSBS = 0;
			HasWindow = false;
			//legacy: IF (OSENV(HTS) > 10) WINDOW=.TRUE. -->Note: WINDOW was set true for roof ponds, solar walls, or other zones
			for ( SBSNR = 1; SBSNR <= TotSurfaces; ++SBSNR ) { // Loop through the surfaces yet again (looking for subsurfaces of GRSNR)...

				if ( ! Surface( SBSNR ).HeatTransSurf ) continue; // Skip non heat transfer subsurfaces
				if ( SBSNR == GRSNR ) continue; // Surface itself cannot be its own subsurface
				if ( Surface( SBSNR ).BaseSurf != GRSNR ) continue; // Ignore subsurfaces of other surfaces and other surfaces

				if ( Construct( Surface( SBSNR ).Construction ).TransDiff > 0.0 ) HasWindow = true; // Check for window
				CHKSBS( HTS, GRSNR, SBSNR ); // Check that the receiving surface completely encloses the subsurface;
				// severe error if not
				++NSBS;
				if ( NSBS > MaxSBS ) {
					SBS.redimension( MaxSBS *= 2, 0 );
				}
				SBS( NSBS ) = SBSNR;

			} // ...end of surfaces DO loop (SBSNR)

			// Check every surface as a back surface
			NBKS = 0;
			//                                        Except for simplified
			//                                        interior solar distribution,
			if ( ( SolarDistribution == FullInteriorExterior ) && ( HasWindow ) ) { // For full interior solar distribution | and a window present on base surface (GRSNR)

				for ( BackSurfaceNumber = 1; BackSurfaceNumber <= TotSurfaces; ++BackSurfaceNumber ) { // Loop through surfaces yet again, looking for back surfaces to GRSNR

					if ( ! Surface( BackSurfaceNumber ).HeatTransSurf ) continue; // Skip non-heat transfer surfaces
					if ( Surface( BackSurfaceNumber ).BaseSurf == GRSNR ) continue; // Skip subsurfaces of this GRSNR
					if ( BackSurfaceNumber == GRSNR ) continue; // A back surface cannot be GRSNR itself
					if ( Surface( BackSurfaceNumber ).Zone != Surface( GRSNR ).Zone ) continue; // Skip if back surface not in zone

					if ( Surface( BackSurfaceNumber ).Class == SurfaceClass_IntMass ) continue;

					// Following line removed 1/27/03 by FCW. Was in original code that didn't do beam solar transmitted through
					// interior windows. Was removed to allow such beam solar but then somehow was put back in.
					//IF (Surface(BackSurfaceNumber)%BaseSurf /= BackSurfaceNumber) CYCLE ! Not for subsurfaces of Back Surface

					CHKBKS( BackSurfaceNumber, GRSNR ); // CHECK FOR CONVEX ZONE; severe error if not
					++NBKS;
					if ( NBKS > MaxBKS ) {
						BKS.redimension( MaxBKS *= 2, 0 );
					}
					BKS( NBKS ) = BackSurfaceNumber;

				} // ...end of surfaces DO loop (BackSurfaceNumber)

			}

			// Put this into the ShadowComb data structure
			ShadowComb( GRSNR ).UseThisSurf = true;
			ShadowComb( GRSNR ).NumGenSurf = NGSS;
			ShadowComb( GRSNR ).NumBackSurf = NBKS;
			ShadowComb( GRSNR ).NumSubSurf = NSBS;
			MaxDim = max( MaxDim, NGSS, NBKS, NSBS );

			ShadowComb( GRSNR ).GenSurf.allocate( {0,ShadowComb( GRSNR ).NumGenSurf} );
			ShadowComb( GRSNR ).GenSurf( 0 ) = 0;
			if ( ShadowComb( GRSNR ).NumGenSurf > 0 ) {
				ShadowComb( GRSNR ).GenSurf( {1,ShadowComb( GRSNR ).NumGenSurf} ) = GSS( {1,NGSS} );
			}

			ShadowComb( GRSNR ).BackSurf.allocate( {0,ShadowComb( GRSNR ).NumBackSurf} );
			ShadowComb( GRSNR ).BackSurf( 0 ) = 0;
			if ( ShadowComb( GRSNR ).NumBackSurf > 0 ) {
				ShadowComb( GRSNR ).BackSurf( {1,ShadowComb( GRSNR ).NumBackSurf} ) = BKS( {1,NBKS} );
			}

			ShadowComb( GRSNR ).SubSurf.allocate( {0,ShadowComb( GRSNR ).NumSubSurf} );
			ShadowComb( GRSNR ).SubSurf( 0 ) = 0;
			if ( ShadowComb( GRSNR ).NumSubSurf > 0 ) {
				ShadowComb( GRSNR ).SubSurf( {1,ShadowComb( GRSNR ).NumSubSurf} ) = SBS( {1,NSBS} );
			}

		} // ...end of surfaces (GRSNR) DO loop

		GSS.deallocate();
		SBS.deallocate();
		BKS.deallocate();

		shd_stream << "Shadowing Combinations\n";
		if ( SolarDistribution == MinimalShadowing ) {
			shd_stream << "..Solar Distribution=Minimal Shadowing, Detached Shading will not be used in shadowing calculations\n";
		} else if ( SolarDistribution == FullExterior ) {
			if ( CalcSolRefl ) {
				shd_stream << "..Solar Distribution=FullExteriorWithReflectionsFromExteriorSurfaces\n";
			} else {
				shd_stream << "..Solar Distribution=FullExterior\n";
			}
		} else if ( SolarDistribution == FullInteriorExterior ) {
			if ( CalcSolRefl ) {
				shd_stream << "..Solar Distribution=FullInteriorAndExteriorWithReflectionsFromExteriorSurfaces\n";
			} else {
				shd_stream << "..Solar Distribution=FullInteriorAndExterior\n";
			}
		} else {
		}

		shd_stream << "..In the following, only the first 10 reference surfaces will be shown.\n";
		shd_stream << "..But all surfaces are used in the calculations.\n";

		for ( HTS = 1; HTS <= TotSurfaces; ++HTS ) {
			shd_stream << "==================================\n";
			if ( ShadowComb( HTS ).UseThisSurf ) {
				if ( Surface( HTS ).IsConvex ) {
					shd_stream << "Surface=" << Surface( HTS ).Name << " is used as Receiving Surface in calculations and is convex.\n";
				} else {
					shd_stream << "Surface=" << Surface( HTS ).Name << " is used as Receiving Surface in calculations and is non-convex.\n";
					if ( ShadowComb( HTS ).NumGenSurf > 0 ) {
						if ( DisplayExtraWarnings ) {
							ShowWarningError( "DetermineShadowingCombinations: Surface=\"" + Surface( HTS ).Name + "\" is a receiving surface and is non-convex." );
							ShowContinueError( "...Shadowing values may be inaccurate. Check .shd report file for more surface shading details" );
						} else {
							++TotalReceivingNonConvexSurfaces;
						}
					}
				}
			} else {
				shd_stream << "Surface=" << Surface( HTS ).Name << " is not used as Receiving Surface in calculations.\n";
			}
			shd_stream << "Number of general casting surfaces=" << ShadowComb( HTS ).NumGenSurf << '\n';
			for ( NGSS = 1; NGSS <= ShadowComb( HTS ).NumGenSurf; ++NGSS ) {
				if ( NGSS <= 10 ) shd_stream << "..Surface=" << Surface( ShadowComb( HTS ).GenSurf( NGSS ) ).Name << '\n';
				CastingSurface( ShadowComb( HTS ).GenSurf( NGSS ) ) = true;
			}
			shd_stream << "Number of back surfaces=" << ShadowComb( HTS ).NumBackSurf << '\n';
			for ( NGSS = 1; NGSS <= min( 10, ShadowComb( HTS ).NumBackSurf ); ++NGSS ) {
				shd_stream << "...Surface=" << Surface( ShadowComb( HTS ).BackSurf( NGSS ) ).Name << '\n';
			}
			shd_stream << "Number of receiving sub surfaces=" << ShadowComb( HTS ).NumSubSurf << '\n';
			for ( NGSS = 1; NGSS <= min( 10, ShadowComb( HTS ).NumSubSurf ); ++NGSS ) {
				shd_stream << "....Surface=" << Surface( ShadowComb( HTS ).SubSurf( NGSS ) ).Name << '\n';
			}
		}

		for ( HTS = 1; HTS <= TotSurfaces; ++HTS ) {
			if ( CastingSurface( HTS ) && ! Surface( HTS ).IsConvex ) {
				if ( DisplayExtraWarnings ) {
					ShowSevereError( "DetermineShadowingCombinations: Surface=\"" + Surface( HTS ).Name + "\" is a casting surface and is non-convex." );
					ShowContinueError( "...Shadowing values may be inaccurate. Check .shd report file for more surface shading details" );
				} else {
					++TotalCastingNonConvexSurfaces;
				}
			}
		}

		CastingSurface.deallocate();

		if ( TotalReceivingNonConvexSurfaces > 0 ) {
			ShowWarningMessage( "DetermineShadowingCombinations: There are " + TrimSigDigits( TotalReceivingNonConvexSurfaces ) + " surfaces which are receiving surfaces and are non-convex." );
			ShowContinueError( "...Shadowing values may be inaccurate. Check .shd report file for more surface shading details" );
			ShowContinueError( "...Add Output:Diagnostics,DisplayExtraWarnings; to see individual warnings for each surface." );
			TotalWarningErrors += TotalReceivingNonConvexSurfaces;
		}

		if ( TotalCastingNonConvexSurfaces > 0 ) {
			ShowSevereMessage( "DetermineShadowingCombinations: There are " + TrimSigDigits( TotalCastingNonConvexSurfaces ) + " surfaces which are casting surfaces and are non-convex." );
			ShowContinueError( "...Shadowing values may be inaccurate. Check .shd report file for more surface shading details" );
			ShowContinueError( "...Add Output:Diagnostics,DisplayExtraWarnings; to see individual severes for each surface." );
			TotalSevereErrors += TotalCastingNonConvexSurfaces;
		}

	}

	void
	SHADOW(
		int const iHour, // Hour index
		int const TS // Time Step
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Legacy Code
		//       DATE WRITTEN
		//       MODIFIED       Nov 2003, FCW: modify to do shadowing on shadowing surfaces
		//       RE-ENGINEERED  Lawrie, Oct 2000

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is a driving routine for calculations of shadows
		// and sunlit areas used in computing the solar beam flux multipliers.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// BLAST/IBLAST code, original author George Walton

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
		Real64 XS; // Intermediate result
		Real64 YS; // Intermediate result
		Real64 ZS; // Intermediate result
		int N; // Vertex number
		int NGRS; // Coordinate transformation index
		int NZ; // Zone Number of surface
		int NVT;
		static Array1D< Real64 > XVT; // X Vertices of Shadows
		static Array1D< Real64 > YVT; // Y vertices of Shadows
		static Array1D< Real64 > ZVT; // Z vertices of Shadows
		static bool OneTimeFlag( true );
		int HTS; // Heat transfer surface number of the general receiving surface
		int GRSNR; // Surface number of general receiving surface
		int NBKS; // Number of back surfaces
		int NGSS; // Number of general shadowing surfaces
		int NSBS; // Number of subsurfaces (windows and doors)
		Real64 SurfArea; // Surface area. For walls, includes all window frame areas.
		// For windows, includes divider area

		if ( OneTimeFlag ) {
			XVT.allocate( MaxVerticesPerSurface + 1 );
			YVT.allocate( MaxVerticesPerSurface + 1 );
			ZVT.allocate( MaxVerticesPerSurface + 1 );
			XVT = 0.0;
			YVT = 0.0;
			ZVT = 0.0;
			OneTimeFlag = false;
		}

#ifdef EP_Count_Calls
		if ( iHour == 0 ) {
			++NumShadow_Calls;
		} else {
			++NumShadowAtTS_Calls;
		}
#endif

		SAREA = 0.0;

		for ( GRSNR = 1; GRSNR <= TotSurfaces; ++GRSNR ) {

			if ( ! ShadowComb( GRSNR ).UseThisSurf ) continue;

			SAREA( GRSNR ) = 0.0;

			NZ = Surface( GRSNR ).Zone;
			NGSS = ShadowComb( GRSNR ).NumGenSurf;
			NGSSHC = 0;
			NBKS = ShadowComb( GRSNR ).NumBackSurf;
			NBKSHC = 0;
			NSBS = ShadowComb( GRSNR ).NumSubSurf;
			NRVLHC = 0;
			NSBSHC = 0;
			LOCHCA = 1;
			// Temporarily determine the old heat transfer surface number (HTS)
			HTS = GRSNR;

			if ( CTHETA( GRSNR ) < SunIsUpValue ) { //.001) THEN ! Receiving surface is not in the sun

				SAREA( HTS ) = 0.0;
				SHDSBS( iHour, GRSNR, NBKS, NSBS, HTS, TS );

			} else if ( ( NGSS <= 0 ) && ( NSBS <= 0 ) ) { // Simple surface--no shaders or subsurfaces

				SAREA( HTS ) = Surface( GRSNR ).NetAreaShadowCalc;
			} else { // Surface in sun and either shading surfaces or subsurfaces present (or both)

				NGRS = Surface( GRSNR ).BaseSurf;
				if ( Surface( GRSNR ).ShadowingSurf ) NGRS = GRSNR;

				// Compute the X and Y displacements of a shadow.
				XS = Surface( NGRS ).lcsx.x * SUNCOS( 1 ) + Surface( NGRS ).lcsx.y * SUNCOS( 2 ) + Surface( NGRS ).lcsx.z * SUNCOS( 3 );
				YS = Surface( NGRS ).lcsy.x * SUNCOS( 1 ) + Surface( NGRS ).lcsy.y * SUNCOS( 2 ) + Surface( NGRS ).lcsy.z * SUNCOS( 3 );
				ZS = Surface( NGRS ).lcsz.x * SUNCOS( 1 ) + Surface( NGRS ).lcsz.y * SUNCOS( 2 ) + Surface( NGRS ).lcsz.z * SUNCOS( 3 );

				if ( std::abs( ZS ) > 1.e-4 ) {
					XShadowProjection = XS / ZS;
					YShadowProjection = YS / ZS;
					if ( std::abs( XShadowProjection ) < 1.e-8 ) XShadowProjection = 0.0;
					if ( std::abs( YShadowProjection ) < 1.e-8 ) YShadowProjection = 0.0;
				} else {
					XShadowProjection = 0.0;
					YShadowProjection = 0.0;
				}

				CTRANS( GRSNR, NGRS, NVT, XVT, YVT, ZVT ); // Transform coordinates of the receiving surface to 2-D form

				// Re-order its vertices to clockwise sequential.
				for ( N = 1; N <= NVT; ++N ) {
					XVS( N ) = XVT( NVT + 1 - N );
					YVS( N ) = YVT( NVT + 1 - N );
				}

				HTRANS1( 1, NVT ); // Transform to homogeneous coordinates.

				HCAREA( 1 ) = -HCAREA( 1 ); // Compute (+) gross surface area.
				HCT( 1 ) = 1.0;

				SHDGSS( NGRS, iHour, TS, GRSNR, NGSS, HTS ); // Determine shadowing on surface.
				if ( ! CalcSkyDifShading ) {
					SHDBKS( NGRS, GRSNR, NBKS, HTS ); // Determine possible back surfaces.
				}

				SHDSBS( iHour, GRSNR, NBKS, NSBS, HTS, TS ); // Subtract subsurf areas from total

				// Error checking:  require that 0 <= SAREA <= AREA.  + or - .01*AREA added for round-off errors
				SurfArea = Surface( GRSNR ).NetAreaShadowCalc;
				SAREA( HTS ) = max( 0.0, SAREA( HTS ) );

				SAREA( HTS ) = min( SAREA( HTS ), SurfArea );

			} // ...end of surface in sun/surface with shaders and/or subsurfaces IF-THEN block

			// NOTE:
			// There used to be a call to legacy subroutine SHDCVR here when the
			// zone type was not a standard zone.

		}

	}

	void
	SHDBKS(
		int const NGRS, // Number of the general receiving surface
		int const CurSurf,
		int const NBKS, // Number of back surfaces
		int const HTS // Heat transfer surface number of the general receiving surf
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Legacy Code
		//       DATE WRITTEN
		//       MODIFIED       na
		//       RE-ENGINEERED  Lawrie, Oct 2000

		// PURPOSE OF THIS SUBROUTINE:
		// This is the driving subroutine for computing
		// the sunlit areas for back surfaces.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// BLAST/IBLAST code, original author George Walton

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
		typedef  Array2D< Int64 >::size_type  size_type;
		int I;
		int M;
		int N;
		int NVR;
		int NVT; // Number of vertices of back surface
		static Array1D< Real64 > XVT; // X,Y,Z coordinates of vertices of
		static Array1D< Real64 > YVT; // back surfaces projected into system
		static Array1D< Real64 > ZVT; // relative to receiving surface
		static bool OneTimeFlag( true );
		int BackSurfaceNumber;
		int NS1; // Number of the figure being overlapped
		int NS2; // Number of the figure doing overlapping
		int NS3; // Location to place results of overlap

		//Tuned Linear indexing

		assert( equal_dimensions( HCX, HCY ) );
		assert( equal_dimensions( HCX, HCA ) );

		if ( OneTimeFlag ) {
			XVT.allocate( MaxVerticesPerSurface + 1 );
			YVT.allocate( MaxVerticesPerSurface + 1 );
			ZVT.allocate( MaxVerticesPerSurface + 1 );
			XVT = 0.0;
			YVT = 0.0;
			ZVT = 0.0;
			OneTimeFlag = false;
		}

		if ( ( NBKS <= 0 ) || ( SAREA( HTS ) <= 0.0 ) || ( OverlapStatus == TooManyVertices ) || ( OverlapStatus == TooManyFigures ) ) return;

		FBKSHC = LOCHCA + 1;

		for ( I = 1; I <= NBKS; ++I ) { // Loop through all back surfaces associated with the receiving surface

			BackSurfaceNumber = ShadowComb( CurSurf ).BackSurf( I );

			if ( CTHETA( BackSurfaceNumber ) > -SunIsUpValue ) continue; //-0.001) CYCLE ! go to next back surface since inside of this surface
			// cannot be in sun if the outside can be

			// Transform coordinates of back surface from general system to the
			// plane of the receiving surface

			CTRANS( BackSurfaceNumber, NGRS, NVT, XVT, YVT, ZVT );

			// Project "shadow" from back surface along sun's rays to receiving surface.  Back surface vertices
			// become clockwise sequential.

			for ( N = 1; N <= NVT; ++N ) {
				XVS( N ) = XVT( N ) - XShadowProjection * ZVT( N );
				YVS( N ) = YVT( N ) - YShadowProjection * ZVT( N );
			}

			// Transform to the homogeneous coordinate system.

			NS3 = LOCHCA + 1;
			HCT( NS3 ) = 0.0;
			HTRANS1( NS3, NVT );

			// Adjust near-duplicate points.

			NVR = HCNV( 1 );
			auto l3( HCX.index( NS3, 1 ) );
			for ( N = 1; N <= NVT; ++N, ++l3 ) {
				auto const x3( HCX[ l3 ] ); // [ l3 ] == ( NS3, N )
				auto const y3( HCY[ l3 ] );
				size_type l1( 0 );
				for ( M = 1; M <= NVR; ++M, ++l1 ) {
					if ( std::abs( HCX[ l1 ] - x3 ) > 6 ) continue; // [ l1 ] == ( 1, M )
					if ( std::abs( HCY[ l1 ] - y3 ) > 6 ) continue;
					HCX[ l3 ] = HCX[ l1 ];
					HCY[ l3 ] = HCY[ l1 ];
					break;
				}
			}

			HTRANS0( NS3, NVT );

			// Determine area of overlap of projected back surface and receiving surface.

			NS1 = 1;
			NS2 = NS3;
			HCT( NS3 ) = 1.0;
			DeterminePolygonOverlap( NS1, NS2, NS3 );

			if ( OverlapStatus == NoOverlap ) continue; // to next back surface
			if ( ( OverlapStatus == TooManyVertices ) || ( OverlapStatus == TooManyFigures ) ) break; // back surfaces DO loop

			// Increment back surface count.

			LOCHCA = NS3;
			HCNS( LOCHCA ) = BackSurfaceNumber;
			HCAREA( LOCHCA ) = -HCAREA( LOCHCA );
			NBKSHC = LOCHCA - FBKSHC + 1;

		}

	}

	void
	SHDGSS(
		int const NGRS,
		int const iHour, // Hour Counter
		int const TS, // TimeStep
		int const CurSurf, // Current Surface
		int const NGSS, // Number of general shadowing surfaces
		int const HTS // Heat transfer surface number of the general receiving surf
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Legacy Code
		//       DATE WRITTEN
		//       MODIFIED       na
		//       RE-ENGINEERED  Lawrie, Oct 2000

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine determines the shadows on a general receiving surface.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// BLAST/IBLAST code, original author George Walton

		// Using/Aliasing
		using ScheduleManager::LookUpScheduleValue;
		using ScheduleManager::GetCurrentScheduleValue;
		using ScheduleManager::GetScheduleMinValue;
		using ScheduleManager::GetScheduleName;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		typedef  Array2D< Int64 >::size_type  size_type;
		int GSSNR; // General shadowing surface number
		int MainOverlapStatus; // Overlap status of the main overlap calculation not the check for
		// multiple overlaps (unless there was an error)
		static Array1D< Real64 > XVT;
		static Array1D< Real64 > YVT;
		static Array1D< Real64 > ZVT;
		static bool OneTimeFlag( true );
		int NS1; // Number of the figure being overlapped
		int NS2; // Number of the figure doing overlapping
		int NS3; // Location to place results of overlap
		Real64 SchValue; // Value for Schedule of shading transmittence

		if ( OneTimeFlag ) {
			XVT.dimension( MaxVerticesPerSurface + 1, 0.0 );
			YVT.dimension( MaxVerticesPerSurface + 1, 0.0 );
			ZVT.dimension( MaxVerticesPerSurface + 1, 0.0 );
			OneTimeFlag = false;
		}

		FGSSHC = LOCHCA + 1;
		MainOverlapStatus = NoOverlap; // Set to ensure that the value from the last surface is not saved
		OverlapStatus = NoOverlap;

		if ( NGSS <= 0 ) { // IF NO S.S., receiving surface FULLY SUNLIT.

			SAREA( HTS ) = HCAREA( 1 ); // Surface fully sunlit

		} else {

			int ExitLoopStatus( -1 );
			auto const & GenSurf( ShadowComb( CurSurf ).GenSurf );
			auto const sunIsUp( SunIsUpValue );
			for ( int I = 1; I <= NGSS; ++I ) { // Loop through all shadowing surfaces...

				GSSNR = GenSurf( I );

				if ( CTHETA( GSSNR ) > sunIsUp ) continue; //.001) CYCLE ! NO SHADOW IF GSS IN SUNLIGHT.

				auto const & surface( Surface( GSSNR ) );
				bool const notHeatTransSurf( ! surface.HeatTransSurf );

				//     This used to check to see if the shadowing surface was not opaque (within the scheduled dates of
				//            transmittance value.  Perhaps it ignored it if it were outside the range.  (if so, was an error)
				//     The proper action seems to be delete this statement all together, but there would also be no shading if
				//            the shading surface were transparent...
				//---former stmt      IF ((.NOT.Surface(GSSNR)%HeatTransSurf) .AND. &
				//---former stmt            GetCurrentScheduleValue(Surface(GSSNR)%SchedShadowSurfIndex,IHOUR) == 0.0) CYCLE

				if ( notHeatTransSurf ) {
					if ( surface.IsTransparent ) continue; // No shadow if shading surface is transparent
					if ( surface.SchedShadowSurfIndex > 0 ) {
						if ( LookUpScheduleValue( surface.SchedShadowSurfIndex, iHour ) == 1.0 ) continue;
						if ( ! CalcSkyDifShading ) {
							if ( LookUpScheduleValue( surface.SchedShadowSurfIndex, iHour, TS ) == 1.0 ) continue;
						}
					}
				}

				//      IF ((.NOT.Surface(GSSNR)%HeatTransSurf) .AND. &
				//            GetCurrentScheduleValue(Surface(GSSNR)%SchedShadowSurfIndex) == 1.0) CYCLE

				// Transform shadow casting surface from cartesian to homogeneous coordinates according to surface type.

				if ( ( notHeatTransSurf ) && ( surface.BaseSurf != 0 ) ) {

					// For shadowing subsurface coordinates of shadow casting surface are relative to the receiving surface
					// project shadow to the receiving surface

					NVS = surface.Sides;
					auto const & XV( ShadeV( GSSNR ).XV );
					auto const & YV( ShadeV( GSSNR ).YV );
					auto const & ZV( ShadeV( GSSNR ).ZV );
					for ( int N = 1; N <= NVS; ++N ) {
						XVS( N ) = XV( N ) - XShadowProjection * ZV( N );
						YVS( N ) = YV( N ) - YShadowProjection * ZV( N );
					}

				} else {
					// Transform coordinates of shadow casting surface from general system to the system relative to the receiving surface
					int NVT;
					CTRANS( GSSNR, NGRS, NVT, XVT, YVT, ZVT );
					CLIP( NVT, XVT, YVT, ZVT ); // Clip portions of the shadow casting surface which are behind the receiving surface

					if ( NumVertInShadowOrClippedSurface <= 2 ) continue;

					// Project shadow from shadow casting surface along sun's rays to receiving surface Shadow vertices
					// become clockwise sequential

					for ( int N = 1; N <= NumVertInShadowOrClippedSurface; ++N ) {
						XVS( N ) = XVC( N ) - XShadowProjection * ZVC( N );
						YVS( N ) = YVC( N ) - YShadowProjection * ZVC( N );
					}

				}

				// Transform to the homogeneous coordinate system.

				NS3 = LOCHCA + 1;
				HTRANS1( NS3, NVS );

				// Adjust near-duplicate points.

				assert( equal_dimensions( HCX, HCY ) );
				assert( HCX.index( 1, 1 ) == 0u );
				size_type j( HCX.index( NS3, 1 ) );
				size_type NVR( HCNV( 1 ) );
				for ( int N = 1; N <= NumVertInShadowOrClippedSurface; ++N, ++j ) { //Tuned Logic change: break after 1st "close" point found
					auto const HCX_N( HCX[ j ] ); // [ j ] == ( NS3, N )
					auto const HCY_N( HCY[ j ] );
					for ( size_type l = 0; l < NVR; ++l ) { // [ l ] == ( 1, l+1 )
						auto const delX( std::abs( HCX[ l ] - HCX_N ) );
						if ( delX > 6 ) continue;
						auto const delY( std::abs( HCY[ l ] - HCY_N ) );
						if ( delY > 6 ) continue;
						if ( delX > 0 ) HCX[ j ] = HCX[ l ]; // [ j ] == ( NS3, N )
						if ( delY > 0 ) HCY[ j ] = HCY[ l ];
						break;
					}
				}
				HTRANS0( NS3, NumVertInShadowOrClippedSurface );
				if ( ! CalcSkyDifShading ) {
					if ( iHour != 0 ) {
						SchValue = LookUpScheduleValue( surface.SchedShadowSurfIndex, iHour, TS );
					} else {
						SchValue = surface.SchedMinValue;
					}
				} else {
					SchValue = surface.SchedMinValue;
				}

				HCT( NS3 ) = SchValue;

				// Determine overlap of shadow with receiving surface

				CurrentShadowingSurface = I;
				CurrentSurfaceBeingShadowed = GSSNR;
				NS1 = 1;
				NS2 = NS3;
				DeterminePolygonOverlap( NS1, NS2, NS3 );
				//  Next statement is special to deal with transmitting shading devices
				if ( OverlapStatus == FirstSurfWithinSecond && SchValue > 0.0 ) OverlapStatus = PartialOverlap;
				MainOverlapStatus = OverlapStatus;
				ExitLoopStatus = MainOverlapStatus;

				if ( MainOverlapStatus == NoOverlap ) { // No overlap of general surface shadow and receiving surface
					// Continue
				} else if ( ( MainOverlapStatus == FirstSurfWithinSecond ) || ( MainOverlapStatus == TooManyVertices ) || ( MainOverlapStatus == TooManyFigures ) ) {
					goto ShadowingSurfaces_exit;
				} else if ( ( MainOverlapStatus == SecondSurfWithinFirst ) || ( MainOverlapStatus == PartialOverlap ) ) {
					// Determine overlaps with previous shadows.
					LOCHCA = NS3;
					NGSSHC = LOCHCA - FGSSHC + 1;
					if ( NGSSHC > 1 ) MULTOL( LOCHCA, FGSSHC - 1, NGSSHC - 1 ); // HOYT - Remove this call
				} else {
					goto ShadowingSurfaces_exit;
				}

				ExitLoopStatus = -1;
			}
			ShadowingSurfaces_exit: ;

			// Compute sunlit area of surface (excluding effects of subsurfs).

			if ( ExitLoopStatus == FirstSurfWithinSecond ) { // Surface fully shaded
				SAREA( HTS ) = 0.0;
				LOCHCA = FGSSHC;

			} else if ( ( ExitLoopStatus == TooManyVertices ) || ( ExitLoopStatus == TooManyFigures ) ) { // Array limits exceeded, estimate
				SAREA( HTS ) = 0.25 * HCAREA( 1 );

			} else {

				// Compute the sunlit area here.
				// Call UnionShadow(FGSSHC,LOCHCA)

				NGSSHC = LOCHCA - FGSSHC + 1;
				if ( NGSSHC <= 0 ) {
					SAREA( HTS ) = HCAREA( 1 ); // Surface fully sunlit
				} else {
					Real64 A( HCAREA( 1 ) ); // Area
					for ( int i = FGSSHC, e = FGSSHC + NGSSHC - 1; i <= e; ++i ) {
						A += HCAREA( i ) * ( 1.0 - HCT( i ) );
					}
					SAREA( HTS ) = A;
					if ( SAREA( HTS ) <= 0.0 ) { // Surface fully shaded
						SAREA( HTS ) = 0.0;
						LOCHCA = FGSSHC;
					}
				}

			}

		}

		NGSSHC = LOCHCA - FGSSHC + 1;

	}

	void
	CalcInteriorSolarOverlaps(
		int const iHour, // Hour Index
		int const NBKS, // Number of back surfaces associated with this GRSNR (in general, only
		int const HTSS, // Surface number of the subsurface (exterior window)
		int const GRSNR, // General receiving surface number (base surface of the exterior window)
		int const TS // Time step Index
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   January 1999
		//       MODIFIED       Nov 2001, FW: include beam radiation overlaps with
		//                       back windows and doors; previously these subsurfaces ignored.
		//                      May 2002, FW: fix problem where reveal was not being considered
		//                       in calculating overlap areas if window is shaded only by reveal.
		//                      June 2002, FW: fix problem that gave incorrect calculation when
		//                       window is not shaded only by reveal
		//                      June 2002, FW: remove incorrect multiplication of overlap areas
		//                       by sunlit fraction when window is shaded only by reveal
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// For an exterior window with surface number HTSS, determines (1) the surface numbers of back
		// surfaces receiving beam radiation from the window and (2) for each such back surface, the area
		// of the portion of the window sending beam radiation to the back surface; this is called the
		// "overlap area."

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// BLAST/IBLAST code, original author George Walton

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//  some of these will receive beam radiation from HTSS this hour)

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const WindowShadedOnlyByReveal( 2 ); // for use with RevealStatus

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		typedef  Array2D< Int64 >::size_type  size_type;
		int JBKS; // Counter of back surfaces with non-zero overlap with HTSS
		int JBKSbase; // Back base surface counter
		int BackSurfNum; // Back surface number
		Real64 OverlapArea; // Overlap area (m2)

		bool UseSimpleDistribution; // TRUE means simple interior solar distribution
		// (all incoming beam assumed to strike floor),
		// FALSE means exact interior solar distribution
		// (track which back surfaces beam illuminates)

		//Tuned Linear indexing

		assert( equal_dimensions( HCX, HCY ) );
		assert( equal_dimensions( HCX, HCA ) );
		assert( equal_dimensions( HCX, HCB ) );
		assert( equal_dimensions( HCX, HCC ) );

		if ( SAREA( HTSS ) > 0.0 ) {

			UseSimpleDistribution = false;

			if ( ( NBKS <= 0 ) || ( Surface( GRSNR ).ExtBoundCond > 0 ) ) {

				UseSimpleDistribution = true;

			} else {
				// Using 'exact' distribution, replace subsurface HC entries with reveal HC entries
				// so that the reveal HC is used in calculating interior solar overlap areas

				// Adding the following line fixes a problem where, if the window was shaded only
				// by reveal, then the reveal was not considered in calculating interior solar
				// overlap areas (FCW 5/3/02).
				//IF(Surface(HTSS)%Reveal > 0.0) NRVLHC = 1
				// Changing the line to the following avoids incorrect calculation when window is not shaded
				// only by reveal (FCW 6/28/02).
				if ( WindowRevealStatus( TS, iHour, HTSS ) == WindowShadedOnlyByReveal ) NRVLHC = 1;
				if ( NRVLHC > 0 ) {
					for ( int I = 1; I <= NRVLHC; ++I ) {
						int const iS( FSBSHC - 1 + I );
						int const iR( FRVLHC - 1 + I );
						HCT( iS ) = HCT( iR );
						HCNV( iS ) = HCNV( iR );
						HCAREA( iS ) = HCAREA( iR );
						size_type lS( HCX.index( iS, 1 ) );
						size_type lR( HCX.index( iR, 1 ) );
						for ( int J = 1; J <= MaxHCV; ++J, ++lS, ++lR ) { // [ lS ] == ( iS, J ), [ lR ] == ( iR, J )
							HCX[ lS ] = HCX[ lR ];
							HCY[ lS ] = HCY[ lR ];
							HCA[ lS ] = HCA[ lR ];
							HCB[ lS ] = HCB[ lR ];
							HCC[ lS ] = HCC[ lR ];
						}
					}
					NSBSHC = NRVLHC;
				}

			}

			// Check for array space.
			if ( FSBSHC + NBKSHC > MaxHCS ) UseSimpleDistribution = true;

			if ( ! UseSimpleDistribution ) { // Compute overlaps

				FINSHC = FSBSHC + NSBSHC;

				JBKS = 0;
				JBKSbase = 0;

				for ( int IBKS = 1; IBKS <= NBKSHC; ++IBKS ) { // Loop over back surfaces to GRSNR this hour. NBKSHC is the number of
					// back surfaces that would receive beam radiation from the base surface, GRSNR,
					// if the base surface was transparent. In general, some (at least one) or all of these
					// will receive beam radiation from the exterior window subsurface, HTSS, of GRSNR,
					// depending on the size of HTSS and its location on GRSNR

					BackSurfNum = HCNS( FBKSHC - 1 + IBKS );

					// Determine if this back surface number can receive beam radiation from the
					// exterior window, HTSS, this hour, i.e., overlap area is positive

					LOCHCA = FINSHC - 1;

					MULTOL( FBKSHC - 1 + IBKS, FSBSHC - 1, NSBSHC );

					// Compute overlap area for this back surface

					NINSHC = LOCHCA - FINSHC + 1;
					if ( NINSHC <= 0 ) continue;
					OverlapArea = HCAREA( FINSHC );
					for ( int J = 2; J <= NINSHC; ++J ) {
						OverlapArea += HCAREA( FINSHC - 1 + J ) * ( 1.0 - HCT( FINSHC - 1 + J ) );
					}

					if ( OverlapArea > 0.001 ) {
						++JBKS;
						if ( Surface( BackSurfNum ).BaseSurf == BackSurfNum ) JBKSbase = JBKS;
						if ( JBKS <= MaxBkSurf ) {
							BackSurfaces( TS, iHour, JBKS, HTSS ) = BackSurfNum;
							// Remove following IF check: multiplying by sunlit fraction in the following is incorrect
							// (FCW, 6/28/02)
							//IF (WindowRevealStatus(HTSS,IHOUR,TS) == WindowShadedOnlyByReveal) THEN
							//  OverlapArea = OverlapArea*(SAREA(HTSS)/Surface(HTSS)%Area)
							//ENDIF
							OverlapAreas( TS, iHour, JBKS, HTSS ) = OverlapArea * SurfaceWindow( HTSS ).GlazedFrac;
							// If this is a subsurface, subtract its overlap area from the base surface
							if ( Surface( BackSurfNum ).BaseSurf != BackSurfNum && JBKSbase != 0 ) {
								OverlapAreas( TS, iHour, JBKSbase, HTSS ) = max( 0.0, OverlapAreas( TS, iHour, JBKSbase, HTSS ) - OverlapAreas( TS, iHour, JBKS, HTSS ) );
							}
						}
					}

				} // End of loop over back surfaces

			}

		} // End of check that sunlit area > 0.

	}

	void
	CalcInteriorSolarDistribution()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   January 1999
		//       MODIFIED       Nov 1999, FW, for Window5 calculation method
		//                      Oct 2000, FW: add transmitted solar variables for reporting
		//                      Mar 2001, FW: add new calc of solar absorbed by window shades
		//                      May 2001, FW: add calc of solar transmitted and absorbed by window blinds
		//                      Oct 2001, LL: remove interpolation, solar now at time step
		//                      Oct 2001, FW: add solar transmitted through interior windows
		//                      Mar 24, 2001, FW: remove incorrect multiplication of Boverlap by sunlit fraction
		//                                        since effect of shadowing is already included in Aoverlap
		//                      Apr 2001, FW: add effects of beam solar reflection from outside and inside reveals
		//                      Jan 2003, FW: add between-glass shades and blinds
		//                      Dec 2003, FW: report beam incident on inside of surface
		//                      Jan 2004, FW: for blinds with horizontal slats, allow different diffuse/diffuse
		//                                    transmittance for ground and sky solar
		//                      Apr 2004, FW: allow diffusing glazing
		//                      May 2006, RR: allow external window screen
		//                      Jan 2010, TH: add calculating and reporting of WinBmBmSolar, WinBmDifSolar,
		//                                    WinBmBmSolarEnergy, and WinBmDifSolarEnergy
		//                      Jun 2013, SV: scheduled surface gains for walls and windows
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// For a time step, calculates solar radiation absorbed by exterior
		// surfaces and interior solar radiation distribution

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using ScheduleManager::GetCurrentScheduleValue;
		using General::POLYF;
		using General::InterpSw;
		using General::InterpBlind;
		using General::InterpSlatAng;
		using General::InterpProfSlatAng;
		using General::BlindBeamBeamTrans;
		using General::InterpProfAng;
		using namespace DataDaylightingDevices;
		using DaylightingDevices::FindTDDPipe;
		using DaylightingDevices::TransTDD;
		using WindowEquivalentLayer::CalcEQLOpticalProperty;
		using WindowEquivalentLayer::CFSDiffAbsTrans;
		using namespace DataWindowEquivalentLayer;

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
		int ZoneNum; // Zone number
		int SurfNum; // Surface number
		int SurfNum2; // Secondary surface number for tubular daylighting device diffuser (TDD:DIFFUSER)
		int PipeNum; // TDD pipe object number
		int ShelfNum; // Daylighting shelf object number
		int InShelfSurf; // Inside daylighting shelf surface number
		int OutShelfSurf; // Outside daylighting shelf surface number
		Real64 ShelfSolarRad; // Shelf diffuse solar radiation
		int BackSurfNum; // Back surface number
		int IBack; // Back surface counter
		int FloorNum; // Floor surface number
		int AdjSurfNum; // Adjacent surface number
		int AdjZoneNum; // Adjacent zone number
		Real64 CosTlt; // Cosine of surface tilt angle
		int ConstrNum; // Construction number
		int ConstrNumSh; // Shaded construction number
		int ConstrNumBack; // Construction number of back surface
		int ConstrNumBackSh; // Shaded construction number of back surface
		int FlConstrNum; // Construction number of floor surface
		int ShadeFlag; // Shading flag for a window
		int ShadeFlagBack; // Shading flag for a window that is a back surface
		int Lay; // Glass layer number
		Real64 SwitchFac; // Switching factor for a window
		Real64 SwitchFacBack; // Switching factor for a window that is a back surface
		Real64 TransBeamWin; // Beam solar transmittance of a window
		Real64 TransBeamWinSh; // Beam solar transmittance of a shaded window
		static Array1D< Real64 > AbsBeamWin; // Glass layer beam solar absorptance of a window
		Real64 AbsBeamWinSh; // Glass layer beam solar absorptance of a shaded window
		static Array1D< Real64 > AbsBeamWinEQL( CFSMAXNL+1 ); // layers beam solar absorptance of a window
		Real64 AbsBeamTotWin; // Sum of window glass layer beam solar absorptances
		Real64 ProfAng; // Window solar profile angle (radians)
		Real64 ProfAngBack; // Back window solar profile angle (radians)
		int BlNum; // Blind number
		int ScNum; // Screen number
		int BlNumBack; // Back surface blind number
		int ScNumBack; // Back surface screen number
		Real64 TBmBm; // Beam-beam solar transmittance for bare window or window with switchable glazing
		Real64 TBmDif; // Beam-diffuse solar transmittance for bare window with diffusing glass
		Real64 TBlBmDif; // Beam-diffuse solar transmittance of blind
		Real64 TScBmDif; // Beam-diffuse solar transmittance of screen
		Real64 TBlDifDif; // Diffuse-diffuse solar transmittance of blind
		Real64 TScDifDif; // Diffuse-diffuse solar transmittance of screen
		Real64 RhoBlBmDifFr; // Beam-diffuse front reflectance of blind
		Real64 RhoBlBmDifBk; // Beam-diffuse back reflectance of blind
		Real64 RScBmDifBk; // Beam-diffuse back reflectance of blind
		Real64 RhoBlDifDifFr; // Diffuse-diffuse front refectance of blind
		Real64 RhoBlDifDifBk; // Diffuse-diffuse back refectance of blind
		Real64 RScDifDifBk; // Diffuse-diffuse back refectance of screen
		Real64 RGlBmFr; // Beam front reflectance of glass
		Real64 RGlDifFr; // Diffuse front reflectance of glass
		Real64 RGlDifBk; // Diffuse back reflectance of glass
		Real64 TBmBmBl; // Beam-beam transmittance for window with blind
		Real64 TBmBmSc; // Beam-beam transmittance for window with screen
		Real64 TBmAllShBlSc; // Beam-beam + beam-diffuse transmittance for window with shade, blind, screen,
		// or switchable glazing
		Real64 TBmAll; // Window beam-to-(beam+diffuse) transmittance
		Real64 TBm; // Window beam-beam transmittance
		Real64 DifSolarInc; // Exterior diffuse solar incident on window (W/m2)
		Real64 SkySolarTrans; // Exterior diffuse sky solar transmitted by TDD (W/m2)
		Real64 GndSolarTrans; // Exterior diffuse ground solar transmitted by TDD (W/m2)
		Real64 TDifBare; // Bare diffuse transmittance of exterior window
		Real64 TGlDif; // Bare diffuse transmittance of back window
		Real64 TGlBm; // Glazing system front solar beam transmittance
		Real64 TGlBmBack; // Glazing system back solar beam transmittance
		Real64 AGlDiffBack; // Glass layer back diffuse solar absorptance
		Real64 RGlDiffBack; // Glazing system back diffuse solar reflectance
		Real64 AGlDiffFront; // Glass layer front diffuse solar absorptance
		Real64 RGlDiffFront; // Glazing system front diffuse solar reflectance
		Real64 RhoBlFront; // Blind solar front beam reflectance
		Real64 RhoBlBack; // Blind solar back beam-diffuse reflectance
		Real64 RScBack; // Screen solar back beam-diffuse reflectance
		Real64 RScDifBack; // Screen solar back diffuse-diffuse reflectance
		Real64 AbsBlFront; // Blind solar front beam absorptance
		Real64 AbsScBeam; // Screen solar beam absorptance
		Real64 AbsBlBack; // Blind solar back beam absorptance
		Real64 AbsScBack; // Screen solar back beam absorptance
		Real64 AbsBlDiffFront; // Blind solar front diffuse absorptance
		Real64 AbsBlDiffBack; // Blind solar back diffuse absorptance
		Real64 AbsScDiffBack; // Screen solar back diffuse absorptance
		Real64 ABlBack; // Blind solar back absorptance for interior solar
		Real64 AScBack; // Screen solar back absorptance for interior solar
		Real64 TrSh; // Shade material solar transmittance
		Real64 AbsSh; // Shade material solar absorptance
		Real64 RhoSh; // Shade material solar reflectance
		Real64 AShBack; // System shade absorptance for interior beam solar
		Real64 TBlBmBm; // Blind solar front beam-beam transmittance
		Real64 TScBmBm; // Screen solar front beam-beam transmittance
		Real64 TBlBmBmBack; // Blind solar back beam-beam transmittance
		Real64 TScBmBmBack; // Screen solar back beam-beam transmittance
		Real64 TBlBmDiff; // Blind solar front beam-diffuse transmittance
		Real64 TScBmDiff; // Screen solar front beam-diffuse transmittance
		Real64 TBlBmDiffBack; // Blind solar back beam-diffuse transmittance
		Real64 TScBmDiffBack; // Screen solar back beam-diffuse transmittance
		Real64 RhoBlDiffFront; // Blind solar front diffuse reflectance
		Real64 RhoBlDiffBack; // Blind solar back diffuse reflectance
		Real64 RScDiffBack; // Screen solar back diffuse reflectance
		Real64 RGlFront; // Glazing system solar front beam-beam reflectance
		Real64 RGlBack; // Glazing system solar back beam-beam reflectance
		Real64 BTOTWinZone; // Transmitted beam solar factor for a window
		Real64 BTOTZone; // (Solar entering a zone as beam or diffuse radiation, originating as beam solar
		//  incident on exterior windows)/(Beam normal solar) [W/(W/m2)]
		Real64 BTOTZoneSSG; // Solar entering a zone in case of scheduled surface gains
		Real64 AbWin; // Factor for front beam radiation absorbed in window glass layer
		Real64 AbWinBack; // Factor for back beam radiation absorbed in window glass layer
		Real64 AbWinSh; // Like AbWin, but for shaded window
		Real64 AbWinEQL; // Factor for front beam radiation absorbed for equivalent layer window model
		//Array1D< Real64 > AdWinEQL( CFSMAXNL+1 ); // Factor for front diffuse radiation absorbed for equivalent layer window model //Unused
		Real64 BABSZone; // Beam radiation from exterior windows absorbed in a zone or transmitted through
		Real64 BABSZoneSSG; // Beam radiation from exterior windows absorbed in a zone (only for scheduled surface gains)
		Real64 AOverlap; // Back surface area irradiated by beam solar from an exterior window,
		//   projected onto window plane
		Real64 BOverlap; // AOverlap multiplied by exterior window beam transmittance
		// and cosine of incidence angle
		Real64 AbsScreen; // Exterior screen beam solar absorptance
		Real64 AbsShade; // Interior shade or blind beam solar absorptance
		Real64 AbsShadeDiff; // Interior shade or blind diffuse solar absorptance
		Real64 DSZoneWin; // Factor for sky diffuse solar gain into a zone from an exterior window
		Real64 DSZoneWinSh; // Factor for sky diffuse solar gain into a zone from a shaded exterior window
		Real64 DGZoneWin; // Factor for ground diffuse solar gain into a zone
		Real64 DGZoneWinSh; // Factor for ground diffuse solar gain into a zone from a shaded exterior window
		Real64 HMovInsul; // Conductance of movable wall insulation
		Real64 AbsIntSurf; // Interior solar absorptance of opaque surface
		Real64 AbsInt;

		Real64 MovInsulSchedVal; // Value of the movable insulation schedule for current time
		Real64 FracSunLit; // Effective fraction of window that is sunlit;
		//  takes shadowing effects of frame and divider into account
		Real64 SunLitFract; // Sunlit fraction w/o shadowing effects of frame and divider
		Real64 InOutProjSLFracMult; // = SurfaceWindow(SurfNum)%InOutProjSLFracMult(HourOfDay)
		Real64 CosInc; // Incidence angle of beam solar radiation on window
		Real64 CosIncBack;
		Real64 SlatAng; // Slat angle this time step for window with blind on (deg)
		Real64 SlatAngBack;
		bool VarSlats; // True if variable slat angle
		bool VarSlatsBack;
		Real64 ADiffWin; // Diffuse solar absorptance of glass layer, bare window
		Real64 ADiffWinSh; // Diffuse solar absorptance of glass layer, window with shading device
		Real64 DiffTrans; // Glazing diffuse solar transmittance (including shade/blind/switching, if present)
		Real64 DiffTransGnd; // Ground diffuse solar transmittance for glazing with blind with horiz. slats or complex fen
		Real64 DiffTransBmGnd; // Complex fen: diffuse solar transmittance for ground-reflected beam radiation
		Real64 DiffTransSky; // Sky diffuse solar transmittance for glazing with blind with horiz. slats or complex fen
		Real64 NomDiffTrans;
		int BaseSurfNum; // Base surface number
		Real64 t1; // Bare-glass beam solar transmittance for glass layers 1,2 and 3
		Real64 t2;
		Real64 t3;
		Real64 t1t2; // t1*t2
		Real64 af1; // Bare-glass beam solar front absorptance for glass layers 1,2 and 3
		Real64 af2;
		Real64 af3;
		Real64 ab1; // Bare-glass beam solar back absorptance for glass layers 1,2 and 3
		Real64 ab2;
		Real64 ab3;
		Real64 rf1; // Bare-glass beam solar front reflectance for glass layers 1,2 and 3
		Real64 rf2;
		Real64 rf3;
		Real64 rb1; // Bare-glass beam solar back reflectance for glass layers 1,2 and 3
		Real64 rb2;
		Real64 rb3;
		Real64 td1; // Bare-glass diffuse solar transmittance for glass layers 1,2 and 3
		Real64 td2;
		Real64 td3;
		Real64 td1td2; // td1*td2
		Real64 afd1; // Bare-glass diffuse solar front absorptance for glass layers 1,2 and 3
		Real64 afd2;
		Real64 afd3;
		Real64 abd1; // Bare-glass diffuse solar back absorptance for glass layers 1,2 and 3
		Real64 abd2;
		Real64 abd3;
		Real64 rfd1; // Bare-glass diffuse solar front reflectance for glass layers 1,2 and 3
		Real64 rfd2;
		Real64 rfd3;
		Real64 rbd1; // Bare-glass diffuse solar back reflectance for glass layers 1,2 and 3
		Real64 rbd2;
		Real64 rbd3;
		Real64 tfshBB; // Bare-blind front and back beam-beam solar transmittance
		Real64 tbshBB;
		Real64 tfshBd; // Bare-blind front and back beam-diffuse solar transmittance
		Real64 tbshBd;
		Real64 tfshd; // Bare-blind front and back diffuse-diffuse solar transmittance
		Real64 tbshd;
		Real64 afshB; // Bare-blind front and back beam solar absorptance
		Real64 abshB;
		Real64 afshd; // Bare-blind front and back diffuse solar absorptance
		Real64 abshd;
		Real64 rfshB; // Bare-blind front and back beam solar reflectance
		Real64 rbshB;
		Real64 rfshd; // Bare-blind front and back diffuse solar reflectance
		Real64 rbshd;
		Real64 t1k; // Back surface bare-glass beam solar transmittance for glass layers 1,2,3
		Real64 t2k;
		Real64 t3k;
		Real64 af2k; // Back surface bare-glass beam solar front absorptance for glass layers 2 and 3
		Real64 af3k;
		Real64 ab1k; // Back surface bare-glass beam solar back absorptance for glass layers 1,2 and 3
		Real64 ab2k;
		Real64 ab3k;
		Real64 rb1k; // Back surface bare-glass beam solar back reflectance for glass layers 1,2
		Real64 rb2k;
		Real64 td1k; // Back surface bare-glass beam diffuse solar transmittance for glass layers 1,2
		Real64 td2k;
		Real64 afd2k; // Back surface bare-glass diffuse solar front absorptance for glass layer 2 and 3
		Real64 afd3k;
		Real64 abd1k; // Back surface bare-glass diffuse solar back absorptance for glass layer 1 and 2
		Real64 abd2k;
		Real64 rfd2k; // Back surface bare-glass diffuse solar front reflectance for glass layer 2 and 3
		Real64 rfd3k;
		Real64 rbd1k; // Back surface bare-glass diffuse solar back reflectance for glass layer 1 and 2
		Real64 rbd2k;
		Real64 tfshBBk; // Back surface bare-blind beam-beam solar front and back transmittance
		Real64 tbshBBk;
		Real64 tfshBdk; // Back surface bare-blind beam-diffuse solar front and back transmittance
		Real64 tbshBdk;
		Real64 tfshdk; // Back surface bare-blind diffuse-diffuse solar front and back transmittance
		Real64 tbshdk;
		Real64 rfshBk; // Back surface bare-blind beam solar front, back reflectance
		Real64 rbshBk;
		Real64 rfshdk; // Back surface bare-blind diffuse solar front, back reflectance
		Real64 rbshdk;
		Real64 afshBk; // Back surface bare-blind beam solar front, back absorptance
		Real64 abshBk;
		Real64 afshdk; // Back surface bare-blind diffuse solar front, back absorptance
		Real64 abshdk;
		int NGlass; // Number of glass layers in a construction
		int NBackGlass; // Number of glass layers in the "back" construction
		Real64 SkySolarInc; // Incident solar radiation on a window: sky diffuse plus beam
		//   reflected from obstruction (W/m2)
		Real64 GndSolarInc; // Incident solar radiation on a window from the ground (W/m2)

		static Array1D< Real64 > ExtBeamAbsByShadFac; // Factor for exterior beam radiation absorbed by shade
		// (1/m2) (absorbed radation = beam incident * ExtBeamAbsByShad
		static Array1D< Real64 > IntBeamAbsByShadFac; // Like ExtBeamAbsByShadFac, but for interior beam radiation.
		static Array1D< Real64 > WinTransBmSolar; // Factor for exterior beam solar transmitted through window,
		// or window plus shade, into zone at current time (m2)
		static Array1D< Real64 > WinTransDifSolar; // Factor for exterior diffuse solar transmitted through window,
		// or window plus shade, into zone at current time (m2)

		static Array1D< Real64 > WinTransDifSolarGnd; // Factor for exterior ground diffuse solar transmitted through
		// window with horizontally-slatted blind into zone at current time (m2)
		static Array1D< Real64 > WinTransDifSolarSky; // Factor for exterior sky diffuse solar transmitted through
		// window with horizontally-slatted blind into zone at current time (m2)
		/////////// hoisted into namespace renamed to ////////////
		//static bool MustAlloc( true ); // True when local arrays must be allocated
		////////////////////////
		Real64 TBmDenom; // TBmDenominator

		Real64 TBmBmShBlSc; // Beam-beam transmittance for window with shade, blind, screen, or switchable glazing
		Real64 TBmDifShBlSc; // Beam-diffuse transmittance for window with shade, blind, screen, or switchable glazing
		Real64 WinTransBmBmSolar; // Factor for exterior beam to beam solar transmitted through window,
		//  or window plus shade, into zone at current time (m2)
		Real64 WinTransBmDifSolar; // Factor for exterior beam to diffuse solar transmitted through window,
		//  or window plus shade, into zone at current time (m2)

		Real64 TBmBmEQL; // Beam-beam solar transmittance for equivalent layer model window W/WO shade
		Real64 TBmDiffEQL; // Beam-diffuse solar transmittance for equivalent layer model window W/WO shade

		// Variables for complex fenestration
		int CurCplxFenState; // Current state for complex fenestration
		int CurBackState; // Current state for back surface if that surface is complex fenestration
		int CurTrnDir; // Current back window surface BSDF direction
		int CurBackDir; // current hit direction to complex fenestration
		int IBm; // Incoming direction of the Sun (for window BSDF)
		int IConst; // Current surface construction number (it depends of state too)
		int NBkSurf; // Number of back surfaces
		int BaseSurf; // Base surface number for current complex window
		int BackSurfaceNumber; // Back surface number
		Array1D< Real64 > CFBoverlap; // Sum of boverlap for each back surface
		Array2D< Real64 > CFDirBoverlap; // Directional boverlap (Direction, IBack)
		Real64 CurLambda; // Current lambda value in BSDF outgoing directions
		Real64 DirTrans; // Current BSDF directional transmittance
		// (for incoming I and outgoing J directions)
		Real64 bestDot; // complex fenestration hits other complex fenestration, it is important to find
		// matching beam directions.  Beam leving one window will have certaing number for it's basis
		// while same beam reaching back surface will have different beam number.  This value is used
		// to keep best matching dot product for those directions
		Real64 curDot; // temporary variable for current dot product
		int bestTrn; // Direction corresponding best dot product for master window
		int bestBackTrn; // Direction corresponding best dot product for back surface window
		int TotSolidLay; // Number of window solid layers

		static Array2D< Real64 > AbsSolBeamEQL( 2, CFSMAXNL+1 ); // absorbed exterior beam radiation by layers fraction
		static Array2D< Real64 > AbsSolDiffEQL( 2, CFSMAXNL+1 ); // absorbed exterior diffuse radiation by layers fraction
		int EQLNum; // equivalent layer fenestration index
		static Array2D< Real64 > AbsSolBeamBackEQL( 2, CFSMAXNL+1 ); // absorbed interior beam radiation by layers fraction from back
		//Array2D< Real64 > AbsSolDiffBackEQL( CFSMAXNL+1, 2 ); // absorbed exterior diffuse radiation by layers fraction from back //Unused

		// scheduled surface gains local variables
		int FenSolAbsPtr;
		int SurfSolIncPtr;
		int iSSG; // scheduled surface gains counter
		Real64 SolarIntoZone; // Solar radiation into zone to current surface

		if ( MustAllocSolarShading ) {
			DBZoneIntWin.allocate( NumOfZones );
			IntBeamAbsByShadFac.allocate( TotSurfaces );
			ExtBeamAbsByShadFac.allocate( TotSurfaces );
			WinTransBmSolar.allocate( TotSurfaces );
			WinTransDifSolar.allocate( TotSurfaces );
			WinTransDifSolarGnd.allocate( TotSurfaces );
			WinTransDifSolarSky.allocate( TotSurfaces );
			MustAllocSolarShading = false;
		}

#ifdef EP_Count_Calls
		++NumIntSolarDist_Calls;
#endif

		DSZone = 0.0;
		DGZone = 0.0;
		DBZone = 0.0;
		DBZoneSSG = 0.0;
		DBZoneIntWin = 0.0;
		AISurf = 0.0;
		AOSurf = 0.0;
		AbWin = 0.0;
		AbWinSh = 0.0;
		AWinSurf = 0.0;
		WinTransBmSolar = 0.0;
		WinTransDifSolar = 0.0;
		WinTransDifSolarGnd = 0.0;
		WinTransDifSolarSky = 0.0;
		WinBmSolar = 0.0;
		WinBmBmSolar = 0.0;
		WinBmDifSolar = 0.0;
		WinTransBmBmSolar = 0.0;
		WinTransBmDifSolar = 0.0;
		TBmBm = 0.0;
		TBmDif = 0.0;
		TBmBmEQL = 0.0;
		TBmDiffEQL = 0.0;

		WinDifSolar = 0.0;
		ZoneTransSolar = 0.0;
		ZoneBmSolFrExtWinsRep = 0.0;
		ZoneBmSolFrIntWinsRep = 0.0;
		ZoneDifSolFrExtWinsRep = 0.0;
		ZoneDifSolFrIntWinsRep = 0.0;
		IntBeamAbsByShadFac = 0.0;
		ExtBeamAbsByShadFac = 0.0;
		//energy
		WinBmSolarEnergy = 0.0;
		WinBmBmSolarEnergy = 0.0;
		WinBmDifSolarEnergy = 0.0;

		WinDifSolarEnergy = 0.0;
		ZoneTransSolarEnergy = 0.0;
		ZoneBmSolFrExtWinsRepEnergy = 0.0;
		ZoneBmSolFrIntWinsRepEnergy = 0.0;
		ZoneDifSolFrExtWinsRepEnergy = 0.0;
		ZoneDifSolFrIntWinsRepEnergy = 0.0;

		for ( auto & window : SurfaceWindow ) {
			window.BmSolTransThruIntWinRep = 0.0;
			window.BmSolTransThruIntWinRepEnergy = 0.0;
		}

		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {

			BTOTZone = 0.0;
			BABSZone = 0.0;

			// Loop over exterior surfaces in this zone

			for ( SurfNum = Zone( ZoneNum ).SurfaceFirst; SurfNum <= Zone( ZoneNum ).SurfaceLast; ++SurfNum ) {
				if ( ( ( Surface( SurfNum ).ExtBoundCond != ExternalEnvironment ) && ( Surface( SurfNum ).ExtBoundCond != OtherSideCondModeledExt ) ) && SurfaceWindow( SurfNum ).OriginalClass != SurfaceClass_TDD_Diffuser ) continue;
				if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
				// TH added 3/24/2010 while debugging CR 7872
				if ( ! Surface( SurfNum ).ExtSolar ) continue;
				ConstrNum = Surface( SurfNum ).Construction;
				ConstrNumSh = SurfaceWindow( SurfNum ).ShadedConstruction;
				if ( SurfaceWindow( SurfNum ).StormWinFlag == 1 ) {
					ConstrNum = Surface( SurfNum ).StormWinConstruction;
					ConstrNumSh = Surface( SurfNum ).StormWinShadedConstruction;
				}
				BlNum = SurfaceWindow( SurfNum ).BlindNumber;
				ScNum = SurfaceWindow( SurfNum ).ScreenNumber;
				ShadeFlag = SurfaceWindow( SurfNum ).ShadingFlag; // Set in subr. WindowShadingManager
				ProfAng = 0.0;
				if ( ShadeFlag != ExtScreenOn && BlNum > 0 ) ProfileAngle( SurfNum, SOLCOS, Blind( BlNum ).SlatOrientation, ProfAng );
				SlatAng = SurfaceWindow( SurfNum ).SlatAngThisTS;
				VarSlats = SurfaceWindow( SurfNum ).MovableSlats;

				if ( SurfaceWindow( SurfNum ).OriginalClass == SurfaceClass_TDD_Diffuser ) {
					PipeNum = FindTDDPipe( SurfNum );
					SurfNum2 = TDDPipe( PipeNum ).Dome;
				} else {
					SurfNum2 = SurfNum;
				}

				ShelfNum = Surface( SurfNum ).Shelf;
				if ( ShelfNum > 0 ) { // Daylighting shelf
					InShelfSurf = Shelf( ShelfNum ).InSurf;
					OutShelfSurf = Shelf( ShelfNum ).OutSurf;
				} else {
					InShelfSurf = 0;
					OutShelfSurf = 0;
				}

				CosInc = CosIncAng( TimeStep, HourOfDay, SurfNum2 );
				SunLitFract = SunlitFrac( TimeStep, HourOfDay, SurfNum2 );

				//-------------------------------------------------------------------------
				// EXTERIOR BEAM SOLAR RADIATION ABSORBED ON THE OUTSIDE OF OPAQUE SURFACES
				//-------------------------------------------------------------------------

				if ( SunLitFract > 0.0 && Construct( ConstrNum ).TransDiff <= 0.0 ) {
					AOSurf( SurfNum ) = Construct( ConstrNum ).OutsideAbsorpSolar * CosInc * SunLitFract;

					// Note: movable insulation, if present, is accounted for in subr. InitIntSolarDistribution,
					// where QRadSWOutMvIns is calculated from QRadSWOutAbs and insulation solar absorptance

				}

				//-------------------------------------------------------------------------------------------
				// EXTERIOR BEAM AND DIFFUSE SOLAR RADIATION ABSORBED IN THE GLASS LAYERS OF EXTERIOR WINDOWS
				//-------------------------------------------------------------------------------------------

				if ( Surface( SurfNum ).Class != SurfaceClass_Window && Surface( SurfNum ).Class != SurfaceClass_TDD_Dome ) continue;

				// Somewhat of a kludge
				if ( Surface( SurfNum ).Class == SurfaceClass_TDD_Dome || SurfaceWindow( SurfNum ).OriginalClass == SurfaceClass_TDD_Diffuser ) SunlitFracWithoutReveal( TimeStep, HourOfDay, SurfNum ) = SunLitFract; // Frames/dividers not allowed

				WinTransBmBmSolar = 0.0;
				WinTransBmDifSolar = 0.0;

				InOutProjSLFracMult = SurfaceWindow( SurfNum ).InOutProjSLFracMult( HourOfDay );
				if ( SunlitFracWithoutReveal( TimeStep, HourOfDay, SurfNum ) > 0.0 ) {

					if ( SurfaceWindow( SurfNum ).WindowModelType != WindowBSDFModel && SurfaceWindow( SurfNum ).WindowModelType != WindowEQLModel ) {

						// For bare glazing or switchable glazing, the following includes the effects of
						// (1) diffuse solar produced by beam solar incident on the outside and inside reveal
						// surfaces, and (2) absorption of beam solar by outside and inside reveal surfaces.
						// If there is an exterior shade/blind both of these effects are ignored. If there
						// is an interior or between-glass shade/blind the effects of beam incident on
						// inside reveal surfaces is ignored.

						NGlass = Construct( ConstrNum ).TotGlassLayers;

						for ( Lay = 1; Lay <= NGlass; ++Lay ) {
							AbWin = POLYF( CosInc, Construct( ConstrNum ).AbsBeamCoef( {1,6}, Lay ) ) * CosInc * SunLitFract * SurfaceWindow( SurfNum ).OutProjSLFracMult( HourOfDay );
							ADiffWin = Construct( ConstrNum ).AbsDiff( Lay );
							if ( ShadeFlag <= 0 || ShadeFlag >= 10 ) {

								// Bare window (ShadeFlag = -1 or 0 or shading device of off)

								// Add contribution of beam reflected from outside and inside reveal
								AWinSurf( Lay, SurfNum ) = AbWin + SurfaceWindow( SurfNum ).OutsRevealDiffOntoGlazing * Construct( ConstrNum ).AbsDiff( Lay ) + SurfaceWindow( SurfNum ).InsRevealDiffOntoGlazing * Construct( ConstrNum ).AbsDiffBack( Lay );

							} else {

								// Shade, screen, blind or switchable glazing on (ShadeFlag > 0)

								FracSunLit = SunLitFract * SurfaceWindow( SurfNum ).OutProjSLFracMult( HourOfDay );
								if ( ShadeFlag == ExtShadeOn || ShadeFlag == ExtBlindOn || ShadeFlag == ExtScreenOn ) FracSunLit = SunLitFract;
								if ( ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == BGShadeOn || ShadeFlag == SwitchableGlazing ) {

									// Shade or switchable glazing on

									AbWinSh = POLYF( CosInc, Construct( ConstrNumSh ).AbsBeamCoef( {1,6}, Lay ) ) * CosInc * FracSunLit;

									ADiffWinSh = Construct( ConstrNumSh ).AbsDiff( Lay );

								} else {
									// Blind or screen on

									if ( Lay == 1 && ShadeFlag != ExtScreenOn ) ProfileAngle( SurfNum, SOLCOS, Blind( BlNum ).SlatOrientation, ProfAng );

									if ( ShadeFlag == IntBlindOn ) {

										// Interior blind on
										if ( Lay == 1 ) {
											TGlBm = POLYF( CosInc, Construct( ConstrNum ).TransSolBeamCoef );
											RGlDiffBack = Construct( ConstrNum ).ReflectSolDiffBack;
											RhoBlFront = InterpProfSlatAng( ProfAng, SlatAng, VarSlats, Blind( BlNum ).SolFrontBeamDiffRefl );
											RhoBlDiffFront = InterpSlatAng( SlatAng, VarSlats, Blind( BlNum ).SolFrontDiffDiffRefl );
										}
										AGlDiffBack = Construct( ConstrNum ).AbsDiffBack( Lay );
										AbWinSh = AbWin + ( TGlBm * AGlDiffBack * RhoBlFront / ( 1.0 - RhoBlFront * RGlDiffBack ) ) * CosInc * FracSunLit;
										ADiffWinSh = ADiffWin + Construct( ConstrNum ).TransDiff * AGlDiffBack * RhoBlDiffFront / ( 1.0 - RhoBlDiffFront * RGlDiffBack );
									} else if ( ShadeFlag == ExtBlindOn ) {

										// Exterior blind on
										if ( Lay == 1 ) {
											TBlBmBm = BlindBeamBeamTrans( ProfAng, SlatAng, Blind( BlNum ).SlatWidth, Blind( BlNum ).SlatSeparation, Blind( BlNum ).SlatThickness );
											TBlBmDiff = InterpProfSlatAng( ProfAng, SlatAng, VarSlats, Blind( BlNum ).SolFrontBeamDiffTrans );
											RhoBlBack = InterpProfSlatAng( ProfAng, SlatAng, VarSlats, Blind( BlNum ).SolBackBeamDiffRefl );
											RhoBlDiffBack = InterpSlatAng( SlatAng, VarSlats, Blind( BlNum ).SolBackDiffDiffRefl );
											RGlFront = POLYF( CosInc, Construct( ConstrNum ).ReflSolBeamFrontCoef );
											RGlDiffFront = Construct( ConstrNum ).ReflectSolDiffFront;
											TBlDifDif = InterpSlatAng( SlatAng, VarSlats, Blind( BlNum ).SolFrontDiffDiffTrans );
											RGlDifFr = Construct( ConstrNum ).ReflectSolDiffFront;
											RhoBlDifDifBk = InterpSlatAng( SlatAng, VarSlats, Blind( BlNum ).SolBackDiffDiffRefl );
										}
										AGlDiffFront = Construct( ConstrNum ).AbsDiff( Lay );
										AbWinSh = TBlBmBm * AbWin + ( ( TBlBmBm * RGlFront * RhoBlBack + TBlBmDiff ) * AGlDiffFront / ( 1 - RGlDiffFront * RhoBlDiffBack ) ) * CosInc * FracSunLit;
										//ADiffWinSh = 0.0  ! Assumes no contribution from reveal reflection when exterior blind in place
										//  Replaced above line with (FCW, 2/10/03):
										ADiffWinSh = ADiffWin * TBlDifDif / ( 1.0 - RGlDifFr * RhoBlDifDifBk );

									} else if ( ShadeFlag == ExtScreenOn ) {

										// Exterior screen on
										if ( Lay == 1 ) {
											TScBmBm = SurfaceScreens( ScNum ).BmBmTrans;
											TScBmDiff = SurfaceScreens( ScNum ).BmDifTrans;
											RScBack = SurfaceScreens( ScNum ).ReflectSolBeamFront;
											RScDifBack = SurfaceScreens( ScNum ).DifReflect;
											RGlFront = POLYF( CosInc, Construct( ConstrNum ).ReflSolBeamFrontCoef );
											RGlDiffFront = Construct( ConstrNum ).ReflectSolDiffFront;
											TScDifDif = SurfaceScreens( ScNum ).DifDifTrans;
											RGlDifFr = Construct( ConstrNum ).ReflectSolDiffFront;
										}
										AGlDiffFront = Construct( ConstrNum ).AbsDiff( Lay );

										//             Reduce the bare window absorbed beam by the screen beam transmittance and then account for interreflections
										AbWinSh = TScBmBm * AbWin + ( TScBmBm * RGlFront * RScBack + TScBmDiff ) * Construct( ConstrNum ).AbsDiff( Lay ) / ( 1.0 - RGlDiffFront * RScDifBack ) * CosInc * FracSunLit;

										ADiffWinSh = ADiffWin * TScDifDif / ( 1.0 - RGlDifFr * RScDifBack );

									} else {
										// Between-glass blind on

										// Isolated glass and blind properties at current incidence angle, profile angle and slat angle
										if ( Lay == 1 ) {
											t1 = POLYF( CosInc, Construct( ConstrNum ).tBareSolCoef( {1,6}, 1 ) );
											t2 = POLYF( CosInc, Construct( ConstrNum ).tBareSolCoef( {1,6}, 2 ) );
											af1 = POLYF( CosInc, Construct( ConstrNum ).afBareSolCoef( {1,6}, 1 ) );
											af2 = POLYF( CosInc, Construct( ConstrNum ).afBareSolCoef( {1,6}, 2 ) );
											ab1 = POLYF( CosInc, Construct( ConstrNum ).abBareSolCoef( {1,6}, 1 ) );
											ab2 = POLYF( CosInc, Construct( ConstrNum ).abBareSolCoef( {1,6}, 2 ) );
											rf1 = POLYF( CosInc, Construct( ConstrNum ).rfBareSolCoef( {1,6}, 1 ) );
											rf2 = POLYF( CosInc, Construct( ConstrNum ).rfBareSolCoef( {1,6}, 2 ) );
											rb1 = POLYF( CosInc, Construct( ConstrNum ).rbBareSolCoef( {1,6}, 1 ) );
											rb2 = POLYF( CosInc, Construct( ConstrNum ).rbBareSolCoef( {1,6}, 2 ) );
											td1 = Construct( ConstrNum ).tBareSolDiff( 1 );
											td2 = Construct( ConstrNum ).tBareSolDiff( 2 );
											afd1 = Construct( ConstrNum ).afBareSolDiff( 1 );
											afd2 = Construct( ConstrNum ).afBareSolDiff( 2 );
											abd1 = Construct( ConstrNum ).abBareSolDiff( 1 );
											abd2 = Construct( ConstrNum ).abBareSolDiff( 2 );
											rfd1 = Construct( ConstrNum ).rfBareSolDiff( 1 );
											rfd2 = Construct( ConstrNum ).rfBareSolDiff( 2 );
											rbd1 = Construct( ConstrNum ).rbBareSolDiff( 1 );
											rbd2 = Construct( ConstrNum ).rbBareSolDiff( 2 );
											tfshBB = BlindBeamBeamTrans( ProfAng, SlatAng, Blind( BlNum ).SlatWidth, Blind( BlNum ).SlatSeparation, Blind( BlNum ).SlatThickness );
											tfshBd = InterpProfSlatAng( ProfAng, SlatAng, VarSlats, Blind( BlNum ).SolFrontBeamDiffTrans );
											tfshd = InterpSlatAng( SlatAng, VarSlats, Blind( BlNum ).SolFrontDiffDiffTrans );
											tbshBB = BlindBeamBeamTrans( ProfAng, Pi - SlatAng, Blind( BlNum ).SlatWidth, Blind( BlNum ).SlatSeparation, Blind( BlNum ).SlatThickness );
											tbshBd = InterpProfSlatAng( ProfAng, SlatAng, VarSlats, Blind( BlNum ).SolBackBeamDiffTrans );
											tbshd = InterpSlatAng( SlatAng, VarSlats, Blind( BlNum ).SolBackDiffDiffTrans );
											afshB = InterpProfSlatAng( ProfAng, SlatAng, VarSlats, Blind( BlNum ).SolFrontBeamAbs );
											abshB = InterpProfSlatAng( ProfAng, SlatAng, VarSlats, Blind( BlNum ).SolBackBeamAbs );
											afshd = InterpSlatAng( SlatAng, VarSlats, Blind( BlNum ).SolFrontDiffAbs );
											abshd = InterpSlatAng( SlatAng, VarSlats, Blind( BlNum ).SolBackDiffAbs );
											rfshB = InterpProfSlatAng( ProfAng, SlatAng, VarSlats, Blind( BlNum ).SolFrontBeamDiffRefl );
											rbshB = InterpProfSlatAng( ProfAng, SlatAng, VarSlats, Blind( BlNum ).SolBackBeamDiffRefl );
											rfshd = InterpSlatAng( SlatAng, VarSlats, Blind( BlNum ).SolFrontDiffDiffRefl );
											rbshd = InterpSlatAng( SlatAng, VarSlats, Blind( BlNum ).SolBackDiffDiffRefl );
										}

										if ( Lay == 1 && NGlass == 3 ) {
											t1t2 = t1 * t2;
											td1td2 = td1 * td2;
											t3 = POLYF( CosInc, Construct( ConstrNum ).tBareSolCoef( {1,6}, 3 ) );
											af3 = POLYF( CosInc, Construct( ConstrNum ).afBareSolCoef( {1,6}, 3 ) );
											ab3 = POLYF( CosInc, Construct( ConstrNum ).abBareSolCoef( {1,6}, 3 ) );
											rf3 = POLYF( CosInc, Construct( ConstrNum ).rfBareSolCoef( {1,6}, 3 ) );
											rb3 = POLYF( CosInc, Construct( ConstrNum ).rbBareSolCoef( {1,6}, 3 ) );
											td3 = Construct( ConstrNum ).tBareSolDiff( 3 );
											afd3 = Construct( ConstrNum ).afBareSolDiff( 3 );
											abd3 = Construct( ConstrNum ).abBareSolDiff( 3 );
											rfd3 = Construct( ConstrNum ).rfBareSolDiff( 3 );
											rbd3 = Construct( ConstrNum ).rbBareSolDiff( 3 );
										}

										if ( NGlass == 2 ) {
											if ( Lay == 1 ) {
												AbWinSh = CosInc * FracSunLit * ( af1 + t1 * tfshBB * rf2 * tbshBB * ab1 + t1 * ( rfshB + rfshB * rbd1 * rfshd + tfshBB * rf2 * tbshBd + tfshBd * rfd2 * tbshd ) * abd1 );
												ADiffWinSh = afd1 + td1 * ( rfshd + rfshd * rbd1 * rfshd + tfshd * rfd2 * tbshd ) * abd1;
											} else if ( Lay == 2 ) {
												AbWinSh = CosInc * FracSunLit * ( t1 * rfshB * af2 + t1 * ( rfshB * rf2 * rbshd + tfshBd * ( 1 + rfd2 * rbshd ) + rfshB * rbd1 * tfshd ) * afd2 );
												ADiffWinSh = td1 * ( tfshd * ( 1 + rfd2 * rbshd ) + rfshd * rbd1 * tfshd ) * afd2;
											}
										} // End of check if NGlass = 2

										if ( NGlass == 3 ) {
											if ( Lay == 1 ) {
												AbWinSh = CosInc * FracSunLit * ( af1 + t1 * rf2 * ab1 + t1t2 * tfshBB * rf3 * tbshBB * t2 * ab1 + t1t2 * ( rfshB * td2 + rfshB * rbd2 * rfshd * td2 + tfshBd * rfd3 * tbshd * td2 ) * abd1 );
												ADiffWinSh = afd1 + td1 * rbd2 * abd1 + td1td2 * ( rfshd * ( 1 + rbd2 * rfshd + td2 * rbd1 * td2 * rfshd ) + tfshd * ( rfd3 * tbshd + rfd3 * rbshd * rfd3 * tbshd ) ) * td2 * abd1;
											} else if ( Lay == 2 ) {
												AbWinSh = CosInc * FracSunLit * ( t1 * af2 + t1t2 * ( tfshBB * rf3 * tbshBB * ab2 + rfshB * td2 * rbd1 * afd2 ) + t1t2 * ( rfshB * ( 1 + rbd2 * rfshd ) + tfshBB * rf3 * tbshBd + tfshBd * rfd3 * tbshd ) * abd2 );
												ADiffWinSh = td1 * afd2 + td1td2 * rfshd * td2 * rbd1 * afd2 + td1td2 * ( rfshd * ( 1 + rbd2 * rfshd ) + tfshd * rfd3 * tbshd ) * abd2;
											} else if ( Lay == 3 ) {
												AbWinSh = CosInc * FracSunLit * ( t1t2 * tfshBB * af3 + t1t2 * ( tfshBB * rf3 * rbshB + tfshBd * ( 1 + rfd3 * rbshd ) + rfshB * ( rbd2 * tfshd + td2 * rbd1 * td2 * tfshd ) ) * afd3 );
												ADiffWinSh = td1td2 * ( tfshd * ( 1 + rfd3 * rbshd ) + rfshd * ( rbd2 * tfshd + td2 * rbd1 * td2 * tfshd ) ) * afd3;
											}
										} // End of check if NGlass = 3

									} // End of check if blind is interior, exterior or between-glass
								} // End of check if a blind is on

								if ( ShadeFlag != SwitchableGlazing ) {

									// Interior or between glass shade or blind on

									AWinSurf( Lay, SurfNum ) = AbWinSh;
									// Add contribution of diffuse from beam on outside reveal
									if ( ShadeFlag == IntShadeOn || ShadeFlag == IntBlindOn || ShadeFlag == BGShadeOn || ShadeFlag == BGBlindOn ) AWinSurf( Lay, SurfNum ) += ADiffWinSh * SurfaceWindow( SurfNum ).OutsRevealDiffOntoGlazing;

								} else {
									// Switchable glazing

									SwitchFac = SurfaceWindow( SurfNum ).SwitchingFactor;
									AWinSurf( Lay, SurfNum ) = InterpSw( SwitchFac, AbWin, AbWinSh );
									// Add contribution of diffuse from beam on outside and inside reveal
									AWinSurf( Lay, SurfNum ) += InterpSw( SwitchFac, ADiffWin, ADiffWinSh ) * SurfaceWindow( SurfNum ).OutsRevealDiffOntoGlazing + InterpSw( SwitchFac, Construct( ConstrNum ).AbsDiffBack( Lay ), Construct( ConstrNumSh ).AbsDiffBack( Lay ) ) * SurfaceWindow( SurfNum ).InsRevealDiffOntoGlazing;
								}
							} // End of check if window has shading device
						} // End of loop over window glass layers

						//-----------------------------------------
						// EXTERIOR BEAM ABSORBED BY SHADING DEVICE
						//-----------------------------------------

						// Exterior beam absorbed by INTERIOR SHADE

						if ( ShadeFlag == IntShadeOn ) {
							// Note that AbsBeamShadeCoef includes effect of shade/glazing inter-reflection
							AbsShade = POLYF( CosInc, Construct( ConstrNumSh ).AbsBeamShadeCoef );

							ExtBeamAbsByShadFac( SurfNum ) = ( AbsShade * CosInc * SunLitFract * InOutProjSLFracMult + SurfaceWindow( SurfNum ).OutsRevealDiffOntoGlazing * Construct( ConstrNumSh ).AbsDiffShade ) * SurfaceWindow( SurfNum ).GlazedFrac;
							// In the above, GlazedFrac corrects for shadowing of divider onto interior shade
						}

						// Exterior beam absorbed by EXTERIOR SHADE

						if ( ShadeFlag == ExtShadeOn ) {
							ExtBeamAbsByShadFac( SurfNum ) = Construct( ConstrNumSh ).AbsDiffShade * CosInc * SunLitFract;

						}

						// Exterior beam absorbed by BETWEEN-GLASS SHADE

						if ( ShadeFlag == BGShadeOn ) {
							AbsShade = POLYF( CosInc, Construct( ConstrNumSh ).AbsBeamShadeCoef );
							ExtBeamAbsByShadFac( SurfNum ) = AbsShade * CosInc * SunLitFract + SurfaceWindow( SurfNum ).OutsRevealDiffOntoGlazing * Construct( ConstrNumSh ).AbsDiffShade;
						}

						// Exterior beam absorbed by INTERIOR BLIND

						if ( ShadeFlag == IntBlindOn ) {
							TBmBm = POLYF( CosInc, Construct( ConstrNum ).TransSolBeamCoef );
							RGlDiffBack = Construct( ConstrNum ).ReflectSolDiffBack;
							RhoBlFront = InterpProfSlatAng( ProfAng, SlatAng, VarSlats, Blind( BlNum ).SolFrontBeamDiffRefl );
							AbsBlFront = InterpProfSlatAng( ProfAng, SlatAng, VarSlats, Blind( BlNum ).SolFrontBeamAbs );
							RhoBlDiffFront = InterpSlatAng( SlatAng, VarSlats, Blind( BlNum ).SolFrontDiffDiffRefl );
							AbsBlDiffFront = InterpSlatAng( SlatAng, VarSlats, Blind( BlNum ).SolFrontDiffAbs );
							AbsShade = TBmBm * ( AbsBlFront + RhoBlFront * RGlDiffBack * AbsBlDiffFront / ( 1.0 - RhoBlDiffFront * RGlDiffBack ) );
							AbsShadeDiff = Construct( ConstrNum ).TransDiff * ( AbsBlDiffFront + RhoBlDiffFront * RGlDiffBack * AbsBlDiffFront / ( 1.0 - RhoBlDiffFront * RGlDiffBack ) );

							ExtBeamAbsByShadFac( SurfNum ) = ( AbsShade * CosInc * SunLitFract * InOutProjSLFracMult + SurfaceWindow( SurfNum ).OutsRevealDiffOntoGlazing * AbsShadeDiff ) * SurfaceWindow( SurfNum ).GlazedFrac;
							// In the above, GlazedFrac corrects for shadowing of divider onto interior blind
						}

						// Exterior beam absorbed by EXTERIOR BLIND

						if ( ShadeFlag == ExtBlindOn ) {
							TBlBmBm = BlindBeamBeamTrans( ProfAng, SlatAng, Blind( BlNum ).SlatWidth, Blind( BlNum ).SlatSeparation, Blind( BlNum ).SlatThickness );
							RGlFront = POLYF( CosInc, Construct( ConstrNum ).ReflSolBeamFrontCoef );
							AbsBlFront = InterpProfSlatAng( ProfAng, SlatAng, VarSlats, Blind( BlNum ).SolFrontBeamAbs );
							AbsBlBack = InterpProfSlatAng( ProfAng, SlatAng, VarSlats, Blind( BlNum ).SolBackBeamAbs );
							AbsBlDiffBack = InterpSlatAng( SlatAng, VarSlats, Blind( BlNum ).SolBackDiffAbs );
							RGlDiffFront = Construct( ConstrNum ).ReflectSolDiffFront;
							RhoBlDiffBack = InterpSlatAng( SlatAng, VarSlats, Blind( BlNum ).SolBackDiffDiffRefl );
							RhoBlBack = InterpProfSlatAng( ProfAng, SlatAng, VarSlats, Blind( BlNum ).SolBackBeamDiffRefl );
							TBlBmDiff = InterpProfSlatAng( ProfAng, SlatAng, VarSlats, Blind( BlNum ).SolFrontBeamDiffTrans );
							AbsShade = AbsBlFront + AbsBlBack * RGlFront * TBlBmBm + ( AbsBlDiffBack * RGlDiffFront / ( 1.0 - RhoBlDiffBack * RGlDiffFront ) ) * ( RGlFront * TBlBmBm * RhoBlBack + TBlBmDiff );
							ExtBeamAbsByShadFac( SurfNum ) = AbsShade * CosInc * SunLitFract * InOutProjSLFracMult;

						}

						// Exterior beam absorbed by EXTERIOR SCREEN
						if ( ShadeFlag == ExtScreenOn ) {
							TScBmBm = SurfaceScreens( SurfaceWindow( SurfNum ).ScreenNumber ).BmBmTrans;
							//        TScBmDiff     = SurfaceScreens(SurfaceWindow(SurfNum)%ScreenNumber)%BmDifTrans
							RGlFront = POLYF( CosInc, Construct( ConstrNum ).ReflSolBeamFrontCoef );
							RGlDiffFront = Construct( ConstrNum ).ReflectSolDiffFront;

							AbsScBeam = SurfaceScreens( ScNum ).AbsorpSolarBeamFront;
							AbsScDiffBack = SurfaceScreens( ScNum ).DifScreenAbsorp;
							RScDifBack = SurfaceScreens( ScNum ).DifReflect;
							RScBack = SurfaceScreens( ScNum ).ReflectSolBeamFront;

							AbsScreen = AbsScBeam * ( 1.0 + TScBmBm * RGlFront ) + ( AbsScDiffBack * TScBmBm * RGlFront * RGlDiffFront * RScBack / ( 1.0 - RScDifBack * RGlDiffFront ) );

							ExtBeamAbsByShadFac( SurfNum ) = AbsScreen * CosInc * SunLitFract * InOutProjSLFracMult;

						}

						// Exterior beam absorbed by BETWEEN-GLASS BLIND

						if ( ShadeFlag == BGBlindOn ) {
							if ( NGlass == 2 ) {
								AbsShade = t1 * ( afshB + tfshBB * rf2 * abshB + tfshBd * rfd2 * abshd + rfshB * rbd1 * afshd );
								AbsShadeDiff = td1 * ( afshd * ( 1 + rfshd * rbd1 ) + tfshd * rfd2 * abshd );
							} else if ( NGlass == 3 ) {
								AbsShade = t1t2 * ( afshB * ( 1 + tfshBB * rf3 ) + afshd * ( tfshBd * rfd3 + rfshB * ( rbd2 + td2 * rbd1 * td2 ) ) );
								AbsShadeDiff = td1td2 * ( afshd + tfshd * rfd3 * abshd + rfshd * ( rfd2 + td2 * rbd2 * td2 ) * afshd );
							}
							ExtBeamAbsByShadFac( SurfNum ) = AbsShade * CosInc * SunLitFract * InOutProjSLFracMult + SurfaceWindow( SurfNum ).OutsRevealDiffOntoGlazing * AbsShadeDiff;
						} // End of check if between-glass blind

					} else if ( SurfaceWindow( SurfNum ).WindowModelType == WindowBSDFModel ) {

						FenSolAbsPtr = WindowScheduledSolarAbs( SurfNum, ConstrNum );

						// Do not read from schedule file here since this will be called only if direct beam is hitting the window and schedule
						// will not be loaded in that case even if diffuse part of solar radiation is entering through the window
						if ( FenSolAbsPtr == 0 ) {
							// Put in the equivalent layer absorptions
							for ( Lay = 1; Lay <= SurfaceWindow( SurfNum ).ComplexFen.State( SurfaceWindow( SurfNum ).ComplexFen.CurrentState ).NLayers; ++Lay ) {
								AbWin = SurfaceWindow( SurfNum ).ComplexFen.State( SurfaceWindow( SurfNum ).ComplexFen.CurrentState ).WinBmFtAbs( HourOfDay, TimeStep, Lay ) * CosInc * SunLitFract * SurfaceWindow( SurfNum ).OutProjSLFracMult( HourOfDay );

								// Add contribution of beam reflected from outside and inside reveal
								AWinSurf( Lay, SurfNum ) = AbWin + SurfaceWindow( SurfNum ).OutsRevealDiffOntoGlazing * SurfaceWindow( SurfNum ).ComplexFen.State( SurfaceWindow( SurfNum ).ComplexFen.CurrentState ).WinFtHemAbs( Lay ) + SurfaceWindow( SurfNum ).InsRevealDiffOntoGlazing * SurfaceWindow( SurfNum ).ComplexFen.State( SurfaceWindow( SurfNum ).ComplexFen.CurrentState ).WinBkHemAbs( Lay );
							}
						}

					} else if ( SurfaceWindow( SurfNum ).WindowModelType == WindowEQLModel ) {
						// call the ASHWAT fenestration model for optical properties
						// determine the beam radiation absorptance and tranmittance of the
						// the equivalent layer window model
						CalcEQLOpticalProperty( SurfNum, isBEAM, AbsSolBeamEQL );

						// recalcuate the diffuse absorptance and transmittance of the
						// the equivalent layer window model if there is shade control
						EQLNum = Construct( Surface( SurfNum ).Construction ).EQLConsPtr;
						if ( CFS( EQLNum ).ISControlled ) {
							CalcEQLOpticalProperty( SurfNum, isDIFF, AbsSolDiffEQL );
						} else {
							AbsSolDiffEQL( _, {1,CFS( EQLNum ).NL + 1} ) = CFSDiffAbsTrans( _, {1,CFS( EQLNum ).NL + 1}, EQLNum );
						}
						Construct( ConstrNum ).TransDiff = AbsSolDiffEQL( 1, CFS( EQLNum ).NL + 1 );

						for ( Lay = 1; Lay <= CFS( EQLNum ).NL + 1; ++Lay ) {
							AbWinEQL = AbsSolBeamEQL( 1, Lay ) * CosInc * SunLitFract * InOutProjSLFracMult;
							if ( CFS( EQLNum ).L( 1 ).LTYPE != ltyGLAZE ) {
								// if the first layer is not glazing (or it is a shade) do not
								AWinSurf( Lay, SurfNum ) = AbWinEQL;
							} else {
								// the first layer is a glazing, include the outside reveal reflection
								// and the inside reveal reflection until indoor shade layer is encountered.
								if ( CFS( EQLNum ).L( Lay ).LTYPE == ltyGLAZE ) {
									AWinSurf( Lay, SurfNum ) = AbWinEQL + SurfaceWindow( SurfNum ).OutsRevealDiffOntoGlazing * AbsSolBeamEQL( 1, Lay ) + SurfaceWindow( SurfNum ).InsRevealDiffOntoGlazing * AbsSolDiffEQL( 2, Lay );
								} else {
									AWinSurf( Lay, SurfNum ) = AbWinEQL + SurfaceWindow( SurfNum ).OutsRevealDiffOntoGlazing * AbsSolBeamEQL( 1, Lay );
								}
							}
						}
						TBmBmEQL = AbsSolBeamEQL( 1, CFS( EQLNum ).NL + 1 );
						// Beam-diffuse transmittance
						TBmDiffEQL = max( 0.0, AbsSolBeamEQL( 2, CFS( EQLNum ).NL + 1 ) );
						// Beam-beam transmittance: difference between beam-total and beam-diffuse transmittance
						TBmBmEQL = max( 0.0, ( TBmBmEQL - TBmDiffEQL ) );
					}

				} // End of SunlitFrac check

				//-----------------------------------------------------------------
				// SKY AND GROUND DIFFUSE SOLAR GAIN INTO ZONE FROM EXTERIOR WINDOW
				//-----------------------------------------------------------------

				SkySolarInc = SurfaceWindow( SurfNum ).SkySolarInc;
				GndSolarInc = SurfaceWindow( SurfNum ).GndSolarInc;

				if ( SurfaceWindow( SurfNum ).WindowModelType != WindowBSDFModel && SurfaceWindow( SurfNum ).WindowModelType != WindowEQLModel ) { // Regular window

					DiffTrans = Construct( ConstrNum ).TransDiff;
					if ( DifSolarRad != 0.0 ) {
						DSZoneWin = ( SkySolarInc * DiffTrans * Surface( SurfNum ).Area ) / ( DifSolarRad );
					} else {
						DSZoneWin = ( SkySolarInc * DiffTrans * Surface( SurfNum ).Area ) / ( 1.e-8 );
					}
					if ( GndSolarRad != 0.0 ) {
						DGZoneWin = ( GndSolarInc * DiffTrans * Surface( SurfNum ).Area ) / ( GndSolarRad );
					} else {
						DGZoneWin = ( GndSolarInc * DiffTrans * Surface( SurfNum ).Area ) / ( 1.e-8 );
					}
				} else if ( SurfaceWindow( SurfNum ).OriginalClass == SurfaceClass_TDD_Diffuser ) {
					DiffTrans = TransTDD( PipeNum, CosInc, SolarAniso );

					DSZoneWin = AnisoSkyMult( SurfNum2 ) * DiffTrans * Surface( SurfNum ).Area;
					DGZoneWin = Surface( SurfNum2 ).ViewFactorGround * TDDPipe( PipeNum ).TransSolIso * Surface( SurfNum ).Area;

				} else if ( Surface( SurfNum ).Class == SurfaceClass_TDD_Dome ) {
					DiffTrans = Construct( ConstrNum ).TransDiff;

					DSZoneWin = 0.0; // Solar not added by TDD:DOME; added to zone via TDD:DIFFUSER
					DGZoneWin = 0.0; // Solar not added by TDD:DOME; added to zone via TDD:DIFFUSER

				} else if ( OutShelfSurf > 0 ) { // Outside daylighting shelf
					DiffTrans = Construct( ConstrNum ).TransDiff;

					DSZoneWin = AnisoSkyMult( SurfNum ) * DiffTrans * Surface( SurfNum ).Area;

					ShelfSolarRad = ( BeamSolarRad * SunlitFrac( TimeStep, HourOfDay, OutShelfSurf ) * CosIncAng( TimeStep, HourOfDay, OutShelfSurf ) + DifSolarRad * AnisoSkyMult( OutShelfSurf ) ) * Shelf( ShelfNum ).OutReflectSol;

					// Add all reflected solar from the outside shelf to the ground solar
					// NOTE:  If the shelf blocks part of the view to the ground, the user must reduce the ground view factor!!

					// In order to get the effect of the daylighting shelf in here, must take into account the fact that this
					// is ultimately multiplied by GndSolarRad to get QD and QDV in InitSolarHeatGains.
					// DGZoneWin = (GndVF*Trans*Area*GndSolarRad + ShelfVF*Trans*Area*ShelfSolarRad) / GndSolarRad
					if ( GndSolarRad != 0.0 ) {
						DGZoneWin = ( Surface( SurfNum ).ViewFactorGround * DiffTrans * Surface( SurfNum ).Area * GndSolarRad + Shelf( ShelfNum ).ViewFactor * DiffTrans * Surface( SurfNum ).Area * ShelfSolarRad ) / GndSolarRad;
					} else {
						DGZoneWin = 0.0;
					}

				} else if ( SurfaceWindow( SurfNum ).WindowModelType == WindowBSDFModel ) { // complex fenestration
					FenSolAbsPtr = WindowScheduledSolarAbs( SurfNum, ConstrNum );
					if ( FenSolAbsPtr == 0 ) {
						//Sky Diffuse transmitted by Complex Fen
						DiffTransSky = SurfaceWindow( SurfNum ).ComplexFen.State( SurfaceWindow( SurfNum ).ComplexFen.CurrentState ).WinSkyTrans;
						if ( DifSolarRad != 0.0 ) {
							DSZoneWin = SkySolarInc * DiffTransSky * Surface( SurfNum ).Area / ( DifSolarRad );
						} else {
							DSZoneWin = SkySolarInc * DiffTransSky * Surface( SurfNum ).Area / ( 1.e-8 );
						}
						//Ground Diffuse transmitted by Complex Fen
						DiffTransGnd = SurfaceWindow( SurfNum ).ComplexFen.State( SurfaceWindow( SurfNum ).ComplexFen.CurrentState ).WinSkyGndTrans;
						DiffTransBmGnd = SurfaceWindow( SurfNum ).ComplexFen.State( SurfaceWindow( SurfNum ).ComplexFen.CurrentState ).WinBmGndTrans( HourOfDay, TimeStep );
						if ( GndSolarRad != 0.0 ) {
							DGZoneWin = ( ( SurfaceWindow( SurfNum ).BmGndSolarInc * DiffTransBmGnd + SurfaceWindow( SurfNum ).SkyGndSolarInc * DiffTransGnd ) * Surface( SurfNum ).Area ) / ( GndSolarRad );
						} else {
							DGZoneWin = ( ( SurfaceWindow( SurfNum ).BmGndSolarInc * DiffTransBmGnd + SurfaceWindow( SurfNum ).SkyGndSolarInc * DiffTransGnd ) * Surface( SurfNum ).Area ) / ( 1.e-8 );
						}

						//Define the effective transmittance for total sky and ground radiation
						if ( ( SkySolarInc + SurfaceWindow( SurfNum ).BmGndSolarInc + SurfaceWindow( SurfNum ).SkyGndSolarInc ) != 0.0 ) {
							DiffTrans = ( SkySolarInc * DiffTransSky + SurfaceWindow( SurfNum ).BmGndSolarInc * DiffTransBmGnd + SurfaceWindow( SurfNum ).SkyGndSolarInc * DiffTransGnd ) / ( SkySolarInc + SurfaceWindow( SurfNum ).BmGndSolarInc + SurfaceWindow( SurfNum ).SkyGndSolarInc );
						} else {
							DiffTrans = 0.0;
						}

						//Also update the nominal diffuse transmittance
						NomDiffTrans = SurfaceWindow( SurfNum ).ComplexFen.State( SurfaceWindow( SurfNum ).ComplexFen.CurrentState ).WinDiffTrans;
						Construct( Surface( SurfNum ).Construction ).TransDiff = NomDiffTrans;
					} else {
						DSZoneWin = 0.0;
						DGZoneWin = 0.0;
						DiffTrans = 0.0;
						TBmBm = 0.0;
						TBmDif = 0.0;
						NomDiffTrans = 0.0;
					}

				} else if ( SurfaceWindow( SurfNum ).WindowModelType == WindowEQLModel ) {

					DiffTrans = Construct( ConstrNum ).TransDiff;

					if ( DifSolarRad != 0.0 ) {
						DSZoneWin = ( SkySolarInc * DiffTrans * Surface( SurfNum ).Area ) / ( DifSolarRad );
					} else {
						DSZoneWin = ( SkySolarInc * DiffTrans * Surface( SurfNum ).Area ) / ( 1.e-8 );
					}
					if ( GndSolarRad != 0.0 ) {
						DGZoneWin = ( GndSolarInc * DiffTrans * Surface( SurfNum ).Area ) / ( GndSolarRad );
					} else {
						DGZoneWin = ( GndSolarInc * DiffTrans * Surface( SurfNum ).Area ) / ( 1.e-8 );
					}

				}

				if ( ( SurfaceWindow( SurfNum ).WindowModelType != WindowBSDFModel ) && ( SurfaceWindow( SurfNum ).WindowModelType != WindowEQLModel ) ) {
					if ( ShadeFlag <= 0 || ShadeFlag >= 10 ) {
						// Unshaded window
						DSZone( ZoneNum ) += DSZoneWin;
						DGZone( ZoneNum ) += DGZoneWin;
					} else if ( ShadeFlag != SwitchableGlazing ) {
						// Shade or blind
						if ( ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == BGShadeOn || ShadeFlag == ExtScreenOn ) {
							// Shade or screen
							DiffTrans = Construct( ConstrNumSh ).TransDiff;
						} else {
							// Blind
							DiffTrans = InterpSlatAng( SlatAng, VarSlats, Construct( ConstrNumSh ).BlTransDiff );
							// For blinds with horizontal slats, allow different diffuse/diffuse transmittance for
							// ground and sky solar
							if ( Blind( SurfaceWindow( SurfNum ).BlindNumber ).SlatOrientation == Horizontal ) {
								DiffTransGnd = InterpSlatAng( SlatAng, VarSlats, Construct( ConstrNumSh ).BlTransDiffGnd );
								DiffTransSky = InterpSlatAng( SlatAng, VarSlats, Construct( ConstrNumSh ).BlTransDiffSky );
							}
						}
						if ( DifSolarRad != 0.0 ) {
							DSZoneWinSh = SkySolarInc * DiffTrans * Surface( SurfNum ).Area / ( DifSolarRad );
						} else {
							DSZoneWinSh = SkySolarInc * DiffTrans * Surface( SurfNum ).Area / ( 1.e-8 );
						}

						if ( GndSolarRad != 0.0 ) {
							DGZoneWinSh = GndSolarInc * DiffTrans * Surface( SurfNum ).Area / ( GndSolarRad );
						} else {
							DGZoneWinSh = GndSolarInc * DiffTrans * Surface( SurfNum ).Area / ( 1.e-8 );
						}

						if ( ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == BGBlindOn ) {
							if ( Blind( SurfaceWindow( SurfNum ).BlindNumber ).SlatOrientation == Horizontal ) {
								CosTlt = Surface( SurfNum ).CosTilt;

								if ( DifSolarRad != 0.0 ) {
									DSZoneWinSh = SkySolarInc * Surface( SurfNum ).Area * ( 0.5 * std::abs( CosTlt ) * DiffTransGnd + ( 1.0 - 0.5 * std::abs( CosTlt ) ) * DiffTransSky ) / ( DifSolarRad );
								} else {
									DSZoneWinSh = SkySolarInc * Surface( SurfNum ).Area * ( 0.5 * std::abs( CosTlt ) * DiffTransGnd + ( 1.0 - 0.5 * std::abs( CosTlt ) ) * DiffTransSky ) / ( 1.e-8 );
								}

								if ( GndSolarRad != 0.0 ) {
									DGZoneWinSh = GndSolarInc * Surface( SurfNum ).Area * ( ( 1.0 - 0.5 * std::abs( CosTlt ) ) * DiffTransGnd + 0.5 * std::abs( CosTlt ) * DiffTransSky ) / ( GndSolarRad );
								} else {
									DGZoneWinSh = GndSolarInc * Surface( SurfNum ).Area * ( ( 1.0 - 0.5 * std::abs( CosTlt ) ) * DiffTransGnd + 0.5 * std::abs( CosTlt ) * DiffTransSky ) / ( 1.e-8 );
								}
							}
						}
						DSZone( ZoneNum ) += DSZoneWinSh;
						DGZone( ZoneNum ) += DGZoneWinSh;
					} else {
						// Switchable glazing
						SwitchFac = SurfaceWindow( SurfNum ).SwitchingFactor;
						DiffTrans = InterpSw( SwitchFac, Construct( ConstrNum ).TransDiff, Construct( ConstrNumSh ).TransDiff );
						if ( DifSolarRad != 0.0 ) {
							DSZoneWinSh = SkySolarInc * DiffTrans * Surface( SurfNum ).Area / ( DifSolarRad );
						} else {
							DSZoneWinSh = SkySolarInc * DiffTrans * Surface( SurfNum ).Area / ( 1.e-8 );
						}
						if ( GndSolarRad != 0.0 ) {
							DGZoneWinSh = GndSolarInc * DiffTrans * Surface( SurfNum ).Area / ( GndSolarRad );
						} else {
							DGZoneWinSh = GndSolarInc * DiffTrans * Surface( SurfNum ).Area / ( 1.e-8 );
						}
						DSZone( ZoneNum ) += InterpSw( SwitchFac, DSZoneWin, DSZoneWinSh );
						DGZone( ZoneNum ) += InterpSw( SwitchFac, DGZoneWin, DGZoneWinSh );
					}
				} else if ( SurfaceWindow( SurfNum ).WindowModelType == WindowBSDFModel ) {
					DSZone( ZoneNum ) += DSZoneWin;
					DGZone( ZoneNum ) += DGZoneWin;
				} else if ( SurfaceWindow( SurfNum ).WindowModelType == WindowEQLModel ) {
					// For equivalent layer model the zone total diffuse solar heat gain
					// through exterior fenestrations are reported as single value.
					DSZoneWin = SkySolarInc * DiffTrans * Surface( SurfNum ).Area / ( DifSolarRad + 1.e-8 );
					DGZoneWin = GndSolarInc * DiffTrans * Surface( SurfNum ).Area / ( GndSolarRad + 1.e-8 );

					DSZone( ZoneNum ) += DSZoneWin;
					DGZone( ZoneNum ) += DGZoneWin;
				}
				//-----------------------------------------------------------------
				// BEAM SOLAR ON EXTERIOR WINDOW TRANSMITTED AS BEAM AND/OR DIFFUSE
				//-----------------------------------------------------------------

				TBmBm = 0.0;
				TBmDif = 0.0;
				TBmAllShBlSc = 0.0;
				TBmBmShBlSc = 0.0;
				TBmDifShBlSc = 0.0;

				// Beam-beam transmittance for bare exterior window
				if ( SunLitFract > 0.0 ) {
					if ( SurfaceWindow( SurfNum ).OriginalClass == SurfaceClass_TDD_Diffuser ) {
						TBmDif = TransTDD( PipeNum, CosInc, SolarBeam );
						TDDPipe( PipeNum ).TransSolBeam = TBmDif; // Report variable
					} else if ( SurfaceWindow( SurfNum ).WindowModelType != WindowBSDFModel && SurfaceWindow( SurfNum ).WindowModelType != WindowEQLModel ) { // Regular window
						if ( ! SurfaceWindow( SurfNum ).SolarDiffusing ) { // Clear glazing
							TBmBm = POLYF( CosInc, Construct( ConstrNum ).TransSolBeamCoef ); //[-]
						} else { // Diffusing glazing
							TBmDif = POLYF( CosInc, Construct( ConstrNum ).TransSolBeamCoef ); //[-]
						}
					} else if ( SurfaceWindow( SurfNum ).WindowModelType == WindowBSDFModel ) {
						// Need to check what effect, if any, defining these here has
						TBmBm = SurfaceWindow( SurfNum ).ComplexFen.State( SurfaceWindow( SurfNum ).ComplexFen.CurrentState ).WinDirSpecTrans( HourOfDay, TimeStep );
						TBmDif = SurfaceWindow( SurfNum ).ComplexFen.State( SurfaceWindow( SurfNum ).ComplexFen.CurrentState ).WinDirHemiTrans( HourOfDay, TimeStep ) - TBmBm;
					} else if ( SurfaceWindow( SurfNum ).WindowModelType == WindowEQLModel ) {
						// get ASHWAT fenestration model beam-beam and beam-diffuse properties
						TBmBm = TBmBmEQL;
						TBmDif = TBmDiffEQL;
					}
				}

				// Report variables
				SurfaceWindow( SurfNum ).GlTsolBmBm = TBmBm;
				SurfaceWindow( SurfNum ).GlTsolBmDif = TBmDif;

				// Diffuse-diffuse transmittance for bare exterior window
				if ( SurfaceWindow( SurfNum ).OriginalClass == SurfaceClass_TDD_Diffuser ) {
					TDifBare = TransTDD( PipeNum, CosInc, SolarAniso );
				} else {
					if ( SurfaceWindow( SurfNum ).WindowModelType == WindowBSDFModel ) {
						//Complex Fenestration: use hemispherical ave of directional-hemispherical transmittance
						//Note: this is not quite the same as the effective transmittance for total of sky and ground radiation
						TDifBare = SurfaceWindow( SurfNum ).ComplexFen.State( SurfaceWindow( SurfNum ).ComplexFen.CurrentState ).WinDiffTrans;
					} else if ( SurfaceWindow( SurfNum ).WindowModelType == WindowEQLModel ) {
						//get ASHWAT fenestration model diffuse-diffuse properties includes shade if present
						TDifBare = Construct( ConstrNum ).TransDiff;
					} else { // Regular window
						TDifBare = Construct( ConstrNum ).TransDiff;
					}
				}
				SurfaceWindow( SurfNum ).GlTsolDifDif = TDifBare;

				if ( SurfaceWindow( SurfNum ).WindowModelType != WindowEQLModel ) {
					if ( ShadeFlag > 0 && ShadeFlag < 10 ) {

						// Shade or screen or blind on, or switchable glazing
						// (note in the following that diffusing glass is not allowed in a window with
						// shade, blind or switchable glazing)

						if ( ShadeFlag != IntBlindOn && ShadeFlag != ExtBlindOn && ShadeFlag != BGBlindOn && ShadeFlag != ExtScreenOn ) {

							// Shade on or switchable glazing

							if ( SunLitFract > 0.0 ) TBmAllShBlSc = POLYF( CosInc, Construct( ConstrNumSh ).TransSolBeamCoef );

						} else {

							// Blind or Screen on

							SurfaceWindow( SurfNum ).BlGlSysTsolDifDif = DiffTrans;
							SurfaceWindow( SurfNum ).ScGlSysTsolDifDif = DiffTrans;
							if ( ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == BGBlindOn ) {
								SurfaceWindow( SurfNum ).BlTsolDifDif = InterpSlatAng( SlatAng, VarSlats, Blind( BlNum ).SolFrontDiffDiffTrans );
							} else if ( ShadeFlag == ExtScreenOn ) {
								SurfaceWindow( SurfNum ).ScTsolDifDif = SurfaceScreens( ScNum ).DifDifTrans;
							}

							if ( SunLitFract > 0.0 ) {
								if ( ShadeFlag == ExtScreenOn ) {
									//           beam transmittance (written in subroutine CalcScreenTransmittance each time step)
									TScBmBm = SurfaceScreens( ScNum ).BmBmTrans;
									SurfaceWindow( SurfNum ).ScTsolBmBm = TScBmBm;
								} else {
									TBlBmBm = BlindBeamBeamTrans( ProfAng, SlatAng, Blind( BlNum ).SlatWidth, Blind( BlNum ).SlatSeparation, Blind( BlNum ).SlatThickness );
									SurfaceWindow( SurfNum ).BlTsolBmBm = TBlBmBm;
								}
								if ( ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn ) {
									// Interior or exterior blind
									TBmBmBl = TBmBm * TBlBmBm;
								} else if ( ShadeFlag == ExtScreenOn ) {
									// Exterior screen
									TBmBmSc = TBmBm * TScBmBm;
								} else {
									// Between-glass blind
									if ( NGlass == 2 ) {
										TBmBmBl = t1 * tfshBB * t2;
									} else { // NGlass = 3
										TBmBmBl = t1 * t2 * tfshBB * t3;
									}
								}
								if ( ShadeFlag == ExtScreenOn ) {
									//           Report variable for Beam-to-Beam transmittance
									SurfaceWindow( SurfNum ).ScGlSysTsolBmBm = TBmBmSc;
								} else {
									SurfaceWindow( SurfNum ).BlGlSysTsolBmBm = TBmBmBl;
								}

								if ( ShadeFlag == ExtScreenOn ) {
									TScBmDif = SurfaceScreens( ScNum ).BmDifTrans;
									//           Report variable for Beam-to-Diffuse transmittance (scattered transmittance)
									SurfaceWindow( SurfNum ).ScTsolBmDif = TScBmDif;
								} else {
									TBlBmDif = InterpProfSlatAng( ProfAng, SlatAng, VarSlats, Blind( BlNum ).SolFrontBeamDiffTrans );
									SurfaceWindow( SurfNum ).BlTsolBmDif = TBlBmDif;
									//CR6913     SurfaceWindow(SurfNum)%BlTsolDifDif = InterpSlatAng(SlatAng,VarSlats,Blind(BlNum)%SolFrontDiffDiffTrans)
								}

								//added TH 12/9/2009
								TBmBmShBlSc = 0.0;
								TBmDifShBlSc = 0.0;

								if ( ShadeFlag == IntBlindOn ) {

									// Interior blind on: beam-beam and diffuse transmittance of exterior beam

									TBlDifDif = InterpSlatAng( SlatAng, VarSlats, Blind( BlNum ).SolFrontDiffDiffTrans );
									RhoBlBmDifFr = InterpProfSlatAng( ProfAng, SlatAng, VarSlats, Blind( BlNum ).SolFrontBeamDiffRefl );
									RGlDifBk = Construct( ConstrNum ).ReflectSolDiffBack;
									RhoBlDifDifFr = InterpSlatAng( SlatAng, VarSlats, Blind( BlNum ).SolFrontDiffDiffRefl );
									TBmAllShBlSc = TBmBm * ( TBlBmBm + TBlBmDif + TBlDifDif * RhoBlBmDifFr * RGlDifBk / ( 1 - RhoBlDifDifFr * RGlDifBk ) );

									//added TH 12/9/2009
									TBmBmShBlSc = TBmBmBl; //TBmBm * TBlBmBm
									TBmDifShBlSc = TBmAllShBlSc - TBmBmShBlSc;
									if ( TBmDifShBlSc < 0.0 ) TBmDifShBlSc = 0.0;

								} else if ( ShadeFlag == ExtBlindOn ) {

									// Exterior blind on: beam-beam and diffuse transmittance of exterior beam

									RhoBlBmDifBk = InterpProfSlatAng( ProfAng, SlatAng, VarSlats, Blind( BlNum ).SolBackBeamDiffRefl );
									RGlBmFr = POLYF( CosInc, Construct( ConstrNum ).ReflSolBeamFrontCoef );
									TBmAllShBlSc = TBlBmBm * ( TBmBm + TDifBare * RGlBmFr * RhoBlBmDifBk / ( 1 - RGlDifFr * RhoBlDifDifBk ) ) + TBlBmDif * TDifBare / ( 1 - RGlDifFr * RhoBlDifDifBk );

									//added TH 12/9/2009
									TBmBmShBlSc = TBmBmBl; //TBmBm * TBlBmBm
									TBmDifShBlSc = TBmAllShBlSc - TBmBmShBlSc;

								} else if ( ShadeFlag == ExtScreenOn ) {

									// Exterior screen on: beam-beam and diffuse transmittance of exterior beam

									RScBack = SurfaceScreens( ScNum ).ReflectSolBeamFront;
									RScDifDifBk = SurfaceScreens( ScNum ).DifReflect;
									RGlBmFr = POLYF( CosInc, Construct( ConstrNum ).ReflSolBeamFrontCoef );
									TBmAllShBlSc = TScBmBm * ( TBmBm + RGlBmFr * RScBack * TDifBare / ( 1 - RGlDifFr * RScDifDifBk ) ) + TScBmDif * TDifBare / ( 1 - RGlDifFr * RScDifDifBk );

									//added TH 12/9/2009
									TBmBmShBlSc = TBmBmSc;
									TBmDifShBlSc = TBmAllShBlSc - TBmBmShBlSc;

								} else {
									// Between-glass blind on: beam-beam and diffuse transmittance of exterior beam

									if ( NGlass == 2 ) {
										TBmAllShBlSc = t1 * tfshBB * t2 + t1 * ( tfshBB * rf2 * rbshB + tfshBd * ( 1.0 + rfd2 * rbshd ) + rfshB * rbd1 * rfshd ) * td2;
									} else { // NGlass = 3
										TBmAllShBlSc = t1t2 * tfshBB * t3 + t1t2 * ( tfshBB * rf3 * rbshB + tfshBd * ( 1.0 + rfd3 * rbshd ) + rbshB * ( rbd2 * tfshd + td2 * rbd1 * td2 * tfshd ) ) * td3;
									}

									//added TH 12/9/2009
									TBmBmShBlSc = TBmBmBl;
									TBmDifShBlSc = TBmAllShBlSc - TBmBmShBlSc;

								}
							}

						}

					} // End of check if ShadeFlag > 0 and ShadeFlag < 10
				}

				if ( ShadeFlag == SwitchableGlazing ) {

					// Switchable glazing

					SwitchFac = SurfaceWindow( SurfNum ).SwitchingFactor;
					if ( ! SurfaceWindow( SurfNum ).SolarDiffusing ) {
						TBmBm = InterpSw( SwitchFac, TBmBm, TBmAllShBlSc );
					} else {
						TBmDif = InterpSw( SwitchFac, TBmDif, TBmAllShBlSc );
					}
				}

				// The following WinTransBmSolar and WinTransDifSolar will be combined later to give
				// WinTransSolar for reporting
				if ( SurfaceWindow( SurfNum ).WindowModelType != WindowEQLModel ) {
					WinTransDifSolar( SurfNum ) = DiffTrans * Surface( SurfNum ).Area;
					if ( ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == BGBlindOn ) {
						if ( Blind( SurfaceWindow( SurfNum ).BlindNumber ).SlatOrientation == Horizontal ) {
							WinTransDifSolarGnd( SurfNum ) = DiffTransGnd * Surface( SurfNum ).Area;
							WinTransDifSolarSky( SurfNum ) = DiffTransSky * Surface( SurfNum ).Area;
						}
					}
				} else {
					// In equivalent layer window model system diffuse transmittance is based on unit
					// diffuse radiation flux, and hence doesn't distinguish between sky and
					// ground reflected diffuse radiations
					WinTransDifSolar( SurfNum ) = DiffTrans * Surface( SurfNum ).Area;
					WinTransDifSolarGnd( SurfNum ) = DiffTrans * Surface( SurfNum ).Area;
					WinTransDifSolarSky( SurfNum ) = DiffTrans * Surface( SurfNum ).Area;
				}
				if ( ShadeFlag < 1 || ShadeFlag == SwitchableGlazing || ShadeFlag >= 10 ) { // Unshaded or switchable glazing
					//Note: with previous defs of TBmBm & TBmDif, these come out right for Complex Fenestration
					// WinTransBmSolar uses the directional-hemispherical transmittance
					WinTransBmSolar( SurfNum ) = ( TBmBm + TBmDif ) * SunLitFract * CosInc * Surface( SurfNum ).Area * InOutProjSLFracMult;

					//added TH 12/9/2009
					WinTransBmBmSolar = TBmBm * SunLitFract * CosInc * Surface( SurfNum ).Area * InOutProjSLFracMult; // m2
					WinTransBmDifSolar = TBmDif * SunLitFract * CosInc * Surface( SurfNum ).Area * InOutProjSLFracMult; // m2

				} else {
					WinTransBmSolar( SurfNum ) = TBmAllShBlSc * SunLitFract * CosInc * Surface( SurfNum ).Area * InOutProjSLFracMult;

					//added TH 12/9/2009
					WinTransBmBmSolar = TBmBmShBlSc * SunLitFract * CosInc * Surface( SurfNum ).Area * InOutProjSLFracMult;
					WinTransBmDifSolar = TBmDifShBlSc * SunLitFract * CosInc * Surface( SurfNum ).Area * InOutProjSLFracMult;
				}

				// Add diffuse transmitted by window from beam reflected from outside reveal

				if ( SurfaceWindow( SurfNum ).WindowModelType == WindowBSDFModel ) { //Complex Fenestration
					FenSolAbsPtr = WindowScheduledSolarAbs( SurfNum, ConstrNum );
					if ( FenSolAbsPtr == 0 ) {
						WinTransBmSolar( SurfNum ) += SurfaceWindow( SurfNum ).OutsRevealDiffOntoGlazing * NomDiffTrans * Surface( SurfNum ).Area;

						WinTransBmDifSolar += SurfaceWindow( SurfNum ).OutsRevealDiffOntoGlazing * NomDiffTrans * Surface( SurfNum ).Area;
					} else {
						WinTransBmSolar( SurfNum ) = 0.0;
						WinTransBmDifSolar = 0.0;
					}
				} else { //Regular window
					// this is also valid for equivalent layer window
					WinTransBmSolar( SurfNum ) += SurfaceWindow( SurfNum ).OutsRevealDiffOntoGlazing * DiffTrans * Surface( SurfNum ).Area;

					//added TH 12/9/2009
					WinTransBmDifSolar += SurfaceWindow( SurfNum ).OutsRevealDiffOntoGlazing * DiffTrans * Surface( SurfNum ).Area;
				}

				// Increment factor for total exterior beam solar entering zone through window as beam or diffuse

				if ( SunLitFract > 0.0 && Surface( SurfNum ).Class != SurfaceClass_TDD_Dome ) {

					if ( ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == BGShadeOn || ShadeFlag == BGBlindOn || ShadeFlag == ExtScreenOn ) {
						TBmAll = TBmAllShBlSc;
					} else {
						TBmAll = TBmBm + TBmDif;
					}

					FenSolAbsPtr = WindowScheduledSolarAbs( SurfNum, ConstrNum );

					// Window is schedule surface gained. Do not make addition to what enters into zone since that information is not
					// available
					if ( FenSolAbsPtr == 0 ) {
						BTOTZone += TBmAll * SunLitFract * CosInc * Surface( SurfNum ).Area * InOutProjSLFracMult; // [m2]
					}

				}

				// Correct for effect of (1) beam absorbed by inside reveal, (2) diffuse entering zone from beam
				// reflected by inside reveal and (3) diffuse transmitted by window from beam reflected from
				// outside reveal.
				if ( CosInc > 0.0 ) {
					// old code
					// BTOTZone = BTOTZone + (SurfaceWindow(SurfNum)%InsRevealDiffIntoZone &
					//                       - SurfaceWindow(SurfNum)%BmSolAbsdInsReveal &
					//                       + SurfaceWindow(SurfNum)%OutsRevealDiffOntoGlazing * DiffTrans) * Surface(SurfNum)%Area

					// CR 7596. TH 5/27/2009
					// The BTOTZone is the solar into zone assuming no inside or outside reveals
					// The inside reveals receive solar (reflected part + absorbed part) from the window, this amount should be
					// deducted from the BTOTZone, then adds the InsRevealDiffIntoZone
					if ( SurfaceWindow( SurfNum ).WindowModelType == WindowBSDFModel ) { //Complex Fenestration
						SurfSolIncPtr = SurfaceScheduledSolarInc( SurfNum, ConstrNum );

						// Do not add total into zone from scheduled surface gains.  That will be added later
						if ( SurfSolIncPtr == 0 ) {
							BTOTZone = BTOTZone - SurfaceWindow( SurfNum ).BmSolRefldInsReveal - SurfaceWindow( SurfNum ).BmSolAbsdInsReveal + SurfaceWindow( SurfNum ).InsRevealDiffIntoZone + SurfaceWindow( SurfNum ).OutsRevealDiffOntoGlazing * NomDiffTrans * Surface( SurfNum ).Area;
						}
					} else { //Regular window
						BTOTZone = BTOTZone - SurfaceWindow( SurfNum ).BmSolRefldInsReveal - SurfaceWindow( SurfNum ).BmSolAbsdInsReveal + SurfaceWindow( SurfNum ).InsRevealDiffIntoZone + SurfaceWindow( SurfNum ).OutsRevealDiffOntoGlazing * DiffTrans * Surface( SurfNum ).Area;
					}
					// Add beam solar absorbed by outside reveal to outside of window's base surface.
					// Add beam solar absorbed by inside reveal to inside of window's base surface.
					// This ignores 2-D heat transfer effects.
					BaseSurfNum = Surface( SurfNum ).BaseSurf;
					AISurf( BaseSurfNum ) += SurfaceWindow( SurfNum ).BmSolAbsdInsReveal / Surface( BaseSurfNum ).Area;
					AOSurf( BaseSurfNum ) += SurfaceWindow( SurfNum ).BmSolAbsdOutsReveal / Surface( BaseSurfNum ).Area;
				}

				if ( SunLitFract > 0.0 ) {

					//---------------------------------------------------------------------------------
					// INTERIOR BEAM FROM EXTERIOR WINDOW THAT IS ABSORBED/TRANSMITTED BY BACK SURFACES
					//---------------------------------------------------------------------------------

					// If shade is in place or there is a diffusing glass layer there is no interior beam
					// from this exterior window since the beam-beam transmittance of shades and diffusing glass
					// is assumed to be zero. The beam-beam transmittance of tubular daylighting devices is also
					// assumed to be zero.

					if ( ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == BGShadeOn || SurfaceWindow( SurfNum ).SolarDiffusing || SurfaceWindow( SurfNum ).OriginalClass == SurfaceClass_TDD_Diffuser || Surface( SurfNum ).Class == SurfaceClass_TDD_Dome ) continue;

					// Find interior beam radiation that is:
					// (1) absorbed by opaque back surfaces;
					// (2) absorbed by glass layers of back surfaces that are interior or exterior windows;
					// (3) absorbed by interior, exterior or between-glass shades or blinds of back surfaces
					//       that are exterior windows; and
					// (4) transmitted through back surfaces that are interior or exterior windows.

					// Beam-beam transmittance of exterior window
					if ( ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == BGBlindOn ) {
						TBm = TBmBmBl; // Interior, exterior or between-glass blind on
					} else if ( ShadeFlag == ExtScreenOn ) {
						TBm = TBmBmSc; // Exterior screen on
					} else {
						TBm = TBmBm; // Bare glass or switchable glazing

						// Correction for beam absorbed by inside reveal
						TBmDenom = ( SunLitFract * CosInc * Surface( SurfNum ).Area * InOutProjSLFracMult );
						if ( TBmDenom != 0.0 ) { // when =0.0, no correction
							TBm -= SurfaceWindow( SurfNum ).BmSolAbsdInsReveal / TBmDenom;
						}

						TBm = max( 0.0, TBm );
					}

					if ( TBm == 0.0 ) continue;

					if ( InShelfSurf > 0 ) { // Inside daylighting shelf
						// Inside daylighting shelves assume that no beam will pass the end of the shelf.
						// Since all beam is absorbed on the shelf, this might cause them to get unrealistically hot at times.

						BTOTWinZone = TBm * SunLitFract * Surface( SurfNum ).Area * CosInc * InOutProjSLFracMult; //[m2]

						// Shelf surface area is divided by 2 because only one side sees beam (Area was multiplied by 2 during init)
						AISurf( InShelfSurf ) += BTOTWinZone / ( 0.5 * Surface( InShelfSurf ).Area ); //[-]
						BABSZone += BTOTWinZone; //[m2]

						continue;
					}

					if ( SolarDistribution == FullInteriorExterior ) { // Full interior solar distribution

						if ( SurfaceWindow( SurfNum ).WindowModelType != WindowBSDFModel && SurfaceWindow( SurfNum ).WindowModelType != WindowEQLModel ) {
							// Loop over back surfaces irradiated by beam from this exterior window

							for ( IBack = 1; IBack <= MaxBkSurf; ++IBack ) {

								BackSurfNum = BackSurfaces( TimeStep, HourOfDay, IBack, SurfNum );

								if ( BackSurfNum == 0 ) break; // No more irradiated back surfaces for this exterior window
								ConstrNumBack = Surface( BackSurfNum ).Construction;
								NBackGlass = Construct( ConstrNumBack ).TotGlassLayers;
								// Irradiated (overlap) area for this back surface, projected onto window plane
								// (includes effect of shadowing on exterior window)
								AOverlap = OverlapAreas( TimeStep, HourOfDay, IBack, SurfNum );
								BOverlap = TBm * AOverlap * CosInc; //[m2]

								if ( Construct( ConstrNumBack ).TransDiff <= 0.0 ) {

									// Back surface is opaque interior or exterior wall

									AbsIntSurf = Construct( ConstrNumBack ).InsideAbsorpSolar;

									// Check for movable insulation; reproduce code from subr. EvalInsideMovableInsulation;
									// Can't call that routine here since cycle prevents SolarShadingGeometry from USEing
									// HeatBalanceSurfaceManager, which contains EvalInsideMovableInsulation
									HMovInsul = 0.0;
									if ( Surface( BackSurfNum ).MaterialMovInsulInt > 0 ) {
										MovInsulSchedVal = GetCurrentScheduleValue( Surface( BackSurfNum ).SchedMovInsulExt );
										if ( MovInsulSchedVal <= 0.0 ) { // Movable insulation not present at current time
											HMovInsul = 0.0;
										} else { // Movable insulation present
											HMovInsul = 1.0 / ( MovInsulSchedVal * Material( Surface( BackSurfNum ).MaterialMovInsulInt ).Resistance );
											AbsInt = Material( Surface( BackSurfNum ).MaterialMovInsulInt ).AbsorpSolar;
										}
									}
									if ( HMovInsul > 0.0 ) AbsIntSurf = AbsInt; // Movable inside insulation present

									AISurf( BackSurfNum ) += BOverlap * AbsIntSurf / Surface( BackSurfNum ).Area; //[-]
									BABSZone += BOverlap * AbsIntSurf; //[m2]

								} else {

									// Back surface is an interior or exterior window

									// Note that exterior back windows can have a shading device but interior back windows
									// are assumed to be bare, i.e., they have no shading device and are non-switchable.
									// The layer order for interior windows is "outside" to "inside," where "outside" refers to
									// the adjacent zone and "inside" refers to the current zone.

									ShadeFlagBack = SurfaceWindow( BackSurfNum ).ShadingFlag;
									SlatAngBack = SurfaceWindow( BackSurfNum ).SlatAngThisTS;
									VarSlatsBack = SurfaceWindow( BackSurfNum ).MovableSlats;
									CosIncBack = std::abs( CosIncAng( TimeStep, HourOfDay, BackSurfNum ) );
									if ( SurfaceWindow( SurfNum ).WindowModelType == WindowBSDFModel ) {
										//Transmitting window is complex fen, change the incident angle to one for ray joining
										// transmitting and back window centers
										CosIncBack = std::abs( ComplexWind( SurfNum ).sdotN( IBack ) );
									}
									ConstrNumBackSh = Surface( BackSurfNum ).ShadedConstruction;
									if ( SurfaceWindow( BackSurfNum ).StormWinFlag == 1 ) {
										ConstrNum = Surface( BackSurfNum ).StormWinConstruction;
										ConstrNumSh = Surface( BackSurfNum ).StormWinShadedConstruction;
									}
									AbsBeamWin.dimension( MaxSolidWinLayers, 0.0 );
									TransBeamWin = 0.0;

									// Interior beam absorptance of glass layers and beam transmittance of back exterior  &
									// or interior window WITHOUT SHADING this timestep

									if ( ShadeFlagBack <= 0 ) {
										for ( Lay = 1; Lay <= NBackGlass; ++Lay ) {
											AbsBeamWin( Lay ) = POLYF( CosIncBack, Construct( ConstrNumBack ).AbsBeamBackCoef( {1,6}, Lay ) );
										}
										TransBeamWin = POLYF( CosIncBack, Construct( ConstrNumBack ).TransSolBeamCoef );
									}

									// Interior beam absorptance of glass layers and beam transmittance
									// of back exterior window with SHADE

									if ( ShadeFlagBack == IntShadeOn || ShadeFlagBack == ExtShadeOn || ShadeFlagBack == BGShadeOn ) {
										for ( Lay = 1; Lay <= Construct( ConstrNumBackSh ).TotGlassLayers; ++Lay ) {
											AbsBeamWin( Lay ) = POLYF( CosIncBack, Construct( ConstrNumBackSh ).AbsBeamBackCoef( {1,6}, Lay ) );
										}
										TransBeamWin = POLYF( CosIncBack, Construct( ConstrNumBackSh ).TransSolBeamCoef );

									}

									// Interior beam absorbed by INTERIOR SHADE of back exterior window

									if ( ShadeFlagBack == IntShadeOn ) {
										IntBeamAbsByShadFac( BackSurfNum ) = BOverlap * Construct( ConstrNumBackSh ).AbsDiffBackShade / ( Surface( BackSurfNum ).Area + SurfaceWindow( BackSurfNum ).DividerArea );
										BABSZone += BOverlap * Construct( ConstrNumBackSh ).AbsDiffBackShade;
									}

									// Interior beam absorbed by EXTERIOR SHADE of back exterior window

									if ( ShadeFlagBack == ExtShadeOn ) {
										RGlFront = Construct( ConstrNumBack ).ReflectSolDiffFront;
										AbsSh = Material( Construct( ConstrNumBackSh ).LayerPoint( 1 ) ).AbsorpSolar;
										RhoSh = 1.0 - AbsSh - Material( Construct( ConstrNumBackSh ).LayerPoint( 1 ) ).Trans;
										AShBack = POLYF( CosIncBack, Construct( ConstrNumBack ).TransSolBeamCoef ) * AbsSh / ( 1.0 - RGlFront * RhoSh );
										BABSZone += BOverlap * AShBack;
										IntBeamAbsByShadFac( BackSurfNum ) = BOverlap * AShBack / ( Surface( BackSurfNum ).Area + SurfaceWindow( BackSurfNum ).DividerArea );
									}

									// Interior beam absorbed by BETWEEN-GLASS SHADE of back exterior window

									if ( ShadeFlagBack == BGShadeOn ) {
										rbd1k = Construct( ConstrNumBack ).rbBareSolDiff( 1 );
										if ( NBackGlass == 2 ) {
											t2k = POLYF( CosIncBack, Construct( ConstrNumBack ).tBareSolCoef( {1,6}, 2 ) );
											rfd2k = Construct( ConstrNumBack ).rfBareSolDiff( 2 );
											TrSh = Material( Construct( ConstrNumBackSh ).LayerPoint( 3 ) ).Trans;
											RhoSh = Material( Construct( ConstrNumBackSh ).LayerPoint( 3 ) ).ReflectShade;
											AbsSh = min( 1.0, max( 0.0, 1 - TrSh - RhoSh ) );
											AShBack = t2k * ( 1 + RhoSh * rfd2k + TrSh * rbd1k ) * AbsSh;
										} else { // NBackGlass = 3
											t3k = POLYF( CosIncBack, Construct( ConstrNumBack ).tBareSolCoef( {1,6}, 3 ) );
											TrSh = Material( Construct( ConstrNumBackSh ).LayerPoint( 5 ) ).Trans;
											RhoSh = Material( Construct( ConstrNumBackSh ).LayerPoint( 5 ) ).ReflectShade;
											AbsSh = min( 1.0, max( 0.0, 1 - TrSh - RhoSh ) );
											AShBack = t3k * ( 1 + RhoSh * rfd3k + TrSh * ( rbd2k + td2k * rbd1k * td2k ) ) * AbsSh;
										}
										IntBeamAbsByShadFac( BackSurfNum ) = BOverlap * AShBack / Surface( BackSurfNum ).Area;
										BABSZone += BOverlap * AShBack;
									}

									// Interior beam absorptance of glass layers and beam absorbed in blind
									// of back exterior window with BLIND

									if ( ShadeFlagBack == IntBlindOn || ShadeFlagBack == ExtBlindOn || ShadeFlagBack == BGBlindOn ) {
										BlNumBack = SurfaceWindow( BackSurfNum ).BlindNumber;
										ProfileAngle( BackSurfNum, SOLCOS, Blind( BlNumBack ).SlatOrientation, ProfAngBack );
										TGlBmBack = POLYF( CosIncBack, Construct( ConstrNumBack ).TransSolBeamCoef );
										TBlBmBmBack = BlindBeamBeamTrans( ProfAngBack, Pi - SlatAngBack, Blind( BlNumBack ).SlatWidth, Blind( BlNumBack ).SlatSeparation, Blind( BlNumBack ).SlatThickness );
										TBlBmDiffBack = InterpProfSlatAng( ProfAngBack, SlatAngBack, VarSlatsBack, Blind( BlNumBack ).SolBackBeamDiffTrans );

										if ( ShadeFlagBack == IntBlindOn ) {

											// Interior beam absorptance of GLASS LAYERS of exterior back window with INTERIOR BLIND

											RhoBlFront = InterpProfSlatAng( ProfAngBack, SlatAngBack, VarSlatsBack, Blind( BlNumBack ).SolFrontBeamDiffRefl );
											RhoBlDiffFront = InterpSlatAng( SlatAngBack, VarSlatsBack, Blind( BlNumBack ).SolFrontDiffDiffRefl );
											RGlBack = POLYF( CosIncBack, Construct( ConstrNumBack ).ReflSolBeamBackCoef( {1,6} ) );
											RGlDiffBack = Construct( ConstrNumBack ).ReflectSolDiffBack;
											for ( Lay = 1; Lay <= NBackGlass; ++Lay ) {
												AbWinBack = POLYF( CosIncBack, Construct( ConstrNumBack ).AbsBeamBackCoef( {1,6}, Lay ) );
												AGlDiffBack = Construct( ConstrNumBack ).AbsDiffBack( Lay );
												AbsBeamWin( Lay ) = TBlBmBmBack * AbWinBack + ( ( TBlBmBmBack * RGlBack * RhoBlFront + TBlBmDiffBack ) * AGlDiffBack / ( 1.0 - RGlDiffBack * RhoBlDiffFront ) );
											}

											// Interior beam transmitted by exterior back window with INTERIOR BLIND

											TGlDif = Construct( ConstrNumBack ).TransDiff;
											TransBeamWin = TBlBmBmBack * ( TGlBmBack + TGlDif * RGlBack * RhoBlFront / ( 1.0 - RGlDiffBack * RhoBlDiffFront ) ) + TBlBmDiffBack * TGlDif / ( 1.0 - RGlDiffBack * RhoBlDiffFront );

											// Interior beam absorbed by BLIND on exterior back window with INTERIOR BLIND

											AbsBlFront = InterpProfSlatAng( ProfAngBack, SlatAngBack, VarSlatsBack, Blind( BlNumBack ).SolFrontBeamAbs );
											AbsBlBack = InterpProfSlatAng( ProfAngBack, SlatAngBack, VarSlatsBack, Blind( BlNumBack ).SolBackBeamAbs );
											AbsBlDiffFront = InterpSlatAng( SlatAngBack, VarSlatsBack, Blind( BlNumBack ).SolFrontDiffAbs );
											ABlBack = AbsBlBack + TBlBmBmBack * RGlBack * AbsBlFront + ( AbsBlDiffFront * RGlDiffBack / ( 1 - RhoBlDiffFront * RGlDiffBack ) ) * ( RGlBack * TBlBmBmBack * RhoBlFront + TBlBmDiffBack );
											IntBeamAbsByShadFac( BackSurfNum ) = BOverlap * ABlBack / ( Surface( BackSurfNum ).Area + SurfaceWindow( BackSurfNum ).DividerArea );
											BABSZone += BOverlap * ABlBack;
										}

										if ( ShadeFlagBack == ExtBlindOn ) {

											// Interior beam absorptance of GLASS LAYERS of exterior back window with EXTERIOR BLIND

											RGlDiffFront = Construct( ConstrNumBack ).ReflectSolDiffFront;
											RhoBlBack = InterpProfSlatAng( ProfAngBack, SlatAngBack, VarSlatsBack, Blind( BlNumBack ).SolBackBeamDiffRefl );
											for ( Lay = 1; Lay <= NBackGlass; ++Lay ) {
												AbWinBack = POLYF( CosIncBack, Construct( ConstrNumBack ).AbsBeamBackCoef( {1,6}, Lay ) );
												AGlDiffFront = Construct( ConstrNumBack ).AbsDiff( Lay );
												AbsBeamWin( Lay ) = AbWinBack + ( TGlBmBack * AGlDiffFront * RhoBlBack / ( 1.0 - RhoBlBack * RGlDiffFront ) );
											}

											// Interior beam transmitted by exterior back window with EXTERIOR BLIND

											TBlDifDif = InterpSlatAng( SlatAngBack, VarSlatsBack, Blind( BlNumBack ).SolBackDiffDiffTrans );
											RhoBlBmDifBk = InterpProfSlatAng( ProfAngBack, SlatAngBack, VarSlatsBack, Blind( BlNumBack ).SolBackBeamDiffRefl );
											RGlDifFr = Construct( ConstrNum ).ReflectSolDiffFront;
											RhoBlDifDifBk = InterpSlatAng( SlatAngBack, VarSlatsBack, Blind( BlNumBack ).SolBackDiffDiffRefl );
											TransBeamWin = TGlBmBack * ( TBlBmBmBack + TBlBmDiffBack + TBlDifDif * RhoBlBmDifBk * RGlDifFr / ( 1.0 - RhoBlDifDifBk * RGlDifFr ) );

											// Interior beam absorbed by EXTERIOR BLIND on exterior back window

											AbsBlBack = InterpProfSlatAng( ProfAngBack, SlatAngBack, VarSlatsBack, Blind( BlNumBack ).SolBackBeamAbs );
											AbsBlDiffBack = InterpSlatAng( SlatAngBack, VarSlatsBack, Blind( BlNumBack ).SolBackDiffAbs );
											RhoBlDiffBack = InterpSlatAng( SlatAngBack, VarSlatsBack, Blind( BlNumBack ).SolBackDiffDiffRefl );
											ABlBack = TGlBmBack * ( AbsBlBack + RhoBlBack * RGlDiffFront * AbsBlDiffBack / ( 1 - RhoBlDiffBack * RGlDiffFront ) );
											BABSZone += BOverlap * ABlBack;
											IntBeamAbsByShadFac( BackSurfNum ) = BOverlap * ABlBack / ( Surface( BackSurfNum ).Area + SurfaceWindow( BackSurfNum ).DividerArea );
										} // End of check if exterior blind on back window

										if ( ShadeFlagBack == BGBlindOn ) {

											t1k = POLYF( CosIncBack, Construct( ConstrNumBack ).tBareSolCoef( {1,6}, 1 ) );
											t2k = POLYF( CosIncBack, Construct( ConstrNumBack ).tBareSolCoef( {1,6}, 2 ) );
											af2k = POLYF( CosIncBack, Construct( ConstrNumBack ).afBareSolCoef( {1,6}, 2 ) );
											ab1k = POLYF( CosIncBack, Construct( ConstrNumBack ).abBareSolCoef( {1,6}, 1 ) );
											ab2k = POLYF( CosIncBack, Construct( ConstrNumBack ).abBareSolCoef( {1,6}, 2 ) );
											rb1k = POLYF( CosIncBack, Construct( ConstrNumBack ).rbBareSolCoef( {1,6}, 1 ) );
											rb2k = POLYF( CosIncBack, Construct( ConstrNumBack ).rbBareSolCoef( {1,6}, 2 ) );
											td1k = Construct( ConstrNumBack ).tBareSolDiff( 1 );
											td2k = Construct( ConstrNumBack ).tBareSolDiff( 2 );
											afd2k = Construct( ConstrNumBack ).afBareSolDiff( 2 );
											abd1k = Construct( ConstrNumBack ).abBareSolDiff( 1 );
											abd2k = Construct( ConstrNumBack ).abBareSolDiff( 2 );
											rfd2k = Construct( ConstrNumBack ).rfBareSolDiff( 2 );
											rbd1k = Construct( ConstrNumBack ).rbBareSolDiff( 1 );
											rbd2k = Construct( ConstrNumBack ).rbBareSolDiff( 2 );
											tfshBBk = BlindBeamBeamTrans( ProfAngBack, SlatAngBack, Blind( BlNumBack ).SlatWidth, Blind( BlNumBack ).SlatSeparation, Blind( BlNumBack ).SlatThickness );
											tfshBdk = InterpProfSlatAng( ProfAngBack, SlatAngBack, VarSlatsBack, Blind( BlNumBack ).SolFrontBeamDiffTrans );
											tfshdk = InterpSlatAng( SlatAngBack, VarSlatsBack, Blind( BlNumBack ).SolFrontDiffDiffTrans );
											tbshBBk = BlindBeamBeamTrans( ProfAngBack, Pi - SlatAngBack, Blind( BlNumBack ).SlatWidth, Blind( BlNumBack ).SlatSeparation, Blind( BlNumBack ).SlatThickness );
											tbshBdk = InterpProfSlatAng( ProfAngBack, SlatAngBack, VarSlatsBack, Blind( BlNumBack ).SolBackBeamDiffTrans );
											tbshdk = InterpSlatAng( SlatAngBack, VarSlatsBack, Blind( BlNumBack ).SolBackDiffDiffTrans );
											rfshBk = InterpProfSlatAng( ProfAngBack, SlatAngBack, VarSlatsBack, Blind( BlNumBack ).SolFrontBeamDiffRefl );
											rbshBk = InterpProfSlatAng( ProfAngBack, SlatAngBack, VarSlatsBack, Blind( BlNumBack ).SolBackBeamDiffRefl );
											rfshdk = InterpSlatAng( SlatAngBack, VarSlatsBack, Blind( BlNumBack ).SolFrontDiffDiffRefl );
											rbshdk = InterpSlatAng( SlatAngBack, VarSlatsBack, Blind( BlNumBack ).SolBackDiffDiffRefl );
											afshdk = InterpSlatAng( SlatAngBack, VarSlatsBack, Blind( BlNumBack ).SolFrontDiffAbs );
											abshdk = InterpSlatAng( SlatAngBack, VarSlatsBack, Blind( BlNumBack ).SolBackDiffAbs );
											afshBk = InterpProfSlatAng( ProfAngBack, SlatAngBack, VarSlatsBack, Blind( BlNumBack ).SolFrontBeamAbs );
											abshBk = InterpProfSlatAng( ProfAngBack, SlatAngBack, VarSlatsBack, Blind( BlNumBack ).SolBackBeamAbs );

											if ( NBackGlass == 3 ) {
												t3k = POLYF( CosIncBack, Construct( ConstrNumBack ).tBareSolCoef( {1,6}, 3 ) );
												af3k = POLYF( CosIncBack, Construct( ConstrNumBack ).afBareSolCoef( {1,6}, 3 ) );
												ab3k = POLYF( CosIncBack, Construct( ConstrNumBack ).abBareSolCoef( {1,6}, 3 ) );
												afd3k = Construct( ConstrNumBack ).afBareSolDiff( 3 );
												rfd3k = Construct( ConstrNumBack ).rfBareSolDiff( 3 );
											}

											// Interior beam absorptance of GLASS LAYERS of exterior back window with BETWEEN-GLASS BLIND

											if ( NBackGlass == 2 ) {
												AbsBeamWin( 2 ) = ab2k + t2k * tbshBBk * rb1k * tfshBBk * af2k + t2k * ( tbshBBk * rb1k * tfshBdk + tbshBdk * rbd1k * tfshdk + rbshBk * ( 1.0 + rfd2k * rbshdk ) ) * afd2k;
												AbsBeamWin( 1 ) = t2k * tbshBBk * ab1k + t2k * ( rbshBk * rfd2k * tbshdk + tbshBdk * ( 1.0 + rbd1k * rfshdk ) ) * abd1k;
											} else { // NBackGlass = 3
												AbsBeamWin( 3 ) = ab3k + t3k * tbshBBk * ( rb2k + t2k * rb1k * t2k ) * tfshBBk * af3k + t3k * ( tbshBdk * rbd2k * tfshdk + tbshBdk * td2k * rbd1k * td2k * tfshdk + rbshBk * ( 1.0 + rfd3k * rbshdk ) ) * afd3k;
												AbsBeamWin( 2 ) = t3k * tbshBBk * ( ab2k + t2k * rb1k * ( af2k + t2k * rfshBk * abd2k ) ) + t3k * ( tbshBdk + tbshBdk * ( rbd2k + td2k * rbd1k * td2k ) * rfshdk + rbshBk * rfd3k * tbshdk ) * abd2k + t3k * tbshBdk * td2k * rbd1k * afd2k;
												AbsBeamWin( 1 ) = t3k * tbshBBk * ( t2k * ab1k + ( rb2k + t2k * rb1k * t2k ) * rfshBk * td2k * abd1k ) + t3k * ( rbshBk * rfd3k * tbshdk + tbshBdk * ( 1.0 + rbd2k * rfshdk + td2k * rbd2k * td2k * rfshdk ) ) * td2k * abd1k;
											}

											// Interior beam transmitted by exterior back window with BETWEEN-GLASS BLIND

											if ( NBackGlass == 2 ) {
												TransBeamWin = t2k * tbshBBk * t1k + t2k * ( tbshBBk * rb1k * rfshBk + rbshBk * rfd2k * tbshdk + tbshBdk * ( 1.0 + rbd1k * rfshdk ) ) * td1k;
											} else { // NGlass = 3
												TransBeamWin = t3k * tbshBBk * t2k * t1k + t3k * ( tbshBBk * ( rb2k * rfshBk + t2k * rb1k * t2k * rfshBk ) + rbshBk * rfd3k * tbshdk + tbshBdk * ( 1.0 + rbd2k * rfshdk + td2k * rbd1k * td2k * rfshdk ) ) * td2k * td1k;
											}

											// Interior beam absorbed by BLIND on exterior back window with BETWEEN-GLASS BLIND

											if ( NBackGlass == 2 ) {
												ABlBack = t2k * ( abshBk + tbshBBk * rb1k * afshBk + rbshBk * rfd2k * abshdk + tbshBdk * rbd1k * afshdk );
											} else { // NBackGlass = 3
												ABlBack = t3k * abshBk + t3k * tbshBBk * ( rb2k + t2k * rb1k * t2k ) * afshBk + t3k * rbshBk * rfd3k * abshdk + t3k * tbshBdk * ( rbd2k + td2k * rbd1k * td2k ) * afshdk;
											}

											BABSZone += BOverlap * ABlBack;
											IntBeamAbsByShadFac( BackSurfNum ) = BOverlap * ABlBack / Surface( BackSurfNum ).Area;

										} // End of check if between-glass blind is on back window

									} // End of check if blind is on back window

									if ( ShadeFlagBack == ExtScreenOn ) {

										// Interior beam absorptance of GLASS LAYERS of exterior back window with EXTERIOR SCREEN
										ScNumBack = SurfaceWindow( BackSurfNum ).ScreenNumber;
										TGlBmBack = POLYF( CosIncBack, Construct( ConstrNumBack ).TransSolBeamCoef );
										RGlDiffFront = Construct( ConstrNumBack ).ReflectSolDiffFront;
										TScBmBmBack = SurfaceScreens( ScNumBack ).BmBmTransBack;
										TScBmDiffBack = SurfaceScreens( ScNumBack ).BmDifTransBack;
										RScBack = SurfaceScreens( ScNumBack ).ReflectSolBeamFront;
										RScDifBack = SurfaceScreens( ScNumBack ).DifReflect;
										for ( Lay = 1; Lay <= NBackGlass; ++Lay ) {
											AbWinBack = POLYF( CosIncBack, Construct( ConstrNumBack ).AbsBeamBackCoef( {1,6}, Lay ) );
											AGlDiffFront = Construct( ConstrNumBack ).AbsDiff( Lay );
											AbsBeamWin( Lay ) = AbWinBack + ( TGlBmBack * AGlDiffFront * RScBack / ( 1.0 - RScDifBack * RGlDiffFront ) );
										}

										// Interior beam transmitted by exterior back window with EXTERIOR SCREEN

										TScDifDif = SurfaceScreens( ScNumBack ).DifDifTrans;
										RScBmDifBk = SurfaceScreens( ScNumBack ).ReflectSolBeamBack;
										RGlDifFr = Construct( ConstrNum ).ReflectSolDiffFront;
										RScDifDifBk = SurfaceScreens( ScNumBack ).DifReflect;
										TransBeamWin = TGlBmBack * ( TScBmBmBack + TScBmDiffBack + TScDifDif * RScBmDifBk * RGlDifFr / ( 1.0 - RScDifDifBk * RGlDifFr ) );

										// Interior beam absorbed by EXTERIOR SCREEN on exterior back window

										AbsScBack = SurfaceScreens( ScNumBack ).AbsorpSolarBeamBack;
										AbsScDiffBack = SurfaceScreens( ScNumBack ).DifScreenAbsorp;
										RScDiffBack = SurfaceScreens( ScNumBack ).ReflectSolBeamFront;
										AScBack = TGlBmBack * ( AbsScBack + RScBack * RGlDiffFront * AbsScDiffBack / ( 1.0 - RScDiffBack * RGlDiffFront ) );
										BABSZone += BOverlap * AScBack;
										IntBeamAbsByShadFac( BackSurfNum ) = BOverlap * AScBack / ( Surface( BackSurfNum ).Area + SurfaceWindow( BackSurfNum ).DividerArea );
									} // End of check if exterior screen on back window

									// Interior beam absorptance of glass layers of back exterior window with SWITCHABLE GLAZING

									if ( ShadeFlagBack == SwitchableGlazing && Surface( BackSurfNum ).ExtBoundCond == 0 ) {

										SwitchFacBack = SurfaceWindow( BackSurfNum ).SwitchingFactor;
										for ( Lay = 1; Lay <= NBackGlass; ++Lay ) {
											AbsBeamWinSh = POLYF( CosIncBack, Construct( ConstrNumBackSh ).AbsBeamBackCoef( {1,6}, Lay ) );
											AbsBeamWin( Lay ) = InterpSw( SwitchFac, AbsBeamWin( Lay ), AbsBeamWinSh );
										}
										TransBeamWinSh = POLYF( CosIncBack, Construct( ConstrNumBackSh ).TransSolBeamCoef );
										TransBeamWin = InterpSw( SwitchFac, TransBeamWin, TransBeamWinSh );
									}

									// Sum of interior beam absorbed by all glass layers of back window

									AbsBeamTotWin = 0.0;
									for ( Lay = 1; Lay <= NBackGlass; ++Lay ) {
										AbsBeamTotWin += AbsBeamWin( Lay );
										AWinSurf( Lay, BackSurfNum ) += BOverlap * AbsBeamWin( Lay ) / ( Surface( BackSurfNum ).Area + SurfaceWindow( BackSurfNum ).DividerArea ); //[-]
									}

									// To BABSZon, add interior beam glass absorption and overall beam transmission for this back window

									BABSZone += BOverlap * ( AbsBeamTotWin + TransBeamWin );

									// Interior beam transmitted to adjacent zone through an interior back window (assumed unshaded);
									// this beam radiation is categorized as diffuse radiation in the adjacent zone.

									AdjSurfNum = Surface( BackSurfNum ).ExtBoundCond;
									if ( AdjSurfNum > 0 ) {
										AdjZoneNum = Surface( AdjSurfNum ).Zone;
										DBZoneIntWin( AdjZoneNum ) += BOverlap * TransBeamWin; //[m2]
										SurfaceWindow( BackSurfNum ).BmSolTransThruIntWinRep += BOverlap * TransBeamWin * BeamSolarRad; //[W]
										SurfaceWindow( BackSurfNum ).BmSolTransThruIntWinRepEnergy = SurfaceWindow( BackSurfNum ).BmSolTransThruIntWinRep * TimeStepZoneSec;
									}
								} // End of check if back surface is opaque or window
								BmIncInsSurfAmountRep( BackSurfNum ) += BOverlap;
								BmIncInsSurfAmountRepEnergy( BackSurfNum ) = BmIncInsSurfAmountRep( BackSurfNum ) * TimeStepZoneSec;
							} // End of loop over back surfaces
						} else if ( SurfaceWindow( SurfNum ).WindowModelType == WindowBSDFModel ) {
							// For complex window calculation goes over outgoing basis directions
							// for current state
							CurCplxFenState = SurfaceWindow( SurfNum ).ComplexFen.CurrentState;

							// Get construction number which keeps transmittance properties
							IConst = SurfaceWindow( SurfNum ).ComplexFen.State( CurCplxFenState ).Konst;

							FenSolAbsPtr = WindowScheduledSolarAbs( SurfNum, IConst );

							// Solar radiation from this window will be calculated only in case when this window is not scheduled surface gained
							if ( FenSolAbsPtr == 0 ) {
								// Current incoming direction number (Sun direction)
								IBm = ComplexWind( SurfNum ).Geom( CurCplxFenState ).SolBmIndex( HourOfDay, TimeStep );

								// Report variables for complex fenestration here
								BSDFBeamDirectionRep( SurfNum ) = IBm;
								BSDFBeamThetaRep( SurfNum ) = ComplexWind( SurfNum ).Geom( CurCplxFenState ).ThetaBm( HourOfDay, TimeStep );
								BSDFBeamPhiRep( SurfNum ) = ComplexWind( SurfNum ).Geom( CurCplxFenState ).PhiBm( HourOfDay, TimeStep );

								BaseSurf = Surface( SurfNum ).BaseSurf;
								// Get total number of back surfaces for current window (surface)
								// Note that it is organized by base surface
								NBkSurf = ShadowComb( BaseSurf ).NumBackSurf;

								if ( ! allocated( CFBoverlap ) ) {
									CFBoverlap.allocate( NBkSurf );
								}

								if ( ! allocated( CFDirBoverlap ) ) {
									CFDirBoverlap.allocate( NBkSurf, ComplexWind( SurfNum ).Geom( CurCplxFenState ).Trn.NBasis );
								}

								CFBoverlap = 0.0;
								// delete values from previous timestep
								AWinCFOverlap = 0.0;

								// Calculate effects on all back surfaces for each of basis directions.  Each of basis directions from the back of the
								// window has to be considered as beam and therefore calcualte CFBoverlap for each of them
								for ( CurTrnDir = 1; CurTrnDir <= ComplexWind( SurfNum ).Geom( CurCplxFenState ).Trn.NBasis; ++CurTrnDir ) {
									CurLambda = ComplexWind( SurfNum ).Geom( CurCplxFenState ).Trn.Lamda( CurTrnDir );
									DirTrans = Construct( IConst ).BSDFInput.SolFrtTrans( IBm, CurTrnDir );
									// Now calculate effect of this direction on all back surfaces
									for ( IBack = 1; IBack <= NBkSurf; ++IBack ) {
										CFDirBoverlap( IBack, CurTrnDir ) = ComplexWind( SurfNum ).Geom( CurCplxFenState ).AOverlap( IBack, CurTrnDir ) * DirTrans * CurLambda * CosInc;
										CFBoverlap( IBack ) += CFDirBoverlap( IBack, CurTrnDir );
									} // DO IBack = 1,MaxBkSurf
								}

								// Summarizing results
								for ( IBack = 1; IBack <= NBkSurf; ++IBack ) {
									BackSurfaceNumber = ShadowComb( BaseSurf ).BackSurf( IBack );
									ConstrNumBack = Surface( BackSurfaceNumber ).Construction;

									// Do not perform any calculation if surface is scheduled for incoming solar radiation
									SurfSolIncPtr = SurfaceScheduledSolarInc( BackSurfaceNumber, ConstrNumBack );

									if ( SurfSolIncPtr == 0 ) {
										// Surface hit is another complex fenestration
										if ( SurfaceWindow( BackSurfaceNumber ).WindowModelType == WindowBSDFModel ) {
											CurBackState = SurfaceWindow( BackSurfaceNumber ).ComplexFen.CurrentState;

											// Do not take into account this window if it is scheduled for surface gains
											FenSolAbsPtr = WindowScheduledSolarAbs( BackSurfaceNumber, ConstrNumBack );

											if ( FenSolAbsPtr == 0 ) {
												// Calculate energy loss per each outgoing orientation
												for ( CurTrnDir = 1; CurTrnDir <= ComplexWind( SurfNum ).Geom( CurCplxFenState ).Trn.NBasis; ++CurTrnDir ) {
													for ( CurBackDir = 1; CurBackDir <= ComplexWind( BackSurfaceNumber ).Geom( CurBackState ).Trn.NBasis; ++CurBackDir ) {
														// Purpose of this part is to find best match for outgoing beam number of window back surface and incoming beam
														// number of complex fenestration which this beam will hit on (back surface again)
														curDot = dot( ComplexWind( SurfNum ).Geom( CurCplxFenState ).sTrn( CurTrnDir ), ComplexWind( BackSurfaceNumber ).Geom( CurBackState ).sTrn( CurBackDir ) );
														if ( CurBackDir == 1 ) {
															bestDot = curDot;
															bestTrn = CurTrnDir;
															bestBackTrn = CurBackDir;
														} else {
															if ( curDot < bestDot ) {
																bestDot = curDot;
																bestTrn = CurTrnDir;
																bestBackTrn = CurBackDir;
															}
														}
													}
													// CurLambda = ComplexWind(BackSurfaceNumber)%Geom(CurBackState)%Trn%Lamda(CurTrnDir)
													// Add influence of this exact direction to what stays in the zone.  It is important to note that
													// this needs to be done for each outgoing direction
													BABSZone += CFDirBoverlap( IBack, CurTrnDir ) * ( 1 - SurfaceWindow( BackSurfaceNumber ).ComplexFen.State( CurBackState ).IntegratedBkRefl( bestBackTrn ) );

													// Absorptance from current back direction
													TotSolidLay = Construct( ConstrNumBack ).TotSolidLayers;
													for ( Lay = 1; Lay <= TotSolidLay; ++Lay ) {
														//IF (ALLOCATED(Construct(ConstrNumBack)%BSDFInput)) THEN
														// CFDirBoverlap is energy transmitted for current basis beam.  It is important to note that AWinOverlap array
														// needs to contain flux and not absorbed energy because later in the code this will be multiplied with window
														// area
														AWinCFOverlap( Lay, BackSurfaceNumber ) += Construct( ConstrNumBack ).BSDFInput.Layer( Lay ).BkAbs( bestBackTrn, 1 ) * CFDirBoverlap( IBack, CurTrnDir ) / Surface( BackSurfaceNumber ).Area;
														//END IF
													}

													// Interior beam transmitted to adjacent zone through an interior back window;
													// This beam radiation is categorized as diffuse radiation in the adjacent zone.
													// Note that this is done for each outgoing direction of exterior window

													AdjSurfNum = Surface( BackSurfaceNumber ).ExtBoundCond;
													if ( AdjSurfNum > 0 ) {
														AdjZoneNum = Surface( AdjSurfNum ).Zone;
														DBZoneIntWin( AdjZoneNum ) += CFDirBoverlap( IBack, CurTrnDir ) * SurfaceWindow( BackSurfaceNumber ).ComplexFen.State( CurBackState ).IntegratedBkTrans( bestBackTrn );
														SurfaceWindow( BackSurfaceNumber ).BmSolTransThruIntWinRep += CFDirBoverlap( IBack, CurTrnDir ) * SurfaceWindow( BackSurfaceNumber ).ComplexFen.State( CurBackState ).IntegratedBkTrans( bestBackTrn ) * BeamSolarRad; //[W]
														SurfaceWindow( BackSurfaceNumber ).BmSolTransThruIntWinRepEnergy = SurfaceWindow( BackSurfaceNumber ).BmSolTransThruIntWinRep * TimeStepZoneSec;
													}
												}
											}
										} else {
											if ( Construct( ConstrNumBack ).TransDiff <= 0.0 ) {
												// Do not take into account this window if it is scheduled for surface gains
												SurfSolIncPtr = SurfaceScheduledSolarInc( BackSurfaceNumber, ConstrNumBack );

												if ( SurfSolIncPtr == 0 ) {
													AbsIntSurf = Construct( ConstrNumBack ).InsideAbsorpSolar;
													AISurf( BackSurfaceNumber ) += CFBoverlap( IBack ) * AbsIntSurf / Surface( BackSurfaceNumber ).Area;
													BABSZone += CFBoverlap( IBack ) * AbsIntSurf;
												}
											} else {
												// Code for mixed windows goes here.  It is same as above code for "ordinary" windows.
												// Try to do something which will not produce duplicate code.
											}
										}
									}
								}

								if ( allocated( CFBoverlap ) ) CFBoverlap.deallocate();
								if ( allocated( CFDirBoverlap ) ) CFDirBoverlap.deallocate();
							}

						} else if ( SurfaceWindow( SurfNum ).WindowModelType == WindowEQLModel ) {

							for ( IBack = 1; IBack <= MaxBkSurf; ++IBack ) {

								BackSurfNum = BackSurfaces( TimeStep, HourOfDay, IBack, SurfNum );

								if ( BackSurfNum == 0 ) break; // No more irradiated back surfaces for this exterior window
								if ( SurfaceWindow( IBack ).WindowModelType != WindowEQLModel ) continue; // only EQL back window is allowed

								ConstrNumBack = Surface( BackSurfNum ).Construction;
								NBackGlass = Construct( ConstrNumBack ).TotGlassLayers;
								// Irradiated (overlap) area for this back surface, projected onto window plane
								// (includes effect of shadowing on exterior window)
								AOverlap = OverlapAreas( TimeStep, HourOfDay, IBack, SurfNum );
								BOverlap = TBm * AOverlap * CosInc; //[m2]

								if ( Construct( ConstrNumBack ).TransDiff <= 0.0 ) {

									// Back surface is opaque interior or exterior wall

									AbsIntSurf = Construct( ConstrNumBack ).InsideAbsorpSolar;

									// Check for movable insulation; reproduce code from subr. EvalInsideMovableInsulation;
									// Can't call that routine here since cycle prevents SolarShadingGeometry from USEing
									// HeatBalanceSurfaceManager, which contains EvalInsideMovableInsulation
									HMovInsul = 0.0;
									if ( Surface( BackSurfNum ).MaterialMovInsulInt > 0 ) {
										MovInsulSchedVal = GetCurrentScheduleValue( Surface( BackSurfNum ).SchedMovInsulExt );
										if ( MovInsulSchedVal <= 0.0 ) { // Movable insulation not present at current time
											HMovInsul = 0.0;
										} else { // Movable insulation present
											HMovInsul = 1.0 / ( MovInsulSchedVal * Material( Surface( BackSurfNum ).MaterialMovInsulInt ).Resistance );
											AbsInt = Material( Surface( BackSurfNum ).MaterialMovInsulInt ).AbsorpSolar;
										}
									}
									if ( HMovInsul > 0.0 ) AbsIntSurf = AbsInt; // Movable inside insulation present

									AISurf( BackSurfNum ) += BOverlap * AbsIntSurf / Surface( BackSurfNum ).Area; //[-]
									BABSZone += BOverlap * AbsIntSurf; //[m2]

								} else {

									// Back surface is an interior or exterior window
									// Note that exterior back windows with and without shades are treated as defined.
									// Equivalent Layer window model has no distinction when treating windows with and
									// without shades (interior, inbetween and exterior shades)

									CosIncBack = std::abs( CosIncAng( TimeStep, HourOfDay, BackSurfNum ) );
									//  Note in equivalent layer window model if storm window exists it is defined as part of
									//  window construction, hence it does not require a separate treatment
									AbsBeamWinEQL = 0.0;
									TransBeamWin = 0.0;

									// Interior beam absorptance of glass layers and beam transmittance of back exterior  &
									// or interior window (treates windows with/without shades as defined) for this timestep

									// call the ASHWAT fenestration model for beam radiation here
									CalcEQLOpticalProperty( BackSurfNum, isBEAM, AbsSolBeamBackEQL );

									EQLNum = Construct( ConstrNumBack ).EQLConsPtr;
									AbsBeamWinEQL( {1,CFS( EQLNum ).NL} ) = AbsSolBeamBackEQL( 1, {1,CFS( EQLNum ).NL} );
									// get the interior beam transmitted through back exterior or interior EQL window
									TransBeamWin = AbsSolBeamBackEQL( 1, CFS( EQLNum ).NL + 1 );
									//   Absorbed by the interior shade layer of back exterior window
									if ( CFS( EQLNum ).L( CFS( EQLNum ).NL ).LTYPE != ltyGLAZE ) {
										IntBeamAbsByShadFac( BackSurfNum ) = BOverlap * AbsSolBeamBackEQL( 1, CFS( EQLNum ).NL ) / ( Surface( BackSurfNum ).Area + SurfaceWindow( BackSurfNum ).DividerArea );
										BABSZone += BOverlap * AbsSolBeamBackEQL( 1, CFS( EQLNum ).NL );
									}
									//   Absorbed by the exterior shade layer of back exterior window
									if ( CFS( EQLNum ).L( 1 ).LTYPE != ltyGLAZE ) {
										IntBeamAbsByShadFac( BackSurfNum ) = BOverlap * AbsSolBeamBackEQL( 1, 1 ) / ( Surface( BackSurfNum ).Area + SurfaceWindow( BackSurfNum ).DividerArea );
										BABSZone += BOverlap * AbsSolBeamBackEQL( 1, 1 );
									}

									// determine the number of glass layers
									NBackGlass = 0;
									for ( Lay = 1; Lay <= CFS( EQLNum ).NL; ++Lay ) {
										if ( CFS( EQLNum ).L( Lay ).LTYPE != ltyGLAZE ) continue;
										++NBackGlass;
									}
									if ( NBackGlass >= 2 ) {
										// If the number of glass is greater than 2, in between glass shade can be present
										for ( Lay = 2; Lay <= CFS( EQLNum ).NL - 1; ++Lay ) {
											if ( CFS( EQLNum ).L( CFS( EQLNum ).NL ).LTYPE != ltyGLAZE ) {
												// if there is in between shade glass determine the shade absorptance
												IntBeamAbsByShadFac( BackSurfNum ) += BOverlap * AbsSolBeamBackEQL( 1, Lay ) / Surface( BackSurfNum ).Area;
												BABSZone += BOverlap * AbsSolBeamBackEQL( 1, Lay );
											}
										}
									}
									// Sum of interior beam absorbed by all glass layers of back window
									AbsBeamTotWin = 0.0;
									for ( Lay = 1; Lay <= CFS( EQLNum ).NL; ++Lay ) {
										AbsBeamTotWin += AbsBeamWinEQL( Lay );
										AWinSurf( Lay, BackSurfNum ) += BOverlap * AbsBeamWinEQL( Lay ) / ( Surface( BackSurfNum ).Area + SurfaceWindow( BackSurfNum ).DividerArea ); //[-]
									}

									// To BABSZon, add interior beam glass absorption and overall beam transmission for this back window

									BABSZone += BOverlap * ( AbsBeamTotWin + TransBeamWin );

									// Interior beam transmitted to adjacent zone through an interior back window (assumed unshaded);
									// this beam radiation is categorized as diffuse radiation in the adjacent zone.

									AdjSurfNum = Surface( BackSurfNum ).ExtBoundCond;
									if ( AdjSurfNum > 0 ) {
										AdjZoneNum = Surface( AdjSurfNum ).Zone;
										DBZoneIntWin( AdjZoneNum ) += BOverlap * TransBeamWin; //[m2]
										SurfaceWindow( BackSurfNum ).BmSolTransThruIntWinRep += BOverlap * TransBeamWin * BeamSolarRad; //[W]
										SurfaceWindow( BackSurfNum ).BmSolTransThruIntWinRepEnergy = SurfaceWindow( BackSurfNum ).BmSolTransThruIntWinRep * TimeStepZoneSec;
									}
								} // End of check if back surface is opaque or window
								BmIncInsSurfAmountRep( BackSurfNum ) += BOverlap;
								BmIncInsSurfAmountRepEnergy( BackSurfNum ) = BmIncInsSurfAmountRep( BackSurfNum ) * TimeStepZoneSec;
							} // End of loop over back surfaces

							//  *****************************

						} // IF (SurfaceWindow(SurfNum)%WindowModelType /= WindowBSDFModel) THEN
					} else { // Simple interior solar distribution. All beam from exterior windows falls on floor;
						// some of this is absorbed/transmitted, rest is reflected to other surfaces.

						for ( FloorNum = Zone( ZoneNum ).SurfaceFirst; FloorNum <= Zone( ZoneNum ).SurfaceLast; ++FloorNum ) {
							// In following, ISABSF is zero except for nominal floor surfaces
							if ( ! Surface( FloorNum ).HeatTransSurf ) continue;
							if ( ISABSF( FloorNum ) <= 0.0 || FloorNum == SurfNum ) continue; // Keep only floor surfaces
							FlConstrNum = Surface( FloorNum ).Construction;

							BTOTWinZone = TBm * SunLitFract * Surface( SurfNum ).Area * CosInc * InOutProjSLFracMult; //[m2]

							if ( Construct( FlConstrNum ).TransDiff <= 0.0 ) {
								// Opaque surface

								AISurf( FloorNum ) += BTOTWinZone * ISABSF( FloorNum ) / Surface( FloorNum ).Area; //[-]
							} else {
								// Window

								// Note that diffuse solar absorptance is used here for floor windows even though we're
								// dealing with incident beam radiation. This is because, for this simple interior distribution,
								// the beam radiation from exterior windows is assumed to be uniformly distributed over the
								// floor and so it makes no sense to use directional absorptances. Note also that floor windows
								// are assumed to not have blinds or shades in this calculation.
								// For the case of the floor window a complex fenestration (strange situation) the correct back
								// diffuse layer absorptions have already been put into the construction
								if ( SurfaceWindow( FloorNum ).StormWinFlag == 1 ) FlConstrNum = Surface( FloorNum ).StormWinConstruction;
								AbsBeamTotWin = 0.0;
								for ( Lay = 1; Lay <= Construct( FlConstrNum ).TotGlassLayers; ++Lay ) {
									AbsBeamTotWin += Construct( FlConstrNum ).AbsDiffBack( Lay );
								}
								// In the following we have to multiply by the AbsDiffBack(Lay)/AbsBeamTotWin ratio to get the
								// layer by layer absorbed beam since ISABSF(FloorNum) is proportional to AbsBeamTotWin
								// (see ComputeIntSolarAbsorpFactors).
								for ( Lay = 1; Lay <= Construct( FlConstrNum ).TotGlassLayers; ++Lay ) {
									AWinSurf( Lay, FloorNum ) += Construct( FlConstrNum ).AbsDiffBack( Lay ) / AbsBeamTotWin * BTOTWinZone * ISABSF( FloorNum ) / Surface( FloorNum ).Area; //[-]
								}
							}

							BABSZone += BTOTWinZone * ISABSF( FloorNum ); //[m2]

							AdjSurfNum = Surface( FloorNum ).ExtBoundCond;
							if ( Construct( FlConstrNum ).TransDiff > 0.0 && AdjSurfNum > 0 ) {

								// Window in an interior floor

								AdjZoneNum = Surface( AdjSurfNum ).Zone;

								// Contribution (assumed diffuse) to adjacent zone of beam radiation passing
								// through this window
								DBZoneIntWin( AdjZoneNum ) += BTOTWinZone * ISABSF( FloorNum ) * Construct( FlConstrNum ).TransDiff / AbsBeamTotWin;

								BABSZone += BTOTWinZone * ISABSF( FloorNum ) * Construct( FlConstrNum ).TransDiff / AbsBeamTotWin;
							}

						} // End of loop over floor sections
					} // End of check on complex vs. simple interior solar distribution

				} // End of sunlit fraction > 0 test

			} // End of first loop over surfaces in zone

			// It is importatnt to do this only one time
			//IF (ZoneNum == 1) THEN
			BABSZoneSSG = 0.0;
			BTOTZoneSSG = 0.0;
			for ( iSSG = 1; iSSG <= TotSurfIncSolSSG; ++iSSG ) {
				SurfNum = SurfIncSolSSG( iSSG ).SurfPtr;
				// do calculation only if construction number match.
				if ( SurfIncSolSSG( iSSG ).ConstrPtr == Surface( SurfNum ).Construction ) {
					if ( Surface( SurfNum ).Zone == ZoneNum ) {
						AbsIntSurf = Construct( Surface( SurfNum ).Construction ).InsideAbsorpSolar;
						//SolarIntoZone = GetCurrentScheduleValue(SurfIncSolSSG(iSSG)%SchedPtr) * Surface(SurfNum)%Area
						SolarIntoZone = GetCurrentScheduleValue( SurfIncSolSSG( iSSG ).SchedPtr );
						AISurf( SurfNum ) = SolarIntoZone * AbsIntSurf;
						BABSZoneSSG += AISurf( SurfNum ) * Surface( SurfNum ).Area;
						BTOTZoneSSG += SolarIntoZone * Surface( SurfNum ).Area;
					}
				}
			}
			DBZoneSSG( ZoneNum ) = BTOTZoneSSG - BABSZoneSSG;
			//END IF

			DBZone( ZoneNum ) = BTOTZone - BABSZone;

			if ( DBZone( ZoneNum ) < 0.0 ) {
				DBZone( ZoneNum ) = 0.0;
			}

			// Variables for reporting
			for ( SurfNum = Zone( ZoneNum ).SurfaceFirst; SurfNum <= Zone( ZoneNum ).SurfaceLast; ++SurfNum ) {
				if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
				if ( SolarDistribution == FullInteriorExterior ) {
					BmIncInsSurfAmountRep( SurfNum ) *= BeamSolarRad;
					BmIncInsSurfAmountRepEnergy( SurfNum ) = BmIncInsSurfAmountRep( SurfNum ) * TimeStepZoneSec;
					BmIncInsSurfIntensRep( SurfNum ) = BmIncInsSurfAmountRep( SurfNum ) / ( Surface( SurfNum ).Area + SurfaceWindow( SurfNum ).DividerArea );
				} else { // Simple interior solar distribution. All beam falls on floor.
					if ( ISABSF( SurfNum ) > 0.0 && Surface( SurfNum ).HeatTransSurf ) {

						if ( Zone( ZoneNum ).FloorArea > 0.0 ) {
							// spread onto all floor surfaces, these may or may not be called "floor"
							BmIncInsSurfIntensRep( SurfNum ) = BeamSolarRad * BTOTZone / Zone( ZoneNum ).FloorArea;
						} else if ( Zone( ZoneNum ).TotalSurfArea > 0.0 ) {
							// spread onto all interior surfaces
							BmIncInsSurfIntensRep( SurfNum ) = BeamSolarRad * BTOTZone / Zone( ZoneNum ).TotalSurfArea;
						} else { //divide be zero otherwise
							BmIncInsSurfIntensRep( SurfNum ) = 0.0;
						}
					}
					BmIncInsSurfAmountRep( SurfNum ) = Surface( SurfNum ).Area * BmIncInsSurfIntensRep( SurfNum );
					BmIncInsSurfAmountRepEnergy( SurfNum ) = BmIncInsSurfAmountRep( SurfNum ) * TimeStepZoneSec;
				}
				if ( Surface( SurfNum ).Class == SurfaceClass_Window || Surface( SurfNum ).Class == SurfaceClass_TDD_Dome ) {

					SurfaceWindow( SurfNum ).IntBeamAbsByShade = IntBeamAbsByShadFac( SurfNum );
					SurfaceWindow( SurfNum ).ExtBeamAbsByShade = BeamSolarRad * ExtBeamAbsByShadFac( SurfNum );

					if ( ( Surface( SurfNum ).ExtBoundCond == ExternalEnvironment ) || ( Surface( SurfNum ).ExtBoundCond == OtherSideCondModeledExt ) ) {

						ShadeFlag = SurfaceWindow( SurfNum ).ShadingFlag;
						BlNum = SurfaceWindow( SurfNum ).BlindNumber;
						ShelfNum = Surface( SurfNum ).Shelf;
						if ( ShelfNum > 0 ) { // Outside daylighting shelf
							OutShelfSurf = Shelf( ShelfNum ).OutSurf;
						} else {
							OutShelfSurf = 0;
						}

						// This lookup may be avoid if this 2nd surf loop can be combined with the 1st
						if ( SurfaceWindow( SurfNum ).OriginalClass == SurfaceClass_TDD_Diffuser ) {
							PipeNum = FindTDDPipe( SurfNum );
							SurfNum2 = TDDPipe( PipeNum ).Dome;

							DifSolarInc = DifSolarRad * AnisoSkyMult( SurfNum2 ) + GndSolarRad * Surface( SurfNum2 ).ViewFactorGround;

							SkySolarTrans = DifSolarRad * TransTDD( PipeNum, CosInc, SolarAniso ) * AnisoSkyMult( SurfNum2 );
							GndSolarTrans = GndSolarRad * TDDPipe( PipeNum ).TransSolIso * Surface( SurfNum2 ).ViewFactorGround;

							WinBmSolar( SurfNum ) = BeamSolarRad * WinTransBmSolar( SurfNum );
							WinDifSolar( SurfNum ) = SkySolarTrans * Surface( SurfNum ).Area + GndSolarTrans * Surface( SurfNum ).Area;
							WinBmSolarEnergy( SurfNum ) = WinBmSolar( SurfNum ) * TimeStepZoneSec;
							WinDifSolarEnergy( SurfNum ) = WinDifSolar( SurfNum ) * TimeStepZoneSec;

							WinTransSolar( SurfNum ) = WinBmSolar( SurfNum ) + WinDifSolar( SurfNum ); //[W]
							WinTransSolarEnergy( SurfNum ) = WinTransSolar( SurfNum ) * TimeStepZoneSec;

							TDDPipe( PipeNum ).TransmittedSolar = WinTransSolar( SurfNum );
							//TDDPipe(PipeNum)%TransSolBeam = TBmBm ! Reported above
							if ( DifSolarInc > 0 ) {
								TDDPipe( PipeNum ).TransSolDiff = ( SkySolarTrans + GndSolarTrans ) / DifSolarInc;
							} else {
								TDDPipe( PipeNum ).TransSolDiff = 0.0;
							}

						} else if ( OutShelfSurf > 0 ) { // Outside daylighting shelf
							ShelfSolarRad = ( BeamSolarRad * SunlitFrac( TimeStep, HourOfDay, OutShelfSurf ) * CosIncAng( TimeStep, HourOfDay, OutShelfSurf ) + DifSolarRad * AnisoSkyMult( OutShelfSurf ) ) * Shelf( ShelfNum ).OutReflectSol;

							DifSolarInc = DifSolarRad * AnisoSkyMult( SurfNum ) + GndSolarRad * Surface( SurfNum ).ViewFactorGround + ShelfSolarRad * Shelf( ShelfNum ).ViewFactor;

							WinBmSolar( SurfNum ) = BeamSolarRad * WinTransBmSolar( SurfNum );
							WinDifSolar( SurfNum ) = DifSolarInc * WinTransDifSolar( SurfNum );
							WinBmSolarEnergy( SurfNum ) = WinBmSolar( SurfNum ) * TimeStepZoneSec;
							WinDifSolarEnergy( SurfNum ) = WinDifSolar( SurfNum ) * TimeStepZoneSec;

							WinTransSolar( SurfNum ) = WinBmSolar( SurfNum ) + WinDifSolar( SurfNum ); //[W]
							WinTransSolarEnergy( SurfNum ) = WinTransSolar( SurfNum ) * TimeStepZoneSec;

						} else { // Regular window
							SkySolarInc = SurfaceWindow( SurfNum ).SkySolarInc;
							GndSolarInc = SurfaceWindow( SurfNum ).GndSolarInc;
							DifSolarInc = SkySolarInc + GndSolarInc;
							WinBmSolar( SurfNum ) = BeamSolarRad * WinTransBmSolar( SurfNum );
							//Note: for complex fenestration, WinTransDifSolar has previously been defined using the effective
							// transmittance for sky and ground diffuse radiation (including beam radiation reflected from the ground)
							// so these calculations should be correct
							WinDifSolar( SurfNum ) = DifSolarInc * WinTransDifSolar( SurfNum );
							WinBmSolarEnergy( SurfNum ) = WinBmSolar( SurfNum ) * TimeStepZoneSec;
							WinDifSolarEnergy( SurfNum ) = WinDifSolar( SurfNum ) * TimeStepZoneSec;
							if ( ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == BGBlindOn ) {
								if ( Blind( SurfaceWindow( SurfNum ).BlindNumber ).SlatOrientation == Horizontal ) {
									WinDifSolar( SurfNum ) = SkySolarInc * WinTransDifSolarSky( SurfNum ) + GndSolarInc * WinTransDifSolarGnd( SurfNum );
									WinDifSolarEnergy( SurfNum ) = WinDifSolar( SurfNum ) * TimeStepZoneSec;
								}
							}

							WinTransSolar( SurfNum ) = WinBmSolar( SurfNum ) + WinDifSolar( SurfNum ); //[W]
							WinTransSolarEnergy( SurfNum ) = WinTransSolar( SurfNum ) * TimeStepZoneSec;

						}

						//added TH 12/9/2009, CR 7907 & 7809
						WinBmBmSolar( SurfNum ) = BeamSolarRad * WinTransBmBmSolar;

						WinBmDifSolar( SurfNum ) = BeamSolarRad * WinTransBmDifSolar;
						WinBmBmSolarEnergy( SurfNum ) = WinBmBmSolar( SurfNum ) * TimeStepZoneSec;
						WinBmDifSolarEnergy( SurfNum ) = WinBmDifSolar( SurfNum ) * TimeStepZoneSec;

						WinDirSolTransAtIncAngle( SurfNum ) = TBmBm + TBmDif; // For TDD:DIFFUSER this is the TDD transmittance

						// Solar not added by TDD:DOME; added to zone via TDD:DIFFUSER
						if ( Surface( SurfNum ).Class != SurfaceClass_TDD_Dome ) {
							ZoneTransSolar( ZoneNum ) += WinTransSolar( SurfNum ); //[W]
							ZoneTransSolarEnergy( ZoneNum ) = ZoneTransSolar( ZoneNum ) * TimeStepZoneSec; //[J]
							ZoneBmSolFrExtWinsRep( ZoneNum ) += WinBmSolar( SurfNum );
							ZoneDifSolFrExtWinsRep( ZoneNum ) += WinDifSolar( SurfNum );
							ZoneBmSolFrExtWinsRepEnergy( ZoneNum ) = ZoneBmSolFrExtWinsRep( ZoneNum ) * TimeStepZoneSec; //[J]
							ZoneDifSolFrExtWinsRepEnergy( ZoneNum ) = ZoneDifSolFrExtWinsRep( ZoneNum ) * TimeStepZoneSec; //[J]
						}

					}
				}
			} // End of second loop over surfaces in zone

		} // End of first zone loop

		// Add interior window contribution to DBZone

		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
			DBZone( ZoneNum ) += DBZoneIntWin( ZoneNum );
			ZoneBmSolFrIntWinsRep( ZoneNum ) = DBZoneIntWin( ZoneNum ) * BeamSolarRad;
			ZoneBmSolFrIntWinsRepEnergy( ZoneNum ) = ZoneBmSolFrIntWinsRep( ZoneNum ) * TimeStepZoneSec; //[J]
		}

		// RJH - Calculate initial distribution of diffuse solar transmitted by exterior windows into each zone
		//       to all interior surfaces in the zone
		//       Includes subsequent transmittance of diffuse solar to adjacent zones through interior windows
		CalcWinTransDifSolInitialDistribution();

	}

	int
	WindowScheduledSolarAbs(
		int const SurfNum, // Surface number
		int const ConstNum // Construction number
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Vidanovic
		//       DATE WRITTEN   June 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Returns scheduled surface gain object for given surface-construction combination

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Return value
		int WindowScheduledSolarAbs;

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

		WindowScheduledSolarAbs = 0;

		for ( i = 1; i <= TotFenLayAbsSSG; ++i ) {
			if ( ( FenLayAbsSSG( i ).SurfPtr == SurfNum ) && ( FenLayAbsSSG( i ).ConstrPtr == ConstNum ) ) {
				WindowScheduledSolarAbs = i;
				return WindowScheduledSolarAbs;
			}
		}

		return WindowScheduledSolarAbs;
	}

	int
	SurfaceScheduledSolarInc(
		int const SurfNum, // Surface number
		int const ConstNum // Construction number
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Vidanovic
		//       DATE WRITTEN   June 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Returns scheduled surface gain pointer for given surface-construction combination

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Return value
		int SurfaceScheduledSolarInc;

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

		SurfaceScheduledSolarInc = 0;

		for ( i = 1; i <= TotSurfIncSolSSG; ++i ) {
			if ( ( SurfIncSolSSG( i ).SurfPtr == SurfNum ) && ( SurfIncSolSSG( i ).ConstrPtr == ConstNum ) ) {
				SurfaceScheduledSolarInc = i;
				return SurfaceScheduledSolarInc;
			}
		}

		return SurfaceScheduledSolarInc;
	}

	void
	PerformSolarCalculations()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   July 1999
		//       MODIFIED       Sept 2003, FCW: add calls to CalcBeamSolDiffuseReflFactors and
		//                       CalcBeamSolSpecularReflFactors
		//                      Jan 2004, FCW: call CalcDayltgCoefficients if storm window status on
		//                       any window has changed
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine determines if new solar/shading calculations need
		// to be performed and calls the proper routines to do the job.

		// METHODOLOGY EMPLOYED:
		// Users are allowed to enter a value for number of days in each period that
		// will be used for calculating solar.  (Later, this could be more complicated as
		// in allowing a number of days in a month or something).  Using this value or the
		// default (20 days) if nothing is entered by the user, the routine will use the
		// number of days left to determine if a new set of calculations should be done.
		// The calculations use the average of "equation of time" and "solar declination"
		// to perform the calculations.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DaylightingManager::CalcDayltgCoefficients;
		using DaylightingManager::TotWindowsWithDayl;
		using DataSystemVariables::DetailedSolarTimestepIntegration;

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
		Real64 SumDec;
		Real64 SumET;
		Real64 AvgEqOfTime;
		Real64 AvgSinSolarDeclin;
		Real64 AvgCosSolarDeclin;
		int PerDayOfYear;
		int Count;
		Real64 SinDec;
		Real64 EqTime;
		//not used INTEGER SurfNum

		// Calculate sky diffuse shading

		if ( BeginSimFlag ) {
			CalcSkyDifShading = true;
			SkyDifSolarShading(); // Calculate factors for shading of sky diffuse solar
			CalcSkyDifShading = false;
		}

		if ( BeginEnvrnFlag ) {
			ShadowingDaysLeft = 0;
		}

		if ( ShadowingDaysLeft <= 0 || DetailedSolarTimestepIntegration ) {

			if ( ! DetailedSolarTimestepIntegration ) {
				//  Perform calculations.
				ShadowingDaysLeft = ShadowingCalcFrequency;
				if ( DayOfSim + ShadowingDaysLeft > NumOfDayInEnvrn ) {
					ShadowingDaysLeft = NumOfDayInEnvrn - DayOfSim + 1;
				}

				//  Calculate average Equation of Time, Declination Angle for this period

				if ( ! WarmupFlag ) {
					DisplayString( "Updating Shadowing Calculations, Start Date=" + CurMnDy );
					DisplayPerfSimulationFlag = true;
				}

				PerDayOfYear = DayOfYear;
				SumDec = 0.0;
				SumET = 0.0;
				for ( Count = 1; Count <= ShadowingDaysLeft; ++Count ) {
					SUN3( PerDayOfYear, SinDec, EqTime );
					SumDec += SinDec;
					SumET += EqTime;
					++PerDayOfYear;
				}

				//  Compute Period Values
				AvgSinSolarDeclin = SumDec / double( ShadowingDaysLeft );
				AvgCosSolarDeclin = std::sqrt( 1.0 - pow_2( AvgSinSolarDeclin ) );
				AvgEqOfTime = SumET / double( ShadowingDaysLeft );
			} else {
				SUN3( DayOfYear, AvgSinSolarDeclin, AvgEqOfTime );
				AvgCosSolarDeclin = std::sqrt( 1.0 - pow_2( AvgSinSolarDeclin ) );
			}

			CalcPerSolarBeam( AvgEqOfTime, AvgSinSolarDeclin, AvgCosSolarDeclin );

			// Calculate factors for solar reflection
			if ( CalcSolRefl ) {
				CalcBeamSolDiffuseReflFactors();
				CalcBeamSolSpecularReflFactors();
				if ( BeginSimFlag ) CalcSkySolDiffuseReflFactors();
			}

			//  Calculate daylighting coefficients
			CalcDayltgCoefficients();

		}

		if ( ! WarmupFlag ) {
			--ShadowingDaysLeft;
		}

		// Recalculate daylighting coefficients if storm window has been added
		// or removed from one or more windows at beginning of day
		if ( TotWindowsWithDayl > 0 && ! BeginSimFlag && ! BeginEnvrnFlag && ! WarmupFlag && TotStormWin > 0 && StormWinChangeThisDay ) {
			CalcDayltgCoefficients();
		}

	}

	void
	SHDRVL(
		int const HTSS, // Heat transfer surface number of the subsurface
		int const SBSNR, // Subsurface number
		int const Hour,
		int const TS
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Legacy Code
		//       DATE WRITTEN
		//       MODIFIED       May 2002 (FCW): allow triangular windows to have reveal.
		//       RE-ENGINEERED  Lawrie, Oct 2000

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine computes the shadowing from a reveal onto a subsurface.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// BLAST/IBLAST code, original author George Walton

		// USE STATEMENTS:
		// na

		// Locals
		int NVS; // Number of verticies

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const None( 0 ); // for use with RevealStatus
		int const EntireWindowShadedByReveal( 1 ); // for use with RevealStatus
		int const WindowShadedOnlyByReveal( 2 ); // for use with RevealStatus

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 A; // Area
		Real64 R; // Depth of the reveal (m)
		int I; // Loop control
		int N; // Vertex number
		int NS1; // Locations in homogeneous coordinate array
		int NS2;
		// note, below dimensions not changed because subsurface still max 4
		Array1D< Real64 > XVT( 5 ); // Projected X coordinates of vertices
		Array1D< Real64 > YVT( 5 ); // Projected Y coordinates of vertices
		bool RevealStatusSet; // Used to control flow through this subroutine.
		// Certain operations performed only if reveal status not yet set.
		int RevealStatus; // Status of the reveal, takes the parameter values above

		// FLOW:
		RevealStatus = None;
		RevealStatusSet = false;

		if ( ! CalcSkyDifShading ) {
			WindowRevealStatus( TS, Hour, SBSNR ) = None;
		}

		R = Surface( SBSNR ).Reveal;
		if ( R <= 0.0 ) {
			RevealStatus = None;
			RevealStatusSet = true;
		}

		if ( ! RevealStatusSet ) {

			FRVLHC = LOCHCA + 1;
			++LOCHCA;
			NVS = Surface( SBSNR ).Sides;

			// Currently (06May02) windows are either rectangles (NVS=4) or triangles (NVS=3)

			if ( NVS == 4 ) { // Rectangular subsurface

				// Determine vertices of reveal.
				// Project the subsurface up to the plane of the wall.

				XVT( 1 ) = ShadeV( SBSNR ).XV( 1 ) + R * max( XShadowProjection, 0.0 );
				XVT( 2 ) = ShadeV( SBSNR ).XV( 2 ) + R * max( XShadowProjection, 0.0 );
				XVT( 3 ) = ShadeV( SBSNR ).XV( 3 ) + R * min( XShadowProjection, 0.0 );
				XVT( 4 ) = ShadeV( SBSNR ).XV( 4 ) + R * min( XShadowProjection, 0.0 );
				YVT( 1 ) = ShadeV( SBSNR ).YV( 1 ) + R * min( YShadowProjection, 0.0 );
				YVT( 2 ) = ShadeV( SBSNR ).YV( 2 ) + R * max( YShadowProjection, 0.0 );
				YVT( 3 ) = ShadeV( SBSNR ).YV( 3 ) + R * max( YShadowProjection, 0.0 );
				YVT( 4 ) = ShadeV( SBSNR ).YV( 4 ) + R * min( YShadowProjection, 0.0 );

				// Check for complete shadowing.

				if ( ( XVT( 2 ) >= XVT( 3 ) ) || ( YVT( 2 ) >= YVT( 1 ) ) ) {

					RevealStatus = EntireWindowShadedByReveal;
					RevealStatusSet = true;

				} else {
					// Re-order vertices to clockwise.

					for ( N = 1; N <= NVS; ++N ) {
						XVS( N ) = XVT( NVS + 1 - N );
						YVS( N ) = YVT( NVS + 1 - N );
					}

					// Transform to homogeneous coordinates

					HTRANS1( FRVLHC, NVS );
					HCAREA( FRVLHC ) = -HCAREA( FRVLHC );
					HCT( FRVLHC ) = 1.0;

					if ( HCAREA( FRVLHC ) <= 0.0 ) {
						RevealStatus = EntireWindowShadedByReveal;
						RevealStatusSet = true;
					}

				}

			} else if ( NVS == 3 ) { // Triangular window

				// Project window to outside plane of parent surface

				for ( N = 1; N <= 3; ++N ) {
					XVT( N ) = ShadeV( SBSNR ).XV( N ) + R * XShadowProjection;
					YVT( N ) = ShadeV( SBSNR ).YV( N ) + R * YShadowProjection;
				}

				// Find the overlap between the original window and the projected window
				// Put XVT,YVT in clockwise order

				for ( N = 1; N <= NVS; ++N ) {
					XVS( N ) = XVT( NVS + 1 - N );
					YVS( N ) = YVT( NVS + 1 - N );
				}

				// Transform to homogeneous coordinates

				NS1 = LOCHCA + 1;
				LOCHCA = NS1;
				HTRANS1( NS1, NVS );

				// Put XV,YV in clockwise order

				for ( N = 1; N <= NVS; ++N ) {
					XVS( N ) = ShadeV( SBSNR ).XV( NVS + 1 - N );
					YVS( N ) = ShadeV( SBSNR ).YV( NVS + 1 - N );
				}

				// Transform to homogenous coordinates

				NS2 = LOCHCA + 1;
				LOCHCA = NS2;
				HTRANS1( NS2, NVS );
				HCT( FRVLHC ) = 1.0;

				// Find overlap

				DeterminePolygonOverlap( NS1, NS2, FRVLHC );
				if ( OverlapStatus == NoOverlap ) {
					RevealStatus = EntireWindowShadedByReveal;
					RevealStatusSet = true;
				}

			}

		}

		if ( ! RevealStatusSet ) {

			// Check for no shadows on window.

			if ( NSBSHC <= 1 ) {
				RevealStatus = WindowShadedOnlyByReveal;
				RevealStatusSet = true;
			} else {
				// Reduce all previous shadows to size of reveal opening.
				LOCHCA = FRVLHC;
				MULTOL( LOCHCA, FSBSHC, NSBSHC - 1 );
				if ( ( OverlapStatus == TooManyVertices ) || ( OverlapStatus == TooManyFigures ) ) {
					RevealStatus = None;
					RevealStatusSet = true;
				} else {
					NRVLHC = LOCHCA - FRVLHC + 1;
					if ( NRVLHC <= 1 ) {
						RevealStatus = WindowShadedOnlyByReveal;
						RevealStatusSet = true;
					}
				}
			}
		}

		if ( ! RevealStatusSet ) {
			// Compute sunlit area.
			A = HCAREA( FRVLHC );
			for ( I = 2; I <= NRVLHC; ++I ) {
				A += HCAREA( FRVLHC - 1 + I ) * ( 1.0 - HCT( FRVLHC - 1 + I ) );
			}
			SAREA( HTSS ) = A;
		}

		if ( ( RevealStatus == EntireWindowShadedByReveal ) || ( SAREA( HTSS ) < 0.0 ) ) {
			SAREA( HTSS ) = 0.0; // Window entirely shaded by reveal.
		} else if ( RevealStatus == WindowShadedOnlyByReveal ) {
			SAREA( HTSS ) = HCAREA( FRVLHC ); // Window shaded only by reveal.
		}

		if ( ! CalcSkyDifShading ) {
			WindowRevealStatus( TS, Hour, SBSNR ) = RevealStatus;
		}

	}

	void
	SHDSBS(
		int const iHour, // Hour Index
		int const CurSurf,
		int const NBKS, // Number of back surfaces
		int const NSBS, // Number of subsurfaces
		int const HTS, // Heat transfer surface number of the general receiving surf
		int const TS // Time step Index
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Legacy Code
		//       DATE WRITTEN
		//       MODIFIED       FCW, Oct 2002: Surface%Area --> Surface%Area + SurfaceWindow%DividerArea
		//                       in calculation of SunlitFracWithoutReveal (i.e., use full window area, not
		//                       just glass area.
		//                      TH, May 2009: Bug fixed to address part of CR 7596 - inside reveals
		//                       causing high cooling loads
		//       RE-ENGINEERED  Lawrie, Oct 2000

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine determines the shadowing on subsurfaces and
		// revises the base surface area accordingly.  It also computes
		// the effect of transparent subsurfaces.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// BLAST/IBLAST code, original author George Walton

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
		Real64 A; // Area
		int I; // Loop control
		int J; // Loop control
		int K; // Window construction number
		int N; // Vertex number
		Real64 SurfArea; // Surface area. For walls, includes all window frame areas.
		// For windows, includes divider area
		//  REAL(r64) FrameAreaAdd    ! Additional frame area sunlit
		//  REAL(r64) DividerAreaAdd  ! Additional frame area sunlit
		int HTSS; // Heat transfer surface number of the subsurface
		int SBSNR; // Subsurface number

		if ( NSBS > 0 ) { // Action taken only if subsurfaces present

			FSBSHC = LOCHCA + 1;

			for ( I = 1; I <= NSBS; ++I ) { // Do for all subsurfaces (sbs).

				SBSNR = ShadowComb( CurSurf ).SubSurf( I );

				HTSS = SBSNR;

				K = Surface( SBSNR ).Construction;

				if ( ( OverlapStatus != TooManyVertices ) && ( OverlapStatus != TooManyFigures ) && ( SAREA( HTS ) > 0.0 ) ) {

					// Re-order vertices to clockwise sequential; compute homogeneous coordinates.
					NVS = Surface( SBSNR ).Sides;
					for ( N = 1; N <= NVS; ++N ) {
						XVS( N ) = ShadeV( SBSNR ).XV( NVS + 1 - N );
						YVS( N ) = ShadeV( SBSNR ).YV( NVS + 1 - N );
					}
					LOCHCA = FSBSHC;
					HTRANS1( LOCHCA, NVS );
					HCAREA( LOCHCA ) = -HCAREA( LOCHCA );
					HCT( LOCHCA ) = 1.0;
					NSBSHC = LOCHCA - FSBSHC + 1;

					// Determine sunlit area of subsurface due to shadows on general receiving surface.
					if ( NGSSHC > 0 ) {
						MULTOL( LOCHCA, FGSSHC - 1, NGSSHC );
						if ( ( OverlapStatus != TooManyVertices ) && ( OverlapStatus != TooManyFigures ) ) NSBSHC = LOCHCA - FSBSHC + 1;
					}

				}

				if ( ( OverlapStatus == TooManyVertices ) || ( OverlapStatus == TooManyFigures ) || ( SAREA( HTS ) <= 0.0 ) ) { // General receiving surface totally shaded.

					SAREA( HTSS ) = 0.0;

					if ( iHour > 0 && TS > 0 ) SunlitFracWithoutReveal( TS, iHour, HTSS ) = 0.0;

				} else if ( ( NGSSHC <= 0 ) || ( NSBSHC == 1 ) ) { // No shadows.

					SAREA( HTSS ) = HCAREA( FSBSHC );
					SAREA( HTS ) -= SAREA( HTSS ); // Revise sunlit area of general receiving surface.

					// TH. This is a bug.  SunLitFracWithoutReveal should be a ratio of area
					//IF(IHour > 0 .AND. TS > 0) SunLitFracWithoutReveal(HTSS,IHour,TS) = &
					//      Surface(HTSS)%NetAreaShadowCalc

					// new code fixed part of CR 7596. TH 5/29/2009
					if ( iHour > 0 && TS > 0 ) SunlitFracWithoutReveal( TS, iHour, HTSS ) = SAREA( HTSS ) / Surface( HTSS ).NetAreaShadowCalc;

					SHDRVL( HTSS, SBSNR, iHour, TS ); // Determine shadowing from reveal.

					if ( ( OverlapStatus == TooManyVertices ) || ( OverlapStatus == TooManyFigures ) ) SAREA( HTSS ) = 0.0;

				} else { // Compute area.

					A = HCAREA( FSBSHC );
					for ( J = 2; J <= NSBSHC; ++J ) {
						A += HCAREA( FSBSHC - 1 + J ) * ( 1.0 - HCT( FSBSHC - 1 + J ) );
					}
					SAREA( HTSS ) = A;
					if ( SAREA( HTSS ) > 0.0 ) {

						SAREA( HTS ) -= SAREA( HTSS ); // Revise sunlit area of general receiving surface.

						if ( iHour > 0 && TS > 0 ) SunlitFracWithoutReveal( TS, iHour, HTSS ) = SAREA( HTSS ) / Surface( HTSS ).Area;

						SHDRVL( HTSS, SBSNR, iHour, TS ); // Determine shadowing from reveal.

						if ( ( OverlapStatus == TooManyVertices ) || ( OverlapStatus == TooManyFigures ) ) SAREA( HTSS ) = 0.0;

					} else { // General receiving surface totally shaded.

						SAREA( HTSS ) = 0.0;

					}

				}

				// Determine transmittance and absorptances of sunlit window.
				if ( Construct( K ).TransDiff > 0.0 ) {

					if ( ! CalcSkyDifShading ) { //Overlaps calculation is only done for beam solar
						//shading, not for sky diffuse solar shading

						CalcInteriorSolarOverlaps( iHour, NBKS, HTSS, CurSurf, TS );

					}

				}

				// Error checking.
				SurfArea = Surface( SBSNR ).NetAreaShadowCalc;
				SAREA( HTSS ) = max( 0.0, SAREA( HTSS ) );

				SAREA( HTSS ) = min( SAREA( HTSS ), SurfArea );

			} // End of subsurface loop

		}

	}

	void
	SUN3(
		int const JulianDayOfYear, // Julian Day Of Year
		Real64 & SineOfSolarDeclination, // Sine of Solar Declination
		Real64 & EquationOfTime // Equation of Time (Degrees)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Legacy Code
		//       DATE WRITTEN
		//       MODIFIED       na
		//       RE-ENGINEERED  Linda K. Lawrie

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine computes the coefficients for determining
		// the solar position.

		// METHODOLOGY EMPLOYED:
		// The expressions are based on least-squares fits of data on p.316 of 'Thermal
		// Environmental Engineering' by Threlkeld and on p.387 of the ASHRAE Handbook
		// of Fundamentals (need date of ASHRAE HOF).

		// REFERENCES:
		// BLAST/IBLAST code, original author George Walton

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static Array1D< Real64 > const SineSolDeclCoef( 9, { 0.00561800, 0.0657911, -0.392779, 0.00064440, -0.00618495, -0.00010101, -0.00007951, -0.00011691, 0.00002096 } ); // Fitted coefficients of Fourier series | SINE OF DECLINATION | COEFFICIENTS
		static Array1D< Real64 > const EqOfTimeCoef( 9, { 0.00021971, -0.122649, 0.00762856, -0.156308, -0.0530028, -0.00388702, -0.00123978, -0.00270502, -0.00167992 } ); // Fitted coefficients of Fourier Series | EQUATION OF TIME | COEFFICIENTS

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 X; // Day of Year in Radians (Computed from Input JulianDayOfYear)
		Real64 CosX; // COS(X)
		Real64 SineX; // SIN(X)

		X = 0.017167 * JulianDayOfYear; // Convert julian date to angle X

		// Calculate sines and cosines of X
		SineX = std::sin( X );
		CosX = std::cos( X );

		SineOfSolarDeclination = SineSolDeclCoef( 1 ) + SineSolDeclCoef( 2 ) * SineX + SineSolDeclCoef( 3 ) * CosX + SineSolDeclCoef( 4 ) * ( SineX * CosX * 2.0 ) + SineSolDeclCoef( 5 ) * ( pow_2( CosX ) - pow_2( SineX ) ) + SineSolDeclCoef( 6 ) * ( SineX * ( pow_2( CosX ) - pow_2( SineX ) ) + CosX * ( SineX * CosX * 2.0 ) ) + SineSolDeclCoef( 7 ) * ( CosX * ( pow_2( CosX ) - pow_2( SineX ) ) - SineX * ( SineX * CosX * 2.0 ) ) + SineSolDeclCoef( 8 ) * ( 2.0 * ( SineX * CosX * 2.0 ) * ( pow_2( CosX ) - pow_2( SineX ) ) ) + SineSolDeclCoef( 9 ) * ( pow_2( pow_2( CosX ) - pow_2( SineX ) ) - pow_2( SineX * CosX * 2.0 ) );

		EquationOfTime = EqOfTimeCoef( 1 ) + EqOfTimeCoef( 2 ) * SineX + EqOfTimeCoef( 3 ) * CosX + EqOfTimeCoef( 4 ) * ( SineX * CosX * 2.0 ) + EqOfTimeCoef( 5 ) * ( pow_2( CosX ) - pow_2( SineX ) ) + EqOfTimeCoef( 6 ) * ( SineX * ( pow_2( CosX ) - pow_2( SineX ) ) + CosX * ( SineX * CosX * 2.0 ) ) + EqOfTimeCoef( 7 ) * ( CosX * ( pow_2( CosX ) - pow_2( SineX ) ) - SineX * ( SineX * CosX * 2.0 ) ) + EqOfTimeCoef( 8 ) * ( 2.0 * ( SineX * CosX * 2.0 ) * ( pow_2( CosX ) - pow_2( SineX ) ) ) + EqOfTimeCoef( 9 ) * ( pow_2( pow_2( CosX ) - pow_2( SineX ) ) - pow_2( SineX * CosX * 2.0 ) );

	}

	void
	SUN4(
		Real64 const CurrentTime, // Time to use in shadowing calculations
		Real64 const EqOfTime, // Equation of time for current day
		Real64 const SinSolarDeclin, // Sine of the Solar declination (current day)
		Real64 const CosSolarDeclin // Cosine of the Solar declination (current day)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Legacy Code
		//       DATE WRITTEN
		//       MODIFIED       na
		//       RE-ENGINEERED  Lawrie, Oct 2000

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine computes solar direction cosines for a given hour.  These
		// cosines are used in the shadowing calculations.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// BLAST/IBLAST code, original author George Walton

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
		Real64 H; // Hour angle (before noon = +) (in radians)
		Real64 HrAngle; // Basic hour angle

		// Compute the hour angle
		HrAngle = ( 15.0 * ( 12.0 - ( CurrentTime + EqOfTime ) ) + ( TimeZoneMeridian - Longitude ) );
		H = HrAngle * DegToRadians;

		// Compute the cosine of the solar zenith angle.
		SUNCOS( 3 ) = SinSolarDeclin * SinLatitude + CosSolarDeclin * CosLatitude * std::cos( H );
		SUNCOS( 2 ) = 0.0;
		SUNCOS( 1 ) = 0.0;

		if ( SUNCOS( 3 ) < SunIsUpValue ) return; // Return if sun not above horizon.

		// Compute other direction cosines.
		SUNCOS( 2 ) = SinSolarDeclin * CosLatitude - CosSolarDeclin * SinLatitude * std::cos( H );
		SUNCOS( 1 ) = CosSolarDeclin * std::sin( H );

	}

	void
	WindowShadingManager()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   December 1998
		//       MODIFIED       November 1999 (FW)
		//                      Aug 2001 (FW): change shading control names, change approach
		//                       to scheduling and glare control, add movable
		//                       insulation controls (mainly for heating reduction)
		//                      Dec 2001 (FW): add slat angle control for blinds
		//                      Aug 2002 (FW): add four new control types:
		//                        OnIfHighOutsideAirTempAndHighSolarOnWindow
		//                        OnIfHighOutsideAirTempAndHighHorizontalSolar
		//                        OnIfHighZoneAirTempAndHighSolarOnWindow
		//                        OnIfHighZoneAirTempAndHighHorizontalSolar
		//                      Dec 2002 (FW): add between-glass shade/blind
		//                      Mar 2003 (FW): allow GlareControlIsActive = .TRUE. only for daylit zones
		//                      Apr 2003 (FW): use SNLoadCoolRate or SNLoadHeatRate only if not first time step
		//                                     (fixes problem when used first time thru and not allocated)
		//                      May 2006 (RR): add exterior window screen
		//                      May 2009 (BG): add EMS actuator override for shade flag and slat angle
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// For windows with shading, selects the shaded construction
		// that is used in the heat balance calculation, and sets
		// the window shading flag, which is:
		//  -1: if window has no shading device
		//   0: if shading device is off
		//   1: if interior shade is on
		//   2: if glazing is switched to darker state
		//   3: if exterior shade is on
		//   6: if interior blind is on
		//   7: if exterior blind is on
		//   8: if between-glass shade is on
		//   9: if between-glass blind is on
		//  10: window has interior shade that is off but may be triggered on later
		//       to control daylight glare
		//  20: window has switchable glazing that is unswitched but may be switched later
		//       to control daylight glare or daylight illuminance
		//  30: window has exterior shade that is off but may be triggered on later
		//       to control daylaight glare or daylight illuminance
		//  60: window has interior blind that is off but may be triggered on later
		//       to control daylaight glare or daylight illuminance
		//  70: window has exterior blind that is off but may be triggered on later
		//       to control daylaight glare or daylight illuminance
		//  80: window has between-glass shade that is off but may be triggered on later
		//       to control daylaight glare or daylight illuminance
		//  90: window has between-glass blind that is off but may be triggered on later
		//       to control daylaight glare or daylight illuminance
		// A "shading device" may be an exterior, interior or between-glass shade or blind,
		// or the lower-transmitting (dark) state of switchable glazing (e.g., electrochromic).
		// In all cases, the unshaded condition is represented
		// by the construction given by window's Surface()%Construction and
		// the shaded condition is represented by the construction given by
		// the window's Surface()%ShadedConstruction
		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHeatBalFanSys::MAT;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataDaylighting::ZoneDaylight;
		using General::POLYF;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int ISurf; // Surface counter
		int IZone; // Zone counter
		int IShadingCtrl; // Pointer to a window's shading control
		Real64 BeamSolarOnWindow; // Direct solar intensity on window (W/m2)
		Real64 SolarOnWindow; // Direct plus diffuse solar intensity on window (W/m2)
		Real64 SkySolarOnWindow; // Sky diffuse solar intensity on window (W/m2)
		int SchedulePtr; // Schedule pointer
		Real64 HorizSolar; // Horizontal direct plus diffuse solar intensity
		Real64 SetPoint; // Control setpoint
		Real64 SetPoint2; // Second control setpoint
		int ShType; // 1 = interior shade is on,
		// 2 = glass is switched to dark state,
		// 3 = exterior shade is on,
		// 4 = exterior screen is on,
		// 6 = interior blind is on,
		// 7 = exterior blind is on,
		// 8 = between-glass shade is on,
		// 9 = between-glass blind is on.
		//  CHARACTER(len=32)  :: ShadingType     ! Type of shading (interior shade, interior blind, etc.)
		int ShadingType; // Type of shading (interior shade, interior blind, etc.)
		bool SchedAllowsControl; // True if control schedule is not specified or is
		//  specified and schedule value = 1
		bool GlareControlIsActive; // True if glare control is active
		int BlNum; // Blind number
		Real64 InputSlatAngle; // Slat angle of associated Material:WindowBlind (rad)
		Real64 ProfAng; // Solar profile angle (rad)
		Real64 SlatAng; // Slat angle this time step (rad)
		Real64 PermeabilityA; // Intermediate variables in blind permeability calc
		Real64 PermeabilityB;
		Real64 ThetaBase; // Intermediate slat angle variable (rad)
		Real64 ThetaBlock1; // Slat angles that just block beam solar (rad)
		Real64 ThetaBlock2;
		static Real64 ThetaBig( 0.0 ); // Larger of ThetaBlock1 and ThetaBlock2 	//Autodesk Used uninitialized in some runs
		static Real64 ThetaSmall( 0.0 ); // Smaller of ThetaBlock1 and ThetaBlock2 //Autodesk Used uninitialized in some runs
		static Real64 ThetaMin( 0.0 ); // Minimum allowed slat angle, resp. (rad)  //Autodesk Used uninitialized in some runs
		static Real64 ThetaMax( 0.0 ); // Maximum allowed slat angle, resp. (rad)  //Autodesk Used uninitialized in some runs

		int IConst; // Construction

		for ( ISurf = 1; ISurf <= TotSurfaces; ++ISurf ) {
			SurfaceWindow( ISurf ).ExtIntShadePrevTS = SurfaceWindow( ISurf ).ShadingFlag;
			SurfaceWindow( ISurf ).ShadingFlag = NoShade;
			SurfaceWindow( ISurf ).FracTimeShadingDeviceOn = 0.0;

			if ( Surface( ISurf ).Class != SurfaceClass_Window ) continue;
			if ( Surface( ISurf ).ExtBoundCond != ExternalEnvironment ) continue;
			if ( Surface( ISurf ).WindowShadingControlPtr == 0 ) continue;

			// Initialize switching factor (applicable only to switchable glazing) to unswitched
			SurfaceWindow( ISurf ).SwitchingFactor = 0.0;

			IConst = Surface( ISurf ).Construction;
			// Vis trans at normal incidence of unswitched glass. Counting the GlazedFrac
			if ( IConst > 0 ) SurfaceWindow( ISurf ).VisTransSelected = POLYF( 1.0, Construct( IConst ).TransVisBeamCoef ) * SurfaceWindow( ISurf ).GlazedFrac;

			// Window has shading control
			IShadingCtrl = Surface( ISurf ).WindowShadingControlPtr;
			ShadingType = WindowShadingControl( IShadingCtrl ).ShadingType;
			SurfaceWindow( ISurf ).ShadingFlag = ShadeOff; // Initialize shading flag to off
			IZone = Surface( ISurf ).Zone;
			// Setpoint for shading
			SetPoint = WindowShadingControl( IShadingCtrl ).SetPoint;
			SetPoint2 = WindowShadingControl( IShadingCtrl ).SetPoint2;

			//                                           ShType = NoShade           ! =-1 (see DataHeatBalance)
			//                                           ShType = ShadeOff          ! =0
			if ( ShadingType == WSC_ST_InteriorShade ) ShType = IntShadeOn; // =1
			if ( ShadingType == WSC_ST_SwitchableGlazing ) ShType = SwitchableGlazing; // =2
			if ( ShadingType == WSC_ST_ExteriorShade ) ShType = ExtShadeOn; // =3
			if ( ShadingType == WSC_ST_ExteriorScreen ) ShType = ExtScreenOn; // =4
			if ( ShadingType == WSC_ST_InteriorBlind ) ShType = IntBlindOn; // =6
			if ( ShadingType == WSC_ST_ExteriorBlind ) ShType = ExtBlindOn; // =7
			if ( ShadingType == WSC_ST_BetweenGlassShade ) ShType = BGShadeOn; // =8
			if ( ShadingType == WSC_ST_BetweenGlassBlind ) ShType = BGBlindOn; // =9

			SchedAllowsControl = true;
			SchedulePtr = WindowShadingControl( IShadingCtrl ).Schedule;
			if ( SchedulePtr != 0 ) {
				if ( WindowShadingControl( IShadingCtrl ).ShadingControlIsScheduled && GetCurrentScheduleValue( SchedulePtr ) <= 0.0 ) SchedAllowsControl = false;
			}

			GlareControlIsActive = ( ZoneDaylight( IZone ).TotalDaylRefPoints > 0 && SunIsUp && WindowShadingControl( IShadingCtrl ).GlareControlIsActive );

			SolarOnWindow = 0.0;
			BeamSolarOnWindow = 0.0;
			HorizSolar = 0.0;
			if ( SunIsUp ) {
				SkySolarOnWindow = AnisoSkyMult( ISurf ) * DifSolarRad;
				BeamSolarOnWindow = BeamSolarRad * CosIncAng( TimeStep, HourOfDay, ISurf ) * SunlitFrac( TimeStep, HourOfDay, ISurf );
				SolarOnWindow = BeamSolarOnWindow + SkySolarOnWindow + GndSolarRad * Surface( ISurf ).ViewFactorGround;
				HorizSolar = BeamSolarRad * SOLCOS( 3 ) + DifSolarRad;
			}

			// Determine whether to deploy shading depending on type of control

			{ auto const SELECT_CASE_var( WindowShadingControl( IShadingCtrl ).ShadingControlType );

			if ( SELECT_CASE_var == WSCT_AlwaysOn ) { // 'ALWAYSON'
				SurfaceWindow( ISurf ).ShadingFlag = ShType;

			} else if ( SELECT_CASE_var == WSCT_AlwaysOff ) { // 'ALWAYSOFF'
				SurfaceWindow( ISurf ).ShadingFlag = ShadeOff;

			} else if ( SELECT_CASE_var == WSCT_OnIfScheduled ) { // 'ONIFSCHEDULEALLOWS'
				if ( SchedAllowsControl ) SurfaceWindow( ISurf ).ShadingFlag = ShType;

			} else if ( SELECT_CASE_var == WSCT_HiSolar ) { // 'ONIFHIGHSOLARONWINDOW'  ! Direct plus diffuse solar intensity on window
				if ( SunIsUp ) {
					if ( SolarOnWindow > SetPoint && SchedAllowsControl ) {
						SurfaceWindow( ISurf ).ShadingFlag = ShType;
					} else if ( GlareControlIsActive ) {
						SurfaceWindow( ISurf ).ShadingFlag = 10 * ShType;
					}
				}

			} else if ( SELECT_CASE_var == WSCT_HiHorzSolar ) { // 'ONIFHIGHHORIZONTALSOLAR'  ! Direct plus diffuse exterior horizontal solar intensity
				if ( SunIsUp ) {
					if ( HorizSolar > SetPoint && SchedAllowsControl ) {
						SurfaceWindow( ISurf ).ShadingFlag = ShType;
					} else if ( GlareControlIsActive ) {
						SurfaceWindow( ISurf ).ShadingFlag = 10 * ShType;
					}
				}

			} else if ( SELECT_CASE_var == WSCT_HiOutAirTemp ) { // 'OnIfHighOutdoorAirTemperature'
				if ( Surface( ISurf ).OutDryBulbTemp > SetPoint && SchedAllowsControl ) {
					SurfaceWindow( ISurf ).ShadingFlag = ShType;
				} else if ( GlareControlIsActive ) {
					SurfaceWindow( ISurf ).ShadingFlag = 10 * ShType;
				}

			} else if ( SELECT_CASE_var == WSCT_HiZoneAirTemp ) { // 'OnIfHighZoneAirTemperature'  ! Previous time step zone air temperature
				if ( MAT( IZone ) > SetPoint && SchedAllowsControl ) {
					SurfaceWindow( ISurf ).ShadingFlag = ShType;
				} else if ( GlareControlIsActive ) {
					SurfaceWindow( ISurf ).ShadingFlag = 10 * ShType;
				}

			} else if ( SELECT_CASE_var == WSCT_OnHiOutTemp_HiSolarWindow ) { // 'OnIfHighOutdoorAirTempAndHighSolarOnWindow'  ! Outside air temp and solar on window
				if ( SunIsUp ) {
					if ( Surface( ISurf ).OutDryBulbTemp > SetPoint && SolarOnWindow > SetPoint2 && SchedAllowsControl ) {
						SurfaceWindow( ISurf ).ShadingFlag = ShType;
					} else if ( GlareControlIsActive ) {
						SurfaceWindow( ISurf ).ShadingFlag = 10 * ShType;
					}
				}

			} else if ( SELECT_CASE_var == WSCT_OnHiOutTemp_HiHorzSolar ) { // 'OnIfHighOutdoorAirTempAndHighHorizontalSolar'  ! Outside air temp and horizontal solar
				if ( SunIsUp ) {
					if ( Surface( ISurf ).OutDryBulbTemp > SetPoint && HorizSolar > SetPoint2 && SchedAllowsControl ) {
						SurfaceWindow( ISurf ).ShadingFlag = ShType;
					} else if ( GlareControlIsActive ) {
						SurfaceWindow( ISurf ).ShadingFlag = 10 * ShType;
					}
				}

			} else if ( SELECT_CASE_var == WSCT_OnHiZoneTemp_HiSolarWindow ) { // 'ONIFHIGHZONEAIRTEMPANDHIGHSOLARONWINDOW'  ! Zone air temp and solar on window
				if ( SunIsUp ) {
					if ( MAT( IZone ) > SetPoint && SolarOnWindow > SetPoint2 && SchedAllowsControl ) {
						SurfaceWindow( ISurf ).ShadingFlag = ShType;
					} else if ( GlareControlIsActive ) {
						SurfaceWindow( ISurf ).ShadingFlag = 10 * ShType;
					}
				}

			} else if ( SELECT_CASE_var == WSCT_OnHiZoneTemp_HiHorzSolar ) { // 'ONIFHIGHZONEAIRTEMPANDHIGHHORIZONTALSOLAR'  ! Zone air temp and horizontal solar
				if ( SunIsUp ) {
					if ( MAT( IZone ) > SetPoint && HorizSolar > SetPoint2 && SchedAllowsControl ) {
						SurfaceWindow( ISurf ).ShadingFlag = ShType;
					} else if ( GlareControlIsActive ) {
						SurfaceWindow( ISurf ).ShadingFlag = 10 * ShType;
					}
				}

			} else if ( SELECT_CASE_var == WSCT_HiZoneCooling ) { // 'ONIFHIGHZONECOOLING'  ! Previous time step zone sensible cooling rate [W]
				// In the following, the check on BeginSimFlag is needed since SNLoadCoolRate (and SNLoadHeatRate,
				// used in other CASEs) are not allocated at this point for the first time step of the simulation.
				if ( ! BeginSimFlag ) {
					if ( SNLoadCoolRate( IZone ) > SetPoint && SchedAllowsControl ) {
						SurfaceWindow( ISurf ).ShadingFlag = ShType;
					} else if ( GlareControlIsActive ) {
						SurfaceWindow( ISurf ).ShadingFlag = 10 * ShType;
					}
				}

			} else if ( SELECT_CASE_var == WSCT_HiGlare ) { // 'ONIFHIGHGLARE'  ! Daylight glare index at first reference point in the zone.
				// This type of shading control is done in DayltgInteriorIllum. Glare control is not affected
				// by control schedule.
				if ( SunIsUp ) SurfaceWindow( ISurf ).ShadingFlag = 10 * ShType;

			} else if ( SELECT_CASE_var == WSCT_MeetDaylIlumSetp ) { // 'MEETDAYLIGHTILLUMINANCESETPOINT')  !  Daylight illuminance test is done in DayltgInteriorIllum
				// Only switchable glazing does daylight illuminance control
				if ( SunIsUp && SchedAllowsControl ) SurfaceWindow( ISurf ).ShadingFlag = GlassConditionallyLightened;

			} else if ( SELECT_CASE_var == WSCT_OnNightLoOutTemp_OffDay ) { // 'OnNightIfLowOutdoorTempAndOffDay'
				if ( ! SunIsUp && Surface( ISurf ).OutDryBulbTemp < SetPoint && SchedAllowsControl ) {
					SurfaceWindow( ISurf ).ShadingFlag = ShType;
				} else if ( GlareControlIsActive ) {
					SurfaceWindow( ISurf ).ShadingFlag = 10 * ShType;
				}

			} else if ( SELECT_CASE_var == WSCT_OnNightLoInTemp_OffDay ) { // 'OnNightIfLowInsideTempAndOffDay')
				if ( ! SunIsUp && MAT( IZone ) < SetPoint && SchedAllowsControl ) {
					SurfaceWindow( ISurf ).ShadingFlag = ShType;
				} else if ( GlareControlIsActive ) {
					SurfaceWindow( ISurf ).ShadingFlag = 10 * ShType;
				}

			} else if ( SELECT_CASE_var == WSCT_OnNightIfHeating_OffDay ) { // 'OnNightIfHeatingAndOffDay'
				if ( ! BeginSimFlag ) {
					if ( ! SunIsUp && SNLoadHeatRate( IZone ) > SetPoint && SchedAllowsControl ) {
						SurfaceWindow( ISurf ).ShadingFlag = ShType;
					} else if ( GlareControlIsActive ) {
						SurfaceWindow( ISurf ).ShadingFlag = 10 * ShType;
					}
				}

			} else if ( SELECT_CASE_var == WSCT_OnNightLoOutTemp_OnDayCooling ) { // 'OnNightIfLowOutdoorTempAndOnDayIfCooling'
				if ( ! BeginSimFlag ) {
					if ( ! SunIsUp ) { // Night
						if ( Surface( ISurf ).OutDryBulbTemp < SetPoint && SchedAllowsControl ) SurfaceWindow( ISurf ).ShadingFlag = ShType;
					} else { // Day
						if ( SNLoadCoolRate( IZone ) > 0.0 && SchedAllowsControl ) {
							SurfaceWindow( ISurf ).ShadingFlag = ShType;
						} else if ( GlareControlIsActive ) {
							SurfaceWindow( ISurf ).ShadingFlag = 10 * ShType;
						}
					}
				}

			} else if ( SELECT_CASE_var == WSCT_OnNightIfHeating_OnDayCooling ) { // 'OnNightIfHeatingAndOnDayIfCooling'
				if ( ! BeginSimFlag ) {
					if ( ! SunIsUp ) { // Night
						if ( SNLoadHeatRate( IZone ) > SetPoint && SchedAllowsControl ) SurfaceWindow( ISurf ).ShadingFlag = ShType;
					} else { // Day
						if ( SNLoadCoolRate( IZone ) > 0.0 && SchedAllowsControl ) {
							SurfaceWindow( ISurf ).ShadingFlag = ShType;
						} else if ( GlareControlIsActive ) {
							SurfaceWindow( ISurf ).ShadingFlag = 10 * ShType;
						}
					}
				}

			} else if ( SELECT_CASE_var == WSCT_OffNight_OnDay_HiSolarWindow ) { // 'OffNightAndOnDayIfCoolingAndHighSolarOnWindow'
				if ( ! BeginSimFlag ) {
					if ( SunIsUp && SNLoadCoolRate( IZone ) > 0.0 && SchedAllowsControl ) {
						if ( SolarOnWindow > SetPoint ) SurfaceWindow( ISurf ).ShadingFlag = ShType;
					} else if ( GlareControlIsActive ) {
						SurfaceWindow( ISurf ).ShadingFlag = 10 * ShType;
					}
				}

			} else if ( SELECT_CASE_var == WSCT_OnNight_OnDay_HiSolarWindow ) { // 'OnNightAndOnDayIfCoolingAndHighSolarOnWindow'
				if ( ! BeginSimFlag ) {
					if ( SunIsUp && SNLoadCoolRate( IZone ) > 0.0 && SchedAllowsControl ) {
						if ( SolarOnWindow > SetPoint ) SurfaceWindow( ISurf ).ShadingFlag = ShType;
					} else if ( ! SunIsUp && SchedAllowsControl ) {
						SurfaceWindow( ISurf ).ShadingFlag = ShType;
					} else if ( GlareControlIsActive ) {
						SurfaceWindow( ISurf ).ShadingFlag = 10 * ShType;
					}
				}

			}}

			// Set switching factor to fully switched if ShadingFlag = 2
			if ( SurfaceWindow( ISurf ).ShadingFlag == SwitchableGlazing ) {
				SurfaceWindow( ISurf ).SwitchingFactor = 1.0;

				// Added TH 1/20/2010
				// Vis trans at normal incidence of fully switched glass
				IConst = Surface( ISurf ).ShadedConstruction;
				SurfaceWindow( ISurf ).VisTransSelected = POLYF( 1.0, Construct( IConst ).TransVisBeamCoef ) * SurfaceWindow( ISurf ).GlazedFrac;
			}

			// Slat angle control for blinds

			SurfaceWindow( ISurf ).SlatAngThisTS = 0.0;
			SurfaceWindow( ISurf ).SlatAngThisTSDeg = 0.0;
			SurfaceWindow( ISurf ).SlatsBlockBeam = false;
			if ( SurfaceWindow( ISurf ).ShadingFlag == IntBlindOn || SurfaceWindow( ISurf ).ShadingFlag == 10 * IntBlindOn || SurfaceWindow( ISurf ).ShadingFlag == ExtBlindOn || SurfaceWindow( ISurf ).ShadingFlag == 10 * ExtBlindOn || SurfaceWindow( ISurf ).ShadingFlag == BGBlindOn || SurfaceWindow( ISurf ).ShadingFlag == 10 * BGBlindOn ) {
				// Blind in place or may be in place due to glare control
				BlNum = SurfaceWindow( ISurf ).BlindNumber;
				if ( BlNum > 0 ) {
					InputSlatAngle = Blind( BlNum ).SlatAngle * DegToRadians;

					if ( Blind( BlNum ).SlatWidth > Blind( BlNum ).SlatSeparation && BeamSolarOnWindow > 0.0 ) {
						ProfileAngle( ISurf, SOLCOS, Blind( BlNum ).SlatOrientation, ProfAng );
						ThetaBase = std::acos( std::cos( ProfAng ) * Blind( BlNum ).SlatSeparation / Blind( BlNum ).SlatWidth );
						// There are two solutions for the slat angle that just blocks beam radiation
						ThetaBlock1 = ProfAng + ThetaBase;
						ThetaBlock2 = ProfAng + Pi - ThetaBase;
						ThetaSmall = min( ThetaBlock1, ThetaBlock2 );
						ThetaBig = max( ThetaBlock1, ThetaBlock2 );
						ThetaMin = Blind( BlNum ).MinSlatAngle * DegToRadians;
						ThetaMax = Blind( BlNum ).MaxSlatAngle * DegToRadians;
					}

					// TH 5/20/2010, CR 8064: Slat Width <= Slat Separation
					if ( Blind( BlNum ).SlatWidth <= Blind( BlNum ).SlatSeparation && BeamSolarOnWindow > 0.0 ) {
						if ( WindowShadingControl( IShadingCtrl ).SlatAngleControlForBlinds == WSC_SAC_BlockBeamSolar ) {

							ProfileAngle( ISurf, SOLCOS, Blind( BlNum ).SlatOrientation, ProfAng );

							if ( std::abs( std::cos( ProfAng ) * Blind( BlNum ).SlatSeparation / Blind( BlNum ).SlatWidth ) <= 1.0 ) {
								// set to block 100% of beam solar, not necessarily to block maximum solar (beam + diffuse)
								ThetaBase = std::acos( std::cos( ProfAng ) * Blind( BlNum ).SlatSeparation / Blind( BlNum ).SlatWidth );
								SurfaceWindow( ISurf ).SlatsBlockBeam = true;
							} else {
								// cannot block 100% of beam solar, turn slats to be perpendicular to sun beam to block maximal beam solar
								ThetaBase = 0.0;
							}

							// There are two solutions for the slat angle that just blocks beam radiation
							ThetaBlock1 = ProfAng + ThetaBase;
							ThetaBlock2 = ProfAng - ThetaBase + Pi;

							ThetaSmall = min( ThetaBlock1, ThetaBlock2 );
							ThetaBig = max( ThetaBlock1, ThetaBlock2 );
							ThetaMin = Blind( BlNum ).MinSlatAngle * DegToRadians;
							ThetaMax = Blind( BlNum ).MaxSlatAngle * DegToRadians;
						}
					}

					{ auto const SELECT_CASE_var( WindowShadingControl( IShadingCtrl ).SlatAngleControlForBlinds );

					if ( SELECT_CASE_var == WSC_SAC_FixedSlatAngle ) { // 'FIXEDSLATANGLE'
						SurfaceWindow( ISurf ).SlatAngThisTS = InputSlatAngle;
						if ( ( SurfaceWindow( ISurf ).SlatAngThisTS <= ThetaSmall || SurfaceWindow( ISurf ).SlatAngThisTS >= ThetaBig ) && ( Blind( BlNum ).SlatWidth > Blind( BlNum ).SlatSeparation ) && ( BeamSolarOnWindow > 0.0 ) ) SurfaceWindow( ISurf ).SlatsBlockBeam = true;

					} else if ( SELECT_CASE_var == WSC_SAC_ScheduledSlatAngle ) { // 'SCHEDULEDSLATANGLE'
						SurfaceWindow( ISurf ).SlatAngThisTS = GetCurrentScheduleValue( WindowShadingControl( IShadingCtrl ).SlatAngleSchedule );
						SurfaceWindow( ISurf ).SlatAngThisTS = max( Blind( BlNum ).MinSlatAngle, min( SurfaceWindow( ISurf ).SlatAngThisTS, Blind( BlNum ).MaxSlatAngle ) ) * DegToRadians;
						if ( ( SurfaceWindow( ISurf ).SlatAngThisTS <= ThetaSmall || SurfaceWindow( ISurf ).SlatAngThisTS >= ThetaBig ) && ( Blind( BlNum ).SlatWidth > Blind( BlNum ).SlatSeparation ) && ( BeamSolarOnWindow > 0.0 ) ) SurfaceWindow( ISurf ).SlatsBlockBeam = true;

					} else if ( SELECT_CASE_var == WSC_SAC_BlockBeamSolar ) { // 'BLOCKBEAMSOLAR'
						if ( BeamSolarOnWindow > 0.0 ) {
							if ( Blind( BlNum ).SlatSeparation >= Blind( BlNum ).SlatWidth ) {
								// TH 5/20/2010. CR 8064.
								// The following line of code assumes slats are always vertical/closed to minimize solar penetration
								// The slat angle can however change if the only goal is to block maximum amount of direct beam solar
								//SurfaceWindow(ISurf)%SlatAngThisTS = 0.0  ! Allows beam penetration but minimizes it

								if ( ThetaSmall >= ThetaMin && ThetaSmall <= ThetaMax ) {
									SurfaceWindow( ISurf ).SlatAngThisTS = ThetaSmall;
								} else if ( ThetaBig >= ThetaMin && ThetaBig <= ThetaMax ) {
									SurfaceWindow( ISurf ).SlatAngThisTS = ThetaBig;
								} else if ( ThetaSmall < ThetaMin && ThetaBig < ThetaMin ) {
									SurfaceWindow( ISurf ).SlatAngThisTS = ThetaMin;
								} else if ( ThetaSmall > ThetaMax && ThetaBig > ThetaMax ) {
									SurfaceWindow( ISurf ).SlatAngThisTS = ThetaMax;
								} else { // ThetaBig > ThetaMax and ThetaSmall < ThetaMin (no-block condition)
									SurfaceWindow( ISurf ).SlatAngThisTS = ThetaMin;
								}

							} else { // Usual case -- slat width greater than slat separation
								if ( ThetaSmall >= ThetaMin && ThetaSmall <= ThetaMax ) {
									SurfaceWindow( ISurf ).SlatAngThisTS = ThetaSmall;
									SurfaceWindow( ISurf ).SlatsBlockBeam = true;
								} else if ( ThetaBig >= ThetaMin && ThetaBig <= ThetaMax ) {
									SurfaceWindow( ISurf ).SlatAngThisTS = ThetaBig;
									SurfaceWindow( ISurf ).SlatsBlockBeam = true;
								} else if ( ThetaSmall < ThetaMin && ThetaBig < ThetaMin ) {
									SurfaceWindow( ISurf ).SlatAngThisTS = ThetaMin;
									SurfaceWindow( ISurf ).SlatsBlockBeam = true;
								} else if ( ThetaSmall > ThetaMax && ThetaBig > ThetaMax ) {
									SurfaceWindow( ISurf ).SlatAngThisTS = ThetaMax;
									SurfaceWindow( ISurf ).SlatsBlockBeam = true;
								} else { // ThetaBig > ThetaMax and ThetaSmall < ThetaMin (no-block condition)
									SurfaceWindow( ISurf ).SlatAngThisTS = ThetaMin;
								}
							}
						} else {
							SurfaceWindow( ISurf ).SlatAngThisTS = InputSlatAngle;
						}

					}}

					SurfaceWindow( ISurf ).SlatAngThisTSDeg = SurfaceWindow( ISurf ).SlatAngThisTS / DegToRadians;
					if ( SurfaceWindow( ISurf ).SlatAngThisTSDegEMSon ) {
						SurfaceWindow( ISurf ).SlatAngThisTSDeg = SurfaceWindow( ISurf ).SlatAngThisTSDegEMSValue;
						SurfaceWindow( ISurf ).SlatAngThisTS = DegToRadians * SurfaceWindow( ISurf ).SlatAngThisTSDeg;
					}
					// Air flow permeability for calculation of convective air flow between blind and glass
					SlatAng = SurfaceWindow( ISurf ).SlatAngThisTS;
					PermeabilityA = std::sin( SlatAng ) - Blind( BlNum ).SlatThickness / Blind( BlNum ).SlatSeparation;
					PermeabilityB = 1.0 - ( std::abs( Blind( BlNum ).SlatWidth * std::cos( SlatAng ) ) + Blind( BlNum ).SlatThickness * std::sin( SlatAng ) ) / Blind( BlNum ).SlatSeparation;
					SurfaceWindow( ISurf ).BlindAirFlowPermeability = min( 1.0, max( 0.0, PermeabilityA, PermeabilityB ) );
				}
			} // End of check if interior or exterior blind in place

			//   CALL CalcScreenTransmittance to intialized all screens prior to HB calc's
			if ( SurfaceWindow( ISurf ).ShadingFlag == ExtScreenOn && SunIsUp ) {
				CalcScreenTransmittance( ISurf );
			}

			// EMS Actuator Point: override setting if ems flag on
			if ( SurfaceWindow( ISurf ).ShadingFlagEMSOn ) {
				SurfaceWindow( ISurf ).ShadingFlag = SurfaceWindow( ISurf ).ShadingFlagEMSValue;
			}

		} //End of surface loop
	}

	void
	WindowGapAirflowControl()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   February 2003
		//       MODIFIED       June 2003, FCW: add fatal error for illegal schedule value
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// For airflow windows, determines the airflow in the gap of
		// double glazing and in the inner gap of triple glazing.

		// REFERENCES:
		// na

		// Using/Aliasing
		using ScheduleManager::GetCurrentScheduleValue;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na
		// INTERFACE BLOCK SPECIFICATIONS
		// na
		// DERIVED TYPE DEFINITIONS
		// na
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int ISurf; // Surface counter
		int SchedulePtr; // Schedule pointer
		Real64 ScheduleMult; // Multiplier value from schedule

		for ( ISurf = 1; ISurf <= TotSurfaces; ++ISurf ) {

			if ( Surface( ISurf ).Class != SurfaceClass_Window ) continue;

			SurfaceWindow( ISurf ).AirflowThisTS = 0.0;
			if ( SurfaceWindow( ISurf ).MaxAirflow == 0.0 ) continue;
			if ( Surface( ISurf ).ExtBoundCond != ExternalEnvironment ) continue;

			{ auto const SELECT_CASE_var( SurfaceWindow( ISurf ).AirflowControlType );

			if ( SELECT_CASE_var == AirFlowWindow_ControlType_MaxFlow ) {
				SurfaceWindow( ISurf ).AirflowThisTS = SurfaceWindow( ISurf ).MaxAirflow;

			} else if ( SELECT_CASE_var == AirFlowWindow_ControlType_AlwaysOff ) {
				SurfaceWindow( ISurf ).AirflowThisTS = 0.0;

			} else if ( SELECT_CASE_var == AirFlowWindow_ControlType_Schedule ) {
				if ( SurfaceWindow( ISurf ).AirflowHasSchedule ) {
					SchedulePtr = SurfaceWindow( ISurf ).AirflowSchedulePtr;
					ScheduleMult = GetCurrentScheduleValue( SchedulePtr );
					if ( ScheduleMult < 0.0 || ScheduleMult > 1.0 ) {
						ShowFatalError( "Airflow schedule has a value outside the range 0.0 to 1.0 for window=" + Surface( ISurf ).Name );
					}
					SurfaceWindow( ISurf ).AirflowThisTS = ScheduleMult * SurfaceWindow( ISurf ).MaxAirflow;
				}

			}}

		} // End of surface loop

	}

	void
	SkyDifSolarShading()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   May 1999
		//       MODIFIED       Sep 2000, FCW: add IR view factor calc
		//                      Sep 2002, FCW: correct error in expression for ground IR view factor.
		//                         Affects only non-vertical surfaces that are shadowed. For these surfaces
		//                         error caused underestimate of IR from ground and shadowing surfaces.
		//                      Dec 2002; LKL: Sky Radiance Distribution now only anisotropic
		//                      Nov 2003: FCW: modify to do sky solar shading of shadowing surfaces
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates factors that account for shading of sky diffuse
		// solar radiation by shadowing surfaces such as overhangs and detached
		// shades.
		// Called by PerformSolarCalculations
		// For each exterior heat transfer surface calculates the following
		// ratio (called DifShdgRatioIsoSky in this subroutine):
		//  R1 = (Diffuse solar from sky dome on surface, with shading)/
		//       (Diffuse solar from sky dome on surface, without shading)
		// To calculate the incident diffuse radiation on a surface the sky
		// hemisphere is divided into source elements ("patches"). Each patch
		// is assumed to have the same radiance, i.e. the sky radiance is isotropic.
		// The irradiance from each patch on a surface is calculated. Then these
		// irradiances are summed to get the net irradiance on a surface, which
		// the denominator of R1.
		// To get the numerator of R1 the same summation is done, but for each surface
		// and each patch the Shadow subroutine is called to determine how much
		// radiation from a patch is blocked by shading surfaces.
		// Also calculated is the following ratio (called DifShdgRatioHoriz in this routine):
		//  R2 = (Diffuse solar from sky horizon band on surface, with shading)/
		//       (Diffuse solar from sky horizon band on surface, without shading)
		// For this ratio only a band of sky just above the horizon is considered.
		// R1 and R2 are used in SUBROUTINE AnisoSkyViewFactors, which determines the
		// sky diffuse solar irradiance on each exterior heat transfer surface each
		// time step. In that routine the sky radiance distribution is a superposition
		// of an isotropic distribution,
		// a horizon brightening distribution and a circumsolar brightening distribution,
		// where the proportion of each distribution depends
		// on cloud cover, sun position and other factors. R1 multiplies the irradiance
		// due to the isotropic component and R2 multiplies the irradiance due to the
		// horizon brightening component.
		// Calculates sky and ground IR view factors assuming sky IR is isotropic and
		// shadowing surfaces are opaque to IR.
		// REFERENCES:
		// na

		// Using/Aliasing
		using DataSystemVariables::DetailedSkyDiffuseAlgorithm;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		int const NPhi( 6 ); // Number of altitude angle steps for sky integration
		int const NTheta( 24 ); // Number of azimuth angle steps for sky integration
		Real64 const Eps( 1.e-10 ); // Small number

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int SurfNum; // Surface counter
		int IPhi; // Altitude step counter
		int ITheta; // Azimuth step counter
		Real64 DPhi; // Altitude step size
		Real64 DTheta; // Azimuth step size
		Real64 DThetaDPhi; // Product of DTheta and DPhi
		Real64 PhiMin; // Minimum altitude
		Real64 Phi; // Altitude angle
		Real64 Theta; // Azimuth angle
		Real64 CosPhi; // Cosine of Phi
		Real64 Fac1WoShdg; // Intermediate calculation factor, without shading
		Real64 FracIlluminated; // Fraction of surface area illuminated by a sky patch
		Real64 Fac1WithShdg; // Intermediate calculation factor, with shading
		Real64 SurfArea; // Surface area (m2)
		bool ShadowingSurf; // True if surface is a shadowing surface
		//REAL(r64), ALLOCATABLE, DIMENSION(:) :: WithShdgIsoSky     ! Diffuse solar irradiance from isotropic
		//                                                          ! sky on surface, with shading
		//REAL(r64), ALLOCATABLE, DIMENSION(:) :: WoShdgIsoSky       ! Diffuse solar from isotropic
		//                                                           ! sky on surface, without shading
		//REAL(r64), ALLOCATABLE, DIMENSION(:) :: WithShdgHoriz      ! Diffuse solar irradiance from horizon portion of
		//                                                           ! sky on surface, with shading
		//REAL(r64), ALLOCATABLE, DIMENSION(:) :: WoShdgHoriz        ! Diffuse solar irradiance from horizon portion of
		//                                                           ! sky on surface, without shading
		//INTEGER iHour,iTS

		// FLOW:

		// Initialize Surfaces Arrays
		SAREA = 0.0;
		WithShdgIsoSky.dimension( TotSurfaces, 0.0 );
		WoShdgIsoSky.dimension( TotSurfaces, 0.0 );
		WithShdgHoriz.dimension( TotSurfaces, 0.0 );
		WoShdgHoriz.dimension( TotSurfaces, 0.0 );
		DifShdgRatioIsoSky.allocate( TotSurfaces );
		DifShdgRatioHoriz.allocate( TotSurfaces );
		// initialized as no shading
		DifShdgRatioIsoSky = 1.0;
		DifShdgRatioHoriz = 1.0;
		if ( DetailedSkyDiffuseAlgorithm && ShadingTransmittanceVaries && SolarDistribution != MinimalShadowing ) {
			curDifShdgRatioIsoSky.dimension( TotSurfaces, 1.0 );
		}

		// only for detailed.
		if ( DetailedSkyDiffuseAlgorithm && ShadingTransmittanceVaries && SolarDistribution != MinimalShadowing ) {
			DifShdgRatioIsoSkyHRTS.allocate( NumOfTimeStepInHour, 24, TotSurfaces );
			DifShdgRatioIsoSkyHRTS = 1.0;
			DifShdgRatioHorizHRTS.allocate( NumOfTimeStepInHour, 24, TotSurfaces );
			DifShdgRatioHorizHRTS = 1.0;
		}

		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			if ( ! Surface( SurfNum ).ExtSolar ) continue;

			// CurrentModuleObject='Surfaces'
			if ( DetailedSkyDiffuseAlgorithm && ShadingTransmittanceVaries && SolarDistribution != MinimalShadowing ) {
				SetupOutputVariable( "Debug Surface Solar Shading Model DifShdgRatioIsoSky []", curDifShdgRatioIsoSky( SurfNum ), "Zone", "Average", Surface( SurfNum ).Name );
			} else {
				SetupOutputVariable( "Debug Surface Solar Shading Model DifShdgRatioIsoSky []", DifShdgRatioIsoSky( SurfNum ), "Zone", "Average", Surface( SurfNum ).Name );
			}
			SetupOutputVariable( "Debug Surface Solar Shading Model DifShdgRatioHoriz []", DifShdgRatioHoriz( SurfNum ), "Zone", "Average", Surface( SurfNum ).Name );
			SetupOutputVariable( "Debug Surface Solar Shading Model WithShdgIsoSky []", WithShdgIsoSky( SurfNum ), "Zone", "Average", Surface( SurfNum ).Name );
			SetupOutputVariable( "Debug Surface Solar Shading Model WoShdgIsoSky []", WoShdgIsoSky( SurfNum ), "Zone", "Average", Surface( SurfNum ).Name );
		}

		DPhi = PiOvr2 / NPhi; // 15 deg for NPhi = 6
		DTheta = 2.0 * Pi / NTheta; // 15 deg for NTheta = 24
		DThetaDPhi = DTheta * DPhi;
		PhiMin = 0.5 * DPhi; // 7.5 deg for DPhi = 15 deg

		for ( IPhi = 1; IPhi <= NPhi; ++IPhi ) { // Loop over patch altitude values
			Phi = PhiMin + ( IPhi - 1 ) * DPhi; // 7.5,22.5,37.5,52.5,67.5,82.5 for NPhi = 6
			SUNCOS( 3 ) = std::sin( Phi );
			CosPhi = std::cos( Phi );

			for ( ITheta = 1; ITheta <= NTheta; ++ITheta ) { // Loop over patch azimuth values
				Theta = ( ITheta - 1 ) * DTheta; // 0,15,30,....,330,345 for NTheta = 24
				SUNCOS( 1 ) = CosPhi * std::cos( Theta );
				SUNCOS( 2 ) = CosPhi * std::sin( Theta );

				for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) { // Cosine of angle of incidence on surface of solar
					// radiation from patch
					ShadowingSurf = Surface( SurfNum ).ShadowingSurf;

					if ( ! ShadowingSurf && ! Surface( SurfNum ).HeatTransSurf ) continue;

					CTHETA( SurfNum ) = SUNCOS( 1 ) * Surface( SurfNum ).OutNormVec( 1 ) + SUNCOS( 2 ) * Surface( SurfNum ).OutNormVec( 2 ) + SUNCOS( 3 ) * Surface( SurfNum ).OutNormVec( 3 );
				}

				SHADOW( 0, 0 );

				for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
					ShadowingSurf = Surface( SurfNum ).ShadowingSurf;

					if ( ! ShadowingSurf && ( ! Surface( SurfNum ).HeatTransSurf || ! Surface( SurfNum ).ExtSolar || ( Surface( SurfNum ).ExtBoundCond != ExternalEnvironment && Surface( SurfNum ).ExtBoundCond != OtherSideCondModeledExt ) ) ) continue;

					if ( CTHETA( SurfNum ) < 0.0 ) continue;

					Fac1WoShdg = CosPhi * DThetaDPhi * CTHETA( SurfNum );
					SurfArea = Surface( SurfNum ).NetAreaShadowCalc;
					if ( SurfArea > Eps ) {
						FracIlluminated = SAREA( SurfNum ) / SurfArea;
					} else {
						FracIlluminated = SAREA( SurfNum ) / ( SurfArea + Eps );
					}
					Fac1WithShdg = Fac1WoShdg * FracIlluminated;
					WithShdgIsoSky( SurfNum ) += Fac1WithShdg;
					WoShdgIsoSky( SurfNum ) += Fac1WoShdg;

					// Horizon region
					if ( IPhi == 1 ) {
						WithShdgHoriz( SurfNum ) += Fac1WithShdg;
						WoShdgHoriz( SurfNum ) += Fac1WoShdg;
					}
				} // End of surface loop
			} // End of Theta loop
		} // End of Phi loop

		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			ShadowingSurf = Surface( SurfNum ).ShadowingSurf;

			if ( ! ShadowingSurf && ( ! Surface( SurfNum ).HeatTransSurf || ! Surface( SurfNum ).ExtSolar || ( Surface( SurfNum ).ExtBoundCond != ExternalEnvironment && Surface( SurfNum ).ExtBoundCond != OtherSideCondModeledExt ) ) ) continue;

			if ( std::abs( WoShdgIsoSky( SurfNum ) ) > Eps ) {
				DifShdgRatioIsoSky( SurfNum ) = ( WithShdgIsoSky( SurfNum ) ) / ( WoShdgIsoSky( SurfNum ) );
			} else {
				DifShdgRatioIsoSky( SurfNum ) = ( WithShdgIsoSky( SurfNum ) ) / ( WoShdgIsoSky( SurfNum ) + Eps );
			}
			if ( std::abs( WoShdgHoriz( SurfNum ) ) > Eps ) {
				DifShdgRatioHoriz( SurfNum ) = ( WithShdgHoriz( SurfNum ) ) / ( WoShdgHoriz( SurfNum ) );
			} else {
				DifShdgRatioHoriz( SurfNum ) = ( WithShdgHoriz( SurfNum ) ) / ( WoShdgHoriz( SurfNum ) + Eps );
			}
		}

		// Get IR view factors. An exterior surface can receive IR radiation from
		// sky, ground or shadowing surfaces. Assume shadowing surfaces have same
		// temperature as outside air (and therefore same temperature as ground),
		// so that the view factor to these shadowing surfaces can be included in
		// the ground view factor. Sky IR is assumed to be isotropic and shadowing
		// surfaces are assumed to be opaque to IR so they totally "shade" IR from
		// sky or ground.

		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			if ( ! DetailedSkyDiffuseAlgorithm || ! ShadingTransmittanceVaries || SolarDistribution == MinimalShadowing ) {
				Surface( SurfNum ).ViewFactorSkyIR *= DifShdgRatioIsoSky( SurfNum );
			} else {
				Surface( SurfNum ).ViewFactorSkyIR *= DifShdgRatioIsoSkyHRTS( 1, 1, SurfNum );
			}
			Surface( SurfNum ).ViewFactorGroundIR = 1.0 - Surface( SurfNum ).ViewFactorSkyIR;
		}

		//  DEALLOCATE(WithShdgIsoSky)
		//  DEALLOCATE(WoShdgIsoSky)
		//  DEALLOCATE(WithShdgHoriz)
		//  DEALLOCATE(WoShdgHoriz)

		if ( DetailedSkyDiffuseAlgorithm && ShadingTransmittanceVaries && SolarDistribution != MinimalShadowing ) {
			for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
				DifShdgRatioIsoSkyHRTS( {1,NumOfTimeStepInHour}, {1,24}, SurfNum ) = DifShdgRatioIsoSky( SurfNum );
				DifShdgRatioHorizHRTS( {1,NumOfTimeStepInHour}, {1,24}, SurfNum ) = DifShdgRatioHoriz( SurfNum );
			}
		}

	}

	void
	CalcWindowProfileAngles()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   April 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na
		// PURPOSE OF THIS SUBROUTINE:
		// Called by CalcPerSolarBeam for wholly or partially sunlit exterior windows
		// Calculates horizontal and vertical beam solar profile angles

		// REFERENCES: na
		// USE STATEMENTS: na

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS: na
		// INTERFACE BLOCK SPECIFICATIONS: na
		// DERIVED TYPE DEFINITIONS: na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int SurfNum; // Surface number
		Real64 ElevSun; // Sun elevation; angle between sun and horizontal
		Real64 ElevWin; // Window elevation: angle between window outward normal and horizontal
		Real64 AzimWin; // Window azimuth (radians)
		Real64 AzimSun; // Sun azimuth (radians)
		Real64 ProfileAngHor; // Solar profile angle (radians) for horizontally oriented window elements
		// such as the top and bottom of a frame.
		// This is the incidence angle in a plane that is normal to the window
		// and parallel to the Y-axis of the window (the axis along
		// which the height of the window is measured).
		Real64 ProfileAngVert; // Solar profile angle (radians) for vertically oriented elements
		// such as the sides of a frame.
		// This is the incidence angle in a plane that is normal to the window
		// and parallel to the X-axis of the window (the axis along
		// which the width of the window is measured).
		Vector3< Real64 > WinNorm; // Unit vector normal to window
		Vector3< Real64 > WinNormCrossBase; // Cross product of WinNorm and vector along window baseline
		Vector3< Real64 > SunPrime; // Projection of sun vector onto plane (perpendicular to
		Vector3< Real64 > const SolCosVec( SOLCOS ); // Local Vector3 copy for speed (until SOLCOS mig to Vector3)
		//  window plane) determined by WinNorm and vector along
		//  baseline of window
		Real64 ThWin; // Azimuth angle of WinNorm (radians)
		Real64 dot1;
		Real64 dot2;
		Real64 dot3;

		ElevSun = PiOvr2 - std::acos( SolCosVec.z );
		AzimSun = std::atan2( SolCosVec.x, SolCosVec.y );

		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {

			if ( Surface( SurfNum ).Class != SurfaceClass_Window || ( Surface( SurfNum ).ExtBoundCond != ExternalEnvironment && Surface( SurfNum ).ExtBoundCond != OtherSideCondModeledExt ) ) continue;

			SurfaceWindow( SurfNum ).ProfileAngHor = 0.0;
			SurfaceWindow( SurfNum ).ProfileAngVert = 0.0;
			if ( CosIncAng( TimeStep, HourOfDay, SurfNum ) <= 0.0 ) continue;

			ElevWin = PiOvr2 - Surface( SurfNum ).Tilt * DegToRadians;
			AzimWin = Surface( SurfNum ).Azimuth * DegToRadians;

			ProfileAngHor = std::atan( std::sin( ElevSun ) / std::abs( std::cos( ElevSun ) * std::cos( AzimWin - AzimSun ) ) ) - ElevWin;

			//CR9280 - were having negative profile angles on west sides.  commenting out previous code (original code) for
			// vertical windows
			//  IF(ABS(ElevWin) < 0.1d0) THEN  ! Near-vertical window
			//    ProfileAngVert = ABS(AzimWin-AzimSun)
			//  ELSE
			WinNorm = Surface( SurfNum ).OutNormVec;
			ThWin = AzimWin - PiOvr2;
			Real64 const sin_Elevwin( std::sin( ElevWin ) );
			WinNormCrossBase.x = -( sin_Elevwin * std::cos( ThWin ) );
			WinNormCrossBase.y = sin_Elevwin * std::sin( ThWin );
			WinNormCrossBase.z = std::cos( ElevWin );
			SunPrime = SolCosVec - WinNormCrossBase * dot( SolCosVec, WinNormCrossBase );
			dot1 = dot( WinNorm, SunPrime );
			dot2 = SunPrime.magnitude();
			dot3 = dot1 / dot2;
			if ( dot3 > 1.0 ) {
				dot3 = 1.0;
			} else if ( dot3 < -1.0 ) {
				dot3 = -1.0;
			}
			//    ProfileAngVert = ABS(ACOS(DOT_PRODUCT(WinNorm,SunPrime)/SQRT(DOT_PRODUCT(SunPrime,SunPrime))))
			ProfileAngVert = std::abs( std::acos( dot3 ) );
			//  END IF
			// Constrain to 0 to pi
			if ( ProfileAngVert > Pi ) ProfileAngVert = TwoPi - ProfileAngVert;

			SurfaceWindow( SurfNum ).ProfileAngHor = ProfileAngHor / DegToRadians;
			SurfaceWindow( SurfNum ).ProfileAngVert = ProfileAngVert / DegToRadians;
			SurfaceWindow( SurfNum ).TanProfileAngHor = std::abs( std::tan( ProfileAngHor ) );
			SurfaceWindow( SurfNum ).TanProfileAngVert = std::abs( std::tan( ProfileAngVert ) );

		}

	}

	void
	CalcFrameDividerShadow(
		int const SurfNum, // Surface number
		int const FrDivNum, // Frame/divider number
		int const HourNum // Hour number
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   June 2000
		//       MODIFIED       Aug 2000, FW: add effective shadowing by inside
		//                      projections
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Called by CalcPerSolarBeam for wholly or partially sunlit exterior windows
		// with a frame and/or divider. Using beam solar profile angles,
		// calculates fraction of glass shaded by exterior frame and divider projections,
		// The frame and divider profiles are assumed to be rectangular.
		// A similar shadowing approach is used to calculate the fraction of glass area
		// that produces beam solar illumination on interior frame and divider projections.
		// This fraction is used in CalcWinFrameAndDividerTemps to determine the
		// beam solar absorbed by inside projections. Beam solar reflected by inside projections
		// is assumed to stay in the zone (as beam solar) although in actuality roughly
		// half of this is reflected back onto the glass and the half that is reflected
		// into the zone is diffuse.
		// For multipane glazing the effect of solar absorbed by the exposed portion of
		// frame or divider between the panes is not calculated. Beam solar incident on
		// these portions is assumed to be transmitted into the zone unchanged.
		// The shadowing of diffuse solar radiation by projections is not considered.

		// REFERENCES: na
		// USE STATEMENTS: na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS: na
		// INTERFACE BLOCK SPECIFICATIONS: na
		// DERIVED TYPE DEFINITIONS: na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 ElevSun; // Sun elevation; angle between sun and horizontal
		Real64 ElevWin; // Window elevation: angle between window outward normal and horizontal
		Real64 AzimWin; // Window azimuth (radians)
		Real64 AzimSun; // Sun azimuth (radians)
		Real64 ProfileAngHor; // Solar profile angle (radians) for horizontally oriented projections
		// such as the top and bottom of a frame or horizontal dividers.
		// This is the incidence angle in a plane that is normal to the window
		// and parallel to the Y-axis of the window (the axis along
		// which the height of the window is measured).
		Real64 ProfileAngVert; // Solar profile angle (radians) for vertically oriented projections
		// such as the top and bottom of a frame or horizontal dividers.
		// This is the incidence angle in a plane that is normal to the window
		// and parallel to the X-axis of the window (the axis along
		// which the width of the window is measured).
		Real64 TanProfileAngHor; // Tangent of ProfileAngHor
		Real64 TanProfileAngVert; // Tangent of ProfileAngVert
		Real64 FrWidth; // Frame width (m)
		Real64 DivWidth; // Divider width (m)
		Real64 FrProjOut; // Outside frame projection (m)
		Real64 DivProjOut; // Outside divider projection (m)
		Real64 FrProjIn; // Inside frame projection (m)
		Real64 DivProjIn; // Inside divider projection (m)
		int NHorDiv; // Number of horizontal dividers
		int NVertDiv; // Number of vertical dividers
		Real64 GlArea; // Glazed area (m2)
		Real64 Arealite; // Area of a single lite of glass (m2); glazed area, GlArea,
		// if there is no divider (in which case there is only one lite).
		Real64 ArealiteCol; // Area of a vertical column of lites (m2)
		Real64 ArealiteRow; // Area of a horizontal row of lites (m2)
		Real64 AshVDout; // Shaded area from all vertical divider outside projections (m2)
		Real64 AshVDin; // Shaded area from all vertical divider inside projections (m2)
		Real64 AshHDout; // Shaded area from all horizontal divider outside projections (m2)
		Real64 AshHDin; // Shaded area from all horizontal divider inside projections (m2)
		Real64 AshVFout; // Shaded area from outside projection of vertical sides of frame (m2)
		Real64 AshVFin; // Shaded area from inside projection of vertical sides of frame (m2)
		Real64 AshHFout; // Shaded area from outside projection of horizontal sides
		//   (top) of frame (m2)
		Real64 AshHFin; // Shaded area from inside projection of horizontal sides
		//   (top) of frame (m2)
		Real64 AshDDover; // Divider/divider shadow overlap area (m2)
		Real64 AshFFover; // Frame/frame shadow overlap area (m2)
		Real64 AshFVDover; // Frame/vertical divider overlap area (m2)
		Real64 AshFHDover; // Frame/horizontal divider overlap area (m2)
		Real64 AshFDtotOut; // Total outside projection shadow area (m2)
		Real64 AshFDtotIn; // Total inside projection shadow area (m2)
		Real64 FracShFDOut; // Fraction of glazing shadowed by frame and divider
		//  outside projections
		Real64 FracShFDin; // Fraction of glazing that illuminates frame and divider
		//  inside projections with beam radiation

		Array1D< Real64 > WinNorm( 3 ); // Window outward normal unit vector
		Real64 ThWin; // Azimuth angle of WinNorm
		Array1D< Real64 > SunPrime( 3 ); // Projection of sun vector onto plane (perpendicular to
		//  window plane) determined by WinNorm and vector along
		//  baseline of window
		Array1D< Real64 > WinNormCrossBase( 3 ); // Cross product of WinNorm and vector along window baseline

		if ( FrameDivider( FrDivNum ).FrameProjectionOut == 0.0 && FrameDivider( FrDivNum ).FrameProjectionIn == 0.0 && FrameDivider( FrDivNum ).DividerProjectionOut == 0.0 && FrameDivider( FrDivNum ).DividerProjectionIn == 0.0 ) return;

		FrProjOut = FrameDivider( FrDivNum ).FrameProjectionOut;
		FrProjIn = FrameDivider( FrDivNum ).FrameProjectionIn;
		DivProjOut = FrameDivider( FrDivNum ).DividerProjectionOut;
		DivProjIn = FrameDivider( FrDivNum ).DividerProjectionIn;

		GlArea = Surface( SurfNum ).Area;
		ElevWin = PiOvr2 - Surface( SurfNum ).Tilt * DegToRadians;
		ElevSun = PiOvr2 - std::acos( SUNCOS( 3 ) );
		AzimWin = Surface( SurfNum ).Azimuth * DegToRadians;
		AzimSun = std::atan2( SUNCOS( 1 ), SUNCOS( 2 ) );

		ProfileAngHor = std::atan( std::sin( ElevSun ) / std::abs( std::cos( ElevSun ) * std::cos( AzimWin - AzimSun ) ) ) - ElevWin;
		if ( std::abs( ElevWin ) < 0.1 ) { // Near-vertical window
			ProfileAngVert = std::abs( AzimWin - AzimSun );
		} else {
			WinNorm = Surface( SurfNum ).OutNormVec;
			ThWin = AzimWin - PiOvr2;
			WinNormCrossBase( 1 ) = -std::sin( ElevWin ) * std::cos( ThWin );
			WinNormCrossBase( 2 ) = std::sin( ElevWin ) * std::sin( ThWin );
			WinNormCrossBase( 3 ) = std::cos( ElevWin );
			SunPrime = SUNCOS - WinNormCrossBase * dot( SUNCOS, WinNormCrossBase );
			ProfileAngVert = std::abs( std::acos( dot( WinNorm, SunPrime ) / magnitude( SunPrime ) ) );
		}
		// Constrain to 0 to pi
		if ( ProfileAngVert > Pi ) ProfileAngVert = 2 * Pi - ProfileAngVert;
		TanProfileAngHor = std::abs( std::tan( ProfileAngHor ) );
		TanProfileAngVert = std::abs( std::tan( ProfileAngVert ) );

		NHorDiv = FrameDivider( FrDivNum ).HorDividers;
		NVertDiv = FrameDivider( FrDivNum ).VertDividers;
		FrWidth = FrameDivider( FrDivNum ).FrameWidth;
		DivWidth = FrameDivider( FrDivNum ).DividerWidth;

		Arealite = ( Surface( SurfNum ).Height / ( NHorDiv + 1.0 ) - DivWidth / 2.0 ) * ( Surface( SurfNum ).Width / ( NVertDiv + 1.0 ) - DivWidth / 2.0 );
		if ( DivProjOut > 0.0 || DivProjIn > 0.0 ) {
			ArealiteCol = ( NHorDiv + 1 ) * Arealite;
			ArealiteRow = ( NVertDiv + 1 ) * Arealite;
		} else {
			ArealiteCol = GlArea;
			ArealiteRow = GlArea;
		}
		AshVDout = 0.0;
		AshVDin = 0.0;
		AshHDout = 0.0;
		AshHDin = 0.0;
		AshVFout = 0.0;
		AshVFin = 0.0;
		AshHFout = 0.0;
		AshHFin = 0.0;
		AshDDover = 0.0;
		AshFFover = 0.0;
		AshFVDover = 0.0;
		AshFHDover = 0.0;

		if ( DivProjOut > 0.0 || DivProjIn > 0.0 ) {

			// Shaded area from all vertical dividers
			AshVDout = NVertDiv * min( ( Surface( SurfNum ).Height - NHorDiv * DivWidth ) * DivProjOut * TanProfileAngVert, ArealiteCol );
			AshVDin = NVertDiv * min( ( Surface( SurfNum ).Height - NHorDiv * DivWidth ) * DivProjIn * TanProfileAngVert, ArealiteCol );

			// Shaded area from all horizontal dividers
			AshHDout = NHorDiv * min( ( Surface( SurfNum ).Width - NVertDiv * DivWidth ) * DivProjOut * TanProfileAngHor, ArealiteRow );
			AshHDin = NHorDiv * min( ( Surface( SurfNum ).Width - NVertDiv * DivWidth ) * DivProjIn * TanProfileAngHor, ArealiteRow );

			// Horizontal divider/vertical divider shadow overlap
			AshDDover = min( DivProjOut * TanProfileAngHor * DivProjOut * TanProfileAngVert, Arealite ) * NHorDiv * NVertDiv;

		}

		if ( FrProjOut > 0.0 || FrProjIn > 0.0 ) {

			// Shaded area from sides of frame; to avoid complications from possible overlaps between
			// shadow from side of frame and shadow from vertical divider the shaded area from side of
			// frame is restricted to the area of one column of lites.
			AshVFout = min( ( Surface( SurfNum ).Height - NHorDiv * DivWidth ) * FrProjOut * TanProfileAngVert, ArealiteCol );
			AshVFin = min( ( Surface( SurfNum ).Height - NHorDiv * DivWidth ) * FrProjIn * TanProfileAngVert, ArealiteCol );

			// Shaded area from top or bottom of frame; to avoid complications from possible overlaps
			// between shadow from top or bottom of frame and shadow from horizontal divider, the shaded
			// area from the top or bottom of frame is restricted to the area of one row of lites.
			AshHFout = min( ( Surface( SurfNum ).Width - NVertDiv * DivWidth ) * FrProjOut * TanProfileAngHor, ArealiteRow );
			AshHFin = min( ( Surface( SurfNum ).Width - NVertDiv * DivWidth ) * FrProjIn * TanProfileAngHor, ArealiteRow );

			// Top/bottom of frame/side of frame shadow overlap
			AshFFover = min( FrProjOut * TanProfileAngHor * FrProjOut * TanProfileAngVert, Arealite );

			if ( DivProjOut > 0.0 ) {
				// Frame/vertical divider shadow overlap
				AshFVDover = min( FrProjOut * DivProjOut * TanProfileAngHor * TanProfileAngVert, Arealite ) * NVertDiv;

				// Frame/horizontal divider shadow overlap
				AshFHDover = min( FrProjOut * DivProjOut * TanProfileAngHor * TanProfileAngVert, Arealite ) * NHorDiv;

			}

		}

		AshFDtotOut = AshVDout + AshHDout + AshVFout + AshHFout - ( AshDDover + AshFFover + AshFVDover + AshFHDover );
		AshFDtotIn = ( AshVDin + AshHDin ) * FrameDivider( FrDivNum ).DividerSolAbsorp + ( AshVFin + AshHFin ) * FrameDivider( FrDivNum ).FrameSolAbsorp;

		// Divide by the glazed area of the window
		FracShFDOut = AshFDtotOut / GlArea;
		FracShFDin = AshFDtotIn / GlArea;
		SurfaceWindow( SurfNum ).OutProjSLFracMult( HourNum ) = 1.0 - FracShFDOut;
		SurfaceWindow( SurfNum ).InOutProjSLFracMult( HourNum ) = 1.0 - ( FracShFDin + FracShFDOut );

	}

	void
	CalcBeamSolarOnWinRevealSurface()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         F. Winkelmann
		//       DATE WRITTEN   April 2002
		//       MODIFIED:na
		//       RE-ENGINEERED:na

		// PURPOSE OF THIS SUBROUTINE
		// Called by InitHeatGains when the sun is up.
		// Calculates beam solar radiation absorbed and reflected by top, bottom,
		// right and left sides of outside and inside window reveal surfaces.
		// In doing this calculation, the shadowing on a reveal surface by other reveal surfaces
		// is determined using the orientation of the reveal surfaces and the sun position.
		// It is assumed that:
		// (1) The window is an exterior window and is rectangular.
		// (2) The reveal surfaces are perpendicular to the window plane.
		// (3) If an exterior shade or blind is in place, there is no beam solar on
		//     on exterior or interior reveal surfaces.
		// (3) If an interior shade or blind is in place, there is no beam solar on
		//     interior reveal surfaces.
		// (4) The effect of window divider, if present, is ignored, including shadowing
		//     of divider on inside reveal surfaces.

		// In the variable names, the "subscript" 1 = outside reveal, 2 = inside reveal
		// The outside reveal surfaces (top, bottom, left, right) are assumed to have the same depth
		// (given by Surface%Reveal and determined from vertices of window and vertices of parent
		// wall) and the same solar absorptance. The inside reveal surfaces are divided into
		// two categories: (1) the bottom reveal surface, called here the "inside sill;" and
		// the other reveal surfaces (left, right and top). The left, right and top inside reveal
		// surfaces are assumed to have the same depth and solar absorptance.
		// The depth of the outside reveal is measured from the outside surface of the glazing;
		// The depth of the inside sill and the other reveal surfaces is measured from the inside
		// surface of the glazing. The inside sill is
		// allowed to have depth and solar absorptance values that are different from the corresponding
		// values for the other inside reveal surfaces. The inside sill depth is required to be
		// greater than or equal to the depth of the other inside reveal surfaces. If the inside sill
		// depth is greater than zero the depth of the other inside reveal surfaces is required to
		// to be greater than zero.
		// The reflection of beam solar radiation from all reveal surfaces is assumed to be isotropic
		// diffuse; there is no specular component. Half of the beam solar reflected from outside
		// reveal surfaces is assumed to go towards the window; the other half is assumed to go back
		// to the exterior environment (i.e., reflection of this outward-going component from
		// other outside reveal surfaces is not considered). The half that goes towards the window
		// is added to the other radiation incident on the window.
		// Correspondingly, half of the beam solar reflected from inside reveal surfaces is assumed
		// to go towards the window, with the other half going into the zone (this half, and the portion
		// going towards the window that is reflected) is added in CalcInteriorSolarDistribution
		// to the variable BTOTzone, which is the total beam solar entering the zone as beam or diffuse.
		// The portion going towards the window that is not reflected is absorbed in the glazing or
		// transmitted back out into the exterior environment.
		// The beam solar that is absorbed by outside reveal surfaces is added to the solar absorbed
		// by the outside surface of the window's parent wall; similarly, the beam solar absorbed
		// by the inside reveal surfaces is added to the solar absorbed by the inside surface of the
		// parent wall (and is subtracted from BTOTzone).
		// The net effect of beam solar reflected from outside reveal surfaces is to INCREASE the
		// the heat gain to the zone, whereas the effect of beam solar reflected from interior reveal
		// surfaces is to DECREASE the heat gain to the zone since part of this reflected solar is
		// transmitted back out the window.
		// If the window has a frame, the absorption of reflected beam solar by the inside and outside
		// surfaces of the frame is considered. The shadowing of the frame onto interior reveal
		// surfaces is also considered.

		// The total glazing thickness is taken to be the sum of the thickness of the glass layers
		// and between-glass gas layers. If the window has an exterior, movable, storm window glass layer
		// the presence of this layer and its adjacent air gap is considered in calculating the glazing
		// properties (solar transmittance, etc.). But the storm window glass is assumed to be close
		// enough to the rest of the glazing that its effect on total glazing thickness and outside
		// reveal depth can be ignored.

		// METHODOLOGY EMPLOYED
		// na

		// REFERENCES
		// na

		// USE STATEMENTS
		// Using/Aliasing
		using General::POLYF;
		using General::InterpSw;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS

		int ConstrNum; // Construction number
		int ConstrNumSh; // Shaded construction number
		Real64 CosBetaBottom; // Cosine of beam solar angle of incidence on bottom reveal
		Real64 CosBetaLeft; // Cosine of beam solar angle of incidence on left reveal
		Real64 CosBeta; // ABS of CosBetaBottom or CosBetaLeft
		Real64 d1; // Depth of outside reveal + half of glazing thickness (m)
		Real64 d2; // Depth of inside sill or of inside reveal plus half of glazing thickness (m)
		Real64 d2prime; // Depth of shadow cast on a reveal surface by opposite reveal (m)
		Real64 d2prime2; // Depth of shadow cast by frame onto inside reveal (m)
		Real64 d12; // d12 = d1 + d2 - d2prime (m)
		Real64 TanAlpha; // Tangent of horizontal or vertical profile angle
		Real64 TanGamma; // Tangent of vertical or horizontal profile angle
		Real64 H; // Window height, width (m)
		Real64 W;
		Real64 L; // Window height or width (m)
		Real64 A1sh; // Shadowed area of outside horizontal or vertical reveal (m2)
		Real64 A2sh; // Shadowed area of inside horizontal or vertical reveal (m2)
		Real64 A1ill; // Illuminated area of outside horizontal or vertical reveal (m2)
		Real64 A2ill; // Illuminated area of inside horizontal or vertical reveal (m2)
		Real64 SolTransGlass; // Beam solar transmittance of glazing
		Real64 SolTransGlassSh; // For switchable glazing, beam solar trans in switched state
		Real64 DiffReflGlass; // Diffuse back reflectance of glazing
		Real64 DiffReflGlassSh; // For switchable glazing, diffuse back refl in switched state
		int HorVertReveal; // Index: 1 = horizontal reveal, 2 = vertical reveal
		Real64 OutsReveal; // Depth of outside reveal (from outside glazing plane to outside wall plane) (m)
		Real64 InsReveal; // Depth of inside reveal (from inside glazing plane to inside wall plane (m)
		Real64 InsSillDepth; // Depth of inside sill, measured from innermost face of glazing (m)
		Real64 GlazingThickness; // Thickness of glazing, measured from innermost face to outermost face (m)
		Real64 InsideRevealSolAbs; // Solar absorptance of inside reveal or inside sill
		Real64 BmSolRefldOutsReveal; // Multiplied by beam solar gives beam solar reflected by horiz or vertical
		//  outside reveal surface (m2)
		Real64 BmSolRefldInsReveal; // Multiplied by beam solar gives beam solar reflected by horiz or vertical
		//  inside reveal surface (m2)
		int SurfNum; // Surface number
		int ShadeFlag; // Shading flag
		int FrameDivNum; // Frame/Divider number
		Real64 FrameWidth; // Frame width (m)
		Real64 P1; // Frame outside/inside projection plus half of glazing thickness (m)
		Real64 P2;
		Real64 f1; // f1=d1-P1, f2=d2-P2 (m)
		Real64 f2;
		Real64 L1; // Average distance of outside/inside illuminated area to frame;
		Real64 L2;
		// used in calculating view factor to frame (m)
		Real64 FracToGlassOuts; // View factor from outside horizontal or vertical reveal to glass
		Real64 FracToGlassIns; // View factor from inside horizontal or vertical reveal to glass
		Real64 TanProfileAngVert; // Tangent of vertical profile angle (the profile angle appropriate for
		// vertical reveal surfaces.
		Real64 TanProfileAngHor; // Tangent of horizontal profile angle (the profile angle appropriate for
		// horizontal reveal surfaces.

		Real64 tmp_SunlitFracWithoutReveal; // Temporary variable

		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {

			// Added TH for initialization. CR 7596 inside reveal causing high cooling loads
			// for outside reveals
			SurfaceWindow( SurfNum ).BmSolAbsdOutsReveal = 0.0;
			SurfaceWindow( SurfNum ).BmSolRefldOutsRevealReport = 0.0;
			SurfaceWindow( SurfNum ).BmSolRefldOutsRevealRepEnergy = 0.0;
			SurfaceWindow( SurfNum ).OutsRevealDiffOntoGlazing = 0.0;
			SurfaceWindow( SurfNum ).OutsRevealDiffOntoFrame = 0.0;
			// for inside reveals
			SurfaceWindow( SurfNum ).BmSolAbsdInsReveal = 0.0;
			SurfaceWindow( SurfNum ).BmSolAbsdInsRevealReport = 0.0;
			SurfaceWindow( SurfNum ).BmSolRefldInsReveal = 0.0;
			SurfaceWindow( SurfNum ).BmSolRefldInsRevealReport = 0.0;
			SurfaceWindow( SurfNum ).BmSolRefldInsRevealRepEnergy = 0.0;
			SurfaceWindow( SurfNum ).InsRevealDiffOntoGlazing = 0.0;
			SurfaceWindow( SurfNum ).InsRevealDiffOntoGlazingReport = 0.0;
			SurfaceWindow( SurfNum ).InsRevealDiffOntoFrame = 0.0;
			SurfaceWindow( SurfNum ).InsRevealDiffOntoFrameReport = 0.0;
			SurfaceWindow( SurfNum ).InsRevealDiffIntoZone = 0.0;
			SurfaceWindow( SurfNum ).InsRevealDiffIntoZoneReport = 0.0;

			if ( Surface( SurfNum ).Class != SurfaceClass_Window || ( Surface( SurfNum ).ExtBoundCond != ExternalEnvironment && Surface( SurfNum ).ExtBoundCond != OtherSideCondModeledExt ) ) continue;
			if ( Surface( SurfNum ).Reveal == 0.0 && SurfaceWindow( SurfNum ).InsideReveal == 0.0 && SurfaceWindow( SurfNum ).InsideSillDepth == 0.0 ) continue;
			if ( Surface( SurfNum ).Sides != 4 ) continue;
			if ( SurfaceWindow( SurfNum ).InsideSillDepth < SurfaceWindow( SurfNum ).InsideReveal ) continue;

			ShadeFlag = SurfaceWindow( SurfNum ).ShadingFlag;
			if ( ShadeFlag == ExtShadeOn || ShadeFlag == ExtBlindOn ) continue;

			if ( CosIncAng( TimeStep, HourOfDay, SurfNum ) <= 0.0 ) continue;

			tmp_SunlitFracWithoutReveal = SunlitFracWithoutReveal( TimeStep, HourOfDay, SurfNum );

			// Calculate cosine of angle of incidence of beam solar on reveal surfaces,
			// assumed to be perpendicular to window plane

			CosBetaBottom = -SOLCOS( 1 ) * Surface( SurfNum ).SinAzim * Surface( SurfNum ).CosTilt - SOLCOS( 2 ) * Surface( SurfNum ).CosAzim * Surface( SurfNum ).CosTilt + SOLCOS( 3 ) * Surface( SurfNum ).SinTilt;

			CosBetaLeft = -SOLCOS( 1 ) * Surface( SurfNum ).CosAzim - SOLCOS( 2 ) * Surface( SurfNum ).SinAzim;

			//Note: CosBetaTop = -CosBetaBottom, CosBetaRight = -CosBetaLeft

			OutsReveal = Surface( SurfNum ).Reveal;
			InsReveal = SurfaceWindow( SurfNum ).InsideReveal;
			InsideRevealSolAbs = 0.0;
			GlazingThickness = SurfaceWindow( SurfNum ).TotGlazingThickness;
			H = Surface( SurfNum ).Height;
			W = Surface( SurfNum ).Width;
			d1 = OutsReveal + 0.5 * GlazingThickness;
			ConstrNum = Surface( SurfNum ).Construction;
			ConstrNumSh = Surface( SurfNum ).ShadedConstruction;
			if ( SurfaceWindow( SurfNum ).StormWinFlag == 1 ) {
				ConstrNum = Surface( SurfNum ).StormWinConstruction;
				ConstrNumSh = Surface( SurfNum ).StormWinShadedConstruction;
			}
			SolTransGlass = POLYF( CosIncAng( TimeStep, HourOfDay, SurfNum ), Construct( ConstrNum ).TransSolBeamCoef );
			TanProfileAngVert = SurfaceWindow( SurfNum ).TanProfileAngVert;
			TanProfileAngHor = SurfaceWindow( SurfNum ).TanProfileAngHor;
			FrameDivNum = Surface( SurfNum ).FrameDivider;
			FrameWidth = 0.0;
			if ( FrameDivNum != 0 ) {
				FrameWidth = FrameDivider( FrameDivNum ).FrameWidth;
				if ( FrameWidth > 0.0 ) {
					P1 = FrameDivider( FrameDivNum ).FrameProjectionOut + 0.5 * GlazingThickness;
					P2 = FrameDivider( FrameDivNum ).FrameProjectionIn + 0.5 * GlazingThickness;
					if ( OutsReveal + 0.5 * GlazingThickness <= P1 ) d1 = P1 + 0.001;
				}
			}
			// Loop over vertical and horizontal reveal surfaces
			for ( HorVertReveal = 1; HorVertReveal <= 2; ++HorVertReveal ) {

				FracToGlassOuts = 0.5;
				FracToGlassIns = 0.5;
				BmSolRefldOutsReveal = 0.0;
				BmSolRefldInsReveal = 0.0;
				A1ill = 0.0;
				A2ill = 0.0;

				// Added TH. 5/27/2009
				A1sh = 0.0;
				A2sh = 0.0;

				if ( HorVertReveal == 1 ) { // Vertical reveal
					TanAlpha = TanProfileAngHor;
					TanGamma = TanProfileAngVert;
					CosBeta = std::abs( CosBetaLeft );
					L = Surface( SurfNum ).Height;
					d2 = InsReveal + 0.5 * GlazingThickness;
					d2prime = d1 + d2 - W / TanGamma;
					InsideRevealSolAbs = SurfaceWindow( SurfNum ).InsideRevealSolAbs;
				} else { // Horizontal reveal
					InsSillDepth = SurfaceWindow( SurfNum ).InsideSillDepth;
					TanAlpha = TanProfileAngVert;
					TanGamma = TanProfileAngHor;
					CosBeta = std::abs( CosBetaBottom );
					L = Surface( SurfNum ).Width;
					if ( CosBetaBottom > 0.0 ) { // Bottom reveal surfaces may be illuminated
						d2 = InsSillDepth + 0.5 * GlazingThickness;
						InsideRevealSolAbs = SurfaceWindow( SurfNum ).InsideSillSolAbs;
					} else { // Top reveal surfaces may be illuminated
						d2 = InsReveal + 0.5 * GlazingThickness;
						InsideRevealSolAbs = SurfaceWindow( SurfNum ).InsideRevealSolAbs;
					}
					d2prime = d1 + d2 - H / TanGamma;
				}
				if ( d2prime < 0.0 ) d2prime = 0.0; // No shadow from opposing reveal
				d12 = d1 + d2 - d2prime;

				if ( FrameWidth <= 0.001 ) {
					// Window without frame

					// Find inside and outside shadowed area of vertical or horizontal reveal surfaces
					// that can be illuminated by beam solar; shadowing is by other reveal surfaces.

					if ( d2prime <= d2 ) {
						if ( d12 * TanAlpha <= L ) {
							A1sh = 0.5 * TanAlpha * pow_2( d1 );
							A2sh = d2prime * L + 0.5 * TanAlpha * pow_2( d12 ) - A1sh;
						} else { // d12*TanAlpha > L
							if ( d1 * TanAlpha <= L ) {
								A1sh = 0.5 * TanAlpha * pow_2( d1 );
								A2sh = d2 * L - 0.5 * TanAlpha * pow_2( L / TanAlpha - d1 );
							} else { // d1*TanAlpha > L
								A1sh = d1 * L - ( 0.5 / TanAlpha ) * pow_2( L );
								A2sh = d2 * L;
							}
						}
					} else { // d2prime > d2
						A2sh = d2 * L;
						if ( d2prime < d1 + d2 ) {
							if ( d12 * TanAlpha <= L ) {
								A1sh = L * ( d2prime - d2 ) + 0.5 * TanAlpha * pow_2( d12 );
							} else { // d12*TanAlpha > L
								A1sh = d1 * L - 0.5 * pow_2( L ) / TanAlpha;
							}
						} else { // d2prime >= d1+d2
							A1sh = d1 * L;
						}
					}

					// Added TH. 5/27/2009
					if ( A1sh < 0.0 ) A1sh = 0.0;
					if ( A2sh < 0.0 ) A2sh = 0.0;

					if ( OutsReveal >= 0.001 ) A1ill = d1 * L - A1sh; // A1ill = 0.0 if OutsReveal < 0.001
					if ( InsReveal >= 0.001 ) A2ill = d2 * L - A2sh; // A2ill = 0.0 if InsReveal < 0.001

				} else { // Window with frame; take into account shadowing
					// of inside reveal surfaces by frame
					f1 = d1 - P1;
					f2 = d2 - P2;
					d2prime2 = FrameWidth / TanGamma;
					if ( HorVertReveal == 1 ) { // Vertical reveal
						if ( InsReveal + 0.5 * GlazingThickness <= P2 ) d2 = P2 + 0.001;
					} else { // Horizontal
						if ( CosBetaBottom > 0.0 ) { // Bottom reveal surfaces may be illuminated
							if ( InsSillDepth + 0.5 * GlazingThickness <= P2 ) d2 = P2 + 0.001;
						} else { // Top reveal surfaces may be illuminated
							if ( InsReveal + 0.5 * GlazingThickness <= P2 ) d2 = P2 + 0.001;
						}
					}

					if ( d2prime <= f2 ) { // Shadow from opposing reveal does not go beyond inside surface of frame

						if ( d12 * TanAlpha <= L ) {
							A1sh = 0.5 * TanAlpha * pow_2( f1 );
							L1 = f1 * ( f1 * TanAlpha / ( 6.0 * L ) + 0.5 );
							if ( d2 - ( d2prime + d2prime2 + P2 ) >= 0.0 ) {
								A2sh = ( d2prime + d2prime2 ) * L + 0.5 * TanAlpha * ( pow_2( d1 + d2 - d2prime ) - pow_2( d1 + P2 + d2prime2 ) );
								L2 = d2prime2 + 0.5 * ( d2 - ( d2prime + d2prime2 + P2 ) );
							} else { // d2-(d2prime+d2prime2+P2) < 0.  ! Inside reveal is fully shadowed by frame and/or opposing reveal
								A2sh = f2 * L;
								L2 = f2;
							}
						} else { // d12*TanAlpha >= L
							if ( ( d1 + P2 ) * TanAlpha <= L ) {
								A1sh = 0.5 * TanAlpha * pow_2( f1 );
								L1 = f1 * ( ( f1 * TanAlpha ) / ( 6.0 * L ) + 0.5 );
								if ( ( d1 + P2 + d2prime2 ) * TanAlpha >= L ) {
									A2sh = f2 * L;
									L2 = f2;
								} else { // (d1+P2+d2prime2)*TanAlpha < L
									A2sh = f2 * L - 0.5 * pow_2( L - ( d1 + P2 ) * TanAlpha ) / TanAlpha + d2prime2 * ( L - ( d1 + P2 + d2prime2 / 2.0 ) * TanAlpha );
									L2 = d2prime2 + ( L / TanAlpha - ( d1 + P2 + d2prime2 ) ) / 3.0;
								}
							} else { // (d1+P2)*TanAlpha > L
								L2 = f2;
								A2sh = f2 * L;
								if ( f1 * TanAlpha <= L ) {
									A1sh = 0.5 * TanAlpha * pow_2( f1 );
									L1 = f1 * ( ( f1 * TanAlpha ) / ( 6.0 * L ) + 0.5 );
								} else { // f1*TanAlpha > L
									A1sh = f1 * L - 0.5 * pow_2( L ) / TanAlpha;
									L1 = f1 - ( L / TanAlpha ) / 3.0;
								}
							}
						}

					} else { // d2prime > f2   ! Shadow from opposing reveal goes beyond inside of frame

						A2sh = f2 * L;
						L2 = f2;
						if ( d2prime >= d1 + d2 ) {
							A1sh = 0.0;
							L1 = f1;
						} else { // d2prime < d1+d2
							if ( d2prime <= d2 + P1 ) {
								if ( f1 * TanAlpha <= L ) {
									A1sh = 0.5 * TanAlpha * pow_2( f1 );
									L1 = f1 * ( ( f1 * TanAlpha ) / ( 6.0 * L ) + 0.5 );
								} else { // f1*TanAlpha > L
									A1sh = f1 * L - 0.5 * pow_2( L ) / TanAlpha;
									L1 = f1 - ( L / TanAlpha ) / 3.0;
								}
							} else { // d2prime > d2+P1
								if ( d12 * TanAlpha <= L ) {
									A1sh = L * ( d2prime - ( d2 + P1 ) ) + 0.5 * TanAlpha * pow_2( d12 );
									L1 = ( L * ( f1 - d12 / 2.0 ) - d12 * TanAlpha * ( f1 / 2 - d12 / 3.0 ) ) / ( L - d12 * TanAlpha / 2.0 );
								} else { // d12*TanAlpha > L
									A1sh = f1 * L - 0.5 * pow_2( L ) / TanAlpha;
									L1 = f1 - ( L / TanAlpha ) / 3.0;
								}
							}
						}

					}

					// Added TH. 5/27/2009
					if ( A1sh < 0.0 ) A1sh = 0.0;
					if ( A2sh < 0.0 ) A2sh = 0.0;

					if ( OutsReveal >= P1 + 0.5 * GlazingThickness + 0.001 ) A1ill = L * f1 - A1sh;
					if ( InsReveal >= P2 + 0.5 * GlazingThickness + 0.001 ) A2ill = L * f2 - A2sh;
					if ( L1 == 0.0 ) {
						FracToGlassOuts = 0.0;
					} else {
						FracToGlassOuts = 0.5 * ( 1.0 - std::atan( FrameWidth / L1 ) / PiOvr2 );
					}
					if ( L2 == 0.0 ) {
						FracToGlassIns = 0.0;
					} else {
						FracToGlassIns = 0.5 * ( 1.0 - std::atan( FrameWidth / L2 ) / PiOvr2 );
					}
				} // End of check if window has frame

				// Added TH. 5/27/2009
				if ( A1ill < 0.0 ) A1ill = 0.0;
				if ( A2ill < 0.0 ) A2ill = 0.0;

				// Quantities related to outside reveal
				if ( A1ill > 1.0e-6 ) {

					SurfaceWindow( SurfNum ).BmSolAbsdOutsReveal += A1ill * SurfaceWindow( SurfNum ).OutsideRevealSolAbs * CosBeta * tmp_SunlitFracWithoutReveal;

					BmSolRefldOutsReveal = A1ill * ( 1.0 - SurfaceWindow( SurfNum ).OutsideRevealSolAbs ) * CosBeta * tmp_SunlitFracWithoutReveal;

					SurfaceWindow( SurfNum ).BmSolRefldOutsRevealReport += BeamSolarRad * BmSolRefldOutsReveal;
					SurfaceWindow( SurfNum ).BmSolRefldOutsRevealRepEnergy = SurfaceWindow( SurfNum ).BmSolRefldOutsRevealReport * TimeStepZoneSec;

					// Reflected solar from outside horizontal and vertical reveal incident on glazing
					SurfaceWindow( SurfNum ).OutsRevealDiffOntoGlazing += FracToGlassOuts * BmSolRefldOutsReveal / Surface( SurfNum ).Area;

					if ( FrameWidth > 0.0 ) {
						// Reflected solar from outside horizontal and vertical reveal incident on frame
						SurfaceWindow( SurfNum ).OutsRevealDiffOntoFrame += ( 0.5 - FracToGlassOuts ) * BmSolRefldOutsReveal / SurfaceWindow( SurfNum ).FrameArea;
					}

				} // End of check if A1ill > 0.0 (actually 10^-6)

				// Quantities related to inside reveal; inside reveal reflection/absorption is assumed
				// to occur only if an interior shade or blind is not in place.

				if ( ShadeFlag <= 0 || ShadeFlag == SwitchableGlazing ) {

					if ( A2ill > 1.0e-6 ) {

						DiffReflGlass = Construct( ConstrNum ).ReflectSolDiffBack;
						if ( ShadeFlag == SwitchableGlazing ) {
							SolTransGlassSh = POLYF( CosIncAng( TimeStep, HourOfDay, SurfNum ), Construct( ConstrNumSh ).TransSolBeamCoef );
							SolTransGlass = InterpSw( SurfaceWindow( SurfNum ).SwitchingFactor, SolTransGlass, SolTransGlassSh );
							DiffReflGlassSh = Construct( ConstrNumSh ).ReflectSolDiffBack;
							DiffReflGlass = InterpSw( SurfaceWindow( SurfNum ).SwitchingFactor, DiffReflGlass, DiffReflGlassSh );
						}

						// Calc beam solar sbsorbed (m2)
						SurfaceWindow( SurfNum ).BmSolAbsdInsReveal += A2ill * SolTransGlass * InsideRevealSolAbs * CosBeta * tmp_SunlitFracWithoutReveal;

						// Added TH 5/26/2009 for reporting purpose - Beam solar absorbed by the inside reveal (W)
						SurfaceWindow( SurfNum ).BmSolAbsdInsRevealReport += BeamSolarRad * A2ill * SolTransGlass * InsideRevealSolAbs * CosBeta * tmp_SunlitFracWithoutReveal;

						// in m2 = Area * solar transmitted fraction * inside reveal reflection fraction
						BmSolRefldInsReveal = A2ill * SolTransGlass * ( 1.0 - InsideRevealSolAbs ) * CosBeta * tmp_SunlitFracWithoutReveal;

						SurfaceWindow( SurfNum ).BmSolRefldInsReveal += BmSolRefldInsReveal;

						SurfaceWindow( SurfNum ).BmSolRefldInsRevealReport += BeamSolarRad * BmSolRefldInsReveal; // W, BeamSolarRad in W/m2
						SurfaceWindow( SurfNum ).BmSolRefldInsRevealRepEnergy = SurfaceWindow( SurfNum ).BmSolRefldInsRevealReport * TimeStepZoneSec;

						// Reflected solar from inside horizontal and vertical reveal incident on glazing
						SurfaceWindow( SurfNum ).InsRevealDiffOntoGlazing += FracToGlassIns * BmSolRefldInsReveal / Surface( SurfNum ).Area;

						// Added TH 5/26/2009 for reporting purpose - diffuse on window glass from inside reveal (W)
						SurfaceWindow( SurfNum ).InsRevealDiffOntoGlazingReport += BeamSolarRad * FracToGlassIns * BmSolRefldInsReveal;

						// Reflected solar from inside horizontal and vertical reveal incident on frame
						if ( FrameWidth > 0.0 ) {
							SurfaceWindow( SurfNum ).InsRevealDiffOntoFrame += ( 0.5 - FracToGlassIns ) * BmSolRefldInsReveal / SurfaceWindow( SurfNum ).FrameArea;

							// Added TH 5/26/2009 for reporting purpose - diffuse on window frame from inside reveal (W)
							SurfaceWindow( SurfNum ).InsRevealDiffOntoFrameReport += BeamSolarRad * ( 0.5 - FracToGlassIns ) * BmSolRefldInsReveal;
						}

						// Reflected solar from inside reveal going directly into zone and reflected from glass.
						// Assumes half of solar reflected from inside reveal goes as diffuse radiation into the zone and
						// half goes as diffuse radiation towards window.
						SurfaceWindow( SurfNum ).InsRevealDiffIntoZone += BmSolRefldInsReveal * ( 0.5 + DiffReflGlass * FracToGlassIns );

						// Added TH 5/26/2009 for reporting purpose - diffuse into zone from inside reveal (W)
						SurfaceWindow( SurfNum ).InsRevealDiffIntoZoneReport += BeamSolarRad * BmSolRefldInsReveal * ( 0.5 + DiffReflGlass * FracToGlassIns );

					} // End of check if A2ill > 0.0 (actually 10^-6)

				} // End of check if interior shade or blind is in place

			} // End of loop over vertical and horizontal reveal

		} // End of surface loop

	}

	void
	ReportSurfaceShading()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   April 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine uses the internal variables used in the Shading
		// calculations and prepares them for reporting (at timestep level).

		// METHODOLOGY EMPLOYED:
		// Because all of the calculations are done on a "daily" basis in this
		// module, it is difficult to formulate the values that might be useful
		// for reporting.  SunlitFrac was the first of these two arrays to be
		// made into "two dimensions".  It is not clear that both have to be
		// two dimensions.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace OutputReportPredefined;

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
		int SurfNum; // Loop Counter
		int RepCol; // the column of the predefined report

		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			SurfSunlitFrac( SurfNum ) = SunlitFrac( TimeStep, HourOfDay, SurfNum );
			SurfSunlitArea( SurfNum ) = SunlitFrac( TimeStep, HourOfDay, SurfNum ) * Surface( SurfNum ).Area;
		}
		//added for predefined reporting
		RepCol = 0;
		if ( Month == 3 && DayOfMonth == 21 ) {
			if ( ( HourOfDay == 9 ) && ( TimeStep == 4 ) ) {
				RepCol = pdchSlfMar21_9;
			} else if ( ( HourOfDay == 12 ) && ( TimeStep == 4 ) ) {
				RepCol = pdchSlfMar21_12;
			} else if ( ( HourOfDay == 15 ) && ( TimeStep == 4 ) ) {
				RepCol = pdchSlfMar21_15;
			}
		} else if ( Month == 6 && DayOfMonth == 21 ) {
			if ( ( HourOfDay == 9 ) && ( TimeStep == 4 ) ) {
				RepCol = pdchSlfJun21_9;
			} else if ( ( HourOfDay == 12 ) && ( TimeStep == 4 ) ) {
				RepCol = pdchSlfJun21_12;
			} else if ( ( HourOfDay == 15 ) && ( TimeStep == 4 ) ) {
				RepCol = pdchSlfJun21_15;
			}
		} else if ( Month == 12 && DayOfMonth == 21 ) {
			if ( ( HourOfDay == 9 ) && ( TimeStep == 4 ) ) {
				RepCol = pdchSlfDec21_9;
			} else if ( ( HourOfDay == 12 ) && ( TimeStep == 4 ) ) {
				RepCol = pdchSlfDec21_12;
			} else if ( ( HourOfDay == 15 ) && ( TimeStep == 4 ) ) {
				RepCol = pdchSlfDec21_15;
			}
		}
		if ( RepCol != 0 ) {
			for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
				if ( Surface( SurfNum ).Class == SurfaceClass_Window ) {
					PreDefTableEntry( RepCol, Surface( SurfNum ).Name, SurfSunlitFrac( SurfNum ) );
				}
			}
		}

	}

	void
	ReportSurfaceErrors()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   November 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine reports some recurring type errors that can get mixed up with more important
		// errors in the error file.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataErrorTracking; // for error tracking
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static Array1D_string const MSG( 4, { "misses", "", "within", "overlaps" } );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Loop1;
		int Loop2;
		int Count;
		int TotCount;
		std::string CountOut;
		Array1D_bool SurfErrorReported;
		Array1D_bool SurfErrorReported2;

		if ( NumTooManyFigures + NumTooManyVertices + NumBaseSubSurround > 0 ) {
			ShowMessage( "" );
			ShowMessage( "===== Recurring Surface Error Summary =====" );
			ShowMessage( "The following surface error messages occurred." );
			ShowMessage( "" );

			if ( NumBaseSubSurround > 0 ) {
				ShowMessage( "Base Surface does not surround subsurface errors occuring..." );
				ShowMessage( "Check that the GlobalGeometryRules object is expressing the proper starting corner and direction [CounterClockwise/Clockwise]" );
				ShowMessage( "" );
			}

			SurfErrorReported.dimension( TotSurfaces, false );
			TotCount = 0;
			for ( Loop1 = 1; Loop1 <= NumBaseSubSurround; ++Loop1 ) {
				Count = 0;
				if ( SurfErrorReported( TrackBaseSubSurround( Loop1 ).SurfIndex1 ) ) continue;
				for ( Loop2 = 1; Loop2 <= NumBaseSubSurround; ++Loop2 ) {
					if ( TrackBaseSubSurround( Loop1 ).SurfIndex1 == TrackBaseSubSurround( Loop2 ).SurfIndex1 && TrackBaseSubSurround( Loop1 ).MiscIndex == TrackBaseSubSurround( Loop2 ).MiscIndex ) {
						++Count;
					}
				}
				gio::write( CountOut, fmtLD ) << Count;
				TotCount += Count;
				TotalWarningErrors += Count - 1;
				ShowWarningError( "Base surface does not surround subsurface (CHKSBS), Overlap Status=" + cOverLapStatus( TrackBaseSubSurround( Loop1 ).MiscIndex ) );
				ShowContinueError( "  The base surround errors occurred " + stripped( CountOut ) + " times." );
				for ( Loop2 = 1; Loop2 <= NumBaseSubSurround; ++Loop2 ) {
					if ( TrackBaseSubSurround( Loop1 ).SurfIndex1 == TrackBaseSubSurround( Loop2 ).SurfIndex1 && TrackBaseSubSurround( Loop1 ).MiscIndex == TrackBaseSubSurround( Loop2 ).MiscIndex ) {
						ShowContinueError( "Surface \"" + Surface( TrackBaseSubSurround( Loop1 ).SurfIndex1 ).Name + "\" " + MSG( TrackBaseSubSurround( Loop1 ).MiscIndex ) + " SubSurface \"" + Surface( TrackBaseSubSurround( Loop2 ).SurfIndex2 ).Name + "\"" );
					}
				}
				SurfErrorReported( TrackBaseSubSurround( Loop1 ).SurfIndex1 ) = true;
			}
			if ( TotCount > 0 ) {
				ShowMessage( "" );
				gio::write( CountOut, fmtLD ) << TotCount;
				ShowContinueError( "  The base surround errors occurred " + stripped( CountOut ) + " times (total)." );
				ShowMessage( "" );
			}

			SurfErrorReported2.allocate( TotSurfaces );
			SurfErrorReported = false;
			TotCount = 0;
			if ( NumTooManyVertices > 0 ) {
				ShowMessage( "Too many vertices [>=" + RoundSigDigits( MaxHCV ) + "] in shadow overlap errors occurring..." );
				ShowMessage( "These occur throughout the year and may occur several times for the same surfaces. You may be able to reduce them by adding Output:Diagnostics,DoNotMirrorDetachedShading;" );
			}
			for ( Loop1 = 1; Loop1 <= NumTooManyVertices; ++Loop1 ) {
				Count = 0;
				SurfErrorReported2 = false;
				if ( SurfErrorReported( TrackTooManyVertices( Loop1 ).SurfIndex1 ) ) continue;
				for ( Loop2 = 1; Loop2 <= NumTooManyVertices; ++Loop2 ) {
					if ( TrackTooManyVertices( Loop1 ).SurfIndex1 == TrackTooManyVertices( Loop2 ).SurfIndex1 ) {
						++Count;
					}
				}
				gio::write( CountOut, fmtLD ) << Count;
				TotCount += Count;
				TotalWarningErrors += Count - 1;
				ShowMessage( "" );
				ShowWarningError( "Too many vertices [>=" + RoundSigDigits( MaxHCV ) + "] in a shadow overlap" );
				ShowContinueError( "Overlapping figure=" + Surface( TrackTooManyVertices( Loop1 ).SurfIndex1 ).Name + ", Surface Class=[" + cSurfaceClass( Surface( TrackTooManyVertices( Loop1 ).SurfIndex1 ).Class ) + ']' );
				ShowContinueError( "  This error occurred " + stripped( CountOut ) + " times." );
				for ( Loop2 = 1; Loop2 <= NumTooManyVertices; ++Loop2 ) {
					if ( TrackTooManyVertices( Loop1 ).SurfIndex1 == TrackTooManyVertices( Loop2 ).SurfIndex1 ) {
						if ( SurfErrorReported2( TrackTooManyVertices( Loop2 ).SurfIndex2 ) ) continue;
						ShowContinueError( "Figure being Overlapped=" + Surface( TrackTooManyVertices( Loop2 ).SurfIndex2 ).Name + ", Surface Class=[" + cSurfaceClass( Surface( TrackTooManyVertices( Loop2 ).SurfIndex2 ).Class ) + ']' );
						SurfErrorReported2( TrackTooManyVertices( Loop2 ).SurfIndex2 ) = true;
					}
				}
				SurfErrorReported( TrackTooManyVertices( Loop1 ).SurfIndex1 ) = true;
			}
			if ( TotCount > 0 ) {
				ShowMessage( "" );
				gio::write( CountOut, fmtLD ) << TotCount;
				ShowContinueError( "  The too many vertices errors occurred " + stripped( CountOut ) + " times (total)." );
				ShowMessage( "" );
			}

			SurfErrorReported = false;
			TotCount = 0;
			if ( NumTooManyFigures > 0 ) {
				ShowMessage( "Too many figures [>=" + RoundSigDigits( MaxHCS ) + "] in shadow overlap errors occurring..." );
				ShowMessage( "These occur throughout the year and may occur several times for the same surfaces. You may be able to reduce them by adding OutputDiagnostics,DoNotMirrorDetachedShading;" );
			}
			for ( Loop1 = 1; Loop1 <= NumTooManyFigures; ++Loop1 ) {
				Count = 0;
				SurfErrorReported2 = false;
				if ( SurfErrorReported( TrackTooManyFigures( Loop1 ).SurfIndex1 ) ) continue;
				for ( Loop2 = 1; Loop2 <= NumTooManyFigures; ++Loop2 ) {
					if ( TrackTooManyFigures( Loop1 ).SurfIndex1 == TrackTooManyFigures( Loop2 ).SurfIndex1 ) {
						++Count;
					}
				}
				gio::write( CountOut, fmtLD ) << Count;
				TotCount += Count;
				TotalWarningErrors += Count - 1;
				ShowMessage( "" );
				ShowWarningError( "Too many figures [>=" + RoundSigDigits( MaxHCS ) + "] in a shadow overlap" );
				ShowContinueError( "Overlapping figure=" + Surface( TrackTooManyFigures( Loop1 ).SurfIndex1 ).Name + ", Surface Class=[" + cSurfaceClass( Surface( TrackTooManyFigures( Loop1 ).SurfIndex1 ).Class ) + ']' );
				ShowContinueError( "  This error occurred " + stripped( CountOut ) + " times." );
				for ( Loop2 = 1; Loop2 <= NumTooManyFigures; ++Loop2 ) {
					if ( TrackTooManyFigures( Loop1 ).SurfIndex1 == TrackTooManyFigures( Loop2 ).SurfIndex1 ) {
						if ( SurfErrorReported2( TrackTooManyFigures( Loop2 ).SurfIndex2 ) ) continue;
						ShowContinueError( "Figure being Overlapped=" + Surface( TrackTooManyFigures( Loop2 ).SurfIndex2 ).Name + ", Surface Class=[" + cSurfaceClass( Surface( TrackTooManyFigures( Loop2 ).SurfIndex2 ).Class ) + ']' );
						SurfErrorReported2( TrackTooManyFigures( Loop2 ).SurfIndex2 ) = true;
					}
				}
				SurfErrorReported( TrackTooManyFigures( Loop1 ).SurfIndex1 ) = true;
			}
			if ( TotCount > 0 ) {
				ShowMessage( "" );
				gio::write( CountOut, fmtLD ) << TotCount;
				ShowContinueError( "  The too many figures errors occurred " + stripped( CountOut ) + " times (total)." );
				ShowMessage( "" );
			}
			SurfErrorReported.deallocate();
			SurfErrorReported2.deallocate();
		}

	}

	void
	ComputeWinShadeAbsorpFactors()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   Mar 2001
		//       MODIFIED       Oct 2002,FCW: change ConstrNumSh = WindowShadingControl(WinShadeCtrlNum)%ShadedConstruction
		//                      to Surface(SurfNum)%ShadedConstruction
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Called by InitSolarCalculations. Finds fractions that apportion radiation absorbed by a
		// window shade to the two faces of the shade. For radiation incident from the left,
		// ShadeAbsFacFace(1) is the fraction of radiation absorbed in the left-hand half of the
		// of the shade and ShadeAbsFacFace(2) is the fraction absorbed in the right-hand half.
		// The shade is assumed to be homogeneous.

		// REFERENCES: See EnergyPlus engineering documentation
		// USE STATEMENTS: na

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS: na
		// INTERFACE BLOCK SPECIFICATIONS: na
		// DERIVED TYPE DEFINITIONS: na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int WinShadeCtrlNum; // Window shading control number
		int SurfNum; // Surface number
		int ConstrNumSh; // Window construction number with shade
		int TotLay; // Total layers in a construction
		int MatNumSh; // Shade layer material number
		Real64 AbsorpEff; // Effective absorptance of isolated shade layer (fraction of
		//  of incident radiation remaining after reflected portion is
		//  removed that is absorbed

		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			if ( Surface( SurfNum ).Class == SurfaceClass_Window && Surface( SurfNum ).WindowShadingControlPtr > 0 ) {
				WinShadeCtrlNum = Surface( SurfNum ).WindowShadingControlPtr;
				if ( WindowShadingControl( WinShadeCtrlNum ).ShadingType == WSC_ST_InteriorShade || WindowShadingControl( WinShadeCtrlNum ).ShadingType == WSC_ST_ExteriorShade || WindowShadingControl( WinShadeCtrlNum ).ShadingType == WSC_ST_BetweenGlassShade ) {
					ConstrNumSh = Surface( SurfNum ).ShadedConstruction;
					TotLay = Construct( ConstrNumSh ).TotLayers;
					if ( WindowShadingControl( WinShadeCtrlNum ).ShadingType == WSC_ST_InteriorShade ) {
						MatNumSh = Construct( ConstrNumSh ).LayerPoint( TotLay ); // Interior shade
					} else if ( WindowShadingControl( WinShadeCtrlNum ).ShadingType == WSC_ST_ExteriorShade ) {
						MatNumSh = Construct( ConstrNumSh ).LayerPoint( 1 ); // Exterior shade
					} else if ( WindowShadingControl( WinShadeCtrlNum ).ShadingType == WSC_ST_BetweenGlassShade ) {
						if ( Construct( ConstrNumSh ).TotGlassLayers == 2 ) {
							MatNumSh = Construct( ConstrNumSh ).LayerPoint( 3 ); // Double pane with between-glass shade
						} else {
							MatNumSh = Construct( ConstrNumSh ).LayerPoint( 5 ); // Triple pane with between-glass shade
						}
					}
					AbsorpEff = Material( MatNumSh ).AbsorpSolar / ( Material( MatNumSh ).AbsorpSolar + Material( MatNumSh ).Trans + 0.0001 );
					AbsorpEff = min( max( AbsorpEff, 0.0001 ), 0.999 ); // Constrain to avoid problems with following log eval
					SurfaceWindow( SurfNum ).ShadeAbsFacFace( 1 ) = ( 1.0 - std::exp( 0.5 * std::log( 1.0 - AbsorpEff ) ) ) / AbsorpEff;
					SurfaceWindow( SurfNum ).ShadeAbsFacFace( 2 ) = 1.0 - SurfaceWindow( SurfNum ).ShadeAbsFacFace( 1 );
				}
			}
		}

	}

	void
	CalcWinTransDifSolInitialDistribution()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rob Hitchcock
		//       DATE WRITTEN   July 2007
		//       MODIFIED       N/A
		//       RE-ENGINEERED  N/A

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates the initial distribution
		// of diffuse solar transmitted through exterior windows
		// to individual heat transfer surfaces in each zone.

		// METHODOLOGY EMPLOYED:
		// Apportions diffuse solar transmitted through each exterior window
		// that is then absorbed, reflected, and/or transmitted
		// by other heat transfer surfaces in the zone.
		// Calculations use:
		// 1. WinDifSolar calculated in SUBROUTINE CalcInteriorSolarDistribution,
		// 2. view factors between each exterior window and
		// other heat transfer surfaces in a zone
		// calculated in SUBROUTINE CalcApproximateViewFactors, and
		// 3. surface absorptances, reflectances, and transmittances
		// determined here using revised code from SUBROUTINE InitIntSolarDistribution

		// REFERENCES:

		// Using/Aliasing
		using General::InterpSw;
		using General::InterpSlatAng;
		using ScheduleManager::GetCurrentScheduleValue;
		using namespace DataViewFactorInformation;

		using DataHeatBalSurface::InitialDifSolInAbs;
		using DataHeatBalSurface::InitialDifSolInTrans;
		using DataHeatBalance::InitialDifSolwinAbs;
		using DataHeatBalance::InitialZoneDifSolReflW;

		using WindowEquivalentLayer::CalcEQLOpticalProperty;
		using namespace DataWindowEquivalentLayer;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ZoneNum; // DO loop counter for zones
		int AdjZoneNum; // Index for adjacent zones
		int AdjSurfNum; // Index for adjacent surfaces
		int DifTransSurfNum; // Diffuse Solar Transmitting Surface number
		int HeatTransSurfNum; // Heat Transfer Surface number
		int ConstrNum; // Construction number
		int AdjConstrNum; // Construction number of other side surface
		int ConstrNumSh; // Shaded construction number
		int IGlass; // Glass layer counter
		int TotGlassLayers; // Number of glass layers in a window construction
		int ShadeFlag; // Shading flag
		Real64 AbsInt; // Tmp var for Inside surface short-wave absorptance
		Real64 MovInsulSchedVal; // Value of the movable insulation schedule for current time
		Real64 HMovInsul; // Conductance of movable insulation
		Real64 InsideDifAbsorptance; // Inside diffuse solar absorptance of a surface
		Real64 InsideDifReflectance; // Inside diffuse solar reflectance of a surface
		int BlNum; // Blind number
		Real64 BlAbsDiffBk; // Glass layer back diffuse solar absorptance when blind in place
		Real64 AbsDiffBkBl; // Blind diffuse back solar absorptance as part of glazing system

		//  REAL(r64)    :: DividerSolAbs      ! Window divider solar absorptance
		//  REAL(r64)    :: DividerSolRefl     ! Window divider solar reflectance
		//  INTEGER :: MatNumGl           ! Glass layer material number
		//  INTEGER :: MatNumSh           ! Shade layer material number
		//  REAL(r64)    :: TransGl,ReflGl,AbsGl ! Glass layer solar transmittance, reflectance, absorptance

		Real64 ViewFactor; // temp var for view factor
		Real64 ViewFactorTotal; // debug var for view factor total
		Real64 WinDifSolarTrans; // debug var for WinDifSolar() [W]
		Real64 WinDifSolarDistTotl; // debug var for window total distributed diffuse solar [W]
		Real64 WinDifSolarDistAbsorbedTotl; // debug var for individual exterior window total distributed
		//    diffuse solar absorbed [W]
		Real64 WinDifSolarDistReflectedTotl; // debug var for individual exterior window total distributed
		//    diffuse solar reflected [W]
		Real64 WinDifSolarDistTransmittedTotl; // debug var for individual exterior window total distributed
		//    diffuse solar transmitted [W]
		Real64 WinDifSolLayAbsW; // temp var for diffuse solar absorbed by individual glass layer [W]
		Real64 ZoneDifSolarTrans; // debug var for WinDifSolar() [W]
		Real64 ZoneDifSolarDistTotl; // debug var for zone total distributed diffuse solar [W]
		Real64 ZoneDifSolarDistAbsorbedTotl; // debug var for zone total distributed diffuse solar absorbed [W]
		Real64 ZoneDifSolarDistReflectedTotl; // debug var for zone total distributed diffuse solar reflected [W]
		Real64 ZoneDifSolarDistTransmittedTotl; // debug var for zone total distributed diffuse solar transmitted [W]

		Real64 DifSolarAbsW; // temp var for diffuse solar absorbed by surface [W]
		Real64 DifSolarAbs; // temp var for diffuse solar absorbed by surface [W/m2]
		Real64 DifSolarReflW; // temp var for diffuse solar reflected by surface [W]
		Real64 DifSolarTransW; // temp var for diffuse solar transmitted through interior window surface [W]
		Real64 ShBlDifSolarAbsW; // temp var for diffuse solar absorbed by shade/blind [W]

		Array2D< Real64 > AbsSolBeamEQL( 2, CFSMAXNL+1 ); // absorbed exterior beam radiation by layers fraction
		Array2D< Real64 > AbsSolDiffEQL( 2, CFSMAXNL+1 ); // absorbed exterior diffuse radiation by layers fraction
		Array2D< Real64 > AbsSolBeamBackEQL( 2, CFSMAXNL+1 ); // absorbed interior beam radiation by layers fraction from back
		Array2D< Real64 > AbsSolDiffBackEQL( 2, CFSMAXNL+1 ); // absorbed exterior diffuse radiation by layers fraction from back
		int EQLNum; // equivalent layer fenestration index
		int Lay; // equivalent layer fenestration layer index

		int FirstZoneSurf; // conversion index for ViewFactor
		int LastZoneSurf;

		// Init accumulators for absorbed diffuse solar for all surfaces for later heat balance calcs
		InitialDifSolInAbs = 0.0;
		InitialDifSolwinAbs = 0.0;

		// Init accumulator for total reflected diffuse solar within each zone for interreflection calcs
		InitialZoneDifSolReflW = 0.0;

		// Init accumulator for transmitted diffuse solar for all surfaces for reporting
		InitialDifSolInTrans = 0.0;

		// Loop over all zones doing initial distribution of diffuse solar to interior heat transfer surfaces
		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {

			// Init Zone accumulators for debugging
			ZoneDifSolarTrans = 0.0;
			ZoneDifSolarDistAbsorbedTotl = 0.0;
			ZoneDifSolarDistReflectedTotl = 0.0;
			ZoneDifSolarDistTransmittedTotl = 0.0;

			// Loop over all diffuse solar transmitting surfaces (i.e., exterior windows and TDDs) in the current zone
			FirstZoneSurf = Zone( ZoneNum ).SurfaceFirst;
			LastZoneSurf = Zone( ZoneNum ).SurfaceLast;
			for ( DifTransSurfNum = FirstZoneSurf; DifTransSurfNum <= LastZoneSurf; ++DifTransSurfNum ) {
				// Skip surfaces that are not exterior, except for TDD_Diffusers
				if ( ( ( Surface( DifTransSurfNum ).ExtBoundCond != ExternalEnvironment ) && ( Surface( DifTransSurfNum ).ExtBoundCond != OtherSideCondModeledExt ) ) && SurfaceWindow( DifTransSurfNum ).OriginalClass != SurfaceClass_TDD_Diffuser ) continue;

				// Do I need to do anything special for TDDs?
//				if ( SurfaceWindow( DifTransSurfNum ).OriginalClass == SurfaceClass_TDD_Diffuser ) {
//				}

				// Skip surfaces that are not exterior windows or TDD diffusers
				if ( Surface( DifTransSurfNum ).Class != SurfaceClass_Window && SurfaceWindow( DifTransSurfNum ).OriginalClass != SurfaceClass_TDD_Diffuser ) continue;

				//----------------------------------------------------------------------------------------------------------
				// DISTRIBUTE TRANSMITTED DIFFUSE SOLAR THROUGH EXTERIOR WINDOWS AND TDDS TO INTERIOR HEAT TRANSFER SURFACES
				//----------------------------------------------------------------------------------------------------------

				// Init transmitted solar debug vars
				ViewFactorTotal = 0.0;
				WinDifSolarTrans = WinDifSolar( DifTransSurfNum );
				ZoneDifSolarTrans += WinDifSolarTrans;

				// Init Exterior Window accumulators for debugging
				WinDifSolarDistAbsorbedTotl = 0.0;
				WinDifSolarDistReflectedTotl = 0.0;
				WinDifSolarDistTransmittedTotl = 0.0;

				// Loop over all heat transfer surfaces in the current zone that might receive diffuse solar
				for ( HeatTransSurfNum = FirstZoneSurf; HeatTransSurfNum <= LastZoneSurf; ++HeatTransSurfNum ) {
					// Skip surfaces that are not heat transfer surfaces
					if ( ! Surface( HeatTransSurfNum ).HeatTransSurf ) continue;
					// Skip tubular daylighting device domes
					if ( Surface( HeatTransSurfNum ).Class == SurfaceClass_TDD_Dome ) continue;

					// View factor from current (sending) window DifTransSurfNum to current (receiving) surface HeatTransSurfNum
					ViewFactor = ZoneInfo( ZoneNum ).F( HeatTransSurfNum - FirstZoneSurf + 1, DifTransSurfNum - FirstZoneSurf + 1 );
					// debug ViewFactorTotal
					ViewFactorTotal += ViewFactor; // debug

					// Skip receiving surfaces with 0.0 view factor
					if ( ViewFactor <= 0.0 ) continue;

					Real64 const WinDifSolarTrans_Factor( WinDifSolarTrans * ViewFactor );
					Real64 const win_SwitchingFactor( SurfaceWindow( HeatTransSurfNum ).SwitchingFactor );
					Real64 const per_HTSurfaceArea( 1.0 / Surface( HeatTransSurfNum ).Area );
					Real64 const HTsurf_slat_ang( SurfaceWindow( HeatTransSurfNum ).SlatAngThisTS );
					bool const HTsurf_movable_slats( SurfaceWindow( HeatTransSurfNum ).MovableSlats );

					// Calculate diffuse solar from current exterior window absorbed and reflected by current heat transfer surface
					// And calculate transmitted diffuse solar to adjacent zones through interior windows
					ConstrNum = Surface( HeatTransSurfNum ).Construction;
					if ( Construct( ConstrNum ).TransDiff <= 0.0 ) { // Interior Opaque Surface

						// Determine the inside (back) diffuse solar absorptance
						// and reflectance of the current heat transfer surface
						InsideDifAbsorptance = Construct( ConstrNum ).InsideAbsorpSolar;
						// Check for movable insulation; reproduce code from subr. EvalInsideMovableInsulation;
						// Can't call that routine here since cycle prevents SolarShadingGeometry from USEing
						// HeatBalanceSurfaceManager, which contains EvalInsideMovableInsulation
						HMovInsul = 0.0;
						if ( Surface( HeatTransSurfNum ).MaterialMovInsulInt > 0 ) {
							MovInsulSchedVal = GetCurrentScheduleValue( Surface( HeatTransSurfNum ).SchedMovInsulExt );
							if ( MovInsulSchedVal <= 0.0 ) { // Movable insulation not present at current time
								HMovInsul = 0.0;
							} else { // Movable insulation present
								HMovInsul = 1.0 / ( MovInsulSchedVal * Material( Surface( HeatTransSurfNum ).MaterialMovInsulInt ).Resistance );
								AbsInt = Material( Surface( HeatTransSurfNum ).MaterialMovInsulInt ).AbsorpSolar;
							}
						}
						if ( HMovInsul > 0.0 ) InsideDifAbsorptance = AbsInt; // Movable inside insulation present
						// Inside (back) diffuse solar reflectance is assumed to be 1 - absorptance
						InsideDifReflectance = 1.0 - InsideDifAbsorptance;

						// Absorbed diffuse solar [W] = current window transmitted diffuse solar [W]
						//    * view factor from current (sending) window DifTransSurfNum to current (receiving) surface HeatTransSurfNum
						//    * current surface inside solar absorptance
						DifSolarAbsW = WinDifSolarTrans_Factor * InsideDifAbsorptance; // [W]

						// Absorbed diffuse solar [W/m2] = Absorbed diffuse solar [W]
						//                                 / current surface net area
						DifSolarAbs = DifSolarAbsW * per_HTSurfaceArea;

						// Accumulate absorbed diffuse solar [W/m2] on this surface for heat balance calcs
						InitialDifSolInAbs( HeatTransSurfNum ) += DifSolarAbs;

						// Reflected diffuse solar [W] = current window transmitted diffuse solar
						//    * view factor from current (sending) window DifTransSurfNum to current (receiving) surface HeatTransSurfNum
						//    * current window inside solar reflectance
						DifSolarReflW = WinDifSolarTrans_Factor * InsideDifReflectance;

						// Accumulate total reflected distributed diffuse solar for each zone for subsequent interreflection calcs
						InitialZoneDifSolReflW( ZoneNum ) += DifSolarReflW; // [W]

						// Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
						// For opaque surfaces all incident diffuse is either absorbed or reflected
						WinDifSolarDistAbsorbedTotl += DifSolarAbsW; // debug [W]
						WinDifSolarDistReflectedTotl += DifSolarReflW; // debug [W]
						ZoneDifSolarDistAbsorbedTotl += DifSolarAbsW; // debug [W]
						ZoneDifSolarDistReflectedTotl += DifSolarReflW; // debug [W]

					} else { // Exterior or Interior Window

						ConstrNumSh = Surface( HeatTransSurfNum ).ShadedConstruction;
						if ( SurfaceWindow( HeatTransSurfNum ).StormWinFlag == 1 ) {
							ConstrNum = Surface( HeatTransSurfNum ).StormWinConstruction;
							ConstrNumSh = Surface( HeatTransSurfNum ).StormWinShadedConstruction;
						}
						TotGlassLayers = Construct( ConstrNum ).TotGlassLayers;
						ShadeFlag = SurfaceWindow( HeatTransSurfNum ).ShadingFlag;

						if ( SurfaceWindow( HeatTransSurfNum ).WindowModelType != WindowEQLModel ) {
							if ( ShadeFlag <= 0 ) { // No window shading
								// Init accumulator for transmittance calc below
								DifSolarAbsW = 0.0;

								// Calc diffuse solar absorbed by all window glass layers
								// Note: I am assuming here that individual glass layer absorptances have been corrected
								//       to account for layer by layer transmittance and reflection effects.
								for ( IGlass = 1; IGlass <= TotGlassLayers; ++IGlass ) {
									// Calc diffuse solar absorbed from the inside by each window glass layer [W]
									AbsInt = Construct( ConstrNum ).AbsDiffBack( IGlass );
									WinDifSolLayAbsW = WinDifSolarTrans_Factor * Construct( ConstrNum ).AbsDiffBack( IGlass );

									// Accumulate distributed diffuse solar absorbed [W] by overall window for transmittance calc below
									DifSolarAbsW += WinDifSolLayAbsW;

									// Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
									WinDifSolarDistAbsorbedTotl += WinDifSolLayAbsW; // debug
									ZoneDifSolarDistAbsorbedTotl += WinDifSolLayAbsW; // debug

									// Accumulate diffuse solar absorbed from the inside by each window glass layer [W/m2] for heat balance calcs
									InitialDifSolwinAbs( IGlass, HeatTransSurfNum ) += WinDifSolLayAbsW * per_HTSurfaceArea;
								}

								// Calc diffuse solar reflected back to zone
								// I don't really care if this is a window or opaque surface since I am just
								// accumulating all reflected diffuse solar in a zone bucket for "interreflected" distribution
								// Reflected diffuse solar [W] = current window transmitted diffuse solar
								//    * view factor from current (sending) window DifTransSurfNum to current (receiving) surface HeatTransSurfNum
								//    * current window inside solar reflectance
								InsideDifReflectance = Construct( ConstrNum ).ReflectSolDiffBack;
								DifSolarReflW = WinDifSolarTrans_Factor * Construct( ConstrNum ).ReflectSolDiffBack;

								// Accumulate total reflected distributed diffuse solar for each zone for subsequent interreflection calcs
								InitialZoneDifSolReflW( ZoneNum ) += DifSolarReflW; // [W]

								// Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
								WinDifSolarDistReflectedTotl += DifSolarReflW; // debug
								ZoneDifSolarDistReflectedTotl += DifSolarReflW; // debug

								//------------------------------------------------------------------------------
								// DISTRIBUTE TRANSMITTED DIFFUSE SOLAR THROUGH INTERIOR WINDOW TO ADJACENT ZONE
								//------------------------------------------------------------------------------

								// If this receiving window surface (HeatTransSurfNum) is an interior window,
								// calc distributed solar transmitted to adjacent zone [W]
								// NOTE: This calc is here because interior windows are currently assumed to have no shading

								// Get the adjacent surface number for this receiving window surface
								AdjSurfNum = Surface( HeatTransSurfNum ).ExtBoundCond;
								// If the adjacent surface number is > 0, this is an interior window
								if ( AdjSurfNum > 0 ) { // this is an interior window surface

									// Calc diffuse solar from current exterior window
									// transmitted through this interior window to adjacent zone [W]
									// Transmitted diffuse solar [W] = current exterior window transmitted diffuse solar
									//    * view factor from current (sending) window DifTransSurfNum to current (receiving) surface HeatTransSurfNum
									//    - diffuse absorbed by this interior window
									//    - diffuse reflected by this interior window
									DifSolarTransW = WinDifSolarTrans_Factor - DifSolarAbsW - DifSolarReflW;
									// HERE 8/15/07 Note Construct(AdjConstrNum)%TransDiff could be used here since the "front" transmittance for an interior window
									// in the adjacent zone is the correct direction as long as I use the Construct() of the Surface in the adjacent zone.
									// However, the above calculation better conserves energy, although possibly at the expense of less accurate
									// transmittance calcs.
									// Preliminary tests showed fairly good agreement between the two DifSolarTransW calculation methods,
									// but for consistency I stuck with the above.
									AdjConstrNum = Surface( AdjSurfNum ).Construction;
									//              DifSolarTransW = WinDifSolar(DifTransSurfNum) &
									//                                * ViewFactor &
									//                                * Construct(AdjConstrNum)%TransDiff

									// Get the adjacent zone index
									AdjZoneNum = Surface( AdjSurfNum ).Zone;

									// Call routine to distribute diffuse solar transmitted through this interior window into adjacent zone
									CalcInteriorWinTransDifSolInitialDistribution( AdjZoneNum, AdjSurfNum, DifSolarTransW );

								} else { // this is an exterior window surface

									// Calc transmitted Window and Zone total distributed diffuse solar to check for conservation of energy
									// This is not very effective since it assigns whatever distributed diffuse solar has not been
									// absorbed or reflected to transmitted.
									DifSolarTransW = WinDifSolarTrans_Factor - DifSolarAbsW - DifSolarReflW;

								} // this is an interior window surface

								// Accumulate transmitted Window and Zone total distributed diffuse solar to check for conservation of energy
								WinDifSolarDistTransmittedTotl += DifSolarTransW; // debug [W]
								ZoneDifSolarDistTransmittedTotl += DifSolarTransW; // debug [W]

								// Accumulate transmitted diffuse solar for reporting
								InitialDifSolInTrans( HeatTransSurfNum ) += DifSolarTransW * per_HTSurfaceArea;

							} else if ( ShadeFlag == IntShadeOn || ShadeFlag >= 3 ) {
								// Interior, exterior or between-glass shade, screen or blind in place

								// Init accumulator for transmittance calc below
								DifSolarAbsW = 0.0;
								WinDifSolLayAbsW = 0.0;

								// First calc diffuse solar absorbed by each glass layer in this window with shade/blind in place
								auto const & construct_sh( Construct( ConstrNumSh ) );
								auto const & construct_sh_AbsDiffBack( construct_sh.AbsDiffBack );
								auto const & construct_sh_BlAbsDiffBack( construct_sh.BlAbsDiffBack );
								for ( IGlass = 1; IGlass <= construct_sh.TotGlassLayers; ++IGlass ) {
									if ( ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == BGShadeOn || ShadeFlag == ExtScreenOn ) {
										// Calc diffuse solar absorbed in each window glass layer and shade
										WinDifSolLayAbsW = WinDifSolarTrans_Factor * construct_sh_AbsDiffBack( IGlass );
									}

									if ( ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == BGBlindOn ) {
										BlAbsDiffBk = InterpSlatAng( HTsurf_slat_ang, HTsurf_movable_slats, construct_sh_BlAbsDiffBack( _, IGlass ) );
										// Calc diffuse solar absorbed in each window glass layer and shade
										WinDifSolLayAbsW = WinDifSolarTrans_Factor * BlAbsDiffBk;
									}

									// Accumulate distributed diffuse solar absorbed [W] by overall window for transmittance calc below
									DifSolarAbsW += WinDifSolLayAbsW;

									// Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
									WinDifSolarDistAbsorbedTotl += WinDifSolLayAbsW; // debug
									ZoneDifSolarDistAbsorbedTotl += WinDifSolLayAbsW; // debug

									// Accumulate diffuse solar absorbed from the inside by each window glass layer [W/m2] for heat balance calcs
									InitialDifSolwinAbs( IGlass, HeatTransSurfNum ) += WinDifSolLayAbsW * per_HTSurfaceArea;
								}

								// Next calc diffuse solar reflected back to zone from window with shade or blind on
								// Diffuse back solar reflectance, bare glass or shade on
								InsideDifReflectance = Construct( ConstrNum ).ReflectSolDiffBack;
								if ( ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn ) {
									// Diffuse back solar reflectance, blind present, vs. slat angle
									InsideDifReflectance = InterpSlatAng( HTsurf_slat_ang, HTsurf_movable_slats, Construct( ConstrNum ).BlReflectSolDiffBack );
								}
								DifSolarReflW = WinDifSolarTrans_Factor * InsideDifReflectance;

								// Accumulate total reflected distributed diffuse solar for each zone for subsequent interreflection calcs
								InitialZoneDifSolReflW( ZoneNum ) += DifSolarReflW; // [W]

								// Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
								WinDifSolarDistReflectedTotl += DifSolarReflW; // debug
								ZoneDifSolarDistReflectedTotl += DifSolarReflW; // debug

								// Now calc diffuse solar absorbed by shade/blind itself
								BlNum = SurfaceWindow( HeatTransSurfNum ).BlindNumber;
								if ( ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == BGShadeOn || ShadeFlag == ExtScreenOn ) {
									// Calc diffuse solar absorbed by shade or screen [W]
									ShBlDifSolarAbsW = WinDifSolarTrans_Factor * construct_sh.AbsDiffBackShade;
								}
								if ( ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == BGBlindOn ) {
									// Calc diffuse solar absorbed by blind [W]
									AbsDiffBkBl = InterpSlatAng( HTsurf_slat_ang, HTsurf_movable_slats, construct_sh.AbsDiffBackBlind );
									ShBlDifSolarAbsW = WinDifSolarTrans_Factor * AbsDiffBkBl;
								}
								// Correct for divider shadowing
								if ( ShadeFlag == ExtShadeOn || ShadeFlag == ExtBlindOn || ShadeFlag == ExtScreenOn ) ShBlDifSolarAbsW *= SurfaceWindow( HeatTransSurfNum ).GlazedFrac;

								// Accumulate diffuse solar absorbed  by shade or screen [W/m2] for heat balance calcs
								SurfaceWindow( HeatTransSurfNum ).InitialDifSolAbsByShade += ShBlDifSolarAbsW * per_HTSurfaceArea;

								// Accumulate distributed diffuse solar absorbed [W] by overall window for transmittance calc below
								DifSolarAbsW += ShBlDifSolarAbsW;

								// Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
								WinDifSolarDistAbsorbedTotl += ShBlDifSolarAbsW; // debug
								ZoneDifSolarDistAbsorbedTotl += ShBlDifSolarAbsW; // debug

								// Accumulate transmitted Window and Zone total distributed diffuse solar to check for conservation of energy
								// This is not very effective since it assigns whatever distributed diffuse solar has not been
								// absorbed or reflected to transmitted.
								DifSolarTransW = WinDifSolarTrans_Factor - DifSolarAbsW - DifSolarReflW;
								WinDifSolarDistTransmittedTotl += DifSolarTransW; // debug [W]
								ZoneDifSolarDistTransmittedTotl += DifSolarTransW; // debug [W]

								// Accumulate transmitted diffuse solar for reporting
								InitialDifSolInTrans( HeatTransSurfNum ) += DifSolarTransW * per_HTSurfaceArea;

							} else if ( ShadeFlag == SwitchableGlazing ) { // Switchable glazing
								// Init accumulator for transmittance calc below
								DifSolarAbsW = 0.0;

								auto const & construct( Construct( ConstrNum ) );
								auto const & construct_AbsDiffBack( construct.AbsDiffBack );
								auto const & construct_sh( Construct( ConstrNumSh ) );
								auto const & construct_sh_AbsDiffBack( construct_sh.AbsDiffBack );
								for ( IGlass = 1; IGlass <= TotGlassLayers; ++IGlass ) {
									// Calc diffuse solar absorbed in each window glass layer
									WinDifSolLayAbsW = WinDifSolarTrans_Factor * InterpSw( win_SwitchingFactor, construct_AbsDiffBack( IGlass ), construct_sh_AbsDiffBack( IGlass ) );

									// Accumulate distributed diffuse solar absorbed [W] by overall window for transmittance calc below
									DifSolarAbsW += WinDifSolLayAbsW;

									// Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
									WinDifSolarDistAbsorbedTotl += WinDifSolLayAbsW; // debug
									ZoneDifSolarDistAbsorbedTotl += WinDifSolLayAbsW; // debug

									// Accumulate diffuse solar absorbed from the inside by each window glass layer [W/m2] for heat balance calcs
									InitialDifSolwinAbs( IGlass, HeatTransSurfNum ) += WinDifSolLayAbsW * per_HTSurfaceArea;

								}

								// Calc diffuse solar reflected back to zone
								DifSolarReflW = WinDifSolarTrans_Factor * InterpSw( win_SwitchingFactor, construct.ReflectSolDiffBack, construct_sh.ReflectSolDiffBack );

								// Accumulate total reflected distributed diffuse solar for each zone for subsequent interreflection calcs
								InitialZoneDifSolReflW( ZoneNum ) += DifSolarReflW; // [W]

								// Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
								WinDifSolarDistReflectedTotl += DifSolarReflW; // debug
								ZoneDifSolarDistReflectedTotl += DifSolarReflW; // debug

								// Accumulate transmitted Window and Zone total distributed diffuse solar to check for conservation of energy
								// This is not very effective since it assigns whatever distributed diffuse solar has not been
								// absorbed or reflected to transmitted.
								DifSolarTransW = WinDifSolarTrans_Factor - DifSolarAbsW - DifSolarReflW;
								WinDifSolarDistTransmittedTotl += DifSolarTransW; // debug [W]
								ZoneDifSolarDistTransmittedTotl += DifSolarTransW; // debug [W]

								// Accumulate transmitted diffuse solar for reporting
								InitialDifSolInTrans( HeatTransSurfNum ) += DifSolarTransW * per_HTSurfaceArea;

							} // End of shading flag check

						} else {
							// SurfaceWindow(HeatTransSurfNum)%WindowModelType == WindowEQLModel
							// ConstrNum=Surface(HeatTransSurfNum)%Construction
							// call the ASHWAT fenestration model for diffuse radiation here
							CalcEQLOpticalProperty( HeatTransSurfNum, isDIFF, AbsSolDiffBackEQL );

							EQLNum = Construct( ConstrNum ).EQLConsPtr;
							for ( Lay = 1; Lay <= CFS( EQLNum ).NL; ++Lay ) {

								// Calc diffuse solar absorbed from the inside by each layer of EQL model [W]
								//WinDifSolLayAbsW = WinDifSolar(DifTransSurfNum)* ViewFactor * Construct(ConstrNum)%AbsDiffBack(Lay)
								WinDifSolLayAbsW = WinDifSolarTrans_Factor * AbsSolDiffBackEQL( 2, Lay );

								// Accumulate distributed diffuse solar absorbed [W] by overall window for transmittance calc below
								DifSolarAbsW += WinDifSolLayAbsW;

								// Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
								WinDifSolarDistAbsorbedTotl += WinDifSolLayAbsW; // debug
								ZoneDifSolarDistAbsorbedTotl += WinDifSolLayAbsW; // debug

								// Accumulate diffuse solar absorbed from the inside by each window layer [W/m2] for heat balance calcs
								InitialDifSolwinAbs( Lay, HeatTransSurfNum ) += WinDifSolLayAbsW * per_HTSurfaceArea;

								// ASHWAT equivalent layer model may require not the individual layer absorption but the flux
								// InitialDifSolwinEQL(HeatTransSurfNum) = WinDifSolar(DifTransSurfNum)* ViewFactor

							}

							// Calc diffuse solar reflected back to zone
							// I don't really care if this is a window or opaque surface since I am just
							// accumulating all reflected diffuse solar in a zone bucket for "interreflected" distribution
							// Reflected diffuse solar [W] = current window transmitted diffuse solar
							//    * view factor from current (sending) window DifTransSurfNum to current (receiving) surface HeatTransSurfNum
							//    * current window inside solar reflectance
							InsideDifReflectance = Construct( ConstrNum ).ReflectSolDiffBack;
							DifSolarReflW = WinDifSolarTrans_Factor * Construct( ConstrNum ).ReflectSolDiffBack;

							// Accumulate total reflected distributed diffuse solar for each zone for subsequent interreflection calcs
							InitialZoneDifSolReflW( ZoneNum ) += DifSolarReflW; // [W]

							// Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
							WinDifSolarDistReflectedTotl += DifSolarReflW; // debug
							ZoneDifSolarDistReflectedTotl += DifSolarReflW; // debug

							//------------------------------------------------------------------------------
							// DISTRIBUTE TRANSMITTED DIFFUSE SOLAR THROUGH INTERIOR WINDOW TO ADJACENT ZONE
							//------------------------------------------------------------------------------

							// If this receiving window surface (HeatTransSurfNum) is an interior window,
							// calc distributed solar transmitted to adjacent zone [W]
							// NOTE: This calc is here because interior windows are currently assumed to have no shading

							// Get the adjacent surface number for this receiving window surface
							AdjSurfNum = Surface( HeatTransSurfNum ).ExtBoundCond;
							// If the adjacent surface number is > 0, this is an interior window
							if ( AdjSurfNum > 0 ) { // this is an interior window surface

								// Calc diffuse solar from current exterior window
								// transmitted through this interior window to adjacent zone [W]
								// Transmitted diffuse solar [W] = current exterior window transmitted diffuse solar
								//    * view factor from current (sending) window DifTransSurfNum to current (receiving) surface HeatTransSurfNum
								DifSolarTransW = AbsSolDiffBackEQL( 2, CFS( EQLNum ).NL + 1 ) * ViewFactor;
								AdjConstrNum = Surface( AdjSurfNum ).Construction;
								// Get the adjacent zone index
								AdjZoneNum = Surface( AdjSurfNum ).Zone;
								// Call routine to distribute diffuse solar transmitted through this interior window into adjacent zone
								CalcInteriorWinTransDifSolInitialDistribution( AdjZoneNum, AdjSurfNum, DifSolarTransW );

							} else { // this is an exterior window surface

								// Calc transmitted Window and Zone total distributed diffuse solar to check for conservation of energy
								// This is not very effective since it assigns whatever distributed diffuse solar has not been
								// absorbed or reflected to transmitted.
								DifSolarTransW = AbsSolDiffBackEQL( 2, CFS( EQLNum ).NL + 1 ) * ViewFactor;

							} // this is an interior window surface

							// Accumulate transmitted Window and Zone total distributed diffuse solar to check for conservation of energy
							WinDifSolarDistTransmittedTotl += DifSolarTransW; // debug [W]
							ZoneDifSolarDistTransmittedTotl += DifSolarTransW; // debug [W]
							// Accumulate transmitted diffuse solar for reporting
							InitialDifSolInTrans( HeatTransSurfNum ) += DifSolarTransW * per_HTSurfaceArea;

						} //IF (SurfaceWindow(HeatTransSurfNum)%WindowModelType /= WindowEQLModel) THEN

						// HERE 8/14/07 Ignore absorptance and reflectance of Frames and Dividers for now.
						// I would need revised view factors that included these surface types.
						// By ignoring them here, the diffuse solar is accounted for on the other surfaces

						//          IF(SurfaceWindow(HeatTransSurfNum)%FrameArea > 0.0) THEN  ! Window has a frame
						// Note that FrameQRadInAbs is initially calculated in InitSolarHeatGains
						//          END IF

						//          IF(SurfaceWindow(HeatTransSurfNum)%DividerArea > 0.0) THEN  ! Window has dividers
						//            DividerSolAbs = SurfaceWindow(HeatTransSurfNum)%DividerSolAbsorp
						//            IF(SurfaceWindow(HeatTransSurfNum)%DividerType == Suspended) THEN ! Suspended divider; account for inside glass
						//              MatNumGl = Construct(ConstrNum)%LayerPoint(Construct(ConstrNum)%TotLayers)
						//              TransGl = Material(MatNumGl)%Trans
						//              ReflGl = Material(MatNumGl)%ReflectSolDiffBack
						//              AbsGl = 1.0d0-TransGl-ReflGl
						//              DividerSolRefl = 1.0d0-DividerSolAbs
						//              DividerSolAbs = AbsGl + TransGl*(DividerSolAbs + DividerSolRefl*AbsGl)/(1.0d0-DividerSolRefl*ReflGl)
						//            END IF
						// Correct for interior shade transmittance
						//            IF(ShadeFlag == IntShadeOn) THEN
						//              MatNumSh = Construct(ConstrNumSh)%LayerPoint(Construct(ConstrNumSh)%TotLayers)
						//              DividerSolAbs = DividerSolAbs * Material(MatNumSh)%Trans
						//            ELSE IF(ShadeFlag == IntBlindOn) THEN
						//              DividerSolAbs = DividerSolAbs * InterpSlatAng(SurfaceWindow(HeatTransSurfNum)%SlatAngThisTS, &
						//                  SurfaceWindow(HeatTransSurfNum)%MovableSlats,Blind(BlNum)%SolBackDiffDiffTrans)
						//            END IF
						// Note that DividerQRadInAbs is initially calculated in InitSolarHeatGains

						//          END IF  ! Window has dividers

					} // opaque or window heat transfer surface

				} // HeatTransSurfNum = Zone(ZoneNum)%SurfaceFirst, Zone(ZoneNum)%SurfaceLast

				// Check debug var for view factors here
				// ViewFactorTotal
				// Check debug vars for individual transmitting surfaces here
				WinDifSolarDistTotl = WinDifSolarDistAbsorbedTotl + WinDifSolarDistReflectedTotl + WinDifSolarDistTransmittedTotl;
				// WinDifSolarTrans

			} // DifTransSurfNum = Zone(ZoneNum)%SurfaceFirst, Zone(ZoneNum)%SurfaceLast

			// Check debug vars for zone totals here
			ZoneDifSolarDistTotl = ZoneDifSolarDistAbsorbedTotl + ZoneDifSolarDistReflectedTotl + ZoneDifSolarDistTransmittedTotl;
			// ZoneDifSolarTrans
			// ZoneDifSolarDistAbsorbedTotl
			// ZoneDifSolarDistReflectedTotl
			// ZoneDifSolarDistTransmittedTotl
			//    CALL DisplayString('Diffuse Solar Distribution Zone Totals')

		} // ZoneNum = 1, NumOfZones

	}

	void
	CalcInteriorWinTransDifSolInitialDistribution(
		int const ZoneNum, // Zone index number
		int const IntWinSurfNum, // Interior Window Surface number in Zone ZoneNum
		Real64 const IntWinDifSolarTransW // Diffuse Solar transmitted through Interior Window IntWinSurfNum from adjacent zone [W]
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rob Hitchcock
		//       DATE WRITTEN   August 2007
		//       MODIFIED       N/A
		//       RE-ENGINEERED  N/A

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates the initial distribution
		// of diffuse solar transmitted through the given interior window
		// to individual heat transfer surfaces in the given zone.
		// Diffuse solar transmitted through interior windows in this zone
		// to adjacent zones, is added to the InitialZoneDifSolReflW
		// of the adjacent zone for subsequent interreflection calcs

		// METHODOLOGY EMPLOYED:
		// Similar to method used in CalcWinTransDifSolInitialDistribution.
		// Apportions diffuse solar transmitted through an interior window
		// that is then absorbed, reflected, and/or transmitted
		// by other heat transfer surfaces in the given zone.
		// Calculations use:
		// 1. DifSolarTransW calculated in SUBROUTINE CalcWinTransDifSolInitialDistribution,
		// 2. view factors between the interior window and
		// other heat transfer surfaces in the given zone
		// calculated in SUBROUTINE CalcApproximateViewFactors, and
		// 3. surface absorptances, reflectances, and transmittances
		// determined here using revised code from SUBROUTINE InitIntSolarDistribution

		// REFERENCES:

		// Using/Aliasing
		using General::InterpSw;
		using General::InterpSlatAng;
		using ScheduleManager::GetCurrentScheduleValue;
		using namespace DataViewFactorInformation;

		using DataHeatBalSurface::InitialDifSolInAbs;
		using DataHeatBalSurface::InitialDifSolInTrans;
		using DataHeatBalance::InitialDifSolwinAbs;
		using DataHeatBalance::InitialZoneDifSolReflW;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int AdjZoneNum; // Index for adjacent zones
		int AdjSurfNum; // Index for adjacent surfaces
		int ConstrNum; // Construction number
		//  INTEGER :: AdjConstrNum       ! Construction number of other side surface
		int ConstrNumSh; // Shaded construction number
		int IGlass; // Glass layer counter
		int TotGlassLayers; // Number of glass layers in a window construction
		int ShadeFlag; // Shading flag
		Real64 AbsInt; // Tmp var for Inside surface short-wave absorptance
		Real64 MovInsulSchedVal; // Value of the movable insulation schedule for current time
		Real64 HMovInsul; // Conductance of movable insulation
		Real64 InsideDifAbsorptance; // Inside diffuse solar absorptance of a surface
		Real64 InsideDifReflectance; // Inside diffuse solar reflectance of a surface
		int BlNum; // Blind number
		Real64 BlAbsDiffBk; // Glass layer back diffuse solar absorptance when blind in place
		Real64 AbsDiffBkBl; // Blind diffuse back solar absorptance as part of glazing system

		//  REAL(r64)    :: DividerSolAbs      ! Window divider solar absorptance
		//  REAL(r64)    :: DividerSolRefl     ! Window divider solar reflectance
		//  INTEGER :: MatNumGl           ! Glass layer material number
		//  INTEGER :: MatNumSh           ! Shade layer material number
		//  REAL(r64)    :: TransGl,ReflGl,AbsGl ! Glass layer solar transmittance, reflectance, absorptance

		Real64 ViewFactor; // temp var for view factor
		Real64 ViewFactorTotal; // debug var for view factor total
		Real64 WinDifSolarTrans; // debug var for WinDifSolar() [W]
//		Real64 WinDifSolarDistTotl; // debug var for window total distributed diffuse solar [W]
//		Real64 WinDifSolarDistAbsorbedTotl( 0.0 ); // debug var for individual exterior window total distributed
		//           diffuse solar absorbed [W]
//		Real64 WinDifSolarDistReflectedTotl( 0.0 ); // debug var for individual exterior window total distributed
		//           diffuse solar reflected [W]
//		Real64 WinDifSolarDistTransmittedTotl( 0.0 ); // debug var for individual exterior window total distributed
		//           diffuse solar transmitted [W]
		Real64 WinDifSolLayAbsW; // temp var for diffuse solar absorbed by individual glass layer [W]
//		Real64 ZoneDifSolarTrans( 0.0 ); // debug var for WinDifSolar() [W]
		//  REAL(r64)    :: ZoneDifSolarDistTotl    ! debug var for zone total distributed diffuse solar [W]
//		Real64 ZoneDifSolarDistAbsorbedTotl( 0.0 ); // debug var for zone total distributed diffuse solar absorbed [W]
//		Real64 ZoneDifSolarDistReflectedTotl( 0.0 ); // debug var for zone total distributed diffuse solar reflected [W]
//		Real64 ZoneDifSolarDistTransmittedTotl( 0.0 ); // debug var for zone total distributed diffuse solar transmitted [W]

		Real64 DifSolarAbsW; // temp var for diffuse solar absorbed by surface [W]
		Real64 DifSolarAbs; // temp var for diffuse solar absorbed by surface [W/m2]
		Real64 DifSolarReflW; // temp var for diffuse solar reflected by surface [W]
		Real64 DifSolarTransW; // temp var for diffuse solar transmitted through interior window surface [W]
		Real64 ShBlDifSolarAbsW; // temp var for diffuse solar absorbed by shade/blind [W]

		//-------------------------------------------------------------------------------------------------
		// DISTRIBUTE TRANSMITTED DIFFUSE SOLAR THROUGH INTERIOR WINDOW TO INTERIOR HEAT TRANSFER SURFACES
		//-------------------------------------------------------------------------------------------------

		// Init debug vars
		ViewFactorTotal = 0.0;
		WinDifSolarTrans = IntWinDifSolarTransW;

		// Init first and last surfnums for this zone
		int const FirstZoneSurf( Zone( ZoneNum ).SurfaceFirst );
		int const LastZoneSurf( Zone( ZoneNum ).SurfaceLast );

		// Loop over all heat transfer surfaces in the current zone that might receive diffuse solar
		Real64 InitialZoneDifSolReflW_zone( 0.0 );
		for ( int HeatTransSurfNum = FirstZoneSurf; HeatTransSurfNum <= LastZoneSurf; ++HeatTransSurfNum ) {
			// Skip surfaces that are not heat transfer surfaces
			if ( ! Surface( HeatTransSurfNum ).HeatTransSurf ) continue;
			// Skip tubular daylighting device domes
			if ( Surface( HeatTransSurfNum ).Class == SurfaceClass_TDD_Dome ) continue;

			// View factor from current (sending) window IntWinSurfNum to current (receiving) surface HeatTransSurfNum
			ViewFactor = ZoneInfo( ZoneNum ).F( HeatTransSurfNum - FirstZoneSurf + 1, IntWinSurfNum - FirstZoneSurf + 1 );
			// debug ViewFactorTotal
			ViewFactorTotal += ViewFactor; // debug

			// Skip receiving surfaces with 0.0 view factor
			if ( ViewFactor <= 0.0 ) continue;
			Real64 const SolarTrans_ViewFactor( IntWinDifSolarTransW * ViewFactor );

			// Calculate diffuse solar from current interior window absorbed and reflected by current heat transfer surface
			// And calculate transmitted diffuse solar to adjacent zones through interior windows
			ConstrNum = Surface( HeatTransSurfNum ).Construction;
			if ( Construct( ConstrNum ).TransDiff <= 0.0 ) { // Interior Opaque Surface

				// Determine the inside (back) diffuse solar absorptance
				// and reflectance of the current heat transfer surface
				InsideDifAbsorptance = Construct( ConstrNum ).InsideAbsorpSolar;
				// Check for movable insulation; reproduce code from subr. EvalInsideMovableInsulation;
				// Can't call that routine here since cycle prevents SolarShadingGeometry from USEing
				// HeatBalanceSurfaceManager, which contains EvalInsideMovableInsulation
				HMovInsul = 0.0;
				if ( Surface( HeatTransSurfNum ).MaterialMovInsulInt > 0 ) {
					MovInsulSchedVal = GetCurrentScheduleValue( Surface( HeatTransSurfNum ).SchedMovInsulExt );
					if ( MovInsulSchedVal <= 0.0 ) { // Movable insulation not present at current time
						HMovInsul = 0.0;
					} else { // Movable insulation present
						HMovInsul = 1.0 / ( MovInsulSchedVal * Material( Surface( HeatTransSurfNum ).MaterialMovInsulInt ).Resistance );
						AbsInt = Material( Surface( HeatTransSurfNum ).MaterialMovInsulInt ).AbsorpSolar;
					}
				}
				if ( HMovInsul > 0.0 ) InsideDifAbsorptance = AbsInt; // Movable inside insulation present
				// Inside (back) diffuse solar reflectance is assumed to be 1 - absorptance
				InsideDifReflectance = 1.0 - InsideDifAbsorptance;

				// Absorbed diffuse solar [W] = current window transmitted diffuse solar [W]
				//    * view factor from current (sending) window IntWinSurfNum to current (receiving) surface HeatTransSurfNum
				//    * current surface inside solar absorptance
				DifSolarAbsW = SolarTrans_ViewFactor * InsideDifAbsorptance; // [W]

				// Absorbed diffuse solar [W/m2] = Absorbed diffuse solar [W]
				//                                 / current surface net area
				DifSolarAbs = DifSolarAbsW / Surface( HeatTransSurfNum ).Area;

				// Accumulate absorbed diffuse solar [W/m2] on this surface for heat balance calcs
				InitialDifSolInAbs( HeatTransSurfNum ) += DifSolarAbs;

				// Reflected diffuse solar [W] = current window transmitted diffuse solar
				//    * view factor from current (sending) window IntWinSurfNum to current (receiving) surface HeatTransSurfNum
				//    * current window inside solar reflectance
				DifSolarReflW = SolarTrans_ViewFactor * InsideDifReflectance;

				// Accumulate total reflected distributed diffuse solar for each zone for subsequent interreflection calcs
				InitialZoneDifSolReflW_zone += DifSolarReflW; // [W]

				// Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
				// For opaque surfaces all incident diffuse is either absorbed or reflected
//				WinDifSolarDistAbsorbedTotl += DifSolarAbsW; // debug [W]
//				WinDifSolarDistReflectedTotl += DifSolarReflW; // debug [W]
//				ZoneDifSolarDistAbsorbedTotl += DifSolarAbsW; // debug [W]
//				ZoneDifSolarDistReflectedTotl += DifSolarReflW; // debug [W]

			} else { // Exterior or Interior Window

				ConstrNumSh = Surface( HeatTransSurfNum ).ShadedConstruction;
				if ( SurfaceWindow( HeatTransSurfNum ).StormWinFlag == 1 ) {
					ConstrNum = Surface( HeatTransSurfNum ).StormWinConstruction;
					ConstrNumSh = Surface( HeatTransSurfNum ).StormWinShadedConstruction;
				}
				TotGlassLayers = Construct( ConstrNum ).TotGlassLayers;
				ShadeFlag = SurfaceWindow( HeatTransSurfNum ).ShadingFlag;

				if ( ShadeFlag <= 0 ) { // No window shading
					// Init accumulator for transmittance calc below
					DifSolarAbsW = 0.0;

					// Calc diffuse solar absorbed by all window glass layers
					// Note: I am assuming here that individual glass layer absorptances have been corrected
					//       to account for layer by layer transmittance and reflection effects.
					for ( IGlass = 1; IGlass <= TotGlassLayers; ++IGlass ) {
						// Calc diffuse solar absorbed from the inside by each window glass layer [W]
						AbsInt = Construct( ConstrNum ).AbsDiffBack( IGlass );
						WinDifSolLayAbsW = SolarTrans_ViewFactor * Construct( ConstrNum ).AbsDiffBack( IGlass );

						// Accumulate distributed diffuse solar absorbed [W] by overall window for transmittance calc below
						DifSolarAbsW += WinDifSolLayAbsW;

						// Accumulate diffuse solar absorbed from the inside by each window glass layer [W/m2] for heat balance calcs
						InitialDifSolwinAbs( IGlass, HeatTransSurfNum ) += ( WinDifSolLayAbsW / Surface( HeatTransSurfNum ).Area );
					}
					// Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
//					WinDifSolarDistAbsorbedTotl += DifSolarAbsW; // debug
//					ZoneDifSolarDistAbsorbedTotl += DifSolarAbsW; // debug

					// Calc diffuse solar reflected back to zone
					// I don't really care if this is a window or opaque surface since I am just
					// accumulating all reflected diffuse solar in a zone bucket for "interreflected" distribution
					// Reflected diffuse solar [W] = current window transmitted diffuse solar
					//    * view factor from current (sending) window IntWinSurfNum to current (receiving) surface HeatTransSurfNum
					//    * current window inside solar reflectance
					DifSolarReflW = SolarTrans_ViewFactor * Construct( ConstrNum ).ReflectSolDiffBack;

					// Accumulate total reflected distributed diffuse solar for each zone for subsequent interreflection calcs
					InitialZoneDifSolReflW_zone += DifSolarReflW; // [W]

					// Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
//					WinDifSolarDistReflectedTotl += DifSolarReflW; // debug
//					ZoneDifSolarDistReflectedTotl += DifSolarReflW; // debug

					// Calc transmitted Window and Zone total distributed diffuse solar to check for conservation of energy
					// This is not very effective since it assigns whatever distributed diffuse solar has not been
					// absorbed or reflected to transmitted.
					DifSolarTransW = SolarTrans_ViewFactor - DifSolarAbsW - DifSolarReflW;

					// Accumulate transmitted Window and Zone total distributed diffuse solar to check for conservation of energy
//					WinDifSolarDistTransmittedTotl += DifSolarTransW; // debug [W]
//					ZoneDifSolarDistTransmittedTotl += DifSolarTransW; // debug [W]

					// Accumulate transmitted diffuse solar for reporting
					InitialDifSolInTrans( HeatTransSurfNum ) += ( DifSolarTransW / Surface( HeatTransSurfNum ).Area );

					//-----------------------------------------------------------------------------------
					// ADD TRANSMITTED DIFFUSE SOLAR THROUGH INTERIOR WINDOW TO ADJACENT ZONE
					// TOTAL REFLECTED DIFFUSE SOLAR FOR SUBSEQUENT INTERREFLECTION CALCS
					//-----------------------------------------------------------------------------------

					// If this receiving window surface (HeatTransSurfNum) is an interior window,
					// add transmitted diffuse solar to adjacent zone total reflected distributed
					// diffuse solar for subsequent interreflection calcs
					// NOTE: This calc is here because interior windows are currently assumed to have no shading

					// Get the adjacent surface number for this receiving window surface
					AdjSurfNum = Surface( HeatTransSurfNum ).ExtBoundCond;
					// If the adjacent surface number is > 0, this is an interior window
					if ( AdjSurfNum > 0 ) { // this is an interior window surface

						// Get the adjacent zone index
						AdjZoneNum = Surface( AdjSurfNum ).Zone;

						// Add transmitted diffuse solar to total reflected distributed diffuse solar for each zone
						// for subsequent interreflection calcs
						InitialZoneDifSolReflW( AdjZoneNum ) += DifSolarTransW; // [W]

					}

				} else if ( ShadeFlag == IntShadeOn || ShadeFlag >= 3 ) {
					// Interior, exterior or between-glass shade, screen or blind in place

					// Init accumulator for transmittance calc below
					DifSolarAbsW = 0.0;
					WinDifSolLayAbsW = 0.0;

					// First calc diffuse solar absorbed by each glass layer in this window with shade/blind in place
					for ( IGlass = 1; IGlass <= Construct( ConstrNumSh ).TotGlassLayers; ++IGlass ) {
						if ( ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == BGShadeOn || ShadeFlag == ExtScreenOn ) {
							// Calc diffuse solar absorbed in each window glass layer and shade
							WinDifSolLayAbsW = SolarTrans_ViewFactor * Construct( ConstrNumSh ).AbsDiffBack( IGlass );
						}

						if ( ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == BGBlindOn ) {
							BlAbsDiffBk = InterpSlatAng( SurfaceWindow( HeatTransSurfNum ).SlatAngThisTS, SurfaceWindow( HeatTransSurfNum ).MovableSlats, Construct( ConstrNumSh ).BlAbsDiffBack( _, IGlass ) );
							// Calc diffuse solar absorbed in each window glass layer and shade
							WinDifSolLayAbsW = SolarTrans_ViewFactor * BlAbsDiffBk;
						}

						// Accumulate distributed diffuse solar absorbed [W] by overall window for transmittance calc below
						DifSolarAbsW += WinDifSolLayAbsW;

						// Accumulate diffuse solar absorbed from the inside by each window glass layer [W/m2] for heat balance calcs
						InitialDifSolwinAbs( IGlass, HeatTransSurfNum ) += ( WinDifSolLayAbsW / Surface( HeatTransSurfNum ).Area );
					}
					// Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
//					WinDifSolarDistAbsorbedTotl += DifSolarAbsW; // debug
//					ZoneDifSolarDistAbsorbedTotl += DifSolarAbsW; // debug

					// Next calc diffuse solar reflected back to zone from window with shade or blind on
					// Diffuse back solar reflectance, bare glass or shade on
					InsideDifReflectance = Construct( ConstrNum ).ReflectSolDiffBack;
					if ( ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn ) {
						// Diffuse back solar reflectance, blind present, vs. slat angle
						InsideDifReflectance = InterpSlatAng( SurfaceWindow( HeatTransSurfNum ).SlatAngThisTS, SurfaceWindow( HeatTransSurfNum ).MovableSlats, Construct( ConstrNum ).BlReflectSolDiffBack );
					}
					DifSolarReflW = SolarTrans_ViewFactor * InsideDifReflectance;

					// Accumulate total reflected distributed diffuse solar for each zone for subsequent interreflection calcs
					InitialZoneDifSolReflW_zone += DifSolarReflW; // [W]

					// Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
//					WinDifSolarDistReflectedTotl += DifSolarReflW; // debug
//					ZoneDifSolarDistReflectedTotl += DifSolarReflW; // debug

					// Now calc diffuse solar absorbed by shade/blind itself
					BlNum = SurfaceWindow( HeatTransSurfNum ).BlindNumber;
					if ( ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == BGShadeOn || ShadeFlag == ExtScreenOn ) {
						// Calc diffuse solar absorbed by shade or screen [W]
						ShBlDifSolarAbsW = SolarTrans_ViewFactor * Construct( ConstrNumSh ).AbsDiffBackShade;
					}
					if ( ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == BGBlindOn ) {
						// Calc diffuse solar absorbed by blind [W]
						AbsDiffBkBl = InterpSlatAng( SurfaceWindow( HeatTransSurfNum ).SlatAngThisTS, SurfaceWindow( HeatTransSurfNum ).MovableSlats, Construct( ConstrNumSh ).AbsDiffBackBlind );
						ShBlDifSolarAbsW = SolarTrans_ViewFactor * AbsDiffBkBl;
					}
					// Correct for divider shadowing
					if ( ShadeFlag == ExtShadeOn || ShadeFlag == ExtBlindOn || ShadeFlag == ExtScreenOn ) ShBlDifSolarAbsW *= SurfaceWindow( HeatTransSurfNum ).GlazedFrac;

					// Accumulate diffuse solar absorbed  by shade or screen [W/m2] for heat balance calcs
					SurfaceWindow( HeatTransSurfNum ).InitialDifSolAbsByShade += ( ShBlDifSolarAbsW / Surface( HeatTransSurfNum ).Area );

					// Accumulate distributed diffuse solar absorbed [W] by overall window for transmittance calc below
					DifSolarAbsW += ShBlDifSolarAbsW;

					// Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
//					WinDifSolarDistAbsorbedTotl += ShBlDifSolarAbsW; // debug
//					ZoneDifSolarDistAbsorbedTotl += ShBlDifSolarAbsW; // debug

					// Accumulate transmitted Window and Zone total distributed diffuse solar to check for conservation of energy
					// This is not very effective since it assigns whatever distributed diffuse solar has not been
					// absorbed or reflected to transmitted.
					DifSolarTransW = SolarTrans_ViewFactor - DifSolarAbsW - DifSolarReflW;
//					WinDifSolarDistTransmittedTotl += DifSolarTransW; // debug [W]
//					ZoneDifSolarDistTransmittedTotl += DifSolarTransW; // debug [W]

					// Accumulate transmitted diffuse solar for reporting
					InitialDifSolInTrans( HeatTransSurfNum ) += ( DifSolarTransW / Surface( HeatTransSurfNum ).Area );

				} else if ( ShadeFlag == SwitchableGlazing ) { // Switchable glazing
					// Init accumulator for transmittance calc below
					DifSolarAbsW = 0.0;

					for ( IGlass = 1; IGlass <= TotGlassLayers; ++IGlass ) {
						// Calc diffuse solar absorbed in each window glass layer
						WinDifSolLayAbsW = SolarTrans_ViewFactor * InterpSw( SurfaceWindow( HeatTransSurfNum ).SwitchingFactor, Construct( ConstrNum ).AbsDiffBack( IGlass ), Construct( ConstrNumSh ).AbsDiffBack( IGlass ) );

						// Accumulate distributed diffuse solar absorbed [W] by overall window for transmittance calc below
						DifSolarAbsW += WinDifSolLayAbsW;

						// Accumulate diffuse solar absorbed from the inside by each window glass layer [W/m2] for heat balance calcs
						InitialDifSolwinAbs( IGlass, HeatTransSurfNum ) += ( WinDifSolLayAbsW / Surface( HeatTransSurfNum ).Area );

					}
					// Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
//					WinDifSolarDistAbsorbedTotl += DifSolarAbsW; // debug
//					ZoneDifSolarDistAbsorbedTotl += DifSolarAbsW; // debug

					// Calc diffuse solar reflected back to zone
					DifSolarReflW = SolarTrans_ViewFactor * InterpSw( SurfaceWindow( HeatTransSurfNum ).SwitchingFactor, Construct( ConstrNum ).ReflectSolDiffBack, Construct( ConstrNumSh ).ReflectSolDiffBack );

					// Accumulate total reflected distributed diffuse solar for each zone for subsequent interreflection calcs
					InitialZoneDifSolReflW_zone += DifSolarReflW; // [W]

					// Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
//					WinDifSolarDistReflectedTotl += DifSolarReflW; // debug
//					ZoneDifSolarDistReflectedTotl += DifSolarReflW; // debug

					// Accumulate transmitted Window and Zone total distributed diffuse solar to check for conservation of energy
					// This is not very effective since it assigns whatever distributed diffuse solar has not been
					// absorbed or reflected to transmitted.
					DifSolarTransW = SolarTrans_ViewFactor - DifSolarAbsW - DifSolarReflW;
//					WinDifSolarDistTransmittedTotl += DifSolarTransW; // debug [W]
//					ZoneDifSolarDistTransmittedTotl += DifSolarTransW; // debug [W]

					// Accumulate transmitted diffuse solar for reporting
					InitialDifSolInTrans( HeatTransSurfNum ) += ( DifSolarTransW / Surface( HeatTransSurfNum ).Area );

				} // End of shading flag check

				// HERE 8/14/07 Ignore absorptance and reflectance of Frames and Dividers for now.
				// I would need revised view factors that included these surface types.
				// By ignoring them here, the diffuse solar is accounted for on the other surfaces

				//          IF(SurfaceWindow(HeatTransSurfNum)%FrameArea > 0.0) THEN  ! Window has a frame
				// Note that FrameQRadInAbs is initially calculated in InitSolarHeatGains
				//          END IF

				//          IF(SurfaceWindow(HeatTransSurfNum)%DividerArea > 0.0) THEN  ! Window has dividers
				//            DividerSolAbs = SurfaceWindow(HeatTransSurfNum)%DividerSolAbsorp
				//            IF(SurfaceWindow(HeatTransSurfNum)%DividerType == Suspended) THEN ! Suspended divider; account for inside glass
				//              MatNumGl = Construct(ConstrNum)%LayerPoint(Construct(ConstrNum)%TotLayers)
				//              TransGl = Material(MatNumGl)%Trans
				//              ReflGl = Material(MatNumGl)%ReflectSolDiffBack
				//              AbsGl = 1.0d0-TransGl-ReflGl
				//              DividerSolRefl = 1.0d0-DividerSolAbs
				//              DividerSolAbs = AbsGl + TransGl*(DividerSolAbs + DividerSolRefl*AbsGl)/(1.0d0-DividerSolRefl*ReflGl)
				//            END IF
				// Correct for interior shade transmittance
				//            IF(ShadeFlag == IntShadeOn) THEN
				//              MatNumSh = Construct(ConstrNumSh)%LayerPoint(Construct(ConstrNumSh)%TotLayers)
				//              DividerSolAbs = DividerSolAbs * Material(MatNumSh)%Trans
				//            ELSE IF(ShadeFlag == IntBlindOn) THEN
				//              DividerSolAbs = DividerSolAbs * InterpSlatAng(SurfaceWindow(HeatTransSurfNum)%SlatAngThisTS, &
				//                  SurfaceWindow(HeatTransSurfNum)%MovableSlats,Blind(BlNum)%SolBackDiffDiffTrans)
				//            END IF
				// Note that DividerQRadInAbs is initially calculated in InitSolarHeatGains

				//          END IF  ! Window has dividers
			} // opaque or window heat transfer surface

		} // HeatTransSurfNum = Zone(ZoneNum)%SurfaceFirst, Zone(ZoneNum)%SurfaceLast
		InitialZoneDifSolReflW( ZoneNum ) += InitialZoneDifSolReflW_zone;

		// Check debug var for view factors here
		// ViewFactorTotal
		// Check debug vars for individual transmitting surfaces here
//		WinDifSolarDistTotl = WinDifSolarDistAbsorbedTotl + WinDifSolarDistReflectedTotl + WinDifSolarDistTransmittedTotl; //Debug
		// WinDifSolarTrans

	}

	void
	CalcComplexWindowOverlap(
		BSDFGeomDescr & Geom, // State Geometry
		BSDFWindowGeomDescr const & Window, // Window Geometry
		int const ISurf // Surface number of the complex fenestration
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Vidanovic
		//       DATE WRITTEN   May 2012
		//       MODIFIED       Simon Vidanovic (May 2013) - added overlaps calculations for daylighting
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// For each of basis directions on back surface of the window calculates
		// overlap areas. It also calculates overlap areas and reflectances for daylighting calculations

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace Vectors;
		using DataHeatBalance::Material;

		// Locals
		Real64 XShadowProjection; // temporary buffer
		Real64 YShadowProjection; // temporary buffer

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 XSp; // for calc BSDF projection direction
		Real64 YSp; // for calc BSDF projection direction
		Real64 ZSp; // for calc BSDF projection direction
		Real64 SdotX; // temporary variable for manipulating .dot. product
		Real64 SdotY; // temporary variable for manipulating .dot. product
		Real64 SdotZ; // temporary variable for manipulating .dot. product
		int BackSurfaceNumber; // current back surface number
		int NVT; // Number of vertices of back surface
		static Array1D< Real64 > XVT; // X,Y,Z coordinates of vertices of
		static Array1D< Real64 > YVT; // back surfaces projected into system
		static Array1D< Real64 > ZVT; // relative to receiving surface
		int NS1; // Number of the figure being overlapped
		int NS2; // Number of the figure doing overlapping
		int NS3; // Location to place results of overlap
		int IRay; // Current ray of BSDF direction
		int KBkSurf; // Current back surface
		int BaseSurf; // Base surface number
		int N;
		int CurBaseSurf; // Currnet base surface number for shadow overlap calcualtions
		int CurBackSurface; // Current back surface number for base surface

		// Daylighting
		int IConst; // Construction number of back surface
		int InsideConLay; // Construction's inside material layer number
		Real64 VisibleReflectance; // Visible reflectance for inside surface material
		Real64 TotAOverlap; // Total overlap area for given outgoing direction
		Real64 TotARhoVisOverlap; // Total overlap area time reflectance for given outgoing direction

		XVT.dimension( MaxVerticesPerSurface + 1, 0.0 );
		YVT.dimension( MaxVerticesPerSurface + 1, 0.0 );
		ZVT.dimension( MaxVerticesPerSurface + 1, 0.0 );

		Geom.AOverlap.dimension( Window.NBkSurf, Geom.Trn.NBasis, 0.0 );
		Geom.ARhoVisOverlap.dimension( Window.NBkSurf, Geom.Trn.NBasis, 0.0 );
		Geom.AveRhoVisOverlap.dimension( Geom.Trn.NBasis, 0.0 );

		// First to calculate and store coordinates of the window surface
		LOCHCA = 1;
		BaseSurf = Surface( ISurf ).BaseSurf;

		// Base surface contains current window surface (ISurf).
		// Since that is case, below transformation should always return ZVT = 0.0
		// for every possible transformation
		CTRANS( ISurf, BaseSurf, NVT, XVT, YVT, ZVT );

		// HTRANS routine is using coordinates stored in XVS and YVS in order to calculate
		// surface area.  Since both projections are equal to zero, then simply
		// compy these values into XVS and YVS arrays
		for ( N = 1; N <= NVT; ++N ) {
			XVS( N ) = XVT( N );
			YVS( N ) = YVT( N );
		}

		// This calculates the area stored in XVS and YVS
		//CALL HTRANS(1,LOCHCA,NVT)
		HTRANS1( LOCHCA, NVT );
		//HCAREA(LOCHCA) = -HCAREA(LOCHCA)

		// Calculation of overlap areas for each outgoing basis direction
		for ( IRay = 1; IRay <= Geom.Trn.NBasis; ++IRay ) { // basis directions loop (on back surface)
			// For current basis direction calculate dot product between window surface
			// and basis direction.  This will be used to calculate projection of each
			// of the back surfaces to window surface for given basis direciton
			SdotX = dot( Surface( ISurf ).lcsx, Geom.sTrn( IRay ) );
			SdotY = dot( Surface( ISurf ).lcsy, Geom.sTrn( IRay ) );
			SdotZ = dot( Surface( ISurf ).lcsz, Geom.sTrn( IRay ) );
			XSp = -SdotX;
			YSp = -SdotY;
			ZSp = -SdotZ;

			// Projection of shadows for current basis direciton
			if ( std::abs( ZSp ) > 1.e-4 ) {
				XShadowProjection = XSp / ZSp;
				YShadowProjection = YSp / ZSp;
				if ( std::abs( XShadowProjection ) < 1.e-8 ) XShadowProjection = 0.0;
				if ( std::abs( YShadowProjection ) < 1.e-8 ) YShadowProjection = 0.0;
			} else {
				XShadowProjection = 0.0;
				YShadowProjection = 0.0;
			}

			for ( KBkSurf = 1; KBkSurf <= Window.NBkSurf; ++KBkSurf ) { //back surf loop
				//BaseSurf = Surface(ISurf)%BaseSurf
				BackSurfaceNumber = ShadowComb( BaseSurf ).BackSurf( KBkSurf );

				// Transform coordinates of back surface from general system to the
				// plane of the receiving surface
				CTRANS( BackSurfaceNumber, BaseSurf, NVT, XVT, YVT, ZVT );

				// Project "shadow" from back surface along sun's rays to receiving surface.  Back surface vertices
				// become clockwise sequential.

				for ( N = 1; N <= NVT; ++N ) {
					XVS( N ) = XVT( N ) - XShadowProjection * ZVT( N );
					YVS( N ) = YVT( N ) - YShadowProjection * ZVT( N );
				}

				// Transform to the homogeneous coordinate system.

				NS3 = LOCHCA + 1;
				//NS3      = LOCHCA
				HCT( NS3 ) = 0.0;
				//CALL HTRANS(1,NS3,NVT)
				HTRANS1( NS3, NVT );

				// Determine area of overlap of projected back surface and receiving surface.

				NS1 = 1;
				NS2 = NS3;
				HCT( NS3 ) = 1.0;
				DeterminePolygonOverlap( NS1, NS2, NS3 );

				if ( OverlapStatus == NoOverlap ) continue; // to next back surface
				if ( ( OverlapStatus == TooManyVertices ) || ( OverlapStatus == TooManyFigures ) ) break; // back surfaces DO loop

				LOCHCA = NS3;
				HCNS( LOCHCA ) = BackSurfaceNumber;
				HCAREA( LOCHCA ) = -HCAREA( LOCHCA );

				Geom.AOverlap( KBkSurf, IRay ) = HCAREA( LOCHCA );
			} // DO KBkSurf  = 1 , NBkSurf

			// If some of back surfaces is contained in base surface, then need to substract shadow of subsurface
			// from shadow on base surface.  Reson is that above shadowing algorithm is calculating shadow wihtout
			// influence of subsurfaces
			for ( KBkSurf = 1; KBkSurf <= Window.NBkSurf; ++KBkSurf ) { //back surf loop
				BackSurfaceNumber = ShadowComb( BaseSurf ).BackSurf( KBkSurf );
				CurBaseSurf = Surface( BackSurfaceNumber ).BaseSurf;
				if ( CurBaseSurf != BackSurfaceNumber ) {
					// Search if that base surface in list of back surfaces for current window
					CurBackSurface = 0;
					for ( N = 1; N <= Window.NBkSurf; ++N ) {
						if ( ShadowComb( BaseSurf ).BackSurf( N ) == CurBaseSurf ) {
							CurBackSurface = N;
							break;
						}
					}
					if ( CurBackSurface != 0 ) {
						Geom.AOverlap( CurBackSurface, IRay ) -= Geom.AOverlap( KBkSurf, IRay );
					}
				}
			}

			// Calculate overlap area times reflectance.  This is necessary for complex fenestration daylighting calculations
			TotAOverlap = 0.0;
			TotARhoVisOverlap = 0.0;
			for ( KBkSurf = 1; KBkSurf <= Window.NBkSurf; ++KBkSurf ) { //back surf loop
				BackSurfaceNumber = ShadowComb( BaseSurf ).BackSurf( KBkSurf );
				CurBaseSurf = Surface( BackSurfaceNumber ).BaseSurf;
				IConst = Surface( BackSurfaceNumber ).Construction;
				InsideConLay = Construct( IConst ).TotLayers;
				if ( SurfaceWindow( BackSurfaceNumber ).WindowModelType == WindowBSDFModel ) {
					VisibleReflectance = Construct( IConst ).ReflectVisDiffBack;
				} else {
					VisibleReflectance = ( 1.0 - Material( InsideConLay ).AbsorpVisible );
				}
				Geom.ARhoVisOverlap( KBkSurf, IRay ) = Geom.AOverlap( KBkSurf, IRay ) * VisibleReflectance;
				TotAOverlap += Geom.AOverlap( KBkSurf, IRay );
				TotARhoVisOverlap += Geom.ARhoVisOverlap( KBkSurf, IRay );
			}

			if ( TotAOverlap != 0.0 ) {
				Geom.AveRhoVisOverlap( IRay ) = TotARhoVisOverlap / TotAOverlap;
			}

		} // DO IRay = 1, Geom%Trn%NBasis

		// Reset back shadowing counter since complex windows do not need it anymore
		LOCHCA = 1;

	}

	void
	TimestepInitComplexFenestration()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Vidanovic
		//       DATE WRITTEN   May 2012
		//       MODIFIED       May 2012 (Initialize complex fenestration in case of EMS)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Performs initialization of complex fenestration. It also performs check if current surface containing
		// complex fenestration have construction changed (by EMS) in which case performs addition of current states
		// into complex fenestration array

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using WindowComplexManager::CheckCFSStates;

		// Locals
		int iSurf; // Current surface number
		int iState; // current state number
		int NumOfStates; // number of states for current window

		for ( iSurf = 1; iSurf <= TotSurfaces; ++iSurf ) {
			if ( SurfaceWindow( iSurf ).WindowModelType == WindowBSDFModel ) {
				// This will check complex fenestrations state and add new one if necessary (EMS case)
				CheckCFSStates( iSurf );

				NumOfStates = ComplexWind( iSurf ).NumStates;

				// Check for overlap areas and initialize if necessary
				for ( iState = 1; iState <= NumOfStates; ++iState ) {
					// do initialization only once
					if ( ComplexWind( iSurf ).Geom( iState ).InitState ) {
						CalcComplexWindowOverlap( ComplexWind( iSurf ).Geom( iState ), ComplexWind( iSurf ), iSurf );
						ComplexWind( iSurf ).Geom( iState ).InitState = false;
					}
				}
			}
		}

	}

} // SolarShading

} // EnergyPlus
