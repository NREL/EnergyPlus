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

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/numeric.hh>

// EnergyPlus Headers
#include <DaylightingDevices.hh>
#include <DataDaylighting.hh>
#include <DataDaylightingDevices.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataIPShortCuts.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSurfaces.hh>
#include <DataSystemVariables.hh>
#include <DisplayRoutines.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <HeatBalanceInternalHeatGains.hh>
#include <InputProcessor.hh>
#include <OutputProcessor.hh>
#include <UtilityRoutines.hh>
#include <Vectors.hh>

namespace EnergyPlus {

namespace DaylightingDevices {

	// MODULE INFORMATION:
	//       AUTHOR         Peter Graham Ellis
	//       DATE WRITTEN   May 2003
	//       MODIFIED       PGE, Aug 2003:  Added daylighting shelves.
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// Simulates daylighting devices, namely tubular daylighting devices (a.k.a. light pipes, sun pipes, or
	// tubular skylights) and daylighting shelves (a.k.a. light shelves).

	// METHODOLOGY EMPLOYED:
	// TUBULAR DAYLIGHTING DEVICE
	// A tubular daylighting device (TDD) is constructed of three components:  a dome, a pipe, and a diffuser.
	// The dome and diffuser are treated as special window surfaces to take advantage of many of the already
	// existing daylighting and heat transfer routines.  Together the dome and diffuser become "receiver"
	// and "transmitter", i.e. radiation entering the dome ends up exiting the diffuser.  The geometry and
	// construction of the pipe and the constructions of the window surfaces determine the transmittance of
	// the TDD.
	// The main task of the module is to determine the total transmittance of the TDD for several
	// types of radiation, including visible beam, solar beam, solar isotropic, and solar anisotropic sky.
	// The fundamental building block for each type of radiation is the transmittance of a beam or ray of
	// radiation (visible or solar) at a given incident angle.  This transmittance is calculated and
	// tabulated for each TDD during initialization using a numerical integral based on the analytical
	// solution derived by Swift and Smith.  Diffuse transmittances are subsequently calculated by integrating
	// discrete rays over the viewable area.
	// There are three parts to the TDD model:
	//   1. Daylighting
	//   2. Solar gain
	//   3. Thermal conductive/convective gain
	// The daylighting simulation uses the visible beam transmittance to find the amount of direct beam
	// solar illuminance that enters the zone.  The visible beam transmittance is also used for calculating
	// the contribution of each discrete ray from a differential area during a comprehensive sky/ground
	// integration.
	// The heat balance simulation handles both the solar gain and thermal conductive/convective gain.
	// Although visible and solar radiation are similar, solar gain is simulated very differently from the
	// daylighting illuminance calculations.  The gain from direct beam solar is found using the
	// solar beam transmittance.  The diffuse solar, however, is more complicated.  A sky/ground integration
	// is NOT performed.  Instead anisotropic sky view factor multipliers (AnisoSkyMult) are calculated for
	// each surface.  The diffuse sky/ground transmittance of the TDD is solved using a modification of the
	// AnisoSkyMult.  The ground radiation transmittance and anisotropic sky transmittance are found separately.
	// See CalcTDDTransSolIso, CalcTDDTransSolHorizon, CalcTDDTransSolAniso below.
	// For thermal conductive/convective gain, TDDs are treated as one big object with an effective R value.
	// The outside face temperature of the dome and the inside face temperature of the diffuser are calculated
	// with the outside and inside heat balances respectively.  The temperatures are then copied to the inside
	// face of the dome and the outside face of the diffuser.  Normal exterior and interior convection and IR
	// radiation exchange occurs for both surfaces.
	// Solar radiation that is not transmitted through the pipe is absorbed and distributed among the transition
	// zones that the pipe passes through between dome and diffuser.  The heat is distributed proportionate to
	// the length of the zone.  Any exterior length of pipe also receives a proportionate amount of heat, but
	// this is lost to the outside.
	// REFERENCES:
	// Ellis, P. G., and Strand, R. K.  Paper to be published.
	// Swift, P. D., and Smith, G. B.  "Cylindrical Mirror Light Pipes",
	//   Solar Energy Materials and Solar Cells 36 (1995), pp. 159-168.
	// DAYLIGHTING SHELVES
	// A daylighting shelf is constructed of up to three components: a window, an inside shelf, and an outside
	// shelf.  Both inside shelf and outside shelf are optional, but if neither is specified, nothing happens.
	// The window must be divided into two window surfaces: an upper window and a lower window.  The upper
	// window interacts with the daylighting shelf but the lower window does not, except to receive shading from
	// the outside shelf.  The inside shelf, if specified, acts to reflect all transmitted light from the
	// upper window onto the ceiling of the zone as diffuse light.  The outside shelf, if specified, changes
	// the total amount of light incident on the window.  All light reflected from the outside shelf also goes
	// onto the zone ceiling.
	// Most of the work for daylighting shelves is actually done in DaylightingManager.cc, SolarShading.cc,
	// and HeatBalanceSurfaceManager.cc.  The main task of the module is to get the input and initialize the
	// shelf.  The biggest part of initialization is calculating the window view factor to the outside shelf.
	// It is up to the user to reduce the window view factor to ground accordingly.
	// The inside shelf is modeled in both daylighting and heat balance simulations by converting all light
	// transmitted by the upper window into diffuse upgoing flux.  No beam or downgoing flux can pass the end
	// of the shelf regardless of the shelf's position or orientation.  Since it is defined as a partition,
	// the inside shelf is essentially the same as an internal mass surface.  The initialization doubles the
	// surface area so that both sides are exposed to the zone air.  All beam solar transmitted by the window
	// is absorbed in one side of the shelf, i.e. half of the doubled area.
	// The outside shelf is modeled in the daylighting simulation after the detailed sky/ground integration has
	// been completed.  Since exterior surfaces currently do not reflect or have a luminance in the Daylighting
	// Manager, the shelf just serves to block part of the ground luminance.  The luminance of the shelf itself
	// is added as a lump sum based on the view factor to the shelf, the sunlit fraction, the reflectance of the
	// shelf construction, and the sun and sky illuminance on the shelf.  All the luminance is added to the
	// diffuse upgoing flux.  The shelf view factor to sky is assumed to be 1.0 for lack of better information.
	// The outside shelf is treated similarly in the heat balance simulation, but here the shelf view factor to
	// sky is conveniently given by AnisoSkyMult.  NOTE:  The solar shading code was modified to allow sunlit
	// fraction, sunlit area, AnisoSkyMult, etc. to be calculated for attached shading surfaces.
	// Future shelf model improvements:
	// 1. Allow beam and downgoing flux to pass the end of the inside shelf depending on actual shelf goemetry.
	// 2. Reduce outside shelf view factor to sky (for daylighting) by taking into account anisotropic sky
	//    distribution and shading, i.e. the daylighting equivalent of AnisoSkyMult.
	// 3. Expand view factor to shelf calculation to handle more complicated geometry.
	// REFERENCES:
	// Mills, A. F.  Heat and Mass Transfer, 1995, p. 499.  (Shape factor for adjacent rectangles.)

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::NumOfZones;
	using DataGlobals::DegToRadians;
	using DataGlobals::Pi;
	using DataGlobals::PiOvr2;
	using DataGlobals::OutputFileInits;
	using DataHeatBalance::Zone;
	using DataHeatBalance::Construct;
	using DataHeatBalance::TotConstructs;
	using DataHeatBalance::SolarDistribution;
	using DataHeatBalance::MinimalShadowing;
	using DataSurfaces::Surface;
	using DataSurfaces::TotSurfaces;
	using DataSurfaces::SurfaceWindow;
	using DataSurfaces::ExternalEnvironment;
	using DataSurfaces::CalcSolRefl;
	using DataSurfaces::SurfaceClass_Window;
	using DataSurfaces::SurfaceClass_Shading;
	using DataSurfaces::SurfaceClass_TDD_Dome;
	using DataSurfaces::SurfaceClass_TDD_Diffuser;
	using DataSurfaces::ShadingTransmittanceVaries;
	using namespace DataDaylightingDevices;

	// Data
	// MODULE PARAMETER DEFINITIONS: na
	// DERIVED TYPE DEFINITIONS: na
	// MODULE VARIABLE TYPE DECLARATIONS: na

	// MODULE VARIABLE DECLARATIONS:
	Array1D< Real64 > COSAngle( NumOfAngles ); // List of cosines of incident angle

	// SUBROUTINE SPECIFICATIONS:

	// MODULE SUBROUTINES:

	// Functions

	void
	InitDaylightingDevices()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   May 2003
		//       MODIFIED       PGE, Aug 2003:  Added daylighting shelves.
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine initializes all daylighting device:  TDD pipes and daylighting shelves.
		// This is only called once at the beginning of the simulation under the BeginSimFlag.

		// METHODOLOGY EMPLOYED:
		// Daylighting and thermal variables are calculated.  BeamTrans/COSAngle table is calculated.

		// REFERENCES: na

		// Using/Aliasing
		using General::RoundSigDigits;
		using DataHeatBalance::IntGainTypeOf_DaylightingDeviceTubular;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS: na

		// DERIVED TYPE DEFINITIONS:

		static gio::Fmt fmtA( "(A)" );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PipeNum; // TDD pipe object number
		int StoredNum; // Stored TDD pipe object number
		int AngleNum;
		int TZoneNum;
		int Loop;
		Real64 Theta; // Angle of entry in degrees, 0 is parallel to pipe axis
		Real64 dTheta; // Angle increment
		Real64 Reflectance; // Visible or solar reflectance of surface
		Real64 SumTZoneLengths;
		bool Found;
		int ShelfNum; // Daylighting shelf object number
		int ShelfSurf; // Daylighting shelf surface number
		int WinSurf; // Window surface number

		static int NumStored( 0 ); // Counter for number of pipes stored as they are calculated
		static bool ShelfReported( false );

		struct TDDPipeStoredData
		{
			// Members
			Real64 AspectRatio; // Aspect ratio, length / diameter
			Real64 Reflectance; // Reflectance of surface
			Array1D< Real64 > TransBeam; // Table of beam transmittance vs. cosine angle

			// Default Constructor
			TDDPipeStoredData() :
				AspectRatio( 0.0 ),
				Reflectance( 0.0 ),
				TransBeam( NumOfAngles, 0.0 )
			{}

		};

		// Object Data
		Array1D< TDDPipeStoredData > TDDPipeStored;

		// FLOW:
		// Initialize tubular daylighting devices (TDDs)
		GetTDDInput();

		if ( NumOfTDDPipes > 0 ) {
			DisplayString( "Initializing Tubular Daylighting Devices" );
			// Setup COSAngle list for all TDDs
			COSAngle( 1 ) = 0.0;
			COSAngle( NumOfAngles ) = 1.0;

			dTheta = 90.0 * DegToRadians / ( NumOfAngles - 1.0 );
			Theta = 90.0 * DegToRadians;
			for ( AngleNum = 2; AngleNum <= NumOfAngles - 1; ++AngleNum ) {
				Theta -= dTheta;
				COSAngle( AngleNum ) = std::cos( Theta );
			} // AngleNum

			TDDPipeStored.allocate( NumOfTDDPipes * 2 );

			for ( PipeNum = 1; PipeNum <= NumOfTDDPipes; ++PipeNum ) {
				// Initialize optical properties
				TDDPipe( PipeNum ).AspectRatio = TDDPipe( PipeNum ).TotLength / TDDPipe( PipeNum ).Diameter;
				TDDPipe( PipeNum ).ReflectVis = 1.0 - Construct( TDDPipe( PipeNum ).Construction ).InsideAbsorpVis;
				TDDPipe( PipeNum ).ReflectSol = 1.0 - Construct( TDDPipe( PipeNum ).Construction ).InsideAbsorpSolar;

				// Calculate the beam transmittance table for visible and solar spectrum
				// First time thru use the visible reflectance
				Reflectance = TDDPipe( PipeNum ).ReflectVis;
				for ( Loop = 1; Loop <= 2; ++Loop ) {
					// For computational efficiency, search stored pipes to see if an identical pipe has already been calculated
					Found = false;
					for ( StoredNum = 1; StoredNum <= NumStored; ++StoredNum ) {
						if ( TDDPipeStored( StoredNum ).AspectRatio != TDDPipe( PipeNum ).AspectRatio ) continue;
						if ( TDDPipeStored( StoredNum ).Reflectance == Reflectance ) {
							Found = true; // StoredNum points to the matching TDDPipeStored
							break;
						}
					} // StoredNum

					if ( ! Found ) { // Not yet calculated

						// Add a new pipe to TDDPipeStored
						++NumStored;
						TDDPipeStored( NumStored ).AspectRatio = TDDPipe( PipeNum ).AspectRatio;
						TDDPipeStored( NumStored ).Reflectance = Reflectance;

						// Set beam transmittances for 0 and 90 degrees
						TDDPipeStored( NumStored ).TransBeam( 1 ) = 0.0;
						TDDPipeStored( NumStored ).TransBeam( NumOfAngles ) = 1.0;

						// Calculate intermediate beam transmittances between 0 and 90 degrees
						Theta = 90.0 * DegToRadians;
						for ( AngleNum = 2; AngleNum <= NumOfAngles - 1; ++AngleNum ) {
							Theta -= dTheta;
							TDDPipeStored( NumStored ).TransBeam( AngleNum ) = CalcPipeTransBeam( Reflectance, TDDPipe( PipeNum ).AspectRatio, Theta );
						} // AngleNum

						StoredNum = NumStored;
					}

					// Assign stored values to TDDPipe
					if ( Loop == 1 ) { // Visible
						TDDPipe( PipeNum ).PipeTransVisBeam = TDDPipeStored( StoredNum ).TransBeam;
					} else { // Solar
						TDDPipe( PipeNum ).PipeTransSolBeam = TDDPipeStored( StoredNum ).TransBeam;
					}

					// Second time thru use the solar reflectance
					Reflectance = TDDPipe( PipeNum ).ReflectSol;
				} // Loop

				// Calculate the solar isotropic diffuse and horizon transmittances.  These values are constant for a given TDD.
				TDDPipe( PipeNum ).TransSolIso = CalcTDDTransSolIso( PipeNum );
				TDDPipe( PipeNum ).TransSolHorizon = CalcTDDTransSolHorizon( PipeNum );

				// Initialize thermal properties
				SumTZoneLengths = 0.0;
				for ( TZoneNum = 1; TZoneNum <= TDDPipe( PipeNum ).NumOfTZones; ++TZoneNum ) {
					SumTZoneLengths += TDDPipe( PipeNum ).TZoneLength( TZoneNum );

					SetupZoneInternalGain( TDDPipe( PipeNum ).TZone( TZoneNum ), "DaylightingDevice:Tubular", TDDPipe( PipeNum ).Name, IntGainTypeOf_DaylightingDeviceTubular, TDDPipe( PipeNum ).TZoneHeatGain( TZoneNum ) );

				} // TZoneNum

				TDDPipe( PipeNum ).ExtLength = TDDPipe( PipeNum ).TotLength - SumTZoneLengths;

				// Setup report variables: CurrentModuleObject='DaylightingDevice:Tubular'
				SetupOutputVariable( "Tubular Daylighting Device Transmitted Solar Radiation Rate [W]", TDDPipe( PipeNum ).TransmittedSolar, "Zone", "Average", TDDPipe( PipeNum ).Name );
				SetupOutputVariable( "Tubular Daylighting Device Pipe Absorbed Solar Radiation Rate [W]", TDDPipe( PipeNum ).PipeAbsorbedSolar, "Zone", "Average", TDDPipe( PipeNum ).Name );
				SetupOutputVariable( "Tubular Daylighting Device Heat Gain Rate [W]", TDDPipe( PipeNum ).HeatGain, "Zone", "Average", TDDPipe( PipeNum ).Name );
				SetupOutputVariable( "Tubular Daylighting Device Heat Loss Rate [W]", TDDPipe( PipeNum ).HeatLoss, "Zone", "Average", TDDPipe( PipeNum ).Name );

				SetupOutputVariable( "Tubular Daylighting Device Beam Solar Transmittance []", TDDPipe( PipeNum ).TransSolBeam, "Zone", "Average", TDDPipe( PipeNum ).Name );
				SetupOutputVariable( "Tubular Daylighting Device Beam Visible Transmittance []", TDDPipe( PipeNum ).TransVisBeam, "Zone", "Average", TDDPipe( PipeNum ).Name );
				SetupOutputVariable( "Tubular Daylighting Device Diffuse Solar Transmittance []", TDDPipe( PipeNum ).TransSolDiff, "Zone", "Average", TDDPipe( PipeNum ).Name );
				SetupOutputVariable( "Tubular Daylighting Device Diffuse Visible Transmittance []", TDDPipe( PipeNum ).TransVisDiff, "Zone", "Average", TDDPipe( PipeNum ).Name );

			} // PipeNum

			TDDPipeStored.deallocate();
		}

		// Initialize daylighting shelves
		GetShelfInput();

		if ( NumOfShelf > 0 ) DisplayString( "Initializing Light Shelf Daylighting Devices" );

		for ( ShelfNum = 1; ShelfNum <= NumOfShelf; ++ShelfNum ) {
			WinSurf = Shelf( ShelfNum ).Window;

			ShelfSurf = Shelf( ShelfNum ).InSurf;
			if ( ShelfSurf > 0 ) {
				// Double surface area so that both sides of the shelf are treated as internal mass
				Surface( ShelfSurf ).Area *= 2.0;
			}

			ShelfSurf = Shelf( ShelfNum ).OutSurf;
			if ( ShelfSurf > 0 ) {
				Shelf( ShelfNum ).OutReflectVis = 1.0 - Construct( Shelf( ShelfNum ).Construction ).OutsideAbsorpVis;
				Shelf( ShelfNum ).OutReflectSol = 1.0 - Construct( Shelf( ShelfNum ).Construction ).OutsideAbsorpSolar;

				if ( Shelf( ShelfNum ).ViewFactor < 0 ) CalcViewFactorToShelf( ShelfNum );

				if ( Shelf( ShelfNum ).ViewFactor + Surface( WinSurf ).ViewFactorSky + Surface( WinSurf ).ViewFactorGround > 1.0 ) {
					ShowWarningError( "DaylightingDevice:Shelf = " + Shelf( ShelfNum ).Name + ":  Window view factors to sky [" + RoundSigDigits( Surface( WinSurf ).ViewFactorSky, 2 ) + "]," );
					ShowContinueError( "ground [" + RoundSigDigits( Surface( WinSurf ).ViewFactorGround, 2 ) + "], and outside shelf [" + RoundSigDigits( Shelf( ShelfNum ).ViewFactor, 2 ) + "] add up to > 1.0." );
				}

				// Report calculated view factor so that user knows what to make the view factor to ground
				if ( ! ShelfReported ) {
					gio::write( OutputFileInits, fmtA ) << "! <Shelf Details>,Name,View Factor to Outside Shelf,Window Name,Window View Factor to Sky,Window View Factor to Ground";
					ShelfReported = true;
				}
				gio::write( OutputFileInits, fmtA ) << Shelf( ShelfNum ).Name + ',' + RoundSigDigits( Shelf( ShelfNum ).ViewFactor, 2 ) + ',' + Surface( WinSurf ).Name + ',' + RoundSigDigits( Surface( WinSurf ).ViewFactorSky, 2 ) + ',' + RoundSigDigits( Surface( WinSurf ).ViewFactorGround, 2 );
				//      CALL SetupOutputVariable('View Factor To Outside Shelf []', &
				//        Shelf(ShelfNum)%ViewFactor,'Zone','Average',Shelf(ShelfNum)%Name)
			}
		}

		// Warning that if Calculate Solar Reflection From Exterior Surfaces = Yes in Building input, then
		// solar reflection calculated from obstructions will not be used in daylighting shelf or tubular device
		// calculation

		if ( CalcSolRefl && ( NumOfTDDPipes > 0 || NumOfShelf > 0 ) ) {
			ShowWarningError( "InitDaylightingDevices: Solar Distribution Model includes Solar Reflection calculations;" );
			ShowContinueError( "the resulting reflected solar values will not be used in the" );
			ShowContinueError( "DaylightingDevice:Shelf or DaylightingDevice:Tubular calculations." );
		}

	}

	void
	GetTDDInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   May 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Gets the input for TDD pipes and does some error checking.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// REFERENCES: na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::FindItemInList;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using General::RoundSigDigits;
		using General::SafeDivide;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS: na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//unused1208  CHARACTER(len=MaxNameLength), &
		//                   DIMENSION(20) :: Alphas                ! Alpha items for object
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		int IOStatus; // Used in GetObjectItem
		bool IsBlank; // TRUE if the name is blank
		bool IsNotOK; // TRUE if there was a problem with a list name
		//unused1208  REAL(r64), DIMENSION(9)             :: Numbers               ! Numeric items for object
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int PipeNum; // TDD pipe object number
		int SurfNum; // Dome or diffuser surface
		int TZoneNum; // Transition zone loop
		std::string TZoneName; // Transition zone name
		Real64 PipeArea;

		// FLOW:
		cCurrentModuleObject = "DaylightingDevice:Tubular";
		NumOfTDDPipes = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumOfTDDPipes > 0 ) {
			TDDPipe.allocate( NumOfTDDPipes );

			for ( PipeNum = 1; PipeNum <= NumOfTDDPipes; ++PipeNum ) {
				GetObjectItem( cCurrentModuleObject, PipeNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				// Pipe name
				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), TDDPipe, PipeNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
				}
				TDDPipe( PipeNum ).Name = cAlphaArgs( 1 );

				// Get TDD:DOME object
				SurfNum = FindItemInList( cAlphaArgs( 2 ), Surface );

				if ( SurfNum == 0 ) {
					ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Dome " + cAlphaArgs( 2 ) + " not found." );
					ErrorsFound = true;
				} else {
					if ( FindTDDPipe( SurfNum ) > 0 ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Dome " + cAlphaArgs( 2 ) + " is referenced by more than one TDD." );
						ErrorsFound = true;
					}

					if ( Surface( SurfNum ).Class != SurfaceClass_TDD_Dome ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Dome " + cAlphaArgs( 2 ) + " is not of surface type TubularDaylightDome." );
						ErrorsFound = true;
					}

					if ( Construct( Surface( SurfNum ).Construction ).TotGlassLayers > 1 ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Dome " + cAlphaArgs( 2 ) + " construction (" + Construct( Surface( SurfNum ).Construction ).Name + ") must have only 1 glass layer." );
						ErrorsFound = true;
					}

					if ( Surface( SurfNum ).WindowShadingControlPtr > 0 ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Dome " + cAlphaArgs( 2 ) + " must not have a shading control." );
						ErrorsFound = true;
					}

					if ( Surface( SurfNum ).FrameDivider > 0 ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Dome " + cAlphaArgs( 2 ) + " must not have a frame/divider." );
						ErrorsFound = true;
					}

					if ( Construct( Surface( SurfNum ).Construction ).WindowTypeEQL ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Dome " + cAlphaArgs( 2 ) + " Equivalent Layer Window is not supported." );
						ErrorsFound = true;
					}
					// Window multiplier is already handled in SurfaceGeometry.cc

					if ( ! Surface( SurfNum ).ExtSolar ) {
						ShowWarningError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Dome " + cAlphaArgs( 2 ) + " is not exposed to exterior radiation." );
					}

					TDDPipe( PipeNum ).Dome = SurfNum;
				}

				// Get TDD:DIFFUSER object
				SurfNum = FindItemInList( cAlphaArgs( 3 ), Surface );

				if ( SurfNum == 0 ) {
					ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Diffuser " + cAlphaArgs( 3 ) + " not found." );
					ErrorsFound = true;
				} else {
					if ( FindTDDPipe( SurfNum ) > 0 ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Diffuser " + cAlphaArgs( 3 ) + " is referenced by more than one TDD." );
						ErrorsFound = true;
					}

					if ( SurfaceWindow( SurfNum ).OriginalClass != SurfaceClass_TDD_Diffuser ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Diffuser " + cAlphaArgs( 3 ) + " is not of surface type TubularDaylightDiffuser." );
						ErrorsFound = true;
					}

					if ( Construct( Surface( SurfNum ).Construction ).TotGlassLayers > 1 ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Diffuser " + cAlphaArgs( 3 ) + " construction (" + Construct( Surface( SurfNum ).Construction ).Name + ") must have only 1 glass layer." );
						ErrorsFound = true;
					}

					if ( Construct( Surface( SurfNum ).Construction ).TransDiff <= 1.0e-10 ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Diffuser " + cAlphaArgs( 3 ) + " construction (" + Construct( Surface( SurfNum ).Construction ).Name + ") invalid value." );
						ShowContinueError( "Diffuse solar transmittance of construction [" + RoundSigDigits( Construct( Surface( SurfNum ).Construction ).TransDiff, 4 ) + "] too small for calculations." );
						ErrorsFound = true;
					}

					if ( TDDPipe( PipeNum ).Dome > 0 && std::abs( Surface( SurfNum ).Area - Surface( TDDPipe( PipeNum ).Dome ).Area ) > 0.1 ) {
						if ( SafeDivide( std::abs( Surface( SurfNum ).Area - Surface( TDDPipe( PipeNum ).Dome ).Area ), Surface( TDDPipe( PipeNum ).Dome ).Area ) > 0.1 ) { // greater than 10%
							ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Dome and diffuser areas are significantly different (>10%)." );
							ShowContinueError( "...Diffuser Area=[" + RoundSigDigits( Surface( SurfNum ).Area, 4 ) + "]; Dome Area=[" + RoundSigDigits( Surface( TDDPipe( PipeNum ).Dome ).Area, 4 ) + "]." );
							ErrorsFound = true;
						} else {
							ShowWarningError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Dome and diffuser areas differ by > .1 m2." );
							ShowContinueError( "...Diffuser Area=[" + RoundSigDigits( Surface( SurfNum ).Area, 4 ) + "]; Dome Area=[" + RoundSigDigits( Surface( TDDPipe( PipeNum ).Dome ).Area, 4 ) + "]." );
						}
					}

					if ( Surface( SurfNum ).WindowShadingControlPtr > 0 ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Diffuser " + cAlphaArgs( 3 ) + " must not have a shading control." );
						ErrorsFound = true;
					}

					if ( Surface( SurfNum ).FrameDivider > 0 ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Diffuser " + cAlphaArgs( 3 ) + " must not have a frame/divider." );
						ErrorsFound = true;
					}

					if ( Construct( Surface( SurfNum ).Construction ).WindowTypeEQL ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Diffuser " + cAlphaArgs( 2 ) + " Equivalent Layer Window is not supported." );
						ErrorsFound = true;
					}

					// Window multiplier is already handled in SurfaceGeometry.cc

					TDDPipe( PipeNum ).Diffuser = SurfNum;
				}

				// Construction
				TDDPipe( PipeNum ).Construction = FindItemInList( cAlphaArgs( 4 ), Construct );

				if ( TDDPipe( PipeNum ).Construction == 0 ) {
					ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Pipe construction " + cAlphaArgs( 4 ) + " not found." );
					ErrorsFound = true;
				} else {
					Construct( TDDPipe( PipeNum ).Construction ).IsUsed = true;
				}

				if ( rNumericArgs( 1 ) > 0 ) {
					TDDPipe( PipeNum ).Diameter = rNumericArgs( 1 );
				} else {
					ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Pipe diameter must be greater than zero." );
					ErrorsFound = true;
				}

				PipeArea = 0.25 * Pi * pow_2( TDDPipe( PipeNum ).Diameter );
				if ( TDDPipe( PipeNum ).Dome > 0 && std::abs( PipeArea - Surface( TDDPipe( PipeNum ).Dome ).Area ) > 0.1 ) {
					if ( SafeDivide( std::abs( PipeArea - Surface( TDDPipe( PipeNum ).Dome ).Area ), Surface( TDDPipe( PipeNum ).Dome ).Area ) > 0.1 ) { // greater than 10%
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Pipe and dome/diffuser areas are significantly different (>10%)." );
						ShowContinueError( "...Pipe Area=[" + RoundSigDigits( PipeArea, 4 ) + "]; Dome/Diffuser Area=[" + RoundSigDigits( Surface( TDDPipe( PipeNum ).Dome ).Area, 4 ) + "]." );
						ErrorsFound = true;
					} else {
						ShowWarningError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Pipe and dome/diffuser areas differ by > .1 m2." );
						ShowContinueError( "...Pipe Area=[" + RoundSigDigits( PipeArea, 4 ) + "]; Dome/Diffuser Area=[" + RoundSigDigits( Surface( TDDPipe( PipeNum ).Dome ).Area, 4 ) + "]." );
					}
				}

				if ( rNumericArgs( 2 ) > 0 ) {
					TDDPipe( PipeNum ).TotLength = rNumericArgs( 2 );
				} else {
					ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Pipe length must be greater than zero." );
					ErrorsFound = true;
				}

				if ( rNumericArgs( 3 ) > 0 ) {
					TDDPipe( PipeNum ).Reff = rNumericArgs( 3 );
				} else {
					ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Effective thermal resistance (R value) must be greater than zero." );
					ErrorsFound = true;
				}

				// Transition zones
				TDDPipe( PipeNum ).NumOfTZones = NumAlphas - 4;

				if ( TDDPipe( PipeNum ).NumOfTZones < 1 ) {
					ShowWarningError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  No transition zones specified.  All pipe absorbed solar goes to exterior." );
				} else if ( TDDPipe( PipeNum ).NumOfTZones > MaxTZones ) {
					ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Maximum number of transition zones exceeded." );
					ErrorsFound = true;
				} else {
					TDDPipe( PipeNum ).TZone.allocate( TDDPipe( PipeNum ).NumOfTZones );
					TDDPipe( PipeNum ).TZoneLength.allocate( TDDPipe( PipeNum ).NumOfTZones );
					TDDPipe( PipeNum ).TZoneHeatGain.allocate( TDDPipe( PipeNum ).NumOfTZones );

					TDDPipe( PipeNum ).TZone = 0;
					TDDPipe( PipeNum ).TZoneLength = 0.0;
					TDDPipe( PipeNum ).TZoneHeatGain = 0.0;

					for ( TZoneNum = 1; TZoneNum <= TDDPipe( PipeNum ).NumOfTZones; ++TZoneNum ) {
						TZoneName = cAlphaArgs( TZoneNum + 4 );
						TDDPipe( PipeNum ).TZone( TZoneNum ) = FindItemInList( TZoneName, Zone );
						if ( TDDPipe( PipeNum ).TZone( TZoneNum ) == 0 ) {
							ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Transition zone " + TZoneName + " not found." );
							ErrorsFound = true;
						}

						TDDPipe( PipeNum ).TZoneLength( TZoneNum ) = rNumericArgs( TZoneNum + 3 );
						if ( TDDPipe( PipeNum ).TZoneLength( TZoneNum ) < 0 ) {
							ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Transition zone length for " + TZoneName + " must be zero or greater." );
							ErrorsFound = true;
						}
					} // TZoneNum
				}

			} // PipeNum

			if ( ErrorsFound ) ShowFatalError( "Errors in DaylightingDevice:Tubular input." );
		}

	}

	void
	GetShelfInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   August 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Gets the input for light shelves and does some error checking.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// REFERENCES: na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::FindItemInList;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using namespace DataIPShortCuts;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS: na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		int IOStatus; // Used in GetObjectItem
		bool IsBlank; // TRUE if the name is blank
		bool IsNotOK; // TRUE if there was a problem with a list name
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int ShelfNum; // Daylighting shelf object number
		int SurfNum; // Window, inside, or outside shelf surfaces
		int ConstrNum; // Outside shelf construction object number

		// FLOW:
		cCurrentModuleObject = "DaylightingDevice:Shelf";
		NumOfShelf = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumOfShelf > 0 ) {
			Shelf.allocate( NumOfShelf );

			for ( ShelfNum = 1; ShelfNum <= NumOfShelf; ++ShelfNum ) {
				GetObjectItem( "DaylightingDevice:Shelf", ShelfNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				// Shelf name
				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), Shelf, ShelfNum - 1, IsNotOK, IsBlank, "DaylightingDevice:Shelf" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
				}
				Shelf( ShelfNum ).Name = cAlphaArgs( 1 );

				// Get window object
				SurfNum = FindItemInList( cAlphaArgs( 2 ), Surface );

				if ( SurfNum == 0 ) {
					ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Window " + cAlphaArgs( 2 ) + " not found." );
					ErrorsFound = true;
				} else {
					if ( Surface( SurfNum ).Class != SurfaceClass_Window ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Window " + cAlphaArgs( 2 ) + " is not of surface type WINDOW." );
						ErrorsFound = true;
					}

					if ( Surface( SurfNum ).Shelf > 0 ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Window " + cAlphaArgs( 2 ) + " is referenced by more than one shelf." );
						ErrorsFound = true;
					}

					if ( Surface( SurfNum ).WindowShadingControlPtr > 0 ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Window " + cAlphaArgs( 2 ) + " must not have a shading control." );
						ErrorsFound = true;
					}

					if ( Surface( SurfNum ).FrameDivider > 0 ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Window " + cAlphaArgs( 2 ) + " must not have a frame/divider." );
						ErrorsFound = true;
					}

					if ( Surface( SurfNum ).Sides != 4 ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Window " + cAlphaArgs( 2 ) + " must have 4 sides." );
						ErrorsFound = true;
					}
					if ( Construct( Surface( SurfNum ).Construction ).WindowTypeEQL ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Window " + cAlphaArgs( 2 ) + " Equivalent Layer Window is not supported." );
						ErrorsFound = true;
					}

					Shelf( ShelfNum ).Window = SurfNum;
					Surface( SurfNum ).Shelf = ShelfNum;
				}

				// Get inside shelf heat transfer surface (optional)
				if ( cAlphaArgs( 3 ) != "" ) {
					SurfNum = FindItemInList( cAlphaArgs( 3 ), Surface );

					if ( SurfNum == 0 ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Inside shelf " + cAlphaArgs( 3 ) + " not found." );
						ErrorsFound = true;
					} else {
						// No error if shelf belongs to more than one window, e.g. concave corners

						if ( Surface( SurfNum ).ExtBoundCond != SurfNum ) {
							ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Inside shelf " + cAlphaArgs( 3 ) + " must be its own Outside Boundary Condition Object." );
							ErrorsFound = true;
						}

						if ( Surface( SurfNum ).Sides != 4 ) {
							ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Inside shelf " + cAlphaArgs( 3 ) + " must have 4 sides." );
							ErrorsFound = true;
						}

						Shelf( ShelfNum ).InSurf = SurfNum;
					}
				}

				// Get outside shelf attached shading surface (optional)
				if ( cAlphaArgs( 4 ) != "" ) {
					SurfNum = FindItemInList( cAlphaArgs( 4 ), Surface );

					if ( SurfNum == 0 ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Outside shelf " + cAlphaArgs( 4 ) + " not found." );
						ErrorsFound = true;
					} else {
						// No error if shelf belongs to more than one window, e.g. concave corners

						if ( Surface( SurfNum ).Class != SurfaceClass_Shading ) {
							ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Outside shelf " + cAlphaArgs( 4 ) + " is not a Shading:Zone:Detailed object." );
							ErrorsFound = true;
						}

						if ( Surface( SurfNum ).SchedShadowSurfIndex > 0 ) {
							ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Outside shelf " + cAlphaArgs( 4 ) + " must not have a transmittance schedule." );
							ErrorsFound = true;
						}

						if ( Surface( SurfNum ).Sides != 4 ) {
							ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Outside shelf " + cAlphaArgs( 4 ) + " must have 4 sides." );
							ErrorsFound = true;
						}

						// Get outside shelf construction (required if outside shelf is specified)
						if ( cAlphaArgs( 5 ) != "" ) {
							ConstrNum = FindItemInList( cAlphaArgs( 5 ), Construct );

							if ( ConstrNum == 0 ) {
								ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Outside shelf construction " + cAlphaArgs( 5 ) + " not found." );
								ErrorsFound = true;
							} else if ( Construct( ConstrNum ).TypeIsWindow ) {
								ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Outside shelf construction " + cAlphaArgs( 5 ) + " must not have WindowMaterial:Glazing." );
								ErrorsFound = true;
							} else {
								Shelf( ShelfNum ).Construction = ConstrNum;
								Construct( ConstrNum ).IsUsed = true;
							}
						} else {
							ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Outside shelf requires an outside shelf construction to be specified." );
							ErrorsFound = true;
						}

						// Get view factor to outside shelf (optional)
						if ( NumNumbers > 0 ) {
							Shelf( ShelfNum ).ViewFactor = rNumericArgs( 1 );

							if ( rNumericArgs( 1 ) == 0.0 ) {
								ShowWarningError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  View factor to outside shelf is zero.  Shelf does not reflect on window." );
							}
						} else {
							Shelf( ShelfNum ).ViewFactor = -1.0; // Flag to have the view factor calculated during initialization
						}

						Shelf( ShelfNum ).OutSurf = SurfNum;

						// Reset some properties of the SURFACE:SHADING:ATTACHED object in order to receive radiation and shading
						// Normally this would be done during initialization, but that's not early enough for some shading calculations
						Surface( SurfNum ).BaseSurf = SurfNum;
						Surface( SurfNum ).HeatTransSurf = true;
						Surface( SurfNum ).Construction = ConstrNum; // Kludge to allow shading surface to be a heat transfer surface
						Construct( ConstrNum ).IsUsed = true;
					}
				}

				if ( Shelf( ShelfNum ).InSurf == 0 && Shelf( ShelfNum ).OutSurf == 0 ) ShowWarningError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  No inside shelf or outside shelf was specified." );

			} // ShelfNum

			if ( ErrorsFound ) ShowFatalError( "Errors in DaylightingDevice:Shelf input." );
		}

	}

	Real64
	CalcPipeTransBeam(
		Real64 const R, // Reflectance of surface, constant (can be made R = f(theta) later)
		Real64 const A, // Aspect ratio, L / d
		Real64 const Theta // Angle of entry in radians
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   May 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the numerical integral for the transmittance of a reflective cylinder with
		// incident collimated beam radiation as described in Swift and Smith.

		// METHODOLOGY EMPLOYED:
		// Since this integral can be slow, a table of values is calculated and stored during
		// initialization of the TDD.  Intermediate values are calculated by interpolation.
		// Transmittance of sky and ground diffuse radiation is done by other functions.

		// REFERENCES:
		// Swift, P. D., and Smith, G. B.  "Cylindrical Mirror Light Pipes",
		//   Solar Energy Materials and Solar Cells 36 (1995), pp. 159-168.

		// OTHER NOTES:
		// The execution time of this function can be reduced by adjusting parameters N and xTol below.
		// However, there is some penalty in accuracy for N < 100,000 and xTol > 150.

		// USE STATEMENTS: na

		// Return value
		Real64 CalcPipeTransBeam;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		Real64 const N( 100000.0 ); // Number of integration points
		Real64 const xTol( 150.0 ); // Tolerance factor to skip iterations where dT is approximately 0
		// Must be >= 1.0, increase this number to decrease the execution time
		Real64 const myLocalTiny( tiny( 1.0 ) );

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 i; // Integration interval between points
		Real64 s; // Entry point
		Real64 dT;
		Real64 T; // Beam transmittance for collimated solar real
		Real64 x; // Intermediate variables for speed optimization
		Real64 c1;
		Real64 c2;
		Real64 xLimit; // Limiting x value to prevent floating point underflow

		// FLOW:
		CalcPipeTransBeam = 0.0;

		T = 0.0;
		i = 1.0 / N;

		xLimit = ( std::log( pow_2( N ) * myLocalTiny ) / std::log( R ) ) / xTol;

		c1 = A * std::tan( Theta );
		c2 = 4.0 / Pi;

		s = i;
		while ( s < ( 1.0 - i ) ) {
			x = c1 / s;

			if ( x < xLimit ) {
				dT = c2 * std::pow( R, int( x ) ) * ( 1.0 - ( 1.0 - R ) * ( x - int( x ) ) ) * pow_2( s ) / std::sqrt( 1.0 - pow_2( s ) );
				T += dT;
			}

			s += i;
		}

		T /= ( N - 1.0 ); // - 1.0, because started on i, not 0

		CalcPipeTransBeam = T;

		return CalcPipeTransBeam;

	}

	Real64
	CalcTDDTransSolIso( int const PipeNum ) // TDD pipe object number
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   July 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the transmittance of sky isotropic radiation for use with the anisotropic sky transmittance.
		// This value is also used for all ground reflected solar radiation (which is isotropic).

		// METHODOLOGY EMPLOYED:
		// The transmittance is calculated and stored once at initialization because the value is a constant.
		// The routine numerically integrates over the entire sky.  All radiation is isotropic, but incident
		// angle varies over the hemisphere.
		// Trans = Flux Transmitted / Flux Incident
		// Not sure if shading and tilt is adequately accounted for by DifShdgRatioIsoSky later on or not...

		// REFERENCES:
		// See AnisoSkyViewFactors in SolarShading.cc.

		// USE STATEMENTS: na

		// Return value
		Real64 CalcTDDTransSolIso;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		int const NPH( 1000 ); // Number of altitude integration points

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 FluxInc; // Incident solar flux
		Real64 FluxTrans; // Transmitted solar flux
		Real64 trans; // Total beam solar transmittance of TDD
		int N; // Loop counter
		Real64 PH; // Altitude angle of sky element
		Real64 dPH; // Altitude angle increment
		Real64 COSI; // Cosine of incident angle
		Real64 SINI; // Sine of incident angle
		Real64 P; // Angular distribution function

		// FLOW:
		FluxInc = 0.0;
		FluxTrans = 0.0;

		// Integrate from 0 to Pi/2 altitude
		dPH = 90.0 * DegToRadians / NPH;
		PH = 0.5 * dPH;
		for ( N = 1; N <= NPH; ++N ) {
			COSI = std::cos( PiOvr2 - PH );
			SINI = std::sin( PiOvr2 - PH );

			P = COSI; // Angular distribution function: P = COS(Incident Angle) for diffuse isotropic

			// Calculate total TDD transmittance for given angle
			trans = TransTDD( PipeNum, COSI, SolarBeam );

			FluxInc += P * SINI * dPH;
			FluxTrans += trans * P * SINI * dPH;

			PH += dPH; // Increment the altitude angle
		} // N

		CalcTDDTransSolIso = FluxTrans / FluxInc;

		return CalcTDDTransSolIso;

	}

	Real64
	CalcTDDTransSolHorizon( int const PipeNum ) // TDD pipe object number
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   July 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the transmittance of sky horizon radiation for use with the anisotropic sky transmittance.

		// METHODOLOGY EMPLOYED:
		// The transmittance is calculated and stored once at initialization because the value is a constant.
		// The routine numerically integrates over the horizon as an infinitesimally narrow strip of the sky.
		// Horizon radiation is isotropic, but incident angle varies over the semicircle.
		// Trans = Flux Transmitted / Flux Incident
		// Not sure if shading is adequately accounted for by DifShdgRatioHoriz later on or not...

		// REFERENCES:
		// See AnisoSkyViewFactors in SolarShading.cc.

		// Using/Aliasing
		using namespace DataSurfaces;

		// Return value
		Real64 CalcTDDTransSolHorizon;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		int const NTH( 18 ); // Number of azimuth integration points

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 FluxInc; // Incident solar flux
		Real64 FluxTrans; // Transmitted solar flux
		Real64 trans; // Total beam solar transmittance of TDD
		int N; // Loop counter
		Real64 TH; // Azimuth angle of sky horizon element
		Real64 dTH; // Azimuth angle increment
		Real64 THMIN; // Minimum azimuth integration limit
		Real64 THMAX; // Maximum azimuth integration limit
		Real64 CosPhi; // Cosine of TDD:DOME altitude angle
		Real64 Theta; // TDD:DOME azimuth angle
		Real64 COSI; // Cosine of the incident angle

		// FLOW:
		FluxInc = 0.0;
		FluxTrans = 0.0;

		CosPhi = std::cos( PiOvr2 - Surface( TDDPipe( PipeNum ).Dome ).Tilt * DegToRadians );
		Theta = Surface( TDDPipe( PipeNum ).Dome ).Azimuth * DegToRadians;

		if ( CosPhi > 0.01 ) { // Dome has a view of the horizon
			// Integrate over the semicircle
			THMIN = Theta - PiOvr2;
			THMAX = Theta + PiOvr2;
			dTH = 180.0 * DegToRadians / NTH;
			TH = THMIN + 0.5 * dTH;
			for ( N = 1; N <= NTH; ++N ) {
				// Calculate incident angle between dome outward normal and horizon element
				COSI = CosPhi * std::cos( TH - Theta );

				// Calculate total TDD transmittance for given angle
				trans = TransTDD( PipeNum, COSI, SolarBeam );

				FluxInc += COSI * dTH;
				FluxTrans += trans * COSI * dTH;

				TH += dTH; // Increment the azimuth angle
			} // N

			CalcTDDTransSolHorizon = FluxTrans / FluxInc;

		} else { // Dome is nearly horizontal and has almost no view of the horizon
			CalcTDDTransSolHorizon = 0.0; // = TransTDD(PipeNum, ???, SolarBeam) ! Could change to an angle near the horizon
		}

		return CalcTDDTransSolHorizon;

	}

	Real64
	CalcTDDTransSolAniso(
		int const PipeNum, // TDD pipe object number
		Real64 const COSI // Cosine of the incident angle
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   July 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the transmittance of the anisotropic sky.

		// METHODOLOGY EMPLOYED:
		// Similar to the Trans = FluxTrans/FluxInc integrations above, the anisotropic sky can be decomposed
		// and have a different transmittance applied to each component.
		//   FluxInc = IsoSkyRad + CircumSolarRad + HorizonRad
		//   FluxTrans = T1*IsoSkyRad + T2*CircumSolarRad + T3*HorizonRad
		// It turns out that FluxTrans/FluxInc is equivalent to AnisoSkyTDDMult/AnisoSkyMult.
		// AnisoSkyMult has been conveniently calculated already in AnisoSkyViewFactors in SolarShading.cc.
		// AnisoSkyMult = MultIsoSky*DifShdgRatioIsoSky + MultCircumSolar*SunlitFrac + MultHorizonZenith*DifShdgRatioHoriz
		// In this routine a similar AnisoSkyTDDMult is calculated that applies the appropriate transmittance to each
		// of the components above.  The result is Trans = AnisoSkyTDDMult/AnisoSkyMult.
		// Shading and orientation are already taken care of by DifShdgRatioIsoSky and DifShdgRatioHoriz.

		// REFERENCES:
		// See AnisoSkyViewFactors in SolarShading.cc.

		// USE STATEMENTS: na
		// Using/Aliasing
		using DataGlobals::HourOfDay;
		using DataGlobals::TimeStep;
		using DataHeatBalance::SunlitFrac;
		using DataHeatBalance::AnisoSkyMult;
		using DataHeatBalance::DifShdgRatioIsoSky;
		using DataHeatBalance::DifShdgRatioHoriz;
		using DataHeatBalance::MultIsoSky;
		using DataHeatBalance::MultCircumSolar;
		using DataHeatBalance::MultHorizonZenith;
		using DataHeatBalance::curDifShdgRatioIsoSky;
		using DataHeatBalance::DifShdgRatioHorizHRTS;
		using DataSystemVariables::DetailedSkyDiffuseAlgorithm;

		// Return value
		Real64 CalcTDDTransSolAniso;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int DomeSurf; // TDD:DOME surface number
		Real64 IsoSkyRad; // Isotropic sky radiation component
		Real64 CircumSolarRad; // Circumsolar sky radiation component
		Real64 HorizonRad; // Horizon sky radiation component
		Real64 AnisoSkyTDDMult; // Anisotropic sky multiplier for TDD

		// FLOW:
		DomeSurf = TDDPipe( PipeNum ).Dome;

		if ( ! DetailedSkyDiffuseAlgorithm || ! ShadingTransmittanceVaries || SolarDistribution == MinimalShadowing ) {
			IsoSkyRad = MultIsoSky( DomeSurf ) * DifShdgRatioIsoSky( DomeSurf );
			HorizonRad = MultHorizonZenith( DomeSurf ) * DifShdgRatioHoriz( DomeSurf );
		} else {
			IsoSkyRad = MultIsoSky( DomeSurf ) * curDifShdgRatioIsoSky( DomeSurf );
			HorizonRad = MultHorizonZenith( DomeSurf ) * DifShdgRatioHorizHRTS( TimeStep, HourOfDay, DomeSurf );
		}
		CircumSolarRad = MultCircumSolar( DomeSurf ) * SunlitFrac( TimeStep, HourOfDay, DomeSurf );

		AnisoSkyTDDMult = TDDPipe( PipeNum ).TransSolIso * IsoSkyRad + TransTDD( PipeNum, COSI, SolarBeam ) * CircumSolarRad + TDDPipe( PipeNum ).TransSolHorizon * HorizonRad;

		if ( AnisoSkyMult( DomeSurf ) > 0.0 ) {
			CalcTDDTransSolAniso = AnisoSkyTDDMult / AnisoSkyMult( DomeSurf );
		} else {
			CalcTDDTransSolAniso = 0.0;
		}

		return CalcTDDTransSolAniso;

	}

	Real64
	TransTDD(
		int const PipeNum, // TDD pipe object number
		Real64 const COSI, // Cosine of the incident angle
		int const RadiationType // Radiation type flag
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   May 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the total transmittance of the TDD for specified radiation type.

		// METHODOLOGY EMPLOYED:
		// The transmittances for each component (i.e. TDD:DIFFUSER, TDD:DOME, and pipe) are calculated.
		// All transmittances are multiplied to get the total for the TDD:
		//   TransTDD = transDome * transPipe * transDiff
		// Transmittance of beam radiation is calculated by interpolating the values in a
		// table created during initialization.  The table values are from Swift and Smith's
		// numerical integral for collimated beam radiation.
		// Transmittances of isotropic and anisotropic diffuse radiation are more complicated and call
		// other subroutines in this module.
		// All light reaching the TDD:DIFFUSER is assumed to be diffuse.
		// NOTE: Dome transmittance could be improved by taking into account curvature of the dome.

		// REFERENCES:
		// Swift, P. D., and Smith, G. B.  "Cylindrical Mirror Light Pipes",
		//   Solar Energy Materials and Solar Cells 36 (1995), pp. 159-168.

		// Using/Aliasing
		using General::POLYF;

		// Return value
		Real64 TransTDD;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int constDome; // Construction object number for TDD:DOME
		int constDiff; // Construction object number for TDD:DIFFUSER
		Real64 transDome;
		Real64 transPipe;
		Real64 transDiff;

		// FLOW:
		TransTDD = 0.0;

		// Get constructions of each TDD component
		constDome = Surface( TDDPipe( PipeNum ).Dome ).Construction;
		constDiff = Surface( TDDPipe( PipeNum ).Diffuser ).Construction;

		// Get the transmittance of each component and of total TDD
		{ auto const SELECT_CASE_var( RadiationType );

		if ( SELECT_CASE_var == VisibleBeam ) {
			transDome = POLYF( COSI, Construct( constDome ).TransVisBeamCoef );
			transPipe = InterpolatePipeTransBeam( COSI, TDDPipe( PipeNum ).PipeTransVisBeam );
			transDiff = Construct( constDiff ).TransDiffVis; // May want to change to POLYF also!

			TransTDD = transDome * transPipe * transDiff;

		} else if ( SELECT_CASE_var == SolarBeam ) {
			transDome = POLYF( COSI, Construct( constDome ).TransSolBeamCoef );
			transPipe = InterpolatePipeTransBeam( COSI, TDDPipe( PipeNum ).PipeTransSolBeam );
			transDiff = Construct( constDiff ).TransDiff; // May want to change to POLYF also!

			TransTDD = transDome * transPipe * transDiff;

		} else if ( SELECT_CASE_var == SolarAniso ) {
			TransTDD = CalcTDDTransSolAniso( PipeNum, COSI );

		} else if ( SELECT_CASE_var == SolarIso ) {
			TransTDD = TDDPipe( PipeNum ).TransSolIso;

		}}

		return TransTDD;

	}

	Real64
	InterpolatePipeTransBeam(
		Real64 const COSI, // Cosine of the incident angle
		Array1A< Real64 > const transBeam // Table of beam transmittance vs. cosine angle
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   July 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Interpolates the beam transmittance vs. cosine angle table.

		// METHODOLOGY EMPLOYED: na
		// REFERENCES: na

		// Using/Aliasing
		using FluidProperties::FindArrayIndex; // USEd code could be copied here to eliminate dependence on FluidProperties

		// Return value
		Real64 InterpolatePipeTransBeam;

		// Argument array dimensioning
		transBeam.dim( NumOfAngles );

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int Lo;
		int Hi;
		Real64 m;
		Real64 b;

		// FLOW:
		InterpolatePipeTransBeam = 0.0;

		// Linearly interpolate transBeam/COSAngle table to get value at current cosine of the angle
		Lo = FindArrayIndex( COSI, COSAngle );
		Hi = Lo + 1;

		if ( Lo > 0 && Hi <= NumOfAngles ) {
			m = ( transBeam( Hi ) - transBeam( Lo ) ) / ( COSAngle( Hi ) - COSAngle( Lo ) );
			b = transBeam( Lo ) - m * COSAngle( Lo );

			InterpolatePipeTransBeam = m * COSI + b;
		} else {
			InterpolatePipeTransBeam = 0.0;
		}

		return InterpolatePipeTransBeam;

	}

	int
	FindTDDPipe( int const WinNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   May 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Given the TDD:DOME or TDD:DIFFUSER object number, returns TDD pipe number.

		// METHODOLOGY EMPLOYED:
		// Similar to FindItemInList defined in InputProcessor.

		// REFERENCES: na
		// Using/Aliasing
		using DataSurfaces::Surface;

		// Return value
		int FindTDDPipe;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int PipeNum; // TDD pipe object number

		// FLOW:
		FindTDDPipe = 0;

		if ( NumOfTDDPipes <= 0 ) {
			ShowFatalError( "FindTDDPipe: Surface=" + Surface( WinNum ).Name + ", TDD:Dome object does not reference a valid Diffuser object....needs DaylightingDevice:Tubular of same name as Surface." );
		}

		for ( PipeNum = 1; PipeNum <= NumOfTDDPipes; ++PipeNum ) {
			if ( ( WinNum == TDDPipe( PipeNum ).Dome ) || ( WinNum == TDDPipe( PipeNum ).Diffuser ) ) {
				FindTDDPipe = PipeNum;
				break;
			}
		} // PipeNum

		return FindTDDPipe;

	}

	void
	DistributeTDDAbsorbedSolar()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   July 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Sums the absorbed solar gains from TDD pipes that pass through transition zones.

		// METHODOLOGY EMPLOYED:
		// The total absorbed solar gain is a sum of the following gains:
		//   1. Inward bound solar absorbed by multiple pipe reflections (solar entering pipe - solar exiting pipe)
		//   2. Outward bound solar absorbed by multiple pipe reflections due to:
		//     a. Reflection off of diffuser surface (inside of TDD)
		//     b. Zone diffuse interior shortwave incident on the diffuser from windows, lights, etc.
		//   3. Inward absorbed solar in dome and diffuser glass
		// This subroutine is called by InitIntSolarDistribution in HeatBalanceSurfaceManager.cc.

		// REFERENCES: na

		// Using/Aliasing
		using DataHeatBalance::QRadSWOutIncident;
		using DataHeatBalance::QRadSWwinAbs;
		using DataHeatBalance::QRadSWwinAbsTot;
		using DataHeatBalance::QS;
		using DataSurfaces::WinTransSolar;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS: na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PipeNum; // TDD pipe object number
		int DiffSurf; // Surface number of TDD:DIFFUSER
		int TZoneNum; // Transition zone index
		//  INTEGER :: ActualZone     ! Actual transition zone number
		Real64 transDiff; // Diffuse transmittance of TDD:DIFFUSER
		Real64 QRefl; // Diffuse radiation reflected back up the pipe
		Real64 TotTDDPipeGain; // Total absorbed solar gain in the tubular daylighting device pipe

		// FLOW:
		for ( PipeNum = 1; PipeNum <= NumOfTDDPipes; ++PipeNum ) {
			DiffSurf = TDDPipe( PipeNum ).Diffuser;
			transDiff = Construct( Surface( DiffSurf ).Construction ).TransDiff;

			// Calculate diffuse solar reflected back up the pipe by the inside surface of the TDD:DIFFUSER
			// All solar arriving at the diffuser is assumed to be isotropically diffuse by this point
			QRefl = ( QRadSWOutIncident( DiffSurf ) - QRadSWwinAbsTot( DiffSurf ) ) * Surface( DiffSurf ).Area - WinTransSolar( DiffSurf );

			// Add diffuse interior shortwave reflected from zone surfaces and from zone sources, lights, etc.
			QRefl += QS( Surface( DiffSurf ).Zone ) * Surface( DiffSurf ).Area * transDiff;

			TotTDDPipeGain = WinTransSolar( TDDPipe( PipeNum ).Dome ) - QRadSWOutIncident( DiffSurf ) * Surface( DiffSurf ).Area + QRefl * ( 1.0 - TDDPipe( PipeNum ).TransSolIso / transDiff ) + QRadSWwinAbs( 1, TDDPipe( PipeNum ).Dome ) * Surface( DiffSurf ).Area / 2.0 + QRadSWwinAbs( 1, DiffSurf ) * Surface( DiffSurf ).Area / 2.0; // Solar entering pipe | Solar exiting pipe | Absorbed due to reflections on the way out | Inward absorbed solar from dome glass | Inward absorbed solar from diffuser glass

			TDDPipe( PipeNum ).PipeAbsorbedSolar = max( 0.0, TotTDDPipeGain ); // Report variable [W]

			for ( TZoneNum = 1; TZoneNum <= TDDPipe( PipeNum ).NumOfTZones; ++TZoneNum ) {
				// Distribute absorbed solar gain in proportion to transition zone length
				TDDPipe( PipeNum ).TZoneHeatGain( TZoneNum ) = TotTDDPipeGain * ( TDDPipe( PipeNum ).TZoneLength( TZoneNum ) / TDDPipe( PipeNum ).TotLength );
			} // TZoneNum
		}

	}

	void
	CalcViewFactorToShelf( int const ShelfNum ) // Daylighting shelf object number
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   August 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Attempts to calculate exact analytical view factor from window to outside shelf.

		// METHODOLOGY EMPLOYED:
		// Uses a standard analytical solution.  It is required that window and shelf have the same width, i.e.
		// one edge (or two vertices) shared in common.  An error or warning is issued if not true.
		// A more general routine should be implemented at some point to solve for more complicated geometries.
		// Until then, the user has the option to specify their own solution for the view factor in the input object.

		// REFERENCES:
		// Mills, A. F.  Heat and Mass Transfer, 1995, p. 499.  (Shape factor for adjacent rectangles.)

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 W; // Width, height, and length of window/shelf geometry
		Real64 H;
		Real64 L;
		Real64 M; // Intermediate variables
		Real64 N;
		Real64 E1; // Intermediate equations
		Real64 E2;
		Real64 E3;
		Real64 E4;
		int VWin; // Vertex indices
		int VShelf;
		int NumMatch; // Number of vertices matched

		// FLOW:
		W = Surface( Shelf( ShelfNum ).Window ).Width;
		H = Surface( Shelf( ShelfNum ).Window ).Height;

		// Find length, i.e. projection, of outside shelf
		if ( Surface( Shelf( ShelfNum ).OutSurf ).Width == W ) {
			L = Surface( Shelf( ShelfNum ).OutSurf ).Height;
		} else if ( Surface( Shelf( ShelfNum ).OutSurf ).Height == W ) {
			L = Surface( Shelf( ShelfNum ).OutSurf ).Width;
		} else {
			ShowFatalError( "DaylightingDevice:Shelf = " + Shelf( ShelfNum ).Name + ":  Width of window and outside shelf do not match." );
		}

		// Error if more or less than two vertices match
		NumMatch = 0;
		for ( VWin = 1; VWin <= 4; ++VWin ) {
			for ( VShelf = 1; VShelf <= 4; ++VShelf ) {
				if ( distance( Surface( Shelf( ShelfNum ).Window ).Vertex( VWin ), Surface( Shelf( ShelfNum ).OutSurf ).Vertex( VShelf ) ) == 0.0 ) ++NumMatch;
			}
		}

		if ( NumMatch < 2 ) {
			ShowWarningError( "DaylightingDevice:Shelf = " + Shelf( ShelfNum ).Name + ":  Window and outside shelf must share two vertices.  View factor calculation may be inaccurate." );
		} else if ( NumMatch > 2 ) {
			ShowFatalError( "DaylightingDevice:Shelf = " + Shelf( ShelfNum ).Name + ":  Window and outside shelf share too many vertices." );
		}

		// Calculate exact analytical view factor from window to outside shelf
		M = H / W;
		N = L / W;

		E1 = M * std::atan( 1.0 / M ) + N * std::atan( 1.0 / N ) - std::sqrt( pow_2( N ) + pow_2( M ) ) * std::atan( std::pow( pow_2( N ) + pow_2( M ), -0.5 ) );
		E2 = ( ( 1.0 + pow_2( M ) ) * ( 1.0 + pow_2( N ) ) ) / ( 1.0 + pow_2( M ) + pow_2( N ) );
		E3 = std::pow( pow_2( M ) * ( 1.0 + pow_2( M ) + pow_2( N ) ) / ( ( 1.0 + pow_2( M ) ) * ( pow_2( M ) + pow_2( N ) ) ), pow_2( M ) );
		E4 = std::pow( pow_2( N ) * ( 1.0 + pow_2( M ) + pow_2( N ) ) / ( ( 1.0 + pow_2( N ) ) * ( pow_2( M ) + pow_2( N ) ) ), pow_2( N ) );

		Shelf( ShelfNum ).ViewFactor = ( 1.0 / ( Pi * M ) ) * ( E1 + 0.25 * std::log( E2 * E3 * E4 ) );

	}

	void
	FigureTDDZoneGains()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Dec 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// intialize zone gains at begin new environment

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;

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
		static bool MyEnvrnFlag( true );
		int Loop;

		if ( NumOfTDDPipes == 0 ) return;

		if ( BeginEnvrnFlag && MyEnvrnFlag ) {
			for ( Loop = 1; Loop <= NumOfTDDPipes; ++Loop ) {
				TDDPipe( Loop ).TZoneHeatGain = 0.0;
			}
			MyEnvrnFlag = false;
		}
		if ( ! BeginEnvrnFlag ) MyEnvrnFlag = true;

	}

} // DaylightingDevices

} // EnergyPlus
