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
#include <algorithm>
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <HeatBalanceSurfaceManager.hh>
#include <CommandLineInterface.hh>
#include <ConvectionCoefficients.hh>
#include <DataAirflowNetwork.hh>
#include <DataDaylighting.hh>
#include <DataDaylightingDevices.hh>
#include <DataDElight.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataStringGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalSurface.hh>
#include <DataLoopNode.hh>
#include <DataMoistureBalance.hh>
#include <DataMoistureBalanceEMPD.hh>
#include <DataPrecisionGlobals.hh>
#include <DataRoomAirModel.hh>
#include <DataRuntimeLanguage.hh>
#include <DataSizing.hh>
#include <DataSurfaces.hh>
#include <DataSystemVariables.hh>
#include <DataTimings.hh>
#include <DataWindowEquivalentLayer.hh>
#include <DataZoneEquipment.hh>
#include <DaylightingDevices.hh>
#include <DaylightingManager.hh>
#include <DElightManagerF.hh>
#include <DisplayRoutines.hh>
#include <EcoRoofManager.hh>
#include <ElectricBaseboardRadiator.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <HeatBalanceAirManager.hh>
#include <HeatBalanceHAMTManager.hh>
#include <HeatBalanceIntRadExchange.hh>
#include <HeatBalanceMovableInsulation.hh>
#include <HeatBalanceSurfaceManager.hh>
#include <HeatBalFiniteDiffManager.hh>
#include <HighTempRadiantSystem.hh>
#include <HWBaseboardRadiator.hh>
#include <InputProcessor.hh>
#include <InternalHeatGains.hh>
#include <LowTempRadiantSystem.hh>
#include <MoistureBalanceEMPDManager.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <OutputReportTabular.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <SolarShading.hh>
#include <SteamBaseboardRadiator.hh>
#include <SwimmingPool.hh>
#include <ThermalComfort.hh>
#include <UtilityRoutines.hh>
#include <WindowEquivalentLayer.hh>
#include <WindowManager.hh>

namespace EnergyPlus {

namespace HeatBalanceSurfaceManager {

	// Module containing the routines dealing with the Heat Balance of the surfaces

	// MODULE INFORMATION:
	//       AUTHOR
	//       DATE WRITTEN
	//       MODIFIED       DJS (PSU Dec 2006) to add ecoroof
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and algorithms required to
	// manage the simluation of the surface heat balance for the building.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// The heat balance method is outlined in the "TARP Reference Manual", NIST, NBSIR 83-2655, Feb 1983.
	// The methods are also summarized in many BSO Theses and papers.

	// OTHER NOTES:
	// This module was created from IBLAST subroutines

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGlobals;
	using namespace DataEnvironment;
	using namespace DataHeatBalFanSys;
	using namespace DataHeatBalance;
	using namespace DataHeatBalSurface;
	using namespace DataSurfaces;
	using DataMoistureBalance::TempOutsideAirFD;
	using DataMoistureBalance::RhoVaporAirOut;
	using DataMoistureBalance::RhoVaporAirIn;
	using DataMoistureBalance::HConvExtFD;
	using DataMoistureBalance::HMassConvExtFD;
	using DataMoistureBalance::HConvInFD;
	using DataMoistureBalance::HMassConvInFD;
	using DataMoistureBalance::RhoVaporSurfIn;
	using DataMoistureBalance::HSkyFD;
	using DataMoistureBalance::HGrndFD;
	using DataMoistureBalance::HAirFD;
	//unused0909USE DataMoistureBalanceEMPD, ONLY: MoistEMPDNew, MoistEMPDFlux

	// Use statements for access to subroutines in other modules
	using namespace InputProcessor;
	using namespace ScheduleManager;
	using namespace SolarShading;
	using namespace DaylightingManager;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	static std::string const BlankString;

	namespace {
		bool ManageSurfaceHeatBalancefirstTime( true );
		bool InitSurfaceHeatBalancefirstTime( true );
		bool ComputeIntSWAbsorpFactorsfirstTime( true ); // First time through routine
		bool UpdateThermalHistoriesFirstTimeFlag( true );
		bool CalculateZoneMRTfirstTime( true ); // Flag for first time calculations
		bool calcHeatBalanceInsideSurfFirstTime( true ); // Used for trapping errors or other problems
	}
	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	// na

	// Subroutine Specifications for the Heat Balance Module
	// Driver Routines

	// Initialization routines for module

	// Algorithms for the module
	// These are now external subroutines
	//PUBLIC  CalcHeatBalanceOutsideSurf  ! The heat balance routines are now public because the
	//PUBLIC  CalcHeatBalanceInsideSurf   ! radiant systems need access to them in order to simulate

	// Record Keeping/Utility Routines for Module

	// Reporting routines for module

	// MODULE SUBROUTINES:
	//*************************************************************************

	// Functions
	void
	clear_state()
	{
		ManageSurfaceHeatBalancefirstTime = true;
		InitSurfaceHeatBalancefirstTime = true;
		ComputeIntSWAbsorpFactorsfirstTime = true;
		UpdateThermalHistoriesFirstTimeFlag = true;
		CalculateZoneMRTfirstTime = true;
		calcHeatBalanceInsideSurfFirstTime = true;
	}

	void
	ManageSurfaceHeatBalance()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   January 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages the heat surface balance method of calculating
		// building thermal loads.  It is called from the HeatBalanceManager
		// at the time step level.  This driver manages the calls to all of
		// the other drivers and simulation algorithms.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using HeatBalanceAirManager::ManageAirHeatBalance;
		using ThermalComfort::ManageThermalComfort;
		using OutputReportTabular::GatherComponentLoadsSurface; // for writing tabular compoonent loads output reports
		using HeatBalFiniteDiffManager::SurfaceFD;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		int SurfNum;
		int ConstrNum;

		// FLOW:
		if ( ManageSurfaceHeatBalancefirstTime ) DisplayString( "Initializing Surfaces" );
		InitSurfaceHeatBalance(); // Initialize all heat balance related parameters

		// Solve the zone heat balance 'Detailed' solution
		// Call the outside and inside surface heat balances
		if ( ManageSurfaceHeatBalancefirstTime ) DisplayString( "Calculate Outside Surface Heat Balance" );
		CalcHeatBalanceOutsideSurf();
		if ( ManageSurfaceHeatBalancefirstTime ) DisplayString( "Calculate Inside Surface Heat Balance" );
		CalcHeatBalanceInsideSurf();

		// The air heat balance must be called before the temperature history
		// updates because there may be a radiant system in the building
		if ( ManageSurfaceHeatBalancefirstTime ) DisplayString( "Calculate Air Heat Balance" );
		ManageAirHeatBalance();

		// IF NECESSARY, do one final "average" heat balance pass.  This is only
		// necessary if a radiant system is present and it was actually on for
		// part or all of the time step.
		UpdateFinalSurfaceHeatBalance();

		// Before we leave the Surface Manager the thermal histories need to be updated
		if ( ( any_eq( HeatTransferAlgosUsed, UseCTF ) ) || ( any_eq( HeatTransferAlgosUsed, UseEMPD ) ) ) {
			UpdateThermalHistories(); //Update the thermal histories
		}

		if ( any_eq( HeatTransferAlgosUsed, UseCondFD ) ) {
			for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
				if ( Surface( SurfNum ).Construction <= 0 ) continue; // Shading surface, not really a heat transfer surface
				ConstrNum = Surface( SurfNum ).Construction;
				if ( Construct( ConstrNum ).TypeIsWindow ) continue; //  Windows simulated in Window module
				if ( Surface( SurfNum ).HeatTransferAlgorithm != HeatTransferModel_CondFD ) continue;
				SurfaceFD( SurfNum ).UpdateMoistureBalance();
			}
		}

		ManageThermalComfort( false ); // "Record keeping" for the zone

		ReportSurfaceHeatBalance();
		if ( ZoneSizingCalc ) GatherComponentLoadsSurface();

		ManageSurfaceHeatBalancefirstTime = false;

	}

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitSurfaceHeatBalance()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard J. Liesen
		//       DATE WRITTEN   January 1998
		//       MODIFIED       Nov. 1999, FCW,
		//                      Move ComputeIntThermalAbsorpFactors
		//                      so called every timestep
		//                      Jan 2004, RJH
		//                      Added calls to alternative daylighting analysis using DElight
		//                      All modifications demarked with RJH (Rob Hitchcock)
		//                      RJH, Jul 2004: add error handling for DElight calls
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for surface initializations within the
		// heat balance.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger record keeping events.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataDaylighting::ZoneDaylight;
		using DataDaylighting::NoDaylighting;
		using DataDaylighting::mapResultsToReport;
		using DataDaylighting::TotIllumMaps;
		using DataDaylightingDevices::NumOfTDDPipes;
		using DataDElight::LUX2FC;
		using namespace SolarShading;
		using ConvectionCoefficients::InitInteriorConvectionCoeffs;
		using InternalHeatGains::ManageInternalHeatGains;
		using DataRoomAirModel::IsZoneDV;
		using DataRoomAirModel::IsZoneCV;
		using DataRoomAirModel::IsZoneUI;
		using HeatBalanceIntRadExchange::CalcInteriorRadExchange;
		using HeatBalFiniteDiffManager::InitHeatBalFiniteDiff;
		using DataSystemVariables::GoodIOStatValue;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		// RJH DElight Modification Begin
		using namespace DElightManagerF;
		// RJH DElight Modification End

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt fmtA( "(A)" );
		static gio::Fmt fmtLD( "*" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ConstrNum; // Construction index
		int NZ; // DO loop counter for zones
		Real64 QIC; // Intermediate calculation variable
		Real64 QOC; // Intermediate calculation variable
		int SurfNum; // DO loop counter for surfaces
		int Term; // DO loop counter for conduction equation terms
		Real64 TSC; // Intermediate calculation variable

		// RJH DElight Modification Begin
		Real64 dPowerReducFac; // Return value Electric Lighting Power Reduction Factor for current Zone and Timestep
		Real64 dHISKFFC; // double value for argument passing
		Real64 dHISUNFFC; // double value for argument passing
		Real64 dSOLCOS1; // double value for argument passing
		Real64 dSOLCOS2; // double value for argument passing
		Real64 dSOLCOS3; // double value for argument passing
		Real64 dLatitude; // double value for argument passing
		Real64 dCloudFraction; // double value for argument passing
		int iErrorFlag; // Error Flag for warning/errors returned from DElight
		int iDElightErrorFile; // Unit number for reading DElight Error File (eplusout.delightdfdmp)
		int iReadStatus; // Error File Read Status
		std::string cErrorLine; // Each DElight Error line can be up to 210 characters long
		std::string cErrorMsg; // Each DElight Error Message can be up to 200 characters long
		bool bEndofErrFile; // End of Error File flag
		int iDElightRefPt; // Reference Point number for reading DElight Dump File (eplusout.delighteldmp)
		Real64 dRefPtIllum; // tmp var for reading RefPt illuminance
		// RJH DElight Modification End

		int MapNum;
		int iwriteStatus;
		bool errFlag;
		bool elOpened;
		//  LOGICAL :: ShadowingSurf

		// FLOW:

		assert( equal_dimensions( TH, QH ) );

		if ( InitSurfaceHeatBalancefirstTime ) DisplayString( "Initializing Outdoor environment for Surfaces" );
		// Initialize zone outdoor environmental variables
		// Bulk Initialization for Temperatures & WindSpeed
		// using the zone, modify the zone  Dry/Wet BulbTemps
		SetZoneOutBulbTempAt();
		CheckZoneOutBulbTempAt();

		SetZoneWindSpeedAt();
		//  DO ZoneNum = 1, NumOfZones
		//    Zone(ZoneNum)%WindSpeed = WindSpeedAt(Zone(ZoneNum)%Centroid%z)
		//  END DO

		// Initialize surface outdoor environmental variables
		// Bulk Initialization for Temperatures & WindSpeed
		// using the surface centroids, modify the surface Dry/Wet BulbTemps
		SetSurfaceOutBulbTempAt();
		CheckSurfaceOutBulbTempAt();

		SetSurfaceWindSpeedAt();
		//  DO SurfNum = 1, TotSurfaces
		//    IF (Surface(SurfNum)%ExtWind) Surface(SurfNum)%WindSpeed = WindSpeedAt(Surface(SurfNum)%Centroid%z)
		//  END DO

		if ( AnyEnergyManagementSystemInModel ) {
			for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
				if ( Surface( SurfNum ).OutDryBulbTempEMSOverrideOn ) {
					Surface( SurfNum ).OutDryBulbTemp = Surface( SurfNum ).OutDryBulbTempEMSOverrideValue;
				}
				if ( Surface( SurfNum ).OutWetBulbTempEMSOverrideOn ) {
					Surface( SurfNum ).OutWetBulbTemp = Surface( SurfNum ).OutWetBulbTempEMSOverrideValue;
				}
				if ( Surface( SurfNum ).WindSpeedEMSOverrideOn ) {
					Surface( SurfNum ).WindSpeed = Surface( SurfNum ).WindSpeedEMSOverrideValue;
				}
			}
		}

		// Do the Begin Simulation initializations
		if ( BeginSimFlag ) {
			AllocateSurfaceHeatBalArrays(); // Allocate the Module Arrays before any inits take place
			InterZoneWindow = std::any_of( Zone.begin(), Zone.end(), []( DataHeatBalance::ZoneData const & e ){ return e.HasInterZoneWindow; } );
			IsZoneDV.dimension( NumOfZones, false );
			IsZoneCV.dimension( NumOfZones, false );
			IsZoneUI.dimension( NumOfZones, false );
		}

		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag ) {
			if ( InitSurfaceHeatBalancefirstTime ) DisplayString( "Initializing Temperature and Flux Histories" );
			InitThermalAndFluxHistories(); // Set initial temperature and flux histories
		}

		// There are no daily initializations done in this portion of the surface heat balance

		// There are no hourly initializations done in this portion of the surface heat balance
		if ( AnyEnergyManagementSystemInModel ) {
			InitEMSControlledConstructions();
			InitEMSControlledSurfaceProperties();

		}

		// Need to be called each timestep in order to check if surface points to new construction (EMS) and if does then
		// complex fenestration needs to be initialized for additional states
		TimestepInitComplexFenestration();

		// Calculate exterior-surface multipliers that account for anisotropy of
		// sky radiance
		if ( SunIsUp && DifSolarRad > 0.0 ) {
			AnisoSkyViewFactors();
		} else {
			AnisoSkyMult = 0.0;
		}

		// Set shading flag for exterior windows (except flags related to daylighting) and
		// window construction (unshaded or shaded) to be used in heat balance calculation
		if ( InitSurfaceHeatBalancefirstTime ) DisplayString( "Initializing Window Shading" );
		WindowShadingManager();

		// Calculate factors that are used to determine how much long-wave radiation from internal
		// gains is absorbed by interior surfaces
		if ( InitSurfaceHeatBalancefirstTime ) DisplayString( "Computing Interior Absorption Factors" );
		ComputeIntThermalAbsorpFactors();

		// Calculate factors for diffuse solar absorbed by room surfaces and interior shades
		if ( InitSurfaceHeatBalancefirstTime ) DisplayString( "Computing Interior Diffuse Solar Absorption Factors" );
		ComputeIntSWAbsorpFactors();

		// Calculate factors for exchange of diffuse solar between zones through interzone windows
		if ( InitSurfaceHeatBalancefirstTime ) DisplayString( "Computing Interior Diffuse Solar Exchange through Interzone Windows" );
		ComputeDifSolExcZonesWIZWindows( NumOfZones );

		// For daylit zones, calculate interior daylight illuminance at reference points and
		// simulate lighting control system to get overhead electric lighting reduction
		// factor due to daylighting.

		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			if ( Surface( SurfNum ).Class == SurfaceClass_Window && Surface( SurfNum ).ExtSolar ) {
				SurfaceWindow( SurfNum ).IllumFromWinAtRefPt1Rep = 0.0;
				SurfaceWindow( SurfNum ).IllumFromWinAtRefPt2Rep = 0.0;
				SurfaceWindow( SurfNum ).LumWinFromRefPt1Rep = 0.0;
				SurfaceWindow( SurfNum ).LumWinFromRefPt2Rep = 0.0;
			}
		}

		for ( NZ = 1; NZ <= NumOfZones; ++NZ ) {
			// RJH DElight Modification Begin - Change Daylighting test to continue for Detailed AND DElight
			if ( ZoneDaylight( NZ ).DaylightType == NoDaylighting ) continue;
			// RJH DElight Modification End - Change Daylighting test to continue for Detailed AND DElight
			ZoneDaylight( NZ ).DaylIllumAtRefPt = 0.0;
			ZoneDaylight( NZ ).GlareIndexAtRefPt = 0.0;
			ZoneDaylight( NZ ).ZonePowerReductionFactor = 1.0;
			ZoneDaylight( NZ ).InterReflIllFrIntWins = 0.0; // inter-reflected illuminance from interior windows
			if ( ZoneDaylight( NZ ).TotalDaylRefPoints != 0 ) {
				ZoneDaylight( NZ ).TimeExceedingGlareIndexSPAtRefPt = 0.0;
				ZoneDaylight( NZ ).TimeExceedingDaylightIlluminanceSPAtRefPt = 0.0;
			}

			if ( SunIsUp && ZoneDaylight( NZ ).TotalDaylRefPoints != 0 ) {
				if ( InitSurfaceHeatBalancefirstTime ) DisplayString( "Computing Interior Daylighting Illumination" );
				DayltgInteriorIllum( NZ );
				if ( ! DoingSizing ) DayltgInteriorMapIllum( NZ );
			}

			if ( SunIsUp && NumOfTDDPipes > 0 && NZ == 1 ) {
				if ( InitSurfaceHeatBalancefirstTime ) DisplayString( "Computing Interior Daylighting Illumination for TDD pipes" );
				DayltgInteriorTDDIllum();
			}

			// RJH DElight Modification Begin - Call to DElight electric lighting control subroutine
			// Check if the sun is up and the current Thermal Zone hosts a Daylighting:DElight object
			if ( SunIsUp && ZoneDaylight( NZ ).TotalDElightRefPts != 0 ) {
				// Call DElight interior illuminance and electric lighting control subroutine
				dPowerReducFac = 1.0;
				dHISKFFC = HISKF * LUX2FC;
				dHISUNFFC = HISUNF * LUX2FC;
				dSOLCOS1 = SOLCOS( 1 );
				dSOLCOS2 = SOLCOS( 2 );
				dSOLCOS3 = SOLCOS( 3 );
				dLatitude = Latitude;
				dCloudFraction = CloudFraction;
				// Init Error Flag to 0 (no Warnings or Errors)
				iErrorFlag = 0;
				DElightElecLtgCtrl( len( Zone( NZ ).Name ), Zone( NZ ).Name, dLatitude, dHISKFFC, dHISUNFFC, dCloudFraction, dSOLCOS1, dSOLCOS2, dSOLCOS3, dPowerReducFac, iErrorFlag );
				// Check Error Flag for Warnings or Errors returning from DElight
				// RJH 2008-03-07: If no warnings/errors then read refpt illuminances for standard output reporting
				if ( iErrorFlag != 0 ) {
					// Open DElight Electric Lighting Error File for reading
					iDElightErrorFile = GetNewUnitNumber();
					// RJH 2008-03-07: open file with READWRITE
					{ IOFlags flags; flags.ACTION( "READWRITE" ); gio::open( iDElightErrorFile, DataStringGlobals::outputDelightDfdmpFileName, flags ); iwriteStatus = flags.ios(); }
					if ( iwriteStatus == 0 ) {
						elOpened = true;
					} else {
						elOpened = false;
					}
					//            IF (iwriteStatus /= 0) THEN
					//              CALL ShowFatalError('InitSurfaceHeatBalance: Could not open file "eplusout.delighteldmp" for output (readwrite).')
					//            ENDIF
					//            Open(unit=iDElightErrorFile, file='eplusout.delighteldmp', action='READ')

					// Sequentially read lines in DElight Electric Lighting Error File
					// and process them using standard EPlus warning/error handling calls
					bEndofErrFile = false;
					iReadStatus = 0;
					while ( ! bEndofErrFile && iwriteStatus == 0 && iReadStatus == 0 ) {
						{ IOFlags flags; gio::read( iDElightErrorFile, fmtA, flags ) >> cErrorLine; iReadStatus = flags.ios(); }
						if ( iReadStatus < GoodIOStatValue ) {
							bEndofErrFile = true;
							continue;
						}
						// Is the current line a Warning message?
						if ( has_prefix( cErrorLine, "WARNING: " ) ) {
							cErrorMsg = cErrorLine.substr( 9 );
							ShowWarningError( cErrorMsg );
						}
						// Is the current line an Error message?
						if ( has_prefix( cErrorLine, "ERROR: " ) ) {
							cErrorMsg = cErrorLine.substr( 7 );
							ShowSevereError( cErrorMsg );
							iErrorFlag = 1;
						}
					}

					// Close DElight Error File and delete
					if ( elOpened ) { IOFlags flags; flags.DISPOSE( "DELETE" ); gio::close( iDElightErrorFile, flags ); };
					// If any DElight Error occurred then ShowFatalError to terminate
					if ( iErrorFlag > 0 ) {
						ShowFatalError( "End of DElight Error Messages" );
					}
				} else { // RJH 2008-03-07: No errors
					// extract reference point illuminance values from DElight Electric Lighting dump file for reporting
					// Open DElight Electric Lighting Dump File for reading
					iDElightErrorFile = GetNewUnitNumber();
					{ IOFlags flags; flags.ACTION( "READWRITE" ); gio::open( iDElightErrorFile, DataStringGlobals::outputDelightDfdmpFileName, flags ); iwriteStatus = flags.ios(); }
					//            IF (iwriteStatus /= 0) THEN
					//              CALL ShowFatalError('InitSurfaceHeatBalance: Could not open file "eplusout.delighteldmp" for output (readwrite).')
					//            ENDIF
					if ( iwriteStatus == 0 ) {
						elOpened = true;
					} else {
						elOpened = false;
					}

					// Sequentially read lines in DElight Electric Lighting Dump File
					// and extract refpt illuminances for standard EPlus output handling
					bEndofErrFile = false;
					iDElightRefPt = 0;
					iReadStatus = 0;
					while ( ! bEndofErrFile && iwriteStatus == 0 && iReadStatus == 0 ) {
						{ IOFlags flags; gio::read( iDElightErrorFile, fmtLD, flags ) >> dRefPtIllum; iReadStatus = flags.ios(); }
						if ( iReadStatus < GoodIOStatValue ) {
							bEndofErrFile = true;
							continue;
						}
						// Increment refpt counter
						++iDElightRefPt;
						// Assure refpt index does not exceed number of refpts in this zone
						if ( iDElightRefPt <= ZoneDaylight( NZ ).TotalDElightRefPts ) {
							ZoneDaylight( NZ ).DaylIllumAtRefPt( iDElightRefPt ) = dRefPtIllum;
						}
					}

					// Close DElight Electric Lighting Dump File and delete
					if ( elOpened ) { IOFlags flags; flags.DISPOSE( "DELETE" ); gio::close( iDElightErrorFile, flags ); };
				}
				// Store the calculated total zone Power Reduction Factor due to DElight daylighting
				// in the ZoneDaylight structure for later use
				ZoneDaylight( NZ ).ZonePowerReductionFactor = dPowerReducFac;
			}
			// RJH DElight Modification End - Call to DElight electric lighting control subroutine

		}

		errFlag = false;
		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			if ( Surface( SurfNum ).Class != SurfaceClass_Window ) continue;
			SurfaceWindow( SurfNum ).FracTimeShadingDeviceOn = 0.0;
			if ( SurfaceWindow( SurfNum ).ShadingFlag > 0 ) {
				SurfaceWindow( SurfNum ).FracTimeShadingDeviceOn = 1.0;
			} else {
				SurfaceWindow( SurfNum ).FracTimeShadingDeviceOn = 0.0;
			}
		}

		CalcInteriorRadExchange( TH( 2, 1, _ ), 0, NetLWRadToSurf, _, "Main" );

		if ( AirflowWindows ) WindowGapAirflowControl();

		// The order of these initializations is important currently.  Over time we hope to
		//  take the appropriate parts of these inits to the other heat balance managers
		if ( InitSurfaceHeatBalancefirstTime ) DisplayString( "Initializing Solar Heat Gains" );
		InitSolarHeatGains();
		if ( SunIsUp && ( BeamSolarRad + GndSolarRad + DifSolarRad > 0.0 ) ) {
			for ( NZ = 1; NZ <= NumOfZones; ++NZ ) {
				if ( ZoneDaylight( NZ ).TotalDaylRefPoints > 0 ) {
					if ( Zone( NZ ).HasInterZoneWindow ) {
						DayltgInterReflIllFrIntWins( NZ );
						DayltgGlareWithIntWins( ZoneDaylight( NZ ).GlareIndexAtRefPt, NZ );
					}
					DayltgElecLightingControl( NZ );
				}
			}
		} else if ( mapResultsToReport && TimeStep == NumOfTimeStepInHour ) {
			for ( MapNum = 1; MapNum <= TotIllumMaps; ++MapNum ) {
				ReportIllumMap( MapNum );
			}
			mapResultsToReport = false;
		}

		if ( InitSurfaceHeatBalancefirstTime ) DisplayString( "Initializing Internal Heat Gains" );
		ManageInternalHeatGains( false );
		if ( InitSurfaceHeatBalancefirstTime ) DisplayString( "Initializing Interior Solar Distribution" );
		InitIntSolarDistribution();
		if ( InitSurfaceHeatBalancefirstTime ) DisplayString( "Initializing Interior Convection Coefficients" );
		InitInteriorConvectionCoeffs( TempSurfInTmp );

		if ( BeginSimFlag ) { // Now's the time to report surfaces, if desired
			//    if (firstTime) CALL DisplayString('Reporting Surfaces')
			//    CALL ReportSurfaces
			if ( InitSurfaceHeatBalancefirstTime ) DisplayString( "Gathering Information for Predefined Reporting" );
			GatherForPredefinedReport();
		}

		// Initialize the temperature history terms for conduction through the surfaces
		if ( any_eq( HeatTransferAlgosUsed, UseCondFD ) ) {
			InitHeatBalFiniteDiff();
		}

		CTFConstOutPart = 0.0;
		CTFConstInPart = 0.0;
		CTFTsrcConstPart = 0.0;
		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) { // Loop through all surfaces...
			auto const & surface( Surface( SurfNum ) );

			if ( ! surface.HeatTransSurf ) continue; // Skip non-heat transfer surfaces
			if ( surface.HeatTransferAlgorithm != HeatTransferModel_CTF && surface.HeatTransferAlgorithm != HeatTransferModel_EMPD ) continue;
			if ( surface.Class == SurfaceClass_Window ) continue;
			// Outside surface temp of "normal" windows not needed in Window5 calculation approach
			// Window layer temperatures are calculated in CalcHeatBalanceInsideSurf

			ConstrNum = surface.Construction;
			auto const & construct( Construct( ConstrNum ) );
			if ( construct.NumCTFTerms > 1 ) { // COMPUTE CONSTANT PORTION OF CONDUCTIVE FLUXES.

				QIC = 0.0;
				QOC = 0.0;
				TSC = 0.0;
				auto l11( TH.index( 1, 2, SurfNum ) );
				auto l12( TH.index( 2, 2, SurfNum ) );
				auto const s3( TH.size3() );
				for ( Term = 1; Term <= construct.NumCTFTerms; ++Term, l11 += s3, l12 += s3 ) { // [ l11 ] == ( 1, Term + 1, SurfNum ), [ l12 ] == ( 1, Term + 1, SurfNum )

					// Sign convention for the various terms in the following two equations
					// is based on the form of the Conduction Transfer Function equation
					// given by:
					// Qin,now  = (Sum of)(Y Tout) - (Sum of)(Z Tin) + (Sum of)(F Qin,old)
					// Qout,now = (Sum of)(X Tout) - (Sum of)(Y Tin) + (Sum of)(F Qout,old)
					// In both equations, flux is positive from outside to inside.

					//Tuned Aliases and linear indexing
					Real64 const ctf_cross( construct.CTFCross( Term ) );
					Real64 const TH11( TH[ l11 ] );
					Real64 const TH12( TH[ l12 ] );

					QIC += ctf_cross * TH11 - construct.CTFInside( Term ) * TH12 + construct.CTFFlux( Term ) * QH[ l12 ];

					QOC += construct.CTFOutside( Term ) * TH11 - ctf_cross * TH12 + construct.CTFFlux( Term ) * QH[ l11 ];

					if ( construct.SourceSinkPresent ) {
						Real64 const QsrcHist1( QsrcHist( SurfNum, Term + 1 ) );

						QIC += construct.CTFSourceIn( Term ) * QsrcHist1;

						QOC += construct.CTFSourceOut( Term ) * QsrcHist1;

						TSC += construct.CTFTSourceOut( Term ) * TH11 + construct.CTFTSourceIn( Term ) * TH12 + construct.CTFTSourceQ( Term ) * QsrcHist1 + construct.CTFFlux( Term ) * TsrcHist( SurfNum, Term + 1 );
					}

				}

				CTFConstOutPart( SurfNum ) = QOC;
				CTFConstInPart( SurfNum ) = QIC;
				CTFTsrcConstPart( SurfNum ) = TSC;

			} else { // Number of CTF Terms = 1-->Resistance only constructions have no history terms.

				CTFConstOutPart( SurfNum ) = 0.0;
				CTFConstInPart( SurfNum ) = 0.0;
				CTFTsrcConstPart( SurfNum ) = 0.0;

			}

		} // ...end of surfaces DO loop for initializing temperature history terms for the surface heat balances

		// Zero out all of the radiant system heat balance coefficient arrays
		RadSysTiHBConstCoef = 0.0;
		RadSysTiHBToutCoef = 0.0;
		RadSysTiHBQsrcCoef = 0.0;
		RadSysToHBConstCoef = 0.0;
		RadSysToHBTinCoef = 0.0;
		RadSysToHBQsrcCoef = 0.0;

		QRadSysSource = 0.0;
		QPVSysSource = 0.0;
		QHTRadSysSurf = 0.0;
		QHWBaseboardSurf = 0.0;
		QSteamBaseboardSurf = 0.0;
		QElecBaseboardSurf = 0.0;
		QPoolSurfNumerator = 0.0;
		PoolHeatTransCoefs = 0.0;

		if ( ZoneSizingCalc ) GatherComponentLoadsSurfAbsFact();

		if ( InitSurfaceHeatBalancefirstTime ) DisplayString( "Completed Initializing Surface Heat Balance" );
		InitSurfaceHeatBalancefirstTime = false;

	}

	void
	GatherForPredefinedReport()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   August 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine reports the information for the predefined reports
		// related to envelope components.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace OutputReportPredefined;
		using WindowManager::CalcNominalWindowCond;

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
		std::string surfName;
		int curCons;
		int zonePt;
		Real64 mult;
		Real64 curAzimuth;
		Real64 curTilt;
		int iSurf;
		Real64 windowArea;
		Real64 frameWidth;
		Real64 frameArea;
		Real64 dividerArea;
		//counts for object count report
		Array1D_int numSurfaces( 20 );
		Array1D_int numExtSurfaces( 20 );
		int frameDivNum;
		bool isExterior;
		Array1D< Real64 > computedNetArea; // holds the gross wall area minus the window and door areas

		// the following variables are for the CalcNominalWindowCond call but only SHGCSummer is needed
		Real64 nomCond;
		Real64 SHGCSummer;
		Real64 TransSolNorm;
		Real64 TransVisNorm;
		Real64 nomUfact;
		int errFlag;
		int curWSC;
		//following variables are totals for fenestration table
		static Real64 windowAreaWMult( 0.0 );
		static Real64 fenTotArea( 0.0 );
		static Real64 fenTotAreaNorth( 0.0 );
		static Real64 fenTotAreaNonNorth( 0.0 );
		static Real64 ufactArea( 0.0 );
		static Real64 ufactAreaNorth( 0.0 );
		static Real64 ufactAreaNonNorth( 0.0 );
		static Real64 shgcArea( 0.0 );
		static Real64 shgcAreaNorth( 0.0 );
		static Real64 shgcAreaNonNorth( 0.0 );
		static Real64 vistranArea( 0.0 );
		static Real64 vistranAreaNorth( 0.0 );
		static Real64 vistranAreaNonNorth( 0.0 );
		static Real64 intFenTotArea( 0.0 );
		static Real64 intUfactArea( 0.0 );
		static Real64 intShgcArea( 0.0 );
		static Real64 intVistranArea( 0.0 );
		bool isNorth;

		numSurfaces = 0;
		numExtSurfaces = 0;

		computedNetArea.allocate( TotSurfaces );
		computedNetArea = 0.0; // start at zero, add wall area and subtract window and door area

		for ( iSurf = 1; iSurf <= TotSurfaces; ++iSurf ) {
			zonePt = Surface( iSurf ).Zone;
			//only exterior surfaces including underground
			if ( ( Surface( iSurf ).ExtBoundCond == ExternalEnvironment ) || ( Surface( iSurf ).ExtBoundCond == Ground ) || ( Surface( iSurf ).ExtBoundCond == GroundFCfactorMethod ) ) {
				isExterior = true;
				{ auto const SELECT_CASE_var( Surface( iSurf ).Class );
				if ( ( SELECT_CASE_var == SurfaceClass_Wall ) || ( SELECT_CASE_var == SurfaceClass_Floor ) || ( SELECT_CASE_var == SurfaceClass_Roof ) ) {
					surfName = Surface( iSurf ).Name;
					curCons = Surface( iSurf ).Construction;
					PreDefTableEntry( pdchOpCons, surfName, Construct( curCons ).Name );
					PreDefTableEntry( pdchOpRefl, surfName, 1 - Construct( curCons ).OutsideAbsorpSolar );
					PreDefTableEntry( pdchOpUfactNoFilm, surfName, NominalU( Surface( iSurf ).Construction ), 3 );
					mult = Zone( zonePt ).Multiplier * Zone( zonePt ).ListMultiplier;
					PreDefTableEntry( pdchOpGrArea, surfName, Surface( iSurf ).GrossArea * mult );
					computedNetArea(iSurf) += Surface( iSurf ).GrossArea * mult;
					curAzimuth = Surface( iSurf ).Azimuth;
					PreDefTableEntry( pdchOpAzimuth, surfName, curAzimuth );
					curTilt = Surface( iSurf ).Tilt;
					PreDefTableEntry( pdchOpTilt, surfName, curTilt );
					if ( ( curTilt >= 60.0 ) && ( curTilt < 180.0 ) ) {
						if ( ( curAzimuth >= 315.0 ) || ( curAzimuth < 45.0 ) ) {
							PreDefTableEntry( pdchOpDir, surfName, "N" );
						} else if ( ( curAzimuth >= 45.0 ) && ( curAzimuth < 135.0 ) ) {
							PreDefTableEntry( pdchOpDir, surfName, "E" );
						} else if ( ( curAzimuth >= 135.0 ) && ( curAzimuth < 225.0 ) ) {
							PreDefTableEntry( pdchOpDir, surfName, "S" );
						} else if ( ( curAzimuth >= 225.0 ) && ( curAzimuth < 315.0 ) ) {
							PreDefTableEntry( pdchOpDir, surfName, "W" );
						}
					}
				} else if ( ( SELECT_CASE_var == SurfaceClass_Window ) || ( SELECT_CASE_var == SurfaceClass_TDD_Dome ) ) {
					surfName = Surface( iSurf ).Name;
					curCons = Surface( iSurf ).Construction;
					PreDefTableEntry( pdchFenCons, surfName, Construct( curCons ).Name );
					zonePt = Surface( iSurf ).Zone;
					mult = Zone( zonePt ).Multiplier * Zone( zonePt ).ListMultiplier * Surface( iSurf ).Multiplier;
					//include the frame area if present
					windowArea = Surface( iSurf ).GrossArea;
					frameArea = 0.0;
					dividerArea = 0.0;
					frameDivNum = Surface( iSurf ).FrameDivider;
					if ( frameDivNum != 0 ) {
						frameWidth = FrameDivider( frameDivNum ).FrameWidth;
						frameArea = ( Surface( iSurf ).Height + 2.0 * frameWidth ) * ( Surface( iSurf ).Width + 2.0 * frameWidth ) - ( Surface( iSurf ).Height * Surface( iSurf ).Width );
						windowArea += frameArea;
						dividerArea = FrameDivider( frameDivNum ).DividerWidth * ( FrameDivider( frameDivNum ).HorDividers * Surface( iSurf ).Width + FrameDivider( frameDivNum ).VertDividers * Surface( iSurf ).Height - FrameDivider( frameDivNum ).HorDividers * FrameDivider( frameDivNum ).VertDividers * FrameDivider( frameDivNum ).DividerWidth );
						PreDefTableEntry( pdchFenFrameConductance, surfName, FrameDivider( frameDivNum ).FrameConductance, 3 );
						PreDefTableEntry( pdchFenDividerConductance, surfName, FrameDivider( frameDivNum ).DividerConductance, 3 );
					}
					windowAreaWMult = windowArea * mult;
					PreDefTableEntry( pdchFenAreaOf1, surfName, windowArea );
					PreDefTableEntry( pdchFenFrameAreaOf1, surfName, frameArea );
					PreDefTableEntry( pdchFenDividerAreaOf1, surfName, dividerArea );
					PreDefTableEntry( pdchFenGlassAreaOf1, surfName, windowArea - ( frameArea + dividerArea ) );
					PreDefTableEntry( pdchFenArea, surfName, windowAreaWMult );
					computedNetArea( Surface( iSurf ).BaseSurf ) -= windowAreaWMult;
					nomUfact = NominalU( Surface( iSurf ).Construction );
					PreDefTableEntry( pdchFenUfact, surfName, nomUfact, 3 );
					//if the construction report is requested the SummerSHGC is already calculated
					if ( Construct( curCons ).SummerSHGC != 0 ) {
						SHGCSummer = Construct( curCons ).SummerSHGC;
						TransVisNorm = Construct( curCons ).VisTransNorm;
					} else {
						//must calculate Summer SHGC
						if ( ! Construct( curCons ).WindowTypeEQL ) {
							CalcNominalWindowCond( curCons, 2, nomCond, SHGCSummer, TransSolNorm, TransVisNorm, errFlag );
						}
					}
					PreDefTableEntry( pdchFenSHGC, surfName, SHGCSummer, 3 );
					PreDefTableEntry( pdchFenVisTr, surfName, TransVisNorm, 3 );
					PreDefTableEntry( pdchFenParent, surfName, Surface( iSurf ).BaseSurfName );
					curAzimuth = Surface( iSurf ).Azimuth;
					PreDefTableEntry( pdchFenAzimuth, surfName, curAzimuth );
					isNorth = false;
					curTilt = Surface( iSurf ).Tilt;
					PreDefTableEntry( pdchFenTilt, surfName, curTilt );
					if ( ( curTilt >= 60.0 ) && ( curTilt < 180.0 ) ) {
						if ( ( curAzimuth >= 315.0 ) || ( curAzimuth < 45.0 ) ) {
							PreDefTableEntry( pdchFenDir, surfName, "N" );
							isNorth = true;
						} else if ( ( curAzimuth >= 45.0 ) && ( curAzimuth < 135.0 ) ) {
							PreDefTableEntry( pdchFenDir, surfName, "E" );
						} else if ( ( curAzimuth >= 135.0 ) && ( curAzimuth < 225.0 ) ) {
							PreDefTableEntry( pdchFenDir, surfName, "S" );
						} else if ( ( curAzimuth >= 225.0 ) && ( curAzimuth < 315.0 ) ) {
							PreDefTableEntry( pdchFenDir, surfName, "W" );
						}
					}
					curWSC = Surface( iSurf ).WindowShadingControlPtr;
					//compute totals for area weighted averages
					fenTotArea += windowAreaWMult;
					ufactArea += nomUfact * windowAreaWMult;
					shgcArea += SHGCSummer * windowAreaWMult;
					vistranArea += TransVisNorm * windowAreaWMult;
					if ( isNorth ) {
						fenTotAreaNorth += windowAreaWMult;
						ufactAreaNorth += nomUfact * windowAreaWMult;
						shgcAreaNorth += SHGCSummer * windowAreaWMult;
						vistranAreaNorth += TransVisNorm * windowAreaWMult;
					} else {
						fenTotAreaNonNorth += windowAreaWMult;
						ufactAreaNonNorth += nomUfact * windowAreaWMult;
						shgcAreaNonNorth += SHGCSummer * windowAreaWMult;
						vistranAreaNonNorth += TransVisNorm * windowAreaWMult;
					}
					// shading
					if ( curWSC != 0 ) {
						PreDefTableEntry( pdchFenSwitchable, surfName, "Yes" );
						//shading report
						PreDefTableEntry( pdchWscName, surfName, WindowShadingControl( curWSC ).Name );
						{ auto const SELECT_CASE_var1( WindowShadingControl( curWSC ).ShadingType );
						if ( SELECT_CASE_var1 == WSC_ST_NoShade ) {
							PreDefTableEntry( pdchWscShading, surfName, "No Shade" );
						} else if ( SELECT_CASE_var1 == WSC_ST_InteriorShade ) {
							PreDefTableEntry( pdchWscShading, surfName, "Interior Shade" );
						} else if ( SELECT_CASE_var1 == WSC_ST_SwitchableGlazing ) {
							PreDefTableEntry( pdchWscShading, surfName, "Switchable Glazing" );
						} else if ( SELECT_CASE_var1 == WSC_ST_ExteriorShade ) {
							PreDefTableEntry( pdchWscShading, surfName, "Exterior Shade" );
						} else if ( SELECT_CASE_var1 == WSC_ST_InteriorBlind ) {
							PreDefTableEntry( pdchWscShading, surfName, "Interior Blind" );
						} else if ( SELECT_CASE_var1 == WSC_ST_ExteriorBlind ) {
							PreDefTableEntry( pdchWscShading, surfName, "Exterior Blind" );
						} else if ( SELECT_CASE_var1 == WSC_ST_BetweenGlassShade ) {
							PreDefTableEntry( pdchWscShading, surfName, "Between Glass Shade" );
						} else if ( SELECT_CASE_var1 == WSC_ST_BetweenGlassBlind ) {
							PreDefTableEntry( pdchWscShading, surfName, "Between Glass Blind" );
						} else if ( SELECT_CASE_var1 == WSC_ST_ExteriorScreen ) {
							PreDefTableEntry( pdchWscShading, surfName, "Exterior Screen" );
						}}
						{ auto const SELECT_CASE_var1( WindowShadingControl( curWSC ).ShadingControlType );
						if ( SELECT_CASE_var1 == WSCT_AlwaysOn ) {
							PreDefTableEntry( pdchWscControl, surfName, "AlwaysOn" );
						} else if ( SELECT_CASE_var1 == WSCT_AlwaysOff ) {
							PreDefTableEntry( pdchWscControl, surfName, "AlwaysOff" );
						} else if ( SELECT_CASE_var1 == WSCT_OnIfScheduled ) {
							PreDefTableEntry( pdchWscControl, surfName, "OnIfScheduleAllows" );
						} else if ( SELECT_CASE_var1 == WSCT_HiSolar ) {
							PreDefTableEntry( pdchWscControl, surfName, "OnIfHighSolarOnWindow" );
						} else if ( SELECT_CASE_var1 == WSCT_HiHorzSolar ) {
							PreDefTableEntry( pdchWscControl, surfName, "OnIfHighHorizontalSolar" );
						} else if ( SELECT_CASE_var1 == WSCT_HiOutAirTemp ) {
							PreDefTableEntry( pdchWscControl, surfName, "OnIfHighOutdoorAirTemperature" );
						} else if ( SELECT_CASE_var1 == WSCT_HiZoneAirTemp ) {
							PreDefTableEntry( pdchWscControl, surfName, "OnIfHighZoneAirTemperature" );
						} else if ( SELECT_CASE_var1 == WSCT_HiZoneCooling ) {
							PreDefTableEntry( pdchWscControl, surfName, "OnIfHighZoneCooling" );
						} else if ( SELECT_CASE_var1 == WSCT_HiGlare ) {
							PreDefTableEntry( pdchWscControl, surfName, "OnIfHighGlare" );
						} else if ( SELECT_CASE_var1 == WSCT_MeetDaylIlumSetp ) {
							PreDefTableEntry( pdchWscControl, surfName, "MeetDaylightIlluminanceSetpoint" );
						} else if ( SELECT_CASE_var1 == WSCT_OnNightLoOutTemp_OffDay ) {
							PreDefTableEntry( pdchWscControl, surfName, "OnNightIfLowOutdoorTempAndOffDay" );
						} else if ( SELECT_CASE_var1 == WSCT_OnNightLoInTemp_OffDay ) {
							PreDefTableEntry( pdchWscControl, surfName, "OnNightIfLowInsideTempAndOffDay" );
						} else if ( SELECT_CASE_var1 == WSCT_OnNightIfHeating_OffDay ) {
							PreDefTableEntry( pdchWscControl, surfName, "OnNightIfHeatingAndOffDay" );
						} else if ( SELECT_CASE_var1 == WSCT_OnNightLoOutTemp_OnDayCooling ) {
							PreDefTableEntry( pdchWscControl, surfName, "OnNightIfLowOutdoorTempAndOnDayIfCooling" );
						} else if ( SELECT_CASE_var1 == WSCT_OnNightIfHeating_OnDayCooling ) {
							PreDefTableEntry( pdchWscControl, surfName, "OnNightIfHeatingAndOnDayIfCooling" );
						} else if ( SELECT_CASE_var1 == WSCT_OffNight_OnDay_HiSolarWindow ) {
							PreDefTableEntry( pdchWscControl, surfName, "OffNightAndOnDayIfCoolingAndHighSolarOnWindow" );
						} else if ( SELECT_CASE_var1 == WSCT_OnNight_OnDay_HiSolarWindow ) {
							PreDefTableEntry( pdchWscControl, surfName, "OnNightAndOnDayIfCoolingAndHighSolarOnWindow" );
						} else if ( SELECT_CASE_var1 == WSCT_OnHiOutTemp_HiSolarWindow ) {
							PreDefTableEntry( pdchWscControl, surfName, "OnIfHighOutdoorAirTempAndHighSolarOnWindow" );
						} else if ( SELECT_CASE_var1 == WSCT_OnHiOutTemp_HiHorzSolar ) {
							PreDefTableEntry( pdchWscControl, surfName, "OnIfHighOutdoorAirTempAndHighHorizontalSolar" );
						} else if ( SELECT_CASE_var1 == WSCT_OnHiZoneTemp_HiSolarWindow ) {
							PreDefTableEntry( pdchWscControl, surfName, "OnIfHighZoneAirTempAndHighSolarOnWindow" );
						} else if ( SELECT_CASE_var1 == WSCT_OnHiZoneTemp_HiHorzSolar ) {
							PreDefTableEntry( pdchWscControl, surfName, "OnIfHighZoneAirTempAndHighHorizontalSolar" );
						}}
						if ( WindowShadingControl( curWSC ).ShadedConstruction != 0 ) {
							PreDefTableEntry( pdchWscShadCons, surfName, Construct( WindowShadingControl( curWSC ).ShadedConstruction ).Name );
						}
						if ( WindowShadingControl( curWSC ).GlareControlIsActive ) {
							PreDefTableEntry( pdchWscGlare, surfName, "Yes" );
						} else {
							PreDefTableEntry( pdchWscGlare, surfName, "No" );
						}
					} else {
						PreDefTableEntry( pdchFenSwitchable, surfName, "No" );
					}
				} else if ( SELECT_CASE_var == SurfaceClass_Door ) {
					surfName = Surface( iSurf ).Name;
					curCons = Surface( iSurf ).Construction;
					PreDefTableEntry( pdchDrCons, surfName, Construct( curCons ).Name );
					PreDefTableEntry( pdchDrUfactNoFilm, surfName, NominalU( Surface( iSurf ).Construction ), 3 );
					mult = Zone( zonePt ).Multiplier * Zone( zonePt ).ListMultiplier;
					PreDefTableEntry( pdchDrGrArea, surfName, Surface( iSurf ).GrossArea * mult );
					PreDefTableEntry( pdchDrParent, surfName, Surface( iSurf ).BaseSurfName );
					computedNetArea( Surface( iSurf ).BaseSurf ) -= Surface( iSurf ).GrossArea * mult;

				}}
			} else {
				isExterior = false;
				// interior window report
				if ( Surface( iSurf ).Class == SurfaceClass_Window ) {
					if ( ! has_prefix( Surface( iSurf ).Name, "iz-" ) ) { // don't count created interzone surfaces that are mirrors of other surfaces
						surfName = Surface( iSurf ).Name;
						curCons = Surface( iSurf ).Construction;
						PreDefTableEntry( pdchIntFenCons, surfName, Construct( curCons ).Name );
						zonePt = Surface( iSurf ).Zone;
						mult = Zone( zonePt ).Multiplier * Zone( zonePt ).ListMultiplier * Surface( iSurf ).Multiplier;
						// include the frame area if present
						windowArea = Surface( iSurf ).GrossArea;
						if ( Surface( iSurf ).FrameDivider != 0 ) {
							frameWidth = FrameDivider( Surface( iSurf ).FrameDivider ).FrameWidth;
							frameArea = ( Surface( iSurf ).Height + 2 * frameWidth ) * ( Surface( iSurf ).Width + 2 * frameWidth ) - ( Surface( iSurf ).Height * Surface( iSurf ).Width );
							windowArea += frameArea;
						}
						windowAreaWMult = windowArea * mult;
						PreDefTableEntry( pdchIntFenAreaOf1, surfName, windowArea );
						PreDefTableEntry( pdchIntFenArea, surfName, windowAreaWMult );
						nomUfact = NominalU( Surface( iSurf ).Construction );
						PreDefTableEntry( pdchIntFenUfact, surfName, nomUfact, 3 );
						// if the construction report is requested the SummerSHGC is already calculated
						if ( Construct( curCons ).SummerSHGC != 0 ) {
							SHGCSummer = Construct( curCons ).SummerSHGC;
							TransVisNorm = Construct( curCons ).VisTransNorm;
						} else {
							// must calculate Summer SHGC
							if ( ! Construct( curCons ).WindowTypeEQL ) {
								CalcNominalWindowCond( curCons, 2, nomCond, SHGCSummer, TransSolNorm, TransVisNorm, errFlag );
							}
						}
						PreDefTableEntry( pdchIntFenSHGC, surfName, SHGCSummer, 3 );
						PreDefTableEntry( pdchIntFenVisTr, surfName, TransVisNorm, 3 );
						PreDefTableEntry( pdchIntFenParent, surfName, Surface( iSurf ).BaseSurfName );
						// compute totals for area weighted averages
						intFenTotArea += windowAreaWMult;
						intUfactArea += nomUfact * windowAreaWMult;
						intShgcArea += SHGCSummer * windowAreaWMult;
						intVistranArea += TransVisNorm * windowAreaWMult;
					}
				}
			}
			if ( ( Surface( iSurf ).Class <= 20 ) && ( Surface( iSurf ).Class >= 1 ) ) {
				++numSurfaces( Surface( iSurf ).Class );
				if ( isExterior ) {
					++numExtSurfaces( Surface( iSurf ).Class );
				}
			}
		}
		// go through all the surfaces again and this time insert the net area results
		for ( iSurf = 1; iSurf <= TotSurfaces; ++iSurf ) {
			zonePt = Surface( iSurf ).Zone;
			//only exterior surfaces including underground
			if ( ( Surface( iSurf ).ExtBoundCond == ExternalEnvironment ) || ( Surface( iSurf ).ExtBoundCond == Ground ) || ( Surface( iSurf ).ExtBoundCond == GroundFCfactorMethod ) ) {
				isExterior = true;
				{ auto const SELECT_CASE_var( Surface( iSurf ).Class );
				if ( ( SELECT_CASE_var == SurfaceClass_Wall ) || ( SELECT_CASE_var == SurfaceClass_Floor ) || ( SELECT_CASE_var == SurfaceClass_Roof ) ) {
					surfName = Surface( iSurf ).Name;
					PreDefTableEntry( pdchOpNetArea, surfName, computedNetArea( iSurf ) );
				}
				}
			}
		}
		// total
		PreDefTableEntry( pdchFenArea, "Total or Average", fenTotArea );
		if ( fenTotArea > 0.0 ) {
			PreDefTableEntry( pdchFenUfact, "Total or Average", ufactArea / fenTotArea, 3 );
			PreDefTableEntry( pdchFenSHGC, "Total or Average", shgcArea / fenTotArea, 3 );
			PreDefTableEntry( pdchFenVisTr, "Total or Average", vistranArea / fenTotArea, 3 );
		} else {
			PreDefTableEntry( pdchFenUfact, "Total or Average", "-" );
			PreDefTableEntry( pdchFenSHGC, "Total or Average", "-" );
			PreDefTableEntry( pdchFenVisTr, "Total or Average", "-" );
		}
		// north
		PreDefTableEntry( pdchFenArea, "North Total or Average", fenTotAreaNorth );
		if ( fenTotAreaNorth > 0.0 ) {
			PreDefTableEntry( pdchFenUfact, "North Total or Average", ufactAreaNorth / fenTotAreaNorth, 3 );
			PreDefTableEntry( pdchFenSHGC, "North Total or Average", shgcAreaNorth / fenTotAreaNorth, 3 );
			PreDefTableEntry( pdchFenVisTr, "North Total or Average", vistranAreaNorth / fenTotAreaNorth, 3 );
		} else {
			PreDefTableEntry( pdchFenUfact, "North Total or Average", "-" );
			PreDefTableEntry( pdchFenSHGC, "North Total or Average", "-" );
			PreDefTableEntry( pdchFenVisTr, "North Total or Average", "-" );
		}
		// non-north
		PreDefTableEntry( pdchFenArea, "Non-North Total or Average", fenTotAreaNonNorth );
		if ( fenTotAreaNonNorth > 0.0 ) {
			PreDefTableEntry( pdchFenUfact, "Non-North Total or Average", ufactAreaNonNorth / fenTotAreaNonNorth, 3 );
			PreDefTableEntry( pdchFenSHGC, "Non-North Total or Average", shgcAreaNonNorth / fenTotAreaNonNorth, 3 );
			PreDefTableEntry( pdchFenVisTr, "Non-North Total or Average", vistranAreaNonNorth / fenTotAreaNonNorth, 3 );
		} else {
			PreDefTableEntry( pdchFenUfact, "Non-North Total or Average", "-" );
			PreDefTableEntry( pdchFenSHGC, "Non-North Total or Average", "-" );
			PreDefTableEntry( pdchFenVisTr, "Non-North Total or Average", "-" );
		}
		//interior fenestration totals
		PreDefTableEntry( pdchIntFenArea, "Total or Average", intFenTotArea );
		if ( intFenTotArea > 0.0 ) {
			PreDefTableEntry( pdchIntFenUfact, "Total or Average", intUfactArea / intFenTotArea, 3 );
			PreDefTableEntry( pdchIntFenSHGC, "Total or Average", intShgcArea / intFenTotArea, 3 );
			PreDefTableEntry( pdchIntFenVisTr, "Total or Average", intVistranArea / intFenTotArea, 3 );
		} else {
			PreDefTableEntry( pdchIntFenUfact, "Total or Average", "-" );
			PreDefTableEntry( pdchIntFenSHGC, "Total or Average", "-" );
			PreDefTableEntry( pdchIntFenVisTr, "Total or Average", "-" );
		}
		//counts
		PreDefTableEntry( pdchSurfCntTot, "Wall", numSurfaces( SurfaceClass_Wall ) );
		PreDefTableEntry( pdchSurfCntExt, "Wall", numExtSurfaces( SurfaceClass_Wall ) );
		PreDefTableEntry( pdchSurfCntTot, "Floor", numSurfaces( SurfaceClass_Floor ) );
		PreDefTableEntry( pdchSurfCntExt, "Floor", numExtSurfaces( SurfaceClass_Floor ) );
		PreDefTableEntry( pdchSurfCntTot, "Roof", numSurfaces( SurfaceClass_Roof ) );
		PreDefTableEntry( pdchSurfCntExt, "Roof", numExtSurfaces( SurfaceClass_Roof ) );
		PreDefTableEntry( pdchSurfCntTot, "Internal Mass", numSurfaces( SurfaceClass_IntMass ) );
		PreDefTableEntry( pdchSurfCntExt, "Internal Mass", numExtSurfaces( SurfaceClass_IntMass ) );
		PreDefTableEntry( pdchSurfCntTot, "Building Detached Shading", numSurfaces( SurfaceClass_Detached_B ) );
		PreDefTableEntry( pdchSurfCntExt, "Building Detached Shading", numExtSurfaces( SurfaceClass_Detached_B ) );
		PreDefTableEntry( pdchSurfCntTot, "Fixed Detached Shading", numSurfaces( SurfaceClass_Detached_F ) );
		PreDefTableEntry( pdchSurfCntExt, "Fixed Detached Shading", numExtSurfaces( SurfaceClass_Detached_F ) );
		PreDefTableEntry( pdchSurfCntTot, "Window", numSurfaces( SurfaceClass_Window ) );
		PreDefTableEntry( pdchSurfCntExt, "Window", numExtSurfaces( SurfaceClass_Window ) );
		PreDefTableEntry( pdchSurfCntTot, "Door", numSurfaces( SurfaceClass_Door ) );
		PreDefTableEntry( pdchSurfCntExt, "Door", numExtSurfaces( SurfaceClass_Door ) );
		PreDefTableEntry( pdchSurfCntTot, "Glass Door", numSurfaces( SurfaceClass_GlassDoor ) );
		PreDefTableEntry( pdchSurfCntExt, "Glass Door", numExtSurfaces( SurfaceClass_GlassDoor ) );
		PreDefTableEntry( pdchSurfCntTot, "Shading", numSurfaces( SurfaceClass_Shading ) );
		PreDefTableEntry( pdchSurfCntExt, "Shading", numExtSurfaces( SurfaceClass_Shading ) );
		PreDefTableEntry( pdchSurfCntTot, "Overhang", numSurfaces( SurfaceClass_Overhang ) );
		PreDefTableEntry( pdchSurfCntExt, "Overhang", numExtSurfaces( SurfaceClass_Overhang ) );
		PreDefTableEntry( pdchSurfCntTot, "Fin", numSurfaces( SurfaceClass_Fin ) );
		PreDefTableEntry( pdchSurfCntExt, "Fin", numExtSurfaces( SurfaceClass_Fin ) );
		PreDefTableEntry( pdchSurfCntTot, "Tubular Daylighting Device Dome", numSurfaces( SurfaceClass_TDD_Dome ) );
		PreDefTableEntry( pdchSurfCntExt, "Tubular Daylighting Device Dome", numExtSurfaces( SurfaceClass_TDD_Dome ) );
		PreDefTableEntry( pdchSurfCntTot, "Tubular Daylighting Device Diffuser", numSurfaces( SurfaceClass_TDD_Diffuser ) );
		PreDefTableEntry( pdchSurfCntExt, "Tubular Daylighting Device Diffuser", numExtSurfaces( SurfaceClass_TDD_Diffuser ) );
	}

	void
	AllocateSurfaceHeatBalArrays()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   February 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger variable allocation.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		//  USE DataRoomAirModel, ONLY: IsZoneDV,IsZoneCV,HVACMassFlow, ZoneDVMixedFlag

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int loop;

		// FLOW:

		// Use the total number of surfaces to allocate variables to avoid a surface number limit
		CTFConstInPart.dimension( TotSurfaces, 0.0 );
		CTFConstOutPart.dimension( TotSurfaces, 0.0 );
		CTFTsrcConstPart.dimension( TotSurfaces, 0.0 );
		TempEffBulkAir.dimension( TotSurfaces, 23.0 );
		HConvIn.dimension( TotSurfaces, 0.0 );
		HcExtSurf.dimension( TotSurfaces, 0.0 );
		HAirExtSurf.dimension( TotSurfaces, 0.0 );
		HSkyExtSurf.dimension( TotSurfaces, 0.0 );
		HGrdExtSurf.dimension( TotSurfaces, 0.0 );
		TempSurfIn.dimension( TotSurfaces, 0.0 );
		TempSurfInTmp.dimension( TotSurfaces, 0.0 );
		QRadSWOutAbs.dimension( TotSurfaces, 0.0 );
		QRadSWInAbs.dimension( TotSurfaces, 0.0 );
		InitialDifSolInAbs.dimension( TotSurfaces, 0.0 );
		InitialDifSolInTrans.dimension( TotSurfaces, 0.0 );
		QRadSWwinAbs.dimension( CFSMAXNL + 1, TotSurfaces, 0.0 );
		InitialDifSolwinAbs.dimension( CFSMAXNL, TotSurfaces, 0.0 );
		QRadSWOutMvIns.dimension( TotSurfaces, 0.0 );
		QRadThermInAbs.dimension( TotSurfaces, 0.0 );
		SUMH.dimension( TotSurfaces, 0 );

		TH.dimension( 2, MaxCTFTerms, TotSurfaces, 0.0 );
		TempSurfOut.dimension( TotSurfaces, 0.0 );
		TempSurfInRep.dimension( TotSurfaces, 0.0 );
		QConvInReport.dimension( TotSurfaces, 0.0 );
		QdotConvInRepPerArea.dimension( TotSurfaces, 0.0 );
		QdotConvInRep.dimension( TotSurfaces, 0.0 );

		QRadNetSurfInReport.dimension( TotSurfaces, 0.0 );
		QdotRadNetSurfInRep.dimension( TotSurfaces, 0.0 );
		QdotRadNetSurfInRepPerArea.dimension( TotSurfaces, 0.0 );

		QRadSolarInReport.dimension( TotSurfaces, 0.0 );
		QdotRadSolarInRep.dimension( TotSurfaces, 0.0 );
		QdotRadSolarInRepPerArea.dimension( TotSurfaces, 0.0 );

		QRadLightsInReport.dimension( TotSurfaces, 0.0 );
		QdotRadLightsInRep.dimension( TotSurfaces, 0.0 );
		QdotRadLightsInRepPerArea.dimension( TotSurfaces, 0.0 );

		QRadIntGainsInReport.dimension( TotSurfaces, 0.0 );
		QdotRadIntGainsInRep.dimension( TotSurfaces, 0.0 );
		QdotRadIntGainsInRepPerArea.dimension( TotSurfaces, 0.0 );

		QRadHVACInReport.dimension( TotSurfaces, 0.0 );
		QdotRadHVACInRep.dimension( TotSurfaces, 0.0 );
		QdotRadHVACInRepPerArea.dimension( TotSurfaces, 0.0 );

		QConvOutReport.dimension( TotSurfaces, 0.0 );
		QdotConvOutRepPerArea.dimension( TotSurfaces, 0.0 );
		QdotConvOutRep.dimension( TotSurfaces, 0.0 );

		QdotRadOutRep.dimension( TotSurfaces, 0.0 );
		QdotRadOutRepPerArea.dimension( TotSurfaces, 0.0 );
		QRadOutReport.dimension( TotSurfaces, 0.0 );

		OpaqSurfInsFaceConduction.dimension( TotSurfaces, 0.0 );
		OpaqSurfInsFaceConductionFlux.dimension( TotSurfaces, 0.0 );
		OpaqSurfInsFaceCondGainRep.dimension( TotSurfaces, 0.0 );
		OpaqSurfInsFaceCondLossRep.dimension( TotSurfaces, 0.0 );
		OpaqSurfInsFaceConductionEnergy.dimension( TotSurfaces, 0.0 );

		SWOutAbsTotalReport.dimension( TotSurfaces, 0.0 );
		SWOutAbsEnergyReport.dimension( TotSurfaces, 0.0 );

		OpaqSurfOutsideFaceConduction.dimension( TotSurfaces, 0.0 );
		OpaqSurfOutsideFaceConductionFlux.dimension( TotSurfaces, 0.0 );
		OpaqSurfExtFaceCondGainRep.dimension( TotSurfaces, 0.0 );
		OpaqSurfExtFaceCondLossRep.dimension( TotSurfaces, 0.0 );
		OpaqSurfOutsideFaceConductionEnergy.dimension( TotSurfaces, 0.0 );

		OpaqSurfAvgFaceCondGainRep.dimension( TotSurfaces, 0.0 );
		OpaqSurfAvgFaceCondLossRep.dimension( TotSurfaces, 0.0 );
		OpaqSurfAvgFaceConduction.dimension( TotSurfaces, 0.0 );
		OpaqSurfAvgFaceConductionFlux.dimension( TotSurfaces, 0.0 );
		OpaqSurfAvgFaceConductionEnergy.dimension( TotSurfaces, 0.0 );

		OpaqSurfStorageGainRep.dimension( TotSurfaces, 0.0 );
		OpaqSurfStorageCondLossRep.dimension( TotSurfaces, 0.0 );
		OpaqSurfStorageConduction.dimension( TotSurfaces, 0.0 );
		OpaqSurfStorageConductionFlux.dimension( TotSurfaces, 0.0 );
		OpaqSurfStorageConductionEnergy.dimension( TotSurfaces, 0.0 );

		OpaqSurfInsFaceBeamSolAbsorbed.dimension( TotSurfaces, 0.0 );
		TempSource.dimension( TotSurfaces, 0.0 );
		QH.dimension( 2, MaxCTFTerms, TotSurfaces, 0.0 );
		THM.dimension( 2, MaxCTFTerms, TotSurfaces, 0.0 );
		QHM.dimension( 2, MaxCTFTerms, TotSurfaces, 0.0 );
		TsrcHist.dimension( TotSurfaces, MaxCTFTerms, 0.0 );
		QsrcHist.dimension( TotSurfaces, MaxCTFTerms, 0.0 );
		TsrcHistM.dimension( TotSurfaces, MaxCTFTerms, 0.0 );
		QsrcHistM.dimension( TotSurfaces, MaxCTFTerms, 0.0 );

		NetLWRadToSurf.dimension( TotSurfaces, 0.0 );
		QRadSWLightsInAbs.dimension( TotSurfaces, 0.0 );

		RadSysTiHBConstCoef.dimension( TotSurfaces, 0.0 );
		RadSysTiHBToutCoef.dimension( TotSurfaces, 0.0 );
		RadSysTiHBQsrcCoef.dimension( TotSurfaces, 0.0 );
		RadSysToHBConstCoef.dimension( TotSurfaces, 0.0 );
		RadSysToHBTinCoef.dimension( TotSurfaces, 0.0 );
		RadSysToHBQsrcCoef.dimension( TotSurfaces, 0.0 );
		QRadSysSource.dimension( TotSurfaces, 0.0 );
		TCondFDSourceNode.dimension( TotSurfaces, 15.0 );
		QHTRadSysSurf.dimension( TotSurfaces, 0.0 );
		QHWBaseboardSurf.dimension( TotSurfaces, 0.0 );
		QSteamBaseboardSurf.dimension( TotSurfaces, 0.0 );
		QElecBaseboardSurf.dimension( TotSurfaces, 0.0 );

		// allocate terms used for pool surface heat balance
		QPoolSurfNumerator.dimension( TotSurfaces, 0.0 );
		PoolHeatTransCoefs.dimension( TotSurfaces, 0.0 );

		// allocate term used as sink for PV electricity
		QPVSysSource.dimension( TotSurfaces, 0.0 );

		//Allocate the moisture balance arrays
		TempOutsideAirFD.dimension( TotSurfaces, 0.0 );
		RhoVaporAirOut.dimension( TotSurfaces, 0.0 );
		RhoVaporSurfIn.dimension( TotSurfaces, 0.0 );
		RhoVaporAirIn.dimension( TotSurfaces, 0.0 );
		HConvExtFD.dimension( TotSurfaces, 0.0 );
		HMassConvExtFD.dimension( TotSurfaces, 0.0 );
		HConvInFD.dimension( TotSurfaces, 0.0 );
		HMassConvInFD.dimension( TotSurfaces, 0.0 );
		HSkyFD.dimension( TotSurfaces, 0.0 );
		HGrndFD.dimension( TotSurfaces, 0.0 );
		HAirFD.dimension( TotSurfaces, 0.0 );

		DisplayString( "Setting up Surface Reporting Variables" );

		// Setup surface report variables CurrentModuleObject='Opaque Surfaces'
		for ( loop = 1; loop <= TotSurfaces; ++loop ) {
			if ( ! Surface( loop ).HeatTransSurf ) continue;
			SetupOutputVariable( "Surface Inside Face Temperature [C]", TempSurfInRep( loop ), "Zone", "State", Surface( loop ).Name );
			SetupOutputVariable( "Surface Outside Face Temperature [C]", TempSurfOut( loop ), "Zone", "State", Surface( loop ).Name );
			SetupOutputVariable( "Surface Inside Face Adjacent Air Temperature [C]", TempEffBulkAir( loop ), "Zone", "State", Surface( loop ).Name );
			SetupOutputVariable( "Surface Inside Face Convection Heat Transfer Coefficient [W/m2-K]", HConvIn( loop ), "Zone", "State", Surface( loop ).Name );
			SetupOutputVariable( "Surface Inside Face Convection Heat Gain Rate [W]", QdotConvInRep( loop ), "Zone", "State", Surface( loop ).Name );
			SetupOutputVariable( "Surface Inside Face Convection Heat Gain Rate per Area [W/m2]", QdotConvInRepPerArea( loop ), "Zone", "State", Surface( loop ).Name );
			SetupOutputVariable( "Surface Inside Face Convection Heat Gain Energy [J]", QConvInReport( loop ), "Zone", "Sum", Surface( loop ).Name );

			SetupOutputVariable( "Surface Inside Face Net Surface Thermal Radiation Heat Gain Rate [W]", QdotRadNetSurfInRep( loop ), "Zone", "State", Surface( loop ).Name );
			SetupOutputVariable( "Surface Inside Face Net Surface Thermal Radiation Heat Gain Rate per Area [W/m2]", QdotRadNetSurfInRepPerArea( loop ), "Zone", "State", Surface( loop ).Name );
			SetupOutputVariable( "Surface Inside Face Net Surface Thermal Radiation Heat Gain Energy [J]", QRadNetSurfInReport( loop ), "Zone", "Sum", Surface( loop ).Name );

			if ( Surface( loop ).Class != SurfaceClass_Window ) {
				SetupOutputVariable( "Surface Inside Face Solar Radiation Heat Gain Rate [W]", QdotRadSolarInRep( loop ), "Zone", "State", Surface( loop ).Name );
				SetupOutputVariable( "Surface Inside Face Solar Radiation Heat Gain Rate per Area [W/m2]", QdotRadSolarInRepPerArea( loop ), "Zone", "State", Surface( loop ).Name );
				SetupOutputVariable( "Surface Inside Face Solar Radiation Heat Gain Energy [J]", QRadSolarInReport( loop ), "Zone", "Sum", Surface( loop ).Name );

				SetupOutputVariable( "Surface Inside Face Lights Radiation Heat Gain Rate [W]", QdotRadLightsInRep( loop ), "Zone", "State", Surface( loop ).Name );
				SetupOutputVariable( "Surface Inside Face Lights Radiation Heat Gain Rate per Area [W/m2]", QdotRadLightsInRepPerArea( loop ), "Zone", "State", Surface( loop ).Name );
				SetupOutputVariable( "Surface Inside Face Lights Radiation Heat Gain Energy [J]", QRadLightsInReport( loop ), "Zone", "Sum", Surface( loop ).Name );
			}

			SetupOutputVariable( "Surface Inside Face Internal Gains Radiation Heat Gain Rate [W]", QdotRadIntGainsInRep( loop ), "Zone", "State", Surface( loop ).Name );
			SetupOutputVariable( "Surface Inside Face Internal Gains Radiation Heat Gain Rate per Area [W/m2]", QdotRadIntGainsInRepPerArea( loop ), "Zone", "State", Surface( loop ).Name );
			SetupOutputVariable( "Surface Inside Face Internal Gains Radiation Heat Gain Energy [J]", QRadIntGainsInReport( loop ), "Zone", "Sum", Surface( loop ).Name );

			SetupOutputVariable( "Surface Inside Face System Radiation Heat Gain Rate [W]", QdotRadHVACInRep( loop ), "Zone", "State", Surface( loop ).Name );
			SetupOutputVariable( "Surface Inside Face System Radiation Heat Gain Rate per Area [W/m2]", QdotRadHVACInRepPerArea( loop ), "Zone", "State", Surface( loop ).Name );
			SetupOutputVariable( "Surface Inside Face System Radiation Heat Gain Energy [J]", QRadHVACInReport( loop ), "Zone", "Sum", Surface( loop ).Name );

			if ( Surface( loop ).ExtBoundCond == ExternalEnvironment || DisplayAdvancedReportVariables ) {
				SetupOutputVariable( "Surface Outside Face Outdoor Air Drybulb Temperature [C]", Surface( loop ).OutDryBulbTemp, "Zone", "State", Surface( loop ).Name );
				SetupOutputVariable( "Surface Outside Face Outdoor Air Wetbulb Temperature [C]", Surface( loop ).OutWetBulbTemp, "Zone", "State", Surface( loop ).Name );
				SetupOutputVariable( "Surface Outside Face Outdoor Air Wind Speed [m/s]", Surface( loop ).WindSpeed, "Zone", "State", Surface( loop ).Name );
				SetupOutputVariable( "Surface Outside Face Convection Heat Gain Rate [W]", QdotConvOutRep( loop ), "Zone", "State", Surface( loop ).Name );
				SetupOutputVariable( "Surface Outside Face Convection Heat Gain Rate per Area [W/m2]", QdotConvOutRepPerArea( loop ), "Zone", "State", Surface( loop ).Name );
				SetupOutputVariable( "Surface Outside Face Convection Heat Gain Energy [J]", QConvOutReport( loop ), "Zone", "Sum", Surface( loop ).Name );
				SetupOutputVariable( "Surface Outside Face Convection Heat Transfer Coefficient [W/m2-K]", HcExtSurf( loop ), "Zone", "State", Surface( loop ).Name );
				SetupOutputVariable( "Surface Outside Face Net Thermal Radiation Heat Gain Rate [W]", QdotRadOutRep( loop ), "Zone", "State", Surface( loop ).Name );
				SetupOutputVariable( "Surface Outside Face Net Thermal Radiation Heat Gain Rate per Area [W/m2]", QdotRadOutRepPerArea( loop ), "Zone", "State", Surface( loop ).Name );
				SetupOutputVariable( "Surface Outside Face Net Thermal Radiation Heat Gain Energy [J]", QRadOutReport( loop ), "Zone", "Sum", Surface( loop ).Name );
				SetupOutputVariable( "Surface Outside Face Thermal Radiation to Air Heat Transfer Coefficient [W/m2-K]", HAirExtSurf( loop ), "Zone", "State", Surface( loop ).Name );
				SetupOutputVariable( "Surface Outside Face Thermal Radiation to Sky Heat Transfer Coefficient [W/m2-K]", HSkyExtSurf( loop ), "Zone", "State", Surface( loop ).Name );
				SetupOutputVariable( "Surface Outside Face Thermal Radiation to Ground Heat Transfer Coefficient [W/m2-K]", HGrdExtSurf( loop ), "Zone", "State", Surface( loop ).Name );
				if ( Surface( loop ).Class != SurfaceClass_Window ) {
					SetupOutputVariable( "Surface Outside Face Solar Radiation Heat Gain Rate [W]", SWOutAbsTotalReport( loop ), "Zone", "Average", Surface( loop ).Name );
					SetupOutputVariable( "Surface Outside Face Solar Radiation Heat Gain Rate per Area [W/m2]", QRadSWOutAbs( loop ), "Zone", "Average", Surface( loop ).Name );
					SetupOutputVariable( "Surface Outside Face Solar Radiation Heat Gain Energy [J]", SWOutAbsEnergyReport( loop ), "Zone", "Sum", Surface( loop ).Name );
				}
			}
			if ( Surface( loop ).Class == SurfaceClass_Floor || Surface( loop ).Class == SurfaceClass_Wall || Surface( loop ).Class == SurfaceClass_IntMass || Surface( loop ).Class == SurfaceClass_Roof || Surface( loop ).Class == SurfaceClass_Door ) {
				//      IF (DisplayAdvancedReportVariables) THEN  !CurrentModuleObject='Opaque Surfaces(Advanced)'
				SetupOutputVariable( "Surface Inside Face Conduction Heat Transfer Rate [W]", OpaqSurfInsFaceConduction( loop ), "Zone", "State", Surface( loop ).Name );
				SetupOutputVariable( "Surface Inside Face Conduction Heat Gain Rate [W]", OpaqSurfInsFaceCondGainRep( loop ), "Zone", "State", Surface( loop ).Name );
				SetupOutputVariable( "Surface Inside Face Conduction Heat Loss Rate [W]", OpaqSurfInsFaceCondLossRep( loop ), "Zone", "State", Surface( loop ).Name );
				SetupOutputVariable( "Surface Inside Face Conduction Heat Transfer Rate per Area [W/m2]", OpaqSurfInsFaceConductionFlux( loop ), "Zone", "State", Surface( loop ).Name );
				SetupOutputVariable( "Surface Inside Face Conduction Heat Transfer Energy [J]", OpaqSurfInsFaceConductionEnergy( loop ), "Zone", "Sum", Surface( loop ).Name );

				SetupOutputVariable( "Surface Outside Face Conduction Heat Transfer Rate [W]", OpaqSurfOutsideFaceConduction( loop ), "Zone", "State", Surface( loop ).Name );
				SetupOutputVariable( "Surface Outside Face Conduction Heat Gain Rate [W]", OpaqSurfExtFaceCondGainRep( loop ), "Zone", "State", Surface( loop ).Name );
				SetupOutputVariable( "Surface Outside Face Conduction Heat Loss Rate [W]", OpaqSurfExtFaceCondLossRep( loop ), "Zone", "State", Surface( loop ).Name );
				SetupOutputVariable( "Surface Outside Face Conduction Heat Transfer Rate per Area [W/m2]", OpaqSurfOutsideFaceConductionFlux( loop ), "Zone", "State", Surface( loop ).Name );
				SetupOutputVariable( "Surface Outside Face Conduction Heat Transfer Energy [J]", OpaqSurfOutsideFaceConductionEnergy( loop ), "Zone", "Sum", Surface( loop ).Name );

				SetupOutputVariable( "Surface Average Face Conduction Heat Transfer Rate [W]", OpaqSurfAvgFaceConduction( loop ), "Zone", "State", Surface( loop ).Name );
				SetupOutputVariable( "Surface Average Face Conduction Heat Gain Rate [W]", OpaqSurfAvgFaceCondGainRep( loop ), "Zone", "State", Surface( loop ).Name );
				SetupOutputVariable( "Surface Average Face Conduction Heat Loss Rate [W]", OpaqSurfAvgFaceCondLossRep( loop ), "Zone", "State", Surface( loop ).Name );
				SetupOutputVariable( "Surface Average Face Conduction Heat Transfer Rate per Area [W/m2]", OpaqSurfAvgFaceConductionFlux( loop ), "Zone", "State", Surface( loop ).Name );
				SetupOutputVariable( "Surface Average Face Conduction Heat Transfer Energy [J]", OpaqSurfAvgFaceConductionEnergy( loop ), "Zone", "Sum", Surface( loop ).Name );

				SetupOutputVariable( "Surface Heat Storage Rate [W]", OpaqSurfStorageConduction( loop ), "Zone", "State", Surface( loop ).Name );
				SetupOutputVariable( "Surface Heat Storage Gain Rate [W]", OpaqSurfStorageGainRep( loop ), "Zone", "State", Surface( loop ).Name );
				SetupOutputVariable( "Surface Heat Storage Loss Rate [W]", OpaqSurfStorageCondLossRep( loop ), "Zone", "State", Surface( loop ).Name );
				SetupOutputVariable( "Surface Heat Storage Rate per Area [W/m2]", OpaqSurfStorageConductionFlux( loop ), "Zone", "State", Surface( loop ).Name );
				SetupOutputVariable( "Surface Heat Storage Energy [J]", OpaqSurfStorageConductionEnergy( loop ), "Zone", "Sum", Surface( loop ).Name );

				//      ENDIF
				//CurrentModuleObject='Opaque Surfaces'

				SetupOutputVariable( "Surface Inside Face Beam Solar Radiation Heat Gain Rate [W]", OpaqSurfInsFaceBeamSolAbsorbed( loop ), "Zone", "State", Surface( loop ).Name );
			}
			if ( Construct( Surface( loop ).Construction ).SourceSinkPresent ) SetupOutputVariable( "Surface Internal Source Location Temperature [C]", TempSource( loop ), "Zone", "State", Surface( loop ).Name );
			if ( Surface( loop ).Class == SurfaceClass_Window ) { // CurrentModuleObject='Windows'
				SetupOutputVariable( "Surface Shading Device Is On Time Fraction []", SurfaceWindow( loop ).FracTimeShadingDeviceOn, "Zone", "Average", Surface( loop ).Name );
				SetupOutputVariable( "Surface Storm Window On Off Status []", SurfaceWindow( loop ).StormWinFlag, "Zone", "State", Surface( loop ).Name );
				SetupOutputVariable( "Surface Window Blind Slat Angle [deg]", SurfaceWindow( loop ).SlatAngThisTSDeg, "Zone", "State", Surface( loop ).Name );
			}
			//    IF (DisplayAdvancedReportVariables) THEN  !CurrentModuleObject='Opaque Surfaces(Advanced)'
			SetupOutputVariable( "Surface Inside Face Convection Classification Index [ ]", Surface( loop ).IntConvClassification, "Zone", "Average", Surface( loop ).Name );
			SetupOutputVariable( "Surface Inside Face Convection Model Equation Index [ ]", Surface( loop ).IntConvHcModelEq, "Zone", "Average", Surface( loop ).Name );
			SetupOutputVariable( "Surface Inside Face Convection Reference Air Index [ ]", Surface( loop ).TAirRef, "Zone", "Average", Surface( loop ).Name );
			if ( Surface( loop ).ExtBoundCond == ExternalEnvironment ) {
				SetupOutputVariable( "Surface Outside Face Convection Classification Index [ ]", Surface( loop ).OutConvClassification, "Zone", "Average", Surface( loop ).Name );
				SetupOutputVariable( "Surface Outside Face Forced Convection Model Equation Index [ ]", Surface( loop ).OutConvHfModelEq, "Zone", "Average", Surface( loop ).Name );
				SetupOutputVariable( "Surface Outside Face Natural Convection Model Equation Index [ ]", Surface( loop ).OutConvHnModelEq, "Zone", "Average", Surface( loop ).Name );
			}
			//     ENDIF
			if ( DisplayAdvancedReportVariables ) {
				SetupOutputVariable( "Surface Construction Index []", Surface( loop ).Construction, "Zone", "Average", Surface( loop ).Name );
			}

		}

		//unused  ALLOCATE(QBV(NumOfZones))
		//unused  QBV=0.0
		QC.dimension( NumOfZones, 0.0 );
		QD.dimension( NumOfZones, 0.0 );
		QDforDaylight.dimension( NumOfZones, 0.0 );
		QDV.dimension( NumOfZones, 0.0 );
		QL.dimension( NumOfZones, 0.0 );

		//UCSD
		MRT.dimension( NumOfZones, 0.0 );

		// Allocate Reporting Variables and set up tracking
		ZoneMRT.dimension( NumOfZones, 0.0 );

		for ( loop = 1; loop <= NumOfZones; ++loop ) {
			//CurrentModuleObject='Zone'
			SetupOutputVariable( "Zone Mean Radiant Temperature [C]", ZoneMRT( loop ), "Zone", "State", Zone( loop ).Name );
		}

	}

	void
	InitThermalAndFluxHistories()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         George Walton
		//       DATE WRITTEN   March 1978
		//       MODIFIED       na
		//       RE-ENGINEERED  Feb98 (RKS)

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine sets the initial temperature and flux histories
		// needed for a stable and reasonable heat balance solution starting
		// point.

		// METHODOLOGY EMPLOYED:
		// This subroutine assumes that the simulation is at steady state at
		// the beginning and then begins to vary.  Thus, the temperatures, the
		// fluxes. and their histories can all be set to the same value.  Some
		// of the initializations depend on the surface characteristics.  This
		// requires a DO loop to perform the proper calculation.

		// REFERENCES:
		// (I)BLAST legacy routine INITTH

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SurfNum; // DO loop counter for surfaces
		int OSCMnum; // DO loop counter for Other side conditions modeled (OSCM)
		// FLOW:

		// First do the "bulk" initializations of arrays sized to NumOfZones
		MRT = 23.0; // module level array
		MAT = 23.0; // DataHeatBalFanSys array
		ZT = 23.0;
		ZTAV = 23.0;
		XMAT = 23.0; // DataHeatBalFanSys array
		XM2T = 23.0; // DataHeatBalFanSys array
		XM3T = 23.0; // DataHeatBalFanSys array
		XM4T = 23.0;
		XMPT = 23.0;
		DSXMAT = 23.0; // DataHeatBalFanSys array
		DSXM2T = 23.0; // DataHeatBalFanSys array
		DSXM3T = 23.0; // DataHeatBalFanSys array
		DSXM4T = 23.0;
		ZoneTMX = 23.0; // DataHeatBalFanSys array
		ZoneTM2 = 23.0; // DataHeatBalFanSys array
		//Initialize the Zone Humidity Ratio here so that it is available for EMPD implementations
		ZoneAirHumRatAvg = OutHumRat;
		ZoneAirHumRat = OutHumRat;
		ZoneAirHumRatOld = OutHumRat;
		SumHmAW = 0.0;
		SumHmARa = 0.0;
		SumHmARaW = 0.0;

		// "Bulk" initializations of arrays sized to TotSurfaces
		SUMH = 0; // module level array
		TempSurfIn = 23.0; // module level array
		TempSurfInTmp = 23.0; // module level array
		HConvIn = 3.076; // module level array
		HcExtSurf = 0.0;
		HAirExtSurf = 0.0;
		HSkyExtSurf = 0.0;
		HGrdExtSurf = 0.0;
		TempSurfOut = 0.0;
		TempSurfInRep = 0.0;
		QConvInReport = 0.0;
		QdotConvInRep = 0.0;
		QdotConvInRepPerArea = 0.0;
		QRadNetSurfInReport = 0.0;
		QdotRadNetSurfInRep = 0.0;
		QdotRadNetSurfInRepPerArea = 0.0;
		QRadSolarInReport = 0.0;
		QdotRadSolarInRep = 0.0;
		QdotRadSolarInRepPerArea = 0.0;
		QRadLightsInReport = 0.0;
		QdotRadLightsInRep = 0.0;
		QdotRadLightsInRepPerArea = 0.0;
		QRadIntGainsInReport = 0.0;
		QdotRadIntGainsInRep = 0.0;
		QdotRadIntGainsInRepPerArea = 0.0;
		QRadHVACInReport = 0.0;
		QdotRadHVACInRep = 0.0;
		QdotRadHVACInRepPerArea = 0.0;
		QConvOutReport = 0.0;
		QdotConvOutRep = 0.0;
		QdotConvOutRepPerArea = 0.0;
		QRadOutReport = 0.0;
		QdotRadOutRep = 0.0;
		QdotRadOutRepPerArea = 0.0;
		OpaqSurfInsFaceConduction = 0.0;
		OpaqSurfInsFaceConductionFlux = 0.0;
		OpaqSurfInsFaceConductionEnergy = 0.0;
		OpaqSurfInsFaceBeamSolAbsorbed = 0.0;
		TempEffBulkAir = 23.0;
		TempTstatAir = 23.0;

		// "Bulk" initializations of temperature arrays with dimensions (TotSurface,MaxCTFTerms,2)
		TH = 23.0; // module level array
		THM = 23.0; // module level array
		TsrcHist = 23.0;
		TsrcHistM = 23.0;
		QH = 0.0;
		QHM = 0.0;
		QsrcHist = 0.0;
		QsrcHistM = 0.0;
		CondFDRelaxFactor = CondFDRelaxFactorInput;
		for ( auto & e : SurfaceWindow ) {
			// Initialize window frame and divider temperatures
			e.FrameTempSurfIn = 23.0;
			e.FrameTempSurfInOld = 23.0;
			e.FrameTempSurfOut = 23.0;
			e.DividerTempSurfIn = 23.0;
			e.DividerTempSurfInOld = 23.0;
			e.DividerTempSurfOut = 23.0;

			// Initialize previous-timestep shading indicators
			e.ExtIntShadePrevTS = 0;
			e.ShadingFlag = ShadeOff;
		}

		// Perform other initializations that depend on the surface characteristics
		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {

			if ( ! Surface( SurfNum ).HeatTransSurf ) continue; // Skip non-heat transfer surfaces

			// Reset outside boundary conditions if necessary
			if ( ( Surface( SurfNum ).ExtBoundCond == ExternalEnvironment ) || ( Surface( SurfNum ).ExtBoundCond == OtherSideCondModeledExt ) ) {

				THM( 1, {1,Construct( Surface( SurfNum ).Construction ).NumCTFTerms + 1}, SurfNum ) = Surface( SurfNum ).OutDryBulbTemp;
				TH( 1, {1,Construct( Surface( SurfNum ).Construction ).NumCTFTerms + 1}, SurfNum ) = Surface( SurfNum ).OutDryBulbTemp;

			} else if ( Surface( SurfNum ).ExtBoundCond == Ground ) {

				THM( 1, {1,Construct( Surface( SurfNum ).Construction ).NumCTFTerms + 1}, SurfNum ) = GroundTemp;
				TH( 1, {1,Construct( Surface( SurfNum ).Construction ).NumCTFTerms + 1}, SurfNum ) = GroundTemp;

			} else if ( Surface( SurfNum ).ExtBoundCond == GroundFCfactorMethod ) {

				THM( 1, {1,Construct( Surface( SurfNum ).Construction ).NumCTFTerms + 1}, SurfNum ) = GroundTempFC;
				TH( 1, {1,Construct( Surface( SurfNum ).Construction ).NumCTFTerms + 1}, SurfNum ) = GroundTempFC;

			}

			if ( Surface( SurfNum ).ExtCavityPresent ) {
				ExtVentedCavity( Surface( SurfNum ).ExtCavNum ).TbaffleLast = 20.0;
				ExtVentedCavity( Surface( SurfNum ).ExtCavNum ).TairLast = 20.0;
			}

			// Initialize the flux histories
			QH( 1, {2,Construct( Surface( SurfNum ).Construction ).NumCTFTerms + 1}, SurfNum ) = Construct( Surface( SurfNum ).Construction ).UValue * ( TH( 1, 1, SurfNum ) - TH( 2, 1, SurfNum ) );
			QH( 2, {2,Construct( Surface( SurfNum ).Construction ).NumCTFTerms + 1}, SurfNum ) = QH( 1, 2, SurfNum );
			QHM( 1, {2,Construct( Surface( SurfNum ).Construction ).NumCTFTerms + 1}, SurfNum ) = QH( 1, 2, SurfNum );
			QHM( 2, {2,Construct( Surface( SurfNum ).Construction ).NumCTFTerms + 1}, SurfNum ) = QH( 1, 2, SurfNum );

		}

		if ( TotOSCM > 1 ) {
			for ( OSCMnum = 1; OSCMnum <= TotOSCM; ++OSCMnum ) {
				OSCM( OSCMnum ).TConv = 20.0;
				OSCM( OSCMnum ).HConv = 4.0;
				OSCM( OSCMnum ).TRad = 20.0;
				OSCM( OSCMnum ).HRad = 4.0;
			}
		}

	}

	void
	InitSolarHeatGains()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Anonymous
		//       DATE WRITTEN   July 1977
		//       MODIFIED       Mar99 (FW): handle movable interior shades and
		//                                  switchable glazing
		//                      Oct99 (FW): account for Window5 glass calculation approach
		//                      May01 (FW): handle interior and exterior blinds
		//                      Sep03 (FW): initialize SurfaceWindow%FrameQRadOutAbs
		//                      May06 (RR): handle exterior window screens
		//       RE-ENGINEERED  Feb98 (RKS)

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine initializes the arrays associated with solar heat
		// gains for both individual surfaces and for zones.  As a result,
		// this routine sets the following variable arrays:
		// QBV(unused), QDV, QC, QD; QRadSWOutAbs and QRadSWInAbs (for opaque surfaces);
		// QRadSWwinAbs (for windows)

		// METHODOLOGY EMPLOYED:
		// If the sun is down, all of the pertinent arrays are zeroed.  If the
		// sun is up, various calculations are made.

		// REFERENCES:
		// (I)BLAST legacy routine QSUN

		// Using/Aliasing
		using SolarShading::CalcInteriorSolarDistribution;
		using namespace HeatBalanceMovableInsulation;
		using General::POLYF;
		using General::InterpSw;
		using General::InterpBlind;
		using General::InterpProfAng;
		using General::InterpSlatAng;
		using General::InterpProfSlatAng;
		using General::BlindBeamBeamTrans;
		using namespace DataDaylightingDevices;
		using DaylightingDevices::FindTDDPipe;
		using DaylightingDevices::TransTDD;
		using namespace DataWindowEquivalentLayer;
		using SolarShading::SurfaceScheduledSolarInc;
		using SolarShading::WindowScheduledSolarAbs;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AbsExt; // Absorptivity of outer most layer (or movable insulation if present)
		int ConstrNum; // Index for the Construct derived type
		int ConstrNumSh; // Shaded window construction
		Real64 HMovInsul; // Resistance or "h" value of movable insulation (from EvalOutsideMovableInsulation, not used)
		int RoughIndexMovInsul; // Roughness index of movable insulation
		int SurfNum; // DO loop counter for surfaces
		int SurfNum2; // TDD:DOME object number
		int PipeNum; // TDD pipe object number
		int ShelfNum; // Daylighting shelf object number
		int InShelfSurf; // Inside daylighting shelf surface number
		int OutShelfSurf; // Outside daylighting shelf surface number
		Real64 ShelfSolarRad; // Shelf diffuse solar radiation
		int ShadeFlag; // Window shading flag
		Real64 SwitchFac; // Switching factor for switchable glazing
		int ZoneNum; // DO loop counter for zones
		Real64 BeamSolar; // Local variable for BeamSolarRad
		Real64 SkySolarInc; // Sky diffuse solar incident on a surface
		Real64 GndSolarInc; // Ground diffuse solar incident on a surface
		int TotGlassLay; // Number of glass layers
		int TotSolidLay; // Number of solid layers in fenestration system (glass + shading)
		int CurrentState; // Current state for Complex Fenestration
		static Array1D< Real64 > AbsDiffWin( CFSMAXNL ); // Diffuse solar absorptance of glass layers //Tuned Made static
		static Array1D< Real64 > AbsDiffWinGnd( CFSMAXNL ); // Ground diffuse solar absorptance of glass layers //Tuned Made static
		static Array1D< Real64 > AbsDiffWinSky( CFSMAXNL ); // Sky diffuse solar absorptance of glass layers //Tuned Made static
		int Lay; // Layer number
		Real64 DividerAbs; // Window divider solar absorptance
		Real64 DividerRefl; // Window divider solar reflectance
		int MatNumGl; // Outer glass layer material number
		int MatNumGlSh; // Outer glass layer material number, switched construction
		Real64 TransGl; // Outer glass solar transmittance, reflectance, absorptance
		Real64 ReflGl;
		Real64 AbsGl;
		Real64 TransGlSh; // Outer glass solar trans, refl, absorptance if switched
		Real64 ReflGlSh;
		Real64 AbsGlSh;
		Real64 TransDiffGl; // Diffuse solar transmittance
		Real64 TransDiffGlSh; // Diffuse solar transmittance, switched construction
		int FrDivNum; // Frame/divider number
		Real64 FrArea; // Frame, divider area (m2)
		Real64 DivArea;
		Real64 FrWidth; // Frame, divider width (m)
		Real64 DivWidth;
		Real64 FrProjOut; // Frame, divider outside projection (m)
		Real64 DivProjOut;
		Real64 FrProjIn; // Frame, divider outside projection (m)
		Real64 DivProjIn;
		Real64 PhiWin; // Altitude and azimuth angle of outward window normal (radians)
		Real64 ThWin;
		Real64 PhiSun; // Altitude and azimuth angle of sun (radians)
		Real64 ThSun;
		Real64 CosInc; // Cosine of incidence angle of beam solar on glass
		Real64 CosIncAngHorProj; // Cosine of incidence angle of sun on horizontal faces of a frame or
		//   divider projection
		Real64 CosIncAngVertProj; // Cosine of incidence angle of sun on vertical faces of a frame or
		//   divider projection
		Real64 FracSunLit; // Fraction of window sunlit this time step
		Real64 BeamFaceInc; // Beam solar incident window plane this time step (W/m2)
		Real64 DifSolarFaceInc; // Diffuse solar incident on window plane this time step (W/m2)
		Real64 FrIncSolarOut; // Total solar incident on outside offrame including solar
		//   on frame projection (W/m2)
		Real64 FrIncSolarIn; // Total solar incident on inside offrame including solar
		//   on frame projection (W/m2)

		Real64 DivIncSolarOutBm; // Beam solar incident on outside of divider including beam on divider
		//   projection (W/m2)
		Real64 DivIncSolarOutDif; // Diffuse solar incident on outside of divider including diffuse on divider
		//   projection (W/m2)
		Real64 DivIncSolarInBm; // Beam solar incident on inside of divider including beam on divider
		//   projection (W/m2)
		Real64 DivIncSolarInDif; // Diffuse solar incident on inside of divider including diffuse on divider
		//   projection (W/m2)
		Real64 BeamFrHorFaceInc; // Beam solar on frame's horizontal projection faces (W/m2)
		Real64 BeamFrVertFaceInc; // Beam solar on frame's vertical projection faces (W/m2)
		Real64 BeamDivHorFaceInc; // Beam solar on divider's horizontal outside projection faces (W/m2)
		Real64 BeamDivVertFaceInc; // Beam solar on divider's vertical outside projection faces (W/m2)
		Real64 BeamDivHorFaceIncIn; // Beam solar on divider's horizontal inside projection faces (W/m2)
		Real64 BeamDivVertFaceIncIn; // Beam solar on divider's vertical inside projection faces (W/m2)
		int BlNum; // Blind number
		Real64 ProfAng; // Solar profile angle (rad)
		Real64 SlatAng; // Slat angle (rad)
		Real64 TBlBmBm; // Blind beam-beam solar transmittance
		Real64 TBlBmDif; // Blind diffuse-diffuse solar transmittance
		Real64 ACosTlt; // Absolute value of cosine of surface tilt angle
		Real64 AbsDiffBlindGnd; // System blind front ground diffuse solar absorptance at a particular slat angle
		Real64 AbsDiffBlindSky; // System blind front sky diffuse solar absorptance at a particular slat angle
		Real64 AbsDiffGlassLayGnd; // System glass layer ground diffuse solar absorptance with blind on
		Real64 AbsDiffGlassLaySky; // System glass layer sky diffuse solar absorptance with blind on
		int OtherZoneNum; // Adjacent zone number
		int SurfSolAbs; // Pointer to scheduled surface gains object for fenestration systems
		int SurfSolIncPtr; // Pointer to schedule surface gain object for interior side of the surface

		// Always initialize the shortwave quantities

		QRadSWOutAbs = 0.0;
		QRadSWInAbs = 0.0;
		QRadSWLightsInAbs = 0.0;
		QRadSWwinAbs = 0.0;
		InitialDifSolInAbs = 0.0;
		InitialDifSolInTrans = 0.0;
		InitialDifSolwinAbs = 0.0;
		InitialZoneDifSolReflW = 0.0;
		QRadSWwinAbsTot = 0.0;
		QRadSWwinAbsLayer = 0.0;
		SWwinAbsTotalReport = 0.0;
		InitialDifSolInAbsReport = 0.0;
		InitialDifSolInTransReport = 0.0;
		SWInAbsTotalReport = 0.0;
		SWOutAbsTotalReport = 0.0;
		SWOutAbsEnergyReport = 0.0;
		QRadSWOutIncident = 0.0;
		QRadSWOutIncidentBeam = 0.0;
		BmIncInsSurfIntensRep = 0.0;
		BmIncInsSurfAmountRep = 0.0;
		IntBmIncInsSurfIntensRep = 0.0;
		IntBmIncInsSurfAmountRep = 0.0;
		QRadSWOutIncidentSkyDiffuse = 0.0;
		QRadSWOutIncidentGndDiffuse = 0.0;
		QRadSWOutIncBmToDiffReflGnd = 0.0;
		QRadSWOutIncSkyDiffReflGnd = 0.0;
		QRadSWOutIncBmToBmReflObs = 0.0;
		QRadSWOutIncBmToDiffReflObs = 0.0;
		QRadSWOutIncSkyDiffReflObs = 0.0;
		CosIncidenceAngle = 0.0;
		BSDFBeamDirectionRep = 0;
		BSDFBeamThetaRep = 0.0;
		BSDFBeamPhiRep = 0.0;
		OpaqSurfInsFaceBeamSolAbsorbed = 0.0;

		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) { // Faster "inline" than calling SurfaceWindow( SurfNum ).InitSolarHeatGains()
			auto & window( SurfaceWindow( SurfNum ) );
			window.FrameQRadOutAbs = 0.0;
			window.FrameQRadInAbs = 0.0;
			window.DividerQRadOutAbs = 0.0;
			window.DividerQRadInAbs = 0.0;
			window.ExtBeamAbsByShade = 0.0;
			window.ExtDiffAbsByShade = 0.0;
			window.IntBeamAbsByShade = 0.0;
			window.IntSWAbsByShade = 0.0;
			window.InitialDifSolAbsByShade = 0.0;
			window.IntLWAbsByShade = 0.0;
			window.ConvHeatFlowNatural = 0.0;
			window.ConvHeatGainToZoneAir = 0.0;
			window.RetHeatGainToZoneAir = 0.0;
			window.DividerConduction = 0.0;
			window.BlTsolBmBm = 0.0;
			window.BlTsolBmDif = 0.0;
			window.BlTsolDifDif = 0.0;
			window.BlGlSysTsolBmBm = 0.0;
			window.BlGlSysTsolDifDif = 0.0;
			window.ScTsolBmBm = 0.0;
			window.ScTsolBmDif = 0.0;
			window.ScTsolDifDif = 0.0;
			window.ScGlSysTsolBmBm = 0.0;
			window.ScGlSysTsolDifDif = 0.0;
			window.GlTsolBmBm = 0.0;
			window.GlTsolBmDif = 0.0;
			window.GlTsolDifDif = 0.0;
			window.BmSolTransThruIntWinRep = 0.0;
			window.BmSolAbsdOutsReveal = 0.0;
			window.BmSolRefldOutsRevealReport = 0.0;
			window.BmSolAbsdInsReveal = 0.0;
			window.BmSolRefldInsReveal = 0.0;
			window.BmSolRefldInsRevealReport = 0.0;
			window.OutsRevealDiffOntoGlazing = 0.0;
			window.InsRevealDiffOntoGlazing = 0.0;
			window.InsRevealDiffIntoZone = 0.0;
			window.OutsRevealDiffOntoFrame = 0.0;
			window.InsRevealDiffOntoFrame = 0.0;
			window.InsRevealDiffOntoGlazingReport = 0.0;
			window.InsRevealDiffIntoZoneReport = 0.0;
			window.InsRevealDiffOntoFrameReport = 0.0;
			window.BmSolAbsdInsRevealReport = 0.0;
			window.BmSolTransThruIntWinRepEnergy = 0.0;
			window.BmSolRefldOutsRevealRepEnergy = 0.0;
			window.BmSolRefldInsRevealRepEnergy = 0.0;
			window.ProfileAngHor = 0.0;
			window.ProfileAngVert = 0.0;
			window.SkySolarInc = 0.0;
			window.GndSolarInc = 0.0;
		}

		WinHeatGain = 0.0;
		WinHeatGainRep = 0.0;
		WinHeatLossRep = 0.0;
		WinGainConvGlazToZoneRep = 0.0;
		WinGainIRGlazToZoneRep = 0.0;
		WinLossSWZoneToOutWinRep = 0.0;
		WinGainFrameDividerToZoneRep = 0.0;
		WinGainConvGlazShadGapToZoneRep = 0.0;
		WinGainConvShadeToZoneRep = 0.0;
		WinGainIRShadeToZoneRep = 0.0;
		OtherConvGainInsideFaceToZoneRep = 0.0;
		WinGapConvHtFlowRep = 0.0;
		OpaqSurfInsFaceCondGainRep = 0.0;
		OpaqSurfInsFaceCondLossRep = 0.0;
		ZoneWinHeatGain = 0.0;
		ZoneWinHeatGainRep = 0.0;
		ZoneWinHeatLossRep = 0.0;
		ZoneOpaqSurfInsFaceCond = 0.0;
		ZoneOpaqSurfInsFaceCondGainRep = 0.0;
		ZoneOpaqSurfInsFaceCondLossRep = 0.0;
		ZoneOpaqSurfExtFaceCond = 0.0;
		ZoneOpaqSurfExtFaceCondGainRep = 0.0;
		ZoneOpaqSurfExtFaceCondLossRep = 0.0;
		WinShadingAbsorbedSolar = 0.0;
		WinSysSolTransmittance = 0.0;
		WinSysSolReflectance = 0.0;
		WinSysSolAbsorptance = 0.0;
		if ( NumOfTDDPipes > 0 ) {
			for ( auto & e : TDDPipe ) {
				e.HeatGain = 0.0;
				e.HeatLoss = 0.0;
			}
		}
		BmIncInsSurfIntensRep = 0.0;
		BmIncInsSurfAmountRep = 0.0;
		IntBmIncInsSurfIntensRep = 0.0;
		IntBmIncInsSurfAmountRep = 0.0;
		//energy
		QRadSWwinAbsTotEnergy = 0.0;
		BmIncInsSurfAmountRepEnergy = 0.0;
		IntBmIncInsSurfAmountRepEnergy = 0.0;
		WinHeatGainRepEnergy = 0.0;
		WinHeatLossRepEnergy = 0.0;
		WinGapConvHtFlowRepEnergy = 0.0;
		ZoneWinHeatGainRepEnergy = 0.0;
		ZoneWinHeatLossRepEnergy = 0.0;
		ZnOpqSurfInsFaceCondGnRepEnrg = 0.0;
		ZnOpqSurfInsFaceCondLsRepEnrg = 0.0;
		ZnOpqSurfExtFaceCondGnRepEnrg = 0.0;
		ZnOpqSurfExtFaceCondLsRepEnrg = 0.0;
		WinShadingAbsorbedSolarEnergy = 0.0;

		if ( ! SunIsUp || ( BeamSolarRad + GndSolarRad + DifSolarRad <= 0.0 ) ) { // Sun is down

			QD = 0.0;
			QDforDaylight = 0.0;
			QC = 0.0;
			QDV = 0.0;
			//unused    QBV = 0.0
			ZoneTransSolar = 0.0;
			ZoneBmSolFrExtWinsRep = 0.0;
			ZoneBmSolFrIntWinsRep = 0.0;
			ZoneDifSolFrExtWinsRep = 0.0;
			ZoneDifSolFrIntWinsRep = 0.0;
			WinTransSolar = 0.0;
			WinBmSolar = 0.0;
			WinBmBmSolar = 0.0;
			WinBmDifSolar = 0.0;

			WinDifSolar = 0.0;
			WinDirSolTransAtIncAngle = 0.0;
			//energy
			ZoneTransSolarEnergy = 0.0;
			ZoneBmSolFrExtWinsRepEnergy = 0.0;
			ZoneBmSolFrIntWinsRepEnergy = 0.0;
			ZoneDifSolFrExtWinsRepEnergy = 0.0;
			ZoneDifSolFrIntWinsRepEnergy = 0.0;
			WinTransSolarEnergy = 0.0;
			WinBmSolarEnergy = 0.0;
			WinBmBmSolarEnergy = 0.0;
			WinBmDifSolarEnergy = 0.0;

			WinDifSolarEnergy = 0.0;

			if ( NumOfTDDPipes > 0 ) {
				for ( auto & e : TDDPipe ) {
					e.TransSolBeam = 0.0;
					e.TransSolDiff = 0.0;
					e.TransVisBeam = 0.0;
					e.TransVisDiff = 0.0;
					e.TransmittedSolar = 0.0;
				}
			}

			if ( CalcSolRefl ) {
				BmToBmReflFacObs = 0.0;
				BmToDiffReflFacObs = 0.0;
				BmToDiffReflFacGnd = 0.0;
			}

		} else { // Sun is up, calculate solar quantities

			assert( equal_dimensions( ReflFacBmToBmSolObs, ReflFacBmToDiffSolObs ) ); // For linear indexing
			assert( equal_dimensions( ReflFacBmToBmSolObs, ReflFacBmToDiffSolGnd ) ); // For linear indexing
			Array2D< Real64 >::size_type lSH( CalcSolRefl ? ReflFacBmToBmSolObs.index( HourOfDay, 1 ) : 0u );
			Array2D< Real64 >::size_type lSP( CalcSolRefl ? ReflFacBmToBmSolObs.index( PreviousHour, 1 ) : 0u );
			for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
				SurfaceWindow( SurfNum ).SkySolarInc = DifSolarRad * AnisoSkyMult( SurfNum );
				SurfaceWindow( SurfNum ).GndSolarInc = GndSolarRad * Surface( SurfNum ).ViewFactorGround;
				// For Complex Fenestrations:
				SurfaceWindow( SurfNum ).SkyGndSolarInc = SurfaceWindow( SurfNum ).GndSolarInc;
				SurfaceWindow( SurfNum ).BmGndSolarInc = 0.0;
				if ( CalcSolRefl ) { //Tuned Linear indexing // [ lSH ] == ( HourOfDay, SurfNum ) // [ lSP ] == ( PreviousHour, SurfNum )

					// For Complex Fenestrations:
					SurfaceWindow( SurfNum ).SkyGndSolarInc = DifSolarRad * GndReflectance * ReflFacSkySolGnd( SurfNum );
					SurfaceWindow( SurfNum ).BmGndSolarInc = BeamSolarRad * SOLCOS( 3 ) * GndReflectance * BmToDiffReflFacGnd( SurfNum );
					BmToBmReflFacObs( SurfNum ) = WeightNow * ReflFacBmToBmSolObs[ lSH ] + WeightPreviousHour * ReflFacBmToBmSolObs[ lSP ];
					BmToDiffReflFacObs( SurfNum ) = WeightNow * ReflFacBmToDiffSolObs[ lSH ] + WeightPreviousHour * ReflFacBmToDiffSolObs[ lSP ];
					BmToDiffReflFacGnd( SurfNum ) = WeightNow * ReflFacBmToDiffSolGnd[ lSH ] + WeightPreviousHour * ReflFacBmToDiffSolGnd[ lSP ];

					// TH2 CR 9056
					SurfaceWindow( SurfNum ).SkySolarInc += BeamSolarRad * ( BmToBmReflFacObs( SurfNum ) + BmToDiffReflFacObs( SurfNum ) ) + DifSolarRad * ReflFacSkySolObs( SurfNum );
					SurfaceWindow( SurfNum ).GndSolarInc = BeamSolarRad * SOLCOS( 3 ) * GndReflectance * BmToDiffReflFacGnd( SurfNum ) + DifSolarRad * GndReflectance * ReflFacSkySolGnd( SurfNum );

					++lSH; ++lSP;
				}
			}

			CalcWindowProfileAngles();

			if ( CalcWindowRevealReflection ) CalcBeamSolarOnWinRevealSurface();

			CalcInteriorSolarDistribution();

			for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {

				// TH 3/24/2010 - QBV is not used!
				//unused      QBV(ZoneNum) = (CBZone(ZoneNum) + DBZone(ZoneNum))*BeamSolarRad

				// RJH 08/30/07 - QDV does not seem to ever be used. NOT USED!
				//QDV(ZoneNum) = DSZone(ZoneNum)*DifSolarRad &
				//                +DGZone(ZoneNum)*GndSolarRad

				// Original QD calc used only for QSDifSol and daylighting calcs
				//QDforDaylight(ZoneNum)  = DBZone(ZoneNum)*BeamSolarRad  &
				//                          +DSZone(ZoneNum)*DifSolarRad  &
				//                          +DGZone(ZoneNum)*GndSolarRad

				// TH 3/23/2010. CR 7869 and CR 7999. QDforDaylight in W
				//  Beam from interior windows (DBZoneIntWin) reflected from floor is counted in DayltgInterReflIllFrIntWins,
				//  DBZone needs to subtract this part since it is already counted in DBZone.
				//  Use InitialZoneDifSolReflW (Rob's previous work) as it better counts initial distribution of
				//   diffuse solar rather than using weighted area*absorptance
				QDforDaylight( ZoneNum ) = ( DBZone( ZoneNum ) - DBZoneIntWin( ZoneNum ) ) * BeamSolarRad + DBZoneSSG( ZoneNum ) + InitialZoneDifSolReflW( ZoneNum );

				// RJH 08/30/07 - Substitute InitialZoneDifSolReflW(ZoneNum) for DSZone and DGZone here
				// to exclude diffuse solar now absorbed/transmitted in CalcWinTransDifSolInitialDistribution
				// DBZone(ZoneNum) is Diffuse Solar from beam reflected from interior surfaces
				// and transmitted through interior windows
				// DBZone is a factor that when multiplied by BeamSolarRad [W/m2] gives Watts
				//QD(ZoneNum)  = DBZone(ZoneNum)*BeamSolarRad  &
				//                +DSZone(ZoneNum)*DifSolarRad  &
				//                +DGZone(ZoneNum)*GndSolarRad
				QD( ZoneNum ) = DBZone( ZoneNum ) * BeamSolarRad + DBZoneSSG( ZoneNum ) + InitialZoneDifSolReflW( ZoneNum );
			}

			// Flux of diffuse solar in each zone

			QSDifSol = 0.0;
			for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
				QSDifSol( ZoneNum ) = QDforDaylight( ZoneNum );
			}

			if ( InterZoneWindow ) {
				for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
					if ( RecDifShortFromZ( ZoneNum ) ) {
						Real64 QSDifSol_sum( 0.0 ); // Accumulator
						auto lZone( FractDifShortZtoZ.index( ZoneNum, 1 ) ); //Tuned Linear indexing
						for ( OtherZoneNum = 1; OtherZoneNum <= NumOfZones; ++OtherZoneNum, ++lZone ) {
							if ( ( OtherZoneNum != ZoneNum ) && ( RecDifShortFromZ( OtherZoneNum ) ) ) {
								QSDifSol_sum += FractDifShortZtoZ[ lZone ] * QDforDaylight( OtherZoneNum ); // [ lZone ] == ( ZoneNum, OtherZoneNum )
							}
						}
						QSDifSol( ZoneNum ) += QSDifSol_sum;
					}
				}
			}

			for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
				QSDifSol( ZoneNum ) *= FractDifShortZtoZ( ZoneNum, ZoneNum ) * VMULT( ZoneNum );
			}

			//    RJH - 09-12-07 commented out report varariable calcs here since they refer to old distribution method
			//    DO SurfNum = 1, TotSurfaces
			//      IF (.NOT. Surface(SurfNum)%HeatTransSurf) CYCLE
			//!!! Following may need to be removed or changed when shelves are considered in adjacent reflection calculations
			//      IF (Surface(SurfNum)%Class == SurfaceClass_Shading) CYCLE
			//      ZoneNum = Surface(SurfNum)%Zone
			// Diffuse solar entering zone through exterior windows is assumed to be uniformly
			// distributed on inside face of surfaces of zone
			//      DifIncInsSurfIntensRep(SurfNum) = (DSZone(ZoneNum)*DifSolarRad + DGZone(ZoneNum)*GndSolarRad) /  &
			//        Zone(ZoneNum)%TotalSurfArea
			//      DifIncInsSurfAmountRep(SurfNum) = (Surface(SurfNum)%Area + SurfaceWindow(SurfNum)%DividerArea) *  &
			//        DifIncInsSurfIntensRep(SurfNum)
			//      DifIncInsSurfAmountRepEnergy(SurfNum) = DifIncInsSurfAmountRep(SurfNum) * TimeStepZoneSec
			//    END DO

			for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
				if ( Surface( SurfNum ).HeatTransSurf ) {
					ConstrNum = Surface( SurfNum ).Construction;
					if ( SurfaceWindow( SurfNum ).StormWinFlag == 1 ) ConstrNum = Surface( SurfNum ).StormWinConstruction;
				} else {
					ConstrNum = 0;
				}
				ShelfNum = Surface( SurfNum ).Shelf;
				if ( ShelfNum > 0 ) {
					InShelfSurf = Shelf( ShelfNum ).InSurf; // Inside daylighting shelf present if > 0
					OutShelfSurf = Shelf( ShelfNum ).OutSurf; // Outside daylighting shelf present if > 0
				} else {
					InShelfSurf = 0;
					OutShelfSurf = 0;
				}

				if ( Surface( SurfNum ).ExtSolar || SurfaceWindow( SurfNum ).OriginalClass == SurfaceClass_TDD_Diffuser ) {

					if ( SurfaceWindow( SurfNum ).OriginalClass == SurfaceClass_TDD_Diffuser ) {
						PipeNum = FindTDDPipe( SurfNum );
						SurfNum2 = TDDPipe( PipeNum ).Dome;

						CosInc = CosIncAng( TimeStep, HourOfDay, SurfNum2 );

						// Reconstruct the beam, sky, and ground radiation transmittance of just the TDD:DOME and TDD pipe
						// by dividing out diffuse solar transmittance of TDD:DIFFUSER
						BeamSolar = BeamSolarRad * TransTDD( PipeNum, CosInc, SolarBeam ) / Construct( ConstrNum ).TransDiff;

						SkySolarInc = DifSolarRad * AnisoSkyMult( SurfNum2 ) * TransTDD( PipeNum, CosInc, SolarAniso ) / Construct( ConstrNum ).TransDiff;

						GndSolarInc = GndSolarRad * Surface( SurfNum2 ).ViewFactorGround * TDDPipe( PipeNum ).TransSolIso / Construct( ConstrNum ).TransDiff;

					} else if ( OutShelfSurf > 0 ) { // Outside daylighting shelf
						SurfNum2 = SurfNum;

						CosInc = CosIncAng( TimeStep, HourOfDay, SurfNum );

						BeamSolar = BeamSolarRad;
						SkySolarInc = DifSolarRad * AnisoSkyMult( SurfNum );

						ShelfSolarRad = ( BeamSolarRad * SunlitFrac( TimeStep, HourOfDay, OutShelfSurf ) * CosIncAng( TimeStep, HourOfDay, OutShelfSurf ) + DifSolarRad * AnisoSkyMult( OutShelfSurf ) ) * Shelf( ShelfNum ).OutReflectSol;

						// Add all reflected solar from the outside shelf to the ground solar
						// NOTE:  If the shelf blocks part of the view to the ground, the user must reduce the ground view factor!!
						GndSolarInc = GndSolarRad * Surface( SurfNum ).ViewFactorGround + ShelfSolarRad * Shelf( ShelfNum ).ViewFactor;

					} else { // Regular surface
						SurfNum2 = SurfNum;
						CosInc = CosIncAng( TimeStep, HourOfDay, SurfNum );
						BeamSolar = BeamSolarRad;
						SkySolarInc = SurfaceWindow( SurfNum ).SkySolarInc;
						GndSolarInc = SurfaceWindow( SurfNum ).GndSolarInc;
					}

					// Cosine of incidence angle and solar incident on outside of surface, for reporting
					CosIncidenceAngle( SurfNum ) = CosInc;

					// Report variables for various incident solar quantities

					// Incident direct (unreflected) beam
					QRadSWOutIncidentBeam( SurfNum ) = BeamSolar * SunlitFrac( TimeStep, HourOfDay, SurfNum2 ) * CosInc; // NOTE: SurfNum2
					// Incident (unreflected) diffuse solar from sky -- TDD_Diffuser calculated differently
					if ( SurfaceWindow( SurfNum ).OriginalClass == SurfaceClass_TDD_Diffuser ) {
						QRadSWOutIncidentSkyDiffuse( SurfNum ) = SkySolarInc;
					} else {
						QRadSWOutIncidentSkyDiffuse( SurfNum ) = DifSolarRad * AnisoSkyMult( SurfNum );
					}
					// Incident diffuse solar from sky diffuse reflected from ground plus beam reflected from ground
					QRadSWOutIncidentGndDiffuse( SurfNum ) = GndSolarInc;
					// Incident diffuse solar from beam-to-diffuse reflection from ground
					if ( CalcSolRefl ) {
						QRadSWOutIncBmToDiffReflGnd( SurfNum ) = BeamSolarRad * SOLCOS( 3 ) * GndReflectance * BmToDiffReflFacGnd( SurfNum );
					} else {
						QRadSWOutIncBmToDiffReflGnd( SurfNum ) = BeamSolarRad * SOLCOS( 3 ) * GndReflectance * Surface( SurfNum ).ViewFactorGround;
					}
					// Incident diffuse solar from sky diffuse reflection from ground
					if ( CalcSolRefl ) {
						QRadSWOutIncSkyDiffReflGnd( SurfNum ) = DifSolarRad * GndReflectance * ReflFacSkySolGnd( SurfNum );
					} else {
						QRadSWOutIncSkyDiffReflGnd( SurfNum ) = DifSolarRad * GndReflectance * Surface( SurfNum ).ViewFactorGround;
					}
					// Total incident solar. Beam and sky reflection from obstructions, if calculated, is included
					// in SkySolarInc.
					// QRadSWOutIncident(SurfNum) = QRadSWOutIncidentBeam(SurfNum) + SkySolarInc + GndSolarInc

					// TH2 CR 9056
					QRadSWOutIncident( SurfNum ) = QRadSWOutIncidentBeam( SurfNum ) + QRadSWOutIncidentSkyDiffuse( SurfNum ) + QRadSWOutIncBmToDiffReflGnd( SurfNum ) + QRadSWOutIncSkyDiffReflGnd( SurfNum );

					if ( CalcSolRefl ) {
						// Incident beam solar from beam-to-beam (specular) reflection from obstructions
						QRadSWOutIncBmToBmReflObs( SurfNum ) = BmToBmReflFacObs( SurfNum ) * BeamSolarRad;
						// Incident diffuse solar from beam-to-diffuse reflection from obstructions
						QRadSWOutIncBmToDiffReflObs( SurfNum ) = BmToDiffReflFacObs( SurfNum ) * BeamSolarRad;
						// Incident diffuse solar from sky diffuse reflection from obstructions
						QRadSWOutIncSkyDiffReflObs( SurfNum ) = DifSolarRad * ReflFacSkySolObs( SurfNum );
						// TH2 CR 9056: Add reflections from obstructions to the total incident
						QRadSWOutIncident( SurfNum ) += QRadSWOutIncBmToBmReflObs( SurfNum ) + QRadSWOutIncBmToDiffReflObs( SurfNum ) + QRadSWOutIncSkyDiffReflObs( SurfNum );
					}

					if ( Surface( SurfNum ).HeatTransSurf ) { // Exclude special shading surfaces which required QRadSWOut calculations above

						RoughIndexMovInsul = 0;

						if ( Surface( SurfNum ).MaterialMovInsulExt > 0 ) EvalOutsideMovableInsulation( SurfNum, HMovInsul, RoughIndexMovInsul, AbsExt );

						if ( RoughIndexMovInsul <= 0 ) { // No movable insulation present

							if ( Construct( ConstrNum ).TransDiff <= 0.0 ) { // Opaque surface

								AbsExt = Material( Construct( ConstrNum ).LayerPoint( 1 ) ).AbsorpSolar;

							} else { // Exterior window

								if ( SurfaceWindow( SurfNum ).WindowModelType != WindowBSDFModel && SurfaceWindow( SurfNum ).WindowModelType != WindowEQLModel ) {
									TotGlassLay = Construct( ConstrNum ).TotGlassLayers;
									for ( Lay = 1; Lay <= TotGlassLay; ++Lay ) {
										AbsDiffWin( Lay ) = Construct( ConstrNum ).AbsDiff( Lay );
									}

									ShadeFlag = SurfaceWindow( SurfNum ).ShadingFlag;

									if ( ShadeFlag > 0 ) { // Shaded window
										ConstrNumSh = Surface( SurfNum ).ShadedConstruction;
										if ( SurfaceWindow( SurfNum ).StormWinFlag == 1 ) ConstrNumSh = Surface( SurfNum ).StormWinShadedConstruction;

										if ( ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == BGShadeOn || ShadeFlag == ExtScreenOn ) { // Shade/screen on

											for ( Lay = 1; Lay <= TotGlassLay; ++Lay ) {
												AbsDiffWin( Lay ) = Construct( ConstrNumSh ).AbsDiff( Lay );
											}
											SurfaceWindow( SurfNum ).ExtDiffAbsByShade = Construct( ConstrNumSh ).AbsDiffShade * ( SkySolarInc + GndSolarInc );
										}

										if ( ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == BGBlindOn ) { // Blind on
											for ( Lay = 1; Lay <= TotGlassLay; ++Lay ) {
												AbsDiffWin( Lay ) = InterpSlatAng( SurfaceWindow( SurfNum ).SlatAngThisTS, SurfaceWindow( SurfNum ).MovableSlats, Construct( ConstrNumSh ).BlAbsDiff( {1,MaxSlatAngs}, Lay ) );
												AbsDiffWinGnd( Lay ) = InterpSlatAng( SurfaceWindow( SurfNum ).SlatAngThisTS, SurfaceWindow( SurfNum ).MovableSlats, Construct( ConstrNumSh ).BlAbsDiffGnd( {1,MaxSlatAngs}, Lay ) );
												AbsDiffWinSky( Lay ) = InterpSlatAng( SurfaceWindow( SurfNum ).SlatAngThisTS, SurfaceWindow( SurfNum ).MovableSlats, Construct( ConstrNumSh ).BlAbsDiffSky( {1,MaxSlatAngs}, Lay ) );
											}
											SurfaceWindow( SurfNum ).ExtDiffAbsByShade = InterpSlatAng( SurfaceWindow( SurfNum ).SlatAngThisTS, SurfaceWindow( SurfNum ).MovableSlats, Construct( ConstrNumSh ).AbsDiffBlind ) * ( SkySolarInc + GndSolarInc );
											if ( Blind( SurfaceWindow( SurfNum ).BlindNumber ).SlatOrientation == Horizontal ) {
												ACosTlt = std::abs( Surface( SurfNum ).CosTilt );
												AbsDiffBlindGnd = InterpSlatAng( SurfaceWindow( SurfNum ).SlatAngThisTS, SurfaceWindow( SurfNum ).MovableSlats, Construct( ConstrNumSh ).AbsDiffBlindGnd );
												AbsDiffBlindSky = InterpSlatAng( SurfaceWindow( SurfNum ).SlatAngThisTS, SurfaceWindow( SurfNum ).MovableSlats, Construct( ConstrNumSh ).AbsDiffBlindSky );
												SurfaceWindow( SurfNum ).ExtDiffAbsByShade = SkySolarInc * ( 0.5 * ACosTlt * AbsDiffBlindGnd + ( 1.0 - 0.5 * ACosTlt ) * AbsDiffBlindSky ) + GndSolarInc * ( ( 1.0 - 0.5 * ACosTlt ) * AbsDiffBlindGnd + 0.5 * ACosTlt * AbsDiffBlindSky );
											}
										}

										// Correct for shadowing of divider onto interior shading device (note that dividers are
										// not allowed in windows with between-glass shade/blind)

										if ( ( ShadeFlag == IntShadeOn || ShadeFlag == IntBlindOn ) && SurfaceWindow( SurfNum ).DividerArea > 0.0 ) SurfaceWindow( SurfNum ).ExtDiffAbsByShade *= SurfaceWindow( SurfNum ).GlazedFrac;

										if ( ShadeFlag == SwitchableGlazing ) { // Switchable glazing
											SwitchFac = SurfaceWindow( SurfNum ).SwitchingFactor;
											for ( Lay = 1; Lay <= TotGlassLay; ++Lay ) {
												AbsDiffWin( Lay ) = InterpSw( SwitchFac, AbsDiffWin( Lay ), Construct( ConstrNumSh ).AbsDiff( Lay ) );
											}
										}

									} // End of check if window has shading device on

									QRadSWwinAbsTot( SurfNum ) = 0.0;
									for ( Lay = 1; Lay <= TotGlassLay; ++Lay ) {
										QRadSWwinAbs( Lay, SurfNum ) = AbsDiffWin( Lay ) * ( SkySolarInc + GndSolarInc ) + AWinSurf( Lay, SurfNum ) * BeamSolar; // AWinSurf is from InteriorSolarDistribution
										if ( ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == BGBlindOn ) {
											if ( Blind( SurfaceWindow( SurfNum ).BlindNumber ).SlatOrientation == Horizontal ) {
												AbsDiffGlassLayGnd = InterpSlatAng( SurfaceWindow( SurfNum ).SlatAngThisTS, SurfaceWindow( SurfNum ).MovableSlats, Construct( ConstrNumSh ).BlAbsDiffGnd( {1,19}, Lay ) );
												AbsDiffGlassLaySky = InterpSlatAng( SurfaceWindow( SurfNum ).SlatAngThisTS, SurfaceWindow( SurfNum ).MovableSlats, Construct( ConstrNumSh ).BlAbsDiffSky( {1,19}, Lay ) );
												QRadSWwinAbs( Lay, SurfNum ) = SkySolarInc * ( 0.5 * ACosTlt * AbsDiffGlassLayGnd + ( 1.0 - 0.5 * ACosTlt ) * AbsDiffGlassLaySky ) + GndSolarInc * ( ( 1.0 - 0.5 * ACosTlt ) * AbsDiffGlassLayGnd + 0.5 * ACosTlt * AbsDiffGlassLaySky ) + AWinSurf( Lay, SurfNum ) * BeamSolar;
											}
										}

										// Total solar absorbed in solid layer (W), for reporting
										QRadSWwinAbsLayer( Lay, SurfNum ) = QRadSWwinAbs( Lay, SurfNum ) * Surface( SurfNum ).Area;

										// Total solar absorbed in all glass layers (W), for reporting
										QRadSWwinAbsTot( SurfNum ) += QRadSWwinAbsLayer( Lay, SurfNum );
									}
									QRadSWwinAbsTotEnergy( SurfNum ) = QRadSWwinAbsTot( SurfNum ) * TimeStepZoneSec;

								} else if ( SurfaceWindow( SurfNum ).WindowModelType == WindowBSDFModel ) {
									TotSolidLay = Construct( ConstrNum ).TotSolidLayers;
									CurrentState = SurfaceWindow( SurfNum ).ComplexFen.CurrentState;
									// Examine for schedule surface gain
									SurfSolAbs = WindowScheduledSolarAbs( SurfNum, ConstrNum );

									for ( Lay = 1; Lay <= TotSolidLay; ++Lay ) {
										if ( SurfSolAbs != 0 ) {
											AWinSurf( Lay, SurfNum ) = GetCurrentScheduleValue( FenLayAbsSSG( SurfSolAbs ).SchedPtrs( Lay ) );
											//ABWin(Lay) = AWinSurf(SurfNum,Lay)
											QRadSWwinAbs( Lay, SurfNum ) = AWinSurf( Lay, SurfNum );
										} else {
											// Several notes about this equation.  First part is accounting for duffuse solar radiation for the ground and
											// from the sky.  Second item (AWinSurf(SurfNum,Lay) * BeamSolar) is accounting for absorbed solar radiation
											// originating from beam on exterior side.  Third item (AWinCFOverlap(SurfNum,Lay)) is accounting for
											// absorptances from beam hitting back of the window which passes through rest of exterior windows
											QRadSWwinAbs( Lay, SurfNum ) = SurfaceWindow( SurfNum ).ComplexFen.State( CurrentState ).WinSkyFtAbs( Lay ) * SkySolarInc + SurfaceWindow( SurfNum ).ComplexFen.State( CurrentState ).WinSkyGndAbs( Lay ) * GndSolarInc + AWinSurf( Lay, SurfNum ) * BeamSolar + AWinCFOverlap( Lay, SurfNum ) * BeamSolar;
										}
										// Total solar absorbed in solid layer (W), for reporting
										QRadSWwinAbsLayer( Lay, SurfNum ) = QRadSWwinAbs( Lay, SurfNum ) * Surface( SurfNum ).Area;

										// Total solar absorbed in all glass layers (W), for reporting
										QRadSWwinAbsTot( SurfNum ) += QRadSWwinAbsLayer( Lay, SurfNum );
									}
									QRadSWwinAbsTotEnergy( SurfNum ) = QRadSWwinAbsTot( SurfNum ) * TimeStepZoneSec;

									ShadeFlag = SurfaceWindow( SurfNum ).ShadingFlag;

								} else if ( SurfaceWindow( SurfNum ).WindowModelType == WindowEQLModel ) {
									QRadSWwinAbsTot( SurfNum ) = 0.0;
									//EQLNum = Construct(Surface(SurfNum)%Construction)%EQLConsPtr
									TotSolidLay = CFS( Construct( Surface( SurfNum ).Construction ).EQLConsPtr ).NL;
									for ( Lay = 1; Lay <= TotSolidLay; ++Lay ) {
										// Absorbed window components include:
										// (1) beam solar radiation absorbed by all layers in the fenestration
										// (2) sky and ground reflected duffuse solar radiation absorbed by all layers
										// (3) diffuse short wave incident on the inside face of the fenestration.  The short wave internal sources
										//     include light, ...
										AbsDiffWin( Lay ) = Construct( ConstrNum ).AbsDiffFrontEQL( Lay );
										QRadSWwinAbs( Lay, SurfNum ) = AWinSurf( Lay, SurfNum ) * BeamSolar + AbsDiffWin( Lay ) * ( SkySolarInc + GndSolarInc );

										// Total solar absorbed in solid layer (W), for reporting
										QRadSWwinAbsLayer( Lay, SurfNum ) = QRadSWwinAbs( Lay, SurfNum ) * Surface( SurfNum ).Area;

										// Total solar absorbed in all glass layers (W), for reporting
										QRadSWwinAbsTot( SurfNum ) += QRadSWwinAbsLayer( Lay, SurfNum );
									}
									QRadSWwinAbsTotEnergy( SurfNum ) = QRadSWwinAbsTot( SurfNum ) * TimeStepZoneSec;
								} // IF (SurfaceWindow(SurfNum)%WindowModelType /= WindowBSDFModel) THEN

								// Solar absorbed by window frame and dividers
								FrDivNum = Surface( SurfNum ).FrameDivider;
								FrArea = SurfaceWindow( SurfNum ).FrameArea;
								if ( FrDivNum > 0 ) {
									FrWidth = FrameDivider( FrDivNum ).FrameWidth;
									FrProjOut = FrameDivider( FrDivNum ).FrameProjectionOut;
									FrProjIn = FrameDivider( FrDivNum ).FrameProjectionIn;
									DivArea = SurfaceWindow( SurfNum ).DividerArea;
									DivWidth = FrameDivider( FrDivNum ).DividerWidth;
									DivProjOut = FrameDivider( FrDivNum ).DividerProjectionOut;
									DivProjIn = FrameDivider( FrDivNum ).DividerProjectionIn;
								} else {
									FrWidth = 0.0;
									FrProjOut = 0.0;
									FrProjIn = 0.0;
									DivArea = 0.0;
									DivWidth = 0.0;
									DivProjOut = 0.0;
									DivProjIn = 0.0;
								}
								CosIncAngHorProj = 0.0;
								CosIncAngVertProj = 0.0;
								if ( FrArea > 0.0 || DivArea > 0.0 ) {
									FracSunLit = SunlitFrac( TimeStep, HourOfDay, SurfNum );
									BeamFaceInc = BeamSolarRad * SunlitFrac( TimeStep, HourOfDay, SurfNum ) * CosInc;
									DifSolarFaceInc = SkySolarInc + GndSolarInc;
								} else {
									FracSunLit = 0.0;
								}
								if ( FracSunLit > 0.0 ) {
									if ( ( FrArea > 0.0 && ( FrProjOut > 0.0 || FrProjIn > 0.0 ) ) || ( DivArea > 0.0 && ( DivProjOut > 0.0 || DivProjIn > 0.0 ) ) ) {
										// Dot products used to calculate beam solar incident on faces of
										// frame and divider perpendicular to the glass surface.
										// Note that SOLCOS is the current timestep's solar direction cosines.
										//                  PhiWin = ASIN(WALCOS(3,SurfNum))
										PhiWin = std::asin( Surface( SurfNum ).OutNormVec( 3 ) );
										ThWin = std::atan2( Surface( SurfNum ).OutNormVec( 2 ), Surface( SurfNum ).OutNormVec( 1 ) );
										PhiSun = std::asin( SOLCOS( 3 ) );
										ThSun = std::atan2( SOLCOS( 2 ), SOLCOS( 1 ) );
										Real64 const cos_PhiWin( std::cos( PhiWin ) );
										Real64 const cos_PhiSun( std::cos( PhiSun ) );
										CosIncAngHorProj = std::abs( std::sin( PhiWin ) * cos_PhiSun * std::cos( ThWin - ThSun ) - cos_PhiWin * std::sin( PhiSun ) );
										CosIncAngVertProj = std::abs( cos_PhiWin * cos_PhiSun * std::sin( ThWin - ThSun ) );
									}
								}

								// Frame solar

								// (A window shade or blind, if present, is assumed to not shade the frame, so no special
								// treatment of frame solar needed if window has an exterior shade or blind.)
								if ( FrArea > 0.0 ) {
									FrIncSolarOut = BeamFaceInc;
									FrIncSolarIn = 0.0;
									TransDiffGl = 0.0;
									if ( FrProjOut > 0.0 || FrProjIn > 0.0 ) {
										BeamFrHorFaceInc = BeamSolarRad * CosIncAngHorProj * ( Surface( SurfNum ).Width - FrameDivider( FrDivNum ).VertDividers * DivWidth ) * FracSunLit / FrArea;
										BeamFrVertFaceInc = BeamSolarRad * CosIncAngVertProj * ( Surface( SurfNum ).Height - FrameDivider( FrDivNum ).HorDividers * DivWidth ) * FracSunLit / FrArea;
										// Beam solar on outside of frame
										FrIncSolarOut += ( BeamFrHorFaceInc + BeamFrVertFaceInc ) * FrProjOut;
										if ( FrProjIn > 0.0 ) {
											TransGl = POLYF( CosInc, Construct( ConstrNum ).TransSolBeamCoef );
											TransDiffGl = Construct( ConstrNum ).TransDiff;
											if ( ShadeFlag == SwitchableGlazing ) { // Switchable glazing
												TransGlSh = POLYF( CosInc, Construct( ConstrNumSh ).TransSolBeamCoef );
												TransGl = InterpSw( SwitchFac, TransGl, TransGlSh );
												TransDiffGlSh = Construct( ConstrNumSh ).TransDiff;
												TransDiffGl = InterpSw( SwitchFac, TransDiffGl, TransDiffGlSh );
											}
											// Beam solar on inside of frame
											FrIncSolarIn = ( BeamFrHorFaceInc + BeamFrVertFaceInc ) * FrProjIn * TransGl;
										}
									}
									// Beam plus diffuse solar on outside of frame
									FrIncSolarOut += DifSolarFaceInc * ( 1.0 + 0.5 * SurfaceWindow( SurfNum ).ProjCorrFrOut );
									SurfaceWindow( SurfNum ).FrameQRadOutAbs = FrIncSolarOut * SurfaceWindow( SurfNum ).FrameSolAbsorp;
									// Add diffuse from beam reflected from window outside reveal surfaces
									SurfaceWindow( SurfNum ).FrameQRadOutAbs += BeamSolarRad * SurfaceWindow( SurfNum ).OutsRevealDiffOntoFrame * SurfaceWindow( SurfNum ).FrameSolAbsorp;

									// Beam plus diffuse solar on inside of frame
									FrIncSolarIn += DifSolarFaceInc * TransDiffGl * 0.5 * SurfaceWindow( SurfNum ).ProjCorrFrIn;
									SurfaceWindow( SurfNum ).FrameQRadInAbs = FrIncSolarIn * SurfaceWindow( SurfNum ).FrameSolAbsorp;
									// Add diffuse from beam reflected from window inside reveal surfaces
									SurfaceWindow( SurfNum ).FrameQRadInAbs += BeamSolarRad * SurfaceWindow( SurfNum ).InsRevealDiffOntoFrame * SurfaceWindow( SurfNum ).FrameSolAbsorp;

								}

								// Divider solar

								// (An exterior shade or blind, when in place, is assumed to completely cover the divider.
								// Dividers are not allowed on windows with between-glass shade/blind so DivProjOut and
								// DivProjIn will be zero in this case.)

								if ( DivArea > 0.0 ) { // Solar absorbed by window divider
									DividerAbs = SurfaceWindow( SurfNum ).DividerSolAbsorp;
									if ( SurfaceWindow( SurfNum ).DividerType == Suspended ) {
										// Suspended (between-glass) divider; account for effect glass on outside of divider
										// (note that outside and inside projection for this type of divider are both zero)
										MatNumGl = Construct( ConstrNum ).LayerPoint( 1 );
										TransGl = Material( MatNumGl ).Trans;
										ReflGl = Material( MatNumGl ).ReflectSolBeamFront;
										AbsGl = 1.0 - TransGl - ReflGl;
										if ( ShadeFlag == SwitchableGlazing ) { // Switchable glazing
											MatNumGlSh = Construct( ConstrNumSh ).LayerPoint( 1 );
											TransGlSh = Material( MatNumGlSh ).Trans;
											ReflGlSh = Material( MatNumGlSh ).ReflectSolBeamFront;
											AbsGlSh = 1.0 - TransGlSh - ReflGlSh;
											TransGl = InterpSw( SwitchFac, TransGl, TransGlSh );
											ReflGl = InterpSw( SwitchFac, ReflGl, ReflGlSh );
											AbsGl = InterpSw( SwitchFac, AbsGl, AbsGlSh );
										}
										DividerRefl = 1.0 - DividerAbs;
										DividerAbs = AbsGl + TransGl * ( DividerAbs + DividerRefl * AbsGl ) / ( 1.0 - DividerRefl * ReflGl );
									}

									BeamDivHorFaceInc = 0.0;
									BeamDivVertFaceInc = 0.0;
									// Beam incident on horizontal and vertical projection faces of divider if no exterior shading
									if ( DivProjOut > 0.0 && ShadeFlag != ExtShadeOn && ShadeFlag != ExtBlindOn && ShadeFlag != ExtScreenOn ) {
										BeamDivHorFaceInc = BeamSolarRad * CosIncAngHorProj * FrameDivider( FrDivNum ).HorDividers * DivProjOut * ( Surface( SurfNum ).Width - FrameDivider( FrDivNum ).VertDividers * DivWidth ) * FracSunLit / DivArea;
										BeamDivVertFaceInc = BeamSolarRad * CosIncAngVertProj * FrameDivider( FrDivNum ).VertDividers * DivProjOut * ( Surface( SurfNum ).Height - FrameDivider( FrDivNum ).HorDividers * DivWidth ) * FracSunLit / DivArea;
									}
									DivIncSolarOutBm = 0.0;
									DivIncSolarOutDif = 0.0;
									DivIncSolarInBm = 0.0;
									DivIncSolarInDif = 0.0;
									if ( ShadeFlag != ExtShadeOn && ShadeFlag != ExtBlindOn && ShadeFlag != BGShadeOn && ShadeFlag != BGBlindOn && ShadeFlag != ExtScreenOn ) { // No exterior or between-glass shading
										DivIncSolarOutBm = BeamFaceInc + BeamDivHorFaceInc + BeamDivVertFaceInc;
										DivIncSolarOutDif = DifSolarFaceInc * ( 1.0 + SurfaceWindow( SurfNum ).ProjCorrDivOut );
										if ( DivProjIn > 0.0 ) {
											TransGl = POLYF( CosInc, Construct( ConstrNum ).TransSolBeamCoef );
											TransDiffGl = Construct( ConstrNum ).TransDiff;
											if ( ShadeFlag == SwitchableGlazing ) { // Switchable glazing
												TransGlSh = POLYF( CosInc, Construct( ConstrNumSh ).TransSolBeamCoef );
												TransGl = InterpSw( SwitchFac, TransGl, TransGlSh );
												TransDiffGlSh = Construct( ConstrNumSh ).TransDiff;
												TransDiffGl = InterpSw( SwitchFac, TransDiffGl, TransDiffGlSh );
											}
											// Beam plus diffuse solar on inside of divider
											BeamDivHorFaceIncIn = BeamSolarRad * CosIncAngHorProj * FrameDivider( FrDivNum ).HorDividers * DivProjIn * ( Surface( SurfNum ).Width - FrameDivider( FrDivNum ).VertDividers * DivWidth ) * FracSunLit / DivArea;
											BeamDivVertFaceIncIn = BeamSolarRad * CosIncAngVertProj * FrameDivider( FrDivNum ).VertDividers * DivProjIn * ( Surface( SurfNum ).Height - FrameDivider( FrDivNum ).HorDividers * DivWidth ) * FracSunLit / DivArea;
											DivIncSolarInBm = TransGl * ( BeamDivHorFaceIncIn + BeamDivVertFaceIncIn );
											DivIncSolarInDif = TransDiffGl * DifSolarFaceInc * SurfaceWindow( SurfNum ).ProjCorrDivIn;
										}
									} else { // Exterior shade, screen or blind present
										DivIncSolarOutBm = BeamFaceInc * ( 1.0 + SurfaceWindow( SurfNum ).ProjCorrDivOut );
										DivIncSolarOutDif = DifSolarFaceInc * ( 1.0 + SurfaceWindow( SurfNum ).ProjCorrDivOut );
										DivIncSolarInBm = BeamFaceInc * SurfaceWindow( SurfNum ).ProjCorrDivIn * Construct( ConstrNum ).TransDiff;
										DivIncSolarInDif = DifSolarFaceInc * SurfaceWindow( SurfNum ).ProjCorrDivIn * Construct( ConstrNum ).TransDiff;
									}

									if ( ShadeFlag != ExtShadeOn && ShadeFlag != ExtBlindOn && ShadeFlag != ExtScreenOn && ShadeFlag != BGShadeOn && ShadeFlag != BGBlindOn ) { // No exterior or between-glass shade, screen or blind
										SurfaceWindow( SurfNum ).DividerQRadOutAbs = DividerAbs * ( DivIncSolarOutBm + DivIncSolarOutDif );
										SurfaceWindow( SurfNum ).DividerQRadInAbs = DividerAbs * ( DivIncSolarInBm + DivIncSolarInDif );
										// Exterior shade, screen or blind
									} else if ( ShadeFlag == ExtShadeOn || ShadeFlag == ExtBlindOn || ShadeFlag == ExtScreenOn ) {

										if ( ShadeFlag == ExtBlindOn ) { // Exterior blind
											BlNum = SurfaceWindow( SurfNum ).BlindNumber;
											ProfileAngle( SurfNum, SOLCOS, Blind( BlNum ).SlatOrientation, ProfAng );
											SlatAng = SurfaceWindow( SurfNum ).SlatAngThisTS;
											TBlBmBm = BlindBeamBeamTrans( ProfAng, SlatAng, Blind( BlNum ).SlatWidth, Blind( BlNum ).SlatSeparation, Blind( BlNum ).SlatThickness );
											TBlBmDif = InterpProfSlatAng( ProfAng, SlatAng, SurfaceWindow( SurfNum ).MovableSlats, Blind( BlNum ).SolFrontBeamDiffTrans );
											SurfaceWindow( SurfNum ).DividerQRadOutAbs = DividerAbs * ( DivIncSolarOutBm * ( TBlBmBm + TBlBmDif ) + DivIncSolarOutDif * InterpSlatAng( SlatAng, SurfaceWindow( SurfNum ).MovableSlats, Blind( BlNum ).SolFrontDiffDiffTrans ) );
											SurfaceWindow( SurfNum ).DividerQRadInAbs = DividerAbs * ( DivIncSolarInBm * ( TBlBmBm + TBlBmDif ) + DivIncSolarInDif * InterpSlatAng( SlatAng, SurfaceWindow( SurfNum ).MovableSlats, Blind( BlNum ).SolFrontDiffDiffTrans ) );

										} else if ( ShadeFlag == ExtShadeOn ) { // Exterior shade
											SurfaceWindow( SurfNum ).DividerQRadOutAbs = DividerAbs * Material( Construct( ConstrNumSh ).LayerPoint( 1 ) ).Trans * ( DivIncSolarOutBm + DivIncSolarOutDif );
											SurfaceWindow( SurfNum ).DividerQRadInAbs = DividerAbs * Material( Construct( ConstrNumSh ).LayerPoint( 1 ) ).Trans * ( DivIncSolarInBm + DivIncSolarInDif );
										} else if ( ShadeFlag == ExtScreenOn ) { // Exterior screen
											SurfaceWindow( SurfNum ).DividerQRadOutAbs = DividerAbs * ( SurfaceScreens( SurfaceWindow( SurfNum ).ScreenNumber ).BmBmTrans + SurfaceScreens( SurfaceWindow( SurfNum ).ScreenNumber ).BmDifTrans ) * ( DivIncSolarOutBm + DivIncSolarOutDif );
											SurfaceWindow( SurfNum ).DividerQRadInAbs = DividerAbs * ( SurfaceScreens( SurfaceWindow( SurfNum ).ScreenNumber ).BmBmTrans + SurfaceScreens( SurfaceWindow( SurfNum ).ScreenNumber ).BmDifTrans ) * ( DivIncSolarInBm + DivIncSolarInDif );
										}
									}

								}

							}

						} // RoughIndexMovInsul <= 0, no movable insulation

						if ( Surface( SurfNum ).HeatTransSurf && Construct( ConstrNum ).TransDiff <= 0.0 ) { // Opaque heat transfer surface
							QRadSWOutAbs( SurfNum ) = AOSurf( SurfNum ) * BeamSolarRad + AbsExt * ( SkySolarInc + GndSolarInc );
							SWOutAbsTotalReport( SurfNum ) = QRadSWOutAbs( SurfNum ) * Surface( SurfNum ).Area;
							SWOutAbsEnergyReport( SurfNum ) = SWOutAbsTotalReport( SurfNum ) * TimeStepZoneSec;
						}
					} // Surface(SurfNum)%HeatTransSurf

				} // Surface(SurfNum)%ExtSolar

				if ( Surface( SurfNum ).HeatTransSurf && ConstrNum > 0 ) {
					SurfSolIncPtr = SurfaceScheduledSolarInc( SurfNum, ConstrNum );
					if ( SurfSolIncPtr == 0 ) {
						if ( Construct( ConstrNum ).TransDiff <= 0.0 ) { // Opaque surface
							QRadSWInAbs( SurfNum ) += AISurf( SurfNum ) * BeamSolarRad;
							if ( InShelfSurf > 0 ) { // Inside daylighting shelf
								// Shelf surface area is divided by 2 because only one side sees beam (Area was multiplied by 2 during init)
								OpaqSurfInsFaceBeamSolAbsorbed( SurfNum ) = AISurf( SurfNum ) * BeamSolarRad * ( 0.5 * Surface( SurfNum ).Area );
							} else { // Regular surface
								OpaqSurfInsFaceBeamSolAbsorbed( SurfNum ) = AISurf( SurfNum ) * BeamSolarRad * Surface( SurfNum ).Area;
							}
						}
					} else {
						QRadSWInAbs( SurfNum ) += AISurf( SurfNum );
					}
				}

			} // End of surface loop

		} // End of sun-up check

	}

	void
	InitIntSolarDistribution()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Anonymous
		//       DATE WRITTEN   July 1977
		//       MODIFIED       Oct 1999 (FW) to handle movable shades
		//                      May 2000 (FW) to handle window frame and dividers
		//                      May 2001 (FW) to handle window blinds
		//                      Jan 2002 (FW) mods for between-glass shade/blind
		//                      May 2006 (RR) to handle exterior window screens
		//       RE-ENGINEERED  Mar98 (RKS)

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine initializes the arrays associated with solar heat
		// gains for both individual surfaces and for zones.

		// METHODOLOGY EMPLOYED:
		// If the sun is down, all of the pertinent arrays are zeroed.  If the
		// sun is up, various calculations are made.

		// REFERENCES:
		// (I)BLAST legacy routine QSUN

		// Using/Aliasing
		using General::InterpSw;
		using General::InterpSlatAng;
		using namespace HeatBalanceMovableInsulation;
		using DaylightingDevices::DistributeTDDAbsorbedSolar;
		using namespace DataWindowEquivalentLayer;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AbsExt; // Solar absorptance of outermost layer (or movable insulation if present)
		Real64 AbsInt; // Inside opaque surface solar absorptance
		Real64 AbsIntSurf; // Inside opaque surface solar absorptance
		Real64 AbsIntSurfVis; // Inside opaque surface visible absorptance
		Real64 HMovInsul; // Resistance or "h" value of movable insulation (from EvalOutsideMovableInsulation, not used)
		int OtherZoneNum; // DO loop counter for zones
		int RoughIndexMovInsul; // Roughness index of movable insulation
		int ConstrNum; // Construction number
		int SurfNum; // Surface number
		int ZoneNum; // Zone number
		int ConstrNumSh; // Shaded construction number
		int SurfNumAdjZone; // Surface number in adjacent zone for interzone surfaces
		int IGlass; // Glass layer counter
		int ShadeFlag; // Shading flag
		Real64 DividerThermAbs; // Window divider thermal absorptance
		Real64 DividerSolAbs; // Window divider solar absorptance
		Real64 DividerSolRefl; // Window divider solar reflectance
		int MatNumGl; // Glass layer material number
		int MatNumSh; // Shade layer material number
		Real64 TransGl; // Glass layer solar transmittance, reflectance, absorptance
		Real64 ReflGl;
		Real64 AbsGl;
		int BlNum; // Blind number
		int TotGlassLayers; // Number of glass layers in a window construction
		Real64 BlAbsDiffBk; // Glass layer back diffuse solar absorptance when blind in place
		Real64 AbsDiffBkBl; // Blind diffuse back solar absorptance as part of glazing system
		Real64 EffBlEmiss; // Blind emissivity (thermal absorptance) as part of glazing system
		Real64 pulseMultipler; // use to create a pulse for the load component report computations
		static Real64 curQL( 0.0 ); // radiant value prior to adjustment for pulse for load component report
		static Real64 adjQL( 0.0 ); // radiant value including adjustment for pulse for load component report
		int EQLNum; // equivalent layer fenestration index
		int Lay; // equivalent layer fenestration layer index

		// FLOW:

		if ( ! allocated( QS ) ) QS.allocate( NumOfZones );
		if ( ! allocated( QSLights ) ) QSLights.allocate( NumOfZones );

		QS = 0.0;
		QSLights = 0.0;

		// COMPUTE TOTAL SHORT-WAVE RADIATION ORIGINATING IN ZONE.
		// Note: If sun is not up, QS is only internal gains
		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
			QS( ZoneNum ) = QD( ZoneNum ) + ZoneIntGain( ZoneNum ).QLTSW;
			QSLights( ZoneNum ) = ZoneIntGain( ZoneNum ).QLTSW;
		}

		if ( InterZoneWindow ) { // DO INTERZONE DISTRIBUTION.

			for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {

				if ( RecDifShortFromZ( ZoneNum ) ) {

					for ( OtherZoneNum = 1; OtherZoneNum <= NumOfZones; ++OtherZoneNum ) {

						if ( ( OtherZoneNum != ZoneNum ) && ( RecDifShortFromZ( OtherZoneNum ) ) ) {
							QS( ZoneNum ) += FractDifShortZtoZ( ZoneNum, OtherZoneNum ) * ( QD( OtherZoneNum ) + ZoneIntGain( OtherZoneNum ).QLTSW );
							ZoneDifSolFrIntWinsRep( ZoneNum ) += FractDifShortZtoZ( ZoneNum, OtherZoneNum ) * QD( OtherZoneNum );
							ZoneDifSolFrIntWinsRepEnergy( ZoneNum ) = ZoneDifSolFrIntWinsRep( ZoneNum ) * TimeStepZoneSec;
						}
					}

				}

			}

		}

		// Beam and diffuse solar on inside surfaces from interior windows (for reporting)

		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
			//!!! Following may need to be removed or changed when shelves are considered in adjacent reflection calculations
			if ( Surface( SurfNum ).Class == SurfaceClass_Shading ) continue;
			ZoneNum = Surface( SurfNum ).Zone;
			IntBmIncInsSurfIntensRep( SurfNum ) = ZoneBmSolFrIntWinsRep( ZoneNum ) / Zone( ZoneNum ).TotalSurfArea;
			IntBmIncInsSurfAmountRep( SurfNum ) = IntBmIncInsSurfIntensRep( SurfNum ) * ( Surface( SurfNum ).Area + SurfaceWindow( SurfNum ).DividerArea );
			IntBmIncInsSurfAmountRepEnergy( SurfNum ) = IntBmIncInsSurfAmountRep( SurfNum ) * TimeStepZoneSec;
			//      IntDifIncInsSurfIntensRep(SurfNum) = ZoneDifSolFrIntWinsRep(ZoneNum)/Zone(ZoneNum)%TotalSurfArea
			//      IntDifIncInsSurfAmountRep(SurfNum) = IntDifIncInsSurfIntensRep(SurfNum) *  &
			//                                             (Surface(SurfNum)%Area + SurfaceWindow(SurfNum)%DividerArea)
			//      IntDifIncInsSurfAmountRepEnergy(SurfNum) = IntDifIncInsSurfAmountRep(SurfNum) * TimeStepZoneSec
		}

		// COMPUTE CONVECTIVE GAINS AND ZONE FLUX DENSITY.
		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
			QS( ZoneNum ) *= FractDifShortZtoZ( ZoneNum, ZoneNum ) * VMULT( ZoneNum );
			// CR 8695, VMULT not based on visible
			QSLights( ZoneNum ) *= FractDifShortZtoZ( ZoneNum, ZoneNum ) * VMULT( ZoneNum );
		}

		// COMPUTE RADIANT GAINS ON SURFACES
		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {

			ZoneNum = Surface( SurfNum ).Zone;
			if ( ! Surface( SurfNum ).HeatTransSurf || ZoneNum == 0 ) continue; // Skip non-heat transfer surfaces
			if ( Surface( SurfNum ).Class == SurfaceClass_TDD_Dome ) continue; // Skip tubular daylighting device domes

			ConstrNum = Surface( SurfNum ).Construction;

			if ( Construct( ConstrNum ).TransDiff <= 0.0 ) { // Opaque surface
				AbsIntSurf = Construct( ConstrNum ).InsideAbsorpSolar;
				AbsIntSurfVis = Construct( ConstrNum ).InsideAbsorpSolar; //to fix CR 8695 change to this = Construct(ConstrNum)%InsideAbsorpVis
				HMovInsul = 0.0;
				if ( Surface( SurfNum ).MaterialMovInsulInt > 0 ) EvalInsideMovableInsulation( SurfNum, HMovInsul, AbsInt );
				if ( HMovInsul > 0.0 ) AbsIntSurf = AbsInt;
				QRadSWInAbs( SurfNum ) += QS( ZoneNum ) * AbsIntSurf;
				QRadSWLightsInAbs( SurfNum ) += QSLights( ZoneNum ) * AbsIntSurfVis;
			} else { // Window

				if ( SurfaceWindow( SurfNum ).WindowModelType != WindowEQLModel ) {
					ConstrNumSh = Surface( SurfNum ).ShadedConstruction;
					if ( SurfaceWindow( SurfNum ).StormWinFlag == 1 ) {
						ConstrNum = Surface( SurfNum ).StormWinConstruction;
						ConstrNumSh = Surface( SurfNum ).StormWinShadedConstruction;
					}
					TotGlassLayers = Construct( ConstrNum ).TotGlassLayers;
					ShadeFlag = SurfaceWindow( SurfNum ).ShadingFlag;

					// These calculations are repeated from InitInternalHeatGains for the Zone Component Loads Report
					pulseMultipler = 0.01; // the W/sqft pulse for the zone
					if ( ! doLoadComponentPulseNow ) {
						QRadThermInAbs( SurfNum ) = QL( ZoneNum ) * TMULT( ZoneNum ) * ITABSF( SurfNum );
					} else {
						curQL = QL( ZoneNum );
						// for the loads component report during the special sizing run increase the radiant portion
						// a small amount to create a "pulse" of heat that is used for the
						adjQL = curQL + Zone( ZoneNum ).FloorArea * pulseMultipler;
						// ITABSF is the Inside Thermal Absorptance
						// TMULT is a mulipliter for each zone
						// QRadThermInAbs is the thermal radiation absorbed on inside surfaces
						QRadThermInAbs( SurfNum ) = adjQL * TMULT( ZoneNum ) * ITABSF( SurfNum );
					}

					if ( ShadeFlag <= 0 ) { // No window shading
						for ( IGlass = 1; IGlass <= TotGlassLayers; ++IGlass ) {
							QRadSWwinAbs( IGlass, SurfNum ) += QS( ZoneNum ) * Construct( ConstrNum ).AbsDiffBack( IGlass );
						}
					} else if ( ShadeFlag == IntShadeOn || ShadeFlag >= 3 ) {
						// Interior, exterior or between-glass shade, screen or blind in place
						for ( IGlass = 1; IGlass <= Construct( ConstrNumSh ).TotGlassLayers; ++IGlass ) {
							if ( ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == BGShadeOn || ShadeFlag == ExtScreenOn ) QRadSWwinAbs( IGlass, SurfNum ) += QS( ZoneNum ) * Construct( ConstrNumSh ).AbsDiffBack( IGlass );
							if ( ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn ) {
								BlAbsDiffBk = InterpSlatAng( SurfaceWindow( SurfNum ).SlatAngThisTS, SurfaceWindow( SurfNum ).MovableSlats, Construct( ConstrNumSh ).BlAbsDiffBack( _, IGlass ) );
								QRadSWwinAbs( IGlass, SurfNum ) += QS( ZoneNum ) * BlAbsDiffBk;
							}
						}
						BlNum = SurfaceWindow( SurfNum ).BlindNumber;
						if ( ShadeFlag == IntShadeOn ) SurfaceWindow( SurfNum ).IntLWAbsByShade = QL( ZoneNum ) * Construct( ConstrNumSh ).ShadeAbsorpThermal * TMULT( ZoneNum );
						if ( ShadeFlag == IntBlindOn ) {
							EffBlEmiss = InterpSlatAng( SurfaceWindow( SurfNum ).SlatAngThisTS, SurfaceWindow( SurfNum ).MovableSlats, SurfaceWindow( SurfNum ).EffShBlindEmiss );
							SurfaceWindow( SurfNum ).IntLWAbsByShade = QL( ZoneNum ) * EffBlEmiss * TMULT( ZoneNum );
						}
						if ( ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == BGShadeOn || ShadeFlag == ExtScreenOn ) SurfaceWindow( SurfNum ).IntSWAbsByShade = QS( ZoneNum ) * Construct( ConstrNumSh ).AbsDiffBackShade;
						if ( ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == BGBlindOn ) {
							AbsDiffBkBl = InterpSlatAng( SurfaceWindow( SurfNum ).SlatAngThisTS, SurfaceWindow( SurfNum ).MovableSlats, Construct( ConstrNumSh ).AbsDiffBackBlind );
							SurfaceWindow( SurfNum ).IntSWAbsByShade = QS( ZoneNum ) * AbsDiffBkBl;
						}
						// Correct for divider shadowing
						if ( ShadeFlag == ExtShadeOn || ShadeFlag == ExtBlindOn || ShadeFlag == ExtScreenOn ) SurfaceWindow( SurfNum ).IntSWAbsByShade *= SurfaceWindow( SurfNum ).GlazedFrac;

					} else if ( ShadeFlag == SwitchableGlazing ) { // Switchable glazing
						for ( IGlass = 1; IGlass <= TotGlassLayers; ++IGlass ) {
							QRadSWwinAbs( IGlass, SurfNum ) += QS( ZoneNum ) * InterpSw( SurfaceWindow( SurfNum ).SwitchingFactor, Construct( ConstrNum ).AbsDiffBack( IGlass ), Construct( ConstrNumSh ).AbsDiffBack( IGlass ) );
						}

					} // End of shading flag check

					// Note that FrameQRadInAbs is initially calculated in InitSolarHeatGains
					if ( SurfaceWindow( SurfNum ).FrameArea > 0.0 ) SurfaceWindow( SurfNum ).FrameQRadInAbs += ( QS( ZoneNum ) * SurfaceWindow( SurfNum ).FrameSolAbsorp + ( QL( ZoneNum ) * TMULT( ZoneNum ) + QHTRadSysSurf( SurfNum ) + QHWBaseboardSurf( SurfNum ) + QSteamBaseboardSurf( SurfNum ) + QElecBaseboardSurf( SurfNum ) ) * SurfaceWindow( SurfNum ).FrameEmis ) * ( 1.0 + 0.5 * SurfaceWindow( SurfNum ).ProjCorrFrIn ); // Window has a frame
					if ( SurfaceWindow( SurfNum ).DividerArea > 0.0 ) { // Window has dividers
						DividerThermAbs = SurfaceWindow( SurfNum ).DividerEmis;
						DividerSolAbs = SurfaceWindow( SurfNum ).DividerSolAbsorp;
						if ( SurfaceWindow( SurfNum ).DividerType == Suspended ) { // Suspended divider; account for inside glass
							MatNumGl = Construct( ConstrNum ).LayerPoint( Construct( ConstrNum ).TotLayers );
							TransGl = Material( MatNumGl ).Trans;
							ReflGl = Material( MatNumGl ).ReflectSolBeamBack;
							AbsGl = 1.0 - TransGl - ReflGl;
							DividerSolRefl = 1.0 - DividerSolAbs;
							DividerSolAbs = AbsGl + TransGl * ( DividerSolAbs + DividerSolRefl * AbsGl ) / ( 1.0 - DividerSolRefl * ReflGl );
							DividerThermAbs = Material( MatNumGl ).AbsorpThermalBack;
						}
						// Correct for interior shade transmittance
						if ( ShadeFlag == IntShadeOn ) {
							MatNumSh = Construct( ConstrNumSh ).LayerPoint( Construct( ConstrNumSh ).TotLayers );
							DividerSolAbs *= Material( MatNumSh ).Trans;
							DividerThermAbs *= Material( MatNumSh ).TransThermal;
						} else if ( ShadeFlag == IntBlindOn ) {
							DividerSolAbs *= InterpSlatAng( SurfaceWindow( SurfNum ).SlatAngThisTS, SurfaceWindow( SurfNum ).MovableSlats, Blind( BlNum ).SolBackDiffDiffTrans );
							DividerThermAbs *= InterpSlatAng( SurfaceWindow( SurfNum ).SlatAngThisTS, SurfaceWindow( SurfNum ).MovableSlats, Blind( BlNum ).IRBackTrans );
						}
						// Note that DividerQRadInAbs is initially calculated in InitSolarHeatGains
						SurfaceWindow( SurfNum ).DividerQRadInAbs += ( QS( ZoneNum ) * DividerSolAbs + ( QL( ZoneNum ) * TMULT( ZoneNum ) + QHTRadSysSurf( SurfNum ) + QHWBaseboardSurf( SurfNum ) + QSteamBaseboardSurf( SurfNum ) + QElecBaseboardSurf( SurfNum ) ) * DividerThermAbs ) * ( 1.0 + SurfaceWindow( SurfNum ).ProjCorrDivIn );
					}

				} else if ( SurfaceWindow( SurfNum ).WindowModelType == WindowEQLModel ) {

					//ConstrNumSh = Surface(SurfNum)%ShadedConstruction
					ConstrNum = Surface( SurfNum ).Construction;
					//TotGlassLayers = Construct(ConstrNum)%TotGlassLayers

					// These calculations are repeated from InitInternalHeatGains for the Zone Component Loads Report
					pulseMultipler = 0.01; // the W/sqft pulse for the zone
					if ( ! doLoadComponentPulseNow ) {
						QRadThermInAbs( SurfNum ) = QL( ZoneNum ) * TMULT( ZoneNum ) * ITABSF( SurfNum );
					} else {
						curQL = QL( ZoneNum );
						// for the loads component report during the special sizing run increase the radiant portion
						// a small amount to create a "pulse" of heat that is used for the
						adjQL = curQL + Zone( ZoneNum ).FloorArea * pulseMultipler;
						// ITABSF is the Inside Thermal Absorptance
						// TMULT is a mulipliter for each zone
						// QRadThermInAbs is the thermal radiation absorbed on inside surfaces
						QRadThermInAbs( SurfNum ) = adjQL * TMULT( ZoneNum ) * ITABSF( SurfNum );
					}
					// Radiations absorbed by the window layers coming from zone side
					EQLNum = Construct( ConstrNum ).EQLConsPtr;
					for ( Lay = 1; Lay <= CFS( EQLNum ).NL; ++Lay ) {
						QRadSWwinAbs( Lay, SurfNum ) += QS( ZoneNum ) * Construct( ConstrNum ).AbsDiffBackEQL( Lay );
					}
					// Window frame has not been included for equivalent layer model yet

				} // end if for IF ( SurfaceWindow(SurfNum)%WindowModelType /= WindowEQLModel) THEN

			} // End of opaque surface vs. window check

			// OUTSIDE OF SURFACE CASES
			if ( Surface( SurfNum ).ExtBoundCond > 0 ) { // Interzone surface

				if ( Construct( ConstrNum ).TransDiff > 0.0 ) { // Interzone window

					// Short-wave radiation absorbed in panes of corresponding window in adjacent zone
					SurfNumAdjZone = Surface( SurfNum ).ExtBoundCond;

					if ( SurfaceWindow( SurfNumAdjZone ).WindowModelType != WindowEQLModel ) {
						for ( IGlass = 1; IGlass <= TotGlassLayers; ++IGlass ) {
							QRadSWwinAbs( IGlass, SurfNumAdjZone ) += QS( ZoneNum ) * Construct( Surface( SurfNumAdjZone ).Construction ).AbsDiff( IGlass );
							// Note that AbsDiff rather than AbsDiffBack is used in the above since the
							// radiation from the current zone is incident on the outside of the adjacent
							// zone's window.
						}
					} else { // IF (SurfaceWindow(SurfNumAdjZone)%WindowModelType == WindowEQLModel) THEN
						ConstrNum = Surface( SurfNumAdjZone ).Construction;
						EQLNum = Construct( ConstrNum ).EQLConsPtr;
						for ( Lay = 1; Lay <= CFS( EQLNum ).NL; ++Lay ) {
							QRadSWwinAbs( Lay, SurfNumAdjZone ) += QS( ZoneNum ) * Construct( ConstrNum ).AbsDiffFrontEQL( Lay );
							// Note that AbsDiffFrontEQL rather than AbsDiffBackEQL is used in the above
							// since the radiation from the current zone is incident on the outside of the
							// adjacent zone's window.
						}
					}
				}

			} else if ( Construct( ConstrNum ).TransDiff <= 0.0 ) { // Opaque exterior surface
				// Calculate absorbed solar on outside if movable exterior insulation in place
				HMovInsul = 0.0;
				if ( Surface( SurfNum ).MaterialMovInsulExt > 0 ) EvalOutsideMovableInsulation( SurfNum, HMovInsul, RoughIndexMovInsul, AbsExt );
				if ( HMovInsul > 0 ) { // Movable outside insulation in place
					QRadSWOutMvIns( SurfNum ) = QRadSWOutAbs( SurfNum ) * AbsExt / Material( Construct( ConstrNum ).LayerPoint( 1 ) ).AbsorpSolar;
					// For transparent insulation, allow some sunlight to get through the movable insulation.
					// The equation below is derived by taking what is transmitted through the layer and applying
					// the fraction that is absorbed plus the back reflected portion (first order reflection only)
					// to the plane between the transparent insulation and the exterior surface face.
					QRadSWOutAbs( SurfNum ) = Material( Surface( SurfNum ).MaterialMovInsulExt ).Trans * QRadSWOutMvIns( SurfNum ) * ( ( Material( Construct( ConstrNum ).LayerPoint( 1 ) ).AbsorpSolar / AbsExt ) + ( 1 - Material( Construct( ConstrNum ).LayerPoint( 1 ) ).AbsorpSolar ) );
					SWOutAbsTotalReport( SurfNum ) = QRadSWOutAbs( SurfNum ) * Surface( SurfNum ).Area;
					SWOutAbsEnergyReport( SurfNum ) = SWOutAbsTotalReport( SurfNum ) * TimeStepZoneSec;
				}

			}

		}

		// RJH 08/30/07 - Add InitialDifSolInAbs, InitialDifSolwinAbs, and InitialDifSolAbsByShade
		// calced in CalcWinTransDifSolInitialDistribution to QRadSWInAbs, QRadSWwinAbs, and IntSWAbsByShade here
		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			ZoneNum = Surface( SurfNum ).Zone;
			if ( ! Surface( SurfNum ).HeatTransSurf || ZoneNum == 0 ) continue; // Skip non-heat transfer surfaces
			if ( Surface( SurfNum ).Class == SurfaceClass_TDD_Dome ) continue; // Skip tubular daylighting device domes
			ConstrNum = Surface( SurfNum ).Construction;
			if ( Construct( ConstrNum ).TransDiff <= 0.0 ) { // Opaque surface
				QRadSWInAbs( SurfNum ) += InitialDifSolInAbs( SurfNum );
			} else { // Window
				if ( SurfaceWindow( SurfNum ).WindowModelType != WindowBSDFModel && SurfaceWindow( SurfNum ).WindowModelType != WindowEQLModel ) {
					ConstrNumSh = Surface( SurfNum ).ShadedConstruction;
					if ( SurfaceWindow( SurfNum ).StormWinFlag == 1 ) {
						ConstrNum = Surface( SurfNum ).StormWinConstruction;
						ConstrNumSh = Surface( SurfNum ).StormWinShadedConstruction;
					}
					TotGlassLayers = Construct( ConstrNum ).TotGlassLayers;
					ShadeFlag = SurfaceWindow( SurfNum ).ShadingFlag;
					if ( ShadeFlag <= 0 ) { // No window shading
						for ( IGlass = 1; IGlass <= TotGlassLayers; ++IGlass ) {
							QRadSWwinAbs( IGlass, SurfNum ) += InitialDifSolwinAbs( IGlass, SurfNum );
						}
					} else if ( ShadeFlag == IntShadeOn || ShadeFlag >= 3 ) {
						// Interior, exterior or between-glass shade, screen or blind in place
						for ( IGlass = 1; IGlass <= Construct( ConstrNumSh ).TotGlassLayers; ++IGlass ) {
							QRadSWwinAbs( IGlass, SurfNum ) += InitialDifSolwinAbs( IGlass, SurfNum );
						}
						if ( ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == BGShadeOn || ShadeFlag == ExtScreenOn ) SurfaceWindow( SurfNum ).IntSWAbsByShade += SurfaceWindow( SurfNum ).InitialDifSolAbsByShade;
						if ( ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == BGBlindOn ) {
							SurfaceWindow( SurfNum ).IntSWAbsByShade += SurfaceWindow( SurfNum ).InitialDifSolAbsByShade;
						}
					} else if ( ShadeFlag == SwitchableGlazing ) { // Switchable glazing
						for ( IGlass = 1; IGlass <= TotGlassLayers; ++IGlass ) {
							QRadSWwinAbs( IGlass, SurfNum ) += InitialDifSolwinAbs( IGlass, SurfNum );
						}
					} // End of shading flag check
				} else if ( SurfaceWindow( SurfNum ).WindowModelType == WindowBSDFModel ) {
					TotGlassLayers = Construct( ConstrNum ).TotGlassLayers;
					for ( IGlass = 1; IGlass <= TotGlassLayers; ++IGlass ) {
						QRadSWwinAbs( IGlass, SurfNum ) += InitialDifSolwinAbs( IGlass, SurfNum );
					}
				} else if ( SurfaceWindow( SurfNum ).WindowModelType == WindowEQLModel ) {

					//ConstrNum   = Surface(SurfNum)%Construction
					EQLNum = Construct( ConstrNum ).EQLConsPtr;

					for ( Lay = 1; Lay <= CFS( EQLNum ).NL; ++Lay ) {
						QRadSWwinAbs( Lay, SurfNum ) += InitialDifSolwinAbs( Lay, SurfNum );
					}

				}
			} // End of Opaque surface vs. Window check
		} // End of SurfNum loop to initialize SW Absorbed values with CalcWinTransDifSolInitialDistribution results

		// RJH 09/07/07 - report variables for surface absorbed short wave radiation
		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			SWwinAbsTotalReport( SurfNum ) = 0.0;
			SWInAbsTotalReport( SurfNum ) = 0.0;
			InitialDifSolInAbsReport( SurfNum ) = 0.0;
			InitialDifSolInTransReport( SurfNum ) = 0.0;
			ZoneNum = Surface( SurfNum ).Zone;
			if ( ! Surface( SurfNum ).HeatTransSurf || ZoneNum == 0 ) continue; // Skip non-heat transfer surfaces
			if ( Surface( SurfNum ).Class == SurfaceClass_TDD_Dome ) continue; // Skip tubular daylighting device domes
			ConstrNum = Surface( SurfNum ).Construction;
			if ( Construct( ConstrNum ).TransDiff <= 0.0 ) { // Opaque surface
				// Initial Transmitted Diffuse Solar Absorbed on Inside of Surface[W]
				InitialDifSolInAbsReport( SurfNum ) = InitialDifSolInAbs( SurfNum ) * Surface( SurfNum ).Area;
				// Total Shortwave Radiation Absorbed on Inside of Surface[W]
				SWInAbsTotalReport( SurfNum ) = QRadSWInAbs( SurfNum ) * Surface( SurfNum ).Area;
			} else { // Window
				// Initial Transmitted Diffuse Solar Transmitted Through Inside of Surface[W]
				InitialDifSolInTransReport( SurfNum ) += InitialDifSolInTrans( SurfNum ) * Surface( SurfNum ).Area;
				if ( SurfaceWindow( SurfNum ).WindowModelType != WindowEQLModel ) {
					ConstrNumSh = Surface( SurfNum ).ShadedConstruction;
					if ( SurfaceWindow( SurfNum ).StormWinFlag == 1 ) {
						ConstrNum = Surface( SurfNum ).StormWinConstruction;
						ConstrNumSh = Surface( SurfNum ).StormWinShadedConstruction;
					}
					if ( SurfaceWindow( SurfNum ).WindowModelType == WindowBSDFModel ) {
						TotGlassLayers = Construct( ConstrNum ).TotSolidLayers;
					} else {
						TotGlassLayers = Construct( ConstrNum ).TotGlassLayers;
					}
					ShadeFlag = SurfaceWindow( SurfNum ).ShadingFlag;
					if ( ShadeFlag <= 0 ) { // No window shading
						for ( IGlass = 1; IGlass <= TotGlassLayers; ++IGlass ) {
							// Initial Transmitted Diffuse Solar Absorbed on Inside of Surface[W]
							InitialDifSolInAbsReport( SurfNum ) += InitialDifSolwinAbs( IGlass, SurfNum ) * Surface( SurfNum ).Area;
							// Total Shortwave Radiation Absorbed on Inside of Surface[W]
							SWInAbsTotalReport( SurfNum ) += QRadSWwinAbs( IGlass, SurfNum ) * Surface( SurfNum ).Area;
							// Total Shortwave Absorbed:All Glass Layers[W]
							SWwinAbsTotalReport( SurfNum ) += QRadSWwinAbs( IGlass, SurfNum ) * Surface( SurfNum ).Area;
						}
					} else if ( ShadeFlag == IntShadeOn || ShadeFlag >= 3 ) {
						// Interior, exterior or between-glass shade, screen or blind in place
						for ( IGlass = 1; IGlass <= Construct( ConstrNumSh ).TotGlassLayers; ++IGlass ) {
							// Initial Transmitted Diffuse Solar Absorbed on Inside of Surface[W]
							InitialDifSolInAbsReport( SurfNum ) += InitialDifSolwinAbs( IGlass, SurfNum ) * Surface( SurfNum ).Area;
							// Total Shortwave Radiation Absorbed on Inside of Surface[W]
							SWInAbsTotalReport( SurfNum ) += QRadSWwinAbs( IGlass, SurfNum ) * Surface( SurfNum ).Area;
							// Total Shortwave Absorbed:All Glass Layers[W]
							SWwinAbsTotalReport( SurfNum ) += QRadSWwinAbs( IGlass, SurfNum ) * Surface( SurfNum ).Area;
						}
					} else if ( ShadeFlag == SwitchableGlazing ) { // Switchable glazing
						for ( IGlass = 1; IGlass <= TotGlassLayers; ++IGlass ) {
							// Initial Transmitted Diffuse Solar Absorbed on Inside of Surface[W]
							InitialDifSolInAbsReport( SurfNum ) += InitialDifSolwinAbs( IGlass, SurfNum ) * Surface( SurfNum ).Area;
							// Total Shortwave Radiation Absorbed on Inside of Surface[W]
							SWInAbsTotalReport( SurfNum ) += QRadSWwinAbs( IGlass, SurfNum ) * Surface( SurfNum ).Area;
							// Total Shortwave Absorbed:All Glass Layers[W]
							SWwinAbsTotalReport( SurfNum ) += QRadSWwinAbs( IGlass, SurfNum ) * Surface( SurfNum ).Area;
						}
					} // End of shading flag check
				} else { //IF (SurfaceWindow(SurfNum)%WindowModelType == WindowEQLModel) THEN
					ConstrNum = Surface( SurfNum ).Construction;
					EQLNum = Construct( ConstrNum ).EQLConsPtr;
					for ( Lay = 1; Lay <= CFS( EQLNum ).NL; ++Lay ) {

						// Initial Transmitted Diffuse Solar Absorbed on Inside of Surface[W]
						InitialDifSolInAbsReport( SurfNum ) += InitialDifSolwinAbs( Lay, SurfNum ) * Surface( SurfNum ).Area;
						// Total Shortwave Radiation Absorbed on Inside of Surface[W]
						SWInAbsTotalReport( SurfNum ) += QRadSWwinAbs( Lay, SurfNum ) * Surface( SurfNum ).Area;
						// Total Shortwave Absorbed:All solid Layers[W]
						SWwinAbsTotalReport( SurfNum ) += QRadSWwinAbs( Lay, SurfNum ) * Surface( SurfNum ).Area;

					}
				}
			} // End of Opaque surface vs. Window check
		} // End of SurfNum loop to report variables for surface total absorbed short wave radiation

		DistributeTDDAbsorbedSolar();

	}

	void
	ComputeIntThermalAbsorpFactors()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Legacy Code (George Walton)
		//       DATE WRITTEN   Legacy: Dec 1976
		//       MODIFIED       Nov. 99, FCW: to take into account movable interior shades and switchable glazing
		//                      June 01, FCW: to take into account interior blinds.
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine computes the fractions of long-wave radiation from lights, equipment and people
		// that is absorbed by each zone surface.

		// METHODOLOGY EMPLOYED:
		// The fraction is assumed to be proportional to the product of the surface area times its thermal absorptivity.

		// REFERENCES:
		// BLAST Routine: CITAF - Compute Interior Thermal Absorption Factors

		// Using/Aliasing
		using namespace HeatBalanceMovableInsulation;
		using General::InterpSw;
		using General::InterpSlatAng;

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
		int ConstrNum; // Construction number
		int ConstrNumSh; // Shaded construction number
		int FirstZoneSurf; // Index of first surface in current zone
		int LastZoneSurf; // Index of last surface in current zone
		Real64 SUM1; // Intermediate calculation value
		Real64 SUM2; // Intermediate calculation value
		int SurfNum; // DO loop counter for zone surfaces
		int ZoneNum; // Loop counter for Zones
		int ShadeFlag; // Window shading flag
		Real64 HMovInsul; // Conductance of movable insulation
		int RoughIndexMovInsul; // Roughness index of movable insulation
		Real64 AbsExt; // Solar absorptance of movable insulation
		Real64 DividerThermAbs; // Window divider thermal absorptance
		int MatNumSh; // Shade layer material number
		Real64 TauShIR; // Shade or blind IR transmittance
		Real64 EffShDevEmiss; // Effective emissivity of shade or blind

		if ( ! allocated( ITABSF ) ) {
			ITABSF.dimension( TotSurfaces, 0.0 );
			TMULT.dimension( NumOfZones, 0.0 );
			TCONV.dimension( NumOfZones, 0.0 );
		}

		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
			ConstrNum = Surface( SurfNum ).Construction;
			ShadeFlag = SurfaceWindow( SurfNum ).ShadingFlag;
			ITABSF( SurfNum ) = Construct( ConstrNum ).InsideAbsorpThermal;
			if ( Construct( ConstrNum ).TransDiff <= 0.0 ) { // Opaque surface
				RoughIndexMovInsul = 0;
				if ( Surface( SurfNum ).MaterialMovInsulExt > 0 ) EvalOutsideMovableInsulation( SurfNum, HMovInsul, RoughIndexMovInsul, AbsExt );
				if ( RoughIndexMovInsul > 0 ) ITABSF( SurfNum ) = Material( Surface( SurfNum ).MaterialMovInsulExt ).AbsorpThermal; // Movable outside insulation present
			}
			// For window with an interior shade or blind, emissivity is a combination of glass and shade/blind emissivity
			if ( ShadeFlag == IntShadeOn || ShadeFlag == IntBlindOn ) ITABSF( SurfNum ) = InterpSlatAng( SurfaceWindow( SurfNum ).SlatAngThisTS, SurfaceWindow( SurfNum ).MovableSlats, SurfaceWindow( SurfNum ).EffShBlindEmiss ) + InterpSlatAng( SurfaceWindow( SurfNum ).SlatAngThisTS, SurfaceWindow( SurfNum ).MovableSlats, SurfaceWindow( SurfNum ).EffGlassEmiss ); // For shades, following interpolation just returns value of first element in array
		}

		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {

			SUM1 = 0.0;
			SUM2 = 0.0;

			FirstZoneSurf = Zone( ZoneNum ).SurfaceFirst;
			LastZoneSurf = Zone( ZoneNum ).SurfaceLast;

			for ( SurfNum = FirstZoneSurf; SurfNum <= LastZoneSurf; ++SurfNum ) {

				if ( ! Surface( SurfNum ).HeatTransSurf ) continue;

				ConstrNum = Surface( SurfNum ).Construction;
				ShadeFlag = SurfaceWindow( SurfNum ).ShadingFlag;
				if ( ShadeFlag != SwitchableGlazing ) {
					SUM1 += Surface( SurfNum ).Area * ITABSF( SurfNum );
				} else { // Switchable glazing
					SUM1 += Surface( SurfNum ).Area * InterpSw( SurfaceWindow( SurfNum ).SwitchingFactor, Construct( ConstrNum ).InsideAbsorpThermal, Construct( SurfaceWindow( SurfNum ).ShadedConstruction ).InsideAbsorpThermal );
				}

				// Window frame and divider effects
				if ( SurfaceWindow( SurfNum ).FrameArea > 0.0 ) SUM1 += SurfaceWindow( SurfNum ).FrameArea * ( 1.0 + 0.5 * SurfaceWindow( SurfNum ).ProjCorrFrIn ) * SurfaceWindow( SurfNum ).FrameEmis;
				if ( SurfaceWindow( SurfNum ).DividerArea > 0.0 ) {
					DividerThermAbs = SurfaceWindow( SurfNum ).DividerEmis;
					// Suspended (between-glass) divider; relevant emissivity is inner glass emissivity
					if ( SurfaceWindow( SurfNum ).DividerType == Suspended ) DividerThermAbs = Construct( ConstrNum ).InsideAbsorpThermal;
					if ( ShadeFlag == IntShadeOn || ShadeFlag == IntBlindOn ) {
						// Interior shade or blind in place
						ConstrNumSh = SurfaceWindow( SurfNum ).ShadedConstruction;
						MatNumSh = Construct( ConstrNumSh ).LayerPoint( Construct( ConstrNumSh ).TotLayers );
						TauShIR = Material( MatNumSh ).TransThermal;
						EffShDevEmiss = SurfaceWindow( SurfNum ).EffShBlindEmiss( 1 );
						if ( ShadeFlag == IntBlindOn ) {
							TauShIR = InterpSlatAng( SurfaceWindow( SurfNum ).SlatAngThisTS, SurfaceWindow( SurfNum ).MovableSlats, Blind( SurfaceWindow( SurfNum ).BlindNumber ).IRBackTrans );
							EffShDevEmiss = InterpSlatAng( SurfaceWindow( SurfNum ).SlatAngThisTS, SurfaceWindow( SurfNum ).MovableSlats, SurfaceWindow( SurfNum ).EffShBlindEmiss );
						}
						SUM1 += SurfaceWindow( SurfNum ).DividerArea * ( EffShDevEmiss + DividerThermAbs * TauShIR );
					} else {
						SUM1 += SurfaceWindow( SurfNum ).DividerArea * ( 1.0 + SurfaceWindow( SurfNum ).ProjCorrDivIn ) * DividerThermAbs;
					}

				}

			} // End of loop over surfaces in zone

			TMULT( ZoneNum ) = 1.0 / SUM1;
			TCONV( ZoneNum ) = SUM2 / SUM1;

		} // End of loop over zones

	}

	void
	ComputeIntSWAbsorpFactors()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Legacy (George Walton)
		//       DATE WRITTEN   Legacy (December 1980)
		//       MODIFIED       Nov. 99, FW; now called every time step to account for movable
		//                      window shades and insulation
		//                      Mar. 00, FW; change name from ComputeVisLightingAbsorpFactors
		//                      to ComputeIntSWAbsorpFactors
		//                      May 00, FW; add window frame and divider effects
		//                      June 01, FW: account for window blinds
		//                      Nov 01, FW: account for absorptance of exterior shades and interior or exterior blinds
		//                      Jan 03, FW: add between-glass shade/blind
		//                      May 06, RR: account for exterior window screens

		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Computes VMULT, the inverse of the sum of area*(short-wave absorptance+transmittance) for
		// the surfaces in a zone. VMULT is used to calculate the zone interior diffuse short-wave radiation
		// absorbed by the inside of opaque zone surfaces or by the glass and shade/blind layers of zone windows.

		// Sets VCONV to zero (VCONV was formerly used to calculate convective gain due to short-wave
		// radiation absorbed by interior window shades).

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// BLAST Routine - CIVAF - Compute Surface Absorption Factors For Short Wave Radiation
		//                         From Zone Lights And Diffuse Solar.

		// Using/Aliasing
		using namespace HeatBalanceMovableInsulation;
		using General::InterpSw;
		using General::InterpSlatAng;
		using namespace DataWindowEquivalentLayer;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const SmallestAreaAbsProductAllowed( 0.01 ); // Avoid a division by zero of the user has entered a bunch
		// of surfaces with zero absorptivity on the inside

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ConstrNum; // DO loop counter for constructions
		int FirstZoneSurf; // Index of first surface in current zone
		int LastZoneSurf; // Index of last surface in current zone
		Real64 SUM1; // Intermediate calculation value for solar absorbed and transmitted
		//   by windows (including shade, blind or insulation, if present)
		int SurfNum; // DO loop counter for zone surfaces
		int ZoneNum; // Loop counter for Zones
		int ShadeFlag; // Shading flag
		int ConstrNumSh; // Shaded construction number
		Real64 SwitchFac; // Switching factor
		int Lay; // Layer number
		Real64 AbsDiffLayWin; // Window layer short-wave absorptance
		Real64 AbsDiffTotWin; // Sum of window layer short-wave absorptances
		Real64 TransDiffWin; // Window diffuse short-wave transmittance
		Real64 DiffAbsShade; // Diffuse short-wave shade or blind absorptance
		Real64 AbsIntSurf; // Inside surface short-wave absorptance
		Real64 AbsInt;
		Real64 HMovInsul; // Conductance of movable insulation
		Real64 DividerAbs; // Window divider solar absorptance
		int MatNumGl; // Glass material number
		Real64 TransGl; // Glass layer short-wave transmittance, reflectance, absorptance
		Real64 ReflGl;
		Real64 AbsGl;
		Real64 DividerRefl; // Window divider short-wave reflectance


		static Array1D_bool FirstCalcZone; // for error message

		// FLOW:

		if ( ! allocated( VMULT ) ) {
			VMULT.dimension( NumOfZones, 0.0 );
			VCONV.dimension( NumOfZones, 0.0 );
		}
		if ( ComputeIntSWAbsorpFactorsfirstTime ) {
			FirstCalcZone.dimension( NumOfZones, true );
			ComputeIntSWAbsorpFactorsfirstTime = false;
		}

		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {

			SUM1 = 0.0;

			FirstZoneSurf = Zone( ZoneNum ).SurfaceFirst;
			LastZoneSurf = Zone( ZoneNum ).SurfaceLast;

			for ( SurfNum = FirstZoneSurf; SurfNum <= LastZoneSurf; ++SurfNum ) {

				if ( ! Surface( SurfNum ).HeatTransSurf ) continue;

				ConstrNum = Surface( SurfNum ).Construction;

				if ( Construct( ConstrNum ).TransDiff <= 0.0 ) {

					// Opaque surface

					AbsIntSurf = Construct( ConstrNum ).InsideAbsorpSolar;
					HMovInsul = 0.0;
					if ( Surface( SurfNum ).MaterialMovInsulInt > 0 ) EvalInsideMovableInsulation( SurfNum, HMovInsul, AbsInt );
					if ( HMovInsul > 0.0 ) AbsIntSurf = AbsInt;
					SUM1 += Surface( SurfNum ).Area * AbsIntSurf;

				} else {

					// Window
					if ( ! Construct( Surface( SurfNum ).Construction ).WindowTypeEQL ) {
						ShadeFlag = SurfaceWindow( SurfNum ).ShadingFlag;
						AbsDiffTotWin = 0.0;
						ConstrNumSh = Surface( SurfNum ).ShadedConstruction;
						if ( SurfaceWindow( SurfNum ).StormWinFlag == 1 ) {
							ConstrNum = Surface( SurfNum ).StormWinConstruction;
							ConstrNumSh = Surface( SurfNum ).StormWinShadedConstruction;
						}
						SwitchFac = SurfaceWindow( SurfNum ).SwitchingFactor;

						// Sum of absorptances of glass layers
						for ( Lay = 1; Lay <= Construct( ConstrNum ).TotGlassLayers; ++Lay ) {
							AbsDiffLayWin = Construct( ConstrNum ).AbsDiffBack( Lay );

							// Window with shade, screen or blind
							if ( ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == BGShadeOn || ShadeFlag == ExtScreenOn ) {
								AbsDiffLayWin = Construct( ConstrNumSh ).AbsDiffBack( Lay );
							} else if ( ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == BGBlindOn ) {
								AbsDiffLayWin = InterpSlatAng( SurfaceWindow( SurfNum ).SlatAngThisTS, SurfaceWindow( SurfNum ).MovableSlats, Construct( ConstrNumSh ).BlAbsDiffBack( _, Lay ) );
							}

							// Switchable glazing
							if ( ShadeFlag == SwitchableGlazing ) AbsDiffLayWin = InterpSw( SwitchFac, AbsDiffLayWin, Construct( ConstrNumSh ).AbsDiffBack( Lay ) );

							AbsDiffTotWin += AbsDiffLayWin;
						}

						TransDiffWin = Construct( ConstrNum ).TransDiff;
						DiffAbsShade = 0.0;

						// Window with shade, screen or blind

						if ( ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == BGShadeOn || ShadeFlag == ExtScreenOn ) {
							TransDiffWin = Construct( ConstrNumSh ).TransDiff;
							DiffAbsShade = Construct( ConstrNumSh ).AbsDiffBackShade;
						} else if ( ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == BGBlindOn ) {
							TransDiffWin = InterpSlatAng( SurfaceWindow( SurfNum ).SlatAngThisTS, SurfaceWindow( SurfNum ).MovableSlats, Construct( ConstrNumSh ).BlTransDiff );
							DiffAbsShade = InterpSlatAng( SurfaceWindow( SurfNum ).SlatAngThisTS, SurfaceWindow( SurfNum ).MovableSlats, Construct( ConstrNumSh ).AbsDiffBackBlind );
						}

						// Switchable glazing

						if ( ShadeFlag == SwitchableGlazing ) TransDiffWin = InterpSw( SwitchFac, TransDiffWin, Construct( ConstrNumSh ).TransDiff );

						SUM1 += Surface( SurfNum ).Area * ( TransDiffWin + AbsDiffTotWin + DiffAbsShade );

						// Window frame and divider effects (shade area is glazed area plus divider area)

						if ( SurfaceWindow( SurfNum ).FrameArea > 0.0 ) SUM1 += SurfaceWindow( SurfNum ).FrameArea * SurfaceWindow( SurfNum ).FrameSolAbsorp * ( 1.0 + 0.5 * SurfaceWindow( SurfNum ).ProjCorrFrIn );
						if ( SurfaceWindow( SurfNum ).DividerArea > 0.0 ) {
							DividerAbs = SurfaceWindow( SurfNum ).DividerSolAbsorp;
							if ( SurfaceWindow( SurfNum ).DividerType == Suspended ) {
								//Suspended (between-glass) divider: account for glass on inside of divider
								MatNumGl = Construct( ConstrNum ).LayerPoint( Construct( ConstrNum ).TotLayers );
								TransGl = Material( MatNumGl ).Trans;
								ReflGl = Material( MatNumGl ).ReflectSolBeamBack;
								AbsGl = 1.0 - TransGl - ReflGl;
								DividerRefl = 1.0 - DividerAbs;
								DividerAbs = AbsGl + TransGl * ( DividerAbs + DividerRefl * AbsGl ) / ( 1.0 - DividerRefl * ReflGl );
							}
							if ( ShadeFlag == IntShadeOn || ShadeFlag == IntBlindOn ) {
								SUM1 += SurfaceWindow( SurfNum ).DividerArea * ( DividerAbs + DiffAbsShade );
							} else {
								SUM1 += SurfaceWindow( SurfNum ).DividerArea * ( 1.0 + SurfaceWindow( SurfNum ).ProjCorrDivIn ) * DividerAbs;
							}
						}
					} else { // equivalent layer window
						// In equivalent layer window solid layers (Glazing and shades) are treated equally
						// frames and dividers are not supported
						AbsDiffTotWin = 0.0;
						AbsDiffLayWin = 0.0;
						TransDiffWin = Construct( ConstrNum ).TransDiff;
						for ( Lay = 1; Lay <= CFS( Construct( ConstrNum ).EQLConsPtr ).NL; ++Lay ) {
							AbsDiffLayWin = Construct( ConstrNum ).AbsDiffBackEQL( Lay );
							AbsDiffTotWin += AbsDiffLayWin;
						}
						SUM1 += Surface( SurfNum ).Area * ( TransDiffWin + AbsDiffTotWin );
					}
				} // End of check if opaque surface or window
			} // End of loop over surfaces in zone

			if ( SUM1 > SmallestAreaAbsProductAllowed ) { // Everything is okay, proceed with the regular calculation
				VMULT( ZoneNum ) = 1.0 / SUM1;

			} else { // the sum of area*solar absorptance for all surfaces in the zone is zero--either the user screwed up
				// or they really want to disallow any solar from being absorbed on the inside surfaces.  Fire off a
				// nasty warning message and then assume that no solar is ever absorbed (basically everything goes
				// back out whatever window is there.  Note that this also assumes that the shade has no effect.
				// That's probably not correct, but how correct is it to assume that no solar is absorbed anywhere
				// in the zone?
				if ( FirstCalcZone( ZoneNum ) ) {
					ShowWarningError( "ComputeIntSWAbsorbFactors: Sum of area times inside solar absorption for all surfaces is zero in Zone: " + Zone( ZoneNum ).Name );
					FirstCalcZone( ZoneNum ) = false;
				}
				VMULT( ZoneNum ) = 0.0;

			}
		} // End of zone loop

	}

	void
	ComputeDifSolExcZonesWIZWindows( int const NumberOfZones ) // Number of zones
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Legacy Code
		//       DATE WRITTEN
		//       MODIFIED       Jun 2007 - Lawrie - Speed enhancements.
		//       RE-ENGINEERED  Winkelmann, Lawrie

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine computes the diffuse solar exchange factors between zones with
		// interzone windows.

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
		static Array2D< Real64 > D;
		int SurfNum;
		int IZ;
		int JZ;
		int KZ;
		int LZ;
		int MZ;
		int NZ;

		if ( ! allocated( FractDifShortZtoZ ) ) {
			FractDifShortZtoZ.allocate( NumberOfZones, NumberOfZones );
			RecDifShortFromZ.allocate( NumberOfZones );
			D.allocate( NumberOfZones, NumberOfZones );
		}

		RecDifShortFromZ = false;
		FractDifShortZtoZ = 0.0;
		D.to_identity();

		//      IF (.not. ANY(Zone%HasInterZoneWindow)) RETURN  ! this caused massive diffs
		if ( KickOffSimulation || KickOffSizing ) return;
		//            Compute fraction transmitted in one pass.

		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
			if ( Surface( SurfNum ).ExtBoundCond <= 0 ) continue;
			if ( Surface( SurfNum ).ExtBoundCond == SurfNum ) continue;
			if ( Construct( Surface( SurfNum ).Construction ).TransDiff <= 0.0 ) continue;

			NZ = Surface( SurfNum ).Zone;
			if ( ! Zone( NZ ).HasInterZoneWindow ) continue;
			MZ = Surface( Surface( SurfNum ).ExtBoundCond ).Zone;
			FractDifShortZtoZ( NZ, MZ ) += Construct( Surface( SurfNum ).Construction ).TransDiff * VMULT( NZ ) * Surface( SurfNum ).Area;
			if ( VMULT( NZ ) != 0.0 ) RecDifShortFromZ( NZ ) = true;
		}
		//          Compute fractions for multiple passes.

		Array2D< Real64 >::size_type l( 0u ), m( 0u ), d( 0u );
		for ( NZ = 1; NZ <= NumberOfZones; ++NZ, d += NumberOfZones + 1 ) {
			m = NZ - 1;
			Real64 D_d( 0.0 ); // Local accumulator
			for ( MZ = 1; MZ <= NumberOfZones; ++MZ, ++l, m += NumberOfZones ) {
				if ( MZ == NZ ) continue;
				D[ l ] = FractDifShortZtoZ[ l ] / ( 1.0 - FractDifShortZtoZ[ l ] * FractDifShortZtoZ[ m ] ); // [ l ] == ( MZ, NZ ), [ m ] == ( NZ, MZ )
				D_d += FractDifShortZtoZ[ m ] * D[ l ];
			}
			D[ d ] += D_d; // [ d ] == ( NZ, NZ )
		}

		FractDifShortZtoZ = D;
		// added for CR 7999 & 7869
		assert( FractDifShortZtoZ.isize1() == NumberOfZones );
		assert( FractDifShortZtoZ.isize2() == NumberOfZones );
		l = 0u;
		for ( NZ = 1; NZ <= NumberOfZones; ++NZ ) {
			for ( MZ = 1; MZ <= NumberOfZones; ++MZ, ++l ) {
				if ( MZ == NZ ) continue;
				if ( FractDifShortZtoZ[ l ] > 0.0 ) { // [ l ] == ( MZ, NZ )
					RecDifShortFromZ( NZ ) = true;
					break;
				}
			}
		}

		//           Compute fractions for multiple zones.

		for ( IZ = 1; IZ <= NumberOfZones; ++IZ ) {
			if ( ! RecDifShortFromZ( IZ ) ) continue;

			for ( JZ = 1; JZ <= NumberOfZones; ++JZ ) {
				if ( ! RecDifShortFromZ( JZ ) ) continue;
				if ( IZ == JZ ) continue;
				if ( D( IZ, JZ ) == 0.0 ) continue;

				for ( KZ = 1; KZ <= NumberOfZones; ++KZ ) {
					if ( ! RecDifShortFromZ( KZ ) ) continue;
					if ( IZ == KZ ) continue;
					if ( JZ == KZ ) continue;
					if ( D( JZ, KZ ) == 0.0 ) continue;
					FractDifShortZtoZ( IZ, KZ ) += D( JZ, KZ ) * D( IZ, JZ );

					for ( LZ = 1; LZ <= NumberOfZones; ++LZ ) {
						if ( ! RecDifShortFromZ( LZ ) ) continue;
						if ( IZ == LZ ) continue;
						if ( JZ == LZ ) continue;
						if ( KZ == LZ ) continue;
						if ( D( KZ, LZ ) == 0.0 ) continue;
						FractDifShortZtoZ( IZ, LZ ) += D( KZ, LZ ) * D( JZ, KZ ) * D( IZ, JZ );

						for ( MZ = 1; MZ <= NumberOfZones; ++MZ ) {
							if ( ! RecDifShortFromZ( MZ ) ) continue;
							if ( IZ == MZ ) continue;
							if ( JZ == MZ ) continue;
							if ( KZ == MZ ) continue;
							if ( LZ == MZ ) continue;
							if ( D( LZ, MZ ) == 0.0 ) continue;
							FractDifShortZtoZ( IZ, MZ ) += D( LZ, MZ ) * D( KZ, LZ ) * D( JZ, KZ ) * D( IZ, JZ );
						} // MZ Loop

					} // LZ Loop

				} // KZ Loop

			} // JZ Loop

		} // IZ Loop

	}

	void
	InitEMSControlledSurfaceProperties()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   April 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// initialize material and construction surface properties if being overriden by EMS

		// METHODOLOGY EMPLOYED:
		// update solar, thermal and visible absorptance values when actuated by EMS

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
		static bool SurfPropOverridesPresent( false ); // detect if EMS ever used for this and inits need to execute
		int MaterNum; // do loop counter over materials
		int ConstrNum; // do loop counter over constructions
		int TotLayers; // count of material layers in a construction
		int InsideMaterNum; // integer pointer for inside face's material layer
		int OutsideMaterNum; // integer pointer for outside face's material layer

		// first determine if anything needs to be done, once yes, then always init
		for ( auto const & mat : Material ) {
			if ( ( mat.AbsorpSolarEMSOverrideOn ) || ( mat.AbsorpThermalEMSOverrideOn ) || ( mat.AbsorpVisibleEMSOverrideOn ) ) {
				SurfPropOverridesPresent = true;
				break;
			}
		}

		if ( ! SurfPropOverridesPresent ) return; // quick return if nothing has ever needed to be done

		// first, loop over materials
		for ( MaterNum = 1; MaterNum <= TotMaterials; ++MaterNum ) {
			if ( Material( MaterNum ).AbsorpSolarEMSOverrideOn ) {
				Material( MaterNum ).AbsorpSolar = max( min( Material( MaterNum ).AbsorpSolarEMSOverride, 0.9999 ), 0.0001 );
			} else {
				Material( MaterNum ).AbsorpSolar = Material( MaterNum ).AbsorpSolarInput;
			}
			if ( Material( MaterNum ).AbsorpThermalEMSOverrideOn ) {
				Material( MaterNum ).AbsorpThermal = max( min( Material( MaterNum ).AbsorpThermalEMSOverride, 0.9999 ), 0.0001 );
			} else {
				Material( MaterNum ).AbsorpThermal = Material( MaterNum ).AbsorpThermalInput;
			}
			if ( Material( MaterNum ).AbsorpVisibleEMSOverrideOn ) {
				Material( MaterNum ).AbsorpVisible = max( min( Material( MaterNum ).AbsorpVisibleEMSOverride, 0.9999 ), 0.0001 );
			} else {
				Material( MaterNum ).AbsorpVisible = Material( MaterNum ).AbsorpVisibleInput;
			}
		} // loop over materials

		// second, loop over constructions
		for ( ConstrNum = 1; ConstrNum <= TotConstructs; ++ConstrNum ) {
			if ( Construct( ConstrNum ).TypeIsWindow ) continue; // only override opaque constructions
			TotLayers = Construct( ConstrNum ).TotLayers;
			if ( TotLayers == 0 ) continue; // error condition
			InsideMaterNum = Construct( ConstrNum ).LayerPoint( TotLayers );
			if ( InsideMaterNum != 0 ) {
				Construct( ConstrNum ).InsideAbsorpVis = Material( InsideMaterNum ).AbsorpVisible;
				Construct( ConstrNum ).InsideAbsorpSolar = Material( InsideMaterNum ).AbsorpSolar;
				Construct( ConstrNum ).InsideAbsorpThermal = Material( InsideMaterNum ).AbsorpThermal;
			}

			OutsideMaterNum = Construct( ConstrNum ).LayerPoint( 1 );
			if ( OutsideMaterNum != 0 ) {
				Construct( ConstrNum ).OutsideAbsorpVis = Material( OutsideMaterNum ).AbsorpVisible;
				Construct( ConstrNum ).OutsideAbsorpSolar = Material( OutsideMaterNum ).AbsorpSolar;
				Construct( ConstrNum ).OutsideAbsorpThermal = Material( OutsideMaterNum ).AbsorpThermal;
			}
		}

	}

	void
	InitEMSControlledConstructions()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Jan 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// change construction on surface if overriden by EMS

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::TrimSigDigits;
		using DataRuntimeLanguage::EMSConstructActuatorChecked;
		using DataRuntimeLanguage::EMSConstructActuatorIsOkay;
		using HeatBalFiniteDiffManager::ConstructFD;

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
		static bool SurfConstructOverridesPresent( false ); // detect if EMS ever used for this and inits need to execute

		if ( std::any_of( Surface.begin(), Surface.end(), []( DataSurfaces::SurfaceData const & e ){ return e.EMSConstructionOverrideON; } ) ) SurfConstructOverridesPresent = true;

		if ( ! SurfConstructOverridesPresent ) return;

		for ( int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {

			if ( Surface( SurfNum ).EMSConstructionOverrideON && ( Surface( SurfNum ).EMSConstructionOverrideValue > 0 ) ) {

				if ( ( EMSConstructActuatorChecked( Surface( SurfNum ).EMSConstructionOverrideValue, SurfNum ) ) && ( EMSConstructActuatorIsOkay( Surface( SurfNum ).EMSConstructionOverrideValue, SurfNum ) ) ) {

					Surface( SurfNum ).Construction = Surface( SurfNum ).EMSConstructionOverrideValue;
					Construct( Surface( SurfNum ).Construction ).IsUsed = true;

				} else { // have not checked yet or is not okay, so see if we need to warn about incompatible
					if ( ! EMSConstructActuatorChecked( Surface( SurfNum ).EMSConstructionOverrideValue, SurfNum ) ) {
						// check if constructions appear compatible

						if ( Construct( Surface( SurfNum ).EMSConstructionOverrideValue ).TypeIsWindow ) { // okay, allways allow windows
							EMSConstructActuatorChecked( Surface( SurfNum ).EMSConstructionOverrideValue, SurfNum ) = true;
							EMSConstructActuatorIsOkay( Surface( SurfNum ).EMSConstructionOverrideValue, SurfNum ) = true;

						} else if ( Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_CTF || Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_EMPD ) {
							// compare old construction to new construction and see if terms match
							// set as okay and turn false if find a big problem
							EMSConstructActuatorIsOkay( Surface( SurfNum ).EMSConstructionOverrideValue, SurfNum ) = true;
							EMSConstructActuatorChecked( Surface( SurfNum ).EMSConstructionOverrideValue, SurfNum ) = true;
							if ( Construct( Surface( SurfNum ).Construction ).NumHistories != Construct( Surface( SurfNum ).EMSConstructionOverrideValue ).NumHistories ) {
								//thow warning, but allow
								ShowWarningError( "InitEMSControlledConstructions: EMS Construction State Actuator may be unrealistic, incompatible CTF timescales are being used." );
								ShowContinueError( "Construction named = " + Construct( Surface( SurfNum ).Construction ).Name + " has CTF timesteps = " + TrimSigDigits( Construct( Surface( SurfNum ).Construction ).NumHistories ) );
								ShowContinueError( "While construction named = " + Construct( Surface( SurfNum ).EMSConstructionOverrideValue ).Name + " has CTF timesteps = " + TrimSigDigits( Construct( Surface( SurfNum ).EMSConstructionOverrideValue ).NumHistories ) );
								ShowContinueError( "Transient heat transfer modeling may not be valid for surface name = " + Surface( SurfNum ).Name + ", and the simulation continues" );

							}
							if ( Construct( Surface( SurfNum ).Construction ).NumCTFTerms != Construct( Surface( SurfNum ).EMSConstructionOverrideValue ).NumCTFTerms ) {
								//thow warning, but allow
								ShowWarningError( "InitEMSControlledConstructions: EMS Construction State Actuator may be unrealistic, incompatible CTF terms are being used." );
								ShowContinueError( "Construction named = " + Construct( Surface( SurfNum ).Construction ).Name + " has number of CTF terms = " + TrimSigDigits( Construct( Surface( SurfNum ).Construction ).NumCTFTerms ) );
								ShowContinueError( "While construction named = " + Construct( Surface( SurfNum ).EMSConstructionOverrideValue ).Name + " has number of CTF terms = " + TrimSigDigits( Construct( Surface( SurfNum ).EMSConstructionOverrideValue ).NumCTFTerms ) );
								ShowContinueError( "The actuator is allowed but the transient heat transfer modeling may not be valid for surface name = " + Surface( SurfNum ).Name + ", and the simulation continues" );

							}

							if ( Construct( Surface( SurfNum ).Construction ).SourceSinkPresent ) {
								if ( ! Construct( Surface( SurfNum ).EMSConstructionOverrideValue ).SourceSinkPresent ) {
									//thow warning, and do not allow
									ShowSevereError( "InitEMSControlledConstructions: EMS Construction State Actuator not valid." );
									ShowContinueError( "Construction named = " + Construct( Surface( SurfNum ).Construction ).Name + " has internal source/sink" );
									ShowContinueError( "While construction named = " + Construct( Surface( SurfNum ).EMSConstructionOverrideValue ).Name + " is not an internal source/sink construction" );
									ShowContinueError( "This actuator is not allowed for surface name = " + Surface( SurfNum ).Name + ", and the simulation continues without the override" );

									EMSConstructActuatorIsOkay( Surface( SurfNum ).EMSConstructionOverrideValue, SurfNum ) = false;
								}

							}

							if ( EMSConstructActuatorIsOkay( Surface( SurfNum ).EMSConstructionOverrideValue, SurfNum ) ) {
								Surface( SurfNum ).Construction = Surface( SurfNum ).EMSConstructionOverrideValue;
							}

						} else if ( Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_CondFD ) {
							EMSConstructActuatorIsOkay( Surface( SurfNum ).EMSConstructionOverrideValue, SurfNum ) = true;
							EMSConstructActuatorChecked( Surface( SurfNum ).EMSConstructionOverrideValue, SurfNum ) = true;
							if ( ConstructFD( Surface( SurfNum ).Construction ).TotNodes != ConstructFD( Surface( SurfNum ).EMSConstructionOverrideValue ).TotNodes ) {
								//thow warning, and do not allow
								ShowSevereError( "InitEMSControlledConstructions: EMS Construction State Actuator not valid." );
								ShowContinueError( "Construction named = " + Construct( Surface( SurfNum ).Construction ).Name + " has number of finite difference nodes =" + TrimSigDigits( ConstructFD( Surface( SurfNum ).Construction ).TotNodes ) );
								ShowContinueError( "While construction named = " + Construct( Surface( SurfNum ).EMSConstructionOverrideValue ).Name + "has number of finite difference nodes =" + TrimSigDigits( ConstructFD( Surface( SurfNum ).EMSConstructionOverrideValue ).TotNodes ) );
								ShowContinueError( "This actuator is not allowed for surface name = " + Surface( SurfNum ).Name + ", and the simulation continues without the override" );

								EMSConstructActuatorIsOkay( Surface( SurfNum ).EMSConstructionOverrideValue, SurfNum ) = false;
							}

							if ( Construct( Surface( SurfNum ).Construction ).SourceSinkPresent ) {
								if ( ! Construct( Surface( SurfNum ).EMSConstructionOverrideValue ).SourceSinkPresent ) {
									//thow warning, and do not allow
									ShowSevereError( "InitEMSControlledConstructions: EMS Construction State Actuator not valid." );
									ShowContinueError( "Construction named = " + Construct( Surface( SurfNum ).Construction ).Name + " has internal source/sink" );
									ShowContinueError( "While construction named = " + Construct( Surface( SurfNum ).EMSConstructionOverrideValue ).Name + " is not an internal source/sink construction" );
									ShowContinueError( "This actuator is not allowed for surface name = " + Surface( SurfNum ).Name + ", and the simulation continues without the override" );

									EMSConstructActuatorIsOkay( Surface( SurfNum ).EMSConstructionOverrideValue, SurfNum ) = false;
								}

							}

							if ( EMSConstructActuatorIsOkay( Surface( SurfNum ).EMSConstructionOverrideValue, SurfNum ) ) {
								Surface( SurfNum ).Construction = Surface( SurfNum ).EMSConstructionOverrideValue;
							}

						} else if ( Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_HAMT ) { // don't allow
							ShowSevereError( "InitEMSControlledConstructions: EMS Construction State Actuator not available with Heat transfer algorithm CombinedHeatAndMoistureFiniteElement." );
							ShowContinueError( "This actuator is not allowed for surface name = " + Surface( SurfNum ).Name + ", and the simulation continues without the override" );
							EMSConstructActuatorChecked( Surface( SurfNum ).EMSConstructionOverrideValue, SurfNum ) = true;
							EMSConstructActuatorIsOkay( Surface( SurfNum ).EMSConstructionOverrideValue, SurfNum ) = false;

						}

					} else {
						// do nothing, has been checked and is not okay with single warning already issued.
					}

				}
			} else {
				Surface( SurfNum ).Construction = Surface( SurfNum ).ConstructionStoredInputValue;
			}

		}

	}

	// End Initialization Section of the Module
	//******************************************************************************

	// Begin Algorithm Section of the Module
	//******************************************************************************

	// Beginning of Record Keeping subroutines for the HB Module
	// *****************************************************************************

	void
	UpdateFinalSurfaceHeatBalance()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   December 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// If a radiant system is present and was on for part of the time step,
		// then we probably need to make yet another pass through the heat balance.
		// This is necessary because the heat source/sink to the surface that is
		// the radiant system may have varied during the system time steps.

		// METHODOLOGY EMPLOYED:
		// First, determine whether or not the radiant system was running.  If
		// any of the Qsource terms are non-zero, then it was running.  Then,
		// update the current source terms with the "average" value calculated
		// by the radiant system algorithm.  This requires the "USE" of the
		// radiant algorithm module.  Finally, using this source value, redo
		// the inside and outside heat balances.

		// REFERENCES:
		// na

		// Using/Aliasing
		using LowTempRadiantSystem::UpdateRadSysSourceValAvg;
		using HighTempRadiantSystem::UpdateHTRadSourceValAvg;
		using HWBaseboardRadiator::UpdateBBRadSourceValAvg;
		using SteamBaseboardRadiator::UpdateBBSteamRadSourceValAvg;
		using ElectricBaseboardRadiator::UpdateBBElecRadSourceValAvg;
		using SwimmingPool::UpdatePoolSourceValAvg;

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
		bool LowTempRadSysOn; // .TRUE. if a low temperature radiant system is running
		bool HighTempRadSysOn; // .TRUE. if a high temperature radiant system is running
		bool HWBaseboardSysOn; // .TRUE. if a water baseboard heater is running
		bool SteamBaseboardSysOn; // .TRUE. if a steam baseboard heater is running
		bool ElecBaseboardSysOn; // .TRUE. if a steam baseboard heater is running
		bool SwimmingPoolOn; // true if a pool is present (running)

		// FLOW:
		UpdateRadSysSourceValAvg( LowTempRadSysOn );
		UpdateHTRadSourceValAvg( HighTempRadSysOn );
		UpdateBBRadSourceValAvg( HWBaseboardSysOn );
		UpdateBBSteamRadSourceValAvg( SteamBaseboardSysOn );
		UpdateBBElecRadSourceValAvg( ElecBaseboardSysOn );
		UpdatePoolSourceValAvg( SwimmingPoolOn );

		if ( LowTempRadSysOn || HighTempRadSysOn || HWBaseboardSysOn || SteamBaseboardSysOn || ElecBaseboardSysOn || SwimmingPoolOn ) {
			// Solve the zone heat balance 'Detailed' solution
			// Call the outside and inside surface heat balances
			CalcHeatBalanceOutsideSurf();
			CalcHeatBalanceInsideSurf();
		}

	}

	void
	UpdateThermalHistories()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   June 1990
		//       MODIFIED       na
		//       RE-ENGINEERED  Mar98 (RKS)

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates and shifts the thermal and flux histories.

		// METHODOLOGY EMPLOYED:
		// If a surface runs on the user selected subhourly time step, then the
		// history terms for the temperatures and fluxes must simply be updated
		// and shifted.  However, if the surface runs at a different (longer) time
		// step, then the "master" history series is used for the interpolated
		// update scheme.

		// REFERENCES:
		// (I)BLAST legacy routine UTHRMH
		// Taylor et.al., Impact of Simultaneous Simulation of Buildings and
		// Mechanical Systems in Heat Balance Based Energy Analysis Programs
		// on System Response and Control, Building Simulation '91, IBPSA, Nice, France.

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int HistTermNum; // DO loop counter for history terms
		int SideNum; // DO loop counter for surfaces sides (inside, outside)
		int SurfNum; // Surface number DO loop counter

		static Array1D< Real64 > QExt1; // Heat flux at the exterior surface during first time step/series
		static Array1D< Real64 > QInt1; // Heat flux at the interior surface during first time step/series
		static Array1D< Real64 > TempInt1; // Temperature of interior surface during first time step/series
		static Array1D< Real64 > TempExt1; // Temperature of exterior surface during first time step/series
		static Array1D< Real64 > Qsrc1; // Heat source/sink (during first time step/series)
		static Array1D< Real64 > Tsrc1; // Temperature at source/sink (during first time step/series)
		static Array1D< Real64 > SumTime; // Amount of time that has elapsed from start of master history to
		// the current time step


		// FLOW:

		//Tuned Assure safe to use shared linear indexing below
		assert( equal_dimensions( TH, THM ) );
		assert( equal_dimensions( TH, QH ) );
		assert( equal_dimensions( TH, QHM ) );
		assert( equal_dimensions( TsrcHist, QsrcHist ) );
		assert( equal_dimensions( TsrcHist, TsrcHistM ) );
		assert( equal_dimensions( TsrcHistM, QsrcHistM ) );

		if ( UpdateThermalHistoriesFirstTimeFlag ) {
			QExt1.dimension( TotSurfaces, 0.0 );
			QInt1.dimension( TotSurfaces, 0.0 );
			TempInt1.dimension( TotSurfaces, 0.0 );
			TempExt1.dimension( TotSurfaces, 0.0 );
			SumTime.dimension( TotSurfaces, 0.0 );
			Qsrc1.dimension( TotSurfaces, 0.0 );
			Tsrc1.dimension( TotSurfaces, 0.0 );
			UpdateThermalHistoriesFirstTimeFlag = false;
		}

		auto const l111( TH.index( 1, 1, 1 ) );
		auto const l211( TH.index( 2, 1, 1 ) );
		auto l11( l111 );
		auto l21( l211 );
		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum, ++l11, ++l21 ) { // Loop through all (heat transfer) surfaces...  [ l11 ] = ( 1, 1, SurfNum ), [ l21 ] = ( 2, 1, SurfNum )
			auto const & surface( Surface( SurfNum ) );

			if ( surface.Class == SurfaceClass_Window || ! surface.HeatTransSurf ) continue;

			if ( ( surface.HeatTransferAlgorithm != HeatTransferModel_CTF ) && ( surface.HeatTransferAlgorithm != HeatTransferModel_EMPD ) ) continue;

			int const ConstrNum( surface.Construction );
			auto const & construct( Construct( ConstrNum ) );

			if ( construct.NumCTFTerms == 0 ) continue; // Skip surfaces with no history terms

			// Sign convention for the various terms in the following two equations
			// is based on the form of the Conduction Transfer Function equation
			// given by:
			// Qin,now  = (Sum of)(Y Tout) - (Sum of)(Z Tin) + (Sum of)(F Qin,old) + (Sum of)(V Qsrc)
			// Qout,now = (Sum of)(X Tout) - (Sum of)(Y Tin) + (Sum of)(F Qout,old) + (Sum of)(W Qsrc)
			// In both equations, flux is positive from outside to inside.  The V and W terms are for radiant systems only.

			Real64 const QsrcHist1( QsrcHist( SurfNum, 1 ) );

			// Set current inside flux:
			Real64 const QH_12 = QH[ l21 ] = TH[ l11 ] * construct.CTFCross( 0 ) - TempSurfIn( SurfNum ) * construct.CTFInside( 0 ) + QsrcHist1 * construct.CTFSourceIn( 0 ) + CTFConstInPart( SurfNum ); // Heat source/sink term for radiant systems
			if ( surface.Class == SurfaceClass_Floor || surface.Class == SurfaceClass_Wall || surface.Class == SurfaceClass_IntMass || surface.Class == SurfaceClass_Roof || surface.Class == SurfaceClass_Door ) {
				OpaqSurfInsFaceConduction( SurfNum ) = surface.Area * QH_12;
				OpaqSurfInsFaceConductionFlux( SurfNum ) = QH_12; //CR 8901
				//      IF (Surface(SurfNum)%Class/=SurfaceClass_IntMass)  &
				//      ZoneOpaqSurfInsFaceCond(Surface(SurfNum)%Zone) = ZoneOpaqSurfInsFaceCond(Surface(SurfNum)%Zone) + &
				//              OpaqSurfInsFaceConduction(SurfNum)
				OpaqSurfInsFaceCondGainRep( SurfNum ) = 0.0;
				OpaqSurfInsFaceCondLossRep( SurfNum ) = 0.0;
				if ( OpaqSurfInsFaceConduction( SurfNum ) >= 0.0 ) {
					OpaqSurfInsFaceCondGainRep( SurfNum ) = OpaqSurfInsFaceConduction( SurfNum );
				} else {
					OpaqSurfInsFaceCondLossRep( SurfNum ) = -OpaqSurfInsFaceConduction( SurfNum );
				}
			}

			// Update the temperature at the source/sink location (if one is present)
			if ( construct.SourceSinkPresent ) {
				TempSource( SurfNum ) = TsrcHist( SurfNum, 1 ) = TH[ l11 ] * construct.CTFTSourceOut( 0 ) + TempSurfIn( SurfNum ) * construct.CTFTSourceIn( 0 ) + QsrcHist1 * construct.CTFTSourceQ( 0 ) + CTFTsrcConstPart( SurfNum );
			}

			if ( surface.ExtBoundCond > 0 ) continue; // Don't need to evaluate outside for partitions

			// Set current outside flux:
			QH[ l11 ] = TH[ l11 ] * construct.CTFOutside( 0 ) - TempSurfIn( SurfNum ) * construct.CTFCross( 0 ) + QsrcHist1 * construct.CTFSourceOut( 0 ) + CTFConstOutPart( SurfNum ); // Heat source/sink term for radiant systems

			if ( surface.Class == SurfaceClass_Floor || surface.Class == SurfaceClass_Wall || surface.Class == SurfaceClass_IntMass || surface.Class == SurfaceClass_Roof || surface.Class == SurfaceClass_Door ) {
				OpaqSurfOutsideFaceConductionFlux( SurfNum ) = -QH[ l11 ]; // switch sign for balance at outside face
				OpaqSurfOutsideFaceConduction( SurfNum ) = surface.Area * OpaqSurfOutsideFaceConductionFlux( SurfNum );

			}

		} // ...end of loop over all (heat transfer) surfaces...

		l11 = l111;
		l21 = l211;
		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum, ++l11, ++l21 ) { // Loop through all (heat transfer) surfaces...  [ l11 ] = ( 1, 1, SurfNum ), [ l21 ] = ( 2, 1, SurfNum )
			auto const & surface( Surface( SurfNum ) );

			if ( surface.Class == SurfaceClass_Window || ! surface.HeatTransSurf ) continue;
			if ( ( surface.HeatTransferAlgorithm != HeatTransferModel_CTF ) && ( surface.HeatTransferAlgorithm != HeatTransferModel_EMPD ) && ( surface.HeatTransferAlgorithm != HeatTransferModel_TDD ) ) continue;
			if ( SUMH( SurfNum ) == 0 ) { // First time step in a block for a surface, update arrays
				TempExt1( SurfNum ) = TH[ l11 ];
				TempInt1( SurfNum ) = TempSurfIn( SurfNum );
				Tsrc1( SurfNum ) = TsrcHist( SurfNum, 1 );
				QExt1( SurfNum ) = QH[ l11 ];
				QInt1( SurfNum ) = QH[ l21 ];
				Qsrc1( SurfNum ) = QsrcHist( SurfNum, 1 );
			}

		} // ...end of loop over all (heat transfer) surfaces...

		// SHIFT TEMPERATURE AND FLUX HISTORIES:
		// SHIFT AIR TEMP AND FLUX SHIFT VALUES WHEN AT BOTTOM OF ARRAY SPACE.
		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) { // Loop through all (heat transfer) surfaces...
			auto const & surface( Surface( SurfNum ) );

			if ( surface.Class == SurfaceClass_Window || surface.Class == SurfaceClass_TDD_Dome || ! surface.HeatTransSurf ) continue;
			if ( ( surface.HeatTransferAlgorithm != HeatTransferModel_CTF ) && ( surface.HeatTransferAlgorithm != HeatTransferModel_EMPD ) && ( surface.HeatTransferAlgorithm != HeatTransferModel_TDD ) ) continue;

			int const ConstrNum( surface.Construction );
			auto const & construct( Construct( ConstrNum ) );

			++SUMH( SurfNum );
			SumTime( SurfNum ) = double( SUMH( SurfNum ) ) * TimeStepZone;

			if ( SUMH( SurfNum ) == construct.NumHistories ) {

				SUMH( SurfNum ) = 0;

				if ( construct.NumCTFTerms > 1 ) {
					int const numCTFTerms( construct.NumCTFTerms );
					for ( SideNum = 1; SideNum <= 2; ++SideNum ) { //Tuned Index order switched for cache friendliness
						auto l( THM.index( SideNum, numCTFTerms, SurfNum ) );
						auto const li( THM.size3() );
						auto l1( l + li );
						for ( HistTermNum = numCTFTerms + 1; HistTermNum >= 3; --HistTermNum, l1 = l, l -= li ) { //Tuned Linear indexing
							//TH( SideNum, HistTermNum, SurfNum ) = THM( SideNum, HistTermNum, SurfNum ) = THM( SideNum, HistTermNum - 1, SurfNum );
							//QH( SideNum, HistTermNum, SurfNum ) = QHM( SideNum, HistTermNum, SurfNum ) = QHM( SideNum, HistTermNum - 1, SurfNum );
							TH[ l1 ] = THM[ l1 ] = THM[ l ];
							QH[ l1 ] = QHM[ l1 ] = QHM[ l ];
						}
					}
					auto m( TsrcHistM.index( SurfNum, numCTFTerms ) );
					auto m1( m + 1 );
					for ( HistTermNum = numCTFTerms + 1; HistTermNum >= 3; --HistTermNum, --m, --m1 ) { //Tuned Linear indexing
						//TsrcHist( SurfNum, HistTerm ) = TsrcHistM( SurfNum, HHistTerm ) = TsrcHistM( SurfNum, HistTermNum - 1 );
						//QsrcHist( SurfNum, HistTerm ) = QsrcHistM( SurfNum, HHistTerm ) = QsrcHistM( SurfNum, HistTermNum - 1 );
						TsrcHist[ m1 ] = TsrcHistM[ m1 ] = TsrcHistM[ m ];
						QsrcHist[ m1 ] = QsrcHistM[ m1 ] = QsrcHistM[ m ];
					}
				}

				//Tuned Linear indexing
				//THM( 1, 2, SurfNum ) = TempExt1( SurfNum );
				//THM( 2, 2, SurfNum ) = TempInt1( SurfNum );
				//TsrcHistM( SurfNum, 2 ) = Tsrc1( SurfNum );
				//QHM( 1, 2, SurfNum ) = QExt1( SurfNum );
				//QHM( 2, 2, SurfNum ) = QInt1( SurfNum );
				//QsrcHistM( SurfNum, 2 ) = Qsrc1( SurfNum );
				//
				//TH( 1, 2, SurfNum ) = THM( 1, 2, SurfNum );
				//TH( 2, 2, SurfNum ) = THM( 2, 2, SurfNum );
				//TsrcHist( SurfNum, 2 ) = TsrcHistM( SurfNum, 2 );
				//QH( 1, 2, SurfNum ) = QHM( 1, 2, SurfNum );
				//QH( 2, 2, SurfNum ) = QHM( 2, 2, SurfNum );
				//QsrcHist( SurfNum, 2 ) = QsrcHistM( SurfNum, 2 );

				auto const l21( TH.index( 1, 2, SurfNum ) ); // Linear index
				auto const l22( TH.index( 2, 2, SurfNum ) ); // Linear index
				THM[ l21 ] = TempExt1( SurfNum );
				THM[ l22 ] = TempInt1( SurfNum );
				TsrcHistM( SurfNum, 2 ) = Tsrc1( SurfNum );
				QHM[ l21 ] = QExt1( SurfNum );
				QHM[ l22 ] = QInt1( SurfNum );
				QsrcHistM( SurfNum, 2 ) = Qsrc1( SurfNum );

				TH[ l21 ] = THM[ l21 ];
				TH[ l22 ] = THM( 2, 2, SurfNum );
				TsrcHist( SurfNum, 2 ) = TsrcHistM( SurfNum, 2 );
				QH[ l21 ] = QHM[ l21 ];
				QH[ l22 ] = QHM( 2, 2, SurfNum );
				QsrcHist( SurfNum, 2 ) = QsrcHistM( SurfNum, 2 );

			} else {

				Real64 const sum_steps( SumTime( SurfNum ) / construct.CTFTimeStep );
				if ( construct.NumCTFTerms > 1 ) {
					int const numCTFTerms( construct.NumCTFTerms );
					for ( SideNum = 1; SideNum <= 2; ++SideNum ) { //Tuned Index order switched for cache friendliness
						auto l( THM.index( SideNum, numCTFTerms, SurfNum ) );
						auto const s3( THM.size3() );
						auto l1( l + s3 );
						for ( HistTermNum = numCTFTerms + 1; HistTermNum >= 3; --HistTermNum, l1 = l, l -= s3 ) { //Tuned Linear indexing
							//Real64 const THM_l1( THM( SideNum, HistTermNum, SurfNum ) );
							//TH( SideNum, HistTermNum, SurfNum ) = THM_l1 - ( THM_l1 - THM( SideNum, HistTermNum - 1, SurfNum ) ) * sum_steps;
							//Real64 const QHM_l1( QHM( SideNum, HistTermNum, SurfNum ) );
							//QH( SideNum, HistTermNum, SurfNum ) = QHM_l1 - ( QHM_l1 - QHM( SideNum, HistTermNum - 1, SurfNum ) ) * sum_steps;
							Real64 const THM_l1( THM[ l1 ] );
							TH[ l1 ] = THM_l1 - ( THM_l1 - THM[ l ] ) * sum_steps;
							Real64 const QHM_l1( QHM[ l1 ] );
							QH[ l1 ] = QHM_l1 - ( QHM_l1 - QHM[ l ] ) * sum_steps;
						}
					}
					auto m( TsrcHistM.index( SurfNum, numCTFTerms ) );
					auto m1( m + 1 );
					for ( HistTermNum = numCTFTerms + 1; HistTermNum >= 3; --HistTermNum, --m, --m1 ) { //Tuned Linear indexing [ l ] == ()
						//Real64 const TsrcHistM_elem( TsrcHistM( SurfNum, HistTermNum ) );
						//TsrcHist( SurfNum, HistTermNum ) = TsrcHistM_elem - ( TsrcHistM_elem - TsrcHistM( SurfNum, HistTermNum - 1 ) ) * sum_steps;
						//Real64 const QsrcHistM_elem( QsrcHistM( SurfNum, HistTermNum ) );
						//QsrcHist( SurfNum, HistTermNum ) = QsrcHistM_elem - ( QsrcHistM_elem - QsrcHistM( SurfNum, HistTermNum - 1 ) ) * sum_steps;
						Real64 const TsrcHistM_m1( TsrcHistM[ m1 ] );
						TsrcHist[ m1 ] = TsrcHistM_m1 - ( TsrcHistM_m1 - TsrcHistM[ m ] ) * sum_steps;
						Real64 const QsrcHistM_m1( QsrcHistM[ m1 ] );
						QsrcHist[ m1 ] = QsrcHistM_m1 - ( QsrcHistM_m1 - QsrcHistM[ m ] ) * sum_steps;
					}
				}

				//Tuned Linear indexing
				//TH( 1, 2, SurfNum ) = THM( 1, 2, SurfNum ) - ( THM( 1, 2, SurfNum ) - TempExt1( SurfNum ) ) * sum_steps;
				//TH( 2, 2, SurfNum ) = THM( 2, 2, SurfNum ) - ( THM( 2, 2, SurfNum ) - TempInt1( SurfNum ) ) * sum_steps;
				//QH( 1, 2, SurfNum ) = QHM( 1, 2, SurfNum ) - ( QHM( 1, 2, SurfNum ) - QExt1( SurfNum ) ) * sum_steps;
				//QH( 2, 2, SurfNum ) = QHM( 2, 2, SurfNum ) - ( QHM( 2, 2, SurfNum ) - QInt1( SurfNum ) ) * sum_steps;

				auto const l21( TH.index( 1, 2, SurfNum ) ); // Linear index
				auto const l22( TH.index( 2, 2, SurfNum ) ); // Linear index
				TH[ l21 ] = THM[ l21 ] - ( THM[ l21 ] - TempExt1( SurfNum ) ) * sum_steps;
				TH[ l22 ] = THM[ l22 ] - ( THM[ l22 ] - TempInt1( SurfNum ) ) * sum_steps;
				QH[ l21 ] = QHM[ l21 ] - ( QHM[ l21 ] - QExt1( SurfNum ) ) * sum_steps;
				QH[ l22 ] = QHM[ l22 ] - ( QHM[ l22 ] - QInt1( SurfNum ) ) * sum_steps;

				//Tuned Linear indexing
				//TsrcHist( SurfNum, 2 ) = TsrcHistM( SurfNum, 2 ) - ( TsrcHistM( SurfNum, 2 ) - Tsrc1( SurfNum ) ) * sum_steps;
				//QsrcHist( SurfNum, 2 ) = QsrcHistM( SurfNum, 2 ) - ( QsrcHistM( SurfNum, 2 ) - Qsrc1( SurfNum ) ) * sum_steps;

				auto const l2( TsrcHist.index( SurfNum, 2 ) );
				TsrcHist[ l2 ] = TsrcHistM[ l2 ] - ( TsrcHistM[ l2 ] - Tsrc1( SurfNum ) ) * sum_steps;
				QsrcHist[ l2 ] = QsrcHistM[ l2 ] - ( QsrcHistM[ l2 ] - Qsrc1( SurfNum ) ) * sum_steps;

			}

		} // ...end of loop over all (heat transfer) surfaces

	}

	void
	CalculateZoneMRT( Optional_int_const ZoneToResimulate ) // if passed in, then only calculate surfaces that have this zone
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   November 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the current zone MRT for thermal comfort and radiation
		// calculation purposes.

		// METHODOLOGY EMPLOYED:
		// If you have to ask...

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

		Real64 SumAET; // Intermediate calculational variable (area*emissivity*T) sum
		static Array1D< Real64 > SurfaceAE; // Product of area and emissivity for each surface
		int SurfNum; // Surface number
		static Array1D< Real64 > ZoneAESum; // Sum of area times emissivity for all zone surfaces
		int ZoneNum; // Zone number

		// FLOW:
		if ( CalculateZoneMRTfirstTime ) {
			SurfaceAE.allocate( TotSurfaces );
			ZoneAESum.allocate( NumOfZones );
			SurfaceAE = 0.0;
			ZoneAESum = 0.0;
			for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
				if ( Surface( SurfNum ).HeatTransSurf ) {
					SurfaceAE( SurfNum ) = Surface( SurfNum ).Area * Construct( Surface( SurfNum ).Construction ).InsideAbsorpThermal;
					ZoneNum = Surface( SurfNum ).Zone;
					if ( ZoneNum > 0 ) ZoneAESum( ZoneNum ) += SurfaceAE( SurfNum );
				}
			}
		}

		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
			if ( present( ZoneToResimulate ) && ( ZoneNum != ZoneToResimulate ) ) continue;
			SumAET = 0.0;
			for ( SurfNum = Zone( ZoneNum ).SurfaceFirst; SurfNum <= Zone( ZoneNum ).SurfaceLast; ++SurfNum ) {
				if ( Surface( SurfNum ).HeatTransSurf ) {
					SumAET += SurfaceAE( SurfNum ) * TempSurfIn( SurfNum );
				}
			}
			if ( ZoneAESum( ZoneNum ) > 0.01 ) {
				MRT( ZoneNum ) = SumAET / ZoneAESum( ZoneNum );
			} else {
				if ( CalculateZoneMRTfirstTime ) {
					ShowWarningError( "Zone areas*inside surface emissivities are summing to zero, for Zone=\"" + Zone( ZoneNum ).Name + "\"" );
					ShowContinueError( "As a result, MRT will be set to MAT for that zone" );
				}
				MRT( ZoneNum ) = MAT( ZoneNum );
			}
		}

		CalculateZoneMRTfirstTime = false;

	}

	// End of Record Keeping subroutines for the HB Module
	// *****************************************************************************

	// Beginning of Reporting subroutines for the HB Module
	// *****************************************************************************

	void
	ReportSurfaceHeatBalance()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   Oct 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine puts the reporting part of the HBSurface Module in one area.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using SolarShading::ReportSurfaceShading;
		using OutputReportTabular::lightSWRadSeq;
		using OutputReportTabular::feneSolarRadSeq;
		using DataGlobals::NumOfTimeStepInHour;
		using DataGlobals::CompLoadReportIsReq;
		using DataSizing::CurOverallSimDay;

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
		int SurfNum;
		int ZoneNum;
		static int TimeStepInDay( 0 );

		ZoneMRT( {1,NumOfZones} ) = MRT( {1,NumOfZones} );

		ReportSurfaceShading();

		// update inside face radiation reports
		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			Real64 const surfaceArea( Surface( SurfNum ).Area );
//Tuned Replaced by one line form below for speed
//			QdotRadNetSurfInRep( SurfNum ) = NetLWRadToSurf( SurfNum ) * surfaceArea;
//			QdotRadNetSurfInRepPerArea( SurfNum ) = NetLWRadToSurf( SurfNum );
			QdotRadNetSurfInRep( SurfNum ) = ( QdotRadNetSurfInRepPerArea( SurfNum ) = NetLWRadToSurf( SurfNum ) ) * surfaceArea;
			QRadNetSurfInReport( SurfNum ) = QdotRadNetSurfInRep( SurfNum ) * TimeStepZoneSec;

			if ( Surface( SurfNum ).Class != SurfaceClass_Window ) { // not a window...
				QdotRadSolarInRepPerArea( SurfNum ) = QRadSWInAbs( SurfNum ) - QRadSWLightsInAbs( SurfNum );
				QdotRadSolarInRep( SurfNum ) = QdotRadSolarInRepPerArea( SurfNum ) * surfaceArea;
				QRadSolarInReport( SurfNum ) = QdotRadSolarInRep( SurfNum ) * TimeStepZoneSec;

				QdotRadLightsInRepPerArea( SurfNum ) = QRadSWLightsInAbs( SurfNum );
				QdotRadLightsInRep( SurfNum ) = QdotRadLightsInRepPerArea( SurfNum ) * surfaceArea;
				QRadLightsInReport( SurfNum ) = QdotRadLightsInRep( SurfNum ) * TimeStepZoneSec;

				if ( ZoneSizingCalc && CompLoadReportIsReq ) {
					TimeStepInDay = ( HourOfDay - 1 ) * NumOfTimeStepInHour + TimeStep;
					lightSWRadSeq( CurOverallSimDay, TimeStepInDay, SurfNum ) = QdotRadLightsInRep( SurfNum );
					feneSolarRadSeq( CurOverallSimDay, TimeStepInDay, SurfNum ) = QdotRadSolarInRep( SurfNum );
				}
			} else { // can we fill these for windows?

			}

//Tuned Replaced by one line form below for speed
//			QdotRadIntGainsInRepPerArea( SurfNum ) = QRadThermInAbs( SurfNum );
//			QdotRadIntGainsInRep( SurfNum ) = QdotRadIntGainsInRepPerArea( SurfNum ) * surfaceArea;
			QdotRadIntGainsInRep( SurfNum ) = ( QdotRadIntGainsInRepPerArea( SurfNum ) = QRadThermInAbs( SurfNum ) ) * surfaceArea;
			QRadIntGainsInReport( SurfNum ) = QdotRadIntGainsInRep( SurfNum ) * TimeStepZoneSec;

//Tuned Replaced by one line form below for speed
//			QdotRadHVACInRepPerArea( SurfNum ) = QHTRadSysSurf( SurfNum ) + QHWBaseboardSurf( SurfNum ) + QSteamBaseboardSurf( SurfNum ) + QElecBaseboardSurf( SurfNum );
//			QdotRadHVACInRep( SurfNum ) = QdotRadHVACInRepPerArea( SurfNum ) * surfaceArea;
			QdotRadHVACInRep( SurfNum ) = ( QdotRadHVACInRepPerArea( SurfNum ) = QHTRadSysSurf( SurfNum ) + QHWBaseboardSurf( SurfNum ) + QSteamBaseboardSurf( SurfNum ) + QElecBaseboardSurf( SurfNum ) ) * surfaceArea;
			QRadHVACInReport( SurfNum ) = QdotRadHVACInRep( SurfNum ) * TimeStepZoneSec;

			if ( Surface( SurfNum ).Class == SurfaceClass_Floor || Surface( SurfNum ).Class == SurfaceClass_Wall || Surface( SurfNum ).Class == SurfaceClass_IntMass || Surface( SurfNum ).Class == SurfaceClass_Roof || Surface( SurfNum ).Class == SurfaceClass_Door ) {

				// inside face conduction updates
				OpaqSurfInsFaceConductionEnergy( SurfNum ) = OpaqSurfInsFaceConduction( SurfNum ) * TimeStepZoneSec;
				ZoneOpaqSurfInsFaceCond( Surface( SurfNum ).Zone ) += OpaqSurfInsFaceConduction( SurfNum );
				OpaqSurfInsFaceCondGainRep( SurfNum ) = 0.0;
				OpaqSurfInsFaceCondLossRep( SurfNum ) = 0.0;
				if ( OpaqSurfInsFaceConduction( SurfNum ) >= 0.0 ) {
					OpaqSurfInsFaceCondGainRep( SurfNum ) = OpaqSurfInsFaceConduction( SurfNum );
				} else {
					OpaqSurfInsFaceCondLossRep( SurfNum ) = -OpaqSurfInsFaceConduction( SurfNum );
				}

				// outside face conduction updates
				OpaqSurfOutsideFaceConductionEnergy( SurfNum ) = OpaqSurfOutsideFaceConduction( SurfNum ) * TimeStepZoneSec;
				ZoneOpaqSurfExtFaceCond( Surface( SurfNum ).Zone ) += OpaqSurfOutsideFaceConduction( SurfNum );
				OpaqSurfExtFaceCondGainRep( SurfNum ) = 0.0;
				OpaqSurfExtFaceCondLossRep( SurfNum ) = 0.0;
				if ( OpaqSurfOutsideFaceConduction( SurfNum ) >= 0.0 ) {
					OpaqSurfExtFaceCondGainRep( SurfNum ) = OpaqSurfOutsideFaceConduction( SurfNum );
				} else {
					OpaqSurfExtFaceCondLossRep( SurfNum ) = -OpaqSurfOutsideFaceConduction( SurfNum );
				}

				// do average surface conduction updates

				OpaqSurfAvgFaceConduction( SurfNum ) = ( OpaqSurfInsFaceConduction( SurfNum ) - OpaqSurfOutsideFaceConduction( SurfNum ) ) / 2.0;
				OpaqSurfAvgFaceConductionFlux( SurfNum ) = ( OpaqSurfInsFaceConductionFlux( SurfNum ) - OpaqSurfOutsideFaceConductionFlux( SurfNum ) ) / 2.0;
				OpaqSurfAvgFaceConductionEnergy( SurfNum ) = OpaqSurfAvgFaceConduction( SurfNum ) * TimeStepZoneSec;
				OpaqSurfAvgFaceCondGainRep( SurfNum ) = 0.0;
				OpaqSurfAvgFaceCondLossRep( SurfNum ) = 0.0;
				if ( OpaqSurfAvgFaceConduction( SurfNum ) >= 0.0 ) {
					OpaqSurfAvgFaceCondGainRep( SurfNum ) = OpaqSurfAvgFaceConduction( SurfNum );
				} else {
					OpaqSurfAvgFaceCondLossRep( SurfNum ) = -OpaqSurfAvgFaceConduction( SurfNum );
				}

				// do surface storage rate updates
				OpaqSurfStorageConductionFlux( SurfNum ) = -( OpaqSurfInsFaceConductionFlux( SurfNum ) + OpaqSurfOutsideFaceConductionFlux( SurfNum ) );
				OpaqSurfStorageConduction( SurfNum ) = -( OpaqSurfInsFaceConduction( SurfNum ) + OpaqSurfOutsideFaceConduction( SurfNum ) );
				OpaqSurfStorageConductionEnergy( SurfNum ) = OpaqSurfStorageConduction( SurfNum ) * TimeStepZoneSec;
				OpaqSurfStorageGainRep( SurfNum ) = 0.0;
				OpaqSurfStorageCondLossRep( SurfNum ) = 0.0;
				if ( OpaqSurfStorageConduction( SurfNum ) >= 0.0 ) {
					OpaqSurfStorageGainRep( SurfNum ) = OpaqSurfStorageConduction( SurfNum );
				} else {
					OpaqSurfStorageCondLossRep( SurfNum ) = -OpaqSurfStorageConduction( SurfNum );
				}

			} // opaque heat transfer surfaces.

		} // loop over surfaces

		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
			if ( ZoneOpaqSurfInsFaceCond( ZoneNum ) >= 0.0 ) {
				ZoneOpaqSurfInsFaceCondGainRep( ZoneNum ) = ZoneOpaqSurfInsFaceCond( ZoneNum );
				ZnOpqSurfInsFaceCondGnRepEnrg( ZoneNum ) = ZoneOpaqSurfInsFaceCondGainRep( ZoneNum ) * TimeStepZoneSec;
			} else {
				ZoneOpaqSurfInsFaceCondLossRep( ZoneNum ) = -ZoneOpaqSurfInsFaceCond( ZoneNum );
				ZnOpqSurfInsFaceCondLsRepEnrg( ZoneNum ) = ZoneOpaqSurfInsFaceCondLossRep( ZoneNum ) * TimeStepZoneSec;
			}

			if ( ZoneOpaqSurfExtFaceCond( ZoneNum ) >= 0.0 ) {
				ZoneOpaqSurfExtFaceCondGainRep( ZoneNum ) = ZoneOpaqSurfExtFaceCond( ZoneNum );
				ZnOpqSurfExtFaceCondGnRepEnrg( ZoneNum ) = ZoneOpaqSurfExtFaceCondGainRep( ZoneNum ) * TimeStepZoneSec;
			} else {
				ZoneOpaqSurfExtFaceCondLossRep( ZoneNum ) = -ZoneOpaqSurfExtFaceCond( ZoneNum );
				ZnOpqSurfExtFaceCondLsRepEnrg( ZoneNum ) = ZoneOpaqSurfExtFaceCondLossRep( ZoneNum ) * TimeStepZoneSec;
			}
		} // loop over zones

	}

	// End of Reporting subroutines for the HB Module
	// *****************************************************************************



// *****************************************************************************
// *****************************************************************************
// *****************************************************************************
// *****************************************************************************

// Formerly EXTERNAL SUBROUTINES (heavily related to HeatBalanceSurfaceManager) now moved into namespace

void
CalcHeatBalanceOutsideSurf( Optional_int_const ZoneToResimulate ) // if passed in, then only calculate surfaces that have this zone
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         George Walton
	//       DATE WRITTEN   December 1979
	//       MODIFIED       Jun 1990 (RDT for new CTF arrays);
	//                      Aug 2000 (RJL for MTF moisture calculations)
	//                      Sep 2000 (RKS for new radiant exchange algorithm)
	//                      Dec 2000 (RKS for radiant system model addition)
	//                      Apr 2002 (COP removed denominator from OSC calculation
	//                      Jul 2008 (P.Biddulph include calls to HAMT)
	//                      Jul 2011, M.J. Witte and C.O. Pedersen, add new fields to OSC for last T, max and min
	//                      Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems
	//       RE-ENGINEERED  Mar 1998 (RKS)

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine performs a heat balance on the outside face of each
	// surface in the building.

	// METHODOLOGY EMPLOYED:
	// Various boundary conditions are set and additional parameters are set-
	// up.  Then, the proper heat balance equation is selected based on the
	// presence of movable insulation, thermal mass of the surface construction,
	// and convection model being used.

	// REFERENCES:
	// (I)BLAST legacy routine HBOUT
	// 1989 ASHRAE Handbook of Fundamentals (Figure 1 on p. 22.4, convection correlations)

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGlobals;
	using namespace DataEnvironment;
	using namespace DataHeatBalFanSys;
	using namespace DataHeatBalance;
	using namespace DataHeatBalSurface;
	using namespace DataSurfaces;
	using DataMoistureBalance::TempOutsideAirFD;
	using DataMoistureBalance::RhoVaporAirOut;
	using DataMoistureBalance::RhoVaporAirIn;
	using DataMoistureBalance::HConvExtFD;
	using DataMoistureBalance::HMassConvExtFD;
	using DataMoistureBalance::HConvInFD;
	using DataMoistureBalance::HMassConvInFD;
	using DataMoistureBalance::RhoVaporSurfIn;
	using DataMoistureBalance::HSkyFD;
	using DataMoistureBalance::HGrndFD;
	using DataMoistureBalance::HAirFD;
	using HeatBalanceMovableInsulation::EvalOutsideMovableInsulation;
	using ConvectionCoefficients::InitExteriorConvectionCoeff;
	using ConvectionCoefficients::SetExtConvectionCoeff;
	using ConvectionCoefficients::SetIntConvectionCoeff;
	using HeatBalanceIntRadExchange::CalcInteriorRadExchange;
	using ScheduleManager::GetCurrentScheduleValue;
	using ScheduleManager::GetScheduleIndex;
	using namespace Psychrometrics;
	using EcoRoofManager::CalcEcoRoof;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	static std::string const RoutineName( "CalcHeatBalanceOutsideSurf" );
	static std::string const RoutineNameGroundTemp( "CalcHeatBalanceOutsideSurf:GroundTemp" );
	static std::string const RoutineNameGroundTempFC( "CalcHeatBalanceOutsideSurf:GroundTempFC" );
	static std::string const RoutineNameOtherSideCoefNoCalcExt( "CalcHeatBalanceOutsideSurf:OtherSideCoefNoCalcExt" );
	static std::string const RoutineNameOtherSideCoefCalcExt( "CalcHeatBalanceOutsideSurf:OtherSideCoefCalcExt" );
	static std::string const RoutineNameOSCM( "CalcHeatBalanceOutsideSurf:OSCM" );
	static std::string const RoutineNameExtEnvWetSurf( "CalcHeatBalanceOutsideSurf:extEnvWetSurf" );
	static std::string const RoutineNameExtEnvDrySurf( "CalcHeatBalanceOutsideSurf:extEnvDrySurf" );
	static std::string const RoutineNameNoWind( "CalcHeatBalanceOutsideSurf:nowind" );
	static std::string const RoutineNameOther( "CalcHeatBalanceOutsideSurf:interior/other" );
	static std::string const RoutineNameIZPart( "CalcHeatBalanceOutsideSurf:IZPart" );
	static std::string const HBSurfManGroundHAMT( "HBSurfMan:Ground:HAMT" );
	static std::string const HBSurfManRainHAMT( "HBSurfMan:Rain:HAMT" );
	static std::string const HBSurfManDrySurfCondFD( "HBSurfMan:DrySurf:CondFD" );
	static std::string const Outside( "Outside" );
	static std::string const BlankString;

	// INTERFACE BLOCK SPECIFICATIONS:
	// na

	// DERIVED TYPE DEFINITIONS:
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	Real64 AbsThermSurf; // Thermal absoptance of the exterior surface
	int ConstrNum; // Construction index for the current surface
	Real64 HGround; // "Convection" coefficient from ground to surface
	Real64 HMovInsul; // "Convection" coefficient of movable insulation
	Real64 HSky; // "Convection" coefficient from sky to surface
	Real64 HAir; // "Convection" coefficient from air to surface (radiation)
	Real64 ConstantTempCoef; // Temperature Coefficient as input or modified using sine wave  COP mod
	int RoughSurf; // Roughness index of the exterior surface
	int SurfNum; // Surface number DO loop counter
	Real64 TempExt; // Exterior temperature boundary condition
	int ZoneNum; // Zone number the current surface is attached to
	int OPtr;
	Real64 RhoVaporSat; // Local temporary saturated vapor density for checking

	// FUNCTION DEFINITIONS:
	// na

	// FLOW:
	for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
		// Need to transfer any source/sink for a surface to the local array.  Note that
		// the local array is flux (W/m2) while the QRadSysSource is heat transfer (W).
		// This must be done at this location so that this is always updated correctly.
		if ( Surface( SurfNum ).Area > 0.0 ) QsrcHist( SurfNum, 1 ) = QRadSysSource( SurfNum ) / Surface( SurfNum ).Area; // Make sure we don't divide by zero...

		// next we add source (actually a sink) from any integrated PV
		if ( Surface( SurfNum ).Area > 0.0 ) QsrcHist( SurfNum, 1 ) += QPVSysSource( SurfNum ) / Surface( SurfNum ).Area; // Make sure we don't divide by zero...
	}

	if ( present( ZoneToResimulate ) ) {
		CalcInteriorRadExchange( TH( 2, 1, _ ), 0, NetLWRadToSurf, ZoneToResimulate, Outside );
	} else {
		CalcInteriorRadExchange( TH( 2, 1, _ ), 0, NetLWRadToSurf, _, Outside );
	}

	for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) { // Loop through all surfaces...

		ZoneNum = Surface( SurfNum ).Zone;

		if ( present( ZoneToResimulate ) ) {
			if ( ( ZoneNum != ZoneToResimulate ) && ( AdjacentZoneToSurface( SurfNum ) != ZoneToResimulate ) ) {
				continue; // skip surfaces that are not associated with this zone
			}
		}

		if ( ! Surface( SurfNum ).HeatTransSurf || ZoneNum == 0 ) continue; // Skip non-heat transfer surfaces

		if ( Surface( SurfNum ).Class == SurfaceClass_Window ) continue;
		// Interior windows in partitions use "normal" heat balance calculations
		// For rest, Outside surface temp of windows not needed in Window5 calculation approach.
		// Window layer temperatures are calculated in CalcHeatBalanceInsideSurf

		// Initializations for this surface
		ConstrNum = Surface( SurfNum ).Construction;
		HMovInsul = 0.0;
		HSky = 0.0;
		HGround = 0.0;
		HAir = 0.0;
		HcExtSurf( SurfNum ) = 0.0;
		HAirExtSurf( SurfNum ) = 0.0;
		HSkyExtSurf( SurfNum ) = 0.0;
		HGrdExtSurf( SurfNum ) = 0.0;

		// Calculate the current outside surface temperature TH(SurfNum,1,1) for the
		// various different boundary conditions
		{ auto const SELECT_CASE_var( Surface( SurfNum ).ExtBoundCond );

		if ( SELECT_CASE_var == Ground ) { // Surface in contact with ground

			TH( 1, 1, SurfNum ) = GroundTemp;

			// Set the only radiant system heat balance coefficient that is non-zero for this case
			if ( Construct( ConstrNum ).SourceSinkPresent ) RadSysToHBConstCoef( SurfNum ) = TH( 1, 1, SurfNum );

			// start HAMT
			if ( Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_HAMT ) {
				// Set variables used in the HAMT moisture balance
				TempOutsideAirFD( SurfNum ) = GroundTemp;
				RhoVaporAirOut( SurfNum ) = PsyRhovFnTdbRh( GroundTemp, 1.0, HBSurfManGroundHAMT );
				HConvExtFD( SurfNum ) = HighHConvLimit;

				HMassConvExtFD( SurfNum ) = HConvExtFD( SurfNum ) / ( ( PsyRhoAirFnPbTdbW( OutBaroPress, GroundTemp, PsyWFnTdbRhPb( GroundTemp, 1.0, OutBaroPress, RoutineNameGroundTemp ) ) + RhoVaporAirOut( SurfNum ) ) * PsyCpAirFnWTdb( OutHumRat, GroundTemp ) );

				HSkyFD( SurfNum ) = HSky;
				HGrndFD( SurfNum ) = HGround;
				HAirFD( SurfNum ) = HAir;
			}
			// end HAMT

			if ( Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_CondFD ) {
				// Set variables used in the FD moisture balance
				TempOutsideAirFD( SurfNum ) = GroundTemp;
				RhoVaporAirOut( SurfNum ) = PsyRhovFnTdbRhLBnd0C( GroundTemp, 1.0 );
				HConvExtFD( SurfNum ) = HighHConvLimit;
				HMassConvExtFD( SurfNum ) = HConvExtFD( SurfNum ) / ( ( PsyRhoAirFnPbTdbW( OutBaroPress, GroundTemp, PsyWFnTdbRhPb( GroundTemp, 1.0, OutBaroPress, RoutineNameGroundTemp ) ) + RhoVaporAirOut( SurfNum ) ) * PsyCpAirFnWTdb( OutHumRat, GroundTemp ) );
				HSkyFD( SurfNum ) = HSky;
				HGrndFD( SurfNum ) = HGround;
				HAirFD( SurfNum ) = HAir;
			}

			// Added for FCfactor grounds
		} else if ( SELECT_CASE_var == GroundFCfactorMethod ) { // Surface in contact with ground

			TH( 1, 1, SurfNum ) = GroundTempFC;

			// Set the only radiant system heat balance coefficient that is non-zero for this case
			if ( Construct( ConstrNum ).SourceSinkPresent ) RadSysToHBConstCoef( SurfNum ) = TH( 1, 1, SurfNum );

			if ( Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_HAMT ) {
				// Set variables used in the HAMT moisture balance
				TempOutsideAirFD( SurfNum ) = GroundTempFC;
				RhoVaporAirOut( SurfNum ) = PsyRhovFnTdbRh( GroundTempFC, 1.0, HBSurfManGroundHAMT );
				HConvExtFD( SurfNum ) = HighHConvLimit;

				HMassConvExtFD( SurfNum ) = HConvExtFD( SurfNum ) / ( ( PsyRhoAirFnPbTdbW( OutBaroPress, GroundTempFC, PsyWFnTdbRhPb( GroundTempFC, 1.0, OutBaroPress, RoutineNameGroundTempFC ) ) + RhoVaporAirOut( SurfNum ) ) * PsyCpAirFnWTdb( OutHumRat, GroundTempFC ) );

				HSkyFD( SurfNum ) = HSky;
				HGrndFD( SurfNum ) = HGround;
				HAirFD( SurfNum ) = HAir;
			}

			if ( Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_CondFD ) {
				// Set variables used in the FD moisture balance
				TempOutsideAirFD( SurfNum ) = GroundTempFC;
				RhoVaporAirOut( SurfNum ) = PsyRhovFnTdbRhLBnd0C( GroundTempFC, 1.0 );
				HConvExtFD( SurfNum ) = HighHConvLimit;
				HMassConvExtFD( SurfNum ) = HConvExtFD( SurfNum ) / ( ( PsyRhoAirFnPbTdbW( OutBaroPress, GroundTempFC, PsyWFnTdbRhPb( GroundTempFC, 1.0, OutBaroPress, RoutineNameGroundTempFC ) ) + RhoVaporAirOut( SurfNum ) ) * PsyCpAirFnWTdb( OutHumRat, GroundTempFC ) );
				HSkyFD( SurfNum ) = HSky;
				HGrndFD( SurfNum ) = HGround;
				HAirFD( SurfNum ) = HAir;
			}

		} else if ( SELECT_CASE_var == OtherSideCoefNoCalcExt ) {
			// Use Other Side Coefficients to determine the surface film coefficient and
			// the exterior boundary condition temperature

			OPtr = Surface( SurfNum ).OSCPtr;
			// Set surface temp from previous timestep
			if ( BeginTimeStepFlag ) {
				OSC( OPtr ).TOutsideSurfPast = TH( 1, 1, SurfNum );
			}

			if ( OSC( OPtr ).ConstTempScheduleIndex != 0 ) { // Determine outside temperature from schedule
				OSC( OPtr ).ConstTemp = GetCurrentScheduleValue( OSC( OPtr ).ConstTempScheduleIndex );
			}

			//  Allow for modification of TemperatureCoefficient with unitary sine wave.
			if ( OSC( OPtr ).SinusoidalConstTempCoef ) { // Sine wave C4
				ConstantTempCoef = std::sin( 2 * Pi * CurrentTime / OSC( OPtr ).SinusoidPeriod );
			} else {
				ConstantTempCoef = OSC( OPtr ).ConstTempCoef;
			}

			OSC( OPtr ).OSCTempCalc = ( OSC( OPtr ).ZoneAirTempCoef * MAT( ZoneNum ) + OSC( OPtr ).ExtDryBulbCoef * Surface( SurfNum ).OutDryBulbTemp + ConstantTempCoef * OSC( OPtr ).ConstTemp + OSC( OPtr ).GroundTempCoef * GroundTemp + OSC( OPtr ).WindSpeedCoef * Surface( SurfNum ).WindSpeed * Surface( SurfNum ).OutDryBulbTemp + OSC( OPtr ).TPreviousCoef * OSC( OPtr ).TOutsideSurfPast );

			// Enforce max/min limits if applicable
			if ( OSC( OPtr ).MinLimitPresent ) OSC( OPtr ).OSCTempCalc = max( OSC( OPtr ).MinTempLimit, OSC( OPtr ).OSCTempCalc );
			if ( OSC( OPtr ).MaxLimitPresent ) OSC( OPtr ).OSCTempCalc = min( OSC( OPtr ).MaxTempLimit, OSC( OPtr ).OSCTempCalc );

			TH( 1, 1, SurfNum ) = OSC( OPtr ).OSCTempCalc;

			// Set the only radiant system heat balance coefficient that is non-zero for this case
			if ( Construct( ConstrNum ).SourceSinkPresent ) RadSysToHBConstCoef( SurfNum ) = TH( 1, 1, SurfNum );

			if ( Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_CondFD || Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_HAMT ) {
				// Set variables used in the FD moisture balance and HAMT
				TempOutsideAirFD( SurfNum ) = TH( 1, 1, SurfNum );
				RhoVaporAirOut( SurfNum ) = PsyRhovFnTdbWPb( TempOutsideAirFD( SurfNum ), OutHumRat, OutBaroPress );
				HConvExtFD( SurfNum ) = HighHConvLimit;
				HMassConvExtFD( SurfNum ) = HConvExtFD( SurfNum ) / ( ( PsyRhoAirFnPbTdbW( OutBaroPress, TempOutsideAirFD( SurfNum ), PsyWFnTdbRhPb( TempOutsideAirFD( SurfNum ), 1.0, OutBaroPress, RoutineNameOtherSideCoefNoCalcExt ) ) + RhoVaporAirOut( SurfNum ) ) * PsyCpAirFnWTdb( OutHumRat, TempOutsideAirFD( SurfNum ) ) );
				HSkyFD( SurfNum ) = HSky;
				HGrndFD( SurfNum ) = HGround;
				HAirFD( SurfNum ) = HAir;
			}

			// This ends the calculations for this surface and goes on to the next SurfNum

		} else if ( SELECT_CASE_var == OtherSideCoefCalcExt ) { // A surface with other side coefficients that define the outside environment

			// First, set up the outside convection coefficient and the exterior temperature
			// boundary condition for the surface
			OPtr = Surface( SurfNum ).OSCPtr;
			// Set surface temp from previous timestep
			if ( BeginTimeStepFlag ) {
				OSC( OPtr ).TOutsideSurfPast = TH( 1, 1, SurfNum );
			}

			if ( OSC( OPtr ).ConstTempScheduleIndex != 0 ) { // Determine outside temperature from schedule
				OSC( OPtr ).ConstTemp = GetCurrentScheduleValue( OSC( OPtr ).ConstTempScheduleIndex );
			}

			HcExtSurf( SurfNum ) = OSC( OPtr ).SurfFilmCoef;

			OSC( OPtr ).OSCTempCalc = ( OSC( OPtr ).ZoneAirTempCoef * MAT( ZoneNum ) + OSC( OPtr ).ExtDryBulbCoef * Surface( SurfNum ).OutDryBulbTemp + OSC( OPtr ).ConstTempCoef * OSC( OPtr ).ConstTemp + OSC( OPtr ).GroundTempCoef * GroundTemp + OSC( OPtr ).WindSpeedCoef * Surface( SurfNum ).WindSpeed * Surface( SurfNum ).OutDryBulbTemp + OSC( OPtr ).TPreviousCoef * OSC( OPtr ).TOutsideSurfPast );

			// Enforce max/min limits if applicable
			if ( OSC( OPtr ).MinLimitPresent ) OSC( OPtr ).OSCTempCalc = max( OSC( OPtr ).MinTempLimit, OSC( OPtr ).OSCTempCalc );
			if ( OSC( OPtr ).MaxLimitPresent ) OSC( OPtr ).OSCTempCalc = min( OSC( OPtr ).MaxTempLimit, OSC( OPtr ).OSCTempCalc );

			TempExt = OSC( OPtr ).OSCTempCalc;

			// Set the only radiant system heat balance coefficient that is non-zero for this case
			if ( Construct( ConstrNum ).SourceSinkPresent ) RadSysToHBConstCoef( SurfNum ) = TH( 1, 1, SurfNum );

			if ( Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_CondFD || Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_HAMT ) {
				// Set variables used in the FD moisture balance and HAMT
				TempOutsideAirFD( SurfNum ) = TempExt;
				RhoVaporAirOut( SurfNum ) = PsyRhovFnTdbWPb( TempOutsideAirFD( SurfNum ), OutHumRat, OutBaroPress );
				HConvExtFD( SurfNum ) = HcExtSurf( SurfNum );
				HMassConvExtFD( SurfNum ) = HConvExtFD( SurfNum ) / ( ( PsyRhoAirFnPbTdbW( OutBaroPress, TempOutsideAirFD( SurfNum ), PsyWFnTdbRhPb( TempOutsideAirFD( SurfNum ), 1.0, OutBaroPress, RoutineNameOtherSideCoefCalcExt ) ) + RhoVaporAirOut( SurfNum ) ) * PsyCpAirFnWTdb( OutHumRat, TempOutsideAirFD( SurfNum ) ) );
				HSkyFD( SurfNum ) = HSkyExtSurf( SurfNum );
				HGrndFD( SurfNum ) = HGrdExtSurf( SurfNum );
				HAirFD( SurfNum ) = HAirExtSurf( SurfNum );
			}

			// Call the outside surface temp calculation and pass the necessary terms
			if ( Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_CTF || Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_EMPD ) CalcOutsideSurfTemp( SurfNum, ZoneNum, ConstrNum, HMovInsul, TempExt );

			// This ends the calculations for this surface and goes on to the next SurfNum

		} else if ( SELECT_CASE_var == OtherSideCondModeledExt ) { // A surface with other side conditions determined from seperate, dynamic component
			//                               modeling that defines the "outside environment"

			// First, set up the outside convection coefficient and the exterior temperature
			// boundary condition for the surface
			OPtr = Surface( SurfNum ).OSCMPtr;
			// EMS overrides
			if ( OSCM( OPtr ).EMSOverrideOnTConv ) OSCM( OPtr ).TConv = OSCM( OPtr ).EMSOverrideTConvValue;
			if ( OSCM( OPtr ).EMSOverrideOnHConv ) OSCM( OPtr ).HConv = OSCM( OPtr ).EMSOverrideHConvValue;
			if ( OSCM( OPtr ).EMSOverrideOnTRad ) OSCM( OPtr ).TRad = OSCM( OPtr ).EMSOverrideTRadValue;
			if ( OSCM( OPtr ).EMSOverrideOnHrad ) OSCM( OPtr ).HRad = OSCM( OPtr ).EMSOverrideHradValue;
			HcExtSurf( SurfNum ) = OSCM( OPtr ).HConv;

			TempExt = OSCM( OPtr ).TConv;

			// Set the only radiant system heat balance coefficient that is non-zero for this case
			if ( Construct( ConstrNum ).SourceSinkPresent ) RadSysToHBConstCoef( SurfNum ) = TH( 1, 1, SurfNum );

			if ( Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_CondFD || Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_HAMT ) {
				// Set variables used in the FD moisture balance and HAMT
				TempOutsideAirFD( SurfNum ) = TempExt;
				RhoVaporAirOut( SurfNum ) = PsyRhovFnTdbWPb( TempOutsideAirFD( SurfNum ), OutHumRat, OutBaroPress );
				HConvExtFD( SurfNum ) = HcExtSurf( SurfNum );
				HMassConvExtFD( SurfNum ) = HConvExtFD( SurfNum ) / ( ( PsyRhoAirFnPbTdbW( OutBaroPress, TempOutsideAirFD( SurfNum ), PsyWFnTdbRhPb( TempOutsideAirFD( SurfNum ), 1.0, OutBaroPress, RoutineNameOSCM ) ) + RhoVaporAirOut( SurfNum ) ) * PsyCpAirFnWTdb( OutHumRat, TempOutsideAirFD( SurfNum ) ) );
				HSkyFD( SurfNum ) = OSCM( OPtr ).HRad; //CR 8046, use sky term for surface to baffle IR
				HGrndFD( SurfNum ) = 0.0; //CR 8046, null out and use only sky term for surface to baffle IR
				HAirFD( SurfNum ) = 0.0; //CR 8046, null out and use only sky term for surface to baffle IR
			}

			// Call the outside surface temp calculation and pass the necessary terms
			if ( Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_CTF || Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_EMPD ) {

				if ( Surface( SurfNum ).ExtCavityPresent ) {
					CalcExteriorVentedCavity( SurfNum );
				}

				CalcOutsideSurfTemp( SurfNum, ZoneNum, ConstrNum, HMovInsul, TempExt );
			} else if ( Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_CondFD || Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_HAMT ) {
				if ( Surface( SurfNum ).ExtCavityPresent ) {
					CalcExteriorVentedCavity( SurfNum );
				}
			}

			// This ends the calculations for this surface and goes on to the next SurfNum
		} else if ( SELECT_CASE_var == ExternalEnvironment ) {

			//checking the EcoRoof presented in the external environment
			// recompute each load by calling ecoroof

			if ( Surface( SurfNum ).ExtEcoRoof ) {
				CalcEcoRoof( SurfNum, ZoneNum, ConstrNum, TempExt );
				continue;
			}

			if ( SurfaceWindow( SurfNum ).StormWinFlag == 1 ) ConstrNum = Surface( SurfNum ).StormWinConstruction;
			RoughSurf = Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Roughness;
			AbsThermSurf = Material( Construct( ConstrNum ).LayerPoint( 1 ) ).AbsorpThermal;

			// Check for outside movable insulation
			if ( Surface( SurfNum ).MaterialMovInsulExt > 0 ) EvalOutsideMovableInsulation( SurfNum, HMovInsul, RoughSurf, AbsThermSurf );

			// Check for exposure to wind (exterior environment)
			if ( Surface( SurfNum ).ExtWind ) {

				// Calculate exterior heat transfer coefficients with windspeed (windspeed is calculated internally in subroutine)
				InitExteriorConvectionCoeff( SurfNum, HMovInsul, RoughSurf, AbsThermSurf, TH( 1, 1, SurfNum ), HcExtSurf( SurfNum ), HSkyExtSurf( SurfNum ), HGrdExtSurf( SurfNum ), HAirExtSurf( SurfNum ) );

				if ( IsRain ) { // Raining: since wind exposed, outside surface gets wet

					if ( Surface( SurfNum ).ExtConvCoeff <= 0 ) { // Reset HcExtSurf because of wetness
						HcExtSurf( SurfNum ) = 1000.0;
					} else { // User set
						HcExtSurf( SurfNum ) = SetExtConvectionCoeff( SurfNum );
					}

					TempExt = Surface( SurfNum ).OutWetBulbTemp;

					// start HAMT
					if ( Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_HAMT ) {
						// Set variables used in the HAMT moisture balance
						TempOutsideAirFD( SurfNum ) = TempExt;
						RhoVaporAirOut( SurfNum ) = PsyRhovFnTdbRh( TempOutsideAirFD( SurfNum ), 1.0, HBSurfManRainHAMT );
						HConvExtFD( SurfNum ) = HcExtSurf( SurfNum );
						HMassConvExtFD( SurfNum ) = HConvExtFD( SurfNum ) / ( ( PsyRhoAirFnPbTdbW( OutBaroPress, TempOutsideAirFD( SurfNum ), PsyWFnTdbRhPb( TempOutsideAirFD( SurfNum ), 1.0, OutBaroPress, RoutineNameExtEnvWetSurf ) ) + RhoVaporAirOut( SurfNum ) ) * PsyCpAirFnWTdb( OutHumRat, TempOutsideAirFD( SurfNum ) ) );
						HSkyFD( SurfNum ) = HSkyExtSurf( SurfNum );
						HGrndFD( SurfNum ) = HGrdExtSurf( SurfNum );
						HAirFD( SurfNum ) = HAirExtSurf( SurfNum );
					}
					// end HAMT

					if ( Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_CondFD ) {
						// Set variables used in the FD moisture balance
						TempOutsideAirFD( SurfNum ) = TempExt;
						RhoVaporAirOut( SurfNum ) = PsyRhovFnTdbRhLBnd0C( TempOutsideAirFD( SurfNum ), 1.0 );
						HConvExtFD( SurfNum ) = HcExtSurf( SurfNum );
						HMassConvExtFD( SurfNum ) = HConvExtFD( SurfNum ) / ( ( PsyRhoAirFnPbTdbW( OutBaroPress, TempOutsideAirFD( SurfNum ), PsyWFnTdbRhPb( TempOutsideAirFD( SurfNum ), 1.0, OutBaroPress, RoutineNameExtEnvWetSurf ) ) + RhoVaporAirOut( SurfNum ) ) * PsyCpAirFnWTdb( OutHumRat, TempOutsideAirFD( SurfNum ) ) );
						HSkyFD( SurfNum ) = HSkyExtSurf( SurfNum );
						HGrndFD( SurfNum ) = HGrdExtSurf( SurfNum );
						HAirFD( SurfNum ) = HAirExtSurf( SurfNum );
					}

				} else { // Surface is dry, use the normal correlation

					TempExt = Surface( SurfNum ).OutDryBulbTemp;

					if ( Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_CondFD || Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_HAMT ) {
						// Set variables used in the FD moisture balance and HAMT
						TempOutsideAirFD( SurfNum ) = TempExt;
						RhoVaporAirOut( SurfNum ) = PsyRhovFnTdbWPb( TempOutsideAirFD( SurfNum ), OutHumRat, OutBaroPress );
						HConvExtFD( SurfNum ) = HcExtSurf( SurfNum );
						HMassConvExtFD( SurfNum ) = HConvExtFD( SurfNum ) / ( ( PsyRhoAirFnPbTdbW( OutBaroPress, TempOutsideAirFD( SurfNum ), PsyWFnTdbRhPb( TempOutsideAirFD( SurfNum ), 1.0, OutBaroPress, RoutineNameExtEnvDrySurf ) ) + RhoVaporAirOut( SurfNum ) ) * PsyCpAirFnWTdb( OutHumRat, TempOutsideAirFD( SurfNum ) ) );
						//  check for saturation conditions of air
						RhoVaporSat = PsyRhovFnTdbRh( TempOutsideAirFD( SurfNum ), 1.0, HBSurfManDrySurfCondFD );
						if ( RhoVaporAirOut( SurfNum ) > RhoVaporSat ) RhoVaporAirOut( SurfNum ) = RhoVaporSat;
						HSkyFD( SurfNum ) = HSkyExtSurf( SurfNum );
						HGrndFD( SurfNum ) = HGrdExtSurf( SurfNum );
						HAirFD( SurfNum ) = HAirExtSurf( SurfNum );
					}

				}

			} else { // No wind

				// Calculate exterior heat transfer coefficients for windspeed = 0
				InitExteriorConvectionCoeff( SurfNum, HMovInsul, RoughSurf, AbsThermSurf, TH( 1, 1, SurfNum ), HcExtSurf( SurfNum ), HSkyExtSurf( SurfNum ), HGrdExtSurf( SurfNum ), HAirExtSurf( SurfNum ) );

				TempExt = Surface( SurfNum ).OutDryBulbTemp;

				if ( Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_CondFD || Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_HAMT ) {
					// Set variables used in the FD moisture balance and HAMT
					TempOutsideAirFD( SurfNum ) = TempExt;
					RhoVaporAirOut( SurfNum ) = PsyRhovFnTdbWPb( TempOutsideAirFD( SurfNum ), OutHumRat, OutBaroPress );
					HConvExtFD( SurfNum ) = HcExtSurf( SurfNum );
					HMassConvExtFD( SurfNum ) = HConvExtFD( SurfNum ) / ( ( PsyRhoAirFnPbTdbW( OutBaroPress, TempOutsideAirFD( SurfNum ), PsyWFnTdbRhPb( TempOutsideAirFD( SurfNum ), 1.0, OutBaroPress, RoutineNameNoWind ) ) + RhoVaporAirOut( SurfNum ) ) * PsyCpAirFnWTdb( OutHumRat, TempOutsideAirFD( SurfNum ) ) );
					HSkyFD( SurfNum ) = HSkyExtSurf( SurfNum );
					HGrndFD( SurfNum ) = HGrdExtSurf( SurfNum );
					HAirFD( SurfNum ) = HAirExtSurf( SurfNum );
				}

			}

			if ( Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_CTF || Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_EMPD ) CalcOutsideSurfTemp( SurfNum, ZoneNum, ConstrNum, HMovInsul, TempExt );

		} else { // for interior or other zone surfaces

			if ( Surface( SurfNum ).ExtBoundCond == SurfNum ) { // Regular partition/internal mass

				TH( 1, 1, SurfNum ) = TempSurfIn( SurfNum );

				// No need to set any radiant system heat balance coefficients here--will be done during inside heat balance

				if ( Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_CondFD || Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_HAMT ) {
					// Set variables used in the FD moisture balance HAMT
					TempOutsideAirFD( SurfNum ) = TempSurfIn( SurfNum );
					RhoVaporAirOut( SurfNum ) = RhoVaporAirIn( SurfNum );
					HConvExtFD( SurfNum ) = HConvIn( SurfNum );
					HMassConvExtFD( SurfNum ) = HConvExtFD( SurfNum ) / ( ( PsyRhoAirFnPbTdbW( OutBaroPress, TempOutsideAirFD( SurfNum ), PsyWFnTdbRhPb( TempOutsideAirFD( SurfNum ), 1.0, OutBaroPress, RoutineNameOther ) ) + RhoVaporAirOut( SurfNum ) ) * PsyCpAirFnWTdb( OutHumRat, TempOutsideAirFD( SurfNum ) ) );
					HSkyFD( SurfNum ) = 0.0;
					HGrndFD( SurfNum ) = 0.0;
					HAirFD( SurfNum ) = 0.0;
				}

			} else { // Interzone partition

				TH( 1, 1, SurfNum ) = TH( 2, 1, Surface( SurfNum ).ExtBoundCond );

				// No need to set any radiant system heat balance coefficients here--will be done during inside heat balance

				if ( Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_CondFD || Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_HAMT ) {
					// Set variables used in the FD moisture balance and HAMT
					TempOutsideAirFD( SurfNum ) = TH( 2, 1, Surface( SurfNum ).ExtBoundCond );
					RhoVaporAirOut( SurfNum ) = RhoVaporAirIn( Surface( SurfNum ).ExtBoundCond );
					HConvExtFD( SurfNum ) = HConvIn( Surface( SurfNum ).ExtBoundCond );
					HMassConvExtFD( SurfNum ) = HConvExtFD( SurfNum ) / ( ( PsyRhoAirFnPbTdbW( OutBaroPress, TempOutsideAirFD( SurfNum ), PsyWFnTdbRhPb( TempOutsideAirFD( SurfNum ), 1.0, OutBaroPress, RoutineNameIZPart ) ) + RhoVaporAirOut( SurfNum ) ) * PsyCpAirFnWTdb( OutHumRat, TempOutsideAirFD( SurfNum ) ) );
					HSkyFD( SurfNum ) = 0.0;
					HGrndFD( SurfNum ) = 0.0;
					HAirFD( SurfNum ) = 0.0;
				}

			}

			// This ends the calculations for this surface and goes on to the next SurfNum
		}}

		//fill in reporting values for outside face
		QdotConvOutRep( SurfNum ) = -Surface( SurfNum ).Area * HcExtSurf( SurfNum ) * ( TH( 1, 1, SurfNum ) - Surface( SurfNum ).OutDryBulbTemp );

		if ( Surface( SurfNum ).OSCMPtr > 0 ) { //Optr is set above in this case, use OSCM boundary data
			QdotConvOutRepPerArea( SurfNum ) = -OSCM( OPtr ).HConv * ( TH( 1, 1, SurfNum ) - OSCM( OPtr ).TConv );
		} else {
			QdotConvOutRepPerArea( SurfNum ) = -HcExtSurf( SurfNum ) * ( TH( 1, 1, SurfNum ) - Surface( SurfNum ).OutDryBulbTemp );
		}

		QConvOutReport( SurfNum ) = QdotConvOutRep( SurfNum ) * TimeStepZoneSec;

	} // ...end of DO loop over all surface (actually heat transfer surfaces)

}



void
CalcHeatBalanceInsideSurf( Optional_int_const ZoneToResimulate ) // if passed in, then only calculate surfaces that have this zone
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         George Walton
	//       DATE WRITTEN   December 1979
	//       MODIFIED       Jun 1990 (RDT for new CTF arrays)
	//                      Dec 1999 (FCW for window calculation)
	//                      May 2000 (FCW for window frame and dividers)
	//                      Aug 2000 (RJL for MTF moisture calculations)
	//                      Sep 2000 (RKS for new radiant exchange algorithm)
	//                      Dec 2000 (RKS for radiant system model addition)
	//                      Jul 2003 (CC) set the reference temperatures for inside surface heat balance
	//                                    depending on convection algorithms and/or air models used
	//                      May 2006 (RR  account for exterior window screen)
	//                      Jul 2008 (P. Biddulph include calls to HAMT)
	//                      Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems
	//       RE-ENGINEERED  Mar 1998 (RKS)

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine performs a heat balance on the outside face of each
	// surface in the building.

	// METHODOLOGY EMPLOYED:
	// Various boundary conditions are set and additional parameters are set-
	// up.  Then, the proper heat balance equation is selected based on whether
	// the surface is a partition or not and on whether or not movable
	// insulation is present on the inside face.

	// REFERENCES:
	// (I)BLAST legacy routine HBSRF

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGlobals;
	using namespace DataEnvironment;
	using namespace DataHeatBalFanSys;
	using namespace DataHeatBalance;
	using namespace DataHeatBalSurface;
	using namespace DataSurfaces;
	using namespace DataDaylightingDevices;
	using DataMoistureBalance::TempOutsideAirFD;
	using DataMoistureBalance::RhoVaporAirOut;
	using DataMoistureBalance::RhoVaporAirIn;
	using DataMoistureBalance::HConvExtFD;
	using DataMoistureBalance::HMassConvExtFD;
	using DataMoistureBalance::HConvInFD;
	using DataMoistureBalance::HMassConvInFD;
	using DataMoistureBalance::RhoVaporSurfIn;
	using DataMoistureBalance::HSkyFD;
	using DataMoistureBalance::HGrndFD;
	using DataMoistureBalance::HAirFD;
	using DataMoistureBalanceEMPD::MoistEMPDNew;
	using DataMoistureBalanceEMPD::MoistEMPDFlux;
	using DataAirflowNetwork::SimulateAirflowNetwork;
	using DataAirflowNetwork::AirflowNetworkControlSimple;

	using HeatBalanceMovableInsulation::EvalInsideMovableInsulation;
	using WindowManager::CalcWindowHeatBalance;
	using HeatBalFiniteDiffManager::ManageHeatBalFiniteDiff;
	using HeatBalFiniteDiffManager::SurfaceFD;
	using HeatBalanceHAMTManager::ManageHeatBalHAMT;
	using HeatBalanceHAMTManager::UpdateHeatBalHAMT;
	using ConvectionCoefficients::InitExteriorConvectionCoeff;
	using ConvectionCoefficients::InitInteriorConvectionCoeffs;
	using ConvectionCoefficients::SetExtConvectionCoeff;
	using ConvectionCoefficients::SetIntConvectionCoeff;
	using HeatBalanceIntRadExchange::CalcInteriorRadExchange;
	using MoistureBalanceEMPDManager::CalcMoistureBalanceEMPD;
	using MoistureBalanceEMPDManager::UpdateMoistureBalanceEMPD;
	using ScheduleManager::GetCurrentScheduleValue;
	using General::RoundSigDigits;
	using DaylightingDevices::FindTDDPipe;
	using DataZoneEquipment::ZoneEquipConfig;
	using DataLoopNode::Node;
	using HeatBalanceSurfaceManager::CalculateZoneMRT;
	using namespace Psychrometrics;
	using OutputReportTabular::loadConvectedNormal;
	using OutputReportTabular::loadConvectedWithPulse;
	using OutputReportTabular::netSurfRadSeq;
	using DataSizing::CurOverallSimDay;
	using namespace DataTimings;
	using WindowEquivalentLayer::EQLWindowOutsideEffectiveEmiss;
	using SwimmingPool::SimSwimmingPool;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	Real64 const Sigma( 5.6697e-08 ); // Stefan-Boltzmann constant
	Real64 const IterDampConst( 5.0 ); // Damping constant for inside surface temperature iterations
	int const ItersReevalConvCoeff( 30 ); // Number of iterations between inside convection coefficient reevaluations
	Real64 const MaxAllowedDelTemp( 0.002 ); // Convergence criteria for inside surface temperatures
	int const MaxIterations( 500 ); // Maximum number of iterations allowed for inside surface temps
	int const IterationsForCondFDRelaxChange( 5 ); // number of iterations for inside temps that triggers a change
	Real64 const SmallNumber( 0.0001 ); // avoid numerical junk causing problems?
	// in the CondFD relaxation factor.
	int const MinEMPDIterations( 4 ); // Minimum number of iterations required for EMPD solution
	static std::string const rhoAirZone( "RhoAirZone" );
	static std::string const wsurf( "Wsurf" );
	static std::string const HBSurfManInsideSurf( "HB,SurfMan:InsideSurf" );
	static std::string const Inside( "Inside" );
	static std::string const BlankString;

	// INTERFACE BLOCK SPECIFICATIONS:
	// na

	// DERIVED TYPE DEFINITIONS:
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	Real64 AbsInt; // Solar absorptance of inside movable insulation
	int ConstrNum; // Construction index for the current surface
	bool Converged; // .TRUE. if inside heat balance has converged
	Real64 F1; // Intermediate calculation value
	Real64 HMovInsul; // "Convection" coefficient of movable insulation
	Real64 MaxDelTemp; // Maximum change in surface temperature for any
	//  opaque surface from one iteration to the next
	int SurfNum; // Surface number
	int ZoneNum; // Zone number the current surface is attached to
	int ConstrNumSh; // Shaded construction number for a window
	int RoughSurf; // Outside surface roughness
	Real64 EmisOut; // Glass outside surface emissivity

	static Array1D< Real64 > TempInsOld; // Holds previous iteration's value for convergence check
	Real64 TempSurfOutTmp; // Local Temporary Surface temperature for the outside surface face
	Real64 TempSurfInSat; // Local temperary surface dew point temperature

	int OtherSideSurfNum; // Surface number index for other side of an interzone partition
	static int MinIterations; // Minimum number of iterations for the inside heat balance
	//  CHARACTER(len=25):: ErrMsg
	//  CHARACTER(len=5) :: TimeStmp
	static int ErrCount( 0 );
	int PipeNum; // TDD pipe object number
	int SurfNum2; // TDD:DIFFUSER object number
	Real64 Ueff; // 1 / effective R value between TDD:DOME and TDD:DIFFUSER

	int ZoneEquipConfigNum;
	//  LOGICAL           :: ControlledZoneAirFlag
	int NodeNum;
	Real64 SumSysMCp; // Zone sum of air system MassFlowRate*Cp
	Real64 SumSysMCpT; // Zone sum of air system MassFlowRate*Cp*T
	Real64 MassFlowRate;
	Real64 NodeTemp;
	Real64 CpAir;
	static Array1D< Real64 > RefAirTemp; // reference air temperatures
	static bool MyEnvrnFlag( true );
	//  LOGICAL, SAVE     :: DoThisLoop
	static int InsideSurfErrCount( 0 );
	Real64 Wsurf; // Moisture ratio for HAMT
	Real64 RhoAirZone; // Zone moisture density for HAMT
	int OtherSideZoneNum; // Zone Number index for other side of an interzone partition HAMT
	static int WarmupSurfTemp;
	static int TimeStepInDay( 0 ); // time step number

	// FLOW:
	if ( calcHeatBalanceInsideSurfFirstTime ) {
		TempInsOld.allocate( TotSurfaces );
		RefAirTemp.allocate( TotSurfaces );
		if ( any_eq( HeatTransferAlgosUsed, UseEMPD ) ) {
			MinIterations = MinEMPDIterations;
		} else {
			MinIterations = 1;
		}
		if ( DisplayAdvancedReportVariables ) {
			SetupOutputVariable( "Surface Inside Face Heat Balance Calculation Iteration Count []", InsideSurfIterations, "ZONE", "Sum", "Simulation" );
		}
	}
	if ( BeginEnvrnFlag && MyEnvrnFlag ) {
		TempInsOld = 23.0;
		RefAirTemp = 23.0;
		TempEffBulkAir = 23.0;
		WarmupSurfTemp = 0;
		MyEnvrnFlag = false;
	}
	if ( ! BeginEnvrnFlag ) {
		MyEnvrnFlag = true;
	}

	bool const PartialResimulate( present( ZoneToResimulate ) );

	//Tuned Relevant surfaces (set below) for performance/scalability //Do Store this once for all relevant Zones at higher level
	std::vector< int > SurfToResimulate;
	std::vector< int > HTSurfToResimulate; // Heat transfer surfaces
	if ( ! PartialResimulate ) { // Avoid resizing
		SurfToResimulate.reserve( TotSurfaces );
		HTSurfToResimulate.reserve( TotSurfaces );
	}

	// determine reference air temperatures
	for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
		ZoneNum = Surface( SurfNum ).Zone;

		if ( PartialResimulate ) {
			if ( ( ZoneNum != ZoneToResimulate ) && ( AdjacentZoneToSurface( SurfNum ) != ZoneToResimulate ) ) { // Surface not relevant
				continue; // skip surfaces that are not associated with this zone
			} else { // Surface is relevant for ZoneToResimulate
				SurfToResimulate.push_back( SurfNum );
				if ( Surface( SurfNum ).HeatTransSurf ) HTSurfToResimulate.push_back( SurfNum ); // Skip non-heat transfer surfaces
			}
		} else {
			SurfToResimulate.push_back( SurfNum );
			if ( Surface( SurfNum ).HeatTransSurf ) HTSurfToResimulate.push_back( SurfNum ); // Skip non-heat transfer surfaces
		}

		// These conditions are not used in every SurfNum loop here so we don't use them to skip surfaces
		if ( ( ZoneNum == 0 ) || ! Surface( SurfNum ).HeatTransSurf ) continue; // Skip non-heat transfer surfaces
		if ( Surface( SurfNum ).Class == SurfaceClass_TDD_Dome ) continue; // Skip TDD:DOME objects.  Inside temp is handled by TDD:DIFFUSER.

		if ( PartialResimulate ) {
			WinHeatGain( SurfNum ) = 0.0;
			WinHeatGainRep( SurfNum ) = 0.0;
			WinHeatLossRep( SurfNum ) = 0.0;
			WinGainConvGlazToZoneRep( SurfNum ) = 0.0;
			WinGainIRGlazToZoneRep( SurfNum ) = 0.0;
			WinLossSWZoneToOutWinRep( SurfNum ) = 0.0;
			WinGainFrameDividerToZoneRep( SurfNum ) = 0.0;
			WinGainConvGlazShadGapToZoneRep( SurfNum ) = 0.0;
			WinGainConvShadeToZoneRep( SurfNum ) = 0.0;
			OtherConvGainInsideFaceToZoneRep( SurfNum ) = 0.0;
			WinGainIRShadeToZoneRep( SurfNum ) = 0.0;
			SurfaceWindow( SurfNum ).FrameQRadOutAbs = 0.0;
			SurfaceWindow( SurfNum ).FrameQRadInAbs = 0.0;
			SurfaceWindow( SurfNum ).DividerQRadOutAbs = 0.0;
			SurfaceWindow( SurfNum ).DividerQRadInAbs = 0.0;
		}

		{ auto const SELECT_CASE_var( Surface( SurfNum ).TAirRef );
		if ( SELECT_CASE_var == ZoneMeanAirTemp ) {
			RefAirTemp( SurfNum ) = MAT( ZoneNum );
			TempEffBulkAir( SurfNum ) = MAT( ZoneNum ); // for reporting surf adjacent air temp
		} else if ( SELECT_CASE_var == AdjacentAirTemp ) {
			RefAirTemp( SurfNum ) = TempEffBulkAir( SurfNum );
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
				CpAir = PsyCpAirFnWTdb( ZoneAirHumRat( ZoneNum ), NodeTemp );
				SumSysMCp += MassFlowRate * CpAir;
				SumSysMCpT += MassFlowRate * CpAir * NodeTemp;
			}
			// a weighted average of the inlet temperatures.
			if ( SumSysMCp > 0.0 ) { // protect div by zero
				RefAirTemp( SurfNum ) = SumSysMCpT / SumSysMCp; // BG changed 02-16-2005 to add index (SurfNum)
			} else {
				RefAirTemp( SurfNum ) = NodeTemp;
			}
			TempEffBulkAir( SurfNum ) = RefAirTemp( SurfNum ); // for reporting surf adjacent air temp
		} else {
			// currently set to mean air temp but should add error warning here
			RefAirTemp( SurfNum ) = MAT( ZoneNum );
			TempEffBulkAir( SurfNum ) = MAT( ZoneNum ); // for reporting surf adjacent air temp
		}}
	}
	auto const nSurfToResimulate( SurfToResimulate.size() );
	auto const nHTSurfToResimulate( HTSurfToResimulate.size() );

	InsideSurfIterations = 0;
	// Following variables must be reset due to possible recall of this routine by radiant and Resimulate routines.
	// CalcWindowHeatBalance is called, then, multiple times and these need to be initialized before each call to
	// CalcWindowHeatBalance.
	if ( ! PartialResimulate ) {
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
		for ( auto & window : SurfaceWindow ) {
			window.FrameQRadOutAbs = 0.0;
			window.FrameQRadInAbs = 0.0;
			window.DividerQRadOutAbs = 0.0;
			window.DividerQRadInAbs = 0.0;
		}
	}

	//Tuned Precompute whether CTF temperature limits will be needed //? Can we do this just once in the FirstTime block to save a little more time (with static array)
	Array1D_bool any_surface_ConFD_or_HAMT( NumOfZones, false );
	for ( int iZone = 1; iZone <= NumOfZones; ++iZone ) {
		auto const & zone( Zone( iZone ) );
		for ( int iSurf = zone.SurfaceFirst, eSurf = zone.SurfaceLast; iSurf <= eSurf; ++iSurf ) { //Tuned Replaced any_eq and array slicing and member array usage
			auto const alg( Surface( iSurf ).HeatTransferAlgorithm );
			if ( ( alg == HeatTransferModel_CondFD ) || ( alg == HeatTransferModel_HAMT ) ) {
				any_surface_ConFD_or_HAMT( iZone ) = true;
				break;
			}
		}
	}

	bool const useCondFDHTalg( any_eq( HeatTransferAlgosUsed, UseCondFD ) );
	Converged = false;
	while ( ! Converged ) { // Start of main inside heat balance DO loop...

		TempInsOld = TempSurfIn; // Keep track of last iteration's temperature values

		CalcInteriorRadExchange( TempSurfIn, InsideSurfIterations, NetLWRadToSurf, ZoneToResimulate, Inside ); // Update the radiation balance

		// Every 30 iterations, recalculate the inside convection coefficients in case
		// there has been a significant drift in the surface temperatures predicted.
		// This is not fool-proof and it basically means that the outside surface
		// heat balance is in error (potentially) once HConvIn is re-evaluated.
		// The choice of 30 is not significant--just want to do this a couple of
		// times before the iteration limit is hit.
		if ( ( InsideSurfIterations > 0 ) && ( mod( InsideSurfIterations, ItersReevalConvCoeff ) == 0 ) ) {
			InitInteriorConvectionCoeffs( TempSurfIn, ZoneToResimulate );
		}

		for ( std::vector< int >::size_type iHTSurfToResimulate = 0u; iHTSurfToResimulate < nHTSurfToResimulate; ++iHTSurfToResimulate ) { // Perform a heat balance on all of the relevant inside surfaces...
			SurfNum = HTSurfToResimulate[ iHTSurfToResimulate ]; // Heat transfer surfaces only
			auto & surface( Surface( SurfNum ) );
			if ( surface.Class == SurfaceClass_TDD_Dome ) continue; // Skip TDD:DOME objects.  Inside temp is handled by TDD:DIFFUSER.
			if ( ( ZoneNum = surface.Zone ) == 0 ) continue; // Skip non-heat transfer surfaces

			Real64 & TH11( TH( 1, 1, SurfNum ) );
			Real64 & TH12( TH( 2, 1, SurfNum ) );
			Real64 & TH22( TH( 2, 2, SurfNum ) );

			ConstrNum = surface.Construction;
			auto const & construct( Construct( ConstrNum ) );
			Real64 const MAT_zone( MAT( ZoneNum ) );
			Real64 const ZoneAirHumRat_zone( max( ZoneAirHumRat( ZoneNum ), 1.0e-5 ) );

			// Calculate the inside surface moisture quantities
			// calculate the inside surface moisture transfer conditions
			// check for saturation conditions of air
			Real64 const HConvIn_surf( HConvInFD( SurfNum ) = HConvIn( SurfNum ) );
			RhoVaporAirIn( SurfNum ) = min( PsyRhovFnTdbWPb_fast( MAT_zone, ZoneAirHumRat_zone, OutBaroPress ), PsyRhovFnTdbRh( MAT_zone, 1.0, HBSurfManInsideSurf ) );
			HMassConvInFD( SurfNum ) = HConvIn_surf / ( ( PsyRhoAirFnPbTdbW_fast( OutBaroPress, MAT_zone, ZoneAirHumRat_zone ) + RhoVaporAirIn( SurfNum ) ) * PsyCpAirFnWTdb_fast( ZoneAirHumRat_zone, MAT_zone ) );

			// Perform heat balance on the inside face of the surface ...
			// The following are possibilities here:
			//   (a) the surface is a pool (no movable insulation, no source/sink, only CTF solution algorithm)
			//   (b) the surface is a partition, in which case the temperature of both sides are the same
			//   (c) standard (or interzone) opaque surface with no movable insulation, normal heat balance equation
			//   (d) standard (or interzone) window: call to CalcWindowHeatBalance to get window layer temperatures
			//   (e) standard opaque surface with movable insulation, special two-part equation
			// In the surface calculation there are the following Algorithm types for opaque surfaces that
			// do not have movable insulation:
			//   (a) the regular CTF calc (SolutionAlgo = UseCTF)
			//   (b) the EMPD calc (Solutionalgo = UseEMPD)
			//   (c) the CondFD calc (SolutionAlgo = UseCondFD)
			//   (d) the HAMT calc (solutionalgo = UseHAMT).

			auto & zone( Zone( ZoneNum ) );
			if ( surface.ExtBoundCond == SurfNum && surface.Class != SurfaceClass_Window ) {
				//CR6869 -- let Window HB take care of it      IF (Surface(SurfNum)%ExtBoundCond == SurfNum) THEN
				// Surface is a partition
				if ( surface.HeatTransferAlgorithm == HeatTransferModel_CTF || surface.HeatTransferAlgorithm == HeatTransferModel_EMPD ) { // Regular CTF Surface and/or EMPD surface

					if ( surface.HeatTransferAlgorithm == HeatTransferModel_EMPD ) {
						CalcMoistureBalanceEMPD( SurfNum, TempSurfInTmp( SurfNum ), TH22, MAT_zone, TempSurfInSat );
					}
					//Pre-calculate a few terms
					Real64 const TempTerm( CTFConstInPart( SurfNum ) + QRadThermInAbs( SurfNum ) + QRadSWInAbs( SurfNum ) + HConvIn_surf * RefAirTemp( SurfNum ) + QHTRadSysSurf( SurfNum ) + QHWBaseboardSurf( SurfNum ) + QSteamBaseboardSurf( SurfNum ) + QElecBaseboardSurf( SurfNum ) + NetLWRadToSurf( SurfNum ) );
					Real64 const TempDiv( 1.0 / ( construct.CTFInside( 0 ) - construct.CTFCross( 0 ) + HConvIn_surf + IterDampConst ) );
					// Calculate the current inside surface temperature
					if ( ( ! surface.IsPool ) || ( ( surface.IsPool ) && ( abs( QPoolSurfNumerator( SurfNum ) ) < SmallNumber ) && ( abs( PoolHeatTransCoefs( SurfNum ) ) < SmallNumber ) ) ) {
						TempSurfInTmp( SurfNum ) = ( TempTerm + construct.CTFSourceIn( 0 ) * QsrcHist( SurfNum, 1 ) + IterDampConst * TempInsOld( SurfNum ) ) * TempDiv; // Constant portion of conduction eq (history terms) | LW radiation from internal sources | SW radiation from internal sources | Convection from surface to zone air | Net radiant exchange with other zone surfaces | Heat source/sink term for radiant systems | (if there is one present) | Radiant flux from a high temperature radiant heater | Radiant flux from a hot water baseboard heater | Radiant flux from a steam baseboard heater | Radiant flux from an electric baseboard heater | Iterative damping term (for stability) | Conduction term (both partition sides same temp) | Conduction term (both partition sides same temp) | Convection and damping term
					} else { // this is a pool and it has been simulated this time step
						TempSurfInTmp( SurfNum ) = ( CTFConstInPart( SurfNum ) + QPoolSurfNumerator( SurfNum ) + IterDampConst * TempInsOld( SurfNum ) ) / ( construct.CTFInside( 0 ) - construct.CTFCross( 0 ) + PoolHeatTransCoefs( SurfNum ) + IterDampConst ); // Constant part of conduction eq (history terms) | Pool modified terms (see non-pool equation for details) | Iterative damping term (for stability) | Conduction term (both partition sides same temp) | Pool and damping term
					}
					if ( surface.HeatTransferAlgorithm == HeatTransferModel_EMPD ) {
						TempSurfInTmp( SurfNum ) -= MoistEMPDFlux( SurfNum ) * TempDiv; // Conduction term (both partition sides same temp) | Conduction term (both partition sides same temp) | Convection and damping term
						if ( TempSurfInSat > TempSurfInTmp( SurfNum ) ) {
							TempSurfInTmp( SurfNum ) = TempSurfInSat; // Surface temp cannot be below dew point
						}
					}
					// if any mixed heat transfer models in zone, apply limits to CTF result
					if ( any_surface_ConFD_or_HAMT( ZoneNum ) ) TempSurfInTmp( SurfNum ) = max( MinSurfaceTempLimit, min( MaxSurfaceTempLimit, TempSurfInTmp( SurfNum ) ) ); // Limit Check //Tuned Precomputed condition to eliminate loop

					if ( construct.SourceSinkPresent ) { // Set the appropriate parameters for the radiant system

						// Radiant system does not need the damping coefficient terms (hopefully) // Partitions are assumed to be symmetric
						Real64 const RadSysDiv( 1.0 / ( construct.CTFInside( 0 ) - construct.CTFCross( 0 ) + HConvIn_surf ) );
						RadSysToHBConstCoef( SurfNum ) = RadSysTiHBConstCoef( SurfNum ) = TempTerm * RadSysDiv; // Constant portion of conduction eq (history terms) | LW radiation from internal sources | SW radiation from internal sources | Convection from surface to zone air | Radiant flux from high temperature radiant heater | Radiant flux from a hot water baseboard heater | Radiant flux from a steam baseboard heater | Radiant flux from an electric baseboard heater | Net radiant exchange with other zone surfaces | Cond term (both partition sides same temp) | Cond term (both partition sides same temp) | Convection and damping term
						RadSysToHBTinCoef( SurfNum ) = RadSysTiHBToutCoef( SurfNum ) = 0.0; // The outside temp is assumed to be equal to the inside temp for a partition
						RadSysToHBQsrcCoef( SurfNum ) = RadSysTiHBQsrcCoef( SurfNum ) = construct.CTFSourceIn( 0 ) * RadSysDiv; // QTF term for the source | Cond term (both partition sides same temp) | Cond term (both partition sides same temp) | Convection and damping term

					}

				} else if ( surface.HeatTransferAlgorithm == HeatTransferModel_CondFD || surface.HeatTransferAlgorithm == HeatTransferModel_HAMT ) {

					if ( surface.HeatTransferAlgorithm == HeatTransferModel_HAMT ) ManageHeatBalHAMT( SurfNum, TempSurfInTmp( SurfNum ), TempSurfOutTmp ); //HAMT

					if ( surface.HeatTransferAlgorithm == HeatTransferModel_CondFD ) ManageHeatBalFiniteDiff( SurfNum, TempSurfInTmp( SurfNum ), TempSurfOutTmp );

					TH11 = TempSurfOutTmp;

				}

				TempSurfIn( SurfNum ) = TempSurfInTmp( SurfNum );

			} else { // Standard surface or interzone surface

				if ( surface.Class != SurfaceClass_Window ) { // Opaque surface

					HMovInsul = 0.0;
					if ( surface.MaterialMovInsulInt > 0 ) EvalInsideMovableInsulation( SurfNum, HMovInsul, AbsInt );

					if ( HMovInsul <= 0.0 ) { // No movable insulation present, normal heat balance equation

						if ( surface.HeatTransferAlgorithm == HeatTransferModel_CTF || surface.HeatTransferAlgorithm == HeatTransferModel_EMPD ) { // Regular CTF Surface and/or EMPD surface

							if ( surface.HeatTransferAlgorithm == HeatTransferModel_EMPD ) {
								CalcMoistureBalanceEMPD( SurfNum, TempSurfInTmp( SurfNum ), TH22, MAT_zone, TempSurfInSat );
							}
							//Pre-calculate a few terms
							Real64 const TempTerm( CTFConstInPart( SurfNum ) + QRadThermInAbs( SurfNum ) + QRadSWInAbs( SurfNum ) + HConvIn_surf * RefAirTemp( SurfNum ) + QHTRadSysSurf( SurfNum ) + QHWBaseboardSurf( SurfNum ) + QSteamBaseboardSurf( SurfNum ) + QElecBaseboardSurf( SurfNum ) + NetLWRadToSurf( SurfNum ) );
							Real64 const TempDiv( 1.0 / ( construct.CTFInside( 0 ) + HConvIn_surf + IterDampConst ) );
							// Calculate the current inside surface temperature
							if ( ( ! surface.IsPool ) || ( ( surface.IsPool ) && ( abs( QPoolSurfNumerator( SurfNum ) ) < SmallNumber ) && ( abs( PoolHeatTransCoefs( SurfNum ) ) < SmallNumber ) ) ) {
								TempSurfInTmp( SurfNum ) = ( TempTerm + construct.CTFSourceIn( 0 ) * QsrcHist( SurfNum, 1 ) + IterDampConst * TempInsOld( SurfNum ) + construct.CTFCross( 0 ) * TH11 ) * TempDiv; // Constant part of conduction eq (history terms) | LW radiation from internal sources | SW radiation from internal sources | Convection from surface to zone air | Net radiant exchange with other zone surfaces | Heat source/sink term for radiant systems | (if there is one present) | Radiant flux from high temp radiant heater | Radiant flux from a hot water baseboard heater | Radiant flux from a steam baseboard heater | Radiant flux from an electric baseboard heater | Iterative damping term (for stability) | Current conduction from | the outside surface | Coefficient for conduction (current time) | Convection and damping term
							} else { // surface is a pool and the pool has been simulated this time step
								TempSurfInTmp( SurfNum ) = ( CTFConstInPart( SurfNum ) + QPoolSurfNumerator( SurfNum ) + IterDampConst * TempInsOld( SurfNum ) + construct.CTFCross( 0 ) * TH11 ) / ( construct.CTFInside( 0 ) + PoolHeatTransCoefs( SurfNum ) + IterDampConst ); // Constant part of conduction eq (history terms) | Pool modified terms (see non-pool equation for details) | Iterative damping term (for stability) | Current conduction from | the outside surface | Coefficient for conduction (current time) | Pool and damping term
							}
							if ( surface.HeatTransferAlgorithm == HeatTransferModel_EMPD ) {
								TempSurfInTmp( SurfNum ) -= MoistEMPDFlux( SurfNum ) * TempDiv; // Coefficient for conduction (current time) | Convection and damping term
								if ( TempSurfInSat > TempSurfInTmp( SurfNum ) ) {
									TempSurfInTmp( SurfNum ) = TempSurfInSat; // Surface temp cannot be below dew point
								}
							}
							// if any mixed heat transfer models in zone, apply limits to CTF result
							if ( any_surface_ConFD_or_HAMT( ZoneNum ) ) TempSurfInTmp( SurfNum ) = max( MinSurfaceTempLimit, min( MaxSurfaceTempLimit, TempSurfInTmp( SurfNum ) ) ); // Limit Check //Tuned Precomputed condition to eliminate loop

							if ( construct.SourceSinkPresent ) { // Set the appropriate parameters for the radiant system

								// Radiant system does not need the damping coefficient terms (hopefully)
								Real64 const RadSysDiv( 1.0 / ( construct.CTFInside( 0 ) + HConvIn_surf ) );
								RadSysTiHBConstCoef( SurfNum ) = TempTerm * RadSysDiv; // Constant portion of cond eq (history terms) | LW radiation from internal sources | SW radiation from internal sources | Convection from surface to zone air | Radiant flux from high temp radiant heater | Radiant flux from a hot water baseboard heater | Radiant flux from a steam baseboard heater | Radiant flux from an electric baseboard heater | Net radiant exchange with other zone surfaces | Cond term (both partition sides same temp) | Convection and damping term
								RadSysTiHBToutCoef( SurfNum ) = construct.CTFCross( 0 ) * RadSysDiv; // Outside temp=inside temp for a partition | Cond term (both partition sides same temp) | Convection and damping term
								RadSysTiHBQsrcCoef( SurfNum ) = construct.CTFSourceIn( 0 ) * RadSysDiv; // QTF term for the source | Cond term (both partition sides same temp) | Convection and damping term

								if ( surface.ExtBoundCond > 0 ) { // This is an interzone partition and we need to set outside params
									// The inside coefficients of one side are equal to the outside coefficients of the other side.  But,
									// the inside coefficients are set up once the heat balance equation for that side has been calculated.
									// For both sides to actually have been set, we have to wait until we get to the second side in the surface
									// derived type.  At that point, both inside coefficient sets have been evaluated.
									if ( surface.ExtBoundCond < SurfNum ) { // Both of the inside coefficients have now been set
										OtherSideSurfNum = surface.ExtBoundCond;
										RadSysToHBConstCoef( OtherSideSurfNum ) = RadSysTiHBConstCoef( SurfNum );
										RadSysToHBTinCoef( OtherSideSurfNum ) = RadSysTiHBToutCoef( SurfNum );
										RadSysToHBQsrcCoef( OtherSideSurfNum ) = RadSysTiHBQsrcCoef( SurfNum );
										RadSysToHBConstCoef( SurfNum ) = RadSysTiHBConstCoef( OtherSideSurfNum );
										RadSysToHBTinCoef( SurfNum ) = RadSysTiHBToutCoef( OtherSideSurfNum );
										RadSysToHBQsrcCoef( SurfNum ) = RadSysTiHBQsrcCoef( OtherSideSurfNum );
									}
								}

							}

						} else if ( surface.HeatTransferAlgorithm == HeatTransferModel_CondFD || surface.HeatTransferAlgorithm == HeatTransferModel_HAMT ) {

							if ( surface.HeatTransferAlgorithm == HeatTransferModel_HAMT ) {
								if ( surface.ExtBoundCond > 0 ) {
									// HAMT get the correct other side zone zone air temperature --
									OtherSideSurfNum = surface.ExtBoundCond;
									ZoneNum = surface.Zone;
									OtherSideZoneNum = Surface( OtherSideSurfNum ).Zone;
									TempOutsideAirFD( SurfNum ) = MAT( OtherSideZoneNum );
								}
								ManageHeatBalHAMT( SurfNum, TempSurfInTmp( SurfNum ), TempSurfOutTmp );
							}

							if ( surface.HeatTransferAlgorithm == HeatTransferModel_CondFD ) ManageHeatBalFiniteDiff( SurfNum, TempSurfInTmp( SurfNum ), TempSurfOutTmp );

							TH11 = TempSurfOutTmp;

						}

						TempSurfIn( SurfNum ) = TempSurfInTmp( SurfNum );

					} else { // Movable insulation present

						if ( construct.SourceSinkPresent && calcHeatBalanceInsideSurfFirstTime ) ShowSevereError( "Movable insulation is not valid with embedded sources/sinks" );

						F1 = HMovInsul / ( HMovInsul + HConvIn_surf + IterDampConst );

						TempSurfIn( SurfNum ) = ( CTFConstInPart( SurfNum ) + QRadSWInAbs( SurfNum ) + construct.CTFCross( 0 ) * TH11 + F1 * ( QRadThermInAbs( SurfNum ) + HConvIn_surf * RefAirTemp( SurfNum ) + NetLWRadToSurf( SurfNum ) + QHTRadSysSurf( SurfNum ) + QHWBaseboardSurf( SurfNum ) + QSteamBaseboardSurf( SurfNum ) + QElecBaseboardSurf( SurfNum ) + IterDampConst * TempInsOld( SurfNum ) ) ) / ( construct.CTFInside( 0 ) + HMovInsul - F1 * HMovInsul ); // Convection from surface to zone air

						TempSurfInTmp( SurfNum ) = ( construct.CTFInside( 0 ) * TempSurfIn( SurfNum ) + HMovInsul * TempSurfIn( SurfNum ) - QRadSWInAbs( SurfNum ) - CTFConstInPart( SurfNum ) - construct.CTFCross( 0 ) * TH11 ) / ( HMovInsul );
						// if any mixed heat transfer models in zone, apply limits to CTF result
						if ( any_surface_ConFD_or_HAMT( ZoneNum ) ) TempSurfInTmp( SurfNum ) = max( MinSurfaceTempLimit, min( MaxSurfaceTempLimit, TempSurfInTmp( SurfNum ) ) ); // Limit Check //Tuned Precomputed condition to eliminate loop
					}

				} else { // Window

					if ( construct.SourceSinkPresent && calcHeatBalanceInsideSurfFirstTime ) ShowSevereError( "Windows are not allowed to have embedded sources/sinks" );

					if ( SurfaceWindow( SurfNum ).OriginalClass == SurfaceClass_TDD_Diffuser ) { // Tubular daylighting device
						// Lookup up the TDD:DOME object
						PipeNum = FindTDDPipe( SurfNum );
						SurfNum2 = TDDPipe( PipeNum ).Dome;
						Ueff = 1.0 / TDDPipe( PipeNum ).Reff;

						// Similar to opaque surface but outside surface temp of TDD:DOME is used, and no embedded sources/sinks.
						// Absorbed shortwave radiation is treated similar to a regular window, but only 1 glass layer is allowed.
						//   = QRadSWwinAbs(SurfNum,1)/2.0
						TempSurfIn( SurfNum ) = TempSurfInTmp( SurfNum ) = ( QRadThermInAbs( SurfNum ) + QRadSWwinAbs( 1, SurfNum ) / 2.0 + HConvIn_surf * RefAirTemp( SurfNum ) + NetLWRadToSurf( SurfNum ) + IterDampConst * TempInsOld( SurfNum ) + Ueff * TH( 1, 1, SurfNum2 ) ) / ( Ueff + HConvIn_surf + IterDampConst ); // LW radiation from internal sources | SW radiation from internal sources and solar | Convection from surface to zone air | Net radiant exchange with other zone surfaces | Iterative damping term (for stability) | Current conduction from the outside surface | Coefficient for conduction (current time) | Convection and damping term

						Real64 const Sigma_Temp_4( Sigma * pow_4( TempSurfIn( SurfNum ) ) );

						// Calculate window heat gain for TDD:DIFFUSER since this calculation is usually done in WindowManager
						WinHeatGain( SurfNum ) = WinTransSolar( SurfNum ) + HConvIn_surf * surface.Area * ( TempSurfIn( SurfNum ) - RefAirTemp( SurfNum ) ) + Construct( surface.Construction ).InsideAbsorpThermal * surface.Area * ( Sigma_Temp_4 - ( SurfaceWindow( SurfNum ).IRfromParentZone + QHTRadSysSurf( SurfNum ) + QHWBaseboardSurf( SurfNum ) + QSteamBaseboardSurf( SurfNum ) + QElecBaseboardSurf( SurfNum ) ) ) - QS( surface.Zone ) * surface.Area * Construct( surface.Construction ).TransDiff; // Transmitted solar | Convection | IR exchange | IR
						// Zone diffuse interior shortwave reflected back into the TDD

						// fill out report vars for components of Window Heat Gain
						WinGainConvGlazToZoneRep( SurfNum ) = HConvIn_surf * surface.Area * ( TempSurfIn( SurfNum ) - RefAirTemp( SurfNum ) );
						WinGainIRGlazToZoneRep( SurfNum ) = Construct( surface.Construction ).InsideAbsorpThermal * surface.Area * ( Sigma_Temp_4 - ( SurfaceWindow( SurfNum ).IRfromParentZone + QHTRadSysSurf( SurfNum ) + QHWBaseboardSurf( SurfNum ) + QSteamBaseboardSurf( SurfNum ) + QElecBaseboardSurf( SurfNum ) ) );
						WinLossSWZoneToOutWinRep( SurfNum ) = QS( surface.Zone ) * surface.Area * Construct( surface.Construction ).TransDiff;
						if ( WinHeatGain( SurfNum ) >= 0.0 ) {
							WinHeatGainRep( SurfNum ) = WinHeatGain( SurfNum );
							WinHeatGainRepEnergy( SurfNum ) = WinHeatGainRep( SurfNum ) * TimeStepZoneSec;
						} else {
							WinHeatLossRep( SurfNum ) = -WinHeatGain( SurfNum );
							WinHeatLossRepEnergy( SurfNum ) = WinHeatLossRep( SurfNum ) * TimeStepZoneSec;
						}

						TDDPipe( PipeNum ).HeatGain = WinHeatGainRep( SurfNum );
						TDDPipe( PipeNum ).HeatLoss = WinHeatLossRep( SurfNum );

					} else { // Regular window
						if ( InsideSurfIterations == 0 ) { // Do windows only once
							if ( SurfaceWindow( SurfNum ).StormWinFlag == 1 ) ConstrNum = surface.StormWinConstruction;
							// Get outside convection coeff for exterior window here to avoid calling
							// InitExteriorConvectionCoeff from CalcWindowHeatBalance, which avoids circular reference
							// (HeatBalanceSurfaceManager USEing and WindowManager and
							// WindowManager USEing HeatBalanceSurfaceManager)
							if ( surface.ExtBoundCond == ExternalEnvironment ) {
								RoughSurf = Material( construct.LayerPoint( 1 ) ).Roughness;
								EmisOut = Material( construct.LayerPoint( 1 ) ).AbsorpThermalFront;
								auto const shading_flag( SurfaceWindow( SurfNum ).ShadingFlag );
								if ( shading_flag == ExtShadeOn || shading_flag == ExtBlindOn || shading_flag == ExtScreenOn ) {
									// Exterior shade in place
									ConstrNumSh = SurfaceWindow( SurfNum ).ShadedConstruction;
									RoughSurf = Material( Construct( ConstrNumSh ).LayerPoint( 1 ) ).Roughness;
									EmisOut = Material( Construct( ConstrNumSh ).LayerPoint( 1 ) ).AbsorpThermal;
								}

								// Get the outside effective emissivity for Equivalent layer model
								if ( construct.WindowTypeEQL ) {
									EmisOut = EQLWindowOutsideEffectiveEmiss( ConstrNum );
								}
								// Set Exterior Convection Coefficient...
								if ( surface.ExtConvCoeff > 0 ) {

									HcExtSurf( SurfNum ) = SetExtConvectionCoeff( SurfNum );

								} else if ( surface.ExtWind ) { // Window is exposed to wind (and possibly rain)

									// Calculate exterior heat transfer coefficients with windspeed (windspeed is calculated internally in subroutine)
									InitExteriorConvectionCoeff( SurfNum, 0.0, RoughSurf, EmisOut, TH11, HcExtSurf( SurfNum ), HSkyExtSurf( SurfNum ), HGrdExtSurf( SurfNum ), HAirExtSurf( SurfNum ) );

									if ( IsRain ) { // Raining: since wind exposed, outside window surface gets wet
										HcExtSurf( SurfNum ) = 1000.0; // Reset HcExtSurf because of wetness
									}

								} else { // Not Wind exposed

									// Calculate exterior heat transfer coefficients for windspeed = 0
									InitExteriorConvectionCoeff( SurfNum, 0.0, RoughSurf, EmisOut, TH11, HcExtSurf( SurfNum ), HSkyExtSurf( SurfNum ), HGrdExtSurf( SurfNum ), HAirExtSurf( SurfNum ) );

								}
							} else { // Interior Surface

								if ( surface.ExtConvCoeff > 0 ) {
									HcExtSurf( SurfNum ) = SetExtConvectionCoeff( SurfNum );
								} else {
									// Exterior Convection Coefficient for the Interior or Interzone Window is the Interior Convection Coeff of same
									HcExtSurf( SurfNum ) = HConvIn( surface.ExtBoundCond );
								}

							}

							// Following call determines inside surface temperature of glazing, and of
							// frame and/or divider, if present
							CalcWindowHeatBalance( SurfNum, HcExtSurf( SurfNum ), TempSurfInTmp( SurfNum ), TH11 );
							if ( WinHeatGain( SurfNum ) >= 0.0 ) {
								WinHeatGainRep( SurfNum ) = WinHeatGain( SurfNum );
								WinHeatGainRepEnergy( SurfNum ) = WinHeatGainRep( SurfNum ) * TimeStepZoneSec;
							} else {
								WinHeatLossRep( SurfNum ) = -WinHeatGain( SurfNum );
								WinHeatLossRepEnergy( SurfNum ) = WinHeatLossRep( SurfNum ) * TimeStepZoneSec;
							}

							TempSurfIn( SurfNum ) = TempSurfInTmp( SurfNum );
						}
					}
				}
			} // ...end of inside surface heat balance equation selection

			TH12 = TempSurfInRep( SurfNum ) = TempSurfIn( SurfNum );
			TempSurfOut( SurfNum ) = TH11; // For reporting

			//if ( std::isnan( TempSurfInRep( SurfNum ) ) ) { // Use IEEE_IS_NAN when GFortran supports it
				//// throw Error
				//ShowFatalError( "Inside surface temperature is out of bound = " + Surface( SurfNum ).Name );
			//}
			// sign convention is positive means energy going into inside face from the air.
			auto const HConvInTemp_fac( -HConvIn_surf * ( TempSurfIn( SurfNum ) - RefAirTemp( SurfNum ) ) );
			QdotConvInRep( SurfNum ) = surface.Area * HConvInTemp_fac;
			QdotConvInRepPerArea( SurfNum ) = HConvInTemp_fac;
			QConvInReport( SurfNum ) = QdotConvInRep( SurfNum ) * TimeStepZoneSec;

			// The QdotConvInRep which is called "Surface Inside Face Convection Heat Gain" is stored during
			// sizing for both the normal and pulse cases so that load components can be derived later.
			if ( ZoneSizingCalc && CompLoadReportIsReq ) {
				if ( ! WarmupFlag ) {
					TimeStepInDay = ( HourOfDay - 1 ) * NumOfTimeStepInHour + TimeStep;
					if ( isPulseZoneSizing ) {
						loadConvectedWithPulse( CurOverallSimDay, TimeStepInDay, SurfNum ) = QdotConvInRep( SurfNum );
					} else {
						loadConvectedNormal( CurOverallSimDay, TimeStepInDay, SurfNum ) = QdotConvInRep( SurfNum );
						netSurfRadSeq( CurOverallSimDay, TimeStepInDay, SurfNum ) = QdotRadNetSurfInRep( SurfNum );
					}
				}
			}

			if ( SurfaceWindow( SurfNum ).OriginalClass == SurfaceClass_TDD_Diffuser ) { // Tubular daylighting device
				// Tubular daylighting devices are treated as one big object with an effective R value.
				// The outside face temperature of the TDD:DOME and the inside face temperature of the
				// TDD:DIFFUSER are calculated with the outside and inside heat balances respectively.
				// Below, the resulting temperatures are copied to the inside face of the TDD:DOME
				// and the outside face of the TDD:DIFFUSER for reporting.

				// Set inside temp variables of TDD:DOME equal to inside temp of TDD:DIFFUSER
				TH( 2, 1, SurfNum2 ) = TempSurfIn( SurfNum2 ) = TempSurfInTmp( SurfNum2 ) = TempSurfInRep( SurfNum2 ) = TempSurfIn( SurfNum );

				// Set outside temp reporting variable of TDD:DOME (since it gets skipped otherwise)
				// Reset outside temp variables of TDD:DIFFUSER equal to outside temp of TDD:DOME
				TH11 = TempSurfOut( SurfNum ) = TempSurfOut( SurfNum2 ) = TH( 1, 1, SurfNum2 );
			}

			if ( ( TH12 > MaxSurfaceTempLimit ) || ( TH12 < MinSurfaceTempLimit ) ) {
				if ( WarmupFlag ) ++WarmupSurfTemp;
				if ( ! WarmupFlag || WarmupSurfTemp > 10 || DisplayExtraWarnings ) {
					if ( TH12 < MinSurfaceTempLimit ) {
						if ( surface.LowTempErrCount == 0 ) {
							ShowSevereMessage( "Temperature (low) out of bounds [" + RoundSigDigits( TH12, 2 ) + "] for zone=\"" + zone.Name + "\", for surface=\"" + surface.Name + "\"" );
							ShowContinueErrorTimeStamp( "" );
							if ( ! zone.TempOutOfBoundsReported ) {
								ShowContinueError( "Zone=\"" + zone.Name + "\", Diagnostic Details:" );
								if ( zone.FloorArea > 0.0 ) {
									ShowContinueError( "...Internal Heat Gain [" + RoundSigDigits( zone.InternalHeatGains / zone.FloorArea, 3 ) + "] W/m2" );
								} else {
									ShowContinueError( "...Internal Heat Gain (no floor) [" + RoundSigDigits( zone.InternalHeatGains, 3 ) + "] W" );
								}
								if ( SimulateAirflowNetwork <= AirflowNetworkControlSimple ) {
									ShowContinueError( "...Infiltration/Ventilation [" + RoundSigDigits( zone.NominalInfilVent, 3 ) + "] m3/s" );
									ShowContinueError( "...Mixing/Cross Mixing [" + RoundSigDigits( zone.NominalMixing, 3 ) + "] m3/s" );
								} else {
									ShowContinueError( "...Airflow Network Simulation: Nominal Infiltration/Ventilation/Mixing not available." );
								}
								if ( zone.IsControlled ) {
									ShowContinueError( "...Zone is part of HVAC controlled system." );
								} else {
									ShowContinueError( "...Zone is not part of HVAC controlled system." );
								}
								zone.TempOutOfBoundsReported = true;
							}
							ShowRecurringSevereErrorAtEnd( "Temperature (low) out of bounds for zone=" + zone.Name + " for surface=" + surface.Name, surface.LowTempErrCount, TH12, TH12, _, "C", "C" );
						} else {
							ShowRecurringSevereErrorAtEnd( "Temperature (low) out of bounds for zone=" + zone.Name + " for surface=" + surface.Name, surface.LowTempErrCount, TH12, TH12, _, "C", "C" );
						}
					} else {
						if ( surface.HighTempErrCount == 0 ) {
							ShowSevereMessage( "Temperature (high) out of bounds (" + RoundSigDigits( TH12, 2 ) + "] for zone=\"" + zone.Name + "\", for surface=\"" + surface.Name + "\"" );
							ShowContinueErrorTimeStamp( "" );
							if ( ! zone.TempOutOfBoundsReported ) {
								ShowContinueError( "Zone=\"" + zone.Name + "\", Diagnostic Details:" );
								if ( zone.FloorArea > 0.0 ) {
									ShowContinueError( "...Internal Heat Gain [" + RoundSigDigits( zone.InternalHeatGains / zone.FloorArea, 3 ) + "] W/m2" );
								} else {
									ShowContinueError( "...Internal Heat Gain (no floor) [" + RoundSigDigits( zone.InternalHeatGains, 3 ) + "] W" );
								}
								if ( SimulateAirflowNetwork <= AirflowNetworkControlSimple ) {
									ShowContinueError( "...Infiltration/Ventilation [" + RoundSigDigits( zone.NominalInfilVent, 3 ) + "] m3/s" );
									ShowContinueError( "...Mixing/Cross Mixing [" + RoundSigDigits( zone.NominalMixing, 3 ) + "] m3/s" );
								} else {
									ShowContinueError( "...Airflow Network Simulation: Nominal Infiltration/Ventilation/Mixing not available." );
								}
								if ( zone.IsControlled ) {
									ShowContinueError( "...Zone is part of HVAC controlled system." );
								} else {
									ShowContinueError( "...Zone is not part of HVAC controlled system." );
								}
								zone.TempOutOfBoundsReported = true;
							}
							ShowRecurringSevereErrorAtEnd( "Temperature (high) out of bounds for zone=" + zone.Name + " for surface=" + surface.Name, surface.HighTempErrCount, TH12, TH12, _, "C", "C" );
						} else {
							ShowRecurringSevereErrorAtEnd( "Temperature (high) out of bounds for zone=" + zone.Name + " for surface=" + surface.Name, surface.HighTempErrCount, TH12, TH12, _, "C", "C" );
						}
					}
					if ( zone.EnforcedReciprocity ) {
						if ( WarmupSurfTemp > 3 ) {
							ShowSevereError( "CalcHeatBalanceInsideSurf: Zone=\"" + zone.Name + "\" has view factor enforced reciprocity" );
							ShowContinueError( " and is having temperature out of bounds errors. Please correct zone geometry and rerun." );
							ShowFatalError( "CalcHeatBalanceInsideSurf: Program terminates due to preceding conditions." );
						}
					} else if ( WarmupSurfTemp > 10 ) {
						ShowFatalError( "CalcHeatBalanceInsideSurf: Program terminates due to preceding conditions." );
					}
				}
			}
			if ( ( TH12 > MaxSurfaceTempLimitBeforeFatal ) || ( TH12 < MinSurfaceTempLimitBeforeFatal ) ) {
				if ( ! WarmupFlag ) {
					if ( TH12 < MinSurfaceTempLimitBeforeFatal ) {
						ShowSevereError( "Temperature (low) out of bounds [" + RoundSigDigits( TH12, 2 ) + "] for zone=\"" + zone.Name + "\", for surface=\"" + surface.Name + "\"" );
						ShowContinueErrorTimeStamp( "" );
						if ( ! zone.TempOutOfBoundsReported ) {
							ShowContinueError( "Zone=\"" + zone.Name + "\", Diagnostic Details:" );
							if ( zone.FloorArea > 0.0 ) {
								ShowContinueError( "...Internal Heat Gain [" + RoundSigDigits( zone.InternalHeatGains / zone.FloorArea, 3 ) + "] W/m2" );
							} else {
								ShowContinueError( "...Internal Heat Gain (no floor) [" + RoundSigDigits( zone.InternalHeatGains / zone.FloorArea, 3 ) + "] W" );
							}
							if ( SimulateAirflowNetwork <= AirflowNetworkControlSimple ) {
								ShowContinueError( "...Infiltration/Ventilation [" + RoundSigDigits( zone.NominalInfilVent, 3 ) + "] m3/s" );
								ShowContinueError( "...Mixing/Cross Mixing [" + RoundSigDigits( zone.NominalMixing, 3 ) + "] m3/s" );
							} else {
								ShowContinueError( "...Airflow Network Simulation: Nominal Infiltration/Ventilation/Mixing not available." );
							}
							if ( zone.IsControlled ) {
								ShowContinueError( "...Zone is part of HVAC controlled system." );
							} else {
								ShowContinueError( "...Zone is not part of HVAC controlled system." );
							}
							zone.TempOutOfBoundsReported = true;
						}
						ShowFatalError( "Program terminates due to preceding condition." );
					} else {
						ShowSevereError( "Temperature (high) out of bounds [" + RoundSigDigits( TH12, 2 ) + "] for zone=\"" + zone.Name + "\", for surface=\"" + surface.Name + "\"" );
						ShowContinueErrorTimeStamp( "" );
						if ( ! zone.TempOutOfBoundsReported ) {
							ShowContinueError( "Zone=\"" + zone.Name + "\", Diagnostic Details:" );
							if ( zone.FloorArea > 0.0 ) {
								ShowContinueError( "...Internal Heat Gain [" + RoundSigDigits( zone.InternalHeatGains / zone.FloorArea, 3 ) + "] W/m2" );
							} else {
								ShowContinueError( "...Internal Heat Gain (no floor) [" + RoundSigDigits( zone.InternalHeatGains / zone.FloorArea, 3 ) + "] W" );
							}
							if ( SimulateAirflowNetwork <= AirflowNetworkControlSimple ) {
								ShowContinueError( "...Infiltration/Ventilation [" + RoundSigDigits( zone.NominalInfilVent, 3 ) + "] m3/s" );
								ShowContinueError( "...Mixing/Cross Mixing [" + RoundSigDigits( zone.NominalMixing, 3 ) + "] m3/s" );
							} else {
								ShowContinueError( "...Airflow Network Simulation: Nominal Infiltration/Ventilation/Mixing not available." );
							}
							if ( zone.IsControlled ) {
								ShowContinueError( "...Zone is part of HVAC controlled system." );
							} else {
								ShowContinueError( "...Zone is not part of HVAC controlled system." );
							}
							zone.TempOutOfBoundsReported = true;
						}
						ShowFatalError( "Program terminates due to preceding condition." );
					}
				}
			}
		} // ...end of loop over all surfaces for inside heat balances

		// Interzone surface updating: interzone surfaces have other side temperatures
		// which can vary as the simulation iterates through the inside heat
		// balance.  This block is intended to "lock" the opposite side (outside)
		// temperatures to the correct value, namely the value calculated by the
		// inside surface heat balance for the other side.
		assert( TH.index( 1, 1, 1 ) == 0u ); // Assumed for linear indexing below
		auto const l211( TH.index( 2, 1, 1 ) - 1 );
		for ( std::vector< int >::size_type iSurfToResimulate = 0u; iSurfToResimulate < nSurfToResimulate; ++iSurfToResimulate ) {
			SurfNum = SurfToResimulate[ iSurfToResimulate ];
			// Interzones must have an exterior boundary condition greater than zero
			// (meaning that the other side is a surface) and the surface number must
			// not be the surface itself (which is just a simple partition)
			int const surfExtBoundCond( Surface( SurfNum ).ExtBoundCond );
			if ( ( surfExtBoundCond > 0 ) && ( surfExtBoundCond != SurfNum ) ) {
				// Set the outside surface temperature to the inside surface temperature
				// of the interzone pair and reassign the reporting variable.  By going
				// through all of the surfaces, this should pick up the other side as well
				// as affect the next iteration.
				TempSurfOut( SurfNum ) = TH[ SurfNum - 1 ] = TH[ l211 + surfExtBoundCond ]; // [ SurfNum - 1 ] == ( 1, 1, SurfNum ) // [ l211 + surfExtBoundCond ] == ( 2, 1, surfExtBoundCond )
			}
		}

		++InsideSurfIterations;

		// Convergence check
		MaxDelTemp = 0.0;
		for ( std::vector< int >::size_type iHTSurfToResimulate = 0u; iHTSurfToResimulate < nHTSurfToResimulate; ++iHTSurfToResimulate ) { // Loop through all relevant surfaces to check for convergence...
			SurfNum = HTSurfToResimulate[ iHTSurfToResimulate ]; // Heat transfer surfaces only
			ConstrNum = Surface( SurfNum ).Construction;
			if ( Construct( ConstrNum ).TransDiff <= 0.0 ) { // Opaque surface
				MaxDelTemp = max( std::abs( TempSurfIn( SurfNum ) - TempInsOld( SurfNum ) ), MaxDelTemp );
				if ( Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_CondFD ) {
					// also check all internal nodes as well as surface faces
					MaxDelTemp = max( MaxDelTemp, SurfaceFD( SurfNum ).MaxNodeDelTemp );
				}
			}

		} // ...end of loop to check for convergence

		if ( ! useCondFDHTalg ) {
			if ( MaxDelTemp <= MaxAllowedDelTemp ) Converged = true;
		} else {
			if ( MaxDelTemp <= MaxAllowedDelTempCondFD ) Converged = true;

			//Feb2012      IF ((InsideSurfIterations > IterationsForCondFDRelaxChange) .and. (.NOT. Converged) .AND.   &
			//Feb2012          (.NOT. CondFDVariableProperties) ) THEN
			//Feb2012          ! adjust relaxation factor down, assume large number of iterations is result of instability
			//Feb2012        CondFDRelaxFactor = CondFDRelaxFactor * 0.9d0
			//Feb2012        IF (CondFDRelaxFactor < 0.2d0) CondFDRelaxFactor = 0.2d0

			// resets relaxation factor to speed up iterations when under-relaxatation is not needed.
			if ( InsideSurfIterations <= 1 ) {
				CondFDRelaxFactor = CondFDRelaxFactorInput;
			}
			if ( ( InsideSurfIterations > IterationsForCondFDRelaxChange ) && ! Converged ) {
				// adjust relaxation factor down, assume large number of iterations is result of instability
				CondFDRelaxFactor *= 0.9;
				if ( CondFDRelaxFactor < 0.1 ) CondFDRelaxFactor = 0.1;
			}

		}

#ifdef EP_Count_Calls
		NumMaxInsideSurfIterations = max( NumMaxInsideSurfIterations, InsideSurfIterations );
#endif

		if ( InsideSurfIterations < MinIterations ) Converged = false;

		if ( InsideSurfIterations > MaxIterations ) {
			if ( ! WarmupFlag ) {
				++ErrCount;
				if ( ErrCount < 16 ) {
					if ( ! useCondFDHTalg ) {
						ShowWarningError( "Inside surface heat balance did not converge with Max Temp Difference [C] =" + RoundSigDigits( MaxDelTemp, 3 ) + " vs Max Allowed Temp Diff [C] =" + RoundSigDigits( MaxAllowedDelTemp, 3 ) );
						ShowContinueErrorTimeStamp( "" );
					} else {
						ShowWarningError( "Inside surface heat balance did not converge with Max Temp Difference [C] =" + RoundSigDigits( MaxDelTemp, 3 ) + " vs Max Allowed Temp Diff [C] =" + RoundSigDigits( MaxAllowedDelTempCondFD, 6 ) );
						ShowContinueErrorTimeStamp( "" );
					}
				} else {
					ShowRecurringWarningErrorAtEnd( "Inside surface heat balance convergence problem continues", InsideSurfErrCount, MaxDelTemp, MaxDelTemp, _, "[C]", "[C]" );
				}
			}
			break; // DO loop
		}

	} // ...end of main inside heat balance DO loop (ends when Converged)

	// Update SumHmXXXX
	if ( useCondFDHTalg || any_eq( HeatTransferAlgosUsed, UseEMPD ) || any_eq( HeatTransferAlgosUsed, UseHAMT ) ) {
		for ( std::vector< int >::size_type iHTSurfToResimulate = 0u; iHTSurfToResimulate < nHTSurfToResimulate; ++iHTSurfToResimulate ) {
			SurfNum = HTSurfToResimulate[ iHTSurfToResimulate ]; // Heat transfer surfaces only
			auto const & surface( Surface( SurfNum ) );
			if ( surface.Class == SurfaceClass_Window ) continue;

			ZoneNum = surface.Zone;

			if ( surface.HeatTransferAlgorithm == HeatTransferModel_HAMT ) {
				UpdateHeatBalHAMT( SurfNum );

				Real64 const FD_Area_fac( HMassConvInFD( SurfNum ) * surface.Area );

				SumHmAW( ZoneNum ) += FD_Area_fac * ( RhoVaporSurfIn( SurfNum ) - RhoVaporAirIn( SurfNum ) );

				Real64 const MAT_zone( MAT( surface.Zone ) );
				RhoAirZone = PsyRhoAirFnPbTdbW( OutBaroPress, MAT_zone, PsyWFnTdbRhPb( MAT_zone, PsyRhFnTdbRhov( MAT_zone, RhoVaporAirIn( SurfNum ), rhoAirZone ), OutBaroPress ) );

				Real64 const surfInTemp( TempSurfInTmp( SurfNum ) );
				Wsurf = PsyWFnTdbRhPb( surfInTemp, PsyRhFnTdbRhov( surfInTemp, RhoVaporSurfIn( SurfNum ), wsurf ), OutBaroPress );

				SumHmARa( ZoneNum ) += FD_Area_fac * RhoAirZone;

				SumHmARaW( ZoneNum ) += FD_Area_fac * RhoAirZone * Wsurf;
			} else if ( surface.HeatTransferAlgorithm == HeatTransferModel_EMPD ) {
				// need to calculate the amount of moisture that is entering or
				// leaving the zone  Qm [kg/sec] = hmi * Area * (Del Rhov)
				// {Hmi [m/sec];     Area [m2];    Rhov [kg moist/m3]  }
				// Positive values are into the zone and negative values are
				// leaving the zone.  SumHmAw is the sum of the moisture entering or
				// leaving the zone from all of the surfaces and is a rate.  Multiply
				// by time to get the actual amount affecting the zone volume of air.

				UpdateMoistureBalanceEMPD( SurfNum );
				RhoVaporSurfIn( SurfNum ) = MoistEMPDNew( SurfNum );
				//SUMC(ZoneNum) = SUMC(ZoneNum)-MoistEMPDFlux(SurfNum)*Surface(SurfNum)%Area

				Real64 const FD_Area_fac( HMassConvInFD( SurfNum ) * surface.Area );
				SumHmAW( ZoneNum ) += FD_Area_fac * ( RhoVaporSurfIn( SurfNum ) - RhoVaporAirIn( SurfNum ) );
				Real64 const surfInTemp( TempSurfInTmp( SurfNum ) );
				SumHmARa( ZoneNum ) += FD_Area_fac * PsyRhoAirFnPbTdbW( OutBaroPress, surfInTemp, PsyWFnTdbRhPb( surfInTemp, PsyRhFnTdbRhovLBnd0C( surfInTemp, RhoVaporAirIn( SurfNum ) ), OutBaroPress ) );
				SumHmARaW( ZoneNum ) += FD_Area_fac * RhoVaporSurfIn( SurfNum );
			}
		}
	}

	// Calculate ZoneWinHeatGain/Loss
	if ( ! PartialResimulate ) {
		ZoneWinHeatGain = 0.0;
		ZoneWinHeatGainRep = 0.0;
		ZoneWinHeatGainRepEnergy = 0.0;
		ZoneWinHeatLossRep = 0.0;
		ZoneWinHeatLossRepEnergy = 0.0;
	} else {
		ZoneWinHeatGain( ZoneToResimulate ) = 0.0;
		ZoneWinHeatGainRep( ZoneToResimulate ) = 0.0;
		ZoneWinHeatGainRepEnergy( ZoneToResimulate ) = 0.0;
		ZoneWinHeatLossRep( ZoneToResimulate ) = 0.0;
		ZoneWinHeatLossRepEnergy( ZoneToResimulate ) = 0.0;
	}

	for ( std::vector< int >::size_type iSurfToResimulate = 0u; iSurfToResimulate < nSurfToResimulate; ++iSurfToResimulate ) { // Perform a heat balance on all of the relevant inside surfaces...
		SurfNum = SurfToResimulate[ iSurfToResimulate ];
		if ( ! Surface( SurfNum ).ExtSolar ) continue; // WindowManager's definition of ZoneWinHeatGain/Loss
		if ( Surface( SurfNum ).Class != SurfaceClass_Window ) continue;
		ZoneNum = Surface( SurfNum ).Zone;
		if ( ZoneNum == 0 ) continue;
		ZoneWinHeatGain( ZoneNum ) += WinHeatGain( SurfNum );
	}
	for ( int ZoneNum = ( PartialResimulate ? ZoneToResimulate() : 1 ), ZoneNum_end = ( PartialResimulate ? ZoneToResimulate() : NumOfZones ); ZoneNum <= ZoneNum_end; ++ZoneNum ) {
		if ( ZoneWinHeatGain( ZoneNum ) >= 0.0 ) {
			ZoneWinHeatGainRep( ZoneNum ) = ZoneWinHeatGain( ZoneNum );
			ZoneWinHeatGainRepEnergy( ZoneNum ) = ZoneWinHeatGainRep( ZoneNum ) * TimeStepZoneSec;
		} else {
			ZoneWinHeatLossRep( ZoneNum ) = -ZoneWinHeatGain( ZoneNum );
			ZoneWinHeatLossRepEnergy( ZoneNum ) = ZoneWinHeatLossRep( ZoneNum ) * TimeStepZoneSec;
		}
	}

	CalculateZoneMRT( ZoneToResimulate ); // Update here so that the proper value of MRT is available to radiant systems

	calcHeatBalanceInsideSurfFirstTime = false;

}

void
CalcOutsideSurfTemp(
	int const SurfNum, // Surface number DO loop counter
	int const ZoneNum, // Zone number the current surface is attached to
	int const ConstrNum, // Construction index for the current surface
	Real64 const HMovInsul, // "Convection" coefficient of movable insulation
	Real64 const TempExt // Exterior temperature boundary condition
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         George Walton
	//       DATE WRITTEN   December 1979
	//       MODIFIED       Jun 1990 (RDT for new CTF arrays)
	//                      Jul 2000 (RJL for Moisture algorithms)
	//                      Sep 2000 (RKS for new radiant exchange algorithm)
	//                      Dec 2000 (RKS for radiant system model addition)
	//                      Aug 2010 (BG added radiant heat flow rate reporting)
	//       RE-ENGINEERED  Mar 1998 (RKS)

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine performs a heat balance on the outside face of each
	// surface in the building.  NOTE that this also sets some coefficients
	// that are needed for radiant system modeling.  Thus, it is extremely
	// important that if someone makes changes to the heat balance equations
	// at a later date that they must also make changes to the coefficient
	// setting portion of this subroutine as well.

	// METHODOLOGY EMPLOYED:
	// Various boundary conditions are set and additional parameters are set-
	// up.  Then, the proper heat balance equation is selected based on the
	// presence of movable insulation, thermal mass of the surface construction,
	// and convection model being used.

	// REFERENCES:
	// (I)BLAST legacy routine HBOUT
	// 1989 ASHRAE Handbook of Fundamentals (Figure 1 on p. 22.4, convection correlations)

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGlobals;
	using namespace DataEnvironment;
	using namespace DataHeatBalFanSys;
	using namespace DataHeatBalance;
	using namespace DataHeatBalSurface;
	using namespace DataSurfaces;
	using DataMoistureBalance::TempOutsideAirFD;
	using DataMoistureBalance::RhoVaporAirOut;
	using DataMoistureBalance::RhoVaporAirIn;
	using DataMoistureBalance::HConvExtFD;
	using DataMoistureBalance::HMassConvExtFD;
	using DataMoistureBalance::HConvInFD;
	using DataMoistureBalance::HMassConvInFD;
	using DataMoistureBalance::RhoVaporSurfIn;
	using DataMoistureBalance::HSkyFD;
	using DataMoistureBalance::HGrndFD;
	using DataMoistureBalance::HAirFD;
	//unused0909  USE DataMoistureBalanceEMPD,   ONLY: MoistEMPDNew, MoistEMPDFlux
	using namespace DataDaylightingDevices;
	using DaylightingDevices::FindTDDPipe;
	using namespace Psychrometrics;

	// Locals
	// SUBROUTINE PARAMETER DEFINITIONS:

	// INTERFACE BLOCK SPECIFICATIONS
	// FUNCTION DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

	Real64 F1; // Intermediate calculation variable
	Real64 F2; // Intermediate calculation variable
	bool MovInsulPresent; // .TRUE. if movable insulation is currently present for surface
	bool QuickConductionSurf; // .TRUE. if the cross CTF term is relatively large
	int PipeNum; // TDD pipe object number
	int SurfNum2; // TDD:DIFFUSER object number
	int ZoneNum2; // TDD:DIFFUSER zone number
	Real64 Ueff; // 1 / effective R value between TDD:DOME and TDD:DIFFUSER
	Real64 RadTemp; // local value for Effective radiation temperature for OtherSideConditions model
	Real64 HRad; // local value for effective (linearized) radiation coefficient

	// FLOW:

	// Determine whether or not movable insulation is present
	MovInsulPresent = ( HMovInsul > 0.0 );

	// Determine whether this surface is a "slow conductive" or "quick conductive"
	// surface.  Designates are inherited from BLAST.  Basically, a "quick" surface
	// requires the inside heat balance to be accounted for in the heat balance
	// while a "slow" surface can used the last time step's value for inside
	// surface temperature.
	auto const & construct( Construct( ConstrNum ) );
	if ( construct.CTFCross( 0 ) > 0.01 ) {
		QuickConductionSurf = true;
		F1 = construct.CTFCross( 0 ) / ( construct.CTFInside( 0 ) + HConvIn( SurfNum ) );
	} else {
		QuickConductionSurf = false;
	}

	// Now, calculate the outside surface temperature using the proper heat balance equation.
	// Each case has been separated out into its own IF-THEN block for clarity.  Additional
	// cases can simply be added anywhere in the following section.  This is the last step
	// in the main loop.  Once the proper heat balance is done, the simulation goes on to
	// the next SurfNum.

	// Outside heat balance case: Tubular daylighting device
	Real64 & TH11( TH( 1, 1, SurfNum ) );
	if ( Surface( SurfNum ).Class == SurfaceClass_TDD_Dome ) {

		// Lookup up the TDD:DIFFUSER object
		PipeNum = FindTDDPipe( SurfNum );
		SurfNum2 = TDDPipe( PipeNum ).Diffuser;
		ZoneNum2 = Surface( SurfNum2 ).Zone;
		Ueff = 1.0 / TDDPipe( PipeNum ).Reff;
		F1 = Ueff / ( Ueff + HConvIn( SurfNum2 ) );

		// Similar to opaque surface but inside conditions of TDD:DIFFUSER are used, and no embedded sources/sinks.
		// Absorbed shortwave radiation is treated similar to a regular window, but only 1 glass layer is allowed.
		//   QRadSWOutAbs(SurfNum) does not apply for TDD:DOME, must use QRadSWwinAbs(SurfNum,1)/2.0 instead.
		//+Construct(ConstrNum)%CTFSourceOut(0)     &   TDDs cannot be radiant systems
		// *QsrcHist(1,SurfNum)                     &
		//+Construct(ConstrNum)%CTFSourceIn(0) &   TDDs cannot be radiant systems
		// *QsrcHist(1,SurfNum)                &
		TH11 = ( QRadSWwinAbs( 1, SurfNum ) / 2.0 + ( HcExtSurf( SurfNum ) + HAirExtSurf( SurfNum ) ) * TempExt + HSkyExtSurf( SurfNum ) * SkyTemp + HGrdExtSurf( SurfNum ) * OutDryBulbTemp + F1 * ( QRadSWwinAbs( 1, SurfNum2 ) / 2.0 + QRadThermInAbs( SurfNum2 ) + HConvIn( SurfNum2 ) * MAT( ZoneNum2 ) + NetLWRadToSurf( SurfNum2 ) ) ) / ( Ueff + HcExtSurf( SurfNum ) + HAirExtSurf( SurfNum ) + HSkyExtSurf( SurfNum ) + HGrdExtSurf( SurfNum ) - F1 * Ueff ); // Instead of QRadSWOutAbs(SurfNum) | ODB used to approx ground surface temp | Use TDD:DIFFUSER surface | Use TDD:DIFFUSER surface | Use TDD:DIFFUSER surface and zone | Use TDD:DIFFUSER surface

		// Outside heat balance case: No movable insulation, slow conduction
	} else if ( ( ! MovInsulPresent ) && ( ! QuickConductionSurf ) ) {
		if ( Surface( SurfNum ).OSCMPtr == 0 ) {
			TH11 = ( -CTFConstOutPart( SurfNum ) + QRadSWOutAbs( SurfNum ) + ( HcExtSurf( SurfNum ) + HAirExtSurf( SurfNum ) ) * TempExt + HSkyExtSurf( SurfNum ) * SkyTemp + HGrdExtSurf( SurfNum ) * OutDryBulbTemp + construct.CTFCross( 0 ) * TempSurfIn( SurfNum ) + construct.CTFSourceOut( 0 ) * QsrcHist( SurfNum, 1 ) ) / ( construct.CTFOutside( 0 ) + HcExtSurf( SurfNum ) + HAirExtSurf( SurfNum ) + HSkyExtSurf( SurfNum ) + HGrdExtSurf( SurfNum ) ); // ODB used to approx ground surface temp
			// Outside Heat Balance case: Other Side Conditions Model
		} else { //( Surface(SurfNum)%OSCMPtr > 0 ) THEN
			// local copies of variables for clarity in radiation terms
			RadTemp = OSCM( Surface( SurfNum ).OSCMPtr ).TRad;
			HRad = OSCM( Surface( SurfNum ).OSCMPtr ).HRad;

			// patterned after "No movable insulation, slow conduction," but with new radiation terms and no sun,
			TH11 = ( -CTFConstOutPart( SurfNum ) + HcExtSurf( SurfNum ) * TempExt + HRad * RadTemp + construct.CTFCross( 0 ) * TempSurfIn( SurfNum ) + construct.CTFSourceOut( 0 ) * QsrcHist( SurfNum, 1 ) ) / ( construct.CTFOutside( 0 ) + HcExtSurf( SurfNum ) + HRad );
		}
		// Outside heat balance case: No movable insulation, quick conduction
	} else if ( ( ! MovInsulPresent ) && ( QuickConductionSurf ) ) {
		if ( Surface( SurfNum ).OSCMPtr == 0 ) {
			TH11 = ( -CTFConstOutPart( SurfNum ) + QRadSWOutAbs( SurfNum ) + ( HcExtSurf( SurfNum ) + HAirExtSurf( SurfNum ) ) * TempExt + HSkyExtSurf( SurfNum ) * SkyTemp + HGrdExtSurf( SurfNum ) * OutDryBulbTemp + construct.CTFSourceOut( 0 ) * QsrcHist( SurfNum, 1 ) + F1 * ( CTFConstInPart( SurfNum ) + QRadSWInAbs( SurfNum ) + QRadThermInAbs( SurfNum ) + construct.CTFSourceIn( 0 ) * QsrcHist( SurfNum, 1 ) + HConvIn( SurfNum ) * MAT( ZoneNum ) + NetLWRadToSurf( SurfNum ) ) ) / ( construct.CTFOutside( 0 ) + HcExtSurf( SurfNum ) + HAirExtSurf( SurfNum ) + HSkyExtSurf( SurfNum ) + HGrdExtSurf( SurfNum ) - F1 * construct.CTFCross( 0 ) ); // ODB used to approx ground surface temp | MAT use here is problem for room air models
			// Outside Heat Balance case: Other Side Conditions Model
		} else { //( Surface(SurfNum)%OSCMPtr > 0 ) THEN
			// local copies of variables for clarity in radiation terms
			RadTemp = OSCM( Surface( SurfNum ).OSCMPtr ).TRad;
			HRad = OSCM( Surface( SurfNum ).OSCMPtr ).HRad;
			// patterned after "No movable insulation, quick conduction," but with new radiation terms and no sun,
			TH11 = ( -CTFConstOutPart( SurfNum ) + HcExtSurf( SurfNum ) * TempExt + HRad * RadTemp + construct.CTFSourceOut( 0 ) * QsrcHist( SurfNum, 1 ) + F1 * ( CTFConstInPart( SurfNum ) + QRadSWInAbs( SurfNum ) + QRadThermInAbs( SurfNum ) + construct.CTFSourceIn( 0 ) * QsrcHist( SurfNum, 1 ) + HConvIn( SurfNum ) * MAT( ZoneNum ) + NetLWRadToSurf( SurfNum ) ) ) / ( construct.CTFOutside( 0 ) + HcExtSurf( SurfNum ) + HRad - F1 * construct.CTFCross( 0 ) ); // MAT use here is problem for room air models
		}
		// Outside heat balance case: Movable insulation, slow conduction
	} else if ( ( MovInsulPresent ) && ( ! QuickConductionSurf ) ) {

		F2 = HMovInsul / ( HMovInsul + HcExtSurf( SurfNum ) + HAirExtSurf( SurfNum ) + HSkyExtSurf( SurfNum ) + HGrdExtSurf( SurfNum ) );

		TH11 = ( -CTFConstOutPart( SurfNum ) + QRadSWOutAbs( SurfNum ) + construct.CTFCross( 0 ) * TempSurfIn( SurfNum ) + F2 * ( QRadSWOutMvIns( SurfNum ) + ( HcExtSurf( SurfNum ) + HAirExtSurf( SurfNum ) ) * TempExt + HSkyExtSurf( SurfNum ) * SkyTemp + HGrdExtSurf( SurfNum ) * OutDryBulbTemp ) ) / ( construct.CTFOutside( 0 ) + HMovInsul - F2 * HMovInsul ); // ODB used to approx ground surface temp

		// Outside heat balance case: Movable insulation, quick conduction
	} else if ( ( MovInsulPresent ) && ( QuickConductionSurf ) ) {

		F2 = HMovInsul / ( HMovInsul + HcExtSurf( SurfNum ) + HAirExtSurf( SurfNum ) + HSkyExtSurf( SurfNum ) + HGrdExtSurf( SurfNum ) );

		TH11 = ( -CTFConstOutPart( SurfNum ) + QRadSWOutAbs( SurfNum ) + F1 * ( CTFConstInPart( SurfNum ) + QRadSWInAbs( SurfNum ) + QRadThermInAbs( SurfNum ) + HConvIn( SurfNum ) * MAT( ZoneNum ) + NetLWRadToSurf( SurfNum ) ) + F2 * ( QRadSWOutMvIns( SurfNum ) + ( HcExtSurf( SurfNum ) + HAirExtSurf( SurfNum ) ) * TempExt + HSkyExtSurf( SurfNum ) * SkyTemp + HGrdExtSurf( SurfNum ) * OutDryBulbTemp ) ) / ( construct.CTFOutside( 0 ) + HMovInsul - F2 * HMovInsul - F1 * construct.CTFCross( 0 ) ); // ODB used to approx ground surface temp

	} // ...end of outside heat balance cases IF-THEN block

	// multiply out linearized radiation coeffs for reporting
	Real64 const HExtSurf_fac( -( HSkyExtSurf( SurfNum ) * ( TH11 - SkyTemp ) + HAirExtSurf( SurfNum ) * ( TH11 - TempExt ) + HGrdExtSurf( SurfNum ) * ( TH11 - OutDryBulbTemp ) ) );
	QdotRadOutRep( SurfNum ) = Surface( SurfNum ).Area * HExtSurf_fac;
	QdotRadOutRepPerArea( SurfNum ) = HExtSurf_fac;
	QRadOutReport( SurfNum ) = QdotRadOutRep( SurfNum ) * TimeStepZoneSec;
	// Set the radiant system heat balance coefficients if this surface is also a radiant system
	if ( construct.SourceSinkPresent ) {

		if ( MovInsulPresent ) {
			// Note: if movable insulation is ever added back in correctly, the heat balance equations above must be fixed
			ShowFatalError( "Movable insulation is not allowed on a radiant system surface at this time" );

		} else {
			Real64 const RadSysDiv( 1.0 / ( construct.CTFOutside( 0 ) + HcExtSurf( SurfNum ) + HAirExtSurf( SurfNum ) + HSkyExtSurf( SurfNum ) + HGrdExtSurf( SurfNum ) ) );

			RadSysToHBConstCoef( SurfNum ) = ( -CTFConstOutPart( SurfNum ) + QRadSWOutAbs( SurfNum ) + ( HcExtSurf( SurfNum ) + HAirExtSurf( SurfNum ) ) * TempExt + HSkyExtSurf( SurfNum ) * SkyTemp + HGrdExtSurf( SurfNum ) * OutDryBulbTemp ) * RadSysDiv; // ODB used to approx ground surface temp

			RadSysToHBTinCoef( SurfNum ) = construct.CTFCross( 0 ) * RadSysDiv;

			RadSysToHBQsrcCoef( SurfNum ) = construct.CTFSourceOut( 0 ) * RadSysDiv;
		}

	}

}

void
CalcExteriorVentedCavity( int const SurfNum ) // index of surface
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         B Griffith
	//       DATE WRITTEN   January 2005
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// manages calculating the temperatures of baffle and air cavity for
	// multi-skin configuration.

	// METHODOLOGY EMPLOYED:
	// derived from CalcPassiveTranspiredCollector

	// REFERENCES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::SecInHour;
	using DataEnvironment::SkyTemp;
	using DataEnvironment::SunIsUp;
	using DataEnvironment::OutBaroPress;
	using DataEnvironment::IsRain;
	using Psychrometrics::PsyRhoAirFnPbTdbW;
	using Psychrometrics::PsyCpAirFnWTdb;
	using Psychrometrics::PsyWFnTdbTwbPb;
	using DataSurfaces::Surface;
	using DataSurfaces::ExtVentedCavity;
	using DataSurfaces::OSCM;
	//unused0909  USE DataHVACGlobals , ONLY: TimeStepSys
	using ConvectionCoefficients::InitExteriorConvectionCoeff;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	static std::string const BlankString;

	// INTERFACE BLOCK SPECIFICATIONS:
	// DERIVED TYPE DEFINITIONS:

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

	// local working variables
	Real64 AspRat; // Aspect Ratio of gap
	Real64 TmpTscoll;
	Real64 TmpTaPlen;
	Real64 RhoAir;
	Real64 holeArea;
	Real64 HrPlen;
	Real64 HcPlen;
	Real64 Isc;
	Real64 MdotVent;
	Real64 VdotWind;
	Real64 VdotThermal;
	int CavNum; // do loop counter
	int iter; // do loop counter
	int thisOSCM;
	Real64 TempExt;
	Real64 OutHumRatExt;

	CavNum = Surface( SurfNum ).ExtCavNum;

	TempExt = Surface( SurfNum ).OutDryBulbTemp;

	OutHumRatExt = PsyWFnTdbTwbPb( Surface( SurfNum ).OutDryBulbTemp, Surface( SurfNum ).OutWetBulbTemp, OutBaroPress );

	RhoAir = PsyRhoAirFnPbTdbW( OutBaroPress, TempExt, OutHumRatExt );

	holeArea = ExtVentedCavity( CavNum ).ActualArea * ExtVentedCavity( CavNum ).Porosity;

	AspRat = ExtVentedCavity( CavNum ).HdeltaNPL * 2.0 / ExtVentedCavity( CavNum ).PlenGapThick;
	TmpTscoll = ExtVentedCavity( CavNum ).TbaffleLast;
	TmpTaPlen = ExtVentedCavity( CavNum ).TairLast;

	// all the work is done in this routine located in GeneralRoutines.cc

	for ( iter = 1; iter <= 3; ++iter ) { // this is a sequential solution approach.

		CalcPassiveExteriorBaffleGap( ExtVentedCavity( CavNum ).SurfPtrs, holeArea, ExtVentedCavity( CavNum ).Cv, ExtVentedCavity( CavNum ).Cd, ExtVentedCavity( CavNum ).HdeltaNPL, ExtVentedCavity( CavNum ).SolAbsorp, ExtVentedCavity( CavNum ).LWEmitt, ExtVentedCavity( CavNum ).Tilt, AspRat, ExtVentedCavity( CavNum ).PlenGapThick, ExtVentedCavity( CavNum ).BaffleRoughness, ExtVentedCavity( CavNum ).QdotSource, TmpTscoll, TmpTaPlen, HcPlen, HrPlen, Isc, MdotVent, VdotWind, VdotThermal );

	} // sequential solution
	//now fill results into derived types
	ExtVentedCavity( CavNum ).Isc = Isc;
	ExtVentedCavity( CavNum ).TAirCav = TmpTaPlen;
	ExtVentedCavity( CavNum ).Tbaffle = TmpTscoll;
	ExtVentedCavity( CavNum ).HrPlen = HrPlen;
	ExtVentedCavity( CavNum ).HcPlen = HcPlen;
	ExtVentedCavity( CavNum ).PassiveACH = ( MdotVent / RhoAir ) * ( 1.0 / ( ExtVentedCavity( CavNum ).ProjArea * ExtVentedCavity( CavNum ).PlenGapThick ) ) * SecInHour;
	ExtVentedCavity( CavNum ).PassiveMdotVent = MdotVent;
	ExtVentedCavity( CavNum ).PassiveMdotWind = VdotWind * RhoAir;
	ExtVentedCavity( CavNum ).PassiveMdotTherm = VdotThermal * RhoAir;

	// now do some updates
	ExtVentedCavity( CavNum ).TairLast = ExtVentedCavity( CavNum ).TAirCav;
	ExtVentedCavity( CavNum ).TbaffleLast = ExtVentedCavity( CavNum ).Tbaffle;

	// update the OtherSideConditionsModel coefficients.
	thisOSCM = ExtVentedCavity( CavNum ).OSCMPtr;

	OSCM( thisOSCM ).TConv = ExtVentedCavity( CavNum ).TAirCav;
	OSCM( thisOSCM ).HConv = ExtVentedCavity( CavNum ).HcPlen;
	OSCM( thisOSCM ).TRad = ExtVentedCavity( CavNum ).Tbaffle;
	OSCM( thisOSCM ).HRad = ExtVentedCavity( CavNum ).HrPlen;

}

void
GatherComponentLoadsSurfAbsFact()
{
	// SUBROUTINE INFORMATION:
	//       AUTHOR         Jason Glazer
	//       DATE WRITTEN   September 2012
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	//   Gather values during sizing used for surface absorption factors

	// METHODOLOGY EMPLOYED:
	//   Save sequence of values for report during sizing.

	// REFERENCES:
	// na

	// USE STATEMENTS:
	// na
	// Using/Aliasing
	using DataGlobals::HourOfDay;
	using DataGlobals::TimeStep;
	using DataGlobals::NumOfZones;
	using DataGlobals::NumOfTimeStepInHour;
	using DataGlobals::CompLoadReportIsReq;
	using DataGlobals::isPulseZoneSizing;
	using DataSizing::CurOverallSimDay;
	using DataHeatBalance::TMULT;
	using DataHeatBalance::ITABSF;
	using DataSurfaces::TotSurfaces;
	using DataSurfaces::Surface;
	using DataSurfaces::SurfaceClass_TDD_Dome;
	using OutputReportTabular::ITABSFseq;
	using OutputReportTabular::TMULTseq;

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
	static int iZone( 0 );
	static int jSurf( 0 );
	static int TimeStepInDay( 0 );

	if ( CompLoadReportIsReq && ! isPulseZoneSizing ) {
		TimeStepInDay = ( HourOfDay - 1 ) * NumOfTimeStepInHour + TimeStep;
		for ( iZone = 1; iZone <= NumOfZones; ++iZone ) {
			TMULTseq( CurOverallSimDay, TimeStepInDay, iZone ) = TMULT( iZone );
		}
		for ( jSurf = 1; jSurf <= TotSurfaces; ++jSurf ) {
			iZone = Surface( jSurf ).Zone;
			if ( ! Surface( jSurf ).HeatTransSurf || iZone == 0 ) continue; // Skip non-heat transfer surfaces
			if ( Surface( jSurf ).Class == SurfaceClass_TDD_Dome ) continue; // Skip tubular daylighting device domes
			ITABSFseq( CurOverallSimDay, TimeStepInDay, jSurf ) = ITABSF( jSurf );
		}
	}
}

} // HeatBalanceSurfaceManager

} // EnergyPlus
