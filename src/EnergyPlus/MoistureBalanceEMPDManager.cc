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
#include <MoistureBalanceEMPDManager.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataIPShortCuts.hh>
#include <DataMoistureBalanceEMPD.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSurfaces.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <OutputProcessor.hh>
#include <Psychrometrics.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace MoistureBalanceEMPDManager {

	// Module containing the routines to calculate moisture adsorption and desorption
	// at interior wall surfaces

	// MODULE INFORMATION:
	//   Authors:        Muthusamy Swami and Lixing Gu
	//   Date written:   August, 1999
	//   Modified:       na
	//   Re-engineered:  na

	// PURPOSE OF THIS MODULE:
	// To calculate moisture adsorption and desorption at interior wall surfaces
	// using EMPD model (Effective Moisture Penetration Depth) developed by
	// Florida Solar Energy Center. Input consists of interior surface temperatures
	// and sorption curve of interior layer materials. Output consists of mositure
	// fluxes from wall interior surfaces, which will be used in zone moisture balance.

	// METHODOLOGY EMPLOYED:
	// Add something
	// EMPD is a simplified method of analysing moisture transport in buildings and
	// is easy to incorporate into existing building energy analysis computer codes.
	// The components of the moisture balance equation involving moisture adsorption
	// and desorption are described in detail where the concept of EMPD is discussed.
	// The assumptions. parameters required, and limitations of the model are also discussed.
	// Results of simulation using the model and comparison with measured data are given.
	// Data of isotherms compiled from the literature of some commonly used building materials are also given.

	// REFERENCES:
	// Kerestecioglu A A., Swami M V., Kamel A A., "Theoretical and computational
	// investigation of simultaneous heat and moisture transfer in buildings: 'Effective
	// penetration depth' theory," ASHRAE Trans., 1990, Vol. 96, Part 1, 447-454

	// OTHER NOTES:

	// USE STATEMENTS:
	// Use statements for data used in the module
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataEnvironment::OutBaroPress;
	using namespace DataHeatBalance;
	using namespace DataGlobals;
	using DataHeatBalFanSys::ZoneAirHumRat;
	using DataSurfaces::TotSurfaces;
	using DataSurfaces::Surface;
	using DataSurfaces::SurfaceClass_Window;
	using namespace DataMoistureBalanceEMPD;

	// Data
	// MODULE VARIABLE and Function DECLARATIONs
	Array1D< Real64 > RhoVapEMPD; // Inside Surface Vapor Density Reporting variable
	Array1D< Real64 > WSurfEMPD; // Inside Surface Humidity Ratio Reporting variable
	Array1D< Real64 > RHEMPD; // Inside Surface Relative Humidity Reporting variable

	// SUBROUTINE SPECIFICATION FOR MODULE MoistureBalanceEMPDManager

	//******************************************************************************

	// Functions

	void
	GetMoistureBalanceEMPDInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Muthusamy V. Swami and Lixing Gu
		//       DATE WRITTEN   August 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is the main driver for initializations within the
		// heat balance using the EMPD model.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using DataSurfaces::HeatTransferModel_EMPD;

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
		int IOStat; // IO Status when calling get input subroutine
		Array1D_string MaterialNames( 3 ); // Number of Material Alpha names defined
		int MaterNum; // Counter to keep track of the material number
		int MaterialNumAlpha; // Number of material alpha names being passed
		int MaterialNumProp; // Number of material properties being passed
		Array1D< Real64 > MaterialProps( 5 ); // Temporary array to transfer material properties
		static bool ErrorsFound( false ); // If errors detected in input

		int EMPDMat; // EMPD Moisture Material additional properties for each base material
		int Loop;
		int Layer;
		int SurfNum; // Surface number
		int MatNum; // Material number at interior layer
		int ConstrNum; // Construction number
		Array1D_bool EMPDzone; // EMPD property check for each zone
		static int ErrCount( 0 );

		// Load the additional EMPD Material properties
		cCurrentModuleObject = "MaterialProperty:MoisturePenetrationDepth:Settings";
		EMPDMat = GetNumObjectsFound( cCurrentModuleObject );

		if ( EMPDMat == 0 ) {
			ShowSevereError( "EMPD Solution requested, but no \"" + cCurrentModuleObject + "\" objects were found." );
			ErrorsFound = true;
		}

		for ( Loop = 1; Loop <= EMPDMat; ++Loop ) {

			//Call Input Get routine to retrieve material data
			GetObjectItem( cCurrentModuleObject, Loop, MaterialNames, MaterialNumAlpha, MaterialProps, MaterialNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			//Load the material derived type from the input data.
			MaterNum = FindItemInList( MaterialNames( 1 ), Material );
			if ( MaterNum == 0 ) {
				ShowSevereError( cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 1 ) + " entered=" + MaterialNames( 1 ) + ", must match to a valid Material name." );
				ErrorsFound = true;
				continue;
			}

			// See if Material was defined with R only.  (No density is defined then and not applicable for EMPD).
			//  What about materials other than "regular materials" (e.g. Glass, Air, etc)
			if ( Material( MaterNum ).Group == RegularMaterial && MaterialProps( 1 ) > 0.0 ) {
				if ( Material( MaterNum ).ROnly ) {
					//        CALL ShowSevereError('EMPD base material = "'//TRIM(Material(MaterNum)%Name)//  &
					//                             '" was Material:NoMass. It cannot be used for EMPD calculations.')
					ShowContinueError( "..Only Material base materials are allowed to have EMPD properties." );
					ShowSevereError( cCurrentModuleObject + ": Reference Material is not appropriate type for EMPD properties, material=" + Material( MaterNum ).Name + ", must have regular properties (L,Cp,K,D)" );
					ErrorsFound = true;
				}
			}
			if ( Material( MaterNum ).Group != RegularMaterial ) {
				//      CALL ShowSevereError('GetMoistureBalanceEMPDInput: Only Material:Regular base materials are allowed '// &
				//                           'to have EMPD properties, material = '// TRIM(Material(MaterNum)%Name))
				ShowSevereError( cCurrentModuleObject + ": Reference Material is not appropriate type for EMPD properties, material=" + Material( MaterNum ).Name + ", must have regular properties (L,Cp,K,D)" );
				ErrorsFound = true;
			}

			// Once the material derived type number is found then load the additional moisture material properties
			Material( MaterNum ).EMPDMaterialProps = true;
			Material( MaterNum ).EMPDVALUE = MaterialProps( 1 );
			Material( MaterNum ).MoistACoeff = MaterialProps( 2 );
			Material( MaterNum ).MoistBCoeff = MaterialProps( 3 );
			Material( MaterNum ).MoistCCoeff = MaterialProps( 4 );
			Material( MaterNum ).MoistDCoeff = MaterialProps( 5 );

		}

		// Ensure at least one interior EMPD surface for each zone
		EMPDzone.dimension( NumOfZones, false );
		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			if ( ! Surface( SurfNum ).HeatTransSurf || Surface( SurfNum ).Class == SurfaceClass_Window ) continue; // Heat transfer surface only and not a window
			if ( Surface( SurfNum ).HeatTransferAlgorithm != HeatTransferModel_EMPD ) continue;
			ConstrNum = Surface( SurfNum ).Construction;
			MatNum = Construct( ConstrNum ).LayerPoint( Construct( ConstrNum ).TotLayers );
			if ( Material( MatNum ).EMPDVALUE > 0.0 && Surface( SurfNum ).Zone > 0 ) {
				EMPDzone( Surface( SurfNum ).Zone ) = true;
			} else {
				++ErrCount;
				if ( ErrCount == 1 && ! DisplayExtraWarnings ) {
					ShowMessage( "GetMoistureBalanceEMPDInput: EMPD properties are not assigned to the inside layer of Surfaces" );
					ShowContinueError( "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual surfaces." );
				}
				if ( DisplayExtraWarnings ) {
					ShowMessage( "GetMoistureBalanceEMPDInput: EMPD properties are not assigned to the inside layer in Surface=" + Surface( SurfNum ).Name );
					ShowContinueError( "with Construction=" + Construct( ConstrNum ).Name );
				}
			}
			if ( Construct( ConstrNum ).TotLayers == 1 ) { // One layer construction
				continue;
			} else { // Multiple layer construction
				if ( Material( Construct( ConstrNum ).LayerPoint( 1 ) ).EMPDMaterialProps && Surface( SurfNum ).ExtBoundCond <= 0 ) { // The external layer is not exposed to zone
					ShowSevereError( "GetMoistureBalanceEMPDInput: EMPD properties are assigned to the outside layer in Construction=" + Construct( ConstrNum ).Name );
					ShowContinueError( "..Outside layer material with EMPD properties = " + Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Name );
					ShowContinueError( "..A material with EMPD properties must be assigned to the inside layer of a construction." );
					ErrorsFound = true;
				}
				for ( Layer = 2; Layer <= Construct( ConstrNum ).TotLayers - 1; ++Layer ) {
					if ( Material( Construct( ConstrNum ).LayerPoint( Layer ) ).EMPDMaterialProps ) {
						ShowSevereError( "GetMoistureBalanceEMPDInput: EMPD properties are assigned to a middle layer in Construction=" + Construct( ConstrNum ).Name );
						ShowContinueError( "..Middle layer material with EMPD properties = " + Material( Construct( ConstrNum ).LayerPoint( Layer ) ).Name );
						ShowContinueError( "..A material with EMPD properties must be assigned to the inside layer of a construction." );
						ErrorsFound = true;
					}
				}
			}
		}

		for ( Loop = 1; Loop <= NumOfZones; ++Loop ) {
			if ( ! EMPDzone( Loop ) ) {
				ShowSevereError( "GetMoistureBalanceEMPDInput: None of the constructions for zone = " + Zone( Loop ).Name + " has an inside layer with EMPD properties" );
				ShowContinueError( "..For each zone, the inside layer of at least one construction must have EMPD properties" );
				ErrorsFound = true;
			}
		}

		EMPDzone.deallocate();

		ReportMoistureBalanceEMPD();

		if ( ErrorsFound ) {
			ShowFatalError( "GetMoistureBalanceEMPDInput: Errors found getting EMPD material properties, program terminated." );
		}

	}

	void
	InitMoistureBalanceEMPD()
	{

		// SUBROUTINE INFORMATION:
		//   Authors:        Muthusamy Swami and Lixing Gu
		//   Date written:   August, 1999
		//   Modified:       na
		//   Re-engineered:  na

		// PURPOSE OF THIS SUBROUTINE:
		// Create dynamic array for surface moisture calculation

		// METHODOLOGY EMPLOYED:

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
		int ZoneNum;
		int Loop;
		int SurfNum;
		static bool InitEnvrnFlag( true );

		if ( InitEnvrnFlag ) {
			MoistEMPDOld.allocate( TotSurfaces );
			MoistEMPDInt.allocate( TotSurfaces );
			MoistEMPDNew.allocate( TotSurfaces );
			MoistEMPDFlux.allocate( TotSurfaces );
			RhoVapEMPD.allocate( TotSurfaces );
			WSurfEMPD.allocate( TotSurfaces );
			RHEMPD.allocate( TotSurfaces );
		}

		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			ZoneNum = Surface( SurfNum ).Zone;
			if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
			if ( ZoneAirHumRat( ZoneNum ) == 0.0 ) {
				MoistEMPDOld( SurfNum ) = 0.0001;
				MoistEMPDInt( SurfNum ) = 0.0001;
				MoistEMPDNew( SurfNum ) = 0.0001;
			} else {
				MoistEMPDOld( SurfNum ) = ZoneAirHumRat( ZoneNum ); // Surface moisture level initialization
				MoistEMPDInt( SurfNum ) = ZoneAirHumRat( ZoneNum ); // by assuming initial values be equal to ZoneAirHumRat
				MoistEMPDNew( SurfNum ) = ZoneAirHumRat( ZoneNum );
			}
		}
		if ( ! InitEnvrnFlag ) return;
		//Initialize the report variable
		RhoVapEMPD = 0.015;
		WSurfEMPD = 0.015;
		RHEMPD = 0.0;
		MoistEMPDFlux = 0.0;

		GetMoistureBalanceEMPDInput();

		for ( Loop = 1; Loop <= TotSurfaces; ++Loop ) {
			if ( ! Surface( Loop ).HeatTransSurf ) continue;
			if ( Surface( Loop ).Class == SurfaceClass_Window ) continue;
			SetupOutputVariable( "EMPD Surface Inside Face Water Vapor Density [kg/m3]", RhoVapEMPD( Loop ), "Zone", "State", Surface( Loop ).Name );
			SetupOutputVariable( "EMPD Surface Inside Face Humidity Ratio [kgWater/kgDryAir]", WSurfEMPD( Loop ), "Zone", "State", Surface( Loop ).Name );
			SetupOutputVariable( "EMPD Surface Inside Face Relative Humidity [%]", RHEMPD( Loop ), "Zone", "State", Surface( Loop ).Name );
		}

		if ( InitEnvrnFlag ) InitEnvrnFlag = false;

	}

	void
	CalcMoistureBalanceEMPD(
		int const SurfNum,
		Real64 const TempSurfIn, // INSIDE SURFACE TEMPERATURE at current time step
		Real64 const TempSurfInOld, // INSIDE SURFACE TEMPERATURE at previous time step.
		Real64 const TempZone, // Zone temperature at current time step.
		Real64 & TempSat // Satutare surface temperature.
	)
	{

		// SUBROUTINE INFORMATION:
		//   Authors:        Muthusamy Swami and Lixing Gu
		//   Date written:   August, 1999
		//   Modified:       na
		//   Re-engineered:  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate surface moisture level using EMPD model

		// METHODOLOGY EMPLOYED:
		// na

		// Using/Aliasing
		using Psychrometrics::PsyRhFnTdbWPb;
		using Psychrometrics::PsyRhFnTdbRhovLBnd0C;
		using Psychrometrics::PsyWFnTdbRhPb;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyCpAirFnWTdb;
		using Psychrometrics::PsyRhovFnTdbWPb;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const Error( 0.01 ); // Totlarence (%)
		Real64 const RLXM( 0.3 ); // Relaxation factor (0-1)
		Real64 const Lam( 2.5e6 ); // Heat of vaporization (J/kg)
		static std::string const RoutineName( "CalcMoistureEMPD" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NOFITR; // Number of iterations
		int ZoneNum; // Surface number
		int MatNum; // Material number at interior layer
		int ConstrNum; // Construction number
		Real64 RHOBULK; // Material bulk density
		Real64 HM; // Moisture transfer coefficient
		Real64 Taver; // Average zone temperature between current time and previous time
		//    REAL(r64)    :: Waver     ! Average zone humidity ratio between current time and previous time
		Real64 RHaver; // Average zone relative humidity {0-1} between current time and previous time
		Real64 RVaver; // Average zone vapor density
		Real64 AT;
		Real64 BR;
		Real64 RALPHA; // Zone vapor density
		Real64 BB; // Coefficient for ODE
		Real64 CC; // Coefficient for ODE
		Real64 ErrorM; // Percent error
		int Flag; // Convergence flag (0 - converged)
		static bool OneTimeFlag( true );
		Real64 Wsurf; // Surface moisture flux
		Real64 PVsurf; // Surface vapor pressure

		//    if (OneTimeFlag) then
		//       Call InitMoistureBalanceEMPD
		//       OneTimeFlag = .False.
		//    end if

		if ( BeginEnvrnFlag && OneTimeFlag ) {
			InitMoistureBalanceEMPD();
			OneTimeFlag = false;
		}

		if ( ! BeginEnvrnFlag ) {
			OneTimeFlag = true;
		}

		MoistEMPDFlux( SurfNum ) = 0.0;
		Flag = 1;
		NOFITR = 0;
		if ( ! Surface( SurfNum ).HeatTransSurf ) {
			return;
		}
		ConstrNum = Surface( SurfNum ).Construction;
		MatNum = Construct( ConstrNum ).LayerPoint( Construct( ConstrNum ).TotLayers ); // Then find the material pointer

		ZoneNum = Surface( SurfNum ).Zone;
		auto const & material( Material( MatNum ) );
		if ( material.EMPDVALUE <= 0.0 ) {
			MoistEMPDNew( SurfNum ) = PsyRhovFnTdbWPb( TempZone, ZoneAirHumRat( ZoneNum ), OutBaroPress );
			return;
		}

		Taver = ( TempSurfIn + TempSurfInOld ) / 2.0;
		Real64 const Taver_237( Taver + 237.7 );
		Real64 const RHaver_fac( 461.52 * ( Taver + KelvinConv ) * std::exp( -23.7093 + 4111.0 / Taver_237 ) );
		Real64 const BR_fac( ( 4111.0 / pow_2( Taver_237 ) ) - ( 1.0 / ( Taver + KelvinConv ) ) );

		while ( Flag > 0 ) {
			RVaver = ( MoistEMPDNew( SurfNum ) + MoistEMPDOld( SurfNum ) ) / 2.0;
			RHaver = RVaver * RHaver_fac;
			if ( RHaver > 1.0 ) {
				RHaver = 1.0;
			} else if ( RHaver < 0.0 ) {
				RHaver = 0.00001;
			}

			AT = ( material.MoistACoeff * material.MoistBCoeff * std::pow( RHaver, material.MoistBCoeff ) + material.MoistCCoeff * material.MoistDCoeff * std::pow( RHaver, material.MoistDCoeff ) ) / RVaver;
			BR = BR_fac * AT * RVaver;
			RHOBULK = material.Density;
			HM = HConvIn( SurfNum ) / ( PsyRhoAirFnPbTdbW( OutBaroPress, TempZone, ZoneAirHumRat( ZoneNum ), RoutineName ) * PsyCpAirFnWTdb( ZoneAirHumRat( ZoneNum ), TempZone ) );
			ZoneNum = Surface( SurfNum ).Zone;
			RALPHA = ZoneAirHumRat( ZoneNum ) * OutBaroPress / ( 461.52 * ( TempZone + KelvinConv ) * ( ZoneAirHumRat( ZoneNum ) + 0.62198 ) );
			BB = HM / ( RHOBULK * material.EMPDVALUE * AT );
			CC = BB * RALPHA + BR / AT * ( TempSurfIn - TempSurfInOld ) / TimeStepZoneSec;
			SolverMoistureBalanceEMPD( MoistEMPDNew( SurfNum ), MoistEMPDOld( SurfNum ), 1.0, BB, CC );

			Flag = 0;
			ErrorM = std::abs( ( MoistEMPDNew( SurfNum ) - MoistEMPDInt( SurfNum ) ) / MoistEMPDInt( SurfNum ) ) * 100.0;
			if ( ErrorM > Error ) ++Flag;

			++NOFITR;
			if ( NOFITR > 500 ) {
				ShowFatalError( "Iteration limit exceeded in EMPD model, program terminated." );
			}

			if ( Flag > 0 ) {
				MoistEMPDNew( SurfNum ) = MoistEMPDNew( SurfNum ) * RLXM + MoistEMPDInt( SurfNum ) * ( 1.0 - RLXM );
			}
			MoistEMPDInt( SurfNum ) = MoistEMPDNew( SurfNum );
		}

		// Calculate latent load
		PVsurf = RHaver * std::exp( 23.7093 - 4111.0 / Taver_237 );
		Wsurf = 0.62198 * RHaver / ( std::exp( -23.7093 + 4111.0 / Taver_237 ) * OutBaroPress - RHaver );
		MoistEMPDFlux( SurfNum ) = HM * ( MoistEMPDNew( SurfNum ) - PsyRhoAirFnPbTdbW( OutBaroPress, TempZone, ZoneAirHumRat( ZoneNum ), RoutineName ) * ZoneAirHumRat( ZoneNum ) ) * Lam;
		// Calculate surface dew point temperature based on surface vapor density
		TempSat = 4111.0 / ( 23.7093 - std::log( PVsurf ) ) + 35.45 - KelvinConv;

		// Put results in the single precision reporting variable
		RhoVapEMPD( SurfNum ) = MoistEMPDNew( SurfNum );
		RHEMPD( SurfNum ) = PsyRhFnTdbRhovLBnd0C( TempSurfIn, RhoVapEMPD( SurfNum ), RoutineName ) * 100.0;
		WSurfEMPD( SurfNum ) = PsyWFnTdbRhPb( TempSurfIn, RHEMPD( SurfNum ) / 100.0, OutBaroPress, RoutineName );

	}

	void
	SolverMoistureBalanceEMPD(
		Real64 & VARNEW, // Value at current time step
		Real64 const VAROLD, // Value at previous time step
		Real64 const A, // Coefficient of time derivative in AdV/dt+BV=C
		Real64 const B, // Coefficienct of variable
		Real64 const C // Constant
	)
	{

		// SUBROUTINE INFORMATION:
		//   Authors:        Muthusamy Swami and Lixing Gu
		//   Date writtenn:  August, 1999
		//   Modified:       na
		//   Re-engineered:  na

		// PURPOSE OF THIS SUBROUTINE:
		// Solve a first order ordinary differential equation, A dV/dt + B V = C

		// METHODOLOGY EMPLOYED:
		// Finite difference method

		// Using/Aliasing

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

		VARNEW = ( VAROLD + TimeStepZoneSec * C / A ) / ( 1.0 + TimeStepZoneSec * B / A );

	}

	void
	CloseMoistureBalanceEMPD()
	{

		// SUBROUTINE INFORMATION:
		//   Authors:        Muthusamy Swami and Lixing Gu
		//   Date writtenn:  August, 1999
		//   Modified:       na
		//   Re-engineered:  na

		// PURPOSE OF THIS SUBROUTINE:
		// Deallocate dynamic arrays for surface moisture calculation

		// METHODOLOGY EMPLOYED:

		// USE STATEMENTS:

		MoistEMPDOld.deallocate();
		MoistEMPDInt.deallocate();
		MoistEMPDNew.deallocate();
		MoistEMPDFlux.deallocate();

	}

	void
	UpdateMoistureBalanceEMPD( int const SurfNum ) // Surface number
	{

		// SUBROUTINE INFORMATION:
		//   Authors:        Muthusamy Swami and Lixing Gu
		//   Date writtenn:  August, 1999
		//   Modified:       na
		//   Re-engineered:  na

		// PURPOSE OF THIS SUBROUTINE:
		// Update inside surface vapor density
		// METHODOLOGY EMPLOYED:

		// USE STATEMENTS:

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

		MoistEMPDOld( SurfNum ) = MoistEMPDNew( SurfNum );

	}

	void
	ReportMoistureBalanceEMPD()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Lixing Gu
		//       DATE WRITTEN   August 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine gives a detailed report to the user about
		// EMPD Properties of each construction.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// The subroutine of ReportCTFs written by Linda Lawrie was used to develop this routine.

		// Using/Aliasing
		using General::ScanForReports;

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
		bool DoReport;

		int ConstrNum;
		int MatNum;

		// Formats
		static gio::Fmt Format_700( "(' Construction EMPD, ',A,', ',A,', ',4(F8.4,', '),F8.4)" );

		ScanForReports( "Constructions", DoReport, "Constructions" );

		if ( ! DoReport ) return;
		//   Write Descriptions
		gio::write( OutputFileInits, fmtA ) << "! <Construction EMPD>, Construction Name, Inside Layer Material Name, Penetration Depth {m}, a, b, c, d";

		for ( ConstrNum = 1; ConstrNum <= TotConstructs; ++ConstrNum ) {
			if ( Construct( ConstrNum ).TypeIsWindow ) continue;
			MatNum = Construct( ConstrNum ).LayerPoint( Construct( ConstrNum ).TotLayers );
			if ( Material( MatNum ).EMPDMaterialProps ) {
				gio::write( OutputFileInits, Format_700 ) << Construct( ConstrNum ).Name << Material( MatNum ).Name << Material( MatNum ).EMPDVALUE << Material( MatNum ).MoistACoeff << Material( MatNum ).MoistBCoeff << Material( MatNum ).MoistCCoeff << Material( MatNum ).MoistDCoeff;
			}
		}

	}

} // MoistureBalanceEMPDManager

} // EnergyPlus
