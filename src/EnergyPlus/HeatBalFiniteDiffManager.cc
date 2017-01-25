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
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <HeatBalFiniteDiffManager.hh>
#include <DataAirflowNetwork.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalSurface.hh>
#include <DataIPShortCuts.hh>
#include <DataMoistureBalance.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSurfaces.hh>
#include <General.hh>
#include <HeatBalanceMovableInsulation.hh>
#include <InputProcessor.hh>
#include <OutputProcessor.hh>
#include <Psychrometrics.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace HeatBalFiniteDiffManager {

	// Module containing the heat balance simulation routines

	// MODULE INFORMATION:
	//       AUTHOR         Richard J. Liesen
	//       DATE WRITTEN   October 2003
	//       RE-ENGINEERED  Curtis Pedersen, 2006, Changed to Implicit FD calc for conduction.
	//                      and included enthalpy formulations for phase change materials
	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and algorithms required to
	// manage the fiite difference heat balance simulation on the building.

	// METHODOLOGY EMPLOYED:

	// REFERENCES:
	// The MFD moisture balance method
	//  C. O. Pedersen, Enthalpy Formulation of conduction heat transfer problems
	//    involving latent heat, Simulation, Vol 18, No. 2, February 1972

	// OTHER NOTES:

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::KelvinConv;
	using DataGlobals::OutputFileDebug;
	using DataGlobals::NumOfTimeStepInHour;
	using DataGlobals::DisplayExtraWarnings;
	using DataGlobals::TimeStepZoneSec;
	using DataGlobals::HourOfDay;
	using DataGlobals::TimeStep;
	using DataGlobals::OutputFileInits;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::DayOfSim;
	using DataGlobals::WarmupFlag;
	using DataGlobals::SecInHour;
	using namespace DataMoistureBalance;
	using DataHeatBalance::Material;
	using DataHeatBalance::TotMaterials;
	using DataHeatBalance::MaxLayersInConstruct;
	using DataHeatBalance::QRadThermInAbs;
	using DataHeatBalance::Construct;
	using DataHeatBalance::TotConstructs;
	using DataHeatBalance::UseCondFD;
	using DataHeatBalance::RegularMaterial;
	using DataHeatBalance::Air;
	using DataHeatBalance::Zone;
	using DataHeatBalSurface::NetLWRadToSurf;
	using DataHeatBalSurface::QRadSWOutAbs;
	using DataHeatBalSurface::QRadSWInAbs;
	using DataHeatBalSurface::QRadSWOutMvIns;
	using DataHeatBalSurface::TempSource;
	using DataHeatBalSurface::OpaqSurfInsFaceConductionFlux;
	using DataHeatBalSurface::OpaqSurfOutsideFaceConductionFlux;
	using DataHeatBalSurface::OpaqSurfOutsideFaceConduction;
	using DataHeatBalSurface::OpaqSurfInsFaceConduction;
	using DataHeatBalSurface::QdotRadOutRepPerArea;
	using DataHeatBalSurface::MinSurfaceTempLimit;
	using DataHeatBalSurface::MaxSurfaceTempLimit;
	using DataHeatBalSurface::QdotRadNetSurfInRep;
	using DataHeatBalSurface::QRadNetSurfInReport;
	using DataSurfaces::Surface;
	using DataSurfaces::TotSurfaces;
	using DataSurfaces::Ground;
	using DataSurfaces::SurfaceClass_Window;
	using DataSurfaces::HeatTransferModel_CondFD;
	using DataHeatBalFanSys::MAT;
	using DataHeatBalFanSys::ZoneAirHumRat;
	using DataHeatBalFanSys::QHTRadSysSurf;
	using DataHeatBalFanSys::QHWBaseboardSurf;
	using DataHeatBalFanSys::QSteamBaseboardSurf;
	using DataHeatBalFanSys::QElecBaseboardSurf;
	using DataEnvironment::SkyTemp;
	using DataEnvironment::IsRain;
	using Psychrometrics::PsyRhFnTdbRhovLBnd0C;
	using Psychrometrics::PsyWFnTdbRhPb;
	using Psychrometrics::PsyHgAirFnWTdb;
	// Fan system Source/Sink heat value, and source/sink location temp from CondFD
	using DataHeatBalFanSys::QRadSysSource;
	using DataHeatBalFanSys::TCondFDSourceNode;
	using DataHeatBalFanSys::QPVSysSource;
	using HeatBalanceMovableInsulation::EvalOutsideMovableInsulation;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	Real64 const Lambda( 2500000.0 );
	Real64 const smalldiff( 1.e-8 ); // Used in places where "equality" tests should not be used.

	int const CrankNicholsonSecondOrder( 1 ); // original CondFD scheme.  semi implicit, second order in time
	int const FullyImplicitFirstOrder( 2 ); // fully implicit scheme, first order in time.
	Array1D_string const cCondFDSchemeType( 2, { "CrankNicholsonSecondOrder", "FullyImplicitFirstOrder" } );

	Real64 const TempInitValue( 23.0 ); // Initialization value for Temperature
	Real64 const RhovInitValue( 0.0115 ); // Initialization value for Rhov
	Real64 const EnthInitValue( 100.0 ); // Initialization value for Enthalpy

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:

	//REAL(r64) :: TFDout   =0.0d0
	//REAL(r64) :: TFDin    =0.0d0
	//REAL(r64) :: rhovFDout=0.0d0
	//REAL(r64) :: rhovFDin =0.0d0
	//REAL(r64) :: TDryout  =0.0d0
	//REAL(r64) :: Tdryin   =0.0d0
	//REAL(r64) :: RHOut    =0.0d0
	//REAL(r64) :: RHIn     =0.0d0
	Array1D< Real64 > SigmaR; // Total Resistance of construction layers
	Array1D< Real64 > SigmaC; // Total Capacitance of construction layers

	//REAL(r64), ALLOCATABLE, DIMENSION(:)   :: WSurfIn         !Humidity Ratio of the inside surface for reporting
	//REAL(r64), ALLOCATABLE, DIMENSION(:)   :: QMassInFlux     !MassFlux on Surface for reporting
	//REAL(r64), ALLOCATABLE, DIMENSION(:)   :: QMassOutFlux    !MassFlux on Surface for reporting
	Array1D< Real64 > QHeatInFlux; // HeatFlux on Surface for reporting
	Array1D< Real64 > QHeatOutFlux; // HeatFlux on Surface for reporting
	//REAL(r64), ALLOCATABLE, DIMENSION(:)   :: QFluxZoneToInSurf !sum of Heat flows at the surface to air interface,
	//                                 ! zone-side boundary conditions W/m2 before CR 8280 was not reported, but was calculated.
	//REAL(r64), ALLOCATABLE, DIMENSION(:)   :: QFluxOutsideToOutSurf !sum of Heat flows at the surface to air interface, Out-side boundary conditions W/m2
	//                                                           ! before CR 8280 was
	//REAL(r64), ALLOCATABLE, DIMENSION(:)   :: QFluxInArrivSurfCond !conduction between surface node and first node into the surface (sensible)
	//                                                           ! before CR 8280 was -- Qdryin    !HeatFlux on Surface for reporting for Sensible only
	//REAL(r64), ALLOCATABLE, DIMENSION(:)   :: QFluxOutArrivSurfCond  !HeatFlux on Surface for reporting for Sensible only
	//                                                                 ! before CR 8280 -- Qdryout         !HeatFlux on Surface for reporting for Sensible only

	int CondFDSchemeType( FullyImplicitFirstOrder ); // solution scheme for CondFD - default
	Real64 SpaceDescritConstant( 3.0 ); // spatial descritization constant,
	Real64 MinTempLimit( -100.0 ); // lower limit check, degree C
	Real64 MaxTempLimit( 100.0 ); // upper limit check, degree C
	//feb2012 INTEGER   :: MaxGSiter = 200  ! maximum number of Gauss Seidel iterations
	int MaxGSiter( 30 ); // maximum number of Gauss Seidel iterations
	Real64 fracTimeStepZone_Hour( 0.0 );
	bool GetHBFiniteDiffInputFlag( true );
	int WarmupSurfTemp( 0 );
	// Subroutine Specifications for the Heat Balance Module
	// Driver Routines

	// Initialization routines for module

	// Algorithms for the module

	// Reporting routines for module

	// Update Data Routine

	// Object Data
	Array1D< ConstructionDataFD > ConstructFD;
	Array1D< SurfaceDataFD > SurfaceFD;
	Array1D< MaterialDataFD > MaterialFD;

	// MODULE SUBROUTINES:
	//*************************************************************************

	// Functions

	void
	clear_state()
	{
		SigmaR.deallocate();
		SigmaC.deallocate();
		QHeatInFlux.deallocate();
		QHeatOutFlux.deallocate();
		CondFDSchemeType = FullyImplicitFirstOrder;
		SpaceDescritConstant = 3.0;
		MinTempLimit = -100.0;
		MaxTempLimit = 100.0;
		MaxGSiter = 30;
		fracTimeStepZone_Hour = 0.0;
		GetHBFiniteDiffInputFlag = true;
		WarmupSurfTemp = 0;
		ConstructFD.deallocate();
		SurfaceFD.deallocate();
		MaterialFD.deallocate();
	}

	void
	ManageHeatBalFiniteDiff(
		int const SurfNum,
		Real64 & TempSurfInTmp, // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		Real64 & TempSurfOutTmp // Outside Surface Temperature of each Heat Transfer Surface
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   May 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages the moisture balance method.  It is called
		// from the HeatBalanceManager at the time step level.
		// This driver manages the calls to all of
		// the other drivers and simulation algorithms.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// FLOW:

		// Get the moisture balance input at the beginning of the simulation only
		if ( GetHBFiniteDiffInputFlag ) {
			// Obtains conduction FD related parameters from input file
			GetCondFDInput();
			GetHBFiniteDiffInputFlag = false;
		}

		// Condition is taken care of by calling routine:
		// IF (Surface(SurfNum)%HeatTransSurf .and. Surface(SurfNum)%Class /= SurfaceClass_Window)

		// Solve the zone heat & moisture balance using a finite difference solution
		CalcHeatBalFiniteDiff( SurfNum, TempSurfInTmp, TempSurfOutTmp );

	}

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetCondFDInput()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Curtis Pedersen
		//       DATE WRITTEN   July 2006
		//       MODIFIED       Brent Griffith Mar 2011, user settings
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is the main driver for initializations for the variable property CondFD part of the
		// MFD algorithm

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using DataHeatBalance::MaxAllowedDelTempCondFD;
		using DataHeatBalance::CondFDRelaxFactor;
		using DataHeatBalance::CondFDRelaxFactorInput;
		using General::RoundSigDigits;

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
		Array1D_string ConstructionName( 3 ); // Name of Construction with CondFDsimplified
		int MaterNum; // Counter to keep track of the material number
		int MaterialNumAlpha; // Number of material alpha names being passed
		int MaterialNumProp; // Number of material properties being passed
		Array1D< Real64 > MaterialProps( 40 ); // Temporary array to transfer material properties
		static bool ErrorsFound( false ); // If errors detected in input
		//  INTEGER :: CondFDMat                ! Number of variable property CondFD materials in input
		int Loop;
		int NumAlphas;
		int NumNumbers;
		int propNum;
		int pcMat;
		int vcMat;
		int inegptr;
		bool nonInc;

		// user settings for numerical parameters
		cCurrentModuleObject = "HeatBalanceSettings:ConductionFiniteDifference";

		if ( GetNumObjectsFound( cCurrentModuleObject ) > 0 ) {
			GetObjectItem( cCurrentModuleObject, 1, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			if ( ! lAlphaFieldBlanks( 1 ) ) {

				{ auto const SELECT_CASE_var( cAlphaArgs( 1 ) );

				if ( SELECT_CASE_var == "CRANKNICHOLSONSECONDORDER" ) {
					CondFDSchemeType = CrankNicholsonSecondOrder;
				} else if ( SELECT_CASE_var == "FULLYIMPLICITFIRSTORDER" ) {
					CondFDSchemeType = FullyImplicitFirstOrder;
				} else {
					ShowSevereError( cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 1 ) + " entered=" + cAlphaArgs( 1 ) + ", must match CrankNicholsonSecondOrder or FullyImplicitFirstOrder." );
					ErrorsFound = true;
				}}

			}

			if ( ! lNumericFieldBlanks( 1 ) ) {
				SpaceDescritConstant = rNumericArgs( 1 );
			}
			if ( ! lNumericFieldBlanks( 2 ) ) {
				CondFDRelaxFactorInput = rNumericArgs( 2 );
				CondFDRelaxFactor = CondFDRelaxFactorInput;
			}
			if ( ! lNumericFieldBlanks( 3 ) ) {
				MaxAllowedDelTempCondFD = rNumericArgs( 3 );
			}

		} // settings object

		pcMat = GetNumObjectsFound( "MaterialProperty:PhaseChange" );
		vcMat = GetNumObjectsFound( "MaterialProperty:VariableThermalConductivity" );

		MaterialFD.allocate( TotMaterials );

		// Load the additional CondFD Material properties
		cCurrentModuleObject = "MaterialProperty:PhaseChange"; // Phase Change Information First

		if ( pcMat != 0 ) { //  Get Phase Change info
			//    CondFDVariableProperties = .TRUE.
			for ( Loop = 1; Loop <= pcMat; ++Loop ) {

				//Call Input Get routine to retrieve material data
				GetObjectItem( cCurrentModuleObject, Loop, MaterialNames, MaterialNumAlpha, MaterialProps, MaterialNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				//Load the material derived type from the input data.
				MaterNum = FindItemInList( MaterialNames( 1 ), Material );
				if ( MaterNum == 0 ) {
					ShowSevereError( cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 1 ) + " entered=" + MaterialNames( 1 ) + ", must match to a valid Material name." );
					ErrorsFound = true;
					continue;
				}

				if ( Material( MaterNum ).Group != RegularMaterial ) {
					ShowSevereError( cCurrentModuleObject + ": Reference Material is not appropriate type for CondFD properties, material=" + Material( MaterNum ).Name + ", must have regular properties (L,Cp,K,D)" );
					ErrorsFound = true;
				}

				// Once the material derived type number is found then load the additional CondFD variable material properties
				//   Some or all may be zero (default).  They will be checked when calculating node temperatures
				MaterialFD( MaterNum ).tk1 = MaterialProps( 1 );
				MaterialFD( MaterNum ).numTempEnth = ( MaterialNumProp - 1 ) / 2;
				if ( MaterialFD( MaterNum ).numTempEnth * 2 != ( MaterialNumProp - 1 ) ) {
					ShowSevereError( "GetCondFDInput: " + cCurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", mismatched pairs" );
					ShowContinueError( "...expected " + RoundSigDigits( MaterialFD( MaterNum ).numTempEnth ) + " pairs, but only entered " + RoundSigDigits( MaterialNumProp - 1 ) + " numbers." );
					ErrorsFound = true;
				}
				MaterialFD( MaterNum ).TempEnth.dimension( 2, MaterialFD( MaterNum ).numTempEnth, 0.0 );
				propNum = 2;
				// Temperature first
				for ( int pcount = 1, pcount_end = MaterialFD( MaterNum ).numTempEnth; pcount <= pcount_end; ++pcount ) {
					MaterialFD( MaterNum ).TempEnth( 1, pcount ) = MaterialProps( propNum );
					propNum += 2;
				}
				propNum = 3;
				// Then Enthalpy
				for ( int pcount = 1, pcount_end = MaterialFD( MaterNum ).numTempEnth; pcount <= pcount_end; ++pcount ) {
					MaterialFD( MaterNum ).TempEnth( 2, pcount ) = MaterialProps( propNum );
					propNum += 2;
				}
				nonInc = false;
				inegptr = 0;
				for ( int pcount = 1, pcount_end = MaterialFD( MaterNum ).numTempEnth - 1; pcount <= pcount_end; ++pcount ) {
					if ( MaterialFD( MaterNum ).TempEnth( 1, pcount ) < MaterialFD( MaterNum ).TempEnth( 1, pcount + 1 ) ) continue;
					nonInc = true;
					inegptr = pcount + 1;
					break;
				}
				if ( nonInc ) {
					ShowSevereError( "GetCondFDInput: " + cCurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", non increasing Temperatures. Temperatures must be strictly increasing." );
					ShowContinueError( "...occurs first at item=[" + RoundSigDigits( inegptr ) + "], value=[" + RoundSigDigits( MaterialFD( MaterNum ).TempEnth( 1, inegptr ), 2 ) + "]." );
					ErrorsFound = true;
				}
				nonInc = false;
				inegptr = 0;
				for ( int pcount = 1, pcount_end = MaterialFD( MaterNum ).numTempEnth - 1; pcount <= pcount_end; ++pcount ) {
					if ( MaterialFD( MaterNum ).TempEnth( 2, pcount ) <= MaterialFD( MaterNum ).TempEnth( 2, pcount + 1 ) ) continue;
					nonInc = true;
					inegptr = pcount + 1;
					break;
				}
				if ( nonInc ) {
					ShowSevereError( "GetCondFDInput: " + cCurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", non increasing Enthalpy." );
					ShowContinueError( "...occurs first at item=[" + RoundSigDigits( inegptr ) + "], value=[" + RoundSigDigits( MaterialFD( MaterNum ).TempEnth( 2, inegptr ), 2 ) + "]." );
					ShowContinueError( "...These values may be Cp (Specific Heat) rather than Enthalpy.  Please correct." );
					ErrorsFound = true;
				}
			}
		}
		//   Get CondFD Variable Thermal Conductivity Input

		cCurrentModuleObject = "MaterialProperty:VariableThermalConductivity"; // Variable Thermal Conductivity Info next
		if ( vcMat != 0 ) { //  variable k info
			//    CondFDVariableProperties = .TRUE.
			for ( Loop = 1; Loop <= vcMat; ++Loop ) {

				//Call Input Get routine to retrieve material data
				GetObjectItem( cCurrentModuleObject, Loop, MaterialNames, MaterialNumAlpha, MaterialProps, MaterialNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				//Load the material derived type from the input data.
				MaterNum = FindItemInList( MaterialNames( 1 ), Material );
				if ( MaterNum == 0 ) {
					ShowSevereError( cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 1 ) + " entered=" + MaterialNames( 1 ) + ", must match to a valid Material name." );
					ErrorsFound = true;
					continue;
				}

				if ( Material( MaterNum ).Group != RegularMaterial ) {
					ShowSevereError( cCurrentModuleObject + ": Reference Material is not appropriate type for CondFD properties, material=" + Material( MaterNum ).Name + ", must have regular properties (L,Cp,K,D)" );
					ErrorsFound = true;
				}

				// Once the material derived type number is found then load the additional CondFD variable material properties
				//   Some or all may be zero (default).  They will be checked when calculating node temperatures
				MaterialFD( MaterNum ).numTempCond = MaterialNumProp / 2;
				if ( MaterialFD( MaterNum ).numTempCond * 2 != MaterialNumProp ) {
					ShowSevereError( "GetCondFDInput: " + cCurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", mismatched pairs" );
					ShowContinueError( "...expected " + RoundSigDigits( MaterialFD( MaterNum ).numTempCond ) + " pairs, but only entered " + RoundSigDigits( MaterialNumProp ) + " numbers." );
					ErrorsFound = true;
				}
				MaterialFD( MaterNum ).TempCond.dimension( 2, MaterialFD( MaterNum ).numTempCond, 0.0 );
				propNum = 1;
				// Temperature first
				for ( int pcount = 1, pcount_end = MaterialFD( MaterNum ).numTempCond; pcount <= pcount_end; ++pcount ) {
					MaterialFD( MaterNum ).TempCond( 1, pcount ) = MaterialProps( propNum );
					propNum += 2;
				}
				propNum = 2;
				// Then Conductivity
				for ( int pcount = 1, pcount_end = MaterialFD( MaterNum ).numTempCond; pcount <= pcount_end; ++pcount ) {
					MaterialFD( MaterNum ).TempCond( 2, pcount ) = MaterialProps( propNum );
					propNum += 2;
				}
				nonInc = false;
				inegptr = 0;
				for ( int pcount = 1, pcount_end = MaterialFD( MaterNum ).numTempCond - 1; pcount <= pcount_end; ++pcount ) {
					if ( MaterialFD( MaterNum ).TempCond( 1, pcount ) < MaterialFD( MaterNum ).TempCond( 1, pcount + 1 ) ) continue;
					nonInc = true;
					inegptr = pcount + 1;
					break;
				}
				if ( nonInc ) {
					ShowSevereError( "GetCondFDInput: " + cCurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", non increasing Temperatures. Temperatures must be strictly increasing." );
					ShowContinueError( "...occurs first at item=[" + RoundSigDigits( inegptr ) + "], value=[" + RoundSigDigits( MaterialFD( MaterNum ).TempCond( 1, inegptr ), 2 ) + "]." );
					ErrorsFound = true;
				}
			}
		}

		for ( MaterNum = 1; MaterNum <= TotMaterials; ++MaterNum ) {
			if ( MaterialFD( MaterNum ).numTempEnth == 0 ) {
				MaterialFD( MaterNum ).numTempEnth = 3;
				MaterialFD( MaterNum ).TempEnth.dimension( 2, 3, -100.0 );
			}
			if ( MaterialFD( MaterNum ).numTempCond == 0 ) {
				MaterialFD( MaterNum ).numTempCond = 3;
				MaterialFD( MaterNum ).TempCond.dimension( 2, 3, -100.0 );
			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( "GetCondFDInput: Errors found getting ConductionFiniteDifference properties. Program terminates." );
		}

		InitialInitHeatBalFiniteDiff();

	}

	void
	InitHeatBalFiniteDiff()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard J. Liesen
		//       DATE WRITTEN   Oct 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  C O Pedersen 2006
		//                      B. Griffith May 2011 move begin-environment and every-timestep inits, cleanup formatting

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine sets the initial values for the FD moisture calculation

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using DataSurfaces::HeatTransferModel_CondFD;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyEnvrnFlag( true );
		int SurfNum;
		int ConstrNum; // Loop counter
		bool ErrorsFound;

		if ( GetHBFiniteDiffInputFlag ) {
			// Obtains conduction FD related parameters from input file
			GetCondFDInput();
			GetHBFiniteDiffInputFlag = false;
		}

		ErrorsFound = false;

		// now do begin environment inits.
		if ( BeginEnvrnFlag && MyEnvrnFlag ) {
			for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
				if ( Surface( SurfNum ).HeatTransferAlgorithm != HeatTransferModel_CondFD ) continue;
				if ( Surface( SurfNum ).Construction <= 0 ) continue; // Shading surface, not really a heat transfer surface
				ConstrNum = Surface( SurfNum ).Construction;
				if ( Construct( ConstrNum ).TypeIsWindow ) continue; //  Windows simulated in Window module
				SurfaceFD( SurfNum ).T = TempInitValue;
				SurfaceFD( SurfNum ).TOld = TempInitValue;
				SurfaceFD( SurfNum ).TT = TempInitValue;
				SurfaceFD( SurfNum ).Rhov = RhovInitValue;
				SurfaceFD( SurfNum ).RhovOld = RhovInitValue;
				SurfaceFD( SurfNum ).RhoT = RhovInitValue;
				SurfaceFD( SurfNum ).TD = TempInitValue;
				SurfaceFD( SurfNum ).TDT = TempInitValue;
				SurfaceFD( SurfNum ).TDTLast = TempInitValue;
				SurfaceFD( SurfNum ).TDOld = TempInitValue;
				SurfaceFD( SurfNum ).TDreport = TempInitValue;
				SurfaceFD( SurfNum ).RH = 0.0;
				SurfaceFD( SurfNum ).RHreport = 0.0;
				SurfaceFD( SurfNum ).EnthOld = EnthInitValue;
				SurfaceFD( SurfNum ).EnthNew = EnthInitValue;
				SurfaceFD( SurfNum ).EnthLast = EnthInitValue;
				SurfaceFD( SurfNum ).QDreport = 0.0;
				SurfaceFD( SurfNum ).CpDelXRhoS1 = 0.0;
				SurfaceFD( SurfNum ).CpDelXRhoS2 = 0.0;
				SurfaceFD( SurfNum ).TDpriortimestep = 0.0;

				TempOutsideAirFD( SurfNum ) = 0.0;
				RhoVaporAirOut( SurfNum ) = 0.0;
				RhoVaporSurfIn( SurfNum ) = 0.0;
				RhoVaporAirIn( SurfNum ) = 0.0;
				HConvExtFD( SurfNum ) = 0.0;
				HMassConvExtFD( SurfNum ) = 0.0;
				HConvInFD( SurfNum ) = 0.0;
				HMassConvInFD( SurfNum ) = 0.0;
				HSkyFD( SurfNum ) = 0.0;
				HGrndFD( SurfNum ) = 0.0;
				HAirFD( SurfNum ) = 0.0;
			}
			WarmupSurfTemp = 0;
			MyEnvrnFlag = false;
		}
		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag = true;
		}

		// now do every timestep inits

		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			if ( Surface( SurfNum ).HeatTransferAlgorithm != HeatTransferModel_CondFD ) continue;
			if ( Surface( SurfNum ).Construction <= 0 ) continue; // Shading surface, not really a heat transfer surface
			ConstrNum = Surface( SurfNum ).Construction;
			if ( Construct( ConstrNum ).TypeIsWindow ) continue; //  Windows simulated in Window module
			SurfaceFD( SurfNum ).T = SurfaceFD( SurfNum ).TOld;
			SurfaceFD( SurfNum ).Rhov = SurfaceFD( SurfNum ).RhovOld;
			SurfaceFD( SurfNum ).TD = SurfaceFD( SurfNum ).TDOld;
			SurfaceFD( SurfNum ).TDT = SurfaceFD( SurfNum ).TDreport; //PT changes from TDold to TDreport
			SurfaceFD( SurfNum ).TDTLast = SurfaceFD( SurfNum ).TDOld;
			SurfaceFD( SurfNum ).EnthOld = SurfaceFD( SurfNum ).EnthOld;
			SurfaceFD( SurfNum ).EnthNew = SurfaceFD( SurfNum ).EnthOld;
			SurfaceFD( SurfNum ).EnthLast = SurfaceFD( SurfNum ).EnthOld;
			SurfaceFD( SurfNum ).TDpriortimestep = SurfaceFD( SurfNum ).TDreport; // Save TD for heat flux calc

		}

	}

	void
	InitialInitHeatBalFiniteDiff()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   March 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine performs the original allocate, inits and setup output variables for the
		// module.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using DataSurfaces::HeatTransferModel_CondFD;
		using DataHeatBalance::HighDiffusivityThreshold;
		using DataHeatBalance::ThinMaterialLayerThreshold;
		using DataGlobals::DisplayAdvancedReportVariables;

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
		int Lay;
		int SurfNum;

		Real64 dxn; // Intermediate calculation of nodal spacing. This is the full dx. There is
		// a half dxn thick node at each surface. dxn is the "capacitor" spacing.
		int Ipts1; // Intermediate calculation for number of full thickness nodes per layer. There
		// are always two half nodes at the layer faces.
		int Layer; // Loop counter
		int OutwardMatLayerNum; // layer index, layer outward of the current layer
		int LayerNode;
		int Delt;
		int ConstrNum; // Loop counter
		int TotNodes; // Loop counter
		int CurrentLayer; // Loop counter
		int Surf; // Loop counter
		int index; // Loop Counters

		Real64 Alpha;
		Real64 mAlpha;
		Real64 StabilityTemp;
		Real64 StabilityMoist;
		Real64 a;
		Real64 b;
		Real64 c;
		Real64 d;
		Real64 kt;
		Real64 RhoS;
		Real64 Por;
		Real64 Cp;
		Real64 Dv;
		bool ErrorsFound;
		Real64 DeltaTimestep; // zone timestep in seconds, for local check of properties
		Real64 ThicknessThreshold; // min thickness consistent with other thermal properties, for local check

		ConstructFD.allocate( TotConstructs );
		SigmaR.allocate( TotConstructs );
		SigmaC.allocate( TotConstructs );

		SurfaceFD.allocate( TotSurfaces );
		QHeatInFlux.allocate( TotSurfaces );
		QHeatOutFlux.allocate( TotSurfaces );
		//  ALLOCATE(QFluxInArrivSurfCond(TotSurfaces))
		//  ALLOCATE(QFluxOutArrivSurfCond(TotSurfaces))
		//  ALLOCATE(OpaqSurfInsFaceConductionFlux(TotSurfaces))
		//  ALLOCATE(OpaqSurfOutsideFaceConductionFlux(TotSurfaces))
		//  ALLOCATE(QFluxZoneToInSurf(TotSurfaces))
		//  ALLOCATE(QFluxOutsideToOutSurf(TotSurfaces))

		// And then initialize
		QHeatInFlux = 0.0;
		QHeatOutFlux = 0.0;
		//QFluxZoneToInSurf = 0.0;
		//QFluxOutsideToOutSurf = 0.0;
		//QFluxInArrivSurfCond = 0.0;
		//QFluxOutArrivSurfCond = 0.0;
		OpaqSurfInsFaceConductionFlux = 0.0;
		OpaqSurfOutsideFaceConductionFlux = 0.0;

		// Setup Output Variables

		//  set a Delt that fits the zone time step and keeps it below 200s.

		fracTimeStepZone_Hour = 1.0 / double( NumOfTimeStepInHour );

		for ( index = 1; index <= 20; ++index ) {
			Delt = ( fracTimeStepZone_Hour * SecInHour ) / index; // TimeStepZone = Zone time step in fractional hours
			if ( Delt <= 200 ) break;
		}

		for ( ConstrNum = 1; ConstrNum <= TotConstructs; ++ConstrNum ) {
			//Need to skip window constructions and eventually window materials
			if ( Construct( ConstrNum ).TypeIsWindow ) continue;

			ConstructFD( ConstrNum ).Name.allocate( Construct( ConstrNum ).TotLayers );
			ConstructFD( ConstrNum ).Thickness.allocate( Construct( ConstrNum ).TotLayers );
			ConstructFD( ConstrNum ).NodeNumPoint.allocate( Construct( ConstrNum ).TotLayers );
			ConstructFD( ConstrNum ).DelX.allocate( Construct( ConstrNum ).TotLayers );
			ConstructFD( ConstrNum ).TempStability.allocate( Construct( ConstrNum ).TotLayers );
			ConstructFD( ConstrNum ).MoistStability.allocate( Construct( ConstrNum ).TotLayers );
			// Node Number of the Interface node following each layer
			//    ALLOCATE(ConstructFD(ConstrNum)%InterfaceNodeNums(Construct(ConstrNum)%TotLayers))

			TotNodes = 0;
			SigmaR( ConstrNum ) = 0.0;
			SigmaC( ConstrNum ) = 0.0;

			for ( Layer = 1; Layer <= Construct( ConstrNum ).TotLayers; ++Layer ) { // Begin layer loop ...

				// Loop through all of the layers in the current construct. The purpose
				// of this loop is to define the thermal properties  and to.
				// determine the total number of full size nodes in each layer.
				// The number of temperature points is one more than this
				// because of the two half nodes at the layer faces.
				// The calculation of dxn used here is based on a standard stability
				// criteria for explicit finite difference solutions.  This criteria
				// was chosen not because it is viewed to be correct, but rather for
				// lack of any better criteria at this time.  The use of a Fourier
				// number based criteria such as this is probably physically correct.
				//  Change to implicit formulation still uses explicit stability, but
				// now there are special equations for R-only layers.

				CurrentLayer = Construct( ConstrNum ).LayerPoint( Layer );

				ConstructFD( ConstrNum ).Name( Layer ) = Material( CurrentLayer ).Name;
				ConstructFD( ConstrNum ).Thickness( Layer ) = Material( CurrentLayer ).Thickness;

				// Do some quick error checks for this section.

				if ( Material( CurrentLayer ).ROnly ) { // Rlayer

					//  These values are only needed temporarily and to calculate flux,
					//   Layer will be handled
					//  as a pure R in the temperature calc.
					// assign other properties based on resistance

					Material( CurrentLayer ).SpecHeat = 0.0001;
					Material( CurrentLayer ).Density = 1.0;
					Material( CurrentLayer ).Thickness = 0.1; //  arbitrary thickness for R layer
					Material( CurrentLayer ).Conductivity = Material( CurrentLayer ).Thickness / Material( CurrentLayer ).Resistance;
					kt = Material( CurrentLayer ).Conductivity;
					ConstructFD( ConstrNum ).Thickness( Layer ) = Material( CurrentLayer ).Thickness;

					SigmaR( ConstrNum ) += Material( CurrentLayer ).Resistance; // add resistance of R layer
					SigmaC( ConstrNum ) += 0.0; //  no capacitance for R layer

					Alpha = kt / ( Material( CurrentLayer ).Density * Material( CurrentLayer ).SpecHeat );

					mAlpha = 0.0;

				} else if ( Material( CurrentLayer ).Group == 1 ) { //  Group 1 = Air

					//  Again, these values are only needed temporarily and to calculate flux,
					//   Air layer will be handled
					//  as a pure R in the temperature calc.
					// assign
					// other properties based on resistance

					Material( CurrentLayer ).SpecHeat = 0.0001;
					Material( CurrentLayer ).Density = 1.0;
					Material( CurrentLayer ).Thickness = 0.1; //  arbitrary thickness for R layer
					Material( CurrentLayer ).Conductivity = Material( CurrentLayer ).Thickness / Material( CurrentLayer ).Resistance;
					kt = Material( CurrentLayer ).Conductivity;
					ConstructFD( ConstrNum ).Thickness( Layer ) = Material( CurrentLayer ).Thickness;

					SigmaR( ConstrNum ) += Material( CurrentLayer ).Resistance; // add resistance of R layer
					SigmaC( ConstrNum ) += 0.0; //  no capacitance for R layer

					Alpha = kt / ( Material( CurrentLayer ).Density * Material( CurrentLayer ).SpecHeat );
					mAlpha = 0.0;
				} else if ( Construct( ConstrNum ).TypeIsIRT ) { // make similar to air? (that didn't seem to work well)
					ShowSevereError( "InitHeatBalFiniteDiff: Construction =\"" + Construct( ConstrNum ).Name + "\" uses Material:InfraredTransparent. Cannot be used currently with finite difference calculations." );
					if ( Construct( ConstrNum ).IsUsed ) {
						ShowContinueError( "...since this construction is used in a surface, the simulation is not allowed." );
						ErrorsFound = true;
					} else {
						ShowContinueError( "...if this construction were used in a surface, the simulation would be terminated." );
					}
					continue;
					//            ! set properties to get past other initializations.
					//          Material(CurrentLayer)%SpecHeat  = 0.0001d0
					//          Material(CurrentLayer)%Density = 0.0001d0
					//          Material(CurrentLayer)%Thickness = 0.1d0  !  arbitrary thickness for R layer
					//          Material(CurrentLayer)%Conductivity= &
					//    -      Material(CurrentLayer)%Thickness/Material(CurrentLayer)%Resistance
					//          kt = Material(CurrentLayer)%Conductivity
					//          ConstructFD(ConstrNum)%Thickness(Layer) = Material(CurrentLayer)%Thickness
					//          SigmaR(ConstrNum) = SigmaR(ConstrNum) + Material(CurrentLayer)%Resistance  ! add resistance of R layer
					//          SigmaC(ConstrNum) = SigmaC(ConstrNum)  + 0.0                               !  no capacitance for R layer
					//          Alpha = kt/(Material(CurrentLayer)%Density*Material(CurrentLayer)%SpecHeat)
					//          mAlpha = 0.0d0
				} else {
					//    Regular material Properties
					a = Material( CurrentLayer ).MoistACoeff;
					b = Material( CurrentLayer ).MoistBCoeff;
					c = Material( CurrentLayer ).MoistCCoeff;
					d = Material( CurrentLayer ).MoistDCoeff;
					kt = Material( CurrentLayer ).Conductivity;
					RhoS = Material( CurrentLayer ).Density;
					Por = Material( CurrentLayer ).Porosity;
					Cp = Material( CurrentLayer ).SpecHeat;
					// Need Resistance for reg layer
					Material( CurrentLayer ).Resistance = Material( CurrentLayer ).Thickness / Material( CurrentLayer ).Conductivity;
					Dv = Material( CurrentLayer ).VaporDiffus;
					SigmaR( ConstrNum ) += Material( CurrentLayer ).Resistance; // add resistance
					SigmaC( ConstrNum ) += Material( CurrentLayer ).Density * Material( CurrentLayer ).SpecHeat * Material( CurrentLayer ).Thickness;
					Alpha = kt / ( RhoS * Cp );
					//   Alpha = kt*(Por+At*RhoS)/(RhoS*(Bv*Por*Lambda+Cp*(Por+At*RhoS)))
					mAlpha = 0.0;

					//check for Material layers that are too thin and highly conductivity (not appropriate for surface models)
					if ( Alpha > HighDiffusivityThreshold && ! Material( CurrentLayer ).WarnedForHighDiffusivity ) {
						DeltaTimestep = TimeStepZoneSec;
						ThicknessThreshold = std::sqrt( Alpha * DeltaTimestep * 3.0 );
						if ( Material( CurrentLayer ).Thickness < ThicknessThreshold ) {
							ShowSevereError( "InitialInitHeatBalFiniteDiff: Found Material that is too thin and/or too highly conductive, material name = " + Material( CurrentLayer ).Name );
							ShowContinueError( "High conductivity Material layers are not well supported by Conduction Finite Difference, material conductivity = " + RoundSigDigits( Material( CurrentLayer ).Conductivity, 3 ) + " [W/m-K]" );
							ShowContinueError( "Material thermal diffusivity = " + RoundSigDigits( Alpha, 3 ) + " [m2/s]" );
							ShowContinueError( "Material with this thermal diffusivity should have thickness > " + RoundSigDigits( ThicknessThreshold, 5 ) + " [m]" );
							if ( Material( CurrentLayer ).Thickness < ThinMaterialLayerThreshold ) {
								ShowContinueError( "Material may be too thin to be modeled well, thickness = " + RoundSigDigits( Material( CurrentLayer ).Thickness, 5 ) + " [m]" );
								ShowContinueError( "Material with this thermal diffusivity should have thickness > " + RoundSigDigits( ThinMaterialLayerThreshold, 5 ) + " [m]" );
							}
							Material( CurrentLayer ).WarnedForHighDiffusivity = true;
						}
					}

				} //  R, Air  or regular material properties and parameters

				// Proceed with setting node sizes in layers

				dxn = std::sqrt( Alpha * Delt * SpaceDescritConstant ); // The Fourier number is set using user constant

				// number of nodes=thickness/spacing.  This is number of full size node spaces across layer.
				Ipts1 = int( Material( CurrentLayer ).Thickness / dxn );
				//  set high conductivity layers to a single full size node thickness. (two half nodes)
				if ( Ipts1 <= 1 ) Ipts1 = 1;
				if ( Material( CurrentLayer ).ROnly || Material( CurrentLayer ).Group == 1 ) {

					Ipts1 = 1; //  single full node in R layers- surfaces of adjacent material or inside/outside layer
				}

				dxn = Material( CurrentLayer ).Thickness / double( Ipts1 ); // full node thickness

				StabilityTemp = Alpha * Delt / pow_2( dxn );
				StabilityMoist = mAlpha * Delt / pow_2( dxn );
				ConstructFD( ConstrNum ).TempStability( Layer ) = StabilityTemp;
				ConstructFD( ConstrNum ).MoistStability( Layer ) = StabilityMoist;
				ConstructFD( ConstrNum ).DelX( Layer ) = dxn;

				TotNodes += Ipts1; //  number of full size nodes
				ConstructFD( ConstrNum ).NodeNumPoint( Layer ) = Ipts1; //  number of full size nodes
			} //  end of layer loop.

			ConstructFD( ConstrNum ).TotNodes = TotNodes;
			ConstructFD( ConstrNum ).DeltaTime = Delt;

		} // End of Construction Loop.  TotNodes in each construction now set

		// now determine x location, or distance that nodes are from the outside face in meters
		for ( ConstrNum = 1; ConstrNum <= TotConstructs; ++ConstrNum ) {
			if ( ConstructFD( ConstrNum ).TotNodes > 0 ) {
				ConstructFD( ConstrNum ).NodeXlocation.allocate( ConstructFD( ConstrNum ).TotNodes + 1 );
				ConstructFD( ConstrNum ).NodeXlocation = 0.0; // init them all
				Ipts1 = 0; // init counter
				for ( Layer = 1; Layer <= Construct( ConstrNum ).TotLayers; ++Layer ) {
					OutwardMatLayerNum = Layer - 1;
					for ( LayerNode = 1; LayerNode <= ConstructFD( ConstrNum ).NodeNumPoint( Layer ); ++LayerNode ) {
						++Ipts1;
						if ( Ipts1 == 1 ) {
							ConstructFD( ConstrNum ).NodeXlocation( Ipts1 ) = 0.0; // first node is on outside face

						} else if ( LayerNode == 1 ) {
							if ( OutwardMatLayerNum > 0 && OutwardMatLayerNum <= Construct( ConstrNum ).TotLayers ) {
								// later nodes are Delx away from previous, but use Delx from previous layer
								ConstructFD( ConstrNum ).NodeXlocation( Ipts1 ) = ConstructFD( ConstrNum ).NodeXlocation( Ipts1 - 1 ) + ConstructFD( ConstrNum ).DelX( OutwardMatLayerNum );
							}
						} else {
							// later nodes are Delx away from previous
							ConstructFD( ConstrNum ).NodeXlocation( Ipts1 ) = ConstructFD( ConstrNum ).NodeXlocation( Ipts1 - 1 ) + ConstructFD( ConstrNum ).DelX( Layer );
						}

					}
				}
				Layer = Construct( ConstrNum ).TotLayers;
				++Ipts1;
				ConstructFD( ConstrNum ).NodeXlocation( Ipts1 ) = ConstructFD( ConstrNum ).NodeXlocation( Ipts1 - 1 ) + ConstructFD( ConstrNum ).DelX( Layer );
			}
		}

		for ( Surf = 1; Surf <= TotSurfaces; ++Surf ) {
			if ( ! Surface( Surf ).HeatTransSurf ) continue;
			if ( Surface( Surf ).Class == SurfaceClass_Window ) continue;
			if ( Surface( Surf ).HeatTransferAlgorithm != HeatTransferModel_CondFD ) continue;
			//    IF(Surface(Surf)%Construction <= 0) CYCLE  ! Shading surface, not really a heat transfer surface
			ConstrNum = Surface( Surf ).Construction;
			//    IF(Construct(ConstrNum)%TypeIsWindow) CYCLE  !  Windows simulated in Window module
			TotNodes = ConstructFD( ConstrNum ).TotNodes;

			//Allocate the Surface Arrays
			SurfaceFD( Surf ).T.allocate( TotNodes + 1 );
			SurfaceFD( Surf ).TOld.allocate( TotNodes + 1 );
			SurfaceFD( Surf ).TT.allocate( TotNodes + 1 );
			SurfaceFD( Surf ).Rhov.allocate( TotNodes + 1 );
			SurfaceFD( Surf ).RhovOld.allocate( TotNodes + 1 );
			SurfaceFD( Surf ).RhoT.allocate( TotNodes + 1 );
			SurfaceFD( Surf ).TD.allocate( TotNodes + 1 );
			SurfaceFD( Surf ).TDT.allocate( TotNodes + 1 );
			SurfaceFD( Surf ).TDTLast.allocate( TotNodes + 1 );
			SurfaceFD( Surf ).TDOld.allocate( TotNodes + 1 );
			SurfaceFD( Surf ).TDreport.allocate( TotNodes + 1 );
			SurfaceFD( Surf ).RH.allocate( TotNodes + 1 );
			SurfaceFD( Surf ).RHreport.allocate( TotNodes + 1 );
			SurfaceFD( Surf ).EnthOld.allocate( TotNodes + 1 );
			SurfaceFD( Surf ).EnthNew.allocate( TotNodes + 1 );
			SurfaceFD( Surf ).EnthLast.allocate( TotNodes + 1 );
			SurfaceFD( Surf ).QDreport.allocate( TotNodes + 1 );
			SurfaceFD( Surf ).CpDelXRhoS1.allocate( TotNodes + 1 );
			SurfaceFD( Surf ).CpDelXRhoS2.allocate( TotNodes + 1 );
			SurfaceFD( Surf ).TDpriortimestep.allocate( TotNodes + 1 );

			//Initialize the allocated arrays.
			SurfaceFD( Surf ).T = TempInitValue;
			SurfaceFD( Surf ).TOld = TempInitValue;
			SurfaceFD( Surf ).TT = TempInitValue;
			SurfaceFD( Surf ).Rhov = RhovInitValue;
			SurfaceFD( Surf ).RhovOld = RhovInitValue;
			SurfaceFD( Surf ).RhoT = RhovInitValue;
			SurfaceFD( Surf ).TD = TempInitValue;
			SurfaceFD( Surf ).TDT = TempInitValue;
			SurfaceFD( Surf ).TDTLast = TempInitValue;
			SurfaceFD( Surf ).TDOld = TempInitValue;
			SurfaceFD( Surf ).TDreport = TempInitValue;
			SurfaceFD( Surf ).RH = 0.0;
			SurfaceFD( Surf ).RHreport = 0.0;
			SurfaceFD( Surf ).EnthOld = EnthInitValue;
			SurfaceFD( Surf ).EnthNew = EnthInitValue;
			SurfaceFD( Surf ).EnthLast = EnthInitValue;
			SurfaceFD( Surf ).QDreport = 0.0;
			SurfaceFD( Surf ).CpDelXRhoS1 = 0.0;
			SurfaceFD( Surf ).CpDelXRhoS2 = 0.0;
			SurfaceFD( Surf ).TDpriortimestep = 0.0;
		}

		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
			if ( Surface( SurfNum ).Class == SurfaceClass_Window ) continue;
			if ( Surface( SurfNum ).HeatTransferAlgorithm != HeatTransferModel_CondFD ) continue;
			//   If(SolutionAlgo == UseCondFD .or. SolutionAlgo == UseCondFDSimple)Then
			//    CALL SetupOutputVariable('CondFD Outside Surface Heat Flux [W/m2]',   QFluxOutArrivSurfCond(SurfNum), &
			//                             'Zone','State',TRIM(Surface(SurfNum)%Name))
			//    CALL SetupOutputVariable('CondFD Inside Surface Heat Flux [W/m2]',    QFluxInArrivSurfCond(SurfNum), &
			//                             'Zone','State',TRIM(Surface(SurfNum)%Name))
			//    CALL SetupOutputVariable('CondFD Outside Heat Flux to Surface [W/m2]',QFluxOutsideToOutSurf(SurfNum), &
			//                             'Zone','State',TRIM(Surface(SurfNum)%Name))
			//    CALL SetupOutputVariable('CondFD Inside Heat Flux to Surface [W/m2]', QFluxZoneToInSurf(SurfNum), &
			//                             'Zone','State',TRIM(Surface(SurfNum)%Name))

			SetupOutputVariable( "CondFD Inner Solver Loop Iteration Count [ ]", SurfaceFD( SurfNum ).GSloopCounter, "Zone", "Sum", Surface( SurfNum ).Name );

			TotNodes = ConstructFD( Surface( SurfNum ).Construction ).TotNodes; // Full size nodes, start with outside face.
			for ( Lay = 1; Lay <= TotNodes + 1; ++Lay ) { // include inside face node
				SetupOutputVariable( "CondFD Surface Temperature Node " + TrimSigDigits( Lay ) + " [C]", SurfaceFD( SurfNum ).TDreport( Lay ), "Zone", "State", Surface( SurfNum ).Name );
				SetupOutputVariable( "CondFD Surface Heat Flux Node " + TrimSigDigits( Lay ) + " [W/m2]", SurfaceFD( SurfNum ).QDreport( Lay ), "Zone", "State", Surface( SurfNum ).Name );
				if ( DisplayAdvancedReportVariables ) {
					SetupOutputVariable( "CondFD Surface Heat Capacitance Outer Half Node " + TrimSigDigits( Lay ) + " [W/m2-K]", SurfaceFD( SurfNum ).CpDelXRhoS1( Lay ), "Zone", "State", Surface( SurfNum ).Name );
					SetupOutputVariable( "CondFD Surface Heat Capacitance Inner Half Node " + TrimSigDigits( Lay ) + " [W/m2-K]", SurfaceFD( SurfNum ).CpDelXRhoS2( Lay ), "Zone", "State", Surface( SurfNum ).Name );
				}
			}

		} // End of the Surface Loop for Report Variable Setup

		ReportFiniteDiffInits(); // Report the results from the Finite Diff Inits

	}

	void
	relax_array(
		Array1< Real64 > & a, // Array to relax
		Array1< Real64 > const & b, // Array to relax towards
		Real64 const r // Relaxation factor [0-1]
	)
	{
		assert( equal_dimensions( a, b ) );
		assert( ( 0.0 <= r ) && ( r <= 1.0 ) );
		Real64 const q( 1.0 - r );
		for ( int i = a.l(), e = a.u(); i <= e; ++i ) {
			a( i ) = r * b( i ) + q * a( i );
		}
	}

	Real64
	sum_array_diff(
		Array1< Real64 > const & a,
		Array1< Real64 > const & b
	)
	{
		assert( equal_dimensions( a, b ) );
		Real64 s( 0.0 );
		for ( int i = a.l(), e = a.u(); i <= e; ++i ) {
			s += a( i ) - b( i ); //? Should this be in abs?
		}
		return s;
	}

	void
	CalcHeatBalFiniteDiff(
		int const Surf, // Surface number
		Real64 & TempSurfInTmp, // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		Real64 & TempSurfOutTmp // Outside Surface Temperature of each Heat Transfer Surface
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard J. Liesen
		//       DATE WRITTEN   Oct 2003
		//       MODIFIED       Aug 2006 by C O Pedersen to include implicit solution and variable properties with
		//                                material enthalpy added for Phase Change Materials.
		//                      Sept 2010 B. Griffith, remove allocate/deallocate, use structure variables
		//                      March 2011 P. Tabares, add relaxation factor and add surfIteration to
		//                                 update TD and TDT, correct interzone partition
		//                      May 2011  B. Griffith add logging and errors when inner GS loop does not converge
		//                      November 2011 P. Tabares fixed problems with adiabatic walls/massless walls and PCM stability problems

		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// this routine controls the calculation of the fluxes and temperatures using
		//      finite difference procedures for
		//      all building surface constructs.

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
//		using General::RoundSigDigits;
		using DataHeatBalance::CondFDRelaxFactor;
//		using DataGlobals::KickOffSimulation;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		static Real64 MaxDelTemp( 0.0 );

		int const ConstrNum( Surface( Surf ).Construction );

		int const TotNodes( ConstructFD( ConstrNum ).TotNodes );
		int const TotLayers( Construct( ConstrNum ).TotLayers );

		TempSurfInTmp = 0.0;
		TempSurfOutTmp = 0.0;

		int const Delt( ConstructFD( ConstrNum ).DeltaTime ); //   (seconds)

		// Aliases
		auto & surfaceFD( SurfaceFD( Surf ) );
		auto const & T( surfaceFD.T );
		auto & TT( surfaceFD.TT );
		auto const & Rhov( surfaceFD.Rhov );
		auto & RhoT( surfaceFD.RhoT );
		auto const & TD( surfaceFD.TD );
		auto & TDT( surfaceFD.TDT );
		auto & TDTLast( surfaceFD.TDTLast );
		auto & TDreport( surfaceFD.TDreport );
		auto & RH( surfaceFD.RH );
		auto & EnthOld( surfaceFD.EnthOld );
		auto & EnthNew( surfaceFD.EnthNew );
		auto & EnthLast( surfaceFD.EnthLast );
		auto & GSloopCounter( surfaceFD.GSloopCounter );
		auto & MaxNodeDelTemp( surfaceFD.MaxNodeDelTemp );

		Real64 HMovInsul; // Equiv H for TIM layer,  Comes with call to
		int RoughIndexMovInsul; // roughness  Movable insulation
		Real64 AbsExt; // exterior absorptivity  movable insulation
		EvalOutsideMovableInsulation( Surf, HMovInsul, RoughIndexMovInsul, AbsExt );
		// Start stepping through the slab with time.
		for ( int J = 1, J_end = nint( TimeStepZoneSec / Delt ); J <= J_end; ++J ) { //PT testing higher time steps

			int GSiter; // iteration counter for implicit repeat calculation
			for ( GSiter = 1; GSiter <= MaxGSiter; ++GSiter ) { //  Iterate implicit equations
				TDTLast = TDT; // Save last iteration's TDT (New temperature) values
				EnthLast = EnthNew; // Last iterations new enthalpy value

				// Original loop version
				int i( 1 ); //  Node counter
				for ( int Lay = 1; Lay <= TotLayers; ++Lay ) { // Begin layer loop ...

					// For the exterior surface node with a convective boundary condition
					if ( ( i == 1 ) && ( Lay == 1 ) ) {
						ExteriorBCEqns( Delt, i, Lay, Surf, T, TT, Rhov, RhoT, RH, TD, TDT, EnthOld, EnthNew, TotNodes, HMovInsul );
					}

					// For the Layer Interior nodes.  Arrive here after exterior surface node or interface node

					if ( TotNodes != 1 ) {
						for ( int ctr = 2, ctr_end = ConstructFD( ConstrNum ).NodeNumPoint( Lay ); ctr <= ctr_end; ++ctr ) {
							++i;
							InteriorNodeEqns( Delt, i, Lay, Surf, T, TT, Rhov, RhoT, RH, TD, TDT, EnthOld, EnthNew );
						}
					}

					if ( ( Lay < TotLayers ) && ( TotNodes != 1 ) ) { // Interface equations for 2 capactive materials
						++i;
						IntInterfaceNodeEqns( Delt, i, Lay, Surf, T, TT, Rhov, RhoT, RH, TD, TDT, EnthOld, EnthNew, GSiter );
					} else if ( Lay == TotLayers ) { // For the Interior surface node with a convective boundary condition
						++i;
						InteriorBCEqns( Delt, i, Lay, Surf, T, TT, Rhov, RhoT, RH, TD, TDT, EnthOld, EnthNew, TDreport );
					}

				} // layer loop

				// Apply Relaxation factor for stability, use current (TDT) and previous (TDTLast) iteration temperature values
				// to obtain the actual temperature that is going to be used for next iteration. This would mostly happen with PCM
				//Tuned Function call to eliminate array temporaries and multiple relaxation passes
				if ( GSiter > 15 ) {
					relax_array( TDT, TDTLast, 0.9875 );
				} else if ( GSiter > 10 ) {
					relax_array( TDT, TDTLast, 0.875 );
				} else if ( GSiter > 5 ) {
					relax_array( TDT, TDTLast, 0.5 );
				}

				// the following could blow up when all the node temps sum to less than 1.0.  seems poorly formulated for temperature in C.
				//PT delete one zero and decrese number of minimum iterations, from 3 (which actually requires 4 iterations) to 2.

				if ( ( GSiter > 2 ) && ( std::abs( sum_array_diff( TDT, TDTLast ) / sum( TDT ) ) < 0.00001 ) ) break;
				//SurfaceFD(Surf)%GSloopCounter = Gsiter  !PT moved out of GSloop so it can actually count all iterations

				//feb2012 the following could blow up when all the node temps sum to less than 1.0.  seems poorly formulated for temperature in C.
				//feb2012      IF (Gsiter .gt. 3  .and.ABS(SUM(SurfaceFD(Surf)%TDT-SurfaceFD(Surf)%TDTLast)/SUM(SurfaceFD(Surf)%TDT)) < 0.000001d0 )  EXIT
				//feb2012      SurfaceFD(Surf)%GSloopCounter = Gsiter
				//      IF ((GSiter == MaxGSiter) .AND. (SolutionAlgo /= UseCondFDSimple)) THEN ! didn't ever converge
				//        IF (.NOT. WarmupFlag .AND. (.NOT. KickOffSimulation)) THEN
				//          ErrCount=ErrCount+1
				//          ErrorSignal = ABS(SUM(SurfaceFD(Surf)%TDT-SurfaceFD(Surf)%TDTLast)/SUM(SurfaceFD(Surf)%TDT))
				//          IF (ErrCount < 10) THEN
				//            CALL ShowWarningError('ConductionFiniteDifference inner iteration loop did not converge for surface named ='// &
				//                          TRIM(Surface(Surf)%Name) // &
				//                          ', with error signal ='//TRIM(RoundSigDigits(ErrorSignal, 8)) // &
				//                          ' vs criteria of 0.000001')
				//            CALL ShowContinueErrorTimeStamp(' ')
				//          ELSE
				//            CALL ShowRecurringWarningErrorAtEnd('ConductionFiniteDifference convergence problem continues for surface named ='// &
				//                                                TRIM(Surface(Surf)%Name) , &
				//                                               SurfaceFD(Surf)%GSloopErrorCount,ReportMaxOf=ErrorSignal,ReportMinOf=ErrorSignal,  &
				//                                               ReportMaxUnits='[ ]',ReportMinUnits='[ ]')
				//          ENDIF
				//        ENDIF
				//      ENDIF

			} // End of Gauss Seidell iteration loop

			GSloopCounter = GSiter; // outputs GSloop iterations, useful for pinpointing stability issues with condFD
			if ( CondFDRelaxFactor != 1.0 ) {
				// Apply Relaxation factor for stability, use current (TDT) and previous (TDreport) temperature values
				//   to obtain the actual temperature that is going to be exported/use
				relax_array( TDT, TDreport, 1.0 - CondFDRelaxFactor );
				EnthOld = EnthNew;
			}

		} // Time Loop  //PT solving time steps

		TempSurfOutTmp = TDT( 1 );
		TempSurfInTmp = TDT( TotNodes + 1 );
		RhoVaporSurfIn( Surf ) = 0.0;

		// For ground surfaces or when raining, outside face inner half-node heat capacity was unknown and set to -1 in ExteriorBCEqns
		// Now check for the flag and set equal to the second node's outer half-node heat capacity if needed
		if ( surfaceFD.CpDelXRhoS2( 1 ) == -1.0 ) {
			surfaceFD.CpDelXRhoS2( 1 ) = surfaceFD.CpDelXRhoS1( 2 ); // Set to node 2's outer half node heat capacity
		}
		CalcNodeHeatFlux( Surf, TotNodes );

		// Determine largest change in node temps
		MaxDelTemp = 0.0;
		for ( int NodeNum = 1; NodeNum <= TotNodes + 1; ++NodeNum ) { // need to consider all nodes
			MaxDelTemp = max( std::abs( TDT( NodeNum ) - TDreport( NodeNum ) ), MaxDelTemp );
		}
		MaxNodeDelTemp = MaxDelTemp;
		//  SurfaceFD(Surf)%TDOld          = SurfaceFD(Surf)%TDT
		TDreport = TDT;
		EnthOld = EnthNew;

	}

	void
	ReportFiniteDiffInits()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   November 2003
		//       MODIFIED       B. Griffith, May 2011 add reporting of node x locations
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine gives a detailed report to the user about
		// the initializations for the Fintie Difference calculations
		// of each construction.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::ScanForReports;
		using General::RoundSigDigits;
		using DataHeatBalance::MaxAllowedDelTempCondFD;
		using DataHeatBalance::CondFDRelaxFactorInput;
		using DataHeatBalance::HeatTransferAlgosUsed;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt fmtLD( "*" );
		static gio::Fmt fmtA( "(A)" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool DoReport;
		std::string InodesChar;
		int ThisNum;
		int Layer;
		int OutwardMatLayerNum;
		int LayerNode;
		int Inodes;

		// Formats
		static gio::Fmt Format_700( "(' Construction CondFD,',A,2(',',A),',',A,',',A)" );
		static gio::Fmt Format_701( "(' Material CondFD Summary,',A,',',A,',',A,',',A,',',A,',',A)" );
		static gio::Fmt Format_702( "(' ConductionFiniteDifference Node,',A,',',A,',',A,',',A,',',A)" );

		gio::write( OutputFileInits, fmtA ) << "! <ConductionFiniteDifference HeatBalanceSettings>,Scheme Type,Space Discretization Constant,Relaxation Factor,Inside Face Surface Temperature Convergence Criteria";
		gio::write( OutputFileInits, fmtA ) << " ConductionFiniteDifference HeatBalanceSettings," + cCondFDSchemeType( CondFDSchemeType ) + ',' + RoundSigDigits( SpaceDescritConstant, 2 ) + ',' + RoundSigDigits( CondFDRelaxFactorInput, 2 ) + ',' + RoundSigDigits( MaxAllowedDelTempCondFD, 4 );
		ScanForReports( "Constructions", DoReport, "Constructions" );

		if ( DoReport ) {

			//                                      Write Descriptions
			gio::write( OutputFileInits, fmtA ) << "! <Construction CondFD>,Construction Name,Index,#Layers,#Nodes,Time Step {hours}";
			gio::write( OutputFileInits, fmtA ) << "! <Material CondFD Summary>,Material Name,Thickness {m},#Layer Elements,Layer Delta X,Layer Alpha*Delt/Delx**2,Layer Moisture Stability";
			//HT Algo issue
			if ( any_eq( HeatTransferAlgosUsed, UseCondFD ) ) gio::write( OutputFileInits, fmtA ) << "! <ConductionFiniteDifference Node>,Node Identifier, Node Distance From Outside Face {m}, Construction Name, Outward Material Name (or Face), Inward Material Name (or Face)";
			for ( ThisNum = 1; ThisNum <= TotConstructs; ++ThisNum ) {

				if ( Construct( ThisNum ).TypeIsWindow ) continue;
				if ( Construct( ThisNum ).TypeIsIRT ) continue;

				gio::write( OutputFileInits, Format_700 ) << Construct( ThisNum ).Name << RoundSigDigits( ThisNum ) << RoundSigDigits( Construct( ThisNum ).TotLayers ) << RoundSigDigits( int( ConstructFD( ThisNum ).TotNodes + 1 ) ) << RoundSigDigits( ConstructFD( ThisNum ).DeltaTime / SecInHour, 6 );

				for ( Layer = 1; Layer <= Construct( ThisNum ).TotLayers; ++Layer ) {
					gio::write( OutputFileInits, Format_701 ) << ConstructFD( ThisNum ).Name( Layer ) << RoundSigDigits( ConstructFD( ThisNum ).Thickness( Layer ), 4 ) << RoundSigDigits( ConstructFD( ThisNum ).NodeNumPoint( Layer ) ) << RoundSigDigits( ConstructFD( ThisNum ).DelX( Layer ), 8 ) << RoundSigDigits( ConstructFD( ThisNum ).TempStability( Layer ), 8 ) << RoundSigDigits( ConstructFD( ThisNum ).MoistStability( Layer ), 8 );
				}

				//now list each CondFD Node with its X distance from outside face in m along with other identifiers
				Inodes = 0;

				for ( Layer = 1; Layer <= Construct( ThisNum ).TotLayers; ++Layer ) {
					OutwardMatLayerNum = Layer - 1;
					for ( LayerNode = 1; LayerNode <= ConstructFD( ThisNum ).NodeNumPoint( Layer ); ++LayerNode ) {
						++Inodes;
						gio::write( InodesChar, fmtLD ) << Inodes;
						if ( Inodes == 1 ) {
							gio::write( OutputFileInits, Format_702 ) << "Node #" + stripped( InodesChar ) << RoundSigDigits( ConstructFD( ThisNum ).NodeXlocation( Inodes ), 8 ) << Construct( ThisNum ).Name << "Surface Outside Face" << ConstructFD( ThisNum ).Name( Layer );

						} else if ( LayerNode == 1 ) {

							if ( OutwardMatLayerNum > 0 && OutwardMatLayerNum <= Construct( ThisNum ).TotLayers ) {
								gio::write( OutputFileInits, Format_702 ) << "Node #" + stripped( InodesChar ) << RoundSigDigits( ConstructFD( ThisNum ).NodeXlocation( Inodes ), 8 ) << Construct( ThisNum ).Name << ConstructFD( ThisNum ).Name( OutwardMatLayerNum ) << ConstructFD( ThisNum ).Name( Layer );

							}
						} else if ( LayerNode > 1 ) {
							OutwardMatLayerNum = Layer;
							gio::write( OutputFileInits, Format_702 ) << "Node #" + stripped( InodesChar ) << RoundSigDigits( ConstructFD( ThisNum ).NodeXlocation( Inodes ), 8 ) << Construct( ThisNum ).Name << ConstructFD( ThisNum ).Name( OutwardMatLayerNum ) << ConstructFD( ThisNum ).Name( Layer );
						}

					}
				}

				Layer = Construct( ThisNum ).TotLayers;
				++Inodes;
				gio::write( InodesChar, fmtLD ) << Inodes;
				gio::write( OutputFileInits, Format_702 ) << "Node #" + stripped( InodesChar ) << RoundSigDigits( ConstructFD( ThisNum ).NodeXlocation( Inodes ), 8 ) << Construct( ThisNum ).Name << ConstructFD( ThisNum ).Name( Layer ) << "Surface Inside Face";

			}

		}

	}

	// Utility Interpolation Function for the Module
	//******************************************************************************

	Real64
	terpld(
		Array2< Real64 > const & a,
		Real64 const x1,
		int const nind,
		int const ndep
	)
	{
		//author:c. o. pedersen
		//purpose:
		//   this function performs a linear interpolation
		//     on a two dimensional array containing both
		//     dependent and independent variables.

		//inputs:
		//  a = two dimensional array
		//  nind=row containing independent variable
		//  ndep=row containing the dependent variable
		//   x1 = specific independent variable value for which
		//      interpolated output is wanted
		//outputs:
		//    the value of dependent variable corresponding
		//       to x1
		//    routine returns first or last dependent variable
		//      for out of range x1.

		int const first( a.l2() );

		assert( a.size() > 0u );
		Array2< Real64 >::size_type l( 1 );
		Real64 r( a[ 0 ] );
		int last( first );
		for ( int i1 = first + 1, e1 = a.u2(); i1 <= e1; ++i1, ++l ) {
			if ( a[ l ] > r ) {
				r = a[ l ];
				last = i1;
			}
		}

		Array2< Real64 >::size_type lind( a.index( nind, 0 ) );
		Array2< Real64 >::size_type ldep( a.index( ndep, 0 ) );
		if ( ( a.size2() == 1u ) || ( x1 <= a[ lind + first ] ) ) { // [ lind + first ] == ( nind, first )
			return a[ ldep + first ]; // [ ldep + first ] == ( ndep, first )
		} else if ( x1 >= a[ lind + last ] ) { // [ lind + last ] == ( nind, last )
			return a[ ldep + last ]; // [ ldep + last ] == ( ndep, last )
		} else {
			int i;
			int i1( first );
			int i2( last );
			while ( ( i2 - i1 ) > 1 ) {
				i = i1 + ( ( i2 - i1 ) >> 1 ); //Tuned bit shift replaces / 2
				if ( x1 < a[ lind + i ] ) { // [ lind + i ] == ( nind, i )
					i2 = i;
				} else {
					i1 = i;
				}
			}
			i = i2;
			lind += i;
			ldep += i;
			Real64 const fract( ( x1 - a[ lind - 1 ] ) / ( a[ lind ] - a[ lind - 1 ] ) ); // [ lind ] == ( nind, i ), [ lind - 1 ] == ( nind, i - 1 )
			return a[ ldep - 1 ] + fract * ( a[ ldep ] - a[ ldep - 1 ] ); // [ ldep ] == ( ndep, i ), [ ldep - 1 ] == ( ndep, i - 1 )
		}
	}

	// Equation Types of the Module
	//******************************************************************************

	void
	ExteriorBCEqns(
		int const Delt, // Time Increment
		int const i, // Node Index
		int const Lay, // Layer Number for Construction
		int const Surf, // Surface number
		Array1< Real64 > const & EP_UNUSED( T ), // Old node Temperature in MFD finite difference solution
		Array1< Real64 > & TT, // New node Temperature in MFD finite difference solution.
		Array1< Real64 > const & EP_UNUSED( Rhov ), // MFD Nodal Vapor Density[kg/m3] and is the old or last time step result.
		Array1< Real64 > & RhoT, // MFD vapor density for the new time step.
		Array1< Real64 > & EP_UNUSED( RH ), // Nodal relative humidity
		Array1< Real64 > const & TD, // The old dry Temperature at each node for the CondFD algorithm..
		Array1< Real64 > & TDT, // The current or new Temperature at each node location for the CondFD solution..
		Array1< Real64 > & EnthOld, // Old Nodal enthalpy
		Array1< Real64 > & EnthNew, // New Nodal enthalpy
		int const TotNodes, // Total nodes in layer
		Real64 const HMovInsul // Conductance of movable(transparent) insulation.
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   November, 2003
		//       MODIFIED       B. Griffith 2010, fix adiabatic and other side surfaces
		//                      May 2011, B. Griffith, P. Tabares
		//                      November 2011 P. Tabares fixed problems with adiabatic walls/massless walls
		//                      November 2011 P. Tabares fixed problems PCM stability problems
		//       RE-ENGINEERED  Curtis Pedersen 2006

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataSurfaces::OtherSideCondModeledExt;
		using DataSurfaces::OSCM;
		using DataSurfaces::HeatTransferModel_CondFD;
		using DataHeatBalSurface::QdotRadOutRepPerArea;
		using DataHeatBalSurface::QdotRadOutRep;
		using DataHeatBalSurface::QRadOutReport;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		auto const & surface( Surface( Surf ) );
		int const surface_ExtBoundCond( surface.ExtBoundCond );

		Real64 Tsky;
		Real64 QRadSWOutFD; // Short wave radiation absorbed on outside of opaque surface
		Real64 QRadSWOutMvInsulFD( 0.0 ); // SW radiation at outside of Movable Insulation
		if ( surface_ExtBoundCond == OtherSideCondModeledExt ) {
			//CR8046 switch modeled rad temp for sky temp.
			Tsky = OSCM( surface.OSCMPtr ).TRad;
			QRadSWOutFD = 0.0; // eliminate incident shortwave on underlying surface
		} else { // Set the external conditions to local variables
			QRadSWOutFD = QRadSWOutAbs( Surf );
			QRadSWOutMvInsulFD = QRadSWOutMvIns( Surf );
			Tsky = SkyTemp;
		}
//		Real64 const Tia( MAT( surface.Zone ) ); //Unused
//		Real64 const SigmaRLoc( SigmaR( ConstrNum ) ); //Unused
//		Real64 const SigmaCLoc( SigmaC( ConstrNum ) ); //Unused

		if ( surface_ExtBoundCond == Ground || IsRain ) {
			TDT( i ) = TT( i ) = TempOutsideAirFD( Surf );
			RhoT( i ) = RhoVaporAirOut( Surf );
			SurfaceFD( Surf ).CpDelXRhoS1( i ) = 0.0; // Outside face  does not have an outer half node
			SurfaceFD( Surf ).CpDelXRhoS2( i ) = -1.0; // Set this to -1 as a flag, then set to node 2's outer half node heat capacity
		} else if ( surface_ExtBoundCond > 0 ) {
			// this is actually the inside face of another surface, or maybe this same surface if adiabatic
			// switch around arguments for the other surf and call routines as for interior side BC from opposite face

			int const ext_bound_construction( Surface( surface_ExtBoundCond ).Construction );
			int const LayIn( Construct( ext_bound_construction ).TotLayers ); // layer number for call to interior eqs
			int const NodeIn( ConstructFD( ext_bound_construction ).TotNodes + 1 ); // node number "I" for call to interior eqs
			int const TotNodesPlusOne( TotNodes + 1 );
			if ( surface_ExtBoundCond == Surf ) { // adiabatic surface, PT addded since it is not the same as interzone wall
				// as Outside Boundary Condition Object can be left blank.

				auto & surfaceFD( SurfaceFD( Surf ) );
				InteriorBCEqns( Delt, NodeIn, LayIn, Surf, surfaceFD.T, surfaceFD.TT, surfaceFD.Rhov, surfaceFD.RhoT, surfaceFD.RH, surfaceFD.TD, surfaceFD.TDT, surfaceFD.EnthOld, surfaceFD.EnthNew, surfaceFD.TDreport );
				TDT( i ) = surfaceFD.TDT( TotNodesPlusOne );
				TT( i ) = surfaceFD.TT( TotNodesPlusOne );
				RhoT( i ) = surfaceFD.RhoT( TotNodesPlusOne );

				surfaceFD.CpDelXRhoS1( i ) = 0.0; // Outside face  does not have an outer half node
				surfaceFD.CpDelXRhoS2( i ) = surfaceFD.CpDelXRhoS1( TotNodesPlusOne ); // Save this for computing node flux values

			} else {

				// potential-lkl-from old      CALL InteriorBCEqns(Delt,nodeIn,LayIn,Surf,SurfaceFD(Surface(Surf)%ExtBoundCond)%T, &
				auto & surfaceFDEBC( SurfaceFD( surface_ExtBoundCond ) );
				InteriorBCEqns( Delt, NodeIn, LayIn, surface_ExtBoundCond, surfaceFDEBC.T, surfaceFDEBC.TT, surfaceFDEBC.Rhov, surfaceFDEBC.RhoT, surfaceFDEBC.RH, surfaceFDEBC.TD, surfaceFDEBC.TDT, surfaceFDEBC.EnthOld, surfaceFDEBC.EnthNew, surfaceFDEBC.TDreport );

				TDT( i ) = surfaceFDEBC.TDT( TotNodesPlusOne );
				TT( i ) = surfaceFDEBC.TT( TotNodesPlusOne );
				RhoT( i ) = surfaceFDEBC.RhoT( TotNodesPlusOne );

				SurfaceFD( Surf ).CpDelXRhoS1( i ) = 0.0; // Outside face  does not have an outer half node
				SurfaceFD( Surf ).CpDelXRhoS2( i ) = surfaceFDEBC.CpDelXRhoS1( TotNodesPlusOne ); // Save this for computing node flux values

			}
			//    CALL InteriorBCEqns(Delt,nodeIn,Layin,Surface(Surf)%ExtBoundCond,SurfaceFD(Surf)%T, &
			//                                         SurfaceFD(Surf)%TT, &
			//                                         SurfaceFD(Surf)%Rhov, &
			//                                         SurfaceFD(Surf)%RhoT, &
			//                                         SurfaceFD(Surf)%RH, &
			//                                         SurfaceFD(Surf)%TD, &
			//                                         SurfaceFD(Surf)%TDT, &
			//                                         SurfaceFD(Surf)%EnthOld, &
			//                                         SurfaceFD(Surf)%EnthNew)

			// now fill results from interior BC model eqns into local result for current call
			//    TDT(I)  = SurfaceFD(Surface(Surf)%ExtBoundCond)%TDT(TotNodesPlusOne)
			//    TT(I)   = SurfaceFD(Surface(Surf)%ExtBoundCond)%TT(TotNodesPlusOne)
			//    RhoT(I) = SurfaceFD(Surface(Surf)%ExtBoundCond)%RhoT(TotNodesPlusOne)
			//    TDT(I)  = SurfaceFD(Surf)%TDT( i)
			//    TT(I)   = SurfaceFD(Surf)%TT( i)
			//    RhoT(I) = SurfaceFD(Surf)%RhoT( i)

			Real64 const QNetSurfFromOutside( OpaqSurfInsFaceConductionFlux( surface_ExtBoundCond ) ); // filled in InteriorBCEqns
			//    QFluxOutsideToOutSurf(Surf)       = QnetSurfFromOutside
			OpaqSurfOutsideFaceConductionFlux( Surf ) = -QNetSurfFromOutside;
			OpaqSurfOutsideFaceConduction( Surf ) = surface.Area * OpaqSurfOutsideFaceConductionFlux( Surf );
			QHeatOutFlux( Surf ) = QNetSurfFromOutside;

		} else if ( surface_ExtBoundCond <= 0 ) { // regular outside conditions
			auto TDT_i( TDT( i ) );
			auto const TDT_p( TDT( i + 1 ) );

			// Boundary Conditions from Simulation for Exterior
			Real64 const hconvo( HConvExtFD( Surf ) );
//			Real64 const hmasso( HMassConvExtFD( Surf ) ); //Unused

			Real64 const hrad( HAirFD( Surf ) );
			Real64 const hsky( HSkyFD( Surf ) );
			Real64 const hgnd( HGrndFD( Surf ) );
			Real64 const Toa( TempOutsideAirFD( Surf ) );
			Real64 const Tgnd( TempOutsideAirFD( Surf ) );

			if ( surface.HeatTransferAlgorithm == HeatTransferModel_CondFD ) {

				int const ConstrNum( surface.Construction );
				int const MatLay( Construct( ConstrNum ).LayerPoint( Lay ) );
				auto const & mat( Material( MatLay ) );
				auto const & matFD( MaterialFD( MatLay ) );

				// regular outside conditions

				// Calculate the Dry Heat Conduction Equation

				if ( mat.ROnly || mat.Group == 1 ) { // R Layer or Air Layer  **********
					// Use algebraic equation for TDT based on R
					Real64 const Rlayer( mat.Resistance );
					TDT_i = ( TDT_p + ( QRadSWOutFD + hgnd * Tgnd + ( hconvo + hrad ) * Toa + hsky * Tsky ) * Rlayer ) / ( 1.0 + ( hconvo + hgnd + hrad + hsky ) * Rlayer );

				} else { // Regular or phase change material layer

					// Set Thermal Conductivity. Can be constant, simple linear temp dep or multiple linear segment temp function dep.
					auto const & matFD_TempCond( matFD.TempCond );
					assert( matFD_TempCond.u2() >= 3 );
					auto const lTC( matFD_TempCond.index( 2, 1 ) );
					Real64 kt;
					if ( matFD_TempCond[ lTC ] + matFD_TempCond[ lTC+1 ] + matFD_TempCond[ lTC+2 ] >= 0.0 ) { // Multiple Linear Segment Function
						// Use average temp of surface and first node for k
						kt = terpld( matFD_TempCond, ( TDT_i + TDT_p ) / 2.0, 1, 2 ); // 1: Temperature, 2: Thermal conductivity
					} else {
						kt = mat.Conductivity; // 20C base conductivity
						Real64 const kt1( matFD.tk1 ); // linear coefficient (normally zero)
						if ( kt1 != 0.0 ) kt =+ kt1 * ( ( TDT_i + TDT_p ) / 2.0 - 20.0 );
					}

					// Check for phase change material
					auto const TD_i( TD( i ) );
					Real64 const Cpo( mat.SpecHeat ); // Specific heat from idf
					Real64 Cp( Cpo ); // Specific heat modified if PCM, otherwise equal to Cpo // Will be changed if PCM
					auto const & matFD_TempEnth( matFD.TempEnth );
					assert( matFD_TempEnth.u2() >= 3 );
					auto const lTE( matFD_TempEnth.index( 2, 1 ) );
					if ( matFD_TempEnth[ lTE ] + matFD_TempEnth[ lTE+1 ] + matFD_TempEnth[ lTE+2 ] >= 0.0 ) { // Phase change material: Use TempEnth data to generate Cp
						//               CheckhT = Material(MatLay)%TempEnth       ! debug
						// Enthalpy function used to get average specific heat. Updated by GS so enthalpy function is followed.
						EnthOld( i ) = terpld( matFD_TempEnth, TD_i, 1, 2 ); // 1: Temperature, 2: Enthalpy
						EnthNew( i ) = terpld( matFD_TempEnth, TDT_i, 1, 2 ); // 1: Temperature, 2: Enthalpy
						if ( EnthNew( i ) != EnthOld( i ) ) {
							Cp = max( Cpo, ( EnthNew( i ) - EnthOld( i ) ) / ( TDT_i - TD_i ) );
						}
					} // Phase Change Material option

					// Choose Regular or Transparent Insulation Case
					Real64 const RhoS( mat.Density );
					Real64 const DelX( ConstructFD( ConstrNum ).DelX( Lay ) );
					Real64 const Delt_DelX( Delt * DelX );
					SurfaceFD( Surf ).CpDelXRhoS1( i ) = 0.0; // Outside face  does not have an outer half node
					SurfaceFD( Surf ).CpDelXRhoS2( i ) = ( Cp * DelX * RhoS ) / 2.0; // Save this for computing node flux values

					if ( HMovInsul <= 0.0 ) { // Regular  case

						if ( CondFDSchemeType == CrankNicholsonSecondOrder ) { // Second Order equation
							Real64 const Cp_DelX_RhoS_2Delt( Cp * DelX * RhoS / ( 2.0 * Delt ) );
							Real64 const kt_2DelX( kt / ( 2.0 * DelX ) );
							Real64 const hsum( 0.5 * ( hconvo + hgnd + hrad + hsky ) );
							TDT_i = ( QRadSWOutFD + Cp_DelX_RhoS_2Delt * TD_i + kt_2DelX * ( TDT_p - TD_i + TD( i + 1 ) ) + hgnd * Tgnd + ( hconvo + hrad ) * Toa + hsky * Tsky - hsum * TD_i ) / ( hsum + kt_2DelX + Cp_DelX_RhoS_2Delt );
							//feb2012            TDT(I)= (1.0d0*QRadSWOutFD + (0.5d0*Cp*Delx*RhoS*TD(I))/DelT + (0.5d0*kt*(-1.0d0*TD(I) + TD(I+1)))/Delx  &
							//feb2012                     + (0.5d0*kt*TDT(I+1))/Delx + 0.5d0*hgnd*Tgnd + 0.5d0*hgnd*(-1.0d0*TD(I) + Tgnd) + 0.5d0*hconvo*Toa +   &
							//feb2012                        0.5d0*hrad*Toa  &
							//feb2012                     + 0.5d0*hconvo*(-1.0d0*TD(I) + Toa) + 0.5d0*hrad*(-1.0d0*TD(I) + Toa) + 0.5d0*hsky*Tsky +   &
							//feb2012                        0.5d0*hsky*(-1.0d0*TD(I) + Tsky))/  &
							//feb2012                       (0.5d0*hconvo + 0.5d0*hgnd + 0.5d0*hrad + 0.5d0*hsky + (0.5d0*kt)/Delx + (0.5d0*Cp*Delx*RhoS)/DelT)
						} else if ( CondFDSchemeType == FullyImplicitFirstOrder ) { // First Order
							Real64 const Two_Delt_DelX( 2.0 * Delt_DelX );
							Real64 const Cp_DelX2_RhoS( Cp * pow_2( DelX ) * RhoS );
							Real64 const Two_Delt_kt( 2.0 * Delt * kt );
							TDT_i = ( Two_Delt_DelX * ( QRadSWOutFD + hgnd * Tgnd + ( hconvo + hrad ) * Toa + hsky * Tsky ) + Cp_DelX2_RhoS * TD_i + Two_Delt_kt * TDT_p ) / ( Two_Delt_DelX * ( hconvo + hgnd + hrad + hsky ) + Two_Delt_kt + Cp_DelX2_RhoS );
						}

					} else { // HMovInsul > 0.0: Transparent insulation on outside
						// Transparent insulaton additions

						// Movable Insulation Layer Outside surface temp

						Real64 const TInsulOut( ( QRadSWOutMvInsulFD + hgnd * Tgnd + HMovInsul * TDT_i + ( hconvo + hrad ) * Toa + hsky * Tsky ) / ( hconvo + hgnd + HMovInsul + hrad + hsky ) ); // Temperature of outisde face of Outside Insulation
						Real64 const Two_Delt_DelX( 2.0 * Delt_DelX );
						Real64 const Cp_DelX2_RhoS( Cp * pow_2( DelX ) * RhoS );
						Real64 const Two_Delt_kt( 2.0 * Delt * kt );

						//List(List(Rule(TDT,(2*Delt*Delx*QradSWOutAbs +
						//-       Cp*Delx**2*Rhos*TD + 2*Delt*kt*TDTP1 +
						//-       2*Delt*Delx*HmovInsul*Tiso)/
						//-     (2*Delt*Delx*HmovInsul + 2*Delt*kt + Cp*Delx**2*Rhos))))

						// Wall first node temperature behind Movable insulation
						if ( CondFDSchemeType == CrankNicholsonSecondOrder ) {
							TDT_i = ( Two_Delt_DelX * ( QRadSWOutFD + HMovInsul * TInsulOut ) + Cp_DelX2_RhoS * TD_i + Two_Delt_kt * TDT_p ) / ( Two_Delt_DelX * HMovInsul + Two_Delt_kt + Cp_DelX2_RhoS );
						} else if ( CondFDSchemeType == FullyImplicitFirstOrder ) {
							// Currently same as Crank Nicholson, need fully implicit formulation
							TDT_i = ( Two_Delt_DelX * ( QRadSWOutFD + HMovInsul * TInsulOut ) + Cp_DelX2_RhoS * TD_i + Two_Delt_kt * TDT_p ) / ( Two_Delt_DelX * HMovInsul + Two_Delt_kt + Cp_DelX2_RhoS );
						} else {
							assert( false ); // Illegal CondFDSchemeType
						}

					} // Regular layer or Movable insulation cases

				} // R layer or Regular layer

				// Limit clipping
				if ( TDT_i < MinSurfaceTempLimit ) {
					TDT_i = MinSurfaceTempLimit;
				} else if ( TDT_i > MaxSurfaceTempLimit ) {
					TDT_i = MaxSurfaceTempLimit;
				}

				TDT( i ) = TDT_i;

			} // regular detailed FD part or SigmaR SigmaC part

			// Determine net heat flux to ooutside face
			// One formulation that works for Fully Implicit and CrankNicholson and massless wall

			Real64 const Toa_TDT_i( Toa - TDT_i );
			Real64 const QNetSurfFromOutside( QRadSWOutFD + ( hgnd * ( -TDT_i + Tgnd ) + ( hconvo + hrad ) * Toa_TDT_i + hsky * ( -TDT_i + Tsky ) ) );

			//S ame sign convention as CTFs
			OpaqSurfOutsideFaceConductionFlux( Surf ) = -QNetSurfFromOutside;
			OpaqSurfOutsideFaceConduction( Surf ) = surface.Area * OpaqSurfOutsideFaceConductionFlux( Surf );

			// Report all outside BC heat fluxes
			QdotRadOutRepPerArea( Surf ) = -( hgnd * ( TDT_i - Tgnd ) + hrad * ( -Toa_TDT_i ) + hsky * ( TDT_i - Tsky ) );
			QdotRadOutRep( Surf ) = surface.Area * QdotRadOutRepPerArea( Surf );
			QRadOutReport( Surf ) = QdotRadOutRep( Surf ) * TimeStepZoneSec;

		} // regular BC part of the ground and Rain check

	}

	void
	InteriorNodeEqns(
		int const Delt, // Time Increment
		int const i, // Node Index
		int const Lay, // Layer Number for Construction
		int const Surf, // Surface number
		Array1< Real64 > const & EP_UNUSED( T ), // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		Array1< Real64 > & EP_UNUSED( TT ), // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		Array1< Real64 > const & EP_UNUSED( Rhov ), // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		Array1< Real64 > & EP_UNUSED( RhoT ), // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		Array1< Real64 > & EP_UNUSED( RH ), // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		Array1< Real64 > const & TD, // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		Array1< Real64 > & TDT, // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		Array1< Real64 > & EnthOld, // Old Nodal enthalpy
		Array1< Real64 > & EnthNew // New Nodal enthalpy
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   November, 2003
		//       MODIFIED       May 2011, B. Griffith and P. Tabares
		//       RE-ENGINEERED  C. O. Pedersen, 2006

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		//  REAL(r64), PARAMETER :: NinetyNine=99.0d0

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		int const ConstrNum( Surface( Surf ).Construction );

		int const MatLay( Construct( ConstrNum ).LayerPoint( Lay ) );
		auto const & mat( Material( MatLay ) );
		auto const & matFD( MaterialFD( MatLay ) );

		auto const TD_i( TD( i ) );

		auto const TDT_m( TDT( i - 1 ) );
		auto TDT_i( TDT( i ) );
		auto const TDT_p( TDT( i + 1 ) );
		auto const TDT_mi( ( TDT_m + TDT_i ) / 2.0 );
		auto const TDT_ip( ( TDT_i + TDT_p ) / 2.0 );

		//  Set Thermal Conductivity.  Can be constant, simple linear temp dep or multiple linear segment temp function dep.
		auto const & matFD_TempCond( matFD.TempCond );
		assert( matFD_TempCond.u2() >= 3 );
		auto const lTC( matFD_TempCond.index( 2, 1 ) );
		Real64 ktA1; // Variable Outer Thermal conductivity in temperature equation
		Real64 ktA2; // Thermal Inner conductivity in temperature equation
		if ( matFD_TempCond[ lTC ] + matFD_TempCond[ lTC+1 ] + matFD_TempCond[ lTC+2 ] >= 0.0 ) { // Multiple Linear Segment Function
			ktA1 = terpld( matFD.TempCond, TDT_ip, 1, 2 ); // 1: Temperature, 2: Thermal conductivity
			ktA2 = terpld( matFD.TempCond, TDT_mi, 1, 2 ); // 1: Temperature, 2: Thermal conductivity
		} else {
			ktA1 = ktA2 = mat.Conductivity; // 20C base conductivity
			Real64 const kt1( matFD.tk1 ); // temperature coefficient for simple temp dep k. // linear coefficient (normally zero)
			if ( kt1 != 0.0 ) {
				ktA1 += kt1 * ( TDT_ip - 20.0 );
				ktA2 += kt1 * ( TDT_mi - 20.0 );
			}
		}

		Real64 const Cpo( mat.SpecHeat ); // Const Cp from input
		Real64 Cp( Cpo ); // Cp used // Will be changed if PCM
		auto const & matFD_TempEnth( matFD.TempEnth );
		assert( matFD_TempEnth.u2() >= 3 );
		auto const lTE( matFD_TempEnth.index( 2, 1 ) );
		if ( matFD_TempEnth[ lTE ] + matFD_TempEnth[ lTE+1 ] + matFD_TempEnth[ lTE+2 ] >= 0.0 ) { // Phase change material: Use TempEnth data
			EnthOld( i ) = terpld( matFD_TempEnth, TD_i, 1, 2 ); // 1: Temperature, 2: Enthalpy
			EnthNew( i ) = terpld( matFD_TempEnth, TDT_i, 1, 2 ); // 1: Temperature, 2: Enthalpy
			if ( EnthNew( i ) != EnthOld( i ) ) {
				Cp = max( Cpo, ( EnthNew( i ) - EnthOld( i ) ) / ( TDT_i - TD_i ) );
			}
		} // Phase Change case

		Real64 const RhoS( mat.Density );
		Real64 const DelX( ConstructFD( ConstrNum ).DelX( Lay ) );
		Real64 const Cp_DelX_RhoS_Delt( Cp * DelX * RhoS / Delt );
		if ( CondFDSchemeType == CrankNicholsonSecondOrder ) { // Adams-Moulton second order
			Real64 const inv2DelX( 1.0 / ( 2.0 * DelX ) );
			TDT_i = ( ( Cp_DelX_RhoS_Delt * TD_i ) + ( ( ktA1 * ( TD( i + 1 ) - TD_i + TDT_p ) + ktA2 * ( TD( i - 1 ) - TD_i + TDT_m ) ) * inv2DelX ) ) / ( ( ( ktA1 + ktA2 ) * inv2DelX ) + Cp_DelX_RhoS_Delt );
		} else if ( CondFDSchemeType == FullyImplicitFirstOrder ) { // Adams-Moulton First order
			Real64 const invDelX( 1.0 / DelX );
			TDT_i = ( ( Cp_DelX_RhoS_Delt * TD_i ) + ( ( ktA2 * TDT_m ) + ( ktA1 * TDT_p ) ) * invDelX ) / ( ( ( ktA1 + ktA2 ) * invDelX ) + Cp_DelX_RhoS_Delt );
		} else {
			assert( false ); // Illegal CondFDSchemeType
		}

		// Limit clipping
		if ( TDT_i < MinSurfaceTempLimit ) {
			TDT_i = MinSurfaceTempLimit;
		} else if ( TDT_i > MaxSurfaceTempLimit ) {
			TDT_i = MaxSurfaceTempLimit;
		}

		TDT( i ) = TDT_i;
		SurfaceFD( Surf ).CpDelXRhoS1( i ) = SurfaceFD( Surf ).CpDelXRhoS2( i ) = ( Cp * DelX * RhoS ) / 2.0; // Save this for computing node flux values, half nodes are the same here
	}

	void
	IntInterfaceNodeEqns(
		int const Delt, // Time Increment
		int const i, // Node Index
		int const Lay, // Layer Number for Construction
		int const Surf, // Surface number
		Array1< Real64 > const & EP_UNUSED( T ), // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		Array1< Real64 > & EP_UNUSED( TT ), // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		Array1< Real64 > const & EP_UNUSED( Rhov ), // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		Array1< Real64 > & EP_UNUSED( RhoT ), // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		Array1< Real64 > & EP_UNUSED( RH ), // RELATIVE HUMIDITY.
		Array1< Real64 > const & TD, // OLD NODE TEMPERATURES OF EACH HEAT TRANSFER SURF IN CONDFD.
		Array1< Real64 > & TDT, // NEW NODE TEMPERATURES OF EACH HEAT TRANSFER SURF IN CONDFD.
		Array1< Real64 > const & EP_UNUSED( EnthOld ), // Old Nodal enthalpy
		Array1< Real64 > & EnthNew, // New Nodal enthalpy
		int const EP_UNUSED( GSiter ) // Iteration number of Gauss Seidell iteration
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   November, 2003
		//       MODIFIED       May 2011, B. Griffith, P. Tabares,  add first order fully implicit, bug fixes, cleanup
		//       RE-ENGINEERED  Curtis Pedersen, Changed to Implit mode and included enthalpy.  FY2006

		// PURPOSE OF THIS SUBROUTINE:
		// calculate finite difference heat transfer for nodes that interface two different material layers inside construction

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		//  REAL(r64), PARAMETER :: NinetyNine=99.0d0

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		auto const & surface( Surface( Surf ) );

		if ( surface.HeatTransferAlgorithm == HeatTransferModel_CondFD ) { // HT Algo issue

			int const ConstrNum( surface.Construction );
			auto const & construct( Construct( ConstrNum ) );

			int const MatLay( construct.LayerPoint( Lay ) );
			auto const & mat( Material( MatLay ) );

			int const MatLay2( construct.LayerPoint( Lay + 1 ) );
			auto const & mat2( Material( MatLay2 ) );

			auto const TDT_m( TDT( i - 1 ) );
			auto const TDT_p( TDT( i + 1 ) );

			bool const RLayerPresent( mat.ROnly || mat.Group == 1 );
			bool const RLayer2Present( mat2.ROnly || mat2.Group == 1 );

			Real64 const Rlayer( mat.Resistance ); // Resistance value of R Layer
			Real64 const Rlayer2( mat2.Resistance ); // Resistance value of next layer to inside

			if ( RLayerPresent && RLayer2Present ) {

				TDT( i ) = ( Rlayer2 * TDT_m + Rlayer * TDT_p ) / ( Rlayer + Rlayer2 ); // Two adjacent R layers

			} else {

				auto const & matFD( MaterialFD( MatLay ) );
				auto const & matFD2( MaterialFD( MatLay2 ) );
				auto TDT_i( TDT( i ) );

				// Set Thermal Conductivity. Can be constant, simple linear temp dep or multiple linear segment temp function dep.

				Real64 kt1( 0.0 );
				if ( ! RLayerPresent ) {
					auto const & matFD_TempCond( matFD.TempCond );
					assert( matFD_TempCond.u2() >= 3 );
					auto const lTC( matFD_TempCond.index( 2, 1 ) );
					if ( matFD_TempCond[ lTC ] + matFD_TempCond[ lTC+1 ] + matFD_TempCond[ lTC+2 ] >= 0.0 ) { // Multiple Linear Segment Function
						kt1 = terpld( matFD.TempCond, ( TDT_i + TDT_m ) / 2.0, 1, 2 ); // 1: Temperature, 2: Thermal conductivity
					} else {
						kt1 = mat.Conductivity; // 20C base conductivity
						Real64 const kt11( matFD.tk1 ); // temperature coefficient for simple temp dep k. // linear coefficient (normally zero)
						if ( kt11 != 0.0 ) kt1 += kt11 * ( ( TDT_i + TDT_m ) / 2.0 - 20.0 );
					}
				}

				Real64 kt2( 0.0 );
				if ( ! RLayer2Present ) {
					auto const & matFD2_TempCond( matFD2.TempCond );
					assert( matFD2_TempCond.u2() >= 3 );
					auto const lTC2( matFD2_TempCond.index( 2, 1 ) );
					if ( matFD2_TempCond[ lTC2 ] + matFD2_TempCond[ lTC2+1 ] + matFD2_TempCond[ lTC2+2 ] >= 0.0 ) { // Multiple Linear Segment Function
						kt2 = terpld( matFD2_TempCond, ( TDT_i + TDT_p ) / 2.0, 1, 2 ); // 1: Temperature, 2: Thermal conductivity
					} else {
						kt2 = mat2.Conductivity; // 20C base conductivity
						Real64 const kt21( matFD2.tk1 ); // temperature coefficient for simple temp dep k. // linear coefficient (normally zero)
						if ( kt21 != 0.0 ) kt2 += kt21 * ( ( TDT_i + TDT_p ) / 2.0 - 20.0 );
					}
				}

				Real64 const RhoS1( mat.Density );
				Real64 const Cpo1( mat.SpecHeat ); // constant Cp from input file
				Real64 Cp1( Cpo1 ); // Will be reset if PCM
				Real64 const Delx1( ConstructFD( ConstrNum ).DelX( Lay ) );

				Real64 const RhoS2( mat2.Density );
				Real64 const Cpo2( mat2.SpecHeat );
				Real64 Cp2( Cpo2 ); // will be reset if PCM
				Real64 const Delx2( ConstructFD( ConstrNum ).DelX( Lay + 1 ) );

				// Calculate the Dry Heat Conduction Equation

				// Source/Sink Flux Capability ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

				Real64 const QSSFlux( ( surface.Area > 0.0 ) && ( construct.SourceSinkPresent && Lay == construct.SourceAfterLayer ) ? ( QRadSysSource( Surf ) + QPVSysSource( Surf ) ) / surface.Area : 0.0 ); // Source/Sink flux value at a layer interface // Includes QPV Source

				//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

				auto const TD_i( TD( i ) );

				auto const & matFD_TempEnth( matFD.TempEnth );
				assert( matFD_TempEnth.u2() >= 3 );
				auto const lTE( matFD_TempEnth.index( 2, 1 ) );
				Real64 const matFD_sum( matFD_TempEnth[ lTE ] + matFD_TempEnth[ lTE+1 ] + matFD_TempEnth[ lTE+2 ] );

				auto const & matFD2_TempEnth( matFD2.TempEnth );
				assert( matFD2_TempEnth.u2() >= 3 );
				auto const lTE2( matFD2_TempEnth.index( 2, 1 ) );
				Real64 const matFD2_sum( matFD2_TempEnth[ lTE2 ] + matFD2_TempEnth[ lTE2+1 ] + matFD2_TempEnth[ lTE2+2 ] );

				if ( RLayerPresent && ! RLayer2Present ) { // R-layer first

					// Check for PCM second layer
					if ( ( matFD_sum < 0.0 ) && ( matFD2_sum > 0.0 ) ) { // Phase change material Layer2, Use TempEnth Data
						Real64 const Enth2Old( terpld( matFD2_TempEnth, TD_i, 1, 2 ) ); // 1: Temperature, 2: Thermal conductivity
						Real64 const Enth2New( terpld( matFD2_TempEnth, TDT_i, 1, 2 ) ); // 1: Temperature, 2: Thermal conductivity
						EnthNew( i ) = Enth2New; // This node really doesn't have an enthalpy, this gives it a value
						if ( ( std::abs( Enth2New - Enth2Old ) > smalldiff ) && ( std::abs( TDT_i - TD_i ) > smalldiff ) ) {
							Cp2 = max( Cpo2, ( Enth2New - Enth2Old ) / ( TDT_i - TD_i ) );
						}
					}

					// R layer first, then PCM or regular layer
					Real64 const Delt_Delx2( Delt * Delx2 );
					Real64 const Cp2_fac( Cp2 * pow_2( Delx2 ) * RhoS2 * Rlayer );
					Real64 const Delt_kt2_Rlayer( Delt * kt2 * Rlayer );
					if ( CondFDSchemeType == CrankNicholsonSecondOrder ) {
						TDT_i = ( 2.0 * Delt_Delx2 * QSSFlux * Rlayer + ( Cp2_fac - Delt_Delx2 - Delt_kt2_Rlayer ) * TD_i + Delt_Delx2 * ( TD( i - 1 ) + TDT_m ) + Delt_kt2_Rlayer * ( TD( i + 1 ) + TDT_p ) ) / ( Delt_Delx2 + Delt_kt2_Rlayer + Cp2_fac );
					} else if ( CondFDSchemeType == FullyImplicitFirstOrder ) {
						Real64 const Two_Delt_Delx2( 2.0 * Delt_Delx2 );
						Real64 const Two_Delt_kt2_Rlayer( 2.0 * Delt_kt2_Rlayer );
						TDT_i = ( Two_Delt_Delx2 * ( QSSFlux * Rlayer + TDT_m ) + Cp2_fac * TD_i + Two_Delt_kt2_Rlayer * TDT_p ) / ( Two_Delt_Delx2 + Two_Delt_kt2_Rlayer + Cp2_fac );
					}

					// Limit clipping
					if ( TDT_i < MinSurfaceTempLimit ) {
						TDT_i = MinSurfaceTempLimit;
					} else if ( TDT_i > MaxSurfaceTempLimit ) {
						TDT_i = MaxSurfaceTempLimit;
					}
					SurfaceFD( Surf ).CpDelXRhoS1( i ) = 0.0; //  - rlayer has no capacitance, so this is zero
					SurfaceFD( Surf ).CpDelXRhoS2( i ) = ( Cp2 * Delx2 * RhoS2 ) / 2.0; // Save this for computing node flux values

				} else if ( ! RLayerPresent && RLayer2Present ) { // R-layer second

					// Check for PCM layer before R layer
					if ( ( matFD_sum > 0.0 ) && ( matFD2_sum < 0.0 ) ) { // Phase change material Layer1, Use TempEnth Data
						Real64 const Enth1Old( terpld( matFD_TempEnth, TD_i, 1, 2 ) ); // 1: Temperature, 2: Thermal conductivity
						Real64 const Enth1New( terpld( matFD_TempEnth, TDT_i, 1, 2 ) ); // 1: Temperature, 2: Thermal conductivity
						EnthNew( i ) = Enth1New; // This node really doesn't have an enthalpy, this gives it a value
						if ( ( std::abs( Enth1New - Enth1Old ) > smalldiff ) && ( std::abs( TDT_i - TD_i ) > smalldiff ) ) {
							Cp1 = max( Cpo1, ( Enth1New - Enth1Old ) / ( TDT_i - TD_i ) );
						}
					}

					Real64 const Delt_Delx1( Delt * Delx1 );
					Real64 const Cp1_fac( Cp1 * pow_2( Delx1 ) * RhoS1 * Rlayer2 );
					Real64 const Delt_kt1_Rlayer2( Delt * kt1 * Rlayer2 );
					if ( CondFDSchemeType == CrankNicholsonSecondOrder ) {
						TDT_i = ( 2.0 * Delt_Delx1 * QSSFlux * Rlayer2 + ( Cp1_fac - Delt_Delx1 - Delt_kt1_Rlayer2 ) * TD_i + Delt_Delx1 * ( TD( i + 1 ) + TDT_p ) + Delt_kt1_Rlayer2 * ( TD( i - 1 ) + TDT_m ) ) / ( Delt_Delx1 + Delt_kt1_Rlayer2 + Cp1_fac );
					} else if ( CondFDSchemeType == FullyImplicitFirstOrder ) {
						Real64 const Two_Delt_Delx1( 2.0 * Delt_Delx1 );
						Real64 const Two_Delt_kt1_Rlayer2( 2.0 * Delt_kt1_Rlayer2 );
						TDT_i = ( Two_Delt_Delx1 * ( QSSFlux * Rlayer2 + TDT_p ) + Cp1_fac * TD_i + Two_Delt_kt1_Rlayer2 * TDT_m ) / ( Two_Delt_Delx1 + Two_Delt_kt1_Rlayer2 + Cp1_fac );
					}

					// Limit clipping
					if ( TDT_i < MinSurfaceTempLimit ) {
						TDT_i = MinSurfaceTempLimit;
					} else if ( TDT_i > MaxSurfaceTempLimit ) {
						TDT_i = MaxSurfaceTempLimit;
					}
					SurfaceFD( Surf ).CpDelXRhoS1( i ) = ( Cp1 * Delx1 * RhoS1 ) / 2.0; // Save this for computing node flux values
					SurfaceFD( Surf ).CpDelXRhoS2( i ) = 0.0; //  - rlayer has no capacitance, so this is zero

				} else { // Regular or Phase Change on both sides of interface

					// Consider the various PCM material location cases
					if ( ( matFD_sum > 0.0 ) && ( matFD2_sum > 0.0 ) ) { // Phase change material both layers, Use TempEnth Data

						Real64 const Enth1Old( terpld( matFD_TempEnth, TD_i, 1, 2 ) ); // 1: Temperature, 2: Thermal conductivity
						Real64 const Enth2Old( terpld( matFD2_TempEnth, TD_i, 1, 2 ) ); // 1: Temperature, 2: Thermal conductivity
						Real64 const Enth1New( terpld( matFD_TempEnth, TDT_i, 1, 2 ) ); // 1: Temperature, 2: Thermal conductivity
						Real64 const Enth2New( terpld( matFD2_TempEnth, TDT_i, 1, 2 ) ); // 1: Temperature, 2: Thermal conductivity

						EnthNew( i ) = Enth1New; // This node really doesn't have an enthalpy, this gives it a value

						if ( ( std::abs( Enth1New - Enth1Old ) > smalldiff ) && ( std::abs( TDT_i - TD_i ) > smalldiff ) ) {
							Cp1 = max( Cpo1, ( Enth1New - Enth1Old ) / ( TDT_i - TD_i ) );
						}

						if ( ( std::abs( Enth2New - Enth2Old ) > smalldiff ) && ( std::abs( TDT_i - TD_i ) > smalldiff ) ) {
							Cp2 = max( Cpo2, ( Enth2New - Enth2Old ) / ( TDT_i - TD_i ) );
						}

					} else if ( ( matFD_sum > 0.0 ) && ( matFD2_sum < 0.0 ) ) { // Phase change material Layer1, Use TempEnth Data

						Real64 const Enth1Old( terpld( matFD_TempEnth, TD_i, 1, 2 ) ); // 1: Temperature, 2: Thermal conductivity
						Real64 const Enth1New( terpld( matFD_TempEnth, TDT_i, 1, 2 ) ); // 1: Temperature, 2: Thermal conductivity
						EnthNew( i ) = Enth1New; // This node really doesn't have an enthalpy, this gives it a value

						if ( ( std::abs( Enth1New - Enth1Old ) > smalldiff ) && ( std::abs( TDT_i - TD_i ) > smalldiff ) ) {
							Cp1 = max( Cpo1, ( Enth1New - Enth1Old ) / ( TDT_i - TD_i ) );
						}

					} else if ( ( matFD_sum < 0.0 ) && ( matFD2_sum > 0.0 ) ) { // Phase change material Layer2, Use TempEnth Data

						Real64 const Enth2Old( terpld( matFD2_TempEnth, TD_i, 1, 2 ) ); // 1: Temperature, 2: Thermal conductivity
						Real64 const Enth2New( terpld( matFD2_TempEnth, TDT_i, 1, 2 ) ); // 1: Temperature, 2: Thermal conductivity
						EnthNew( i ) = Enth2New; // This node really doesn't have an enthalpy, this gives it a value

						if ( ( std::abs( Enth2New - Enth2Old ) > smalldiff ) && ( std::abs( TDT_i - TD_i ) > smalldiff ) ) {
							Cp2 = max( Cpo2, ( Enth2New - Enth2Old ) / ( TDT_i - TD_i ) );
						}

					} // Phase change material check

					Real64 const Delt_Delx1( Delt * Delx1 );
					Real64 const Delt_Delx2( Delt * Delx2 );
					Real64 const Delt_Delx1_kt2( Delt_Delx1 * kt2 );
					Real64 const Delt_Delx2_kt1( Delt_Delx2 * kt1 );
					Real64 const Delt_sum( Delt_Delx1_kt2 + Delt_Delx2_kt1 );
					Real64 const Cp1_fac( Cp1 * pow_2( Delx1 ) * Delx2 * RhoS1 );
					Real64 const Cp2_fac( Cp2 * Delx1 * pow_2( Delx2 ) * RhoS2 );
					Real64 const Cp_fac( Cp1_fac + Cp2_fac );
					if ( CondFDSchemeType == CrankNicholsonSecondOrder ) { // Regular Internal Interface Node with Source/sink using Adams Moulton second order
						TDT_i = ( 2.0 * Delt_Delx1 * Delx2 * QSSFlux + ( Cp_fac - Delt_sum ) * TD_i + Delt_Delx1_kt2 * ( TD( i + 1 ) + TDT_p ) + Delt_Delx2_kt1 * ( TD( i - 1 ) + TDT_m ) ) / ( Delt_sum + Cp_fac );
					} else if ( CondFDSchemeType == FullyImplicitFirstOrder ) { // First order adams moulton
						TDT_i = ( 2.0 * ( Delt_Delx1 * Delx2 * QSSFlux + Delt_Delx2_kt1 * TDT_m + Delt_Delx1_kt2 * TDT_p ) + Cp_fac * TD_i ) / ( 2.0 * ( Delt_Delx2_kt1 + Delt_Delx1_kt2 ) + Cp_fac );
					}

					// Limit clipping
					if ( TDT_i < MinSurfaceTempLimit ) {
						TDT_i = MinSurfaceTempLimit;
					} else if ( TDT_i > MaxSurfaceTempLimit ) {
						TDT_i = MaxSurfaceTempLimit;
					}
					SurfaceFD( Surf ).CpDelXRhoS1( i ) = ( Cp1 * Delx1 * RhoS1 ) / 2.0; // Save this for computing node flux values
					SurfaceFD( Surf ).CpDelXRhoS2( i ) = ( Cp2 * Delx2 * RhoS2 ) / 2.0; // Save this for computing node flux values

					if ( construct.SourceSinkPresent && ( Lay == construct.SourceAfterLayer ) ) {
						TCondFDSourceNode( Surf ) = TDT_i; // Transfer node temp to Radiant System
						TempSource( Surf ) = TDT_i; // Transfer node temp to DataHeatBalSurface module
						SurfaceFD( Surf ).QSource = QSSFlux;
						SurfaceFD( Surf ).SourceNodeNum = i;
					}

				} // End of R-layer and Regular check

				TDT( i ) = TDT_i;

			}

		} // End of the CondFD if block

	}

	void
	InteriorBCEqns(
		int const Delt, // Time Increment
		int const i, // Node Index
		int const Lay, // Layer Number for Construction
		int const Surf, // Surface number
		Array1< Real64 > const & EP_UNUSED( T ), // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF (Old).
		Array1< Real64 > & EP_UNUSED( TT ), // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF (New).
		Array1< Real64 > const & EP_UNUSED( Rhov ), // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		Array1< Real64 > & EP_UNUSED( RhoT ), // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		Array1< Real64 > & EP_UNUSED( RH ), // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		Array1< Real64 > const & TD, // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		Array1< Real64 > & TDT, // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		Array1< Real64 > & EnthOld, // Old Nodal enthalpy
		Array1< Real64 > & EnthNew, // New Nodal enthalpy
		Array1< Real64 > & TDreport // Temperature value from previous HeatSurfaceHeatManager titeration's value
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   November, 2003
		//       MODIFIED       B. Griffith, P. Tabares, May 2011, add first order fully implicit, bug fixes, cleanup
		//                      November 2011 P. Tabares fixed problems with adiabatic walls/massless walls
		//                      November 2011 P. Tabares fixed problems PCM stability problems
		//       RE-ENGINEERED  C. O. Pedersen 2006

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate the heat transfer at the node on the surfaces inside face (facing zone)

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHeatBalFanSys::MAT;
		using DataHeatBalFanSys::ZoneAirHumRat;
		using DataHeatBalFanSys::QHTRadSysSurf;
		using DataHeatBalFanSys::QHWBaseboardSurf;
		using DataHeatBalFanSys::QSteamBaseboardSurf;
		using DataHeatBalFanSys::QElecBaseboardSurf;
		using DataSurfaces::HeatTransferModel_CondFD;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		auto const & surface( Surface( Surf ) );

		int const ConstrNum( surface.Construction );
//		Real64 const SigmaRLoc( SigmaR( ConstrNum ) ); //Unused
//		Real64 const SigmaCLoc( SigmaC( ConstrNum ) ); //Unused

		// Set the internal conditions to local variables
		Real64 const NetLWRadToSurfFD( NetLWRadToSurf( Surf ) ); // Net interior long wavelength radiation to surface from other surfaces
		Real64 const QRadSWInFD( QRadSWInAbs( Surf ) ); // Short wave radiation absorbed on inside of opaque surface
		Real64 const QHtRadSysSurfFD( QHTRadSysSurf( Surf ) ); // Current radiant heat flux at a surface due to the presence of high temperature radiant heaters
		Real64 const QHWBaseboardSurfFD( QHWBaseboardSurf( Surf ) ); // Current radiant heat flux at a surface due to the presence of hot water baseboard heaters
		Real64 const QSteamBaseboardSurfFD( QSteamBaseboardSurf( Surf ) ); // Current radiant heat flux at a surface due to the presence of steam baseboard heaters
		Real64 const QElecBaseboardSurfFD( QElecBaseboardSurf( Surf ) ); // Current radiant heat flux at a surface due to the presence of electric baseboard heaters
		Real64 const QRadThermInFD( QRadThermInAbs( Surf ) ); // Thermal radiation absorbed on inside surfaces

		// Boundary Conditions from Simulation for Interior
		Real64 hconvi( HConvInFD( Surf ) );
//		Real64 hmassi( HMassConvInFD( Surf ) ); //Unused

		Real64 const Tia( MAT( surface.Zone ) );
//		Real64 const Rhovi( RhoVaporAirIn( Surf ) ); //Unused

		//++++++++++++++++++++++++++++++++++++++++++++++++++++++
		//    Do all the nodes in the surface   Else will switch to SigmaR,SigmaC
		auto TDT_i( TDT( i ) );
		Real64 const QFac( NetLWRadToSurfFD + QHtRadSysSurfFD + QHWBaseboardSurfFD + QSteamBaseboardSurfFD + QElecBaseboardSurfFD + QRadSWInFD + QRadThermInFD );
		if ( surface.HeatTransferAlgorithm == HeatTransferModel_CondFD ) {
			int const MatLay( Construct( ConstrNum ).LayerPoint( Lay ) );
			auto const & mat( Material( MatLay ) );
			auto const & matFD( MaterialFD( MatLay ) );

			// Calculate the Dry Heat Conduction Equation

			if ( mat.ROnly || mat.Group == 1 ) { // R Layer or Air Layer
				// Use algebraic equation for TDT based on R
				Real64 const IterDampConst( 5.0 ); // Damping constant for inside surface temperature iterations. Only used for massless (R-value only) Walls
				Real64 const Rlayer( mat.Resistance );
				if ( ( i == 1 ) && ( surface.ExtBoundCond > 0 ) ) { // this is for an adiabatic partition
					TDT_i = ( TDT( i + 1 ) + ( QFac + hconvi * Tia + TDreport( i ) * IterDampConst ) * Rlayer ) / ( 1.0 + ( hconvi + IterDampConst ) * Rlayer );
				} else { // regular wall
					TDT_i = ( TDT( i - 1 ) + ( QFac + hconvi * Tia + TDreport( i ) * IterDampConst ) * Rlayer ) / ( 1.0 + ( hconvi + IterDampConst ) * Rlayer );
				}
				SurfaceFD( Surf ).CpDelXRhoS1( i ) = 0.0; // Save this for computing node flux values - rlayer has no capacitance
				SurfaceFD( Surf ).CpDelXRhoS2( i ) = 0.0; // Inside face  does not have an inner half node

			} else { //  Regular or PCM
				auto const TDT_m( TDT( i - 1 ) );

				// Set Thermal Conductivity. Can be constant, simple linear temp dep or multiple linear segment temp function dep.
				auto const & matFD_TempCond( matFD.TempCond );
				assert( matFD_TempCond.u2() >= 3 );
				auto const lTC( matFD_TempCond.index( 2, 1 ) );
				Real64 kt;
				if ( matFD_TempCond[ lTC ] + matFD_TempCond[ lTC+1 ] + matFD_TempCond[ lTC+2 ] >= 0.0 ) { // Multiple Linear Segment Function
					// Use average of surface and first node temp for determining k
					kt = terpld( matFD_TempCond, ( TDT_i + TDT_m ) / 2.0, 1, 2 ); // 1: Temperature, 2: Thermal conductivity
				} else {
					kt = mat.Conductivity; // 20C base conductivity
					Real64 const kt1( matFD.tk1 ); // linear coefficient (normally zero)
					if ( kt1 != 0.0 ) kt =+ kt1 * ( ( TDT_i + TDT_m ) / 2.0 - 20.0 );
				}

				auto const TD_i( TD( i ) );
				Real64 const Cpo( mat.SpecHeat );
				Real64 Cp( Cpo ); // Will be changed if PCM
				auto const & matFD_TempEnth( matFD.TempEnth );
				assert( matFD_TempEnth.u2() >= 3 );
				auto const lTE( matFD_TempEnth.index( 2, 1 ) );
				if ( matFD_TempEnth[ lTE ] + matFD_TempEnth[ lTE+1 ] + matFD_TempEnth[ lTE+2 ] >= 0.0 ) { // Phase change material: Use TempEnth data
					EnthOld( i ) = terpld( matFD_TempEnth, TD_i, 1, 2 ); // 1: Temperature, 2: Enthalpy
					EnthNew( i ) = terpld( matFD_TempEnth, TDT_i, 1, 2 ); // 1: Temperature, 2: Enthalpy
					if ( ( std::abs( EnthNew( i ) - EnthOld( i ) ) > smalldiff ) && ( std::abs( TDT_i - TD_i ) > smalldiff ) ) {
						Cp = max( Cpo, ( EnthNew( i ) - EnthOld( i ) ) / ( TDT_i - TD_i ) );
					}
				} // Phase change material check

				Real64 const RhoS( mat.Density );
				Real64 const DelX( ConstructFD( ConstrNum ).DelX( Lay ) );
				Real64 const Delt_DelX( Delt * DelX );
				Real64 const Two_Delt_DelX( 2.0 * Delt_DelX );
				Real64 const Delt_kt( Delt * kt );
				Real64 const Cp_DelX2_RhoS( Cp * pow_2( DelX ) * RhoS );
				if ( ( surface.ExtBoundCond > 0 ) && ( i == 1 ) ) { // this is for an adiabatic or interzone partition
					if ( CondFDSchemeType == CrankNicholsonSecondOrder ) { // Adams-Moulton second order
						TDT_i = ( Two_Delt_DelX * ( QFac + hconvi * Tia ) + ( Cp_DelX2_RhoS - Delt_DelX * hconvi - Delt_kt ) * TD_i + Delt_kt * ( TD( i + 1 ) + TDT( i + 1 ) ) ) / ( Delt_DelX * hconvi + Delt_kt + Cp_DelX2_RhoS );
					} else if ( CondFDSchemeType == FullyImplicitFirstOrder ) { // Adams-Moulton First order
						Real64 const Two_Delt_kt( 2.0 * Delt_kt );
						TDT_i = ( Two_Delt_DelX * ( QFac + hconvi * Tia ) + Cp_DelX2_RhoS * TD_i + Two_Delt_kt * TDT( i + 1 ) ) / ( Two_Delt_DelX * hconvi + Two_Delt_kt + Cp_DelX2_RhoS );
					}
				} else { // for regular or interzone walls
					if ( CondFDSchemeType == CrankNicholsonSecondOrder ) {
						TDT_i = ( Two_Delt_DelX * ( QFac + hconvi * Tia ) + ( Cp_DelX2_RhoS - Delt_DelX * hconvi - Delt_kt ) * TD_i + Delt_kt * ( TD( i - 1 ) + TDT_m ) ) / ( Delt_DelX * hconvi + Delt_kt + Cp_DelX2_RhoS );
					} else if ( CondFDSchemeType == FullyImplicitFirstOrder ) {
						Real64 const Two_Delt_kt( 2.0 * Delt_kt );
						TDT_i = ( Two_Delt_DelX * ( QFac + hconvi * Tia ) + Cp_DelX2_RhoS * TD_i + Two_Delt_kt * TDT_m ) / ( Two_Delt_DelX * hconvi + Two_Delt_kt + Cp_DelX2_RhoS );
					}
				}
				SurfaceFD( Surf ).CpDelXRhoS1( i ) = ( Cp * DelX * RhoS ) / 2.0; // Save this for computing node flux values
				SurfaceFD( Surf ).CpDelXRhoS2( i ) = 0.0; // Inside face  does not have an inner half node

				//  Pass inside conduction Flux [W/m2] to DataHeatBalanceSurface array
				//          OpaqSurfInsFaceConductionFlux(Surf)= (TDT(I-1)-TDT(I))*kt/Delx
			} // Regular or R layer

			// Limit clipping
			if ( TDT_i < MinSurfaceTempLimit ) {
				TDT_i = MinSurfaceTempLimit;
			} else if ( TDT_i > MaxSurfaceTempLimit ) {
				TDT_i = MaxSurfaceTempLimit;
			}

			TDT( i ) = TDT_i;

		} //  End of Regular node or SigmaR SigmaC option

		Real64 const QNetSurfInside( -( QFac + hconvi * ( -TDT_i + Tia ) ) );
		// note -- no change ref: CR8575
		//feb2012  QNetSurfInside=NetLWRadToSurfFD + QHtRadSysSurfFD + QRadSWInFD + QRadThermInFD + QHWBaseboardSurfFD  + &
		//feb2012             QSteamBaseboardSurfFD+QElecBaseboardSurfFD+hconvi*(-TDT(I) + Tia)

		//  Pass inside conduction Flux [W/m2] to DataHeatBalanceSurface array
		OpaqSurfInsFaceConductionFlux( Surf ) = QNetSurfInside;
		//  QFluxZoneToInSurf(Surf) = QNetSurfInside
		OpaqSurfInsFaceConduction( Surf ) = QNetSurfInside * surface.Area; // for reporting as in CTF, PT

	}

	void
	CheckFDSurfaceTempLimits(
		int const SurfNum, // surface number
		Real64 const CheckTemperature // calculated temperature, not reset
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   August 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Provides a single entry point for checking surface temperature limits as well as
		// setting up for recurring errors if too low or too high.

		// METHODOLOGY EMPLOYED:
		// Use methodology similar to HBSurfaceManager

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::RoundSigDigits;
		using DataAirflowNetwork::SimulateAirflowNetwork;
		using DataAirflowNetwork::AirflowNetworkControlSimple;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ZoneNum;

		ZoneNum = Surface( SurfNum ).Zone;

		//      IF ((TH(SurfNum,1,2) > MaxSurfaceTempLimit) .OR. &
		//          (TH(SurfNum,1,2) < MinSurfaceTempLimit) ) THEN
		if ( WarmupFlag ) ++WarmupSurfTemp;
		if ( ! WarmupFlag || WarmupSurfTemp > 10 || DisplayExtraWarnings ) {
			if ( CheckTemperature < MinSurfaceTempLimit ) {
				if ( Surface( SurfNum ).LowTempErrCount == 0 ) {
					ShowSevereMessage( "Temperature (low) out of bounds [" + RoundSigDigits( CheckTemperature, 2 ) + "] for zone=\"" + Zone( ZoneNum ).Name + "\", for surface=\"" + Surface( SurfNum ).Name + "\"" );
					ShowContinueErrorTimeStamp( "" );
					if ( ! Zone( ZoneNum ).TempOutOfBoundsReported ) {
						ShowContinueError( "Zone=\"" + Zone( ZoneNum ).Name + "\", Diagnostic Details:" );
						if ( Zone( ZoneNum ).FloorArea > 0.0 ) {
							ShowContinueError( "...Internal Heat Gain [" + RoundSigDigits( Zone( ZoneNum ).InternalHeatGains / Zone( ZoneNum ).FloorArea, 3 ) + "] W/m2" );
						} else {
							ShowContinueError( "...Internal Heat Gain (no floor) [" + RoundSigDigits( Zone( ZoneNum ).InternalHeatGains, 3 ) + "] W" );
						}
						if ( SimulateAirflowNetwork <= AirflowNetworkControlSimple ) {
							ShowContinueError( "...Infiltration/Ventilation [" + RoundSigDigits( Zone( ZoneNum ).NominalInfilVent, 3 ) + "] m3/s" );
							ShowContinueError( "...Mixing/Cross Mixing [" + RoundSigDigits( Zone( ZoneNum ).NominalMixing, 3 ) + "] m3/s" );
						} else {
							ShowContinueError( "...Airflow Network Simulation: Nominal Infiltration/Ventilation/Mixing not available." );
						}
						if ( Zone( ZoneNum ).IsControlled ) {
							ShowContinueError( "...Zone is part of HVAC controlled system." );
						} else {
							ShowContinueError( "...Zone is not part of HVAC controlled system." );
						}
						Zone( ZoneNum ).TempOutOfBoundsReported = true;
					}
					ShowRecurringSevereErrorAtEnd( "Temperature (low) out of bounds for zone=" + Zone( ZoneNum ).Name + " for surface=" + Surface( SurfNum ).Name, Surface( SurfNum ).LowTempErrCount, CheckTemperature, CheckTemperature, _, "C", "C" );
				} else {
					ShowRecurringSevereErrorAtEnd( "Temperature (low) out of bounds for zone=" + Zone( ZoneNum ).Name + " for surface=" + Surface( SurfNum ).Name, Surface( SurfNum ).LowTempErrCount, CheckTemperature, CheckTemperature, _, "C", "C" );
				}
			} else {
				if ( Surface( SurfNum ).HighTempErrCount == 0 ) {
					ShowSevereMessage( "Temperature (high) out of bounds (" + RoundSigDigits( CheckTemperature, 2 ) + "] for zone=\"" + Zone( ZoneNum ).Name + "\", for surface=\"" + Surface( SurfNum ).Name + "\"" );
					ShowContinueErrorTimeStamp( "" );
					if ( ! Zone( ZoneNum ).TempOutOfBoundsReported ) {
						ShowContinueError( "Zone=\"" + Zone( ZoneNum ).Name + "\", Diagnostic Details:" );
						if ( Zone( ZoneNum ).FloorArea > 0.0 ) {
							ShowContinueError( "...Internal Heat Gain [" + RoundSigDigits( Zone( ZoneNum ).InternalHeatGains / Zone( ZoneNum ).FloorArea, 3 ) + "] W/m2" );
						} else {
							ShowContinueError( "...Internal Heat Gain (no floor) [" + RoundSigDigits( Zone( ZoneNum ).InternalHeatGains, 3 ) + "] W" );
						}
						if ( SimulateAirflowNetwork <= AirflowNetworkControlSimple ) {
							ShowContinueError( "...Infiltration/Ventilation [" + RoundSigDigits( Zone( ZoneNum ).NominalInfilVent, 3 ) + "] m3/s" );
							ShowContinueError( "...Mixing/Cross Mixing [" + RoundSigDigits( Zone( ZoneNum ).NominalMixing, 3 ) + "] m3/s" );
						} else {
							ShowContinueError( "...Airflow Network Simulation: Nominal Infiltration/Ventilation/Mixing not available." );
						}
						if ( Zone( ZoneNum ).IsControlled ) {
							ShowContinueError( "...Zone is part of HVAC controlled system." );
						} else {
							ShowContinueError( "...Zone is not part of HVAC controlled system." );
						}
						Zone( ZoneNum ).TempOutOfBoundsReported = true;
					}
					ShowRecurringSevereErrorAtEnd( "Temperature (high) out of bounds for zone=" + Zone( ZoneNum ).Name + " for surface=" + Surface( SurfNum ).Name, Surface( SurfNum ).HighTempErrCount, CheckTemperature, CheckTemperature, _, "C", "C" );
				} else {
					ShowRecurringSevereErrorAtEnd( "Temperature (high) out of bounds for zone=" + Zone( ZoneNum ).Name + " for surface=" + Surface( SurfNum ).Name, Surface( SurfNum ).HighTempErrCount, CheckTemperature, CheckTemperature, _, "C", "C" );
				}
			}
		}

	}

	void
	CalcNodeHeatFlux(
		int const Surf, // surface number
		int const TotNodes // number of nodes in surface
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         M.J. Witte
		//       DATE WRITTEN   Sept-Nov 2015
		// PURPOSE OF THIS SUBROUTINE:
		// Calculate flux at each condFD node
		using General::RoundSigDigits;

		int node; // node counter

		auto & surfaceFD( SurfaceFD( Surf ) );

		// SurfaceFD.QDreport( n ) is the flux at node n
		// When this is called TDT( NodeNum ) is the new node temp and TDpriortimestep( NodeNum ) holds the previous node temp
		// For the TDT and TDpriortimestep arrays, Node 1 is the outside face, and Node TotNodes+1 is the inside face

		// Last node is always the surface inside face.  Start calculations here because the outside face is not defined for all surfaces.
		// Note that TotNodes is the number of nodes in the surface including the outside face node, but not the inside face node
		// so the arrays are all allocated to Totodes+1

		// Heat flux at the inside face node (TotNodes+1)
		surfaceFD.QDreport( TotNodes + 1 ) = OpaqSurfInsFaceConductionFlux( Surf );

		// Heat flux for remaining nodes.
		for ( node = TotNodes; node >= 1; --node ) {
				// Start with inside face (above) and work outward, positive value is flowing towards the inside face
				// CpDelXRhoS1 is outer half-node heat capacity, CpDelXRhoS2 is inner half node heat capacity
			Real64 interNodeFlux; // heat flux at the plane between node and node+1 [W/m2]
			Real64 sourceFlux; // Internal source flux [W/m2]
			if ( surfaceFD.SourceNodeNum == node) {
				sourceFlux = surfaceFD.QSource;
			} else {
				sourceFlux = 0.0;
			}
			interNodeFlux = surfaceFD.QDreport( node + 1 ) + surfaceFD.CpDelXRhoS1( node + 1 )  * ( surfaceFD.TDT( node + 1 ) - surfaceFD.TDpriortimestep( node + 1 ) ) / TimeStepZoneSec;
			surfaceFD.QDreport( node ) = interNodeFlux - sourceFlux + surfaceFD.CpDelXRhoS2( node )  * ( surfaceFD.TDT( node ) - surfaceFD.TDpriortimestep( node ) ) / TimeStepZoneSec;
		}
	}
	// *****************************************************************************


} // HeatBalFiniteDiffManager

} // EnergyPlus
