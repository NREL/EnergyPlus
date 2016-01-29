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
#include <limits>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/member.functions.hh>

// EnergyPlus Headers
#include <ConvectionCoefficients.hh>
#include <CurveManager.hh>
#include <DataEnvironment.hh>
#include <DataErrorTracking.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalSurface.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataRoomAirModel.hh>
#include <DataSurfaces.hh>
#include <DataZoneEquipment.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>
#include <Vectors.hh>

namespace EnergyPlus {

namespace ConvectionCoefficients {

	// Module containing the routines dealing with the convection coefficients

	// MODULE INFORMATION:
	//       AUTHOR         Rick Strand
	//       DATE WRITTEN   August 2000
	//       MODIFIED       Brent Griffith, August 2010 expanded model choices
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module contain the routines dealing with convection coefficients.
	// This module collects correlations/calculations for both the interior and exterior
	// Manages a portion of the input and calculations for Hc values for use in surface heat balances
	// .

	// METHODOLOGY EMPLOYED:
	// Subroutines are called to fill the variable HConvIn with the convection coefficient at
	// the inside face.  or outside face for the current surface.

	// REFERENCES:

	// OTHER NOTES: none

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGlobals;
	using namespace DataLoopNode;
	using namespace DataHeatBalance;
	using namespace DataSurfaces;
	using namespace DataVectorTypes;
	using General::RoundSigDigits;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	Real64 const AdaptiveHcInsideLowLimit( 0.5 ); // W/m2-K
	Real64 const AdaptiveHcOutsideLowLimit( 1.0 ); // W/m2-K
	static gio::Fmt fmtx( "(A,I4,1x,A,1x,6f16.8)" );
	static gio::Fmt fmty( "(A,1x,6f16.8)" );

	Real64 const MinFlow( 0.01 ); // Minimum mass flow rate
	Real64 const MaxACH( 100.0 ); // Maximum ceiling diffuser correlation limit
	static std::string const BlankString;

	Real64 const OneThird( 1.0 / 3.0 ); // 1/3 in highest precision
	Real64 const OneFourth( 1.0 / 4.0 ); // 1/4 in highest precision
	Real64 const OneFifth( 1.0 / 5.0 ); // 1/5 in highest precision
	Real64 const OneSixth( 1.0 / 6.0 ); // 1/6 in highest precision
	Real64 const FourFifths( 4.0 / 5.0 ); // 4/5 in highest precision

	// Coefficients that modify the convection coeff based on surface roughness
	Array1D< Real64 > const RoughnessMultiplier( 6, { 2.17, 1.67, 1.52, 1.13, 1.11, 1.0 } );

	// parameters for identifying more specific hc model equations, inside face
	int const HcInt_UserValue( 200 );
	int const HcInt_UserSchedule( 201 );
	int const HcInt_UserCurve( 202 );
	int const HcInt_ASHRAEVerticalWall( 203 );
	int const HcInt_WaltonUnstableHorizontalOrTilt( 204 );
	int const HcInt_WaltonStableHorizontalOrTilt( 205 );
	int const HcInt_FisherPedersenCeilDiffuserFloor( 206 );
	int const HcInt_FisherPedersenCeilDiffuserCeiling( 207 );
	int const HcInt_FisherPedersenCeilDiffuserWalls( 208 );
	int const HcInt_AlamdariHammondStableHorizontal( 209 );
	int const HcInt_AlamdariHammondVerticalWall( 210 );
	int const HcInt_AlamdariHammondUnstableHorizontal( 211 );
	int const HcInt_KhalifaEq3WallAwayFromHeat( 212 );
	int const HcInt_KhalifaEq4CeilingAwayFromHeat( 213 );
	int const HcInt_KhalifaEq5WallNearHeat( 214 );
	int const HcInt_KhalifaEq6NonHeatedWalls( 215 );
	int const HcInt_KhalifaEq7Ceiling( 216 );
	int const HcInt_AwbiHattonHeatedFloor( 217 );
	int const HcInt_AwbiHattonHeatedWall( 218 );
	int const HcInt_BeausoleilMorrisonMixedAssistingWall( 219 );
	int const HcInt_BeausoleilMorrisonMixedOppossingWall( 220 );
	int const HcInt_BeausoleilMorrisonMixedStableCeiling( 221 );
	int const HcInt_BeausoleilMorrisonMixedUnstableCeiling( 222 );
	int const HcInt_BeausoleilMorrisonMixedStableFloor( 223 );
	int const HcInt_BeausoleilMorrisonMixedUnstableFloor( 224 );
	int const HcInt_FohannoPolidoriVerticalWall( 225 );
	int const HcInt_KaradagChilledCeiling( 226 );
	int const HcInt_ISO15099Windows( 227 );
	int const HcInt_GoldsteinNovoselacCeilingDiffuserWindow( 228 );
	int const HcInt_GoldsteinNovoselacCeilingDiffuserWalls( 229 );
	int const HcInt_GoldsteinNovoselacCeilingDiffuserFloor( 230 );

	//parameters for identifying more specific hc model equations, outside face
	int const HcExt_None( 300 ); // none is allowed because Hn and Hf are split
	int const HcExt_UserValue( 301 );
	int const HcExt_UserSchedule( 302 );
	int const HcExt_UserCurve( 303 );
	int const HcExt_ASHRAESimpleCombined( 304 );
	int const HcExt_NaturalASHRAEVerticalWall( 305 );
	int const HcExt_NaturalWaltonUnstableHorizontalOrTilt( 306 );
	int const HcExt_NaturalWaltonStableHorizontalOrTilt( 307 );
	int const HcExt_SparrowWindward( 308 );
	int const HcExt_SparrowLeeward( 309 );
	int const HcExt_MoWiTTWindward( 310 );
	int const HcExt_MoWiTTLeeward( 311 );
	int const HcExt_DOE2Windward( 312 );
	int const HcExt_DOE2Leeward( 313 );
	int const HcExt_NusseltJurges( 314 );
	int const HcExt_McAdams( 315 );
	int const HcExt_Mitchell( 316 );
	int const HcExt_ClearRoof( 317 );
	int const HcExt_BlockenWindward( 318 );
	int const HcExt_EmmelVertical( 319 );
	int const HcExt_EmmelRoof( 320 );
	int const HcExt_AlamdariHammondVerticalWall( 321 );
	int const HcExt_FohannoPolidoriVerticalWall( 322 );
	int const HcExt_ISO15099Windows( 323 );
	int const HcExt_AlamdariHammondStableHorizontal( 324 );
	int const HcExt_AlamdariHammondUnstableHorizontal( 325 );

	//parameters, by zone, for flow regimes for adaptive convection on inside face
	int const InConvFlowRegime_A1( 1 ); // In-floor heating or in-ceiling cooling
	int const InConvFlowRegime_A2( 2 ); // In-wall heating
	int const InConvFlowRegime_A3( 3 ); // no HVAC system, all bouyancy
	int const InConvFlowRegime_B( 4 ); // Convective heater in zone
	int const InConvFlowRegime_C( 5 ); // central mechanical air
	int const InConvFlowRegime_D( 6 ); // zone mechanical air
	int const InConvFlowRegime_E( 7 ); // mixed. mechancial air and bouyancy

	//params for reference temperature type
	int const RefTempMeanAirTemp( 1 );
	int const RefTempAdjacentAirTemp( 2 );
	int const RefTempSupplyAirTemp( 3 );
	int const RefTempOutDryBulbAtZ( 4 );
	int const RefTempOutDryBulbEPW( 5 );
	int const RefTempOutWetBulbAtZ( 6 );
	int const RefTempOutWetBulbEPW( 7 );

	//params for wind speed type
	int const RefWindWeatherFile( 1 );
	int const RefWindAtZ( 2 );
	int const RefWindParallComp( 3 );
	int const RefWindParallCompAtZ( 4 );

	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:

	int TotOutsideHcUserCurves( 0 );
	int TotInsideHcUserCurves( 0 );
	bool GetUserSuppliedConvectionCoeffs( true ); // Get user input first call for Init

	bool ConvectionGeometryMetaDataSetup( false ); // set to true once geometry meta data are setup
	Real64 CubeRootOfOverallBuildingVolume( 0.0 ); // building meta data. cube root of the volume of all the zones
	Real64 RoofLongAxisOutwardAzimuth( 0.0 ); // roof surfaces meta data. outward normal azimuth for longest roof edge

	// SUBROUTINE SPECIFICATIONS:
	//PRIVATE ApplyConvectionValue ! internal to GetUserConvectionCoefficients

	//more specific Hc model equations

	// Object Data
	InsideFaceAdaptiveConvAlgoStruct InsideFaceAdaptiveConvectionAlgo; // stores rules for Hc model equations
	OutsideFaceAdpativeConvAlgoStruct OutsideFaceAdaptiveConvectionAlgo;
	Array1D< HcInsideFaceUserCurveStruct > HcInsideUserCurve;
	Array1D< HcOutsideFaceUserCurveStruct > HcOutsideUserCurve;
	RoofGeoCharactisticsStruct RoofGeo;

	// Functions

	void
	InitInteriorConvectionCoeffs(
		Array1S< Real64 > const SurfaceTemperatures, // Temperature of surfaces for evaluation of HcIn
		Optional_int_const ZoneToResimulate // if passed in, then only calculate surfaces that have this zone
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   March 1998
		//       MODIFIED       Dan Fisher, Nov 2000
		//                      Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine initializes the arrays associated with interior
		// surface convection.  The main parameter which is initialized
		// in this routine is HConvIn, the convection coefficient on the
		// inside surface.

		// METHODOLOGY EMPLOYED:
		// Determine the temperature difference between the surface and the
		// zone air for the last time step and then base the calculation
		// of the convection coefficient on that value and the surface tilt.

		// REFERENCES:
		// (I)BLAST legacy routine VARTMP
		// 1.  Passive Solar Extension of the BLAST Program
		//       Appendix E. p. 17,18
		// 2.  ASHRAE
		//       Simple Algorithm:    ASHRAE Handbook of Fundamentals 1985, p. 23.2, Table 1
		//       Detailed Algorithm:  ASHRAE Handbook of Fundamentals 2001, p. 3.12, Table 5
		// 3.  Walton, G. N. 1983. Thermal Analysis Research Program (TARP) Reference Manual,
		//     NBSSIR 83-2655, National Bureau of Standards, "Surface Inside Heat Balances", pp 79-80
		// 4.  Fisher, D.E. and C.O. Pedersen, Convective Heat Transfer in Building Energy and
		//       Thermal Load Calculations, ASHRAE Transactions, vol. 103, Pt. 2, 1997, p.137
		// 5.  ISO Standard 15099:2003e

		// Using/Aliasing
		using DataHeatBalFanSys::MAT;
		using DataHeatBalance::Construct;
		using DataZoneEquipment::ZoneEquipInputsFilled;
		using DataZoneEquipment::ZoneEquipSimulatedOnce;
		using DataGlobals::BeginEnvrnFlag;
		using DataLoopNode::Node;
		using DataLoopNode::NumOfNodes;

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ZoneNum; // DO loop counter for zones
		int SurfNum; // DO loop counter for surfaces in zone
		static bool NodeCheck( true ); // for CeilingDiffuser Zones
		static bool ActiveSurfaceCheck( true ); // for radiant surfaces in zone
		static bool MyEnvirnFlag( true );

		// FLOW:
		if ( GetUserSuppliedConvectionCoeffs ) {
			GetUserConvectionCoefficients();
			GetUserSuppliedConvectionCoeffs = false;
		}

		if ( NodeCheck ) { // done once when conditions are ready...
			if ( ! SysSizingCalc && ! ZoneSizingCalc && ZoneEquipInputsFilled && allocated( Node ) ) {
				NodeCheck = false;
				for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
					if ( Zone( ZoneNum ).InsideConvectionAlgo != CeilingDiffuser ) continue;
					if ( Zone( ZoneNum ).SystemZoneNodeNumber != 0 ) continue;
					ShowSevereError( "InitInteriorConvectionCoeffs: Inside Convection=CeilingDiffuser, but no system inlet node defined, Zone=" + Zone( ZoneNum ).Name );
					ShowContinueError( "Defaulting inside convection to TARP. Check ZoneHVAC:EquipmentConnections for Zone=" + Zone( ZoneNum ).Name );
					Zone( ZoneNum ).InsideConvectionAlgo = ASHRAETARP;
				}
				//insert one-time setup for adpative inside face
			}
		}

		if ( ActiveSurfaceCheck && ! SysSizingCalc && ! ZoneSizingCalc && ZoneEquipSimulatedOnce ) {
			SetupAdaptiveConvectionRadiantSurfaceData();
			ActiveSurfaceCheck = false;
		}

		if ( BeginEnvrnFlag && MyEnvirnFlag ) {
			if (
			 std::any_of( Surface.begin(), Surface.end(), []( DataSurfaces::SurfaceData const & e ){ return e.IntConvCoeff == DataHeatBalance::AdaptiveConvectionAlgorithm; } ) ||
			 std::any_of( Zone.begin(), Zone.end(), []( DataHeatBalance::ZoneData const & e ){ return e.InsideConvectionAlgo == DataHeatBalance::AdaptiveConvectionAlgorithm; } ) ) {
				// need to clear out node conditions because dynamic assignments will be affected
				if ( NumOfNodes > 0 && allocated( Node ) ) {
					for ( auto & e : Node ) {
						e.Temp = DefaultNodeValues.Temp;
						e.TempMin = DefaultNodeValues.TempMin;
						e.TempMax = DefaultNodeValues.TempMax;
						e.TempSetPoint = DefaultNodeValues.TempSetPoint;
						e.MassFlowRate = DefaultNodeValues.MassFlowRate;
						e.MassFlowRateMin = DefaultNodeValues.MassFlowRateMin;
						e.MassFlowRateMax = DefaultNodeValues.MassFlowRateMax;
						e.MassFlowRateMinAvail = DefaultNodeValues.MassFlowRateMinAvail;
						e.MassFlowRateMaxAvail = DefaultNodeValues.MassFlowRateMaxAvail;
						e.MassFlowRateSetPoint = DefaultNodeValues.MassFlowRateSetPoint;
						e.Quality = DefaultNodeValues.Quality;
						e.Press = DefaultNodeValues.Press;
						e.Enthalpy = DefaultNodeValues.Enthalpy;
						e.HumRat = DefaultNodeValues.HumRat;
						e.HumRatMin = DefaultNodeValues.HumRatMin;
						e.HumRatMax = DefaultNodeValues.HumRatMax;
						e.HumRatSetPoint = DefaultNodeValues.HumRatSetPoint;
						e.TempSetPointHi = DefaultNodeValues.TempSetPointHi;
						e.TempSetPointLo = DefaultNodeValues.TempSetPointLo;
					}
					if ( allocated( MoreNodeInfo ) ) {
						for ( auto & e : MoreNodeInfo ) {
							e.WetBulbTemp = DefaultNodeValues.Temp;
							e.RelHumidity = 0.0;
							e.ReportEnthalpy = DefaultNodeValues.Enthalpy;
							e.VolFlowRateStdRho = 0.0;
							e.VolFlowRateCrntRho = 0.0;
							e.Density = 0.0;
						}
					}
				}
			}
			MyEnvirnFlag = false;
		}
		if ( ! BeginEnvrnFlag ) MyEnvirnFlag = true;
		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {

			{ auto const SELECT_CASE_var( Zone( ZoneNum ).InsideConvectionAlgo );
			// Ceiling Diffuser and Trombe Wall only make sense at Zone Level
			// Interior convection coeffs are first calculated here and then at surface level
			if ( SELECT_CASE_var == CeilingDiffuser ) {
				CalcCeilingDiffuserIntConvCoeff( ZoneNum );

			} else if ( SELECT_CASE_var == TrombeWall ) {
				CalcTrombeWallIntConvCoeff( ZoneNum, SurfaceTemperatures );

			} else {

			}}

		}
		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {

			for ( SurfNum = Zone( ZoneNum ).SurfaceFirst; SurfNum <= Zone( ZoneNum ).SurfaceLast; ++SurfNum ) {

				if ( ! Surface( SurfNum ).HeatTransSurf ) continue; // Skip non-heat transfer surfaces

				if ( present( ZoneToResimulate ) ) {
					if ( ( ZoneNum != ZoneToResimulate ) && ( AdjacentZoneToSurface( SurfNum ) != ZoneToResimulate ) ) {
						continue; // skip surfaces that are not associated with this zone
					}
				}

				{ auto const SELECT_CASE_var( Surface( SurfNum ).IntConvCoeff );

				if ( ( SELECT_CASE_var <= -1 ) ) { // Set by user using one of the standard algorithms...

					{ auto const SELECT_CASE_var1( std::abs( Surface( SurfNum ).IntConvCoeff ) );

					if ( SELECT_CASE_var1 == ASHRAESimple ) {
						CalcASHRAESimpleIntConvCoeff( SurfNum, SurfaceTemperatures( SurfNum ), MAT( ZoneNum ) );
						// Establish some lower limit to avoid a zero convection coefficient (and potential divide by zero problems)
						if ( HConvIn( SurfNum ) < LowHConvLimit ) HConvIn( SurfNum ) = LowHConvLimit;

					} else if ( SELECT_CASE_var1 == ASHRAETARP ) {
						if ( ! Construct( Surface( SurfNum ).Construction ).TypeIsWindow ) {
							CalcASHRAEDetailedIntConvCoeff( SurfNum, SurfaceTemperatures( SurfNum ), MAT( ZoneNum ) );
						} else {
							CalcISO15099WindowIntConvCoeff( SurfNum, SurfaceTemperatures( SurfNum ), MAT( ZoneNum ) );
						}

						// Establish some lower limit to avoid a zero convection coefficient (and potential divide by zero problems)
						if ( HConvIn( SurfNum ) < LowHConvLimit ) HConvIn( SurfNum ) = LowHConvLimit;

					} else if ( SELECT_CASE_var1 == AdaptiveConvectionAlgorithm ) {

						ManageInsideAdaptiveConvectionAlgo( SurfNum );

					} else {
						ShowFatalError( "Unhandled convection coefficient algorithm." );
					}}

				} else if ( SELECT_CASE_var == 0 ) { // Not set by user, uses Zone Setting

					{ auto const SELECT_CASE_var1( Zone( ZoneNum ).InsideConvectionAlgo );

					if ( SELECT_CASE_var1 == ASHRAESimple ) {
						CalcASHRAESimpleIntConvCoeff( SurfNum, SurfaceTemperatures( SurfNum ), MAT( ZoneNum ) );
						// Establish some lower limit to avoid a zero convection coefficient (and potential divide by zero problems)
						if ( HConvIn( SurfNum ) < LowHConvLimit ) HConvIn( SurfNum ) = LowHConvLimit;

					} else if ( SELECT_CASE_var1 == ASHRAETARP ) {
						if ( ! Construct( Surface( SurfNum ).Construction ).TypeIsWindow ) {
							CalcASHRAEDetailedIntConvCoeff( SurfNum, SurfaceTemperatures( SurfNum ), MAT( ZoneNum ) );
						} else {
							CalcISO15099WindowIntConvCoeff( SurfNum, SurfaceTemperatures( SurfNum ), MAT( ZoneNum ) );
						}
						// Establish some lower limit to avoid a zero convection coefficient (and potential divide by zero problems)
						if ( HConvIn( SurfNum ) < LowHConvLimit ) HConvIn( SurfNum ) = LowHConvLimit;

					} else if ( SELECT_CASE_var1 == AdaptiveConvectionAlgorithm ) {

						ManageInsideAdaptiveConvectionAlgo( SurfNum );

					} else if ( ( SELECT_CASE_var1 == CeilingDiffuser ) || ( SELECT_CASE_var1 == TrombeWall ) ) {
						// Already done above and can't be at individual surface

					} else {
						ShowFatalError( "Unhandled convection coefficient algorithm." );

					}}

				} else { // Interior convection has been set by the user with "value" or "schedule"
					HConvIn( SurfNum ) = SetIntConvectionCoeff( SurfNum );
					// Establish some lower limit to avoid a zero convection coefficient (and potential divide by zero problems)
					if ( HConvIn( SurfNum ) < LowHConvLimit ) HConvIn( SurfNum ) = LowHConvLimit;

				}}

				if ( Surface( SurfNum ).EMSOverrideIntConvCoef ) HConvIn( SurfNum ) = Surface( SurfNum ).EMSValueForIntConvCoef;
			}
		}

	}

	void
	InitExteriorConvectionCoeff(
		int const SurfNum, // Surface number (in Surface derived type)
		Real64 const HMovInsul, // Equivalent convection coefficient of movable insulation
		int const Roughness, // Roughness index (1-6), see DataHeatBalance parameters
		Real64 const AbsExt, // Exterior thermal absorptance
		Real64 const TempExt, // Exterior surface temperature (C)
		Real64 & HExt, // Convection coefficient to exterior air
		Real64 & HSky, // "Convection" coefficient to sky temperature
		Real64 & HGround, // "Convection" coefficient to ground temperature
		Real64 & HAir // Radiation to Air Component
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         George Walton
		//       DATE WRITTEN   January 1990
		//       MODIFIED       na
		//       RE-ENGINEERED  Mar98 (RKS); Sep03 (LKL): Add additional flavors of Ext Convection Coeff.
		//                      Dec03 (PGE): Re-eng'd ASHRAEDetailed to match BLAST and TARP.
		//                      Aug04 (PGE): Corrected error for calculating local wind speeds for different terrains.
		//                      Aug 2010 B. Griffith.  for outside air convection, added new adaptive convection algorithm etc.

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine determines the outside convection coefficient for
		// a particular surface.

		// METHODOLOGY EMPLOYED:
		// Based on the properties of a particular surface, determine what the
		// outside convection coefficients are for outside air, the sky, and
		// the ground.  Convection coefficients for the sky and ground are
		// actually linearized radiation coefficients.  The ground surface is
		// assumed to be the same temperature as the outside air.

		// REFERENCES:
		// (I)BLAST legacy routine OCNVCO
		// TARP Reference Manual, "Surface Outside Heat Balances", pp 71ff

		// Using/Aliasing
		using DataEnvironment::SkyTempKelvin;
		using DataEnvironment::WindDir;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//  REAL(r64),    INTENT(IN)  :: WindSpeedExt  ! Exterior wind speed (m/s)  **No longer used

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const MoWiTTTurbulentConstant( 0.84 ); // Turbulent natural convection constant

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 TAir; // Absolute dry bulb temperature of outdoor air (K)
		//  REAL(r64) :: TSky           ! Absolute temperature of the sky (K)
		Real64 TSurf; // Absolute temperature of the exterior surface (K)
		Real64 SurfWindSpeed; // Local wind speed at height of the heat transfer surface (m/s)
		Real64 ConstantA; // = a, Constant, W/(m2K(m/s)^b)
		Real64 ConstantB; // = b, Constant, W/(m2K^(4/3))
		Real64 Hn; // Natural part of exterior convection
		Real64 Hf; // Forced part of exterior convection
		Real64 HcGlass;
		Real64 rCalcPerimeter; // approximation for Perimeter
		int BaseSurf;
		// REAL(r64) :: flag

		// FLOW:
		if ( GetUserSuppliedConvectionCoeffs ) {
			GetUserConvectionCoefficients();
			GetUserSuppliedConvectionCoeffs = false;
		}

		TAir = Surface( SurfNum ).OutDryBulbTemp + KelvinConv;
		TSurf = TempExt + KelvinConv;

		BaseSurf = Surface( SurfNum ).BaseSurf; // If this is a base surface, BaseSurf = SurfNum

		if ( ! Surface( SurfNum ).ExtWind ) {
			SurfWindSpeed = 0.0; // No wind exposure
		} else if ( Surface( SurfNum ).Class == SurfaceClass_Window && SurfaceWindow( SurfNum ).ShadingFlag == ExtShadeOn ) {
			SurfWindSpeed = 0.0; // Assume zero wind speed at outside glass surface of window with exterior shade
		} else {
			SurfWindSpeed = Surface( SurfNum ).WindSpeed;
		}

		// Check if exterior is to be set by user
		{ auto const SELECT_CASE_var( Surface( SurfNum ).ExtConvCoeff );

		if ( ( SELECT_CASE_var <= -1 ) ) { // Set by user using one of the standard algorithms...

			{ auto const SELECT_CASE_var1( std::abs( Surface( SurfNum ).ExtConvCoeff ) );

			if ( SELECT_CASE_var1 == ASHRAESimple ) {

				HExt = CalcASHRAESimpExtConvectCoeff( Roughness, SurfWindSpeed ); // includes radiation to sky, ground, and air

			} else if ( ( SELECT_CASE_var1 == ASHRAETARP ) || ( SELECT_CASE_var1 == BLASTHcOutside ) || ( SELECT_CASE_var1 == TarpHcOutside ) ) {
				//   Convection is split into forced and natural components. The total
				//   convective heat transfer coefficient is the sum of these components.
				//   Coefficients for subsurfaces are handled in a special way.  The values for perimeter and gross area
				//   are actually referencing the base surface because a subsurface does not initiate a completely new
				//   thermal boundary layer (although it may add some additional complexity that cannot be accounted for
				//   here).  The values for height (Z) and roughness do, however, come from the subsurface.
				//   BLAST algorithm has been replaced by this one since it was identical except for the standard wind
				//   speed measurement height which was only different because of unit conversions:  10 m vs. 30 ft (= 9.14 m).
				//   ASHRAE/BLAST REFERENCES:
				//   ?
				//   TARP REFERENCES:
				//   Walton, G. N.  1983.  Thermal Analysis Research Program Reference Manual.
				//   National Bureau of Standards.  NBSSIR 83-2655.

				// due to outlying calculations when perimeter is very small compared to area, use Perimeter
				// approximation calculation

				if ( Surface( BaseSurf ).GrossArea != 0.0 && Surface( BaseSurf ).Height != 0.0 ) {
					rCalcPerimeter = 2.0 * ( Surface( BaseSurf ).GrossArea / Surface( BaseSurf ).Height + Surface( BaseSurf ).Height );
					Hf = CalcHfExteriorSparrow( SurfWindSpeed, Surface( BaseSurf ).GrossArea, rCalcPerimeter, Surface( SurfNum ).CosTilt, Surface( SurfNum ).Azimuth, Roughness, WindDir );
				} else {
					Hf = 0.0;
				}

				if ( HMovInsul > 0.0 ) TSurf = ( HMovInsul * TSurf + Hf * TAir ) / ( HMovInsul + Hf );
				Hn = CalcHnASHRAETARPExterior( TSurf, TAir, Surface( SurfNum ).CosTilt );
				HExt = Hn + Hf;

			} else if ( SELECT_CASE_var1 == MoWiTTHcOutside ) {
				//   The MoWiTT model is based on measurements taken at the Mobile Window
				//   Thermal Test (MoWiTT) facility.  Appropriate for very smooth surfaces.
				//   REFERENCES:
				//   Yazdanian, M. and J.H. Klems.  1994.  Measurement of the exterior convective
				//   film coefficient for windows in low-rise buildings.
				//   ASHRAE Transactions 100(1):  1087.

				if ( Windward( Surface( SurfNum ).CosTilt, Surface( SurfNum ).Azimuth, WindDir ) ) {
					ConstantA = 3.26;
					ConstantB = 0.89;
				} else { // leeward
					ConstantA = 3.55;
					ConstantB = 0.617;
				}

				// NOTE: Movable insulation is not taken into account here
				HExt = std::sqrt( pow_2( MoWiTTTurbulentConstant * std::pow( std::abs( TAir - TSurf ), OneThird ) ) + pow_2( ConstantA * std::pow( SurfWindSpeed, ConstantB ) ) );

			} else if ( SELECT_CASE_var1 == DOE2HcOutside ) {
				//   The DOE-2 convection model is a combination of the MoWiTT and the BLAST
				//   convection models. However, it calculates the coefficient for very smooth
				//   surfaces (glass) first and then modified for other surfaces.
				//   REFERENCES:
				//   Lawrence Berkeley Laboratory.  1994.  DOE2.1E-053 source code.

				if ( Windward( Surface( SurfNum ).CosTilt, Surface( SurfNum ).Azimuth, WindDir ) ) {
					ConstantA = 3.26;
					ConstantB = 0.89;
				} else { // leeward
					ConstantA = 3.55;
					ConstantB = 0.617;
				}

				Hn = CalcHnASHRAETARPExterior( TSurf, TAir, Surface( SurfNum ).CosTilt );
				HcGlass = std::sqrt( pow_2( Hn ) + pow_2( ConstantA * std::pow( SurfWindSpeed, ConstantB ) ) );
				Hf = RoughnessMultiplier( Roughness ) * ( HcGlass - Hn );
				if ( HMovInsul > 0.0 ) {
					TSurf = ( HMovInsul * TSurf + Hf * TAir ) / ( HMovInsul + Hf );
					Hn = CalcHnASHRAETARPExterior( TSurf, TAir, Surface( SurfNum ).CosTilt );
					// Better if there was iteration for movable insulation?
				}

				HExt = Hn + Hf;

			} else if ( SELECT_CASE_var1 == AdaptiveConvectionAlgorithm ) {

				ManageOutsideAdaptiveConvectionAlgo( SurfNum, HExt );

			} else {
				ShowFatalError( "InitExtConvection Coefficients: invalid parameter -- outside convection type, Surface=" + Surface( SurfNum ).Name );

			}} // choice of algorithm type

			if ( Surface( SurfNum ).EMSOverrideExtConvCoef ) HExt = Surface( SurfNum ).EMSValueForExtConvCoef;

			if ( TSurf == SkyTempKelvin || std::abs( Surface( SurfNum ).ExtConvCoeff ) == ASHRAESimple ) {
				HSky = 0.0;
			} else {
				// Compute sky radiation coefficient
				HSky = StefanBoltzmann * AbsExt * Surface( SurfNum ).ViewFactorSkyIR * AirSkyRadSplit( SurfNum ) * ( pow_4( TSurf ) - pow_4( SkyTempKelvin ) ) / ( TSurf - SkyTempKelvin );
			}

			if ( TSurf == TAir || std::abs( Surface( SurfNum ).ExtConvCoeff ) == ASHRAESimple ) {
				HGround = 0.0;
				HAir = 0.0;
			} else {
				// Compute ground radiation coefficient
				HGround = StefanBoltzmann * AbsExt * Surface( SurfNum ).ViewFactorGroundIR * ( pow_4( TSurf ) - pow_4( TAir ) ) / ( TSurf - TAir );

				// Compute air radiation coefficient
				HAir = StefanBoltzmann * AbsExt * Surface( SurfNum ).ViewFactorSkyIR * ( 1.0 - AirSkyRadSplit( SurfNum ) ) * ( pow_4( TSurf ) - pow_4( TAir ) ) / ( TSurf - TAir );
			}

		} else if ( SELECT_CASE_var == 0 ) { // Not set by user  -- uses Zone setting

			{ auto const SELECT_CASE_var1( Zone( Surface( SurfNum ).Zone ).OutsideConvectionAlgo ); // Algorithm type

			if ( SELECT_CASE_var1 == ASHRAESimple ) {

				HExt = CalcASHRAESimpExtConvectCoeff( Roughness, SurfWindSpeed ); // includes radiation to sky, ground, and air

			} else if ( ( SELECT_CASE_var1 == ASHRAETARP ) || ( SELECT_CASE_var1 == BLASTHcOutside ) || ( SELECT_CASE_var1 == TarpHcOutside ) ) {
				//   Convection is split into forced and natural components. The total
				//   convective heat transfer coefficient is the sum of these components.
				//   Coefficients for subsurfaces are handled in a special way.  The values for perimeter and gross area
				//   are actually referencing the base surface because a subsurface does not initiate a completely new
				//   thermal boundary layer (although it may add some additional complexity that cannot be accounted for
				//   here).  The values for height (Z) and roughness do, however, come from the subsurface.
				//   BLAST algorithm has been replaced by this one since it was identical except for the standard wind
				//   speed measurement height which was only different because of unit conversions:  10 m vs. 30 ft (= 9.14 m).
				//   ASHRAE/BLAST REFERENCES:
				//   ?
				//   TARP REFERENCES:
				//   Walton, G. N.  1983.  Thermal Analysis Research Program Reference Manual.
				//   National Bureau of Standards.  NBSSIR 83-2655.

				// due to outlying calculations when perimeter is very small compared to area, use Perimeter
				// approximation calculation

				if ( Surface( BaseSurf ).GrossArea != 0.0 && Surface( BaseSurf ).Height != 0.0 ) {
					rCalcPerimeter = 2.0 * ( Surface( BaseSurf ).GrossArea / Surface( BaseSurf ).Height + Surface( BaseSurf ).Height );
					Hf = CalcHfExteriorSparrow( SurfWindSpeed, Surface( BaseSurf ).GrossArea, rCalcPerimeter, Surface( SurfNum ).CosTilt, Surface( SurfNum ).Azimuth, Roughness, WindDir );
				} else {
					Hf = 0.0;
				}

				if ( HMovInsul > 0.0 ) TSurf = ( HMovInsul * TSurf + Hf * TAir ) / ( HMovInsul + Hf );
				Hn = CalcHnASHRAETARPExterior( TSurf, TAir, Surface( SurfNum ).CosTilt );
				HExt = Hn + Hf;

			} else if ( SELECT_CASE_var1 == MoWiTTHcOutside ) {
				//   The MoWiTT model is based on measurements taken at the Mobile Window
				//   Thermal Test (MoWiTT) facility.  Appropriate for very smooth surfaces.
				//   REFERENCES:
				//   Yazdanian, M. and J.H. Klems.  1994.  Measurement of the exterior convective
				//   film coefficient for windows in low-rise buildings.
				//   ASHRAE Transactions 100(1):  1087.
				if ( Windward( Surface( SurfNum ).CosTilt, Surface( SurfNum ).Azimuth, WindDir ) ) {
					ConstantA = 3.26;
					ConstantB = 0.89;
				} else { // leeward
					ConstantA = 3.55;
					ConstantB = 0.617;
				}

				// NOTE: Movable insulation is not taken into account here
				HExt = std::sqrt( pow_2( MoWiTTTurbulentConstant * std::pow( std::abs( TAir - TSurf ), OneThird ) ) + pow_2( ConstantA * std::pow( SurfWindSpeed, ConstantB ) ) );

			} else if ( SELECT_CASE_var1 == DOE2HcOutside ) {
				//   The DOE-2 convection model is a combination of the MoWiTT and the BLAST
				//   convection models. However, it calculates the coefficient for very smooth
				//   surfaces (glass) first and then modified for other surfaces.
				//   REFERENCES:
				//   Lawrence Berkeley Laboratory.  1994.  DOE2.1E-053 source code.

				if ( Windward( Surface( SurfNum ).CosTilt, Surface( SurfNum ).Azimuth, WindDir ) ) {
					ConstantA = 3.26;
					ConstantB = 0.89;
				} else { // leeward
					ConstantA = 3.55;
					ConstantB = 0.617;
				}

				Hn = CalcHnASHRAETARPExterior( TSurf, TAir, Surface( SurfNum ).CosTilt );
				HcGlass = std::sqrt( pow_2( Hn ) + pow_2( ConstantA * std::pow( SurfWindSpeed, ConstantB ) ) );
				Hf = RoughnessMultiplier( Roughness ) * ( HcGlass - Hn );
				if ( HMovInsul > 0.0 ) {
					TSurf = ( HMovInsul * TSurf + Hf * TAir ) / ( HMovInsul + Hf );
					Hn = CalcHnASHRAETARPExterior( TSurf, TAir, Surface( SurfNum ).CosTilt );
					// Better if there was iteration for movable insulation?
				}

				HExt = Hn + Hf;

			} else if ( SELECT_CASE_var1 == AdaptiveConvectionAlgorithm ) {

				ManageOutsideAdaptiveConvectionAlgo( SurfNum, HExt );

			} else {
				ShowFatalError( "InitExtConvection Coefficients: invalid parameter -- outside convection type, Surface=" + Surface( SurfNum ).Name );

			}} // choice of algorithm type

			if ( Surface( SurfNum ).EMSOverrideExtConvCoef ) HExt = Surface( SurfNum ).EMSValueForExtConvCoef;

			if ( TSurf == SkyTempKelvin || Zone( Surface( SurfNum ).Zone ).OutsideConvectionAlgo == ASHRAESimple ) {
				HSky = 0.0;
			} else {
				// Compute sky radiation coefficient
				HSky = StefanBoltzmann * AbsExt * Surface( SurfNum ).ViewFactorSkyIR * AirSkyRadSplit( SurfNum ) * ( pow_4( TSurf ) - pow_4( SkyTempKelvin ) ) / ( TSurf - SkyTempKelvin );
			}

			if ( TSurf == TAir || Zone( Surface( SurfNum ).Zone ).OutsideConvectionAlgo == ASHRAESimple ) {
				HGround = 0.0;
				HAir = 0.0;
			} else {
				// Compute ground radiation coefficient
				HGround = StefanBoltzmann * AbsExt * Surface( SurfNum ).ViewFactorGroundIR * ( pow_4( TSurf ) - pow_4( TAir ) ) / ( TSurf - TAir );

				// Compute air radiation coefficient
				HAir = StefanBoltzmann * AbsExt * Surface( SurfNum ).ViewFactorSkyIR * ( 1.0 - AirSkyRadSplit( SurfNum ) ) * ( pow_4( TSurf ) - pow_4( TAir ) ) / ( TSurf - TAir );
			}

		} else { // Exterior convection scheme for this surface has been set by user

			HExt = SetExtConvectionCoeff( SurfNum );

			if ( Surface( SurfNum ).EMSOverrideExtConvCoef ) HExt = Surface( SurfNum ).EMSValueForExtConvCoef;

			if ( TSurf == SkyTempKelvin || Zone( Surface( SurfNum ).Zone ).OutsideConvectionAlgo == ASHRAESimple ) {
				HSky = 0.0;
			} else {
				// Compute sky radiation coefficient
				HSky = StefanBoltzmann * AbsExt * Surface( SurfNum ).ViewFactorSkyIR * AirSkyRadSplit( SurfNum ) * ( pow_4( TSurf ) - pow_4( SkyTempKelvin ) ) / ( TSurf - SkyTempKelvin );
			}

			if ( TSurf == TAir || Zone( Surface( SurfNum ).Zone ).OutsideConvectionAlgo == ASHRAESimple ) {
				HGround = 0.0;
				HAir = 0.0;
			} else {
				// Compute ground radiation coefficient
				HGround = StefanBoltzmann * AbsExt * Surface( SurfNum ).ViewFactorGroundIR * ( pow_4( TSurf ) - pow_4( TAir ) ) / ( TSurf - TAir );

				// Compute air radiation coefficient
				HAir = StefanBoltzmann * AbsExt * Surface( SurfNum ).ViewFactorSkyIR * ( 1.0 - AirSkyRadSplit( SurfNum ) ) * ( pow_4( TSurf ) - pow_4( TAir ) ) / ( TSurf - TAir );
			}

		}}

	}

	Real64
	CalcHfExteriorSparrow(
		Real64 const SurfWindSpeed, // Local wind speed at height of the heat transfer surface (m/s)
		Real64 const GrossArea, // Gross surface area {m2}
		Real64 const Perimeter, // Surface perimeter length {m}
		Real64 const CosTilt, // Cosine of the Surface Tilt Angle
		Real64 const Azimuth, // Facing angle (degrees) of the surface outward normal
		int const Roughness, // Surface roughness index (6=very smooth, 5=smooth, 4=medium smooth,
		Real64 const WindDirection // Wind (compass) direction (degrees)
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function returns the forced convection piece of the
		// exterior convection coefficient.

		// METHODOLOGY EMPLOYED:
		// The forced convection calculation is based on a semi-empirical correlation
		// developed by Sparrow, Ramsey, and Mass.

		// REFERENCES:
		//   1. Sparrow, E. M., J. W. Ramsey, and E. A. Mass.  1979.  Effect of finite
		//   width on heat transfer and fluid flow about an inclined rectangular plate.
		//   Journal of Heat Transfer 101:  204.
		//   2. McClellan, T.M.  1996.  Investigation of a heat balance cooling load
		//   procedure with a detailed study of outside heat transfer parameters.
		//   M.S. Thesis, Department of Mechanical and Industrial Engineering,
		//   University of Illinois at Urbana-Champaign.
		//   3. ASHRAE Loads Toolkit.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hf; // Surface exterior forced convective heat transfer coefficient, W/(m2-K)

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:
		// (Angle between the ground and the surface outward normal)
		// 3=medium rough,2=rough,1=very rough)

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 WindDirectionModifier;

		if ( Windward( CosTilt, Azimuth, WindDirection ) ) {
			WindDirectionModifier = 1.0;
		} else {
			WindDirectionModifier = 0.5;
		}

		Hf = 2.537 * WindDirectionModifier * RoughnessMultiplier( Roughness ) * std::sqrt( SurfWindSpeed * Perimeter / GrossArea );

		return Hf;

	}

	Real64
	CalcHnASHRAETARPExterior(
		Real64 const TOutSurf, // Exterior surface temperature
		Real64 const TAir, // Outdoor Air temperature
		Real64 const CosTilt // Cosine of the Surface Tilt Angle (Angle between the ground outward normal and
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function returns the natural convection piece of the
		// exterior convection coefficient.

		// METHODOLOGY EMPLOYED:
		// Needs description, as appropriate.

		// REFERENCES:
		//   1. ASHRAE.  1993.  ASHRAE Handbook - 1993 Fundamentals.  Atlanta.
		//   2. McClellan, T.M.  1996.  Investigation of a heat balance cooling load
		//   procedure with a detailed study of outside heat transfer parameters.
		//   M.S. Thesis, Department of Mechanical and Industrial Engineering,
		//   University of Illinois at Urbana-Champaign.
		//   3. ASHRAE Loads Toolkit

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hn; // Natural convective heat transfer coefficient,{W/(m2-K)}

		// Locals
		Real64 const OneThird( ( 1.0 / 3.0 ) ); // 1/3 in highest precision

		// FUNCTION ARGUMENT DEFINITIONS:
		// the surface outward normal)

		// FUNCTION PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		// Notes: CosTilt > 0 = faces up (roof), CosTilt < 0 = faces down (floor),
		// CosTilt=0 = vertical surface (wall)

		Hn = 0.0;

		if ( CosTilt == 0.0 ) { // Vertical Surface

			Hn = 1.31 * ( std::pow( std::abs( TOutSurf - TAir ), OneThird ) );

		} else if ( ( ( CosTilt < 0.0 ) && ( TOutSurf < TAir ) ) || ( ( CosTilt > 0.0 ) && ( TOutSurf > TAir ) ) ) { // Enhanced convection

			Hn = 9.482 * ( std::pow( std::abs( TOutSurf - TAir ), OneThird ) ) / ( 7.238 - std::abs( CosTilt ) );

		} else if ( ( ( CosTilt < 0.0 ) && ( TOutSurf > TAir ) ) || ( ( CosTilt > 0.0 ) && ( TOutSurf < TAir ) ) ) { // Reduced convection

			Hn = 1.810 * ( std::pow( std::abs( TOutSurf - TAir ), OneThird ) ) / ( 1.382 + std::abs( CosTilt ) );

		} // Only other condition is TOutSurf=TAir, in which case there is no natural convection part.

		return Hn;

	}

	bool
	Windward(
		Real64 const CosTilt, // Cosine of the surface tilt angle
		Real64 const Azimuth, // or Facing, Direction the surface outward normal faces (degrees)
		Real64 const WindDirection // Wind direction measured clockwise from geographhic North
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function determines if a surface is "windward" or "leeward" (that is,
		// into / against the wind (true) or in shelter from wind (false).

		// METHODOLOGY EMPLOYED:
		// Leeward is defined as greater than 100 degrees from normal incidence.
		// Note that a sufficiently horizontal surface is always considered windward.

		// REFERENCES:
		//   Walton, G. N.  1981.  Passive solar extension of the Building Loads
		//   Analysis and System Thermodynamics (BLAST) program.  Technical Report,
		//   United States Army Construction Engineering Research Laboratory,
		//   Champaign, IL.

		// USE STATEMENTS:
		// na

		// Return value
		bool AgainstWind; // True for windward, false for leeward.

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 Diff; // Difference between the wind direction and the surface azimuth

		AgainstWind = true;
		if ( std::abs( CosTilt ) < 0.98 ) { // Surface is not horizontal
			Diff = std::abs( WindDirection - Azimuth );
			if ( ( Diff - 180.0 ) > 0.001 ) Diff -= 360.0;
			if ( ( std::abs( Diff ) - 100.0 ) > 0.001 ) AgainstWind = false; // Surface is leeward
		}

		return AgainstWind;

	}

	void
	GetUserConvectionCoefficients()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   February 2003
		//       MODIFIED       November 2004; add more "user supplied convection coefficients"
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the input for the object "Convection Coefficients" which
		// can be specified by a user to override the "normally" calculated convection coefficients.  The
		// change (November 2004) allows the user to specify down to the "surface level" the
		// exterior or interior algorithm to be used.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// This routine gets the objects:
		//SurfaceProperty:ConvectionCoefficients,
		//      \memo Allow user settable interior and/or exterior convection coefficients.
		//      \memo Note that some other factors may limit the lower bounds for these values, such as
		//      \memo for windows, the interior convection coefficient must be >.28,
		//      \memo for trombe wall algorithm selection (zone), the interior convection coefficient must be >.1
		//      \memo for detailed interior convection, the lower limit is also .1
		//  A1, \field Surface Name
		//      \required-field
		//      \type object-list
		//      \object-list SurfaceNames
		//  A2, \field Convection Coefficient 1 Location
		//      \required-field
		//      \type choice
		//      \key Outside
		//      \key Inside
		//  A3, \field Convection Coefficient 1 Type
		//      \required-field
		//      \type choice
		//      \key Value
		//      \key Schedule
		//      \key Simple
		//      \key Detailed
		//      \key BLAST
		//      \key TARP
		//      \key DOE-2
		//      \key MoWitt
		//  N1, \field Convection Coefficient 1
		//      \note used if Convection Value Type=value, then minimum must be > 0.0, Maximum <= 1000.
		//      \units W/m2-K
		//  A4, \field Convection Coefficient 1 Schedule Name
		//       \note used if Convection Value Type=Schedule Name
		//       \type object-list
		//       \object-list ScheduleNames
		//   < Remainder fields are a repeat of A2, A3, N1, A4>
		//SurfaceProperty:ConvectionCoefficients:MultipleSurface,
		//      \memo Allow user settable interior and/or exterior convection coefficients.
		//      \memo Note that some other factors may limit the lower bounds for these values, such as
		//      \memo for windows, the interior convection coefficient must be >.28,
		//      \memo for trombe wall algorithm selection (zone), the interior convection coefficient must be >.1
		//      \memo for detailed interior convection, the lower limit is also .1
		//  A1, \field Surface Type
		//      \required-field
		//      \type choice
		//      \key AllExteriorSurfaces
		//      \key AllExteriorWindows
		//      \key AllExteriorWalls
		//      \key AllExteriorRoofs
		//      \key AllExteriorFloors
		//      \key AllInteriorSurfaces
		//      \key AllInteriorWalls
		//      \key AllInteriorWindows
		//      \key AllInteriorCeilings
		//      \key AllInteriorFloors
		//  A2, \field Convection Coefficient 1 Location
		//      \required-field
		//      \type choice
		//      \key Outside
		//      \key Inside
		//  A3, \field Convection Coefficient 1 Type
		//      \required-field
		//      \type choice
		//      \key Value
		//      \key Schedule
		//      \key Simple
		//      \key Detailed
		//      \key BLAST
		//      \key TARP
		//      \key DOE-2
		//      \key MoWitt
		//  N1, \field Convection Coefficient 1
		//      \note used if Convection Value Type=value, then minimum must be > 0.0, Maximum <= 1000.
		//      \units W/m2-K
		//  A4, \field Convection Coefficient 1 Schedule Name
		//       \note used if Convection Coefficient Type=Schedule
		//       \type object-list
		//       \object-list ScheduleNames
		//   < Remainder fields are a repeat of A2, A3, N1, A4>

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using ScheduleManager::GetScheduleIndex;
		using ScheduleManager::CheckScheduleValueMinMax;
		using CurveManager::GetCurveIndex;
		using CurveManager::GetCurveType;
		using DataErrorTracking::TotalSevereErrors;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetUserConvectionCoefficients" );
		int const NumValidSurfaceTypes( 11 );
		static Array1D_string const ValidSurfaceTypes( 11, { "ALLEXTERIORSURFACES", "ALLEXTERIORWINDOWS", "ALLEXTERIORWALLS", "ALLEXTERIORROOFS", "ALLEXTERIORFLOORS", "ALLINTERIORSURFACES", "ALLINTERIORWINDOWS", "ALLINTERIORWALLS", "ALLINTERIORROOFS", "ALLINTERIORCEILINGS", "ALLINTERIORFLOORS" } );

		int const NumValidExtConvectionValueTypes( 22 );
		static Array1D_string const ValidExtConvectionValueTypes( 22, { "VALUE", "SCHEDULE", "SIMPLECOMBINED", "TARP", "MOWITT", "DOE-2", "ADAPTIVECONVECTIONALGORITHM", "USERCURVE", "ASHRAEVERTICALWALL", "WALTONUNSTABLEHORIZONTALORTILT", "WALTONSTABLEHORIZONTALORTILT", "NUSSELTJURGES", "MCADAMS", "MITCHELL", "CLEARROOF", "EMMELVERTICAL", "EMMELROOF", "ALAMDARIHAMMONDVERTICALWALL", "FOHANNOPOLIDORIVERTICALWALL", "ISO15099WINDOWS", "ALAMDARIHAMMONDSTABLEHORIZONTAL", "ALAMDARIHAMMONDUNSTABLEHORIZONTAL" } );

		static Array1D_int const ExtConvectionValue( 22, { -999, -999, ASHRAESimple, TarpHcOutside, MoWiTTHcOutside, DOE2HcOutside, AdaptiveConvectionAlgorithm, HcExt_UserCurve, HcExt_NaturalASHRAEVerticalWall, HcExt_NaturalWaltonUnstableHorizontalOrTilt, HcExt_NaturalWaltonStableHorizontalOrTilt, HcExt_NusseltJurges, HcExt_McAdams, HcExt_Mitchell, HcExt_ClearRoof, HcExt_EmmelVertical, HcExt_EmmelRoof, HcExt_AlamdariHammondVerticalWall, HcExt_FohannoPolidoriVerticalWall, HcExt_ISO15099Windows, HcExt_AlamdariHammondStableHorizontal, HcExt_AlamdariHammondUnstableHorizontal } );

		int const NumValidSpecificExtWindConvValueTypes( 15 );
		static Array1D_string const ValidSpecificExtWindConvValueTypes( 15, { "SIMPLECOMBINED", "TARPWINDWARD", "TARPLEEWARD", "MOWITTWINDWARD", "MOWITTLEEWARD", "DOE2WINDWARD", "DOE2LEEWARD", "NUSSELTJURGES", "MCADAMS", "MITCHELL", "EMMELVERTICAL", "EMMELROOF", "BLOCKENWINDWARD", "CLEARROOF", "USERCURVE" } );
		static Array1D_int const MoreSpecificExtWindConvectionValue( 15, { HcExt_ASHRAESimpleCombined, HcExt_SparrowWindward, HcExt_SparrowLeeward, HcExt_MoWiTTWindward, HcExt_DOE2Windward, HcExt_MoWiTTLeeward, HcExt_DOE2Leeward, HcExt_NusseltJurges, HcExt_McAdams, HcExt_Mitchell, HcExt_EmmelVertical, HcExt_EmmelRoof, HcExt_BlockenWindward, HcExt_ClearRoof, HcExt_UserCurve } );

		int const NumValidSpecificExtNatConvectValueTypes( 10 );
		static Array1D_string const ValidSpecificExtNatConvectValueTypes( 10, { "ASHRAEVERTICALWALL", "ALAMDARIHAMMONDVERTICALWALL", "FOHANNOPOLIDORIVERTICALWALL", "WALTONUNSTABLEHORIZONTALORTILT", "WALTONSTABLEHORIZONTALORTILT", "ALAMDARIHAMMONDSTABLEHORIZONTAL", "ALAMDARIHAMMONDUNSTABLEHORIZONTAL", "ISO15099WINDOWS", "USERCURVE", "NONE" } );
		static Array1D_int const SpecificExtNatConvectionValue( 10, { HcExt_NaturalASHRAEVerticalWall, HcExt_AlamdariHammondVerticalWall, HcExt_FohannoPolidoriVerticalWall, HcExt_NaturalWaltonUnstableHorizontalOrTilt, HcExt_NaturalWaltonStableHorizontalOrTilt, HcExt_AlamdariHammondStableHorizontal, HcExt_AlamdariHammondUnstableHorizontal, HcExt_ISO15099Windows, HcExt_UserCurve, HcExt_None } );

		// CeilingDiffuser and TrombeWall Interior types are only Zone Level settings.
		int const NumValidIntConvectionValueTypes( 34 );
		static Array1D_string const ValidIntConvectionValueTypes( 34, { "VALUE", "SCHEDULE", "SIMPLE", "TARP", "ADAPTIVECONVECTIONALGORITHM", "USERCURVE", "ASHRAEVERTICALWALL", "WALTONUNSTABLEHORIZONTALORTILT", "WALTONSTABLEHORIZONTALORTILT", "FISHERPEDERSENCEILINGDIFFUSERWALLS", "FISHERPEDERSENCEILINGDIFFUSERCEILING", "FISHERPEDERSENCEILINGDIFFUSERFLOOR", "ALAMDARIHAMMONDSTABLEHORIZONTAL", "ALAMDARIHAMMONDUNSTABLEHORIZONTAL", "ALAMDARIHAMMONDVERTICALWALL", "KHALIFAEQ3WALLAWAYFROMHEAT", "KHALIFAEQ4CEILINGAWAYFROMHEAT", "KHALIFAEQ5WALLNEARHEAT", "KHALIFAEQ6NONHEATEDWALLS", "KHALIFAEQ7CEILING", "AWBIHATTONHEATEDFLOOR", "AWBIHATTONHEATEDWALL", "BEAUSOLEILMORRISONMIXEDASSISTEDWALL", "BEAUSOLEILMORRISONMIXEDOPPOSINGWALL", "BEAUSOLEILMORRISONMIXEDSTABLEFLOOR", "BEAUSOLEILMORRISONMIXEDUNSTABLEFLOOR", "BEAUSOLEILMORRISONMIXEDSTABLECEILING", "BEAUSOLEILMORRISONMIXEDUNSTABLECEILING", "FOHANNOPOLIDORIVERTICALWALL", "KARADAGCHILLEDCEILING", "ISO15099WINDOWS", "GOLDSTEINNOVOSELACCEILINGDIFFUSERWINDOW", "GOLDSTEINNOVOSELACCEILINGDIFFUSERWALLS", "GOLDSTEINNOVOSELACCEILINGDIFFUSERFLOOR" } );
		static Array1D_int const IntConvectionValue( 34, { -999, -999, ASHRAESimple, ASHRAETARP, AdaptiveConvectionAlgorithm, HcInt_UserCurve, HcInt_ASHRAEVerticalWall, HcInt_WaltonUnstableHorizontalOrTilt, HcInt_WaltonStableHorizontalOrTilt, HcInt_FisherPedersenCeilDiffuserWalls, HcInt_FisherPedersenCeilDiffuserCeiling, HcInt_FisherPedersenCeilDiffuserFloor, HcInt_AlamdariHammondStableHorizontal, HcInt_AlamdariHammondUnstableHorizontal, HcInt_AlamdariHammondVerticalWall, HcInt_KhalifaEq3WallAwayFromHeat, HcInt_KhalifaEq4CeilingAwayFromHeat, HcInt_KhalifaEq5WallNearHeat, HcInt_KhalifaEq6NonHeatedWalls, HcInt_KhalifaEq7Ceiling, HcInt_AwbiHattonHeatedFloor, HcInt_AwbiHattonHeatedWall, HcInt_BeausoleilMorrisonMixedAssistingWall, HcInt_BeausoleilMorrisonMixedOppossingWall, HcInt_BeausoleilMorrisonMixedStableFloor, HcInt_BeausoleilMorrisonMixedUnstableFloor, HcInt_BeausoleilMorrisonMixedStableCeiling, HcInt_BeausoleilMorrisonMixedUnstableCeiling, HcInt_FohannoPolidoriVerticalWall, HcInt_KaradagChilledCeiling, HcInt_ISO15099Windows, HcInt_GoldsteinNovoselacCeilingDiffuserWindow, HcInt_GoldsteinNovoselacCeilingDiffuserWalls, HcInt_GoldsteinNovoselacCeilingDiffuserFloor } );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Array1D_string Alphas( 9 );
		Array1D< Real64 > Numbers( 2 );
		int NumAlphas;
		int NumNumbers;
		int Loop;
		int Loop1;
		int Count;
		int Status;
		int Found;
		static bool ErrorsFound( false );
		static bool errFlag( false );
		static bool IsValidType( false );
		int ExtValue;
		int IntValue;
		int Ptr;
		int Pass;
		int FieldNo;
		int NumField;
		std::string CurrentModuleObject;
		int PotentialAssignedValue;
		int SurfNum;

		// first get user-defined H models so they can be processed for later objects
		CurrentModuleObject = "SurfaceConvectionAlgorithm:Inside:UserCurve";
		TotInsideHcUserCurves = GetNumObjectsFound( CurrentModuleObject );
		HcInsideUserCurve.allocate( TotInsideHcUserCurves );
		for ( Loop = 1; Loop <= TotInsideHcUserCurves; ++Loop ) {
			GetObjectItem( CurrentModuleObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, Status, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			HcInsideUserCurve( Loop ).Name = cAlphaArgs( 1 );
			{ auto const SELECT_CASE_var( cAlphaArgs( 2 ) );
			if ( SELECT_CASE_var == "MEANAIRTEMPERATURE" ) {
				HcInsideUserCurve( Loop ).ReferenceTempType = RefTempMeanAirTemp;
			} else if ( SELECT_CASE_var == "ADJACENTAIRTEMPERATURE" ) {
				HcInsideUserCurve( Loop ).ReferenceTempType = RefTempAdjacentAirTemp;
			} else if ( SELECT_CASE_var == "SUPPLYAIRTEMPERATURE" ) {
				HcInsideUserCurve( Loop ).ReferenceTempType = RefTempSupplyAirTemp;
			} else {
				ShowSevereError( "GetUserSuppliedConvectionCoefficients: " + CurrentModuleObject + ": Invalid Key choice Entered, for " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
				ErrorsFound = true;
			}}

			if ( ! lAlphaFieldBlanks( 3 ) ) {
				HcInsideUserCurve( Loop ).HcFnTempDiffCurveNum = GetCurveIndex( cAlphaArgs( 3 ) );
				if ( HcInsideUserCurve( Loop ).HcFnTempDiffCurveNum == 0 ) {
					ShowSevereError( "GetUserSuppliedConvectionCoefficients: " + CurrentModuleObject + ": Invalid Name Entered, for " + cAlphaFieldNames( 3 ) + '=' + cAlphaArgs( 3 ) );
					ErrorsFound = true;
				} else { // check type
					{ auto const SELECT_CASE_var( GetCurveType( HcInsideUserCurve( Loop ).HcFnTempDiffCurveNum ) );

					if ( ( SELECT_CASE_var == "LINEAR" ) || ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) || ( SELECT_CASE_var == "EXPONENT" ) || ( SELECT_CASE_var == "QUARTIC" ) ) {
						//okay do nothing, have the right type of curve (single independent variable)
					} else {
						ShowSevereError( CurrentModuleObject + " \"" + cAlphaArgs( 1 ) + "\"" );
						ShowContinueError( "...illegal " + cAlphaFieldNames( 3 ) + " type for this object = " + GetCurveType( GetCurveIndex( cAlphaArgs( 3 ) ) ) );
						ErrorsFound = true;
					}}

				}
			} else {
				HcInsideUserCurve( Loop ).HcFnTempDiffCurveNum = 0;
			}

			if ( ! lAlphaFieldBlanks( 4 ) ) {
				HcInsideUserCurve( Loop ).HcFnTempDiffDivHeightCurveNum = GetCurveIndex( cAlphaArgs( 4 ) );
				if ( HcInsideUserCurve( Loop ).HcFnTempDiffDivHeightCurveNum == 0 ) {
					ShowSevereError( "GetUserSuppliedConvectionCoefficients: " + CurrentModuleObject + ": Invalid Name Entered, for " + cAlphaFieldNames( 4 ) + '=' + cAlphaArgs( 4 ) );
					ErrorsFound = true;
				} else { // check type
					{ auto const SELECT_CASE_var( GetCurveType( HcInsideUserCurve( Loop ).HcFnTempDiffDivHeightCurveNum ) );

					if ( ( SELECT_CASE_var == "LINEAR" ) || ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) || ( SELECT_CASE_var == "EXPONENT" ) || ( SELECT_CASE_var == "QUARTIC" ) ) {
						//okay do nothing, have the right type of curve (single independent variable)
					} else {
						ShowSevereError( CurrentModuleObject + " \"" + cAlphaArgs( 1 ) + "\"" );
						ShowContinueError( "...illegal " + cAlphaFieldNames( 4 ) + " type for this object = " + GetCurveType( GetCurveIndex( cAlphaArgs( 4 ) ) ) );
						ErrorsFound = true;
					}}
				}
			} else {
				HcInsideUserCurve( Loop ).HcFnTempDiffDivHeightCurveNum = 0;
			}

			if ( ! lAlphaFieldBlanks( 5 ) ) {
				HcInsideUserCurve( Loop ).HcFnACHCurveNum = GetCurveIndex( cAlphaArgs( 5 ) );
				if ( HcInsideUserCurve( Loop ).HcFnACHCurveNum == 0 ) {
					ShowSevereError( "GetUserSuppliedConvectionCoefficients: " + CurrentModuleObject + ": Invalid Name Entered, for " + cAlphaFieldNames( 5 ) + '=' + cAlphaArgs( 5 ) );
					ErrorsFound = true;
				} else { // check type
					{ auto const SELECT_CASE_var( GetCurveType( HcInsideUserCurve( Loop ).HcFnACHCurveNum ) );

					if ( ( SELECT_CASE_var == "LINEAR" ) || ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) || ( SELECT_CASE_var == "EXPONENT" ) || ( SELECT_CASE_var == "QUARTIC" ) ) {
						//okay do nothing, have the right type of curve (single independent variable)
					} else {
						ShowSevereError( CurrentModuleObject + " \"" + cAlphaArgs( 1 ) + "\"" );
						ShowContinueError( "...illegal " + cAlphaFieldNames( 5 ) + " type for this object = " + GetCurveType( GetCurveIndex( cAlphaArgs( 5 ) ) ) );
						ErrorsFound = true;
					}}
				}
			} else {
				HcInsideUserCurve( Loop ).HcFnACHCurveNum = 0;
			}

			if ( ! lAlphaFieldBlanks( 6 ) ) {
				HcInsideUserCurve( Loop ).HcFnACHDivPerimLengthCurveNum = GetCurveIndex( cAlphaArgs( 6 ) );
				if ( HcInsideUserCurve( Loop ).HcFnACHDivPerimLengthCurveNum == 0 ) {
					ShowSevereError( "GetUserSuppliedConvectionCoefficients: " + CurrentModuleObject + ": Invalid Name Entered, for " + cAlphaFieldNames( 6 ) + '=' + cAlphaArgs( 6 ) );
					ErrorsFound = true;
				} else { // check type
					{ auto const SELECT_CASE_var( GetCurveType( HcInsideUserCurve( Loop ).HcFnACHDivPerimLengthCurveNum ) );

					if ( ( SELECT_CASE_var == "LINEAR" ) || ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) || ( SELECT_CASE_var == "EXPONENT" ) || ( SELECT_CASE_var == "QUARTIC" ) ) {
						//okay do nothing, have the right type of curve (single independent variable)
					} else {
						ShowSevereError( CurrentModuleObject + " \"" + cAlphaArgs( 1 ) + "\"" );
						ShowContinueError( "...illegal " + cAlphaFieldNames( 6 ) + " type for this object = " + GetCurveType( GetCurveIndex( cAlphaArgs( 6 ) ) ) );
						ErrorsFound = true;
					}}
				}
			} else {
				HcInsideUserCurve( Loop ).HcFnACHDivPerimLengthCurveNum = 0;
			}

		} //end of 'SurfaceConvectionAlgorithm:Inside:UserCurve'

		CurrentModuleObject = "SurfaceConvectionAlgorithm:Outside:UserCurve";
		TotOutsideHcUserCurves = GetNumObjectsFound( CurrentModuleObject );
		HcOutsideUserCurve.allocate( TotOutsideHcUserCurves );
		for ( Loop = 1; Loop <= TotOutsideHcUserCurves; ++Loop ) {
			GetObjectItem( CurrentModuleObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, Status, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			HcOutsideUserCurve( Loop ).Name = cAlphaArgs( 1 );

			{ auto const SELECT_CASE_var( cAlphaArgs( 2 ) );

			if ( SELECT_CASE_var == "WEATHERFILE" ) {
				HcOutsideUserCurve( Loop ).WindSpeedType = RefWindWeatherFile;
			} else if ( SELECT_CASE_var == "HEIGHTADJUST" ) {
				HcOutsideUserCurve( Loop ).WindSpeedType = RefWindAtZ;
			} else if ( SELECT_CASE_var == "PARALLELCOMPONENT" ) {
				HcOutsideUserCurve( Loop ).WindSpeedType = RefWindParallComp;
			} else if ( SELECT_CASE_var == "PARALLELCOMPONENTHEIGHTADJUST" ) {
				HcOutsideUserCurve( Loop ).WindSpeedType = RefWindParallCompAtZ;
			} else {
				ShowSevereError( "GetUserSuppliedConvectionCoefficients: " + CurrentModuleObject + ": Invalid Key choice Entered, for " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
				ErrorsFound = true;
			}}

			// A3 , \field Hf Function of Wind Speed Curve Name
			if ( ! lAlphaFieldBlanks( 3 ) ) {
				HcOutsideUserCurve( Loop ).HfFnWindSpeedCurveNum = GetCurveIndex( cAlphaArgs( 3 ) );
				if ( HcOutsideUserCurve( Loop ).HfFnWindSpeedCurveNum == 0 ) {
					ShowSevereError( "GetUserSuppliedConvectionCoefficients: " + CurrentModuleObject + ": Invalid Name Entered, for " + cAlphaFieldNames( 3 ) + '=' + cAlphaArgs( 3 ) );
					ErrorsFound = true;
				} else { // check type
					{ auto const SELECT_CASE_var( GetCurveType( HcOutsideUserCurve( Loop ).HfFnWindSpeedCurveNum ) );

					if ( ( SELECT_CASE_var == "LINEAR" ) || ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) || ( SELECT_CASE_var == "EXPONENT" ) || ( SELECT_CASE_var == "QUARTIC" ) ) {
						//okay do nothing, have the right type of curve (single independent variable)
					} else {
						ShowSevereError( CurrentModuleObject + " \"" + cAlphaArgs( 1 ) + "\"" );
						ShowContinueError( "...illegal " + cAlphaFieldNames( 3 ) + " type for this object = " + GetCurveType( GetCurveIndex( cAlphaArgs( 3 ) ) ) );
						ErrorsFound = true;
					}}
				}
			} else {
				HcOutsideUserCurve( Loop ).HfFnWindSpeedCurveNum = 0;
			}

			//  A4 , \field Hn Function of Temperature Difference Curve Name
			if ( ! lAlphaFieldBlanks( 4 ) ) {
				HcOutsideUserCurve( Loop ).HnFnTempDiffCurveNum = GetCurveIndex( cAlphaArgs( 4 ) );
				if ( HcOutsideUserCurve( Loop ).HnFnTempDiffCurveNum == 0 ) {
					ShowSevereError( "GetUserSuppliedConvectionCoefficients: " + CurrentModuleObject + ": Invalid Name Entered, for " + cAlphaFieldNames( 4 ) + '=' + cAlphaArgs( 4 ) );
					ErrorsFound = true;
				} else { // check type
					{ auto const SELECT_CASE_var( GetCurveType( HcOutsideUserCurve( Loop ).HnFnTempDiffCurveNum ) );

					if ( ( SELECT_CASE_var == "LINEAR" ) || ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) || ( SELECT_CASE_var == "EXPONENT" ) || ( SELECT_CASE_var == "QUARTIC" ) ) {
						//okay do nothing, have the right type of curve (single independent variable)
					} else {
						ShowSevereError( CurrentModuleObject + " \"" + cAlphaArgs( 1 ) + "\"" );
						ShowContinueError( "...illegal " + cAlphaFieldNames( 4 ) + " type for this object = " + GetCurveType( GetCurveIndex( cAlphaArgs( 4 ) ) ) );
						ErrorsFound = true;
					}}
				}
			} else {
				HcOutsideUserCurve( Loop ).HnFnTempDiffCurveNum = 0;
			}

			//  A5 , \field Hn Function of Temperature Difference Divided by Height Curve Name
			if ( ! lAlphaFieldBlanks( 5 ) ) {
				HcOutsideUserCurve( Loop ).HnFnTempDiffDivHeightCurveNum = GetCurveIndex( cAlphaArgs( 5 ) );
				if ( HcOutsideUserCurve( Loop ).HnFnTempDiffDivHeightCurveNum == 0 ) {
					ShowSevereError( "GetUserSuppliedConvectionCoefficients: " + CurrentModuleObject + ": Invalid Name Entered, for " + cAlphaFieldNames( 5 ) + '=' + cAlphaArgs( 5 ) );
					ErrorsFound = true;
				} else { // check type
					{ auto const SELECT_CASE_var( GetCurveType( HcOutsideUserCurve( Loop ).HnFnTempDiffDivHeightCurveNum ) );

					if ( ( SELECT_CASE_var == "LINEAR" ) || ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) || ( SELECT_CASE_var == "EXPONENT" ) || ( SELECT_CASE_var == "QUARTIC" ) ) {
						//okay do nothing, have the right type of curve (single independent variable)
					} else {
						ShowSevereError( CurrentModuleObject + " \"" + cAlphaArgs( 1 ) + "\"" );
						ShowContinueError( "...illegal " + cAlphaFieldNames( 5 ) + " type for this object = " + GetCurveType( GetCurveIndex( cAlphaArgs( 5 ) ) ) );
						ErrorsFound = true;
					}}
				}
			} else {
				HcOutsideUserCurve( Loop ).HnFnTempDiffDivHeightCurveNum = 0;
			}

		} // 'SurfaceConvectionAlgorithm:Outside:UserCurve'

		// now get user directed overrides at the surface level.
		TotIntConvCoeff = 0;
		TotExtConvCoeff = 0;
		CurrentModuleObject = "SurfaceProperty:ConvectionCoefficients:MultipleSurface";
		Count = GetNumObjectsFound( CurrentModuleObject );
		for ( Loop = 1; Loop <= Count; ++Loop ) {
			GetObjectItem( CurrentModuleObject, Loop, Alphas, NumAlphas, Numbers, NumNumbers, Status, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			if ( Alphas( 2 ) == "INSIDE" ) {
				++TotIntConvCoeff;
			}
			if ( Alphas( 6 ) == "INSIDE" ) {
				++TotIntConvCoeff;
			}
			if ( Alphas( 2 ) == "OUTSIDE" ) {
				++TotExtConvCoeff;
			}
			if ( Alphas( 6 ) == "OUTSIDE" ) {
				++TotExtConvCoeff;
			}
			if ( NumAlphas >= 2 && lAlphaFieldBlanks( 2 ) ) {
				ShowWarningError( "GetUserConvectionCoefficients: " + CurrentModuleObject + ", for " + cAlphaFieldNames( 1 ) + '=' + Alphas( 1 ) );
				ShowContinueError( cAlphaFieldNames( 2 ) + " is blank and rest of fields will not be processed." );
			}
			if ( NumAlphas >= 6 && lAlphaFieldBlanks( 6 ) ) {
				ShowWarningError( "GetUserConvectionCoefficients: " + CurrentModuleObject + ", for " + cAlphaFieldNames( 1 ) + '=' + Alphas( 1 ) );
				ShowContinueError( cAlphaFieldNames( 6 ) + " is blank and rest of fields will not be processed." );
			}
		}
		CurrentModuleObject = "SurfaceProperty:ConvectionCoefficients";
		Count = GetNumObjectsFound( CurrentModuleObject );
		for ( Loop = 1; Loop <= Count; ++Loop ) {
			GetObjectItem( CurrentModuleObject, Loop, Alphas, NumAlphas, Numbers, NumNumbers, Status, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			if ( Alphas( 2 ) == "INSIDE" ) {
				++TotIntConvCoeff;
			}
			if ( Alphas( 6 ) == "INSIDE" ) {
				++TotIntConvCoeff;
			}
			if ( Alphas( 2 ) == "OUTSIDE" ) {
				++TotExtConvCoeff;
			}
			if ( Alphas( 6 ) == "OUTSIDE" ) {
				++TotExtConvCoeff;
			}
			if ( NumAlphas >= 2 && lAlphaFieldBlanks( 2 ) ) {
				ShowWarningError( "GetUserConvectionCoefficients: " + CurrentModuleObject + ", for " + cAlphaFieldNames( 1 ) + '=' + Alphas( 1 ) );
				ShowContinueError( cAlphaFieldNames( 2 ) + " is blank and rest of fields will not be processed." );
			}
			if ( NumAlphas >= 6 && lAlphaFieldBlanks( 6 ) ) {
				ShowWarningError( "GetUserConvectionCoefficients: " + CurrentModuleObject + ", for " + cAlphaFieldNames( 1 ) + '=' + Alphas( 1 ) );
				ShowContinueError( cAlphaFieldNames( 6 ) + " is blank and rest of fields will not be processed." );
			}
		}

		UserIntConvectionCoeffs.allocate( TotIntConvCoeff );
		UserExtConvectionCoeffs.allocate( TotExtConvCoeff );

		TotIntConvCoeff = 0;
		TotExtConvCoeff = 0;

		//   Now, get for real and check for consistency
		CurrentModuleObject = "SurfaceProperty:ConvectionCoefficients";
		Count = GetNumObjectsFound( CurrentModuleObject );
		for ( Loop = 1; Loop <= Count; ++Loop ) {
			GetObjectItem( CurrentModuleObject, Loop, Alphas, NumAlphas, Numbers, NumNumbers, Status, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			Found = FindItemInList( Alphas( 1 ), Surface );
			if ( Found == 0 ) {
				ShowSevereError( "GetUserConvectionCoefficients: " + CurrentModuleObject + ", illegal value for " + cAlphaFieldNames( 1 ) + '=' + Alphas( 1 ) );
				ErrorsFound = true;
				continue;
			}

			Ptr = 2;
			FieldNo = 2;
			NumField = 1;
			for ( Pass = 1; Pass <= 2; ++Pass ) {

				{ auto const SELECT_CASE_var( Alphas( Ptr ) );
				if ( SELECT_CASE_var == "OUTSIDE" ) {
					if ( Surface( Found ).OSCPtr > 0 ) {
						ShowSevereError( "GetUserSuppliedConvectionCoefficients: " + CurrentModuleObject + ", OUTSIDE " + CurrentModuleObject + " cannot be specified for OtherSideCoefficient Surface=" + Alphas( 1 ) );
						ErrorsFound = true;
					}
					IsValidType = false;
					ExtValue = 0;
					PotentialAssignedValue = 0;
					for ( Loop1 = 1; Loop1 <= NumValidExtConvectionValueTypes; ++Loop1 ) {
						if ( Alphas( Ptr + 1 ) != ValidExtConvectionValueTypes( Loop1 ) ) continue;
						ExtValue = ExtConvectionValue( Loop1 );
						IsValidType = true;
						break;
					}

					if ( IsValidType && ( Loop1 > 2 ) && ( Loop1 <= 7 ) ) {
						PotentialAssignedValue = -ExtValue;
					} else if ( IsValidType && Loop1 == 1 ) { // Value
						++TotExtConvCoeff;
						UserExtConvectionCoeffs( TotExtConvCoeff ).SurfaceName = Alphas( 1 );
						UserExtConvectionCoeffs( TotExtConvCoeff ).WhichSurface = Found;
						if ( Numbers( NumField ) < LowHConvLimit || Numbers( NumField ) > HighHConvLimit ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + ", out of range value" );
							ShowContinueError( cAlphaFieldNames( Ptr ) + '=' + Alphas( Ptr ) + ", " + cNumericFieldNames( NumField ) + "=[" + RoundSigDigits( Numbers( NumField ), 5 ) + "]." );
							ShowContinueError( "Out-of-range from low/high limits=[>=" + RoundSigDigits( LowHConvLimit, 9 ) + ", <=" + RoundSigDigits( HighHConvLimit, 1 ) + "]." );
							//            CALL RangeCheck(errFlag,'"'//TRIM(cAlphaFieldNames(FieldNo+1))//'"','object',  &
							//                       'SEVERE','>='//TRIM(RoundSigDigits(LowHConvLimit,9)),(Numbers(NumField)>=LowHConvLimit),&
							//                       '<='//TRIM(RoundSigDigits(HighHConvLimit,1)),(Numbers(NumField)<=HighHConvLimit))
							ShowContinueError( "Limits are set (or default) in HeatBalanceAlgorithm object." );
							ErrorsFound = true;
							errFlag = false;
						}
						UserExtConvectionCoeffs( TotExtConvCoeff ).OverrideType = ConvCoefValue;
						UserExtConvectionCoeffs( TotExtConvCoeff ).OverrideValue = Numbers( NumField );
						if ( ! lAlphaFieldBlanks( Ptr + 2 ) ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + ", duplicate value" );
							ShowContinueError( "Since VALUE is used for \"" + cAlphaFieldNames( FieldNo + 2 ) + "\", " + cAlphaFieldNames( Ptr + 2 ) + '=' + Alphas( Ptr + 2 ) + " is ignored." );
						}
						PotentialAssignedValue = TotExtConvCoeff;
					} else if ( IsValidType && Loop1 == 2 ) { // Schedule
						++TotExtConvCoeff;
						UserExtConvectionCoeffs( TotExtConvCoeff ).SurfaceName = Alphas( 1 );
						UserExtConvectionCoeffs( TotExtConvCoeff ).WhichSurface = Found;
						UserExtConvectionCoeffs( TotExtConvCoeff ).OverrideType = ConvCoefSchedule;
						UserExtConvectionCoeffs( TotExtConvCoeff ).ScheduleIndex = GetScheduleIndex( Alphas( Ptr + 2 ) );
						if ( UserExtConvectionCoeffs( TotExtConvCoeff ).ScheduleIndex == 0 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + ", invalid value" );
							ShowContinueError( " Invalid " + cAlphaFieldNames( Ptr + 2 ) + " entered=" + Alphas( Ptr + 2 ) );
							ErrorsFound = true;
						} else {
							UserExtConvectionCoeffs( TotExtConvCoeff ).ScheduleName = Alphas( Ptr + 2 );
						}
						PotentialAssignedValue = TotExtConvCoeff;
					} else if ( IsValidType && ExtValue == HcExt_UserCurve ) { // User curve

						++TotExtConvCoeff;
						UserExtConvectionCoeffs( TotExtConvCoeff ).SurfaceName = Alphas( 1 );
						UserExtConvectionCoeffs( TotExtConvCoeff ).WhichSurface = Found;
						UserExtConvectionCoeffs( TotExtConvCoeff ).OverrideType = ConvCoefUserCurve;
						UserExtConvectionCoeffs( TotExtConvCoeff ).UserCurveIndex = FindItemInList( Alphas( Ptr + 3 ), HcOutsideUserCurve );
						if ( UserExtConvectionCoeffs( TotExtConvCoeff ).UserCurveIndex == 0 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + ", invalid value" );
							ShowContinueError( " Invalid " + cAlphaFieldNames( Ptr + 3 ) + " entered=" + Alphas( Ptr + 3 ) );
							ErrorsFound = true;
						}
						PotentialAssignedValue = TotExtConvCoeff;
					} else if ( IsValidType && ExtValue > HcExt_UserCurve ) {
						// specificmodel
						++TotExtConvCoeff;
						UserExtConvectionCoeffs( TotExtConvCoeff ).SurfaceName = Alphas( 1 );
						UserExtConvectionCoeffs( TotExtConvCoeff ).WhichSurface = Found;
						UserExtConvectionCoeffs( TotExtConvCoeff ).OverrideType = ConvCoefSpecifiedModel;
						UserExtConvectionCoeffs( TotExtConvCoeff ).HcModelEq = ExtValue;
						PotentialAssignedValue = TotExtConvCoeff;

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + ", check input" );
						ShowContinueError( "Check Input Entered :" + Alphas( Ptr + 1 ) );
						ErrorsFound = true;
					}
					if ( Surface( Found ).ExtConvCoeff != 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + ", invalid value" );
						ShowContinueError( "Duplicate (Outside) assignment attempt" );
						ErrorsFound = true;
					} else {
						Surface( Found ).ExtConvCoeff = PotentialAssignedValue;
					}

				} else if ( SELECT_CASE_var == "INSIDE" ) {
					IsValidType = false;
					IntValue = 0;
					PotentialAssignedValue = 0;
					for ( Loop1 = 1; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) {
						if ( Alphas( Ptr + 1 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
						IntValue = IntConvectionValue( Loop1 );
						IsValidType = true;
						break;
					}

					if ( IsValidType && ( Loop1 > 2 ) && ( Loop1 <= 5 ) ) {
						PotentialAssignedValue = -IntValue;
					} else if ( IsValidType && Loop1 == 1 ) { // Value
						++TotIntConvCoeff;
						UserIntConvectionCoeffs( TotIntConvCoeff ).SurfaceName = Alphas( 1 );
						UserIntConvectionCoeffs( TotIntConvCoeff ).WhichSurface = Found;
						if ( Numbers( NumField ) < LowHConvLimit || Numbers( NumField ) > HighHConvLimit ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + ", out of range value" );
							ShowContinueError( cAlphaFieldNames( Ptr ) + '=' + Alphas( Ptr ) + ", " + cNumericFieldNames( NumField ) + "=[" + RoundSigDigits( Numbers( NumField ), 5 ) + "]." );
							ShowContinueError( "Out-of-range from low/high limits=[>=" + RoundSigDigits( LowHConvLimit, 9 ) + ", <=" + RoundSigDigits( HighHConvLimit, 1 ) + "]." );
							//            CALL RangeCheck(errFlag,'"'//TRIM(cAlphaFieldNames(FieldNo+1))//'"','object',  &
							//                       'SEVERE','>='//TRIM(RoundSigDigits(LowHConvLimit,9)),(Numbers(NumField)>=LowHConvLimit),&
							//                       '<='//TRIM(RoundSigDigits(HighHConvLimit,1)),(Numbers(NumField)<=HighHConvLimit))
							ShowContinueError( "Limits are set (or default) in HeatBalanceAlgorithm object." );
							ErrorsFound = true;
							errFlag = false;
						}
						UserIntConvectionCoeffs( TotIntConvCoeff ).OverrideType = ConvCoefValue;
						UserIntConvectionCoeffs( TotIntConvCoeff ).OverrideValue = Numbers( NumField );
						if ( ! lAlphaFieldBlanks( Ptr + 2 ) ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + ", duplicate value" );
							ShowContinueError( "Since VALUE is used for \"" + cAlphaFieldNames( FieldNo + 1 ) + "\", " + cAlphaFieldNames( Ptr + 2 ) + '=' + Alphas( Ptr + 2 ) + " is ignored." );
						}
						PotentialAssignedValue = TotIntConvCoeff;
					} else if ( IsValidType && Loop1 == 2 ) { // Schedule
						++TotIntConvCoeff;
						UserIntConvectionCoeffs( TotIntConvCoeff ).SurfaceName = Alphas( 1 );
						UserIntConvectionCoeffs( TotIntConvCoeff ).WhichSurface = Found;
						UserIntConvectionCoeffs( TotIntConvCoeff ).OverrideType = ConvCoefSchedule;
						UserIntConvectionCoeffs( TotIntConvCoeff ).ScheduleIndex = GetScheduleIndex( Alphas( Ptr + 2 ) );
						if ( UserIntConvectionCoeffs( TotIntConvCoeff ).ScheduleIndex == 0 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + ", invalid value" );
							ShowContinueError( " Invalid " + cAlphaFieldNames( Ptr + 2 ) + " entered=" + Alphas( Ptr + 2 ) );
							ErrorsFound = true;
						} else {
							UserIntConvectionCoeffs( TotIntConvCoeff ).ScheduleName = Alphas( Ptr + 2 );
						}
						PotentialAssignedValue = TotIntConvCoeff;
					} else if ( IsValidType && IntValue == HcInt_UserCurve ) {
						++TotIntConvCoeff;
						UserIntConvectionCoeffs( TotIntConvCoeff ).SurfaceName = Alphas( 1 );
						UserIntConvectionCoeffs( TotIntConvCoeff ).WhichSurface = Found;
						UserIntConvectionCoeffs( TotIntConvCoeff ).OverrideType = ConvCoefUserCurve;
						UserIntConvectionCoeffs( TotIntConvCoeff ).UserCurveIndex = FindItemInList( Alphas( Ptr + 3 ), HcInsideUserCurve );
						if ( UserIntConvectionCoeffs( TotIntConvCoeff ).UserCurveIndex == 0 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + ", invalid value" );
							ShowContinueError( " Invalid " + cAlphaFieldNames( Ptr + 3 ) + " entered=" + Alphas( Ptr + 3 ) );
							ErrorsFound = true;
						}
						PotentialAssignedValue = TotIntConvCoeff;
					} else if ( IsValidType && IntValue > HcInt_UserCurve ) {
						// specificmodel
						++TotIntConvCoeff;
						UserIntConvectionCoeffs( TotIntConvCoeff ).SurfaceName = Alphas( 1 );
						UserIntConvectionCoeffs( TotIntConvCoeff ).WhichSurface = Found;
						UserIntConvectionCoeffs( TotIntConvCoeff ).OverrideType = ConvCoefSpecifiedModel;
						UserIntConvectionCoeffs( TotIntConvCoeff ).HcModelEq = IntValue;
						PotentialAssignedValue = TotIntConvCoeff;

					} else {
						// treat CeilingDiffuser and TrombeWall special
						if ( SameString( Alphas( Ptr + 1 ), "CEILINGDIFFUSER" ) || SameString( Alphas( Ptr + 1 ), "TROMBEWALL" ) ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + ", invalid value" );
							ShowContinueError( "Invalid Value Entered, for " + cAlphaFieldNames( Ptr ) + '=' + Alphas( Ptr ) );
							ShowContinueError( "invalid value in " + cAlphaFieldNames( Ptr + 1 ) + '=' + Alphas( Ptr + 1 ) + "\". This type is only applicable at a Zone level." );
							ErrorsFound = true;
						} else { // really invalid
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + ", invalid value" );
							ShowContinueError( "Invalid Value Entered, for " + cAlphaFieldNames( Ptr ) + '=' + Alphas( Ptr ) );
							ShowContinueError( "invalid value in " + cAlphaFieldNames( Ptr + 1 ) + '=' + Alphas( Ptr + 1 ) );
							ErrorsFound = true;
						}
					}
					if ( Surface( Found ).IntConvCoeff != 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + ", duplicate (inside)" );
						ShowContinueError( "Duplicate (Inside) assignment attempt." );
						ErrorsFound = true;
					} else {
						Surface( Found ).IntConvCoeff = PotentialAssignedValue;
					}

				} else if ( SELECT_CASE_var == BlankString ) { // Blank

				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + ", invalid value" );
					ShowContinueError( "Invalid Value Entered, for " + cAlphaFieldNames( Ptr ) + '=' + Alphas( Ptr ) );
					ErrorsFound = true;
				}}

				Ptr += 4;
				FieldNo += 4;
				++NumField;
			}
		}

		CurrentModuleObject = "SurfaceProperty:ConvectionCoefficients:MultipleSurface";
		Count = GetNumObjectsFound( CurrentModuleObject );
		for ( Loop = 1; Loop <= Count; ++Loop ) {
			GetObjectItem( CurrentModuleObject, Loop, Alphas, NumAlphas, Numbers, NumNumbers, Status, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			// Check Field 1 for validity
			IsValidType = false;
			for ( Loop1 = 1; Loop1 <= NumValidSurfaceTypes; ++Loop1 ) {
				if ( Alphas( 1 ) != ValidSurfaceTypes( Loop1 ) ) continue;
				IsValidType = true;
				break;
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + ", invalid value" );
				ShowContinueError( "illegal value for " + cAlphaFieldNames( 1 ) + '=' + Alphas( 1 ) );
				ErrorsFound = true;
				continue;
			}
			Ptr = 2;
			FieldNo = 2;
			NumField = 1;
			for ( Pass = 1; Pass <= 2; ++Pass ) {

				{ auto const SELECT_CASE_var( Alphas( Ptr ) );
				if ( SELECT_CASE_var == "OUTSIDE" ) {
					IsValidType = false;
					for ( Loop1 = 1; Loop1 <= NumValidExtConvectionValueTypes; ++Loop1 ) {
						if ( Alphas( Ptr + 1 ) != ValidExtConvectionValueTypes( Loop1 ) ) continue;
						ExtValue = ExtConvectionValue( Loop1 );
						IsValidType = true;
						break;
					}

					if ( IsValidType && ( Loop1 > 2 ) && ( Loop1 <= 7 ) ) {
						ApplyConvectionValue( Alphas( 1 ), "OUTSIDE", -ExtValue );
					} else if ( IsValidType && Loop1 == 1 ) { // Value
						// SimpleValueAssignment via UserExtConvectionCoeffs array
						++TotExtConvCoeff;
						UserExtConvectionCoeffs( TotExtConvCoeff ).SurfaceName = Alphas( Ptr );
						UserExtConvectionCoeffs( TotExtConvCoeff ).WhichSurface = -999;
						if ( Numbers( NumField ) < LowHConvLimit || Numbers( NumField ) > HighHConvLimit ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + ", out of range value" );
							ShowContinueError( cAlphaFieldNames( Ptr ) + '=' + Alphas( Ptr ) + ", " + cNumericFieldNames( NumField ) + "=[" + RoundSigDigits( Numbers( NumField ), 5 ) + "]." );
							ShowContinueError( "Out-of-range from low/high limits=[>=" + RoundSigDigits( LowHConvLimit, 9 ) + ", <=" + RoundSigDigits( HighHConvLimit, 1 ) + "]." );
							//            CALL RangeCheck(errFlag,'"'//TRIM(cAlphaFieldNames(FieldNo+1))//'"','object',  &
							//                       'SEVERE','>='//TRIM(RoundSigDigits(LowHConvLimit,9)),(Numbers(NumField)>=LowHConvLimit),&
							//                       '<='//TRIM(RoundSigDigits(HighHConvLimit,1)),(Numbers(NumField)<=HighHConvLimit))
							ShowContinueError( "Limits are set (or default) in HeatBalanceAlgorithm object." );
							ErrorsFound = true;
							errFlag = false;
						}
						UserExtConvectionCoeffs( TotExtConvCoeff ).OverrideType = ConvCoefValue;
						UserExtConvectionCoeffs( TotExtConvCoeff ).OverrideValue = Numbers( NumField );
						if ( ! lAlphaFieldBlanks( Ptr + 2 ) ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + ", duplicate value" );
							ShowContinueError( "Since VALUE is used for \"" + cAlphaFieldNames( FieldNo + 2 ) + "\", " + cAlphaFieldNames( Ptr + 2 ) + '=' + Alphas( Ptr + 2 ) + " is ignored." );
						}
						ApplyConvectionValue( Alphas( 1 ), "OUTSIDE", TotExtConvCoeff );
					} else if ( IsValidType && Loop1 == 2 ) { // Schedule
						++TotExtConvCoeff;
						UserExtConvectionCoeffs( TotExtConvCoeff ).SurfaceName = Alphas( Ptr );
						UserExtConvectionCoeffs( TotExtConvCoeff ).WhichSurface = -999;
						UserExtConvectionCoeffs( TotExtConvCoeff ).OverrideType = ConvCoefSchedule;
						UserExtConvectionCoeffs( TotExtConvCoeff ).ScheduleIndex = GetScheduleIndex( Alphas( Ptr + 2 ) );
						if ( UserExtConvectionCoeffs( TotExtConvCoeff ).ScheduleIndex == 0 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + ", invalid value" );
							ShowContinueError( " Invalid " + cAlphaFieldNames( Ptr + 2 ) + " entered=" + Alphas( Ptr + 2 ) );
							ErrorsFound = true;
						} else {
							UserExtConvectionCoeffs( TotExtConvCoeff ).ScheduleName = Alphas( Ptr + 2 );
						}
						ApplyConvectionValue( Alphas( 1 ), "OUTSIDE", TotExtConvCoeff );
					} else if ( IsValidType && ExtValue == HcExt_UserCurve ) { // User curve
						++TotExtConvCoeff;
						UserExtConvectionCoeffs( TotExtConvCoeff ).SurfaceName = Alphas( Ptr );
						UserExtConvectionCoeffs( TotExtConvCoeff ).WhichSurface = -999;
						UserExtConvectionCoeffs( TotExtConvCoeff ).OverrideType = ConvCoefUserCurve;
						UserExtConvectionCoeffs( TotExtConvCoeff ).UserCurveIndex = FindItemInList( Alphas( Ptr + 3 ), HcOutsideUserCurve );
						if ( UserExtConvectionCoeffs( TotExtConvCoeff ).UserCurveIndex == 0 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + ", invalid value" );
							ShowContinueError( " Invalid " + cAlphaFieldNames( Ptr + 3 ) + " entered=" + Alphas( Ptr + 3 ) );
							ErrorsFound = true;
						}
						PotentialAssignedValue = TotExtConvCoeff;
						ApplyConvectionValue( Alphas( 1 ), "OUTSIDE", TotExtConvCoeff );

					} else if ( IsValidType && ExtValue > HcExt_UserCurve ) {
						// specificmodel
						++TotExtConvCoeff;
						UserExtConvectionCoeffs( TotExtConvCoeff ).SurfaceName = Alphas( Ptr );
						UserExtConvectionCoeffs( TotExtConvCoeff ).WhichSurface = -999;
						UserExtConvectionCoeffs( TotExtConvCoeff ).OverrideType = ConvCoefSpecifiedModel;
						UserExtConvectionCoeffs( TotExtConvCoeff ).HcModelEq = ExtValue;
						PotentialAssignedValue = TotExtConvCoeff;
						ApplyConvectionValue( Alphas( 1 ), "OUTSIDE", TotExtConvCoeff );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + ", check input" );
						ShowContinueError( "Check Input Entered :" + Alphas( Ptr + 1 ) );
						ErrorsFound = true;
					}

				} else if ( SELECT_CASE_var == "INSIDE" ) {
					IsValidType = false;
					for ( Loop1 = 1; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) {
						if ( Alphas( Ptr + 1 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
						IntValue = IntConvectionValue( Loop1 );
						IsValidType = true;
						break;
					}

					if ( IsValidType && ( Loop1 > 2 ) && ( Loop1 <= 5 ) ) {
						ApplyConvectionValue( Alphas( 1 ), "INSIDE", -IntValue );
					} else if ( IsValidType && Loop1 == 1 ) { // Value
						// SimpleValueAssignment via UserExtConvectionCoeffs array
						++TotIntConvCoeff;
						UserIntConvectionCoeffs( TotIntConvCoeff ).SurfaceName = Alphas( Ptr );
						UserIntConvectionCoeffs( TotIntConvCoeff ).WhichSurface = -999;
						if ( Numbers( NumField ) < LowHConvLimit || Numbers( NumField ) > HighHConvLimit ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + ", out of range value" );
							ShowContinueError( cAlphaFieldNames( Ptr ) + '=' + Alphas( Ptr ) + ", " + cNumericFieldNames( NumField ) + "=[" + RoundSigDigits( Numbers( NumField ), 5 ) + "]." );
							ShowContinueError( "Out-of-range from low/high limits=[>=" + RoundSigDigits( LowHConvLimit, 9 ) + ", <=" + RoundSigDigits( HighHConvLimit, 1 ) + "]." );
							//            CALL RangeCheck(errFlag,'"'//TRIM(cAlphaFieldNames(FieldNo+1))//'"','object',  &
							//                       'SEVERE','>='//TRIM(RoundSigDigits(LowHConvLimit,9)),(Numbers(NumField)>=LowHConvLimit),&
							//                       '<='//TRIM(RoundSigDigits(HighHConvLimit,1)),(Numbers(NumField)<=HighHConvLimit))
							ShowContinueError( "Limits are set (or default) in HeatBalanceAlgorithm object." );
							ErrorsFound = true;
							errFlag = false;
						}
						UserIntConvectionCoeffs( TotIntConvCoeff ).OverrideType = ConvCoefValue;
						UserIntConvectionCoeffs( TotIntConvCoeff ).OverrideValue = Numbers( NumField );
						if ( ! lAlphaFieldBlanks( Ptr + 2 ) ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + ", duplicate value" );
							ShowContinueError( "Since VALUE is used for \"" + cAlphaFieldNames( FieldNo + 2 ) + "\", " + cAlphaFieldNames( Ptr + 2 ) + '=' + Alphas( Ptr + 2 ) + " is ignored." );
						}
						ApplyConvectionValue( Alphas( 1 ), "INSIDE", TotIntConvCoeff );
					} else if ( IsValidType && Loop1 == 2 ) { // Schedule
						++TotIntConvCoeff;
						UserIntConvectionCoeffs( TotIntConvCoeff ).SurfaceName = Alphas( Ptr );
						UserIntConvectionCoeffs( TotIntConvCoeff ).WhichSurface = -999;
						UserIntConvectionCoeffs( TotIntConvCoeff ).OverrideType = ConvCoefSchedule;
						UserIntConvectionCoeffs( TotIntConvCoeff ).ScheduleIndex = GetScheduleIndex( Alphas( Ptr + 2 ) );
						if ( UserIntConvectionCoeffs( TotIntConvCoeff ).ScheduleIndex == 0 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + ", invalid value" );
							ShowContinueError( " Invalid " + cAlphaFieldNames( Ptr + 2 ) + " entered=" + Alphas( Ptr + 2 ) );
							ErrorsFound = true;
						} else {
							UserIntConvectionCoeffs( TotIntConvCoeff ).ScheduleName = Alphas( Ptr + 2 );
						}
						ApplyConvectionValue( Alphas( 1 ), "INSIDE", TotIntConvCoeff );
					} else if ( IsValidType && IntValue == HcInt_UserCurve ) {
						++TotIntConvCoeff;
						UserIntConvectionCoeffs( TotIntConvCoeff ).SurfaceName = Alphas( Ptr );
						UserIntConvectionCoeffs( TotIntConvCoeff ).WhichSurface = -999;
						UserIntConvectionCoeffs( TotIntConvCoeff ).OverrideType = ConvCoefUserCurve;
						UserIntConvectionCoeffs( TotIntConvCoeff ).UserCurveIndex = FindItemInList( Alphas( Ptr + 3 ), HcInsideUserCurve );
						if ( UserIntConvectionCoeffs( TotIntConvCoeff ).UserCurveIndex == 0 ) {

							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + ", invalid value" );
							ShowContinueError( " Invalid " + cAlphaFieldNames( Ptr + 3 ) + " entered=" + Alphas( Ptr + 3 ) );
							ErrorsFound = true;
						}
						PotentialAssignedValue = TotIntConvCoeff;
						ApplyConvectionValue( Alphas( 1 ), "INSIDE", TotIntConvCoeff );
					} else if ( IsValidType && IntValue > HcInt_UserCurve ) {
						// specificmodel
						++TotIntConvCoeff;
						UserIntConvectionCoeffs( TotIntConvCoeff ).SurfaceName = Alphas( Ptr );
						UserIntConvectionCoeffs( TotIntConvCoeff ).WhichSurface = -999;
						UserIntConvectionCoeffs( TotIntConvCoeff ).OverrideType = ConvCoefSpecifiedModel;
						UserIntConvectionCoeffs( TotIntConvCoeff ).HcModelEq = IntValue;
						PotentialAssignedValue = TotIntConvCoeff;
						ApplyConvectionValue( Alphas( 1 ), "INSIDE", TotIntConvCoeff );

					} else {
						// treat CeilingDiffuser and TrombeWall special
						if ( SameString( Alphas( Ptr + 1 ), "CEILINGDIFFUSER" ) || SameString( Alphas( Ptr + 1 ), "TROMBEWALL" ) ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + ", invalid value" );
							ShowContinueError( " Invalid " + cAlphaFieldNames( Ptr ) + " entered=" + Alphas( Ptr ) );
							ShowContinueError( "invalid value in " + cAlphaFieldNames( Ptr + 1 ) + '=' + Alphas( Ptr + 1 ) + "\". This type is only applicable at a Zone level." );
							ErrorsFound = true;
						} else { // really invalid
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + ", invalid value" );
							ShowContinueError( " Invalid " + cAlphaFieldNames( Ptr + 1 ) + " entered=" + Alphas( Ptr + 1 ) );
							ErrorsFound = true;
						}
					}

				} else if ( SELECT_CASE_var == BlankString ) { // Blank

				} else { // Error Case
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + ", invalid value" );
					ShowContinueError( " Invalid " + cAlphaFieldNames( Ptr ) + " entered=" + Alphas( Ptr ) );
					ErrorsFound = true;
				}}

				Ptr += 4;
				FieldNo += 4;
				++NumField;
			}
		}

		for ( Loop = 1; Loop <= TotIntConvCoeff; ++Loop ) {
			if ( UserIntConvectionCoeffs( Loop ).OverrideType != ConvCoefSchedule ) continue;
			if ( UserIntConvectionCoeffs( Loop ).ScheduleIndex == 0 ) continue;
			if ( CheckScheduleValueMinMax( UserIntConvectionCoeffs( Loop ).ScheduleIndex, ">=", LowHConvLimit, "<=", HighHConvLimit ) ) continue;
			ShowSevereError( RoutineName + "Surface=\"" + UserIntConvectionCoeffs( Loop ).SurfaceName + "\", out-of-range convection coefficient:" );
			ShowContinueError( "Out-of-range value found in schedule=" + UserIntConvectionCoeffs( Loop ).ScheduleName );
			ShowContinueError( "User supplied convection coefficients must be in range [>=" + RoundSigDigits( LowHConvLimit, 9 ) + ", <=" + RoundSigDigits( HighHConvLimit, 1 ) + ']' );
			ShowContinueError( "Limits are set (or default) in HeatBalanceAlgorithm object." );
			ErrorsFound = true;
		}

		for ( Loop = 1; Loop <= TotExtConvCoeff; ++Loop ) {
			if ( UserExtConvectionCoeffs( Loop ).OverrideType != ConvCoefSchedule ) continue;
			if ( UserExtConvectionCoeffs( Loop ).ScheduleIndex == 0 ) continue;
			if ( CheckScheduleValueMinMax( UserExtConvectionCoeffs( Loop ).ScheduleIndex, ">=", LowHConvLimit, "<=", HighHConvLimit ) ) continue;
			ShowSevereError( RoutineName + "Surface=\"" + UserExtConvectionCoeffs( Loop ).SurfaceName + "\", out-of-range convection coefficient:" );
			ShowContinueError( "Out-of-range value found in schedule=" + UserExtConvectionCoeffs( Loop ).ScheduleName );
			ShowContinueError( "User supplied convection coefficients must be in range [>=" + RoundSigDigits( LowHConvLimit, 9 ) + ", <=" + RoundSigDigits( HighHConvLimit, 1 ) + ']' );
			ShowContinueError( "Limits are set (or default) in HeatBalanceAlgorithm object." );
			ErrorsFound = true;
		}

		if ( DefaultOutsideConvectionAlgo == ASHRAESimple || std::any_of( Zone.begin(), Zone.end(), []( DataHeatBalance::ZoneData const & e ){ return e.OutsideConvectionAlgo == DataHeatBalance::ASHRAESimple; } ) ) {
			Count = 0;
			for ( Loop = 1; Loop <= TotExtConvCoeff; ++Loop ) {
				SurfNum = UserExtConvectionCoeffs( Loop ).WhichSurface;
				// Tests show that Zone will override the simple convection specification of global.
				if ( SurfNum <= 0 ) continue; // ignore this error condition
				if ( Surface( SurfNum ).Zone == 0 ) continue; // ignore this error condition
				if ( Zone( Surface( SurfNum ).Zone ).OutsideConvectionAlgo == ASHRAESimple && ( ( UserExtConvectionCoeffs( Loop ).OverrideType == ConvCoefSpecifiedModel && UserExtConvectionCoeffs( Loop ).HcModelEq != ASHRAESimple ) || UserExtConvectionCoeffs( Loop ).OverrideType != ConvCoefSpecifiedModel ) ) {
					++Count;
					if ( DisplayExtraWarnings ) {
						ShowSevereError( RoutineName + "Surface=\"" + UserExtConvectionCoeffs( Loop ).SurfaceName + "\", mixed algorithms." );
						ShowContinueError( "Zone Outside Convection Algorithm specifies \"SimpleCombined\". SimpleCombined will be used for this surface." );
					}
				}
			}
			if ( Count > 0 ) {
				ShowSevereMessage( RoutineName + RoundSigDigits( Count ) + " surfaces had different outside convection algorithms specified when" );
				ShowContinueError( "the Zone Outside Convection Algorithm specifies \"SimpleCombined\". SimpleCombined will be used for these surfaces." );
				if ( ! DisplayExtraWarnings ) {
					ShowContinueError( "Use OutputDiagnostics,DisplayExtraWarnings; to see specific instances." );
					TotalSevereErrors += Count;
				}
			}
		}

		//get SurfaceConvectionAlgorithm:Inside:AdaptiveModelSelections

		CurrentModuleObject = "SurfaceConvectionAlgorithm:Inside:AdaptiveModelSelections";
		Count = GetNumObjectsFound( CurrentModuleObject );
		//IF (Count > 1) ! throw  error ... TODO or IP handles it
		if ( Count == 1 ) {
			GetObjectItem( CurrentModuleObject, 1, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, Status, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			InsideFaceAdaptiveConvectionAlgo.Name = cAlphaArgs( 1 ); //not used by E+, unique object
			InsideFaceAdaptiveConvectionAlgo.EnteredByUser = true;

			// A2 , \field Simple Bouyancy Vertical Wall Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 2 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.SimpleBouyVertWallEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.SimpleBouyVertWallEqNum == HcInt_UserCurve ) {
					// A3 , \field Simple Bouyancy Vertical Wall User Curve Name
					InsideFaceAdaptiveConvectionAlgo.SimpleBouyVertWallUserCurveNum = FindItemInList( cAlphaArgs( 3 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.SimpleBouyVertWallUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + ", invalid value" );
						ShowContinueError( " Invalid " + cAlphaFieldNames( 3 ) + " entered=" + cAlphaArgs( 3 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
				ErrorsFound = true;
			}

			// A4 , \field Simple Bouyancy Stable Horizontal Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 4 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.SimpleBouyStableHorizEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.SimpleBouyStableHorizEqNum == HcInt_UserCurve ) {
					// A5 , \field Simple Bouyancy Stable Horizontal Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.SimpleBouyStableHorizUserCurveNum = FindItemInList( cAlphaArgs( 5 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.SimpleBouyStableHorizUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 5 ) + '=' + cAlphaArgs( 5 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 4 ) + '=' + cAlphaArgs( 4 ) );
				ErrorsFound = true;
			}

			// A6 , \field Simple Bouyancy Unstable Horizontal Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 6 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.SimpleBouyUnstableHorizEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.SimpleBouyUnstableHorizEqNum == HcInt_UserCurve ) {
					// A7 , \field Simple Bouyancy Unstable Horizontal Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.SimpleBouyUnstableHorizUserCurveNum = FindItemInList( cAlphaArgs( 7 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.SimpleBouyUnstableHorizUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 7 ) + '=' + cAlphaArgs( 7 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 6 ) + '=' + cAlphaArgs( 6 ) );
				ErrorsFound = true;
			}

			// A8 , \field Simple Bouyancy Stable Tilted Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 8 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.SimpleBouyStableTiltedEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.SimpleBouyStableTiltedEqNum == HcInt_UserCurve ) {
					// A9 , \field Simple Bouyancy Stable Tilted Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.SimpleBouyStableTiltedUserCurveNum = FindItemInList( cAlphaArgs( 9 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.SimpleBouyStableTiltedUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 9 ) + '=' + cAlphaArgs( 9 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 8 ) + '=' + cAlphaArgs( 8 ) );
				ErrorsFound = true;
			}

			// A10 , \field Simple Bouyancy Unstable Tilted Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 10 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.SimpleBouyUnstableTiltedEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.SimpleBouyUnstableTiltedEqNum == HcInt_UserCurve ) {
					// A11, \field Simple Bouyancy Unstable Tilted Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.SimpleBouyUnstableTiltedUserCurveNum = FindItemInList( cAlphaArgs( 11 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.SimpleBouyUnstableTiltedUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 11 ) + '=' + cAlphaArgs( 11 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 10 ) + '=' + cAlphaArgs( 10 ) );
				ErrorsFound = true;
			}

			// A12, \field Simple Bouyancy Windows Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 12 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.SimpleBouyWindowsEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.SimpleBouyWindowsEqNum == HcInt_UserCurve ) {
					// A13, \field Simple Bouyancy Windows Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.SimpleBouyWindowsUserCurveNum = FindItemInList( cAlphaArgs( 13 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.SimpleBouyWindowsUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 13 ) + '=' + cAlphaArgs( 13 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 12 ) + '=' + cAlphaArgs( 12 ) );
				ErrorsFound = true;
			}

			// A14, \field Floor Heat Ceiling Cool Vertical Wall Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 14 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolVertWallEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolVertWallEqNum == HcInt_UserCurve ) {
					//  A15, \field Floor Heat Ceiling Cool Vertical Wall Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolVertWallUserCurveNum = FindItemInList( cAlphaArgs( 15 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolVertWallUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 15 ) + '=' + cAlphaArgs( 15 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 14 ) + '=' + cAlphaArgs( 14 ) );
				ErrorsFound = true;
			}

			// A16, \field Floor Heat Ceiling Cool Stable Horizontal Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 16 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolStableHorizEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolStableHorizEqNum == HcInt_UserCurve ) {
					//  A17, \field Floor Heat Ceiling Cool Stable Horizontal Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolStableHorizUserCurveNum = FindItemInList( cAlphaArgs( 17 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolStableHorizUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 17 ) + '=' + cAlphaArgs( 17 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 16 ) + '=' + cAlphaArgs( 16 ) );
				ErrorsFound = true;
			}

			// A18, \field Floor Heat Ceiling Cool Unstable Horizontal Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 18 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolUnstableHorizEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolUnstableHorizEqNum == HcInt_UserCurve ) {
					// A19, \field Floor Heat Ceiling Cool Unstable Horizontal Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolUnstableHorizUserCurveNum = FindItemInList( cAlphaArgs( 19 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolUnstableHorizUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 19 ) + '=' + cAlphaArgs( 19 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 18 ) + '=' + cAlphaArgs( 18 ) );
				ErrorsFound = true;
			}

			// A20, \field Floor Heat Ceiling Cool Heated Floor Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 20 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolHeatedFloorEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolHeatedFloorEqNum == HcInt_UserCurve ) {
					// A21, \field Floor Heat Ceiling Cool Heated Floor Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolHeatedFloorUserCurveNum = FindItemInList( cAlphaArgs( 21 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolHeatedFloorUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 21 ) + '=' + cAlphaArgs( 21 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 20 ) + '=' + cAlphaArgs( 20 ) );
				ErrorsFound = true;
			}

			// A22, \field Floor Heat Ceiling Cool Chilled Ceiling Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 22 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolChilledCeilingEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolChilledCeilingEqNum == HcInt_UserCurve ) {
					// A23, \field Floor Heat Ceiling Cool Chilled Ceiling Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolChilledCeilingUserCurveNum = FindItemInList( cAlphaArgs( 23 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolChilledCeilingUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 23 ) + '=' + cAlphaArgs( 23 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 22 ) + '=' + cAlphaArgs( 22 ) );
				ErrorsFound = true;
			}

			// A24, \field Floor Heat Ceiling Cool Stable Tilted Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 24 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolStableTiltedEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolStableTiltedEqNum == HcInt_UserCurve ) {
					//   A25, \field Floor Heat Ceiling Cool Stable Tilted Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolStableTiltedUserCurveNum = FindItemInList( cAlphaArgs( 25 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolStableTiltedUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 25 ) + '=' + cAlphaArgs( 25 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 24 ) + '=' + cAlphaArgs( 24 ) );
				ErrorsFound = true;
			}

			// A26, \field Floor Heat Ceiling Cool Unstable Tilted Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 26 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolUnstableTiltedEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolUnstableTiltedEqNum == HcInt_UserCurve ) {
					//   A27, \field Floor Heat Ceiling Cool Unstable Tilted Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolUnstableTiltedUserCurveNum = FindItemInList( cAlphaArgs( 27 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolUnstableTiltedUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 27 ) + '=' + cAlphaArgs( 27 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 26 ) + '=' + cAlphaArgs( 26 ) );
				ErrorsFound = true;
			}

			// A28, \field Floor Heat Ceiling Cool Window Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 28 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolWindowsEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolWindowsEqNum == HcInt_UserCurve ) {
					//    A29, \field Floor Heat Ceiling Cool Window Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolWindowsUserCurveNum = FindItemInList( cAlphaArgs( 29 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolWindowsUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 29 ) + '=' + cAlphaArgs( 29 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 28 ) + '=' + cAlphaArgs( 28 ) );
				ErrorsFound = true;
			}

			// A30, \field Wall Panel Heating Vertical Wall Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 30 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.WallPanelHeatVertWallEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.WallPanelHeatVertWallEqNum == HcInt_UserCurve ) {
					//    A31, \field Wall Panel Heating Vertical Wall Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.WallPanelHeatVertWallUserCurveNum = FindItemInList( cAlphaArgs( 31 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.WallPanelHeatVertWallUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 31 ) + '=' + cAlphaArgs( 31 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 30 ) + '=' + cAlphaArgs( 30 ) );
				ErrorsFound = true;
			}

			//  A32, \field Wall Panel Heating Heated Wall Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 32 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.WallPanelHeatHeatedWallEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.WallPanelHeatHeatedWallEqNum == HcInt_UserCurve ) {
					//   A33, \field Wall Panel Heating Heated Wall Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.WallPanelHeatHeatedWallUserCurveNum = FindItemInList( cAlphaArgs( 33 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.WallPanelHeatHeatedWallUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 33 ) + '=' + cAlphaArgs( 33 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 32 ) + '=' + cAlphaArgs( 32 ) );
				ErrorsFound = true;
			}

			//  A34, \field Wall Panel Heating Stable Horizontal Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 34 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.WallPanelHeatStableHorizEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.WallPanelHeatStableHorizEqNum == HcInt_UserCurve ) {
					//   A35, \field Wall Panel Heating Stable Horizontal Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.WallPanelHeatStableHorizUserCurveNum = FindItemInList( cAlphaArgs( 35 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.WallPanelHeatStableHorizUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 35 ) + '=' + cAlphaArgs( 35 ) );
						ErrorsFound = true;

					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 34 ) + '=' + cAlphaArgs( 34 ) );
				ErrorsFound = true;
			}

			// A36, \field Wall Panel Heating Unstable Horizontal Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 36 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.WallPanelHeatUnstableHorizEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.WallPanelHeatUnstableHorizEqNum == HcInt_UserCurve ) {
					//  A37, \field Wall Panel Heating Unstable Horizontal Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.WallPanelHeatUnstableHorizUserCurveNum = FindItemInList( cAlphaArgs( 37 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.WallPanelHeatUnstableHorizUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 37 ) + '=' + cAlphaArgs( 37 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 36 ) + '=' + cAlphaArgs( 36 ) );
				ErrorsFound = true;
			}

			// A38, \field Wall Panel Heating Stable Tilted Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 38 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.WallPanelHeatStableTiltedEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.WallPanelHeatStableTiltedEqNum == HcInt_UserCurve ) {
					//  A39, \field Wall Panel Heating Stable Tilted Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.WallPanelHeatStableTiltedUserCurveNum = FindItemInList( cAlphaArgs( 39 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.WallPanelHeatStableTiltedUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 39 ) + '=' + cAlphaArgs( 39 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 38 ) + '=' + cAlphaArgs( 38 ) );
				ErrorsFound = true;
			}

			//   A40, \field Wall Panel Heating Unstable Tilted Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 40 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.WallPanelHeatUnstableTiltedEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.WallPanelHeatUnstableTiltedEqNum == HcInt_UserCurve ) {
					//  A41, \field Wall Panel Heating Unstable Tilted Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.WallPanelHeatUnstableTiltedUserCurveNum = FindItemInList( cAlphaArgs( 41 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.WallPanelHeatUnstableTiltedUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 41 ) + '=' + cAlphaArgs( 41 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 40 ) + '=' + cAlphaArgs( 40 ) );
				ErrorsFound = true;
			}

			//  A42, \field Wall Panel Heating Window Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 42 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.WallPanelHeatWindowsEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.WallPanelHeatWindowsEqNum == HcInt_UserCurve ) {
					//  A43, \field Wall Panel Heating Window Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.WallPanelHeatWindowsUserCurveNum = FindItemInList( cAlphaArgs( 43 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.WallPanelHeatWindowsUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 43 ) + '=' + cAlphaArgs( 43 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 42 ) + '=' + cAlphaArgs( 42 ) );
				ErrorsFound = true;
			}

			//  A44, \field Convective Zone Heater Vertical Wall Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 44 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatVertWallEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatVertWallEqNum == HcInt_UserCurve ) {
					// A45, \field Convective Zone Heater Vertical Wall Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatVertWallUserCurveNum = FindItemInList( cAlphaArgs( 45 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatVertWallUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 45 ) + '=' + cAlphaArgs( 45 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 44 ) + '=' + cAlphaArgs( 44 ) );
				ErrorsFound = true;
			}

			//  A46, \field Convective Zone Heater Vertical Walls Near Heater Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 46 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatVertWallNearHeaterEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatVertWallNearHeaterEqNum == HcInt_UserCurve ) {
					// A47, \field Convective Zone Heater Vertical Walls Near Heater Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatVertWallNearHeaterUserCurveNum = FindItemInList( cAlphaArgs( 47 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatVertWallNearHeaterUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 47 ) + '=' + cAlphaArgs( 47 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 46 ) + '=' + cAlphaArgs( 46 ) );
				ErrorsFound = true;
			}

			//  A48, \field Convective Zone Heater Stable Horizontal Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 48 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatStableHorizEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatStableHorizEqNum == HcInt_UserCurve ) {
					// A49, \field Convective Zone Heater Stable Horizontal Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatStableHorizUserCurveNum = FindItemInList( cAlphaArgs( 49 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatStableHorizUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 49 ) + '=' + cAlphaArgs( 49 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 48 ) + '=' + cAlphaArgs( 48 ) );
				ErrorsFound = true;
			}

			//  A50, \field Convective Zone Heater Unstable Horizontal Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 50 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatUnstableHorizEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatUnstableHorizEqNum == HcInt_UserCurve ) {
					//  A51, \field Convective Zone Heater Unstable Horizontal Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatUnstableHorizUserCurveNum = FindItemInList( cAlphaArgs( 51 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatUnstableHorizUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 51 ) + '=' + cAlphaArgs( 51 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 50 ) + '=' + cAlphaArgs( 50 ) );
				ErrorsFound = true;
			}

			//  A52, \field Convective Zone Heater Stable Tilted Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 52 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatStableTiltedEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatStableTiltedEqNum == HcInt_UserCurve ) {
					//  A53, \field Convective Zone Heater Stable Tilted Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatStableTiltedUserCurveNum = FindItemInList( cAlphaArgs( 53 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatStableTiltedUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 53 ) + '=' + cAlphaArgs( 53 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 52 ) + '=' + cAlphaArgs( 52 ) );
				ErrorsFound = true;
			}

			//  A54, \field Convective Zone Heater Unstable Tilted Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 54 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatUnstableTiltedEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatUnstableTiltedEqNum == HcInt_UserCurve ) {
					//  A55, \field Convective Zone Heater Unstable Tilted Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatUnstableTiltedUserCurveNum = FindItemInList( cAlphaArgs( 55 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatUnstableTiltedUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 55 ) + '=' + cAlphaArgs( 55 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 54 ) + '=' + cAlphaArgs( 54 ) );
				ErrorsFound = true;
			}

			//  A56, \field Convective Zone Heater Windows Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 56 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatWindowsEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatWindowsEqNum == HcInt_UserCurve ) {
					//   A57, \field Convective Zone Heater Windows Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatWindowsUserCurveNum = FindItemInList( cAlphaArgs( 57 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatWindowsUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 57 ) + '=' + cAlphaArgs( 57 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 56 ) + '=' + cAlphaArgs( 56 ) );
				ErrorsFound = true;
			}

			//  A58, \field Central Air Diffuser Wall Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 58 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.CentralAirWallEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.CentralAirWallEqNum == HcInt_UserCurve ) {
					//   A59, \field Central Air Diffuser Wall Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.CentralAirWallUserCurveNum = FindItemInList( cAlphaArgs( 59 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.CentralAirWallUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 59 ) + '=' + cAlphaArgs( 59 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 58 ) + '=' + cAlphaArgs( 58 ) );
				ErrorsFound = true;
			}

			//   A60, \field Central Air Diffuser Ceiling Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 60 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.CentralAirCeilingEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.CentralAirCeilingEqNum == HcInt_UserCurve ) {
					//   A61, \field Central Air Diffuser Ceiling Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.CentralAirCeilingUserCurveNum = FindItemInList( cAlphaArgs( 61 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.CentralAirCeilingUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 61 ) + '=' + cAlphaArgs( 61 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 60 ) + '=' + cAlphaArgs( 60 ) );
				ErrorsFound = true;
			}

			//  A62, \field Central Air Diffuser Floor Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 62 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.CentralAirFloorEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.CentralAirFloorEqNum == HcInt_UserCurve ) {
					//  A63, \field Central Air Diffuser Floor Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.CentralAirFloorUserCurveNum = FindItemInList( cAlphaArgs( 63 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.CentralAirFloorUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 63 ) + '=' + cAlphaArgs( 63 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 62 ) + '=' + cAlphaArgs( 62 ) );
				ErrorsFound = true;
			}

			//  A64, \field Central Air Diffuser Window Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 64 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.CentralAirWindowsEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.CentralAirWindowsEqNum == HcInt_UserCurve ) {
					//   A65, \field Central Air Diffuser Window Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.CentralAirWindowsUserCurveNum = FindItemInList( cAlphaArgs( 65 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.CentralAirWindowsUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 65 ) + '=' + cAlphaArgs( 65 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 64 ) + '=' + cAlphaArgs( 64 ) );
				ErrorsFound = true;
			}

			// A66, \field Mechanical Zone Fan Circulation Vertical Wall Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 66 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.ZoneFanCircVertWallEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.ZoneFanCircVertWallEqNum == HcInt_UserCurve ) {
					//   A67, \field Mechanical Zone Fan Circulation Vertical Wall Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.ZoneFanCircVertWallUserCurveNum = FindItemInList( cAlphaArgs( 67 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.ZoneFanCircVertWallUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 67 ) + '=' + cAlphaArgs( 67 ) );
						ErrorsFound = true;

					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 66 ) + '=' + cAlphaArgs( 66 ) );
				ErrorsFound = true;
			}

			// A68, \field Mechanical Zone Fan Circulation Stable Horizontal Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 68 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.ZoneFanCircStableHorizEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.ZoneFanCircStableHorizEqNum == HcInt_UserCurve ) {
					//   A69, \field Mechanical Zone Fan Circulation Stable Horizontal Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.ZoneFanCircStableHorizUserCurveNum = FindItemInList( cAlphaArgs( 69 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.ZoneFanCircStableHorizUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 69 ) + '=' + cAlphaArgs( 69 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 68 ) + '=' + cAlphaArgs( 68 ) );
				ErrorsFound = true;
			}

			// A70, \field Mechanical Zone Fan Circulation Unstable Horizontal Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 70 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.ZoneFanCircUnstableHorizEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.ZoneFanCircUnstableHorizEqNum == HcInt_UserCurve ) {
					//   A71, \field Mechanical Zone Fan Circulation Unstable Horizontal Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.ZoneFanCircUnstableHorizUserCurveNum = FindItemInList( cAlphaArgs( 71 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.ZoneFanCircUnstableHorizUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 71 ) + '=' + cAlphaArgs( 71 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 70 ) + '=' + cAlphaArgs( 70 ) );
				ErrorsFound = true;
			}

			// A72, \field Mechanical Zone Fan Circulation Stable Tilted Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 72 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.ZoneFanCircStableTiltedEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.ZoneFanCircStableTiltedEqNum == HcInt_UserCurve ) {
					//  A73, \field Mechanical Zone Fan Circulation Stable Tilted Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.ZoneFanCircStableTiltedUserCurveNum = FindItemInList( cAlphaArgs( 73 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.ZoneFanCircStableTiltedUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 73 ) + '=' + cAlphaArgs( 73 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 72 ) + '=' + cAlphaArgs( 72 ) );
				ErrorsFound = true;
			}

			// A74, \field Mechanical Zone Fan Circulation Unstable Tilted Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 74 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.ZoneFanCircUnstableTiltedEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.ZoneFanCircUnstableTiltedEqNum == HcInt_UserCurve ) {
					//  A75, \field Mechanical Zone Fan Circulation Unstable Tilted Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.ZoneFanCircUnstableTiltedUserCurveNum = FindItemInList( cAlphaArgs( 75 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.ZoneFanCircUnstableTiltedUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 75 ) + '=' + cAlphaArgs( 75 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 74 ) + '=' + cAlphaArgs( 74 ) );
				ErrorsFound = true;
			}

			// A76, \field Mechanical Zone Fan Circulation Window Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 76 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.ZoneFanCircWindowsEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.ZoneFanCircWindowsEqNum == HcInt_UserCurve ) {
					//  A77, \field Mechanical Zone Fan Circulation Window Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.ZoneFanCircWindowsUserCurveNum = FindItemInList( cAlphaArgs( 77 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.ZoneFanCircWindowsUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 77 ) + '=' + cAlphaArgs( 77 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 76 ) + '=' + cAlphaArgs( 76 ) );
				ErrorsFound = true;
			}

			// A78, \field Mixed Regime Bouyancy Assisting Flow on Walls Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 78 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.MixedBouyAssistingFlowWallEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.MixedBouyAssistingFlowWallEqNum == HcInt_UserCurve ) {
					//  A79, \field Mixed Regime Bouyancy Assisting Flow on Walls Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.MixedBouyAssistingFlowWallUserCurveNum = FindItemInList( cAlphaArgs( 79 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.MixedBouyAssistingFlowWallUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 79 ) + '=' + cAlphaArgs( 79 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 78 ) + '=' + cAlphaArgs( 78 ) );
				ErrorsFound = true;
			}

			// A80, \field Mixed Regime Bouyancy Oppossing Flow on Walls Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 80 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.MixedBouyOppossingFlowWallEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.MixedBouyOppossingFlowWallEqNum == HcInt_UserCurve ) {
					//  A81, \field Mixed Regime Bouyancy Oppossing Flow on Walls Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.MixedBouyOppossingFlowWallUserCurveNum = FindItemInList( cAlphaArgs( 81 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.MixedBouyOppossingFlowWallUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 81 ) + '=' + cAlphaArgs( 81 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 80 ) + '=' + cAlphaArgs( 80 ) );
				ErrorsFound = true;
			}

			// A82, \field Mixed Regime Stable Floor Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 82 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.MixedStableFloorEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.MixedStableFloorEqNum == HcInt_UserCurve ) {
					//  A83, \field Mixed Regime Stable Floor Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.MixedStableFloorUserCurveNum = FindItemInList( cAlphaArgs( 83 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.MixedStableFloorUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 83 ) + '=' + cAlphaArgs( 83 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 82 ) + '=' + cAlphaArgs( 82 ) );
				ErrorsFound = true;
			}

			// A84, \field Mixed Regime Unstable Floor Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 84 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.MixedUnstableFloorEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.MixedUnstableFloorEqNum == HcInt_UserCurve ) {
					//  A85, \field Mixed Regime Unstable Floor Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.MixedUnstableFloorUserCurveNum = FindItemInList( cAlphaArgs( 85 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.MixedUnstableFloorUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 85 ) + '=' + cAlphaArgs( 85 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 84 ) + '=' + cAlphaArgs( 84 ) );
				ErrorsFound = true;
			}

			// A86, \field Mixed Regime Stable Ceiling Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 86 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.MixedStableCeilingEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.MixedStableCeilingEqNum == HcInt_UserCurve ) {
					//  A87, \field Mixed Regime Stable Ceiling Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.MixedStableCeilingUserCurveNum = FindItemInList( cAlphaArgs( 87 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.MixedStableCeilingUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 87 ) + '=' + cAlphaArgs( 87 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 86 ) + '=' + cAlphaArgs( 86 ) );
				ErrorsFound = true;
			}

			// A88, \field Mixed Regime Unstable Ceiling Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 88 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.MixedUnstableCeilingEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.MixedUnstableCeilingEqNum == HcInt_UserCurve ) {
					//  A89, \field Mixed Regime Unstable Ceiling Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.MixedUnstableCeilingUserCurveNum = FindItemInList( cAlphaArgs( 89 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.MixedUnstableCeilingUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 89 ) + '=' + cAlphaArgs( 89 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 88 ) + '=' + cAlphaArgs( 88 ) );
				ErrorsFound = true;
			}

			// A90, \field Mixed Regime Window Equation Source
			IsValidType = false;
			for ( Loop1 = 6; Loop1 <= NumValidIntConvectionValueTypes; ++Loop1 ) { //skipping first 5 whole-model types
				if ( cAlphaArgs( 90 ) != ValidIntConvectionValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				InsideFaceAdaptiveConvectionAlgo.MixedWindowsEqNum = IntConvectionValue( Loop1 );
				if ( InsideFaceAdaptiveConvectionAlgo.MixedWindowsEqNum == HcInt_UserCurve ) {
					//   A91; \field Mixed Regime Window Equation User Curve Name
					InsideFaceAdaptiveConvectionAlgo.MixedWindowsUserCurveNum = FindItemInList( cAlphaArgs( 91 ), HcInsideUserCurve );
					if ( InsideFaceAdaptiveConvectionAlgo.MixedWindowsUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 91 ) + '=' + cAlphaArgs( 91 ) );
						ErrorsFound = true;
					}
				}
				break; //found it
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 90 ) + '=' + cAlphaArgs( 90 ) );
				ErrorsFound = true;
			}

		} //end of 'SurfaceConvectionAlgorithm:Inside:AdaptiveModelSelections'

		CurrentModuleObject = "SurfaceConvectionAlgorithm:Outside:AdaptiveModelSelections";
		Count = GetNumObjectsFound( CurrentModuleObject );
		//IF (Count > 1) ! throw  error ... TODO or IP handles it
		if ( Count == 1 ) {
			GetObjectItem( CurrentModuleObject, 1, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, Status, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			OutsideFaceAdaptiveConvectionAlgo.Name = cAlphaArgs( 1 ); //not used by E+, unique object
			OutsideFaceAdaptiveConvectionAlgo.EnteredByUser = true;

			// A2 , \field Wind Convection Windward Vertical Wall Equation Source
			IsValidType = false;
			for ( Loop1 = 2; Loop1 <= NumValidSpecificExtWindConvValueTypes; ++Loop1 ) {
				if ( cAlphaArgs( 2 ) != ValidSpecificExtWindConvValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				OutsideFaceAdaptiveConvectionAlgo.HWindWallWindwardEqNum = MoreSpecificExtWindConvectionValue( Loop1 );
				if ( OutsideFaceAdaptiveConvectionAlgo.HWindWallWindwardEqNum == HcExt_UserCurve ) {
					//  A3 , \field Wind Convection Windward Equation Vertical Wall User Curve Name
					OutsideFaceAdaptiveConvectionAlgo.HWindWallWindwardUserCurveNum = FindItemInList( cAlphaArgs( 3 ), HcOutsideUserCurve );
					if ( OutsideFaceAdaptiveConvectionAlgo.HWindWallWindwardUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 3 ) + '=' + cAlphaArgs( 3 ) );
						ErrorsFound = true;
					}
				}
				break;
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
				ErrorsFound = true;
			}

			// A4 , \field Wind Convection Leeward Vertical Wall Equation Source
			IsValidType = false;
			for ( Loop1 = 2; Loop1 <= NumValidSpecificExtWindConvValueTypes; ++Loop1 ) {
				if ( cAlphaArgs( 4 ) != ValidSpecificExtWindConvValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				OutsideFaceAdaptiveConvectionAlgo.HWindWallLeewardEqNum = MoreSpecificExtWindConvectionValue( Loop1 );
				if ( OutsideFaceAdaptiveConvectionAlgo.HWindWallLeewardEqNum == HcExt_UserCurve ) {
					// A5 , \field Wind Convection Leeward Vertical Wall Equation User Curve Name
					OutsideFaceAdaptiveConvectionAlgo.HWindWallLeewardUserCurveNum = FindItemInList( cAlphaArgs( 5 ), HcOutsideUserCurve );
					if ( OutsideFaceAdaptiveConvectionAlgo.HWindWallLeewardUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 5 ) + '=' + cAlphaArgs( 5 ) );
						ErrorsFound = true;
					}
				}
				break;
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 4 ) + '=' + cAlphaArgs( 4 ) );
				ErrorsFound = true;
			}

			// A6 , \field Wind Convection Horizontal Roof Equation Source
			IsValidType = false;
			for ( Loop1 = 1; Loop1 <= NumValidSpecificExtWindConvValueTypes; ++Loop1 ) {
				if ( cAlphaArgs( 6 ) != ValidSpecificExtWindConvValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				OutsideFaceAdaptiveConvectionAlgo.HWindHorizRoofEqNum = MoreSpecificExtWindConvectionValue( Loop1 );
				if ( OutsideFaceAdaptiveConvectionAlgo.HWindHorizRoofEqNum == HcExt_UserCurve ) {
					//  A7 , \field Wind Convection Horizontal Roof User Curve Name
					OutsideFaceAdaptiveConvectionAlgo.HWindHorizRoofUserCurveNum = FindItemInList( cAlphaArgs( 7 ), HcOutsideUserCurve );
					if ( OutsideFaceAdaptiveConvectionAlgo.HWindHorizRoofUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 7 ) + '=' + cAlphaArgs( 7 ) );
						ErrorsFound = true;
					}
				}
				break;
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 6 ) + '=' + cAlphaArgs( 6 ) );
				ErrorsFound = true;
			}

			//  A8 , \field Natural Convection Vertical Wall Equation Source
			IsValidType = false;
			for ( Loop1 = 1; Loop1 <= NumValidSpecificExtNatConvectValueTypes; ++Loop1 ) {
				if ( cAlphaArgs( 8 ) != ValidSpecificExtNatConvectValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				OutsideFaceAdaptiveConvectionAlgo.HNatVertWallEqNum = SpecificExtNatConvectionValue( Loop1 );
				if ( OutsideFaceAdaptiveConvectionAlgo.HNatVertWallEqNum == HcExt_UserCurve ) {
					//  A9 , \field Natural Convection Vertical Wall Equation User Curve Name
					OutsideFaceAdaptiveConvectionAlgo.HNatVertWallUserCurveNum = FindItemInList( cAlphaArgs( 9 ), HcOutsideUserCurve );
					if ( OutsideFaceAdaptiveConvectionAlgo.HNatVertWallUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 9 ) + '=' + cAlphaArgs( 9 ) );
						ErrorsFound = true;
					}
				}
				break;
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 8 ) + '=' + cAlphaArgs( 8 ) );
				ErrorsFound = true;
			}

			//  A10, \field Natural Convection Stable Horizontal Equation Source
			IsValidType = false;
			for ( Loop1 = 1; Loop1 <= NumValidSpecificExtNatConvectValueTypes; ++Loop1 ) {
				if ( cAlphaArgs( 10 ) != ValidSpecificExtNatConvectValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				OutsideFaceAdaptiveConvectionAlgo.HNatStableHorizEqNum = SpecificExtNatConvectionValue( Loop1 );
				if ( OutsideFaceAdaptiveConvectionAlgo.HNatStableHorizEqNum == HcExt_UserCurve ) {
					//  A11, \field Natural Convection Stable Horizontal Equation User Curve Name
					OutsideFaceAdaptiveConvectionAlgo.HNatStableHorizUserCurveNum = FindItemInList( cAlphaArgs( 11 ), HcOutsideUserCurve );
					if ( OutsideFaceAdaptiveConvectionAlgo.HNatStableHorizUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 11 ) + '=' + cAlphaArgs( 11 ) );
						ErrorsFound = true;
					}
				}
				break;
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 10 ) + '=' + cAlphaArgs( 10 ) );
				ErrorsFound = true;
			}

			//   A12, \field Natural Convection Unstable Horizontal Equation Source
			IsValidType = false;
			for ( Loop1 = 1; Loop1 <= NumValidSpecificExtNatConvectValueTypes; ++Loop1 ) {
				if ( cAlphaArgs( 12 ) != ValidSpecificExtNatConvectValueTypes( Loop1 ) ) continue;
				IsValidType = true;
				OutsideFaceAdaptiveConvectionAlgo.HNatStableHorizEqNum = SpecificExtNatConvectionValue( Loop1 );
				if ( OutsideFaceAdaptiveConvectionAlgo.HNatStableHorizEqNum == HcExt_UserCurve ) {
					// A13; \field Natural Convection Unstable Horizontal Equation User Curve Name
					OutsideFaceAdaptiveConvectionAlgo.HNatStableHorizUserCurveNum = FindItemInList( cAlphaArgs( 13 ), HcOutsideUserCurve );
					if ( OutsideFaceAdaptiveConvectionAlgo.HNatStableHorizUserCurveNum == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
						ShowContinueError( "Invalid Name choice Entered, for " + cAlphaFieldNames( 13 ) + '=' + cAlphaArgs( 13 ) );
						ErrorsFound = true;
					}
				}
				break;
			}
			if ( ! IsValidType ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", invalid value" );
				ShowContinueError( "Invalid Key choice Entered, for " + cAlphaFieldNames( 12 ) + '=' + cAlphaArgs( 12 ) );
				ErrorsFound = true;
			}

		} // end of 'SurfaceConvectionAlgorithm:Outside:AdaptiveModelSelections'

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found getting input.  Program termination." );
		}

		SetupAdaptiveConvectionStaticMetaData();

	}

	void
	ApplyConvectionValue(
		std::string const & SurfaceTypes,
		std::string const & ConvectionType,
		int const Value
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   November 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine applies a convection type to a set of surfaces.  This is
		// one of the "regular" convection types and becomes a "negative" convection
		// type to that surface.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SurfNum;
		bool SurfacesOfType;
		int SurfaceCountOutside;
		int SurfaceCountInside;
		std::string OverwriteMessage;

		{ auto const SELECT_CASE_var( SurfaceTypes );

		if ( SELECT_CASE_var == "ALLEXTERIORSURFACES" ) {
			SurfacesOfType = false;
			SurfaceCountOutside = 0;
			SurfaceCountInside = 0;
			for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
				if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
				if ( Surface( SurfNum ).ExtBoundCond > 0 ) continue; // Interior surfaces
				SurfacesOfType = true;
				if ( ConvectionType == "OUTSIDE" ) {
					if ( Surface( SurfNum ).OSCPtr > 0 ) continue;
					if ( Surface( SurfNum ).ExtConvCoeff != 0 ) {
						if ( DisplayExtraWarnings ) {
							ShowWarningError( "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes + "\", not overwriting already assigned value for (Outside) in Surface=" + Surface( SurfNum ).Name );
						} else {
							++SurfaceCountOutside;
						}
					} else {
						Surface( SurfNum ).ExtConvCoeff = Value;
					}
				} else {
					if ( Surface( SurfNum ).IntConvCoeff != 0 ) {
						if ( DisplayExtraWarnings ) {
							ShowWarningError( "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes + "\", not overwriting already assigned value for (Inside) in Surface=" + Surface( SurfNum ).Name );
						} else {
							++SurfaceCountInside;
						}
					} else {
						Surface( SurfNum ).IntConvCoeff = Value;
					}
				}
			}
			if ( ! DisplayExtraWarnings && ( SurfaceCountOutside > 0 || SurfaceCountInside > 0 ) ) {
				if ( SurfaceCountOutside > 0 ) {
					OverwriteMessage = TrimSigDigits( SurfaceCountOutside ) + " Outside";
				}
				if ( SurfaceCountInside > 0 ) {
					OverwriteMessage = TrimSigDigits( SurfaceCountInside ) + " Inside";
				}
				ShowWarningError( "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes + "\", not overwriting already assigned values for " + OverwriteMessage + " assignments." );
			}

		} else if ( SELECT_CASE_var == "ALLEXTERIORWINDOWS" ) {
			SurfacesOfType = false;
			SurfaceCountOutside = 0;
			SurfaceCountInside = 0;
			for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
				if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
				if ( Surface( SurfNum ).ExtBoundCond > 0 ) continue; // Interior surfaces
				if ( ! Construct( Surface( SurfNum ).Construction ).TypeIsWindow ) continue;
				SurfacesOfType = true;
				if ( ConvectionType == "OUTSIDE" ) {
					if ( Surface( SurfNum ).OSCPtr > 0 ) continue;
					if ( Surface( SurfNum ).ExtConvCoeff != 0 ) {
						if ( DisplayExtraWarnings ) {
							ShowWarningError( "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes + "\", not overwriting already assigned value for (Outside) in Surface=" + Surface( SurfNum ).Name );
						} else {
							++SurfaceCountOutside;
						}
					} else {
						Surface( SurfNum ).ExtConvCoeff = Value;
					}
				} else {
					if ( Surface( SurfNum ).IntConvCoeff != 0 ) {
						if ( DisplayExtraWarnings ) {
							ShowWarningError( "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes + "\", not overwriting already assigned value for (Inside) in Surface=" + Surface( SurfNum ).Name );
						} else {
							++SurfaceCountInside;
						}
					} else {
						Surface( SurfNum ).IntConvCoeff = Value;
					}
				}
			}
			if ( ! DisplayExtraWarnings && ( SurfaceCountOutside > 0 || SurfaceCountInside > 0 ) ) {
				if ( SurfaceCountOutside > 0 ) {
					OverwriteMessage = TrimSigDigits( SurfaceCountOutside ) + " Outside";
				}
				if ( SurfaceCountInside > 0 ) {
					OverwriteMessage = TrimSigDigits( SurfaceCountInside ) + " Inside";
				}
				ShowWarningError( "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes + "\", not overwriting already assigned values for " + OverwriteMessage + " assignments." );
			}

		} else if ( SELECT_CASE_var == "ALLEXTERIORWALLS" ) {
			SurfacesOfType = false;
			SurfaceCountOutside = 0;
			SurfaceCountInside = 0;
			for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
				if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
				if ( Surface( SurfNum ).ExtBoundCond > 0 ) continue; // Interior surfaces
				if ( Surface( SurfNum ).Class != SurfaceClass_Wall ) continue;
				SurfacesOfType = true;
				if ( ConvectionType == "OUTSIDE" ) {
					if ( Surface( SurfNum ).OSCPtr > 0 ) continue;
					if ( Surface( SurfNum ).ExtConvCoeff != 0 ) {
						if ( DisplayExtraWarnings ) {
							ShowWarningError( "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes + "\", not overwriting already assigned value for (Outside) in Surface=" + Surface( SurfNum ).Name );
						} else {
							++SurfaceCountOutside;
						}
					} else {
						Surface( SurfNum ).ExtConvCoeff = Value;
					}
				} else {
					if ( Surface( SurfNum ).IntConvCoeff != 0 ) {
						if ( DisplayExtraWarnings ) {
							ShowWarningError( "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes + "\", not overwriting already assigned value for (Inside) in Surface=" + Surface( SurfNum ).Name );
						} else {
							++SurfaceCountInside;
						}
					} else {
						Surface( SurfNum ).IntConvCoeff = Value;
					}
				}
			}
			if ( ! DisplayExtraWarnings && ( SurfaceCountOutside > 0 || SurfaceCountInside > 0 ) ) {
				if ( SurfaceCountOutside > 0 ) {
					OverwriteMessage = TrimSigDigits( SurfaceCountOutside ) + " Outside";
				}
				if ( SurfaceCountInside > 0 ) {
					OverwriteMessage = TrimSigDigits( SurfaceCountInside ) + " Inside";
				}
				ShowWarningError( "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes + "\", not overwriting already assigned values for " + OverwriteMessage + " assignments." );
			}

		} else if ( SELECT_CASE_var == "ALLEXTERIORROOFS" ) {
			SurfacesOfType = false;
			SurfaceCountOutside = 0;
			SurfaceCountInside = 0;
			for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
				if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
				if ( Surface( SurfNum ).ExtBoundCond > 0 ) continue; // Interior surfaces
				if ( Surface( SurfNum ).Class != SurfaceClass_Roof ) continue;
				SurfacesOfType = true;
				if ( ConvectionType == "OUTSIDE" ) {
					if ( Surface( SurfNum ).OSCPtr > 0 ) continue;
					if ( Surface( SurfNum ).ExtConvCoeff != 0 ) {
						if ( DisplayExtraWarnings ) {
							ShowWarningError( "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes + "\", not overwriting already assigned value for (Outside) in Surface=" + Surface( SurfNum ).Name );
						} else {
							++SurfaceCountOutside;
						}
					} else {
						Surface( SurfNum ).ExtConvCoeff = Value;
					}
				} else {
					if ( Surface( SurfNum ).IntConvCoeff != 0 ) {
						if ( DisplayExtraWarnings ) {
							ShowWarningError( "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes + "\", not overwriting already assigned value for (Inside) in Surface=" + Surface( SurfNum ).Name );
						} else {
							++SurfaceCountInside;
						}
					} else {
						Surface( SurfNum ).IntConvCoeff = Value;
					}
				}
			}
			if ( ! DisplayExtraWarnings && ( SurfaceCountOutside > 0 || SurfaceCountInside > 0 ) ) {
				if ( SurfaceCountOutside > 0 ) {
					OverwriteMessage = TrimSigDigits( SurfaceCountOutside ) + " Outside";
				}
				if ( SurfaceCountInside > 0 ) {
					OverwriteMessage = TrimSigDigits( SurfaceCountInside ) + " Inside";
				}
				ShowWarningError( "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes + "\", not overwriting already assigned values for " + OverwriteMessage + " assignments." );
			}

		} else if ( SELECT_CASE_var == "ALLEXTERIORFLOORS" ) {
			SurfacesOfType = false;
			SurfaceCountOutside = 0;
			SurfaceCountInside = 0;
			for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
				if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
				if ( Surface( SurfNum ).ExtBoundCond > 0 ) continue; // Interior surfaces
				if ( Surface( SurfNum ).Class != SurfaceClass_Floor ) continue;
				SurfacesOfType = true;
				if ( ConvectionType == "OUTSIDE" ) {
					if ( Surface( SurfNum ).OSCPtr > 0 ) continue;
					if ( Surface( SurfNum ).ExtConvCoeff != 0 ) {
						if ( DisplayExtraWarnings ) {
							ShowWarningError( "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes + "\", not overwriting already assigned value for (Outside) in Surface=" + Surface( SurfNum ).Name );
						} else {
							++SurfaceCountOutside;
						}
					} else {
						Surface( SurfNum ).ExtConvCoeff = Value;
					}
				} else {
					if ( Surface( SurfNum ).IntConvCoeff != 0 ) {
						if ( DisplayExtraWarnings ) {
							ShowWarningError( "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes + "\", not overwriting already assigned value for (Inside) in Surface=" + Surface( SurfNum ).Name );
						} else {
							++SurfaceCountInside;
						}
					} else {
						Surface( SurfNum ).IntConvCoeff = Value;
					}
				}
			}
			if ( ! DisplayExtraWarnings && ( SurfaceCountOutside > 0 || SurfaceCountInside > 0 ) ) {
				if ( SurfaceCountOutside > 0 ) {
					OverwriteMessage = TrimSigDigits( SurfaceCountOutside ) + " Outside";
				}
				if ( SurfaceCountInside > 0 ) {
					OverwriteMessage = TrimSigDigits( SurfaceCountInside ) + " Inside";
				}
				ShowWarningError( "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes + "\", not overwriting already assigned values for " + OverwriteMessage + " assignments." );
			}

		} else if ( SELECT_CASE_var == "ALLINTERIORSURFACES" ) {
			SurfacesOfType = false;
			SurfaceCountOutside = 0;
			SurfaceCountInside = 0;
			for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
				if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
				if ( Surface( SurfNum ).ExtBoundCond <= 0 ) continue; // Exterior surfaces
				SurfacesOfType = true;
				if ( ConvectionType == "OUTSIDE" ) {
					if ( Surface( SurfNum ).OSCPtr > 0 ) continue;
					if ( Surface( SurfNum ).ExtConvCoeff != 0 ) {
						if ( DisplayExtraWarnings ) {
							ShowWarningError( "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes + "\", not overwriting already assigned value for (Outside) in Surface=" + Surface( SurfNum ).Name );
						} else {
							++SurfaceCountOutside;
						}
					} else {
						Surface( SurfNum ).ExtConvCoeff = Value;
					}
				} else {
					if ( Surface( SurfNum ).IntConvCoeff != 0 ) {
						if ( DisplayExtraWarnings ) {
							ShowWarningError( "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes + "\", not overwriting already assigned value for (Inside) in Surface=" + Surface( SurfNum ).Name );
						} else {
							++SurfaceCountInside;
						}
					} else {
						Surface( SurfNum ).IntConvCoeff = Value;
					}
				}
			}
			if ( ! DisplayExtraWarnings && ( SurfaceCountOutside > 0 || SurfaceCountInside > 0 ) ) {
				if ( SurfaceCountOutside > 0 ) {
					OverwriteMessage = TrimSigDigits( SurfaceCountOutside ) + " Outside";
				}
				if ( SurfaceCountInside > 0 ) {
					OverwriteMessage = TrimSigDigits( SurfaceCountInside ) + " Inside";
				}
				ShowWarningError( "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes + "\", not overwriting already assigned values for " + OverwriteMessage + " assignments." );
			}

		} else if ( SELECT_CASE_var == "ALLINTERIORWINDOWS" ) {
			SurfacesOfType = false;
			SurfaceCountOutside = 0;
			SurfaceCountInside = 0;
			for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
				if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
				if ( Surface( SurfNum ).ExtBoundCond <= 0 ) continue; // Exterior surfaces
				if ( ! Construct( Surface( SurfNum ).Construction ).TypeIsWindow ) continue;
				SurfacesOfType = true;
				if ( ConvectionType == "OUTSIDE" ) {
					if ( Surface( SurfNum ).OSCPtr > 0 ) continue;
					if ( Surface( SurfNum ).ExtConvCoeff != 0 ) {
						if ( DisplayExtraWarnings ) {
							ShowWarningError( "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes + "\", not overwriting already assigned value for (Outside) in Surface=" + Surface( SurfNum ).Name );
						} else {
							++SurfaceCountOutside;
						}
					} else {
						Surface( SurfNum ).ExtConvCoeff = Value;
					}
				} else {
					if ( Surface( SurfNum ).IntConvCoeff != 0 ) {
						if ( DisplayExtraWarnings ) {
							ShowWarningError( "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes + "\", not overwriting already assigned value for (Inside) in Surface=" + Surface( SurfNum ).Name );
						} else {
							++SurfaceCountInside;
						}
					} else {
						Surface( SurfNum ).IntConvCoeff = Value;
					}
				}
			}
			if ( ! DisplayExtraWarnings && ( SurfaceCountOutside > 0 || SurfaceCountInside > 0 ) ) {
				if ( SurfaceCountOutside > 0 ) {
					OverwriteMessage = TrimSigDigits( SurfaceCountOutside ) + " Outside";
				}
				if ( SurfaceCountInside > 0 ) {
					OverwriteMessage = TrimSigDigits( SurfaceCountInside ) + " Inside";
				}
				ShowWarningError( "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes + "\", not overwriting already assigned values for " + OverwriteMessage + " assignments." );
			}

		} else if ( SELECT_CASE_var == "ALLINTERIORWALLS" ) {
			SurfacesOfType = false;
			SurfaceCountOutside = 0;
			SurfaceCountInside = 0;
			for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
				if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
				if ( Surface( SurfNum ).ExtBoundCond <= 0 ) continue; // Exterior surfaces
				if ( Surface( SurfNum ).Class != SurfaceClass_Wall ) continue;
				SurfacesOfType = true;
				if ( ConvectionType == "OUTSIDE" ) {
					if ( Surface( SurfNum ).OSCPtr > 0 ) continue;
					if ( Surface( SurfNum ).ExtConvCoeff != 0 ) {
						if ( DisplayExtraWarnings ) {
							ShowWarningError( "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes + "\", not overwriting already assigned value for (Outside) in Surface=" + Surface( SurfNum ).Name );
						} else {
							++SurfaceCountOutside;
						}
					} else {
						Surface( SurfNum ).ExtConvCoeff = Value;
					}
				} else {
					if ( Surface( SurfNum ).IntConvCoeff != 0 ) {
						if ( DisplayExtraWarnings ) {
							ShowWarningError( "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes + "\", not overwriting already assigned value for (Inside) in Surface=" + Surface( SurfNum ).Name );
						} else {
							++SurfaceCountInside;
						}
					} else {
						Surface( SurfNum ).IntConvCoeff = Value;
					}
				}
			}
			if ( ! DisplayExtraWarnings && ( SurfaceCountOutside > 0 || SurfaceCountInside > 0 ) ) {
				if ( SurfaceCountOutside > 0 ) {
					OverwriteMessage = TrimSigDigits( SurfaceCountOutside ) + " Outside";
				}
				if ( SurfaceCountInside > 0 ) {
					OverwriteMessage = TrimSigDigits( SurfaceCountInside ) + " Inside";
				}
				ShowWarningError( "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes + "\", not overwriting already assigned values for " + OverwriteMessage + " assignments." );
			}

		} else if ( ( SELECT_CASE_var == "ALLINTERIORROOFS" ) || ( SELECT_CASE_var == "ALLINTERIORCEILINGS" ) ) {
			SurfacesOfType = false;
			SurfaceCountOutside = 0;
			SurfaceCountInside = 0;
			for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
				if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
				if ( Surface( SurfNum ).ExtBoundCond <= 0 ) continue; // Exterior surfaces
				if ( Surface( SurfNum ).Class != SurfaceClass_Roof ) continue;
				SurfacesOfType = true;
				if ( ConvectionType == "OUTSIDE" ) {
					if ( Surface( SurfNum ).OSCPtr > 0 ) continue;
					if ( Surface( SurfNum ).ExtConvCoeff != 0 ) {
						if ( DisplayExtraWarnings ) {
							ShowWarningError( "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes + "\", not overwriting already assigned value for (Outside) in Surface=" + Surface( SurfNum ).Name );
						} else {
							++SurfaceCountOutside;
						}
					} else {
						Surface( SurfNum ).ExtConvCoeff = Value;
					}
				} else {
					if ( Surface( SurfNum ).IntConvCoeff != 0 ) {
						if ( DisplayExtraWarnings ) {
							ShowWarningError( "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes + "\", not overwriting already assigned value for (Inside) in Surface=" + Surface( SurfNum ).Name );
						} else {
							++SurfaceCountInside;
						}
					} else {
						Surface( SurfNum ).IntConvCoeff = Value;
					}
				}
			}
			if ( ! DisplayExtraWarnings && ( SurfaceCountOutside > 0 || SurfaceCountInside > 0 ) ) {
				if ( SurfaceCountOutside > 0 ) {
					OverwriteMessage = TrimSigDigits( SurfaceCountOutside ) + " Outside";
				}
				if ( SurfaceCountInside > 0 ) {
					OverwriteMessage = TrimSigDigits( SurfaceCountInside ) + " Inside";
				}
				ShowWarningError( "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes + "\", not overwriting already assigned values for " + OverwriteMessage + " assignments." );
			}

		} else if ( SELECT_CASE_var == "ALLINTERIORFLOORS" ) {
			SurfacesOfType = false;
			SurfaceCountOutside = 0;
			SurfaceCountInside = 0;
			for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
				if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
				if ( Surface( SurfNum ).ExtBoundCond <= 0 ) continue; // Exterior surfaces
				if ( Surface( SurfNum ).Class != SurfaceClass_Floor ) continue;
				SurfacesOfType = true;
				if ( ConvectionType == "OUTSIDE" ) {
					if ( Surface( SurfNum ).OSCPtr > 0 ) continue;
					if ( Surface( SurfNum ).ExtConvCoeff != 0 ) {
						if ( DisplayExtraWarnings ) {
							ShowWarningError( "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes + "\", not overwriting already assigned value for (Outside) in Surface=" + Surface( SurfNum ).Name );
						} else {
							++SurfaceCountOutside;
						}
					} else {
						Surface( SurfNum ).ExtConvCoeff = Value;
					}
				} else {
					if ( Surface( SurfNum ).IntConvCoeff != 0 ) {
						if ( DisplayExtraWarnings ) {
							ShowWarningError( "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes + "\", not overwriting already assigned value for (Inside) in Surface=" + Surface( SurfNum ).Name );
						} else {
							++SurfaceCountInside;
						}
					} else {
						Surface( SurfNum ).IntConvCoeff = Value;
					}
				}
			}
			if ( ! DisplayExtraWarnings && ( SurfaceCountOutside > 0 || SurfaceCountInside > 0 ) ) {
				if ( SurfaceCountOutside > 0 ) {
					OverwriteMessage = TrimSigDigits( SurfaceCountOutside ) + " Outside";
				}
				if ( SurfaceCountInside > 0 ) {
					OverwriteMessage = TrimSigDigits( SurfaceCountInside ) + " Inside";
				}
				ShowWarningError( "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes + "\", not overwriting already assigned values for " + OverwriteMessage + " assignments." );
			}

		} else {
			SurfacesOfType = false;

		}}

		if ( ! SurfacesOfType ) {
			ShowWarningError( "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes + "\", there were no surfaces of that type found for " + ConvectionType + " assignment." );
		}

	}

	Real64
	CalcASHRAESimpExtConvectCoeff(
		int const Roughness, // Integer index for roughness, relates to parameter array indices
		Real64 const SurfWindSpeed // Current wind speed, m/s
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   August 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This subroutine calculates the exterior convection coefficient
		// using the ASHRAE Simple Method from a correlation from Figure 1
		// on p. 22.4 of the 1989 ASHRAE Handbook of Fundamentals.
		// This is a combined coefficient that includes radiation to sky, ground, and air.

		// METHODOLOGY EMPLOYED:
		// Apply the correlation based on the input data.

		// REFERENCES:
		// ASHRAE Handbook of Fundamentals 1989, p.22.4

		// USE STATEMENTS:
		// na

		// Return value
		Real64 CalcASHRAESimpExtConvectCoeff;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		static Array1D< Real64 > const D( 6, { 11.58, 12.49, 10.79, 8.23, 10.22, 8.23 } );
		static Array1D< Real64 > const E( 6, { 5.894, 4.065, 4.192, 4.00, 3.100, 3.33 } );
		static Array1D< Real64 > const F( 6, { 0.0, 0.028, 0.0, -0.057, 0.0, -0.036 } );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		// FLOW:
		CalcASHRAESimpExtConvectCoeff = D( Roughness ) + E( Roughness ) * SurfWindSpeed + F( Roughness ) * pow_2( SurfWindSpeed );

		return CalcASHRAESimpExtConvectCoeff;

	}

	void
	CalcASHRAESimpleIntConvCoeff(
		int const SurfNum, // surface number for which coefficients are being calculated
		Real64 const SurfaceTemperature, // Temperature of surface for evaluation of HcIn
		Real64 const ZoneMeanAirTemperature // Mean Air Temperature of Zone
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   August 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This subroutine calculates the interior convection coefficient for a surface.

		// METHODOLOGY EMPLOYED:
		// The convection coefficients are taken directly from the TARP Reference Manual.  TARP calculated
		// its coefficients using the surface conductances for e=0.9 found in ASHRAE Handbook of Fundamentals
		// 1985 in Table 1 on p. 23.2, but subtracted off the radiative component which was estimated at
		// 1.02 * 0.9 = 0.918 BTU/h-ft2-F.  Coefficients were then converted to SI units to yield the values
		// in this subroutine.

		// REFERENCES:
		// 1.  Walton, G. N. 1983. Thermal Analysis Research Program (TARP) Reference Manual,
		//     NBSSIR 83-2655, National Bureau of Standards, "Surface Inside Heat Balances", pp 79.
		// 2.  ASHRAE Handbook of Fundamentals 1985, p. 23.2, Table 1.

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
		Real64 DeltaTemp; // Temperature difference between the zone air and the surface

		if ( std::abs( Surface( SurfNum ).CosTilt ) >= 0.3827 ) { // Recalculate HConvIn

			DeltaTemp = ZoneMeanAirTemperature - SurfaceTemperature;

			// Set HConvIn using the proper correlation based on DeltaTemp and Cosine of the Tilt of the Surface
			if ( std::abs( Surface( SurfNum ).CosTilt ) >= 0.9239 ) { // Horizontal Surface

				if ( DeltaTemp * Surface( SurfNum ).CosTilt < 0.0 ) { // Horizontal, Reduced Convection

					HConvIn( SurfNum ) = 0.948;

				} else if ( DeltaTemp * Surface( SurfNum ).CosTilt == 0.0 ) { // Vertical Surface

					HConvIn( SurfNum ) = 3.076;

				} else if ( DeltaTemp * Surface( SurfNum ).CosTilt > 0.0 ) { // Horizontal, Enhanced Convection

					HConvIn( SurfNum ) = 4.040;

				}

			} else { // Tilted Surface

				if ( DeltaTemp * Surface( SurfNum ).CosTilt < 0.0 ) { // Tilted, Reduced Convection

					HConvIn( SurfNum ) = 2.281;

				} else if ( DeltaTemp * Surface( SurfNum ).CosTilt == 0.0 ) { // Vertical Surface

					HConvIn( SurfNum ) = 3.076;

				} else if ( DeltaTemp * Surface( SurfNum ).CosTilt > 0.0 ) { // Tilted, Enhanced Convection

					HConvIn( SurfNum ) = 3.870;

				}

			} // ...end of correlation selection IF-THEN block

		} // ...end of HConvIn recalculation IF-THEN block

		// Establish some lower limit to avoid a zero convection coefficient (and potential divide by zero problems)
		if ( HConvIn( SurfNum ) < LowHConvLimit ) HConvIn( SurfNum ) = LowHConvLimit;

	}

	void
	CalcASHRAEDetailedIntConvCoeff(
		int const SurfNum, // surface number for which coefficients are being calculated
		Real64 const SurfaceTemperature, // Temperature of surface for evaluation of HcIn
		Real64 const ZoneMeanAirTemperature // Mean Air Temperature of Zone
	)
	{

		//SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   August 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This subroutine calculates the interior convection coefficient for a surface.

		// METHODOLOGY EMPLOYED:
		// The algorithm for convection coefficients is taken directly from the TARP Reference Manual.
		// ASHRAE Handbook of Fundamentals 2001, p. 3.12, Table 5 gives equations for natural convection
		// heat transfer coefficients in the turbulent range for large, vertical plates and for large,
		// horizontal plates facing upward when heated (or downward when cooled).  A note in the text
		// also gives an approximation for large, horizontal places facing downward when heated (or
		// upward when cooled) recommending that it should be half of the facing upward value.
		// TARP then adds a curve fit as a function of the cosine of the tilt angle to provide intermediate
		// values between vertical and horizontal.  The curve fit values at the extremes match the ASHRAE
		// values very well.

		// REFERENCES:
		// 1.  Walton, G. N. 1983. Thermal Analysis Research Program (TARP) Reference Manual,
		//     NBSSIR 83-2655, National Bureau of Standards, "Surface Inside Heat Balances", pp 79-80.
		// 2.  ASHRAE Handbook of Fundamentals 2001, p. 3.12, Table 5.

		// USE STATEMENTS:
		// na

		// Locals

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 DeltaTemp; // Temperature difference between the zone air and the surface

		// FLOW:

		DeltaTemp = SurfaceTemperature - ZoneMeanAirTemperature;

		// Set HConvIn using the proper correlation based on DeltaTemp and Surface (Cosine Tilt)

		if ( ( DeltaTemp == 0.0 ) || ( Surface( SurfNum ).CosTilt == 0.0 ) ) { // Vertical Surface

			HConvIn( SurfNum ) = CalcASHRAEVerticalWall( DeltaTemp );

		} else if ( ( ( DeltaTemp < 0.0 ) && ( Surface( SurfNum ).CosTilt > 0.0 ) ) || ( ( DeltaTemp > 0.0 ) && ( Surface( SurfNum ).CosTilt < 0.0 ) ) ) { // Enhanced Convection

			HConvIn( SurfNum ) = CalcWaltonUnstableHorizontalOrTilt( DeltaTemp, Surface( SurfNum ).CosTilt );

		} else if ( ( ( DeltaTemp > 0.0 ) && ( Surface( SurfNum ).CosTilt > 0.0 ) ) || ( ( DeltaTemp < 0.0 ) && ( Surface( SurfNum ).CosTilt < 0.0 ) ) ) { // Reduced Convection

			HConvIn( SurfNum ) = CalcWaltonStableHorizontalOrTilt( DeltaTemp, Surface( SurfNum ).CosTilt );

		} // ...end of IF-THEN block to set HConvIn

		// Establish some lower limit to avoid a zero convection coefficient (and potential divide by zero problems)
		if ( HConvIn( SurfNum ) < LowHConvLimit ) HConvIn( SurfNum ) = LowHConvLimit;

	}

	void
	CalcDetailedHcInForDVModel(
		int const SurfNum, // surface number for which coefficients are being calculated
		Array1S< Real64 > const SurfaceTemperatures, // Temperature of surfaces for evaluation of HcIn
		Array1S< Real64 > HcIn, // Interior Convection Coeff Array
		Optional< Array1S< Real64 > const > Vhc // Velocity array for forced convection coeff calculation
	)
	{

		//SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   August 2000
		//       MODIFIED       Used for DV model; Feb 2004, LKL
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This subroutine calculates the interior convection coefficient for a surface.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHeatBalFanSys::MAT;
		using DataRoomAirModel::AirModel;
		using DataRoomAirModel::RoomAirModel_UCSDDV;
		using DataRoomAirModel::RoomAirModel_UCSDCV;
		using DataRoomAirModel::RoomAirModel_UCSDUFI;
		using DataRoomAirModel::RoomAirModel_UCSDUFE;

		// Argument array dimensioning

		// Locals
		Real64 const OneThird( ( 1.0 / 3.0 ) ); // 1/3 in highest precision

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 DeltaTemp; // Temperature difference between the zone air and the surface
		Real64 TAirConv;
		Real64 Hf;

		// FLOW:

		if ( Surface( SurfNum ).HeatTransSurf ) { // Only treat heat transfer surfaces

			// UCSD
			{ auto const SELECT_CASE_var( Surface( SurfNum ).TAirRef );
			if ( SELECT_CASE_var == AdjacentAirTemp ) {
				TAirConv = TempEffBulkAir( SurfNum );
			} else {
				// currently set to mean air temp but should add error warning here
				TAirConv = MAT( Surface( SurfNum ).Zone );
			}}
			DeltaTemp = SurfaceTemperatures( SurfNum ) - TAirConv;

			if ( AirModel( Surface( SurfNum ).Zone ).AirModelType == RoomAirModel_UCSDDV || AirModel( Surface( SurfNum ).Zone ).AirModelType == RoomAirModel_UCSDUFI || AirModel( Surface( SurfNum ).Zone ).AirModelType == RoomAirModel_UCSDUFE ) {

				// Set HConvIn using the proper correlation based on DeltaTemp and CosTiltSurf
				if ( Surface( SurfNum ).IntConvCoeff != 0 ) {

					HcIn( SurfNum ) = SetIntConvectionCoeff( SurfNum );

				} else if ( ( DeltaTemp == 0.0 ) || ( Surface( SurfNum ).CosTilt == 0.0 ) ) { // Vertical Surface

					HcIn( SurfNum ) = 1.31 * std::pow( std::abs( DeltaTemp ), OneThird );

				} else if ( ( ( DeltaTemp < 0.0 ) && ( Surface( SurfNum ).CosTilt > 0.0 ) ) || ( ( DeltaTemp > 0.0 ) && ( Surface( SurfNum ).CosTilt < 0.0 ) ) ) { // Enhanced Convection

					HcIn( SurfNum ) = 9.482 * std::pow( std::abs( DeltaTemp ), OneThird ) / ( 7.283 - std::abs( Surface( SurfNum ).CosTilt ) );

				} else if ( ( ( DeltaTemp > 0.0 ) && ( Surface( SurfNum ).CosTilt > 0.0 ) ) || ( ( DeltaTemp < 0.0 ) && ( Surface( SurfNum ).CosTilt < 0.0 ) ) ) { // Reduced Convection

					HcIn( SurfNum ) = 1.810 * std::pow( std::abs( DeltaTemp ), OneThird ) / ( 1.382 + std::abs( Surface( SurfNum ).CosTilt ) );

				} // ...end of IF-THEN block to set HConvIn

			} else if ( AirModel( Surface( SurfNum ).Zone ).AirModelType == RoomAirModel_UCSDCV ) {

				Hf = 4.3 * Vhc()( Surface( SurfNum ).Zone );

				// Set HConvIn using the proper correlation based on DeltaTemp and CosTiltSurf
				if ( Surface( SurfNum ).IntConvCoeff != 0 ) {

					HcIn( SurfNum ) = SetIntConvectionCoeff( SurfNum );

				} else if ( ( DeltaTemp == 0.0 ) || ( Surface( SurfNum ).CosTilt == 0.0 ) ) { // Vertical Surface

					HcIn( SurfNum ) = 1.31 * std::pow( std::abs( DeltaTemp ), OneThird );

					HcIn( SurfNum ) = std::pow( std::pow( HcIn( SurfNum ), 3.2 ) + std::pow( Hf, 3.2 ), 1.0 / 3.2 );

				} else if ( ( ( DeltaTemp < 0.0 ) && ( Surface( SurfNum ).CosTilt > 0.0 ) ) || ( ( DeltaTemp > 0.0 ) && ( Surface( SurfNum ).CosTilt < 0.0 ) ) ) { // Enhanced Convection

					HcIn( SurfNum ) = 9.482 * std::pow( std::abs( DeltaTemp ), 1.0 / 3.0 ) / ( 7.283 - std::abs( Surface( SurfNum ).CosTilt ) );
					HcIn( SurfNum ) = std::pow( std::pow( HcIn( SurfNum ), 3.2 ) + std::pow( Hf, 3.2 ), 1.0 / 3.2 );

				} else if ( ( ( DeltaTemp > 0.0 ) && ( Surface( SurfNum ).CosTilt > 0.0 ) ) || ( ( DeltaTemp < 0.0 ) && ( Surface( SurfNum ).CosTilt < 0.0 ) ) ) { // Reduced Convection

					HcIn( SurfNum ) = 1.810 * std::pow( std::abs( DeltaTemp ), OneThird ) / ( 1.382 + std::abs( Surface( SurfNum ).CosTilt ) );
					HcIn( SurfNum ) = std::pow( std::pow( HcIn( SurfNum ), 3.2 ) + std::pow( Hf, 3.2 ), 1.0 / 3.2 );

				} // ...end of IF-THEN block to set HConvIn

			}

		}

		// Establish some lower limit to avoid a zero convection coefficient (and potential divide by zero problems)
		if ( HcIn( SurfNum ) < LowHConvLimit ) HcIn( SurfNum ) = LowHConvLimit;

	}

	void
	CalcCeilingDiffuserIntConvCoeff( int const ZoneNum ) // zone number for which coefficients are being calculated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   August 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This subroutine calculates the interior convection coefficients
		// for ceiling diffusers correlated to the outlet air temperature.

		// METHODOLOGY EMPLOYED:
		// call functions with the actual model equations

		// REFERENCES:
		// Fisher, D.E. and C.O. Pedersen, Convective Heat Transfer in Building Energy and
		//       Thermal Load Calculations, ASHRAE Transactions, vol. 103, Pt. 2, 1997, p.137

		// OTHER NOTES:
		// The correlations shown below differ from (and are less accurate than) those shown
		// in the reference above (Fisher 1997).  They have been reformulated with an outlet
		// temperature reference in order to accomodate the structure of the EnergyPlus code.

		// Using/Aliasing
		using DataEnvironment::OutBaroPress;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyWFnTdpPb;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SurfNum; // DO loop counter for surfaces
		Real64 ACH; // Air changes per hour
		int ZoneNode; // Zone node as defined in system simulation
		Real64 ZoneVolume; // Zone node as defined in system simulation
		Real64 ZoneMassFlowRate; // Zone node as defined in system simulation
		Real64 AirDensity; // zone air density
		Real64 ZoneMult;

		// FLOW:
		if ( SysSizingCalc || ZoneSizingCalc || ! allocated( Node ) ) {
			ACH = 0.0;
		} else {
			// Set local variables
			ZoneVolume = Zone( ZoneNum ).Volume;
			ZoneNode = Zone( ZoneNum ).SystemZoneNodeNumber;
			ZoneMult = Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier;
			if ( ! BeginEnvrnFlag ) {
				AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, Node( ZoneNode ).Temp, PsyWFnTdpPb( Node( ZoneNode ).Temp, OutBaroPress ) );
				ZoneMassFlowRate = Node( ZoneNode ).MassFlowRate / ZoneMult;
			} else { // because these are not updated yet for new environment
				AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, 0.0, PsyWFnTdpPb( 0.0, OutBaroPress ) );
				ZoneMassFlowRate = 0.0;
			}

			if ( ZoneMassFlowRate < MinFlow ) {
				ACH = 0.0;
			} else {
				// Calculate ACH
				ACH = ZoneMassFlowRate / AirDensity / ZoneVolume * SecInHour;
				// Limit ACH to range of correlation
				ACH = min( ACH, MaxACH );
				ACH = max( ACH, 0.0 );
			}
		}

		// If the Ceiling Diffuser option is selected the following correlations are used.
		// The development of the ceiling diffuser convection correlations is shown in reference 4.
		// The correlations shown below differ from (and are less accurate than) those shown in reference 4 because they have been
		// reformulated with an outlet temperature reference in order to accomodate the structure of the
		// EnergyPlus code.
		for ( SurfNum = Zone( ZoneNum ).SurfaceFirst; SurfNum <= Zone( ZoneNum ).SurfaceLast; ++SurfNum ) {
			if ( ! Surface( SurfNum ).HeatTransSurf ) continue; // Skip non-heat transfer surfaces

			// Set HConvIn using the proper correlation based on Surface Tilt
			if ( Surface( SurfNum ).Tilt > 135.0 ) {
				HConvIn( SurfNum ) = CalcFisherPedersenCeilDiffuserFloor( ACH ); // Floor correlation
			} else if ( Surface( SurfNum ).Tilt < 45.0 ) {
				HConvIn( SurfNum ) = CalcFisherPedersenCeilDiffuserCeiling( ACH ); // Ceiling correlation
			} else {
				HConvIn( SurfNum ) = CalcFisherPedersenCeilDiffuserWalls( ACH ); // Wall correlation
			}
			// Establish some lower limit to avoid a zero convection coefficient (and potential divide by zero problems)
			if ( HConvIn( SurfNum ) < LowHConvLimit ) HConvIn( SurfNum ) = LowHConvLimit;

		} // SurfNum

	}

	// CalcCeilingDiffuserInletCorr should replace CalcCeilingDiffuser (above), if ZoneTempPredictorCorrector can
	// ever be made to work correctly with the inlet air temperature.

	void
	CalcCeilingDiffuserInletCorr(
		int const ZoneNum, // Zone number
		Array1S< Real64 > const SurfaceTemperatures // For CalcASHRAEDetailed, if called
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   August 2000
		//       RE-ENGINEERED  July 2003 (Peter Graham Ellis)
		//       MODIFIED       July 2003, (CC) set a flag for reference temperature so that supply air temperature
		//                                      is used as the reference in the inside heat balance calculations

		// PURPOSE OF THIS FUNCTION:
		// This subroutine calculates the interior convection coefficients
		// for ceiling diffusers correlated to the inlet air temperature.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// Fisher, D.E. and C.O. Pedersen, Convective Heat Transfer in Building Energy and
		//   Thermal Load Calculations, ASHRAE Transactions, vol. 103, Pt. 2, 1997, p.137

		// Using/Aliasing
		using DataEnvironment::OutBaroPress;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyWFnTdpPb;
		using DataHeatBalFanSys::MAT;

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 ACH; // Air changes per hour
		int ZoneNode; // Zone node as defined in system simulation
		Real64 ZoneVolume; // Zone node as defined in system simulation
		Real64 ZoneMassFlowRate; // Zone node as defined in system simulation
		Real64 AirDensity; // zone air density
		int SurfNum; // DO loop counter for surfaces
		Real64 Tilt; // Surface tilt
		Real64 ZoneMult;

		// FLOW:
		if ( SysSizingCalc || ZoneSizingCalc || ! allocated( Node ) ) {
			ACH = 0.0;
		} else {
			// Set local variables
			ZoneVolume = Zone( ZoneNum ).Volume;
			ZoneNode = Zone( ZoneNum ).SystemZoneNodeNumber;
			ZoneMult = Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier;
			AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, Node( ZoneNode ).Temp, PsyWFnTdpPb( Node( ZoneNode ).Temp, OutBaroPress ) );
			ZoneMassFlowRate = Node( ZoneNode ).MassFlowRate / ZoneMult;

			if ( ZoneMassFlowRate < MinFlow ) {
				ACH = 0.0;
			} else {
				// Calculate ACH
				ACH = ZoneMassFlowRate / AirDensity / ZoneVolume * SecInHour;
				// Limit ACH to range of correlation
				ACH = min( ACH, MaxACH );
				ACH = max( ACH, 0.0 );
			}
		}

		for ( SurfNum = Zone( ZoneNum ).SurfaceFirst; SurfNum <= Zone( ZoneNum ).SurfaceLast; ++SurfNum ) {
			if ( ! Surface( SurfNum ).HeatTransSurf ) continue; // Skip non-heat transfer surfaces

			if ( ACH <= 3.0 ) { // Use the other convection algorithm
				if ( ! Construct( Surface( SurfNum ).Construction ).TypeIsWindow ) {
					CalcASHRAEDetailedIntConvCoeff( SurfNum, SurfaceTemperatures( SurfNum ), MAT( ZoneNum ) );
				} else {
					CalcISO15099WindowIntConvCoeff( SurfNum, SurfaceTemperatures( SurfNum ), MAT( ZoneNum ) );
				}
			} else { // Use forced convection correlations
				Tilt = Surface( SurfNum ).Tilt;

				// assume that reference air temp for user defined convection coefficient is the mean air temperature (=MAT)
				// Calculate the convection coefficient based on inlet (supply) air conditions
				if ( Tilt < 45.0 ) {
					HConvIn( SurfNum ) = 0.49 * std::pow( ACH, 0.8 ); // Ceiling correlation
				} else if ( Tilt > 135.0 ) {
					HConvIn( SurfNum ) = 0.13 * std::pow( ACH, 0.8 ); // Floor correlation
				} else {
					HConvIn( SurfNum ) = 0.19 * std::pow( ACH, 0.8 ); // Wall correlation
				}
				// set flag for reference air temperature
				Surface( SurfNum ).TAirRef = ZoneSupplyAirTemp;
			}

			// Establish some lower limit to avoid a zero convection coefficient (and potential divide by zero problems)
			if ( HConvIn( SurfNum ) < LowHConvLimit ) HConvIn( SurfNum ) = LowHConvLimit;

		} // SurfNum

		if ( ACH > 100.0 ) ShowWarningError( "CeilingDiffuser convection correlation is out of range: ACH > 100" );

	}

	void
	CalcTrombeWallIntConvCoeff(
		int const ZoneNum, // Zone number for which coefficients are being calculated
		Array1S< Real64 > const SurfaceTemperatures // Temperature of surfaces for evaluation of HcIn
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   ?????
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This subroutine calculates the interior convection coefficient
		// using the Trombe Wall correlation ?????

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHeatBalFanSys::MAT;

		// Argument array dimensioning

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const g( 9.81 ); // gravity constant (m/s**2)
		Real64 const v( 15.89e-6 ); // kinematic viscosity (m**2/s) for air at 300 K
		Real64 const k( 0.0263 ); // thermal conductivity (W/m K) for air at 300 K
		Real64 const Pr( 0.71 ); // Prandtl number for air at ?

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SurfNum; // DO loop counter for surfaces
		int Surf1; // first major wall surface
		int Surf2; // second major wall surface

		Real64 H; // height of enclosure
		Real64 minorW; // width of enclosure (narrow dimension)
		Real64 majorW; // width of major surface
		Real64 gapW; // width of air gap
		Real64 asp; // aspect ratio H/gapW
		Real64 beta; // volumetric thermal expansion coefficient
		Real64 Gr; // Grashof number
		Real64 Nu; // Nusselt number
		Real64 HConvNet; // net heat transfer coefficient from wall to wall
		Real64 Tso; // outside surface temperature [K]
		Real64 Tsi; // inside surface temperature [K]

		// If the Trombe Wall option is selected the following correlations
		// will be used based on references by .....
		// tall enclosed rectangular cavity

		// This routine assumes that the major Trombe wall surfaces are of the
		// "WALL" class and are vertical.  The important heat transfer surfaces
		// are assumed to have exactly equal widths AND must have a greater
		// width than the side surfaces.

		Surf1 = 0;
		Surf2 = 0;

		H = Zone( ZoneNum ).CeilingHeight;
		minorW = 100000.0; // An impossibly big width
		majorW = 0.0;
		gapW = 0.0;

		Tso = 0.0;
		Tsi = 0.0;
		HConvNet = 0.0;

		// determine major width and minor width
		for ( SurfNum = Zone( ZoneNum ).SurfaceFirst; SurfNum <= Zone( ZoneNum ).SurfaceLast; ++SurfNum ) {
			if ( Surface( SurfNum ).Class != SurfaceClass_Wall ) continue;

			if ( Surface( SurfNum ).Width > majorW ) {
				majorW = Surface( SurfNum ).Width;
			}

			if ( Surface( SurfNum ).Width < minorW ) {
				minorW = Surface( SurfNum ).Width;
			}
		}

		// assign major surfaces
		for ( SurfNum = Zone( ZoneNum ).SurfaceFirst; SurfNum <= Zone( ZoneNum ).SurfaceLast; ++SurfNum ) {
			if ( Surface( SurfNum ).Class != SurfaceClass_Wall ) continue;

			if ( Surface( SurfNum ).Width == majorW ) {
				if ( Surf1 == 0 ) {
					Surf1 = SurfNum;
				} else {
					Surf2 = SurfNum;

					break; // both major surfaces are now assigned
				}
			}
		}

		// check to make sure major surfaces were found
		if ( Surf1 > 0 && Surf2 > 0 ) {
			gapW = minorW;
			asp = H / gapW; // This calc should only be done once for the zone

			// make sure inside surface is hot, outside is cold
			// NOTE: this is not ideal.  could have circumstances that reverse this?
			if ( SurfaceTemperatures( Surf1 ) > SurfaceTemperatures( Surf2 ) ) {
				Tsi = SurfaceTemperatures( Surf1 ) + KelvinConv;
				Tso = SurfaceTemperatures( Surf2 ) + KelvinConv;
			} else {
				Tso = SurfaceTemperatures( Surf1 ) + KelvinConv;
				Tsi = SurfaceTemperatures( Surf2 ) + KelvinConv;
			}

			beta = 2.0 / ( Tso + Tsi );

			Gr = ( g * beta * std::abs( Tsi - Tso ) * pow_3( gapW ) ) / pow_2( v ); // curve fit for v = v(T)?

			CalcNusselt( SurfNum, asp, Tso, Tsi, Gr, Pr, Nu ); // curve fit for Pr = Pr(T)?

			HConvNet = ( k / gapW ) * Nu; // curve fit for k = k(T)?

		} else {
			// fatal Error msg "heat transfer surfaces not found"
		}

		// Assign convection coefficients
		for ( SurfNum = Zone( ZoneNum ).SurfaceFirst; SurfNum <= Zone( ZoneNum ).SurfaceLast; ++SurfNum ) {
			if ( ! Surface( SurfNum ).HeatTransSurf ) continue; // Skip non-heat transfer surfaces

			// Use ASHRAESimple correlation to give values for all the minor surfaces
			CalcASHRAESimpleIntConvCoeff( SurfNum, SurfaceTemperatures( SurfNum ), MAT( ZoneNum ) );

			// assign the convection coefficent to the major surfaces and any subsurfaces on them
			if ( ( Surface( SurfNum ).BaseSurf == Surf1 ) || ( Surface( SurfNum ).BaseSurf == Surf2 ) ) {
				HConvIn( SurfNum ) = 2.0 * HConvNet;
			}

			// Establish some lower limit to avoid a zero convection coefficient (and potential divide by zero problems)
			if ( HConvIn( SurfNum ) < LowHConvLimit ) HConvIn( SurfNum ) = LowHConvLimit;
		}

	}

	void
	CalcNusselt(
		int const SurfNum, // Surface number
		Real64 const asp, // Aspect ratio: window height to gap width
		Real64 const tso, // Temperature of gap surface closest to outside (K)
		Real64 const tsi, // Temperature of gap surface closest to zone (K)
		Real64 const gr, // Gap gas Grashof number
		Real64 const pr, // Gap gas Prandtl number
		Real64 & gnu // Gap gas Nusselt number
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis, based on code adapted by Fred Winkelmann
		//                      from Window5 subroutine NusseltNumber
		//       DATE WRITTEN   September 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Finds the Nusselt number for gas-filled gaps between isothermal solid layers.
		// The gap may be filled with a single gas or a gas mixture.

		// METHODOLOGY EMPLOYED:
		// Based on methodology in Chapter 5 of the July 18, 2001 draft of ISO 15099,
		// "Thermal Performance of Windows, Doors and Shading Devices--Detailed Calculations."
		// The equation numbers below correspond to those in the standard.

		// REFERENCES:
		// Window5 source code; ISO 15099

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS
		Real64 ra; // Rayleigh number
		Real64 gnu901; // Nusselt number temporary variables for
		Real64 gnu902;
		Real64 gnu90;
		Real64 gnu601;
		Real64 gnu602; // different tilt and Ra ranges
		Real64 gnu60;
		Real64 gnu601a;
		Real64 gnua;
		Real64 gnub;
		Real64 cra; // Temporary variables
		Real64 a;
		Real64 b;
		Real64 g;
		Real64 ang;
		Real64 tilt;
		Real64 tiltr;
		Real64 costilt;
		Real64 sintilt;

		tilt = Surface( SurfNum ).Tilt;
		tiltr = tilt * DegToRadians;
		costilt = Surface( SurfNum ).CosTilt;
		sintilt = Surface( SurfNum ).SinTilt;
		ra = gr * pr;
		//!fw if (ra > 2.0e6): error that outside range of Rayleigh number?

		if ( ra <= 1.0e4 ) gnu901 = 1.0 + 1.7596678e-10 * std::pow( ra, 2.2984755 ); // eq. 51
		if ( ra > 1.0e4 && ra <= 5.0e4 ) gnu901 = 0.028154 * std::pow( ra, 0.4134 ); // eq. 50
		if ( ra > 5.0e4 ) gnu901 = 0.0673838 * std::pow( ra, 1.0 / 3.0 ); // eq. 49

		gnu902 = 0.242 * std::pow( ra / asp, 0.272 ); // eq. 52
		gnu90 = max( gnu901, gnu902 );

		if ( tso > tsi ) { // window heated from above
			gnu = 1.0 + ( gnu90 - 1.0 ) * sintilt; // eq. 53
		} else { // window heated from below
			if ( tilt >= 60.0 ) {
				g = 0.5 * std::pow( 1.0 + std::pow( ra / 3160.0, 20.6 ), -0.1 ); // eq. 47
				gnu601a = 1.0 + pow_7( 0.0936 * std::pow( ra, 0.314 ) / ( 1.0 + g ) ); // eq. 45
				gnu601 = std::pow( gnu601a, 0.142857 );

				// For any aspect ratio
				gnu602 = ( 0.104 + 0.175 / asp ) * std::pow( ra, 0.283 ); // eq. 46
				gnu60 = max( gnu601, gnu602 );

				// linear interpolation for layers inclined at angles between 60 and 90 deg
				gnu = ( ( 90.0 - tilt ) * gnu60 + ( tilt - 60.0 ) * gnu90 ) / 30.0;
			}
			if ( tilt < 60.0 ) { // eq. 42
				cra = ra * costilt;
				a = 1.0 - 1708.0 / cra;
				b = std::pow( cra / 5830.0, 0.33333 ) - 1.0; // LKL- replace .333 with OneThird?
				gnua = ( std::abs( a ) + a ) / 2.0;
				gnub = ( std::abs( b ) + b ) / 2.0;
				ang = 1708.0 * std::pow( std::sin( 1.8 * tiltr ), 1.6 );
				gnu = 1.0 + 1.44 * gnua * ( 1.0 - ang / cra ) + gnub;
			}
		}

	}

	Real64
	SetExtConvectionCoeff( int const SurfNum ) // Surface Number
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   May 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function accesses the data structure for the User
		// Supplied Exterior Convection Coefficients and returns that
		// as the result of this function.  The surface has already
		// been verified to have user supplied exterior convection values.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using ScheduleManager::GetCurrentScheduleValue;

		// Return value
		Real64 SetExtConvectionCoeff;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 HExt( 0.0 ); // Will become the returned value

		{ auto const SELECT_CASE_var( UserExtConvectionCoeffs( Surface( SurfNum ).ExtConvCoeff ).OverrideType );

		if ( SELECT_CASE_var == ConvCoefValue ) {
			HExt = UserExtConvectionCoeffs( Surface( SurfNum ).ExtConvCoeff ).OverrideValue;
			Surface( SurfNum ).OutConvHfModelEq = HcExt_UserValue; //reporting
			Surface( SurfNum ).OutConvHnModelEq = HcExt_None; //reporting
		} else if ( SELECT_CASE_var == ConvCoefSchedule ) {
			HExt = GetCurrentScheduleValue( UserExtConvectionCoeffs( Surface( SurfNum ).ExtConvCoeff ).ScheduleIndex );
			// Need to check for validity
			Surface( SurfNum ).OutConvHfModelEq = HcExt_UserSchedule; //reporting
			Surface( SurfNum ).OutConvHnModelEq = HcExt_None; //reporting
		} else if ( SELECT_CASE_var == ConvCoefUserCurve ) {
			CalcUserDefinedOutsideHcModel( SurfNum, UserExtConvectionCoeffs( Surface( SurfNum ).ExtConvCoeff ).UserCurveIndex, HExt );
			Surface( SurfNum ).OutConvHfModelEq = HcExt_UserCurve; //reporting
			Surface( SurfNum ).OutConvHnModelEq = HcExt_None; //reporting
		} else if ( SELECT_CASE_var == ConvCoefSpecifiedModel ) {
			EvaluateExtHcModels( SurfNum, UserExtConvectionCoeffs( Surface( SurfNum ).ExtConvCoeff ).HcModelEq, UserExtConvectionCoeffs( Surface( SurfNum ).ExtConvCoeff ).HcModelEq, HExt );
			Surface( SurfNum ).OutConvHfModelEq = UserExtConvectionCoeffs( Surface( SurfNum ).ExtConvCoeff ).HcModelEq; //reporting
			Surface( SurfNum ).OutConvHnModelEq = UserExtConvectionCoeffs( Surface( SurfNum ).ExtConvCoeff ).HcModelEq; //reporting
		} else {
			assert( false );
		}}

		SetExtConvectionCoeff = HExt;

		return SetExtConvectionCoeff;

	}

	Real64
	SetIntConvectionCoeff( int const SurfNum ) // Surface Number
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   May 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function accesses the data structre for the User
		// Supplied Interior Convection Coefficients and returns that
		// as the result of this function.  The surface has already
		// been verified to have user supplied interior convection values.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using ScheduleManager::GetCurrentScheduleValue;

		// Return value
		Real64 SetIntConvectionCoeff;

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

		Real64 HInt( 0.0 ); // Will become the returned value

		{ auto const SELECT_CASE_var( UserIntConvectionCoeffs( Surface( SurfNum ).IntConvCoeff ).OverrideType );

		if ( SELECT_CASE_var == ConvCoefValue ) {
			HInt = UserIntConvectionCoeffs( Surface( SurfNum ).IntConvCoeff ).OverrideValue;
			Surface( SurfNum ).IntConvHcModelEq = HcInt_UserValue; //reporting
		} else if ( SELECT_CASE_var == ConvCoefSchedule ) {
			HInt = GetCurrentScheduleValue( UserIntConvectionCoeffs( Surface( SurfNum ).IntConvCoeff ).ScheduleIndex );
			// Need to check for validity
			Surface( SurfNum ).IntConvHcModelEq = HcInt_UserSchedule; //reporting
		} else if ( SELECT_CASE_var == ConvCoefUserCurve ) {

			CalcUserDefinedInsideHcModel( SurfNum, UserIntConvectionCoeffs( Surface( SurfNum ).IntConvCoeff ).UserCurveIndex, HInt );
			Surface( SurfNum ).IntConvHcModelEq = HcInt_UserCurve; //reporting
		} else if ( SELECT_CASE_var == ConvCoefSpecifiedModel ) {

			EvaluateIntHcModels( SurfNum, UserIntConvectionCoeffs( Surface( SurfNum ).IntConvCoeff ).HcModelEq, HInt );
			Surface( SurfNum ).IntConvHcModelEq = UserIntConvectionCoeffs( Surface( SurfNum ).IntConvCoeff ).HcModelEq;
		} else {
			assert( false );
		}}

		SetIntConvectionCoeff = HInt;

		return SetIntConvectionCoeff;

	}

	void
	CalcISO15099WindowIntConvCoeff(
		int const SurfNum, // surface number for which coefficients are being calculated
		Real64 const SurfaceTemperature, // Temperature of surface for evaluation of HcIn
		Real64 const AirTemperature // Mean Air Temperature of Zone (or adjacent air temperature)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   January 2009
		//       MODIFIED       BG May 2009, added EMS override for window coeffs.
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate interior surface convection coefficients for windows

		// METHODOLOGY EMPLOYED:
		// correlation documented in ISO 15099, Section 8.3.2.2

		// REFERENCES:
		// Internation Standard ISO 15099. Thermal performance of windows, doors and shading devices -- Detailed Calculations
		// First Edition 2003-11-15. ISO 15099:2003(E)

		// Using/Aliasing
		using Psychrometrics::PsyCpAirFnWTdb;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using DataHeatBalFanSys::ZoneAirHumRatAvg;
		using DataEnvironment::OutHumRat;
		using DataEnvironment::OutBaroPress;

		// Locals
		Real64 const OneThird( ( 1.0 / 3.0 ) ); // 1/3 in highest precision
		static Real64 const pow_5_25( 0.56 * root_4( 1.0E+5 ) );
		static Real64 const pow_11_25( 0.56 * root_4( 1.0E+11 ) );
		static Real64 const pow_11_2( 0.58 * std::pow( 1.0E+11, 0.2 ) );
		static std::string const RoutineName( "WindowTempsForNominalCond" );

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 DeltaTemp; // Temperature difference between the zone air and the surface
		Real64 TmeanFilm; // mean film temperature
		Real64 TmeanFilmKelvin; // mean film temperature for property evaluation
		Real64 rho; // density of air [kg/m3]
		Real64 g; // acceleration due to gravity [m/s2]
		Real64 Height; // window cavity height [m]
		Real64 Cp; // specific heat of air [J/kg-K]
		Real64 lambda; // thermal conductivity of air [W/m-K]
		Real64 mu; // dynamic viscosity of air [kg/m-s]
		Real64 RaH; // Rayleigh number for cavity height [ Non dim]
		Real64 RaCV; // Rayleigh number for slanted cavity
		Real64 TiltDeg; // glazing tilt in degrees
		Real64 sineTilt; // sine of glazing tilt
		Real64 Nuint( 0.0 ); // Nusselt number for interior surface convection
		Real64 SurfTempKelvin; // surface temperature in Kelvin
		Real64 AirTempKelvin; // air temperature in Kelvin
		Real64 AirHumRat; // air humidity ratio

		SurfTempKelvin = SurfaceTemperature + 273.15;
		AirTempKelvin = AirTemperature + 273.15;
		DeltaTemp = SurfaceTemperature - AirTemperature;

		// protect against wildly out of range temperatures
		if ( ( AirTempKelvin < 200.0 ) || ( AirTempKelvin > 400.0 ) ) { // out of range
			HConvIn( SurfNum ) = LowHConvLimit;
			return;
		}
		if ( ( SurfTempKelvin < 180.0 ) || ( SurfTempKelvin > 450.0 ) ) { // out of range
			HConvIn( SurfNum ) = LowHConvLimit;
			return;
		}

		// Get humidity ratio
		if ( Surface( SurfNum ).Zone > 0 ) {
			AirHumRat = ZoneAirHumRatAvg( Surface( SurfNum ).Zone );
		} else {
			AirHumRat = OutHumRat;
		}

		// mean film temperature
		TmeanFilmKelvin = AirTempKelvin + 0.25 * ( SurfTempKelvin - AirTempKelvin ); // eq. 133 in ISO 15099
		TmeanFilm = TmeanFilmKelvin - 273.15;

		rho = PsyRhoAirFnPbTdbW( OutBaroPress, TmeanFilm, AirHumRat, RoutineName );
		g = 9.81;
		Height = Surface( SurfNum ).Height;

		// the following properties are probably for dry air, should maybe be remade for moist-air
		lambda = 2.873E-3 + 7.76E-5 * TmeanFilmKelvin; // Table B.1 in ISO 15099,
		mu = 3.723E-6 + 4.94E-8 * TmeanFilmKelvin; // Table B.2 in ISO 15099

		Cp = PsyCpAirFnWTdb( AirHumRat, TmeanFilm );

		TiltDeg = Surface( SurfNum ).Tilt;
		sineTilt = Surface( SurfNum ).SinTilt;

		// four cases depending on tilt and DeltaTemp (heat flow direction )
		if ( DeltaTemp > 0.0 ) TiltDeg = 180.0 - TiltDeg; // complement angle if cooling situation

		RaH = ( pow_2( rho ) * pow_3( Height ) * g * Cp * ( std::abs( SurfTempKelvin - AirTempKelvin ) ) ) / ( TmeanFilmKelvin * mu * lambda ); // eq 132 in ISO 15099

		// case a)
		if ( ( 0.0 <= TiltDeg ) && ( TiltDeg < 15.0 ) ) {

			Nuint = 0.13 * std::pow( RaH, OneThird );

			// case b)
		} else if ( ( 15.0 <= TiltDeg ) && ( TiltDeg <= 90.0 ) ) {

			RaCV = 2.5E+5 * std::pow( std::exp( 0.72 * TiltDeg ) / sineTilt, 0.2 ); // eq. 137

			if ( RaH <= RaCV ) {
				Nuint = 0.56 * root_4( RaH * sineTilt ); // eq. 135 in ISO 15099
			} else {
				Nuint = 0.13 * ( std::pow( RaH, OneThird ) - std::pow( RaCV, OneThird ) ) + 0.56 * root_4( RaCV * sineTilt ); // eq. 136 in ISO 15099
			}

			//case c)
		} else if ( ( 90.0 < TiltDeg ) && ( TiltDeg <= 179.0 ) ) {
			// bound by applicability
			if ( RaH * sineTilt < 1.0E+5 ) {
				Nuint = pow_5_25; // bounded
			} else if ( RaH * sineTilt >= 1.0E+11 ) {
				Nuint = pow_11_25; // bounded
			} else {
				Nuint = 0.56 * root_4( RaH * sineTilt ); // eq.. 138
			}

			// case d)
		} else if ( ( 179.0 < TiltDeg ) && ( TiltDeg <= 180.0 ) ) {

			if ( RaH > 1.0E+11 ) {
				Nuint = pow_11_2; // bounded
			} else {
				Nuint = 0.58 * std::pow( RaH, 0.2 );
			}

		} else {
			assert( false );
		}

		HConvIn( SurfNum ) = Nuint * lambda / Height;

		// EMS override point (Violates Standard 15099?  throw warning? scary.
		if ( Surface( SurfNum ).EMSOverrideIntConvCoef ) HConvIn( SurfNum ) = Surface( SurfNum ).EMSValueForIntConvCoef;

		// Establish some lower limit to avoid a zero convection coefficient (and potential divide by zero problems)
		if ( HConvIn( SurfNum ) < LowHConvLimit ) HConvIn( SurfNum ) = LowHConvLimit;

	}

	void
	SetupAdaptiveConvectionStaticMetaData()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//do one-time setup needed to store static data
		// for adaptive convection algorithm

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// none, developed for EnergyPlus version 6.0, see Eng Ref.

		// Using/Aliasing
		using Vectors::DetermineAzimuthAndTilt;
		using Vectors::CreateNewellSurfaceNormalVector;
		using Vectors::CreateNewellAreaVector;
		using Vectors::VecLength;
		using General::ScanForReports;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool FirstRoofSurf( true );
		int ZoneLoop;
		int SurfLoop;
		int VertLoop;
		//  REAL(r64) :: thisZoneHeight
		Real64 BldgVolumeSum;
		Real64 PerimExtLengthSum;

		Real64 thisWWR;
		Real64 thisZoneSimplePerim;
		Real64 thisZoneHorizHydralicDiameter;
		int ExtWallCount;
		int ExtWindowCount;
		Real64 thisAzimuth;
		Real64 thisArea;
		int thisZone;
		Array1D< Real64 > RoofBoundZvals( 8 );
		Array1D< Real64 > TestDist( 4 );
		//  TYPE(Vector), DIMENSION(4) :: BoundSurf
		Real64 surfacearea;
		Real64 BoundTilt;
		Real64 BoundAzimuth;
		bool DoReport;
		Real64 SideALength;
		Real64 SideBLength;
		Real64 SideCLength;
		Real64 SideDLength;
		std::string YesNo1;
		std::string YesNo2;

		struct FacadeGeoCharactisticsStruct
		{
			// Members
			Real64 AzimuthRangeLow;
			Real64 AzimuthRangeHi;
			Real64 Zmax;
			Real64 Zmin;
			Real64 Ymax;
			Real64 Ymin;
			Real64 Xmax;
			Real64 Xmin;
			Real64 Area;
			Real64 Perimeter;
			Real64 Height;

			// Default Constructor
			FacadeGeoCharactisticsStruct()
			{}

			// Member Constructor
			FacadeGeoCharactisticsStruct(
				Real64 const AzimuthRangeLow,
				Real64 const AzimuthRangeHi,
				Real64 const Zmax,
				Real64 const Zmin,
				Real64 const Ymax,
				Real64 const Ymin,
				Real64 const Xmax,
				Real64 const Xmin,
				Real64 const Area,
				Real64 const Perimeter,
				Real64 const Height
			) :
				AzimuthRangeLow( AzimuthRangeLow ),
				AzimuthRangeHi( AzimuthRangeHi ),
				Zmax( Zmax ),
				Zmin( Zmin ),
				Ymax( Ymax ),
				Ymin( Ymin ),
				Xmax( Xmax ),
				Xmin( Xmin ),
				Area( Area ),
				Perimeter( Perimeter ),
				Height( Height )
			{}

		};

		// Object Data
		Vector BoundNewellVec;
		Vector BoundNewellAreaVec;
		Vector dummy1;
		Vector dummy2;
		Vector dummy3;
		static FacadeGeoCharactisticsStruct NorthFacade( 332.5, 22.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 );
		static FacadeGeoCharactisticsStruct NorthEastFacade( 22.5, 67.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 );
		static FacadeGeoCharactisticsStruct EastFacade( 67.5, 112.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 );
		static FacadeGeoCharactisticsStruct SouthEastFacade( 112.5, 157.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 );
		static FacadeGeoCharactisticsStruct SouthFacade( 157.5, 202.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 );
		static FacadeGeoCharactisticsStruct SouthWestFacade( 202.5, 247.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 );
		static FacadeGeoCharactisticsStruct WestFacade( 247.5, 287.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 );
		static FacadeGeoCharactisticsStruct NorthWestFacade( 287.5, 332.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 );

		// Formats
		static gio::Fmt Format_900( "('! <Surface Convection Parameters>, Surface Name, Outside Model Assignment, Outside Area [m2], ','Outside Perimeter [m], Outside Height [m], Inside Model Assignment, ','Inside Height [cm], Inside Perimeter Envelope [m], Inside Hydraulic Diameter [m], Window Wall Ratio [ ], ','Window Location [ ], Near Radiant [Yes/No], Has Active HVAC [Yes/No]')" );
		static gio::Fmt Format_901( "('Surface Convection Parameters,',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A)" );
		static gio::Fmt Format_8000( "('! <Building Convection Parameters:North Facade>, Perimeter, Height, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax ')" );
		static gio::Fmt Format_8001( "('Building Convection Parameters:North Facade, ',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A)" );
		static gio::Fmt Format_8100( "('! <Building Convection Parameters:Northeast Facade>, Perimeter, Height, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax ')" );
		static gio::Fmt Format_8101( "('Building Convection Parameters:Northeast Facade, ',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A)" );
		static gio::Fmt Format_8200( "('! <Building Convection Parameters:East Facade>, Perimeter, Height, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax ')" );
		static gio::Fmt Format_8201( "('Building Convection Parameters:East Facade, ',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A)" );
		static gio::Fmt Format_8300( "('! <Building Convection Parameters:Southeast Facade>, Perimeter, Height, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax ')" );
		static gio::Fmt Format_8301( "('Building Convection Parameters:Southeast Facade, ',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A)" );
		static gio::Fmt Format_8400( "('! <Building Convection Parameters:South Facade>, Perimeter, Height, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax ')" );
		static gio::Fmt Format_8401( "('Building Convection Parameters:South Facade, ',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A)" );
		static gio::Fmt Format_8500( "('! <Building Convection Parameters:Southwest Facade>, Perimeter, Height, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax ')" );
		static gio::Fmt Format_8501( "('Building Convection Parameters:Southwest Facade, ',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A)" );
		static gio::Fmt Format_8600( "('! <Building Convection Parameters:West Facade>, Perimeter, Height, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax ')" );
		static gio::Fmt Format_8601( "('Building Convection Parameters:West Facade, ',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A)" );
		static gio::Fmt Format_8700( "('! <Building Convection Parameters:Northwest Facade>, Perimeter, Height, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax ')" );
		static gio::Fmt Format_8701( "('Building Convection Parameters:NorthwWest Facade, ',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A)" );
		static gio::Fmt Format_8800( "('! <Building Convection Parameters:Roof>, Area [m2], Perimeter [m], Height [m], ','XdYdZd:X, XdYdZd:Y, XdYdZd:Z',',XdYdZu:X, XdYdZu:Y, XdYdZu:Z',',XdYuZd:X, XdYuZd:Y, XdYuZd:Z',',XdYuZu:X, XdYuZu:Y, XdYuZu:Z',',XuYdZd:X, XuYdZd:Y, XuYdZd:Z',',XuYuZd:X, XuYuZd:Y, XuYuZd:Z',',XuYdZu:X, XuYdZu:Y, XuYdZu:Z',',XuYuZu:X, XuYuZu:Y, XuYuZu:Z')" );
		static gio::Fmt Format_8801( "('Building Convection Parameters:Roof,',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',')" );
		static gio::Fmt Format_88012( "(A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',')" );
		static gio::Fmt Format_88013( "(A,',',A,',',A,',',A,',',A,',',A,',',A)" );

		BldgVolumeSum = 0.0;
		RoofBoundZvals = 0.0;
		for ( ZoneLoop = 1; ZoneLoop <= NumOfZones; ++ZoneLoop ) {

			BldgVolumeSum += Zone( ZoneLoop ).Volume * Zone( ZoneLoop ).Multiplier * Zone( ZoneLoop ).ListMultiplier;
			PerimExtLengthSum = 0.0; // init
			ExtWallCount = 0; // init
			ExtWindowCount = 0; // init
			//model perimeter of bounding horizontal rectangle from max and min x and y values
			thisZoneSimplePerim = 2.0 * ( Zone( ZoneLoop ).MaximumY - Zone( ZoneLoop ).MinimumY ) + 2.0 * ( Zone( ZoneLoop ).MaximumX - Zone( ZoneLoop ).MinimumX );
			if ( thisZoneSimplePerim > 0.0 ) {
				thisZoneHorizHydralicDiameter = 4.0 * Zone( ZoneLoop ).FloorArea / thisZoneSimplePerim;
			} else {
				if ( Zone( ZoneLoop ).FloorArea > 0.0 ) {
					thisZoneHorizHydralicDiameter = std::sqrt( Zone( ZoneLoop ).FloorArea );
				}
			}

			if ( Zone( ZoneLoop ).ExtGrossWallArea > 0.0 ) {
				thisWWR = Zone( ZoneLoop ).ExtWindowArea / Zone( ZoneLoop ).ExtGrossWallArea;
			} else {
				thisWWR = -999.0; //throw error?
			}
			// first pass thru this zones surfaces to gather data
			for ( SurfLoop = Zone( ZoneLoop ).SurfaceFirst; SurfLoop <= Zone( ZoneLoop ).SurfaceLast; ++SurfLoop ) {
				//first catch exterior walls and do summations
				if ( ( Surface( SurfLoop ).ExtBoundCond == ExternalEnvironment ) && ( Surface( SurfLoop ).Class == SurfaceClass_Wall ) ) {
					PerimExtLengthSum += Surface( SurfLoop ).Width;
					++ExtWallCount;
				}
				if ( ( Surface( SurfLoop ).ExtBoundCond == ExternalEnvironment ) && ( ( Surface( SurfLoop ).Class == SurfaceClass_Window ) || ( Surface( SurfLoop ).Class == SurfaceClass_GlassDoor ) ) ) {
					++ExtWindowCount;
				}
			}

			//second pass thru zone surfs to fill data
			for ( SurfLoop = Zone( ZoneLoop ).SurfaceFirst; SurfLoop <= Zone( ZoneLoop ).SurfaceLast; ++SurfLoop ) {
				//now fill values
				Surface( SurfLoop ).IntConvZoneWallHeight = Zone( ZoneLoop ).CeilingHeight;
				Surface( SurfLoop ).IntConvZonePerimLength = PerimExtLengthSum;
				Surface( SurfLoop ).IntConvZoneHorizHydrDiam = thisZoneHorizHydralicDiameter;
				Surface( SurfLoop ).IntConvWindowWallRatio = thisWWR;
			} //2nd pass over surfaces.

			//third pass for window locations
			if ( ( ExtWindowCount > 0 ) && ( ExtWallCount > 0 ) ) {
				for ( SurfLoop = Zone( ZoneLoop ).SurfaceFirst; SurfLoop <= Zone( ZoneLoop ).SurfaceLast; ++SurfLoop ) {
					if ( ( Surface( SurfLoop ).ExtBoundCond == ExternalEnvironment ) && ( ( Surface( SurfLoop ).Class == SurfaceClass_Window ) || ( Surface( SurfLoop ).Class == SurfaceClass_GlassDoor ) ) ) {
						if ( Surface( SurfLoop ).IntConvWindowWallRatio < 0.5 ) {
							if ( Surface( SurfLoop ).Centroid.z < Zone( ZoneLoop ).Centroid.z ) {
								Surface( SurfLoop ).IntConvWindowLocation = InConvWinLoc_LowerPartOfExteriorWall;
							} else {
								Surface( SurfLoop ).IntConvWindowLocation = InConvWinLoc_UpperPartOfExteriorWall;
							}
						} else {
							Surface( SurfLoop ).IntConvWindowLocation = InConvWinLoc_LargePartOfExteriorWall;
						}
						if ( ( Surface( Surface( SurfLoop ).BaseSurf ).ExtBoundCond == ExternalEnvironment ) && ( Surface( Surface( SurfLoop ).BaseSurf ).Class == SurfaceClass_Wall ) ) {
							if ( Surface( Surface( SurfLoop ).BaseSurf ).Centroid.z < Surface( SurfLoop ).Centroid.z ) {
								Surface( Surface( SurfLoop ).BaseSurf ).IntConvWindowLocation = InConvWinLoc_WindowAboveThis;
							} else {
								Surface( Surface( SurfLoop ).BaseSurf ).IntConvWindowLocation = InConvWinLoc_WindowBelowThis;
							}
						}
					}
					if ( ( Surface( SurfLoop ).ExtBoundCond == ExternalEnvironment ) && ( Surface( SurfLoop ).Class == SurfaceClass_Wall ) && ( Surface( SurfLoop ).IntConvWindowLocation == InConvWinLoc_NotSet ) ) {
						if ( Surface( SurfLoop ).Centroid.z < Zone( ZoneLoop ).Centroid.z ) {
							Surface( SurfLoop ).IntConvWindowLocation = InConvWinLoc_WindowAboveThis;
						} else {
							Surface( SurfLoop ).IntConvWindowLocation = InConvWinLoc_WindowBelowThis;
						}

					}
				} //third pass over surfaces

			}
		} //loop over zones for inside face parameters

		CubeRootOfOverallBuildingVolume = std::pow( BldgVolumeSum, OneThird );

		// first pass over surfaces for outside face params
		for ( SurfLoop = 1; SurfLoop <= TotSurfaces; ++SurfLoop ) {
			if ( Surface( SurfLoop ).ExtBoundCond != ExternalEnvironment ) continue;
			if ( ! Surface( SurfLoop ).HeatTransSurf ) continue;
			thisAzimuth = Surface( SurfLoop ).Azimuth;
			thisArea = Surface( SurfLoop ).Area;
			thisZone = Surface( SurfLoop ).Zone;
			if ( ( Surface( SurfLoop ).Tilt >= 45.0 ) && ( Surface( SurfLoop ).Tilt < 135.0 ) ) { // treat as vertical wall

				auto const & vertices( Surface( SurfLoop ).Vertex );
				Real64 const x_min( minval( vertices, &Vector::x ) );
				Real64 const y_min( minval( vertices, &Vector::y ) );
				Real64 const z_min( minval( vertices, &Vector::z ) );
				Real64 const x_max( maxval( vertices, &Vector::x ) );
				Real64 const y_max( maxval( vertices, &Vector::y ) );
				Real64 const z_max( maxval( vertices, &Vector::z ) );

				if ( ( thisAzimuth >= NorthFacade.AzimuthRangeLow ) || ( thisAzimuth < NorthFacade.AzimuthRangeHi ) ) {
					NorthFacade.Area += thisArea;
					NorthFacade.Zmax = max( z_max, NorthFacade.Zmax );
					NorthFacade.Zmin = min( z_min, NorthFacade.Zmin );
					NorthFacade.Ymax = max( y_max, NorthFacade.Ymax );
					NorthFacade.Ymin = min( y_min, NorthFacade.Ymin );
					NorthFacade.Xmax = max( x_max, NorthFacade.Xmax );
					NorthFacade.Xmin = min( x_min, NorthFacade.Xmin );

				} else if ( ( thisAzimuth >= NorthEastFacade.AzimuthRangeLow ) && ( thisAzimuth < NorthEastFacade.AzimuthRangeHi ) ) {
					NorthEastFacade.Area += thisArea;
					NorthEastFacade.Zmax = max( z_max, NorthEastFacade.Zmax );
					NorthEastFacade.Zmin = min( z_min, NorthEastFacade.Zmin );
					NorthEastFacade.Ymax = max( y_max, NorthEastFacade.Ymax );
					NorthEastFacade.Ymin = min( y_min, NorthEastFacade.Ymin );
					NorthEastFacade.Xmax = max( x_max, NorthEastFacade.Xmax );
					NorthEastFacade.Xmin = min( x_min, NorthEastFacade.Xmin );

				} else if ( ( thisAzimuth >= EastFacade.AzimuthRangeLow ) && ( thisAzimuth < EastFacade.AzimuthRangeHi ) ) {
					EastFacade.Area += thisArea;
					EastFacade.Zmax = max( z_max, EastFacade.Zmax );
					EastFacade.Zmin = min( z_min, EastFacade.Zmin );
					EastFacade.Ymax = max( y_max, EastFacade.Ymax );
					EastFacade.Ymin = min( y_min, EastFacade.Ymin );
					EastFacade.Xmax = max( x_max, EastFacade.Xmax );
					EastFacade.Xmin = min( x_min, EastFacade.Xmin );
				} else if ( ( thisAzimuth >= SouthEastFacade.AzimuthRangeLow ) && ( thisAzimuth < SouthEastFacade.AzimuthRangeHi ) ) {
					SouthEastFacade.Area += thisArea;
					SouthEastFacade.Zmax = max( z_max, SouthEastFacade.Zmax );
					SouthEastFacade.Zmin = min( z_min, SouthEastFacade.Zmin );
					SouthEastFacade.Ymax = max( y_max, SouthEastFacade.Ymax );
					SouthEastFacade.Ymin = min( y_min, SouthEastFacade.Ymin );
					SouthEastFacade.Xmax = max( x_max, SouthEastFacade.Xmax );
					SouthEastFacade.Xmin = min( x_min, SouthEastFacade.Xmin );

				} else if ( ( thisAzimuth >= SouthFacade.AzimuthRangeLow ) && ( thisAzimuth < SouthFacade.AzimuthRangeHi ) ) {
					SouthFacade.Area += thisArea;
					SouthFacade.Zmax = max( z_max, SouthFacade.Zmax );
					SouthFacade.Zmin = min( z_min, SouthFacade.Zmin );
					SouthFacade.Ymax = max( y_max, SouthFacade.Ymax );
					SouthFacade.Ymin = min( y_min, SouthFacade.Ymin );
					SouthFacade.Xmax = max( x_max, SouthFacade.Xmax );
					SouthFacade.Xmin = min( x_min, SouthFacade.Xmin );

				} else if ( ( thisAzimuth >= SouthWestFacade.AzimuthRangeLow ) && ( thisAzimuth < SouthWestFacade.AzimuthRangeHi ) ) {
					SouthWestFacade.Area += thisArea;
					SouthWestFacade.Zmax = max( z_max, SouthWestFacade.Zmax );
					SouthWestFacade.Zmin = min( z_min, SouthWestFacade.Zmin );
					SouthWestFacade.Ymax = max( y_max, SouthWestFacade.Ymax );
					SouthWestFacade.Ymin = min( y_min, SouthWestFacade.Ymin );
					SouthWestFacade.Xmax = max( x_max, SouthWestFacade.Xmax );
					SouthWestFacade.Xmin = min( x_min, SouthWestFacade.Xmin );

				} else if ( ( thisAzimuth >= WestFacade.AzimuthRangeLow ) && ( thisAzimuth < WestFacade.AzimuthRangeHi ) ) {
					WestFacade.Area += thisArea;
					WestFacade.Zmax = max( z_max, WestFacade.Zmax );
					WestFacade.Zmin = min( z_min, WestFacade.Zmin );
					WestFacade.Ymax = max( y_max, WestFacade.Ymax );
					WestFacade.Ymin = min( y_min, WestFacade.Ymin );
					WestFacade.Xmax = max( x_max, WestFacade.Xmax );
					WestFacade.Xmin = min( x_min, WestFacade.Xmin );

				} else if ( ( thisAzimuth >= NorthWestFacade.AzimuthRangeLow ) && ( thisAzimuth < NorthWestFacade.AzimuthRangeHi ) ) {
					NorthWestFacade.Area += thisArea;
					NorthWestFacade.Zmax = max( z_max, NorthWestFacade.Zmax );
					NorthWestFacade.Zmin = min( z_min, NorthWestFacade.Zmin );
					NorthWestFacade.Ymax = max( y_max, NorthWestFacade.Ymax );
					NorthWestFacade.Ymin = min( y_min, NorthWestFacade.Ymin );
					NorthWestFacade.Xmax = max( x_max, NorthWestFacade.Xmax );
					NorthWestFacade.Xmin = min( x_min, NorthWestFacade.Xmin );

				}
			} else if ( Surface( SurfLoop ).Tilt < 45.0 ) { //TODO Double check tilt wrt outside vs inside

				if ( FirstRoofSurf ) { //Init with something in the group
					RoofGeo.XdYdZd.SurfNum = SurfLoop;
					RoofGeo.XdYdZd.VertNum = 1;
					RoofGeo.XdYdZd.Vertex = Surface( SurfLoop ).Vertex( 1 );

					RoofGeo.XdYdZu.SurfNum = SurfLoop;
					RoofGeo.XdYdZu.VertNum = 1;
					RoofGeo.XdYdZu.Vertex = Surface( SurfLoop ).Vertex( 1 );

					RoofGeo.XdYuZd.SurfNum = SurfLoop;
					RoofGeo.XdYuZd.VertNum = 1;
					RoofGeo.XdYuZd.Vertex = Surface( SurfLoop ).Vertex( 1 );

					RoofGeo.XdYuZu.SurfNum = SurfLoop;
					RoofGeo.XdYuZu.VertNum = 1;
					RoofGeo.XdYuZu.Vertex = Surface( SurfLoop ).Vertex( 1 );

					RoofGeo.XuYdZd.SurfNum = SurfLoop;
					RoofGeo.XuYdZd.VertNum = 1;
					RoofGeo.XuYdZd.Vertex = Surface( SurfLoop ).Vertex( 1 );

					RoofGeo.XuYuZd.SurfNum = SurfLoop;
					RoofGeo.XuYuZd.VertNum = 1;
					RoofGeo.XuYuZd.Vertex = Surface( SurfLoop ).Vertex( 1 );

					RoofGeo.XuYdZu.SurfNum = SurfLoop;
					RoofGeo.XuYdZu.VertNum = 1;
					RoofGeo.XuYdZu.Vertex = Surface( SurfLoop ).Vertex( 1 );

					RoofGeo.XuYuZu.SurfNum = SurfLoop;
					RoofGeo.XuYuZu.VertNum = 1;
					RoofGeo.XuYuZu.Vertex = Surface( SurfLoop ).Vertex( 1 );

					FirstRoofSurf = false;
				}
				// treat as part of roof group
				RoofGeo.Area += thisArea;
				for ( VertLoop = 1; VertLoop <= Surface( SurfLoop ).Sides; ++VertLoop ) {

					//1 low x, low y, low z
					if ( ( Surface( SurfLoop ).Vertex( VertLoop ).x <= RoofGeo.XdYdZd.Vertex.x ) && ( Surface( SurfLoop ).Vertex( VertLoop ).y <= RoofGeo.XdYdZd.Vertex.y ) && ( Surface( SurfLoop ).Vertex( VertLoop ).z <= RoofGeo.XdYdZd.Vertex.z ) ) {
						//this point is more toward this bound
						RoofGeo.XdYdZd.SurfNum = SurfLoop;
						RoofGeo.XdYdZd.VertNum = VertLoop;
						RoofGeo.XdYdZd.Vertex = Surface( SurfLoop ).Vertex( VertLoop );
						RoofBoundZvals( 1 ) = Surface( SurfLoop ).Vertex( VertLoop ).z;
					}

					// 2 low x, low y, hi z
					if ( ( Surface( SurfLoop ).Vertex( VertLoop ).x <= RoofGeo.XdYdZu.Vertex.x ) && ( Surface( SurfLoop ).Vertex( VertLoop ).y <= RoofGeo.XdYdZu.Vertex.y ) && ( Surface( SurfLoop ).Vertex( VertLoop ).z >= RoofGeo.XdYdZu.Vertex.z ) ) {
						//this point is more toward this bound
						RoofGeo.XdYdZu.SurfNum = SurfLoop;
						RoofGeo.XdYdZu.VertNum = VertLoop;
						RoofGeo.XdYdZu.Vertex = Surface( SurfLoop ).Vertex( VertLoop );
						RoofBoundZvals( 2 ) = Surface( SurfLoop ).Vertex( VertLoop ).z;
					}

					// 3 low x, hi y, low z
					if ( ( Surface( SurfLoop ).Vertex( VertLoop ).x <= RoofGeo.XdYuZd.Vertex.x ) && ( Surface( SurfLoop ).Vertex( VertLoop ).y >= RoofGeo.XdYuZd.Vertex.y ) && ( Surface( SurfLoop ).Vertex( VertLoop ).z <= RoofGeo.XdYuZd.Vertex.z ) ) {
						//this point is more toward this bound
						RoofGeo.XdYuZd.SurfNum = SurfLoop;
						RoofGeo.XdYuZd.VertNum = VertLoop;
						RoofGeo.XdYuZd.Vertex = Surface( SurfLoop ).Vertex( VertLoop );
						RoofBoundZvals( 3 ) = Surface( SurfLoop ).Vertex( VertLoop ).z;
					}

					// 4 low x, hi y, hi z
					if ( ( Surface( SurfLoop ).Vertex( VertLoop ).x <= RoofGeo.XdYuZu.Vertex.x ) && ( Surface( SurfLoop ).Vertex( VertLoop ).y >= RoofGeo.XdYuZu.Vertex.y ) && ( Surface( SurfLoop ).Vertex( VertLoop ).z >= RoofGeo.XdYuZu.Vertex.z ) ) {
						//this point is more toward this bound
						RoofGeo.XdYuZu.SurfNum = SurfLoop;
						RoofGeo.XdYuZu.VertNum = VertLoop;
						RoofGeo.XdYuZu.Vertex = Surface( SurfLoop ).Vertex( VertLoop );
						RoofBoundZvals( 4 ) = Surface( SurfLoop ).Vertex( VertLoop ).z;
					}

					// 5 hi x, low y, low z
					if ( ( Surface( SurfLoop ).Vertex( VertLoop ).x >= RoofGeo.XuYdZd.Vertex.x ) && ( Surface( SurfLoop ).Vertex( VertLoop ).y <= RoofGeo.XuYdZd.Vertex.y ) && ( Surface( SurfLoop ).Vertex( VertLoop ).z <= RoofGeo.XuYdZd.Vertex.z ) ) {
						//this point is more toward this bound
						RoofGeo.XuYdZd.SurfNum = SurfLoop;
						RoofGeo.XuYdZd.VertNum = VertLoop;
						RoofGeo.XuYdZd.Vertex = Surface( SurfLoop ).Vertex( VertLoop );
						RoofBoundZvals( 5 ) = Surface( SurfLoop ).Vertex( VertLoop ).z;
					}

					// 6 hi x, hi y, low z
					if ( ( Surface( SurfLoop ).Vertex( VertLoop ).x >= RoofGeo.XuYuZd.Vertex.x ) && ( Surface( SurfLoop ).Vertex( VertLoop ).y >= RoofGeo.XuYuZd.Vertex.y ) && ( Surface( SurfLoop ).Vertex( VertLoop ).z <= RoofGeo.XuYuZd.Vertex.z ) ) {
						//this point is more toward this bound
						RoofGeo.XuYuZd.SurfNum = SurfLoop;
						RoofGeo.XuYuZd.VertNum = VertLoop;
						RoofGeo.XuYuZd.Vertex = Surface( SurfLoop ).Vertex( VertLoop );
						RoofBoundZvals( 6 ) = Surface( SurfLoop ).Vertex( VertLoop ).z;
					}

					// 7 hi x, low y, hi z
					if ( ( Surface( SurfLoop ).Vertex( VertLoop ).x >= RoofGeo.XuYdZu.Vertex.x ) && ( Surface( SurfLoop ).Vertex( VertLoop ).y <= RoofGeo.XuYdZu.Vertex.y ) && ( Surface( SurfLoop ).Vertex( VertLoop ).z >= RoofGeo.XuYdZu.Vertex.z ) ) {
						//this point is more toward this bound
						RoofGeo.XuYdZu.SurfNum = SurfLoop;
						RoofGeo.XuYdZu.VertNum = VertLoop;
						RoofGeo.XuYdZu.Vertex = Surface( SurfLoop ).Vertex( VertLoop );
						RoofBoundZvals( 7 ) = Surface( SurfLoop ).Vertex( VertLoop ).z;
					}

					// 8 hi x, hi y, hi z
					if ( ( Surface( SurfLoop ).Vertex( VertLoop ).x >= RoofGeo.XuYuZu.Vertex.x ) && ( Surface( SurfLoop ).Vertex( VertLoop ).y >= RoofGeo.XuYuZu.Vertex.y ) && ( Surface( SurfLoop ).Vertex( VertLoop ).z >= RoofGeo.XuYuZu.Vertex.z ) ) {
						//this point is more toward this bound
						RoofGeo.XuYuZu.SurfNum = SurfLoop;
						RoofGeo.XuYuZu.VertNum = VertLoop;
						RoofGeo.XuYuZu.Vertex = Surface( SurfLoop ).Vertex( VertLoop );
						RoofBoundZvals( 8 ) = Surface( SurfLoop ).Vertex( VertLoop ).z;
					}

				}
			}
		} // fist loop over surfaces for outside face params

		NorthFacade.Perimeter = 2.0 * std::sqrt( pow_2( NorthFacade.Xmax - NorthFacade.Xmin ) + pow_2( NorthFacade.Ymax - NorthFacade.Ymin ) ) + 2.0 * ( NorthFacade.Zmax - NorthFacade.Zmin );
		NorthFacade.Height = NorthFacade.Zmax - NorthFacade.Zmin;

		NorthEastFacade.Perimeter = 2.0 * std::sqrt( pow_2( NorthEastFacade.Xmax - NorthEastFacade.Xmin ) + pow_2( NorthEastFacade.Ymax - NorthEastFacade.Ymin ) ) + 2.0 * ( NorthEastFacade.Zmax - NorthEastFacade.Zmin );
		NorthEastFacade.Height = NorthEastFacade.Zmax - NorthEastFacade.Zmin;

		EastFacade.Perimeter = 2.0 * std::sqrt( pow_2( EastFacade.Xmax - EastFacade.Xmin ) + pow_2( EastFacade.Ymax - EastFacade.Ymin ) ) + 2.0 * ( EastFacade.Zmax - EastFacade.Zmin );
		EastFacade.Height = EastFacade.Zmax - EastFacade.Zmin;

		SouthEastFacade.Perimeter = 2.0 * std::sqrt( pow_2( SouthEastFacade.Xmax - SouthEastFacade.Xmin ) + pow_2( SouthEastFacade.Ymax - SouthEastFacade.Ymin ) ) + 2.0 * ( SouthEastFacade.Zmax - SouthEastFacade.Zmin );
		SouthEastFacade.Height = SouthEastFacade.Zmax - SouthEastFacade.Zmin;

		SouthFacade.Perimeter = 2.0 * std::sqrt( pow_2( SouthFacade.Xmax - SouthFacade.Xmin ) + pow_2( SouthFacade.Ymax - SouthFacade.Ymin ) ) + 2.0 * ( SouthFacade.Zmax - SouthFacade.Zmin );
		SouthFacade.Height = SouthFacade.Zmax - SouthFacade.Zmin;

		SouthWestFacade.Perimeter = 2.0 * std::sqrt( pow_2( SouthWestFacade.Xmax - SouthWestFacade.Xmin ) + pow_2( SouthWestFacade.Ymax - SouthWestFacade.Ymin ) ) + 2.0 * ( SouthWestFacade.Zmax - SouthWestFacade.Zmin );
		SouthWestFacade.Height = SouthWestFacade.Zmax - SouthWestFacade.Zmin;

		WestFacade.Perimeter = 2.0 * std::sqrt( pow_2( WestFacade.Xmax - WestFacade.Xmin ) + pow_2( WestFacade.Ymax - WestFacade.Ymin ) ) + 2.0 * ( WestFacade.Zmax - WestFacade.Zmin );
		WestFacade.Height = WestFacade.Zmax - WestFacade.Zmin;

		NorthWestFacade.Perimeter = 2.0 * std::sqrt( pow_2( NorthWestFacade.Xmax - NorthWestFacade.Xmin ) + pow_2( NorthWestFacade.Ymax - NorthWestFacade.Ymin ) ) + 2.0 * ( NorthWestFacade.Zmax - NorthWestFacade.Zmin );
		NorthWestFacade.Height = NorthWestFacade.Zmax - NorthWestFacade.Zmin;

		//now model roof perimeter
		// move around bounding boxes side walls and find the longest of the four distances
		// Side A: Y low -- uses XdYdZd, XdYdZu, XuYdZd, XuYdZu
		TestDist( 1 ) = distance( RoofGeo.XdYdZd.Vertex, RoofGeo.XuYdZd.Vertex );
		TestDist( 2 ) = distance( RoofGeo.XdYdZd.Vertex, RoofGeo.XuYdZu.Vertex );
		TestDist( 3 ) = distance( RoofGeo.XdYdZu.Vertex, RoofGeo.XuYdZd.Vertex );
		TestDist( 4 ) = distance( RoofGeo.XdYdZu.Vertex, RoofGeo.XuYdZu.Vertex );
		SideALength = maxval( TestDist );

		// Side B: X Hi -- uses XuYdZd, XuYuZd, XuYdZu, XuYuZu
		TestDist( 1 ) = distance( RoofGeo.XuYdZd.Vertex, RoofGeo.XuYuZd.Vertex );
		TestDist( 2 ) = distance( RoofGeo.XuYdZd.Vertex, RoofGeo.XuYuZu.Vertex );
		TestDist( 3 ) = distance( RoofGeo.XuYdZu.Vertex, RoofGeo.XuYuZd.Vertex );
		TestDist( 4 ) = distance( RoofGeo.XuYdZu.Vertex, RoofGeo.XuYuZu.Vertex );
		SideBLength = maxval( TestDist );

		// Side C: Y Hi -- uses XdYuZd, XdYuZu, XuYuZd, XuYuZu
		TestDist( 1 ) = distance( RoofGeo.XdYuZd.Vertex, RoofGeo.XuYuZd.Vertex );
		TestDist( 2 ) = distance( RoofGeo.XdYuZd.Vertex, RoofGeo.XuYuZu.Vertex );
		TestDist( 3 ) = distance( RoofGeo.XdYuZu.Vertex, RoofGeo.XuYuZd.Vertex );
		TestDist( 4 ) = distance( RoofGeo.XdYuZu.Vertex, RoofGeo.XuYuZu.Vertex );
		SideCLength = maxval( TestDist );

		// Side D: X Lo Hi -- uses XdYuZd, XdYuZu, XuYuZd, XuYuZu
		TestDist( 1 ) = distance( RoofGeo.XdYuZd.Vertex, RoofGeo.XuYuZd.Vertex );
		TestDist( 2 ) = distance( RoofGeo.XdYuZd.Vertex, RoofGeo.XuYuZu.Vertex );
		TestDist( 3 ) = distance( RoofGeo.XdYuZu.Vertex, RoofGeo.XuYuZd.Vertex );
		TestDist( 4 ) = distance( RoofGeo.XdYuZu.Vertex, RoofGeo.XuYuZu.Vertex );
		SideDLength = maxval( TestDist );

		RoofGeo.Perimeter = SideALength + SideBLength + SideCLength + SideDLength;

		RoofGeo.Height = maxval( RoofBoundZvals ) - minval( RoofBoundZvals );

		// now find the longest bound face
		if ( ( SideALength >= SideBLength ) && ( SideALength >= SideCLength ) && ( SideALength >= SideDLength ) ) {
			// Side A: Y low -- uses XdYdZd, XdYdZu, XuYdZd, XuYdZu
			RoofGeo.BoundSurf( 1 ) = RoofGeo.XdYdZd.Vertex;
			RoofGeo.BoundSurf( 2 ) = RoofGeo.XuYdZd.Vertex;
			RoofGeo.BoundSurf( 3 ) = RoofGeo.XuYdZu.Vertex;
			RoofGeo.BoundSurf( 4 ) = RoofGeo.XdYdZu.Vertex;

		} else if ( ( SideBLength >= SideALength ) && ( SideBLength >= SideCLength ) && ( SideBLength >= SideDLength ) ) {
			// Side B: X Hi -- uses XuYdZd, XuYuZd, XuYdZu, XuYuZu
			RoofGeo.BoundSurf( 1 ) = RoofGeo.XuYdZd.Vertex;
			RoofGeo.BoundSurf( 2 ) = RoofGeo.XuYuZd.Vertex;
			RoofGeo.BoundSurf( 3 ) = RoofGeo.XuYuZu.Vertex;
			RoofGeo.BoundSurf( 4 ) = RoofGeo.XuYdZu.Vertex;
		} else if ( ( SideCLength >= SideALength ) && ( SideCLength >= SideBLength ) && ( SideCLength >= SideDLength ) ) {
			// Side C: Y Hi -- uses XdYuZd, XdYuZu, XuYuZd, XuYuZu
			RoofGeo.BoundSurf( 1 ) = RoofGeo.XdYuZd.Vertex;
			RoofGeo.BoundSurf( 2 ) = RoofGeo.XuYuZd.Vertex;
			RoofGeo.BoundSurf( 3 ) = RoofGeo.XuYuZu.Vertex;
			RoofGeo.BoundSurf( 4 ) = RoofGeo.XdYuZu.Vertex;
		} else if ( ( SideDLength >= SideALength ) && ( SideDLength >= SideCLength ) && ( SideDLength >= SideBLength ) ) {
			// Side D: X Lo Hi -- uses XdYuZd, XdYuZu, XuYuZd, XuYuZu
			RoofGeo.BoundSurf( 1 ) = RoofGeo.XdYuZd.Vertex;
			RoofGeo.BoundSurf( 2 ) = RoofGeo.XuYuZd.Vertex;
			RoofGeo.BoundSurf( 3 ) = RoofGeo.XuYuZu.Vertex;
			RoofGeo.BoundSurf( 4 ) = RoofGeo.XdYuZu.Vertex;
		}

		CreateNewellAreaVector( RoofGeo.BoundSurf, 4, BoundNewellAreaVec );
		surfacearea = VecLength( BoundNewellAreaVec );
		if ( surfacearea > 0.001 ) { // Roof is not flat
			CreateNewellSurfaceNormalVector( RoofGeo.BoundSurf, 4, BoundNewellVec );
			DetermineAzimuthAndTilt( RoofGeo.BoundSurf, 4, BoundAzimuth, BoundTilt, dummy1, dummy2, dummy3, surfacearea, BoundNewellVec );
			RoofLongAxisOutwardAzimuth = BoundAzimuth;
		} else {
			RoofLongAxisOutwardAzimuth = 0.0; // flat roofs don't really have azimuth
		}

		for ( SurfLoop = 1; SurfLoop <= TotSurfaces; ++SurfLoop ) {
			if ( Surface( SurfLoop ).ExtBoundCond != ExternalEnvironment ) continue;
			if ( ! Surface( SurfLoop ).HeatTransSurf ) continue;
			thisAzimuth = Surface( SurfLoop ).Azimuth;

			auto const & vertices( Surface( SurfLoop ).Vertex );
			Real64 const z_min( minval( vertices, &Vector::z ) );
			Real64 const z_max( maxval( vertices, &Vector::z ) );
			Real64 const z_del( z_max - z_min );

			if ( ( Surface( SurfLoop ).Tilt >= 45.0 ) && ( Surface( SurfLoop ).Tilt < 135.0 ) ) { // treat as vertical wall
				if ( ( thisAzimuth >= NorthFacade.AzimuthRangeLow ) || ( thisAzimuth < NorthFacade.AzimuthRangeHi ) ) {
					Surface( SurfLoop ).OutConvFaceArea = max( NorthFacade.Area, Surface( SurfLoop ).GrossArea );
					Surface( SurfLoop ).OutConvFacePerimeter = max( NorthFacade.Perimeter, Surface( SurfLoop ).Perimeter );
					Surface( SurfLoop ).OutConvFaceHeight = max( NorthFacade.Height, z_del );
				} else if ( ( thisAzimuth >= NorthEastFacade.AzimuthRangeLow ) && ( thisAzimuth < NorthEastFacade.AzimuthRangeHi ) ) {
					Surface( SurfLoop ).OutConvFaceArea = max( NorthEastFacade.Area, Surface( SurfLoop ).GrossArea );
					Surface( SurfLoop ).OutConvFacePerimeter = max( NorthEastFacade.Perimeter, Surface( SurfLoop ).Perimeter );
					Surface( SurfLoop ).OutConvFaceHeight = max( NorthEastFacade.Height, z_del );
				} else if ( ( thisAzimuth >= EastFacade.AzimuthRangeLow ) && ( thisAzimuth < EastFacade.AzimuthRangeHi ) ) {
					Surface( SurfLoop ).OutConvFaceArea = max( EastFacade.Area, Surface( SurfLoop ).GrossArea );
					Surface( SurfLoop ).OutConvFacePerimeter = max( EastFacade.Perimeter, Surface( SurfLoop ).Perimeter );
					Surface( SurfLoop ).OutConvFaceHeight = max( EastFacade.Height, z_del );
				} else if ( ( thisAzimuth >= SouthEastFacade.AzimuthRangeLow ) && ( thisAzimuth < SouthEastFacade.AzimuthRangeHi ) ) {
					Surface( SurfLoop ).OutConvFaceArea = max( SouthEastFacade.Area, Surface( SurfLoop ).GrossArea );
					Surface( SurfLoop ).OutConvFacePerimeter = max( SouthEastFacade.Perimeter, Surface( SurfLoop ).Perimeter );
					Surface( SurfLoop ).OutConvFaceHeight = max( SouthEastFacade.Height, z_del );
				} else if ( ( thisAzimuth >= SouthFacade.AzimuthRangeLow ) && ( thisAzimuth < SouthFacade.AzimuthRangeHi ) ) {
					Surface( SurfLoop ).OutConvFaceArea = max( SouthFacade.Area, Surface( SurfLoop ).GrossArea );
					Surface( SurfLoop ).OutConvFacePerimeter = max( SouthFacade.Perimeter, Surface( SurfLoop ).Perimeter );
					Surface( SurfLoop ).OutConvFaceHeight = max( SouthFacade.Height, z_del );
				} else if ( ( thisAzimuth >= SouthWestFacade.AzimuthRangeLow ) && ( thisAzimuth < SouthWestFacade.AzimuthRangeHi ) ) {
					Surface( SurfLoop ).OutConvFaceArea = max( SouthWestFacade.Area, Surface( SurfLoop ).GrossArea );
					Surface( SurfLoop ).OutConvFacePerimeter = max( SouthWestFacade.Perimeter, Surface( SurfLoop ).Perimeter );
					Surface( SurfLoop ).OutConvFaceHeight = max( SouthWestFacade.Height, z_del );
				} else if ( ( thisAzimuth >= WestFacade.AzimuthRangeLow ) && ( thisAzimuth < WestFacade.AzimuthRangeHi ) ) {
					Surface( SurfLoop ).OutConvFaceArea = max( WestFacade.Area, Surface( SurfLoop ).GrossArea );
					Surface( SurfLoop ).OutConvFacePerimeter = max( WestFacade.Perimeter, Surface( SurfLoop ).Perimeter );
					Surface( SurfLoop ).OutConvFaceHeight = max( WestFacade.Height, z_del );
				} else if ( ( thisAzimuth >= NorthWestFacade.AzimuthRangeLow ) && ( thisAzimuth < NorthWestFacade.AzimuthRangeHi ) ) {
					Surface( SurfLoop ).OutConvFaceArea = max( NorthWestFacade.Area, Surface( SurfLoop ).GrossArea );
					Surface( SurfLoop ).OutConvFacePerimeter = max( NorthWestFacade.Perimeter, Surface( SurfLoop ).Perimeter );
					Surface( SurfLoop ).OutConvFaceHeight = max( NorthWestFacade.Height, z_del );
				}
			} else if ( Surface( SurfLoop ).Tilt < 45.0 ) { // assume part of roof
				Surface( SurfLoop ).OutConvFaceArea = max( RoofGeo.Area, Surface( SurfLoop ).GrossArea );
				Surface( SurfLoop ).OutConvFacePerimeter = max( RoofGeo.Perimeter, Surface( SurfLoop ).Perimeter );
				Surface( SurfLoop ).OutConvFaceHeight = max( RoofGeo.Height, z_del );
			} else if ( Surface( SurfLoop ).Tilt >= 135.0 ) { //assume floor over exterior, just use surface's geometry
				Surface( SurfLoop ).OutConvFaceArea = Surface( SurfLoop ).GrossArea;
				Surface( SurfLoop ).OutConvFacePerimeter = Surface( SurfLoop ).Perimeter;
				Surface( SurfLoop ).OutConvFaceHeight = z_del;
			}
		} // second pass thru surfs for outside face convection params.

		// now send to EIO if surface reporting selected
		ScanForReports( "Surfaces", DoReport, "Details" );
		if ( DoReport ) { // echo out static geometry data related to convection models

			gio::write( OutputFileInits, Format_900 ); //header
			for ( SurfLoop = 1; SurfLoop <= TotSurfaces; ++SurfLoop ) {
				if ( ! Surface( SurfLoop ).HeatTransSurf ) continue;
				if ( Surface( SurfLoop ).IntConvSurfGetsRadiantHeat ) {
					YesNo1 = "Yes";
				} else {
					YesNo1 = "No";
				}
				if ( Surface( SurfLoop ).IntConvSurfHasActiveInIt ) {
					YesNo2 = "Yes";
				} else {
					YesNo2 = "No";
				}
				gio::write( OutputFileInits, Format_901 ) << Surface( SurfLoop ).Name << RoundSigDigits( Surface( SurfLoop ).ExtConvCoeff ) << RoundSigDigits( Surface( SurfLoop ).OutConvFaceArea, 2 ) << RoundSigDigits( Surface( SurfLoop ).OutConvFacePerimeter, 2 ) << RoundSigDigits( Surface( SurfLoop ).OutConvFaceHeight, 2 ) << RoundSigDigits( Surface( SurfLoop ).IntConvCoeff ) << RoundSigDigits( Surface( SurfLoop ).IntConvZoneWallHeight, 2 ) << RoundSigDigits( Surface( SurfLoop ).IntConvZonePerimLength, 2 ) << RoundSigDigits( Surface( SurfLoop ).IntConvZoneHorizHydrDiam, 2 ) << RoundSigDigits( Surface( SurfLoop ).IntConvWindowWallRatio, 2 ) << RoundSigDigits( Surface( SurfLoop ).IntConvWindowLocation ) << YesNo1 << YesNo2; // [m] length of perimeter zone's exterior wall | [m] hydraulic diameter, usually 4 times the zone floor area div by perimeter | [-] area of windows over area of exterior wall for zone | relative location of window in zone for interior Hc models

			}

			//if display advanced reports also dump meta group data used for convection geometry
			if ( DisplayAdvancedReportVariables ) {
				gio::write( OutputFileInits, Format_8000 ); //header for north facade
				gio::write( OutputFileInits, Format_8001 ) << RoundSigDigits( NorthFacade.Perimeter, 2 ) << RoundSigDigits( NorthFacade.Height, 2 ) << RoundSigDigits( NorthFacade.Xmin, 2 ) << RoundSigDigits( NorthFacade.Xmax, 2 ) << RoundSigDigits( NorthFacade.Ymin, 2 ) << RoundSigDigits( NorthFacade.Ymax, 2 ) << RoundSigDigits( NorthFacade.Zmin, 2 ) << RoundSigDigits( NorthFacade.Zmax, 2 );
				gio::write( OutputFileInits, Format_8100 ); //header for northeast facade
				gio::write( OutputFileInits, Format_8101 ) << RoundSigDigits( NorthEastFacade.Perimeter, 2 ) << RoundSigDigits( NorthEastFacade.Height, 2 ) << RoundSigDigits( NorthEastFacade.Xmin, 2 ) << RoundSigDigits( NorthEastFacade.Xmax, 2 ) << RoundSigDigits( NorthEastFacade.Ymin, 2 ) << RoundSigDigits( NorthEastFacade.Ymax, 2 ) << RoundSigDigits( NorthEastFacade.Zmin, 2 ) << RoundSigDigits( NorthEastFacade.Zmax, 2 );
				gio::write( OutputFileInits, Format_8200 ); //header for east facade
				gio::write( OutputFileInits, Format_8201 ) << RoundSigDigits( EastFacade.Perimeter, 2 ) << RoundSigDigits( EastFacade.Height, 2 ) << RoundSigDigits( EastFacade.Xmin, 2 ) << RoundSigDigits( EastFacade.Xmax, 2 ) << RoundSigDigits( EastFacade.Ymin, 2 ) << RoundSigDigits( EastFacade.Ymax, 2 ) << RoundSigDigits( EastFacade.Zmin, 2 ) << RoundSigDigits( EastFacade.Zmax, 2 );

				gio::write( OutputFileInits, Format_8300 ); //header for southeast facade
				gio::write( OutputFileInits, Format_8301 ) << RoundSigDigits( SouthEastFacade.Perimeter, 2 ) << RoundSigDigits( SouthEastFacade.Height, 2 ) << RoundSigDigits( SouthEastFacade.Xmin, 2 ) << RoundSigDigits( SouthEastFacade.Xmax, 2 ) << RoundSigDigits( SouthEastFacade.Ymin, 2 ) << RoundSigDigits( SouthEastFacade.Ymax, 2 ) << RoundSigDigits( SouthEastFacade.Zmin, 2 ) << RoundSigDigits( SouthEastFacade.Zmax, 2 );

				gio::write( OutputFileInits, Format_8400 ); //header for south facade
				gio::write( OutputFileInits, Format_8401 ) << RoundSigDigits( SouthFacade.Perimeter, 2 ) << RoundSigDigits( SouthFacade.Height, 2 ) << RoundSigDigits( SouthFacade.Xmin, 2 ) << RoundSigDigits( SouthFacade.Xmax, 2 ) << RoundSigDigits( SouthFacade.Ymin, 2 ) << RoundSigDigits( SouthFacade.Ymax, 2 ) << RoundSigDigits( SouthFacade.Zmin, 2 ) << RoundSigDigits( SouthFacade.Zmax, 2 );
				gio::write( OutputFileInits, Format_8500 ); //header for southwest facade
				gio::write( OutputFileInits, Format_8501 ) << RoundSigDigits( SouthWestFacade.Perimeter, 2 ) << RoundSigDigits( SouthWestFacade.Height, 2 ) << RoundSigDigits( SouthWestFacade.Xmin, 2 ) << RoundSigDigits( SouthWestFacade.Xmax, 2 ) << RoundSigDigits( SouthWestFacade.Ymin, 2 ) << RoundSigDigits( SouthWestFacade.Ymax, 2 ) << RoundSigDigits( SouthWestFacade.Zmin, 2 ) << RoundSigDigits( SouthWestFacade.Zmax, 2 );
				gio::write( OutputFileInits, Format_8600 ); //header for west facade
				gio::write( OutputFileInits, Format_8601 ) << RoundSigDigits( WestFacade.Perimeter, 2 ) << RoundSigDigits( WestFacade.Height, 2 ) << RoundSigDigits( WestFacade.Xmin, 2 ) << RoundSigDigits( WestFacade.Xmax, 2 ) << RoundSigDigits( WestFacade.Ymin, 2 ) << RoundSigDigits( WestFacade.Ymax, 2 ) << RoundSigDigits( WestFacade.Zmin, 2 ) << RoundSigDigits( WestFacade.Zmax, 2 );
				gio::write( OutputFileInits, Format_8700 ); //header for northwest facade
				gio::write( OutputFileInits, Format_8701 ) << RoundSigDigits( NorthWestFacade.Perimeter, 2 ) << RoundSigDigits( NorthWestFacade.Height, 2 ) << RoundSigDigits( NorthWestFacade.Xmin, 2 ) << RoundSigDigits( NorthWestFacade.Xmax, 2 ) << RoundSigDigits( NorthWestFacade.Ymin, 2 ) << RoundSigDigits( NorthWestFacade.Ymax, 2 ) << RoundSigDigits( NorthWestFacade.Zmin, 2 ) << RoundSigDigits( NorthWestFacade.Zmax, 2 );
				gio::write( OutputFileInits, Format_8800 ); // header for roof
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, Format_8801, flags ) << RoundSigDigits( RoofGeo.Area, 2 ) << RoundSigDigits( RoofGeo.Perimeter, 2 ) << RoundSigDigits( RoofGeo.Height, 2 ) << RoundSigDigits( RoofGeo.XdYdZd.Vertex.x, 3 ) << RoundSigDigits( RoofGeo.XdYdZd.Vertex.y, 3 ) << RoundSigDigits( RoofGeo.XdYdZd.Vertex.z, 3 ) << RoundSigDigits( RoofGeo.XdYdZu.Vertex.x, 3 ) << RoundSigDigits( RoofGeo.XdYdZu.Vertex.y, 3 ) << RoundSigDigits( RoofGeo.XdYdZu.Vertex.z, 3 ) << RoundSigDigits( RoofGeo.XdYuZd.Vertex.x, 3 ); }
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, Format_88012, flags ) << RoundSigDigits( RoofGeo.XdYuZd.Vertex.y, 3 ) << RoundSigDigits( RoofGeo.XdYuZd.Vertex.z, 3 ) << RoundSigDigits( RoofGeo.XdYuZu.Vertex.x, 3 ) << RoundSigDigits( RoofGeo.XdYuZu.Vertex.y, 3 ) << RoundSigDigits( RoofGeo.XdYuZu.Vertex.z, 3 ) << RoundSigDigits( RoofGeo.XuYdZd.Vertex.x, 3 ) << RoundSigDigits( RoofGeo.XuYdZd.Vertex.y, 3 ) << RoundSigDigits( RoofGeo.XuYdZd.Vertex.z, 3 ) << RoundSigDigits( RoofGeo.XuYuZd.Vertex.x, 3 ) << RoundSigDigits( RoofGeo.XuYuZd.Vertex.y, 3 ); }
				gio::write( OutputFileInits, Format_88013 ) << RoundSigDigits( RoofGeo.XuYuZd.Vertex.z, 3 ) << RoundSigDigits( RoofGeo.XuYdZu.Vertex.x, 3 ) << RoundSigDigits( RoofGeo.XuYdZu.Vertex.y, 3 ) << RoundSigDigits( RoofGeo.XuYdZu.Vertex.z, 3 ) << RoundSigDigits( RoofGeo.XuYuZu.Vertex.x, 3 ) << RoundSigDigits( RoofGeo.XuYuZu.Vertex.y, 3 ) << RoundSigDigits( RoofGeo.XuYuZu.Vertex.z, 3 );

			}

		}

		ConvectionGeometryMetaDataSetup = true;

	}

	void
	SetupAdaptiveConvectionRadiantSurfaceData()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Sept 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// identify Zones that have active radiant elements for convection model classifications

		// METHODOLOGY EMPLOYED:
		// Need to fill in values for ZoneEquipConfig%InWallActiveElement, ZoneEquipConfig%InCeilingActiveElement
		// and ZoneEquipConfig(ZoneNum)%InFloorActiveElement.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataZoneEquipment;

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
		static int ActiveWallCount( 0 );
		static Real64 ActiveWallArea( 0.0 );
		static int ActiveCeilingCount( 0 );
		static Real64 ActiveCeilingArea( 0.0 );
		static int ActiveFloorCount( 0 );
		static Real64 ActiveFloorArea( 0.0 );
		int ZoneLoop;
		int SurfLoop;

		for ( ZoneLoop = 1; ZoneLoop <= NumOfZones; ++ZoneLoop ) {
			ActiveWallCount = 0;
			ActiveWallArea = 0.0;
			ActiveCeilingCount = 0;
			ActiveCeilingArea = 0.0;
			ActiveFloorCount = 0;
			ActiveFloorArea = 0.0;

			for ( SurfLoop = Zone( ZoneLoop ).SurfaceFirst; SurfLoop <= Zone( ZoneLoop ).SurfaceLast; ++SurfLoop ) {
				if ( ! Surface( SurfLoop ).IntConvSurfHasActiveInIt ) continue;
				if ( Surface( SurfLoop ).Class == SurfaceClass_Wall || Surface( SurfLoop ).Class == SurfaceClass_Door ) {
					++ActiveWallCount;
					ActiveWallArea += Surface( SurfLoop ).Area;
				} else if ( Surface( SurfLoop ).Class == SurfaceClass_Roof ) {
					++ActiveCeilingCount;
					ActiveCeilingArea += Surface( SurfLoop ).Area;
				} else if ( Surface( SurfLoop ).Class == SurfaceClass_Floor ) {
					++ActiveFloorCount;
					ActiveFloorArea += Surface( SurfLoop ).Area;
				}
			} // surface loop

			if ( ( ActiveWallCount > 0 ) && ( ActiveWallArea > 0.0 ) ) {
				ZoneEquipConfig( ZoneLoop ).InWallActiveElement = true;
			}
			if ( ( ActiveCeilingCount > 0 ) && ( ActiveCeilingArea > 0.0 ) ) {
				ZoneEquipConfig( ZoneLoop ).InCeilingActiveElement = true;
			}
			if ( ( ActiveFloorCount > 0 ) && ( ActiveFloorArea > 0 ) ) {
				ZoneEquipConfig( ZoneLoop ).InFloorActiveElement = true;
			}
		} // zone loop

	}

	void
	ManageInsideAdaptiveConvectionAlgo( int const SurfNum ) // surface number for which coefficients are being calculated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages the calculation of interior convection coefficient for a surface.

		// METHODOLOGY EMPLOYED:
		// This routine implements the Adaptive Convection Algorithm developed by IB-M 2000 and IB-M 2002
		//  - first calls a large routine, DynamicIntConvSurfaceClassification, that has most of the complex logic
		//  - then calls a straightforward routine that maps the classification to model equation
		//  - then calls a routine with a large case statement that calls model equations.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// this next call sets up the flow regime and assigns a classification to surface
		//  TODO: candidate for rework to do zone level calcs once rather than for each surface
		DynamicIntConvSurfaceClassification( SurfNum );

		// simple worker routine takes surface classification and fills in model to use (IntConvHcModelEq) for that surface
		MapIntConvClassificationToHcModels( SurfNum );

		EvaluateIntHcModels( SurfNum, Surface( SurfNum ).IntConvHcModelEq, HConvIn( SurfNum ) );
		//if ( std::isnan( HConvIn( SurfNum ) ) ) { // Use IEEE_IS_NAN when GFortran supports it
			//// throw Error
			//ShowSevereError( "Inside convection coefficient is out of bound = " + Surface( SurfNum ).Name );
			//ShowFatalError( "Inside convection coefficient model number = " + TrimSigDigits( Surface( SurfNum ).IntConvHcModelEq ) );
		//}
	}

	void
	ManageOutsideAdaptiveConvectionAlgo(
		int const SurfNum, // surface number for which coefficients are being calculated
		Real64 & Hc // result for Hc Outside face, becomes HExt.
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates the convection coefficient for the outside face of a surface

		// METHODOLOGY EMPLOYED:
		// This routine implements an adpative struture and classification system for outdoor
		//   It calls a series of separable worker routines

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		DynamicExtConvSurfaceClassification( SurfNum );

		MapExtConvClassificationToHcModels( SurfNum );

		EvaluateExtHcModels( SurfNum, Surface( SurfNum ).OutConvHnModelEq, Surface( SurfNum ).OutConvHfModelEq, Hc );

	}

	void
	EvaluateIntHcModels(
		int const SurfNum,
		int const ConvModelEquationNum,
		Real64 & Hc // calculated Hc value
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// central case statement for calling inside convection models

		// METHODOLOGY EMPLOYED:
		//  - fills value for Hc by calling the appropriate convection model, usually as a function.
		//     preperation of argument values for the function calls is contained in each Case block (repeats)
		//  - also updates the reference air temperature type for use in the surface heat balance calcs

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHeatBalSurface::TH;
		using DataHeatBalSurface::QdotConvInRepPerArea;
		using DataHeatBalFanSys::MAT;
		using namespace DataZoneEquipment;
		using DataEnvironment::OutBaroPress;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyWFnTdpPb;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Real64 SupplyAirTemp;
		Real64 AirChangeRate;
		int ZoneNum; // zone associated with inside face of surface
		int ZoneNode; // the system node for the zone, node index
		int EquipNum;
		Real64 SumMdotTemp;
		Real64 SumMdot;
		Real64 AirDensity;
		Real64 AirSystemVolFlowRate;
		int thisZoneInletNode;
		Real64 tmpHc;
		Real64 ZoneMult; // local product of zone multiplier and zonelist multipler

		tmpHc = 0.0;
		//now call appropriate function to calculate Hc
		{ auto const SELECT_CASE_var( ConvModelEquationNum );

		if ( SELECT_CASE_var == HcInt_UserCurve ) {
			CalcUserDefinedInsideHcModel( SurfNum, Surface( SurfNum ).IntConvHcUserCurveIndex, tmpHc );
		} else if ( SELECT_CASE_var == HcInt_ASHRAEVerticalWall ) {
			ZoneNum = Surface( SurfNum ).Zone;
			tmpHc = CalcASHRAEVerticalWall( ( TH( 2, 1, SurfNum ) - MAT( ZoneNum ) ) );
			Surface( SurfNum ).TAirRef = ZoneMeanAirTemp;
		} else if ( SELECT_CASE_var == HcInt_WaltonUnstableHorizontalOrTilt ) {
			ZoneNum = Surface( SurfNum ).Zone;
			tmpHc = CalcWaltonUnstableHorizontalOrTilt( ( TH( 2, 1, SurfNum ) - MAT( ZoneNum ) ), Surface( SurfNum ).CosTilt ); //TODO verify CosTilt in vs out
			Surface( SurfNum ).TAirRef = ZoneMeanAirTemp;
		} else if ( SELECT_CASE_var == HcInt_WaltonStableHorizontalOrTilt ) {
			ZoneNum = Surface( SurfNum ).Zone;
			tmpHc = CalcWaltonStableHorizontalOrTilt( ( TH( 2, 1, SurfNum ) - MAT( ZoneNum ) ), Surface( SurfNum ).CosTilt ); //TODO verify CosTilt in vs out
			Surface( SurfNum ).TAirRef = ZoneMeanAirTemp;
		} else if ( SELECT_CASE_var == HcInt_FisherPedersenCeilDiffuserFloor ) {
			ZoneNum = Surface( SurfNum ).Zone;
			ZoneNode = Zone( ZoneNum ).SystemZoneNodeNumber;
			if ( ZoneNode > 0 ) {
				ZoneMult = Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier;
				AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, Node( ZoneNode ).Temp, PsyWFnTdpPb( Node( ZoneNode ).Temp, OutBaroPress ) );
				AirChangeRate = ( Node( ZoneNode ).MassFlowRate * SecInHour ) / ( AirDensity * Zone( ZoneNum ).Volume * ZoneMult );
				AirChangeRate = min( AirChangeRate, MaxACH );
				AirChangeRate = max( AirChangeRate, 0.0 );
				Surface( SurfNum ).TAirRef = ZoneSupplyAirTemp;
			} else {
				AirChangeRate = 0.0;
				Surface( SurfNum ).TAirRef = ZoneMeanAirTemp;
			}
			tmpHc = CalcFisherPedersenCeilDiffuserFloor( AirChangeRate );

		} else if ( SELECT_CASE_var == HcInt_FisherPedersenCeilDiffuserCeiling ) {
			ZoneNum = Surface( SurfNum ).Zone;
			ZoneNode = Zone( ZoneNum ).SystemZoneNodeNumber;
			if ( ZoneNode > 0 ) {
				ZoneMult = Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier;
				AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, Node( ZoneNode ).Temp, PsyWFnTdpPb( Node( ZoneNode ).Temp, OutBaroPress ) );
				AirChangeRate = ( Node( ZoneNode ).MassFlowRate * SecInHour ) / ( AirDensity * Zone( ZoneNum ).Volume * ZoneMult );
				AirChangeRate = min( AirChangeRate, MaxACH );
				AirChangeRate = max( AirChangeRate, 0.0 );
				Surface( SurfNum ).TAirRef = ZoneSupplyAirTemp;
			} else {
				AirChangeRate = 0.0;
				Surface( SurfNum ).TAirRef = ZoneMeanAirTemp;
			}
			tmpHc = CalcFisherPedersenCeilDiffuserCeiling( AirChangeRate );

		} else if ( SELECT_CASE_var == HcInt_FisherPedersenCeilDiffuserWalls ) {
			ZoneNum = Surface( SurfNum ).Zone;
			ZoneNode = Zone( ZoneNum ).SystemZoneNodeNumber;
			if ( ZoneNode > 0 ) {
				ZoneMult = Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier;
				AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, Node( ZoneNode ).Temp, PsyWFnTdpPb( Node( ZoneNode ).Temp, OutBaroPress ) );
				AirChangeRate = ( Node( ZoneNode ).MassFlowRate * SecInHour ) / ( AirDensity * Zone( ZoneNum ).Volume * ZoneMult );
				AirChangeRate = min( AirChangeRate, MaxACH );
				AirChangeRate = max( AirChangeRate, 0.0 );
				Surface( SurfNum ).TAirRef = ZoneSupplyAirTemp;
			} else {
				AirChangeRate = 0.0;
				Surface( SurfNum ).TAirRef = ZoneMeanAirTemp;
			}
			tmpHc = CalcFisherPedersenCeilDiffuserWalls( AirChangeRate );

		} else if ( SELECT_CASE_var == HcInt_AlamdariHammondStableHorizontal ) {
			ZoneNum = Surface( SurfNum ).Zone;
			tmpHc = CalcAlamdariHammondStableHorizontal( ( TH( 2, 1, SurfNum ) - MAT( ZoneNum ) ), Surface( SurfNum ).IntConvZoneHorizHydrDiam, SurfNum );
			Surface( SurfNum ).TAirRef = ZoneMeanAirTemp;
		} else if ( SELECT_CASE_var == HcInt_AlamdariHammondVerticalWall ) {
			ZoneNum = Surface( SurfNum ).Zone;
			tmpHc = CalcAlamdariHammondVerticalWall( ( TH( 2, 1, SurfNum ) - MAT( ZoneNum ) ), Surface( SurfNum ).IntConvZoneWallHeight, SurfNum );
			Surface( SurfNum ).TAirRef = ZoneMeanAirTemp;
		} else if ( SELECT_CASE_var == HcInt_AlamdariHammondUnstableHorizontal ) {
			ZoneNum = Surface( SurfNum ).Zone;
			tmpHc = CalcAlamdariHammondUnstableHorizontal( ( TH( 2, 1, SurfNum ) - MAT( ZoneNum ) ), Surface( SurfNum ).IntConvZoneHorizHydrDiam, SurfNum );
			Surface( SurfNum ).TAirRef = ZoneMeanAirTemp;
		} else if ( SELECT_CASE_var == HcInt_KhalifaEq3WallAwayFromHeat ) {
			ZoneNum = Surface( SurfNum ).Zone;
			tmpHc = CalcKhalifaEq3WallAwayFromHeat( ( TH( 2, 1, SurfNum ) - MAT( ZoneNum ) ) );
			Surface( SurfNum ).TAirRef = ZoneMeanAirTemp;
		} else if ( SELECT_CASE_var == HcInt_KhalifaEq4CeilingAwayFromHeat ) {
			ZoneNum = Surface( SurfNum ).Zone;
			tmpHc = CalcKhalifaEq4CeilingAwayFromHeat( ( TH( 2, 1, SurfNum ) - MAT( ZoneNum ) ) );
			Surface( SurfNum ).TAirRef = ZoneMeanAirTemp;
		} else if ( SELECT_CASE_var == HcInt_KhalifaEq5WallNearHeat ) {
			ZoneNum = Surface( SurfNum ).Zone;
			tmpHc = CalcKhalifaEq5WallsNearHeat( ( TH( 2, 1, SurfNum ) - MAT( ZoneNum ) ) );
			Surface( SurfNum ).TAirRef = ZoneMeanAirTemp;
		} else if ( SELECT_CASE_var == HcInt_KhalifaEq6NonHeatedWalls ) {
			ZoneNum = Surface( SurfNum ).Zone;
			tmpHc = CalcKhalifaEq6NonHeatedWalls( ( TH( 2, 1, SurfNum ) - MAT( ZoneNum ) ) );
			Surface( SurfNum ).TAirRef = ZoneMeanAirTemp;
		} else if ( SELECT_CASE_var == HcInt_KhalifaEq7Ceiling ) {
			ZoneNum = Surface( SurfNum ).Zone;
			tmpHc = CalcKhalifaEq7Ceiling( ( TH( 2, 1, SurfNum ) - MAT( ZoneNum ) ) );
			Surface( SurfNum ).TAirRef = ZoneMeanAirTemp;
		} else if ( SELECT_CASE_var == HcInt_AwbiHattonHeatedFloor ) {
			ZoneNum = Surface( SurfNum ).Zone;
			tmpHc = CalcAwbiHattonHeatedFloor( ( TH( 2, 1, SurfNum ) - MAT( ZoneNum ) ), Surface( SurfNum ).IntConvZoneHorizHydrDiam );
			Surface( SurfNum ).TAirRef = ZoneMeanAirTemp;
		} else if ( SELECT_CASE_var == HcInt_AwbiHattonHeatedWall ) {
			ZoneNum = Surface( SurfNum ).Zone;
			tmpHc = CalcAwbiHattonHeatedWall( ( TH( 2, 1, SurfNum ) - MAT( ZoneNum ) ), Surface( SurfNum ).IntConvZoneHorizHydrDiam );
			Surface( SurfNum ).TAirRef = ZoneMeanAirTemp;
		} else if ( SELECT_CASE_var == HcInt_BeausoleilMorrisonMixedAssistingWall ) {
			ZoneNum = Surface( SurfNum ).Zone;
			ZoneNode = Zone( ZoneNum ).SystemZoneNodeNumber;
			if ( ZoneNode > 0 ) {
				ZoneMult = Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier;
				AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, Node( ZoneNode ).Temp, PsyWFnTdpPb( Node( ZoneNode ).Temp, OutBaroPress ) );
				AirChangeRate = ( Node( ZoneNode ).MassFlowRate * SecInHour ) / ( AirDensity * Zone( ZoneNum ).Volume * ZoneMult );
				SumMdotTemp = 0.0;
				SumMdot = 0.0;
				for ( EquipNum = 1; EquipNum <= ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).NumOfEquipTypes; ++EquipNum ) {
					if ( ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).EquipData( EquipNum ).NumOutlets > 0 ) {
						thisZoneInletNode = ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).EquipData( EquipNum ).OutletNodeNums( 1 );
						if ( ( thisZoneInletNode > 0 ) && ( Node( thisZoneInletNode ).MassFlowRate > 0.0 ) ) {
							SumMdotTemp += Node( thisZoneInletNode ).MassFlowRate * Node( thisZoneInletNode ).Temp;
							SumMdot += Node( thisZoneInletNode ).MassFlowRate;
						}
					}
				}
				if ( SumMdot > 0.0 ) {
					SupplyAirTemp = SumMdotTemp / SumMdot; // mass flow weighted inlet temperature
				} else {
					if ( thisZoneInletNode > 0 ) {
						SupplyAirTemp = Node( thisZoneInletNode ).Temp;
					} else {
						SupplyAirTemp = Node( ZoneNode ).Temp;
					}
				}
			} else {
				AirChangeRate = 0.0;
				SupplyAirTemp = Node( ZoneNode ).Temp;
			}
			tmpHc = CalcBeausoleilMorrisonMixedAssistedWall( ( TH( 2, 1, SurfNum ) - MAT( ZoneNum ) ), Surface( SurfNum ).IntConvZoneWallHeight, TH( 2, 1, SurfNum ), SupplyAirTemp, AirChangeRate, ZoneNum );
			Surface( SurfNum ).TAirRef = ZoneMeanAirTemp;
		} else if ( SELECT_CASE_var == HcInt_BeausoleilMorrisonMixedOppossingWall ) {
			ZoneNum = Surface( SurfNum ).Zone;
			ZoneNode = Zone( ZoneNum ).SystemZoneNodeNumber;
			if ( ZoneNode > 0 ) {
				ZoneMult = Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier;
				AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, Node( ZoneNode ).Temp, PsyWFnTdpPb( Node( ZoneNode ).Temp, OutBaroPress ) );
				AirChangeRate = ( Node( ZoneNode ).MassFlowRate * SecInHour ) / ( AirDensity * Zone( ZoneNum ).Volume * ZoneMult );
				SumMdotTemp = 0.0;
				SumMdot = 0.0;
				for ( EquipNum = 1; EquipNum <= ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).NumOfEquipTypes; ++EquipNum ) {
					if ( ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).EquipData( EquipNum ).NumOutlets > 0 ) {
						thisZoneInletNode = ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).EquipData( EquipNum ).OutletNodeNums( 1 );
						if ( ( thisZoneInletNode > 0 ) && ( Node( thisZoneInletNode ).MassFlowRate > 0.0 ) ) {
							SumMdotTemp += Node( thisZoneInletNode ).MassFlowRate * Node( thisZoneInletNode ).Temp;
							SumMdot += Node( thisZoneInletNode ).MassFlowRate;
						}
					}
				}
				if ( SumMdot > 0.0 ) {
					SupplyAirTemp = SumMdotTemp / SumMdot; // mass flow weighted inlet temperature
				} else {
					if ( thisZoneInletNode > 0 ) {
						SupplyAirTemp = Node( thisZoneInletNode ).Temp;
					} else {
						SupplyAirTemp = Node( ZoneNode ).Temp;
					}
				}
			} else {
				AirChangeRate = 0.0;
				SupplyAirTemp = Node( ZoneNode ).Temp;
			}

			tmpHc = CalcBeausoleilMorrisonMixedOpposingWall( ( TH( 2, 1, SurfNum ) - MAT( ZoneNum ) ), Surface( SurfNum ).IntConvZoneWallHeight, TH( 2, 1, SurfNum ), SupplyAirTemp, AirChangeRate, ZoneNum );

			Surface( SurfNum ).TAirRef = ZoneMeanAirTemp;
		} else if ( SELECT_CASE_var == HcInt_BeausoleilMorrisonMixedStableCeiling ) {
			ZoneNum = Surface( SurfNum ).Zone;
			ZoneNode = Zone( ZoneNum ).SystemZoneNodeNumber;
			if ( ZoneNode > 0 ) {
				ZoneMult = Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier;
				AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, Node( ZoneNode ).Temp, PsyWFnTdpPb( Node( ZoneNode ).Temp, OutBaroPress ) );
				AirChangeRate = ( Node( ZoneNode ).MassFlowRate * SecInHour ) / ( AirDensity * Zone( ZoneNum ).Volume * ZoneMult );
				SumMdotTemp = 0.0;
				SumMdot = 0.0;
				for ( EquipNum = 1; EquipNum <= ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).NumOfEquipTypes; ++EquipNum ) {
					if ( ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).EquipData( EquipNum ).NumOutlets > 0 ) {
						thisZoneInletNode = ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).EquipData( EquipNum ).OutletNodeNums( 1 );
						if ( ( thisZoneInletNode > 0 ) && ( Node( thisZoneInletNode ).MassFlowRate > 0.0 ) ) {
							SumMdotTemp += Node( thisZoneInletNode ).MassFlowRate * Node( thisZoneInletNode ).Temp;
							SumMdot += Node( thisZoneInletNode ).MassFlowRate;
						}
					}
				}
				if ( SumMdot > 0.0 ) {
					SupplyAirTemp = SumMdotTemp / SumMdot; // mass flow weighted inlet temperature
				} else {
					if ( thisZoneInletNode > 0 ) {
						SupplyAirTemp = Node( thisZoneInletNode ).Temp;
					} else {
						SupplyAirTemp = Node( ZoneNode ).Temp;
					}
				}
			} else {
				AirChangeRate = 0.0;
				SupplyAirTemp = Node( ZoneNode ).Temp;
			}
			tmpHc = CalcBeausoleilMorrisonMixedStableCeiling( ( TH( 2, 1, SurfNum ) - MAT( ZoneNum ) ), Surface( SurfNum ).IntConvZoneHorizHydrDiam, TH( 2, 1, SurfNum ), SupplyAirTemp, AirChangeRate, ZoneNum );
			Surface( SurfNum ).TAirRef = ZoneMeanAirTemp;
		} else if ( SELECT_CASE_var == HcInt_BeausoleilMorrisonMixedUnstableCeiling ) {
			ZoneNum = Surface( SurfNum ).Zone;
			ZoneNode = Zone( ZoneNum ).SystemZoneNodeNumber;
			if ( ZoneNode > 0 ) {
				ZoneMult = Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier;
				AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, Node( ZoneNode ).Temp, PsyWFnTdpPb( Node( ZoneNode ).Temp, OutBaroPress ) );
				AirChangeRate = ( Node( ZoneNode ).MassFlowRate * SecInHour ) / ( AirDensity * Zone( ZoneNum ).Volume * ZoneMult );
				SumMdotTemp = 0.0;
				SumMdot = 0.0;
				for ( EquipNum = 1; EquipNum <= ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).NumOfEquipTypes; ++EquipNum ) {
					if ( ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).EquipData( EquipNum ).NumOutlets > 0 ) {
						thisZoneInletNode = ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).EquipData( EquipNum ).OutletNodeNums( 1 );
						if ( ( thisZoneInletNode > 0 ) && ( Node( thisZoneInletNode ).MassFlowRate > 0.0 ) ) {
							SumMdotTemp += Node( thisZoneInletNode ).MassFlowRate * Node( thisZoneInletNode ).Temp;
							SumMdot += Node( thisZoneInletNode ).MassFlowRate;
						}
					}
				}
				if ( SumMdot > 0.0 ) {
					SupplyAirTemp = SumMdotTemp / SumMdot; // mass flow weighted inlet temperature
				} else {
					if ( thisZoneInletNode > 0 ) {
						SupplyAirTemp = Node( thisZoneInletNode ).Temp;
					} else {
						SupplyAirTemp = Node( ZoneNode ).Temp;
					}
				}
			} else {
				AirChangeRate = 0.0;
				SupplyAirTemp = Node( ZoneNode ).Temp;
			}
			tmpHc = CalcBeausoleilMorrisonMixedUnstableCeiling( ( TH( 2, 1, SurfNum ) - MAT( ZoneNum ) ), Surface( SurfNum ).IntConvZoneHorizHydrDiam, TH( 2, 1, SurfNum ), SupplyAirTemp, AirChangeRate, ZoneNum );
			Surface( SurfNum ).TAirRef = ZoneMeanAirTemp;
		} else if ( SELECT_CASE_var == HcInt_BeausoleilMorrisonMixedStableFloor ) {
			ZoneNum = Surface( SurfNum ).Zone;
			ZoneNode = Zone( ZoneNum ).SystemZoneNodeNumber;
			if ( ZoneNode > 0 ) {
				ZoneMult = Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier;
				AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, Node( ZoneNode ).Temp, PsyWFnTdpPb( Node( ZoneNode ).Temp, OutBaroPress ) );
				AirChangeRate = ( Node( ZoneNode ).MassFlowRate * SecInHour ) / ( AirDensity * Zone( ZoneNum ).Volume * ZoneMult );
				SumMdotTemp = 0.0;
				SumMdot = 0.0;
				for ( EquipNum = 1; EquipNum <= ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).NumOfEquipTypes; ++EquipNum ) {
					if ( ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).EquipData( EquipNum ).NumOutlets > 0 ) {
						thisZoneInletNode = ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).EquipData( EquipNum ).OutletNodeNums( 1 );
						if ( ( thisZoneInletNode > 0 ) && ( Node( thisZoneInletNode ).MassFlowRate > 0.0 ) ) {
							SumMdotTemp += Node( thisZoneInletNode ).MassFlowRate * Node( thisZoneInletNode ).Temp;
							SumMdot += Node( thisZoneInletNode ).MassFlowRate;
						}
					}
				}
				if ( SumMdot > 0.0 ) {
					SupplyAirTemp = SumMdotTemp / SumMdot; // mass flow weighted inlet temperature
				} else {
					if ( thisZoneInletNode > 0 ) {
						SupplyAirTemp = Node( thisZoneInletNode ).Temp;
					} else {
						SupplyAirTemp = Node( ZoneNode ).Temp;
					}
				}
			} else {
				AirChangeRate = 0.0;
				SupplyAirTemp = Node( ZoneNode ).Temp;
			}
			tmpHc = CalcBeausoleilMorrisonMixedStableFloor( ( TH( 2, 1, SurfNum ) - MAT( ZoneNum ) ), Surface( SurfNum ).IntConvZoneHorizHydrDiam, TH( 2, 1, SurfNum ), SupplyAirTemp, AirChangeRate, ZoneNum );
			Surface( SurfNum ).TAirRef = ZoneMeanAirTemp;
		} else if ( SELECT_CASE_var == HcInt_BeausoleilMorrisonMixedUnstableFloor ) {
			ZoneNum = Surface( SurfNum ).Zone;
			ZoneNode = Zone( ZoneNum ).SystemZoneNodeNumber;
			if ( ZoneNode > 0.0 ) {
				ZoneMult = Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier;
				AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, Node( ZoneNode ).Temp, PsyWFnTdpPb( Node( ZoneNode ).Temp, OutBaroPress ) );
				AirChangeRate = ( Node( ZoneNode ).MassFlowRate * SecInHour ) / ( AirDensity * Zone( ZoneNum ).Volume * ZoneMult );
				SumMdotTemp = 0.0;
				SumMdot = 0.0;
				for ( EquipNum = 1; EquipNum <= ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).NumOfEquipTypes; ++EquipNum ) {
					if ( ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).EquipData( EquipNum ).NumOutlets > 0 ) {
						thisZoneInletNode = ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).EquipData( EquipNum ).OutletNodeNums( 1 );
						if ( ( thisZoneInletNode > 0 ) && ( Node( thisZoneInletNode ).MassFlowRate > 0.0 ) ) {
							SumMdotTemp += Node( thisZoneInletNode ).MassFlowRate * Node( thisZoneInletNode ).Temp;
							SumMdot += Node( thisZoneInletNode ).MassFlowRate;
						}
					}
				}
				if ( SumMdot > 0.0 ) {
					SupplyAirTemp = SumMdotTemp / SumMdot; // mass flow weighted inlet temperature
				} else {
					if ( thisZoneInletNode > 0 ) {
						SupplyAirTemp = Node( thisZoneInletNode ).Temp;
					} else {
						SupplyAirTemp = Node( ZoneNode ).Temp;
					}
				}
			} else {
				AirChangeRate = 0.0;
				SupplyAirTemp = Node( ZoneNode ).Temp;
			}
			tmpHc = CalcBeausoleilMorrisonMixedUnstableFloor( ( TH( 2, 1, SurfNum ) - MAT( ZoneNum ) ), Surface( SurfNum ).IntConvZoneHorizHydrDiam, TH( 2, 1, SurfNum ), SupplyAirTemp, AirChangeRate, ZoneNum );
			Surface( SurfNum ).TAirRef = ZoneMeanAirTemp;
		} else if ( SELECT_CASE_var == HcInt_FohannoPolidoriVerticalWall ) {
			ZoneNum = Surface( SurfNum ).Zone;
			tmpHc = CalcFohannoPolidoriVerticalWall( ( TH( 2, 1, SurfNum ) - MAT( ZoneNum ) ), Surface( SurfNum ).IntConvZoneWallHeight, TH( 2, 1, SurfNum ), -QdotConvInRepPerArea( SurfNum ), SurfNum );
			Surface( SurfNum ).TAirRef = ZoneMeanAirTemp;
		} else if ( SELECT_CASE_var == HcInt_KaradagChilledCeiling ) {
			ZoneNum = Surface( SurfNum ).Zone;
			tmpHc = CalcKaradagChilledCeiling( ( TH( 2, 1, SurfNum ) - MAT( ZoneNum ) ) );
			Surface( SurfNum ).TAirRef = ZoneMeanAirTemp;
		} else if ( SELECT_CASE_var == HcInt_ISO15099Windows ) {
			ZoneNum = Surface( SurfNum ).Zone;
			CalcISO15099WindowIntConvCoeff( SurfNum, TH( 2, 1, SurfNum ), MAT( ZoneNum ) );
			tmpHc = HConvIn( SurfNum );
			Surface( SurfNum ).TAirRef = ZoneMeanAirTemp;
		} else if ( SELECT_CASE_var == HcInt_GoldsteinNovoselacCeilingDiffuserWindow ) {
			ZoneNum = Surface( SurfNum ).Zone;
			ZoneNode = Zone( ZoneNum ).SystemZoneNodeNumber;
			if ( ZoneNode > 0 ) {
				ZoneMult = Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier;
				AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, Node( ZoneNode ).Temp, PsyWFnTdpPb( Node( ZoneNode ).Temp, OutBaroPress ) );
				AirSystemVolFlowRate = Node( ZoneNode ).MassFlowRate / ( AirDensity * ZoneMult );
				Surface( SurfNum ).TAirRef = ZoneSupplyAirTemp;
			} else {
				AirSystemVolFlowRate = 0.0;
				Surface( SurfNum ).TAirRef = ZoneMeanAirTemp;
			}
			tmpHc = CalcGoldsteinNovoselacCeilingDiffuserWindow( AirSystemVolFlowRate, Surface( SurfNum ).IntConvZonePerimLength, Surface( SurfNum ).IntConvWindowWallRatio, Surface( SurfNum ).IntConvWindowLocation, ZoneNum );

		} else if ( SELECT_CASE_var == HcInt_GoldsteinNovoselacCeilingDiffuserWalls ) {
			ZoneNum = Surface( SurfNum ).Zone;
			ZoneNode = Zone( ZoneNum ).SystemZoneNodeNumber;
			if ( ZoneNode > 0 ) {
				ZoneMult = Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier;
				AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, Node( ZoneNode ).Temp, PsyWFnTdpPb( Node( ZoneNode ).Temp, OutBaroPress ) );
				AirSystemVolFlowRate = Node( ZoneNode ).MassFlowRate / ( AirDensity * ZoneMult );
				Surface( SurfNum ).TAirRef = ZoneSupplyAirTemp;
			} else {
				AirSystemVolFlowRate = 0.0;
				Surface( SurfNum ).TAirRef = ZoneMeanAirTemp;
			}
			tmpHc = CalcGoldsteinNovoselacCeilingDiffuserWall( AirSystemVolFlowRate, Surface( SurfNum ).IntConvZonePerimLength, Surface( SurfNum ).IntConvWindowLocation, ZoneNum );

		} else if ( SELECT_CASE_var == HcInt_GoldsteinNovoselacCeilingDiffuserFloor ) {
			ZoneNum = Surface( SurfNum ).Zone;
			ZoneNode = Zone( ZoneNum ).SystemZoneNodeNumber;
			if ( ZoneNode > 0 ) {
				ZoneMult = Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier;
				AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, Node( ZoneNode ).Temp, PsyWFnTdpPb( Node( ZoneNode ).Temp, OutBaroPress ) );
				AirSystemVolFlowRate = Node( ZoneNode ).MassFlowRate / ( AirDensity * ZoneMult );
				Surface( SurfNum ).TAirRef = ZoneSupplyAirTemp;
			} else {
				AirSystemVolFlowRate = 0.0;
				Surface( SurfNum ).TAirRef = ZoneMeanAirTemp;
			}
			tmpHc = CalcGoldsteinNovoselacCeilingDiffuserFloor( AirSystemVolFlowRate, Surface( SurfNum ).IntConvZonePerimLength, ZoneNum );

		}}

		if ( tmpHc < AdaptiveHcInsideLowLimit ) tmpHc = AdaptiveHcInsideLowLimit;

		Hc = tmpHc;

	}

	void
	EvaluateExtHcModels(
		int const SurfNum,
		int const NaturalConvModelEqNum,
		int const ForcedConvModelEqNum,
		Real64 & Hc
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Grifith
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// central case statement for evaluating exterior specific convection models

		// METHODOLOGY EMPLOYED:
		// separated out long case statement for selecting models.

		// REFERENCES:

		// Using/Aliasing
		using DataHeatBalSurface::TH;
		using DataHeatBalSurface::QdotConvOutRepPerArea;
		using DataEnvironment::WindSpeed;
		using DataEnvironment::WindDir;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static Real64 Hf( 0.0 ); // the forced, or wind driven portion of film coefficient
		static Real64 Hn( 0.0 ); // the natural, or bouyancy driven portion of film coefficient
		int ConstructNum;
		Real64 SurfWindSpeed;
		Real64 HydraulicDiameter;

		// first call Hn models
		{ auto const SELECT_CASE_var( NaturalConvModelEqNum );

		if ( SELECT_CASE_var == HcExt_None ) {
			Hn = 0.0;
		} else if ( SELECT_CASE_var == HcExt_UserCurve ) {

			CalcUserDefinedOutsideHcModel( SurfNum, Surface( SurfNum ).OutConvHnUserCurveIndex, Hn );

		} else if ( SELECT_CASE_var == HcExt_NaturalASHRAEVerticalWall ) {
			Hn = CalcASHRAEVerticalWall( ( TH( 1, 1, SurfNum ) - Surface( SurfNum ).OutDryBulbTemp ) );
		} else if ( SELECT_CASE_var == HcExt_NaturalWaltonUnstableHorizontalOrTilt ) {
			Hn = CalcWaltonUnstableHorizontalOrTilt( ( TH( 1, 1, SurfNum ) - Surface( SurfNum ).OutDryBulbTemp ), Surface( SurfNum ).CosTilt ); //TODO verify CosTilt in vs out
		} else if ( SELECT_CASE_var == HcExt_NaturalWaltonStableHorizontalOrTilt ) {
			Hn = CalcWaltonStableHorizontalOrTilt( ( TH( 1, 1, SurfNum ) - Surface( SurfNum ).OutDryBulbTemp ), Surface( SurfNum ).CosTilt ); //TODO verify CosTilt in vs out
		} else if ( SELECT_CASE_var == HcExt_AlamdariHammondVerticalWall ) {

			Hn = CalcAlamdariHammondVerticalWall( ( TH( 1, 1, SurfNum ) - Surface( SurfNum ).OutDryBulbTemp ), Surface( SurfNum ).OutConvFaceHeight, SurfNum );
		} else if ( SELECT_CASE_var == HcExt_FohannoPolidoriVerticalWall ) {
			Hn = CalcFohannoPolidoriVerticalWall( ( TH( 1, 1, SurfNum ) - Surface( SurfNum ).OutDryBulbTemp ), Surface( SurfNum ).OutConvFaceHeight, TH( 1, 1, SurfNum ), -QdotConvOutRepPerArea( SurfNum ), SurfNum );
			//  CASE (HcExt_ISO15099Windows)

		} else if ( SELECT_CASE_var == HcExt_AlamdariHammondStableHorizontal ) {
			if ( Surface( SurfNum ).OutConvFacePerimeter > 0.0 ) {
				HydraulicDiameter = 4.0 * Surface( SurfNum ).OutConvFaceArea / Surface( SurfNum ).OutConvFacePerimeter;
			} else {
				HydraulicDiameter = std::sqrt( Surface( SurfNum ).OutConvFaceArea );
			}
			Hn = CalcAlamdariHammondStableHorizontal( ( TH( 1, 1, SurfNum ) - Surface( SurfNum ).OutDryBulbTemp ), HydraulicDiameter, SurfNum );
		} else if ( SELECT_CASE_var == HcExt_AlamdariHammondUnstableHorizontal ) {
			if ( Surface( SurfNum ).OutConvFacePerimeter > 0.0 ) {
				HydraulicDiameter = 4.0 * Surface( SurfNum ).OutConvFaceArea / Surface( SurfNum ).OutConvFacePerimeter;
			} else {
				HydraulicDiameter = std::sqrt( Surface( SurfNum ).OutConvFaceArea );
			}
			Hn = CalcAlamdariHammondUnstableHorizontal( ( TH( 1, 1, SurfNum ) - Surface( SurfNum ).OutDryBulbTemp ), HydraulicDiameter, SurfNum );
		}}

		{ auto const SELECT_CASE_var( ForcedConvModelEqNum );

		if ( SELECT_CASE_var == HcExt_None ) {
			Hf = 0.0;
		} else if ( SELECT_CASE_var == HcExt_UserCurve ) {
			CalcUserDefinedOutsideHcModel( SurfNum, Surface( SurfNum ).OutConvHfUserCurveIndex, Hf );
		} else if ( SELECT_CASE_var == HcExt_SparrowWindward ) {
			ConstructNum = Surface( SurfNum ).Construction;
			if ( ! Surface( SurfNum ).ExtWind ) {
				SurfWindSpeed = 0.0; // No wind exposure
			} else if ( Surface( SurfNum ).Class == SurfaceClass_Window && SurfaceWindow( SurfNum ).ShadingFlag == ExtShadeOn ) {
				SurfWindSpeed = 0.0; // Assume zero wind speed at outside glass surface of window with exterior shade
			} else {
				SurfWindSpeed = Surface( SurfNum ).WindSpeed;
			}
			Hf = CalcSparrowWindward( Material( Construct( ConstructNum ).LayerPoint( 1 ) ).Roughness, Surface( SurfNum ).OutConvFacePerimeter, Surface( SurfNum ).OutConvFaceArea, SurfWindSpeed, SurfNum );

		} else if ( SELECT_CASE_var == HcExt_SparrowLeeward ) {
			ConstructNum = Surface( SurfNum ).Construction;
			if ( ! Surface( SurfNum ).ExtWind ) {
				SurfWindSpeed = 0.0; // No wind exposure
			} else if ( Surface( SurfNum ).Class == SurfaceClass_Window && SurfaceWindow( SurfNum ).ShadingFlag == ExtShadeOn ) {
				SurfWindSpeed = 0.0; // Assume zero wind speed at outside glass surface of window with exterior shade
			} else {
				SurfWindSpeed = Surface( SurfNum ).WindSpeed;
			}
			Hf = CalcSparrowLeeward( Material( Construct( ConstructNum ).LayerPoint( 1 ) ).Roughness, Surface( SurfNum ).OutConvFacePerimeter, Surface( SurfNum ).OutConvFaceArea, SurfWindSpeed, SurfNum );
		} else if ( SELECT_CASE_var == HcExt_MoWiTTWindward ) {
			if ( ! Surface( SurfNum ).ExtWind ) {
				SurfWindSpeed = 0.0; // No wind exposure
			} else if ( Surface( SurfNum ).Class == SurfaceClass_Window && SurfaceWindow( SurfNum ).ShadingFlag == ExtShadeOn ) {
				SurfWindSpeed = 0.0; // Assume zero wind speed at outside glass surface of window with exterior shade
			} else {
				SurfWindSpeed = Surface( SurfNum ).WindSpeed;
			}
			Hf = CalcMoWITTWindward( TH( 1, 1, SurfNum ) - Surface( SurfNum ).OutDryBulbTemp, SurfWindSpeed );
		} else if ( SELECT_CASE_var == HcExt_MoWiTTLeeward ) {
			if ( ! Surface( SurfNum ).ExtWind ) {
				SurfWindSpeed = 0.0; // No wind exposure
			} else if ( Surface( SurfNum ).Class == SurfaceClass_Window && SurfaceWindow( SurfNum ).ShadingFlag == ExtShadeOn ) {
				SurfWindSpeed = 0.0; // Assume zero wind speed at outside glass surface of window with exterior shade
			} else {
				SurfWindSpeed = Surface( SurfNum ).WindSpeed;
			}
			Hf = CalcMoWITTLeeward( ( TH( 1, 1, SurfNum ) - Surface( SurfNum ).OutDryBulbTemp ), SurfWindSpeed );
		} else if ( SELECT_CASE_var == HcExt_DOE2Windward ) {
			ConstructNum = Surface( SurfNum ).Construction;
			if ( ! Surface( SurfNum ).ExtWind ) {
				SurfWindSpeed = 0.0; // No wind exposure
			} else if ( Surface( SurfNum ).Class == SurfaceClass_Window && SurfaceWindow( SurfNum ).ShadingFlag == ExtShadeOn ) {
				SurfWindSpeed = 0.0; // Assume zero wind speed at outside glass surface of window with exterior shade
			} else {
				SurfWindSpeed = Surface( SurfNum ).WindSpeed;
			}
			Hf = CalcDOE2Windward( TH( 1, 1, SurfNum ), Surface( SurfNum ).OutDryBulbTemp, Surface( SurfNum ).CosTilt, SurfWindSpeed, Material( Construct( ConstructNum ).LayerPoint( 1 ) ).Roughness );
		} else if ( SELECT_CASE_var == HcExt_DOE2Leeward ) {
			ConstructNum = Surface( SurfNum ).Construction;
			if ( ! Surface( SurfNum ).ExtWind ) {
				SurfWindSpeed = 0.0; // No wind exposure
			} else if ( Surface( SurfNum ).Class == SurfaceClass_Window && SurfaceWindow( SurfNum ).ShadingFlag == ExtShadeOn ) {
				SurfWindSpeed = 0.0; // Assume zero wind speed at outside glass surface of window with exterior shade
			} else {
				SurfWindSpeed = Surface( SurfNum ).WindSpeed;
			}
			Hf = CalcDOE2Leeward( TH( 1, 1, SurfNum ), Surface( SurfNum ).OutDryBulbTemp, Surface( SurfNum ).CosTilt, SurfWindSpeed, Material( Construct( ConstructNum ).LayerPoint( 1 ) ).Roughness );
		} else if ( SELECT_CASE_var == HcExt_NusseltJurges ) {
			if ( ! Surface( SurfNum ).ExtWind ) {
				SurfWindSpeed = 0.0; // No wind exposure
			} else if ( Surface( SurfNum ).Class == SurfaceClass_Window && SurfaceWindow( SurfNum ).ShadingFlag == ExtShadeOn ) {
				SurfWindSpeed = 0.0; // Assume zero wind speed at outside glass surface of window with exterior shade
			} else {
				SurfWindSpeed = Surface( SurfNum ).WindSpeed;
			}
			Hf = CalcNusseltJurges( SurfWindSpeed );

		} else if ( SELECT_CASE_var == HcExt_McAdams ) {
			if ( ! Surface( SurfNum ).ExtWind ) {
				SurfWindSpeed = 0.0; // No wind exposure
			} else if ( Surface( SurfNum ).Class == SurfaceClass_Window && SurfaceWindow( SurfNum ).ShadingFlag == ExtShadeOn ) {
				SurfWindSpeed = 0.0; // Assume zero wind speed at outside glass surface of window with exterior shade
			} else {
				SurfWindSpeed = Surface( SurfNum ).WindSpeed;
			}
			Hf = CalcMcAdams( SurfWindSpeed );
		} else if ( SELECT_CASE_var == HcExt_Mitchell ) {
			if ( ! Surface( SurfNum ).ExtWind ) {
				SurfWindSpeed = 0.0; // No wind exposure
			} else if ( Surface( SurfNum ).Class == SurfaceClass_Window && SurfaceWindow( SurfNum ).ShadingFlag == ExtShadeOn ) {
				SurfWindSpeed = 0.0; // Assume zero wind speed at outside glass surface of window with exterior shade
			} else {
				SurfWindSpeed = Surface( SurfNum ).WindSpeed;
			}
			Hf = CalcMitchell( SurfWindSpeed, CubeRootOfOverallBuildingVolume, SurfNum );

		} else if ( SELECT_CASE_var == HcExt_ClearRoof ) {
			if ( ! Surface( SurfNum ).ExtWind ) {
				SurfWindSpeed = 0.0; // No wind exposure
			} else if ( Surface( SurfNum ).Class == SurfaceClass_Window && SurfaceWindow( SurfNum ).ShadingFlag == ExtShadeOn ) {
				SurfWindSpeed = 0.0; // Assume zero wind speed at outside glass surface of window with exterior shade
			} else {
				SurfWindSpeed = Surface( SurfNum ).WindSpeed;
			}
			Hf = CalcClearRoof( SurfNum, TH( 1, 1, SurfNum ), Surface( SurfNum ).OutDryBulbTemp, SurfWindSpeed, WindDir, Surface( SurfNum ).OutConvFaceArea, Surface( SurfNum ).OutConvFacePerimeter );
		} else if ( SELECT_CASE_var == HcExt_BlockenWindward ) {
			Hf = CalcBlockenWindward( WindSpeed, WindDir, Surface( SurfNum ).Azimuth );
		} else if ( SELECT_CASE_var == HcExt_EmmelVertical ) {
			Hf = CalcEmmelVertical( WindSpeed, WindDir, Surface( SurfNum ).Azimuth, SurfNum );
		} else if ( SELECT_CASE_var == HcExt_EmmelRoof ) {

			Hf = CalcEmmelRoof( WindSpeed, WindDir, RoofLongAxisOutwardAzimuth, SurfNum );

		}}

		Hc = Hf + Hn;
		if ( Hc < AdaptiveHcOutsideLowLimit ) Hc = AdaptiveHcOutsideLowLimit;

	}

	void
	DynamicExtConvSurfaceClassification( int const SurfNum ) // surface number
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   August 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// Decide surface classification based on wind and bouyancy, class, orientation

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHeatBalSurface::TH;
		using DataEnvironment::WindDir;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static Real64 DeltaTemp( 0.0 );

		if ( Surface( SurfNum ).Class == SurfaceClass_Roof ) {
			DeltaTemp = TH( 1, 1, SurfNum ) - Surface( SurfNum ).OutDryBulbTemp;
			if ( DeltaTemp < 0.0 ) {
				Surface( SurfNum ).OutConvClassification = OutConvClass_RoofStable;
			} else {
				Surface( SurfNum ).OutConvClassification = OutConvClass_RoofUnstable;
			}

		} else {

			if ( Windward( Surface( SurfNum ).CosTilt, Surface( SurfNum ).Azimuth, WindDir ) ) {
				Surface( SurfNum ).OutConvClassification = OutConvClass_WindwardVertWall;
			} else {
				Surface( SurfNum ).OutConvClassification = OutConvClass_LeewardVertWall;
			}
		}

	}

	void
	MapExtConvClassificationToHcModels( int const SurfNum ) // surface number
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na
		{ auto const SELECT_CASE_var( Surface( SurfNum ).OutConvClassification );

		if ( SELECT_CASE_var == OutConvClass_WindwardVertWall ) {
			Surface( SurfNum ).OutConvHfModelEq = OutsideFaceAdaptiveConvectionAlgo.HWindWallWindwardEqNum;
			if ( Surface( SurfNum ).OutConvHfModelEq == HcExt_UserCurve ) {
				Surface( SurfNum ).OutConvHfUserCurveIndex = OutsideFaceAdaptiveConvectionAlgo.HWindWallWindwardUserCurveNum;
			}
			Surface( SurfNum ).OutConvHnModelEq = OutsideFaceAdaptiveConvectionAlgo.HNatVertWallEqNum;
			if ( Surface( SurfNum ).OutConvHnModelEq == HcExt_UserCurve ) {
				Surface( SurfNum ).OutConvHnUserCurveIndex = OutsideFaceAdaptiveConvectionAlgo.HNatVertWallUserCurveNum;
			}
		} else if ( SELECT_CASE_var == OutConvClass_LeewardVertWall ) {
			Surface( SurfNum ).OutConvHfModelEq = OutsideFaceAdaptiveConvectionAlgo.HWindWallLeewardEqNum;
			if ( Surface( SurfNum ).OutConvHfModelEq == HcExt_UserCurve ) {
				Surface( SurfNum ).OutConvHfUserCurveIndex = OutsideFaceAdaptiveConvectionAlgo.HWindWallLeewardUserCurveNum;
			}
			Surface( SurfNum ).OutConvHnModelEq = OutsideFaceAdaptiveConvectionAlgo.HNatVertWallEqNum;
			if ( Surface( SurfNum ).OutConvHnModelEq == HcExt_UserCurve ) {
				Surface( SurfNum ).OutConvHfUserCurveIndex = OutsideFaceAdaptiveConvectionAlgo.HNatVertWallUserCurveNum;
			}

		} else if ( SELECT_CASE_var == OutConvClass_RoofStable ) {
			Surface( SurfNum ).OutConvHfModelEq = OutsideFaceAdaptiveConvectionAlgo.HWindHorizRoofEqNum;
			if ( Surface( SurfNum ).OutConvHfModelEq == HcExt_UserCurve ) {
				Surface( SurfNum ).OutConvHfUserCurveIndex = OutsideFaceAdaptiveConvectionAlgo.HWindHorizRoofUserCurveNum;
			}
			Surface( SurfNum ).OutConvHnModelEq = OutsideFaceAdaptiveConvectionAlgo.HNatStableHorizEqNum;
			if ( Surface( SurfNum ).OutConvHnModelEq == HcExt_UserCurve ) {
				Surface( SurfNum ).OutConvHfUserCurveIndex = OutsideFaceAdaptiveConvectionAlgo.HNatStableHorizUserCurveNum;
			}
		} else if ( SELECT_CASE_var == OutConvClass_RoofUnstable ) {
			Surface( SurfNum ).OutConvHfModelEq = OutsideFaceAdaptiveConvectionAlgo.HWindHorizRoofEqNum;
			if ( Surface( SurfNum ).OutConvHfModelEq == HcExt_UserCurve ) {
				Surface( SurfNum ).OutConvHfUserCurveIndex = OutsideFaceAdaptiveConvectionAlgo.HWindHorizRoofUserCurveNum;
			}
			Surface( SurfNum ).OutConvHnModelEq = OutsideFaceAdaptiveConvectionAlgo.HNatUnstableHorizEqNum;
			if ( Surface( SurfNum ).OutConvHnModelEq == HcExt_UserCurve ) {
				Surface( SurfNum ).OutConvHfUserCurveIndex = OutsideFaceAdaptiveConvectionAlgo.HNatUstableHorizUserCurveNum;
			}
		} else {
			ShowSevereError( "MapExtConvClassificationToHcModels: caught unknown outdoor surfce classification:" + RoundSigDigits( Surface( SurfNum ).OutConvClassification ) );
		}}

	}

	void
	DynamicIntConvSurfaceClassification( int const SurfNum ) // surface number
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR        Brent Griffith
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// collects dynamic updates needed for adaptive convectin algorithm

		// METHODOLOGY EMPLOYED:
		// Decide flow regime to set IntConvClassification
		//  done by zone using the following rules

		// Using zone flow regime, and surface's characteristics assign IntConvHcModelEq

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataZoneEquipment;
		using DataHeatBalSurface::TH;
		using DataHeatBalFanSys::MAT;
		using DataEnvironment::OutBaroPress;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyWFnTdpPb;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const g( 9.81 ); // gravity constant (m/s**2)
		Real64 const v( 15.89e-6 ); // kinematic viscosity (m**2/s) for air at 300 K
		Real64 const ActiveDelTempThreshold( 1.5 ); // deg C, temperature difference for surfaces to be considered "active"

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int ZoneNum( 0 );
		static int PriorityEquipOn( 0 );
		static Array1D_int HeatingPriorityStack( {0,10}, 0 );
		static Array1D_int CoolingPriorityStack( {0,10}, 0 );
		static Array1D_int FlowRegimeStack( {0,10}, 0 );
		static int EquipNum( 0 );
		static int ZoneNode( 0 );
		static int EquipOnCount( 0 );
		static int EquipOnLoop( 0 );
		static int thisZoneInletNode( 0 );
		//  INTEGER :: thisZnEqInletNode = 0
		static int FinalFlowRegime( 0 );
		static Real64 Tmin( 0.0 ); // temporary min surf temp
		static Real64 Tmax( 0.0 ); // temporary max surf temp
		static Real64 GrH( 0.0 ); // Grashof number for zone height H
		static Real64 Re( 0.0 ); // Reynolds number for zone air system flow
		static Real64 Ri( 0.0 ); // Richardson Number, Gr/Re**2 for determining mixed regime
		static Real64 AirDensity( 0.0 ); // temporary zone air density
		static Real64 DeltaTemp( 0.0 ); // temporary temperature difference (Tsurf - Tair)
		int SurfLoop; // local for separate looping across surfaces in the zone that has SurfNum

		EquipOnCount = 0;
		ZoneNum = Surface( SurfNum ).Zone;
		ZoneNode = Zone( ZoneNum ).SystemZoneNodeNumber;
		FlowRegimeStack = 0;

		//HVAC connections
		if ( ! Zone( ZoneNum ).IsControlled ) { // no HVAC control
			FlowRegimeStack( 0 ) = InConvFlowRegime_A3;
		} else { // is controlled, lets see by how and if that means is currently active

			if ( ! ( ZoneEquipConfig( ZoneNum ).EquipListIndex > 0 ) ) {
				FlowRegimeStack( 0 ) = InConvFlowRegime_A3;
			} else {

				for ( EquipNum = 1; EquipNum <= ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).NumOfEquipTypes; ++EquipNum ) {

					{ auto const SELECT_CASE_var( ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).EquipType_Num( EquipNum ) );

					if ( ( SELECT_CASE_var == AirDistUnit_Num ) || ( SELECT_CASE_var == DirectAir_Num ) || ( SELECT_CASE_var == PurchasedAir_Num ) ) { // central air equipment
						if ( ! ( allocated( ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).EquipData( EquipNum ).OutletNodeNums ) ) ) continue;
						//get inlet node, not zone node if possible
						thisZoneInletNode = ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).EquipData( EquipNum ).OutletNodeNums( 1 );
						if ( thisZoneInletNode > 0 ) {
							if ( Node( thisZoneInletNode ).MassFlowRate > 0.0 ) {
								EquipOnCount = min( EquipOnCount + 1, 10 );
								FlowRegimeStack( EquipOnCount ) = InConvFlowRegime_C;
								HeatingPriorityStack( EquipOnCount ) = ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).HeatingPriority( EquipNum );
								CoolingPriorityStack( EquipOnCount ) = ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).CoolingPriority( EquipNum );
							}
						} else {
							if ( Node( ZoneNode ).MassFlowRate > 0.0 ) {
								EquipOnCount = min( EquipOnCount + 1, 10 );
								FlowRegimeStack( EquipOnCount ) = InConvFlowRegime_C;
								HeatingPriorityStack( EquipOnCount ) = ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).HeatingPriority( EquipNum );
								CoolingPriorityStack( EquipOnCount ) = ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).CoolingPriority( EquipNum );
							}
						}
					} else if ( ( SELECT_CASE_var == WindowAC_Num ) || ( SELECT_CASE_var == PkgTermHPAirToAir_Num ) || ( SELECT_CASE_var == PkgTermACAirToAir_Num ) || ( SELECT_CASE_var == ZoneDXDehumidifier_Num ) || ( SELECT_CASE_var == PkgTermHPWaterToAir_Num ) || ( SELECT_CASE_var == FanCoil4Pipe_Num ) || ( SELECT_CASE_var == UnitVentilator_Num ) || ( SELECT_CASE_var == UnitHeater_Num ) || ( SELECT_CASE_var == OutdoorAirUnit_Num ) ) {
						if ( ! ( allocated( ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).EquipData( EquipNum ).OutletNodeNums ) ) ) continue;
						thisZoneInletNode = ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).EquipData( EquipNum ).OutletNodeNums( 1 );
						if ( thisZoneInletNode > 0 ) {
							if ( Node( thisZoneInletNode ).MassFlowRate > 0.0 ) {
								EquipOnCount = min( EquipOnCount + 1, 10 );
								FlowRegimeStack( EquipOnCount ) = InConvFlowRegime_D;
								HeatingPriorityStack( EquipOnCount ) = ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).HeatingPriority( EquipNum );
								CoolingPriorityStack( EquipOnCount ) = ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).CoolingPriority( EquipNum );
							}
						} else {
							if ( Node( ZoneNode ).MassFlowRate > 0.0 ) {
								EquipOnCount = min( EquipOnCount + 1, 10 );
								FlowRegimeStack( EquipOnCount ) = InConvFlowRegime_D;
								HeatingPriorityStack( EquipOnCount ) = ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).HeatingPriority( EquipNum );
								CoolingPriorityStack( EquipOnCount ) = ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).CoolingPriority( EquipNum );
							}
						}
					} else if ( ( SELECT_CASE_var == BBSteam_Num ) || ( SELECT_CASE_var == BBWaterConvective_Num ) || ( SELECT_CASE_var == BBElectricConvective_Num ) || ( SELECT_CASE_var == BBWater_Num ) ) {

						if ( ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).EquipData( EquipNum ).ON ) {
							EquipOnCount = min( EquipOnCount + 1, 10 );
							FlowRegimeStack( EquipOnCount ) = InConvFlowRegime_B;
							HeatingPriorityStack( EquipOnCount ) = ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).HeatingPriority( EquipNum );
							CoolingPriorityStack( EquipOnCount ) = ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).CoolingPriority( EquipNum );
						}
					} else if ( ( SELECT_CASE_var == BBElectric_Num ) || ( SELECT_CASE_var == HiTempRadiant_Num ) ) {
						if ( ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).EquipData( EquipNum ).ON ) {
							EquipOnCount = min( EquipOnCount + 1, 10 );
							FlowRegimeStack( EquipOnCount ) = InConvFlowRegime_B;
							HeatingPriorityStack( EquipOnCount ) = ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).HeatingPriority( EquipNum );
							CoolingPriorityStack( EquipOnCount ) = ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).CoolingPriority( EquipNum );
						}
					} else if ( ( SELECT_CASE_var == VentilatedSlab_Num ) || ( SELECT_CASE_var == LoTempRadiant_Num ) ) {

						if ( ZoneEquipConfig( ZoneNum ).InFloorActiveElement ) {
							for ( SurfLoop = Zone( ZoneNum ).SurfaceFirst; SurfLoop <= Zone( ZoneNum ).SurfaceLast; ++SurfLoop ) {
								if ( ! Surface( SurfLoop ).IntConvSurfHasActiveInIt ) continue;
								if ( Surface( SurfLoop ).Class == SurfaceClass_Floor ) {
									DeltaTemp = TH( 2, 1, SurfLoop ) - MAT( ZoneNum );
									if ( DeltaTemp > ActiveDelTempThreshold ) { // assume heating with floor
										// system ON is not enough because floor surfaces can continue to heat because of thermal capacity
										EquipOnCount = min( EquipOnCount + 1, 10 );
										FlowRegimeStack( EquipOnCount ) = InConvFlowRegime_A1;
										HeatingPriorityStack( EquipOnCount ) = ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).HeatingPriority( EquipNum );
										CoolingPriorityStack( EquipOnCount ) = ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).CoolingPriority( EquipNum );
										break;
									}
								}
							}
						}

						if ( ZoneEquipConfig( ZoneNum ).InCeilingActiveElement ) {
							for ( SurfLoop = Zone( ZoneNum ).SurfaceFirst; SurfLoop <= Zone( ZoneNum ).SurfaceLast; ++SurfLoop ) {
								if ( ! Surface( SurfLoop ).IntConvSurfHasActiveInIt ) continue;
								if ( Surface( SurfLoop ).Class == SurfaceClass_Roof ) {
									DeltaTemp = TH( 2, 1, SurfLoop ) - MAT( ZoneNum );
									if ( DeltaTemp < ActiveDelTempThreshold ) { // assume cooling with ceiling
										// system ON is not enough because  surfaces can continue to cool because of thermal capacity
										EquipOnCount = min( EquipOnCount + 1, 10 );
										FlowRegimeStack( EquipOnCount ) = InConvFlowRegime_A1;
										HeatingPriorityStack( EquipOnCount ) = ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).HeatingPriority( EquipNum );
										CoolingPriorityStack( EquipOnCount ) = ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).CoolingPriority( EquipNum );
										break;
									}
								}
							}
						}

						if ( ZoneEquipConfig( ZoneNum ).InWallActiveElement ) {
							for ( SurfLoop = Zone( ZoneNum ).SurfaceFirst; SurfLoop <= Zone( ZoneNum ).SurfaceLast; ++SurfLoop ) {
								if ( ! Surface( SurfLoop ).IntConvSurfHasActiveInIt ) continue;
								if ( Surface( SurfLoop ).Class == SurfaceClass_Wall || Surface( SurfLoop ).Class == SurfaceClass_Door ) {
									DeltaTemp = TH( 2, 1, SurfLoop ) - MAT( ZoneNum );
									if ( DeltaTemp > ActiveDelTempThreshold ) { // assume heating with wall panel
										// system ON is not enough because  surfaces can continue to heat because of thermal capacity
										EquipOnCount = min( EquipOnCount + 1, 10 );
										FlowRegimeStack( EquipOnCount ) = InConvFlowRegime_A2;
										HeatingPriorityStack( EquipOnCount ) = ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).HeatingPriority( EquipNum );
										CoolingPriorityStack( EquipOnCount ) = ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).CoolingPriority( EquipNum );
									} else { // not heating, no special models wall cooling so use simple bouyancy
										EquipOnCount = min( EquipOnCount + 1, 10 );
										FlowRegimeStack( EquipOnCount ) = InConvFlowRegime_A3;
										HeatingPriorityStack( EquipOnCount ) = ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).HeatingPriority( EquipNum );
										CoolingPriorityStack( EquipOnCount ) = ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).CoolingPriority( EquipNum );
									}
								}
							}
						}

					}}
				} //loop over equipment for this zone
			}
		}

		// now select which equipment type is dominant compared to all those that are ON
		if ( EquipOnCount > 0 ) {
			if ( SNLoadPredictedRate( ZoneNum ) >= 0.0 ) { // heating load
				PriorityEquipOn = 1;
				for ( EquipOnLoop = 1; EquipOnLoop <= EquipOnCount; ++EquipOnLoop ) {
					//assume highest priority/first sim order is dominant for flow regime
					if ( HeatingPriorityStack( EquipOnLoop ) < HeatingPriorityStack( PriorityEquipOn ) ) {
						PriorityEquipOn = EquipOnLoop;
					}
				}
			} else if ( SNLoadPredictedRate( ZoneNum ) < 0.0 ) { // cooling load
				PriorityEquipOn = 1;
				for ( EquipOnLoop = 1; EquipOnLoop <= EquipOnCount; ++EquipOnLoop ) {
					//assume highest priority/first sim order is dominant for flow regime
					if ( CoolingPriorityStack( EquipOnLoop ) < CoolingPriorityStack( PriorityEquipOn ) ) {
						PriorityEquipOn = EquipOnLoop;
					}
				}
			}
			FinalFlowRegime = FlowRegimeStack( PriorityEquipOn );
		} else {
			// no equipment on, so simple bouyancy flow regime
			FinalFlowRegime = InConvFlowRegime_A3;
		}

		// now if flow regimes C or D, then check for Mixed regime or very low flow rates
		if ( ( FinalFlowRegime == InConvFlowRegime_C ) || ( FinalFlowRegime == InConvFlowRegime_D ) ) {

			//Calculate Grashof, Reynolds, and Richardson numbers for the zone
			//Grashof for zone air based on largest delta T between surfaces and zone height
			Tmin = minval( TH( 2, 1, {Zone( ZoneNum ).SurfaceFirst,Zone( ZoneNum ).SurfaceLast} ) );
			Tmax = maxval( TH( 2, 1, {Zone( ZoneNum ).SurfaceFirst,Zone( ZoneNum ).SurfaceLast} ) );
			GrH = ( g * ( Tmax - Tmin ) * pow_3( Zone( ZoneNum ).CeilingHeight ) ) / ( ( MAT( ZoneNum ) + KelvinConv ) * pow_2( v ) );

			// Reynolds number = Vdot supply / v * cube root of zone volume (Goldstein and Noveselac 2010)
			if ( Node( ZoneNode ).MassFlowRate > 0.0 ) {
				AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, Node( ZoneNode ).Temp, PsyWFnTdpPb( Node( ZoneNode ).Temp, OutBaroPress ) );
				Re = Node( ZoneNode ).MassFlowRate / ( v * AirDensity * std::pow( Zone( ZoneNum ).Volume, OneThird ) );
			} else {
				Re = 0.0;
			}

			if ( Re > 0.0 ) {
				Ri = GrH / pow_2( Re ); //Richardson Number
				if ( Ri > 10.0 ) { // natural convection expected
					FinalFlowRegime = InConvFlowRegime_A3;
				} else if ( Ri < 0.1 ) { //forced
					// no change, already a forced regime
				} else { // mixed
					FinalFlowRegime = InConvFlowRegime_E;
				}
			} else { // natural convection expected
				FinalFlowRegime = InConvFlowRegime_A3;
			}
		}

		// now finish out specific model eq for this surface
		//Surface(SurfNum)%IntConvClassification = 0 !init/check
		{ auto const SELECT_CASE_var( FinalFlowRegime );

		if ( SELECT_CASE_var == InConvFlowRegime_A1 ) {
			DeltaTemp = TH( 2, 1, SurfNum ) - MAT( ZoneNum );
			if ( Surface( SurfNum ).Class == SurfaceClass_Wall || Surface( SurfNum ).Class == SurfaceClass_Door ) {

				if ( ( Surface( SurfNum ).Tilt > 85.0 ) && ( Surface( SurfNum ).Tilt < 95.0 ) ) { //vertical wall
					Surface( SurfNum ).IntConvClassification = InConvClass_A1_VertWalls;
				} else if ( Surface( SurfNum ).Tilt >= 95.0 ) { //tilted upwards
					if ( DeltaTemp > 0.0 ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_A1_UnstableTilted;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_A1_StableTilted;
					}
				} else if ( Surface( SurfNum ).Tilt <= 85.0 ) { //tilted downwards
					if ( DeltaTemp < 0.0 ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_A1_UnstableTilted;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_A1_StableTilted;
					}
				}

			} else if ( Surface( SurfNum ).Class == SurfaceClass_Roof ) {
				if ( Surface( SurfNum ).IntConvSurfHasActiveInIt ) {
					Surface( SurfNum ).IntConvClassification = InConvClass_A1_ChilledCeil;
				} else if ( Surface( SurfNum ).Tilt < 5.0 ) {
					if ( DeltaTemp < 0.0 ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_A1_UnstableHoriz;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_A1_StableHoriz;
					}
				} else if ( ( Surface( SurfNum ).Tilt >= 5.0 ) && ( ( Surface( SurfNum ).Tilt < 95.0 ) ) ) { //tilted downwards
					if ( DeltaTemp < 0.0 ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_A1_UnstableTilted;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_A1_StableTilted;
					}
				} else if ( ( Surface( SurfNum ).Tilt > 85.0 ) && ( Surface( SurfNum ).Tilt < 95.0 ) ) { //vertical wall
					Surface( SurfNum ).IntConvClassification = InConvClass_A1_VertWalls;
				} else if ( Surface( SurfNum ).Tilt >= 95.0 ) { //tilted upwards
					if ( DeltaTemp > 0.0 ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_A1_UnstableTilted;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_A1_StableTilted;
					}
				}

			} else if ( Surface( SurfNum ).Class == SurfaceClass_Floor ) {
				if ( Surface( SurfNum ).IntConvSurfHasActiveInIt ) {
					Surface( SurfNum ).IntConvClassification = InConvClass_A1_HeatedFloor;
				} else if ( Surface( SurfNum ).Tilt > 175.0 ) { //floor
					if ( DeltaTemp > 0.0 ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_A1_UnstableHoriz;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_A1_StableHoriz;
					}
				} else if ( ( Surface( SurfNum ).Tilt <= 175.0 ) && ( Surface( SurfNum ).Tilt >= 95.0 ) ) {
					if ( DeltaTemp > 0.0 ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_A1_UnstableTilted;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_A1_StableTilted;
					}
				}
			} else if ( ( Surface( SurfNum ).Class == SurfaceClass_Window ) || ( Surface( SurfNum ).Class == SurfaceClass_GlassDoor ) || ( Surface( SurfNum ).Class == SurfaceClass_TDD_Diffuser ) ) {
				Surface( SurfNum ).IntConvClassification = InConvClass_A1_Windows;
			} else if ( Surface( SurfNum ).Class == SurfaceClass_IntMass ) {
				// assume horizontal upwards.
				if ( DeltaTemp > 0.0 ) {
					Surface( SurfNum ).IntConvClassification = InConvClass_A1_UnstableHoriz;
				} else {
					Surface( SurfNum ).IntConvClassification = InConvClass_A1_StableHoriz;
				}
			}

			if ( Surface( SurfNum ).IntConvClassification == 0 ) {
				ShowSevereError( "DynamicIntConvSurfaceClassification: failed to resolve Hc model for A1 surface named" + Surface( SurfNum ).Name );
			}

		} else if ( SELECT_CASE_var == InConvFlowRegime_A2 ) {
			DeltaTemp = TH( 2, 1, SurfNum ) - MAT( ZoneNum );
			if ( Surface( SurfNum ).Class == SurfaceClass_Wall || Surface( SurfNum ).Class == SurfaceClass_Door ) {

				if ( Surface( SurfNum ).IntConvSurfHasActiveInIt ) {
					Surface( SurfNum ).IntConvClassification = InConvClass_A2_HeatedVerticalWall;
				} else if ( ( Surface( SurfNum ).Tilt > 85.0 ) && ( Surface( SurfNum ).Tilt < 95.0 ) ) { //vertical wall
					Surface( SurfNum ).IntConvClassification = InConvClass_A2_VertWallsNonHeated;
				} else if ( Surface( SurfNum ).Tilt >= 95.0 ) { //tilted upwards
					if ( DeltaTemp > 0.0 ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_A2_UnstableTilted;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_A2_StableTilted;
					}
				} else if ( Surface( SurfNum ).Tilt <= 85.0 ) { //tilted downwards
					if ( DeltaTemp < 0.0 ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_A2_UnstableTilted;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_A2_StableTilted;
					}
				}

			} else if ( Surface( SurfNum ).Class == SurfaceClass_Roof ) {
				if ( Surface( SurfNum ).Tilt < 5.0 ) {
					if ( DeltaTemp < 0.0 ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_A2_UnstableHoriz;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_A2_StableHoriz;
					}
				} else if ( ( Surface( SurfNum ).Tilt >= 5.0 ) && ( ( Surface( SurfNum ).Tilt < 95.0 ) ) ) { //tilted downwards
					if ( DeltaTemp < 0.0 ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_A2_UnstableTilted;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_A2_StableTilted;
					}
				} else if ( ( Surface( SurfNum ).Tilt > 85.0 ) && ( Surface( SurfNum ).Tilt < 95.0 ) ) { //vertical wall
					Surface( SurfNum ).IntConvClassification = InConvClass_A2_VertWallsNonHeated;
				} else if ( Surface( SurfNum ).Tilt >= 95.0 ) { //tilted upwards
					if ( DeltaTemp > 0.0 ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_A2_UnstableTilted;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_A2_StableTilted;
					}
				}

			} else if ( Surface( SurfNum ).Class == SurfaceClass_Floor ) {
				if ( Surface( SurfNum ).Tilt > 175.0 ) {
					if ( DeltaTemp > 0.0 ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_A2_UnstableHoriz;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_A2_StableHoriz;
					}
				} else if ( ( Surface( SurfNum ).Tilt <= 175.0 ) && ( Surface( SurfNum ).Tilt >= 95.0 ) ) {
					if ( DeltaTemp > 0.0 ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_A2_UnstableTilted;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_A2_StableTilted;
					}
				}
			} else if ( ( Surface( SurfNum ).Class == SurfaceClass_Window ) || ( Surface( SurfNum ).Class == SurfaceClass_GlassDoor ) || ( Surface( SurfNum ).Class == SurfaceClass_TDD_Diffuser ) ) {
				Surface( SurfNum ).IntConvClassification = InConvClass_A2_Windows;
			} else if ( Surface( SurfNum ).Class == SurfaceClass_IntMass ) {
				// assume horizontal upwards.
				if ( DeltaTemp > 0.0 ) {
					Surface( SurfNum ).IntConvClassification = InConvClass_A2_UnstableHoriz;
				} else {
					Surface( SurfNum ).IntConvClassification = InConvClass_A2_StableHoriz;
				}
			}

			if ( Surface( SurfNum ).IntConvClassification == 0 ) {
				ShowSevereError( "DynamicIntConvSurfaceClassification: failed to resolve Hc model for A2 surface named" + Surface( SurfNum ).Name );
			}
		} else if ( SELECT_CASE_var == InConvFlowRegime_A3 ) {
			DeltaTemp = TH( 2, 1, SurfNum ) - MAT( ZoneNum );
			if ( Surface( SurfNum ).Class == SurfaceClass_Wall || Surface( SurfNum ).Class == SurfaceClass_Door ) {

				if ( ( Surface( SurfNum ).Tilt > 85.0 ) && ( Surface( SurfNum ).Tilt < 95.0 ) ) { //vertical wall
					Surface( SurfNum ).IntConvClassification = InConvClass_A3_VertWalls;
				} else if ( Surface( SurfNum ).Tilt >= 95.0 ) { //tilted upwards
					if ( DeltaTemp > 0.0 ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_A3_UnstableTilted;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_A3_StableTilted;
					}
				} else if ( Surface( SurfNum ).Tilt <= 85.0 ) { //tilted downwards
					if ( DeltaTemp < 0.0 ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_A3_UnstableTilted;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_A3_StableTilted;
					}
				}

			} else if ( Surface( SurfNum ).Class == SurfaceClass_Roof ) {
				if ( Surface( SurfNum ).Tilt < 5.0 ) {
					if ( DeltaTemp < 0.0 ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_A3_UnstableHoriz;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_A3_StableHoriz;
					}
				} else if ( ( Surface( SurfNum ).Tilt > 5.0 ) && ( ( Surface( SurfNum ).Tilt < 85.0 ) ) ) { //tilted downwards
					if ( DeltaTemp < 0.0 ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_A3_UnstableTilted;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_A3_StableTilted;
					}
				} else if ( ( Surface( SurfNum ).Tilt > 85.0 ) && ( Surface( SurfNum ).Tilt < 95.0 ) ) { //vertical wall
					Surface( SurfNum ).IntConvClassification = InConvClass_A3_VertWalls;
				} else if ( Surface( SurfNum ).Tilt >= 95.0 ) { //tilted upwards
					if ( DeltaTemp > 0.0 ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_A3_UnstableTilted;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_A3_StableTilted;
					}
				}

			} else if ( Surface( SurfNum ).Class == SurfaceClass_Floor ) {
				if ( Surface( SurfNum ).Tilt > 175.0 ) {
					if ( DeltaTemp > 0.0 ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_A3_UnstableHoriz;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_A3_StableHoriz;
					}
				} else if ( ( Surface( SurfNum ).Tilt <= 175.0 ) && ( Surface( SurfNum ).Tilt >= 95.0 ) ) {
					if ( DeltaTemp > 0.0 ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_A3_UnstableTilted;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_A3_StableTilted;
					}
				}
			} else if ( ( Surface( SurfNum ).Class == SurfaceClass_Window ) || ( Surface( SurfNum ).Class == SurfaceClass_GlassDoor ) || ( Surface( SurfNum ).Class == SurfaceClass_TDD_Diffuser ) ) {
				Surface( SurfNum ).IntConvClassification = InConvClass_A3_Windows;
			} else if ( Surface( SurfNum ).Class == SurfaceClass_IntMass ) {
				// assume horizontal upwards.
				if ( DeltaTemp >= 0.0 ) {
					Surface( SurfNum ).IntConvClassification = InConvClass_A3_UnstableHoriz;
				} else {
					Surface( SurfNum ).IntConvClassification = InConvClass_A3_StableHoriz;
				}
			}

			if ( Surface( SurfNum ).IntConvClassification == 0 ) {
				ShowSevereError( "DynamicIntConvSurfaceClassification: failed to resolve Hc model for A3 surface named" + Surface( SurfNum ).Name );
			}
		} else if ( SELECT_CASE_var == InConvFlowRegime_B ) {
			DeltaTemp = TH( 2, 1, SurfNum ) - MAT( ZoneNum );
			if ( Surface( SurfNum ).Class == SurfaceClass_Wall || Surface( SurfNum ).Class == SurfaceClass_Door ) {

				if ( ( Surface( SurfNum ).Tilt > 85.0 ) && ( Surface( SurfNum ).Tilt < 95.0 ) ) { //vertical wall
					if ( Surface( SurfNum ).IntConvSurfGetsRadiantHeat ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_B_VertWallsNearHeat;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_B_VertWalls;
					}

				} else if ( Surface( SurfNum ).Tilt >= 95.0 ) { //tilted upwards
					if ( DeltaTemp > 0.0 ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_B_UnstableTilted;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_B_StableTilted;
					}
				} else if ( Surface( SurfNum ).Tilt <= 85.0 ) { //tilted downwards
					if ( DeltaTemp < 0.0 ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_B_UnstableTilted;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_B_StableTilted;
					}
				}

			} else if ( Surface( SurfNum ).Class == SurfaceClass_Roof ) {
				if ( Surface( SurfNum ).Tilt < 5.0 ) {
					if ( DeltaTemp < 0.0 ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_B_UnstableHoriz;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_B_StableHoriz;
					}
				} else if ( ( Surface( SurfNum ).Tilt >= 5.0 ) && ( ( Surface( SurfNum ).Tilt < 85.0 ) ) ) { //tilted downwards
					if ( DeltaTemp < 0.0 ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_B_UnstableTilted;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_B_StableTilted;
					}
				} else if ( ( Surface( SurfNum ).Tilt > 85.0 ) && ( Surface( SurfNum ).Tilt < 95.0 ) ) { //vertical wall
					if ( Surface( SurfNum ).IntConvSurfGetsRadiantHeat ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_B_VertWallsNearHeat;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_B_VertWalls;
					}
				} else if ( Surface( SurfNum ).Tilt >= 95.0 ) { //tilted upwards
					if ( DeltaTemp > 0.0 ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_B_UnstableTilted;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_B_StableTilted;
					}
				}

			} else if ( Surface( SurfNum ).Class == SurfaceClass_Floor ) {
				if ( Surface( SurfNum ).Tilt > 175.0 ) {
					if ( DeltaTemp > 0.0 ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_B_UnstableHoriz;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_B_StableHoriz;
					}
				} else if ( ( Surface( SurfNum ).Tilt <= 175.0 ) && ( Surface( SurfNum ).Tilt >= 95.0 ) ) {
					if ( DeltaTemp > 0.0 ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_B_UnstableTilted;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_B_StableTilted;
					}
				}
			} else if ( ( Surface( SurfNum ).Class == SurfaceClass_Window ) || ( Surface( SurfNum ).Class == SurfaceClass_GlassDoor ) || ( Surface( SurfNum ).Class == SurfaceClass_TDD_Diffuser ) ) {
				Surface( SurfNum ).IntConvClassification = InConvClass_B_Windows;
			} else if ( Surface( SurfNum ).Class == SurfaceClass_IntMass ) {
				// assume horizontal upwards.
				if ( DeltaTemp > 0.0 ) {
					Surface( SurfNum ).IntConvClassification = InConvClass_B_UnstableHoriz;
				} else {
					Surface( SurfNum ).IntConvClassification = InConvClass_B_StableHoriz;
				}
			}

			if ( Surface( SurfNum ).IntConvClassification == 0 ) {
				ShowSevereError( "DynamicIntConvSurfaceClassification: failed to resolve Hc model for B surface named" + Surface( SurfNum ).Name );
			}
		} else if ( SELECT_CASE_var == InConvFlowRegime_C ) {
			if ( Surface( SurfNum ).Class == SurfaceClass_Wall || Surface( SurfNum ).Class == SurfaceClass_Door ) {
				Surface( SurfNum ).IntConvClassification = InConvClass_C_Walls;
			} else if ( Surface( SurfNum ).Class == SurfaceClass_Roof ) {
				Surface( SurfNum ).IntConvClassification = InConvClass_C_Ceiling;
			} else if ( Surface( SurfNum ).Class == SurfaceClass_Floor ) {
				Surface( SurfNum ).IntConvClassification = InConvClass_C_Floor;
			} else if ( ( Surface( SurfNum ).Class == SurfaceClass_Window ) || ( Surface( SurfNum ).Class == SurfaceClass_GlassDoor ) || ( Surface( SurfNum ).Class == SurfaceClass_TDD_Diffuser ) ) {
				Surface( SurfNum ).IntConvClassification = InConvClass_C_Windows;
			} else if ( Surface( SurfNum ).Class == SurfaceClass_IntMass ) {
				Surface( SurfNum ).IntConvClassification = InConvClass_C_Floor;
			}
			if ( Surface( SurfNum ).IntConvClassification == 0 ) {
				ShowSevereError( "DynamicIntConvSurfaceClassification: failed to resolve Hc model for C surface named" + Surface( SurfNum ).Name );
			}

		} else if ( SELECT_CASE_var == InConvFlowRegime_D ) {

			DeltaTemp = TH( 2, 1, SurfNum ) - MAT( ZoneNum );
			if ( Surface( SurfNum ).Class == SurfaceClass_Wall || Surface( SurfNum ).Class == SurfaceClass_Door ) {

				if ( ( Surface( SurfNum ).Tilt > 85.0 ) && ( Surface( SurfNum ).Tilt < 95.0 ) ) { //vertical wall

					Surface( SurfNum ).IntConvClassification = InConvClass_D_Walls;

				} else if ( Surface( SurfNum ).Tilt >= 95.0 ) { //tilted upwards
					if ( DeltaTemp > 0.0 ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_D_UnstableTilted;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_D_StableTilted;
					}
				} else if ( Surface( SurfNum ).Tilt <= 85.0 ) { //tilted downwards
					if ( DeltaTemp < 0.0 ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_D_UnstableTilted;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_D_StableTilted;
					}
				}

			} else if ( Surface( SurfNum ).Class == SurfaceClass_Roof ) {
				if ( Surface( SurfNum ).Tilt < 5.0 ) {
					if ( DeltaTemp < 0.0 ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_D_UnstableHoriz;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_D_StableHoriz;
					}
				} else if ( ( Surface( SurfNum ).Tilt >= 5.0 ) && ( ( Surface( SurfNum ).Tilt <= 85.0 ) ) ) { //tilted downwards
					if ( DeltaTemp < 0.0 ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_D_UnstableTilted;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_D_StableTilted;
					}
				} else if ( ( Surface( SurfNum ).Tilt > 85.0 ) && ( Surface( SurfNum ).Tilt < 95.0 ) ) { //vertical wall

					Surface( SurfNum ).IntConvClassification = InConvClass_D_Walls;

				} else if ( Surface( SurfNum ).Tilt >= 95.0 ) { //tilted upwards
					if ( DeltaTemp > 0.0 ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_D_UnstableTilted;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_D_StableTilted;
					}
				}

			} else if ( Surface( SurfNum ).Class == SurfaceClass_Floor ) {
				if ( Surface( SurfNum ).Tilt > 175.0 ) { //floor
					if ( DeltaTemp > 0.0 ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_D_UnstableHoriz;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_D_StableHoriz;
					}
				} else if ( ( Surface( SurfNum ).Tilt <= 175.0 ) && ( Surface( SurfNum ).Tilt >= 95.0 ) ) {
					if ( DeltaTemp > 0.0 ) {
						Surface( SurfNum ).IntConvClassification = InConvClass_D_UnstableTilted;
					} else {
						Surface( SurfNum ).IntConvClassification = InConvClass_D_StableTilted;
					}
				}
			} else if ( ( Surface( SurfNum ).Class == SurfaceClass_Window ) || ( Surface( SurfNum ).Class == SurfaceClass_GlassDoor ) || ( Surface( SurfNum ).Class == SurfaceClass_TDD_Diffuser ) ) {
				Surface( SurfNum ).IntConvClassification = InConvClass_D_Windows;
			} else if ( Surface( SurfNum ).Class == SurfaceClass_IntMass ) {
				// assume horizontal upwards.
				if ( DeltaTemp > 0.0 ) {
					Surface( SurfNum ).IntConvClassification = InConvClass_D_UnstableHoriz;
				} else {
					Surface( SurfNum ).IntConvClassification = InConvClass_D_StableHoriz;
				}
			}

			if ( Surface( SurfNum ).IntConvClassification == 0 ) {
				ShowSevereError( "DynamicIntConvSurfaceClassification: failed to resolve Hc model for D surface named" + Surface( SurfNum ).Name );
			}

		} else if ( SELECT_CASE_var == InConvFlowRegime_E ) {

			DeltaTemp = TH( 2, 1, SurfNum ) - MAT( ZoneNum );
			if ( Surface( SurfNum ).Class == SurfaceClass_Wall || Surface( SurfNum ).Class == SurfaceClass_Door ) {

				//mixed regime, but need to know what regime it was before it was mixed
				{ auto const SELECT_CASE_var1( FlowRegimeStack( PriorityEquipOn ) );

				if ( SELECT_CASE_var1 == InConvFlowRegime_C ) {
					//assume forced flow is down along wall (ceiling diffuser)
					if ( DeltaTemp > 0.0 ) { // surface is hotter so plume upwards and forces oppose
						Surface( SurfNum ).IntConvClassification = InConvClass_E_OpposFlowWalls;
					} else { // surface is cooler so plume down and forces assist
						Surface( SurfNum ).IntConvClassification = InConvClass_E_AssistFlowWalls;
					}
				} else if ( SELECT_CASE_var1 == InConvFlowRegime_D ) {
					// assume forced flow is upward along wall (perimeter zone HVAC with fan)
					if ( DeltaTemp > 0.0 ) { // surface is hotter so plume up and forces assist
						Surface( SurfNum ).IntConvClassification = InConvClass_E_AssistFlowWalls;
					} else { // surface is cooler so plume downward and forces oppose
						Surface( SurfNum ).IntConvClassification = InConvClass_E_OpposFlowWalls;
					}
				}}

			} else if ( Surface( SurfNum ).Class == SurfaceClass_Roof ) {
				if ( DeltaTemp > 0.0 ) { //surface is hotter so stable
					Surface( SurfNum ).IntConvClassification = InConvClass_E_StableCeiling;
				} else {
					Surface( SurfNum ).IntConvClassification = InConvClass_E_UnstableCieling;
				}
			} else if ( Surface( SurfNum ).Class == SurfaceClass_Floor ) {
				if ( DeltaTemp > 0.0 ) { //surface is hotter so unstable
					Surface( SurfNum ).IntConvClassification = InConvClass_E_UnstableFloor;
				} else {
					Surface( SurfNum ).IntConvClassification = InConvClass_E_StableFloor;
				}
			} else if ( ( Surface( SurfNum ).Class == SurfaceClass_Window ) || ( Surface( SurfNum ).Class == SurfaceClass_GlassDoor ) || ( Surface( SurfNum ).Class == SurfaceClass_TDD_Diffuser ) ) {
				Surface( SurfNum ).IntConvClassification = InConvClass_E_Windows;
			} else if ( Surface( SurfNum ).Class == SurfaceClass_IntMass ) {
				if ( DeltaTemp > 0.0 ) {
					Surface( SurfNum ).IntConvClassification = InConvClass_E_UnstableFloor;
				} else {
					Surface( SurfNum ).IntConvClassification = InConvClass_E_StableFloor;
				}
			}
			if ( Surface( SurfNum ).IntConvClassification == 0 ) {
				ShowSevereError( "DynamicIntConvSurfaceClassification: failed to resolve Hc model for D surface named " + Surface( SurfNum ).Name );
			}
		} else {
			ShowSevereError( "DynamicIntConvSurfaceClassification: failed to deterime zone flow regime for surface named " + Surface( SurfNum ).Name );

		}}

	}

	void
	MapIntConvClassificationToHcModels( int const SurfNum ) // surface pointer index
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Map Hc model equation data from central structure to surface structure

		// METHODOLOGY EMPLOYED:
		// Long case statement depends on surface classification determined in DynamicIntConvSurfaceClassification
		// then simply map data stored in InsideFaceAdaptiveConvectionAlgo into the surface's structure
		// if model type is user-defined, also store the index to the user curve to be used.

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
		{ auto const SELECT_CASE_var( Surface( SurfNum ).IntConvClassification );

		if ( SELECT_CASE_var == InConvClass_A1_VertWalls ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolVertWallEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolVertWallUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_A1_StableHoriz ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolStableHorizEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolStableHorizUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_A1_UnstableHoriz ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolUnstableHorizEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolUnstableHorizUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_A1_HeatedFloor ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolHeatedFloorEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolHeatedFloorUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_A1_ChilledCeil ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolChilledCeilingEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolChilledCeilingUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_A1_StableTilted ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolStableTiltedEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolStableTiltedUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_A1_UnstableTilted ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolUnstableTiltedEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolUnstableTiltedUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_A1_Windows ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolWindowsEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolWindowsUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_A2_VertWallsNonHeated ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.WallPanelHeatVertWallEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.WallPanelHeatVertWallUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_A2_HeatedVerticalWall ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.WallPanelHeatHeatedWallEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.WallPanelHeatHeatedWallUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_A2_StableHoriz ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.WallPanelHeatStableHorizEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.WallPanelHeatStableHorizUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_A2_UnstableHoriz ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.WallPanelHeatUnstableHorizEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.WallPanelHeatUnstableHorizUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_A2_StableTilted ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.WallPanelHeatStableTiltedEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.WallPanelHeatStableTiltedUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_A2_UnstableTilted ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.WallPanelHeatUnstableTiltedEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.WallPanelHeatUnstableTiltedUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_A2_Windows ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.WallPanelHeatWindowsEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.WallPanelHeatWindowsUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_A3_VertWalls ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.SimpleBouyVertWallEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.SimpleBouyVertWallUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_A3_StableHoriz ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.SimpleBouyStableHorizEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.SimpleBouyStableHorizUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_A3_UnstableHoriz ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.SimpleBouyUnstableHorizEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.SimpleBouyUnstableHorizUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_A3_StableTilted ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.SimpleBouyStableTiltedEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.SimpleBouyStableTiltedUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_A3_UnstableTilted ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.SimpleBouyUnstableTiltedEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.SimpleBouyUnstableTiltedUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_A3_Windows ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.SimpleBouyWindowsEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.SimpleBouyWindowsUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_B_VertWalls ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatVertWallEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatVertWallUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_B_VertWallsNearHeat ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatVertWallNearHeaterEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatVertWallNearHeaterUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_B_StableHoriz ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatStableHorizEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatStableHorizUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_B_UnstableHoriz ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatUnstableHorizEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatUnstableHorizUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_B_StableTilted ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatStableTiltedEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatStableTiltedUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_B_UnstableTilted ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatUnstableTiltedEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatUnstableTiltedUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_B_Windows ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatWindowsEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatWindowsUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_C_Walls ) {
			if ( ( Surface( SurfNum ).IntConvZonePerimLength == 0.0 ) && ( InsideFaceAdaptiveConvectionAlgo.CentralAirWallEqNum == HcInt_GoldsteinNovoselacCeilingDiffuserWalls ) ) {
				// no perimeter, Goldstein Novolselac model not good so revert to fisher pedersen model
				Surface( SurfNum ).IntConvHcModelEq = HcInt_FisherPedersenCeilDiffuserWalls;
			} else {
				Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.CentralAirWallEqNum;
			}
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.CentralAirWallUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_C_Ceiling ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.CentralAirCeilingEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.CentralAirCeilingUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_C_Floor ) {
			if ( ( Surface( SurfNum ).IntConvZonePerimLength == 0.0 ) && ( InsideFaceAdaptiveConvectionAlgo.CentralAirFloorEqNum == HcInt_GoldsteinNovoselacCeilingDiffuserFloor ) ) {
				// no perimeter, Goldstein Novolselac model not good so revert to fisher pedersen model
				Surface( SurfNum ).IntConvHcModelEq = HcInt_FisherPedersenCeilDiffuserFloor;
			} else {
				Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.CentralAirFloorEqNum;
			}
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.CentralAirFloorUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_C_Windows ) {
			if ( ( Surface( SurfNum ).IntConvZonePerimLength == 0.0 ) && ( InsideFaceAdaptiveConvectionAlgo.CentralAirWindowsEqNum == HcInt_GoldsteinNovoselacCeilingDiffuserWindow ) ) {
				// no perimeter, Goldstein Novolselac model not good so revert to ISO15099
				Surface( SurfNum ).IntConvHcModelEq = HcInt_ISO15099Windows;
			} else {
				Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.CentralAirWindowsEqNum;
			}
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.CentralAirWindowsUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_D_Walls ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.ZoneFanCircVertWallEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.ZoneFanCircVertWallUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_D_StableHoriz ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.ZoneFanCircStableHorizEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.ZoneFanCircStableHorizUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_D_UnstableHoriz ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.ZoneFanCircUnstableHorizEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.ZoneFanCircUnstableHorizUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_D_StableTilted ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.ZoneFanCircStableTiltedEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.ZoneFanCircStableTiltedUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_D_UnstableTilted ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.ZoneFanCircUnstableTiltedEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.ZoneFanCircUnstableTiltedUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_D_Windows ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.ZoneFanCircWindowsEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.ZoneFanCircWindowsUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_E_AssistFlowWalls ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.MixedBouyAssistingFlowWallEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.MixedBouyAssistingFlowWallUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_E_OpposFlowWalls ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.MixedBouyOppossingFlowWallEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.MixedBouyOppossingFlowWallUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_E_StableFloor ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.MixedStableFloorEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.MixedStableFloorUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_E_UnstableFloor ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.MixedUnstableFloorEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.MixedUnstableFloorUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_E_StableCeiling ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.MixedStableCeilingEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.MixedStableCeilingUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_E_UnstableCieling ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.MixedUnstableCeilingEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.MixedUnstableCeilingUserCurveNum;
			}
		} else if ( SELECT_CASE_var == InConvClass_E_Windows ) {
			Surface( SurfNum ).IntConvHcModelEq = InsideFaceAdaptiveConvectionAlgo.MixedWindowsEqNum;
			if ( Surface( SurfNum ).IntConvHcModelEq == HcInt_UserCurve ) {
				Surface( SurfNum ).IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo.MixedWindowsUserCurveNum;
			}
		}}

	}

	void
	CalcUserDefinedInsideHcModel(
		int const SurfNum,
		int const UserCurveNum,
		Real64 & Hc
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// calculate user-defined convection correlations for inside face

		// METHODOLOGY EMPLOYED:
		// call curve objects to evaluate user's model equation
		// prepare independent parameters for x values

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataZoneEquipment;
		using DataHeatBalSurface::TH;
		using DataHeatBalFanSys::MAT;
		using DataEnvironment::OutBaroPress;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyWFnTdpPb;
		using CurveManager::CurveValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 tmpHc;
		Real64 tmpAirTemp;
		Real64 SupplyAirTemp;
		Real64 AirChangeRate;
		int ZoneNum;
		int ZoneNode;
		int EquipNum;
		Real64 SumMdotTemp;
		Real64 SumMdot;
		Real64 AirDensity;
		int thisZoneInletNode;

		ZoneNum = Surface( SurfNum ).Zone;
		SumMdotTemp = 0.0;
		SumMdot = 0.0;
		SupplyAirTemp = MAT( ZoneNum );
		if ( Zone( ZoneNum ).IsControlled ) {
			ZoneNode = Zone( ZoneNum ).SystemZoneNodeNumber;
			AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, Node( ZoneNode ).Temp, PsyWFnTdpPb( Node( ZoneNode ).Temp, OutBaroPress ) );
			AirChangeRate = ( Node( ZoneNode ).MassFlowRate * SecInHour ) / ( AirDensity * Zone( ZoneNum ).Volume );
			if ( ZoneEquipConfig( ZoneNum ).EquipListIndex > 0 ) {
				for ( EquipNum = 1; EquipNum <= ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).NumOfEquipTypes; ++EquipNum ) {
					if ( allocated( ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).EquipData( EquipNum ).OutletNodeNums ) ) {
						thisZoneInletNode = ZoneEquipList( ZoneEquipConfig( ZoneNum ).EquipListIndex ).EquipData( EquipNum ).OutletNodeNums( 1 );
						if ( ( thisZoneInletNode > 0 ) && ( Node( thisZoneInletNode ).MassFlowRate > 0.0 ) ) {
							SumMdotTemp += Node( thisZoneInletNode ).MassFlowRate * Node( thisZoneInletNode ).Temp;
						}
					}
				}
			}
			if ( SumMdot > 0.0 ) {
				SupplyAirTemp = SumMdotTemp / SumMdot; // mass flow weighted inlet temperature
			}
		}

		{ auto const SELECT_CASE_var( HcInsideUserCurve( UserCurveNum ).ReferenceTempType );

		if ( SELECT_CASE_var == RefTempMeanAirTemp ) {
			tmpAirTemp = MAT( ZoneNum );
			Surface( SurfNum ).TAirRef = ZoneMeanAirTemp;
		} else if ( SELECT_CASE_var == RefTempAdjacentAirTemp ) {
			tmpAirTemp = TempEffBulkAir( SurfNum );
			Surface( SurfNum ).TAirRef = AdjacentAirTemp;
		} else if ( SELECT_CASE_var == RefTempSupplyAirTemp ) {
			tmpAirTemp = SupplyAirTemp;
			Surface( SurfNum ).TAirRef = ZoneSupplyAirTemp;
		}}

		tmpHc = 0.0;
		if ( HcInsideUserCurve( UserCurveNum ).HcFnTempDiffCurveNum > 0 ) {
			tmpHc = CurveValue( HcInsideUserCurve( UserCurveNum ).HcFnTempDiffCurveNum, std::abs( TH( 2, 1, SurfNum ) - tmpAirTemp ) );
		}

		if ( HcInsideUserCurve( UserCurveNum ).HcFnTempDiffDivHeightCurveNum > 0 ) {
			tmpHc += CurveValue( HcInsideUserCurve( UserCurveNum ).HcFnTempDiffDivHeightCurveNum, ( std::abs( TH( 2, 1, SurfNum ) - tmpAirTemp ) / Surface( SurfNum ).IntConvZoneWallHeight ) );
		}

		if ( HcInsideUserCurve( UserCurveNum ).HcFnACHCurveNum > 0 ) {
			tmpHc += CurveValue( HcInsideUserCurve( UserCurveNum ).HcFnACHCurveNum, AirChangeRate );
		}

		if ( HcInsideUserCurve( UserCurveNum ).HcFnACHDivPerimLengthCurveNum > 0 ) {
			tmpHc += CurveValue( HcInsideUserCurve( UserCurveNum ).HcFnACHDivPerimLengthCurveNum, ( AirChangeRate / Surface( SurfNum ).IntConvZonePerimLength ) );
		}

		Hc = tmpHc;

	}

	void
	CalcUserDefinedOutsideHcModel(
		int const SurfNum,
		int const UserCurveNum,
		Real64 & H
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         <Brent Griffith
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// calculate user-defined convection correlations for outside face

		// METHODOLOGY EMPLOYED:
		// call curve objects to evaluate user's model equation
		// prepare independent parameters for x values

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataEnvironment::WindSpeed;
		using DataEnvironment::WindDir;
		using CurveManager::CurveValue;
		using DataHeatBalSurface::TH;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 tmpHc;
		Real64 windVel;
		Real64 Theta;
		Real64 ThetaRad;

		{ auto const SELECT_CASE_var( HcOutsideUserCurve( UserCurveNum ).WindSpeedType );

		if ( SELECT_CASE_var == RefWindWeatherFile ) {
			windVel = WindSpeed;
		} else if ( SELECT_CASE_var == RefWindAtZ ) {
			windVel = Surface( SurfNum ).WindSpeed;
		} else if ( SELECT_CASE_var == RefWindParallComp ) {
			// WindSpeed , WindDir, surface Azimuth
			Theta = WindDir - Surface( SurfNum ).Azimuth - 90.0; //TODO double check theta
			ThetaRad = Theta * DegToRadians;
			windVel = std::cos( ThetaRad ) * WindSpeed;
		} else if ( SELECT_CASE_var == RefWindParallCompAtZ ) {
			// Surface WindSpeed , WindDir, surface Azimuth
			Theta = WindDir - Surface( SurfNum ).Azimuth - 90.0; //TODO double check theta
			ThetaRad = Theta * DegToRadians;
			windVel = std::cos( ThetaRad ) * Surface( SurfNum ).WindSpeed;
		}}

		tmpHc = 0.0;
		if ( HcOutsideUserCurve( UserCurveNum ).HfFnWindSpeedCurveNum > 0 ) {
			tmpHc = CurveValue( HcOutsideUserCurve( UserCurveNum ).HfFnWindSpeedCurveNum, windVel );
		}

		if ( HcOutsideUserCurve( UserCurveNum ).HnFnTempDiffCurveNum > 0 ) {
			tmpHc += CurveValue( HcOutsideUserCurve( UserCurveNum ).HnFnTempDiffCurveNum, std::abs( TH( 1, 1, SurfNum ) - Surface( SurfNum ).OutDryBulbTemp ) );
		}

		if ( HcOutsideUserCurve( UserCurveNum ).HnFnTempDiffDivHeightCurveNum > 0 ) {
			if ( Surface( SurfNum ).OutConvFaceHeight > 0.0 ) {
				tmpHc += CurveValue( HcOutsideUserCurve( UserCurveNum ).HnFnTempDiffDivHeightCurveNum, ( ( std::abs( TH( 1, 1, SurfNum ) - Surface( SurfNum ).OutDryBulbTemp ) ) / Surface( SurfNum ).OutConvFaceHeight ) );
			}
		}

		H = tmpHc;

	}

	//** Begin catalog of Hc equation functions. **** !*************************************************

	Real64
	CalcASHRAEVerticalWall( Real64 const DeltaTemp ) // [C] temperature difference between surface and air
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate the model equation attributed to ASHRAE for vertical walls for natural convection

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// 2.  ASHRAE Handbook of Fundamentals 2001, p. 3.12, Table 5.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hn; // function result

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
		Hn = 1.31 * std::pow( std::abs( DeltaTemp ), OneThird );

		return Hn;

	}

	Real64
	CalcWaltonUnstableHorizontalOrTilt(
		Real64 const DeltaTemp, // [C] temperature difference between surface and air
		Real64 const CosineTilt // Cosine of tilt angle
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate the model equation attributed to Walton's TARP program for horizontal
		// and tilted surfaces with enhanced, thermally unstable natural convection

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// 1.  Walton, G. N. 1983. Thermal Analysis Research Program (TARP) Reference Manual,
		//     NBSSIR 83-2655, National Bureau of Standards, "Surface Inside Heat Balances", pp 79-80.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hn; // function result

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
		Hn = 9.482 * std::pow( std::abs( DeltaTemp ), OneThird ) / ( 7.283 - std::abs( CosineTilt ) );

		return Hn;

	}

	Real64
	CalcWaltonStableHorizontalOrTilt(
		Real64 const DeltaTemp, // [C] temperature difference between surface and air
		Real64 const CosineTilt // Cosine of tilt angle
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate the model equation attributed to Walton's TARP program for horizontal
		// and tilted surfaces with reduced, thermally stable natural convection

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// 1.  Walton, G. N. 1983. Thermal Analysis Research Program (TARP) Reference Manual,
		//     NBSSIR 83-2655, National Bureau of Standards, "Surface Inside Heat Balances", pp 79-80.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hn; // function result

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
		Hn = 1.810 * std::pow( std::abs( DeltaTemp ), OneThird ) / ( 1.382 + std::abs( CosineTilt ) );

		return Hn;

	}

	Real64
	CalcFisherPedersenCeilDiffuserFloor( Real64 const AirChangeRate ) // [1/hr] air system air change rate
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate the model equation by Fisher and Pedersen for floors with ceiling diffusers

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// Fisher, D.E. and C.O. Pedersen, Convective Heat Transfer in Building Energy and
		//       Thermal Load Calculations, ASHRAE Transactions, vol. 103, Pt. 2, 1997, p.13

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hc; // function result

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
		Hc = 3.873 + 0.082 * std::pow( AirChangeRate, 0.98 );

		return Hc;

	}

	Real64
	CalcFisherPedersenCeilDiffuserCeiling( Real64 const AirChangeRate ) // [1/hr] air system air change rate
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate the model equation by Fisher and Pedersen for ceilings with ceiling diffusers

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// Fisher, D.E. and C.O. Pedersen, Convective Heat Transfer in Building Energy and
		//       Thermal Load Calculations, ASHRAE Transactions, vol. 103, Pt. 2, 1997, p.13

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hc; // function result

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
		Hc = 2.234 + 4.099 * std::pow( AirChangeRate, 0.503 );

		return Hc;

	}

	Real64
	CalcFisherPedersenCeilDiffuserWalls( Real64 const AirChangeRate ) // [1/hr] air system air change rate
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate the model equation by Fisher and Pedersen for walls with ceiling diffusers

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// Fisher, D.E. and C.O. Pedersen, Convective Heat Transfer in Building Energy and
		//       Thermal Load Calculations, ASHRAE Transactions, vol. 103, Pt. 2, 1997, p.13

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hc; // function result

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
		Hc = 1.208 + 1.012 * std::pow( AirChangeRate, 0.604 );

		return Hc;

	}

	Real64
	CalcAlamdariHammondUnstableHorizontal(
		Real64 const DeltaTemp, // [C] temperature difference between surface and air
		Real64 const HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
		int const SurfNum // for messages
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Jul 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate model equation for Alamdari and Hammond
		// This function only for the Unstable heat flow direction for horizontal surfaces

		// METHODOLOGY EMPLOYED:
		// isolate function for equation.

		// REFERENCES:
		// Alamdari, F. and G.P. Hammond. 1983. Improved data correlations
		// for buoyancy-driven convection in rooms.  Building Services Engineering
		// Research & Technology. Vol. 4, No. 3.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hn; // function result

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		static int ErrorIndex( 0 );

		if ( HydraulicDiameter > 0.0 ) {
			Hn = std::pow( pow_6( 1.4 * std::pow( std::abs( DeltaTemp ) / HydraulicDiameter, OneFourth ) ) + ( 1.63 * pow_2( DeltaTemp ) ), OneSixth ); //Tuned pow_6( std::pow( std::abs( DeltaTemp ), OneThird ) ) changed to pow_2( DeltaTemp )
		} else {
			Hn = 9.999;
			if ( ErrorIndex == 0 ) {
				ShowSevereMessage( "CalcAlamdariHammondUnstableHorizontal: Convection model not evaluated (would divide by zero)" );
				ShowContinueError( "Effective hydraulic diameter is zero, convection model not applicable for surface =" + Surface( SurfNum ).Name );
				ShowContinueError( "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues" );

			}
			ShowRecurringSevereErrorAtEnd( "CalcAlamdariHammondUnstableHorizontal: Convection model not evaluated because zero hydraulic diameter and set to 9.999 [W/m2-K]", ErrorIndex );
		}

		return Hn;

	}

	Real64
	CalcAlamdariHammondStableHorizontal(
		Real64 const DeltaTemp, // [C] temperature difference between surface and air
		Real64 const HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
		int const SurfNum // for messages
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Jul 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate model equation for Alamdari and Hammond
		// This function only for the Stable heat flow direction for horizontal surfaces

		// METHODOLOGY EMPLOYED:
		// isolate function for equation.

		// REFERENCES:
		// Alamdari, F. and G.P. Hammond. 1983. Improved data correlations
		// for buoyancy-driven convection in rooms.  Building Services Engineering
		// Research & Technology. Vol. 4, No. 3.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hn; // function result, natural convection Hc value

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		static int ErrorIndex( 0 );

		if ( HydraulicDiameter > 0.0 ) {
			Hn = 0.6 * std::pow( std::abs( DeltaTemp ) / pow_2( HydraulicDiameter ), OneFifth );
		} else {
			Hn = 9.999;
			if ( ErrorIndex == 0 ) {
				ShowSevereMessage( "CalcAlamdariHammondStableHorizontal: Convection model not evaluated (would divide by zero)" );
				ShowContinueError( "Effective hydraulic diameter is zero, convection model not applicable for surface =" + Surface( SurfNum ).Name );
				ShowContinueError( "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues" );
			}
			ShowRecurringSevereErrorAtEnd( "CalcAlamdariHammondStableHorizontal: Convection model not evaluated because zero hydraulic diameter and set to 9.999 [W/m2-K]", ErrorIndex );
		}

		return Hn;

	}

	Real64
	CalcAlamdariHammondVerticalWall(
		Real64 const DeltaTemp, // [C] temperature difference between surface and air
		Real64 const Height, // [m] characteristic size, = zone height
		int const SurfNum // for messages
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Jul 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate model equation for Alamdari and Hammond
		// This function only for the vertical wall surfaces

		// METHODOLOGY EMPLOYED:
		// isolate function for equation.

		// REFERENCES:
		// Alamdari, F. and G.P. Hammond. 1983. Improved data correlations
		// for buoyancy-driven convection in rooms.  Building Services Engineering
		// Research & Technology. Vol. 4, No. 3.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hn; // function result, natural convection Hc value

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		static int ErrorIndex( 0 );

		if ( Height > 0.0 ) {
			Hn = std::pow( pow_6( 1.5 * std::pow( std::abs( DeltaTemp ) / Height, OneFourth ) ) + ( 1.23 * pow_2( DeltaTemp ) ), OneSixth ); //Tuned pow_6( std::pow( std::abs( DeltaTemp ), OneThird ) ) changed to pow_2( DeltaTemp )
		} else {
			Hn = 9.999;
			if ( ErrorIndex == 0 ) {
				ShowSevereMessage( "CalcAlamdariHammondVerticalWall: Convection model not evaluated (would divide by zero)" );
				ShowContinueError( "Effective hydraulic diameter is zero, convection model not applicable for surface =" + Surface( SurfNum ).Name );
				ShowContinueError( "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues" );
			}
			ShowRecurringSevereErrorAtEnd( "CalcAlamdariHammondVerticalWall: Convection model not evaluated because zero hydraulic diameter and set to 9.999 [W/m2-K]", ErrorIndex );
		}

		return Hn;

	}

	Real64
	CalcKhalifaEq3WallAwayFromHeat( Real64 const DeltaTemp ) // [C] temperature difference between surface and air
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Jul 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate model equation for Khalifa's Eq 3 for Walls Away From Heat

		// METHODOLOGY EMPLOYED:
		// isolate function for equation.

		// REFERENCES:
		// Khalifa AJN. 1989 Heat transfer processes in buildings. Ph.D. Thesis,
		//   University of Wales College of Cardiff, Cardiff, UK.
		// Equations actually from Beausoleil-Morrison 2000 who referenced Khalifa
		// Beausoleil-Morrison, I. 2000. The adaptive coupling of heat and
		//  air flow modeling within dynamic whole-building simulations.
		//  PhD. Thesis. University of Strathclyde, Glasgow, UK.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hc; // function result

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		Hc = 2.07 * std::pow( std::abs( DeltaTemp ), 0.23 );

		return Hc;

	}

	Real64
	CalcKhalifaEq4CeilingAwayFromHeat( Real64 const DeltaTemp ) // [C] temperature difference between surface and air
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Jul 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate model equation for Khalifa's Eq 4 for Ceilings Away From Heat

		// METHODOLOGY EMPLOYED:
		// isolate function for equation.

		// REFERENCES:
		// Khalifa AJN. 1989 Heat transfer processes in buildings. Ph.D. Thesis,
		//   University of Wales College of Cardiff, Cardiff, UK.
		// Equations actually from Beausoleil-Morrison 2000 who referenced Khalifa
		// Beausoleil-Morrison, I. 2000. The adaptive coupling of heat and
		//  air flow modeling within dynamic whole-building simulations.
		//  PhD. Thesis. University of Strathclyde, Glasgow, UK.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hc; // function result

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		Hc = 2.72 * std::pow( std::abs( DeltaTemp ), 0.13 );

		return Hc;

	}

	Real64
	CalcKhalifaEq5WallsNearHeat( Real64 const DeltaTemp ) // [C] temperature difference between surface and air
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Jul 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate model equation for Khalifa's Eq 5 for Walls near the heater

		// METHODOLOGY EMPLOYED:
		// isolate function for equation.

		// REFERENCES:
		// Khalifa AJN. 1989 Heat transfer processes in buildings. Ph.D. Thesis,
		//   University of Wales College of Cardiff, Cardiff, UK.
		// Equations actually from Beausoleil-Morrison 2000 who referenced Khalifa
		// Beausoleil-Morrison, I. 2000. The adaptive coupling of heat and
		//  air flow modeling within dynamic whole-building simulations.
		//  PhD. Thesis. University of Strathclyde, Glasgow, UK.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hc; // function result

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		Hc = 1.98 * std::pow( std::abs( DeltaTemp ), 0.32 );

		return Hc;

	}

	Real64
	CalcKhalifaEq6NonHeatedWalls( Real64 const DeltaTemp ) // [C] temperature difference between surface and air
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Jul 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate model equation for Khalifa's Eq 6 for non-heated walls

		// METHODOLOGY EMPLOYED:
		// isolate function for equation.

		// REFERENCES:
		// Khalifa AJN. 1989 Heat transfer processes in buildings. Ph.D. Thesis,
		//   University of Wales College of Cardiff, Cardiff, UK.
		// Equations actually from Beausoleil-Morrison 2000 who referenced Khalifa
		// Beausoleil-Morrison, I. 2000. The adaptive coupling of heat and
		//  air flow modeling within dynamic whole-building simulations.
		//  PhD. Thesis. University of Strathclyde, Glasgow, UK.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hc; // function result

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		Hc = 2.30 * std::pow( std::abs( DeltaTemp ), 0.24 );

		return Hc;

	}

	Real64
	CalcKhalifaEq7Ceiling( Real64 const DeltaTemp ) // [C] temperature difference between surface and air
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Jul 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate model equation for Khalifa's Eq 7 for ceilings

		// METHODOLOGY EMPLOYED:
		// isolate function for equation.

		// REFERENCES:
		// Khalifa AJN. 1989 Heat transfer processes in buildings. Ph.D. Thesis,
		//   University of Wales College of Cardiff, Cardiff, UK.
		// Equations actually from Beausoleil-Morrison 2000 who referenced Khalifa
		// Beausoleil-Morrison, I. 2000. The adaptive coupling of heat and
		//  air flow modeling within dynamic whole-building simulations.
		//  PhD. Thesis. University of Strathclyde, Glasgow, UK.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hc; // function result

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		Hc = 3.10 * std::pow( std::abs( DeltaTemp ), 0.17 );

		return Hc;

	}

	Real64
	CalcAwbiHattonHeatedFloor(
		Real64 const DeltaTemp, // [C] temperature difference between surface and air
		Real64 const HydraulicDiameter // [m] characteristic size, = (4 * area) / perimeter
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Jul 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate model equation for Awbi and Hatton for heated floors

		// METHODOLOGY EMPLOYED:
		// isolate function for equation.
		// apply numerical protection for low values of hydraulic diameter

		// REFERENCES:
		// Awbi, H.B. and A. Hatton. 1999. Natural convection from heated room surfaces.
		//   Energy and Buildings 30 (1999) 233-244.
		//   This function is for equation 15 in the reference

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hc; // function result

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		static Real64 const pow_fac( 2.175 / std::pow( 1.0, 0.076 ) );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		if ( HydraulicDiameter > 1.0 ) {
			Hc = 2.175 * std::pow( std::abs( DeltaTemp ), 0.308 ) / std::pow( HydraulicDiameter, 0.076 );
		} else {
			Hc = pow_fac * std::pow( std::abs( DeltaTemp ), 0.308 );
		}

		return Hc;

	}

	Real64
	CalcAwbiHattonHeatedWall(
		Real64 const DeltaTemp, // [C] temperature difference between surface and air
		Real64 const HydraulicDiameter // [m] characteristic size, = (4 * area) / perimeter
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Jul 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate model equation for Awbi and Hatton for heated walls

		// METHODOLOGY EMPLOYED:
		// isolate function for equation.

		// REFERENCES:
		// Awbi, H.B. and A. Hatton. 1999. Natural convection from heated room surfaces.
		//   Energy and Buildings 30 (1999) 233-244.
		//   This function is for equation 12 in the reference

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hc; // function result

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		if ( HydraulicDiameter > 1.0 ) {
			Hc = 1.823 * std::pow( std::abs( DeltaTemp ), 0.293 ) / std::pow( HydraulicDiameter, 0.121 );
		} else {
			Hc = 1.823 * std::pow( std::abs( DeltaTemp ), 0.293 ) / std::pow( 1.0, 0.121 );
		}

		return Hc;

	}

	Real64
	CalcBeausoleilMorrisonMixedAssistedWall(
		Real64 const DeltaTemp, // [C] temperature difference between surface and air
		Real64 const Height, // [m] characteristic size
		Real64 const SurfTemp, // [C] surface temperature
		Real64 const SupplyAirTemp, // [C] temperature of supply air into zone
		Real64 const AirChangeRate, // [ACH] [1/hour] supply air ACH for zone
		int const ZoneNum // index of zone for messaging
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Jul 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate model equation Beausoleil-Morrison's mixed flow regime
		// with mechanical and bouyancy forces assisting each other along a Wall

		// METHODOLOGY EMPLOYED:
		// isolate function for equation.

		// REFERENCES:
		// Beausoleil-Morrison, I. 2000. The adaptive coupling of heat and
		//  air flow modeling within dynamic whole-building simulations.
		//  PhD. Thesis. University of Strathclyde, Glasgow, UK.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hc; // function result

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 cofpow;
		static int ErrorIndex( 0 );

		if ( ( DeltaTemp != 0.0 ) && ( Height != 0.0 ) ) {
			cofpow = std::sqrt( pow_6( 1.5 * std::pow( std::abs( DeltaTemp ) / Height, OneFourth ) ) + std::pow( 1.23 * pow_2( DeltaTemp ), OneSixth ) ) + pow_3( ( ( SurfTemp - SupplyAirTemp ) / std::abs( DeltaTemp ) ) * ( -0.199 + 0.190 * std::pow( AirChangeRate, 0.8 ) ) ); //Tuned pow_6( std::pow( std::abs( DeltaTemp ), OneThird ) ) changed to pow_2( DeltaTemp )
			Hc = std::pow( std::abs( cofpow ), OneThird ); //Tuned pow_6( std::pow( std::abs( DeltaTemp ), OneThird ) ) changed to pow_2( DeltaTemp )
			if ( cofpow < 0.0 ) {
				Hc = -Hc;
			}
		} else {
			Hc = 9.999;
			if ( Height == 0.0 ) {
				ShowWarningMessage( "CalcBeausoleilMorrisonMixedAssistedWall: Convection model not evaluated (would divide by zero)" );
				ShowContinueError( "Effective height is zero, convection model not applicable for zone named =" + Zone( ZoneNum ).Name );
				ShowContinueError( "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues" );
			}
			if ( DeltaTemp == 0.0 && ! WarmupFlag ) {
				if ( ErrorIndex == 0 ) {
					ShowWarningMessage( "CalcBeausoleilMorrisonMixedAssistedWall: Convection model not evaluated (would divide by zero)" );
					ShowContinueError( "The temperature difference between surface and air is zero" );
					ShowContinueError( "Occurs for zone named = " + Zone( ZoneNum ).Name );
					ShowContinueError( "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues" );
				}

				ShowRecurringWarningErrorAtEnd( "CalcBeausoleilMorrisonMixedAssistedWall: Convection model not evaluated because of zero temperature difference and set to 9.999 [W/m2-K]", ErrorIndex );
			}

		}
		return Hc;

	}

	Real64
	CalcBeausoleilMorrisonMixedOpposingWall(
		Real64 const DeltaTemp, // [C] temperature difference between surface and air
		Real64 const Height, // [m] characteristic size
		Real64 const SurfTemp, // [C] surface temperature
		Real64 const SupplyAirTemp, // [C] temperature of supply air into zone
		Real64 const AirChangeRate, // [ACH] [1/hour] supply air ACH for zone
		int const ZoneNum // index of zone for messaging
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Jul 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate model equation Beausoleil-Morrison's mixed flow regime
		// with mechanical and bouyancy forces opposing each other along a Wall

		// METHODOLOGY EMPLOYED:
		// isolate function for equation.

		// REFERENCES:
		// Beausoleil-Morrison, I. 2000. The adaptive coupling of heat and
		//  air flow modeling within dynamic whole-building simulations.
		//  PhD. Thesis. University of Strathclyde, Glasgow, UK.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hc; // function result

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 HcTmp1;
		Real64 HcTmp2;
		Real64 HcTmp3;
		Real64 cofpow;
		static int ErrorIndex( 0 );
		static int ErrorIndex2( 0 );

		if ( ( DeltaTemp != 0.0 ) ) { // protect divide by zero

			if ( Height != 0.0 ) {
				cofpow = std::sqrt( pow_6( 1.5 * std::pow( std::abs( DeltaTemp ) / Height, OneFourth ) ) + std::pow( 1.23 * pow_2( DeltaTemp ), OneSixth ) ) - pow_3( ( ( SurfTemp - SupplyAirTemp ) / std::abs( DeltaTemp ) ) * ( -0.199 + 0.190 * std::pow( AirChangeRate, 0.8 ) ) ); //Tuned pow_6( std::pow( std::abs( DeltaTemp ), OneThird ) ) changed to pow_2( DeltaTemp )
				HcTmp1 = std::pow( std::abs( cofpow ), OneThird ); //Tuned pow_6( std::pow( std::abs( DeltaTemp ), OneThird ) ) changed to pow_2( DeltaTemp )
				if ( cofpow < 0.0 ) {
					HcTmp1 = -HcTmp1;
				}

				HcTmp2 = 0.8 * std::pow( pow_6( 1.5 * std::pow( std::abs( DeltaTemp ) / Height, OneFourth ) ) + ( 1.23 * pow_2( DeltaTemp ) ), OneSixth ); //Tuned pow_6( std::pow( std::abs( DeltaTemp ), OneThird ) ) changed to pow_2( DeltaTemp )
			} else {
				HcTmp1 = 9.999;
				HcTmp2 = 9.999;
				if ( ErrorIndex2 == 0 ) {
					ShowSevereMessage( "CalcBeausoleilMorrisonMixedOpposingWall: Convection model not evaluated (would divide by zero)" );
					ShowContinueError( "Effective height is zero, convection model not applicable for zone named =" + Zone( ZoneNum ).Name );
					ShowContinueError( "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues" );

				}
				ShowRecurringSevereErrorAtEnd( "CalcBeausoleilMorrisonMixedOpposingWall: Convection model not evaluated because of zero height and set to 9.999 [W/m2-K]", ErrorIndex2 );

			}
			HcTmp3 = 0.8 * ( ( SurfTemp - SupplyAirTemp ) / std::abs( DeltaTemp ) ) * ( -0.199 + 0.190 * std::pow( AirChangeRate, 0.8 ) );

			Hc = max( max( HcTmp1, HcTmp2 ), HcTmp3 );

		} else {
			Hc = 9.999;
			if ( ! WarmupFlag ) {
				if ( ErrorIndex == 0 ) {
					ShowSevereMessage( "CalcBeausoleilMorrisonMixedOpposingWall: Convection model not evaluated (would divide by zero)" );
					ShowContinueError( "The temperature difference between surface and air is zero" );
					ShowContinueError( "Occurs for zone named = " + Zone( ZoneNum ).Name );
					ShowContinueError( "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues" );
				}
				ShowRecurringSevereErrorAtEnd( "CalcBeausoleilMorrisonMixedOpposingWall: Convection model not evaluated because of zero temperature difference and set to 9.999 [W/m2-K]", ErrorIndex );
			}
		}

		return Hc;

	}

	Real64
	CalcBeausoleilMorrisonMixedStableFloor(
		Real64 const DeltaTemp, // [C] temperature difference between surface and air
		Real64 const HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
		Real64 const SurfTemp, // [C] surface temperature
		Real64 const SupplyAirTemp, // [C] temperature of supply air into zone
		Real64 const AirChangeRate, // [ACH] [1/hour] supply air ACH for zone
		int const ZoneNum // index of zone for messaging
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Jul 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate model equation Beausoleil-Morrison's mixed flow regime
		// with mechanical and bouyancy forces acting on an thermally stable floor

		// METHODOLOGY EMPLOYED:
		// isolate function for equation.

		// REFERENCES:
		// Beausoleil-Morrison, I. 2000. The adaptive coupling of heat and
		//  air flow modeling within dynamic whole-building simulations.
		//  PhD. Thesis. University of Strathclyde, Glasgow, UK.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hc; // function result

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 cofpow;
		static int ErrorIndex( 0 );

		if ( ( HydraulicDiameter != 0.0 ) && ( DeltaTemp != 0.0 ) ) {
			cofpow = pow_3( 0.6 * std::pow( std::abs( DeltaTemp ) / HydraulicDiameter, OneFifth ) ) + pow_3( ( ( SurfTemp - SupplyAirTemp ) / std::abs( DeltaTemp ) ) * ( 0.159 + 0.116 * std::pow( AirChangeRate, 0.8 ) ) );
			Hc = std::pow( std::abs( cofpow ), OneThird );
			if ( cofpow < 0.0 ) {
				Hc = -Hc;
			}
		} else {
			Hc = 9.999;
			if ( HydraulicDiameter == 0.0 ) {
				ShowWarningMessage( "CalcBeausoleilMorrisonMixedStableFloor: Convection model not evaluated (would divide by zero)" );
				ShowContinueError( "Effective hydraulic diameter is zero, convection model not applicable for zone named =" + Zone( ZoneNum ).Name );
				ShowContinueError( "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues" );
			}
			if ( DeltaTemp == 0.0 && ! WarmupFlag ) {
				if ( ErrorIndex == 0 ) {
					ShowWarningMessage( "CalcBeausoleilMorrisonMixedStableFloor: Convection model not evaluated (would divide by zero)" );
					ShowContinueError( "The temperature difference between surface and air is zero" );
					ShowContinueError( "Occurs for zone named = " + Zone( ZoneNum ).Name );
					ShowContinueError( "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues" );
				}

				ShowRecurringWarningErrorAtEnd( "CalcBeausoleilMorrisonMixedStableFloor: Convection model not evaluated because of zero temperature difference and set to 9.999 [W/m2-K]", ErrorIndex );
			}

		}
		return Hc;

	}

	Real64
	CalcBeausoleilMorrisonMixedUnstableFloor(
		Real64 const DeltaTemp, // [C] temperature difference between surface and air
		Real64 const HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
		Real64 const SurfTemp, // [C] surface temperature
		Real64 const SupplyAirTemp, // [C] temperature of supply air into zone
		Real64 const AirChangeRate, // [ACH] [1/hour] supply air ACH for zone
		int const ZoneNum // index of zone for messaging
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Jul 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate model equation Beausoleil-Morrison's mixed flow regime
		// with mechanical and bouyancy forces acting on an thermally unstable floor

		// METHODOLOGY EMPLOYED:
		// isolate function for equation.

		// REFERENCES:
		// Beausoleil-Morrison, I. 2000. The adaptive coupling of heat and
		//  air flow modeling within dynamic whole-building simulations.
		//  PhD. Thesis. University of Strathclyde, Glasgow, UK.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hc; // function result, total convection coefficient

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 cofpow;
		static int ErrorIndex( 0 );

		if ( ( HydraulicDiameter != 0.0 ) && ( DeltaTemp != 0.0 ) ) {
			cofpow = std::sqrt( pow_6( 1.4 * std::pow( std::abs( DeltaTemp ) / HydraulicDiameter, OneFourth ) ) + pow_6( 1.63 * std::pow( std::abs( DeltaTemp ), OneThird ) ) ) + pow_3( ( ( SurfTemp - SupplyAirTemp ) / std::abs( DeltaTemp ) ) * ( 0.159 + 0.116 * std::pow( AirChangeRate, 0.8 ) ) );
			Hc = std::pow( std::abs( cofpow ), OneThird );
			if ( cofpow < 0.0 ) {
				Hc = -Hc;
			}
		} else {
			Hc = 9.999;
			if ( HydraulicDiameter == 0.0 ) {
				ShowWarningMessage( "CalcBeausoleilMorrisonMixedUnstableFloor: Convection model not evaluated (would divide by zero)" );
				ShowContinueError( "Effective hydraulic diameter is zero, convection model not applicable for zone named =" + Zone( ZoneNum ).Name );
				ShowContinueError( "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues" );
			}
			if ( DeltaTemp == 0.0 && ! WarmupFlag ) {
				if ( ErrorIndex == 0 ) {
					ShowWarningMessage( "CalcBeausoleilMorrisonMixedUnstableFloor: Convection model not evaluated (would divide by zero)" );
					ShowContinueError( "The temperature difference between surface and air is zero" );
					ShowContinueError( "Occurs for zone named = " + Zone( ZoneNum ).Name );
					ShowContinueError( "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues" );
				}

				ShowRecurringWarningErrorAtEnd( "CalcBeausoleilMorrisonMixedUnstableFloor: Convection model not evaluated because of zero temperature difference and set to 9.999 [W/m2-K]", ErrorIndex );
			}

		}

		return Hc;

	}

	Real64
	CalcBeausoleilMorrisonMixedStableCeiling(
		Real64 const DeltaTemp, // [C] temperature difference between surface and air
		Real64 const HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
		Real64 const SurfTemp, // [C] surface temperature
		Real64 const SupplyAirTemp, // [C] temperature of supply air into zone
		Real64 const AirChangeRate, // [ACH] [1/hour] supply air ACH for zone
		int const ZoneNum // index of zone for messaging
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Jul 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate model equation Beausoleil-Morrison's mixed flow regime
		// with mechanical and bouyancy forces acting on a thermally stable ceiling

		// METHODOLOGY EMPLOYED:
		// isolate function for equation.

		// REFERENCES:
		// Beausoleil-Morrison, I. 2000. The adaptive coupling of heat and
		//  air flow modeling within dynamic whole-building simulations.
		//  PhD. Thesis. University of Strathclyde, Glasgow, UK.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hc; // function result, total convection coefficient

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 cofpow;
		static int ErrorIndex( 0 );

		if ( ( HydraulicDiameter != 0.0 ) && ( DeltaTemp != 0.0 ) ) {
			cofpow = pow_3( 0.6 * std::pow( std::abs( DeltaTemp ) / HydraulicDiameter, OneFifth ) ) + pow_3( ( ( SurfTemp - SupplyAirTemp ) / std::abs( DeltaTemp ) ) * ( -0.166 + 0.484 * std::pow( AirChangeRate, 0.8 ) ) );
			Hc = std::pow( std::abs( cofpow ), OneThird );
			if ( cofpow < 0.0 ) {
				Hc = -Hc;
			}
		} else {
			Hc = 9.999;
			if ( HydraulicDiameter == 0.0 ) {
				ShowWarningMessage( "CalcBeausoleilMorrisonMixedStableCeiling: Convection model not evaluated (would divide by zero)" );
				ShowContinueError( "Effective hydraulic diameter is zero, convection model not applicable for zone named =" + Zone( ZoneNum ).Name );
				ShowContinueError( "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues" );
			}
			if ( DeltaTemp == 0.0 && ! WarmupFlag ) {
				if ( ErrorIndex == 0 ) {
					ShowWarningMessage( "CalcBeausoleilMorrisonMixedStableCeiling: Convection model not evaluated (would divide by zero)" );
					ShowContinueError( "The temperature difference between surface and air is zero" );
					ShowContinueError( "Occurs for zone named = " + Zone( ZoneNum ).Name );
					ShowContinueError( "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues" );
				}

				ShowRecurringWarningErrorAtEnd( "CalcBeausoleilMorrisonMixedStableCeiling: Convection model not evaluated because of zero temperature difference and set to 9.999 [W/m2-K]", ErrorIndex );
			}

		}
		return Hc;

	}

	Real64
	CalcBeausoleilMorrisonMixedUnstableCeiling(
		Real64 const DeltaTemp, // [C] temperature difference between surface and air
		Real64 const HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
		Real64 const SurfTemp, // [C] surface temperature
		Real64 const SupplyAirTemp, // [C] temperature of supply air into zone
		Real64 const AirChangeRate, // [ACH] [1/hour] supply air ACH for zone
		int const ZoneNum // index of zone for messaging
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Jul 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate model equation Beausoleil-Morrison's mixed flow regime
		// with mechanical and bouyancy forces acting on a thermally unstable ceiling

		// METHODOLOGY EMPLOYED:
		// isolate function for equation.

		// REFERENCES:
		// Beausoleil-Morrison, I. 2000. The adaptive coupling of heat and
		//  air flow modeling within dynamic whole-building simulations.
		//  PhD. Thesis. University of Strathclyde, Glasgow, UK.

		// Using/Aliasing
		using DataGlobals::WarmupFlag;

		// Return value
		Real64 Hc; // function result, total convection coefficient

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 cofpow;
		static int ErrorIndex( 0 );

		if ( ( HydraulicDiameter != 0.0 ) && ( DeltaTemp != 0.0 ) ) {
			cofpow = std::sqrt( pow_6( 1.4 * std::pow( std::abs( DeltaTemp ) / HydraulicDiameter, OneFourth ) ) + pow_6( 1.63 * std::pow( std::abs( DeltaTemp ), OneThird ) ) ) + pow_3( ( ( SurfTemp - SupplyAirTemp ) / std::abs( DeltaTemp ) ) * ( -0.166 + 0.484 * std::pow( AirChangeRate, 0.8 ) ) );
			Hc = std::pow( std::abs( cofpow ), OneThird );
			if ( cofpow < 0.0 ) {
				Hc = -Hc;
			}
		} else {
			Hc = 9.999;
			if ( HydraulicDiameter == 0.0 ) {
				ShowWarningMessage( "CalcBeausoleilMorrisonMixedUnstableCeiling: Convection model not evaluated (would divide by zero)" );
				ShowContinueError( "Effective hydraulic diameter is zero, convection model not applicable for zone named =" + Zone( ZoneNum ).Name );
				ShowContinueError( "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues" );
			}
			if ( DeltaTemp == 0.0 && ! WarmupFlag ) {
				if ( ErrorIndex == 0 ) {
					ShowWarningMessage( "CalcBeausoleilMorrisonMixedUnstableCeiling: Convection model not evaluated (would divide by zero)" );
					ShowContinueError( "The temperature difference between surface and air is zero" );
					ShowContinueError( "Occurs for zone named = " + Zone( ZoneNum ).Name );
					ShowContinueError( "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues" );
				}

				ShowRecurringWarningErrorAtEnd( "CalcBeausoleilMorrisonMixedUnstableCeiling: Convection model not evaluated because of zero temperature difference and set to 9.999 [W/m2-K]", ErrorIndex );
			}
		}
		return Hc;

	}

	Real64
	CalcFohannoPolidoriVerticalWall(
		Real64 const DeltaTemp, // [C] temperature difference between surface and air
		Real64 const Height, // [m] characteristic size, height of zone
		Real64 const SurfTemp, // [C] surface temperature
		Real64 const QdotConv, // [W/m2] heat flux rate for rayleigh #
		int const SurfNum // for messages
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Jul 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate model equation for natural convection

		// METHODOLOGY EMPLOYED:
		// isolate function for equation.

		// REFERENCES:
		// Fohanno, S., and G. Polidori. 2006. Modelling of natural convective heat transfer
		// at an internal surface. Energy and Buildings 38 (2006) 548 - 553

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hn; // function result, natural convection coefficient

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		Real64 const g( 9.81 ); // gravity constant (m/s**2)
		Real64 const v( 15.89e-6 ); // kinematic viscosity (m**2/s) for air at 300 K
		Real64 const k( 0.0263 ); // thermal conductivity (W/m K) for air at 300 K
		Real64 const Pr( 0.71 ); // Prandtl number for air at ?
		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		static Real64 RaH( 0.0 );
		static Real64 BetaFilm( 0.0 );
		static int ErrorIndex( 0 );

		BetaFilm = 1.0 / ( KelvinConv + SurfTemp + 0.5 * DeltaTemp ); // TODO check sign on DeltaTemp
		if ( Height > 0.0 ) {
			RaH = ( g * BetaFilm * QdotConv * pow_4( Height ) * Pr ) / ( k * pow_2( v ) );

			if ( RaH <= 6.3e09 ) {
				Hn = 1.332 * std::pow( std::abs( DeltaTemp ) / Height, OneFourth );
			} else {
				Hn = 1.235 * std::exp( 0.0467 * Height ) * std::pow( std::abs( DeltaTemp ), 0.316 );
			}
		} else {
			// bad value for Height, but we have little info to identify calling culprit
			Hn = 9.999;
			if ( ErrorIndex == 0 ) {
				ShowSevereMessage( "CalcFohannoPolidoriVerticalWall: Convection model not evaluated (would divide by zero)" );
				ShowContinueError( "Effective surface height is zero, convection model not applicable for surface =" + Surface( SurfNum ).Name );
				ShowContinueError( "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues" );

			}
			ShowRecurringSevereErrorAtEnd( "CalcFohannoPolidoriVerticalWall: Convection model not evaluated because zero height and set to 9.999 [W/m2-K]", ErrorIndex );
		}

		return Hn;

	}

	Real64
	CalcKaradagChilledCeiling( Real64 const DeltaTemp ) // [C] temperature difference between surface and air
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Jul 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate model equation for natural convection developed by Karadag for chilled ceilings

		// METHODOLOGY EMPLOYED:
		// isolate function for equation.

		// REFERENCES:
		// Karadag, R. 2009. New approach relevant to total heat transfer coefficient
		//   including the effect of radiation and convection at the ceiling in a cooled
		//   ceiling room.  Applied Thermal Engineering 29 (2009) 1561-1565
		//    This function is for equation 8 in the reference

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hn; // function result, natural convection coefficient

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		Hn = 3.1 * std::pow( std::abs( DeltaTemp ), 0.22 );

		return Hn;

	}

	Real64
	CalcGoldsteinNovoselacCeilingDiffuserWindow(
		Real64 const AirSystemFlowRate, // [m3/s] air system flow rate
		Real64 const ZoneExtPerimLength, // [m] length of zone perimeter with exterior walls
		Real64 const WindWallRatio, // [ ] fraction of window area to wall area for zone
		int const WindowLocationType, // index for location types
		int const ZoneNum // for messages
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate model equation for windows in zones with slot diffusers on them
		//  developed by Novoselac for RP-1416

		// METHODOLOGY EMPLOYED:
		// isolate function for equation.

		// REFERENCES:
		// Goldstien, K. and A. Novoselac. 2010. Convective Heat Transfer in Rooms
		//  With Ceiling Slot Diffusers (RP-1416). HVAC&R Research Journal TBD

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hc; // function result, total convection coefficient

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		static int ErrorIndex( 0 );
		static int ErrorIndex2( 0 );

		if ( ZoneExtPerimLength > 0.0 ) {
			if ( WindWallRatio <= 0.5 ) {

				if ( WindowLocationType == InConvWinLoc_UpperPartOfExteriorWall ) {
					Hc = 0.117 * std::pow( AirSystemFlowRate / ZoneExtPerimLength, 0.8 );
				} else if ( WindowLocationType == InConvWinLoc_LowerPartOfExteriorWall ) {
					Hc = 0.093 * std::pow( AirSystemFlowRate / ZoneExtPerimLength, 0.8 );
				} else if ( WindowLocationType == InConvWinLoc_LargePartOfExteriorWall ) {
					Hc = 0.117 * std::pow( AirSystemFlowRate / ZoneExtPerimLength, 0.8 ); // assumption for case not covered by model
				} else if ( WindowLocationType == InConvWinLoc_NotSet ) {
					Hc = 0.117 * std::pow( AirSystemFlowRate / ZoneExtPerimLength, 0.8 ); // assumption for case not covered by model
				} else {
					//shouldn'tcome
					Hc = 9.999;
					if ( ErrorIndex == 0 ) {
						ShowSevereMessage( "CalcGoldsteinNovoselacCeilingDiffuserWindow: Convection model not evaluated (bad relative window location)" );
						ShowContinueError( "Value for window location = " + RoundSigDigits( WindowLocationType ) );
						ShowContinueError( "Occurs for zone named = " + Zone( ZoneNum ).Name );
						ShowContinueError( "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues" );
					}
					ShowRecurringSevereErrorAtEnd( "CalcGoldsteinNovoselacCeilingDiffuserWindow: Convection model not evaluated because bad window location and set to 9.999 [W/m2-K]", ErrorIndex );
				}
			} else {
				Hc = 0.103 * std::pow( AirSystemFlowRate / ZoneExtPerimLength, 0.8 );
			}
		} else {
			Hc = 9.999;
			if ( ErrorIndex2 == 0 ) {
				ShowSevereMessage( "CalcGoldsteinNovoselacCeilingDiffuserWindow: Convection model not evaluated (zero zone exterior perimeter length)" );
				ShowContinueError( "Value for zone exterior perimeter length = " + RoundSigDigits( ZoneExtPerimLength, 5 ) );
				ShowContinueError( "Occurs for zone named = " + Zone( ZoneNum ).Name );
				ShowContinueError( "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues" );
			}
			ShowRecurringSevereErrorAtEnd( "CalcGoldsteinNovoselacCeilingDiffuserWindow: Convection model not evaluated because bad perimeter length and set to 9.999 [W/m2-K]", ErrorIndex2 );
		}
		return Hc;

	}

	Real64
	CalcGoldsteinNovoselacCeilingDiffuserWall(
		Real64 const AirSystemFlowRate, // [m3/s] air system flow rate
		Real64 const ZoneExtPerimLength, // [m] length of zone perimeter with exterior walls
		int const WindowLocationType, // index for location types
		int const ZoneNum // for messages
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate model equation for exterior walls in zones with slot diffusers on them
		//  developed by Novoselac for RP-1416

		// METHODOLOGY EMPLOYED:
		// isolate function for equation.

		// REFERENCES:
		// Goldstien, K. and A. Novoselac. 2010. Convective Heat Transfer in Rooms
		//  With Ceiling Slot Diffusers (RP-1416). HVAC&R Research Journal TBD

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hc; // function result, total convection coefficient

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		static int ErrorIndex( 0 );
		static int ErrorIndex2( 0 );

		if ( ZoneExtPerimLength > 0.0 ) {
			if ( WindowLocationType == InConvWinLoc_WindowAboveThis ) {
				Hc = 0.063 * std::pow( AirSystemFlowRate / ZoneExtPerimLength, 0.8 );
			} else if ( WindowLocationType == InConvWinLoc_WindowBelowThis ) {
				Hc = 0.093 * std::pow( AirSystemFlowRate / ZoneExtPerimLength, 0.8 );
			} else if ( WindowLocationType == InConvWinLoc_NotSet ) {
				Hc = 0.063 * std::pow( AirSystemFlowRate / ZoneExtPerimLength, 0.8 ); // assumption for case not covered by model
			} else {
				Hc = 9.999;
				if ( ErrorIndex == 0 ) {
					ShowSevereMessage( "CalcGoldsteinNovoselacCeilingDiffuserWall: Convection model not evaluated (bad relative window location)" );
					ShowContinueError( "Value for window location = " + RoundSigDigits( WindowLocationType ) );
					ShowContinueError( "Occurs for zone named = " + Zone( ZoneNum ).Name );
					ShowContinueError( "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues" );
				}
				ShowRecurringSevereErrorAtEnd( "CalcGoldsteinNovoselacCeilingDiffuserWall: Convection model not evaluated because bad window location and set to 9.999 [W/m2-K]", ErrorIndex );

			}
		} else {
			Hc = 9.999;
			if ( ErrorIndex2 == 0 ) {
				ShowSevereMessage( "CalcGoldsteinNovoselacCeilingDiffuserWall: Convection model not evaluated (zero zone exterior perimeter length)" );
				ShowContinueError( "Value for zone exterior perimeter length = " + RoundSigDigits( ZoneExtPerimLength, 5 ) );
				ShowContinueError( "Occurs for zone named = " + Zone( ZoneNum ).Name );
				ShowContinueError( "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues" );
			}
			ShowRecurringSevereErrorAtEnd( "CalcGoldsteinNovoselacCeilingDiffuserWall: Convection model not evaluated because bad perimeter length and set to 9.999 [W/m2-K]", ErrorIndex2 );

		}
		return Hc;

	}

	Real64
	CalcGoldsteinNovoselacCeilingDiffuserFloor(
		Real64 const AirSystemFlowRate, // [m3/s] air system flow rate
		Real64 const ZoneExtPerimLength, // [m] length of zone perimeter with exterior walls
		int const ZoneNum // for messages
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate model equation for floors in zones with slot diffusers on them
		//  developed by Novoselac for RP-1416

		// METHODOLOGY EMPLOYED:
		// isolate function for equation.

		// REFERENCES:
		// Goldstien, K. and A. Novoselac. 2010. Convective Heat Transfer in Rooms
		//  With Ceiling Slot Diffusers (RP-1416). HVAC&R Research Journal TBD

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hc; // function result, total convection coefficient

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		static int ErrorIndex( 0 );

		if ( ZoneExtPerimLength > 0.0 ) {
			Hc = 0.048 * std::pow( AirSystemFlowRate / ZoneExtPerimLength, 0.8 );
		} else {
			if ( ErrorIndex == 0 ) {
				ShowSevereMessage( "CalcGoldsteinNovoselacCeilingDiffuserFloor: Convection model not evaluated (zero zone exterior perimeter length)" );
				ShowContinueError( "Value for zone exterior perimeter length = " + RoundSigDigits( ZoneExtPerimLength, 5 ) );
				ShowContinueError( "Occurs for zone named = " + Zone( ZoneNum ).Name );
				ShowContinueError( "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues" );
			}
			ShowRecurringSevereErrorAtEnd( "CalcGoldsteinNovoselacCeilingDiffuserFloor: Convection model not evaluated because bad perimeter length and set to 9.999 [W/m2-K]", ErrorIndex );

			Hc = 9.999; // safe but noticeable
		}
		return Hc;

	}

	Real64
	CalcSparrowWindward(
		int const RoughnessIndex,
		Real64 const FacePerimeter,
		Real64 const FaceArea,
		Real64 const WindAtZ,
		int const SurfNum
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate Sparrow Hf for windward surfaces

		// METHODOLOGY EMPLOYED:
		// encapsulate equation as a function

		// REFERENCES:

		//   1. TARP Reference Manual, "Surface Outside Heat Balances", pp 71ff
		//   2. Sparrow, E. M., J. W. Ramsey, and E. A. Mass.  1979.  Effect of finite
		//   width on heat transfer and fluid flow about an inclined rectangular plate.
		//   Journal of Heat Transfer 101:  204.
		//   3. McClellan, T.M.  1996.  Investigation of a heat balance cooling load
		//   procedure with a detailed study of outside heat transfer parameters.
		//   M.S. Thesis, Department of Mechanical and Industrial Engineering,
		//   University of Illinois at Urbana-Champaign.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hf;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		static int ErrorIndex( 0 );

		if ( FaceArea > 0.0 ) {
			Hf = 2.53 * RoughnessMultiplier( RoughnessIndex ) * std::sqrt( FacePerimeter * WindAtZ / FaceArea );

		} else {
			if ( ErrorIndex == 0 ) {
				ShowSevereMessage( "CalcSparrowWindward: Convection model not evaluated (bad face area)" );
				ShowContinueError( "Value for effective face area = " + RoundSigDigits( FaceArea, 5 ) );
				ShowContinueError( "Occurs for surface named = " + Surface( SurfNum ).Name );
				ShowContinueError( "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues" );
			}
			ShowRecurringSevereErrorAtEnd( "CalcSparrowWindward: Convection model not evaluated because bad face area and set to 9.999 [W/m2-k]", ErrorIndex );
			Hf = 9.999; // safe but noticeable
		}
		return Hf;

	}

	Real64
	CalcSparrowLeeward(
		int const RoughnessIndex,
		Real64 const FacePerimeter,
		Real64 const FaceArea,
		Real64 const WindAtZ,
		int const SurfNum
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate Sparrow Hf for leeward surfaces

		// METHODOLOGY EMPLOYED:
		// encapsulate equation as a function

		// REFERENCES:

		//   1. TARP Reference Manual, "Surface Outside Heat Balances", pp 71ff
		//   2. Sparrow, E. M., J. W. Ramsey, and E. A. Mass.  1979.  Effect of finite
		//   width on heat transfer and fluid flow about an inclined rectangular plate.
		//   Journal of Heat Transfer 101:  204.
		//   3. McClellan, T.M.  1996.  Investigation of a heat balance cooling load
		//   procedure with a detailed study of outside heat transfer parameters.
		//   M.S. Thesis, Department of Mechanical and Industrial Engineering,
		//   University of Illinois at Urbana-Champaign.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hf;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		static int ErrorIndex( 0 );

		if ( FaceArea > 0.0 ) {
			Hf = 2.53 * 0.5 * RoughnessMultiplier( RoughnessIndex ) * std::sqrt( FacePerimeter * WindAtZ / FaceArea );
		} else {
			if ( ErrorIndex == 0 ) {
				ShowSevereMessage( "CalcSparrowLeeward: Convection model not evaluated (bad face area)" );
				ShowContinueError( "Value for effective face area = " + RoundSigDigits( FaceArea, 5 ) );
				ShowContinueError( "Occurs for surface named = " + Surface( SurfNum ).Name );
				ShowContinueError( "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues" );
			}
			ShowRecurringSevereErrorAtEnd( "CalcSparrowLeeward: Convection model not evaluated because bad face area and set to 9.999 [W/m2-k]", ErrorIndex );

			Hf = 9.999; // safe but noticeable
		}
		return Hf;

	}

	Real64
	CalcMoWITTWindward(
		Real64 const DeltaTemp,
		Real64 const WindAtZ
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// calculate MoWITT Hc equation for windward surfaces

		// METHODOLOGY EMPLOYED:
		// encapsulate model equation in a function

		// REFERENCES:
		//   Yazdanian, M. and J.H. Klems.  1994.  Measurement of the exterior convective
		//   film coefficient for windows in low-rise buildings.
		//   ASHRAE Transactions 100(1):  1087.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hc; // total convection coefficient

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		static Real64 const temp_fac( pow_2( 0.84 ) );
		static Real64 const wind_fac( pow_2( 3.26 ) );
		static Real64 const two_thirds( 2.0 / 3.0 );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

//		Hc = std::pow( pow_2( 0.84 * std::pow( std::abs( DeltaTemp ), OneThird ) ) + pow_2( 3.26 * std::pow( WindAtZ, 0.89 ) ), 0.5 );
		Hc = std::sqrt( temp_fac * std::pow( std::abs( DeltaTemp ), two_thirds ) + wind_fac * std::pow( WindAtZ, 1.78 ) ); //Tuned

		return Hc;

	}

	Real64
	CalcMoWITTLeeward(
		Real64 const DeltaTemp,
		Real64 const WindAtZ
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// calculate MoWITT Hc equation for leeward surfaces

		// METHODOLOGY EMPLOYED:
		// encapsulate model equation in a function

		// REFERENCES:
		//   Yazdanian, M. and J.H. Klems.  1994.  Measurement of the exterior convective
		//   film coefficient for windows in low-rise buildings.
		//   ASHRAE Transactions 100(1):  1087.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hc; // total convection coefficient

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		static Real64 const temp_fac( pow_2( 0.84 ) );
		static Real64 const wind_fac( pow_2( 3.55 ) );
		static Real64 const two_thirds( 2.0 / 3.0 );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

//		Hc = std::pow( pow_2( 0.84 * std::pow( std::abs( DeltaTemp ), OneThird ) ) + pow_2( 3.55 * std::pow( WindAtZ, 0.617 ) ), 0.5 );
		Hc = std::sqrt( temp_fac * std::pow( std::abs( DeltaTemp ), two_thirds ) + wind_fac * std::pow( WindAtZ, 1.234 ) ); //Tuned

		return Hc;

	}

	Real64
	CalcDOE2Windward(
		Real64 const SurfaceTemp,
		Real64 const AirTemp,
		Real64 const CosineTilt,
		Real64 const WindAtZ,
		int const RoughnessIndex
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// calculate DOE-2 Hc equation for windward surfaces

		// METHODOLOGY EMPLOYED:
		// encapsulate model equation in a function

		// REFERENCES:
		//   Lawrence Berkeley Laboratory.  1994.  DOE2.1E-053 source code.
		//   Yazdanian, M. and J.H. Klems.  1994.  Measurement of the exterior convective
		//   film coefficient for windows in low-rise buildings.
		//   ASHRAE Transactions 100(1):  1087.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hf; // forced convection coefficient

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 HcSmooth;
		Real64 Hn;
		Real64 DeltaTemp;

		DeltaTemp = SurfaceTemp - AirTemp;

		Hn = CalcHnASHRAETARPExterior( SurfaceTemp, AirTemp, CosineTilt );

		HcSmooth = std::sqrt( pow_2( Hn ) + pow_2( 3.26 * std::pow( WindAtZ, 0.89 ) ) );

		Hf = RoughnessMultiplier( RoughnessIndex ) * ( HcSmooth - Hn );

		return Hf;

	}

	Real64
	CalcDOE2Leeward(
		Real64 const SurfaceTemp,
		Real64 const AirTemp,
		Real64 const CosineTilt,
		Real64 const WindAtZ,
		int const RoughnessIndex
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// calculate DOE-2 Hc equation for leeward surfaces

		// METHODOLOGY EMPLOYED:
		// encapsulate model equation in a function

		// REFERENCES:
		//   Lawrence Berkeley Laboratory.  1994.  DOE2.1E-053 source code.
		//   Yazdanian, M. and J.H. Klems.  1994.  Measurement of the exterior convective
		//   film coefficient for windows in low-rise buildings.
		//   ASHRAE Transactions 100(1):  1087.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hf; // forced convection coefficient

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 HcSmooth;
		Real64 Hn;
		Real64 DeltaTemp;

		DeltaTemp = SurfaceTemp - AirTemp;

		Hn = CalcHnASHRAETARPExterior( SurfaceTemp, AirTemp, CosineTilt );

		HcSmooth = std::sqrt( pow_2( Hn ) + pow_2( 3.55 * std::pow( WindAtZ, 0.617 ) ) );

		Hf = RoughnessMultiplier( RoughnessIndex ) * ( HcSmooth - Hn );

		return Hf;

	}

	Real64
	CalcNusseltJurges( Real64 const WindAtZ )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// calculate model equation for forced convection using Nusselt Jurges correlation
		// model is attributed to Nusselt and Jurges but the equation is recast in current units
		// by Palyvos

		// METHODOLOGY EMPLOYED:
		// encapsulate the model equation in a function

		// REFERENCES:
		// 1. Nusselt, W., W. Jurges. 1922. Die Kuhlung einer ebenen Wand durch einen Luftstrom
		//     (The cooling of a plane wall by an air flow). Gesundheits Ingenieur 52, Heft, 45, Jargang.
		// 2. Palyvos, J.A., 2008. A survey of wind convection coefficient correlations for building
		//     envelope energy systems' modeling. Applied Thermal Engineering 28 (2008) 801-808. Elsevier.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hc;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Hc = 5.8 + 3.94 * WindAtZ;

		return Hc;

	}

	Real64
	CalcMcAdams( Real64 const WindAtZ )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// calculate model equation for forced convection using McAdams correlation
		// model is attributed to McAdams but the equation is as recast in current units
		// by Palyvos

		// METHODOLOGY EMPLOYED:
		// encapsulate the model equation in a function

		// REFERENCES:
		// 1. McAdams, W.H., 1954. Heat Transmission, third ed., McGraw-Hill, New York.
		// 2. Palyvos, J.A., 2008. A survey of wind convection coefficient correlations for building
		//     envelope energy systems' modeling. Applied Thermal Engineering 28 (2008) 801-808. Elsevier.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hc;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Hc = 5.8 + 3.8 * WindAtZ;

		return Hc;

	}

	Real64
	CalcMitchell(
		Real64 const WindAtZ,
		Real64 const LengthScale,
		int const SurfNum
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// calculate model equation for forced convection using Mitchell correlation
		// model is attributed to Mitchell but the equation is as recast in current units
		// by Palyvos

		// METHODOLOGY EMPLOYED:
		// encapsulate the model equation in a function

		// REFERENCES:
		// 1. Mitchell, J.W., 1976. Heat transfer from spheres and other animal forms. Biophy. J. 16 (1976) 561
		// 2. Palyvos, J.A., 2008. A survey of wind convection coefficient correlations for building
		//     envelope energy systems' modeling. Applied Thermal Engineering 28 (2008) 801-808. Elsevier.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hf;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		static int ErrorIndex( 0 );

		if ( LengthScale > 0.0 ) {
			Hf = 8.6 * std::pow( WindAtZ, 0.6 ) / std::pow( LengthScale, 0.4 );
		} else {
			if ( ErrorIndex == 0 ) {
				ShowSevereMessage( "CalcMitchell: Convection model not evaluated (bad length scale)" );
				ShowContinueError( "Value for effective length scale = " + RoundSigDigits( LengthScale, 5 ) );
				ShowContinueError( "Occurs for surface named = " + Surface( SurfNum ).Name );
				ShowContinueError( "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues" );
			}
			ShowRecurringSevereErrorAtEnd( "CalcMitchell: Convection model not evaluated because bad length scale and set to 9.999 [W/m2-k]", ErrorIndex );
			Hf = 9.999; // safe but noticeable
		}
		return Hf;

	}

	Real64
	CalcBlockenWindward(
		Real64 const WindAt10m,
		Real64 const WindDir, // Wind direction measured clockwise from geographhic North
		Real64 const SurfAzimuth // or Facing, Direction the surface outward normal faces (degrees)
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// calculate model equation for forced convection using Blocken correlation

		// METHODOLOGY EMPLOYED:
		// encapsulate model in function

		// REFERENCES:
		// Blocken, B., T. Defraeye, D. Derome, J. Carmeliet. 2009.
		//  High-Resolution CFD Simulations for Forced Convection
		//   Heat Transfer Coefficients at the Facade of a Low-Rise Building.
		//   Building and Environment 44 (2009) 2396 - 2412.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hf;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 Theta; // angle between wind and surface azimuth

		Theta = WindDir - SurfAzimuth - 90.0; //TODO double check theta
		if ( Theta > 180.0 ) Theta -= 360.0;

		if ( Theta <= 11.25 ) {
			Hf = 4.6 * std::pow( WindAt10m, 0.89 );
		} else if ( ( 11.25 < Theta ) && ( Theta <= 33.75 ) ) {
			Hf = 5.0 * std::pow( WindAt10m, 0.8 );
		} else if ( ( 33.75 < Theta ) && ( Theta <= 56.25 ) ) {
			Hf = 4.6 * std::pow( WindAt10m, 0.84 );
		} else if ( ( 56.25 < Theta ) && ( Theta <= 100.0 ) ) {
			Hf = 4.5 * std::pow( WindAt10m, 0.81 );
		} else {
			// should not be used for leeward... check why come here?
			Hf = 3.54 * std::pow( WindAt10m, 0.76 ); //emmel model for robustness?
		}
		return Hf;

	}

	Real64
	CalcEmmelVertical(
		Real64 const WindAt10m,
		Real64 const WindDir, // Wind direction measured clockwise from geographhic North
		Real64 const SurfAzimuth, // or Facing, Direction the surface outward normal faces (degrees)
		int const SurfNum
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// calculate model equation for forced convection using Emmel correlation
		// for vertical walls

		// METHODOLOGY EMPLOYED:
		// encapsulate model in function

		// REFERENCES:
		// Emmel, M.G., M.O. Abadie, N. Mendes. 2007. New external convective
		//   heat transfer coefficient correlations for isolated low-rise buildings.
		//    Energy and Buildings 39 (2007) 335- 342

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hf;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 Theta; // angle between wind and surface azimuth
		static int ErrorIndex( 0 );

		Theta = WindDir - SurfAzimuth - 90.0; //TODO double check theta
		if ( Theta > 180.0 ) Theta -= 360.0;

		if ( Theta <= 22.5 ) {
			Hf = 5.15 * std::pow( WindAt10m, 0.81 );
		} else if ( ( 22.5 < Theta ) && ( Theta <= 67.5 ) ) {
			Hf = 3.34 * std::pow( WindAt10m, 0.84 );
		} else if ( ( 67.5 < Theta ) && ( Theta <= 112.5 ) ) {
			Hf = 4.78 * std::pow( WindAt10m, 0.71 );
		} else if ( ( 112.5 < Theta ) && ( Theta <= 157.5 ) ) {
			Hf = 4.05 * std::pow( WindAt10m, 0.77 );
		} else if ( ( 157.5 < Theta ) && ( Theta <= 180.0 ) ) {
			Hf = 3.54 * std::pow( WindAt10m, 0.76 );

		} else {
			if ( ErrorIndex == 0 ) {
				ShowSevereMessage( "CalcEmmelVertical: Convection model wind angle calculation suspect (developer issue)" );
				ShowContinueError( "Value for theta angle = " + RoundSigDigits( Theta, 5 ) );
				ShowContinueError( "Occurs for surface named = " + Surface( SurfNum ).Name );
				ShowContinueError( "Convection model uses high theta correlation and the simulation continues" );
			}
			ShowRecurringSevereErrorAtEnd( "CalcEmmelVertical: Convection model wind angle calculation suspect and high theta correlation", ErrorIndex );
			Hf = 3.54 * std::pow( WindAt10m, 0.76 );
		}
		return Hf;

	}

	Real64
	CalcEmmelRoof(
		Real64 const WindAt10m,
		Real64 const WindDir, // Wind direction measured clockwise from geographhic North
		Real64 const LongAxisOutwardAzimuth, // or Facing, Direction the surface outward normal faces (degrees)
		int const SurfNum
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// calculate model equation for forced convection using Emmel correlation
		// for horizontal roofs

		// METHODOLOGY EMPLOYED:
		// encapsulate model in function

		// REFERENCES:
		// Emmel, M.G., M.O. Abadie, N. Mendes. 2007. New external convective
		//   heat transfer coefficient correlations for isolated low-rise buildings.
		//    Energy and Buildings 39 (2007) 335- 342

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Hf;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 Theta; // angle between wind and surface azimuth
		static int ErrorIndex( 0 );

		Theta = WindDir - LongAxisOutwardAzimuth - 90.0; //TODO double check theta
		if ( Theta > 180.0 ) Theta -= 360.0;

		if ( Theta <= 22.5 ) {
			Hf = 5.15 * std::pow( WindAt10m, 0.81 );
		} else if ( ( 22.5 < Theta ) && ( Theta <= 67.5 ) ) {
			Hf = 3.34 * std::pow( WindAt10m, 0.84 );
		} else if ( ( 67.5 < Theta ) && ( Theta <= 112.5 ) ) {
			Hf = 4.78 * std::pow( WindAt10m, 0.71 );
		} else if ( ( 112.5 < Theta ) && ( Theta <= 157.5 ) ) {
			Hf = 4.05 * std::pow( WindAt10m, 0.77 );
		} else if ( ( 157.5 < Theta ) && ( Theta <= 180.0 ) ) {
			Hf = 3.54 * std::pow( WindAt10m, 0.76 );

		} else {
			if ( ErrorIndex == 0 ) {
				ShowSevereMessage( "CalcEmmelRoof: Convection model wind angle calculation suspect (developer issue)" );
				ShowContinueError( "Value for theta angle = " + RoundSigDigits( Theta, 5 ) );
				ShowContinueError( "Occurs for surface named = " + Surface( SurfNum ).Name );
				ShowContinueError( "Convection model uses high theta correlation and the simulation continues" );
			}
			ShowRecurringSevereErrorAtEnd( "CalcEmmelRoof: Convection model wind angle calculation suspect and high theta correlation", ErrorIndex );

			Hf = 3.54 * std::pow( WindAt10m, 0.76 );
		}
		return Hf;

	}

	Real64
	CalcClearRoof(
		int const SurfNum,
		Real64 const SurfTemp,
		Real64 const AirTemp,
		Real64 const WindAtZ,
		Real64 const EP_UNUSED( WindDirect ), // Wind direction measured clockwise from geographhic North
		Real64 const RoofArea,
		Real64 const RoofPerimeter
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         <author>
		//       DATE WRITTEN   <date_written>
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataEnvironment::OutBaroPress;
		using DataEnvironment::OutHumRat;
		using Psychrometrics::PsyRhoAirFnPbTdbW;

		// Return value
		Real64 Hc;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		Real64 const g( 9.81 ); // gravity constant (m/s**2)
		Real64 const v( 15.89e-6 ); // kinematic viscosity (m**2/s) for air at 300 K
		Real64 const k( 0.0263 ); // thermal conductivity (W/m K) for air at 300 K
		Real64 const Pr( 0.71 ); // Prandtl number for air at ?

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 DeltaTemp;
		Real64 Ln;
		Real64 RaLn; // Rayleigh number
		Real64 GrLn; // Grashof number
		Real64 AirDensity;
		Real64 Rex; // Reynolds number
		Real64 x; // distance to roof edge toward wind direction
		Real64 eta;
		Array1D< Real64 > RfARR( 6 );
		Real64 Rf;
		Real64 BetaFilm;
		static int ErrorIndex( 0 );

		RfARR = { 2.10, 1.67, 1.52, 1.13, 1.11, 1.0 };

		Rf = RfARR( Material( Construct( Surface( SurfNum ).Construction ).LayerPoint( 1 ) ).Roughness );
		//find x, don't know x. avoid time consuming geometry algorithm
		x = std::sqrt( RoofArea ) / 2.0; // quick simplification, geometry routines to develop

		if ( RoofPerimeter > 0.0 ) {
			Ln = RoofArea / RoofPerimeter;
		} else {
			Ln = std::sqrt( RoofArea );
		}
		DeltaTemp = SurfTemp - AirTemp;
		BetaFilm = 1.0 / ( KelvinConv + SurfTemp + 0.5 * DeltaTemp );
		AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, AirTemp, OutHumRat );

		GrLn = g * pow_2( AirDensity ) * pow_3( Ln ) * std::abs( DeltaTemp ) * BetaFilm / pow_2( v );
		RaLn = GrLn * Pr;

		Rex = WindAtZ * AirDensity * x / v;

		if ( Rex > 0.1 ) { //avoid zero and crazy small denominators
			eta = ( std::log( 1.0 + GrLn / pow_2( Rex ) ) ) / ( 1.0 + std::log( 1.0 + GrLn / pow_2( Rex ) ) );
		} else {
			eta = 1.0; // forced convection gone because no wind
		}

		if ( x > 0.0 ) {
			Hc = eta * ( k / Ln ) * 0.15 * std::pow( RaLn, OneThird ) + ( k / x ) * Rf * 0.0296 * std::pow( Rex, FourFifths ) * std::pow( Pr, OneThird );
		} else {
			if ( ErrorIndex == 0 ) {
				ShowSevereMessage( "CalcClearRoof: Convection model not evaluated (bad value for distance to roof edge)" );
				ShowContinueError( "Value for distance to roof edge =" + RoundSigDigits( x, 3 ) );
				ShowContinueError( "Occurs for surface named = " + Surface( SurfNum ).Name );
				ShowContinueError( "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues" );
			}
			ShowRecurringSevereErrorAtEnd( "CalcClearRoof: Convection model not evaluated because bad value for distance to roof edge and set to 9.999 [W/m2-k]", ErrorIndex );
			Hc = 9.9999; // safe but noticeable
		}
		return Hc;

	}

} // ConvectionCoefficients

} // EnergyPlus
