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

#ifndef ConvectionCoefficients_hh_INCLUDED
#define ConvectionCoefficients_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataVectorTypes.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace ConvectionCoefficients {

	// Using/Aliasing
	using DataVectorTypes::Vector;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern Real64 const AdaptiveHcInsideLowLimit; // W/m2-K
	extern Real64 const AdaptiveHcOutsideLowLimit; // W/m2-K

	extern Real64 const MinFlow; // Minimum mass flow rate
	extern Real64 const MaxACH; // Maximum ceiling diffuser correlation limit

	extern Real64 const OneThird; // 1/3 in highest precision
	extern Real64 const OneFourth; // 1/4 in highest precision
	extern Real64 const OneFifth; // 1/5 in highest precision
	extern Real64 const OneSixth; // 1/6 in highest precision
	extern Real64 const FourFifths; // 4/5 in highest precision

	// Coefficients that modify the convection coeff based on surface roughness
	extern Array1D< Real64 > const RoughnessMultiplier;

	// parameters for identifying more specific hc model equations, inside face
	extern int const HcInt_UserValue;
	extern int const HcInt_UserSchedule;
	extern int const HcInt_UserCurve;
	extern int const HcInt_ASHRAEVerticalWall;
	extern int const HcInt_WaltonUnstableHorizontalOrTilt;
	extern int const HcInt_WaltonStableHorizontalOrTilt;
	extern int const HcInt_FisherPedersenCeilDiffuserFloor;
	extern int const HcInt_FisherPedersenCeilDiffuserCeiling;
	extern int const HcInt_FisherPedersenCeilDiffuserWalls;
	extern int const HcInt_AlamdariHammondStableHorizontal;
	extern int const HcInt_AlamdariHammondVerticalWall;
	extern int const HcInt_AlamdariHammondUnstableHorizontal;
	extern int const HcInt_KhalifaEq3WallAwayFromHeat;
	extern int const HcInt_KhalifaEq4CeilingAwayFromHeat;
	extern int const HcInt_KhalifaEq5WallNearHeat;
	extern int const HcInt_KhalifaEq6NonHeatedWalls;
	extern int const HcInt_KhalifaEq7Ceiling;
	extern int const HcInt_AwbiHattonHeatedFloor;
	extern int const HcInt_AwbiHattonHeatedWall;
	extern int const HcInt_BeausoleilMorrisonMixedAssistingWall;
	extern int const HcInt_BeausoleilMorrisonMixedOppossingWall;
	extern int const HcInt_BeausoleilMorrisonMixedStableCeiling;
	extern int const HcInt_BeausoleilMorrisonMixedUnstableCeiling;
	extern int const HcInt_BeausoleilMorrisonMixedStableFloor;
	extern int const HcInt_BeausoleilMorrisonMixedUnstableFloor;
	extern int const HcInt_FohannoPolidoriVerticalWall;
	extern int const HcInt_KaradagChilledCeiling;
	extern int const HcInt_ISO15099Windows;
	extern int const HcInt_GoldsteinNovoselacCeilingDiffuserWindow;
	extern int const HcInt_GoldsteinNovoselacCeilingDiffuserWalls;
	extern int const HcInt_GoldsteinNovoselacCeilingDiffuserFloor;

	//parameters for identifying more specific hc model equations, outside face
	extern int const HcExt_None; // none is allowed because Hn and Hf are split
	extern int const HcExt_UserValue;
	extern int const HcExt_UserSchedule;
	extern int const HcExt_UserCurve;
	extern int const HcExt_ASHRAESimpleCombined;
	extern int const HcExt_NaturalASHRAEVerticalWall;
	extern int const HcExt_NaturalWaltonUnstableHorizontalOrTilt;
	extern int const HcExt_NaturalWaltonStableHorizontalOrTilt;
	extern int const HcExt_SparrowWindward;
	extern int const HcExt_SparrowLeeward;
	extern int const HcExt_MoWiTTWindward;
	extern int const HcExt_MoWiTTLeeward;
	extern int const HcExt_DOE2Windward;
	extern int const HcExt_DOE2Leeward;
	extern int const HcExt_NusseltJurges;
	extern int const HcExt_McAdams;
	extern int const HcExt_Mitchell;
	extern int const HcExt_ClearRoof;
	extern int const HcExt_BlockenWindward;
	extern int const HcExt_EmmelVertical;
	extern int const HcExt_EmmelRoof;
	extern int const HcExt_AlamdariHammondVerticalWall;
	extern int const HcExt_FohannoPolidoriVerticalWall;
	extern int const HcExt_ISO15099Windows;
	extern int const HcExt_AlamdariHammondStableHorizontal;
	extern int const HcExt_AlamdariHammondUnstableHorizontal;

	//parameters, by zone, for flow regimes for adaptive convection on inside face
	extern int const InConvFlowRegime_A1; // In-floor heating or in-ceiling cooling
	extern int const InConvFlowRegime_A2; // In-wall heating
	extern int const InConvFlowRegime_A3; // no HVAC system, all bouyancy
	extern int const InConvFlowRegime_B; // Convective heater in zone
	extern int const InConvFlowRegime_C; // central mechanical air
	extern int const InConvFlowRegime_D; // zone mechanical air
	extern int const InConvFlowRegime_E; // mixed. mechancial air and bouyancy

	//params for reference temperature type
	extern int const RefTempMeanAirTemp;
	extern int const RefTempAdjacentAirTemp;
	extern int const RefTempSupplyAirTemp;
	extern int const RefTempOutDryBulbAtZ;
	extern int const RefTempOutDryBulbEPW;
	extern int const RefTempOutWetBulbAtZ;
	extern int const RefTempOutWetBulbEPW;

	//params for wind speed type
	extern int const RefWindWeatherFile;
	extern int const RefWindAtZ;
	extern int const RefWindParallComp;
	extern int const RefWindParallCompAtZ;

	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:

	extern int TotOutsideHcUserCurves;
	extern int TotInsideHcUserCurves;
	extern bool GetUserSuppliedConvectionCoeffs; // Get user input first call for Init

	extern bool ConvectionGeometryMetaDataSetup; // set to true once geometry meta data are setup
	extern Real64 CubeRootOfOverallBuildingVolume; // building meta data. cube root of the volume of all the zones
	extern Real64 RoofLongAxisOutwardAzimuth; // roof surfaces meta data. outward normal azimuth for longest roof edge

	// SUBROUTINE SPECIFICATIONS:
	//PRIVATE ApplyConvectionValue ! internal to GetUserConvectionCoefficients

	//more specific Hc model equations

	// Types

	struct HcInsideFaceUserCurveStruct
	{
		// Members
		std::string Name; // user's name for object
		int ReferenceTempType;
		int HcFnTempDiffCurveNum;
		int HcFnTempDiffDivHeightCurveNum;
		int HcFnACHCurveNum;
		int HcFnACHDivPerimLengthCurveNum;

		// Default Constructor
		HcInsideFaceUserCurveStruct() :
			ReferenceTempType( 0 ),
			HcFnTempDiffCurveNum( 0 ),
			HcFnTempDiffDivHeightCurveNum( 0 ),
			HcFnACHCurveNum( 0 ),
			HcFnACHDivPerimLengthCurveNum( 0 )
		{}

	};

	struct HcOutsideFaceUserCurveStruct
	{
		// Members
		std::string Name;
		int ReferenceTempType;
		bool SuppressRainChange;
		int WindSpeedType;
		int HfFnWindSpeedCurveNum;
		int HnFnTempDiffCurveNum;
		int HnFnTempDiffDivHeightCurveNum;

		// Default Constructor
		HcOutsideFaceUserCurveStruct() :
			ReferenceTempType( 0 ),
			SuppressRainChange( false ),
			WindSpeedType( 0 ),
			HfFnWindSpeedCurveNum( 0 ),
			HnFnTempDiffCurveNum( 0 ),
			HnFnTempDiffDivHeightCurveNum( 0 )
		{}

	};

	struct InsideFaceAdaptiveConvAlgoStruct
	{
		// Members
		bool EnteredByUser;
		std::string Name;
		int SimpleBouyVertWallEqNum; // InConvClass_A3_VertWalls
		int SimpleBouyVertWallUserCurveNum;
		int SimpleBouyStableHorizEqNum; // InConvClass_A3_StableHoriz
		int SimpleBouyStableHorizUserCurveNum;
		int SimpleBouyUnstableHorizEqNum; // InConvClass_A3_UnstableHoriz
		int SimpleBouyUnstableHorizUserCurveNum;
		int SimpleBouyStableTiltedEqNum; // InConvClass_A3_StableTilted
		int SimpleBouyStableTiltedUserCurveNum;
		int SimpleBouyUnstableTiltedEqNum; // InConvClass_A3_UnstableTilted
		int SimpleBouyUnstableTiltedUserCurveNum;
		int SimpleBouyWindowsEqNum; // InConvClass_A3_Windows
		int SimpleBouyWindowsUserCurveNum;
		int FloorHeatCeilingCoolVertWallEqNum; // InConvClass_A1_VertWalls
		int FloorHeatCeilingCoolVertWallUserCurveNum;
		int FloorHeatCeilingCoolStableHorizEqNum; // InConvClass_A1_StableHoriz
		int FloorHeatCeilingCoolStableHorizUserCurveNum;
		int FloorHeatCeilingCoolUnstableHorizEqNum; // InConvClass_A1_UntableHoriz
		int FloorHeatCeilingCoolUnstableHorizUserCurveNum;
		int FloorHeatCeilingCoolHeatedFloorEqNum; // InConvClass_A1_HeatedFloor
		int FloorHeatCeilingCoolHeatedFloorUserCurveNum;
		int FloorHeatCeilingCoolChilledCeilingEqNum; // InConvClass_A1_ChilledCeil
		int FloorHeatCeilingCoolChilledCeilingUserCurveNum;
		int FloorHeatCeilingCoolStableTiltedEqNum; // InConvClass_A1_StableTilted
		int FloorHeatCeilingCoolStableTiltedUserCurveNum;
		int FloorHeatCeilingCoolUnstableTiltedEqNum; // InConvClass_A1_UnstableTilted
		int FloorHeatCeilingCoolUnstableTiltedUserCurveNum;
		int FloorHeatCeilingCoolWindowsEqNum; // InConvClass_A1_Windows
		int FloorHeatCeilingCoolWindowsUserCurveNum;
		int WallPanelHeatVertWallEqNum; // InConvClass_A2_VertWallsNonHeated
		int WallPanelHeatVertWallUserCurveNum;
		int WallPanelHeatHeatedWallEqNum; // InConvClass_A2_HeatedVerticalWall
		int WallPanelHeatHeatedWallUserCurveNum;
		int WallPanelHeatStableHorizEqNum; // InConvClass_A2_StableHoriz
		int WallPanelHeatStableHorizUserCurveNum;
		int WallPanelHeatUnstableHorizEqNum; // InConvClass_A2_UnstableHoriz
		int WallPanelHeatUnstableHorizUserCurveNum;
		int WallPanelHeatStableTiltedEqNum; // InConvClass_A2_StableTilted
		int WallPanelHeatStableTiltedUserCurveNum;
		int WallPanelHeatUnstableTiltedEqNum; // InConvClass_A2_UnstableTilted
		int WallPanelHeatUnstableTiltedUserCurveNum;
		int WallPanelHeatWindowsEqNum; // InConvClass_A2_Windows
		int WallPanelHeatWindowsUserCurveNum;
		int ConvectiveHeatVertWallEqNum;
		int ConvectiveHeatVertWallUserCurveNum;
		int ConvectiveHeatVertWallNearHeaterEqNum;
		int ConvectiveHeatVertWallNearHeaterUserCurveNum;
		int ConvectiveHeatStableHorizEqNum;
		int ConvectiveHeatStableHorizUserCurveNum;
		int ConvectiveHeatUnstableHorizEqNum;
		int ConvectiveHeatUnstableHorizUserCurveNum;
		int ConvectiveHeatStableTiltedEqNum;
		int ConvectiveHeatStableTiltedUserCurveNum;
		int ConvectiveHeatUnstableTiltedEqNum;
		int ConvectiveHeatUnstableTiltedUserCurveNum;
		int ConvectiveHeatWindowsEqNum;
		int ConvectiveHeatWindowsUserCurveNum;
		int CentralAirWallEqNum;
		int CentralAirWallUserCurveNum;
		int CentralAirCeilingEqNum;
		int CentralAirCeilingUserCurveNum;
		int CentralAirFloorEqNum;
		int CentralAirFloorUserCurveNum;
		int CentralAirWindowsEqNum;
		int CentralAirWindowsUserCurveNum;
		int ZoneFanCircVertWallEqNum;
		int ZoneFanCircVertWallUserCurveNum;
		int ZoneFanCircStableHorizEqNum;
		int ZoneFanCircStableHorizUserCurveNum;
		int ZoneFanCircUnstableHorizEqNum;
		int ZoneFanCircUnstableHorizUserCurveNum;
		int ZoneFanCircStableTiltedEqNum;
		int ZoneFanCircStableTiltedUserCurveNum;
		int ZoneFanCircUnstableTiltedEqNum;
		int ZoneFanCircUnstableTiltedUserCurveNum;
		int ZoneFanCircWindowsEqNum;
		int ZoneFanCircWindowsUserCurveNum;
		int MixedBouyAssistingFlowWallEqNum;
		int MixedBouyAssistingFlowWallUserCurveNum;
		int MixedBouyOppossingFlowWallEqNum;
		int MixedBouyOppossingFlowWallUserCurveNum;
		int MixedStableFloorEqNum;
		int MixedStableFloorUserCurveNum;
		int MixedUnstableFloorEqNum;
		int MixedUnstableFloorUserCurveNum;
		int MixedStableCeilingEqNum;
		int MixedStableCeilingUserCurveNum;
		int MixedUnstableCeilingEqNum;
		int MixedUnstableCeilingUserCurveNum;
		int MixedWindowsEqNum;
		int MixedWindowsUserCurveNum;

		// Default Constructor
		InsideFaceAdaptiveConvAlgoStruct() :
			EnteredByUser( false ),
			SimpleBouyVertWallEqNum( HcInt_FohannoPolidoriVerticalWall ),
			SimpleBouyVertWallUserCurveNum( 0 ),
			SimpleBouyStableHorizEqNum( HcInt_AlamdariHammondStableHorizontal ),
			SimpleBouyStableHorizUserCurveNum( 0 ),
			SimpleBouyUnstableHorizEqNum( HcInt_AlamdariHammondUnstableHorizontal ),
			SimpleBouyUnstableHorizUserCurveNum( 0 ),
			SimpleBouyStableTiltedEqNum( HcInt_WaltonStableHorizontalOrTilt ),
			SimpleBouyStableTiltedUserCurveNum( 0 ),
			SimpleBouyUnstableTiltedEqNum( HcInt_WaltonUnstableHorizontalOrTilt ),
			SimpleBouyUnstableTiltedUserCurveNum( 0 ),
			SimpleBouyWindowsEqNum( HcInt_ISO15099Windows ),
			SimpleBouyWindowsUserCurveNum( 0 ),
			FloorHeatCeilingCoolVertWallEqNum( HcInt_KhalifaEq3WallAwayFromHeat ),
			FloorHeatCeilingCoolVertWallUserCurveNum( 0 ),
			FloorHeatCeilingCoolStableHorizEqNum( HcInt_AlamdariHammondStableHorizontal ),
			FloorHeatCeilingCoolStableHorizUserCurveNum( 0 ),
			FloorHeatCeilingCoolUnstableHorizEqNum( HcInt_KhalifaEq4CeilingAwayFromHeat ),
			FloorHeatCeilingCoolUnstableHorizUserCurveNum( 0 ),
			FloorHeatCeilingCoolHeatedFloorEqNum( HcInt_AwbiHattonHeatedFloor ),
			FloorHeatCeilingCoolHeatedFloorUserCurveNum( 0 ),
			FloorHeatCeilingCoolChilledCeilingEqNum( HcInt_KaradagChilledCeiling ),
			FloorHeatCeilingCoolChilledCeilingUserCurveNum( 0 ),
			FloorHeatCeilingCoolStableTiltedEqNum( HcInt_WaltonStableHorizontalOrTilt ),
			FloorHeatCeilingCoolStableTiltedUserCurveNum( 0 ),
			FloorHeatCeilingCoolUnstableTiltedEqNum( HcInt_WaltonUnstableHorizontalOrTilt ),
			FloorHeatCeilingCoolUnstableTiltedUserCurveNum( 0 ),
			FloorHeatCeilingCoolWindowsEqNum( HcInt_ISO15099Windows ),
			FloorHeatCeilingCoolWindowsUserCurveNum( 0 ),
			WallPanelHeatVertWallEqNum( HcInt_KhalifaEq6NonHeatedWalls ),
			WallPanelHeatVertWallUserCurveNum( 0 ),
			WallPanelHeatHeatedWallEqNum( HcInt_AwbiHattonHeatedWall ),
			WallPanelHeatHeatedWallUserCurveNum( 0 ),
			WallPanelHeatStableHorizEqNum( HcInt_AlamdariHammondStableHorizontal ),
			WallPanelHeatStableHorizUserCurveNum( 0 ),
			WallPanelHeatUnstableHorizEqNum( HcInt_KhalifaEq7Ceiling ),
			WallPanelHeatUnstableHorizUserCurveNum( 0 ),
			WallPanelHeatStableTiltedEqNum( HcInt_WaltonStableHorizontalOrTilt ),
			WallPanelHeatStableTiltedUserCurveNum( 0 ),
			WallPanelHeatUnstableTiltedEqNum( HcInt_WaltonUnstableHorizontalOrTilt ),
			WallPanelHeatUnstableTiltedUserCurveNum( 0 ),
			WallPanelHeatWindowsEqNum( HcInt_ISO15099Windows ),
			WallPanelHeatWindowsUserCurveNum( 0 ),
			ConvectiveHeatVertWallEqNum( HcInt_FohannoPolidoriVerticalWall ),
			ConvectiveHeatVertWallUserCurveNum( 0 ),
			ConvectiveHeatVertWallNearHeaterEqNum( HcInt_KhalifaEq5WallNearHeat ),
			ConvectiveHeatVertWallNearHeaterUserCurveNum( 0 ),
			ConvectiveHeatStableHorizEqNum( HcInt_AlamdariHammondStableHorizontal ),
			ConvectiveHeatStableHorizUserCurveNum( 0 ),
			ConvectiveHeatUnstableHorizEqNum( HcInt_KhalifaEq7Ceiling ),
			ConvectiveHeatUnstableHorizUserCurveNum( 0 ),
			ConvectiveHeatStableTiltedEqNum( HcInt_WaltonStableHorizontalOrTilt ),
			ConvectiveHeatStableTiltedUserCurveNum( 0 ),
			ConvectiveHeatUnstableTiltedEqNum( HcInt_WaltonUnstableHorizontalOrTilt ),
			ConvectiveHeatUnstableTiltedUserCurveNum( 0 ),
			ConvectiveHeatWindowsEqNum( HcInt_ISO15099Windows ),
			ConvectiveHeatWindowsUserCurveNum( 0 ),
			CentralAirWallEqNum( HcInt_GoldsteinNovoselacCeilingDiffuserWalls ),
			CentralAirWallUserCurveNum( 0 ),
			CentralAirCeilingEqNum( HcInt_FisherPedersenCeilDiffuserCeiling ),
			CentralAirCeilingUserCurveNum( 0 ),
			CentralAirFloorEqNum( HcInt_GoldsteinNovoselacCeilingDiffuserFloor ),
			CentralAirFloorUserCurveNum( 0 ),
			CentralAirWindowsEqNum( HcInt_GoldsteinNovoselacCeilingDiffuserWindow ),
			CentralAirWindowsUserCurveNum( 0 ),
			ZoneFanCircVertWallEqNum( HcInt_KhalifaEq3WallAwayFromHeat ),
			ZoneFanCircVertWallUserCurveNum( 0 ),
			ZoneFanCircStableHorizEqNum( HcInt_AlamdariHammondStableHorizontal ),
			ZoneFanCircStableHorizUserCurveNum( 0 ),
			ZoneFanCircUnstableHorizEqNum( HcInt_KhalifaEq4CeilingAwayFromHeat ),
			ZoneFanCircUnstableHorizUserCurveNum( 0 ),
			ZoneFanCircStableTiltedEqNum( HcInt_WaltonStableHorizontalOrTilt ),
			ZoneFanCircStableTiltedUserCurveNum( 0 ),
			ZoneFanCircUnstableTiltedEqNum( HcInt_WaltonUnstableHorizontalOrTilt ),
			ZoneFanCircUnstableTiltedUserCurveNum( 0 ),
			ZoneFanCircWindowsEqNum( HcInt_ISO15099Windows ),
			ZoneFanCircWindowsUserCurveNum( 0 ),
			MixedBouyAssistingFlowWallEqNum( HcInt_BeausoleilMorrisonMixedAssistingWall ),
			MixedBouyAssistingFlowWallUserCurveNum( 0 ),
			MixedBouyOppossingFlowWallEqNum( HcInt_BeausoleilMorrisonMixedOppossingWall ),
			MixedBouyOppossingFlowWallUserCurveNum( 0 ),
			MixedStableFloorEqNum( HcInt_BeausoleilMorrisonMixedStableFloor ),
			MixedStableFloorUserCurveNum( 0 ),
			MixedUnstableFloorEqNum( HcInt_BeausoleilMorrisonMixedUnstableFloor ),
			MixedUnstableFloorUserCurveNum( 0 ),
			MixedStableCeilingEqNum( HcInt_BeausoleilMorrisonMixedStableCeiling ),
			MixedStableCeilingUserCurveNum( 0 ),
			MixedUnstableCeilingEqNum( HcInt_BeausoleilMorrisonMixedUnstableCeiling ),
			MixedUnstableCeilingUserCurveNum( 0 ),
			MixedWindowsEqNum( HcInt_GoldsteinNovoselacCeilingDiffuserWindow ),
			MixedWindowsUserCurveNum( 0 )
		{}

	};

	struct OutsideFaceAdpativeConvAlgoStruct
	{
		// Members
		bool EnteredByUser;
		std::string Name;
		bool SuppressRainChange;
		int HWindWallWindwardEqNum;
		int HWindWallWindwardUserCurveNum;
		int HWindWallLeewardEqNum;
		int HWindWallLeewardUserCurveNum;
		int HWindHorizRoofEqNum;
		int HWindHorizRoofUserCurveNum;
		int HNatVertWallEqNum;
		int HNatVertWallUserCurveNum;
		int HNatStableHorizEqNum;
		int HNatStableHorizUserCurveNum;
		int HNatUnstableHorizEqNum;
		int HNatUstableHorizUserCurveNum;

		// Default Constructor
		OutsideFaceAdpativeConvAlgoStruct() :
			EnteredByUser( false ),
			SuppressRainChange( false ),
			HWindWallWindwardEqNum( HcExt_BlockenWindward ),
			HWindWallWindwardUserCurveNum( 0 ),
			HWindWallLeewardEqNum( HcExt_EmmelVertical ),
			HWindWallLeewardUserCurveNum( 0 ),
			HWindHorizRoofEqNum( HcExt_ClearRoof ),
			HWindHorizRoofUserCurveNum( 0 ),
			HNatVertWallEqNum( HcExt_NaturalASHRAEVerticalWall ),
			HNatVertWallUserCurveNum( 0 ),
			HNatStableHorizEqNum( HcExt_NaturalWaltonStableHorizontalOrTilt ),
			HNatStableHorizUserCurveNum( 0 ),
			HNatUnstableHorizEqNum( HcExt_NaturalWaltonUnstableHorizontalOrTilt ),
			HNatUstableHorizUserCurveNum( 0 )
		{}

	};

	struct BoundingBoxVertStruct
	{
		// Members
		int SurfNum;
		int VertNum;
		Vector Vertex;

		// Default Constructor
		BoundingBoxVertStruct() :
			SurfNum( 0 ),
			VertNum( 0 ),
			Vertex( 0.0, 0.0, 0.0 )
		{}

	};

	struct RoofGeoCharactisticsStruct
	{
		// Members
		BoundingBoxVertStruct XdYdZd; // 1 low x, low y, low z
		BoundingBoxVertStruct XdYdZu; // 2 low x, low y, hi z
		BoundingBoxVertStruct XdYuZd; // 3 low x, hi y, low z
		BoundingBoxVertStruct XdYuZu; // 4 low x, hi y, hi z
		BoundingBoxVertStruct XuYdZd; // 5 hi x, low y, low z
		BoundingBoxVertStruct XuYuZd; // 6 hi x, hi y, low z
		BoundingBoxVertStruct XuYdZu; // 7 hi x, low y, hi z
		BoundingBoxVertStruct XuYuZu; // 8 hi x, hi y, hi z
		Array1D< Vector > BoundSurf; // long edge of roof group bounding surface
		Real64 Area;
		Real64 Perimeter;
		Real64 Height;

		// Default Constructor
		RoofGeoCharactisticsStruct() :
			BoundSurf( 4 ),
			Area( 0.0 ),
			Perimeter( 0.0 ),
			Height( 0.0 )
		{}

	};

	// Object Data
	extern InsideFaceAdaptiveConvAlgoStruct InsideFaceAdaptiveConvectionAlgo; // stores rules for Hc model equations
	extern OutsideFaceAdpativeConvAlgoStruct OutsideFaceAdaptiveConvectionAlgo;
	extern Array1D< HcInsideFaceUserCurveStruct > HcInsideUserCurve;
	extern Array1D< HcOutsideFaceUserCurveStruct > HcOutsideUserCurve;
	extern RoofGeoCharactisticsStruct RoofGeo;

	// Functions

	void
	InitInteriorConvectionCoeffs(
		Array1S< Real64 > const SurfaceTemperatures, // Temperature of surfaces for evaluation of HcIn
		Optional_int_const ZoneToResimulate = _ // if passed in, then only calculate surfaces that have this zone
	);

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
	);

	Real64
	CalcHfExteriorSparrow(
		Real64 const SurfWindSpeed, // Local wind speed at height of the heat transfer surface (m/s)
		Real64 const GrossArea, // Gross surface area {m2}
		Real64 const Perimeter, // Surface perimeter length {m}
		Real64 const CosTilt, // Cosine of the Surface Tilt Angle
		Real64 const Azimuth, // Facing angle (degrees) of the surface outward normal
		int const Roughness, // Surface roughness index (6=very smooth, 5=smooth, 4=medium smooth,
		Real64 const WindDirection // Wind (compass) direction (degrees)
	);

	Real64
	CalcHnASHRAETARPExterior(
		Real64 const TOutSurf, // Exterior surface temperature
		Real64 const TAir, // Outdoor Air temperature
		Real64 const CosTilt // Cosine of the Surface Tilt Angle (Angle between the ground outward normal and
	);

	bool
	Windward(
		Real64 const CosTilt, // Cosine of the surface tilt angle
		Real64 const Azimuth, // or Facing, Direction the surface outward normal faces (degrees)
		Real64 const WindDirection // Wind direction measured clockwise from geographhic North
	);

	void
	GetUserConvectionCoefficients();

	void
	ApplyConvectionValue(
		std::string const & SurfaceTypes,
		std::string const & ConvectionType,
		int const Value
	);

	Real64
	CalcASHRAESimpExtConvectCoeff(
		int const Roughness, // Integer index for roughness, relates to parameter array indices
		Real64 const SurfWindSpeed // Current wind speed, m/s
	);

	void
	CalcASHRAESimpleIntConvCoeff(
		int const SurfNum, // surface number for which coefficients are being calculated
		Real64 const SurfaceTemperature, // Temperature of surface for evaluation of HcIn
		Real64 const ZoneMeanAirTemperature // Mean Air Temperature of Zone
	);

	void
	CalcASHRAEDetailedIntConvCoeff(
		int const SurfNum, // surface number for which coefficients are being calculated
		Real64 const SurfaceTemperature, // Temperature of surface for evaluation of HcIn
		Real64 const ZoneMeanAirTemperature // Mean Air Temperature of Zone
	);

	void
	CalcDetailedHcInForDVModel(
		int const SurfNum, // surface number for which coefficients are being calculated
		Array1S< Real64 > const SurfaceTemperatures, // Temperature of surfaces for evaluation of HcIn
		Array1S< Real64 > HcIn, // Interior Convection Coeff Array
		Optional< Array1S< Real64 > const > Vhc = _ // Velocity array for forced convection coeff calculation
	);

	void
	CalcCeilingDiffuserIntConvCoeff( int const ZoneNum ); // zone number for which coefficients are being calculated

	// CalcCeilingDiffuserInletCorr should replace CalcCeilingDiffuser (above), if ZoneTempPredictorCorrector can
	// ever be made to work correctly with the inlet air temperature.

	void
	CalcCeilingDiffuserInletCorr(
		int const ZoneNum, // Zone number
		Array1S< Real64 > const SurfaceTemperatures // For CalcASHRAEDetailed, if called
	);

	void
	CalcTrombeWallIntConvCoeff(
		int const ZoneNum, // Zone number for which coefficients are being calculated
		Array1S< Real64 > const SurfaceTemperatures // Temperature of surfaces for evaluation of HcIn
	);

	void
	CalcNusselt(
		int const SurfNum, // Surface number
		Real64 const asp, // Aspect ratio: window height to gap width
		Real64 const tso, // Temperature of gap surface closest to outside (K)
		Real64 const tsi, // Temperature of gap surface closest to zone (K)
		Real64 const gr, // Gap gas Grashof number
		Real64 const pr, // Gap gas Prandtl number
		Real64 & gnu // Gap gas Nusselt number
	);

	Real64
	SetExtConvectionCoeff( int const SurfNum ); // Surface Number

	Real64
	SetIntConvectionCoeff( int const SurfNum ); // Surface Number

	void
	CalcISO15099WindowIntConvCoeff(
		int const SurfNum, // surface number for which coefficients are being calculated
		Real64 const SurfaceTemperature, // Temperature of surface for evaluation of HcIn
		Real64 const AirTemperature // Mean Air Temperature of Zone (or adjacent air temperature)
	);

	void
	SetupAdaptiveConvectionStaticMetaData();

	void
	SetupAdaptiveConvectionRadiantSurfaceData();

	void
	ManageInsideAdaptiveConvectionAlgo( int const SurfNum ); // surface number for which coefficients are being calculated

	void
	ManageOutsideAdaptiveConvectionAlgo(
		int const SurfNum, // surface number for which coefficients are being calculated
		Real64 & Hc // result for Hc Outside face, becomes HExt.
	);

	void
	EvaluateIntHcModels(
		int const SurfNum,
		int const ConvModelEquationNum,
		Real64 & Hc // calculated Hc value
	);

	void
	EvaluateExtHcModels(
		int const SurfNum,
		int const NaturalConvModelEqNum,
		int const ForcedConvModelEqNum,
		Real64 & Hc
	);

	void
	DynamicExtConvSurfaceClassification( int const SurfNum ); // surface number

	void
	MapExtConvClassificationToHcModels( int const SurfNum ); // surface number

	void
	DynamicIntConvSurfaceClassification( int const SurfNum ); // surface number

	void
	MapIntConvClassificationToHcModels( int const SurfNum ); // surface pointer index

	void
	CalcUserDefinedInsideHcModel(
		int const SurfNum,
		int const UserCurveNum,
		Real64 & Hc
	);

	void
	CalcUserDefinedOutsideHcModel(
		int const SurfNum,
		int const UserCurveNum,
		Real64 & H
	);

	//** Begin catalog of Hc equation functions. **** !*************************************************

	Real64
	CalcASHRAEVerticalWall( Real64 const DeltaTemp ); // [C] temperature difference between surface and air

	Real64
	CalcWaltonUnstableHorizontalOrTilt(
		Real64 const DeltaTemp, // [C] temperature difference between surface and air
		Real64 const CosineTilt // Cosine of tilt angle
	);

	Real64
	CalcWaltonStableHorizontalOrTilt(
		Real64 const DeltaTemp, // [C] temperature difference between surface and air
		Real64 const CosineTilt // Cosine of tilt angle
	);

	Real64
	CalcFisherPedersenCeilDiffuserFloor( Real64 const AirChangeRate ); // [1/hr] air system air change rate

	Real64
	CalcFisherPedersenCeilDiffuserCeiling( Real64 const AirChangeRate ); // [1/hr] air system air change rate

	Real64
	CalcFisherPedersenCeilDiffuserWalls( Real64 const AirChangeRate ); // [1/hr] air system air change rate

	Real64
	CalcAlamdariHammondUnstableHorizontal(
		Real64 const DeltaTemp, // [C] temperature difference between surface and air
		Real64 const HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
		int const SurfNum // for messages
	);

	Real64
	CalcAlamdariHammondStableHorizontal(
		Real64 const DeltaTemp, // [C] temperature difference between surface and air
		Real64 const HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
		int const SurfNum // for messages
	);

	Real64
	CalcAlamdariHammondVerticalWall(
		Real64 const DeltaTemp, // [C] temperature difference between surface and air
		Real64 const Height, // [m] characteristic size, = zone height
		int const SurfNum // for messages
	);

	Real64
	CalcKhalifaEq3WallAwayFromHeat( Real64 const DeltaTemp ); // [C] temperature difference between surface and air

	Real64
	CalcKhalifaEq4CeilingAwayFromHeat( Real64 const DeltaTemp ); // [C] temperature difference between surface and air

	Real64
	CalcKhalifaEq5WallsNearHeat( Real64 const DeltaTemp ); // [C] temperature difference between surface and air

	Real64
	CalcKhalifaEq6NonHeatedWalls( Real64 const DeltaTemp ); // [C] temperature difference between surface and air

	Real64
	CalcKhalifaEq7Ceiling( Real64 const DeltaTemp ); // [C] temperature difference between surface and air

	Real64
	CalcAwbiHattonHeatedFloor(
		Real64 const DeltaTemp, // [C] temperature difference between surface and air
		Real64 const HydraulicDiameter // [m] characteristic size, = (4 * area) / perimeter
	);

	Real64
	CalcAwbiHattonHeatedWall(
		Real64 const DeltaTemp, // [C] temperature difference between surface and air
		Real64 const HydraulicDiameter // [m] characteristic size, = (4 * area) / perimeter
	);

	Real64
	CalcBeausoleilMorrisonMixedAssistedWall(
		Real64 const DeltaTemp, // [C] temperature difference between surface and air
		Real64 const Height, // [m] characteristic size
		Real64 const SurfTemp, // [C] surface temperature
		Real64 const SupplyAirTemp, // [C] temperature of supply air into zone
		Real64 const AirChangeRate, // [ACH] [1/hour] supply air ACH for zone
		int const ZoneNum // index of zone for messaging
	);

	Real64
	CalcBeausoleilMorrisonMixedOpposingWall(
		Real64 const DeltaTemp, // [C] temperature difference between surface and air
		Real64 const Height, // [m] characteristic size
		Real64 const SurfTemp, // [C] surface temperature
		Real64 const SupplyAirTemp, // [C] temperature of supply air into zone
		Real64 const AirChangeRate, // [ACH] [1/hour] supply air ACH for zone
		int const ZoneNum // index of zone for messaging
	);

	Real64
	CalcBeausoleilMorrisonMixedStableFloor(
		Real64 const DeltaTemp, // [C] temperature difference between surface and air
		Real64 const HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
		Real64 const SurfTemp, // [C] surface temperature
		Real64 const SupplyAirTemp, // [C] temperature of supply air into zone
		Real64 const AirChangeRate, // [ACH] [1/hour] supply air ACH for zone
		int const ZoneNum // index of zone for messaging
	);

	Real64
	CalcBeausoleilMorrisonMixedUnstableFloor(
		Real64 const DeltaTemp, // [C] temperature difference between surface and air
		Real64 const HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
		Real64 const SurfTemp, // [C] surface temperature
		Real64 const SupplyAirTemp, // [C] temperature of supply air into zone
		Real64 const AirChangeRate, // [ACH] [1/hour] supply air ACH for zone
		int const ZoneNum // index of zone for messaging
	);

	Real64
	CalcBeausoleilMorrisonMixedStableCeiling(
		Real64 const DeltaTemp, // [C] temperature difference between surface and air
		Real64 const HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
		Real64 const SurfTemp, // [C] surface temperature
		Real64 const SupplyAirTemp, // [C] temperature of supply air into zone
		Real64 const AirChangeRate, // [ACH] [1/hour] supply air ACH for zone
		int const ZoneNum // index of zone for messaging
	);

	Real64
	CalcBeausoleilMorrisonMixedUnstableCeiling(
		Real64 const DeltaTemp, // [C] temperature difference between surface and air
		Real64 const HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
		Real64 const SurfTemp, // [C] surface temperature
		Real64 const SupplyAirTemp, // [C] temperature of supply air into zone
		Real64 const AirChangeRate, // [ACH] [1/hour] supply air ACH for zone
		int const ZoneNum // index of zone for messaging
	);

	Real64
	CalcFohannoPolidoriVerticalWall(
		Real64 const DeltaTemp, // [C] temperature difference between surface and air
		Real64 const Height, // [m] characteristic size, height of zone
		Real64 const SurfTemp, // [C] surface temperature
		Real64 const QdotConv, // [W/m2] heat flux rate for rayleigh #
		int const SurfNum // for messages
	);

	Real64
	CalcKaradagChilledCeiling( Real64 const DeltaTemp ); // [C] temperature difference between surface and air

	Real64
	CalcGoldsteinNovoselacCeilingDiffuserWindow(
		Real64 const AirSystemFlowRate, // [m3/s] air system flow rate
		Real64 const ZoneExtPerimLength, // [m] length of zone perimeter with exterior walls
		Real64 const WindWallRatio, // [ ] fraction of window area to wall area for zone
		int const WindowLocationType, // index for location types
		int const ZoneNum // for messages
	);

	Real64
	CalcGoldsteinNovoselacCeilingDiffuserWall(
		Real64 const AirSystemFlowRate, // [m3/s] air system flow rate
		Real64 const ZoneExtPerimLength, // [m] length of zone perimeter with exterior walls
		int const WindowLocationType, // index for location types
		int const ZoneNum // for messages
	);

	Real64
	CalcGoldsteinNovoselacCeilingDiffuserFloor(
		Real64 const AirSystemFlowRate, // [m3/s] air system flow rate
		Real64 const ZoneExtPerimLength, // [m] length of zone perimeter with exterior walls
		int const ZoneNum // for messages
	);

	Real64
	CalcSparrowWindward(
		int const RoughnessIndex,
		Real64 const FacePerimeter,
		Real64 const FaceArea,
		Real64 const WindAtZ,
		int const SurfNum
	);

	Real64
	CalcSparrowLeeward(
		int const RoughnessIndex,
		Real64 const FacePerimeter,
		Real64 const FaceArea,
		Real64 const WindAtZ,
		int const SurfNum
	);

	Real64
	CalcMoWITTWindward(
		Real64 const DeltaTemp,
		Real64 const WindAtZ
	);

	Real64
	CalcMoWITTLeeward(
		Real64 const DeltaTemp,
		Real64 const WindAtZ
	);

	Real64
	CalcDOE2Windward(
		Real64 const SurfaceTemp,
		Real64 const AirTemp,
		Real64 const CosineTilt,
		Real64 const WindAtZ,
		int const RoughnessIndex
	);

	Real64
	CalcDOE2Leeward(
		Real64 const SurfaceTemp,
		Real64 const AirTemp,
		Real64 const CosineTilt,
		Real64 const WindAtZ,
		int const RoughnessIndex
	);

	Real64
	CalcNusseltJurges( Real64 const WindAtZ );

	Real64
	CalcMcAdams( Real64 const WindAtZ );

	Real64
	CalcMitchell(
		Real64 const WindAtZ,
		Real64 const LengthScale,
		int const SurfNum
	);

	Real64
	CalcBlockenWindward(
		Real64 const WindAt10m,
		Real64 const WindDir, // Wind direction measured clockwise from geographhic North
		Real64 const SurfAzimuth // or Facing, Direction the surface outward normal faces (degrees)
	);

	Real64
	CalcEmmelVertical(
		Real64 const WindAt10m,
		Real64 const WindDir, // Wind direction measured clockwise from geographhic North
		Real64 const SurfAzimuth, // or Facing, Direction the surface outward normal faces (degrees)
		int const SurfNum
	);

	Real64
	CalcEmmelRoof(
		Real64 const WindAt10m,
		Real64 const WindDir, // Wind direction measured clockwise from geographhic North
		Real64 const LongAxisOutwardAzimuth, // or Facing, Direction the surface outward normal faces (degrees)
		int const SurfNum
	);

	Real64
	CalcClearRoof(
		int const SurfNum,
		Real64 const SurfTemp,
		Real64 const AirTemp,
		Real64 const WindAtZ,
		Real64 const WindDirect, // Wind direction measured clockwise from geographhic North
		Real64 const RoofArea,
		Real64 const RoofPerimeter
	);

} // ConvectionCoefficients

} // EnergyPlus

#endif
