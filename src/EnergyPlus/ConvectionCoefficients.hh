// EnergyPlus, Copyright (c) 1996-2021, The Board of Trustees of the University of Illinois,
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
// National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other
// contributors. All rights reserved.
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
//     similar designation, without the U.S. Department of Energy's prior written consent.
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

#ifndef ConvectionCoefficients_hh_INCLUDED
#define ConvectionCoefficients_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus/ConvectionConstants.hh>
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataVectorTypes.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace ConvectionCoefficients {

    // Using/Aliasing
    using DataVectorTypes::Vector;

    struct HcInsideFaceUserCurveStruct
    {
        // Members
        std::string Name; // user's name for object
        ConvectionConstants::RefTemp ReferenceTempType;
        int HcFnTempDiffCurveNum;
        int HcFnTempDiffDivHeightCurveNum;
        int HcFnACHCurveNum;
        int HcFnACHDivPerimLengthCurveNum;

        // Default Constructor
        HcInsideFaceUserCurveStruct()
            : ReferenceTempType(ConvectionConstants::RefTemp::Invalid), HcFnTempDiffCurveNum(0), HcFnTempDiffDivHeightCurveNum(0), HcFnACHCurveNum(0),
              HcFnACHDivPerimLengthCurveNum(0)
        {
        }
    };

    struct HcOutsideFaceUserCurveStruct
    {
        // Members
        std::string Name;
        int ReferenceTempType;
        bool SuppressRainChange;
        ConvectionConstants::RefWind WindSpeedType;
        int HfFnWindSpeedCurveNum;
        int HnFnTempDiffCurveNum;
        int HnFnTempDiffDivHeightCurveNum;

        // Default Constructor
        HcOutsideFaceUserCurveStruct()
            : ReferenceTempType(0), SuppressRainChange(false), WindSpeedType(ConvectionConstants::RefWind::Invalid), HfFnWindSpeedCurveNum(0),
              HnFnTempDiffCurveNum(0), HnFnTempDiffDivHeightCurveNum(0)
        {
        }
    };

    struct InsideFaceAdaptiveConvAlgoStruct
    {
        // Members
        std::string Name;
        int SimpleBuoyVertWallEqNum; // InConvClass_A3_VertWalls
        int SimpleBuoyVertWallUserCurveNum;
        int SimpleBuoyStableHorizEqNum; // InConvClass_A3_StableHoriz
        int SimpleBuoyStableHorizUserCurveNum;
        int SimpleBuoyUnstableHorizEqNum; // InConvClass_A3_UnstableHoriz
        int SimpleBuoyUnstableHorizUserCurveNum;
        int SimpleBuoyStableTiltedEqNum; // InConvClass_A3_StableTilted
        int SimpleBuoyStableTiltedUserCurveNum;
        int SimpleBuoyUnstableTiltedEqNum; // InConvClass_A3_UnstableTilted
        int SimpleBuoyUnstableTiltedUserCurveNum;
        int SimpleBuoyWindowsEqNum; // InConvClass_A3_Windows
        int SimpleBuoyWindowsUserCurveNum;
        int FloorHeatCeilingCoolVertWallEqNum; // InConvClass_A1_VertWalls
        int FloorHeatCeilingCoolVertWallUserCurveNum;
        int FloorHeatCeilingCoolStableHorizEqNum; // InConvClass_A1_StableHoriz
        int FloorHeatCeilingCoolStableHorizUserCurveNum;
        int FloorHeatCeilingCoolUnstableHorizEqNum; // InConvClass_A1_UnstableHoriz
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
        int MixedBuoyAssistingFlowWallEqNum;
        int MixedBuoyAssistingFlowWallUserCurveNum;
        int MixedBuoyOpposingFlowWallEqNum;
        int MixedBuoyOpposingFlowWallUserCurveNum;
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
        InsideFaceAdaptiveConvAlgoStruct()
            : SimpleBuoyVertWallEqNum(ConvectionConstants::HcInt_FohannoPolidoriVerticalWall), SimpleBuoyVertWallUserCurveNum(0),
              SimpleBuoyStableHorizEqNum(ConvectionConstants::HcInt_AlamdariHammondStableHorizontal), SimpleBuoyStableHorizUserCurveNum(0),
              SimpleBuoyUnstableHorizEqNum(ConvectionConstants::HcInt_AlamdariHammondUnstableHorizontal), SimpleBuoyUnstableHorizUserCurveNum(0),
              SimpleBuoyStableTiltedEqNum(ConvectionConstants::HcInt_WaltonStableHorizontalOrTilt), SimpleBuoyStableTiltedUserCurveNum(0),
              SimpleBuoyUnstableTiltedEqNum(ConvectionConstants::HcInt_WaltonUnstableHorizontalOrTilt), SimpleBuoyUnstableTiltedUserCurveNum(0),
              SimpleBuoyWindowsEqNum(ConvectionConstants::HcInt_ISO15099Windows), SimpleBuoyWindowsUserCurveNum(0),
              FloorHeatCeilingCoolVertWallEqNum(ConvectionConstants::HcInt_KhalifaEq3WallAwayFromHeat), FloorHeatCeilingCoolVertWallUserCurveNum(0),
              FloorHeatCeilingCoolStableHorizEqNum(ConvectionConstants::HcInt_AlamdariHammondStableHorizontal),
              FloorHeatCeilingCoolStableHorizUserCurveNum(0),
              FloorHeatCeilingCoolUnstableHorizEqNum(ConvectionConstants::HcInt_KhalifaEq4CeilingAwayFromHeat),
              FloorHeatCeilingCoolUnstableHorizUserCurveNum(0),
              FloorHeatCeilingCoolHeatedFloorEqNum(ConvectionConstants::HcInt_AwbiHattonHeatedFloor), FloorHeatCeilingCoolHeatedFloorUserCurveNum(0),
              FloorHeatCeilingCoolChilledCeilingEqNum(ConvectionConstants::HcInt_KaradagChilledCeiling),
              FloorHeatCeilingCoolChilledCeilingUserCurveNum(0),
              FloorHeatCeilingCoolStableTiltedEqNum(ConvectionConstants::HcInt_WaltonStableHorizontalOrTilt),
              FloorHeatCeilingCoolStableTiltedUserCurveNum(0),
              FloorHeatCeilingCoolUnstableTiltedEqNum(ConvectionConstants::HcInt_WaltonUnstableHorizontalOrTilt),
              FloorHeatCeilingCoolUnstableTiltedUserCurveNum(0), FloorHeatCeilingCoolWindowsEqNum(ConvectionConstants::HcInt_ISO15099Windows),
              FloorHeatCeilingCoolWindowsUserCurveNum(0), WallPanelHeatVertWallEqNum(ConvectionConstants::HcInt_KhalifaEq6NonHeatedWalls),
              WallPanelHeatVertWallUserCurveNum(0), WallPanelHeatHeatedWallEqNum(ConvectionConstants::HcInt_AwbiHattonHeatedWall),
              WallPanelHeatHeatedWallUserCurveNum(0), WallPanelHeatStableHorizEqNum(ConvectionConstants::HcInt_AlamdariHammondStableHorizontal),
              WallPanelHeatStableHorizUserCurveNum(0), WallPanelHeatUnstableHorizEqNum(ConvectionConstants::HcInt_KhalifaEq7Ceiling),
              WallPanelHeatUnstableHorizUserCurveNum(0), WallPanelHeatStableTiltedEqNum(ConvectionConstants::HcInt_WaltonStableHorizontalOrTilt),
              WallPanelHeatStableTiltedUserCurveNum(0), WallPanelHeatUnstableTiltedEqNum(ConvectionConstants::HcInt_WaltonUnstableHorizontalOrTilt),
              WallPanelHeatUnstableTiltedUserCurveNum(0), WallPanelHeatWindowsEqNum(ConvectionConstants::HcInt_ISO15099Windows),
              WallPanelHeatWindowsUserCurveNum(0), ConvectiveHeatVertWallEqNum(ConvectionConstants::HcInt_FohannoPolidoriVerticalWall),
              ConvectiveHeatVertWallUserCurveNum(0), ConvectiveHeatVertWallNearHeaterEqNum(ConvectionConstants::HcInt_KhalifaEq5WallNearHeat),
              ConvectiveHeatVertWallNearHeaterUserCurveNum(0),
              ConvectiveHeatStableHorizEqNum(ConvectionConstants::HcInt_AlamdariHammondStableHorizontal), ConvectiveHeatStableHorizUserCurveNum(0),
              ConvectiveHeatUnstableHorizEqNum(ConvectionConstants::HcInt_KhalifaEq7Ceiling), ConvectiveHeatUnstableHorizUserCurveNum(0),
              ConvectiveHeatStableTiltedEqNum(ConvectionConstants::HcInt_WaltonStableHorizontalOrTilt), ConvectiveHeatStableTiltedUserCurveNum(0),
              ConvectiveHeatUnstableTiltedEqNum(ConvectionConstants::HcInt_WaltonUnstableHorizontalOrTilt),
              ConvectiveHeatUnstableTiltedUserCurveNum(0), ConvectiveHeatWindowsEqNum(ConvectionConstants::HcInt_ISO15099Windows),
              ConvectiveHeatWindowsUserCurveNum(0), CentralAirWallEqNum(ConvectionConstants::HcInt_GoldsteinNovoselacCeilingDiffuserWalls),
              CentralAirWallUserCurveNum(0), CentralAirCeilingEqNum(ConvectionConstants::HcInt_FisherPedersenCeilDiffuserCeiling),
              CentralAirCeilingUserCurveNum(0), CentralAirFloorEqNum(ConvectionConstants::HcInt_GoldsteinNovoselacCeilingDiffuserFloor),
              CentralAirFloorUserCurveNum(0), CentralAirWindowsEqNum(ConvectionConstants::HcInt_GoldsteinNovoselacCeilingDiffuserWindow),
              CentralAirWindowsUserCurveNum(0), ZoneFanCircVertWallEqNum(ConvectionConstants::HcInt_KhalifaEq3WallAwayFromHeat),
              ZoneFanCircVertWallUserCurveNum(0), ZoneFanCircStableHorizEqNum(ConvectionConstants::HcInt_AlamdariHammondStableHorizontal),
              ZoneFanCircStableHorizUserCurveNum(0), ZoneFanCircUnstableHorizEqNum(ConvectionConstants::HcInt_KhalifaEq4CeilingAwayFromHeat),
              ZoneFanCircUnstableHorizUserCurveNum(0), ZoneFanCircStableTiltedEqNum(ConvectionConstants::HcInt_WaltonStableHorizontalOrTilt),
              ZoneFanCircStableTiltedUserCurveNum(0), ZoneFanCircUnstableTiltedEqNum(ConvectionConstants::HcInt_WaltonUnstableHorizontalOrTilt),
              ZoneFanCircUnstableTiltedUserCurveNum(0), ZoneFanCircWindowsEqNum(ConvectionConstants::HcInt_ISO15099Windows),
              ZoneFanCircWindowsUserCurveNum(0), MixedBuoyAssistingFlowWallEqNum(ConvectionConstants::HcInt_BeausoleilMorrisonMixedAssistingWall),
              MixedBuoyAssistingFlowWallUserCurveNum(0),
              MixedBuoyOpposingFlowWallEqNum(ConvectionConstants::HcInt_BeausoleilMorrisonMixedOppossingWall),
              MixedBuoyOpposingFlowWallUserCurveNum(0), MixedStableFloorEqNum(ConvectionConstants::HcInt_BeausoleilMorrisonMixedStableFloor),
              MixedStableFloorUserCurveNum(0), MixedUnstableFloorEqNum(ConvectionConstants::HcInt_BeausoleilMorrisonMixedUnstableFloor),
              MixedUnstableFloorUserCurveNum(0), MixedStableCeilingEqNum(ConvectionConstants::HcInt_BeausoleilMorrisonMixedStableCeiling),
              MixedStableCeilingUserCurveNum(0), MixedUnstableCeilingEqNum(ConvectionConstants::HcInt_BeausoleilMorrisonMixedUnstableCeiling),
              MixedUnstableCeilingUserCurveNum(0), MixedWindowsEqNum(ConvectionConstants::HcInt_GoldsteinNovoselacCeilingDiffuserWindow),
              MixedWindowsUserCurveNum(0)
        {
        }
    };

    struct OutsideFaceAdaptiveConvAlgoStruct
    {
        // Members
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
        int HNatUnstableHorizUserCurveNum;

        // Default Constructor
        OutsideFaceAdaptiveConvAlgoStruct()
            : SuppressRainChange(false), HWindWallWindwardEqNum(ConvectionConstants::HcExt_SparrowWindward), HWindWallWindwardUserCurveNum(0),
              HWindWallLeewardEqNum(ConvectionConstants::HcExt_SparrowLeeward), HWindWallLeewardUserCurveNum(0),
              HWindHorizRoofEqNum(ConvectionConstants::HcExt_ClearRoof), HWindHorizRoofUserCurveNum(0),
              HNatVertWallEqNum(ConvectionConstants::HcExt_NaturalASHRAEVerticalWall), HNatVertWallUserCurveNum(0),
              HNatStableHorizEqNum(ConvectionConstants::HcExt_NaturalWaltonStableHorizontalOrTilt), HNatStableHorizUserCurveNum(0),
              HNatUnstableHorizEqNum(ConvectionConstants::HcExt_NaturalWaltonUnstableHorizontalOrTilt), HNatUnstableHorizUserCurveNum(0)
        {
        }
    };

    struct BoundingBoxVertStruct
    {
        // Members
        int SurfNum;
        int VertNum;
        Vector Vertex;

        // Default Constructor
        BoundingBoxVertStruct() : SurfNum(0), VertNum(0), Vertex(0.0, 0.0, 0.0)
        {
        }
    };

    struct RoofGeoCharacteristicsStruct
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
        Array1D<Vector> BoundSurf;    // long edge of roof group bounding surface
        Real64 Area;
        Real64 Perimeter;
        Real64 Height;

        // Default Constructor
        RoofGeoCharacteristicsStruct() : BoundSurf(4), Area(0.0), Perimeter(0.0), Height(0.0)
        {
        }
    };

    struct FacadeGeoCharacteristicsStruct
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
        FacadeGeoCharacteristicsStruct() = default;

        // Member Constructor
        FacadeGeoCharacteristicsStruct(Real64 const AzimuthRangeLow,
                                       Real64 const AzimuthRangeHi,
                                       Real64 const Zmax,
                                       Real64 const Zmin,
                                       Real64 const Ymax,
                                       Real64 const Ymin,
                                       Real64 const Xmax,
                                       Real64 const Xmin,
                                       Real64 const Area,
                                       Real64 const Perimeter,
                                       Real64 const Height)
            : AzimuthRangeLow(AzimuthRangeLow), AzimuthRangeHi(AzimuthRangeHi), Zmax(Zmax), Zmin(Zmin), Ymax(Ymax), Ymin(Ymin), Xmax(Xmax),
              Xmin(Xmin), Area(Area), Perimeter(Perimeter), Height(Height)
        {
        }
    };

    // Functions

    void InitInteriorConvectionCoeffs(EnergyPlusData &state,
                                      const Array1D<Real64> &SurfaceTemperatures, // Temperature of surfaces for evaluation of HcIn
                                      Optional_int_const ZoneToResimulate = _     // if passed in, then only calculate surfaces that have this zone
    );

    void InitExteriorConvectionCoeff(EnergyPlusData &state,
                                     int SurfNum,                              // Surface number (in Surface derived type)
                                     Real64 HMovInsul,                         // Equivalent convection coefficient of movable insulation
                                     DataSurfaces::SurfaceRoughness Roughness, // Roughness index (1-6), see DataHeatBalance parameters
                                     Real64 AbsExt,                            // Exterior thermal absorptance
                                     Real64 TempExt,                           // Exterior surface temperature (C)
                                     Real64 &HExt,                             // Convection coefficient to exterior air
                                     Real64 &HSky,                             // "Convection" coefficient to sky temperature
                                     Real64 &HGround,                          // "Convection" coefficient to ground temperature
                                     Real64 &HAir                              // Radiation to Air Component
    );

    Real64 CalcHfExteriorSparrow(Real64 SurfWindSpeed,                     // Local wind speed at height of the heat transfer surface (m/s)
                                 Real64 GrossArea,                         // Gross surface area {m2}
                                 Real64 Perimeter,                         // Surface perimeter length {m}
                                 Real64 CosTilt,                           // Cosine of the Surface Tilt Angle
                                 Real64 Azimuth,                           // Facing angle (degrees) of the surface outward normal
                                 DataSurfaces::SurfaceRoughness Roughness, // Surface roughness index
                                 Real64 WindDirection                      // Wind (compass) direction (degrees)
    );

    bool Windward(Real64 CosTilt,      // Cosine of the surface tilt angle
                  Real64 Azimuth,      // or Facing, Direction the surface outward normal faces (degrees)
                  Real64 WindDirection // Wind direction measured clockwise from geographhic North
    );

    void GetUserConvectionCoefficients(EnergyPlusData &state);

    void ApplyConvectionValue(EnergyPlusData &state, std::string const &SurfaceTypes, std::string const &ConvectionType, int Value);

    Real64 CalcASHRAESimpExtConvectCoeff(DataSurfaces::SurfaceRoughness Roughness, // Integer index for roughness, relates to parameter array indices
                                         Real64 SurfWindSpeed                      // Current wind speed, m/s
    );

    Real64 CalcASHRAESimpleIntConvCoeff(Real64 Tsurf, Real64 Tamb, Real64 cosTilt);

    void CalcASHRAESimpleIntConvCoeff(EnergyPlusData &state,
                                      int SurfNum,                  // surface number for which coefficients are being calculated
                                      Real64 SurfaceTemperature,    // Temperature of surface for evaluation of HcIn
                                      Real64 ZoneMeanAirTemperature // Mean Air Temperature of Zone
    );

    Real64 CalcASHRAETARPNatural(Real64 Tsurf, Real64 Tamb, Real64 cosTilt);

    void CalcASHRAEDetailedIntConvCoeff(EnergyPlusData &state,
                                        int SurfNum,                  // surface number for which coefficients are being calculated
                                        Real64 SurfaceTemperature,    // Temperature of surface for evaluation of HcIn
                                        Real64 ZoneMeanAirTemperature // Mean Air Temperature of Zone
    );

    void CalcDetailedHcInForDVModel(EnergyPlusData &state,
                                    int SurfNum,                                // surface number for which coefficients are being calculated
                                    const Array1D<Real64> &SurfaceTemperatures, // Temperature of surfaces for evaluation of HcIn
                                    Array1D<Real64> &HcIn,                      // Interior Convection Coeff Array
                                    Optional<Array1S<Real64> const> Vhc = _     // Velocity array for forced convection coeff calculation
    );

    Real64 CalcZoneSupplyAirTemp(EnergyPlusData &state, int ZoneNum);

    Real64 CalcZoneSystemVolFlowRate(EnergyPlusData &state, int ZoneNum);

    Real64 CalcZoneSystemACH(EnergyPlusData &state, int ZoneNum);

    Real64 CalcCeilingDiffuserACH(EnergyPlusData &state, int ZoneNum);

    Real64 CalcCeilingDiffuserIntConvCoeff(EnergyPlusData &state,
                                           Real64 ACH, // [1/hr] air system air change rate
                                           Real64 Tsurf,
                                           Real64 Tair,
                                           Real64 cosTilt,
                                           Real64 humRat,
                                           Real64 height,
                                           bool isWindow = false);

    void CalcCeilingDiffuserIntConvCoeff(EnergyPlusData &state,
                                         int ZoneNum,
                                         const Array1D<Real64> &SurfaceTemperatures); // zone number for which coefficients are being calculated

    // CalcCeilingDiffuserInletCorr should replace CalcCeilingDiffuser (above), if ZoneTempPredictorCorrector can
    // ever be made to work correctly with the inlet air temperature.

    void CalcCeilingDiffuserInletCorr(EnergyPlusData &state,
                                      int ZoneNum,                         // Zone number
                                      Array1D<Real64> &SurfaceTemperatures // For CalcASHRAEDetailed, if called
    );

    void CalcTrombeWallIntConvCoeff(EnergyPlusData &state,
                                    int ZoneNum,                               // Zone number for which coefficients are being calculated
                                    const Array1D<Real64> &SurfaceTemperatures // Temperature of surfaces for evaluation of HcIn
    );

    void CalcNusselt(EnergyPlusData &state,
                     int SurfNum, // Surface number
                     Real64 asp,  // Aspect ratio: window height to gap width
                     Real64 tso,  // Temperature of gap surface closest to outside (K)
                     Real64 tsi,  // Temperature of gap surface closest to zone (K)
                     Real64 gr,   // Gap gas Grashof number
                     Real64 pr,   // Gap gas Prandtl number
                     Real64 &gnu  // Gap gas Nusselt number
    );

    Real64 SetExtConvectionCoeff(EnergyPlusData &state, int SurfNum); // Surface Number

    Real64 SetIntConvectionCoeff(EnergyPlusData &state, int SurfNum); // Surface Number

    Real64 CalcISO15099WindowIntConvCoeff(EnergyPlusData &state,
                                          Real64 SurfaceTemperature, // Temperature of surface for evaluation of HcIn
                                          Real64 AirTemperature,     // Mean Air Temperature of Zone (or adjacent air temperature)
                                          Real64 AirHumRat,          // air humidity ratio
                                          Real64 Height,             // window cavity height [m]
                                          Real64 TiltDeg,            // glazing tilt in degrees
                                          Real64 sineTilt            // sine of glazing tilt
    );

    void CalcISO15099WindowIntConvCoeff(EnergyPlusData &state,
                                        int SurfNum,               // surface number for which coefficients are being calculated
                                        Real64 SurfaceTemperature, // Temperature of surface for evaluation of HcIn
                                        Real64 AirTemperature      // Mean Air Temperature of Zone (or adjacent air temperature)
    );

    void SetupAdaptiveConvectionStaticMetaData(EnergyPlusData &state);

    void SetupAdaptiveConvectionRadiantSurfaceData(EnergyPlusData &state);

    void ManageInsideAdaptiveConvectionAlgo(EnergyPlusData &state, int SurfNum); // surface number for which coefficients are being calculated

    void ManageOutsideAdaptiveConvectionAlgo(EnergyPlusData &state,
                                             int SurfNum, // surface number for which coefficients are being calculated
                                             Real64 &Hc   // result for Hc Outside face, becomes HExt.
    );

    void EvaluateIntHcModels(EnergyPlusData &state,
                             int SurfNum,
                             int ConvModelEquationNum,
                             Real64 &Hc // calculated Hc value
    );

    void EvaluateExtHcModels(EnergyPlusData &state, int SurfNum, int NaturalConvModelEqNum, int ForcedConvModelEqNum, Real64 &Hc);

    void DynamicExtConvSurfaceClassification(EnergyPlusData &state, int SurfNum); // surface number

    void MapExtConvClassificationToHcModels(EnergyPlusData &state, int SurfNum); // surface number

    void DynamicIntConvSurfaceClassification(EnergyPlusData &state, int SurfNum); // surface number

    void MapIntConvClassificationToHcModels(EnergyPlusData &state, int SurfNum); // surface pointer index

    void CalcUserDefinedInsideHcModel(EnergyPlusData &state, int SurfNum, int UserCurveNum, Real64 &Hc);

    void CalcUserDefinedOutsideHcModel(EnergyPlusData &state, int SurfNum, int UserCurveNum, Real64 &H);

    //** Begin catalog of Hc equation functions. **** !*************************************************

    inline Real64 CalcASHRAEVerticalWall(Real64 const DeltaTemp) // [C] temperature difference between surface and air
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   Aug 2010
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Calculate the model equation attributed to ASHRAE for vertical walls for natural convection

        // REFERENCES:
        // 2.  ASHRAE Handbook of Fundamentals 2001, p. 3.12, Table 5.

        return 1.31 * std::pow(std::abs(DeltaTemp), ConvectionConstants::OneThird);
    }

    inline Real64 CalcWaltonUnstableHorizontalOrTilt(Real64 const DeltaTemp, // [C] temperature difference between surface and air
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

        return 9.482 * std::pow(std::abs(DeltaTemp), ConvectionConstants::OneThird) / (7.238 - std::abs(CosineTilt));
    }

    inline Real64 CalcWaltonStableHorizontalOrTilt(Real64 const DeltaTemp, // [C] temperature difference between surface and air
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

        // REFERENCES:
        // 1.  Walton, G. N. 1983. Thermal Analysis Research Program (TARP) Reference Manual,
        //     NBSSIR 83-2655, National Bureau of Standards, "Surface Inside Heat Balances", pp 79-80.

        return 1.810 * std::pow(std::abs(DeltaTemp), ConvectionConstants::OneThird) / (1.382 + std::abs(CosineTilt));
    }

    Real64 CalcFisherPedersenCeilDiffuserFloor(EnergyPlusData &state,
                                               Real64 ACH, // [1/hr] air system air change rate
                                               Real64 Tsurf,
                                               Real64 Tair,
                                               Real64 cosTilt,
                                               Real64 humRat,
                                               Real64 height,
                                               bool isWindow = false);

    Real64 CalcFisherPedersenCeilDiffuserCeiling(EnergyPlusData &state,
                                                 Real64 ACH, // [1/hr] air system air change rate
                                                 Real64 Tsurf,
                                                 Real64 Tair,
                                                 Real64 cosTilt,
                                                 Real64 humRat,
                                                 Real64 height,
                                                 bool isWindow = false);

    Real64 CalcFisherPedersenCeilDiffuserWalls(EnergyPlusData &state,
                                               Real64 ACH, // [1/hr] air system air change rate
                                               Real64 Tsurf,
                                               Real64 Tair,
                                               Real64 cosTilt,
                                               Real64 humRat,
                                               Real64 height,
                                               bool isWindow = false);

    Real64 CalcFisherPedersenCeilDiffuserNatConv(
        EnergyPlusData &state, Real64 Hforced, Real64 ACH, Real64 Tsurf, Real64 Tair, Real64 cosTilt, Real64 humRat, Real64 height, bool isWindow);

    Real64 CalcAlamdariHammondUnstableHorizontal(Real64 DeltaTemp,        // [C] temperature difference between surface and air
                                                 Real64 HydraulicDiameter // [m] characteristic size, = (4 * area) / perimeter
    );

    Real64 CalcAlamdariHammondUnstableHorizontal(EnergyPlusData &state,
                                                 Real64 DeltaTemp,         // [C] temperature difference between surface and air
                                                 Real64 HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
                                                 int SurfNum               // for messages
    );

    Real64 CalcAlamdariHammondStableHorizontal(Real64 DeltaTemp,        // [C] temperature difference between surface and air
                                               Real64 HydraulicDiameter // [m] characteristic size, = (4 * area) / perimeter
    );

    Real64 CalcAlamdariHammondStableHorizontal(EnergyPlusData &state,
                                               Real64 DeltaTemp,         // [C] temperature difference between surface and air
                                               Real64 HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
                                               int SurfNum               // for messages
    );

    Real64 CalcAlamdariHammondVerticalWall(Real64 DeltaTemp, // [C] temperature difference between surface and air
                                           Real64 Height     // [m] characteristic size, = zone height
    );

    Real64 CalcAlamdariHammondVerticalWall(EnergyPlusData &state,
                                           Real64 DeltaTemp, // [C] temperature difference between surface and air
                                           Real64 Height,    // [m] characteristic size, = zone height
                                           int SurfNum       // for messages
    );

    Real64 CalcKhalifaEq3WallAwayFromHeat(Real64 DeltaTemp); // [C] temperature difference between surface and air

    Real64 CalcKhalifaEq4CeilingAwayFromHeat(Real64 DeltaTemp); // [C] temperature difference between surface and air

    Real64 CalcKhalifaEq5WallsNearHeat(Real64 DeltaTemp); // [C] temperature difference between surface and air

    Real64 CalcKhalifaEq6NonHeatedWalls(Real64 DeltaTemp); // [C] temperature difference between surface and air

    Real64 CalcKhalifaEq7Ceiling(Real64 DeltaTemp); // [C] temperature difference between surface and air

    Real64 CalcAwbiHattonHeatedFloor(Real64 DeltaTemp,        // [C] temperature difference between surface and air
                                     Real64 HydraulicDiameter // [m] characteristic size, = (4 * area) / perimeter
    );

    Real64 CalcAwbiHattonHeatedWall(Real64 DeltaTemp,        // [C] temperature difference between surface and air
                                    Real64 HydraulicDiameter // [m] characteristic size, = (4 * area) / perimeter
    );

    Real64 CalcBeausoleilMorrisonMixedAssistedWall(Real64 const &DeltaTemp,     // [C] temperature difference between surface and air
                                                   Real64 const &Height,        // [m] characteristic size
                                                   Real64 const &SurfTemp,      // [C] surface temperature
                                                   Real64 const &SupplyAirTemp, // [C] temperature of supply air into zone
                                                   Real64 const &AirChangeRate  // [ACH] [1/hour] supply air ACH for zone
    );

    Real64 CalcBeausoleilMorrisonMixedAssistedWall(EnergyPlusData &state,
                                                   Real64 const &DeltaTemp, // [C] temperature difference between surface and air
                                                   Real64 const &Height,    // [m] characteristic size
                                                   Real64 const &SurfTemp,  // [C] surface temperature
                                                   int ZoneNum              // index of zone for messaging
    );

    Real64 CalcBeausoleilMorrisonMixedOpposingWall(Real64 const &DeltaTemp,     // [C] temperature difference between surface and air
                                                   Real64 const &Height,        // [m] characteristic size
                                                   Real64 const &SurfTemp,      // [C] surface temperature
                                                   Real64 const &SupplyAirTemp, // [C] temperature of supply air into zone
                                                   Real64 const &AirChangeRate  // [ACH] [1/hour] supply air ACH for zone
    );

    Real64 CalcBeausoleilMorrisonMixedOpposingWall(EnergyPlusData &state,
                                                   Real64 const &DeltaTemp, // [C] temperature difference between surface and air
                                                   Real64 const &Height,    // [m] characteristic size
                                                   Real64 const &SurfTemp,  // [C] surface temperature
                                                   int ZoneNum              // index of zone for messaging
    );

    Real64 CalcBeausoleilMorrisonMixedStableFloor(Real64 const &DeltaTemp,         // [C] temperature difference between surface and air
                                                  Real64 const &HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
                                                  Real64 const &SurfTemp,          // [C] surface temperature
                                                  Real64 const &SupplyAirTemp,     // [C] temperature of supply air into zone
                                                  Real64 const &AirChangeRate      // [ACH] [1/hour] supply air ACH for zone
    );

    Real64 CalcBeausoleilMorrisonMixedStableFloor(EnergyPlusData &state,
                                                  Real64 const &DeltaTemp,         // [C] temperature difference between surface and air
                                                  Real64 const &HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
                                                  Real64 const &SurfTemp,          // [C] surface temperature
                                                  int ZoneNum                      // index of zone for messaging
    );

    Real64 CalcBeausoleilMorrisonMixedUnstableFloor(Real64 const &DeltaTemp,         // [C] temperature difference between surface and air
                                                    Real64 const &HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
                                                    Real64 const &SurfTemp,          // [C] surface temperature
                                                    Real64 const &SupplyAirTemp,     // [C] temperature of supply air into zone
                                                    Real64 const &AirChangeRate      // [ACH] [1/hour] supply air ACH for zone
    );

    Real64 CalcBeausoleilMorrisonMixedUnstableFloor(EnergyPlusData &state,
                                                    Real64 const &DeltaTemp,         // [C] temperature difference between surface and air
                                                    Real64 const &HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
                                                    Real64 const &SurfTemp,          // [C] surface temperature
                                                    int ZoneNum                      // index of zone for messaging
    );

    Real64 CalcBeausoleilMorrisonMixedStableCeiling(Real64 const &DeltaTemp,         // [C] temperature difference between surface and air
                                                    Real64 const &HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
                                                    Real64 const &SurfTemp,          // [C] surface temperature
                                                    Real64 const &SupplyAirTemp,     // [C] temperature of supply air into zone
                                                    Real64 const &AirChangeRate      // [ACH] [1/hour] supply air ACH for zone
    );

    Real64 CalcBeausoleilMorrisonMixedStableCeiling(EnergyPlusData &state,
                                                    Real64 const &DeltaTemp,         // [C] temperature difference between surface and air
                                                    Real64 const &HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
                                                    Real64 const &SurfTemp,          // [C] surface temperature
                                                    int ZoneNum                      // index of zone for messaging
    );

    Real64 CalcBeausoleilMorrisonMixedUnstableCeiling(Real64 const &DeltaTemp,         // [C] temperature difference between surface and air
                                                      Real64 const &HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
                                                      Real64 const &SurfTemp,          // [C] surface temperature
                                                      Real64 const &SupplyAirTemp,     // [C] temperature of supply air into zone
                                                      Real64 const &AirChangeRate      // [ACH] [1/hour] supply air ACH for zone
    );

    Real64 CalcBeausoleilMorrisonMixedUnstableCeiling(EnergyPlusData &state,
                                                      Real64 const &DeltaTemp,         // [C] temperature difference between surface and air
                                                      Real64 const &HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
                                                      Real64 const &SurfTemp,          // [C] surface temperature
                                                      int ZoneNum                      // index of zone for messaging
    );

    Real64 CalcFohannoPolidoriVerticalWall(Real64 DeltaTemp, // [C] temperature difference between surface and air
                                           Real64 Height,    // [m] characteristic size, height of zone
                                           Real64 SurfTemp,  // [C] surface temperature
                                           Real64 QdotConv   // [W/m2] heat flux rate for rayleigh #
    );

    Real64 CallCalcFohannoPolidoriVerticalWall(EnergyPlusData &state,
                                               Real64 DeltaTemp, // [C] temperature difference between surface and air
                                               Real64 Height,    // [m] characteristic size, height of zone
                                               Real64 SurfTemp,  // [C] surface temperature
                                               Real64 QdotConv,  // [W/m2] heat flux rate for rayleigh #
                                               int SurfNum       // for messages
    );

    Real64 CalcKaradagChilledCeiling(Real64 DeltaTemp); // [C] temperature difference between surface and air

    Real64 CalcGoldsteinNovoselacCeilingDiffuserWindow(Real64 AirSystemFlowRate,  // [m3/s] air system flow rate
                                                       Real64 ZoneExtPerimLength, // [m] length of zone perimeter with exterior walls
                                                       Real64 WindWallRatio,      // [ ] fraction of window area to wall area for zone
                                                       ConvectionConstants::InConvWinLoc WindowLocationType // index for location types
    );

    Real64 CalcGoldsteinNovoselacCeilingDiffuserWindow(EnergyPlusData &state,
                                                       Real64 ZoneExtPerimLength, // [m] length of zone perimeter with exterior walls
                                                       Real64 WindWallRatio,      // [ ] fraction of window area to wall area for zone
                                                       ConvectionConstants::InConvWinLoc WindowLocationType, // index for location types
                                                       int ZoneNum                                           // for messages
    );

    Real64 CalcGoldsteinNovoselacCeilingDiffuserWall(Real64 AirSystemFlowRate,  // [m3/s] air system flow rate
                                                     Real64 ZoneExtPerimLength, // [m] length of zone perimeter with exterior walls
                                                     ConvectionConstants::InConvWinLoc WindowLocationType // index for location types
    );

    Real64 CalcGoldsteinNovoselacCeilingDiffuserWall(EnergyPlusData &state,
                                                     Real64 ZoneExtPerimLength, // [m] length of zone perimeter with exterior walls
                                                     ConvectionConstants::InConvWinLoc WindowLocationType, // index for location types
                                                     int ZoneNum                                           // for messages
    );

    Real64 CalcGoldsteinNovoselacCeilingDiffuserFloor(Real64 AirSystemFlowRate, // [m3/s] air system flow rate
                                                      Real64 ZoneExtPerimLength // [m] length of zone perimeter with exterior walls
    );

    Real64 CalcGoldsteinNovoselacCeilingDiffuserFloor(EnergyPlusData &state,
                                                      Real64 ZoneExtPerimLength, // [m] length of zone perimeter with exterior walls
                                                      int ZoneNum                // for messages
    );

    Real64 CalcSparrowWindward(DataSurfaces::SurfaceRoughness RoughnessIndex, Real64 FacePerimeter, Real64 FaceArea, Real64 WindAtZ);

    Real64 CalcSparrowWindward(
        EnergyPlusData &state, DataSurfaces::SurfaceRoughness RoughnessIndex, Real64 FacePerimeter, Real64 FaceArea, Real64 WindAtZ, int SurfNum);

    Real64 CalcSparrowLeeward(DataSurfaces::SurfaceRoughness RoughnessIndex, Real64 FacePerimeter, Real64 FaceArea, Real64 WindAtZ);

    Real64 CalcSparrowLeeward(
        EnergyPlusData &state, DataSurfaces::SurfaceRoughness RoughnessIndex, Real64 FacePerimeter, Real64 FaceArea, Real64 WindAtZ, int SurfNum);

    Real64 CalcMoWITTNatural(Real64 DeltaTemp);

    Real64 CalcMoWITTForcedWindward(Real64 WindAtZ);

    Real64 CalcMoWITTForcedLeeward(Real64 WindAtZ);

    Real64 CalcMoWITTWindward(Real64 DeltaTemp, Real64 WindAtZ);

    Real64 CalcMoWITTLeeward(Real64 DeltaTemp, Real64 WindAtZ);

    Real64 CalcDOE2Forced(Real64 SurfaceTemp, Real64 AirTemp, Real64 CosineTilt, Real64 HfSmooth, DataSurfaces::SurfaceRoughness RoughnessIndex);

    Real64 CalcDOE2Windward(Real64 SurfaceTemp, Real64 AirTemp, Real64 CosineTilt, Real64 WindAtZ, DataSurfaces::SurfaceRoughness RoughnessIndex);

    Real64 CalcDOE2Leeward(Real64 SurfaceTemp, Real64 AirTemp, Real64 CosineTilt, Real64 WindAtZ, DataSurfaces::SurfaceRoughness RoughnessIndex);

    Real64 CalcNusseltJurges(Real64 WindAtZ);

    Real64 CalcMcAdams(Real64 WindAtZ);

    Real64 CalcMitchell(Real64 WindAtZ, Real64 LengthScale);

    Real64 CalcMitchell(EnergyPlusData &state, Real64 WindAtZ, Real64 LengthScale, int SurfNum);

    Real64 CalcBlockenWindward(Real64 WindAt10m,
                               Real64 WindDir,    // Wind direction measured clockwise from geographic North
                               Real64 SurfAzimuth // or Facing, Direction the surface outward normal faces (degrees)
    );

    Real64 CalcEmmelVertical(EnergyPlusData &state,
                             Real64 WindAt10m,
                             Real64 WindDir,     // Wind direction measured clockwise from geographic North
                             Real64 SurfAzimuth, // or Facing, Direction the surface outward normal faces (degrees)
                             int SurfNum);

    Real64 CalcEmmelRoof(EnergyPlusData &state,
                         Real64 WindAt10m,
                         Real64 WindDir,                // Wind direction measured clockwise from geographic North
                         Real64 LongAxisOutwardAzimuth, // or Facing, Direction the surface outward normal faces (degrees)
                         int SurfNum);

    Real64 CalcClearRoof(EnergyPlusData &state,
                         Real64 AirTemp,
                         Real64 WindAtZ,
                         Real64 WindDirect, // Wind direction measured clockwise from geographic North
                         Real64 RoofArea,
                         Real64 RoofPerimeter,
                         DataSurfaces::SurfaceRoughness RoughnessIndex);

    Real64 CalcClearRoof(EnergyPlusData &state,
                         int SurfNum,
                         Real64 SurfTemp,
                         Real64 AirTemp,
                         Real64 WindAtZ,
                         Real64 WindDirect, // Wind direction measured clockwise from geographic North
                         Real64 RoofArea,
                         Real64 RoofPerimeter);

    void CalcASTMC1340ConvCoeff(EnergyPlusData &state,
                                int const SurfNum,                  // surface number for which coefficients are being calculated
                                Real64 const SurfaceTemperature,    // Temperature of surface for evaluation of HcIn
                                Real64 const ZoneMeanAirTemperature // Mean Air Temperature of Zone
    );

    Real64
    CalcASTMC1340ConvCoeff(EnergyPlusData &state, int const SurfNum, Real64 const Tsurf, Real64 const Tair, Real64 const Vair, Real64 const Tilt);

    ConvectionConstants::SurfConvOrientation GetSurfConvOrientation(Real64 const Tilt);

} // namespace ConvectionCoefficients

struct ConvectionCoefficientsData : BaseGlobalStruct
{

    bool GetUserSuppliedConvectionCoeffs = true;  // Get user input first call for Init
    Real64 CubeRootOfOverallBuildingVolume = 0.0; // building meta data. cube root of the volume of all the zones
    Real64 RoofLongAxisOutwardAzimuth = 0.0;      // roof surfaces meta data. outward normal azimuth for longest roof edge

    int BMMixedAssistedWallErrorIDX1 = 0;
    int BMMixedAssistedWallErrorIDX2 = 0;
    int BMMixedOpposingWallErrorIDX1 = 0;
    int BMMixedOpposingWallErrorIDX2 = 0;
    int BMMixedStableFloorErrorIDX1 = 0;
    int BMMixedStableFloorErrorIDX2 = 0;
    int BMMixedUnstableFloorErrorIDX1 = 0;
    int BMMixedUnstableFloorErrorIDX2 = 0;
    int BMMixedStableCeilingErrorIDX1 = 0;
    int BMMixedStableCeilingErrorIDX2 = 0;
    int BMMixedUnstableCeilingErrorIDX1 = 0;
    int BMMixedUnstableCeilingErrorIDX2 = 0;
    int AHUnstableHorizontalErrorIDX = 0;
    int AHStableHorizontalErrorIDX = 0;
    int AHVerticalWallErrorIDX = 0;
    int CalcFohannoPolidoriVerticalWallErrorIDX = 0;
    int CalcGoldsteinNovoselacCeilingDiffuserWindowErrorIDX1 = 0;
    int CalcGoldsteinNovoselacCeilingDiffuserWindowErrorIDX2 = 0;
    int CalcGoldsteinNovoselacCeilingDiffuserWallErrorIDX1 = 0;
    int CalcGoldsteinNovoselacCeilingDiffuserWallErrorIDX2 = 0;
    int CalcGoldsteinNovoselacCeilingDiffuserFloorErrorIDX = 0;
    int CalcSparrowWindwardErrorIDX = 0;
    int CalcSparrowLeewardErrorIDX = 0;
    int CalcEmmelVerticalErrorIDX = 0;
    int CalcEmmelRoofErrorIDX = 0;
    int CalcClearRoofErrorIDX = 0;
    int CalcMitchellErrorIDX = 0;

    // move random statics so they can be reset for unit tests
    bool NodeCheck = true;
    bool ActiveSurfaceCheck = true;
    bool MyEnvirnFlag = true;
    bool FirstRoofSurf = true;
    int ActiveWallCount = 0;
    Real64 ActiveWallArea = 0.0;
    int ActiveCeilingCount = 0;
    Real64 ActiveCeilingArea = 0.0;
    int ActiveFloorCount = 0;
    Real64 ActiveFloorArea = 0.0;

    // Object Data
    ConvectionCoefficients::InsideFaceAdaptiveConvAlgoStruct InsideFaceAdaptiveConvectionAlgo; // stores rules for Hc model equations
    ConvectionCoefficients::OutsideFaceAdaptiveConvAlgoStruct OutsideFaceAdaptiveConvectionAlgo;
    Array1D<ConvectionCoefficients::HcInsideFaceUserCurveStruct> HcInsideUserCurve;
    Array1D<ConvectionCoefficients::HcOutsideFaceUserCurveStruct> HcOutsideUserCurve;
    ConvectionCoefficients::RoofGeoCharacteristicsStruct RoofGeo;

    ConvectionCoefficients::FacadeGeoCharacteristicsStruct NorthFacade = {332.5, 22.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
    ConvectionCoefficients::FacadeGeoCharacteristicsStruct NorthEastFacade = {22.5, 67.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
    ConvectionCoefficients::FacadeGeoCharacteristicsStruct EastFacade = {67.5, 112.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
    ConvectionCoefficients::FacadeGeoCharacteristicsStruct SouthEastFacade = {112.5, 157.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
    ConvectionCoefficients::FacadeGeoCharacteristicsStruct SouthFacade = {157.5, 202.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
    ConvectionCoefficients::FacadeGeoCharacteristicsStruct SouthWestFacade = {202.5, 247.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
    ConvectionCoefficients::FacadeGeoCharacteristicsStruct WestFacade = {247.5, 287.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
    ConvectionCoefficients::FacadeGeoCharacteristicsStruct NorthWestFacade = {287.5, 332.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};

    void clear_state() override
    {
        this->GetUserSuppliedConvectionCoeffs = true;
        this->CubeRootOfOverallBuildingVolume = 0.0;
        this->RoofLongAxisOutwardAzimuth = 0.0;

        // error indices
        this->BMMixedAssistedWallErrorIDX1 = 0;
        this->BMMixedAssistedWallErrorIDX2 = 0;
        this->BMMixedOpposingWallErrorIDX1 = 0;
        this->BMMixedOpposingWallErrorIDX2 = 0;
        this->BMMixedStableFloorErrorIDX1 = 0;
        this->BMMixedStableFloorErrorIDX2 = 0;
        this->BMMixedUnstableFloorErrorIDX1 = 0;
        this->BMMixedUnstableFloorErrorIDX2 = 0;
        this->BMMixedStableCeilingErrorIDX1 = 0;
        this->BMMixedStableCeilingErrorIDX2 = 0;
        this->BMMixedUnstableCeilingErrorIDX1 = 0;
        this->BMMixedUnstableCeilingErrorIDX2 = 0;
        this->AHUnstableHorizontalErrorIDX = 0;
        this->AHStableHorizontalErrorIDX = 0;
        this->AHVerticalWallErrorIDX = 0;
        this->CalcFohannoPolidoriVerticalWallErrorIDX = 0;
        this->CalcGoldsteinNovoselacCeilingDiffuserWindowErrorIDX1 = 0;
        this->CalcGoldsteinNovoselacCeilingDiffuserWindowErrorIDX2 = 0;
        this->CalcGoldsteinNovoselacCeilingDiffuserWallErrorIDX1 = 0;
        this->CalcGoldsteinNovoselacCeilingDiffuserWallErrorIDX2 = 0;
        this->CalcGoldsteinNovoselacCeilingDiffuserFloorErrorIDX = 0;
        this->CalcSparrowWindwardErrorIDX = 0;
        this->CalcSparrowLeewardErrorIDX = 0;
        this->CalcEmmelVerticalErrorIDX = 0;
        this->CalcEmmelRoofErrorIDX = 0;
        this->CalcClearRoofErrorIDX = 0;
        this->CalcMitchellErrorIDX = 0;

        // move random statics so they can be reset for unit tests
        this->NodeCheck = true;
        this->ActiveSurfaceCheck = true;
        this->MyEnvirnFlag = true;
        this->FirstRoofSurf = true;
        this->ActiveWallCount = 0;
        this->ActiveWallArea = 0.0;
        this->ActiveCeilingCount = 0;
        this->ActiveCeilingArea = 0.0;
        this->ActiveFloorCount = 0;
        this->ActiveFloorArea = 0.0;

        // Object Data
        this->InsideFaceAdaptiveConvectionAlgo = {}; // stores rules for Hc model equations
        this->OutsideFaceAdaptiveConvectionAlgo = {};
        this->HcInsideUserCurve.deallocate();
        this->HcOutsideUserCurve.deallocate();
        this->RoofGeo = {};

        this->NorthFacade = {332.5, 22.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
        this->NorthEastFacade = {22.5, 67.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
        this->EastFacade = {67.5, 112.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
        this->SouthEastFacade = {112.5, 157.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
        this->SouthFacade = {157.5, 202.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
        this->SouthWestFacade = {202.5, 247.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
        this->WestFacade = {247.5, 287.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
        this->NorthWestFacade = {287.5, 332.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
    }
};

} // namespace EnergyPlus

#endif
