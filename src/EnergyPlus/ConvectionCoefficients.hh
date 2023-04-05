// EnergyPlus, Copyright (c) 1996-2023, The Board of Trustees of the University of Illinois,
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
        ConvectionConstants::RefTemp ReferenceTempType = ConvectionConstants::RefTemp::Invalid;
        int HcFnTempDiffCurveNum = 0;
        int HcFnTempDiffDivHeightCurveNum = 0;
        int HcFnACHCurveNum = 0;
        int HcFnACHDivPerimLengthCurveNum = 0;
    };

    struct HcOutsideFaceUserCurveStruct
    {
        // Members
        std::string Name;
        int ReferenceTempType = 0;
        bool SuppressRainChange = false;
        ConvectionConstants::RefWind WindSpeedType = ConvectionConstants::RefWind::Invalid;
        int HfFnWindSpeedCurveNum = 0;
        int HnFnTempDiffCurveNum = 0;
        int HnFnTempDiffDivHeightCurveNum = 0;
    };

    struct InsideFaceAdaptiveConvAlgoStruct
    {
        // Members
        std::string Name;
        int SimpleBuoyVertWallEqNum = ConvectionConstants::HcInt_FohannoPolidoriVerticalWall; // InConvClass_A3_VertWalls
        int SimpleBuoyVertWallUserCurveNum = 0;
        int SimpleBuoyStableHorizEqNum = ConvectionConstants::HcInt_AlamdariHammondStableHorizontal; // InConvClass_A3_StableHoriz
        int SimpleBuoyStableHorizUserCurveNum = 0;
        int SimpleBuoyUnstableHorizEqNum = ConvectionConstants::HcInt_AlamdariHammondUnstableHorizontal; // InConvClass_A3_UnstableHoriz
        int SimpleBuoyUnstableHorizUserCurveNum = 0;
        int SimpleBuoyStableTiltedEqNum = ConvectionConstants::HcInt_WaltonStableHorizontalOrTilt; // InConvClass_A3_StableTilted
        int SimpleBuoyStableTiltedUserCurveNum = 0;
        int SimpleBuoyUnstableTiltedEqNum = ConvectionConstants::HcInt_WaltonUnstableHorizontalOrTilt; // InConvClass_A3_UnstableTilted
        int SimpleBuoyUnstableTiltedUserCurveNum = 0;
        int SimpleBuoyWindowsEqNum = ConvectionConstants::HcInt_ISO15099Windows; // InConvClass_A3_Windows
        int SimpleBuoyWindowsUserCurveNum = 0;
        int FloorHeatCeilingCoolVertWallEqNum = ConvectionConstants::HcInt_KhalifaEq3WallAwayFromHeat; // InConvClass_A1_VertWalls
        int FloorHeatCeilingCoolVertWallUserCurveNum = 0;
        int FloorHeatCeilingCoolStableHorizEqNum = ConvectionConstants::HcInt_AlamdariHammondStableHorizontal; // InConvClass_A1_StableHoriz
        int FloorHeatCeilingCoolStableHorizUserCurveNum = 0;
        int FloorHeatCeilingCoolUnstableHorizEqNum = ConvectionConstants::HcInt_KhalifaEq4CeilingAwayFromHeat; // InConvClass_A1_UnstableHoriz
        int FloorHeatCeilingCoolUnstableHorizUserCurveNum = 0;
        int FloorHeatCeilingCoolHeatedFloorEqNum = ConvectionConstants::HcInt_AwbiHattonHeatedFloor; // InConvClass_A1_HeatedFloor
        int FloorHeatCeilingCoolHeatedFloorUserCurveNum = 0;
        int FloorHeatCeilingCoolChilledCeilingEqNum = ConvectionConstants::HcInt_KaradagChilledCeiling; // InConvClass_A1_ChilledCeil
        int FloorHeatCeilingCoolChilledCeilingUserCurveNum = 0;
        int FloorHeatCeilingCoolStableTiltedEqNum = ConvectionConstants::HcInt_WaltonStableHorizontalOrTilt; // InConvClass_A1_StableTilted
        int FloorHeatCeilingCoolStableTiltedUserCurveNum = 0;
        int FloorHeatCeilingCoolUnstableTiltedEqNum = ConvectionConstants::HcInt_WaltonUnstableHorizontalOrTilt; // InConvClass_A1_UnstableTilted
        int FloorHeatCeilingCoolUnstableTiltedUserCurveNum = 0;
        int FloorHeatCeilingCoolWindowsEqNum = ConvectionConstants::HcInt_ISO15099Windows; // InConvClass_A1_Windows
        int FloorHeatCeilingCoolWindowsUserCurveNum = 0;
        int WallPanelHeatVertWallEqNum = ConvectionConstants::HcInt_KhalifaEq6NonHeatedWalls; // InConvClass_A2_VertWallsNonHeated
        int WallPanelHeatVertWallUserCurveNum = 0;
        int WallPanelHeatHeatedWallEqNum = ConvectionConstants::HcInt_AwbiHattonHeatedWall; // InConvClass_A2_HeatedVerticalWall
        int WallPanelHeatHeatedWallUserCurveNum = 0;
        int WallPanelHeatStableHorizEqNum = ConvectionConstants::HcInt_AlamdariHammondStableHorizontal; // InConvClass_A2_StableHoriz
        int WallPanelHeatStableHorizUserCurveNum = 0;
        int WallPanelHeatUnstableHorizEqNum = ConvectionConstants::HcInt_KhalifaEq7Ceiling; // InConvClass_A2_UnstableHoriz
        int WallPanelHeatUnstableHorizUserCurveNum = 0;
        int WallPanelHeatStableTiltedEqNum = ConvectionConstants::HcInt_WaltonStableHorizontalOrTilt; // InConvClass_A2_StableTilted
        int WallPanelHeatStableTiltedUserCurveNum = 0;
        int WallPanelHeatUnstableTiltedEqNum = ConvectionConstants::HcInt_WaltonUnstableHorizontalOrTilt; // InConvClass_A2_UnstableTilted
        int WallPanelHeatUnstableTiltedUserCurveNum = 0;
        int WallPanelHeatWindowsEqNum = ConvectionConstants::HcInt_ISO15099Windows; // InConvClass_A2_Windows
        int WallPanelHeatWindowsUserCurveNum = 0;
        int ConvectiveHeatVertWallEqNum = ConvectionConstants::HcInt_FohannoPolidoriVerticalWall;
        int ConvectiveHeatVertWallUserCurveNum = 0;
        int ConvectiveHeatVertWallNearHeaterEqNum = ConvectionConstants::HcInt_KhalifaEq5WallNearHeat;
        int ConvectiveHeatVertWallNearHeaterUserCurveNum = 0;
        int ConvectiveHeatStableHorizEqNum = ConvectionConstants::HcInt_AlamdariHammondStableHorizontal;
        int ConvectiveHeatStableHorizUserCurveNum = 0;
        int ConvectiveHeatUnstableHorizEqNum = ConvectionConstants::HcInt_KhalifaEq7Ceiling;
        int ConvectiveHeatUnstableHorizUserCurveNum = 0;
        int ConvectiveHeatStableTiltedEqNum = ConvectionConstants::HcInt_WaltonStableHorizontalOrTilt;
        int ConvectiveHeatStableTiltedUserCurveNum = 0;
        int ConvectiveHeatUnstableTiltedEqNum = ConvectionConstants::HcInt_WaltonUnstableHorizontalOrTilt;
        int ConvectiveHeatUnstableTiltedUserCurveNum = 0;
        int ConvectiveHeatWindowsEqNum = ConvectionConstants::HcInt_ISO15099Windows;
        int ConvectiveHeatWindowsUserCurveNum = 0;
        int CentralAirWallEqNum = ConvectionConstants::HcInt_GoldsteinNovoselacCeilingDiffuserWalls;
        int CentralAirWallUserCurveNum = 0;
        int CentralAirCeilingEqNum = ConvectionConstants::HcInt_FisherPedersenCeilDiffuserCeiling;
        int CentralAirCeilingUserCurveNum = 0;
        int CentralAirFloorEqNum = ConvectionConstants::HcInt_GoldsteinNovoselacCeilingDiffuserFloor;
        int CentralAirFloorUserCurveNum = 0;
        int CentralAirWindowsEqNum = ConvectionConstants::HcInt_GoldsteinNovoselacCeilingDiffuserWindow;
        int CentralAirWindowsUserCurveNum = 0;
        int ZoneFanCircVertWallEqNum = ConvectionConstants::HcInt_KhalifaEq3WallAwayFromHeat;
        int ZoneFanCircVertWallUserCurveNum = 0;
        int ZoneFanCircStableHorizEqNum = ConvectionConstants::HcInt_AlamdariHammondStableHorizontal;
        int ZoneFanCircStableHorizUserCurveNum = 0;
        int ZoneFanCircUnstableHorizEqNum = ConvectionConstants::HcInt_KhalifaEq4CeilingAwayFromHeat;
        int ZoneFanCircUnstableHorizUserCurveNum = 0;
        int ZoneFanCircStableTiltedEqNum = ConvectionConstants::HcInt_WaltonStableHorizontalOrTilt;
        int ZoneFanCircStableTiltedUserCurveNum = 0;
        int ZoneFanCircUnstableTiltedEqNum = ConvectionConstants::HcInt_WaltonUnstableHorizontalOrTilt;
        int ZoneFanCircUnstableTiltedUserCurveNum = 0;
        int ZoneFanCircWindowsEqNum = ConvectionConstants::HcInt_ISO15099Windows;
        int ZoneFanCircWindowsUserCurveNum = 0;
        int MixedBuoyAssistingFlowWallEqNum = ConvectionConstants::HcInt_BeausoleilMorrisonMixedAssistingWall;
        int MixedBuoyAssistingFlowWallUserCurveNum = 0;
        int MixedBuoyOpposingFlowWallEqNum = ConvectionConstants::HcInt_BeausoleilMorrisonMixedOppossingWall;
        int MixedBuoyOpposingFlowWallUserCurveNum = 0;
        int MixedStableFloorEqNum = ConvectionConstants::HcInt_BeausoleilMorrisonMixedStableFloor;
        int MixedStableFloorUserCurveNum = 0;
        int MixedUnstableFloorEqNum = ConvectionConstants::HcInt_BeausoleilMorrisonMixedUnstableFloor;
        int MixedUnstableFloorUserCurveNum = 0;
        int MixedStableCeilingEqNum = ConvectionConstants::HcInt_BeausoleilMorrisonMixedStableCeiling;
        int MixedStableCeilingUserCurveNum = 0;
        int MixedUnstableCeilingEqNum = ConvectionConstants::HcInt_BeausoleilMorrisonMixedUnstableCeiling;
        int MixedUnstableCeilingUserCurveNum = 0;
        int MixedWindowsEqNum = ConvectionConstants::HcInt_GoldsteinNovoselacCeilingDiffuserWindow;
        int MixedWindowsUserCurveNum = 0;
    };

    struct OutsideFaceAdaptiveConvAlgoStruct
    {
        // Members
        std::string Name;
        bool SuppressRainChange = false;
        int HWindWallWindwardEqNum = ConvectionConstants::HcExt_SparrowWindward;
        int HWindWallWindwardUserCurveNum = 0;
        int HWindWallLeewardEqNum = ConvectionConstants::HcExt_SparrowLeeward;
        int HWindWallLeewardUserCurveNum = 0;
        int HWindHorizRoofEqNum = ConvectionConstants::HcExt_ClearRoof;
        int HWindHorizRoofUserCurveNum = 0;
        int HNatVertWallEqNum = ConvectionConstants::HcExt_NaturalASHRAEVerticalWall;
        int HNatVertWallUserCurveNum = 0;
        int HNatStableHorizEqNum = ConvectionConstants::HcExt_NaturalWaltonStableHorizontalOrTilt;
        int HNatStableHorizUserCurveNum = 0;
        int HNatUnstableHorizEqNum = ConvectionConstants::HcExt_NaturalWaltonUnstableHorizontalOrTilt;
        int HNatUnstableHorizUserCurveNum = 0;
    };

    struct BoundingBoxVertStruct
    {
        // Members
        int SurfNum = 0;
        int VertNum = 0;
        Vector Vertex{0.0, 0.0, 0.0};
    };

    struct RoofGeoCharacteristicsStruct
    {
        // Members
        Real64 Area = 0.0;      // Sum of all roof surface areas
        Real64 Perimeter = 0.0; // Actual perimeter of all roof surfaces, after removing all edges that are used twice (and inserting vertices
                                // to split surfaces as needed)
        Real64 Height = 0.0;    // Weighted average mean vertical height: for each surface, take max - Zmin value,
                                // then do a weighted average by surface area
        Real64 Azimuth = 0.0;   // Weighted average azimuth
        Real64 Tilt = 0.0;      // Weighted average tilt
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

    void
    InitInteriorConvectionCoeffs(EnergyPlusData &state,
                                 const Array1D<Real64> &SurfaceTemperatures,        // Temperature of surfaces for evaluation of HcIn
                                 ObjexxFCL::Optional_int_const ZoneToResimulate = _ // if passed in, then only calculate surfaces that have this zone
    );

    void InitExteriorConvectionCoeff(EnergyPlusData &state,
                                     int SurfNum,                          // Surface number (in Surface derived type)
                                     Real64 HMovInsul,                     // Equivalent convection coefficient of movable insulation
                                     Material::SurfaceRoughness Roughness, // Roughness index (1-6), see DataHeatBalance parameters
                                     Real64 AbsExt,                        // Exterior thermal absorptance
                                     Real64 TempExt,                       // Exterior surface temperature (C)
                                     Real64 &HExt,                         // Convection coefficient to exterior air
                                     Real64 &HSky,                         // "Convection" coefficient to sky temperature
                                     Real64 &HGround,                      // "Convection" coefficient to ground temperature
                                     Real64 &HAir                          // Radiation to Air Component
    );

    Real64 CalcHfExteriorSparrow(Real64 SurfWindSpeed,                 // Local wind speed at height of the heat transfer surface (m/s)
                                 Real64 GrossArea,                     // Gross surface area {m2}
                                 Real64 Perimeter,                     // Surface perimeter length {m}
                                 Real64 CosTilt,                       // Cosine of the Surface Tilt Angle
                                 Real64 Azimuth,                       // Facing angle (degrees) of the surface outward normal
                                 Material::SurfaceRoughness Roughness, // Surface roughness index
                                 Real64 WindDirection                  // Wind (compass) direction (degrees)
    );

    bool Windward(Real64 CosTilt,      // Cosine of the surface tilt angle
                  Real64 Azimuth,      // or Facing, Direction the surface outward normal faces (degrees)
                  Real64 WindDirection // Wind direction measured clockwise from geographhic North
    );

    void GetUserConvectionCoefficients(EnergyPlusData &state);

    void ApplyConvectionValue(EnergyPlusData &state, std::string const &SurfaceTypes, std::string const &ConvectionType, int Value);

    Real64 CalcASHRAESimpExtConvectCoeff(Material::SurfaceRoughness Roughness, // Integer index for roughness, relates to parameter array indices
                                         Real64 SurfWindSpeed                  // Current wind speed, m/s
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
                                    int SurfNum,                                       // surface number for which coefficients are being calculated
                                    const Array1D<Real64> &SurfaceTemperatures,        // Temperature of surfaces for evaluation of HcIn
                                    Array1D<Real64> &HcIn,                             // Interior Convection Coeff Array
                                    ObjexxFCL::Optional<Array1S<Real64> const> Vhc = _ // Velocity array for forced convection coeff calculation
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

    RoofGeoCharacteristicsStruct getRoofGeometryInformation(EnergyPlusData &state);

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

    Real64 CalcBeausoleilMorrisonMixedAssistedWall(Real64 DeltaTemp,     // [C] temperature difference between surface and air
                                                   Real64 Height,        // [m] characteristic size
                                                   Real64 SurfTemp,      // [C] surface temperature
                                                   Real64 SupplyAirTemp, // [C] temperature of supply air into zone
                                                   Real64 AirChangeRate  // [ACH] [1/hour] supply air ACH for zone
    );

    Real64 CalcBeausoleilMorrisonMixedAssistedWall(EnergyPlusData &state,
                                                   Real64 DeltaTemp, // [C] temperature difference between surface and air
                                                   Real64 Height,    // [m] characteristic size
                                                   Real64 SurfTemp,  // [C] surface temperature
                                                   int ZoneNum       // index of zone for messaging
    );

    Real64 CalcBeausoleilMorrisonMixedOpposingWall(Real64 DeltaTemp,     // [C] temperature difference between surface and air
                                                   Real64 Height,        // [m] characteristic size
                                                   Real64 SurfTemp,      // [C] surface temperature
                                                   Real64 SupplyAirTemp, // [C] temperature of supply air into zone
                                                   Real64 AirChangeRate  // [ACH] [1/hour] supply air ACH for zone
    );

    Real64 CalcBeausoleilMorrisonMixedOpposingWall(EnergyPlusData &state,
                                                   Real64 DeltaTemp, // [C] temperature difference between surface and air
                                                   Real64 Height,    // [m] characteristic size
                                                   Real64 SurfTemp,  // [C] surface temperature
                                                   int ZoneNum       // index of zone for messaging
    );

    Real64 CalcBeausoleilMorrisonMixedStableFloor(Real64 DeltaTemp,         // [C] temperature difference between surface and air
                                                  Real64 HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
                                                  Real64 SurfTemp,          // [C] surface temperature
                                                  Real64 SupplyAirTemp,     // [C] temperature of supply air into zone
                                                  Real64 AirChangeRate      // [ACH] [1/hour] supply air ACH for zone
    );

    Real64 CalcBeausoleilMorrisonMixedStableFloor(EnergyPlusData &state,
                                                  Real64 DeltaTemp,         // [C] temperature difference between surface and air
                                                  Real64 HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
                                                  Real64 SurfTemp,          // [C] surface temperature
                                                  int ZoneNum               // index of zone for messaging
    );

    Real64 CalcBeausoleilMorrisonMixedUnstableFloor(Real64 DeltaTemp,         // [C] temperature difference between surface and air
                                                    Real64 HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
                                                    Real64 SurfTemp,          // [C] surface temperature
                                                    Real64 SupplyAirTemp,     // [C] temperature of supply air into zone
                                                    Real64 AirChangeRate      // [ACH] [1/hour] supply air ACH for zone
    );

    Real64 CalcBeausoleilMorrisonMixedUnstableFloor(EnergyPlusData &state,
                                                    Real64 DeltaTemp,         // [C] temperature difference between surface and air
                                                    Real64 HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
                                                    Real64 SurfTemp,          // [C] surface temperature
                                                    int ZoneNum               // index of zone for messaging
    );

    Real64 CalcBeausoleilMorrisonMixedStableCeiling(Real64 DeltaTemp,         // [C] temperature difference between surface and air
                                                    Real64 HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
                                                    Real64 SurfTemp,          // [C] surface temperature
                                                    Real64 SupplyAirTemp,     // [C] temperature of supply air into zone
                                                    Real64 AirChangeRate      // [ACH] [1/hour] supply air ACH for zone
    );

    Real64 CalcBeausoleilMorrisonMixedStableCeiling(EnergyPlusData &state,
                                                    Real64 DeltaTemp,         // [C] temperature difference between surface and air
                                                    Real64 HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
                                                    Real64 SurfTemp,          // [C] surface temperature
                                                    int ZoneNum               // index of zone for messaging
    );

    Real64 CalcBeausoleilMorrisonMixedUnstableCeiling(Real64 DeltaTemp,         // [C] temperature difference between surface and air
                                                      Real64 HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
                                                      Real64 SurfTemp,          // [C] surface temperature
                                                      Real64 SupplyAirTemp,     // [C] temperature of supply air into zone
                                                      Real64 AirChangeRate      // [ACH] [1/hour] supply air ACH for zone
    );

    Real64 CalcBeausoleilMorrisonMixedUnstableCeiling(EnergyPlusData &state,
                                                      Real64 DeltaTemp,         // [C] temperature difference between surface and air
                                                      Real64 HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
                                                      Real64 SurfTemp,          // [C] surface temperature
                                                      int ZoneNum               // index of zone for messaging
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

    Real64 CalcSparrowWindward(Material::SurfaceRoughness RoughnessIndex, Real64 FacePerimeter, Real64 FaceArea, Real64 WindAtZ);

    Real64 CalcSparrowWindward(
        EnergyPlusData &state, Material::SurfaceRoughness RoughnessIndex, Real64 FacePerimeter, Real64 FaceArea, Real64 WindAtZ, int SurfNum);

    Real64 CalcSparrowLeeward(Material::SurfaceRoughness RoughnessIndex, Real64 FacePerimeter, Real64 FaceArea, Real64 WindAtZ);

    Real64 CalcSparrowLeeward(
        EnergyPlusData &state, Material::SurfaceRoughness RoughnessIndex, Real64 FacePerimeter, Real64 FaceArea, Real64 WindAtZ, int SurfNum);

    Real64 CalcMoWITTNatural(Real64 DeltaTemp);

    Real64 CalcMoWITTForcedWindward(Real64 WindAtZ);

    Real64 CalcMoWITTForcedLeeward(Real64 WindAtZ);

    Real64 CalcMoWITTWindward(Real64 DeltaTemp, Real64 WindAtZ);

    Real64 CalcMoWITTLeeward(Real64 DeltaTemp, Real64 WindAtZ);

    Real64 CalcDOE2Forced(Real64 SurfaceTemp, Real64 AirTemp, Real64 CosineTilt, Real64 HfSmooth, Material::SurfaceRoughness RoughnessIndex);

    Real64 CalcDOE2Windward(Real64 SurfaceTemp, Real64 AirTemp, Real64 CosineTilt, Real64 WindAtZ, Material::SurfaceRoughness RoughnessIndex);

    Real64 CalcDOE2Leeward(Real64 SurfaceTemp, Real64 AirTemp, Real64 CosineTilt, Real64 WindAtZ, Material::SurfaceRoughness RoughnessIndex);

    Real64 CalcNusseltJurges(Real64 WindAtZ);

    Real64 CalcMcAdams(Real64 WindAtZ);

    Real64 CalcMitchell(Real64 WindAtZ, Real64 LengthScale);

    Real64 CalcMitchell(EnergyPlusData &state, Real64 WindAtZ, Real64 LengthScale, int SurfNum);

    Real64 CalcBlockenWindward(EnergyPlusData &state,
                               Real64 WindAt10m,
                               Real64 WindDir,     // Wind direction measured clockwise from geographic North
                               Real64 SurfAzimuth, // or Facing, Direction the surface outward normal faces (degrees)
                               int SurfNum);

    Real64 CalcWindSurfaceTheta(Real64 const WindDir, Real64 const SurfAzimuth);

    Real64 CalcEmmelVertical(Real64 WindAt10m,
                             Real64 WindDir,      // Wind direction measured clockwise from geographic North
                             Real64 SurfAzimuth); // or Facing, Direction the surface outward normal faces (degrees)

    Real64 CalcEmmelRoof(Real64 WindAt10m,
                         Real64 WindDir,                 // Wind direction measured clockwise from geographic North
                         Real64 LongAxisOutwardAzimuth); // or Facing, Direction the surface outward normal faces (degrees)

    Real64 CalcClearRoof(EnergyPlusData &state,
                         Real64 AirTemp,
                         Real64 WindAtZ,
                         Real64 WindDirect, // Wind direction measured clockwise from geographic North
                         Real64 RoofArea,
                         Real64 RoofPerimeter,
                         Material::SurfaceRoughness RoughnessIndex);

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
    int CalcBlockenWindwardErrorIDX = 0;
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
        *this = ConvectionCoefficientsData();
    }
};

} // namespace EnergyPlus

#endif
