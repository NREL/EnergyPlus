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

#ifndef ConvectionConstants_hh_INCLUDED
#define ConvectionConstants_hh_INCLUDED

// EnergyPlus Headers
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus::ConvectionConstants {

// Data
// MODULE PARAMETER DEFINITIONS:
Real64 constexpr AdaptiveHcInsideLowLimit{0.5};  // W/m2-K
Real64 constexpr AdaptiveHcOutsideLowLimit{1.0}; // W/m2-K

Real64 constexpr OneThird{1.0 / 3.0};   // 1/3 in highest precision
Real64 constexpr OneFourth{1.0 / 4.0};  // 1/4 in highest precision
Real64 constexpr OneFifth{1.0 / 5.0};   // 1/5 in highest precision
Real64 constexpr OneSixth{1.0 / 6.0};   // 1/6 in highest precision
Real64 constexpr FourFifths{4.0 / 5.0}; // 4/5 in highest precision

// parameters for identifying more specific hc model equations, inside face
int constexpr HcInt_Value{-999};
int constexpr HcInt_Schedule{-998};
int constexpr HcInt_SetByZone{0};
int constexpr HcInt_ASHRAESimple{1};
int constexpr HcInt_ASHRAETARP{2};
int constexpr HcInt_CeilingDiffuser{3};
int constexpr HcInt_TrombeWall{4};
int constexpr HcInt_AdaptiveConvectionAlgorithm{9};
int constexpr HcInt_ASTMC1340{10};
int constexpr HcInt_UserValue{200};
int constexpr HcInt_UserSchedule{201};
int constexpr HcInt_UserCurve{202};
int constexpr HcInt_ASHRAEVerticalWall{203};
int constexpr HcInt_WaltonUnstableHorizontalOrTilt{204};
int constexpr HcInt_WaltonStableHorizontalOrTilt{205};
int constexpr HcInt_FisherPedersenCeilDiffuserFloor{206};
int constexpr HcInt_FisherPedersenCeilDiffuserCeiling{207};
int constexpr HcInt_FisherPedersenCeilDiffuserWalls{208};
int constexpr HcInt_AlamdariHammondStableHorizontal{209};
int constexpr HcInt_AlamdariHammondVerticalWall{210};
int constexpr HcInt_AlamdariHammondUnstableHorizontal{211};
int constexpr HcInt_KhalifaEq3WallAwayFromHeat{212};
int constexpr HcInt_KhalifaEq4CeilingAwayFromHeat{213};
int constexpr HcInt_KhalifaEq5WallNearHeat{214};
int constexpr HcInt_KhalifaEq6NonHeatedWalls{215};
int constexpr HcInt_KhalifaEq7Ceiling{216};
int constexpr HcInt_AwbiHattonHeatedFloor{217};
int constexpr HcInt_AwbiHattonHeatedWall{218};
int constexpr HcInt_BeausoleilMorrisonMixedAssistingWall{219};
int constexpr HcInt_BeausoleilMorrisonMixedOppossingWall{220};
int constexpr HcInt_BeausoleilMorrisonMixedStableCeiling{221};
int constexpr HcInt_BeausoleilMorrisonMixedUnstableCeiling{222};
int constexpr HcInt_BeausoleilMorrisonMixedStableFloor{223};
int constexpr HcInt_BeausoleilMorrisonMixedUnstableFloor{224};
int constexpr HcInt_FohannoPolidoriVerticalWall{225};
int constexpr HcInt_KaradagChilledCeiling{226};
int constexpr HcInt_ISO15099Windows{227};
int constexpr HcInt_GoldsteinNovoselacCeilingDiffuserWindow{228};
int constexpr HcInt_GoldsteinNovoselacCeilingDiffuserWalls{229};
int constexpr HcInt_GoldsteinNovoselacCeilingDiffuserFloor{230};

// parameters for identifying more specific hc model equations, outside face
int constexpr HcExt_Value{-999};
int constexpr HcExt_Schedule{-998};
int constexpr HcExt_ASHRAESimple{1};
int constexpr HcExt_ASHRAETARP{2};
int constexpr HcExt_TarpHcOutside{5};
int constexpr HcExt_MoWiTTHcOutside{6};
int constexpr HcExt_DOE2HcOutside{7};
int constexpr HcExt_BLASTHcOutside{8};
int constexpr HcExt_AdaptiveConvectionAlgorithm{9};
int constexpr HcExt_None{300}; // none is allowed because Hn and Hf are split
int constexpr HcExt_UserValue{301};
int constexpr HcExt_UserSchedule{302};
int constexpr HcExt_UserCurve{303};
int constexpr HcExt_ASHRAESimpleCombined{304};
int constexpr HcExt_NaturalASHRAEVerticalWall{305};
int constexpr HcExt_NaturalWaltonUnstableHorizontalOrTilt{306};
int constexpr HcExt_NaturalWaltonStableHorizontalOrTilt{307};
int constexpr HcExt_SparrowWindward{308};
int constexpr HcExt_SparrowLeeward{309};
int constexpr HcExt_MoWiTTWindward{310};
int constexpr HcExt_MoWiTTLeeward{311};
int constexpr HcExt_DOE2Windward{312};
int constexpr HcExt_DOE2Leeward{313};
int constexpr HcExt_NusseltJurges{314};
int constexpr HcExt_McAdams{315};
int constexpr HcExt_Mitchell{316};
int constexpr HcExt_ClearRoof{317};
int constexpr HcExt_BlockenWindward{318};
int constexpr HcExt_EmmelVertical{319};
int constexpr HcExt_EmmelRoof{320};
int constexpr HcExt_AlamdariHammondVerticalWall{321};
int constexpr HcExt_FohannoPolidoriVerticalWall{322};
int constexpr HcExt_ISO15099Windows{323};
int constexpr HcExt_AlamdariHammondStableHorizontal{324};
int constexpr HcExt_AlamdariHammondUnstableHorizontal{325};

// Parameters for classification of outside face of surfaces
enum class OutConvClass
{
    Invalid = -1,
    WindwardVertWall = 101,
    LeewardVertWall = 102,
    RoofStable = 103,
    RoofUnstable = 104,
};

enum class ConvSurfDeltaT : int
{
    Invalid = -1,
    Positive,
    Zero,
    Negative,
    Num, // count, always the last element
};

enum class SurfConvOrientation : int
{
    Invalid = -1,
    HorizontalDown,
    TiltedDownward,
    Vertical,
    TiltedUpward,
    HorizontalUp,
    Num, // count, always the last element
};

// Parameters for fenestration relative location in zone
enum class InConvWinLoc
{
    Invalid = -1,
    NotSet = 0,
    LowerPartOfExteriorWall = 1, // this is a window in the lower part of wall
    UpperPartOfExteriorWall = 2, // this is a window in the upper part of wall
    WindowAboveThis = 3,         // this is a wall with window above it
    WindowBelowThis = 4,         // this is a wall with window below it
    LargePartOfExteriorWall = 5, // this is a big window taking up most of wall
};

// Parameters for adaptive convection algorithm's classification of inside face of surfaces
enum class InConvClass
{
    Invalid = -1,
    A1_VertWalls = 1,           // flow regime A1, vertical walls
    A1_StableHoriz = 2,         // flow regime A1
    A1_UnstableHoriz = 3,       // flow regime A1
    A1_HeatedFloor = 4,         // flow regime A1
    A1_ChilledCeil = 5,         // flow regime A1
    A1_StableTilted = 6,        // flow regime A1
    A1_UnstableTilted = 7,      // flow regime A1
    A1_Windows = 8,             // flow regime A1
    A2_VertWallsNonHeated = 9,  // flow regime A2
    A2_HeatedVerticalWall = 10, // flow regime A2
    A2_StableHoriz = 11,        // flow regime A2
    A2_UnstableHoriz = 12,      // flow regime A2
    A2_StableTilted = 13,       // flow regime A2
    A2_UnstableTilted = 14,     // flow regime A2
    A2_Windows = 15,            // flow regime A2
    A3_VertWalls = 16,          // flow regime A3
    A3_StableHoriz = 17,        // flow regime A3
    A3_UnstableHoriz = 18,      // flow regime A3
    A3_StableTilted = 19,       // flow regime A3
    A3_UnstableTilted = 20,     // flow regime A3
    A3_Windows = 21,            // flow regime A3
    B_VertWalls = 22,           // flow regime B
    B_VertWallsNearHeat = 23,   // flow regime B
    B_StableHoriz = 24,         // flow regime B
    B_UnstableHoriz = 25,       // flow regime B
    B_StableTilted = 26,        // flow regime B
    B_UnstableTilted = 27,      // flow regime B
    B_Windows = 28,             // flow regime B
    C_Walls = 29,               // flow regime C
    C_Ceiling = 30,             // flow regime C
    C_Floor = 31,               // flow regime C
    C_Windows = 32,             // flow regime C
    D_Walls = 33,               // flow regime D
    D_StableHoriz = 34,         // flow regime D
    D_UnstableHoriz = 35,       // flow regime D
    D_StableTilted = 36,        // flow regime D
    D_UnstableTilted = 37,      // flow regime D
    D_Windows = 38,             // flow regime D
    E_AssistFlowWalls = 39,     // flow regime E
    E_OpposFlowWalls = 40,      // flow regime E
    E_StableFloor = 41,         // flow regime E
    E_UnstableFloor = 42,       // flow regime E
    E_StableCeiling = 43,       // flow regime E
    E_UnstableCeiling = 44,     // flow regime E
    E_Windows = 45,             // flow regime E
};

// Parameters to indicate user specified convection coefficients (for surface)
enum class ConvCoefOverrideType : int
{
    Invalid = -1,
    Value,          // User specified "value" as the override type
    Schedule,       // User specified "schedule" as the override type
    UserCurve,      // User specified "UserCurve" as the override type
    SpecifiedModel, // one of the direct named model equation keys
    Num,            // count, always last element
};

// parameters, by zone, for flow regimes for adaptive convection on inside face
enum class InConvFlowRegime
{
    Invalid = -1,
    A1, // In-floor heating or in-ceiling cooling
    A2, // In-wall heating
    A3, // no HVAC system, all buoyancy
    B,  // Convective heater in zone
    C,  // central mechanical air
    D,  // zone mechanical air
    E,  // mixed. mechanical air and buoyancy
};

// params for reference temperature type
enum class RefTemp
{
    Invalid = -1,
    MeanAirTemp,
    AdjacentAirTemp,
    SupplyAirTemp,
};

// params for wind speed type
enum class RefWind
{
    Invalid = -1,
    WeatherFile,
    AtZ,
    ParallelComp,
    ParallelCompAtZ,
};

} // namespace EnergyPlus::ConvectionConstants

#endif
