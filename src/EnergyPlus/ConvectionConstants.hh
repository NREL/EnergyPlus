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

#ifndef ConvectionConstants_hh_INCLUDED
#define ConvectionConstants_hh_INCLUDED

// EnergyPlus Headers
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus::Convect {

// Data
// MODULE PARAMETER DEFINITIONS:
Real64 constexpr AdaptiveHcIntLowLimit = 0.5;  // W/m2-K
Real64 constexpr AdaptiveHcExtLowLimit = 1.0; // W/m2-K

// parameters for identifying more specific hc model equations, inside face
enum class HcInt {
    Invalid = -1,
    Value,
    Schedule,
    SetByZone,
    ASHRAESimple,
    ASHRAETARP,
    CeilingDiffuser,
    TrombeWall,
    AdaptiveConvectionAlgorithm,
    ASTMC1340,
    UserValue,
    UserSchedule,
    UserCurve,
    ASHRAEVerticalWall,
    WaltonUnstableHorizontalOrTilt,
    WaltonStableHorizontalOrTilt,
    FisherPedersenCeilDiffuserFloor,
    FisherPedersenCeilDiffuserCeiling,
    FisherPedersenCeilDiffuserWalls,
    AlamdariHammondStableHorizontal,
    AlamdariHammondVerticalWall,
    AlamdariHammondUnstableHorizontal,
    KhalifaEq3WallAwayFromHeat,
    KhalifaEq4CeilingAwayFromHeat,
    KhalifaEq5WallNearHeat,
    KhalifaEq6NonHeatedWalls,
    KhalifaEq7Ceiling,
    AwbiHattonHeatedFloor,
    AwbiHattonHeatedWall,
    BeausoleilMorrisonMixedAssistingWall,
    BeausoleilMorrisonMixedOppossingWall,
    BeausoleilMorrisonMixedStableFloor,
    BeausoleilMorrisonMixedUnstableFloor,
    BeausoleilMorrisonMixedStableCeiling,
    BeausoleilMorrisonMixedUnstableCeiling,
    FohannoPolidoriVerticalWall,
    KaradagChilledCeiling,
    ISO15099Windows,
    GoldsteinNovoselacCeilingDiffuserWindow,
    GoldsteinNovoselacCeilingDiffuserWalls,
    GoldsteinNovoselacCeilingDiffuserFloor,
    Num
};

constexpr std::array<std::string_view, static_cast<int>(HcInt::Num)> HcIntNamesUC = {
    "VALUE", 
    "SCHEDULE", 
    "SETBYZONE", 
    "SIMPLE", 
    "TARP",
    "CEILINGDIFFUSER",
    "TROMBEWALL",
    "ADAPTIVECONVECTIONALGORITHM", 
    "ASTMC1340",
    "USERVALUE",
    "USERSCHEDULE",
    "USERCURVE", 
    "ASHRAEVERTICALWALL", 
    "WALTONUNSTABLEHORIZONTALORTILT", 
    "WALTONSTABLEHORIZONTALORTILT", 
    "FISHERPEDERSENCEILINGDIFFUSERWALLS", 
    "FISHERPEDERSENCEILINGDIFFUSERCEILING", 
    "FISHERPEDERSENCEILINGDIFFUSERFLOOR", 
    "ALAMDARIHAMMONDSTABLEHORIZONTAL", 
    "ALAMDARIHAMMONDVERTICALWALL", 
    "ALAMDARIHAMMONDUNSTABLEHORIZONTAL", 
    "KHALIFAEQ3WALLAWAYFROMHEAT", 
    "KHALIFAEQ4CEILINGAWAYFROMHEAT", 
    "KHALIFAEQ5WALLNEARHEAT", 
    "KHALIFAEQ6NONHEATEDWALLS", 
    "KHALIFAEQ7CEILING", 
    "AWBIHATTONHEATEDFLOOR", 
    "AWBIHATTONHEATEDWALL", 
    "BEAUSOLEILMORRISONMIXEDASSISTEDWALL", 
    "BEAUSOLEILMORRISONMIXEDOPPOSINGWALL", 
    "BEAUSOLEILMORRISONMIXEDSTABLEFLOOR", 
    "BEAUSOLEILMORRISONMIXEDUNSTABLEFLOOR", 
    "BEAUSOLEILMORRISONMIXEDSTABLECEILING", 
    "BEAUSOLEILMORRISONMIXEDUNSTABLECEILING", 
    "FOHANNOPOLIDORIVERTICALWALL", 
    "KARADAGCHILLEDCEILING", 
    "ISO15099WINDOWS", 
    "GOLDSTEINNOVOSELACCEILINGDIFFUSERWINDOW", 
    "GOLDSTEINNOVOSELACCEILINGDIFFUSERWALLS", 
    "GOLDSTEINNOVOSELACCEILINGDIFFUSERFLOOR" 
};
	
constexpr std::array<int, static_cast<int>(HcInt::Num)> HcIntReportVals = {
    -999,// Value
    -998,// Schedule
    0,   // SetByZone
    1,   // ASHRAESimple
    2,   // ASHRAETARP
    3,   // CeilingDiffuser
    4,   // TrombeWall
    9,   // AdaptiveConvectionAlgorithm
    10,  // ASTMC1340
    200, // UserValue
    201, // UesrSchedule
    202, // UserCurve
    203, // ASHRAEVerticalWall
    204, // WaltonUnstableHorizontalOrTilt
    205, // WaltonStableHorizontalOrTilt
    206, // FisherPedersenCeilDiffuserWalls
    207, // FisherPedersenCeilDiffuserCeiling
    208, // FisherPedersenCeilDiffuserFloor
    209, // AlamdariHammondStableHorizontal
    210, // AlamdariHammondUnstableHorizontal
    211, // AlamdariHammondVerticalWall
    212, // KhalifaEq3WallAwayFromHeat
    213, // KhalifaEq4CeilingAwayFromHeat
    214, // KhalifaEq5WallNearHeat
    215, // KhalifaEq6NonHeatedWalls
    216, // KhalifaEq7Ceiling
    217, // AwbiHattonHeatedFloor
    218, // AwbiHattonHeatedWall
    219, // BeausoleilMorrisonMixedAssistingWall
    220, // BeausoleilMorrisonMixedOppossingWall
    221, // BeausoleilMorrisonMixedStableFloor
    222, // BeausoleilMorrisonMixedUnstableFloor
    223, // BeausoleilMorrisonMixedStableCeiling
    224, // BeausoleilMorrisonMixedUnstableCeiling
    225, // FohannoPolidoriVerticalWall
    226, // KaradagChilledCeiling
    227, // ISO15099Windows
    228, // GoldsteinNovoselacCeilingDiffuserWindow
    229, // GoldsteinNovoselacCeilingDiffuserWalls
    230  // GoldsteinNovoselacCeilingDiffuserFloor 
};
	
enum class HcExt {
    Invalid = -1,
    Value,
    Schedule,
    SetByZone,
    ASHRAESimple,
    ASHRAETARP,
    TarpHcOutside,
    MoWiTTHcOutside,
    DOE2HcOutside,
    BLASTHcOutside,
    AdaptiveConvectionAlgorithm,
    None, // none is allowed because Hn and Hf are split
    UserValue,
    UserSchedule,
    UserCurve,
    ASHRAESimpleCombined,
    NaturalASHRAEVerticalWall,
    NaturalWaltonUnstableHorizontalOrTilt,
    NaturalWaltonStableHorizontalOrTilt,
    SparrowWindward,
    SparrowLeeward,
    MoWiTTWindward,
    MoWiTTLeeward,
    DOE2Windward,
    DOE2Leeward,
    NusseltJurges,
    McAdams,
    Mitchell,
    ClearRoof,
    BlockenWindward,
    EmmelVertical,
    EmmelRoof,
    AlamdariHammondVerticalWall,
    FohannoPolidoriVerticalWall,
    ISO15099Windows,
    AlamdariHammondStableHorizontal,
    AlamdariHammondUnstableHorizontal,
    Num
};
	
constexpr std::array<std::string_view, static_cast<int>(HcExt::Num)> HcExtNamesUC = {
    "VALUE",
    "SCHEDULE",
    "SETBYZONE", 
    "SIMPLECOMBINED",
    "TARP",
    "TARPOUTSIDE",
    "MOWITT",
    "DOE-2",
    "BLAST",
    "ADAPTIVECONVECTIONALGORITHM",
    "NONE",
    "USERVALUE",
    "USERSCHEDULE",
    "USERCURVE",
    "SIMPLECOMBINED",
    "ASHRAEVERTICALWALL",
    "WALTONUNSTABLEHORIZONTALORTILT",
    "WALTONSTABLEHORIZONTALORTILT",
    "TARPWINDWARD",
    "TARPLEEWARD",
    "MOWITTWINDWARD",
    "MOWITTLEEWARD",
    "DOE2WINDWARD",
    "DOE2LEEWARD",
    "NUSSELTJURGES",
    "MCADAMS",
    "MITCHELL",
    "CLEARROOF",
    "BLOCKENWINDWARD",
    "EMMELVERTICAL",
    "EMMELROOF",
    "ALAMDARIHAMMONDVERTICALWALL",
    "FOHANNOPOLIDORIVERTICALWALL",
    "ISO15099WINDOWS",
    "ALAMDARIHAMMONDSTABLEHORIZONTAL",
    "ALAMDARIHAMMONDUNSTABLEHORIZONTAL"
};

constexpr std::array<int, static_cast<int>(HcExt::Num)> HcExtReportVals = {
    -999, // Value
    -998, // Schedule
    0,    // SetByZone
    1,    // ASHRAESimple
    2,    // ASHRAETARP
    5,    // TarpHcOutside
    6,    // MoWiTTHcOutside
    7,    // DOE2HcOutside
    8,    // BLASTHcOutside
    9,    // AdaptiveConvectionAlgorithm
    300,  // None
    301,  // UserValue
    302,  // UserSchedule
    303,  // UserCurve
    304,  // ASHRAESimpleCombined
    305,  // NaturalASHRAEVerticalWall
    306,  // NaturalWaltonUnstableHorizontalOrTilt
    307,  // NaturalWaltonStableHorizontalOrTilt
    308,  // SparrowWindward 
    309,  // SparrowLeeward
    310,  // MoWiTTWindward
    311,  // MoWiTTLeeward
    312,  // DOE2Windward
    313,  // DOE2Leeward
    314,  // NusseltJurges
    315,  // McAdams
    316,  // Mitchell
    317,  // ClearRoof
    318,  // BlockenWindward
    319,  // EmmelVertical
    320,  // EmmelRoof
    321,  // AlamdariHammondVerticalWall
    322,  // FohannoPolidoriVerticalWall
    323,  // ISO15099Windows
    324,  // AlamdariHammondStableHorizontal
    325   // AlamdariHammondUnstableHorizontal
};
	
// Parameters for classification of outside face of surfaces
enum class ExtConvClass
{
    Invalid = -1,
    WindwardVertWall,
    LeewardVertWall,
    RoofStable,
    RoofUnstable,
    Num
};

enum class ExtConvClass2
{
    Invalid = -1,
    WindConvection_WallWindward,
    WindConvection_WallLeeward,
    WindConvection_HorizRoof,
    NaturalConvection_VertWall,
    NaturalConvection_StableHoriz,
    NaturalConvection_UnstableHoriz,
    Num
};
	
// Report Values for "Surface Outside Face Convection Classification Index"
// note that Invalid (-1) is also reported but not included here
// where used, that should be handled with a static_cast<int>(OutConvClass::Invalid)
constexpr static std::array<int, static_cast<int>(ExtConvClass::Num)> ExtConvClassReportVals = {101, 102, 103, 104};

enum class SurfOrientation
{
    Invalid = -1,
    HorizontalDown,
    TiltedDownward,
    Vertical,
    TiltedUpward,
    HorizontalUp,
    Num
};

// Parameters for fenestration relative location in zone
enum class IntConvWinLoc
{
    Invalid = -1,
    NotSet,
    LowerPartOfExteriorWall, // this is a window in the lower part of wall
    UpperPartOfExteriorWall, // this is a window in the upper part of wall
    WindowAboveThis,         // this is a wall with window above it
    WindowBelowThis,         // this is a wall with window below it
    LargePartOfExteriorWall, // this is a big window taking up most of wall
    Num
};

// Parameters for adaptive convection algorithm's classification of inside face of surfaces
enum class IntConvClass
{
    Invalid = -1,
    // SimpleBuoy goes first in the IDF objects, so has to go first here too, A3 or not.
    A3_SimpleBuoy_VertWalls,          // flow regime A3
    A3_SimpleBuoy_StableHoriz,        // flow regime A3
    A3_SimpleBuoy_UnstableHoriz,      // flow regime A3
    A3_SimpleBuoy_StableTilted,       // flow regime A3
    A3_SimpleBuoy_UnstableTilted,     // flow regime A3
    A3_SimpleBuoy_Windows,            // flow regime A3
    A1_FloorHeatCeilCool_VertWalls,          // flow regime A1, vertical walls
    A1_FloorHeatCeilCool_StableHoriz,        // flow regime A1
    A1_FloorHeatCeilCool_UnstableHoriz,      // flow regime A1
    A1_FloorHeatCeilCool_HeatedFloor,        // flow regime A1
    A1_FloorHeatCeilCool_ChilledCeil,        // flow regime A1
    A1_FloorHeatCeilCool_StableTilted,       // flow regime A1
    A1_FloorHeatCeilCool_UnstableTilted,     // flow regime A1
    A1_FloorHeatCeilCool_Windows,            // flow regime A1
    A2_WallPanelHeat_VertWallsNonHeated, // flow regime A2
    A2_WallPanelHeat_HeatedVerticalWall, // flow regime A2
    A2_WallPanelHeat_StableHoriz,        // flow regime A2
    A2_WallPanelHeat_UnstableHoriz,      // flow regime A2
    A2_WallPanelHeat_StableTilted,       // flow regime A2
    A2_WallPanelHeat_UnstableTilted,     // flow regime A2
    A2_WallPanelHeat_Windows,            // flow regime A2
    B_ConvectiveHeat_VertWalls,           // flow regime B
    B_ConvectiveHeat_VertWallsNearHeat,   // flow regime B
    B_ConvectiveHeat_StableHoriz,         // flow regime B
    B_ConvectiveHeat_UnstableHoriz,       // flow regime B
    B_ConvectiveHeat_StableTilted,        // flow regime B
    B_ConvectiveHeat_UnstableTilted,      // flow regime B
    B_ConvectiveHeat_Windows,             // flow regime B
    C_CentralAirHeat_Walls,               // flow regime C
    C_CentralAirHeat_Ceiling,             // flow regime C
    C_CentralAirHeat_Floor,               // flow regime C
    C_CentralAirHeat_Windows,             // flow regime C
    D_ZoneFanCirc_Walls,               // flow regime D
    D_ZoneFanCirc_StableHoriz,         // flow regime D
    D_ZoneFanCirc_UnstableHoriz,       // flow regime D
    D_ZoneFanCirc_StableTilted,        // flow regime D
    D_ZoneFanCirc_UnstableTilted,      // flow regime D
    D_ZoneFanCirc_Windows,             // flow regime D
    E_MixedBuoy_AssistFlowWalls,     // flow regime E
    E_MixedBuoy_OpposFlowWalls,      // flow regime E
    E_MixedBuoy_StableFloor,         // flow regime E
    E_MixedBuoy_UnstableFloor,       // flow regime E
    E_MixedBuoy_StableCeiling,       // flow regime E
    E_MixedBuoy_UnstableCeiling,     // flow regime E
    E_MixedBuoy_Windows,             // flow regime E
    Num
};

// Report values for "Surface Inside Face Convection Classification Index"
// note that Invalid (-1) is also reported but not included here
// where used, that should be handled with a static_cast<int>(InConvClass::Invalid)
constexpr static std::array<int, static_cast<int>(IntConvClass::Num)> IntConvClassReportVals = {
    1,  2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
    24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45};

// Parameters to indicate user specified convection coefficients (for surface)
enum class OverrideType
{
    Invalid = -1,
    Value,          // User specified "value" as the override type
    Schedule,       // User specified "schedule" as the override type
    UserCurve,      // User specified "UserCurve" as the override type
    SpecifiedModel, // one of the direct named model equation keys
    Num             // count, always last element
};

// params for reference temperature type
enum class RefTemp
{
    Invalid = -1,
    MeanAirTemp,
    AdjacentAirTemp,
    SupplyAirTemp,
    Num
};

// params for wind speed type
enum class RefWind
{
    Invalid = -1,
    WeatherFile,
    AtZ,
    ParallelComp,
    ParallelCompAtZ,
    Num
};

} // namespace EnergyPlus::Convect

#endif
