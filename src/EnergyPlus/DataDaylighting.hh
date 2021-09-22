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

#ifndef DataDaylighting_hh_INCLUDED
#define DataDaylighting_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Array3D.hh>
#include <ObjexxFCL/Array4D.hh>
#include <ObjexxFCL/Array5D.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EPVector.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

namespace DataDaylighting {

    // Two kinds of reference points: used directly in daylighting, used to show illuminance map of zone
    constexpr int MaxMapRefPoints(2500); // Maximum number of Illuminance Map Ref Points

    enum class SkyType : int
    {
        Clear = 1,
        ClearTurbid,
        Intermediate,
        Overcast
    };

    enum class iExtWinType
    {
        NotInOrAdjZoneExtWin, // Exterior window is not in a Daylighting:Detailed zone or in an adjacent zone with a shared interior window
        InZoneExtWin,         // Exterior window is in a Daylighting:Detailed zone
        AdjZoneExtWin         // Exterior window is in a zone adjacent to a Daylighting:Detailed zone with which it shares an interior window
    };

    enum class iCalledFor
    {
        RefPoint,
        MapPoint
    };

    enum class iDaylightingMethod
    {
        NoDaylighting,
        SplitFluxDaylighting,
        DElightDaylighting
    };

    // Parameters for "Lighting Control Type" - these are the values expected by DElight
    enum class LtgCtrlType
    {
        Invalid = 0,
        Continuous = 1,
        Stepped = 2,
        ContinuousOff = 3
    };

    constexpr std::array<std::string_view, 4> LtgCtrlTypeNamesUC = {"INVALID", "CONTINUOUS", "STEPPED", "CONTINUOUSOFF"};

    struct IntWinAdjEnclExtWinStruct // nested structure for EnclDaylightCalc
    {
        // Members
        int SurfNum = 0;         // exterior window index
        int NumOfIntWindows = 0; // count of interior windows associated with this ext win
        Array1D_int IntWinNum;   // index numbers for interior windows assoc with this ext win
    };

    struct EnclDaylightCalc
    {
        Real64 aveVisDiffReflect = 0.0; // Area-weighted average inside surface visible reflectance of zone
        Real64 totInsSurfArea = 0.0;    // Total inside surface area of a daylit zone (m2)
        Real64 floorVisRefl = 0.0;      // Area-weighted visible reflectance of floor of a daylit zone
        int TotalExtWindows = 0;        // Total number of exterior windows in the zone or same solar enclosure
        Array1D_int AdjIntWinEnclNums;  // List of enclosure numbers of adjacent enclosures that have exterior windows and
        // share one or more interior windows with target enclosure
        int NumOfIntWinAdjEncls = 0; // Number of adjacent enclosures that have exterior windows and share one or
        // more interior windows with target enclosure
        int NumOfIntWinAdjEnclExtWins = 0; // number of exterior windows associated with enclosure via interior windows
        Array1D<IntWinAdjEnclExtWinStruct>
            IntWinAdjEnclExtWin;          // nested structure | info about exterior window associated with enclosure via interior window
        int NumOfDayltgExtWins = 0;       // Number of associated exterior windows providing daylight to this enclosure
        Array1D_int DayltgExtWinSurfNums; // List of surface numbers of enclosure's exterior windows or
        // exterior windows in adjacent enclosures sharing interior windows with the enclosure
        bool adjEnclHasDayltgCtrl = false;  // True if at least one adjacent enclosure, sharing one or more interior windows, has daylighting control
        Real64 MinIntWinSolidAng = 0.0;     // Minimum solid angle subtended by an interior window in a zone
        Real64 InterReflIllFrIntWins = 0.0; // Inter-reflected illuminance due to beam and diffuse solar passing
        //  through a zone's interior windows (lux)
        bool hasSplitFluxDaylighting = false;
        EPVector<int> daylightControlIndexes; // Indexes to daylighting:controls object operating in this enclosure
    };

    struct DaylightingControl
    {
        std::string Name;     // Name of the daylighting:controls object
        std::string ZoneName; // name of the zone where the daylighting:controls object is located
        int zoneIndex = 0;    // Index to zone where the daylighting:controls object is located
        int spaceIndex = 0;   // Index to space where the daylighting:controls object is located (0 if specified for a zone)
        int enclIndex = 0;    // Index to enclosure where the daylighting:controls object is located
        DataDaylighting::iDaylightingMethod DaylightMethod = iDaylightingMethod::NoDaylighting; // Type of Daylighting (1=SplitFlux, 2=DElight)
        int AvailSchedNum = 0;                                                                  // pointer to availability schedule if present
        int TotalDaylRefPoints = 0;        // Number of daylighting reference points for this control
        Array1D_int DaylRefPtNum;          // Reference number to DaylRefPt array that stores Daylighting:ReferencePoint
        Array2D<Real64> DaylRefPtAbsCoord; // =0.0 ! X,Y,Z coordinates of all daylighting reference points
        // in absolute coordinate system (m)
        // Points 1 and 2 are the control reference points
        Array1D_bool DaylRefPtInBounds;                         // True when coordinates are in bounds of zone coordinates
        Array1D<Real64> FracZoneDaylit;                         // =0.0  ! Fraction of zone controlled by each reference point
        Real64 sumFracLights = 0.0;                             // Sum of lighting control fractions for this daylighting control
        Array1D<Real64> IllumSetPoint;                          // =0.0  ! Illuminance setpoint at each reference point (lux)
        LtgCtrlType LightControlType = LtgCtrlType::Continuous; // Lighting control type (same for all reference points)
        // (1=continuous, 2=stepped, 3=continuous/off)
        int glareRefPtNumber = 0;                  // from field: Glare Calculation Daylighting Reference Point Name
        Real64 ViewAzimuthForGlare = 0.0;          // View direction relative to window for glare calculation (deg)
        int MaxGlareallowed = 0;                   // Maximum allowable discomfort glare index
        Real64 MinPowerFraction = 0.0;             // Minimum fraction of power input that continuous dimming system can dim down to
        Real64 MinLightFraction = 0.0;             // Minimum fraction of light output that continuous dimming system can dim down to
        int LightControlSteps = 0;                 // Number of levels (excluding zero) of stepped control system
        Real64 LightControlProbability = 0.0;      // For manual control of stepped systems, probability that lighting will
        Real64 PowerReductionFactor = 1.0;         // Electric power reduction factor for this control due to daylighting
        Real64 DElightGriddingResolution = 0.0;    // Field: Delight Gridding Resolution
        Array1D<Real64> RefPtPowerReductionFactor; // =1.0  ! Electric power reduction factor at reference points
        // due to daylighting
        Array1D<Real64> DaylIllumAtRefPt;        // =0.0 ! Daylight illuminance at reference points (lux)
        Array1D<Real64> GlareIndexAtRefPt;       // =0.0 ! Glare index at reference points
        Array1D<Real64> BacLum;                  // =0.0 ! Background luminance at each reference point (cd/m2)
        Array2D<Real64> SolidAngAtRefPt;         // (Number of Zones, Total Daylighting Reference Points)
        Array2D<Real64> SolidAngAtRefPtWtd;      // (Number of Zones, Total Daylighting Reference Points)
        Array3D<Real64> IllumFromWinAtRefPt;     // (Number of Zones, 2, Total Daylighting Reference Points)
        Array3D<Real64> BackLumFromWinAtRefPt;   // (Number of Zones, 2, Total Daylighting Reference Points)
        Array3D<Real64> SourceLumFromWinAtRefPt; // (Number of Zones, 2, Total Daylighting Reference Points)
        Array1D<Real64> TimeExceedingGlareIndexSPAtRefPt;
        // Allocatable daylight factor arrays
        // Arguments (dimensions) for Dayl---Sky are:
        //  1: Sun position index / HourOfDay (1 to 24)
        //  2: Shading index (1 to MaxSlatAngs+1; 1 = bare window; 2 = with shade, or, if blinds
        //      2 = first slat position, 3 = second position, ..., MaxSlatAngs+1 = last position)
        //  3: Reference point number (1 to Total Daylighting Reference Points)
        //  4: Sky type (1 to 4; 1 = clear, 2 = clear turbid, 3 = intermediate, 4 = overcast
        //  5: Daylit window number (1 to NumOfDayltgExtWins)
        Array5D<Real64> DaylIllFacSky;
        Array5D<Real64> DaylSourceFacSky;
        Array5D<Real64> DaylBackFacSky;
        // Arguments (dimensions) for Dayl---Sun are:
        //  1: Sun position index / HourOfDay (1 to 24)
        //  2: Shading index (1 to MaxSlatAngs+1; 1 = bare window; 2 = with shade, or, if blinds
        //      2 = first slat position, 3 = second position, ..., MaxSlatAngs+1 = last position)
        //  3: Reference point number (1 to Total Daylighting Reference Points)
        //  4: Daylit window number (1 to NumOfDayltgExtWins)
        Array4D<Real64> DaylIllFacSun;
        Array4D<Real64> DaylIllFacSunDisk;
        Array4D<Real64> DaylSourceFacSun;
        Array4D<Real64> DaylSourceFacSunDisk;
        Array4D<Real64> DaylBackFacSun;
        Array4D<Real64> DaylBackFacSunDisk;
        // Time exceeding daylight illuminance setpoint at reference points (hours)
        Array1D<Real64> TimeExceedingDaylightIlluminanceSPAtRefPt;
        std::vector<std::vector<int>> ShadeDeployOrderExtWins; // describes how the fenestration surfaces should deploy the shades.
        // It is a list of lists. Each sublist is a group of fenestration surfaces that should be deployed together. Many times the
        // sublists a just a single index to a fenestration surface if they are deployed one at a time.
        Array1D_int MapShdOrdToLoopNum; // list that maps back the original loop order when using ShadeDeployOrderExtWins for shade deployment
        // Time exceeding maximum allowable discomfort glare index at reference points (hours)
    };

    struct ZoneDaylightCalc
    {
        Real64 zoneAvgIllumSum = 0.0; // For VisualResilienceSummary reported average illuminance
        int totRefPts = 0.0;          // For VisualResilienceSummary total number of rereference points
    };

    struct IllumMapData
    {
        // Members
        std::string Name;                    // Map name
        int zoneIndex;                       // Index to zone being mapped
        int enclIndex = 0;                   // Index to enclosure for this map
        Real64 Z = 0.0;                      // Elevation or height
        Real64 Xmin = 0.0;                   // Minimum X value
        Real64 Xmax = 0.0;                   // Maximum X value
        int Xnum = 0;                        // Number of X reference points (going N-S)
        Real64 Xinc = 0.0;                   // Increment between X reference points
        Real64 Ymin = 0.0;                   // Minimum Y value
        Real64 Ymax = 0.0;                   // Maximum Y value
        int Ynum = 0;                        // Number of Y reference points (going E-W)
        Real64 Yinc = 0.0;                   // Increment between Y reference points
        SharedFileHandle mapFile;            // Unit number for map output (later merged to final file)
        bool HeaderXLineLengthNeeded = true; // X header will likely be the longest line in the file
        int HeaderXLineLength = 0;           // actual length of this X header line
        std::string pointsHeader;            // part of the header that lists the reference points in the same zone
    };

    struct MapCalcData
    {
        // Members
        int TotalMapRefPoints = 0;        // Number of illuminance map reference points in this zone (up to 100)
        int zoneIndex = 0;                // Pointer to zone being mapped
        int enclIndex = 0;                // Index to enclosure for this map
        Array2D<Real64> MapRefPtAbsCoord; // X,Y,Z coordinates of all illuminance map reference points
        // in absolute coordinate system (m)
        // Points 1 and 2 are the control reference points
        Array1D_bool MapRefPtInBounds;     // True when coordinates are in bounds of zone coordinates
        Array1D<Real64> DaylIllumAtMapPt;  // Daylight illuminance at illuminance map points (lux)
        Array1D<Real64> GlareIndexAtMapPt; // Glare index at illuminance map points
        // following Hr - report avg hr
        Array1D<Real64> DaylIllumAtMapPtHr;      // Daylight illuminance at illuminance map points (lux)
        Array1D<Real64> GlareIndexAtMapPtHr;     // Glare index at illuminance map points
        Array2D<Real64> SolidAngAtMapPt;         // (Number of Zones, Total Map Reference Points)
        Array2D<Real64> SolidAngAtMapPtWtd;      // (Number of Zones, Total Map Reference Points)
        Array3D<Real64> IllumFromWinAtMapPt;     // (Number of Zones, 2, Total Map Reference Points)
        Array3D<Real64> BackLumFromWinAtMapPt;   // (Number of Zones, 2, Total Map Reference Points)
        Array3D<Real64> SourceLumFromWinAtMapPt; // (Number of Zones, 2, Total Map Reference Points)
        // Arguments (dimensions) for Dayl---Sky are:
        //  1: Sun position index / HourOfDay (1 to 24)
        //  2: Shading index (1 to MaxSlatAngs+1; 1 = bare window; 2 = with shade, or, if blinds
        //      2 = first slat position, 3 = second position, ..., MaxSlatAngs+1 = last position)
        //  3: Reference point number (1 to Total Map Reference Points)
        //  4: Sky type (1 to 4; 1 = clear, 2 = clear turbid, 3 = intermediate, 4 = overcast
        //  5: Daylit window number (1 to NumOfDayltgExtWins)
        Array5D<Real64> DaylIllFacSky;
        Array5D<Real64> DaylSourceFacSky;
        Array5D<Real64> DaylBackFacSky;
        // Arguments (dimensions) for Dayl---Sun are:
        //  1: Sun position index / HourOfDay (1 to 24)
        //  2: Shading index (1 to MaxSlatAngs+1; 1 = bare window; 2 = with shade, or, if blinds
        //      2 = first slat position, 3 = second position, ..., MaxSlatAngs+1 = last position)
        //  3: Reference point number (1 to Total Map Reference Points)
        //  4: Daylit window number (1 to NumOfDayltgExtWins)
        Array4D<Real64> DaylIllFacSun;
        Array4D<Real64> DaylIllFacSunDisk;
        Array4D<Real64> DaylSourceFacSun;
        Array4D<Real64> DaylSourceFacSunDisk;
        Array4D<Real64> DaylBackFacSun;
        Array4D<Real64> DaylBackFacSunDisk;
    };

    struct RefPointData
    {
        std::string Name; // Map name
        int ZoneNum;      // Pointer to zone being referenced
        Real64 x;         // x coordinate
        Real64 y;         // y coordinate
        Real64 z;         // z coordinate
        int indexToFracAndIllum;

        // Default Constructor
        RefPointData() : ZoneNum(0), x(0.0), y(0.0), z(0.0), indexToFracAndIllum(0)
        {
        }
    };

    struct DElightComplexFeneData // holds Daylighting:DELight:ComplexFenestration
    {
        std::string Name;
        std::string ComplexFeneType; // Complex Fenestration Type
        std::string surfName;        // Building Surface name
        std::string wndwName;        // Window name
        Real64 feneRota;             // Fenestration Rotation
    };

} // namespace DataDaylighting

struct DaylightingData : BaseGlobalStruct
{

    int totDaylightingControls = 0;
    int TotRefPoints = 0;
    int maxRefPointsPerControl = 0;
    int TotIllumMaps = 0;
    bool mapResultsToReport = false; // used when only partial hour has "sun up"
    bool mapResultsReported = false; // when no map results are ever reported this will still be false
    char MapColSep;                  // Character for separating map columns (tab, space, comma)
    bool DFSReportSizingDays = false;
    bool DFSReportAllShadowCalculationDays = false;
    int TotDElightCFS = 0;

    Array1D<DataDaylighting::EnclDaylightCalc> enclDaylight;
    Array1D<DataDaylighting::ZoneDaylightCalc> ZoneDaylight;
    Array1D<DataDaylighting::DaylightingControl> daylightControl;
    Array1D<DataDaylighting::IllumMapData> IllumMap;
    Array1D<DataDaylighting::MapCalcData> IllumMapCalc;
    Array1D<DataDaylighting::RefPointData> DaylRefPt;
    Array1D<DataDaylighting::DElightComplexFeneData> DElightComplexFene;
    Array1D<Real64> spacePowerReductionFactor; // Average electric power reduction factor for space due to daylighting

    void clear_state() override
    {
        this->totDaylightingControls = 0;
        this->TotRefPoints = 0;
        this->maxRefPointsPerControl = 0;
        this->TotIllumMaps = 0;
        this->mapResultsToReport = false;
        this->mapResultsReported = false;
        this->MapColSep = char();
        this->DFSReportSizingDays = false;
        this->DFSReportAllShadowCalculationDays = false;
        this->TotDElightCFS = 0;
        this->enclDaylight.deallocate();
        this->ZoneDaylight.deallocate();
        this->daylightControl.deallocate();
        this->IllumMap.deallocate();
        this->IllumMapCalc.deallocate();
        this->DaylRefPt.deallocate();
        this->DElightComplexFene.deallocate();
        this->spacePowerReductionFactor.deallocate();
    }
};

} // namespace EnergyPlus

#endif
