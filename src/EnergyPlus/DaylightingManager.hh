// EnergyPlus, Copyright (c) 1996-2022, The Board of Trustees of the University of Illinois,
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

#ifndef DaylightingManager_hh_INCLUDED
#define DaylightingManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Array2A.hh>
#include <ObjexxFCL/Array2S.hh>
#include <ObjexxFCL/Array3D.hh>
#include <ObjexxFCL/Vector3.fwd.hh>

// EnergyPlus Headers
#include <EnergyPlus/DataBSDFWindow.hh>
#include <EnergyPlus/DataDaylighting.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace DaylightingManager {

    // Surface count crossover for using octree algorithm
    // The octree gives lower computational complexity for much higher performance
    //  as the surface count increases but has some overhead such that the direct
    //  algorithm can be more efficient at small surface counts.
    // Testing to date shows that the octree performance is close to that of the
    //  direct algorithm even with small surface counts and that there is no single
    //  crossover that is ideal for all models: some cases with 10-30 surfaces were
    //  faster with the octree but another with 80 surfaces was faster with the
    //  direct algorithm.
    // A reasonable, conservative crossover is selected but may be refined as more
    //  experience is gained.
    constexpr int octreeCrossover(100); // Octree surface count crossover
    constexpr int NTH(18);              // Number of azimuth steps for sky integration
    constexpr int NPH(8);               // Number of altitude steps for sky integration
    constexpr int NPHMAX(10);           // Number of sky/ground integration steps in altitude
    constexpr int NTHMAX(16);           // Number of sky/ground integration steps in azimuth

    void DayltgAveInteriorReflectance(EnergyPlusData &state, int const enclNum); // Enclosure number

    void CalcDayltgCoefficients(EnergyPlusData &state);

    void CalcDayltgCoeffsRefMapPoints(EnergyPlusData &state);

    void CalcDayltgCoeffsRefPoints(EnergyPlusData &state, int const daylightCtrlNum);

    void CalcDayltgCoeffsMapPoints(EnergyPlusData &state, int const mapNum);

    void FigureDayltgCoeffsAtPointsSetupForWindow(
        EnergyPlusData &state,
        int const daylightCtrlNum, // zero if called for map points
        int const iRefPoint,
        int const loopwin,
        DataDaylighting::CalledFor const CalledFrom, // indicate  which type of routine called this routine
        Vector3<Real64> const &RREF,                 // Location of a reference point in absolute coordinate system
        Vector3<Real64> const &VIEWVC,               // View vector in absolute coordinate system
        int &IWin,
        int &IWin2,
        int &NWX,
        int &NWY,
        Vector3<Real64> &W2,                     // Second vertex of window
        Vector3<Real64> &W3,                     // Third vertex of window
        Vector3<Real64> &W21,                    // Vector from window vertex 2 to window vertex 1
        Vector3<Real64> &W23,                    // Vector from window vertex 2 to window vertex 3
        int &LSHCAL,                             // Interior shade calculation flag:  0=not yet calculated, 1=already calculated
        int &InShelfSurf,                        // Inside daylighting shelf surface number
        int &ICtrl,                              // Window control counter
        DataSurfaces::WinShadingType &ShType,    // Window shading type
        int &BlNum,                              // Window blind number
        Vector3<Real64> &WNORM2,                 // Unit vector normal to window
        DataDaylighting::ExtWinType &ExtWinType, // Exterior window type (InZoneExtWin, AdjZoneExtWin, NotInOrAdjZoneExtWin)
        int &IConst,                             // Construction counter
        Vector3<Real64> &RREF2,                  // Location of virtual reference point in absolute coordinate system
        Real64 &DWX,                             // Horizontal dimension of window element (m)
        Real64 &DWY,                             // Vertical dimension of window element (m)
        Real64 &DAXY,                            // Area of window element
        Vector3<Real64> &U2,                     // Second vertex of window for TDD:DOME (if exists)
        Vector3<Real64> &U23,                    // Vector from window vertex 2 to window vertex 3 for TDD:DOME (if exists)
        Vector3<Real64> &U21,                    // Vector from window vertex 2 to window vertex 1 for TDD:DOME (if exists)
        Vector3<Real64> &VIEWVC2,                // Virtual view vector in absolute coordinate system
        bool &Rectangle,                         // True if window is rectangular
        bool &Triangle,                          // True if window is triangular
        int const MapNum = 0);

    void FigureDayltgCoeffsAtPointsForWindowElements(
        EnergyPlusData &state,
        int const daylightCtrlNum, // Current daylighting control number (only used when called from RefPoint)
        int const iRefPoint,
        int const loopwin,
        DataDaylighting::CalledFor const CalledFrom, // indicate  which type of routine called this routine
        int const WinEl,                             // Current window element number
        int const IWin,
        int const IWin2,
        int const iXelement,
        int const iYelement,
        Real64 &SkyObstructionMult,
        Vector3<Real64> const &W2,                    // Second vertex of window
        Vector3<Real64> const &W21,                   // Vector from window vertex 2 to window vertex 1
        Vector3<Real64> const &W23,                   // Vector from window vertex 2 to window vertex 3
        Vector3<Real64> const &RREF,                  // Location of a reference point in absolute coordinate system
        int const NWYlim,                             // For triangle, largest NWY for a given IX
        Vector3<Real64> const &VIEWVC2,               // Virtual view vector in absolute coordinate system
        Real64 const DWX,                             // Horizontal dimension of window element (m)
        Real64 const DWY,                             // Vertical dimension of window element (m)
        Real64 const DAXY,                            // Area of window element
        Vector3<Real64> const &U2,                    // Second vertex of window for TDD:DOME (if exists)
        Vector3<Real64> const &U23,                   // Vector from window vertex 2 to window vertex 3 for TDD:DOME (if exists)
        Vector3<Real64> const &U21,                   // Vector from window vertex 2 to window vertex 1 for TDD:DOME (if exists)
        Vector3<Real64> &RWIN,                        // Center of a window element for TDD:DOME (if exists) in abs coord sys
        Vector3<Real64> &RWIN2,                       // Center of a window element for TDD:DOME (if exists) in abs coord sys
        Vector3<Real64> &Ray,                         // Unit vector along ray from reference point to window element
        Real64 &PHRAY,                                // Altitude of ray from reference point to window element (radians)
        int &LSHCAL,                                  // Interior shade calculation flag:  0=not yet calculated, 1=already calculated
        Real64 &COSB,                                 // Cosine of angle between window outward normal and ray from reference point to window element
        Real64 &ObTrans,                              // Product of solar transmittances of exterior obstructions hit by ray
        Real64 &TVISB,                                // Visible transmittance of window for COSB angle of incidence (times light well
        Real64 &DOMEGA,                               // Solid angle subtended by window element wrt reference point (steradians)
        Real64 &THRAY,                                // Azimuth of ray from reference point to window element (radians)
        bool &hitIntObs,                              // True iff interior obstruction hit
        bool &hitExtObs,                              // True iff ray from ref pt to ext win hits an exterior obstruction
        Vector3<Real64> const &WNORM2,                // Unit vector normal to window
        DataDaylighting::ExtWinType const ExtWinType, // Exterior window type (InZoneExtWin, AdjZoneExtWin, NotInOrAdjZoneExtWin)
        int const IConst,                             // Construction counter
        Vector3<Real64> const &RREF2,                 // Location of virtual reference point in absolute coordinate system
        bool const Triangle,
        Real64 &TVISIntWin,     // Visible transmittance of int win at COSBIntWin for light from ext win
        Real64 &TVISIntWinDisk, // Visible transmittance of int win at COSBIntWin for sun
        int const MapNum = 0);

    void InitializeCFSDaylighting(EnergyPlusData &state,
                                  int const daylightCtrlNum,       // Current daylighting control number
                                  int const IWin,                  // Complex fenestration number
                                  int const NWX,                   // Number of horizontal divisions
                                  int const NWY,                   // Number of vertical divisions
                                  Vector3<Real64> const &RefPoint, // reference point coordinates
                                  int const NRefPts,               // Number of reference points
                                  int const iRefPoint,             // Reference points counter
                                  DataDaylighting::CalledFor const CalledFrom,
                                  int const MapNum = 0);

    void InitializeCFSStateData(EnergyPlusData &state,
                                DataBSDFWindow::BSDFRefPoints &StateRefPoint,
                                DataBSDFWindow::BSDFRefPointsGeomDescr &DaylghtGeomDescr,
                                int const daylightCtrlNum, // Current daylighting control number
                                int const iWin,
                                Vector3<Real64> const &RefPoint, // reference point
                                int const CurFenState,
                                int const NBasis,
                                int const NTrnBasis,
                                Real64 const AZVIEW,
                                int const NWX,
                                int const NWY,
                                Vector3<Real64> const &W2,
                                Vector3<Real64> const &W21,
                                Vector3<Real64> const &W23,
                                Real64 const DWX,
                                Real64 const DWY,
                                Vector3<Real64> const &WNorm, // unit vector from window (point towards outside)
                                Real64 const WinElArea);

    void AllocateForCFSRefPointsState(
        EnergyPlusData &state, DataBSDFWindow::BSDFRefPoints &StateRefPoint, int const NumOfWinEl, int const NBasis, int const NTrnBasis);

    void AllocateForCFSRefPointsGeometry(DataBSDFWindow::BSDFRefPointsGeomDescr &RefPointsGeomDescr, int const NumOfWinEl);

    void CFSRefPointSolidAngle(EnergyPlusData &state,
                               Vector3<Real64> const &RefPoint,
                               Vector3<Real64> const &RWin,
                               Vector3<Real64> const &WNorm,
                               DataBSDFWindow::BSDFRefPoints &RefPointMap,
                               DataBSDFWindow::BSDFRefPointsGeomDescr &RefPointGeomMap,
                               int const iWin,
                               int const CurFenState,
                               int const NTrnBasis,
                               int const curWinEl,
                               Real64 const WinElArea);

    void CFSRefPointPosFactor(EnergyPlusData &state,
                              Vector3<Real64> const &RefPoint,
                              DataBSDFWindow::BSDFRefPoints &RefPointMap,
                              int const iWin,
                              int const CurFenState,
                              int const NTrnBasis,
                              Real64 const AZVIEW);

    Real64 CalcObstrMultiplier(EnergyPlusData &state,
                               Vector3<Real64> const &GroundHitPt, // Coordinates of point that ray hits ground (m)
                               int const AltSteps,                 // Number of steps in altitude angle for solar reflection calc
                               int const AzimSteps                 // Number of steps in azimuth angle of solar reflection calc
    );

    void FigureDayltgCoeffsAtPointsForSunPosition(
        EnergyPlusData &state,
        int const daylightCtrlNum, // Current daylighting control number
        int const iRefPoint,
        int const iXelement,
        int const NWX, // Number of window elements in x direction for dayltg calc
        int const iYelement,
        int const NWY,   // Number of window elements in y direction for dayltg calc
        int const WinEl, // Current window element counter
        int const IWin,
        int const IWin2,
        int const iHour,
        int &ISunPos,
        Real64 const SkyObstructionMult,
        Vector3<Real64> const &RWIN2, // Center of a window element for TDD:DOME (if exists) in abs coord sys
        Vector3<Real64> const &Ray,   // Unit vector along ray from reference point to window element
        Real64 const PHRAY,           // Altitude of ray from reference point to window element (radians)
        int const LSHCAL,             // Interior shade calculation flag:  0=not yet calculated, 1=already calculated
        int const InShelfSurf,        // Inside daylighting shelf surface number
        Real64 const COSB,            // Cosine of angle between window outward normal and ray from reference point to window element
        Real64 const ObTrans, // Product of solar transmittances of exterior obstructions hit by ray from reference point through a window element
        Real64 const TVISB,   // Visible transmittance of window for COSB angle of incidence (times light well efficiency, if appropriate)
        Real64 const DOMEGA,  // Solid angle subtended by window element wrt reference point (steradians)
        int const ICtrl,      // Window control counter
        DataSurfaces::WinShadingType const ShType,    // Window shading type
        int const BlNum,                              // Window blind number
        Real64 const THRAY,                           // Azimuth of ray from reference point to window element (radians)
        Vector3<Real64> const &WNORM2,                // Unit vector normal to window
        DataDaylighting::ExtWinType const ExtWinType, // Exterior window type (InZoneExtWin, AdjZoneExtWin, NotInOrAdjZoneExtWin)
        int const IConst,                             // Construction counter
        Real64 const AZVIEW,                          // Azimuth of view vector in absolute coord system for glare calculation (radians)
        Vector3<Real64> const &RREF2,                 // Location of virtual reference point in absolute coordinate system
        bool const hitIntObs,                         // True iff interior obstruction hit
        bool const hitExtObs,                         // True iff ray from ref pt to ext win hits an exterior obstruction
        DataDaylighting::CalledFor const CalledFrom,  // indicate  which type of routine called this routine
        Real64 &TVISIntWin,                           // Visible transmittance of int win at COSBIntWin for light from ext win
        Real64 &TVISIntWinDisk,                       // Visible transmittance of int win at COSBIntWin for sun
        int const MapNum = 0);

    void FigureRefPointDayltgFactorsToAddIllums(EnergyPlusData &state,
                                                int const daylightCtrlNum, // Current daylighting control number
                                                int const iRefPoint,
                                                int const iHour,
                                                int &ISunPos,
                                                int const IWin,
                                                int const loopwin,
                                                int const NWX,  // Number of window elements in x direction for dayltg calc
                                                int const NWY,  // Number of window elements in y direction for dayltg calc
                                                int const ICtrl // Window control counter
    );

    void FigureMapPointDayltgFactorsToAddIllums(EnergyPlusData &state,
                                                int const MapNum,
                                                int const iMapPoint,
                                                int const iHour,
                                                int const IWin,
                                                int const loopwin,
                                                int const ICtrl // Window control counter
    );

    void GetDaylightingParametersInput(EnergyPlusData &state);

    void GetInputIlluminanceMap(EnergyPlusData &state, bool &ErrorsFound);

    void GetDaylightingControls(EnergyPlusData &state, bool &ErrorsFound);

    void GeometryTransformForDaylighting(EnergyPlusData &state);

    void GetInputDayliteRefPt(EnergyPlusData &state, bool &ErrorsFound);

    bool doesDayLightingUseDElight(EnergyPlusData &state);

    void CheckTDDsAndLightShelvesInDaylitZones(EnergyPlusData &state);

    void AssociateWindowShadingControlWithDaylighting(EnergyPlusData &state);

    void GetLightWellData(EnergyPlusData &state, bool &ErrorsFound); // If errors found in input

    int findWinShadingStatus(int const IWin);

    void DayltgGlare(EnergyPlusData &state,
                     int &IL,                  // Reference point index: 1=first ref pt, 2=second ref pt
                     Real64 &BLUM,             // Window background (surround) luminance (cd/m2)
                     Real64 &GLINDX,           // Glare index
                     int const daylightCtrlNum // Current daylighting control number
    );

    void DayltgGlareWithIntWins(EnergyPlusData &state,
                                Array1D<Real64> &GLINDX,  // Glare index
                                int const daylightCtrlNum // Current daylighting control number
    );

    void DayltgExtHorizIllum(EnergyPlusData &state,
                             Array1A<Real64> HISK, // Horizontal illuminance from sky for different sky types
                             Real64 &HISU          // Horizontal illuminance from sun for unit beam normal
    );

    void DayltgHitObstruction(EnergyPlusData &state,
                              int const IHOUR,           // Hour number
                              int const IWin,            // Window index
                              Vector3<Real64> const &R1, // Origin of ray (m)
                              Vector3<Real64> const &RN, // Unit vector along ray
                              Real64 &ObTrans            // Product of solar transmittances of exterior obstructions
    );

    void DayltgHitInteriorObstruction(EnergyPlusData &state,
                                      int const IWin,            // Window index
                                      Vector3<Real64> const &R1, // Origin of ray (m)
                                      Vector3<Real64> const &R2, // Destination of ray (m)
                                      bool &hit                  // True iff ray hits an obstruction
    );

    void DayltgHitBetWinObstruction(EnergyPlusData &state,
                                    int const IWin1,           // Surface number of origin window
                                    int const IWin2,           // Surface number of destination window
                                    Vector3<Real64> const &R1, // Origin of ray (on IWin1) (m)
                                    Vector3<Real64> const &R2, // Destination of ray (on IWin2) (m)
                                    bool &hit                  // True iff ray hits an obstruction
    );

    void initDaylighting(EnergyPlusData &state, bool const initSurfaceHeatBalancefirstTime);

    void manageDaylighting(EnergyPlusData &state);

    void DayltgInteriorIllum(EnergyPlusData &state, int const daylightCtrlNum); // Daylighting:Controls number

    void DayltgInteriorTDDIllum(EnergyPlusData &state);

    void DayltgElecLightingControl(EnergyPlusData &state);

    Real64 DayltgGlarePositionFactor(Real64 &X, // Lateral and vertical distance of luminous window element from
                                     Real64 &Y);

    void DayltgInterReflectedIllum(EnergyPlusData &state,
                                   int const ISunPos, // Sun position counter; used to avoid calculating various
                                   int const IHR,     // Hour of day
                                   int const enclNum, // Daylighting enclosure index
                                   int const IWin     // Window index
    );

    void ComplexFenestrationLuminances(EnergyPlusData &state,
                                       int const IWin,
                                       int const WinEl,
                                       int const NBasis,
                                       int const IHR,
                                       int const iRefPoint,
                                       Array2<Real64> &ElementLuminanceSky,      // sky related luminance at window element (exterior side)
                                       Array1D<Real64> &ElementLuminanceSun,     // sun related luminance at window element (exterior side),
                                       Array1D<Real64> &ElementLuminanceSunDisk, // sun related luminance at window element (exterior side),
                                       DataDaylighting::CalledFor const CalledFrom,
                                       int const MapNum = 0);

    void DayltgInterReflectedIllumComplexFenestration(EnergyPlusData &state,
                                                      int const IWin,            // Window index
                                                      int const WinEl,           // Current window element counter
                                                      int const IHR,             // Hour of day
                                                      int const daylightCtrlNum, // Daylighting control number
                                                      int const iRefPoint,       // reference point counter
                                                      DataDaylighting::CalledFor const CalledFrom,
                                                      int const MapNum = 0);

    void DayltgDirectIllumComplexFenestration(EnergyPlusData &state,
                                              int const IWin,      // Window index
                                              int const WinEl,     // Current window element counter
                                              int const IHR,       // Hour of day
                                              int const iRefPoint, // reference point index
                                              DataDaylighting::CalledFor const CalledFrom,
                                              int const MapNum = 0);

    void DayltgDirectSunDiskComplexFenestration(EnergyPlusData &state,
                                                int const iWin,  // Window index
                                                int const iHour, // Hour of day
                                                int const iRefPoint,
                                                int const NumEl,                             // Total number of window elements
                                                Real64 const AZVIEW,                         // Azimuth of view vector in absolute coord system for
                                                DataDaylighting::CalledFor const CalledFrom, // indicate  which type of routine called this routine
                                                int const MapNum = 0);

    Real64 DayltgSkyLuminance(EnergyPlusData &state,
                              int const ISky,     // Sky type: 1=clear, 2=clear turbid, 3=intermediate, 4=overcast
                              Real64 const THSKY, // Azimuth and altitude of sky element (radians)
                              Real64 const PHSKY);

    void ProfileAngle(EnergyPlusData &state,
                      int const SurfNum,                                      // Surface number
                      Vector3<Real64> const &CosDirSun,                       // Solar direction cosines
                      DataWindowEquivalentLayer::Orientation const HorOrVert, // If HORIZONTAL, calculates ProfileAngHor
                      Real64 &ProfileAng                                      // Solar profile angle (radians).
    );

    void DayltgClosestObstruction(EnergyPlusData &state,
                                  Vector3<Real64> const &RecPt,  // Point on window from which ray emanates (m)
                                  Vector3<Real64> const &RayVec, // Unit vector along ray pointing away from window (m)
                                  int &NearestHitSurfNum,        // Surface number of nearest obstruction that is hit by ray;
                                  Vector3<Real64> &NearestHitPt  // Ray's hit point on nearest obstruction (m)
    );

    void DayltgSurfaceLumFromSun(EnergyPlusData &state,
                                 int const IHR,                    // Hour number
                                 Vector3<Real64> const &Ray,       // Ray from window to reflecting surface (m)
                                 int const ReflSurfNum,            // Number of surface for which luminance is being calculated
                                 Vector3<Real64> const &ReflHitPt, // Point on ReflSurfNum for luminance calculation (m)
                                 Real64 &LumAtReflHitPtFrSun       // Luminance at ReflHitPt from beam solar reflection for unit
    );

    void DayltgInteriorMapIllum(EnergyPlusData &state);

    void ReportIllumMap(EnergyPlusData &state, int const MapNum);

    void CloseReportIllumMaps(EnergyPlusData &state);

    void CloseDFSFile(EnergyPlusData &state);

    void DayltgSetupAdjZoneListsAndPointers(EnergyPlusData &state);

    std::size_t CreateShadeDeploymentOrder(EnergyPlusData &state, int const enclNum);

    void MapShadeDeploymentOrderToLoopNumber(EnergyPlusData &state, int const enclNum);

    void DayltgInterReflIllFrIntWins(EnergyPlusData &state, int const enclNum);

    void CalcMinIntWinSolidAngs(EnergyPlusData &state);

    void CheckForGeometricTransform(EnergyPlusData &state, bool &doTransform, Real64 &OldAspectRatio, Real64 &NewAspectRatio);

    void WriteDaylightMapTitle(EnergyPlusData &state,
                               int const mapNum,
                               InputOutputFile &mapFile,
                               std::string const &mapName,
                               std::string const &environmentName,
                               int const ZoneNum,
                               std::string const &refPts,
                               Real64 const zcoord);

} // namespace DaylightingManager

struct DaylightingManagerData : BaseGlobalStruct
{

    bool CalcDayltghCoefficients_firstTime = true;
    bool getDaylightingParametersInputFlag = true;
    bool refFirstTime = true;
    bool DayltgInteriorIllum_firstTime = true; // true first time routine is called
    bool FirstTimeDaylFacCalc = true;
    bool VeryFirstTime = true;
    bool mapFirstTime = true;
    bool CheckTDDs_firstTime = true;
    bool DayltgExtHorizIllum_firstTime = true; // flag for first time thru to initialize
    bool DayltgInteriorMapIllum_FirstTimeFlag = true;
    bool ReportIllumMap_firstTime = true;
    bool SQFirstTime = true;
    bool doSkyReporting = true;
    bool CreateDFSReportFile = true;

    int TotWindowsWithDayl = 0;                          // Total number of exterior windows in all daylit zones
    Array1D<Real64> DaylIllum;                           // Daylight illuminance at reference points (lux)
    int maxNumRefPtInAnyDaylCtrl = 0;                    // The most number of reference points that any single daylighting control has
    int maxNumRefPtInAnyEncl = 0;                        // The most number of reference points that any single enclosure has
    Real64 PHSUN = 0.0;                                  // Solar altitude (radians)
    Real64 SPHSUN = 0.0;                                 // Sine of solar altitude
    Real64 CPHSUN = 0.0;                                 // Cosine of solar altitude
    Real64 THSUN = 0.0;                                  // Solar azimuth (rad) in Absolute Coordinate System (azimuth=0 along east)
    Array1D<Real64> PHSUNHR = Array1D<Real64>(24, 0.0);  // Hourly values of PHSUN
    Array1D<Real64> SPHSUNHR = Array1D<Real64>(24, 0.0); // Hourly values of the sine of PHSUN
    Array1D<Real64> CPHSUNHR = Array1D<Real64>(24, 0.0); // Hourly values of the cosine of PHSUN
    Array1D<Real64> THSUNHR = Array1D<Real64>(24, 0.0);  // Hourly values of THSUN

    // In the following I,J,K arrays:
    // I = 1 for clear sky, 2 for clear turbid, 3 for intermediate, 4 for overcast;
    // J = 1 for bare window, 2 - 12 for shaded;
    // K = sun position index.
    Array3D<Real64> EINTSK; // Sky-related portion of internally reflected illuminance
    Array2D<Real64> EINTSU; // Sun-related portion of internally reflected illuminance,
    // excluding entering beam
    Array2D<Real64> EINTSUdisk; // Sun-related portion of internally reflected illuminance
    // due to entering beam
    Array3D<Real64> WLUMSK;     // Sky-related window luminance
    Array2D<Real64> WLUMSU;     // Sun-related window luminance, excluding view of solar disk
    Array2D<Real64> WLUMSUdisk; // Sun-related window luminance, due to view of solar disk

    Array2D<Real64> GILSK = Array2D<Real64>(24, 4, 0.0); // Horizontal illuminance from sky, by sky type, for each hour of the day
    Array1D<Real64> GILSU = Array1D<Real64>(24, 0.0);    // Horizontal illuminance from sun for each hour of the day

    Array3D<Real64> EDIRSK;     // Sky-related component of direct illuminance
    Array2D<Real64> EDIRSU;     // Sun-related component of direct illuminance (excluding beam solar at ref pt)
    Array2D<Real64> EDIRSUdisk; // Sun-related component of direct illuminance due to beam solar at ref pt
    Array3D<Real64> AVWLSK;     // Sky-related average window luminance
    Array2D<Real64> AVWLSU;     // Sun-related average window luminance, excluding view of solar disk
    Array2D<Real64> AVWLSUdisk; // Sun-related average window luminance due to view of solar disk

    // Allocatable daylight factor arrays  -- are in the ZoneDaylight Structure

    Array2D<Real64> TDDTransVisBeam;
    Array3D<Real64> TDDFluxInc;
    Array3D<Real64> TDDFluxTrans;

    Array2D_int MapErrIndex;
    Array2D_int RefErrIndex;

    bool MySunIsUpFlag = false;
    bool CalcDayltgCoeffsMapPointsMySunIsUpFlag = false;

    // static variables extracted from functions
    Vector3<Real64> AR;      // Inside surface area sum for floor/wall/ceiling (m2)
    Vector3<Real64> ARH;     // Inside surface area*reflectance sum for floor/wall/ceiling (m2)
    Vector3<Real64> AP;      // Zone inside surface floor/wall/ceiling area without a selected floor/wall/ceiling (m2)
    Vector3<Real64> ARHP;    // Zone inside surface floor/wall/ceiling area*reflectance without a selected floor/wall/ceiling (m2)
    Vector3<Real64> W2;      // Second vertex of window
    Vector3<Real64> W3;      // Third vertex of window
    Vector3<Real64> W21;     // Vector from window vertex 2 to window vertex 1
    Vector3<Real64> W23;     // Vector from window vertex 2 to window vertex 3
    Vector3<Real64> RREF;    // Location of a reference point in absolute coordinate system
    Vector3<Real64> RREF2;   // Location of virtual reference point in absolute coordinate system
    Vector3<Real64> RWIN;    // Center of a window element in absolute coordinate system
    Vector3<Real64> RWIN2;   // Center of a window element for TDD:DOME (if exists) in abs coord sys
    Vector3<Real64> Ray;     // Unit vector along ray from reference point to window element
    Vector3<Real64> WNORM2;  // Unit vector normal to TDD:DOME (if exists)
    Vector3<Real64> VIEWVC;  // View vector in absolute coordinate system
    Vector3<Real64> U2;      // Second vertex of window for TDD:DOME (if exists)
    Vector3<Real64> U21;     // Vector from window vertex 2 to window vertex 1 for TDD:DOME (if exists)
    Vector3<Real64> U23;     // Vector from window vertex 2 to window vertex 3 for TDD:DOME (if exists)
    Vector3<Real64> VIEWVC2; // Virtual view vector in absolute coordinate system

    Vector3<Real64> W1;     // First vertex of window (where vertices are numbered
    Vector3<Real64> WC;     // Center point of window
    Vector3<Real64> REFWC;  // Vector from reference point to center of window
    Vector3<Real64> WNORM;  // Unit vector normal to window (pointing away from room)
    Vector3<Real64> W2REF;  // Vector from window origin to project of ref. pt. on window plane
    Vector3<Real64> REFD;   // Vector from ref pt to center of win in TDD:DIFFUSER coord sys (if exists)
    Vector3<Real64> VIEWVD; // Virtual view vector in TDD:DIFFUSER coord sys (if exists)
    Vector3<Real64> U1;     // First vertex of window for TDD:DOME (if exists)
    Vector3<Real64> U3;     // Third vertex of window for TDD:DOME (if exists)
    Vector3<Real64> RayVector;

    Vector3<Real64> HitPtIntWin; // Intersection point on an interior window for ray from ref pt to ext win (m)
    Vector3<Real64> GroundHitPt; // Coordinates of point that ray hits ground (m)
    Vector3<Real64> URay;        // Unit vector in (Phi,Theta) direction
    Vector3<Real64> ObsHitPt;    // Coordinates of hit point on an obstruction (m)

    Vector3<Real64> WNorm;      // unit vector from window (point towards outside)
    Vector3<Real64> RayNorm;    // unit vector along ray from window to reference point
    Vector3<Real64> InterPoint; // Intersection point
    Vector3<Real64> RWin;       // window element center point (same as centroid)
    Vector3<Real64> V;          // vector array

    Vector3<Real64> NearestHitPt;    // Hit point of ray on nearest obstruction
    Vector3<Real64> ReflNorm;        // Normal vector to reflecting surface
    Vector3<Real64> SunVecMir;       // Sun ray mirrored in reflecting surface
    Vector3<Real64> HitPtRefl;       // Point that ray hits reflecting surface
    Vector3<Real64> HitPtObs;        // Hit point on obstruction
    Vector3<Real64> HitPtIntWinDisk; // Intersection point on an interior window for ray from ref pt to sun (m)

    int AltSteps_last = 0;
    Array1D<Real64> cos_Phi; // cos( Phi ) table
    Array1D<Real64> sin_Phi; // sin( Phi ) table
    int AzimSteps_last = 0;
    Array1D<Real64> cos_Theta; // cos( Theta ) table
    Array1D<Real64> sin_Theta; // sin( Theta ) table

    int IConstShaded = 0; // The shaded window construction for switchable windows
    Real64 VTDark = 0.0;  // Visible transmittance (VT) of electrochromic (EC) windows in fully dark state
    Real64 VTMULT = 1.0;  // VT multiplier for EC windows

    Vector3<Real64> WinNorm; // Window outward normal unit vector
    Vector3<Real64>
        SunPrime; // Projection of sun vector onto plane (perpendicular to window plane) determined by WinNorm and vector along baseline of window
    Vector3<Real64> WinNormCrossBase;                         // Cross product of WinNorm and vector along window baseline
    Vector4<Real64> XEDIRSK;                                  // Illuminance contribution from luminance element, sky-related
    Vector4<Real64> XAVWLSK;                                  // Luminance of window element, sky-related
    Vector3<Real64> RAYCOS;                                   // Unit vector from reference point to sun
    Vector3<Real64> HP;                                       // Hit coordinates, if ray hits
    Vector3<Real64> DayltgHitObstructionHP;                   // Hit coordinates, if ray hits an obstruction
    Vector3<Real64> DayltgHitInteriorObstructionHP;           // Hit coordinates, if ray hits an obstruction
    Vector3<Real64> RN;                                       // Unit vector along ray
    Vector3<Real64> DayltgHitBetWinObstructionHP;             // Hit coordinates, if ray hits an obstruction surface (m)
    Vector3<Real64> DayltgHitBetWinObstructionRN;             // Unit vector along ray from R1 to R2
    Vector2<Real64> DFSUHR;                                   // Sun daylight factor for bare/shaded window
    Vector2<Real64> BFSUHR;                                   // Sun background luminance factor for bare/shaded window
    Vector2<Real64> SFSUHR;                                   // Sun source luminance factor for bare/shaded window
    Vector4<Real64> HorIllSky;                                // Horizontal illuminance for different sky types
    Vector4<Real64> TDDTransVisDiff;                          // Weighted diffuse visible transmittance for each sky type
    Vector3<Real64> HitPt;                                    // Hit point on an obstruction (m)
    Vector3<Real64> DayltgSurfaceLumFromSunReflNorm;          // Unit normal to reflecting surface (m)
    Vector3<Real64> DayltgSurfaceLumFromSunObsHitPt;          // Hit point on obstruction (m)
    Vector3<Real64> U;                                        // Unit vector in (PH,TH) direction
    Vector3<Real64> DayltgInterReflectedIllumNearestHitPt;    // Hit point of ray on nearest obstruction (m)
    Vector3<Real64> DayltgInterReflectedIllumObsHitPt;        // Coordinates of hit point on an obstruction (m)
    Vector3<Real64> DayltgInterReflectedIllumGroundHitPt;     // Coordinates of point that ray from window center hits the ground (m)
    Vector3<Real64> DayltgInterReflectedIllumURay;            // Unit vector in (Phi,Theta) direction
    Vector3<Real64> ComplexFenestrationLuminancesObsHitPt;    // Coordinates of hit point on an obstruction (m)
    Vector3<Real64> ComplexFenestrationLuminancesGroundHitPt; // Coordinates of point that ray from window center hits the ground (m)
    Vector4<Real64> ZSK;                                      // Sky-related and sun-related illuminance on window from sky/ground
    Vector4<Real64> FFSKTot;
    Vector4<Real64> WinLumSK;                                   // Sky related window luminance
    Vector4<Real64> EDirSky;                                    // Sky related direct illuminance
    Vector3<Real64> DayltgDirectSunDiskComplexFenestrationV;    // temporary vector
    Vector3<Real64> DayltgDirectSunDiskComplexFenestrationRWin; // Window center
    Vector2<Real64> DayltgInteriorMapIllumDFSUHR;               // Sun daylight factor for bare/shaded window
    Vector4<Real64> DayltgInteriorMapIllumHorIllSky;            // Horizontal illuminance for different sky types
    Vector3<Real64> CalcMinIntWinSolidAngsW1;                   // Window vertices
    Vector3<Real64> CalcMinIntWinSolidAngsW2;
    Vector3<Real64> CalcMinIntWinSolidAngsW3;
    Vector3<Real64> CalcMinIntWinSolidAngsWC;  // Center point of window
    Vector3<Real64> CalcMinIntWinSolidAngsW21; // Unit vectors from window vertex 2 to 1 and 2 to 3
    Vector3<Real64> CalcMinIntWinSolidAngsW23;
    Vector3<Real64> CalcMinIntWinSolidAngsRREF;  // Location of a reference point in absolute coordinate system
    Vector3<Real64> CalcMinIntWinSolidAngsRay;   // Unit vector along ray from reference point to window center
    Vector3<Real64> CalcMinIntWinSolidAngsREFWC; // Vector from reference point to center of window
    Vector3<Real64> CalcMinIntWinSolidAngsWNORM; // Unit vector normal to window (pointing away from room)

    Array2D<Real64> DayltgInteriorMapIllumDFSKHR =
        Array2D<Real64>(2, 4); // Sky daylight factor for sky type (first index), bare/shaded window (second index)
    Array2D<Real64> DayltgInteriorMapIllumBFSKHR =
        Array2D<Real64>(2, 4); // Sky background luminance factor for sky type (first index), bare/shaded window (second index)
    Array2D<Real64> DayltgInteriorMapIllumSFSKHR =
        Array2D<Real64>(2, 4); // Sky source luminance factor for sky type (first index), bare/shaded window (second index)
    Array1D<Real64> BACLUM;
    Array1D<Real64> DayltgInteriorMapIllumGLRNDX;
    Array1D<Real64> daylight_illum;
    Array1D<Real64> FLFWSU;                                            // Sun-related downgoing luminous flux, excluding entering beam
    Array1D<Real64> FLFWSUdisk;                                        // Sun-related downgoing luminous flux, due to entering beam
    Array1D<Real64> FLCWSU;                                            // Sun-related upgoing luminous flux
    Array1D<Real64> TransMult;                                         // Transmittance multiplier
    Array1D<Real64> DayltgInterReflectedIllumTransBmBmMult;            // Isolated blind beam-beam transmittance
    Array1D<Real64> TransBmBmMult;                                     // Beam-beam transmittance of isolated blind
    Array1D<Real64> TransBmBmMultRefl;                                 // As above but for beam reflected from exterior obstruction
    Array1D<Real64> PH = Array1D<Real64>(DaylightingManager::NPH);     // Altitude of sky element (radians)
    Array1D<Real64> TH = Array1D<Real64>(DaylightingManager::NTH);     // Azimuth of sky element (radians)
    Array1D<Real64> SPHCPH = Array1D<Real64>(DaylightingManager::NPH); // Sine times cosine of altitude of sky element
    Array1D<Real64> SetPnt;                                            // Illuminance setpoint at reference points (lux)
    Array1D<Real64> GLRNDX;                                            // Glare index at reference point
    Array1D<Real64> GLRNEW;                                            // New glare index at reference point
    Array2D<Real64> FLCWSK;                                            // Sky-related upgoing luminous flux
    Array2D<Real64> SkyObstructionMult =
        Array2D<Real64>(DaylightingManager::NPHMAX,
                        DaylightingManager::NTHMAX); // Ratio of obstructed to unobstructed sky diffuse at a ground point for each (TH,PH) direction
    Array2D<Real64> FLFWSK;                          // Sky-related downgoing luminous flux
    Array2D<Real64> ObTransM = Array2D<Real64>(DaylightingManager::NPHMAX, DaylightingManager::NTHMAX); // ObTrans value for each (TH,PH) direction
    Array2D<Real64> SFSKHR = Array2D<Real64>(2, 4); // Sky source luminance factor for sky type (second index), bare/shaded window (first index)
    Array2D<Real64> DFSKHR = Array2D<Real64>(2, 4); // Sky daylight factor for sky type (second index), bare/shaded window (first index)
    Array2D<Real64> BFSKHR = Array2D<Real64>(2, 4); // Sky background luminance factor for sky type (second index), bare/shaded window (first index)
    Array3D<Real64> tmpIllumFromWinAtRefPt;
    Array3D<Real64> tmpBackLumFromWinAtRefPt;
    Array3D<Real64> tmpSourceLumFromWinAtRefPt;
    Array1D_bool FirstTimeMaps;
    Array1D_bool EnvrnPrint;
    Array1D_string SavedMnDy;
    Array1D<Real64> XValue;
    Array1D<Real64> YValue;
    Array2D<Real64> IllumValue;

    Real64 tmpSWSL1 = 0.0;
    Real64 tmpSWSL2 = 0.0;
    Real64 tmpSWFactor = 0.0; // new switching factor to meet glare criteria
    Real64 tmpMult = 0.0;

    bool GlareOK = false;
    bool blnCycle = false;

    Array1D<Real64> DILLSW;         // Illuminance a ref point from a group of windows that can be switched together,
    Array1D<Real64> DILLUN;         //  and from those that aren't (lux)
    Array1D_bool previously_shaded; // array of flags to indicate that previously groups would have already shaded this window
    Array3D<Real64> WDAYIL;         // Illuminance from window at reference point (second index)
    //   for shade open/closed (first index), the number of shade deployment groups (third index)
    Array3D<Real64> WBACLU; // Background illuminance from window at reference point (second index)
    //   for shade open/closed (first index), the number of shade deployment groups (third index)
    Array2D<Real64> RDAYIL; // Illuminance from window at reference point after closing shade
    Array2D<Real64> RBACLU; // Background illuminance from window at reference point after closing shade
    Array1D<Real64> TVIS1;  // Visible transmittance at normal incidence of unswitched glazing
    Array1D<Real64> TVIS2;  // Visible transmittance at normal incidence of fully-switched glazing
    Array1D<Real64> ASETIL; // Illuminance ratio (lux)

    DaylightingManagerData()
    {
        this->cos_Phi = Array1D<Real64>(DataSurfaces::AltAngStepsForSolReflCalc / 2);    // cos( Phi ) table
        this->sin_Phi = Array1D<Real64>(DataSurfaces::AltAngStepsForSolReflCalc / 2);    // sin( Phi ) table
        this->cos_Theta = Array1D<Real64>(2 * DataSurfaces::AzimAngStepsForSolReflCalc); // cos( Theta ) table
        this->sin_Theta = Array1D<Real64>(2 * DataSurfaces::AzimAngStepsForSolReflCalc); // sin( Theta ) table
    }

    void clear_state() override
    {
        this->CalcDayltghCoefficients_firstTime = true;
        this->getDaylightingParametersInputFlag = true;
        this->refFirstTime = true;
        this->DayltgInteriorIllum_firstTime = true;
        this->FirstTimeDaylFacCalc = true;
        this->VeryFirstTime = true;
        this->mapFirstTime = true;
        this->CheckTDDs_firstTime = true;
        this->DayltgExtHorizIllum_firstTime = true;
        this->DayltgInteriorMapIllum_FirstTimeFlag = true;
        this->ReportIllumMap_firstTime = true;
        this->SQFirstTime = true;
        this->doSkyReporting = true;
        this->CreateDFSReportFile = true;
        this->TotWindowsWithDayl = 0;
        this->DaylIllum.deallocate();
        this->maxNumRefPtInAnyDaylCtrl = 0;
        this->maxNumRefPtInAnyEncl = 0;
        this->PHSUN = 0.0;
        this->SPHSUN = 0.0;
        this->CPHSUN = 0.0;
        this->THSUN = 0.0;
        this->PHSUNHR = Array1D<Real64>(24, 0.0);
        this->SPHSUNHR = Array1D<Real64>(24, 0.0);
        this->CPHSUNHR = Array1D<Real64>(24, 0.0);
        this->THSUNHR = Array1D<Real64>(24, 0.0);
        this->EINTSK.deallocate();
        this->EINTSU.deallocate();
        this->EINTSUdisk.deallocate();
        this->WLUMSK.deallocate();
        this->WLUMSU.deallocate();
        this->WLUMSUdisk.deallocate();
        this->GILSK = Array2D<Real64>(24, 4, 0.0);
        this->GILSU = Array1D<Real64>(24, 0.0);
        this->EDIRSK.deallocate();
        this->EDIRSU.deallocate();
        this->EDIRSUdisk.deallocate();
        this->AVWLSK.deallocate();
        this->AVWLSU.deallocate();
        this->AVWLSUdisk.deallocate();
        this->TDDTransVisBeam.deallocate();
        this->TDDFluxInc.deallocate();
        this->TDDFluxTrans.deallocate();
        this->MapErrIndex.deallocate();
        this->RefErrIndex.deallocate();
        this->MySunIsUpFlag = false;
        this->CalcDayltgCoeffsMapPointsMySunIsUpFlag = false;
        // seems like a reasonable initialization for the Vector variables
        this->AR = 0.0;
        this->ARH = 0.0;
        this->AP = 0.0;
        this->ARHP = 0.0;
        this->W2 = 0.0;
        this->W3 = 0.0;
        this->W21 = 0.0;
        this->W23 = 0.0;
        this->RREF = 0.0;
        this->RREF2 = 0.0;
        this->RWIN = 0.0;
        this->RWIN2 = 0.0;
        this->Ray = 0.0;
        this->WNORM2 = 0.0;
        this->VIEWVC = 0.0;
        this->U2 = 0.0;
        this->U21 = 0.0;
        this->U23 = 0.0;
        this->VIEWVC2 = 0.0;
        this->W1 = 0.0;
        this->WC = 0.0;
        this->REFWC = 0.0;
        this->WNORM = 0.0;
        this->W2REF = 0.0;
        this->REFD = 0.0;
        this->VIEWVD = 0.0;
        this->U1 = 0.0;
        this->U3 = 0.0;
        this->RayVector = 0.0;
        this->HitPtIntWin = 0.0;
        this->GroundHitPt = 0.0;
        this->URay = 0.0;
        this->ObsHitPt = 0.0;
        this->WNorm = 0.0;
        this->RayNorm = 0.0;
        this->InterPoint = 0.0;
        this->RWin = 0.0;
        this->V = 0.0;
        this->NearestHitPt = 0.0;
        this->ReflNorm = 0.0;
        this->SunVecMir = 0.0;
        this->HitPtRefl = 0.0;
        this->HitPtObs = 0.0;
        this->HitPtIntWinDisk = 0.0;

        this->AltSteps_last = 0;
        this->AzimSteps_last = 0;
        this->cos_Phi = Array1D<Real64>(DataSurfaces::AltAngStepsForSolReflCalc / 2);    // cos( Phi ) table
        this->sin_Phi = Array1D<Real64>(DataSurfaces::AltAngStepsForSolReflCalc / 2);    // sin( Phi ) table
        this->cos_Theta = Array1D<Real64>(2 * DataSurfaces::AzimAngStepsForSolReflCalc); // cos( Theta ) table
        this->sin_Theta = Array1D<Real64>(2 * DataSurfaces::AzimAngStepsForSolReflCalc); // sin( Theta ) table

        this->IConstShaded = 0;
        this->VTDark = 0.0;
        this->VTMULT = 1.0;

        this->DayltgInteriorMapIllumDFSKHR = Array2D<Real64>(2, 4);
        this->DayltgInteriorMapIllumBFSKHR = Array2D<Real64>(2, 4);
        this->DayltgInteriorMapIllumSFSKHR = Array2D<Real64>(2, 4);
        this->BACLUM.clear();
        this->DayltgInteriorMapIllumGLRNDX.clear();
        this->daylight_illum.clear();
        this->FLFWSU.clear();
        this->FLFWSUdisk.clear();
        this->FLCWSU.clear();
        this->TransMult.clear();
        this->DayltgInterReflectedIllumTransBmBmMult.clear();
        this->TransBmBmMult.clear();
        this->TransBmBmMultRefl.clear();
        this->PH = Array1D<Real64>(DaylightingManager::NPH);
        this->TH = Array1D<Real64>(DaylightingManager::NTH);
        this->SPHCPH = Array1D<Real64>(DaylightingManager::NPH);
        this->SetPnt.clear();
        this->GLRNDX.clear();
        this->GLRNEW.clear();
        this->FLCWSK.clear();
        this->SkyObstructionMult = Array2D<Real64>(DaylightingManager::NPHMAX, DaylightingManager::NTHMAX);
        this->FLFWSK.clear();
        this->ObTransM = Array2D<Real64>(DaylightingManager::NPHMAX, DaylightingManager::NTHMAX);
        this->SFSKHR = Array2D<Real64>(2, 4);
        this->DFSKHR = Array2D<Real64>(2, 4);
        this->BFSKHR = Array2D<Real64>(2, 4);
        this->tmpIllumFromWinAtRefPt.clear();
        this->tmpBackLumFromWinAtRefPt.clear();
        this->tmpSourceLumFromWinAtRefPt.clear();
        this->FirstTimeMaps.clear();
        this->EnvrnPrint.clear();
        this->SavedMnDy.clear();
        this->XValue.clear();
        this->YValue.clear();
        this->IllumValue.clear();

        this->tmpSWSL1 = 0.0;
        this->tmpSWSL2 = 0.0;
        this->tmpSWFactor = 0.0;
        this->tmpMult = 0.0;
        this->GlareOK = false;
        this->blnCycle = false;

        this->DILLSW.clear();
        this->DILLUN.clear();
        this->previously_shaded.clear();
        this->WDAYIL.clear();
        this->WBACLU.clear();
        this->RDAYIL.clear();
        this->RBACLU.clear();
        this->TVIS1.clear();
        this->TVIS2.clear();
        this->ASETIL.clear();
    }
};

} // namespace EnergyPlus

#endif
