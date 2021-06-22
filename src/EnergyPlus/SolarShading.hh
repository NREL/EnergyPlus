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

#ifndef SolarShading_hh_INCLUDED
#define SolarShading_hh_INCLUDED

// C++ Headers
#include <fstream>
#include <memory>

// ObjexxFCL Headers
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Array3D.hh>

// Penumbra Headers
#ifndef EP_NO_OPENGL
#include <penumbra/penumbra.h>
#endif

// EnergyPlus Headers
#include <EnergyPlus/DataBSDFWindow.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataVectorTypes.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/WindowEquivalentLayer.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace SolarShading {

    // Using/Aliasing
    using DataBSDFWindow::BSDFGeomDescr;
    using DataBSDFWindow::BSDFWindowGeomDescr;
    using DataVectorTypes::Vector;

    struct SurfaceErrorTracking
    {
        // Members
        int SurfIndex1; // Tracking for main error message
        int SurfIndex2; // Tracking for Overlapping Figure Name or Surface # 1
        int MiscIndex;  // Used for other pertinent information to be stored

        // Default Constructor
        SurfaceErrorTracking() : SurfIndex1(0), SurfIndex2(0), MiscIndex(0)
        {
        }
    };

    void InitSolarCalculations(EnergyPlusData &state);

    void GetShadowingInput(EnergyPlusData &state);

    void AllocateModuleArrays(EnergyPlusData &state);

    void AnisoSkyViewFactors(EnergyPlusData &state);

    void CHKBKS(EnergyPlusData &state,
                int const NBS, // Surface Number of the potential back surface
                int const NRS  // Surface Number of the potential shadow receiving surface
    );

    void CHKGSS(EnergyPlusData &state,
                int const NRS,     // Surface number of the potential shadow receiving surface
                int const NSS,     // Surface number of the potential shadow casting surface
                Real64 const ZMIN, // Lowest point of the receiving surface
                bool &CannotShade  // TRUE if shadow casting surface cannot shade receiving surface.
    );

    void CHKSBS(EnergyPlusData &state,
                int const HTS,   // Heat transfer surface number of the general receiving surf
                int const GRSNR, // Surface number of general receiving surface
                int const SBSNR  // Surface number of subsurface
    );

    bool polygon_contains_point(int const nsides,            // number of sides (vertices)
                                Array1D<Vector> &polygon_3d, // points of polygon
                                Vector const &point_3d,      // point to be tested
                                bool const ignorex,
                                bool const ignorey,
                                bool const ignorez);

    void ComputeIntSolarAbsorpFactors(EnergyPlusData &state);

    void CLIP(EnergyPlusData &state, int const NVT, Array1D<Real64> &XVT, Array1D<Real64> &YVT, Array1D<Real64> &ZVT);

    void CTRANS(EnergyPlusData &state,
                int const NS,         // Surface number whose vertex coordinates are being transformed
                int const NGRS,       // Base surface number for surface NS
                int &NVT,             // Number of vertices for surface NS
                Array1D<Real64> &XVT, // XYZ coordinates of vertices of NS in plane of NGRS
                Array1D<Real64> &YVT,
                Array1D<Real64> &ZVT);

    void HTRANS(EnergyPlusData &state,
                int const I,          // Mode selector: 0 - Compute H.C. of sides
                int const NS,         // Figure Number
                int const NumVertices // Number of vertices
    );

    void HTRANS0(EnergyPlusData &state,
                 int const NS,         // Figure Number
                 int const NumVertices // Number of vertices
    );

    void HTRANS1(EnergyPlusData &state,
                 int const NS,         // Figure Number
                 int const NumVertices // Number of vertices
    );

    void INCLOS(EnergyPlusData &state,
                int const N1,            // Figure number of figure 1
                int const N1NumVert,     // Number of vertices of figure 1
                int const N2,            // Figure number of figure 2
                int const N2NumVert,     // Number of vertices of figure 2
                int &NumVerticesOverlap, // Number of vertices which overlap
                int &NIN                 // Number of vertices of figure 1 within figure 2
    );

    void INTCPT(EnergyPlusData &state,
                int const NV1, // Number of vertices of figure NS1
                int const NV2, // Number of vertices of figure NS2
                int &NV3,      // Number of vertices of figure NS3
                int const NS1, // Number of the figure being overlapped
                int const NS2  // Number of the figure doing overlapping
    );

    void CLIPPOLY(EnergyPlusData &state,
                  int const NS1, // Figure number of figure 1 (The subject polygon)
                  int const NS2, // Figure number of figure 2 (The clipping polygon)
                  int const NV1, // Number of vertices of figure 1
                  int const NV2, // Number of vertices of figure 2
                  int &NV3       // Number of vertices of figure 3
    );

    void MULTOL(EnergyPlusData &state,
                int const NNN,   // argument
                int const LOC0,  // Location in the homogeneous coordinate array
                int const NRFIGS // Number of figures overlapped
    );

    void ORDER(EnergyPlusData &state,
               int const NV3, // Number of vertices of figure NS3
               int const NS3  // Location to place results of overlap
    );

    void DeterminePolygonOverlap(EnergyPlusData &state,
                                 int const NS1, // Number of the figure being overlapped
                                 int const NS2, // Number of the figure doing overlapping
                                 int const NS3  // Location to place results of overlap
    );

    void CalcPerSolarBeam(EnergyPlusData &state,
                          Real64 const AvgEqOfTime,       // Average value of Equation of Time for period
                          Real64 const AvgSinSolarDeclin, // Average value of Sine of Solar Declination for period
                          Real64 const AvgCosSolarDeclin  // Average value of Cosine of Solar Declination for period
    );

    void FigureSunCosines(EnergyPlusData &state,
                          int const iHour,
                          int const iTimeStep,
                          Real64 const EqOfTime,       // value of Equation of Time for period
                          Real64 const SinSolarDeclin, // value of Sine of Solar Declination for period
                          Real64 const CosSolarDeclin  // value of Cosine of Solar Declination for period
    );

    void FigureSolarBeamAtTimestep(EnergyPlusData &state, int const iHour, int const iTimeStep);

    void DetermineShadowingCombinations(EnergyPlusData &state);

    void SHADOW(EnergyPlusData &state,
                int const iHour, // Hour index
                int const TS     // Time Step
    );

    void SHDBKS(EnergyPlusData &state,
                int const NGRS, // Number of the general receiving surface
                int const CurSurf,
                int const NBKS, // Number of back surfaces
                int const HTS   // Heat transfer surface number of the general receiving surf
    );

    void SHDGSS(EnergyPlusData &state,
                int const NGRS,
                int const iHour,   // Hour Counter
                int const TS,      // TimeStep
                int const CurSurf, // Current Surface
                int const NGSS,    // Number of general shadowing surfaces
                int const HTS      // Heat transfer surface number of the general receiving surf
    );

    void CalcInteriorSolarOverlaps(EnergyPlusData &state,
                                   int const iHour, // Hour Index
                                   int const NBKS,  // Number of back surfaces associated with this GRSNR (in general, only
                                   int const HTSS,  // Surface number of the subsurface (exterior window)
                                   int const GRSNR, // General receiving surface number (base surface of the exterior window)
                                   int const TS     // Time step Index
    );

    void CalcInteriorSolarDistribution(EnergyPlusData &state);

    void CalcAbsorbedOnExteriorOpaqueSurfaces(EnergyPlusData &state);

    void CalcInteriorSolarDistributionWCESimple(EnergyPlusData &state);

    int WindowScheduledSolarAbs(EnergyPlusData &state,
                                int const SurfNum, // Surface number
                                int const ConstNum // Construction number
    );

    int SurfaceScheduledSolarInc(EnergyPlusData &state,
                                 int const SurfNum, // Surface number
                                 int const ConstNum // Construction number
    );

    void PerformSolarCalculations(EnergyPlusData &state);

    void SHDRVL(EnergyPlusData &state,
                int const HTSS,  // Heat transfer surface number of the subsurface
                int const SBSNR, // Subsurface number
                int const Hour,
                int const TS);

    void SHDSBS(EnergyPlusData &state,
                int const iHour, // Hour Index
                int const CurSurf,
                int const NBKS, // Number of back surfaces
                int const NSBS, // Number of subsurfaces
                int const HTS,  // Heat transfer surface number of the general receiving surf
                int const TS    // Time step Index
    );

    void SUN3(int const JulianDayOfYear,      // Julian Day Of Year
              Real64 &SineOfSolarDeclination, // Sine of Solar Declination
              Real64 &EquationOfTime          // Equation of Time (Degrees)
    );

    void SUN4(EnergyPlusData &state,
              Real64 const CurrentTime,    // Time to use in shadowing calculations
              Real64 const EqOfTime,       // Equation of time for current day
              Real64 const SinSolarDeclin, // Sine of the Solar declination (current day)
              Real64 const CosSolarDeclin  // Cosine of the Solar declination (current day)
    );

    void WindowShadingManager(EnergyPlusData &state);

    void CheckGlazingShadingStatusChange(EnergyPlusData &state);

    DataSurfaces::WinShadingType findValueInEnumeration(Real64 controlValue);

    int selectActiveWindowShadingControlIndex(EnergyPlusData &state, int curSurface);

    void WindowGapAirflowControl(EnergyPlusData &state);

    void SkyDifSolarShading(EnergyPlusData &state);

    void CalcWindowProfileAngles(EnergyPlusData &state);

    void CalcFrameDividerShadow(EnergyPlusData &state,
                                int const SurfNum,  // Surface number
                                int const FrDivNum, // Frame/divider number
                                int const HourNum   // Hour number
    );

    void CalcBeamSolarOnWinRevealSurface(EnergyPlusData &state);

    void ReportSurfaceShading(EnergyPlusData &state);

    void ReportSurfaceErrors(EnergyPlusData &state);

    void ComputeWinShadeAbsorpFactors(EnergyPlusData &state);

    void CalcWinTransDifSolInitialDistribution(EnergyPlusData &state);

    void CalcInteriorWinTransDifSolInitialDistribution(
        EnergyPlusData &state,
        int const IntWinEnclosureNum,     // Interior Window Enclosure index number
        int const IntWinSurfNum,          // Interior Window Surface number
        Real64 const IntWinDifSolarTransW // Diffuse Solar transmitted through Interior Window IntWinSurfNum from adjacent enclosure [W]
    );

    void CalcComplexWindowOverlap(EnergyPlusData &state,
                                  BSDFGeomDescr &Geom,               // State Geometry
                                  BSDFWindowGeomDescr const &Window, // Window Geometry
                                  int const ISurf                    // Surface number of the complex fenestration
    );

    void TimestepInitComplexFenestration(EnergyPlusData &state);

} // namespace SolarShading

struct SolarShadingData : BaseGlobalStruct
{

    Real64 const SmallIncrement = 1.0e-10; // Small increment added for shading/sunlit area calculations.
    Real64 const HCMULT = 100000.0;        // Multiplier used to change meters to .01 millimeters for homogeneous coordinates.
                                    // Homogeneous Coordinates are represented in integers (64 bit). This changes the surface coordinates from meters
                                    // to .01 millimeters -- making that the resolution for shadowing, polygon clipping, etc.
    Real64 const sqHCMULT = (HCMULT * HCMULT);        // Square of HCMult used in Homogeneous coordinates
    Real64 const sqHCMULT_fac = (0.5 / sqHCMULT);     // ( 0.5 / sqHCMULT ) factor
    Real64 const kHCMULT = (1.0 / (HCMULT * HCMULT)); // half of inverse square of HCMult used in Homogeneous coordinates

    // Parameters for use with the variable OverlapStatus...
    int const NoOverlap = 1;
    int const FirstSurfWithinSecond = 2;
    int const SecondSurfWithinFirst = 3;
    int const PartialOverlap = 4;
    int const TooManyVertices = 5;
    int const TooManyFigures = 6;
    Array1D_string const cOverLapStatus;
    int MaxHCV = 15;             // Maximum number of HC vertices
                                 // (needs to be based on maxnumvertices)
    int MaxHCS = 15000;          // 200      ! Maximum number of HC surfaces (was 56)
                                 // Following are initially set in AllocateModuleArrays
    int MAXHCArrayBounds = 0;    // Bounds based on Max Number of Vertices in surfaces
    int MAXHCArrayIncrement = 0; // Increment based on Max Number of Vertices in surfaces
                                 // The following variable should be re-engineered to lower in module hierarchy but need more analysis
    int NVS;                     // Number of vertices of the shadow/clipped surface
    int NumVertInShadowOrClippedSurface;
    int CurrentSurfaceBeingShadowed;
    int CurrentShadowingSurface;
    int OverlapStatus; // Results of overlap calculation:
                       // 1=No overlap; 2=NS1 completely within NS2
                       // 3=NS2 completely within NS1; 4=Partial overlap

    Array1D<Real64> CTHETA;         // Cosine of angle of incidence of sun's rays on surface NS
    int FBKSHC;                     // HC location of first back surface
    int FGSSHC;                     // HC location of first general shadowing surface
    int FINSHC;                     // HC location of first back surface overlap
    int FRVLHC;                     // HC location of first reveal surface
    int FSBSHC;                     // HC location of first subsurface
    int LOCHCA = 0;                 // Location of highest data in the HC arrays
    int NBKSHC;                     // Number of back surfaces in the HC arrays
    int NGSSHC;                     // Number of general shadowing surfaces in the HC arrays
    int NINSHC;                     // Number of back surface overlaps in the HC arrays
    int NRVLHC;                     // Number of reveal surfaces in HC array
    int NSBSHC;                     // Number of subsurfaces in the HC arrays
    bool CalcSkyDifShading;         // True when sky diffuse solar shading is
    int ShadowingCalcFrequency = 0; // Frequency for Shadowing Calculations
    int ShadowingDaysLeft = 0;      // Days left in current shadowing period

    Array1D_int HCNS;   // Surface number of back surface HC figures
    Array1D_int HCNV;   // Number of vertices of each HC figure
    Array2D<Int64> HCA; // 'A' homogeneous coordinates of sides
    Array2D<Int64> HCB; // 'B' homogeneous coordinates of sides
    Array2D<Int64> HCC; // 'C' homogeneous coordinates of sides
    Array2D<Int64> HCX; // 'X' homogeneous coordinates of vertices of figure.
    Array2D<Int64> HCY; // 'Y' homogeneous coordinates of vertices of figure.
    Array3D_int WindowRevealStatus;
    Array1D<Real64> HCAREA; // Area of each HC figure.  Sign Convention:  Base Surface
                            // - Positive, Shadow - Negative, Overlap between two shadows
                            // - positive, etc., so that sum of HC areas=base sunlit area
    Array1D<Real64> HCT;    // Transmittance of each HC figure
    Array1D<Real64> ISABSF; // For simple interior solar distribution (in which all beam
                            // radiation entering zone is assumed to strike the floor),
                            // fraction of beam radiation absorbed by each floor surface
    Array1D<Real64> SAREA;  // Sunlit area of heat transfer surface HTS
                            // Excludes multiplier for windows
                            // Shadowing combinations data structure...See ShadowingCombinations type
    int NumTooManyFigures = 0;
    int NumTooManyVertices = 0;
    int NumBaseSubSurround = 0;
    Array1D<Real64> SUNCOS;   // Direction cosines of solar position
    Real64 XShadowProjection; // X projection of a shadow (formerly called C)
    Real64 YShadowProjection; // Y projection of a shadow (formerly called S)
    Array1D<Real64> XTEMP;    // Temporary 'X' values for HC vertices of the overlap
    Array1D<Real64> XVC;      // X-vertices of the clipped figure
    Array1D<Real64> XVS;      // X-vertices of the shadow
    Array1D<Real64> YTEMP;    // Temporary 'Y' values for HC vertices of the overlap
    Array1D<Real64> YVC;      // Y-vertices of the clipped figure
    Array1D<Real64> YVS;      // Y-vertices of the shadow
    Array1D<Real64> ZVC;      // Z-vertices of the clipped figure
                              // Used in Sutherland Hodman poly clipping
    Array1D<Real64> ATEMP;    // Temporary 'A' values for HC vertices of the overlap
    Array1D<Real64> BTEMP;    // Temporary 'B' values for HC vertices of the overlap
    Array1D<Real64> CTEMP;    // Temporary 'C' values for HC vertices of the overlap
    Array1D<Real64> XTEMP1;   // Temporary 'X' values for HC vertices of the overlap
    Array1D<Real64> YTEMP1;   // Temporary 'Y' values for HC vertices of the overlap
    int maxNumberOfFigures = 0;

#ifdef EP_NO_OPENGL
    bool penumbra = false;
#else
    std::unique_ptr<Pumbra::Penumbra> penumbra = nullptr;
#endif

    bool GetInputFlag = true;
    bool firstTime = true;
    bool debugging = false;
    std::vector<unsigned> penumbraIDs;

    bool InitComplexOnce = true;
    bool ShadowOneTimeFlag = true;
    bool CHKSBSOneTimeFlag = true;
    bool ORDERFirstTimeFlag = true;
    bool TooManyFiguresMessage = false;
    bool TooManyVerticesMessage = false;
    bool SHDBKSOneTimeFlag = true;
    bool SHDGSSOneTimeFlag = true;

    Array1D<SolarShading::SurfaceErrorTracking> TrackTooManyFigures;
    Array1D<SolarShading::SurfaceErrorTracking> TrackTooManyVertices;
    Array1D<SolarShading::SurfaceErrorTracking> TrackBaseSubSurround;

    Real64 TolValue = 0.0003;
    Array1D<Real64> XVT;   // X Vertices of
    Array1D<Real64> YVT;   // Y vertices of
    Array1D<Real64> ZVT;   // Z vertices of
    Array1D<Real64> SLOPE; // Slopes from left-most vertex to others.
    int MaxGSS = 50;       // Current Max for GSS array
    int MaxBKS = 50;       // Current Max for BKS array
    int MaxSBS = 50;       // Current Max for SBS array
    int MaxDim = 0;
    Array1D<Real64> XVrt;  // X Vertices of Shadows
    Array1D<Real64> YVrt;  // Y vertices of Shadows
    Array1D<Real64> ZVrt;  // Z vertices of Shadows
    Array1D<Real64> XVrtx; // X,Y,Z coordinates of vertices of
    Array1D<Real64> YVrtx; // back surfaces projected into system
    Array1D<Real64> ZVrtx; // relative to receiving surface
    Array1D<Real64> XVert;
    Array1D<Real64> YVert;
    Array1D<Real64> ZVert;
    Array1D<Real64> AbsBeamWin;                                                               // Glass layer beam solar absorptance of a window
    Array1D<Real64> AbsBeamWinEQL = Array1D<Real64>(DataWindowEquivalentLayer::CFSMAXNL + 1); // layers beam solar absorptance of a window
    Array1D<Real64>
        ExtBeamAbsByShadFac; // Factor for exterior beam radiation absorbed by shade (1/m2) (absorbed radation = beam incident * ExtBeamAbsByShad
    Array1D<Real64> IntBeamAbsByShadFac; // Like ExtBeamAbsByShadFac, but for interior beam radiation.
    Array1D<Real64>
        WinTransBmSolar; // Factor for exterior beam solar transmitted through window, or window plus shade, into zone at current time (m2)
    Array1D<Real64>
        WinTransDifSolar; // Factor for exterior diffuse solar transmitted through window, or window plus shade, into zone at current time (m2)
    Array1D<Real64> WinTransDifSolarGnd; // Factor for exterior ground diffuse solar transmitted through window with horizontally-slatted blind into
                                         // zone at current time (m2)
    Array1D<Real64> WinTransDifSolarSky; // Factor for exterior sky diffuse solar transmitted through window with horizontally-slatted blind into zone
                                         // at current time (m2)
    Array2D<Real64> AbsSolBeamEQL =
        Array2D<Real64>(2, DataWindowEquivalentLayer::CFSMAXNL + 1); // absorbed exterior beam radiation by layers fraction
    Array2D<Real64> AbsSolDiffEQL =
        Array2D<Real64>(2, DataWindowEquivalentLayer::CFSMAXNL + 1); // absorbed exterior diffuse radiation by layers fraction
    Array2D<Real64> AbsSolBeamBackEQL =
        Array2D<Real64>(2, DataWindowEquivalentLayer::CFSMAXNL + 1); // absorbed interior beam radiation by layers fraction from back
    Array1D<Real64>
        WinTransBmBmSolar; // Factor for exterior beam to beam solar transmitted through window, or window plus shade, into zone at current time (m2)
    Array1D<Real64> WinTransBmDifSolar; // Factor for exterior beam to diffuse solar transmitted through window, or window plus shade, into zone at
                                        // current time (m2)
    Real64 ThetaBig = 0.0;              // Larger of ThetaBlock1 and ThetaBlock2     //Autodesk Used uninitialized in some runs
    Real64 ThetaSmall = 0.0;            // Smaller of ThetaBlock1 and ThetaBlock2 //Autodesk Used uninitialized in some runs
    Real64 ThetaMin = 0.0;              // Minimum allowed slat angle, resp. (rad)  //Autodesk Used uninitialized in some runs
    Real64 ThetaMax = 0.0;              // Maximum allowed slat angle, resp. (rad)  //Autodesk Used uninitialized in some runs
    Array1D<Real64> XVertex;            // X,Y,Z coordinates of vertices of
    Array1D<Real64> YVertex;            // back surfaces projected into system
    Array1D<Real64> ZVertex;            // relative to receiving surface
    std::vector<Real64> sin_Phi;
    std::vector<Real64> cos_Phi;
    std::vector<Real64> sin_Theta;
    std::vector<Real64> cos_Theta;
    std::unique_ptr<std::iostream> shd_stream; // Shading file stream

    void clear_state() override
    {
        this->MaxHCV = 15;
        this->MaxHCS = 1500;
        this->MAXHCArrayBounds = 0;
        this->MAXHCArrayIncrement = 0;
        this->NVS = 0;
        this->NumVertInShadowOrClippedSurface = 0;
        this->CurrentSurfaceBeingShadowed = 0;
        this->CurrentShadowingSurface = 0;
        this->OverlapStatus = 0;
        this->CTHETA.deallocate();
        this->FBKSHC = 0;
        this->FGSSHC = 0;
        this->FINSHC = 0;
        this->FRVLHC = 0;
        this->FSBSHC = 0;
        this->LOCHCA = 0;
        this->NBKSHC = 0;
        this->NGSSHC = 0;
        this->NINSHC = 0;
        this->NRVLHC = 0;
        this->NSBSHC = 0;
        this->CalcSkyDifShading = false;
        this->ShadowingCalcFrequency = 0; // Frequency for Shadowing Calculations
        this->ShadowingDaysLeft = 0;      // Days left in current shadowing period
        this->debugging = false;
        this->GetInputFlag = true;
        this->firstTime = true;
        this->HCNS.deallocate();
        this->HCNV.deallocate();
        this->HCA.deallocate();
        this->HCB.deallocate();
        this->HCC.deallocate();
        this->HCX.deallocate();
        this->HCY.deallocate();
        this->WindowRevealStatus.deallocate();
        this->HCAREA.deallocate();
        this->HCT.deallocate();
        this->ISABSF.deallocate();
        this->SAREA.deallocate();
        this->NumTooManyFigures = 0;
        this->NumTooManyVertices = 0;
        this->NumBaseSubSurround = 0;
        this->XShadowProjection = 0.0;
        this->YShadowProjection = 0.0;
        this->XTEMP.deallocate();
        this->XVC.deallocate();
        this->XVS.deallocate();
        this->YTEMP.deallocate();
        this->YVC.deallocate();
        this->YVS.deallocate();
        this->ZVC.deallocate();
        this->ATEMP.deallocate();
        this->BTEMP.deallocate();
        this->CTEMP.deallocate();
        this->XTEMP1.deallocate();
        this->YTEMP1.deallocate();
        this->maxNumberOfFigures = 0;
        this->TrackTooManyFigures.deallocate();
        this->TrackTooManyVertices.deallocate();
        this->TrackBaseSubSurround.deallocate();
        this->ISABSF.deallocate();
        this->InitComplexOnce = true;
        this->ShadowOneTimeFlag = true;
        this->CHKSBSOneTimeFlag = true;
        this->ORDERFirstTimeFlag = true;
        this->TooManyFiguresMessage = false;
        this->TooManyVerticesMessage = false;
        this->SHDBKSOneTimeFlag = true;
        this->SHDGSSOneTimeFlag = true;
        this->TolValue = 0.0003;
        this->XVT.deallocate();
        this->YVT.deallocate();
        this->ZVT.deallocate();
        this->SLOPE.deallocate();
        this->MaxGSS = 50;
        this->MaxBKS = 50;
        this->MaxSBS = 50;
        this->MaxDim = 0;
        this->XVrt.deallocate();
        this->YVrt.deallocate();
        this->ZVrt.deallocate();
        this->XVrtx.deallocate();
        this->YVrtx.deallocate();
        this->ZVrtx.deallocate();
        this->XVert.deallocate();
        this->YVert.deallocate();
        this->ZVert.deallocate();
        this->AbsBeamWin.deallocate();
        this->AbsBeamWinEQL = Array1D<Real64>(DataWindowEquivalentLayer::CFSMAXNL + 1);
        this->ExtBeamAbsByShadFac.deallocate();
        this->IntBeamAbsByShadFac.deallocate();
        this->WinTransBmSolar.deallocate();
        this->WinTransDifSolar.deallocate();
        this->WinTransDifSolarGnd.deallocate();
        this->WinTransDifSolarSky.deallocate();
        this->AbsSolBeamEQL = Array2D<Real64>(2, DataWindowEquivalentLayer::CFSMAXNL + 1);
        this->AbsSolDiffEQL = Array2D<Real64>(2, DataWindowEquivalentLayer::CFSMAXNL + 1);
        this->AbsSolBeamBackEQL = Array2D<Real64>(2, DataWindowEquivalentLayer::CFSMAXNL + 1);
        this->WinTransBmBmSolar.deallocate();
        this->WinTransBmDifSolar.deallocate();
        this->ThetaBig = 0.0;
        this->ThetaSmall = 0.0;
        this->ThetaMin = 0.0;
        this->ThetaMax = 0.0;
        this->XVertex.deallocate();
        this->YVertex.deallocate();
        this->ZVertex.deallocate();
        this->sin_Phi.clear();
        this->cos_Phi.clear();
        this->sin_Theta.clear();
        this->cos_Theta.clear();
        this->shd_stream.reset();
    }

    // Default Constructor
    SolarShadingData()
        : cOverLapStatus(6, {"No-Overlap", "1st-Surf-within-2nd", "2nd-Surf-within-1st", "Partial-Overlap", "Too-Many-Vertices", "Too-Many-Figures"}),
          SUNCOS(3)
    {
    }
};
} // namespace EnergyPlus

#endif
