// EnergyPlus, Copyright (c) 1996-2020, The Board of Trustees of the University of Illinois,
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

    extern std::unique_ptr<std::iostream> os; // Shading file stream

    // Types

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

    void AnisoSkyViewFactors();

    void CHKBKS(int const NBS, // Surface Number of the potential back surface
                int const NRS  // Surface Number of the potential shadow receiving surface
    );

    void CHKGSS(int const NRS,     // Surface number of the potential shadow receiving surface
                int const NSS,     // Surface number of the potential shadow casting surface
                Real64 const ZMIN, // Lowest point of the receiving surface
                bool &CannotShade  // TRUE if shadow casting surface cannot shade receiving surface.
    );

    void CHKSBS(EnergyPlusData &state, int const HTS,   // Heat transfer surface number of the general receiving surf
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

    void CTRANS(int const NS,         // Surface number whose vertex coordinates are being transformed
                int const NGRS,       // Base surface number for surface NS
                int &NVT,             // Number of vertices for surface NS
                Array1D<Real64> &XVT, // XYZ coordinates of vertices of NS in plane of NGRS
                Array1D<Real64> &YVT,
                Array1D<Real64> &ZVT);

    void HTRANS(EnergyPlusData &state, int const I,          // Mode selector: 0 - Compute H.C. of sides
                int const NS,         // Figure Number
                int const NumVertices // Number of vertices
    );

    void HTRANS0(EnergyPlusData &state, int const NS,         // Figure Number
                 int const NumVertices // Number of vertices
    );

    void HTRANS1(EnergyPlusData &state, int const NS,         // Figure Number
                 int const NumVertices // Number of vertices
    );

    void INCLOS(EnergyPlusData &state, int const N1,            // Figure number of figure 1
                int const N1NumVert,     // Number of vertices of figure 1
                int const N2,            // Figure number of figure 2
                int const N2NumVert,     // Number of vertices of figure 2
                int &NumVerticesOverlap, // Number of vertices which overlap
                int &NIN                 // Number of vertices of figure 1 within figure 2
    );

    void INTCPT(EnergyPlusData &state, int const NV1, // Number of vertices of figure NS1
                int const NV2, // Number of vertices of figure NS2
                int &NV3,      // Number of vertices of figure NS3
                int const NS1, // Number of the figure being overlapped
                int const NS2  // Number of the figure doing overlapping
    );

    void CLIPPOLY(EnergyPlusData &state, int const NS1, // Figure number of figure 1 (The subject polygon)
                  int const NS2, // Figure number of figure 2 (The clipping polygon)
                  int const NV1, // Number of vertices of figure 1
                  int const NV2, // Number of vertices of figure 2
                  int &NV3       // Number of vertices of figure 3
    );

    void MULTOL(EnergyPlusData &state, int const NNN,   // argument
                int const LOC0,  // Location in the homogeneous coordinate array
                int const NRFIGS // Number of figures overlapped
    );

    void ORDER(EnergyPlusData &state, int const NV3, // Number of vertices of figure NS3
               int const NS3  // Location to place results of overlap
    );

    void DeterminePolygonOverlap(EnergyPlusData &state, int const NS1, // Number of the figure being overlapped
                                 int const NS2, // Number of the figure doing overlapping
                                 int const NS3  // Location to place results of overlap
    );

    void CalcPerSolarBeam(EnergyPlusData &state,
                          Real64 const AvgEqOfTime,       // Average value of Equation of Time for period
                          Real64 const AvgSinSolarDeclin, // Average value of Sine of Solar Declination for period
                          Real64 const AvgCosSolarDeclin  // Average value of Cosine of Solar Declination for period
    );

    void FigureSunCosines(EnergyPlusData &state, int const iHour,
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

    void SHDBKS(EnergyPlusData &state, int const NGRS, // Number of the general receiving surface
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

    void CalcInteriorSolarOverlaps(EnergyPlusData &state, int const iHour, // Hour Index
                                   int const NBKS,  // Number of back surfaces associated with this GRSNR (in general, only
                                   int const HTSS,  // Surface number of the subsurface (exterior window)
                                   int const GRSNR, // General receiving surface number (base surface of the exterior window)
                                   int const TS     // Time step Index
    );

    void CalcInteriorSolarDistribution(EnergyPlusData &state);

    void CalcAbsorbedOnExteriorOpaqueSurfaces(EnergyPlusData &state);

    void CalcInteriorSolarDistributionWCE(EnergyPlusData &state);

    void CalcInteriorSolarDistributionWCESimple(EnergyPlusData &state);

    int WindowScheduledSolarAbs(int const SurfNum, // Surface number
                                int const ConstNum // Construction number
    );

    int SurfaceScheduledSolarInc(int const SurfNum, // Surface number
                                 int const ConstNum // Construction number
    );

    void PerformSolarCalculations(EnergyPlusData &state);

    void SHDRVL(EnergyPlusData &state, int const HTSS,  // Heat transfer surface number of the subsurface
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

    void SUN4(EnergyPlusData &state, Real64 const CurrentTime,    // Time to use in shadowing calculations
              Real64 const EqOfTime,       // Equation of time for current day
              Real64 const SinSolarDeclin, // Sine of the Solar declination (current day)
              Real64 const CosSolarDeclin  // Cosine of the Solar declination (current day)
    );

    void WindowShadingManager(EnergyPlusData &state);

    int selectActiveWindowShadingControlIndex(int curSurface);

    void WindowGapAirflowControl();

    void SkyDifSolarShading(EnergyPlusData &state);

    void CalcWindowProfileAngles();

    void CalcFrameDividerShadow(EnergyPlusData &state, int const SurfNum,  // Surface number
                                int const FrDivNum, // Frame/divider number
                                int const HourNum   // Hour number
    );

    void CalcBeamSolarOnWinRevealSurface(EnergyPlusData &state);

    void ReportSurfaceShading();

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

struct SolarShadingData : BaseGlobalStruct {

    Real64 const SmallIncrement = 1.0e-10; // Small increment added for shading/sunlit area calculations.
    Real64 const HCMULT = 100000.0;        // Multiplier used to change meters to .01 millimeters for homogeneous coordinates.
                                          // Homogeneous Coordinates are represented in integers (64 bit). This changes the surface coordinates from meters
                                          // to .01 millimeters -- making that the resolution for shadowing, polygon clipping, etc.
    Real64 const sqHCMULT = (HCMULT *HCMULT);         // Square of HCMult used in Homogeneous coordinates
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
    int MaxHCV = 15; // Maximum number of HC vertices
                    // (needs to be based on maxnumvertices)
    int MaxHCS = 15000; // 200      ! Maximum number of HC surfaces (was 56)
                       // Following are initially set in AllocateModuleArrays
    int MAXHCArrayBounds = 0;    // Bounds based on Max Number of Vertices in surfaces
    int MAXHCArrayIncrement = 0; // Increment based on Max Number of Vertices in surfaces
                                // The following variable should be re-engineered to lower in module hierarchy but need more analysis
    int NVS; // Number of vertices of the shadow/clipped surface
    int NumVertInShadowOrClippedSurface;
    int CurrentSurfaceBeingShadowed;
    int CurrentShadowingSurface;
    int OverlapStatus; // Results of overlap calculation:
                       // 1=No overlap; 2=NS1 completely within NS2
                       // 3=NS2 completely within NS1; 4=Partial overlap

    Array1D<Real64> CTHETA;        // Cosine of angle of incidence of sun's rays on surface NS
    int FBKSHC;                    // HC location of first back surface
    int FGSSHC;                    // HC location of first general shadowing surface
    int FINSHC;                    // HC location of first back surface overlap
    int FRVLHC;                    // HC location of first reveal surface
    int FSBSHC;                    // HC location of first subsurface
    int LOCHCA = 0;                 // Location of highest data in the HC arrays
    int NBKSHC;                    // Number of back surfaces in the HC arrays
    int NGSSHC;                    // Number of general shadowing surfaces in the HC arrays
    int NINSHC;                    // Number of back surface overlaps in the HC arrays
    int NRVLHC;                    // Number of reveal surfaces in HC array
    int NSBSHC;                    // Number of subsurfaces in the HC arrays
    bool CalcSkyDifShading;        // True when sky diffuse solar shading is
    int ShadowingCalcFrequency = 0; // Frequency for Shadowing Calculations
    int ShadowingDaysLeft = 0;      // Days left in current shadowing period

    Array1D_int HCNS;         // Surface number of back surface HC figures
    Array1D_int HCNV;         // Number of vertices of each HC figure
    Array2D<Int64> HCA;       // 'A' homogeneous coordinates of sides
    Array2D<Int64> HCB;       // 'B' homogeneous coordinates of sides
    Array2D<Int64> HCC;       // 'C' homogeneous coordinates of sides
    Array2D<Int64> HCX;       // 'X' homogeneous coordinates of vertices of figure.
    Array2D<Int64> HCY;       // 'Y' homogeneous coordinates of vertices of figure.
    Array3D_int WindowRevealStatus;
    Array1D<Real64> HCAREA; // Area of each HC figure.  Sign Convention:  Base Surface
                            // - Positive, Shadow - Negative, Overlap between two shadows
                            // - positive, etc., so that sum of HC areas=base sunlit area
    Array1D<Real64> HCT;    // Transmittance of each HC figure
    Array1D<Real64> ISABSF; // For simple interior solar distribution (in which all beam
                            // radiation entering zone is assumed to strike the floor),
                            // fraction of beam radiation absorbed by each floor surface
    Array1D<Real64> SAREA; // Sunlit area of heat transfer surface HTS
                           // Excludes multiplier for windows
                           // Shadowing combinations data structure...See ShadowingCombinations type
    int NumTooManyFigures = 0;
    int NumTooManyVertices = 0;
    int NumBaseSubSurround = 0;
    Array1D<Real64> SUNCOS; // Direction cosines of solar position
    Real64 XShadowProjection;  // X projection of a shadow (formerly called C)
    Real64 YShadowProjection;  // Y projection of a shadow (formerly called S)
    Array1D<Real64> XTEMP;     // Temporary 'X' values for HC vertices of the overlap
    Array1D<Real64> XVC;       // X-vertices of the clipped figure
    Array1D<Real64> XVS;       // X-vertices of the shadow
    Array1D<Real64> YTEMP;     // Temporary 'Y' values for HC vertices of the overlap
    Array1D<Real64> YVC;       // Y-vertices of the clipped figure
    Array1D<Real64> YVS;       // Y-vertices of the shadow
    Array1D<Real64> ZVC;       // Z-vertices of the clipped figure
                               // Used in Sutherland Hodman poly clipping
    Array1D<Real64> ATEMP;  // Temporary 'A' values for HC vertices of the overlap
    Array1D<Real64> BTEMP;  // Temporary 'B' values for HC vertices of the overlap
    Array1D<Real64> CTEMP;  // Temporary 'C' values for HC vertices of the overlap
    Array1D<Real64> XTEMP1; // Temporary 'X' values for HC vertices of the overlap
    Array1D<Real64> YTEMP1; // Temporary 'Y' values for HC vertices of the overlap
    int maxNumberOfFigures = 0;

    #ifdef EP_NO_OPENGL
        bool penumbra = false;
    #else
        std::unique_ptr<Pumbra::Penumbra> penumbra = nullptr;
    #endif

    bool MustAllocSolarShading = true;
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

    void clear_state() override
    {
        MaxHCV = 15;
        MaxHCS = 1500;
        MAXHCArrayBounds = 0;
        MAXHCArrayIncrement = 0;
        NVS = 0;
        NumVertInShadowOrClippedSurface = 0;
        CurrentSurfaceBeingShadowed = 0;
        CurrentShadowingSurface = 0;
        OverlapStatus = 0;
        CTHETA.deallocate();
        FBKSHC = 0;
        FGSSHC = 0;
        FINSHC = 0;
        FRVLHC = 0;
        FSBSHC = 0;
        LOCHCA = 0;
        NBKSHC = 0;
        NGSSHC = 0;
        NINSHC = 0;
        NRVLHC = 0;
        NSBSHC = 0;
        CalcSkyDifShading = false;
        ShadowingCalcFrequency = 0; // Frequency for Shadowing Calculations
        ShadowingDaysLeft = 0;      // Days left in current shadowing period
        debugging = false;
        MustAllocSolarShading = true;
        GetInputFlag = true;
        firstTime = true;
        HCNS.deallocate();
        HCNV.deallocate();
        HCA.deallocate();
        HCB.deallocate();
        HCC.deallocate();
        HCX.deallocate();
        HCY.deallocate();
        WindowRevealStatus.deallocate();
        HCAREA.deallocate();
        HCT.deallocate();
        ISABSF.deallocate();
        SAREA.deallocate();
        NumTooManyFigures = 0;
        NumTooManyVertices = 0;
        NumBaseSubSurround = 0;
        XShadowProjection = 0.0;
        YShadowProjection = 0.0;
        XTEMP.deallocate();
        XVC.deallocate();
        XVS.deallocate();
        YTEMP.deallocate();
        YVC.deallocate();
        YVS.deallocate();
        ZVC.deallocate();
        ATEMP.deallocate();
        BTEMP.deallocate();
        CTEMP.deallocate();
        XTEMP1.deallocate();
        YTEMP1.deallocate();
        maxNumberOfFigures = 0;
        TrackTooManyFigures.deallocate();
        TrackTooManyVertices.deallocate();
        TrackBaseSubSurround.deallocate();
        DataSurfaces::DBZoneIntWin.deallocate();
        ISABSF.deallocate();
        InitComplexOnce = true;
        ShadowOneTimeFlag = true;
        CHKSBSOneTimeFlag = true;
        ORDERFirstTimeFlag = true;
        TooManyFiguresMessage = false;
        TooManyVerticesMessage = false;
        SHDBKSOneTimeFlag = true;
        SHDGSSOneTimeFlag = true;
    }

    // Default Constructor
    SolarShadingData() :
        SUNCOS(3),
        cOverLapStatus(6, {"No-Overlap", "1st-Surf-within-2nd", "2nd-Surf-within-1st", "Partial-Overlap", "Too-Many-Vertices", "Too-Many-Figures"})
    {}
};
} // namespace EnergyPlus

#endif
