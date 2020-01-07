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

#ifndef DataBSDFWindow_hh_INCLUDED
#define DataBSDFWindow_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Array3D.hh>

// EnergyPlus Headers
#include <EnergyPlus/DataVectorTypes.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

namespace DataBSDFWindow {

    // Using/Aliasing
    using DataVectorTypes::Vector;

    // Data
    // MODULE PARAMETER DEFINITIONS:

    extern int const BasisType_WINDOW;
    extern int const BasisType_Custom;

    extern int const BasisSymmetry_Axisymmetric;
    extern int const BasisSymmetry_None;

    // Thermal calculations for complex fenestration can be used to generate reports for standard cases
    // noCondition is used when performing timestep calculations
    // summerCondtion will override certain parameters so that produced results are matching standard summer WINDOW (software) results
    // winterCondition will override certain parameters so that produced resuls are matching standard winter WINDOW (software) results
    extern int const noCondition;
    extern int const summerCondition;
    extern int const winterCondition;

    // DERIVED TYPE DEFINITIONS:

    // Structure to keep reference points coefficients for different reference points and illuminance maps

    // Allocation of complex fenestration data:  SurfaceWindow(:)%ComplexFen is a structure of type BSDFWindowDescript
    // defined in DataSurfaces.  ComplexWind(:) is an array of type BSDF WindowGeomDescr defined as a module
    // variable in WindowComplexManager

    // MODULE VARIABLE DECLARATIONS:

    extern int TotComplexFenStates; // Number of complex fenestration construction definitions
    extern int FirstBSDF;           // Location of first complex fenestration construction definition in Constr array
    extern int MaxBkSurf;           // was 20    Maximum number of back surfaces in solar overlap & interior solar distribution
    extern int TotThermalModels;    // Number of thermal models
    // calculation
    extern Array3D<Real64> SUNCOSTS;     // Timestep values of solar direction cosines
    extern Array2D<Real64> BSDFTempMtrx; // Temporary matrix for holding axisymmetric input

    // Types

    struct BasisElemDescr
    {
        // Members
        // The following are in the local coordinate system corresponding to the matrix
        Real64 Theta;     // Centroid Theta value
        Real64 Phi;       // Centroid Phi value
        Real64 dTheta;    // Element width, Theta
        Real64 dPhi;      // Element width, Phi
        Real64 UpprTheta; // Patch upper edge, Theta
        Real64 LwrTheta;  // Patch lower edge, Theta
        Real64 UpprPhi;   // Patch upper edge, Phi
        Real64 LwrPhi;    // Patch lower edge, Phi
        // Note: The dimension index of the BasisElementDescription object corresponds to
        // the position (index) of this element in the row or column of property matrix
        // Note:  the following are intended to be used for interpolating directions among basis elements
        int INNbInL;  // Index of inward (lower Theta) neighbor, lower phi
        int INNbInH;  // Index of inward (lower Theta) neighbor, higher phi
        int INNbOutL; // Index of outward (higher Theta) neighbor, lower phi
        int INNbOutH; // Index of outward (higher Theta) neighbor, higher phi
        int INNbLft;  // Index of leftward (higher Phi) neighbor (same Theta)
        int INNbRt;   // Index of rightward (lower Phi) neighbor (same Theta)
        // These indices are in the BasisElement array, which matches the row/column of the matrix

        // Default Constructor
        BasisElemDescr()
        {
        }
    };

    struct BSDFDaylghtPosition
    {
        // Members
        Real64 Altitude; // Altitude range is from -pi/2 to pi/2. Horizontal vector have altitude of zero
        Real64 Azimuth;  // Azimuth is measured from positive x counter clockwise. Its range is from -pi to pi

        // Default Constructor
        BSDFDaylghtPosition()
        {
        }

        // Member Constructor
        BSDFDaylghtPosition(Real64 const Altitude, // Altitude range is from -pi/2 to pi/2. Horizontal vector have altitude of zero
                            Real64 const Azimuth   // Azimuth is measured from positive x counter clockwise. Its range is from -pi to pi
                            )
            : Altitude(Altitude), Azimuth(Azimuth)
        {
        }
    };

    struct BasisStruct
    {
        // Members
        int BasisType;                // BasisType_WINDOW or BasisType_Custom  (see HeatBalanceManager)
        int BasisSymmetryType;        // BasisSymmetry_Axisymmetric or BasisSymmetry_None  (see HeatBalanceManager)
        int BasisMatIndex;            // pointer to matrix for basis
        int NBasis;                   // No. elements in basis
        Array1D<Real64> Lamda;        // Vector of diagonal Lamda matrix elems for grid
        Array1D<Real64> SolAng;       // Vector of basis element solid angles for grid
        int NThetas;                  // No. Theta values in basis
        Array1D<Real64> Thetas;       // List of basis theta values
        Array1D_int NPhis;            // No. basis phi values for each theta
        Array2D<Real64> Phis;         // List of basis phi values for each theta
        Array2D_int BasisIndex;       // Index of basis element for theta, phi
        Array1D<BasisElemDescr> Grid; // actual basis (to be constructed from matrix)

        // Default Constructor
        BasisStruct() : BasisType(0), BasisSymmetryType(0), BasisMatIndex(0), NBasis(0), NThetas(0)
        {
        }
    };

    struct BSDFGeomDescr
    {
        // Members
        BasisStruct Inc;                   // Basis for incident hemisphere
        Array1D<Vector> sInc;              // Central direction vectors of incident grid (World coords)
        Array1D<BSDFDaylghtPosition> pInc; // azimuth and altitude of incidence vectors
        Array1D<Real64> CosInc;            // cosine of incident angle
        Array1D<Real64> DAInc;             // cosine of incident angle times delta theta time delta phi (used in daylighting calculations)
        int NSkyUnobs;                     // Number of Inc basis rays from unobstructed sky
        int NGndUnobs;                     // Number of Inc basis rays from unobstructed ground
        int NSky;                          // Number of Inc basis rays from sky
        int NGnd;                          // Number of Inc basis rays from gnd
        int NReflSurf;                     // Number of Inc basis rays from (potentially reflecting) surfaces
        Array1D_int SkyIndex;              // list of sky basis indices
        Array1D_int GndIndex;              // list of gnd basis indices
        Array1D<Vector> GndPt;             // gnd intersection pt of gnd basis ray (z=0)
        Array1D_int RefSurfIndex;          // list of basis indices of rays striking exterior surf
        Array1D_int RefRayNHits;           // for a given ray striking a surface, no. of surfaces pierced
        Array2D_int HitSurfNo;             // for a given ray striking surface, list of intersected surf nos
        Array2D<Real64> HitSurfDSq;        // for a given ray striking surface, list of distance^2
        //  from window
        Array2D<Vector> HitPt;       // for a given ray striking surface, list of hit pts
        Array1D<Real64> SolSkyWt;    // Sky intensity weights
        Array1D<Real64> SolSkyGndWt; // Wts for sky rad refl from grn
        Array3D<Real64> SolBmGndWt;  // Wts for beam rad refl from gnd (hour, timestep)
        Array2D_int SolBmIndex;      // Basis index corresponding to beam dir (hour, timestep)
        // Note this is zero if sun is not in incident hemisphere
        // otherwise in range 1..NBasis
        Array2D<Real64> ThetaBm; // Theta angle corresponging to beam dir (hour, timestep) (rad)
        Array2D<Real64> PhiBm;   // Theta angle corresponging to beam dir (hour, timestep) (rad)
        BasisStruct Trn;
        Array1D<Vector> sTrn;              // Central direction vectors of Outgoing grid (World coords)
        Array1D<BSDFDaylghtPosition> pTrn; // azimuth and altitude of incidence vectors
        Array1D_int NSurfInt;              // No. of basis rays intersecting back surface (dim from
        // NBkSurf in BSDF State Descr)
        Array2D_int SurfInt;    // Basis index (IBkSurf, j) of the jth ray intersecting IBkSurf
        Array2D<Real64> SjdotN; // dot product (IBksurf, j) of the jth ray direction with
        // the normal to the back surface
        Array2D<Real64> AOverlap; // Overlap areas for each outgoing
        // direction (Trn) (no of outgoing dir, NBKSurf)
        Array2D<Real64>
            ARhoVisOverlap; // Overlap areas multiplied with surface reflectance for each outgoing direction (Trn) (no of outgoing dir, NBKSurf)
        Array1D<Real64> AveRhoVisOverlap; // Average visible reflectance from overlap surface which originates from one outgoing direction
        bool InitState;                   // Flag for marking that state needs to be initalized

        // Default Constructor
        BSDFGeomDescr() : InitState(true)
        {
        }
    };

    struct BSDFRefPoints
    {
        // Members
        Array1D_int NSky;                   // number of sky elements for each window element (# window el)
        Array1D_int NGnd;                   // number of ground elements for each window element (# window el)
        Array1D_int NReflSurf;              // number of Inc basis rays from reflecting surfaces (# window el)
        Array2D_int SkyIndex;               // list of sky basis indices (# window el, NSky)
        Array2D_int GndIndex;               // list of gnd basis indices (# window el, NGnd)
        Array2D<Vector> GndPt;              // gnd intersection pt of gnd basis ray (z=0) (# window el, NGnd)
        Array2D<Real64> GndObstrMultiplier; // ground obstruction multiplier used in reflection calculatations (# window el, NGnd)
        Array2D_int RefSurfIndex;           // list of basis indices of rays striking exterior surf (# window el, NReflSurf)
        Array2D_int RefRayNHits;            // for a given ray striking a surface, no. of surfaces pierced (# window el, NReflSurf)
        Array2D<Real64> TransOutSurf; // total transmittance of exterior obstructions for given incoming basis direction. (# window el, NReflSurf)
        Array3D_int HitSurfNo;        // for a given ray striking surface, list of intersected surf nos (# window el, NReflSurf, RefRayNHits)
        Array3D<Real64> HitSurfDSq;   // for a given ray striking surface, list of distance^2 from window (# window el, NReflSurf, RefRayNHits)
        Array3D<Vector> HitPt;        // for a given ray striking surface, list of hit pts (# window el, NReflSurf, RefRayNHits)
        Array1D_int RefPointIndex;    // outgoing direction which containts reference point (# window el)
        Array1D_bool RefPointIntersection; // determines if reference point is laying in light tube of bsdf outgoing direction (NTrnBasis)
        Array1D<Real64> RefPtIntPosFac; // position factors for intersections from reference point to window for each outgoing direction (NTrnBasis)

        // Default Constructor
        BSDFRefPoints()
        {
        }
    };

    struct BSDFDaylghtGeomDescr
    {
        // Members
        Array2D<BSDFRefPoints>
            IlluminanceMap; // array to keep bsdf coefficients for different illuminance maps (# of illuminance maps, # of reference points)
        Array1D<BSDFRefPoints> RefPoint; // keep reference points daylight coefficients (# of reference points)

        // Default Constructor
        BSDFDaylghtGeomDescr()
        {
        }
    };

    struct BSDFBkSurfDescr
    {
        // Members
        Array2D<Real64> WinDHBkRefl; // Back directional hemispherical reflectance (hour, timestep)
        // of this window for radiation from the back surface window
        Array3D<Real64> WinDirBkAbs; // back absorptance (hr, timestep, layer)
        //   for beam radiation absorbed in this
        //   window that comes from the back surface window
        // Performance May be faster in (layer,hr,timestep) order (loops are not consistent)
        // Note:  WinDHBkRefl and WinDirBkAbs are the same for all hours & timesteps if the back surface window is a
        // Complex Fenestration; they depend on the sun direction if the back surface window is a regular window

        // Default Constructor
        BSDFBkSurfDescr()
        {
        }
    };

    struct BSDFStateDescr
    {
        // Members
        int Konst; // pointer to construction for this state; property matrices are in the construction
        // INTEGER      :: ThermConst =0  ! pointer to thermal construction for this state
        Real64 WinDiffTrans; // Window hemispherical ave diff trans
        // for use in corrections requiring a diffuse trans
        // that have not been redone in detail for Compex Fen
        Real64 WinDiffVisTrans;          // Window hemispherical ave diff trans for visible spectrum
        Array2D<Real64> WinDirHemiTrans; // Directional-hemispherical transmittance(hr,ts)
        Array2D<Real64> WinDirSpecTrans; // Directional specular transmittance(hr,ts)
        Real64 WinSkyTrans;              // Transmittance for sky radiation (weighted average over sky viewed)
        Real64 WinSkyGndTrans;           // Transmittance for sky radiation reflected from ground (average over
        // viewed part of ground)
        Array2D<Real64> WinBmGndTrans; // Transmittance (hour, timestep) for beam radiation reflected
        // from ground (average over unshaded ground viewed)
        Real64 WinBkHemRefl;    // Window back hemispherical reflectance
        Real64 WinBkHemVisRefl; // Window back hemispherical reflectance (visible spectrum)
        //(for reflection of interior diffuse radiation)
        int NLayers;                  // Number of absorbing layers in this window
        Array3D<Real64> WinBmFtAbs;   // Front directional absorptance (hour, timestep, layer)
        Array1D<Real64> WinSkyFtAbs;  // Front absorptance (layer) averaged over sky
        Array1D<Real64> WinSkyGndAbs; // Front absorptance (layer) averaged over ground
        // viewed part of gnd  (for ground-reflected sky radiation)
        Array3D<Real64> WinBmGndAbs; // Front absorptance (hour, timestep, layer) averaged
        // over unshaded ground viewed by beam
        Array1D<Real64> WinFtHemAbs;      // Front hemispherical absorptance (layers)
        Array1D<Real64> WinBkHemAbs;      // Back hemispherical absorptance (layers)
        Array3D<Real64> WinToSurfBmTrans; // Beam transmittance (hour, timestep, bk surf no)
        // to back surface
        // Note: the following will be evaluated only if the given back surface is a  window
        Array1D<BSDFBkSurfDescr> BkSurf; // Structure dimensioned (bk surface no)
        // Integrated beam values at front and back sides of window.  It is used in calculations of how much of the energy is
        // leaving throught the window to other zone or to the outside for certain beam direction
        Array1D<Real64> IntegratedFtAbs;   // Sum of all back layer absorptances (for each back direction)
        Array1D<Real64> IntegratedFtRefl;  // Integrated back layer reflectance (for each back direction)
        Array1D<Real64> IntegratedFtTrans; // Integrated back layer transmittance (for each back direction)
        Array1D<Real64> IntegratedBkAbs;   // Sum of all back layer absorptances (for each back direction)
        Array1D<Real64> IntegratedBkRefl;  // Integrated back layer reflectance (for each back direction)
        Array1D<Real64> IntegratedBkTrans; // Integrated back layer transmittance (for each back direction)

        // Default Constructor
        BSDFStateDescr() : Konst(0), WinSkyTrans(0.0), WinSkyGndTrans(0.0), WinBkHemRefl(0.0), WinBkHemVisRefl(0.0), NLayers(0)
        {
        }
    };

    struct BSDFRefPointsGeomDescr
    {
        // Members
        Array1D<Real64> SolidAngle;    // Solid angle from daylighting reference point to each window element (# window el)
        Array1D<Vector> SolidAngleVec; // unit vector from reference point towards center of window element (# window el)

        // Default Constructor
        BSDFRefPointsGeomDescr()
        {
        }
    };

    struct BSDFWindowGeomDescr
    {
        // Members
        // This contains all the geometry info that we don't want to carry around in SurfaceWindow
        // This is dimensioned like SurfaceWindow, but only surfaces that are complex windows
        // will have the structure below allocated
        int NumStates;                             // Number of states for this window
        Array1D<BSDFGeomDescr> Geom;               // This is dimensioned with number of states
        Array1D<BSDFDaylghtGeomDescr> DaylghtGeom; // This is dimensioned with number of states
        bool DaylightingInitialized;               // used for one time initialization only
        int NBkSurf;                               // Number of back (interior) surfaces viewed by this window
        Array1D<Vector> sWinSurf;                  // Unit vector from window center to center of IBkSurf
        Array1D<Real64> sdotN;                     // Dot product of unit vector s with back surface normal
        // here s is vector from center of window to center of back surface
        // Function of the following subsumed by using an index of 0 if no beam incidence
        // REAL(r64), DIMENSION(: , : ), ALLOCATABLE  ::  SolBmWt  !Intensity wt for beam radiation (Hour, timestep)
        Array2D<BSDFRefPointsGeomDescr>
            IlluminanceMap; // array to keep bsdf coefficients for different illuminance maps (# of illuminance maps, # of reference points)
        Array1D<BSDFRefPointsGeomDescr> RefPoint; // keep reference points daylight coefficients (# of reference points)

        // Default Constructor
        BSDFWindowGeomDescr() : DaylightingInitialized(false), NBkSurf(0)
        {
        }
    };

    struct BSDFWindowDescript
    {
        // Members
        int NumStates;                   // Number of states for this window
        int CurrentState;                // Current state of this window
        Array2D<Real64> ResultAllStates; // Array to hold calculated
        // quantities for all states.
        // Currently unallocated.  To be defined when control
        // scheme worked out.  This is an array (nvar, nstates)
        // to be set up for some number of variables, and calculated
        // for all states 1...NumStates each time step.  e.g., one variable could be
        // total beam transmitted solar, another total transmitted diffuse
        // The idea is that for a given time step when one has the
        // actual result (total cooling load or whatever), one needs to have
        // some information about all the states to decide where to
        // set the state variable for the next time step
        Array1D<BSDFStateDescr> State; // State description, dimensioned with number of states

        // Default Constructor
        BSDFWindowDescript() : CurrentState(1)
        {
        }
    };

    struct BSDFLayerAbsorpStruct
    {
        // Members
        int MaterialIndex;      // pointer to material layer
        int FrtAbsIndex;        // pointer to matrix for Front directional absorptance vector
        int AbsNcols;           // Number of elements (columns) in each of the absorption (row) vectors
        Array2D<Real64> FrtAbs; // Front directional absorptance vector
        int BkAbsIndex;         // pointer to matrix for Back directional absorptance vector
        Array2D<Real64> BkAbs;  // Back directional absorptance vector

        // Default Constructor
        BSDFLayerAbsorpStruct() : MaterialIndex(0), FrtAbsIndex(0), AbsNcols(0), BkAbsIndex(0)
        {
        }
    };

    struct BSDFWindowInputStruct
    {
        // Members
        // nested data for Construction
        int BasisType;
        int BasisSymmetryType;
        int ThermalModel;            // Pointer to thermal model
        int BasisMatIndex;           // pointer to matrix for basis
        int BasisMatNrows;           // No. rows in matrix
        int BasisMatNcols;           // No. columns in matrix
        int NBasis;                  // No. elements in basis
        Array2D<Real64> BasisMat;    // basis matrix
        int SolFrtTransIndex;        // pointer to matrix for Front optical transmittance matrix
        int SolFrtTransNrows;        // No. rows in matrix
        int SolFrtTransNcols;        // No. columns in matrix
        Array2D<Real64> SolFrtTrans; // Front optical transmittance matrix
        int SolBkReflIndex;          // pointer to matrix for Back optical reflectance matrix
        int SolBkReflNrows;          // No. rows in matrix
        int SolBkReflNcols;          // No. columns in matrix
        Array2D<Real64> SolBkRefl;   // Back optical reflectance matrix
        int VisFrtTransIndex;        // pointer to matrix for Front visible transmittance matrix
        int VisFrtTransNrows;        // No. rows in matrix
        int VisFrtTransNcols;        // No. columns in matrix
        Array2D<Real64> VisFrtTrans; // Front visible transmittance matrix
        int VisBkReflIndex;          // pointer to matrix for Back visible reflectance matrix
        int VisBkReflNrows;          // No. rows in matrix
        int VisBkReflNcols;          // No. columns in matrix
        Array2D<Real64> VisBkRefl;   // Back visible reflectance matrix
        // INTEGER   :: ThermalConstruction  !Pointer to location in Construct array of thermal construction for the state
        // (to be implemented)
        int NumLayers;
        Array1D<BSDFLayerAbsorpStruct> Layer;

        // Default Constructor
        BSDFWindowInputStruct()
            : BasisType(0), BasisSymmetryType(0), ThermalModel(0), BasisMatIndex(0), BasisMatNrows(0), BasisMatNcols(0), NBasis(0),
              SolFrtTransIndex(0), SolFrtTransNrows(0), SolFrtTransNcols(0), SolBkReflIndex(0), SolBkReflNrows(0), SolBkReflNcols(0),
              VisFrtTransIndex(0), VisFrtTransNrows(0), VisFrtTransNcols(0), VisBkReflIndex(0), VisBkReflNrows(0), VisBkReflNcols(0), NumLayers(0)
        {
        }
    };

    // Object Data
    extern Array1D<BSDFWindowGeomDescr> ComplexWind; // Window geometry structure: set in CalcPerSolarBeam/SolarShading

} // namespace DataBSDFWindow

} // namespace EnergyPlus

#endif
