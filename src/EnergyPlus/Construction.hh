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

#ifndef Construction_hh_INCLUDED
#define Construction_hh_INCLUDED

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataBSDFWindow.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataWindowEquivalentLayer.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace Construction {

    int constexpr MaxLayersInConstruct(11); // Maximum number of layers allowed in a single construction
    int constexpr MaxCTFTerms(19);          // Maximum number of CTF terms allowed to still allow stability
    // Note Sync with SurfaceGroundHeatExchanger::local::MaxCTFTerms
    // ** has to be big enough to hold no matter what window model
    //    each window model should validate layers individually
    int constexpr MaxSpectralDataElements(800); // Maximum number in Spectral Data arrays.

    struct ConstructionProps
    {
        // Members
        std::string Name; // Name of construction
        int TotLayers;    // Total number of layers for the construction; for windows
        //  this is the total of the glass, gas and shade layers
        int TotSolidLayers;     // Total number of solid (glass or shade) layers (windows only)
        int TotGlassLayers;     // Total number of glass layers (windows only)
        Array1D_int LayerPoint; // Pointer array which refers back to
        // the Material structure; LayerPoint(i)=j->Material(j)%Name,etc
        bool IsUsed;                // Marked true when the construction is used
        bool IsUsedCTF;             // Mark true when the construction is used for a surface with CTF calculations
        Real64 InsideAbsorpVis;     // Inside Layer visible absorptance of an opaque surface; not used for windows.
        Real64 OutsideAbsorpVis;    // Outside Layer visible absorptance of an opaque surface; not used for windows.
        Real64 InsideAbsorpSolar;   // Inside Layer solar absorptance of an opaque surface; not used for windows.
        Real64 OutsideAbsorpSolar;  // Outside Layer solar absorptance of an opaque surface; not used for windows.
        Real64 InsideAbsorpThermal; // Inside Layer Thermal absorptance for opaque surfaces or windows;
        // for windows, applies to innermost glass layer
        Real64 OutsideAbsorpThermal; // Outside Layer Thermal absorptance
        int OutsideRoughness;        // Outside Surface roughness index (6=very smooth, 5=smooth,
        // 4=medium smooth, 3=medium rough, 2=rough, 1=very rough)
        int DayltPropPtr;   // Pointer to Daylight Construction Properties
        int W5FrameDivider; // FrameDivider number for window construction from Window5 data file;
        //  zero is construction not from Window5 file or Window5 construction has no frame.
        // Conductive properties for the construction
        Array1D<Real64> CTFCross;     // Cross or Y terms of the CTF equation
        Array1D<Real64> CTFFlux;      // Flux history terms of the CTF equation
        Array1D<Real64> CTFInside;    // Inside or Z terms of the CTF equation
        Array1D<Real64> CTFOutside;   // Outside or X terms of the CTF equation
        Array1D<Real64> CTFSourceIn;  // Heat source/sink inside terms of CTF equation
        Array1D<Real64> CTFSourceOut; // Heat source/sink outside terms of CTF equation
        Real64 CTFTimeStep;           // Time increment for stable simulation of construct (could be greater than TimeStep)
        // The next three series of terms are used to calculate the temperature at the location of a source/sink
        // in the QTF formulation.  This calculation is necessary to allow the proper simulation of a
        // radiant system.
        Array1D<Real64> CTFTSourceOut; // Outside terms of the CTF equation for interior temp
        // calc@source location
        Array1D<Real64> CTFTSourceIn; // Inside terms of the CTF equation for interior temp
        // calc@source location
        Array1D<Real64> CTFTSourceQ; // Source/sink terms of the CTF equation for interior temp
        // calc@source location
        // The next three series of terms are used to calculate the temperature at a location specified by the user.
        // This location must be between two layers and is intended to allow the user to evaluate whether or not
        // condensation is a possibility between material layers.
        Array1D<Real64> CTFTUserOut; // Outside terms of the CTF equation for interior temp
        // calc@user location
        Array1D<Real64> CTFTUserIn; // Inside terms of the CTF equation for interior temp
        // calc@user location
        Array1D<Real64> CTFTUserSource; // Source/sink terms of the CTF equation for interior temp
        // calc@user location
        int NumHistories; // CTFTimeStep/TimeStepZone or the number of temp/flux history series
        // for the construction
        int NumCTFTerms;        // Number of CTF terms for this construction (not including terms at current time)
        Real64 UValue;          // Overall heat transfer coefficient for the construction
        int SolutionDimensions; // Number of dimensions in the solution (1 for normal constructions,
        // 1 or 2 for constructions with sources or sinks)-->may allow 3-D later?
        int SourceAfterLayer; // Source/sink is present after this layer in the construction
        int TempAfterLayer;   // User is requesting a temperature calculation after this layer in the construction
        // This location is also the position of a temperature on the interior of a slab
        // that could be used to control a low temperature radiant system
        Real64 ThicknessPerpend; // Thickness between planes of symmetry in the direction
        // perpendicular to the main direction of heat transfer
        // (same as half the distance between tubes)
        Real64 userTemperatureLocationPerpendicular; // Location of the source perpendicular to the main direction
        // of heat transfer.  Used in conjunction with the TempAfterLayer
        // term to provide specific location of user defined temperature.
        // This value is only used when SolutionDimension = 2.
        // Moisture Transfer Functions term belong here as well
        // BLAST detailed solar model parameters
        Real64 AbsDiffIn;  // Inner absorptance coefficient for diffuse radiation
        Real64 AbsDiffOut; // Outer absorptance coefficient for diffuse radiation
        // Variables for window constructions
        Array1D<Real64> AbsDiff; // Diffuse solar absorptance for each glass layer,
        // bare glass or shade on
        Array2D<Real64> BlAbsDiff; // Diffuse solar absorptance for each glass layer vs.
        // slat angle, blind on
        Array2D<Real64> BlAbsDiffGnd; // Diffuse ground solar absorptance for each glass layer
        // vs. slat angle, blind on
        Array2D<Real64> BlAbsDiffSky; // Diffuse sky solar absorptance for each glass layer
        // vs. slat angle, blind on
        Array1D<Real64> AbsDiffBack;   // Diffuse back solar absorptance for each glass layer
        Array2D<Real64> BlAbsDiffBack; // Diffuse back solar absorptance for each glass layer,
        //  vs. slat angle, blind on
        Real64 AbsDiffShade;                  // Diffuse solar absorptance for shade
        Array1D<Real64> AbsDiffBlind;         // Diffuse solar absorptance for blind, vs. slat angle
        Array1D<Real64> AbsDiffBlindGnd;      // Diffuse ground solar absorptance for blind, vs. slat angle
        Array1D<Real64> AbsDiffBlindSky;      // Diffuse sky solar absorptance for blind, vs. slat angle
        Real64 AbsDiffBackShade;              // Diffuse back solar absorptance for shade
        Array1D<Real64> AbsDiffBackBlind;     // Diffuse back solar absorptance for blind, vs. slat angle
        Real64 ShadeAbsorpThermal;            // Diffuse back thermal absorptance of shade
        Array1D<Array1D<Real64>> AbsBeamCoef; // Coefficients of incidence-angle polynomial for solar
        // absorptance for each solid glazing layer
        Array1D<Array1D<Real64>> AbsBeamBackCoef; // As for AbsBeamCoef but for back-incident solar
        Array1D<Real64> AbsBeamShadeCoef;         // Coefficients of incidence-angle polynomial for solar
        // absorptance of shade
        Real64 TransDiff;                      // Diffuse solar transmittance, bare glass or shade on
        Array1D<Real64> BlTransDiff;           // Diffuse solar transmittance, blind present, vs. slat angle
        Array1D<Real64> BlTransDiffGnd;        // Ground diffuse solar transmittance, blind present, vs. slat angle
        Array1D<Real64> BlTransDiffSky;        // Sky diffuse solar transmittance, blind present, vs. slat angle
        Real64 TransDiffVis;                   // Diffuse visible transmittance, bare glass or shade on
        Array1D<Real64> BlTransDiffVis;        // Diffuse visible transmittance, blind present, vs. slat angle
        Real64 ReflectSolDiffBack;             // Diffuse back solar reflectance, bare glass or shade on
        Array1D<Real64> BlReflectSolDiffBack;  // Diffuse back solar reflectance, blind present, vs. slat angle
        Real64 ReflectSolDiffFront;            // Diffuse front solar reflectance, bare glass or shade on
        Array1D<Real64> BlReflectSolDiffFront; // Diffuse front solar reflectance, blind present, vs. slat angle
        Real64 ReflectVisDiffBack;             // Diffuse back visible reflectance, bare glass or shade on
        Array1D<Real64> BlReflectVisDiffBack;  // Diffuse back visible reflectance, blind present, vs. slat angle
        Real64 ReflectVisDiffFront;            // Diffuse front visible reflectance, bare glass or shade on
        Array1D<Real64> BlReflectVisDiffFront; // Diffuse front visible reflectance, blind present, vs. slat angle
        Array1D<Real64> TransSolBeamCoef;      // Coeffs of incidence-angle polynomial for beam sol trans,
        // bare glass or shade on
        Array1D<Real64> TransVisBeamCoef; // Coeffs of incidence-angle polynomial for beam vis trans,
        // bare glass or shade on
        Array1D<Real64> ReflSolBeamFrontCoef; // Coeffs of incidence-angle polynomial for beam sol front refl,
        // bare glass or shade on
        Array1D<Real64> ReflSolBeamBackCoef;    // Like ReflSolBeamFrontCoef, but for back-incident beam solar
        Array1D<Array1D<Real64>> tBareSolCoef;  // Isolated glass solar transmittance coeffs of inc. angle polynomial
        Array1D<Array1D<Real64>> tBareVisCoef;  // Isolated glass visible transmittance coeffs of inc. angle polynomial
        Array1D<Array1D<Real64>> rfBareSolCoef; // Isolated glass front solar reflectance coeffs of inc. angle polynomial
        Array1D<Array1D<Real64>> rfBareVisCoef; // Isolated glass front visible reflectance coeffs of inc. angle polynomial
        Array1D<Array1D<Real64>> rbBareSolCoef; // Isolated glass back solar reflectance coeffs of inc. angle polynomial
        Array1D<Array1D<Real64>> rbBareVisCoef; // Isolated glass back visible reflectance coeffs of inc. angle polynomial
        Array1D<Array1D<Real64>> afBareSolCoef; // Isolated glass front solar absorptance coeffs of inc. angle polynomial
        Array1D<Array1D<Real64>> abBareSolCoef; // Isolated glass back solar absorptance coeffs of inc. angle polynomial
        Array1D<Real64> tBareSolDiff;           // Isolated glass diffuse solar transmittance
        Array1D<Real64> tBareVisDiff;           // Isolated glass diffuse visible transmittance
        Array1D<Real64> rfBareSolDiff;          // Isolated glass diffuse solar front reflectance
        Array1D<Real64> rfBareVisDiff;          // Isolated glass diffuse visible front reflectance
        Array1D<Real64> rbBareSolDiff;          // Isolated glass diffuse solar back reflectance
        Array1D<Real64> rbBareVisDiff;          // Isolated glass diffuse visible back reflectance
        Array1D<Real64> afBareSolDiff;          // Isolated glass diffuse solar front absorptance
        Array1D<Real64> abBareSolDiff;          // Isolated glass diffuse solar back absorptance
        bool FromWindow5DataFile;               // True if this is a window construction extracted from the Window5 data file
        Real64 W5FileMullionWidth;              // Width of mullion for construction from Window5 data file (m)
        int W5FileMullionOrientation;           // Orientation of mullion, if present, for Window5 data file construction,
        Real64 W5FileGlazingSysWidth;           // Glass width for construction from Window5 data file (m)
        Real64 W5FileGlazingSysHeight;          // Glass height for construction form Window5 data file (m)
        Real64 SummerSHGC;                      // Calculated ASHRAE SHGC for summer conditions
        Real64 VisTransNorm;                    // The normal visible transmittance
        Real64 SolTransNorm;                    // the normal solar transmittance
        bool SourceSinkPresent;                 // .TRUE. if there is a source/sink within this construction
        bool TypeIsWindow;                      // True if a window construction, false otherwise
        bool WindowTypeBSDF;                    // True for complex window, false otherwise
        bool TypeIsEcoRoof;                     // -- true for construction with ecoRoof outside, the flag
        //-- is turned on when the outside layer is of type EcoRoof
        bool TypeIsIRT;          // -- true for construction with IRT material
        bool TypeIsCfactorWall;  // -- true for construction with Construction:CfactorUndergroundWall
        bool TypeIsFfactorFloor; // -- true for construction with Construction:FfactorGroundFloor
        // Added TH 12/22/2008 for thermochromic windows
        int TCFlag; // 0: this construction is not a thermochromic window construction
        // 1: it is a TC window construction
        int TCLayer;       // Reference to the TC glazing material layer in the Material array
        int TCMasterConst; // The master TC construction referenced by its slave constructions
        int TCLayerID;     // Which material layer is the TC glazing, counting all material layers.
        int TCGlassID;     // Which glass layer is the TC glazing, counting from glass layers only.
        // For CFactor underground walls
        Real64 CFactor;
        Real64 Height;
        // For FFactor slabs-on-grade or underground floors
        Real64 FFactor;
        Real64 Area;
        Real64 PerimeterExposed;
        bool ReverseConstructionNumLayersWarning;
        bool ReverseConstructionLayersOrderWarning;
        // Complex Fenestration
        DataBSDFWindow::BSDFWindowInputStruct BSDFInput; // nest structure with user input for complex fenestration
        // EquivalentLayer Window
        bool WindowTypeEQL;              // True for equivalent layer window, false otherwise
        int EQLConsPtr;                  // Pointer to equivalent Layer window construction
        Array1D<Real64> AbsDiffFrontEQL; // Diffuse layer system front absorptance for EQL window
        Array1D<Real64> AbsDiffBackEQL;  // Diffuse layer system back absorptance for EQL window
        Real64 TransDiffFrontEQL;        // Diffuse system front transmittance for EQL window
        Real64 TransDiffBackEQL;         // Diffuse system back transmittance for EQL window
        // Air boundary
        bool TypeIsAirBoundary;       // true for Construction:AirBoundary
        bool TypeIsAirBoundaryMixing; // true for Construction:AirBoundary with SimpleMixing for air exchange
        Real64 AirBoundaryACH;        // Air boundary simple mixing air changes per hour [1/hr]
        int AirBoundaryMixingSched;   // Air boundary simple mixing schedule index

        int rcmax;                  // Total number of nodes in the construct (<= MaxTotNodes)
        Array2D<Real64> AExp;       // Exponential of AMat
        Array2D<Real64> AInv;       // Inverse of AMat
        Array2D<Real64> AMat;       // "A" matrix from Seem's dissertation (constant coefficients of linear system)
        Array1D<Real64> BMat;       // "B" matrix of state space method (non-zero elements)
        Array1D<Real64> CMat;       // "C" matrix of state space method (non-zero elements)
        Array1D<Real64> DMat;       // "D" matrix of state space method (non-zero elements)
        Array1D<Real64> e;          // Coefficients for the surface flux history term
        Array2D<Real64> Gamma1;     // Intermediate calculation array corresponding to a term in Seem's dissertation
        Array2D<Real64> Gamma2;     // Intermediate calculation array corresponding to a term in Seem's dissertation
        Array3D<Real64> s;          // Coefficients for the surface temperature history terms
        Array2D<Real64> s0;         // Coefficients for the current surface temperature terms
        Array2D<Real64> IdenMatrix; // Identity Matrix
        int NumOfPerpendNodes = 7;  // Number of nodes in the direction
        // perpendicular to the main direction of heat transfer.  This is only used
        // when a two-dimensional solution has been requested for a construction
        // with a heat source/sink.
        int NodeSource;   // Node at which a source or sink is present
        int NodeUserTemp; // Node where user wishes to calculate a temperature (for constructions with sources/sinks only)

        // Default Constructor
        ConstructionProps()
            : TotLayers(0), TotSolidLayers(0), TotGlassLayers(0), LayerPoint(MaxLayersInConstruct, 0), IsUsed(false), IsUsedCTF(false),
              InsideAbsorpVis(0.0), OutsideAbsorpVis(0.0), InsideAbsorpSolar(0.0), OutsideAbsorpSolar(0.0), InsideAbsorpThermal(0.0),
              OutsideAbsorpThermal(0.0), OutsideRoughness(0), DayltPropPtr(0), W5FrameDivider(0), CTFCross({0, MaxCTFTerms - 1}, 0.0),
              CTFFlux(MaxCTFTerms - 1, 0.0), CTFInside({0, MaxCTFTerms - 1}, 0.0), CTFOutside({0, MaxCTFTerms - 1}, 0.0),
              CTFSourceIn({0, MaxCTFTerms - 1}, 0.0), CTFSourceOut({0, MaxCTFTerms - 1}, 0.0), CTFTSourceOut({0, MaxCTFTerms - 1}, 0.0),
              CTFTSourceIn({0, MaxCTFTerms - 1}, 0.0), CTFTSourceQ({0, MaxCTFTerms - 1}, 0.0), CTFTUserOut({0, MaxCTFTerms - 1}, 0.0),
              CTFTUserIn({0, MaxCTFTerms - 1}, 0.0), CTFTUserSource({0, MaxCTFTerms - 1}, 0.0), NumHistories(0), NumCTFTerms(0), UValue(0.0),
              SolutionDimensions(0), SourceAfterLayer(0), TempAfterLayer(0), ThicknessPerpend(0.0), userTemperatureLocationPerpendicular(0.0),
              AbsDiffIn(0.0), AbsDiffOut(0.0), AbsDiffShade(0.0), AbsDiffBlind(DataSurfaces::MaxSlatAngs, 0.0),
              AbsDiffBlindGnd(DataSurfaces::MaxSlatAngs, 0.0), AbsDiffBlindSky(DataSurfaces::MaxSlatAngs, 0.0), AbsDiffBackShade(0.0),
              AbsDiffBackBlind(DataSurfaces::MaxSlatAngs, 0.0), ShadeAbsorpThermal(0.0), AbsBeamShadeCoef(6, 0.0), TransDiff(0.0),
              BlTransDiff(DataSurfaces::MaxSlatAngs, 0.0), BlTransDiffGnd(DataSurfaces::MaxSlatAngs, 0.0),
              BlTransDiffSky(DataSurfaces::MaxSlatAngs, 0.0), TransDiffVis(0.0), BlTransDiffVis(DataSurfaces::MaxSlatAngs, 0.0),
              ReflectSolDiffBack(0.0), BlReflectSolDiffBack(DataSurfaces::MaxSlatAngs, 0.0), ReflectSolDiffFront(0.0),
              BlReflectSolDiffFront(DataSurfaces::MaxSlatAngs, 0.0), ReflectVisDiffBack(0.0), BlReflectVisDiffBack(DataSurfaces::MaxSlatAngs, 0.0),
              ReflectVisDiffFront(0.0), BlReflectVisDiffFront(DataSurfaces::MaxSlatAngs, 0.0), TransSolBeamCoef(6, 0.0), TransVisBeamCoef(6, 0.0),
              ReflSolBeamFrontCoef(6, 0.0), ReflSolBeamBackCoef(6, 0.0), tBareSolDiff(5, 0.0), tBareVisDiff(5, 0.0), rfBareSolDiff(5, 0.0),
              rfBareVisDiff(5, 0.0), rbBareSolDiff(5, 0.0), rbBareVisDiff(5, 0.0), afBareSolDiff(5, 0.0), abBareSolDiff(5, 0.0),
              FromWindow5DataFile(false), W5FileMullionWidth(0.0), W5FileMullionOrientation(0), W5FileGlazingSysWidth(0.0),
              W5FileGlazingSysHeight(0.0), SummerSHGC(0.0), VisTransNorm(0.0), SolTransNorm(0.0), SourceSinkPresent(false), TypeIsWindow(false),
              WindowTypeBSDF(false), TypeIsEcoRoof(false), TypeIsIRT(false), TypeIsCfactorWall(false), TypeIsFfactorFloor(false), TCFlag(0),
              TCLayer(0), TCMasterConst(0), TCLayerID(0), TCGlassID(0), CFactor(0.0), Height(0.0), FFactor(0.0), Area(0.0), PerimeterExposed(0.0),
              ReverseConstructionNumLayersWarning(false), ReverseConstructionLayersOrderWarning(false), WindowTypeEQL(false), EQLConsPtr(0),
              AbsDiffFrontEQL(DataWindowEquivalentLayer::CFSMAXNL, 0.0), AbsDiffBackEQL(DataWindowEquivalentLayer::CFSMAXNL, 0.0),
              TransDiffFrontEQL(0.0), TransDiffBackEQL(0.0), TypeIsAirBoundary(false), TypeIsAirBoundaryMixing(false), AirBoundaryACH(0.0),
              AirBoundaryMixingSched(0), rcmax(0), NodeSource(0), NodeUserTemp(0)
        {
            BMat.allocate(3);
            CMat.allocate(2);
            DMat.allocate(2);
            s0.allocate(3, 4);
        }

        void calculateTransferFunction(EnergyPlusData &state, bool &ErrorsFound, bool &DoCTFErrorReport);

        void calculateExponentialMatrix(); // Time step of the resulting CTFs

        void calculateInverseMatrix();

        void calculateGammas();

        void calculateFinalCoefficients();

        void reportTransferFunction(EnergyPlusData &state, int cCounter);

        bool isGlazingConstruction(EnergyPlusData &state) const;

        Real64 setUserTemperatureLocationPerpendicular(EnergyPlusData &state, Real64 userValue);

        void setNodeSourceAndUserTemp(Array1D_int &Nodes);

        void setArraysBasedOnMaxSolidWinLayers(EnergyPlusData &state);
    };
} // namespace Construction

struct ConstructionData : BaseGlobalStruct
{
    Array1D<Construction::ConstructionProps> Construct;
    Array1D_int LayerPoint = Array1D<int>(Construction::MaxLayersInConstruct, 0);

    void clear_state() override
    {
        this->Construct.deallocate();
        this->LayerPoint.deallocate();
        this->LayerPoint.dimension(Construction::MaxLayersInConstruct, 0);
    }
};

} // namespace EnergyPlus

#endif
