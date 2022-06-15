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

// C++ Headers
#include <algorithm>
#include <cassert>
#include <cmath>
#include <memory>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/ConvectionCoefficients.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataBSDFWindow.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataWindowEquivalentLayer.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/HeatBalanceSurfaceManager.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WindowComplexManager.hh>
#include <EnergyPlus/WindowEquivalentLayer.hh>
#include <EnergyPlus/WindowManager.hh>
#include <EnergyPlus/WindowManagerExteriorOptical.hh>
#include <EnergyPlus/WindowManagerExteriorThermal.hh>
#include <EnergyPlus/WindowModel.hh>

namespace EnergyPlus {

namespace WindowManager {

    // MODULE INFORMATION
    //       AUTHOR         Fred Winkelmann
    //       DATE WRITTEN   September 1999
    //       MODIFIED       August 2001 (FW): add window shade thermal calculation;
    //                                        add window blind optical and thermal model.
    //                      February 2003 (FW/LKL): Name changed to WindowManager
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // Manages the window optical and thermal calculations derived
    // from WINDOW 4 and WINDOW 5.

    // METHODOLOGY EMPLOYED:

    // REFERENCES:
    // WINDOW 4:
    // D.Arasteh, M.Reilly and M.Rubin. A versative procedure for
    // calculating heat transfer through windows. ASHRAE Trans. 1989, Vol. 95, Pt. 2.

    // E.Finlayson, D.Arasteh, C.Huizenga, M.Rubin, and M.Reilly. WINDOW 4.0:
    // Documentation of calculation procedures. LBL-33943. July 1993.

    // WINDOW 5:
    // ASHRAE Standard 142P (draft 1/13/98): Standard method for determining and expressing
    // the heat transfer and total optical properties of fenestration products.

    // Shade and blind thermal model:
    // ISO/DIS 15099, Thermal Performance of Windows, Doors and Shading Devices,
    // Detailed Calculations, 1/12/00.

    // Blind optical model:
    // H. Simmler, U. Fischer and Frederick Winkelmann, Solar-Thermal Window Blind Model
    // for DOE-2, Lawrence Berkeley National Laboratory, Jan. 1996.

    // Using/Aliasing
    using namespace DataEnvironment;
    using namespace DataHeatBalance;
    using namespace DataHeatBalFanSys;
    using namespace DataSurfaces;

    // SUBROUTINE SPECIFICATIONS FOR MODULE WindowManager:
    //   Optical Calculation Routines
    //   Heat Balance Routines

    void InitWindowOpticalCalculations(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   September 2016
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Manages if optical calculation will be performed with internal or external routines

        // check and read custom solar and/or visible spectrum data if any
        CheckAndReadCustomSprectrumData(state);

        // allocate surface level adj ratio data member
        state.dataHeatBalSurf->SurfWinCoeffAdjRatio.dimension(state.dataSurface->TotSurfaces, 1.0);

        if (state.dataWindowManager->inExtWindowModel->isExternalLibraryModel()) {
            InitWCE_SimplifiedOpticalData(state);
        } else {
            InitGlassOpticalCalculations(state);
        }
    }

    void InitGlassOpticalCalculations(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         F. Winkelmann
        //       DATE WRITTEN   August 1999
        //       MODIFIED       May 2001 (FW): add window blinds
        //                      Jan 2002 (FW): add blinds with variable slat angle
        //                      Jan 2003 (FW): add between-glass shade/blind
        //                      May 2006 (RR): add exterior window screen
        //                      Aug 2010 (TH): allow spectral data for between-glass shade/blind
        //                      Aug 2013 (TH): allow user defined solar and visible spectrum data
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Manages the calculation of the solar and visible properties of a multi-layer glazing
        // system from the properties of the individual glazing and shading layers

        // Using/Aliasing
        using namespace Vectors;

        using WindowEquivalentLayer::InitEquivalentLayerWindowCalculations;

        int CoefNum;                    // Polynomial coefficient number
        int j;                          // Wavelength counter
        int TotLay;                     // Total solid and gas layers in a window construction
        int ConstrNum;                  // Construction number
        int ConstrNumSh;                // Shaded construction number
        int SurfNum;                    // Surface number
        int ShadeLayNum;                // Layer number for shade or blind, if present
        int ShadeLayPtr;                // Material number for shade or blind
        bool lquasi;                    // True if one or more glass layers have no spectral data
        bool AllGlassIsSpectralAverage; // True if all glazing in a construction is spectral average
        bool IntShade;                  // True if construction has an interior,exterior or between-glass shade
        bool ExtShade;
        bool BGShade;
        bool IntBlind; // True if construction has an interior,exterior or between-glass blind
        bool ExtBlind;
        bool BGBlind;
        bool ExtScreen;                                         // True if construction has an exterior screen
        bool ScreenOn;                                          // True if construction has an exterior screen
        bool BlindOn;                                           // True if IntBlind, ExtBlind or BGBlind is true
        bool ShadeOn;                                           // True if IntShade, ExtShade or BGShade is true
        int BlNum;                                              // Blind number
        int ScNum;                                              // Screen number
        Array1D<Real64> sabsPhi(state.dataWindowManager->nume); // Glazing system absorptance for a glass layer
        //  and angle of incidence, for each wavelength
        //   glass layer for an angle of incidence, for each wavelength
        Array1D<Real64> solabsDiff(5); // Glazing system layer solar absorptance for each glass layer
        Array1D<Real64> solabsPhiLay(
            state.dataWindowManager->MaxNumOfIncidentAngles); // Glazing system solar absorptance for a layer at each incidence angle
        Array1D<Real64> tsolPhiFit(
            state.dataWindowManager->MaxNumOfIncidentAngles); // Glazing system solar transmittance from fit at each incidence angle
        Array1D<Real64> tvisPhiFit(
            state.dataWindowManager->MaxNumOfIncidentAngles); // Glazing system visible transmittance from fit at each incidence angle
        Array2D<Real64> tBareSolPhi(5,
                                    state.dataWindowManager->MaxNumOfIncidentAngles); // Isolated glass solar transmittance for each incidence angle
        Real64 t1;                                                                    // = tBareSolPhi(,1)(,2)
        Real64 t2;
        Array2D<Real64> tBareVisPhi(5,
                                    state.dataWindowManager->MaxNumOfIncidentAngles); // Isolated glass visible transmittance for each incidence angle
        Real64 t1v;                                                                   // = tBareVisPhi(,1)(,2)
        Real64 t2v;
        Array2D<Real64> rfBareSolPhi(
            5,
            state.dataWindowManager->MaxNumOfIncidentAngles); // Isolated glass front solar reflectance for each incidence angle
        Array2D<Real64> rfBareVisPhi(
            5,
            state.dataWindowManager->MaxNumOfIncidentAngles); // Isolated glass front visible reflectance for each incidence angle
        Array2D<Real64> rbBareSolPhi(
            5,
            state.dataWindowManager->MaxNumOfIncidentAngles); // Isolated glass back solar reflectance for each incidence angle
        Array2D<Real64> rbBareVisPhi(
            5,
            state.dataWindowManager->MaxNumOfIncidentAngles); // Isolated glass back visible reflectance for each incidence angle
        Array2D<Real64> afBareSolPhi(
            5,
            state.dataWindowManager->MaxNumOfIncidentAngles); // Isolated glass front solar absorptance for each incidence angle
        Real64 af1;                                           // = afBareSolPhi(,1)(,2)
        Real64 af2;
        Real64 rbmf2; // Isolated glass #2 front beam reflectance
        Array2D<Real64> abBareSolPhi(
            5,
            state.dataWindowManager->MaxNumOfIncidentAngles); // Isolated glass back solar absorptance for each incidence angle
        Real64 ab1;                                           // = abBareSolPhi(,1)(,2)
        Real64 ab2;
        Real64 td1; // Isolated glass diffuse solar transmittance
        Real64 td2;
        Real64 td3;
        Real64 td1v; // Isolated glass diffuse visible transmittance
        Real64 td2v;
        Real64 td3v;
        Real64 rf1; // Isolated glass diffuse solar front reflectance
        Real64 rf2;
        Real64 rf3;
        Real64 rf1v; // Isolated glass diffuse visible front reflectance
        Real64 rf2v;
        Real64 rf3v;
        Real64 rb1; // Isolated glass diffuse solar back reflectance
        Real64 rb2;
        Real64 rb3;
        Real64 rb1v; // Isolated glass diffuse visible back reflectance
        Real64 rb2v;
        Real64 rb3v;
        Real64 afd1; // Isolated glass diffuse solar front absorptance
        Real64 afd2;
        Real64 afd3;
        Real64 abd1; // Isolated glass diffuse solar back absorptance
        Real64 abd2;
        Real64 abd3;
        Real64 TauShIR;  // IR transmittance of isolated shade
        Real64 EpsShIR;  // IR absorptance of isolated shade
        Real64 RhoShIR;  // IR reflectance of isolated shade
        Real64 EpsGlIR;  // IR absorptance of front or back of isolated glass
        Real64 RhoGlIR;  // IR reflectance of inside face of inside glass
        int NGlass;      // Number of glass layers in a construction
        int IGlass;      // Glass layer counter
        int LayNum;      // Layer number for a glass layer
        int LayPtr;      // Material number corresponding to LayNum
        int IPhi;        // Incidence angle counter
        Real64 Phi;      // Incidence angle (deg)
        Real64 CosPhi;   // Cosine of incidence angle
        int ILam;        // Wavelength counter
        Real64 tsolDiff; // Glazing system diffuse solar transmittance
        Real64 tvisDiff; // Glazing system diffuse visible transmittance
        int IGlassBack;  // Glass layer number counted from back of window
        Real64 ShadeAbs; // Solar absorptance of isolated shade
        Real64 ash;      // = ShadeAbs
        Real64 afsh;     // Diffuse solar front absorptance of isolated blind
        Real64 afshGnd;  // Ground and sky diffuse solar front absorptance of isolated blind
        Real64 afshSky;
        Real64 absh;          // Diffuse solar back absorptance of isolated blind
        Real64 ShadeTrans;    // Solar transmittance of isolated shade/blind
        Real64 ShadeTransGnd; // Diffuse-diffuse transmittance of isolated vertical blind with
        // horizontal slats for isotropic ground solar
        Real64 ShadeTransSky; // Diffuse-diffuse transmittance of isolated vertical blind with
        // horizontal slats for isotropic sky solar
        Real64 tsh;    // = ShadeTrans
        Real64 tshGnd; // = ShadeTransGnd,ShadeTransSky
        Real64 tshSky;
        Real64 tsh2;         // = tsh**2
        Real64 ShadeRefl;    // Solar reflectance of isolated shade
        Real64 ShadeReflGnd; // Front blind reflectance for ground diffuse solar
        Real64 ShadeReflSky; // Front blind reflectance for sky diffuse solar
        Real64 rsh;          // = ShadeRefl
        Real64 rfsh;         // Diffuse solar front reflectance of isolated blind
        Real64 rfshGnd;      // Ground and sky diffuse solar front reflectance of isolated blind
        Real64 rfshSky;
        Real64 rbsh;            // Diffuse solar back reflectance of isolated blind
        Real64 ShadeReflFac;    // Shade/blind solar reflection factor
        Real64 ShadeTransVis;   // Visible transmittance of isolated shade/blind
        Real64 tshv;            // = ShadeTransVis
        Real64 tshv2;           // = tshv**2
        Real64 ShadeReflVis;    // Visible reflectance of isolated shade
        Real64 rshv;            // = ShadeReflVis
        Real64 rfshv;           // Diffuse visible front reflectance of isolated blind
        Real64 rbshv;           // Diffuse visible back reflectance of isolated blind
        Real64 ShadeReflFacVis; // Shade/blind visible reflection factor
        int SpecDataNum;        // Spectral data set number
        int numptDAT;           // Number of wavelengths in a spectral data set
        int ISlatAng;           // Slat angle counter
        bool StormWinConst;     // True if a construction with a storm window
        bool Triangle;          // True if window is triangular
        bool Rectangle;         // True if window is rectangular
        Array1D<Real64> W1(3);  // Window vertices (m)
        Array1D<Real64> W2(3);
        Array1D<Real64> W3(3);
        Array1D<Real64> W21(3); // W1-W2, W3-W2, resp. (m)
        Array1D<Real64> W23(3);

        W5InitGlassParameters(state);

        // Calculate optical properties of blind-type layers entered with MATERIAL:WindowBlind
        if (state.dataHeatBal->TotBlinds > 0) CalcWindowBlindProperties(state);

        // Initialize SurfaceScreen structure
        if (state.dataHeatBal->NumSurfaceScreens > 0) CalcWindowScreenProperties(state);

        // Get glazing system optical properties of constructions with glass or glass plus
        //   shade, screen or blind
        // Loop over constructions and find those that are glazing constructions
        for (ConstrNum = 1; ConstrNum <= state.dataHeatBal->TotConstructs; ++ConstrNum) {
            if (!state.dataConstruction->Construct(ConstrNum).TypeIsWindow) continue;
            if (state.dataConstruction->Construct(ConstrNum).WindowTypeBSDF) continue; // Skip Complex Fenestrations, they have separate
            if (state.dataConstruction->Construct(ConstrNum).WindowTypeEQL) continue;  // skip Equivalent Layer Fenestration
            // handling of optical properties

            for (IPhi = 1; IPhi <= 10; ++IPhi) {
                state.dataWindowManager->CosPhiIndepVar(IPhi) = std::cos((IPhi - 1) * 10.0 * DataGlobalConstants::DegToRadians);
            }

            TotLay = state.dataConstruction->Construct(ConstrNum).TotLayers;

            // First layer must be glass, shade, screen or blind to be a glazing construction
            if (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).Group !=
                    DataHeatBalance::MaterialGroup::WindowGlass &&
                state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).Group !=
                    DataHeatBalance::MaterialGroup::Shade &&
                state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).Group !=
                    DataHeatBalance::MaterialGroup::Screen &&
                state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).Group !=
                    DataHeatBalance::MaterialGroup::WindowBlind &&
                state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).Group !=
                    DataHeatBalance::MaterialGroup::WindowSimpleGlazing)
                continue;

            ShadeLayNum = 0;
            ExtShade = false;
            IntShade = false;
            BGShade = false;
            ExtBlind = false;
            IntBlind = false;
            BGBlind = false;
            ExtScreen = false;
            StormWinConst = false;
            state.dataWindowManager->lSimpleGlazingSystem = false;

            if (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).Group ==
                DataHeatBalance::MaterialGroup::WindowSimpleGlazing) {
                // what if outside layer is shade, blind, or screen?
                state.dataWindowManager->lSimpleGlazingSystem = true;
                state.dataWindowManager->SimpleGlazingSHGC =
                    state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).SimpleWindowSHGC;
                state.dataWindowManager->SimpleGlazingU =
                    state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).SimpleWindowUfactor;
            }

            if (has_prefix(state.dataConstruction->Construct(ConstrNum).Name, "BARECONSTRUCTIONWITHSTORMWIN") ||
                has_prefix(state.dataConstruction->Construct(ConstrNum).Name, "SHADEDCONSTRUCTIONWITHSTORMWIN"))
                StormWinConst = true;

            // Get layer number of shade/blind
            if (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).Group ==
                DataHeatBalance::MaterialGroup::Shade) {
                ExtShade = true;
                ShadeLayNum = 1;
            } else if (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(TotLay)).Group ==
                       DataHeatBalance::MaterialGroup::Shade) {
                IntShade = true;
                ShadeLayNum = TotLay;
            } else if (state.dataConstruction->Construct(ConstrNum).TotLayers == 5) {
                if (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(3)).Group ==
                    DataHeatBalance::MaterialGroup::Shade) {
                    BGShade = true;
                    ShadeLayNum = 3;
                }
            } else if (state.dataConstruction->Construct(ConstrNum).TotLayers == 7) {
                if (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(5)).Group ==
                    DataHeatBalance::MaterialGroup::Shade) {
                    BGShade = true;
                    ShadeLayNum = 5;
                }
            }

            if (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).Group ==
                DataHeatBalance::MaterialGroup::WindowBlind) {
                ExtBlind = true;
                ShadeLayNum = 1;
                BlNum = state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(ShadeLayNum)).BlindDataPtr;
            } else if (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(TotLay)).Group ==
                       DataHeatBalance::MaterialGroup::WindowBlind) {
                IntBlind = true;
                ShadeLayNum = TotLay;
                BlNum = state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(ShadeLayNum)).BlindDataPtr;
            } else if (state.dataConstruction->Construct(ConstrNum).TotLayers == 5) {
                if (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(3)).Group ==
                    DataHeatBalance::MaterialGroup::WindowBlind) {
                    BGBlind = true;
                    ShadeLayNum = 3;
                    BlNum = state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(ShadeLayNum)).BlindDataPtr;
                }
            } else if (state.dataConstruction->Construct(ConstrNum).TotLayers == 7) {
                if (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(5)).Group ==
                    DataHeatBalance::MaterialGroup::WindowBlind) {
                    BGBlind = true;
                    ShadeLayNum = 5;
                    BlNum = state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(ShadeLayNum)).BlindDataPtr;
                }
            }

            if (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).Group ==
                DataHeatBalance::MaterialGroup::Screen) {
                ShadeLayNum = 1;
                ScNum = state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(ShadeLayNum)).ScreenDataPtr;
                //   Disregard orphaned constructs with exterior screen
                if (ScNum == 0) continue;
                ExtScreen = true;
            }

            ScreenOn = ExtScreen;
            BlindOn = IntBlind || ExtBlind || BGBlind;
            ShadeOn = IntShade || ExtShade || BGShade;
            state.dataWindowManager->BGFlag = BGBlind || BGShade;

            // For construction with interior or exterior shade, get shade thermal absorptance (emissivity)
            // (accounting for inter-reflection with glazing) and correct the inside glass InsideAbsorpThermal
            // for presence of interior shade. Assumes inner and outer glass layers have zero thermal transmittance.

            if (IntShade || ExtShade || ExtScreen) {
                ShadeLayPtr = state.dataConstruction->Construct(ConstrNum).LayerPoint(ShadeLayNum);
                if (ExtScreen) {
                    TauShIR = state.dataHeatBal->SurfaceScreens(ScNum).DifDifTrans;
                } else {
                    TauShIR = state.dataMaterial->Material(ShadeLayPtr).TransThermal;
                }
                EpsShIR = state.dataMaterial->Material(ShadeLayPtr).AbsorpThermal;
                RhoShIR = max(0.0, 1.0 - TauShIR - EpsShIR);
                if (ExtShade || ExtScreen) { // Exterior shade or screen
                    EpsGlIR = state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(2)).AbsorpThermalFront;
                } else { // Interior shade
                    EpsGlIR = state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(TotLay - 1)).AbsorpThermalBack;
                }
                RhoGlIR = max(0.0, 1.0 - EpsGlIR);
                state.dataConstruction->Construct(ConstrNum).ShadeAbsorpThermal = EpsShIR * (1.0 + TauShIR * RhoGlIR / (1.0 - RhoShIR * RhoGlIR));
                if (IntShade) state.dataConstruction->Construct(ConstrNum).InsideAbsorpThermal *= TauShIR / (1.0 - RhoShIR * RhoGlIR);
            }

            // From the individual glass layer properties, get the glazing system optical properties
            // for BARE GLASS (i.e., interior, exterior or between-glass shade or blind, or exterior screen, if present, not in place).
            // Get one set of system properties for solar incident on front of
            // window and a second set for solar incident on back of window. (The back-incident
            // properties are used with interior short-wave radiation striking the window from inside.)

            // After the front and back system optical properties are calculated for bare glass,
            // a correction is made for the effect of a shade, screen or blind if one of these
            // is present in the construction.

            NGlass = state.dataConstruction->Construct(ConstrNum).TotGlassLayers;

            //--------------------------------------------------------------------------------------------
            // Front calculation (solar incident from outside of room); bare glass portion of construction
            //--------------------------------------------------------------------------------------------

            lquasi = false;
            AllGlassIsSpectralAverage = true;
            int TotalIPhi = 10;
            state.dataWindowManager->LayerNum = {0};

            // Loop over glass layers in the construction
            for (IGlass = 1; IGlass <= NGlass; ++IGlass) {
                LayNum = 1 + 2 * (IGlass - 1);
                if (ExtShade || ExtBlind || ExtScreen) LayNum = 2 + 2 * (IGlass - 1);
                if (BGShade || BGBlind) {
                    LayNum = 1;
                    if (NGlass == 2) {
                        if (IGlass == 2) LayNum = 5;
                    } else { // NGlass = 3
                        if (IGlass == 2) LayNum = 3;
                        if (IGlass == 3) LayNum = 7;
                    }
                }

                state.dataWindowManager->LayerNum[IGlass - 1] = LayNum;
                LayPtr = state.dataConstruction->Construct(ConstrNum).LayerPoint(LayNum);
                SpecDataNum = state.dataMaterial->Material(LayPtr).GlassSpectralDataPtr;
                if (SpecDataNum != 0) {
                    if (!state.dataWindowManager->BGFlag) AllGlassIsSpectralAverage = false;

                    // Get the spectral data for the transmittance, front reflectance and
                    // back reflectance (all at normal incidence) for this layer.
                    // In this case, "front" means incident from the outside and "back"
                    // means incident from the inside.
                    numptDAT = state.dataHeatBal->SpectralData(SpecDataNum).NumOfWavelengths;
                    state.dataWindowManager->numpt[IGlass - 1] = numptDAT;

                    for (ILam = 1; ILam <= numptDAT; ++ILam) {
                        state.dataWindowManager->wlt[IGlass - 1][ILam - 1] = state.dataHeatBal->SpectralData(SpecDataNum).WaveLength(ILam);
                        state.dataWindowManager->t[IGlass - 1][ILam - 1] = state.dataHeatBal->SpectralData(SpecDataNum).Trans(ILam);
                        if ((IGlass == 1 || (IGlass == 2 && StormWinConst)) && (!state.dataWindowManager->BGFlag))
                            state.dataWindowManager->t[IGlass - 1][ILam - 1] *= state.dataMaterial->Material(LayPtr).GlassTransDirtFactor;
                        state.dataWindowManager->rff[IGlass - 1][ILam - 1] = state.dataHeatBal->SpectralData(SpecDataNum).ReflFront(ILam);
                        state.dataWindowManager->rbb[IGlass - 1][ILam - 1] = state.dataHeatBal->SpectralData(SpecDataNum).ReflBack(ILam);
                    }

                    // If there is spectral data for between-glass shades or blinds, calc the average spectral properties for use.
                    if (state.dataWindowManager->BGFlag) {
                        // Add warning message for the glazing defined with full spectral data.
                        ShowWarningError(state,
                                         "Window glazing material \"" + state.dataMaterial->Material(LayPtr).Name +
                                             "\" was defined with full spectral data and has been converted to average spectral data");
                        ShowContinueError(state,
                                          "due to its use with between-glass shades or blinds of the window construction \"" +
                                              state.dataConstruction->Construct(ConstrNum).Name + "\".");
                        ShowContinueError(state, "All occurrences of this glazing material will be modeled as SpectralAverage.");
                        ShowContinueError(state,
                                          "If this material is also used in other window constructions  without between-glass shades or blinds,");
                        ShowContinueError(state,
                                          "then make a duplicate material (with new name) if you want to model those windows  (and reference the new "
                                          "material) using the full spectral data.");
                        // calc Trans, TransVis, ReflectSolBeamFront, ReflectSolBeamBack, ReflectVisBeamFront, ReflectVisBeamBack
                        //  assuming wlt same as wle
                        state.dataWindowManager->tmpTrans = solarSpectrumAverage(state, state.dataWindowManager->t[0]);
                        state.dataWindowManager->tmpReflectSolBeamFront = solarSpectrumAverage(state, state.dataWindowManager->rff[0]);
                        state.dataWindowManager->tmpReflectSolBeamBack = solarSpectrumAverage(state, state.dataWindowManager->rbb[0]);

                        // visible properties
                        state.dataWindowManager->tmpTransVis = visibleSpectrumAverage(state, state.dataWindowManager->t[0]);
                        state.dataWindowManager->tmpReflectVisBeamFront = visibleSpectrumAverage(state, state.dataWindowManager->rff[0]);
                        state.dataWindowManager->tmpReflectVisBeamBack = visibleSpectrumAverage(state, state.dataWindowManager->rbb[0]);

                        // set this material to average spectral data
                        state.dataMaterial->Material(LayPtr).GlassSpectralDataPtr = 0;
                        state.dataMaterial->Material(LayPtr).Trans = state.dataWindowManager->tmpTrans;
                        state.dataMaterial->Material(LayPtr).TransVis = state.dataWindowManager->tmpTransVis;
                        state.dataMaterial->Material(LayPtr).ReflectSolBeamFront = state.dataWindowManager->tmpReflectSolBeamFront;
                        state.dataMaterial->Material(LayPtr).ReflectSolBeamBack = state.dataWindowManager->tmpReflectSolBeamBack;
                        state.dataMaterial->Material(LayPtr).ReflectVisBeamFront = state.dataWindowManager->tmpReflectVisBeamFront;
                        state.dataMaterial->Material(LayPtr).ReflectVisBeamBack = state.dataWindowManager->tmpReflectVisBeamBack;
                        SpecDataNum = 0;
                    }
                }

                if (SpecDataNum == 0 &&
                    !state.dataMaterial->Material(LayPtr).GlassSpectralAndAngle) { // No spectral data for this layer; use spectral average values
                    lquasi = true;
                    state.dataWindowManager->numpt[IGlass - 1] = 2;
                    state.dataWindowManager->t[IGlass - 1][0] = state.dataMaterial->Material(LayPtr).Trans;
                    if (IGlass == 1 || (IGlass == 2 && StormWinConst))
                        state.dataWindowManager->t[IGlass - 1][0] *= state.dataMaterial->Material(LayPtr).GlassTransDirtFactor;
                    state.dataWindowManager->t[IGlass - 1][1] = state.dataMaterial->Material(LayPtr).TransVis;
                    if (IGlass == 1 || (IGlass == 2 && StormWinConst))
                        state.dataWindowManager->t[IGlass - 1][1] *= state.dataMaterial->Material(LayPtr).GlassTransDirtFactor;
                    state.dataWindowManager->rff[IGlass - 1][0] = state.dataMaterial->Material(LayPtr).ReflectSolBeamFront;
                    state.dataWindowManager->rbb[IGlass - 1][0] = state.dataMaterial->Material(LayPtr).ReflectSolBeamBack;
                    state.dataWindowManager->rff[IGlass - 1][1] = state.dataMaterial->Material(LayPtr).ReflectVisBeamFront;
                    state.dataWindowManager->rbb[IGlass - 1][1] = state.dataMaterial->Material(LayPtr).ReflectVisBeamBack;
                }
                if (state.dataMaterial->Material(LayPtr).GlassSpectralAndAngle) {
                    if (!state.dataWindowManager->BGFlag) AllGlassIsSpectralAverage = false;
                    numptDAT = state.dataWindowManager->wle.size();
                    state.dataWindowManager->numpt[IGlass - 1] = numptDAT;
                    if (state.dataWindowManager->BGFlag) {
                        // 5/16/2012 CR 8793. Add warning message for the glazing defined with full spectral data.
                        ShowWarningError(state,
                                         "Window glazing material \"" + state.dataMaterial->Material(LayPtr).Name +
                                             "\" was defined with full spectral and angular data and has been converted to average spectral data");
                        ShowContinueError(state,
                                          "due to its use with between-glass shades or blinds of the window construction \"" +
                                              state.dataConstruction->Construct(ConstrNum).Name + "\".");
                        ShowContinueError(state, "All occurrences of this glazing material will be modeled as SpectralAverage.");
                        ShowContinueError(state,
                                          "If this material is also used in other window constructions  without between-glass shades or blinds,");
                        ShowContinueError(state,
                                          "then make a duplicate material (with new name) if you want to model those windows  (and reference the new "
                                          "material) using the full spectral data.");
                        // calc Trans, TransVis, ReflectSolBeamFront, ReflectSolBeamBack, ReflectVisBeamFront, ReflectVisBeamBack
                        //  assuming wlt same as wle
                        for (ILam = 1; ILam <= (int)state.dataWindowManager->wle.size(); ++ILam) {
                            auto lam = state.dataWindowManager->wle[ILam - 1];
                            state.dataWindowManager->wlt[IGlass - 1][ILam - 1] = lam;
                            state.dataWindowManager->t[IGlass - 1][ILam - 1] =
                                CurveManager::CurveValue(state, state.dataMaterial->Material(LayPtr).GlassSpecAngTransDataPtr, 0.0, lam);
                            state.dataWindowManager->rff[IGlass - 1][ILam - 1] =
                                CurveManager::CurveValue(state, state.dataMaterial->Material(LayPtr).GlassSpecAngFRefleDataPtr, 0.0, lam);
                            state.dataWindowManager->rbb[IGlass - 1][ILam - 1] =
                                CurveManager::CurveValue(state, state.dataMaterial->Material(LayPtr).GlassSpecAngBRefleDataPtr, 0.0, lam);
                        }
                        state.dataWindowManager->tmpTrans = solarSpectrumAverage(state, state.dataWindowManager->t[0]);
                        state.dataWindowManager->tmpReflectSolBeamFront = solarSpectrumAverage(state, state.dataWindowManager->rff[0]);
                        state.dataWindowManager->tmpReflectSolBeamBack = solarSpectrumAverage(state, state.dataWindowManager->rbb[0]);

                        // visible properties
                        state.dataWindowManager->tmpTransVis = visibleSpectrumAverage(state, state.dataWindowManager->t[0]);
                        state.dataWindowManager->tmpReflectVisBeamFront = visibleSpectrumAverage(state, state.dataWindowManager->rff[0]);
                        state.dataWindowManager->tmpReflectVisBeamBack = visibleSpectrumAverage(state, state.dataWindowManager->rbb[0]);

                        // set this material to average spectral data
                        state.dataMaterial->Material(LayPtr).GlassSpectralAndAngle = false;
                        state.dataMaterial->Material(LayPtr).Trans = state.dataWindowManager->tmpTrans;
                        state.dataMaterial->Material(LayPtr).TransVis = state.dataWindowManager->tmpTransVis;
                        state.dataMaterial->Material(LayPtr).ReflectSolBeamFront = state.dataWindowManager->tmpReflectSolBeamFront;
                        state.dataMaterial->Material(LayPtr).ReflectSolBeamBack = state.dataWindowManager->tmpReflectSolBeamBack;
                        state.dataMaterial->Material(LayPtr).ReflectVisBeamFront = state.dataWindowManager->tmpReflectVisBeamFront;
                        state.dataMaterial->Material(LayPtr).ReflectVisBeamBack = state.dataWindowManager->tmpReflectVisBeamBack;
                        SpecDataNum = 0;
                    }
                }
            } // End of loop over glass layers in the construction for front calculation

            if (TotalIPhi > state.dataWindowManager->MaxNumOfIncidentAngles) {
                ShowSevereError(state,
                                format("WindowManage::InitGlassOpticalCalculations = {}, Invalid maximum value of common incidet angles = {}.",
                                       state.dataConstruction->Construct(ConstrNum).Name,
                                       TotalIPhi));
                ShowContinueError(state,
                                  format("The maximum number of incident angles for each construct is {}. Please rearrange the dataset.",
                                         state.dataWindowManager->MaxNumOfIncidentAngles));
                ShowFatalError(state, "Errors found getting inputs. Previous error(s) cause program termination.");
            }

            // Loop over incidence angle from 0 to 90 deg in 10 deg increments.
            // Get glass layer properties, then glazing system properties (which include the
            // effect of inter-reflection among glass layers) at each incidence angle.

            for (IPhi = 1; IPhi <= TotalIPhi; ++IPhi) {
                // 10 degree increment for incident angle is only value for a construction without a layer = SpectralAndAngle
                Phi = double(IPhi - 1) * 10.0;
                CosPhi = std::cos(Phi * DataGlobalConstants::DegToRadians);
                if (std::abs(CosPhi) < 0.0001) CosPhi = 0.0;

                // For each wavelength, get glass layer properties at this angle of incidence
                // from properties at normal incidence
                for (IGlass = 1; IGlass <= NGlass; ++IGlass) {
                    LayPtr = state.dataConstruction->Construct(ConstrNum).LayerPoint(state.dataWindowManager->LayerNum[IGlass - 1]);
                    if (!state.dataMaterial->Material(LayPtr).GlassSpectralAndAngle) {
                        for (ILam = 1; ILam <= state.dataWindowManager->numpt[IGlass - 1]; ++ILam) {
                            TransAndReflAtPhi(CosPhi,
                                              state.dataWindowManager->t[IGlass - 1][ILam - 1],
                                              state.dataWindowManager->rff[IGlass - 1][ILam - 1],
                                              state.dataWindowManager->rbb[IGlass - 1][ILam - 1],
                                              state.dataWindowManager->tPhi[IGlass - 1][ILam - 1],
                                              state.dataWindowManager->rfPhi[IGlass - 1][ILam - 1],
                                              state.dataWindowManager->rbPhi[IGlass - 1][ILam - 1],
                                              state.dataWindowManager->lSimpleGlazingSystem,
                                              state.dataWindowManager->SimpleGlazingSHGC,
                                              state.dataWindowManager->SimpleGlazingU);
                        }
                    } else {
                        for (ILam = 1; ILam <= (int)state.dataWindowManager->wle.size(); ++ILam) {
                            auto lam = state.dataWindowManager->wle[ILam - 1];
                            state.dataWindowManager->wlt[IGlass - 1][ILam - 1] = lam;
                            state.dataWindowManager->tPhi[IGlass - 1][ILam - 1] =
                                CurveManager::CurveValue(state, state.dataMaterial->Material(LayPtr).GlassSpecAngTransDataPtr, Phi, lam);
                            state.dataWindowManager->rfPhi[IGlass - 1][ILam - 1] =
                                CurveManager::CurveValue(state, state.dataMaterial->Material(LayPtr).GlassSpecAngFRefleDataPtr, Phi, lam);
                            state.dataWindowManager->rbPhi[IGlass - 1][ILam - 1] =
                                CurveManager::CurveValue(state, state.dataMaterial->Material(LayPtr).GlassSpecAngBRefleDataPtr, Phi, lam);
                        }
                    }
                    // For use with between-glass shade/blind, save angular properties of isolated glass
                    // for case that all glass layers were input with spectral-average properties
                    //  only used by between-glass shades or blinds
                    if (AllGlassIsSpectralAverage) {
                        tBareSolPhi(IGlass, IPhi) = state.dataWindowManager->tPhi[IGlass - 1][0];
                        tBareVisPhi(IGlass, IPhi) = state.dataWindowManager->tPhi[IGlass - 1][1];
                        rfBareSolPhi(IGlass, IPhi) = state.dataWindowManager->rfPhi[IGlass - 1][0];
                        rfBareVisPhi(IGlass, IPhi) = state.dataWindowManager->rfPhi[IGlass - 1][1];
                        rbBareSolPhi(IGlass, IPhi) = state.dataWindowManager->rbPhi[IGlass - 1][0];
                        rbBareVisPhi(IGlass, IPhi) = state.dataWindowManager->rbPhi[IGlass - 1][1];
                        afBareSolPhi(IGlass, IPhi) = max(0.0, 1.0 - (tBareSolPhi(IGlass, IPhi) + rfBareSolPhi(IGlass, IPhi)));
                        abBareSolPhi(IGlass, IPhi) = max(0.0, 1.0 - (tBareSolPhi(IGlass, IPhi) + rbBareSolPhi(IGlass, IPhi)));
                    }
                }

                // For each wavelength in the solar spectrum, calculate system properties
                // stPhi, srfPhi, srbPhi and saPhi at this angle of incidence.
                // In the following the argument "1" indicates that spectral average solar values
                // should be used for layers without spectral data.
                SystemSpectralPropertiesAtPhi(state, 1, NGlass, 0.0, 2.54);

                // Get solar properties of system by integrating over solar irradiance spectrum.
                // For now it is assumed that the exterior and interior irradiance spectra are the same.
                state.dataWindowManager->tsolPhi(IPhi) = solarSpectrumAverage(state, state.dataWindowManager->stPhi);
                state.dataWindowManager->rfsolPhi(IPhi) = solarSpectrumAverage(state, state.dataWindowManager->srfPhi);
                state.dataWindowManager->rbsolPhi(IPhi) = solarSpectrumAverage(state, state.dataWindowManager->srbPhi);

                for (IGlass = 1; IGlass <= NGlass; ++IGlass) {
                    for (ILam = 1; ILam <= state.dataWindowManager->nume; ++ILam) {
                        sabsPhi(ILam) = state.dataWindowManager->saPhi(IGlass, ILam);
                    }
                    SolarSprectrumAverage(state, sabsPhi, state.dataWindowManager->solabsPhi(IGlass, IPhi));
                }

                // Get visible properties of system by integrating over solar irradiance
                // spectrum weighted by photopic response.
                // Need to redo the calculation of system spectral properties here only if
                // one or more glass layers have no spectral data (lquasi = .TRUE.); in this
                // case the spectral average visible properties will be used for the layers
                // without spectral data, as indicated by the argument "2".

                if (lquasi) SystemSpectralPropertiesAtPhi(state, 2, NGlass, 0.37, 0.78);
                state.dataWindowManager->tvisPhi(IPhi) = visibleSpectrumAverage(state, state.dataWindowManager->stPhi);
                state.dataWindowManager->rfvisPhi(IPhi) = visibleSpectrumAverage(state, state.dataWindowManager->srfPhi);
                state.dataWindowManager->rbvisPhi(IPhi) = visibleSpectrumAverage(state, state.dataWindowManager->srbPhi);

            } // End of loop over incidence angles for front calculation

            //  only used by between-glass shades or blinds
            if (AllGlassIsSpectralAverage) {
                for (IGlass = 1; IGlass <= NGlass; ++IGlass) {
                    W5LsqFit(state.dataWindowManager->CosPhiIndepVar,
                             tBareSolPhi(IGlass, _),
                             6,
                             1,
                             TotalIPhi,
                             state.dataConstruction->Construct(ConstrNum).tBareSolCoef(IGlass));
                    W5LsqFit(state.dataWindowManager->CosPhiIndepVar,
                             tBareVisPhi(IGlass, _),
                             6,
                             1,
                             TotalIPhi,
                             state.dataConstruction->Construct(ConstrNum).tBareVisCoef(IGlass));
                    W5LsqFit(state.dataWindowManager->CosPhiIndepVar,
                             rfBareSolPhi(IGlass, _),
                             6,
                             1,
                             TotalIPhi,
                             state.dataConstruction->Construct(ConstrNum).rfBareSolCoef(IGlass));
                    W5LsqFit(state.dataWindowManager->CosPhiIndepVar,
                             rfBareVisPhi(IGlass, _),
                             6,
                             1,
                             TotalIPhi,
                             state.dataConstruction->Construct(ConstrNum).rfBareVisCoef(IGlass));
                    W5LsqFit(state.dataWindowManager->CosPhiIndepVar,
                             rbBareSolPhi(IGlass, _),
                             6,
                             1,
                             TotalIPhi,
                             state.dataConstruction->Construct(ConstrNum).rbBareSolCoef(IGlass));
                    W5LsqFit(state.dataWindowManager->CosPhiIndepVar,
                             rbBareVisPhi(IGlass, _),
                             6,
                             1,
                             TotalIPhi,
                             state.dataConstruction->Construct(ConstrNum).rbBareVisCoef(IGlass));
                    W5LsqFit(state.dataWindowManager->CosPhiIndepVar,
                             afBareSolPhi(IGlass, _),
                             6,
                             1,
                             TotalIPhi,
                             state.dataConstruction->Construct(ConstrNum).afBareSolCoef(IGlass));
                    W5LsqFit(state.dataWindowManager->CosPhiIndepVar,
                             abBareSolPhi(IGlass, _),
                             6,
                             1,
                             TotalIPhi,
                             state.dataConstruction->Construct(ConstrNum).abBareSolCoef(IGlass));
                }
            }

            state.dataConstruction->Construct(ConstrNum).ReflectSolDiffFront = DiffuseAverage(state.dataWindowManager->rfsolPhi);
            state.dataConstruction->Construct(ConstrNum).ReflectSolDiffBack = DiffuseAverage(state.dataWindowManager->rbsolPhi);
            state.dataConstruction->Construct(ConstrNum).ReflectVisDiffFront = DiffuseAverage(state.dataWindowManager->rfvisPhi);
            state.dataConstruction->Construct(ConstrNum).ReflectVisDiffBack = DiffuseAverage(state.dataWindowManager->rbvisPhi);

            tsolDiff = DiffuseAverage(state.dataWindowManager->tsolPhi);
            tvisDiff = DiffuseAverage(state.dataWindowManager->tvisPhi);
            state.dataConstruction->Construct(ConstrNum).TransDiff = tsolDiff;
            state.dataConstruction->Construct(ConstrNum).TransDiffVis = tvisDiff;
            for (IGlass = 1; IGlass <= NGlass; ++IGlass) {
                solabsPhiLay({1, TotalIPhi}) = state.dataWindowManager->solabsPhi(IGlass, {1, TotalIPhi});
                solabsDiff(IGlass) = DiffuseAverage(solabsPhiLay);
                state.dataConstruction->Construct(ConstrNum).AbsDiff(IGlass) = solabsDiff(IGlass);

                // For use with between-glass shade/blind, get diffuse properties of isolated glass for case when
                // all glass layers were input with spectral-average properties
                //  only used by between-glass shades or blinds
                if (AllGlassIsSpectralAverage) {
                    state.dataConstruction->Construct(ConstrNum).tBareSolDiff(IGlass) = DiffuseAverage(tBareSolPhi(IGlass, {1, TotalIPhi}));
                    state.dataConstruction->Construct(ConstrNum).tBareVisDiff(IGlass) = DiffuseAverage(tBareVisPhi(IGlass, {1, TotalIPhi}));
                    state.dataConstruction->Construct(ConstrNum).rfBareSolDiff(IGlass) = DiffuseAverage(rfBareSolPhi(IGlass, {1, TotalIPhi}));
                    state.dataConstruction->Construct(ConstrNum).rfBareVisDiff(IGlass) = DiffuseAverage(rfBareVisPhi(IGlass, {1, TotalIPhi}));
                    state.dataConstruction->Construct(ConstrNum).rbBareSolDiff(IGlass) = DiffuseAverage(rbBareSolPhi(IGlass, {1, TotalIPhi}));
                    state.dataConstruction->Construct(ConstrNum).rbBareVisDiff(IGlass) = DiffuseAverage(rbBareVisPhi(IGlass, {1, TotalIPhi}));
                    state.dataConstruction->Construct(ConstrNum).afBareSolDiff(IGlass) =
                        max(0.0,
                            1.0 - (state.dataConstruction->Construct(ConstrNum).tBareSolDiff(IGlass) +
                                   state.dataConstruction->Construct(ConstrNum).rfBareSolDiff(IGlass)));
                    state.dataConstruction->Construct(ConstrNum).abBareSolDiff(IGlass) =
                        max(0.0,
                            1.0 - (state.dataConstruction->Construct(ConstrNum).tBareSolDiff(IGlass) +
                                   state.dataConstruction->Construct(ConstrNum).rbBareSolDiff(IGlass)));
                }
            }

            //------------------------------------------------------------------------------------------
            // Back calculation (solar incident from inside of room); bare glass portion of construction
            //------------------------------------------------------------------------------------------

            lquasi = false;
            state.dataWindowManager->LayerNum = {0};

            // Loop over glass layers in the construction.
            for (IGlass = 1; IGlass <= NGlass; ++IGlass) {
                LayNum = 1 + (NGlass - IGlass) * 2;
                if (ExtShade || ExtBlind || ExtScreen) LayNum = 2 + (NGlass - IGlass) * 2;
                if (BGShade || BGBlind) {
                    if (NGlass == 2) {
                        if (IGlass == 1) LayNum = 5;
                        if (IGlass == 2) LayNum = 1;
                    } else { // NGlass = 3
                        if (IGlass == 1) LayNum = 7;
                        if (IGlass == 2) LayNum = 3;
                        if (IGlass == 3) LayNum = 1;
                    }
                }
                state.dataWindowManager->LayerNum[IGlass - 1] = LayNum;
                LayPtr = state.dataConstruction->Construct(ConstrNum).LayerPoint(LayNum);

                SpecDataNum = state.dataMaterial->Material(LayPtr).GlassSpectralDataPtr;
                if (SpecDataNum != 0) {

                    // Get the spectral data for the transmittance, front reflectance and
                    // back reflectance (all at normal incidence) for this layer.
                    // In this case, "front" means incident from the inside and "back"
                    // means incident from the outside.

                    numptDAT = state.dataHeatBal->SpectralData(SpecDataNum).NumOfWavelengths;
                    state.dataWindowManager->numpt[IGlass - 1] = numptDAT;

                    for (ILam = 1; ILam <= numptDAT; ++ILam) {
                        state.dataWindowManager->wlt[IGlass - 1][ILam - 1] = state.dataHeatBal->SpectralData(SpecDataNum).WaveLength(ILam);
                        state.dataWindowManager->t[IGlass - 1][ILam - 1] = state.dataHeatBal->SpectralData(SpecDataNum).Trans(ILam);
                        if (IGlass == NGlass || (IGlass == (NGlass - 1) && StormWinConst))
                            state.dataWindowManager->t[IGlass - 1][ILam - 1] *= state.dataMaterial->Material(LayPtr).GlassTransDirtFactor;
                        state.dataWindowManager->rff[IGlass - 1][ILam - 1] = state.dataHeatBal->SpectralData(SpecDataNum).ReflBack(ILam);
                        state.dataWindowManager->rbb[IGlass - 1][ILam - 1] = state.dataHeatBal->SpectralData(SpecDataNum).ReflFront(ILam);
                    }

                } else { // No spectral data for this layer; use spectral average values
                    if (!state.dataMaterial->Material(LayPtr).GlassSpectralAndAngle) {
                        lquasi = true;
                        state.dataWindowManager->numpt[IGlass - 1] = 2;
                        state.dataWindowManager->t[IGlass - 1][0] = state.dataMaterial->Material(LayPtr).Trans;
                        if (IGlass == NGlass || (IGlass == (NGlass - 1) && StormWinConst))
                            state.dataWindowManager->t[IGlass - 1][0] *= state.dataMaterial->Material(LayPtr).GlassTransDirtFactor;
                        state.dataWindowManager->t[IGlass - 1][1] = state.dataMaterial->Material(LayPtr).TransVis;
                        if (IGlass == NGlass || (IGlass == (NGlass - 1) && StormWinConst))
                            state.dataWindowManager->t[IGlass - 1][1] *= state.dataMaterial->Material(LayPtr).GlassTransDirtFactor;
                        state.dataWindowManager->rff[IGlass - 1][0] = state.dataMaterial->Material(LayPtr).ReflectSolBeamBack;
                        state.dataWindowManager->rbb[IGlass - 1][0] = state.dataMaterial->Material(LayPtr).ReflectSolBeamFront;
                        state.dataWindowManager->rff[IGlass - 1][1] = state.dataMaterial->Material(LayPtr).ReflectVisBeamBack;
                        state.dataWindowManager->rbb[IGlass - 1][1] = state.dataMaterial->Material(LayPtr).ReflectVisBeamFront;
                    }
                }
                if (state.dataMaterial->Material(LayPtr).GlassSpectralAndAngle) {
                    numptDAT = state.dataWindowManager->wle.size();
                    state.dataWindowManager->numpt[IGlass - 1] = numptDAT;
                }
            } // End of loop over glass layers in the construction for back calculation

            // Loop over incidence angle from 0 to 90 deg in 10 deg increments.
            // Get bare glass layer properties, then glazing system properties at each incidence angle.
            // The glazing system properties include the effect of inter-reflection among glass layers,
            // but exclude the effect of a shade or blind if present in the construction.
            // When a construction has a layer = SpectralAndAngle, the 10 degree increment will be overridden.
            for (IPhi = 1; IPhi <= TotalIPhi; ++IPhi) {
                Phi = double(IPhi - 1) * 10.0;
                CosPhi = std::cos(Phi * DataGlobalConstants::DegToRadians);
                if (std::abs(CosPhi) < 0.0001) CosPhi = 0.0;

                // For each wavelength, get glass layer properties at this angle of incidence
                // from properties at normal incidence
                for (IGlass = 1; IGlass <= NGlass; ++IGlass) {
                    LayPtr = state.dataConstruction->Construct(ConstrNum).LayerPoint(state.dataWindowManager->LayerNum[IGlass - 1]);
                    if (!state.dataMaterial->Material(LayPtr).GlassSpectralAndAngle) {
                        for (ILam = 1; ILam <= state.dataWindowManager->numpt[IGlass - 1]; ++ILam) {

                            TransAndReflAtPhi(CosPhi,
                                              state.dataWindowManager->t[IGlass - 1][ILam - 1],
                                              state.dataWindowManager->rff[IGlass - 1][ILam - 1],
                                              state.dataWindowManager->rbb[IGlass - 1][ILam - 1],
                                              state.dataWindowManager->tPhi[IGlass - 1][ILam - 1],
                                              state.dataWindowManager->rfPhi[IGlass - 1][ILam - 1],
                                              state.dataWindowManager->rbPhi[IGlass - 1][ILam - 1],
                                              state.dataWindowManager->lSimpleGlazingSystem,
                                              state.dataWindowManager->SimpleGlazingSHGC,
                                              state.dataWindowManager->SimpleGlazingU);
                        }
                    } else {
                        for (ILam = 1; ILam <= (int)state.dataWindowManager->wle.size(); ++ILam) {
                            auto lam = state.dataWindowManager->wle[ILam - 1];
                            state.dataWindowManager->wlt[IGlass - 1][ILam - 1] = lam;
                            state.dataWindowManager->tPhi[IGlass - 1][ILam - 1] =
                                CurveManager::CurveValue(state, state.dataMaterial->Material(LayPtr).GlassSpecAngTransDataPtr, Phi, lam);
                            state.dataWindowManager->rfPhi[IGlass - 1][ILam - 1] =
                                CurveManager::CurveValue(state, state.dataMaterial->Material(LayPtr).GlassSpecAngFRefleDataPtr, Phi, lam);
                            state.dataWindowManager->rbPhi[IGlass - 1][ILam - 1] =
                                CurveManager::CurveValue(state, state.dataMaterial->Material(LayPtr).GlassSpecAngBRefleDataPtr, Phi, lam);
                        }
                    }
                }

                // For each wavelength in the solar spectrum, calculate system properties
                // stPhi, srfPhi, srbPhi and saPhi at this angle of incidence
                SystemSpectralPropertiesAtPhi(state, 1, NGlass, 0.0, 2.54);

                // Get back absorptance properties of system by integrating over solar irradiance spectrum.
                // For now it is assumed that the exterior and interior irradiance spectra are the same.

                for (IGlass = 1; IGlass <= NGlass; ++IGlass) {
                    for (j = 1; j <= state.dataWindowManager->nume; ++j) {
                        sabsPhi(j) = state.dataWindowManager->saPhi(IGlass, j);
                    }
                    SolarSprectrumAverage(state, sabsPhi, state.dataWindowManager->solabsBackPhi(IGlass, IPhi));
                }

            } // End of loop over incidence angles for back calculation

            for (IGlass = 1; IGlass <= NGlass; ++IGlass) {
                IGlassBack = NGlass - IGlass + 1;
                state.dataConstruction->Construct(ConstrNum).AbsDiffBack(IGlass) =
                    DiffuseAverage(state.dataWindowManager->solabsBackPhi(IGlassBack, {1, 10}));
            }

            //-----------------------------------------------------------------------
            // Correction for effect of shade, screen or blind if present in the construction
            //-----------------------------------------------------------------------

            // For construction with shade, screen or blind, get system shading device absorptance
            // and correct the system glass layer absorptances for the effect of reflection
            // and transmission by shade, screen or blind. Get system reflectance (front and back,
            // solar and visible)

            if (ShadeOn || BlindOn || ScreenOn) {

                // Solar and visible properties of isolated shade or blind
                // (Note: for shades or screen we go through the following loop over slat angles only once.)

                Real64 const tsolDiff_2(pow_2(tsolDiff));
                Real64 const tvisDiff_2(pow_2(tvisDiff));
                for (ISlatAng = 1; ISlatAng <= MaxSlatAngs; ++ISlatAng) {

                    if (ShadeOn) {
                        ShadeAbs = state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(ShadeLayNum)).AbsorpSolar;
                        ShadeTrans = state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(ShadeLayNum)).Trans;
                        ShadeTransVis = state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(ShadeLayNum)).TransVis;
                        ShadeRefl = state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(ShadeLayNum)).ReflectShade;
                        ShadeReflVis =
                            state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(ShadeLayNum)).ReflectShadeVis;
                        rsh = ShadeRefl;
                        rshv = ShadeReflVis;
                        tsh = ShadeTrans;
                        tshv = ShadeTransVis;
                        ash = ShadeAbs;
                    } else if (IntBlind || ExtBlind) {
                        ShadeTrans = state.dataHeatBal->Blind(BlNum).SolFrontDiffDiffTrans(ISlatAng);
                        ShadeTransGnd = state.dataHeatBal->Blind(BlNum).SolFrontDiffDiffTransGnd(ISlatAng);
                        ShadeTransSky = state.dataHeatBal->Blind(BlNum).SolFrontDiffDiffTransSky(ISlatAng);
                        ShadeTransVis = state.dataHeatBal->Blind(BlNum).VisFrontDiffDiffTrans(ISlatAng);
                        if (IntBlind) { // Interior blind
                            ShadeAbs = state.dataHeatBal->Blind(BlNum).SolFrontDiffAbs(ISlatAng);
                            ShadeRefl = state.dataHeatBal->Blind(BlNum).SolFrontDiffDiffRefl(ISlatAng);
                            ShadeReflGnd = state.dataHeatBal->Blind(BlNum).SolFrontDiffDiffReflGnd(ISlatAng);
                            ShadeReflSky = state.dataHeatBal->Blind(BlNum).SolFrontDiffDiffReflSky(ISlatAng);
                            ShadeReflVis = state.dataHeatBal->Blind(BlNum).VisFrontDiffDiffRefl(ISlatAng);
                        } else { // Exterior blind
                            ShadeAbs = state.dataHeatBal->Blind(BlNum).SolBackDiffAbs(ISlatAng);
                            ShadeRefl = state.dataHeatBal->Blind(BlNum).SolBackDiffDiffRefl(ISlatAng);
                            ShadeReflVis = state.dataHeatBal->Blind(BlNum).VisBackDiffDiffRefl(ISlatAng);
                        }
                    } else if (BGBlind) {
                        tsh = state.dataHeatBal->Blind(BlNum).SolFrontDiffDiffTrans(ISlatAng);
                        tshGnd = state.dataHeatBal->Blind(BlNum).SolFrontDiffDiffTransGnd(ISlatAng);
                        tshSky = state.dataHeatBal->Blind(BlNum).SolFrontDiffDiffTransSky(ISlatAng);
                        tshv = state.dataHeatBal->Blind(BlNum).VisFrontDiffDiffTrans(ISlatAng);
                        rfsh = state.dataHeatBal->Blind(BlNum).SolFrontDiffDiffRefl(ISlatAng);
                        rfshGnd = state.dataHeatBal->Blind(BlNum).SolFrontDiffDiffReflGnd(ISlatAng);
                        rfshSky = state.dataHeatBal->Blind(BlNum).SolFrontDiffDiffReflSky(ISlatAng);
                        rfshv = state.dataHeatBal->Blind(BlNum).VisFrontDiffDiffRefl(ISlatAng);
                        rbsh = state.dataHeatBal->Blind(BlNum).SolBackDiffDiffRefl(ISlatAng);
                        rbshv = state.dataHeatBal->Blind(BlNum).VisBackDiffDiffRefl(ISlatAng);
                        afsh = state.dataHeatBal->Blind(BlNum).SolFrontDiffAbs(ISlatAng);
                        afshGnd = state.dataHeatBal->Blind(BlNum).SolFrontDiffAbsGnd(ISlatAng);
                        afshSky = state.dataHeatBal->Blind(BlNum).SolFrontDiffAbsSky(ISlatAng);
                        absh = state.dataHeatBal->Blind(BlNum).SolBackDiffAbs(ISlatAng);
                    } else if (ScreenOn && ScNum > 0) {
                        //       diffuse screen properties are calculated during initialization (quarter-hemispherical integration of beam properties)
                        ShadeAbs = state.dataHeatBal->SurfaceScreens(ScNum).DifScreenAbsorp;
                        ShadeTrans = state.dataHeatBal->SurfaceScreens(ScNum).DifDifTrans;
                        ShadeTransVis = state.dataHeatBal->SurfaceScreens(ScNum).DifDifTransVis;
                        ShadeRefl = state.dataHeatBal->SurfaceScreens(ScNum).DifReflect;
                        ShadeReflVis = state.dataHeatBal->SurfaceScreens(ScNum).DifReflectVis;
                        rsh = ShadeRefl;
                        rshv = ShadeReflVis;
                        tsh = ShadeTrans;
                        tshv = ShadeTransVis;
                        ash = ShadeAbs;
                    }

                    // Correction factors for inter-reflections between glass and shading device

                    if (ExtShade || ExtBlind || ExtScreen) {
                        ShadeReflFac = 1.0 / (1.0 - ShadeRefl * state.dataConstruction->Construct(ConstrNum).ReflectSolDiffFront);
                        ShadeReflFacVis = 1.0 / (1.0 - ShadeReflVis * state.dataConstruction->Construct(ConstrNum).ReflectVisDiffFront);
                    } else if (IntShade || IntBlind) {
                        ShadeReflFac = 1.0 / (1.0 - ShadeRefl * state.dataConstruction->Construct(ConstrNum).ReflectSolDiffBack);
                        ShadeReflFacVis = 1.0 / (1.0 - ShadeReflVis * state.dataConstruction->Construct(ConstrNum).ReflectVisDiffBack);
                    }

                    if (ExtShade || ExtBlind || ExtScreen) { // Exterior shade or blind

                        // Front incident solar, beam, exterior shade, screen or blind

                        if (ExtShade) {
                            for (IPhi = 1; IPhi <= 10; ++IPhi) {
                                for (IGlass = 1; IGlass <= NGlass; ++IGlass) {
                                    state.dataWindowManager->solabsPhi(IGlass, IPhi) = ShadeTrans * solabsDiff(IGlass) * ShadeReflFac;
                                }
                                state.dataWindowManager->tsolPhi(IPhi) = ShadeTrans * ShadeReflFac * tsolDiff;
                                state.dataWindowManager->tvisPhi(IPhi) = ShadeTransVis * ShadeReflFacVis * tvisDiff;
                                state.dataWindowManager->solabsShadePhi(IPhi) =
                                    ShadeAbs * (1.0 + ShadeTrans * ShadeReflFac * state.dataConstruction->Construct(ConstrNum).ReflectSolDiffFront);
                            }
                        }

                        // Front incident solar, diffuse, exterior shade/screen/blind

                        for (IGlass = 1; IGlass <= NGlass; ++IGlass) {
                            if (ExtBlind) {
                                state.dataConstruction->Construct(ConstrNum).BlAbsDiff(ISlatAng, IGlass) =
                                    ShadeTrans * ShadeReflFac * solabsDiff(IGlass);
                                state.dataConstruction->Construct(ConstrNum).BlAbsDiffGnd(ISlatAng, IGlass) =
                                    ShadeTransGnd * ShadeReflFac * solabsDiff(IGlass);
                                state.dataConstruction->Construct(ConstrNum).BlAbsDiffSky(ISlatAng, IGlass) =
                                    ShadeTransSky * ShadeReflFac * solabsDiff(IGlass);
                            }
                            if (ExtShade || ExtScreen)
                                state.dataConstruction->Construct(ConstrNum).AbsDiff(IGlass) = ShadeTrans * ShadeReflFac * solabsDiff(IGlass);
                        }
                        if (ExtBlind) {
                            state.dataConstruction->Construct(ConstrNum).AbsDiffBlind(ISlatAng) =
                                state.dataHeatBal->Blind(BlNum).SolFrontDiffAbs(ISlatAng) +
                                ShadeTrans * ShadeReflFac * state.dataConstruction->Construct(ConstrNum).ReflectSolDiffFront * ShadeAbs;
                            state.dataConstruction->Construct(ConstrNum).AbsDiffBlindGnd(ISlatAng) =
                                state.dataHeatBal->Blind(BlNum).SolFrontDiffAbsGnd(ISlatAng) +
                                ShadeTransGnd * ShadeReflFac * state.dataConstruction->Construct(ConstrNum).ReflectSolDiffFront * ShadeAbs;
                            state.dataConstruction->Construct(ConstrNum).AbsDiffBlindSky(ISlatAng) =
                                state.dataHeatBal->Blind(BlNum).SolFrontDiffAbsSky(ISlatAng) +
                                ShadeTransSky * ShadeReflFac * state.dataConstruction->Construct(ConstrNum).ReflectSolDiffFront * ShadeAbs;
                            state.dataConstruction->Construct(ConstrNum).BlTransDiff(ISlatAng) = tsolDiff * ShadeReflFac * ShadeTrans;
                            state.dataConstruction->Construct(ConstrNum).BlTransDiffGnd(ISlatAng) = tsolDiff * ShadeReflFac * ShadeTransGnd;
                            state.dataConstruction->Construct(ConstrNum).BlTransDiffSky(ISlatAng) = tsolDiff * ShadeReflFac * ShadeTransSky;
                            state.dataConstruction->Construct(ConstrNum).BlTransDiffVis(ISlatAng) = tvisDiff * ShadeReflFacVis * ShadeTransVis;
                            state.dataConstruction->Construct(ConstrNum).BlReflectSolDiffFront(ISlatAng) =
                                ShadeRefl + pow_2(ShadeTrans) * state.dataConstruction->Construct(ConstrNum).ReflectSolDiffFront * ShadeReflFac;
                            state.dataConstruction->Construct(ConstrNum).BlReflectVisDiffFront(ISlatAng) =
                                ShadeReflVis +
                                pow_2(ShadeTransVis) * state.dataConstruction->Construct(ConstrNum).ReflectVisDiffFront * ShadeReflFacVis;
                        }
                        if (ExtShade || ExtScreen) {
                            state.dataConstruction->Construct(ConstrNum).AbsDiffShade =
                                ShadeAbs * (1.0 + ShadeTrans * ShadeReflFac * state.dataConstruction->Construct(ConstrNum).ReflectSolDiffFront);
                            state.dataConstruction->Construct(ConstrNum).TransDiff = tsolDiff * ShadeReflFac * ShadeTrans;
                            state.dataConstruction->Construct(ConstrNum).TransDiffVis = tvisDiff * ShadeReflFacVis * ShadeTransVis;
                            state.dataConstruction->Construct(ConstrNum).ReflectSolDiffFront =
                                ShadeRefl + pow_2(ShadeTrans) * state.dataConstruction->Construct(ConstrNum).ReflectSolDiffFront * ShadeReflFac;
                            state.dataConstruction->Construct(ConstrNum).ReflectVisDiffFront =
                                ShadeReflVis +
                                pow_2(ShadeTransVis) * state.dataConstruction->Construct(ConstrNum).ReflectVisDiffFront * ShadeReflFacVis;
                        }

                        // Back incident solar, diffuse, exterior shade/blind

                        if (ExtBlind) {
                            for (IGlass = 1; IGlass <= NGlass; ++IGlass) {
                                state.dataConstruction->Construct(ConstrNum).BlAbsDiffBack(ISlatAng, IGlass) =
                                    state.dataConstruction->Construct(ConstrNum).AbsDiffBack(IGlass) +
                                    tsolDiff * ShadeRefl * ShadeReflFac * solabsDiff(IGlass);
                            }
                            state.dataConstruction->Construct(ConstrNum).AbsDiffBackBlind(ISlatAng) = tsolDiff * ShadeReflFac * ShadeAbs;
                            state.dataConstruction->Construct(ConstrNum).BlReflectSolDiffBack(ISlatAng) =
                                state.dataConstruction->Construct(ConstrNum).ReflectSolDiffBack + tsolDiff_2 * ShadeRefl * ShadeReflFac;
                            state.dataConstruction->Construct(ConstrNum).BlReflectVisDiffBack(ISlatAng) =
                                state.dataConstruction->Construct(ConstrNum).ReflectVisDiffBack + tvisDiff_2 * ShadeReflVis * ShadeReflFacVis;
                        }
                        if (ExtShade || ExtScreen) {
                            for (IGlass = 1; IGlass <= NGlass; ++IGlass) {
                                state.dataConstruction->Construct(ConstrNum).AbsDiffBack(IGlass) +=
                                    tsolDiff * ShadeRefl * ShadeReflFac * solabsDiff(IGlass);
                            }
                            state.dataConstruction->Construct(ConstrNum).AbsDiffBackShade = tsolDiff * ShadeReflFac * ShadeAbs;
                            state.dataConstruction->Construct(ConstrNum).ReflectSolDiffBack += tsolDiff_2 * ShadeRefl * ShadeReflFac;
                            state.dataConstruction->Construct(ConstrNum).ReflectVisDiffBack += tvisDiff_2 * ShadeReflVis * ShadeReflFacVis;
                        }

                    } // End check if exterior shade, screen or blind

                    if (IntShade || IntBlind) { // Interior shade or blind

                        // Front incident solar, beam, interior shade

                        if (IntShade) {
                            for (IPhi = 1; IPhi <= 10; ++IPhi) {
                                for (IGlass = 1; IGlass <= NGlass; ++IGlass) {
                                    state.dataWindowManager->solabsPhi(IGlass, IPhi) +=
                                        state.dataWindowManager->tsolPhi(IPhi) * ShadeRefl * ShadeReflFac *
                                        state.dataConstruction->Construct(ConstrNum).AbsDiffBack(IGlass);
                                }
                                state.dataWindowManager->solabsShadePhi(IPhi) = state.dataWindowManager->tsolPhi(IPhi) * ShadeReflFac * ShadeAbs;
                                state.dataWindowManager->tsolPhi(IPhi) *= ShadeReflFac * ShadeTrans;
                                state.dataWindowManager->tvisPhi(IPhi) *= ShadeReflFacVis * ShadeTransVis;
                            }
                        } // End of check if interior shade

                        // Front incident solar, diffuse, interior blind

                        if (IntBlind) {
                            for (IGlass = 1; IGlass <= NGlass; ++IGlass) {
                                state.dataConstruction->Construct(ConstrNum).BlAbsDiff(ISlatAng, IGlass) =
                                    state.dataConstruction->Construct(ConstrNum).AbsDiff(IGlass) +
                                    tsolDiff * ShadeRefl * ShadeReflFac * state.dataConstruction->Construct(ConstrNum).AbsDiffBack(IGlass);
                                state.dataConstruction->Construct(ConstrNum).BlAbsDiffGnd(ISlatAng, IGlass) =
                                    state.dataConstruction->Construct(ConstrNum).AbsDiff(IGlass) +
                                    tsolDiff * ShadeReflGnd * ShadeReflFac * state.dataConstruction->Construct(ConstrNum).AbsDiffBack(IGlass);
                                state.dataConstruction->Construct(ConstrNum).BlAbsDiffSky(ISlatAng, IGlass) =
                                    state.dataConstruction->Construct(ConstrNum).AbsDiff(IGlass) +
                                    tsolDiff * ShadeReflSky * ShadeReflFac * state.dataConstruction->Construct(ConstrNum).AbsDiffBack(IGlass);
                            }

                            state.dataConstruction->Construct(ConstrNum).AbsDiffBlind(ISlatAng) = tsolDiff * ShadeReflFac * ShadeAbs;
                            state.dataConstruction->Construct(ConstrNum).AbsDiffBlindGnd(ISlatAng) =
                                tsolDiff * ShadeReflFac * state.dataHeatBal->Blind(BlNum).SolFrontDiffAbsGnd(ISlatAng);
                            state.dataConstruction->Construct(ConstrNum).AbsDiffBlindSky(ISlatAng) =
                                tsolDiff * ShadeReflFac * state.dataHeatBal->Blind(BlNum).SolFrontDiffAbsSky(ISlatAng);
                            state.dataConstruction->Construct(ConstrNum).BlTransDiff(ISlatAng) = tsolDiff * ShadeReflFac * ShadeTrans;
                            state.dataConstruction->Construct(ConstrNum).BlTransDiffGnd(ISlatAng) = tsolDiff * ShadeReflFac * ShadeTransGnd;
                            state.dataConstruction->Construct(ConstrNum).BlTransDiffSky(ISlatAng) = tsolDiff * ShadeReflFac * ShadeTransSky;
                            state.dataConstruction->Construct(ConstrNum).BlTransDiffVis(ISlatAng) = tvisDiff * ShadeReflFacVis * ShadeTransVis;
                            state.dataConstruction->Construct(ConstrNum).BlReflectSolDiffFront(ISlatAng) =
                                state.dataConstruction->Construct(ConstrNum).ReflectSolDiffFront + tsolDiff_2 * ShadeRefl * ShadeReflFac;
                            state.dataConstruction->Construct(ConstrNum).BlReflectVisDiffFront(ISlatAng) =
                                state.dataConstruction->Construct(ConstrNum).ReflectVisDiffFront + tvisDiff_2 * ShadeReflVis * ShadeReflFacVis;

                            // Back incident solar, diffuse, interior blind

                            for (IGlass = 1; IGlass <= NGlass; ++IGlass) {
                                state.dataConstruction->Construct(ConstrNum).BlAbsDiffBack(ISlatAng, IGlass) =
                                    state.dataConstruction->Construct(ConstrNum).AbsDiffBack(IGlass) * ShadeTrans * ShadeReflFac;
                            }

                            state.dataConstruction->Construct(ConstrNum).AbsDiffBackBlind(ISlatAng) =
                                state.dataHeatBal->Blind(BlNum).SolBackDiffAbs(ISlatAng) +
                                ShadeTrans * ShadeReflFac * state.dataConstruction->Construct(ConstrNum).ReflectSolDiffBack * ShadeAbs;
                            state.dataConstruction->Construct(ConstrNum).BlReflectSolDiffBack(ISlatAng) =
                                state.dataHeatBal->Blind(BlNum).SolBackDiffDiffRefl(ISlatAng) +
                                pow_2(ShadeTrans) * state.dataConstruction->Construct(ConstrNum).ReflectSolDiffBack * ShadeReflFac;
                            state.dataConstruction->Construct(ConstrNum).BlReflectVisDiffBack(ISlatAng) =
                                state.dataHeatBal->Blind(BlNum).VisBackDiffDiffRefl(ISlatAng) +
                                pow_2(ShadeTransVis) * state.dataConstruction->Construct(ConstrNum).ReflectVisDiffBack * ShadeReflFacVis;
                        } // End of check if interior blind

                        // Front incident solar, diffuse, interior shade

                        if (IntShade) {
                            for (IGlass = 1; IGlass <= NGlass; ++IGlass) {
                                state.dataConstruction->Construct(ConstrNum).AbsDiff(IGlass) +=
                                    tsolDiff * ShadeRefl * ShadeReflFac * solabsDiff(IGlass);
                            }

                            state.dataConstruction->Construct(ConstrNum).AbsDiffShade = tsolDiff * ShadeReflFac * ShadeAbs;
                            state.dataConstruction->Construct(ConstrNum).TransDiff = tsolDiff * ShadeReflFac * ShadeTrans;
                            state.dataConstruction->Construct(ConstrNum).TransDiffVis = tvisDiff * ShadeReflFacVis * ShadeTransVis;
                            state.dataConstruction->Construct(ConstrNum).ReflectSolDiffFront += tsolDiff_2 * ShadeRefl * ShadeReflFac;
                            state.dataConstruction->Construct(ConstrNum).ReflectVisDiffFront += tvisDiff_2 * ShadeReflVis * ShadeReflFacVis;

                            // Back incident solar, diffuse, interior shade

                            for (IGlass = 1; IGlass <= NGlass; ++IGlass) {
                                state.dataConstruction->Construct(ConstrNum).AbsDiffBack(IGlass) *= ShadeTrans * ShadeReflFac;
                            }

                            state.dataConstruction->Construct(ConstrNum).AbsDiffBackShade =
                                ShadeAbs * (1 + ShadeTrans * ShadeReflFac * state.dataConstruction->Construct(ConstrNum).ReflectSolDiffBack);
                            state.dataConstruction->Construct(ConstrNum).ReflectSolDiffBack =
                                ShadeRefl + pow_2(ShadeTrans) * state.dataConstruction->Construct(ConstrNum).ReflectSolDiffBack * ShadeReflFac;
                            state.dataConstruction->Construct(ConstrNum).ReflectVisDiffBack =
                                ShadeReflVis +
                                pow_2(ShadeTransVis) * state.dataConstruction->Construct(ConstrNum).ReflectVisDiffBack * ShadeReflFacVis;
                        } // End of check if interior shade

                    } // End check if interior shade or blind

                    if (BGShade || BGBlind) { // Between-glass shade/blind; assumed to be between glass #2 and glass #3

                        tsh2 = pow_2(tsh);
                        tshv2 = pow_2(tshv);
                        td1 = state.dataConstruction->Construct(ConstrNum).tBareSolDiff(1);
                        td2 = state.dataConstruction->Construct(ConstrNum).tBareSolDiff(2);
                        td1v = state.dataConstruction->Construct(ConstrNum).tBareVisDiff(1);
                        td2v = state.dataConstruction->Construct(ConstrNum).tBareVisDiff(2);
                        afd1 = state.dataConstruction->Construct(ConstrNum).afBareSolDiff(1);
                        afd2 = state.dataConstruction->Construct(ConstrNum).afBareSolDiff(2);
                        abd1 = state.dataConstruction->Construct(ConstrNum).abBareSolDiff(1);
                        abd2 = state.dataConstruction->Construct(ConstrNum).abBareSolDiff(2);
                        rb1 = state.dataConstruction->Construct(ConstrNum).rbBareSolDiff(1);
                        rb2 = state.dataConstruction->Construct(ConstrNum).rbBareSolDiff(2);
                        rb1v = state.dataConstruction->Construct(ConstrNum).rbBareVisDiff(1);
                        rb2v = state.dataConstruction->Construct(ConstrNum).rbBareVisDiff(2);
                        rf1 = state.dataConstruction->Construct(ConstrNum).rfBareSolDiff(1);
                        rf2 = state.dataConstruction->Construct(ConstrNum).rfBareSolDiff(2);
                        rf1v = state.dataConstruction->Construct(ConstrNum).rfBareVisDiff(1);
                        rf2v = state.dataConstruction->Construct(ConstrNum).rfBareVisDiff(2);

                        if (BGShade) {
                            if (NGlass == 2) {

                                // Front incident solar, beam, between-glass shade, NGlass = 2

                                for (IPhi = 1; IPhi <= 10; ++IPhi) {
                                    t1 = tBareSolPhi(1, IPhi);
                                    t1v = tBareVisPhi(1, IPhi);
                                    af1 = afBareSolPhi(1, IPhi);
                                    ab1 = abBareSolPhi(1, IPhi);
                                    state.dataWindowManager->tsolPhi(IPhi) = t1 * (tsh + rsh * rb1 * tsh + tsh * rf2 * rsh) * td2;
                                    state.dataWindowManager->tvisPhi(IPhi) = t1v * (tshv + rshv * rb1v * tshv + tshv * rf2v * rshv) * td2v;
                                    state.dataWindowManager->solabsShadePhi(IPhi) = t1 * (ash + rsh * rb1 + tsh * rf2) * ash;
                                    state.dataWindowManager->solabsPhi(1, IPhi) = af1 + t1 * (rsh + rsh * rb1 * rsh + tsh * rf2 * tsh) * abd1;
                                    state.dataWindowManager->solabsPhi(2, IPhi) = t1 * (tsh + rsh * rb1 * tsh + tsh * rf2 * rsh) * afd2;
                                } // End of loop over incidence angles

                                // Front incident solar, diffuse, between-glass shade, NGlass = 2

                                state.dataConstruction->Construct(ConstrNum).TransDiff = td1 * (tsh + rsh * rb1 * tsh + tsh * rb2 * rsh) * td2;
                                state.dataConstruction->Construct(ConstrNum).TransDiffVis =
                                    td1v * (tshv + rshv * rb1v * tshv + tshv * rb2v * rshv) * td2v;
                                state.dataConstruction->Construct(ConstrNum).AbsDiffShade = td1 * (ash + rsh * rb1 * ash + tsh * rf2 * ash);
                                state.dataConstruction->Construct(ConstrNum).AbsDiff(1) = afd1 + td1 * (rsh + tsh * rb2 * tsh) * abd1;
                                state.dataConstruction->Construct(ConstrNum).AbsDiff(2) = td1 * (tsh + rsh * rb1 * tsh + tsh * rf2 * rsh) * afd2;
                                state.dataConstruction->Construct(ConstrNum).ReflectSolDiffFront =
                                    rf1 + td1 * (rsh + rsh * rb1 * rsh + tsh * rf2 * tsh) * td1;
                                state.dataConstruction->Construct(ConstrNum).ReflectVisDiffFront =
                                    rf1v + td1v * (rshv + rshv * rb1v * rshv + tshv * rf2v * tshv) * td1v;

                                // Back incident solar, diffuse, between-glass shade, NGlass = 2

                                state.dataConstruction->Construct(ConstrNum).AbsDiffBackShade = td2 * (ash + rsh * rf2 * ash + tsh * rb1 * ash);
                                state.dataConstruction->Construct(ConstrNum).AbsDiffBack(1) = td2 * (tsh + rsh * rf2 * tsh + tsh * rb1 * rsh) * abd1;
                                state.dataConstruction->Construct(ConstrNum).AbsDiffBack(2) =
                                    abd2 + td2 * (rsh + rsh * rf2 * rsh + tsh * rb1 * tsh) * afd2;
                                state.dataConstruction->Construct(ConstrNum).ReflectSolDiffBack =
                                    rb2 + td2 * (rsh + rsh * rf2 * rsh + tsh * rb1 * tsh) * td2;
                                state.dataConstruction->Construct(ConstrNum).ReflectVisDiffBack =
                                    rb2v + td2v * (rshv + rshv * rf2v * rshv + tshv * rb1v * tshv) * td2v;

                            } // End of check if NGlass = 2

                            if (NGlass == 3) {

                                td3 = state.dataConstruction->Construct(ConstrNum).tBareSolDiff(3);
                                td3v = state.dataConstruction->Construct(ConstrNum).tBareVisDiff(3);
                                afd3 = state.dataConstruction->Construct(ConstrNum).afBareSolDiff(3);
                                abd3 = state.dataConstruction->Construct(ConstrNum).abBareSolDiff(3);
                                rb3 = state.dataConstruction->Construct(ConstrNum).rbBareSolDiff(3);
                                rb3v = state.dataConstruction->Construct(ConstrNum).rbBareVisDiff(3);
                                rf3 = state.dataConstruction->Construct(ConstrNum).rfBareSolDiff(3);
                                rf3v = state.dataConstruction->Construct(ConstrNum).rfBareVisDiff(3);

                                // Front incident solar, beam, between-glass shade, NGlass = 3

                                for (IPhi = 1; IPhi <= 10; ++IPhi) {
                                    t1 = tBareSolPhi(1, IPhi);
                                    t1v = tBareVisPhi(1, IPhi);
                                    t2 = tBareSolPhi(2, IPhi);
                                    t2v = tBareVisPhi(2, IPhi);
                                    af1 = afBareSolPhi(1, IPhi);
                                    af2 = afBareSolPhi(2, IPhi);
                                    ab1 = abBareSolPhi(1, IPhi);
                                    ab2 = abBareSolPhi(2, IPhi);
                                    rbmf2 = max(0.0, 1.0 - (t2 + af2));

                                    state.dataWindowManager->tsolPhi(IPhi) =
                                        t1 * t2 * (tsh + tsh * rf3 * rsh + rsh * td2 * rb1 * td2 * tsh + rsh * rb2 * tsh) * td3;
                                    state.dataWindowManager->tvisPhi(IPhi) =
                                        t1v * t2v * (tshv + tshv * rf3v * rshv + rshv * td2v * rb1v * td2v * tshv + rshv * rb2v * tshv) * td3v;
                                    state.dataWindowManager->solabsShadePhi(IPhi) = t1 * t2 * (1 + rsh * td2 * rb1 * td2 + rsh * rb2) * ash;
                                    state.dataWindowManager->solabsPhi(1, IPhi) =
                                        af1 + rbmf2 * ab1 + t1 * t2 * rsh * (1 + rf3 * tsh + rb2 * rsh + td2 * rb1 * td2 * rsh) * td2 * abd1;
                                    state.dataWindowManager->solabsPhi(2, IPhi) =
                                        t1 * af2 + t1 * t2 * ((rsh + tsh * rf3 * tsh + rsh * rb2 * rsh) * abd2 + rsh * td2 * rb1 * afd2);
                                    state.dataWindowManager->solabsPhi(3, IPhi) =
                                        t1 * t2 * (tsh + rsh * (rb2 * tsh + td2 * rb2 * td2 * tsh + rf3 * rsh)) * afd3;
                                } // End of loop over incidence angle

                                // Front incident solar, diffuse, between-glass shade, NGlass = 3

                                state.dataConstruction->Construct(ConstrNum).TransDiff =
                                    td1 * td2 * (tsh + rsh * td2 * rb1 * td2 * tsh + rsh * rb2 * tsh + tsh * rf3 * rsh) * td3;
                                state.dataConstruction->Construct(ConstrNum).TransDiffVis =
                                    td1v * td2v * (tshv + rshv * td2v * rb1v * td2v * tshv + rshv * rb2v * tshv + tshv * rf3v * rshv) * td3v;
                                state.dataConstruction->Construct(ConstrNum).AbsDiffShade =
                                    td1 * td2 * (ash * (1 + rsh * td2 * rb1 * td2 + rsh * rb2 * ash) + tsh * rf3 * ash);
                                state.dataConstruction->Construct(ConstrNum).AbsDiff(1) =
                                    afd1 + td1 * (rf2 + td2 * (rsh + rsh * rb2 * rsh + tsh * rf3 * tsh + rsh * td2 * rb1 * td2 * rsh) * td2) * abd1;
                                state.dataConstruction->Construct(ConstrNum).AbsDiff(2) =
                                    td1 * (afd2 + td2 * (rsh + rsh * rb2 * rsh + tsh * rf3 * tsh) * abd2);
                                state.dataConstruction->Construct(ConstrNum).AbsDiff(3) =
                                    td1 * td2 * (tsh + rsh * rb2 * tsh + rsh * td2 * rb1 * td2 * tsh + tsh * rf3 * rsh) * afd3;
                                state.dataConstruction->Construct(ConstrNum).ReflectSolDiffFront =
                                    rf1 + td1 * rf2 * td1 +
                                    td1 * td2 * (rsh + tsh * rf3 * tsh + rsh * rb2 * rsh + rsh * td2 * rb1 * td2 * rsh) * td2 * td1;
                                state.dataConstruction->Construct(ConstrNum).ReflectVisDiffFront =
                                    rf1v + td1v * rf2v * td1v +
                                    td1v * td2v * (rshv + tshv * rf3v * tshv + rshv * rb2v * rshv + rshv * td2v * rb1v * td2v * rshv) * td2v * td1v;

                                // Back incident solar, diffuse, between-glass shade, NGlass = 3

                                state.dataConstruction->Construct(ConstrNum).AbsDiffBackShade =
                                    td3 * ((1 + rsh * rf3) * ash + (tsh * td2 * rb1 * td2 + tsh * rb2) * ash);
                                state.dataConstruction->Construct(ConstrNum).AbsDiffBack(1) =
                                    td3 * (tsh + rsh * rf3 * tsh + tsh * rb2 * rsh + tsh * td2 * rb1 * td2 * rsh) * td2 * abd1;
                                state.dataConstruction->Construct(ConstrNum).AbsDiffBack(2) =
                                    td3 * ((tsh + rsh * rf3 * tsh) * abd2 + (tsh * td2 * rb1 * td2 + tsh * rb2) * afd2);
                                state.dataConstruction->Construct(ConstrNum).AbsDiffBack(3) =
                                    abd3 + td3 * (rsh + tsh * rb2 * tsh + tsh * td2 * rb1 * td2 * tsh) * afd3;
                                state.dataConstruction->Construct(ConstrNum).ReflectSolDiffBack =
                                    rb3 + td3 * (rsh + rsh * rf3 * rsh + tsh * rb2 * tsh + tsh * td2 * rb1 * td2 * tsh) * td3;
                                state.dataConstruction->Construct(ConstrNum).ReflectVisDiffBack =
                                    rb3v + td3v * (rshv + rshv * rf3 * rshv + tshv * rb2v * tshv + tshv * td2v * rb1v * td2v * tshv) * td3v;

                            } // End of check if NGlass = 3

                        } // End of check if between-glass shade

                        if (BGBlind) {

                            if (NGlass == 2) {

                                // Front incident solar, diffuse, between-glass blind, NGlass = 2

                                state.dataConstruction->Construct(ConstrNum).BlAbsDiff(ISlatAng, 1) =
                                    afd1 + td1 * (rfsh + rfsh * rb1 * rfsh + tsh * rb2 * tsh) * abd1;
                                state.dataConstruction->Construct(ConstrNum).BlAbsDiffGnd(ISlatAng, 1) =
                                    afd1 + td1 * (rfshGnd + rfshGnd * rb1 * rfshGnd + tshGnd * rb2 * tsh) * abd1;
                                state.dataConstruction->Construct(ConstrNum).BlAbsDiffSky(ISlatAng, 1) =
                                    afd1 + td1 * (rfshSky + rfshSky * rb1 * rfshSky + tshSky * rb2 * tsh) * abd1;
                                state.dataConstruction->Construct(ConstrNum).BlAbsDiff(ISlatAng, 2) =
                                    td1 * (tsh + rfsh * rb1 * tsh + tsh * rf2 * rbsh) * afd2;
                                state.dataConstruction->Construct(ConstrNum).BlAbsDiffGnd(ISlatAng, 2) =
                                    td1 * (tshGnd + rfshGnd * rb1 * tsh + tshGnd * rf2 * rbsh) * afd2;
                                state.dataConstruction->Construct(ConstrNum).BlAbsDiffSky(ISlatAng, 2) =
                                    td1 * (tshSky + rfshSky * rb1 * tsh + tshSky * rf2 * rbsh) * afd2;
                                state.dataConstruction->Construct(ConstrNum).AbsDiffBlind(ISlatAng) =
                                    td1 * (afsh + rfsh * rb1 * afsh + tsh * rf2 * absh);
                                state.dataConstruction->Construct(ConstrNum).AbsDiffBlindGnd(ISlatAng) =
                                    td1 * (afshGnd + rfsh * rb1 * afsh + tshGnd * rf2 * absh);
                                state.dataConstruction->Construct(ConstrNum).AbsDiffBlindSky(ISlatAng) =
                                    td1 * (afshSky + rfsh * rb1 * afsh + tshSky * rf2 * absh);
                                state.dataConstruction->Construct(ConstrNum).BlTransDiff(ISlatAng) =
                                    td1 * (tsh + rfsh * rb1 * tsh + tsh * rb2 * rbsh) * td2;
                                state.dataConstruction->Construct(ConstrNum).BlTransDiffGnd(ISlatAng) =
                                    td1 * (tshGnd + rfsh * rb1 * tshGnd + tshGnd * rb2 * rbsh) * td2;
                                state.dataConstruction->Construct(ConstrNum).BlTransDiffSky(ISlatAng) =
                                    td1 * (tshSky + rfsh * rb1 * tshSky + tshSky * rb2 * rbsh) * td2;
                                state.dataConstruction->Construct(ConstrNum).BlTransDiffVis(ISlatAng) =
                                    td1v * (tshv + rfshv * rb1v * tshv + tshv * rb2v * rbshv) * td2v;
                                state.dataConstruction->Construct(ConstrNum).BlReflectSolDiffFront(ISlatAng) =
                                    rf1 + td1 * (rfsh + rfsh * rb1 * rfsh + tsh * rf2 * tsh) * td1;
                                state.dataConstruction->Construct(ConstrNum).BlReflectVisDiffFront(ISlatAng) =
                                    rf1v + td1v * (rfshv + rfshv * rb1v * rfshv + tshv * rf2v * tshv) * td1v;

                                // Back incident solar, diffuse, between-glass blind, NGlass = 2

                                state.dataConstruction->Construct(ConstrNum).BlAbsDiffBack(ISlatAng, 1) =
                                    td2 * (tsh + rbsh * rf2 * tsh + tsh * rb1 * rfsh) * abd1;
                                state.dataConstruction->Construct(ConstrNum).BlAbsDiffBack(ISlatAng, 2) =
                                    abd2 + td2 * (rbsh + rbsh * rf2 * rbsh + tsh * rb1 * tsh) * afd2;
                                state.dataConstruction->Construct(ConstrNum).AbsDiffBackBlind(ISlatAng) =
                                    td2 * (absh + rbsh * rf2 * absh + tsh * rb1 * afsh);
                                state.dataConstruction->Construct(ConstrNum).BlReflectSolDiffBack(ISlatAng) =
                                    rb2 + td2 * (rbsh + rbsh * rf2 * rbsh + tsh * rb1 * tsh) * td2;
                                state.dataConstruction->Construct(ConstrNum).BlReflectVisDiffBack(ISlatAng) =
                                    rb2v + td2v * (rbshv + rbshv * rf2v * rbshv + tshv * rb1v * tshv) * td2v;

                            } // End of check if NGlass = 2

                            if (NGlass == 3) {

                                td3 = state.dataConstruction->Construct(ConstrNum).tBareSolDiff(3);
                                td3v = state.dataConstruction->Construct(ConstrNum).tBareVisDiff(3);
                                afd3 = state.dataConstruction->Construct(ConstrNum).afBareSolDiff(3);
                                abd3 = state.dataConstruction->Construct(ConstrNum).abBareSolDiff(3);
                                rb3 = state.dataConstruction->Construct(ConstrNum).rbBareSolDiff(3);
                                rb3v = state.dataConstruction->Construct(ConstrNum).rbBareVisDiff(3);
                                rf3 = state.dataConstruction->Construct(ConstrNum).rfBareSolDiff(3);
                                rf3v = state.dataConstruction->Construct(ConstrNum).rfBareVisDiff(3);

                                // Front incident solar, diffuse, between-glass blind, NGlass = 3

                                state.dataConstruction->Construct(ConstrNum).BlAbsDiff(ISlatAng, 1) =
                                    afd1 +
                                    td1 * (rf2 + td2 * (rfsh + rfsh * rb2 * rfsh + tsh * rf3 * tsh + rfsh * td2 * rb1 * td2 * rfsh) * td2) * abd1;
                                state.dataConstruction->Construct(ConstrNum).BlAbsDiffGnd(ISlatAng, 1) =
                                    afd1 +
                                    td1 *
                                        (rf2 + td2 * (rfshGnd + rfshGnd * rb2 * rfsh + tshGnd * rf3 * tsh + rfshGnd * td2 * rb1 * td2 * rfsh) * td2) *
                                        abd1;
                                state.dataConstruction->Construct(ConstrNum).BlAbsDiffSky(ISlatAng, 1) =
                                    afd1 +
                                    td1 *
                                        (rf2 + td2 * (rfshSky + rfshSky * rb2 * rfsh + tshSky * rf3 * tsh + rfshSky * td2 * rb1 * td2 * rfsh) * td2) *
                                        abd1;
                                state.dataConstruction->Construct(ConstrNum).BlAbsDiff(ISlatAng, 2) =
                                    td1 * (afd2 + td2 * (rfsh + rfsh * rb2 * rfsh + tsh * rf3 * tsh) * abd2);
                                state.dataConstruction->Construct(ConstrNum).BlAbsDiffGnd(ISlatAng, 2) =
                                    td1 * (afd2 + td2 * (rfshGnd + rfshGnd * rb2 * rfsh + tshGnd * rf3 * tsh) * abd2);
                                state.dataConstruction->Construct(ConstrNum).BlAbsDiffSky(ISlatAng, 2) =
                                    td1 * (afd2 + td2 * (rfshSky + rfshSky * rb2 * rfsh + tshSky * rf3 * tsh) * abd2);
                                state.dataConstruction->Construct(ConstrNum).BlAbsDiff(ISlatAng, 3) =
                                    td1 * td2 * (tsh + rfsh * rb2 * tsh + rfsh * td2 * rb1 * td2 * tsh + tsh * rf3 * rbsh) * afd3;
                                state.dataConstruction->Construct(ConstrNum).BlAbsDiffGnd(ISlatAng, 3) =
                                    td1 * td2 * (tshGnd + rfshGnd * rb2 * tsh + rfshGnd * td2 * rb1 * td2 * tsh + tshGnd * rf3 * rbsh) * afd3;
                                state.dataConstruction->Construct(ConstrNum).BlAbsDiffSky(ISlatAng, 3) =
                                    td1 * td2 * (tshSky + rfshSky * rb2 * tsh + rfshSky * td2 * rb1 * td2 * tsh + tshSky * rf3 * rbsh) * afd3;
                                state.dataConstruction->Construct(ConstrNum).AbsDiffBlind(ISlatAng) =
                                    td1 * td2 * (afsh * (1 + rfsh * td2 * rb1 * td2) + rfsh * rb2 * afsh + tsh * rf3 * absh);
                                state.dataConstruction->Construct(ConstrNum).AbsDiffBlindGnd(ISlatAng) =
                                    td1 * td2 * (afshGnd + afsh * rfsh * (td2 * rb1 * td2 + rb2) + tshGnd * rf3 * absh);
                                state.dataConstruction->Construct(ConstrNum).AbsDiffBlindSky(ISlatAng) =
                                    td1 * td2 * (afshSky + afsh * rfsh * (td2 * rb1 * td2 + rb2) + tshSky * rf3 * absh);
                                state.dataConstruction->Construct(ConstrNum).BlTransDiff(ISlatAng) =
                                    td1 * td2 * (tsh + rfsh * td2 * rb1 * td2 * tsh + rfsh * rb2 * tsh + tsh * rf3 * rbsh) * td3;
                                state.dataConstruction->Construct(ConstrNum).BlTransDiffGnd(ISlatAng) =
                                    td1 * td2 * (tshGnd + rfsh * td2 * rb1 * td2 * tshGnd + rfsh * rb2 * tshGnd + tshGnd * rf3 * rbsh) * td3;
                                state.dataConstruction->Construct(ConstrNum).BlTransDiffSky(ISlatAng) =
                                    td1 * td2 * (tshSky + rfsh * td2 * rb1 * td2 * tshSky + rfsh * rb2 * tshSky + tshSky * rf3 * rbsh) * td3;
                                state.dataConstruction->Construct(ConstrNum).BlTransDiffVis(ISlatAng) =
                                    td1v * td2v * (tshv + rfshv * td2v * rb1v * td2v * tshv + rfshv * rb2v * tshv + tshv * rf3v * rbshv) * td3v;
                                state.dataConstruction->Construct(ConstrNum).BlReflectSolDiffFront(ISlatAng) =
                                    rf1 + td1 * rf2 * td1 +
                                    td1 * td2 * (rfsh + tsh * rf3 * tsh + rfsh * rb2 * rfsh + rfsh * td2 * rb1 * td2 * rfsh) * td2 * td1;
                                state.dataConstruction->Construct(ConstrNum).BlReflectVisDiffFront(ISlatAng) =
                                    rf1v + td1v * rf2v * td1v +
                                    td1v * td2v * (rfshv + tshv * rf3v * tshv + rfshv * rb2v * rfshv + rfshv * td2v * rb1v * td2v * rfshv) * td2v *
                                        td1v;

                                // Back incident solar, diffuse, between-glass blind, NGlass = 3

                                state.dataConstruction->Construct(ConstrNum).BlAbsDiffBack(ISlatAng, 1) =
                                    td3 * (tsh + rbsh * rf3 * tsh + tsh * rb2 * rfsh + tsh * td2 * rb1 * td2 * rfsh) * td2 * abd1;
                                state.dataConstruction->Construct(ConstrNum).BlAbsDiffBack(ISlatAng, 2) =
                                    td3 * ((tsh + rbsh * rf3 * tsh) * abd2 + (tsh * td2 * rb1 * td2 + tsh * rb2) * afd2);
                                state.dataConstruction->Construct(ConstrNum).BlAbsDiffBack(ISlatAng, 3) =
                                    abd3 + td3 * (rbsh + tsh * rb2 * tsh + tsh * td2 * rb1 * td2 * tsh) * afd3;
                                state.dataConstruction->Construct(ConstrNum).AbsDiffBackBlind(ISlatAng) =
                                    td3 * ((1 + rbsh * rf3) * absh + (tsh * td2 * rb1 * td2 + tsh * rb2) * afsh);
                                state.dataConstruction->Construct(ConstrNum).BlReflectSolDiffBack(ISlatAng) =
                                    rb3 + td3 * (rbsh + rbsh * rf3 * rbsh + tsh * rb2 * tsh + tsh * td2 * rb1 * td2 * tsh) * td3;
                                state.dataConstruction->Construct(ConstrNum).BlReflectVisDiffBack(ISlatAng) =
                                    rb3v + td3v * (rbshv + rbshv * rf3v * rbshv + tshv * rb2v * tshv + tshv * td2v * rb1v * td2v * tshv) * td3v;

                            } // End of check if NGlass = 3

                        } // End of check if between-glass blind

                    } // End of check if between-glass shade or blind

                    // Continue loop over slat angles only for blinds with variable slat angle
                    if (ShadeOn || ScreenOn) break;
                    if (BlindOn) {
                        if (state.dataHeatBal->Blind(BlNum).SlatAngleType == DataWindowEquivalentLayer::AngleType::Fixed) break;
                    }
                } // End of slat angle loop
            }     // End of check if construction has a shade or blind

            // Curve fits to get solar transmittance, reflectance, layer absorptance and
            // visible transmittance as polynomials in cosine of incidence angle

            if (!BlindOn && !ScreenOn) { // Bare glass or shade on
                W5LsqFit(state.dataWindowManager->CosPhiIndepVar,
                         state.dataWindowManager->tsolPhi,
                         6,
                         1,
                         TotalIPhi,
                         state.dataConstruction->Construct(ConstrNum).TransSolBeamCoef);
                W5LsqFit(state.dataWindowManager->CosPhiIndepVar,
                         state.dataWindowManager->rfsolPhi,
                         6,
                         1,
                         TotalIPhi,
                         state.dataConstruction->Construct(ConstrNum).ReflSolBeamFrontCoef);
                W5LsqFit(state.dataWindowManager->CosPhiIndepVar,
                         state.dataWindowManager->rbsolPhi,
                         6,
                         1,
                         TotalIPhi,
                         state.dataConstruction->Construct(ConstrNum).ReflSolBeamBackCoef);
                W5LsqFit(state.dataWindowManager->CosPhiIndepVar,
                         state.dataWindowManager->tvisPhi,
                         6,
                         1,
                         TotalIPhi,
                         state.dataConstruction->Construct(ConstrNum).TransVisBeamCoef);
                for (IGlass = 1; IGlass <= NGlass; ++IGlass) {
                    // Front absorptance coefficients for glass layers
                    state.dataWindowManager->DepVarCurveFit({1, TotalIPhi}) = state.dataWindowManager->solabsPhi(IGlass, {1, TotalIPhi});
                    W5LsqFit(state.dataWindowManager->CosPhiIndepVar,
                             state.dataWindowManager->DepVarCurveFit,
                             6,
                             1,
                             TotalIPhi,
                             state.dataWindowManager->CoeffsCurveFit);
                    state.dataConstruction->Construct(ConstrNum).AbsBeamCoef(IGlass) = state.dataWindowManager->CoeffsCurveFit;
                    // Back absorptance coefficients for glass layers
                    IGlassBack = NGlass - IGlass + 1;
                    state.dataWindowManager->DepVarCurveFit({1, TotalIPhi}) = state.dataWindowManager->solabsBackPhi(IGlassBack, {1, TotalIPhi});
                    W5LsqFit(state.dataWindowManager->CosPhiIndepVar,
                             state.dataWindowManager->DepVarCurveFit,
                             6,
                             1,
                             TotalIPhi,
                             state.dataWindowManager->CoeffsCurveFit);
                    state.dataConstruction->Construct(ConstrNum).AbsBeamBackCoef(IGlass) = state.dataWindowManager->CoeffsCurveFit;
                }

                // To check goodness of fit //Tuned
                auto const &solBeamCoef(state.dataConstruction->Construct(ConstrNum).TransSolBeamCoef);
                auto const &visBeamCoef(state.dataConstruction->Construct(ConstrNum).TransVisBeamCoef);
                for (IPhi = 1; IPhi <= TotalIPhi; ++IPhi) {
                    tsolPhiFit(IPhi) = 0.0;
                    tvisPhiFit(IPhi) = 0.0;

                    Phi = double(IPhi - 1) * 10.0;
                    CosPhi = std::cos(Phi * DataGlobalConstants::DegToRadians);
                    if (std::abs(CosPhi) < 0.0001) CosPhi = 0.0;
                    Real64 cos_pow(1.0);
                    for (CoefNum = 1; CoefNum <= 6; ++CoefNum) {
                        cos_pow *= CosPhi;
                        tsolPhiFit(IPhi) += solBeamCoef(CoefNum) * cos_pow;
                        tvisPhiFit(IPhi) += visBeamCoef(CoefNum) * cos_pow;
                    }
                }
            }

            if (ShadeOn)
                W5LsqFit(state.dataWindowManager->CosPhiIndepVar,
                         state.dataWindowManager->solabsShadePhi,
                         6,
                         1,
                         TotalIPhi,
                         state.dataConstruction->Construct(ConstrNum).AbsBeamShadeCoef);

        } // End of loop over constructions

        // Get effective glass and shade/blind emissivities for windows that have interior blind or
        // shade. These are used to calculate zone MRT contribution from window when
        // interior blind/shade is deployed.

        // Loop for ordinary windows
        for (SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
            if (!state.dataSurface->Surface(SurfNum).HeatTransSurf) continue;
            if (!state.dataConstruction->Construct(state.dataSurface->Surface(SurfNum).Construction).TypeIsWindow) continue;
            if (state.dataSurface->SurfWinWindowModelType(SurfNum) == WindowBSDFModel) continue; // Irrelevant for Complex Fen
            if (state.dataConstruction->Construct(state.dataSurface->Surface(SurfNum).Construction).WindowTypeEQL) continue; // not required
            ConstrNumSh = state.dataSurface->Surface(SurfNum).activeShadedConstruction;
            if (ConstrNumSh == 0) continue;
            TotLay = state.dataConstruction->Construct(ConstrNumSh).TotLayers;
            IntShade = false;
            IntBlind = false;
            if (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNumSh).LayerPoint(TotLay)).Group ==
                DataHeatBalance::MaterialGroup::Shade) {
                IntShade = true;
                ShadeLayPtr = state.dataConstruction->Construct(ConstrNumSh).LayerPoint(TotLay);
            }
            if (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNumSh).LayerPoint(TotLay)).Group ==
                DataHeatBalance::MaterialGroup::WindowBlind) {
                IntBlind = true;
                BlNum = state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNumSh).LayerPoint(TotLay)).BlindDataPtr;
            }

            if (IntShade || IntBlind) {
                for (ISlatAng = 1; ISlatAng <= MaxSlatAngs; ++ISlatAng) {
                    if (IntShade || IntBlind) {
                        EpsGlIR =
                            state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNumSh).LayerPoint(TotLay - 1)).AbsorpThermalBack;
                        RhoGlIR = 1 - EpsGlIR;
                    }
                    if (IntShade) {
                        TauShIR = state.dataMaterial->Material(ShadeLayPtr).TransThermal;
                        EpsShIR = state.dataMaterial->Material(ShadeLayPtr).AbsorpThermal;
                        RhoShIR = max(0.0, 1.0 - TauShIR - EpsShIR);
                        state.dataSurface->SurfaceWindow(SurfNum).EffShBlindEmiss(1) =
                            EpsShIR * (1.0 + RhoGlIR * TauShIR / (1.0 - RhoGlIR * RhoShIR));
                        state.dataSurface->SurfaceWindow(SurfNum).EffGlassEmiss(1) = EpsGlIR * TauShIR / (1.0 - RhoGlIR * RhoShIR);
                    }
                    if (IntBlind) {
                        TauShIR = state.dataHeatBal->Blind(BlNum).IRFrontTrans(ISlatAng);
                        EpsShIR = state.dataHeatBal->Blind(BlNum).IRBackEmiss(ISlatAng);
                        RhoShIR = max(0.0, 1.0 - TauShIR - EpsShIR);
                        state.dataSurface->SurfaceWindow(SurfNum).EffShBlindEmiss(ISlatAng) =
                            EpsShIR * (1.0 + RhoGlIR * TauShIR / (1.0 - RhoGlIR * RhoShIR));
                        state.dataSurface->SurfaceWindow(SurfNum).EffGlassEmiss(ISlatAng) = EpsGlIR * TauShIR / (1.0 - RhoGlIR * RhoShIR);
                    }
                    // Loop over remaining slat angles only if blind with movable slats
                    if (IntShade) break; // Loop over remaining slat angles only if blind
                    if (IntBlind) {
                        if (state.dataHeatBal->Blind(BlNum).SlatAngleType == DataWindowEquivalentLayer::AngleType::Fixed) break;
                    }
                } // End of slat angle loop
            }     // End of check if interior shade or interior blind
        }         // End of surface loop

        for (SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
            if (state.dataSurface->Surface(SurfNum).Construction <= 0) continue;
            if (!state.dataConstruction->Construct(state.dataSurface->Surface(SurfNum).Construction).TypeIsWindow) continue;
            ConstrNum = state.dataSurface->Surface(SurfNum).Construction;
            // Total thickness of glazing system (used in calculation of inside reveal reflection/absorption
            state.dataSurface->SurfWinTotGlazingThickness(SurfNum) = 0.0;
            for (LayNum = 1; LayNum <= state.dataConstruction->Construct(ConstrNum).TotLayers; ++LayNum) {
                state.dataSurface->SurfWinTotGlazingThickness(SurfNum) +=
                    state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(LayNum)).Thickness;
            }
            // Sine and cosine of azimuth and tilt
            //    SurfaceWindow(SurfNum)%SinAzim = Surface(SurfNum)%SinAzim
            //    SurfaceWindow(SurfNum)%CosAzim = Surface(SurfNum)%CosAzim
            //    SurfaceWindow(SurfNum)%SinTilt = Surface(SurfNum)%SinTilt
            //    SurfaceWindow(SurfNum)%CosTilt = Surface(SurfNum)%CosTilt
            //    ! Outward normal unit vector (pointing away from room)
            //    SurfaceWindow(SurfNum)%OutNormVec(1) = Surface(SurfNum)%OutNormVec(1)
            //    SurfaceWindow(SurfNum)%OutNormVec(2) = Surface(SurfNum)%OutNormVec(2)
            //    SurfaceWindow(SurfNum)%OutNormVec(3) = Surface(SurfNum)%OutNormVec(3)
            //    write(outputfiledebug,*) 'window='//TRIM(surface(SurfNum)%name)
            //    write(outputfiledebug,*) '  swindow%outnormvec=',surfacewindow(SurfNum)%outnormvec
            //    write(outputfiledebug,*) '  surface%outnormvec=',surface(SurfNum)%outnormvec
            // Window center
            Rectangle = false;
            Triangle = false;
            if (state.dataSurface->Surface(SurfNum).Sides == 3) Triangle = true;
            if (state.dataSurface->Surface(SurfNum).Sides == 4) Rectangle = true;
            if (Rectangle) {
                // Vertices of window (numbered counter-clockwise starting at upper left as viewed from inside of room).
                // Assumes original vertices are numbered counter-clockwise from upper left as viewed from outside.
                W3 = state.dataSurface->Surface(SurfNum).Vertex(2);
                W2 = state.dataSurface->Surface(SurfNum).Vertex(3);
                W1 = state.dataSurface->Surface(SurfNum).Vertex(4);
            } else if (Triangle) {
                W3 = state.dataSurface->Surface(SurfNum).Vertex(2);
                W2 = state.dataSurface->Surface(SurfNum).Vertex(3);
                W1 = state.dataSurface->Surface(SurfNum).Vertex(1);
            }
            W21 = W1 - W2;
            W23 = W3 - W2;
            if (Rectangle) {
                state.dataSurface->SurfaceWindow(SurfNum).WinCenter = W2 + (W23 + W21) / 2.0;
            } else if (Triangle) {
                state.dataSurface->SurfaceWindow(SurfNum).WinCenter = W2 + (W23 + W21) / 3.0;
            }
        } // End of surface loop

        ReportGlass(state);
    }

    //*****************************************************************************************

    void W5InitGlassParameters(EnergyPlusData &state)
    {
        // Initializes variables used in the window optical and thermal calculation.

        int ConstrNum;        // Construction number
        int SurfNum;          // Surface number
        int IPhi;             // Angle of incidence counter
        int FrDivNum;         // Pointer to frame/divider
        Real64 FrWidth;       // Window frame width {m}
        Real64 FrEdgeWidth;   // Frame edge width {m}
        Real64 DivWidth;      // Window divider width {m}
        Real64 DivEdgeWidth;  // Divider edge width {m}
        Real64 GlHeight;      // Height of glazed part of window {m}
        Real64 GlWidth;       // Width of glazed part of window {m}
        int NumHorDividers;   // Number of horizontal divider elements
        int NumVertDividers;  // Number of vertical divider elements
        int BaseSurfNum;      // Base surface number
        int MatNum;           // Material number
        int DifOverrideCount; // Count the number of SolarDiffusing material overrides

        for (ConstrNum = 1; ConstrNum <= state.dataHeatBal->TotConstructs; ++ConstrNum) {
            if (state.dataConstruction->Construct(ConstrNum).FromWindow5DataFile) continue;
            if (state.dataConstruction->Construct(ConstrNum).WindowTypeBSDF) continue;
            state.dataConstruction->Construct(ConstrNum).TransDiff = 0.0;
            state.dataConstruction->Construct(ConstrNum).TransDiffVis = 0.0;
            state.dataConstruction->Construct(ConstrNum).AbsDiffBackShade = 0.0;
            state.dataConstruction->Construct(ConstrNum).ShadeAbsorpThermal = 0.0;
            state.dataConstruction->Construct(ConstrNum).ReflectSolDiffBack = 0.0;
            state.dataConstruction->Construct(ConstrNum).ReflectSolDiffFront = 0.0;
            state.dataConstruction->Construct(ConstrNum).ReflectVisDiffFront = 0.0;
            state.dataConstruction->Construct(ConstrNum).AbsBeamShadeCoef = 0.0;
            state.dataConstruction->Construct(ConstrNum).TransSolBeamCoef = 0.0;
            state.dataConstruction->Construct(ConstrNum).ReflSolBeamFrontCoef = 0.0;
            state.dataConstruction->Construct(ConstrNum).ReflSolBeamBackCoef = 0.0;
            state.dataConstruction->Construct(ConstrNum).TransVisBeamCoef = 0.0;
            state.dataConstruction->Construct(ConstrNum).AbsDiff = 0.0;
            state.dataConstruction->Construct(ConstrNum).AbsDiffBack = 0.0;
            for (int Layer = 1; Layer <= state.dataHeatBal->MaxSolidWinLayers; ++Layer) {
                for (int index = 1; index <= DataSurfaces::MaxPolyCoeff; ++index) {
                    state.dataConstruction->Construct(state.dataHeatBal->TotConstructs).AbsBeamCoef(Layer)(index) = 0.0;
                    state.dataConstruction->Construct(state.dataHeatBal->TotConstructs).AbsBeamBackCoef(Layer)(index) = 0.0;
                }
            }
        }

        for (SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
            // For a window with shading device, get number of shaded construction and, if window
            // has a blind (interior, exterior or between glass), get blind data pointer.

            if (state.dataSurface->Surface(SurfNum).HasShadeControl) {
                for (int winShadCtrl : state.dataSurface->Surface(SurfNum).windowShadingControlList) {
                    WinShadingType ShadingType = state.dataSurface->WindowShadingControl(winShadCtrl).ShadingType;
                    if (ShadingType == WinShadingType::ExtScreen) {
                        //     Count number of exterior window screens, initialize in InitGlassOpticalCalculations after returning
                        //     from this subroutine. The blind structure is initialized first and then the screen structure is initialized.
                        ++state.dataHeatBal->NumSurfaceScreens;
                        break; // only need to find the first window shading control since they should be identical
                    }
                }
            }
        }

        // Set some static exterior-window frame and divider SurfaceWindow values
        // from values in FrameDivider derived type
        for (SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
            FrDivNum = state.dataSurface->Surface(SurfNum).FrameDivider;
            if (FrDivNum > 0) { // Surface is a window with a frame and/or divider
                FrWidth = state.dataSurface->FrameDivider(FrDivNum).FrameWidth;
                GlHeight = state.dataSurface->Surface(SurfNum).Height;
                GlWidth = state.dataSurface->Surface(SurfNum).Width;
                NumVertDividers = state.dataSurface->FrameDivider(FrDivNum).VertDividers;
                NumHorDividers = state.dataSurface->FrameDivider(FrDivNum).HorDividers;
                BaseSurfNum = state.dataSurface->Surface(SurfNum).BaseSurf;
                state.dataSurface->SurfWinFrameConductance(SurfNum) = state.dataSurface->FrameDivider(FrDivNum).FrameConductance;
                state.dataSurface->SurfWinFrameSolAbsorp(SurfNum) = state.dataSurface->FrameDivider(FrDivNum).FrameSolAbsorp;
                state.dataSurface->SurfWinFrameVisAbsorp(SurfNum) = state.dataSurface->FrameDivider(FrDivNum).FrameVisAbsorp;
                state.dataSurface->SurfWinFrameEmis(SurfNum) = state.dataSurface->FrameDivider(FrDivNum).FrameEmis;
                state.dataSurface->SurfWinFrEdgeToCenterGlCondRatio(SurfNum) = state.dataSurface->FrameDivider(FrDivNum).FrEdgeToCenterGlCondRatio;
                state.dataSurface->SurfWinDividerType(SurfNum) = DataSurfaces::FrameDividerType::DividedLite;
                if (state.dataSurface->FrameDivider(FrDivNum).DividerType == DataSurfaces::FrameDividerType::Suspended)
                    state.dataSurface->SurfWinDividerType(SurfNum) = DataSurfaces::FrameDividerType::Suspended;
                DivWidth = state.dataSurface->FrameDivider(FrDivNum).DividerWidth;
                state.dataSurface->SurfWinDividerConductance(SurfNum) = state.dataSurface->FrameDivider(FrDivNum).DividerConductance;
                state.dataSurface->SurfWinDividerSolAbsorp(SurfNum) = state.dataSurface->FrameDivider(FrDivNum).DividerSolAbsorp;
                state.dataSurface->SurfWinDividerVisAbsorp(SurfNum) = state.dataSurface->FrameDivider(FrDivNum).DividerVisAbsorp;
                state.dataSurface->SurfWinDividerEmis(SurfNum) = state.dataSurface->FrameDivider(FrDivNum).DividerEmis;
                state.dataSurface->SurfWinDivEdgeToCenterGlCondRatio(SurfNum) = state.dataSurface->FrameDivider(FrDivNum).DivEdgeToCenterGlCondRatio;

                state.dataSurface->SurfWinOutsideRevealSolAbs(SurfNum) = state.dataSurface->FrameDivider(FrDivNum).OutsideRevealSolAbs;
                state.dataSurface->SurfWinInsideSillDepth(SurfNum) = state.dataSurface->FrameDivider(FrDivNum).InsideSillDepth;
                state.dataSurface->SurfWinInsideReveal(SurfNum) = state.dataSurface->FrameDivider(FrDivNum).InsideReveal;
                state.dataSurface->SurfWinInsideSillSolAbs(SurfNum) = state.dataSurface->FrameDivider(FrDivNum).InsideSillSolAbs;
                state.dataSurface->SurfWinInsideRevealSolAbs(SurfNum) = state.dataSurface->FrameDivider(FrDivNum).InsideRevealSolAbs;

                FrEdgeWidth = state.dataSurface->FrameDivider(FrDivNum).FrameEdgeWidth;
                DivEdgeWidth = state.dataSurface->FrameDivider(FrDivNum).DividerEdgeWidth;
                state.dataSurface->SurfWinFrameEdgeArea(SurfNum) =
                    2 * FrEdgeWidth * (GlHeight - FrEdgeWidth - NumHorDividers * DivWidth + GlWidth - FrEdgeWidth - NumVertDividers * DivWidth);
                state.dataSurface->SurfWinDividerEdgeArea(SurfNum) =
                    2 * DivEdgeWidth * (NumHorDividers * (GlWidth - 2 * FrEdgeWidth) + NumVertDividers * (GlHeight - 2 * FrEdgeWidth)) -
                    NumHorDividers * NumVertDividers * (4 * pow_2(DivEdgeWidth) + 4 * FrEdgeWidth * DivWidth);
                state.dataSurface->SurfWinCenterGlArea(SurfNum) = state.dataSurface->Surface(SurfNum).Area -
                                                                  state.dataSurface->SurfWinFrameEdgeArea(SurfNum) -
                                                                  state.dataSurface->SurfWinDividerEdgeArea(SurfNum);
                state.dataSurface->SurfWinEdgeGlCorrFac(SurfNum) =
                    (state.dataSurface->SurfWinFrameEdgeArea(SurfNum) * state.dataSurface->SurfWinFrEdgeToCenterGlCondRatio(SurfNum) +
                     state.dataSurface->SurfWinDividerEdgeArea(SurfNum) * state.dataSurface->SurfWinDivEdgeToCenterGlCondRatio(SurfNum) +
                     state.dataSurface->SurfWinCenterGlArea(SurfNum)) /
                    (state.dataSurface->SurfWinFrameEdgeArea(SurfNum) + state.dataSurface->SurfWinDividerEdgeArea(SurfNum) +
                     state.dataSurface->SurfWinCenterGlArea(SurfNum));
            }
        }

        // Set SolarDiffusing to true for exterior windows that have a construction with an innermost diffusing glass layer
        DifOverrideCount = 0;
        for (SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
            state.dataSurface->SurfWinSolarDiffusing(SurfNum) = false;
            if (state.dataSurface->Surface(SurfNum).Class == SurfaceClass::Window &&
                state.dataSurface->Surface(SurfNum).ExtBoundCond == ExternalEnvironment && state.dataSurface->SurfWinStormWinConstr(SurfNum) == 0) {
                ConstrNum = state.dataSurface->Surface(SurfNum).Construction;
                MatNum = state.dataConstruction->Construct(ConstrNum).LayerPoint(state.dataConstruction->Construct(ConstrNum).TotLayers);
                if (state.dataMaterial->Material(MatNum).SolarDiffusing) {
                    if (!state.dataSurface->Surface(SurfNum).HasShadeControl) {
                        state.dataSurface->SurfWinSolarDiffusing(SurfNum) = true;
                    } else { // There is a shading control
                        if (state.dataSurface->WindowShadingControl(state.dataSurface->Surface(SurfNum).activeWindowShadingControl).ShadingType ==
                            WinShadingType::SwitchableGlazing) {
                            state.dataSurface->SurfWinSolarDiffusing(SurfNum) = true;
                        } else {
                            state.dataSurface->SurfWinSolarDiffusing(SurfNum) = false;
                            ++DifOverrideCount;
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                ShowWarningError(
                                    state,
                                    "W5InitGlassParameters: Window=\"" + state.dataSurface->Surface(SurfNum).Name +
                                        "\" has interior material with Solar Diffusing=Yes, but existing Window Shading Device sets Diffusing=No.");
                            }
                        }
                    }
                }
            }
        }

        if (DifOverrideCount > 0) {
            if (!state.dataGlobal->DisplayExtraWarnings) {
                ShowWarningError(state,
                                 format("W5InitGlassParameters: {} Windows had Solar Diffusing=Yes overridden by presence of Window Shading Device.",
                                        DifOverrideCount));
            } else {
                ShowMessage(state,
                            format("W5InitGlassParameters: {} Windows had Solar Diffusing=Yes overridden by presence of Window Shading Device.",
                                   DifOverrideCount));
            }
        }

        for (IPhi = 1; IPhi <= 10; ++IPhi) {
            state.dataWindowManager->CosPhiIndepVar(IPhi) = std::cos((IPhi - 1) * 10.0 * DataGlobalConstants::DegToRadians);
        }
    }

    //****************************************************************************
    // WINDOW 5 Optical Calculation Subroutines
    //****************************************************************************

    void SystemSpectralPropertiesAtPhi(EnergyPlusData &state,
                                       int const iquasi,   // When there is no spectral data, this is the wavelength
                                       int const ngllayer, // Number of glass layers in construction
                                       Real64 const wlbot, // Lowest and highest wavelength considered
                                       Real64 const wltop)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Adapted by F.Winkelmann from WINDOW 5
        //                      subroutine opcalc
        //       DATE WRITTEN   August 1999
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // For a particular angle of incidence, calculates system properties
        // for a multi-layer glazing for each wavelength in the solar spectrum.
        // Handles the special case of one or more layers that do not have spectral data.

        // Returns, for a particular angle of incidence:
        //   stPhi     transmissivity of system at each wavelength in swl
        //   srfPhi    front reflectance of system at each wavelength in swl
        //   srbPhi    back reflectance of system at each wavelength in swl
        //   sabsPhi   absorptance by layer at each wavelength in swl

        Array1D<Real64> sabsPhi(5); // System solar absorptance in each glass layer for
        //   particular angle of incidence
        int in; // Glass layer counter
        int i;
        int iwl; // Wavelength counter
        int j;
        Real64 wl; // Wavelength
        //   index to use in tPhi, rfPhi and rbPhi

        // For each glass layer find tPhi, rfPhi, and rbPhi at each wavelength

        for (in = 1; in <= ngllayer; ++in) {
            for (iwl = 1; iwl <= state.dataWindowManager->nume; ++iwl) {
                wl = state.dataWindowManager->wle[iwl - 1];
                if (wl < wlbot || wl > wltop) continue;
                // In the following numpt is the number of spectral data points for each layer;
                // numpt = 2 if there is no spectral data for a layer.
                if (state.dataWindowManager->numpt[in - 1] <= 2) {
                    state.dataWindowManager->tadjPhi[in - 1][iwl - 1] = state.dataWindowManager->tPhi[in - 1][iquasi - 1];
                    state.dataWindowManager->rfadjPhi[in - 1][iwl - 1] = state.dataWindowManager->rfPhi[in - 1][iquasi - 1];
                    state.dataWindowManager->rbadjPhi[in - 1][iwl - 1] = state.dataWindowManager->rbPhi[in - 1][iquasi - 1];
                } else {
                    // Interpolate to get properties at the solar spectrum wavelengths
                    state.dataWindowManager->tadjPhi[in - 1][iwl - 1] = Interpolate(
                        state.dataWindowManager->wlt[in - 1], state.dataWindowManager->tPhi[in - 1], state.dataWindowManager->numpt[in - 1], wl);
                    state.dataWindowManager->rfadjPhi[in - 1][iwl - 1] = Interpolate(
                        state.dataWindowManager->wlt[in - 1], state.dataWindowManager->rfPhi[in - 1], state.dataWindowManager->numpt[in - 1], wl);
                    state.dataWindowManager->rbadjPhi[in - 1][iwl - 1] = Interpolate(
                        state.dataWindowManager->wlt[in - 1], state.dataWindowManager->rbPhi[in - 1], state.dataWindowManager->numpt[in - 1], wl);
                }
            }
        }

        // Calculate system properties at each wavelength
        for (j = 1; j <= state.dataWindowManager->nume; ++j) {
            wl = state.dataWindowManager->wle[j - 1];
            if (wl < wlbot || wl > wltop) continue;

            // Set diagonal of matrix for subroutine SystemPropertiesAtLambdaAndPhi
            for (i = 1; i <= ngllayer; ++i) {
                state.dataWindowManager->top[i - 1][i - 1] = state.dataWindowManager->tadjPhi[i - 1][j - 1];
                state.dataWindowManager->rfop[i - 1][i - 1] = state.dataWindowManager->rfadjPhi[i - 1][j - 1];
                state.dataWindowManager->rbop[i - 1][i - 1] = state.dataWindowManager->rbadjPhi[i - 1][j - 1];
            }

            // Calculate glazing system properties
            if (ngllayer == 1) { // Single-layer system
                state.dataWindowManager->stPhi[j - 1] = state.dataWindowManager->top[0][0];
                state.dataWindowManager->srfPhi[j - 1] = state.dataWindowManager->rfop[0][0];
                state.dataWindowManager->srbPhi[j - 1] = state.dataWindowManager->rbop[0][0];
                sabsPhi(1) = 1.0 - state.dataWindowManager->stPhi[j - 1] - state.dataWindowManager->srfPhi[j - 1];
            } else { // Multilayer system
                // Get glazing system properties stPhi, etc., at this wavelength and incidence angle
                SystemPropertiesAtLambdaAndPhi(state,
                                               ngllayer,
                                               state.dataWindowManager->stPhi[j - 1],
                                               state.dataWindowManager->srfPhi[j - 1],
                                               state.dataWindowManager->srbPhi[j - 1],
                                               sabsPhi);
            }

            for (i = 1; i <= ngllayer; ++i) {
                state.dataWindowManager->saPhi(i, j) = sabsPhi(i);
            }

        } // End of wavelength loop
    }

    //************************************************************************

    void SystemPropertiesAtLambdaAndPhi(EnergyPlusData &state,
                                        int const n, // Number of glass layers
                                        Real64 &tt,  // System transmittance
                                        Real64 &rft, // System front and back reflectance
                                        Real64 &rbt,
                                        Array1A<Real64> aft // System absorptance of each glass layer
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Adapted by F. Winkelmann from WINDOW 5
        //                      subroutine op
        //       DATE WRITTEN   August 1999
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // For a given angle of incidence, finds the overall properties of
        // of a series of layers at a particular wavelength

        // Argument array dimensioning
        aft.dim(5);

        int i; // Glass layer counters
        int j;
        Real64 denom; // Intermediate variables
        Real64 denom1;
        Real64 denom2;
        Real64 t0; // Transmittance, back reflectance and front
        Real64 rb0;
        Real64 rf0;
        //   reflectance variables
        Real64 af; // Front and back absorptance variables
        Real64 ab;

        // Calculate perimeter elements of rt matrix
        for (i = 1; i <= n - 1; ++i) {
            for (j = i + 1; j <= n; ++j) {
                denom = 1.0 - state.dataWindowManager->rfop[j - 1][j - 1] * state.dataWindowManager->rbop[i - 1][j - 2];
                if (denom == 0.0) {
                    state.dataWindowManager->top[j - 1][i - 1] = 0.0;
                    state.dataWindowManager->rfop[j - 1][i - 1] = 1.0;
                    state.dataWindowManager->rbop[i - 1][j - 1] = 1.0;
                } else {
                    state.dataWindowManager->top[j - 1][i - 1] =
                        state.dataWindowManager->top[j - 2][i - 1] * state.dataWindowManager->top[j - 1][j - 1] / denom;
                    state.dataWindowManager->rfop[j - 1][i - 1] =
                        state.dataWindowManager->rfop[j - 2][i - 1] +
                        pow_2(state.dataWindowManager->top[j - 2][i - 1]) * state.dataWindowManager->rfop[j - 1][j - 1] / denom;
                    state.dataWindowManager->rbop[i - 1][j - 1] =
                        state.dataWindowManager->rbop[j - 1][j - 1] +
                        pow_2(state.dataWindowManager->top[j - 1][j - 1]) * state.dataWindowManager->rbop[i - 1][j - 2] / denom;
                }
            }
        }
        // System properties: transmittance, front and back reflectance
        tt = state.dataWindowManager->top[n - 1][0];
        rft = state.dataWindowManager->rfop[n - 1][0];
        rbt = state.dataWindowManager->rbop[0][n - 1];

        // Absorptance in each layer
        for (j = 1; j <= n; ++j) {
            if (j == 1) {
                t0 = 1.0;
                rb0 = 0.0;
            } else {
                t0 = state.dataWindowManager->top[j - 2][0];
                rb0 = state.dataWindowManager->rbop[0][j - 2];
            }

            if (j == n) {
                rf0 = 0.0;
            } else {
                rf0 = state.dataWindowManager->rfop[n - 1][j];
            }

            af = 1.0 - state.dataWindowManager->top[j - 1][j - 1] - state.dataWindowManager->rfop[j - 1][j - 1];
            ab = 1.0 - state.dataWindowManager->top[j - 1][j - 1] - state.dataWindowManager->rbop[j - 1][j - 1];
            denom1 = 1.0 - state.dataWindowManager->rfop[n - 1][j - 1] * rb0;
            denom2 = 1.0 - state.dataWindowManager->rbop[0][j - 1] * rf0;

            if (denom1 == 0.0 || denom2 == 0.0) {
                aft(j) = 0.0;
            } else {
                aft(j) = (t0 * af) / denom1 + (state.dataWindowManager->top[j - 1][0] * rf0 * ab) / denom2;
            }
        }
    }

    //*************************************************************************

    void SolarSprectrumAverage(EnergyPlusData &state,
                               Array1A<Real64> p, // Quantity to be weighted by solar spectrum
                               Real64 &psol       // Quantity p weighted by solar spectrum
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Adapted by F.Winkelmann from WINDOW 5 subroutine solar
        //       DATE WRITTEN   August 1999
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates average of property p weighted by solar spectral irradiance, e

        // Argument array dimensioning
        p.dim(state.dataWindowManager->nume);

        // Locals
        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        Real64 up; // Intermediate variables
        Real64 down;
        int i;       // Wavelength counter
        Real64 esol; // Solar spectrum value times delta wavelength

        up = 0.0;
        down = 0.0;

        for (i = 1; i <= state.dataWindowManager->nume - 1; ++i) {
            esol = (state.dataWindowManager->wle[i] - state.dataWindowManager->wle[i - 1]) * 0.5 *
                   (state.dataWindowManager->e[i - 1] + state.dataWindowManager->e[i]);
            up += 0.5 * (p(i) + p(i + 1)) * esol;
            down += esol;
        }

        psol = up / down;
    }

    Real64 solarSpectrumAverage(EnergyPlusData &state, gsl::span<Real64> p)
    {
        Real64 num = 0.0;
        Real64 denom = 0.0;
        for (int i = 1; i <= state.dataWindowManager->nume - 1; ++i) {
            Real64 const esol = (state.dataWindowManager->wle[i] - state.dataWindowManager->wle[i - 1]) * 0.5 *
                                (state.dataWindowManager->e[i - 1] + state.dataWindowManager->e[i]);
            num += 0.5 * (p[i - 1] + p[i]) * esol;
            denom += esol;
        }
        return num / denom; // dangerous, doesn't check for zero denominator
    }

    Real64 visibleSpectrumAverage(EnergyPlusData &state, gsl::span<Real64> p)
    {
        //       AUTHOR         Adapted by F.Winkelmann from WINDOW 5
        //                      subroutine w4vis
        //       DATE WRITTEN   August 1999

        // Calculates visible average of property p by weighting with solar
        // spectral irradiance, e, and photopic response, y30

        Real64 num = 0.0;
        Real64 denom = 0.0;
        Real64 y30new = 0.0;
        Real64 y30ils1 = 0.0;
        for (int i = 2; i <= state.dataWindowManager->nume;
             ++i) { // Autodesk:BoundsViolation e|wle|p(i-1) @ i=1: Changed start index from 1 to 2: wle
            // values prevented this violation from occurring in practice
            // Restrict to visible range
            if (state.dataWindowManager->wle[i - 1] >= 0.37 && state.dataWindowManager->wle[i - 1] <= 0.78) {
                y30new = Interpolate(
                    state.dataWindowManager->wlt3, state.dataWindowManager->y30, state.dataWindowManager->numt3, state.dataWindowManager->wle[i - 1]);
                Real64 evis = state.dataWindowManager->e[i - 2] * 0.5 * (y30new + y30ils1) *
                              (state.dataWindowManager->wle[i - 1] - state.dataWindowManager->wle[i - 2]);
                num += 0.5 * (p[i - 1] + p[i - 2]) * evis;
                denom += evis;
                y30ils1 = y30new;
            }
        }
        return num / denom; // dangerous, doesn't check for zero denominator
    }

    Real64 Interpolate(gsl::span<Real64> x, // Array of data points for independent variable
                       gsl::span<Real64> y, // Array of data points for dependent variable
                       int const npts,      // Number of data pairs
                       Real64 const xin     // Given value of x
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Adapted by F.Winkelmann from WINDOW 5 subroutine interp
        //       DATE WRITTEN   August 1999
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Linearly interpolates between data points. Outputs yout, interpolated
        // value of y corresponding to xin

        for (int i = 1; i <= npts; ++i) {
            if (xin <= x[i - 1]) {
                if (i - 1 == 0) {
                    return y[0];
                } else {
                    return y[i - 2] + (y[i - 1] - y[i - 2]) * (xin - x[i - 2]) / (x[i - 1] - x[i - 2]);
                }
            }
        }

        // Past the end of the array, so return endpoint
        return y[npts - 1];
    }

    //***********************************************************************************
    // Window Thermal Calculation Subroutines
    //***********************************************************************************

    void CalcWindowHeatBalance(EnergyPlusData &state,
                               int const SurfNum,          // Surface number
                               Real64 const HextConvCoeff, // Outside air film conductance coefficient
                               Real64 &SurfInsideTemp,     // Inside window surface temperature
                               Real64 &SurfOutsideTemp     // Outside surface temperature (C)
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         S. Vidanovic
        //       DATE WRITTEN   June 2016
        //       MODIFIED       na
        //       RE-ENGINEERED  na
        //
        // PURPOSE OF THIS SUBROUTINE:
        // Subroutine to direct whether to use exterior or interior window routines
        if (state.dataGlobal->KickOffSizing || state.dataGlobal->KickOffSimulation) return;

        if (state.dataWindowManager->inExtWindowModel->isExternalLibraryModel()) {
            CalcWindowHeatBalanceExternalRoutines(state, SurfNum, HextConvCoeff, SurfInsideTemp, SurfOutsideTemp);
        } else {
            CalcWindowHeatBalanceInternalRoutines(state, SurfNum, HextConvCoeff, SurfInsideTemp, SurfOutsideTemp);
        }
    }

    void CalcWindowHeatBalanceInternalRoutines(EnergyPlusData &state,
                                               int const SurfNum,          // Surface number
                                               Real64 const HextConvCoeff, // Outside air film conductance coefficient
                                               Real64 &SurfInsideTemp,     // Inside window surface temperature
                                               Real64 &SurfOutsideTemp     // Outside surface temperature (C)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         F. Winkelmann
        //       DATE WRITTEN   November 1999
        //       MODIFIED       FW, July 2000 (call better solution method)
        //                      FW, June 2001 (handle window blinds)
        //                      FW, Dec  2002 (add between-glass shades and blinds)
        //                      FW, Mar  2003 (extend condensation flag to airflow windows)
        //                      CC, Jul  2003 (set the reference temperatures for inside surface heat balance
        //                                    depending on convection algorithms and/or air models used)
        //                      FW, Sep  2003 (increment ZoneWinHeatGain only for exterior windows)
        //                      RR, May  2006 (add exterior window screen)
        //                      TH, Dec  2008 (add thermochromic windows)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Sets up information needed to calculate the window thermal behavior.
        // Calls SolveForWindowTemperatures, which calculates the inside and outside
        // face temperature of each glass layer by solving the heat balance
        // equations on each face. Also calls CalcWinFrameAndDividerTemps,
        // which calculates the outside and inside face temperatures of the
        // window frame and divider if either of these are present.
        // The resulting inside face temperature of the inner glass pane and the
        // inside surface temperatures of frame and divider are used in the zone
        // heat balance calculation. The inside face temperature of an interior shade
        // or blind, if present, and the natural convection air flow between the
        // shade/blind and inside glass face also appear in the zone heat balance calculation.
        // The logical variable NRSolution is currently set to false, which means
        // that the Newton-Raphson solution method for the glass layer heat balance
        // is not used (because it sometimes didn't converge for 3- and 4-pane
        // constructions with one or more low-emissivity layers). Instead, a more
        // robust solution method is used that successively solves linearized heat
        // balance equations until convergence is reached (see SolveForWindowTemperatures).
        // CalcWindowHeatBalance is called by CalcHeatBalanceInsideSurface once each
        // time step for each window.

        // Using/Aliasing
        using namespace DataBSDFWindow;
        using Psychrometrics::PsyCpAirFnW;
        using Psychrometrics::PsyTdpFnWPb;
        // unused0909  USE DataEnvironment, ONLY: CurMnDyHr
        using ScheduleManager::GetCurrentScheduleValue;
        using WindowComplexManager::CalcComplexWindowThermal;
        using WindowEquivalentLayer::EQLWindowSurfaceHeatBalance;

        // SUBROUTINE ARGUMENT DEFINITIONS:
        // (temperature of innermost face) [C]

        int ZoneNum;    // Zone number corresponding to SurfNum
        int BlNum;      // Window blind number
        int SurfNumAdj; // An interzone surface's number in the adjacent zone
        int ZoneNumAdj; // An interzone surface's adjacent zone number
        int TotLay;     // Total number of layers in a construction
        //   (sum of solid layers and gap layers)
        int TotGlassLay;          // Total number of glass layers in a construction
        int Lay;                  // Layer number
        int LayPtr;               // Material number for a layer
        int IGlass;               // glass layer number (1,2,3,...)
        int IGap;                 // Gap layer number (1,2,...)
        int IMix;                 // Gas number in a mixture of gases
        int ICoeff;               // Gas property index (1,2,3)
        WinShadingType ShadeFlag; // Flag indicating whether shade or blind is on, and shade/blind position
        int k;                    // Layer counter
        // REAL(r64) :: tsky                         ! Sky temperature [K]
        int ShadeLayPtr; // Material number corresponding to a shade layer
        Real64 dth1;     // Temperature difference across glass layers [K]
        Real64 dth2;
        Real64 dth3;
        Real64 dth4;
        Real64 EffShBlEmiss;    // Effective interior shade or blind emissivity
        Real64 EffGlEmiss;      // Effective inside glass emissivity when interior shade or blind
        Real64 RoomHumRat;      // Room air humidity ratio
        Real64 RoomDewPoint;    // Room air dewpoint temperature (C)
        Real64 InsideGlassTemp; // Temperature of room side of innermost glass layer (C)
        Real64 Tleft;           // For airflow windows, temperature of the glass faces adjacent
        Real64 Tright;

        Real64 SrdSurfTempAbs; // Absolute temperature of a surrounding surface
        Real64 SrdSurfViewFac; // View factor of a surrounding surface
        Real64 OutSrdIR;       // LWR from surrouding srfs

        // New variables for thermochromic windows calc
        Real64 locTCSpecTemp;  // The temperature corresponding to the specified optical properties of the TC layer
        Real64 locTCLayerTemp; // TC layer temperature at each time step. C
        int i;
        auto &deltaTemp = state.dataWindowManager->deltaTemp;
        auto &iMinDT = state.dataWindowManager->iMinDT;
        auto &IDConst = state.dataWindowManager->IDConst;
        Real64 dT0(0.0);
        Real64 dT1(0.0);
        Real64 SurfOutsideEmiss; // temporary for result of outside surface emissivity
        Real64 Tsout;            // temporary for result of outside surface temp in Kelvin
        int temp;

        // Shorthand references
        auto &window(state.dataSurface->SurfaceWindow(SurfNum));
        auto &surface(state.dataSurface->Surface(SurfNum));
        int ConstrNum = state.dataSurface->SurfActiveConstruction(SurfNum);

        if (state.dataSurface->SurfWinWindowModelType(SurfNum) == WindowBSDFModel) {

            temp = 0;

            // Simon: Complex fenestration state works only with tarcog
            CalcComplexWindowThermal(
                state, SurfNum, temp, HextConvCoeff, SurfInsideTemp, SurfOutsideTemp, SurfOutsideEmiss, DataBSDFWindow::Condition::Invalid);

            TotGlassLay = state.dataConstruction->Construct(ConstrNum).TotGlassLayers;
            state.dataWindowManager->ngllayer =
                state.dataConstruction->Construct(ConstrNum).TotSolidLayers; // Simon: This is necessary to keep for frame calculations
            // Simon: need to transfer surface temperatures because of frames calculation
            for (i = 1; i <= 2 * state.dataConstruction->Construct(ConstrNum).TotSolidLayers; ++i) {
                state.dataWindowManager->thetas[i - 1] = window.ThetaFace(i);
            }
            state.dataWindowManager->hcout = HextConvCoeff;
            state.dataWindowManager->hcin = state.dataHeatBalSurf->SurfHConvInt(SurfNum);
            state.dataWindowManager->tin = state.dataHeatBalFanSys->MAT(surface.Zone) + state.dataWindowManager->TKelvin; // Inside air temperature

            // This is code repeating and it is necessary to calculate report variables.  Do not know
            // how to solve this in more elegant way :(
            if (surface.ExtWind) {             // Window is exposed to wind (and possibly rain)
                if (state.dataEnvrn->IsRain) { // Raining: since wind exposed, outside window surface gets wet
                    state.dataWindowManager->tout = state.dataSurface->SurfOutWetBulbTemp(SurfNum) + state.dataWindowManager->TKelvin;
                } else { // Dry
                    state.dataWindowManager->tout = state.dataSurface->SurfOutDryBulbTemp(SurfNum) + state.dataWindowManager->TKelvin;
                }
            } else { // Window not exposed to wind
                state.dataWindowManager->tout = state.dataSurface->SurfOutDryBulbTemp(SurfNum) + state.dataWindowManager->TKelvin;
            }

            state.dataWindowManager->Ebout = state.dataWindowManager->sigma * pow_4(state.dataWindowManager->tout);
            state.dataWindowManager->Outir =
                surface.ViewFactorSkyIR *
                    (state.dataSurface->SurfAirSkyRadSplit(SurfNum) * state.dataWindowManager->sigma * pow_4(state.dataEnvrn->SkyTempKelvin) +
                     (1.0 - state.dataSurface->SurfAirSkyRadSplit(SurfNum)) * state.dataWindowManager->Ebout) +
                surface.ViewFactorGroundIR * state.dataWindowManager->Ebout;

        } else if (state.dataSurface->SurfWinWindowModelType(SurfNum) == WindowEQLModel) {

            EQLWindowSurfaceHeatBalance(
                state, SurfNum, HextConvCoeff, SurfInsideTemp, SurfOutsideTemp, SurfOutsideEmiss, DataBSDFWindow::Condition::Invalid);
            state.dataWindowManager->hcout = HextConvCoeff;
            // Required for report variables calculations.
            if (surface.ExtWind) {             // Window is exposed to wind (and possibly rain)
                if (state.dataEnvrn->IsRain) { // Raining: since wind exposed, outside window surface gets wet
                    state.dataWindowManager->tout = state.dataSurface->SurfOutWetBulbTemp(SurfNum) + state.dataWindowManager->TKelvin;
                } else { // Dry
                    state.dataWindowManager->tout = state.dataSurface->SurfOutDryBulbTemp(SurfNum) + state.dataWindowManager->TKelvin;
                }
            } else { // Window not exposed to wind
                state.dataWindowManager->tout = state.dataSurface->SurfOutDryBulbTemp(SurfNum) + state.dataWindowManager->TKelvin;
            }

        } else { // regular window, not BSDF, not EQL Window
            // Added for thermochromic windows
            state.dataWindowManager->locTCFlag = (state.dataConstruction->Construct(ConstrNum).TCFlag == 1);

            if (state.dataWindowManager->locTCFlag) {
                locTCSpecTemp = state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).TCLayer).SpecTemp;
                state.dataSurface->SurfWinSpecTemp(SurfNum) = locTCSpecTemp;
                // Check to see whether needs to switch to a new TC window construction
                locTCLayerTemp = state.dataSurface->SurfWinTCLayerTemp(SurfNum);
                dT0 = std::abs(locTCLayerTemp - locTCSpecTemp);
                if (dT0 >= 1) {
                    // Find the TC construction that is closed to the TCLayerTemp
                    i = 0;
                    deltaTemp = 0.0;
                    IDConst = 0;
                    for (k = 1; k <= state.dataHeatBal->TotConstructs; ++k) {
                        if (state.dataConstruction->Construct(k).TCMasterConst == state.dataConstruction->Construct(ConstrNum).TCMasterConst) {
                            dT1 = std::abs(locTCLayerTemp - state.dataMaterial->Material(state.dataConstruction->Construct(k).TCLayer).SpecTemp);
                            if (dT1 < dT0) {
                                ++i;
                                deltaTemp(i) = dT1;
                                IDConst(i) = k;
                            }
                        }
                    }
                    if (i >= 1) {
                        // Find the closest item
                        iMinDT = minloc(deltaTemp, deltaTemp > 0.0);
                        // Use the new TC window construction
                        ConstrNum = IDConst(iMinDT(1));
                        surface.Construction = ConstrNum;
                        state.dataSurface->SurfActiveConstruction(SurfNum) = ConstrNum;
                        state.dataSurface->SurfWinSpecTemp(SurfNum) =
                            state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).TCLayer).SpecTemp;
                    }
                }
            }
            // end new TC code

            ZoneNum = surface.Zone;
            TotLay = state.dataConstruction->Construct(ConstrNum).TotLayers;
            TotGlassLay = state.dataConstruction->Construct(ConstrNum).TotGlassLayers;
            state.dataWindowManager->ngllayer = TotGlassLay;
            state.dataWindowManager->nglface = 2 * state.dataWindowManager->ngllayer;
            ShadeFlag = state.dataSurface->SurfWinShadingFlag(SurfNum);
            state.dataWindowManager->tilt = surface.Tilt;
            state.dataWindowManager->tiltr = state.dataWindowManager->tilt * DataGlobalConstants::DegToRadians;
            SurfNumAdj = surface.ExtBoundCond;
            state.dataWindowManager->hcin = state.dataHeatBalSurf->SurfHConvInt(SurfNum); // Room-side surface convective film conductance
            Real64 RefAirTemp = state.dataSurface->Surface(SurfNum).getInsideAirTemperature(state, SurfNum);
            state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = RefAirTemp;
            state.dataWindowManager->tin = RefAirTemp + state.dataWindowManager->TKelvin; // Inside air temperature

            // Reset hcin if necessary since too small a value sometimes causes non-convergence
            // of window layer heat balance solution.
            if (state.dataSurface->SurfIntConvCoeffIndex(SurfNum) == 0) {
                if (state.dataWindowManager->hcin <=
                    state.dataHeatBal->LowHConvLimit) { // may be redundent now, check is also in HeatBalanceConvectionCoeffs.cc
                    //  hcin = 3.076d0  !BG this is rather high value and abrupt change. changed to set to lower limit
                    state.dataWindowManager->hcin = state.dataHeatBal->LowHConvLimit;
                    state.dataHeatBalSurf->SurfHConvInt(SurfNum) = state.dataWindowManager->hcin; // store for accurate reporting.
                }
            }

            // IR incident on window from zone surfaces and high-temp radiant sources
            state.dataWindowManager->Rmir =
                state.dataSurface->SurfWinIRfromParentZone(SurfNum) + state.dataHeatBalSurf->SurfQdotRadHVACInPerArea(SurfNum);

            // Short-wave radiation (from interior and exterior solar and zone lights)
            // absorbed at each face. Assumes equal split between faces of short-wave absorbed in glass layer.

            for (IGlass = 1; IGlass <= TotGlassLay; ++IGlass) {
                state.dataWindowManager->AbsRadGlassFace[2 * IGlass - 2] = state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum, IGlass) / 2.0;
                state.dataWindowManager->AbsRadGlassFace[2 * IGlass - 1] = state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum, IGlass) / 2.0;
            }

            // IR from zone internal gains (lights, equipment and people) absorbed on zone-side face
            // (assumes inside glass layer is opaque to IR, so no contribution to other layers)
            state.dataWindowManager->AbsRadGlassFace[2 * TotGlassLay - 1] += state.dataHeatBal->SurfQdotRadIntGainsInPerArea(SurfNum);

            // Fill the layer properties needed for the thermal calculation.
            // For switchable glazing it is assumed that thermal properties, such
            // as surface emissivity, are the same for the unswitched and switched state,
            // so the thermal properties of the unswitched state are used.
            // For windows with a blind or shade it is assumed
            // that the blind or shade does not affect the thermal properties of the glazing,
            // so the thermal properties of the construction without the blind or shade are used.

            // The layer and face numbering are as follows (for the triple glazing case):
            // Glass layers are 1,2 and 3, where 1 is the outside (outside environment facing)
            //   layer and 3 is the inside (room-facing) layer;
            // Faces (also called surfaces) are 1,2,3,4,5 and 6, where face 1 is the
            //   outside (front) face of glass layer 1, face 2 is the inside (back)
            //   face of glass layer 1, face 3 is the outer face of glass layer 2, face 4 is the
            //   inner face of glass layer 2, etc.
            // Gap layers are 1 and 2, where gap layer 1 is between glass layers 1 and 2
            //   and gap layer 2 is between glass layers 2 and 3.
            // If an exterior, interior or between-glass blind or shade is in place, 7 and 8
            //   are the blind/shade faces, from outside to inside. If an exterior or interior
            //   blind/shade is in place, gap layer 3 is between the blind/shade and adjacent
            //   glass layer and is assumed to be air.
            // Between-glass blind/shade is modeled only for double and triple glazing.
            //   For double glazing, gap 1 is between glass 1 and blind/shade and gap 2 is between
            //   blind/shade and glass 2.
            //   For triple glazing, the blind/shade is assumed to be between the inner two glass
            //   layers, i.e., between glass layers 2 and 3. In this case gap 1 is between glass 1
            //   and glass 2, gap 2 is between glass 2 and blind/shade, and gap 3 is between
            //   blind/shade and glass 3.

            int IConst = ConstrNum;
            if (ANY_SHADE_SCREEN(ShadeFlag) || ANY_BLIND(ShadeFlag)) {
                IConst = state.dataSurface->SurfWinActiveShadedConstruction(SurfNum);
            }
            TotLay = state.dataConstruction->Construct(IConst).TotLayers;
            IGlass = 0;
            IGap = 0;

            // Fill window layer properties needed for window layer heat balance calculation

            for (Lay = 1; Lay <= TotLay; ++Lay) {
                LayPtr = state.dataConstruction->Construct(IConst).LayerPoint(Lay);

                if ((state.dataMaterial->Material(LayPtr).Group == DataHeatBalance::MaterialGroup::WindowGlass) ||
                    (state.dataMaterial->Material(LayPtr).Group == DataHeatBalance::MaterialGroup::WindowSimpleGlazing)) {
                    ++IGlass;
                    state.dataWindowManager->thick[IGlass - 1] = state.dataMaterial->Material(LayPtr).Thickness;
                    state.dataWindowManager->scon[IGlass - 1] =
                        state.dataMaterial->Material(LayPtr).Conductivity / state.dataMaterial->Material(LayPtr).Thickness;
                    state.dataWindowManager->emis[2 * IGlass - 2] = state.dataMaterial->Material(LayPtr).AbsorpThermalFront;
                    state.dataWindowManager->emis[2 * IGlass - 1] = state.dataMaterial->Material(LayPtr).AbsorpThermalBack;
                    state.dataWindowManager->tir[2 * IGlass - 2] = state.dataMaterial->Material(LayPtr).TransThermal;
                    state.dataWindowManager->tir[2 * IGlass - 1] = state.dataMaterial->Material(LayPtr).TransThermal;
                }

                if (state.dataMaterial->Material(LayPtr).Group == DataHeatBalance::MaterialGroup::Shade ||
                    state.dataMaterial->Material(LayPtr).Group == DataHeatBalance::MaterialGroup::WindowBlind ||
                    state.dataMaterial->Material(LayPtr).Group == DataHeatBalance::MaterialGroup::Screen) {
                    if (ANY_INTERIOR_SHADE_BLIND(ShadeFlag))
                        ShadeLayPtr = state.dataConstruction->Construct(IConst).LayerPoint(state.dataConstruction->Construct(IConst).TotLayers);
                    if (ANY_EXTERIOR_SHADE_BLIND_SCREEN(ShadeFlag)) ShadeLayPtr = state.dataConstruction->Construct(IConst).LayerPoint(1);
                    if (ANY_BETWEENGLASS_SHADE_BLIND(ShadeFlag)) {
                        ShadeLayPtr = state.dataConstruction->Construct(IConst).LayerPoint(3);
                        if (TotGlassLay == 3) ShadeLayPtr = state.dataConstruction->Construct(IConst).LayerPoint(5);
                    }
                    if (ANY_SHADE_SCREEN(ShadeFlag)) {
                        // Shade or screen on
                        if (state.dataGlobal
                                ->AnyEnergyManagementSystemInModel) { // check to make sure the user hasn't messed up the shade control values
                            if (state.dataMaterial->Material(ShadeLayPtr).Group == DataHeatBalance::MaterialGroup::WindowBlind) {
                                ShowSevereError(state,
                                                "CalcWindowHeatBalance: ShadeFlag indicates Shade but Blind=\"" +
                                                    state.dataMaterial->Material(ShadeLayPtr).Name + "\" is being used.");
                                ShowContinueError(state, "This is most likely a fault of the EMS values for shading control.");
                                ShowFatalError(state, "Preceding condition terminates program.");
                            }
                        }
                        state.dataWindowManager->thick[TotGlassLay] = state.dataMaterial->Material(ShadeLayPtr).Thickness;
                        state.dataWindowManager->scon[TotGlassLay] =
                            state.dataMaterial->Material(ShadeLayPtr).Conductivity / state.dataMaterial->Material(ShadeLayPtr).Thickness;
                        if (ShadeFlag == WinShadingType::ExtScreen) {
                            state.dataWindowManager->emis[state.dataWindowManager->nglface] =
                                state.dataMaterial->Material(ShadeLayPtr).AbsorpThermalFront;
                            state.dataWindowManager->tir[state.dataWindowManager->nglface] =
                                state.dataHeatBal->SurfaceScreens(state.dataMaterial->Material(ShadeLayPtr).ScreenDataPtr).DifDifTrans;
                            state.dataWindowManager->tir[state.dataWindowManager->nglface + 1] =
                                state.dataHeatBal->SurfaceScreens(state.dataMaterial->Material(ShadeLayPtr).ScreenDataPtr).DifDifTrans;
                        } else {
                            state.dataWindowManager->emis[state.dataWindowManager->nglface] = state.dataMaterial->Material(ShadeLayPtr).AbsorpThermal;
                            state.dataWindowManager->tir[state.dataWindowManager->nglface] = state.dataMaterial->Material(ShadeLayPtr).TransThermal;
                            state.dataWindowManager->tir[state.dataWindowManager->nglface + 1] =
                                state.dataMaterial->Material(ShadeLayPtr).TransThermal;
                        }
                        state.dataWindowManager->emis[state.dataWindowManager->nglface + 1] = state.dataMaterial->Material(ShadeLayPtr).AbsorpThermal;

                    } else {
                        if (state.dataGlobal
                                ->AnyEnergyManagementSystemInModel) { // check to make sure the user hasn't messed up the shade control values
                            if (state.dataMaterial->Material(ShadeLayPtr).Group == DataHeatBalance::MaterialGroup::Shade ||
                                state.dataMaterial->Material(ShadeLayPtr).Group == DataHeatBalance::MaterialGroup::Screen) {
                                ShowSevereError(state,
                                                "CalcWindowHeatBalance: ShadeFlag indicates Blind but Shade/Screen=\"" +
                                                    state.dataMaterial->Material(ShadeLayPtr).Name + "\" is being used.");
                                ShowContinueError(state, "This is most likely a fault of the EMS values for shading control.");
                                ShowFatalError(state, "Preceding condition terminates program.");
                            }
                        }
                        // Blind on
                        BlNum = state.dataSurface->SurfWinBlindNumber(SurfNum);
                        state.dataWindowManager->thick[TotGlassLay] = state.dataHeatBal->Blind(BlNum).SlatThickness;
                        state.dataWindowManager->scon[TotGlassLay] =
                            state.dataHeatBal->Blind(BlNum).SlatConductivity / state.dataHeatBal->Blind(BlNum).SlatThickness;

                        if (state.dataSurface->SurfWinMovableSlats(SurfNum)) {
                            int SurfWinSlatsAngIndex = state.dataSurface->SurfWinSlatsAngIndex(SurfNum);
                            Real64 SurfWinSlatsAngInterpFac = state.dataSurface->SurfWinSlatsAngInterpFac(SurfNum);
                            state.dataWindowManager->emis[state.dataWindowManager->nglface] =
                                General::InterpGeneral(state.dataHeatBal->Blind(BlNum).IRFrontEmiss(SurfWinSlatsAngIndex),
                                                       state.dataHeatBal->Blind(BlNum).IRFrontEmiss(std::min(MaxSlatAngs, SurfWinSlatsAngIndex + 1)),
                                                       SurfWinSlatsAngInterpFac);
                            state.dataWindowManager->emis[state.dataWindowManager->nglface + 1] =
                                General::InterpGeneral(state.dataHeatBal->Blind(BlNum).IRBackEmiss(SurfWinSlatsAngIndex),
                                                       state.dataHeatBal->Blind(BlNum).IRBackEmiss(std::min(MaxSlatAngs, SurfWinSlatsAngIndex + 1)),
                                                       SurfWinSlatsAngInterpFac);
                            state.dataWindowManager->tir[state.dataWindowManager->nglface] =
                                General::InterpGeneral(state.dataHeatBal->Blind(BlNum).IRFrontTrans(SurfWinSlatsAngIndex),
                                                       state.dataHeatBal->Blind(BlNum).IRFrontTrans(std::min(MaxSlatAngs, SurfWinSlatsAngIndex + 1)),
                                                       SurfWinSlatsAngInterpFac);
                            state.dataWindowManager->tir[state.dataWindowManager->nglface + 1] =
                                General::InterpGeneral(state.dataHeatBal->Blind(BlNum).IRBackTrans(SurfWinSlatsAngIndex),
                                                       state.dataHeatBal->Blind(BlNum).IRBackTrans(std::min(MaxSlatAngs, SurfWinSlatsAngIndex + 1)),
                                                       SurfWinSlatsAngInterpFac);
                        } else {
                            state.dataWindowManager->emis[state.dataWindowManager->nglface] = state.dataHeatBal->Blind(BlNum).IRFrontEmiss(1);
                            state.dataWindowManager->emis[state.dataWindowManager->nglface + 1] = state.dataHeatBal->Blind(BlNum).IRBackEmiss(1);
                            state.dataWindowManager->tir[state.dataWindowManager->nglface] = state.dataHeatBal->Blind(BlNum).IRFrontTrans(1);
                            state.dataWindowManager->tir[state.dataWindowManager->nglface + 1] = state.dataHeatBal->Blind(BlNum).IRBackTrans(1);
                        }
                    }
                }

                if (state.dataMaterial->Material(LayPtr).Group == DataHeatBalance::MaterialGroup::WindowGas ||
                    state.dataMaterial->Material(LayPtr).Group == DataHeatBalance::MaterialGroup::WindowGasMixture) {
                    ++IGap;
                    state.dataWindowManager->gap[IGap - 1] = state.dataMaterial->Material(LayPtr).Thickness;
                    state.dataWindowManager->gnmix[IGap - 1] = state.dataMaterial->Material(LayPtr).NumberOfGasesInMixture;
                    for (IMix = 1; IMix <= state.dataWindowManager->gnmix[IGap - 1]; ++IMix) {
                        state.dataWindowManager->gwght[IMix - 1][IGap - 1] = state.dataMaterial->Material(LayPtr).GasWght(IMix);
                        state.dataWindowManager->gfract[IMix - 1][IGap - 1] = state.dataMaterial->Material(LayPtr).GasFract(IMix);
                        for (ICoeff = 1; ICoeff <= 3; ++ICoeff) {
                            state.dataWindowManager->gcon[ICoeff - 1][IMix - 1][IGap - 1] = state.dataMaterial->Material(LayPtr).GasCon(ICoeff, IMix);
                            state.dataWindowManager->gvis[ICoeff - 1][IMix - 1][IGap - 1] = state.dataMaterial->Material(LayPtr).GasVis(ICoeff, IMix);
                            state.dataWindowManager->gcp[ICoeff - 1][IMix - 1][IGap - 1] = state.dataMaterial->Material(LayPtr).GasCp(ICoeff, IMix);
                        }
                    }
                }

            } // End of loop over glass, gap and blind/shade layers in a window construction

            if (ANY_INTERIOR_SHADE_BLIND(ShadeFlag) || ANY_EXTERIOR_SHADE_BLIND_SCREEN(ShadeFlag)) {
                // Interior or exterior blind, shade or screen is on.
                // Fill gap between blind/shade and adjacent glass with air properties.
                ++IGap;
                if (ShadeFlag == WinShadingType::IntShade || ShadeFlag == WinShadingType::ExtShade ||
                    ShadeFlag == WinShadingType::ExtScreen) { // Interior or exterior shade
                    state.dataWindowManager->gap[IGap - 1] = state.dataMaterial->Material(ShadeLayPtr).WinShadeToGlassDist;
                } else { // Interior or exterior blind
                    state.dataWindowManager->gap[IGap - 1] =
                        state.dataHeatBal->Blind(state.dataSurface->SurfWinBlindNumber(SurfNum)).BlindToGlassDist;
                }
                state.dataWindowManager->gnmix[IGap - 1] = 1;
                state.dataWindowManager->gwght[0][IGap - 1] = GasWght[0];
                for (ICoeff = 1; ICoeff <= 3; ++ICoeff) {
                    state.dataWindowManager->gcon[ICoeff - 1][0][IGap - 1] = GasCoeffsCon[ICoeff - 1][0];
                    state.dataWindowManager->gvis[ICoeff - 1][0][IGap - 1] = GasCoeffsVis[ICoeff - 1][0];
                    state.dataWindowManager->gcp[ICoeff - 1][0][IGap - 1] = GasCoeffsCp[ICoeff - 1][0];
                }
            }

            // Exterior convection coefficient, exterior air temperature and IR radiance
            // of exterior surround. Depend on whether window is interzone (in an interzone
            // wall or exterior (in an exterior wall).

            state.dataWindowManager->hcout = HextConvCoeff; // Exterior convection coefficient is passed in from outer routine
            // tsky = SkyTemp + TKelvin

            if (SurfNumAdj > 0) { // Interzone window

                ZoneNumAdj = state.dataSurface->Surface(SurfNumAdj).Zone;
                Real64 RefAirTemp = state.dataSurface->Surface(SurfNumAdj).getInsideAirTemperature(state, SurfNumAdj);
                state.dataHeatBal->SurfTempEffBulkAir(SurfNumAdj) = RefAirTemp;
                state.dataWindowManager->tout = RefAirTemp + state.dataWindowManager->TKelvin; // outside air temperature

                // Add long-wave radiation from adjacent zone absorbed by glass layer closest to the adjacent zone.
                state.dataWindowManager->AbsRadGlassFace[0] += state.dataHeatBal->SurfQdotRadIntGainsInPerArea(SurfNumAdj);

                // The IR radiance of this window's "exterior" surround is the IR radiance
                // from surfaces and high-temp radiant sources in the adjacent zone

                state.dataWindowManager->Outir =
                    state.dataSurface->SurfWinIRfromParentZone(SurfNumAdj) + state.dataHeatBalSurf->SurfQdotRadHVACInPerArea(SurfNumAdj);

            } else { // Exterior window (Ext BoundCond = 0)
                // Calculate LWR from surrounding surfaces if defined for an exterior window
                OutSrdIR = 0;
                if (state.dataGlobal->AnyLocalEnvironmentsInModel) {
                    if (state.dataSurface->SurfHasSurroundingSurfProperties(SurfNum)) {
                        int SrdSurfsNum = state.dataSurface->SurfSurroundingSurfacesNum(SurfNum);
                        auto &SrdSurfsProperty = state.dataSurface->SurroundingSurfsProperty(SrdSurfsNum);
                        for (int SrdSurfNum = 1; SrdSurfNum <= SrdSurfsProperty.TotSurroundingSurface; SrdSurfNum++) {
                            SrdSurfViewFac = SrdSurfsProperty.SurroundingSurfs(SrdSurfNum).ViewFactor;
                            SrdSurfTempAbs = GetCurrentScheduleValue(state, SrdSurfsProperty.SurroundingSurfs(SrdSurfNum).TempSchNum) +
                                             DataGlobalConstants::KelvinConv;
                            OutSrdIR += state.dataWindowManager->sigma * SrdSurfViewFac * pow_4(SrdSurfTempAbs);
                        }
                    }
                }
                if (surface.ExtWind) {             // Window is exposed to wind (and possibly rain)
                    if (state.dataEnvrn->IsRain) { // Raining: since wind exposed, outside window surface gets wet
                        state.dataWindowManager->tout = state.dataSurface->SurfOutWetBulbTemp(SurfNum) + state.dataWindowManager->TKelvin;
                    } else { // Dry
                        state.dataWindowManager->tout = state.dataSurface->SurfOutDryBulbTemp(SurfNum) + state.dataWindowManager->TKelvin;
                    }
                } else { // Window not exposed to wind
                    state.dataWindowManager->tout = state.dataSurface->SurfOutDryBulbTemp(SurfNum) + state.dataWindowManager->TKelvin;
                }
                state.dataWindowManager->Ebout = state.dataWindowManager->sigma * pow_4(state.dataWindowManager->tout);
                state.dataWindowManager->Outir =
                    surface.ViewFactorSkyIR *
                        (state.dataSurface->SurfAirSkyRadSplit(SurfNum) * state.dataWindowManager->sigma * pow_4(state.dataEnvrn->SkyTempKelvin) +
                         (1.0 - state.dataSurface->SurfAirSkyRadSplit(SurfNum)) * state.dataWindowManager->Ebout) +
                    surface.ViewFactorGroundIR * state.dataWindowManager->Ebout + OutSrdIR;
            }

            // Factors used in window layer temperature solution
            if (state.dataWindowManager->ngllayer >= 2) {
                state.dataWindowManager->A23P =
                    -state.dataWindowManager->emis[2] / (1.0 - (1.0 - state.dataWindowManager->emis[1]) * (1.0 - state.dataWindowManager->emis[2]));
                state.dataWindowManager->A32P =
                    state.dataWindowManager->emis[1] / (1.0 - (1.0 - state.dataWindowManager->emis[1]) * (1.0 - state.dataWindowManager->emis[2]));
                state.dataWindowManager->A23 = state.dataWindowManager->emis[1] * state.dataWindowManager->sigma * state.dataWindowManager->A23P;
            }

            if (state.dataWindowManager->ngllayer >= 3) {
                state.dataWindowManager->A45P =
                    -state.dataWindowManager->emis[4] / (1.0 - (1.0 - state.dataWindowManager->emis[3]) * (1.0 - state.dataWindowManager->emis[4]));
                state.dataWindowManager->A54P =
                    state.dataWindowManager->emis[3] / (1.0 - (1.0 - state.dataWindowManager->emis[3]) * (1.0 - state.dataWindowManager->emis[4]));
                state.dataWindowManager->A45 = state.dataWindowManager->emis[3] * state.dataWindowManager->sigma * state.dataWindowManager->A45P;
            }

            if (state.dataWindowManager->ngllayer == 4) {
                state.dataWindowManager->A67P =
                    -state.dataWindowManager->emis[6] / (1.0 - (1.0 - state.dataWindowManager->emis[5]) * (1.0 - state.dataWindowManager->emis[6]));
                state.dataWindowManager->A76P =
                    state.dataWindowManager->emis[5] / (1.0 - (1.0 - state.dataWindowManager->emis[5]) * (1.0 - state.dataWindowManager->emis[6]));
                state.dataWindowManager->A67 = state.dataWindowManager->emis[5] * state.dataWindowManager->sigma * state.dataWindowManager->A67P;
            }

            state.dataWindowManager->thetas = {0.0};
            state.dataWindowManager->thetasPrev = {0.0};
            state.dataWindowManager->fvec = {0.0};

            // Calculate window face temperatures

            SolveForWindowTemperatures(state, SurfNum);

            // Temperature difference across glass layers (for debugging)

            dth1 = state.dataWindowManager->thetas[1] - state.dataWindowManager->thetas[0];
            dth2 = state.dataWindowManager->thetas[3] - state.dataWindowManager->thetas[2];
            dth3 = state.dataWindowManager->thetas[5] - state.dataWindowManager->thetas[4];
            dth4 = state.dataWindowManager->thetas[7] - state.dataWindowManager->thetas[6];

            if (ANY_INTERIOR_SHADE_BLIND(ShadeFlag)) {
                SurfInsideTemp = state.dataWindowManager->thetas[2 * state.dataWindowManager->ngllayer + 1] - state.dataWindowManager->TKelvin;
                if (state.dataSurface->SurfWinMovableSlats(SurfNum)) {
                    EffShBlEmiss =
                        General::InterpGeneral(window.EffShBlindEmiss(state.dataSurface->SurfWinSlatsAngIndex(SurfNum)),
                                               window.EffShBlindEmiss(std::min(MaxSlatAngs, state.dataSurface->SurfWinSlatsAngIndex(SurfNum) + 1)),
                                               state.dataSurface->SurfWinSlatsAngInterpFac(SurfNum));
                    EffGlEmiss =
                        General::InterpGeneral(window.EffGlassEmiss(state.dataSurface->SurfWinSlatsAngIndex(SurfNum)),
                                               window.EffGlassEmiss(std::min(MaxSlatAngs, state.dataSurface->SurfWinSlatsAngIndex(SurfNum) + 1)),
                                               state.dataSurface->SurfWinSlatsAngInterpFac(SurfNum));
                } else {
                    EffShBlEmiss = state.dataSurface->SurfaceWindow(SurfNum).EffShBlindEmiss(1);
                    EffGlEmiss = state.dataSurface->SurfaceWindow(SurfNum).EffGlassEmiss(1);
                }
                state.dataSurface->SurfWinEffInsSurfTemp(SurfNum) =
                    (EffShBlEmiss * SurfInsideTemp +
                     EffGlEmiss * (state.dataWindowManager->thetas[2 * state.dataWindowManager->ngllayer - 1] - state.dataWindowManager->TKelvin)) /
                    (EffShBlEmiss + EffGlEmiss);
            } else {
                SurfInsideTemp = state.dataWindowManager->thetas[2 * state.dataWindowManager->ngllayer - 1] - state.dataWindowManager->TKelvin;
            }
            if (ANY_EXTERIOR_SHADE_BLIND_SCREEN(ShadeFlag)) {
                SurfOutsideTemp = state.dataWindowManager->thetas[2 * state.dataWindowManager->ngllayer] -
                                  state.dataWindowManager->TKelvin; // this index looks suspicious (CR 8202)
                // SurfOutsideEmiss = emis(1)  ! this index should be coordinated with previous line
                SurfOutsideEmiss = state.dataWindowManager->emis[2 * state.dataWindowManager->ngllayer]; // fix for CR 8202
            } else {
                SurfOutsideEmiss = state.dataWindowManager->emis[0];
                SurfOutsideTemp = state.dataWindowManager->thetas[0] - state.dataWindowManager->TKelvin;
            }

            // Save temperatures for use next time step

            for (k = 1; k <= state.dataWindowManager->nglfacep; ++k) {
                window.ThetaFace(k) = state.dataWindowManager->thetas[k - 1];
            }

            // Added TH 12/23/2008 for thermochromic windows to save the current TC layer temperature
            if (state.dataWindowManager->locTCFlag) {
                state.dataSurface->SurfWinTCLayerTemp(SurfNum) =
                    (state.dataWindowManager->thetas[2 * state.dataConstruction->Construct(ConstrNum).TCGlassID - 2] +
                     state.dataWindowManager->thetas[2 * state.dataConstruction->Construct(ConstrNum).TCGlassID - 1]) /
                        2 -
                    state.dataWindowManager->TKelvin; // degree C
            }
        } // regular window, not BSDF, not EQL

        // Set condensation flag to 1 if condensation expected to occur on the innermost glass face,
        // or, for airflow windows, on either or the two glass faces in the airflow gap
        if (!state.dataConstruction->Construct(surface.Construction).WindowTypeEQL) {
            InsideGlassTemp = state.dataWindowManager->thetas[2 * state.dataWindowManager->ngllayer - 1] - state.dataWindowManager->TKelvin;
            RoomHumRat = state.dataHeatBalFanSys->ZoneAirHumRat(surface.Zone);
            RoomDewPoint = PsyTdpFnWPb(state, RoomHumRat, state.dataEnvrn->OutBaroPress);
            state.dataSurface->SurfWinInsideGlassCondensationFlag(SurfNum) = 0;
            if (InsideGlassTemp < RoomDewPoint) state.dataSurface->SurfWinInsideGlassCondensationFlag(SurfNum) = 1;
            // If airflow window, is there condensation on either glass face of the airflow gap?
            if (state.dataSurface->SurfWinAirflowThisTS(SurfNum) > 0.0) {
                Tleft = state.dataWindowManager->thetas[2 * state.dataWindowManager->ngllayer - 3] - state.dataWindowManager->TKelvin;
                Tright = state.dataWindowManager->thetas[2 * state.dataWindowManager->ngllayer - 2] - state.dataWindowManager->TKelvin;
                if (state.dataSurface->SurfWinAirflowSource(SurfNum) == AirFlowWindow_Source_IndoorAir) {
                    if (Tleft < RoomDewPoint || Tright < RoomDewPoint) state.dataSurface->SurfWinInsideGlassCondensationFlag(SurfNum) = 1;
                } else if (state.dataSurface->SurfWinAirflowSource(SurfNum) == AirFlowWindow_Source_OutdoorAir) {
                    if (Tleft < state.dataEnvrn->OutDewPointTemp || Tright < state.dataEnvrn->OutDewPointTemp)
                        state.dataSurface->SurfWinInsideGlassCondensationFlag(SurfNum) = 1;
                }
            }

            // Do frame and divider calculation
            if (state.dataSurface->SurfWinFrameArea(SurfNum) > 0.0 || state.dataSurface->SurfWinDividerArea(SurfNum) > 0.0)
                CalcWinFrameAndDividerTemps(state,
                                            SurfNum,
                                            state.dataWindowManager->tout,
                                            state.dataWindowManager->tin,
                                            state.dataWindowManager->hcout,
                                            state.dataWindowManager->hcin,
                                            state.dataWindowManager->Outir,
                                            ConstrNum);
            if (state.dataSurface->SurfWinFrameArea(SurfNum) > 0.0) {
                state.dataSurface->SurfWinInsideFrameCondensationFlag(SurfNum) = 0;
                if (state.dataSurface->SurfWinFrameTempIn(SurfNum) < RoomDewPoint) state.dataSurface->SurfWinInsideFrameCondensationFlag(SurfNum) = 1;
            }
            if (state.dataSurface->SurfWinDividerArea(SurfNum) > 0.0) {
                state.dataSurface->SurfWinInsideDividerCondensationFlag(SurfNum) = 0;
                if (state.dataSurface->SurfWinDividerTempIn(SurfNum) < RoomDewPoint)
                    state.dataSurface->SurfWinInsideDividerCondensationFlag(SurfNum) = 1;
            }
        }
        // update exterior environment surface heat loss reporting
        Tsout = SurfOutsideTemp + state.dataWindowManager->TKelvin;
        state.dataHeatBalSurf->SurfQdotConvOutPerArea(SurfNum) = -state.dataWindowManager->hcout * (Tsout - state.dataWindowManager->tout);

        Real64 const Tsout_4(pow_4(Tsout)); // Tuned To reduce pow calls and redundancies
        Real64 const Tout_4(pow_4(state.dataWindowManager->tout));
        Real64 const emiss_sigma_product(SurfOutsideEmiss * state.dataWindowManager->sigma);
        Real64 rad_out_lw_srd_per_area = 0;

        if (state.dataGlobal->AnyLocalEnvironmentsInModel) {
            if (state.dataSurface->SurfHasSurroundingSurfProperties(SurfNum)) {
                int SrdSurfsNum = state.dataSurface->SurfSurroundingSurfacesNum(SurfNum);
                for (int SrdSurfNum = 1; SrdSurfNum <= state.dataSurface->SurroundingSurfsProperty(SrdSurfsNum).TotSurroundingSurface; SrdSurfNum++) {
                    SrdSurfViewFac = state.dataSurface->SurroundingSurfsProperty(SrdSurfsNum).SurroundingSurfs(SrdSurfNum).ViewFactor;
                    SrdSurfTempAbs = GetCurrentScheduleValue(
                                         state, state.dataSurface->SurroundingSurfsProperty(SrdSurfsNum).SurroundingSurfs(SrdSurfNum).TempSchNum) +
                                     DataGlobalConstants::KelvinConv;
                    rad_out_lw_srd_per_area += -emiss_sigma_product * SrdSurfViewFac * (Tsout_4 - pow_4(SrdSurfTempAbs));
                }
            }
        }

        Real64 const rad_out_air_per_area =
            -emiss_sigma_product * (1.0 - state.dataSurface->SurfAirSkyRadSplit(SurfNum)) * surface.ViewFactorSkyIR * (Tsout_4 - Tout_4);
        Real64 const rad_out_ground_per_area = -emiss_sigma_product * surface.ViewFactorGroundIR * (Tsout_4 - Tout_4);
        Real64 const rad_out_sky_per_area = -emiss_sigma_product * state.dataSurface->SurfAirSkyRadSplit(SurfNum) * surface.ViewFactorSkyIR *
                                            (Tsout_4 - pow_4(state.dataEnvrn->SkyTempKelvin));
        Real64 const rad_out_per_area = rad_out_air_per_area + rad_out_sky_per_area + rad_out_ground_per_area + rad_out_lw_srd_per_area;

        state.dataHeatBalSurf->SurfHAirExt(SurfNum) = rad_out_air_per_area / (Tsout - state.dataWindowManager->tout);
        state.dataHeatBalSurf->SurfQRadLWOutSrdSurfs(SurfNum) = rad_out_lw_srd_per_area;
        state.dataHeatBalSurf->SurfQdotRadOutRepPerArea(SurfNum) = rad_out_per_area;
    }

    //****************************************************************************

    void WindowHeatBalanceEquations(EnergyPlusData &state, int const SurfNum) // Surface number
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         F. Winkelmann
        //       DATE WRITTEN   February 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Evaluates heat balance functions at each glass face.
        // Also evaluates Jacobian.
        // Currently limited to three glass layers.

        Array1D<Real64> hgap(5); // Gap gas conductance
        Real64 gr;               // Gap gas Grashof number
        Real64 con;              // Gap gas conductivity
        Real64 pr;               // Gap gas Prandtl number
        Real64 nu;               // Gap gas Nusselt number
        Real64 thetas_2_3_4;
        Real64 thetas_4_5_4;
        Real64 thetas_6_7_4;

        // Have to zero fvec each time since LUdecompostion and LUsolution may
        // add values to this array in unexpected places
        state.dataWindowManager->fvec = {0.0};

        switch (state.dataWindowManager->ngllayer) {

        case 1: // single pane
            state.dataWindowManager->fvec[0] =
                state.dataWindowManager->Outir * state.dataWindowManager->emis[0] -
                state.dataWindowManager->emis[0] * state.dataWindowManager->sigma * pow_4(state.dataWindowManager->thetas[0]) +
                state.dataWindowManager->scon[0] * (state.dataWindowManager->thetas[1] - state.dataWindowManager->thetas[0]) +
                state.dataWindowManager->hcout * (state.dataWindowManager->tout - state.dataWindowManager->thetas[0]) +
                state.dataWindowManager->AbsRadGlassFace[0];
            state.dataWindowManager->fvec[1] =
                state.dataWindowManager->Rmir * state.dataWindowManager->emis[1] -
                state.dataWindowManager->emis[1] * state.dataWindowManager->sigma * pow_4(state.dataWindowManager->thetas[1]) +
                state.dataWindowManager->scon[0] * (state.dataWindowManager->thetas[0] - state.dataWindowManager->thetas[1]) +
                state.dataWindowManager->hcin * (state.dataWindowManager->tin - state.dataWindowManager->thetas[1]) +
                state.dataWindowManager->AbsRadGlassFace[1];
            break;
        case 2: // double pane
            WindowGasConductance(state, state.dataWindowManager->thetas[1], state.dataWindowManager->thetas[2], 1, con, pr, gr);
            NusseltNumber(state, SurfNum, state.dataWindowManager->thetas[1], state.dataWindowManager->thetas[2], 1, gr, pr, nu);
            hgap(1) = (con / state.dataWindowManager->gap[0] * nu) * state.dataSurface->SurfWinEdgeGlCorrFac(SurfNum);

            state.dataWindowManager->fvec[0] =
                state.dataWindowManager->Outir * state.dataWindowManager->emis[0] -
                state.dataWindowManager->emis[0] * state.dataWindowManager->sigma * pow_4(state.dataWindowManager->thetas[0]) +
                state.dataWindowManager->scon[0] * (state.dataWindowManager->thetas[1] - state.dataWindowManager->thetas[0]) +
                state.dataWindowManager->hcout * (state.dataWindowManager->tout - state.dataWindowManager->thetas[0]) +
                state.dataWindowManager->AbsRadGlassFace[0];
            thetas_2_3_4 = pow_4(state.dataWindowManager->thetas[1]) - pow_4(state.dataWindowManager->thetas[2]);
            state.dataWindowManager->fvec[1] =
                state.dataWindowManager->scon[0] * (state.dataWindowManager->thetas[0] - state.dataWindowManager->thetas[1]) +
                hgap(1) * (state.dataWindowManager->thetas[2] - state.dataWindowManager->thetas[1]) + state.dataWindowManager->A23 * thetas_2_3_4 +
                state.dataWindowManager->AbsRadGlassFace[1];
            state.dataWindowManager->fvec[2] =
                hgap(1) * (state.dataWindowManager->thetas[1] - state.dataWindowManager->thetas[2]) +
                state.dataWindowManager->scon[1] * (state.dataWindowManager->thetas[3] - state.dataWindowManager->thetas[2]) -
                state.dataWindowManager->A23 * thetas_2_3_4 + state.dataWindowManager->AbsRadGlassFace[2];
            state.dataWindowManager->fvec[3] =
                state.dataWindowManager->Rmir * state.dataWindowManager->emis[3] -
                state.dataWindowManager->emis[3] * state.dataWindowManager->sigma * pow_4(state.dataWindowManager->thetas[3]) +
                state.dataWindowManager->scon[1] * (state.dataWindowManager->thetas[2] - state.dataWindowManager->thetas[3]) +
                state.dataWindowManager->hcin * (state.dataWindowManager->tin - state.dataWindowManager->thetas[3]) +
                state.dataWindowManager->AbsRadGlassFace[3];
            break;
        case 3: // Triple Pane
            WindowGasConductance(state, state.dataWindowManager->thetas[1], state.dataWindowManager->thetas[2], 1, con, pr, gr);
            NusseltNumber(state, SurfNum, state.dataWindowManager->thetas[1], state.dataWindowManager->thetas[2], 1, gr, pr, nu);
            hgap(1) = con / state.dataWindowManager->gap[0] * nu * state.dataSurface->SurfWinEdgeGlCorrFac(SurfNum);

            WindowGasConductance(state, state.dataWindowManager->thetas[3], state.dataWindowManager->thetas[4], 2, con, pr, gr);
            NusseltNumber(state, SurfNum, state.dataWindowManager->thetas[3], state.dataWindowManager->thetas[4], 2, gr, pr, nu);
            hgap(2) = con / state.dataWindowManager->gap[1] * nu * state.dataSurface->SurfWinEdgeGlCorrFac(SurfNum);

            thetas_2_3_4 = pow_4(state.dataWindowManager->thetas[1]) - pow_4(state.dataWindowManager->thetas[2]);
            thetas_4_5_4 = pow_4(state.dataWindowManager->thetas[3]) - pow_4(state.dataWindowManager->thetas[4]);
            state.dataWindowManager->fvec[0] =
                state.dataWindowManager->Outir * state.dataWindowManager->emis[0] -
                state.dataWindowManager->emis[0] * state.dataWindowManager->sigma * pow_4(state.dataWindowManager->thetas[0]) +
                state.dataWindowManager->scon[0] * (state.dataWindowManager->thetas[1] - state.dataWindowManager->thetas[0]) +
                state.dataWindowManager->hcout * (state.dataWindowManager->tout - state.dataWindowManager->thetas[0]) +
                state.dataWindowManager->AbsRadGlassFace[0];
            state.dataWindowManager->fvec[1] =
                state.dataWindowManager->scon[0] * (state.dataWindowManager->thetas[0] - state.dataWindowManager->thetas[1]) +
                hgap(1) * (state.dataWindowManager->thetas[2] - state.dataWindowManager->thetas[1]) + state.dataWindowManager->A23 * thetas_2_3_4 +
                state.dataWindowManager->AbsRadGlassFace[1];
            state.dataWindowManager->fvec[2] =
                hgap(1) * (state.dataWindowManager->thetas[1] - state.dataWindowManager->thetas[2]) +
                state.dataWindowManager->scon[1] * (state.dataWindowManager->thetas[3] - state.dataWindowManager->thetas[2]) -
                state.dataWindowManager->A23 * thetas_2_3_4 + state.dataWindowManager->AbsRadGlassFace[2];
            state.dataWindowManager->fvec[3] =
                state.dataWindowManager->scon[1] * (state.dataWindowManager->thetas[2] - state.dataWindowManager->thetas[3]) +
                hgap(2) * (state.dataWindowManager->thetas[4] - state.dataWindowManager->thetas[3]) + state.dataWindowManager->A45 * thetas_4_5_4 +
                state.dataWindowManager->AbsRadGlassFace[3];
            state.dataWindowManager->fvec[4] =
                hgap(2) * (state.dataWindowManager->thetas[3] - state.dataWindowManager->thetas[4]) +
                state.dataWindowManager->scon[2] * (state.dataWindowManager->thetas[5] - state.dataWindowManager->thetas[4]) -
                state.dataWindowManager->A45 * thetas_4_5_4 + state.dataWindowManager->AbsRadGlassFace[4];
            state.dataWindowManager->fvec[5] =
                state.dataWindowManager->Rmir * state.dataWindowManager->emis[5] -
                state.dataWindowManager->emis[5] * state.dataWindowManager->sigma * pow_4(state.dataWindowManager->thetas[5]) +
                state.dataWindowManager->scon[2] * (state.dataWindowManager->thetas[4] - state.dataWindowManager->thetas[5]) +
                state.dataWindowManager->hcin * (state.dataWindowManager->tin - state.dataWindowManager->thetas[5]) +
                state.dataWindowManager->AbsRadGlassFace[5];
            break;
        case 4: // Quad Pane
            WindowGasConductance(state, state.dataWindowManager->thetas[1], state.dataWindowManager->thetas[2], 1, con, pr, gr);
            NusseltNumber(state, SurfNum, state.dataWindowManager->thetas[1], state.dataWindowManager->thetas[2], 1, gr, pr, nu);
            hgap(1) = con / state.dataWindowManager->gap[0] * nu * state.dataSurface->SurfWinEdgeGlCorrFac(SurfNum);

            WindowGasConductance(state, state.dataWindowManager->thetas[3], state.dataWindowManager->thetas[4], 2, con, pr, gr);
            NusseltNumber(state, SurfNum, state.dataWindowManager->thetas[3], state.dataWindowManager->thetas[4], 2, gr, pr, nu);
            hgap(2) = con / state.dataWindowManager->gap[1] * nu * state.dataSurface->SurfWinEdgeGlCorrFac(SurfNum);

            WindowGasConductance(state, state.dataWindowManager->thetas[5], state.dataWindowManager->thetas[6], 3, con, pr, gr);
            NusseltNumber(state, SurfNum, state.dataWindowManager->thetas[5], state.dataWindowManager->thetas[6], 3, gr, pr, nu);
            hgap(3) = con / state.dataWindowManager->gap[2] * nu * state.dataSurface->SurfWinEdgeGlCorrFac(SurfNum);

            thetas_2_3_4 = pow_4(state.dataWindowManager->thetas[1]) - pow_4(state.dataWindowManager->thetas[2]);
            thetas_4_5_4 = pow_4(state.dataWindowManager->thetas[3]) - pow_4(state.dataWindowManager->thetas[4]);
            thetas_6_7_4 = pow_4(state.dataWindowManager->thetas[5]) - pow_4(state.dataWindowManager->thetas[6]);
            state.dataWindowManager->fvec[0] =
                state.dataWindowManager->Outir * state.dataWindowManager->emis[0] -
                state.dataWindowManager->emis[0] * state.dataWindowManager->sigma * pow_4(state.dataWindowManager->thetas[0]) +
                state.dataWindowManager->scon[0] * (state.dataWindowManager->thetas[1] - state.dataWindowManager->thetas[0]) +
                state.dataWindowManager->hcout * (state.dataWindowManager->tout - state.dataWindowManager->thetas[0]) +
                state.dataWindowManager->AbsRadGlassFace[0];
            state.dataWindowManager->fvec[1] =
                state.dataWindowManager->scon[0] * (state.dataWindowManager->thetas[0] - state.dataWindowManager->thetas[1]) +
                hgap(1) * (state.dataWindowManager->thetas[2] - state.dataWindowManager->thetas[1]) + state.dataWindowManager->A23 * thetas_2_3_4 +
                state.dataWindowManager->AbsRadGlassFace[1];
            state.dataWindowManager->fvec[2] =
                hgap(1) * (state.dataWindowManager->thetas[1] - state.dataWindowManager->thetas[2]) +
                state.dataWindowManager->scon[1] * (state.dataWindowManager->thetas[3] - state.dataWindowManager->thetas[2]) -
                state.dataWindowManager->A23 * thetas_2_3_4 + state.dataWindowManager->AbsRadGlassFace[2];
            state.dataWindowManager->fvec[3] =
                state.dataWindowManager->scon[1] * (state.dataWindowManager->thetas[2] - state.dataWindowManager->thetas[3]) +
                hgap(2) * (state.dataWindowManager->thetas[4] - state.dataWindowManager->thetas[3]) + state.dataWindowManager->A45 * thetas_4_5_4 +
                state.dataWindowManager->AbsRadGlassFace[3];
            state.dataWindowManager->fvec[4] =
                hgap(2) * (state.dataWindowManager->thetas[3] - state.dataWindowManager->thetas[4]) +
                state.dataWindowManager->scon[2] * (state.dataWindowManager->thetas[5] - state.dataWindowManager->thetas[4]) -
                state.dataWindowManager->A45 * thetas_4_5_4 + state.dataWindowManager->AbsRadGlassFace[4];
            state.dataWindowManager->fvec[5] =
                state.dataWindowManager->scon[2] * (state.dataWindowManager->thetas[4] - state.dataWindowManager->thetas[5]) +
                hgap(3) * (state.dataWindowManager->thetas[6] - state.dataWindowManager->thetas[5]) + state.dataWindowManager->A67 * thetas_6_7_4 +
                state.dataWindowManager->AbsRadGlassFace[5];
            state.dataWindowManager->fvec[6] =
                hgap(3) * (state.dataWindowManager->thetas[5] - state.dataWindowManager->thetas[6]) +
                state.dataWindowManager->scon[3] * (state.dataWindowManager->thetas[7] - state.dataWindowManager->thetas[6]) -
                state.dataWindowManager->A67 * thetas_6_7_4 + state.dataWindowManager->AbsRadGlassFace[6];
            state.dataWindowManager->fvec[7] =
                state.dataWindowManager->Rmir * state.dataWindowManager->emis[7] -
                state.dataWindowManager->emis[7] * state.dataWindowManager->sigma * pow_4(state.dataWindowManager->thetas[7]) +
                state.dataWindowManager->scon[3] * (state.dataWindowManager->thetas[6] - state.dataWindowManager->thetas[7]) +
                state.dataWindowManager->hcin * (state.dataWindowManager->tin - state.dataWindowManager->thetas[7]) +
                state.dataWindowManager->AbsRadGlassFace[7];
            break;
        }
    }

    //****************************************************************************

    void GetHeatBalanceEqCoefMatrixSimple(EnergyPlusData &state,
                                          int const nglasslayer,     // Number of glass layers
                                          Array1D<Real64> const &hr, // Radiative conductance (W/m2-K)
                                          Array1A<Real64> &hgap,     // Gap gas conductive conductance (W/m2-K)
                                          Array2D<Real64> &Aface,    // Coefficient in equation Aface*thetas = Bface
                                          Array1D<Real64> &Bface     // Coefficient in equation Aface*thetas = Bface
    )
    {
        Real64 gr;  // Grashof number of gas in a gap
        Real64 con; // Gap gas conductivity
        Real64 pr;  // Gap gas Prandtl number
        Real64 nu;  // Gap gas Nusselt number

        if (nglasslayer == 1) {
            Bface(1) = state.dataWindowManager->Outir * state.dataWindowManager->emis[0] +
                       state.dataWindowManager->hcout * state.dataWindowManager->tout + state.dataWindowManager->AbsRadGlassFace[0];
            Bface(2) = state.dataWindowManager->Rmir * state.dataWindowManager->emis[1] +
                       state.dataWindowManager->hcin * state.dataWindowManager->tin + state.dataWindowManager->AbsRadGlassFace[1];

            Aface(1, 1) = hr(1) + state.dataWindowManager->scon[0] + state.dataWindowManager->hcout;
            Aface(2, 1) = -state.dataWindowManager->scon[0];
            Aface(1, 2) = -state.dataWindowManager->scon[0];
            Aface(2, 2) = hr(2) + state.dataWindowManager->scon[0] + state.dataWindowManager->hcin;

        } else if (nglasslayer == 2) {
            WindowGasConductance(state, state.dataWindowManager->thetas[1], state.dataWindowManager->thetas[2], 1, con, pr, gr);
            NusseltNumber(state, 0, state.dataWindowManager->thetas[1], state.dataWindowManager->thetas[2], 1, gr, pr, nu);
            hgap(1) = con / state.dataWindowManager->gap[0] * nu;

            Bface(1) = state.dataWindowManager->Outir * state.dataWindowManager->emis[0] +
                       state.dataWindowManager->hcout * state.dataWindowManager->tout + state.dataWindowManager->AbsRadGlassFace[0];
            Bface(2) = state.dataWindowManager->AbsRadGlassFace[1];
            Bface(3) = state.dataWindowManager->AbsRadGlassFace[2];
            Bface(4) = state.dataWindowManager->Rmir * state.dataWindowManager->emis[3] +
                       state.dataWindowManager->hcin * state.dataWindowManager->tin + state.dataWindowManager->AbsRadGlassFace[3];

            Aface(1, 1) = hr(1) + state.dataWindowManager->scon[0] + state.dataWindowManager->hcout;
            Aface(2, 1) = -state.dataWindowManager->scon[0];

            Aface(1, 2) = -state.dataWindowManager->scon[0];
            Aface(2, 2) = state.dataWindowManager->scon[0] + hgap(1) - state.dataWindowManager->A23P * hr(2);
            Aface(3, 2) = -hgap(1) - state.dataWindowManager->A32P * hr(3);

            Aface(2, 3) = -hgap(1) + state.dataWindowManager->A23P * hr(2);
            Aface(3, 3) = hgap(1) + state.dataWindowManager->scon[1] + state.dataWindowManager->A32P * hr(3);
            Aface(4, 3) = -state.dataWindowManager->scon[1];

            Aface(3, 4) = -state.dataWindowManager->scon[1];
            Aface(4, 4) = hr(4) + state.dataWindowManager->scon[1] + state.dataWindowManager->hcin;

        } else if (nglasslayer == 3) {
            WindowGasConductance(state, state.dataWindowManager->thetas[1], state.dataWindowManager->thetas[2], 1, con, pr, gr);
            NusseltNumber(state, 0, state.dataWindowManager->thetas[1], state.dataWindowManager->thetas[2], 1, gr, pr, nu);
            hgap(1) = con / state.dataWindowManager->gap[0] * nu;

            WindowGasConductance(state, state.dataWindowManager->thetas[3], state.dataWindowManager->thetas[4], 2, con, pr, gr);
            NusseltNumber(state, 0, state.dataWindowManager->thetas[3], state.dataWindowManager->thetas[4], 2, gr, pr, nu);
            hgap(2) = con / state.dataWindowManager->gap[1] * nu;

            Bface(1) = state.dataWindowManager->Outir * state.dataWindowManager->emis[0] +
                       state.dataWindowManager->hcout * state.dataWindowManager->tout + state.dataWindowManager->AbsRadGlassFace[0];
            Bface(2) = state.dataWindowManager->AbsRadGlassFace[1];
            Bface(3) = state.dataWindowManager->AbsRadGlassFace[2];
            Bface(4) = state.dataWindowManager->AbsRadGlassFace[3];
            Bface(5) = state.dataWindowManager->AbsRadGlassFace[4];
            Bface(6) = state.dataWindowManager->Rmir * state.dataWindowManager->emis[5] +
                       state.dataWindowManager->hcin * state.dataWindowManager->tin + state.dataWindowManager->AbsRadGlassFace[5];

            Aface(1, 1) = hr(1) + state.dataWindowManager->scon[0] + state.dataWindowManager->hcout;
            Aface(2, 1) = -state.dataWindowManager->scon[0];

            Aface(1, 2) = -state.dataWindowManager->scon[0];
            Aface(2, 2) = state.dataWindowManager->scon[0] + hgap(1) - state.dataWindowManager->A23P * hr(2);
            Aface(3, 2) = -hgap(1) - state.dataWindowManager->A32P * hr(3);

            Aface(2, 3) = -hgap(1) + state.dataWindowManager->A23P * hr(2);
            Aface(3, 3) = hgap(1) + state.dataWindowManager->scon[1] + state.dataWindowManager->A32P * hr(3);
            Aface(4, 3) = -state.dataWindowManager->scon[1];

            Aface(3, 4) = -state.dataWindowManager->scon[1];
            Aface(4, 4) = state.dataWindowManager->scon[1] + hgap(2) - state.dataWindowManager->A45P * hr(4);
            Aface(5, 4) = -hgap(2) - state.dataWindowManager->A54P * hr(5);

            Aface(4, 5) = -hgap(2) + state.dataWindowManager->A45P * hr(4);
            Aface(5, 5) = hgap(2) + state.dataWindowManager->scon[2] + state.dataWindowManager->A54P * hr(5);
            Aface(6, 5) = -state.dataWindowManager->scon[2];

            Aface(5, 6) = -state.dataWindowManager->scon[2];
            Aface(6, 6) = hr(6) + state.dataWindowManager->scon[2] + state.dataWindowManager->hcin;

        } else if (nglasslayer == 4) {
            WindowGasConductance(state, state.dataWindowManager->thetas[1], state.dataWindowManager->thetas[2], 1, con, pr, gr);
            NusseltNumber(state, 0, state.dataWindowManager->thetas[1], state.dataWindowManager->thetas[2], 1, gr, pr, nu);
            hgap(1) = con / state.dataWindowManager->gap[0] * nu;

            WindowGasConductance(state, state.dataWindowManager->thetas[3], state.dataWindowManager->thetas[4], 2, con, pr, gr);
            NusseltNumber(state, 0, state.dataWindowManager->thetas[3], state.dataWindowManager->thetas[4], 2, gr, pr, nu);
            hgap(2) = con / state.dataWindowManager->gap[1] * nu;

            WindowGasConductance(state, state.dataWindowManager->thetas[5], state.dataWindowManager->thetas[6], 3, con, pr, gr);
            NusseltNumber(state, 0, state.dataWindowManager->thetas[5], state.dataWindowManager->thetas[6], 3, gr, pr, nu);
            hgap(3) = con / state.dataWindowManager->gap[2] * nu;

            Bface(1) = state.dataWindowManager->Outir * state.dataWindowManager->emis[0] +
                       state.dataWindowManager->hcout * state.dataWindowManager->tout + state.dataWindowManager->AbsRadGlassFace[0];
            Bface(2) = state.dataWindowManager->AbsRadGlassFace[1];
            Bface(3) = state.dataWindowManager->AbsRadGlassFace[2];
            Bface(4) = state.dataWindowManager->AbsRadGlassFace[3];
            Bface(5) = state.dataWindowManager->AbsRadGlassFace[4];
            Bface(6) = state.dataWindowManager->AbsRadGlassFace[5];
            Bface(7) = state.dataWindowManager->AbsRadGlassFace[6];
            Bface(8) = state.dataWindowManager->Rmir * state.dataWindowManager->emis[7] +
                       state.dataWindowManager->hcin * state.dataWindowManager->tin + state.dataWindowManager->AbsRadGlassFace[7];

            Aface(1, 1) = hr(1) + state.dataWindowManager->scon[0] + state.dataWindowManager->hcout;
            Aface(2, 1) = -state.dataWindowManager->scon[0];

            Aface(1, 2) = -state.dataWindowManager->scon[0];
            Aface(2, 2) = state.dataWindowManager->scon[0] + hgap(1) - state.dataWindowManager->A23P * hr(2);
            Aface(3, 2) = -hgap(1) - state.dataWindowManager->A32P * hr(3);

            Aface(2, 3) = -hgap(1) + state.dataWindowManager->A23P * hr(2);
            Aface(3, 3) = hgap(1) + state.dataWindowManager->scon[1] + state.dataWindowManager->A32P * hr(3);
            Aface(4, 3) = -state.dataWindowManager->scon[1];

            Aface(3, 4) = -state.dataWindowManager->scon[1];
            Aface(4, 4) = state.dataWindowManager->scon[1] + hgap(2) - state.dataWindowManager->A45P * hr(4);
            Aface(5, 4) = -hgap(2) - state.dataWindowManager->A54P * hr(5);

            Aface(4, 5) = -hgap(2) + state.dataWindowManager->A45P * hr(4);
            Aface(5, 5) = hgap(2) + state.dataWindowManager->scon[2] + state.dataWindowManager->A54P * hr(5);
            Aface(6, 5) = -state.dataWindowManager->scon[2];

            Aface(5, 6) = -state.dataWindowManager->scon[2];
            Aface(6, 6) = state.dataWindowManager->scon[2] + hgap(3) - state.dataWindowManager->A67P * hr(6);
            Aface(7, 6) = -hgap(3) - state.dataWindowManager->A76P * hr(7);

            Aface(6, 7) = -hgap(3) + state.dataWindowManager->A67P * hr(6);
            Aface(7, 7) = hgap(3) + state.dataWindowManager->scon[3] + state.dataWindowManager->A76P * hr(7);
            Aface(8, 7) = -state.dataWindowManager->scon[3];

            Aface(7, 8) = -state.dataWindowManager->scon[3];
            Aface(8, 8) = hr(8) + state.dataWindowManager->scon[3] + state.dataWindowManager->hcin;
        }
    }

    void GetHeatBalanceEqCoefMatrix(EnergyPlusData &state,
                                    int const SurfNum,
                                    int const nglasslayer,
                                    WinShadingType const ShadeFlag,
                                    Real64 const sconsh,
                                    Real64 const TauShIR,
                                    Real64 const EpsShIR1,
                                    Real64 const EpsShIR2,
                                    Real64 const RhoShIR1,
                                    Real64 const RhoShIR2,
                                    Real64 const ShGlReflFacIR,
                                    Real64 const RhoGlIR1,
                                    Real64 const RhoGlIR2,
                                    Real64 const hcv,             // Convection coefficient from gap glass or shade/blind to gap air (W/m2-K)
                                    Real64 const TGapNew,         // Current-iteration average air temp in airflow gap (K)
                                    Real64 const TAirflowGapNew,  // Average air temp in airflow gap between glass panes (K)
                                    Real64 const hcvAirflowGap,   // Convection coefficient from airflow gap glass to airflow gap air (W/m2-K)
                                    Array1A<Real64> const &hcvBG, // Convection coefficient from gap glass or shade to gap gas (W/m2-K)
                                    Array1A<Real64> const &TGapNewBG,
                                    Array1A<Real64> const &AbsRadShadeFace,
                                    Array1D<Real64> const &hr,
                                    Array2D<Real64> &Aface,
                                    Array1D<Real64> &Bface)
    {
        auto &hgap = state.dataWindowManager->hgap;
        auto &RhoIR = state.dataWindowManager->RhoIR;

        Real64 gr;  // Grashof number of gas in a gap
        Real64 con; // Gap gas conductivity
        Real64 pr;  // Gap gas Prandtl number
        Real64 nu;  // Gap gas Nusselt number

        Real64 FacRhoIR25;         // Intermediate variable
        Real64 FacRhoIR63;         // Intermediate variable
        Real64 RhoIRfp;            // Intermediate variable
        Real64 RhoIRbp;            // Intermediate variable
        Real64 FacRhoIR2fp;        // Intermediate variable
        Real64 FacRhoIR3bp;        // Intermediate variable
        Real64 FacRhoIR2fpRhoIR63; // Intermediate variable
        Real64 FacRhoIR3bpRhoIR25; // Intermediate variable
        Real64 FacRhoIR47;         // Intermediate variable
        Real64 FacRhoIR85;         // Intermediate variable
        Real64 FacRhoIR4fp;        // Intermediate variable
        Real64 FacRhoIR5bp;        // Intermediate variable
        Real64 FacRhoIR4fpRhoIR85; // Intermediate variable
        Real64 FacRhoIR5bpRhoIR47; // Intermediate variable

        int i; // Counter

        if (nglasslayer == 1) {
            Bface(1) = state.dataWindowManager->Outir * state.dataWindowManager->emis[0] +
                       state.dataWindowManager->hcout * state.dataWindowManager->tout + state.dataWindowManager->AbsRadGlassFace[0];
            Bface(2) = state.dataWindowManager->Rmir * state.dataWindowManager->emis[1] +
                       state.dataWindowManager->hcin * state.dataWindowManager->tin + state.dataWindowManager->AbsRadGlassFace[1];

            Aface(1, 1) = hr(1) + state.dataWindowManager->scon[0] + state.dataWindowManager->hcout;
            Aface(2, 1) = -state.dataWindowManager->scon[0];
            Aface(1, 2) = -state.dataWindowManager->scon[0];
            Aface(2, 2) = hr(2) + state.dataWindowManager->scon[0] + state.dataWindowManager->hcin;

            if (ANY_INTERIOR_SHADE_BLIND(ShadeFlag)) {
                // interior shade, single pane
                //            ||   ||
                //  outside  1||2 3||4
                //            ||   ||
                //            gl  bl/sh/sc
                Bface(2) = state.dataWindowManager->Rmir * state.dataWindowManager->emis[1] * TauShIR / ShGlReflFacIR + hcv * TGapNew +
                           state.dataWindowManager->AbsRadGlassFace[1];
                Bface(3) = state.dataWindowManager->Rmir * TauShIR * RhoGlIR2 * EpsShIR1 / ShGlReflFacIR + hcv * TGapNew + AbsRadShadeFace(1);
                Bface(4) =
                    state.dataWindowManager->Rmir * EpsShIR2 + state.dataWindowManager->hcin * state.dataWindowManager->tin + AbsRadShadeFace(2);

                Aface(2, 2) = hr(2) * (1 - RhoShIR1) / ShGlReflFacIR + state.dataWindowManager->scon[0] + hcv;
                Aface(3, 2) = -state.dataWindowManager->emis[1] * hr(3) / ShGlReflFacIR;
                Aface(2, 3) = -hr(2) * EpsShIR1 / ShGlReflFacIR;
                Aface(3, 3) = hr(3) * (1 - RhoGlIR2 * (EpsShIR1 + RhoShIR1)) / ShGlReflFacIR + sconsh + hcv;
                Aface(4, 3) = -sconsh;
                Aface(3, 4) = -sconsh;
                Aface(4, 4) = hr(4) + sconsh + state.dataWindowManager->hcin;
            }

            if (ANY_EXTERIOR_SHADE_BLIND_SCREEN(ShadeFlag)) {
                // exterior shade, single pane
                //           ||      ||
                //  outside 3||4    1||2
                //           ||      ||
                //        bl/sh/sc   gl
                Bface(1) = state.dataWindowManager->Outir * state.dataWindowManager->emis[0] * TauShIR / ShGlReflFacIR + hcv * TGapNew +
                           state.dataWindowManager->AbsRadGlassFace[0];
                Bface(3) =
                    state.dataWindowManager->Outir * EpsShIR1 + state.dataWindowManager->hcout * state.dataWindowManager->tout + AbsRadShadeFace(1);
                Bface(4) = state.dataWindowManager->Outir * TauShIR * RhoGlIR1 * EpsShIR2 / ShGlReflFacIR + hcv * TGapNew + AbsRadShadeFace(2);

                Aface(1, 1) = hr(1) * (1 - RhoShIR2) / ShGlReflFacIR + state.dataWindowManager->scon[0] + hcv;
                Aface(4, 1) = -state.dataWindowManager->emis[0] * hr(4) / ShGlReflFacIR;
                Aface(3, 3) = hr(3) + sconsh + state.dataWindowManager->hcout;
                Aface(4, 3) = -sconsh;
                Aface(1, 4) = -hr(1) * EpsShIR2 / ShGlReflFacIR;
                Aface(3, 4) = -sconsh;
                Aface(4, 4) = hr(4) * (1 - RhoGlIR1 * (EpsShIR2 + RhoShIR2)) / ShGlReflFacIR + sconsh + hcv;
            }

        } else if (nglasslayer == 2) {
            WindowGasConductance(state, state.dataWindowManager->thetas[1], state.dataWindowManager->thetas[2], 1, con, pr, gr);
            NusseltNumber(state, SurfNum, state.dataWindowManager->thetas[1], state.dataWindowManager->thetas[2], 1, gr, pr, nu);
            hgap(1) = con / state.dataWindowManager->gap[0] * nu;
            if (state.dataSurface->SurfWinEdgeGlCorrFac(SurfNum) > 1.0) { // Edge of glass correction
                state.dataWindowManager->hrgap[0] =
                    0.5 * std::abs(state.dataWindowManager->A23) * pow_3(state.dataWindowManager->thetas[1] + state.dataWindowManager->thetas[2]);
                hgap(1) = hgap(1) * state.dataSurface->SurfWinEdgeGlCorrFac(SurfNum) +
                          state.dataWindowManager->hrgap[0] * (state.dataSurface->SurfWinEdgeGlCorrFac(SurfNum) - 1.0);
            }

            Bface(1) = state.dataWindowManager->Outir * state.dataWindowManager->emis[0] +
                       state.dataWindowManager->hcout * state.dataWindowManager->tout + state.dataWindowManager->AbsRadGlassFace[0];
            Bface(2) = state.dataWindowManager->AbsRadGlassFace[1];
            Bface(3) = state.dataWindowManager->AbsRadGlassFace[2];
            Bface(4) = state.dataWindowManager->Rmir * state.dataWindowManager->emis[3] +
                       state.dataWindowManager->hcin * state.dataWindowManager->tin + state.dataWindowManager->AbsRadGlassFace[3];

            Aface(1, 1) = hr(1) + state.dataWindowManager->scon[0] + state.dataWindowManager->hcout;
            Aface(2, 1) = -state.dataWindowManager->scon[0];

            Aface(1, 2) = -state.dataWindowManager->scon[0];
            Aface(2, 2) = state.dataWindowManager->scon[0] + hgap(1) - state.dataWindowManager->A23P * hr(2);
            Aface(3, 2) = -hgap(1) - state.dataWindowManager->A32P * hr(3);

            Aface(2, 3) = -hgap(1) + state.dataWindowManager->A23P * hr(2);
            Aface(3, 3) = hgap(1) + state.dataWindowManager->scon[1] + state.dataWindowManager->A32P * hr(3);
            Aface(4, 3) = -state.dataWindowManager->scon[1];

            Aface(3, 4) = -state.dataWindowManager->scon[1];
            Aface(4, 4) = hr(4) + state.dataWindowManager->scon[1] + state.dataWindowManager->hcin;

            if (!ANY_BETWEENGLASS_SHADE_BLIND(ShadeFlag) && state.dataSurface->SurfWinAirflowThisTS(SurfNum) > 0.0) {
                Bface(2) = state.dataWindowManager->AbsRadGlassFace[1] + hcvAirflowGap * TAirflowGapNew;
                Bface(3) = state.dataWindowManager->AbsRadGlassFace[2] + hcvAirflowGap * TAirflowGapNew;
                Aface(2, 2) = state.dataWindowManager->scon[0] + hcvAirflowGap - state.dataWindowManager->A23P * hr(2);
                Aface(3, 2) = -state.dataWindowManager->A32P * hr(3);
                Aface(2, 3) = state.dataWindowManager->A23P * hr(2);
                Aface(3, 3) = hcvAirflowGap + state.dataWindowManager->scon[1] + state.dataWindowManager->A32P * hr(3);
            }

            if (ANY_INTERIOR_SHADE_BLIND(ShadeFlag)) {
                Bface(4) = state.dataWindowManager->Rmir * state.dataWindowManager->emis[3] * TauShIR / ShGlReflFacIR + hcv * TGapNew +
                           state.dataWindowManager->AbsRadGlassFace[3];
                Bface(5) = state.dataWindowManager->Rmir * TauShIR * RhoGlIR2 * EpsShIR1 / ShGlReflFacIR + hcv * TGapNew + AbsRadShadeFace(1);
                Bface(6) =
                    state.dataWindowManager->Rmir * EpsShIR2 + state.dataWindowManager->hcin * state.dataWindowManager->tin + AbsRadShadeFace(2);

                Aface(4, 4) = hr(4) * (1 - RhoShIR1) / ShGlReflFacIR + state.dataWindowManager->scon[1] + hcv;
                Aface(5, 4) = -state.dataWindowManager->emis[3] * hr(5) / ShGlReflFacIR;
                Aface(4, 5) = -hr(4) * EpsShIR1 / ShGlReflFacIR;
                Aface(5, 5) = hr(5) * (1 - RhoGlIR2 * (EpsShIR1 + RhoShIR1)) / ShGlReflFacIR + sconsh + hcv;
                Aface(6, 5) = -sconsh;
                Aface(5, 6) = -sconsh;
                Aface(6, 6) = hr(6) + sconsh + state.dataWindowManager->hcin;
            }

            if (ANY_EXTERIOR_SHADE_BLIND_SCREEN(ShadeFlag)) {
                Bface(1) = state.dataWindowManager->Outir * state.dataWindowManager->emis[0] * TauShIR / ShGlReflFacIR + hcv * TGapNew +
                           state.dataWindowManager->AbsRadGlassFace[0];
                Bface(5) =
                    state.dataWindowManager->Outir * EpsShIR1 + state.dataWindowManager->hcout * state.dataWindowManager->tout + AbsRadShadeFace(1);
                Bface(6) = state.dataWindowManager->Outir * TauShIR * RhoGlIR1 * EpsShIR2 / ShGlReflFacIR + hcv * TGapNew + AbsRadShadeFace(2);

                Aface(1, 1) = hr(1) * (1 - RhoShIR2) / ShGlReflFacIR + state.dataWindowManager->scon[0] + hcv;
                Aface(6, 1) = -state.dataWindowManager->emis[0] * hr(6) / ShGlReflFacIR;
                Aface(5, 5) = hr(5) + sconsh + state.dataWindowManager->hcout;
                Aface(6, 5) = -sconsh;
                Aface(1, 6) = -hr(1) * EpsShIR2 / ShGlReflFacIR;
                Aface(5, 6) = -sconsh;
                Aface(6, 6) = hr(6) * (1 - RhoGlIR1 * (EpsShIR2 + RhoShIR2)) / ShGlReflFacIR + sconsh + hcv;
            }

            if (ANY_BETWEENGLASS_SHADE_BLIND(ShadeFlag)) {
                for (i = 1; i <= 6; ++i) {
                    RhoIR(i) = max(0.0, 1.0 - state.dataWindowManager->tir[i - 1] - state.dataWindowManager->emis[i - 1]);
                }
                FacRhoIR25 = 1.0 - RhoIR(2) * RhoIR(5);
                FacRhoIR63 = 1.0 - RhoIR(6) * RhoIR(3);
                Real64 const tir_5_squared(pow_2(state.dataWindowManager->tir[4]));
                RhoIRfp = RhoIR(5) + tir_5_squared * RhoIR(3) / FacRhoIR63;
                RhoIRbp = RhoIR(6) + tir_5_squared * RhoIR(2) / FacRhoIR25;
                FacRhoIR2fp = 1.0 - RhoIRfp * RhoIR(2);
                FacRhoIR3bp = 1.0 - RhoIRbp * RhoIR(3);
                FacRhoIR2fpRhoIR63 = FacRhoIR2fp * FacRhoIR63;
                FacRhoIR3bpRhoIR25 = FacRhoIR3bp * FacRhoIR25;
                Aface(2, 2) =
                    state.dataWindowManager->scon[0] + hcvBG(1) + hr(2) * (1 - RhoIRfp * (state.dataWindowManager->emis[1] + RhoIR(2))) / FacRhoIR2fp;
                Aface(3, 2) = -state.dataWindowManager->emis[1] * hr(3) * state.dataWindowManager->tir[4] / FacRhoIR2fpRhoIR63;
                Aface(5, 2) = -state.dataWindowManager->emis[1] * hr(5) / FacRhoIR2fp;
                Aface(6, 2) = -state.dataWindowManager->emis[1] * hr(6) * RhoIR(3) * state.dataWindowManager->tir[4] / FacRhoIR2fpRhoIR63;
                Bface(2) = hcvBG(1) * TGapNewBG(1) + state.dataWindowManager->AbsRadGlassFace[1];
                Aface(2, 3) = -state.dataWindowManager->emis[2] * hr(2) * state.dataWindowManager->tir[4] / FacRhoIR3bpRhoIR25;
                Aface(3, 3) =
                    state.dataWindowManager->scon[1] + hcvBG(2) + hr(3) * (1 - RhoIRbp * (state.dataWindowManager->emis[2] + RhoIR(3))) / FacRhoIR3bp;
                Aface(5, 3) = -state.dataWindowManager->emis[2] * hr(5) * RhoIR(2) * state.dataWindowManager->tir[4] / FacRhoIR3bpRhoIR25;
                Aface(6, 3) = -state.dataWindowManager->emis[2] * hr(6) / FacRhoIR3bp;
                Bface(3) = hcvBG(2) * TGapNewBG(2) + state.dataWindowManager->AbsRadGlassFace[2];
                Aface(2, 5) = -state.dataWindowManager->emis[4] * hr(2) / FacRhoIR2fp;
                Aface(3, 5) = -hr(3) * state.dataWindowManager->tir[4] * RhoIR(2) * state.dataWindowManager->emis[4] / FacRhoIR2fpRhoIR63;
                Aface(5, 5) = sconsh + hcvBG(1) + hr(5) * (1 - RhoIR(2) * state.dataWindowManager->emis[4] / FacRhoIR2fp);
                Aface(6, 5) =
                    -sconsh - hr(6) * RhoIR(2) * state.dataWindowManager->tir[4] * RhoIR(3) * state.dataWindowManager->emis[4] / FacRhoIR2fpRhoIR63;
                Bface(5) = hcvBG(1) * TGapNewBG(1) + AbsRadShadeFace(1);
                Aface(2, 6) = -hr(2) * state.dataWindowManager->tir[4] * RhoIR(3) * state.dataWindowManager->emis[5] / FacRhoIR3bpRhoIR25;
                Aface(3, 6) = -state.dataWindowManager->emis[5] * hr(3) / FacRhoIR3bp;
                Aface(5, 6) =
                    -sconsh - hr(5) * RhoIR(3) * state.dataWindowManager->tir[4] * RhoIR(2) * state.dataWindowManager->emis[5] / FacRhoIR3bpRhoIR25;
                Aface(6, 6) = sconsh + hcvBG(2) + hr(6) * (1 - RhoIR(3) * state.dataWindowManager->emis[5] / FacRhoIR3bp);
                Bface(6) = hcvBG(2) * TGapNewBG(2) + AbsRadShadeFace(2);
            }

        } else if (nglasslayer == 3) {
            WindowGasConductance(state, state.dataWindowManager->thetas[1], state.dataWindowManager->thetas[2], 1, con, pr, gr);
            NusseltNumber(state, SurfNum, state.dataWindowManager->thetas[1], state.dataWindowManager->thetas[2], 1, gr, pr, nu);
            hgap(1) = con / state.dataWindowManager->gap[0] * nu;
            if (state.dataSurface->SurfWinEdgeGlCorrFac(SurfNum) > 1.0) { // Edge of glass correction
                state.dataWindowManager->hrgap[0] =
                    0.5 * std::abs(state.dataWindowManager->A23) * pow_3(state.dataWindowManager->thetas[1] + state.dataWindowManager->thetas[2]);
                hgap(1) = hgap(1) * state.dataSurface->SurfWinEdgeGlCorrFac(SurfNum) +
                          state.dataWindowManager->hrgap[0] * (state.dataSurface->SurfWinEdgeGlCorrFac(SurfNum) - 1.0);
            }

            WindowGasConductance(state, state.dataWindowManager->thetas[3], state.dataWindowManager->thetas[4], 2, con, pr, gr);
            NusseltNumber(state, SurfNum, state.dataWindowManager->thetas[3], state.dataWindowManager->thetas[4], 2, gr, pr, nu);
            hgap(2) = con / state.dataWindowManager->gap[1] * nu;
            if (state.dataSurface->SurfWinEdgeGlCorrFac(SurfNum) > 1.0) { // Edge of glass correction
                state.dataWindowManager->hrgap[1] =
                    0.5 * std::abs(state.dataWindowManager->A45) * pow_3(state.dataWindowManager->thetas[3] + state.dataWindowManager->thetas[4]);
                hgap(2) = hgap(2) * state.dataSurface->SurfWinEdgeGlCorrFac(SurfNum) +
                          state.dataWindowManager->hrgap[1] * (state.dataSurface->SurfWinEdgeGlCorrFac(SurfNum) - 1.0);
            }

            Bface(1) = state.dataWindowManager->Outir * state.dataWindowManager->emis[0] +
                       state.dataWindowManager->hcout * state.dataWindowManager->tout + state.dataWindowManager->AbsRadGlassFace[0];
            Bface(2) = state.dataWindowManager->AbsRadGlassFace[1];
            Bface(3) = state.dataWindowManager->AbsRadGlassFace[2];
            Bface(4) = state.dataWindowManager->AbsRadGlassFace[3];
            Bface(5) = state.dataWindowManager->AbsRadGlassFace[4];
            Bface(6) = state.dataWindowManager->Rmir * state.dataWindowManager->emis[5] +
                       state.dataWindowManager->hcin * state.dataWindowManager->tin + state.dataWindowManager->AbsRadGlassFace[5];

            Aface(1, 1) = hr(1) + state.dataWindowManager->scon[0] + state.dataWindowManager->hcout;
            Aface(2, 1) = -state.dataWindowManager->scon[0];

            Aface(1, 2) = -state.dataWindowManager->scon[0];
            Aface(2, 2) = state.dataWindowManager->scon[0] + hgap(1) - state.dataWindowManager->A23P * hr(2);
            Aface(3, 2) = -hgap(1) - state.dataWindowManager->A32P * hr(3);

            Aface(2, 3) = -hgap(1) + state.dataWindowManager->A23P * hr(2);
            Aface(3, 3) = hgap(1) + state.dataWindowManager->scon[1] + state.dataWindowManager->A32P * hr(3);
            Aface(4, 3) = -state.dataWindowManager->scon[1];

            Aface(3, 4) = -state.dataWindowManager->scon[1];
            Aface(4, 4) = state.dataWindowManager->scon[1] + hgap(2) - state.dataWindowManager->A45P * hr(4);
            Aface(5, 4) = -hgap(2) - state.dataWindowManager->A54P * hr(5);

            Aface(4, 5) = -hgap(2) + state.dataWindowManager->A45P * hr(4);
            Aface(5, 5) = hgap(2) + state.dataWindowManager->scon[2] + state.dataWindowManager->A54P * hr(5);
            Aface(6, 5) = -state.dataWindowManager->scon[2];

            Aface(5, 6) = -state.dataWindowManager->scon[2];
            Aface(6, 6) = hr(6) + state.dataWindowManager->scon[2] + state.dataWindowManager->hcin;

            if (!ANY_BETWEENGLASS_SHADE_BLIND(ShadeFlag) && state.dataSurface->SurfWinAirflowThisTS(SurfNum) > 0.0) {
                Bface(4) = state.dataWindowManager->AbsRadGlassFace[3] + hcvAirflowGap * TAirflowGapNew;
                Bface(5) = state.dataWindowManager->AbsRadGlassFace[4] + hcvAirflowGap * TAirflowGapNew;
                Aface(4, 4) = state.dataWindowManager->scon[1] + hcvAirflowGap - state.dataWindowManager->A45P * hr(4);
                Aface(5, 4) = -state.dataWindowManager->A54P * hr(5);
                Aface(4, 5) = state.dataWindowManager->A45P * hr(4);
                Aface(5, 5) = hcvAirflowGap + state.dataWindowManager->scon[2] + state.dataWindowManager->A54P * hr(5);
            }

            if (ANY_INTERIOR_SHADE_BLIND(ShadeFlag)) {
                Bface(6) = state.dataWindowManager->Rmir * state.dataWindowManager->emis[5] * TauShIR / ShGlReflFacIR + hcv * TGapNew +
                           state.dataWindowManager->AbsRadGlassFace[5];
                Bface(7) = state.dataWindowManager->Rmir * TauShIR * RhoGlIR2 * EpsShIR1 / ShGlReflFacIR + hcv * TGapNew + AbsRadShadeFace(1);
                Bface(8) =
                    state.dataWindowManager->Rmir * EpsShIR2 + state.dataWindowManager->hcin * state.dataWindowManager->tin + AbsRadShadeFace(2);

                Aface(6, 6) = hr(6) * (1 - RhoShIR1) / ShGlReflFacIR + state.dataWindowManager->scon[2] + hcv;
                Aface(7, 6) = -state.dataWindowManager->emis[5] * hr(7) / ShGlReflFacIR;
                Aface(6, 7) = -hr(6) * EpsShIR1 / ShGlReflFacIR;
                Aface(7, 7) = hr(7) * (1 - RhoGlIR2 * (EpsShIR1 + RhoShIR1)) / ShGlReflFacIR + sconsh + hcv;
                Aface(8, 7) = -sconsh;
                Aface(7, 8) = -sconsh;
                Aface(8, 8) = hr(8) + sconsh + state.dataWindowManager->hcin;
            } else if (ANY_EXTERIOR_SHADE_BLIND_SCREEN(ShadeFlag)) {
                Bface(1) = state.dataWindowManager->Outir * state.dataWindowManager->emis[0] * TauShIR / ShGlReflFacIR + hcv * TGapNew +
                           state.dataWindowManager->AbsRadGlassFace[0];
                Bface(7) =
                    state.dataWindowManager->Outir * EpsShIR1 + state.dataWindowManager->hcout * state.dataWindowManager->tout + AbsRadShadeFace(1);
                Bface(8) = state.dataWindowManager->Outir * TauShIR * RhoGlIR1 * EpsShIR2 / ShGlReflFacIR + hcv * TGapNew + AbsRadShadeFace(2);

                Aface(1, 1) = hr(1) * (1 - RhoShIR2) / ShGlReflFacIR + state.dataWindowManager->scon[0] + hcv;
                Aface(8, 1) = -state.dataWindowManager->emis[0] * hr(8) / ShGlReflFacIR;
                Aface(7, 7) = hr(7) + sconsh + state.dataWindowManager->hcout;
                Aface(8, 7) = -sconsh;
                Aface(1, 8) = -hr(1) * EpsShIR2 / ShGlReflFacIR;
                Aface(7, 8) = -sconsh;
                Aface(8, 8) = hr(8) * (1 - RhoGlIR1 * (EpsShIR2 + RhoShIR2)) / ShGlReflFacIR + sconsh + hcv;
            } else if (ANY_BETWEENGLASS_SHADE_BLIND(ShadeFlag)) {
                for (i = 1; i <= 8; ++i) {
                    RhoIR(i) = max(0.0, 1.0 - state.dataWindowManager->tir[i - 1] - state.dataWindowManager->emis[i - 1]);
                }
                FacRhoIR47 = 1 - RhoIR(4) * RhoIR(7);
                FacRhoIR85 = 1 - RhoIR(8) * RhoIR(5);
                Real64 const tir_7_squared(pow_2(state.dataWindowManager->tir[6]));
                RhoIRfp = RhoIR(7) + tir_7_squared * RhoIR(5) / FacRhoIR85;
                RhoIRbp = RhoIR(8) + tir_7_squared * RhoIR(4) / FacRhoIR47;
                FacRhoIR4fp = 1 - RhoIRfp * RhoIR(4);
                FacRhoIR5bp = 1 - RhoIRbp * RhoIR(5);
                FacRhoIR4fpRhoIR85 = FacRhoIR4fp * FacRhoIR85;
                FacRhoIR5bpRhoIR47 = FacRhoIR5bp * FacRhoIR47;
                Aface(4, 4) =
                    state.dataWindowManager->scon[1] + hcvBG(1) + hr(4) * (1 - RhoIRfp * (state.dataWindowManager->emis[3] + RhoIR(4))) / FacRhoIR4fp;
                Aface(5, 4) = -state.dataWindowManager->emis[3] * hr(5) * state.dataWindowManager->tir[6] / FacRhoIR4fpRhoIR85;
                Aface(7, 4) = -state.dataWindowManager->emis[3] * hr(7) / FacRhoIR4fp;
                Aface(8, 4) = -state.dataWindowManager->emis[3] * hr(8) * RhoIR(5) * state.dataWindowManager->tir[6] / FacRhoIR4fpRhoIR85;
                Bface(4) = hcvBG(1) * TGapNewBG(1) + state.dataWindowManager->AbsRadGlassFace[3];
                Aface(4, 5) = -state.dataWindowManager->emis[4] * hr(4) * state.dataWindowManager->tir[6] / FacRhoIR5bpRhoIR47;
                Aface(5, 5) =
                    state.dataWindowManager->scon[2] + hcvBG(2) + hr(5) * (1 - RhoIRbp * (state.dataWindowManager->emis[4] + RhoIR(5))) / FacRhoIR5bp;
                Aface(7, 5) = -state.dataWindowManager->emis[4] * hr(7) * RhoIR(4) * state.dataWindowManager->tir[6] / FacRhoIR5bpRhoIR47;
                Aface(8, 5) = -state.dataWindowManager->emis[4] * hr(8) / FacRhoIR5bp;
                Bface(5) = hcvBG(2) * TGapNewBG(2) + state.dataWindowManager->AbsRadGlassFace[4];
                Aface(4, 7) = -state.dataWindowManager->emis[6] * hr(4) / FacRhoIR4fp;
                Aface(5, 7) = -hr(5) * state.dataWindowManager->tir[6] * RhoIR(4) * state.dataWindowManager->emis[6] / FacRhoIR4fpRhoIR85;
                Aface(7, 7) = sconsh + hcvBG(1) + hr(7) * (1 - RhoIR(4) * state.dataWindowManager->emis[6] / FacRhoIR4fp);
                Aface(8, 7) =
                    -sconsh - hr(8) * RhoIR(4) * state.dataWindowManager->tir[6] * RhoIR(5) * state.dataWindowManager->emis[6] / FacRhoIR4fpRhoIR85;
                Bface(7) = hcvBG(1) * TGapNewBG(1) + AbsRadShadeFace(1);
                Aface(4, 8) = -hr(4) * state.dataWindowManager->tir[6] * RhoIR(5) * state.dataWindowManager->emis[7] / FacRhoIR5bpRhoIR47;
                Aface(5, 8) = -state.dataWindowManager->emis[7] * hr(5) / FacRhoIR5bp;
                Aface(7, 8) =
                    -sconsh - hr(7) * RhoIR(5) * state.dataWindowManager->tir[6] * RhoIR(4) * state.dataWindowManager->emis[7] / FacRhoIR5bpRhoIR47;
                Aface(8, 8) = sconsh + hcvBG(2) + hr(8) * (1 - RhoIR(5) * state.dataWindowManager->emis[7] / FacRhoIR5bp);
                Bface(8) = hcvBG(2) * TGapNewBG(2) + AbsRadShadeFace(2);
            }

        } else if (nglasslayer == 4) {
            WindowGasConductance(state, state.dataWindowManager->thetas[1], state.dataWindowManager->thetas[2], 1, con, pr, gr);
            NusseltNumber(state, SurfNum, state.dataWindowManager->thetas[1], state.dataWindowManager->thetas[2], 1, gr, pr, nu);
            hgap(1) = con / state.dataWindowManager->gap[0] * nu;
            if (state.dataSurface->SurfWinEdgeGlCorrFac(SurfNum) > 1.0) { // Edge of glass correction
                state.dataWindowManager->hrgap[0] =
                    0.5 * std::abs(state.dataWindowManager->A23) * pow_3(state.dataWindowManager->thetas[1] + state.dataWindowManager->thetas[2]);
                hgap(1) = hgap(1) * state.dataSurface->SurfWinEdgeGlCorrFac(SurfNum) +
                          state.dataWindowManager->hrgap[0] * (state.dataSurface->SurfWinEdgeGlCorrFac(SurfNum) - 1.0);
            }

            WindowGasConductance(state, state.dataWindowManager->thetas[3], state.dataWindowManager->thetas[4], 2, con, pr, gr);
            NusseltNumber(state, SurfNum, state.dataWindowManager->thetas[3], state.dataWindowManager->thetas[4], 2, gr, pr, nu);
            hgap(2) = con / state.dataWindowManager->gap[1] * nu;
            if (state.dataSurface->SurfWinEdgeGlCorrFac(SurfNum) > 1.0) { // Edge of glass correction
                state.dataWindowManager->hrgap[1] =
                    0.5 * std::abs(state.dataWindowManager->A45) * pow_3(state.dataWindowManager->thetas[3] + state.dataWindowManager->thetas[4]);
                hgap(2) = hgap(2) * state.dataSurface->SurfWinEdgeGlCorrFac(SurfNum) +
                          state.dataWindowManager->hrgap[1] * (state.dataSurface->SurfWinEdgeGlCorrFac(SurfNum) - 1.0);
            }

            WindowGasConductance(state, state.dataWindowManager->thetas[5], state.dataWindowManager->thetas[6], 3, con, pr, gr);
            NusseltNumber(state, SurfNum, state.dataWindowManager->thetas[5], state.dataWindowManager->thetas[6], 3, gr, pr, nu);
            hgap(3) = con / state.dataWindowManager->gap[2] * nu;
            if (state.dataSurface->SurfWinEdgeGlCorrFac(SurfNum) > 1.0) { // Edge of glass correction
                state.dataWindowManager->hrgap[2] =
                    0.5 * std::abs(state.dataWindowManager->A67) * pow_3(state.dataWindowManager->thetas[5] + state.dataWindowManager->thetas[6]);
                hgap(3) = hgap(3) * state.dataSurface->SurfWinEdgeGlCorrFac(SurfNum) +
                          state.dataWindowManager->hrgap[2] * (state.dataSurface->SurfWinEdgeGlCorrFac(SurfNum) - 1.0);
            }
            Bface(1) = state.dataWindowManager->Outir * state.dataWindowManager->emis[0] +
                       state.dataWindowManager->hcout * state.dataWindowManager->tout + state.dataWindowManager->AbsRadGlassFace[0];
            Bface(2) = state.dataWindowManager->AbsRadGlassFace[1];
            Bface(3) = state.dataWindowManager->AbsRadGlassFace[2];
            Bface(4) = state.dataWindowManager->AbsRadGlassFace[3];
            Bface(5) = state.dataWindowManager->AbsRadGlassFace[4];
            Bface(6) = state.dataWindowManager->AbsRadGlassFace[5];
            Bface(7) = state.dataWindowManager->AbsRadGlassFace[6];
            Bface(8) = state.dataWindowManager->Rmir * state.dataWindowManager->emis[7] +
                       state.dataWindowManager->hcin * state.dataWindowManager->tin + state.dataWindowManager->AbsRadGlassFace[7];

            Aface(1, 1) = hr(1) + state.dataWindowManager->scon[0] + state.dataWindowManager->hcout;
            Aface(2, 1) = -state.dataWindowManager->scon[0];

            Aface(1, 2) = -state.dataWindowManager->scon[0];
            Aface(2, 2) = state.dataWindowManager->scon[0] + hgap(1) - state.dataWindowManager->A23P * hr(2);
            Aface(3, 2) = -hgap(1) - state.dataWindowManager->A32P * hr(3);

            Aface(2, 3) = -hgap(1) + state.dataWindowManager->A23P * hr(2);
            Aface(3, 3) = hgap(1) + state.dataWindowManager->scon[1] + state.dataWindowManager->A32P * hr(3);
            Aface(4, 3) = -state.dataWindowManager->scon[1];

            Aface(3, 4) = -state.dataWindowManager->scon[1];
            Aface(4, 4) = state.dataWindowManager->scon[1] + hgap(2) - state.dataWindowManager->A45P * hr(4);
            Aface(5, 4) = -hgap(2) - state.dataWindowManager->A54P * hr(5);

            Aface(4, 5) = -hgap(2) + state.dataWindowManager->A45P * hr(4);
            Aface(5, 5) = hgap(2) + state.dataWindowManager->scon[2] + state.dataWindowManager->A54P * hr(5);
            Aface(6, 5) = -state.dataWindowManager->scon[2];

            Aface(5, 6) = -state.dataWindowManager->scon[2];
            Aface(6, 6) = state.dataWindowManager->scon[2] + hgap(3) - state.dataWindowManager->A67P * hr(6);
            Aface(7, 6) = -hgap(3) - state.dataWindowManager->A76P * hr(7);

            Aface(6, 7) = -hgap(3) + state.dataWindowManager->A67P * hr(6);
            Aface(7, 7) = hgap(3) + state.dataWindowManager->scon[3] + state.dataWindowManager->A76P * hr(7);
            Aface(8, 7) = -state.dataWindowManager->scon[3];

            Aface(7, 8) = -state.dataWindowManager->scon[3];
            Aface(8, 8) = hr(8) + state.dataWindowManager->scon[3] + state.dataWindowManager->hcin;

            if (ANY_INTERIOR_SHADE_BLIND(ShadeFlag)) {
                Bface(8) = state.dataWindowManager->Rmir * state.dataWindowManager->emis[7] * TauShIR / ShGlReflFacIR + hcv * TGapNew +
                           state.dataWindowManager->AbsRadGlassFace[7];
                Bface(9) = state.dataWindowManager->Rmir * TauShIR * RhoGlIR2 * EpsShIR1 / ShGlReflFacIR + hcv * TGapNew + AbsRadShadeFace(1);
                Bface(10) =
                    state.dataWindowManager->Rmir * EpsShIR2 + state.dataWindowManager->hcin * state.dataWindowManager->tin + AbsRadShadeFace(2);

                Aface(8, 8) = hr(8) * (1 - RhoShIR1) / ShGlReflFacIR + state.dataWindowManager->scon[3] + hcv;
                Aface(9, 8) = -state.dataWindowManager->emis[7] * hr(9) / ShGlReflFacIR;
                Aface(8, 9) = -hr(8) * EpsShIR1 / ShGlReflFacIR;
                Aface(9, 9) = hr(9) * (1 - RhoGlIR2 * (EpsShIR1 + RhoShIR1)) / ShGlReflFacIR + sconsh + hcv;
                Aface(10, 9) = -sconsh;
                Aface(9, 10) = -sconsh;
                Aface(10, 10) = hr(10) + sconsh + state.dataWindowManager->hcin;
            }

            if (ANY_EXTERIOR_SHADE_BLIND_SCREEN(ShadeFlag)) {
                Bface(1) = state.dataWindowManager->Outir * state.dataWindowManager->emis[0] * TauShIR / ShGlReflFacIR + hcv * TGapNew +
                           state.dataWindowManager->AbsRadGlassFace[0];
                Bface(9) =
                    state.dataWindowManager->Outir * EpsShIR1 + state.dataWindowManager->hcout * state.dataWindowManager->tout + AbsRadShadeFace(1);
                Bface(10) = state.dataWindowManager->Outir * TauShIR * RhoGlIR1 * EpsShIR2 / ShGlReflFacIR + hcv * TGapNew + AbsRadShadeFace(2);

                Aface(1, 1) = hr(1) * (1 - RhoShIR2) / ShGlReflFacIR + state.dataWindowManager->scon[0] + hcv;
                Aface(10, 1) = -state.dataWindowManager->emis[0] * hr(10) / ShGlReflFacIR;
                Aface(9, 9) = hr(9) + sconsh + state.dataWindowManager->hcout;
                Aface(10, 9) = -sconsh;
                Aface(1, 10) = -hr(1) * EpsShIR2 / ShGlReflFacIR;
                Aface(9, 10) = -sconsh;
                Aface(10, 10) = hr(10) * (1 - RhoGlIR1 * (EpsShIR2 + RhoShIR2)) / ShGlReflFacIR + sconsh + hcv;
            }

        } else {
            ShowFatalError(
                state, format("SolveForWindowTemperatures: Invalid number of Glass Layers={}, up to 4 allowed.", state.dataWindowManager->ngllayer));
        }
    }

    void SolveForWindowTemperatures(EnergyPlusData &state, int const SurfNum) // Surface number
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         F. Winkelmann
        //       DATE WRITTEN   July 2000
        //       MODIFIED       Oct 2000, FW: modify edge-of-glass correction to account
        //                       for gap radiative conductance affects
        //                      Feb 2001, FW: add interior or exterior shade to layer
        //                       heat balance calculation.
        //                      Mar 2001, FW: relax error tolerance if MaxIterations reached.
        //                      Jun 2001, FW: add interior or exterior blind
        //                      Nov 2002, FW: add relaxation on face temperatures
        //                       to improve convergence for multipane cases where outer pane
        //                       has high solar absorptance: temp --> 0.5*(temp + previous temp);
        //                       also, increase MaxIterations from 50 to 100.
        //                      Dec 2002, FW: add between-glass shade/blind for double and triple glazing.
        //                      Mar 2003, FW: remove redundant relaxation on radiative conductances
        //                      Mar 2003, FW: increase convergence tolerance from 0.01 to 0.02 to enhance
        //                                    convergence in difficult cases.
        //                      June 2003, FW: correct the expression for convective gain to zone air
        //                       from airflow windows with airflow destination = InsideAir. Previously
        //                       convective gain of air as it passed through gap was used, which is correct
        //                       for airflow source = InsideAir but incorrect for airflow source = OutsideAir.
        //                       Save SurfaceWindow%TAirflowGapOutlet for use in calculating convective heat
        //                       gain to return air when airflow source = InsideAir, destination = ReturnAir.
        //                      Dec 2003, FW: enhance converge for difficult cases by increasing relaxation
        //                       in layer surface temperatures for iterations > MaxIterations/4
        //                      May 2006, RR: add exterior window screen
        //                      January 2009, BG: inserted call to recalc inside face convection inside iteration loop
        //                        per ISO 15099 Section 8.3.2.2
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Evaluates the coefficients Aface and Bface in the system of linear
        // algebraic equations
        //     Sum [Aface(i,j)*thetas(j)] = Bface(i), i = 1,nglfacep, j=1,nglfacep
        // where
        // nglface  = number of glass faces (= 2 * number of glass layers), or, if shade or blind is present,
        // nglgacep = number of glass faces + 2
        // thetas(j) = temperature of face j
        // If an interior, exterior or between-glass shade or blind, or exterior screen is present
        // the face numbering is as follows:
        //   1 to 2*nglface are glass faces, from outside to inside;
        //   2*nglface+1 and 2*nglface+2 are the shade or blind faces, from outside to inside
        // For example, the following diagram shows the face number for an exterior shade, screen or blind
        // on double glazing:
        //     ||   ||   ||
        //    5||6 1||2 3||4
        //     ||   ||   ||
        // bl/sh/sc gl   gl

        // And for a between-glass shade/blind in triple glazing:
        //     ||   ||   ||   ||
        //    1||2 3||4 7||8 5||6
        //     ||   ||   ||   ||
        //     gl   gl  bl/sh gl

        // METHODOLOGY EMPLOYED:
        // The Aface and Bface coefficients are determined by the equations for
        // heat balance at the glass and shade/blind faces. The system of linear equations is solved
        // by LU decomposition.

        // Using/Aliasing
        using ConvectionCoefficients::CalcISO15099WindowIntConvCoeff;
        using General::InterpSw;
        using Psychrometrics::PsyCpAirFnW;
        using Psychrometrics::PsyHFnTdbW;
        using Psychrometrics::PsyRhoAirFnPbTdbW;
        using Psychrometrics::PsyTdbFnHW;

        constexpr int MaxIterations(100); // Maximum allowed number of iterations (increased 9/01 from 15 to 50,
        //   increased 11/02 from 50 to 100)
        constexpr Real64 errtemptol(0.02); // Tolerance on errtemp for convergence (increased from 0.01, 3/4/03)

        int ZoneNum; // Zone number corresponding to SurfNum
        int i;       // Counter
        Real64 d;    // +1 if number of row interchanges is even,
        // -1 if odd (in LU decomposition)

        auto &hr = state.dataWindowManager->hr;
        auto &indx = state.dataWindowManager->indx;
        auto &Aface = state.dataWindowManager->Aface;
        auto &Bface = state.dataWindowManager->Bface;
        auto &TGapNewBG = state.dataWindowManager->TGapNewBG;
        auto &hcvBG = state.dataWindowManager->hcvBG;
        auto &AbsRadShadeFace = state.dataWindowManager->AbsRadShadeFace;

        int iter = 0;                    // Iteration number
        Real64 errtemp = 0.0;            // Absolute value of sum of face temperature differences between iterations, divided by number of faces
        Real64 VGap = 0.0;               // Air velocity in gap between glass and shade/blind (m/s)
        Real64 VAirflowGap = 0.0;        // Air velocity in airflow gap between glass panes (m/s)
        Real64 VGapPrev = 0.0;           // Value of VGap from previous iteration
        Real64 TGapNew = 0.0;            // Average air temp in gap between glass and shade/blind (K)
        Real64 TAirflowGapNew = 0.0;     // Average air temp in airflow gap between glass panes (K)
        Real64 TGapOutlet = 0.0;         // Temperature of air leaving gap between glass and shade/blind (K)
        Real64 TAirflowGapOutlet = 0.0;  // Temperature of air leaving airflow gap between glass panes (K)
        Real64 TAirflowGapOutletC = 0.0; // Temperature of air leaving airflow gap between glass panes (C)
        Real64 hcv = 0.0;                // Convection coefficient from gap glass or shade/blind to gap air (W/m2-K)
        Real64 hcvAirflowGap = 0.0;      // Convection coefficient from airflow gap glass to airflow gap air (W/m2-K)
        Real64 hcvPrev = 0.0;            // Value of hcv from previous iteration
        Real64 ConvHeatFlowForced = 0.0; // Convective heat flow from forced airflow gap (W)
        Real64 ShGlReflFacIR = 0.0;      // Factor for long-wave inter-reflection between shade/blind and adjacent glass
        Real64 RhoGlIR1 = 0.0;           // Long-wave reflectance of glass surface facing shade/blind; 1=exterior shade/blind,
        Real64 RhoGlIR2 = 0.0;
        //  2=exterior shade/blind
        Real64 EpsShIR1 = 0.0; // Long-wave emissivity of shade/blind surface facing glass; 1=interior shade/blind,
        Real64 EpsShIR2 = 0.0;
        //  2=interior shade/blind
        Real64 RhoShIR1 = 0.0; // Long-wave reflectance of shade/blind surface facing glass; 1=interior shade/blind,
        Real64 RhoShIR2 = 0.0;
        //  2=exterior shade/blind
        Real64 TauShIR = 0.0; // Long-wave transmittance of isolated shade/blind
        Real64 sconsh = 0.0;  // shade/blind conductance (W/m2-K)

        WinShadingType ShadeFlag = WinShadingType::NoShade; // Shading flag
        //  radiation from lights and zone equipment absorbed by faces of shade/blind (W/m2)
        Real64 ShadeArea = 0.0; // shade/blind area (m2)
        // Real64 CondHeatGainGlass = 0.0; // Conduction through inner glass layer, outside to inside (W)
        // Real64 CondHeatGainShade = 0.0; // Conduction through shade/blind, outside to inside (W)
        //  shade/blind is present. Zero if shade/blind has zero IR transmittance (W)
        // Real64 IncidentSolar = 0.0;         // Solar incident on outside of window (W)
        Real64 TransDiff = 0.0;      // Diffuse shortwave transmittance
        Real64 TotAirflowGap = 0.0;  // Total volumetric airflow through window gap (m3/s)
        Real64 CpAirOutlet = 0.0;    // Heat capacity of air from window gap (J/kg-K)
        Real64 CpAirZone = 0.0;      // Heat capacity of zone air (J/kg-K)
        Real64 InletAirHumRat = 0.0; // Humidity ratio of air from window gap entering fan
        Real64 ZoneTemp = 0.0;       // Zone air temperature (C)
        int InsideFaceIndex = 0;     // intermediate variable for index of inside face in thetas

        state.dataWindowManager->nglfacep = state.dataWindowManager->nglface;
        ShadeFlag = state.dataSurface->SurfWinShadingFlag(SurfNum);
        ZoneNum = state.dataSurface->Surface(SurfNum).Zone;
        AbsRadShadeFace = 0.0;

        if (ANY_SHADE_SCREEN(ShadeFlag) || ANY_BLIND(ShadeFlag)) {
            state.dataWindowManager->nglfacep = state.dataWindowManager->nglface + 2;
            AbsRadShadeFace(1) = DataSurfaces::AbsFrontSide(state, SurfNum);
            AbsRadShadeFace(2) = DataSurfaces::AbsBackSide(state, SurfNum);
            if (ANY_INTERIOR_SHADE_BLIND(ShadeFlag)) AbsRadShadeFace(2) += state.dataSurface->SurfWinIntLWAbsByShade(SurfNum);
            sconsh = state.dataWindowManager->scon[state.dataWindowManager->ngllayer];
            TauShIR = state.dataWindowManager->tir[state.dataWindowManager->nglface];
            EpsShIR1 = state.dataWindowManager->emis[state.dataWindowManager->nglface];
            EpsShIR2 = state.dataWindowManager->emis[state.dataWindowManager->nglface + 1];
            RhoShIR1 = max(0.0, 1.0 - TauShIR - EpsShIR1);
            RhoShIR2 = max(0.0, 1.0 - TauShIR - EpsShIR2);
            if (ANY_INTERIOR_SHADE_BLIND(ShadeFlag)) {
                RhoGlIR2 = 1.0 - state.dataWindowManager->emis[2 * state.dataWindowManager->ngllayer - 1];
                ShGlReflFacIR = 1.0 - RhoGlIR2 * RhoShIR1;
            } else if (ANY_EXTERIOR_SHADE_BLIND_SCREEN(ShadeFlag)) {
                RhoGlIR1 = 1.0 - state.dataWindowManager->emis[0];
                ShGlReflFacIR = 1.0 - RhoGlIR1 * RhoShIR2;
            }
        } // End of check if shade or blind is on

        // Initialize face temperatures.

        StartingWindowTemps(state, SurfNum, AbsRadShadeFace);

        hcvPrev = 0.0;
        VGapPrev = 0.0;

        // Calculate radiative conductances

        errtemp = errtemptol * 2.0;

        while (iter < MaxIterations && errtemp > errtemptol) {

            for (i = 1; i <= state.dataWindowManager->nglfacep; ++i) {
                hr(i) = state.dataWindowManager->emis[i - 1] * state.dataWindowManager->sigma * pow_3(state.dataWindowManager->thetas[i - 1]);
                // Following line is redundant since thetas is being relaxed;
                // removed by FCW, 3/4/03
                //! fw if ( iter >= 1 ) hr(i) = 0.5*(hrprev(i)+hr(i))
            }

            // call for new interior film coeff (since it is temperature dependent) if using Detailed inside coef model
            if (((state.dataSurface->SurfIntConvCoeffIndex(SurfNum) == 0) &&
                 (state.dataHeatBal->Zone(ZoneNum).InsideConvectionAlgo == ConvectionConstants::HcInt_ASHRAETARP)) ||
                (state.dataSurface->SurfIntConvCoeffIndex(SurfNum) == -2)) {
                // coef model is "detailed" and not prescribed by user
                // need to find inside face index, varies with shade/blind etc.
                if (ANY_INTERIOR_SHADE_BLIND(ShadeFlag)) {
                    InsideFaceIndex = state.dataWindowManager->nglfacep;
                } else {
                    InsideFaceIndex = state.dataWindowManager->nglface;
                }
                CalcISO15099WindowIntConvCoeff(state,
                                               SurfNum,
                                               state.dataWindowManager->thetas[InsideFaceIndex - 1] - DataGlobalConstants::KelvinConv,
                                               state.dataWindowManager->tin - DataGlobalConstants::KelvinConv);
                state.dataWindowManager->hcin = state.dataHeatBalSurf->SurfHConvInt(SurfNum);
            }

            Aface = 0.0;
            Bface = 0.0;

            // If interior or exterior shade or blind is present, get heat transfer
            // coefficient from glass and shade/blind to gap between glass and shade/blind,
            // effective gap air temperature, velocity of air in gap and gap outlet temperature.

            if (ANY_EXTERIOR_SHADE_BLIND_SCREEN(ShadeFlag) || ANY_INTERIOR_SHADE_BLIND(ShadeFlag)) {
                ExtOrIntShadeNaturalFlow(
                    state, SurfNum, iter, VGap, TGapNew, TGapOutlet, hcv, state.dataSurface->SurfWinConvHeatFlowNatural(SurfNum));
                if (iter >= 1) {
                    hcv = 0.5 * (hcvPrev + hcv);
                    VGap = 0.5 * (VGapPrev + VGap);
                }
                hcvPrev = hcv;
                VGapPrev = VGap;
            }

            TAirflowGapOutlet = 0.0;
            // If between-glass shade or blind is not present and this is an airflow window
            // (i.e., with forced airflow in the gap for double glass or in the inner gap for triple glass)
            // get glass-to-air forced convection heat transfer coefficient, average gap air temperature, and
            // convective heat flow from gap.

            if (!ANY_BETWEENGLASS_SHADE_BLIND(ShadeFlag) && state.dataSurface->SurfWinAirflowThisTS(SurfNum) > 0.0) {
                BetweenGlassForcedFlow(state, SurfNum, iter, VAirflowGap, TAirflowGapNew, TAirflowGapOutlet, hcvAirflowGap, ConvHeatFlowForced);
            }

            // If between-glass shade or blind is present, get convective heat transfer
            // coefficients from glass and shade/blind to the two gaps on either side of the shade/blind.
            // Also get average gas temperature in the two gaps, and, for airflow window, the sum of the
            // convective heat flows from the gaps.

            if (ANY_BETWEENGLASS_SHADE_BLIND(ShadeFlag)) {
                if (state.dataSurface->SurfWinAirflowThisTS(SurfNum) == 0.0) { // Natural convection in gaps
                    BetweenGlassShadeNaturalFlow(state, SurfNum, iter, VGap, TGapNewBG, hcvBG);
                } else { // Forced convection in gaps
                    BetweenGlassShadeForcedFlow(state, SurfNum, iter, VGap, TGapNewBG, TAirflowGapOutlet, hcvBG, ConvHeatFlowForced);
                }
            }

            ++iter;

            // Calculations based on number of glass layers
            GetHeatBalanceEqCoefMatrix(state,
                                       SurfNum,
                                       state.dataWindowManager->ngllayer,
                                       ShadeFlag,
                                       sconsh,
                                       TauShIR,
                                       EpsShIR1,
                                       EpsShIR2,
                                       RhoShIR1,
                                       RhoShIR2,
                                       ShGlReflFacIR,
                                       RhoGlIR1,
                                       RhoGlIR2,
                                       hcv,
                                       TGapNew,
                                       TAirflowGapNew,
                                       hcvAirflowGap,
                                       hcvBG,
                                       TGapNewBG,
                                       AbsRadShadeFace,
                                       hr,
                                       Aface,
                                       Bface);
            LUdecomposition(state, Aface, state.dataWindowManager->nglfacep, indx,
                            d); // Note that these routines change Aface;
            LUsolution(Aface, state.dataWindowManager->nglfacep, indx,
                       Bface); // face temperatures are returned in Bface

            for (i = 1; i <= state.dataWindowManager->nglfacep; ++i) {
                state.dataWindowManager->thetasPrev[i - 1] = state.dataWindowManager->thetas[i - 1];
                if (iter < MaxIterations / 4) {
                    state.dataWindowManager->thetas[i - 1] = 0.5 * state.dataWindowManager->thetas[i - 1] + 0.5 * Bface(i);
                } else {
                    state.dataWindowManager->thetas[i - 1] = 0.75 * state.dataWindowManager->thetas[i - 1] + 0.25 * Bface(i);
                }
            }

            errtemp = 0.0;
            for (i = 1; i <= state.dataWindowManager->nglfacep; ++i) {
                errtemp += std::abs(state.dataWindowManager->thetas[i - 1] - state.dataWindowManager->thetasPrev[i - 1]);
            }
            errtemp /= state.dataWindowManager->nglfacep;
        }

        state.dataSurface->SurfWinWindowCalcIterationsRep(SurfNum) = iter;

        // We have reached iteration limit or we have converged. If we have reached the
        // iteration limit the following test relaxes the convergence tolerance.
        // If we have converged (errtemp <= errtemptol) the following test has not effect.

        if (errtemp < 10 * errtemptol) {

            // Window heat balance solution has converged.

            // For interior shade, add convective gain from glass/shade gap air flow to zone convective gain;
            // For all cases, get total window heat gain for reporting. See CalcWinFrameAndDividerTemps for
            // contribution of frame and divider.
            // IncidentSolar = state.dataSurface->Surface(SurfNum).Area * state.dataHeatBal->SurfQRadSWOutIncident(SurfNum);
            if (ANY_INTERIOR_SHADE_BLIND(ShadeFlag)) {
                // Interior shade or blind
                // Window heat gain from glazing and shade/blind to zone. Consists of transmitted solar, convection
                //   from air exiting gap, convection from zone-side of shade/blind, net IR to zone from shade and net IR to
                //   zone from the glass adjacent to the shade/blind (zero if shade/blind IR transmittance is zero).
                // Following assumes glazed area = window area (i.e., dividers ignored) in calculating
                //   IR to zone from glass when interior shade/blind is present.
                ShadeArea = state.dataSurface->Surface(SurfNum).Area + state.dataSurface->SurfWinDividerArea(SurfNum);
                // CondHeatGainShade = ShadeArea * sconsh *
                //                     (state.dataWindowManager->thetas(state.dataWindowManager->nglfacep - 1) -
                //                     state.dataWindowManager->thetas[state.dataWindowManager->nglfacep-1]);
                state.dataSurface->SurfWinGainIRShadeToZoneRep(SurfNum) =
                    ShadeArea * EpsShIR2 *
                        (state.dataWindowManager->sigma * pow_4(state.dataWindowManager->thetas[state.dataWindowManager->nglfacep - 1]) -
                         state.dataWindowManager->Rmir) +
                    EpsShIR1 *
                        (state.dataWindowManager->sigma * pow_4(state.dataWindowManager->thetas[state.dataWindowManager->nglfacep - 2]) -
                         state.dataWindowManager->Rmir) *
                        RhoGlIR2 * TauShIR / ShGlReflFacIR;
                state.dataSurface->SurfWinGainIRGlazToZoneRep(SurfNum) =
                    ShadeArea * (state.dataWindowManager->emis[2 * state.dataWindowManager->ngllayer - 1] * TauShIR / ShGlReflFacIR) *
                    (state.dataWindowManager->sigma * pow_4(state.dataWindowManager->thetas[2 * state.dataWindowManager->ngllayer - 1]) -
                     state.dataWindowManager->Rmir);
                state.dataSurface->SurfWinGainConvShadeToZoneRep(SurfNum) =
                    ShadeArea * state.dataWindowManager->hcin *
                    (state.dataWindowManager->thetas[state.dataWindowManager->nglfacep - 1] - state.dataWindowManager->tin);
                state.dataSurface->SurfWinHeatGain(SurfNum) =
                    state.dataSurface->SurfWinTransSolar(SurfNum) + state.dataSurface->SurfWinConvHeatFlowNatural(SurfNum) +
                    state.dataSurface->SurfWinGainConvShadeToZoneRep(SurfNum) + state.dataSurface->SurfWinGainIRGlazToZoneRep(SurfNum) +
                    state.dataSurface->SurfWinGainIRShadeToZoneRep(SurfNum);
            } else {
                // Interior shade or blind not present; innermost layer is glass
                // CondHeatGainGlass = state.dataSurface->Surface(SurfNum).Area * state.dataWindowManager->scon(state.dataWindowManager->ngllayer) *
                //                     (state.dataWindowManager->thetas(2 * state.dataWindowManager->ngllayer - 1) -
                //                     state.dataWindowManager->thetas[2 * state.dataWindowManager->ngllayer - 1]);
                state.dataSurface->SurfWinGainIRGlazToZoneRep(SurfNum) =
                    state.dataSurface->Surface(SurfNum).Area * state.dataWindowManager->emis[2 * state.dataWindowManager->ngllayer - 1] *
                    (state.dataWindowManager->sigma * pow_4(state.dataWindowManager->thetas[2 * state.dataWindowManager->ngllayer - 1]) -
                     state.dataWindowManager->Rmir);
                state.dataSurface->SurfWinGainConvGlazToZoneRep(SurfNum) =
                    state.dataSurface->Surface(SurfNum).Area * state.dataWindowManager->hcin *
                    (state.dataWindowManager->thetas[2 * state.dataWindowManager->ngllayer - 1] - state.dataWindowManager->tin);
                state.dataSurface->SurfWinHeatGain(SurfNum) = state.dataSurface->SurfWinTransSolar(SurfNum) +
                                                              state.dataSurface->SurfWinGainConvGlazToZoneRep(SurfNum) +
                                                              state.dataSurface->SurfWinGainIRGlazToZoneRep(SurfNum);
            }

            // Add convective heat gain from airflow window
            // Note: effect of fan heat on gap outlet temperature is neglected since fan power (based
            // on pressure drop through the gap) is extremely small

            state.dataSurface->SurfWinGapConvHtFlowRep(SurfNum) = 0.0;
            state.dataSurface->SurfWinGapConvHtFlowRepEnergy(SurfNum) = 0.0;
            TotAirflowGap = state.dataSurface->SurfWinAirflowThisTS(SurfNum) * state.dataSurface->Surface(SurfNum).Width;
            TAirflowGapOutletC = TAirflowGapOutlet - state.dataWindowManager->TKelvin;
            state.dataSurface->SurfWinTAirflowGapOutlet(SurfNum) = TAirflowGapOutletC;
            if (state.dataSurface->SurfWinAirflowThisTS(SurfNum) > 0.0) {
                state.dataSurface->SurfWinGapConvHtFlowRep(SurfNum) = ConvHeatFlowForced;
                state.dataSurface->SurfWinGapConvHtFlowRepEnergy(SurfNum) =
                    state.dataSurface->SurfWinGapConvHtFlowRep(SurfNum) * state.dataGlobal->TimeStepZoneSec;
                // Add heat from gap airflow to zone air if destination is inside air; save the heat gain to return
                // air in case it needs to be sent to the zone (due to no return air determined in HVAC simulation)
                if (state.dataSurface->SurfWinAirflowDestination(SurfNum) == AirFlowWindow_Destination_IndoorAir ||
                    state.dataSurface->SurfWinAirflowDestination(SurfNum) == AirFlowWindow_Destination_ReturnAir) {
                    if (state.dataSurface->SurfWinAirflowSource(SurfNum) == AirFlowWindow_Source_IndoorAir) {
                        InletAirHumRat = state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum);
                    } else { // AirflowSource = outside air
                        InletAirHumRat = state.dataEnvrn->OutHumRat;
                    }
                    ZoneTemp = state.dataHeatBalFanSys->MAT(ZoneNum); // this should be Tin (account for different reference temps)
                    CpAirOutlet = PsyCpAirFnW(InletAirHumRat);
                    CpAirZone = PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum));
                    state.dataSurface->SurfWinRetHeatGainToZoneAir(SurfNum) =
                        TotAirflowGap * (CpAirOutlet * (TAirflowGapOutletC)-CpAirZone * ZoneTemp);
                    if (state.dataSurface->SurfWinAirflowDestination(SurfNum) == AirFlowWindow_Destination_IndoorAir) {
                        state.dataSurface->SurfWinHeatGain(SurfNum) += state.dataSurface->SurfWinRetHeatGainToZoneAir(SurfNum);
                    }
                }
                // For AirflowDestination = ReturnAir in a controlled (i.e., conditioned) zone with return air, see CalcZoneLeavingConditions
                // for calculation of modification of return-air temperature due to airflow from window gaps into return air.
            }

            // Correct WinHeatGain for interior diffuse shortwave (solar and shortwave from lights) transmitted
            // back out window
            int const ConstrNum = state.dataSurface->SurfActiveConstruction(SurfNum);
            int const ConstrNumSh = state.dataSurface->SurfWinActiveShadedConstruction(SurfNum);

            TransDiff = state.dataConstruction->Construct(ConstrNum).TransDiff; // Default value for TransDiff here
            if (NOT_SHADED(ShadeFlag)) {
                TransDiff = state.dataConstruction->Construct(ConstrNum).TransDiff;
            } else if (ANY_SHADE_SCREEN(ShadeFlag)) {
                TransDiff = state.dataConstruction->Construct(ConstrNumSh).TransDiff;
            } else if (ANY_BLIND(ShadeFlag)) {
                if (state.dataSurface->SurfWinMovableSlats(SurfNum)) {
                    TransDiff = General::InterpGeneral(
                        state.dataConstruction->Construct(ConstrNumSh).BlTransDiff(state.dataSurface->SurfWinSlatsAngIndex(SurfNum)),
                        state.dataConstruction->Construct(ConstrNumSh)
                            .BlTransDiff(std::min(MaxSlatAngs, state.dataSurface->SurfWinSlatsAngIndex(SurfNum) + 1)),
                        state.dataSurface->SurfWinSlatsAngInterpFac(SurfNum));
                } else {
                    TransDiff = state.dataConstruction->Construct(ConstrNumSh).BlTransDiff(1);
                }
            } else if (ShadeFlag == WinShadingType::SwitchableGlazing) {
                TransDiff = InterpSw(state.dataSurface->SurfWinSwitchingFactor(SurfNum),
                                     state.dataConstruction->Construct(ConstrNum).TransDiff,
                                     state.dataConstruction->Construct(ConstrNumSh).TransDiff);
            }
            // shouldn't this be + outward flowing fraction of absorbed SW? -- do not know whose comment this is?  LKL (9/2012)
            state.dataSurface->SurfWinLossSWZoneToOutWinRep(SurfNum) =
                state.dataHeatBal->EnclSolQSWRad(state.dataSurface->Surface(SurfNum).SolarEnclIndex) * state.dataSurface->Surface(SurfNum).Area *
                TransDiff;
            state.dataSurface->SurfWinHeatGain(SurfNum) -= state.dataSurface->SurfWinLossSWZoneToOutWinRep(SurfNum);

            if (ANY_SHADE_SCREEN(ShadeFlag) || ANY_BLIND(ShadeFlag)) {
                state.dataSurface->SurfWinShadingAbsorbedSolar(SurfNum) =
                    (state.dataSurface->SurfWinExtBeamAbsByShade(SurfNum) + state.dataSurface->SurfWinExtDiffAbsByShade(SurfNum)) *
                    (state.dataSurface->Surface(SurfNum).Area + state.dataSurface->SurfWinDividerArea(SurfNum));
                state.dataSurface->SurfWinShadingAbsorbedSolarEnergy(SurfNum) =
                    state.dataSurface->SurfWinShadingAbsorbedSolar(SurfNum) * state.dataGlobal->TimeStepZoneSec;
            }
            if (state.dataEnvrn->SunIsUp) {

                state.dataSurface->SurfWinSysSolTransmittance(SurfNum) =
                    state.dataSurface->SurfWinTransSolar(SurfNum) /
                    (state.dataHeatBal->SurfQRadSWOutIncident(SurfNum) *
                         (state.dataSurface->Surface(SurfNum).Area + state.dataSurface->SurfWinDividerArea(SurfNum)) +
                     0.0001);
                state.dataSurface->SurfWinSysSolAbsorptance(SurfNum) =
                    (state.dataHeatBal->SurfWinQRadSWwinAbsTot(SurfNum) + state.dataSurface->SurfWinShadingAbsorbedSolar(SurfNum)) /
                    (state.dataHeatBal->SurfQRadSWOutIncident(SurfNum) *
                         (state.dataSurface->Surface(SurfNum).Area + state.dataSurface->SurfWinDividerArea(SurfNum)) +
                     0.0001);
                state.dataSurface->SurfWinSysSolReflectance(SurfNum) =
                    1.0 - state.dataSurface->SurfWinSysSolTransmittance(SurfNum) - state.dataSurface->SurfWinSysSolAbsorptance(SurfNum);
            } else {
                state.dataSurface->SurfWinSysSolTransmittance(SurfNum) = 0.0;
                state.dataSurface->SurfWinSysSolAbsorptance(SurfNum) = 0.0;
                state.dataSurface->SurfWinSysSolReflectance(SurfNum) = 0.0;
            }

            // Save hcv for use in divider calc with interior or exterior shade (see CalcWinFrameAndDividerTemps)
            if (ANY_INTERIOR_SHADE_BLIND(ShadeFlag) || ANY_EXTERIOR_SHADE_BLIND_SCREEN(ShadeFlag))
                state.dataSurface->SurfWinConvCoeffWithShade(SurfNum) = hcv;
        } else {
            // No convergence after MaxIterations even with relaxed error tolerance
            ShowSevereError(state, "Convergence error in SolveForWindowTemperatures for window " + state.dataSurface->Surface(SurfNum).Name);
            ShowContinueErrorTimeStamp(state, "");

            if (state.dataGlobal->DisplayExtraWarnings) {
                // report out temperatures
                for (i = 1; i <= state.dataWindowManager->nglfacep; ++i) {
                    ShowContinueError(state,
                                      format("Glazing face index = {} ; new temperature ={:.4R}C  ; previous temperature = {:.4R}C",
                                             i,
                                             state.dataWindowManager->thetas[i - 1] - DataGlobalConstants::KelvinConv,
                                             state.dataWindowManager->thetasPrev[i - 1] - DataGlobalConstants::KelvinConv));
                }
            }

            ShowFatalError(state,
                           "Program halted because of convergence error in SolveForWindowTemperatures for window " +
                               state.dataSurface->Surface(SurfNum).Name);
        }
    }

    //****************************************************************************

    void ExtOrIntShadeNaturalFlow(EnergyPlusData &state,
                                  int const SurfNum,  // Surface number
                                  int const iter,     // Iteration number for glass heat balance calculation
                                  Real64 &VGap,       // Air velocity in glass-shade/blind gap (m/s)
                                  Real64 &TGapNew,    // Current-iteration average air temp in glass-shade/blind gap (K)
                                  Real64 &TGapOutlet, // Temperature of air leaving glass-shade/blind gap at top for upward
                                  Real64 &hcv,        // Convection coefficient from gap glass or shade to gap air (W/m2-K)
                                  Real64 &QConvGap    // Convective heat gain from glass-shade/blind gap for interior shade (W)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         F. Winkelmann
        //       DATE WRITTEN   December 2000
        //       MODIFIED       June 2001: add window blinds
        //                      May 2006 (RR): add exterior window screens
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Called by SolveForWindowTemperatures for windows that have an interior
        // or exterior blind or shade in place.
        // Solves for air flow in gap between glass and shade/blind.
        // Finds temperature of gap air and coefficient for convective heat transfer
        // from glass to gap air and shade/blind to gap air.

        // METHODOLOGY EMPLOYED:
        // Based on ISO/DIS 15099, "Thermal Performance of Windows, Doors and Shading Devices --
        // Detailed Calculations," 1/12/2000, Chapter 7, "Shading Devices."

        // SUBROUTINE ARGUMENT DEFINITIONS:
        //   air flow or bottom for downward air flow (K)

        int ConstrNumSh;  // Shaded construction number
        int MatNumSh;     // Material number of shade/blind layer
        int nglassfaces;  // Number of glass faces in contruction
        Real64 TGapInlet; // Temperature of air entering glass-shade/blind gap at bottom for upward
        //   air flow or top for downward air flow (K)
        Real64 TGlassFace; // Temperature of glass surface facing glass-shade/blind gap (K)
        Real64 TShadeFace; // Temperature of shade surface facing glass-shade/blind gap (K)
        Real64 hGapStill;  // Still-air glass-shade/blind gap conduction/convection coeff (W/m2-K)
        Real64 TGapOld;    // Previous-iteration average air temp in glass-shade/blind gap (K)
        Real64 GapHeight;  // Vertical length of glass-shade/blind gap (m)
        Real64 GapDepth;   // Distance from shade to glass (m)
        Real64 RhoAir;     // Density of glass-shade/blind gap air at a temperature of TGapOld (kg/m3)
        Real64 RhoTRef;    // Density of glass-shade/blind air at reference temp = KelvinConv (kg/m3)
        Real64 ViscAir;    // Viscosity of glass-shade/blind gap air at a temperature of TGapOld (kg/m3)
        Real64 AGap;       // Cross sectional area of glass-shade/blind gap (m2); for vertical window, this
        //   is in horizontal plane normal to window.
        Real64 ATopGap; // Area of the top and bottom openings (m2)
        Real64 ABotGap;
        Real64 ALeftGap; // Area of the left and right openings (m2)
        Real64 ARightGap;
        Real64 AHolesGap; // Area of the holes in the shade (assumed homogeneously
        //   distributed) (m2)
        Real64 ATopLRH; // Intermediate variables
        Real64 ABotLRH;
        Real64 AEqInlet; // Equivalent inlet and outlet opening areas (m2)
        Real64 AEqOutlet;
        Real64 Zinlet; // Inlet and outlet pressure loss factors
        Real64 Zoutlet;
        Real64 AVGap;         // Coeff. of VGap**2 term in pressure balance equation
        Real64 BVGap;         // Coeff. of VGap term in pressure balance equation
        Real64 CVGap;         // VGap-independent term in pressure balance equation
        Real64 GapHeightChar; // Characteristic height of the gap air temperature profile (m)
        Real64 TAve;          // Average of TGlass and TShade (K)
        // REAL(r64)            :: AirProps(8)         ! Air properties
        int TotGaps;              // Glass/glass gaps + glass-shade/blind gap
        Real64 con;               // Gap conductivity and derivative
        Real64 gr;                // glass-shade/blind gap Grashof number
        Real64 pr;                // glass-shade/blind gap Prandtl number
        Real64 nu;                // glass-shade/blind gap Nusselt number
        WinShadingType ShadeFlag; // Shading flag
        int BlNum;                // Blind number

        // Air properties
        //               Dens  dDens/dT  Con    dCon/dT   Vis    dVis/dT Prandtl dPrandtl/dT
        // DATA AirProps / 1.29, -0.4d-2, 2.41d-2, 7.6d-5, 1.73d-5, 1.0d-7, 0.72,   1.8d-3  /

        ConstrNumSh = state.dataSurface->SurfWinActiveShadedConstruction(SurfNum);
        ShadeFlag = state.dataSurface->SurfWinShadingFlag(SurfNum);
        nglassfaces = 2 * state.dataConstruction->Construct(ConstrNumSh).TotGlassLayers;
        TotGaps = state.dataConstruction->Construct(ConstrNumSh).TotGlassLayers;

        if (ANY_INTERIOR_SHADE_BLIND(ShadeFlag)) { // Interior shade or blind
            MatNumSh = state.dataConstruction->Construct(ConstrNumSh).LayerPoint(nglassfaces);
            TGapInlet = state.dataWindowManager->tin;
            TGlassFace = state.dataWindowManager->thetas[nglassfaces - 1];
            TShadeFace = state.dataWindowManager->thetas[nglassfaces];
        } else { // Exterior shade, screen or blind
            MatNumSh = state.dataConstruction->Construct(ConstrNumSh).LayerPoint(1);
            TGapInlet = state.dataWindowManager->tout;
            TGlassFace = state.dataWindowManager->thetas[0];
            TShadeFace = state.dataWindowManager->thetas[nglassfaces + 1];
        }
        TAve = 0.5 * (TGlassFace + TShadeFace);

        if (iter == 0) {
            TGapOld = 0.5 * (TAve + TGapInlet);
        } else {
            TGapOld = TGapNew;
        }

        // Conductance of gap between glass and shade assuming gap is sealed
        WindowGasConductance(state, TGlassFace, TShadeFace, TotGaps, con, pr, gr);
        NusseltNumber(state, SurfNum, TGlassFace, TShadeFace, TotGaps, gr, pr, nu);
        hGapStill = con / state.dataWindowManager->gap[TotGaps - 1] * nu;

        // For near-horizontal windows (i.e., no more than 5 deg from horizontal) assume
        // there is no air flow thru gap

        if (std::abs(state.dataSurface->Surface(SurfNum).SinTilt) < 0.0872) {
            VGap = 0.0;
            hcv = 2.0 * hGapStill;
            QConvGap = 0.0;
            TGapNew = TAve;
            TGapOutlet = TAve;
            return;
        }

        GapHeight = state.dataSurface->Surface(SurfNum).Height;

        if (ShadeFlag == WinShadingType::IntShade || ShadeFlag == WinShadingType::ExtShade || ShadeFlag == WinShadingType::ExtScreen) {
            // Shade or Screen on
            GapDepth = state.dataMaterial->Material(MatNumSh).WinShadeToGlassDist;
            AGap = GapDepth * state.dataSurface->Surface(SurfNum).Width;
            ATopGap = state.dataMaterial->Material(MatNumSh).WinShadeTopOpeningMult * AGap;
            ABotGap = state.dataMaterial->Material(MatNumSh).WinShadeBottomOpeningMult * AGap;
            ALeftGap = state.dataMaterial->Material(MatNumSh).WinShadeLeftOpeningMult * GapHeight * GapDepth;
            ARightGap = state.dataMaterial->Material(MatNumSh).WinShadeRightOpeningMult * GapHeight * GapDepth;
            AHolesGap = state.dataMaterial->Material(MatNumSh).WinShadeAirFlowPermeability * GapHeight * state.dataSurface->Surface(SurfNum).Width;
        } else {
            // Blind on
            BlNum = state.dataSurface->SurfWinBlindNumber(SurfNum);
            GapDepth = state.dataHeatBal->Blind(BlNum).BlindToGlassDist;
            AGap = GapDepth * state.dataSurface->Surface(SurfNum).Width;
            ATopGap = state.dataHeatBal->Blind(BlNum).BlindTopOpeningMult * AGap;
            ABotGap = state.dataHeatBal->Blind(BlNum).BlindBottomOpeningMult * AGap;
            ALeftGap = state.dataHeatBal->Blind(BlNum).BlindLeftOpeningMult * GapHeight * GapDepth;
            ARightGap = state.dataHeatBal->Blind(BlNum).BlindRightOpeningMult * GapHeight * GapDepth;
            AHolesGap = state.dataSurface->SurfWinBlindAirFlowPermeability(SurfNum) * GapHeight * state.dataSurface->Surface(SurfNum).Width;
        }

        RhoAir = state.dataWindowManager->AirProps[0] + state.dataWindowManager->AirProps[1] * (TGapOld - state.dataWindowManager->TKelvin);
        ViscAir = state.dataWindowManager->AirProps[4] + state.dataWindowManager->AirProps[5] * (TGapOld - state.dataWindowManager->TKelvin);
        // The factor 12 in the next line is based on the solution of steady laminar flow between fixed
        // parallel plates given in Sec. 6.9.1 of Fundamentals of Fluid Mechanics, Munson/Young/Okishi, Third Edition
        // Update, John Wiley & Sons, 1998; ISO 15099 has 8 for this factor, which is for flow through a tube.
        BVGap = 12.0 * ViscAir * GapHeight / pow_2(GapDepth);
        // Adding 0.000001 and 0.000002 in the following gives ATopLRH = ABotLRH =
        // 0.25*(ALeftGap + ARightGap + AHolesGap) when ABotGap = ATopGap = 0.0 (shade/blind sealed at
        // bottom and top but possibly open at left side, right side and/or in-shade/blind)
        ATopLRH = 0.5 * ((ATopGap + 0.000001) / (ABotGap + ATopGap + 0.000002)) * (ALeftGap + ARightGap + AHolesGap);
        ABotLRH = 0.5 * ((ABotGap + 0.000001) / (ABotGap + ATopGap + 0.000002)) * (ALeftGap + ARightGap + AHolesGap);
        if (TGapOld > TGapInlet) {
            AEqInlet = ABotGap + ATopLRH;
            AEqOutlet = ATopGap + ABotLRH;
        } else {
            AEqOutlet = ABotGap + ATopLRH;
            AEqInlet = ATopGap + ABotLRH;
        }
        // Adding 0.000001 in the following gives very large value of Zinlet for AEqInlet = 0 and
        // very large value of Zoutlet for AEqInlet = 0; this gives VGap close to zero, as required
        // when there is no inlet and/or outlet for air. This then reduces to the
        // case of a completely sealed shade, in which hcv = 2*hGapStill and QConvGap = 0.
        Zinlet = pow_2(AGap / (0.6 * AEqInlet + 0.000001) - 1.0);
        Zoutlet = pow_2(AGap / (0.6 * AEqOutlet + 0.000001) - 1.0);
        AVGap = 0.5 * RhoAir * (1 + Zinlet + Zoutlet);
        RhoTRef = state.dataWindowManager->AirProps[0] * state.dataWindowManager->TKelvin;
        CVGap = RhoTRef * 9.81 * GapHeight * state.dataSurface->Surface(SurfNum).SinTilt * (TGapOld - TGapInlet) / (TGapOld * TGapInlet);

        // Solution of quadratic equation in VGap
        VGap = (std::sqrt(pow_2(BVGap) + std::abs(4.0 * AVGap * CVGap)) - BVGap) / (2.0 * AVGap);
        hcv = 2.0 * hGapStill + 4.0 * VGap;
        GapHeightChar = RhoAir * 1008.0 * GapDepth * VGap / (2.0 * hcv);
        // The following avoids divide by zero and exponential underflow
        if (GapHeightChar == 0.0) {
            TGapOutlet = TAve;
        } else if ((GapHeight / GapHeightChar) > 15.0) {
            TGapOutlet = TAve;
        } else {
            TGapOutlet = TAve - (TAve - TGapInlet) * std::exp(-GapHeight / GapHeightChar);
        }
        TGapNew = TAve - (GapHeightChar / GapHeight) * (TGapOutlet - TGapInlet);

        // Convective heat flow from gap to room air for interior shade or blind
        if (ANY_INTERIOR_SHADE_BLIND(ShadeFlag)) {
            RhoAir = state.dataWindowManager->AirProps[0] + state.dataWindowManager->AirProps[1] * (TGapNew - state.dataWindowManager->TKelvin);
            QConvGap = RhoAir * AGap * VGap * 1008.0 * (TGapOutlet - TGapInlet);
            // Exclude convection to gap due to divider, if present; divider convection handled
            // separately in CalcWinFrameAndDividerTemps
            QConvGap *= 0.5 * (1.0 + state.dataSurface->Surface(SurfNum).Area /
                                         (state.dataSurface->Surface(SurfNum).Area + state.dataSurface->SurfWinDividerArea(SurfNum)));
        }
    }

    //****************************************************************************

    void BetweenGlassShadeNaturalFlow(EnergyPlusData &state,
                                      int const SurfNum,       // Surface number
                                      int const iter,          // Iteration number for glass heat balance calculation
                                      Real64 &VGap,            // Gas velocity in gaps (m/s)
                                      Array1A<Real64> TGapNew, // Current-iteration average gas temp in gaps (K)
                                      Array1A<Real64> hcv      // Convection coefficient from gap glass or shade to gap gas (W/m2-K)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         F. Winkelmann
        //       DATE WRITTEN   December 2002
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Called by SolveForWindowTemperatures for windows that have a
        // between-glass shade or blind in place.
        // Solves for gas flow in the two gaps on either side of shade/blind.
        // Finds average temperature of gas in the two gaps, and the coefficient
        // for convective heat transfer from glass to gap gas and shade/blind to gap gas
        // for the two gaps. The two gaps are assumed to have the same depth so that the
        // gas velocity due to natural convection is the same in the two gaps.
        // The Between-glass shade/blind is between the two glass layers of double glazing
        // or between the two inner glass layers of triple glazing. The quadruple glazing
        // case is not considered.

        // METHODOLOGY EMPLOYED:
        // Based on ISO/DIS 15099, "Thermal Performance of Windows, Doors and Shading Devices --
        // Detailed Calculations," 1/12/2000, Chapter 7, "Shading Devices."

        // Argument array dimensioning
        TGapNew.dim(2);
        hcv.dim(2);

        int ConstrNumSh; // Shaded construction number
        int MatNumSh;    // Material number of shade/blind layer
        int nglassfaces; // Number of glass faces in contruction
        // In the following, "gaps" refer to the gaps on either side of the shade/blind
        Array1D<Real64> TGlassFace(2); // Temperature of glass surfaces facing gaps (K)
        Array1D<Real64> TShadeFace(2); // Temperature of shade surfaces facing gaps (K)
        Array1D<Real64> hGapStill(2);  // Still-air conduction/convection coeffs for the gaps (W/m2-K)
        Array1D<Real64> TGapOld(2);    // Previous-iteration average gas temp in gaps (K)
        Real64 GapHeight;              // Vertical length of glass-shade/blind gap (m)
        Real64 GapDepth;               // Distance from shade/blind to glass; assumed same for both gaps (m)
        Array1D<Real64> RhoGas(2);     // Density of gap gas at a temperature of TGapOld (kg/m3)
        Real64 RhoTRef;                // Density of gap gas at reference temp = KelvinConvK (kg/m3)
        Array1D<Real64> ViscGas(2);    // Viscosity of gap gas at a temperature of TGapOld (kg/m3)
        Real64 RhoGasZero;             // Gas density at KelvinConvK
        Real64 ViscGasZero;            // Gas viscosity at KelvinConvK (not used)
        Real64 AGap;                   // Cross sectional area of gaps (m2); for vertical window, this
        //   is in horizontal plane normal to window.
        Real64 ATopGap; // Area of the top and bottom openings of shade/blind (m2)
        Real64 ABotGap;
        Real64 ALeftGap; // Area of the left and right openings of shade/blind (m2)
        Real64 ARightGap;
        Real64 AHolesGap; // Area of the holes in the shade/blind (assumed homogeneously
        //   distributed) (m2)
        Real64 ATopLRH; // Intermediate variables
        Real64 ABotLRH;
        Real64 AEqInlet; // Equivalent inlet and outlet opening areas (m2)
        Real64 AEqOutlet;
        Real64 Zinlet; // Inlet and outlet pressure loss factors
        Real64 Zoutlet;
        Real64 AVGap;                     // Coeff. of VGap**2 term in pressure balance equation
        Real64 BVGap;                     // Coeff. of VGap term in pressure balance equation
        Real64 CVGap;                     // VGap-independent term in pressure balance equation
        Array1D<Real64> GapHeightChar(2); // Characteristic height of the gap gas temperature profile (m)
        Array1D<Real64> EpsChar(2);       // EXP(-GapHeight/GapHeightChar(IGap))
        Array1D<Real64> TAve(2);          // Average of TGlass and TShade for the gaps (K)
        Real64 con;                       // Gap gas conductivity and derivative
        Real64 gr;                        // Gap gas Grashof number
        Real64 pr;                        // Gap gas Prandtl number
        Real64 nu;                        // Gap gas Nusselt number
        WinShadingType ShadeFlag;         // Shading flag
        int BlNum;                        // Blind number
        int IGap;                         // Gap counter; 1 = gap on outer side of shade/blind, 2 = gap on inner side.
        int IGapInc;                      // Gap increment (0 or 1)

        ConstrNumSh = state.dataSurface->Surface(SurfNum).activeShadedConstruction;
        ShadeFlag = state.dataSurface->SurfWinShadingFlag(SurfNum);
        nglassfaces = 2 * state.dataConstruction->Construct(ConstrNumSh).TotGlassLayers;

        if (state.dataConstruction->Construct(ConstrNumSh).TotGlassLayers == 2) { // Double glazing
            MatNumSh = state.dataConstruction->Construct(ConstrNumSh).LayerPoint(3);
            IGapInc = 0;
            for (IGap = 1; IGap <= 2; ++IGap) {
                TGlassFace(IGap) = state.dataWindowManager->thetas[IGap];
                TShadeFace(IGap) = state.dataWindowManager->thetas[IGap + 3];
            }
        } else { // Triple glazing
            MatNumSh = state.dataConstruction->Construct(ConstrNumSh).LayerPoint(5);
            IGapInc = 1;
            for (IGap = 1; IGap <= 2; ++IGap) {
                TGlassFace(IGap) = state.dataWindowManager->thetas[IGap + 2];
                TShadeFace(IGap) = state.dataWindowManager->thetas[IGap + 5];
            }
        }

        for (IGap = 1; IGap <= 2; ++IGap) {
            TAve(IGap) = 0.5 * (TGlassFace(IGap) + TShadeFace(IGap));
            if (iter == 0) {
                TGapOld(IGap) = TAve(IGap);
            } else {
                TGapOld(IGap) = TGapNew(IGap);
            }
            // Conductance of gaps on either side of shade/blind assuming gaps are sealed
            WindowGasConductance(state, TGlassFace(IGap), TShadeFace(IGap), IGap + IGapInc, con, pr, gr);
            NusseltNumber(state, SurfNum, TGlassFace(IGap), TShadeFace(IGap), IGap + IGapInc, gr, pr, nu);
            hGapStill(IGap) = con / state.dataWindowManager->gap[IGap + IGapInc - 1] * nu;
        }

        // For near-horizontal windows (i.e., no more than 5 deg from horizontal) assume
        // there is no air flow thru gap

        if (std::abs(state.dataSurface->Surface(SurfNum).SinTilt) < 0.0872) {
            VGap = 0.0;
            for (IGap = 1; IGap <= 2; ++IGap) {
                hcv(IGap) = 2.0 * hGapStill(IGap);
                TGapNew(IGap) = TAve(IGap);
            }
            return;
        }

        GapHeight = state.dataSurface->Surface(SurfNum).Height;
        GapDepth = state.dataWindowManager->gap[IGapInc];
        AGap = GapDepth * state.dataSurface->Surface(SurfNum).Width;

        if (ShadeFlag == WinShadingType::BGShade) {
            // Shade on
            ATopGap = state.dataMaterial->Material(MatNumSh).WinShadeTopOpeningMult * AGap;
            ABotGap = state.dataMaterial->Material(MatNumSh).WinShadeBottomOpeningMult * AGap;
            ALeftGap = state.dataMaterial->Material(MatNumSh).WinShadeLeftOpeningMult * GapHeight * GapDepth;
            ARightGap = state.dataMaterial->Material(MatNumSh).WinShadeRightOpeningMult * GapHeight * GapDepth;
            AHolesGap = state.dataMaterial->Material(MatNumSh).WinShadeAirFlowPermeability * GapHeight * state.dataSurface->Surface(SurfNum).Width;
        } else {
            // Blind on
            BlNum = state.dataSurface->SurfWinBlindNumber(SurfNum);
            ATopGap = state.dataHeatBal->Blind(BlNum).BlindTopOpeningMult * AGap;
            ABotGap = state.dataHeatBal->Blind(BlNum).BlindBottomOpeningMult * AGap;
            ALeftGap = state.dataHeatBal->Blind(BlNum).BlindLeftOpeningMult * GapHeight * GapDepth;
            ARightGap = state.dataHeatBal->Blind(BlNum).BlindRightOpeningMult * GapHeight * GapDepth;
            AHolesGap = state.dataSurface->SurfWinBlindAirFlowPermeability(SurfNum) * GapHeight * state.dataSurface->Surface(SurfNum).Width;
        }

        for (IGap = 1; IGap <= 2; ++IGap) {
            WindowGasPropertiesAtTemp(state, TGapOld(IGap), IGap + IGapInc, RhoGas(IGap), ViscGas(IGap));
        }

        BVGap = 12.0 * (ViscGas(1) + ViscGas(2)) * GapHeight / pow_2(GapDepth);
        // Adding 0.000001 and 0.000002 in the following gives ATopLRH = ABotLRH =
        // 0.25*(ALeftGap + ARightGap + AHolesGap) when ABotGap = ATopGap = 0.0 (shade/blind sealed at
        // bottom and top but possibly open at left side, right side and/or in shade/blind)
        ATopLRH = 0.5 * ((ATopGap + 0.000001) / (ABotGap + ATopGap + 0.000002)) * (ALeftGap + ARightGap + AHolesGap);
        ABotLRH = 0.5 * ((ABotGap + 0.000001) / (ABotGap + ATopGap + 0.000002)) * (ALeftGap + ARightGap + AHolesGap);
        AEqInlet = ABotGap + ATopLRH;
        AEqOutlet = ATopGap + ABotLRH;

        // Adding 0.000001 in the following gives very large value of Zinlet for AEqInlet = 0 and
        // very large value of Zoutlet for AEqInlet = 0; this gives VGap close to zero, as required
        // when there is no inlet and/or outlet for air. This then reduces to the
        // case of a completely sealed shade, in which hcv = 2*hGapStill and QConvGap = 0.
        Zinlet = pow_2(AGap / (0.6 * AEqInlet + 0.000001) - 1.0);
        Zoutlet = pow_2(AGap / (0.6 * AEqOutlet + 0.000001) - 1.0);
        AVGap = 0.5 * (RhoGas(1) + RhoGas(2)) * (1.0 + Zinlet + Zoutlet);
        WindowGasPropertiesAtTemp(state, state.dataWindowManager->TKelvin, 1 + IGapInc, RhoGasZero, ViscGasZero);
        RhoTRef = RhoGasZero * state.dataWindowManager->TKelvin;
        CVGap = RhoTRef * 9.81 * GapHeight * state.dataSurface->Surface(SurfNum).SinTilt * (TGapOld(1) - TGapOld(2)) / (TGapOld(1) * TGapOld(2));

        // Solution of quadratic equation in VGap

        VGap = (std::sqrt(pow_2(BVGap) + std::abs(4 * AVGap * CVGap)) - BVGap) / (2 * AVGap);

        for (IGap = 1; IGap <= 2; ++IGap) {
            hcv(IGap) = 2.0 * hGapStill(IGap) + 4.0 * VGap;
            GapHeightChar(IGap) = RhoGas(IGap) * 1008.0 * GapDepth * VGap / (2.0 * hcv(IGap));
            // The following avoids divide by zero and exponential underflow
            if (GapHeightChar(IGap) == 0.0) {
                EpsChar(IGap) = 0.0;
            } else if ((GapHeight / GapHeightChar(IGap)) > 15.0) {
                EpsChar(IGap) = 0.0;
            } else {
                EpsChar(IGap) = std::exp(-GapHeight / GapHeightChar(IGap));
            }
        }

        TGapNew(1) =
            TAve(1) - (TAve(1) - TAve(2)) * (GapHeightChar(1) / GapHeight) * (1 - EpsChar(1)) * (1 - EpsChar(2)) / (1 - EpsChar(1) * EpsChar(2));

        TGapNew(2) =
            TAve(2) - (TAve(2) - TAve(1)) * (GapHeightChar(2) / GapHeight) * (1 - EpsChar(1)) * (1 - EpsChar(2)) / (1 - EpsChar(1) * EpsChar(2));
    }

    //****************************************************************************

    void BetweenGlassForcedFlow(EnergyPlusData &state,
                                int const SurfNum,  // Surface number
                                int const iter,     // Iteration number for glass heat balance calculation
                                Real64 &VGap,       // Air velocity in airflow gap (m/s)
                                Real64 &TGapNew,    // Current-iteration average air temp in airflow gap (K)
                                Real64 &TGapOutlet, // Temperature of air leaving glass-shade/blind gap at top for upward
                                Real64 &hcv,        // Convection coefficient from gap glass faces to gap air (W/m2-K)
                                Real64 &QConvGap    // Convective heat gain from air flow gap (W)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         F. Winkelmann
        //       DATE WRITTEN   February 2003
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Called by SolveForWindowTemperatures for "airflow windows",i.e., windows
        // with forced airflow in one of the gaps between layers of glass. Based on
        // the velocity of airflow through gap, finds effective temperature of gap air,
        // convective heat transfer coefficient from glass to gap air,
        // the gap outlet temperature, and the outlet convective heat flow.

        // Called only for double and triple glazing. For triple glazing the airflow
        // is assumed to be between the inner two layers of glass (glass layers 2 and 3).

        // METHODOLOGY EMPLOYED:
        // Based on ISO/DIS 15099, "Thermal Performance of Windows, Doors and Shading Devices --
        // Detailed Calculations"

        // Using/Aliasing
        using ScheduleManager::GetCurrentScheduleValue;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        //   air flow or bottom for downward air flow (K)

        int ConstrNum;    // Construction number of surface
        int NGlass;       // Number of glass layers in construction
        int GapNum;       // Number of airflow gap
        Real64 TGapInlet; // Temperature of air entering glass-shade/blind gap at bottom for upward
        //   air flow or top for downward air flow (K)
        Real64 TGlassFace1; // Temperature of left-hand glass surface facing airflow gap (K)
        Real64 TGlassFace2; // Temperature of right-hand glass surface facing airflow gap (K)
        Real64 hGapStill;   // Still-air gap conduction/convection coeff (W/m2-K)
        Real64 TGapOld;     // Previous-iteration average air temp in airflow gap (K)
        Real64 GapHeight;   // Vertical length of airflow gap (m)
        Real64 GapDepth;    // Thickness of airflow gap (m)
        Real64 RhoAir;      // Density of airflow gap air at a temperature of TGapOld (kg/m3)
        Real64 AGap;        // Cross sectional area of airflow gap (m2); for vertical window, this
        //   is in horizontal plane normal to window.
        Real64 GapHeightChar; // Characteristic height of the airflow gap air temperature profile (m)
        Real64 TAve;          // Average of TGlassFace1 and TGlassFace2 (K)
        // REAL(r64)            :: AirProps(8)         ! Air properties
        Real64 con; // Gap conductivity and derivative
        Real64 gr;  // Gap air Grashof number
        Real64 pr;  // Gap air Prandtl number
        Real64 nu;  // Gap air Nusselt number

        // Air properties
        //               Dens  dDens/dT  Con    dCon/dT   Vis    dVis/dT Prandtl dPrandtl/dT
        // DATA AirProps / 1.29, -0.4d-2, 2.41d-2, 7.6d-5, 1.73d-5, 1.0d-7, 0.72,   1.8d-3  /

        ConstrNum = state.dataSurface->Surface(SurfNum).Construction;
        NGlass = state.dataConstruction->Construct(ConstrNum).TotGlassLayers;
        TGlassFace1 = state.dataWindowManager->thetas[2 * NGlass - 3];
        TGlassFace2 = state.dataWindowManager->thetas[2 * NGlass - 2];
        GapNum = NGlass - 1;
        TAve = 0.5 * (TGlassFace1 + TGlassFace2);

        if (state.dataSurface->SurfWinAirflowSource(SurfNum) == AirFlowWindow_Source_IndoorAir) {
            TGapInlet = state.dataWindowManager->tin; // Source is inside air
        } else {
            TGapInlet = state.dataWindowManager->tout; // Source is outside air
        }

        if (iter == 0) {
            TGapOld = 0.5 * (TAve + TGapInlet);
        } else {
            TGapOld = TGapNew;
        }

        // Conductance of gap assuming it is sealed
        WindowGasConductance(state, TGlassFace1, TGlassFace2, GapNum, con, pr, gr);
        NusseltNumber(state, SurfNum, TGlassFace1, TGlassFace2, GapNum, gr, pr, nu);
        hGapStill = con / state.dataWindowManager->gap[GapNum - 1] * nu;
        GapHeight = state.dataSurface->Surface(SurfNum).Height;
        GapDepth = state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(2 * NGlass - 2)).Thickness;
        AGap = GapDepth * state.dataSurface->Surface(SurfNum).Width;
        VGap = state.dataSurface->SurfWinAirflowThisTS(SurfNum) / GapDepth;
        hcv = 2.0 * hGapStill + 4.0 * VGap;
        RhoAir = state.dataWindowManager->AirProps[0] + state.dataWindowManager->AirProps[1] * (TGapOld - state.dataWindowManager->TKelvin);
        GapHeightChar = RhoAir * 1008.0 * GapDepth * VGap / (2.0 * hcv);
        // The following avoids divide by zero and exponential underflow
        if (GapHeightChar == 0.0) {
            TGapOutlet = TAve;
        } else if ((GapHeight / GapHeightChar) > 15.0) {
            TGapOutlet = TAve;
        } else {
            TGapOutlet = TAve - (TAve - TGapInlet) * std::exp(-GapHeight / GapHeightChar);
        }
        TGapNew = TAve - (GapHeightChar / GapHeight) * (TGapOutlet - TGapInlet);
        // Convective heat flow from gap [W]
        RhoAir = state.dataWindowManager->AirProps[0] + state.dataWindowManager->AirProps[1] * (TGapNew - state.dataWindowManager->TKelvin);
        QConvGap = RhoAir * AGap * VGap * 1008.0 * (TGapOutlet - TGapInlet);
    }

    //****************************************************************************

    void BetweenGlassShadeForcedFlow(EnergyPlusData &state,
                                     int const SurfNum,       // Surface number
                                     int const iter,          // Iteration number for glass heat balance calculation
                                     Real64 &VGap,            // Air velocity in each gap (m/s)
                                     Array1A<Real64> TGapNew, // Current-iteration average gas temp in gaps (K)
                                     Real64 &TGapOutletAve,   // Average of TGapOutlet(1) and TGapOutlet(2) (K)
                                     Array1A<Real64> hcv,     // Convection coefficient from gap glass or shade to gap gas (W/m2-K)
                                     Real64 &QConvTot         // Sum of convective heat flow from gaps (W)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         F. Winkelmann
        //       DATE WRITTEN   February 2003
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Called by SolveForWindowTemperatures for airflow windows with a
        // between-glass shade or blind over which fan-forced air flows.
        // Based on the air flow velocity (which is assumed to be the same in the
        // gaps on either side of the shade/blind), finds, for each gap: the average
        // air temperature, the shade/blind or glass surface to air convective heat
        // transfer coefficient, the gap outlet temperature, and the outlet convective heat flow.

        // Called only for double and triple glazing. For triple glazing the airflow
        // is assumed to be between the inner two layers of glass (glass layers 2 and 3),
        // between which the shade/blind is located.

        // METHODOLOGY EMPLOYED:
        // Based on ISO/DIS 15099, "Thermal Performance of Windows, Doors and Shading Devices --
        // Detailed Calculations," 1/12/2000, Chapter 7, "Shading Devices."

        // Argument array dimensioning
        TGapNew.dim(2);
        hcv.dim(2);

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int ConstrNumSh; // Shaded construction number
        int MatNumSh;    // Material number of shade/blind layer
        // In the following, "gaps" refer to the gaps on either side of the shade/blind
        Array1D<Real64> TGlassFace(2); // Temperature of glass surfaces facing gaps (K)
        Array1D<Real64> TShadeFace(2); // Temperature of shade surfaces facing gaps (K)
        Array1D<Real64> hGapStill(2);  // Still-air conduction/convection coeffs for the gaps (W/m2-K)
        Array1D<Real64> TGapOld(2);    // Previous-iteration average gas temp in gaps (K)
        Real64 GapHeight;              // Vertical length of glass-shade/blind gap (m)
        Real64 GapDepth;               // Distance from shade/blind to glass; assumed same for both gaps (m)
        Array1D<Real64> RhoAir(2);     // Density of gap air (kg/m3)
        Real64 AGap;                   // Cross sectional area of each gap (m2); for vertical window, this
        //   is in horizontal plane normal to window.
        Real64 TGapInlet;                 // Gap inlet air temperature (K)
        Array1D<Real64> TGapOutlet(2);    // Gap outlet air temperature (K)
        Array1D<Real64> QConvGap(2);      // Convective heat flow from each gap (W)
        Array1D<Real64> GapHeightChar(2); // Characteristic height of the gap air temperature profile (m)
        Array1D<Real64> TAve(2);          // Average of TGlass and TShade for the gaps (K)
        Real64 con;                       // Gap air conductivity and derivative
        Real64 gr;                        // Gap air Grashof number
        Real64 pr;                        // Gap air Prandtl number
        Real64 nu;                        // Gap air Nusselt number
        WinShadingType ShadeFlag;         // Shading flag
        int IGap;                         // Gap counter; 1 = gap on outer side of shade/blind, 2 = gap on inner side.
        int IGapInc;                      // Gap increment; =0, double glass, =1, triple glass
        // REAL(r64)            :: AirProps(8)         ! Air properties

        // Air properties
        //               Dens  dDens/dT  Con    dCon/dT   Vis    dVis/dT Prandtl dPrandtl/dT
        // DATA AirProps / 1.29, -0.4d-2, 2.41d-2, 7.6d-5, 1.73d-5, 1.0d-7, 0.72,   1.8d-3  /

        ConstrNumSh = state.dataSurface->Surface(SurfNum).activeShadedConstruction;
        ShadeFlag = state.dataSurface->SurfWinShadingFlag(SurfNum);

        if (state.dataConstruction->Construct(ConstrNumSh).TotGlassLayers == 2) { // Double glazing
            MatNumSh = state.dataConstruction->Construct(ConstrNumSh).LayerPoint(3);
            IGapInc = 0;
            for (IGap = 1; IGap <= 2; ++IGap) {
                TGlassFace(IGap) = state.dataWindowManager->thetas[IGap];
                TShadeFace(IGap) = state.dataWindowManager->thetas[IGap + 3];
            }
        } else { // Triple glazing
            MatNumSh = state.dataConstruction->Construct(ConstrNumSh).LayerPoint(5);
            IGapInc = 1;
            for (IGap = 1; IGap <= 2; ++IGap) {
                TGlassFace(IGap) = state.dataWindowManager->thetas[IGap + 2];
                TShadeFace(IGap) = state.dataWindowManager->thetas[IGap + 5];
            }
        }

        if (state.dataSurface->SurfWinAirflowSource(SurfNum) == AirFlowWindow_Source_IndoorAir) {
            TGapInlet = state.dataWindowManager->tin;
        } else {
            TGapInlet = state.dataWindowManager->tout;
        }

        GapHeight = state.dataSurface->Surface(SurfNum).Height;
        GapDepth = state.dataWindowManager->gap[IGapInc];
        AGap = GapDepth * state.dataSurface->Surface(SurfNum).Width;
        // Factor of 2 below assumes gaps on either side of shade/blind have same depth
        VGap = state.dataSurface->SurfWinAirflowThisTS(SurfNum) / (2.0 * GapDepth);

        for (IGap = 1; IGap <= 2; ++IGap) {
            TAve(IGap) = 0.5 * (TGlassFace(IGap) + TShadeFace(IGap));
            if (iter == 0) {
                TGapOld(IGap) = TAve(IGap);
            } else {
                TGapOld(IGap) = TGapNew(IGap);
            }
            // Conductance of gaps on either side of shade/blind assuming gaps are sealed
            WindowGasConductance(state, TGlassFace(IGap), TShadeFace(IGap), IGap + IGapInc, con, pr, gr);
            NusseltNumber(state, SurfNum, TGlassFace(IGap), TShadeFace(IGap), IGap + IGapInc, gr, pr, nu);
            hGapStill(IGap) = con / state.dataWindowManager->gap[IGap + IGapInc - 1] * nu;
            // Shade/blind or glass surface to air convection coefficient
            hcv(IGap) = 2.0 * hGapStill(IGap) + 4.0 * VGap;
            RhoAir(IGap) =
                state.dataWindowManager->AirProps[0] + state.dataWindowManager->AirProps[1] * (TGapOld(IGap) - state.dataWindowManager->TKelvin);
            hcv(IGap) = 2.0 * hGapStill(IGap) + 4.0 * VGap;
            GapHeightChar(IGap) = RhoAir(IGap) * 1008.0 * GapDepth * VGap / (2.0 * hcv(IGap));
            // The following avoids divide by zero and exponential underflow
            if (GapHeightChar(IGap) == 0.0) {
                TGapOutlet(IGap) = TAve(IGap);
            } else if ((GapHeight / GapHeightChar(IGap)) > 15.0) {
                TGapOutlet(IGap) = TAve(IGap);
            } else {
                TGapOutlet(IGap) = TAve(IGap) - (TAve(IGap) - TGapInlet) * std::exp(-GapHeight / GapHeightChar(IGap));
            }
            TGapNew(IGap) = TAve(IGap) - (GapHeightChar(IGap) / GapHeight) * (TGapOutlet(IGap) - TGapInlet);
            // Convective heat flow from gap [W]
            RhoAir(IGap) =
                state.dataWindowManager->AirProps[0] + state.dataWindowManager->AirProps[1] * (TGapNew(IGap) - state.dataWindowManager->TKelvin);
            QConvGap(IGap) = RhoAir(IGap) * AGap * VGap * 1008.0 * (TGapOutlet(IGap) - TGapInlet);
        }

        QConvTot = QConvGap(1) + QConvGap(2);
        TGapOutletAve = 0.5 * (TGapOutlet(1) + TGapOutlet(2));
    }

    //****************************************************************************

    void LUdecomposition(EnergyPlusData &state,
                         Array2<Real64> &ajac, // As input: matrix to be decomposed;
                         int const n,          // Dimension of matrix
                         Array1D_int &indx,    // Vector of row permutations
                         Real64 &d             // +1 if even number of row interchange is even, -1
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         F. Winkelmann, adapted from Numerical Recipes
        //       DATE WRITTEN   February 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Performs LU decomposition of a matrix.

        // SUBROUTINE ARGUMENT DEFINITIONS:
        //   as output: decomposed matrix
        //   if odd

        int i; // Counters
        int j;
        int k;
        int imax; // Temporary variable
        //   as output: decomposed matrix
        auto &vv = state.dataWindowManager->vv;
        Real64 aamax; // Absolute value of largest element of matrix
        Real64 dum;   // Temporary variable
        Real64 sum;   // Sum of products of matrix elements

        assert(n <= 10); // vv sizing

        d = 1.0;
        for (i = 1; i <= n; ++i) {
            aamax = 0.0;
            for (j = 1; j <= n; ++j) {
                if (std::abs(ajac(j, i)) > aamax) aamax = std::abs(ajac(j, i));
            }
            if (aamax == 0.0) ShowFatalError(state, "Singular matrix in LUdecomposition, window calculations");
            vv[i - 1] = 1.0 / aamax;
        }
        for (j = 1; j <= n; ++j) {
            for (i = 1; i <= j - 1; ++i) {
                sum = ajac(j, i);
                for (k = 1; k <= i - 1; ++k) {
                    sum -= ajac(k, i) * ajac(j, k);
                }
                ajac(j, i) = sum;
            }
            aamax = 0.0;
            for (i = j; i <= n; ++i) {
                sum = ajac(j, i);
                for (k = 1; k <= j - 1; ++k) {
                    sum -= ajac(k, i) * ajac(j, k);
                }
                ajac(j, i) = sum;
                dum = vv[i - 1] * std::abs(sum);
                if (dum >= aamax) {
                    imax = i;
                    aamax = dum;
                }
            }
            if (j != imax) {
                for (k = 1; k <= n; ++k) {
                    dum = ajac(k, imax);
                    ajac(k, imax) = ajac(k, j);
                    ajac(k, j) = dum;
                }
                d = -d;
                vv[imax - 1] = vv[j - 1];
            }
            indx(j) = imax;
            if (ajac(j, j) == 0.0) ajac(j, j) = DataGlobalConstants::rTinyValue;
            if (j != n) {
                dum = 1.0 / ajac(j, j);
                for (i = j + 1; i <= n; ++i) {
                    ajac(j, i) *= dum;
                }
            }
        }
    }

    //**************************************************************************

    void LUsolution(Array2<Real64> const &a, // Matrix and vector in a.x = b;
                    int const n,             // Dimension of a and b
                    Array1D_int const &indx, // Vector of row permutations
                    Array1D<Real64> &b       // Matrix and vector in a.x = b;
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         F. Winkelmann, adapted from Numerical Recipes
        //       DATE WRITTEN   February 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Solves set of linear equations a.x = b

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        //   b is also output as the solution, x
        //   b is also output as the solution, x

        int i; // Counters
        int j;
        int ii; // Intermediate variables
        int ll;
        Real64 sum; // Summation variable

        ii = 0;
        for (i = 1; i <= n; ++i) {
            ll = indx(i);
            sum = b(ll);
            b(ll) = b(i);
            if (ii != 0) {
                for (j = ii; j <= i - 1; ++j) {
                    sum -= a(j, i) * b(j);
                }
            } else if (sum != 0.0) {
                ii = i;
            }
            b(i) = sum;
        }
        for (i = n; i >= 1; --i) {
            sum = b(i);
            for (j = i + 1; j <= n; ++j) {
                sum -= a(j, i) * b(j);
            }
            b(i) = sum / a(i, i);
        }
    }

    //******************************************************************************

    void WindowGasConductance(EnergyPlusData &state,
                              Real64 const tleft,  // Temperature of gap surface closest to outside (K)
                              Real64 const tright, // Temperature of gap surface closest to zone (K)
                              int const IGap,      // Gap number
                              Real64 &con,         // Gap gas conductance (W/m2-K)
                              Real64 &pr,          // Gap gas Prandtl number
                              Real64 &gr           // Gap gas Grashof number
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Adapted by Fred Winkelmann from Window5 subroutine gasses
        //       DATE WRITTEN   September 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Find the coefficient of convective/conductive heat transfer in the gas-filled gap
        // between isothermal solid layers. The gap may be filled with a single gas or a gas mixture.

        // METHODOLOGY EMPLOYED:
        // Based on methodology in Chapter 5 of the July 18, 2001 draft of ISO 15099,
        // "Thermal Performance of Windows, Doors and Shading Devices--Detailed Calculations."
        // The equation numbers below correspond to those in the standard.

        // REFERENCES:
        // Window5 source code; ISO 15099

        constexpr Real64 pres(1.0e5);     // Gap gas pressure (Pa)
        constexpr Real64 gaslaw(8314.51); // Molar gas constant (J/kMol-K)
        Real64 const two_sqrt_2(2.0 * std::sqrt(2.0));

        int IMix; // Counters of gases in a mixture
        int i;
        int j;
        int NMix;      // Number of gases in a mixture
        Real64 molmix; // Molecular weight of mixture

        auto &kprime = state.dataWindowManager->kprime;
        auto &kdblprm = state.dataWindowManager->kdblprm;
        auto &mukpdwn = state.dataWindowManager->mukpdwn;
        auto &kpdown = state.dataWindowManager->kpdown;
        auto &kdpdown = state.dataWindowManager->kdpdown;
        auto &frct = state.dataWindowManager->frct;
        auto &fvis = state.dataWindowManager->fvis;
        auto &fcon = state.dataWindowManager->fcon;
        auto &fdens = state.dataWindowManager->fdens;
        auto &fcp = state.dataWindowManager->fcp;

        Real64 kpmix; // Monotonic thermal conductivity of mixture
        Real64 kdpmix;
        Real64 kmix;      // For accumulating conductance of gas mixture
        Real64 mumix;     // For accumulating viscosity of gas mixture
        Real64 visc(0.0); // Dynamic viscosity of mixture at tmean (g/m-s)
        Real64 cp(0.0);   // Specific heat of mixture at tmean (J/m3-K)
        Real64 dens(0.0); // Density of mixture at tmean (kg/m3)
        Real64 cpmixm;    // Gives cp when divided by molmix
        Real64 phimup;    // Numerator factor
        Real64 downer;    // Denominator factor
        Real64 psiup;     // Numerator factor
        Real64 psiterm;   // Factor
        Real64 phikup;    // Numerator factor
        Real64 rhomix;    // Density of gas mixture (kg/m3)

        // Autodesk:Logic Either assert NMix>0 or handle NMix<=0 in logic so that con and locals guar. initialized before use
        NMix = state.dataWindowManager->gnmix[IGap - 1];

        for (IMix = 1; IMix <= NMix; ++IMix) {
            frct[IMix - 1] = state.dataWindowManager->gfract[IMix - 1][IGap - 1];
        }

        Real64 const tmean(0.5 * (tleft + tright)); // Average gap gas temperature (K)
        Real64 const tmean_2(pow_2(tmean));

        fcon[0] = state.dataWindowManager->gcon[0][0][IGap - 1] + state.dataWindowManager->gcon[1][0][IGap - 1] * tmean +
                  state.dataWindowManager->gcon[2][0][IGap - 1] * tmean_2;
        fvis[0] = state.dataWindowManager->gvis[0][0][IGap - 1] + state.dataWindowManager->gvis[1][0][IGap - 1] * tmean +
                  state.dataWindowManager->gvis[2][0][IGap - 1] * tmean_2;
        fcp[0] = state.dataWindowManager->gcp[0][0][IGap - 1] + state.dataWindowManager->gcp[1][0][IGap - 1] * tmean +
                 state.dataWindowManager->gcp[2][0][IGap - 1] * tmean_2;
        fdens[0] = pres * state.dataWindowManager->gwght[0][IGap - 1] / (gaslaw * tmean); // Density using ideal gas law:
        //  rho=(presure*molecweight)/(gasconst*tmean)

        if (NMix == 1) { // Single gas
            con = fcon[0];
            visc = fvis[0];
            cp = fcp[0];
            dens = fdens[0];
        } else if (NMix > 1) {                                                                   // Multiple gases; calculate mixture properties
            molmix = frct[0] * state.dataWindowManager->gwght[0][IGap - 1];                      // initialize eq. 56
            cpmixm = molmix * fcp[0];                                                            // initialize eq. 58
            kprime[0] = 3.75 * (gaslaw / state.dataWindowManager->gwght[0][IGap - 1]) * fvis[0]; // eq. 67
            kdblprm[0] = fcon[0] - kprime[0];                                                    // eq. 67

            // Initialize summations for eqns 60-66
            mumix = 0.0;
            kpmix = 0.0;
            kdpmix = 0.0;
            mukpdwn[0] = 1.0;
            kpdown[0] = 1.0;
            kdpdown[0] = 1.0;

            // Calculate properties of mixture constituents
            for (i = 2; i <= NMix; ++i) {
                fcon[i - 1] = state.dataWindowManager->gcon[0][i - 1][IGap - 1] + state.dataWindowManager->gcon[1][i - 1][IGap - 1] * tmean +
                              state.dataWindowManager->gcon[2][i - 1][IGap - 1] * tmean_2;
                fvis[i - 1] = state.dataWindowManager->gvis[0][i - 1][IGap - 1] + state.dataWindowManager->gvis[1][i - 1][IGap - 1] * tmean +
                              state.dataWindowManager->gvis[2][i - 1][IGap - 1] * tmean_2;
                fcp[i - 1] = state.dataWindowManager->gcp[0][i - 1][IGap - 1] + state.dataWindowManager->gcp[1][i - 1][IGap - 1] * tmean +
                             state.dataWindowManager->gcp[2][i - 1][IGap - 1] * tmean_2;
                fdens[i - 1] = pres * state.dataWindowManager->gwght[i - 1][IGap - 1] / (gaslaw * tmean);
                molmix += frct[i - 1] * state.dataWindowManager->gwght[i - 1][IGap - 1];                       // eq. 56
                cpmixm += frct[i - 1] * fcp[i - 1] * state.dataWindowManager->gwght[i - 1][IGap - 1];          // eq. 58-59
                kprime[i - 1] = 3.75 * gaslaw / state.dataWindowManager->gwght[i - 1][IGap - 1] * fvis[i - 1]; // eq. 67
                kdblprm[i - 1] = fcon[i - 1] - kprime[i - 1];                                                  // eq. 68
                mukpdwn[i - 1] = 1.0;                                                                          // initialize denomonator of eq. 60
                kpdown[i - 1] = 1.0;                                                                           // initialize denomonator of eq. 63
                kdpdown[i - 1] = 1.0;                                                                          // initialize denomonator of eq. 65
            }

            for (i = 1; i <= NMix; ++i) {
                for (j = 1; j <= NMix; ++j) {
                    // numerator of equation 61
                    phimup = pow_2(1.0 + std::sqrt(fvis[i - 1] / fvis[j - 1]) * root_4(state.dataWindowManager->gwght[j - 1][IGap - 1] /
                                                                                       state.dataWindowManager->gwght[i - 1][IGap - 1]));
                    // denomonator of eq. 61, 64 and 66
                    downer = two_sqrt_2 *
                             std::sqrt(1 + (state.dataWindowManager->gwght[i - 1][IGap - 1] / state.dataWindowManager->gwght[j - 1][IGap - 1]));
                    // calculate the denominator of eq. 60
                    if (i != j) mukpdwn[i - 1] += phimup / downer * frct[j - 1] / frct[i - 1];
                    // numerator of eq. 64; psiterm is the multiplied term in backets
                    psiup = pow_2(1.0 + std::sqrt(kprime[i - 1] / kprime[j - 1]) * root_4(state.dataWindowManager->gwght[i - 1][IGap - 1] /
                                                                                          state.dataWindowManager->gwght[j - 1][IGap - 1]));
                    psiterm = 1.0 + 2.41 * (state.dataWindowManager->gwght[i - 1][IGap - 1] - state.dataWindowManager->gwght[j - 1][IGap - 1]) *
                                        (state.dataWindowManager->gwght[i - 1][IGap - 1] - 0.142 * state.dataWindowManager->gwght[j - 1][IGap - 1]) /
                                        pow_2(state.dataWindowManager->gwght[i - 1][IGap - 1] + state.dataWindowManager->gwght[j - 1][IGap - 1]);
                    // using the common denominator, downer, calculate the denominator for eq. 63
                    if (i != j) kpdown[i - 1] += psiup * (psiterm / downer) * (frct[j - 1] / frct[i - 1]);
                    // calculate the numerator of eq. 66
                    phikup = pow_2(1.0 + std::sqrt(kprime[i - 1] / kprime[j - 1]) * root_4(state.dataWindowManager->gwght[i - 1][IGap - 1] /
                                                                                           state.dataWindowManager->gwght[j - 1][IGap - 1]));
                    // using the common denominator, downer, calculate the denomonator for eq. 65
                    if (i != j) kdpdown[i - 1] += (phikup / downer) * (frct[j - 1] / frct[i - 1]);
                }
                mumix += fvis[i - 1] / mukpdwn[i - 1];     // eq. 60
                kpmix += kprime[i - 1] / kpdown[i - 1];    // eq. 63
                kdpmix += kdblprm[i - 1] / kdpdown[i - 1]; // eq. 65
            }

            // Calculate the density of the mixture assuming an ideal gas
            rhomix = pres * molmix / (gaslaw * tmean); // eq. 57
            kmix = kpmix + kdpmix;                     // eq. 68-a

            // Final mixture properties
            visc = mumix;
            con = kmix;
            dens = rhomix;
            cp = cpmixm / molmix;

        } else {
            assert(false);
        } // End of check if single or multiple gases in gap

        pr = cp * visc / con;
        gr = 9.807 * pow_3(state.dataWindowManager->gap[IGap - 1]) * std::abs(tleft - tright) * pow_2(dens) / (tmean * pow_2(visc));
    }

    //******************************************************************************

    void WindowGasPropertiesAtTemp(EnergyPlusData &state,
                                   Real64 const tmean, // Temperature of gas in gap (K)
                                   int const IGap,     // Gap number
                                   Real64 &dens,       // Gap gas density at tmean (kg/m3)
                                   Real64 &visc        // Gap gas dynamic viscosity at tmean (g/m-s)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         F. Winkelmann
        //       DATE WRITTEN   December 2002
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Finds the density and viscosity of the gas in a gap at a particular temperature.
        // The gap may be filled with a single gas or a gas mixture.
        // Based on Subroutine WindowGasConductance.

        // METHODOLOGY EMPLOYED:
        // See Subr. WindowGasConductance

        // REFERENCES:
        // See Subr. WindowGasConductance

        Real64 constexpr pres(1.0e5);     // Gap gas pressure (Pa)
        Real64 constexpr gaslaw(8314.51); // Molar gas constant (J/kMol-K)
        Real64 const two_sqrt_2(2.0 * std::sqrt(2.0));

        int IMix; // Counters of gases in a mixture
        int i;
        int j;
        int NMix;                    // Number of gases in a mixture
        Real64 molmix;               // Molecular weight of mixture
        Array1D<Real64> mukpdwn(10); // Denominator term
        Real64 mumix;                // For accumulating viscosity of gas mixture
        Real64 phimup;               // Numerator factor
        Real64 downer;               // Denominator factor
        Real64 rhomix;               // Density of gas mixture (kg/m3)
        Array1D<Real64> frct(10);    // Fraction of each gas in a mixture
        Array1D<Real64> fvis(10);    // Viscosity of each gas in a mixture (g/m-s)
        Array1D<Real64> fdens(10);   // Density of each gas in a mixture (kg/m3)

        NMix = state.dataWindowManager->gnmix[IGap - 1];

        for (IMix = 1; IMix <= NMix; ++IMix) {
            frct(IMix) = state.dataWindowManager->gfract[IMix - 1][IGap - 1];
        }

        Real64 const tmean_2(pow_2(tmean));
        fvis(1) = state.dataWindowManager->gvis[0][0][IGap - 1] + state.dataWindowManager->gvis[1][0][IGap - 1] * tmean +
                  state.dataWindowManager->gvis[2][0][IGap - 1] * tmean_2;
        fdens(1) = pres * state.dataWindowManager->gwght[0][IGap - 1] / (gaslaw * tmean); // Density using ideal gas law:
        //  rho=(presure*molecweight)/(gasconst*tmean)
        if (NMix == 1) { // Single gas
            visc = fvis(1);
            dens = fdens(1);
        } else {                                                            // Multiple gases; calculate mixture properties
            molmix = frct(1) * state.dataWindowManager->gwght[0][IGap - 1]; // initialize eq. 56

            // Initialize summations for eqns 60-66
            mumix = 0.0;
            mukpdwn(1) = 1.0;

            // Calculate properties of mixture constituents
            for (i = 2; i <= NMix; ++i) {
                fvis(i) = state.dataWindowManager->gvis[0][i - 1][IGap - 1] + state.dataWindowManager->gvis[1][i - 1][IGap - 1] * tmean +
                          state.dataWindowManager->gvis[2][i - 1][IGap - 1] * tmean_2;
                fdens(i) = pres * state.dataWindowManager->gwght[i - 1][IGap - 1] / (gaslaw * tmean);
                molmix += frct(i) * state.dataWindowManager->gwght[i - 1][IGap - 1]; // eq. 56
                mukpdwn(i) = 1.0;                                                    // initialize denomonator of eq. 60
            }

            for (i = 1; i <= NMix; ++i) {
                for (j = 1; j <= NMix; ++j) {
                    // numerator of equation 61
                    phimup = pow_2(1.0 + std::sqrt(fvis(i) / fvis(j)) * root_4(state.dataWindowManager->gwght[j - 1][IGap - 1] /
                                                                               state.dataWindowManager->gwght[i - 1][IGap - 1]));
                    // denomonator of eq. 61, 64 and 66
                    downer = two_sqrt_2 *
                             std::sqrt(1 + (state.dataWindowManager->gwght[i - 1][IGap - 1] / state.dataWindowManager->gwght[j - 1][IGap - 1]));
                    // calculate the denominator of eq. 60
                    if (i != j) mukpdwn(i) += phimup / downer * frct(j) / frct(i);
                }
                mumix += fvis(i) / mukpdwn(i); // eq. 60
            }

            // Calculate the density of the mixture assuming an ideal gas
            rhomix = pres * molmix / (gaslaw * tmean); // eq. 57

            // Final mixture properties
            visc = mumix;
            dens = rhomix;

        } // End of check if single or multiple gases in gap
    }

    //********************************************************************************

    void StartingWindowTemps(EnergyPlusData &state,
                             int const SurfNum,          // Surface number
                             Array1A<Real64> AbsRadShade // Short-wave radiation absorbed by shade/blind faces
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         F. Winkelmann
        //       DATE WRITTEN   January 2000
        //       MODIFIED       March 2003, FW: add rough calc of increase above ambient of
        //                        initial shade/blind temperature when shade/blind deployed
        //                        after having been off.
        //                      Jan 2004, FW: take into account whether storm window was added
        //                        or removed in the current time step.
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Initializes face temperature distribution prior to iteration

        // Argument array dimensioning
        AbsRadShade.dim(2);

        constexpr Real64 hrad(5.3);    // Typical radiative conductance (W/m2-K)
        constexpr Real64 resgap(0.21); // Typical gap resistance (m2-K/W)

        int i;                      // Face counter
        WinShadingType ShadeFlag;   // Shading flag
        Array1D<Real64> rguess(11); // Combined radiative/convective resistance (m2-K/W) of
        // inside or outside air film, or gap
        Real64 restot; // Total window resistance including outside
        //   and inside air films (m2-K/W)
        Real64 temdiff;          // Inside/outside air temperature difference (K)
        Real64 ressum;           // Resistance sum (m2-K/W)
        int StormWinFlagPrevDay; // Previous time step value (day) of storm window flag
        int StormWinFlagThisDay; // Current time step value (day) of storm window flag
        int nglfacePrevDay;      // Previous time step value (dya) of number of glass faces (may differ
        //   current time step value, nglface, if storm window was
        //   added or removed during the current time step).

        StormWinFlagPrevDay = state.dataSurface->SurfWinStormWinFlagPrevDay(SurfNum);
        StormWinFlagThisDay = state.dataSurface->SurfWinStormWinFlag(SurfNum);

        if (state.dataGlobal->BeginEnvrnFlag || (StormWinFlagThisDay != StormWinFlagPrevDay)) {

            // Guess values of glass face temperatures based on a simple resistance-network solution
            // that (1) ignores short- and long-wave radiation (from lights and zone equipment) absorbed
            // by the glass faces, and (2) assumes zero glass resistance. Absorbed solar is also ignored
            // since the tests on BeginEnvrnFlag and storm window transition can be true only at midnight.
            // Interaction with shade or blind, if one of these is present, is ignored. See below for
            // separate calculation of shade/blind temperature.

            rguess(1) = 1.0 / (state.dataWindowManager->hcout + hrad);
            rguess(state.dataWindowManager->nglface + 1) = 1.0 / (state.dataWindowManager->hcin + hrad);

            for (i = 2; i <= state.dataWindowManager->nglface; i += 2) {
                rguess(i) = 1.0 / state.dataWindowManager->scon[i / 2 - 1];
                if (i < state.dataWindowManager->nglface) rguess(i + 1) = resgap;
            }

            restot = 0.0;
            for (i = 1; i <= state.dataWindowManager->nglface + 1; ++i) {
                restot += rguess(i);
            }

            temdiff = state.dataWindowManager->tin - state.dataWindowManager->tout;
            if (std::abs(temdiff) < 0.5) temdiff = 2.0;

            ressum = 0.0;
            for (i = 1; i <= state.dataWindowManager->nglface; ++i) {
                ressum += rguess(i);
                state.dataWindowManager->thetas[i - 1] = (ressum / restot) * temdiff + state.dataWindowManager->tout;
            }

        } else {
            // Use previous time step values
            for (i = 1; i <= state.dataWindowManager->nglface; ++i) {
                state.dataWindowManager->thetas[i - 1] = state.dataSurface->SurfaceWindow(SurfNum).ThetaFace(i);
            }
        }

        // Initialize face temperatures of shade or blind, if present

        ShadeFlag = state.dataSurface->SurfWinShadingFlag(SurfNum);
        if (ANY_INTERIOR_SHADE_BLIND(state.dataSurface->SurfWinExtIntShadePrevTS(SurfNum)) ||
            state.dataSurface->SurfWinExtIntShadePrevTS(SurfNum) == WinShadingType::ExtShade ||
            state.dataSurface->SurfWinExtIntShadePrevTS(SurfNum) == WinShadingType::ExtBlind ||
            ANY_BETWEENGLASS_SHADE_BLIND(state.dataSurface->SurfWinExtIntShadePrevTS(SurfNum))) {
            // Shade or blind is on during the previous TS; use previous-TS values of shade/blind face temps.
            // Note that if shade or blind is NOT on in the current TS the following two
            // temperature values, although calculated here, are not used. The shade/blind face numbers
            // during the previous time step depend on whether a storm window glass layer was added to
            // or removed from the window during the current time step.
            nglfacePrevDay = state.dataWindowManager->nglface;
            if (StormWinFlagPrevDay == 0 && StormWinFlagThisDay == 1) nglfacePrevDay = state.dataWindowManager->nglface - 2;
            if (StormWinFlagPrevDay == 1 && StormWinFlagThisDay == 0) nglfacePrevDay = state.dataWindowManager->nglface + 2;
            state.dataWindowManager->thetas[state.dataWindowManager->nglface] =
                state.dataSurface->SurfaceWindow(SurfNum).ThetaFace(nglfacePrevDay + 1);
            state.dataWindowManager->thetas[state.dataWindowManager->nglface + 1] =
                state.dataSurface->SurfaceWindow(SurfNum).ThetaFace(nglfacePrevDay + 2);
        } else {
            // No shade or blind previous time step; guess starting values of shade/blind
            // taking into account short- and long-wave radiation (from solar, lights and zone equipment)
            // absorbed by shade/blind faces. Face temps are assumed to be the same and
            // equal to shade/blind temp. For interior shade/blind, air temp on either side is
            // assumed to be the same and equal to tin; for exterior blind it is assumed to be
            // equal to tout. For between-glass shade/blind it is assumed to be equal to the
            // average temperature of the adjacent glass faces.

            if (ANY_INTERIOR_SHADE_BLIND(ShadeFlag)) {
                state.dataWindowManager->thetas[state.dataWindowManager->nglface] =
                    state.dataWindowManager->tin + (AbsRadShade(1) + AbsRadShade(2)) / (2 * (state.dataWindowManager->hcin + hrad));
                state.dataWindowManager->thetas[state.dataWindowManager->nglface + 1] =
                    state.dataWindowManager->thetas[state.dataWindowManager->nglface];
            } else if (ShadeFlag == WinShadingType::ExtShade || ShadeFlag == WinShadingType::ExtBlind) {
                state.dataWindowManager->thetas[state.dataWindowManager->nglface] =
                    state.dataWindowManager->tout + (AbsRadShade(1) + AbsRadShade(2)) / (2 * (state.dataWindowManager->hcout + hrad));
                state.dataWindowManager->thetas[state.dataWindowManager->nglface + 1] =
                    state.dataWindowManager->thetas[state.dataWindowManager->nglface];
            } else if (ANY_BETWEENGLASS_SHADE_BLIND(ShadeFlag)) {
                // Between-glass shade/blind allowed only for double and triple glazing.
                // The factor 16.0 below is based on a combined convective/radiative heat transfer
                // coefficient on either side of the shade/blind of 8.0 W/m2-K -- about 1.4 Btu/h-ft2-F.
                if (state.dataWindowManager->nglface == 4) { // double glazing
                    state.dataWindowManager->thetas[state.dataWindowManager->nglface] =
                        0.5 * (state.dataWindowManager->thetas[1] + state.dataWindowManager->thetas[2]) + (AbsRadShade(1) + AbsRadShade(2)) / 16.0;
                    state.dataWindowManager->thetas[state.dataWindowManager->nglface + 1] =
                        state.dataWindowManager->thetas[state.dataWindowManager->nglface];
                } else { // triple glazing
                    state.dataWindowManager->thetas[state.dataWindowManager->nglface] =
                        0.5 * (state.dataWindowManager->thetas[3] + state.dataWindowManager->thetas[4]) + (AbsRadShade(1) + AbsRadShade(2)) / 16.0;
                    state.dataWindowManager->thetas[state.dataWindowManager->nglface + 1] =
                        state.dataWindowManager->thetas[state.dataWindowManager->nglface];
                }
            }
        }
    }

    //****************************************************************************

    void NusseltNumber(EnergyPlusData &state,
                       int const SurfNum, // Surface number
                       Real64 const tso,  // Temperature of gap surface closest to outside (K)
                       Real64 const tsi,  // Temperature of gap surface closest to zone (K)
                       int const IGap,    // Gap number
                       Real64 const gr,   // Gap gas Grashof number
                       Real64 const pr,   // Gap gas Prandtl number
                       Real64 &gnu        // Gap gas Nusselt number
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Adapted by Fred Winkelmann from Window5 subroutine nusselt
        //       DATE WRITTEN   September 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Finds the Nusselt number for gas-filled gaps between isothermal solid layers.
        // The gap may be filled with a single gas or a gas mixture.

        // METHODOLOGY EMPLOYED:
        // Based on methodology in Chapter 5 of the July 18, 2001 draft of ISO 15099,
        // "Thermal Performance of Windows, Doors and Shading Devices--Detailed Calculations."
        // The equation numbers below correspond to those in the standard.

        // REFERENCES:
        // Window5 source code; ISO 15099

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 asp;    // Aspect ratio: window height to gap width
        Real64 ra;     // Rayleigh number
        Real64 gnu901; // Nusselt number temporary variables for
        Real64 gnu902;
        Real64 gnu90;
        Real64 gnu601;
        Real64 gnu602; // different tilt and Ra ranges
        Real64 gnu60;
        Real64 gnu601a;
        Real64 gnua;
        Real64 gnub;
        Real64 cra; // Temporary variables
        Real64 a;
        Real64 b;
        Real64 g;
        Real64 ang;

        if (SurfNum > 0) {
            asp = state.dataSurface->Surface(SurfNum).Height / state.dataWindowManager->gap[IGap - 1];
        } else { // SurfNum = 0 when NusseltNumber is called from CalcNominalWindowCond, which applies to a
            // particular construction. So window height is not known and we assume 5 ft (1.524 m)
            asp = 1.524 / state.dataWindowManager->gap[IGap - 1];
        }

        state.dataWindowManager->tiltr = state.dataWindowManager->tilt * DataGlobalConstants::DegToRadians;
        ra = gr * pr;
        //! fw if (ra > 2.0e6): error that outside range of Rayleigh number?

        if (ra <= 1.0e4) gnu901 = 1.0 + 1.7596678e-10 * std::pow(ra, 2.2984755); // eq. 51
        if (ra > 1.0e4 && ra <= 5.0e4) gnu901 = 0.028154 * std::pow(ra, 0.4134); // eq. 50
        if (ra > 5.0e4) gnu901 = 0.0673838 * std::pow(ra, 1.0 / 3.0);            // eq. 49

        gnu902 = 0.242 * std::pow(ra / asp, 0.272); // eq. 52
        gnu90 = max(gnu901, gnu902);

        if (tso > tsi) {                                                          // window heated from above
            gnu = 1.0 + (gnu90 - 1.0) * std::sin(state.dataWindowManager->tiltr); // eq. 53
        } else {                                                                  // window heated from below
            if (state.dataWindowManager->tilt >= 60.0) {
                if (ra >= 0.001) {
                    g = 0.5 * std::pow(1.0 + std::pow(ra / 3160.0, 20.6), -0.1); // eq. 47
                } else {
                    g = 0.5;
                }
                gnu601a = 1.0 + pow_7(0.0936 * std::pow(ra, 0.314) / (1.0 + g)); // eq. 45
                gnu601 = std::pow(gnu601a, 0.142857);

                // For any aspect ratio
                gnu602 = (0.104 + 0.175 / asp) * std::pow(ra, 0.283); // eq. 46
                gnu60 = max(gnu601, gnu602);

                // linear interpolation for layers inclined at angles between 60 and 90 deg
                gnu = ((90.0 - state.dataWindowManager->tilt) * gnu60 + (state.dataWindowManager->tilt - 60.0) * gnu90) / 30.0;
            }
            if (state.dataWindowManager->tilt < 60.0) { // eq. 42
                cra = ra * std::cos(state.dataWindowManager->tiltr);
                a = 1.0 - 1708.0 / cra;
                b = std::pow(cra / 5830.0, 0.33333) - 1.0;
                gnua = (std::abs(a) + a) / 2.0;
                gnub = (std::abs(b) + b) / 2.0;
                ang = 1708.0 * std::pow(std::sin(1.8 * state.dataWindowManager->tiltr), 1.6);
                gnu = 1.0 + 1.44 * gnua * (1.0 - ang / cra) + gnub;
            }
        }
    }

    //*******************************************************************************************************

    void TransAndReflAtPhi(Real64 const cs,                // Cosine of incidence angle
                           Real64 const tf0,               // Transmittance at zero incidence angle
                           Real64 const rf0,               // Front reflectance at zero incidence angle
                           Real64 const rb0,               // Back reflectance at zero incidence angle
                           Real64 &tfp,                    // Transmittance at cs
                           Real64 &rfp,                    // Front reflectance at cs
                           Real64 &rbp,                    // Back reflectance at cs
                           bool const SimpleGlazingSystem, // .TRUE. if simple block model being used
                           Real64 const SimpleGlazingSHGC, // SHGC value to use in alternate model for simple glazing system
                           Real64 const SimpleGlazingU     // U-factor value to use in alternate model for simple glazing system
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         F. Winkelmann
        //       DATE WRITTEN   January 2000
        //       MODIFIED       5 June 2003, FCW: modify to correspond to WINDOW 4 and WINDOW 5.
        //                      Original routine was based on the method in E.U. Finlayson et al,
        //                      "WINDOW 4.0: Documentation of Calculation Procedures," LBL-33943,
        //                      July 1993, which is not used in either WINDOW 4 or WINDOW 5.
        //                      The current routine is based on ASHRAE Handbook of Fundamentals,
        //                      2001, pp. 30.20-23, "Optical Properties of Single Glazing Layers."
        //                      Original routine underpredicted transmittance at angles of
        //                      incidence > 60 degrees.
        //                      June 2009.  Brent Griffith.  add simple window correlation
        //                                   newer model from LBNL windows group 5/15/2009
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // For a single glazing layer, calculate transmittance and reflectance at an arbitrary
        // angle of incidence given transmittance and reflectance at zero incidence angle.

        // REFERENCES:
        // ASHRAE Handbook of Fundamentals, 2001, pp. 30.20-23,
        // "Optical Properties of Single Glazing Layers."

        Real64 tfp1; // Transmittance at cs for each polarization
        Real64 tfp2;
        Real64 rfp1; // Front reflectance at cs for each polarization
        Real64 rfp2;
        Real64 rbp1; // Back reflectance at cs for each polarization
        Real64 rbp2;
        Real64 betaf; // Intermediate variables
        Real64 betab;
        Real64 r0f;
        Real64 r0b;
        Real64 abf;
        Real64 abb;
        Real64 ngf; // Front and back index of refraction
        Real64 ngb;
        Real64 cgf; // Intermediate variables
        Real64 cgb;
        Real64 rpf1; // Front and back air/glass interface reflectivity
        Real64 rpb1;
        Real64 tpf1;
        Real64 tpb1;
        //  and transmittivity for first polarization
        Real64 rpf2; // Front and back air/glass interface reflectivity
        Real64 rpb2;
        Real64 tpf2;
        Real64 tpb2;
        //  and transmittivity for second polarization
        Real64 tcl; // Transmittance and reflectance for clear glass
        Real64 rcl;
        Real64 tbnz; // Transmittance and reflectance for bronze glass
        Real64 rbnz;
        Real64 expmabfdivcgf;
        Real64 expm2abfdivcgf;
        Real64 expmabbdivcgb;

        Real64 testval; // temporary value for calculations
        Real64 tmp1;    // temporary value for calculations
        Real64 tmp2;    // temporary value for calculations
        Real64 tmp3;    // temporary value for calculations
        Real64 tmp4;    // temporary value for calculations
        Real64 tmp5;    // temporary value for calculations
        Real64 tmp6;    // temporary value for calculations
        Real64 tmp7;    // temporary value for calculations
        Real64 tmp8;    // temporary value for calculations
        Real64 tmp9;    // temporary value for calculations

        if (SimpleGlazingSystem) { // use alternate angular dependence model for block model of simple glazing input

            Real64 const cs_2(pow_2(cs));
            Real64 const cs_3(pow_3(cs));
            Real64 const cs_4(pow_4(cs));
            Real64 TransCurveA = 0.00 + 3.36 * cs - 3.85 * cs_2 + 1.49 * cs_3 + 0.01 * cs_4;
            Real64 TransCurveB = 0.00 + 2.83 * cs - 2.42 * cs_2 + 0.04 * cs_3 + 0.55 * cs_4;
            Real64 TransCurveC = 0.00 + 2.45 * cs - 1.58 * cs_2 - 0.64 * cs_3 + 0.77 * cs_4;
            Real64 TransCurveD = 0.00 + 2.85 * cs - 2.58 * cs_2 + 0.40 * cs_3 + 0.35 * cs_4;
            Real64 TransCurveE = 0.00 + 1.51 * cs + 2.49 * cs_2 - 5.87 * cs_3 + 2.88 * cs_4;
            Real64 TransCurveF = 0.00 + 1.21 * cs + 3.14 * cs_2 - 6.37 * cs_3 + 3.03 * cs_4;
            Real64 TransCurveG = 0.00 + 1.09 * cs + 3.54 * cs_2 - 6.84 * cs_3 + 3.23 * cs_4;
            Real64 TransCurveH = 0.00 + 0.98 * cs + 3.83 * cs_2 - 7.13 * cs_3 + 3.33 * cs_4;
            Real64 TransCurveI = 0.00 + 0.79 * cs + 3.93 * cs_2 - 6.86 * cs_3 + 3.15 * cs_4;
            Real64 TransCurveJ = 0.00 + 0.08 * cs + 6.02 * cs_2 - 8.84 * cs_3 + 3.74 * cs_4;
            Real64 TransCurveFGHI = (TransCurveF + TransCurveG + TransCurveH + TransCurveI) / 4.0;
            Real64 TransCurveFH = (TransCurveF + TransCurveH) / 2.0;
            Real64 TransCurveBDCD = (TransCurveB + TransCurveD + TransCurveC + TransCurveD) / 4.0;

            Real64 ReflectCurveA = 1.00 - 0.70 * cs + 2.57 * cs_2 - 3.20 * cs_3 + 1.33 * cs_4 - TransCurveA;
            Real64 ReflectCurveB = 1.00 - 1.87 * cs + 6.50 * cs_2 - 7.86 * cs_3 + 3.23 * cs_4 - TransCurveB;
            Real64 ReflectCurveC = 1.00 - 2.52 * cs + 8.40 * cs_2 - 9.86 * cs_3 + 3.99 * cs_4 - TransCurveC;
            Real64 ReflectCurveD = 1.00 - 1.85 * cs + 6.40 * cs_2 - 7.64 * cs_3 + 3.11 * cs_4 - TransCurveD;
            Real64 ReflectCurveE = 1.00 - 1.57 * cs + 5.60 * cs_2 - 6.82 * cs_3 + 2.80 * cs_4 - TransCurveE;
            Real64 ReflectCurveF = 1.00 - 3.15 * cs + 10.98 * cs_2 - 13.14 * cs_3 + 5.32 * cs_4 - TransCurveF;
            Real64 ReflectCurveG = 1.00 - 3.25 * cs + 11.32 * cs_2 - 13.54 * cs_3 + 5.49 * cs_4 - TransCurveG;
            Real64 ReflectCurveH = 1.00 - 3.39 * cs + 11.70 * cs_2 - 13.94 * cs_3 + 5.64 * cs_4 - TransCurveH;
            Real64 ReflectCurveI = 1.00 - 4.06 * cs + 13.55 * cs_2 - 15.74 * cs_3 + 6.27 * cs_4 - TransCurveI;
            Real64 ReflectCurveJ = 1.00 - 4.35 * cs + 14.27 * cs_2 - 16.32 * cs_3 + 6.39 * cs_4 - TransCurveJ;

            Real64 ReflectCurveFGHI = (ReflectCurveF + ReflectCurveG + ReflectCurveH + ReflectCurveI) / 4.0;
            Real64 ReflectCurveFH = (ReflectCurveF + ReflectCurveH) / 2.0;
            Real64 ReflectCurveBDCD = (ReflectCurveB + ReflectCurveD + ReflectCurveC + ReflectCurveD) / 4.0;

            Real64 TransTmp(0.0);
            Real64 ReflectTmp(0.0);

            if (SimpleGlazingU < 1.4195) { // cell 1, 2, or 3
                if (SimpleGlazingSHGC > 0.45) {
                    // cell # 1
                    // Curve E
                    TransTmp = TransCurveE;
                    ReflectTmp = ReflectCurveE;

                } else if ((0.35 <= SimpleGlazingSHGC) && (SimpleGlazingSHGC <= 0.45)) {
                    // cell # 2
                    // 2 way interpolation between Curve E and Curve J
                    TransTmp = InterpolateBetweenTwoValues(SimpleGlazingSHGC, 0.35, 0.45, TransCurveJ, TransCurveE);
                    ReflectTmp = InterpolateBetweenTwoValues(SimpleGlazingSHGC, 0.35, 0.45, ReflectCurveJ, ReflectCurveE);

                } else if (SimpleGlazingSHGC < 0.35) {
                    // cell # 3
                    // Curve J
                    TransTmp = TransCurveJ;
                    ReflectTmp = ReflectCurveJ;
                }

            } else if ((1.4195 <= SimpleGlazingU) && (SimpleGlazingU <= 1.7034)) { // cell 4, 5 , 6, 7, 8, 9, or 10
                if (SimpleGlazingSHGC > 0.55) {
                    // cell # 4
                    // Curve E
                    TransTmp = TransCurveE;
                    ReflectTmp = ReflectCurveE;

                } else if ((0.5 < SimpleGlazingSHGC) && (SimpleGlazingSHGC <= 0.55)) {
                    // cell # 5
                    // 4 way interpolation between Curve E , Curve E, Curve E and Curve FGHI

                    TransTmp = InterpolateBetweenFourValues(
                        SimpleGlazingU, SimpleGlazingSHGC, 1.4195, 1.7034, 0.50, 0.55, TransCurveE, TransCurveE, TransCurveFGHI, TransCurveE);
                    ReflectTmp = InterpolateBetweenFourValues(
                        SimpleGlazingU, SimpleGlazingSHGC, 1.4195, 1.7034, 0.50, 0.55, ReflectCurveE, ReflectCurveE, ReflectCurveFGHI, ReflectCurveE);

                } else if ((0.45 < SimpleGlazingSHGC) && (SimpleGlazingSHGC <= 0.5)) {
                    // cell # 6
                    // 2 way interpolation between Curve E and Curve FGHI
                    TransTmp = InterpolateBetweenTwoValues(SimpleGlazingU, 1.4195, 1.7034, TransCurveE, TransCurveFGHI);
                    ReflectTmp = InterpolateBetweenTwoValues(SimpleGlazingU, 1.4195, 1.7034, ReflectCurveE, ReflectCurveFGHI);

                } else if ((0.35 < SimpleGlazingSHGC) && (SimpleGlazingSHGC <= 0.45)) {
                    // cell # 7
                    // 4 way interpolation between Curve E , Curve FGHI, Curve J and Curve FGHI
                    TransTmp = InterpolateBetweenFourValues(
                        SimpleGlazingU, SimpleGlazingSHGC, 1.4195, 1.7034, 0.35, 0.45, TransCurveJ, TransCurveE, TransCurveFGHI, TransCurveFGHI);
                    ReflectTmp = InterpolateBetweenFourValues(SimpleGlazingU,
                                                              SimpleGlazingSHGC,
                                                              1.4195,
                                                              1.7034,
                                                              0.35,
                                                              0.45,
                                                              ReflectCurveJ,
                                                              ReflectCurveE,
                                                              ReflectCurveFGHI,
                                                              ReflectCurveFGHI);

                } else if ((0.3 < SimpleGlazingSHGC) && (SimpleGlazingSHGC <= 0.35)) {
                    // cell # 8
                    // 2 way interpolation between Curve J and Curve FGHI
                    TransTmp = InterpolateBetweenTwoValues(SimpleGlazingU, 1.4195, 1.7034, TransCurveJ, TransCurveFGHI);
                    ReflectTmp = InterpolateBetweenTwoValues(SimpleGlazingU, 1.4195, 1.7034, ReflectCurveJ, ReflectCurveFGHI);

                } else if ((0.25 < SimpleGlazingSHGC) && (SimpleGlazingSHGC <= 0.3)) {
                    // cell # 9
                    // 4 way interpolation between Curve J, Curve FGHI, Curve J and Curve FH
                    TransTmp = InterpolateBetweenFourValues(
                        SimpleGlazingU, SimpleGlazingSHGC, 1.4195, 1.7034, 0.25, 0.3, TransCurveJ, TransCurveJ, TransCurveFH, TransCurveFGHI);
                    ReflectTmp = InterpolateBetweenFourValues(
                        SimpleGlazingU, SimpleGlazingSHGC, 1.4195, 1.7034, 0.25, 0.3, ReflectCurveJ, ReflectCurveJ, ReflectCurveFH, ReflectCurveFGHI);

                } else if (SimpleGlazingSHGC <= 0.25) {
                    // cell # 10
                    // 2 way interpolation between Curve J and Curve FH
                    TransTmp = InterpolateBetweenTwoValues(SimpleGlazingU, 1.4195, 1.7034, TransCurveJ, TransCurveFH);
                    ReflectTmp = InterpolateBetweenTwoValues(SimpleGlazingU, 1.4195, 1.7034, ReflectCurveJ, ReflectCurveFH);
                }
            } else if ((1.7034 < SimpleGlazingU) && (SimpleGlazingU < 3.4068)) { // cell 11, 12, 13, 14, or 15
                if (SimpleGlazingSHGC > 0.55) {
                    // cell # 11
                    // Curve E
                    TransTmp = TransCurveE;
                    ReflectTmp = ReflectCurveE;

                } else if ((0.5 <= SimpleGlazingSHGC) && (SimpleGlazingSHGC <= 0.55)) {
                    // cell # 12
                    // 2 way interpolation between Curve E and Curve FGHI
                    TransTmp = InterpolateBetweenTwoValues(SimpleGlazingSHGC, 0.5, 0.55, TransCurveFGHI, TransCurveE);
                    ReflectTmp = InterpolateBetweenTwoValues(SimpleGlazingSHGC, 0.5, 0.55, ReflectCurveFGHI, ReflectCurveE);

                } else if ((0.3 < SimpleGlazingSHGC) && (SimpleGlazingSHGC < 0.5)) {
                    // cell # 13
                    // Curve FGHI
                    TransTmp = TransCurveFGHI;
                    ReflectTmp = ReflectCurveFGHI;

                } else if ((0.25 <= SimpleGlazingSHGC) && (SimpleGlazingSHGC <= 0.3)) {
                    // cell # 14
                    // 2 way interpolation between Curve FGHI and Curve FH
                    TransTmp = InterpolateBetweenTwoValues(SimpleGlazingSHGC, 0.25, 0.30, TransCurveFH, TransCurveFGHI);
                    ReflectTmp = InterpolateBetweenTwoValues(SimpleGlazingSHGC, 0.25, 0.30, ReflectCurveFH, ReflectCurveFGHI);

                } else if (SimpleGlazingSHGC < 0.25) {
                    // cell # 15
                    // Curve FH
                    TransTmp = TransCurveFH;
                    ReflectTmp = ReflectCurveFH;
                }

            } else if ((3.4068 <= SimpleGlazingU) && (SimpleGlazingU <= 4.5424)) { // cell 16, 17, 18, 19, 20, 21, 22, or 23
                if (SimpleGlazingSHGC > 0.65) {
                    // cell # 16
                    // 2 way interpolation between Curve E and Curve A
                    TransTmp = InterpolateBetweenTwoValues(SimpleGlazingU, 3.4068, 4.5424, TransCurveE, TransCurveA);
                    ReflectTmp = InterpolateBetweenTwoValues(SimpleGlazingU, 3.4068, 4.5424, ReflectCurveE, ReflectCurveA);

                } else if ((0.6 < SimpleGlazingSHGC) && (SimpleGlazingSHGC <= 0.65)) {
                    // cell # 17
                    // 4 way interpolation between Curve E , Curve E, Curve A, and Curve BDCD
                    TransTmp = InterpolateBetweenFourValues(
                        SimpleGlazingU, SimpleGlazingSHGC, 3.4068, 4.5424, 0.6, 0.65, TransCurveE, TransCurveE, TransCurveBDCD, TransCurveA);
                    ReflectTmp = InterpolateBetweenFourValues(
                        SimpleGlazingU, SimpleGlazingSHGC, 3.4068, 4.5424, 0.6, 0.65, ReflectCurveE, ReflectCurveE, ReflectCurveBDCD, ReflectCurveA);

                } else if ((0.55 < SimpleGlazingSHGC) && (SimpleGlazingSHGC <= 0.6)) {
                    // cell # 18
                    // 2 way interpolation between Curve E and Curve BDCD
                    TransTmp = InterpolateBetweenTwoValues(SimpleGlazingU, 3.4068, 4.5424, TransCurveE, TransCurveBDCD);
                    ReflectTmp = InterpolateBetweenTwoValues(SimpleGlazingU, 3.4068, 4.5424, ReflectCurveE, ReflectCurveBDCD);

                } else if ((0.5 < SimpleGlazingSHGC) && (SimpleGlazingSHGC <= 0.55)) {
                    // cell # 19
                    // 4 way interpolation between Curve E , Curve FGHI, Curve BDCD and Curve BDCD
                    TransTmp = InterpolateBetweenFourValues(
                        SimpleGlazingU, SimpleGlazingSHGC, 3.4068, 4.5424, 0.5, 0.55, TransCurveFGHI, TransCurveE, TransCurveBDCD, TransCurveBDCD);
                    ReflectTmp = InterpolateBetweenFourValues(SimpleGlazingU,
                                                              SimpleGlazingSHGC,
                                                              3.4068,
                                                              4.5424,
                                                              0.5,
                                                              0.55,
                                                              ReflectCurveFGHI,
                                                              ReflectCurveE,
                                                              ReflectCurveBDCD,
                                                              ReflectCurveBDCD);

                } else if ((0.45 < SimpleGlazingSHGC) && (SimpleGlazingSHGC <= 0.5)) {
                    // cell # 20
                    // 2 way interpolation between Curve FGHI and Curve BDCD
                    TransTmp = InterpolateBetweenTwoValues(SimpleGlazingU, 3.4068, 4.5424, TransCurveFGHI, TransCurveBDCD);
                    ReflectTmp = InterpolateBetweenTwoValues(SimpleGlazingU, 3.4068, 4.5424, ReflectCurveFGHI, ReflectCurveBDCD);

                } else if ((0.3 < SimpleGlazingSHGC) && (SimpleGlazingSHGC <= 0.45)) {
                    // cell # 21
                    // 4 way interpolation between Curve FGHI, Curve FGHI, Curve BDCD, and Curve D
                    TransTmp = InterpolateBetweenFourValues(
                        SimpleGlazingU, SimpleGlazingSHGC, 3.4068, 4.5424, 0.3, 0.45, TransCurveFGHI, TransCurveFGHI, TransCurveD, TransCurveBDCD);
                    ReflectTmp = InterpolateBetweenFourValues(SimpleGlazingU,
                                                              SimpleGlazingSHGC,
                                                              3.4068,
                                                              4.5424,
                                                              0.3,
                                                              0.45,
                                                              ReflectCurveFGHI,
                                                              ReflectCurveFGHI,
                                                              ReflectCurveD,
                                                              ReflectCurveBDCD);

                } else if ((0.25 < SimpleGlazingSHGC) && (SimpleGlazingSHGC <= 0.3)) {
                    // cell # 22
                    // 4 way interpolation between Curve FGHI, Curve FH, Curve D, and Curve D
                    TransTmp = InterpolateBetweenFourValues(
                        SimpleGlazingU, SimpleGlazingSHGC, 3.4068, 4.5424, 0.25, 0.3, TransCurveFH, TransCurveFGHI, TransCurveD, TransCurveD);
                    ReflectTmp = InterpolateBetweenFourValues(
                        SimpleGlazingU, SimpleGlazingSHGC, 3.4068, 4.5424, 0.25, 0.3, ReflectCurveFH, ReflectCurveFGHI, ReflectCurveD, ReflectCurveD);

                } else if (SimpleGlazingSHGC <= 0.25) {
                    // cell # 23
                    // 2 way interpolation between Curve FH and Curve D
                    TransTmp = InterpolateBetweenTwoValues(SimpleGlazingU, 3.4068, 4.5424, TransCurveFH, TransCurveD);
                    ReflectTmp = InterpolateBetweenTwoValues(SimpleGlazingU, 3.4068, 4.5424, ReflectCurveFH, ReflectCurveD);
                }
            } else if (SimpleGlazingU > 4.5424) { // cell 24, 25, 26, 27, or 28
                if (SimpleGlazingSHGC > 0.65) {
                    // cell # 24
                    // Curve A
                    TransTmp = TransCurveA;
                    ReflectTmp = ReflectCurveA;
                } else if ((0.6 <= SimpleGlazingSHGC) && (SimpleGlazingSHGC <= 0.65)) {
                    // cell # 25
                    // 2 way interpolation between Curve A and Curve BDCD
                    TransTmp = InterpolateBetweenTwoValues(SimpleGlazingSHGC, 0.6, 0.65, TransCurveBDCD, TransCurveA);
                    ReflectTmp = InterpolateBetweenTwoValues(SimpleGlazingSHGC, 0.6, 0.65, ReflectCurveBDCD, ReflectCurveA);

                } else if ((0.45 < SimpleGlazingSHGC) && (SimpleGlazingSHGC < 0.6)) {
                    // cell # 26
                    // Curve BDCD
                    TransTmp = TransCurveBDCD;
                    ReflectTmp = ReflectCurveBDCD;

                } else if ((0.3 <= SimpleGlazingSHGC) && (SimpleGlazingSHGC <= 0.45)) {
                    // cell # 27
                    // 2 way interpolation between Curve BDCD and Curve D
                    TransTmp = InterpolateBetweenTwoValues(SimpleGlazingSHGC, 0.3, 0.45, TransCurveD, TransCurveBDCD);
                    ReflectTmp = InterpolateBetweenTwoValues(SimpleGlazingSHGC, 0.3, 0.45, ReflectCurveD, ReflectCurveBDCD);

                } else if (SimpleGlazingSHGC < 0.3) {
                    // cell # 28
                    // Curve D
                    TransTmp = TransCurveD;
                    ReflectTmp = ReflectCurveD;

                } else {
                    assert(false);
                }
            } else {
                assert(false);
            }

            if (cs == 1.0) { // at 0 deg incident, TransTmp and ReflectTmp should be 1.0
                TransTmp = 1.0;
                ReflectTmp = 0.0;
            }

            // now apply normalization factors to zero incidence angle properties
            tfp = tf0 * TransTmp;
            tfp = max(min(1.0, tfp), 0.0);

            rfp = rf0 * (1. - ReflectTmp) + ReflectTmp;
            rfp = max(min(0.9999 - tfp, rfp), 0.0);

            rbp = rfp;

        } else if (tf0 <= 0.0) {
            // This is an opaque window.  For all angles, set transmittance to 0; set reflectance to that at zero incidence angle.
            tfp = 0.0;
            rfp = rf0;
            rbp = rb0;
        } else {

            betaf = pow_2(tf0) - pow_2(rf0) + 2.0 * rf0 + 1.0;
            betab = pow_2(tf0) - pow_2(rb0) + 2.0 * rb0 + 1.0;
            r0f = (betaf - std::sqrt(pow_2(betaf) - 4.0 * (2.0 - rf0) * rf0)) / (2.0 * (2.0 - rf0));
            r0b = (betab - std::sqrt(pow_2(betab) - 4.0 * (2.0 - rb0) * rb0)) / (2.0 * (2.0 - rb0));

            tmp1 = std::abs(r0f - r0b);
            if (tmp1 != 0.0) {
                testval = std::abs(r0f - r0b) / (r0f + r0b);
            } else {
                testval = 0.0;
            }

            if (testval < 0.001) { // CR8830, CR8942, implications of relaxation of glazing properties CR8413
                // UNCOATED GLASS
                tmp1 = r0f * tf0;
                if (tmp1 != 0.0) {
                    abf = std::log(tmp1 / (rf0 - r0f));
                } else {
                    abf = 0.0;
                }
                tmp2 = r0b * tf0;
                if (tmp2 != 0.0) {
                    abb = std::log(tmp2 / (rb0 - r0b));
                } else {
                    abb = 0.0;
                }
                ngf = (1.0 + std::sqrt(r0f)) / (1.0 - std::sqrt(r0f));
                ngb = (1.0 + std::sqrt(r0b)) / (1.0 - std::sqrt(r0b));
                cgf = std::sqrt(1.0 - (1.0 - cs * cs) / pow_2(ngf));
                cgb = std::sqrt(1.0 - (1.0 - cs * cs) / pow_2(ngb));
                tmp3 = ngf * cs - cgf;
                if (tmp3 != 0.0) {
                    rpf1 = pow_2(tmp3 / (ngf * cs + cgf));
                } else {
                    rpf1 = 0.0;
                }
                tmp4 = ngf * cgf - cs;
                if (tmp4 != 0.0) {
                    rpf2 = pow_2(tmp4 / (ngf * cgf + cs));
                } else {
                    rpf2 = 0.0;
                }
                tpf1 = 1 - rpf1;
                tpf2 = 1 - rpf2;
                tmp5 = ngb * cs - cgb;
                if (tmp5 != 0.0) {
                    rpb1 = pow_2(tmp5 / (ngb * cs + cgb));
                } else {
                    rpb1 = 0.0;
                }
                tmp6 = ngb * cgb - cs;
                if (tmp6 != 0.0) {
                    rpb2 = pow_2(tmp6 / (ngb * cgb + cs));
                } else {
                    rpb2 = 0.0;
                }
                tpb1 = 1 - rpf1;
                tpb2 = 1 - rpf2;
                tmp7 = -abf;
                if (cgf != 0.0) {
                    expmabfdivcgf = std::exp(tmp7 / cgf);
                } else {
                    expmabfdivcgf = 0.0;
                }
                tmp8 = -2.0 * abf;
                if (cgf != 0.0) {
                    expm2abfdivcgf = std::exp(tmp8 / cgf);
                } else {
                    expm2abfdivcgf = 0.0;
                }
                if (tpf1 != 0.0) {
                    tfp1 = pow_2(tpf1) * expmabfdivcgf / (1.0 - pow_2(rpf1) * expm2abfdivcgf);
                } else {
                    tfp1 = 0.0;
                }
                rfp1 = rpf1 * (1.0 + tfp1 * expmabfdivcgf);
                if (tpf2 != 0.0) {
                    tfp2 = pow_2(tpf2) * expmabfdivcgf / (1.0 - pow_2(rpf2) * expm2abfdivcgf);
                } else {
                    tfp2 = 0.0;
                }
                rfp2 = rpf2 * (1.0 + tfp2 * expmabfdivcgf);
                tfp = 0.5 * (tfp1 + tfp2);
                rfp = 0.5 * (rfp1 + rfp2);
                tmp9 = -abb;
                if (tmp9 != 0.0) {
                    expmabbdivcgb = std::exp(tmp9 / cgb);
                } else {
                    expmabbdivcgb = 0.0;
                }
                rbp1 = rpb1 * (1.0 + tfp1 * expmabbdivcgb);
                rbp2 = rpb2 * (1.0 + tfp2 * expmabbdivcgb);
                rbp = 0.5 * (rbp1 + rbp2);
            } else {
                // COATED GLASS
                if (tf0 > 0.645) {
                    // Use clear glass angular distribution.
                    // Normalized clear glass transmittance and reflectance distribution
                    if (cs > 0.999) { // Angle of incidence = 0 deg
                        tcl = 1.0;
                        rcl = 0.0;
                    } else if (cs < 0.001) { // Angle of incidence = 90 deg
                        tcl = 0.0;
                        rcl = 1.0;
                    } else {
                        tcl = -0.0015 + (3.355 + (-3.840 + (1.460 + 0.0288 * cs) * cs) * cs) * cs;
                        rcl = 0.999 + (-0.563 + (2.043 + (-2.532 + 1.054 * cs) * cs) * cs) * cs - tcl;
                    }
                    tfp = tf0 * tcl;
                    rfp = rf0 * (1.0 - rcl) + rcl;
                    rbp = rb0 * (1.0 - rcl) + rcl;
                } else {
                    // Use bronze glass angular distribution.
                    // Normalized bronze tinted glass transmittance and reflectance distribution
                    if (cs > 0.999) { // Angle of incidence = 0 deg
                        tbnz = 1.0;
                        rbnz = 0.0;
                    } else if (cs < 0.001) { // Angle of incidence = 90 deg
                        tbnz = 0.0;
                        rbnz = 1.0;
                    } else {
                        tbnz = -0.002 + (2.813 + (-2.341 + (-0.05725 + 0.599 * cs) * cs) * cs) * cs;
                        rbnz = 0.997 + (-1.868 + (6.513 + (-7.862 + 3.225 * cs) * cs) * cs) * cs - tbnz;
                    }
                    tfp = tf0 * tbnz;
                    rfp = rf0 * (1.0 - rbnz) + rbnz;
                    rbp = rb0 * (1.0 - rbnz) + rbnz;
                }
            }
        }

        // total absorptance cannot be negative
        assert(1.0 - rfp - tfp >= -1e6);
        assert(1.0 - rbp - tfp >= -1e6);
    }

    Real64 InterpolateBetweenTwoValues(Real64 const X, Real64 const X0, Real64 const X1, Real64 const F0, Real64 const F1)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   June 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Interpolate between two results

        // METHODOLOGY EMPLOYED:
        // linear interpolation

        Real64 InterpResult;

        InterpResult = F0 + ((X - X0) / (X1 - X0)) * (F1 - F0);
        return InterpResult;
    }

    Real64 InterpolateBetweenFourValues(Real64 const X,
                                        Real64 const Y,
                                        Real64 const X1,
                                        Real64 const X2,
                                        Real64 const Y1,
                                        Real64 const Y2,
                                        Real64 const Fx1y1,
                                        Real64 const Fx1y2,
                                        Real64 const Fx2y1,
                                        Real64 const Fx2y2)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   June 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Interpolate between four results.

        // METHODOLOGY EMPLOYED:
        // bilinear interpolation (approximate)

        // REFERENCES:
        // http://en.wikipedia.org/wiki/Bilinear_interpolation

        // Return value
        Real64 InterpResult;

        InterpResult = (Fx1y1 / ((X2 - X1) * (Y2 - Y1))) * (X2 - X) * (Y2 - Y) + (Fx2y1 / ((X2 - X1) * (Y2 - Y1))) * (X - X1) * (Y2 - Y) +
                       (Fx1y2 / ((X2 - X1) * (Y2 - Y1))) * (X2 - X) * (Y - Y1) + (Fx2y2 / ((X2 - X1) * (Y2 - Y1))) * (X - X1) * (Y - Y1);
        return InterpResult;
    }

    //**************************************************************************

    void W5LsqFit(Array1S<Real64> const IndepVar, // Independent variables
                  Array1S<Real64> const DepVar,   // Dependent variables
                  int const N,                    // Order of polynomial
                  int const N1,                   // First and last data points used
                  int const N2,
                  Array1S<Real64> CoeffsCurve // Polynomial coeffients from fit
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         George Walton
        //       DATE WRITTEN   April 1976
        //       MODIFIED       November 1999 F.Winkelmann
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Does least squares fit for coefficients of a polynomial
        // that gives a window property, such as transmittance, as a function of
        // the cosine of the angle of incidence. The polynomial is of the
        // form C1*X + C2*X**2 + C3*X**3 + ... +CN*X**N, where N <= 6.
        // Adapted from BLAST subroutine LSQFIT.

        Array2D<Real64> A(6, 6);  // Least squares derivative matrix
        Array1D<Real64> B(6);     // Least squares derivative vector
        Array2D<Real64> D(6, 16); // Powers of independent variable
        Real64 ACON;              // Intermediate variables
        Real64 SUM;
        int i; // Loop parameters
        int j;
        int K;
        int L;
        int M;
        int KP1;
        int LP1;
        int NM1;

        // Set up least squares matrix
        for (M = N1; M <= N2; ++M) {
            D(1, M) = IndepVar(M);
        }

        for (i = 2; i <= N; ++i) {
            for (M = N1; M <= N2; ++M) {
                D(i, M) = D(i - 1, M) * IndepVar(M);
            }
        }

        for (i = 1; i <= N; ++i) {
            SUM = 0.0;
            for (M = N1; M <= N2; ++M) {
                SUM += DepVar(M) * D(i, M);
            }
            B(i) = SUM;
            for (j = 1; j <= N; ++j) {
                SUM = 0.0;
                for (M = N1; M <= N2; ++M) {
                    SUM += D(i, M) * D(j, M);
                }
                A(j, i) = SUM;
                A(i, j) = SUM;
            }
        }

        // Solve the simultaneous equations using Gauss elimination
        NM1 = N - 1;
        for (K = 1; K <= NM1; ++K) {
            KP1 = K + 1;
            for (i = KP1; i <= N; ++i) {
                ACON = A(K, i) / A(K, K);
                B(i) -= B(K) * ACON;
                for (j = K; j <= N; ++j) {
                    A(j, i) -= A(j, K) * ACON;
                }
            }
        }

        // Perform back substituion
        CoeffsCurve(N) = B(N) / A(N, N);
        LP1 = N;
        L = N - 1;

        while (L > 0) {
            SUM = 0.0;
            for (j = LP1; j <= N; ++j) {
                SUM += A(j, L) * CoeffsCurve(j);
            }
            CoeffsCurve(L) = (B(L) - SUM) / A(L, L);
            LP1 = L;
            --L;
        }
    }

    //********************************************************************************

    void W5LsqFit2(Array1A<Real64> const IndepVar, // Independent variables
                   Array1A<Real64> const DepVar,   // Dependent variables
                   int const N,                    // Order of polynomial
                   int const N1,                   // First and last data points used
                   int const N2,
                   Array1A<Real64> CoeffsCurve // Polynomial coefficients from fit
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         George Walton
        //       DATE WRITTEN   April 1976
        //       MODIFIED       November 1999 F.Winkelmann
        //                      May 2001 F. Winkelmann, to do 19 indep. variables
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Does least squares fit for coefficients of a polynomial
        // that gives a window property, such as transmittance, as a function of
        // the cosine of the angle of incidence. The polynomial is of the
        // form C1*X + C2*X**2 + C3*X**3 + ... +CN*X**N, where N <= 6.
        // Adapted from BLAST subroutine LSQFIT.

        // Argument array dimensioning
        IndepVar.dim(19);
        DepVar.dim(19);
        CoeffsCurve.dim(6);

        Array2D<Real64> A(6, 6);  // Least squares derivative matrix
        Array1D<Real64> B(6);     // Least squares derivative vector
        Array2D<Real64> D(6, 16); // Powers of independent variable
        Real64 ACON;              // Intermediate variables
        Real64 SUM;
        int i; // Loop parameters
        int j;
        int K;
        int L;
        int M;
        int KP1;
        int LP1;
        int NM1;

        // Set up least squares matrix
        for (M = N1; M <= N2; ++M) {
            D(1, M) = IndepVar(M);
        }

        for (i = 2; i <= N; ++i) {
            for (M = N1; M <= N2; ++M) {
                D(i, M) = D(i - 1, M) * IndepVar(M);
            }
        }

        for (i = 1; i <= N; ++i) {
            SUM = 0.0;
            for (M = N1; M <= N2; ++M) {
                SUM += DepVar(M) * D(i, M);
            }
            B(i) = SUM;
            for (j = 1; j <= N; ++j) {
                SUM = 0.0;
                for (M = N1; M <= N2; ++M) {
                    SUM += D(i, M) * D(j, M);
                }
                A(j, i) = SUM;
                A(i, j) = SUM;
            }
        }

        // Solve the simultaneous equations using Gauss elimination
        NM1 = N - 1;
        for (K = 1; K <= NM1; ++K) {
            KP1 = K + 1;
            for (i = KP1; i <= N; ++i) {
                ACON = A(K, i) / A(K, K);
                B(i) -= B(K) * ACON;
                for (j = K; j <= N; ++j) {
                    A(j, i) -= A(j, K) * ACON;
                }
            }
        }

        // Perform back substituion
        CoeffsCurve(N) = B(N) / A(N, N);
        LP1 = N;
        L = N - 1;

        while (L > 0) {
            SUM = 0.0;
            for (j = LP1; j <= N; ++j) {
                SUM += A(j, L) * CoeffsCurve(j);
            }
            CoeffsCurve(L) = (B(L) - SUM) / A(L, L);
            LP1 = L;
            --L;
        }
    }

    //***********************************************************************

    Real64 DiffuseAverage(Array1S<Real64> const PropertyValue) // Property value at angles of incidence
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Fred Winkelmann
        //       DATE WRITTEN   November 1999
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Calculate value of property, such as transmittance, for hemispherical
        // diffuse radiation from property values at angles of incidence from
        // 0 to 90 degrees in 10 degree increments.

        // METHODOLOGY EMPLOYED:
        // By Simpson's rule, evaluates the integral from 0 to 90 deg of
        // 2*PropertyValue(phi)*cos(phi)*sin(phi)*dphi (which is same as
        // PropertyValue(phi)*sin(2*phi)*dphi)

        // Return value
        Real64 DiffuseAverage;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // 0,10,20,...,80,90 degress

        Real64 const DPhiR(10.0 * DataGlobalConstants::DegToRadians); // Half of 10-deg incidence angle increment (radians)
        int IPhi;                                                     // Incidence angle counter

        DiffuseAverage = 0.0;
        for (IPhi = 1; IPhi <= 9; ++IPhi) {
            DiffuseAverage +=
                0.5 * DPhiR * (PropertyValue(IPhi) * std::sin(2.0 * (IPhi - 1) * DPhiR) + PropertyValue(IPhi + 1) * std::sin(2.0 * IPhi * DPhiR));
        }
        if (DiffuseAverage < 0.0) DiffuseAverage = 0.0;

        return DiffuseAverage;
    }

    //*************************************************************************************

    Real64 DiffuseAverageProfAngGnd(Array1S<Real64> const Property) // Property value vs. profile angle
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Fred Winkelmann
        //       DATE WRITTEN   January 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Calculates diffuse average of Property, such as blind transmittance, over profile angles
        // corresponding to (upgoing) radiation from the ground.

        // METHODOLOGY EMPLOYED:
        // Integration by Simpson's rule assuming uniform radiance distribution.

        // Using/Aliasing
        using General::InterpProfAng;

        // Return value
        Real64 DiffuseAverageProfAngGnd;

        Real64 Phi;  // Profile angle (radians)
        Real64 DPhi; // Phi increment
        int IPhi;    // Phi index
        Real64 Sum;  // Sums
        Real64 SumDenom;

        Sum = 0.0;
        SumDenom = 0.0;
        DPhi = 5.0 * DataGlobalConstants::DegToRadians;

        // Integrate from -90 to 0 deg
        for (IPhi = 1; IPhi <= 18; ++IPhi) {
            Phi = -DataGlobalConstants::PiOvr2 + (IPhi - 0.5) * DPhi;
            Sum += std::cos(Phi) * DPhi * InterpProfAng(Phi, Property);
            SumDenom += std::cos(Phi) * DPhi;
        }

        DiffuseAverageProfAngGnd = Sum / SumDenom;
        if (DiffuseAverageProfAngGnd < 0.0) DiffuseAverageProfAngGnd = 0.0;

        return DiffuseAverageProfAngGnd;
    }

    //*************************************************************************************

    Real64 DiffuseAverageProfAngSky(Array1S<Real64> const Property) // Property value vs. profile angle
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Fred Winkelmann
        //       DATE WRITTEN   January 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Calculates diffuse average of Property, such as blind transmittance, over profile angles
        // corresponding to (downgoing) radiation from the sky.

        // METHODOLOGY EMPLOYED:
        // Integration by Simpson's rule assuming uniform radiance distribution.

        // Using/Aliasing
        using General::InterpProfAng;

        // Return value
        Real64 DiffuseAverageProfAngSky;

        Real64 Phi;  // Profile angle (radians)
        Real64 DPhi; // Phi increment
        int IPhi;    // Phi index
        Real64 Sum;  // Sums
        Real64 SumDenom;

        Sum = 0.0;
        SumDenom = 0.0;
        DPhi = 5.0 * DataGlobalConstants::DegToRadians;

        // Integrate from 0 to 90 deg
        for (IPhi = 19; IPhi <= 36; ++IPhi) {
            Phi = -DataGlobalConstants::PiOvr2 + (IPhi - 0.5) * DPhi;
            Sum += std::cos(Phi) * DPhi * InterpProfAng(Phi, Property);
            SumDenom += std::cos(Phi) * DPhi;
        }

        DiffuseAverageProfAngSky = Sum / SumDenom;
        if (DiffuseAverageProfAngSky < 0.0) DiffuseAverageProfAngSky = 0.0;

        return DiffuseAverageProfAngSky;
    }

    //*************************************************************************************

    void CalcWinFrameAndDividerTemps(EnergyPlusData &state,
                                     int const SurfNum,     // Surface number
                                     Real64 const tout,     // Outside air temperature (K)
                                     Real64 const tin,      // Inside air temperature (K)
                                     Real64 const HOutConv, // Outside convective air film conductance (W/m2-K)
                                     Real64 const HInConv,  // Inside convective air film conductance (W/m2-K)
                                     Real64 const Outir,    // Exterior IR irradiance from sky and ground
                                     int const ConstrNum    // Construction number of window
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         F. Winkelmann
        //       DATE WRITTEN   May 2000
        //       MODIFIED       Aug 2000, FCW: Add effect of frame and divider projections
        //                      Jun 2001, FCW: Add frame/divider contribution to WinHeatGain
        //                      Aug 2003, FCW: Fix calculation of divider outside temp: frame
        //                       inside temp was being used instead of divider inside temp
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates window frame divider face temperatures from a linearized
        // heat balance on the inside and outside faces

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 HInRad;            // Inside radiative conductance (W/m2-K)
        Real64 HOutRad;           // Outside radiative conductance (W/m2-K)
        int FrDivNum;             // Frame/divider number
        Real64 TInRad;            // Inside radiative temperature (K)
        Real64 TInRadFr;          // Effective inside radiative temperature for frame (K)
        Real64 TInRadDiv;         // Effective inside radiative temperature for divider (K)
        Real64 TOutRad;           // Outside radiative temperature (K)
        Real64 TOutRadFr;         // Effective outside radiative temperature for frame (K)
        Real64 TOutRadDiv;        // Effective outside radiative temperature for divider (K)
        WinShadingType ShadeFlag; // Window shading flag
        Real64 FrameCon;          // Frame conductance (W/m2-K)

        Real64 Afac; // Intermediate calculation variables
        Real64 Bfac;
        Real64 Dfac;
        Real64 Efac;
        DataSurfaces::FrameDividerType DivType; // Divider type
        Real64 DivCon;                          // Divider conductance (W/m2-K)
        Real64 DivEmisIn;                       // Inside divider emissivity
        Real64 DivEmisOut;                      // Outside divider emissivity

        Real64 ProjCorrFrOut; // Outside correction factor for absorbed radiation
        //   for frame with outside projection
        Real64 ProjCorrFrIn; // Inside correction factor for absorbed radiation
        //   for frame with inside projection
        Real64 HOutConvFr; // Effective outside convective coeff for frame
        //   with outside projection (W/m2-K)
        Real64 HOutConvDiv; // Effective outside convective coeff for divider
        //   with outside projection (W/m2-K)
        Real64 HInConvFr; // Effective inside convective coeff for frame
        //   with inside projection (W/m2-K)
        Real64 HInConvDiv; // Effective inside convective coeff for divider
        //   with inside projection (W/m2-K)
        Real64 EmisGlassOut; // Outside surface emissivity of window glazing
        Real64 EmisGlassIn;  // Inside surface emissivity of window glazing
        int TotGlassLayers;  // Total number of glass layers
        int TotLayers;       // Total number of layers in unshaded construction
        // Real64 DivTempOut;          // Outside surface divider temperature (K)
        Real64 FrameHeatGain; // Heat gain to zone from frame (W)
        // Real64 FrameHeatTransfer;   // Heat tansfer through frame (W)
        // Real64 ProjCorrWinHeatGain; // Inside projection correction to IR from divider to zone
        //   for window heat gain calculation
        Real64 DividerHeatGain; // Heat gain to zone from divider (W)
        // Real64 DividerHeatTransfer; // Heat transfer through divider (W)

        TInRad = root_4(state.dataSurface->SurfWinIRfromParentZone(SurfNum) / state.dataWindowManager->sigma);
        TOutRad = root_4(Outir / state.dataWindowManager->sigma);
        ShadeFlag = state.dataSurface->SurfWinShadingFlag(SurfNum);
        FrDivNum = state.dataSurface->Surface(SurfNum).FrameDivider;
        TotLayers = state.dataConstruction->Construct(ConstrNum).TotLayers;
        TotGlassLayers = state.dataConstruction->Construct(ConstrNum).TotSolidLayers;
        EmisGlassOut = state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).AbsorpThermalFront;
        EmisGlassIn = state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(TotLayers)).AbsorpThermalBack;
        FrameHeatGain = 0.0;
        DividerHeatGain = 0.0;
        state.dataSurface->SurfWinFrameHeatGain(SurfNum) = 0.0;
        state.dataSurface->SurfWinFrameHeatLoss(SurfNum) = 0.0;
        state.dataSurface->SurfWinDividerHeatGain(SurfNum) = 0.0;
        state.dataSurface->SurfWinDividerHeatLoss(SurfNum) = 0.0;

        if (state.dataSurface->SurfWinFrameArea(SurfNum) > 0.0) {
            // Window has a frame. Note that if a shade, screen or blind is present it covers only the glazed part of the
            // window and is assumed not to shadow long- or short-wave radiation incident on the frame elements.
            ProjCorrFrOut = state.dataSurface->SurfWinProjCorrFrOut(SurfNum);
            ProjCorrFrIn = state.dataSurface->SurfWinProjCorrFrIn(SurfNum);
            TOutRadFr = TOutRad * root_4((1.0 + 0.5 * ProjCorrFrOut) / (1.0 + ProjCorrFrOut));
            TInRadFr = TInRad * root_4((1.0 + 0.5 * ProjCorrFrIn) / (1.0 + ProjCorrFrIn));
            FrameCon = state.dataSurface->SurfWinFrameConductance(SurfNum);
            HInRad = 0.5 * state.dataSurface->SurfWinFrameEmis(SurfNum) * state.dataWindowManager->sigma *
                     pow_3(TInRadFr + state.dataSurface->SurfWinFrameTempIn(SurfNum) + state.dataWindowManager->TKelvin);
            HInConvFr = HInConv;
            HOutRad = 0.5 * state.dataSurface->SurfWinFrameEmis(SurfNum) * state.dataWindowManager->sigma *
                      pow_3(TOutRadFr + state.dataSurface->SurfWinFrameTempSurfOut(SurfNum) + state.dataWindowManager->TKelvin);
            HOutConvFr = HOutConv;
            if (state.dataSurface->FrameDivider(FrDivNum).FrameProjectionOut > 0.0) {
                HOutRad *= (1.0 + ProjCorrFrOut);
                HOutConvFr = HOutConv * (1.0 + ProjCorrFrOut);
                // Add long-wave from outside window surface absorbed by frame outside projection
                state.dataSurface->SurfWinFrameQRadOutAbs(SurfNum) +=
                    0.5 * state.dataSurface->SurfWinProjCorrFrOut(SurfNum) * state.dataSurface->FrameDivider(FrDivNum).FrameEmis * EmisGlassOut *
                    state.dataWindowManager->sigma * pow_4(state.dataSurface->SurfaceWindow(SurfNum).ThetaFace(1));
            }
            if (state.dataSurface->FrameDivider(FrDivNum).FrameProjectionIn > 0.0) {
                HInRad *= (1.0 + ProjCorrFrIn);
                HInConvFr = HInConv * (1.0 + ProjCorrFrIn);
                // Add long-wave from inside window surface absorbed by frame inside projection
                state.dataSurface->SurfWinFrameQRadInAbs(SurfNum) +=
                    0.5 * state.dataSurface->SurfWinProjCorrFrIn(SurfNum) * state.dataSurface->FrameDivider(FrDivNum).FrameEmis * EmisGlassIn *
                    state.dataWindowManager->sigma * pow_4(state.dataSurface->SurfaceWindow(SurfNum).ThetaFace(2 * TotGlassLayers));
            }
            Afac = (HOutRad * TOutRadFr + HOutConvFr * tout + state.dataSurface->SurfWinFrameQRadOutAbs(SurfNum)) / (HOutRad + FrameCon + HOutConvFr);
            Bfac = FrameCon / (HOutRad + FrameCon + HOutConvFr);
            Dfac = (HInRad * TInRadFr + HInConvFr * tin + state.dataSurface->SurfWinFrameQRadInAbs(SurfNum)) / (HInRad + FrameCon + HInConvFr);
            Efac = FrameCon / (HInRad + FrameCon + HInConvFr);
            state.dataSurface->SurfWinFrameTempIn(SurfNum) = (Dfac + Efac * Afac) / (1.0 - Efac * Bfac) - state.dataWindowManager->TKelvin;
            state.dataSurface->SurfWinFrameTempSurfOut(SurfNum) =
                Afac + Bfac * (state.dataSurface->SurfWinFrameTempIn(SurfNum) + state.dataWindowManager->TKelvin) - state.dataWindowManager->TKelvin;
            // Heat gain to zone from frame

            // FrameHeatTransfer = state.dataSurface->SurfWinFrameArea(SurfNum) * FrameCon *
            //                     (state.dataSurface->SurfWinFrameTempSurfOut(SurfNum) - state.dataSurface->SurfWinFrameTempIn(SurfNum));
            FrameHeatGain = state.dataSurface->SurfWinFrameArea(SurfNum) * (1.0 + state.dataSurface->SurfWinProjCorrFrIn(SurfNum)) *
                            (HInConvFr * (state.dataSurface->SurfWinFrameTempIn(SurfNum) + state.dataWindowManager->TKelvin - tin));

            if (FrameHeatGain > 0.0) {
                state.dataSurface->SurfWinFrameHeatGain(SurfNum) = FrameHeatGain;
            } else {
                state.dataSurface->SurfWinFrameHeatLoss(SurfNum) = std::abs(FrameHeatGain);
            }

            state.dataSurface->SurfWinHeatGain(SurfNum) += FrameHeatGain;
            state.dataSurface->SurfWinGainFrameDividerToZoneRep(SurfNum) = FrameHeatGain;
        } // End of check if window has a frame

        if (state.dataSurface->SurfWinDividerArea(SurfNum) > 0.0 && state.dataSurface->SurfWinStormWinFlag(SurfNum) < 1) {
            // Window has divider. Note that if the window has a storm window layer in place (StormWinFlag = 1)
            // the divider heat transfer calculation is not done.

            DivType = state.dataSurface->SurfWinDividerType(SurfNum);
            DivCon = state.dataSurface->SurfWinDividerConductance(SurfNum);

            if (DivType == DataSurfaces::FrameDividerType::DividedLite) { // Divided lite
                DivEmisIn = state.dataSurface->SurfWinDividerEmis(SurfNum);
                DivEmisOut = DivEmisIn;
            } else { // Suspended (between-glass) divider
                DivEmisOut = state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).AbsorpThermalFront;
                DivEmisIn =
                    state.dataMaterial
                        ->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(state.dataConstruction->Construct(ConstrNum).TotLayers))
                        .AbsorpThermalBack;
            }

            TOutRadDiv = TOutRad * root_4((1.0 + state.dataSurface->SurfWinProjCorrDivOut(SurfNum)) /
                                          (1.0 + 2.0 * state.dataSurface->SurfWinProjCorrDivOut(SurfNum)));
            TInRadDiv = TInRad * root_4((1.0 + state.dataSurface->SurfWinProjCorrDivIn(SurfNum)) /
                                        (1.0 + 2.0 * state.dataSurface->SurfWinProjCorrDivIn(SurfNum)));
            HInRad = 0.5 * DivEmisIn * state.dataWindowManager->sigma *
                     pow_3(TInRadDiv + state.dataSurface->SurfWinDividerTempIn(SurfNum) + state.dataWindowManager->TKelvin);
            HOutRad = 0.5 * DivEmisOut * state.dataWindowManager->sigma *
                      pow_3(TOutRadDiv + state.dataSurface->SurfWinDividerTempSurfOut(SurfNum) + state.dataWindowManager->TKelvin);
            HOutConvDiv = HOutConv;

            if (state.dataSurface->FrameDivider(FrDivNum).DividerProjectionOut > 0.0) {
                HOutRad *= (1.0 + 2.0 * state.dataSurface->SurfWinProjCorrDivOut(SurfNum));
                if (state.dataSurface->SurfWinShadingFlag(SurfNum) == WinShadingType::ExtShade)
                    HOutConvDiv = state.dataSurface->SurfWinConvCoeffWithShade(SurfNum);
                HOutConvDiv *= (1.0 + 2.0 * state.dataSurface->SurfWinProjCorrDivOut(SurfNum));
                // Add long-wave from outside window surface absorbed by divider outside projection
                state.dataSurface->SurfWinDividerQRadOutAbs(SurfNum) +=
                    state.dataSurface->SurfWinProjCorrDivOut(SurfNum) * state.dataSurface->FrameDivider(FrDivNum).DividerEmis * EmisGlassOut *
                    state.dataWindowManager->sigma * pow_4(state.dataSurface->SurfaceWindow(SurfNum).ThetaFace(1));
            }

            HInConvDiv = HInConv;

            if (state.dataSurface->FrameDivider(FrDivNum).DividerProjectionIn > 0.0) {
                HInRad *= (1.0 + 2.0 * state.dataSurface->SurfWinProjCorrDivIn(SurfNum));
                if (state.dataSurface->SurfWinShadingFlag(SurfNum) == WinShadingType::IntShade)
                    HInConvDiv = state.dataSurface->SurfWinConvCoeffWithShade(SurfNum);
                HInConvDiv *= (1.0 + 2.0 * state.dataSurface->SurfWinProjCorrDivIn(SurfNum));
                // Add long-wave from inside window surface absorbed by divider inside projection
                state.dataSurface->SurfWinDividerQRadInAbs(SurfNum) +=
                    state.dataSurface->SurfWinProjCorrDivIn(SurfNum) * state.dataSurface->FrameDivider(FrDivNum).DividerEmis * EmisGlassIn *
                    state.dataWindowManager->sigma * pow_4(state.dataSurface->SurfaceWindow(SurfNum).ThetaFace(2 * TotGlassLayers));
            }
            Afac =
                (HOutRad * TOutRadDiv + HOutConvDiv * tout + state.dataSurface->SurfWinDividerQRadOutAbs(SurfNum)) / (HOutRad + DivCon + HOutConvDiv);
            Bfac = DivCon / (HOutRad + DivCon + HOutConvDiv);
            Dfac = (HInRad * TInRadDiv + HInConvDiv * tin + state.dataSurface->SurfWinDividerQRadInAbs(SurfNum)) / (HInRad + DivCon + HInConvDiv);
            Efac = DivCon / (HInRad + DivCon + HInConvDiv);
            state.dataSurface->SurfWinDividerTempIn(SurfNum) = (Dfac + Efac * Afac) / (1 - Efac * Bfac) - state.dataWindowManager->TKelvin;
            state.dataSurface->SurfWinDividerTempSurfOut(SurfNum) =
                Afac + Bfac * (state.dataSurface->SurfWinDividerTempIn(SurfNum) + state.dataWindowManager->TKelvin) -
                state.dataWindowManager->TKelvin;
            // Contribution of divider to window heat gain
            // ProjCorrWinHeatGain = 1.0 + 2.0 * state.dataSurface->SurfWinProjCorrDivIn(SurfNum);

            DividerHeatGain = state.dataSurface->SurfWinDividerArea(SurfNum) * (1.0 + state.dataSurface->SurfWinProjCorrDivIn(SurfNum)) *
                              (HInConvDiv * (state.dataSurface->SurfWinDividerTempIn(SurfNum) + state.dataWindowManager->TKelvin - tin));
            // DividerHeatTransfer = state.dataSurface->SurfWinDividerArea(SurfNum) * DivCon *
            //                      (state.dataSurface->SurfWinDividerTempSurfOut(SurfNum) - state.dataSurface->SurfWinDividerTempIn(SurfNum));

            if (DividerHeatGain > 0.0) {
                state.dataSurface->SurfWinDividerHeatGain(SurfNum) = DividerHeatGain;
            } else {
                state.dataSurface->SurfWinDividerHeatLoss(SurfNum) = std::abs(DividerHeatGain);
            }
            state.dataSurface->SurfWinHeatGain(SurfNum) += DividerHeatGain;
            state.dataSurface->SurfWinGainFrameDividerToZoneRep(SurfNum) += DividerHeatGain;
            // If interior shade is present it is assumed that both the convective and IR radiative gain
            // from the inside surface of the divider goes directly into the zone air -- i.e., the IR radiative
            // interaction between divider and shade is ignored due to the difficulty of calculating this interaction
            // at the same time that the interaction between glass and shade is calculated.
            if (ANY_INTERIOR_SHADE_BLIND(state.dataSurface->SurfWinShadingFlag(SurfNum)))
                state.dataSurface->SurfWinDividerHeatGain(SurfNum) = DividerHeatGain;
            // DivTempOut = state.dataSurface->SurfWinDividerTempSurfOut(SurfNum) + state.dataWindowManager->TKelvin;
        } // End of check if window has dividers
    }

    //************************************************************************************

    void CalcNominalWindowCond(EnergyPlusData &state,
                               int const ConstrNum,        // Construction number
                               int const WinterSummerFlag, // 1=winter, 2=summer
                               Real64 &NominalConductance, // Nominal center-of-glass conductance, including air films
                               Real64 &SHGC,               // Nominal center-of-glass solar heat gain coefficient for
                               Real64 &TSolNorm,           // Overall beam solar transmittance at normal incidence
                               Real64 &TVisNorm,           // Overall beam visible transmittance at normal incidence
                               int &errFlag                // Error flag
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         F. Winkelmann
        //       DATE WRITTEN   September 2000
        //       MODIFIED       Oct 2000, FW: add solar heat gain coefficient
        //                      June 2001, FW: account for blinds; change summer outside air
        //                       temp from 35.0C to 31.7C to correspond to ASHRAE value
        //                      Feb 2003, FW: add comments that this routine is not called for
        //                       between-glass shade/blind constructions.
        //                      May 2006, RR: account for screens
        //                      Oct 2007, LKL: change temps to match Window 5 values
        //                      Feb 2009, BG: Changes for CR7682 (SHGC)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates nominal center-of-glass U-value and solar heat gain coefficient
        // (SHGC) of a window construction for ASHRAE winter and summer conditions.
        // Winter:
        // Inside air temperature = 21.C (69.80F)
        // Outside air temperature = -18C (-.4F)
        // Windspeed = 5.5 m/s (12.3 mph)
        // No solar radiation
        // Replaced Winter:
        // Inside air temperature = 21.1C (70F)
        // Outside air temperature = -17.8C (0F)
        // Windspeed = 6.71 m/s (15 mph)
        // No solar radiation
        // Summer:
        // Inside air temperature = 24C (75.2F)
        // Outside air temperature = 32C (89.6F)
        // Windspeed = 2.8 m/s (6.2 mph)
        // 783 W/m2 (248 Btu/h-ft2) incident beam solar radiation normal to glazing
        // Replaced Summer:
        // Inside air temperature = 24C (75.2F) ! BG changed again Feb. 2009 by 0.1 (per Window5 team)
        // Outside air temperature = 31.7C (89F)
        // Windspeed = 3.35 m/s (7.5 mph)
        // 783 W/m2 (248 Btu/h-ft2) incident beam solar radiation normal to glazing
        // The window's inside surround is assumed to be a black body at the inside air temp.
        // The window's outside surround is assumed t be a black body at the outside air temp.
        // Note that in this routine we use a value of 26 W/m2 for the outside convective
        // air film conductance for 5.5 m/s (12.3 mph) wind speed.
        // This is the value used in Window 5 and is also the value for which the center-of-glass
        // conductances in the EnergyPlus window construction reference data set were calculated.
        // However, in the time step loop we will have different values of outside film
        // conductance depending on that time step's wind speed, wind direction, surface-to-air temp difference,
        // etc.(see subroutine InitExteriorConvectionCoeff).
        // This routine will return an error and exit for window constructions with between-glass shade or blind
        // until a method is worked out to determine the nominal conductance and SHGC in this case.
        // If an interior or exterior shade or blind is present in the construction,
        // the conductance calculation does not include the effect of the shade or blind.
        // This is because in this case the conductance depends on the natural convective
        // air flow in the shade/glass, screen/glass or blind/glass channel, which in turn is highly dependent
        // on window height and other parameters that are not part of the construction definition.
        // Therefore, the reported conductance value will be too high for windows with a tightly fitting
        // shade, screen or blind with a relatively high thermal resistance.
        // For SHGC calculation, all solar absorbed by interior blind or shade is assumed
        // to go into zone air. (This is not true in general; the fraction of this absorbed solar that
        // is conducted back out is accounted for in the time-step glazing calculation.)
        // For CR 7682, the SHGC calculations were changed to model the absorbed solar arriving at the middle of the layer
        // rather than at the outer face of the layer.  The resistances changed by one half the glazing layer, or 0.5/scon(n).
        // (CR 7682 also changed WindowTempsForNominalCond to include absorbed solar, a bigger change)

        // Using/Aliasing
        using General::BlindBeamBeamTrans;
        using General::InterpProfSlatAng;
        using General::InterpSlatAng;
        using General::POLYF;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        //   normal incidence beam solar radiation

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int TotLay; // Total number of layers in a construction
        //   (sum of solid layers and gap layers)
        int TotGlassLay; // Total number of glass layers in a construction
        int Lay;         // Layer number
        int LayPtr;      // Material number for a layer
        int IGlass;      // glass layer number (1,2,3,...)
        int IGap;        // Gap layer number (1,2,...)
        int IMix;        // Gas component loop index for gap gas mixture
        int ICoeff;      // Gas property coefficient index

        Real64 BeamSolarInc; // Incident beam radiation at zero angle of incidence (W/m2)
                             //        Real64 hOutRad;      // Radiative conductance of outside and inside airfilm [W/m2-K]
                             //        Real64 hInRad;
                             //        Real64 rOut; // Combined radiative and conductive outside and inside film
                             //        Real64 rIn;
        //   resistance [m2-K/W]
        Array1D<Real64> hgap(5);        // Conductive gap conductance [W/m2-K]
                                        //        Array1D<Real64> hGapTot(5);     // Combined radiative and conductive gap conductance [W/m2-K]
                                        //        Real64 Rbare;                   // Nominal center-of-glass resistance without air films [m2-K/W]
        WinShadingType ShadeFlag;       // Shading flag
        Real64 ShadeRes;                // Thermal resistance of shade
        int MatOutside;                 // Material number of outside layer of construction
        int MatInside;                  // Material number of inside layer of construction
        int MatShade;                   // Material number of shade layer
        Array1D<Real64> AbsBeamNorm(5); // Beam absorptance at normal incidence for each glass layer
        Real64 AbsBeamShadeNorm;        // Shade solar absorptance at normal incidence
        int ConstrNum1;                 // Construction counter
        int ConstrNumBare;              // Construction without shading device
        int BlNum;                      // Blind number
        int ScNum;                      // Screen number
        bool VarSlats;                  // True if slats in blind are variable angle
        Real64 SlatAng;                 // Slat angle (rad)
        int LayPtrSh;                   // Layer pointer of blind
        Real64 TBmBm;                   // Bare glass normal incidence beam-beam transmittance
        Real64 TBmBmVis;
        Real64 TBlBmBm; // Normal incidence blind beam-beam transmittance
        Real64 TScBmBm; // Screen incident beam-beam transmittance
        Real64 TScBmBmVis;
        Real64 TBmBmBl; // TBmBm * TBlBmBm, TBmBmVis * TBlBmBm
        Real64 TBmBmBlVis;
        Real64 RGlDiffBack; // Bare glass back sol/vis reflectance
        Real64 RGlDiffBackVis;
        Real64 RGlDiffFront; // Bare glass front sol/vis reflectance
        Real64 RGlDiffFrontVis;
        Real64 RhoBlFront; // Blind normal front beam-diffuse sol/vis reflectance
        Real64 RhoBlFrontVis;
        Real64 RhoBlBack; // Blind normal back beam-diffuse sol/vis reflectance
        Real64 RhoBlBackVis;
        Real64 RScBack; // Screen back beam-diffuse sol/vis reflectance (same as front)
        Real64 RScBackVis;
        Real64 AbsBlFront;     // Blind normal front beam solar absorptance
        Real64 AbsBlBack;      // Blind normal back beam solar absorptance
        Real64 RhoBlDiffFront; // Blind front diffuse-diffuse sol/vis reflectance
        Real64 RhoBlDiffFrontVis;
        Real64 AbsBlDiffFront; // Blind front diffuse solar absorptance
        Real64 AbsBlDiffBack;  // Blind back diffuse solar absorptance
        Real64 RGlFront;       // Bare glass normal front beam sol/vis reflectance
        Real64 RGlFrontVis;
        Real64 RhoBlDiffBack; // Blind back diffuse-diffuse sol/vis reflectance
        Real64 RhoBlDiffBackVis;
        Real64 RScDifBack; // Screen back diffuse-diffuse sol/vis reflectance (doesn't change with sun angle)
        Real64 RScDifBackVis;
        Real64 TBlBmDif; // Blind front normal beam-diffuse sol/vis transmittance
        Real64 TBlBmDifVis;
        Real64 TBlDifDif; // Blind front diffuse-diffuse sol/vis transmittance
        Real64 TBlDifDifVis;
        Real64 TScBmDif; // Screen front beam-diffuse sol/vis transmittance
        Real64 TScBmDifVis;
        Real64 TDif; // Bare glass diffuse sol/vis transmittance
        Real64 TDifVis;
        Real64 AGlDiffBack; // Back diffuse solar absorptance of a glass layer

        // Autodesk:Uninit Initialize variables used uninitialized
        //        Rbare = 0.0; // Autodesk:Uninit Force default initialization
        NominalConductance = 0.0;
        errFlag = 0;
        TotLay = state.dataConstruction->Construct(ConstrNum).TotLayers;
        TotGlassLay = state.dataConstruction->Construct(ConstrNum).TotGlassLayers;
        state.dataWindowManager->ngllayer = TotGlassLay; // Autodesk:Uninit This routine needs to check/enforce 1<=ngllayer<=4
        // EPTeam - believe that is done on input.
        state.dataWindowManager->nglface = 2 * state.dataWindowManager->ngllayer;
        state.dataWindowManager->tilt = 90.0; // Assume vertical window

        if (WinterSummerFlag == 1) { // Winter
            // LKL Oct 2007:  According to Window5, Winter environmental conditions are:
            state.dataWindowManager->tin = 294.15;  // Inside air temperature (69.8F, 21.C)
            state.dataWindowManager->tout = 255.15; // Outside air temperature (-.4F, -18C)
            state.dataWindowManager->hcout = 26.0;  // Outside convective film conductance for 5.5 m/s (12.3 mph) wind speed
            // (the value used in Window 5)
            //  tin = 294.26   ! Inside air temperature (70F, 21.1C)
            //  tout = 255.35  ! Outside air temperature (0F, -17.8C)
            //  hcout = 25.47  ! Outside convective film conductance for 6.71 m/s (15 mph) wind speed
            //                 ! (the value used in Window 4)
            BeamSolarInc = 0.0;
        } else { // Summer
            // LKL Oct 2007: According to Window5, Summer environmental conditions are:
            // tin = 297.05d0   ! Inside air temperature (75.2F, 24C)
            // BG Feb. 2009: According to Window5 Expert Christian Kohler, it is exactly 24C or 297.15
            state.dataWindowManager->tin = 297.15;
            state.dataWindowManager->tout = 305.15; // Outside air temperature (89.6F, 32C)
            state.dataWindowManager->hcout = 15.0;  // Outside convective film conductance for 2.8 m/s (6.2 mph) wind speed
            // (the value used in Window 5)
            //  tin = 297.05   ! Inside air temperature (75F, 23.9C)
            //  !tout = 308.15  ! Outside air temperature (95F, 35.0C)
            //  ! Changed 6/20/01 by FCW to make consistent with Window 4 and 5.
            //  tout = 304.82  ! Outside air temperature (89F, 31.7C)
            //  hcout = 18.86  ! Outside convective film conductance for 3.35 m/s (7.5 mph) wind speed
            //                 ! (average of Window 4 0 m/s and 6.71 m/s values)
            BeamSolarInc = 783.0;
        }

        // IR incident on inside of glazing (inside surround assumed to be
        // a black body at inside air temperature)
        state.dataWindowManager->Rmir = state.dataWindowManager->sigma * pow_4(state.dataWindowManager->tin);

        // IR incident on outside of glazing
        // (outside surround is assumed to be a black body at outside air temperature)
        state.dataWindowManager->Outir = state.dataWindowManager->sigma * pow_4(state.dataWindowManager->tout);

        // Determine whether construction has an exterior or interior shade or blind
        ShadeFlag = WinShadingType::NoShade;
        ShadeRes = 0.0;
        MatOutside = state.dataConstruction->Construct(ConstrNum).LayerPoint(1);
        MatInside = state.dataConstruction->Construct(ConstrNum).LayerPoint(TotLay);
        if (state.dataMaterial->Material(MatOutside).Group == DataHeatBalance::MaterialGroup::Shade) { // Exterior shade present
            MatShade = MatOutside;
            ShadeFlag = WinShadingType::ExtShade;
            // Set glazing outside convection coefficient to Window 4 still-air value
            state.dataWindowManager->hcout = 12.25;
        } else if (state.dataMaterial->Material(MatOutside).Group == DataHeatBalance::MaterialGroup::Screen) { // Exterior screen present
            MatShade = MatOutside;
            ScNum = state.dataMaterial->Material(MatShade).ScreenDataPtr;
            // Orphaned constructs with exterior screen are ignored
            if (ScNum > 0) ShadeFlag = WinShadingType::ExtScreen;
            state.dataWindowManager->hcout = 12.25;
        } else if (state.dataMaterial->Material(MatOutside).Group == DataHeatBalance::MaterialGroup::WindowBlind) { // Exterior blind present
            MatShade = MatOutside;
            ShadeFlag = WinShadingType::ExtBlind;
            BlNum = state.dataMaterial->Material(MatShade).BlindDataPtr;
            state.dataWindowManager->hcout = 12.25;
        } else if (state.dataMaterial->Material(MatInside).Group == DataHeatBalance::MaterialGroup::Shade) { // Interior shade present
            MatShade = MatInside;
            ShadeFlag = WinShadingType::IntShade;
        } else if (state.dataMaterial->Material(MatInside).Group == DataHeatBalance::MaterialGroup::WindowBlind) { // Interior blind present
            MatShade = MatInside;
            BlNum = state.dataMaterial->Material(MatShade).BlindDataPtr;
            ShadeFlag = WinShadingType::IntBlind;
        } else if (TotGlassLay == 2) {
            if (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(3)).Group ==
                DataHeatBalance::MaterialGroup::Shade)
                ShadeFlag = WinShadingType::BGShade;
            if (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(3)).Group ==
                DataHeatBalance::MaterialGroup::WindowBlind)
                ShadeFlag = WinShadingType::BGBlind;
        } else if (TotGlassLay == 3) {
            if (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(5)).Group ==
                DataHeatBalance::MaterialGroup::Shade)
                ShadeFlag = WinShadingType::BGShade;
            if (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(5)).Group ==
                DataHeatBalance::MaterialGroup::WindowBlind)
                ShadeFlag = WinShadingType::BGBlind;
        }

        if (ANY_BETWEENGLASS_SHADE_BLIND(ShadeFlag)) {
            errFlag = 2;
            return;
        }

        TSolNorm = POLYF(1.0, state.dataConstruction->Construct(ConstrNum).TransSolBeamCoef);
        TVisNorm = POLYF(1.0, state.dataConstruction->Construct(ConstrNum).TransVisBeamCoef);
        AbsBeamShadeNorm = 0.0;
        if (ShadeFlag == WinShadingType::IntShade || ShadeFlag == WinShadingType::ExtShade) { // Exterior or interior shade on
            AbsBeamShadeNorm = POLYF(1.0, state.dataConstruction->Construct(ConstrNum).AbsBeamShadeCoef);
            // Exterior blind or screen or interior blind on
        } else if (ShadeFlag == WinShadingType::IntBlind || ShadeFlag == WinShadingType::ExtBlind || ShadeFlag == WinShadingType::ExtScreen) {
            // Find unshaded construction that goes with this construction w/blind or screen
            ConstrNumBare = 0;
            for (ConstrNum1 = 1; ConstrNum1 <= state.dataHeatBal->TotConstructs; ++ConstrNum1) {
                if (ConstrNum1 != ConstrNum && state.dataConstruction->Construct(ConstrNum1).TypeIsWindow &&
                    state.dataConstruction->Construct(ConstrNum1).TotGlassLayers == state.dataConstruction->Construct(ConstrNum1).TotSolidLayers &&
                    state.dataConstruction->Construct(ConstrNum1).TotGlassLayers == state.dataConstruction->Construct(ConstrNum).TotGlassLayers) {
                    // We have an unshaded window construction with the same number of glass layers as ConstrNum;
                    // see if the glass and gas layers match
                    ConstrNumBare = ConstrNum1;
                    for (Lay = 1; Lay <= state.dataConstruction->Construct(ConstrNum1).TotLayers; ++Lay) {
                        LayPtr = state.dataConstruction->Construct(ConstrNum1).LayerPoint(Lay);
                        if (ShadeFlag == WinShadingType::IntBlind) { // The shaded construction has an interior blind
                            LayPtrSh = state.dataConstruction->Construct(ConstrNum).LayerPoint(Lay);
                        } else { // The shaded construction has an exterior blind or screen
                            LayPtrSh = state.dataConstruction->Construct(ConstrNum).LayerPoint(Lay + 1);
                        }
                        if (LayPtrSh != LayPtr) ConstrNumBare = 0;
                    }
                    if (ConstrNumBare != 0) break;
                }
            }
            if (ConstrNumBare == 0) {
                // No matching bare construction found for this construction with blind or screen
                errFlag = 1;
                return;
            }

            TBmBm = POLYF(1.0, state.dataConstruction->Construct(ConstrNumBare).TransSolBeamCoef);
            TBmBmVis = POLYF(1.0, state.dataConstruction->Construct(ConstrNumBare).TransVisBeamCoef);
            if (ShadeFlag == WinShadingType::ExtScreen) {
                //   Don't need to call subroutine, use normal incident properties (SUBROUTINE CalcNominalWindowCond)
                //   Last call to CalcScreenTransmittance(ISurf) was done at direct normal angle (0,0) in CalcWindowScreenProperties
                TScBmBm = state.dataHeatBal->SurfaceScreens(ScNum).BmBmTrans;
                TScBmBmVis = state.dataHeatBal->SurfaceScreens(ScNum).BmBmTransVis;
                TScBmDif = state.dataHeatBal->SurfaceScreens(ScNum).BmDifTrans;
                TScBmDifVis = state.dataHeatBal->SurfaceScreens(ScNum).BmDifTransVis;
                TDif = state.dataConstruction->Construct(ConstrNumBare).TransDiff;
                TDifVis = state.dataConstruction->Construct(ConstrNumBare).TransDiffVis;
                RScBack = state.dataHeatBal->SurfaceScreens(ScNum).ReflectScreen;
                RScBackVis = state.dataHeatBal->SurfaceScreens(ScNum).ReflectScreenVis;
                RScDifBack = state.dataHeatBal->SurfaceScreens(ScNum).DifReflect;
                RScDifBackVis = state.dataHeatBal->SurfaceScreens(ScNum).DifReflectVis;
                RGlFront = POLYF(1.0, state.dataConstruction->Construct(ConstrNumBare).ReflSolBeamFrontCoef);
                RGlFrontVis = POLYF(1.0, state.dataConstruction->Construct(ConstrNumBare).ReflSolBeamFrontCoef);
                RGlDiffFront = state.dataConstruction->Construct(ConstrNumBare).ReflectSolDiffFront;
                RGlDiffFrontVis = state.dataConstruction->Construct(ConstrNumBare).ReflectVisDiffFront;
                TSolNorm = TScBmBm * (TBmBm + TDif * RGlFront * RScBack / (1 - RGlDiffFront * RScDifBack)) +
                           TScBmDif * TDif / (1 - RGlDiffFront * RScDifBack);
                TVisNorm = TScBmBmVis * (TBmBmVis + TDifVis * RGlFrontVis * RScBackVis / (1 - RGlDiffFrontVis * RScDifBackVis)) +
                           TScBmDifVis * TDifVis / (1 - RGlDiffFrontVis * RScDifBackVis);
            } else {
                VarSlats = false;
                if (state.dataHeatBal->Blind(BlNum).SlatAngleType == DataWindowEquivalentLayer::AngleType::Variable) VarSlats = true;
                SlatAng = state.dataHeatBal->Blind(BlNum).SlatAngle * DataGlobalConstants::DegToRadians;
                TBlBmBm = BlindBeamBeamTrans(0.0,
                                             SlatAng,
                                             state.dataHeatBal->Blind(BlNum).SlatWidth,
                                             state.dataHeatBal->Blind(BlNum).SlatSeparation,
                                             state.dataHeatBal->Blind(BlNum).SlatThickness);
                TBmBmBl = TBmBm * TBlBmBm;
                TBmBmBlVis = TBmBmVis * TBlBmBm;
                TBlBmDif = InterpProfSlatAng(0.0, SlatAng, VarSlats, state.dataHeatBal->Blind(BlNum).SolFrontBeamDiffTrans);
                TBlBmDifVis = InterpProfSlatAng(0.0, SlatAng, VarSlats, state.dataHeatBal->Blind(BlNum).VisFrontBeamDiffTrans);
                TDif = state.dataConstruction->Construct(ConstrNumBare).TransDiff;
                TDifVis = state.dataConstruction->Construct(ConstrNumBare).TransDiffVis;
                if (ShadeFlag == WinShadingType::IntBlind) {
                    RGlDiffBack = state.dataConstruction->Construct(ConstrNumBare).ReflectSolDiffBack;
                    RGlDiffBackVis = state.dataConstruction->Construct(ConstrNumBare).ReflectVisDiffBack;
                    RhoBlFront = InterpProfSlatAng(0.0, SlatAng, VarSlats, state.dataHeatBal->Blind(BlNum).SolFrontBeamDiffRefl);
                    RhoBlFrontVis = InterpProfSlatAng(0.0, SlatAng, VarSlats, state.dataHeatBal->Blind(BlNum).VisFrontBeamDiffRefl);
                    AbsBlFront = InterpProfSlatAng(0.0, SlatAng, VarSlats, state.dataHeatBal->Blind(BlNum).SolFrontBeamAbs);
                    RhoBlDiffFront = InterpSlatAng(SlatAng, VarSlats, state.dataHeatBal->Blind(BlNum).SolFrontDiffDiffRefl);
                    RhoBlDiffFrontVis = InterpSlatAng(SlatAng, VarSlats, state.dataHeatBal->Blind(BlNum).VisFrontDiffDiffRefl);
                    AbsBlDiffFront = InterpSlatAng(SlatAng, VarSlats, state.dataHeatBal->Blind(BlNum).SolFrontDiffAbs);
                    AbsBeamShadeNorm = TBmBm * (AbsBlFront + RhoBlFront * RGlDiffBack * AbsBlDiffFront / (1.0 - RhoBlDiffFront * RGlDiffBack));
                    TBlDifDif = InterpSlatAng(SlatAng, VarSlats, state.dataHeatBal->Blind(BlNum).SolFrontDiffDiffTrans);
                    TBlDifDifVis = InterpSlatAng(SlatAng, VarSlats, state.dataHeatBal->Blind(BlNum).VisFrontDiffDiffTrans);
                    TSolNorm = TBmBm * (TBlBmBm + TBlBmDif + TBlDifDif * RhoBlFront * RGlDiffBack / (1.0 - RhoBlDiffFront * RGlDiffBack));
                    //     use of TBlBmBm here is correct, visible and IR transmittance are the same (reference deleted CR6925 on 3/20/2006)
                    TVisNorm = TBmBmVis *
                               (TBlBmBm + TBlBmDifVis + TBlDifDifVis * RhoBlFrontVis * RGlDiffBackVis / (1.0 - RhoBlDiffFrontVis * RGlDiffBackVis));
                } // (IntBlind)
                if (ShadeFlag == WinShadingType::ExtBlind) {
                    TBlBmBm = BlindBeamBeamTrans(0.0,
                                                 SlatAng,
                                                 state.dataHeatBal->Blind(BlNum).SlatWidth,
                                                 state.dataHeatBal->Blind(BlNum).SlatSeparation,
                                                 state.dataHeatBal->Blind(BlNum).SlatThickness);
                    RGlFront = POLYF(1.0, state.dataConstruction->Construct(ConstrNumBare).ReflSolBeamFrontCoef);
                    RGlFrontVis = POLYF(1.0, state.dataConstruction->Construct(ConstrNumBare).ReflSolBeamFrontCoef);
                    AbsBlFront = InterpProfSlatAng(0.0, SlatAng, VarSlats, state.dataHeatBal->Blind(BlNum).SolFrontBeamAbs);
                    AbsBlBack = InterpProfSlatAng(0.0, SlatAng, VarSlats, state.dataHeatBal->Blind(BlNum).SolBackBeamAbs);
                    AbsBlDiffBack = InterpSlatAng(SlatAng, VarSlats, state.dataHeatBal->Blind(BlNum).SolBackDiffAbs);
                    RGlDiffFront = state.dataConstruction->Construct(ConstrNumBare).ReflectSolDiffFront;
                    RGlDiffFrontVis = state.dataConstruction->Construct(ConstrNumBare).ReflectVisDiffFront;
                    RhoBlDiffBack = InterpSlatAng(SlatAng, VarSlats, state.dataHeatBal->Blind(BlNum).SolBackDiffDiffRefl);
                    RhoBlDiffBackVis = InterpSlatAng(SlatAng, VarSlats, state.dataHeatBal->Blind(BlNum).VisBackDiffDiffRefl);
                    RhoBlBack = InterpProfSlatAng(0.0, SlatAng, VarSlats, state.dataHeatBal->Blind(BlNum).SolBackBeamDiffRefl);
                    RhoBlBackVis = InterpProfSlatAng(0.0, SlatAng, VarSlats, state.dataHeatBal->Blind(BlNum).SolBackBeamDiffRefl);
                    AbsBeamShadeNorm =
                        AbsBlFront + AbsBlBack * RGlFront * TBlBmBm +
                        (AbsBlDiffBack * RGlDiffFront / (1.0 - RhoBlDiffBack * RGlDiffFront)) * (RGlFront * TBlBmBm * RhoBlBack + TBlBmDif);
                    RGlDiffFront = state.dataConstruction->Construct(ConstrNumBare).ReflectSolDiffFront;
                    TSolNorm = TBlBmBm * (TBmBm + TDif * RGlFront * RhoBlBack / (1 - RGlDiffFront * RhoBlDiffBack)) +
                               TBlBmDif * TDif / (1.0 - RGlDiffFront * RhoBlDiffBack);
                    TVisNorm = TBlBmBm * (TBmBmVis + TDifVis * RGlFrontVis * RhoBlBackVis / (1 - RGlDiffFrontVis * RhoBlDiffBackVis)) +
                               TBlBmDifVis * TDifVis / (1.0 - RGlDiffFrontVis * RhoBlDiffBackVis);
                } // (ExtBlind)
            }     // (Screen or Blind)
        }         // (Shade, Blind, or Screen)

        // Fill the layer properties needed for the thermal calculation.

        // The layer and face numbering are as follows (for the triple glazing case):
        // Glass layers are 1,2 and 3, where 1 is the outside (outside environment facing)
        //   layer and 3 is the inside (room-facing) layer;
        // Faces (also called surfaces) are 1,2,3,4,5 and 6, where face 1 is the
        //   outside (front) face of glass layer 1, face 2 is the inside (back)
        //   face of glass layer 1, face 3 is the outer face of glass layer 2, face 4 is the
        //   inner face of glass layer 2, etc.
        // Gap layers are 1 and 2, where gap layer 1 is between glass layers 1 and 2
        //   and gap layer 2 is between glass layers 2 and 3.

        IGlass = 0;
        IGap = 0;

        for (Lay = 1; Lay <= TotLay; ++Lay) {
            LayPtr = state.dataConstruction->Construct(ConstrNum).LayerPoint(Lay);
            if ((state.dataMaterial->Material(LayPtr).Group == DataHeatBalance::MaterialGroup::WindowGlass) ||
                (state.dataMaterial->Material(LayPtr).Group == DataHeatBalance::MaterialGroup::WindowSimpleGlazing)) {
                ++IGlass;
                state.dataWindowManager->thick[IGlass - 1] = state.dataMaterial->Material(LayPtr).Thickness;
                state.dataWindowManager->scon[IGlass - 1] =
                    state.dataMaterial->Material(LayPtr).Conductivity / state.dataMaterial->Material(LayPtr).Thickness;
                state.dataWindowManager->emis[2 * IGlass - 2] = state.dataMaterial->Material(LayPtr).AbsorpThermalFront;
                state.dataWindowManager->emis[2 * IGlass - 1] = state.dataMaterial->Material(LayPtr).AbsorpThermalBack;
                state.dataWindowManager->tir[2 * IGlass - 2] = state.dataMaterial->Material(LayPtr).TransThermal;
                state.dataWindowManager->tir[2 * IGlass - 1] = state.dataMaterial->Material(LayPtr).TransThermal;
                AbsBeamNorm(IGlass) = POLYF(1.0, state.dataConstruction->Construct(ConstrNum).AbsBeamCoef(IGlass));
                if (ShadeFlag == WinShadingType::IntBlind) { // Interior blind on
                    AbsBeamNorm(IGlass) = POLYF(1.0, state.dataConstruction->Construct(ConstrNumBare).AbsBeamCoef(IGlass));
                    AGlDiffBack = state.dataConstruction->Construct(ConstrNumBare).AbsDiffBack(IGlass);
                    AbsBeamNorm(IGlass) += TBmBm * AGlDiffBack * RhoBlFront / (1.0 - RhoBlFront * RGlDiffBack);
                } else if (ShadeFlag == WinShadingType::ExtBlind) { // Exterior blind on
                    AbsBeamNorm(IGlass) = POLYF(1.0, state.dataConstruction->Construct(ConstrNumBare).AbsBeamCoef(IGlass));
                    AbsBeamNorm(IGlass) = TBlBmBm * AbsBeamNorm(IGlass) + (TBlBmBm * RGlFront * RhoBlBack + TBlBmDif) *
                                                                              state.dataConstruction->Construct(ConstrNumBare).AbsDiff(IGlass) /
                                                                              (1.0 - RGlDiffFront * RhoBlDiffBack);
                } else if (ShadeFlag == WinShadingType::ExtScreen) { // Exterior screen on
                    AbsBeamNorm(IGlass) = POLYF(1.0, state.dataConstruction->Construct(ConstrNumBare).AbsBeamCoef(IGlass));
                    AbsBeamNorm(IGlass) = TScBmBm * AbsBeamNorm(IGlass) + (TScBmBm * RGlFront * RScBack + TScBmDif) *
                                                                              state.dataConstruction->Construct(ConstrNumBare).AbsDiff(IGlass) /
                                                                              (1.0 - RGlDiffFront * RScDifBack);
                }
                state.dataWindowManager->AbsRadGlassFace[2 * IGlass - 2] = 0.5 * BeamSolarInc * AbsBeamNorm(IGlass);
                state.dataWindowManager->AbsRadGlassFace[2 * IGlass - 1] = 0.5 * BeamSolarInc * AbsBeamNorm(IGlass);
            }
            if (state.dataMaterial->Material(LayPtr).Group == DataHeatBalance::MaterialGroup::WindowGas ||
                state.dataMaterial->Material(LayPtr).Group == DataHeatBalance::MaterialGroup::WindowGasMixture ||
                state.dataMaterial->Material(LayPtr).Group == DataHeatBalance::MaterialGroup::ComplexWindowGap) { // Gap layer
                ++IGap;
                // Simon: Need to re-reference gas data in casee of complex fenestration gap
                if (state.dataMaterial->Material(LayPtr).Group == DataHeatBalance::MaterialGroup::ComplexWindowGap) {
                    LayPtr = state.dataMaterial->Material(LayPtr).GasPointer;
                }
                state.dataWindowManager->gap[IGap - 1] = state.dataMaterial->Material(LayPtr).Thickness;
                state.dataWindowManager->gnmix[IGap - 1] = state.dataMaterial->Material(LayPtr).NumberOfGasesInMixture;
                for (IMix = 1; IMix <= state.dataWindowManager->gnmix[IGap - 1]; ++IMix) {
                    state.dataWindowManager->gwght[IMix - 1][IGap - 1] = state.dataMaterial->Material(LayPtr).GasWght(IMix);
                    state.dataWindowManager->gfract[IMix - 1][IGap - 1] = state.dataMaterial->Material(LayPtr).GasFract(IMix);
                    for (ICoeff = 1; ICoeff <= 3; ++ICoeff) {
                        state.dataWindowManager->gcon[ICoeff - 1][IMix - 1][IGap - 1] = state.dataMaterial->Material(LayPtr).GasCon(ICoeff, IMix);
                        state.dataWindowManager->gvis[ICoeff - 1][IMix - 1][IGap - 1] = state.dataMaterial->Material(LayPtr).GasVis(ICoeff, IMix);
                        state.dataWindowManager->gcp[ICoeff - 1][IMix - 1][IGap - 1] = state.dataMaterial->Material(LayPtr).GasCp(ICoeff, IMix);
                    }
                }
            }
        }

        // Factors used in glass temperature solution
        if (state.dataWindowManager->ngllayer >= 2) {
            state.dataWindowManager->A23P =
                -state.dataWindowManager->emis[2] / (1.0 - (1.0 - state.dataWindowManager->emis[1]) * (1.0 - state.dataWindowManager->emis[2]));
            state.dataWindowManager->A32P =
                state.dataWindowManager->emis[1] / (1.0 - (1.0 - state.dataWindowManager->emis[1]) * (1.0 - state.dataWindowManager->emis[2]));
            state.dataWindowManager->A23 = state.dataWindowManager->emis[1] * state.dataWindowManager->sigma * state.dataWindowManager->A23P;
        }

        if (state.dataWindowManager->ngllayer >= 3) {
            state.dataWindowManager->A45P =
                -state.dataWindowManager->emis[4] / (1.0 - (1.0 - state.dataWindowManager->emis[3]) * (1.0 - state.dataWindowManager->emis[4]));
            state.dataWindowManager->A54P =
                state.dataWindowManager->emis[3] / (1.0 - (1.0 - state.dataWindowManager->emis[3]) * (1.0 - state.dataWindowManager->emis[4]));
            state.dataWindowManager->A45 = state.dataWindowManager->emis[3] * state.dataWindowManager->sigma * state.dataWindowManager->A45P;
        }

        if (state.dataWindowManager->ngllayer == 4) {
            state.dataWindowManager->A67P =
                -state.dataWindowManager->emis[6] / (1.0 - (1.0 - state.dataWindowManager->emis[5]) * (1.0 - state.dataWindowManager->emis[6]));
            state.dataWindowManager->A76P =
                state.dataWindowManager->emis[5] / (1.0 - (1.0 - state.dataWindowManager->emis[5]) * (1.0 - state.dataWindowManager->emis[6]));
            state.dataWindowManager->A67 = state.dataWindowManager->emis[5] * state.dataWindowManager->sigma * state.dataWindowManager->A67P;
        }

        state.dataWindowManager->thetas = {0.0};

        WindowTempsForNominalCond(state, ConstrNum, hgap, 1.0);

        if (!ANY_INTERIOR_SHADE_BLIND(ShadeFlag)) AbsBeamShadeNorm = 0.0;

        // Get center-of-glass conductance and solar heat gain coefficient
        // including inside and outside air films
        Real64 inputU = state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).SimpleWindowUfactor;

        // Calculate the NominalConductance glazing only (before adjusted)
        EvalNominalWindowCond(state, AbsBeamShadeNorm, AbsBeamNorm, hgap, NominalConductance, SHGC, TSolNorm);

        if (WinterSummerFlag == 1) {
            state.dataHeatBal->NominalUBeforeAdjusted(ConstrNum) = NominalConductance;
            if (inputU > 0) {                  // only compute adjustment ratio when there is valid user input U
                Real64 wettedAreaAdjRatio = 1; // Adjustment ratio for the wetted area
                Real64 hcoutRated = state.dataWindowManager->hcout;
                // Adjustment ratio applies to convective film coefficients when input U value is above the limit of the simple glazing nominal U
                // Representing the nominal highly conductive frame effects. Solved iteratively.
                Real64 adjLower = 1.0;
                Real64 adjUpper = 2.0;
                int MaxIter = 100;
                while (std::abs(inputU - NominalConductance) > 0.01 && MaxIter > 0) {
                    wettedAreaAdjRatio = (adjLower + adjUpper) / 2;
                    WindowTempsForNominalCond(
                        state, ConstrNum, hgap, wettedAreaAdjRatio); // reeval hcout at each iteration, hcin is not linear to wetted area
                    state.dataWindowManager->hcout = hcoutRated * wettedAreaAdjRatio; // reeval hcout
                    EvalNominalWindowCond(state, AbsBeamShadeNorm, AbsBeamNorm, hgap, NominalConductance, SHGC, TSolNorm);
                    if (NominalConductance < inputU) {
                        adjLower = wettedAreaAdjRatio;
                    } else {
                        adjUpper = wettedAreaAdjRatio;
                    }
                    MaxIter -= 1;
                }
                state.dataHeatBal->CoeffAdjRatio(ConstrNum) = wettedAreaAdjRatio;
            }
        }

        // EPTeam - again -- believe that is enforced in input //Autodesk But this routine is not self-protecting: Add as an assert

        // init the surface convective and radiative adjustment ratio
        for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
            int const firstSurfWin = state.dataHeatBal->Zone(zoneNum).WindowSurfaceFirst;
            int const lastSurfWin = state.dataHeatBal->Zone(zoneNum).WindowSurfaceLast;
            for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
                if (state.dataSurface->Surface(SurfNum).ExtBoundCond == ExternalEnvironment) {
                    int ConstrNum2 = state.dataSurface->Surface(SurfNum).Construction;
                    state.dataHeatBalSurf->SurfWinCoeffAdjRatio(SurfNum) = state.dataHeatBal->CoeffAdjRatio(ConstrNum2);
                }
            }
        }

        // Need to add variables writing here since this routine will override previously calcualted values from WinCalc-Engine
        if (state.dataWindowManager->inExtWindowModel->isExternalLibraryModel()) {
            TSolNorm = GetSolarTransDirectHemispherical(state, ConstrNum);
            TVisNorm = GetVisibleTransDirectHemispherical(state, ConstrNum);
        }
    }

    void EvalNominalWindowCond(EnergyPlusData &state,
                               Real64 const AbsBeamShadeNorm,     // Shade solar absorptance at normal incidence
                               Array1D<Real64> const AbsBeamNorm, // Beam absorptance at normal incidence for each glass layer
                               Array1D<Real64> const hgap,        // Conductive gap conductance [W/m2-K]
                               Real64 &NominalConductance,        // Nominal center-of-glass conductance, including air films
                               Real64 &SHGC,                      // Nominal center-of-glass solar heat gain coefficient for
                               Real64 &TSolNorm                   // Overall beam solar transmittance at normal incidence
    )
    {
        Array1D<Real64> hGapTot(5); // Combined radiative and conductive gap conductance [W/m2-K]

        Real64 hOutRad = state.dataWindowManager->emis[0] * state.dataWindowManager->sigma * 0.5 *
                         pow_3(state.dataWindowManager->tout + state.dataWindowManager->thetas[0]);
        Real64 rOut = 1.0 / (hOutRad + state.dataWindowManager->hcout);
        Real64 hInRad = state.dataWindowManager->emis[state.dataWindowManager->nglface - 1] * state.dataWindowManager->sigma * 0.5 *
                        pow_3(state.dataWindowManager->tin + state.dataWindowManager->thetas[state.dataWindowManager->nglface - 1]);
        Real64 rIn = 1.0 / (hInRad + state.dataWindowManager->hcin);
        Real64 Rbare = 0;

        switch (state.dataWindowManager->ngllayer) {
        // the switch cases here are just the integer number of layers, not exactly "magic" numbers 1, 2, 3. and 4.
        case 1:
            Rbare = 1.0 / state.dataWindowManager->scon[0];
            state.dataWindowManager->Rtot = rOut + Rbare + rIn;
            SHGC = AbsBeamNorm(1) * (rOut + (0.5 / state.dataWindowManager->scon[0])) /
                   state.dataWindowManager->Rtot; // BG changed for CR7682 (solar absorbed in middle of layer)
            SHGC += AbsBeamShadeNorm;
            SHGC += TSolNorm;
            break;

        case 2:
            hGapTot(1) = hgap(1) + std::abs(state.dataWindowManager->A23) * 0.5 *
                                       pow_3(state.dataWindowManager->thetas[1] + state.dataWindowManager->thetas[2]);
            Rbare = 1.0 / state.dataWindowManager->scon[0] + 1.0 / hGapTot(1) + 1.0 / state.dataWindowManager->scon[1];
            state.dataWindowManager->Rtot = rOut + Rbare + rIn;
            SHGC = AbsBeamNorm(1) * (rOut + 0.5 / state.dataWindowManager->scon[0]) / state.dataWindowManager->Rtot +
                   AbsBeamNorm(2) * (rOut + 1.0 / state.dataWindowManager->scon[0] + 1.0 / hGapTot(1) + 0.5 / state.dataWindowManager->scon[1]) /
                       state.dataWindowManager->Rtot; // CR7682
            SHGC += AbsBeamShadeNorm;
            SHGC += TSolNorm;
            break;

        case 3:
            hGapTot(1) = hgap(1) + std::abs(state.dataWindowManager->A23) * 0.5 *
                                       pow_3(state.dataWindowManager->thetas[1] + state.dataWindowManager->thetas[2]);
            hGapTot(2) = hgap(2) + std::abs(state.dataWindowManager->A45) * 0.5 *
                                       pow_3(state.dataWindowManager->thetas[3] + state.dataWindowManager->thetas[4]);
            Rbare = 1.0 / state.dataWindowManager->scon[0] + 1.0 / hGapTot(1) + 1.0 / state.dataWindowManager->scon[1] + 1.0 / hGapTot(2) +
                    1.0 / state.dataWindowManager->scon[2];
            state.dataWindowManager->Rtot = rOut + Rbare + rIn;
            SHGC = AbsBeamNorm(1) * (rOut + 0.5 / state.dataWindowManager->scon[0]) / state.dataWindowManager->Rtot +
                   AbsBeamNorm(2) * (rOut + 1.0 / state.dataWindowManager->scon[0] + 1.0 / hGapTot(1) + 0.5 / state.dataWindowManager->scon[1]) /
                       state.dataWindowManager->Rtot +
                   AbsBeamNorm(3) *
                       (rOut + 1.0 / state.dataWindowManager->scon[0] + 1.0 / hGapTot(1) + 1.0 / state.dataWindowManager->scon[1] + 1.0 / hGapTot(2) +
                        0.5 / state.dataWindowManager->scon[2]) /
                       state.dataWindowManager->Rtot;
            SHGC += AbsBeamShadeNorm;
            SHGC += TSolNorm;
            break;

        case 4:
            hGapTot(1) = hgap(1) + std::abs(state.dataWindowManager->A23) * 0.5 *
                                       pow_3(state.dataWindowManager->thetas[1] + state.dataWindowManager->thetas[2]);
            hGapTot(2) = hgap(2) + std::abs(state.dataWindowManager->A45) * 0.5 *
                                       pow_3(state.dataWindowManager->thetas[3] + state.dataWindowManager->thetas[4]);
            hGapTot(3) = hgap(3) + std::abs(state.dataWindowManager->A67) * 0.5 *
                                       pow_3(state.dataWindowManager->thetas[5] + state.dataWindowManager->thetas[6]);
            Rbare = 1.0 / state.dataWindowManager->scon[0] + 1.0 / hGapTot(1) + 1.0 / state.dataWindowManager->scon[1] + 1.0 / hGapTot(2) +
                    1.0 / state.dataWindowManager->scon[2] + 1.0 / hGapTot(3) + 1.0 / state.dataWindowManager->scon[3];
            state.dataWindowManager->Rtot = rOut + Rbare + rIn;
            SHGC = AbsBeamNorm(1) * (rOut + 0.5 / state.dataWindowManager->scon[0]) / state.dataWindowManager->Rtot +
                   AbsBeamNorm(2) * (rOut + 1.0 / state.dataWindowManager->scon[0] + 1.0 / hGapTot(1) + 0.5 / state.dataWindowManager->scon[1]) /
                       state.dataWindowManager->Rtot +
                   AbsBeamNorm(3) *
                       (rOut + 1.0 / state.dataWindowManager->scon[0] + 1.0 / hGapTot(1) + 1.0 / state.dataWindowManager->scon[1] + 1.0 / hGapTot(2) +
                        0.5 / state.dataWindowManager->scon[2]) /
                       state.dataWindowManager->Rtot +
                   AbsBeamNorm(4) *
                       (rOut + 1.0 / state.dataWindowManager->scon[0] + 1.0 / hGapTot(1) + 1.0 / state.dataWindowManager->scon[1] + 1.0 / hGapTot(2) +
                        1.0 / state.dataWindowManager->scon[2] + 1.0 / hGapTot(3) + 0.5 / state.dataWindowManager->scon[3]) /
                       state.dataWindowManager->Rtot; // CR7682
            SHGC += AbsBeamShadeNorm;
            SHGC += TSolNorm;
            break;
        default:
            break;
        }
        NominalConductance = 1.0 / (rOut + Rbare + rIn);
    }
    //****************************************************************************

    void WindowTempsForNominalCond(EnergyPlusData &state,
                                   int const ConstrNum,  // Construction number
                                   Array1A<Real64> hgap, // Gap gas conductive conductance (W/m2-K)
                                   Real64 const adjRatio // adjusment Ratio to hcin
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         F. Winkelmann
        //       DATE WRITTEN   September 2000
        //       MODIFIED       Nov 2002, FW: increase MaxIterations from 15 to 100, add face
        //                       temperature relaxation, and increase convergence tolerance by
        //                       a factor of 10 if no convergence after MaxIterations,
        //                       all for consistency with SolveForWindowTemperatures.
        //                      Mar 2003, FW: increase convergence tolerance from 0.01 to 0.02;
        //                       remove redundant relaxation on radiative conductances (both of
        //                       these were also done in SolveForWindowTemperatures).
        //                      Jan 2009, BG: changed interior convection coefficient correlation to match
        //                       ISO 15099.
        //                      Feb 2009, BG: extended coefficient to include absorbed radiation
        //                       to cover summer conditions for SHGC determination.
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This is a shortened form of SolveForWindowTemperatures tailored
        // for calculation of the nominal center-of-glass U-value for a window
        // construction at ASHRAE winter conditions and for determining conditions at
        // summer conditions for calculationg SHGC.
        // Evaluates the coefficients Aface and Bface in the system of linear
        // algebraic equations
        //     Sum    [Aface(i,j)*thetas(j)] = Bface(i), i = 1,nglface
        //  j=1,nglface
        // where
        // nglface = number of glass faces (= 2 * number of layers) and
        // thetas(j) = temperature of face j

        // METHODOLOGY EMPLOYED:
        // The Aface and Bface coefficients are determined by the equations for
        // heat balance at the glass faces. The system of linear equations is solved
        // by LU decomposition.

        using Psychrometrics::PsyRhoAirFnPbTdbW;

        // Argument array dimensioning
        hgap.dim(5);

        int constexpr MaxIterations(100);  // Maximum allowed number of iterations
        Real64 constexpr errtemptol(0.02); // Tolerance on errtemp for convergence
        static constexpr std::string_view RoutineName("WindowTempsForNominalCond");

        int i;                  // Counter
        Array1D<Real64> hr(10); // Radiative conductance (W/m2-K)
        Real64 hcinprev;        // Value of hcin from previous iteration
        Real64 d;               // +1 if number of row interchanges is even,
        // -1 if odd (in LU decomposition)
        Array1D_int indx(10);          // Vector of row permutations in LU decomposition
        Array2D<Real64> Aface(10, 10); // Coefficient in equation Aface*thetas = Bface
        Array1D<Real64> Bface(10);     // Coefficient in equation Aface*thetas = Bface
        int iter;                      // Iteration number
        Real64 errtemp;                // Absolute value of sum of face temperature differences
        //   between iterations, divided by number of faces
        Real64 TmeanFilm;       // mean film temperature
        Real64 TmeanFilmKelvin; // mean film temperature for property evaluation
        Real64 rho;             // density of (apparently dry) air [kg/m3]
        Real64 g;               // acceleration due to gravity [m/s2]
        Real64 Height;          // window cavity height [m]
        Real64 Cp;              // specific heat of air [J/kg-K]
        Real64 lambda;          // thermal conductivity of air [W/m-K]
        Real64 mu;              // dynamic viscosity of air [kg/m-s]
        Real64 RaH;             // Rayleigh number for cavity height [ Non dim]
        Real64 TiltDeg;         // glazing tilt in degrees
        Real64 sineTilt;        // sine of glazing tilt
        Real64 Nuint;           // Nusselt number for interior surface convection

        iter = 0;

        // Initialize face temperatures
        StartingWinTempsForNominalCond(state);

        // Calculate radiative conductance
        errtemp = errtemptol * 2.0;

        TiltDeg = 90.0;

        sineTilt = std::sin(TiltDeg * DataGlobalConstants::DegToRadians); // degrees as arg

        while (iter < MaxIterations && errtemp > errtemptol) {
            for (i = 1; i <= state.dataWindowManager->nglface; ++i) {
                hr(i) = state.dataWindowManager->emis[i - 1] * state.dataWindowManager->sigma * pow_3(state.dataWindowManager->thetas[i - 1]);
                //! fw 3/4/03 if ( iter >= 1 ) hr(i) = 0.5*(hrprev(i)+hr(i))
            }

            Aface = 0.0;
            Bface = 0.0;

            // Inside convective film conductance for vertical window
            if (iter >= 1) {
                hcinprev = state.dataWindowManager->hcin;
            }
            // CR7670 BG this next correlation was used for hcin but is not "standard" for windows
            //  hcin = 1.31d0*((ABS(thetas(nglface)-tin))**0.3333d0)
            // Begin calculating for ISO 15099 method.
            // mean film temperature
            TmeanFilmKelvin = state.dataWindowManager->tin + 0.25 * (state.dataWindowManager->thetas[state.dataWindowManager->nglface - 1] -
                                                                     state.dataWindowManager->tin); // eq. 133 in ISO 15099
            TmeanFilm = TmeanFilmKelvin - 273.15;
            // the following properties are constants or linear relations for "standard" type reporting
            rho = PsyRhoAirFnPbTdbW(state, 101325.0, TmeanFilm, 0.0, RoutineName); // dry air assumption
            g = 9.81;
            Height = 1.0; // standard window rating practice is to use 1 meter (rather than actual)

            lambda = 2.873E-3 + 7.76E-5 * TmeanFilmKelvin; // Table B.1 in ISO 15099
            mu = 3.723E-6 + 4.94E-8 * TmeanFilmKelvin;     // Table B.2 in ISO 15099
            Cp = 1002.737 + 1.2324E-2 * TmeanFilmKelvin;   // Table B.3 in ISO 15099

            RaH = (pow_2(rho) * pow_3(Height) * g * Cp *
                   (std::abs(state.dataWindowManager->thetas[state.dataWindowManager->nglface - 1] - state.dataWindowManager->tin))) /
                  (TmeanFilmKelvin * mu * lambda); // eq 132 in ISO 15099

            Nuint = 0.56 * root_4(RaH * sineTilt); // eq. 135 in ISO 15099 (only need this one because tilt is 90 deg

            state.dataWindowManager->hcin = Nuint * lambda / Height;

            // End calculations for ISO 15099 method.

            if (iter >= 1) state.dataWindowManager->hcin = 0.5 * (hcinprev + state.dataWindowManager->hcin);

            state.dataWindowManager->hcin *= adjRatio;

            ++iter;

            GetHeatBalanceEqCoefMatrixSimple(state, state.dataWindowManager->ngllayer, hr, hgap, Aface, Bface);

            LUdecomposition(state, Aface, state.dataWindowManager->nglface, indx,
                            d); // Note that these routines change Aface;
            LUsolution(Aface, state.dataWindowManager->nglface, indx,
                       Bface); // face temperatures are returned in Bface

            errtemp = 0.0;
            for (i = 1; i <= state.dataWindowManager->nglface; ++i) {
                errtemp += std::abs(state.dataWindowManager->thetas[i - 1] - Bface(i)) / state.dataWindowManager->nglface;
            }

            for (i = 1; i <= state.dataWindowManager->nglface; ++i) {
                state.dataWindowManager->thetas[i - 1] = 0.5 * (state.dataWindowManager->thetas[i - 1] + Bface(i));
            }
        }

        // No convergence after MaxIterations; and/or error tolerance
        if (errtemp >= 10 * errtemptol) {
            // Fatal error: didn't converge
            ShowFatalError(state,
                           "Convergence error in WindowTempsForNominalCond for construction " + state.dataConstruction->Construct(ConstrNum).Name);
        }
    }

    //****************************************************************************

    void StartingWinTempsForNominalCond(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         F. Winkelmann
        //       DATE WRITTEN   September 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Initializes face temperature distribution prior to iteration.
        // This is a shortened form of StartingWindowTemps for use in calculating
        // the nominal center-of-glass U-value.

        Real64 constexpr hrad(5.3);           // Typical radiative conductance (W/m2-K)
        Real64 constexpr hcinStartValue(3.2); // Starting value for inside air film convective
        //   conductance (estimated for typical double glazing
        //   using 1.31(dT**0.333), where dT =
        //   room air temp - inside surface temp = 14.2K)
        Real64 constexpr resgap(0.21); // Typical gap resistance (m2-K/W)

        int i;                      // Face counter
        Array1D<Real64> rguess(11); // Combined radiative/convective resistance (m2-K/W) of
        // inside or outside air film, or gap
        Real64 restot; // Total window resistance including outside
        //   and inside air films (m2-K/W)
        Real64 temdiff; // Inside/outside air temperature difference (K)
        Real64 ressum;  // Resistance sum (m2-K/W)

        rguess(1) = 1.0 / (state.dataWindowManager->hcout + hrad);
        rguess(state.dataWindowManager->nglface + 1) = 1.0 / (hcinStartValue + hrad);

        for (i = 2; i <= state.dataWindowManager->nglface; i += 2) {
            rguess(i) = 1.0 / state.dataWindowManager->scon[i / 2 - 1];
            if (i < state.dataWindowManager->nglface) rguess(i + 1) = resgap;
        }
        restot = 0.0;

        for (i = 1; i <= state.dataWindowManager->nglface + 1; ++i) {
            restot += rguess(i);
        }

        temdiff = state.dataWindowManager->tin - state.dataWindowManager->tout;
        if (std::abs(temdiff) < 0.5) temdiff = 2.0;
        ressum = 0.0;

        for (i = 1; i <= state.dataWindowManager->nglface; ++i) {
            ressum += rguess(i);
            state.dataWindowManager->thetas[i - 1] = (ressum / restot) * temdiff + state.dataWindowManager->tout;
        }
    }

    //****************************************************************************

    void ReportGlass(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   March 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine gives a detailed report to the user about
        // the calculation parameters for windows and their associated
        // materials.

        using General::POLYF;

        using General::ScanForReports;
        using WindowComplexManager::CalcComplexWindowThermal;
        using WindowComplexManager::UpdateComplexWindows;

        static Array1D_string const Roughness({0, 5}, {"VeryRough", "Rough", "MediumRough", "MediumSmooth", "Smooth", "VerySmooth"});
        static Array1D_string const GasTypeName({0, 4}, {"Custom", "Air", "Argon", "Krypton", "Xenon"});

        Real64 TempVar(0.0); // just temporary usage for complex fenestration

        int ThisNum;
        int Layer;
        int BlNum; // Blind number
        int i;
        Real64 NominalConductanceWinter; // Nominal center-of-glass conductance of a window construction
        // for ASHRAE winter conditions (W/m2-K):
        // Inside air temperature = 21.1C (70F)
        // Outside air temperature = -17.8C (0F)
        // Windspeed = 6.71 m/s (15 mph)
        // No solar radiation
        Real64 NominalConductanceSummer; // Nominal center-of-glass conductance of a window construction
        // for ASHRAE summer conditions (W/m2-K):
        // Inside air temperature = 23.9C (75F)
        // Outside air temperature = 35.0C (95F)
        // Windspeed = 3.35 m/s (7.5 mph)
        // 783 W/m2 (248 Btu/h-ft2) incident beam solar radiation normal to glazing
        Real64 SHGCWinter(0.0); // Center-of-glass solar heat gain coefficient for ASHRAE
        Real64 SHGCSummer(0.0);
        // winter and summer conditions
        Real64 TransSolNorm;        // Window construction solar transmittance at normal incidence
        Real64 TransVisNorm;        // Window construction visible transmittance at normal incidence
        int errFlag;                // Error flag
        std::string SolarDiffusing; // 'Yes' if glass is solar diffusing; otherwise 'No' (clear glass)
        std::string SpectralDataName;
        std::string OpticalDataType;
        std::string SlateOrientation;
        std::string GapVentType;

        ScanForReports(state, "Constructions", state.dataWindowManager->DoReport, "Constructions");

        if (std::any_of(state.dataConstruction->Construct.begin(),
                        state.dataConstruction->Construct.end(),
                        [](Construction::ConstructionProps const &e) { return e.TypeIsWindow; }))
            state.dataWindowManager->HasWindows = true;
        if (std::any_of(state.dataConstruction->Construct.begin(),
                        state.dataConstruction->Construct.end(),
                        [](Construction::ConstructionProps const &e) { return e.WindowTypeBSDF; }))
            state.dataWindowManager->HasComplexWindows = true; // Yes, this is a bit different than actually using them.
        if (std::any_of(state.dataConstruction->Construct.begin(),
                        state.dataConstruction->Construct.end(),
                        [](Construction::ConstructionProps const &e) { return e.WindowTypeEQL; }))
            state.dataWindowManager->HasEQLWindows = true; // for reporting purpose only
        if (state.dataWindowManager->DoReport &&
            (state.dataWindowManager->HasWindows || state.dataWindowManager->HasComplexWindows || state.dataWindowManager->HasEQLWindows)) {
            //                                      Write Descriptions
            print(state.files.eio,
                  "{}\n",
                  "! <WindowConstruction>,Construction Name,Index,#Layers,Roughness,Conductance {W/m2-K},Conductance (Before Adjusted) {W/m2-K},"
                  "Convection Coefficient Adjustment Ratio,SHGC,"
                  "Solar Transmittance at Normal Incidence,Visible Transmittance at Normal Incidence");
            if ((state.dataHeatBal->TotSimpleWindow > 0) || (state.dataHeatBal->W5GlsMat > 0) || (state.dataHeatBal->W5GlsMatAlt > 0))
                print(state.files.eio,
                      "{}\n",
                      "! <WindowMaterial:Glazing>, Material Name, Optical Data Type, Spectral Data Set Name, "
                      "Thickness {m}, Solar Transmittance,Front Solar Reflectance, Back Solar Reflectance, Visible "
                      "Transmittance, Front Visible Reflectance,Back Visible Reflectance,Infrared Transmittance, "
                      "Front Thermal Emissivity, Back Thermal Emissivity,Conductivity {W/m-K},Dirt Factor,Solar "
                      "Diffusing");
            if ((state.dataHeatBal->W5GasMat > 0) || (state.dataHeatBal->W5GasMatMixture > 0))
                print(state.files.eio, "{}\n", "! <WindowMaterial:Gas>,Material Name,GasType,Thickness {m}");
            if (state.dataHeatBal->TotShades > 0)
                print(state.files.eio,
                      "{}\n",
                      "! <WindowMaterial:Shade>,Material Name,Thickness {m},Conductivity {W/m-K},Thermal "
                      "Absorptance,Transmittance,Visible Transmittance,Shade Reflectance");
            if (state.dataHeatBal->TotScreens > 0)
                print(state.files.eio,
                      "{}\n",
                      "! <WindowMaterial:Screen>,Material Name,Thickness {m},Conductivity {W/m-K},Thermal "
                      "Absorptance,Transmittance,Reflectance,Visible Reflectance,Diffuse Reflectance,Diffuse Visible "
                      "Reflectance,Screen Material Diameter To Spacing Ratio,Screen To GlassDistance {m}");
            if (state.dataHeatBal->TotBlinds > 0)
                print(state.files.eio,
                      "{}\n",
                      "! <WindowMaterial:Blind>,Material Name,Slat Width {m},Slat Separation {m},Slat Thickness "
                      "{m},Slat Angle {deg},Slat Beam Solar Transmittance,Slat Beam Solar Front Reflectance,Blind To "
                      "Glass Distance {m}");

            if (state.dataWindowManager->HasComplexWindows)
                print(state.files.eio,
                      "{}\n",
                      "! <WindowConstruction:Complex>,Construction Name,Index,#Layers,U-factor {W/m2-K},SHGC"
                      "NFRC Product Type,Assembly U-Factor {W/m2-K},Assembly SHGC,Assembly Visible Transmittance");

            if (state.dataWindowManager->HasEQLWindows)
                print(state.files.eio,
                      "{}\n",
                      "! <Construction:WindowEquivalentLayer>,Construction Name,Index,#Layers,U-factor {W/m2-K},SHGC, "
                      "Solar Transmittance at Normal Incidence");
            if (state.dataHeatBal->W5GlsMatEQL > 0)
                print(state.files.eio,
                      "{}\n",
                      "! <WindowMaterial:Glazing:EquivalentLayer>, Material Name, Optical Data Type, Spectral Data "
                      "Set Name, Front Side Beam-Beam Solar Transmittance, Back Side Beam-Beam Solar Transmittance, "
                      "Front Side Beam-Beam Solar Reflectance, Back Side Beam-Beam Solar Reflectance, Front Side "
                      "Beam-Diffuse Solar Transmittance, Back Side Beam-Diffuse Solar Transmittance, , Front Side "
                      "Beam-Diffuse Solar Reflectance, Back Side Beam-Diffuse Solar Reflectance, Diffuse-Diffuse "
                      "Solar Transmittance, Front Side Diffuse-Diffuse Solar Reflectance, Back Side Diffuse-Diffuse "
                      "Solar Reflectance, Infrared Transmittance, Front Side Infrared Emissivity, Back Side Infrared "
                      "Emissivity");
            if (state.dataHeatBal->TotShadesEQL > 0)
                print(state.files.eio,
                      "{}\n",
                      "! <WindowMaterial:Shade:EquivalentLayer>, Material Name, Front Side Beam-Beam Solar "
                      "Transmittance, Back Side Beam-Beam Solar Transmittance, Front Side Beam-Diffuse Solar "
                      "Transmittance, Back Side Beam-Diffuse Solar Transmittance, , Front Side Beam-Diffuse Solar "
                      "Reflectance, Back Side Beam-Diffuse Solar Reflectance, Infrared Transmittance, Front Side "
                      "Infrared Emissivity, Back Side Infrared Emissivity");

            if (state.dataHeatBal->TotDrapesEQL > 0)
                print(state.files.eio,
                      "{}\n",
                      "! <WindowMaterial:Drape:EquivalentLayer>, Material Name, Front Side Beam-Beam Solar "
                      "Transmittance, Back Side Beam-Beam Solar Transmittance, Front Side Beam-Diffuse Solar "
                      "Transmittance, Back Side Beam-Diffuse Solar Transmittance, , Front Side Beam-Diffuse Solar "
                      "Reflectance, Back Side Beam-Diffuse Solar Reflectance, Infrared Transmittance, Front Side "
                      "Infrared Emissivity, Back Side Infrared Emissivity, Width of Pleated Fabric, Length of Pleated "
                      "Fabric");

            if (state.dataHeatBal->TotBlindsEQL > 0)
                print(state.files.eio,
                      "{}\n",
                      "! <WindowMaterial:Blind:EquivalentLayer>, Material Name, Slat Orientation, Slat Width, Slat "
                      "Separation, Slat Crown, Slat Angle, Front Side Slate Beam-Diffuse Solar Transmittance, Back "
                      "Side Slate Beam-Diffuse Solar Transmittance, Front Side Slate Beam-Diffuse Solar Reflectance, "
                      "Back Side Slate Beam-Diffuse Solar Reflectance, Slat Diffuse-Diffuse Solar Transmittance, "
                      "Front Side Slat Diffuse-Diffuse Solar Reflectance, Back Side Slat Diffuse-Diffuse Solar "
                      "Reflectance, Infrared Transmittance, Front Side Infrared Emissivity, Back Side Infrared "
                      "Emissivity, Slat Angle Control");
            if (state.dataHeatBal->TotScreensEQL > 0)
                print(state.files.eio,
                      "{}\n",
                      "! <WindowMaterial:Screen:EquivalentLayer>, Material Name, Screen Beam-Beam Solar "
                      "Transmittance, Screen Beam-Diffuse Solar Transmittance, Screen Beam-Diffuse Solar Reflectance, "
                      "Screen Infrared Transmittance, Screen Infrared Emissivity, Screen Wire Spacing, Screen Wire "
                      "Diameter");
            if (state.dataHeatBal->W5GapMatEQL > 0)
                print(state.files.eio, "{}\n", "! <WindowMaterial:Gap:EquivalentLayer>, Material Name, GasType, Gap Thickness {m}, Gap Vent Type");

            for (ThisNum = 1; ThisNum <= state.dataHeatBal->TotConstructs; ++ThisNum) {

                if (state.dataConstruction->Construct(ThisNum).WindowTypeBSDF) {

                    i = ThisNum;
                    CalcComplexWindowThermal(state, 0, i, TempVar, TempVar, TempVar, TempVar, DataBSDFWindow::Condition::Winter);
                    CalcComplexWindowThermal(state, 0, i, TempVar, TempVar, TempVar, TempVar, DataBSDFWindow::Condition::Summer);

                    static constexpr std::string_view Format_800(" WindowConstruction:Complex,{},{},{},{:.3R},{:.3R}\n");
                    print(state.files.eio,
                          Format_800,
                          state.dataConstruction->Construct(ThisNum).Name,
                          ThisNum,
                          state.dataConstruction->Construct(ThisNum).TotSolidLayers,
                          state.dataHeatBal->NominalU(ThisNum),
                          state.dataConstruction->Construct(ThisNum).SummerSHGC);

                } else if (state.dataConstruction->Construct(ThisNum).TypeIsWindow) {
                    // Calculate for ASHRAE winter and summer conditions:
                    // (1) nominal center-of-glass conductance, including inside and outside air films,
                    // (2) solar heat gain coefficient (SHGC),
                    // (3) solar transmittance at normal incidence, and (4) visible transmittance at normal incidence.

                    if (state.dataConstruction->Construct(ThisNum).WindowTypeEQL) {
                        // for equivalent layer Window already calculated
                        // NominalU(ThisNum)=NominalConductanceWinter
                        // Save the SHGC for later use in tabular report IVRS
                        // Construct(ThisNum)%SummerSHGC = SHGCSummer
                        state.dataConstruction->Construct(ThisNum).VisTransNorm = 0.0; // TODO list

                        static constexpr std::string_view Format_799(" Construction:WindowEquivalentLayer,{},{},{},{:.3R},{:.3R},{:.3R}\n");
                        print(state.files.eio,
                              Format_799,
                              state.dataConstruction->Construct(ThisNum).Name,
                              ThisNum,
                              state.dataConstruction->Construct(ThisNum).TotSolidLayers,
                              state.dataHeatBal->NominalU(ThisNum),
                              state.dataConstruction->Construct(ThisNum).SummerSHGC,
                              state.dataConstruction->Construct(ThisNum).SolTransNorm);

                    } else {

                        CalcNominalWindowCond(state, ThisNum, 1, NominalConductanceWinter, SHGCWinter, TransSolNorm, TransVisNorm, errFlag);

                        if (errFlag == 1) {
                            ShowWarningError(state,
                                             "Window construction " + state.dataConstruction->Construct(ThisNum).Name +
                                                 " has an interior or exterior blind");
                            ShowContinueError(state, "but the corresponding construction without the blind cannot be found.");
                            ShowContinueError(state, "The ReportGlass entry for this construction will not be printed in eplusout.eio.");
                            continue;
                        }

                        // Skip constructions with between-glass shade/blind until method is worked out to determine
                        // nominal conductance and SHGC.

                        if (errFlag == 2) {
                            ShowWarningError(state,
                                             "Window construction " + state.dataConstruction->Construct(ThisNum).Name +
                                                 " has a between-glass shade or blind");
                            ShowContinueError(state, "The ReportGlass entry for this construction will not be printed in eplusout.eio.");
                            continue;
                        }

                        state.dataHeatBal->NominalU(ThisNum) = NominalConductanceWinter;
                        if (!state.dataConstruction->Construct(ThisNum).WindowTypeEQL) {
                            CalcNominalWindowCond(state, ThisNum, 2, NominalConductanceSummer, SHGCSummer, TransSolNorm, TransVisNorm, errFlag);
                        }
                        // Save the SHGC for later use in tabular report IVRS
                        state.dataConstruction->Construct(ThisNum).SummerSHGC = SHGCSummer;
                        state.dataConstruction->Construct(ThisNum).VisTransNorm = TransVisNorm;
                        state.dataConstruction->Construct(ThisNum).SolTransNorm = TransSolNorm;

                        static constexpr std::string_view Format_700(" WindowConstruction,{},{},{},{},{:.3R},{:.3R},{:.3R},{:.3R}\n");
                        print(state.files.eio,
                              Format_700,
                              state.dataConstruction->Construct(ThisNum).Name,
                              ThisNum,
                              state.dataConstruction->Construct(ThisNum).TotLayers,
                              Roughness(static_cast<int>(state.dataConstruction->Construct(ThisNum).OutsideRoughness)),
                              NominalConductanceWinter,
                              state.dataHeatBal->NominalUBeforeAdjusted(ThisNum),
                              state.dataHeatBal->CoeffAdjRatio(ThisNum),
                              SHGCSummer,
                              TransSolNorm,
                              TransVisNorm);
                    }
                    //    Write(OutputFileConstrainParams, 705)  TRIM(Construct(ThisNum)%Name), SHGCSummer ,TransVisNorm

                    for (i = 1; i <= state.dataConstruction->Construct(ThisNum).TotLayers; ++i) {
                        Layer = state.dataConstruction->Construct(ThisNum).LayerPoint(i);
                        switch (state.dataMaterial->Material(Layer).Group) {
                        case DataHeatBalance::MaterialGroup::WindowGas: {
                            static constexpr std::string_view Format_702(" WindowMaterial:Gas,{},{},{:.3R}\n");
                            print(state.files.eio,
                                  Format_702,
                                  state.dataMaterial->Material(Layer).Name,
                                  GasTypeName(state.dataMaterial->Material(Layer).GasType(1)),
                                  state.dataMaterial->Material(Layer).Thickness);
                            //! fw CASE(WindowGasMixture)
                        } break;
                        case DataHeatBalance::MaterialGroup::Shade: {
                            static constexpr std::string_view Format_703(" WindowMaterial:Shade,,{},{:.3R},{:.3R},{:.3R},{:.3R},{:.3R},{:.3R}\n");
                            print(state.files.eio,
                                  Format_703,
                                  state.dataMaterial->Material(Layer).Name,
                                  state.dataMaterial->Material(Layer).Thickness,
                                  state.dataMaterial->Material(Layer).Conductivity,
                                  state.dataMaterial->Material(Layer).AbsorpThermal,
                                  state.dataMaterial->Material(Layer).Trans,
                                  state.dataMaterial->Material(Layer).TransVis,
                                  state.dataMaterial->Material(Layer).ReflectShade);
                        } break;
                        case DataHeatBalance::MaterialGroup::WindowBlind: {
                            BlNum = state.dataMaterial->Material(Layer).BlindDataPtr;
                            static constexpr std::string_view Format_704(
                                " WindowMaterial:Blind,{},{:.4R},{:.4R},{:.4R},{:.3R},{:.3R},{:.3R},{:.3R}\n");
                            print(state.files.eio,
                                  Format_704,
                                  state.dataMaterial->Material(Layer).Name,
                                  state.dataHeatBal->Blind(BlNum).SlatWidth,
                                  state.dataHeatBal->Blind(BlNum).SlatSeparation,
                                  state.dataHeatBal->Blind(BlNum).SlatThickness,
                                  state.dataHeatBal->Blind(BlNum).SlatAngle,
                                  state.dataHeatBal->Blind(BlNum).SlatTransSolBeamDiff,
                                  state.dataHeatBal->Blind(BlNum).SlatFrontReflSolBeamDiff,
                                  state.dataHeatBal->Blind(BlNum).BlindToGlassDist);
                        } break;
                        case DataHeatBalance::MaterialGroup::Screen: {
                            if (state.dataMaterial->Material(Layer).ScreenDataPtr > 0) {
                                static constexpr std::string_view Format_706(
                                    " WindowMaterial:Screen,{},{:.5R},{:.3R},{:.3R},{:.3R},{:.3R},{:.3R},{:.3R},{:.3R},{:.3R},{:.3R}\n");
                                print(
                                    state.files.eio,
                                    Format_706,
                                    state.dataMaterial->Material(Layer).Name,
                                    state.dataMaterial->Material(Layer).Thickness,
                                    state.dataMaterial->Material(Layer).Conductivity,
                                    state.dataMaterial->Material(Layer).AbsorpThermal,
                                    state.dataHeatBal->SurfaceScreens(state.dataMaterial->Material(Layer).ScreenDataPtr).BmBmTrans,
                                    state.dataHeatBal->SurfaceScreens(state.dataMaterial->Material(Layer).ScreenDataPtr).ReflectSolBeamFront,
                                    state.dataHeatBal->SurfaceScreens(state.dataMaterial->Material(Layer).ScreenDataPtr).ReflectVisBeamFront,
                                    state.dataHeatBal->SurfaceScreens(state.dataMaterial->Material(Layer).ScreenDataPtr).DifReflect,
                                    state.dataHeatBal->SurfaceScreens(state.dataMaterial->Material(Layer).ScreenDataPtr).DifReflectVis,
                                    state.dataHeatBal->SurfaceScreens(state.dataMaterial->Material(Layer).ScreenDataPtr).ScreenDiameterToSpacingRatio,
                                    state.dataMaterial->Material(Layer).WinShadeToGlassDist);
                            }
                        } break;
                        case DataHeatBalance::MaterialGroup::WindowGlass:
                        case DataHeatBalance::MaterialGroup::WindowSimpleGlazing: {
                            SolarDiffusing = "No";
                            if (state.dataMaterial->Material(Layer).SolarDiffusing) SolarDiffusing = "Yes";
                            OpticalDataType = "SpectralAverage";
                            SpectralDataName = "";
                            if (state.dataMaterial->Material(Layer).GlassSpectralDataPtr > 0) {
                                OpticalDataType = "Spectral";
                                SpectralDataName = state.dataHeatBal->SpectralData(state.dataMaterial->Material(Layer).GlassSpectralDataPtr).Name;
                            }
                            if (state.dataMaterial->Material(Layer).GlassSpectralAndAngle) {
                                OpticalDataType = "SpectralAndAngle";
                                SpectralDataName =
                                    state.dataCurveManager->PerfCurve(state.dataMaterial->Material(Layer).GlassSpecAngTransDataPtr).Name + ", " +
                                    state.dataCurveManager->PerfCurve(state.dataMaterial->Material(Layer).GlassSpecAngFRefleDataPtr).Name + ", " +
                                    state.dataCurveManager->PerfCurve(state.dataMaterial->Material(Layer).GlassSpecAngBRefleDataPtr).Name;
                            }
                            static constexpr std::string_view Format_707(
                                " WindowMaterial:Glazing,{},{},{},{:.5R},{:.5R},{:.5R},{:.5R},{:.5R},{:.5R},{:.5R},{"
                                ":.5R},{:.5R},{:.5R},{:.5R},{:.5R},{}\n");
                            print(state.files.eio,
                                  Format_707,
                                  state.dataMaterial->Material(Layer).Name,
                                  OpticalDataType,
                                  SpectralDataName,
                                  state.dataMaterial->Material(Layer).Thickness,
                                  state.dataMaterial->Material(Layer).Trans,
                                  state.dataMaterial->Material(Layer).ReflectSolBeamFront,
                                  state.dataMaterial->Material(Layer).ReflectSolBeamBack,
                                  state.dataMaterial->Material(Layer).TransVis,
                                  state.dataMaterial->Material(Layer).ReflectVisBeamFront,
                                  state.dataMaterial->Material(Layer).ReflectVisBeamBack,
                                  state.dataMaterial->Material(Layer).TransThermal,
                                  state.dataMaterial->Material(Layer).AbsorpThermalFront,
                                  state.dataMaterial->Material(Layer).AbsorpThermalBack,
                                  state.dataMaterial->Material(Layer).Conductivity,
                                  state.dataMaterial->Material(Layer).GlassTransDirtFactor,
                                  SolarDiffusing);
                        } break;
                        case DataHeatBalance::MaterialGroup::GlassEquivalentLayer: {
                            OpticalDataType = "SpectralAverage";
                            SpectralDataName = "";
                            static constexpr std::string_view Format_708(
                                " WindowMaterial:Glazing:EquivalentLayer,{},{},{},{:.5R},{:.5R},{:.5R},{:.5R},{:.5R}"
                                ",{:.5R},{:.5R},{:.5R},{:.5R},{:.5R},{:.5R},{:.5R},{:.5R},{:.5R}\n");
                            print(state.files.eio,
                                  Format_708,
                                  state.dataMaterial->Material(Layer).Name,
                                  OpticalDataType,
                                  SpectralDataName,
                                  state.dataMaterial->Material(Layer).TausFrontBeamBeam,
                                  state.dataMaterial->Material(Layer).TausBackBeamBeam,
                                  state.dataMaterial->Material(Layer).ReflFrontBeamBeam,
                                  state.dataMaterial->Material(Layer).ReflBackBeamBeam,
                                  state.dataMaterial->Material(Layer).TausFrontBeamDiff,
                                  state.dataMaterial->Material(Layer).TausBackBeamDiff,
                                  state.dataMaterial->Material(Layer).ReflFrontBeamDiff,
                                  state.dataMaterial->Material(Layer).ReflBackBeamDiff,
                                  state.dataMaterial->Material(Layer).TausDiffDiff,
                                  state.dataMaterial->Material(Layer).ReflFrontDiffDiff,
                                  state.dataMaterial->Material(Layer).ReflBackDiffDiff,
                                  state.dataMaterial->Material(Layer).TausThermal,
                                  state.dataMaterial->Material(Layer).EmissThermalFront,
                                  state.dataMaterial->Material(Layer).EmissThermalBack);
                        } break;
                        case DataHeatBalance::MaterialGroup::ShadeEquivalentLayer: {
                            static constexpr std::string_view Format_709(
                                " WindowMaterial:Shade:EquivalentLayer,{},{:.4R},{:.4R},{:.4R},{:.4R},{:.4R},{:.4R},{:.4R},{:.4R},{:.4R}\n");
                            print(state.files.eio,
                                  Format_709,
                                  state.dataMaterial->Material(Layer).Name,
                                  state.dataMaterial->Material(Layer).TausFrontBeamBeam,
                                  state.dataMaterial->Material(Layer).TausBackBeamBeam,
                                  state.dataMaterial->Material(Layer).TausFrontBeamDiff,
                                  state.dataMaterial->Material(Layer).TausBackBeamDiff,
                                  state.dataMaterial->Material(Layer).ReflFrontBeamDiff,
                                  state.dataMaterial->Material(Layer).ReflBackBeamDiff,
                                  state.dataMaterial->Material(Layer).TausThermal,
                                  state.dataMaterial->Material(Layer).EmissThermalFront,
                                  state.dataMaterial->Material(Layer).EmissThermalBack);
                        } break;
                        case DataHeatBalance::MaterialGroup::DrapeEquivalentLayer: {
                            static constexpr std::string_view Format_710(
                                " WindowMaterial:Drape:EquivalentLayer,{},{:.4R},{:.4R},{:.4R},{:.4R},{:.4R},{:.4R},"
                                "{:.4R},{:.4R},{:.5R},{:.5R}\n");
                            print(state.files.eio,
                                  Format_710,
                                  state.dataMaterial->Material(Layer).Name,
                                  state.dataMaterial->Material(Layer).TausFrontBeamBeam,
                                  state.dataMaterial->Material(Layer).TausFrontBeamDiff,
                                  state.dataMaterial->Material(Layer).TausBackBeamDiff,
                                  state.dataMaterial->Material(Layer).ReflFrontBeamDiff,
                                  state.dataMaterial->Material(Layer).ReflBackBeamDiff,
                                  state.dataMaterial->Material(Layer).TausThermal,
                                  state.dataMaterial->Material(Layer).EmissThermalFront,
                                  state.dataMaterial->Material(Layer).EmissThermalBack,
                                  state.dataMaterial->Material(Layer).PleatedDrapeWidth,
                                  state.dataMaterial->Material(Layer).PleatedDrapeLength);
                        } break;
                        case DataHeatBalance::MaterialGroup::ScreenEquivalentLayer: {
                            static constexpr std::string_view Format_711(
                                " WindowMaterial:Screen:EquivalentLayer,{},{:.4R},{:.4R},{:.4R},{:.4R},{:.4R},{:.4R}"
                                ",{:.4R},{:.4R},{:.5R},{:.5R}\n");
                            print(state.files.eio,
                                  Format_711,
                                  state.dataMaterial->Material(Layer).Name,
                                  state.dataMaterial->Material(Layer).TausFrontBeamBeam,
                                  state.dataMaterial->Material(Layer).TausFrontBeamDiff,
                                  state.dataMaterial->Material(Layer).TausBackBeamDiff,
                                  state.dataMaterial->Material(Layer).ReflFrontBeamDiff,
                                  state.dataMaterial->Material(Layer).ReflBackBeamDiff,
                                  state.dataMaterial->Material(Layer).TausThermal,
                                  state.dataMaterial->Material(Layer).EmissThermalFront,
                                  state.dataMaterial->Material(Layer).EmissThermalBack,
                                  state.dataMaterial->Material(Layer).ScreenWireSpacing,
                                  state.dataMaterial->Material(Layer).ScreenWireDiameter);
                        } break;
                        case DataHeatBalance::MaterialGroup::BlindEquivalentLayer: {
                            SlateOrientation = "Horizontal";
                            if (state.dataMaterial->Material(Layer).SlatOrientation == DataWindowEquivalentLayer::Orientation::Vertical) {
                                SlateOrientation = "Vertical";
                            }
                            // Formats
                            static constexpr std::string_view Format_712(
                                " WindowMaterial:Blind:EquivalentLayer,{},{},{:.5R},{:.5R},{:.5R},{:.5R},{:.5R},{:."
                                "5R},{:.5R},{:.5R},{:.5R},{:.5R},{:.5R},{:.5R},{:.5R},{:.5R}");
                            print(state.files.eio,
                                  Format_712,
                                  state.dataMaterial->Material(Layer).Name,
                                  SlateOrientation,
                                  state.dataMaterial->Material(Layer).SlatWidth,
                                  state.dataMaterial->Material(Layer).SlatSeparation,
                                  state.dataMaterial->Material(Layer).SlatCrown,
                                  state.dataMaterial->Material(Layer).SlatAngle,
                                  state.dataMaterial->Material(Layer).TausFrontBeamDiff,
                                  state.dataMaterial->Material(Layer).TausBackBeamDiff,
                                  state.dataMaterial->Material(Layer).ReflFrontBeamDiff,
                                  state.dataMaterial->Material(Layer).ReflBackBeamDiff,
                                  state.dataMaterial->Material(Layer).TausDiffDiff,
                                  state.dataMaterial->Material(Layer).ReflFrontDiffDiff,
                                  state.dataMaterial->Material(Layer).ReflBackDiffDiff,
                                  state.dataMaterial->Material(Layer).TausThermal,
                                  state.dataMaterial->Material(Layer).EmissThermalFront,
                                  state.dataMaterial->Material(Layer).EmissThermalBack);
                        } break;
                        case DataHeatBalance::MaterialGroup::GapEquivalentLayer: {
                            GapVentType = "Sealed";
                            if (state.dataMaterial->Material(Layer).GapVentType == 2) {
                                GapVentType = "VentedIndoor";
                            } else if (state.dataMaterial->Material(Layer).GapVentType == 3) {
                                GapVentType = "VentedOutdoor";
                            }
                            static constexpr std::string_view Format_713(" WindowMaterial:Gap:EquivalentLayer,{},{},{:.3R},{}\n");
                            print(state.files.eio,
                                  Format_713,
                                  state.dataMaterial->Material(Layer).Name,
                                  GasTypeName(state.dataMaterial->Material(Layer).GasType(1)),
                                  state.dataMaterial->Material(Layer).Thickness,
                                  GapVentType);
                        } break;
                        default:
                            break;
                        }
                    }
                }
            }

        } else if (state.dataWindowManager->HasWindows) {

            for (ThisNum = 1; ThisNum <= state.dataHeatBal->TotConstructs; ++ThisNum) {

                if (!state.dataConstruction->Construct(ThisNum).TypeIsWindow) continue;
                if (state.dataConstruction->Construct(ThisNum).WindowTypeEQL) continue; // skip if equivalent layer window

                // Calculate for ASHRAE winter and summer conditions: (1)nominal center-of-glass conductance,
                // (2) solar heat gain coefficient (SHGC), including inside and outside air films,
                // (3) solar transmittance at normal incidence, and (4) visible transmittance at normal incidence.

                CalcNominalWindowCond(state, ThisNum, 1, NominalConductanceWinter, SHGCWinter, TransSolNorm, TransVisNorm, errFlag);
                if (errFlag == 1 || errFlag == 2) continue;
                state.dataHeatBal->NominalU(ThisNum) = NominalConductanceWinter;
                // Need to have this because of window assembly reports (Simon)
                state.dataConstruction->Construct(ThisNum).SummerSHGC = SHGCSummer;
                state.dataConstruction->Construct(ThisNum).VisTransNorm = TransVisNorm;
            }
        }
    }

    //*************************************************************************************

    void CalcWindowBlindProperties(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Hans Simmler
        //       DATE WRITTEN   July-Aug 1995
        //       MODIFIED       Aug 2001 (FCW): adapt to EnergyPlus
        //                      Dec 2001 (FCW): add variable slat angle
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates solar-optical properties of a window blind
        // from slat properties and solar profile angle. Assumes flat slats.

        // METHODOLOGY EMPLOYED:
        // The solar profile angle is varied from -90 to +90 deg and slat angle is varied from 0 to 180deg,
        // covering the full range of possible profile angles and slat angles.
        // (The profile angle is defined as the angle of incidence when the radiation
        // source is located in a plane that (1)is perpendicular to the  plane of the blinds [which is
        // the same as the window plane] and (2) contains the slat normal vector.)

        // In the time-step calculation,the blind properties vs. profile angle and slat angle
        // that are calculated here will be applicable to windows and slats
        // of arbitrary orientation, and to arbitrary sun positions, as long as the appropiate
        // profile angle is used. The slat angle for a particular window with blinds is determined
        // each time step in subroutine WindowShadingManager on the basis of user-specified
        // slat control options.

        // REFERENCES:
        // "Solar-Thermal Window Blind Model for DOE-2," H. Simmler, U. Fischer and
        // F. Winkelmann, Lawrence Berkeley National Laboratory, Jan. 1996.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        Array1D<Real64> bld_pr(15);     // Slat properties
        Array1D<Real64> st_lay(16);     // Solar-optical blind/glazing system properties
        Real64 sun_el;                  // Solar profile angle (radians)
        Array1D<Real64> sun_el_deg(37); // Solar profile angle (deg) corresponding to sun_el values
        Real64 bld_el;                  // Slat angle (elevation of slat normal vector in plane
        //  perpendicular to window and containing the slat normal vector) (radians)
        int ISolVis;  // 1 = do solar and IR calculation; 2 = do visible calculation
        int IProfAng; // Profile angle index
        int BlindNum; // Blind number
        int ISlatAng; // Slat angle index

        for (BlindNum = 1; BlindNum <= state.dataHeatBal->TotBlinds; ++BlindNum) {

            bld_pr(2) = state.dataHeatBal->Blind(BlindNum).SlatWidth;
            bld_pr(3) = state.dataHeatBal->Blind(BlindNum).SlatSeparation;

            for (ISolVis = 1; ISolVis <= 2; ++ISolVis) {
                if (ISolVis == 1) { // For solar calculation
                    bld_pr(4) = 0.0;
                    bld_pr(5) = 0.0;
                    bld_pr(6) = 0.0;
                    bld_pr(7) = state.dataHeatBal->Blind(BlindNum).SlatTransSolBeamDiff;
                    bld_pr(8) = state.dataHeatBal->Blind(BlindNum).SlatFrontReflSolBeamDiff;
                    bld_pr(9) = state.dataHeatBal->Blind(BlindNum).SlatBackReflSolBeamDiff;
                    bld_pr(10) = state.dataHeatBal->Blind(BlindNum).SlatTransSolDiffDiff;
                    bld_pr(11) = state.dataHeatBal->Blind(BlindNum).SlatFrontReflSolDiffDiff;
                    bld_pr(12) = state.dataHeatBal->Blind(BlindNum).SlatBackReflSolDiffDiff;
                } else { // For visible calculation
                    bld_pr(4) = 0.0;
                    bld_pr(5) = 0.0;
                    bld_pr(6) = 0.0;
                    bld_pr(7) = state.dataHeatBal->Blind(BlindNum).SlatTransVisBeamDiff;
                    bld_pr(8) = state.dataHeatBal->Blind(BlindNum).SlatFrontReflVisBeamDiff;
                    bld_pr(9) = state.dataHeatBal->Blind(BlindNum).SlatBackReflVisBeamDiff;
                    bld_pr(10) = state.dataHeatBal->Blind(BlindNum).SlatTransVisDiffDiff;
                    bld_pr(11) = state.dataHeatBal->Blind(BlindNum).SlatFrontReflVisDiffDiff;
                    bld_pr(12) = state.dataHeatBal->Blind(BlindNum).SlatBackReflVisDiffDiff;
                }
                // For IR calculation
                bld_pr(13) = state.dataHeatBal->Blind(BlindNum).SlatTransIR;
                bld_pr(14) = state.dataHeatBal->Blind(BlindNum).SlatFrontEmissIR;
                bld_pr(15) = state.dataHeatBal->Blind(BlindNum).SlatBackEmissIR;

                // Calculate diffuse properties of blind. If blind has variable slat angle, &
                // vary slat angle from 0 to 180 deg in 10-deg steps (for MaxSlatAngs = 19).
                // If blind has fixed slat angle, calculate properties at that angle only.

                for (ISlatAng = 1; ISlatAng <= MaxSlatAngs; ++ISlatAng) {

                    st_lay = 0.0;
                    if (state.dataHeatBal->Blind(BlindNum).SlatAngleType == DataWindowEquivalentLayer::AngleType::Fixed) {
                        bld_el = state.dataHeatBal->Blind(BlindNum).SlatAngle * DataGlobalConstants::DegToRadians;
                    } else {                                                                     // Variable slat angle
                        bld_el = (DataGlobalConstants::Pi / (MaxSlatAngs - 1)) * (ISlatAng - 1); // 0 <= bld_el <= 180 deg
                    }
                    BlindOpticsDiffuse(state, BlindNum, ISolVis, bld_pr, bld_el, st_lay);

                    if (ISolVis == 1) { // Fill blind diffuse solar and IR properties
                        state.dataHeatBal->Blind(BlindNum).SolFrontDiffDiffTrans(ISlatAng) = st_lay(9);
                        state.dataHeatBal->Blind(BlindNum).SolFrontDiffDiffRefl(ISlatAng) = st_lay(10);
                        state.dataHeatBal->Blind(BlindNum).SolBackDiffDiffTrans(ISlatAng) = st_lay(11);
                        state.dataHeatBal->Blind(BlindNum).SolBackDiffDiffRefl(ISlatAng) = st_lay(12);
                        state.dataHeatBal->Blind(BlindNum).SolFrontDiffAbs(ISlatAng) = max(0.0, 1.0 - st_lay(9) - st_lay(10));
                        state.dataHeatBal->Blind(BlindNum).SolBackDiffAbs(ISlatAng) = max(0.0, 1.0 - st_lay(11) - st_lay(12));
                        state.dataHeatBal->Blind(BlindNum).IRFrontTrans(ISlatAng) = st_lay(13);
                        state.dataHeatBal->Blind(BlindNum).IRFrontEmiss(ISlatAng) = st_lay(14);
                        state.dataHeatBal->Blind(BlindNum).IRBackTrans(ISlatAng) = st_lay(13);
                        state.dataHeatBal->Blind(BlindNum).IRBackEmiss(ISlatAng) = st_lay(15);
                    } else { // Fill blind diffuse visible properties
                        state.dataHeatBal->Blind(BlindNum).VisFrontDiffDiffTrans(ISlatAng) = st_lay(9);
                        state.dataHeatBal->Blind(BlindNum).VisFrontDiffDiffRefl(ISlatAng) = st_lay(10);
                        state.dataHeatBal->Blind(BlindNum).VisBackDiffDiffTrans(ISlatAng) = st_lay(11);
                        state.dataHeatBal->Blind(BlindNum).VisBackDiffDiffRefl(ISlatAng) = st_lay(12);
                    }

                    if (state.dataHeatBal->Blind(BlindNum).SlatAngleType == DataWindowEquivalentLayer::AngleType::Fixed) break;
                } // End of slat angle loop

                // Calculate beam properties of blind. Vary profile angle from -90 to +90 deg in 5-deg steps.
                // If blind has variable slat angle, vary slat angle from 0 to 180 deg in 10-deg steps
                // (for MaxSlatAngs = 19). If blind has fixed slat angle, calculate properties at that angle only.

                for (IProfAng = 1; IProfAng <= 37; ++IProfAng) {
                    sun_el = -DataGlobalConstants::Pi / 2.0 + (DataGlobalConstants::Pi / 36.0) * (IProfAng - 1);
                    sun_el_deg(IProfAng) = 57.2958 * sun_el;

                    for (ISlatAng = 1; ISlatAng <= MaxSlatAngs; ++ISlatAng) {
                        st_lay = 0.0;
                        if (state.dataHeatBal->Blind(BlindNum).SlatAngleType == DataWindowEquivalentLayer::AngleType::Fixed) {
                            bld_el = state.dataHeatBal->Blind(BlindNum).SlatAngle * DataGlobalConstants::DegToRadians;
                        } else {                                                                     // Variable slat angle
                            bld_el = (DataGlobalConstants::Pi / (MaxSlatAngs - 1)) * (ISlatAng - 1); // 0 <= bld_el <= 180 deg
                        }

                        // Beam solar-optical properties of blind for given profile angle and slat angle

                        BlindOpticsBeam(state, BlindNum, bld_pr, bld_el, sun_el, st_lay);

                        if (ISolVis == 1) { // Fill blind beam solar properties
                            state.dataHeatBal->Blind(BlindNum).SolFrontBeamBeamTrans(ISlatAng, IProfAng) = st_lay(1);
                            state.dataHeatBal->Blind(BlindNum).SolFrontBeamBeamRefl(ISlatAng, IProfAng) = st_lay(2);
                            state.dataHeatBal->Blind(BlindNum).SolBackBeamBeamTrans(ISlatAng, IProfAng) = st_lay(3);
                            state.dataHeatBal->Blind(BlindNum).SolBackBeamBeamRefl(ISlatAng, IProfAng) = st_lay(4);
                            state.dataHeatBal->Blind(BlindNum).SolFrontBeamDiffTrans(ISlatAng, IProfAng) = st_lay(5);
                            state.dataHeatBal->Blind(BlindNum).SolFrontBeamDiffRefl(ISlatAng, IProfAng) = st_lay(6);
                            state.dataHeatBal->Blind(BlindNum).SolBackBeamDiffTrans(ISlatAng, IProfAng) = st_lay(7);
                            state.dataHeatBal->Blind(BlindNum).SolBackBeamDiffRefl(ISlatAng, IProfAng) = st_lay(8);
                            state.dataHeatBal->Blind(BlindNum).SolFrontBeamAbs(ISlatAng, IProfAng) =
                                max(0.0, 1.0 - st_lay(6) - st_lay(1) - st_lay(5));
                            state.dataHeatBal->Blind(BlindNum).SolBackBeamAbs(ISlatAng, IProfAng) = max(0.0, 1.0 - st_lay(7) - st_lay(3) - st_lay(8));

                        } else { // Fill blind beam visible properties
                            state.dataHeatBal->Blind(BlindNum).VisFrontBeamBeamTrans(ISlatAng, IProfAng) = st_lay(1);
                            state.dataHeatBal->Blind(BlindNum).VisFrontBeamBeamRefl(ISlatAng, IProfAng) = st_lay(2);
                            state.dataHeatBal->Blind(BlindNum).VisBackBeamBeamTrans(ISlatAng, IProfAng) = st_lay(3);
                            state.dataHeatBal->Blind(BlindNum).VisBackBeamBeamRefl(ISlatAng, IProfAng) = st_lay(4);
                            state.dataHeatBal->Blind(BlindNum).VisFrontBeamDiffTrans(ISlatAng, IProfAng) = st_lay(5);
                            state.dataHeatBal->Blind(BlindNum).VisFrontBeamDiffRefl(ISlatAng, IProfAng) = st_lay(6);
                            state.dataHeatBal->Blind(BlindNum).VisBackBeamDiffTrans(ISlatAng, IProfAng) = st_lay(7);
                            state.dataHeatBal->Blind(BlindNum).VisBackBeamDiffRefl(ISlatAng, IProfAng) = st_lay(8);
                        }

                        if (state.dataHeatBal->Blind(BlindNum).SlatAngleType == DataWindowEquivalentLayer::AngleType::Fixed) break;
                    } // End of loop over slat angles
                }     // End of loop over profile angles

                if (ISolVis == 1) {
                    for (ISlatAng = 1; ISlatAng <= MaxSlatAngs; ++ISlatAng) {
                        state.dataHeatBal->Blind(BlindNum).SolFrontDiffDiffTransGnd(ISlatAng) =
                            DiffuseAverageProfAngGnd(state.dataHeatBal->Blind(BlindNum).SolFrontBeamBeamTrans(ISlatAng, {1, 37})) +
                            DiffuseAverageProfAngGnd(state.dataHeatBal->Blind(BlindNum).SolFrontBeamDiffTrans(ISlatAng, {1, 37}));
                        state.dataHeatBal->Blind(BlindNum).SolFrontDiffDiffTransSky(ISlatAng) =
                            DiffuseAverageProfAngSky(state.dataHeatBal->Blind(BlindNum).SolFrontBeamBeamTrans(ISlatAng, {1, 37})) +
                            DiffuseAverageProfAngSky(state.dataHeatBal->Blind(BlindNum).SolFrontBeamDiffTrans(ISlatAng, {1, 37}));
                        state.dataHeatBal->Blind(BlindNum).SolFrontDiffAbsGnd(ISlatAng) =
                            DiffuseAverageProfAngGnd(state.dataHeatBal->Blind(BlindNum).SolFrontBeamAbs(ISlatAng, {1, 37}));
                        state.dataHeatBal->Blind(BlindNum).SolFrontDiffAbsSky(ISlatAng) =
                            DiffuseAverageProfAngSky(state.dataHeatBal->Blind(BlindNum).SolFrontBeamAbs(ISlatAng, {1, 37}));
                        state.dataHeatBal->Blind(BlindNum).SolFrontDiffDiffReflGnd(ISlatAng) =
                            DiffuseAverageProfAngGnd(state.dataHeatBal->Blind(BlindNum).SolFrontBeamDiffRefl(ISlatAng, {1, 37}));
                        state.dataHeatBal->Blind(BlindNum).SolFrontDiffDiffReflSky(ISlatAng) =
                            DiffuseAverageProfAngSky(state.dataHeatBal->Blind(BlindNum).SolFrontBeamDiffRefl(ISlatAng, {1, 37}));

                        // TH 2/17/2010. Added. Loop only for movable slat blinds
                        if (state.dataHeatBal->Blind(BlindNum).SlatAngleType == DataWindowEquivalentLayer::AngleType::Fixed) break;
                    }
                }

            } // End of loop over solar vs. visible properties

        } // End of loop over blinds
    }

    //*************************************************************************************

    void CalcWindowScreenProperties(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   April 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Initialize static properties of window screens.

        // METHODOLOGY EMPLOYED:
        // Loop through all surfaces to determine which window has an exterior screen. Static
        // variables are defined here, dynamic variables are calculated in CalcScreenTransmittance.

        // Locals
        // SUBROUTINE PARAMETER DEFINITIONS:
        int constexpr M(18);
        int constexpr N(18);

        int SurfNum;     // Index to surface number
        int ScreenNum;   // Index to each screen used on exterior of window
        int ConstrNumSh; // Index to shaded construction
        int MatNum;      // Index to material number
        int i;           // Integration loop counters
        int j;
        Real64 SumTrans;      // Integration variable for transmittance
        Real64 SumTransVis;   // Integration variable for visible transmittance
        Real64 SumReflect;    // Integration variable for reflectance
        Real64 SumReflectVis; // Integration variable for visible reflectance
        Real64 SumArea;       // Integration variable for area of quarter hemisphere
        bool FoundMaterial;   // Flag to avoid printing screen transmittance data multiple times when Material:WindowScreen
        // is used on multiple surfaces
        bool PrintTransMap; // Flag used to print transmittance map

        state.dataHeatBal->SurfaceScreens.allocate(state.dataHeatBal->NumSurfaceScreens);
        state.dataHeatBal->ScreenTrans.allocate(state.dataHeatBal->NumSurfaceScreens);
        ScreenNum = 0;

        // Pre-calculate these constants
        std::vector<Real64> sunAzimuth;
        std::vector<Real64> sin_sunAzimuth;
        std::vector<Real64> cos_sunAzimuth;
        std::vector<Real64> sunAltitude;
        std::vector<Real64> sin_sunAltitude;
        std::vector<Real64> cos_sunAltitude;
        std::vector<Real64> skyArea;      // Area of integration
        Array2D<Real64> relativeAzimuth;  // Relative azimuth angle of sun with respect to surface outward normal
        Array2D<Real64> relativeAltitude; // Relative altitude angle of sun with respect to surface outward normal

        relativeAzimuth.allocate(N, M);
        relativeAltitude.allocate(N, M);

        for (j = 0; j <= N - 1; ++j) {
            Real64 currAzimuth = (90.0 / N) * j * DataGlobalConstants::DegToRadians;
            sunAzimuth.push_back(currAzimuth); // Azimuth angle of sun during integration
            sin_sunAzimuth.push_back(std::sin(currAzimuth));
            cos_sunAzimuth.push_back(std::cos(currAzimuth));
        }

        for (i = 0; i <= M - 1; ++i) {
            Real64 currAltitude = (90.0 / M) * i * DataGlobalConstants::DegToRadians;
            sunAltitude.push_back(currAltitude); // Altitude angle of sun during integration
            sin_sunAltitude.push_back(std::sin(currAltitude));
            cos_sunAltitude.push_back(std::cos(currAltitude));
            skyArea.push_back(sin_sunAltitude[i] * cos_sunAltitude[i]);
        }

        for (j = 1; j <= N; ++j) {
            for (i = 1; i <= M; ++i) {
                // Integrate transmittance using coordinate transform
                relativeAzimuth(i, j) = std::asin(sin_sunAltitude[i - 1] * cos_sunAzimuth[j - 1]);        // phi prime
                relativeAltitude(i, j) = std::atan(std::tan(sunAltitude[i - 1]) * sin_sunAzimuth[j - 1]); // alpha
            }
        }

        PrintTransMap = false;
        for (SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {

            if (state.dataSurface->Surface(SurfNum).HasShadeControl) {
                ConstrNumSh = state.dataSurface->Surface(SurfNum).activeShadedConstruction;
                MatNum = state.dataConstruction->Construct(ConstrNumSh).LayerPoint(1);
                WinShadingType ShadingType =
                    state.dataSurface->WindowShadingControl(state.dataSurface->Surface(SurfNum).activeWindowShadingControl).ShadingType;
                if (ShadingType == WinShadingType::ExtScreen) {

                    if (state.dataMaterial->Material(MatNum).ScreenMapResolution > 0) PrintTransMap = true;
                    ++ScreenNum;
                    state.dataSurface->SurfWinScreenNumber(SurfNum) = ScreenNum;
                    //     If a screen material is used more than once, the Material structure's screen data pointer holds the screen number
                    //     of the last window surface. Use this method to access the screen parameter's only for static variables such as
                    //     diffuse properties (InitGlassOpticalCalculations). For all cases where the screen properties are a function of
                    //     sun azimuth and altitude angles, use the SurfaceScreens structure.
                    state.dataMaterial->Material(MatNum).ScreenDataPtr = ScreenNum;
                    state.dataHeatBal->SurfaceScreens(ScreenNum).MaterialNumber = MatNum;
                    //     Invert calculation done in GetMaterialInput to find Diameter to Spacing ratio (Props(7)/Props(6))
                    //     dataMaterial.Material(MaterNum)%Trans = (1 - MaterialProps(7)/MaterialProps(6))**2.0
                    state.dataHeatBal->SurfaceScreens(ScreenNum).ScreenDiameterToSpacingRatio =
                        1.0 - std::sqrt(state.dataMaterial->Material(MatNum).Trans);

                    if (UtilityRoutines::SameString(state.dataMaterial->Material(MatNum).ReflectanceModeling, "DoNotModel")) {
                        state.dataHeatBal->SurfaceScreens(ScreenNum).ScreenBeamReflectanceAccounting = DoNotModel;
                    } else if (UtilityRoutines::SameString(state.dataMaterial->Material(MatNum).ReflectanceModeling, "ModelAsDirectBeam")) {
                        state.dataHeatBal->SurfaceScreens(ScreenNum).ScreenBeamReflectanceAccounting = ModelAsDirectBeam;
                    } else if (UtilityRoutines::SameString(state.dataMaterial->Material(MatNum).ReflectanceModeling, "ModelAsDiffuse")) {
                        state.dataHeatBal->SurfaceScreens(ScreenNum).ScreenBeamReflectanceAccounting = ModelAsDiffuse;
                    }

                    // Reflectance of screen material only
                    state.dataHeatBal->SurfaceScreens(ScreenNum).ReflectCylinder =
                        state.dataMaterial->Material(MatNum).ReflectShade / (1 - state.dataMaterial->Material(MatNum).Trans);
                    state.dataHeatBal->SurfaceScreens(ScreenNum).ReflectCylinderVis =
                        state.dataMaterial->Material(MatNum).ReflectShadeVis / (1 - state.dataMaterial->Material(MatNum).Trans);

                    //     Integrate the transmittance over a quarter hemisphere for use in diffuse calculations
                    SumTrans = 0.0;
                    SumTransVis = 0.0;
                    SumReflect = 0.0;
                    SumReflectVis = 0.0;
                    SumArea = 0.0;
                    //     Integration over quarter hemisphere in polar coordinates and converting to rectangular to call screen model.
                    //     Proceed in reverse order such that the last calculation yields zero sun angle to window screen normal (angles=0,0).
                    //     The properties calculated at zero sun angle are then used elsewhere prior to the start of the actual simulation.

                    for (j = N; j >= 1; --j) {
                        for (i = M; i >= 1; --i) {
                            // Integrate transmittance using coordinate transform
                            CalcScreenTransmittance(state, 0, relativeAltitude(i, j), relativeAzimuth(i, j), ScreenNum);
                            SumTrans +=
                                (state.dataHeatBal->SurfaceScreens(ScreenNum).BmBmTrans + state.dataHeatBal->SurfaceScreens(ScreenNum).BmDifTrans) *
                                skyArea[i - 1];
                            SumTransVis += (state.dataHeatBal->SurfaceScreens(ScreenNum).BmBmTransVis +
                                            state.dataHeatBal->SurfaceScreens(ScreenNum).BmDifTransVis) *
                                           skyArea[i - 1];
                            SumReflect += state.dataHeatBal->SurfaceScreens(ScreenNum).ReflectSolBeamFront * skyArea[i - 1];
                            SumReflectVis += state.dataHeatBal->SurfaceScreens(ScreenNum).ReflectVisBeamFront * skyArea[i - 1];
                            SumArea += skyArea[i - 1];
                        }
                    }

                    // Reflectance of overall screen including openings and scattered transmittance
                    state.dataHeatBal->SurfaceScreens(ScreenNum).ReflectScreen =
                        state.dataHeatBal->SurfaceScreens(ScreenNum).ReflectCylinder *
                        (1.0 - (state.dataHeatBal->SurfaceScreens(ScreenNum).BmBmTrans + state.dataHeatBal->SurfaceScreens(ScreenNum).BmDifTrans));
                    state.dataHeatBal->SurfaceScreens(ScreenNum).ReflectScreenVis =
                        state.dataHeatBal->SurfaceScreens(ScreenNum).ReflectCylinderVis *
                        (1.0 -
                         (state.dataHeatBal->SurfaceScreens(ScreenNum).BmBmTransVis + state.dataHeatBal->SurfaceScreens(ScreenNum).BmDifTransVis));

                    if (SumArea != 0) {
                        state.dataHeatBal->SurfaceScreens(ScreenNum).DifDifTrans = SumTrans / SumArea;
                        state.dataHeatBal->SurfaceScreens(ScreenNum).DifDifTransVis = SumTransVis / SumArea;
                        state.dataHeatBal->SurfaceScreens(ScreenNum).DifReflect = SumReflect / SumArea;
                        state.dataHeatBal->SurfaceScreens(ScreenNum).DifReflectVis = SumReflectVis / SumArea;
                    }
                    state.dataHeatBal->SurfaceScreens(ScreenNum).DifScreenAbsorp = max(
                        0.0,
                        (1.0 - state.dataHeatBal->SurfaceScreens(ScreenNum).DifDifTrans - state.dataHeatBal->SurfaceScreens(ScreenNum).DifReflect));

                    state.dataMaterial->Material(MatNum).AbsorpThermalBack = state.dataHeatBal->SurfaceScreens(ScreenNum).DifScreenAbsorp;
                    state.dataMaterial->Material(MatNum).AbsorpThermalFront = state.dataHeatBal->SurfaceScreens(ScreenNum).DifScreenAbsorp;
                    state.dataMaterial->Material(MatNum).ReflectSolBeamFront = state.dataHeatBal->SurfaceScreens(ScreenNum).DifReflect;
                    state.dataMaterial->Material(MatNum).ReflectSolBeamBack = state.dataHeatBal->SurfaceScreens(ScreenNum).DifReflect;

                } // (ShadingType == 'EXTERIORSCREEN')
            }
        } // End of screen surface initialization

        // Write transmittance versus direct normal angle to csv file

        if (PrintTransMap) {
            // Fortran version did not have error handling in case of file open failure. This one does.
            // Which is correct?
            auto screenCsvFile = state.files.screenCsv.open(state, "CalcWindowScreenComponents", state.files.outputControl.screen);

            //  WRITE(ScreenTransUnitNo,*)' '
            for (ScreenNum = 1; ScreenNum <= state.dataHeatBal->NumSurfaceScreens; ++ScreenNum) {
                MatNum = state.dataHeatBal->SurfaceScreens(ScreenNum).MaterialNumber;
                //   Do not print transmittance map if angle increment is equal to 0
                if (state.dataMaterial->Material(MatNum).ScreenMapResolution == 0) continue;
                FoundMaterial = false;
                for (i = ScreenNum + 1; i <= state.dataHeatBal->NumSurfaceScreens; ++i) {
                    //     Write out transmittance data once for each Material:WindowScreen object
                    if (MatNum == state.dataHeatBal->SurfaceScreens(i).MaterialNumber) FoundMaterial = true;
                }
                if (FoundMaterial) continue;
                //   Store transmittance at direct normal angle
                if (state.dataMaterial->Material(MatNum).ScreenMapResolution != 0) {
                    state.dataHeatBal->ScreenTrans(ScreenNum).Trans.allocate(90 / state.dataMaterial->Material(MatNum).ScreenMapResolution + 1,
                                                                             90 / state.dataMaterial->Material(MatNum).ScreenMapResolution + 1);
                    state.dataHeatBal->ScreenTrans(ScreenNum).Scatt.allocate(90 / state.dataMaterial->Material(MatNum).ScreenMapResolution + 1,
                                                                             90 / state.dataMaterial->Material(MatNum).ScreenMapResolution + 1);
                    state.dataHeatBal->ScreenTrans(ScreenNum).Trans = 0.0;
                    state.dataHeatBal->ScreenTrans(ScreenNum).Scatt = 0.0;
                    for (j = 90 / state.dataMaterial->Material(MatNum).ScreenMapResolution + 1; j >= 1; --j) {
                        for (i = 90 / state.dataMaterial->Material(MatNum).ScreenMapResolution + 1; i >= 1; --i) {
                            Real64 SunAzimuth =
                                state.dataMaterial->Material(MatNum).ScreenMapResolution * (j - 1) * DataGlobalConstants::DegToRadians;
                            Real64 SunAltitude =
                                state.dataMaterial->Material(MatNum).ScreenMapResolution * (i - 1) * DataGlobalConstants::DegToRadians;
                            CalcScreenTransmittance(state, 0, SunAltitude, SunAzimuth, ScreenNum);
                            state.dataHeatBal->ScreenTrans(ScreenNum).Trans(i, j) = state.dataHeatBal->SurfaceScreens(ScreenNum).BmBmTrans;
                            state.dataHeatBal->ScreenTrans(ScreenNum).Scatt(i, j) = state.dataHeatBal->SurfaceScreens(ScreenNum).BmDifTrans;
                        }
                    }

                    print(screenCsvFile,
                          "MATERIAL:WINDOWSCREEN:{}\n",
                          state.dataMaterial->Material(state.dataHeatBal->SurfaceScreens(ScreenNum).MaterialNumber).Name);
                    print(screenCsvFile,
                          "Tabular data for beam solar transmittance at varying \"relative\" azimuth (row) and "
                          "altitude (column) angles (deg) [relative to surface normal].\n");
                    print(screenCsvFile, ",90");
                    for (i = 90 / state.dataMaterial->Material(MatNum).ScreenMapResolution; i >= 2; --i) {
                        print(screenCsvFile, ",{}", (i - 1) * state.dataMaterial->Material(MatNum).ScreenMapResolution);
                    }
                    print(screenCsvFile, ",0\n");

                    for (j = 1; j <= 90 / state.dataMaterial->Material(MatNum).ScreenMapResolution + 1; ++j) {
                        print(screenCsvFile, "{}", (j - 1) * state.dataMaterial->Material(MatNum).ScreenMapResolution);
                        for (i = 90 / state.dataMaterial->Material(MatNum).ScreenMapResolution + 1; i >= 2; --i) {
                            print(screenCsvFile, ",{:.6R}", state.dataHeatBal->ScreenTrans(ScreenNum).Trans(i, j));
                        }
                        print(screenCsvFile, ",{:.6R}\n", state.dataHeatBal->ScreenTrans(ScreenNum).Trans(i, j));
                    }
                    print(screenCsvFile, "\n\n");

                    print(screenCsvFile,
                          "MATERIAL:WINDOWSCREEN:{}\n",
                          state.dataMaterial->Material(state.dataHeatBal->SurfaceScreens(ScreenNum).MaterialNumber).Name);
                    print(screenCsvFile,
                          "Tabular data for scattered solar transmittance at varying \"relative\" azimuth (row) and "
                          "altitude (column) angles (deg) [relative to surface normal].\n");

                    for (i = 1; i <= 90 / state.dataMaterial->Material(MatNum).ScreenMapResolution; ++i) {
                        print(screenCsvFile, ",{}", (i - 1) * state.dataMaterial->Material(MatNum).ScreenMapResolution);
                    }
                    print(screenCsvFile, ",{}\n", (i - 1) * state.dataMaterial->Material(MatNum).ScreenMapResolution);

                    for (j = 1; j <= 90 / state.dataMaterial->Material(MatNum).ScreenMapResolution + 1; ++j) {
                        print(screenCsvFile, "{}", (j - 1) * state.dataMaterial->Material(MatNum).ScreenMapResolution);
                        for (i = 1; i <= 90 / state.dataMaterial->Material(MatNum).ScreenMapResolution; ++i) {
                            print(screenCsvFile, ",{:.6R}", state.dataHeatBal->ScreenTrans(ScreenNum).Scatt(i, j));
                        }
                        print(screenCsvFile, ",{:.6R}\n", state.dataHeatBal->ScreenTrans(ScreenNum).Scatt(i, j));
                    }
                    print(screenCsvFile, "\n\n");
                }
            }
        }
        state.dataHeatBal->ScreenTrans.deallocate();
    }

    void BlindOpticsDiffuse(EnergyPlusData &state,
                            int const BlindNum,      // Blind number
                            int const ISolVis,       // 1 = solar and IR calculation; 2 = visible calculation
                            Array1A<Real64> const c, // Slat properties
                            Real64 const b_el,       // Slat elevation (radians)
                            Array1A<Real64> p        // Blind properties
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Hans Simmler
        //       DATE WRITTEN   July-Aug 1995
        //       MODIFIED       Aug 2001 (FCW): adapt to EnergyPlus
        //                      Aug 2002 (FCW): make corrections so that calculations are consistent with
        //                       G(i) = Sum over j of J(j)*F(j,i). Previously, i,j was
        //                      interchanged in F, so that
        //                       G(i) = Sum over j of J(j)*F(i,j), which is wrong.
        //                      This change was made to resolve discrepancies between EnergyPlus results
        //                      and blind transmittance measurements made at Oklahoma State Univ.
        //                      Feb 2004 (FCW): modify slat edge correction calc to avoid possible divide by zero
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // From the slat properties, calculates the diffuse solar, diffuse visible and IR
        // transmission and reflection properties of a window blind.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // "Solar-Thermal Window Blind Model for DOE-2," H. Simmler, U. Fischer and
        // F. Winkelmann, Lawrence Berkeley National Laboratory, Jan. 1996.

        // Argument array dimensioning
        c.dim(15);
        p.dim(16);

        Real64 ri; // Front and back IR slat reflectance
        Real64 rib;
        Real64 phib;                     // Elevation of slat normal vector (radians)
        Real64 phis;                     // Source elevation (radians)
        Real64 delphis;                  // Angle increment for integration over source distribution (radians)
        Array1D<Real64> fEdgeSource(10); // Slat edge correction factor vs source elevation
        Array1D<Real64> fEdgeA(2);       // Average slat edge correction factor for upper and lower quadrants
        //  seen by window blind
        Real64 gamma; // phib - phis
        int Iphis;    // Source elevation counter
        int IUpDown;  // =1 for source in upper quadrant, =2 for source in lower quadrant
        Real64 fEdge; // Slat edge correction factor
        Real64 fEdge1;
        Array1D<Real64> j(6);       // Slat section radiosity vector
        Array1D<Real64> G(6);       // Slat section irradiance vector
        Array1D<Real64> Q(6);       // Slat section radiance vector
        Array2D<Real64> F(6, 6);    // View factor array
        Array2D<Real64> X(4, 4);    // Exchange matrix
        Array2D<Real64> Xinv(4, 4); // Inverse of exchange matrix
        int k;                      // Array indices
        int m;
        Array1D_int indx(4);     // LU decomposition indices
        Real64 BlindIRreflFront; // Blind front IR reflectance
        Real64 BlindIRreflBack;  // Blind back IR reflectance

        // The slat input properties are:
        // c(1)    0. (unused)
        // c(2)    Slat width (m)
        // c(3)    Slat separation (m)
        // c(4)    0. (unused)
        // c(5)    0. (unused)
        // c(6)    0. (unused)
        //      The following are solar or visible properties
        // c(7)    trans beam-diff
        // c(8)    refl front beam-diff
        // c(9)    refl back beam-diff
        // c(10)   trans diff-diff
        // c(11)   refl front diff-diff
        // c(12)   refl back diff-diff
        //      The following are hemispherical thermal IR properties
        // c(13)   trans diff-diff
        // c(14)   emiss front diff
        // c(15)   emiss back diff

        // The calculated blind properties are:
        //      The following are solar or visible properties
        // p(1)    trans front beam-beam
        // p(2)    refl front beam-beam
        // p(3)    trans back beam-beam
        // p(4)    refl back beam-beam
        // p(5)    trans front beam-diff
        // p(6)    refl front beam-diff
        // p(7)    trans back beam-diff
        // p(8)    refl back beam-diff
        // p(9)    trans front diff-diff
        // p(10)   refl front diff-diff
        // p(11)   trans back diff-diff
        // p(12)   refl back diff-diff
        //      The following are IR properties
        // p(13)   IR trans front (same as IR trans back)
        // p(14)   IR emissivity front
        // p(15)   IR emissivity back
        // p(16)   0.0 (unused)

        //     Calculate view factors between slat sections (slat is divided longitudinally into two equal parts)

        ViewFac(c(2), c(3), b_el, DataGlobalConstants::PiOvr2, F);

        //     Set up exchange matrix X for diffuse properties

        for (k = 3; k <= 5; k += 2) {
            for (m = 3; m <= 6; ++m) {
                X(m - 2, k - 2) = -c(12) * F(k, m) - c(10) * F(k + 1, m);
                X(m - 2, k - 1) = -c(10) * F(k, m) - c(11) * F(k + 1, m);
            }
        }

        for (k = 1; k <= 4; ++k) {
            ++X(k, k);
        }

        indx = 0;
        InvertMatrix(state, X, Xinv, indx, 4, 4); // Autodesk:Note X modified by this call

        //---------Calculate diffuse short-wave properties for the front side of the blind

        //     Sources

        Q(3) = c(12) * F(3, 1) + c(10) * F(4, 1);
        Q(4) = c(10) * F(3, 1) + c(11) * F(4, 1);
        Q(5) = c(12) * F(5, 1) + c(10) * F(6, 1);
        Q(6) = c(10) * F(5, 1) + c(11) * F(6, 1);

        //     Radiosities

        j(1) = 1.0;
        j(2) = 0.0;
        for (k = 3; k <= 6; ++k) {
            j(k) = 0.0;
            for (m = 3; m <= 6; ++m) {
                j(k) += Xinv(m - 2, k - 2) * Q(m);
            }
        }

        //     Irradiances

        for (k = 1; k <= 6; ++k) {
            G(k) = 0.0;
            for (m = 1; m <= 6; ++m) {
                // G(k)=G(k)+F(k,m)*J(m)
                G(k) += j(m) * F(k, m);
            }
        }

        //     Slat edge correction factor
        phib = b_el;
        delphis = DataGlobalConstants::PiOvr2 / 10.0;
        for (IUpDown = 1; IUpDown <= 2; ++IUpDown) {
            for (Iphis = 1; Iphis <= 10; ++Iphis) {
                phis = -(Iphis - 0.5) * delphis;
                if (IUpDown == 2) phis = (Iphis - 0.5) * delphis;
                fEdgeSource(Iphis) = 0.0;
                fEdge1 = 0.0;
                gamma = phib - phis;
                if (std::abs(std::sin(gamma)) > 0.01) {
                    if ((phib > 0.0 && phib <= DataGlobalConstants::PiOvr2 && phis <= phib) ||
                        (phib > DataGlobalConstants::PiOvr2 && phib <= DataGlobalConstants::Pi && phis > -(DataGlobalConstants::Pi - phib))) {
                        fEdge1 = state.dataHeatBal->Blind(BlindNum).SlatThickness * std::abs(std::sin(gamma)) /
                                 ((state.dataHeatBal->Blind(BlindNum).SlatSeparation +
                                   state.dataHeatBal->Blind(BlindNum).SlatThickness / std::abs(std::sin(phib))) *
                                  std::cos(phis));
                    }
                    fEdgeSource(Iphis) = min(1.0, std::abs(fEdge1));
                }
            }
            fEdgeA(IUpDown) = DiffuseAverage(fEdgeSource);
        }
        fEdge = 0.5 * (fEdgeA(1) + fEdgeA(2));

        //     Front diffuse-diffuse transmittance (transmittance of slat edge assumed zero)
        p(9) = G(2) * (1.0 - fEdge);

        //     Front diffuse-diffuse reflectance (edge of slat is assumed to have same diffuse
        //     reflectance as front side of slat, c(11))
        p(10) = G(1) * (1.0 - fEdge) + fEdge * c(11);

        //-----------Calculate diffuse short-wave properties for the back side of the blind

        //     Sources

        Q(3) = c(12) * F(3, 2) + c(10) * F(4, 2);
        Q(4) = c(10) * F(3, 2) + c(11) * F(4, 2);
        Q(5) = c(12) * F(5, 2) + c(10) * F(6, 2);
        Q(6) = c(10) * F(5, 2) + c(11) * F(6, 2);

        //     Radiosities

        j(1) = 0.0;
        j(2) = 1.0;
        for (k = 3; k <= 6; ++k) {
            j(k) = 0.0;
            for (m = 3; m <= 6; ++m) {
                j(k) += Xinv(m - 2, k - 2) * Q(m);
            }
        }

        //     Irradiances

        for (k = 1; k <= 6; ++k) {
            G(k) = 0.0;
            for (m = 1; m <= 6; ++m) {
                // G(k)=G(k)+F(k,m)*J(m)
                G(k) += j(m) * F(k, m);
            }
        }

        //     Back diffuse-diffuse transmittance
        p(11) = G(1) * (1.0 - fEdge);

        //     Back hemi-hemi reflectance
        p(12) = G(2) * (1.0 - fEdge) + fEdge * c(11);

        if (ISolVis == 1) {

            //-----------Calculate IR properties of the blind
            //           (use same set of view factors as for diffuse short-wave properties)

            //     Front and back slat IR reflectances
            ri = 1 - c(13) - c(14);
            rib = 1 - c(13) - c(15);

            //     Set up exchange matrix X for diffuse properties

            for (k = 3; k <= 5; k += 2) {
                for (m = 3; m <= 6; ++m) {
                    X(m - 2, k - 2) = -rib * F(k, m) - c(13) * F(k + 1, m);
                    X(m - 2, k - 1) = -c(13) * F(k, m) - ri * F(k + 1, m);
                }
            }

            for (k = 1; k <= 4; ++k) {
                ++X(k, k);
            }

            indx = 0;
            InvertMatrix(state, X, Xinv, indx, 4, 4); // Autodesk:Note X modified by this call

            //---------Calculate diffuse IR properties for the FRONT side of the blind

            //     Sources

            Q(3) = rib * F(3, 1) + c(13) * F(4, 1);
            Q(4) = c(13) * F(3, 1) + ri * F(4, 1);
            Q(5) = rib * F(5, 1) + c(13) * F(6, 1);
            Q(6) = c(13) * F(5, 1) + ri * F(6, 1);

            //     Radiosities

            j(1) = 1.0;
            j(2) = 0.0;
            for (k = 3; k <= 6; ++k) {
                j(k) = 0.0;
                for (m = 3; m <= 6; ++m) {
                    j(k) += Xinv(m - 2, k - 2) * Q(m);
                }
            }

            //     Irradiances
            for (k = 1; k <= 6; ++k) {
                G(k) = 0.0;
                for (m = 1; m <= 6; ++m) {
                    // G(k)=G(k)+F(k,m)*J(m)
                    G(k) += j(m) * F(k, m);
                }
            }

            //     Front diffuse-diffuse IR transmittance (transmittance of slat edge assumed zero)
            p(13) = G(2) * (1.0 - fEdge);

            //     Front diffuse-diffuse IR reflectance (edge of slat is assumed to have same IR
            //     reflectance as front side of slat, ri)
            BlindIRreflFront = G(1) * (1.0 - fEdge) + fEdge * ri;

            //     Front IR emissivity
            p(14) = max(0.0001, 1.0 - p(13) - BlindIRreflFront);

            //-----------Calculate diffuse IR properties for the BACK side of the blind

            //     Sources

            Q(3) = rib * F(3, 2) + c(13) * F(4, 2);
            Q(4) = c(13) * F(3, 2) + ri * F(4, 2);
            Q(5) = rib * F(5, 2) + c(13) * F(6, 2);
            Q(6) = c(13) * F(5, 2) + ri * F(6, 2);

            //     Radiosities

            j(1) = 0.0;
            j(2) = 1.0;
            for (k = 3; k <= 6; ++k) {
                j(k) = 0.0;
                for (m = 3; m <= 6; ++m) {
                    j(k) += Xinv(m - 2, k - 2) * Q(m);
                }
            }

            //     Irradiances

            for (k = 1; k <= 6; ++k) {
                G(k) = 0.0;
                for (m = 1; m <= 6; ++m) {
                    // G(k)=G(k)+F(k,m)*J(m)
                    G(k) += j(m) * F(k, m);
                }
            }

            //     Back diffuse-diffuse IR reflectance
            BlindIRreflBack = G(2) * (1.0 - fEdge) + fEdge * ri;

            //     Back IR emissivity
            p(15) = max(0.0001, 1.0 - p(13) - BlindIRreflBack);

        } // End of IR properties calculation
    }

    //**********************************************************************************************

    void BlindOpticsBeam(EnergyPlusData &state,
                         int const BlindNum,      // Blind number
                         Array1A<Real64> const c, // Slat properties (equivalent to BLD_PR)
                         Real64 const b_el,       // Slat elevation (radians)
                         Real64 const s_el,       // Solar profile angle (radians)
                         Array1A<Real64> p        // Blind properties (equivalent to ST_LAY)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Hans Simmler
        //       DATE WRITTEN   July-Aug 1995
        //       MODIFIED       Aug 2001 (FCW): adapt to EnergyPlus
        //                      Aug 2002 (FCW): make corrections so that calculations are consistent with
        //                       G(i) = Sum over j of J(j)*F(j,i). Previously, i,j was
        //                      interchanged in F, so that
        //                       G(i) = Sum over j of J(j)*F(i,j), which is wrong.
        //                      This change was made to resolve discrepancies between EnergyPlus results
        //                      and blind transmittance measurements made at Oklahoma State Univ.
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //     Calculates the beam radiation properties of a
        //     window blind consisting of flat slats with known material properties.
        //     The calculation for the reverse direction is done with the radiation source
        //     reflected at the window plane.

        // REFERENCES:
        // "Solar-Thermal Window Blind Model for DOE-2," H. Simmler, U. Fischer and
        // F. Winkelmann, Lawrence Berkeley National Laboratory, Jan. 1996.

        using General::BlindBeamBeamTrans; // Blind beam-to-beam transmittance function

        // Argument array dimensioning
        c.dim(15);
        p.dim(16);

        // The slat input properties are:
        // c(1)    0. (unused)
        // c(2)    Slat width (m)
        // c(3)    Slat separation (m)
        // c(4)    0. (unused)
        // c(5)    0. (unused)
        // c(6)    0. (unused)
        //      The following are solar or visible properties
        // c(7)    trans beam-diff
        // c(8)    refl front beam-diff
        // c(9)    refl back beam-diff
        // c(10)   trans diff-diff
        // c(11)   refl front diff-diff
        // c(12)   refl back diff-diff
        //      The following are hemispherical thermal IR properties
        // c(13)   trans diff-diff
        // c(14)   emiss front diff
        // c(15)   emiss back diff

        // The calculated blind properties are:
        //      The following are solar or visible properties
        // p(1)    trans front beam-beam
        // p(2)    refl front beam-beam
        // p(3)    trans back beam-beam
        // p(4)    refl back beam-beam
        // p(5)    trans front beam-diff
        // p(6)    refl front beam-diff
        // p(7)    trans back beam-diff
        // p(8)    refl back beam-diff
        // p(9)    trans front diff-diff
        // p(10)   refl front diff-diff
        // p(11)   trans back diff-diff
        // p(12)   refl back diff-diff
        //      The following are IR properties
        // p(13)   IR trans front (same as IR trans back)
        // p(14)   IR emissivity front
        // p(15)   IR emissivity back
        // p(16)   0.0 (unused)

        Real64 phib;                // Elevation angle of normal vector to front of slat (0 to pi radians)
        Real64 phis;                // Elevation angle of source vector; same as "profile angle" (-pi/2 to pi/2 radians)
        Real64 gamma;               // phib - phis (radians)
        Array1D<Real64> j(6);       // Slat surface section radiosity vector
        Array1D<Real64> G(6);       // Slat surface section irradiance vector
        Array1D<Real64> Q(6);       // Slat surface section source vector
        Array2D<Real64> F(6, 6);    // View factor array
        Array2D<Real64> X(4, 4);    // X*J = Q
        Array2D<Real64> Xinv(4, 4); // J = Xinv*Q
        Real64 fEdge;               // Slat edge correction factor
        Real64 fEdge1;
        int i; // Array indices
        int k;
        int m;
        Array1D_int indx(4); // Indices for LU decomposition

        p = 0.0;

        //     Elevation of radiation source; source is assumed to be in a plane that
        //     (1) contains the slat outward normal and (2) is perpendicular to plane of the blinds.
        phis = s_el;

        //     Elevation of slat outward normal
        phib = b_el;

        //     Loop twice for front and back side properties of blind
        for (i = 0; i <= 2; i += 2) {

            //       For back-side properties, reflect the source position so that it is the mirror
            //       image of the original source position, where the "mirror" is in the plane of the
            //       blinds. This is equivalent to keeping the original source position but rotating
            //       the slats so that the original slat angle (e.g., 45 deg) becomes 180 - original slat
            //       angle (135 deg).

            if (i == 2) {
                phib = DataGlobalConstants::Pi - phib;
            }

            //       Correction factor that accounts for finite thickness of slats. It is used to modify the
            //       blind transmittance and reflectance to account for reflection and absorption by the
            //       edge of the slat. fEdge is ratio of area subtended by edge of slat
            //       to area between tops of adjacent slats.

            fEdge = 0.0;
            fEdge1 = 0.0;
            gamma = phib - phis;
            if (std::abs(std::sin(gamma)) > 0.01) {
                if ((phib > 0.0 && phib <= DataGlobalConstants::PiOvr2 && phis <= phib) ||
                    (phib > DataGlobalConstants::PiOvr2 && phib <= DataGlobalConstants::Pi && phis > -(DataGlobalConstants::Pi - phib))) {
                    fEdge1 = state.dataHeatBal->Blind(BlindNum).SlatThickness * std::abs(std::sin(gamma)) /
                             ((state.dataHeatBal->Blind(BlindNum).SlatSeparation +
                               state.dataHeatBal->Blind(BlindNum).SlatThickness / std::abs(std::sin(phib))) *
                              std::cos(phis));
                }
                fEdge = min(1.0, std::abs(fEdge1));
            }

            //       Direct-to-direct transmittance (portion of beam that passes between slats without
            //       without touching them

            p(1 + i) = BlindBeamBeamTrans(phis,
                                          phib,
                                          state.dataHeatBal->Blind(BlindNum).SlatWidth,
                                          state.dataHeatBal->Blind(BlindNum).SlatSeparation,
                                          state.dataHeatBal->Blind(BlindNum).SlatThickness);
            //       Direct-to-direct reflectance; this is zero for now since all reflection is assumed to be diffuse.
            p(2 + i) = 0.0;

            //       View factors between slat sections for calculating direct-to-diffuse transmittance and reflectance
            ViewFac(c(2), c(3), phib, phis, F);

            //       Set up exchange matrix X for calculating direct-to-diffuse properties

            for (k = 3; k <= 5; k += 2) {
                for (m = 3; m <= 6; ++m) {
                    X(m - 2, k - 2) = -c(12) * F(k, m) - c(10) * F(k + 1, m);
                    X(m - 2, k - 1) = -c(10) * F(k, m) - c(11) * F(k + 1, m);
                }
            }

            for (k = 1; k <= 4; ++k) {
                ++X(k, k);
            }

            indx = 0;
            // In the following, note that InvertMatrix changes X
            InvertMatrix(state, X, Xinv, indx, 4, 4);

            //       Set up sources for direct-diffuse slat properties
            if (std::abs(phis - phib) <= DataGlobalConstants::PiOvr2) { // Beam hits front of slat
                Q(3) = c(4) + c(7);                                     // beam-beam trans of slat + beam-diff trans of slat
                Q(4) = c(5) + c(8);                                     // front beam-beam refl of slat + front beam-diff refl of slat
            } else {                                                    // Beam hits back of slat
                Q(3) = c(6) + c(9);                                     // back beam-beam refl of slat  + back beam-diff refl of slat
                Q(4) = c(4) + c(7);                                     // beam-beam trans of slat + beam-diff trans of slat
            }

            //       Correct for fraction of beam that is not directly transmitted; 1 - this fraction is
            //       the fraction of the incoming beam that is incident on the front or back surfaces of the slats.
            Q(3) *= (1.0 - p(1 + i));
            Q(4) *= (1.0 - p(1 + i));

            //       Radiosities (radiance of slat sections)
            j(1) = 0.0;
            j(2) = 0.0;
            for (k = 3; k <= 6; ++k) {
                j(k) = 0.0;
                for (m = 3; m <= 4; ++m) {
                    j(k) += Xinv(m - 2, k - 2) * Q(m);
                }
            }

            //       Irradiance on slat sections
            for (k = 1; k <= 6; ++k) {
                G(k) = 0.0;
                for (m = 3; m <= 6; ++m) {
                    G(k) += j(m) * F(k, m);
                }
            }

            //       Direct-to-diffuse transmittance
            p(5 + i) = G(2) * (1.0 - fEdge);

            //       Direct-to-diffuse reflectance (assuming the edge reflectance is the same as the
            //       reflectance of the front side of the slat, C(8))
            p(6 + i) = G(1) * (1.0 - fEdge) + fEdge * c(8);

        } // End of loop over front and back side properties of blind
    }

    //********************************************************************************************

    void ViewFac(Real64 const s,    // Slat width (m)
                 Real64 const h,    // Distance between faces of adjacent slats (m)
                 Real64 const phib, // Elevation angle of normal to slat (radians)
                 Real64 const phis, // Profile angle of radiation source (radians)
                 Array2A<Real64> F  // View factor array
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Hans Simmler
        //       DATE WRITTEN   July-Aug 1995
        //       MODIFIED       Aug 2001 (FCW): adapt to EnergyPlus
        //                      Apr 2002 (FCW): prevent sqrt of small negative argument
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //     Calculates the view factors between sections of adjacent slats,
        //     where each slat is divided longitudinally into two equal sections whose
        //     dimensions depend on source profile angle and slat geometry. The view
        //     factors are used in BlindOpticsBeam and BlindOpticsDiffuse to determine blind
        //     transmittance and reflectance for short-wave and long-wave radiation.

        // METHODOLOGY EMPLOYED:
        //     Uses expressions for view factor between flat strips with a common edge
        //     and flat strips displaced from one another. See engineering documentation.

        // REFERENCES:
        // "Solar-Thermal Window Blind Model for DOE-2," H. Simmler, U. Fischer and
        // F. Winkelmann, Lawrence Berkeley National Laboratory, Jan. 1996.

        // Argument array dimensioning
        F.dim(6, 6);

        Array1D<Real64> L(6); // Length of slat sections: L1 = L2 = h; L3, L5 = length
        Real64 L3;
        Real64 L5;
        //  of upper slat sections; L4, L6 = length of lower slat
        //  slat sections (m)
        Real64 d1; // Slat geometry variables (m)
        Real64 d2;
        Real64 d3;
        Real64 d4;
        Real64 d5;
        Real64 d6;
        Real64 h2; // h**2
        Real64 ht; // 2*h
        Real64 w;  // Slat geometry variable (m)
        Real64 a;  // Intermediate variable (m)
        Real64 co; // Cosine of source profile angle
        int i;     // View factor array indices
        int j;

        h2 = pow_2(h);
        ht = 2.0 * h;
        co = std::cos(phis);
        if (std::abs(co) < 0.001) co = 0.0;
        w = ht;
        if (co != 0.0) w = s * std::cos(phib - phis) / co;
        L3 = s * h / std::abs(w);
        if (L3 > s) L3 = s;
        L5 = s - L3;
        a = ht * std::cos(phib);
        // MAX(0.,...) in the following prevents small negative argument for sqrt
        d1 = std::sqrt(max(0.0, s * s + h2 + a * s));
        d2 = std::sqrt(max(0.0, s * s + h2 - a * s));
        d3 = std::sqrt(max(0.0, L3 * L3 + h2 + a * L3));
        d4 = std::sqrt(max(0.0, L3 * L3 + h2 - a * L3));
        d5 = std::sqrt(max(0.0, L5 * L5 + h2 - a * L5));
        d6 = std::sqrt(max(0.0, L5 * L5 + h2 + a * L5));
        for (i = 1; i <= 6; ++i) {
            F(i, i) = 0.0;
        }
        F(1, 1) = 0.0;
        F(2, 1) = (d1 + d2 - 2.0 * s) / ht;
        F(3, 1) = (h + L3 - d3) / ht;
        F(4, 1) = (h + L3 - d4) / ht;
        F(5, 1) = (L5 + d3 - d1) / ht;
        F(6, 1) = (L5 + d4 - d2) / ht;
        F(3, 2) = (L3 + d5 - d2) / ht;
        F(4, 2) = (L3 + d6 - d1) / ht;
        F(5, 2) = (h + L5 - d5) / ht;
        F(6, 2) = (h + L5 - d6) / ht;
        F(4, 3) = (d3 + d4 - ht) / (2.0 * L3);
        F(5, 3) = 0.0;
        F(6, 3) = (d2 + h - d4 - d5) / (2.0 * L3);
        F(5, 4) = (d1 + h - d3 - d6) / (2.0 * L3);
        F(6, 4) = 0.0;
        F(6, 5) = 0.0;
        if (L5 > 0.0) F(6, 5) = (d5 + d6 - ht) / (2.0 * L5);
        L(1) = h;
        L(2) = h;
        L(3) = L3;
        L(4) = L3;
        L(5) = L5;
        L(6) = L5;
        for (i = 2; i <= 6; ++i) {
            for (j = 1; j <= i - 1; ++j) {
                F(j, i) = 0.0;
                if (L(i) > 0.0) F(j, i) = F(i, j) * L(j) / L(i);
            }
        }
    }

    //*****************************************************************************************

    void InvertMatrix(EnergyPlusData &state,
                      Array2A<Real64> a, // Matrix to be inverted
                      Array2A<Real64> y, // Inverse of matrix a
                      Array1A_int indx,  // Index vector for LU decomposition
                      int const np,      // Dimension of matrix
                      int const n)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Hans Simmler
        //       DATE WRITTEN   July-Aug 1995
        //       MODIFIED       Aug 2001 (FCW): adapt to EnergyPlus
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //     Inverts a matrix.

        // METHODOLOGY EMPLOYED:
        //     Uses LU decomposition.

        // Argument array dimensioning
        a.dim(np, np);
        y.dim(np, np);
        indx.dim(np);

        int i; // Array indices
        int j;
        int d;

        y = 0.0;
        for (i = 1; i <= n; ++i) {
            y(i, i) = 1.0;
        }
        indx = 0;

        LUDCMP(state, a, n, np, indx, d);

        for (j = 1; j <= n; ++j) {
            LUBKSB(a, n, np, indx, y(j, 1));
        }
    }

    //*****************************************************************************************

    void LUDCMP(EnergyPlusData &state,
                Array2A<Real64> A, // matrix
                int const N,
                int const NP,
                Array1A_int INDX,
                int &D)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Winkelmann?
        //       DATE WRITTEN   <date_written>
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine performs a LU decompostion of given matrix.

        // Argument array dimensioning
        A.dim(NP, NP);
        INDX.dim(N);

        Array1D<Real64> VV(100);
        Real64 sum;
        Real64 aamax;
        Real64 dum;
        int i;
        int j;
        int K;
        int imax;

        D = 1;
        for (i = 1; i <= N; ++i) {
            aamax = 0.0;
            for (j = 1; j <= N; ++j) {
                if (std::abs(A(j, i)) > aamax) aamax = std::abs(A(j, i));
            }

            if (aamax == 0.0) {
                ShowFatalError(state, "Singular matrix in LUDCMP, window calculations");
            }
            VV(i) = 1.0 / aamax; // Was commented out prior to 10/5/01, which caused overflows
            // in this routine in rare cases
        }

        for (j = 1; j <= N; ++j) {
            if (j > 1) {
                for (i = 1; i <= j - 1; ++i) {
                    sum = A(j, i);
                    if (i > 1) {
                        for (K = 1; K <= i - 1; ++K) {
                            sum -= A(K, i) * A(j, K);
                        }

                        A(j, i) = sum;
                    }
                }
            }
            aamax = 0.0;
            for (i = j; i <= N; ++i) {
                sum = A(j, i);
                if (j > 1) {
                    for (K = 1; K <= j - 1; ++K) {
                        sum -= A(K, i) * A(j, K);
                    }
                    A(j, i) = sum;
                }

                dum = VV(i) * std::abs(sum);
                if (dum >= aamax) {
                    imax = i;
                    aamax = dum;
                }
            }

            if (j != imax) {
                for (K = 1; K <= N; ++K) {
                    dum = A(K, imax);
                    A(K, imax) = A(K, j);
                    A(K, j) = dum;
                }

                D = -D;
                VV(imax) = VV(j);
            }

            INDX(j) = imax;
            if (j != N) {
                if (A(j, j) == 0.0) A(j, j) = DataGlobalConstants::rTinyValue;

                dum = 1.0 / A(j, j);
                for (i = j + 1; i <= N; ++i) {
                    A(j, i) *= dum;
                }
            }
        }

        if (A(N, N) == 0.0) A(N, N) = DataGlobalConstants::rTinyValue;
    }

    //*****************************************************************************************

    void LUBKSB(Array2A<Real64> A, int const N, int const NP, Array1A_int INDX, Array1A<Real64> B)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         <author>
        //       DATE WRITTEN   <date_written>
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine performs back substitution of a LU matrix.

        // Argument array dimensioning
        A.dim(NP, NP);
        INDX.dim(N);
        B.dim(N);

        int j;
        int i;
        int ii;
        int LL;
        Real64 sum;

        ii = 0;

        for (i = 1; i <= N; ++i) {
            LL = INDX(i);
            sum = B(LL);
            B(LL) = B(i);
            if (ii != 0) {
                for (j = ii; j <= i - 1; ++j) {
                    sum -= A(j, i) * B(j);
                }
            } else if (sum != 0.0) {
                ii = i;
            }

            B(i) = sum;
        }

        for (i = N; i >= 1; --i) {
            sum = B(i);
            if (i < N) {
                for (j = i + 1; j <= N; ++j) {
                    sum -= A(j, i) * B(j);
                }
            }

            B(i) = sum / A(i, i);
        }
    }

    // added for custom solar or visible spectrum

    void CheckAndReadCustomSprectrumData(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         T. Hong
        //       DATE WRITTEN   August 2013
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Check, read, and assign the custom solar or visible spectrum to:
        //  solar: nume, wle(nume), e(nume). nume = 107
        //  visible: numt3, wlt3(numt3), y30(numt3). numt3 = 81
        // Three related IDD objects:
        //  EnergyManagementSystem:ConstructionIndexVariable
        //  Site:SolarAndVisibleSpectrum, Site:SpectrumData

        // METHODOLOGY EMPLOYED:
        // Overwriting the default values

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        bool ErrorsFound(false); // If errors detected in input
        int NumAlphas;           // Number of Alphas for each GetobjectItem call
        int NumNumbers;          // Number of Numbers for each GetobjectItem call
        int NumArgs;
        int IOStatus;
        Array1D_string cAlphaArgs;    // Alpha input items for object
        Array1D<Real64> rNumericArgs; // Numeric input items for object

        std::string cCurrentModuleObject;
        std::string cSolarSpectrum;
        std::string cVisibleSpectrum;
        int iSolarSpectrum(0);
        int iVisibleSpectrum(0);
        int NumSiteSpectrum(0);
        int Loop;
        int iTmp;

        if (state.dataWindowManager->RunMeOnceFlag) return;

        // Step 1 - check whether there is custom solar or visible spectrum
        cCurrentModuleObject = "Site:SolarAndVisibleSpectrum";
        NumSiteSpectrum = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        // no custom spectrum data, done!
        if (NumSiteSpectrum == 0) {
            state.dataWindowManager->RunMeOnceFlag = true;
            return;
        }

        // read custom spectrum data from Site:SolarAndVisibleSpectrum
        if (NumSiteSpectrum > 1) { // throw error
            ShowSevereError(state, "Only one " + cCurrentModuleObject + " object is allowed");
            ErrorsFound = true;
        }

        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumArgs, NumAlphas, NumNumbers);
        cAlphaArgs.allocate(NumAlphas);
        rNumericArgs.dimension(NumNumbers, 0.0);

        if (NumSiteSpectrum == 1) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     1,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlphas,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus);

            // use default spectrum data, done!
            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(2), "Default")) {
                state.dataWindowManager->RunMeOnceFlag = true;
                return;
            }

            // now read custom solar and visible spectrum data
            cSolarSpectrum = state.dataIPShortCut->cAlphaArgs(3);
            cVisibleSpectrum = state.dataIPShortCut->cAlphaArgs(4);

            cCurrentModuleObject = "Site:SpectrumData";
            NumSiteSpectrum = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
            if (NumSiteSpectrum == 0) { // throw error
                ShowSevereError(state, "No " + cCurrentModuleObject + " object is found");
                ErrorsFound = true;
            }

            cAlphaArgs.deallocate();
            rNumericArgs.deallocate();

            state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumArgs, NumAlphas, NumNumbers);
            cAlphaArgs.allocate(NumAlphas);
            rNumericArgs.dimension(NumNumbers, 0.0);

            iSolarSpectrum = 0;
            iVisibleSpectrum = 0;
            for (Loop = 1; Loop <= NumSiteSpectrum; ++Loop) {
                // Step 2 - read user-defined spectrum data
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         cCurrentModuleObject,
                                                                         Loop,
                                                                         state.dataIPShortCut->cAlphaArgs,
                                                                         NumAlphas,
                                                                         state.dataIPShortCut->rNumericArgs,
                                                                         NumNumbers,
                                                                         IOStatus);
                if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(1), cSolarSpectrum)) {
                    iSolarSpectrum = Loop;
                    // overwrite the default solar spectrum
                    if (NumNumbers > 2 * state.dataWindowManager->nume) {
                        ShowSevereError(state,
                                        "Solar spectrum data pair is more than 107 - " + cCurrentModuleObject + " - " +
                                            state.dataIPShortCut->cAlphaArgs(1));
                        ErrorsFound = true;
                    } else {
                        // Step 3 - overwrite default solar spectrum data
                        for (iTmp = 1; iTmp <= state.dataWindowManager->nume; ++iTmp) {
                            if (iTmp <= NumNumbers / 2) {
                                state.dataWindowManager->wle[iTmp - 1] = state.dataIPShortCut->rNumericArgs(2 * iTmp - 1);
                                state.dataWindowManager->e[iTmp - 1] = state.dataIPShortCut->rNumericArgs(2 * iTmp);
                            } else {
                                state.dataWindowManager->wle[iTmp - 1] = 0.0;
                                state.dataWindowManager->e[iTmp - 1] = 0.0;
                            }
                        }
                    }
                }
                if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(1), cVisibleSpectrum)) {
                    iVisibleSpectrum = Loop;
                    // overwrite the default solar spectrum
                    if (NumNumbers > 2 * state.dataWindowManager->numt3) {
                        ShowSevereError(state,
                                        "Visible spectrum data pair is more than 81 - " + cCurrentModuleObject + " - " +
                                            state.dataIPShortCut->cAlphaArgs(1));
                        ErrorsFound = true;
                    } else {
                        // Step 3 - overwrite default visible spectrum data
                        for (iTmp = 1; iTmp <= state.dataWindowManager->numt3; ++iTmp) {
                            if (iTmp <= NumNumbers / 2) {
                                state.dataWindowManager->wlt3[iTmp - 1] = state.dataIPShortCut->rNumericArgs(2 * iTmp - 1);
                                state.dataWindowManager->y30[iTmp - 1] = state.dataIPShortCut->rNumericArgs(2 * iTmp);
                            } else {
                                state.dataWindowManager->wlt3[iTmp - 1] = 0.0;
                                state.dataWindowManager->y30[iTmp - 1] = 0.0;
                            }
                        }
                    }
                }
                if ((iSolarSpectrum > 0) && (iVisibleSpectrum > 0)) break;
            }
        }

        cAlphaArgs.deallocate();
        rNumericArgs.deallocate();

        if (ErrorsFound) {
            ShowFatalError(state, "Errors found in processing input for user-defined solar/visible spectrum");
        }

        state.dataWindowManager->RunMeOnceFlag = true;
    }

    //*****************************************************************************************

    void initWindowModel(EnergyPlusData &state)
    {
        const std::string objectName = "WindowsCalculationEngine";
        state.dataWindowManager->inExtWindowModel = CWindowModel::WindowModelFactory(state, objectName);
        state.dataWindowManager->winOpticalModel = CWindowOpticalModel::WindowOpticalModelFactory(state);
    }

    //*****************************************************************************************

} // namespace WindowManager

} // namespace EnergyPlus
