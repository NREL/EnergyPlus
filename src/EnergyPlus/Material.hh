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

#ifndef Material_hh_INCLUDED
#define Material_hh_INCLUDED

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/PhaseChangeModeling/HysteresisModel.hh>

namespace EnergyPlus {

namespace Material {

    struct MaterialProperties
    {
        // Members
        std::string Name; // Name of material layer
        int Group;        // Material group type (see Material Parameters above.  Currently
        // active: RegularMaterial, Shade, Air, WindowGlass,
        // WindowGas, WindowBlind, WindowGasMixture, Screen, EcoRoof,
        // IRTMaterial, WindowSimpleGlazing, ComplexWindowShade, ComplexWindowGap)
        int Roughness; // Surface roughness index (See Surface Roughness parameters
        // above.  Current: VerySmooth, Smooth, MediumSmooth,
        // MediumRough, Rough, VeryRough)
        // Thermo-physical material properties
        Real64 Conductivity; // Thermal conductivity of layer (W/m2K)
        Real64 Density;      // Layer density (kg/m3)
        Real64 IsoMoistCap;  // Isothermal moisture capacity on water vapor density (m3/kg)
        Real64 Porosity;     // Layer porosity
        Real64 Resistance;   // Layer thermal resistance (alternative to Density,
        // Conductivity, Thickness, and Specific Heat; K/W)
        bool ROnly;           // Material defined with "R" only
        Real64 SpecHeat;      // Layer specific heat (J/kgK)
        Real64 ThermGradCoef; // Thermal-gradient coefficient for moisture capacity
        // based on the water vapor density (kg/kgK)
        Real64 Thickness;    // Layer thickness (m)
        Real64 VaporDiffus;  // Layer vapor diffusivity
        Array1D_int GasType; // Gas type (air=1, argon=2, krypton=3, xenon=4, custom=0) for
        //  up to 5 gases in a mixture [Window gas only].  It is defined as parameter (GasCoefs)
        int GlassSpectralDataPtr;         // Number of a spectral data set associated with a window glass material
        int NumberOfGasesInMixture;       // Number of gases in a window gas mixture
        Array2D<Real64> GasCon;           // Gas conductance coefficients for up to 5 gases in a mixture
        Array2D<Real64> GasVis;           // Gas viscosity coefficients for up to 5 gases in a mixture
        Array2D<Real64> GasCp;            // Gas specific-heat coefficients for up to 5 gases in a mixture
        Array1D<Real64> GasWght;          // Gas molecular weight for up to 5 gases in a mixture
        Array1D<Real64> GasSpecHeatRatio; // Gas specific heat ratio (used for low pressure calculations)
        Array1D<Real64> GasFract;         // Gas fractions for up to 5 gases in a mixture
        // Radiation parameters
        Real64 AbsorpSolar;              // Layer solar absorptance
        Real64 AbsorpSolarInput;         // Layer solar absorptance input by user
        bool AbsorpSolarEMSOverrideOn;   // if true, then EMS calling to override value for solar absorptance
        Real64 AbsorpSolarEMSOverride;   // value to use when EMS calling to override value for solar absorptance
        Real64 AbsorpThermal;            // Layer thermal absorptance
        Real64 AbsorpThermalInput;       // Layer thermal absorptance input by user
        bool AbsorpThermalEMSOverrideOn; // if true, then EMS calling to override value for thermal absorptance
        Real64 AbsorpThermalEMSOverride; // value to use when EMS calling to override value for thermal absorptance
        Real64 AbsorpVisible;            // Layer Visible Absorptance
        Real64 AbsorpVisibleInput;       // Layer Visible Absorptance input by user
        bool AbsorpVisibleEMSOverrideOn; // if true, then EMS calling to override value for visible absorptance
        Real64 AbsorpVisibleEMSOverride; // value to use when EMS calling to override value for visible absorptance
        // Window-related radiation parameters
        Real64 Trans;                    // Transmittance of layer (glass, shade)
        Real64 TransVis;                 // Visible transmittance (at normal incidence)
        Real64 GlassTransDirtFactor;     // Multiplier on glass transmittance due to dirt
        bool SolarDiffusing;             // True if glass diffuses beam solar radiation
        Real64 ReflectShade;             // Shade or screen reflectance (interior shade only)
        Real64 ReflectShadeVis;          // Shade reflectance for visible radiation
        Real64 AbsorpThermalBack;        // Infrared radiation back absorption
        Real64 AbsorpThermalFront;       // Infrared radiation front absorption
        Real64 ReflectSolBeamBack;       // Solar back reflectance (beam to everything)
        Real64 ReflectSolBeamFront;      // Solar front reflectance (beam to everything)
        Real64 ReflectSolDiffBack;       // Solar back diffuse reflectance
        Real64 ReflectSolDiffFront;      // Solar front diffuse reflectance
        Real64 ReflectVisBeamBack;       // Visible back reflectance (beam to everything)
        Real64 ReflectVisBeamFront;      // Visible front reflectance (beam to everything)
        Real64 ReflectVisDiffBack;       // Visible back diffuse reflectance
        Real64 ReflectVisDiffFront;      // Visible front diffuse reflectance
        std::string ReflectanceModeling; // method used to account for screen scattering
        Real64 TransSolBeam;             // Solar transmittance (beam to everything)
        Real64 TransThermal;             // Infrared radiation transmittance
        Real64 TransVisBeam;             // Visible transmittance (beam to everything)
        int BlindDataPtr;                // Pointer to window blind data
        int ScreenDataPtr;               // Pointer to window screen data
        int ScreenMapResolution;         // Resolution of azimuth and altitude angles to print in transmittance map
        // Complex fenestration parameters
        Real64 YoungModulus;       // Young's modulus (Pa) - used in window deflection calculations
        Real64 PoissonsRatio;      // Poisson's ratio - used in window deflection calculations
        Real64 DeflectedThickness; // Minimum gap thickness in deflected state (m).  Used with measured deflection
        Real64 Pressure;           // Window Gap pressure (Pa)
        int SupportPillarPtr;      // Pointer to support pillar data
        int DeflectionStatePtr;    // Pointer to deflection state
        int ComplexShadePtr;       // Pointer to complex shade data
        int GasPointer;            // Pointer to gas or gas mixture used in the gap
        // Window-shade thermal model parameters
        Real64 WinShadeToGlassDist;    // Distance between window shade and adjacent glass (m)
        Real64 WinShadeTopOpeningMult; // Area of air-flow opening at top of shade, expressed as a fraction
        //  of the shade-to-glass opening area at the top of the shade
        Real64 WinShadeBottomOpeningMult; // Area of air-flow opening at bottom of shade, expressed as a fraction
        //  of the shade-to-glass opening area at the bottom of the shade
        Real64 WinShadeLeftOpeningMult; // Area of air-flow opening at left side of shade, expressed as a fraction
        //  of the shade-to-glass opening area at the left side of the shade
        Real64 WinShadeRightOpeningMult; // Area of air-flow opening at right side of shade, expressed as a fraction
        //  of the shade-to-glass opening area at the right side of the shade
        Real64 WinShadeAirFlowPermeability; // The effective area of openings in the shade itself, expressed as a
        //  fraction of the shade area
        bool EMPDMaterialProps;      // True if EMPD properties have been assigned
        Real64 EMPDmu;               // Water Vapor Diffusion Resistance Factor (dimensionless)
        Real64 MoistACoeff;          // Moisture Equation Coefficient a
        Real64 MoistBCoeff;          // Moisture Equation Coefficient b
        Real64 MoistCCoeff;          // Moisture Equation Coefficient c
        Real64 MoistDCoeff;          // Moisture Equation Coefficient d
        Real64 EMPDSurfaceDepth;     // Surface-layer penetration depth (m)
        Real64 EMPDDeepDepth;        // Deep-layer penetration depth (m)
        Real64 EMPDCoatingThickness; // Coating Layer Thickness (m)
        Real64 EMPDmuCoating;        // Coating Layer water vapor diffusion resistance factor (dimensionless)
        // EcoRoof-Related properties, essentially for the plant layer,
        //    the soil layer uses the same resource as a regular material
        int EcoRoofCalculationMethod; // 1-Simple, 2-SchaapGenuchten
        Real64 HeightOfPlants;        // plants' height
        Real64 LAI;                   // LeafAreaIndex (Dimensionless???)
        Real64 Lreflectivity;         // LeafReflectivity
        Real64 LEmissitivity;         // LeafEmissivity
        Real64 InitMoisture;          // Initial soil moisture DJS
        Real64 MinMoisture;           // Minimum moisture allowed DJS
        Real64 RStomata;              // Minimum stomatal resistance DJS
        // HAMT
        int niso;                 // Number of data points
        Array1D<Real64> isodata;  // isotherm values
        Array1D<Real64> isorh;    // isotherm RH values
        int nsuc;                 // Number of data points
        Array1D<Real64> sucdata;  // suction values
        Array1D<Real64> sucwater; // suction water values
        int nred;                 // Number of data points
        Array1D<Real64> reddata;  // redistribution values
        Array1D<Real64> redwater; // redistribution water values
        int nmu;                  // Number of data points
        Array1D<Real64> mudata;   // mu values
        Array1D<Real64> murh;     // mu rh values
        int ntc;                  // Number of data points
        Array1D<Real64> tcdata;   // thermal conductivity values
        Array1D<Real64> tcwater;  // thermal conductivity water values
        Real64 itemp;             // initial Temperature
        Real64 irh;               // Initial RH
        Real64 iwater;            // Initial water content kg/kg
        int divs;                 // Number of divisions
        Real64 divsize;           // Average Cell Size
        int divmin;               // Minimum number of cells
        int divmax;               // Maximum number of cells
        // Added 12/22/2008 for thermochromic window glazing material
        Real64 SpecTemp; // Temperature corresponding to the specified material properties
        int TCParent;    // Reference to the parent object WindowMaterial:Glazing:Thermochromic
        // Simple Glazing System
        Real64 SimpleWindowUfactor;     // user input for simple window U-factor with film coeffs (W/m2-k)
        Real64 SimpleWindowSHGC;        // user input for simple window Solar Heat Gain Coefficient (non-dimensional)
        Real64 SimpleWindowVisTran;     // (optional) user input for simple window Visual Transmittance (non-dimensional)
        bool SimpleWindowVTinputByUser; // false means not input, true means user provide VT input
        bool WarnedForHighDiffusivity;  // used to limit error messaging to just the first instance
        // Equivalent Layer (ASHWAT) Model
        Real64 ReflFrontBeamBeam;    // Beam-Beam solar reflectance front at zero incident
        Real64 ReflBackBeamBeam;     // Beam-Beam solar reflectance back at zero incident
        Real64 TausFrontBeamBeam;    // Beam-Beam solar transmittance front at zero incident
        Real64 TausBackBeamBeam;     // Beam-Beam solar transmittance back at zero incident
        Real64 ReflFrontBeamBeamVis; // Beam-Beam visible reflectance front at zero incident
        Real64 ReflBackBeamBeamVis;  // Beam-Beam visible reflectance back at zero incident
        Real64 TausFrontBeamBeamVis; // Beam-Beam visible transmittance front at zero incident
        Real64 TausBackBeamBeamVis;  // Beam-Beam visible transmittance back at zero incident
        Real64 ReflFrontBeamDiff;    // Beam-Diffuse solar reflectance front at zero incident
        Real64 ReflBackBeamDiff;     // Beam-Diffuse solar reflectance back at zero incident
        Real64 TausFrontBeamDiff;    // Beam-Diffuse solar transmittance front at zero incident
        Real64 TausBackBeamDiff;     // Beam-Diffuse solar transmittance back at zero incident
        Real64 ReflFrontBeamDiffVis; // Beam-Diffuse visible reflectance front at zero incident
        Real64 ReflBackBeamDiffVis;  // Beam-Diffuse visible reflectance back at zero incident
        Real64 TausFrontBeamDiffVis; // Beam-Diffuse visible transmittance front at zero incident
        Real64 TausBackBeamDiffVis;  // Beam-Diffuse visible transmittance back at zero incident
        Real64 ReflFrontDiffDiff;    // Diffuse-Diffuse solar reflectance front
        Real64 ReflBackDiffDiff;     // Diffuse-Diffuse solar reflectance back
        Real64 TausDiffDiff;         // Diffuse-Diffuse solar transmittance (front and back)
        Real64 ReflFrontDiffDiffVis; // Diffuse-Diffuse visible reflectance front
        Real64 ReflBackDiffDiffVis;  // Diffuse-Diffuse visible reflectance back
        Real64 TausDiffDiffVis;      // Diffuse-Diffuse visible transmittance (front and back)
        Real64 EmissThermalFront;    // Front side thermal or infrared Emissivity
        Real64 EmissThermalBack;     // Back side thermal or infrared Emissivity
        Real64 TausThermal;          // Thermal transmittance (front and back)
        int GapVentType;             // Gap Ven type for equivalent Layer window model
        bool ISPleatedDrape;         // if pleated drape= true, if nonpleated drape = false
        Real64 PleatedDrapeWidth;    // width of the pleated drape fabric section
        Real64 PleatedDrapeLength;   // length of the pleated drape fabric section
        Real64 ScreenWireSpacing;    // insect screen wire spacing
        Real64 ScreenWireDiameter;   // insect screen wire diameter
        Real64 SlatWidth;            // slat width
        Real64 SlatSeparation;       // slat separation
        Real64 SlatCrown;            // slat crown
        Real64 SlatAngle;            // slat angle
        int SlatAngleType;           // slat angle control type, 0=fixed, 1=maximize solar, 2=block beam
        int SlatOrientation;         // horizontal or vertical
        std::string GasName;         // Name of gas type ("Air", "Argon", "Krypton", "Xenon")
        HysteresisPhaseChange::HysteresisPhaseChange *phaseChange = nullptr;
        bool GlassSpectralAndAngle;    // if SpectralAndAngle is an entered choice
        int GlassSpecAngTransDataPtr;  // Data set index of transmittance as a function of spectral and angle associated with a window glass material
        int GlassSpecAngFRefleDataPtr; // Data set index of front reflectance as a function of spectral and angle associated with a window glass
        // material
        int GlassSpecAngBRefleDataPtr; // Data set index of back reflectance as a function of spectral and angle associated with a window glass
        // material

        // Default Constructor
        MaterialProperties()
            : Group(-1), Roughness(0), Conductivity(0.0), Density(0.0), IsoMoistCap(0.0), Porosity(0.0), Resistance(0.0), ROnly(false), SpecHeat(0.0),
              ThermGradCoef(0.0), Thickness(0.0), VaporDiffus(0.0), GasType(5, 0), GlassSpectralDataPtr(0), NumberOfGasesInMixture(0),
              GasCon(3, 5, 0.0), GasVis(3, 5, 0.0), GasCp(3, 5, 0.0), GasWght(5, 0.0), GasSpecHeatRatio(5, 0.0), GasFract(5, 0.0), AbsorpSolar(0.0),
              AbsorpSolarInput(0.0), AbsorpSolarEMSOverrideOn(false), AbsorpSolarEMSOverride(0.0), AbsorpThermal(0.0), AbsorpThermalInput(0.0),
              AbsorpThermalEMSOverrideOn(false), AbsorpThermalEMSOverride(0.0), AbsorpVisible(0.0), AbsorpVisibleInput(0.0),
              AbsorpVisibleEMSOverrideOn(false), AbsorpVisibleEMSOverride(0.0), Trans(0.0), TransVis(0.0), GlassTransDirtFactor(1.0),
              SolarDiffusing(false), ReflectShade(0.0), ReflectShadeVis(0.0), AbsorpThermalBack(0.0), AbsorpThermalFront(0.0),
              ReflectSolBeamBack(0.0), ReflectSolBeamFront(0.0), ReflectSolDiffBack(0.0), ReflectSolDiffFront(0.0), ReflectVisBeamBack(0.0),
              ReflectVisBeamFront(0.0), ReflectVisDiffBack(0.0), ReflectVisDiffFront(0.0), TransSolBeam(0.0), TransThermal(0.0), TransVisBeam(0.0),
              BlindDataPtr(0), ScreenDataPtr(0), ScreenMapResolution(0), YoungModulus(0.0), PoissonsRatio(0.0), DeflectedThickness(0.0),
              Pressure(0.0), SupportPillarPtr(0), DeflectionStatePtr(0), ComplexShadePtr(0), GasPointer(0), WinShadeToGlassDist(0.0),
              WinShadeTopOpeningMult(0.0), WinShadeBottomOpeningMult(0.0), WinShadeLeftOpeningMult(0.0), WinShadeRightOpeningMult(0.0),
              WinShadeAirFlowPermeability(0.0), EMPDMaterialProps(false), EMPDmu(0.0), MoistACoeff(0.0), MoistBCoeff(0.0), MoistCCoeff(0.0),
              MoistDCoeff(0.0), EMPDSurfaceDepth(0.0), EMPDDeepDepth(0.0), EMPDCoatingThickness(0.0), EMPDmuCoating(0.0), EcoRoofCalculationMethod(0),
              HeightOfPlants(0.0), LAI(0.0), Lreflectivity(0.0), LEmissitivity(0.0), InitMoisture(0.0), MinMoisture(0.0), RStomata(0.0), niso(-1),
              isodata(27, 0.0), isorh(27, 0.0), nsuc(-1), sucdata(27, 0.0), sucwater(27, 0.0), nred(-1), reddata(27, 0.0), redwater(27, 0.0), nmu(-1),
              mudata(27, 0.0), murh(27, 0.0), ntc(-1), tcdata(27, 0.0), tcwater(27, 0.0), itemp(10.0), irh(0.5), iwater(0.2), divs(3), divsize(0.005),
              divmin(3), divmax(10), SpecTemp(0.0), TCParent(0), SimpleWindowUfactor(0.0), SimpleWindowSHGC(0.0), SimpleWindowVisTran(0.0),
              SimpleWindowVTinputByUser(false), WarnedForHighDiffusivity(false), ReflFrontBeamBeam(0.0), ReflBackBeamBeam(0.0),
              TausFrontBeamBeam(0.0), TausBackBeamBeam(0.0), ReflFrontBeamBeamVis(0.0), ReflBackBeamBeamVis(0.0), TausFrontBeamBeamVis(0.0),
              TausBackBeamBeamVis(0.0), ReflFrontBeamDiff(0.0), ReflBackBeamDiff(0.0), TausFrontBeamDiff(0.0), TausBackBeamDiff(0.0),
              ReflFrontBeamDiffVis(0.0), ReflBackBeamDiffVis(0.0), TausFrontBeamDiffVis(0.0), TausBackBeamDiffVis(0.0), ReflFrontDiffDiff(0.0),
              ReflBackDiffDiff(0.0), TausDiffDiff(0.0), ReflFrontDiffDiffVis(0.0), ReflBackDiffDiffVis(0.0), TausDiffDiffVis(0.0),
              EmissThermalFront(0.0), EmissThermalBack(0.0), TausThermal(0.0), GapVentType(0), ISPleatedDrape(false), PleatedDrapeWidth(0.0),
              PleatedDrapeLength(0.0), ScreenWireSpacing(0.0), ScreenWireDiameter(0.0), SlatWidth(0.0), SlatSeparation(0.0), SlatCrown(0.0),
              SlatAngle(0.0), SlatAngleType(0), SlatOrientation(0), GlassSpectralAndAngle(false), GlassSpecAngTransDataPtr(0),
              GlassSpecAngFRefleDataPtr(0), GlassSpecAngBRefleDataPtr(0)
        {
        }
    };

} // namespace Material

struct MaterialData : BaseGlobalStruct
{
    Array1D<Material::MaterialProperties> Material;

    void clear_state() override
    {
        Material.deallocate();
    }
};

} // namespace EnergyPlus

#endif