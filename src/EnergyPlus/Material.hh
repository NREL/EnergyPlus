// EnergyPlus, Copyright (c) 1996-2024, The Board of Trustees of the University of Illinois,
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
#include <EnergyPlus/DataWindowEquivalentLayer.hh>
#include <EnergyPlus/EPVector.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/PhaseChangeModeling/HysteresisModel.hh>
#include <EnergyPlus/TARCOGGassesParams.hh>
#include <EnergyPlus/TARCOGParams.hh>

namespace EnergyPlus {

namespace Material {

    constexpr int MaxSlatAngs(19);
    constexpr int MaxProfAngs(37);

    // Parameters to indicate material group type for use with the Material
    // derived type (see below):
    enum class Group
    {
        Invalid = -1,
        Regular,
        Air,
        Shade,
        WindowGlass,
        WindowGas,
        WindowBlind,
        WindowGasMixture,
        Screen,
        EcoRoof,
        IRTransparent,
        WindowSimpleGlazing,
        ComplexWindowShade,
        ComplexWindowGap,
        GlassEquivalentLayer,
        ShadeEquivalentLayer,
        DrapeEquivalentLayer,
        BlindEquivalentLayer,
        ScreenEquivalentLayer,
        GapEquivalentLayer,
        Num
    };

    enum class GasType
    {
        Invalid = -1,
        Custom,
        Air,
        Argon,
        Krypton,
        Xenon,
        Num
    };

#ifdef GET_OUT
    // Air       Argon     Krypton   Xenon
    // Gas conductivity coefficients for gases in a mixture
    extern const std::array<std::array<Real64, 10>, 3> GasCoeffsCon;
    // Gas viscosity coefficients for gases in a mixture
    extern const std::array<std::array<Real64, 10>, 3> GasCoeffsVis;
    // Gas specific heat coefficients for gases in a mixture
    extern const std::array<std::array<Real64, 10>, 3> GasCoeffsCp;
    extern const std::array<Real64, 10> GasWght;
    extern const std::array<Real64, 10> GasSpecificHeatRatio;
#endif // GET_OUT

    enum class GapVentType
    {
        Invalid = -1,
        Sealed,
        VentedIndoor,
        VentedOutdoor,
        Num
    };

    constexpr std::array<std::string_view, (int)GapVentType::Num> gapVentTypeNamesUC = {"SEALED", "VENTEDINDOOR", "VENTEDOUTDOOR"};

    extern const std::array<std::string_view, (int)GasType::Num> gasTypeNames;
    extern const std::array<std::string_view, (int)GapVentType::Num> gapVentTypeNames;

    enum class SlatAngleType
    {
        Invalid = -1,
        FixedSlatAngle,
        MaximizeSolar,
        BlockBeamSolar,
        Num
    };

    constexpr std::array<std::string_view, (int)SlatAngleType::Num> slatAngleTypeNamesUC = {"FIXEDSLATANGLE", "MAXIMIZESOLAR", "BLOCKBEAMSOLAR"};

    // Parameter for window screens beam reflectance accounting
    enum class ScreenBeamReflectanceModel
    {
        Invalid = -1,
        DoNotModel,
        DirectBeam,
        Diffuse,
        Num
    };

    constexpr std::array<std::string_view, (int)ScreenBeamReflectanceModel::Num> screenBeamReflectanceModelNamesUC = {
        "DONOTMODEL", "MODELASDIRECTBEAM", "MODELASDIFFUSE"};

    enum class VariableAbsCtrlSignal
    {
        Invalid = -1,
        SurfaceTemperature,
        SurfaceReceivedSolarRadiation,
        SpaceHeatingCoolingMode,
        Scheduled,
        Num
    };

    constexpr std::array<std::string_view, (int)VariableAbsCtrlSignal::Num> variableAbsCtrlSignalNamesUC = {
        "SURFACETEMPERATURE", "SURFACERECEIVEDSOLARRADIATION", "SPACEHEATINGCOOLINGMODE", "SCHEDULED"};

    // Parameters to indicate surface roughness for use with the Material
    // derived type:
    enum class SurfaceRoughness
    {
        Invalid = -1,
        VeryRough,
        Rough,
        MediumRough,
        MediumSmooth,
        Smooth,
        VerySmooth,
        Num
    };

    constexpr std::array<std::string_view, (int)SurfaceRoughness::Num> surfaceRoughnessNamesUC{
        "VERYROUGH", "ROUGH", "MEDIUMROUGH", "MEDIUMSMOOTH", "SMOOTH", "VERYSMOOTH"};

    extern const std::array<std::string_view, (int)SurfaceRoughness::Num> surfaceRoughnessNames;

    struct MaterialBase
    {
        // Members
        std::string Name = "";        // Name of material layer
        Group group = Group::Invalid; // Material group type (see Material Parameters above.  Currently
        // active: RegularMaterial, Shade, Air, WindowGlass,
        // WindowGas, WindowBlind, WindowGasMixture, Screen, EcoRoof,
        // IRTMaterial, WindowSimpleGlazing, ComplexWindowShade, ComplexWindowGap)
        SurfaceRoughness Roughness = SurfaceRoughness::Invalid; // Surface roughness index (See Surface Roughness parameters
        // above.  Current: VerySmooth, Smooth, MediumSmooth,
        // MediumRough, Rough, VeryRough)
        // Thermo-physical material properties
        Real64 Conductivity = 0.0; // Thermal conductivity of layer (W/m2K)
        Real64 Density = 0.0;      // Layer density (kg/m3)
        Real64 Resistance = 0.0;   // Layer thermal resistance (alternative to Density,
        // Conductivity, Thickness, and Specific Heat; K/W)
        bool ROnly = false;     // Material defined with "R" only
        Real64 SpecHeat = 0.0;  // Layer specific heat (J/kgK)
        Real64 Thickness = 0.0; // Layer thickness (m)

        Real64 AbsorpThermal = 0.0;      // Layer thermal absorptance
        Real64 AbsorpThermalInput = 0.0; // Layer thermal absorptance input by user
        Real64 AbsorpThermalBack = 0.0;  // Infrared radiation back absorption
        Real64 AbsorpThermalFront = 0.0; // Infrared radiation front absorption

        Real64 AbsorpSolar = 0.0;      // Layer solar absorptance
        Real64 AbsorpSolarInput = 0.0; // Layer solar absorptance input by user

        Real64 AbsorpVisible = 0.0;      // Layer Visible Absorptance
        Real64 AbsorpVisibleInput = 0.0; // Layer Visible Absorptance input by user

        Real64 Trans = 0.0;        // Transmittance of layer (glass, shade)
        Real64 TransVis = 0.0;     // Visible transmittance (at normal incidence)
        Real64 TransThermal = 0.0; // Infrared radiation transmittance

        Real64 ReflectSolBeamBack = 0.0;  // Solar back reflectance (beam to everything)
        Real64 ReflectSolBeamFront = 0.0; // Solar front reflectance (beam to everything)

        // TODO: these and others need to be moved to a child class
        // Simple Glazing System
        Real64 SimpleWindowUfactor = 0.0;       // user input for simple window U-factor with film coeffs (W/m2-k)
        Real64 SimpleWindowSHGC = 0.0;          // user input for simple window Solar Heat Gain Coefficient (non-dimensional)
        Real64 SimpleWindowVisTran = 0.0;       // (optional) user input for simple window Visual Transmittance (non-dimensional)
        bool SimpleWindowVTinputByUser = false; // false means not input, true means user provide VT input
        bool WarnedForHighDiffusivity = false;  // used to limit error messaging to just the first instance

        bool isUsed = false;

        // Moved these into the base class for SQLite purposes
        Real64 Porosity = 0.0;      // Layer porosity
        Real64 IsoMoistCap = 0.0;   // Isothermal moisture capacity on water vapor density (m3/kg)
        Real64 ThermGradCoef = 0.0; // Thermal-gradient coefficient for moisture capacity based on the water vapor density (kg/kgK)
        Real64 VaporDiffus = 0.0;   // Layer vapor diffusivity

        virtual bool dummy()
        {
            return true;
        } // Need at least one virtual function (vtable) for dynamic casting to work (duh)
        virtual ~MaterialBase() = default;
    };

    struct MaterialChild : public MaterialBase
    {
        int GlassSpectralDataPtr = 0; // Number of a spectral data set associated with a window glass material
        // Radiation parameters
        bool AbsorpSolarEMSOverrideOn = false;   // if true, then EMS calling to override value for solar absorptance
        Real64 AbsorpSolarEMSOverride = false;   // value to use when EMS calling to override value for solar absorptance
        bool AbsorpThermalEMSOverrideOn = false; // if true, then EMS calling to override value for thermal absorptance
        Real64 AbsorpThermalEMSOverride = 0.0;   // value to use when EMS calling to override value for thermal absorptance
        // dynamic thermal and solar absorptance coating parameters
        VariableAbsCtrlSignal absorpVarCtrlSignal = VariableAbsCtrlSignal::Invalid;
        int absorpThermalVarSchedIdx = 0;
        int absorpThermalVarFuncIdx = 0;
        int absorpSolarVarSchedIdx = 0;
        int absorpSolarVarFuncIdx = 0;
        bool AbsorpVisibleEMSOverrideOn = false; // if true, then EMS calling to override value for visible absorptance
        Real64 AbsorpVisibleEMSOverride = 0.0;   // value to use when EMS calling to override value for visible absorptance

        // Window-related radiation parameters
        Real64 GlassTransDirtFactor = 1.0; // Multiplier on glass transmittance due to dirt
        bool SolarDiffusing = false;       // True if glass diffuses beam solar radiation
        Real64 ReflectShade = 0.0;         // Shade or screen reflectance (interior shade only)
        Real64 ReflectShadeVis = 0.0;      // Shade reflectance for visible radiation
        Real64 ReflectSolDiffBack = 0.0;   // Solar back diffuse reflectance
        Real64 ReflectSolDiffFront = 0.0;  // Solar front diffuse reflectance
        Real64 ReflectVisBeamBack = 0.0;   // Visible back reflectance (beam to everything)
        Real64 ReflectVisBeamFront = 0.0;  // Visible front reflectance (beam to everything)
        Real64 ReflectVisDiffBack = 0.0;   // Visible back diffuse reflectance
        Real64 ReflectVisDiffFront = 0.0;  // Visible front diffuse reflectance
        Real64 TransSolBeam = 0.0;         // Solar transmittance (beam to everything)
        Real64 TransVisBeam = 0.0;         // Visible transmittance (beam to everything)
        int BlindDataPtr = 0;              // Pointer to window blind data
        // Complex fenestration parameters
        Real64 YoungModulus = 0.0;       // Young's modulus (Pa) - used in window deflection calculations
        Real64 PoissonsRatio = 0.0;      // Poisson's ratio - used in window deflection calculations
        Real64 DeflectedThickness = 0.0; // Minimum gap thickness in deflected state (m).  Used with measured deflection
        Real64 Pressure = 0.0;           // Window Gap pressure (Pa)
        int SupportPillarPtr = 0;        // Pointer to support pillar data
        int DeflectionStatePtr = 0;      // Pointer to deflection state
        int ComplexShadePtr = 0;         // Pointer to complex shade data
        int GasPointer = 0;              // Pointer to gas or gas mixture used in the gap
        // Window-shade thermal model parameters
        Real64 WinShadeToGlassDist = 0.0;    // Distance between window shade and adjacent glass (m)
        Real64 WinShadeTopOpeningMult = 0.0; // Area of air-flow opening at top of shade, expressed as a fraction
        //  of the shade-to-glass opening area at the top of the shade
        Real64 WinShadeBottomOpeningMult = 0.0; // Area of air-flow opening at bottom of shade, expressed as a fraction
        //  of the shade-to-glass opening area at the bottom of the shade
        Real64 WinShadeLeftOpeningMult = 0.0; // Area of air-flow opening at left side of shade, expressed as a fraction
        //  of the shade-to-glass opening area at the left side of the shade
        Real64 WinShadeRightOpeningMult = 0.0; // Area of air-flow opening at right side of shade, expressed as a fraction
        //  of the shade-to-glass opening area at the right side of the shade
        Real64 WinShadeAirFlowPermeability = 0.0; // The effective area of openings in the shade itself, expressed as a
        //  fraction of the shade area
        bool EMPDMaterialProps = false;    // True if EMPD properties have been assigned
        Real64 EMPDmu = 0.0;               // Water Vapor Diffusion Resistance Factor (dimensionless)
        Real64 MoistACoeff = 0.0;          // Moisture Equation Coefficient a
        Real64 MoistBCoeff = 0.0;          // Moisture Equation Coefficient b
        Real64 MoistCCoeff = 0.0;          // Moisture Equation Coefficient c
        Real64 MoistDCoeff = 0.0;          // Moisture Equation Coefficient d
        Real64 EMPDSurfaceDepth = 0.0;     // Surface-layer penetration depth (m)
        Real64 EMPDDeepDepth = 0.0;        // Deep-layer penetration depth (m)
        Real64 EMPDCoatingThickness = 0.0; // Coating Layer Thickness (m)
        Real64 EMPDmuCoating = 0.0;        // Coating Layer water vapor diffusion resistance factor (dimensionless)
        // EcoRoof-Related properties, essentially for the plant layer,
        //    the soil layer uses the same resource as a regular material
        int EcoRoofCalculationMethod = 0; // 1-Simple, 2-SchaapGenuchten
        Real64 HeightOfPlants = 0.0;      // plants' height
        Real64 LAI = 0.0;                 // LeafAreaIndex (Dimensionless???)
        Real64 Lreflectivity = 0.0;       // LeafReflectivity
        Real64 LEmissitivity = 0.0;       // LeafEmissivity
        Real64 InitMoisture = 0.0;        // Initial soil moisture DJS
        Real64 MinMoisture = 0.0;         // Minimum moisture allowed DJS
        Real64 RStomata = 0.0;            // Minimum stomatal resistance DJS
        // HAMT
        int niso = -1;                                       // Number of data points
        Array1D<Real64> isodata = Array1D<Real64>(27, 0.0);  // isotherm values
        Array1D<Real64> isorh = Array1D<Real64>(27, 0.0);    // isotherm RH values
        int nsuc = -1;                                       // Number of data points
        Array1D<Real64> sucdata = Array1D<Real64>(27, 0.0);  // suction values
        Array1D<Real64> sucwater = Array1D<Real64>(27, 0.0); // suction water values
        int nred = -1;                                       // Number of data points
        Array1D<Real64> reddata = Array1D<Real64>(27, 0.0);  // redistribution values
        Array1D<Real64> redwater = Array1D<Real64>(27, 0.0); // redistribution water values
        int nmu = -1;                                        // Number of data points
        Array1D<Real64> mudata = Array1D<Real64>(27, 0.0);   // mu values
        Array1D<Real64> murh = Array1D<Real64>(27, 0.0);     // mu rh values
        int ntc = -1;                                        // Number of data points
        Array1D<Real64> tcdata = Array1D<Real64>(27, 0.0);   // thermal conductivity values
        Array1D<Real64> tcwater = Array1D<Real64>(27, 0.0);  // thermal conductivity water values
        Real64 itemp = 10.0;                                 // initial Temperature
        Real64 irh = 0.5;                                    // Initial RH
        Real64 iwater = 0.2;                                 // Initial water content kg/kg
        int divs = 3;                                        // Number of divisions
        Real64 divsize = 0.005;                              // Average Cell Size
        int divmin = 3;                                      // Minimum number of cells
        int divmax = 10;                                     // Maximum number of cells
        // Added 12/22/2008 for thermochromic window glazing material
        Real64 SpecTemp = 0.0; // Temperature corresponding to the specified material properties
        int TCParent = 0;      // Reference to the parent object WindowMaterial:Glazing:Thermochromic
        // Equivalent Layer (ASHWAT) Model
        Real64 ScreenWireSpacing = 0.0;  // insect screen wire spacing
        Real64 ScreenWireDiameter = 0.0; // insect screen wire diameter

        Real64 ReflFrontBeamBeam = 0.0;    // Beam-Beam solar reflectance front at zero incident
        Real64 ReflBackBeamBeam = 0.0;     // Beam-Beam solar reflectance back at zero incident
        Real64 TausFrontBeamBeam = 0.0;    // Beam-Beam solar transmittance front at zero incident
        Real64 TausBackBeamBeam = 0.0;     // Beam-Beam solar transmittance back at zero incident
        Real64 ReflFrontBeamBeamVis = 0.0; // Beam-Beam visible reflectance front at zero incident
        Real64 ReflBackBeamBeamVis = 0.0;  // Beam-Beam visible reflectance back at zero incident
        Real64 TausFrontBeamBeamVis = 0.0; // Beam-Beam visible transmittance front at zero incident
        Real64 TausBackBeamBeamVis = 0.0;  // Beam-Beam visible transmittance back at zero incident
        Real64 ReflFrontBeamDiff = 0.0;    // Beam-Diffuse solar reflectance front at zero incident
        Real64 ReflBackBeamDiff = 0.0;     // Beam-Diffuse solar reflectance back at zero incident
        Real64 TausFrontBeamDiff = 0.0;    // Beam-Diffuse solar transmittance front at zero incident
        Real64 TausBackBeamDiff = 0.0;     // Beam-Diffuse solar transmittance back at zero incident
        Real64 ReflFrontBeamDiffVis = 0.0; // Beam-Diffuse visible reflectance front at zero incident
        Real64 ReflBackBeamDiffVis = 0.0;  // Beam-Diffuse visible reflectance back at zero incident
        Real64 TausFrontBeamDiffVis = 0.0; // Beam-Diffuse visible transmittance front at zero incident
        Real64 TausBackBeamDiffVis = 0.0;  // Beam-Diffuse visible transmittance back at zero incident

        Real64 ReflFrontDiffDiff = 0.0;    // Diffuse-Diffuse solar reflectance front
        Real64 ReflBackDiffDiff = 0.0;     // Diffuse-Diffuse solar reflectance back
        Real64 TausDiffDiff = 0.0;         // Diffuse-Diffuse solar transmittance (front and back)
        Real64 ReflFrontDiffDiffVis = 0.0; // Diffuse-Diffuse visible reflectance front
        Real64 ReflBackDiffDiffVis = 0.0;  // Diffuse-Diffuse visible reflectance back
        Real64 TausDiffDiffVis = 0.0;      // Diffuse-Diffuse visible transmittance (front and back)
        Real64 EmissThermalFront = 0.0;    // Front side thermal or infrared Emissivity
        Real64 EmissThermalBack = 0.0;     // Back side thermal or infrared Emissivity
        Real64 TausThermal = 0.0;          // Thermal transmittance (front and back)

        bool ISPleatedDrape = false;                                 // if pleated drape= true, if nonpleated drape = false
        Real64 PleatedDrapeWidth = 0.0;                              // width of the pleated drape fabric section
        Real64 PleatedDrapeLength = 0.0;                             // length of the pleated drape fabric section
        Real64 SlatWidth = 0.0;                                      // slat width
        Real64 SlatSeparation = 0.0;                                 // slat separation
        Real64 SlatCrown = 0.0;                                      // slat crown
        Real64 SlatAngle = 0.0;                                      // slat angle
        SlatAngleType slatAngleType = SlatAngleType::FixedSlatAngle; // slat angle control type, 0=fixed, 1=maximize solar, 2=block beam
        DataWindowEquivalentLayer::Orientation SlatOrientation = DataWindowEquivalentLayer::Orientation::Invalid; // horizontal or vertical
        HysteresisPhaseChange::HysteresisPhaseChange *phaseChange = nullptr;
        bool GlassSpectralAndAngle = false; // if SpectralAndAngle is an entered choice
        int GlassSpecAngTransDataPtr =
            0; // Data set index of transmittance as a function of spectral and angle associated with a window glass material
        int GlassSpecAngFRefleDataPtr = 0; // Data set index of front reflectance as a function of spectral and angle associated with a window glass
        // material
        int GlassSpecAngBRefleDataPtr = 0; // Data set index of back reflectance as a function of spectral and angle associated with a window glass
        // material

        virtual bool dummy()
        {
            return true;
        }
        virtual ~MaterialChild() = default;
    };

    struct WindowBlindProperties
    {
        // Members
        std::string Name;
        int MaterialNumber = 0; // Material pointer for the blind
        // Input properties
        DataWindowEquivalentLayer::Orientation SlatOrientation = DataWindowEquivalentLayer::Orientation::Invalid; // HORIZONTAL or VERTICAL
        DataWindowEquivalentLayer::AngleType SlatAngleType = DataWindowEquivalentLayer::AngleType::Fixed;         // FIXED or VARIABLE
        Real64 SlatWidth = 0.0;                                                                                   // Slat width (m)
        Real64 SlatSeparation = 0.0;                                                                              // Slat separation (m)
        Real64 SlatThickness = 0.0;                                                                               // Slat thickness (m)
        Real64 SlatCrown = 0.0;        // the height of the slate (length from the chord to the curve)
        Real64 SlatAngle = 0.0;        // Slat angle (deg)
        Real64 MinSlatAngle = 0.0;     // Minimum slat angle for variable-angle slats (deg) (user input)
        Real64 MaxSlatAngle = 0.0;     // Maximum slat angle for variable-angle slats (deg) (user input)
        Real64 SlatConductivity = 0.0; // Slat conductivity (W/m-K)
        // Solar slat properties
        Real64 SlatTransSolBeamDiff = 0.0;     // Slat solar beam-diffuse transmittance
        Real64 SlatFrontReflSolBeamDiff = 0.0; // Slat front solar beam-diffuse reflectance
        Real64 SlatBackReflSolBeamDiff = 0.0;  // Slat back solar beam-diffuse reflectance
        Real64 SlatTransSolDiffDiff = 0.0;     // Slat solar diffuse-diffuse transmittance
        Real64 SlatFrontReflSolDiffDiff = 0.0; // Slat front solar diffuse-diffuse reflectance
        Real64 SlatBackReflSolDiffDiff = 0.0;  // Slat back solar diffuse-diffuse reflectance
        // Visible slat properties
        Real64 SlatTransVisBeamDiff = 0.0;     // Slat visible beam-diffuse transmittance
        Real64 SlatFrontReflVisBeamDiff = 0.0; // Slat front visible beam-diffuse reflectance
        Real64 SlatBackReflVisBeamDiff = 0.0;  // Slat back visible beam-diffuse reflectance
        Real64 SlatTransVisDiffDiff = 0.0;     // Slat visible diffuse-diffuse transmittance
        Real64 SlatFrontReflVisDiffDiff = 0.0; // Slat front visible diffuse-diffuse reflectance
        Real64 SlatBackReflVisDiffDiff = 0.0;  // Slat back visible diffuse-diffuse reflectance
        // Long-wave (IR) slat properties
        Real64 SlatTransIR = 0.0;      // Slat IR transmittance
        Real64 SlatFrontEmissIR = 0.0; // Slat front emissivity
        Real64 SlatBackEmissIR = 0.0;  // Slat back emissivity
        // Some characteristics for blind thermal calculation
        Real64 BlindToGlassDist = 0.0;    // Distance between window shade and adjacent glass (m)
        Real64 BlindTopOpeningMult = 0.0; // Area of air-flow opening at top of blind, expressed as a fraction
        //  of the blind-to-glass opening area at the top of the blind
        Real64 BlindBottomOpeningMult = 0.0; // Area of air-flow opening at bottom of blind, expressed as a fraction
        //  of the blind-to-glass opening area at the bottom of the blind
        Real64 BlindLeftOpeningMult = 0.0; // Area of air-flow opening at left side of blind, expressed as a fraction
        //  of the blind-to-glass opening area at the left side of the blind
        Real64 BlindRightOpeningMult = 0.0; // Area of air-flow opening at right side of blind, expressed as a fraction
        //  of the blind-to-glass opening area at the right side of the blind
        // Calculated blind properties
        // Blind solar properties
        Array2D<Real64> SolFrontBeamBeamTrans; // Blind solar front beam-beam transmittance vs.
        // profile angle, slat angle
        Array2D<Real64> SolFrontBeamBeamRefl; // Blind solar front beam-beam reflectance vs. profile angle,
        // slat angle (zero)
        Array2D<Real64> SolBackBeamBeamTrans; // Blind solar back beam-beam transmittance vs. profile angle,
        // slat angle
        Array2D<Real64> SolBackBeamBeamRefl; // Blind solar back beam-beam reflectance vs. profile angle,
        // slat angle (zero)
        Array2D<Real64> SolFrontBeamDiffTrans; // Blind solar front beam-diffuse transmittance
        // vs. profile angle, slat angle
        Array2D<Real64> SolFrontBeamDiffRefl; // Blind solar front beam-diffuse reflectance
        // vs. profile angle, slat angle
        Array2D<Real64> SolBackBeamDiffTrans; // Blind solar back beam-diffuse transmittance
        // vs. profile angle, slat angle
        Array2D<Real64> SolBackBeamDiffRefl; // Blind solar back beam-diffuse reflectance
        // vs. profile angle, slat angle
        Array1D<Real64> SolFrontDiffDiffTrans; // Blind solar front diffuse-diffuse transmittance
        // vs. slat angle
        Array1D<Real64> SolFrontDiffDiffTransGnd; // Blind ground solar front diffuse-diffuse transmittance
        // vs. slat angle
        Array1D<Real64> SolFrontDiffDiffTransSky; // Blind sky solar front diffuse-diffuse transmittance
        // vs. slat angle
        Array1D<Real64> SolFrontDiffDiffRefl; // Blind solar front diffuse-diffuse reflectance
        // vs. slat angle
        Array1D<Real64> SolFrontDiffDiffReflGnd; // Blind ground solar front diffuse-diffuse reflectance
        // vs. slat angle
        Array1D<Real64> SolFrontDiffDiffReflSky; // Blind sky solar front diffuse-diffuse reflectance
        // vs. slat angle
        Array1D<Real64> SolBackDiffDiffTrans; // Blind solar back diffuse-diffuse transmittance
        // vs. slat angle
        Array1D<Real64> SolBackDiffDiffRefl; // Blind solar back diffuse-diffuse reflectance
        // vs. slat angle
        Array2D<Real64> SolFrontBeamAbs;    // Blind solar front beam absorptance vs. slat angle
        Array2D<Real64> SolBackBeamAbs;     // Blind solar back beam absorptance vs. slat angle
        Array1D<Real64> SolFrontDiffAbs;    // Blind solar front diffuse absorptance vs. slat angle
        Array1D<Real64> SolFrontDiffAbsGnd; // Blind ground solar front diffuse absorptance vs. slat angle
        Array1D<Real64> SolFrontDiffAbsSky; // Blind sky solar front diffuse absorptance vs. slat angle
        Array1D<Real64> SolBackDiffAbs;     // Blind solar back diffuse absorptance vs. slat angle
        // Blind visible properties
        Array2D<Real64> VisFrontBeamBeamTrans; // Blind visible front beam-beam transmittance
        // vs. profile angle, slat angle
        Array2D<Real64> VisFrontBeamBeamRefl; // Blind visible front beam-beam reflectance
        // vs. profile angle, slat angle (zero)
        Array2D<Real64> VisBackBeamBeamTrans; // Blind visible back beam-beam transmittance
        // vs. profile angle, slat angle
        Array2D<Real64> VisBackBeamBeamRefl; // Blind visible back beam-beam reflectance
        // vs. profile angle, slat angle (zero)
        Array2D<Real64> VisFrontBeamDiffTrans; // Blind visible front beam-diffuse transmittance
        // vs. profile angle, slat angle
        Array2D<Real64> VisFrontBeamDiffRefl; // Blind visible front beam-diffuse reflectance
        // vs. profile angle, slat angle
        Array2D<Real64> VisBackBeamDiffTrans; // Blind visible back beam-diffuse transmittance
        // vs. profile angle, slat angle
        Array2D<Real64> VisBackBeamDiffRefl; // Blind visible back beam-diffuse reflectance
        // vs. profile angle, slat angle
        Array1D<Real64> VisFrontDiffDiffTrans; // Blind visible front diffuse-diffuse transmittance
        // vs. slat angle
        Array1D<Real64> VisFrontDiffDiffRefl; // Blind visible front diffuse-diffuse reflectance
        // vs. slat angle
        Array1D<Real64> VisBackDiffDiffTrans; // Blind visible back diffuse-diffuse transmittance
        // vs. slat angle
        Array1D<Real64> VisBackDiffDiffRefl; // Blind visible back diffuse-diffuse reflectance
        // vs. slat angle
        // Long-wave (IR) blind properties
        Array1D<Real64> IRFrontTrans; // Blind IR front transmittance vs. slat angle
        Array1D<Real64> IRFrontEmiss; // Blind IR front emissivity vs. slat angle
        Array1D<Real64> IRBackTrans;  // Blind IR back transmittance vs. slat angle
        Array1D<Real64> IRBackEmiss;  // Blind IR back emissivity vs. slat angle

        // Default Constructor
        WindowBlindProperties()
            : SolFrontBeamBeamTrans(MaxSlatAngs, MaxProfAngs, 0.0), SolFrontBeamBeamRefl(MaxSlatAngs, MaxProfAngs, 0.0),
              SolBackBeamBeamTrans(MaxSlatAngs, MaxProfAngs, 0.0), SolBackBeamBeamRefl(MaxSlatAngs, MaxProfAngs, 0.0),
              SolFrontBeamDiffTrans(MaxSlatAngs, MaxProfAngs, 0.0), SolFrontBeamDiffRefl(MaxSlatAngs, MaxProfAngs, 0.0),
              SolBackBeamDiffTrans(MaxSlatAngs, MaxProfAngs, 0.0), SolBackBeamDiffRefl(MaxSlatAngs, MaxProfAngs, 0.0),
              SolFrontDiffDiffTrans(MaxSlatAngs, 0.0), SolFrontDiffDiffTransGnd(MaxSlatAngs, 0.0), SolFrontDiffDiffTransSky(MaxSlatAngs, 0.0),
              SolFrontDiffDiffRefl(MaxSlatAngs, 0.0), SolFrontDiffDiffReflGnd(MaxSlatAngs, 0.0), SolFrontDiffDiffReflSky(MaxSlatAngs, 0.0),
              SolBackDiffDiffTrans(MaxSlatAngs, 0.0), SolBackDiffDiffRefl(MaxSlatAngs, 0.0), SolFrontBeamAbs(MaxSlatAngs, MaxProfAngs, 0.0),
              SolBackBeamAbs(MaxSlatAngs, MaxProfAngs, 0.0), SolFrontDiffAbs(MaxSlatAngs, 0.0), SolFrontDiffAbsGnd(MaxSlatAngs, 0.0),
              SolFrontDiffAbsSky(MaxSlatAngs, 0.0), SolBackDiffAbs(MaxSlatAngs, 0.0), VisFrontBeamBeamTrans(MaxSlatAngs, MaxProfAngs, 0.0),
              VisFrontBeamBeamRefl(MaxSlatAngs, MaxProfAngs, 0.0), VisBackBeamBeamTrans(MaxSlatAngs, MaxProfAngs, 0.0),
              VisBackBeamBeamRefl(MaxSlatAngs, MaxProfAngs, 0.0), VisFrontBeamDiffTrans(MaxSlatAngs, MaxProfAngs, 0.0),
              VisFrontBeamDiffRefl(MaxSlatAngs, MaxProfAngs, 0.0), VisBackBeamDiffTrans(MaxSlatAngs, MaxProfAngs, 0.0),
              VisBackBeamDiffRefl(MaxSlatAngs, MaxProfAngs, 0.0), VisFrontDiffDiffTrans(MaxSlatAngs, 0.0), VisFrontDiffDiffRefl(MaxSlatAngs, 0.0),
              VisBackDiffDiffTrans(MaxSlatAngs, 0.0), VisBackDiffDiffRefl(MaxSlatAngs, 0.0), IRFrontTrans(MaxSlatAngs, 0.0),
              IRFrontEmiss(MaxSlatAngs, 0.0), IRBackTrans(MaxSlatAngs, 0.0), IRBackEmiss(MaxSlatAngs, 0.0)
        {
        }
    };

    struct WindowComplexShade
    {
        // Members
        std::string Name; // Name for complex shade
        TARCOGParams::TARCOGLayerType LayerType =
            TARCOGParams::TARCOGLayerType::Invalid; // Layer type (OtherShadingType, Venetian, Woven, Perforated)
        Real64 Thickness = 0.0;                     // Layer thickness (m)
        Real64 Conductivity = 0.0;                  // Layer conductivity (W/m2K)
        Real64 IRTransmittance = 0.0;               // IR Transmittance
        Real64 FrontEmissivity = 0.0;               // Emissivity of front surface
        Real64 BackEmissivity = 0.0;                // Emissivity of back surface
        Real64 TopOpeningMultiplier = 0.0;          // Coverage percent for top opening (%)
        Real64 BottomOpeningMultiplier = 0.0;       // Coverage percent for bottom opening (%)
        Real64 LeftOpeningMultiplier = 0.0;         // Coverage percent for left opening (%)
        Real64 RightOpeningMultiplier = 0.0;        // Coverage percent for right opening (%)
        Real64 FrontOpeningMultiplier = 0.0;        // Coverage percent for front opening (%)
        Real64 SlatWidth = 0.0;                     // Slat width (m)
        Real64 SlatSpacing = 0.0;                   // Slat spacing (m)
        Real64 SlatThickness = 0.0;                 // Slat thickness (m)
        Real64 SlatAngle = 0.0;                     // Slat angle (deg)
        Real64 SlatConductivity = 0.0;              // Slat conductivity (W/m2K)
        Real64 SlatCurve = 0.0;                     // Curvature radius of slat (if =0 then flat) (m)
    };

    struct WindowThermalModelParams
    {
        // Members
        std::string Name;                                                                                   // Window thermal model name
        TARCOGGassesParams::Stdrd CalculationStandard = TARCOGGassesParams::Stdrd::Invalid;                 // Tarcog calculation standard
        TARCOGParams::TARCOGThermalModel ThermalModel = TARCOGParams::TARCOGThermalModel::Invalid;          // Tarcog thermal model
        Real64 SDScalar = 0.0;                                                                              // SDScalar coefficient
        TARCOGParams::DeflectionCalculation DeflectionModel = TARCOGParams::DeflectionCalculation::Invalid; // Deflection model
        Real64 VacuumPressureLimit = 0.0; // Pressure limit at which it will be considered vacuum gas state
        Real64 InitialTemperature = 0.0;  // Window(s) temperature in time of fabrication
        Real64 InitialPressure = 0.0;     // Window(s) pressure in time of fabrication
    };

    int constexpr maxMixGases = 5;

    struct GasCoeffs
    {
        Real64 c0 = 0.0;
        Real64 c1 = 0.0;
        Real64 c2 = 0.0;
    };

    struct Gas
    {
        GasType type = GasType::Custom;
        GasCoeffs con = GasCoeffs();
        GasCoeffs vis = GasCoeffs();
        GasCoeffs cp = GasCoeffs();
        Real64 wght = 0.0;
        Real64 specHeatRatio = 0.0;
    };

    extern const std::array<Gas, 10> gases;

    struct MaterialGasMix : public MaterialBase
    {
        //  up to 5 gases in a mixture [Window gas only].  It is defined as parameter (GasCoefs)
        int numGases = 0; // Number of gases in a window gas mixture

        std::array<Real64, maxMixGases> gasFracts = {0.0};
        std::array<Gas, maxMixGases> gases = {Gas()};

        GapVentType gapVentType = GapVentType::Sealed; // Gap Ven type for equivalent Layer window model

        virtual bool dummy()
        {
            return true;
        }

        MaterialGasMix() : MaterialBase()
        {
            group = Group::WindowGas;
        }
    };

    struct ScreenBmTransAbsRef
    {
        Real64 BmTrans = 0.0; // Beam solar transmittance (dependent on sun angle)
        // (this value can include scattering if the user so chooses)
        Real64 BmTransBack = 0.0; // Beam solar transmittance (dependent on sun angle) from back side of screen
        Real64 BmTransVis = 0.0;  // Visible solar transmittance (dependent on sun angle)
        // (this value can include visible scattering if the user so chooses)
        Real64 DfTrans = 0.0;     // Beam solar transmitted as diffuse radiation (dependent on sun angle)
        Real64 DfTransBack = 0.0; // Beam solar transmitted as diffuse radiation (dependent on sun angle) from back side
        Real64 DfTransVis = 0.0;  // Visible solar transmitted as diffuse radiation (dependent on sun angle)

        // The following reflectance properties are dependent on sun angle:
        Real64 RefSolFront = 0.0; // Beam solar reflected as diffuse radiation when sun is in front of screen
        Real64 RefVisFront = 0.0; // Visible solar reflected as diffuse radiation when sun is in front of screen
        Real64 RefSolBack = 0.0;  // Beam solar reflected as diffuse radiation when sun is in back of screen
        Real64 RefVisBack = 0.0;  // Visible solar reflected as diffuse radiation when sun is in back of screen
        Real64 AbsSolFront = 0.0; // Front surface solar beam absorptance
        Real64 AbsSolBack = 0.0;  // Back surface solar beam absorptance
    };

#define PRECALC_INTERP_SCREEN

    constexpr int minDegResolution = 5;

    constexpr int maxIPhi = (Constant::Pi * Constant::RadToDeg / minDegResolution) + 1;
    constexpr int maxITheta = (Constant::Pi * Constant::RadToDeg / minDegResolution) + 1;

    struct MaterialScreen : public MaterialBase
    {
        Real64 diameterToSpacingRatio = 0.0; // ratio of screen material diameter to screen material spacing

        ScreenBeamReflectanceModel bmRefModel = ScreenBeamReflectanceModel::Invalid; // user specified method of accounting for scattered solar beam

        Real64 DfTrans = 0.0;    // Back surface diffuse solar transmitted
        Real64 DfTransVis = 0.0; // Back surface diffuse visible solar transmitted
        Real64 DfRef = 0.0;      // Back reflection of solar diffuse radiation
        Real64 DfRefVis = 0.0;   // Back reflection of visible diffuse radiation
        Real64 DfAbs = 0.0;      // Absorption of diffuse radiation

        Real64 ShadeRef = 0.0;       // Screen assembly solar reflectance (user input adjusted for holes in screen)
        Real64 ShadeRefVis = 0.0;    // Screen assembly visible reflectance (user input adjusted for holes in screen)
        Real64 CylinderRef = 0.0;    // Screen material solar reflectance (user input, does not account for holes in screen)
        Real64 CylinderRefVis = 0.0; // Screen material visible reflectance (user input, does not account for holes in screen)

        int mapDegResolution = 0;                                      // Resolution of azimuth and altitude angles to print in transmittance map
        Real64 dPhi = (Real64)minDegResolution * Constant::DegToRad;   // phi increments (rad)
        Real64 dTheta = (Real64)minDegResolution * Constant::DegToRad; // theta increments (rad)

        std::array<std::array<ScreenBmTransAbsRef, maxITheta>, maxIPhi> btars;

        // These are shared with window shade and blind
        Real64 toGlassDist = 0.0;    // Distance between window shade and adjacent glass (m)
        Real64 topOpeningMult = 0.0; // Area of air-flow opening at top of shade, expressed as a fraction
        //  of the shade-to-glass opening area at the top of the shade
        Real64 bottomOpeningMult = 0.0; // Area of air-flow opening at bottom of shade, expressed as a fraction
        //  of the shade-to-glass opening area at the bottom of the shade
        Real64 leftOpeningMult = 0.0; // Area of air-flow opening at left side of shade, expressed as a fraction
        //  of the shade-to-glass opening area at the left side of the shade
        Real64 rightOpeningMult = 0.0; // Area of air-flow opening at right side of shade, expressed as a fraction
        //  of the shade-to-glass opening area at the right side of the shade
        Real64 airFlowPermeability = 0.0; // The effective area of openings in the shade itself, expressed as a
        //  fraction of the shade area

        virtual bool dummy()
        {
            return true;
        }

        MaterialScreen() : MaterialBase()
        {
            group = Group::Screen;
        }
    };

    void GetMaterialData(EnergyPlusData &state, bool &errorsFound); // set to true if errors found in input
    void GetVariableAbsorptanceInput(EnergyPlusData &state, bool &errorsFound);

    // Angles must be in radians
    void GetRelativePhiTheta(Real64 phiWin, Real64 thetaWin, Vector3<Real64> const &solcos, Real64 &phi, Real64 &theta);
    void NormalizePhiTheta(Real64 &phi, Real64 &theta);
    void GetPhiThetaIndices(Real64 phi, Real64 theta, Real64 dPhi, Real64 dTheta, int &iPhi1, int &iPhi2, int &iTheta1, int &iTheta2);

    void CalcScreenTransmittance(EnergyPlusData &state,
                                 MaterialScreen const *screen,
                                 Real64 phi,   // Sun altitude relative to surface outward normal (rad)
                                 Real64 theta, // Sun azimuth relative to surface outward normal (rad)
                                 ScreenBmTransAbsRef &tar);

} // namespace Material

struct MaterialData : BaseGlobalStruct
{
    EPVector<Material::MaterialBase *> Material;
    int TotMaterials = 0;     // Total number of unique materials (layers) in this simulation
    int TotComplexShades = 0; // Total number of shading materials for complex fenestrations

    EPVector<Material::WindowBlindProperties> Blind;
    EPVector<Material::WindowComplexShade> ComplexShade;
    EPVector<Material::WindowThermalModelParams> WindowThermalModel;

    void init_state([[maybe_unused]] EnergyPlusData &state) override
    {
    }

    void clear_state() override
    {
        for (int i = 0; i < TotMaterials; ++i) {
            delete Material[i]; //
        }
        Material.deallocate();
    }
};

} // namespace EnergyPlus

#endif
