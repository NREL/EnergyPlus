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

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/Material.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus::Material {

constexpr std::array<std::string_view, (int)GapVentType::Num> gapVentTypeNames = {"Sealed", "VentedIndoor", "VentedOutdoor"};
constexpr std::array<std::string_view, (int)GasType::Num> gasTypeNames = {"Custom", "Air", "Argon", "Krypton", "Xenon"};
constexpr std::array<std::string_view, (int)GasType::Num> gasTypeNamesUC = {"CUSTOM", "AIR", "ARGON", "KRYPTON", "XENON"};
constexpr std::array<std::string_view, (int)SurfaceRoughness::Num> surfaceRoughnessNames = {
    "VeryRough", "Rough", "MediumRough", "MediumSmooth", "Smooth", "VerySmooth"};
        
constexpr std::array<Material::Gas, 10> gases = {
    Gas(), // Empty
    {GasType::Air, {2.873e-3, 7.760e-5, 0.0}, {3.723e-6, 4.940e-8, 0.0}, {1002.737, 1.2324e-2, 0.0}, 28.97, 1.4}, // Air
    {GasType::Argon, {2.285e-3, 5.149e-5, 0.0}, {3.379e-6, 6.451e-8, 0.0}, {521.929, 0.0, 0.0}, 39.948, 1.67}, // Argon
    {GasType::Krypton, {9.443e-4, 2.826e-5, 0.0}, {2.213e-6, 7.777e-8, 0.0}, {248.091, 0.0, 0.0}, 83.8, 1.68}, // Krypton
    {GasType::Xenon, {4.538e-4, 1.723e-5, 0.0}, {1.069e-6, 7.414e-8, 0.0}, {158.340, 0.0, 0.0}, 131.3, 1.66}, // Xenon
    Gas(), // Empty
    Gas(), // Empty
    Gas(), // Empty
    Gas(), // Empty
    Gas() // Empty
};
        
void GetMaterialData(EnergyPlusData &state, bool &ErrorsFound) // set to true if errors found in input
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Liesen
    //       DATE WRITTEN   September 1997
    //       MODIFIED       April 1999; L.Lawrie
    //                      Sept 1999, FCW, Window5 modifications
    //                      Mar 2001, FCW, WindowShade mods
    //                      Sep 2001, FCW, add Material:WindowGasMixture
    //                      Oct 2001, FCW, add Material:WindowBlind
    //                      Dec 2003, FCW, add glass solar/visible transmittance dirt factor
    //                      Feb 2009, TH, added WindowMaterial:GlazingGroup:Thermochromic

    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // The purpose of this subroutine is to serve as a transfer agent
    // between the input file and the material derived type.  The new input
    // file is working, and this file reads the material data directly
    // from the input file and transfer that information to the new data
    // structure.  Data read in this routine is stored in a
    // derived type (Material) defined in the DataHeatBalance module.

    // In April 1999, a new set of material definitions replaced the one "all-purpose"
    // material definition.  There are now 10 flavors of materials.  Definitions from
    // the IDD appear below before their counterpart "gets".

    using Curve::GetCurveIndex;
    using Curve::GetCurveMinMaxValues;

    using General::ScanForReports;

    // if this has a size, then input has already been gotten
    if (state.dataHeatBalMgr->UniqueMaterialNames.size()) {
        return;
    }

    int IOStat;                        // IO Status when calling get input subroutine
    Array1D_string MaterialNames(7);   // Number of Material Alpha names defined
    int MaterNum;                      // Counter to keep track of the material number
    int MaterialNumAlpha;              // Number of material alpha names being passed
    int MaterialNumProp;               // Number of material properties being passed
    Array1D<Real64> MaterialProps(27); // Temporary array to transfer material properties
    int RegMat;                        // Regular Materials -- full property definition
    int RegRMat;                       // Regular Materials -- R only property definition
    int AirMat;                        // Air space materials in opaque constructions
    int IRTMat;                        // Infrared Transmitting Materials -- R only property definition

    int EcoRoofMat;                     // Materials for ecoRoof
    int NumGas;                         // Index for loop over gap gases in a mixture
    int NumGases;                       // Number of gasses in a mixture
    GasType gasType = GasType::Invalid; // Gas type index: 1=air, 2=argon, 3=krypton, 4=xenon
    int Loop;
    int ICoeff;            // Gas property coefficient index
    Real64 MinSlatAngGeom; // Minimum and maximum slat angle allowed by slat geometry (deg)
    Real64 MaxSlatAngGeom;
    Real64 ReflectivitySol;   // Glass reflectivity, solar
    Real64 ReflectivityVis;   // Glass reflectivity, visible
    Real64 TransmittivitySol; // Glass transmittivity, solar
    Real64 TransmittivityVis; // Glass transmittivity, visible
    Real64 DenomRGas;         // Denominator for WindowGas calculations of NominalR
    Real64 Openness;          // insect screen openness fraction = (1-d/s)^2
    Real64 minAngValue;       // minimum value of angle
    Real64 maxAngValue;       // maximum value of angle
    Real64 minLamValue;       // minimum value of wavelength
    Real64 maxLamValue;       // maximum value of wavelength

    // Added TH 1/9/2009 to read the thermochromic glazings
    int iTC(0);
    int iMat(0);

    // Added TH 7/27/2009 for constructions defined with F or C factor method
    int TotFfactorConstructs; // Number of slabs-on-grade or underground floor constructions defined with F factors
    int TotCfactorConstructs; // Number of underground wall constructions defined with C factors

    static constexpr std::string_view RoutineName("GetMaterialData: ");

    auto &ip = state.dataInputProcessing->inputProcessor;
    auto &ipsc = state.dataIPShortCut;
    
    RegMat = ip->getNumObjectsFound(state, "Material");
    RegRMat = ip->getNumObjectsFound(state, "Material:NoMass");
    IRTMat = ip->getNumObjectsFound(state, "Material:InfraredTransparent");
    AirMat = ip->getNumObjectsFound(state, "Material:AirGap");
    state.dataHeatBal->W5GlsMat = ip->getNumObjectsFound(state, "WindowMaterial:Glazing");
    state.dataHeatBal->W5GlsMatAlt = ip->getNumObjectsFound(state, "WindowMaterial:Glazing:RefractionExtinctionMethod");
    state.dataHeatBal->W5GasMat = ip->getNumObjectsFound(state, "WindowMaterial:Gas");
    state.dataHeatBal->W5GasMatMixture = ip->getNumObjectsFound(state, "WindowMaterial:GasMixture");
    state.dataHeatBal->TotShades = ip->getNumObjectsFound(state, "WindowMaterial:Shade");
    state.dataMaterial->TotComplexShades = ip->getNumObjectsFound(state, "WindowMaterial:ComplexShade");
    state.dataHeatBal->TotComplexGaps = ip->getNumObjectsFound(state, "WindowMaterial:Gap");
    state.dataHeatBal->TotScreens = ip->getNumObjectsFound(state, "WindowMaterial:Screen");
    state.dataHeatBal->TotBlinds = ip->getNumObjectsFound(state, "WindowMaterial:Blind");
    EcoRoofMat = ip->getNumObjectsFound(state, "Material:RoofVegetation");
    state.dataHeatBal->TotSimpleWindow = ip->getNumObjectsFound(state, "WindowMaterial:SimpleGlazingSystem");

    state.dataHeatBal->W5GlsMatEQL = ip->getNumObjectsFound(state, "WindowMaterial:Glazing:EquivalentLayer");
    state.dataHeatBal->TotShadesEQL = ip->getNumObjectsFound(state, "WindowMaterial:Shade:EquivalentLayer");
    state.dataHeatBal->TotDrapesEQL = ip->getNumObjectsFound(state, "WindowMaterial:Drape:EquivalentLayer");
    state.dataHeatBal->TotBlindsEQL = ip->getNumObjectsFound(state, "WindowMaterial:Blind:EquivalentLayer");
    state.dataHeatBal->TotScreensEQL = ip->getNumObjectsFound(state, "WindowMaterial:Screen:EquivalentLayer");
    state.dataHeatBal->W5GapMatEQL = ip->getNumObjectsFound(state, "WindowMaterial:Gap:EquivalentLayer");

    state.dataMaterial->TotMaterials = RegMat + RegRMat + AirMat + state.dataHeatBal->W5GlsMat + state.dataHeatBal->W5GlsMatAlt +
                                       state.dataHeatBal->W5GasMat + state.dataHeatBal->W5GasMatMixture + state.dataHeatBal->TotShades +
                                       state.dataHeatBal->TotScreens + state.dataHeatBal->TotBlinds + EcoRoofMat + IRTMat +
                                       state.dataHeatBal->TotSimpleWindow + state.dataMaterial->TotComplexShades + state.dataHeatBal->TotComplexGaps +
                                       state.dataHeatBal->W5GlsMatEQL + state.dataHeatBal->TotShadesEQL + state.dataHeatBal->TotDrapesEQL +
                                       state.dataHeatBal->TotBlindsEQL + state.dataHeatBal->TotScreensEQL + state.dataHeatBal->W5GapMatEQL;

    TotFfactorConstructs = ip->getNumObjectsFound(state, "Construction:FfactorGroundFloor");
    TotCfactorConstructs = ip->getNumObjectsFound(state, "Construction:CfactorUndergroundWall");

    if (TotFfactorConstructs > 0) {
        state.dataHeatBal->NoFfactorConstructionsUsed = false;
    }

    if (TotCfactorConstructs > 0) {
        state.dataHeatBal->NoCfactorConstructionsUsed = false;
    }

    if (TotFfactorConstructs + TotCfactorConstructs >= 1) {
        // Add a new fictitious insulation layer and a thermal mass layer for each F or C factor defined construction
        state.dataMaterial->TotMaterials += 1 + TotFfactorConstructs + TotCfactorConstructs;
    }

    // yujie: This looks kind of silly, but we need it to keep the Materials array in IDF order.
    for (int i = 1; i <= state.dataMaterial->TotMaterials; i++) {
        MaterialBase *p = new MaterialBase;
        state.dataMaterial->Material.push_back(p);
    }
    state.dataHeatBalMgr->UniqueMaterialNames.reserve(static_cast<unsigned>(state.dataMaterial->TotMaterials));

    state.dataHeatBal->NominalR.dimension(state.dataMaterial->TotMaterials, 0.0);

    MaterNum = 0;

    // Regular Materials

    state.dataHeatBalMgr->CurrentModuleObject = "Material";
    auto const instances = ip->epJSON.find(state.dataHeatBalMgr->CurrentModuleObject);
    if (instances != ip->epJSON.end()) {
        auto const &objectSchemaProps = ip->getObjectSchemaProps(state, state.dataHeatBalMgr->CurrentModuleObject);

        int counter = 0;
        auto &instancesValue = instances.value();
        for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
            auto const &objectFields = instance.value();
            std::string const &thisObjectName = Util::makeUPPER(instance.key());
            ip->markObjectAsUsed(state.dataHeatBalMgr->CurrentModuleObject, instance.key());
            std::string materialName = thisObjectName;

            if (GlobalNames::VerifyUniqueInterObjectName(state,
                                                         state.dataHeatBalMgr->UniqueMaterialNames,
                                                         materialName,
                                                         state.dataHeatBalMgr->CurrentModuleObject,
                                                         ipsc->cAlphaFieldNames(1),
                                                         ErrorsFound)) {
                continue;
            }
            // For incoming idf, maintain object order
            ++counter;
            MaterNum = ip->getIDFObjNum(state, state.dataHeatBalMgr->CurrentModuleObject, counter);

            // Load the material derived type from the input data.
            auto *thisMaterial = new MaterialChild;
            state.dataMaterial->Material(MaterNum) = thisMaterial;
            thisMaterial->group = Group::Regular;
            thisMaterial->Name = materialName;

            std::string roughness = ip->getAlphaFieldValue(objectFields, objectSchemaProps, "roughness");
            thisMaterial->Roughness = static_cast<SurfaceRoughness>(getEnumValue(surfaceRoughnessNamesUC, Util::makeUPPER(roughness)));
            thisMaterial->Thickness = ip->getRealFieldValue(objectFields, objectSchemaProps, "thickness");
            thisMaterial->Conductivity = ip->getRealFieldValue(objectFields, objectSchemaProps, "conductivity");
            thisMaterial->Density = ip->getRealFieldValue(objectFields, objectSchemaProps, "density");
            thisMaterial->SpecHeat = ip->getRealFieldValue(objectFields, objectSchemaProps, "specific_heat");
            thisMaterial->AbsorpThermal = ip->getRealFieldValue(objectFields, objectSchemaProps, "thermal_absorptance");
            thisMaterial->AbsorpThermalInput = thisMaterial->AbsorpThermal;
            thisMaterial->AbsorpSolar = ip->getRealFieldValue(objectFields, objectSchemaProps, "solar_absorptance");
            thisMaterial->AbsorpSolarInput = thisMaterial->AbsorpSolar;
            thisMaterial->AbsorpVisible = ip->getRealFieldValue(objectFields, objectSchemaProps, "visible_absorptance");
            thisMaterial->AbsorpVisibleInput = thisMaterial->AbsorpVisible;

            if (thisMaterial->Conductivity > 0.0) {
                state.dataHeatBal->NominalR(MaterNum) = thisMaterial->Thickness / thisMaterial->Conductivity;
                thisMaterial->Resistance = state.dataHeatBal->NominalR(MaterNum);
            } else {
                ShowSevereError(state, format("Positive thermal conductivity required for material {}", thisMaterial->Name));
                ErrorsFound = true;
            }
        }
        MaterNum = counter; // This works here, because this is the first material type processed
    }
    // Add the 6" heavy concrete for constructions defined with F or C factor method
    if (TotFfactorConstructs + TotCfactorConstructs >= 1) {
        ++MaterNum;
        auto *thisMaterial = new MaterialChild;
        state.dataMaterial->Material(MaterNum) = thisMaterial;
        thisMaterial->group = Group::Regular;
        thisMaterial->Name = "~FC_Concrete";
        thisMaterial->Thickness = 0.15;    // m, 0.15m = 6 inches
        thisMaterial->Conductivity = 1.95; // W/mK
        thisMaterial->Density = 2240.0;    // kg/m3
        thisMaterial->SpecHeat = 900.0;    // J/kgK
        thisMaterial->Roughness = SurfaceRoughness::MediumRough;
        thisMaterial->AbsorpSolar = 0.7;
        thisMaterial->AbsorpThermal = 0.9;
        thisMaterial->AbsorpVisible = 0.7;
        state.dataHeatBal->NominalR(MaterNum) = thisMaterial->Thickness / thisMaterial->Conductivity;
        thisMaterial->Resistance = state.dataHeatBal->NominalR(MaterNum);

        ++RegMat;
    }

    state.dataHeatBalMgr->CurrentModuleObject = "Material:NoMass";
    for (Loop = 1; Loop <= RegRMat; ++Loop) {

        // Call Input Get routine to retrieve material data
        ip->getObjectItem(state,
                          state.dataHeatBalMgr->CurrentModuleObject,
                          Loop,
                          MaterialNames,
                          MaterialNumAlpha,
                          MaterialProps,
                          MaterialNumProp,
                          IOStat,
                          ipsc->lNumericFieldBlanks,
                          ipsc->lAlphaFieldBlanks,
                          ipsc->cAlphaFieldNames,
                          ipsc->cNumericFieldNames);
        if (GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataHeatBalMgr->UniqueMaterialNames,
                                                     MaterialNames(1),
                                                     state.dataHeatBalMgr->CurrentModuleObject,
                                                     ipsc->cAlphaFieldNames(1),
                                                     ErrorsFound)) {
            ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
            continue;
        }

        // Load the material derived type from the input data.
        ++MaterNum;
        auto *thisMaterial = new MaterialChild;
        state.dataMaterial->Material(MaterNum) = thisMaterial;
        thisMaterial->group = Group::Regular;
        thisMaterial->Name = MaterialNames(1);

        thisMaterial->Roughness = static_cast<SurfaceRoughness>(getEnumValue(surfaceRoughnessNamesUC, Util::makeUPPER(MaterialNames(2))));

        thisMaterial->Resistance = MaterialProps(1);
        thisMaterial->ROnly = true;
        if (MaterialNumProp >= 2) {
            thisMaterial->AbsorpThermal = MaterialProps(2);
            thisMaterial->AbsorpThermalInput = MaterialProps(2);
        } else {
            thisMaterial->AbsorpThermal = 0.9;
            thisMaterial->AbsorpThermalInput = 0.9;
        }
        if (MaterialNumProp >= 3) {
            thisMaterial->AbsorpSolar = MaterialProps(3);
            thisMaterial->AbsorpSolarInput = MaterialProps(3);
        } else {
            thisMaterial->AbsorpSolar = 0.7;
            thisMaterial->AbsorpSolarInput = 0.7;
        }
        if (MaterialNumProp >= 4) {
            thisMaterial->AbsorpVisible = MaterialProps(4);
            thisMaterial->AbsorpVisibleInput = MaterialProps(4);
        } else {
            thisMaterial->AbsorpVisible = 0.7;
            thisMaterial->AbsorpVisibleInput = 0.7;
        }

        state.dataHeatBal->NominalR(MaterNum) = thisMaterial->Resistance;
    }

    // Add a fictitious insulation layer for each construction defined with F or C factor method
    if (TotFfactorConstructs + TotCfactorConstructs >= 1) {
        for (Loop = 1; Loop <= TotFfactorConstructs + TotCfactorConstructs; ++Loop) {
            ++MaterNum;
            auto *thisMaterial = new MaterialChild;
            state.dataMaterial->Material(MaterNum) = thisMaterial;
            thisMaterial->group = Group::Regular;
            thisMaterial->Name = format("~FC_Insulation_{}", Loop);
            thisMaterial->ROnly = true;
            thisMaterial->Roughness = SurfaceRoughness::MediumRough;
            thisMaterial->AbsorpSolar = 0.0;
            thisMaterial->AbsorpThermal = 0.0;
            thisMaterial->AbsorpVisible = 0.0;
        }
        RegRMat += TotFfactorConstructs + TotCfactorConstructs;
    }

    // Air Materials (for air spaces in opaque constructions)
    state.dataHeatBalMgr->CurrentModuleObject = "Material:AirGap";
    for (Loop = 1; Loop <= AirMat; ++Loop) {

        // Call Input Get routine to retrieve material data
        ip->getObjectItem(state,
                          state.dataHeatBalMgr->CurrentModuleObject,
                          Loop,
                          MaterialNames,
                          MaterialNumAlpha,
                          MaterialProps,
                          MaterialNumProp,
                          IOStat,
                          ipsc->lNumericFieldBlanks,
                          ipsc->lAlphaFieldBlanks,
                          ipsc->cAlphaFieldNames,
                          ipsc->cNumericFieldNames);
        if (GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataHeatBalMgr->UniqueMaterialNames,
                                                     MaterialNames(1),
                                                     state.dataHeatBalMgr->CurrentModuleObject,
                                                     ipsc->cAlphaFieldNames(1),
                                                     ErrorsFound)) {
            ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
            continue;
        }

        // Load the material derived type from the input data.
        ++MaterNum;
        auto *thisMaterial = new MaterialChild;
        state.dataMaterial->Material(MaterNum) = thisMaterial;
        thisMaterial->group = Group::Air;
        thisMaterial->Name = MaterialNames(1);

        thisMaterial->Roughness = SurfaceRoughness::MediumRough;

        thisMaterial->Resistance = MaterialProps(1);
        thisMaterial->ROnly = true;

        state.dataHeatBal->NominalR(MaterNum) = thisMaterial->Resistance;
    }

    state.dataHeatBalMgr->CurrentModuleObject = "Material:InfraredTransparent";
    for (Loop = 1; Loop <= IRTMat; ++Loop) {

        // Call Input Get routine to retrieve material data
        ip->getObjectItem(state,
                          state.dataHeatBalMgr->CurrentModuleObject,
                          Loop,
                          MaterialNames,
                          MaterialNumAlpha,
                          MaterialProps,
                          MaterialNumProp,
                          IOStat,
                          ipsc->lNumericFieldBlanks,
                          ipsc->lAlphaFieldBlanks,
                          ipsc->cAlphaFieldNames,
                          ipsc->cNumericFieldNames);
        if (GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataHeatBalMgr->UniqueMaterialNames,
                                                     MaterialNames(1),
                                                     state.dataHeatBalMgr->CurrentModuleObject,
                                                     ipsc->cAlphaFieldNames(1),
                                                     ErrorsFound)) {
            ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
            continue;
        }

        ++MaterNum;
        auto *thisMaterial = new MaterialChild;
        state.dataMaterial->Material(MaterNum) = thisMaterial;
        thisMaterial->group = Group::IRTransparent;

        // Load the material derived type from the input data.
        thisMaterial->Name = MaterialNames(1);

        // Load data for other properties that need defaults
        thisMaterial->ROnly = true;
        thisMaterial->Resistance = 0.01;
        thisMaterial->AbsorpThermal = 0.9999;
        thisMaterial->AbsorpThermalInput = 0.9999;
        thisMaterial->AbsorpSolar = 1.0;
        thisMaterial->AbsorpSolarInput = 1.0;
        thisMaterial->AbsorpVisible = 1.0;
        thisMaterial->AbsorpVisibleInput = 1.0;

        state.dataHeatBal->NominalR(MaterNum) = thisMaterial->Resistance;
    }

    // Glass materials, regular input: transmittance and front/back reflectance

    state.dataHeatBalMgr->CurrentModuleObject = "WindowMaterial:Glazing";
    for (Loop = 1; Loop <= state.dataHeatBal->W5GlsMat; ++Loop) {

        // Call Input Get routine to retrieve material data
        ip->getObjectItem(state,
                          state.dataHeatBalMgr->CurrentModuleObject,
                          Loop,
                          MaterialNames,
                          MaterialNumAlpha,
                          MaterialProps,
                          MaterialNumProp,
                          IOStat,
                          ipsc->lNumericFieldBlanks,
                          ipsc->lAlphaFieldBlanks,
                          ipsc->cAlphaFieldNames,
                          ipsc->cNumericFieldNames);
        if (GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataHeatBalMgr->UniqueMaterialNames,
                                                     MaterialNames(1),
                                                     state.dataHeatBalMgr->CurrentModuleObject,
                                                     ipsc->cAlphaFieldNames(1),
                                                     ErrorsFound)) {
            ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
            continue;
        }

        ++MaterNum;
        auto *thisMaterial = new MaterialChild;
        state.dataMaterial->Material(MaterNum) = thisMaterial;
        thisMaterial->group = Group::WindowGlass;

        // Load the material derived type from the input data.

        thisMaterial->Name = MaterialNames(1);
        thisMaterial->Roughness = SurfaceRoughness::VerySmooth;
        thisMaterial->ROnly = true;
        thisMaterial->Thickness = MaterialProps(1);
        if (!Util::SameString(MaterialNames(2), "SpectralAndAngle")) {
            thisMaterial->Trans = MaterialProps(2);
            thisMaterial->ReflectSolBeamFront = MaterialProps(3);
            thisMaterial->ReflectSolBeamBack = MaterialProps(4);
            thisMaterial->TransVis = MaterialProps(5);
            thisMaterial->ReflectVisBeamFront = MaterialProps(6);
            thisMaterial->ReflectVisBeamBack = MaterialProps(7);
            thisMaterial->TransThermal = MaterialProps(8);
        }
        thisMaterial->AbsorpThermalFront = MaterialProps(9);
        thisMaterial->AbsorpThermalBack = MaterialProps(10);
        thisMaterial->Conductivity = MaterialProps(11);
        thisMaterial->GlassTransDirtFactor = MaterialProps(12);
        thisMaterial->YoungModulus = MaterialProps(13);
        thisMaterial->PoissonsRatio = MaterialProps(14);
        if (MaterialProps(12) == 0.0) thisMaterial->GlassTransDirtFactor = 1.0;
        thisMaterial->AbsorpThermal = thisMaterial->AbsorpThermalBack;

        if (thisMaterial->Conductivity > 0.0) {
            state.dataHeatBal->NominalR(MaterNum) = thisMaterial->Thickness / thisMaterial->Conductivity;
            thisMaterial->Resistance = state.dataHeatBal->NominalR(MaterNum);
        } else {
            ErrorsFound = true;
            ShowSevereError(state, format("Window glass material {} has Conductivity = 0.0, must be >0.0, default = .9", thisMaterial->Name));
        }

        thisMaterial->GlassSpectralDataPtr = 0;
        if (state.dataHeatBal->TotSpectralData > 0 && !ipsc->lAlphaFieldBlanks(3)) {
            thisMaterial->GlassSpectralDataPtr = Util::FindItemInList(MaterialNames(3), state.dataHeatBal->SpectralData);
        }
        if (Util::SameString(MaterialNames(2), "SpectralAverage")) thisMaterial->GlassSpectralDataPtr = 0;
        // No need for spectral data for BSDF either
        if (Util::SameString(MaterialNames(2), "BSDF")) thisMaterial->GlassSpectralDataPtr = 0;
        if (Util::SameString(MaterialNames(2), "SpectralAndAngle")) thisMaterial->GlassSpectralAndAngle = true;

        if (thisMaterial->GlassSpectralDataPtr == 0 && Util::SameString(MaterialNames(2), "Spectral")) {
            ErrorsFound = true;
            ShowSevereError(state,
                            format("{}=\"{}\" has {} = Spectral but has no matching MaterialProperty:GlazingSpectralData set",
                                   state.dataHeatBalMgr->CurrentModuleObject,
                                   thisMaterial->Name,
                                   ipsc->cAlphaFieldNames(2)));
            if (ipsc->lAlphaFieldBlanks(3)) {
                ShowContinueError(state, format("...{} is blank.", ipsc->cAlphaFieldNames(3)));
            } else {
                ShowContinueError(state,
                                  format("...{}=\"{}\" not found as item in MaterialProperty:GlazingSpectralData objects.",
                                         ipsc->cAlphaFieldNames(3),
                                         MaterialNames(3)));
            }
        }

        if (!Util::SameString(MaterialNames(2), "SpectralAverage") && !Util::SameString(MaterialNames(2), "Spectral") &&
            !Util::SameString(MaterialNames(2), "BSDF") && !Util::SameString(MaterialNames(2), "SpectralAndAngle")) {
            ErrorsFound = true;
            ShowSevereError(state, format("{}=\"{}\", invalid specification.", state.dataHeatBalMgr->CurrentModuleObject, thisMaterial->Name));
            ShowContinueError(state,
                              format("{} must be SpectralAverage, Spectral, BSDF or SpectralAndAngle, value={}",
                                     ipsc->cAlphaFieldNames(2),
                                     MaterialNames(2)));
        }

        // TH 8/24/2011, allow glazing properties MaterialProps(2 to 10) to equal 0 or 1: 0.0 =< Prop <= 1.0
        // Fixed CR 8413 - modeling spandrel panels as glazing systems
        if (Util::SameString(MaterialNames(2), "SpectralAverage")) {

            if (MaterialProps(2) + MaterialProps(3) > 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, format("{}=\"{}\", Illegal value combination.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
                ShowContinueError(
                    state, format("{} + {} not <= 1.0", ipsc->cNumericFieldNames(2), ipsc->cNumericFieldNames(3)));
            }

            if (MaterialProps(2) + MaterialProps(4) > 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, format("{}=\"{}\", Illegal value combination.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
                ShowContinueError(
                    state, format("{} + {} not <= 1.0", ipsc->cNumericFieldNames(2), ipsc->cNumericFieldNames(4)));
            }

            if (MaterialProps(5) + MaterialProps(6) > 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, format("{}=\"{}\", Illegal value combination.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
                ShowContinueError(
                    state, format("{} + {} not <= 1.0", ipsc->cNumericFieldNames(5), ipsc->cNumericFieldNames(6)));
            }

            if (MaterialProps(5) + MaterialProps(7) > 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, format("{}=\"{}\", Illegal value combination.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
                ShowContinueError(
                    state, format("{} + {} not <= 1.0", ipsc->cNumericFieldNames(5), ipsc->cNumericFieldNames(7)));
            }

            if (MaterialProps(8) + MaterialProps(9) > 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, format("{}=\"{}\", Illegal value combination.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
                ShowContinueError(
                    state, format("{} + {} not <= 1.0", ipsc->cNumericFieldNames(8), ipsc->cNumericFieldNames(9)));
            }

            if (MaterialProps(8) + MaterialProps(10) > 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, format("{}=\"{}\", Illegal value combination.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
                ShowContinueError(
                    state, format("{} + {} not <= 1.0", ipsc->cNumericFieldNames(8), ipsc->cNumericFieldNames(10)));
            }

            if (MaterialProps(2) < 0.0) {
                ShowSevereError(state, format("{}=\"{}\", Illegal value.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
                ShowContinueError(state, format("{} not >= 0.0", ipsc->cNumericFieldNames(2)));
                ErrorsFound = true;
            }

            if (MaterialProps(2) > 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, format("{}=\"{}\", Illegal value.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
                ShowContinueError(state, format("{} not <= 1.0", ipsc->cNumericFieldNames(2)));
            }

            if (MaterialProps(3) < 0.0 || MaterialProps(3) > 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, format("{}=\"{}\", Illegal value.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
                ShowContinueError(state, format("{} not >= 0.0 and <= 1.0", ipsc->cNumericFieldNames(3)));
            }

            if (MaterialProps(4) < 0.0 || MaterialProps(4) > 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, format("{}=\"{}\", Illegal value.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
                ShowContinueError(state, format("{} not >= 0.0 and <= 1.0", ipsc->cNumericFieldNames(4)));
            }

            if (MaterialProps(5) < 0.0) {
                ShowWarningError(state, format("{}=\"{}\", minimal value.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
                ShowWarningError(state, format("{} not >= 0.0", ipsc->cNumericFieldNames(5)));
            }

            if (MaterialProps(5) > 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, format("{}=\"{}\", Illegal value.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
                ShowContinueError(state, format("{} not <= 1.0", ipsc->cNumericFieldNames(5)));
            }

            if (MaterialProps(6) < 0.0 || MaterialProps(6) > 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, format("{}=\"{}\", Illegal value.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
                ShowContinueError(state, format("{} not >= 0.0 and <= 1.0", ipsc->cNumericFieldNames(6)));
            }

            if (MaterialProps(7) < 0.0 || MaterialProps(7) > 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, format("{}=\"{}\", Illegal value.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
                ShowContinueError(state, format("{} not >= 0.0 and <= 1.0", ipsc->cNumericFieldNames(7)));
            }
        }

        if (MaterialProps(8) > 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, format("{}=\"{}\", Illegal value.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
            ShowContinueError(state, format("{} not <= 1.0", ipsc->cNumericFieldNames(8)));
        }

        if (MaterialProps(9) <= 0.0 || MaterialProps(9) >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, format("{}=\"{}\", Illegal value.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
            ShowContinueError(state, format("{} not > 0.0 and < 1.0", ipsc->cNumericFieldNames(9)));
        }

        if (MaterialProps(10) <= 0.0 || MaterialProps(10) >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, format("{}=\"{}\", Illegal value.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
            ShowContinueError(state, format("{} not > 0.0 and < 1.0", ipsc->cNumericFieldNames(10)));
        }

        if (MaterialProps(11) <= 0.0) {
            ErrorsFound = true;
            ShowSevereError(state, format("{}=\"{}\", Illegal value.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
            ShowContinueError(state, format("{} not > 0.0", ipsc->cNumericFieldNames(11)));
        }

        if (MaterialProps(13) < 0.0) {
            ErrorsFound = true;
            ShowSevereError(state, format("{}=\"{}\", Illegal value.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
            ShowContinueError(state, format("{} not > 0.0", ipsc->cNumericFieldNames(13)));
        }

        if (MaterialProps(14) < 0.0 || MaterialProps(14) >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, format("{}=\"{}\", Illegal value.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
            ShowContinueError(state, format("{} not > 0.0 and < 1.0", ipsc->cNumericFieldNames(14)));
        }

        if (MaterialNames(4) == "") {
            thisMaterial->SolarDiffusing = false;
        } else {
            BooleanSwitch answer = getYesNoValue(MaterialNames(4));
            if (answer == BooleanSwitch::Invalid) {
                ErrorsFound = true;
                ShowSevereError(state, format("{}=\"{}\", Illegal value.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
                ShowContinueError(state,
                                  format("{} must be Yes or No, entered value={}", ipsc->cNumericFieldNames(4), MaterialNames(4)));
            } else {
                thisMaterial->SolarDiffusing = (answer == BooleanSwitch::Yes);
            }
        }
        // Get SpectralAndAngle table names
        if (thisMaterial->GlassSpectralAndAngle) {
            if (ipsc->lAlphaFieldBlanks(5)) {
                ErrorsFound = true;
                ShowSevereError(state, format("{}=\"{}\", blank field.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
                ShowContinueError(state, " Table name must be entered when the key SpectralAndAngle is selected as Optical Data Type.");
            } else {
                thisMaterial->GlassSpecAngTransDataPtr = Curve::GetCurveIndex(state, MaterialNames(5));
                if (thisMaterial->GlassSpecAngTransDataPtr == 0) {
                    ErrorsFound = true;
                    ShowSevereError(state, format("{}=\"{}\", Invalid name.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
                    ShowContinueError(state,
                                      format("{} requires a valid table object name, entered input={}",
                                             ipsc->cAlphaFieldNames(5),
                                             MaterialNames(5)));
                } else {
                    ErrorsFound |= Curve::CheckCurveDims(state,
                                                         thisMaterial->GlassSpecAngTransDataPtr,     // Curve index
                                                         {2},                                        // Valid dimensions
                                                         RoutineName,                                // Routine name
                                                         state.dataHeatBalMgr->CurrentModuleObject,  // Object Type
                                                         thisMaterial->Name,                         // Object Name
                                                         ipsc->cAlphaFieldNames(5)); // Field Name

                    GetCurveMinMaxValues(state, thisMaterial->GlassSpecAngTransDataPtr, minAngValue, maxAngValue, minLamValue, maxLamValue);
                    if (minAngValue > 1.0e-6) {
                        ErrorsFound = true;
                        ShowSevereError(state,
                                        format("{}=\"{}\", Invalid minimum value of angle = {:.2R}.",
                                               state.dataHeatBalMgr->CurrentModuleObject,
                                               MaterialNames(1),
                                               minAngValue));
                        ShowContinueError(state,
                                          format("{} requires the minumum value = 0.0 in the entered table name={}",
                                                 ipsc->cAlphaFieldNames(5),
                                                 MaterialNames(5)));
                    }
                    if (std::abs(maxAngValue - 90.0) > 1.0e-6) {
                        ErrorsFound = true;
                        ShowSevereError(state,
                                        format("{}=\"{}\", Invalid maximum value of angle = {:.2R}.",
                                               state.dataHeatBalMgr->CurrentModuleObject,
                                               MaterialNames(1),
                                               maxAngValue));
                        ShowContinueError(state,
                                          format("{} requires the maximum value = 90.0 in the entered table name={}",
                                                 ipsc->cAlphaFieldNames(5),
                                                 MaterialNames(5)));
                    }
                    if (minLamValue < 0.1) {
                        ErrorsFound = true;
                        ShowSevereError(state,
                                        format("{}=\"{}\", Invalid minimum value of wavelength = {:.2R}.",
                                               state.dataHeatBalMgr->CurrentModuleObject,
                                               MaterialNames(1),
                                               minLamValue));
                        ShowContinueError(state,
                                          format("{} requires the minumum value = 0.1 micron in the entered table name={}",
                                                 ipsc->cAlphaFieldNames(5),
                                                 MaterialNames(5)));
                    }
                    if (maxLamValue > 4.0) {
                        ErrorsFound = true;
                        ShowSevereError(state,
                                        format("{}=\"{}\", Invalid maximum value of wavelength = {:.2R}.",
                                               state.dataHeatBalMgr->CurrentModuleObject,
                                               MaterialNames(1),
                                               maxLamValue));
                        ShowContinueError(state,
                                          format("{} requires the maximum value = 4.0 microns in the entered table name={}",
                                                 ipsc->cAlphaFieldNames(5),
                                                 MaterialNames(5)));
                    }
                }
            }
            if (ipsc->lAlphaFieldBlanks(6)) {
                ErrorsFound = true;
                ShowSevereError(state, format("{}=\"{}\", blank field.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
                ShowContinueError(state, " Table name must be entered when the key SpectralAndAngle is selected as Optical Data Type.");
            } else {
                thisMaterial->GlassSpecAngFRefleDataPtr = Curve::GetCurveIndex(state, MaterialNames(6));
                if (thisMaterial->GlassSpecAngFRefleDataPtr == 0) {
                    ErrorsFound = true;
                    ShowSevereError(state, format("{}=\"{}\", Invalid name.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
                    ShowContinueError(state,
                                      format("{} requires a valid table object name, entered input={}",
                                             ipsc->cAlphaFieldNames(6),
                                             MaterialNames(6)));
                } else {
                    ErrorsFound |= Curve::CheckCurveDims(state,
                                                         thisMaterial->GlassSpecAngFRefleDataPtr,    // Curve index
                                                         {2},                                        // Valid dimensions
                                                         RoutineName,                                // Routine name
                                                         state.dataHeatBalMgr->CurrentModuleObject,  // Object Type
                                                         thisMaterial->Name,                         // Object Name
                                                         ipsc->cAlphaFieldNames(6)); // Field Name

                    GetCurveMinMaxValues(state, thisMaterial->GlassSpecAngFRefleDataPtr, minAngValue, maxAngValue, minLamValue, maxLamValue);
                    if (minAngValue > 1.0e-6) {
                        ErrorsFound = true;
                        ShowSevereError(state,
                                        format("{}=\"{}\", Invalid minimum value of angle = {:.2R}.",
                                               state.dataHeatBalMgr->CurrentModuleObject,
                                               MaterialNames(1),
                                               minAngValue));
                        ShowContinueError(state,
                                          format("{} requires the minumum value = 0.0 in the entered table name={}",
                                                 ipsc->cAlphaFieldNames(5),
                                                 MaterialNames(5)));
                    }
                    if (std::abs(maxAngValue - 90.0) > 1.0e-6) {
                        ErrorsFound = true;
                        ShowSevereError(state,
                                        format("{}=\"{}\", Invalid maximum value of angle = {:.2R}.",
                                               state.dataHeatBalMgr->CurrentModuleObject,
                                               MaterialNames(1),
                                               maxAngValue));
                        ShowContinueError(state,
                                          format("{} requires the maximum value = 90.0 in the entered table name={}",
                                                 ipsc->cAlphaFieldNames(5),
                                                 MaterialNames(5)));
                    }
                    if (minLamValue < 0.1) {
                        ErrorsFound = true;
                        ShowSevereError(state,
                                        format("{}=\"{}\", Invalid minimum value of wavelength = {:.2R}.",
                                               state.dataHeatBalMgr->CurrentModuleObject,
                                               MaterialNames(1),
                                               minLamValue));
                        ShowContinueError(state,
                                          format("{} requires the minumum value = 0.1 micron in the entered table name={}",
                                                 ipsc->cAlphaFieldNames(5),
                                                 MaterialNames(5)));
                    }
                    if (maxLamValue > 4.0) {
                        ErrorsFound = true;
                        ShowSevereError(state,
                                        format("{}=\"{}\", Invalid maximum value of wavelength = {:.2R}.",
                                               state.dataHeatBalMgr->CurrentModuleObject,
                                               MaterialNames(1),
                                               maxLamValue));
                        ShowContinueError(state,
                                          format("{} requires the maximum value = 4.0 microns in the entered table name={}",
                                                 ipsc->cAlphaFieldNames(5),
                                                 MaterialNames(5)));
                    }
                }
            }
            if (ipsc->lAlphaFieldBlanks(7)) {
                ErrorsFound = true;
                ShowSevereError(state, format("{}=\"{}\", blank field.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
                ShowContinueError(state, " Table name must be entered when the key SpectralAndAngle is selected as Optical Data Type.");
            } else {
                thisMaterial->GlassSpecAngBRefleDataPtr = Curve::GetCurveIndex(state, MaterialNames(7));
                if (thisMaterial->GlassSpecAngBRefleDataPtr == 0) {
                    ErrorsFound = true;
                    ShowSevereError(state, format("{}=\"{}\", Invalid name.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
                    ShowContinueError(state,
                                      format("{} requires a valid table object name, entered input={}",
                                             ipsc->cAlphaFieldNames(7),
                                             MaterialNames(7)));
                } else {
                    ErrorsFound |= Curve::CheckCurveDims(state,
                                                         thisMaterial->GlassSpecAngBRefleDataPtr,    // Curve index
                                                         {2},                                        // Valid dimensions
                                                         RoutineName,                                // Routine name
                                                         state.dataHeatBalMgr->CurrentModuleObject,  // Object Type
                                                         thisMaterial->Name,                         // Object Name
                                                         ipsc->cAlphaFieldNames(7)); // Field Name

                    GetCurveMinMaxValues(state, thisMaterial->GlassSpecAngBRefleDataPtr, minAngValue, maxAngValue, minLamValue, maxLamValue);
                    if (minAngValue > 1.0e-6) {
                        ErrorsFound = true;
                        ShowSevereError(state,
                                        format("{}=\"{}\", Invalid minimum value of angle = {:.2R}.",
                                               state.dataHeatBalMgr->CurrentModuleObject,
                                               MaterialNames(1),
                                               minAngValue));
                        ShowContinueError(state,
                                          format("{} requires the minumum value = 0.0 in the entered table name={}",
                                                 ipsc->cAlphaFieldNames(5),
                                                 MaterialNames(5)));
                    }
                    if (std::abs(maxAngValue - 90.0) > 1.0e-6) {
                        ErrorsFound = true;
                        ShowSevereError(state,
                                        format("{}=\"{}\", Invalid maximum value of angle = {:.2R}.",
                                               state.dataHeatBalMgr->CurrentModuleObject,
                                               MaterialNames(1),
                                               maxAngValue));
                        ShowContinueError(state,
                                          format("{} requires the maximum value = 90.0 in the entered table name={}",
                                                 ipsc->cAlphaFieldNames(5),
                                                 MaterialNames(5)));
                    }
                    if (minLamValue < 0.1) {
                        ErrorsFound = true;
                        ShowSevereError(state,
                                        format("{}=\"{}\", Invalid minimum value of wavelength = {:.2R}.",
                                               state.dataHeatBalMgr->CurrentModuleObject,
                                               MaterialNames(1),
                                               minLamValue));
                        ShowContinueError(state,
                                          format("{} requires the minumum value = 0.1 micron in the entered table name={}",
                                                 ipsc->cAlphaFieldNames(5),
                                                 MaterialNames(5)));
                    }
                    if (maxLamValue > 4.0) {
                        ErrorsFound = true;
                        ShowSevereError(state,
                                        format("{}=\"{}\", Invalid maximum value of wavelength = {:.2R}.",
                                               state.dataHeatBalMgr->CurrentModuleObject,
                                               MaterialNames(1),
                                               maxLamValue));
                        ShowContinueError(state,
                                          format("{} requires the maximum value = 4.0 microns in the entered table name={}",
                                                 ipsc->cAlphaFieldNames(5),
                                                 MaterialNames(5)));
                    }
                }
            }
        }
    }

    // Glass materials, alternative input: index of refraction and extinction coefficient

    state.dataHeatBalMgr->CurrentModuleObject = "WindowMaterial:Glazing:RefractionExtinctionMethod";
    for (Loop = 1; Loop <= state.dataHeatBal->W5GlsMatAlt; ++Loop) {

        // Call Input Get routine to retrieve material data
        ip->getObjectItem(state,
                          state.dataHeatBalMgr->CurrentModuleObject,
                          Loop,
                          MaterialNames,
                          MaterialNumAlpha,
                          MaterialProps,
                          MaterialNumProp,
                          IOStat,
                          ipsc->lNumericFieldBlanks,
                          ipsc->lAlphaFieldBlanks,
                          ipsc->cAlphaFieldNames,
                          ipsc->cNumericFieldNames);
        if (GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataHeatBalMgr->UniqueMaterialNames,
                                                     MaterialNames(1),
                                                     state.dataHeatBalMgr->CurrentModuleObject,
                                                     ipsc->cAlphaFieldNames(1),
                                                     ErrorsFound)) {
            ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
            continue;
        }

        ++MaterNum;
        auto *thisMaterial = new MaterialChild;
        state.dataMaterial->Material(MaterNum) = thisMaterial;
        thisMaterial->group = Group::WindowGlass;

        // Load the material derived type from the input data.

        thisMaterial->Name = MaterialNames(1);
        thisMaterial->Roughness = SurfaceRoughness::VerySmooth;
        thisMaterial->Thickness = MaterialProps(1);
        thisMaterial->ROnly = true;

        // Calculate solar and visible transmittance and reflectance at normal incidence from thickness,
        // index of refraction and extinction coefficient. With the alternative input the front and back
        // properties are assumed to be the same.

        ReflectivitySol = pow_2((MaterialProps(2) - 1.0) / (MaterialProps(2) + 1.0));
        ReflectivityVis = pow_2((MaterialProps(4) - 1.0) / (MaterialProps(4) + 1.0));
        TransmittivitySol = std::exp(-MaterialProps(3) * MaterialProps(1));
        TransmittivityVis = std::exp(-MaterialProps(5) * MaterialProps(1));
        thisMaterial->Trans = TransmittivitySol * pow_2(1.0 - ReflectivitySol) / (1.0 - pow_2(ReflectivitySol * TransmittivitySol));
        thisMaterial->ReflectSolBeamFront =
            ReflectivitySol * (1.0 + pow_2(1.0 - ReflectivitySol) * pow_2(TransmittivitySol) / (1.0 - pow_2(ReflectivitySol * TransmittivitySol)));
        thisMaterial->ReflectSolBeamBack = thisMaterial->ReflectSolBeamFront;
        thisMaterial->TransVis = TransmittivityVis * pow_2(1.0 - ReflectivityVis) / (1.0 - pow_2(ReflectivityVis * TransmittivityVis));

        thisMaterial->ReflectVisBeamFront =
            ReflectivityVis * (1.0 + pow_2(1.0 - ReflectivityVis) * pow_2(TransmittivityVis) / (1.0 - pow_2(ReflectivityVis * TransmittivityVis)));
        thisMaterial->ReflectVisBeamBack = thisMaterial->ReflectSolBeamFront;
        thisMaterial->TransThermal = MaterialProps(6);
        thisMaterial->AbsorpThermalFront = MaterialProps(7);
        thisMaterial->AbsorpThermalBack = MaterialProps(7);
        thisMaterial->Conductivity = MaterialProps(8);
        thisMaterial->GlassTransDirtFactor = MaterialProps(9);
        if (MaterialProps(9) == 0.0) thisMaterial->GlassTransDirtFactor = 1.0;
        thisMaterial->AbsorpThermal = thisMaterial->AbsorpThermalBack;

        if (thisMaterial->Conductivity > 0.0) {
            state.dataHeatBal->NominalR(MaterNum) = thisMaterial->Thickness / thisMaterial->Conductivity;
            thisMaterial->Resistance = state.dataHeatBal->NominalR(MaterNum);
        }

        thisMaterial->GlassSpectralDataPtr = 0;

        if (MaterialProps(6) + MaterialProps(7) >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, format("{}=\"{}\", Illegal value combination.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
            ShowContinueError(state,
                              format("{} + {} not < 1.0", ipsc->cNumericFieldNames(6), ipsc->cNumericFieldNames(7)));
        }

        if (MaterialNames(2) == "") {
            thisMaterial->SolarDiffusing = false;
        } else if (MaterialNames(2) == "YES") {
            thisMaterial->SolarDiffusing = true;
        } else if (MaterialNames(2) == "NO") {
            thisMaterial->SolarDiffusing = false;
        } else {
            ErrorsFound = true;
            ShowSevereError(state, format("{}=\"{}\", Illegal value.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
            ShowContinueError(state, format("{} must be Yes or No, entered value={}", ipsc->cNumericFieldNames(2), MaterialNames(4)));
        }
    }

    // Glass materials, equivalent layer (ASHWAT) method
    state.dataHeatBalMgr->CurrentModuleObject = "WindowMaterial:Glazing:EquivalentLayer";
    for (Loop = 1; Loop <= state.dataHeatBal->W5GlsMatEQL; ++Loop) {

        // Call Input Get routine to retrieve material data
        ip->getObjectItem(state,
                          state.dataHeatBalMgr->CurrentModuleObject,
                          Loop,
                          MaterialNames,
                          MaterialNumAlpha,
                          MaterialProps,
                          MaterialNumProp,
                          IOStat,
                          ipsc->lNumericFieldBlanks,
                          ipsc->lAlphaFieldBlanks,
                          ipsc->cAlphaFieldNames,
                          ipsc->cNumericFieldNames);
        if (GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataHeatBalMgr->UniqueMaterialNames,
                                                     MaterialNames(1),
                                                     state.dataHeatBalMgr->CurrentModuleObject,
                                                     ipsc->cAlphaFieldNames(1),
                                                     ErrorsFound)) {
            ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
            continue;
        }

        ++MaterNum;
        auto *thisMaterial = new MaterialChild;
        state.dataMaterial->Material(MaterNum) = thisMaterial;
        thisMaterial->group = Group::GlassEquivalentLayer;

        // Load the material derived type from the input data.
        thisMaterial->Name = MaterialNames(1);
        thisMaterial->Roughness = SurfaceRoughness::VerySmooth;
        thisMaterial->ROnly = true;

        thisMaterial->TausFrontBeamBeam = MaterialProps(1);
        thisMaterial->TausBackBeamBeam = MaterialProps(2);
        thisMaterial->ReflFrontBeamBeam = MaterialProps(3);
        thisMaterial->ReflBackBeamBeam = MaterialProps(4);
        thisMaterial->TausFrontBeamBeamVis = MaterialProps(5);
        thisMaterial->TausBackBeamBeamVis = MaterialProps(6);
        thisMaterial->ReflFrontBeamBeamVis = MaterialProps(7);
        thisMaterial->ReflBackBeamBeamVis = MaterialProps(8);
        thisMaterial->TausFrontBeamDiff = MaterialProps(9);
        thisMaterial->TausBackBeamDiff = MaterialProps(10);
        thisMaterial->ReflFrontBeamDiff = MaterialProps(11);
        thisMaterial->ReflBackBeamDiff = MaterialProps(12);
        thisMaterial->TausFrontBeamDiffVis = MaterialProps(13);
        thisMaterial->TausBackBeamDiffVis = MaterialProps(14);
        thisMaterial->ReflFrontBeamDiffVis = MaterialProps(15);
        thisMaterial->ReflBackBeamDiffVis = MaterialProps(16);
        thisMaterial->TausDiffDiff = MaterialProps(17);
        thisMaterial->ReflFrontDiffDiff = MaterialProps(18);
        thisMaterial->ReflBackDiffDiff = MaterialProps(19);
        thisMaterial->TausDiffDiffVis = MaterialProps(20);
        thisMaterial->ReflFrontDiffDiffVis = MaterialProps(21);
        thisMaterial->ReflBackDiffDiffVis = MaterialProps(22);
        thisMaterial->TausThermal = MaterialProps(23);
        thisMaterial->EmissThermalFront = MaterialProps(24);
        thisMaterial->EmissThermalBack = MaterialProps(25);
        thisMaterial->Resistance = MaterialProps(26);
        if (thisMaterial->Resistance <= 0.0) thisMaterial->Resistance = 0.158; // equivalent to single pane of 1/4" inch standard glass
        // Assumes thermal emissivity is the same as thermal absorptance
        thisMaterial->AbsorpThermalFront = thisMaterial->EmissThermalFront;
        thisMaterial->AbsorpThermalBack = thisMaterial->EmissThermalBack;
        thisMaterial->TransThermal = thisMaterial->TausThermal;

        if (Util::SameString(MaterialNames(2), "SpectralAverage")) thisMaterial->GlassSpectralDataPtr = 0;

        // IF(dataMaterial.Material(MaterNum)%GlassSpectralDataPtr == 0 .AND. Util::SameString(MaterialNames(2),'Spectral')) THEN
        //  ErrorsFound = .TRUE.
        //  CALL ShowSevereError(state, TRIM(state.dataHeatBalMgr->CurrentModuleObject)//'="'//Trim(dataMaterial.Material(MaterNum)%Name)// &
        //        '" has '//TRIM(cAlphaFieldNames(2))//' = Spectral but has no matching MaterialProperty:GlazingSpectralData set')
        //  if (ipsc->lAlphaFieldBlanks(3)) THEN
        //    CALL ShowContinueError(state, '...'//TRIM(cAlphaFieldNames(3))//' is blank.')
        //  ELSE
        //    CALL ShowContinueError(state, '...'//TRIM(cAlphaFieldNames(3))//'="'//TRIM(MaterialNames(3))//  &
        //       '" not found as item in MaterialProperty:GlazingSpectralData objects.')
        //  END IF
        // END IF

        if (!Util::SameString(MaterialNames(2), "SpectralAverage")) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + thisMaterial->Name + "\", invalid specification.");
            ShowContinueError(state, ipsc->cAlphaFieldNames(2) + " must be SpectralAverage, value=" + MaterialNames(2));
        }

    } // W5GlsMatEQL loop

    // Window gas materials (for gaps with a single gas)

    state.dataHeatBalMgr->CurrentModuleObject = "WindowMaterial:Gas";
    for (Loop = 1; Loop <= state.dataHeatBal->W5GasMat; ++Loop) {

        // Call Input Get routine to retrieve material data
        ip->getObjectItem(state,
                          state.dataHeatBalMgr->CurrentModuleObject,
                          Loop,
                          MaterialNames,
                          MaterialNumAlpha,
                          MaterialProps,
                          MaterialNumProp,
                          IOStat,
                          ipsc->lNumericFieldBlanks,
                          ipsc->lAlphaFieldBlanks,
                          ipsc->cAlphaFieldNames,
                          ipsc->cNumericFieldNames);
        if (GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataHeatBalMgr->UniqueMaterialNames,
                                                     MaterialNames(1),
                                                     state.dataHeatBalMgr->CurrentModuleObject,
                                                     ipsc->cAlphaFieldNames(1),
                                                     ErrorsFound)) {
            ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
            continue;
        }

        ++MaterNum;
        auto *matGas = new MaterialGasMix;
        state.dataMaterial->Material(MaterNum) = matGas;
        matGas->group = Group::WindowGas;
        matGas->numGases = 1;
        matGas->gasFracts[0] = 1.0;

        // Load the material derived type from the input data.

        matGas->Name = MaterialNames(1);
        matGas->gases[0].type = static_cast<GasType>(getEnumValue(gasTypeNamesUC, Util::makeUPPER(MaterialNames(2))));
        matGas->Roughness = SurfaceRoughness::MediumRough;

        matGas->Thickness = MaterialProps(1);
        matGas->ROnly = true;

        gasType = matGas->gases[0].type;
        if (gasType != GasType::Custom) {
            matGas->gases[0] = gases[(int)gasType];
        }

        // Custom gas

        if (gasType == GasType::Custom) {
            matGas->gases[0].con.c0 = MaterialProps(2);
            matGas->gases[0].con.c1 = MaterialProps(3);
            matGas->gases[0].con.c2 = MaterialProps(4);
            matGas->gases[0].vis.c0 = MaterialProps(5);
            matGas->gases[0].vis.c1 = MaterialProps(6);
            matGas->gases[0].vis.c2 = MaterialProps(7);
            matGas->gases[0].cp.c0 = MaterialProps(8);
            matGas->gases[0].cp.c1 = MaterialProps(9);
            matGas->gases[0].cp.c2 = MaterialProps(10);
            matGas->gases[0].wght = MaterialProps(11);
            matGas->gases[0].specHeatRatio = MaterialProps(12);

            // Check for errors in custom gas properties
            //      IF(dataMaterial.Material(MaterNum)%GasCon(1,1) <= 0.0) THEN
            //        ErrorsFound = .TRUE.
            //        CALL ShowSevereError(state, 'Conductivity Coefficient A for custom window gas='&
            //                 //TRIM(MaterialNames(1))//' should be > 0.')
            //      END IF

            if (matGas->gases[0].vis.c0 <= 0.0) {
                ErrorsFound = true;
                ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                ShowContinueError(state, ipsc->cNumericFieldNames(5) + " not > 0.0");
            }
            if (matGas->gases[0].cp.c0 <= 0.0) {
                ErrorsFound = true;
                ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                ShowContinueError(state, ipsc->cNumericFieldNames(8) + " not > 0.0");
            }
            if (matGas->gases[0].wght <= 0.0) {
                ErrorsFound = true;
                ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                ShowContinueError(state, ipsc->cNumericFieldNames(11) + " not > 0.0");
            }
        }

        // Nominal resistance of gap at room temperature
        if (!ErrorsFound) {
            DenomRGas = (matGas->gases[0].con.c0 + matGas->gases[0].con.c1 * 300.0 + matGas->gases[0].con.c2 * 90000.0);
            if (DenomRGas > 0.0) {
                state.dataHeatBal->NominalR(MaterNum) = matGas->Thickness / DenomRGas;
            } else {
                ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                ShowContinueError(state,
                                  format("Nominal resistance of gap at room temperature calculated at a negative Conductivity=[{:.3R}].", DenomRGas));
                ErrorsFound = true;
            }
        }
    }

    // Window gap materials (for gaps with a single gas for EquivalentLayer)

    state.dataHeatBalMgr->CurrentModuleObject = "WindowMaterial:Gap:EquivalentLayer";
    for (Loop = 1; Loop <= state.dataHeatBal->W5GapMatEQL; ++Loop) {

        // Call Input Get routine to retrieve material data
        ip->getObjectItem(state,
                          state.dataHeatBalMgr->CurrentModuleObject,
                          Loop,
                          MaterialNames,
                          MaterialNumAlpha,
                          MaterialProps,
                          MaterialNumProp,
                          IOStat,
                          ipsc->lNumericFieldBlanks,
                          ipsc->lAlphaFieldBlanks,
                          ipsc->cAlphaFieldNames,
                          ipsc->cNumericFieldNames);
        if (GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataHeatBalMgr->UniqueMaterialNames,
                                                     MaterialNames(1),
                                                     state.dataHeatBalMgr->CurrentModuleObject,
                                                     ipsc->cAlphaFieldNames(1),
                                                     ErrorsFound)) {
            ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
            continue;
        }

        ++MaterNum;
        auto *matGas = new MaterialGasMix;
        state.dataMaterial->Material(MaterNum) = matGas;
        matGas->group = Group::GapEquivalentLayer;
        matGas->numGases = 1;
        matGas->gasFracts[0] = 1.0;

        // Load the material derived type from the input data.

        matGas->Name = MaterialNames(1);
        matGas->gases[0].type = static_cast<GasType>(getEnumValue(gasTypeNamesUC, Util::makeUPPER(MaterialNames(2)))); // Error check?

        matGas->Roughness = SurfaceRoughness::MediumRough;

        matGas->Thickness = MaterialProps(1);
        matGas->ROnly = true;

        gasType = matGas->gases[0].type;
        if (gasType != GasType::Custom) {
            matGas->gases[0] = gases[(int)gasType];
        }

        if (!ipsc->lAlphaFieldBlanks(2)) {
            // Get gap vent type
            matGas->gapVentType = static_cast<GapVentType>(getEnumValue(gapVentTypeNamesUC, Util::makeUPPER(MaterialNames(3))));
        }

        if (gasType == GasType::Custom) {
            for (ICoeff = 1; ICoeff <= 3; ++ICoeff) {
                matGas->gases[0].con.c0 = MaterialProps(2);
                matGas->gases[0].con.c1 = MaterialProps(3);
                matGas->gases[0].con.c2 = MaterialProps(4);
                matGas->gases[0].vis.c0 = MaterialProps(5);
                matGas->gases[0].vis.c1 = MaterialProps(6);
                matGas->gases[0].vis.c2 = MaterialProps(7);
                matGas->gases[0].cp.c0 = MaterialProps(8);
                matGas->gases[0].cp.c1 = MaterialProps(9);
                matGas->gases[0].cp.c2 = MaterialProps(10);
            }
            matGas->gases[0].wght = MaterialProps(11);
            matGas->gases[0].specHeatRatio = MaterialProps(12);

            if (matGas->gases[0].vis.c0 <= 0.0) {
                ErrorsFound = true;
                ShowSevereError(state, format("{}=\"{}\", Illegal value.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
                ShowContinueError(state, format("{} not > 0.0", ipsc->cNumericFieldNames(5)));
            }
            if (matGas->gases[0].cp.c0 <= 0.0) {
                ErrorsFound = true;
                ShowSevereError(state, format("{}=\"{}\", Illegal value.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
                ShowContinueError(state, format("{} not > 0.0", ipsc->cNumericFieldNames(8)));
            }
            if (matGas->gases[0].wght <= 0.0) {
                ErrorsFound = true;
                ShowSevereError(state, format("{}=\"{}\", Illegal value.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
                ShowContinueError(state, format("{} not > 0.0", ipsc->cNumericFieldNames(11)));
            }
        }

        // Nominal resistance of gap at room temperature
        if (!ErrorsFound) {
            DenomRGas = (matGas->gases[0].con.c0 + matGas->gases[0].con.c1 * 300.0 + matGas->gases[0].con.c2 * 90000.0);
            if (DenomRGas > 0.0) {
                state.dataHeatBal->NominalR(MaterNum) = matGas->Thickness / DenomRGas;
            } else {
                ShowSevereError(state, format("{}=\"{}\", Illegal value.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
                ShowContinueError(state,
                                  format("Nominal resistance of gap at room temperature calculated at a negative Conductivity=[{:.3R}].", DenomRGas));
                ErrorsFound = true;
            }
        }
    } // for (Loop : W5MatEQL) 

    // Window gas mixtures (for gaps with two or more gases)

    state.dataHeatBalMgr->CurrentModuleObject = "WindowMaterial:GasMixture";
    for (Loop = 1; Loop <= state.dataHeatBal->W5GasMatMixture; ++Loop) {

        // Call Input Get routine to retrieve material data
        ip->getObjectItem(state,
                          state.dataHeatBalMgr->CurrentModuleObject,
                          Loop,
                          ipsc->cAlphaArgs,
                          MaterialNumAlpha,
                          MaterialProps,
                          MaterialNumProp,
                          IOStat,
                          ipsc->lNumericFieldBlanks,
                          ipsc->lAlphaFieldBlanks,
                          ipsc->cAlphaFieldNames,
                          ipsc->cNumericFieldNames);
        if (GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataHeatBalMgr->UniqueMaterialNames,
                                                     ipsc->cAlphaArgs(1),
                                                     state.dataHeatBalMgr->CurrentModuleObject,
                                                     ipsc->cAlphaFieldNames(1),
                                                     ErrorsFound)) {
            ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
            continue;
        }

        ++MaterNum;
        auto *matGas = new MaterialGasMix;
        state.dataMaterial->Material(MaterNum) = matGas;
        matGas->group = Group::WindowGasMixture;
        matGas->gases[0].type = matGas->gases[1].type = matGas->gases[2].type = matGas->gases[3].type = matGas->gases[4].type = GasType::Invalid;

        // Load the material derived type from the input data.

        matGas->Name = ipsc->cAlphaArgs(1);
        NumGases = MaterialProps(2);
        matGas->numGases = NumGases;
        for (NumGas = 0; NumGas < NumGases; ++NumGas) {
             auto &gas = matGas->gases[NumGas];
             gas.type = static_cast<GasType>(getEnumValue(gasTypeNamesUC, Util::makeUPPER(ipsc->cAlphaArgs(2 + NumGas))));
             if (gas.type == GasType::Invalid) {
                 ShowSevereError(state, format("{}=\"{}\", Illegal value.", state.dataHeatBalMgr->CurrentModuleObject, ipsc->cAlphaArgs(1 + NumGas)));
                     // Error check?
                 ErrorsFound = true;
             }
        }

        matGas->Roughness = SurfaceRoughness::MediumRough; // Unused

        matGas->Thickness = MaterialProps(1);
        if (matGas->Thickness <= 0.0) {
            ShowSevereError(state, format("{}=\"{}\", Illegal value.", state.dataHeatBalMgr->CurrentModuleObject, ipsc->cAlphaArgs(1)));
            ShowContinueError(state, ipsc->cNumericFieldNames(1) + " must be greater than 0.");
        }
        matGas->ROnly = true;

        for (NumGas = 0; NumGas < NumGases; ++NumGas) {
            GasType gasType = matGas->gases[NumGas].type;
            if (gasType != GasType::Custom) {
                matGas->gasFracts[NumGas] = MaterialProps(3 + NumGas);
                matGas->gases[NumGas] = gases[(int)gasType];
            }
        }

        // Nominal resistance of gap at room temperature (based on first gas in mixture)
        state.dataHeatBal->NominalR(MaterNum) =
            matGas->Thickness / (matGas->gases[0].con.c0 + matGas->gases[0].con.c1 * 300.0 + matGas->gases[0].con.c2 * 90000.0);
    }

    // Window Shade Materials

    state.dataHeatBalMgr->CurrentModuleObject = "WindowMaterial:Shade";
    for (Loop = 1; Loop <= state.dataHeatBal->TotShades; ++Loop) {

        // Call Input Get routine to retrieve material data
        ip->getObjectItem(state,
                          state.dataHeatBalMgr->CurrentModuleObject,
                          Loop,
                          MaterialNames,
                          MaterialNumAlpha,
                          MaterialProps,
                          MaterialNumProp,
                          IOStat,
                          ipsc->lNumericFieldBlanks,
                          ipsc->lAlphaFieldBlanks,
                          ipsc->cAlphaFieldNames,
                          ipsc->cNumericFieldNames);
        if (GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataHeatBalMgr->UniqueMaterialNames,
                                                     MaterialNames(1),
                                                     state.dataHeatBalMgr->CurrentModuleObject,
                                                     ipsc->cAlphaFieldNames(1),
                                                     ErrorsFound)) {
            ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
            continue;
        }

        ++MaterNum;
        auto *thisMaterial = new MaterialChild;
        state.dataMaterial->Material(MaterNum) = thisMaterial;
        thisMaterial->group = Group::Shade;

        // Load the material derived type from the input data.

        thisMaterial->Name = MaterialNames(1);
        thisMaterial->Roughness = SurfaceRoughness::MediumRough;
        thisMaterial->Trans = MaterialProps(1);
        thisMaterial->ReflectShade = MaterialProps(2);
        thisMaterial->TransVis = MaterialProps(3);
        thisMaterial->ReflectShadeVis = MaterialProps(4);
        thisMaterial->AbsorpThermal = MaterialProps(5);
        thisMaterial->AbsorpThermalInput = MaterialProps(5);
        thisMaterial->TransThermal = MaterialProps(6);
        thisMaterial->Thickness = MaterialProps(7);
        thisMaterial->Conductivity = MaterialProps(8);
        thisMaterial->AbsorpSolar = max(0.0, 1.0 - thisMaterial->Trans - thisMaterial->ReflectShade);
        thisMaterial->AbsorpSolarInput = thisMaterial->AbsorpSolar;
        thisMaterial->WinShadeToGlassDist = MaterialProps(9);
        thisMaterial->WinShadeTopOpeningMult = MaterialProps(10);
        thisMaterial->WinShadeBottomOpeningMult = MaterialProps(11);
        thisMaterial->WinShadeLeftOpeningMult = MaterialProps(12);
        thisMaterial->WinShadeRightOpeningMult = MaterialProps(13);
        thisMaterial->WinShadeAirFlowPermeability = MaterialProps(14);
        thisMaterial->ROnly = true;

        if (thisMaterial->Conductivity > 0.0) {
            state.dataHeatBal->NominalR(MaterNum) = thisMaterial->Thickness / thisMaterial->Conductivity;
        } else {
            state.dataHeatBal->NominalR(MaterNum) = 1.0;
        }

        if (MaterialProps(1) + MaterialProps(2) >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
            ShowContinueError(state,
                              ipsc->cNumericFieldNames(1) + " + " + ipsc->cNumericFieldNames(2) + " not < 1.0");
        }

        if (MaterialProps(3) + MaterialProps(4) >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
            ShowContinueError(state,
                              ipsc->cNumericFieldNames(3) + " + " + ipsc->cNumericFieldNames(4) + " not < 1.0");
        }

        if (MaterialProps(5) + MaterialProps(6) >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
            ShowContinueError(state,
                              ipsc->cNumericFieldNames(5) + " + " + ipsc->cNumericFieldNames(6) + " not < 1.0");
        }
    }

    // Window Shade Materials

    state.dataHeatBalMgr->CurrentModuleObject = "WindowMaterial:Shade:EquivalentLayer";
    for (Loop = 1; Loop <= state.dataHeatBal->TotShadesEQL; ++Loop) {

        MaterialProps = 0;

        // Call Input Get routine to retrieve material data
        ip->getObjectItem(state,
                          state.dataHeatBalMgr->CurrentModuleObject,
                          Loop,
                          MaterialNames,
                          MaterialNumAlpha,
                          MaterialProps,
                          MaterialNumProp,
                          IOStat,
                          ipsc->lNumericFieldBlanks,
                          ipsc->lAlphaFieldBlanks,
                          ipsc->cAlphaFieldNames,
                          ipsc->cNumericFieldNames);
        if (GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataHeatBalMgr->UniqueMaterialNames,
                                                     MaterialNames(1),
                                                     state.dataHeatBalMgr->CurrentModuleObject,
                                                     ipsc->cAlphaFieldNames(1),
                                                     ErrorsFound)) {
            ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
            continue;
        }

        ++MaterNum;
        auto *thisMaterial = new MaterialChild;
        state.dataMaterial->Material(MaterNum) = thisMaterial;
        thisMaterial->group = Group::ShadeEquivalentLayer;

        thisMaterial->Name = MaterialNames(1);
        thisMaterial->Roughness = SurfaceRoughness::MediumRough;
        thisMaterial->ROnly = true;

        //  Front side and back side have the same beam-Beam Transmittance
        thisMaterial->TausFrontBeamBeam = MaterialProps(1);
        thisMaterial->TausBackBeamBeam = MaterialProps(1);
        thisMaterial->TausFrontBeamDiff = MaterialProps(2);
        thisMaterial->TausBackBeamDiff = MaterialProps(3);
        thisMaterial->ReflFrontBeamDiff = MaterialProps(4);
        thisMaterial->ReflBackBeamDiff = MaterialProps(5);
        thisMaterial->TausFrontBeamBeamVis = MaterialProps(6);
        thisMaterial->TausFrontBeamDiffVis = MaterialProps(7);
        thisMaterial->ReflFrontBeamDiffVis = MaterialProps(8);
        thisMaterial->TausThermal = MaterialProps(9);
        thisMaterial->EmissThermalFront = MaterialProps(10);
        thisMaterial->EmissThermalBack = MaterialProps(11);
        // Assumes thermal emissivity is the same as thermal absorptance
        thisMaterial->AbsorpThermalFront = thisMaterial->EmissThermalFront;
        thisMaterial->AbsorpThermalBack = thisMaterial->EmissThermalBack;
        thisMaterial->TransThermal = thisMaterial->TausThermal;

        if (MaterialProps(1) + MaterialProps(2) + MaterialProps(4) >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
            ShowContinueError(state,
                              ipsc->cNumericFieldNames(1) + " + " + ipsc->cNumericFieldNames(2) + " + " +
                                  ipsc->cNumericFieldNames(4) + "not < 1.0");
        }
        if (MaterialProps(1) + MaterialProps(3) + MaterialProps(5) >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
            ShowContinueError(state,
                              ipsc->cNumericFieldNames(1) + " + " + ipsc->cNumericFieldNames(3) + " + " +
                                  ipsc->cNumericFieldNames(5) + "not < 1.0");
        }
        if (MaterialProps(6) + MaterialProps(7) + MaterialProps(8) >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
            ShowContinueError(state,
                              ipsc->cNumericFieldNames(6) + " + " + ipsc->cNumericFieldNames(7) + " + " +
                                  ipsc->cNumericFieldNames(8) + "not < 1.0");
        }
        if (MaterialProps(9) + MaterialProps(10) >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
            ShowContinueError(state,
                              ipsc->cNumericFieldNames(9) + " + " + ipsc->cNumericFieldNames(10) + " not < 1.0");
        }
        if (MaterialProps(9) + MaterialProps(11) >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
            ShowContinueError(state,
                              ipsc->cNumericFieldNames(9) + " + " + ipsc->cNumericFieldNames(11) + " not < 1.0");
        }

    } // TotShadesEQL loop

    // Window drape materials

    state.dataHeatBalMgr->CurrentModuleObject = "WindowMaterial:Drape:EquivalentLayer";
    for (Loop = 1; Loop <= state.dataHeatBal->TotDrapesEQL; ++Loop) {

        MaterialProps = 0;

        // Call Input Get routine to retrieve material data
        ip->getObjectItem(state,
                          state.dataHeatBalMgr->CurrentModuleObject,
                          Loop,
                          MaterialNames,
                          MaterialNumAlpha,
                          MaterialProps,
                          MaterialNumProp,
                          IOStat,
                          ipsc->lNumericFieldBlanks,
                          ipsc->lAlphaFieldBlanks,
                          ipsc->cAlphaFieldNames,
                          ipsc->cNumericFieldNames);
        if (GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataHeatBalMgr->UniqueMaterialNames,
                                                     MaterialNames(1),
                                                     state.dataHeatBalMgr->CurrentModuleObject,
                                                     ipsc->cAlphaFieldNames(1),
                                                     ErrorsFound)) {
            ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
            continue;
        }

        ++MaterNum;
        auto *thisMaterial = new MaterialChild;
        state.dataMaterial->Material(MaterNum) = thisMaterial;
        thisMaterial->group = Group::DrapeEquivalentLayer;

        thisMaterial->Name = MaterialNames(1);
        thisMaterial->Roughness = SurfaceRoughness::MediumRough;
        thisMaterial->ROnly = true;

        //  Front side and back side have the same properties
        thisMaterial->TausFrontBeamBeam = MaterialProps(1);
        thisMaterial->TausBackBeamBeam = MaterialProps(1);

        thisMaterial->TausFrontBeamDiff = MaterialProps(2);
        thisMaterial->TausBackBeamDiff = MaterialProps(3);

        thisMaterial->ReflFrontBeamDiff = MaterialProps(4);
        thisMaterial->ReflBackBeamDiff = MaterialProps(5);
        thisMaterial->TausFrontBeamBeamVis = MaterialProps(6);
        thisMaterial->TausFrontBeamDiffVis = MaterialProps(7);
        thisMaterial->ReflFrontBeamDiffVis = MaterialProps(8);
        thisMaterial->TausThermal = MaterialProps(9);
        thisMaterial->EmissThermalFront = MaterialProps(10);
        thisMaterial->EmissThermalBack = MaterialProps(11);
        // Assumes thermal emissivity is the same as thermal absorptance
        thisMaterial->AbsorpThermalFront = thisMaterial->EmissThermalFront;
        thisMaterial->AbsorpThermalBack = thisMaterial->EmissThermalBack;
        thisMaterial->TransThermal = thisMaterial->TausThermal;

        if (!ipsc->lNumericFieldBlanks(12) && !ipsc->lNumericFieldBlanks(13)) {
            if (MaterialProps(12) != 0.0 && MaterialProps(13) != 0.0) {
                thisMaterial->PleatedDrapeWidth = MaterialProps(12);
                thisMaterial->PleatedDrapeLength = MaterialProps(13);
                thisMaterial->ISPleatedDrape = true;
            }
        } else {
            thisMaterial->ISPleatedDrape = false;
        }
        if (MaterialProps(1) + MaterialProps(2) + MaterialProps(4) >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
            ShowContinueError(state,
                              ipsc->cNumericFieldNames(1) + " + " + ipsc->cNumericFieldNames(2) + " + " +
                                  ipsc->cNumericFieldNames(4) + "not < 1.0");
        }
        if (MaterialProps(6) + MaterialProps(7) + MaterialProps(8) >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
            ShowContinueError(state,
                              ipsc->cNumericFieldNames(4) + " + " + ipsc->cNumericFieldNames(5) + " + " +
                                  ipsc->cNumericFieldNames(6) + "not < 1.0");
        }
        if (MaterialProps(9) + MaterialProps(10) > 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
            ShowContinueError(state,
                              ipsc->cNumericFieldNames(9) + " + " + ipsc->cNumericFieldNames(10) + " not < 1.0");
        }

    } // TotDrapesEQL loop

    // Window Screen Materials

    state.dataHeatBalMgr->CurrentModuleObject = "WindowMaterial:Screen";
    for (Loop = 1; Loop <= state.dataHeatBal->TotScreens; ++Loop) {

        // Call GetObjectItem routine to retrieve material data
        ip->getObjectItem(state,
                          state.dataHeatBalMgr->CurrentModuleObject,
                          Loop,
                          MaterialNames,
                          MaterialNumAlpha,
                          MaterialProps,
                          MaterialNumProp,
                          IOStat,
                          ipsc->lNumericFieldBlanks,
                          ipsc->lAlphaFieldBlanks,
                          ipsc->cAlphaFieldNames,
                          ipsc->cNumericFieldNames);
        if (GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataHeatBalMgr->UniqueMaterialNames,
                                                     MaterialNames(1),
                                                     state.dataHeatBalMgr->CurrentModuleObject,
                                                     ipsc->cAlphaFieldNames(1),
                                                     ErrorsFound)) {
            ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
            continue;
        }

        ++MaterNum;
        auto *matScreen = new MaterialScreen;
        state.dataMaterial->Material(MaterNum) = matScreen;

        // Load the material derived type from the input data.

        matScreen->Name = MaterialNames(1);
        matScreen->bmRefModel = static_cast<ScreenBeamReflectanceModel>(getEnumValue(screenBeamReflectanceModelNamesUC, Util::makeUPPER(MaterialNames(2))));
        if (matScreen->bmRefModel == ScreenBeamReflectanceModel::Invalid) {
            ShowSevereError(state, format("{}=\"{}\", Illegal value.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
            ShowContinueError(state, format("{}=\"{}\", must be one of DoNotModel, ModelAsDirectBeam or ModelAsDiffuse.",
                                            ipsc->cAlphaFieldNames(2), MaterialNames(2)));
            ErrorsFound = true;
        }
        matScreen->Roughness = SurfaceRoughness::MediumRough;
        matScreen->ShadeRef = MaterialProps(1);
        if (matScreen->ShadeRef < 0.0 || matScreen->ShadeRef > 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
            ShowContinueError(state, ipsc->cNumericFieldNames(1) + " must be >= 0 and <= 1");
        }
        matScreen->ShadeRefVis = MaterialProps(2);
        if (matScreen->ShadeRefVis < 0.0 || matScreen->ShadeRefVis > 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
            ShowContinueError(state, ipsc->cNumericFieldNames(2) + " must be >= 0 and <= 1 for material " + matScreen->Name + '.');
        }
        matScreen->AbsorpThermal = MaterialProps(3);
        matScreen->AbsorpThermalInput = MaterialProps(3);
        if (matScreen->AbsorpThermal < 0.0 || matScreen->AbsorpThermal > 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
            ShowContinueError(state, ipsc->cNumericFieldNames(3) + " must be >= 0 and <= 1");
        }
        matScreen->Conductivity = MaterialProps(4);
        matScreen->Thickness = MaterialProps(6); // thickness = diameter

        if (MaterialProps(5) > 0.0) {
            //      Screens(ScNum)%ScreenDiameterToSpacingRatio = MaterialProps(6)/MaterialProps(5) or
            //      1-SQRT(dataMaterial.Material(MaterNum)%Trans
            if (MaterialProps(6) / MaterialProps(5) >= 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state,
                                  ipsc->cNumericFieldNames(6) + " must be less than " + ipsc->cNumericFieldNames(5));
            } else {
                //       Calculate direct normal transmittance (open area fraction)
                matScreen->Trans = pow_2(1.0 - MaterialProps(6) / MaterialProps(5));
            }
        } else {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
            ShowContinueError(state, ipsc->cNumericFieldNames(5) + " must be > 0.");
            MaterialProps(5) = 0.000000001;
        }

        if (MaterialProps(6) <= 0.0) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
            ShowContinueError(state, ipsc->cNumericFieldNames(6) + " must be > 0.");
        }

        //   Modify reflectance to account for the open area in the screen assembly
        matScreen->ShadeRef *= (1.0 - matScreen->Trans);
        matScreen->ShadeRefVis *= (1.0 - matScreen->Trans);

        matScreen->toGlassDist = MaterialProps(7);
        if (matScreen->toGlassDist < 0.001 || matScreen->toGlassDist > 1.0) {
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
            ShowContinueError(state,
                              ipsc->cNumericFieldNames(7) + " must be greater than or equal to 0.001 and less than or equal to 1.");
        }

        matScreen->topOpeningMult = MaterialProps(8);
        if (matScreen->topOpeningMult < 0.0 || matScreen->topOpeningMult > 1.0) {
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
            ShowContinueError(state,
                              ipsc->cNumericFieldNames(8) + " must be greater than or equal to 0 and less than or equal to 1.");
        }

        matScreen->bottomOpeningMult = MaterialProps(9);
        if (matScreen->bottomOpeningMult < 0.0 || matScreen->bottomOpeningMult > 1.0) {
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
            ShowContinueError(state,
                              ipsc->cNumericFieldNames(9) + " must be greater than or equal to 0 and less than or equal to 1.");
        }

        matScreen->leftOpeningMult = MaterialProps(10);
        if (matScreen->leftOpeningMult < 0.0 || matScreen->leftOpeningMult > 1.0) {
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
            ShowContinueError(state,
                              ipsc->cNumericFieldNames(10) + " must be greater than or equal to 0 and less than or equal to 1.");
        }

        matScreen->rightOpeningMult = MaterialProps(11);
        if (matScreen->rightOpeningMult < 0.0 || matScreen->rightOpeningMult > 1.0) {
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
            ShowContinueError(state,
                              ipsc->cNumericFieldNames(11) + " must be greater than or equal to 0 and less than or equal to 1.");
        }

        matScreen->mapDegResolution = MaterialProps(12);
        if (matScreen->mapDegResolution < 0 || matScreen->mapDegResolution > 5 || matScreen->mapDegResolution == 4) {
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
            ShowContinueError(state, ipsc->cNumericFieldNames(12) + " must be 0, 1, 2, 3, or 5.");
            ErrorsFound = true;
        }

        //   Default air flow permeability to open area fraction
        matScreen->airFlowPermeability = matScreen->Trans;
        matScreen->TransThermal = matScreen->Trans;
        matScreen->TransVis = matScreen->Trans;

        matScreen->ROnly = true;

        //   Calculate absorptance accounting for the open area in the screen assembly (used only in CreateShadedWindowConstruction)
        matScreen->AbsorpSolar = max(0.0, 1.0 - matScreen->Trans - matScreen->ShadeRef);
        matScreen->AbsorpSolarInput = matScreen->AbsorpSolar;
        matScreen->AbsorpVisible = max(0.0, 1.0 - matScreen->TransVis - matScreen->ShadeRefVis);
        matScreen->AbsorpVisibleInput = matScreen->AbsorpVisible;
        matScreen->AbsorpThermal *= (1.0 - matScreen->Trans);
        matScreen->AbsorpThermalInput = matScreen->AbsorpThermal;

        if (matScreen->Conductivity > 0.0) {
            state.dataHeatBal->NominalR(MaterNum) = (1.0 - matScreen->Trans) * matScreen->Thickness / matScreen->Conductivity;
        } else {
            state.dataHeatBal->NominalR(MaterNum) = 1.0;
            ShowWarningError(
                state,
                "Conductivity for material=\"" + matScreen->Name +
                    "\" must be greater than 0 for calculating Nominal R-value, Nominal R is defaulted to 1 and the simulation continues.");
        }

        if (matScreen->Trans + matScreen->ShadeRef >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
            ShowContinueError(state, "Calculated solar transmittance + solar reflectance not < 1.0");
            ShowContinueError(state, "See Engineering Reference for calculation procedure for solar transmittance.");
        }

        if (matScreen->TransVis + matScreen->ShadeRefVis >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
            ShowContinueError(state, "Calculated visible transmittance + visible reflectance not < 1.0");
            ShowContinueError(state, "See Engineering Reference for calculation procedure for visible solar transmittance.");
        }

        if (matScreen->TransThermal + matScreen->AbsorpThermal >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
            ShowSevereError(state, "Thermal hemispherical emissivity plus open area fraction (1-diameter/spacing)**2 not < 1.0");
        }
    }

    state.dataHeatBalMgr->CurrentModuleObject = "WindowMaterial:Screen:EquivalentLayer";
    for (Loop = 1; Loop <= state.dataHeatBal->TotScreensEQL; ++Loop) {

        MaterialProps = 0;

        // Call GetObjectItem routine to retrieve material data
        ip->getObjectItem(state,
                          state.dataHeatBalMgr->CurrentModuleObject,
                          Loop,
                          MaterialNames,
                          MaterialNumAlpha,
                          MaterialProps,
                          MaterialNumProp,
                          IOStat,
                          ipsc->lNumericFieldBlanks,
                          ipsc->lAlphaFieldBlanks,
                          ipsc->cAlphaFieldNames,
                          ipsc->cNumericFieldNames);
        if (GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataHeatBalMgr->UniqueMaterialNames,
                                                     MaterialNames(1),
                                                     state.dataHeatBalMgr->CurrentModuleObject,
                                                     ipsc->cAlphaFieldNames(1),
                                                     ErrorsFound)) {
            ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
            continue;
        }

        ++MaterNum;
        auto *matScreen = new MaterialChild;
        state.dataMaterial->Material(MaterNum) = matScreen;
        matScreen->group = Group::ScreenEquivalentLayer;

        // Load the material derived type from the input data.
        // WindowMaterial:Screen:EquivalentLayer,
        matScreen->Name = MaterialNames(1);
        matScreen->Roughness = SurfaceRoughness::MediumRough;
        matScreen->ROnly = true;
        matScreen->TausFrontBeamBeam = MaterialProps(1);
        matScreen->TausBackBeamBeam = MaterialProps(1);
        matScreen->TausFrontBeamDiff = MaterialProps(2);
        matScreen->TausBackBeamDiff = MaterialProps(2);
        matScreen->ReflFrontBeamDiff = MaterialProps(3);
        matScreen->ReflBackBeamDiff = MaterialProps(3);
        matScreen->TausFrontBeamBeamVis = MaterialProps(4);
        matScreen->TausFrontBeamDiffVis = MaterialProps(5);
        matScreen->ReflFrontDiffDiffVis = MaterialProps(6);
        matScreen->TausThermal = MaterialProps(7);
        matScreen->EmissThermalFront = MaterialProps(8);
        matScreen->EmissThermalBack = MaterialProps(8);

        // Assumes thermal emissivity is the same as thermal absorptance
        matScreen->AbsorpThermalFront = matScreen->EmissThermalFront;
        matScreen->AbsorpThermalBack = matScreen->EmissThermalBack;
        matScreen->TransThermal = matScreen->TausThermal;

        if (MaterialProps(3) < 0.0 || MaterialProps(3) > 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
            ShowContinueError(state, ipsc->cNumericFieldNames(3) + " must be >= 0 and <= 1");
        }

        if (MaterialProps(6) < 0.0 || MaterialProps(6) > 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
            ShowContinueError(state, ipsc->cNumericFieldNames(6) + " must be >= 0 and <= 1 for material " + matScreen->Name + '.');
        }

        if (!ipsc->lNumericFieldBlanks(9)) {
            if (MaterialProps(9) > 0.00001) {
                matScreen->ScreenWireSpacing = MaterialProps(9); // screen wire spacing
            } else {
                ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                ShowContinueError(state, ipsc->cNumericFieldNames(9) + " must be > 0.");
                ShowContinueError(state, "...Setting screen wire spacing to a default value of 0.025m and simulation continues.");
                matScreen->ScreenWireSpacing = 0.025;
            }
        }

        if (!ipsc->lNumericFieldBlanks(10)) {
            if (MaterialProps(10) > 0.00001 && MaterialProps(10) < matScreen->ScreenWireSpacing) {
                matScreen->ScreenWireDiameter = MaterialProps(10); // screen wire spacing
            } else {
                ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value.");
                ShowContinueError(state, ipsc->cNumericFieldNames(10) + " must be > 0.");
                ShowContinueError(state, "...Setting screen wire diameter to a default value of 0.005m and simulation continues.");
                matScreen->ScreenWireDiameter = 0.005;
            }
        }

        if (matScreen->ScreenWireSpacing > 0.0) {
            if (matScreen->ScreenWireDiameter / matScreen->ScreenWireSpacing >= 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state,
                                  ipsc->cNumericFieldNames(10) + " must be less than " + ipsc->cNumericFieldNames(9));
            } else {
                //  Calculate direct normal transmittance (open area fraction)
                Openness = pow_2(1.0 - matScreen->ScreenWireDiameter / matScreen->ScreenWireSpacing);
                if ((matScreen->TausFrontBeamBeam - Openness) / Openness > 0.01) {
                    ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", screen openness specified.");
                    ShowContinueError(state, ipsc->cNumericFieldNames(1) + " is > 1.0% of the value calculated from input fields:");
                    ShowContinueError(state, ipsc->cNumericFieldNames(9) + " and " + (ipsc->cNumericFieldNames(10)));
                    ShowContinueError(state, " using the formula (1-diameter/spacing)**2");
                    ShowContinueError(state, " ...the screen diameter is recalculated from the material openness specified ");
                    ShowContinueError(state, " ...and wire spacing using the formula = wire spacing * (1.0 - SQRT(Opennes))");
                    matScreen->ScreenWireDiameter = matScreen->ScreenWireSpacing * (1.0 - std::sqrt(matScreen->TausFrontBeamBeam));
                    ShowContinueError(
                        state,
                        format(" ...Recalculated {}={:.4R} m", ipsc->cNumericFieldNames(10), matScreen->ScreenWireDiameter));
                }
            }
        }

        if (matScreen->TausFrontBeamBeam + matScreen->ReflFrontBeamDiff >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
            ShowContinueError(state, "Calculated solar transmittance + solar reflectance not < 1.0");
            ShowContinueError(state, "See Engineering Reference for calculation procedure for solar transmittance.");
        }

        if (matScreen->TausFrontBeamBeamVis + matScreen->ReflFrontDiffDiffVis >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
            ShowContinueError(state, "Calculated visible transmittance + visible reflectance not < 1.0");
            ShowContinueError(state, "See Engineering Reference for calculation procedure for visible solar transmittance.");
        }
        if (matScreen->TransThermal + matScreen->AbsorpThermal >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
            ShowSevereError(state, "Thermal hemispherical emissivity plus open area fraction (1-diameter/spacing)**2 not < 1.0");
        }

    } // TotScreensEQL loop

    // Window Blind Materials
    if ((state.dataHeatBal->TotBlindsEQL == 0) && (state.dataHeatBal->TotBlinds == 0)) {
        state.dataSurface->actualMaxSlatAngs = 1; // first slot is used for shades
    }

    if (state.dataHeatBal->TotBlinds > 0) {
        state.dataMaterial->Blind.allocate(state.dataHeatBal->TotBlinds); // Allocate the array Size to the number of blinds
    }

    state.dataHeatBalMgr->CurrentModuleObject = "WindowMaterial:Blind";
    for (Loop = 1; Loop <= state.dataHeatBal->TotBlinds; ++Loop) {

        // Call Input Get routine to retrieve material data
        ip->getObjectItem(state,
                          state.dataHeatBalMgr->CurrentModuleObject,
                          Loop,
                          MaterialNames,
                          MaterialNumAlpha,
                          MaterialProps,
                          MaterialNumProp,
                          IOStat,
                          ipsc->lNumericFieldBlanks,
                          ipsc->lAlphaFieldBlanks,
                          ipsc->cAlphaFieldNames,
                          ipsc->cNumericFieldNames);
        if (GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataHeatBalMgr->UniqueMaterialNames,
                                                     MaterialNames(1),
                                                     state.dataHeatBalMgr->CurrentModuleObject,
                                                     ipsc->cAlphaFieldNames(1),
                                                     ErrorsFound)) {
            ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
            continue;
        }

        ++MaterNum;
        auto *thisMaterial = new MaterialChild;
        state.dataMaterial->Material(MaterNum) = thisMaterial;
        thisMaterial->group = Group::WindowBlind;

        // Load the material derived type from the input data.

        thisMaterial->Name = MaterialNames(1);
        state.dataMaterial->Blind(Loop).Name = MaterialNames(1);
        thisMaterial->Roughness = SurfaceRoughness::Rough;
        thisMaterial->BlindDataPtr = Loop;
        thisMaterial->ROnly = true;

        state.dataMaterial->Blind(Loop).MaterialNumber = MaterNum;
        if (Util::SameString(MaterialNames(2), "Horizontal")) {
            state.dataMaterial->Blind(Loop).SlatOrientation = DataWindowEquivalentLayer::Orientation::Horizontal;
        } else if (Util::SameString(MaterialNames(2), "Vertical")) {
            state.dataMaterial->Blind(Loop).SlatOrientation = DataWindowEquivalentLayer::Orientation::Vertical;
        }
        state.dataMaterial->Blind(Loop).SlatWidth = MaterialProps(1);
        state.dataMaterial->Blind(Loop).SlatSeparation = MaterialProps(2);
        state.dataMaterial->Blind(Loop).SlatThickness = MaterialProps(3);
        state.dataMaterial->Blind(Loop).SlatAngle = MaterialProps(4);
        state.dataMaterial->Blind(Loop).SlatConductivity = MaterialProps(5);
        state.dataMaterial->Blind(Loop).SlatTransSolBeamDiff = MaterialProps(6);
        state.dataMaterial->Blind(Loop).SlatFrontReflSolBeamDiff = MaterialProps(7);
        state.dataMaterial->Blind(Loop).SlatBackReflSolBeamDiff = MaterialProps(8);
        state.dataMaterial->Blind(Loop).SlatTransSolDiffDiff = MaterialProps(9);
        state.dataMaterial->Blind(Loop).SlatFrontReflSolDiffDiff = MaterialProps(10);
        state.dataMaterial->Blind(Loop).SlatBackReflSolDiffDiff = MaterialProps(11);
        state.dataMaterial->Blind(Loop).SlatTransVisBeamDiff = MaterialProps(12);
        state.dataMaterial->Blind(Loop).SlatFrontReflVisBeamDiff = MaterialProps(13);
        state.dataMaterial->Blind(Loop).SlatBackReflVisBeamDiff = MaterialProps(14);
        state.dataMaterial->Blind(Loop).SlatTransVisDiffDiff = MaterialProps(15);
        state.dataMaterial->Blind(Loop).SlatFrontReflVisDiffDiff = MaterialProps(16);
        state.dataMaterial->Blind(Loop).SlatBackReflVisDiffDiff = MaterialProps(17);
        state.dataMaterial->Blind(Loop).SlatTransIR = MaterialProps(18);
        state.dataMaterial->Blind(Loop).SlatFrontEmissIR = MaterialProps(19);
        state.dataMaterial->Blind(Loop).SlatBackEmissIR = MaterialProps(20);
        state.dataMaterial->Blind(Loop).BlindToGlassDist = MaterialProps(21);
        state.dataMaterial->Blind(Loop).BlindTopOpeningMult = MaterialProps(22);
        state.dataMaterial->Blind(Loop).BlindBottomOpeningMult = MaterialProps(23);
        state.dataMaterial->Blind(Loop).BlindLeftOpeningMult = MaterialProps(24);
        state.dataMaterial->Blind(Loop).BlindRightOpeningMult = MaterialProps(25);
        state.dataMaterial->Blind(Loop).MinSlatAngle = MaterialProps(26);
        state.dataMaterial->Blind(Loop).MaxSlatAngle = MaterialProps(27);

        // TH 2/11/2010. For CR 8010
        // By default all blinds have fixed slat angle, new blinds with variable slat angle are created if
        //  they are used with window shading controls that adjust slat angles like ScheduledSlatAngle or BlockBeamSolar
        state.dataMaterial->Blind(Loop).SlatAngleType = DataWindowEquivalentLayer::AngleType::Fixed;

        if (state.dataMaterial->Blind(Loop).SlatWidth < state.dataMaterial->Blind(Loop).SlatSeparation) {
            ShowWarningError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Slat Angles/Widths");
            ShowContinueError(state,
                              format("{} [{:.2R}] is less than {} [{:.2R}].",
                                     ipsc->cNumericFieldNames(1),
                                     state.dataMaterial->Blind(Loop).SlatWidth,
                                     ipsc->cNumericFieldNames(2),
                                     state.dataMaterial->Blind(Loop).SlatSeparation));
            ShowContinueError(state, "This will allow direct beam to be transmitted when Slat angle = 0.");
        }

        if (!Util::SameString(MaterialNames(2), "Horizontal") && !Util::SameString(MaterialNames(2), "Vertical")) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value");
            ShowContinueError(state, ipsc->cAlphaFieldNames(2) + "=\"" + MaterialNames(2) + "\", must be Horizontal or Vertical.");
        }

        if ((MaterialProps(6) + MaterialProps(7) >= 1.0)) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
            ShowContinueError(state,
                              ipsc->cNumericFieldNames(6) + " + " + ipsc->cNumericFieldNames(7) + " not < 1.0");
        }
        if ((MaterialProps(6) + MaterialProps(8) >= 1.0)) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
            ShowContinueError(state,
                              ipsc->cNumericFieldNames(6) + " + " + ipsc->cNumericFieldNames(8) + " not < 1.0");
        }

        if ((MaterialProps(9) + MaterialProps(10) >= 1.0)) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
            ShowContinueError(state,
                              ipsc->cNumericFieldNames(9) + " + " + ipsc->cNumericFieldNames(10) + " not < 1.0");
        }
        if ((MaterialProps(9) + MaterialProps(11) >= 1.0)) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
            ShowContinueError(state,
                              ipsc->cNumericFieldNames(9) + " + " + ipsc->cNumericFieldNames(11) + " not < 1.0");
        }

        if ((MaterialProps(12) + MaterialProps(13) >= 1.0) || (MaterialProps(12) + MaterialProps(14) >= 1.0)) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
            ShowContinueError(state,
                              ipsc->cNumericFieldNames(12) + " + " + ipsc->cNumericFieldNames(13) + " not < 1.0 OR");
            ShowContinueError(state,
                              ipsc->cNumericFieldNames(12) + " + " + ipsc->cNumericFieldNames(14) + " not < 1.0");
        }

        if ((MaterialProps(12) + MaterialProps(13) >= 1.0)) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
            ShowContinueError(state,
                              ipsc->cNumericFieldNames(12) + " + " + ipsc->cNumericFieldNames(13) + " not < 1.0");
        }
        if ((MaterialProps(12) + MaterialProps(14) >= 1.0)) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
            ShowContinueError(state,
                              ipsc->cNumericFieldNames(12) + " + " + ipsc->cNumericFieldNames(14) + " not < 1.0");
        }

        if ((MaterialProps(15) + MaterialProps(16) >= 1.0)) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
            ShowContinueError(state,
                              ipsc->cNumericFieldNames(15) + " + " + ipsc->cNumericFieldNames(16) + " not < 1.0");
        }
        if ((MaterialProps(15) + MaterialProps(17) >= 1.0)) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
            ShowContinueError(state,
                              ipsc->cNumericFieldNames(15) + " + " + ipsc->cNumericFieldNames(17) + " not < 1.0");
        }

        // Require that beam and diffuse properties be the same
        if (std::abs(MaterialProps(9) - MaterialProps(6)) > 1.e-5) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
            ShowContinueError(state, ipsc->cNumericFieldNames(6) + " must equal " + ipsc->cNumericFieldNames(9));
        }

        if (std::abs(MaterialProps(10) - MaterialProps(7)) > 1.e-5) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
            ShowContinueError(state, ipsc->cNumericFieldNames(7) + " must equal " + ipsc->cNumericFieldNames(10));
        }

        if (std::abs(MaterialProps(11) - MaterialProps(8)) > 1.e-5) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
            ShowContinueError(state, ipsc->cNumericFieldNames(8) + " must equal " + ipsc->cNumericFieldNames(11));
        }

        if (std::abs(MaterialProps(15) - MaterialProps(12)) > 1.e-5) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
            ShowContinueError(state, ipsc->cNumericFieldNames(12) + " must equal " + ipsc->cNumericFieldNames(15));
        }

        if (std::abs(MaterialProps(16) - MaterialProps(13)) > 1.e-5) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
            ShowContinueError(state, ipsc->cNumericFieldNames(13) + " must equal " + ipsc->cNumericFieldNames(16));
        }

        if (std::abs(MaterialProps(17) - MaterialProps(14)) > 1.e-5) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
            ShowContinueError(state, ipsc->cNumericFieldNames(14) + " must equal " + ipsc->cNumericFieldNames(17));
        }

        if ((MaterialProps(18) + MaterialProps(19) >= 1.0)) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
            ShowContinueError(state,
                              ipsc->cNumericFieldNames(18) + " + " + ipsc->cNumericFieldNames(19) + " not < 1.0");
        }
        if ((MaterialProps(18) + MaterialProps(20) >= 1.0)) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
            ShowContinueError(state,
                              ipsc->cNumericFieldNames(18) + " + " + ipsc->cNumericFieldNames(20) + " not < 1.0");
        }

        if (state.dataMaterial->Blind(Loop).BlindToGlassDist < 0.5 * state.dataMaterial->Blind(Loop).SlatWidth) {
            ErrorsFound = true;
            ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
            ShowContinueError(
                state, ipsc->cNumericFieldNames(21) + " is less than half of the " + ipsc->cNumericFieldNames(1));
        }

        // Minimum and maximum slat angles allowed by slat geometry
        if (state.dataMaterial->Blind(Loop).SlatWidth > state.dataMaterial->Blind(Loop).SlatSeparation) {
            MinSlatAngGeom = std::asin(state.dataMaterial->Blind(Loop).SlatThickness /
                                       (state.dataMaterial->Blind(Loop).SlatThickness + state.dataMaterial->Blind(Loop).SlatSeparation)) /
                             Constant::DegToRadians;
        } else {
            MinSlatAngGeom = 0.0;
        }
        MaxSlatAngGeom = 180.0 - MinSlatAngGeom;

        // Error if input slat angle not in range allowed by slat geometry
        if ((state.dataMaterial->Blind(Loop).SlatSeparation + state.dataMaterial->Blind(Loop).SlatThickness) <
            state.dataMaterial->Blind(Loop).SlatWidth) {
            if (state.dataMaterial->Blind(Loop).SlatAngle < MinSlatAngGeom) {
                ErrorsFound = true;
                ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state,
                                  format("{}=[{:.1R}], is less than smallest allowed by slat dimensions and spacing, [{:.1R}] deg.",
                                         ipsc->cNumericFieldNames(4),
                                         state.dataMaterial->Blind(Loop).SlatAngle,
                                         MinSlatAngGeom));
            } else if (state.dataMaterial->Blind(Loop).SlatAngle > MaxSlatAngGeom) {
                ErrorsFound = true;
                ShowSevereError(state, state.dataHeatBalMgr->CurrentModuleObject + "=\"" + MaterialNames(1) + "\", Illegal value combination.");
                ShowContinueError(state,
                                  format("{}=[{:.1R}], is greater than largest allowed by slat dimensions and spacing, [{:.1R}] deg.",
                                         ipsc->cNumericFieldNames(4),
                                         state.dataMaterial->Blind(Loop).SlatAngle,
                                         MinSlatAngGeom));
            }
        }

        // By default all Blinds are "fixed" slats.  Only with Shading Control is one considered variable and this check
        // is now done when that happens.  9.3.2009 LKL

        //    IF(Blind(Loop)%SlatAngleType == VariableSlats) THEN
        //      ! Error if maximum slat angle less than minimum
        //      IF(Blind(Loop)%MaxSlatAngle < Blind(Loop)%MinSlatAngle) THEN
        //        ErrorsFound = .TRUE.
        //        CALL ShowSevereError(state, TRIM(state.dataHeatBalMgr->CurrentModuleObject)//'="'//TRIM(MaterialNames(1))//'", Illegal value
        //        combination.') CALL ShowContinueError(state,
        //        TRIM(cNumericFieldNames(26))//'=['//TRIM(RoundSigDigits(Blind(Loop)%MinSlatAngle,1))//  &
        //           '], is greater than '//TRIM(cNumericFieldNames(27))//'=['//  &
        //           TRIM(RoundSigDigits(Blind(Loop)%MaxSlatAngle,1))//'] deg.')
        //      END IF
        //      ! Error if input slat angle not in input min/max range
        //      IF(Blind(Loop)%MaxSlatAngle > Blind(Loop)%MinSlatAngle .AND. (Blind(Loop)%SlatAngle < Blind(Loop)%MinSlatAngle &
        //          .OR. Blind(Loop)%SlatAngle > Blind(Loop)%MaxSlatAngle)) THEN
        //        ErrorsFound = .TRUE.
        //        CALL ShowSevereError(state, TRIM(state.dataHeatBalMgr->CurrentModuleObject)//'="'//TRIM(MaterialNames(1))//'", Illegal value
        //        combination.') CALL ShowContinueError(state, TRIM(cNumericFieldNames(4))//'=['//TRIM(RoundSigDigits(Blind(Loop)%SlatAngle,1))//
        //        &
        //           '] is outside of the input min/max range, min=['//TRIM(RoundSigDigits(Blind(Loop)%MinSlatAngle,1))//  &
        //           '], max=['//TRIM(RoundSigDigits(Blind(Loop)%MaxSlatAngle,1))//'] deg.')
        //      END IF
        //      ! Error if input minimum slat angle is less than that allowed by slat geometry
        //      IF(Blind(Loop)%MinSlatAngle < MinSlatAngGeom) THEN
        //        CALL ShowSevereError(state, TRIM(state.dataHeatBalMgr->CurrentModuleObject)//'="'//TRIM(MaterialNames(1))//'", Illegal value
        //        combination.') CALL ShowContinueError(state,
        //        TRIM(cNumericFieldNames(26))//'=['//TRIM(RoundSigDigits(Blind(Loop)%MinSlatAngle,1))//  &
        //           '] is less than the smallest allowed by slat dimensions and spacing, min=['//  &
        //           TRIM(RoundSigDigits(MinSlatAngGeom,1))//'] deg.')
        //        CALL ShowContinueError(state, 'Minimum Slat Angle will be set to '//TRIM(RoundSigDigits(MinSlatAngGeom,1))//' deg.')
        //        Blind(Loop)%MinSlatAngle = MinSlatAngGeom
        //      END IF
        //      ! Error if input maximum slat angle is greater than that allowed by slat geometry
        //      IF(Blind(Loop)%MaxSlatAngle > MaxSlatAngGeom) THEN
        //        CALL ShowWarningError(state, TRIM(state.dataHeatBalMgr->CurrentModuleObject)//'="'//TRIM(MaterialNames(1))//'", Illegal value
        //        combination.') CALL ShowContinueError(state,
        //        TRIM(cNumericFieldNames(27))//'=['//TRIM(RoundSigDigits(Blind(Loop)%MaxSlatAngle,1))//  &
        //           '] is greater than the largest allowed by slat dimensions and spacing, ['//  &
        //           TRIM(RoundSigDigits(MaxSlatAngGeom,1))//'] deg.')
        //        CALL ShowContinueError(state, 'Maximum Slat Angle will be set to '//TRIM(RoundSigDigits(MaxSlatAngGeom,1))//' deg.')
        //        Blind(Loop)%MaxSlatAngle = MaxSlatAngGeom
        //      END IF
        //    END IF  ! End of check if slat angle is variable
    }

    // Window Blind Materials for EquivalentLayer Model

    state.dataHeatBalMgr->CurrentModuleObject = "WindowMaterial:Blind:EquivalentLayer";
    for (Loop = 1; Loop <= state.dataHeatBal->TotBlindsEQL; ++Loop) {

        // Call Input Get routine to retrieve material data
        ip->getObjectItem(state,
                          state.dataHeatBalMgr->CurrentModuleObject,
                          Loop,
                          MaterialNames,
                          MaterialNumAlpha,
                          MaterialProps,
                          MaterialNumProp,
                          IOStat,
                          ipsc->lNumericFieldBlanks,
                          ipsc->lAlphaFieldBlanks,
                          ipsc->cAlphaFieldNames,
                          ipsc->cNumericFieldNames);
        if (GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataHeatBalMgr->UniqueMaterialNames,
                                                     MaterialNames(1),
                                                     state.dataHeatBalMgr->CurrentModuleObject,
                                                     ipsc->cAlphaFieldNames(1),
                                                     ErrorsFound)) {
            ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
            continue;
        }

        ++MaterNum;
        auto *thisMaterial = new MaterialChild;
        state.dataMaterial->Material(MaterNum) = thisMaterial;
        thisMaterial->group = Group::BlindEquivalentLayer;

        thisMaterial->Name = MaterialNames(1);
        thisMaterial->Roughness = SurfaceRoughness::Rough;
        thisMaterial->ROnly = true;

        if (Util::SameString(MaterialNames(2), "Horizontal")) {
            thisMaterial->SlatOrientation = DataWindowEquivalentLayer::Orientation::Horizontal;
        } else if (Util::SameString(MaterialNames(2), "Vertical")) {
            thisMaterial->SlatOrientation = DataWindowEquivalentLayer::Orientation::Vertical;
        }
        thisMaterial->SlatWidth = MaterialProps(1);
        thisMaterial->SlatSeparation = MaterialProps(2);
        thisMaterial->SlatCrown = MaterialProps(3);
        thisMaterial->SlatAngle = MaterialProps(4);

        thisMaterial->TausFrontBeamDiff = MaterialProps(5);
        thisMaterial->TausBackBeamDiff = MaterialProps(6);
        thisMaterial->ReflFrontBeamDiff = MaterialProps(7);
        thisMaterial->ReflBackBeamDiff = MaterialProps(8);

        if (!ipsc->lNumericFieldBlanks(9) && !ipsc->lNumericFieldBlanks(10) &&
            !ipsc->lNumericFieldBlanks(11) && !ipsc->lNumericFieldBlanks(12)) {
            thisMaterial->TausFrontBeamDiffVis = MaterialProps(9);
            thisMaterial->TausBackBeamDiffVis = MaterialProps(10);
            thisMaterial->ReflFrontBeamDiffVis = MaterialProps(11);
            thisMaterial->ReflBackBeamDiffVis = MaterialProps(12);
        }
        if (!ipsc->lNumericFieldBlanks(13) && !ipsc->lNumericFieldBlanks(14) &&
            !ipsc->lNumericFieldBlanks(15)) {
            thisMaterial->TausDiffDiff = MaterialProps(13);
            thisMaterial->ReflFrontDiffDiff = MaterialProps(14);
            thisMaterial->ReflBackDiffDiff = MaterialProps(15);
        }
        if (!ipsc->lNumericFieldBlanks(16) && !ipsc->lNumericFieldBlanks(17) &&
            !ipsc->lNumericFieldBlanks(18)) {
            thisMaterial->TausDiffDiffVis = MaterialProps(13);
            thisMaterial->ReflFrontDiffDiffVis = MaterialProps(14);
            thisMaterial->ReflBackDiffDiffVis = MaterialProps(15);
        }
        if (!ipsc->lNumericFieldBlanks(19)) {
            thisMaterial->TausThermal = MaterialProps(19);
        }
        if (!ipsc->lNumericFieldBlanks(20)) {
            thisMaterial->EmissThermalFront = MaterialProps(20);
        }
        if (!ipsc->lNumericFieldBlanks(21)) {
            thisMaterial->EmissThermalBack = MaterialProps(21);
        }
        // Assumes thermal emissivity is the same as thermal absorptance
        thisMaterial->AbsorpThermalFront = thisMaterial->EmissThermalFront;
        thisMaterial->AbsorpThermalBack = thisMaterial->EmissThermalBack;
        thisMaterial->TransThermal = thisMaterial->TausThermal;

        // By default all blinds have fixed slat angle,
        //  they are used with window shading controls that adjust slat angles like
        //  MaximizeSolar or BlockBeamSolar
        thisMaterial->slatAngleType = SlatAngleType::FixedSlatAngle;
        if (!ipsc->lAlphaFieldBlanks(3)) {
            thisMaterial->slatAngleType = static_cast<SlatAngleType>(getEnumValue(slatAngleTypeNamesUC, Util::makeUPPER(MaterialNames(3))));
        }
        if (thisMaterial->SlatWidth < thisMaterial->SlatSeparation) {
            ShowWarningError(state, format("{}=\"{}\", Slat Seperation/Width", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
            ShowContinueError(state,
                              format("{} [{:.2R}] is less than {} [{:.2R}].",
                                     ipsc->cNumericFieldNames(1),
                                     thisMaterial->SlatWidth,
                                     ipsc->cNumericFieldNames(2),
                                     thisMaterial->SlatSeparation));
            ShowContinueError(state, "This will allow direct beam to be transmitted when Slat angle = 0.");
        }
        if (thisMaterial->SlatSeparation < 0.001) {
            ShowWarningError(state, format("{}=\"{}\", Slat Seperation", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
            ShowContinueError(
                state, format("{} [{:.2R}]. Slate spacing must be > 0.0", ipsc->cNumericFieldNames(2), thisMaterial->SlatSeparation));
            ShowContinueError(state,
                              "...Setting slate spacing to default value of 0.025 m and "
                              "simulation continues.");
            thisMaterial->SlatSeparation = 0.025;
        }
        if (thisMaterial->SlatWidth < 0.001 || thisMaterial->SlatWidth >= 2.0 * thisMaterial->SlatSeparation) {
            ShowWarningError(state, format("{}=\"{}\", Slat Width", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
            ShowContinueError(state,
                              format("{} [{:.2R}]. Slat width range is 0 < Width <= 2*Spacing",
                                     ipsc->cNumericFieldNames(1),
                                     thisMaterial->SlatWidth));
            ShowContinueError(state, "...Setting slate width equal to slate spacing and simulation continues.");
            thisMaterial->SlatWidth = thisMaterial->SlatSeparation;
        }
        if (thisMaterial->SlatCrown < 0.0 || thisMaterial->SlatCrown >= 0.5 * thisMaterial->SlatWidth) {
            ShowWarningError(state, format("{}=\"{}\", Slat Crown", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
            ShowContinueError(state,
                              format("{} [{:.2R}]. Slat crwon range is 0 <= crown < 0.5*Width",
                                     ipsc->cNumericFieldNames(3),
                                     thisMaterial->SlatCrown));
            ShowContinueError(state, "...Setting slate crown to 0.0 and simulation continues.");
            thisMaterial->SlatCrown = 0.0;
        }
        if (thisMaterial->SlatAngle < -90.0 || thisMaterial->SlatAngle > 90.0) {
            ShowWarningError(state, format("{}=\"{}\", Slat Angle", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
            ShowContinueError(state,
                              format("{} [{:.2R}]. Slat angle range is -90.0 <= Angle < 90.0",
                                     ipsc->cNumericFieldNames(4),
                                     thisMaterial->SlatAngle));
            ShowContinueError(state, "...Setting slate angle to 0.0 and simulation continues.");
            thisMaterial->SlatAngle = 0.0;
        }

        if (!Util::SameString(MaterialNames(2), "Horizontal") && !Util::SameString(MaterialNames(2), "Vertical")) {
            ErrorsFound = true;
            ShowSevereError(state, format("{}=\"{}\", Illegal value", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
            ShowContinueError(state,
                              format("{}=\"{}\", must be Horizontal or Vertical.", ipsc->cAlphaFieldNames(2), MaterialNames(2)));
        }

        if ((MaterialProps(5) + MaterialProps(7) >= 1.0)) {
            ErrorsFound = true;
            ShowSevereError(state, format("{}=\"{}\", Illegal value combination.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
            ShowContinueError(state,
                              format("{} + {} not < 1.0", ipsc->cNumericFieldNames(5), ipsc->cNumericFieldNames(7)));
        }
        if ((MaterialProps(6) + MaterialProps(8) >= 1.0)) {
            ErrorsFound = true;
            ShowSevereError(state, format("{}=\"{}\", Illegal value combination.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
            ShowContinueError(state,
                              format("{} + {} not < 1.0", ipsc->cNumericFieldNames(6), ipsc->cNumericFieldNames(8)));
        }
        if ((MaterialProps(9) + MaterialProps(11) >= 1.0)) {
            ErrorsFound = true;
            ShowSevereError(state, format("{}=\"{}\", Illegal value combination.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
            ShowContinueError(state,
                              format("{} + {} not < 1.0", ipsc->cNumericFieldNames(9), ipsc->cNumericFieldNames(11)));
        }
        if ((MaterialProps(10) + MaterialProps(12) >= 1.0)) {
            ErrorsFound = true;
            ShowSevereError(state, format("{}=\"{}\", Illegal value combination.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
            ShowContinueError(
                state, format("{} + {} not < 1.0", ipsc->cNumericFieldNames(10), ipsc->cNumericFieldNames(12)));
        }

    } // TotBlindsEQL loop

    // EcoRoof Materials
    // PSU 2006
    state.dataHeatBalMgr->CurrentModuleObject = "Material:RoofVegetation";
    for (Loop = 1; Loop <= EcoRoofMat; ++Loop) {
        // Call Input Get Routine to retrieve material data from ecoroof

        ip->getObjectItem(state,
                          state.dataHeatBalMgr->CurrentModuleObject,
                          Loop,
                          MaterialNames,
                          MaterialNumAlpha,
                          MaterialProps,
                          MaterialNumProp,
                          IOStat,
                          ipsc->lNumericFieldBlanks,
                          ipsc->lAlphaFieldBlanks,
                          ipsc->cAlphaFieldNames,
                          ipsc->cNumericFieldNames);
        if (GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataHeatBalMgr->UniqueMaterialNames,
                                                     MaterialNames(1),
                                                     state.dataHeatBalMgr->CurrentModuleObject,
                                                     ipsc->cAlphaFieldNames(1),
                                                     ErrorsFound)) {
            ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
            continue;
        }

        // this part is similar to the regular material
        // Load the material derived type from the input data.
        ++MaterNum;
        auto *thisMaterial = new MaterialChild;
        state.dataMaterial->Material(MaterNum) = thisMaterial;
        thisMaterial->group = Group::EcoRoof;

        // this part is new for Ecoroof properties,
        // especially for the Plant Layer of the ecoroof
        thisMaterial->HeightOfPlants = MaterialProps(1);
        thisMaterial->LAI = MaterialProps(2);
        thisMaterial->Lreflectivity = MaterialProps(3); // Albedo
        thisMaterial->LEmissitivity = MaterialProps(4);
        thisMaterial->RStomata = MaterialProps(5);

        thisMaterial->Name = MaterialNames(1);
        // need to treat the A2 with is just the name of the soil(it is
        // not important)
        thisMaterial->Roughness = static_cast<SurfaceRoughness>(getEnumValue(surfaceRoughnessNamesUC, Util::makeUPPER(MaterialNames(3))));
        if (Util::SameString(MaterialNames(4), "Simple")) {
            thisMaterial->EcoRoofCalculationMethod = 1;
        } else if (Util::SameString(MaterialNames(4), "Advanced") || ipsc->lAlphaFieldBlanks(4)) {
            thisMaterial->EcoRoofCalculationMethod = 2;
        } else {
            ShowSevereError(state, format("{}=\"{}\", Illegal value", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
            ShowContinueError(state, format("{}=\"{}\".", ipsc->cAlphaFieldNames(4), MaterialNames(4)));
            ShowContinueError(state, "...Valid values are \"Simple\" or \"Advanced\".");
            ErrorsFound = true;
        }

        thisMaterial->Thickness = MaterialProps(6);
        thisMaterial->Conductivity = MaterialProps(7);
        thisMaterial->Density = MaterialProps(8);
        thisMaterial->SpecHeat = MaterialProps(9);
        thisMaterial->AbsorpThermal = MaterialProps(10); // emissivity
        thisMaterial->AbsorpSolar = MaterialProps(11);   // (1 - Albedo)
        thisMaterial->AbsorpVisible = MaterialProps(12);
        thisMaterial->Porosity = MaterialProps(13);
        thisMaterial->MinMoisture = MaterialProps(14);
        thisMaterial->InitMoisture = MaterialProps(15);

        if (thisMaterial->Conductivity > 0.0) {
            state.dataHeatBal->NominalR(MaterNum) = thisMaterial->Thickness / thisMaterial->Conductivity;
            thisMaterial->Resistance = state.dataHeatBal->NominalR(MaterNum);
        } else {
            ShowSevereError(
                state, format("{}=\"{}\" is not defined correctly.", state.dataHeatBalMgr->CurrentModuleObject, ipsc->cAlphaArgs(1)));
            ShowContinueError(state, format("{} is <=0.", ipsc->cNumericFieldNames(7)));
            ErrorsFound = true;
        }

        if (thisMaterial->InitMoisture > thisMaterial->Porosity) {
            ShowWarningError(state, format("{}=\"{}\", Illegal value combination.", state.dataHeatBalMgr->CurrentModuleObject, MaterialNames(1)));
            ShowContinueError(state,
                              format("{} is greater than {}. It must be less or equal.",
                                     ipsc->cNumericFieldNames(15),
                                     ipsc->cNumericFieldNames(13)));
            ShowContinueError(state, format("{} = {:.3T}.", ipsc->cNumericFieldNames(13), thisMaterial->Porosity));
            ShowContinueError(state, format("{} = {:.3T}.", ipsc->cNumericFieldNames(15), thisMaterial->InitMoisture));
            ShowContinueError(state,
                              format("{} is reset to the maximum (saturation) value = {:.3T}.",
                                     ipsc->cNumericFieldNames(15),
                                     thisMaterial->Porosity));
            ShowContinueError(state, "Simulation continues.");
            thisMaterial->InitMoisture = thisMaterial->Porosity;
        }
    }

    // Thermochromic glazing group
    // get the number of WindowMaterial:GlazingGroup:Thermochromic objects in the idf file
    state.dataHeatBalMgr->CurrentModuleObject = "WindowMaterial:GlazingGroup:Thermochromic";
    state.dataHeatBal->TotTCGlazings =
        ip->getNumObjectsFound(state, state.dataHeatBalMgr->CurrentModuleObject);
    if (state.dataHeatBal->TotTCGlazings >= 1) {
        // Read TC glazings
        state.dataHeatBal->TCGlazings.allocate(state.dataHeatBal->TotTCGlazings);

        for (Loop = 1; Loop <= state.dataHeatBal->TotTCGlazings; ++Loop) {
            // Get each TCGlazings from the input processor
            ip->getObjectItem(state,
                              state.dataHeatBalMgr->CurrentModuleObject,
                              Loop,
                              ipsc->cAlphaArgs,
                              MaterialNumAlpha,
                              ipsc->rNumericArgs,
                              MaterialNumProp,
                              IOStat,
                              ipsc->lNumericFieldBlanks,
                              ipsc->lAlphaFieldBlanks,
                              ipsc->cAlphaFieldNames,
                              ipsc->cNumericFieldNames);

            if (Util::IsNameEmpty(state, ipsc->cAlphaArgs(1), state.dataHeatBalMgr->CurrentModuleObject, ErrorsFound)) {
                ShowContinueError(state, "...All Thermochromic Glazing names must be unique regardless of subtype.");
                continue;
            }

            if (MaterialNumProp + 1 != MaterialNumAlpha) {
                ShowSevereError(
                    state,
                    format("{}=\"{}\" is not defined correctly.", state.dataHeatBalMgr->CurrentModuleObject, ipsc->cAlphaArgs(1)));
                ShowContinueError(state,
                                  format("Check number of {} compared to number of {}",
                                         ipsc->cAlphaFieldNames(2),
                                         ipsc->cNumericFieldNames(1)));
                ErrorsFound = true;
                continue;
            }

            // Allocate arrays
            state.dataHeatBal->TCGlazings(Loop).SpecTemp.allocate(MaterialNumProp);
            state.dataHeatBal->TCGlazings(Loop).LayerName.allocate(MaterialNumProp);
            state.dataHeatBal->TCGlazings(Loop).LayerPoint.allocate(MaterialNumProp);
            state.dataHeatBal->TCGlazings(Loop).SpecTemp = 0.0;
            state.dataHeatBal->TCGlazings(Loop).LayerName = "";
            state.dataHeatBal->TCGlazings(Loop).LayerPoint = 0;

            state.dataHeatBal->TCGlazings(Loop).Name = ipsc->cAlphaArgs(1);
            state.dataHeatBal->TCGlazings(Loop).NumGlzMat = MaterialNumProp;

            for (iTC = 1; iTC <= MaterialNumProp; ++iTC) {
                state.dataHeatBal->TCGlazings(Loop).SpecTemp(iTC) = ipsc->rNumericArgs(iTC);
                state.dataHeatBal->TCGlazings(Loop).LayerName(iTC) = ipsc->cAlphaArgs(1 + iTC);

                // Find this glazing material in the material list
                iMat = Util::FindItemInPtrList(ipsc->cAlphaArgs(1 + iTC), state.dataMaterial->Material);
                if (iMat != 0) {
                    // TC glazing
                    auto *thisMaterial = dynamic_cast<MaterialChild *>(state.dataMaterial->Material(iMat));
                    assert(thisMaterial != nullptr);
                    thisMaterial->SpecTemp = ipsc->rNumericArgs(iTC);
                    thisMaterial->TCParent = Loop;
                    state.dataHeatBal->TCGlazings(Loop).LayerPoint(iTC) = iMat;

                    // test that named material is of the right type
                    if (thisMaterial->group != Group::WindowGlass) {
                        ShowSevereError(state,
                                        format("{}=\"{}\" is not defined correctly.",
                                               state.dataHeatBalMgr->CurrentModuleObject,
                                               ipsc->cAlphaArgs(1)));
                        ShowContinueError(state, format("Material named: {} is not a window glazing ", ipsc->cAlphaArgs(1 + iTC)));
                        ErrorsFound = true;
                    }

                } else { // thow error because not found
                    ShowSevereError(state,
                                    format("{}=\"{}\" is not defined correctly.",
                                           state.dataHeatBalMgr->CurrentModuleObject,
                                           ipsc->cAlphaArgs(1)));
                    ShowContinueError(state, format("Material named: {} was not found ", ipsc->cAlphaArgs(1 + iTC)));
                    ErrorsFound = true;
                }
            }
        }
    }
    auto &cCurrentModuleObject = ipsc->cCurrentModuleObject;
    cCurrentModuleObject = "WindowMaterial:SimpleGlazingSystem";
    for (Loop = 1; Loop <= state.dataHeatBal->TotSimpleWindow; ++Loop) {

        ip->getObjectItem(state,
                          cCurrentModuleObject,
                          Loop,
                          ipsc->cAlphaArgs,
                          MaterialNumAlpha,
                          ipsc->rNumericArgs,
                          MaterialNumProp,
                          IOStat,
                          ipsc->lNumericFieldBlanks,
                          ipsc->lAlphaFieldBlanks,
                          ipsc->cAlphaFieldNames,
                          ipsc->cNumericFieldNames);
        if (GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataHeatBalMgr->UniqueMaterialNames,
                                                     ipsc->cAlphaArgs(1),
                                                     state.dataHeatBalMgr->CurrentModuleObject,
                                                     ipsc->cAlphaFieldNames(1),
                                                     ErrorsFound)) {
            ShowContinueError(state, "...All Material names must be unique regardless of subtype.");
            continue;
        }
        
        ++MaterNum;
        auto *thisMaterial = new MaterialChild;
        state.dataMaterial->Material(MaterNum) = thisMaterial;
        thisMaterial->group = Group::WindowSimpleGlazing;
        thisMaterial->Name = ipsc->cAlphaArgs(1);
        thisMaterial->SimpleWindowUfactor = ipsc->rNumericArgs(1);
        thisMaterial->SimpleWindowSHGC = ipsc->rNumericArgs(2);
        if (!ipsc->lNumericFieldBlanks(3)) {
            thisMaterial->SimpleWindowVisTran = ipsc->rNumericArgs(3);
            thisMaterial->SimpleWindowVTinputByUser = true;
        }

        HeatBalanceManager::SetupSimpleWindowGlazingSystem(state, MaterNum);
    }

    // Simon: Place to load materials for complex fenestrations
    if ((state.dataMaterial->TotComplexShades > 0) || (state.dataHeatBal->TotComplexGaps > 0)) {
        HeatBalanceManager::SetupComplexFenestrationMaterialInput(state, MaterNum, ErrorsFound);
        if (ErrorsFound) {
            ShowSevereError(state, "Errors found in processing complex fenestration material input");
        }
    }
    ScanForReports(state, "Constructions", state.dataHeatBalMgr->DoReport, "Materials");

    if (state.dataHeatBalMgr->DoReport) {

        print(state.files.eio,
              "! <Material Details>,Material Name,ThermalResistance {{m2-K/w}},Roughness,Thickness {{m}},Conductivity "
              "{{w/m-K}},Density {{kg/m3}},Specific Heat "
              "{{J/kg-K}},Absorptance:Thermal,Absorptance:Solar,Absorptance:Visible\n");

        print(state.files.eio, "! <Material:Air>,Material Name,ThermalResistance {{m2-K/w}}\n");

        // Formats
        constexpr std::string_view Format_701(" Material Details,{},{:.4R},{},{:.4R},{:.3R},{:.3R},{:.3R},{:.4R},{:.4R},{:.4R}\n");
        constexpr std::string_view Format_702(" Material:Air,{},{:.4R}\n");

        for (auto const *mat : state.dataMaterial->Material) {

            switch (mat->group) {
            case Group::Air: {
                print(state.files.eio, Format_702, mat->Name, mat->Resistance);
            } break;
            default: {
                print(state.files.eio,
                      Format_701,
                      mat->Name,
                      mat->Resistance,
                      surfaceRoughnessNames[(int)mat->Roughness],
                      mat->Thickness,
                      mat->Conductivity,
                      mat->Density,
                      mat->SpecHeat,
                      mat->AbsorpThermal,
                      mat->AbsorpSolar,
                      mat->AbsorpVisible);
            } break;
            }
        }
    }

    //  FORMATS.

    if (state.dataGlobal->AnyEnergyManagementSystemInModel) { // setup surface property EMS actuators

        for (auto *mat : state.dataMaterial->Material) { 
            if (mat->group != Group::Regular) continue;
                
            auto *matReg = dynamic_cast<MaterialChild *>(mat);
            assert(matReg != nullptr);
            SetupEMSActuator(state,
                             "Material",
                             matReg->Name,
                             "Surface Property Solar Absorptance",
                             "[ ]",
                             matReg->AbsorpSolarEMSOverrideOn,
                             matReg->AbsorpSolarEMSOverride);
            SetupEMSActuator(state,
                             "Material",
                             matReg->Name,
                             "Surface Property Thermal Absorptance",
                             "[ ]",
                             matReg->AbsorpThermalEMSOverrideOn,
                             matReg->AbsorpThermalEMSOverride);
            SetupEMSActuator(state,
                             "Material",
                             matReg->Name,
                             "Surface Property Visible Absorptance",
                             "[ ]",
                             matReg->AbsorpVisibleEMSOverrideOn,
                             matReg->AbsorpVisibleEMSOverride);
        }
    }

    // try assigning phase change material properties for each material, won't do anything for non pcm surfaces
    for (auto *mBase : state.dataMaterial->Material) {
        if (mBase->group == Material::Group::Screen ||
            mBase->group == Material::Group::WindowGas || 
            mBase->group == Material::Group::WindowGasMixture ||
            mBase->group == Material::Group::GapEquivalentLayer) continue;
            
        auto *m = dynamic_cast<MaterialChild *>(mBase);
        assert(m != nullptr);
        m->phaseChange = HysteresisPhaseChange::HysteresisPhaseChange::factory(state, m->Name);
    }

    GetVariableAbsorptanceInput(state, ErrorsFound); // Read variable thermal and solar absorptance add-on data
}

void GetVariableAbsorptanceInput(EnergyPlusData &state, bool &errorsFound)
{
    int IOStat; // IO Status when calling get input subroutine
    int numAlphas;
    int numNumbers;
    Array1D_string alphas(7);   // character string data
    Array1D<Real64> numbers(1); // numeric data
    std::string_view cCurrentModuleObject{"MaterialProperty:VariableAbsorptance"};

    auto &ip = state.dataInputProcessing->inputProcessor;
    auto &ipsc = state.dataIPShortCut;
    
    int numVariAbs = ip->getNumObjectsFound(state, cCurrentModuleObject);
    state.dataHeatBal->AnyVariableAbsorptance = (numVariAbs > 0);
    for (int i = 1; i <= numVariAbs; ++i) {
        // Call Input Get routine to retrieve material data
        ip->getObjectItem(state,
                          cCurrentModuleObject,
                          i,
                          alphas,
                          numAlphas,
                          numbers,
                          numNumbers,
                          IOStat,
                          ipsc->lNumericFieldBlanks,
                          ipsc->lAlphaFieldBlanks,
                          ipsc->cAlphaFieldNames,
                          ipsc->cNumericFieldNames);

        // Load the material derived type from the input data.
        int MaterNum = Util::FindItemInPtrList(alphas(2), state.dataMaterial->Material);
        if (MaterNum == 0) {
            ShowSevereError(state,
                            format("{}: invalid {} entered={}, must match to a valid Material name.",
                                   cCurrentModuleObject,
                                   ipsc->cAlphaFieldNames(2),
                                   alphas(2)));
            errorsFound = true;
            return;
        }
        auto *thisMaterial = dynamic_cast<MaterialChild *>(state.dataMaterial->Material(MaterNum));
        assert(thisMaterial != nullptr);

        if (thisMaterial->group != Group::Regular) {
            ShowSevereError(
                state,
                format("{}: Reference Material is not appropriate type for Thermal/Solar Absorptance properties, material={}, must have regular "
                       "properties (Thermal/Solar Absorptance)",
                       cCurrentModuleObject,
                       thisMaterial->Name));
            errorsFound = true;
            return;
        }

        thisMaterial->absorpVarCtrlSignal = VariableAbsCtrlSignal::SurfaceTemperature; // default value
        thisMaterial->absorpVarCtrlSignal = static_cast<VariableAbsCtrlSignal>(getEnumValue(variableAbsCtrlSignalNamesUC, Util::makeUPPER(alphas(3))));
        //    init to 0 as GetScheduleIndex returns 0 for not-found schedule
        thisMaterial->absorpThermalVarFuncIdx = Curve::GetCurveIndex(state, alphas(4));
        thisMaterial->absorpThermalVarSchedIdx = ScheduleManager::GetScheduleIndex(state, alphas(5));
        thisMaterial->absorpSolarVarFuncIdx = Curve::GetCurveIndex(state, alphas(6));
        thisMaterial->absorpSolarVarSchedIdx = ScheduleManager::GetScheduleIndex(state, alphas(7));
        if (thisMaterial->absorpVarCtrlSignal == VariableAbsCtrlSignal::Scheduled) {
            if ((thisMaterial->absorpThermalVarSchedIdx == 0) && (thisMaterial->absorpSolarVarSchedIdx == 0)) {
                ShowSevereError(
                    state,
                    format("{}: Control signal \"Scheduled\" is chosen but both thermal and solar absorptance schedules are undefined, for object {}",
                           cCurrentModuleObject,
                           alphas(1)));
                errorsFound = true;
                return;
            }
            if ((thisMaterial->absorpThermalVarFuncIdx > 0) || (thisMaterial->absorpSolarVarFuncIdx > 0)) {
                ShowWarningError(state,
                                 format("{}: Control signal \"Scheduled\" is chosen. Thermal or solar absorptance function name is going to be "
                                        "ignored, for object {}",
                                        cCurrentModuleObject,
                                        alphas(1)));
                errorsFound = true;
                return;
            }
        } else { // controlled by performance table or curve
            if ((thisMaterial->absorpThermalVarFuncIdx == 0) && (thisMaterial->absorpSolarVarFuncIdx == 0)) {
                ShowSevereError(state,
                                format("{}: Non-schedule control signal is chosen but both thermal and solar absorptance table or curve are "
                                       "undefined, for object {}",
                                       cCurrentModuleObject,
                                       alphas(1)));
                errorsFound = true;
                return;
            }
            if ((thisMaterial->absorpThermalVarSchedIdx > 0) || (thisMaterial->absorpSolarVarSchedIdx > 0)) {
                ShowWarningError(state,
                                 format("{}: Non-schedule control signal is chosen. Thermal or solar absorptance schedule name is going to be "
                                        "ignored, for object {}",
                                        cCurrentModuleObject,
                                        alphas(1)));
                errorsFound = true;
                return;
            }
        }
    }
}

void CalcScreenTransmittance(EnergyPlusData &state,
                             MaterialScreen const *screen,
                             Real64 phi,     // Optional sun altitude relative to surface outward normal (radians)
                             Real64 theta,   // Optional sun azimuth relative to surface outward normal (radians)
                             ScreenBmTransAbsRef &tar
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   May 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    //  Calculate transmittance of window screen given azimuth and altitude angle
    //  of sun and surface orientation.

    // METHODOLOGY EMPLOYED:
    //  Window screen solar beam transmittance varies as the sun moves across the sky
    //  due to the geometry of the screen material and the angle of incidence
    //  of the solar beam. Azimuth and altitude angle are calculated with respect
    //  to the surface outward normal. Solar beam reflectance and absorptance are also
    //  accounted for.

    Real64 constexpr Small(1.E-9); // Small Number used to approximate zero

    Real64 Tdirect;                   // Beam solar transmitted through screen (dependent on sun angle)
    Real64 Tscattered;                // Beam solar reflected through screen (dependent on sun angle)
    Real64 TscatteredVis;             // Visible beam solar reflected through screen (dependent on sun angle)
    Real64 Tscattermax;               // Maximum solar beam  scattered transmittance
    Real64 TscattermaxVis;            // Maximum visible beam scattered transmittance
    Real64 ExponentInterior;          // Exponent used in scattered transmittance calculation
    // when Delta < DeltaMax (0,0 to peak)
    Real64 ExponentExterior; // Exponent used in scattered transmittance calculation
    // when Delta > DeltaMax (peak to max)

    Real64 sinPhi = std::sin(phi);
    Real64 cosPhi = std::cos(phi);
    Real64 tanPhi = sinPhi / cosPhi;
    Real64 sinTheta = std::sin(theta);
    Real64 cosTheta = std::cos(theta);
    Real64 tanTheta = sinTheta / cosTheta;
    
    bool sunInFront = (phi < Constant::PiOvr2) && (theta < Constant::PiOvr2); // Sun is in front of screen

    // ratio of screen material diameter to screen material spacing
    Real64 Gamma = screen->diameterToSpacingRatio;

    // ************************************************************************************************
    // * calculate transmittance of totally absorbing screen material (beam passing through open area)*
    // ************************************************************************************************

    // calculate compliment of relative solar azimuth
    Real64 Beta = Constant::PiOvr2 - theta;

    // Catch all divide by zero instances
    Real64 TransYDir;
    Real64 TransXDir;
    if (Beta > Small && std::abs(phi - Constant::PiOvr2) > Small) {
        Real64 AlphaDblPrime = std::atan(tanPhi / cosTheta);
        TransYDir = 1.0 - Gamma * (std::cos(AlphaDblPrime) + std::sin(AlphaDblPrime) * tanPhi * std::sqrt(1.0 + pow_2(1.0 / std::tan(Beta))));
        TransYDir = max(0.0, TransYDir);
    } else {
        TransYDir = 0.0;
    }

    Real64 COSMu = std::sqrt(pow_2(cosPhi) * pow_2(cosTheta) + pow_2(sinPhi));
    if (COSMu <= Small) {
        TransXDir = 1.0 - Gamma;
    } else {            
        Real64 Epsilon = std::acos(cosPhi * cosTheta / COSMu);
        Real64 Eta = Constant::PiOvr2 - Epsilon;
        if (std::cos(Epsilon) != 0.0 && Eta != 0.0) {
            Real64 MuPrime = std::atan(std::tan(std::acos(COSMu)) / std::cos(Epsilon));
            TransXDir = 1.0 - Gamma * (std::cos(MuPrime) + std::sin(MuPrime) * std::tan(std::acos(COSMu)) * std::sqrt(1.0 + pow_2(1.0 / std::tan(Eta))));
            TransXDir = max(0.0, TransXDir);
        } else {
            TransXDir = 0.0;
        }
    }
    Tdirect = max(0.0, TransXDir * TransYDir);

    // *******************************************************************************
    // * calculate transmittance of scattered beam due to reflecting screen material *
    // *******************************************************************************

    Real64 ReflCyl = screen->CylinderRef;
    Real64 ReflCylVis = screen->CylinderRefVis;

    if (std::abs(theta - Constant::PiOvr2) < Small || std::abs(phi - Constant::PiOvr2) < Small) {
        Tscattered = 0.0;
        TscatteredVis = 0.0;
    } else {
        //   DeltaMax and Delta are in degrees
        Real64 DeltaMax = 89.7 - (10.0 * Gamma / 0.16);
        Real64 Delta = std::sqrt(pow_2(theta / Constant::DegToRad) + pow_2(phi / Constant::DegToRad));

        //   Use empirical model to determine maximum (peak) scattering
        Real64 Tscattermax = 0.0229 * Gamma + 0.2971 * ReflCyl - 0.03624 * pow_2(Gamma) + 0.04763 * pow_2(ReflCyl) - 0.44416 * Gamma * ReflCyl;
        Real64 TscattermaxVis = 0.0229 * Gamma + 0.2971 * ReflCylVis - 0.03624 * pow_2(Gamma) + 0.04763 * pow_2(ReflCylVis) - 0.44416 * Gamma * ReflCylVis;

        //   Vary slope of interior and exterior surface of scattering model
        Real64 ExponentInterior = -pow_2(Delta - DeltaMax) / 600.0;
        Real64 ExponentExterior = -std::pow(std::abs(Delta - DeltaMax), 2.5) / 600.0;

        //   Determine ratio of scattering at 0,0 incident angle to maximum (peak) scattering
        Real64 PeakToPlateauRatio = 1.0 / (0.2 * (1 - Gamma) * ReflCyl);
        Real64 PeakToPlateauRatioVis = 1.0 / (0.2 * (1 - Gamma) * ReflCylVis);

        //     Apply offset for plateau and use exterior exponential function to simulate actual scattering as a function of solar angles
        if (Delta > DeltaMax) {
            Tscattered = 0.2 * (1.0 - Gamma) * ReflCyl * Tscattermax * (1.0 + (PeakToPlateauRatio - 1.0) * std::exp(ExponentExterior));
            TscatteredVis = 0.2 * (1.0 - Gamma) * ReflCylVis * TscattermaxVis * (1.0 + (PeakToPlateauRatioVis - 1.0) * std::exp(ExponentExterior));
            //     Trim off offset if solar angle (delta) is greater than maximum (peak) scattering angle
            Tscattered -= (0.2 * (1.0 - Gamma) * ReflCyl * Tscattermax) * max(0.0, (Delta - DeltaMax) / (90.0 - DeltaMax));
            TscatteredVis -= (0.2 * (1.0 - Gamma) * ReflCylVis * TscattermaxVis) * max(0.0, (Delta - DeltaMax) / (90.0 - DeltaMax));
        } else {
            Tscattered = 0.2 * (1.0 - Gamma) * ReflCyl * Tscattermax * (1.0 + (PeakToPlateauRatio - 1.0) * std::exp(ExponentInterior));
            TscatteredVis = 0.2 * (1.0 - Gamma) * ReflCylVis * TscattermaxVis * (1.0 + (PeakToPlateauRatioVis - 1.0) * std::exp(ExponentInterior));
        }
    }

    if (screen->bmRefModel == Material::ScreenBeamReflectanceModel::DoNotModel) {
        if (sunInFront) {
            tar.BmTrans = Tdirect;
            tar.BmTransVis = Tdirect;
            tar.BmTransBack = 0.0;
        } else {
            tar.BmTrans = 0.0;
            tar.BmTransVis = 0.0;
            tar.BmTransBack = Tdirect;
        }
        Tscattered = 0.0;
        TscatteredVis = 0.0;
    } else if (screen->bmRefModel == Material::ScreenBeamReflectanceModel::DirectBeam) {
        if (sunInFront) {
            tar.BmTrans = Tdirect + Tscattered;
            tar.BmTransVis = Tdirect + TscatteredVis;
            tar.BmTransBack = 0.0;
        } else {
            tar.BmTrans = 0.0;
            tar.BmTransVis = 0.0;
            tar.BmTransBack = Tdirect + Tscattered;
        }
        Tscattered = 0.0;
        TscatteredVis = 0.0;
    } else if (screen->bmRefModel == Material::ScreenBeamReflectanceModel::Diffuse) {
        if (sunInFront) {
            tar.BmTrans = Tdirect;
            tar.BmTransVis = Tdirect;
            tar.BmTransBack = 0.0;
        } else {
            tar.BmTrans = 0.0;
            tar.BmTransVis = 0.0;
            tar.BmTransBack = Tdirect;
        }
        Tscattered = max(0.0, Tscattered);
        TscatteredVis = max(0.0, TscatteredVis);
    }

    if (sunInFront) {
        tar.DfTrans = Tscattered;
        tar.DfTransVis = TscatteredVis;
        tar.DfTransBack = 0.0;
        tar.RefSolFront = max(0.0, ReflCyl * (1.0 - Tdirect) - Tscattered);
        tar.RefVisFront = max(0.0, ReflCylVis * (1.0 - Tdirect) - TscatteredVis);
        tar.AbsSolFront = max(0.0, (1.0 - Tdirect) * (1.0 - ReflCyl));
        tar.RefSolBack = 0.0;
        tar.RefVisBack = 0.0;
        tar.AbsSolBack = 0.0;
    } else {
        tar.DfTrans = 0.0;
        tar.DfTransVis = 0.0;
        tar.DfTransBack = Tscattered;
        tar.RefSolFront = 0.0;
        tar.RefVisFront = 0.0;
        tar.AbsSolFront = 0.0;
        tar.RefSolBack = max(0.0, ReflCyl * (1.0 - Tdirect) - Tscattered);
        tar.RefVisBack = max(0.0, ReflCylVis * (1.0 - Tdirect) - TscatteredVis);
        tar.AbsSolBack = max(0.0, (1.0 - Tdirect) * (1.0 - ReflCyl));
    }
} // CalcScreenTransmittance()
        
void GetRelativePhiTheta(Real64 phiWin, Real64 thetaWin, Vector3<Real64> const &solcos, Real64 &phi, Real64 &theta)
{
    phi = std::abs(std::acos(solcos.z) - phiWin);
    theta = std::abs(std::atan2(solcos.x, solcos.y) - thetaWin);

    NormalizePhiTheta(phi, theta);
} // GetRelativePhiTheta()

// Use reflection around Pi to normalize to the range 0 to Pi        
void NormalizePhiTheta(Real64 &phi, Real64 &theta) {
    if (phi > Constant::Pi) phi = 2 * Constant::Pi - phi;
    if (theta > Constant::Pi) theta = 2 * Constant::Pi - theta;
} // NormalizePhiTheta()

void GetPhiThetaIndices(Real64 phi, Real64 theta, Real64 dPhi, Real64 dTheta, int &iPhi1, int &iPhi2, int &iTheta1, int &iTheta2)
{
    iPhi1 = int(phi / dPhi);
    iPhi2 = (iPhi1 == maxIPhi - 1) ? iPhi1 : iPhi1 + 1;
    iTheta1 = int(theta / dTheta);
    iTheta2 = (iTheta1 == maxITheta - 1) ? iTheta1 : iTheta1 + 1;
} // GetPhiThetaIndices()
                        
} // namespace EnergyPlus::Material
