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
#include <EnergyPlus/WindowModel.hh>

namespace EnergyPlus::Material {

constexpr std::array<std::string_view, (int)GapVentType::Num> gapVentTypeNames = {"Sealed", "VentedIndoor", "VentedOutdoor"};
constexpr std::array<std::string_view, (int)GasType::Num> gasTypeNames = {"Custom", "Air", "Argon", "Krypton", "Xenon"};
constexpr std::array<std::string_view, (int)GasType::Num> gasTypeNamesUC = {"CUSTOM", "AIR", "ARGON", "KRYPTON", "XENON"};
constexpr std::array<std::string_view, (int)SurfaceRoughness::Num> surfaceRoughnessNames = {
    "VeryRough", "Rough", "MediumRough", "MediumSmooth", "Smooth", "VerySmooth"};

constexpr std::array<Material::Gas, 10> gases = {
    Gas(),                                                                                                        // Empty
    {GasType::Air, {2.873e-3, 7.760e-5, 0.0}, {3.723e-6, 4.940e-8, 0.0}, {1002.737, 1.2324e-2, 0.0}, 28.97, 1.4}, // Air
    {GasType::Argon, {2.285e-3, 5.149e-5, 0.0}, {3.379e-6, 6.451e-8, 0.0}, {521.929, 0.0, 0.0}, 39.948, 1.67},    // Argon
    {GasType::Krypton, {9.443e-4, 2.826e-5, 0.0}, {2.213e-6, 7.777e-8, 0.0}, {248.091, 0.0, 0.0}, 83.8, 1.68},    // Krypton
    {GasType::Xenon, {4.538e-4, 1.723e-5, 0.0}, {1.069e-6, 7.414e-8, 0.0}, {158.340, 0.0, 0.0}, 131.3, 1.66},     // Xenon
    Gas(),                                                                                                        // Empty
    Gas(),                                                                                                        // Empty
    Gas(),                                                                                                        // Empty
    Gas(),                                                                                                        // Empty
    Gas()                                                                                                         // Empty
};

constexpr std::array<std::string_view, (int)EcoRoofCalcMethod::Num> ecoRoofCalcMethodNamesUC = {"SIMPLE", "ADVANCED"};

int GetMaterialNum(EnergyPlusData &state, std::string const &matName)
{
    auto &s_mat = state.dataMaterial;
    auto found = s_mat->materialMap.find(Util::makeUPPER(matName));
    return (found != s_mat->materialMap.end()) ? found->second : 0;
}

MaterialBase *GetMaterial(EnergyPlusData &state, std::string const &matName)
{
    auto &s_mat = state.dataMaterial;
    int matNum = GetMaterialNum(state, matName);
    return (matNum > 0) ? s_mat->materials(matNum) : nullptr;
}
        
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

    int IOStat;                        // IO Status when calling get input subroutine
    int MaterialNumAlpha;              // Number of material alpha names being passed
    int MaterialNumProp;               // Number of material properties being passed
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

    static constexpr std::string_view routineName = "GetMaterialData";

    auto &s_mat = state.dataMaterial;
    auto &s_ip = state.dataInputProcessing->inputProcessor;
    auto &s_ipsc = state.dataIPShortCut;

    RegMat = s_ip->getNumObjectsFound(state, "Material");
    RegRMat = s_ip->getNumObjectsFound(state, "Material:NoMass");
    IRTMat = s_ip->getNumObjectsFound(state, "Material:InfraredTransparent");
    AirMat = s_ip->getNumObjectsFound(state, "Material:AirGap");
    state.dataHeatBal->W5GlsMat = s_ip->getNumObjectsFound(state, "WindowMaterial:Glazing");
    state.dataHeatBal->W5GlsMatAlt = s_ip->getNumObjectsFound(state, "WindowMaterial:Glazing:RefractionExtinctionMethod");
    state.dataHeatBal->W5GasMat = s_ip->getNumObjectsFound(state, "WindowMaterial:Gas");
    state.dataHeatBal->W5GasMatMixture = s_ip->getNumObjectsFound(state, "WindowMaterial:GasMixture");
    state.dataHeatBal->TotShades = s_ip->getNumObjectsFound(state, "WindowMaterial:Shade");
    s_mat->TotComplexShades = s_ip->getNumObjectsFound(state, "WindowMaterial:ComplexShade");
    state.dataHeatBal->TotComplexGaps = s_ip->getNumObjectsFound(state, "WindowMaterial:Gap");
    state.dataHeatBal->TotScreens = s_ip->getNumObjectsFound(state, "WindowMaterial:Screen");
    state.dataHeatBal->TotBlinds = s_ip->getNumObjectsFound(state, "WindowMaterial:Blind");
    EcoRoofMat = s_ip->getNumObjectsFound(state, "Material:RoofVegetation");
    state.dataHeatBal->TotSimpleWindow = s_ip->getNumObjectsFound(state, "WindowMaterial:SimpleGlazingSystem");

    state.dataHeatBal->W5GlsMatEQL = s_ip->getNumObjectsFound(state, "WindowMaterial:Glazing:EquivalentLayer");
    state.dataHeatBal->TotShadesEQL = s_ip->getNumObjectsFound(state, "WindowMaterial:Shade:EquivalentLayer");
    state.dataHeatBal->TotDrapesEQL = s_ip->getNumObjectsFound(state, "WindowMaterial:Drape:EquivalentLayer");
    state.dataHeatBal->TotBlindsEQL = s_ip->getNumObjectsFound(state, "WindowMaterial:Blind:EquivalentLayer");
    state.dataHeatBal->TotScreensEQL = s_ip->getNumObjectsFound(state, "WindowMaterial:Screen:EquivalentLayer");
    state.dataHeatBal->W5GapMatEQL = s_ip->getNumObjectsFound(state, "WindowMaterial:Gap:EquivalentLayer");

    s_mat->TotMaterials = RegMat + RegRMat + AirMat + state.dataHeatBal->W5GlsMat + state.dataHeatBal->W5GlsMatAlt +
                                       state.dataHeatBal->W5GasMat + state.dataHeatBal->W5GasMatMixture + state.dataHeatBal->TotShades +
                                       state.dataHeatBal->TotScreens + state.dataHeatBal->TotBlinds + EcoRoofMat + IRTMat +
                                       state.dataHeatBal->TotSimpleWindow + s_mat->TotComplexShades + state.dataHeatBal->TotComplexGaps +
                                       state.dataHeatBal->W5GlsMatEQL + state.dataHeatBal->TotShadesEQL + state.dataHeatBal->TotDrapesEQL +
                                       state.dataHeatBal->TotBlindsEQL + state.dataHeatBal->TotScreensEQL + state.dataHeatBal->W5GapMatEQL;

    TotFfactorConstructs = s_ip->getNumObjectsFound(state, "Construction:FfactorGroundFloor");
    TotCfactorConstructs = s_ip->getNumObjectsFound(state, "Construction:CfactorUndergroundWall");

    if (TotFfactorConstructs > 0) {
        state.dataHeatBal->NoFfactorConstructionsUsed = false;
    }

    if (TotCfactorConstructs > 0) {
        state.dataHeatBal->NoCfactorConstructionsUsed = false;
    }

    if (TotFfactorConstructs + TotCfactorConstructs >= 1) {
        // Add a new fictitious insulation layer and a thermal mass layer for each F or C factor defined construction
        s_mat->TotMaterials += 1 + TotFfactorConstructs + TotCfactorConstructs;
    }

    state.dataHeatBal->NominalR.dimension(s_mat->TotMaterials, 0.0);

    // Regular Materials

    s_ipsc->cCurrentModuleObject = "Material";
    auto const instances = s_ip->epJSON.find(s_ipsc->cCurrentModuleObject);
    if (instances != s_ip->epJSON.end()) {
        auto const &objectSchemaProps = s_ip->getObjectSchemaProps(state, s_ipsc->cCurrentModuleObject);

        auto &instancesValue = instances.value();

        std::vector<std::string> idfSortedKeys = s_ip->getIDFOrderedKeys(state, s_ipsc->cCurrentModuleObject);
        for (std::string const &key : idfSortedKeys) {
        
            auto instance = instancesValue.find(key);
            assert(instance != instancesValue.end());
            
            auto const &objectFields = instance.value();
            std::string matNameUC = Util::makeUPPER(key);
            s_ip->markObjectAsUsed(s_ipsc->cCurrentModuleObject, key);

            ErrorObjectHeader eoh{routineName, s_ipsc->cCurrentModuleObject, key};
            
            if (s_mat->materialMap.find(matNameUC) != s_mat->materialMap.end()) {
                ShowSevereDuplicateName(state, eoh);
                ErrorsFound = true;
                continue;
            }

            // Load the material derived type from the input data.
            auto *mat = new MaterialChild;
            mat->group = Group::Regular;
            mat->Name = key;

            s_mat->materials.push_back(mat);
            mat->Num = s_mat->materials.isize();
            s_mat->materialMap.insert_or_assign(matNameUC, mat->Num);

            std::string roughness = s_ip->getAlphaFieldValue(objectFields, objectSchemaProps, "roughness");
            mat->Roughness = static_cast<SurfaceRoughness>(getEnumValue(surfaceRoughnessNamesUC, Util::makeUPPER(roughness)));
            mat->Thickness = s_ip->getRealFieldValue(objectFields, objectSchemaProps, "thickness");
            mat->Conductivity = s_ip->getRealFieldValue(objectFields, objectSchemaProps, "conductivity");
            mat->Density = s_ip->getRealFieldValue(objectFields, objectSchemaProps, "density");
            mat->SpecHeat = s_ip->getRealFieldValue(objectFields, objectSchemaProps, "specific_heat");
            mat->AbsorpThermal = s_ip->getRealFieldValue(objectFields, objectSchemaProps, "thermal_absorptance");
            mat->AbsorpThermalInput = mat->AbsorpThermal;
            mat->AbsorpSolar = s_ip->getRealFieldValue(objectFields, objectSchemaProps, "solar_absorptance");
            mat->AbsorpSolarInput = mat->AbsorpSolar;
            mat->AbsorpVisible = s_ip->getRealFieldValue(objectFields, objectSchemaProps, "visible_absorptance");
            mat->AbsorpVisibleInput = mat->AbsorpVisible;

            if (mat->Conductivity > 0.0) {
                    state.dataHeatBal->NominalR(mat->Num) = mat->Thickness / mat->Conductivity;
                mat->Resistance = state.dataHeatBal->NominalR(mat->Num);
            } else {
                ShowSevereError(state, format("Positive thermal conductivity required for material {}", mat->Name));
                ErrorsFound = true;
            }
        }
    }
    
    // Add the 6" heavy concrete for constructions defined with F or C factor method
    if (TotFfactorConstructs + TotCfactorConstructs >= 1) {
        auto *mat = new MaterialChild;
        mat->group = Group::Regular;
        mat->Name = "~FC_Concrete";
        
        s_mat->materials.push_back(mat);
        mat->Num = s_mat->materials.isize();
        s_mat->materialMap.insert_or_assign(Util::makeUPPER(mat->Name), mat->Num);
        
        mat->Thickness = 0.15;    // m, 0.15m = 6 inches
        mat->Conductivity = 1.95; // W/mK
        mat->Density = 2240.0;    // kg/m3
        mat->SpecHeat = 900.0;    // J/kgK
        mat->Roughness = SurfaceRoughness::MediumRough;
        mat->AbsorpSolar = 0.7;
        mat->AbsorpThermal = 0.9;
        mat->AbsorpVisible = 0.7;
        mat->Resistance = state.dataHeatBal->NominalR(mat->Num) = mat->Thickness / mat->Conductivity;

        ++RegMat;
    }

    s_ipsc->cCurrentModuleObject = "Material:NoMass";
    for (Loop = 1; Loop <= RegRMat; ++Loop) {

        // Call Input Get routine to retrieve material data
        s_ip->getObjectItem(state,
                          s_ipsc->cCurrentModuleObject,
                          Loop,
                          s_ipsc->cAlphaArgs,
                          MaterialNumAlpha,
                          s_ipsc->rNumericArgs,
                          MaterialNumProp,
                          IOStat,
                          s_ipsc->lNumericFieldBlanks,
                          s_ipsc->lAlphaFieldBlanks,
                          s_ipsc->cAlphaFieldNames,
                          s_ipsc->cNumericFieldNames);

        ErrorObjectHeader eoh{routineName, s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)};

        if (s_mat->materialMap.find(s_ipsc->cAlphaArgs(1)) != s_mat->materialMap.end()) {
            ShowSevereDuplicateName(state, eoh);
            ErrorsFound = true;
            continue;
        }

        auto *mat = new MaterialChild;
        mat->group = Group::Regular;
        mat->Name = s_ipsc->cAlphaArgs(1);
        
        s_mat->materials.push_back(mat);
        mat->Num = s_mat->materials.isize();
        s_mat->materialMap.insert_or_assign(mat->Name, mat->Num);

        mat->Roughness = static_cast<SurfaceRoughness>(getEnumValue(surfaceRoughnessNamesUC, Util::makeUPPER(s_ipsc->cAlphaArgs(2))));

        mat->Resistance = s_ipsc->rNumericArgs(1);
        mat->ROnly = true;
        if (MaterialNumProp >= 2) {
            mat->AbsorpThermal = s_ipsc->rNumericArgs(2);
            mat->AbsorpThermalInput = s_ipsc->rNumericArgs(2);
        } else {
            mat->AbsorpThermal = 0.9;
            mat->AbsorpThermalInput = 0.9;
        }
        if (MaterialNumProp >= 3) {
            mat->AbsorpSolar = s_ipsc->rNumericArgs(3);
            mat->AbsorpSolarInput = s_ipsc->rNumericArgs(3);
        } else {
            mat->AbsorpSolar = 0.7;
            mat->AbsorpSolarInput = 0.7;
        }
        if (MaterialNumProp >= 4) {
            mat->AbsorpVisible = s_ipsc->rNumericArgs(4);
            mat->AbsorpVisibleInput = s_ipsc->rNumericArgs(4);
        } else {
            mat->AbsorpVisible = 0.7;
            mat->AbsorpVisibleInput = 0.7;
        }

        state.dataHeatBal->NominalR(mat->Num) = mat->Resistance;
    }

    // Add a fictitious insulation layer for each construction defined with F or C factor method
    if (TotFfactorConstructs + TotCfactorConstructs >= 1) {
        for (Loop = 1; Loop <= TotFfactorConstructs + TotCfactorConstructs; ++Loop) {
            auto *mat = new MaterialChild;
            mat->group = Group::Regular;
            mat->Name = format("~FC_Insulation_{}", Loop);

            s_mat->materials.push_back(mat);
            mat->Num = s_mat->materials.isize();
            s_mat->materialMap.insert_or_assign(Util::makeUPPER(mat->Name), mat->Num);

            mat->ROnly = true;
            mat->Roughness = SurfaceRoughness::MediumRough;
            mat->AbsorpSolar = 0.0;
            mat->AbsorpThermal = 0.0;
            mat->AbsorpVisible = 0.0;
        }
        RegRMat += TotFfactorConstructs + TotCfactorConstructs;
    }

    // Air Materials (for air spaces in opaque constructions)
    s_ipsc->cCurrentModuleObject = "Material:AirGap";
    for (Loop = 1; Loop <= AirMat; ++Loop) {

        // Call Input Get routine to retrieve material data
        s_ip->getObjectItem(state,
                          s_ipsc->cCurrentModuleObject,
                          Loop,
                          s_ipsc->cAlphaArgs,
                          MaterialNumAlpha,
                          s_ipsc->rNumericArgs,
                          MaterialNumProp,
                          IOStat,
                          s_ipsc->lNumericFieldBlanks,
                          s_ipsc->lAlphaFieldBlanks,
                          s_ipsc->cAlphaFieldNames,
                          s_ipsc->cNumericFieldNames);

        ErrorObjectHeader eoh{routineName, s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)};

        if (s_mat->materialMap.find(s_ipsc->cAlphaArgs(1)) != s_mat->materialMap.end()) {
            ShowSevereDuplicateName(state, eoh);
            ErrorsFound = true;
            continue;
        }

        // Load the material derived type from the input data.
        auto *mat = new MaterialChild;
        mat->group = Group::Air;
        mat->Name = s_ipsc->cAlphaArgs(1);

        s_mat->materials.push_back(mat);
        mat->Num = s_mat->materials.isize();
        s_mat->materialMap.insert_or_assign(mat->Name, mat->Num);
        
        mat->Roughness = SurfaceRoughness::MediumRough;

        mat->Resistance = s_ipsc->rNumericArgs(1);
        mat->ROnly = true;

        state.dataHeatBal->NominalR(mat->Num) = mat->Resistance;
    }

    s_ipsc->cCurrentModuleObject = "Material:InfraredTransparent";
    for (Loop = 1; Loop <= IRTMat; ++Loop) {

        // Call Input Get routine to retrieve material data
        s_ip->getObjectItem(state,
                          s_ipsc->cCurrentModuleObject,
                          Loop,
                          s_ipsc->cAlphaArgs,
                          MaterialNumAlpha,
                          s_ipsc->rNumericArgs,
                          MaterialNumProp,
                          IOStat,
                          s_ipsc->lNumericFieldBlanks,
                          s_ipsc->lAlphaFieldBlanks,
                          s_ipsc->cAlphaFieldNames,
                          s_ipsc->cNumericFieldNames);

        ErrorObjectHeader eoh{routineName, s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)};

        if (s_mat->materialMap.find(s_ipsc->cAlphaArgs(1)) != s_mat->materialMap.end()) {
            ShowSevereDuplicateName(state, eoh);
            ErrorsFound = true;
            continue;
        }

        auto *mat = new MaterialChild;
        mat->group = Group::IRTransparent;
        mat->Name = s_ipsc->cAlphaArgs(1);

        s_mat->materials.push_back(mat);
        mat->Num = s_mat->materials.isize();
        s_mat->materialMap.insert_or_assign(mat->Name, mat->Num);

        // Load data for other properties that need defaults
        mat->ROnly = true;
        mat->Resistance = 0.01;
        mat->AbsorpThermal = 0.9999;
        mat->AbsorpThermalInput = 0.9999;
        mat->AbsorpSolar = 1.0;
        mat->AbsorpSolarInput = 1.0;
        mat->AbsorpVisible = 1.0;
        mat->AbsorpVisibleInput = 1.0;

        state.dataHeatBal->NominalR(mat->Num) = mat->Resistance;
    }

    // Glass materials, regular input: transmittance and front/back reflectance

    s_ipsc->cCurrentModuleObject = "WindowMaterial:Glazing";
    for (Loop = 1; Loop <= state.dataHeatBal->W5GlsMat; ++Loop) {

        // Call Input Get routine to retrieve material data
        s_ip->getObjectItem(state,
                          s_ipsc->cCurrentModuleObject,
                          Loop,
                          s_ipsc->cAlphaArgs,
                          MaterialNumAlpha,
                          s_ipsc->rNumericArgs,
                          MaterialNumProp,
                          IOStat,
                          s_ipsc->lNumericFieldBlanks,
                          s_ipsc->lAlphaFieldBlanks,
                          s_ipsc->cAlphaFieldNames,
                          s_ipsc->cNumericFieldNames);

        ErrorObjectHeader eoh{routineName, s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)};

        if (s_mat->materialMap.find(s_ipsc->cAlphaArgs(1)) != s_mat->materialMap.end()) {
            ShowSevereDuplicateName(state, eoh);
            ErrorsFound = true;
            continue;
        }

        auto *mat = new MaterialChild;
        mat->group = Group::WindowGlass;
        mat->Name = s_ipsc->cAlphaArgs(1);

        s_mat->materials.push_back(mat);
        mat->Num = s_mat->materials.isize();
        s_mat->materialMap.insert_or_assign(mat->Name, mat->Num);

        mat->Roughness = SurfaceRoughness::VerySmooth;
        mat->ROnly = true;
        mat->Thickness = s_ipsc->rNumericArgs(1);

        mat->windowOpticalData = static_cast<Window::OpticalDataModel>(getEnumValue(Window::opticalDataModelNamesUC, s_ipsc->cAlphaArgs(2)));
        if (mat->windowOpticalData != Window::OpticalDataModel::SpectralAndAngle) {
            mat->Trans = s_ipsc->rNumericArgs(2);
            mat->ReflectSolBeamFront = s_ipsc->rNumericArgs(3);
            mat->ReflectSolBeamBack = s_ipsc->rNumericArgs(4);
            mat->TransVis = s_ipsc->rNumericArgs(5);
            mat->ReflectVisBeamFront = s_ipsc->rNumericArgs(6);
            mat->ReflectVisBeamBack = s_ipsc->rNumericArgs(7);
            mat->TransThermal = s_ipsc->rNumericArgs(8);
        }
        mat->AbsorpThermalFront = s_ipsc->rNumericArgs(9);
        mat->AbsorpThermalBack = s_ipsc->rNumericArgs(10);
        mat->Conductivity = s_ipsc->rNumericArgs(11);
        mat->GlassTransDirtFactor = s_ipsc->rNumericArgs(12);
        mat->YoungModulus = s_ipsc->rNumericArgs(13);
        mat->PoissonsRatio = s_ipsc->rNumericArgs(14);
        if (s_ipsc->rNumericArgs(12) == 0.0) mat->GlassTransDirtFactor = 1.0;
        mat->AbsorpThermal = mat->AbsorpThermalBack;

        if (mat->Conductivity > 0.0) {
            mat->Resistance = state.dataHeatBal->NominalR(mat->Num) = mat->Thickness / mat->Conductivity;
        } else {
            ErrorsFound = true;
            ShowSevereError(state, format("Window glass material {} has Conductivity = 0.0, must be >0.0, default = .9", mat->Name));
        }

        mat->windowOpticalData = static_cast<Window::OpticalDataModel>(getEnumValue(Window::opticalDataModelNamesUC, s_ipsc->cAlphaArgs(2)));
        
        if (mat->windowOpticalData == Window::OpticalDataModel::Spectral) {
            if (s_ipsc->lAlphaFieldBlanks(3)) {
                ShowSevereCustomMessage(state, eoh,
                    format("{} = Spectral but {} is blank.", s_ipsc->cAlphaFieldNames(2), s_ipsc->cAlphaFieldNames(3)));
                ErrorsFound = true;
            } else if ((mat->GlassSpectralDataPtr = Util::FindItemInList(s_ipsc->cAlphaArgs(3), state.dataHeatBal->SpectralData)) == 0) {
                ShowSevereItemNotFound(state, eoh, s_ipsc->cAlphaFieldNames(3), s_ipsc->cAlphaArgs(3));
                ErrorsFound = true;
            }

        // TH 8/24/2011, allow glazing properties s_ipsc->rNumericArgs(2 to 10) to equal 0 or 1: 0.0 =< Prop <= 1.0
        // Fixed CR 8413 - modeling spandrel panels as glazing systems
        } else if (mat->windowOpticalData == Window::OpticalDataModel::SpectralAverage) {

            if (s_ipsc->rNumericArgs(2) + s_ipsc->rNumericArgs(3) > 1.0) {
                ErrorsFound = true;
                ShowSevereCustomMessage(state, eoh, format("{} + {} not <= 1.0", s_ipsc->cNumericFieldNames(2), s_ipsc->cNumericFieldNames(3)));
            }

            if (s_ipsc->rNumericArgs(2) + s_ipsc->rNumericArgs(4) > 1.0) {
                ErrorsFound = true;
                ShowSevereCustomMessage(state, eoh, format("{} + {} not <= 1.0", s_ipsc->cNumericFieldNames(2), s_ipsc->cNumericFieldNames(4)));
            }

            if (s_ipsc->rNumericArgs(5) + s_ipsc->rNumericArgs(6) > 1.0) {
                ErrorsFound = true;
                ShowSevereCustomMessage(state, eoh, format("{} + {} not <= 1.0", s_ipsc->cNumericFieldNames(5), s_ipsc->cNumericFieldNames(6)));
            }

            if (s_ipsc->rNumericArgs(5) + s_ipsc->rNumericArgs(7) > 1.0) {
                ErrorsFound = true;
                ShowSevereCustomMessage(state, eoh, format("{} + {} not <= 1.0", s_ipsc->cNumericFieldNames(5), s_ipsc->cNumericFieldNames(7)));
            }

            if (s_ipsc->rNumericArgs(8) + s_ipsc->rNumericArgs(9) > 1.0) {
                ErrorsFound = true;
                ShowSevereCustomMessage(state, eoh, format("{} + {} not <= 1.0", s_ipsc->cNumericFieldNames(8), s_ipsc->cNumericFieldNames(9)));
            }

            if (s_ipsc->rNumericArgs(8) + s_ipsc->rNumericArgs(10) > 1.0) {
                ErrorsFound = true;
                ShowSevereCustomMessage(state, eoh, format("{} + {} not <= 1.0", s_ipsc->cNumericFieldNames(8), s_ipsc->cNumericFieldNames(10)));
            }

            if (s_ipsc->rNumericArgs(2) < 0.0) {
                ShowSevereCustomMessage(state, eoh, format("{} not >= 0.0", s_ipsc->cNumericFieldNames(2)));
                ErrorsFound = true;
            }

            if (s_ipsc->rNumericArgs(2) > 1.0) {
                ErrorsFound = true;
                ShowSevereCustomMessage(state, eoh, format("{} not <= 1.0", s_ipsc->cNumericFieldNames(2)));
            }

            if (s_ipsc->rNumericArgs(3) < 0.0 || s_ipsc->rNumericArgs(3) > 1.0) {
                ErrorsFound = true;
                ShowSevereCustomMessage(state, eoh, format("{} not >= 0.0 and <= 1.0", s_ipsc->cNumericFieldNames(3)));
            }

            if (s_ipsc->rNumericArgs(4) < 0.0 || s_ipsc->rNumericArgs(4) > 1.0) {
                ErrorsFound = true;
                ShowSevereCustomMessage(state, eoh, format("{} not >= 0.0 and <= 1.0", s_ipsc->cNumericFieldNames(4)));
            }

            if (s_ipsc->rNumericArgs(5) < 0.0) {
                ShowWarningCustomMessage(state, eoh, format("{} not >= 0.0", s_ipsc->cNumericFieldNames(5)));
            }

            if (s_ipsc->rNumericArgs(5) > 1.0) {
                ErrorsFound = true;
                ShowSevereCustomMessage(state, eoh, format("{} not <= 1.0", s_ipsc->cNumericFieldNames(5)));
            }

            if (s_ipsc->rNumericArgs(6) < 0.0 || s_ipsc->rNumericArgs(6) > 1.0) {
                ErrorsFound = true;
                ShowSevereCustomMessage(state, eoh, format("{} not >= 0.0 and <= 1.0", s_ipsc->cNumericFieldNames(6)));
            }

            if (s_ipsc->rNumericArgs(7) < 0.0 || s_ipsc->rNumericArgs(7) > 1.0) {
                ErrorsFound = true;
                ShowSevereCustomMessage(state, eoh, format("{} not >= 0.0 and <= 1.0", s_ipsc->cNumericFieldNames(7)));
            }
        }

        if (s_ipsc->rNumericArgs(8) > 1.0) {
            ErrorsFound = true;
            ShowSevereCustomMessage(state, eoh, format("{} not <= 1.0", s_ipsc->cNumericFieldNames(8)));
        }

        if (s_ipsc->rNumericArgs(9) <= 0.0 || s_ipsc->rNumericArgs(9) >= 1.0) {
            ErrorsFound = true;
            ShowSevereCustomMessage(state, eoh, format("{} not > 0.0 and < 1.0", s_ipsc->cNumericFieldNames(9)));
        }

        if (s_ipsc->rNumericArgs(10) <= 0.0 || s_ipsc->rNumericArgs(10) >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, format("{}=\"{}\", Illegal value.", s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)));
            ShowContinueError(state, format("{} not > 0.0 and < 1.0", s_ipsc->cNumericFieldNames(10)));
        }

        if (s_ipsc->rNumericArgs(11) <= 0.0) {
            ErrorsFound = true;
            ShowSevereCustomMessage(state, eoh, format("{} not > 0.0", s_ipsc->cNumericFieldNames(11)));
        }

        if (s_ipsc->rNumericArgs(13) < 0.0) {
            ErrorsFound = true;
            ShowSevereCustomMessage(state, eoh, format("{} not > 0.0", s_ipsc->cNumericFieldNames(13)));
        }

        if (s_ipsc->rNumericArgs(14) < 0.0 || s_ipsc->rNumericArgs(14) >= 1.0) {
            ErrorsFound = true;
            ShowSevereCustomMessage(state, eoh, format("{} not > 0.0 and < 1.0", s_ipsc->cNumericFieldNames(14)));
        }

        if (s_ipsc->cAlphaArgs(4) == "") {
            mat->SolarDiffusing = false;
        } else {
            BooleanSwitch answer = getYesNoValue(s_ipsc->cAlphaArgs(4));
            if (answer == BooleanSwitch::Invalid) {
                ErrorsFound = true;
                ShowSevereError(state, format("{}=\"{}\", Illegal value.", s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)));
                ShowContinueError(state, format("{} must be Yes or No, entered value={}", s_ipsc->cNumericFieldNames(4), s_ipsc->cAlphaArgs(4)));
            } else {
                mat->SolarDiffusing = (answer == BooleanSwitch::Yes);
            }
        }
        
        // Get SpectralAndAngle table names
        if (mat->windowOpticalData == Window::OpticalDataModel::SpectralAndAngle) {
            if (s_ipsc->lAlphaFieldBlanks(5)) {
                ErrorsFound = true;
                ShowSevereEmptyField(state, eoh, s_ipsc->cAlphaFieldNames(5), s_ipsc->cAlphaFieldNames(2), "SpectralAndAngle");
            } else if ((mat->GlassSpecAngTransDataPtr = Curve::GetCurveIndex(state, s_ipsc->cAlphaArgs(5))) == 0) {
                ErrorsFound = true;
                ShowSevereItemNotFound(state, eoh, s_ipsc->cAlphaFieldNames(5), s_ipsc->cAlphaArgs(5));
            } else {
                ErrorsFound |= Curve::CheckCurveDims(state,
                                                     mat->GlassSpecAngTransDataPtr,    // Curve index
                                                     {2},                                       // Valid dimensions
                                                     routineName,                               // Routine name
                                                     s_ipsc->cCurrentModuleObject, // Object Type
                                                     mat->Name,                        // Object Name
                                                     s_ipsc->cAlphaFieldNames(5));                // Field Name

                GetCurveMinMaxValues(state, mat->GlassSpecAngTransDataPtr, minAngValue, maxAngValue, minLamValue, maxLamValue);
                if (minAngValue > 1.0e-6) {
                    ErrorsFound = true;
                    ShowSevereCustomMessage(state, eoh, 
                        format("{} requires the minumum value = 0.0 in the entered table name={}", s_ipsc->cAlphaFieldNames(5), s_ipsc->cAlphaArgs(5)));
                    }
                
                if (std::abs(maxAngValue - 90.0) > 1.0e-6) {
                    ErrorsFound = true;
                    ShowSevereCustomMessage(state, eoh, 
                        format("{} requires the maximum value = 90.0 in the entered table name={}", s_ipsc->cAlphaFieldNames(5), s_ipsc->cAlphaArgs(5)));
                    }

                if (minLamValue < 0.1) {
                    ErrorsFound = true;
                    ShowSevereCustomMessage(state, eoh, 
                        format("{} requires the minumum value = 0.1 micron in the entered table name={}", s_ipsc->cAlphaFieldNames(5), s_ipsc->cAlphaArgs(5)));
                }

                if (maxLamValue > 4.0) {
                    ErrorsFound = true;
                    ShowSevereCustomMessage(state, eoh, 
                        format("{} requires the maximum value = 4.0 microns in the entered table name={}", s_ipsc->cAlphaFieldNames(5), s_ipsc->cAlphaArgs(5)));
                }
            }

        
            if (s_ipsc->lAlphaFieldBlanks(6)) {
                ErrorsFound = true;
                ShowSevereEmptyField(state, eoh, s_ipsc->cAlphaFieldNames(6), s_ipsc->cAlphaFieldNames(2), "SpectralAndAngle");
            } else if ((mat->GlassSpecAngFRefleDataPtr = Curve::GetCurveIndex(state, s_ipsc->cAlphaArgs(6))) == 0) {
                ErrorsFound = true;
                ShowSevereItemNotFound(state, eoh, s_ipsc->cAlphaFieldNames(6), s_ipsc->cAlphaArgs(6));
            } else {
                ErrorsFound |= Curve::CheckCurveDims(state,
                                                     mat->GlassSpecAngFRefleDataPtr,   // Curve index
                                                     {2},                                       // Valid dimensions
                                                     routineName,                               // Routine name
                                                     s_ipsc->cCurrentModuleObject, // Object Type
                                                     mat->Name,                        // Object Name
                                                     s_ipsc->cAlphaFieldNames(6));                // Field Name
                
                GetCurveMinMaxValues(state, mat->GlassSpecAngFRefleDataPtr, minAngValue, maxAngValue, minLamValue, maxLamValue);
                if (minAngValue > 1.0e-6) {
                        ErrorsFound = true;
                        ShowSevereCustomMessage(state, eoh, 
                            format("{} requires the minumum value = 0.0 in the entered table name={}", s_ipsc->cAlphaFieldNames(5), s_ipsc->cAlphaArgs(5)));
                    }
                if (std::abs(maxAngValue - 90.0) > 1.0e-6) {
                    ErrorsFound = true;
                    ShowSevereCustomMessage(state, eoh, 
                        format("{} requires the maximum value = 90.0 in the entered table name={}", s_ipsc->cAlphaFieldNames(5), s_ipsc->cAlphaArgs(5)));
                }
                if (minLamValue < 0.1) {
                    ErrorsFound = true;
                    ShowSevereCustomMessage(state, eoh, 
                        format("{} requires the minumum value = 0.1 micron in the entered table name={}", s_ipsc->cAlphaFieldNames(5), s_ipsc->cAlphaArgs(5)));
                }
                if (maxLamValue > 4.0) {
                    ErrorsFound = true;
                    ShowSevereCustomMessage(state, eoh, 
                        format("{} requires the maximum value = 4.0 microns in the entered table name={}", s_ipsc->cAlphaFieldNames(5), s_ipsc->cAlphaArgs(5)));
                }
            }

            if (s_ipsc->lAlphaFieldBlanks(7)) {
                ErrorsFound = true;
                ShowSevereEmptyField(state, eoh, s_ipsc->cAlphaFieldNames(7), s_ipsc->cAlphaFieldNames(2), "SpectralAndAngle");
            } else if ((mat->GlassSpecAngBRefleDataPtr = Curve::GetCurveIndex(state, s_ipsc->cAlphaArgs(7))) == 0) {
                ErrorsFound = true;
                ShowSevereItemNotFound(state, eoh, s_ipsc->cAlphaFieldNames(7), s_ipsc->cAlphaArgs(7));
            } else {
                ErrorsFound |= Curve::CheckCurveDims(state,
                                                     mat->GlassSpecAngBRefleDataPtr,   // Curve index
                                                     {2},                                       // Valid dimensions
                                                     routineName,                               // Routine name
                                                     s_ipsc->cCurrentModuleObject, // Object Type
                                                     mat->Name,                        // Object Name
                                                     s_ipsc->cAlphaFieldNames(7));                // Field Name
                
                GetCurveMinMaxValues(state, mat->GlassSpecAngBRefleDataPtr, minAngValue, maxAngValue, minLamValue, maxLamValue);
                if (minAngValue > 1.0e-6) {
                    ErrorsFound = true;
                    ShowSevereCustomMessage(state, eoh, 
                        format("{} requires the minumum value = 0.0 in the entered table name={}", s_ipsc->cAlphaFieldNames(5), s_ipsc->cAlphaArgs(5)));
                }
                if (std::abs(maxAngValue - 90.0) > 1.0e-6) {
                    ErrorsFound = true;
                    ShowSevereCustomMessage(state, eoh, 
                        format("{} requires the maximum value = 90.0 in the entered table name={}", s_ipsc->cAlphaFieldNames(5), s_ipsc->cAlphaArgs(5)));
                }
                if (minLamValue < 0.1) {
                    ErrorsFound = true;
                    ShowSevereCustomMessage(state, eoh, 
                        format("{} requires the minumum value = 0.1 micron in the entered table name={}", s_ipsc->cAlphaFieldNames(5), s_ipsc->cAlphaArgs(5)));
                }
                if (maxLamValue > 4.0) {
                    ErrorsFound = true;
                    ShowSevereCustomMessage(state, eoh, 
                        format("{} requires the maximum value = 4.0 microns in the entered table name={}", s_ipsc->cAlphaFieldNames(5), s_ipsc->cAlphaArgs(5)));
                }
            }
        }
    }

    // Glass materials, alternative input: index of refraction and extinction coefficient

    s_ipsc->cCurrentModuleObject = "WindowMaterial:Glazing:RefractionExtinctionMethod";
    for (Loop = 1; Loop <= state.dataHeatBal->W5GlsMatAlt; ++Loop) {

        // Call Input Get routine to retrieve material data
        s_ip->getObjectItem(state,
                          s_ipsc->cCurrentModuleObject,
                          Loop,
                          s_ipsc->cAlphaArgs,
                          MaterialNumAlpha,
                          s_ipsc->rNumericArgs,
                          MaterialNumProp,
                          IOStat,
                          s_ipsc->lNumericFieldBlanks,
                          s_ipsc->lAlphaFieldBlanks,
                          s_ipsc->cAlphaFieldNames,
                          s_ipsc->cNumericFieldNames);

        ErrorObjectHeader eoh{routineName, s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)};
        
        if (s_mat->materialMap.find(s_ipsc->cAlphaArgs(1)) != s_mat->materialMap.end()) {
            ShowSevereDuplicateName(state, eoh);
            ErrorsFound = true;
            continue;
        }

        auto *mat = new MaterialChild;
        mat->group = Group::WindowGlass;
        mat->Name = s_ipsc->cAlphaArgs(1);

        s_mat->materials.push_back(mat);
        mat->Num = s_mat->materials.isize();
        s_mat->materialMap.insert_or_assign(mat->Name, mat->Num);
        
        mat->Roughness = SurfaceRoughness::VerySmooth;
        mat->Thickness = s_ipsc->rNumericArgs(1);
        mat->ROnly = true;

        // Calculate solar and visible transmittance and reflectance at normal incidence from thickness,
        // index of refraction and extinction coefficient. With the alternative input the front and back
        // properties are assumed to be the same.

        ReflectivitySol = pow_2((s_ipsc->rNumericArgs(2) - 1.0) / (s_ipsc->rNumericArgs(2) + 1.0));
        ReflectivityVis = pow_2((s_ipsc->rNumericArgs(4) - 1.0) / (s_ipsc->rNumericArgs(4) + 1.0));
        TransmittivitySol = std::exp(-s_ipsc->rNumericArgs(3) * s_ipsc->rNumericArgs(1));
        TransmittivityVis = std::exp(-s_ipsc->rNumericArgs(5) * s_ipsc->rNumericArgs(1));
        mat->Trans = TransmittivitySol * pow_2(1.0 - ReflectivitySol) / (1.0 - pow_2(ReflectivitySol * TransmittivitySol));
        mat->ReflectSolBeamFront =
            ReflectivitySol * (1.0 + pow_2(1.0 - ReflectivitySol) * pow_2(TransmittivitySol) / (1.0 - pow_2(ReflectivitySol * TransmittivitySol)));
        mat->ReflectSolBeamBack = mat->ReflectSolBeamFront;
        mat->TransVis = TransmittivityVis * pow_2(1.0 - ReflectivityVis) / (1.0 - pow_2(ReflectivityVis * TransmittivityVis));

        mat->ReflectVisBeamFront =
            ReflectivityVis * (1.0 + pow_2(1.0 - ReflectivityVis) * pow_2(TransmittivityVis) / (1.0 - pow_2(ReflectivityVis * TransmittivityVis)));
        mat->ReflectVisBeamBack = mat->ReflectSolBeamFront;
        mat->TransThermal = s_ipsc->rNumericArgs(6);
        mat->AbsorpThermalFront = s_ipsc->rNumericArgs(7);
        mat->AbsorpThermalBack = s_ipsc->rNumericArgs(7);
        mat->Conductivity = s_ipsc->rNumericArgs(8);
        mat->GlassTransDirtFactor = s_ipsc->rNumericArgs(9);
        if (s_ipsc->rNumericArgs(9) == 0.0) mat->GlassTransDirtFactor = 1.0;
        mat->AbsorpThermal = mat->AbsorpThermalBack;

        if (mat->Conductivity > 0.0) {
            mat->Resistance = state.dataHeatBal->NominalR(mat->Num) = mat->Thickness / mat->Conductivity;
        }

        mat->GlassSpectralDataPtr = 0;

        if (s_ipsc->rNumericArgs(6) + s_ipsc->rNumericArgs(7) >= 1.0) {
            ErrorsFound = true;
            ShowSevereCustomMessage(state, eoh, format("{} + {} not < 1.0", s_ipsc->cNumericFieldNames(6), s_ipsc->cNumericFieldNames(7)));
        }

        if (s_ipsc->cAlphaArgs(2) == "") {
            mat->SolarDiffusing = false;
        } else if (s_ipsc->cAlphaArgs(2) == "YES") {
            mat->SolarDiffusing = true;
        } else if (s_ipsc->cAlphaArgs(2) == "NO") {
            mat->SolarDiffusing = false;
        } else {
            ErrorsFound = true;
            ShowSevereError(state, format("{}=\"{}\", Illegal value.", s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)));
            ShowContinueError(state, format("{} must be Yes or No, entered value={}", s_ipsc->cNumericFieldNames(2), s_ipsc->cAlphaArgs(4)));
        }
    }

    // Glass materials, equivalent layer (ASHWAT) method
    s_ipsc->cCurrentModuleObject = "WindowMaterial:Glazing:EquivalentLayer";
    for (Loop = 1; Loop <= state.dataHeatBal->W5GlsMatEQL; ++Loop) {

        // Call Input Get routine to retrieve material data
        s_ip->getObjectItem(state,
                          s_ipsc->cCurrentModuleObject,
                          Loop,
                          s_ipsc->cAlphaArgs,
                          MaterialNumAlpha,
                          s_ipsc->rNumericArgs,
                          MaterialNumProp,
                          IOStat,
                          s_ipsc->lNumericFieldBlanks,
                          s_ipsc->lAlphaFieldBlanks,
                          s_ipsc->cAlphaFieldNames,
                          s_ipsc->cNumericFieldNames);

        ErrorObjectHeader eoh{routineName, s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)};
        
        if (s_mat->materialMap.find(s_ipsc->cAlphaArgs(1)) != s_mat->materialMap.end()) {
            ShowSevereDuplicateName(state, eoh);
            ErrorsFound = true;
            continue;
        }

        auto *mat = new MaterialChild;
        mat->group = Group::GlassEquivalentLayer;
        mat->Name = s_ipsc->cAlphaArgs(1);

        s_mat->materials.push_back(mat);
        mat->Num = s_mat->materials.isize();
        s_mat->materialMap.insert_or_assign(mat->Name, mat->Num);
        
        mat->Roughness = SurfaceRoughness::VerySmooth;
        mat->ROnly = true;

        mat->TausFrontBeamBeam = s_ipsc->rNumericArgs(1);
        mat->TausBackBeamBeam = s_ipsc->rNumericArgs(2);
        mat->ReflFrontBeamBeam = s_ipsc->rNumericArgs(3);
        mat->ReflBackBeamBeam = s_ipsc->rNumericArgs(4);
        mat->TausFrontBeamBeamVis = s_ipsc->rNumericArgs(5);
        mat->TausBackBeamBeamVis = s_ipsc->rNumericArgs(6);
        mat->ReflFrontBeamBeamVis = s_ipsc->rNumericArgs(7);
        mat->ReflBackBeamBeamVis = s_ipsc->rNumericArgs(8);
        mat->TausFrontBeamDiff = s_ipsc->rNumericArgs(9);
        mat->TausBackBeamDiff = s_ipsc->rNumericArgs(10);
        mat->ReflFrontBeamDiff = s_ipsc->rNumericArgs(11);
        mat->ReflBackBeamDiff = s_ipsc->rNumericArgs(12);
        mat->TausFrontBeamDiffVis = s_ipsc->rNumericArgs(13);
        mat->TausBackBeamDiffVis = s_ipsc->rNumericArgs(14);
        mat->ReflFrontBeamDiffVis = s_ipsc->rNumericArgs(15);
        mat->ReflBackBeamDiffVis = s_ipsc->rNumericArgs(16);
        mat->TausDiffDiff = s_ipsc->rNumericArgs(17);
        mat->ReflFrontDiffDiff = s_ipsc->rNumericArgs(18);
        mat->ReflBackDiffDiff = s_ipsc->rNumericArgs(19);
        mat->TausDiffDiffVis = s_ipsc->rNumericArgs(20);
        mat->ReflFrontDiffDiffVis = s_ipsc->rNumericArgs(21);
        mat->ReflBackDiffDiffVis = s_ipsc->rNumericArgs(22);
        mat->TausThermal = s_ipsc->rNumericArgs(23);
        mat->EmissThermalFront = s_ipsc->rNumericArgs(24);
        mat->EmissThermalBack = s_ipsc->rNumericArgs(25);
        mat->Resistance = s_ipsc->rNumericArgs(26);
        if (mat->Resistance <= 0.0) mat->Resistance = 0.158; // equivalent to single pane of 1/4" inch standard glass
        // Assumes thermal emissivity is the same as thermal absorptance
        mat->AbsorpThermalFront = mat->EmissThermalFront;
        mat->AbsorpThermalBack = mat->EmissThermalBack;
        mat->TransThermal = mat->TausThermal;

        mat->windowOpticalData = static_cast<Window::OpticalDataModel>(getEnumValue(Window::opticalDataModelNamesUC, s_ipsc->cAlphaArgs(2)));

        // IF(dataMaterial.Material(MaterNum)%GlassSpectralDataPtr == 0 .AND. Util::SameString(s_ipsc->cAlphaArgs(2),'Spectral')) THEN
        //  ErrorsFound = .TRUE.
        //  CALL ShowSevereError(state, TRIM(s_ipsc->cCurrentModuleObject)//'="'//Trim(dataMaterial.Material(MaterNum)%Name)// &
        //        '" has '//TRIM(cAlphaFieldNames(2))//' = Spectral but has no matching MaterialProperty:GlazingSpectralData set')
        //  if (s_ipsc->lAlphaFieldBlanks(3)) THEN
        //    CALL ShowContinueError(state, '...'//TRIM(cAlphaFieldNames(3))//' is blank.')
        //  ELSE
        //    CALL ShowContinueError(state, '...'//TRIM(cAlphaFieldNames(3))//'="'//TRIM(s_ipsc->cAlphaArgs(3))//  &
        //       '" not found as item in MaterialProperty:GlazingSpectralData objects.')
        //  END IF
        // END IF

        if (mat->windowOpticalData != Window::OpticalDataModel::SpectralAverage) {
            ErrorsFound = true;
            ShowSevereInvalidKey(state, eoh, s_ipsc->cAlphaFieldNames(2), s_ipsc->cAlphaArgs(2), "Must be \"SpectralAverage\".");
        }

    } // W5GlsMatEQL loop

    // Window gas materials (for gaps with a single gas)

    s_ipsc->cCurrentModuleObject = "WindowMaterial:Gas";
    for (Loop = 1; Loop <= state.dataHeatBal->W5GasMat; ++Loop) {

        // Call Input Get routine to retrieve material data
        s_ip->getObjectItem(state,
                          s_ipsc->cCurrentModuleObject,
                          Loop,
                          s_ipsc->cAlphaArgs,
                          MaterialNumAlpha,
                          s_ipsc->rNumericArgs,
                          MaterialNumProp,
                          IOStat,
                          s_ipsc->lNumericFieldBlanks,
                          s_ipsc->lAlphaFieldBlanks,
                          s_ipsc->cAlphaFieldNames,
                          s_ipsc->cNumericFieldNames);

        ErrorObjectHeader eoh{routineName, s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)};
        
        if (s_mat->materialMap.find(s_ipsc->cAlphaArgs(1)) != s_mat->materialMap.end()) {
            ShowSevereDuplicateName(state, eoh);
            ErrorsFound = true;
            continue;
        }

        auto *matGas = new MaterialGasMix;
        matGas->group = Group::WindowGas;
        matGas->Name = s_ipsc->cAlphaArgs(1);
        
        s_mat->materials.push_back(matGas);
        matGas->Num = s_mat->materials.isize();
        s_mat->materialMap.insert_or_assign(matGas->Name, matGas->Num);
        
        matGas->numGases = 1;
        matGas->gasFracts[0] = 1.0;

        // Load the material derived type from the input data.

        matGas->gases[0].type = static_cast<GasType>(getEnumValue(gasTypeNamesUC, Util::makeUPPER(s_ipsc->cAlphaArgs(2))));
        matGas->Roughness = SurfaceRoughness::MediumRough;

        matGas->Thickness = s_ipsc->rNumericArgs(1);
        matGas->ROnly = true;

        gasType = matGas->gases[0].type;
        if (gasType != GasType::Custom) {
            matGas->gases[0] = gases[(int)gasType];
        }

        // Custom gas

        if (gasType == GasType::Custom) {
            matGas->gases[0].con.c0 = s_ipsc->rNumericArgs(2);
            matGas->gases[0].con.c1 = s_ipsc->rNumericArgs(3);
            matGas->gases[0].con.c2 = s_ipsc->rNumericArgs(4);
            matGas->gases[0].vis.c0 = s_ipsc->rNumericArgs(5);
            matGas->gases[0].vis.c1 = s_ipsc->rNumericArgs(6);
            matGas->gases[0].vis.c2 = s_ipsc->rNumericArgs(7);
            matGas->gases[0].cp.c0 = s_ipsc->rNumericArgs(8);
            matGas->gases[0].cp.c1 = s_ipsc->rNumericArgs(9);
            matGas->gases[0].cp.c2 = s_ipsc->rNumericArgs(10);
            matGas->gases[0].wght = s_ipsc->rNumericArgs(11);
            matGas->gases[0].specHeatRatio = s_ipsc->rNumericArgs(12);

            // Check for errors in custom gas properties
            //      IF(dataMaterial.Material(MaterNum)%GasCon(1,1) <= 0.0) THEN
            //        ErrorsFound = .TRUE.
            //        CALL ShowSevereError(state, 'Conductivity Coefficient A for custom window gas='&
            //                 //TRIM(s_ipsc->cAlphaArgs(1))//' should be > 0.')
            //      END IF

            if (matGas->gases[0].vis.c0 <= 0.0) {
                ErrorsFound = true;
                ShowSevereCustomMessage(state, eoh, format("{} not > 0.0", s_ipsc->cNumericFieldNames(5)));
            }
            if (matGas->gases[0].cp.c0 <= 0.0) {
                ErrorsFound = true;
                ShowSevereCustomMessage(state, eoh, format("{} not > 0.0", s_ipsc->cNumericFieldNames(8)));
            }
            if (matGas->gases[0].wght <= 0.0) {
                ErrorsFound = true;
                ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value.");
                ShowContinueError(state, s_ipsc->cNumericFieldNames(11) + " not > 0.0");
            }
        }

        // Nominal resistance of gap at room temperature
        if (!ErrorsFound) {
            DenomRGas = (matGas->gases[0].con.c0 + matGas->gases[0].con.c1 * 300.0 + matGas->gases[0].con.c2 * 90000.0);
            if (DenomRGas > 0.0) {
                state.dataHeatBal->NominalR(matGas->Num) = matGas->Thickness / DenomRGas;
            } else {
                ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value.");
                ShowContinueError(state,
                                  format("Nominal resistance of gap at room temperature calculated at a negative Conductivity=[{:.3R}].", DenomRGas));
                ErrorsFound = true;
            }
        }
    }

    // Window gap materials (for gaps with a single gas for EquivalentLayer)

    s_ipsc->cCurrentModuleObject = "WindowMaterial:Gap:EquivalentLayer";
    for (Loop = 1; Loop <= state.dataHeatBal->W5GapMatEQL; ++Loop) {

        // Call Input Get routine to retrieve material data
        s_ip->getObjectItem(state,
                          s_ipsc->cCurrentModuleObject,
                          Loop,
                          s_ipsc->cAlphaArgs,
                          MaterialNumAlpha,
                          s_ipsc->rNumericArgs,
                          MaterialNumProp,
                          IOStat,
                          s_ipsc->lNumericFieldBlanks,
                          s_ipsc->lAlphaFieldBlanks,
                          s_ipsc->cAlphaFieldNames,
                          s_ipsc->cNumericFieldNames);

        ErrorObjectHeader eoh{routineName, s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)};
        
        if (s_mat->materialMap.find(s_ipsc->cAlphaArgs(1)) != s_mat->materialMap.end()) {
            ShowSevereDuplicateName(state, eoh);
            ErrorsFound = true;
            continue;
        }

        auto *matGas = new MaterialGasMix;
        matGas->group = Group::GapEquivalentLayer;
        matGas->Name = s_ipsc->cAlphaArgs(1);

        s_mat->materials.push_back(matGas);
        matGas->Num = s_mat->materials.isize();
        s_mat->materialMap.insert_or_assign(matGas->Name, matGas->Num);
        
        matGas->numGases = 1;
        matGas->gasFracts[0] = 1.0;

        // Load the material derived type from the input data.

        matGas->gases[0].type = static_cast<GasType>(getEnumValue(gasTypeNamesUC, Util::makeUPPER(s_ipsc->cAlphaArgs(2)))); // Error check?

        matGas->Roughness = SurfaceRoughness::MediumRough;

        matGas->Thickness = s_ipsc->rNumericArgs(1);
        matGas->ROnly = true;

        gasType = matGas->gases[0].type;
        if (gasType != GasType::Custom) {
            matGas->gases[0] = gases[(int)gasType];
        }

        if (!s_ipsc->lAlphaFieldBlanks(2)) {
            // Get gap vent type
            matGas->gapVentType = static_cast<GapVentType>(getEnumValue(gapVentTypeNamesUC, Util::makeUPPER(s_ipsc->cAlphaArgs(3))));
        }

        if (gasType == GasType::Custom) {
            for (ICoeff = 1; ICoeff <= 3; ++ICoeff) {
                matGas->gases[0].con.c0 = s_ipsc->rNumericArgs(2);
                matGas->gases[0].con.c1 = s_ipsc->rNumericArgs(3);
                matGas->gases[0].con.c2 = s_ipsc->rNumericArgs(4);
                matGas->gases[0].vis.c0 = s_ipsc->rNumericArgs(5);
                matGas->gases[0].vis.c1 = s_ipsc->rNumericArgs(6);
                matGas->gases[0].vis.c2 = s_ipsc->rNumericArgs(7);
                matGas->gases[0].cp.c0 = s_ipsc->rNumericArgs(8);
                matGas->gases[0].cp.c1 = s_ipsc->rNumericArgs(9);
                matGas->gases[0].cp.c2 = s_ipsc->rNumericArgs(10);
            }
            matGas->gases[0].wght = s_ipsc->rNumericArgs(11);
            matGas->gases[0].specHeatRatio = s_ipsc->rNumericArgs(12);

            if (matGas->gases[0].vis.c0 <= 0.0) {
                ErrorsFound = true;
                ShowSevereError(state, format("{}=\"{}\", Illegal value.", s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)));
                ShowContinueError(state, format("{} not > 0.0", s_ipsc->cNumericFieldNames(5)));
            }
            if (matGas->gases[0].cp.c0 <= 0.0) {
                ErrorsFound = true;
                ShowSevereError(state, format("{}=\"{}\", Illegal value.", s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)));
                ShowContinueError(state, format("{} not > 0.0", s_ipsc->cNumericFieldNames(8)));
            }
            if (matGas->gases[0].wght <= 0.0) {
                ErrorsFound = true;
                ShowSevereError(state, format("{}=\"{}\", Illegal value.", s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)));
                ShowContinueError(state, format("{} not > 0.0", s_ipsc->cNumericFieldNames(11)));
            }
        }

        // Nominal resistance of gap at room temperature
        if (!ErrorsFound) {
            DenomRGas = (matGas->gases[0].con.c0 + matGas->gases[0].con.c1 * 300.0 + matGas->gases[0].con.c2 * 90000.0);
            if (DenomRGas > 0.0) {
                state.dataHeatBal->NominalR(matGas->Num) = matGas->Thickness / DenomRGas;
            } else {
                ShowSevereError(state, format("{}=\"{}\", Illegal value.", s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)));
                ShowContinueError(state,
                                  format("Nominal resistance of gap at room temperature calculated at a negative Conductivity=[{:.3R}].", DenomRGas));
                ErrorsFound = true;
            }
        }
    } // for (Loop : W5MatEQL)

    // Window gas mixtures (for gaps with two or more gases)

    s_ipsc->cCurrentModuleObject = "WindowMaterial:GasMixture";
    for (Loop = 1; Loop <= state.dataHeatBal->W5GasMatMixture; ++Loop) {

        // Call Input Get routine to retrieve material data
        s_ip->getObjectItem(state,
                          s_ipsc->cCurrentModuleObject,
                          Loop,
                          s_ipsc->cAlphaArgs,
                          MaterialNumAlpha,
                          s_ipsc->rNumericArgs,
                          MaterialNumProp,
                          IOStat,
                          s_ipsc->lNumericFieldBlanks,
                          s_ipsc->lAlphaFieldBlanks,
                          s_ipsc->cAlphaFieldNames,
                          s_ipsc->cNumericFieldNames);
        ErrorObjectHeader eoh{routineName, s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)};
        
        if (s_mat->materialMap.find(s_ipsc->cAlphaArgs(1)) != s_mat->materialMap.end()) {
            ShowSevereDuplicateName(state, eoh);
            ErrorsFound = true;
            continue;
        }

        auto *matGas = new MaterialGasMix;
        matGas->group = Group::WindowGasMixture;
        matGas->Name = s_ipsc->cAlphaArgs(1);

        s_mat->materials.push_back(matGas);
        matGas->Num = s_mat->materials.isize();
        s_mat->materialMap.insert_or_assign(matGas->Name, matGas->Num);
        
        matGas->gases[0].type = matGas->gases[1].type = matGas->gases[2].type = matGas->gases[3].type = matGas->gases[4].type = GasType::Invalid;

        // Load the material derived type from the input data.

        NumGases = s_ipsc->rNumericArgs(2);
        matGas->numGases = NumGases;
        for (NumGas = 0; NumGas < NumGases; ++NumGas) {
            auto &gas = matGas->gases[NumGas];
            gas.type = static_cast<GasType>(getEnumValue(gasTypeNamesUC, Util::makeUPPER(s_ipsc->cAlphaArgs(2 + NumGas))));
            if (gas.type == GasType::Invalid) {
                ShowSevereError(state, format("{}=\"{}\", Illegal value.", s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1 + NumGas)));
                // Error check?
                ErrorsFound = true;
            }
        }

        matGas->Roughness = SurfaceRoughness::MediumRough; // Unused

        matGas->Thickness = s_ipsc->rNumericArgs(1);
        if (matGas->Thickness <= 0.0) {
            ShowSevereError(state, format("{}=\"{}\", Illegal value.", s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)));
            ShowContinueError(state, s_ipsc->cNumericFieldNames(1) + " must be greater than 0.");
        }
        matGas->ROnly = true;

        for (NumGas = 0; NumGas < NumGases; ++NumGas) {
            GasType gasType = matGas->gases[NumGas].type;
            if (gasType != GasType::Custom) {
                matGas->gasFracts[NumGas] = s_ipsc->rNumericArgs(3 + NumGas);
                matGas->gases[NumGas] = gases[(int)gasType];
            }
        }

        // Nominal resistance of gap at room temperature (based on first gas in mixture)
        state.dataHeatBal->NominalR(matGas->Num) =
            matGas->Thickness / (matGas->gases[0].con.c0 + matGas->gases[0].con.c1 * 300.0 + matGas->gases[0].con.c2 * 90000.0);
    }

    // Window Shade Materials

    s_ipsc->cCurrentModuleObject = "WindowMaterial:Shade";
    for (Loop = 1; Loop <= state.dataHeatBal->TotShades; ++Loop) {

        // Call Input Get routine to retrieve material data
        s_ip->getObjectItem(state,
                          s_ipsc->cCurrentModuleObject,
                          Loop,
                          s_ipsc->cAlphaArgs,
                          MaterialNumAlpha,
                          s_ipsc->rNumericArgs,
                          MaterialNumProp,
                          IOStat,
                          s_ipsc->lNumericFieldBlanks,
                          s_ipsc->lAlphaFieldBlanks,
                          s_ipsc->cAlphaFieldNames,
                          s_ipsc->cNumericFieldNames);

        ErrorObjectHeader eoh{routineName, s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)};
        
        if (s_mat->materialMap.find(s_ipsc->cAlphaArgs(1)) != s_mat->materialMap.end()) {
            ShowSevereDuplicateName(state, eoh);
            ErrorsFound = true;
            continue;
        }

        auto *mat = new MaterialChild;
        mat->group = Group::Shade;
        mat->Name = s_ipsc->cAlphaArgs(1);

        s_mat->materials.push_back(mat);
        mat->Num = s_mat->materials.isize();
        s_mat->materialMap.insert_or_assign(mat->Name, mat->Num);

        mat->Roughness = SurfaceRoughness::MediumRough;
        mat->Trans = s_ipsc->rNumericArgs(1);
        mat->ReflectShade = s_ipsc->rNumericArgs(2);
        mat->TransVis = s_ipsc->rNumericArgs(3);
        mat->ReflectShadeVis = s_ipsc->rNumericArgs(4);
        mat->AbsorpThermal = s_ipsc->rNumericArgs(5);
        mat->AbsorpThermalInput = s_ipsc->rNumericArgs(5);
        mat->TransThermal = s_ipsc->rNumericArgs(6);
        mat->Thickness = s_ipsc->rNumericArgs(7);
        mat->Conductivity = s_ipsc->rNumericArgs(8);
        mat->AbsorpSolar = max(0.0, 1.0 - mat->Trans - mat->ReflectShade);
        mat->AbsorpSolarInput = mat->AbsorpSolar;
        mat->WinShadeToGlassDist = s_ipsc->rNumericArgs(9);
        mat->WinShadeTopOpeningMult = s_ipsc->rNumericArgs(10);
        mat->WinShadeBottomOpeningMult = s_ipsc->rNumericArgs(11);
        mat->WinShadeLeftOpeningMult = s_ipsc->rNumericArgs(12);
        mat->WinShadeRightOpeningMult = s_ipsc->rNumericArgs(13);
        mat->WinShadeAirFlowPermeability = s_ipsc->rNumericArgs(14);
        mat->ROnly = true;

        if (mat->Conductivity > 0.0) {
            state.dataHeatBal->NominalR(mat->Num) = mat->Thickness / mat->Conductivity;
        } else {
            state.dataHeatBal->NominalR(mat->Num) = 1.0;
        }

        if (s_ipsc->rNumericArgs(1) + s_ipsc->rNumericArgs(2) >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(1) + " + " + s_ipsc->cNumericFieldNames(2) + " not < 1.0");
        }

        if (s_ipsc->rNumericArgs(3) + s_ipsc->rNumericArgs(4) >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(3) + " + " + s_ipsc->cNumericFieldNames(4) + " not < 1.0");
        }

        if (s_ipsc->rNumericArgs(5) + s_ipsc->rNumericArgs(6) >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(5) + " + " + s_ipsc->cNumericFieldNames(6) + " not < 1.0");
        }
    }

    // Window Shade Materials

    s_ipsc->cCurrentModuleObject = "WindowMaterial:Shade:EquivalentLayer";
    for (Loop = 1; Loop <= state.dataHeatBal->TotShadesEQL; ++Loop) {

        s_ipsc->rNumericArgs = 0;

        // Call Input Get routine to retrieve material data
        s_ip->getObjectItem(state,
                          s_ipsc->cCurrentModuleObject,
                          Loop,
                          s_ipsc->cAlphaArgs,
                          MaterialNumAlpha,
                          s_ipsc->rNumericArgs,
                          MaterialNumProp,
                          IOStat,
                          s_ipsc->lNumericFieldBlanks,
                          s_ipsc->lAlphaFieldBlanks,
                          s_ipsc->cAlphaFieldNames,
                          s_ipsc->cNumericFieldNames);

        ErrorObjectHeader eoh{routineName, s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)};
        
        if (s_mat->materialMap.find(s_ipsc->cAlphaArgs(1)) != s_mat->materialMap.end()) {
            ShowSevereDuplicateName(state, eoh);
            ErrorsFound = true;
            continue;
        }

        auto *mat = new MaterialChild;
        mat->group = Group::ShadeEquivalentLayer;
        mat->Name = s_ipsc->cAlphaArgs(1);
        
        s_mat->materials.push_back(mat);
        mat->Num = s_mat->materials.isize();
        s_mat->materialMap.insert_or_assign(mat->Name, mat->Num);
        
        mat->Roughness = SurfaceRoughness::MediumRough;
        mat->ROnly = true;

        //  Front side and back side have the same beam-Beam Transmittance
        mat->TausFrontBeamBeam = s_ipsc->rNumericArgs(1);
        mat->TausBackBeamBeam = s_ipsc->rNumericArgs(1);
        mat->TausFrontBeamDiff = s_ipsc->rNumericArgs(2);
        mat->TausBackBeamDiff = s_ipsc->rNumericArgs(3);
        mat->ReflFrontBeamDiff = s_ipsc->rNumericArgs(4);
        mat->ReflBackBeamDiff = s_ipsc->rNumericArgs(5);
        mat->TausFrontBeamBeamVis = s_ipsc->rNumericArgs(6);
        mat->TausFrontBeamDiffVis = s_ipsc->rNumericArgs(7);
        mat->ReflFrontBeamDiffVis = s_ipsc->rNumericArgs(8);
        mat->TausThermal = s_ipsc->rNumericArgs(9);
        mat->EmissThermalFront = s_ipsc->rNumericArgs(10);
        mat->EmissThermalBack = s_ipsc->rNumericArgs(11);
        // Assumes thermal emissivity is the same as thermal absorptance
        mat->AbsorpThermalFront = mat->EmissThermalFront;
        mat->AbsorpThermalBack = mat->EmissThermalBack;
        mat->TransThermal = mat->TausThermal;

        if (s_ipsc->rNumericArgs(1) + s_ipsc->rNumericArgs(2) + s_ipsc->rNumericArgs(4) >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
            ShowContinueError(state,
                              s_ipsc->cNumericFieldNames(1) + " + " + s_ipsc->cNumericFieldNames(2) + " + " + s_ipsc->cNumericFieldNames(4) + "not < 1.0");
        }
        if (s_ipsc->rNumericArgs(1) + s_ipsc->rNumericArgs(3) + s_ipsc->rNumericArgs(5) >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
            ShowContinueError(state,
                              s_ipsc->cNumericFieldNames(1) + " + " + s_ipsc->cNumericFieldNames(3) + " + " + s_ipsc->cNumericFieldNames(5) + "not < 1.0");
        }
        if (s_ipsc->rNumericArgs(6) + s_ipsc->rNumericArgs(7) + s_ipsc->rNumericArgs(8) >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
            ShowContinueError(state,
                              s_ipsc->cNumericFieldNames(6) + " + " + s_ipsc->cNumericFieldNames(7) + " + " + s_ipsc->cNumericFieldNames(8) + "not < 1.0");
        }
        if (s_ipsc->rNumericArgs(9) + s_ipsc->rNumericArgs(10) >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(9) + " + " + s_ipsc->cNumericFieldNames(10) + " not < 1.0");
        }
        if (s_ipsc->rNumericArgs(9) + s_ipsc->rNumericArgs(11) >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(9) + " + " + s_ipsc->cNumericFieldNames(11) + " not < 1.0");
        }

    } // TotShadesEQL loop

    // Window drape materials

    s_ipsc->cCurrentModuleObject = "WindowMaterial:Drape:EquivalentLayer";
    for (Loop = 1; Loop <= state.dataHeatBal->TotDrapesEQL; ++Loop) {

        s_ipsc->rNumericArgs = 0;

        // Call Input Get routine to retrieve material data
        s_ip->getObjectItem(state,
                          s_ipsc->cCurrentModuleObject,
                          Loop,
                          s_ipsc->cAlphaArgs,
                          MaterialNumAlpha,
                          s_ipsc->rNumericArgs,
                          MaterialNumProp,
                          IOStat,
                          s_ipsc->lNumericFieldBlanks,
                          s_ipsc->lAlphaFieldBlanks,
                          s_ipsc->cAlphaFieldNames,
                          s_ipsc->cNumericFieldNames);

        ErrorObjectHeader eoh{routineName, s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)};
        
        if (s_mat->materialMap.find(s_ipsc->cAlphaArgs(1)) != s_mat->materialMap.end()) {
            ShowSevereDuplicateName(state, eoh);
            ErrorsFound = true;
            continue;
        }

        auto *mat = new MaterialChild;
        mat->group = Group::DrapeEquivalentLayer;
        mat->Name = s_ipsc->cAlphaArgs(1);

        s_mat->materials.push_back(mat);
        mat->Num = s_mat->materials.isize();
        s_mat->materialMap.insert_or_assign(mat->Name, mat->Num);
        
        mat->Roughness = SurfaceRoughness::MediumRough;
        mat->ROnly = true;

        //  Front side and back side have the same properties
        mat->TausFrontBeamBeam = s_ipsc->rNumericArgs(1);
        mat->TausBackBeamBeam = s_ipsc->rNumericArgs(1);

        mat->TausFrontBeamDiff = s_ipsc->rNumericArgs(2);
        mat->TausBackBeamDiff = s_ipsc->rNumericArgs(3);

        mat->ReflFrontBeamDiff = s_ipsc->rNumericArgs(4);
        mat->ReflBackBeamDiff = s_ipsc->rNumericArgs(5);
        mat->TausFrontBeamBeamVis = s_ipsc->rNumericArgs(6);
        mat->TausFrontBeamDiffVis = s_ipsc->rNumericArgs(7);
        mat->ReflFrontBeamDiffVis = s_ipsc->rNumericArgs(8);
        mat->TausThermal = s_ipsc->rNumericArgs(9);
        mat->EmissThermalFront = s_ipsc->rNumericArgs(10);
        mat->EmissThermalBack = s_ipsc->rNumericArgs(11);
        // Assumes thermal emissivity is the same as thermal absorptance
        mat->AbsorpThermalFront = mat->EmissThermalFront;
        mat->AbsorpThermalBack = mat->EmissThermalBack;
        mat->TransThermal = mat->TausThermal;

        if (!s_ipsc->lNumericFieldBlanks(12) && !s_ipsc->lNumericFieldBlanks(13)) {
            if (s_ipsc->rNumericArgs(12) != 0.0 && s_ipsc->rNumericArgs(13) != 0.0) {
                mat->PleatedDrapeWidth = s_ipsc->rNumericArgs(12);
                mat->PleatedDrapeLength = s_ipsc->rNumericArgs(13);
                mat->ISPleatedDrape = true;
            }
        } else {
            mat->ISPleatedDrape = false;
        }
        if (s_ipsc->rNumericArgs(1) + s_ipsc->rNumericArgs(2) + s_ipsc->rNumericArgs(4) >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
            ShowContinueError(state,
                              s_ipsc->cNumericFieldNames(1) + " + " + s_ipsc->cNumericFieldNames(2) + " + " + s_ipsc->cNumericFieldNames(4) + "not < 1.0");
        }
        if (s_ipsc->rNumericArgs(6) + s_ipsc->rNumericArgs(7) + s_ipsc->rNumericArgs(8) >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
            ShowContinueError(state,
                              s_ipsc->cNumericFieldNames(4) + " + " + s_ipsc->cNumericFieldNames(5) + " + " + s_ipsc->cNumericFieldNames(6) + "not < 1.0");
        }
        if (s_ipsc->rNumericArgs(9) + s_ipsc->rNumericArgs(10) > 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(9) + " + " + s_ipsc->cNumericFieldNames(10) + " not < 1.0");
        }

    } // TotDrapesEQL loop

    // Window Screen Materials

    s_ipsc->cCurrentModuleObject = "WindowMaterial:Screen";
    for (Loop = 1; Loop <= state.dataHeatBal->TotScreens; ++Loop) {

        // Call GetObjectItem routine to retrieve material data
        s_ip->getObjectItem(state,
                          s_ipsc->cCurrentModuleObject,
                          Loop,
                          s_ipsc->cAlphaArgs,
                          MaterialNumAlpha,
                          s_ipsc->rNumericArgs,
                          MaterialNumProp,
                          IOStat,
                          s_ipsc->lNumericFieldBlanks,
                          s_ipsc->lAlphaFieldBlanks,
                          s_ipsc->cAlphaFieldNames,
                          s_ipsc->cNumericFieldNames);

        ErrorObjectHeader eoh{routineName, s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)};
        
        if (s_mat->materialMap.find(s_ipsc->cAlphaArgs(1)) != s_mat->materialMap.end()) {
            ShowSevereDuplicateName(state, eoh);
            ErrorsFound = true;
            continue;
        }

        auto *matScreen = new MaterialScreen;
        matScreen->Name = s_ipsc->cAlphaArgs(1);

        s_mat->materials.push_back(matScreen);
        matScreen->Num = s_mat->materials.isize();
        s_mat->materialMap.insert_or_assign(matScreen->Name, matScreen->Num);

        // Load the material derived type from the input data.

        matScreen->bmRefModel =
            static_cast<ScreenBeamReflectanceModel>(getEnumValue(screenBeamReflectanceModelNamesUC, Util::makeUPPER(s_ipsc->cAlphaArgs(2))));
        if (matScreen->bmRefModel == ScreenBeamReflectanceModel::Invalid) {
            ShowSevereError(state, format("{}=\"{}\", Illegal value.", s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)));
            ShowContinueError(
                state,
                format("{}=\"{}\", must be one of DoNotModel, ModelAsDirectBeam or ModelAsDiffuse.", s_ipsc->cAlphaFieldNames(2), s_ipsc->cAlphaArgs(2)));
            ErrorsFound = true;
        }
        matScreen->Roughness = SurfaceRoughness::MediumRough;
        matScreen->ShadeRef = s_ipsc->rNumericArgs(1);
        if (matScreen->ShadeRef < 0.0 || matScreen->ShadeRef > 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(1) + " must be >= 0 and <= 1");
        }
        matScreen->ShadeRefVis = s_ipsc->rNumericArgs(2);
        if (matScreen->ShadeRefVis < 0.0 || matScreen->ShadeRefVis > 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(2) + " must be >= 0 and <= 1 for material " + matScreen->Name + '.');
        }
        matScreen->AbsorpThermal = s_ipsc->rNumericArgs(3);
        matScreen->AbsorpThermalInput = s_ipsc->rNumericArgs(3);
        if (matScreen->AbsorpThermal < 0.0 || matScreen->AbsorpThermal > 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(3) + " must be >= 0 and <= 1");
        }
        matScreen->Conductivity = s_ipsc->rNumericArgs(4);
        matScreen->Thickness = s_ipsc->rNumericArgs(6); // thickness = diameter

        if (s_ipsc->rNumericArgs(5) > 0.0) {
            //      Screens(ScNum)%ScreenDiameterToSpacingRatio = s_ipsc->rNumericArgs(6)/s_ipsc->rNumericArgs(5) or
            //      1-SQRT(dataMaterial.Material(MaterNum)%Trans
            if (s_ipsc->rNumericArgs(6) / s_ipsc->rNumericArgs(5) >= 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
                ShowContinueError(state, s_ipsc->cNumericFieldNames(6) + " must be less than " + s_ipsc->cNumericFieldNames(5));
            } else {
                //       Calculate direct normal transmittance (open area fraction)
                matScreen->Trans = pow_2(1.0 - s_ipsc->rNumericArgs(6) / s_ipsc->rNumericArgs(5));
            }
        } else {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(5) + " must be > 0.");
            s_ipsc->rNumericArgs(5) = 0.000000001;
        }

        if (s_ipsc->rNumericArgs(6) <= 0.0) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(6) + " must be > 0.");
        }

        //   Modify reflectance to account for the open area in the screen assembly
        matScreen->ShadeRef *= (1.0 - matScreen->Trans);
        matScreen->ShadeRefVis *= (1.0 - matScreen->Trans);

        matScreen->toGlassDist = s_ipsc->rNumericArgs(7);
        if (matScreen->toGlassDist < 0.001 || matScreen->toGlassDist > 1.0) {
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(7) + " must be greater than or equal to 0.001 and less than or equal to 1.");
        }

        matScreen->topOpeningMult = s_ipsc->rNumericArgs(8);
        if (matScreen->topOpeningMult < 0.0 || matScreen->topOpeningMult > 1.0) {
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(8) + " must be greater than or equal to 0 and less than or equal to 1.");
        }

        matScreen->bottomOpeningMult = s_ipsc->rNumericArgs(9);
        if (matScreen->bottomOpeningMult < 0.0 || matScreen->bottomOpeningMult > 1.0) {
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(9) + " must be greater than or equal to 0 and less than or equal to 1.");
        }

        matScreen->leftOpeningMult = s_ipsc->rNumericArgs(10);
        if (matScreen->leftOpeningMult < 0.0 || matScreen->leftOpeningMult > 1.0) {
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(10) + " must be greater than or equal to 0 and less than or equal to 1.");
        }

        matScreen->rightOpeningMult = s_ipsc->rNumericArgs(11);
        if (matScreen->rightOpeningMult < 0.0 || matScreen->rightOpeningMult > 1.0) {
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(11) + " must be greater than or equal to 0 and less than or equal to 1.");
        }

        matScreen->mapDegResolution = s_ipsc->rNumericArgs(12);
        if (matScreen->mapDegResolution < 0 || matScreen->mapDegResolution > 5 || matScreen->mapDegResolution == 4) {
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(12) + " must be 0, 1, 2, 3, or 5.");
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
            state.dataHeatBal->NominalR(matScreen->Num) = (1.0 - matScreen->Trans) * matScreen->Thickness / matScreen->Conductivity;
        } else {
            state.dataHeatBal->NominalR(matScreen->Num) = 1.0;
            ShowWarningError(
                state,
                "Conductivity for material=\"" + matScreen->Name +
                    "\" must be greater than 0 for calculating Nominal R-value, Nominal R is defaulted to 1 and the simulation continues.");
        }

        if (matScreen->Trans + matScreen->ShadeRef >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
            ShowContinueError(state, "Calculated solar transmittance + solar reflectance not < 1.0");
            ShowContinueError(state, "See Engineering Reference for calculation procedure for solar transmittance.");
        }

        if (matScreen->TransVis + matScreen->ShadeRefVis >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
            ShowContinueError(state, "Calculated visible transmittance + visible reflectance not < 1.0");
            ShowContinueError(state, "See Engineering Reference for calculation procedure for visible solar transmittance.");
        }

        if (matScreen->TransThermal + matScreen->AbsorpThermal >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
            ShowSevereError(state, "Thermal hemispherical emissivity plus open area fraction (1-diameter/spacing)**2 not < 1.0");
        }
    }

    s_ipsc->cCurrentModuleObject = "WindowMaterial:Screen:EquivalentLayer";
    for (Loop = 1; Loop <= state.dataHeatBal->TotScreensEQL; ++Loop) {

        s_ipsc->rNumericArgs = 0;

        // Call GetObjectItem routine to retrieve material data
        s_ip->getObjectItem(state,
                          s_ipsc->cCurrentModuleObject,
                          Loop,
                          s_ipsc->cAlphaArgs,
                          MaterialNumAlpha,
                          s_ipsc->rNumericArgs,
                          MaterialNumProp,
                          IOStat,
                          s_ipsc->lNumericFieldBlanks,
                          s_ipsc->lAlphaFieldBlanks,
                          s_ipsc->cAlphaFieldNames,
                          s_ipsc->cNumericFieldNames);

        ErrorObjectHeader eoh{routineName, s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)};
        
        if (s_mat->materialMap.find(s_ipsc->cAlphaArgs(1)) != s_mat->materialMap.end()) {
            ShowSevereDuplicateName(state, eoh);
            ErrorsFound = true;
            continue;
        }

        auto *matScreen = new MaterialChild;
        matScreen->group = Group::ScreenEquivalentLayer;
        matScreen->Name = s_ipsc->cAlphaArgs(1);

        s_mat->materials.push_back(matScreen);
        matScreen->Num = s_mat->materials.isize();
        s_mat->materialMap.insert_or_assign(matScreen->Name, matScreen->Num);
        
        // Load the material derived type from the input data.
        // WindowMaterial:Screen:EquivalentLayer,
        matScreen->Roughness = SurfaceRoughness::MediumRough;
        matScreen->ROnly = true;
        matScreen->TausFrontBeamBeam = s_ipsc->rNumericArgs(1);
        matScreen->TausBackBeamBeam = s_ipsc->rNumericArgs(1);
        matScreen->TausFrontBeamDiff = s_ipsc->rNumericArgs(2);
        matScreen->TausBackBeamDiff = s_ipsc->rNumericArgs(2);
        matScreen->ReflFrontBeamDiff = s_ipsc->rNumericArgs(3);
        matScreen->ReflBackBeamDiff = s_ipsc->rNumericArgs(3);
        matScreen->TausFrontBeamBeamVis = s_ipsc->rNumericArgs(4);
        matScreen->TausFrontBeamDiffVis = s_ipsc->rNumericArgs(5);
        matScreen->ReflFrontDiffDiffVis = s_ipsc->rNumericArgs(6);
        matScreen->TausThermal = s_ipsc->rNumericArgs(7);
        matScreen->EmissThermalFront = s_ipsc->rNumericArgs(8);
        matScreen->EmissThermalBack = s_ipsc->rNumericArgs(8);

        // Assumes thermal emissivity is the same as thermal absorptance
        matScreen->AbsorpThermalFront = matScreen->EmissThermalFront;
        matScreen->AbsorpThermalBack = matScreen->EmissThermalBack;
        matScreen->TransThermal = matScreen->TausThermal;

        if (s_ipsc->rNumericArgs(3) < 0.0 || s_ipsc->rNumericArgs(3) > 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(3) + " must be >= 0 and <= 1");
        }

        if (s_ipsc->rNumericArgs(6) < 0.0 || s_ipsc->rNumericArgs(6) > 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(6) + " must be >= 0 and <= 1 for material " + matScreen->Name + '.');
        }

        if (!s_ipsc->lNumericFieldBlanks(9)) {
            if (s_ipsc->rNumericArgs(9) > 0.00001) {
                matScreen->ScreenWireSpacing = s_ipsc->rNumericArgs(9); // screen wire spacing
            } else {
                ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value.");
                ShowContinueError(state, s_ipsc->cNumericFieldNames(9) + " must be > 0.");
                ShowContinueError(state, "...Setting screen wire spacing to a default value of 0.025m and simulation continues.");
                matScreen->ScreenWireSpacing = 0.025;
            }
        }

        if (!s_ipsc->lNumericFieldBlanks(10)) {
            if (s_ipsc->rNumericArgs(10) > 0.00001 && s_ipsc->rNumericArgs(10) < matScreen->ScreenWireSpacing) {
                matScreen->ScreenWireDiameter = s_ipsc->rNumericArgs(10); // screen wire spacing
            } else {
                ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value.");
                ShowContinueError(state, s_ipsc->cNumericFieldNames(10) + " must be > 0.");
                ShowContinueError(state, "...Setting screen wire diameter to a default value of 0.005m and simulation continues.");
                matScreen->ScreenWireDiameter = 0.005;
            }
        }

        if (matScreen->ScreenWireSpacing > 0.0) {
            if (matScreen->ScreenWireDiameter / matScreen->ScreenWireSpacing >= 1.0) {
                ErrorsFound = true;
                ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
                ShowContinueError(state, s_ipsc->cNumericFieldNames(10) + " must be less than " + s_ipsc->cNumericFieldNames(9));
            } else {
                //  Calculate direct normal transmittance (open area fraction)
                Openness = pow_2(1.0 - matScreen->ScreenWireDiameter / matScreen->ScreenWireSpacing);
                if ((matScreen->TausFrontBeamBeam - Openness) / Openness > 0.01) {
                    ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", screen openness specified.");
                    ShowContinueError(state, s_ipsc->cNumericFieldNames(1) + " is > 1.0% of the value calculated from input fields:");
                    ShowContinueError(state, s_ipsc->cNumericFieldNames(9) + " and " + (s_ipsc->cNumericFieldNames(10)));
                    ShowContinueError(state, " using the formula (1-diameter/spacing)**2");
                    ShowContinueError(state, " ...the screen diameter is recalculated from the material openness specified ");
                    ShowContinueError(state, " ...and wire spacing using the formula = wire spacing * (1.0 - SQRT(Opennes))");
                    matScreen->ScreenWireDiameter = matScreen->ScreenWireSpacing * (1.0 - std::sqrt(matScreen->TausFrontBeamBeam));
                    ShowContinueError(state, format(" ...Recalculated {}={:.4R} m", s_ipsc->cNumericFieldNames(10), matScreen->ScreenWireDiameter));
                }
            }
        }

        if (matScreen->TausFrontBeamBeam + matScreen->ReflFrontBeamDiff >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
            ShowContinueError(state, "Calculated solar transmittance + solar reflectance not < 1.0");
            ShowContinueError(state, "See Engineering Reference for calculation procedure for solar transmittance.");
        }

        if (matScreen->TausFrontBeamBeamVis + matScreen->ReflFrontDiffDiffVis >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
            ShowContinueError(state, "Calculated visible transmittance + visible reflectance not < 1.0");
            ShowContinueError(state, "See Engineering Reference for calculation procedure for visible solar transmittance.");
        }
        if (matScreen->TransThermal + matScreen->AbsorpThermal >= 1.0) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
            ShowSevereError(state, "Thermal hemispherical emissivity plus open area fraction (1-diameter/spacing)**2 not < 1.0");
        }

    } // TotScreensEQL loop

    s_ipsc->cCurrentModuleObject = "WindowMaterial:Blind";
    for (Loop = 1; Loop <= state.dataHeatBal->TotBlinds; ++Loop) {

        // Call Input Get routine to retrieve material data
        s_ip->getObjectItem(state,
                          s_ipsc->cCurrentModuleObject,
                          Loop,
                          s_ipsc->cAlphaArgs,
                          MaterialNumAlpha,
                          s_ipsc->rNumericArgs,
                          MaterialNumProp,
                          IOStat,
                          s_ipsc->lNumericFieldBlanks,
                          s_ipsc->lAlphaFieldBlanks,
                          s_ipsc->cAlphaFieldNames,
                          s_ipsc->cNumericFieldNames);

        ErrorObjectHeader eoh{routineName, s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)};
        
        if (s_mat->materialMap.find(s_ipsc->cAlphaArgs(1)) != s_mat->materialMap.end()) {
            ShowSevereDuplicateName(state, eoh);
            ErrorsFound = true;
            continue;
        }

        auto *matBlind = new MaterialBlind;
        matBlind->Name = s_ipsc->cAlphaArgs(1);

        s_mat->materials.push_back(matBlind);
        matBlind->Num = s_mat->materials.isize();
        s_mat->materialMap.insert_or_assign(matBlind->Name, matBlind->Num);
        
        matBlind->Roughness = SurfaceRoughness::Rough;
        matBlind->ROnly = true;

        matBlind->SlatOrientation = static_cast<DataWindowEquivalentLayer::Orientation>(
            getEnumValue(DataWindowEquivalentLayer::orientationNamesUC, s_ipsc->cAlphaArgs(2)));
        
        matBlind->SlatWidth = s_ipsc->rNumericArgs(1);
        matBlind->SlatSeparation = s_ipsc->rNumericArgs(2);
        matBlind->SlatThickness = s_ipsc->rNumericArgs(3);
        matBlind->SlatAngle = s_ipsc->rNumericArgs(4);
        matBlind->SlatConductivity = s_ipsc->rNumericArgs(5);
        matBlind->SlatTransSolBeamDiff = s_ipsc->rNumericArgs(6);
        matBlind->SlatFrontReflSolBeamDiff = s_ipsc->rNumericArgs(7);
        matBlind->SlatBackReflSolBeamDiff = s_ipsc->rNumericArgs(8);
        matBlind->SlatTransSolDiffDiff = s_ipsc->rNumericArgs(9);
        matBlind->SlatFrontReflSolDiffDiff = s_ipsc->rNumericArgs(10);
        matBlind->SlatBackReflSolDiffDiff = s_ipsc->rNumericArgs(11);
        matBlind->SlatTransVisBeamDiff = s_ipsc->rNumericArgs(12);
        matBlind->SlatFrontReflVisBeamDiff = s_ipsc->rNumericArgs(13);
        matBlind->SlatBackReflVisBeamDiff = s_ipsc->rNumericArgs(14);
        matBlind->SlatTransVisDiffDiff = s_ipsc->rNumericArgs(15);
        matBlind->SlatFrontReflVisDiffDiff = s_ipsc->rNumericArgs(16);
        matBlind->SlatBackReflVisDiffDiff = s_ipsc->rNumericArgs(17);
        matBlind->SlatTransIR = s_ipsc->rNumericArgs(18);
        matBlind->SlatFrontEmissIR = s_ipsc->rNumericArgs(19);
        matBlind->SlatBackEmissIR = s_ipsc->rNumericArgs(20);
        matBlind->toGlassDist = s_ipsc->rNumericArgs(21);
        matBlind->topOpeningMult = s_ipsc->rNumericArgs(22);
        matBlind->bottomOpeningMult = s_ipsc->rNumericArgs(23);
        matBlind->leftOpeningMult = s_ipsc->rNumericArgs(24);
        matBlind->rightOpeningMult = s_ipsc->rNumericArgs(25);
        matBlind->MinSlatAngle = s_ipsc->rNumericArgs(26);
        matBlind->MaxSlatAngle = s_ipsc->rNumericArgs(27);

        // TH 2/11/2010. For CR 8010
        // By default all blinds have fixed slat angle, new blinds with variable slat angle are created if
        //  they are used with window shading controls that adjust slat angles like ScheduledSlatAngle or BlockBeamSolar
        matBlind->SlatAngleType = DataWindowEquivalentLayer::AngleType::Fixed;

        if (matBlind->SlatWidth < matBlind->SlatSeparation) {
            ShowWarningError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Slat Angles/Widths");
            ShowContinueError(state,
                              format("{} [{:.2R}] is less than {} [{:.2R}].",
                                     s_ipsc->cNumericFieldNames(1),
                                     matBlind->SlatWidth,
                                     s_ipsc->cNumericFieldNames(2),
                                     matBlind->SlatSeparation));
            ShowContinueError(state, "This will allow direct beam to be transmitted when Slat angle = 0.");
        }

        if ((s_ipsc->rNumericArgs(6) + s_ipsc->rNumericArgs(7) >= 1.0)) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(6) + " + " + s_ipsc->cNumericFieldNames(7) + " not < 1.0");
        }
        if ((s_ipsc->rNumericArgs(6) + s_ipsc->rNumericArgs(8) >= 1.0)) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(6) + " + " + s_ipsc->cNumericFieldNames(8) + " not < 1.0");
        }

        if ((s_ipsc->rNumericArgs(9) + s_ipsc->rNumericArgs(10) >= 1.0)) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(9) + " + " + s_ipsc->cNumericFieldNames(10) + " not < 1.0");
        }
        if ((s_ipsc->rNumericArgs(9) + s_ipsc->rNumericArgs(11) >= 1.0)) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(9) + " + " + s_ipsc->cNumericFieldNames(11) + " not < 1.0");
        }

        if ((s_ipsc->rNumericArgs(12) + s_ipsc->rNumericArgs(13) >= 1.0) || (s_ipsc->rNumericArgs(12) + s_ipsc->rNumericArgs(14) >= 1.0)) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(12) + " + " + s_ipsc->cNumericFieldNames(13) + " not < 1.0 OR");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(12) + " + " + s_ipsc->cNumericFieldNames(14) + " not < 1.0");
        }

        if ((s_ipsc->rNumericArgs(12) + s_ipsc->rNumericArgs(13) >= 1.0)) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(12) + " + " + s_ipsc->cNumericFieldNames(13) + " not < 1.0");
        }
        if ((s_ipsc->rNumericArgs(12) + s_ipsc->rNumericArgs(14) >= 1.0)) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(12) + " + " + s_ipsc->cNumericFieldNames(14) + " not < 1.0");
        }

        if ((s_ipsc->rNumericArgs(15) + s_ipsc->rNumericArgs(16) >= 1.0)) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(15) + " + " + s_ipsc->cNumericFieldNames(16) + " not < 1.0");
        }
        if ((s_ipsc->rNumericArgs(15) + s_ipsc->rNumericArgs(17) >= 1.0)) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(15) + " + " + s_ipsc->cNumericFieldNames(17) + " not < 1.0");
        }

        // Require that beam and diffuse properties be the same
        if (std::abs(s_ipsc->rNumericArgs(9) - s_ipsc->rNumericArgs(6)) > 1.e-5) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(6) + " must equal " + s_ipsc->cNumericFieldNames(9));
        }

        if (std::abs(s_ipsc->rNumericArgs(10) - s_ipsc->rNumericArgs(7)) > 1.e-5) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(7) + " must equal " + s_ipsc->cNumericFieldNames(10));
        }

        if (std::abs(s_ipsc->rNumericArgs(11) - s_ipsc->rNumericArgs(8)) > 1.e-5) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(8) + " must equal " + s_ipsc->cNumericFieldNames(11));
        }

        if (std::abs(s_ipsc->rNumericArgs(15) - s_ipsc->rNumericArgs(12)) > 1.e-5) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(12) + " must equal " + s_ipsc->cNumericFieldNames(15));
        }

        if (std::abs(s_ipsc->rNumericArgs(16) - s_ipsc->rNumericArgs(13)) > 1.e-5) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(13) + " must equal " + s_ipsc->cNumericFieldNames(16));
        }

        if (std::abs(s_ipsc->rNumericArgs(17) - s_ipsc->rNumericArgs(14)) > 1.e-5) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(14) + " must equal " + s_ipsc->cNumericFieldNames(17));
        }

        if ((s_ipsc->rNumericArgs(18) + s_ipsc->rNumericArgs(19) >= 1.0)) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(18) + " + " + s_ipsc->cNumericFieldNames(19) + " not < 1.0");
        }
        if ((s_ipsc->rNumericArgs(18) + s_ipsc->rNumericArgs(20) >= 1.0)) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(18) + " + " + s_ipsc->cNumericFieldNames(20) + " not < 1.0");
        }

        if (matBlind->toGlassDist < 0.5 * matBlind->SlatWidth) {
            ErrorsFound = true;
            ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
            ShowContinueError(state, s_ipsc->cNumericFieldNames(21) + " is less than half of the " + s_ipsc->cNumericFieldNames(1));
        }

        // Minimum and maximum slat angles allowed by slat geometry
        if (matBlind->SlatWidth > matBlind->SlatSeparation) {
            MinSlatAngGeom = std::asin(matBlind->SlatThickness /
                                       (matBlind->SlatThickness + matBlind->SlatSeparation)) /
                             Constant::DegToRadians;
        } else {
            MinSlatAngGeom = 0.0;
        }
        MaxSlatAngGeom = 180.0 - MinSlatAngGeom;

        // Error if input slat angle not in range allowed by slat geometry
        if ((matBlind->SlatSeparation + matBlind->SlatThickness) <
            matBlind->SlatWidth) {
            if (matBlind->SlatAngle < MinSlatAngGeom) {
                ErrorsFound = true;
                ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
                ShowContinueError(state,
                                  format("{}=[{:.1R}], is less than smallest allowed by slat dimensions and spacing, [{:.1R}] deg.",
                                         s_ipsc->cNumericFieldNames(4),
                                         matBlind->SlatAngle,
                                         MinSlatAngGeom));
            } else if (matBlind->SlatAngle > MaxSlatAngGeom) {
                ErrorsFound = true;
                ShowSevereError(state, s_ipsc->cCurrentModuleObject + "=\"" + s_ipsc->cAlphaArgs(1) + "\", Illegal value combination.");
                ShowContinueError(state,
                                  format("{}=[{:.1R}], is greater than largest allowed by slat dimensions and spacing, [{:.1R}] deg.",
                                         s_ipsc->cNumericFieldNames(4),
                                         matBlind->SlatAngle,
                                         MinSlatAngGeom));
            }
        }

        // By default all Blinds are "fixed" slats.  Only with Shading Control is one considered variable and this check
        // is now done when that happens.  9.3.2009 LKL

        //    IF(Blind(Loop)%SlatAngleType == VariableSlats) THEN
        //      ! Error if maximum slat angle less than minimum
        //      IF(Blind(Loop)%MaxSlatAngle < Blind(Loop)%MinSlatAngle) THEN
        //        ErrorsFound = .TRUE.
        //        CALL ShowSevereError(state, TRIM(s_ipsc->cCurrentModuleObject)//'="'//TRIM(s_ipsc->cAlphaArgs(1))//'", Illegal value
        //        combination.') CALL ShowContinueError(state,
        //        TRIM(cNumericFieldNames(26))//'=['//TRIM(RoundSigDigits(Blind(Loop)%MinSlatAngle,1))//  &
        //           '], is greater than '//TRIM(cNumericFieldNames(27))//'=['//  &
        //           TRIM(RoundSigDigits(Blind(Loop)%MaxSlatAngle,1))//'] deg.')
        //      END IF
        //      ! Error if input slat angle not in input min/max range
        //      IF(Blind(Loop)%MaxSlatAngle > Blind(Loop)%MinSlatAngle .AND. (Blind(Loop)%SlatAngle < Blind(Loop)%MinSlatAngle &
        //          .OR. Blind(Loop)%SlatAngle > Blind(Loop)%MaxSlatAngle)) THEN
        //        ErrorsFound = .TRUE.
        //        CALL ShowSevereError(state, TRIM(s_ipsc->cCurrentModuleObject)//'="'//TRIM(s_ipsc->cAlphaArgs(1))//'", Illegal value
        //        combination.') CALL ShowContinueError(state, TRIM(cNumericFieldNames(4))//'=['//TRIM(RoundSigDigits(Blind(Loop)%SlatAngle,1))//
        //        &
        //           '] is outside of the input min/max range, min=['//TRIM(RoundSigDigits(Blind(Loop)%MinSlatAngle,1))//  &
        //           '], max=['//TRIM(RoundSigDigits(Blind(Loop)%MaxSlatAngle,1))//'] deg.')
        //      END IF
        //      ! Error if input minimum slat angle is less than that allowed by slat geometry
        //      IF(Blind(Loop)%MinSlatAngle < MinSlatAngGeom) THEN
        //        CALL ShowSevereError(state, TRIM(s_ipsc->cCurrentModuleObject)//'="'//TRIM(s_ipsc->cAlphaArgs(1))//'", Illegal value
        //        combination.') CALL ShowContinueError(state,
        //        TRIM(cNumericFieldNames(26))//'=['//TRIM(RoundSigDigits(Blind(Loop)%MinSlatAngle,1))//  &
        //           '] is less than the smallest allowed by slat dimensions and spacing, min=['//  &
        //           TRIM(RoundSigDigits(MinSlatAngGeom,1))//'] deg.')
        //        CALL ShowContinueError(state, 'Minimum Slat Angle will be set to '//TRIM(RoundSigDigits(MinSlatAngGeom,1))//' deg.')
        //        Blind(Loop)%MinSlatAngle = MinSlatAngGeom
        //      END IF
        //      ! Error if input maximum slat angle is greater than that allowed by slat geometry
        //      IF(Blind(Loop)%MaxSlatAngle > MaxSlatAngGeom) THEN
        //        CALL ShowWarningError(state, TRIM(s_ipsc->cCurrentModuleObject)//'="'//TRIM(s_ipsc->cAlphaArgs(1))//'", Illegal value
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

    s_ipsc->cCurrentModuleObject = "WindowMaterial:Blind:EquivalentLayer";
    for (Loop = 1; Loop <= state.dataHeatBal->TotBlindsEQL; ++Loop) {

        // Call Input Get routine to retrieve material data
        s_ip->getObjectItem(state,
                          s_ipsc->cCurrentModuleObject,
                          Loop,
                          s_ipsc->cAlphaArgs,
                          MaterialNumAlpha,
                          s_ipsc->rNumericArgs,
                          MaterialNumProp,
                          IOStat,
                          s_ipsc->lNumericFieldBlanks,
                          s_ipsc->lAlphaFieldBlanks,
                          s_ipsc->cAlphaFieldNames,
                          s_ipsc->cNumericFieldNames);

        ErrorObjectHeader eoh{routineName, s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)};
        
        if (s_mat->materialMap.find(s_ipsc->cAlphaArgs(1)) != s_mat->materialMap.end()) {
            ShowSevereDuplicateName(state, eoh);
            ErrorsFound = true;
            continue;
        }

        auto *mat = new MaterialChild;
        mat->group = Group::BlindEquivalentLayer;
        mat->Name = s_ipsc->cAlphaArgs(1);

        s_mat->materials.push_back(mat);
        mat->Num = s_mat->materials.isize();
        s_mat->materialMap.insert_or_assign(mat->Name, mat->Num);
        
        mat->Roughness = SurfaceRoughness::Rough;
        mat->ROnly = true;

        mat->SlatOrientation = static_cast<DataWindowEquivalentLayer::Orientation>(
            getEnumValue(DataWindowEquivalentLayer::orientationNamesUC, s_ipsc->cAlphaArgs(2)));
                
        mat->SlatWidth = s_ipsc->rNumericArgs(1);
        mat->SlatSeparation = s_ipsc->rNumericArgs(2);
        mat->SlatCrown = s_ipsc->rNumericArgs(3);
        mat->SlatAngle = s_ipsc->rNumericArgs(4);

        mat->TausFrontBeamDiff = s_ipsc->rNumericArgs(5);
        mat->TausBackBeamDiff = s_ipsc->rNumericArgs(6);
        mat->ReflFrontBeamDiff = s_ipsc->rNumericArgs(7);
        mat->ReflBackBeamDiff = s_ipsc->rNumericArgs(8);

        if (!s_ipsc->lNumericFieldBlanks(9) && !s_ipsc->lNumericFieldBlanks(10) && !s_ipsc->lNumericFieldBlanks(11) && !s_ipsc->lNumericFieldBlanks(12)) {
            mat->TausFrontBeamDiffVis = s_ipsc->rNumericArgs(9);
            mat->TausBackBeamDiffVis = s_ipsc->rNumericArgs(10);
            mat->ReflFrontBeamDiffVis = s_ipsc->rNumericArgs(11);
            mat->ReflBackBeamDiffVis = s_ipsc->rNumericArgs(12);
        }
        if (!s_ipsc->lNumericFieldBlanks(13) && !s_ipsc->lNumericFieldBlanks(14) && !s_ipsc->lNumericFieldBlanks(15)) {
            mat->TausDiffDiff = s_ipsc->rNumericArgs(13);
            mat->ReflFrontDiffDiff = s_ipsc->rNumericArgs(14);
            mat->ReflBackDiffDiff = s_ipsc->rNumericArgs(15);
        }
        if (!s_ipsc->lNumericFieldBlanks(16) && !s_ipsc->lNumericFieldBlanks(17) && !s_ipsc->lNumericFieldBlanks(18)) {
            mat->TausDiffDiffVis = s_ipsc->rNumericArgs(13);
            mat->ReflFrontDiffDiffVis = s_ipsc->rNumericArgs(14);
            mat->ReflBackDiffDiffVis = s_ipsc->rNumericArgs(15);
        }
        if (!s_ipsc->lNumericFieldBlanks(19)) {
            mat->TausThermal = s_ipsc->rNumericArgs(19);
        }
        if (!s_ipsc->lNumericFieldBlanks(20)) {
            mat->EmissThermalFront = s_ipsc->rNumericArgs(20);
        }
        if (!s_ipsc->lNumericFieldBlanks(21)) {
            mat->EmissThermalBack = s_ipsc->rNumericArgs(21);
        }
        // Assumes thermal emissivity is the same as thermal absorptance
        mat->AbsorpThermalFront = mat->EmissThermalFront;
        mat->AbsorpThermalBack = mat->EmissThermalBack;
        mat->TransThermal = mat->TausThermal;

        // By default all blinds have fixed slat angle,
        //  they are used with window shading controls that adjust slat angles like
        //  MaximizeSolar or BlockBeamSolar
        mat->slatAngleType = SlatAngleType::FixedSlatAngle;
        if (!s_ipsc->lAlphaFieldBlanks(3)) {
            mat->slatAngleType = static_cast<SlatAngleType>(getEnumValue(slatAngleTypeNamesUC, Util::makeUPPER(s_ipsc->cAlphaArgs(3))));
        }
        if (mat->SlatWidth < mat->SlatSeparation) {
            ShowWarningError(state, format("{}=\"{}\", Slat Seperation/Width", s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)));
            ShowContinueError(state,
                              format("{} [{:.2R}] is less than {} [{:.2R}].",
                                     s_ipsc->cNumericFieldNames(1),
                                     mat->SlatWidth,
                                     s_ipsc->cNumericFieldNames(2),
                                     mat->SlatSeparation));
            ShowContinueError(state, "This will allow direct beam to be transmitted when Slat angle = 0.");
        }
        if (mat->SlatSeparation < 0.001) {
            ShowWarningError(state, format("{}=\"{}\", Slat Seperation", s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)));
            ShowContinueError(state, format("{} [{:.2R}]. Slate spacing must be > 0.0", s_ipsc->cNumericFieldNames(2), mat->SlatSeparation));
            ShowContinueError(state,
                              "...Setting slate spacing to default value of 0.025 m and "
                              "simulation continues.");
            mat->SlatSeparation = 0.025;
        }
        if (mat->SlatWidth < 0.001 || mat->SlatWidth >= 2.0 * mat->SlatSeparation) {
            ShowWarningError(state, format("{}=\"{}\", Slat Width", s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)));
            ShowContinueError(
                state, format("{} [{:.2R}]. Slat width range is 0 < Width <= 2*Spacing", s_ipsc->cNumericFieldNames(1), mat->SlatWidth));
            ShowContinueError(state, "...Setting slate width equal to slate spacing and simulation continues.");
            mat->SlatWidth = mat->SlatSeparation;
        }
        if (mat->SlatCrown < 0.0 || mat->SlatCrown >= 0.5 * mat->SlatWidth) {
            ShowWarningError(state, format("{}=\"{}\", Slat Crown", s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)));
            ShowContinueError(
                state, format("{} [{:.2R}]. Slat crwon range is 0 <= crown < 0.5*Width", s_ipsc->cNumericFieldNames(3), mat->SlatCrown));
            ShowContinueError(state, "...Setting slate crown to 0.0 and simulation continues.");
            mat->SlatCrown = 0.0;
        }
        if (mat->SlatAngle < -90.0 || mat->SlatAngle > 90.0) {
            ShowWarningError(state, format("{}=\"{}\", Slat Angle", s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)));
            ShowContinueError(state,
                              format("{} [{:.2R}]. Slat angle range is -90.0 <= Angle < 90.0", s_ipsc->cNumericFieldNames(4), mat->SlatAngle));
            ShowContinueError(state, "...Setting slate angle to 0.0 and simulation continues.");
            mat->SlatAngle = 0.0;
        }

        if ((s_ipsc->rNumericArgs(5) + s_ipsc->rNumericArgs(7) >= 1.0)) {
            ErrorsFound = true;
            ShowSevereError(state, format("{}=\"{}\", Illegal value combination.", s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)));
            ShowContinueError(state, format("{} + {} not < 1.0", s_ipsc->cNumericFieldNames(5), s_ipsc->cNumericFieldNames(7)));
        }
        if ((s_ipsc->rNumericArgs(6) + s_ipsc->rNumericArgs(8) >= 1.0)) {
            ErrorsFound = true;
            ShowSevereError(state, format("{}=\"{}\", Illegal value combination.", s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)));
            ShowContinueError(state, format("{} + {} not < 1.0", s_ipsc->cNumericFieldNames(6), s_ipsc->cNumericFieldNames(8)));
        }
        if ((s_ipsc->rNumericArgs(9) + s_ipsc->rNumericArgs(11) >= 1.0)) {
            ErrorsFound = true;
            ShowSevereError(state, format("{}=\"{}\", Illegal value combination.", s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)));
            ShowContinueError(state, format("{} + {} not < 1.0", s_ipsc->cNumericFieldNames(9), s_ipsc->cNumericFieldNames(11)));
        }
        if ((s_ipsc->rNumericArgs(10) + s_ipsc->rNumericArgs(12) >= 1.0)) {
            ErrorsFound = true;
            ShowSevereError(state, format("{}=\"{}\", Illegal value combination.", s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)));
            ShowContinueError(state, format("{} + {} not < 1.0", s_ipsc->cNumericFieldNames(10), s_ipsc->cNumericFieldNames(12)));
        }

    } // TotBlindsEQL loop

    // EcoRoof Materials
    // PSU 2006
    s_ipsc->cCurrentModuleObject = "Material:RoofVegetation";
    for (Loop = 1; Loop <= EcoRoofMat; ++Loop) {
        // Call Input Get Routine to retrieve material data from ecoroof

        s_ip->getObjectItem(state,
                          s_ipsc->cCurrentModuleObject,
                          Loop,
                          s_ipsc->cAlphaArgs,
                          MaterialNumAlpha,
                          s_ipsc->rNumericArgs,
                          MaterialNumProp,
                          IOStat,
                          s_ipsc->lNumericFieldBlanks,
                          s_ipsc->lAlphaFieldBlanks,
                          s_ipsc->cAlphaFieldNames,
                          s_ipsc->cNumericFieldNames);

        ErrorObjectHeader eoh{routineName, s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)};
        
        if (s_mat->materialMap.find(s_ipsc->cAlphaArgs(1)) != s_mat->materialMap.end()) {
            ShowSevereDuplicateName(state, eoh);
            ErrorsFound = true;
            continue;
        }

        // this part is similar to the regular material
        // Load the material derived type from the input data.
        auto *mat = new MaterialChild;
        mat->group = Group::EcoRoof;
        mat->Name = s_ipsc->cAlphaArgs(1);

        s_mat->materials.push_back(mat);
        mat->Num = s_mat->materials.isize();
        s_mat->materialMap.insert_or_assign(mat->Name, mat->Num);
        
        mat->HeightOfPlants = s_ipsc->rNumericArgs(1);
        mat->LAI = s_ipsc->rNumericArgs(2);
        mat->Lreflectivity = s_ipsc->rNumericArgs(3); // Albedo
        mat->LEmissitivity = s_ipsc->rNumericArgs(4);
        mat->RStomata = s_ipsc->rNumericArgs(5);

        mat->Name = s_ipsc->cAlphaArgs(1);
        // need to treat the A2 with is just the name of the soil(it is
        // not important)
        mat->Roughness = static_cast<SurfaceRoughness>(getEnumValue(surfaceRoughnessNamesUC, Util::makeUPPER(s_ipsc->cAlphaArgs(3))));

        if (s_ipsc->lAlphaFieldBlanks(4)) {
            mat->ecoRoofCalcMethod = EcoRoofCalcMethod::SchaapGenuchten;
        } else {
            mat->ecoRoofCalcMethod = static_cast<EcoRoofCalcMethod>(getEnumValue(ecoRoofCalcMethodNamesUC, s_ipsc->cAlphaArgs(4)));
        }

        mat->Thickness = s_ipsc->rNumericArgs(6);
        mat->Conductivity = s_ipsc->rNumericArgs(7);
        mat->Density = s_ipsc->rNumericArgs(8);
        mat->SpecHeat = s_ipsc->rNumericArgs(9);
        mat->AbsorpThermal = s_ipsc->rNumericArgs(10); // emissivity
        mat->AbsorpSolar = s_ipsc->rNumericArgs(11);   // (1 - Albedo)
        mat->AbsorpVisible = s_ipsc->rNumericArgs(12);
        mat->Porosity = s_ipsc->rNumericArgs(13);
        mat->MinMoisture = s_ipsc->rNumericArgs(14);
        mat->InitMoisture = s_ipsc->rNumericArgs(15);

        if (mat->Conductivity > 0.0) {
            mat->Resistance = state.dataHeatBal->NominalR(mat->Num) = mat->Thickness / mat->Conductivity;
        } else {
            ShowSevereError(state, format("{}=\"{}\" is not defined correctly.", s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)));
            ShowContinueError(state, format("{} is <=0.", s_ipsc->cNumericFieldNames(7)));
            ErrorsFound = true;
        }

        if (mat->InitMoisture > mat->Porosity) {
            ShowWarningError(state, format("{}=\"{}\", Illegal value combination.", s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)));
            ShowContinueError(state,
                              format("{} is greater than {}. It must be less or equal.", s_ipsc->cNumericFieldNames(15), s_ipsc->cNumericFieldNames(13)));
            ShowContinueError(state, format("{} = {:.3T}.", s_ipsc->cNumericFieldNames(13), mat->Porosity));
            ShowContinueError(state, format("{} = {:.3T}.", s_ipsc->cNumericFieldNames(15), mat->InitMoisture));
            ShowContinueError(
                state, format("{} is reset to the maximum (saturation) value = {:.3T}.", s_ipsc->cNumericFieldNames(15), mat->Porosity));
            ShowContinueError(state, "Simulation continues.");
            mat->InitMoisture = mat->Porosity;
        }
    }

    // Thermochromic glazing group
    // get the number of WindowMaterial:GlazingGroup:Thermochromic objects in the idf file
    s_ipsc->cCurrentModuleObject = "WindowMaterial:GlazingGroup:Thermochromic";
    state.dataHeatBal->TotTCGlazings = s_ip->getNumObjectsFound(state, s_ipsc->cCurrentModuleObject);
    if (state.dataHeatBal->TotTCGlazings >= 1) {
        // Read TC glazings
        state.dataHeatBal->TCGlazings.allocate(state.dataHeatBal->TotTCGlazings);

        for (Loop = 1; Loop <= state.dataHeatBal->TotTCGlazings; ++Loop) {
            // Get each TCGlazings from the input processor
            s_ip->getObjectItem(state,
                              s_ipsc->cCurrentModuleObject,
                              Loop,
                              s_ipsc->cAlphaArgs,
                              MaterialNumAlpha,
                              s_ipsc->rNumericArgs,
                              MaterialNumProp,
                              IOStat,
                              s_ipsc->lNumericFieldBlanks,
                              s_ipsc->lAlphaFieldBlanks,
                              s_ipsc->cAlphaFieldNames,
                              s_ipsc->cNumericFieldNames);

            ErrorObjectHeader eoh{routineName, s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)};
        
            if (s_mat->materialMap.find(s_ipsc->cAlphaArgs(1)) != s_mat->materialMap.end()) {
                ShowSevereDuplicateName(state, eoh);
                ErrorsFound = true;
                continue;
            }

            if (MaterialNumProp + 1 != MaterialNumAlpha) {
                ShowSevereError(state, format("{}=\"{}\" is not defined correctly.", s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)));
                ShowContinueError(state,
                                  format("Check number of {} compared to number of {}", s_ipsc->cAlphaFieldNames(2), s_ipsc->cNumericFieldNames(1)));
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

            state.dataHeatBal->TCGlazings(Loop).Name = s_ipsc->cAlphaArgs(1);
            state.dataHeatBal->TCGlazings(Loop).NumGlzMat = MaterialNumProp;

            for (iTC = 1; iTC <= MaterialNumProp; ++iTC) {
                state.dataHeatBal->TCGlazings(Loop).SpecTemp(iTC) = s_ipsc->rNumericArgs(iTC);
                state.dataHeatBal->TCGlazings(Loop).LayerName(iTC) = s_ipsc->cAlphaArgs(1 + iTC);

                // Find this glazing material in the material list
                iMat = Material::GetMaterialNum(state, s_ipsc->cAlphaArgs(1 + iTC));
                if (iMat != 0) {
                    // TC glazing
                    auto *mat = dynamic_cast<MaterialChild *>(s_mat->materials(iMat));
                    assert(mat != nullptr);
                    mat->SpecTemp = s_ipsc->rNumericArgs(iTC);
                    mat->TCParent = Loop;
                    state.dataHeatBal->TCGlazings(Loop).LayerPoint(iTC) = iMat;

                    // test that named material is of the right type
                    if (mat->group != Group::WindowGlass) {
                        ShowSevereError(
                            state, format("{}=\"{}\" is not defined correctly.", s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)));
                        ShowContinueError(state, format("Material named: {} is not a window glazing ", s_ipsc->cAlphaArgs(1 + iTC)));
                        ErrorsFound = true;
                    }

                } else { // thow error because not found
                    ShowSevereError(state,
                                    format("{}=\"{}\" is not defined correctly.", s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)));
                    ShowContinueError(state, format("Material named: {} was not found ", s_ipsc->cAlphaArgs(1 + iTC)));
                    ErrorsFound = true;
                }
            }
        }
    }
    
    auto &cCurrentModuleObject = s_ipsc->cCurrentModuleObject;
    cCurrentModuleObject = "WindowMaterial:SimpleGlazingSystem";
    for (Loop = 1; Loop <= state.dataHeatBal->TotSimpleWindow; ++Loop) {

        s_ip->getObjectItem(state,
                          cCurrentModuleObject,
                          Loop,
                          s_ipsc->cAlphaArgs,
                          MaterialNumAlpha,
                          s_ipsc->rNumericArgs,
                          MaterialNumProp,
                          IOStat,
                          s_ipsc->lNumericFieldBlanks,
                          s_ipsc->lAlphaFieldBlanks,
                          s_ipsc->cAlphaFieldNames,
                          s_ipsc->cNumericFieldNames);

        ErrorObjectHeader eoh{routineName, s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)};
        
        if (s_mat->materialMap.find(s_ipsc->cAlphaArgs(1)) != s_mat->materialMap.end()) {
            ShowSevereDuplicateName(state, eoh);
            ErrorsFound = true;
            continue;
        }

        auto *mat = new MaterialChild;
        mat->group = Group::WindowSimpleGlazing;
        mat->Name = s_ipsc->cAlphaArgs(1);

        s_mat->materials.push_back(mat);
        mat->Num = s_mat->materials.isize();
        s_mat->materialMap.insert_or_assign(mat->Name, mat->Num);
        
        mat->SimpleWindowUfactor = s_ipsc->rNumericArgs(1);
        mat->SimpleWindowSHGC = s_ipsc->rNumericArgs(2);
        if (!s_ipsc->lNumericFieldBlanks(3)) {
            mat->SimpleWindowVisTran = s_ipsc->rNumericArgs(3);
            mat->SimpleWindowVTinputByUser = true;
        }

        HeatBalanceManager::SetupSimpleWindowGlazingSystem(state, mat->Num);
    }

    // Simon: Place to load materials for complex fenestrations
    if ((s_mat->TotComplexShades > 0) || (state.dataHeatBal->TotComplexGaps > 0)) {
        int MaterNum = s_mat->materials.isize();
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

        for (auto const *mat : s_mat->materials) {

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

        for (auto *mat : s_mat->materials) {
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
    for (auto *mBase : s_mat->materials) {
            if (mBase->group == Material::Group::Screen || mBase->group == Material::Group::WindowBlind ||
                mBase->group == Material::Group::WindowGas ||mBase->group == Material::Group::WindowGasMixture ||
                mBase->group == Material::Group::GapEquivalentLayer)
            continue;

        auto *m = dynamic_cast<MaterialChild *>(mBase);
        assert(m != nullptr);
        m->phaseChange = HysteresisPhaseChange::HysteresisPhaseChange::factory(state, m->Name);
    }

    GetVariableAbsorptanceInput(state, ErrorsFound); // Read variable thermal and solar absorptance add-on data
}

void GetVariableAbsorptanceInput(EnergyPlusData &state, bool &errorsFound)
{
    constexpr std::string_view routineName = "GetVariableAbsorptanceInput";
    
    int IOStat; // IO Status when calling get input subroutine
    int numAlphas;
    int numNumbers;

    auto &s_ip = state.dataInputProcessing->inputProcessor;
    auto &s_ipsc = state.dataIPShortCut;
    auto &s_mat = state.dataMaterial; 

    s_ipsc->cCurrentModuleObject = "MaterialProperty:VariableAbsorptance";
    
    int numVariAbs = s_ip->getNumObjectsFound(state, s_ipsc->cCurrentModuleObject);
    state.dataHeatBal->AnyVariableAbsorptance = (numVariAbs > 0);
    for (int i = 1; i <= numVariAbs; ++i) {
        // Call Input Get routine to retrieve material data
        s_ip->getObjectItem(state,
                          s_ipsc->cCurrentModuleObject,
                          i,
                          s_ipsc->cAlphaArgs,
                          numAlphas,
                          s_ipsc->rNumericArgs,
                          numNumbers,
                          IOStat,
                          s_ipsc->lNumericFieldBlanks,
                          s_ipsc->lAlphaFieldBlanks,
                          s_ipsc->cAlphaFieldNames,
                          s_ipsc->cNumericFieldNames);

        ErrorObjectHeader eoh{routineName, s_ipsc->cCurrentModuleObject, s_ipsc->cAlphaArgs(1)};
        
        // Load the material derived type from the input data.
        int matNum = Material::GetMaterialNum(state, s_ipsc->cAlphaArgs(2));
        if (matNum == 0) {
            ShowSevereItemNotFound(state, eoh, s_ipsc->cAlphaFieldNames(2), s_ipsc->cAlphaArgs(2));
            errorsFound = true;
            return;
        }
    
        auto *mat = dynamic_cast<MaterialChild *>(s_mat->materials(matNum));
        assert(mat != nullptr);

        if (mat->group != Group::Regular) {
            ShowSevereError(
                state,
                format("{}: Reference Material is not appropriate type for Thermal/Solar Absorptance properties, material={}, must have regular "
                       "properties (Thermal/Solar Absorptance)",
                       s_ipsc->cCurrentModuleObject,
                       mat->Name));
            errorsFound = true;
            return;
        }

        mat->absorpVarCtrlSignal = VariableAbsCtrlSignal::SurfaceTemperature; // default value
        mat->absorpVarCtrlSignal = static_cast<VariableAbsCtrlSignal>(getEnumValue(variableAbsCtrlSignalNamesUC, s_ipsc->cAlphaArgs(3)));
        //    init to 0 as GetScheduleIndex returns 0 for not-found schedule
        mat->absorpThermalVarFuncIdx = Curve::GetCurveIndex(state, s_ipsc->cAlphaArgs(4));
        mat->absorpThermalVarSchedIdx = ScheduleManager::GetScheduleIndex(state, s_ipsc->cAlphaArgs(5));
        mat->absorpSolarVarFuncIdx = Curve::GetCurveIndex(state, s_ipsc->cAlphaArgs(6));
        mat->absorpSolarVarSchedIdx = ScheduleManager::GetScheduleIndex(state, s_ipsc->cAlphaArgs(7));
        if (mat->absorpVarCtrlSignal == VariableAbsCtrlSignal::Scheduled) {
            if ((mat->absorpThermalVarSchedIdx == 0) && (mat->absorpSolarVarSchedIdx == 0)) {
                ShowSevereError(
                    state,
                    format("{}: Control signal \"Scheduled\" is chosen but both thermal and solar absorptance schedules are undefined, for object {}",
                           s_ipsc->cCurrentModuleObject,
                           s_ipsc->cAlphaArgs(1)));
                errorsFound = true;
                return;
            }
            if ((mat->absorpThermalVarFuncIdx > 0) || (mat->absorpSolarVarFuncIdx > 0)) {
                ShowWarningError(state,
                                 format("{}: Control signal \"Scheduled\" is chosen. Thermal or solar absorptance function name is going to be "
                                        "ignored, for object {}",
                                        s_ipsc->cCurrentModuleObject,
                                        s_ipsc->cAlphaArgs(1)));
                errorsFound = true;
                return;
            }
        } else { // controlled by performance table or curve
            if ((mat->absorpThermalVarFuncIdx == 0) && (mat->absorpSolarVarFuncIdx == 0)) {
                ShowSevereError(state,
                                format("{}: Non-schedule control signal is chosen but both thermal and solar absorptance table or curve are "
                                       "undefined, for object {}",
                                       s_ipsc->cCurrentModuleObject,
                                       s_ipsc->cAlphaArgs(1)));
                errorsFound = true;
                return;
            }
            if ((mat->absorpThermalVarSchedIdx > 0) || (mat->absorpSolarVarSchedIdx > 0)) {
                ShowWarningError(state,
                                 format("{}: Non-schedule control signal is chosen. Thermal or solar absorptance schedule name is going to be "
                                        "ignored, for object {}",
                                        s_ipsc->cCurrentModuleObject,
                                        s_ipsc->cAlphaArgs(1)));
                errorsFound = true;
                return;
            }
        }
    }
}

void CalcScreenTransmittance([[maybe_unused]] EnergyPlusData &state,
                             MaterialScreen const *screen,
                             Real64 phi,   // Sun altitude relative to surface outward normal (radians, 0 to Pi)
                             Real64 theta, // Optional sun azimuth relative to surface outward normal (radians, 0 to Pi)
                             ScreenBmTransAbsRef &tar)
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

    Real64 Tdirect;          // Beam solar transmitted through screen (dependent on sun angle)
    Real64 Tscattered;       // Beam solar reflected through screen (dependent on sun angle)
    Real64 TscatteredVis;    // Visible beam solar reflected through screen (dependent on sun angle)

    assert(phi >= 0.0 && phi <= Constant::Pi);
    assert(theta >= 0.0 && theta <= Constant::Pi);

    Real64 sinPhi = std::sin(phi);
    Real64 cosPhi = std::cos(phi);
    Real64 tanPhi = sinPhi / cosPhi;
    Real64 cosTheta = std::cos(theta);

    bool sunInFront = (phi < Constant::PiOvr2) && (theta < Constant::PiOvr2); // Sun is in front of screen

    // ratio of screen material diameter to screen material spacing
    Real64 Gamma = screen->diameterToSpacingRatio;

    // ************************************************************************************************
    // * calculate transmittance of totally absorbing screen material (beam passing through open area)*
    // ************************************************************************************************

    // Now we need to normalize phi and theta to the 0 to Pi/2 range using reflection.
    if (phi > Constant::PiOvr2) phi = Constant::Pi - phi;
    if (theta > Constant::PiOvr2) theta = Constant::Pi - theta;

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
            TransXDir =
                1.0 - Gamma * (std::cos(MuPrime) + std::sin(MuPrime) * std::tan(std::acos(COSMu)) * std::sqrt(1.0 + pow_2(1.0 / std::tan(Eta))));
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

    if ((Constant::PiOvr2 - theta) < Small || (Constant::PiOvr2 - phi) < Small) {
        Tscattered = 0.0;
        TscatteredVis = 0.0;
    } else {
        //   DeltaMax and Delta are in degrees
        Real64 DeltaMax = 89.7 - (10.0 * Gamma / 0.16);
        Real64 Delta = std::sqrt(pow_2(theta / Constant::DegToRad) + pow_2(phi / Constant::DegToRad));

        //   Use empirical model to determine maximum (peak) scattering
        Real64 Tscattermax = 0.0229 * Gamma + 0.2971 * ReflCyl - 0.03624 * pow_2(Gamma) + 0.04763 * pow_2(ReflCyl) - 0.44416 * Gamma * ReflCyl;
        Real64 TscattermaxVis =
            0.0229 * Gamma + 0.2971 * ReflCylVis - 0.03624 * pow_2(Gamma) + 0.04763 * pow_2(ReflCylVis) - 0.44416 * Gamma * ReflCylVis;

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
void NormalizePhiTheta(Real64 &phi, Real64 &theta)
{

    while (phi > 2 * Constant::Pi)
        phi -= 2 * Constant::Pi;
    if (phi > Constant::Pi) phi = 2 * Constant::Pi - phi;

    while (theta > 2 * Constant::Pi)
        theta -= 2 * Constant::Pi;
    if (theta > Constant::Pi) theta = 2 * Constant::Pi - theta;
} // NormalizePhiTheta()

void GetPhiThetaIndices(Real64 phi, Real64 theta, Real64 dPhi, Real64 dTheta, int &iPhi1, int &iPhi2, int &iTheta1, int &iTheta2)
{
    iPhi1 = int(phi / dPhi);
    iPhi2 = (iPhi1 == maxIPhi - 1) ? iPhi1 : iPhi1 + 1;
    iTheta1 = int(theta / dTheta);
    iTheta2 = (iTheta1 == maxITheta - 1) ? iTheta1 : iTheta1 + 1;
} // GetPhiThetaIndices()


Real64 MaterialBlind::BeamBeamTrans(Real64 const ProfAng,        // Solar profile angle (rad)
                                    Real64 const SlatAng        // Slat angle (rad)
) const 
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Fred Winkelmann
    //       DATE WRITTEN   Jan 2002
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculates beam-to-beam transmittance of a window blind

    // METHODOLOGY EMPLOYED:
    // Based on solar profile angle and slat geometry

    Real64 CosProfAng = std::cos(ProfAng); // Cosine of profile angle
    Real64 gamma = SlatAng - ProfAng;
    Real64 wbar = this->SlatSeparation;
    if (CosProfAng != 0.0) wbar = this->SlatWidth * std::cos(gamma) / CosProfAng;
    Real64 BeamBeamTrans = max(0.0, 1.0 - std::abs(wbar / this->SlatSeparation));

    if (BeamBeamTrans > 0.0) {

        // Correction factor that accounts for finite thickness of slats. It is used to modify the
        // blind transmittance to account for reflection and absorption by the slat edges.
        // fEdge is ratio of area subtended by edge of slat to area between tops of adjacent slats.

        Real64 fEdge = 0.0; // Slat edge correction factor
        Real64 fEdge1 = 0.0;
        if (std::abs(std::sin(gamma)) > 0.01) {
            if ((SlatAng > 0.0 && SlatAng <= Constant::PiOvr2 && ProfAng <= SlatAng) ||
                (SlatAng > Constant::PiOvr2 && SlatAng <= Constant::Pi && ProfAng > -(Constant::Pi - SlatAng)))
                fEdge1 = this->SlatThickness * std::abs(std::sin(gamma)) / ((this->SlatSeparation + this->SlatThickness / std::abs(std::sin(SlatAng))) * CosProfAng);
                fEdge = min(1.0, std::abs(fEdge1));
            }
            BeamBeamTrans *= (1.0 - fEdge);
        }

        return BeamBeamTrans;

    } // MaterialBlind::BeamBeamTrans()


void GetProfIndices(Real64 profAng, int &idxLo, int &idxHi)
{
    idxLo = int((profAng + Constant::PiOvr2) / dProfAng) + 1;
    idxHi = std::min(MaxProfAngs, idxLo+1);
}
                
void GetSlatIndicesInterpFac(Real64 slatAng, int &idxLo, int &idxHi, Real64 &interpFac)
{
    idxLo = int(slatAng / dSlatAng);
    idxHi = std::min(MaxSlatAngs, idxLo+1);
    interpFac = (slatAng - (idxLo * dSlatAng)) / dSlatAng;
}
        
        
} // namespace EnergyPlus::Material

