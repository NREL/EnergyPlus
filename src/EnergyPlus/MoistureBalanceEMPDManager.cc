// EnergyPlus, Copyright (c) 1996-2023, The Board of Trustees of the University of Illinois,
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
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataMoistureBalance.hh>
#include <EnergyPlus/DataMoistureBalanceEMPD.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/Material.hh>
#include <EnergyPlus/MoistureBalanceEMPDManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/ZoneTempPredictorCorrector.hh>

namespace EnergyPlus::MoistureBalanceEMPDManager {

// Module containing the routines to calculate moisture adsorption and desorption
// at interior wall surfaces

// MODULE INFORMATION:
//   Authors:        Muthusamy Swami and Lixing Gu
//   Date written:   August, 1999
//   Modified:       na
//   Re-engineered:  Jason Woods and Noel Merket, August 2015

// PURPOSE OF THIS MODULE:
// To calculate moisture adsorption and desorption at interior wall surfaces
// using EMPD model (Effective Moisture Penetration Depth) developed by
// Florida Solar Energy Center. Input consists of interior surface temperatures
// and sorption curve of interior layer materials. Output consists of moisture
// fluxes from wall interior surfaces, which will be used in zone moisture balance.

// METHODOLOGY EMPLOYED:
// Add something
// EMPD is a simplified method of analyzing moisture transport in buildings and
// is easy to incorporate into existing building energy analysis computer codes.
// The components of the moisture balance equation involving moisture adsorption
// and desorption are described in detail where the concept of EMPD is discussed.
// The assumptions. parameters required, and limitations of the model are also discussed.
// Results of simulation using the model and comparison with measured data are given.
// Data of isotherms compiled from the literature of some commonly used building materials are also given.

// REFERENCES:
// Kerestecioglu A A., Swami M V., Kamel A A., "Theoretical and computational
// investigation of simultaneous heat and moisture transfer in buildings: 'Effective
// penetration depth' theory," ASHRAE Trans., 1990, Vol. 96, Part 1, 447-454

// Using/Aliasing
using namespace DataHeatBalance;
using namespace DataMoistureBalanceEMPD;

Real64 CalcDepthFromPeriod(EnergyPlusData &state,
                           Real64 const period,                  // in seconds
                           Material::MaterialBase const *matBase // material
)
{

    // Assume T, RH, P
    Real64 constexpr T = 24.0; // C
    Real64 constexpr RH = 0.45;
    Real64 constexpr P_amb = 101325; // Pa

    // Calculate saturation vapor pressure at assumed temperature
    Real64 const PV_sat = Psychrometrics::PsyPsatFnTemp(state, T, "CalcDepthFromPeriod");

    auto const *mat = dynamic_cast<const Material::MaterialChild *>(matBase);
    assert(mat != nullptr);
    // Calculate slope of moisture sorption curve
    Real64 const slope_MC = mat->MoistACoeff * mat->MoistBCoeff * std::pow(RH, mat->MoistBCoeff - 1) +
                            mat->MoistCCoeff * mat->MoistDCoeff * std::pow(RH, mat->MoistDCoeff - 1);

    // Equation for the diffusivity of water vapor in air
    Real64 const diffusivity_air = 2.0e-7 * std::pow(T + 273.15, 0.81) / P_amb;

    // Convert mu to diffusivity [kg/m^2-s-Pa]
    Real64 const EMPDdiffusivity = diffusivity_air / mat->EMPDmu;

    // Calculate penetration depth
    Real64 const PenetrationDepth = std::sqrt(EMPDdiffusivity * PV_sat * period / (mat->Density * slope_MC * DataGlobalConstants::Pi));

    return PenetrationDepth;
}

void GetMoistureBalanceEMPDInput(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Muthusamy V. Swami and Lixing Gu
    //       DATE WRITTEN   August 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is the main driver for initializations within the
    // heat balance using the EMPD model.

    // Using/Aliasing

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int IOStat;                       // IO Status when calling get input subroutine
    Array1D_string MaterialNames(3);  // Number of Material Alpha names defined
    int MaterNum;                     // Counter to keep track of the material number
    int MaterialNumAlpha;             // Number of material alpha names being passed
    int MaterialNumProp;              // Number of material properties being passed
    Array1D<Real64> MaterialProps(9); // Temporary array to transfer material properties
    bool ErrorsFound(false);          // If errors detected in input

    int EMPDMat; // EMPD Moisture Material additional properties for each base material
    int Loop;
    int Layer;
    int SurfNum;           // Surface number
    int MatNum;            // Material number at interior layer
    int ConstrNum;         // Construction number
    Array1D_bool EMPDzone; // EMPD property check for each zone

    auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;

    // Load the additional EMPD Material properties
    cCurrentModuleObject = "MaterialProperty:MoisturePenetrationDepth:Settings";
    EMPDMat = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    if (EMPDMat == 0) {
        ShowSevereError(state, format("EMPD Solution requested, but no \"{}\" objects were found.", cCurrentModuleObject));
        ErrorsFound = true;
    }

    for (Loop = 1; Loop <= EMPDMat; ++Loop) {

        // Call Input Get routine to retrieve material data
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 Loop,
                                                                 MaterialNames,
                                                                 MaterialNumAlpha,
                                                                 MaterialProps,
                                                                 MaterialNumProp,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);

        // Load the material derived type from the input data.
        MaterNum = UtilityRoutines::FindItemInPtrList(MaterialNames(1), state.dataMaterial->Material);
        if (MaterNum == 0) {
            ShowSevereError(state,
                            format("{}: invalid {} entered={}, must match to a valid Material name.",
                                   cCurrentModuleObject,
                                   state.dataIPShortCut->cAlphaFieldNames(1),
                                   MaterialNames(1)));
            ErrorsFound = true;
            continue;
        }

        auto *material(dynamic_cast<Material::MaterialChild *>(state.dataMaterial->Material(MaterNum)));
        assert(material != nullptr);
        // See if Material was defined with R only.  (No density is defined then and not applicable for EMPD).
        //  What about materials other than "regular materials" (e.g. Glass, Air, etc)
        if (material->group == Material::Group::Regular && MaterialProps(1) > 0.0) {
            if (material->ROnly) {
                //        CALL ShowSevereError('EMPD base material = "'//TRIM(dataMaterial.Material(MaterNum)%Name)//  &
                //                             '" was Material:NoMass. It cannot be used for EMPD calculations.')
                ShowContinueError(state, "..Only Material base materials are allowed to have EMPD properties.");
                ShowSevereError(
                    state,
                    format("{}: Reference Material is not appropriate type for EMPD properties, material={}, must have regular properties (L,Cp,K,D)",
                           cCurrentModuleObject,
                           material->Name));
                ErrorsFound = true;
            }
        }
        if (material->group != Material::Group::Regular) {
            //      CALL ShowSevereError('GetMoistureBalanceEMPDInput: Only Material:Regular base materials are allowed '// &
            //                           'to have EMPD properties, material = '// TRIM(dataMaterial.Material(MaterNum)%Name))
            ShowSevereError(
                state,
                format("{}: Reference Material is not appropriate type for EMPD properties, material={}, must have regular properties (L,Cp,K,D)",
                       cCurrentModuleObject,
                       material->Name));
            ErrorsFound = true;
        }

        // Once the material derived type number is found then load the additional moisture material properties
        material->EMPDmu = MaterialProps(1);
        material->MoistACoeff = MaterialProps(2);
        material->MoistBCoeff = MaterialProps(3);
        material->MoistCCoeff = MaterialProps(4);
        material->MoistDCoeff = MaterialProps(5);
        if (state.dataIPShortCut->lNumericFieldBlanks(6) || MaterialProps(6) == DataGlobalConstants::AutoCalculate) {
            material->EMPDSurfaceDepth = CalcDepthFromPeriod(state, 24 * 3600, material); // 1 day
        } else {
            material->EMPDSurfaceDepth = MaterialProps(6);
        }
        if (state.dataIPShortCut->lNumericFieldBlanks(7) || MaterialProps(7) == DataGlobalConstants::AutoCalculate) {
            material->EMPDDeepDepth = CalcDepthFromPeriod(state, 21 * 24 * 3600, material); // 3 weeks
        } else {
            material->EMPDDeepDepth = MaterialProps(7);
        }
        material->EMPDCoatingThickness = MaterialProps(8);
        material->EMPDmuCoating = MaterialProps(9);

        if (material->EMPDDeepDepth <= material->EMPDSurfaceDepth && material->EMPDDeepDepth != 0.0) {
            ShowWarningError(state, format("{}: material=\"{}\"", cCurrentModuleObject, material->Name));
            ShowContinueError(state, "Deep-layer penetration depth should be zero or greater than the surface-layer penetration depth.");
        }
    }

    // Ensure at least one interior EMPD surface for each zone
    EMPDzone.dimension(state.dataGlobal->NumOfZones, false);
    for (SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
        if (!state.dataSurface->Surface(SurfNum).HeatTransSurf || state.dataSurface->Surface(SurfNum).Class == DataSurfaces::SurfaceClass::Window)
            continue; // Heat transfer surface only and not a window
        if (state.dataSurface->Surface(SurfNum).HeatTransferAlgorithm != DataSurfaces::HeatTransferModel::EMPD) continue;
        ConstrNum = state.dataSurface->Surface(SurfNum).Construction;
        auto const &thisConstruct = state.dataConstruction->Construct(ConstrNum);
        MatNum = thisConstruct.LayerPoint(state.dataConstruction->Construct(ConstrNum).TotLayers);
        auto const *thisMaterial = dynamic_cast<const Material::MaterialChild *>(state.dataMaterial->Material(MatNum));
        assert(thisMaterial != nullptr);
        if (thisMaterial->EMPDmu > 0.0 && state.dataSurface->Surface(SurfNum).Zone > 0) {
            EMPDzone(state.dataSurface->Surface(SurfNum).Zone) = true;
        } else {
            ++state.dataMoistureBalEMPD->ErrCount;
            if (state.dataMoistureBalEMPD->ErrCount == 1 && !state.dataGlobal->DisplayExtraWarnings) {
                ShowMessage(state, "GetMoistureBalanceEMPDInput: EMPD properties are not assigned to the inside layer of Surfaces");
                ShowContinueError(state, "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual surfaces.");
            }
            if (state.dataGlobal->DisplayExtraWarnings) {
                ShowMessage(state,
                            format("GetMoistureBalanceEMPDInput: EMPD properties are not assigned to the inside layer in Surface={}",
                                   state.dataSurface->Surface(SurfNum).Name));
                ShowContinueError(state, format("with Construction={}", thisConstruct.Name));
            }
        }
        if (thisConstruct.TotLayers == 1) { // One layer construction
            continue;
        } else { // Multiple layer construction
            if (dynamic_cast<Material::MaterialChild *>(state.dataMaterial->Material(thisConstruct.LayerPoint(1)))->EMPDMaterialProps &&
                state.dataSurface->Surface(SurfNum).ExtBoundCond <= 0) { // The external layer is not exposed to zone
                ShowSevereError(
                    state, "GetMoistureBalanceEMPDInput: EMPD properties are assigned to the outside layer in Construction=" + thisConstruct.Name);
                ShowContinueError(
                    state, "..Outside layer material with EMPD properties = " + state.dataMaterial->Material(thisConstruct.LayerPoint(1))->Name);
                ShowContinueError(state, "..A material with EMPD properties must be assigned to the inside layer of a construction.");
                ErrorsFound = true;
            }
            for (Layer = 2; Layer <= thisConstruct.TotLayers - 1; ++Layer) {
                if (dynamic_cast<Material::MaterialChild *>(state.dataMaterial->Material(thisConstruct.LayerPoint(Layer)))->EMPDMaterialProps) {
                    ShowSevereError(
                        state, "GetMoistureBalanceEMPDInput: EMPD properties are assigned to a middle layer in Construction=" + thisConstruct.Name);
                    ShowContinueError(state,
                                      "..Middle layer material with EMPD properties = " +
                                          state.dataMaterial->Material(thisConstruct.LayerPoint(Layer))->Name);
                    ShowContinueError(state, "..A material with EMPD properties must be assigned to the inside layer of a construction.");
                    ErrorsFound = true;
                }
            }
        }
    }

    for (Loop = 1; Loop <= state.dataGlobal->NumOfZones; ++Loop) {
        if (!EMPDzone(Loop)) {
            ShowSevereError(state,
                            format("GetMoistureBalanceEMPDInput: None of the constructions for zone = {} has an inside layer with EMPD properties",
                                   state.dataHeatBal->Zone(Loop).Name));
            ShowContinueError(state, "..For each zone, the inside layer of at least one construction must have EMPD properties");
            ErrorsFound = true;
        }
    }

    EMPDzone.deallocate();

    ReportMoistureBalanceEMPD(state);

    if (ErrorsFound) {
        ShowFatalError(state, "GetMoistureBalanceEMPDInput: Errors found getting EMPD material properties, program terminated.");
    }
}

void InitMoistureBalanceEMPD(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //   Authors:        Muthusamy Swami and Lixing Gu
    //   Date written:   August, 1999
    //   Modified:       na
    //   Re-engineered:  na

    // PURPOSE OF THIS SUBROUTINE:
    // Create dynamic array for surface moisture calculation

    // USE STATEMENTS:
    using Psychrometrics::PsyRhovFnTdbRh;
    using Psychrometrics::PsyRhovFnTdbWPb_fast;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ZoneNum;
    int SurfNum;

    if (state.dataMoistureBalEMPD->InitEnvrnFlag) {
        state.dataMstBalEMPD->RVSurfaceOld.allocate(state.dataSurface->TotSurfaces);
        state.dataMstBalEMPD->RVSurface.allocate(state.dataSurface->TotSurfaces);
        state.dataMstBalEMPD->HeatFluxLatent.allocate(state.dataSurface->TotSurfaces);
        state.dataMoistureBalEMPD->EMPDReportVars.allocate(state.dataSurface->TotSurfaces);
        state.dataMstBalEMPD->RVSurfLayer.allocate(state.dataSurface->TotSurfaces);
        state.dataMstBalEMPD->RVSurfLayerOld.allocate(state.dataSurface->TotSurfaces);
        state.dataMstBalEMPD->RVDeepLayer.allocate(state.dataSurface->TotSurfaces);
        state.dataMstBalEMPD->RVdeepOld.allocate(state.dataSurface->TotSurfaces);
        state.dataMstBalEMPD->RVwall.allocate(state.dataSurface->TotSurfaces);
    }

    for (SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
        ZoneNum = state.dataSurface->Surface(SurfNum).Zone;
        if (!state.dataSurface->Surface(SurfNum).HeatTransSurf) continue;
        Real64 const rv_air_in_initval =
            min(PsyRhovFnTdbWPb_fast(state.dataZoneTempPredictorCorrector->zoneHeatBalance(ZoneNum).MAT,
                                     max(state.dataZoneTempPredictorCorrector->zoneHeatBalance(ZoneNum).ZoneAirHumRat, 1.0e-5),
                                     state.dataEnvrn->OutBaroPress),
                PsyRhovFnTdbRh(state, state.dataZoneTempPredictorCorrector->zoneHeatBalance(ZoneNum).MAT, 1.0, "InitMoistureBalanceEMPD"));
        state.dataMstBalEMPD->RVSurfaceOld(SurfNum) = rv_air_in_initval;
        state.dataMstBalEMPD->RVSurface(SurfNum) = rv_air_in_initval;
        state.dataMstBalEMPD->RVSurfLayer(SurfNum) = rv_air_in_initval;
        state.dataMstBalEMPD->RVSurfLayerOld(SurfNum) = rv_air_in_initval;
        state.dataMstBalEMPD->RVDeepLayer(SurfNum) = rv_air_in_initval;
        state.dataMstBalEMPD->RVdeepOld(SurfNum) = rv_air_in_initval;
        state.dataMstBalEMPD->RVwall(SurfNum) = rv_air_in_initval;
    }
    if (!state.dataMoistureBalEMPD->InitEnvrnFlag) return;
    // Initialize the report variable

    GetMoistureBalanceEMPDInput(state);

    for (SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
        if (!state.dataSurface->Surface(SurfNum).HeatTransSurf) continue;
        if (state.dataSurface->Surface(SurfNum).Class == DataSurfaces::SurfaceClass::Window) continue;
        EMPDReportVarsData &rvd = state.dataMoistureBalEMPD->EMPDReportVars(SurfNum);
        const std::string surf_name = state.dataSurface->Surface(SurfNum).Name;
        SetupOutputVariable(state,
                            "EMPD Surface Inside Face Water Vapor Density",
                            OutputProcessor::Unit::kg_m3,
                            rvd.rv_surface,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::State,
                            surf_name);
        SetupOutputVariable(state,
                            "EMPD Surface Layer Moisture Content",
                            OutputProcessor::Unit::kg_m3,
                            rvd.u_surface_layer,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::State,
                            surf_name);
        SetupOutputVariable(state,
                            "EMPD Deep Layer Moisture Content",
                            OutputProcessor::Unit::kg_m3,
                            rvd.u_deep_layer,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::State,
                            surf_name);
        SetupOutputVariable(state,
                            "EMPD Surface Layer Equivalent Relative Humidity",
                            OutputProcessor::Unit::Perc,
                            rvd.RH_surface_layer,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::State,
                            surf_name);
        SetupOutputVariable(state,
                            "EMPD Deep Layer Equivalent Relative Humidity",
                            OutputProcessor::Unit::Perc,
                            rvd.RH_deep_layer,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::State,
                            surf_name);
        SetupOutputVariable(state,
                            "EMPD Surface Layer Equivalent Humidity Ratio",
                            OutputProcessor::Unit::kgWater_kgDryAir,
                            rvd.w_surface_layer,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::State,
                            surf_name);
        SetupOutputVariable(state,
                            "EMPD Deep Layer Equivalent Humidity Ratio",
                            OutputProcessor::Unit::kgWater_kgDryAir,
                            rvd.w_deep_layer,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::State,
                            surf_name);
        SetupOutputVariable(state,
                            "EMPD Surface Moisture Flux to Zone",
                            OutputProcessor::Unit::kg_m2s,
                            rvd.mass_flux_zone,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::State,
                            surf_name);
        SetupOutputVariable(state,
                            "EMPD Deep Layer Moisture Flux",
                            OutputProcessor::Unit::kg_m2s,
                            rvd.mass_flux_deep,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::State,
                            surf_name);
    }

    if (state.dataMoistureBalEMPD->InitEnvrnFlag) state.dataMoistureBalEMPD->InitEnvrnFlag = false;
}

void CalcMoistureBalanceEMPD(EnergyPlusData &state,
                             int const SurfNum,
                             Real64 const SurfTempIn, // INSIDE SURFACE TEMPERATURE at current time step
                             Real64 const TempZone,   // Zone temperature at current time step.
                             Real64 &TempSat          // Saturated surface temperature.
)
{

    // SUBROUTINE INFORMATION:
    //   Authors:        Muthusamy Swami and Lixing Gu
    //   Date written:   August, 1999
    //   Modified:       na
    //   Re-engineered:  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculate surface moisture level using EMPD model

    // Using/Aliasing
    using DataMoistureBalanceEMPD::Lam;
    using Psychrometrics::PsyCpAirFnW;
    using Psychrometrics::PsyPsatFnTemp;
    using Psychrometrics::PsyRhFnTdbRhov;
    using Psychrometrics::PsyRhFnTdbRhovLBnd0C;
    using Psychrometrics::PsyRhFnTdbWPb;
    using Psychrometrics::PsyRhoAirFnPbTdbW;
    using Psychrometrics::PsyRhovFnTdbRh;
    using Psychrometrics::PsyRhovFnTdbWPb;
    using Psychrometrics::PsyRhovFnTdbWPb_fast;
    using Psychrometrics::PsyWFnTdbRhPb;

    static constexpr std::string_view RoutineName("CalcMoistureEMPD");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int NOFITR;           // Number of iterations
    int MatNum;           // Material number at interior layer
    int ConstrNum;        // Construction number
    Real64 hm_deep_layer; // Overall deep-layer transfer coefficient
    Real64 RSurfaceLayer; // Mass transfer resistance between actual surface and surface layer node
    Real64 Taver;         // Average zone temperature between current time and previous time
    //    REAL(r64)    :: Waver     ! Average zone humidity ratio between current time and previous time
    Real64 RHaver; // Average zone relative humidity {0-1} between current time and previous time
    Real64 RVaver; // Average zone vapor density
    Real64 dU_dRH;
    int Flag;             // Convergence flag (0 - converged)
    Real64 PVsurf;        // Surface vapor pressure
    Real64 PV_surf_layer; // Vapor pressure of surface layer
    Real64 PV_deep_layer;
    Real64 PVsat; // saturation vapor pressure at the surface
    Real64 RH_surf_layer_old;
    Real64 RH_deep_layer_old;
    Real64 EMPDdiffusivity;
    Real64 Rcoating;
    Real64 RH_surf_layer;
    Real64 RH_surf_layer_tmp;
    Real64 RH_deep_layer;

    if (state.dataGlobal->BeginEnvrnFlag && state.dataMoistureBalEMPD->OneTimeFlag) {
        InitMoistureBalanceEMPD(state);
        state.dataMoistureBalEMPD->OneTimeFlag = false;
    }

    if (!state.dataGlobal->BeginEnvrnFlag) {
        state.dataMoistureBalEMPD->OneTimeFlag = true;
    }

    auto const &surface(state.dataSurface->Surface(SurfNum));                // input
    auto &rv_surface(state.dataMstBalEMPD->RVSurface(SurfNum));              // output
    auto const &rv_surface_old(state.dataMstBalEMPD->RVSurfaceOld(SurfNum)); // input
    auto const &h_mass_conv_in_fd(state.dataMstBal->HMassConvInFD(SurfNum)); // input
    auto const &rho_vapor_air_in(state.dataMstBal->RhoVaporAirIn(SurfNum));  // input
    Real64 RHZone;
    Real64 mass_flux_surf_deep;
    Real64 mass_flux_surf_deep_max;
    Real64 mass_flux_zone_surf;
    Real64 mass_flux_zone_surf_max;
    Real64 mass_flux_surf_layer;
    Real64 mass_flux_deep_layer;
    Real64 mass_flux_zone;
    auto &rv_surf_layer(state.dataMstBalEMPD->RVSurfLayer(SurfNum));              // output
    auto const &rv_surf_layer_old(state.dataMstBalEMPD->RVSurfLayerOld(SurfNum)); // input
    Real64 hm_surf_layer;
    auto &rv_deep_layer(state.dataMstBalEMPD->RVDeepLayer(SurfNum));       // output
    auto const &rv_deep_old(state.dataMstBalEMPD->RVdeepOld(SurfNum));     // input
    auto &heat_flux_latent(state.dataMstBalEMPD->HeatFluxLatent(SurfNum)); // output

    heat_flux_latent = 0.0;
    Flag = 1;
    NOFITR = 0;
    if (!surface.HeatTransSurf) {
        return;
    }
    ConstrNum = surface.Construction;
    MatNum = state.dataConstruction->Construct(ConstrNum).LayerPoint(
        state.dataConstruction->Construct(ConstrNum).TotLayers); // Then find the material pointer

    auto const *material(dynamic_cast<Material::MaterialChild *>(state.dataMaterial->Material(MatNum)));
    assert(material != nullptr);
    if (material->EMPDmu <= 0.0) {
        rv_surface = PsyRhovFnTdbWPb(
            TempZone, state.dataZoneTempPredictorCorrector->zoneHeatBalance(surface.Zone).ZoneAirHumRat, state.dataEnvrn->OutBaroPress);
        return;
    }

    Taver = SurfTempIn;
    // Calculate average vapor density [kg/m^3], and RH for use in material property calculations.
    RVaver = rv_surface_old;
    RHaver = RVaver * 461.52 * (Taver + DataGlobalConstants::KelvinConv) * std::exp(-23.7093 + 4111.0 / (Taver + 237.7));

    // Calculate the saturated vapor pressure, surface vapor pressure and dewpoint. Used to check for condensation in HeatBalanceSurfaceManager
    PVsat = PsyPsatFnTemp(state, Taver, RoutineName);
    PVsurf = RHaver * std::exp(23.7093 - 4111.0 / (Taver + 237.7));
    TempSat = 4111.0 / (23.7093 - std::log(PVsurf)) + 35.45 - DataGlobalConstants::KelvinConv;

    // Convert vapor resistance factor (user input) to diffusivity. Evaluate at local surface temperature.
    // 2e-7*T^0.81/P = vapor diffusivity in air. [kg/m-s-Pa]
    // 461.52 = universal gas constant for water [J/kg-K]
    // EMPDdiffusivity = [m^2/s]
    EMPDdiffusivity = (2.0e-7 * pow(Taver + DataGlobalConstants::KelvinConv, 0.81) / state.dataEnvrn->OutBaroPress) / material->EMPDmu * 461.52 *
                      (Taver + DataGlobalConstants::KelvinConv);

    // Calculate slope of moisture sorption curve at current RH. [kg/kg-RH]
    dU_dRH = material->MoistACoeff * material->MoistBCoeff * pow(RHaver, material->MoistBCoeff - 1) +
             material->MoistCCoeff * material->MoistDCoeff * pow(RHaver, material->MoistDCoeff - 1);

    // Convert vapor density and temperature of zone air to RH
    RHZone = rho_vapor_air_in * 461.52 * (TempZone + DataGlobalConstants::KelvinConv) *
             std::exp(-23.7093 + 4111.0 / ((TempZone + DataGlobalConstants::KelvinConv) - 35.45));

    // Convert stored vapor density from previous timestep to RH.
    RH_deep_layer_old = PsyRhFnTdbRhov(state, Taver, rv_deep_old);
    RH_surf_layer_old = PsyRhFnTdbRhov(state, Taver, rv_surf_layer_old);

    // If coating vapor resistance factor equals 0, coating resistance is zero (avoid divide by zero).
    // Otherwise, calculate coating resistance with coating vapor resistance factor and thickness. [s/m]
    if (material->EMPDmuCoating <= 0.0) {
        Rcoating = 0;
    } else {
        Rcoating = material->EMPDCoatingThickness * material->EMPDmuCoating * state.dataEnvrn->OutBaroPress /
                   (2.0e-7 * pow(Taver + DataGlobalConstants::KelvinConv, 0.81) * 461.52 * (Taver + DataGlobalConstants::KelvinConv));
    }

    // Calculate mass-transfer coefficient between zone air and center of surface layer. [m/s]
    hm_surf_layer = 1.0 / (0.5 * material->EMPDSurfaceDepth / EMPDdiffusivity + 1.0 / h_mass_conv_in_fd + Rcoating);
    // Calculate mass-transfer coefficient between center of surface layer and center of deep layer. [m/s]
    // If deep layer depth = 0, set mass-transfer coefficient to zero (simulates with no deep layer).
    if (material->EMPDDeepDepth <= 0.0) {
        hm_deep_layer = 0;
    } else {
        hm_deep_layer = 2.0 * EMPDdiffusivity / (material->EMPDDeepDepth + material->EMPDSurfaceDepth);
    }
    // Calculate resistance between surface-layer/air interface and center of surface layer. [s/m]
    // This is the physical surface of the material.
    RSurfaceLayer = 1.0 / hm_surf_layer - 1.0 / h_mass_conv_in_fd;

    // Calculate vapor flux leaving surface layer, entering deep layer, and entering zone.
    mass_flux_surf_deep_max =
        material->EMPDDeepDepth * material->Density * dU_dRH * (RH_surf_layer_old - RH_deep_layer_old) / (state.dataGlobal->TimeStepZone * 3600.0);
    mass_flux_surf_deep = hm_deep_layer * (rv_surf_layer_old - rv_deep_old);
    if (std::abs(mass_flux_surf_deep_max) < std::abs(mass_flux_surf_deep)) {
        mass_flux_surf_deep = mass_flux_surf_deep_max;
    }

    mass_flux_zone_surf_max =
        material->EMPDSurfaceDepth * material->Density * dU_dRH * (RHZone - RH_surf_layer_old) / (state.dataGlobal->TimeStepZone * 3600.0);
    mass_flux_zone_surf = hm_surf_layer * (rho_vapor_air_in - rv_surf_layer_old);
    if (std::abs(mass_flux_zone_surf_max) < std::abs(mass_flux_zone_surf)) {
        mass_flux_zone_surf = mass_flux_zone_surf_max;
    }

    // mass_flux_surf_layer = -mass_flux_zone_surf + mass_flux_surf_deep;
    // mass_flux_deep_layer = mass_flux_surf_deep;
    // mass_flux_zone = -mass_flux_zone_surf;

    mass_flux_surf_layer = hm_surf_layer * (rv_surf_layer_old - rho_vapor_air_in) + hm_deep_layer * (rv_surf_layer_old - rv_deep_old);
    mass_flux_deep_layer = hm_deep_layer * (rv_surf_layer_old - rv_deep_old);
    mass_flux_zone = hm_surf_layer * (rv_surf_layer_old - rho_vapor_air_in);

    // Calculate new surface layer RH using mass balance on surface layer
    RH_surf_layer_tmp = RH_surf_layer_old +
                        state.dataGlobal->TimeStepZone * 3600.0 * (-mass_flux_surf_layer / (material->Density * material->EMPDSurfaceDepth * dU_dRH));

    //    RH_surf_layer = RH_surf_layer_tmp;

    if (RH_surf_layer_old < RH_deep_layer_old && RH_surf_layer_old < RHZone) {
        if (RHZone > RH_deep_layer_old) {
            if (RH_surf_layer_tmp > RHZone) {
                RH_surf_layer = RHZone;
            } else {
                RH_surf_layer = RH_surf_layer_tmp;
            }
        } else if (RH_surf_layer_tmp > RH_deep_layer_old) {
            RH_surf_layer = RH_deep_layer_old;
        } else {
            RH_surf_layer = RH_surf_layer_tmp;
        }

    } else if (RH_surf_layer_old < RH_deep_layer_old && RH_surf_layer_old > RHZone) {
        if (RH_surf_layer_tmp > RH_deep_layer_old) {
            RH_surf_layer = RH_deep_layer_old;
        } else if (RH_surf_layer_tmp < RHZone) {
            RH_surf_layer = RHZone;
        } else {
            RH_surf_layer = RH_surf_layer_tmp;
        }
    } else if (RH_surf_layer_old > RH_deep_layer_old && RH_surf_layer_old < RHZone) {
        if (RH_surf_layer_tmp > RHZone) {
            RH_surf_layer = RHZone;
        } else if (RH_surf_layer_tmp < RH_deep_layer_old) {
            RH_surf_layer = RH_deep_layer_old;
        } else
            RH_surf_layer = RH_surf_layer_tmp;
    } else if (RHZone < RH_deep_layer_old) {
        if (RH_surf_layer_tmp < RHZone) {
            RH_surf_layer = RHZone;
        } else {
            RH_surf_layer = RH_surf_layer_tmp;
        }
    } else if (RH_surf_layer_tmp < RH_deep_layer_old) {
        RH_surf_layer = RH_deep_layer_old;
    } else {
        RH_surf_layer = RH_surf_layer_tmp;
    }

    // Calculate new deep layer RH using mass balance on deep layer (unless depth <= 0).
    if (material->EMPDDeepDepth <= 0.0) {
        RH_deep_layer = RH_deep_layer_old;
    } else {
        RH_deep_layer = RH_deep_layer_old +
                        state.dataGlobal->TimeStepZone * 3600.0 * mass_flux_deep_layer / (material->Density * material->EMPDDeepDepth * dU_dRH);
    }
    // Convert calculated RH back to vapor density of surface and deep layers.
    rv_surf_layer = PsyRhovFnTdbRh(state, Taver, RH_surf_layer);
    rv_deep_layer = PsyRhovFnTdbRh(state, Taver, RH_deep_layer);

    // Calculate surface-layer and deep-layer vapor pressures [Pa]
    PV_surf_layer = RH_surf_layer * std::exp(23.7093 - 4111.0 / (Taver + 237.7));
    PV_deep_layer = RH_deep_layer * std::exp(23.7093 - 4111.0 / (Taver + 237.7));

    // Calculate vapor density at physical material surface (surface-layer/air interface). This is used to calculate total moisture flow terms for
    // each zone in HeatBalanceSurfaceManager
    rv_surface = rv_surf_layer - mass_flux_zone * RSurfaceLayer;

    // Calculate heat flux from latent-sensible conversion due to moisture adsorption [W/m^2]
    heat_flux_latent = mass_flux_zone * Lam;

    // Put results in the reporting variables
    // Will add RH and W of deep layer as outputs
    // Need to also add moisture content (kg/kg) of surface and deep layers, and moisture flow from each surface (kg/s), per Rongpeng's suggestion
    EMPDReportVarsData &rvd = state.dataMoistureBalEMPD->EMPDReportVars(SurfNum);
    rvd.rv_surface = rv_surface;
    rvd.RH_surface_layer = RH_surf_layer * 100.0;
    rvd.RH_deep_layer = RH_deep_layer * 100.0;
    rvd.w_surface_layer = 0.622 * PV_surf_layer / (state.dataEnvrn->OutBaroPress - PV_surf_layer);
    rvd.w_deep_layer = 0.622 * PV_deep_layer / (state.dataEnvrn->OutBaroPress - PV_deep_layer);
    rvd.mass_flux_zone = mass_flux_zone;
    rvd.mass_flux_deep = mass_flux_deep_layer;
    rvd.u_surface_layer =
        material->MoistACoeff * pow(RH_surf_layer, material->MoistBCoeff) + material->MoistCCoeff * pow(RH_surf_layer, material->MoistDCoeff);
    rvd.u_deep_layer =
        material->MoistACoeff * pow(RH_deep_layer, material->MoistBCoeff) + material->MoistCCoeff * pow(RH_deep_layer, material->MoistDCoeff);
}

void UpdateMoistureBalanceEMPD(EnergyPlusData &state, int const SurfNum) // Surface number
{

    // SUBROUTINE INFORMATION:
    //   Authors:        Muthusamy Swami and Lixing Gu
    //   Date writtenn:  August, 1999
    //   Modified:       na
    //   Re-engineered:  na

    // PURPOSE OF THIS SUBROUTINE:
    // Update inside surface vapor density

    state.dataMstBalEMPD->RVSurfaceOld(SurfNum) = state.dataMstBalEMPD->RVSurface(SurfNum);
    state.dataMstBalEMPD->RVdeepOld(SurfNum) = state.dataMstBalEMPD->RVDeepLayer(SurfNum);
    state.dataMstBalEMPD->RVSurfLayerOld(SurfNum) = state.dataMstBalEMPD->RVSurfLayer(SurfNum);
}

void ReportMoistureBalanceEMPD(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Lixing Gu
    //       DATE WRITTEN   August 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This routine gives a detailed report to the user about
    // EMPD Properties of each construction.

    // Using/Aliasing
    using General::ScanForReports;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool DoReport;

    int ConstrNum;
    int MatNum;

    ScanForReports(state, "Constructions", DoReport, "Constructions");

    if (!DoReport) return;
    //   Write Descriptions
    print(state.files.eio,
          "{}",
          "! <Construction EMPD>, Construction Name, Inside Layer Material Name, Vapor Resistance Factor, a, b, "
          "c, d, Surface Penetration Depth {m}, Deep Penetration Depth {m}, Coating Vapor Resistance Factor, "
          "Coating Thickness {m}\n");

    for (ConstrNum = 1; ConstrNum <= state.dataHeatBal->TotConstructs; ++ConstrNum) {
        if (state.dataConstruction->Construct(ConstrNum).TypeIsWindow) continue;
        MatNum = state.dataConstruction->Construct(ConstrNum).LayerPoint(state.dataConstruction->Construct(ConstrNum).TotLayers);
        auto const *thisMaterial = dynamic_cast<const Material::MaterialChild *>(state.dataMaterial->Material(MatNum));
        assert(thisMaterial != nullptr);
        if (thisMaterial->EMPDMaterialProps) {
            static constexpr std::string_view Format_700(
                " Construction EMPD, {}, {:8.4F}, {:8.4F}, {:8.4F}, {:8.4F}, {:8.4F}, {:8.4F}, {:8.4F}, {:8.4F}, {:8.4F}\n");
            print(state.files.eio,
                  Format_700,
                  state.dataConstruction->Construct(ConstrNum).Name,
                  thisMaterial->Name,
                  thisMaterial->EMPDmu,
                  thisMaterial->MoistACoeff,
                  thisMaterial->MoistBCoeff,
                  thisMaterial->MoistCCoeff,
                  thisMaterial->MoistDCoeff,
                  thisMaterial->EMPDSurfaceDepth,
                  thisMaterial->EMPDDeepDepth,
                  thisMaterial->EMPDmuCoating,
                  thisMaterial->EMPDCoatingThickness);
        }
    }
}

} // namespace EnergyPlus::MoistureBalanceEMPDManager
