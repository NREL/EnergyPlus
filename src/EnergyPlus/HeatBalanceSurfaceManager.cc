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

// C++ Headers
#include <algorithm>
#include <cassert>
#include <cmath>
// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <AirflowNetwork/Elements.hpp>
#include <EnergyPlus/ChilledCeilingPanelSimple.hh>
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/ConvectionCoefficients.hh>
#include <EnergyPlus/DElightManagerF.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataDElight.hh>
#include <EnergyPlus/DataDaylighting.hh>
#include <EnergyPlus/DataDaylightingDevices.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataMoistureBalance.hh>
#include <EnergyPlus/DataMoistureBalanceEMPD.hh>
#include <EnergyPlus/DataRoomAirModel.hh>
#include <EnergyPlus/DataRuntimeLanguage.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataSystemVariables.hh>
#include <EnergyPlus/DataViewFactorInformation.hh>
#include <EnergyPlus/DataWindowEquivalentLayer.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DaylightingDevices.hh>
#include <EnergyPlus/DaylightingManager.hh>
#include <EnergyPlus/DisplayRoutines.hh>
#include <EnergyPlus/EcoRoofManager.hh>
#include <EnergyPlus/ElectricBaseboardRadiator.hh>
#include <EnergyPlus/FileSystem.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/HWBaseboardRadiator.hh>
#include <EnergyPlus/HeatBalFiniteDiffManager.hh>
#include <EnergyPlus/HeatBalanceAirManager.hh>
#include <EnergyPlus/HeatBalanceHAMTManager.hh>
#include <EnergyPlus/HeatBalanceIntRadExchange.hh>
#include <EnergyPlus/HeatBalanceKivaManager.hh>
#include <EnergyPlus/HeatBalanceSurfaceManager.hh>
#include <EnergyPlus/HighTempRadiantSystem.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/InternalHeatGains.hh>
#include <EnergyPlus/LowTempRadiantSystem.hh>
#include <EnergyPlus/MoistureBalanceEMPDManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/OutputReportTabular.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SolarShading.hh>
#include <EnergyPlus/SteamBaseboardRadiator.hh>
#include <EnergyPlus/SurfaceGeometry.hh>
#include <EnergyPlus/SwimmingPool.hh>
#include <EnergyPlus/ThermalComfort.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WindowComplexManager.hh>
#include <EnergyPlus/WindowEquivalentLayer.hh>
#include <EnergyPlus/WindowManager.hh>
#include <EnergyPlus/WindowManagerExteriorData.hh>
#include <WCECommon.hpp>
#include <WCEMultiLayerOptics.hpp>
#include <WCESingleLayerOptics.hpp>

namespace EnergyPlus::HeatBalanceSurfaceManager {

// Module containing the routines dealing with the Heat Balance of the surfaces

// MODULE INFORMATION:
//       AUTHOR
//       DATE WRITTEN
//       MODIFIED       DJS (PSU Dec 2006) to add ecoroof
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// To encapsulate the data and algorithms required to
// manage the simluation of the surface heat balance for the building.

// METHODOLOGY EMPLOYED:
// na

// REFERENCES:
// The heat balance method is outlined in the "TARP Reference Manual", NIST, NBSIR 83-2655, Feb 1983.
// The methods are also summarized in many BSO Theses and papers.

// OTHER NOTES:
// This module was created from IBLAST subroutines

// USE STATEMENTS:
// Use statements for data only modules
// Using/Aliasing
using namespace DataEnvironment;
using namespace DataHeatBalFanSys;
using namespace DataHeatBalance;
using namespace DataHeatBalSurface;
using namespace DataSurfaces;

// Use statements for access to subroutines in other modules
using namespace ScheduleManager;
using namespace SolarShading;
using namespace DaylightingManager;
using namespace WindowManager;
using namespace FenestrationCommon;
using namespace SingleLayerOptics;
using namespace MultiLayerOptics;

// Data
// MODULE PARAMETER DEFINITIONS:
static std::string const BlankString;

namespace {

} // namespace

// These are now external subroutines
// PUBLIC  CalcHeatBalanceOutsideSurf  ! The heat balance routines are now public because the
// PUBLIC  CalcHeatBalanceInsideSurf   ! radiant systems need access to them in order to simulate

void ManageSurfaceHeatBalance(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Liesen
    //       DATE WRITTEN   January 1998
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine manages the heat surface balance method of calculating
    // building thermal loads.  It is called from the HeatBalanceManager
    // at the time step level.  This driver manages the calls to all of
    // the other drivers and simulation algorithms.

    using HeatBalanceAirManager::ManageAirHeatBalance;
    using OutputReportTabular::GatherComponentLoadsSurface; // for writing tabular component loads output reports
    using ThermalComfort::ManageThermalComfort;

    auto &Surface(state.dataSurface->Surface);

    if (state.dataHeatBalSurfMgr->ManageSurfaceHeatBalancefirstTime) DisplayString(state, "Initializing Surfaces");
    InitSurfaceHeatBalance(state); // Initialize all heat balance related parameters

    // Solve the zone heat balance 'Detailed' solution
    // Call the outside and inside surface heat balances
    if (state.dataHeatBalSurfMgr->ManageSurfaceHeatBalancefirstTime) DisplayString(state, "Calculate Outside Surface Heat Balance");
    CalcHeatBalanceOutsideSurf(state);
    if (state.dataHeatBalSurfMgr->ManageSurfaceHeatBalancefirstTime) DisplayString(state, "Calculate Inside Surface Heat Balance");
    CalcHeatBalanceInsideSurf(state);

    // The air heat balance must be called before the temperature history
    // updates because there may be a radiant system in the building
    if (state.dataHeatBalSurfMgr->ManageSurfaceHeatBalancefirstTime) DisplayString(state, "Calculate Air Heat Balance");
    ManageAirHeatBalance(state);

    // IF NECESSARY, do one final "average" heat balance pass.  This is only
    // necessary if a radiant system is present and it was actually on for
    // part or all of the time step.
    UpdateFinalSurfaceHeatBalance(state);

    // Before we leave the Surface Manager the thermal histories need to be updated
    if (state.dataHeatBal->AnyCTF || state.dataHeatBal->AnyEMPD) {
        UpdateThermalHistories(state); // Update the thermal histories
    }

    if (state.dataHeatBal->AnyCondFD) {
        for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
            int const ConstrNum = Surface(SurfNum).Construction;
            if (ConstrNum <= 0) continue;                                            // Shading surface, not really a heat transfer surface
            if (state.dataConstruction->Construct(ConstrNum).TypeIsWindow) continue; //  Windows simulated in Window module
            if (Surface(SurfNum).HeatTransferAlgorithm != DataSurfaces::iHeatTransferModel::CondFD) continue;
            state.dataHeatBalFiniteDiffMgr->SurfaceFD(SurfNum).UpdateMoistureBalance();
        }
    }

    ManageThermalComfort(state, false); // "Record keeping" for the zone

    ReportSurfaceHeatBalance(state);
    if (state.dataGlobal->ZoneSizingCalc) GatherComponentLoadsSurface(state);

    CalcThermalResilience(state);

    if (state.dataOutRptTab->displayThermalResilienceSummary) {
        ReportThermalResilience(state);
    }

    if (state.dataOutRptTab->displayCO2ResilienceSummary) {
        ReportCO2Resilience(state);
    }

    if (state.dataOutRptTab->displayVisualResilienceSummary) {
        ReportVisualResilience(state);
    }

    state.dataHeatBalSurfMgr->ManageSurfaceHeatBalancefirstTime = false;
}

// Beginning Initialization Section of the Module
//******************************************************************************

void InitSurfaceHeatBalance(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard J. Liesen
    //       DATE WRITTEN   January 1998
    //       MODIFIED       Nov. 1999, FCW,
    //                      Move ComputeIntThermalAbsorpFactors
    //                      so called every timestep
    //                      Jan 2004, RJH
    //                      Added calls to alternative daylighting analysis using DElight
    //                      All modifications demarked with RJH (Rob Hitchcock)
    //                      RJH, Jul 2004: add error handling for DElight calls
    //       MODIFIED       Aug. 2017
    //                      Add initializations of surface data to linked air node value if defined
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for surface initializations within the
    // heat balance.

    // METHODOLOGY EMPLOYED:
    // Uses the status flags to trigger record keeping events.

    // Using/Aliasing
    using DataDElight::LUX2FC;
    using namespace SolarShading;
    using ConvectionCoefficients::InitInteriorConvectionCoeffs;
    using HeatBalanceIntRadExchange::CalcInteriorRadExchange;
    using HeatBalFiniteDiffManager::InitHeatBalFiniteDiff;
    using InternalHeatGains::ManageInternalHeatGains;
    using namespace DElightManagerF;

    auto &Surface(state.dataSurface->Surface);

    assert(equal_dimensions(state.dataHeatBalSurf->TH, state.dataHeatBalSurf->QH));

    if (state.dataHeatBalSurfMgr->InitSurfaceHeatBalancefirstTime) DisplayString(state, "Initializing Outdoor environment for Surfaces");

    // set zone level wind dir to global value
    // Initialize zone outdoor environmental variables
    // Bulk Initialization for Temperatures & WindSpeed
    // using the zone, modify the zone  Dry/Wet BulbTemps

    //  DO ZoneNum = 1, NumOfZones
    //    Zone(ZoneNum)%WindSpeed = WindSpeedAt(Zone(ZoneNum)%Centroid%z)
    //  END DO

    // Initialize surface outdoor environmental variables
    // Bulk Initialization for Temperatures & WindSpeed
    // using the surface centroids, modify the surface Dry/Wet BulbTemps
    SetSurfaceOutBulbTempAt(state);
    CheckSurfaceOutBulbTempAt(state);

    SetSurfaceWindSpeedAt(state);
    SetSurfaceWindDirAt(state);
    //  DO SurfNum = 1, TotSurfaces
    //    IF (Surface(SurfNum)%ExtWind) Surface(SurfNum)%WindSpeed = WindSpeedAt(Surface(SurfNum)%Centroid%z)
    //  END DO
    if (state.dataGlobal->AnyLocalEnvironmentsInModel) {
        for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
            if (state.dataSurface->SurfHasLinkedOutAirNode(SurfNum)) {
                int const linkedNode = state.dataSurface->SurfLinkedOutAirNode(SurfNum);
                state.dataSurface->SurfOutDryBulbTemp(SurfNum) = state.dataLoopNodes->Node(linkedNode).OutAirDryBulb;
                state.dataSurface->SurfOutWetBulbTemp(SurfNum) = state.dataLoopNodes->Node(linkedNode).OutAirWetBulb;
                state.dataSurface->SurfOutWindSpeed(SurfNum) = state.dataLoopNodes->Node(linkedNode).OutAirWindSpeed;
                state.dataSurface->SurfOutWindDir(SurfNum) = state.dataLoopNodes->Node(linkedNode).OutAirWindDir;
            }

            if (state.dataHeatBalSurfMgr->InitSurfaceHeatBalancefirstTime && state.dataSurface->SurfHasSurroundingSurfProperties(SurfNum)) {
                Real64 SrdSurfsNum = state.dataSurface->SurfSurroundingSurfacesNum(SurfNum);
                Real64 SrdSurfsViewFactor = 0;
                if (state.dataSurface->SurroundingSurfsProperty(SrdSurfsNum).SkyViewFactor >= 0) {
                    SrdSurfsViewFactor += state.dataSurface->SurroundingSurfsProperty(SrdSurfsNum).SkyViewFactor;
                }
                if (state.dataSurface->SurroundingSurfsProperty(SrdSurfsNum).GroundViewFactor >= 0) {
                    SrdSurfsViewFactor += state.dataSurface->SurroundingSurfsProperty(SrdSurfsNum).GroundViewFactor;
                }
                for (int SrdSurfNum = 1; SrdSurfNum <= state.dataSurface->SurroundingSurfsProperty(SrdSurfsNum).TotSurroundingSurface; SrdSurfNum++) {
                    SrdSurfsViewFactor += state.dataSurface->SurroundingSurfsProperty(SrdSurfsNum).SurroundingSurfs(SrdSurfNum).ViewFactor;
                }
                // Check if the sum of all defined view factors > 1.0
                if (SrdSurfsViewFactor > 1.0) {
                    ShowSevereError(state, "Illegal surrounding surfaces view factors for " + Surface(SurfNum).Name + ".");
                    ShowContinueError(state, " The sum of sky, ground, and all surrounding surfaces view factors should be less than 1.0.");
                }
                if (state.dataSurface->SurroundingSurfsProperty(SrdSurfsNum).SkyViewFactor >= 0 &&
                    state.dataSurface->SurroundingSurfsProperty(SrdSurfsNum).GroundViewFactor >= 0) {
                    // If both surface sky and ground view factor defined, overwrite with the defined value
                    Surface(SurfNum).ViewFactorSkyIR = state.dataSurface->SurroundingSurfsProperty(SrdSurfsNum).SkyViewFactor;
                    Surface(SurfNum).ViewFactorGroundIR = state.dataSurface->SurroundingSurfsProperty(SrdSurfsNum).GroundViewFactor;
                } else if (state.dataSurface->SurroundingSurfsProperty(SrdSurfsNum).SkyViewFactor >= 0 &&
                           state.dataSurface->SurroundingSurfsProperty(SrdSurfsNum).GroundViewFactor < 0) {
                    // If only sky view factor defined, ground view factor = 1 - all other defined view factors.
                    Surface(SurfNum).ViewFactorSkyIR = state.dataSurface->SurroundingSurfsProperty(SrdSurfsNum).SkyViewFactor;
                    Surface(SurfNum).ViewFactorGroundIR = 1 - SrdSurfsViewFactor;
                } else if (state.dataSurface->SurroundingSurfsProperty(SrdSurfsNum).SkyViewFactor < 0 &&
                           state.dataSurface->SurroundingSurfsProperty(SrdSurfsNum).GroundViewFactor >= 0) {
                    // If only ground view factor defined, sky view factor = 1 - all other defined view factors.
                    Surface(SurfNum).ViewFactorGroundIR = state.dataSurface->SurroundingSurfsProperty(SrdSurfsNum).GroundViewFactor;
                    Surface(SurfNum).ViewFactorSkyIR = 1 - SrdSurfsViewFactor;
                } else {
                    // If neither ground or sky view factor define, continue to use the original proportion.
                    Surface(SurfNum).ViewFactorSkyIR *= 1 - SrdSurfsViewFactor;
                    Surface(SurfNum).ViewFactorGroundIR *= 1 - SrdSurfsViewFactor;
                }
            }
        }
    }
    // Overwriting surface and zone level environmental data with EMS override value
    if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
        for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
            if (state.dataSurface->SurfOutDryBulbTempEMSOverrideOn(SurfNum)) {
                state.dataSurface->SurfOutDryBulbTemp(SurfNum) = state.dataSurface->SurfOutDryBulbTempEMSOverrideValue(SurfNum);
            }
            if (state.dataSurface->SurfOutWetBulbTempEMSOverrideOn(SurfNum)) {
                state.dataSurface->SurfOutWetBulbTemp(SurfNum) = state.dataSurface->SurfOutWetBulbTempEMSOverrideValue(SurfNum);
            }
            if (state.dataSurface->SurfWindSpeedEMSOverrideOn(SurfNum)) {
                state.dataSurface->SurfOutWindSpeed(SurfNum) = state.dataSurface->SurfWindSpeedEMSOverrideValue(SurfNum);
            }
            if (state.dataSurface->SurfWindDirEMSOverrideOn(SurfNum)) {
                state.dataSurface->SurfOutWindDir(SurfNum) = state.dataSurface->SurfWindDirEMSOverrideValue(SurfNum);
            }
            if (state.dataSurface->SurfViewFactorGroundEMSOverrideOn(SurfNum)) {
                Surface(SurfNum).ViewFactorGround = state.dataSurface->SurfViewFactorGroundEMSOverrideValue(SurfNum);
            }
        }
    }

    // Do the Begin Simulation initializations
    if (state.dataGlobal->BeginSimFlag) {
        AllocateSurfaceHeatBalArrays(state); // Allocate the Module Arrays before any inits take place
        state.dataHeatBalSurf->InterZoneWindow = std::any_of(
            state.dataHeatBal->Zone.begin(), state.dataHeatBal->Zone.end(), [](DataHeatBalance::ZoneData const &e) { return e.HasInterZoneWindow; });
        state.dataRoomAirMod->IsZoneDV.dimension(state.dataGlobal->NumOfZones, false);
        state.dataRoomAirMod->IsZoneCV.dimension(state.dataGlobal->NumOfZones, false);
        state.dataRoomAirMod->IsZoneUI.dimension(state.dataGlobal->NumOfZones, false);
    }
    if (state.dataGlobal->BeginSimFlag || state.dataGlobal->AnySurfPropOverridesInModel) {
        for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
            int const firstSurf = state.dataHeatBal->Zone(zoneNum).HTSurfaceFirst;
            int const lastSurf = state.dataHeatBal->Zone(zoneNum).HTSurfaceLast;
            for (int SurfNum = firstSurf; SurfNum <= lastSurf; ++SurfNum) {
                int ConstrNum = state.dataSurface->SurfActiveConstruction(SurfNum); // SurfActiveConstruction set above
                state.dataHeatBalSurf->SurfAbsSolarInt(SurfNum) = state.dataConstruction->Construct(ConstrNum).InsideAbsorpSolar;
                state.dataHeatBalSurf->SurfAbsThermalInt(SurfNum) = state.dataConstruction->Construct(ConstrNum).InsideAbsorpThermal;
                state.dataHeatBalSurf->SurfRoughnessExt(SurfNum) = state.dataConstruction->Construct(ConstrNum).OutsideRoughness;
                state.dataHeatBalSurf->SurfAbsSolarExt(SurfNum) = state.dataConstruction->Construct(ConstrNum).OutsideAbsorpSolar;
                state.dataHeatBalSurf->SurfAbsThermalExt(SurfNum) = state.dataConstruction->Construct(ConstrNum).OutsideAbsorpThermal;
            }
        }
    }

    // Do the Begin Environment initializations
    if (state.dataGlobal->BeginEnvrnFlag) {
        if (state.dataHeatBalSurfMgr->InitSurfaceHeatBalancefirstTime) DisplayString(state, "Initializing Temperature and Flux Histories");
        InitThermalAndFluxHistories(state); // Set initial temperature and flux histories
    }

    // Calc movable insulation properties
    if (state.dataSurface->AnyMovableInsulation) {
        EvalOutsideMovableInsulation(state);
        EvalInsideMovableInsulation(state);
    }

    // There are no daily initializations done in this portion of the surface heat balance

    // There are no hourly initializations done in this portion of the surface heat balance

    // Need to be called each timestep in order to check if surface points to new construction (EMS) and if does then
    // complex fenestration needs to be initialized for additional states
    TimestepInitComplexFenestration(state);

    // Calculate exterior-surface multipliers that account for anisotropy of
    // sky radiance
    if (state.dataEnvrn->SunIsUp && state.dataEnvrn->DifSolarRad > 0.0) {
        AnisoSkyViewFactors(state);
    } else {
        state.dataHeatBal->SurfAnisoSkyMult = 0.0;
    }

    // Set shading flag for exterior windows (except flags related to daylighting) and
    // window construction (unshaded or shaded) to be used in heat balance calculation
    if (state.dataHeatBalSurfMgr->InitSurfaceHeatBalancefirstTime) DisplayString(state, "Initializing Window Shading");

    WindowShadingManager(state);

    // Calculate factors that are used to determine how much long-wave radiation from internal
    // gains is absorbed by interior surfaces
    if (state.dataHeatBalSurfMgr->InitSurfaceHeatBalancefirstTime) DisplayString(state, "Computing Interior Absorption Factors");
    if (state.dataHeatBalSurfMgr->InitSurfaceHeatBalancefirstTime) HeatBalanceIntRadExchange::InitInteriorRadExchange(state);
    ComputeIntThermalAbsorpFactors(state);

    // Calculate factors for diffuse solar absorbed by room surfaces and interior shades
    if (state.dataHeatBalSurfMgr->InitSurfaceHeatBalancefirstTime) DisplayString(state, "Computing Interior Diffuse Solar Absorption Factors");
    ComputeIntSWAbsorpFactors(state);

    if (state.dataHeatBalSurf->InterZoneWindow) {
        if (state.dataHeatBalSurfMgr->InitSurfaceHeatBalancefirstTime) {
            DisplayString(state, "Computing Interior Diffuse Solar Exchange through Interzone Windows");
        }
        ComputeDifSolExcZonesWIZWindows(state, state.dataGlobal->NumOfZones);
    }

    // For daylit zones, calculate interior daylight illuminance at reference points and
    // simulate lighting control system to get overhead electric lighting reduction
    // factor due to daylighting.
    for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
        int const firstSurfWin = state.dataHeatBal->Zone(zoneNum).WindowSurfaceFirst;
        int const lastSurfWin = state.dataHeatBal->Zone(zoneNum).WindowSurfaceLast;
        for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
            if (Surface(SurfNum).ExtSolar) {
                state.dataSurface->SurfaceWindow(SurfNum).IllumFromWinAtRefPtRep = 0.0;
                state.dataSurface->SurfaceWindow(SurfNum).LumWinFromRefPtRep = 0.0;
            }
        }
    }

    for (int NZ = 1; NZ <= state.dataGlobal->NumOfZones; ++NZ) {
        if (state.dataDaylightingData->ZoneDaylight(NZ).DaylightMethod == DataDaylighting::iDaylightingMethod::NoDaylighting) continue;
        state.dataDaylightingData->ZoneDaylight(NZ).DaylIllumAtRefPt = 0.0;
        state.dataDaylightingData->ZoneDaylight(NZ).GlareIndexAtRefPt = 0.0;
        state.dataDaylightingData->ZoneDaylight(NZ).ZonePowerReductionFactor = 1.0;
        state.dataDaylightingData->ZoneDaylight(NZ).InterReflIllFrIntWins = 0.0; // inter-reflected illuminance from interior windows
        if (state.dataDaylightingData->ZoneDaylight(NZ).TotalDaylRefPoints != 0) {
            state.dataDaylightingData->ZoneDaylight(NZ).TimeExceedingGlareIndexSPAtRefPt = 0.0;
            state.dataDaylightingData->ZoneDaylight(NZ).TimeExceedingDaylightIlluminanceSPAtRefPt = 0.0;
        }

        if (state.dataEnvrn->SunIsUp && state.dataDaylightingData->ZoneDaylight(NZ).TotalDaylRefPoints != 0) {
            if (state.dataHeatBalSurfMgr->InitSurfaceHeatBalancefirstTime) DisplayString(state, "Computing Interior Daylighting Illumination");
            DayltgInteriorIllum(state, NZ);
            if (!state.dataGlobal->DoingSizing) DayltgInteriorMapIllum(state, NZ);
        }

        if (state.dataEnvrn->SunIsUp && state.dataDaylightingDevicesData->NumOfTDDPipes > 0 && NZ == 1) {
            if (state.dataHeatBalSurfMgr->InitSurfaceHeatBalancefirstTime)
                DisplayString(state, "Computing Interior Daylighting Illumination for TDD pipes");
            DayltgInteriorTDDIllum(state);
        }

        // RJH DElight Modification Begin - Call to DElight electric lighting control subroutine
        // Check if the sun is up and the current Thermal Zone hosts a Daylighting:DElight object
        if (state.dataEnvrn->SunIsUp && state.dataDaylightingData->ZoneDaylight(NZ).TotalDaylRefPoints != 0 &&
            (state.dataDaylightingData->ZoneDaylight(NZ).DaylightMethod == DataDaylighting::iDaylightingMethod::DElightDaylighting)) {
            // Call DElight interior illuminance and electric lighting control subroutine
            Real64 dPowerReducFac = 1.0; // Return value Electric Lighting Power Reduction Factor for current Zone and Timestep
            Real64 dHISKFFC = state.dataEnvrn->HISKF * LUX2FC;
            Real64 dHISUNFFC = state.dataEnvrn->HISUNF * LUX2FC;
            Real64 dSOLCOS1 = state.dataEnvrn->SOLCOS(1);
            Real64 dSOLCOS2 = state.dataEnvrn->SOLCOS(2);
            Real64 dSOLCOS3 = state.dataEnvrn->SOLCOS(3);
            Real64 dLatitude = state.dataEnvrn->Latitude;
            Real64 dCloudFraction = state.dataEnvrn->CloudFraction;
            // Init Error Flag to 0 (no Warnings or Errors) (returned from DElight)
            int iErrorFlag = 0;
            bool elOpened;

            int iReadStatus;       // Error File Read Status
            std::string cErrorMsg; // Each DElight Error Message can be up to 200 characters long
            bool bEndofErrFile;    // End of Error File flag

            DElightElecLtgCtrl(len(state.dataHeatBal->Zone(NZ).Name),
                               state.dataHeatBal->Zone(NZ).Name,
                               dLatitude,
                               dHISKFFC,
                               dHISUNFFC,
                               dCloudFraction,
                               dSOLCOS1,
                               dSOLCOS2,
                               dSOLCOS3,
                               dPowerReducFac,
                               iErrorFlag);
            // Check Error Flag for Warnings or Errors returning from DElight
            // RJH 2008-03-07: If no warnings/errors then read refpt illuminances for standard output reporting
            if (iErrorFlag != 0) {
                // Open DElight Electric Lighting Error File for reading
                auto iDElightErrorFile = state.files.outputDelightDfdmpFilePath.try_open(state.files.outputControl.delightdfdmp);
                if (iDElightErrorFile.good()) {
                    elOpened = true;
                } else {
                    elOpened = false;
                }
                //            IF (iwriteStatus /= 0) THEN
                //              CALL ShowFatalError(state, 'InitSurfaceHeatBalance: Could not open file "eplusout.delighteldmp" for output
                //              (readwrite).')
                //            ENDIF
                //            Open(unit=iDElightErrorFile, file='eplusout.delighteldmp', action='READ')

                // Sequentially read lines in DElight Electric Lighting Error File
                // and process them using standard EPlus warning/error handling calls
                bEndofErrFile = false;
                iReadStatus = 0;
                while (!bEndofErrFile && elOpened) {
                    auto cErrorLine = iDElightErrorFile.readLine();
                    if (cErrorLine.eof) {
                        bEndofErrFile = true;
                        continue;
                    }

                    // Is the current line a Warning message?
                    if (has_prefix(cErrorLine.data, "WARNING: ")) {
                        cErrorMsg = cErrorLine.data.substr(9);
                        ShowWarningError(state, cErrorMsg);
                    }
                    // Is the current line an Error message?
                    if (has_prefix(cErrorLine.data, "ERROR: ")) {
                        cErrorMsg = cErrorLine.data.substr(7);
                        ShowSevereError(state, cErrorMsg);
                        iErrorFlag = 1;
                    }
                }

                // Close DElight Error File and delete

                if (elOpened) {
                    iDElightErrorFile.close();
                    FileSystem::removeFile(iDElightErrorFile.filePath);
                }
                // If any DElight Error occurred then ShowFatalError to terminate
                if (iErrorFlag > 0) {
                    ShowFatalError(state, "End of DElight Error Messages");
                }
            } else { // RJH 2008-03-07: No errors
                // extract reference point illuminance values from DElight Electric Lighting dump file for reporting
                // Open DElight Electric Lighting Dump File for reading
                auto iDElightErrorFile = state.files.outputDelightEldmpFilePath.try_open(state.files.outputControl.delighteldmp);
                if (iDElightErrorFile.is_open()) {
                    elOpened = true;
                } else {
                    elOpened = false;
                }

                // Sequentially read lines in DElight Electric Lighting Dump File
                // and extract refpt illuminances for standard EPlus output handling
                bEndofErrFile = false;
                int iDElightRefPt = 0; // Reference Point number for reading DElight Dump File (eplusout.delighteldmp)
                iReadStatus = 0;
                while (!bEndofErrFile && elOpened) {
                    auto line = iDElightErrorFile.read<Real64>();
                    Real64 dRefPtIllum = line.data; // tmp var for reading RefPt illuminance
                    if (line.eof) {
                        bEndofErrFile = true;
                        continue;
                    }
                    // Increment refpt counter
                    ++iDElightRefPt;
                    // Assure refpt index does not exceed number of refpts in this zone
                    if (iDElightRefPt <= state.dataDaylightingData->ZoneDaylight(NZ).TotalDaylRefPoints) {
                        state.dataDaylightingData->ZoneDaylight(NZ).DaylIllumAtRefPt(iDElightRefPt) = dRefPtIllum;
                    }
                }

                // Close DElight Electric Lighting Dump File and delete
                if (elOpened) {
                    iDElightErrorFile.close();
                    FileSystem::removeFile(iDElightErrorFile.filePath);
                };
            }
            // Store the calculated total zone Power Reduction Factor due to DElight daylighting
            // in the ZoneDaylight structure for later use
            state.dataDaylightingData->ZoneDaylight(NZ).ZonePowerReductionFactor = dPowerReducFac;
        }
        // RJH DElight Modification End - Call to DElight electric lighting control subroutine
    }

    for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
        int const firstSurfWin = state.dataHeatBal->Zone(zoneNum).WindowSurfaceFirst;
        int const lastSurfWin = state.dataHeatBal->Zone(zoneNum).WindowSurfaceLast;
        for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
            state.dataSurface->SurfWinFracTimeShadingDeviceOn(SurfNum) = 0.0;
            if (IS_SHADED(state.dataSurface->SurfWinShadingFlag(SurfNum))) {
                state.dataSurface->SurfWinFracTimeShadingDeviceOn(SurfNum) = 1.0;
            } else {
                state.dataSurface->SurfWinFracTimeShadingDeviceOn(SurfNum) = 0.0;
            }
        }
    }

    CalcInteriorRadExchange(state, state.dataHeatBalSurf->TH(2, 1, _), 0, state.dataHeatBalSurf->SurfNetLWRadToSurf, _, "Main");

    if (state.dataSurface->AirflowWindows) WindowGapAirflowControl(state);

    // The order of these initializations is important currently.  Over time we hope to
    //  take the appropriate parts of these inits to the other heat balance managers
    if (state.dataHeatBalSurfMgr->InitSurfaceHeatBalancefirstTime) DisplayString(state, "Initializing Solar Heat Gains");

    InitSolarHeatGains(state);
    if (state.dataEnvrn->SunIsUp && (state.dataEnvrn->BeamSolarRad + state.dataEnvrn->GndSolarRad + state.dataEnvrn->DifSolarRad > 0.0)) {
        for (int NZ = 1; NZ <= state.dataGlobal->NumOfZones; ++NZ) {
            if (state.dataDaylightingData->ZoneDaylight(NZ).TotalDaylRefPoints > 0) {
                if (state.dataHeatBal->Zone(NZ).HasInterZoneWindow) {
                    DayltgInterReflIllFrIntWins(state, NZ);
                    DayltgGlareWithIntWins(state, state.dataDaylightingData->ZoneDaylight(NZ).GlareIndexAtRefPt, NZ);
                }
                DayltgElecLightingControl(state, NZ);
            }
        }
    } else if (state.dataDaylightingData->mapResultsToReport && state.dataGlobal->TimeStep == state.dataGlobal->NumOfTimeStepInHour) {
        for (int MapNum = 1; MapNum <= state.dataDaylightingData->TotIllumMaps; ++MapNum) {
            ReportIllumMap(state, MapNum);
        }
        state.dataDaylightingData->mapResultsToReport = false;
    }

    if (state.dataHeatBalSurfMgr->InitSurfaceHeatBalancefirstTime) DisplayString(state, "Initializing Internal Heat Gains");
    ManageInternalHeatGains(state, false);
    if (state.dataHeatBalSurfMgr->InitSurfaceHeatBalancefirstTime) DisplayString(state, "Initializing Interior Solar Distribution");
    InitIntSolarDistribution(state);

    if (state.dataHeatBalSurfMgr->InitSurfaceHeatBalancefirstTime) DisplayString(state, "Initializing Interior Convection Coefficients");
    InitInteriorConvectionCoeffs(state, state.dataHeatBalSurf->TempSurfInTmp);

    if (state.dataGlobal->BeginSimFlag) { // Now's the time to report surfaces, if desired
        //    if (firstTime) CALL DisplayString('Reporting Surfaces')
        //    CALL ReportSurfaces
        if (state.dataHeatBalSurfMgr->InitSurfaceHeatBalancefirstTime) DisplayString(state, "Gathering Information for Predefined Reporting");
        GatherForPredefinedReport(state);
    }

    // Initialize the temperature history terms for conduction through the surfaces
    if (state.dataHeatBal->AnyCondFD) {
        InitHeatBalFiniteDiff(state);
    }

    for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) { // Loop through all surfaces...
        int const firstSurfOpaque = state.dataHeatBal->Zone(zoneNum).OpaqOrIntMassSurfaceFirst;
        int const lastSurfOpaque = state.dataHeatBal->Zone(zoneNum).OpaqOrIntMassSurfaceLast;
        for (int SurfNum = firstSurfOpaque; SurfNum <= lastSurfOpaque; ++SurfNum) {
            auto const &surface(Surface(SurfNum));
            if (surface.HeatTransferAlgorithm != DataSurfaces::iHeatTransferModel::CTF &&
                surface.HeatTransferAlgorithm != DataSurfaces::iHeatTransferModel::EMPD)
                continue;
            // Outside surface temp of "normal" windows not needed in Window5 calculation approach
            // Window layer temperatures are calculated in CalcHeatBalanceInsideSurf

            int const ConstrNum = surface.Construction;
            auto const &construct(state.dataConstruction->Construct(ConstrNum));
            if (construct.NumCTFTerms > 1) { // COMPUTE CONSTANT PORTION OF CONDUCTIVE FLUXES.

                Real64 QIC = 0.0;
                Real64 QOC = 0.0;
                Real64 TSC;
                Real64 TUC;
                if (construct.SourceSinkPresent) {
                    TSC = 0.0;
                    TUC = 0.0;
                }
                auto l11(state.dataHeatBalSurf->TH.index(1, 2, SurfNum));
                auto l12(state.dataHeatBalSurf->TH.index(2, 2, SurfNum));
                auto const s3(state.dataHeatBalSurf->TH.size3());
                for (int Term = 1; Term <= construct.NumCTFTerms; ++Term, l11 += s3, l12 += s3) {
                    // [ l11 ] == ( 1, Term + 1, SurfNum ), [ l12 ] == ( 1, Term + 1, SurfNum )

                    // Sign convention for the various terms in the following two equations
                    // is based on the form of the Conduction Transfer Function equation
                    // given by:
                    // Qin,now  = (Sum of)(Y Tout) - (Sum of)(Z Tin) + (Sum of)(F Qin,old)
                    // Qout,now = (Sum of)(X Tout) - (Sum of)(Y Tin) + (Sum of)(F Qout,old)
                    // In both equations, flux is positive from outside to inside.

                    // Tuned Aliases and linear indexing
                    Real64 const ctf_cross(construct.CTFCross(Term));
                    Real64 const TH11(state.dataHeatBalSurf->TH[l11]);
                    Real64 const TH12(state.dataHeatBalSurf->TH[l12]);

                    QIC += ctf_cross * TH11 - construct.CTFInside(Term) * TH12 + construct.CTFFlux(Term) * state.dataHeatBalSurf->QH[l12];

                    QOC += construct.CTFOutside(Term) * TH11 - ctf_cross * TH12 + construct.CTFFlux(Term) * state.dataHeatBalSurf->QH[l11];

                    if (construct.SourceSinkPresent) {
                        Real64 const QsrcHist1(state.dataHeatBalSurf->QsrcHist(SurfNum, Term + 1));

                        QIC += construct.CTFSourceIn(Term) * QsrcHist1;

                        QOC += construct.CTFSourceOut(Term) * QsrcHist1;

                        TSC += construct.CTFTSourceOut(Term) * TH11 + construct.CTFTSourceIn(Term) * TH12 + construct.CTFTSourceQ(Term) * QsrcHist1 +
                               construct.CTFFlux(Term) * state.dataHeatBalSurf->TsrcHist(SurfNum, Term + 1);

                        TUC += construct.CTFTUserOut(Term) * TH11 + construct.CTFTUserIn(Term) * TH12 + construct.CTFTUserSource(Term) * QsrcHist1 +
                               construct.CTFFlux(Term) * state.dataHeatBalSurf->TuserHist(SurfNum, Term + 1);
                    }
                }

                state.dataHeatBalSurf->CTFConstOutPart(SurfNum) = QOC;
                state.dataHeatBalSurf->CTFConstInPart(SurfNum) = QIC;
                if (construct.SourceSinkPresent) {
                    state.dataHeatBalFanSys->CTFTsrcConstPart(SurfNum) = TSC;
                    state.dataHeatBalFanSys->CTFTuserConstPart(SurfNum) = TUC;
                }
            } else { // Number of CTF Terms = 1-->Resistance only constructions have no history terms.

                state.dataHeatBalSurf->CTFConstOutPart(SurfNum) = 0.0;
                state.dataHeatBalSurf->CTFConstInPart(SurfNum) = 0.0;
                if (construct.SourceSinkPresent) {
                    state.dataHeatBalFanSys->CTFTsrcConstPart(SurfNum) = 0.0;
                    state.dataHeatBalFanSys->CTFTuserConstPart(SurfNum) = 0.0;
                }
            }
        }

    } // ...end of surfaces DO loop for initializing temperature history terms for the surface heat balances

    // Zero out all of the radiant system heat balance coefficient arrays
    for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) { // Loop through all surfaces...
        int const firstSurf = state.dataHeatBal->Zone(zoneNum).HTSurfaceFirst;
        int const lastSurf = state.dataHeatBal->Zone(zoneNum).HTSurfaceLast;
        for (int SurfNum = firstSurf; SurfNum <= lastSurf; ++SurfNum) {
            state.dataHeatBalFanSys->RadSysTiHBConstCoef(SurfNum) = 0.0;
            state.dataHeatBalFanSys->RadSysTiHBToutCoef(SurfNum) = 0.0;
            state.dataHeatBalFanSys->RadSysTiHBQsrcCoef(SurfNum) = 0.0;
            state.dataHeatBalFanSys->RadSysToHBConstCoef(SurfNum) = 0.0;
            state.dataHeatBalFanSys->RadSysToHBTinCoef(SurfNum) = 0.0;
            state.dataHeatBalFanSys->RadSysToHBQsrcCoef(SurfNum) = 0.0;

            state.dataHeatBalFanSys->QRadSysSource(SurfNum) = 0.0;
            state.dataHeatBalFanSys->QPVSysSource(SurfNum) = 0.0;
            state.dataHeatBalFanSys->QHTRadSysSurf(SurfNum) = 0.0;
            state.dataHeatBalFanSys->QHWBaseboardSurf(SurfNum) = 0.0;
            state.dataHeatBalFanSys->QSteamBaseboardSurf(SurfNum) = 0.0;
            state.dataHeatBalFanSys->QElecBaseboardSurf(SurfNum) = 0.0;
            state.dataHeatBalFanSys->QCoolingPanelSurf(SurfNum) = 0.0;
            state.dataHeatBalFanSys->QPoolSurfNumerator(SurfNum) = 0.0;
            state.dataHeatBalFanSys->PoolHeatTransCoefs(SurfNum) = 0.0;
        } // ...end of Zone Surf loop
    }     // ...end of Zone loop

    if (state.dataGlobal->ZoneSizingCalc) GatherComponentLoadsSurfAbsFact(state);

    if (state.dataHeatBalSurfMgr->InitSurfaceHeatBalancefirstTime) DisplayString(state, "Completed Initializing Surface Heat Balance");
    state.dataHeatBalSurfMgr->InitSurfaceHeatBalancefirstTime = false;
}

void GatherForPredefinedReport(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Jason Glazer
    //       DATE WRITTEN   August 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine reports the information for the predefined reports
    // related to envelope components.

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES:
    // na

    // Using/Aliasing
    using namespace OutputReportPredefined;
    using WindowManager::CalcNominalWindowCond;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:
    // na

    // SUBROUTINE PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS:
    // na

    // DERIVED TYPE DEFINITIONS:
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    std::string surfName;
    int curCons;
    int zonePt;
    Real64 mult;
    Real64 curAzimuth;
    Real64 curTilt;
    Real64 windowArea;
    Real64 frameWidth;
    Real64 frameArea;
    Real64 dividerArea;
    // counts for object count report
    int SurfaceClassCount = int(SurfaceClass::Count);
    Array1D_int numSurfaces(SurfaceClassCount);
    Array1D_int numExtSurfaces(SurfaceClassCount);
    int frameDivNum;
    bool isExterior;
    Array1D<Real64> computedNetArea; // holds the gross wall area minus the window and door areas

    // the following variables are for the CalcNominalWindowCond call but only SHGCSummer is needed
    Real64 nomCond;
    Real64 SHGCSummer;
    Real64 TransSolNorm;
    Real64 TransVisNorm;
    Real64 nomUfact;
    int errFlag;
    int curWSC;
    // following variables are totals for fenestration table
    Real64 windowAreaWMult(0.0);
    Real64 fenTotArea(0.0);
    Real64 fenTotAreaNorth(0.0);
    Real64 fenTotAreaNonNorth(0.0);
    Real64 ufactArea(0.0);
    Real64 ufactAreaNorth(0.0);
    Real64 ufactAreaNonNorth(0.0);
    Real64 shgcArea(0.0);
    Real64 shgcAreaNorth(0.0);
    Real64 shgcAreaNonNorth(0.0);
    Real64 vistranArea(0.0);
    Real64 vistranAreaNorth(0.0);
    Real64 vistranAreaNonNorth(0.0);
    Real64 intFenTotArea(0.0);
    Real64 intUfactArea(0.0);
    Real64 intShgcArea(0.0);
    Real64 intVistranArea(0.0);
    bool isNorth;

    auto &Surface(state.dataSurface->Surface);

    numSurfaces = 0;
    numExtSurfaces = 0;

    computedNetArea.allocate(state.dataSurface->TotSurfaces);
    computedNetArea = 0.0; // start at zero, add wall area and subtract window and door area

    for (int iSurf : state.dataSurface->AllSurfaceListReportOrder) {
        zonePt = Surface(iSurf).Zone;
        // only exterior surfaces including underground
        if ((Surface(iSurf).ExtBoundCond == ExternalEnvironment) || (Surface(iSurf).ExtBoundCond == Ground) ||
            (Surface(iSurf).ExtBoundCond == GroundFCfactorMethod) || (Surface(iSurf).ExtBoundCond == KivaFoundation)) {
            isExterior = true;
            {
                auto const SELECT_CASE_var(Surface(iSurf).Class);
                if ((SELECT_CASE_var == SurfaceClass::Wall) || (SELECT_CASE_var == SurfaceClass::Floor) || (SELECT_CASE_var == SurfaceClass::Roof)) {
                    surfName = Surface(iSurf).Name;
                    curCons = Surface(iSurf).Construction;
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchOpCons, surfName, state.dataConstruction->Construct(curCons).Name);
                    PreDefTableEntry(
                        state, state.dataOutRptPredefined->pdchOpRefl, surfName, 1 - state.dataConstruction->Construct(curCons).OutsideAbsorpSolar);
                    PreDefTableEntry(
                        state, state.dataOutRptPredefined->pdchOpUfactNoFilm, surfName, state.dataHeatBal->NominalU(Surface(iSurf).Construction), 3);
                    mult = state.dataHeatBal->Zone(zonePt).Multiplier * state.dataHeatBal->Zone(zonePt).ListMultiplier;
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchOpGrArea, surfName, Surface(iSurf).GrossArea * mult);
                    computedNetArea(iSurf) += Surface(iSurf).GrossArea * mult;
                    curAzimuth = Surface(iSurf).Azimuth;
                    // Round to two decimals, like the display in tables
                    // (PreDefTableEntry uses a fortran style write, that rounds rather than trim)
                    curAzimuth = round(curAzimuth * 100.0) / 100.0;
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchOpAzimuth, surfName, curAzimuth);
                    curTilt = Surface(iSurf).Tilt;
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchOpTilt, surfName, curTilt);
                    if ((curTilt >= 60.0) && (curTilt < 180.0)) {
                        if ((curAzimuth >= 315.0) || (curAzimuth < 45.0)) {
                            PreDefTableEntry(state, state.dataOutRptPredefined->pdchOpDir, surfName, "N");
                        } else if ((curAzimuth >= 45.0) && (curAzimuth < 135.0)) {
                            PreDefTableEntry(state, state.dataOutRptPredefined->pdchOpDir, surfName, "E");
                        } else if ((curAzimuth >= 135.0) && (curAzimuth < 225.0)) {
                            PreDefTableEntry(state, state.dataOutRptPredefined->pdchOpDir, surfName, "S");
                        } else if ((curAzimuth >= 225.0) && (curAzimuth < 315.0)) {
                            PreDefTableEntry(state, state.dataOutRptPredefined->pdchOpDir, surfName, "W");
                        }
                    }
                } else if ((SELECT_CASE_var == SurfaceClass::Window) || (SELECT_CASE_var == SurfaceClass::TDD_Dome)) {
                    surfName = Surface(iSurf).Name;
                    curCons = Surface(iSurf).Construction;
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenCons, surfName, state.dataConstruction->Construct(curCons).Name);
                    zonePt = Surface(iSurf).Zone;
                    mult = state.dataHeatBal->Zone(zonePt).Multiplier * state.dataHeatBal->Zone(zonePt).ListMultiplier * Surface(iSurf).Multiplier;
                    // include the frame area if present
                    windowArea = Surface(iSurf).GrossArea;
                    frameArea = 0.0;
                    dividerArea = 0.0;
                    frameDivNum = Surface(iSurf).FrameDivider;
                    if (frameDivNum != 0) {
                        frameWidth = state.dataSurface->FrameDivider(frameDivNum).FrameWidth;
                        frameArea = (Surface(iSurf).Height + 2.0 * frameWidth) * (Surface(iSurf).Width + 2.0 * frameWidth) -
                                    (Surface(iSurf).Height * Surface(iSurf).Width);
                        windowArea += frameArea;
                        dividerArea =
                            state.dataSurface->FrameDivider(frameDivNum).DividerWidth *
                            (state.dataSurface->FrameDivider(frameDivNum).HorDividers * Surface(iSurf).Width +
                             state.dataSurface->FrameDivider(frameDivNum).VertDividers * Surface(iSurf).Height -
                             state.dataSurface->FrameDivider(frameDivNum).HorDividers * state.dataSurface->FrameDivider(frameDivNum).VertDividers *
                                 state.dataSurface->FrameDivider(frameDivNum).DividerWidth);
                        PreDefTableEntry(state,
                                         state.dataOutRptPredefined->pdchFenFrameConductance,
                                         surfName,
                                         state.dataSurface->FrameDivider(frameDivNum).FrameConductance,
                                         3);
                        PreDefTableEntry(state,
                                         state.dataOutRptPredefined->pdchFenDividerConductance,
                                         surfName,
                                         state.dataSurface->FrameDivider(frameDivNum).DividerConductance,
                                         3);
                    }
                    windowAreaWMult = windowArea * mult;
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenAreaOf1, surfName, windowArea);
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenFrameAreaOf1, surfName, frameArea);
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenDividerAreaOf1, surfName, dividerArea);
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenGlassAreaOf1, surfName, windowArea - (frameArea + dividerArea));
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenArea, surfName, windowAreaWMult);
                    computedNetArea(Surface(iSurf).BaseSurf) -= windowAreaWMult;
                    nomUfact = state.dataHeatBal->NominalU(Surface(iSurf).Construction);
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenUfact, surfName, nomUfact, 3);
                    // if the construction report is requested the SummerSHGC is already calculated
                    if (state.dataConstruction->Construct(curCons).SummerSHGC != 0) {
                        SHGCSummer = state.dataConstruction->Construct(curCons).SummerSHGC;
                        TransVisNorm = state.dataConstruction->Construct(curCons).VisTransNorm;
                    } else {
                        // must calculate Summer SHGC
                        if (!state.dataConstruction->Construct(curCons).WindowTypeEQL) {
                            CalcNominalWindowCond(state, curCons, 2, nomCond, SHGCSummer, TransSolNorm, TransVisNorm, errFlag);
                        }
                    }
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenSHGC, surfName, SHGCSummer, 3);
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenVisTr, surfName, TransVisNorm, 3);
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenParent, surfName, Surface(iSurf).BaseSurfName);
                    curAzimuth = Surface(iSurf).Azimuth;
                    // Round to two decimals, like the display in tables
                    curAzimuth = round(curAzimuth * 100.0) / 100.0;
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenAzimuth, surfName, curAzimuth);
                    isNorth = false;
                    curTilt = Surface(iSurf).Tilt;
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenTilt, surfName, curTilt);
                    if ((curTilt >= 60.0) && (curTilt < 180.0)) {
                        if ((curAzimuth >= 315.0) || (curAzimuth < 45.0)) {
                            PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenDir, surfName, "N");
                            isNorth = true;
                        } else if ((curAzimuth >= 45.0) && (curAzimuth < 135.0)) {
                            PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenDir, surfName, "E");
                        } else if ((curAzimuth >= 135.0) && (curAzimuth < 225.0)) {
                            PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenDir, surfName, "S");
                        } else if ((curAzimuth >= 225.0) && (curAzimuth < 315.0)) {
                            PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenDir, surfName, "W");
                        }
                    }
                    curWSC = Surface(iSurf).activeWindowShadingControl;
                    // compute totals for area weighted averages
                    fenTotArea += windowAreaWMult;
                    ufactArea += nomUfact * windowAreaWMult;
                    shgcArea += SHGCSummer * windowAreaWMult;
                    vistranArea += TransVisNorm * windowAreaWMult;
                    if (isNorth) {
                        fenTotAreaNorth += windowAreaWMult;
                        ufactAreaNorth += nomUfact * windowAreaWMult;
                        shgcAreaNorth += SHGCSummer * windowAreaWMult;
                        vistranAreaNorth += TransVisNorm * windowAreaWMult;
                    } else {
                        fenTotAreaNonNorth += windowAreaWMult;
                        ufactAreaNonNorth += nomUfact * windowAreaWMult;
                        shgcAreaNonNorth += SHGCSummer * windowAreaWMult;
                        vistranAreaNonNorth += TransVisNorm * windowAreaWMult;
                    }
                    // shading
                    if (Surface(iSurf).HasShadeControl) {
                        PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenSwitchable, surfName, "Yes");
                        PreDefTableEntry(
                            state, state.dataOutRptPredefined->pdchWscName, surfName, state.dataSurface->WindowShadingControl(curWSC).Name);
                        // shading report
                        std::vector<std::string> WindowShadingTypeStr = {
                            "No Shade",  // 0
                            "Shade Off", // 1
                            "Interior Shade",
                            "Switchable Glazing",
                            "Exterior Shade",
                            "Exterior Screen",
                            "Interior Blind",
                            "Exterior Blind",
                            "Between Glass Shade",
                            "Between Glass Blind",
                        };
                        std::vector<std::string> WindowShadingControlTypeStr = {"Uncontrolled",
                                                                                "AlwaysOn",
                                                                                "AlwaysOff",
                                                                                "OnIfScheduleAllows",
                                                                                "OnIfHighSolarOnWindow",
                                                                                "OnIfHighHorizontalSolar",
                                                                                "OnIfHighOutdoorAirTemperature",
                                                                                "OnIfHighZoneAirTemperature",
                                                                                "OnIfHighZoneCooling",
                                                                                "OnIfHighGlare",
                                                                                "MeetDaylightIlluminanceSetpoint",
                                                                                "OnNightIfLowOutdoorTempAndOffDay",
                                                                                "OnNightIfLowInsideTempAndOffDay",
                                                                                "OnNightIfHeatingAndOffDay",
                                                                                "OnNightIfLowOutdoorTempAndOnDayIfCooling",
                                                                                "OnNightIfHeatingAndOnDayIfCooling",
                                                                                "OffNightAndOnDayIfCoolingAndHighSolarOnWindow",
                                                                                "OnNightAndOnDayIfCoolingAndHighSolarOnWindow",
                                                                                "OnIfHighOutdoorAirTempAndHighSolarOnWindow",
                                                                                "OnIfHighOutdoorAirTempAndHighHorizontalSolar",
                                                                                "OnIfHighZoneAirTempAndHighSolarOnWindow",
                                                                                "OnIfHighZoneAirTempAndHighHorizontalSolar"};
                        PreDefTableEntry(state,
                                         state.dataOutRptPredefined->pdchWscShading,
                                         surfName,
                                         WindowShadingTypeStr[int(state.dataSurface->WindowShadingControl(curWSC).ShadingType)]);
                        PreDefTableEntry(state,
                                         state.dataOutRptPredefined->pdchWscControl,
                                         surfName,
                                         WindowShadingControlTypeStr[int(state.dataSurface->WindowShadingControl(curWSC).ShadingControlType)]);

                        // output list of all possible shading contructions for shaded windows including those with storms
                        std::string names = "";
                        for (auto construction : Surface(iSurf).shadedConstructionList) {
                            if (!names.empty()) names.append("; ");
                            names.append(state.dataConstruction->Construct(construction).Name);
                        }
                        for (auto construction : Surface(iSurf).shadedStormWinConstructionList) {
                            if (!names.empty()) names.append("; ");
                            names.append(state.dataConstruction->Construct(construction).Name);
                        }
                        PreDefTableEntry(state, state.dataOutRptPredefined->pdchWscShadCons, surfName, names);

                        if (state.dataSurface->WindowShadingControl(curWSC).GlareControlIsActive) {
                            PreDefTableEntry(state, state.dataOutRptPredefined->pdchWscGlare, surfName, "Yes");
                        } else {
                            PreDefTableEntry(state, state.dataOutRptPredefined->pdchWscGlare, surfName, "No");
                        }
                    } else {
                        PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenSwitchable, surfName, "No");
                    }
                } else if (SELECT_CASE_var == SurfaceClass::Door) {
                    surfName = Surface(iSurf).Name;
                    curCons = Surface(iSurf).Construction;
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchDrCons, surfName, state.dataConstruction->Construct(curCons).Name);
                    PreDefTableEntry(
                        state, state.dataOutRptPredefined->pdchDrUfactNoFilm, surfName, state.dataHeatBal->NominalU(Surface(iSurf).Construction), 3);
                    mult = state.dataHeatBal->Zone(zonePt).Multiplier * state.dataHeatBal->Zone(zonePt).ListMultiplier;
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchDrGrArea, surfName, Surface(iSurf).GrossArea * mult);
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchDrParent, surfName, Surface(iSurf).BaseSurfName);
                    computedNetArea(Surface(iSurf).BaseSurf) -= Surface(iSurf).GrossArea * mult;
                }
            }
        } else {
            // interior surfaces
            isExterior = false;
            if ((Surface(iSurf).Class == SurfaceClass::Wall) || (Surface(iSurf).Class == SurfaceClass::Floor) ||
                (Surface(iSurf).Class == SurfaceClass::Roof)) {
                surfName = Surface(iSurf).Name;
                curCons = Surface(iSurf).Construction;
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchIntOpCons, surfName, state.dataConstruction->Construct(curCons).Name);
                PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchIntOpRefl, surfName, 1 - state.dataConstruction->Construct(curCons).OutsideAbsorpSolar);
                PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchIntOpUfactNoFilm, surfName, state.dataHeatBal->NominalU(Surface(iSurf).Construction), 3);
                mult = state.dataHeatBal->Zone(zonePt).Multiplier * state.dataHeatBal->Zone(zonePt).ListMultiplier;
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchIntOpGrArea, surfName, Surface(iSurf).GrossArea * mult);
                computedNetArea(iSurf) += Surface(iSurf).GrossArea * mult;
                curAzimuth = Surface(iSurf).Azimuth;
                // Round to two decimals, like the display in tables
                // (PreDefTableEntry uses a fortran style write, that rounds rather than trim)
                curAzimuth = round(curAzimuth * 100.0) / 100.0;
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchIntOpAzimuth, surfName, curAzimuth);
                curTilt = Surface(iSurf).Tilt;
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchIntOpTilt, surfName, curTilt);
                if ((curTilt >= 60.0) && (curTilt < 180.0)) {
                    if ((curAzimuth >= 315.0) || (curAzimuth < 45.0)) {
                        PreDefTableEntry(state, state.dataOutRptPredefined->pdchIntOpDir, surfName, "N");
                    } else if ((curAzimuth >= 45.0) && (curAzimuth < 135.0)) {
                        PreDefTableEntry(state, state.dataOutRptPredefined->pdchIntOpDir, surfName, "E");
                    } else if ((curAzimuth >= 135.0) && (curAzimuth < 225.0)) {
                        PreDefTableEntry(state, state.dataOutRptPredefined->pdchIntOpDir, surfName, "S");
                    } else if ((curAzimuth >= 225.0) && (curAzimuth < 315.0)) {
                        PreDefTableEntry(state, state.dataOutRptPredefined->pdchIntOpDir, surfName, "W");
                    }
                }
                // interior window report
            } else if ((Surface(iSurf).Class == SurfaceClass::Window) || (Surface(iSurf).Class == SurfaceClass::TDD_Dome)) {
                if (!has_prefix(Surface(iSurf).Name, "iz-")) { // don't count created interzone surfaces that are mirrors of other surfaces
                    surfName = Surface(iSurf).Name;
                    curCons = Surface(iSurf).Construction;
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchIntFenCons, surfName, state.dataConstruction->Construct(curCons).Name);
                    zonePt = Surface(iSurf).Zone;
                    mult = state.dataHeatBal->Zone(zonePt).Multiplier * state.dataHeatBal->Zone(zonePt).ListMultiplier * Surface(iSurf).Multiplier;
                    // include the frame area if present
                    windowArea = Surface(iSurf).GrossArea;
                    if (Surface(iSurf).FrameDivider != 0) {
                        frameWidth = state.dataSurface->FrameDivider(Surface(iSurf).FrameDivider).FrameWidth;
                        frameArea = (Surface(iSurf).Height + 2 * frameWidth) * (Surface(iSurf).Width + 2 * frameWidth) -
                                    (Surface(iSurf).Height * Surface(iSurf).Width);
                        windowArea += frameArea;
                    }
                    windowAreaWMult = windowArea * mult;
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchIntFenAreaOf1, surfName, windowArea);
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchIntFenArea, surfName, windowAreaWMult);
                    computedNetArea(Surface(iSurf).BaseSurf) -= windowAreaWMult;
                    nomUfact = state.dataHeatBal->NominalU(Surface(iSurf).Construction);
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchIntFenUfact, surfName, nomUfact, 3);
                    // if the construction report is requested the SummerSHGC is already calculated
                    if (state.dataConstruction->Construct(curCons).SummerSHGC != 0) {
                        SHGCSummer = state.dataConstruction->Construct(curCons).SummerSHGC;
                        TransVisNorm = state.dataConstruction->Construct(curCons).VisTransNorm;
                    } else {
                        // must calculate Summer SHGC
                        if (!state.dataConstruction->Construct(curCons).WindowTypeEQL) {
                            CalcNominalWindowCond(state, curCons, 2, nomCond, SHGCSummer, TransSolNorm, TransVisNorm, errFlag);
                        }
                    }
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchIntFenSHGC, surfName, SHGCSummer, 3);
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchIntFenVisTr, surfName, TransVisNorm, 3);
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchIntFenParent, surfName, Surface(iSurf).BaseSurfName);
                    // compute totals for area weighted averages
                    intFenTotArea += windowAreaWMult;
                    intUfactArea += nomUfact * windowAreaWMult;
                    intShgcArea += SHGCSummer * windowAreaWMult;
                    intVistranArea += TransVisNorm * windowAreaWMult;
                }
            } else if (Surface(iSurf).Class == SurfaceClass::Door) {
                surfName = Surface(iSurf).Name;
                curCons = Surface(iSurf).Construction;
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchIntDrCons, surfName, state.dataConstruction->Construct(curCons).Name);
                PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchIntDrUfactNoFilm, surfName, state.dataHeatBal->NominalU(Surface(iSurf).Construction), 3);
                mult = state.dataHeatBal->Zone(zonePt).Multiplier * state.dataHeatBal->Zone(zonePt).ListMultiplier;
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchIntDrGrArea, surfName, Surface(iSurf).GrossArea * mult);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchIntDrParent, surfName, Surface(iSurf).BaseSurfName);
                computedNetArea(Surface(iSurf).BaseSurf) -= Surface(iSurf).GrossArea * mult;
            }
        }
        int currSurfaceClass = int(Surface(iSurf).Class);
        assert(currSurfaceClass < int(SurfaceClass::Count));
        assert(currSurfaceClass > int(SurfaceClass::None));
        ++numSurfaces(currSurfaceClass);
        if (isExterior) {
            ++numExtSurfaces(currSurfaceClass);
        }
        if (Surface(iSurf).Class == SurfaceClass::Window) {
            if (state.dataSurface->SurfWinOriginalClass(iSurf) == SurfaceClass::GlassDoor ||
                state.dataSurface->SurfWinOriginalClass(iSurf) == SurfaceClass::TDD_Diffuser) {
                int currOriginalSurfaceClass = int(state.dataSurface->SurfWinOriginalClass(iSurf));
                ++numSurfaces(currOriginalSurfaceClass);
                if (isExterior) {
                    ++numExtSurfaces(currOriginalSurfaceClass);
                }
            }
        }
    }
    // for fins and overhangs just add them explicitly since not otherwise classified
    int totOverhangs = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Shading:Overhang") +
                       state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Shading:Overhang:Projection");
    numSurfaces(int(SurfaceClass::Overhang)) = totOverhangs;
    numExtSurfaces(int(SurfaceClass::Overhang)) = totOverhangs;
    int totFins = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Shading:Fin") +
                  state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Shading:Fin:Projection");
    numSurfaces(int(SurfaceClass::Fin)) = totFins;
    numExtSurfaces(int(SurfaceClass::Fin)) = totFins;
    // go through all the surfaces again and this time insert the net area results
    for (int iSurf : state.dataSurface->AllSurfaceListReportOrder) {
        zonePt = Surface(iSurf).Zone;
        auto const SurfaceClass(Surface(iSurf).Class);
        // exterior surfaces including underground
        if ((Surface(iSurf).ExtBoundCond == ExternalEnvironment) || (Surface(iSurf).ExtBoundCond == Ground) ||
            (Surface(iSurf).ExtBoundCond == GroundFCfactorMethod) || (Surface(iSurf).ExtBoundCond == KivaFoundation)) {
            if ((SurfaceClass == SurfaceClass::Wall) || (SurfaceClass == SurfaceClass::Floor) || (SurfaceClass == SurfaceClass::Roof)) {
                surfName = Surface(iSurf).Name;
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchOpNetArea, surfName, computedNetArea(iSurf));
            }
        } else {
            if ((SurfaceClass == SurfaceClass::Wall) || (SurfaceClass == SurfaceClass::Floor) || (SurfaceClass == SurfaceClass::Roof)) {
                surfName = Surface(iSurf).Name;
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchIntOpNetArea, surfName, computedNetArea(iSurf));
            }
        } // interior surfaces
    }
    // total
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenArea, "Total or Average", fenTotArea);
    if (fenTotArea > 0.0) {
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenUfact, "Total or Average", ufactArea / fenTotArea, 3);
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenSHGC, "Total or Average", shgcArea / fenTotArea, 3);
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenVisTr, "Total or Average", vistranArea / fenTotArea, 3);
    } else {
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenUfact, "Total or Average", "-");
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenSHGC, "Total or Average", "-");
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenVisTr, "Total or Average", "-");
    }
    // north
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenArea, "North Total or Average", fenTotAreaNorth);
    if (fenTotAreaNorth > 0.0) {
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenUfact, "North Total or Average", ufactAreaNorth / fenTotAreaNorth, 3);
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenSHGC, "North Total or Average", shgcAreaNorth / fenTotAreaNorth, 3);
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenVisTr, "North Total or Average", vistranAreaNorth / fenTotAreaNorth, 3);
    } else {
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenUfact, "North Total or Average", "-");
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenSHGC, "North Total or Average", "-");
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenVisTr, "North Total or Average", "-");
    }
    // non-north
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenArea, "Non-North Total or Average", fenTotAreaNonNorth);
    if (fenTotAreaNonNorth > 0.0) {
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenUfact, "Non-North Total or Average", ufactAreaNonNorth / fenTotAreaNonNorth, 3);
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenSHGC, "Non-North Total or Average", shgcAreaNonNorth / fenTotAreaNonNorth, 3);
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenVisTr, "Non-North Total or Average", vistranAreaNonNorth / fenTotAreaNonNorth, 3);
    } else {
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenUfact, "Non-North Total or Average", "-");
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenSHGC, "Non-North Total or Average", "-");
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchFenVisTr, "Non-North Total or Average", "-");
    }
    // interior fenestration totals
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchIntFenArea, "Total or Average", intFenTotArea);
    if (intFenTotArea > 0.0) {
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchIntFenUfact, "Total or Average", intUfactArea / intFenTotArea, 3);
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchIntFenSHGC, "Total or Average", intShgcArea / intFenTotArea, 3);
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchIntFenVisTr, "Total or Average", intVistranArea / intFenTotArea, 3);
    } else {
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchIntFenUfact, "Total or Average", "-");
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchIntFenSHGC, "Total or Average", "-");
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchIntFenVisTr, "Total or Average", "-");
    }
    // counts
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchSurfCntTot, "Wall", numSurfaces(int(SurfaceClass::Wall)));
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchSurfCntExt, "Wall", numExtSurfaces(int(SurfaceClass::Wall)));
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchSurfCntTot, "Floor", numSurfaces(int(SurfaceClass::Floor)));
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchSurfCntExt, "Floor", numExtSurfaces(int(SurfaceClass::Floor)));
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchSurfCntTot, "Roof", numSurfaces(int(SurfaceClass::Roof)));
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchSurfCntExt, "Roof", numExtSurfaces(int(SurfaceClass::Roof)));
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchSurfCntTot, "Internal Mass", numSurfaces(int(SurfaceClass::IntMass)));
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchSurfCntExt, "Internal Mass", numExtSurfaces(int(SurfaceClass::IntMass)));
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchSurfCntTot, "Building Detached Shading", numSurfaces(int(SurfaceClass::Detached_B)));
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchSurfCntExt, "Building Detached Shading", numExtSurfaces(int(SurfaceClass::Detached_B)));
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchSurfCntTot, "Fixed Detached Shading", numSurfaces(int(SurfaceClass::Detached_F)));
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchSurfCntExt, "Fixed Detached Shading", numExtSurfaces(int(SurfaceClass::Detached_F)));
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchSurfCntTot, "Window", numSurfaces(int(SurfaceClass::Window)));
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchSurfCntExt, "Window", numExtSurfaces(int(SurfaceClass::Window)));
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchSurfCntTot, "Door", numSurfaces(int(SurfaceClass::Door)));
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchSurfCntExt, "Door", numExtSurfaces(int(SurfaceClass::Door)));
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchSurfCntTot, "Glass Door", numSurfaces(int(SurfaceClass::GlassDoor)));
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchSurfCntExt, "Glass Door", numExtSurfaces(int(SurfaceClass::GlassDoor)));
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchSurfCntTot, "Shading", numSurfaces(int(SurfaceClass::Shading)));
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchSurfCntExt, "Shading", numExtSurfaces(int(SurfaceClass::Shading)));
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchSurfCntTot, "Overhang", numSurfaces(int(SurfaceClass::Overhang)));
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchSurfCntExt, "Overhang", numExtSurfaces(int(SurfaceClass::Overhang)));
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchSurfCntTot, "Fin", numSurfaces(int(SurfaceClass::Fin)));
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchSurfCntExt, "Fin", numExtSurfaces(int(SurfaceClass::Fin)));
    PreDefTableEntry(state, state.dataOutRptPredefined->pdchSurfCntTot, "Tubular Daylighting Device Dome", numSurfaces(int(SurfaceClass::TDD_Dome)));
    PreDefTableEntry(
        state, state.dataOutRptPredefined->pdchSurfCntExt, "Tubular Daylighting Device Dome", numExtSurfaces(int(SurfaceClass::TDD_Dome)));
    PreDefTableEntry(
        state, state.dataOutRptPredefined->pdchSurfCntTot, "Tubular Daylighting Device Diffuser", numSurfaces(int(SurfaceClass::TDD_Diffuser)));
    PreDefTableEntry(
        state, state.dataOutRptPredefined->pdchSurfCntExt, "Tubular Daylighting Device Diffuser", numExtSurfaces(int(SurfaceClass::TDD_Diffuser)));
}

void AllocateSurfaceHeatBalArrays(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Liesen
    //       DATE WRITTEN   February 1998
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine

    // METHODOLOGY EMPLOYED:
    // Uses the status flags to trigger variable allocation.

    // REFERENCES:
    // na

    // USE STATEMENTS:
    //  USE DataRoomAirModel, ONLY: IsZoneDV,IsZoneCV,HVACMassFlow, ZoneDVMixedFlag

    auto &Surface(state.dataSurface->Surface);

    // Use the total number of surfaces to allocate variables to avoid a surface number limit
    state.dataHeatBalSurf->CTFConstInPart.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->CTFConstOutPart.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->CTFCross0.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->CTFInside0.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->TH11Surf.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->CTFSourceIn0.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->QsrcHistSurf1.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->IsAdiabatic.dimension(state.dataSurface->TotSurfaces, 0);
    state.dataHeatBalSurf->IsNotAdiabatic.dimension(state.dataSurface->TotSurfaces, 0);
    state.dataHeatBalSurf->IsSource.dimension(state.dataSurface->TotSurfaces, 0);
    state.dataHeatBalSurf->IsNotSource.dimension(state.dataSurface->TotSurfaces, 0);
    state.dataHeatBalSurf->IsPoolSurf.dimension(state.dataSurface->TotSurfaces, 0);
    state.dataHeatBalSurf->IsNotPoolSurf.dimension(state.dataSurface->TotSurfaces, 0);
    state.dataHeatBalSurf->TempTermSurf.dimension(state.dataSurface->TotSurfaces, 0);
    state.dataHeatBalSurf->TempDivSurf.dimension(state.dataSurface->TotSurfaces, 0);
    if (state.dataHeatBal->AnyInternalHeatSourceInInput) {
        state.dataHeatBalFanSys->CTFTsrcConstPart.dimension(state.dataSurface->TotSurfaces, 0.0);
        state.dataHeatBalFanSys->CTFTuserConstPart.dimension(state.dataSurface->TotSurfaces, 0.0);
    }

    state.dataHeatBal->SurfTempEffBulkAir.dimension(state.dataSurface->TotSurfaces, ZoneInitialTemp);
    state.dataHeatBal->HConvIn.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->HcExtSurf.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->HAirExtSurf.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->HSkyExtSurf.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->HGrdExtSurf.dimension(state.dataSurface->TotSurfaces, 0.0);

    state.dataHeatBalSurf->TempSurfIn.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->TempInsOld.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->TempSurfInTmp.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurfMgr->RefAirTemp.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->SurfQRadLWOutSrdSurfs.dimension(state.dataSurface->TotSurfaces, 0.0);

    state.dataHeatBal->SurfWinQRadSWwinAbs.dimension(state.dataSurface->TotSurfaces, DataWindowEquivalentLayer::CFSMAXNL + 1, 0.0);
    state.dataHeatBal->SurfWinInitialDifSolwinAbs.dimension(state.dataSurface->TotSurfaces, DataWindowEquivalentLayer::CFSMAXNL, 0.0);
    state.dataHeatBalSurf->SurfQRadSWOutMvIns.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBal->SurfQRadThermInAbs.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->SurfQAdditionalHeatSourceOutside.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->SurfQAdditionalHeatSourceInside.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->SUMH.dimension(state.dataSurface->TotSurfaces, 0);

    state.dataHeatBalSurf->TH.dimension(2, Construction::MaxCTFTerms, state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->SurfTempOut.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->TempSurfInRep.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->TempSurfInMovInsRep.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->QConvInReport.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->QdotConvInRepPerArea.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->QdotConvInRep.dimension(state.dataSurface->TotSurfaces, 0.0);

    state.dataHeatBalSurf->QRadNetSurfInReport.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->QdotRadNetSurfInRep.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->QdotRadNetSurfInRepPerArea.dimension(state.dataSurface->TotSurfaces, 0.0);

    state.dataHeatBalSurf->QRadSolarInReport.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->QdotRadSolarInRep.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->QdotRadSolarInRepPerArea.dimension(state.dataSurface->TotSurfaces, 0.0);

    state.dataHeatBalSurf->QRadLightsInReport.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->QdotRadLightsInRep.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->QdotRadLightsInRepPerArea.dimension(state.dataSurface->TotSurfaces, 0.0);

    state.dataHeatBalSurf->QRadIntGainsInReport.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->QdotRadIntGainsInRep.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->QdotRadIntGainsInRepPerArea.dimension(state.dataSurface->TotSurfaces, 0.0);

    state.dataHeatBalSurf->QRadHVACInReport.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->QdotRadHVACInRep.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->QdotRadHVACInRepPerArea.dimension(state.dataSurface->TotSurfaces, 0.0);

    state.dataHeatBalSurf->QConvOutReport.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->QdotConvOutRepPerArea.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->QdotConvOutRep.dimension(state.dataSurface->TotSurfaces, 0.0);

    state.dataHeatBalSurf->QdotRadOutRep.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->QdotRadOutRepPerArea.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->QRadOutReport.dimension(state.dataSurface->TotSurfaces, 0.0);

    state.dataHeatBalSurf->QAirExtReport.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->QHeatEmiReport.dimension(state.dataSurface->TotSurfaces, 0.0);

    state.dataHeatBal->SurfOpaqSWOutAbsTotalReport.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBal->SurfOpaqSWOutAbsEnergyReport.dimension(state.dataSurface->TotSurfaces, 0.0);

    state.dataHeatBalSurf->SurfOpaqInsFaceConduction.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->SurfOpaqInsFaceConductionFlux.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->SurfOpaqInsFaceCondGainRep.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->SurfOpaqInsFaceCondLossRep.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->SurfOpaqInsFaceConductionEnergy.dimension(state.dataSurface->TotSurfaces, 0.0);

    state.dataHeatBalSurf->SurfOpaqOutsideFaceConduction.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->SurfOpaqOutsideFaceConductionFlux.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->SurfOpaqExtFaceCondGainRep.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->SurfOpaqExtFaceCondLossRep.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->SurfOpaqOutsideFaceConductionEnergy.dimension(state.dataSurface->TotSurfaces, 0.0);

    state.dataHeatBalSurf->SurfOpaqAvgFaceCondGainRep.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->SurfOpaqAvgFaceCondLossRep.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->SurfOpaqAvgFaceConduction.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->SurfOpaqAvgFaceConductionFlux.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->SurfOpaqAvgFaceConductionEnergy.dimension(state.dataSurface->TotSurfaces, 0.0);

    state.dataHeatBalSurf->SurfOpaqStorageGainRep.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->SurfOpaqStorageCondLossRep.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->SurfOpaqStorageConduction.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->SurfOpaqStorageConductionFlux.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->SurfOpaqStorageConductionEnergy.dimension(state.dataSurface->TotSurfaces, 0.0);

    state.dataHeatBalSurf->SurfOpaqInsFaceBeamSolAbsorbed.dimension(state.dataSurface->TotSurfaces, 0.0);

    state.dataHeatBalSurf->SurfOpaqQRadSWOutAbs.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->SurfOpaqQRadSWInAbs.dimension(state.dataSurface->TotSurfaces, 0.0);

    state.dataHeatBalSurf->SurfOpaqInitialDifSolInAbs.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->SurfWinInitialDifSolInTrans.dimension(state.dataSurface->TotSurfaces, 0.0);

    state.dataHeatBalSurf->SurfNetLWRadToSurf.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->SurfOpaqQRadSWLightsInAbs.dimension(state.dataSurface->TotSurfaces, 0.0);

    state.dataHeatBalSurf->QH.dimension(2, Construction::MaxCTFTerms, state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->THM.dimension(2, Construction::MaxCTFTerms, state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->QHM.dimension(2, Construction::MaxCTFTerms, state.dataSurface->TotSurfaces, 0.0);
    if (state.dataHeatBal->AnyInternalHeatSourceInInput) {
        state.dataHeatBalSurf->TempSource.dimension(state.dataSurface->TotSurfaces, 0.0);
        state.dataHeatBalSurf->TempUserLoc.dimension(state.dataSurface->TotSurfaces, 0.0);
        state.dataHeatBalSurf->TsrcHist.dimension(state.dataSurface->TotSurfaces, Construction::MaxCTFTerms, 0.0);
        state.dataHeatBalSurf->TuserHist.dimension(state.dataSurface->TotSurfaces, Construction::MaxCTFTerms, 0.0);
        state.dataHeatBalSurf->QsrcHist.dimension(state.dataSurface->TotSurfaces, Construction::MaxCTFTerms, 0.0);
        state.dataHeatBalSurf->TsrcHistM.dimension(state.dataSurface->TotSurfaces, Construction::MaxCTFTerms, 0.0);
        state.dataHeatBalSurf->TuserHistM.dimension(state.dataSurface->TotSurfaces, Construction::MaxCTFTerms, 0.0);
        state.dataHeatBalSurf->QsrcHistM.dimension(state.dataSurface->TotSurfaces, Construction::MaxCTFTerms, 0.0);
    }

    state.dataHeatBalFanSys->RadSysTiHBConstCoef.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalFanSys->RadSysTiHBToutCoef.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalFanSys->RadSysTiHBQsrcCoef.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalFanSys->RadSysToHBConstCoef.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalFanSys->RadSysToHBTinCoef.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalFanSys->RadSysToHBQsrcCoef.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalFanSys->QRadSysSource.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalFanSys->TCondFDSourceNode.dimension(state.dataSurface->TotSurfaces, 15.0);
    state.dataHeatBalFanSys->QHTRadSysSurf.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalFanSys->QHWBaseboardSurf.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalFanSys->QSteamBaseboardSurf.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalFanSys->QElecBaseboardSurf.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalFanSys->QCoolingPanelSurf.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalFanSys->QRadSurfAFNDuct.dimension(state.dataSurface->TotSurfaces, 0.0);

    // allocate terms used for pool surface heat balance
    state.dataHeatBalFanSys->QPoolSurfNumerator.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalFanSys->PoolHeatTransCoefs.dimension(state.dataSurface->TotSurfaces, 0.0);

    // allocate term used as sink for PV electricity
    state.dataHeatBalFanSys->QPVSysSource.dimension(state.dataSurface->TotSurfaces, 0.0);

    // Allocate the moisture balance arrays
    state.dataMstBal->TempOutsideAirFD.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataMstBal->RhoVaporAirOut.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataMstBal->RhoVaporSurfIn.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataMstBal->RhoVaporAirIn.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataMstBal->HConvExtFD.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataMstBal->HMassConvExtFD.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataMstBal->HConvInFD.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataMstBal->HMassConvInFD.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataMstBal->HSkyFD.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataMstBal->HGrndFD.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataMstBal->HAirFD.dimension(state.dataSurface->TotSurfaces, 0.0);

    state.dataHeatBalSurf->SurfNetLWRadToSurf.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->SurfOpaqQRadSWLightsInAbs.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataSurface->SurfSkySolarInc.dimension(state.dataSurface->TotSurfaces, 0);
    state.dataSurface->SurfGndSolarInc.dimension(state.dataSurface->TotSurfaces, 0);
    // allocate movable insulation arrays
    if (state.dataSurface->AnyMovableInsulation) {
        state.dataHeatBalSurf->SurfMovInsulExtPresent.dimension(state.dataSurface->TotSurfaces, false);
        state.dataHeatBalSurf->SurfMovInsulIntPresent.dimension(state.dataSurface->TotSurfaces, false);
        state.dataHeatBalSurf->SurfMovInsulIntPresentPrevTS.dimension(state.dataSurface->TotSurfaces, false);
        state.dataHeatBalSurf->SurfMovInsulHExt.dimension(state.dataSurface->TotSurfaces, 0.0);
        state.dataHeatBalSurf->SurfMovInsulHInt.dimension(state.dataSurface->TotSurfaces, 0.0);
    }
    state.dataHeatBalSurf->SurfAbsSolarExt.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->SurfAbsThermalExt.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->SurfRoughnessExt.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->SurfAbsSolarInt.dimension(state.dataSurface->TotSurfaces, 0.0);
    state.dataHeatBalSurf->SurfAbsThermalInt.dimension(state.dataSurface->TotSurfaces, 0.0);

    DisplayString(state, "Setting up Surface Reporting Variables");
    // Setup surface report variables CurrentModuleObject='Opaque Surfaces'
    for (int loop = 1; loop <= state.dataSurface->TotSurfaces; ++loop) {
        if (!Surface(loop).HeatTransSurf) continue;
        SetupOutputVariable(state,
                            "Surface Inside Face Temperature",
                            OutputProcessor::Unit::C,
                            state.dataHeatBalSurf->TempSurfInRep(loop),
                            "Zone",
                            "State",
                            Surface(loop).Name);
        SetupOutputVariable(state,
                            "Surface Inside Face Interior Movable Insulation Temperature",
                            OutputProcessor::Unit::C,
                            state.dataHeatBalSurf->TempSurfInMovInsRep(loop),
                            "Zone",
                            "State",
                            Surface(loop).Name);

        if (Surface(loop).ExtBoundCond != KivaFoundation) {
            SetupOutputVariable(state,
                                "Surface Outside Face Temperature",
                                OutputProcessor::Unit::C,
                                state.dataHeatBalSurf->SurfTempOut(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);
        }

        SetupOutputVariable(state,
                            "Surface Inside Face Adjacent Air Temperature",
                            OutputProcessor::Unit::C,
                            state.dataHeatBal->SurfTempEffBulkAir(loop),
                            "Zone",
                            "State",
                            Surface(loop).Name);
        SetupOutputVariable(state,
                            "Surface Inside Face Convection Heat Transfer Coefficient",
                            OutputProcessor::Unit::W_m2K,
                            state.dataHeatBal->HConvIn(loop),
                            "Zone",
                            "State",
                            Surface(loop).Name);
        SetupOutputVariable(state,
                            "Surface Inside Face Convection Heat Gain Rate",
                            OutputProcessor::Unit::W,
                            state.dataHeatBalSurf->QdotConvInRep(loop),
                            "Zone",
                            "State",
                            Surface(loop).Name);
        SetupOutputVariable(state,
                            "Surface Inside Face Convection Heat Gain Rate per Area",
                            OutputProcessor::Unit::W_m2,
                            state.dataHeatBalSurf->QdotConvInRepPerArea(loop),
                            "Zone",
                            "State",
                            Surface(loop).Name);
        SetupOutputVariable(state,
                            "Surface Inside Face Convection Heat Gain Energy",
                            OutputProcessor::Unit::J,
                            state.dataHeatBalSurf->QConvInReport(loop),
                            "Zone",
                            "Sum",
                            Surface(loop).Name);

        SetupOutputVariable(state,
                            "Surface Inside Face Net Surface Thermal Radiation Heat Gain Rate",
                            OutputProcessor::Unit::W,
                            state.dataHeatBalSurf->QdotRadNetSurfInRep(loop),
                            "Zone",
                            "State",
                            Surface(loop).Name);
        SetupOutputVariable(state,
                            "Surface Inside Face Net Surface Thermal Radiation Heat Gain Rate per Area",
                            OutputProcessor::Unit::W_m2,
                            state.dataHeatBalSurf->QdotRadNetSurfInRepPerArea(loop),
                            "Zone",
                            "State",
                            Surface(loop).Name);
        SetupOutputVariable(state,
                            "Surface Inside Face Net Surface Thermal Radiation Heat Gain Energy",
                            OutputProcessor::Unit::J,
                            state.dataHeatBalSurf->QRadNetSurfInReport(loop),
                            "Zone",
                            "Sum",
                            Surface(loop).Name);

        if (Surface(loop).Class != SurfaceClass::Window) {
            SetupOutputVariable(state,
                                "Surface Inside Face Solar Radiation Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBalSurf->QdotRadSolarInRep(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);
            SetupOutputVariable(state,
                                "Surface Inside Face Solar Radiation Heat Gain Rate per Area",
                                OutputProcessor::Unit::W_m2,
                                state.dataHeatBalSurf->QdotRadSolarInRepPerArea(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);
            SetupOutputVariable(state,
                                "Surface Inside Face Solar Radiation Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBalSurf->QRadSolarInReport(loop),
                                "Zone",
                                "Sum",
                                Surface(loop).Name);

            SetupOutputVariable(state,
                                "Surface Inside Face Lights Radiation Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBalSurf->QdotRadLightsInRep(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);
            SetupOutputVariable(state,
                                "Surface Inside Face Lights Radiation Heat Gain Rate per Area",
                                OutputProcessor::Unit::W_m2,
                                state.dataHeatBalSurf->QdotRadLightsInRepPerArea(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);
            SetupOutputVariable(state,
                                "Surface Inside Face Lights Radiation Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBalSurf->QRadLightsInReport(loop),
                                "Zone",
                                "Sum",
                                Surface(loop).Name);
        }

        SetupOutputVariable(state,
                            "Surface Inside Face Internal Gains Radiation Heat Gain Rate",
                            OutputProcessor::Unit::W,
                            state.dataHeatBalSurf->QdotRadIntGainsInRep(loop),
                            "Zone",
                            "State",
                            Surface(loop).Name);
        SetupOutputVariable(state,
                            "Surface Inside Face Internal Gains Radiation Heat Gain Rate per Area",
                            OutputProcessor::Unit::W_m2,
                            state.dataHeatBalSurf->QdotRadIntGainsInRepPerArea(loop),
                            "Zone",
                            "State",
                            Surface(loop).Name);
        SetupOutputVariable(state,
                            "Surface Inside Face Internal Gains Radiation Heat Gain Energy",
                            OutputProcessor::Unit::J,
                            state.dataHeatBalSurf->QRadIntGainsInReport(loop),
                            "Zone",
                            "Sum",
                            Surface(loop).Name);

        SetupOutputVariable(state,
                            "Surface Inside Face System Radiation Heat Gain Rate",
                            OutputProcessor::Unit::W,
                            state.dataHeatBalSurf->QdotRadHVACInRep(loop),
                            "Zone",
                            "State",
                            Surface(loop).Name);
        SetupOutputVariable(state,
                            "Surface Inside Face System Radiation Heat Gain Rate per Area",
                            OutputProcessor::Unit::W_m2,
                            state.dataHeatBalSurf->QdotRadHVACInRepPerArea(loop),
                            "Zone",
                            "State",
                            Surface(loop).Name);
        SetupOutputVariable(state,
                            "Surface Inside Face System Radiation Heat Gain Energy",
                            OutputProcessor::Unit::J,
                            state.dataHeatBalSurf->QRadHVACInReport(loop),
                            "Zone",
                            "Sum",
                            Surface(loop).Name);

        if (Surface(loop).ExtBoundCond == ExternalEnvironment || state.dataGlobal->DisplayAdvancedReportVariables) {
            SetupOutputVariable(state,
                                "Surface Outside Face Outdoor Air Drybulb Temperature",
                                OutputProcessor::Unit::C,
                                state.dataSurface->SurfOutDryBulbTemp(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);
            SetupOutputVariable(state,
                                "Surface Outside Face Outdoor Air Wetbulb Temperature",
                                OutputProcessor::Unit::C,
                                state.dataSurface->SurfOutWetBulbTemp(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);
            SetupOutputVariable(state,
                                "Surface Outside Face Outdoor Air Wind Speed",
                                OutputProcessor::Unit::m_s,
                                state.dataSurface->SurfOutWindSpeed(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);
            SetupOutputVariable(state,
                                "Surface Outside Face Outdoor Air Wind Direction",
                                OutputProcessor::Unit::deg,
                                state.dataSurface->SurfOutWindDir(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);
            SetupOutputVariable(state,
                                "Surface Outside Face Convection Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBalSurf->QdotConvOutRep(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);
            SetupOutputVariable(state,
                                "Surface Outside Face Convection Heat Gain Rate per Area",
                                OutputProcessor::Unit::W_m2,
                                state.dataHeatBalSurf->QdotConvOutRepPerArea(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);
            SetupOutputVariable(state,
                                "Surface Outside Face Convection Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBalSurf->QConvOutReport(loop),
                                "Zone",
                                "Sum",
                                Surface(loop).Name);
            SetupOutputVariable(state,
                                "Surface Outside Face Convection Heat Transfer Coefficient",
                                OutputProcessor::Unit::W_m2K,
                                state.dataHeatBalSurf->HcExtSurf(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);
            SetupOutputVariable(state,
                                "Surface Outside Face Net Thermal Radiation Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBalSurf->QdotRadOutRep(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);
            SetupOutputVariable(state,
                                "Surface Outside Face Net Thermal Radiation Heat Gain Rate per Area",
                                OutputProcessor::Unit::W_m2,
                                state.dataHeatBalSurf->QdotRadOutRepPerArea(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);
            SetupOutputVariable(state,
                                "Surface Outside Face Net Thermal Radiation Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBalSurf->QRadOutReport(loop),
                                "Zone",
                                "Sum",
                                Surface(loop).Name);
            SetupOutputVariable(state,
                                "Surface Outside Face Thermal Radiation to Air Heat Transfer Coefficient",
                                OutputProcessor::Unit::W_m2K,
                                state.dataHeatBalSurf->HAirExtSurf(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);
            SetupOutputVariable(state,
                                "Surface Outside Face Thermal Radiation to Sky Heat Transfer Coefficient",
                                OutputProcessor::Unit::W_m2K,
                                state.dataHeatBalSurf->HSkyExtSurf(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);
            SetupOutputVariable(state,
                                "Surface Outside Face Thermal Radiation to Ground Heat Transfer Coefficient",
                                OutputProcessor::Unit::W_m2K,
                                state.dataHeatBalSurf->HGrdExtSurf(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);
            SetupOutputVariable(state,
                                "Surface Outside Face Thermal Radiation to Air Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBalSurf->QAirExtReport(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);
            SetupOutputVariable(state,
                                "Surface Outside Face Heat Emission to Air Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBalSurf->QHeatEmiReport(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);

            if (Surface(loop).Class != SurfaceClass::Window) {
                SetupOutputVariable(state,
                                    "Surface Outside Face Solar Radiation Heat Gain Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->SurfOpaqSWOutAbsTotalReport(loop),
                                    "Zone",
                                    "Average",
                                    Surface(loop).Name);
                SetupOutputVariable(state,
                                    "Surface Outside Face Solar Radiation Heat Gain Rate per Area",
                                    OutputProcessor::Unit::W_m2,
                                    state.dataHeatBalSurf->SurfOpaqQRadSWOutAbs(loop),
                                    "Zone",
                                    "Average",
                                    Surface(loop).Name);
                SetupOutputVariable(state,
                                    "Surface Outside Face Solar Radiation Heat Gain Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->SurfOpaqSWOutAbsEnergyReport(loop),
                                    "Zone",
                                    "Sum",
                                    Surface(loop).Name);
            }
        }
        if (Surface(loop).Class == SurfaceClass::Floor || Surface(loop).Class == SurfaceClass::Wall || Surface(loop).Class == SurfaceClass::IntMass ||
            Surface(loop).Class == SurfaceClass::Roof || Surface(loop).Class == SurfaceClass::Door) {
            //      IF (DisplayAdvancedReportVariables) THEN  !CurrentModuleObject='Opaque Surfaces(Advanced)'
            SetupOutputVariable(state,
                                "Surface Inside Face Conduction Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBalSurf->SurfOpaqInsFaceConduction(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);
            SetupOutputVariable(state,
                                "Surface Inside Face Conduction Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBalSurf->SurfOpaqInsFaceCondGainRep(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);
            SetupOutputVariable(state,
                                "Surface Inside Face Conduction Heat Loss Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBalSurf->SurfOpaqInsFaceCondLossRep(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);
            SetupOutputVariable(state,
                                "Surface Inside Face Conduction Heat Transfer Rate per Area",
                                OutputProcessor::Unit::W_m2,
                                state.dataHeatBalSurf->SurfOpaqInsFaceConductionFlux(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);
            SetupOutputVariable(state,
                                "Surface Inside Face Conduction Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBalSurf->SurfOpaqInsFaceConductionEnergy(loop),
                                "Zone",
                                "Sum",
                                Surface(loop).Name);

            if (Surface(loop).ExtBoundCond != KivaFoundation) {
                SetupOutputVariable(state,
                                    "Surface Outside Face Conduction Heat Transfer Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBalSurf->SurfOpaqOutsideFaceConduction(loop),
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
                SetupOutputVariable(state,
                                    "Surface Outside Face Conduction Heat Gain Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBalSurf->SurfOpaqExtFaceCondGainRep(loop),
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
                SetupOutputVariable(state,
                                    "Surface Outside Face Conduction Heat Loss Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBalSurf->SurfOpaqExtFaceCondLossRep(loop),
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
                SetupOutputVariable(state,
                                    "Surface Outside Face Conduction Heat Transfer Rate per Area",
                                    OutputProcessor::Unit::W_m2,
                                    state.dataHeatBalSurf->SurfOpaqOutsideFaceConductionFlux(loop),
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
                SetupOutputVariable(state,
                                    "Surface Outside Face Conduction Heat Transfer Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBalSurf->SurfOpaqOutsideFaceConductionEnergy(loop),
                                    "Zone",
                                    "Sum",
                                    Surface(loop).Name);

                SetupOutputVariable(state,
                                    "Surface Average Face Conduction Heat Transfer Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBalSurf->SurfOpaqAvgFaceConduction(loop),
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
                SetupOutputVariable(state,
                                    "Surface Average Face Conduction Heat Gain Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBalSurf->SurfOpaqAvgFaceCondGainRep(loop),
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
                SetupOutputVariable(state,
                                    "Surface Average Face Conduction Heat Loss Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBalSurf->SurfOpaqAvgFaceCondLossRep(loop),
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
                SetupOutputVariable(state,
                                    "Surface Average Face Conduction Heat Transfer Rate per Area",
                                    OutputProcessor::Unit::W_m2,
                                    state.dataHeatBalSurf->SurfOpaqAvgFaceConductionFlux(loop),
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
                SetupOutputVariable(state,
                                    "Surface Average Face Conduction Heat Transfer Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBalSurf->SurfOpaqAvgFaceConductionEnergy(loop),
                                    "Zone",
                                    "Sum",
                                    Surface(loop).Name);

                SetupOutputVariable(state,
                                    "Surface Heat Storage Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBalSurf->SurfOpaqStorageConduction(loop),
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
                SetupOutputVariable(state,
                                    "Surface Heat Storage Gain Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBalSurf->SurfOpaqStorageGainRep(loop),
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
                SetupOutputVariable(state,
                                    "Surface Heat Storage Loss Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBalSurf->SurfOpaqStorageCondLossRep(loop),
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
                SetupOutputVariable(state,
                                    "Surface Heat Storage Rate per Area",
                                    OutputProcessor::Unit::W_m2,
                                    state.dataHeatBalSurf->SurfOpaqStorageConductionFlux(loop),
                                    "Zone",
                                    "State",
                                    Surface(loop).Name);
                SetupOutputVariable(state,
                                    "Surface Heat Storage Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBalSurf->SurfOpaqStorageConductionEnergy(loop),
                                    "Zone",
                                    "Sum",
                                    Surface(loop).Name);
            }

            //      ENDIF
            // CurrentModuleObject='Opaque Surfaces'

            SetupOutputVariable(state,
                                "Surface Inside Face Beam Solar Radiation Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBalSurf->SurfOpaqInsFaceBeamSolAbsorbed(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);
        }
        if (state.dataConstruction->Construct(Surface(loop).Construction).SourceSinkPresent) {
            SetupOutputVariable(state,
                                "Surface Internal Source Location Temperature",
                                OutputProcessor::Unit::C,
                                state.dataHeatBalSurf->TempSource(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);
            SetupOutputVariable(state,
                                "Surface Internal User Specified Location Temperature",
                                OutputProcessor::Unit::C,
                                state.dataHeatBalSurf->TempUserLoc(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);
        }

        if (Surface(loop).Class == SurfaceClass::Window) { // CurrentModuleObject='Windows'
            SetupOutputVariable(state,
                                "Surface Shading Device Is On Time Fraction",
                                OutputProcessor::Unit::None,
                                state.dataSurface->SurfWinFracTimeShadingDeviceOn(loop),
                                "Zone",
                                "Average",
                                Surface(loop).Name);
            SetupOutputVariable(state,
                                "Surface Storm Window On Off Status",
                                OutputProcessor::Unit::None,
                                state.dataSurface->SurfWinStormWinFlag(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);
            SetupOutputVariable(state,
                                "Surface Window Blind Slat Angle",
                                OutputProcessor::Unit::deg,
                                state.dataSurface->SurfWinSlatAngThisTSDeg(loop),
                                "Zone",
                                "State",
                                Surface(loop).Name);
        }
        //    IF (DisplayAdvancedReportVariables) THEN  !CurrentModuleObject='Opaque Surfaces(Advanced)'
        SetupOutputVariable(state,
                            "Surface Inside Face Convection Classification Index",
                            OutputProcessor::Unit::None,
                            state.dataSurface->SurfIntConvClassification(loop),
                            "Zone",
                            "Average",
                            Surface(loop).Name);
        SetupOutputVariable(state,
                            "Surface Inside Face Convection Model Equation Index",
                            OutputProcessor::Unit::None,
                            state.dataSurface->SurfIntConvHcModelEq(loop),
                            "Zone",
                            "Average",
                            Surface(loop).Name);
        SetupOutputVariable(state,
                            "Surface Inside Face Convection Reference Air Index",
                            OutputProcessor::Unit::None,
                            state.dataSurface->SurfTAirRef(loop),
                            "Zone",
                            "Average",
                            Surface(loop).Name);
        if (Surface(loop).ExtBoundCond == ExternalEnvironment) {
            SetupOutputVariable(state,
                                "Surface Outside Face Convection Classification Index",
                                OutputProcessor::Unit::None,
                                state.dataSurface->SurfOutConvClassification(loop),
                                "Zone",
                                "Average",
                                Surface(loop).Name);
            SetupOutputVariable(state,
                                "Surface Outside Face Forced Convection Model Equation Index",
                                OutputProcessor::Unit::None,
                                state.dataSurface->SurfOutConvHfModelEq(loop),
                                "Zone",
                                "Average",
                                Surface(loop).Name);
            SetupOutputVariable(state,
                                "Surface Outside Face Natural Convection Model Equation Index",
                                OutputProcessor::Unit::None,
                                state.dataSurface->SurfOutConvHnModelEq(loop),
                                "Zone",
                                "Average",
                                Surface(loop).Name);
        }

        SetupOutputVariable(state,
                            "Surface Inside Face Heat Source Gain Rate per Area",
                            OutputProcessor::Unit::W_m2,
                            state.dataHeatBalSurf->SurfQAdditionalHeatSourceInside(loop),
                            "Zone",
                            "Average",
                            Surface(loop).Name);
        SetupOutputVariable(state,
                            "Surface Outside Face Heat Source Gain Rate per Area",
                            OutputProcessor::Unit::W_m2,
                            state.dataHeatBalSurf->SurfQAdditionalHeatSourceOutside(loop),
                            "Zone",
                            "Average",
                            Surface(loop).Name);

        //     ENDIF
        if (state.dataGlobal->DisplayAdvancedReportVariables) {
            SetupOutputVariable(
                state, "Surface Construction Index", OutputProcessor::Unit::None, Surface(loop).Construction, "Zone", "Average", Surface(loop).Name);
        }
    }

    SetupOutputVariable(state,
                        "Site Total Surface Heat Emission to Air",
                        OutputProcessor::Unit::J,
                        state.dataHeatBalSurf->SumSurfaceHeatEmission,
                        "Zone",
                        "Sum",
                        "Environment");
}

void InitThermalAndFluxHistories(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         George Walton
    //       DATE WRITTEN   March 1978
    //       MODIFIED       na
    //       RE-ENGINEERED  Feb98 (RKS)

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine sets the initial temperature and flux histories
    // needed for a stable and reasonable heat balance solution starting
    // point.

    // METHODOLOGY EMPLOYED:
    // This subroutine assumes that the simulation is at steady state at
    // the beginning and then begins to vary.  Thus, the temperatures, the
    // fluxes. and their histories can all be set to the same value.  Some
    // of the initializations depend on the surface characteristics.  This
    // requires a DO loop to perform the proper calculation.

    // REFERENCES:
    // (I)BLAST legacy routine INITTH

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int SurfNum; // DO loop counter for surfaces
    int OSCMnum; // DO loop counter for Other side conditions modeled (OSCM)

    auto &Surface(state.dataSurface->Surface);

    // First do the "bulk" initializations of arrays sized to NumOfZones
    for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
        state.dataHeatBal->ZoneMRT(zoneNum) = ZoneInitialTemp;   // module level array
        state.dataHeatBalFanSys->MAT(zoneNum) = ZoneInitialTemp; // DataHeatBalFanSys array
        state.dataHeatBalFanSys->ZT(zoneNum) = ZoneInitialTemp;
        state.dataHeatBalFanSys->ZTAV(zoneNum) = ZoneInitialTemp;
        state.dataHeatBalFanSys->XMAT(zoneNum) = ZoneInitialTemp; // DataHeatBalFanSys array
        state.dataHeatBalFanSys->XM2T(zoneNum) = ZoneInitialTemp; // DataHeatBalFanSys array
        state.dataHeatBalFanSys->XM3T(zoneNum) = ZoneInitialTemp; // DataHeatBalFanSys array
        state.dataHeatBalFanSys->XM4T(zoneNum) = ZoneInitialTemp;
        state.dataHeatBalFanSys->XMPT(zoneNum) = ZoneInitialTemp;
        state.dataHeatBalFanSys->DSXMAT(zoneNum) = ZoneInitialTemp; // DataHeatBalFanSys array
        state.dataHeatBalFanSys->DSXM2T(zoneNum) = ZoneInitialTemp; // DataHeatBalFanSys array
        state.dataHeatBalFanSys->DSXM3T(zoneNum) = ZoneInitialTemp; // DataHeatBalFanSys array
        state.dataHeatBalFanSys->DSXM4T(zoneNum) = ZoneInitialTemp;
        state.dataHeatBalFanSys->ZoneTMX(zoneNum) = ZoneInitialTemp; // DataHeatBalFanSys array
        state.dataHeatBalFanSys->ZoneTM2(zoneNum) = ZoneInitialTemp; // DataHeatBalFanSys array
        // Initialize the Zone Humidity Ratio here so that it is available for EMPD implementations
        state.dataHeatBalFanSys->ZoneAirHumRatAvg(zoneNum) = state.dataEnvrn->OutHumRat;
        state.dataHeatBalFanSys->ZoneAirHumRat(zoneNum) = state.dataEnvrn->OutHumRat;
        state.dataHeatBalFanSys->ZoneAirHumRatOld(zoneNum) = state.dataEnvrn->OutHumRat;
        state.dataHeatBalFanSys->SumHmAW(zoneNum) = 0.0;
        state.dataHeatBalFanSys->SumHmARa(zoneNum) = 0.0;
        state.dataHeatBalFanSys->SumHmARaW(zoneNum) = 0.0;
        state.dataHeatBalFanSys->TempTstatAir(zoneNum) = ZoneInitialTemp;
    }

    // "Bulk" initializations of arrays sized to TotSurfaces
    for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
        int const firstSurf = state.dataHeatBal->Zone(zoneNum).HTSurfaceFirst;
        int const lastSurf = state.dataHeatBal->Zone(zoneNum).HTSurfaceLast;
        for (int SurfNum = firstSurf; SurfNum <= lastSurf; ++SurfNum) {
            state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = SurfInitialTemp;
            state.dataHeatBalSurf->SUMH(SurfNum) = 0;                        // module level array
            state.dataHeatBalSurf->TempSurfIn(SurfNum) = SurfInitialTemp;    // module level array
            state.dataHeatBalSurf->TempSurfInTmp(SurfNum) = SurfInitialTemp; // module level array
            state.dataHeatBal->HConvIn(SurfNum) = SurfInitialConvCoeff;      // module level array
            state.dataHeatBalSurf->HcExtSurf(SurfNum) = 0.0;
            state.dataHeatBalSurf->HAirExtSurf(SurfNum) = 0.0;
            state.dataHeatBalSurf->HSkyExtSurf(SurfNum) = 0.0;
            state.dataHeatBalSurf->HGrdExtSurf(SurfNum) = 0.0;
            state.dataHeatBalSurf->SurfTempOut(SurfNum) = 0.0;
            state.dataHeatBalSurf->TempSurfInRep(SurfNum) = 0.0;
            state.dataHeatBalSurf->TempSurfInMovInsRep(SurfNum) = 0.0;
            state.dataHeatBalSurf->QConvInReport(SurfNum) = 0.0;
            state.dataHeatBalSurf->QdotConvInRep(SurfNum) = 0.0;
            state.dataHeatBalSurf->QdotConvInRepPerArea(SurfNum) = 0.0;
            state.dataHeatBalSurf->QRadNetSurfInReport(SurfNum) = 0.0;
            state.dataHeatBalSurf->QdotRadNetSurfInRep(SurfNum) = 0.0;
            state.dataHeatBalSurf->QdotRadNetSurfInRepPerArea(SurfNum) = 0.0;
            state.dataHeatBalSurf->QRadSolarInReport(SurfNum) = 0.0;
            state.dataHeatBalSurf->QdotRadSolarInRep(SurfNum) = 0.0;
            state.dataHeatBalSurf->QdotRadSolarInRepPerArea(SurfNum) = 0.0;
            state.dataHeatBalSurf->QRadLightsInReport(SurfNum) = 0.0;
            state.dataHeatBalSurf->QdotRadLightsInRep(SurfNum) = 0.0;
            state.dataHeatBalSurf->QdotRadLightsInRepPerArea(SurfNum) = 0.0;
            state.dataHeatBalSurf->QRadIntGainsInReport(SurfNum) = 0.0;
            state.dataHeatBalSurf->QdotRadIntGainsInRep(SurfNum) = 0.0;
            state.dataHeatBalSurf->QdotRadIntGainsInRepPerArea(SurfNum) = 0.0;
            state.dataHeatBalSurf->QRadHVACInReport(SurfNum) = 0.0;
            state.dataHeatBalSurf->QdotRadHVACInRep(SurfNum) = 0.0;
            state.dataHeatBalSurf->QdotRadHVACInRepPerArea(SurfNum) = 0.0;
            state.dataHeatBalSurf->QConvOutReport(SurfNum) = 0.0;
            state.dataHeatBalSurf->QdotConvOutRep(SurfNum) = 0.0;
            state.dataHeatBalSurf->QdotConvOutRepPerArea(SurfNum) = 0.0;
            state.dataHeatBalSurf->QRadOutReport(SurfNum) = 0.0;
            state.dataHeatBalSurf->QdotRadOutRep(SurfNum) = 0.0;
            state.dataHeatBalSurf->QdotRadOutRepPerArea(SurfNum) = 0.0;
            state.dataHeatBalSurf->QAirExtReport(SurfNum) = 0.0;
            state.dataHeatBalSurf->QHeatEmiReport(SurfNum) = 0.0;
        } // end of  Surf array
        int const firstSurfOpaq = state.dataHeatBal->Zone(zoneNum).OpaqOrIntMassSurfaceFirst;
        int const lastSurfOpaq = state.dataHeatBal->Zone(zoneNum).OpaqOrIntMassSurfaceLast;
        if (firstSurfOpaq >= 0) {
            for (int SurfNum = firstSurfOpaq; SurfNum <= lastSurfOpaq; ++SurfNum) {
                state.dataHeatBalSurf->SurfOpaqInsFaceConduction(SurfNum) = 0.0;
                state.dataHeatBalSurf->SurfOpaqInsFaceConductionFlux(SurfNum) = 0.0;
                state.dataHeatBalSurf->SurfOpaqInsFaceConductionEnergy(SurfNum) = 0.0;
                state.dataHeatBalSurf->SurfOpaqInsFaceBeamSolAbsorbed(SurfNum) = 0.0;
            } // end of Zone Surf
        }
        int const firstSurfWin = state.dataHeatBal->Zone(zoneNum).WindowSurfaceFirst;
        int const lastSurfWin = state.dataHeatBal->Zone(zoneNum).WindowSurfaceLast;
        if (firstSurfWin >= 0) {
            for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
                // Initialize window frame and divider temperatures
                state.dataSurface->SurfWinFrameTempSurfIn(SurfNum) = SurfInitialTemp;
                state.dataSurface->SurfWinFrameTempSurfInOld(SurfNum) = SurfInitialTemp;
                state.dataSurface->SurfWinFrameTempSurfOut(SurfNum) = SurfInitialTemp;
                state.dataSurface->SurfWinDividerTempSurfIn(SurfNum) = SurfInitialTemp;
                state.dataSurface->SurfWinDividerTempSurfInOld(SurfNum) = SurfInitialTemp;
                state.dataSurface->SurfWinDividerTempSurfOut(SurfNum) = SurfInitialTemp;

                // Initialize previous-timestep shading indicators
                state.dataSurface->SurfWinExtIntShadePrevTS(SurfNum) = WinShadingType::NoShade;
                state.dataSurface->SurfWinShadingFlag(SurfNum) = WinShadingType::NoShade;
            } // end of Zone Surf
        }
    } // end of Zone

    // "Bulk" initializations of temperature arrays with dimensions (TotSurface,MaxCTFTerms,2)
    for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
        int const firstSurf = state.dataHeatBal->Zone(zoneNum).HTSurfaceFirst;
        int const lastSurf = state.dataHeatBal->Zone(zoneNum).HTSurfaceLast;
        for (int side = 1; side <= 2; ++side) {
            for (int CTFTermNum = 1; CTFTermNum <= Construction::MaxCTFTerms; ++CTFTermNum) {
                for (int SurfNum = firstSurf; SurfNum <= lastSurf; ++SurfNum) {
                    state.dataHeatBalSurf->TH(side, CTFTermNum, SurfNum) = SurfInitialTemp;  // module level array
                    state.dataHeatBalSurf->THM(side, CTFTermNum, SurfNum) = SurfInitialTemp; // module level array
                    state.dataHeatBalSurf->QH(side, CTFTermNum, SurfNum) = 0.0;
                    state.dataHeatBalSurf->QHM(side, CTFTermNum, SurfNum) = 0.0;
                }
            }
        }
    }
    if (state.dataHeatBal->AnyInternalHeatSourceInInput) {
        for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
            int const firstSurf = state.dataHeatBal->Zone(zoneNum).HTSurfaceFirst;
            int const lastSurf = state.dataHeatBal->Zone(zoneNum).HTSurfaceLast;
            for (int SurfNum = firstSurf; SurfNum <= lastSurf; ++SurfNum) {
                for (int CTFTermNum = 1; CTFTermNum <= Construction::MaxCTFTerms; ++CTFTermNum) {
                    state.dataHeatBalSurf->TsrcHist(SurfNum, CTFTermNum) = SurfInitialTemp;
                    state.dataHeatBalSurf->TsrcHistM(SurfNum, CTFTermNum) = SurfInitialTemp;
                    state.dataHeatBalSurf->TuserHist(SurfNum, CTFTermNum) = SurfInitialTemp;
                    state.dataHeatBalSurf->TuserHistM(SurfNum, CTFTermNum) = SurfInitialTemp;
                    state.dataHeatBalSurf->QsrcHist(SurfNum, CTFTermNum) = 0.0;
                    state.dataHeatBalSurf->QsrcHistM(SurfNum, CTFTermNum) = 0.0;
                }
            }
        }
    }
    state.dataHeatBal->CondFDRelaxFactor = state.dataHeatBal->CondFDRelaxFactorInput;

    // Perform other initializations that depend on the surface characteristics
    for (SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {

        if (!Surface(SurfNum).HeatTransSurf) continue; // Skip non-heat transfer surfaces

        // Reset outside boundary conditions if necessary
        if ((Surface(SurfNum).ExtBoundCond == ExternalEnvironment) || (Surface(SurfNum).ExtBoundCond == OtherSideCondModeledExt)) {

            state.dataHeatBalSurf->THM(1, {1, state.dataConstruction->Construct(Surface(SurfNum).Construction).NumCTFTerms + 1}, SurfNum) =
                state.dataSurface->SurfOutDryBulbTemp(SurfNum);
            state.dataHeatBalSurf->TH(1, {1, state.dataConstruction->Construct(Surface(SurfNum).Construction).NumCTFTerms + 1}, SurfNum) =
                state.dataSurface->SurfOutDryBulbTemp(SurfNum);

        } else if (Surface(SurfNum).ExtBoundCond == Ground) {

            state.dataHeatBalSurf->THM(1, {1, state.dataConstruction->Construct(Surface(SurfNum).Construction).NumCTFTerms + 1}, SurfNum) =
                state.dataEnvrn->GroundTemp;
            state.dataHeatBalSurf->TH(1, {1, state.dataConstruction->Construct(Surface(SurfNum).Construction).NumCTFTerms + 1}, SurfNum) =
                state.dataEnvrn->GroundTemp;

        } else if (Surface(SurfNum).ExtBoundCond == GroundFCfactorMethod) {

            state.dataHeatBalSurf->THM(1, {1, state.dataConstruction->Construct(Surface(SurfNum).Construction).NumCTFTerms + 1}, SurfNum) =
                state.dataEnvrn->GroundTempFC;
            state.dataHeatBalSurf->TH(1, {1, state.dataConstruction->Construct(Surface(SurfNum).Construction).NumCTFTerms + 1}, SurfNum) =
                state.dataEnvrn->GroundTempFC;
        }

        if (state.dataSurface->SurfExtCavityPresent(SurfNum)) {
            state.dataSurface->ExtVentedCavity(state.dataSurface->SurfExtCavNum(SurfNum)).TbaffleLast = 20.0;
            state.dataSurface->ExtVentedCavity(state.dataSurface->SurfExtCavNum(SurfNum)).TairLast = 20.0;
        }

        // Initialize Kiva convection algorithms
        if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
            state.dataSurfaceGeometry->kivaManager.surfaceConvMap[SurfNum].in = KIVA_CONST_CONV(3.076);
            state.dataSurfaceGeometry->kivaManager.surfaceConvMap[SurfNum].f = KIVA_HF_DEF;
            state.dataSurfaceGeometry->kivaManager.surfaceConvMap[SurfNum].out = KIVA_CONST_CONV(0.0);
        }

        // Initialize the flux histories
        state.dataHeatBalSurf->QH(1, {2, state.dataConstruction->Construct(Surface(SurfNum).Construction).NumCTFTerms + 1}, SurfNum) =
            state.dataConstruction->Construct(Surface(SurfNum).Construction).UValue *
            (state.dataHeatBalSurf->TH(1, 1, SurfNum) - state.dataHeatBalSurf->TH(2, 1, SurfNum));
        state.dataHeatBalSurf->QH(2, {2, state.dataConstruction->Construct(Surface(SurfNum).Construction).NumCTFTerms + 1}, SurfNum) =
            state.dataHeatBalSurf->QH(1, 2, SurfNum);
        state.dataHeatBalSurf->QHM(1, {2, state.dataConstruction->Construct(Surface(SurfNum).Construction).NumCTFTerms + 1}, SurfNum) =
            state.dataHeatBalSurf->QH(1, 2, SurfNum);
        state.dataHeatBalSurf->QHM(2, {2, state.dataConstruction->Construct(Surface(SurfNum).Construction).NumCTFTerms + 1}, SurfNum) =
            state.dataHeatBalSurf->QH(1, 2, SurfNum);
    }

    if (state.dataSurface->TotOSCM >= 1) {
        for (OSCMnum = 1; OSCMnum <= state.dataSurface->TotOSCM; ++OSCMnum) {
            state.dataSurface->OSCM(OSCMnum).TConv = 20.0;
            state.dataSurface->OSCM(OSCMnum).HConv = 4.0;
            state.dataSurface->OSCM(OSCMnum).TRad = 20.0;
            state.dataSurface->OSCM(OSCMnum).HRad = 4.0;
        }
    }
}

void EvalOutsideMovableInsulation(EnergyPlusData &state)
{
    // This subroutine determines whether or not outside movable insulation on opaque surfaces is present at the current time.
    for (int SurfNum : state.dataHeatBalSurf->SurfMovInsulIndexList) {
        Real64 MovInsulSchedVal = GetCurrentScheduleValue(state, state.dataSurface->SurfSchedMovInsulExt(SurfNum));
        if (MovInsulSchedVal <= 0) { // Movable insulation not present at current time
            state.dataHeatBalSurf->SurfMovInsulExtPresent(SurfNum) = false;
            int ConstrNum = state.dataSurface->SurfActiveConstruction(SurfNum);
            state.dataHeatBalSurf->SurfAbsSolarExt(SurfNum) =
                state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).AbsorpSolar;
            state.dataHeatBalSurf->SurfAbsThermalExt(SurfNum) =
                state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).AbsorpThermal;
            state.dataHeatBalSurf->SurfRoughnessExt(SurfNum) =
                state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).Roughness;
            continue;
        }
        int const MaterialIndex(state.dataSurface->SurfMaterialMovInsulExt(SurfNum));
        int const MaterialGroupNum(state.dataMaterial->Material(MaterialIndex).Group);
        state.dataHeatBalSurf->SurfMovInsulExtPresent(SurfNum) = true;
        state.dataHeatBalSurf->SurfMovInsulHExt(SurfNum) = 1.0 / (MovInsulSchedVal * state.dataMaterial->Material(MaterialIndex).Resistance);
        if (MaterialGroupNum == DataHeatBalance::WindowGlass || MaterialGroupNum == DataHeatBalance::GlassEquivalentLayer) {
            state.dataHeatBalSurf->SurfAbsSolarExt(SurfNum) =
                max(0.0, 1.0 - state.dataMaterial->Material(MaterialIndex).Trans - state.dataMaterial->Material(MaterialIndex).ReflectSolBeamFront);
        } else {
            state.dataHeatBalSurf->SurfAbsSolarExt(SurfNum) = state.dataMaterial->Material(MaterialIndex).AbsorpSolar;
        }
        state.dataHeatBalSurf->SurfAbsThermalExt(SurfNum) = state.dataMaterial->Material(MaterialIndex).AbsorpThermal;
        state.dataHeatBalSurf->SurfRoughnessExt(SurfNum) = state.dataMaterial->Material(MaterialIndex).Roughness;
    }
}

void EvalInsideMovableInsulation(EnergyPlusData &state)
{
    // This subroutine determines whether or not inside movable insulation is present at the current time.
    for (int SurfNum : state.dataHeatBalSurf->SurfMovInsulIndexList) {
        Real64 MovInsulSchedVal = GetCurrentScheduleValue(state, state.dataSurface->SurfSchedMovInsulInt(SurfNum));
        if (MovInsulSchedVal <= 0.0) { // Movable insulation not present at current time
            state.dataHeatBalSurf->SurfMovInsulIntPresent(SurfNum) = false;
            int ConstrNum = state.dataSurface->SurfActiveConstruction(SurfNum);
            state.dataHeatBalSurf->SurfAbsSolarInt(SurfNum) = state.dataConstruction->Construct(ConstrNum).InsideAbsorpSolar;
            state.dataHeatBalSurf->SurfAbsThermalInt(SurfNum) = state.dataConstruction->Construct(ConstrNum).InsideAbsorpThermal;
            continue;
        }
        int const MaterialIndex(state.dataSurface->SurfMaterialMovInsulInt(SurfNum));
        int const MaterialGroupNum(state.dataMaterial->Material(MaterialIndex).Group);
        state.dataHeatBalSurf->SurfMovInsulIntPresent(SurfNum) = true;
        state.dataHeatBalSurf->SurfMovInsulHInt(SurfNum) = 1.0 / (MovInsulSchedVal * state.dataMaterial->Material(MaterialIndex).Resistance);
        if (MaterialGroupNum == DataHeatBalance::WindowGlass || MaterialGroupNum == DataHeatBalance::GlassEquivalentLayer) {
            state.dataHeatBalSurf->SurfAbsSolarInt(SurfNum) =
                max(0.0, 1.0 - state.dataMaterial->Material(MaterialIndex).Trans - state.dataMaterial->Material(MaterialIndex).ReflectSolBeamFront);
        } else {
            state.dataHeatBalSurf->SurfAbsSolarInt(SurfNum) = state.dataMaterial->Material(MaterialIndex).AbsorpSolar;
        }
        state.dataHeatBalSurf->SurfAbsThermalInt(SurfNum) = state.dataMaterial->Material(MaterialIndex).AbsorpThermal;
    }
}
void InitSolarHeatGains(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Anonymous
    //       DATE WRITTEN   July 1977
    //       MODIFIED       Mar99 (FW): handle movable interior shades and
    //                                  switchable glazing
    //                      Oct99 (FW): account for Window5 glass calculation approach
    //                      May01 (FW): handle interior and exterior blinds
    //                      Sep03 (FW): initialize SurfaceWindow%FrameQRadOutAbs
    //                      May06 (RR): handle exterior window screens
    //       RE-ENGINEERED  Feb98 (RKS)

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine initializes the arrays associated with solar heat
    // gains for both individual surfaces and for zones.  As a result,
    // this routine sets the following variable arrays:
    // QBV(unused), QDV, QC, QD; QRadSWOutAbs and QRadSWInAbs (for opaque surfaces);
    // QRadSWwinAbs (for windows)

    // METHODOLOGY EMPLOYED:
    // If the sun is down, all of the pertinent arrays are zeroed.  If the
    // sun is up, various calculations are made.

    // REFERENCES:
    // (I)BLAST legacy routine QSUN

    auto &Surface(state.dataSurface->Surface);

    // Using/Aliasing
    using DaylightingDevices::TransTDD;
    using General::InterpSw;
    using General::POLYF;
    using SolarShading::CalcInteriorSolarDistribution;
    using namespace DataWindowEquivalentLayer;
    using SolarShading::SurfaceScheduledSolarInc;
    using SolarShading::WindowScheduledSolarAbs;

    auto &AbsDiffWin = state.dataHeatBalSurfMgr->AbsDiffWin;
    auto &AbsDiffWinGnd = state.dataHeatBalSurfMgr->AbsDiffWinGnd;
    auto &AbsDiffWinSky = state.dataHeatBalSurfMgr->AbsDiffWinSky;

    Array1D<Real64> currCosInc(state.dataSurface->TotSurfaces);      // Cosine of incidence angle of beam solar on glass
    Array1D<Real64> currBeamSolar(state.dataSurface->TotSurfaces);   // Local variable for BeamSolarRad
    Array1D<Real64> currSkySolarInc(state.dataSurface->TotSurfaces); // Sky diffuse solar incident on a surface
    Array1D<Real64> currGndSolarInc(state.dataSurface->TotSurfaces); // Ground diffuse solar incident on a surface

    for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
        state.dataHeatBal->ZoneInitialDifSolReflW(zoneNum) = 0.0;
        state.dataHeatBal->ZoneWinHeatGainRepEnergy(zoneNum) = 0.0;
        state.dataHeatBal->ZoneWinHeatLossRepEnergy(zoneNum) = 0.0;
        state.dataHeatBal->ZnOpqSurfInsFaceCondGnRepEnrg(zoneNum) = 0.0;
        state.dataHeatBal->ZnOpqSurfInsFaceCondLsRepEnrg(zoneNum) = 0.0;
        state.dataHeatBal->ZnOpqSurfExtFaceCondGnRepEnrg(zoneNum) = 0.0;
        state.dataHeatBal->ZnOpqSurfExtFaceCondLsRepEnrg(zoneNum) = 0.0;

        state.dataHeatBal->ZoneWinHeatGain(zoneNum) = 0.0;
        state.dataHeatBal->ZoneWinHeatGainRep(zoneNum) = 0.0;
        state.dataHeatBal->ZoneWinHeatLossRep(zoneNum) = 0.0;
        state.dataHeatBal->ZoneOpaqSurfInsFaceCond(zoneNum) = 0.0;
        state.dataHeatBal->ZoneOpaqSurfInsFaceCondGainRep(zoneNum) = 0.0;
        state.dataHeatBal->ZoneOpaqSurfInsFaceCondLossRep(zoneNum) = 0.0;
        state.dataHeatBal->ZoneOpaqSurfExtFaceCond(zoneNum) = 0.0;
        state.dataHeatBal->ZoneOpaqSurfExtFaceCondGainRep(zoneNum) = 0.0;
        state.dataHeatBal->ZoneOpaqSurfExtFaceCondLossRep(zoneNum) = 0.0;
    }
    for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
        int const firstSurfOpaq = state.dataHeatBal->Zone(zoneNum).OpaqOrIntMassSurfaceFirst;
        int const lastSurfOpaq = state.dataHeatBal->Zone(zoneNum).OpaqOrIntMassSurfaceLast;
        for (int SurfNum = firstSurfOpaq; SurfNum <= lastSurfOpaq; ++SurfNum) {
            state.dataHeatBalSurf->SurfOpaqInsFaceCondGainRep(SurfNum) = 0.0;
            state.dataHeatBalSurf->SurfOpaqInsFaceCondLossRep(SurfNum) = 0.0;
            state.dataHeatBalSurf->SurfOpaqQRadSWInAbs(SurfNum) = 0.0;
            state.dataHeatBalSurf->SurfOpaqQRadSWLightsInAbs(SurfNum) = 0.0;
            state.dataHeatBalSurf->SurfOpaqQRadSWOutAbs(SurfNum) = 0.0;
            state.dataHeatBalSurf->SurfOpaqInitialDifSolInAbs(SurfNum) = 0.0;
            state.dataHeatBalSurf->SurfOpaqInsFaceBeamSolAbsorbed(SurfNum) = 0.0;
            state.dataHeatBal->SurfOpaqSWOutAbsTotalReport(SurfNum) = 0.0;
            state.dataHeatBal->SurfOpaqSWOutAbsEnergyReport(SurfNum) = 0.0;
        }

        int const firstSurfWin = state.dataHeatBal->Zone(zoneNum).WindowSurfaceFirst;
        int const lastSurfWin = state.dataHeatBal->Zone(zoneNum).WindowSurfaceLast;
        for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
            // Faster "inline" than calling SurfaceWindow( SurfNum ).InitSolarHeatGains()
            state.dataSurface->SurfWinFrameQRadOutAbs(SurfNum) = 0.0;
            state.dataSurface->SurfWinFrameQRadInAbs(SurfNum) = 0.0;
            state.dataSurface->SurfWinDividerQRadOutAbs(SurfNum) = 0.0;
            state.dataSurface->SurfWinDividerQRadInAbs(SurfNum) = 0.0;
            state.dataSurface->SurfWinIntSWAbsByShade(SurfNum) = 0.0;
            state.dataSurface->SurfWinIntLWAbsByShade(SurfNum) = 0.0;
            state.dataSurface->SurfWinConvHeatFlowNatural(SurfNum) = 0.0;
            state.dataSurface->SurfWinConvHeatGainToZoneAir(SurfNum) = 0.0;
            state.dataSurface->SurfWinRetHeatGainToZoneAir(SurfNum) = 0.0;
            state.dataSurface->SurfWinDividerHeatGain(SurfNum) = 0.0;
        }

        for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
            state.dataSurface->SurfWinGainConvGlazToZoneRep(SurfNum) = 0.0;
            state.dataSurface->SurfWinGainIRGlazToZoneRep(SurfNum) = 0.0;
            state.dataSurface->SurfWinLossSWZoneToOutWinRep(SurfNum) = 0.0;
            state.dataSurface->SurfWinGainFrameDividerToZoneRep(SurfNum) = 0.0;
            state.dataSurface->SurfWinGainConvGlazShadGapToZoneRep(SurfNum) = 0.0;
            state.dataSurface->SurfWinGainConvShadeToZoneRep(SurfNum) = 0.0;
            state.dataSurface->SurfWinGainIRShadeToZoneRep(SurfNum) = 0.0;
            state.dataSurface->SurfWinGapConvHtFlowRep(SurfNum) = 0.0;
            state.dataSurface->SurfWinShadingAbsorbedSolar(SurfNum) = 0.0;
            state.dataSurface->SurfWinSysSolTransmittance(SurfNum) = 0.0;
        }
        for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
            state.dataSurface->SurfWinHeatGain(SurfNum) = 0.0;
            state.dataSurface->SurfWinHeatTransfer(SurfNum) = 0.0;
            state.dataSurface->SurfWinHeatGainRep(SurfNum) = 0.0;
            state.dataSurface->SurfWinHeatLossRep(SurfNum) = 0.0;
            state.dataSurface->SurfWinHeatGainRepEnergy(SurfNum) = 0.0;
            state.dataSurface->SurfWinHeatLossRepEnergy(SurfNum) = 0.0;
            state.dataSurface->SurfWinGapConvHtFlowRepEnergy(SurfNum) = 0.0;
            state.dataSurface->SurfWinHeatTransferRepEnergy(SurfNum) = 0.0;
            state.dataSurface->SurfWinShadingAbsorbedSolarEnergy(SurfNum) = 0.0;
            state.dataSurface->SurfWinOtherConvGainInsideFaceToZoneRep(SurfNum) = 0.0;
        }
        for (int Lay = 1; Lay <= DataWindowEquivalentLayer::CFSMAXNL + 1; Lay++) {
            for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
                state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum, Lay) = 0.0;
            }
        }
    }
    if (state.dataHeatBalSurfMgr->InitSurfaceHeatBalancefirstTime) {
        for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
            state.dataSurface->SurfBmToDiffReflFacGnd(SurfNum) = Surface(SurfNum).ViewFactorGround;
            state.dataSurface->SurfSkyDiffReflFacGnd(SurfNum) = Surface(SurfNum).ViewFactorGround;
        }
    }
    bool currSolRadPositive =
        state.dataEnvrn->SunIsUp && (state.dataEnvrn->BeamSolarRad + state.dataEnvrn->GndSolarRad + state.dataEnvrn->DifSolarRad > 0.0);
    bool sunset = (!currSolRadPositive) && state.dataEnvrn->PreviousSolRadPositive;
    bool sunIsUpNoRad = state.dataEnvrn->SunIsUp && (!currSolRadPositive);
    bool resetSolar = state.dataGlobal->BeginEnvrnFlag || sunIsUpNoRad ||
                      sunset; // Reset at (1) Beginning of simulation (2) sunset time, and SunIsUp but not solar time.
    state.dataEnvrn->PreviousSolRadPositive = currSolRadPositive;

    if (currSolRadPositive || resetSolar) {
        for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
            state.dataHeatBal->SurfBmIncInsSurfIntensRep(SurfNum) = 0.0;
            state.dataHeatBal->SurfBmIncInsSurfAmountRep(SurfNum) = 0.0;
            state.dataHeatBal->SurfIntBmIncInsSurfIntensRep(SurfNum) = 0.0;
            state.dataHeatBal->SurfIntBmIncInsSurfAmountRep(SurfNum) = 0.0;
            state.dataHeatBal->SurfIntBmIncInsSurfAmountRepEnergy(SurfNum) = 0.0;

            state.dataHeatBal->SurfQRadSWOutIncident(SurfNum) = 0.0;
            state.dataHeatBal->SurfQRadSWOutIncidentBeam(SurfNum) = 0.0;
            state.dataHeatBal->SurfQRadSWOutIncidentSkyDiffuse(SurfNum) = 0.0;
            state.dataHeatBal->SurfQRadSWOutIncidentGndDiffuse(SurfNum) = 0.0;

            state.dataHeatBal->SurfQRadSWOutIncBmToDiffReflGnd(SurfNum) = 0.0;
            state.dataHeatBal->SurfQRadSWOutIncSkyDiffReflGnd(SurfNum) = 0.0;
            state.dataHeatBal->SurfQRadSWOutIncBmToBmReflObs(SurfNum) = 0.0;
            state.dataHeatBal->SurfQRadSWOutIncBmToDiffReflObs(SurfNum) = 0.0;
            state.dataHeatBal->SurfQRadSWOutIncSkyDiffReflObs(SurfNum) = 0.0;

            state.dataSurface->SurfSkySolarInc(SurfNum) = 0.0;
            state.dataSurface->SurfGndSolarInc(SurfNum) = 0.0;
        }
        for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
            state.dataHeatBal->ZoneTransSolar(zoneNum) = 0.0;
            state.dataHeatBal->ZoneBmSolFrExtWinsRep(zoneNum) = 0.0;
            state.dataHeatBal->ZoneBmSolFrIntWinsRep(zoneNum) = 0.0;
            state.dataHeatBal->ZoneDifSolFrExtWinsRep(zoneNum) = 0.0;
            state.dataHeatBal->ZoneDifSolFrIntWinsRep(zoneNum) = 0.0;
            state.dataHeatBal->ZoneTransSolarEnergy(zoneNum) = 0.0;
            state.dataHeatBal->ZoneBmSolFrExtWinsRepEnergy(zoneNum) = 0.0;
            state.dataHeatBal->ZoneBmSolFrIntWinsRepEnergy(zoneNum) = 0.0;
            state.dataHeatBal->ZoneDifSolFrExtWinsRepEnergy(zoneNum) = 0.0;
            state.dataHeatBal->ZoneDifSolFrIntWinsRepEnergy(zoneNum) = 0.0;
        }
        for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
            int const firstSurfWin = state.dataHeatBal->Zone(zoneNum).WindowSurfaceFirst;
            int const lastSurfWin = state.dataHeatBal->Zone(zoneNum).WindowSurfaceLast;
            for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
                state.dataSurface->SurfWinExtBeamAbsByShade(SurfNum) = 0.0;
                state.dataSurface->SurfWinExtDiffAbsByShade(SurfNum) = 0.0;
                state.dataSurface->SurfWinIntBeamAbsByShade(SurfNum) = 0.0;
                state.dataSurface->SurfWinInitialDifSolAbsByShade(SurfNum) = 0.0;
                state.dataHeatBal->SurfWinQRadSWwinAbsTot(SurfNum) = 0.0;
                state.dataHeatBal->SurfWinQRadSWwinAbsTotEnergy(SurfNum) = 0.0;
                state.dataHeatBal->SurfWinSWwinAbsTotalReport(SurfNum) = 0.0;
                state.dataHeatBalSurf->SurfWinInitialDifSolInTrans(SurfNum) = 0.0;
                state.dataHeatBal->SurfWinInitialDifSolInTransReport(SurfNum) = 0.0;
            }
            for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
                state.dataSurface->SurfWinBlTsolBmBm(SurfNum) = 0.0;
                state.dataSurface->SurfWinBlTsolBmDif(SurfNum) = 0.0;
                state.dataSurface->SurfWinBlTsolDifDif(SurfNum) = 0.0;
                state.dataSurface->SurfWinBlGlSysTsolBmBm(SurfNum) = 0.0;
                state.dataSurface->SurfWinBlGlSysTsolDifDif(SurfNum) = 0.0;
                state.dataSurface->SurfWinScTsolBmBm(SurfNum) = 0.0;
                state.dataSurface->SurfWinScTsolBmDif(SurfNum) = 0.0;
                state.dataSurface->SurfWinScTsolDifDif(SurfNum) = 0.0;
                state.dataSurface->SurfWinScGlSysTsolBmBm(SurfNum) = 0.0;
                state.dataSurface->SurfWinScGlSysTsolDifDif(SurfNum) = 0.0;
                state.dataSurface->SurfWinGlTsolBmBm(SurfNum) = 0.0;
                state.dataSurface->SurfWinGlTsolBmDif(SurfNum) = 0.0;
                state.dataSurface->SurfWinGlTsolDifDif(SurfNum) = 0.0;
            }
            for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
                state.dataSurface->SurfWinBmSolTransThruIntWinRep(SurfNum) = 0.0;
                state.dataSurface->SurfWinBmSolAbsdOutsReveal(SurfNum) = 0.0;
                state.dataSurface->SurfWinBmSolAbsdInsReveal(SurfNum) = 0.0;
                state.dataSurface->SurfWinBmSolRefldInsReveal(SurfNum) = 0.0;
                state.dataSurface->SurfWinOutsRevealDiffOntoGlazing(SurfNum) = 0.0;
                state.dataSurface->SurfWinInsRevealDiffOntoGlazing(SurfNum) = 0.0;
                state.dataSurface->SurfWinInsRevealDiffIntoZone(SurfNum) = 0.0;
                state.dataSurface->SurfWinOutsRevealDiffOntoFrame(SurfNum) = 0.0;
                state.dataSurface->SurfWinInsRevealDiffOntoFrame(SurfNum) = 0.0;
            }
            for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
                state.dataSurface->SurfWinBmSolRefldOutsRevealReport(SurfNum) = 0.0;
                state.dataSurface->SurfWinBmSolRefldInsRevealReport(SurfNum) = 0.0;
                state.dataSurface->SurfWinBmSolAbsdInsRevealReport(SurfNum) = 0.0;
                state.dataSurface->SurfWinInsRevealDiffOntoGlazingReport(SurfNum) = 0.0;
                state.dataSurface->SurfWinInsRevealDiffIntoZoneReport(SurfNum) = 0.0;
                state.dataSurface->SurfWinInsRevealDiffOntoFrameReport(SurfNum) = 0.0;
                state.dataSurface->SurfWinBmSolTransThruIntWinRepEnergy(SurfNum) = 0.0;
                state.dataSurface->SurfWinBmSolRefldOutsRevealRepEnergy(SurfNum) = 0.0;
                state.dataSurface->SurfWinBmSolRefldInsRevealRepEnergy(SurfNum) = 0.0;
            }

            for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
                state.dataSurface->SurfWinTransSolar(SurfNum) = 0.0;
                state.dataSurface->SurfWinBmSolar(SurfNum) = 0.0;
                state.dataSurface->SurfWinBmBmSolar(SurfNum) = 0.0;
                state.dataSurface->SurfWinBmDifSolar(SurfNum) = 0.0;
                state.dataSurface->SurfWinDifSolar(SurfNum) = 0.0;
                state.dataSurface->SurfWinTransSolarEnergy(SurfNum) = 0.0;
                state.dataSurface->SurfWinBmSolarEnergy(SurfNum) = 0.0;
                state.dataSurface->SurfWinBmBmSolarEnergy(SurfNum) = 0.0;
                state.dataSurface->SurfWinBmDifSolarEnergy(SurfNum) = 0.0;
                state.dataSurface->SurfWinDifSolarEnergy(SurfNum) = 0.0;
                state.dataHeatBal->SurfWinBSDFBeamDirectionRep(SurfNum) = 0;
                state.dataHeatBal->SurfWinBSDFBeamThetaRep(SurfNum) = 0.0;
                state.dataHeatBal->SurfWinBSDFBeamPhiRep(SurfNum) = 0.0;
            }
            for (int Lay = 1; Lay <= state.dataHeatBal->MaxSolidWinLayers; Lay++) {
                for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
                    state.dataHeatBal->SurfWinQRadSWwinAbsLayer(SurfNum, Lay) = 0.0;
                }
            }
            for (int Lay = 1; Lay <= DataWindowEquivalentLayer::CFSMAXNL; Lay++) {
                for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
                    state.dataHeatBal->SurfWinInitialDifSolwinAbs(SurfNum, Lay) = 0.0;
                }
            }
        }
    }
    if (resetSolar) {
        for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
            state.dataHeatBal->EnclSolQD(zoneNum) = 0.0;
            state.dataHeatBal->EnclSolQDforDaylight(zoneNum) = 0.0;
        }

        // TTD domes are currently not considered in the window list of a zone
        if (state.dataDaylightingDevicesData->NumOfTDDPipes > 0) {
            for (auto &e : state.dataDaylightingDevicesData->TDDPipe) {
                e.TransSolBeam = 0.0;
                e.TransSolDiff = 0.0;
                e.TransVisBeam = 0.0;
                e.TransVisDiff = 0.0;
                e.TransmittedSolar = 0.0;
                int SurfDome = e.Dome;
                state.dataSurface->SurfWinTransSolar(SurfDome) = 0.0;
                state.dataHeatBal->SurfQRadSWOutIncident(SurfDome) = 0.0;
                state.dataHeatBal->SurfWinQRadSWwinAbsTot(SurfDome) = 0.0;
                for (int Lay = 1; Lay <= DataWindowEquivalentLayer::CFSMAXNL + 1; Lay++) {
                    state.dataHeatBal->SurfWinQRadSWwinAbs(SurfDome, Lay) = 0.0;
                }
            }
        }

        if (state.dataSurface->CalcSolRefl) {
            for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
                state.dataSurface->SurfBmToBmReflFacObs(SurfNum) = 0.0;
                state.dataSurface->SurfBmToDiffReflFacObs(SurfNum) = 0.0;
                state.dataSurface->SurfBmToDiffReflFacGnd(SurfNum) = 0.0;
            }
        }
        for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
            state.dataHeatBal->SurfInitialDifSolInAbsReport(SurfNum) = 0.0;
            state.dataHeatBal->SurfCosIncidenceAngle(SurfNum) = 0.0;
            state.dataHeatBal->SurfSWInAbsTotalReport(SurfNum) = 0.0;
            state.dataSurface->SurfWinProfileAngHor(SurfNum) = 0.0;
            state.dataSurface->SurfWinProfileAngVert(SurfNum) = 0.0;
            state.dataSurface->SurfWinSysSolReflectance(SurfNum) = 0.0;
            state.dataSurface->SurfWinSysSolAbsorptance(SurfNum) = 0.0;
        }
    }

    if (currSolRadPositive) { // Sun is up, calculate solar quantities
        assert(equal_dimensions(state.dataSurface->SurfReflFacBmToBmSolObs, state.dataSurface->SurfReflFacBmToDiffSolObs)); // For linear indexing
        assert(equal_dimensions(state.dataSurface->SurfReflFacBmToBmSolObs, state.dataSurface->SurfReflFacBmToDiffSolGnd)); // For linear indexing
        for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
            state.dataSurface->SurfSkySolarInc(SurfNum) = state.dataEnvrn->DifSolarRad * state.dataHeatBal->SurfAnisoSkyMult(SurfNum);
            state.dataSurface->SurfGndSolarInc(SurfNum) = state.dataEnvrn->GndSolarRad * Surface(SurfNum).ViewFactorGround;
            state.dataSurface->SurfWinSkyGndSolarInc(SurfNum) = state.dataSurface->SurfGndSolarInc(SurfNum);
            state.dataSurface->SurfWinBmGndSolarInc(SurfNum) = 0.0;
        }
        if (state.dataSurface->CalcSolRefl) {
            // [ lSH ] == ( HourOfDay, SurfNum ) // [ lSP ] == ( PreviousHour, SurfNum )
            Array1D<Real64>::size_type lSH = state.dataSurface->SurfReflFacBmToBmSolObs.index(state.dataGlobal->HourOfDay, 1) - 1;
            Array1D<Real64>::size_type lSP = state.dataSurface->SurfReflFacBmToBmSolObs.index(state.dataGlobal->PreviousHour, 1) - 1;
            // For Complex Fenestrations:
            for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
                state.dataSurface->SurfWinSkyGndSolarInc(SurfNum) =
                    state.dataEnvrn->DifSolarRad * state.dataEnvrn->GndReflectance * state.dataSurface->SurfReflFacSkySolGnd(SurfNum);
                state.dataSurface->SurfWinBmGndSolarInc(SurfNum) = state.dataEnvrn->BeamSolarRad * state.dataEnvrn->SOLCOS(3) *
                                                                   state.dataEnvrn->GndReflectance *
                                                                   state.dataSurface->SurfBmToDiffReflFacGnd(SurfNum);
                state.dataSurface->SurfBmToBmReflFacObs(SurfNum) =
                    state.dataGlobal->WeightNow * state.dataSurface->SurfReflFacBmToBmSolObs[lSH + SurfNum] +
                    state.dataGlobal->WeightPreviousHour * state.dataSurface->SurfReflFacBmToBmSolObs[lSP + SurfNum];
                state.dataSurface->SurfBmToDiffReflFacObs(SurfNum) =
                    state.dataGlobal->WeightNow * state.dataSurface->SurfReflFacBmToDiffSolObs[lSH + SurfNum] +
                    state.dataGlobal->WeightPreviousHour * state.dataSurface->SurfReflFacBmToDiffSolObs[lSP + SurfNum];
                state.dataSurface->SurfBmToDiffReflFacGnd(SurfNum) =
                    state.dataGlobal->WeightNow * state.dataSurface->SurfReflFacBmToDiffSolGnd[lSH + SurfNum] +
                    state.dataGlobal->WeightPreviousHour * state.dataSurface->SurfReflFacBmToDiffSolGnd[lSP + SurfNum];
                // TH2 CR 9056
                state.dataSurface->SurfSkySolarInc(SurfNum) += state.dataEnvrn->BeamSolarRad * (state.dataSurface->SurfBmToBmReflFacObs(SurfNum) +
                                                                                                state.dataSurface->SurfBmToDiffReflFacObs(SurfNum)) +
                                                               state.dataEnvrn->DifSolarRad * state.dataSurface->SurfReflFacSkySolObs(SurfNum);
                state.dataSurface->SurfGndSolarInc(SurfNum) =
                    state.dataEnvrn->BeamSolarRad * state.dataEnvrn->SOLCOS(3) * state.dataEnvrn->GndReflectance *
                        state.dataSurface->SurfBmToDiffReflFacGnd(SurfNum) +
                    state.dataEnvrn->DifSolarRad * state.dataEnvrn->GndReflectance * state.dataSurface->SurfReflFacSkySolGnd(SurfNum);
                state.dataSurface->SurfSkyDiffReflFacGnd(SurfNum) = state.dataSurface->SurfReflFacSkySolGnd(SurfNum);
            }
        }

        CalcWindowProfileAngles(state);

        if (state.dataHeatBal->CalcWindowRevealReflection) CalcBeamSolarOnWinRevealSurface(state);

        if (state.dataWindowManager->inExtWindowModel->isExternalLibraryModel() && state.dataWindowManager->winOpticalModel->isSimplifiedModel()) {
            CalcAbsorbedOnExteriorOpaqueSurfaces(state);
            if (state.dataWindowManager->winOpticalModel->isSimplifiedModel()) {
                CalcInteriorSolarDistributionWCESimple(state);
            }
        } else {
            CalcInteriorSolarDistribution(state);
        }

        for (int ZoneNum = 1; ZoneNum <= state.dataViewFactor->NumOfSolarEnclosures; ++ZoneNum) {

            // TH 3/24/2010 - QBV is not used!
            // unused      QBV(ZoneNum) = (CBZone(ZoneNum) + EnclSolDB(ZoneNum))*BeamSolarRad

            // RJH 08/30/07 - QDV does not seem to ever be used. NOT USED!
            // QDV(ZoneNum) = EnclSolDS(ZoneNum)*DifSolarRad &
            //                +EnclSolDG(ZoneNum)*GndSolarRad

            // Original QD calc used only for EnclSolQSDifSol and daylighting calcs
            // QDforDaylight(ZoneNum)  = EnclSolDB(ZoneNum)*BeamSolarRad  &
            //                          +EnclSolDS(ZoneNum)*DifSolarRad  &
            //                          +EnclSolDG(ZoneNum)*GndSolarRad

            //  Beam from interior windows (EnclSolDBIntWin) reflected from floor is counted in DayltgInterReflIllFrIntWins,
            //  EnclSolDB needs to subtract this part since it is already counted in EnclSolDB.
            //  Use ZoneInitialDifSolReflW (Rob's previous work) as it better counts initial distribution of
            //   diffuse solar rather than using weighted area*absorptance
            state.dataHeatBal->EnclSolQDforDaylight(ZoneNum) =
                (state.dataHeatBal->EnclSolDB(ZoneNum) - state.dataHeatBal->EnclSolDBIntWin(ZoneNum)) * state.dataEnvrn->BeamSolarRad +
                state.dataHeatBal->EnclSolDBSSG(ZoneNum) + state.dataHeatBal->ZoneInitialDifSolReflW(ZoneNum);

            // to exclude diffuse solar now absorbed/transmitted in CalcWinTransDifSolInitialDistribution
            // EnclSolDB(ZoneNum) is Diffuse Solar from beam reflected from interior surfaces
            // and transmitted through interior windows
            // EnclSolDB is a factor that when multiplied by BeamSolarRad [W/m2] gives Watts
            // QD(ZoneNum)  = EnclSolDB(ZoneNum)*BeamSolarRad  &
            //                +EnclSolDS(ZoneNum)*DifSolarRad  &
            //                +EnclSolDG(ZoneNum)*GndSolarRad
            state.dataHeatBal->EnclSolQD(ZoneNum) = state.dataHeatBal->EnclSolDB(ZoneNum) * state.dataEnvrn->BeamSolarRad +
                                                    state.dataHeatBal->EnclSolDBSSG(ZoneNum) + state.dataHeatBal->ZoneInitialDifSolReflW(ZoneNum);
        }

        // Flux of diffuse solar in each zone

        state.dataHeatBal->EnclSolQSDifSol = 0.0;
        for (int enclNum = 1; enclNum <= state.dataViewFactor->NumOfSolarEnclosures; ++enclNum) {
            state.dataHeatBal->EnclSolQSDifSol(enclNum) = state.dataHeatBal->EnclSolQDforDaylight(enclNum);
        }

        if (state.dataHeatBalSurf->InterZoneWindow) {
            for (int enclNum = 1; enclNum <= state.dataViewFactor->NumOfSolarEnclosures; ++enclNum) {
                if (state.dataHeatBalSurf->RecDifShortFromZ(enclNum)) {
                    Real64 EnclSolQSDifSol_sum(0.0);                                        // Accumulator
                    auto lZone(state.dataHeatBalSurf->FractDifShortZtoZ.index(enclNum, 1)); // Tuned Linear indexing
                    for (int otherEnclNum = 1; otherEnclNum <= state.dataViewFactor->NumOfSolarEnclosures; ++otherEnclNum, ++lZone) {
                        if ((otherEnclNum != enclNum) && (state.dataHeatBalSurf->RecDifShortFromZ(otherEnclNum))) {
                            EnclSolQSDifSol_sum += state.dataHeatBalSurf->FractDifShortZtoZ[lZone] *
                                                   state.dataHeatBal->EnclSolQDforDaylight(otherEnclNum); // [ lZone ] == ( enclNum, otherEnclNum )
                        }
                    }
                    state.dataHeatBal->EnclSolQSDifSol(enclNum) += EnclSolQSDifSol_sum;
                }
            }
        }

        for (int enclNum = 1; enclNum <= state.dataViewFactor->NumOfSolarEnclosures; ++enclNum) {
            if (state.dataHeatBalSurf->InterZoneWindow)
                state.dataHeatBal->EnclSolQSDifSol(enclNum) *=
                    state.dataHeatBalSurf->FractDifShortZtoZ(enclNum, enclNum) * state.dataHeatBal->EnclSolVMULT(enclNum);
            else
                state.dataHeatBal->EnclSolQSDifSol(enclNum) *= state.dataHeatBal->EnclSolVMULT(enclNum);
        }

        //    RJH - 09-12-07 commented out report varariable calcs here since they refer to old distribution method
        //    DO SurfNum = 1, TotSurfaces
        //      IF (.NOT. Surface(SurfNum)%HeatTransSurf) CYCLE
        //!!! Following may need to be removed or changed when shelves are considered in adjacent reflection calculations
        //      IF (Surface(SurfNum)%Class == SurfaceClass::Shading) CYCLE
        //      ZoneNum = Surface(SurfNum)%Zone
        // Diffuse solar entering zone through exterior windows is assumed to be uniformly
        // distributed on inside face of surfaces of zone
        //      DifIncInsSurfIntensRep(SurfNum) = (EnclSolDS(ZoneNum)*DifSolarRad + EnclSolDG(ZoneNum)*GndSolarRad) /  &
        //        Zone(ZoneNum)%TotalSurfArea
        //      DifIncInsSurfAmountRep(SurfNum) = (Surface(SurfNum)%Area + SurfaceWindow(SurfNum)%DividerArea) *  &
        //        DifIncInsSurfIntensRep(SurfNum)
        //      DifIncInsSurfAmountRepEnergy(SurfNum) = DifIncInsSurfAmountRep(SurfNum) * TimeStepZoneSec
        //    END DO
        if (state.dataSurface->BuildingShadingCount || state.dataSurface->FixedShadingCount || state.dataSurface->AttachedShadingCount) {
            for (int SurfNum = state.dataSurface->ShadingSurfaceFirst; SurfNum <= state.dataSurface->ShadingSurfaceLast; SurfNum++) {
                // Cosine of incidence angle and solar incident on outside of surface, for reporting
                Real64 CosInc = state.dataHeatBal->CosIncAng(state.dataGlobal->TimeStep, state.dataGlobal->HourOfDay, SurfNum);
                state.dataHeatBal->SurfCosIncidenceAngle(SurfNum) = CosInc;
                // Incident direct (unreflected) beam
                state.dataHeatBal->SurfQRadSWOutIncidentBeam(SurfNum) =
                    state.dataEnvrn->BeamSolarRad * state.dataHeatBal->SunlitFrac(state.dataGlobal->TimeStep, state.dataGlobal->HourOfDay, SurfNum) *
                    CosInc;
                // Incident (unreflected) diffuse solar from sky -- TDD_Diffuser calculated differently
                state.dataHeatBal->SurfQRadSWOutIncidentSkyDiffuse(SurfNum) =
                    state.dataEnvrn->DifSolarRad * state.dataHeatBal->SurfAnisoSkyMult(SurfNum);
                // Incident diffuse solar from sky diffuse reflected from ground plus beam reflected from ground
                state.dataHeatBal->SurfQRadSWOutIncidentGndDiffuse(SurfNum) = state.dataSurface->SurfGndSolarInc(SurfNum);
                // Incident diffuse solar from beam-to-diffuse reflection from ground
                state.dataHeatBal->SurfQRadSWOutIncBmToDiffReflGnd(SurfNum) = state.dataEnvrn->BeamSolarRad * state.dataEnvrn->SOLCOS(3) *
                                                                              state.dataEnvrn->GndReflectance *
                                                                              state.dataSurface->SurfBmToDiffReflFacGnd(SurfNum);
                // Incident diffuse solar from sky diffuse reflection from ground
                state.dataHeatBal->SurfQRadSWOutIncSkyDiffReflGnd(SurfNum) =
                    state.dataEnvrn->DifSolarRad * state.dataEnvrn->GndReflectance * state.dataSurface->SurfSkyDiffReflFacGnd(SurfNum);
                // Total incident solar. Beam and sky reflection from obstructions, if calculated, is included
                // in SkySolarInc.
                state.dataHeatBal->SurfQRadSWOutIncident(SurfNum) =
                    state.dataHeatBal->SurfQRadSWOutIncidentBeam(SurfNum) + state.dataHeatBal->SurfQRadSWOutIncidentSkyDiffuse(SurfNum) +
                    state.dataHeatBal->SurfQRadSWOutIncBmToDiffReflGnd(SurfNum) + state.dataHeatBal->SurfQRadSWOutIncSkyDiffReflGnd(SurfNum);
            }
        }
        for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
            for (int SurfNum : state.dataHeatBal->Zone(zoneNum).ZoneExtSolarSurfaceList) {
                // Regular surface
                currCosInc(SurfNum) = state.dataHeatBal->CosIncAng(state.dataGlobal->TimeStep, state.dataGlobal->HourOfDay, SurfNum);
                currBeamSolar(SurfNum) = state.dataEnvrn->BeamSolarRad;
                currSkySolarInc(SurfNum) = state.dataSurface->SurfSkySolarInc(SurfNum);
                currGndSolarInc(SurfNum) = state.dataSurface->SurfGndSolarInc(SurfNum);
                // Cosine of incidence angle and solar incident on outside of surface, for reporting
                state.dataHeatBal->SurfCosIncidenceAngle(SurfNum) = currCosInc(SurfNum);
                // Report variables for various incident solar quantities
                // Incident direct (unreflected) beam
                state.dataHeatBal->SurfQRadSWOutIncidentBeam(SurfNum) =
                    currBeamSolar(SurfNum) * state.dataHeatBal->SunlitFrac(state.dataGlobal->TimeStep, state.dataGlobal->HourOfDay, SurfNum) *
                    currCosInc(SurfNum);

                // Incident (unreflected) diffuse solar from sky -- TDD_Diffuser calculated differently
                state.dataHeatBal->SurfQRadSWOutIncidentSkyDiffuse(SurfNum) =
                    state.dataEnvrn->DifSolarRad * state.dataHeatBal->SurfAnisoSkyMult(SurfNum);
                // Incident diffuse solar from sky diffuse reflected from ground plus beam reflected from ground
                state.dataHeatBal->SurfQRadSWOutIncidentGndDiffuse(SurfNum) = currGndSolarInc(SurfNum);
                // Incident diffuse solar from beam-to-diffuse reflection from ground
                state.dataHeatBal->SurfQRadSWOutIncBmToDiffReflGnd(SurfNum) = state.dataEnvrn->BeamSolarRad * state.dataEnvrn->SOLCOS(3) *
                                                                              state.dataEnvrn->GndReflectance *
                                                                              state.dataSurface->SurfBmToDiffReflFacGnd(SurfNum);

                // Incident diffuse solar from sky diffuse reflection from ground
                state.dataHeatBal->SurfQRadSWOutIncSkyDiffReflGnd(SurfNum) =
                    state.dataEnvrn->DifSolarRad * state.dataEnvrn->GndReflectance * state.dataSurface->SurfSkyDiffReflFacGnd(SurfNum);
                // Total incident solar. Beam and sky reflection from obstructions, if calculated, is included
                // in SkySolarInc.
                // QRadSWOutIncident(SurfNum) = QRadSWOutIncidentBeam(SurfNum) + SkySolarInc + GndSolarInc
                // TH2 CR 9056
                state.dataHeatBal->SurfQRadSWOutIncident(SurfNum) =
                    state.dataHeatBal->SurfQRadSWOutIncidentBeam(SurfNum) + state.dataHeatBal->SurfQRadSWOutIncidentSkyDiffuse(SurfNum) +
                    state.dataHeatBal->SurfQRadSWOutIncBmToDiffReflGnd(SurfNum) + state.dataHeatBal->SurfQRadSWOutIncSkyDiffReflGnd(SurfNum);

                if (state.dataSurface->CalcSolRefl) {
                    // Incident beam solar from beam-to-beam (specular) reflection from obstructions
                    state.dataHeatBal->SurfQRadSWOutIncBmToBmReflObs(SurfNum) =
                        state.dataSurface->SurfBmToBmReflFacObs(SurfNum) * state.dataEnvrn->BeamSolarRad;
                    // Incident diffuse solar from beam-to-diffuse reflection from obstructions
                    state.dataHeatBal->SurfQRadSWOutIncBmToDiffReflObs(SurfNum) =
                        state.dataSurface->SurfBmToDiffReflFacObs(SurfNum) * state.dataEnvrn->BeamSolarRad;
                    // Incident diffuse solar from sky diffuse reflection from obstructions
                    state.dataHeatBal->SurfQRadSWOutIncSkyDiffReflObs(SurfNum) =
                        state.dataEnvrn->DifSolarRad * state.dataSurface->SurfReflFacSkySolObs(SurfNum);
                    // TH2 CR 9056: Add reflections from obstructions to the total incident
                    state.dataHeatBal->SurfQRadSWOutIncident(SurfNum) += state.dataHeatBal->SurfQRadSWOutIncBmToBmReflObs(SurfNum) +
                                                                         state.dataHeatBal->SurfQRadSWOutIncBmToDiffReflObs(SurfNum) +
                                                                         state.dataHeatBal->SurfQRadSWOutIncSkyDiffReflObs(SurfNum);
                }
            }
        }
        for (int PipeNum = 1; PipeNum <= state.dataDaylightingDevicesData->NumOfTDDPipes; ++PipeNum) {
            int const SurfNum = state.dataDaylightingDevicesData->TDDPipe(PipeNum).Diffuser; // TDD: Diffuser object number
            int const SurfNum2 = state.dataDaylightingDevicesData->TDDPipe(PipeNum).Dome;    // TDD: DOME object number
            int const ConstrNum = state.dataSurface->SurfActiveConstruction(SurfNum);

            // Reconstruct the beam, sky, and ground radiation transmittance of just the TDD:DOME and TDD pipe
            // by dividing out diffuse solar transmittance of TDD:DIFFUSER
            currCosInc(SurfNum) = state.dataHeatBal->CosIncAng(state.dataGlobal->TimeStep, state.dataGlobal->HourOfDay, SurfNum2);
            currBeamSolar(SurfNum) = state.dataEnvrn->BeamSolarRad *
                                     TransTDD(state, PipeNum, currCosInc(SurfNum), DataDaylightingDevices::iRadType::SolarBeam) /
                                     state.dataConstruction->Construct(ConstrNum).TransDiff;

            currSkySolarInc(SurfNum) = state.dataEnvrn->DifSolarRad * state.dataHeatBal->SurfAnisoSkyMult(SurfNum2) *
                                       TransTDD(state, PipeNum, currCosInc(SurfNum), DataDaylightingDevices::iRadType::SolarAniso) /
                                       state.dataConstruction->Construct(ConstrNum).TransDiff;

            currGndSolarInc(SurfNum) = state.dataEnvrn->GndSolarRad * Surface(SurfNum2).ViewFactorGround *
                                       state.dataDaylightingDevicesData->TDDPipe(PipeNum).TransSolIso /
                                       state.dataConstruction->Construct(ConstrNum).TransDiff;
            // Incident direct (unreflected) beam
            state.dataHeatBal->SurfQRadSWOutIncidentBeam(SurfNum) =
                currBeamSolar(SurfNum) * state.dataHeatBal->SunlitFrac(state.dataGlobal->TimeStep, state.dataGlobal->HourOfDay, SurfNum2) *
                currCosInc(SurfNum); // NOTE: sunlit and coninc array set to SurfNum2

            // Incident (unreflected) diffuse solar from sky -- TDD_Diffuser calculated differently
            state.dataHeatBal->SurfQRadSWOutIncidentSkyDiffuse(SurfNum) = currSkySolarInc(SurfNum);
            state.dataHeatBal->SurfQRadSWOutIncident(SurfNum) =
                state.dataHeatBal->SurfQRadSWOutIncidentBeam(SurfNum) + state.dataHeatBal->SurfQRadSWOutIncidentSkyDiffuse(SurfNum) +
                state.dataHeatBal->SurfQRadSWOutIncBmToDiffReflGnd(SurfNum) + state.dataHeatBal->SurfQRadSWOutIncSkyDiffReflGnd(SurfNum);
        }

        for (int ShelfNum = 1; ShelfNum <= state.dataDaylightingDevicesData->NumOfShelf; ++ShelfNum) {
            int SurfNum = state.dataDaylightingDevicesData->Shelf(ShelfNum).Window;       // Daylighting shelf object number
            int OutShelfSurf = state.dataDaylightingDevicesData->Shelf(ShelfNum).OutSurf; // Outside daylighting shelf present if > 0
            currCosInc(SurfNum) = state.dataHeatBal->CosIncAng(state.dataGlobal->TimeStep, state.dataGlobal->HourOfDay, SurfNum);
            currBeamSolar(SurfNum) = state.dataEnvrn->BeamSolarRad;
            currSkySolarInc(SurfNum) = state.dataEnvrn->DifSolarRad * state.dataHeatBal->SurfAnisoSkyMult(SurfNum);
            // Shelf diffuse solar radiation
            Real64 ShelfSolarRad = (state.dataEnvrn->BeamSolarRad *
                                        state.dataHeatBal->SunlitFrac(state.dataGlobal->TimeStep, state.dataGlobal->HourOfDay, OutShelfSurf) *
                                        state.dataHeatBal->CosIncAng(state.dataGlobal->TimeStep, state.dataGlobal->HourOfDay, OutShelfSurf) +
                                    state.dataEnvrn->DifSolarRad * state.dataHeatBal->SurfAnisoSkyMult(OutShelfSurf)) *
                                   state.dataDaylightingDevicesData->Shelf(ShelfNum).OutReflectSol;

            // Add all reflected solar from the outside shelf to the ground solar
            // NOTE:  If the shelf blocks part of the view to the ground, the user must reduce the ground view factor!!
            currGndSolarInc(SurfNum) = state.dataEnvrn->GndSolarRad * Surface(SurfNum).ViewFactorGround +
                                       ShelfSolarRad * state.dataDaylightingDevicesData->Shelf(ShelfNum).ViewFactor;
        }

        for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
            int const firstSurfOpaq = state.dataHeatBal->Zone(zoneNum).OpaqOrIntMassSurfaceFirst;
            int const lastSurfOpaq = state.dataHeatBal->Zone(zoneNum).OpaqOrIntMassSurfaceLast;
            for (int SurfNum = firstSurfOpaq; SurfNum <= lastSurfOpaq; ++SurfNum) {
                int const ConstrNum = state.dataSurface->Surface(SurfNum).Construction;
                if (Surface(SurfNum).ExtSolar) {
                    Real64 AbsExt =
                        state.dataHeatBalSurf->SurfAbsSolarExt(SurfNum); // Absorptivity of outer most layer (or movable insulation if present)
                    state.dataHeatBalSurf->SurfOpaqQRadSWOutAbs(SurfNum) = state.dataSurface->SurfOpaqAO(SurfNum) * state.dataEnvrn->BeamSolarRad +
                                                                           AbsExt * (currSkySolarInc(SurfNum) + currGndSolarInc(SurfNum));
                    state.dataHeatBal->SurfOpaqSWOutAbsTotalReport(SurfNum) =
                        state.dataHeatBalSurf->SurfOpaqQRadSWOutAbs(SurfNum) * Surface(SurfNum).Area;
                    state.dataHeatBal->SurfOpaqSWOutAbsEnergyReport(SurfNum) =
                        state.dataHeatBal->SurfOpaqSWOutAbsTotalReport(SurfNum) * state.dataGlobal->TimeStepZoneSec;
                }
                if (ConstrNum > 0) {
                    int SurfSolIncPtr = SurfaceScheduledSolarInc(state, SurfNum, ConstrNum);
                    if (SurfSolIncPtr == 0) {
                        if (state.dataConstruction->Construct(ConstrNum).TransDiff <= 0.0) {    // Opaque surface
                            int ShelfNum = state.dataSurface->SurfDaylightingShelfInd(SurfNum); // Daylighting shelf object number
                            int InShelfSurf = 0;                                                // Inside daylighting shelf surface number
                            if (ShelfNum > 0) {
                                InShelfSurf = state.dataDaylightingDevicesData->Shelf(ShelfNum).InSurf; // Inside daylighting shelf present if > 0
                            }
                            state.dataHeatBalSurf->SurfOpaqQRadSWInAbs(SurfNum) +=
                                state.dataSurface->SurfOpaqAI(SurfNum) * state.dataEnvrn->BeamSolarRad;
                            if (InShelfSurf > 0) { // Inside daylighting shelf
                                // Shelf surface area is divided by 2 because only one side sees beam (Area was multiplied by 2 during init)
                                state.dataHeatBalSurf->SurfOpaqInsFaceBeamSolAbsorbed(SurfNum) =
                                    state.dataSurface->SurfOpaqAI(SurfNum) * state.dataEnvrn->BeamSolarRad * (0.5 * Surface(SurfNum).Area);
                            } else { // Regular surface
                                state.dataHeatBalSurf->SurfOpaqInsFaceBeamSolAbsorbed(SurfNum) =
                                    state.dataSurface->SurfOpaqAI(SurfNum) * state.dataEnvrn->BeamSolarRad * Surface(SurfNum).Area;
                            }
                        }
                    } else {
                        state.dataHeatBalSurf->SurfOpaqQRadSWInAbs(SurfNum) += state.dataSurface->SurfOpaqAI(SurfNum);
                    }
                }
            }
            int const firstSurfWin = state.dataHeatBal->Zone(zoneNum).WindowSurfaceFirst;
            int const lastSurfWin = state.dataHeatBal->Zone(zoneNum).WindowSurfaceLast;
            for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
                if (Surface(SurfNum).ExtSolar || state.dataSurface->SurfWinOriginalClass(SurfNum) == SurfaceClass::TDD_Diffuser) {
                    // Exclude special shading surfaces which required QRadSWOut calculations above
                    int const ConstrNum = state.dataSurface->SurfActiveConstruction(SurfNum);
                    Real64 CosInc = currCosInc(SurfNum);           // Cosine of incidence angle of beam solar on glass
                    Real64 BeamSolar = currBeamSolar(SurfNum);     // Local variable for BeamSolarRad
                    Real64 SkySolarInc = currSkySolarInc(SurfNum); // Sky diffuse solar incident on a surface
                    Real64 GndSolarInc = currGndSolarInc(SurfNum); // Ground diffuse solar incident on a surface

                    WinShadingType ShadeFlag = state.dataSurface->SurfWinShadingFlag(SurfNum);

                    if (state.dataSurface->SurfWinWindowModelType(SurfNum) == Window5DetailedModel &&
                        !state.dataWindowManager->inExtWindowModel->isExternalLibraryModel()) {
                        int TotGlassLay = state.dataConstruction->Construct(ConstrNum).TotGlassLayers; // Number of glass layers
                        for (int Lay = 1; Lay <= TotGlassLay; ++Lay) {
                            AbsDiffWin(Lay) = state.dataConstruction->Construct(ConstrNum).AbsDiff(Lay);
                        }

                        if (IS_SHADED(ShadeFlag)) { // Shaded window

                            int const ConstrNumSh = state.dataSurface->SurfWinActiveShadedConstruction(SurfNum); // Shaded window construction

                            if (ANY_SHADE_SCREEN(ShadeFlag)) { // Shade/screen on
                                for (int Lay = 1; Lay <= TotGlassLay; ++Lay) {
                                    AbsDiffWin(Lay) = state.dataConstruction->Construct(ConstrNumSh).AbsDiff(Lay);
                                }
                                state.dataSurface->SurfWinExtDiffAbsByShade(SurfNum) =
                                    state.dataConstruction->Construct(ConstrNumSh).AbsDiffShade * (SkySolarInc + GndSolarInc);
                            } else if (ANY_BLIND(ShadeFlag)) { // Blind on
                                int SurfWinSlatsAngIndex = state.dataSurface->SurfWinSlatsAngIndex(SurfNum);
                                Real64 SurfWinSlatsAngInterpFac = state.dataSurface->SurfWinSlatsAngInterpFac(SurfNum);
                                Real64 AbsDiffBlind;
                                if (state.dataSurface->SurfWinMovableSlats(SurfNum)) {
                                    for (int Lay = 1; Lay <= TotGlassLay; ++Lay) {
                                        AbsDiffWin(Lay) = General::InterpGeneral(
                                            state.dataConstruction->Construct(ConstrNumSh).BlAbsDiff(SurfWinSlatsAngIndex, Lay),
                                            state.dataConstruction->Construct(ConstrNumSh)
                                                .BlAbsDiff(std::min(MaxSlatAngs, SurfWinSlatsAngIndex + 1), Lay),
                                            SurfWinSlatsAngInterpFac);
                                        AbsDiffWinGnd(Lay) = General::InterpGeneral(
                                            state.dataConstruction->Construct(ConstrNumSh).BlAbsDiffGnd(SurfWinSlatsAngIndex, Lay),
                                            state.dataConstruction->Construct(ConstrNumSh)
                                                .BlAbsDiffGnd(std::min(MaxSlatAngs, SurfWinSlatsAngIndex + 1), Lay),
                                            SurfWinSlatsAngInterpFac);
                                        AbsDiffWinSky(Lay) = General::InterpGeneral(
                                            state.dataConstruction->Construct(ConstrNumSh).BlAbsDiffSky(SurfWinSlatsAngIndex, Lay),
                                            state.dataConstruction->Construct(ConstrNumSh)
                                                .BlAbsDiffSky(std::min(MaxSlatAngs, SurfWinSlatsAngIndex + 1), Lay),
                                            SurfWinSlatsAngInterpFac);
                                    }
                                    AbsDiffBlind = General::InterpGeneral(
                                        state.dataConstruction->Construct(ConstrNumSh).AbsDiffBlind(SurfWinSlatsAngIndex),
                                        state.dataConstruction->Construct(ConstrNumSh).AbsDiffBlind(std::min(MaxSlatAngs, SurfWinSlatsAngIndex + 1)),
                                        SurfWinSlatsAngInterpFac);
                                } else {
                                    for (int Lay = 1; Lay <= TotGlassLay; ++Lay) {
                                        AbsDiffWin(Lay) = state.dataConstruction->Construct(ConstrNumSh).BlAbsDiff(1, Lay);
                                        AbsDiffWinGnd(Lay) = state.dataConstruction->Construct(ConstrNumSh).BlAbsDiffGnd(1, Lay);
                                        AbsDiffWinSky(Lay) = state.dataConstruction->Construct(ConstrNumSh).BlAbsDiffSky(1, Lay);
                                    }
                                    AbsDiffBlind = state.dataConstruction->Construct(ConstrNumSh).AbsDiffBlind(1);
                                }
                                state.dataSurface->SurfWinExtDiffAbsByShade(SurfNum) = AbsDiffBlind * (SkySolarInc + GndSolarInc);

                                if (state.dataHeatBal->Blind(state.dataSurface->SurfWinBlindNumber(SurfNum)).SlatOrientation == Horizontal) {
                                    Real64 ACosTlt = std::abs(Surface(SurfNum).CosTilt);
                                    Real64 AbsDiffBlindGnd;
                                    Real64 AbsDiffBlindSky;
                                    if (state.dataSurface->SurfWinMovableSlats(SurfNum)) {
                                        AbsDiffBlindGnd = General::InterpGeneral(
                                            state.dataConstruction->Construct(ConstrNumSh).AbsDiffBlindGnd(SurfWinSlatsAngIndex),
                                            state.dataConstruction->Construct(ConstrNumSh)
                                                .AbsDiffBlindGnd(std::min(MaxSlatAngs, SurfWinSlatsAngIndex + 1)),
                                            SurfWinSlatsAngInterpFac);
                                        AbsDiffBlindSky = General::InterpGeneral(
                                            state.dataConstruction->Construct(ConstrNumSh).AbsDiffBlindSky(SurfWinSlatsAngIndex),
                                            state.dataConstruction->Construct(ConstrNumSh)
                                                .AbsDiffBlindSky(std::min(MaxSlatAngs, SurfWinSlatsAngIndex + 1)),
                                            SurfWinSlatsAngInterpFac);
                                    } else {
                                        AbsDiffBlindGnd = state.dataConstruction->Construct(ConstrNumSh).AbsDiffBlindGnd(1);
                                        AbsDiffBlindSky = state.dataConstruction->Construct(ConstrNumSh).AbsDiffBlindSky(1);
                                    }
                                    state.dataSurface->SurfWinExtDiffAbsByShade(SurfNum) =
                                        SkySolarInc * (0.5 * ACosTlt * AbsDiffBlindGnd + (1.0 - 0.5 * ACosTlt) * AbsDiffBlindSky) +
                                        GndSolarInc * ((1.0 - 0.5 * ACosTlt) * AbsDiffBlindGnd + 0.5 * ACosTlt * AbsDiffBlindSky);
                                }
                            }

                            // Correct for shadowing of divider onto interior shading device (note that dividers are
                            // not allowed in windows with between-glass shade/blind)

                            if (ANY_INTERIOR_SHADE_BLIND(ShadeFlag) && state.dataSurface->SurfWinDividerArea(SurfNum) > 0.0)
                                state.dataSurface->SurfWinExtDiffAbsByShade(SurfNum) *= state.dataSurface->SurfWinGlazedFrac(SurfNum);

                            if (ShadeFlag == WinShadingType::SwitchableGlazing) {                      // Switchable glazing
                                Real64 SwitchFac = state.dataSurface->SurfWinSwitchingFactor(SurfNum); // Switching factor for switchable glazing
                                for (int Lay = 1; Lay <= TotGlassLay; ++Lay) {
                                    AbsDiffWin(Lay) =
                                        InterpSw(SwitchFac, AbsDiffWin(Lay), state.dataConstruction->Construct(ConstrNumSh).AbsDiff(Lay));
                                }
                            }

                        } // End of check if window has shading device on

                        state.dataHeatBal->SurfWinQRadSWwinAbsTot(SurfNum) = 0.0;
                        for (int Lay = 1; Lay <= TotGlassLay; ++Lay) {
                            state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum, Lay) =
                                AbsDiffWin(Lay) * (SkySolarInc + GndSolarInc) + state.dataSurface->SurfWinA(SurfNum, Lay) * BeamSolar;
                            // SurfWinA is from InteriorSolarDistribution
                            if (ANY_BLIND(ShadeFlag)) {
                                int ConstrNumSh = Surface(SurfNum).activeShadedConstruction;
                                if (state.dataHeatBal->Blind(state.dataSurface->SurfWinBlindNumber(SurfNum)).SlatOrientation == Horizontal) {
                                    Real64 ACosTlt = std::abs(Surface(SurfNum).CosTilt); // Absolute value of cosine of surface tilt angle
                                    Real64 AbsDiffGlassLayGnd; // System glass layer ground diffuse solar absorptance with blind on
                                    Real64 AbsDiffGlassLaySky; // System glass layer sky diffuse solar absorptance with blind on
                                    if (state.dataSurface->SurfWinMovableSlats(SurfNum)) {
                                        AbsDiffGlassLayGnd = General::InterpGeneral(
                                            state.dataConstruction->Construct(ConstrNumSh)
                                                .BlAbsDiffGnd(state.dataSurface->SurfWinSlatsAngIndex(SurfNum), Lay),
                                            state.dataConstruction->Construct(ConstrNumSh)
                                                .BlAbsDiffGnd(std::min(MaxSlatAngs, state.dataSurface->SurfWinSlatsAngIndex(SurfNum) + 1), Lay),
                                            state.dataSurface->SurfWinSlatsAngInterpFac(SurfNum));
                                        AbsDiffGlassLaySky = General::InterpGeneral(
                                            state.dataConstruction->Construct(ConstrNumSh)
                                                .BlAbsDiffSky(state.dataSurface->SurfWinSlatsAngIndex(SurfNum), Lay),
                                            state.dataConstruction->Construct(ConstrNumSh)
                                                .BlAbsDiffSky(std::min(MaxSlatAngs, state.dataSurface->SurfWinSlatsAngIndex(SurfNum) + 1), Lay),
                                            state.dataSurface->SurfWinSlatsAngInterpFac(SurfNum));
                                    } else {
                                        AbsDiffGlassLayGnd = state.dataConstruction->Construct(ConstrNumSh).BlAbsDiffGnd(1, Lay);
                                        AbsDiffGlassLaySky = state.dataConstruction->Construct(ConstrNumSh).BlAbsDiffSky(1, Lay);
                                    }
                                    state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum, Lay) =
                                        SkySolarInc * (0.5 * ACosTlt * AbsDiffGlassLayGnd + (1.0 - 0.5 * ACosTlt) * AbsDiffGlassLaySky) +
                                        GndSolarInc * ((1.0 - 0.5 * ACosTlt) * AbsDiffGlassLayGnd + 0.5 * ACosTlt * AbsDiffGlassLaySky) +
                                        state.dataSurface->SurfWinA(SurfNum, Lay) * BeamSolar;
                                }
                            }
                            // Total solar absorbed in solid layer (W), for reporting
                            state.dataHeatBal->SurfWinQRadSWwinAbsLayer(SurfNum, Lay) =
                                state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum, Lay) * Surface(SurfNum).Area;

                            // Total solar absorbed in all glass layers (W), for reporting
                            state.dataHeatBal->SurfWinQRadSWwinAbsTot(SurfNum) += state.dataHeatBal->SurfWinQRadSWwinAbsLayer(SurfNum, Lay);
                        }
                        state.dataHeatBal->SurfWinQRadSWwinAbsTotEnergy(SurfNum) =
                            state.dataHeatBal->SurfWinQRadSWwinAbsTot(SurfNum) * state.dataGlobal->TimeStepZoneSec;
                        // Need to do it this way for now beaucse of scheduled surface gains. They do work only with
                        // BSDF windows and overwriting absorbtances will work only for ordinary windows
                        // } else if ( SurfaceWindow( SurfNum ).WindowModelType != WindowBSDFModel &&
                        //   SurfaceWindow( SurfNum ).WindowModelType != WindowEQLModel &&
                        //   inExtWindowModel->isExternalLibraryModel() ) {
                        //   TotSolidLay = Construct( ConstrNum ).TotSolidLayers;
                        //   for ( Lay = 1; Lay <= TotSolidLay; ++Lay ) {
                        //     QRadSWwinAbs( Lay, SurfNum ) = SurfWinA( Lay, SurfNum ) *
                        //       ( QRadSWOutIncident( SurfNum ) + QS( Surface( SurfNum ).Zone ) );
                        //   }
                    } else if (state.dataSurface->SurfWinWindowModelType(SurfNum) == WindowBSDFModel) {
                        int TotSolidLay = state.dataConstruction->Construct(ConstrNum).TotSolidLayers;
                        // Number of solid layers in fenestration system (glass + shading)
                        int CurrentState = state.dataSurface->SurfaceWindow(SurfNum).ComplexFen.CurrentState;
                        // Current state for Complex Fenestration
                        // Examine for schedule surface gain
                        Real64 SurfSolAbs = WindowScheduledSolarAbs(state,
                                                                    SurfNum,
                                                                    ConstrNum); // Pointer to scheduled surface gains object for fenestration systems

                        for (int Lay = 1; Lay <= TotSolidLay; ++Lay) {
                            if (SurfSolAbs != 0) {
                                state.dataSurface->SurfWinA(SurfNum, Lay) =
                                    GetCurrentScheduleValue(state, state.dataSurface->FenLayAbsSSG(SurfSolAbs).SchedPtrs(Lay));
                                // ABWin(Lay) = SurfWinA(SurfNum,Lay)
                                state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum, Lay) = state.dataSurface->SurfWinA(SurfNum, Lay);
                            } else {
                                // Several notes about this equation.  First part is accounting for duffuse solar radiation for the ground
                                // and from the sky.  Second item (SurfWinA(SurfNum,Lay) * BeamSolar) is accounting for absorbed solar
                                // radiation originating from beam on exterior side.  Third item (SurfWinACFOverlap(SurfNum,Lay)) is
                                // accounting for absorptances from beam hitting back of the window which passes through rest of exterior
                                // windows
                                state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum, Lay) =
                                    state.dataSurface->SurfaceWindow(SurfNum).ComplexFen.State(CurrentState).WinSkyFtAbs(Lay) * SkySolarInc +
                                    state.dataSurface->SurfaceWindow(SurfNum).ComplexFen.State(CurrentState).WinSkyGndAbs(Lay) * GndSolarInc +
                                    state.dataSurface->SurfWinA(SurfNum, Lay) * BeamSolar +
                                    state.dataSurface->SurfWinACFOverlap(SurfNum, Lay) * BeamSolar;
                            }
                            // Total solar absorbed in solid layer (W), for reporting
                            state.dataHeatBal->SurfWinQRadSWwinAbsLayer(SurfNum, Lay) =
                                state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum, Lay) * Surface(SurfNum).Area;

                            // Total solar absorbed in all glass layers (W), for reporting
                            state.dataHeatBal->SurfWinQRadSWwinAbsTot(SurfNum) += state.dataHeatBal->SurfWinQRadSWwinAbsLayer(SurfNum, Lay);
                        }
                        state.dataHeatBal->SurfWinQRadSWwinAbsTotEnergy(SurfNum) =
                            state.dataHeatBal->SurfWinQRadSWwinAbsTot(SurfNum) * state.dataGlobal->TimeStepZoneSec;

                    } else if (state.dataSurface->SurfWinWindowModelType(SurfNum) == WindowEQLModel) {
                        state.dataHeatBal->SurfWinQRadSWwinAbsTot(SurfNum) = 0.0;
                        // EQLNum = Construct(Surface(SurfNum)%Construction)%EQLConsPtr
                        int TotSolidLay =
                            state.dataWindowEquivLayer->CFS(state.dataConstruction->Construct(Surface(SurfNum).Construction).EQLConsPtr).NL;
                        for (int Lay = 1; Lay <= TotSolidLay; ++Lay) {
                            // Absorbed window components include:
                            // (1) beam solar radiation absorbed by all layers in the fenestration
                            // (2) sky and ground reflected duffuse solar radiation absorbed by all layers
                            // (3) diffuse short wave incident on the inside face of the fenestration.  The short wave internal sources
                            //     include light, ...
                            AbsDiffWin(Lay) = state.dataConstruction->Construct(ConstrNum).AbsDiffFrontEQL(Lay);
                            state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum, Lay) =
                                state.dataSurface->SurfWinA(SurfNum, Lay) * BeamSolar + AbsDiffWin(Lay) * (SkySolarInc + GndSolarInc);

                            // Total solar absorbed in solid layer (W), for reporting
                            state.dataHeatBal->SurfWinQRadSWwinAbsLayer(SurfNum, Lay) =
                                state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum, Lay) * Surface(SurfNum).Area;

                            // Total solar absorbed in all glass layers (W), for reporting
                            state.dataHeatBal->SurfWinQRadSWwinAbsTot(SurfNum) += state.dataHeatBal->SurfWinQRadSWwinAbsLayer(SurfNum, Lay);
                        }

                        state.dataHeatBal->SurfWinQRadSWwinAbsTotEnergy(SurfNum) =
                            state.dataHeatBal->SurfWinQRadSWwinAbsTot(SurfNum) * state.dataGlobal->TimeStepZoneSec;
                    } else if (state.dataWindowManager->inExtWindowModel->isExternalLibraryModel()) {
                        int SurfNum2 = SurfNum;
                        if (state.dataSurface->SurfWinOriginalClass(SurfNum) == SurfaceClass::TDD_Diffuser) {
                            SurfNum2 = state.dataDaylightingDevicesData->TDDPipe(state.dataSurface->SurfWinTDDPipeNum(SurfNum)).Dome;
                        }

                        std::pair<Real64, Real64> incomingAngle = getSunWCEAngles(state, SurfNum2, BSDFHemisphere::Incoming);
                        Real64 Theta = incomingAngle.first;
                        Real64 Phi = incomingAngle.second;

                        std::shared_ptr<CMultiLayerScattered> aLayer =
                            CWindowConstructionsSimplified::instance().getEquivalentLayer(state, WavelengthRange::Solar, ConstrNum);

                        size_t totLayers = aLayer->getNumOfLayers();
                        for (size_t Lay = 1; Lay <= totLayers; ++Lay) {
                            Real64 AbWinDiff = aLayer->getAbsorptanceLayer(Lay, Side::Front, ScatteringSimple::Diffuse, Theta, Phi);

                            state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum, Lay) =
                                AbWinDiff * (SkySolarInc + GndSolarInc) + state.dataSurface->SurfWinA(SurfNum, Lay) * BeamSolar;

                            // Total solar absorbed in solid layer (W), for reporting
                            state.dataHeatBal->SurfWinQRadSWwinAbsLayer(SurfNum, Lay) =
                                state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum, Lay) * Surface(SurfNum).Area;

                            // Total solar absorbed in all glass layers (W), for reporting
                            state.dataHeatBal->SurfWinQRadSWwinAbsTot(SurfNum) += state.dataHeatBal->SurfWinQRadSWwinAbsLayer(SurfNum, Lay);
                        }
                    }

                    // Solar absorbed by window frame and dividers
                    int FrDivNum = Surface(SurfNum).FrameDivider; // Frame/divider number
                    if (FrDivNum > 0) {
                        Real64 FrArea = state.dataSurface->SurfWinFrameArea(SurfNum);                    // Frame, divider area (m2)
                        Real64 FrProjOut = state.dataSurface->FrameDivider(FrDivNum).FrameProjectionOut; // Frame, divider outside projection (m)
                        Real64 FrProjIn = state.dataSurface->FrameDivider(FrDivNum).FrameProjectionIn;
                        Real64 DivArea = state.dataSurface->SurfWinDividerArea(SurfNum);
                        Real64 DivWidth = state.dataSurface->FrameDivider(FrDivNum).DividerWidth;
                        Real64 DivProjOut = state.dataSurface->FrameDivider(FrDivNum).DividerProjectionOut;
                        Real64 DivProjIn = state.dataSurface->FrameDivider(FrDivNum).DividerProjectionIn;
                        Real64 CosIncAngHorProj = 0.0;  // Cosine of incidence angle of sun on horizontal faces of a frame or divider projection
                        Real64 CosIncAngVertProj = 0.0; // Cosine of incidence angle of sun on vertical faces of a frame or divider projection
                        Real64 FracSunLit = 0.0;        // Fraction of window sunlit this time step
                        Real64 BeamFaceInc;             // Beam solar incident window plane this time step (W/m2)
                        Real64 DifSolarFaceInc;         // Diffuse solar incident on window plane this time step (W/m2)
                        if (FrArea > 0.0 || DivArea > 0.0) {
                            FracSunLit = state.dataHeatBal->SunlitFrac(state.dataGlobal->TimeStep, state.dataGlobal->HourOfDay, SurfNum);
                            BeamFaceInc = state.dataEnvrn->BeamSolarRad *
                                          state.dataHeatBal->SunlitFrac(state.dataGlobal->TimeStep, state.dataGlobal->HourOfDay, SurfNum) * CosInc;
                            DifSolarFaceInc = SkySolarInc + GndSolarInc;
                        }
                        if (FracSunLit > 0.0) {
                            if ((FrArea > 0.0 && (FrProjOut > 0.0 || FrProjIn > 0.0)) || (DivArea > 0.0 && (DivProjOut > 0.0 || DivProjIn > 0.0))) {
                                // Dot products used to calculate beam solar incident on faces of
                                // frame and divider perpendicular to the glass surface.
                                // Note that SOLCOS is the current timestep's solar direction cosines.
                                //                  PhiWin = ASIN(WALCOS(3,SurfNum))
                                Real64 PhiWin =
                                    std::asin(Surface(SurfNum).OutNormVec(3)); // Altitude and azimuth angle of outward window normal (radians)
                                Real64 ThWin = std::atan2(Surface(SurfNum).OutNormVec(2), Surface(SurfNum).OutNormVec(1));
                                Real64 PhiSun = std::asin(state.dataEnvrn->SOLCOS(3)); // Altitude and azimuth angle of sun (radians)
                                Real64 ThSun = std::atan2(state.dataEnvrn->SOLCOS(2), state.dataEnvrn->SOLCOS(1));
                                Real64 const cos_PhiWin(std::cos(PhiWin));
                                Real64 const cos_PhiSun(std::cos(PhiSun));
                                CosIncAngHorProj = std::abs(std::sin(PhiWin) * cos_PhiSun * std::cos(ThWin - ThSun) - cos_PhiWin * std::sin(PhiSun));
                                CosIncAngVertProj = std::abs(cos_PhiWin * cos_PhiSun * std::sin(ThWin - ThSun));
                            }
                        }
                        // Frame solar
                        // (A window shade or blind, if present, is assumed to not shade the frame, so no special
                        // treatment of frame solar needed if window has an exterior shade or blind.)
                        if (FrArea > 0.0) {
                            Real64 FrIncSolarOut = BeamFaceInc; // Total solar incident on outside offrame including solar
                            Real64 FrIncSolarIn = 0.0;          // Total solar incident on inside offrame including solar on frame projection (W/m2)
                            Real64 TransDiffGl = 0.0;           // Diffuse solar transmittance
                            if (FrProjOut > 0.0 || FrProjIn > 0.0) {
                                Real64 BeamFrHorFaceInc =
                                    state.dataEnvrn->BeamSolarRad * CosIncAngHorProj *
                                    (Surface(SurfNum).Width - state.dataSurface->FrameDivider(FrDivNum).VertDividers * DivWidth) * FracSunLit /
                                    FrArea;
                                Real64 BeamFrVertFaceInc =
                                    state.dataEnvrn->BeamSolarRad * CosIncAngVertProj *
                                    (Surface(SurfNum).Height - state.dataSurface->FrameDivider(FrDivNum).HorDividers * DivWidth) * FracSunLit /
                                    FrArea;
                                // Beam solar on outside of frame
                                FrIncSolarOut += (BeamFrHorFaceInc + BeamFrVertFaceInc) * FrProjOut;
                                if (FrProjIn > 0.0) {
                                    Real64 TransGl = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).TransSolBeamCoef);
                                    TransDiffGl = state.dataConstruction->Construct(ConstrNum).TransDiff;
                                    if (ShadeFlag == WinShadingType::SwitchableGlazing) { // Switchable glazing
                                        Real64 SwitchFac = state.dataSurface->SurfWinSwitchingFactor(SurfNum);
                                        int ConstrNumSh = Surface(SurfNum).activeShadedConstruction;
                                        Real64 TransGlSh = POLYF(CosInc, state.dataConstruction->Construct(ConstrNumSh).TransSolBeamCoef);
                                        TransGl = InterpSw(SwitchFac, TransGl, TransGlSh);
                                        Real64 TransDiffGlSh = state.dataConstruction->Construct(ConstrNumSh).TransDiff;
                                        TransDiffGl = InterpSw(SwitchFac, TransDiffGl, TransDiffGlSh);
                                    }
                                    // Beam solar on inside of frame
                                    FrIncSolarIn = (BeamFrHorFaceInc + BeamFrVertFaceInc) * FrProjIn * TransGl;
                                }
                            }
                            // Beam plus diffuse solar on outside of frame
                            FrIncSolarOut += DifSolarFaceInc * (1.0 + 0.5 * state.dataSurface->SurfWinProjCorrFrOut(SurfNum));
                            state.dataSurface->SurfWinFrameQRadOutAbs(SurfNum) = FrIncSolarOut * state.dataSurface->SurfWinFrameSolAbsorp(SurfNum);
                            // Add diffuse from beam reflected from window outside reveal surfaces
                            state.dataSurface->SurfWinFrameQRadOutAbs(SurfNum) += state.dataEnvrn->BeamSolarRad *
                                                                                  state.dataSurface->SurfWinOutsRevealDiffOntoFrame(SurfNum) *
                                                                                  state.dataSurface->SurfWinFrameSolAbsorp(SurfNum);

                            // Beam plus diffuse solar on inside of frame
                            FrIncSolarIn += DifSolarFaceInc * TransDiffGl * 0.5 * state.dataSurface->SurfWinProjCorrFrIn(SurfNum);
                            state.dataSurface->SurfWinFrameQRadInAbs(SurfNum) = FrIncSolarIn * state.dataSurface->SurfWinFrameSolAbsorp(SurfNum);
                            // Add diffuse from beam reflected from window inside reveal surfaces
                            state.dataSurface->SurfWinFrameQRadInAbs(SurfNum) += state.dataEnvrn->BeamSolarRad *
                                                                                 state.dataSurface->SurfWinInsRevealDiffOntoFrame(SurfNum) *
                                                                                 state.dataSurface->SurfWinFrameSolAbsorp(SurfNum);
                        }

                        // Divider solar
                        // (An exterior shade or blind, when in place, is assumed to completely cover the divider.
                        // Dividers are not allowed on windows with between-glass shade/blind so DivProjOut and
                        // DivProjIn will be zero in this case.)
                        if (DivArea > 0.0) {                                                         // Solar absorbed by window divider
                            Real64 DividerAbs = state.dataSurface->SurfWinDividerSolAbsorp(SurfNum); // Window divider solar absorptance
                            if (state.dataSurface->SurfWinDividerType(SurfNum) == Suspended) {
                                // Suspended (between-glass) divider; account for effect glass on outside of divider
                                // (note that outside and inside projection for this type of divider are both zero)
                                int MatNumGl = state.dataConstruction->Construct(ConstrNum).LayerPoint(1); // Outer glass layer material number
                                Real64 TransGl =
                                    state.dataMaterial->Material(MatNumGl).Trans; // Outer glass layer material number, switched construction
                                Real64 ReflGl = state.dataMaterial->Material(MatNumGl).ReflectSolBeamFront;
                                Real64 AbsGl = 1.0 - TransGl - ReflGl;
                                Real64 SwitchFac = state.dataSurface->SurfWinSwitchingFactor(SurfNum);
                                int ConstrNumSh = Surface(SurfNum).activeShadedConstruction;
                                if (ShadeFlag == WinShadingType::SwitchableGlazing) { // Switchable glazing
                                    Real64 MatNumGlSh = state.dataConstruction->Construct(ConstrNumSh).LayerPoint(1);
                                    Real64 TransGlSh = state.dataMaterial->Material(MatNumGlSh).Trans;
                                    Real64 ReflGlSh = state.dataMaterial->Material(MatNumGlSh).ReflectSolBeamFront;
                                    Real64 AbsGlSh = 1.0 - TransGlSh - ReflGlSh;
                                    TransGl = InterpSw(SwitchFac, TransGl, TransGlSh);
                                    ReflGl = InterpSw(SwitchFac, ReflGl, ReflGlSh);
                                    AbsGl = InterpSw(SwitchFac, AbsGl, AbsGlSh);
                                }
                                Real64 DividerRefl = 1.0 - DividerAbs; // Window divider solar reflectance
                                DividerAbs = AbsGl + TransGl * (DividerAbs + DividerRefl * AbsGl) / (1.0 - DividerRefl * ReflGl);
                            }

                            Real64 BeamDivHorFaceInc = 0.0;  // Beam solar on divider's horizontal outside projection faces (W/m2)
                            Real64 BeamDivVertFaceInc = 0.0; // Beam solar on divider's vertical outside projection faces (W/m2)
                            // Beam incident on horizontal and vertical projection faces of divider if no exterior shading
                            if (DivProjOut > 0.0 && !ANY_EXTERIOR_SHADE_BLIND_SCREEN(ShadeFlag)) {
                                BeamDivHorFaceInc = state.dataEnvrn->BeamSolarRad * CosIncAngHorProj *
                                                    state.dataSurface->FrameDivider(FrDivNum).HorDividers * DivProjOut *
                                                    (Surface(SurfNum).Width - state.dataSurface->FrameDivider(FrDivNum).VertDividers * DivWidth) *
                                                    FracSunLit / DivArea;
                                BeamDivVertFaceInc = state.dataEnvrn->BeamSolarRad * CosIncAngVertProj *
                                                     state.dataSurface->FrameDivider(FrDivNum).VertDividers * DivProjOut *
                                                     (Surface(SurfNum).Height - state.dataSurface->FrameDivider(FrDivNum).HorDividers * DivWidth) *
                                                     FracSunLit / DivArea;
                            }
                            Real64 DivIncSolarOutBm = 0.0; // Diffuse solar incident on outside of divider including beam on divider projection (W/m2)
                            Real64 DivIncSolarOutDif =
                                0.0; // Diffuse solar incident on outside of divider including diffuse on divider projection (W/m2)
                            Real64 DivIncSolarInBm = 0.0; // Diffuse solar incident on inside of divider including beam on divider projection (W/m2)
                            Real64 DivIncSolarInDif =
                                0.0; // Diffuse solar incident on inside of divider including diffuse on divider projection (W/m2)
                            if (!ANY_EXTERIOR_SHADE_BLIND_SCREEN(ShadeFlag) &&
                                !ANY_BETWEENGLASS_SHADE_BLIND(ShadeFlag)) { // No exterior or between-glass shading
                                DivIncSolarOutBm = BeamFaceInc + BeamDivHorFaceInc + BeamDivVertFaceInc;
                                DivIncSolarOutDif = DifSolarFaceInc * (1.0 + state.dataSurface->SurfWinProjCorrDivOut(SurfNum));
                                if (DivProjIn > 0.0) {
                                    Real64 TransGl = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).TransSolBeamCoef);
                                    Real64 TransDiffGl = state.dataConstruction->Construct(ConstrNum).TransDiff; // Diffuse solar transmittance
                                    if (ShadeFlag == WinShadingType::SwitchableGlazing) {                        // Switchable glazing
                                        Real64 SwitchFac = state.dataSurface->SurfWinSwitchingFactor(SurfNum);
                                        int ConstrNumSh = Surface(SurfNum).activeShadedConstruction;
                                        Real64 TransGlSh = POLYF(CosInc, state.dataConstruction->Construct(ConstrNumSh).TransSolBeamCoef);
                                        // Outer glass solar trans, refl, absorptance if switched
                                        TransGl = InterpSw(SwitchFac, TransGl, TransGlSh);
                                        Real64 TransDiffGlSh = state.dataConstruction->Construct(ConstrNumSh).TransDiff;
                                        // Diffuse solar transmittance, switched construction
                                        TransDiffGl = InterpSw(SwitchFac, TransDiffGl, TransDiffGlSh);
                                    }
                                    // Beam plus diffuse solar on inside of divider
                                    // BeamDivHorFaceIncIn - Beam solar on divider's horizontal inside projection faces (W/m2)
                                    // BeamDivVertFaceIncIn - Beam solar on divider's vertical inside projection faces (W/m2)
                                    Real64 BeamDivHorFaceIncIn =
                                        state.dataEnvrn->BeamSolarRad * CosIncAngHorProj * state.dataSurface->FrameDivider(FrDivNum).HorDividers *
                                        DivProjIn * (Surface(SurfNum).Width - state.dataSurface->FrameDivider(FrDivNum).VertDividers * DivWidth) *
                                        FracSunLit / DivArea;
                                    Real64 BeamDivVertFaceIncIn =
                                        state.dataEnvrn->BeamSolarRad * CosIncAngVertProj * state.dataSurface->FrameDivider(FrDivNum).VertDividers *
                                        DivProjIn * (Surface(SurfNum).Height - state.dataSurface->FrameDivider(FrDivNum).HorDividers * DivWidth) *
                                        FracSunLit / DivArea;
                                    DivIncSolarInBm = TransGl * (BeamDivHorFaceIncIn + BeamDivVertFaceIncIn);
                                    DivIncSolarInDif = TransDiffGl * DifSolarFaceInc * state.dataSurface->SurfWinProjCorrDivIn(SurfNum);
                                }
                            } else { // Exterior shade, screen or blind present

                                DivIncSolarOutBm = BeamFaceInc * (1.0 + state.dataSurface->SurfWinProjCorrDivOut(SurfNum));
                                DivIncSolarOutDif = DifSolarFaceInc * (1.0 + state.dataSurface->SurfWinProjCorrDivOut(SurfNum));
                                DivIncSolarInBm = BeamFaceInc * state.dataSurface->SurfWinProjCorrDivIn(SurfNum) *
                                                  state.dataConstruction->Construct(ConstrNum).TransDiff;
                                DivIncSolarInDif = DifSolarFaceInc * state.dataSurface->SurfWinProjCorrDivIn(SurfNum) *
                                                   state.dataConstruction->Construct(ConstrNum).TransDiff;
                            }
                            if (!ANY_EXTERIOR_SHADE_BLIND_SCREEN(ShadeFlag) && !ANY_BETWEENGLASS_SHADE_BLIND(ShadeFlag)) {
                                // No exterior or between-glass shade, screen or blind
                                state.dataSurface->SurfWinDividerQRadOutAbs(SurfNum) = DividerAbs * (DivIncSolarOutBm + DivIncSolarOutDif);
                                state.dataSurface->SurfWinDividerQRadInAbs(SurfNum) = DividerAbs * (DivIncSolarInBm + DivIncSolarInDif);
                                // Exterior shade, screen or blind
                            } else if (ShadeFlag == WinShadingType::ExtBlind) { // Exterior blind
                                int BlNum = state.dataSurface->SurfWinBlindNumber(SurfNum);
                                int SlatsAngIndexLower = state.dataSurface->SurfWinSlatsAngIndex(SurfNum);
                                int SlatsAngIndexUpper = std::min(MaxProfAngs, SlatsAngIndexLower + 1);
                                Real64 SlatsAngInterpFac = state.dataSurface->SurfWinSlatsAngInterpFac(SurfNum);

                                Real64 FrontDiffTrans;
                                Real64 TBlBmDif; // Blind diffuse-diffuse solar transmittance
                                if (state.dataSurface->SurfWinMovableSlats(SurfNum)) {
                                    FrontDiffTrans = General::InterpGeneral(state.dataHeatBal->Blind(BlNum).SolFrontDiffDiffTrans(SlatsAngIndexLower),
                                                                            state.dataHeatBal->Blind(BlNum).SolFrontDiffDiffTrans(SlatsAngIndexUpper),
                                                                            SlatsAngInterpFac);
                                    TBlBmDif = General::InterpProfSlat(
                                        state.dataHeatBal->Blind(BlNum).SolFrontBeamDiffTrans(SlatsAngIndexLower,
                                                                                              state.dataSurface->SurfWinProfAngIndex(SurfNum)),
                                        state.dataHeatBal->Blind(BlNum).SolFrontBeamDiffTrans(SlatsAngIndexUpper,
                                                                                              state.dataSurface->SurfWinProfAngIndex(SurfNum)),
                                        state.dataHeatBal->Blind(BlNum).SolFrontBeamDiffTrans(
                                            SlatsAngIndexLower, std::min(MaxProfAngs, state.dataSurface->SurfWinProfAngIndex(SurfNum) + 1)),
                                        state.dataHeatBal->Blind(BlNum).SolFrontBeamDiffTrans(
                                            SlatsAngIndexUpper, std::min(MaxProfAngs, state.dataSurface->SurfWinProfAngIndex(SurfNum) + 1)),
                                        SlatsAngInterpFac,
                                        state.dataSurface->SurfWinProfAngInterpFac(SurfNum));
                                } else {
                                    FrontDiffTrans = state.dataHeatBal->Blind(BlNum).SolFrontDiffDiffTrans(1);
                                    TBlBmDif = General::InterpGeneral(
                                        state.dataHeatBal->Blind(BlNum).SolFrontBeamDiffTrans(1, state.dataSurface->SurfWinProfAngIndex(SurfNum)),
                                        state.dataHeatBal->Blind(BlNum).SolFrontBeamDiffTrans(
                                            1, std::min(MaxProfAngs, state.dataSurface->SurfWinProfAngIndex(SurfNum) + 1)),
                                        SlatsAngInterpFac);
                                }

                                // TBlBmBm - Blind beam-beam solar transmittance
                                Real64 TBlBmBm = state.dataSurface->SurfWinBlindBmBmTrans(SurfNum);
                                state.dataSurface->SurfWinDividerQRadOutAbs(SurfNum) =
                                    DividerAbs * (DivIncSolarOutBm * (TBlBmBm + TBlBmDif) + DivIncSolarOutDif * FrontDiffTrans);
                                state.dataSurface->SurfWinDividerQRadInAbs(SurfNum) =
                                    DividerAbs * (DivIncSolarInBm * (TBlBmBm + TBlBmDif) + DivIncSolarInDif * FrontDiffTrans);

                            } else if (ShadeFlag == WinShadingType::ExtShade) { // Exterior shade
                                int ConstrNumSh = Surface(SurfNum).activeShadedConstruction;
                                state.dataSurface->SurfWinDividerQRadOutAbs(SurfNum) =
                                    DividerAbs * state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNumSh).LayerPoint(1)).Trans *
                                    (DivIncSolarOutBm + DivIncSolarOutDif);
                                state.dataSurface->SurfWinDividerQRadInAbs(SurfNum) =
                                    DividerAbs * state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNumSh).LayerPoint(1)).Trans *
                                    (DivIncSolarInBm + DivIncSolarInDif);

                            } else if (ShadeFlag == WinShadingType::ExtScreen) { // Exterior screen
                                state.dataSurface->SurfWinDividerQRadOutAbs(SurfNum) =
                                    DividerAbs *
                                    (state.dataHeatBal->SurfaceScreens(state.dataSurface->SurfWinScreenNumber(SurfNum)).BmBmTrans +
                                     state.dataHeatBal->SurfaceScreens(state.dataSurface->SurfWinScreenNumber(SurfNum)).BmDifTrans) *
                                    (DivIncSolarOutBm + DivIncSolarOutDif);
                                state.dataSurface->SurfWinDividerQRadInAbs(SurfNum) =
                                    DividerAbs *
                                    (state.dataHeatBal->SurfaceScreens(state.dataSurface->SurfWinScreenNumber(SurfNum)).BmBmTrans +
                                     state.dataHeatBal->SurfaceScreens(state.dataSurface->SurfWinScreenNumber(SurfNum)).BmDifTrans) *
                                    (DivIncSolarInBm + DivIncSolarInDif);
                            }
                        }
                    }
                } // Surface(SurfNum)%ExtSolar
            }     // end of surface window loop
        }         // end of zone loop
        for (int PipeNum = 1; PipeNum <= state.dataDaylightingDevicesData->NumOfTDDPipes; ++PipeNum) {
            int const SurfNum = state.dataDaylightingDevicesData->TDDPipe(PipeNum).Dome; // TDD: DOME object number
            int const ConstrNum = Surface(SurfNum).Construction;
            int const TotGlassLay = state.dataConstruction->Construct(ConstrNum).TotGlassLayers; // Number of glass layers
            state.dataHeatBal->SurfWinQRadSWwinAbsTot(SurfNum) = 0.0;
            for (int Lay = 1; Lay <= TotGlassLay; ++Lay) {
                AbsDiffWin(Lay) = state.dataConstruction->Construct(ConstrNum).AbsDiff(Lay);
                state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum, Lay) = AbsDiffWin(Lay) * (currSkySolarInc(SurfNum) + currGndSolarInc(SurfNum)) +
                                                                       state.dataSurface->SurfWinA(SurfNum, Lay) * currBeamSolar(SurfNum);
                state.dataHeatBal->SurfWinQRadSWwinAbsLayer(SurfNum, Lay) =
                    state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum, Lay) * Surface(SurfNum).Area;
                state.dataHeatBal->SurfWinQRadSWwinAbsTot(SurfNum) += state.dataHeatBal->SurfWinQRadSWwinAbsLayer(SurfNum, Lay);
                state.dataHeatBal->SurfWinQRadSWwinAbsTotEnergy(SurfNum) =
                    state.dataHeatBal->SurfWinQRadSWwinAbsTot(SurfNum) * state.dataGlobal->TimeStepZoneSec;
            }
        }
    } // End of sun-up check
}

void InitIntSolarDistribution(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Anonymous
    //       DATE WRITTEN   July 1977
    //       MODIFIED       Oct 1999 (FW) to handle movable shades
    //                      May 2000 (FW) to handle window frame and dividers
    //                      May 2001 (FW) to handle window blinds
    //                      Jan 2002 (FW) mods for between-glass shade/blind
    //                      May 2006 (RR) to handle exterior window screens
    //       RE-ENGINEERED  Mar98 (RKS)

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine initializes the arrays associated with solar heat
    // gains for both individual surfaces and for zones.

    // METHODOLOGY EMPLOYED:
    // If the sun is down, all of the pertinent arrays are zeroed.  If the
    // sun is up, various calculations are made.

    // REFERENCES:
    // (I)BLAST legacy routine QSUN

    // Using/Aliasing
    using DaylightingDevices::DistributeTDDAbsorbedSolar;
    using General::InterpSw;
    using namespace DataWindowEquivalentLayer;
    // Locals
    // SUBROUTINE PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS:
    // na

    // DERIVED TYPE DEFINITIONS:
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    auto &Surface(state.dataSurface->Surface);

    // COMPUTE TOTAL SHORT-WAVE RADIATION ORIGINATING IN ZONE.
    // Note: If sun is not up, QS is only internal gains
    for (int enclosureNum = 1; enclosureNum <= state.dataViewFactor->NumOfSolarEnclosures; ++enclosureNum) {
        Real64 sumZoneQLTSW = 0.0;
        for (int zoneNum : state.dataViewFactor->ZoneSolarInfo(enclosureNum).ZoneNums) {
            sumZoneQLTSW += state.dataHeatBal->ZoneIntGain(zoneNum).QLTSW;
        }
        state.dataHeatBal->EnclSolQSWRad(enclosureNum) = state.dataHeatBal->EnclSolQD(enclosureNum) + sumZoneQLTSW;
        state.dataHeatBal->EnclSolQSWRadLights(enclosureNum) = sumZoneQLTSW;
    }

    if (state.dataHeatBalSurf->InterZoneWindow) { // DO INTERZONE DISTRIBUTION.

        for (int enclosureNum = 1; enclosureNum <= state.dataViewFactor->NumOfSolarEnclosures; ++enclosureNum) {

            if (state.dataHeatBalSurf->RecDifShortFromZ(enclosureNum)) {

                for (int OtherenclosureNum = 1; OtherenclosureNum <= state.dataViewFactor->NumOfSolarEnclosures; ++OtherenclosureNum) {

                    if ((OtherenclosureNum != enclosureNum) && (state.dataHeatBalSurf->RecDifShortFromZ(OtherenclosureNum))) {
                        Real64 sumZoneQLTSW = 0.0;
                        for (int zoneNum : state.dataViewFactor->ZoneSolarInfo(OtherenclosureNum).ZoneNums) {
                            sumZoneQLTSW += state.dataHeatBal->ZoneIntGain(zoneNum).QLTSW;
                        }
                        state.dataHeatBal->EnclSolQSWRad(enclosureNum) += state.dataHeatBalSurf->FractDifShortZtoZ(enclosureNum, OtherenclosureNum) *
                                                                          (state.dataHeatBal->EnclSolQD(OtherenclosureNum) + sumZoneQLTSW);
                        state.dataHeatBal->ZoneDifSolFrIntWinsRep(enclosureNum) +=
                            state.dataHeatBalSurf->FractDifShortZtoZ(enclosureNum, OtherenclosureNum) *
                            state.dataHeatBal->EnclSolQD(OtherenclosureNum);
                        state.dataHeatBal->ZoneDifSolFrIntWinsRepEnergy(enclosureNum) =
                            state.dataHeatBal->ZoneDifSolFrIntWinsRep(enclosureNum) * state.dataGlobal->TimeStepZoneSec;
                    }
                }
            }
        }
    }

    // Beam and diffuse solar on inside surfaces from interior windows (for reporting)

    if (state.dataEnvrn->SunIsUp) {
        for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
            if (!Surface(SurfNum).HeatTransSurf) continue;
            //!!! Following may need to be removed or changed when shelves are considered in adjacent reflection calculations
            if (Surface(SurfNum).Class == SurfaceClass::Shading) continue;
            int const enclosureNum = Surface(SurfNum).SolarEnclIndex;
            state.dataHeatBal->SurfIntBmIncInsSurfIntensRep(SurfNum) =
                state.dataHeatBal->ZoneBmSolFrIntWinsRep(enclosureNum) / state.dataViewFactor->ZoneSolarInfo(enclosureNum).TotalSurfArea;
            state.dataHeatBal->SurfIntBmIncInsSurfAmountRep(SurfNum) =
                state.dataHeatBal->SurfIntBmIncInsSurfIntensRep(SurfNum) * (Surface(SurfNum).Area + state.dataSurface->SurfWinDividerArea(SurfNum));
            state.dataHeatBal->SurfIntBmIncInsSurfAmountRepEnergy(SurfNum) =
                state.dataHeatBal->SurfIntBmIncInsSurfAmountRep(SurfNum) * state.dataGlobal->TimeStepZoneSec;
            //      IntDifIncInsSurfIntensRep(SurfNum) = ZoneDifSolFrIntWinsRep(ZoneNum)/Zone(ZoneNum)%TotalSurfArea
            //      IntDifIncInsSurfAmountRep(SurfNum) = IntDifIncInsSurfIntensRep(SurfNum) *  &
            //                                             (Surface(SurfNum)%Area + SurfaceWindow(SurfNum)%DividerArea)
            //      IntDifIncInsSurfAmountRepEnergy(SurfNum) = IntDifIncInsSurfAmountRep(SurfNum) * TimeStepZoneSec
        }
    }

    // COMPUTE CONVECTIVE GAINS AND ZONE FLUX DENSITY.
    for (int enclosureNum = 1; enclosureNum <= state.dataViewFactor->NumOfSolarEnclosures; ++enclosureNum) {
        if (state.dataHeatBalSurf->InterZoneWindow) {
            state.dataHeatBal->EnclSolQSWRad(enclosureNum) *=
                state.dataHeatBalSurf->FractDifShortZtoZ(enclosureNum, enclosureNum) * state.dataHeatBal->EnclSolVMULT(enclosureNum);
            // CR 8695, VMULT not based on visible
            state.dataHeatBal->EnclSolQSWRadLights(enclosureNum) *=
                state.dataHeatBalSurf->FractDifShortZtoZ(enclosureNum, enclosureNum) * state.dataHeatBal->EnclSolVMULT(enclosureNum);
        } else {
            state.dataHeatBal->EnclSolQSWRad(enclosureNum) *= state.dataHeatBal->EnclSolVMULT(enclosureNum);
            state.dataHeatBal->EnclSolQSWRadLights(enclosureNum) *= state.dataHeatBal->EnclSolVMULT(enclosureNum);
        }
    }

    // COMPUTE RADIANT GAINS ON SURFACES
    for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
        int const firstSurfOpaque = state.dataHeatBal->Zone(zoneNum).OpaqOrIntMassSurfaceFirst;
        int const lastSurfOpaque = state.dataHeatBal->Zone(zoneNum).OpaqOrIntMassSurfaceLast;
        int const radEnclosureNum = state.dataHeatBal->Zone(zoneNum).RadiantEnclosureNum;
        int const solEnclosureNum = state.dataHeatBal->Zone(zoneNum).SolarEnclosureNum;
        for (int SurfNum = firstSurfOpaque; SurfNum <= lastSurfOpaque; ++SurfNum) {
            int const ConstrNum = Surface(SurfNum).Construction;

            Real64 AbsIntSurf = state.dataHeatBalSurf->SurfAbsSolarInt(SurfNum);
            // TODO - AbsIntSurfVis = InsideAbsorpSolar instead of InsideAbsorpVis?
            Real64 AbsIntSurfVis =
                state.dataConstruction->Construct(ConstrNum).InsideAbsorpSolar; // Inside opaque surface visible absorptance to fix CR 8695 change to

            state.dataHeatBalSurf->SurfOpaqQRadSWInAbs(SurfNum) += state.dataHeatBal->EnclSolQSWRad(solEnclosureNum) * AbsIntSurf;
            state.dataHeatBalSurf->SurfOpaqQRadSWLightsInAbs(SurfNum) += state.dataHeatBal->EnclSolQSWRadLights(solEnclosureNum) * AbsIntSurfVis;

            // Calculate absorbed solar on outside if movable exterior insulation in place
            if (state.dataSurface->AnyMovableInsulation &&
                state.dataHeatBalSurf->SurfMovInsulExtPresent(SurfNum)) { // Movable outside insulation in place
                Real64 AbsExt = state.dataHeatBalSurf->SurfAbsSolarExt(SurfNum);
                state.dataHeatBalSurf->SurfQRadSWOutMvIns(SurfNum) =
                    state.dataHeatBalSurf->SurfOpaqQRadSWOutAbs(SurfNum) * AbsExt /
                    state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).AbsorpSolar;
                // For transparent insulation, allow some sunlight to get through the movable insulation.
                // The equation below is derived by taking what is transmitted through the layer and applying
                // the fraction that is absorbed plus the back reflected portion (first order reflection only)
                // to the plane between the transparent insulation and the exterior surface face.
                state.dataHeatBalSurf->SurfOpaqQRadSWOutAbs(SurfNum) =
                    state.dataMaterial->Material(state.dataSurface->SurfMaterialMovInsulExt(SurfNum)).Trans *
                    state.dataHeatBalSurf->SurfQRadSWOutMvIns(SurfNum) *
                    ((state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).AbsorpSolar / AbsExt) +
                     (1 - state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).AbsorpSolar));
                state.dataHeatBal->SurfOpaqSWOutAbsTotalReport(SurfNum) =
                    state.dataHeatBalSurf->SurfOpaqQRadSWOutAbs(SurfNum) * Surface(SurfNum).Area;
                state.dataHeatBal->SurfOpaqSWOutAbsEnergyReport(SurfNum) =
                    state.dataHeatBal->SurfOpaqSWOutAbsTotalReport(SurfNum) * state.dataGlobal->TimeStepZoneSec;
            }
            // RJH 08/30/07 - Add InitialDifSolInAbs, InitialDifSolwinAbs, and InitialDifSolAbsByShade
            // calced in CalcWinTransDifSolInitialDistribution to QRadSWInAbs, QRadSWwinAbs, and IntSWAbsByShade here
            state.dataHeatBalSurf->SurfOpaqQRadSWInAbs(SurfNum) += state.dataHeatBalSurf->SurfOpaqInitialDifSolInAbs(SurfNum);
            // Initial Transmitted Diffuse Solar Absorbed on Inside of Surface[W]
            state.dataHeatBal->SurfInitialDifSolInAbsReport(SurfNum) =
                state.dataHeatBalSurf->SurfOpaqInitialDifSolInAbs(SurfNum) * Surface(SurfNum).Area;
            // Total Shortwave Radiation Absorbed on Inside of Surface[W]
            state.dataHeatBal->SurfSWInAbsTotalReport(SurfNum) = state.dataHeatBalSurf->SurfOpaqQRadSWInAbs(SurfNum) * Surface(SurfNum).Area;

        } // end of opaque

        int const firstSurfWin = state.dataHeatBal->Zone(zoneNum).WindowSurfaceFirst;
        int const lastSurfWin = state.dataHeatBal->Zone(zoneNum).WindowSurfaceLast;
        for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) { // Window
            int const ConstrNum = state.dataSurface->SurfActiveConstruction(SurfNum);

            if (state.dataSurface->SurfWinWindowModelType(SurfNum) != WindowEQLModel) {
                int const ConstrNumSh = state.dataSurface->SurfWinActiveShadedConstruction(SurfNum);

                int TotGlassLayers = state.dataConstruction->Construct(ConstrNum).TotGlassLayers;
                WinShadingType ShadeFlag = state.dataSurface->SurfWinShadingFlag(SurfNum);

                // These calculations are repeated from InitInternalHeatGains for the Zone Component Loads Report
                Real64 pulseMultipler = 0.01; // use to create a pulse for the load component report computations, the W/sqft pulse for the zone
                if (!state.dataGlobal->doLoadComponentPulseNow) {
                    state.dataHeatBal->SurfQRadThermInAbs(SurfNum) = state.dataHeatBal->EnclRadQThermalRad(radEnclosureNum) *
                                                                     state.dataHeatBal->TMULT(radEnclosureNum) * state.dataHeatBal->ITABSF(SurfNum);
                } else {
                    state.dataHeatBalSurfMgr->curQL = state.dataHeatBal->EnclRadQThermalRad(radEnclosureNum);
                    // for the loads component report during the special sizing run increase the radiant portion
                    // a small amount to create a "pulse" of heat that is used for the
                    state.dataHeatBalSurfMgr->adjQL =
                        state.dataHeatBalSurfMgr->curQL + state.dataViewFactor->ZoneRadiantInfo(radEnclosureNum).FloorArea * pulseMultipler;
                    // ITABSF is the Inside Thermal Absorptance
                    // TMULT is a multiplier for each zone/enclosure
                    // QRadThermInAbs is the thermal radiation absorbed on inside surfaces
                    state.dataHeatBal->SurfQRadThermInAbs(SurfNum) =
                        state.dataHeatBalSurfMgr->adjQL * state.dataHeatBal->TMULT(radEnclosureNum) * state.dataHeatBal->ITABSF(SurfNum);
                }

                if (NOT_SHADED(ShadeFlag)) { // No window shading
                    for (int IGlass = 1; IGlass <= TotGlassLayers; ++IGlass) {
                        state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum, IGlass) +=
                            state.dataHeatBal->EnclSolQSWRad(solEnclosureNum) * state.dataConstruction->Construct(ConstrNum).AbsDiffBack(IGlass);
                    }
                } else if (ConstrNumSh != 0 && ShadeFlag != WinShadingType::SwitchableGlazing) {
                    // Interior, exterior or between-glass shade, screen or blind in place
                    for (int IGlass = 1; IGlass <= state.dataConstruction->Construct(ConstrNumSh).TotGlassLayers; ++IGlass) {
                        if (ANY_SHADE_SCREEN(ShadeFlag)) {
                            state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum, IGlass) +=
                                state.dataHeatBal->EnclSolQSWRad(solEnclosureNum) *
                                state.dataConstruction->Construct(ConstrNumSh).AbsDiffBack(IGlass);
                        } else if (BITF_TEST_ANY(BITF(ShadeFlag), BITF(WinShadingType::IntBlind) | BITF(WinShadingType::ExtBlind))) {
                            Real64 BlAbsDiffBk; // Glass layer back diffuse solar absorptance when blind in place
                            if (state.dataSurface->SurfWinMovableSlats(SurfNum)) {
                                BlAbsDiffBk = General::InterpGeneral(
                                    state.dataConstruction->Construct(ConstrNumSh)
                                        .BlAbsDiffBack(state.dataSurface->SurfWinSlatsAngIndex(SurfNum), IGlass),
                                    state.dataConstruction->Construct(ConstrNumSh)
                                        .BlAbsDiffBack(std::min(MaxSlatAngs, state.dataSurface->SurfWinSlatsAngIndex(SurfNum) + 1), IGlass),
                                    state.dataSurface->SurfWinSlatsAngInterpFac(SurfNum));
                            } else {
                                BlAbsDiffBk = state.dataConstruction->Construct(ConstrNumSh).BlAbsDiffBack(1, IGlass);
                            }
                            state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum, IGlass) +=
                                state.dataHeatBal->EnclSolQSWRad(solEnclosureNum) * BlAbsDiffBk;
                        }
                    }
                    if (ShadeFlag == WinShadingType::IntShade) {
                        state.dataSurface->SurfWinIntLWAbsByShade(SurfNum) = state.dataHeatBal->EnclRadQThermalRad(radEnclosureNum) *
                                                                             state.dataConstruction->Construct(ConstrNumSh).ShadeAbsorpThermal *
                                                                             state.dataHeatBal->TMULT(radEnclosureNum);
                    } else if (ShadeFlag == WinShadingType::IntBlind) {
                        Real64 EffBlEmiss; // Blind emissivity (thermal absorptance) as part of glazing system
                        if (state.dataSurface->SurfWinMovableSlats(SurfNum)) {
                            EffBlEmiss = General::InterpGeneral(
                                state.dataSurface->SurfaceWindow(SurfNum).EffShBlindEmiss(state.dataSurface->SurfWinSlatsAngIndex(SurfNum)),
                                state.dataSurface->SurfaceWindow(SurfNum).EffShBlindEmiss(
                                    std::min(MaxSlatAngs, state.dataSurface->SurfWinSlatsAngIndex(SurfNum) + 1)),
                                state.dataSurface->SurfWinSlatsAngInterpFac(SurfNum));
                        } else {
                            EffBlEmiss = state.dataSurface->SurfaceWindow(SurfNum).EffShBlindEmiss(1);
                        }
                        state.dataSurface->SurfWinIntLWAbsByShade(SurfNum) =
                            state.dataHeatBal->EnclRadQThermalRad(radEnclosureNum) * EffBlEmiss * state.dataHeatBal->TMULT(radEnclosureNum);
                    }
                    if (ANY_SHADE_SCREEN(ShadeFlag)) {
                        state.dataSurface->SurfWinIntSWAbsByShade(SurfNum) =
                            state.dataHeatBal->EnclSolQSWRad(solEnclosureNum) * state.dataConstruction->Construct(ConstrNumSh).AbsDiffBackShade;
                    } else if (ANY_BLIND(ShadeFlag)) {
                        Real64 AbsDiffBkBl;
                        if (state.dataSurface->SurfWinMovableSlats(SurfNum)) {
                            AbsDiffBkBl = General::InterpGeneral(
                                state.dataConstruction->Construct(ConstrNumSh).AbsDiffBackBlind(state.dataSurface->SurfWinSlatsAngIndex(SurfNum)),
                                state.dataConstruction->Construct(ConstrNumSh)
                                    .AbsDiffBackBlind(std::min(MaxSlatAngs, state.dataSurface->SurfWinSlatsAngIndex(SurfNum) + 1)),
                                state.dataSurface->SurfWinSlatsAngInterpFac(SurfNum));
                        } else {
                            AbsDiffBkBl = state.dataConstruction->Construct(ConstrNumSh).AbsDiffBackBlind(1);
                        }
                        state.dataSurface->SurfWinIntSWAbsByShade(SurfNum) = state.dataHeatBal->EnclSolQSWRad(solEnclosureNum) * AbsDiffBkBl;
                    }
                    // Correct for divider shadowing
                    if (ANY_EXTERIOR_SHADE_BLIND_SCREEN(ShadeFlag)) {
                        state.dataSurface->SurfWinIntSWAbsByShade(SurfNum) *= state.dataSurface->SurfWinGlazedFrac(SurfNum);
                    }

                } else if (ShadeFlag == WinShadingType::SwitchableGlazing) { // Switchable glazing
                    for (int IGlass = 1; IGlass <= TotGlassLayers; ++IGlass) {

                        state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum, IGlass) +=
                            state.dataHeatBal->EnclSolQSWRad(solEnclosureNum) *
                            InterpSw(state.dataSurface->SurfWinSwitchingFactor(SurfNum),
                                     state.dataConstruction->Construct(ConstrNum).AbsDiffBack(IGlass),
                                     state.dataConstruction->Construct(ConstrNumSh).AbsDiffBack(IGlass));
                    }

                } // End of shading flag check

                // Note that FrameQRadInAbs is initially calculated in InitSolarHeatGains
                if (state.dataSurface->SurfWinFrameArea(SurfNum) > 0.0)
                    state.dataSurface->SurfWinFrameQRadInAbs(SurfNum) +=
                        (state.dataHeatBal->EnclSolQSWRad(solEnclosureNum) * state.dataSurface->SurfWinFrameSolAbsorp(SurfNum) +
                         (state.dataHeatBal->EnclRadQThermalRad(radEnclosureNum) * state.dataHeatBal->TMULT(radEnclosureNum) +
                          state.dataHeatBalFanSys->QHTRadSysSurf(SurfNum) + state.dataHeatBalFanSys->QCoolingPanelSurf(SurfNum) +
                          state.dataHeatBalFanSys->QHWBaseboardSurf(SurfNum) + state.dataHeatBalFanSys->QSteamBaseboardSurf(SurfNum) +
                          state.dataHeatBalFanSys->QElecBaseboardSurf(SurfNum)) *
                             state.dataSurface->SurfWinFrameEmis(SurfNum)) *
                        (1.0 + 0.5 * state.dataSurface->SurfWinProjCorrFrIn(SurfNum));          // Window has a frame
                if (state.dataSurface->SurfWinDividerArea(SurfNum) > 0.0) {                     // Window has dividers
                    Real64 DividerThermAbs = state.dataSurface->SurfWinDividerEmis(SurfNum);    // Window divider thermal absorptance
                    Real64 DividerSolAbs = state.dataSurface->SurfWinDividerSolAbsorp(SurfNum); // Window divider solar absorptance
                    if (state.dataSurface->SurfWinDividerType(SurfNum) == Suspended) {          // Suspended divider; account for inside glass
                        Real64 MatNumGl = state.dataConstruction->Construct(ConstrNum).LayerPoint(
                            state.dataConstruction->Construct(ConstrNum).TotLayers);   // Glass layer material number
                        Real64 TransGl = state.dataMaterial->Material(MatNumGl).Trans; // Glass layer solar transmittance, reflectance, absorptance
                        Real64 ReflGl = state.dataMaterial->Material(MatNumGl).ReflectSolBeamBack;
                        Real64 AbsGl = 1.0 - TransGl - ReflGl;
                        Real64 DividerSolRefl = 1.0 - DividerSolAbs; // Window divider solar reflectance
                        DividerSolAbs = AbsGl + TransGl * (DividerSolAbs + DividerSolRefl * AbsGl) / (1.0 - DividerSolRefl * ReflGl);
                        DividerThermAbs = state.dataMaterial->Material(MatNumGl).AbsorpThermalBack;
                    }
                    // Correct for interior shade transmittance
                    if (ShadeFlag == WinShadingType::IntShade) {
                        int MatNumSh = state.dataConstruction->Construct(ConstrNumSh)
                                           .LayerPoint(state.dataConstruction->Construct(ConstrNumSh).TotLayers); // Shade layer material number
                        DividerSolAbs *= state.dataMaterial->Material(MatNumSh).Trans;
                        DividerThermAbs *= state.dataMaterial->Material(MatNumSh).TransThermal;
                    } else if (ShadeFlag == WinShadingType::IntBlind) {
                        int BlNum = state.dataSurface->SurfWinBlindNumber(SurfNum);
                        Real64 SolBackDiffDiffTrans;
                        Real64 IRBackTrans;
                        if (state.dataSurface->SurfWinMovableSlats(SurfNum)) {
                            int SurfWinSlatsAngIndex = state.dataSurface->SurfWinSlatsAngIndex(SurfNum);
                            Real64 SurfWinSlatsAngInterpFac = state.dataSurface->SurfWinSlatsAngInterpFac(SurfNum);
                            SolBackDiffDiffTrans = General::InterpGeneral(
                                state.dataHeatBal->Blind(BlNum).SolBackDiffDiffTrans(SurfWinSlatsAngIndex),
                                state.dataHeatBal->Blind(BlNum).SolBackDiffDiffTrans(std::min(MaxSlatAngs, SurfWinSlatsAngIndex + 1)),
                                SurfWinSlatsAngInterpFac);
                            IRBackTrans =
                                General::InterpGeneral(state.dataHeatBal->Blind(BlNum).IRBackTrans(SurfWinSlatsAngIndex),
                                                       state.dataHeatBal->Blind(BlNum).IRBackTrans(std::min(MaxSlatAngs, SurfWinSlatsAngIndex + 1)),
                                                       SurfWinSlatsAngInterpFac);
                        } else {
                            SolBackDiffDiffTrans = state.dataHeatBal->Blind(BlNum).SolBackDiffDiffTrans(1);
                            IRBackTrans = state.dataHeatBal->Blind(BlNum).IRBackTrans(1);
                        }
                        DividerSolAbs *= SolBackDiffDiffTrans;
                        DividerThermAbs *= IRBackTrans;
                    }
                    // Note that DividerQRadInAbs is initially calculated in InitSolarHeatGains
                    state.dataSurface->SurfWinDividerQRadInAbs(SurfNum) +=
                        (state.dataHeatBal->EnclSolQSWRad(solEnclosureNum) * DividerSolAbs +
                         (state.dataHeatBal->EnclRadQThermalRad(radEnclosureNum) * state.dataHeatBal->TMULT(radEnclosureNum) +
                          state.dataHeatBalFanSys->QHTRadSysSurf(SurfNum) + state.dataHeatBalFanSys->QCoolingPanelSurf(SurfNum) +
                          state.dataHeatBalFanSys->QHWBaseboardSurf(SurfNum) + state.dataHeatBalFanSys->QSteamBaseboardSurf(SurfNum) +
                          state.dataHeatBalFanSys->QElecBaseboardSurf(SurfNum)) *
                             DividerThermAbs) *
                        (1.0 + state.dataSurface->SurfWinProjCorrDivIn(SurfNum));
                }

            } else {
                // These calculations are repeated from InitInternalHeatGains for the Zone Component Loads Report
                Real64 pulseMultipler = 0.01; // the W/sqft pulse for the zone
                if (!state.dataGlobal->doLoadComponentPulseNow) {
                    state.dataHeatBal->SurfQRadThermInAbs(SurfNum) = state.dataHeatBal->EnclRadQThermalRad(radEnclosureNum) *
                                                                     state.dataHeatBal->TMULT(radEnclosureNum) * state.dataHeatBal->ITABSF(SurfNum);
                } else {
                    state.dataHeatBalSurfMgr->curQL = state.dataHeatBal->EnclRadQThermalRad(radEnclosureNum);
                    // for the loads component report during the special sizing run increase the radiant portion
                    // a small amount to create a "pulse" of heat that is used for the
                    state.dataHeatBalSurfMgr->adjQL =
                        state.dataHeatBalSurfMgr->curQL + state.dataViewFactor->ZoneRadiantInfo(radEnclosureNum).FloorArea * pulseMultipler;
                    // ITABSF is the Inside Thermal Absorptance
                    // TMULT is a multiplier for each zone/radiant enclosure
                    // QRadThermInAbs is the thermal radiation absorbed on inside surfaces
                    state.dataHeatBal->SurfQRadThermInAbs(SurfNum) =
                        state.dataHeatBalSurfMgr->adjQL * state.dataHeatBal->TMULT(radEnclosureNum) * state.dataHeatBal->ITABSF(SurfNum);
                }
                // Radiations absorbed by the window layers coming from zone side
                int EQLNum = state.dataConstruction->Construct(ConstrNum).EQLConsPtr;
                for (int Lay = 1; Lay <= state.dataWindowEquivLayer->CFS(EQLNum).NL; ++Lay) {
                    state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum, Lay) +=
                        state.dataHeatBal->EnclSolQSWRad(solEnclosureNum) * state.dataConstruction->Construct(ConstrNum).AbsDiffBackEQL(Lay);
                }
                // Window frame has not been included for equivalent layer model yet

            } // end if for IF ( SurfaceWindow(SurfNum)%WindowModelType /= WindowEQLModel) THEN

            if (Surface(SurfNum).ExtBoundCond > 0) { // Interzone surface
                // Short-wave radiation absorbed in panes of corresponding window in adjacent zone
                int SurfNumAdjZone = Surface(SurfNum).ExtBoundCond; // Surface number in adjacent zone for interzone surfaces
                if (state.dataSurface->SurfWinWindowModelType(SurfNumAdjZone) != WindowEQLModel) {
                    int TotGlassLayers = state.dataConstruction->Construct(ConstrNum).TotGlassLayers;
                    for (int IGlass = 1; IGlass <= TotGlassLayers; ++IGlass) {
                        state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNumAdjZone, IGlass) +=
                            state.dataHeatBal->EnclSolQSWRad(solEnclosureNum) *
                            state.dataConstruction->Construct(Surface(SurfNumAdjZone).Construction).AbsDiff(IGlass);
                        // Note that AbsDiff rather than AbsDiffBack is used in the above since the
                        // radiation from the current zone is incident on the outside of the adjacent
                        // zone's window.
                    }
                } else { // IF (SurfaceWindow(SurfNumAdjZone)%WindowModelType == WindowEQLModel) THEN
                    int const AdjConstrNum = Surface(SurfNumAdjZone).Construction;
                    int const EQLNum = state.dataConstruction->Construct(AdjConstrNum).EQLConsPtr;
                    for (int Lay = 1; Lay <= state.dataWindowEquivLayer->CFS(EQLNum).NL; ++Lay) {
                        state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNumAdjZone, Lay) +=
                            state.dataHeatBal->EnclSolQSWRad(solEnclosureNum) * state.dataConstruction->Construct(ConstrNum).AbsDiffFrontEQL(Lay);
                        // Note that AbsDiffFrontEQL rather than AbsDiffBackEQL is used in the above
                        // since the radiation from the current zone is incident on the outside of the
                        // adjacent zone's window.
                    }
                }
            }

            if (state.dataSurface->SurfWinWindowModelType(SurfNum) == Window5DetailedModel) {
                int const ConstrNumSh = state.dataSurface->SurfWinActiveShadedConstruction(SurfNum);
                int TotGlassLayers = state.dataConstruction->Construct(ConstrNum).TotGlassLayers;
                WinShadingType ShadeFlag = state.dataSurface->SurfWinShadingFlag(SurfNum);
                if (NOT_SHADED(ShadeFlag)) { // No window shading
                    for (int IGlass = 1; IGlass <= TotGlassLayers; ++IGlass) {
                        state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum, IGlass) += state.dataHeatBal->SurfWinInitialDifSolwinAbs(SurfNum, IGlass);
                    }
                } else if (ShadeFlag == WinShadingType::SwitchableGlazing) { // Switchable glazing
                    for (int IGlass = 1; IGlass <= TotGlassLayers; ++IGlass) {
                        state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum, IGlass) += state.dataHeatBal->SurfWinInitialDifSolwinAbs(SurfNum, IGlass);
                    }
                } else {
                    // Interior, exterior or between-glass shade, screen or blind in place
                    for (int IGlass = 1; IGlass <= state.dataConstruction->Construct(ConstrNumSh).TotGlassLayers; ++IGlass) {
                        state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum, IGlass) += state.dataHeatBal->SurfWinInitialDifSolwinAbs(SurfNum, IGlass);
                    }
                    if (ANY_SHADE_SCREEN(ShadeFlag)) {
                        state.dataSurface->SurfWinIntSWAbsByShade(SurfNum) += state.dataSurface->SurfWinInitialDifSolAbsByShade(SurfNum);
                    } else if (ANY_BLIND(ShadeFlag)) {
                        state.dataSurface->SurfWinIntSWAbsByShade(SurfNum) += state.dataSurface->SurfWinInitialDifSolAbsByShade(SurfNum);
                    }
                } // End of shading flag check
            } else if (state.dataSurface->SurfWinWindowModelType(SurfNum) == WindowBSDFModel) {
                int TotGlassLayers = state.dataConstruction->Construct(ConstrNum).TotGlassLayers;
                for (int IGlass = 1; IGlass <= TotGlassLayers; ++IGlass) {
                    state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum, IGlass) += state.dataHeatBal->SurfWinInitialDifSolwinAbs(SurfNum, IGlass);
                }
            } else if (state.dataSurface->SurfWinWindowModelType(SurfNum) == WindowEQLModel) {

                // ConstrNum   = Surface(SurfNum)%Construction
                int EQLNum = state.dataConstruction->Construct(ConstrNum).EQLConsPtr;

                for (int Lay = 1; Lay <= state.dataWindowEquivLayer->CFS(EQLNum).NL; ++Lay) {
                    state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum, Lay) += state.dataHeatBal->SurfWinInitialDifSolwinAbs(SurfNum, Lay);
                }
            }

            // report variables for surface absorbed short wave radiation
            state.dataHeatBal->SurfWinSWwinAbsTotalReport(SurfNum) = 0.0;
            state.dataHeatBal->SurfSWInAbsTotalReport(SurfNum) = 0.0;
            state.dataHeatBal->SurfInitialDifSolInAbsReport(SurfNum) = 0.0;
            state.dataHeatBal->SurfWinInitialDifSolInTransReport(SurfNum) = 0.0;
            state.dataHeatBal->SurfWinInitialDifSolInTransReport(SurfNum) +=
                state.dataHeatBalSurf->SurfWinInitialDifSolInTrans(SurfNum) * Surface(SurfNum).Area;
            if (state.dataSurface->SurfWinWindowModelType(SurfNum) != WindowEQLModel) {
                int const ConstrNumSh = state.dataSurface->SurfWinActiveShadedConstruction(SurfNum);
                int TotGlassLayers;
                if (state.dataSurface->SurfWinWindowModelType(SurfNum) == WindowBSDFModel) {
                    TotGlassLayers = state.dataConstruction->Construct(ConstrNum).TotSolidLayers;
                } else {
                    TotGlassLayers = state.dataConstruction->Construct(ConstrNum).TotGlassLayers;
                }
                WinShadingType ShadeFlag = state.dataSurface->SurfWinShadingFlag(SurfNum);
                if (NOT_SHADED(ShadeFlag) || state.dataSurface->SurfWinWindowModelType(SurfNum) == WindowBSDFModel) { // No window shading
                    for (int IGlass = 1; IGlass <= TotGlassLayers; ++IGlass) {
                        // Initial Transmitted Diffuse Solar Absorbed on Inside of Surface[W]
                        state.dataHeatBal->SurfInitialDifSolInAbsReport(SurfNum) +=
                            state.dataHeatBal->SurfWinInitialDifSolwinAbs(SurfNum, IGlass) * Surface(SurfNum).Area;
                        // Total Shortwave Radiation Absorbed on Inside of Surface[W]
                        state.dataHeatBal->SurfSWInAbsTotalReport(SurfNum) +=
                            state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum, IGlass) * Surface(SurfNum).Area;
                        // Total Shortwave Absorbed:All Glass Layers[W]
                        state.dataHeatBal->SurfWinSWwinAbsTotalReport(SurfNum) +=
                            state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum, IGlass) * Surface(SurfNum).Area;
                    }
                } else if (ShadeFlag == WinShadingType::SwitchableGlazing) { // Switchable glazing
                    for (int IGlass = 1; IGlass <= TotGlassLayers; ++IGlass) {
                        // Initial Transmitted Diffuse Solar Absorbed on Inside of Surface[W]
                        state.dataHeatBal->SurfInitialDifSolInAbsReport(SurfNum) +=
                            state.dataHeatBal->SurfWinInitialDifSolwinAbs(SurfNum, IGlass) * Surface(SurfNum).Area;
                        // Total Shortwave Radiation Absorbed on Inside of Surface[W]
                        state.dataHeatBal->SurfSWInAbsTotalReport(SurfNum) +=
                            state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum, IGlass) * Surface(SurfNum).Area;
                        // Total Shortwave Absorbed:All Glass Layers[W]
                        state.dataHeatBal->SurfWinSWwinAbsTotalReport(SurfNum) +=
                            state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum, IGlass) * Surface(SurfNum).Area;
                    }
                } else {
                    // Interior, exterior or between-glass shade, screen or blind in place
                    for (int IGlass = 1; IGlass <= state.dataConstruction->Construct(ConstrNumSh).TotGlassLayers; ++IGlass) {
                        // Initial Transmitted Diffuse Solar Absorbed on Inside of Surface[W]
                        state.dataHeatBal->SurfInitialDifSolInAbsReport(SurfNum) +=
                            state.dataHeatBal->SurfWinInitialDifSolwinAbs(SurfNum, IGlass) * Surface(SurfNum).Area;
                        // Total Shortwave Radiation Absorbed on Inside of Surface[W]
                        state.dataHeatBal->SurfSWInAbsTotalReport(SurfNum) +=
                            state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum, IGlass) * Surface(SurfNum).Area;
                        // Total Shortwave Absorbed:All Glass Layers[W]
                        state.dataHeatBal->SurfWinSWwinAbsTotalReport(SurfNum) +=
                            state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum, IGlass) * Surface(SurfNum).Area;
                    }
                }    // End of shading flag check
            } else { // IF (SurfaceWindow(SurfNum)%WindowModelType == WindowEQLModel) THEN
                int EQLNum = state.dataConstruction->Construct(ConstrNum).EQLConsPtr;
                for (int Lay = 1; Lay <= state.dataWindowEquivLayer->CFS(EQLNum).NL; ++Lay) {
                    // Initial Transmitted Diffuse Solar Absorbed on Inside of Surface[W]
                    state.dataHeatBal->SurfInitialDifSolInAbsReport(SurfNum) +=
                        state.dataHeatBal->SurfWinInitialDifSolwinAbs(SurfNum, Lay) * Surface(SurfNum).Area;
                    // Total Shortwave Radiation Absorbed on Inside of Surface[W]
                    state.dataHeatBal->SurfSWInAbsTotalReport(SurfNum) +=
                        state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum, Lay) * Surface(SurfNum).Area;
                    // Total Shortwave Absorbed:All solid Layers[W]
                    state.dataHeatBal->SurfWinSWwinAbsTotalReport(SurfNum) +=
                        state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum, Lay) * Surface(SurfNum).Area;
                }
            }
        } // End of window
    }
    DistributeTDDAbsorbedSolar(state);
}

void ComputeIntThermalAbsorpFactors(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Legacy Code (George Walton)
    //       DATE WRITTEN   Legacy: Dec 1976
    //       MODIFIED       Nov. 99, FCW: to take into account movable interior shades and switchable glazing
    //                      June 01, FCW: to take into account interior blinds.
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This routine computes the fractions of long-wave radiation from lights, equipment and people
    // that is absorbed by each zone surface.

    // METHODOLOGY EMPLOYED:
    // The fraction is assumed to be proportional to the product of the surface area times its thermal absorptivity.

    // REFERENCES:
    // BLAST Routine: CITAF - Compute Interior Thermal Absorption Factors

    using General::InterpSw;
    auto &Surface(state.dataSurface->Surface);

    if (!allocated(state.dataHeatBal->ITABSF)) {
        state.dataHeatBal->ITABSF.dimension(state.dataSurface->TotSurfaces, 0.0);
        state.dataHeatBal->TMULT.dimension(state.dataGlobal->NumOfZones, 0.0);
    }

    for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
        int const firstSurf = state.dataHeatBal->Zone(zoneNum).HTSurfaceFirst;
        int const lastSurf = state.dataHeatBal->Zone(zoneNum).HTSurfaceLast;
        for (int SurfNum = firstSurf; SurfNum <= lastSurf; ++SurfNum) {
            state.dataHeatBal->ITABSF(SurfNum) = state.dataHeatBalSurf->SurfAbsThermalInt(SurfNum); // Movable inside insulation present
        }
        int const firstSurfWin = state.dataHeatBal->Zone(zoneNum).WindowSurfaceFirst;
        int const lastSurfWin = state.dataHeatBal->Zone(zoneNum).WindowSurfaceLast;
        for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
            // For window with an interior shade or blind, emissivity is a combination of glass and shade/blind emissivity
            WinShadingType ShadeFlag = state.dataSurface->SurfWinShadingFlag(SurfNum);
            if (ANY_INTERIOR_SHADE_BLIND(ShadeFlag)) {
                Real64 BlindEmiss;
                Real64 GlassEmiss;
                if (state.dataSurface->SurfWinMovableSlats(SurfNum)) {
                    int SurfWinSlatsAngIndex = state.dataSurface->SurfWinSlatsAngIndex(SurfNum);
                    Real64 SurfWinSlatsAngInterpFac = state.dataSurface->SurfWinSlatsAngInterpFac(SurfNum);
                    BlindEmiss = General::InterpGeneral(
                        state.dataSurface->SurfaceWindow(SurfNum).EffShBlindEmiss(SurfWinSlatsAngIndex),
                        state.dataSurface->SurfaceWindow(SurfNum).EffShBlindEmiss(std::min(MaxSlatAngs, SurfWinSlatsAngIndex + 1)),
                        SurfWinSlatsAngInterpFac);
                    GlassEmiss = General::InterpGeneral(
                        state.dataSurface->SurfaceWindow(SurfNum).EffGlassEmiss(SurfWinSlatsAngIndex),
                        state.dataSurface->SurfaceWindow(SurfNum).EffGlassEmiss(std::min(MaxSlatAngs, SurfWinSlatsAngIndex + 1)),
                        SurfWinSlatsAngInterpFac);
                } else {
                    BlindEmiss = state.dataSurface->SurfaceWindow(SurfNum).EffShBlindEmiss(1);
                    GlassEmiss = state.dataSurface->SurfaceWindow(SurfNum).EffGlassEmiss(1);
                }
                state.dataHeatBal->ITABSF(SurfNum) = BlindEmiss + GlassEmiss;
                // For shades, following interpolation just returns value of first element in array
            }
        }
    }

    for (int radEnclosureNum = 1; radEnclosureNum <= state.dataViewFactor->NumOfRadiantEnclosures; ++radEnclosureNum) {

        Real64 SUM1 = 0.0;
        auto &thisEnclosure(state.dataViewFactor->ZoneRadiantInfo(radEnclosureNum));

        for (int const SurfNum : thisEnclosure.SurfacePtr) {

            if (!Surface(SurfNum).HeatTransSurf) continue;
            int const ConstrNum = Surface(SurfNum).Construction;
            WinShadingType ShadeFlag = state.dataSurface->SurfWinShadingFlag(SurfNum);
            if (ShadeFlag != WinShadingType::SwitchableGlazing) {
                SUM1 += Surface(SurfNum).Area * state.dataHeatBal->ITABSF(SurfNum);
            } else { // Switchable glazing
                SUM1 += Surface(SurfNum).Area *
                        InterpSw(state.dataSurface->SurfWinSwitchingFactor(SurfNum),
                                 state.dataConstruction->Construct(ConstrNum).InsideAbsorpThermal,
                                 state.dataConstruction->Construct(Surface(SurfNum).activeShadedConstruction).InsideAbsorpThermal);
            }

            // Window frame and divider effects
            if (state.dataSurface->SurfWinFrameArea(SurfNum) > 0.0)
                SUM1 += state.dataSurface->SurfWinFrameArea(SurfNum) * (1.0 + 0.5 * state.dataSurface->SurfWinProjCorrFrIn(SurfNum)) *
                        state.dataSurface->SurfWinFrameEmis(SurfNum);
            if (state.dataSurface->SurfWinDividerArea(SurfNum) > 0.0) {
                Real64 DividerThermAbs = state.dataSurface->SurfWinDividerEmis(SurfNum); // Window divider thermal absorptance
                // Suspended (between-glass) divider; relevant emissivity is inner glass emissivity
                if (state.dataSurface->SurfWinDividerType(SurfNum) == Suspended)
                    DividerThermAbs = state.dataConstruction->Construct(ConstrNum).InsideAbsorpThermal;
                if (ANY_INTERIOR_SHADE_BLIND(ShadeFlag)) {
                    // Interior shade or blind in place
                    int const ConstrNumSh = Surface(SurfNum).activeShadedConstruction;
                    if (state.dataSurface->SurfWinHasShadeOrBlindLayer(SurfNum)) {
                        // Shade layer material number
                        int MatNumSh =
                            state.dataConstruction->Construct(ConstrNumSh).LayerPoint(state.dataConstruction->Construct(ConstrNumSh).TotLayers);
                        // Shade or blind IR transmittance
                        Real64 TauShIR = state.dataMaterial->Material(MatNumSh).TransThermal;
                        // Effective emissivity of shade or blind
                        Real64 EffShDevEmiss = state.dataSurface->SurfaceWindow(SurfNum).EffShBlindEmiss(1);
                        if (ShadeFlag == WinShadingType::IntBlind) {
                            TauShIR = state.dataHeatBal->Blind(state.dataSurface->SurfWinBlindNumber(SurfNum)).IRBackTrans(1);
                            if (state.dataSurface->SurfWinMovableSlats(SurfNum)) {
                                int SurfWinSlatsAngIndex = state.dataSurface->SurfWinSlatsAngIndex(SurfNum);
                                Real64 SurfWinSlatsAngInterpFac = state.dataSurface->SurfWinSlatsAngInterpFac(SurfNum);
                                TauShIR = General::InterpGeneral(
                                    state.dataHeatBal->Blind(state.dataSurface->SurfWinBlindNumber(SurfNum)).IRBackTrans(SurfWinSlatsAngIndex),
                                    state.dataHeatBal->Blind(state.dataSurface->SurfWinBlindNumber(SurfNum))
                                        .IRBackTrans(std::min(MaxSlatAngs, SurfWinSlatsAngIndex + 1)),
                                    SurfWinSlatsAngInterpFac);
                                EffShDevEmiss = General::InterpGeneral(
                                    state.dataSurface->SurfaceWindow(SurfNum).EffShBlindEmiss(SurfWinSlatsAngIndex),
                                    state.dataSurface->SurfaceWindow(SurfNum).EffShBlindEmiss(std::min(MaxSlatAngs, SurfWinSlatsAngIndex + 1)),
                                    SurfWinSlatsAngInterpFac);
                            }
                        }
                        SUM1 += state.dataSurface->SurfWinDividerArea(SurfNum) * (EffShDevEmiss + DividerThermAbs * TauShIR);
                    } else {
                        // this is for EMS activated shade/blind but the window construction has no shade/blind layer
                        SUM1 += state.dataSurface->SurfWinDividerArea(SurfNum) * (1.0 + state.dataSurface->SurfWinProjCorrDivIn(SurfNum)) *
                                DividerThermAbs;
                    }
                } else {
                    SUM1 +=
                        state.dataSurface->SurfWinDividerArea(SurfNum) * (1.0 + state.dataSurface->SurfWinProjCorrDivIn(SurfNum)) * DividerThermAbs;
                }
            }

        } // End of loop over surfaces in zone/enclosure

        state.dataHeatBal->TMULT(radEnclosureNum) = 1.0 / SUM1;

    } // End of loop over zones
}

void ComputeIntSWAbsorpFactors(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Legacy (George Walton)
    //       DATE WRITTEN   Legacy (December 1980)
    //       MODIFIED       Nov. 99, FW; now called every time step to account for movable
    //                      window shades and insulation
    //                      Mar. 00, FW; change name from ComputeVisLightingAbsorpFactors
    //                      to ComputeIntSWAbsorpFactors
    //                      May 00, FW; add window frame and divider effects
    //                      June 01, FW: account for window blinds
    //                      Nov 01, FW: account for absorptance of exterior shades and interior or exterior blinds
    //                      Jan 03, FW: add between-glass shade/blind
    //                      May 06, RR: account for exterior window screens

    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Computes VMULT, the inverse of the sum of area*(short-wave absorptance+transmittance) for
    // the surfaces in a zone. VMULT is used to calculate the zone interior diffuse short-wave radiation
    // absorbed by the inside of opaque zone surfaces or by the glass and shade/blind layers of zone windows.

    // Sets VCONV to zero (VCONV was formerly used to calculate convective gain due to short-wave
    // radiation absorbed by interior window shades).

    // METHODOLOGY EMPLOYED:
    // <description>

    // REFERENCES:
    // BLAST Routine - CIVAF - Compute Surface Absorption Factors For Short Wave Radiation
    //                         From Zone Lights And Diffuse Solar.
    // Using/Aliasing
    using General::InterpSw;
    using namespace DataWindowEquivalentLayer;

    auto &Surface(state.dataSurface->Surface);

    Real64 const SmallestAreaAbsProductAllowed(
        0.01); // Avoid a division by zero of the user has entered a bunch of surfaces with zero absorptivity on the inside

    if (!allocated(state.dataHeatBal->EnclSolVMULT)) {
        state.dataHeatBal->EnclSolVMULT.dimension(state.dataViewFactor->NumOfSolarEnclosures, 0.0);
    }
    if (state.dataHeatBalSurfMgr->ComputeIntSWAbsorpFactorsfirstTime) {
        state.dataHeatBalSurfMgr->FirstCalcZone.dimension(state.dataGlobal->NumOfZones, true);
        state.dataHeatBalSurfMgr->ComputeIntSWAbsorpFactorsfirstTime = false;
    }

    for (int enclosureNum = 1; enclosureNum <= state.dataViewFactor->NumOfSolarEnclosures; ++enclosureNum) {
        Real64 SUM1 = 0.0; // Intermediate calculation value for solar absorbed and transmitted

        for (int const SurfNum : state.dataViewFactor->ZoneSolarInfo(enclosureNum).SurfacePtr) {
            int const ConstrNum = state.dataSurface->SurfActiveConstruction(SurfNum);
            if (state.dataConstruction->Construct(ConstrNum).TransDiff <= 0.0) {
                // Opaque surface
                Real64 AbsIntSurf = state.dataHeatBalSurf->SurfAbsSolarInt(SurfNum); // Inside surface short-wave absorptance
                SUM1 += Surface(SurfNum).Area * AbsIntSurf;

            } else {

                // Window
                if (!state.dataConstruction->Construct(Surface(SurfNum).Construction).WindowTypeEQL) {
                    WinShadingType ShadeFlag = state.dataSurface->SurfWinShadingFlag(SurfNum);
                    Real64 AbsDiffTotWin = 0.0; // Sum of window layer short-wave absorptances
                    int const ConstrNumSh = state.dataSurface->SurfWinActiveShadedConstruction(SurfNum);
                    Real64 SwitchFac = state.dataSurface->SurfWinSwitchingFactor(SurfNum);

                    // Sum of absorptances of glass layers
                    for (int Lay = 1; Lay <= state.dataConstruction->Construct(ConstrNum).TotGlassLayers; ++Lay) {
                        Real64 AbsDiffLayWin = state.dataConstruction->Construct(ConstrNum).AbsDiffBack(Lay); // Window layer short-wave absorptance

                        // Window with shade, screen or blind
                        if (ConstrNumSh != 0) {
                            if (ANY_SHADE_SCREEN(ShadeFlag)) {
                                AbsDiffLayWin = state.dataConstruction->Construct(ConstrNumSh).AbsDiffBack(Lay);
                            } else if (ANY_BLIND(ShadeFlag)) {
                                if (state.dataSurface->SurfWinMovableSlats(SurfNum)) {
                                    AbsDiffLayWin = General::InterpGeneral(
                                        state.dataConstruction->Construct(ConstrNumSh)
                                            .BlAbsDiffBack(state.dataSurface->SurfWinSlatsAngIndex(SurfNum), Lay),
                                        state.dataConstruction->Construct(ConstrNumSh)
                                            .BlAbsDiffBack(std::min(MaxSlatAngs, state.dataSurface->SurfWinSlatsAngIndex(SurfNum) + 1), Lay),
                                        state.dataSurface->SurfWinSlatsAngInterpFac(SurfNum));
                                } else {
                                    AbsDiffLayWin = state.dataConstruction->Construct(ConstrNumSh).BlAbsDiffBack(1, Lay);
                                }
                            }
                        }

                        // Switchable glazing
                        if (ShadeFlag == WinShadingType::SwitchableGlazing)
                            AbsDiffLayWin = InterpSw(SwitchFac, AbsDiffLayWin, state.dataConstruction->Construct(ConstrNumSh).AbsDiffBack(Lay));

                        AbsDiffTotWin += AbsDiffLayWin;
                    }

                    Real64 TransDiffWin = state.dataConstruction->Construct(ConstrNum).TransDiff; // Window diffuse short-wave transmittance
                    Real64 DiffAbsShade = 0.0;                                                    // Diffuse short-wave shade or blind absorptance

                    // Window with shade, screen or blind

                    if (ConstrNumSh != 0) {
                        if (ANY_SHADE_SCREEN(ShadeFlag)) {
                            TransDiffWin = state.dataConstruction->Construct(ConstrNumSh).TransDiff;
                            DiffAbsShade = state.dataConstruction->Construct(ConstrNumSh).AbsDiffBackShade;
                        } else if (ANY_BLIND(ShadeFlag)) {
                            if (state.dataSurface->SurfWinMovableSlats(SurfNum)) {
                                int SurfWinSlatsAngIndex = state.dataSurface->SurfWinSlatsAngIndex(SurfNum);
                                Real64 SurfWinSlatsAngInterpFac = state.dataSurface->SurfWinSlatsAngInterpFac(SurfNum);
                                TransDiffWin = General::InterpGeneral(
                                    state.dataConstruction->Construct(ConstrNumSh).BlTransDiff(SurfWinSlatsAngIndex),
                                    state.dataConstruction->Construct(ConstrNumSh).BlTransDiff(std::min(MaxSlatAngs, SurfWinSlatsAngIndex + 1)),
                                    SurfWinSlatsAngInterpFac);
                                DiffAbsShade = General::InterpGeneral(
                                    state.dataConstruction->Construct(ConstrNumSh).AbsDiffBackBlind(SurfWinSlatsAngIndex),
                                    state.dataConstruction->Construct(ConstrNumSh).AbsDiffBackBlind(std::min(MaxSlatAngs, SurfWinSlatsAngIndex + 1)),
                                    SurfWinSlatsAngInterpFac);
                            } else {
                                TransDiffWin = state.dataConstruction->Construct(ConstrNumSh).BlTransDiff(1);
                                DiffAbsShade = state.dataConstruction->Construct(ConstrNumSh).AbsDiffBackBlind(1);
                            }
                        }
                    }

                    // Switchable glazing

                    if (ShadeFlag == WinShadingType::SwitchableGlazing)
                        TransDiffWin = InterpSw(SwitchFac, TransDiffWin, state.dataConstruction->Construct(ConstrNumSh).TransDiff);

                    SUM1 += Surface(SurfNum).Area * (TransDiffWin + AbsDiffTotWin + DiffAbsShade);

                    // Window frame and divider effects (shade area is glazed area plus divider area)

                    if (state.dataSurface->SurfWinFrameArea(SurfNum) > 0.0)
                        SUM1 += state.dataSurface->SurfWinFrameArea(SurfNum) * state.dataSurface->SurfWinFrameSolAbsorp(SurfNum) *
                                (1.0 + 0.5 * state.dataSurface->SurfWinProjCorrFrIn(SurfNum));
                    if (state.dataSurface->SurfWinDividerArea(SurfNum) > 0.0) {
                        Real64 DividerAbs = state.dataSurface->SurfWinDividerSolAbsorp(SurfNum); // Window divider solar absorptance
                        if (state.dataSurface->SurfWinDividerType(SurfNum) == Suspended) {
                            // Suspended (between-glass) divider: account for glass on inside of divider
                            Real64 MatNumGl = state.dataConstruction->Construct(ConstrNum).LayerPoint(
                                state.dataConstruction->Construct(ConstrNum).TotLayers); // Glass material number
                            Real64 TransGl =
                                state.dataMaterial->Material(MatNumGl).Trans; // Glass layer short-wave transmittance, reflectance, absorptance
                            Real64 ReflGl = state.dataMaterial->Material(MatNumGl).ReflectSolBeamBack;
                            Real64 AbsGl = 1.0 - TransGl - ReflGl;
                            Real64 DividerRefl = 1.0 - DividerAbs; // Window divider short-wave reflectance
                            DividerAbs = AbsGl + TransGl * (DividerAbs + DividerRefl * AbsGl) / (1.0 - DividerRefl * ReflGl);
                        }
                        if (ANY_INTERIOR_SHADE_BLIND(ShadeFlag)) {
                            SUM1 += state.dataSurface->SurfWinDividerArea(SurfNum) * (DividerAbs + DiffAbsShade);
                        } else {
                            SUM1 += state.dataSurface->SurfWinDividerArea(SurfNum) * (1.0 + state.dataSurface->SurfWinProjCorrDivIn(SurfNum)) *
                                    DividerAbs;
                        }
                    }
                } else { // equivalent layer window
                    // In equivalent layer window solid layers (Glazing and shades) are treated equally
                    // frames and dividers are not supported
                    Real64 AbsDiffTotWin = 0.0;
                    Real64 AbsDiffLayWin = 0.0;
                    Real64 TransDiffWin = state.dataConstruction->Construct(ConstrNum).TransDiff;
                    for (int Lay = 1; Lay <= state.dataWindowEquivLayer->CFS(state.dataConstruction->Construct(ConstrNum).EQLConsPtr).NL; ++Lay) {
                        AbsDiffLayWin = state.dataConstruction->Construct(ConstrNum).AbsDiffBackEQL(Lay);
                        AbsDiffTotWin += AbsDiffLayWin;
                    }
                    SUM1 += Surface(SurfNum).Area * (TransDiffWin + AbsDiffTotWin);
                }
            } // End of check if opaque surface or window
        }     // End of loop over surfaces in zone

        if (SUM1 > SmallestAreaAbsProductAllowed) { // Everything is okay, proceed with the regular calculation
            state.dataHeatBal->EnclSolVMULT(enclosureNum) = 1.0 / SUM1;

        } else { // the sum of area*solar absorptance for all surfaces in the zone is zero--either the user screwed up
            // or they really want to disallow any solar from being absorbed on the inside surfaces.  Fire off a
            // nasty warning message and then assume that no solar is ever absorbed (basically everything goes
            // back out whatever window is there.  Note that this also assumes that the shade has no effect.
            // That's probably not correct, but how correct is it to assume that no solar is absorbed anywhere
            // in the zone?
            if (state.dataHeatBalSurfMgr->FirstCalcZone(enclosureNum)) {
                ShowWarningError(state,
                                 "ComputeIntSWAbsorbFactors: Sum of area times inside solar absorption for all surfaces is zero in Zone: " +
                                     state.dataViewFactor->ZoneSolarInfo(enclosureNum).Name);
                state.dataHeatBalSurfMgr->FirstCalcZone(enclosureNum) = false;
            }
            state.dataHeatBal->EnclSolVMULT(enclosureNum) = 0.0;
        }
    } // End of zone/enclosure loop
}

void ComputeDifSolExcZonesWIZWindows(EnergyPlusData &state, int const NumberOfEnclosures) // Number of solar enclosures
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Legacy Code
    //       DATE WRITTEN
    //       MODIFIED       Jun 2007 - Lawrie - Speed enhancements.
    //       RE-ENGINEERED  Winkelmann, Lawrie

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine computes the diffuse solar exchange factors between zones with
    // interzone windows.

    auto &Surface(state.dataSurface->Surface);

    if (!allocated(state.dataHeatBalSurf->FractDifShortZtoZ)) {
        state.dataHeatBalSurf->FractDifShortZtoZ.allocate(NumberOfEnclosures, NumberOfEnclosures);
        state.dataHeatBalSurf->RecDifShortFromZ.allocate(NumberOfEnclosures);
        state.dataHeatBalSurfMgr->DiffuseArray.allocate(NumberOfEnclosures, NumberOfEnclosures);
    }

    state.dataHeatBalSurf->RecDifShortFromZ = false;
    state.dataHeatBalSurf->FractDifShortZtoZ.to_identity();
    state.dataHeatBalSurfMgr->DiffuseArray.to_identity();

    //      IF (.not. ANY(Zone%HasInterZoneWindow)) RETURN  ! this caused massive diffs
    if (state.dataGlobal->KickOffSimulation || state.dataGlobal->KickOffSizing) return;
    //            Compute fraction transmitted in one pass.

    for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
        if (!Surface(SurfNum).HeatTransSurf) continue;
        if (Surface(SurfNum).ExtBoundCond <= 0) continue;
        if (Surface(SurfNum).ExtBoundCond == SurfNum) continue;
        if (state.dataConstruction->Construct(Surface(SurfNum).Construction).TransDiff <= 0.0) continue;

        int surfZoneNum = Surface(SurfNum).Zone;
        if (!state.dataHeatBal->Zone(surfZoneNum).HasInterZoneWindow) continue;
        int NZ = Surface(SurfNum).SolarEnclIndex;
        int MZ = Surface(Surface(SurfNum).ExtBoundCond).SolarEnclIndex;
        state.dataHeatBalSurf->FractDifShortZtoZ(NZ, MZ) +=
            state.dataConstruction->Construct(Surface(SurfNum).Construction).TransDiff * state.dataHeatBal->EnclSolVMULT(NZ) * Surface(SurfNum).Area;
        if (state.dataHeatBal->EnclSolVMULT(NZ) != 0.0) state.dataHeatBalSurf->RecDifShortFromZ(NZ) = true;
    }
    //          Compute fractions for multiple passes.

    Array2D<Real64>::size_type l(0u), m(0u), d(0u);
    for (int NZ = 1; NZ <= NumberOfEnclosures; ++NZ, d += NumberOfEnclosures + 1) {
        m = NZ - 1;
        Real64 D_d(0.0); // Local accumulator
        for (int MZ = 1; MZ <= NumberOfEnclosures; ++MZ, ++l, m += NumberOfEnclosures) {
            if (MZ == NZ) continue;
            state.dataHeatBalSurfMgr->DiffuseArray[l] =
                state.dataHeatBalSurf->FractDifShortZtoZ[l] /
                (1.0 - state.dataHeatBalSurf->FractDifShortZtoZ[l] *
                           state.dataHeatBalSurf->FractDifShortZtoZ[m]); // [ l ] == ( MZ, NZ ), [ m ] == ( NZ, MZ )
            D_d += state.dataHeatBalSurf->FractDifShortZtoZ[m] * state.dataHeatBalSurfMgr->DiffuseArray[l];
        }
        state.dataHeatBalSurfMgr->DiffuseArray[d] += D_d; // [ d ] == ( NZ, NZ )
    }

    state.dataHeatBalSurf->FractDifShortZtoZ = state.dataHeatBalSurfMgr->DiffuseArray;
    // added for CR 7999 & 7869
    assert(state.dataHeatBalSurf->FractDifShortZtoZ.isize1() == NumberOfEnclosures);
    assert(state.dataHeatBalSurf->FractDifShortZtoZ.isize2() == NumberOfEnclosures);
    l = 0u;
    for (int NZ = 1; NZ <= NumberOfEnclosures; ++NZ) {
        for (int MZ = 1; MZ <= NumberOfEnclosures; ++MZ, ++l) {
            if (MZ == NZ) continue;
            if (state.dataHeatBalSurf->FractDifShortZtoZ[l] > 0.0) { // [ l ] == ( MZ, NZ )
                state.dataHeatBalSurf->RecDifShortFromZ(NZ) = true;
                break;
            }
        }
    }

    //           Compute fractions for multiple zones.

    for (int IZ = 1; IZ <= NumberOfEnclosures; ++IZ) {
        if (!state.dataHeatBalSurf->RecDifShortFromZ(IZ)) continue;

        for (int JZ = 1; JZ <= NumberOfEnclosures; ++JZ) {
            if (!state.dataHeatBalSurf->RecDifShortFromZ(JZ)) continue;
            if (IZ == JZ) continue;
            if (state.dataHeatBalSurfMgr->DiffuseArray(IZ, JZ) == 0.0) continue;

            for (int KZ = 1; KZ <= NumberOfEnclosures; ++KZ) {
                if (!state.dataHeatBalSurf->RecDifShortFromZ(KZ)) continue;
                if (IZ == KZ) continue;
                if (JZ == KZ) continue;
                if (state.dataHeatBalSurfMgr->DiffuseArray(JZ, KZ) == 0.0) continue;
                state.dataHeatBalSurf->FractDifShortZtoZ(IZ, KZ) +=
                    state.dataHeatBalSurfMgr->DiffuseArray(JZ, KZ) * state.dataHeatBalSurfMgr->DiffuseArray(IZ, JZ);

                for (int LZ = 1; LZ <= NumberOfEnclosures; ++LZ) {
                    if (!state.dataHeatBalSurf->RecDifShortFromZ(LZ)) continue;
                    if (IZ == LZ) continue;
                    if (JZ == LZ) continue;
                    if (KZ == LZ) continue;
                    if (state.dataHeatBalSurfMgr->DiffuseArray(KZ, LZ) == 0.0) continue;
                    state.dataHeatBalSurf->FractDifShortZtoZ(IZ, LZ) += state.dataHeatBalSurfMgr->DiffuseArray(KZ, LZ) *
                                                                        state.dataHeatBalSurfMgr->DiffuseArray(JZ, KZ) *
                                                                        state.dataHeatBalSurfMgr->DiffuseArray(IZ, JZ);

                    for (int MZ = 1; MZ <= NumberOfEnclosures; ++MZ) {
                        if (!state.dataHeatBalSurf->RecDifShortFromZ(MZ)) continue;
                        if (IZ == MZ) continue;
                        if (JZ == MZ) continue;
                        if (KZ == MZ) continue;
                        if (LZ == MZ) continue;
                        if (state.dataHeatBalSurfMgr->DiffuseArray(LZ, MZ) == 0.0) continue;
                        state.dataHeatBalSurf->FractDifShortZtoZ(IZ, MZ) +=
                            state.dataHeatBalSurfMgr->DiffuseArray(LZ, MZ) * state.dataHeatBalSurfMgr->DiffuseArray(KZ, LZ) *
                            state.dataHeatBalSurfMgr->DiffuseArray(JZ, KZ) * state.dataHeatBalSurfMgr->DiffuseArray(IZ, JZ);
                    } // MZ Loop

                } // LZ Loop

            } // KZ Loop

        } // JZ Loop

    } // IZ Loop
}

void InitEMSControlledSurfaceProperties(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   April 2011
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // initialize material and construction surface properties if being overriden by EMS

    // METHODOLOGY EMPLOYED:
    // update solar, thermal and visible absorptance values when actuated by EMS

    // REFERENCES:
    // na

    // USE STATEMENTS:
    // na

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:
    // na

    // SUBROUTINE PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS:
    // na

    // DERIVED TYPE DEFINITIONS:
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int MaterNum;        // do loop counter over materials
    int ConstrNum;       // do loop counter over constructions
    int TotLayers;       // count of material layers in a construction
    int InsideMaterNum;  // integer pointer for inside face's material layer
    int OutsideMaterNum; // integer pointer for outside face's material layer

    // first determine if anything needs to be done, once yes, then always init
    for (auto const &mat : state.dataMaterial->Material) {
        if ((mat.AbsorpSolarEMSOverrideOn) || (mat.AbsorpThermalEMSOverrideOn) || (mat.AbsorpVisibleEMSOverrideOn)) {
            state.dataGlobal->AnySurfPropOverridesInModel = true;
            break;
        }
    }

    if (!state.dataGlobal->AnySurfPropOverridesInModel) return; // quick return if nothing has ever needed to be done

    // first, loop over materials
    for (MaterNum = 1; MaterNum <= state.dataHeatBal->TotMaterials; ++MaterNum) {
        if (state.dataMaterial->Material(MaterNum).AbsorpSolarEMSOverrideOn) {
            state.dataMaterial->Material(MaterNum).AbsorpSolar =
                max(min(state.dataMaterial->Material(MaterNum).AbsorpSolarEMSOverride, 0.9999), 0.0001);
        } else {
            state.dataMaterial->Material(MaterNum).AbsorpSolar = state.dataMaterial->Material(MaterNum).AbsorpSolarInput;
        }
        if (state.dataMaterial->Material(MaterNum).AbsorpThermalEMSOverrideOn) {
            state.dataMaterial->Material(MaterNum).AbsorpThermal =
                max(min(state.dataMaterial->Material(MaterNum).AbsorpThermalEMSOverride, 0.9999), 0.0001);
        } else {
            state.dataMaterial->Material(MaterNum).AbsorpThermal = state.dataMaterial->Material(MaterNum).AbsorpThermalInput;
        }
        if (state.dataMaterial->Material(MaterNum).AbsorpVisibleEMSOverrideOn) {
            state.dataMaterial->Material(MaterNum).AbsorpVisible =
                max(min(state.dataMaterial->Material(MaterNum).AbsorpVisibleEMSOverride, 0.9999), 0.0001);
        } else {
            state.dataMaterial->Material(MaterNum).AbsorpVisible = state.dataMaterial->Material(MaterNum).AbsorpVisibleInput;
        }
    } // loop over materials

    // second, loop over constructions
    for (ConstrNum = 1; ConstrNum <= state.dataHeatBal->TotConstructs; ++ConstrNum) {
        if (state.dataConstruction->Construct(ConstrNum).TypeIsWindow) continue; // only override opaque constructions
        TotLayers = state.dataConstruction->Construct(ConstrNum).TotLayers;
        if (TotLayers == 0) continue; // error condition
        InsideMaterNum = state.dataConstruction->Construct(ConstrNum).LayerPoint(TotLayers);
        if (InsideMaterNum != 0) {
            state.dataConstruction->Construct(ConstrNum).InsideAbsorpVis = state.dataMaterial->Material(InsideMaterNum).AbsorpVisible;
            state.dataConstruction->Construct(ConstrNum).InsideAbsorpSolar = state.dataMaterial->Material(InsideMaterNum).AbsorpSolar;
            state.dataConstruction->Construct(ConstrNum).InsideAbsorpThermal = state.dataMaterial->Material(InsideMaterNum).AbsorpThermal;
        }

        OutsideMaterNum = state.dataConstruction->Construct(ConstrNum).LayerPoint(1);
        if (OutsideMaterNum != 0) {
            state.dataConstruction->Construct(ConstrNum).OutsideAbsorpVis = state.dataMaterial->Material(OutsideMaterNum).AbsorpVisible;
            state.dataConstruction->Construct(ConstrNum).OutsideAbsorpSolar = state.dataMaterial->Material(OutsideMaterNum).AbsorpSolar;
            state.dataConstruction->Construct(ConstrNum).OutsideAbsorpThermal = state.dataMaterial->Material(OutsideMaterNum).AbsorpThermal;
        }
    }
}

void InitEMSControlledConstructions(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   Jan 2012
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // change construction on surface if overriden by EMS

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool SurfConstructOverridesPresent(false); // detect if EMS ever used for this and inits need to execute

    auto &Surface(state.dataSurface->Surface);

    for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
        if (state.dataSurface->SurfEMSConstructionOverrideON(SurfNum)) {
            SurfConstructOverridesPresent = true;
            break;
        }
    }
    if (!SurfConstructOverridesPresent) return;

    for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {

        if (state.dataSurface->SurfEMSConstructionOverrideON(SurfNum) && (state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum) > 0)) {

            if (state.dataConstruction->Construct(state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum))
                    .TypeIsWindow) { // okay, allways allow windows
                state.dataRuntimeLang->EMSConstructActuatorChecked(state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum), SurfNum) = true;
                state.dataRuntimeLang->EMSConstructActuatorIsOkay(state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum), SurfNum) = true;
            }

            if ((state.dataRuntimeLang->EMSConstructActuatorChecked(state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum), SurfNum)) &&
                (state.dataRuntimeLang->EMSConstructActuatorIsOkay(state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum), SurfNum))) {

                Surface(SurfNum).Construction = state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum);
                state.dataConstruction->Construct(Surface(SurfNum).Construction).IsUsed = true;
                state.dataSurface->SurfActiveConstruction(SurfNum) = state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum);
                ;

            } else { // have not checked yet or is not okay, so see if we need to warn about incompatible
                if (!state.dataRuntimeLang->EMSConstructActuatorChecked(state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum), SurfNum)) {
                    // check if constructions appear compatible

                    if (Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::CTF ||
                        Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::EMPD) {
                        // compare old construction to new construction and see if terms match
                        // set as okay and turn false if find a big problem
                        state.dataRuntimeLang->EMSConstructActuatorIsOkay(state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum), SurfNum) =
                            true;
                        state.dataRuntimeLang->EMSConstructActuatorChecked(state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum), SurfNum) =
                            true;
                        if (state.dataConstruction->Construct(Surface(SurfNum).Construction).NumHistories !=
                            state.dataConstruction->Construct(state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum)).NumHistories) {
                            // thow warning, but allow
                            ShowWarningError(state,
                                             "InitEMSControlledConstructions: EMS Construction State Actuator may be unrealistic, incompatible "
                                             "CTF timescales are being used.");
                            ShowContinueError(state,
                                              format("Construction named = {} has CTF timesteps = {}",
                                                     state.dataConstruction->Construct(Surface(SurfNum).Construction).Name,
                                                     state.dataConstruction->Construct(Surface(SurfNum).Construction).NumHistories));
                            ShowContinueError(
                                state,
                                format("While construction named = {} has CTF timesteps = {}",
                                       state.dataConstruction->Construct(state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum)).Name,
                                       state.dataConstruction->Construct(state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum)).NumHistories));
                            ShowContinueError(state,
                                              "Transient heat transfer modeling may not be valid for surface name = " + Surface(SurfNum).Name +
                                                  ", and the simulation continues");
                        }
                        if (state.dataConstruction->Construct(Surface(SurfNum).Construction).NumCTFTerms !=
                            state.dataConstruction->Construct(state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum)).NumCTFTerms) {
                            // thow warning, but allow
                            ShowWarningError(state,
                                             "InitEMSControlledConstructions: EMS Construction State Actuator may be unrealistic, incompatible "
                                             "CTF terms are being used.");
                            ShowContinueError(state,
                                              format("Construction named = {} has number of CTF terms = {}",
                                                     state.dataConstruction->Construct(Surface(SurfNum).Construction).Name,
                                                     state.dataConstruction->Construct(Surface(SurfNum).Construction).NumCTFTerms));
                            ShowContinueError(
                                state,
                                format("While construction named = {} has number of CTF terms = {}",
                                       state.dataConstruction->Construct(state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum)).Name,
                                       state.dataConstruction->Construct(state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum)).NumCTFTerms));
                            ShowContinueError(
                                state,
                                "The actuator is allowed but the transient heat transfer modeling may not be valid for surface name = " +
                                    Surface(SurfNum).Name + ", and the simulation continues");
                        }

                        if (state.dataConstruction->Construct(Surface(SurfNum).Construction).SourceSinkPresent) {
                            if (!state.dataConstruction->Construct(state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum)).SourceSinkPresent) {
                                // thow warning, and do not allow
                                ShowSevereError(state, "InitEMSControlledConstructions: EMS Construction State Actuator not valid.");
                                ShowContinueError(state,
                                                  "Construction named = " + state.dataConstruction->Construct(Surface(SurfNum).Construction).Name +
                                                      " has internal source/sink");
                                ShowContinueError(
                                    state,
                                    "While construction named = " +
                                        state.dataConstruction->Construct(state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum)).Name +
                                        " is not an internal source/sink construction");
                                ShowContinueError(state,
                                                  "This actuator is not allowed for surface name = " + Surface(SurfNum).Name +
                                                      ", and the simulation continues without the override");

                                state.dataRuntimeLang->EMSConstructActuatorIsOkay(state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum),
                                                                                  SurfNum) = false;
                            }
                        }

                        if (state.dataRuntimeLang->EMSConstructActuatorIsOkay(state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum),
                                                                              SurfNum)) {
                            Surface(SurfNum).Construction = state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum);
                        }

                    } else if (Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::CondFD) {
                        state.dataRuntimeLang->EMSConstructActuatorIsOkay(state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum), SurfNum) =
                            true;
                        state.dataRuntimeLang->EMSConstructActuatorChecked(state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum), SurfNum) =
                            true;
                        if (state.dataHeatBalFiniteDiffMgr->ConstructFD(Surface(SurfNum).Construction).TotNodes !=
                            state.dataHeatBalFiniteDiffMgr->ConstructFD(state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum)).TotNodes) {
                            // thow warning, and do not allow
                            ShowSevereError(state, "InitEMSControlledConstructions: EMS Construction State Actuator not valid.");
                            ShowContinueError(state,
                                              format("Construction named = {} has number of finite difference nodes ={}",
                                                     state.dataConstruction->Construct(Surface(SurfNum).Construction).Name,
                                                     state.dataHeatBalFiniteDiffMgr->ConstructFD(Surface(SurfNum).Construction).TotNodes));
                            ShowContinueError(
                                state,
                                format("While construction named = {}has number of finite difference nodes ={}",
                                       state.dataConstruction->Construct(state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum)).Name,
                                       state.dataHeatBalFiniteDiffMgr->ConstructFD(state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum))
                                           .TotNodes));
                            ShowContinueError(state,
                                              "This actuator is not allowed for surface name = " + Surface(SurfNum).Name +
                                                  ", and the simulation continues without the override");

                            state.dataRuntimeLang->EMSConstructActuatorIsOkay(state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum), SurfNum) =
                                false;
                        }

                        if (state.dataConstruction->Construct(Surface(SurfNum).Construction).SourceSinkPresent) {
                            if (!state.dataConstruction->Construct(state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum)).SourceSinkPresent) {
                                // thow warning, and do not allow
                                ShowSevereError(state, "InitEMSControlledConstructions: EMS Construction State Actuator not valid.");
                                ShowContinueError(state,
                                                  "Construction named = " + state.dataConstruction->Construct(Surface(SurfNum).Construction).Name +
                                                      " has internal source/sink");
                                ShowContinueError(
                                    state,
                                    "While construction named = " +
                                        state.dataConstruction->Construct(state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum)).Name +
                                        " is not an internal source/sink construction");
                                ShowContinueError(state,
                                                  "This actuator is not allowed for surface name = " + Surface(SurfNum).Name +
                                                      ", and the simulation continues without the override");

                                state.dataRuntimeLang->EMSConstructActuatorIsOkay(state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum),
                                                                                  SurfNum) = false;
                            }
                        }

                        if (state.dataRuntimeLang->EMSConstructActuatorIsOkay(state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum),
                                                                              SurfNum)) {
                            Surface(SurfNum).Construction = state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum);
                        }

                    } else if (Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::HAMT) { // don't allow
                        ShowSevereError(state,
                                        "InitEMSControlledConstructions: EMS Construction State Actuator not available with Heat transfer "
                                        "algorithm CombinedHeatAndMoistureFiniteElement.");
                        ShowContinueError(state,
                                          "This actuator is not allowed for surface name = " + Surface(SurfNum).Name +
                                              ", and the simulation continues without the override");
                        state.dataRuntimeLang->EMSConstructActuatorChecked(state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum), SurfNum) =
                            true;
                        state.dataRuntimeLang->EMSConstructActuatorIsOkay(state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum), SurfNum) =
                            false;

                    } else if (Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::Kiva) { // don't allow
                        ShowSevereError(state,
                                        "InitEMSControlledConstructions: EMS Construction State Actuator not available for Surfaces with "
                                        "Foundation Outside Boundary Condition.");
                        ShowContinueError(state,
                                          "This actuator is not allowed for surface name = " + Surface(SurfNum).Name +
                                              ", and the simulation continues without the override");
                        state.dataRuntimeLang->EMSConstructActuatorChecked(state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum), SurfNum) =
                            true;
                        state.dataRuntimeLang->EMSConstructActuatorIsOkay(state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum), SurfNum) =
                            false;
                    }

                } else {
                    // do nothing, has been checked and is not okay with single warning already issued.
                }
            }
        } else {
            Surface(SurfNum).Construction = Surface(SurfNum).ConstructionStoredInputValue;
        }
    }
}

// End Initialization Section of the Module
//******************************************************************************

// Begin Algorithm Section of the Module
//******************************************************************************

// Beginning of Record Keeping subroutines for the HB Module
// *****************************************************************************

void UpdateFinalSurfaceHeatBalance(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   December 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // If a radiant system is present and was on for part of the time step,
    // then we probably need to make yet another pass through the heat balance.
    // This is necessary because the heat source/sink to the surface that is
    // the radiant system may have varied during the system time steps.

    // METHODOLOGY EMPLOYED:
    // First, determine whether or not the radiant system was running.  If
    // any of the Qsource terms are non-zero, then it was running.  Then,
    // update the current source terms with the "average" value calculated
    // by the radiant system algorithm.  This requires the "USE" of the
    // radiant algorithm module.  Finally, using this source value, redo
    // the inside and outside heat balances.

    using CoolingPanelSimple::UpdateCoolingPanelSourceValAvg;
    using ElectricBaseboardRadiator::UpdateBBElecRadSourceValAvg;
    using HighTempRadiantSystem::UpdateHTRadSourceValAvg;
    using HWBaseboardRadiator::UpdateBBRadSourceValAvg;
    using LowTempRadiantSystem::UpdateRadSysSourceValAvg;
    using SteamBaseboardRadiator::UpdateBBSteamRadSourceValAvg;
    using SwimmingPool::UpdatePoolSourceValAvg;

    bool LowTempRadSysOn;     // .TRUE. if a low temperature radiant system is running
    bool HighTempRadSysOn;    // .TRUE. if a high temperature radiant system is running
    bool HWBaseboardSysOn;    // .TRUE. if a water baseboard heater is running
    bool SteamBaseboardSysOn; // .TRUE. if a steam baseboard heater is running
    bool ElecBaseboardSysOn;  // .TRUE. if a steam baseboard heater is running
    bool CoolingPanelSysOn;   // true if a simple cooling panel is running
    bool SwimmingPoolOn;      // true if a pool is present (running)

    UpdateRadSysSourceValAvg(state, LowTempRadSysOn);
    UpdateHTRadSourceValAvg(state, HighTempRadSysOn);
    UpdateBBRadSourceValAvg(state, HWBaseboardSysOn);
    UpdateBBSteamRadSourceValAvg(state, SteamBaseboardSysOn);
    UpdateBBElecRadSourceValAvg(state, ElecBaseboardSysOn);
    UpdateCoolingPanelSourceValAvg(state, CoolingPanelSysOn);
    UpdatePoolSourceValAvg(state, SwimmingPoolOn);

    if (LowTempRadSysOn || HighTempRadSysOn || HWBaseboardSysOn || SteamBaseboardSysOn || ElecBaseboardSysOn || CoolingPanelSysOn || SwimmingPoolOn) {
        // Solve the zone heat balance 'Detailed' solution
        // Call the outside and inside surface heat balances
        CalcHeatBalanceOutsideSurf(state);
        CalcHeatBalanceInsideSurf(state);
    }
}

void UpdateThermalHistories(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Russ Taylor
    //       DATE WRITTEN   June 1990
    //       MODIFIED       na
    //       RE-ENGINEERED  Mar98 (RKS)

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine updates and shifts the thermal and flux histories.

    // METHODOLOGY EMPLOYED:
    // If a surface runs on the user selected subhourly time step, then the
    // history terms for the temperatures and fluxes must simply be updated
    // and shifted.  However, if the surface runs at a different (longer) time
    // step, then the "master" history series is used for the interpolated
    // update scheme.

    // REFERENCES:
    // (I)BLAST legacy routine UTHRMH
    // Taylor et.al., Impact of Simultaneous Simulation of Buildings and
    // Mechanical Systems in Heat Balance Based Energy Analysis Programs
    // on System Response and Control, Building Simulation '91, IBPSA, Nice, France.

    int HistTermNum; // DO loop counter for history terms
    int SideNum;     // DO loop counter for surfaces sides (inside, outside)

    // Tuned Assure safe to use shared linear indexing below
    assert(equal_dimensions(state.dataHeatBalSurf->TH, state.dataHeatBalSurf->THM));
    assert(equal_dimensions(state.dataHeatBalSurf->TH, state.dataHeatBalSurf->QH));
    assert(equal_dimensions(state.dataHeatBalSurf->TH, state.dataHeatBalSurf->QHM));
    if (state.dataHeatBal->AnyInternalHeatSourceInInput) {
        assert(equal_dimensions(state.dataHeatBalSurf->TsrcHist, state.dataHeatBalSurf->QsrcHist));
        assert(equal_dimensions(state.dataHeatBalSurf->TsrcHist, state.dataHeatBalSurf->TsrcHistM));
        assert(equal_dimensions(state.dataHeatBalSurf->TsrcHistM, state.dataHeatBalSurf->QsrcHistM));
        assert(equal_dimensions(state.dataHeatBalSurf->TuserHist, state.dataHeatBalSurf->QsrcHist));
        assert(equal_dimensions(state.dataHeatBalSurf->TuserHist, state.dataHeatBalSurf->TuserHistM));
        assert(equal_dimensions(state.dataHeatBalSurf->TuserHistM, state.dataHeatBalSurf->QsrcHistM));
    }

    if (state.dataHeatBalSurfMgr->UpdateThermalHistoriesFirstTimeFlag) {
        state.dataHeatBalSurfMgr->QExt1.dimension(state.dataSurface->TotSurfaces, 0.0);
        state.dataHeatBalSurfMgr->QInt1.dimension(state.dataSurface->TotSurfaces, 0.0);
        state.dataHeatBalSurfMgr->TempInt1.dimension(state.dataSurface->TotSurfaces, 0.0);
        state.dataHeatBalSurfMgr->TempExt1.dimension(state.dataSurface->TotSurfaces, 0.0);
        state.dataHeatBalSurfMgr->SumTime.dimension(state.dataSurface->TotSurfaces, 0.0);
        if (state.dataHeatBal->AnyInternalHeatSourceInInput) {
            state.dataHeatBalSurfMgr->Qsrc1.dimension(state.dataSurface->TotSurfaces, 0.0);
            state.dataHeatBalSurfMgr->Tsrc1.dimension(state.dataSurface->TotSurfaces, 0.0);
            state.dataHeatBalSurfMgr->Tuser1.dimension(state.dataSurface->TotSurfaces, 0.0);
        }
        state.dataHeatBalSurfMgr->UpdateThermalHistoriesFirstTimeFlag = false;
    }

    auto const l111(state.dataHeatBalSurf->TH.index(1, 1, 1));
    auto const l211(state.dataHeatBalSurf->TH.index(2, 1, 1));
    auto &Surface(state.dataSurface->Surface);

    for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
        int const firstSurfOpaq = state.dataHeatBal->Zone(zoneNum).OpaqOrIntMassSurfaceFirst;
        int const lastSurfOpaq = state.dataHeatBal->Zone(zoneNum).OpaqOrIntMassSurfaceLast;
        for (int SurfNum = firstSurfOpaq; SurfNum <= lastSurfOpaq; ++SurfNum) {
            // Loop through all (heat transfer) surfaces...  [ l11 ] = ( 1, 1, SurfNum ), [ l21 ] = ( 2, 1, SurfNum )
            int l11 = l111 + SurfNum - 1;
            int l21 = l211 + SurfNum - 1;
            auto const &surface(Surface(SurfNum));

            if ((surface.HeatTransferAlgorithm != DataSurfaces::iHeatTransferModel::CTF) &&
                (surface.HeatTransferAlgorithm != DataSurfaces::iHeatTransferModel::EMPD))
                continue;

            int const ConstrNum(surface.Construction);
            auto const &construct(state.dataConstruction->Construct(ConstrNum));

            if (construct.NumCTFTerms == 0) continue; // Skip surfaces with no history terms

            // Sign convention for the various terms in the following two equations
            // is based on the form of the Conduction Transfer Function equation
            // given by:
            // Qin,now  = (Sum of)(Y Tout) - (Sum of)(Z Tin) + (Sum of)(F Qin,old) + (Sum of)(V Qsrc)
            // Qout,now = (Sum of)(X Tout) - (Sum of)(Y Tin) + (Sum of)(F Qout,old) + (Sum of)(W Qsrc)
            // In both equations, flux is positive from outside to inside.  The V and W terms are for radiant systems only.

            // Set current inside flux:
            Real64 const QH_12 = state.dataHeatBalSurf->QH[l21] =
                state.dataHeatBalSurf->TH[l11] * construct.CTFCross(0) - state.dataHeatBalSurf->TempSurfIn(SurfNum) * construct.CTFInside(0) +
                state.dataHeatBalSurf->CTFConstInPart(SurfNum); // Heat source/sink term for radiant systems
            // Only HT opaq surfaces are evaluated, previous if (surface.Class == SurfaceClass::Floor || surface.Class == SurfaceClass::Wall ||
            // surface.Class == SurfaceClass::IntMass || surface.Class == SurfaceClass::Roof || surface.Class == SurfaceClass::Door) checks are
            // reduncant.
            if (construct.SourceSinkPresent) {
                Real64 const QH_12s = state.dataHeatBalSurf->QH[l21] = QH_12 + state.dataHeatBalSurf->QsrcHist(SurfNum, 1) * construct.CTFSourceIn(0);
                state.dataHeatBalSurf->SurfOpaqInsFaceConduction(SurfNum) = surface.Area * QH_12s;
                state.dataHeatBalSurf->SurfOpaqInsFaceConductionFlux(SurfNum) = QH_12s;
            } else {
                state.dataHeatBalSurf->SurfOpaqInsFaceConduction(SurfNum) = surface.Area * QH_12;
                state.dataHeatBalSurf->SurfOpaqInsFaceConductionFlux(SurfNum) = QH_12; // CR 8901
            }
            //      IF (Surface(SurfNum)%Class/=SurfaceClass::IntMass)  &
            //      ZoneOpaqSurfInsFaceCond(Surface(SurfNum)%Zone) = ZoneOpaqSurfInsFaceCond(Surface(SurfNum)%Zone) + &
            //              OpaqSurfInsFaceConduction(SurfNum)
            state.dataHeatBalSurf->SurfOpaqInsFaceCondGainRep(SurfNum) = 0.0;
            state.dataHeatBalSurf->SurfOpaqInsFaceCondLossRep(SurfNum) = 0.0;
            if (state.dataHeatBalSurf->SurfOpaqInsFaceConduction(SurfNum) >= 0.0) {
                state.dataHeatBalSurf->SurfOpaqInsFaceCondGainRep(SurfNum) = state.dataHeatBalSurf->SurfOpaqInsFaceConduction(SurfNum);
            } else {
                state.dataHeatBalSurf->SurfOpaqInsFaceCondLossRep(SurfNum) = -state.dataHeatBalSurf->SurfOpaqInsFaceConduction(SurfNum);
            }

            // Update the temperature at the source/sink location (if one is present)
            if (construct.SourceSinkPresent) {
                state.dataHeatBalSurf->TempSource(SurfNum) = state.dataHeatBalSurf->TsrcHist(SurfNum, 1) =
                    state.dataHeatBalSurf->TH[l11] * construct.CTFTSourceOut(0) +
                    state.dataHeatBalSurf->TempSurfIn(SurfNum) * construct.CTFTSourceIn(0) +
                    state.dataHeatBalSurf->QsrcHist(SurfNum, 1) * construct.CTFTSourceQ(0) + state.dataHeatBalFanSys->CTFTsrcConstPart(SurfNum);
                state.dataHeatBalSurf->TempUserLoc(SurfNum) = state.dataHeatBalSurf->TuserHist(SurfNum, 1) =
                    state.dataHeatBalSurf->TH[l11] * construct.CTFTUserOut(0) + state.dataHeatBalSurf->TempSurfIn(SurfNum) * construct.CTFTUserIn(0) +
                    state.dataHeatBalSurf->QsrcHist(SurfNum, 1) * construct.CTFTUserSource(0) + state.dataHeatBalFanSys->CTFTuserConstPart(SurfNum);
            }

            if (surface.ExtBoundCond > 0) continue; // Don't need to evaluate outside for partitions

            // Set current outside flux:
            if (construct.SourceSinkPresent) {
                state.dataHeatBalSurf->QH[l11] = state.dataHeatBalSurf->TH[l11] * construct.CTFOutside(0) -
                                                 state.dataHeatBalSurf->TempSurfIn(SurfNum) * construct.CTFCross(0) +
                                                 state.dataHeatBalSurf->QsrcHist(SurfNum, 1) * construct.CTFSourceOut(0) +
                                                 state.dataHeatBalSurf->CTFConstOutPart(SurfNum); // Heat source/sink term for radiant systems
            } else {
                state.dataHeatBalSurf->QH[l11] = state.dataHeatBalSurf->TH[l11] * construct.CTFOutside(0) -
                                                 state.dataHeatBalSurf->TempSurfIn(SurfNum) * construct.CTFCross(0) +
                                                 state.dataHeatBalSurf->CTFConstOutPart(SurfNum);
            }
            state.dataHeatBalSurf->SurfOpaqOutsideFaceConductionFlux(SurfNum) =
                -state.dataHeatBalSurf->QH[l11]; // switch sign for balance at outside face
            state.dataHeatBalSurf->SurfOpaqOutsideFaceConduction(SurfNum) =
                surface.Area * state.dataHeatBalSurf->SurfOpaqOutsideFaceConductionFlux(SurfNum);
        }
    } // ...end of loop over all (heat transfer) surfaces...

    for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
        int const firstSurfOpaq = state.dataHeatBal->Zone(zoneNum).OpaqOrIntMassSurfaceFirst;
        int const lastSurfOpaq = state.dataHeatBal->Zone(zoneNum).OpaqOrIntMassSurfaceLast;
        for (int SurfNum = firstSurfOpaq; SurfNum <= lastSurfOpaq; ++SurfNum) {
            // Loop through all (heat transfer) surfaces...  [ l11 ] = ( 1, 1, SurfNum ), [ l21 ] = ( 2, 1, SurfNum )
            int l11 = l111 + SurfNum - 1;
            int l21 = l211 + SurfNum - 1;
            if ((Surface(SurfNum).HeatTransferAlgorithm != DataSurfaces::iHeatTransferModel::CTF) &&
                (Surface(SurfNum).HeatTransferAlgorithm != DataSurfaces::iHeatTransferModel::EMPD))
                continue;
            if (state.dataHeatBalSurf->SUMH(SurfNum) == 0) { // First time step in a block for a surface, update arrays
                state.dataHeatBalSurfMgr->TempExt1(SurfNum) = state.dataHeatBalSurf->TH[l11];
                state.dataHeatBalSurfMgr->TempInt1(SurfNum) = state.dataHeatBalSurf->TempSurfIn(SurfNum);
                state.dataHeatBalSurfMgr->QExt1(SurfNum) = state.dataHeatBalSurf->QH[l11];
                state.dataHeatBalSurfMgr->QInt1(SurfNum) = state.dataHeatBalSurf->QH[l21];
                if (state.dataHeatBal->AnyInternalHeatSourceInInput) {
                    state.dataHeatBalSurfMgr->Tsrc1(SurfNum) = state.dataHeatBalSurf->TsrcHist(SurfNum, 1);
                    state.dataHeatBalSurfMgr->Tuser1(SurfNum) = state.dataHeatBalSurf->TuserHist(SurfNum, 1);
                    state.dataHeatBalSurfMgr->Qsrc1(SurfNum) = state.dataHeatBalSurf->QsrcHist(SurfNum, 1);
                }
            }
        }

    } // ...end of loop over all (heat transfer) surfaces...

    // SHIFT TEMPERATURE AND FLUX HISTORIES:
    // SHIFT AIR TEMP AND FLUX SHIFT VALUES WHEN AT BOTTOM OF ARRAY SPACE.

    for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
        int const firstSurfOpaq = state.dataHeatBal->Zone(zoneNum).OpaqOrIntMassSurfaceFirst;
        int const lastSurfOpaq = state.dataHeatBal->Zone(zoneNum).OpaqOrIntMassSurfaceLast;
        for (int SurfNum = firstSurfOpaq; SurfNum <= lastSurfOpaq; ++SurfNum) {
            auto const &surface(Surface(SurfNum));

            if ((surface.HeatTransferAlgorithm != DataSurfaces::iHeatTransferModel::CTF) &&
                (surface.HeatTransferAlgorithm != DataSurfaces::iHeatTransferModel::EMPD))
                continue;

            int const ConstrNum(surface.Construction);
            auto const &construct(state.dataConstruction->Construct(ConstrNum));

            ++state.dataHeatBalSurf->SUMH(SurfNum);
            state.dataHeatBalSurfMgr->SumTime(SurfNum) = double(state.dataHeatBalSurf->SUMH(SurfNum)) * state.dataGlobal->TimeStepZone;

            if (state.dataHeatBalSurf->SUMH(SurfNum) == construct.NumHistories) {

                state.dataHeatBalSurf->SUMH(SurfNum) = 0;

                if (construct.NumCTFTerms > 1) {
                    int const numCTFTerms(construct.NumCTFTerms);
                    for (SideNum = 1; SideNum <= 2; ++SideNum) { // Tuned Index order switched for cache friendliness
                        auto l(state.dataHeatBalSurf->THM.index(SideNum, numCTFTerms, SurfNum));
                        auto const li(state.dataHeatBalSurf->THM.size3());
                        auto l1(l + li);
                        for (HistTermNum = numCTFTerms + 1; HistTermNum >= 3; --HistTermNum, l1 = l, l -= li) { // Tuned Linear indexing
                            // TH( SideNum, HistTermNum, SurfNum ) = THM( SideNum, HistTermNum, SurfNum ) = THM( SideNum, HistTermNum - 1, SurfNum );
                            // QH( SideNum, HistTermNum, SurfNum ) = QHM( SideNum, HistTermNum, SurfNum ) = QHM( SideNum, HistTermNum - 1, SurfNum );
                            state.dataHeatBalSurf->TH[l1] = state.dataHeatBalSurf->THM[l1] = state.dataHeatBalSurf->THM[l];
                            state.dataHeatBalSurf->QH[l1] = state.dataHeatBalSurf->QHM[l1] = state.dataHeatBalSurf->QHM[l];
                        }
                    }
                    if (construct.SourceSinkPresent) {
                        auto m(state.dataHeatBalSurf->TsrcHistM.index(SurfNum, numCTFTerms));
                        auto m1(m + 1);
                        for (HistTermNum = numCTFTerms + 1; HistTermNum >= 3; --HistTermNum, --m, --m1) { // Tuned Linear indexing
                            // TsrcHist( SurfNum, HistTerm ) = TsrcHistM( SurfNum, HHistTerm ) = TsrcHistM( SurfNum, HistTermNum - 1 );
                            // QsrcHist( SurfNum, HistTerm ) = QsrcHistM( SurfNum, HHistTerm ) = QsrcHistM( SurfNum, HistTermNum - 1 );
                            state.dataHeatBalSurf->TsrcHist[m1] = state.dataHeatBalSurf->TsrcHistM[m1] = state.dataHeatBalSurf->TsrcHistM[m];
                            state.dataHeatBalSurf->QsrcHist[m1] = state.dataHeatBalSurf->QsrcHistM[m1] = state.dataHeatBalSurf->QsrcHistM[m];
                            state.dataHeatBalSurf->TuserHist[m1] = state.dataHeatBalSurf->TuserHistM[m1] = state.dataHeatBalSurf->TuserHistM[m];
                        }
                    }
                }

                // Tuned Linear indexing
                // THM( 1, 2, SurfNum ) = TempExt1( SurfNum );
                // THM( 2, 2, SurfNum ) = TempInt1( SurfNum );
                // TsrcHistM( SurfNum, 2 ) = Tsrc1( SurfNum );
                // QHM( 1, 2, SurfNum ) = QExt1( SurfNum );
                // QHM( 2, 2, SurfNum ) = QInt1( SurfNum );
                // QsrcHistM( SurfNum, 2 ) = Qsrc1( SurfNum );
                //
                // TH( 1, 2, SurfNum ) = THM( 1, 2, SurfNum );
                // TH( 2, 2, SurfNum ) = THM( 2, 2, SurfNum );
                // TsrcHist( SurfNum, 2 ) = TsrcHistM( SurfNum, 2 );
                // QH( 1, 2, SurfNum ) = QHM( 1, 2, SurfNum );
                // QH( 2, 2, SurfNum ) = QHM( 2, 2, SurfNum );
                // QsrcHist( SurfNum, 2 ) = QsrcHistM( SurfNum, 2 );

                auto const l21(state.dataHeatBalSurf->TH.index(1, 2, SurfNum)); // Linear index
                auto const l22(state.dataHeatBalSurf->TH.index(2, 2, SurfNum)); // Linear index
                state.dataHeatBalSurf->THM[l21] = state.dataHeatBalSurfMgr->TempExt1(SurfNum);
                state.dataHeatBalSurf->THM[l22] = state.dataHeatBalSurfMgr->TempInt1(SurfNum);
                state.dataHeatBalSurf->QHM[l21] = state.dataHeatBalSurfMgr->QExt1(SurfNum);
                state.dataHeatBalSurf->QHM[l22] = state.dataHeatBalSurfMgr->QInt1(SurfNum);

                state.dataHeatBalSurf->TH[l21] = state.dataHeatBalSurf->THM[l21];
                state.dataHeatBalSurf->TH[l22] = state.dataHeatBalSurf->THM(2, 2, SurfNum);
                state.dataHeatBalSurf->QH[l21] = state.dataHeatBalSurf->QHM[l21];
                state.dataHeatBalSurf->QH[l22] = state.dataHeatBalSurf->QHM(2, 2, SurfNum);

                if (construct.SourceSinkPresent) {
                    state.dataHeatBalSurf->TsrcHistM(SurfNum, 2) = state.dataHeatBalSurfMgr->Tsrc1(SurfNum);
                    state.dataHeatBalSurf->TuserHistM(SurfNum, 2) = state.dataHeatBalSurfMgr->Tuser1(SurfNum);
                    state.dataHeatBalSurf->QsrcHistM(SurfNum, 2) = state.dataHeatBalSurfMgr->Qsrc1(SurfNum);
                    state.dataHeatBalSurf->TsrcHist(SurfNum, 2) = state.dataHeatBalSurf->TsrcHistM(SurfNum, 2);
                    state.dataHeatBalSurf->TuserHist(SurfNum, 2) = state.dataHeatBalSurf->TuserHistM(SurfNum, 2);
                    state.dataHeatBalSurf->QsrcHist(SurfNum, 2) = state.dataHeatBalSurf->QsrcHistM(SurfNum, 2);
                }

            } else {

                Real64 const sum_steps(state.dataHeatBalSurfMgr->SumTime(SurfNum) / construct.CTFTimeStep);
                if (construct.NumCTFTerms > 1) {
                    int const numCTFTerms(construct.NumCTFTerms);
                    for (SideNum = 1; SideNum <= 2; ++SideNum) { // Tuned Index order switched for cache friendliness
                        auto l(state.dataHeatBalSurf->THM.index(SideNum, numCTFTerms, SurfNum));
                        auto const s3(state.dataHeatBalSurf->THM.size3());
                        auto l1(l + s3);
                        for (HistTermNum = numCTFTerms + 1; HistTermNum >= 3; --HistTermNum, l1 = l, l -= s3) { // Tuned Linear indexing
                            // Real64 const THM_l1( THM( SideNum, HistTermNum, SurfNum ) );
                            // TH( SideNum, HistTermNum, SurfNum ) = THM_l1 - ( THM_l1 - THM( SideNum, HistTermNum - 1, SurfNum ) ) * sum_steps;
                            // Real64 const QHM_l1( QHM( SideNum, HistTermNum, SurfNum ) );
                            // QH( SideNum, HistTermNum, SurfNum ) = QHM_l1 - ( QHM_l1 - QHM( SideNum, HistTermNum - 1, SurfNum ) ) * sum_steps;
                            Real64 const THM_l1(state.dataHeatBalSurf->THM[l1]);
                            state.dataHeatBalSurf->TH[l1] = THM_l1 - (THM_l1 - state.dataHeatBalSurf->THM[l]) * sum_steps;
                            Real64 const QHM_l1(state.dataHeatBalSurf->QHM[l1]);
                            state.dataHeatBalSurf->QH[l1] = QHM_l1 - (QHM_l1 - state.dataHeatBalSurf->QHM[l]) * sum_steps;
                        }
                    }
                    if (construct.SourceSinkPresent) {
                        auto m(state.dataHeatBalSurf->TsrcHistM.index(SurfNum, numCTFTerms));
                        auto m1(m + 1);
                        for (HistTermNum = numCTFTerms + 1; HistTermNum >= 3; --HistTermNum, --m, --m1) { // Tuned Linear indexing [ l ] == ()
                            // Real64 const TsrcHistM_elem( TsrcHistM( SurfNum, HistTermNum ) );
                            // TsrcHist( SurfNum, HistTermNum ) = TsrcHistM_elem - ( TsrcHistM_elem - TsrcHistM( SurfNum, HistTermNum - 1 ) ) *
                            // sum_steps;  Real64 const QsrcHistM_elem( QsrcHistM( SurfNum, HistTermNum ) );  QsrcHist( SurfNum, HistTermNum ) =
                            // QsrcHistM_elem - ( QsrcHistM_elem - QsrcHistM( SurfNum, HistTermNum - 1 ) ) * sum_steps;
                            Real64 const TsrcHistM_m1(state.dataHeatBalSurf->TsrcHistM[m1]);
                            state.dataHeatBalSurf->TsrcHist[m1] = TsrcHistM_m1 - (TsrcHistM_m1 - state.dataHeatBalSurf->TsrcHistM[m]) * sum_steps;
                            Real64 const QsrcHistM_m1(state.dataHeatBalSurf->QsrcHistM[m1]);
                            state.dataHeatBalSurf->QsrcHist[m1] = QsrcHistM_m1 - (QsrcHistM_m1 - state.dataHeatBalSurf->QsrcHistM[m]) * sum_steps;
                            Real64 const TuserHistM_m1(state.dataHeatBalSurf->TuserHistM[m1]);
                            state.dataHeatBalSurf->TuserHist[m1] = TuserHistM_m1 - (TuserHistM_m1 - state.dataHeatBalSurf->TuserHistM[m]) * sum_steps;
                        }
                    }
                }

                // Tuned Linear indexing
                // TH( 1, 2, SurfNum ) = THM( 1, 2, SurfNum ) - ( THM( 1, 2, SurfNum ) - TempExt1( SurfNum ) ) * sum_steps;
                // TH( 2, 2, SurfNum ) = THM( 2, 2, SurfNum ) - ( THM( 2, 2, SurfNum ) - TempInt1( SurfNum ) ) * sum_steps;
                // QH( 1, 2, SurfNum ) = QHM( 1, 2, SurfNum ) - ( QHM( 1, 2, SurfNum ) - QExt1( SurfNum ) ) * sum_steps;
                // QH( 2, 2, SurfNum ) = QHM( 2, 2, SurfNum ) - ( QHM( 2, 2, SurfNum ) - QInt1( SurfNum ) ) * sum_steps;

                auto const l21(state.dataHeatBalSurf->TH.index(1, 2, SurfNum)); // Linear index
                auto const l22(state.dataHeatBalSurf->TH.index(2, 2, SurfNum)); // Linear index
                state.dataHeatBalSurf->TH[l21] =
                    state.dataHeatBalSurf->THM[l21] - (state.dataHeatBalSurf->THM[l21] - state.dataHeatBalSurfMgr->TempExt1(SurfNum)) * sum_steps;
                state.dataHeatBalSurf->TH[l22] =
                    state.dataHeatBalSurf->THM[l22] - (state.dataHeatBalSurf->THM[l22] - state.dataHeatBalSurfMgr->TempInt1(SurfNum)) * sum_steps;
                state.dataHeatBalSurf->QH[l21] =
                    state.dataHeatBalSurf->QHM[l21] - (state.dataHeatBalSurf->QHM[l21] - state.dataHeatBalSurfMgr->QExt1(SurfNum)) * sum_steps;
                state.dataHeatBalSurf->QH[l22] =
                    state.dataHeatBalSurf->QHM[l22] - (state.dataHeatBalSurf->QHM[l22] - state.dataHeatBalSurfMgr->QInt1(SurfNum)) * sum_steps;

                // Tuned Linear indexing
                // TsrcHist( SurfNum, 2 ) = TsrcHistM( SurfNum, 2 ) - ( TsrcHistM( SurfNum, 2 ) - Tsrc1( SurfNum ) ) * sum_steps;
                // QsrcHist( SurfNum, 2 ) = QsrcHistM( SurfNum, 2 ) - ( QsrcHistM( SurfNum, 2 ) - Qsrc1( SurfNum ) ) * sum_steps;

                if (construct.SourceSinkPresent) {
                    auto const l2(state.dataHeatBalSurf->TsrcHist.index(SurfNum, 2));
                    state.dataHeatBalSurf->TsrcHist[l2] =
                        state.dataHeatBalSurf->TsrcHistM[l2] -
                        (state.dataHeatBalSurf->TsrcHistM[l2] - state.dataHeatBalSurfMgr->Tsrc1(SurfNum)) * sum_steps;
                    state.dataHeatBalSurf->QsrcHist[l2] =
                        state.dataHeatBalSurf->QsrcHistM[l2] -
                        (state.dataHeatBalSurf->QsrcHistM[l2] - state.dataHeatBalSurfMgr->Qsrc1(SurfNum)) * sum_steps;
                    state.dataHeatBalSurf->TuserHist[l2] =
                        state.dataHeatBalSurf->TuserHistM[l2] -
                        (state.dataHeatBalSurf->TuserHistM[l2] - state.dataHeatBalSurfMgr->Tuser1(SurfNum)) * sum_steps;
                }
            }
        }
    } // ...end of loop over all (heat transfer) surfaces
}

void CalculateZoneMRT(EnergyPlusData &state, Optional_int_const ZoneToResimulate) // if passed in, then only calculate surfaces that have this zone
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   November 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculates the current zone MRT for thermal comfort and radiation
    // calculation purposes.

    Real64 SumAET; // Intermediate calculational variable (area*emissivity*T) sum
    int SurfNum;   // Surface number
    int ZoneNum;   // Zone number

    auto &Surface(state.dataSurface->Surface);

    if (state.dataHeatBalSurfMgr->CalculateZoneMRTfirstTime) {
        state.dataHeatBalSurfMgr->SurfaceAE.allocate(state.dataSurface->TotSurfaces);
        state.dataHeatBalSurfMgr->ZoneAESum.allocate(state.dataGlobal->NumOfZones);
        state.dataHeatBalSurfMgr->SurfaceAE = 0.0;
        state.dataHeatBalSurfMgr->ZoneAESum = 0.0;
        for (SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
            if (Surface(SurfNum).HeatTransSurf) {
                state.dataHeatBalSurfMgr->SurfaceAE(SurfNum) =
                    Surface(SurfNum).Area * state.dataConstruction->Construct(Surface(SurfNum).Construction).InsideAbsorpThermal;
                ZoneNum = Surface(SurfNum).Zone;
                if (ZoneNum > 0) state.dataHeatBalSurfMgr->ZoneAESum(ZoneNum) += state.dataHeatBalSurfMgr->SurfaceAE(SurfNum);
            }
        }
    }

    for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
        if (present(ZoneToResimulate) && (ZoneNum != ZoneToResimulate)) continue;
        SumAET = 0.0;
        for (SurfNum = state.dataHeatBal->Zone(ZoneNum).HTSurfaceFirst; SurfNum <= state.dataHeatBal->Zone(ZoneNum).HTSurfaceLast; ++SurfNum) {
            SumAET += state.dataHeatBalSurfMgr->SurfaceAE(SurfNum) * state.dataHeatBalSurf->TempSurfIn(SurfNum);
        }
        if (state.dataHeatBalSurfMgr->ZoneAESum(ZoneNum) > 0.01) {
            state.dataHeatBal->ZoneMRT(ZoneNum) = SumAET / state.dataHeatBalSurfMgr->ZoneAESum(ZoneNum);
        } else {
            if (state.dataHeatBalSurfMgr->CalculateZoneMRTfirstTime) {
                ShowWarningError(
                    state, "Zone areas*inside surface emissivities are summing to zero, for Zone=\"" + state.dataHeatBal->Zone(ZoneNum).Name + "\"");
                ShowContinueError(state, "As a result, MRT will be set to MAT for that zone");
            }
            state.dataHeatBal->ZoneMRT(ZoneNum) = state.dataHeatBalFanSys->MAT(ZoneNum);
        }
    }

    state.dataHeatBalSurfMgr->CalculateZoneMRTfirstTime = false;
}

// End of Record Keeping subroutines for the HB Module
// *****************************************************************************

// Beginning of Reporting subroutines for the HB Module
// *****************************************************************************

void CalcThermalResilience(EnergyPlusData &state)
{
    // This function calculate timestep-wise heat index and humidex.

    // The computation of the heat index is a refinement of a result obtained by multiple regression analysis
    // carried out by Lans P. Rothfusz and described in a 1990 National Weather Service (NWS)
    // Technical Attachment (SR 90-23).
    // Reference: https://www.wpc.ncep.noaa.gov/html/heatindex_equation.shtml

    // The current formula for determining the humidex was developed by J. M. Masterton and F. A. Richardson of
    // Canada's Atmospheric Environment Service in 1979.
    // Reference: Masterson, J., and F. Richardson, 1979: Humidex, a method of quantifying human
    // discomfort due to excessive heat and humidity CLI 1-79, Environment Canada, Atmosheric Environment Servic
    //        using OutputProcessor::ReqRepVars;
    if (state.dataHeatBalSurfMgr->ManageSurfaceHeatBalancefirstTime) {
        for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            SetupOutputVariable(state,
                                "Zone Heat Index",
                                OutputProcessor::Unit::C,
                                state.dataHeatBalFanSys->ZoneHeatIndex(ZoneNum),
                                "Zone",
                                "State",
                                state.dataHeatBal->Zone(ZoneNum).Name);
            SetupOutputVariable(state,
                                "Zone Humidity Index",
                                OutputProcessor::Unit::None,
                                state.dataHeatBalFanSys->ZoneHumidex(ZoneNum),
                                "Zone",
                                "State",
                                state.dataHeatBal->Zone(ZoneNum).Name);
        }
        for (int Loop = 1; Loop <= state.dataOutputProcessor->NumOfReqVariables; ++Loop) {
            if (state.dataOutputProcessor->ReqRepVars(Loop).VarName == "Zone Heat Index") {
                state.dataHeatBalSurfMgr->reportVarHeatIndex = true;
            }
            if (state.dataOutputProcessor->ReqRepVars(Loop).VarName == "Zone Humidity") {
                state.dataHeatBalSurfMgr->reportVarHumidex = true;
            }
        }
    }

    // Constance for heat index regression equation of Rothfusz.
    Real64 c1 = -42.379;
    Real64 c2 = 2.04901523;
    Real64 c3 = 10.14333127;
    Real64 c4 = -.22475541;
    Real64 c5 = -.00683783;
    Real64 c6 = -.05481717;
    Real64 c7 = .00122874;
    Real64 c8 = .00085282;
    Real64 c9 = -.00000199;

    // Calculate Heat Index and Humidex.
    // The heat index equation set is fit to Fahrenheit units, so the zone air temperature values are first convert to F,
    // then heat index is calculated and converted back to C.
    if (state.dataHeatBalSurfMgr->reportVarHeatIndex || state.dataOutRptTab->displayThermalResilienceSummary) {
        for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            Real64 ZoneT = state.dataHeatBalFanSys->ZTAV(ZoneNum);
            Real64 ZoneW = state.dataHeatBalFanSys->ZoneAirHumRatAvg(ZoneNum);
            Real64 ZoneRH = Psychrometrics::PsyRhFnTdbWPb(state, ZoneT, ZoneW, state.dataEnvrn->OutBaroPress) * 100.0;
            Real64 ZoneTF = ZoneT * (9.0 / 5.0) + 32.0;
            Real64 HI;

            if (ZoneTF < 80) {
                HI = 0.5 * (ZoneTF + 61.0 + (ZoneTF - 68.0) * 1.2 + (ZoneRH * 0.094));
            } else {
                HI = c1 + c2 * ZoneTF + c3 * ZoneRH + c4 * ZoneTF * ZoneRH + c5 * ZoneTF * ZoneTF + c6 * ZoneRH * ZoneRH +
                     c7 * ZoneTF * ZoneTF * ZoneRH + c8 * ZoneTF * ZoneRH * ZoneRH + c9 * ZoneTF * ZoneTF * ZoneRH * ZoneRH;
                if (ZoneRH < 13 && ZoneTF < 112) {
                    HI -= (13 - ZoneRH) / 4 * std::sqrt((17 - abs(ZoneTF - 95)) / 17);
                } else if (ZoneRH > 85 && ZoneTF < 87) {
                    HI += (ZoneRH - 85) / 10 * (87 - ZoneTF) / 5;
                }
            }
            HI = (HI - 32.0) * (5.0 / 9.0);
            state.dataHeatBalFanSys->ZoneHeatIndex(ZoneNum) = HI;
        }
    }
    if (state.dataHeatBalSurfMgr->reportVarHumidex || state.dataOutRptTab->displayThermalResilienceSummary) {
        for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            Real64 ZoneW = state.dataHeatBalFanSys->ZoneAirHumRatAvg(ZoneNum);
            Real64 ZoneT = state.dataHeatBalFanSys->ZTAV(ZoneNum);
            Real64 TDewPointK = Psychrometrics::PsyTdpFnWPb(state, ZoneW, state.dataEnvrn->OutBaroPress) + DataGlobalConstants::KelvinConv;
            Real64 e = 6.11 * std::exp(5417.7530 * ((1 / 273.16) - (1 / TDewPointK)));
            Real64 h = 5.0 / 9.0 * (e - 10.0);
            Real64 Humidex = ZoneT + h;
            state.dataHeatBalFanSys->ZoneHumidex(ZoneNum) = Humidex;
        }
    }
}

void ReportThermalResilience(EnergyPlusData &state)
{

    int HINoBins = 5;      // Heat Index range - number of bins
    int HumidexNoBins = 5; // Humidex range - number of bins
    int SETNoBins = 4;     // SET report column numbers

    if (state.dataHeatBalSurfMgr->reportThermalResilienceFirstTime) {
        if (state.dataHeatBal->TotPeople == 0) state.dataHeatBalSurfMgr->hasPierceSET = false;
        for (int iPeople = 1; iPeople <= state.dataHeatBal->TotPeople; ++iPeople) {
            if (!state.dataHeatBal->People(iPeople).Pierce) {
                state.dataHeatBalSurfMgr->hasPierceSET = false;
            }
        }
        for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            state.dataHeatBalFanSys->ZoneHeatIndexHourBins(ZoneNum).assign(HINoBins, 0.0);
            state.dataHeatBalFanSys->ZoneHeatIndexOccuHourBins(ZoneNum).assign(HINoBins, 0.0);
            state.dataHeatBalFanSys->ZoneHumidexHourBins(ZoneNum).assign(HumidexNoBins, 0.0);
            state.dataHeatBalFanSys->ZoneHumidexOccuHourBins(ZoneNum).assign(HumidexNoBins, 0.0);
            if (state.dataHeatBalSurfMgr->hasPierceSET) {
                state.dataHeatBalFanSys->ZoneLowSETHours(ZoneNum).assign(SETNoBins, 0.0);
                state.dataHeatBalFanSys->ZoneHighSETHours(ZoneNum).assign(SETNoBins, 0.0);
            }
        }
        state.dataHeatBalSurfMgr->lowSETLongestHours.assign(state.dataGlobal->NumOfZones, 0.0);
        state.dataHeatBalSurfMgr->highSETLongestHours.assign(state.dataGlobal->NumOfZones, 0.0);
        state.dataHeatBalSurfMgr->lowSETLongestStart.assign(state.dataGlobal->NumOfZones, 0.0);
        state.dataHeatBalSurfMgr->highSETLongestStart.assign(state.dataGlobal->NumOfZones, 0.0);
        state.dataHeatBalSurfMgr->reportThermalResilienceFirstTime = false;
    }

    // Count hours only during weather simulation periods
    if (DataGlobalConstants::KindOfSim::RunPeriodWeather == state.dataGlobal->KindOfSim && !state.dataGlobal->WarmupFlag) {
        // Trace current time step Zone Pierce SET; NaN if no occupant or SET not calculated
        // Record last time step SET to trace SET unmet duration;
        for (int iPeople = 1; iPeople <= state.dataHeatBal->TotPeople; ++iPeople) {
            int ZoneNum = state.dataHeatBal->People(iPeople).ZonePtr;
            state.dataHeatBalFanSys->ZoneNumOcc(ZoneNum) = state.dataHeatBal->People(iPeople).NumberOfPeople *
                                                           GetCurrentScheduleValue(state, state.dataHeatBal->People(iPeople).NumberOfPeoplePtr);
            state.dataHeatBalFanSys->ZoneOccPierceSETLastStep(ZoneNum) = state.dataHeatBalFanSys->ZoneOccPierceSET(ZoneNum);
            if (state.dataHeatBalFanSys->ZoneNumOcc(ZoneNum) > 0) {
                if (state.dataHeatBal->People(iPeople).Pierce) {
                    state.dataHeatBalFanSys->ZoneOccPierceSET(ZoneNum) = state.dataThermalComforts->ThermalComfortData(iPeople).PierceSET;
                } else {
                    state.dataHeatBalFanSys->ZoneOccPierceSET(ZoneNum) = -1;
                }
            } else {
                state.dataHeatBalFanSys->ZoneOccPierceSET(ZoneNum) = -1;
            }
        }
        for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            Real64 HI = state.dataHeatBalFanSys->ZoneHeatIndex(ZoneNum);
            Real64 Humidex = state.dataHeatBalFanSys->ZoneHumidex(ZoneNum);

            int NumOcc = state.dataHeatBalFanSys->ZoneNumOcc(ZoneNum);
            if (HI <= 26.7) {
                state.dataHeatBalFanSys->ZoneHeatIndexHourBins(ZoneNum)[0] += state.dataGlobal->TimeStepZone;
                state.dataHeatBalFanSys->ZoneHeatIndexOccuHourBins(ZoneNum)[0] += NumOcc * state.dataGlobal->TimeStepZone;
            } else if (HI > 26.7 && HI <= 32.2) {
                state.dataHeatBalFanSys->ZoneHeatIndexHourBins(ZoneNum)[1] += state.dataGlobal->TimeStepZone;
                state.dataHeatBalFanSys->ZoneHeatIndexOccuHourBins(ZoneNum)[1] += NumOcc * state.dataGlobal->TimeStepZone;
            } else if (HI > 32.2 && HI <= 39.4) {
                state.dataHeatBalFanSys->ZoneHeatIndexHourBins(ZoneNum)[2] += state.dataGlobal->TimeStepZone;
                state.dataHeatBalFanSys->ZoneHeatIndexOccuHourBins(ZoneNum)[2] += NumOcc * state.dataGlobal->TimeStepZone;
            } else if (HI > 39.4 && HI <= 51.7) {
                state.dataHeatBalFanSys->ZoneHeatIndexHourBins(ZoneNum)[3] += state.dataGlobal->TimeStepZone;
                state.dataHeatBalFanSys->ZoneHeatIndexOccuHourBins(ZoneNum)[3] += NumOcc * state.dataGlobal->TimeStepZone;
            } else {
                state.dataHeatBalFanSys->ZoneHeatIndexHourBins(ZoneNum)[4] += state.dataGlobal->TimeStepZone;
                state.dataHeatBalFanSys->ZoneHeatIndexOccuHourBins(ZoneNum)[4] += NumOcc * state.dataGlobal->TimeStepZone;
            }

            if (Humidex <= 29) {
                state.dataHeatBalFanSys->ZoneHumidexHourBins(ZoneNum)[0] += state.dataGlobal->TimeStepZone;
                state.dataHeatBalFanSys->ZoneHumidexOccuHourBins(ZoneNum)[0] += NumOcc * state.dataGlobal->TimeStepZone;
            } else if (Humidex > 29 && Humidex <= 40) {
                state.dataHeatBalFanSys->ZoneHumidexHourBins(ZoneNum)[1] += state.dataGlobal->TimeStepZone;
                state.dataHeatBalFanSys->ZoneHumidexOccuHourBins(ZoneNum)[1] += NumOcc * state.dataGlobal->TimeStepZone;
            } else if (Humidex > 40 && Humidex <= 45) {
                state.dataHeatBalFanSys->ZoneHumidexHourBins(ZoneNum)[2] += state.dataGlobal->TimeStepZone;
                state.dataHeatBalFanSys->ZoneHumidexOccuHourBins(ZoneNum)[2] += NumOcc * state.dataGlobal->TimeStepZone;
            } else if (Humidex > 45 && Humidex <= 50) {
                state.dataHeatBalFanSys->ZoneHumidexHourBins(ZoneNum)[3] += state.dataGlobal->TimeStepZone;
                state.dataHeatBalFanSys->ZoneHumidexOccuHourBins(ZoneNum)[3] += NumOcc * state.dataGlobal->TimeStepZone;
            } else {
                state.dataHeatBalFanSys->ZoneHumidexHourBins(ZoneNum)[4] += state.dataGlobal->TimeStepZone;
                state.dataHeatBalFanSys->ZoneHumidexOccuHourBins(ZoneNum)[4] += NumOcc * state.dataGlobal->TimeStepZone;
            }

            if (state.dataHeatBalSurfMgr->hasPierceSET) {
                int encodedMonDayHrMin;
                if (NumOcc > 0) {
                    Real64 PierceSET = state.dataHeatBalFanSys->ZoneOccPierceSET(ZoneNum);
                    Real64 PierceSETLast = state.dataHeatBalFanSys->ZoneOccPierceSETLastStep(ZoneNum);

                    if (PierceSET <= 12.2) {
                        state.dataHeatBalFanSys->ZoneLowSETHours(ZoneNum)[0] += (12.2 - PierceSET) * state.dataGlobal->TimeStepZone;
                        state.dataHeatBalFanSys->ZoneLowSETHours(ZoneNum)[1] += (12.2 - PierceSET) * NumOcc * state.dataGlobal->TimeStepZone;
                        // Reset duration when last step is out of range.
                        if (PierceSETLast == -1 || PierceSETLast > 12.2) {
                            General::EncodeMonDayHrMin(encodedMonDayHrMin,
                                                       state.dataEnvrn->Month,
                                                       state.dataEnvrn->DayOfMonth,
                                                       state.dataGlobal->HourOfDay,
                                                       state.dataGlobal->TimeStepZone * (state.dataGlobal->TimeStep - 1) * 60);
                            state.dataHeatBalSurfMgr->lowSETLongestHours[ZoneNum - 1] = 0;
                            state.dataHeatBalSurfMgr->lowSETLongestStart[ZoneNum - 1] = encodedMonDayHrMin;
                        }
                        // Keep the longest duration record.
                        state.dataHeatBalSurfMgr->lowSETLongestHours[ZoneNum - 1] += state.dataGlobal->TimeStepZone;
                        if (state.dataHeatBalSurfMgr->lowSETLongestHours[ZoneNum - 1] > state.dataHeatBalFanSys->ZoneLowSETHours(ZoneNum)[2]) {
                            state.dataHeatBalFanSys->ZoneLowSETHours(ZoneNum)[2] = state.dataHeatBalSurfMgr->lowSETLongestHours[ZoneNum - 1];
                            state.dataHeatBalFanSys->ZoneLowSETHours(ZoneNum)[3] = state.dataHeatBalSurfMgr->lowSETLongestStart[ZoneNum - 1];
                        }
                    } else if (PierceSET > 30) {
                        state.dataHeatBalFanSys->ZoneHighSETHours(ZoneNum)[0] += (PierceSET - 30) * state.dataGlobal->TimeStepZone;
                        state.dataHeatBalFanSys->ZoneHighSETHours(ZoneNum)[1] += (PierceSET - 30) * NumOcc * state.dataGlobal->TimeStepZone;
                        if (PierceSETLast == -1 || PierceSETLast <= 30) {
                            General::EncodeMonDayHrMin(encodedMonDayHrMin,
                                                       state.dataEnvrn->Month,
                                                       state.dataEnvrn->DayOfMonth,
                                                       state.dataGlobal->HourOfDay,
                                                       state.dataGlobal->TimeStepZone * (state.dataGlobal->TimeStep - 1) * 60);
                            state.dataHeatBalSurfMgr->highSETLongestHours[ZoneNum - 1] = 0;
                            state.dataHeatBalSurfMgr->highSETLongestStart[ZoneNum - 1] = encodedMonDayHrMin;
                        }
                        state.dataHeatBalSurfMgr->highSETLongestHours[ZoneNum - 1] += state.dataGlobal->TimeStepZone;
                        if (state.dataHeatBalSurfMgr->highSETLongestHours[ZoneNum - 1] > state.dataHeatBalFanSys->ZoneHighSETHours(ZoneNum)[2]) {
                            state.dataHeatBalFanSys->ZoneHighSETHours(ZoneNum)[2] = state.dataHeatBalSurfMgr->highSETLongestHours[ZoneNum - 1];
                            state.dataHeatBalFanSys->ZoneHighSETHours(ZoneNum)[3] = state.dataHeatBalSurfMgr->highSETLongestStart[ZoneNum - 1];
                        }
                    }
                } else {
                    // No occupants: record the last time step duration if longer than the record.
                    if (state.dataHeatBalSurfMgr->lowSETLongestHours[ZoneNum - 1] > state.dataHeatBalFanSys->ZoneLowSETHours(ZoneNum)[2]) {
                        state.dataHeatBalFanSys->ZoneLowSETHours(ZoneNum)[2] = state.dataHeatBalSurfMgr->lowSETLongestHours[ZoneNum - 1];
                        state.dataHeatBalFanSys->ZoneLowSETHours(ZoneNum)[3] = state.dataHeatBalSurfMgr->lowSETLongestStart[ZoneNum - 1];
                    }
                    if (state.dataHeatBalSurfMgr->highSETLongestHours[ZoneNum - 1] > state.dataHeatBalFanSys->ZoneHighSETHours(ZoneNum)[2]) {
                        state.dataHeatBalFanSys->ZoneHighSETHours(ZoneNum)[2] = state.dataHeatBalSurfMgr->highSETLongestHours[ZoneNum - 1];
                        state.dataHeatBalFanSys->ZoneHighSETHours(ZoneNum)[3] = state.dataHeatBalSurfMgr->highSETLongestStart[ZoneNum - 1];
                    }
                    state.dataHeatBalSurfMgr->lowSETLongestHours[ZoneNum - 1] = 0;
                    state.dataHeatBalSurfMgr->highSETLongestHours[ZoneNum - 1] = 0;
                }
            }
        }
    } // loop over zones
}

void ReportCO2Resilience(EnergyPlusData &state)
{
    int NoBins = 3;
    if (state.dataHeatBalSurfMgr->reportCO2ResilienceFirstTime) {
        for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            state.dataHeatBalFanSys->ZoneCO2LevelHourBins(ZoneNum).assign(NoBins, 0.0);
            state.dataHeatBalFanSys->ZoneCO2LevelOccuHourBins(ZoneNum).assign(NoBins, 0.0);
        }
        state.dataHeatBalSurfMgr->reportCO2ResilienceFirstTime = false;
        if (!state.dataContaminantBalance->Contaminant.CO2Simulation) {
            if (state.dataOutRptTab->displayCO2ResilienceSummaryExplicitly) {
                ShowWarningError(state,
                                 "Writing Annual CO2 Resilience Summary - CO2 Level Hours reports: "
                                 "Zone Air CO2 Concentration output is required, "
                                 "but no ZoneAirContaminantBalance object is defined.");
            }
            state.dataOutRptTab->displayCO2ResilienceSummary = false;
            return;
        }
    }

    if (DataGlobalConstants::KindOfSim::RunPeriodWeather == state.dataGlobal->KindOfSim && !state.dataGlobal->WarmupFlag) {
        for (int iPeople = 1; iPeople <= state.dataHeatBal->TotPeople; ++iPeople) {
            int ZoneNum = state.dataHeatBal->People(iPeople).ZonePtr;
            state.dataHeatBalFanSys->ZoneNumOcc(ZoneNum) = state.dataHeatBal->People(iPeople).NumberOfPeople *
                                                           GetCurrentScheduleValue(state, state.dataHeatBal->People(iPeople).NumberOfPeoplePtr);
        }
        for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            Real64 ZoneAirCO2 = state.dataContaminantBalance->ZoneAirCO2Avg(ZoneNum);

            int NumOcc = state.dataHeatBalFanSys->ZoneNumOcc(ZoneNum);
            if (ZoneAirCO2 <= 1000) {
                state.dataHeatBalFanSys->ZoneCO2LevelHourBins(ZoneNum)[0] += state.dataGlobal->TimeStepZone;
                state.dataHeatBalFanSys->ZoneCO2LevelOccuHourBins(ZoneNum)[0] += NumOcc * state.dataGlobal->TimeStepZone;
            } else if (ZoneAirCO2 > 1000 && ZoneAirCO2 <= 5000) {
                state.dataHeatBalFanSys->ZoneCO2LevelHourBins(ZoneNum)[1] += state.dataGlobal->TimeStepZone;
                state.dataHeatBalFanSys->ZoneCO2LevelOccuHourBins(ZoneNum)[1] += NumOcc * state.dataGlobal->TimeStepZone;
            } else {
                state.dataHeatBalFanSys->ZoneCO2LevelHourBins(ZoneNum)[2] += state.dataGlobal->TimeStepZone;
                state.dataHeatBalFanSys->ZoneCO2LevelOccuHourBins(ZoneNum)[2] += NumOcc * state.dataGlobal->TimeStepZone;
            }
        }
    } // loop over zones
}

void ReportVisualResilience(EnergyPlusData &state)
{
    int NoBins = 4;
    if (state.dataHeatBalSurfMgr->reportVisualResilienceFirstTime) {
        for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            state.dataHeatBalFanSys->ZoneLightingLevelHourBins(ZoneNum).assign(NoBins, 0.0);
            state.dataHeatBalFanSys->ZoneLightingLevelOccuHourBins(ZoneNum).assign(NoBins, 0.0);
        }
        state.dataHeatBalSurfMgr->reportVisualResilienceFirstTime = false;
        bool hasDayLighting = false;
        for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            if (state.dataDaylightingData->ZoneDaylight(ZoneNum).DaylightMethod != DataDaylighting::iDaylightingMethod::NoDaylighting) {
                hasDayLighting = true;
                break;
            }
        }
        if (!hasDayLighting) {
            if (state.dataOutRptTab->displayVisualResilienceSummaryExplicitly) {
                ShowWarningError(state,
                                 "Writing Annual Visual Resilience Summary - Lighting Level Hours reports: "
                                 "Zone Average Daylighting Reference Point Illuminance output is required, "
                                 "but no Daylighting Control Object is defined.");
            }
            state.dataOutRptTab->displayVisualResilienceSummary = false;
            return;
        }
    }

    if (DataGlobalConstants::KindOfSim::RunPeriodWeather == state.dataGlobal->KindOfSim && !state.dataGlobal->WarmupFlag) {
        for (int iPeople = 1; iPeople <= state.dataHeatBal->TotPeople; ++iPeople) {
            int ZoneNum = state.dataHeatBal->People(iPeople).ZonePtr;
            state.dataHeatBalFanSys->ZoneNumOcc(ZoneNum) = state.dataHeatBal->People(iPeople).NumberOfPeople *
                                                           GetCurrentScheduleValue(state, state.dataHeatBal->People(iPeople).NumberOfPeoplePtr);
        }
        for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            // Place holder
            if (state.dataDaylightingData->ZoneDaylight(ZoneNum).DaylightMethod == DataDaylighting::iDaylightingMethod::NoDaylighting) continue;

            Array1D<Real64> ZoneIllumRef = state.dataDaylightingData->ZoneDaylight(ZoneNum).DaylIllumAtRefPt;
            Real64 ZoneIllum = 0.0;
            for (size_t i = 1; i <= ZoneIllumRef.size(); i++) {
                ZoneIllum += ZoneIllumRef(i);
            }
            ZoneIllum /= ZoneIllumRef.size();

            if (state.dataDaylightingData->ZoneDaylight(ZoneNum).ZonePowerReductionFactor > 0) {
                Array1D<Real64> ZoneIllumSetpoint = state.dataDaylightingData->ZoneDaylight(ZoneNum).IllumSetPoint;
                ZoneIllum = 0.0;
                for (size_t i = 1; i <= ZoneIllumSetpoint.size(); i++) {
                    ZoneIllum += ZoneIllumSetpoint(i);
                }
                ZoneIllum /= ZoneIllumSetpoint.size();
            }

            int NumOcc = state.dataHeatBalFanSys->ZoneNumOcc(ZoneNum);
            if (ZoneIllum <= 100) {
                state.dataHeatBalFanSys->ZoneLightingLevelHourBins(ZoneNum)[0] += state.dataGlobal->TimeStepZone;
                state.dataHeatBalFanSys->ZoneLightingLevelOccuHourBins(ZoneNum)[0] += NumOcc * state.dataGlobal->TimeStepZone;
            } else if (ZoneIllum > 100 && ZoneIllum <= 300) {
                state.dataHeatBalFanSys->ZoneLightingLevelHourBins(ZoneNum)[1] += state.dataGlobal->TimeStepZone;
                state.dataHeatBalFanSys->ZoneLightingLevelOccuHourBins(ZoneNum)[1] += NumOcc * state.dataGlobal->TimeStepZone;
            } else if (ZoneIllum > 300 && ZoneIllum <= 500) {
                state.dataHeatBalFanSys->ZoneLightingLevelHourBins(ZoneNum)[2] += state.dataGlobal->TimeStepZone;
                state.dataHeatBalFanSys->ZoneLightingLevelOccuHourBins(ZoneNum)[2] += NumOcc * state.dataGlobal->TimeStepZone;
            } else {
                state.dataHeatBalFanSys->ZoneLightingLevelHourBins(ZoneNum)[3] += state.dataGlobal->TimeStepZone;
                state.dataHeatBalFanSys->ZoneLightingLevelOccuHourBins(ZoneNum)[3] += NumOcc * state.dataGlobal->TimeStepZone;
            }
        }
    } // loop over zones
}
void ReportSurfaceHeatBalance(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   Oct 2000

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine puts the reporting part of the HBSurface Module in one area.

    using SolarShading::ReportSurfaceShading;

    state.dataHeatBalSurf->SumSurfaceHeatEmission = 0.0;

    ReportSurfaceShading(state);
    auto &Surface(state.dataSurface->Surface);

    // update inside face radiation reports
    for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
        Real64 const surfaceArea(Surface(SurfNum).Area);
        // Tuned Replaced by one line form below for speed
        //            QdotRadNetSurfInRep( SurfNum ) = NetLWRadToSurf( SurfNum ) * surfaceArea;
        //            QdotRadNetSurfInRepPerArea( SurfNum ) = NetLWRadToSurf( SurfNum );
        state.dataHeatBalSurf->QdotRadNetSurfInRep(SurfNum) =
            (state.dataHeatBalSurf->QdotRadNetSurfInRepPerArea(SurfNum) = state.dataHeatBalSurf->SurfNetLWRadToSurf(SurfNum)) * surfaceArea;
        state.dataHeatBalSurf->QRadNetSurfInReport(SurfNum) = state.dataHeatBalSurf->QdotRadNetSurfInRep(SurfNum) * state.dataGlobal->TimeStepZoneSec;

        if (Surface(SurfNum).Class != SurfaceClass::Window) { // not a window...
            state.dataHeatBalSurf->QdotRadSolarInRepPerArea(SurfNum) =
                state.dataHeatBalSurf->SurfOpaqQRadSWInAbs(SurfNum) - state.dataHeatBalSurf->SurfOpaqQRadSWLightsInAbs(SurfNum);
            state.dataHeatBalSurf->QdotRadSolarInRep(SurfNum) = state.dataHeatBalSurf->QdotRadSolarInRepPerArea(SurfNum) * surfaceArea;
            state.dataHeatBalSurf->QRadSolarInReport(SurfNum) = state.dataHeatBalSurf->QdotRadSolarInRep(SurfNum) * state.dataGlobal->TimeStepZoneSec;

            state.dataHeatBalSurf->QdotRadLightsInRepPerArea(SurfNum) = state.dataHeatBalSurf->SurfOpaqQRadSWLightsInAbs(SurfNum);
            state.dataHeatBalSurf->QdotRadLightsInRep(SurfNum) = state.dataHeatBalSurf->QdotRadLightsInRepPerArea(SurfNum) * surfaceArea;
            state.dataHeatBalSurf->QRadLightsInReport(SurfNum) =
                state.dataHeatBalSurf->QdotRadLightsInRep(SurfNum) * state.dataGlobal->TimeStepZoneSec;

            if (state.dataGlobal->ZoneSizingCalc && state.dataGlobal->CompLoadReportIsReq) {
                int TimeStepInDay = (state.dataGlobal->HourOfDay - 1) * state.dataGlobal->NumOfTimeStepInHour + state.dataGlobal->TimeStep;
                state.dataOutRptTab->lightSWRadSeq(state.dataSize->CurOverallSimDay, TimeStepInDay, SurfNum) =
                    state.dataHeatBalSurf->QdotRadLightsInRep(SurfNum);
                state.dataOutRptTab->feneSolarRadSeq(state.dataSize->CurOverallSimDay, TimeStepInDay, SurfNum) =
                    state.dataHeatBalSurf->QdotRadSolarInRep(SurfNum);
            }
        } else { // can we fill these for windows?
        }

        // Tuned Replaced by one line form below for speed
        //            QdotRadIntGainsInRepPerArea( SurfNum ) = QRadThermInAbs( SurfNum );
        //            QdotRadIntGainsInRep( SurfNum ) = QdotRadIntGainsInRepPerArea( SurfNum ) * surfaceArea;
        state.dataHeatBalSurf->QdotRadIntGainsInRep(SurfNum) =
            (state.dataHeatBalSurf->QdotRadIntGainsInRepPerArea(SurfNum) = state.dataHeatBal->SurfQRadThermInAbs(SurfNum)) * surfaceArea;
        state.dataHeatBalSurf->QRadIntGainsInReport(SurfNum) =
            state.dataHeatBalSurf->QdotRadIntGainsInRep(SurfNum) * state.dataGlobal->TimeStepZoneSec;

        state.dataHeatBalSurf->QdotRadHVACInRepPerArea(SurfNum) =
            state.dataHeatBalFanSys->QHTRadSysSurf(SurfNum) + state.dataHeatBalFanSys->QCoolingPanelSurf(SurfNum) +
            state.dataHeatBalFanSys->QHWBaseboardSurf(SurfNum) + state.dataHeatBalFanSys->QSteamBaseboardSurf(SurfNum) +
            state.dataHeatBalFanSys->QElecBaseboardSurf(SurfNum);
        state.dataHeatBalSurf->QdotRadHVACInRep(SurfNum) = state.dataHeatBalSurf->QdotRadHVACInRepPerArea(SurfNum) * Surface(SurfNum).Area;
        // Tuned Replaced by one line form below for speed
        //            QdotRadHVACInRepPerArea( SurfNum ) = QHTRadSysSurf( SurfNum ) + QHWBaseboardSurf( SurfNum ) + QSteamBaseboardSurf(
        // SurfNum
        //)
        //+  QElecBaseboardSurf( SurfNum );             QdotRadHVACInRep( SurfNum ) = QdotRadHVACInRepPerArea( SurfNum ) *
        // surfaceArea;
        state.dataHeatBalSurf->QdotRadHVACInRep(SurfNum) =
            (state.dataHeatBalSurf->QdotRadHVACInRepPerArea(SurfNum) =
                 state.dataHeatBalFanSys->QHTRadSysSurf(SurfNum) + state.dataHeatBalFanSys->QCoolingPanelSurf(SurfNum) +
                 state.dataHeatBalFanSys->QHWBaseboardSurf(SurfNum) + state.dataHeatBalFanSys->QSteamBaseboardSurf(SurfNum) +
                 state.dataHeatBalFanSys->QElecBaseboardSurf(SurfNum)) *
            surfaceArea;
        state.dataHeatBalSurf->QRadHVACInReport(SurfNum) = state.dataHeatBalSurf->QdotRadHVACInRep(SurfNum) * state.dataGlobal->TimeStepZoneSec;
        if (Surface(SurfNum).ExtBoundCond == ExternalEnvironment) {
            state.dataHeatBalSurf->SumSurfaceHeatEmission += state.dataHeatBalSurf->QHeatEmiReport(SurfNum) * state.dataGlobal->TimeStepZoneSec;
        }
    }
    for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
        int const firstSurfOpaq = state.dataHeatBal->Zone(ZoneNum).OpaqOrIntMassSurfaceFirst;
        int const lastSurfOpaq = state.dataHeatBal->Zone(ZoneNum).OpaqOrIntMassSurfaceLast;
        for (int SurfNum = firstSurfOpaq; SurfNum <= lastSurfOpaq; ++SurfNum) {
            // inside face conduction updates
            state.dataHeatBalSurf->SurfOpaqInsFaceConductionEnergy(SurfNum) =
                state.dataHeatBalSurf->SurfOpaqInsFaceConduction(SurfNum) * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneOpaqSurfInsFaceCond(ZoneNum) += state.dataHeatBalSurf->SurfOpaqInsFaceConduction(SurfNum);
            state.dataHeatBalSurf->SurfOpaqInsFaceCondGainRep(SurfNum) = 0.0;
            state.dataHeatBalSurf->SurfOpaqInsFaceCondLossRep(SurfNum) = 0.0;
            if (state.dataHeatBalSurf->SurfOpaqInsFaceConduction(SurfNum) >= 0.0) {
                state.dataHeatBalSurf->SurfOpaqInsFaceCondGainRep(SurfNum) = state.dataHeatBalSurf->SurfOpaqInsFaceConduction(SurfNum);
            } else {
                state.dataHeatBalSurf->SurfOpaqInsFaceCondLossRep(SurfNum) = -state.dataHeatBalSurf->SurfOpaqInsFaceConduction(SurfNum);
            }

            // outside face conduction updates
            state.dataHeatBalSurf->SurfOpaqOutsideFaceConductionEnergy(SurfNum) =
                state.dataHeatBalSurf->SurfOpaqOutsideFaceConduction(SurfNum) * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneOpaqSurfExtFaceCond(ZoneNum) += state.dataHeatBalSurf->SurfOpaqOutsideFaceConduction(SurfNum);
            state.dataHeatBalSurf->SurfOpaqExtFaceCondGainRep(SurfNum) = 0.0;
            state.dataHeatBalSurf->SurfOpaqExtFaceCondLossRep(SurfNum) = 0.0;
            if (state.dataHeatBalSurf->SurfOpaqOutsideFaceConduction(SurfNum) >= 0.0) {
                state.dataHeatBalSurf->SurfOpaqExtFaceCondGainRep(SurfNum) = state.dataHeatBalSurf->SurfOpaqOutsideFaceConduction(SurfNum);
            } else {
                state.dataHeatBalSurf->SurfOpaqExtFaceCondLossRep(SurfNum) = -state.dataHeatBalSurf->SurfOpaqOutsideFaceConduction(SurfNum);
            }

            // do average surface conduction updates

            state.dataHeatBalSurf->SurfOpaqAvgFaceConduction(SurfNum) =
                (state.dataHeatBalSurf->SurfOpaqInsFaceConduction(SurfNum) - state.dataHeatBalSurf->SurfOpaqOutsideFaceConduction(SurfNum)) / 2.0;
            state.dataHeatBalSurf->SurfOpaqAvgFaceConductionFlux(SurfNum) =
                (state.dataHeatBalSurf->SurfOpaqInsFaceConductionFlux(SurfNum) - state.dataHeatBalSurf->SurfOpaqOutsideFaceConductionFlux(SurfNum)) /
                2.0;
            state.dataHeatBalSurf->SurfOpaqAvgFaceConductionEnergy(SurfNum) =
                state.dataHeatBalSurf->SurfOpaqAvgFaceConduction(SurfNum) * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBalSurf->SurfOpaqAvgFaceCondGainRep(SurfNum) = 0.0;
            state.dataHeatBalSurf->SurfOpaqAvgFaceCondLossRep(SurfNum) = 0.0;
            if (state.dataHeatBalSurf->SurfOpaqAvgFaceConduction(SurfNum) >= 0.0) {
                state.dataHeatBalSurf->SurfOpaqAvgFaceCondGainRep(SurfNum) = state.dataHeatBalSurf->SurfOpaqAvgFaceConduction(SurfNum);
            } else {
                state.dataHeatBalSurf->SurfOpaqAvgFaceCondLossRep(SurfNum) = -state.dataHeatBalSurf->SurfOpaqAvgFaceConduction(SurfNum);
            }

            // do surface storage rate updates
            state.dataHeatBalSurf->SurfOpaqStorageConductionFlux(SurfNum) =
                -(state.dataHeatBalSurf->SurfOpaqInsFaceConductionFlux(SurfNum) + state.dataHeatBalSurf->SurfOpaqOutsideFaceConductionFlux(SurfNum));
            state.dataHeatBalSurf->SurfOpaqStorageConduction(SurfNum) =
                -(state.dataHeatBalSurf->SurfOpaqInsFaceConduction(SurfNum) + state.dataHeatBalSurf->SurfOpaqOutsideFaceConduction(SurfNum));
            state.dataHeatBalSurf->SurfOpaqStorageConductionEnergy(SurfNum) =
                state.dataHeatBalSurf->SurfOpaqStorageConduction(SurfNum) * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBalSurf->SurfOpaqStorageGainRep(SurfNum) = 0.0;
            state.dataHeatBalSurf->SurfOpaqStorageCondLossRep(SurfNum) = 0.0;
            if (state.dataHeatBalSurf->SurfOpaqStorageConduction(SurfNum) >= 0.0) {
                state.dataHeatBalSurf->SurfOpaqStorageGainRep(SurfNum) = state.dataHeatBalSurf->SurfOpaqStorageConduction(SurfNum);
            } else {
                state.dataHeatBalSurf->SurfOpaqStorageCondLossRep(SurfNum) = -state.dataHeatBalSurf->SurfOpaqStorageConduction(SurfNum);
            }
        } // opaque heat transfer surfaces.

        if (state.dataHeatBal->ZoneOpaqSurfInsFaceCond(ZoneNum) >= 0.0) {
            state.dataHeatBal->ZoneOpaqSurfInsFaceCondGainRep(ZoneNum) = state.dataHeatBal->ZoneOpaqSurfInsFaceCond(ZoneNum);
            state.dataHeatBal->ZnOpqSurfInsFaceCondGnRepEnrg(ZoneNum) =
                state.dataHeatBal->ZoneOpaqSurfInsFaceCondGainRep(ZoneNum) * state.dataGlobal->TimeStepZoneSec;
        } else {
            state.dataHeatBal->ZoneOpaqSurfInsFaceCondLossRep(ZoneNum) = -state.dataHeatBal->ZoneOpaqSurfInsFaceCond(ZoneNum);
            state.dataHeatBal->ZnOpqSurfInsFaceCondLsRepEnrg(ZoneNum) =
                state.dataHeatBal->ZoneOpaqSurfInsFaceCondLossRep(ZoneNum) * state.dataGlobal->TimeStepZoneSec;
        }

        if (state.dataHeatBal->ZoneOpaqSurfExtFaceCond(ZoneNum) >= 0.0) {
            state.dataHeatBal->ZoneOpaqSurfExtFaceCondGainRep(ZoneNum) = state.dataHeatBal->ZoneOpaqSurfExtFaceCond(ZoneNum);
            state.dataHeatBal->ZnOpqSurfExtFaceCondGnRepEnrg(ZoneNum) =
                state.dataHeatBal->ZoneOpaqSurfExtFaceCondGainRep(ZoneNum) * state.dataGlobal->TimeStepZoneSec;
        } else {
            state.dataHeatBal->ZoneOpaqSurfExtFaceCondLossRep(ZoneNum) = -state.dataHeatBal->ZoneOpaqSurfExtFaceCond(ZoneNum);
            state.dataHeatBal->ZnOpqSurfExtFaceCondLsRepEnrg(ZoneNum) =
                state.dataHeatBal->ZoneOpaqSurfExtFaceCondLossRep(ZoneNum) * state.dataGlobal->TimeStepZoneSec;
        }
    } // loop over zones
}

void ReportIntMovInsInsideSurfTemp(EnergyPlusData &state)
{
    state.dataHeatBalSurf->TempSurfInMovInsRep = state.dataHeatBalSurf->TempSurfIn;
    for (int SurfNum : state.dataHeatBalSurf->SurfMovInsulIndexList) {
        if (state.dataHeatBalSurf->SurfMovInsulIntPresent(SurfNum)) {
            state.dataHeatBalSurf->TempSurfInMovInsRep(SurfNum) = state.dataHeatBalSurf->TempSurfInTmp(SurfNum);
        }
    }
}
// End of Reporting subroutines for the HB Module
// *****************************************************************************

// *****************************************************************************
// *****************************************************************************
// *****************************************************************************
// *****************************************************************************

// Formerly EXTERNAL SUBROUTINES (heavily related to HeatBalanceSurfaceManager) now moved into namespace

void CalcHeatBalanceOutsideSurf(EnergyPlusData &state,
                                Optional_int_const ZoneToResimulate) // if passed in, then only calculate surfaces that have this zone
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         George Walton
    //       DATE WRITTEN   December 1979
    //       MODIFIED       Jun 1990 (RDT for new CTF arrays);
    //                      Aug 2000 (RJL for MTF moisture calculations)
    //                      Sep 2000 (RKS for new radiant exchange algorithm)
    //                      Dec 2000 (RKS for radiant system model addition)
    //                      Apr 2002 (COP removed denominator from OSC calculation
    //                      Jul 2008 (P.Biddulph include calls to HAMT)
    //                      Jul 2011, M.J. Witte and C.O. Pedersen, add new fields to OSC for last T, max and min
    //                      Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems
    //       RE-ENGINEERED  Mar 1998 (RKS)

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine performs a heat balance on the outside face of each
    // surface in the building.

    // METHODOLOGY EMPLOYED:
    // Various boundary conditions are set and additional parameters are set-
    // up.  Then, the proper heat balance equation is selected based on the
    // presence of movable insulation, thermal mass of the surface construction,
    // and convection model being used.

    // REFERENCES:
    // (I)BLAST legacy routine HBOUT
    // 1989 ASHRAE Handbook of Fundamentals (Figure 1 on p. 22.4, convection correlations)

    // Using/Aliasing
    using namespace DataEnvironment;
    using namespace DataHeatBalFanSys;
    using namespace DataHeatBalance;
    using namespace DataHeatBalSurface;
    using namespace DataSurfaces;
    using ConvectionCoefficients::InitExteriorConvectionCoeff;
    using ConvectionCoefficients::SetExtConvectionCoeff;
    using ConvectionCoefficients::SetIntConvectionCoeff;
    using HeatBalanceIntRadExchange::CalcInteriorRadExchange;
    using ScheduleManager::GetCurrentScheduleValue;
    using ScheduleManager::GetScheduleIndex;
    using namespace Psychrometrics;
    using EcoRoofManager::CalcEcoRoof;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:
    constexpr const char *RoutineNameGroundTemp("CalcHeatBalanceOutsideSurf:GroundTemp");
    constexpr const char *RoutineNameGroundTempFC("CalcHeatBalanceOutsideSurf:GroundTempFC");
    constexpr const char *RoutineNameOtherSideCoefNoCalcExt("CalcHeatBalanceOutsideSurf:OtherSideCoefNoCalcExt");
    constexpr const char *RoutineNameOtherSideCoefCalcExt("CalcHeatBalanceOutsideSurf:OtherSideCoefCalcExt");
    constexpr const char *RoutineNameOSCM("CalcHeatBalanceOutsideSurf:OSCM");
    constexpr const char *RoutineNameExtEnvWetSurf("CalcHeatBalanceOutsideSurf:extEnvWetSurf");
    constexpr const char *RoutineNameExtEnvDrySurf("CalcHeatBalanceOutsideSurf:extEnvDrySurf");
    constexpr const char *RoutineNameNoWind("CalcHeatBalanceOutsideSurf:nowind");
    constexpr const char *RoutineNameOther("CalcHeatBalanceOutsideSurf:interior/other");
    constexpr const char *RoutineNameIZPart("CalcHeatBalanceOutsideSurf:IZPart");
    constexpr const char *HBSurfManGroundHAMT("HBSurfMan:Ground:HAMT");
    constexpr const char *HBSurfManRainHAMT("HBSurfMan:Rain:HAMT");
    constexpr const char *HBSurfManDrySurfCondFD("HBSurfMan:DrySurf:CondFD");
    constexpr const char *Outside("Outside");
    static std::string const BlankString;

    bool MovInsulErrorFlag = false; // Movable Insulation error flag

    auto &Surface(state.dataSurface->Surface);

    if (state.dataHeatBal->AnyInternalHeatSourceInInput) {
        for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
            // Need to transfer any source/sink for a surface to the local array.  Note that
            // the local array is flux (W/m2) while the QRadSysSource is heat transfer (W).
            // This must be done at this location so that this is always updated correctly.
            if (Surface(SurfNum).Area > 0.0)
                state.dataHeatBalSurf->QsrcHist(SurfNum, 1) =
                    state.dataHeatBalFanSys->QRadSysSource(SurfNum) / Surface(SurfNum).Area; // Make sure we don't divide by zero...

            // next we add source (actually a sink) from any integrated PV
            if (Surface(SurfNum).Area > 0.0)
                state.dataHeatBalSurf->QsrcHist(SurfNum, 1) +=
                    state.dataHeatBalFanSys->QPVSysSource(SurfNum) / Surface(SurfNum).Area; // Make sure we don't divide by zero...
        }
    }

    if (present(ZoneToResimulate)) {
        CalcInteriorRadExchange(state, state.dataHeatBalSurf->TH(2, 1, _), 0, state.dataHeatBalSurf->SurfNetLWRadToSurf, ZoneToResimulate, Outside);
    } else {
        CalcInteriorRadExchange(state, state.dataHeatBalSurf->TH(2, 1, _), 0, state.dataHeatBalSurf->SurfNetLWRadToSurf, _, Outside);
    }

    for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) { // Loop through all surfaces...
        for (int SurfNum = state.dataHeatBal->Zone(zoneNum).HTSurfaceFirst; SurfNum <= state.dataHeatBal->Zone(zoneNum).HTSurfaceLast; ++SurfNum) {
            if (Surface(SurfNum).Class == SurfaceClass::Window) continue;
            if (present(ZoneToResimulate)) {
                if ((zoneNum != ZoneToResimulate) && (state.dataSurface->AdjacentZoneToSurface(SurfNum) != ZoneToResimulate)) {
                    continue; // skip surfaces that are not associated with this zone
                }
            }
            // Interior windows in partitions use "normal" heat balance calculations
            // For rest, Outside surface temp of windows not needed in Window5 calculation approach.
            // Window layer temperatures are calculated in CalcHeatBalanceInsideSurf

            // Initializations for this surface
            int const ConstrNum = state.dataSurface->SurfActiveConstruction(SurfNum);
            Real64 HMovInsul = 0.0; // "Convection" coefficient of movable insulation
            Real64 HSky = 0.0;      // "Convection" coefficient from sky to surface
            Real64 HGround = 0.0;   // "Convection" coefficient from ground to surface
            Real64 HAir = 0.0;      // "Convection" coefficient from air to surface (radiation)
            state.dataHeatBalSurf->HcExtSurf(SurfNum) = 0.0;
            state.dataHeatBalSurf->HAirExtSurf(SurfNum) = 0.0;
            state.dataHeatBalSurf->HSkyExtSurf(SurfNum) = 0.0;
            state.dataHeatBalSurf->HGrdExtSurf(SurfNum) = 0.0;
            state.dataHeatBalSurf->SurfQRadLWOutSrdSurfs(SurfNum) = 0.0;

            // Calculate heat extract due to additional heat flux source term as the surface boundary condition

            if (Surface(SurfNum).OutsideHeatSourceTermSchedule) {
                state.dataHeatBalSurf->SurfQAdditionalHeatSourceOutside(SurfNum) =
                    EnergyPlus::ScheduleManager::GetCurrentScheduleValue(state, Surface(SurfNum).OutsideHeatSourceTermSchedule);
            }

            // Calculate the current outside surface temperature TH(SurfNum,1,1) for the
            // various different boundary conditions
            {
                auto const SELECT_CASE_var(Surface(SurfNum).ExtBoundCond);

                if (SELECT_CASE_var == Ground) { // Surface in contact with ground

                    state.dataHeatBalSurf->TH(1, 1, SurfNum) = state.dataEnvrn->GroundTemp;

                    // Set the only radiant system heat balance coefficient that is non-zero for this case
                    if (state.dataConstruction->Construct(ConstrNum).SourceSinkPresent)
                        state.dataHeatBalFanSys->RadSysToHBConstCoef(SurfNum) = state.dataHeatBalSurf->TH(1, 1, SurfNum);

                    // start HAMT
                    if (Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::HAMT) {
                        // Set variables used in the HAMT moisture balance
                        state.dataMstBal->TempOutsideAirFD(SurfNum) = state.dataEnvrn->GroundTemp;
                        state.dataMstBal->RhoVaporAirOut(SurfNum) = PsyRhovFnTdbRh(state, state.dataEnvrn->GroundTemp, 1.0, HBSurfManGroundHAMT);
                        state.dataMstBal->HConvExtFD(SurfNum) = state.dataHeatBal->HighHConvLimit;

                        state.dataMstBal->HMassConvExtFD(SurfNum) =
                            state.dataMstBal->HConvExtFD(SurfNum) /
                            ((PsyRhoAirFnPbTdbW(
                                  state,
                                  state.dataEnvrn->OutBaroPress,
                                  state.dataEnvrn->GroundTemp,
                                  PsyWFnTdbRhPb(state, state.dataEnvrn->GroundTemp, 1.0, state.dataEnvrn->OutBaroPress, RoutineNameGroundTemp)) +
                              state.dataMstBal->RhoVaporAirOut(SurfNum)) *
                             PsyCpAirFnW(state.dataEnvrn->OutHumRat));

                        state.dataMstBal->HSkyFD(SurfNum) = HSky;
                        state.dataMstBal->HGrndFD(SurfNum) = HGround;
                        state.dataMstBal->HAirFD(SurfNum) = HAir;
                    }
                    // end HAMT

                    if (Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::CondFD) {
                        // Set variables used in the FD moisture balance
                        state.dataMstBal->TempOutsideAirFD(SurfNum) = state.dataEnvrn->GroundTemp;
                        state.dataMstBal->RhoVaporAirOut(SurfNum) = PsyRhovFnTdbRhLBnd0C(state.dataEnvrn->GroundTemp, 1.0);
                        state.dataMstBal->HConvExtFD(SurfNum) = state.dataHeatBal->HighHConvLimit;
                        state.dataMstBal->HMassConvExtFD(SurfNum) =
                            state.dataMstBal->HConvExtFD(SurfNum) /
                            ((PsyRhoAirFnPbTdbW(
                                  state,
                                  state.dataEnvrn->OutBaroPress,
                                  state.dataEnvrn->GroundTemp,
                                  PsyWFnTdbRhPb(state, state.dataEnvrn->GroundTemp, 1.0, state.dataEnvrn->OutBaroPress, RoutineNameGroundTemp)) +
                              state.dataMstBal->RhoVaporAirOut(SurfNum)) *
                             PsyCpAirFnW(state.dataEnvrn->OutHumRat));
                        state.dataMstBal->HSkyFD(SurfNum) = HSky;
                        state.dataMstBal->HGrndFD(SurfNum) = HGround;
                        state.dataMstBal->HAirFD(SurfNum) = HAir;
                    }

                    // Added for FCfactor grounds
                } else if (SELECT_CASE_var == GroundFCfactorMethod) { // Surface in contact with ground

                    state.dataHeatBalSurf->TH(1, 1, SurfNum) = state.dataEnvrn->GroundTempFC;

                    // Set the only radiant system heat balance coefficient that is non-zero for this case
                    if (state.dataConstruction->Construct(ConstrNum).SourceSinkPresent)
                        state.dataHeatBalFanSys->RadSysToHBConstCoef(SurfNum) = state.dataHeatBalSurf->TH(1, 1, SurfNum);

                    if (Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::HAMT) {
                        // Set variables used in the HAMT moisture balance
                        state.dataMstBal->TempOutsideAirFD(SurfNum) = state.dataEnvrn->GroundTempFC;
                        state.dataMstBal->RhoVaporAirOut(SurfNum) = PsyRhovFnTdbRh(state, state.dataEnvrn->GroundTempFC, 1.0, HBSurfManGroundHAMT);
                        state.dataMstBal->HConvExtFD(SurfNum) = state.dataHeatBal->HighHConvLimit;

                        state.dataMstBal->HMassConvExtFD(SurfNum) =
                            state.dataMstBal->HConvExtFD(SurfNum) /
                            ((PsyRhoAirFnPbTdbW(
                                  state,
                                  state.dataEnvrn->OutBaroPress,
                                  state.dataEnvrn->GroundTempFC,
                                  PsyWFnTdbRhPb(state, state.dataEnvrn->GroundTempFC, 1.0, state.dataEnvrn->OutBaroPress, RoutineNameGroundTempFC)) +
                              state.dataMstBal->RhoVaporAirOut(SurfNum)) *
                             PsyCpAirFnW(state.dataEnvrn->OutHumRat));

                        state.dataMstBal->HSkyFD(SurfNum) = HSky;
                        state.dataMstBal->HGrndFD(SurfNum) = HGround;
                        state.dataMstBal->HAirFD(SurfNum) = HAir;
                    }

                    if (Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::CondFD) {
                        // Set variables used in the FD moisture balance
                        state.dataMstBal->TempOutsideAirFD(SurfNum) = state.dataEnvrn->GroundTempFC;
                        state.dataMstBal->RhoVaporAirOut(SurfNum) = PsyRhovFnTdbRhLBnd0C(state.dataEnvrn->GroundTempFC, 1.0);
                        state.dataMstBal->HConvExtFD(SurfNum) = state.dataHeatBal->HighHConvLimit;
                        state.dataMstBal->HMassConvExtFD(SurfNum) =
                            state.dataMstBal->HConvExtFD(SurfNum) /
                            ((PsyRhoAirFnPbTdbW(
                                  state,
                                  state.dataEnvrn->OutBaroPress,
                                  state.dataEnvrn->GroundTempFC,
                                  PsyWFnTdbRhPb(state, state.dataEnvrn->GroundTempFC, 1.0, state.dataEnvrn->OutBaroPress, RoutineNameGroundTempFC)) +
                              state.dataMstBal->RhoVaporAirOut(SurfNum)) *
                             PsyCpAirFnW(state.dataEnvrn->OutHumRat));
                        state.dataMstBal->HSkyFD(SurfNum) = HSky;
                        state.dataMstBal->HGrndFD(SurfNum) = HGround;
                        state.dataMstBal->HAirFD(SurfNum) = HAir;
                    }

                } else if (SELECT_CASE_var == OtherSideCoefNoCalcExt) {
                    // Use Other Side Coefficients to determine the surface film coefficient and
                    // the exterior boundary condition temperature

                    int OPtr = Surface(SurfNum).OSCPtr;
                    // Set surface temp from previous timestep
                    if (state.dataGlobal->BeginTimeStepFlag) {
                        state.dataSurface->OSC(OPtr).TOutsideSurfPast = state.dataHeatBalSurf->TH(1, 1, SurfNum);
                    }

                    if (state.dataSurface->OSC(OPtr).ConstTempScheduleIndex != 0) { // Determine outside temperature from schedule
                        state.dataSurface->OSC(OPtr).ConstTemp = GetCurrentScheduleValue(state, state.dataSurface->OSC(OPtr).ConstTempScheduleIndex);
                    }

                    //  Allow for modification of TemperatureCoefficient with unitary sine wave.
                    Real64 ConstantTempCoef; // Temperature Coefficient as input or modified using sine wave COP mod
                    if (state.dataSurface->OSC(OPtr).SinusoidalConstTempCoef) { // Sine wave C4
                        ConstantTempCoef =
                            std::sin(2 * DataGlobalConstants::Pi * state.dataGlobal->CurrentTime / state.dataSurface->OSC(OPtr).SinusoidPeriod);
                    } else {
                        ConstantTempCoef = state.dataSurface->OSC(OPtr).ConstTempCoef;
                    }

                    state.dataSurface->OSC(OPtr).OSCTempCalc =
                        (state.dataSurface->OSC(OPtr).ZoneAirTempCoef * state.dataHeatBalFanSys->MAT(zoneNum) +
                         state.dataSurface->OSC(OPtr).ExtDryBulbCoef * state.dataSurface->SurfOutDryBulbTemp(SurfNum) +
                         ConstantTempCoef * state.dataSurface->OSC(OPtr).ConstTemp +
                         state.dataSurface->OSC(OPtr).GroundTempCoef * state.dataEnvrn->GroundTemp +
                         state.dataSurface->OSC(OPtr).WindSpeedCoef * state.dataSurface->SurfOutWindSpeed(SurfNum) *
                             state.dataSurface->SurfOutDryBulbTemp(SurfNum) +
                         state.dataSurface->OSC(OPtr).TPreviousCoef * state.dataSurface->OSC(OPtr).TOutsideSurfPast);

                    // Enforce max/min limits if applicable
                    if (state.dataSurface->OSC(OPtr).MinLimitPresent)
                        state.dataSurface->OSC(OPtr).OSCTempCalc =
                            max(state.dataSurface->OSC(OPtr).MinTempLimit, state.dataSurface->OSC(OPtr).OSCTempCalc);
                    if (state.dataSurface->OSC(OPtr).MaxLimitPresent)
                        state.dataSurface->OSC(OPtr).OSCTempCalc =
                            min(state.dataSurface->OSC(OPtr).MaxTempLimit, state.dataSurface->OSC(OPtr).OSCTempCalc);

                    state.dataHeatBalSurf->TH(1, 1, SurfNum) = state.dataSurface->OSC(OPtr).OSCTempCalc;

                    // Set the only radiant system heat balance coefficient that is non-zero for this case
                    if (state.dataConstruction->Construct(ConstrNum).SourceSinkPresent)
                        state.dataHeatBalFanSys->RadSysToHBConstCoef(SurfNum) = state.dataHeatBalSurf->TH(1, 1, SurfNum);

                    if (Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::CondFD ||
                        Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::HAMT) {
                        // Set variables used in the FD moisture balance and HAMT
                        state.dataMstBal->TempOutsideAirFD(SurfNum) = state.dataHeatBalSurf->TH(1, 1, SurfNum);
                        state.dataMstBal->RhoVaporAirOut(SurfNum) =
                            PsyRhovFnTdbWPb(state.dataMstBal->TempOutsideAirFD(SurfNum), state.dataEnvrn->OutHumRat, state.dataEnvrn->OutBaroPress);
                        state.dataMstBal->HConvExtFD(SurfNum) = state.dataHeatBal->HighHConvLimit;
                        state.dataMstBal->HMassConvExtFD(SurfNum) =
                            state.dataMstBal->HConvExtFD(SurfNum) / ((PsyRhoAirFnPbTdbW(state,
                                                                                        state.dataEnvrn->OutBaroPress,
                                                                                        state.dataMstBal->TempOutsideAirFD(SurfNum),
                                                                                        PsyWFnTdbRhPb(state,
                                                                                                      state.dataMstBal->TempOutsideAirFD(SurfNum),
                                                                                                      1.0,
                                                                                                      state.dataEnvrn->OutBaroPress,
                                                                                                      RoutineNameOtherSideCoefNoCalcExt)) +
                                                                      state.dataMstBal->RhoVaporAirOut(SurfNum)) *
                                                                     PsyCpAirFnW(state.dataEnvrn->OutHumRat));
                        state.dataMstBal->HSkyFD(SurfNum) = HSky;
                        state.dataMstBal->HGrndFD(SurfNum) = HGround;
                        state.dataMstBal->HAirFD(SurfNum) = HAir;
                    }

                    // This ends the calculations for this surface and goes on to the next SurfNum

                } else if (SELECT_CASE_var == OtherSideCoefCalcExt) { // A surface with other side coefficients that define the outside environment

                    // First, set up the outside convection coefficient and the exterior temperature
                    // boundary condition for the surface
                    int OPtr = Surface(SurfNum).OSCPtr;
                    // Set surface temp from previous timestep
                    if (state.dataGlobal->BeginTimeStepFlag) {
                        state.dataSurface->OSC(OPtr).TOutsideSurfPast = state.dataHeatBalSurf->TH(1, 1, SurfNum);
                    }

                    if (state.dataSurface->OSC(OPtr).ConstTempScheduleIndex != 0) { // Determine outside temperature from schedule
                        state.dataSurface->OSC(OPtr).ConstTemp = GetCurrentScheduleValue(state, state.dataSurface->OSC(OPtr).ConstTempScheduleIndex);
                    }

                    state.dataHeatBalSurf->HcExtSurf(SurfNum) = state.dataSurface->OSC(OPtr).SurfFilmCoef;

                    state.dataSurface->OSC(OPtr).OSCTempCalc =
                        (state.dataSurface->OSC(OPtr).ZoneAirTempCoef * state.dataHeatBalFanSys->MAT(zoneNum) +
                         state.dataSurface->OSC(OPtr).ExtDryBulbCoef * state.dataSurface->SurfOutDryBulbTemp(SurfNum) +
                         state.dataSurface->OSC(OPtr).ConstTempCoef * state.dataSurface->OSC(OPtr).ConstTemp +
                         state.dataSurface->OSC(OPtr).GroundTempCoef * state.dataEnvrn->GroundTemp +
                         state.dataSurface->OSC(OPtr).WindSpeedCoef * state.dataSurface->SurfOutWindSpeed(SurfNum) *
                             state.dataSurface->SurfOutDryBulbTemp(SurfNum) +
                         state.dataSurface->OSC(OPtr).TPreviousCoef * state.dataSurface->OSC(OPtr).TOutsideSurfPast);

                    // Enforce max/min limits if applicable
                    if (state.dataSurface->OSC(OPtr).MinLimitPresent)
                        state.dataSurface->OSC(OPtr).OSCTempCalc =
                            max(state.dataSurface->OSC(OPtr).MinTempLimit, state.dataSurface->OSC(OPtr).OSCTempCalc);
                    if (state.dataSurface->OSC(OPtr).MaxLimitPresent)
                        state.dataSurface->OSC(OPtr).OSCTempCalc =
                            min(state.dataSurface->OSC(OPtr).MaxTempLimit, state.dataSurface->OSC(OPtr).OSCTempCalc);

                    Real64 TempExt = state.dataSurface->OSC(OPtr).OSCTempCalc;

                    // Set the only radiant system heat balance coefficient that is non-zero for this case
                    if (state.dataConstruction->Construct(ConstrNum).SourceSinkPresent)
                        state.dataHeatBalFanSys->RadSysToHBConstCoef(SurfNum) = state.dataHeatBalSurf->TH(1, 1, SurfNum);

                    if (Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::CondFD ||
                        Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::HAMT) {
                        // Set variables used in the FD moisture balance and HAMT
                        state.dataMstBal->TempOutsideAirFD(SurfNum) = TempExt;
                        state.dataMstBal->RhoVaporAirOut(SurfNum) =
                            PsyRhovFnTdbWPb(state.dataMstBal->TempOutsideAirFD(SurfNum), state.dataEnvrn->OutHumRat, state.dataEnvrn->OutBaroPress);
                        state.dataMstBal->HConvExtFD(SurfNum) = state.dataHeatBalSurf->HcExtSurf(SurfNum);
                        state.dataMstBal->HMassConvExtFD(SurfNum) =
                            state.dataMstBal->HConvExtFD(SurfNum) / ((PsyRhoAirFnPbTdbW(state,
                                                                                        state.dataEnvrn->OutBaroPress,
                                                                                        state.dataMstBal->TempOutsideAirFD(SurfNum),
                                                                                        PsyWFnTdbRhPb(state,
                                                                                                      state.dataMstBal->TempOutsideAirFD(SurfNum),
                                                                                                      1.0,
                                                                                                      state.dataEnvrn->OutBaroPress,
                                                                                                      RoutineNameOtherSideCoefCalcExt)) +
                                                                      state.dataMstBal->RhoVaporAirOut(SurfNum)) *
                                                                     PsyCpAirFnW(state.dataEnvrn->OutHumRat));
                        state.dataMstBal->HSkyFD(SurfNum) = state.dataHeatBalSurf->HSkyExtSurf(SurfNum);
                        state.dataMstBal->HGrndFD(SurfNum) = state.dataHeatBalSurf->HGrdExtSurf(SurfNum);
                        state.dataMstBal->HAirFD(SurfNum) = state.dataHeatBalSurf->HAirExtSurf(SurfNum);
                    }

                    // Call the outside surface temp calculation and pass the necessary terms
                    if (Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::CTF ||
                        Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::EMPD) {
                        CalcOutsideSurfTemp(state, SurfNum, zoneNum, ConstrNum, HMovInsul, TempExt, MovInsulErrorFlag);
                        if (MovInsulErrorFlag) ShowFatalError(state, "CalcOutsideSurfTemp: Program terminates due to preceding conditions.");
                    }

                    // This ends the calculations for this surface and goes on to the next SurfNum

                } else if (SELECT_CASE_var ==
                           OtherSideCondModeledExt) { // A surface with other side conditions determined from seperate, dynamic component
                    // modeling that defines the "outside environment"

                    // First, set up the outside convection coefficient and the exterior temperature
                    // boundary condition for the surface
                    int OPtr = Surface(SurfNum).OSCMPtr;
                    // EMS overrides
                    if (state.dataSurface->OSCM(OPtr).EMSOverrideOnTConv)
                        state.dataSurface->OSCM(OPtr).TConv = state.dataSurface->OSCM(OPtr).EMSOverrideTConvValue;
                    if (state.dataSurface->OSCM(OPtr).EMSOverrideOnHConv)
                        state.dataSurface->OSCM(OPtr).HConv = state.dataSurface->OSCM(OPtr).EMSOverrideHConvValue;
                    if (state.dataSurface->OSCM(OPtr).EMSOverrideOnTRad)
                        state.dataSurface->OSCM(OPtr).TRad = state.dataSurface->OSCM(OPtr).EMSOverrideTRadValue;
                    if (state.dataSurface->OSCM(OPtr).EMSOverrideOnHrad)
                        state.dataSurface->OSCM(OPtr).HRad = state.dataSurface->OSCM(OPtr).EMSOverrideHradValue;
                    state.dataHeatBalSurf->HcExtSurf(SurfNum) = state.dataSurface->OSCM(OPtr).HConv;

                    Real64 TempExt = state.dataSurface->OSCM(OPtr).TConv;

                    // Set the only radiant system heat balance coefficient that is non-zero for this case
                    if (state.dataConstruction->Construct(ConstrNum).SourceSinkPresent)
                        state.dataHeatBalFanSys->RadSysToHBConstCoef(SurfNum) = state.dataHeatBalSurf->TH(1, 1, SurfNum);

                    if (Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::CondFD ||
                        Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::HAMT) {
                        // Set variables used in the FD moisture balance and HAMT
                        state.dataMstBal->TempOutsideAirFD(SurfNum) = TempExt;
                        state.dataMstBal->RhoVaporAirOut(SurfNum) =
                            PsyRhovFnTdbWPb(state.dataMstBal->TempOutsideAirFD(SurfNum), state.dataEnvrn->OutHumRat, state.dataEnvrn->OutBaroPress);
                        state.dataMstBal->HConvExtFD(SurfNum) = state.dataHeatBalSurf->HcExtSurf(SurfNum);
                        state.dataMstBal->HMassConvExtFD(SurfNum) =
                            state.dataMstBal->HConvExtFD(SurfNum) /
                            ((PsyRhoAirFnPbTdbW(
                                  state,
                                  state.dataEnvrn->OutBaroPress,
                                  state.dataMstBal->TempOutsideAirFD(SurfNum),
                                  PsyWFnTdbRhPb(
                                      state, state.dataMstBal->TempOutsideAirFD(SurfNum), 1.0, state.dataEnvrn->OutBaroPress, RoutineNameOSCM)) +
                              state.dataMstBal->RhoVaporAirOut(SurfNum)) *
                             PsyCpAirFnW(state.dataEnvrn->OutHumRat));
                        state.dataMstBal->HSkyFD(SurfNum) = state.dataSurface->OSCM(OPtr).HRad; // CR 8046, use sky term for surface to baffle IR
                        state.dataMstBal->HGrndFD(SurfNum) = 0.0; // CR 8046, null out and use only sky term for surface to baffle IR
                        state.dataMstBal->HAirFD(SurfNum) = 0.0;  // CR 8046, null out and use only sky term for surface to baffle IR
                    }

                    // Call the outside surface temp calculation and pass the necessary terms
                    if (Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::CTF ||
                        Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::EMPD) {

                        if (state.dataSurface->SurfExtCavityPresent(SurfNum)) {
                            CalcExteriorVentedCavity(state, SurfNum);
                        }

                        CalcOutsideSurfTemp(state, SurfNum, zoneNum, ConstrNum, HMovInsul, TempExt, MovInsulErrorFlag);
                        if (MovInsulErrorFlag) ShowFatalError(state, "CalcOutsideSurfTemp: Program terminates due to preceding conditions.");

                    } else if (Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::CondFD ||
                               Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::HAMT) {
                        if (state.dataSurface->SurfExtCavityPresent(SurfNum)) {
                            CalcExteriorVentedCavity(state, SurfNum);
                        }
                    }

                    // This ends the calculations for this surface and goes on to the next SurfNum
                } else if (SELECT_CASE_var == ExternalEnvironment) {

                    // checking the EcoRoof presented in the external environment
                    // recompute each load by calling ecoroof

                    Real64 TempExt;

                    if (state.dataSurface->SurfExtEcoRoof(SurfNum)) {
                        CalcEcoRoof(state, SurfNum, zoneNum, ConstrNum, TempExt);
                        continue;
                    }
                    // Roughness index of the exterior surface
                    int RoughSurf = state.dataHeatBalSurf->SurfRoughnessExt(SurfNum);
                    // Thermal absoptance of the exterior surface
                    Real64 AbsThermSurf = state.dataHeatBalSurf->SurfAbsThermalExt(SurfNum);
                    HMovInsul = 0;
                    // Check for outside movable insulation
                    if (state.dataSurface->AnyMovableInsulation && state.dataHeatBalSurf->SurfMovInsulExtPresent(SurfNum)) {
                        HMovInsul = state.dataHeatBalSurf->SurfMovInsulHExt(SurfNum);
                    }

                    // Check for exposure to wind (exterior environment)
                    if (Surface(SurfNum).ExtWind) {

                        // Calculate exterior heat transfer coefficients with windspeed (windspeed is calculated internally in subroutine)
                        InitExteriorConvectionCoeff(state,
                                                    SurfNum,
                                                    HMovInsul,
                                                    RoughSurf,
                                                    AbsThermSurf,
                                                    state.dataHeatBalSurf->TH(1, 1, SurfNum),
                                                    state.dataHeatBalSurf->HcExtSurf(SurfNum),
                                                    state.dataHeatBalSurf->HSkyExtSurf(SurfNum),
                                                    state.dataHeatBalSurf->HGrdExtSurf(SurfNum),
                                                    state.dataHeatBalSurf->HAirExtSurf(SurfNum));

                        if (state.dataEnvrn->IsRain) { // Raining: since wind exposed, outside surface gets wet

                            if (state.dataSurface->SurfExtConvCoeffIndex(SurfNum) <= 0) { // Reset HcExtSurf because of wetness
                                state.dataHeatBalSurf->HcExtSurf(SurfNum) = 1000.0;
                            } else { // User set
                                state.dataHeatBalSurf->HcExtSurf(SurfNum) = SetExtConvectionCoeff(state, SurfNum);
                            }

                            TempExt = state.dataSurface->SurfOutWetBulbTemp(SurfNum);

                            // start HAMT
                            if (Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::HAMT) {
                                // Set variables used in the HAMT moisture balance
                                state.dataMstBal->TempOutsideAirFD(SurfNum) = TempExt;
                                state.dataMstBal->RhoVaporAirOut(SurfNum) =
                                    PsyRhovFnTdbRh(state, state.dataMstBal->TempOutsideAirFD(SurfNum), 1.0, HBSurfManRainHAMT);
                                state.dataMstBal->HConvExtFD(SurfNum) = state.dataHeatBalSurf->HcExtSurf(SurfNum);
                                state.dataMstBal->HMassConvExtFD(SurfNum) =
                                    state.dataMstBal->HConvExtFD(SurfNum) /
                                    ((PsyRhoAirFnPbTdbW(state,
                                                        state.dataEnvrn->OutBaroPress,
                                                        state.dataMstBal->TempOutsideAirFD(SurfNum),
                                                        PsyWFnTdbRhPb(state,
                                                                      state.dataMstBal->TempOutsideAirFD(SurfNum),
                                                                      1.0,
                                                                      state.dataEnvrn->OutBaroPress,
                                                                      RoutineNameExtEnvWetSurf)) +
                                      state.dataMstBal->RhoVaporAirOut(SurfNum)) *
                                     PsyCpAirFnW(state.dataEnvrn->OutHumRat));
                                state.dataMstBal->HSkyFD(SurfNum) = state.dataHeatBalSurf->HSkyExtSurf(SurfNum);
                                state.dataMstBal->HGrndFD(SurfNum) = state.dataHeatBalSurf->HGrdExtSurf(SurfNum);
                                state.dataMstBal->HAirFD(SurfNum) = state.dataHeatBalSurf->HAirExtSurf(SurfNum);
                            }
                            // end HAMT
                            if (Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::CondFD) {
                                // Set variables used in the FD moisture balance
                                state.dataMstBal->TempOutsideAirFD(SurfNum) = TempExt;
                                state.dataMstBal->RhoVaporAirOut(SurfNum) = PsyRhovFnTdbRhLBnd0C(state.dataMstBal->TempOutsideAirFD(SurfNum), 1.0);
                                state.dataMstBal->HConvExtFD(SurfNum) = state.dataHeatBalSurf->HcExtSurf(SurfNum);
                                state.dataMstBal->HMassConvExtFD(SurfNum) =
                                    state.dataMstBal->HConvExtFD(SurfNum) /
                                    ((PsyRhoAirFnPbTdbW(state,
                                                        state.dataEnvrn->OutBaroPress,
                                                        state.dataMstBal->TempOutsideAirFD(SurfNum),
                                                        PsyWFnTdbRhPb(state,
                                                                      state.dataMstBal->TempOutsideAirFD(SurfNum),
                                                                      1.0,
                                                                      state.dataEnvrn->OutBaroPress,
                                                                      RoutineNameExtEnvWetSurf)) +
                                      state.dataMstBal->RhoVaporAirOut(SurfNum)) *
                                     PsyCpAirFnW(state.dataEnvrn->OutHumRat));
                                state.dataMstBal->HSkyFD(SurfNum) = state.dataHeatBalSurf->HSkyExtSurf(SurfNum);
                                state.dataMstBal->HGrndFD(SurfNum) = state.dataHeatBalSurf->HGrdExtSurf(SurfNum);
                                state.dataMstBal->HAirFD(SurfNum) = state.dataHeatBalSurf->HAirExtSurf(SurfNum);
                            }

                        } else { // Surface is dry, use the normal correlation

                            TempExt = state.dataSurface->SurfOutDryBulbTemp(SurfNum);

                            if (Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::CondFD ||
                                Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::HAMT) {
                                // Set variables used in the FD moisture balance and HAMT
                                state.dataMstBal->TempOutsideAirFD(SurfNum) = TempExt;
                                state.dataMstBal->RhoVaporAirOut(SurfNum) = PsyRhovFnTdbWPb(
                                    state.dataMstBal->TempOutsideAirFD(SurfNum), state.dataEnvrn->OutHumRat, state.dataEnvrn->OutBaroPress);
                                state.dataMstBal->HConvExtFD(SurfNum) = state.dataHeatBalSurf->HcExtSurf(SurfNum);
                                state.dataMstBal->HMassConvExtFD(SurfNum) =
                                    state.dataMstBal->HConvExtFD(SurfNum) /
                                    ((PsyRhoAirFnPbTdbW(state,
                                                        state.dataEnvrn->OutBaroPress,
                                                        state.dataMstBal->TempOutsideAirFD(SurfNum),
                                                        PsyWFnTdbRhPb(state,
                                                                      state.dataMstBal->TempOutsideAirFD(SurfNum),
                                                                      1.0,
                                                                      state.dataEnvrn->OutBaroPress,
                                                                      RoutineNameExtEnvDrySurf)) +
                                      state.dataMstBal->RhoVaporAirOut(SurfNum)) *
                                     PsyCpAirFnW(state.dataEnvrn->OutHumRat));
                                //  check for saturation conditions of air
                                // Local temporary saturated vapor density for checking
                                Real64 RhoVaporSat = PsyRhovFnTdbRh(state, state.dataMstBal->TempOutsideAirFD(SurfNum), 1.0, HBSurfManDrySurfCondFD);
                                if (state.dataMstBal->RhoVaporAirOut(SurfNum) > RhoVaporSat) state.dataMstBal->RhoVaporAirOut(SurfNum) = RhoVaporSat;
                                state.dataMstBal->HSkyFD(SurfNum) = state.dataHeatBalSurf->HSkyExtSurf(SurfNum);
                                state.dataMstBal->HGrndFD(SurfNum) = state.dataHeatBalSurf->HGrdExtSurf(SurfNum);
                                state.dataMstBal->HAirFD(SurfNum) = state.dataHeatBalSurf->HAirExtSurf(SurfNum);
                            }
                        }

                    } else { // No wind

                        // Calculate exterior heat transfer coefficients for windspeed = 0
                        InitExteriorConvectionCoeff(state,
                                                    SurfNum,
                                                    HMovInsul,
                                                    RoughSurf,
                                                    AbsThermSurf,
                                                    state.dataHeatBalSurf->TH(1, 1, SurfNum),
                                                    state.dataHeatBalSurf->HcExtSurf(SurfNum),
                                                    state.dataHeatBalSurf->HSkyExtSurf(SurfNum),
                                                    state.dataHeatBalSurf->HGrdExtSurf(SurfNum),
                                                    state.dataHeatBalSurf->HAirExtSurf(SurfNum));

                        TempExt = state.dataSurface->SurfOutDryBulbTemp(SurfNum);

                        if (Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::CondFD ||
                            Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::HAMT) {
                            // Set variables used in the FD moisture balance and HAMT
                            state.dataMstBal->TempOutsideAirFD(SurfNum) = TempExt;
                            state.dataMstBal->RhoVaporAirOut(SurfNum) = PsyRhovFnTdbWPb(
                                state.dataMstBal->TempOutsideAirFD(SurfNum), state.dataEnvrn->OutHumRat, state.dataEnvrn->OutBaroPress);
                            state.dataMstBal->HConvExtFD(SurfNum) = state.dataHeatBalSurf->HcExtSurf(SurfNum);
                            state.dataMstBal->HMassConvExtFD(SurfNum) =
                                state.dataMstBal->HConvExtFD(SurfNum) / ((PsyRhoAirFnPbTdbW(state,
                                                                                            state.dataEnvrn->OutBaroPress,
                                                                                            state.dataMstBal->TempOutsideAirFD(SurfNum),
                                                                                            PsyWFnTdbRhPb(state,
                                                                                                          state.dataMstBal->TempOutsideAirFD(SurfNum),
                                                                                                          1.0,
                                                                                                          state.dataEnvrn->OutBaroPress,
                                                                                                          RoutineNameNoWind)) +
                                                                          state.dataMstBal->RhoVaporAirOut(SurfNum)) *
                                                                         PsyCpAirFnW(state.dataEnvrn->OutHumRat));
                            state.dataMstBal->HSkyFD(SurfNum) = state.dataHeatBalSurf->HSkyExtSurf(SurfNum);
                            state.dataMstBal->HGrndFD(SurfNum) = state.dataHeatBalSurf->HGrdExtSurf(SurfNum);
                            state.dataMstBal->HAirFD(SurfNum) = state.dataHeatBalSurf->HAirExtSurf(SurfNum);
                        }
                    }
                    // Calculate LWR from surrounding surfaces if defined for an exterior surface
                    if (state.dataSurface->SurfHasSurroundingSurfProperties(SurfNum)) {
                        int SrdSurfsNum = state.dataSurface->SurfSurroundingSurfacesNum(SurfNum);
                        // Absolute temperature of the outside surface of an exterior surface
                        Real64 TSurf = state.dataHeatBalSurf->TH(1, 1, SurfNum) + DataGlobalConstants::KelvinConv;
                        for (int SrdSurfNum = 1; SrdSurfNum <= state.dataSurface->SurroundingSurfsProperty(SrdSurfsNum).TotSurroundingSurface;
                             SrdSurfNum++) {
                            // View factor of a surrounding surface
                            Real64 SrdSurfViewFac = state.dataSurface->SurroundingSurfsProperty(SrdSurfsNum).SurroundingSurfs(SrdSurfNum).ViewFactor;
                            // Absolute temperature of a surrounding surface
                            Real64 SrdSurfTempAbs =
                                GetCurrentScheduleValue(
                                    state, state.dataSurface->SurroundingSurfsProperty(SrdSurfsNum).SurroundingSurfs(SrdSurfNum).TempSchNum) +
                                DataGlobalConstants::KelvinConv;
                            state.dataHeatBalSurf->SurfQRadLWOutSrdSurfs(SurfNum) +=
                                DataGlobalConstants::StefanBoltzmann * AbsThermSurf * SrdSurfViewFac * (pow_4(SrdSurfTempAbs) - pow_4(TSurf));
                        }
                    }

                    if (Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::CTF ||
                        Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::EMPD ||
                        Surface(SurfNum).Class == SurfaceClass::TDD_Dome) {
                        CalcOutsideSurfTemp(state, SurfNum, zoneNum, ConstrNum, HMovInsul, TempExt, MovInsulErrorFlag);
                        if (MovInsulErrorFlag) ShowFatalError(state, "CalcOutsideSurfTemp: Program terminates due to preceding conditions.");
                    }

                } else if (SELECT_CASE_var == KivaFoundation) {
                    int RoughSurf = state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).Roughness;
                    Real64 AbsThermSurf = state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).AbsorpThermal;

                    // Set Kiva exterior convection algorithms
                    InitExteriorConvectionCoeff(state,
                                                SurfNum,
                                                HMovInsul,
                                                RoughSurf,
                                                AbsThermSurf,
                                                state.dataHeatBalSurf->TH(1, 1, SurfNum),
                                                state.dataHeatBalSurf->HcExtSurf(SurfNum),
                                                state.dataHeatBalSurf->HSkyExtSurf(SurfNum),
                                                state.dataHeatBalSurf->HGrdExtSurf(SurfNum),
                                                state.dataHeatBalSurf->HAirExtSurf(SurfNum));

                } else { // for interior or other zone surfaces

                    if (Surface(SurfNum).ExtBoundCond == SurfNum) { // Regular partition/internal mass

                        state.dataHeatBalSurf->TH(1, 1, SurfNum) = state.dataHeatBalSurf->TempSurfIn(SurfNum);

                        // No need to set any radiant system heat balance coefficients here--will be done during inside heat balance

                        if (Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::CondFD ||
                            Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::HAMT) {
                            // Set variables used in the FD moisture balance HAMT
                            state.dataMstBal->TempOutsideAirFD(SurfNum) = state.dataHeatBalSurf->TempSurfIn(SurfNum);
                            state.dataMstBal->RhoVaporAirOut(SurfNum) = state.dataMstBal->RhoVaporAirIn(SurfNum);
                            state.dataMstBal->HConvExtFD(SurfNum) = state.dataHeatBal->HConvIn(SurfNum);
                            state.dataMstBal->HMassConvExtFD(SurfNum) =
                                state.dataMstBal->HConvExtFD(SurfNum) /
                                ((PsyRhoAirFnPbTdbW(
                                      state,
                                      state.dataEnvrn->OutBaroPress,
                                      state.dataMstBal->TempOutsideAirFD(SurfNum),
                                      PsyWFnTdbRhPb(
                                          state, state.dataMstBal->TempOutsideAirFD(SurfNum), 1.0, state.dataEnvrn->OutBaroPress, RoutineNameOther)) +
                                  state.dataMstBal->RhoVaporAirOut(SurfNum)) *
                                 PsyCpAirFnW(state.dataEnvrn->OutHumRat));
                            state.dataMstBal->HSkyFD(SurfNum) = 0.0;
                            state.dataMstBal->HGrndFD(SurfNum) = 0.0;
                            state.dataMstBal->HAirFD(SurfNum) = 0.0;
                        }

                    } else { // Interzone partition

                        state.dataHeatBalSurf->TH(1, 1, SurfNum) = state.dataHeatBalSurf->TH(2, 1, Surface(SurfNum).ExtBoundCond);

                        // No need to set any radiant system heat balance coefficients here--will be done during inside heat balance

                        if (Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::CondFD ||
                            Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::HAMT) {
                            // Set variables used in the FD moisture balance and HAMT
                            state.dataMstBal->TempOutsideAirFD(SurfNum) = state.dataHeatBalSurf->TH(2, 1, Surface(SurfNum).ExtBoundCond);
                            state.dataMstBal->RhoVaporAirOut(SurfNum) = state.dataMstBal->RhoVaporAirIn(Surface(SurfNum).ExtBoundCond);
                            state.dataMstBal->HConvExtFD(SurfNum) = state.dataHeatBal->HConvIn(Surface(SurfNum).ExtBoundCond);
                            state.dataMstBal->HMassConvExtFD(SurfNum) =
                                state.dataMstBal->HConvExtFD(SurfNum) / ((PsyRhoAirFnPbTdbW(state,
                                                                                            state.dataEnvrn->OutBaroPress,
                                                                                            state.dataMstBal->TempOutsideAirFD(SurfNum),
                                                                                            PsyWFnTdbRhPb(state,
                                                                                                          state.dataMstBal->TempOutsideAirFD(SurfNum),
                                                                                                          1.0,
                                                                                                          state.dataEnvrn->OutBaroPress,
                                                                                                          RoutineNameIZPart)) +
                                                                          state.dataMstBal->RhoVaporAirOut(SurfNum)) *
                                                                         PsyCpAirFnW(state.dataEnvrn->OutHumRat));
                            state.dataMstBal->HSkyFD(SurfNum) = 0.0;
                            state.dataMstBal->HGrndFD(SurfNum) = 0.0;
                            state.dataMstBal->HAirFD(SurfNum) = 0.0;
                        }
                    }

                    // This ends the calculations for this surface and goes on to the next SurfNum
                }
            }

            // fill in reporting values for outside face

            state.dataHeatBalSurf->QdotConvOutRepPerArea(SurfNum) = GetQdotConvOutRepPerArea(state, SurfNum);

            state.dataHeatBalSurf->QdotConvOutRep(SurfNum) = state.dataHeatBalSurf->QdotConvOutRepPerArea(SurfNum) * Surface(SurfNum).Area;

            state.dataHeatBalSurf->QConvOutReport(SurfNum) = state.dataHeatBalSurf->QdotConvOutRep(SurfNum) * state.dataGlobal->TimeStepZoneSec;

            state.dataHeatBalSurf->QHeatEmiReport(SurfNum) =
                state.dataHeatBalSurf->QAirExtReport(SurfNum) - state.dataHeatBalSurf->QdotConvOutRep(SurfNum);
        }
    } // ...end of DO loop over all surface (actually heat transfer surfaces)
}

Real64 GetQdotConvOutRepPerArea(EnergyPlusData &state, int const SurfNum)
{
    auto &Surface(state.dataSurface->Surface);
    int OPtr = Surface(SurfNum).OSCMPtr;
    if (Surface(SurfNum).OSCMPtr > 0) { // Optr is set above in this case, use OSCM boundary data
        return -state.dataSurface->OSCM(OPtr).HConv * (state.dataHeatBalSurf->TH(1, 1, SurfNum) - state.dataSurface->OSCM(OPtr).TConv);
    } else {
        if (state.dataEnvrn->IsRain) {
            return -state.dataHeatBalSurf->HcExtSurf(SurfNum) *
                   (state.dataHeatBalSurf->TH(1, 1, SurfNum) - state.dataSurface->SurfOutWetBulbTemp(SurfNum));
        } else {
            return -state.dataHeatBalSurf->HcExtSurf(SurfNum) *
                   (state.dataHeatBalSurf->TH(1, 1, SurfNum) - state.dataSurface->SurfOutDryBulbTemp(SurfNum));
        }
    }
}

void CalcHeatBalanceInsideSurf(EnergyPlusData &state,
                               Optional_int_const ZoneToResimulate) // if passed in, then only calculate surfaces that have this zone
{
    auto &Surface(state.dataSurface->Surface);
    if (state.dataHeatBalSurfMgr->calcHeatBalInsideSurfFirstTime) {
        if (state.dataHeatBal->AnyEMPD) {
            state.dataHeatBalSurf->MinIterations = MinEMPDIterations;
        }
        if (state.dataGlobal->DisplayAdvancedReportVariables) {
            SetupOutputVariable(state,
                                "Surface Inside Face Heat Balance Calculation Iteration Count",
                                OutputProcessor::Unit::None,
                                state.dataHeatBal->InsideSurfIterations,
                                "ZONE",
                                "Sum",
                                "Simulation");
        }
        // Precompute whether CTF temperature limits will be needed
        state.dataHeatBalSurf->Zone_has_mixed_HT_models.resize(state.dataGlobal->NumOfZones + 1, false);
        for (int iZone = 1; iZone <= state.dataGlobal->NumOfZones; ++iZone) {
            auto const &zone(state.dataHeatBal->Zone(iZone));
            for (int iSurf = zone.HTSurfaceFirst, eSurf = zone.HTSurfaceLast; iSurf <= eSurf; ++iSurf) {
                auto const alg(Surface(iSurf).HeatTransferAlgorithm);
                if ((alg == DataSurfaces::iHeatTransferModel::CondFD) || (alg == DataSurfaces::iHeatTransferModel::HAMT) ||
                    (alg == DataSurfaces::iHeatTransferModel::Kiva)) {
                    state.dataHeatBalSurf->Zone_has_mixed_HT_models[iZone] = true;
                    break;
                }
            }
        }
        state.dataHeatBalSurfMgr->calcHeatBalInsideSurfFirstTime = false;
    }

    if (state.dataGlobal->BeginEnvrnFlag && state.dataHeatBalSurfMgr->calcHeatBalInsideSurEnvrnFlag) {
        state.dataHeatBalSurf->TempInsOld = 23.0;
        state.dataHeatBalSurfMgr->RefAirTemp = 23.0;
        state.dataHeatBal->SurfTempEffBulkAir = 23.0;
        state.dataHeatBalSurfMgr->calcHeatBalInsideSurfWarmupErrCount = 0;
        state.dataHeatBalSurfMgr->calcHeatBalInsideSurEnvrnFlag = false;

        // Initialize Kiva instances ground temperatures
        if (state.dataHeatBal->AnyKiva) {
            state.dataSurfaceGeometry->kivaManager.initKivaInstances(state);
        }
    }
    if (!state.dataGlobal->BeginEnvrnFlag) {
        state.dataHeatBalSurfMgr->calcHeatBalInsideSurEnvrnFlag = true;
    }

    // Pass correct list of surfaces to CalcHeatBalanceInsideSurf2
    bool const PartialResimulate(present(ZoneToResimulate));

    if (!PartialResimulate) {
        // Zero window heat gains for all zones
        state.dataHeatBal->ZoneWinHeatGain = 0.0;
        state.dataHeatBal->ZoneWinHeatGainRep = 0.0;
        state.dataHeatBal->ZoneWinHeatGainRepEnergy = 0.0;
        state.dataHeatBal->ZoneWinHeatLossRep = 0.0;
        state.dataHeatBal->ZoneWinHeatLossRepEnergy = 0.0;

        if (state.dataHeatBal->AllCTF) {
            CalcHeatBalanceInsideSurf2CTFOnly(state, 1, state.dataGlobal->NumOfZones, state.dataSurface->AllIZSurfaceList);
        } else {
            CalcHeatBalanceInsideSurf2(state,
                                       state.dataSurface->AllHTSurfaceList,
                                       state.dataSurface->AllIZSurfaceList,
                                       state.dataSurface->AllHTNonWindowSurfaceList,
                                       state.dataSurface->AllHTWindowSurfaceList);
        }
        // Sort window heat gain/loss
        for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
            if (state.dataHeatBal->ZoneWinHeatGain(zoneNum) >= 0.0) {
                state.dataHeatBal->ZoneWinHeatGainRep(zoneNum) = state.dataHeatBal->ZoneWinHeatGain(zoneNum);
                state.dataHeatBal->ZoneWinHeatGainRepEnergy(zoneNum) =
                    state.dataHeatBal->ZoneWinHeatGainRep(zoneNum) * state.dataGlobal->TimeStepZoneSec;
            } else {
                state.dataHeatBal->ZoneWinHeatLossRep(zoneNum) = -state.dataHeatBal->ZoneWinHeatGain(zoneNum);
                state.dataHeatBal->ZoneWinHeatLossRepEnergy(zoneNum) =
                    state.dataHeatBal->ZoneWinHeatLossRep(zoneNum) * state.dataGlobal->TimeStepZoneSec;
            }
        }
    } else {
        // Zero window heat gains for resimulate zone
        state.dataHeatBal->ZoneWinHeatGain(ZoneToResimulate) = 0.0;
        state.dataHeatBal->ZoneWinHeatGainRep(ZoneToResimulate) = 0.0;
        state.dataHeatBal->ZoneWinHeatGainRepEnergy(ZoneToResimulate) = 0.0;
        state.dataHeatBal->ZoneWinHeatLossRep(ZoneToResimulate) = 0.0;
        state.dataHeatBal->ZoneWinHeatLossRepEnergy(ZoneToResimulate) = 0.0;

        auto const &zoneHTSurfList(state.dataHeatBal->Zone(ZoneToResimulate).ZoneHTSurfaceList);
        auto const &zoneIZSurfList(state.dataHeatBal->Zone(ZoneToResimulate).ZoneIZSurfaceList);
        auto const &zoneHTNonWindowSurfList(state.dataHeatBal->Zone(ZoneToResimulate).ZoneHTNonWindowSurfaceList);
        auto const &zoneHTWindowSurfList(state.dataHeatBal->Zone(ZoneToResimulate).ZoneHTWindowSurfaceList);
        // Cannot use CalcHeatBalanceInsideSurf2CTFOnly because resimulated zone includes adjacent interzone surfaces
        CalcHeatBalanceInsideSurf2(state, zoneHTSurfList, zoneIZSurfList, zoneHTNonWindowSurfList, zoneHTWindowSurfList, ZoneToResimulate);
        // Sort window heat gain/loss
        if (state.dataHeatBal->ZoneWinHeatGain(ZoneToResimulate) >= 0.0) {
            state.dataHeatBal->ZoneWinHeatGainRep(ZoneToResimulate) = state.dataHeatBal->ZoneWinHeatGain(ZoneToResimulate);
            state.dataHeatBal->ZoneWinHeatGainRepEnergy(ZoneToResimulate) =
                state.dataHeatBal->ZoneWinHeatGainRep(ZoneToResimulate) * state.dataGlobal->TimeStepZoneSec;
        } else {
            state.dataHeatBal->ZoneWinHeatLossRep(ZoneToResimulate) = -state.dataHeatBal->ZoneWinHeatGain(ZoneToResimulate);
            state.dataHeatBal->ZoneWinHeatLossRepEnergy(ZoneToResimulate) =
                state.dataHeatBal->ZoneWinHeatLossRep(ZoneToResimulate) * state.dataGlobal->TimeStepZoneSec;
        }
    }
}

void CalcHeatBalanceInsideSurf2(EnergyPlusData &state,
                                const std::vector<int> &HTSurfs,          // Heat transfer surfaces to simulate (opaque and windows)
                                const std::vector<int> &IZSurfs,          // Interzone heat transfer surfaces to simulate
                                const std::vector<int> &HTNonWindowSurfs, // Non-window heat transfer surfaces to simulate
                                const std::vector<int> &HTWindowSurfs,    // Window heat transfer surfaces to simulate
                                Optional_int_const ZoneToResimulate)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         George Walton
    //       DATE WRITTEN   December 1979
    //       MODIFIED       Jun 1990 (RDT for new CTF arrays)
    //                      Dec 1999 (FCW for window calculation)
    //                      May 2000 (FCW for window frame and dividers)
    //                      Aug 2000 (RJL for MTF moisture calculations)
    //                      Sep 2000 (RKS for new radiant exchange algorithm)
    //                      Dec 2000 (RKS for radiant system model addition)
    //                      Jul 2003 (CC) set the reference temperatures for inside surface heat balance
    //                                    depending on convection algorithms and/or air models used
    //                      May 2006 (RR  account for exterior window screen)
    //                      Jul 2008 (P. Biddulph include calls to HAMT)
    //                      Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems
    //       RE-ENGINEERED  Mar 1998 (RKS)

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine performs a heat balance on the inside face of each
    // surface in the building.

    // METHODOLOGY EMPLOYED:
    // Various boundary conditions are set and additional parameters are set-
    // up.  Then, the proper heat balance equation is selected based on whether
    // the surface is a partition or not and on whether or not movable
    // insulation is present on the inside face.

    // REFERENCES:
    // (I)BLAST legacy routine HBSRF

    constexpr const char *rhoAirZone("RhoAirZone");
    constexpr const char *wsurf("Wsurf");
    constexpr const char *HBSurfManInsideSurf("HB,SurfMan:InsideSurf");
    constexpr const char *Inside("Inside");
    static std::string const BlankString;

    Real64 TempSurfOutTmp; // Local Temporary Surface temperature for the outside surface face
    Real64 TempSurfInSat;  // Local temporary surface dew point temperature

    Real64 Wsurf;         // Moisture ratio for HAMT
    Real64 RhoAirZone;    // Zone moisture density for HAMT
    int OtherSideZoneNum; // Zone Number index for other side of an interzone partition HAMT
    auto &Surface(state.dataSurface->Surface);

    // determine reference air temperatures
    for (int SurfNum : HTSurfs) {
        int ZoneNum = Surface(SurfNum).Zone;

        // These conditions are not used in every SurfNum loop here so we don't use them to skip surfaces
        if (Surface(SurfNum).Class == SurfaceClass::TDD_Dome) continue; // Skip TDD:DOME objects.  Inside temp is handled by TDD:DIFFUSER.

        {
            auto const SELECT_CASE_var(state.dataSurface->SurfTAirRef(SurfNum));
            if (SELECT_CASE_var == ZoneMeanAirTemp) {
                state.dataHeatBalSurfMgr->RefAirTemp(SurfNum) = state.dataHeatBalFanSys->MAT(ZoneNum);
                state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataHeatBalFanSys->MAT(ZoneNum); // for reporting surf adjacent air temp
            } else if (SELECT_CASE_var == AdjacentAirTemp) {
                state.dataHeatBalSurfMgr->RefAirTemp(SurfNum) = state.dataHeatBal->SurfTempEffBulkAir(SurfNum);
            } else if (SELECT_CASE_var == ZoneSupplyAirTemp) {
                // determine ZoneEquipConfigNum for this zone
                int ZoneEquipConfigNum = ZoneNum;
                // check whether this zone is a controlled zone or not
                if (!state.dataHeatBal->Zone(ZoneNum).IsControlled) {
                    ShowFatalError(state,
                                   "Zones must be controlled for Ceiling-Diffuser Convection model. No system serves zone " +
                                       state.dataHeatBal->Zone(ZoneNum).Name);
                    return;
                }
                // determine supply air conditions
                Real64 SumSysMCp = 0.0;
                Real64 SumSysMCpT = 0.0;
                Real64 const CpAir = Psychrometrics::PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum));
                for (int NodeNum = 1; NodeNum <= state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).NumInletNodes; ++NodeNum) {
                    Real64 NodeTemp = state.dataLoopNodes->Node(state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).Temp;
                    Real64 MassFlowRate =
                        state.dataLoopNodes->Node(state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).MassFlowRate;
                    // Real64 CpAir2 = PsyCpAirFnW(ZoneAirHumRat(ZoneNum), NodeTemp);
                    SumSysMCp += MassFlowRate * CpAir;
                    SumSysMCpT += MassFlowRate * CpAir * NodeTemp;
                }
                // a weighted average of the inlet temperatures.
                if (SumSysMCp > 0.0) {                                                      // protect div by zero
                    state.dataHeatBalSurfMgr->RefAirTemp(SurfNum) = SumSysMCpT / SumSysMCp; // BG changed 02-16-2005 to add index (SurfNum)
                } else {
                    state.dataHeatBalSurfMgr->RefAirTemp(SurfNum) = state.dataHeatBalFanSys->MAT(ZoneNum);
                }
                state.dataHeatBal->SurfTempEffBulkAir(SurfNum) =
                    state.dataHeatBalSurfMgr->RefAirTemp(SurfNum); // for reporting surf adjacent air temp
            } else {
                // currently set to mean air temp but should add error warning here
                state.dataHeatBalSurfMgr->RefAirTemp(SurfNum) = state.dataHeatBalFanSys->MAT(ZoneNum);
                state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataHeatBalFanSys->MAT(ZoneNum); // for reporting surf adjacent air temp
            }
        }
    }

    // Following variables must be reset due to possible recall of this routine by radiant and Resimulate routines.
    // CalcWindowHeatBalance is called, then, multiple times and these need to be initialized before each call to
    // CalcWindowHeatBalance.
    // Only for Surface(SurfNum).Class == DataSurfaces::SurfaceClass::Window
    for (int surfNum : HTWindowSurfs) {
        state.dataSurface->SurfWinHeatGain(surfNum) = 0.0;
        state.dataSurface->SurfWinHeatTransfer(surfNum) = 0.0;
        state.dataSurface->SurfWinHeatGainRep(surfNum) = 0.0;
        state.dataSurface->SurfWinHeatLossRep(surfNum) = 0.0;
        state.dataSurface->SurfWinGainConvGlazToZoneRep(surfNum) = 0.0;
        state.dataSurface->SurfWinGainIRGlazToZoneRep(surfNum) = 0.0;
        state.dataSurface->SurfWinLossSWZoneToOutWinRep(surfNum) = 0.0;
        state.dataSurface->SurfWinGainFrameDividerToZoneRep(surfNum) = 0.0;
        state.dataSurface->SurfWinGainConvGlazShadGapToZoneRep(surfNum) = 0.0;
        state.dataSurface->SurfWinGainConvShadeToZoneRep(surfNum) = 0.0;
        state.dataSurface->SurfWinOtherConvGainInsideFaceToZoneRep(surfNum) = 0.0;
        state.dataSurface->SurfWinGainIRShadeToZoneRep(surfNum) = 0.0;
        state.dataSurface->SurfWinFrameQRadOutAbs(surfNum) = 0.0;
        state.dataSurface->SurfWinFrameQRadInAbs(surfNum) = 0.0;
        state.dataSurface->SurfWinDividerQRadOutAbs(surfNum) = 0.0;
        state.dataSurface->SurfWinDividerQRadInAbs(surfNum) = 0.0;
    }

    state.dataHeatBal->InsideSurfIterations = 0;

    // Calculate heat extract due to additional heat flux source term as the surface boundary condition
    if (state.dataSurface->AnyHeatBalanceInsideSourceTerm) {
        for (int SurfNum : HTSurfs) {
            if (Surface(SurfNum).InsideHeatSourceTermSchedule) {
                state.dataHeatBalSurf->SurfQAdditionalHeatSourceInside(SurfNum) =
                    EnergyPlus::ScheduleManager::GetCurrentScheduleValue(state, Surface(SurfNum).InsideHeatSourceTermSchedule);
            }
        }
    }

    // Calculate Kiva instances
    if (state.dataHeatBal->AnyKiva) {
        if (((state.dataSurfaceGeometry->kivaManager.settings.timestepType == HeatBalanceKivaManager::KivaManager::Settings::HOURLY &&
              state.dataGlobal->TimeStep == 1) ||
             state.dataSurfaceGeometry->kivaManager.settings.timestepType == HeatBalanceKivaManager::KivaManager::Settings::TIMESTEP) &&
            !state.dataGlobal->WarmupFlag) {
            state.dataSurfaceGeometry->kivaManager.calcKivaInstances(state);
        }
    }

    bool Converged = false; // .TRUE. if inside heat balance has converged
    while (!Converged) {    // Start of main inside heat balance DO loop...

        state.dataHeatBalSurf->TempInsOld = state.dataHeatBalSurf->TempSurfIn; // Keep track of last iteration's temperature values

        if (state.dataHeatBal->AnyKiva) {
            for (auto &kivaSurf : state.dataSurfaceGeometry->kivaManager.surfaceMap) {
                state.dataHeatBalSurf->TempSurfIn(kivaSurf.first) =
                    kivaSurf.second.results.Tavg - DataGlobalConstants::KelvinConv; // TODO: Use average radiant temp? Trad?
            }
        }

        HeatBalanceIntRadExchange::CalcInteriorRadExchange(state,
                                                           state.dataHeatBalSurf->TempSurfIn,
                                                           state.dataHeatBal->InsideSurfIterations,
                                                           state.dataHeatBalSurf->SurfNetLWRadToSurf,
                                                           ZoneToResimulate,
                                                           Inside); // Update the radiation balance

        if (state.dataHeatBal->AnyKiva) {
            for (auto &kivaSurf : state.dataSurfaceGeometry->kivaManager.surfaceMap) {
                state.dataHeatBalSurf->TempSurfIn(kivaSurf.first) = state.dataHeatBalSurf->TempInsOld(kivaSurf.first);
            }
        }

        // Every 30 iterations, recalculate the inside convection coefficients in case
        // there has been a significant drift in the surface temperatures predicted.
        // This is not fool-proof and it basically means that the outside surface
        // heat balance is in error (potentially) once HConvIn is re-evaluated.
        // The choice of 30 is not significant--just want to do this a couple of
        // times before the iteration limit is hit.
        if ((state.dataHeatBal->InsideSurfIterations > 0) && (mod(state.dataHeatBal->InsideSurfIterations, ItersReevalConvCoeff) == 0)) {
            ConvectionCoefficients::InitInteriorConvectionCoeffs(state, state.dataHeatBalSurf->TempSurfIn, ZoneToResimulate);
        }

        if (state.dataHeatBal->AnyEMPD || state.dataHeatBal->AnyHAMT) {
            for (int SurfNum : HTSurfs) {
                auto &surface(Surface(SurfNum));
                if (surface.Class == SurfaceClass::TDD_Dome) continue; // Skip TDD:DOME objects.  Inside temp is handled by TDD:DIFFUSER.

                // Calculate the inside surface moisture quantities
                // calculate the inside surface moisture transfer conditions
                // check for saturation conditions of air
                if ((surface.HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::EMPD) ||
                    (surface.HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::HAMT)) {
                    int ZoneNum = Surface(SurfNum).Zone;
                    Real64 const MAT_zone(state.dataHeatBalFanSys->MAT(ZoneNum));
                    Real64 const ZoneAirHumRat_zone(max(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum), 1.0e-5));
                    Real64 const HConvIn_surf(state.dataMstBal->HConvInFD(SurfNum) = state.dataHeatBal->HConvIn(SurfNum));

                    state.dataMstBal->RhoVaporAirIn(SurfNum) =
                        min(Psychrometrics::PsyRhovFnTdbWPb_fast(MAT_zone, ZoneAirHumRat_zone, state.dataEnvrn->OutBaroPress),
                            Psychrometrics::PsyRhovFnTdbRh(state, MAT_zone, 1.0, HBSurfManInsideSurf));
                    state.dataMstBal->HMassConvInFD(SurfNum) =
                        HConvIn_surf / (Psychrometrics::PsyRhoAirFnPbTdbW_fast(state, state.dataEnvrn->OutBaroPress, MAT_zone, ZoneAirHumRat_zone) *
                                        Psychrometrics::PsyCpAirFnW_fast(ZoneAirHumRat_zone));
                }
            }
        }

        for (int SurfNum : HTNonWindowSurfs) {
            // Perform heat balance on the inside face of the surface ...
            // The following are possibilities here:
            //   (a) the surface is a pool (no movable insulation, no source/sink, only CTF solution algorithm)
            //   (b) the surface is a partition, in which case the temperature of both sides are the same
            //   (c) standard (or interzone) opaque surface with no movable insulation, normal heat balance equation
            //   (d) standard (or interzone) window: call to CalcWindowHeatBalance to get window layer temperatures
            //   (e) standard opaque surface with movable insulation, special two-part equation
            // In the surface calculation there are the following Algorithm types for opaque surfaces that
            // do not have movable insulation:
            //   (a) the regular CTF calc (SolutionAlgo = UseCTF)
            //   (b) the EMPD calc (Solutionalgo = UseEMPD)
            //   (c) the CondFD calc (SolutionAlgo = UseCondFD)
            //   (d) the HAMT calc (solutionalgo = UseHAMT).

            int const ZoneNum = Surface(SurfNum).Zone;
            auto &surface(Surface(SurfNum));
            Real64 &TH11(state.dataHeatBalSurf->TH(1, 1, SurfNum));
            int const ConstrNum = surface.Construction;
            auto const &construct(state.dataConstruction->Construct(ConstrNum));
            Real64 const MAT_zone(state.dataHeatBalFanSys->MAT(ZoneNum));
            Real64 const HConvIn_surf(state.dataMstBal->HConvInFD(SurfNum) = state.dataHeatBal->HConvIn(SurfNum));

            if (surface.ExtBoundCond == SurfNum) {
                // CR6869 -- let Window HB take care of it      IF (Surface(SurfNum)%ExtBoundCond == SurfNum) THEN
                // Surface is adiabatic
                if (surface.HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::CTF ||
                    surface.HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::EMPD) { // Regular CTF Surface and/or EMPD surface

                    if (surface.HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::EMPD) {
                        MoistureBalanceEMPDManager::CalcMoistureBalanceEMPD(
                            state, SurfNum, state.dataHeatBalSurf->TempSurfInTmp(SurfNum), MAT_zone, TempSurfInSat);
                    }
                    // Pre-calculate a few terms
                    //
                    Real64 const TempTerm(state.dataHeatBalSurf->CTFConstInPart(SurfNum) + state.dataHeatBal->SurfQRadThermInAbs(SurfNum) +
                                          state.dataHeatBalSurf->SurfOpaqQRadSWInAbs(SurfNum) +
                                          state.dataHeatBalSurf->SurfQAdditionalHeatSourceInside(SurfNum) +
                                          HConvIn_surf * state.dataHeatBalSurfMgr->RefAirTemp(SurfNum) +
                                          state.dataHeatBalFanSys->QHTRadSysSurf(SurfNum) + state.dataHeatBalFanSys->QCoolingPanelSurf(SurfNum) +
                                          state.dataHeatBalFanSys->QHWBaseboardSurf(SurfNum) + state.dataHeatBalFanSys->QSteamBaseboardSurf(SurfNum) +
                                          state.dataHeatBalFanSys->QElecBaseboardSurf(SurfNum) + state.dataHeatBalSurf->SurfNetLWRadToSurf(SurfNum) +
                                          (state.dataHeatBalFanSys->QRadSurfAFNDuct(SurfNum) / state.dataGlobal->TimeStepZoneSec));
                    Real64 const TempDiv(1.0 / (construct.CTFInside(0) - construct.CTFCross(0) + HConvIn_surf + IterDampConst));
                    // Calculate the current inside surface temperature
                    if ((!state.dataSurface->SurfIsPool(SurfNum)) ||
                        ((state.dataSurface->SurfIsPool(SurfNum)) &&
                         (std::abs(state.dataHeatBalFanSys->QPoolSurfNumerator(SurfNum)) < PoolIsOperatingLimit) &&
                         (std::abs(state.dataHeatBalFanSys->PoolHeatTransCoefs(SurfNum)) < PoolIsOperatingLimit))) {
                        if (construct.SourceSinkPresent) {
                            state.dataHeatBalSurf->TempSurfInTmp(SurfNum) =
                                (TempTerm + construct.CTFSourceIn(0) * state.dataHeatBalSurf->QsrcHist(SurfNum, 1) +
                                 IterDampConst * state.dataHeatBalSurf->TempInsOld(SurfNum)) *
                                TempDiv; // Constant portion of conduction eq (history terms) | LW radiation from internal sources | SW radiation
                                         // from internal sources | Convection from surface to zone air | Net radiant exchange with other zone
                                         // surfaces | Heat source/sink term for radiant systems | (if there is one present) | Radiant flux from a
                                         // high temperature radiant heater | Radiant flux from a hot water baseboard heater | Radiant flux from a
                                         // steam baseboard heater | Radiant flux from an electric baseboard heater | Iterative damping term (for
                                         // stability) | Conduction term (both partition sides same temp) | Conduction term (both partition sides
                                         // same temp) | Convection and damping term | Radiation from AFN ducts
                        } else {
                            state.dataHeatBalSurf->TempSurfInTmp(SurfNum) =
                                (TempTerm + IterDampConst * state.dataHeatBalSurf->TempInsOld(SurfNum)) *
                                TempDiv; // Constant portion of conduction eq (history terms) | LW radiation from internal sources | SW radiation
                                         // from internal sources | Convection from surface to zone air | Net radiant exchange with other zone
                                         // surfaces | Heat source/sink term for radiant systems | (if there is one present) | Radiant flux from a
                                         // high temperature radiant heater | Radiant flux from a hot water baseboard heater | Radiant flux from a
                                         // steam baseboard heater | Radiant flux from an electric baseboard heater | Iterative damping term (for
                                         // stability) | Conduction term (both partition sides same temp) | Conduction term (both partition sides
                                         // same temp) | Convection and damping term | Radiation from AFN ducts
                        }
                    } else { // this is a pool and it has been simulated this time step
                        state.dataHeatBalSurf->TempSurfInTmp(SurfNum) =
                            (state.dataHeatBalSurf->CTFConstInPart(SurfNum) + state.dataHeatBalFanSys->QPoolSurfNumerator(SurfNum) +
                             IterDampConst * state.dataHeatBalSurf->TempInsOld(SurfNum)) /
                            (construct.CTFInside(0) - construct.CTFCross(0) + state.dataHeatBalFanSys->PoolHeatTransCoefs(SurfNum) +
                             IterDampConst); // Constant part of conduction eq (history terms) | Pool modified terms (see
                                             // non-pool equation for details) | Iterative damping term (for stability) |
                                             // Conduction term (both partition sides same temp) | Pool and damping term
                    }
                    if (surface.HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::EMPD) {
                        state.dataHeatBalSurf->TempSurfInTmp(SurfNum) -=
                            state.dataMstBalEMPD->HeatFluxLatent(SurfNum) * TempDiv; // Conduction term (both partition sides same temp) |
                                                                                     // Conduction term (both partition sides same temp) |
                                                                                     // Convection and damping term
                        if (TempSurfInSat > state.dataHeatBalSurf->TempSurfInTmp(SurfNum)) {
                            state.dataHeatBalSurf->TempSurfInTmp(SurfNum) = TempSurfInSat; // Surface temp cannot be below dew point
                        }
                    }
                    // if any mixed heat transfer models in zone, apply limits to CTF result
                    if (state.dataHeatBalSurf->Zone_has_mixed_HT_models[ZoneNum])
                        state.dataHeatBalSurf->TempSurfInTmp(SurfNum) =
                            max(MinSurfaceTempLimit,
                                min(state.dataHeatBalSurf->MaxSurfaceTempLimit,
                                    state.dataHeatBalSurf->TempSurfInTmp(SurfNum))); // Limit Check //Tuned Precomputed condition to eliminate loop

                    if (construct.SourceSinkPresent) { // Set the appropriate parameters for the radiant system

                        // Radiant system does not need the damping coefficient terms (hopefully) // Partitions are assumed to be symmetric
                        Real64 const RadSysDiv(1.0 / (construct.CTFInside(0) - construct.CTFCross(0) + HConvIn_surf));
                        state.dataHeatBalFanSys->RadSysToHBConstCoef(SurfNum) = state.dataHeatBalFanSys->RadSysTiHBConstCoef(SurfNum) =
                            TempTerm * RadSysDiv; // Constant portion of conduction eq (history terms) | LW radiation from internal sources | SW
                                                  // radiation from internal sources | Convection from surface to zone air | Radiant flux from
                                                  // high temperature radiant heater | Radiant flux from a hot water baseboard heater | Radiant
                                                  // flux from a steam baseboard heater | Radiant flux from an electric baseboard heater | Net
                                                  // radiant exchange with other zone surfaces | Cond term (both partition sides same temp) | Cond
                                                  // term (both partition sides same temp) | Convection and damping term
                        state.dataHeatBalFanSys->RadSysToHBTinCoef(SurfNum) = state.dataHeatBalFanSys->RadSysTiHBToutCoef(SurfNum) =
                            0.0; // The outside temp is assumed to be equal to the inside temp for a partition
                        state.dataHeatBalFanSys->RadSysToHBQsrcCoef(SurfNum) = state.dataHeatBalFanSys->RadSysTiHBQsrcCoef(SurfNum) =
                            construct.CTFSourceIn(0) * RadSysDiv; // QTF term for the source | Cond term (both partition sides same temp) | Cond
                                                                  // term (both partition sides same temp) | Convection and damping term
                    }

                } else if (surface.HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::CondFD ||
                           surface.HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::HAMT) {

                    if (surface.HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::HAMT)
                        HeatBalanceHAMTManager::ManageHeatBalHAMT(
                            state, SurfNum, state.dataHeatBalSurf->TempSurfInTmp(SurfNum), TempSurfOutTmp); // HAMT

                    if (surface.HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::CondFD) {
                        HeatBalFiniteDiffManager::ManageHeatBalFiniteDiff(
                            state, SurfNum, state.dataHeatBalSurf->TempSurfInTmp(SurfNum), TempSurfOutTmp);
                    }

                    TH11 = TempSurfOutTmp;
                }

                state.dataHeatBalSurf->TempSurfIn(SurfNum) = state.dataHeatBalSurf->TempSurfInTmp(SurfNum);

            } else { // Standard surface or interzone surface
                bool movableInsulPresent = state.dataSurface->AnyMovableInsulation && state.dataHeatBalSurf->SurfMovInsulIntPresent(SurfNum);
                if (!movableInsulPresent) { // No movable insulation present, normal heat balance equation

                    if (surface.HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::CTF ||
                        surface.HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::EMPD) { // Regular CTF Surface and/or EMPD surface

                        if (surface.HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::EMPD) {
                            MoistureBalanceEMPDManager::CalcMoistureBalanceEMPD(
                                state, SurfNum, state.dataHeatBalSurf->TempSurfInTmp(SurfNum), MAT_zone, TempSurfInSat);
                        }
                        // Pre-calculate a few terms
                        Real64 const TempTerm(
                            state.dataHeatBalSurf->CTFConstInPart(SurfNum) + state.dataHeatBal->SurfQRadThermInAbs(SurfNum) +
                            state.dataHeatBalSurf->SurfOpaqQRadSWInAbs(SurfNum) + state.dataHeatBalSurf->SurfQAdditionalHeatSourceInside(SurfNum) +
                            HConvIn_surf * state.dataHeatBalSurfMgr->RefAirTemp(SurfNum) + state.dataHeatBalFanSys->QHTRadSysSurf(SurfNum) +
                            state.dataHeatBalFanSys->QCoolingPanelSurf(SurfNum) + state.dataHeatBalFanSys->QHWBaseboardSurf(SurfNum) +
                            state.dataHeatBalFanSys->QSteamBaseboardSurf(SurfNum) + state.dataHeatBalFanSys->QElecBaseboardSurf(SurfNum) +
                            state.dataHeatBalSurf->SurfNetLWRadToSurf(SurfNum) +
                            (state.dataHeatBalFanSys->QRadSurfAFNDuct(SurfNum) / state.dataGlobal->TimeStepZoneSec));
                        Real64 const TempDiv(1.0 / (construct.CTFInside(0) + HConvIn_surf + IterDampConst));
                        // Calculate the current inside surface temperature
                        if ((!state.dataSurface->SurfIsPool(SurfNum)) ||
                            ((state.dataSurface->SurfIsPool(SurfNum)) &&
                             (std::abs(state.dataHeatBalFanSys->QPoolSurfNumerator(SurfNum)) < PoolIsOperatingLimit) &&
                             (std::abs(state.dataHeatBalFanSys->PoolHeatTransCoefs(SurfNum)) < PoolIsOperatingLimit))) {
                            if (construct.SourceSinkPresent) {
                                state.dataHeatBalSurf->TempSurfInTmp(SurfNum) =
                                    (TempTerm + construct.CTFSourceIn(0) * state.dataHeatBalSurf->QsrcHist(SurfNum, 1) +
                                     IterDampConst * state.dataHeatBalSurf->TempInsOld(SurfNum) + construct.CTFCross(0) * TH11) *
                                    TempDiv; // Constant part of conduction eq (history terms) | LW radiation from internal sources | SW
                                             // radiation from internal sources | Convection from surface to zone air | Net radiant exchange
                                             // with other zone surfaces | Heat source/sink term for radiant systems | (if there is one
                                             // present) | Radiant flux from high temp radiant heater | Radiant flux from a hot water
                                             // baseboard heater | Radiant flux from a steam baseboard heater | Radiant flux from an electric
                                             // baseboard heater | Iterative damping term (for stability) | Current conduction from | the
                                             // outside surface | Coefficient for conduction (current time) | Convection and damping term |
                                             // Radiation from AFN ducts
                            } else {
                                state.dataHeatBalSurf->TempSurfInTmp(SurfNum) =
                                    (TempTerm + IterDampConst * state.dataHeatBalSurf->TempInsOld(SurfNum) + construct.CTFCross(0) * TH11) *
                                    TempDiv; // Constant part of conduction eq (history terms) | LW radiation from internal sources | SW
                                             // radiation from internal sources | Convection from surface to zone air | Net radiant exchange
                                             // with other zone surfaces | Heat source/sink term for radiant systems | (if there is one
                                             // present) | Radiant flux from high temp radiant heater | Radiant flux from a hot water
                                             // baseboard heater | Radiant flux from a steam baseboard heater | Radiant flux from an electric
                                             // baseboard heater | Iterative damping term (for stability) | Current conduction from | the
                                             // outside surface | Coefficient for conduction (current time) | Convection and damping term |
                                             // Radiation from AFN ducts
                            }
                        } else { // surface is a pool and the pool has been simulated this time step
                            state.dataHeatBalSurf->TempSurfInTmp(SurfNum) =
                                (state.dataHeatBalSurf->CTFConstInPart(SurfNum) + state.dataHeatBalFanSys->QPoolSurfNumerator(SurfNum) +
                                 IterDampConst * state.dataHeatBalSurf->TempInsOld(SurfNum) + construct.CTFCross(0) * TH11) /
                                (construct.CTFInside(0) + state.dataHeatBalFanSys->PoolHeatTransCoefs(SurfNum) +
                                 IterDampConst); // Constant part of conduction eq (history terms) | Pool modified terms
                                                 // (see non-pool equation for details) | Iterative damping term (for
                                                 // stability) | Current conduction from | the outside surface |
                                                 // Coefficient for conduction (current time) | Pool and damping term
                        }
                        if (surface.HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::EMPD) {
                            state.dataHeatBalSurf->TempSurfInTmp(SurfNum) -=
                                state.dataMstBalEMPD->HeatFluxLatent(SurfNum) *
                                TempDiv; // Coefficient for conduction (current time) | Convection and damping term
                            if (TempSurfInSat > state.dataHeatBalSurf->TempSurfInTmp(SurfNum)) {
                                state.dataHeatBalSurf->TempSurfInTmp(SurfNum) = TempSurfInSat; // Surface temp cannot be below dew point
                            }
                        }
                        // if any mixed heat transfer models in zone, apply limits to CTF result
                        if (state.dataHeatBalSurf->Zone_has_mixed_HT_models[ZoneNum])
                            state.dataHeatBalSurf->TempSurfInTmp(SurfNum) = max(
                                MinSurfaceTempLimit,
                                min(state.dataHeatBalSurf->MaxSurfaceTempLimit,
                                    state.dataHeatBalSurf->TempSurfInTmp(SurfNum))); // Limit Check //Tuned Precomputed condition to eliminate loop

                        if (construct.SourceSinkPresent) { // Set the appropriate parameters for the radiant system

                            // Radiant system does not need the damping coefficient terms (hopefully)
                            Real64 const RadSysDiv(1.0 / (construct.CTFInside(0) + HConvIn_surf));
                            state.dataHeatBalFanSys->RadSysTiHBConstCoef(SurfNum) =
                                TempTerm * RadSysDiv; // Constant portion of cond eq (history terms) | LW radiation from internal sources | SW
                                                      // radiation from internal sources | Convection from surface to zone air | Radiant flux
                                                      // from high temp radiant heater | Radiant flux from a hot water baseboard heater |
                                                      // Radiant flux from a steam baseboard heater | Radiant flux from an electric baseboard
                                                      // heater | Net radiant exchange with other zone surfaces | Cond term (both partition
                                                      // sides same temp) | Convection and damping term
                            state.dataHeatBalFanSys->RadSysTiHBToutCoef(SurfNum) =
                                construct.CTFCross(0) * RadSysDiv; // Outside temp=inside temp for a partition |
                                                                   // Cond term (both partition sides same temp) |
                                                                   // Convection and damping term
                            state.dataHeatBalFanSys->RadSysTiHBQsrcCoef(SurfNum) =
                                construct.CTFSourceIn(0) * RadSysDiv; // QTF term for the source | Cond term (both
                                                                      // partition sides same temp) | Convection and
                                                                      // damping term

                            if (surface.ExtBoundCond > 0) { // This is an interzone partition and we need to set outside params
                                // The inside coefficients of one side are equal to the outside coefficients of the other side.  But,
                                // the inside coefficients are set up once the heat balance equation for that side has been calculated.
                                // For both sides to actually have been set, we have to wait until we get to the second side in the surface
                                // derived type.  At that point, both inside coefficient sets have been evaluated.
                                if (surface.ExtBoundCond < SurfNum) { // Both of the inside coefficients have now been set
                                    int OtherSideSurfNum = surface.ExtBoundCond;
                                    state.dataHeatBalFanSys->RadSysToHBConstCoef(OtherSideSurfNum) =
                                        state.dataHeatBalFanSys->RadSysTiHBConstCoef(SurfNum);
                                    state.dataHeatBalFanSys->RadSysToHBTinCoef(OtherSideSurfNum) =
                                        state.dataHeatBalFanSys->RadSysTiHBToutCoef(SurfNum);
                                    state.dataHeatBalFanSys->RadSysToHBQsrcCoef(OtherSideSurfNum) =
                                        state.dataHeatBalFanSys->RadSysTiHBQsrcCoef(SurfNum);
                                    state.dataHeatBalFanSys->RadSysToHBConstCoef(SurfNum) =
                                        state.dataHeatBalFanSys->RadSysTiHBConstCoef(OtherSideSurfNum);
                                    state.dataHeatBalFanSys->RadSysToHBTinCoef(SurfNum) =
                                        state.dataHeatBalFanSys->RadSysTiHBToutCoef(OtherSideSurfNum);
                                    state.dataHeatBalFanSys->RadSysToHBQsrcCoef(SurfNum) =
                                        state.dataHeatBalFanSys->RadSysTiHBQsrcCoef(OtherSideSurfNum);
                                }
                            }
                        }

                    } else if (surface.HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::CondFD ||
                               surface.HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::HAMT) {

                        if (surface.HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::HAMT) {
                            if (surface.ExtBoundCond > 0) {
                                // HAMT get the correct other side zone zone air temperature --
                                int OtherSideSurfNum = surface.ExtBoundCond;
                                // ZoneNum = surface.Zone;
                                OtherSideZoneNum = Surface(OtherSideSurfNum).Zone;
                                state.dataMstBal->TempOutsideAirFD(SurfNum) = state.dataHeatBalFanSys->MAT(OtherSideZoneNum);
                            }
                            HeatBalanceHAMTManager::ManageHeatBalHAMT(state, SurfNum, state.dataHeatBalSurf->TempSurfInTmp(SurfNum), TempSurfOutTmp);
                        }

                        if (surface.HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::CondFD)
                            HeatBalFiniteDiffManager::ManageHeatBalFiniteDiff(
                                state, SurfNum, state.dataHeatBalSurf->TempSurfInTmp(SurfNum), TempSurfOutTmp);

                        TH11 = TempSurfOutTmp;

                    } else if (surface.HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::Kiva) {
                        // Read Kiva results for each surface
                        state.dataHeatBalSurf->TempSurfInTmp(SurfNum) =
                            state.dataSurfaceGeometry->kivaManager.surfaceMap[SurfNum].results.Tconv - DataGlobalConstants::KelvinConv;
                        state.dataHeatBalSurf->SurfOpaqInsFaceConductionFlux(SurfNum) =
                            state.dataSurfaceGeometry->kivaManager.surfaceMap[SurfNum].results.qtot;
                        state.dataHeatBalSurf->SurfOpaqInsFaceConduction(SurfNum) =
                            state.dataHeatBalSurf->SurfOpaqInsFaceConductionFlux(SurfNum) * Surface(SurfNum).Area;

                        TH11 = 0.0;
                    }

                    state.dataHeatBalSurf->TempSurfIn(SurfNum) = state.dataHeatBalSurf->TempSurfInTmp(SurfNum);

                } else { // Movable insulation present
                    Real64 HMovInsul = state.dataHeatBalSurf->SurfMovInsulHInt(SurfNum);
                    if (construct.SourceSinkPresent) {

                        ShowSevereError(state, "Interior movable insulation is not valid with embedded sources/sinks");
                        ShowContinueError(state, "Construction " + construct.Name + " contains an internal source or sink but also uses");
                        ShowContinueError(state,
                                          "interior movable insulation " +
                                              state.dataMaterial->Material(state.dataSurface->SurfMaterialMovInsulInt(SurfNum)).Name +
                                              " for a surface with that construction.");
                        ShowContinueError(state,
                                          "This is not currently allowed because the heat balance equations do not currently accommodate "
                                          "this combination.");
                        ShowFatalError(state, "CalcHeatBalanceInsideSurf: Program terminates due to preceding conditions.");
                    }

                    Real64 F1 = HMovInsul / (HMovInsul + HConvIn_surf + IterDampConst);

                    state.dataHeatBalSurf->TempSurfIn(SurfNum) =
                        (state.dataHeatBalSurf->CTFConstInPart(SurfNum) + state.dataHeatBalSurf->SurfOpaqQRadSWInAbs(SurfNum) +
                         construct.CTFCross(0) * TH11 +
                         F1 * (state.dataHeatBal->SurfQRadThermInAbs(SurfNum) + HConvIn_surf * state.dataHeatBalSurfMgr->RefAirTemp(SurfNum) +
                               state.dataHeatBalSurf->SurfNetLWRadToSurf(SurfNum) + state.dataHeatBalFanSys->QHTRadSysSurf(SurfNum) +
                               state.dataHeatBalFanSys->QCoolingPanelSurf(SurfNum) + state.dataHeatBalFanSys->QHWBaseboardSurf(SurfNum) +
                               state.dataHeatBalFanSys->QSteamBaseboardSurf(SurfNum) + state.dataHeatBalFanSys->QElecBaseboardSurf(SurfNum) +
                               state.dataHeatBalSurf->SurfQAdditionalHeatSourceInside(SurfNum) +
                               IterDampConst * state.dataHeatBalSurf->TempInsOld(SurfNum))) /
                        (construct.CTFInside(0) + HMovInsul - F1 * HMovInsul); // Convection from surface to zone air

                    state.dataHeatBalSurf->TempSurfInTmp(SurfNum) =
                        (construct.CTFInside(0) * state.dataHeatBalSurf->TempSurfIn(SurfNum) +
                         HMovInsul * state.dataHeatBalSurf->TempSurfIn(SurfNum) - state.dataHeatBalSurf->SurfOpaqQRadSWInAbs(SurfNum) -
                         state.dataHeatBalSurf->CTFConstInPart(SurfNum) - construct.CTFCross(0) * TH11) /
                        (HMovInsul);
                    // if any mixed heat transfer models in zone, apply limits to CTF result
                    if (state.dataHeatBalSurf->Zone_has_mixed_HT_models[ZoneNum])
                        state.dataHeatBalSurf->TempSurfInTmp(SurfNum) =
                            max(MinSurfaceTempLimit,
                                min(state.dataHeatBalSurf->MaxSurfaceTempLimit,
                                    state.dataHeatBalSurf->TempSurfInTmp(SurfNum))); // Limit Check //Tuned Precomputed condition to eliminate loop
                }
            }
        }
        for (int SurfNum : HTWindowSurfs) {
            auto &surface(Surface(SurfNum));
            Real64 &TH11(state.dataHeatBalSurf->TH(1, 1, SurfNum));
            int const ConstrNum = state.dataSurface->SurfActiveConstruction(SurfNum); // Not const, because storm window may change this
            auto const &construct(state.dataConstruction->Construct(ConstrNum));
            if (state.dataSurface->SurfWinOriginalClass(SurfNum) == SurfaceClass::TDD_Diffuser) { // Tubular daylighting device
                // Lookup up the TDD:DOME object
                int const pipeNum = state.dataSurface->SurfWinTDDPipeNum(SurfNum);
                int const domeNum = state.dataDaylightingDevicesData->TDDPipe(pipeNum).Dome;
                // Ueff = 1 / effective R value between TDD:DOME and TDD:DIFFUSER
                Real64 Ueff = 1.0 / state.dataDaylightingDevicesData->TDDPipe(pipeNum).Reff;

                // Similar to opaque surface but outside surface temp of TDD:DOME is used, and no embedded sources/sinks.
                // Absorbed shortwave radiation is treated similar to a regular window, but only 1 glass layer is allowed.
                //   = QRadSWwinAbs(SurfNum,1)/2.0
                Real64 const HConvIn_surf(state.dataMstBal->HConvInFD(SurfNum) = state.dataHeatBal->HConvIn(SurfNum));
                state.dataHeatBalSurf->TempSurfIn(SurfNum) = state.dataHeatBalSurf->TempSurfInTmp(SurfNum) =
                    (state.dataHeatBal->SurfQRadThermInAbs(SurfNum) + state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum, 1) / 2.0 +
                     state.dataHeatBalSurf->SurfQAdditionalHeatSourceInside(SurfNum) + HConvIn_surf * state.dataHeatBalSurfMgr->RefAirTemp(SurfNum) +
                     state.dataHeatBalSurf->SurfNetLWRadToSurf(SurfNum) + IterDampConst * state.dataHeatBalSurf->TempInsOld(SurfNum) +
                     Ueff * state.dataHeatBalSurf->TH(1, 1, domeNum)) /
                    (Ueff + HConvIn_surf + IterDampConst); // LW radiation from internal sources | SW radiation from internal sources and
                                                           // solar | Convection from surface to zone air | Net radiant exchange with
                                                           // other zone surfaces | Iterative damping term (for stability) | Current
                                                           // conduction from the outside surface | Coefficient for conduction (current
                                                           // time) | Convection and damping term

                Real64 const Sigma_Temp_4(DataGlobalConstants::StefanBoltzmann * pow_4(state.dataHeatBalSurf->TempSurfIn(SurfNum)));

                // Calculate window heat gain for TDD:DIFFUSER since this calculation is usually done in WindowManager
                state.dataSurface->SurfWinHeatGain(SurfNum) =
                    state.dataSurface->SurfWinTransSolar(SurfNum) +
                    HConvIn_surf * surface.Area * (state.dataHeatBalSurf->TempSurfIn(SurfNum) - state.dataHeatBalSurfMgr->RefAirTemp(SurfNum)) +
                    state.dataConstruction->Construct(surface.Construction).InsideAbsorpThermal * surface.Area *
                        (Sigma_Temp_4 -
                         (state.dataSurface->SurfWinIRfromParentZone(SurfNum) + state.dataHeatBalFanSys->QHTRadSysSurf(SurfNum) +
                          state.dataHeatBalFanSys->QCoolingPanelSurf(SurfNum) + state.dataHeatBalFanSys->QHWBaseboardSurf(SurfNum) +
                          state.dataHeatBalFanSys->QSteamBaseboardSurf(SurfNum) + state.dataHeatBalFanSys->QElecBaseboardSurf(SurfNum))) -
                    state.dataHeatBal->EnclSolQSWRad(surface.SolarEnclIndex) * surface.Area *
                        state.dataConstruction->Construct(surface.Construction).TransDiff;
                // Transmitted solar | Convection | IR exchange | IR
                // Zone diffuse interior shortwave reflected back into the TDD
                state.dataSurface->SurfWinHeatTransfer(SurfNum) = state.dataSurface->SurfWinHeatGain(SurfNum);

                // fill out report vars for components of Window Heat Gain
                state.dataSurface->SurfWinGainConvGlazToZoneRep(SurfNum) =
                    HConvIn_surf * surface.Area * (state.dataHeatBalSurf->TempSurfIn(SurfNum) - state.dataHeatBalSurfMgr->RefAirTemp(SurfNum));
                state.dataSurface->SurfWinGainIRGlazToZoneRep(SurfNum) =
                    state.dataConstruction->Construct(surface.Construction).InsideAbsorpThermal * surface.Area *
                    (Sigma_Temp_4 - (state.dataSurface->SurfWinIRfromParentZone(SurfNum) + state.dataHeatBalFanSys->QHTRadSysSurf(SurfNum) +
                                     state.dataHeatBalFanSys->QCoolingPanelSurf(SurfNum) + state.dataHeatBalFanSys->QHWBaseboardSurf(SurfNum) +
                                     state.dataHeatBalFanSys->QSteamBaseboardSurf(SurfNum) + state.dataHeatBalFanSys->QElecBaseboardSurf(SurfNum)));
                state.dataSurface->SurfWinLossSWZoneToOutWinRep(SurfNum) = state.dataHeatBal->EnclSolQSWRad(surface.SolarEnclIndex) * surface.Area *
                                                                           state.dataConstruction->Construct(surface.Construction).TransDiff;
            } else {                                                // Regular window
                if (state.dataHeatBal->InsideSurfIterations == 0) { // Do windows only once
                    // Get outside convection coeff for exterior window here to avoid calling
                    // InitExteriorConvectionCoeff from CalcWindowHeatBalance, which avoids circular reference
                    // (HeatBalanceSurfaceManager USEing and WindowManager and
                    // WindowManager USEing HeatBalanceSurfaceManager)
                    if (surface.ExtBoundCond == ExternalEnvironment) {
                        int RoughSurf = state.dataMaterial->Material(construct.LayerPoint(1)).Roughness;           // Outside surface roughness
                        Real64 EmisOut = state.dataMaterial->Material(construct.LayerPoint(1)).AbsorpThermalFront; // Glass outside surface emissivity
                        auto const shading_flag(state.dataSurface->SurfWinShadingFlag(SurfNum));
                        if (ANY_EXTERIOR_SHADE_BLIND_SCREEN(shading_flag)) {
                            // Exterior shade in place
                            int const ConstrNumSh = Surface(SurfNum).activeShadedConstruction;
                            if (ConstrNumSh != 0) {
                                RoughSurf = state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNumSh).LayerPoint(1)).Roughness;
                                EmisOut = state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNumSh).LayerPoint(1)).AbsorpThermal;
                            }
                        }

                        // Get the outside effective emissivity for Equivalent layer model
                        if (construct.WindowTypeEQL) {
                            EmisOut = WindowEquivalentLayer::EQLWindowOutsideEffectiveEmiss(state, ConstrNum);
                        }
                        // Set Exterior Convection Coefficient...
                        if (state.dataSurface->SurfExtConvCoeffIndex(SurfNum) > 0) {

                            state.dataHeatBalSurf->HcExtSurf(SurfNum) = ConvectionCoefficients::SetExtConvectionCoeff(state, SurfNum);

                        } else if (surface.ExtWind) { // Window is exposed to wind (and possibly rain)

                            // Calculate exterior heat transfer coefficients with windspeed (windspeed is calculated internally in
                            // subroutine)
                            ConvectionCoefficients::InitExteriorConvectionCoeff(state,
                                                                                SurfNum,
                                                                                0.0,
                                                                                RoughSurf,
                                                                                EmisOut,
                                                                                TH11,
                                                                                state.dataHeatBalSurf->HcExtSurf(SurfNum),
                                                                                state.dataHeatBalSurf->HSkyExtSurf(SurfNum),
                                                                                state.dataHeatBalSurf->HGrdExtSurf(SurfNum),
                                                                                state.dataHeatBalSurf->HAirExtSurf(SurfNum));

                            if (state.dataEnvrn->IsRain) {                          // Raining: since wind exposed, outside window surface gets wet
                                state.dataHeatBalSurf->HcExtSurf(SurfNum) = 1000.0; // Reset HcExtSurf because of wetness
                            }

                        } else { // Not Wind exposed

                            // Calculate exterior heat transfer coefficients for windspeed = 0
                            ConvectionCoefficients::InitExteriorConvectionCoeff(state,
                                                                                SurfNum,
                                                                                0.0,
                                                                                RoughSurf,
                                                                                EmisOut,
                                                                                TH11,
                                                                                state.dataHeatBalSurf->HcExtSurf(SurfNum),
                                                                                state.dataHeatBalSurf->HSkyExtSurf(SurfNum),
                                                                                state.dataHeatBalSurf->HGrdExtSurf(SurfNum),
                                                                                state.dataHeatBalSurf->HAirExtSurf(SurfNum));
                        }
                    } else { // Interior Surface

                        if (state.dataSurface->SurfExtConvCoeffIndex(SurfNum) > 0) {
                            state.dataHeatBalSurf->HcExtSurf(SurfNum) = ConvectionCoefficients::SetExtConvectionCoeff(state, SurfNum);
                        } else {
                            // Exterior Convection Coefficient for the Interior or Interzone Window is the Interior Convection Coeff of
                            // same
                            state.dataHeatBalSurf->HcExtSurf(SurfNum) = state.dataHeatBal->HConvIn(surface.ExtBoundCond);
                        }
                    }

                    // Following call determines inside surface temperature of glazing, and of
                    // frame and/or divider, if present
                    CalcWindowHeatBalance(
                        state, SurfNum, state.dataHeatBalSurf->HcExtSurf(SurfNum), state.dataHeatBalSurf->TempSurfInTmp(SurfNum), TH11);

                    state.dataHeatBalSurf->TempSurfIn(SurfNum) = state.dataHeatBalSurf->TempSurfInTmp(SurfNum);
                }
            }
        } // ...end of inside surface heat balance equation selection

        for (int SurfNum : HTSurfs) {
            int const ZoneNum = Surface(SurfNum).Zone;
            auto &zone(state.dataHeatBal->Zone(ZoneNum));
            Real64 &TH11(state.dataHeatBalSurf->TH(1, 1, SurfNum));
            Real64 &TH12(state.dataHeatBalSurf->TH(2, 1, SurfNum));
            TH12 = state.dataHeatBalSurf->TempSurfInRep(SurfNum) = state.dataHeatBalSurf->TempSurfIn(SurfNum);
            state.dataHeatBalSurf->SurfTempOut(SurfNum) = TH11; // For reporting
            if (state.dataSurface->SurfWinOriginalClass(SurfNum) == SurfaceClass::TDD_Dome) continue;
            if (state.dataSurface->SurfWinOriginalClass(SurfNum) == SurfaceClass::TDD_Diffuser) { // Tubular daylighting device
                // Tubular daylighting devices are treated as one big object with an effective R value.
                // The outside face temperature of the TDD:DOME and the inside face temperature of the
                // TDD:DIFFUSER are calculated with the outside and inside heat balances respectively.
                // Below, the resulting temperatures are copied to the inside face of the TDD:DOME
                // and the outside face of the TDD:DIFFUSER for reporting.

                // Set inside temp variables of TDD:DOME equal to inside temp of TDD:DIFFUSER
                int domeNum = state.dataDaylightingDevicesData->TDDPipe(state.dataSurface->SurfWinTDDPipeNum(SurfNum)).Dome;
                state.dataHeatBalSurf->TH(2, 1, domeNum) = state.dataHeatBalSurf->TempSurfIn(domeNum) =
                    state.dataHeatBalSurf->TempSurfInTmp(domeNum) = state.dataHeatBalSurf->TempSurfInRep(domeNum) =
                        state.dataHeatBalSurf->TempSurfIn(SurfNum);

                // Set outside temp reporting variable of TDD:DOME (since it gets skipped otherwise)
                // Reset outside temp variables of TDD:DIFFUSER equal to outside temp of TDD:DOME
                TH11 = state.dataHeatBalSurf->SurfTempOut(SurfNum) = state.dataHeatBalSurf->SurfTempOut(domeNum) =
                    state.dataHeatBalSurf->TH(1, 1, domeNum);
            }

            if ((TH12 > state.dataHeatBalSurf->MaxSurfaceTempLimit) || (TH12 < MinSurfaceTempLimit)) {
                TestSurfTempCalcHeatBalanceInsideSurf(state, TH12, SurfNum, zone, state.dataHeatBalSurfMgr->calcHeatBalInsideSurfWarmupErrCount);
            }

        } // ...end of main loops over all surfaces for inside heat balances

        // Interzone surface updating: interzone surfaces have other side temperatures
        // which can vary as the simulation iterates through the inside heat
        // balance.  This block is intended to "lock" the opposite side (outside)
        // temperatures to the correct value, namely the value calculated by the
        // inside surface heat balance for the other side.
        assert(state.dataHeatBalSurf->TH.index(1, 1, 1) == 0u); // Assumed for linear indexing below
        auto const l211(state.dataHeatBalSurf->TH.index(2, 1, 1) - 1);
        for (int SurfNum : IZSurfs) {
            int const surfExtBoundCond(Surface(SurfNum).ExtBoundCond);
            // Set the outside surface temperature to the inside surface temperature of the interzone pair.
            // By going through all of the surfaces, this should pick up the other side as well as affect the next iteration.
            // [ SurfNum - 1 ] == ( 1, 1, SurfNum )
            // [ l211 + surfExtBoundCond ] == ( 2, 1, surfExtBoundCond )
            state.dataHeatBalSurf->SurfTempOut(SurfNum) = state.dataHeatBalSurf->TH[SurfNum - 1] = state.dataHeatBalSurf->TH[l211 + surfExtBoundCond];
        }

        ++state.dataHeatBal->InsideSurfIterations;

        // Convergence check - Loop through all relevant non-window surfaces to check for convergence...
        Real64 MaxDelTemp = 0.0; // Maximum change in surface temperature for any opaque surface from one iteration to the next
        for (int SurfNum : HTNonWindowSurfs) {
            MaxDelTemp = max(std::abs(state.dataHeatBalSurf->TempSurfIn(SurfNum) - state.dataHeatBalSurf->TempInsOld(SurfNum)), MaxDelTemp);
            if (Surface(SurfNum).HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::CondFD) {
                // also check all internal nodes as well as surface faces
                MaxDelTemp = max(MaxDelTemp, state.dataHeatBalFiniteDiffMgr->SurfaceFD(SurfNum).MaxNodeDelTemp);
            }
        } // ...end of loop to check for convergence

        if (!state.dataHeatBal->AnyCondFD) {
            if (MaxDelTemp <= state.dataHeatBal->MaxAllowedDelTemp) Converged = true;
        } else {
            if (MaxDelTemp <= state.dataHeatBal->MaxAllowedDelTempCondFD) Converged = true;

            // resets relaxation factor to speed up iterations when under-relaxation is not needed.
            if (state.dataHeatBal->InsideSurfIterations <= 1) {
                state.dataHeatBal->CondFDRelaxFactor = state.dataHeatBal->CondFDRelaxFactorInput;
            }
            if ((state.dataHeatBal->InsideSurfIterations > IterationsForCondFDRelaxChange) && !Converged) {
                // adjust relaxation factor down, assume large number of iterations is result of instability
                state.dataHeatBal->CondFDRelaxFactor *= 0.9;
                if (state.dataHeatBal->CondFDRelaxFactor < 0.1) state.dataHeatBal->CondFDRelaxFactor = 0.1;
            }
        }

#ifdef EP_Count_Calls
        state.dataTimingsData->NumMaxInsideSurfIterations =
            max(state.dataTimingsData->NumMaxInsideSurfIterations, state.dataHeatBal->InsideSurfIterations);
#endif

        if (state.dataHeatBal->InsideSurfIterations < state.dataHeatBalSurf->MinIterations) Converged = false;

        if (state.dataHeatBal->InsideSurfIterations > MaxIterations) {
            if (!state.dataGlobal->WarmupFlag) {
                ++state.dataHeatBalSurfMgr->calcHeatBalInsideSurfErrCount;
                if (state.dataHeatBalSurfMgr->calcHeatBalInsideSurfErrCount < 16) {
                    if (!state.dataHeatBal->AnyCondFD) {
                        ShowWarningError(state,
                                         format("Inside surface heat balance did not converge with Max Temp Difference [C] ={:.3R} vs Max "
                                                "Allowed Temp Diff [C] ={:.3R}",
                                                MaxDelTemp,
                                                state.dataHeatBal->MaxAllowedDelTemp));
                        ShowContinueErrorTimeStamp(state, "");
                    } else {
                        ShowWarningError(state,
                                         format("Inside surface heat balance did not converge with Max Temp Difference [C] ={:.3R} vs Max "
                                                "Allowed Temp Diff [C] ={:.6R}",
                                                MaxDelTemp,
                                                state.dataHeatBal->MaxAllowedDelTempCondFD));
                        ShowContinueErrorTimeStamp(state, "");
                    }
                } else {
                    ShowRecurringWarningErrorAtEnd(state,
                                                   "Inside surface heat balance convergence problem continues",
                                                   state.dataHeatBalSurfMgr->calcHeatBalInsideSurfErrPointer,
                                                   MaxDelTemp,
                                                   MaxDelTemp,
                                                   _,
                                                   "[C]",
                                                   "[C]");
                }
            }
            break; // iteration loop
        }

    } // ...end of main inside heat balance DO loop (ends when Converged)

    // Set various surface output variables and other record keeping - after iterations are complete
    for (int surfNum : HTSurfs) {
        if (Surface(surfNum).Class == SurfaceClass::TDD_Dome) continue; // Skip TDD:DOME objects.  Inside temp is handled by TDD:DIFFUSER.

        // Inside Face Convection - sign convention is positive means energy going into inside face from the air.
        auto const HConvInTemp_fac(-state.dataHeatBal->HConvIn(surfNum) *
                                   (state.dataHeatBalSurf->TempSurfIn(surfNum) - state.dataHeatBalSurfMgr->RefAirTemp(surfNum)));
        state.dataHeatBalSurf->QdotConvInRep(surfNum) = Surface(surfNum).Area * HConvInTemp_fac;
        state.dataHeatBalSurf->QdotConvInRepPerArea(surfNum) = HConvInTemp_fac;
        state.dataHeatBalSurf->QConvInReport(surfNum) = state.dataHeatBalSurf->QdotConvInRep(surfNum) * state.dataGlobal->TimeStepZoneSec;

        // The QdotConvInRep which is called "Surface Inside Face Convection Heat Gain" is stored during
        // sizing for both the normal and pulse cases so that load components can be derived later.
        if (state.dataGlobal->ZoneSizingCalc && state.dataGlobal->CompLoadReportIsReq) {
            if (!state.dataGlobal->WarmupFlag) {
                int TimeStepInDay = (state.dataGlobal->HourOfDay - 1) * state.dataGlobal->NumOfTimeStepInHour + state.dataGlobal->TimeStep;
                if (state.dataGlobal->isPulseZoneSizing) {
                    state.dataOutRptTab->loadConvectedWithPulse(state.dataSize->CurOverallSimDay, TimeStepInDay, surfNum) =
                        state.dataHeatBalSurf->QdotConvInRep(surfNum);
                } else {
                    state.dataOutRptTab->loadConvectedNormal(state.dataSize->CurOverallSimDay, TimeStepInDay, surfNum) =
                        state.dataHeatBalSurf->QdotConvInRep(surfNum);
                    state.dataOutRptTab->netSurfRadSeq(state.dataSize->CurOverallSimDay, TimeStepInDay, surfNum) =
                        state.dataHeatBalSurf->SurfNetLWRadToSurf(surfNum) * Surface(surfNum).Area;
                }
            }
        }

        // Window heat gain/loss
        if (Surface(surfNum).Class == DataSurfaces::SurfaceClass::Window) {
            if (state.dataSurface->SurfWinHeatGain(surfNum) >= 0.0) {
                state.dataSurface->SurfWinHeatGainRep(surfNum) = state.dataSurface->SurfWinHeatGain(surfNum);
                state.dataSurface->SurfWinHeatGainRepEnergy(surfNum) =
                    state.dataSurface->SurfWinHeatGainRep(surfNum) * state.dataGlobal->TimeStepZoneSec;
            } else {
                state.dataSurface->SurfWinHeatLossRep(surfNum) = -state.dataSurface->SurfWinHeatGain(surfNum);
                state.dataSurface->SurfWinHeatLossRepEnergy(surfNum) =
                    state.dataSurface->SurfWinHeatLossRep(surfNum) * state.dataGlobal->TimeStepZoneSec;
            }
            state.dataSurface->SurfWinHeatTransferRepEnergy(surfNum) =
                state.dataSurface->SurfWinHeatGain(surfNum) * state.dataGlobal->TimeStepZoneSec;
            if (state.dataSurface->SurfWinOriginalClass(surfNum) == DataSurfaces::SurfaceClass::TDD_Diffuser) { // Tubular daylighting device
                int pipeNum = state.dataSurface->SurfWinTDDPipeNum(surfNum);
                state.dataDaylightingDevicesData->TDDPipe(pipeNum).HeatGain = state.dataSurface->SurfWinHeatGainRep(surfNum);
                state.dataDaylightingDevicesData->TDDPipe(pipeNum).HeatLoss = state.dataSurface->SurfWinHeatLossRep(surfNum);
            }
            if (Surface(surfNum).ExtSolar) { // WindowManager's definition of ZoneWinHeatGain/Loss
                int zoneNum = Surface(surfNum).Zone;
                state.dataHeatBal->ZoneWinHeatGain(zoneNum) += state.dataSurface->SurfWinHeatGain(surfNum);
            }
        }
    }

    // Update SumHmXXXX for non-window EMPD or HAMT surfaces
    if (state.dataHeatBal->AnyEMPD || state.dataHeatBal->AnyHAMT) {
        for (int SurfNum : HTNonWindowSurfs) {
            auto const &surface(Surface(SurfNum));
            int ZoneNum = surface.Zone;

            if (surface.HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::HAMT) {
                HeatBalanceHAMTManager::UpdateHeatBalHAMT(state, SurfNum);

                Real64 const FD_Area_fac(state.dataMstBal->HMassConvInFD(SurfNum) * surface.Area);

                state.dataHeatBalFanSys->SumHmAW(ZoneNum) +=
                    FD_Area_fac * (state.dataMstBal->RhoVaporSurfIn(SurfNum) - state.dataMstBal->RhoVaporAirIn(SurfNum));

                Real64 const MAT_zone(state.dataHeatBalFanSys->MAT(surface.Zone));
                RhoAirZone = Psychrometrics::PsyRhoAirFnPbTdbW(
                    state,
                    state.dataEnvrn->OutBaroPress,
                    MAT_zone,
                    Psychrometrics::PsyWFnTdbRhPb(
                        state,
                        MAT_zone,
                        Psychrometrics::PsyRhFnTdbRhov(state, MAT_zone, state.dataMstBal->RhoVaporAirIn(SurfNum), rhoAirZone),
                        state.dataEnvrn->OutBaroPress));

                Real64 const surfInTemp(state.dataHeatBalSurf->TempSurfInTmp(SurfNum));
                Wsurf =
                    Psychrometrics::PsyWFnTdbRhPb(state,
                                                  surfInTemp,
                                                  Psychrometrics::PsyRhFnTdbRhov(state, surfInTemp, state.dataMstBal->RhoVaporSurfIn(SurfNum), wsurf),
                                                  state.dataEnvrn->OutBaroPress);

                state.dataHeatBalFanSys->SumHmARa(ZoneNum) += FD_Area_fac * RhoAirZone;

                state.dataHeatBalFanSys->SumHmARaW(ZoneNum) +=
                    FD_Area_fac * state.dataMstBal->RhoVaporSurfIn(SurfNum); // old eq'n: FD_Area_fac * RhoAirZone * Wsurf;

            } else if (surface.HeatTransferAlgorithm == DataSurfaces::iHeatTransferModel::EMPD) {
                // need to calculate the amount of moisture that is entering or
                // leaving the zone  Qm [kg/sec] = hmi * Area * (Del Rhov)
                // {Hmi [m/sec];     Area [m2];    Rhov [kg moist/m3]  }
                // Positive values are into the zone and negative values are
                // leaving the zone.  SumHmAw is the sum of the moisture entering or
                // leaving the zone from all of the surfaces and is a rate.  Multiply
                // by time to get the actual amount affecting the zone volume of air.

                MoistureBalanceEMPDManager::UpdateMoistureBalanceEMPD(state, SurfNum);
                state.dataMstBal->RhoVaporSurfIn(SurfNum) = state.dataMstBalEMPD->RVSurface(SurfNum);
                Real64 const FD_Area_fac(state.dataMstBal->HMassConvInFD(SurfNum) * surface.Area);
                state.dataHeatBalFanSys->SumHmAW(ZoneNum) +=
                    FD_Area_fac * (state.dataMstBal->RhoVaporSurfIn(SurfNum) - state.dataMstBal->RhoVaporAirIn(SurfNum));
                Real64 const MAT_zone(state.dataHeatBalFanSys->MAT(ZoneNum));
                state.dataHeatBalFanSys->SumHmARa(ZoneNum) +=
                    FD_Area_fac *
                    Psychrometrics::PsyRhoAirFnPbTdbW(
                        state,
                        state.dataEnvrn->OutBaroPress,
                        MAT_zone,
                        Psychrometrics::PsyWFnTdbRhPb(state,
                                                      MAT_zone,
                                                      Psychrometrics::PsyRhFnTdbRhovLBnd0C(state, MAT_zone, state.dataMstBal->RhoVaporAirIn(SurfNum)),
                                                      state.dataEnvrn->OutBaroPress)); // surfInTemp, PsyWFnTdbRhPb( surfInTemp, PsyRhFnTdbRhovLBnd0C(
                                                                                       // surfInTemp, RhoVaporAirIn( SurfNum ) ), OutBaroPress ) );
                state.dataHeatBalFanSys->SumHmARaW(ZoneNum) += FD_Area_fac * state.dataMstBal->RhoVaporSurfIn(SurfNum);
            }
        }
    }

    if (state.dataSurface->AnyMovableInsulation) ReportIntMovInsInsideSurfTemp(state);

    CalculateZoneMRT(state, ZoneToResimulate); // Update here so that the proper value of MRT is available to radiant systems
}

void CalcHeatBalanceInsideSurf2CTFOnly(EnergyPlusData &state,
                                       const int FirstZone,             // First zone to simulate
                                       const int LastZone,              // Last zone to simulate
                                       const std::vector<int> &IZSurfs, // Last zone to simulate
                                       Optional_int_const ZoneToResimulate)
{

    // This function performs a heat balance on the inside face of each
    // surface in the building. It is a copy of CalcHeatBalanceInsideSurf,
    // simplified for CTF surfaces only.

    // REFERENCES:
    // (I)BLAST legacy routine HBSRF
    auto &Surface(state.dataSurface->Surface);

    constexpr const char *Inside("Inside");

    if (state.dataHeatBalSurfMgr->calcHeatBalInsideSurfCTFOnlyFirstTime) {
        // Set up coefficient arrays that never change - loop over non-window HT surfaces
        for (int zoneNum = FirstZone; zoneNum <= LastZone; ++zoneNum) {
            int const firstSurf = state.dataHeatBal->Zone(zoneNum).OpaqOrIntMassSurfaceFirst;
            int const lastSurf = state.dataHeatBal->Zone(zoneNum).OpaqOrIntMassSurfaceLast;
            for (int surfNum = firstSurf; surfNum <= lastSurf; ++surfNum) {
                int const ConstrNum = Surface(surfNum).Construction;
                auto const &construct(state.dataConstruction->Construct(ConstrNum));
                if (Surface(surfNum).ExtBoundCond == surfNum) {
                    state.dataHeatBalSurf->IsAdiabatic(surfNum) = 1;
                    state.dataHeatBalSurf->IsNotAdiabatic(surfNum) = 0;
                } else {
                    state.dataHeatBalSurf->IsAdiabatic(surfNum) = 0;
                    state.dataHeatBalSurf->IsNotAdiabatic(surfNum) = 1;
                }
                if (construct.SourceSinkPresent) {
                    state.dataHeatBalSurf->IsSource(surfNum) = 1;
                    state.dataHeatBalSurf->IsNotSource(surfNum) = 0;
                } else {
                    state.dataHeatBalSurf->QsrcHistSurf1(surfNum) = 0.0;
                    state.dataHeatBalSurf->IsSource(surfNum) = 0;
                    state.dataHeatBalSurf->IsNotSource(surfNum) = 1;
                }
                if (!state.dataSurface->SurfIsPool(surfNum)) {
                    state.dataHeatBalSurf->IsPoolSurf(surfNum) = 0;
                    state.dataHeatBalSurf->IsNotPoolSurf(surfNum) = 1;
                }
            }
        }

        state.dataHeatBalSurfMgr->calcHeatBalInsideSurfCTFOnlyFirstTime = false;
    }

    for (int zoneNum = FirstZone; zoneNum <= LastZone; ++zoneNum) {
        // loop over all heat transfer surface except TDD Dome.
        int const firstSurf = state.dataHeatBal->Zone(zoneNum).OpaqOrWinSurfaceFirst;
        int const lastSurf = state.dataHeatBal->Zone(zoneNum).OpaqOrWinSurfaceLast;
        // determine reference air temperatures and other variable terms - loop over all surfaces
        for (int surfNum = firstSurf; surfNum <= lastSurf; ++surfNum) {

            int const ConstrNum = Surface(surfNum).Construction;
            auto const &construct(state.dataConstruction->Construct(ConstrNum));
            state.dataHeatBalSurf->CTFCross0(surfNum) = construct.CTFCross(0);
            state.dataHeatBalSurf->CTFInside0(surfNum) = construct.CTFInside(0);
            state.dataHeatBalSurf->CTFSourceIn0(surfNum) = construct.CTFSourceIn(0);
            state.dataHeatBalSurf->TH11Surf(surfNum) = state.dataHeatBalSurf->TH(1, 1, surfNum);
            if (construct.SourceSinkPresent) {
                state.dataHeatBalSurf->QsrcHistSurf1(surfNum) = state.dataHeatBalSurf->QsrcHist(surfNum, 1);
            }

            // The special heat balance terms for pools are used only when the pool is operating, so IsPool can change
            if (state.dataSurface->SurfIsPool(surfNum)) {
                if ((std::abs(state.dataHeatBalFanSys->QPoolSurfNumerator(surfNum)) >= PoolIsOperatingLimit) ||
                    (std::abs(state.dataHeatBalFanSys->PoolHeatTransCoefs(surfNum)) >= PoolIsOperatingLimit)) {
                    state.dataHeatBalSurf->IsPoolSurf(surfNum) = 1;
                    state.dataHeatBalSurf->IsNotPoolSurf(surfNum) = 0;
                } else {
                    state.dataHeatBalSurf->IsPoolSurf(surfNum) = 0;
                    state.dataHeatBalSurf->IsNotPoolSurf(surfNum) = 1;
                }
            }
            auto const SELECT_CASE_var(state.dataSurface->SurfTAirRef(surfNum));
            if (SELECT_CASE_var == ZoneMeanAirTemp) {
                state.dataHeatBalSurfMgr->RefAirTemp(surfNum) = state.dataHeatBalFanSys->MAT(zoneNum);
                state.dataHeatBal->SurfTempEffBulkAir(surfNum) = state.dataHeatBalFanSys->MAT(zoneNum); // for reporting surf adjacent air temp
            } else if (SELECT_CASE_var == AdjacentAirTemp) {
                state.dataHeatBalSurfMgr->RefAirTemp(surfNum) = state.dataHeatBal->SurfTempEffBulkAir(surfNum);
            } else if (SELECT_CASE_var == ZoneSupplyAirTemp) {
                // determine ZoneEquipConfigNum for this zone
                int ZoneEquipConfigNum = zoneNum;
                // check whether this zone is a controlled zone or not
                if (!state.dataHeatBal->Zone(zoneNum).IsControlled) {
                    ShowFatalError(state,
                                   "Zones must be controlled for Ceiling-Diffuser Convection model. No system serves zone " +
                                       state.dataHeatBal->Zone(zoneNum).Name);
                    return;
                }
                // determine supply air conditions
                Real64 SumSysMCp = 0.0;
                Real64 SumSysMCpT = 0.0;
                Real64 const CpAir = Psychrometrics::PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(zoneNum));
                for (int NodeNum = 1; NodeNum <= state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).NumInletNodes; ++NodeNum) {
                    Real64 NodeTemp = state.dataLoopNodes->Node(state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).Temp;
                    Real64 MassFlowRate =
                        state.dataLoopNodes->Node(state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).MassFlowRate;
                    SumSysMCp += MassFlowRate * CpAir;
                    SumSysMCpT += MassFlowRate * CpAir * NodeTemp;
                }
                // a weighted average of the inlet temperatures.
                if (SumSysMCp > 0.0) {                                                      // protect div by zero
                    state.dataHeatBalSurfMgr->RefAirTemp(surfNum) = SumSysMCpT / SumSysMCp; // BG changed 02-16-2005 to add index (SurfNum)
                } else {
                    state.dataHeatBalSurfMgr->RefAirTemp(surfNum) = state.dataHeatBalFanSys->MAT(zoneNum);
                }
                state.dataHeatBal->SurfTempEffBulkAir(surfNum) =
                    state.dataHeatBalSurfMgr->RefAirTemp(surfNum); // for reporting surf adjacent air temp
            } else {
                // currently set to mean air temp but should add error warning here
                state.dataHeatBalSurfMgr->RefAirTemp(surfNum) = state.dataHeatBalFanSys->MAT(zoneNum);
                state.dataHeatBal->SurfTempEffBulkAir(surfNum) = state.dataHeatBalFanSys->MAT(zoneNum); // for reporting surf adjacent air temp
            }
        }

        // Following variables must be reset due to possible recall of this routine by radiant and Resimulate routines.
        // CalcWindowHeatBalance is called, then, multiple times and these need to be initialized before each call to
        // CalcWindowHeatBalance.
        // Only for Surface(SurfNum).Class == DataSurfaces::SurfaceClass::Window
        int const firstWindowSurf = state.dataHeatBal->Zone(zoneNum).WindowSurfaceFirst;
        int const lastWindowSurf = state.dataHeatBal->Zone(zoneNum).WindowSurfaceLast;
        for (int surfNum = firstWindowSurf; surfNum <= lastWindowSurf; ++surfNum) {
            state.dataSurface->SurfWinHeatGain(surfNum) = 0.0;
            state.dataSurface->SurfWinHeatTransfer(surfNum) = 0.0;
            state.dataSurface->SurfWinHeatGainRep(surfNum) = 0.0;
            state.dataSurface->SurfWinHeatLossRep(surfNum) = 0.0;
            state.dataSurface->SurfWinGainConvGlazToZoneRep(surfNum) = 0.0;
            state.dataSurface->SurfWinGainIRGlazToZoneRep(surfNum) = 0.0;
            state.dataSurface->SurfWinLossSWZoneToOutWinRep(surfNum) = 0.0;
            state.dataSurface->SurfWinGainFrameDividerToZoneRep(surfNum) = 0.0;
            state.dataSurface->SurfWinGainConvGlazShadGapToZoneRep(surfNum) = 0.0;
            state.dataSurface->SurfWinGainConvShadeToZoneRep(surfNum) = 0.0;
            state.dataSurface->SurfWinOtherConvGainInsideFaceToZoneRep(surfNum) = 0.0;
            state.dataSurface->SurfWinGainIRShadeToZoneRep(surfNum) = 0.0;
            state.dataSurface->SurfWinFrameQRadOutAbs(surfNum) = 0.0;
            state.dataSurface->SurfWinFrameQRadInAbs(surfNum) = 0.0;
            state.dataSurface->SurfWinDividerQRadOutAbs(surfNum) = 0.0;
            state.dataSurface->SurfWinDividerQRadInAbs(surfNum) = 0.0;
        }

        // Calculate heat extract due to additional heat flux source term as the surface boundary condition - all HT surfaces
        if (state.dataSurface->AnyHeatBalanceInsideSourceTerm) {
            for (int surfNum = firstSurf; surfNum <= lastSurf; ++surfNum) {
                if (Surface(surfNum).InsideHeatSourceTermSchedule) {
                    state.dataHeatBalSurf->SurfQAdditionalHeatSourceInside(surfNum) =
                        EnergyPlus::ScheduleManager::GetCurrentScheduleValue(state, Surface(surfNum).InsideHeatSourceTermSchedule);
                }
            }
        }

        // Set up coefficient arrays prior to calculations and precalc terms that do no change during iteration - non-window surfaces
        int const firstNonWinSurf = state.dataHeatBal->Zone(zoneNum).OpaqOrIntMassSurfaceFirst;
        int const lastNonWinSurf = state.dataHeatBal->Zone(zoneNum).OpaqOrIntMassSurfaceLast;
        Real64 const timeStepZoneSeconds = state.dataGlobal->TimeStepZoneSec; // local for vectorization
        Real64 const iterDampConstant = IterDampConst;                        // local for vectorization
        // this loop auto-vectorizes
        for (int surfNum = firstNonWinSurf; surfNum <= lastNonWinSurf; ++surfNum) {

            // Pre-calculate a few terms before the iteration loop
            state.dataHeatBalSurf->TempTermSurf(surfNum) =
                state.dataHeatBalSurf->CTFConstInPart(surfNum) + state.dataHeatBal->SurfQRadThermInAbs(surfNum) +
                state.dataHeatBalSurf->SurfOpaqQRadSWInAbs(surfNum) + state.dataHeatBalSurf->SurfQAdditionalHeatSourceInside(surfNum) +
                state.dataHeatBal->HConvIn(surfNum) * state.dataHeatBalSurfMgr->RefAirTemp(surfNum) +
                state.dataHeatBalFanSys->QHTRadSysSurf(surfNum) + state.dataHeatBalFanSys->QCoolingPanelSurf(surfNum) +
                state.dataHeatBalFanSys->QHWBaseboardSurf(surfNum) + state.dataHeatBalFanSys->QSteamBaseboardSurf(surfNum) +
                state.dataHeatBalFanSys->QElecBaseboardSurf(surfNum) + (state.dataHeatBalFanSys->QRadSurfAFNDuct(surfNum) / timeStepZoneSeconds);
            state.dataHeatBalSurf->TempDivSurf(surfNum) =
                1.0 / (state.dataHeatBalSurf->CTFInside0(surfNum) -
                       state.dataHeatBalSurf->IsAdiabatic(surfNum) * state.dataHeatBalSurf->CTFCross0(surfNum) +
                       state.dataHeatBalSurf->IsPoolSurf(surfNum) * state.dataHeatBalFanSys->PoolHeatTransCoefs(surfNum) +
                       state.dataHeatBalSurf->IsNotPoolSurf(surfNum) * state.dataHeatBal->HConvIn(surfNum) + iterDampConstant);
        }
    }

    state.dataHeatBal->InsideSurfIterations = 0;
    bool Converged = false; // .TRUE. if inside heat balance has converged
    while (!Converged) {    // Start of main inside heat balance iteration loop...

        state.dataHeatBalSurf->TempInsOld = state.dataHeatBalSurf->TempSurfIn; // Keep track of last iteration's temperature values

        HeatBalanceIntRadExchange::CalcInteriorRadExchange(state,
                                                           state.dataHeatBalSurf->TempSurfIn,
                                                           state.dataHeatBal->InsideSurfIterations,
                                                           state.dataHeatBalSurf->SurfNetLWRadToSurf,
                                                           ZoneToResimulate,
                                                           Inside); // Update the radiation balance

        // Every 30 iterations, recalculate the inside convection coefficients in case
        // there has been a significant drift in the surface temperatures predicted.
        // This is not fool-proof and it basically means that the outside surface
        // heat balance is in error (potentially) once HConvIn is re-evaluated.
        // The choice of 30 is not significant--just want to do this a couple of
        // times before the iteration limit is hit.
        if ((state.dataHeatBal->InsideSurfIterations > 0) && (mod(state.dataHeatBal->InsideSurfIterations, ItersReevalConvCoeff) == 0)) {
            ConvectionCoefficients::InitInteriorConvectionCoeffs(state, state.dataHeatBalSurf->TempSurfIn, ZoneToResimulate);
            // Since HConvIn has changed re-calculate a few terms - non-window surfaces
            for (int zoneNum = FirstZone; zoneNum <= LastZone; ++zoneNum) {
                int const firstSurf = state.dataHeatBal->Zone(zoneNum).OpaqOrIntMassSurfaceFirst;
                int const lastSurf = state.dataHeatBal->Zone(zoneNum).OpaqOrIntMassSurfaceLast;
                Real64 const timeStepZoneSeconds = state.dataGlobal->TimeStepZoneSec; // local for vectorization
                Real64 const iterDampConstant = IterDampConst;                        // local for vectorization
                // this loop auto-vectorizes
                for (int surfNum = firstSurf; surfNum <= lastSurf; ++surfNum) {
                    state.dataHeatBalSurf->TempTermSurf(surfNum) =
                        state.dataHeatBalSurf->CTFConstInPart(surfNum) + state.dataHeatBal->SurfQRadThermInAbs(surfNum) +
                        state.dataHeatBalSurf->SurfOpaqQRadSWInAbs(surfNum) + state.dataHeatBalSurf->SurfQAdditionalHeatSourceInside(surfNum) +
                        state.dataHeatBal->HConvIn(surfNum) * state.dataHeatBalSurfMgr->RefAirTemp(surfNum) +
                        state.dataHeatBalFanSys->QHTRadSysSurf(surfNum) + state.dataHeatBalFanSys->QCoolingPanelSurf(surfNum) +
                        state.dataHeatBalFanSys->QHWBaseboardSurf(surfNum) + state.dataHeatBalFanSys->QSteamBaseboardSurf(surfNum) +
                        state.dataHeatBalFanSys->QElecBaseboardSurf(surfNum) +
                        (state.dataHeatBalFanSys->QRadSurfAFNDuct(surfNum) / timeStepZoneSeconds);
                    state.dataHeatBalSurf->TempDivSurf(surfNum) =
                        1.0 / (state.dataHeatBalSurf->CTFInside0(surfNum) -
                               state.dataHeatBalSurf->IsAdiabatic(surfNum) * state.dataHeatBalSurf->CTFCross0(surfNum) +
                               state.dataHeatBalSurf->IsPoolSurf(surfNum) * state.dataHeatBalFanSys->PoolHeatTransCoefs(surfNum) +
                               state.dataHeatBalSurf->IsNotPoolSurf(surfNum) * state.dataHeatBal->HConvIn(surfNum) + iterDampConstant);
                }
            }
        }

        // Loop over non-window surfaces
        for (int zoneNum = FirstZone; zoneNum <= LastZone; ++zoneNum) {
            int const firstNonWinSurf = state.dataHeatBal->Zone(zoneNum).OpaqOrIntMassSurfaceFirst;
            int const lastNonWinSurf = state.dataHeatBal->Zone(zoneNum).OpaqOrIntMassSurfaceLast;
            Real64 const iterDampConstant = IterDampConst; // local for vectorization
            // this loop auto-vectorizes
            for (int surfNum = firstNonWinSurf; surfNum <= lastNonWinSurf; ++surfNum) {
                // Perform heat balance on the inside face of the surface ...
                // The following are possibilities here (this function only does CTF, see CalcHeatBalanceInsideSurf2 for others):
                //   (a) the surface is a pool (no movable insulation, no source/sink, only CTF solution algorithm)
                //   (b) the surface is adiabatic (a partition), in which case the temperature of both sides are the same
                //   (c) standard (or interzone) opaque surface with no movable insulation, normal heat balance equation
                //   (d) standard (or interzone) window: call to CalcWindowHeatBalance to get window layer temperatures
                //   (e) standard opaque surface with movable insulation, special two-part equation
                // In the surface calculation there are the following Algorithm types for opaque surfaces that
                // do not have movable insulation:
                //   (a) the regular CTF calc (SolutionAlgo = UseCTF)
                //   (b) the EMPD calc (Solutionalgo = UseEMPD)
                //   (c) the CondFD calc (SolutionAlgo = UseCondFD)
                //   (d) the HAMT calc (solutionalgo = UseHAMT).

                // For adiabatic surface:
                // Adiabatic:   TempDiv = (1.0 / (construct.CTFInside(0) - construct.CTFCross(0) + HConvIn_surf + IterDampConst));
                // Adiabatic:   TempSurfInTmp(SurfNum) = (TempTerm + IterDampConst * TempInsOld(SurfNum)) * TempDiv;
                // Ad+Source:   TempSurfInTmp(SurfNum) = (TempTerm + construct.CTFSourceIn(0) * QsrcHist(SurfNum, 1) + IterDampConst *
                // TempInsOld(SurfNum)) * TempDiv; Ad+Pool:     TempDiv = (1.0 / (construct.CTFInside(0) - construct.CTFCross(0) +
                // PoolHeatTransCoefs(SurfNum) + IterDampConst); Ad+Pool:     TempSurfInTmp(SurfNum) = (CTFConstInPart(SurfNum) +
                // QPoolSurfNumerator(SurfNum) + IterDampConst * TempInsOld(SurfNum)) * TempDiv;

                // For standard or interzone surface:
                // Standard:    TempDiv = (1.0 / (construct.CTFInside(0) + HConvIn_surf + IterDampConst));
                // Standard:    TempSurfInTmp(SurfNum) = (TempTerm + IterDampConst * TempInsOld(SurfNum) + construct.CTFCross(0) * TH11) *
                // TempDiv; Std+Source:  TempSurfInTmp(SurfNum) = (TempTerm + construct.CTFSourceIn(0) * QsrcHist(SurfNum, 1) + IterDampConst *
                // TempInsOld(SurfNum)) * TempDiv; Std+Pool:    TempDiv = (1.0 / (construct.CTFInside(0) + PoolHeatTransCoefs(SurfNum) +
                // IterDampConst); Std+Pool:    TempSurfInTmp(SurfNum) = (CTFConstInPart(SurfNum) + QPoolSurfNumerator(SurfNum) + IterDampConst*
                // TempInsOld(SurfNum) + construct.CTFCross(0) * TH11) * TempDiv;

                // Composite with Adiabatic/Source/Pool flags:
                //              TempDiv = (1.0 / (construct.CTFInside(0) - IsAdiabatic*construct.CTFCross(0)+
                //              IsPoolSurf*PoolHeatTransCoefs(SurfNum) + IsNotPoolSurf*HConvIn_surf + IterDampConst)); TempSurfInTmp(SurfNum) =
                //              (IsNotPoolSurf*TempTerm + IsSource*construct.CTFSourceIn(0) * QsrcHist(SurfNum, 1) +
                //              IsPoolSurf*CTFConstInPart(SurfNum) + IsPoolSurf*QPoolSurfNumerator(SurfNum)
                //                                        + IterDampConst * TempInsOld(SurfNum)+ IsNotAdiabatic*IsNotSource*construct.CTFCross(0)
                //                                        * TH11) * TempDiv;

                // Calculate the current inside surface temperature
                state.dataHeatBalSurf->TempSurfInTmp(surfNum) =
                    (state.dataHeatBalSurf->IsNotPoolSurf(surfNum) *
                         (state.dataHeatBalSurf->TempTermSurf(surfNum) + state.dataHeatBalSurf->SurfNetLWRadToSurf(surfNum)) +
                     state.dataHeatBalSurf->IsSource(surfNum) * state.dataHeatBalSurf->CTFSourceIn0(surfNum) *
                         state.dataHeatBalSurf->QsrcHistSurf1(surfNum) +
                     state.dataHeatBalSurf->IsPoolSurf(surfNum) * state.dataHeatBalSurf->CTFConstInPart(surfNum) +
                     state.dataHeatBalSurf->IsPoolSurf(surfNum) * state.dataHeatBalFanSys->QPoolSurfNumerator(surfNum) +
                     iterDampConstant * state.dataHeatBalSurf->TempInsOld(surfNum) +
                     state.dataHeatBalSurf->IsNotAdiabatic(surfNum) * state.dataHeatBalSurf->CTFCross0(surfNum) *
                         state.dataHeatBalSurf->TH11Surf(surfNum)) *
                    state.dataHeatBalSurf->TempDivSurf(
                        surfNum); // Constant part of conduction eq (history terms) | LW radiation from internal sources | SW
                                  // radiation from internal sources | Convection from surface to zone air | Net radiant
                                  // exchange with other zone surfaces | Heat source/sink term for radiant systems | (if there
                                  // is one present) | Radiant flux from high temp radiant heater | Radiant flux from a hot
                                  // water baseboard heater | Radiant flux from a steam baseboard heater | Radiant flux from
                                  // an electric baseboard heater | Iterative damping term (for stability) | Current
                                  // conduction from | the outside surface | Coefficient for conduction (current time) |
                                  // Convection and damping term | Radiation from AFN ducts

                state.dataHeatBalSurf->TempSurfIn(surfNum) = state.dataHeatBalSurf->TempSurfInTmp(surfNum);
            }

            // Loop over non-window surfaces (includes TubularDaylightingDomes)
            for (int surfNum = firstNonWinSurf; surfNum <= lastNonWinSurf; ++surfNum) {
                bool movableInsulPresent = state.dataSurface->AnyMovableInsulation && state.dataHeatBalSurf->SurfMovInsulIntPresent(surfNum);
                if (movableInsulPresent) { // Movable insulation present, recalc surface temps
                    Real64 HMovInsul = state.dataHeatBalSurf->SurfMovInsulHInt(surfNum);
                    Real64 F1 = HMovInsul / (HMovInsul + state.dataHeatBal->HConvIn(surfNum) + IterDampConst);
                    state.dataHeatBalSurf->TempSurfIn(surfNum) =
                        (state.dataHeatBalSurf->CTFConstInPart(surfNum) + state.dataHeatBalSurf->SurfOpaqQRadSWInAbs(surfNum) +
                         state.dataHeatBalSurf->CTFCross0(surfNum) * state.dataHeatBalSurf->TH11Surf(surfNum) +
                         F1 * (state.dataHeatBal->SurfQRadThermInAbs(surfNum) +
                               state.dataHeatBal->HConvIn(surfNum) * state.dataHeatBalSurfMgr->RefAirTemp(surfNum) +
                               state.dataHeatBalSurf->SurfNetLWRadToSurf(surfNum) + state.dataHeatBalFanSys->QHTRadSysSurf(surfNum) +
                               state.dataHeatBalFanSys->QCoolingPanelSurf(surfNum) + state.dataHeatBalFanSys->QHWBaseboardSurf(surfNum) +
                               state.dataHeatBalFanSys->QSteamBaseboardSurf(surfNum) + state.dataHeatBalFanSys->QElecBaseboardSurf(surfNum) +
                               state.dataHeatBalSurf->SurfQAdditionalHeatSourceInside(surfNum) +
                               IterDampConst * state.dataHeatBalSurf->TempInsOld(surfNum))) /
                        (state.dataHeatBalSurf->CTFInside0(surfNum) + HMovInsul - F1 * HMovInsul); // Convection from surface to zone air

                    state.dataHeatBalSurf->TempSurfInTmp(surfNum) =
                        (state.dataHeatBalSurf->CTFInside0(surfNum) * state.dataHeatBalSurf->TempSurfIn(surfNum) +
                         HMovInsul * state.dataHeatBalSurf->TempSurfIn(surfNum) - state.dataHeatBalSurf->SurfOpaqQRadSWInAbs(surfNum) -
                         state.dataHeatBalSurf->CTFConstInPart(surfNum) -
                         state.dataHeatBalSurf->CTFCross0(surfNum) * state.dataHeatBalSurf->TH11Surf(surfNum)) /
                        (HMovInsul);
                }

                if (state.dataHeatBal->AnyInternalHeatSourceInInput) {
                    if (state.dataConstruction->Construct(Surface(surfNum).Construction).SourceSinkPresent) {
                        // Set the appropriate parameters for the radiant system
                        // Radiant system does not need the damping coefficient terms (hopefully)
                        Real64 const RadSysDiv(1.0 / (state.dataHeatBalSurf->CTFInside0(surfNum) + state.dataHeatBal->HConvIn(surfNum)));
                        Real64 const TempTerm(
                            state.dataHeatBalSurf->CTFConstInPart(surfNum) + state.dataHeatBal->SurfQRadThermInAbs(surfNum) +
                            state.dataHeatBalSurf->SurfOpaqQRadSWInAbs(surfNum) + state.dataHeatBalSurf->SurfQAdditionalHeatSourceInside(surfNum) +
                            state.dataHeatBal->HConvIn(surfNum) * state.dataHeatBalSurfMgr->RefAirTemp(surfNum) +
                            state.dataHeatBalFanSys->QHTRadSysSurf(surfNum) + state.dataHeatBalFanSys->QCoolingPanelSurf(surfNum) +
                            state.dataHeatBalFanSys->QHWBaseboardSurf(surfNum) + state.dataHeatBalFanSys->QSteamBaseboardSurf(surfNum) +
                            state.dataHeatBalFanSys->QElecBaseboardSurf(surfNum) + state.dataHeatBalSurf->SurfNetLWRadToSurf(surfNum) +
                            (state.dataHeatBalFanSys->QRadSurfAFNDuct(surfNum) / state.dataGlobal->TimeStepZoneSec));
                        state.dataHeatBalFanSys->RadSysTiHBConstCoef(surfNum) =
                            TempTerm * RadSysDiv; // Constant portion of cond eq (history terms) | LW radiation from internal sources | SW
                                                  // radiation from internal sources | Convection from surface to zone air | Radiant flux
                                                  // from high temp radiant heater | Radiant flux from a hot water baseboard heater |
                                                  // Radiant flux from a steam baseboard heater | Radiant flux from an electric baseboard
                                                  // heater | Net radiant exchange with other zone surfaces | Cond term (both partition
                                                  // sides same temp) | Convection and damping term
                        state.dataHeatBalFanSys->RadSysTiHBToutCoef(surfNum) =
                            state.dataHeatBalSurf->CTFCross0(surfNum) * RadSysDiv; // Outside temp=inside temp for a partition |
                                                                                   // Cond term (both partition sides same temp) |
                                                                                   // Convection and damping term
                        state.dataHeatBalFanSys->RadSysTiHBQsrcCoef(surfNum) =
                            state.dataHeatBalSurf->CTFSourceIn0(surfNum) * RadSysDiv; // QTF term for the source | Cond term (both
                                                                                      // partition sides same temp) | Convection and
                                                                                      // damping term

                        if (Surface(surfNum).ExtBoundCond > 0) { // This is an interzone partition and we need to set outside params
                            // The inside coefficients of one side are equal to the outside coefficients of the other side.  But,
                            // the inside coefficients are set up once the heat balance equation for that side has been calculated.
                            // For both sides to actually have been set, we have to wait until we get to the second side in the surface
                            // derived type.  At that point, both inside coefficient sets have been evaluated.
                            if (Surface(surfNum).ExtBoundCond <= surfNum) { // Both of the inside coefficients have now been set
                                int OtherSideSurfNum = Surface(surfNum).ExtBoundCond;
                                state.dataHeatBalFanSys->RadSysToHBConstCoef(OtherSideSurfNum) =
                                    state.dataHeatBalFanSys->RadSysTiHBConstCoef(surfNum);
                                state.dataHeatBalFanSys->RadSysToHBTinCoef(OtherSideSurfNum) = state.dataHeatBalFanSys->RadSysTiHBToutCoef(surfNum);
                                state.dataHeatBalFanSys->RadSysToHBQsrcCoef(OtherSideSurfNum) = state.dataHeatBalFanSys->RadSysTiHBQsrcCoef(surfNum);
                                state.dataHeatBalFanSys->RadSysToHBConstCoef(surfNum) =
                                    state.dataHeatBalFanSys->RadSysTiHBConstCoef(OtherSideSurfNum);
                                state.dataHeatBalFanSys->RadSysToHBTinCoef(surfNum) = state.dataHeatBalFanSys->RadSysTiHBToutCoef(OtherSideSurfNum);
                                state.dataHeatBalFanSys->RadSysToHBQsrcCoef(surfNum) = state.dataHeatBalFanSys->RadSysTiHBQsrcCoef(OtherSideSurfNum);
                            }
                        }
                    }
                }
            }

            // Loop over window surfaces
            int const firstWindowSurf = state.dataHeatBal->Zone(zoneNum).WindowSurfaceFirst;
            int const lastWindowSurf = state.dataHeatBal->Zone(zoneNum).WindowSurfaceLast;
            for (int surfNum = firstWindowSurf; surfNum <= lastWindowSurf; ++surfNum) {
                auto &surface(Surface(surfNum));
                Real64 &TH11(state.dataHeatBalSurf->TH(1, 1, surfNum));
                int const ConstrNum = state.dataSurface->SurfActiveConstruction(surfNum);
                auto const &construct(state.dataConstruction->Construct(ConstrNum));
                if (state.dataSurface->SurfWinOriginalClass(surfNum) == SurfaceClass::TDD_Diffuser) { // Tubular daylighting device
                    // Lookup up the TDD:DOME object
                    int const pipeNum = state.dataSurface->SurfWinTDDPipeNum(surfNum);
                    int const domeNum = state.dataDaylightingDevicesData->TDDPipe(pipeNum).Dome;
                    // Ueff = 1 / effective R value between TDD:DOME and TDD:DIFFUSER
                    Real64 Ueff = 1.0 / state.dataDaylightingDevicesData->TDDPipe(pipeNum).Reff;

                    // Similar to opaque surface but outside surface temp of TDD:DOME is used, and no embedded sources/sinks.
                    // Absorbed shortwave radiation is treated similar to a regular window, but only 1 glass layer is allowed.
                    //   = QRadSWwinAbs(surfNum,1)/2.0
                    Real64 const HConvIn_surf(state.dataMstBal->HConvInFD(surfNum) = state.dataHeatBal->HConvIn(surfNum));
                    state.dataHeatBalSurf->TempSurfIn(surfNum) = state.dataHeatBalSurf->TempSurfInTmp(surfNum) =
                        (state.dataHeatBal->SurfQRadThermInAbs(surfNum) + state.dataHeatBal->SurfWinQRadSWwinAbs(surfNum, 1) / 2.0 +
                         state.dataHeatBalSurf->SurfQAdditionalHeatSourceInside(surfNum) +
                         HConvIn_surf * state.dataHeatBalSurfMgr->RefAirTemp(surfNum) + state.dataHeatBalSurf->SurfNetLWRadToSurf(surfNum) +
                         IterDampConst * state.dataHeatBalSurf->TempInsOld(surfNum) + Ueff * state.dataHeatBalSurf->TH(1, 1, domeNum)) /
                        (Ueff + HConvIn_surf + IterDampConst); // LW radiation from internal sources | SW radiation from internal sources and
                                                               // solar | Convection from surface to zone air | Net radiant exchange with
                                                               // other zone surfaces | Iterative damping term (for stability) | Current
                                                               // conduction from the outside surface | Coefficient for conduction (current
                                                               // time) | Convection and damping term

                    Real64 const Sigma_Temp_4(DataGlobalConstants::StefanBoltzmann * pow_4(state.dataHeatBalSurf->TempSurfIn(surfNum)));

                    // Calculate window heat gain for TDD:DIFFUSER since this calculation is usually done in WindowManager
                    state.dataSurface->SurfWinHeatGain(surfNum) =
                        state.dataSurface->SurfWinTransSolar(surfNum) +
                        HConvIn_surf * surface.Area * (state.dataHeatBalSurf->TempSurfIn(surfNum) - state.dataHeatBalSurfMgr->RefAirTemp(surfNum)) +
                        state.dataConstruction->Construct(surface.Construction).InsideAbsorpThermal * surface.Area *
                            (Sigma_Temp_4 -
                             (state.dataSurface->SurfWinIRfromParentZone(surfNum) + state.dataHeatBalFanSys->QHTRadSysSurf(surfNum) +
                              state.dataHeatBalFanSys->QCoolingPanelSurf(surfNum) + state.dataHeatBalFanSys->QHWBaseboardSurf(surfNum) +
                              state.dataHeatBalFanSys->QSteamBaseboardSurf(surfNum) + state.dataHeatBalFanSys->QElecBaseboardSurf(surfNum))) -
                        state.dataHeatBal->EnclSolQSWRad(surface.SolarEnclIndex) * surface.Area *
                            state.dataConstruction->Construct(surface.Construction)
                                .TransDiff; // Transmitted solar | Convection | IR exchange | IR
                                            // Zone diffuse interior shortwave reflected back into the TDD
                    state.dataSurface->SurfWinHeatTransfer(surfNum) = state.dataSurface->SurfWinHeatGain(surfNum);

                    // fill out report vars for components of Window Heat Gain
                    state.dataSurface->SurfWinGainConvGlazToZoneRep(surfNum) =
                        HConvIn_surf * surface.Area * (state.dataHeatBalSurf->TempSurfIn(surfNum) - state.dataHeatBalSurfMgr->RefAirTemp(surfNum));
                    state.dataSurface->SurfWinGainIRGlazToZoneRep(surfNum) =
                        state.dataConstruction->Construct(surface.Construction).InsideAbsorpThermal * surface.Area *
                        (Sigma_Temp_4 -
                         (state.dataSurface->SurfWinIRfromParentZone(surfNum) + state.dataHeatBalFanSys->QHTRadSysSurf(surfNum) +
                          state.dataHeatBalFanSys->QCoolingPanelSurf(surfNum) + state.dataHeatBalFanSys->QHWBaseboardSurf(surfNum) +
                          state.dataHeatBalFanSys->QSteamBaseboardSurf(surfNum) + state.dataHeatBalFanSys->QElecBaseboardSurf(surfNum)));
                    state.dataSurface->SurfWinLossSWZoneToOutWinRep(surfNum) = state.dataHeatBal->EnclSolQSWRad(surface.SolarEnclIndex) *
                                                                               surface.Area *
                                                                               state.dataConstruction->Construct(surface.Construction).TransDiff;
                } else {                                                // Regular window
                    if (state.dataHeatBal->InsideSurfIterations == 0) { // Do windows only once
                        // Get outside convection coeff for exterior window here to avoid calling
                        // InitExteriorConvectionCoeff from CalcWindowHeatBalance, which avoids circular reference
                        // (HeatBalanceSurfaceManager USEing and WindowManager and
                        // WindowManager USEing HeatBalanceSurfaceManager)
                        if (surface.ExtBoundCond == ExternalEnvironment) {
                            int RoughSurf = state.dataMaterial->Material(construct.LayerPoint(1)).Roughness; // Outside surface roughness
                            Real64 EmisOut =
                                state.dataMaterial->Material(construct.LayerPoint(1)).AbsorpThermalFront; // Glass outside surface emissivity
                            auto const shading_flag(state.dataSurface->SurfWinShadingFlag(surfNum));
                            if (ANY_EXTERIOR_SHADE_BLIND_SCREEN(shading_flag)) {
                                // Exterior shade in place
                                int const ConstrNumSh = Surface(surfNum).activeShadedConstruction;
                                if (ConstrNumSh != 0) {
                                    RoughSurf = state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNumSh).LayerPoint(1)).Roughness;
                                    EmisOut =
                                        state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNumSh).LayerPoint(1)).AbsorpThermal;
                                }
                            }

                            // Get the outside effective emissivity for Equivalent layer model
                            if (construct.WindowTypeEQL) {
                                EmisOut = WindowEquivalentLayer::EQLWindowOutsideEffectiveEmiss(state, ConstrNum);
                            }
                            // Set Exterior Convection Coefficient...
                            if (state.dataSurface->SurfExtConvCoeffIndex(surfNum) > 0) {

                                state.dataHeatBalSurf->HcExtSurf(surfNum) = ConvectionCoefficients::SetExtConvectionCoeff(state, surfNum);

                            } else if (surface.ExtWind) { // Window is exposed to wind (and possibly rain)

                                // Calculate exterior heat transfer coefficients with windspeed (windspeed is calculated internally in
                                // subroutine)
                                ConvectionCoefficients::InitExteriorConvectionCoeff(state,
                                                                                    surfNum,
                                                                                    0.0,
                                                                                    RoughSurf,
                                                                                    EmisOut,
                                                                                    TH11,
                                                                                    state.dataHeatBalSurf->HcExtSurf(surfNum),
                                                                                    state.dataHeatBalSurf->HSkyExtSurf(surfNum),
                                                                                    state.dataHeatBalSurf->HGrdExtSurf(surfNum),
                                                                                    state.dataHeatBalSurf->HAirExtSurf(surfNum));

                                if (state.dataEnvrn->IsRain) { // Raining: since wind exposed, outside window surface gets wet
                                    state.dataHeatBalSurf->HcExtSurf(surfNum) = 1000.0; // Reset HcExtSurf because of wetness
                                }

                            } else { // Not Wind exposed

                                // Calculate exterior heat transfer coefficients for windspeed = 0
                                ConvectionCoefficients::InitExteriorConvectionCoeff(state,
                                                                                    surfNum,
                                                                                    0.0,
                                                                                    RoughSurf,
                                                                                    EmisOut,
                                                                                    TH11,
                                                                                    state.dataHeatBalSurf->HcExtSurf(surfNum),
                                                                                    state.dataHeatBalSurf->HSkyExtSurf(surfNum),
                                                                                    state.dataHeatBalSurf->HGrdExtSurf(surfNum),
                                                                                    state.dataHeatBalSurf->HAirExtSurf(surfNum));
                            }
                        } else { // Interior Surface

                            if (state.dataSurface->SurfExtConvCoeffIndex(surfNum) > 0) {
                                state.dataHeatBalSurf->HcExtSurf(surfNum) = ConvectionCoefficients::SetExtConvectionCoeff(state, surfNum);
                            } else {
                                // Exterior Convection Coefficient for the Interior or Interzone Window is the Interior Convection Coeff of
                                // same
                                state.dataHeatBalSurf->HcExtSurf(surfNum) = state.dataHeatBal->HConvIn(surface.ExtBoundCond);
                            }
                        }

                        // Following call determines inside surface temperature of glazing, and of
                        // frame and/or divider, if present
                        CalcWindowHeatBalance(
                            state, surfNum, state.dataHeatBalSurf->HcExtSurf(surfNum), state.dataHeatBalSurf->TempSurfInTmp(surfNum), TH11);

                        state.dataHeatBalSurf->TempSurfIn(surfNum) = state.dataHeatBalSurf->TempSurfInTmp(surfNum);
                    }
                }
            }

            int const firstSurf = state.dataHeatBal->Zone(zoneNum).OpaqOrWinSurfaceFirst;
            int const lastSurf = state.dataHeatBal->Zone(zoneNum).OpaqOrWinSurfaceLast;
            for (int surfNum = firstSurf; surfNum <= lastSurf; ++surfNum) {
                auto &zone(state.dataHeatBal->Zone(zoneNum));
                Real64 &TH11(state.dataHeatBalSurf->TH(1, 1, surfNum));
                Real64 &TH12(state.dataHeatBalSurf->TH(2, 1, surfNum));
                TH12 = state.dataHeatBalSurf->TempSurfInRep(surfNum) = state.dataHeatBalSurf->TempSurfIn(surfNum);
                state.dataHeatBalSurf->SurfTempOut(surfNum) = TH11;                                   // For reporting
                if (state.dataSurface->SurfWinOriginalClass(surfNum) == SurfaceClass::TDD_Diffuser) { // Tubular daylighting device
                    // Tubular daylighting devices are treated as one big object with an effective R value.
                    // The outside face temperature of the TDD:DOME and the inside face temperature of the
                    // TDD:DIFFUSER are calculated with the outside and inside heat balances respectively.
                    // Below, the resulting temperatures are copied to the inside face of the TDD:DOME
                    // and the outside face of the TDD:DIFFUSER for reporting.

                    // Set inside temp variables of TDD:DOME equal to inside temp of TDD:DIFFUSER
                    int domeNum = state.dataDaylightingDevicesData->TDDPipe(state.dataSurface->SurfWinTDDPipeNum(surfNum)).Dome;
                    state.dataHeatBalSurf->TH(2, 1, domeNum) = state.dataHeatBalSurf->TempSurfIn(domeNum) =
                        state.dataHeatBalSurf->TempSurfInTmp(domeNum) = state.dataHeatBalSurf->TempSurfInRep(domeNum) =
                            state.dataHeatBalSurf->TempSurfIn(surfNum);

                    // Set outside temp reporting variable of TDD:DOME (since it gets skipped otherwise)
                    // Reset outside temp variables of TDD:DIFFUSER equal to outside temp of TDD:DOME
                    TH11 = state.dataHeatBalSurf->SurfTempOut(surfNum) = state.dataHeatBalSurf->SurfTempOut(domeNum) =
                        state.dataHeatBalSurf->TH(1, 1, domeNum);
                }

                if ((TH12 > state.dataHeatBalSurf->MaxSurfaceTempLimit) || (TH12 < MinSurfaceTempLimit)) {
                    TestSurfTempCalcHeatBalanceInsideSurf(state, TH12, surfNum, zone, state.dataHeatBalSurfMgr->calcHeatBalInsideSurfWarmupErrCount);
                }
            }
        } // ...end of main loops over all surfaces for inside heat balances

        // Interzone surface updating: interzone surfaces have other side temperatures
        // which can vary as the simulation iterates through the inside heat
        // balance.  This block is intended to "lock" the opposite side (outside)
        // temperatures to the correct value, namely the value calculated by the
        // inside surface heat balance for the other side.
        assert(state.dataHeatBalSurf->TH.index(1, 1, 1) == 0u); // Assumed for linear indexing below
        auto const l211(state.dataHeatBalSurf->TH.index(2, 1, 1) - 1);
        for (int SurfNum : IZSurfs) {
            int const surfExtBoundCond(Surface(SurfNum).ExtBoundCond);
            // Set the outside surface temperature to the inside surface temperature of the interzone pair.
            // By going through all of the surfaces, this should pick up the other side as well as affect the next iteration.
            // [ SurfNum - 1 ] == ( 1, 1, SurfNum )
            // [ l211 + surfExtBoundCond ] == ( 2, 1, surfExtBoundCond )
            state.dataHeatBalSurf->SurfTempOut(SurfNum) = state.dataHeatBalSurf->TH[SurfNum - 1] = state.dataHeatBalSurf->TH[l211 + surfExtBoundCond];
            state.dataHeatBalSurf->TH11Surf(SurfNum) = state.dataHeatBalSurf->SurfTempOut(SurfNum);
        }

        ++state.dataHeatBal->InsideSurfIterations;

        // Convergence check - Loop through all relevant non-window surfaces to check for convergence...
        Real64 MaxDelTemp = 0.0; // Maximum change in surface temperature for any opaque surface from one iteration to the next
        for (int zoneNum = FirstZone; zoneNum <= LastZone; ++zoneNum) {
            int const firstNonWinSurf = state.dataHeatBal->Zone(zoneNum).OpaqOrIntMassSurfaceFirst;
            int const lastNonWinSurf = state.dataHeatBal->Zone(zoneNum).OpaqOrIntMassSurfaceLast;
            for (int surfNum = firstNonWinSurf; surfNum <= lastNonWinSurf; ++surfNum) {
                Real64 delta = state.dataHeatBalSurf->TempSurfIn(surfNum) - state.dataHeatBalSurf->TempInsOld(surfNum);
                Real64 absDif = std::abs(delta);
                MaxDelTemp = std::max(absDif, MaxDelTemp);
            }
        } // ...end of loop to check for convergence

        if (MaxDelTemp <= state.dataHeatBal->MaxAllowedDelTemp) Converged = true;

#ifdef EP_Count_Calls
        state.dataTimingsData->NumMaxInsideSurfIterations =
            max(state.dataTimingsData->NumMaxInsideSurfIterations, state.dataHeatBal->InsideSurfIterations);
#endif

        if (state.dataHeatBal->InsideSurfIterations < state.dataHeatBalSurf->MinIterations) Converged = false;

        if (state.dataHeatBal->InsideSurfIterations > MaxIterations) {
            if (!state.dataGlobal->WarmupFlag) {
                ++state.dataHeatBalSurfMgr->calcHeatBalInsideSurfErrCount;
                if (state.dataHeatBalSurfMgr->calcHeatBalInsideSurfErrCount < 16) {
                    ShowWarningError(state,
                                     format("Inside surface heat balance did not converge with Max Temp Difference [C] ={:.3R} vs Max Allowed "
                                            "Temp Diff [C] ={:.6R}",
                                            MaxDelTemp,
                                            state.dataHeatBal->MaxAllowedDelTempCondFD));
                    ShowContinueErrorTimeStamp(state, "");
                } else {
                    ShowRecurringWarningErrorAtEnd(state,
                                                   "Inside surface heat balance convergence problem continues",
                                                   state.dataHeatBalSurfMgr->calcHeatBalInsideSurfErrPointer,
                                                   MaxDelTemp,
                                                   MaxDelTemp,
                                                   _,
                                                   "[C]",
                                                   "[C]");
                }
            }
            break; // iteration loop
        }

    } // ...end of main inside heat balance iteration loop (ends when Converged)

    // Set various surface output variables and other record keeping - after iterations are complete - all HT surfaces
    for (int zoneNum = FirstZone; zoneNum <= LastZone; ++zoneNum) {
        int const firstSurf = state.dataHeatBal->Zone(zoneNum).OpaqOrWinSurfaceFirst;
        int const lastSurf = state.dataHeatBal->Zone(zoneNum).OpaqOrWinSurfaceLast;
        for (int surfNum = firstSurf; surfNum <= lastSurf; ++surfNum) {
            // Inside Face Convection - sign convention is positive means energy going into inside face from the air.
            auto const HConvInTemp_fac(-state.dataHeatBal->HConvIn(surfNum) *
                                       (state.dataHeatBalSurf->TempSurfIn(surfNum) - state.dataHeatBalSurfMgr->RefAirTemp(surfNum)));
            state.dataHeatBalSurf->QdotConvInRep(surfNum) = Surface(surfNum).Area * HConvInTemp_fac;
            state.dataHeatBalSurf->QdotConvInRepPerArea(surfNum) = HConvInTemp_fac;
            state.dataHeatBalSurf->QConvInReport(surfNum) = state.dataHeatBalSurf->QdotConvInRep(surfNum) * state.dataGlobal->TimeStepZoneSec;

            // The QdotConvInRep which is called "Surface Inside Face Convection Heat Gain" is stored during
            // sizing for both the normal and pulse cases so that load components can be derived later.
            if (state.dataGlobal->ZoneSizingCalc && state.dataGlobal->CompLoadReportIsReq) {
                if (!state.dataGlobal->WarmupFlag) {
                    int TimeStepInDay = (state.dataGlobal->HourOfDay - 1) * state.dataGlobal->NumOfTimeStepInHour + state.dataGlobal->TimeStep;
                    if (state.dataGlobal->isPulseZoneSizing) {
                        state.dataOutRptTab->loadConvectedWithPulse(state.dataSize->CurOverallSimDay, TimeStepInDay, surfNum) =
                            state.dataHeatBalSurf->QdotConvInRep(surfNum);
                    } else {
                        state.dataOutRptTab->loadConvectedNormal(state.dataSize->CurOverallSimDay, TimeStepInDay, surfNum) =
                            state.dataHeatBalSurf->QdotConvInRep(surfNum);
                        state.dataOutRptTab->netSurfRadSeq(state.dataSize->CurOverallSimDay, TimeStepInDay, surfNum) =
                            state.dataHeatBalSurf->SurfNetLWRadToSurf(surfNum) * Surface(surfNum).Area;
                    }
                }
            }

            // Window heat gain/loss
            if (Surface(surfNum).Class == DataSurfaces::SurfaceClass::Window) {
                if (state.dataSurface->SurfWinHeatGain(surfNum) >= 0.0) {
                    state.dataSurface->SurfWinHeatGainRep(surfNum) = state.dataSurface->SurfWinHeatGain(surfNum);
                    state.dataSurface->SurfWinHeatGainRepEnergy(surfNum) =
                        state.dataSurface->SurfWinHeatGainRep(surfNum) * state.dataGlobal->TimeStepZoneSec;
                } else {
                    state.dataSurface->SurfWinHeatLossRep(surfNum) = -state.dataSurface->SurfWinHeatGain(surfNum);
                    state.dataSurface->SurfWinHeatLossRepEnergy(surfNum) =
                        state.dataSurface->SurfWinHeatLossRep(surfNum) * state.dataGlobal->TimeStepZoneSec;
                }
                state.dataSurface->SurfWinHeatTransferRepEnergy(surfNum) =
                    state.dataSurface->SurfWinHeatGain(surfNum) * state.dataGlobal->TimeStepZoneSec;
                if (state.dataSurface->SurfWinOriginalClass(surfNum) == DataSurfaces::SurfaceClass::TDD_Diffuser) { // Tubular daylighting device
                    int pipeNum = state.dataSurface->SurfWinTDDPipeNum(surfNum);
                    state.dataDaylightingDevicesData->TDDPipe(pipeNum).HeatGain = state.dataSurface->SurfWinHeatGainRep(surfNum);
                    state.dataDaylightingDevicesData->TDDPipe(pipeNum).HeatLoss = state.dataSurface->SurfWinHeatLossRep(surfNum);
                }
                if (Surface(surfNum).ExtSolar) { // WindowManager's definition of ZoneWinHeatGain/Loss
                    int zoneNum = Surface(surfNum).Zone;
                    state.dataHeatBal->ZoneWinHeatGain(zoneNum) += state.dataSurface->SurfWinHeatGain(surfNum);
                }
            }
        }
    }

    if (state.dataSurface->AnyMovableInsulation) ReportIntMovInsInsideSurfTemp(state);

    CalculateZoneMRT(state, ZoneToResimulate); // Update here so that the proper value of MRT is available to radiant systems
}

void TestSurfTempCalcHeatBalanceInsideSurf(EnergyPlusData &state, Real64 TH12, int const SurfNum, ZoneData &zone, int WarmupSurfTemp)
{
    std::string surfName = state.dataSurface->Surface(SurfNum).Name;

    if ((TH12 > state.dataHeatBalSurf->MaxSurfaceTempLimit) || (TH12 < MinSurfaceTempLimit)) {
        if (state.dataGlobal->WarmupFlag) ++WarmupSurfTemp;
        if (!state.dataGlobal->WarmupFlag || WarmupSurfTemp > 10 || state.dataGlobal->DisplayExtraWarnings) {
            if (TH12 < MinSurfaceTempLimit) {
                if (state.dataSurface->SurfLowTempErrCount(SurfNum) == 0) {
                    ShowSevereMessage(
                        state, format("Temperature (low) out of bounds [{:.2R}] for zone=\"{}\", for surface=\"{}\"", TH12, zone.Name, surfName));
                    ShowContinueErrorTimeStamp(state, "");
                    if (!zone.TempOutOfBoundsReported) {
                        ShowContinueError(state, "Zone=\"" + zone.Name + "\", Diagnostic Details:");
                        if (zone.FloorArea > 0.0) {
                            ShowContinueError(state, format("...Internal Heat Gain [{:.3R}] W/m2", zone.InternalHeatGains / zone.FloorArea));
                        } else {
                            ShowContinueError(state, format("...Internal Heat Gain (no floor) [{:.3R}] W", zone.InternalHeatGains));
                        }
                        if (state.dataAirflowNetwork->SimulateAirflowNetwork <= AirflowNetwork::AirflowNetworkControlSimple) {
                            ShowContinueError(state, format("...Infiltration/Ventilation [{:.3R}] m3/s", zone.NominalInfilVent));
                            ShowContinueError(state, format("...Mixing/Cross Mixing [{:.3R}] m3/s", zone.NominalMixing));
                        } else {
                            ShowContinueError(state, "...Airflow Network Simulation: Nominal Infiltration/Ventilation/Mixing not available.");
                        }
                        if (zone.IsControlled) {
                            ShowContinueError(state, "...Zone is part of HVAC controlled system.");
                        } else {
                            ShowContinueError(state, "...Zone is not part of HVAC controlled system.");
                        }
                        zone.TempOutOfBoundsReported = true;
                    }
                    ShowRecurringSevereErrorAtEnd(state,
                                                  "Temperature (low) out of bounds for zone=" + zone.Name + " for surface=" + surfName,
                                                  state.dataSurface->SurfLowTempErrCount(SurfNum),
                                                  TH12,
                                                  TH12,
                                                  _,
                                                  "C",
                                                  "C");
                } else {
                    ShowRecurringSevereErrorAtEnd(state,
                                                  "Temperature (low) out of bounds for zone=" + zone.Name + " for surface=" + surfName,
                                                  state.dataSurface->SurfLowTempErrCount(SurfNum),
                                                  TH12,
                                                  TH12,
                                                  _,
                                                  "C",
                                                  "C");
                }
            } else {
                if (state.dataSurface->SurfHighTempErrCount(SurfNum) == 0) {
                    ShowSevereMessage(
                        state, format("Temperature (high) out of bounds ({:.2R}] for zone=\"{}\", for surface=\"{}\"", TH12, zone.Name, surfName));
                    ShowContinueErrorTimeStamp(state, "");
                    if (!zone.TempOutOfBoundsReported) {
                        ShowContinueError(state, "Zone=\"" + zone.Name + "\", Diagnostic Details:");
                        if (zone.FloorArea > 0.0) {
                            ShowContinueError(state, format("...Internal Heat Gain [{:.3R}] W/m2", zone.InternalHeatGains / zone.FloorArea));
                        } else {
                            ShowContinueError(state, format("...Internal Heat Gain (no floor) [{:.3R}] W", zone.InternalHeatGains));
                        }
                        if (state.dataAirflowNetwork->SimulateAirflowNetwork <= AirflowNetwork::AirflowNetworkControlSimple) {
                            ShowContinueError(state, format("...Infiltration/Ventilation [{:.3R}] m3/s", zone.NominalInfilVent));
                            ShowContinueError(state, format("...Mixing/Cross Mixing [{:.3R}] m3/s", zone.NominalMixing));
                        } else {
                            ShowContinueError(state, "...Airflow Network Simulation: Nominal Infiltration/Ventilation/Mixing not available.");
                        }
                        if (zone.IsControlled) {
                            ShowContinueError(state, "...Zone is part of HVAC controlled system.");
                        } else {
                            ShowContinueError(state, "...Zone is not part of HVAC controlled system.");
                        }
                        zone.TempOutOfBoundsReported = true;
                    }
                    ShowRecurringSevereErrorAtEnd(state,
                                                  "Temperature (high) out of bounds for zone=" + zone.Name + " for surface=" + surfName,
                                                  state.dataSurface->SurfHighTempErrCount(SurfNum),
                                                  TH12,
                                                  TH12,
                                                  _,
                                                  "C",
                                                  "C");
                } else {
                    ShowRecurringSevereErrorAtEnd(state,
                                                  "Temperature (high) out of bounds for zone=" + zone.Name + " for surface=" + surfName,
                                                  state.dataSurface->SurfHighTempErrCount(SurfNum),
                                                  TH12,
                                                  TH12,
                                                  _,
                                                  "C",
                                                  "C");
                }
            }
            if (zone.EnforcedReciprocity) {
                if (WarmupSurfTemp > 3) {
                    ShowSevereError(state, "CalcHeatBalanceInsideSurf: Zone=\"" + zone.Name + "\" has view factor enforced reciprocity");
                    ShowContinueError(state, " and is having temperature out of bounds errors. Please correct zone geometry and rerun.");
                    ShowFatalError(state, "CalcHeatBalanceInsideSurf: Program terminates due to preceding conditions.");
                }
            } else if (WarmupSurfTemp > 10) {
                ShowFatalError(state, "CalcHeatBalanceInsideSurf: Program terminates due to preceding conditions.");
            }
        }
    }
    if ((TH12 > state.dataHeatBalSurf->MaxSurfaceTempLimitBeforeFatal) || (TH12 < MinSurfaceTempLimitBeforeFatal)) {
        if (!state.dataGlobal->WarmupFlag) {
            if (TH12 < MinSurfaceTempLimitBeforeFatal) {
                ShowSevereError(state,
                                format("Temperature (low) out of bounds [{:.2R}] for zone=\"{}\", for surface=\"{}\"", TH12, zone.Name, surfName));
                ShowContinueErrorTimeStamp(state, "");
                if (!zone.TempOutOfBoundsReported) {
                    ShowContinueError(state, "Zone=\"" + zone.Name + "\", Diagnostic Details:");
                    if (zone.FloorArea > 0.0) {
                        ShowContinueError(state, format("...Internal Heat Gain [{:.3R}] W/m2", zone.InternalHeatGains / zone.FloorArea));
                    } else {
                        ShowContinueError(state, format("...Internal Heat Gain (no floor) [{:.3R}] W", zone.InternalHeatGains / zone.FloorArea));
                    }
                    if (state.dataAirflowNetwork->SimulateAirflowNetwork <= AirflowNetwork::AirflowNetworkControlSimple) {
                        ShowContinueError(state, format("...Infiltration/Ventilation [{:.3R}] m3/s", zone.NominalInfilVent));
                        ShowContinueError(state, format("...Mixing/Cross Mixing [{:.3R}] m3/s", zone.NominalMixing));
                    } else {
                        ShowContinueError(state, "...Airflow Network Simulation: Nominal Infiltration/Ventilation/Mixing not available.");
                    }
                    if (zone.IsControlled) {
                        ShowContinueError(state, "...Zone is part of HVAC controlled system.");
                    } else {
                        ShowContinueError(state, "...Zone is not part of HVAC controlled system.");
                    }
                    zone.TempOutOfBoundsReported = true;
                }
                ShowFatalError(state, "Program terminates due to preceding condition.");
            } else {
                ShowSevereError(state,
                                format("Temperature (high) out of bounds [{:.2R}] for zone=\"{}\", for surface=\"{}\"", TH12, zone.Name, surfName));
                ShowContinueErrorTimeStamp(state, "");
                if (!zone.TempOutOfBoundsReported) {
                    ShowContinueError(state, "Zone=\"" + zone.Name + "\", Diagnostic Details:");
                    if (zone.FloorArea > 0.0) {
                        ShowContinueError(state, format("...Internal Heat Gain [{:.3R}] W/m2", zone.InternalHeatGains / zone.FloorArea));
                    } else {
                        ShowContinueError(state, format("...Internal Heat Gain (no floor) [{:.3R}] W", zone.InternalHeatGains / zone.FloorArea));
                    }
                    if (state.dataAirflowNetwork->SimulateAirflowNetwork <= AirflowNetwork::AirflowNetworkControlSimple) {
                        ShowContinueError(state, format("...Infiltration/Ventilation [{:.3R}] m3/s", zone.NominalInfilVent));
                        ShowContinueError(state, format("...Mixing/Cross Mixing [{:.3R}] m3/s", zone.NominalMixing));
                    } else {
                        ShowContinueError(state, "...Airflow Network Simulation: Nominal Infiltration/Ventilation/Mixing not available.");
                    }
                    if (zone.IsControlled) {
                        ShowContinueError(state, "...Zone is part of HVAC controlled system.");
                    } else {
                        ShowContinueError(state, "...Zone is not part of HVAC controlled system.");
                    }
                    zone.TempOutOfBoundsReported = true;
                }
                ShowFatalError(state, "Program terminates due to preceding condition.");
            }
        } else {
            if (TH12 < -10000. || TH12 > 10000.) {
                ShowSevereError(
                    state,
                    format("CalcHeatBalanceInsideSurf: The temperature of {:.2R} C for zone=\"{}\", for surface=\"{}\"", TH12, zone.Name, surfName));
                ShowContinueError(state, "..is very far out of bounds during warmup. This may be an indication of a malformed zone.");
                ShowContinueErrorTimeStamp(state, "");
                ShowFatalError(state, "Program terminates due to preceding condition.");
            }
        }
    }
}

void CalcOutsideSurfTemp(EnergyPlusData &state,
                         int const SurfNum,      // Surface number DO loop counter
                         int const ZoneNum,      // Zone number the current surface is attached to
                         int const ConstrNum,    // Construction index for the current surface
                         Real64 const HMovInsul, // "Convection" coefficient of movable insulation
                         Real64 const TempExt,   // Exterior temperature boundary condition
                         bool &ErrorFlag         // Error flag for movable insulation problem
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         George Walton
    //       DATE WRITTEN   December 1979
    //       MODIFIED       Jun 1990 (RDT for new CTF arrays)
    //                      Jul 2000 (RJL for Moisture algorithms)
    //                      Sep 2000 (RKS for new radiant exchange algorithm)
    //                      Dec 2000 (RKS for radiant system model addition)
    //                      Aug 2010 (BG added radiant heat flow rate reporting)
    //       RE-ENGINEERED  Mar 1998 (RKS)

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine performs a heat balance on the outside face of each
    // surface in the building.  NOTE that this also sets some coefficients
    // that are needed for radiant system modeling.  Thus, it is extremely
    // important that if someone makes changes to the heat balance equations
    // at a later date that they must also make changes to the coefficient
    // setting portion of this subroutine as well.

    // METHODOLOGY EMPLOYED:
    // Various boundary conditions are set and additional parameters are set-
    // up.  Then, the proper heat balance equation is selected based on the
    // presence of movable insulation, thermal mass of the surface construction,
    // and convection model being used.

    // REFERENCES:
    // (I)BLAST legacy routine HBOUT
    // 1989 ASHRAE Handbook of Fundamentals (Figure 1 on p. 22.4, convection correlations)

    // Using/Aliasing
    using namespace DataEnvironment;
    using namespace DataHeatBalFanSys;
    using namespace DataHeatBalance;
    using namespace DataHeatBalSurface;
    using namespace DataSurfaces;
    using namespace Psychrometrics;

    // Determine whether or not movable insulation is present
    bool MovInsulPresent = (HMovInsul > 0.0); // .TRUE. if movable insulation is currently present for surface
    bool QuickConductionSurf;                 // .TRUE. if the cross CTF term is relatively large
    Real64 F1;                                // Intermediate calculation variable
    Real64 F2;                                // Intermediate calculation variable
    // Determine whether this surface is a "slow conductive" or "quick conductive"
    // surface.  Designates are inherited from BLAST.  Basically, a "quick" surface
    // requires the inside heat balance to be accounted for in the heat balance
    // while a "slow" surface can used the last time step's value for inside
    // surface temperature.
    auto &Surface(state.dataSurface->Surface);
    auto const &construct(state.dataConstruction->Construct(ConstrNum));
    if (construct.CTFCross(0) > 0.01) {
        QuickConductionSurf = true;
        F1 = construct.CTFCross(0) / (construct.CTFInside(0) + state.dataHeatBal->HConvIn(SurfNum));
    } else {
        QuickConductionSurf = false;
    }

    Real64 TSky = state.dataEnvrn->SkyTemp;
    Real64 TGround = state.dataEnvrn->OutDryBulbTemp;

    if (state.dataSurface->SurfHasSurroundingSurfProperties(SurfNum)) {
        int SrdSurfsNum = state.dataSurface->SurfSurroundingSurfacesNum(SurfNum);
        if (state.dataSurface->SurroundingSurfsProperty(SrdSurfsNum).SkyTempSchNum != 0) {
            TSky = GetCurrentScheduleValue(state, state.dataSurface->SurroundingSurfsProperty(SrdSurfsNum).SkyTempSchNum);
        }
        if (state.dataSurface->SurroundingSurfsProperty(SrdSurfsNum).GroundTempSchNum != 0) {
            TGround = GetCurrentScheduleValue(state, state.dataSurface->SurroundingSurfsProperty(SrdSurfsNum).GroundTempSchNum);
        }
    }

    // Now, calculate the outside surface temperature using the proper heat balance equation.
    // Each case has been separated out into its own IF-THEN block for clarity.  Additional
    // cases can simply be added anywhere in the following section.  This is the last step
    // in the main loop.  Once the proper heat balance is done, the simulation goes on to
    // the next SurfNum.

    // Outside heat balance case: Tubular daylighting device
    Real64 &TH11(state.dataHeatBalSurf->TH(1, 1, SurfNum));
    if (Surface(SurfNum).Class == SurfaceClass::TDD_Dome) {

        // Lookup up the TDD:DIFFUSER object
        int PipeNum = state.dataSurface->SurfWinTDDPipeNum(SurfNum);
        int SurfNum2 = state.dataDaylightingDevicesData->TDDPipe(PipeNum).Diffuser;
        int ZoneNum2 = Surface(SurfNum2).Zone;
        Real64 Ueff = 1.0 / state.dataDaylightingDevicesData->TDDPipe(PipeNum).Reff; // 1 / effective R value between TDD:DOME and TDD:DIFFUSER
        F1 = Ueff / (Ueff + state.dataHeatBal->HConvIn(SurfNum2));

        // Similar to opaque surface but inside conditions of TDD:DIFFUSER are used, and no embedded sources/sinks.
        // Absorbed shortwave radiation is treated similar to a regular window, but only 1 glass layer is allowed.
        //   QRadSWOutAbs(SurfNum) does not apply for TDD:DOME, must use QRadSWwinAbs(SurfNum,1)/2.0 instead.
        //+Construct(ConstrNum)%CTFSourceOut(0)     &   TDDs cannot be radiant systems
        // *QsrcHist(1,SurfNum)                     &
        //+Construct(ConstrNum)%CTFSourceIn(0) &   TDDs cannot be radiant systems
        // *QsrcHist(1,SurfNum)                &
        TH11 = (state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum, 1) / 2.0 + state.dataHeatBalSurf->SurfQRadLWOutSrdSurfs(SurfNum) +
                (state.dataHeatBalSurf->HcExtSurf(SurfNum) + state.dataHeatBalSurf->HAirExtSurf(SurfNum)) * TempExt +
                state.dataHeatBalSurf->SurfQAdditionalHeatSourceOutside(SurfNum) + state.dataHeatBalSurf->HSkyExtSurf(SurfNum) * TSky +
                state.dataHeatBalSurf->HGrdExtSurf(SurfNum) * TGround +
                F1 * (state.dataHeatBal->SurfWinQRadSWwinAbs(SurfNum2, 1) / 2.0 + state.dataHeatBal->SurfQRadThermInAbs(SurfNum2) +
                      state.dataHeatBal->HConvIn(SurfNum2) * state.dataHeatBalFanSys->MAT(ZoneNum2) +
                      state.dataHeatBalSurf->SurfNetLWRadToSurf(SurfNum2))) /
               (Ueff + state.dataHeatBalSurf->HcExtSurf(SurfNum) + state.dataHeatBalSurf->HAirExtSurf(SurfNum) +
                state.dataHeatBalSurf->HSkyExtSurf(SurfNum) + state.dataHeatBalSurf->HGrdExtSurf(SurfNum) -
                F1 * Ueff); // Instead of QRadSWOutAbs(SurfNum) | ODB used to approx ground surface temp | Use TDD:DIFFUSER surface | Use
                            // TDD:DIFFUSER surface | Use TDD:DIFFUSER surface and zone | Use TDD:DIFFUSER surface

        // Outside heat balance case: No movable insulation, slow conduction
    } else if ((!MovInsulPresent) && (!QuickConductionSurf)) {
        // Add LWR from surrounding surfaces
        if (Surface(SurfNum).OSCMPtr == 0) {
            if (construct.SourceSinkPresent) {
                TH11 = (-state.dataHeatBalSurf->CTFConstOutPart(SurfNum) + state.dataHeatBalSurf->SurfOpaqQRadSWOutAbs(SurfNum) +
                        state.dataHeatBalSurf->SurfQRadLWOutSrdSurfs(SurfNum) +
                        (state.dataHeatBalSurf->HcExtSurf(SurfNum) + state.dataHeatBalSurf->HAirExtSurf(SurfNum)) * TempExt +
                        state.dataHeatBalSurf->SurfQAdditionalHeatSourceOutside(SurfNum) + state.dataHeatBalSurf->HSkyExtSurf(SurfNum) * TSky +
                        state.dataHeatBalSurf->HGrdExtSurf(SurfNum) * TGround + construct.CTFCross(0) * state.dataHeatBalSurf->TempSurfIn(SurfNum) +
                        construct.CTFSourceOut(0) * state.dataHeatBalSurf->QsrcHist(SurfNum, 1)) /
                       (construct.CTFOutside(0) + state.dataHeatBalSurf->HcExtSurf(SurfNum) + state.dataHeatBalSurf->HAirExtSurf(SurfNum) +
                        state.dataHeatBalSurf->HSkyExtSurf(SurfNum) +
                        state.dataHeatBalSurf->HGrdExtSurf(SurfNum)); // ODB used to approx ground surface temp
            } else {
                TH11 = (-state.dataHeatBalSurf->CTFConstOutPart(SurfNum) + state.dataHeatBalSurf->SurfOpaqQRadSWOutAbs(SurfNum) +
                        state.dataHeatBalSurf->SurfQRadLWOutSrdSurfs(SurfNum) +
                        (state.dataHeatBalSurf->HcExtSurf(SurfNum) + state.dataHeatBalSurf->HAirExtSurf(SurfNum)) * TempExt +
                        state.dataHeatBalSurf->SurfQAdditionalHeatSourceOutside(SurfNum) + state.dataHeatBalSurf->HSkyExtSurf(SurfNum) * TSky +
                        state.dataHeatBalSurf->HGrdExtSurf(SurfNum) * TGround + construct.CTFCross(0) * state.dataHeatBalSurf->TempSurfIn(SurfNum)) /
                       (construct.CTFOutside(0) + state.dataHeatBalSurf->HcExtSurf(SurfNum) + state.dataHeatBalSurf->HAirExtSurf(SurfNum) +
                        state.dataHeatBalSurf->HSkyExtSurf(SurfNum) +
                        state.dataHeatBalSurf->HGrdExtSurf(SurfNum)); // ODB used to approx ground surface temp
            }
            // Outside Heat Balance case: Other Side Conditions Model
        } else { //( Surface(SurfNum)%OSCMPtr > 0 ) THEN
            // local copies of variables for clarity in radiation terms
            // TODO: - int OSCMPtr; // "Pointer" to OSCM data structure (other side conditions from a model)
            Real64 RadTemp = state.dataSurface->OSCM(Surface(SurfNum).OSCMPtr)
                                 .TRad; // local value for Effective radiation temperature for OtherSideConditions model
            Real64 HRad = state.dataSurface->OSCM(Surface(SurfNum).OSCMPtr).HRad; // local value for effective (linearized) radiation coefficient

            // patterned after "No movable insulation, slow conduction," but with new radiation terms and no sun,
            if (construct.SourceSinkPresent) {
                TH11 = (-state.dataHeatBalSurf->CTFConstOutPart(SurfNum) + state.dataHeatBalSurf->HcExtSurf(SurfNum) * TempExt +
                        state.dataHeatBalSurf->SurfQAdditionalHeatSourceOutside(SurfNum) + HRad * RadTemp +
                        construct.CTFCross(0) * state.dataHeatBalSurf->TempSurfIn(SurfNum) +
                        construct.CTFSourceOut(0) * state.dataHeatBalSurf->QsrcHist(SurfNum, 1)) /
                       (construct.CTFOutside(0) + state.dataHeatBalSurf->HcExtSurf(SurfNum) + HRad);
            } else {
                TH11 = (-state.dataHeatBalSurf->CTFConstOutPart(SurfNum) + state.dataHeatBalSurf->HcExtSurf(SurfNum) * TempExt +
                        state.dataHeatBalSurf->SurfQAdditionalHeatSourceOutside(SurfNum) + HRad * RadTemp +
                        construct.CTFCross(0) * state.dataHeatBalSurf->TempSurfIn(SurfNum)) /
                       (construct.CTFOutside(0) + state.dataHeatBalSurf->HcExtSurf(SurfNum) + HRad);
            }
        }
        // Outside heat balance case: No movable insulation, quick conduction
    } else if ((!MovInsulPresent) && (QuickConductionSurf)) {
        if (Surface(SurfNum).OSCMPtr == 0) {
            if (construct.SourceSinkPresent) {
                TH11 =
                    (-state.dataHeatBalSurf->CTFConstOutPart(SurfNum) + state.dataHeatBalSurf->SurfOpaqQRadSWOutAbs(SurfNum) +
                     state.dataHeatBalSurf->SurfQRadLWOutSrdSurfs(SurfNum) +
                     (state.dataHeatBalSurf->HcExtSurf(SurfNum) + state.dataHeatBalSurf->HAirExtSurf(SurfNum)) * TempExt +
                     state.dataHeatBalSurf->SurfQAdditionalHeatSourceOutside(SurfNum) + state.dataHeatBalSurf->HSkyExtSurf(SurfNum) * TSky +
                     state.dataHeatBalSurf->HGrdExtSurf(SurfNum) * TGround + construct.CTFSourceOut(0) * state.dataHeatBalSurf->QsrcHist(SurfNum, 1) +
                     F1 * (state.dataHeatBalSurf->CTFConstInPart(SurfNum) + state.dataHeatBalSurf->SurfOpaqQRadSWInAbs(SurfNum) +
                           state.dataHeatBal->SurfQRadThermInAbs(SurfNum) +
                           state.dataHeatBal->HConvIn(SurfNum) * state.dataHeatBalFanSys->MAT(ZoneNum) +
                           state.dataHeatBalSurf->SurfNetLWRadToSurf(SurfNum))) /
                    (construct.CTFOutside(0) + state.dataHeatBalSurf->HcExtSurf(SurfNum) + state.dataHeatBalSurf->HAirExtSurf(SurfNum) +
                     state.dataHeatBalSurf->HSkyExtSurf(SurfNum) + state.dataHeatBalSurf->HGrdExtSurf(SurfNum) -
                     F1 * construct.CTFCross(0)); // ODB used to approx ground surface temp | MAT use here is problem for room air models
            } else {
                TH11 = (-state.dataHeatBalSurf->CTFConstOutPart(SurfNum) + state.dataHeatBalSurf->SurfOpaqQRadSWOutAbs(SurfNum) +
                        state.dataHeatBalSurf->SurfQRadLWOutSrdSurfs(SurfNum) +
                        (state.dataHeatBalSurf->HcExtSurf(SurfNum) + state.dataHeatBalSurf->HAirExtSurf(SurfNum)) * TempExt +
                        state.dataHeatBalSurf->SurfQAdditionalHeatSourceOutside(SurfNum) + state.dataHeatBalSurf->HSkyExtSurf(SurfNum) * TSky +
                        state.dataHeatBalSurf->HGrdExtSurf(SurfNum) * TGround +
                        F1 * (state.dataHeatBalSurf->CTFConstInPart(SurfNum) + state.dataHeatBalSurf->SurfOpaqQRadSWInAbs(SurfNum) +
                              state.dataHeatBal->SurfQRadThermInAbs(SurfNum) +
                              state.dataHeatBal->HConvIn(SurfNum) * state.dataHeatBalFanSys->MAT(ZoneNum) +
                              state.dataHeatBalSurf->SurfNetLWRadToSurf(SurfNum))) /
                       (construct.CTFOutside(0) + state.dataHeatBalSurf->HcExtSurf(SurfNum) + state.dataHeatBalSurf->HAirExtSurf(SurfNum) +
                        state.dataHeatBalSurf->HSkyExtSurf(SurfNum) + state.dataHeatBalSurf->HGrdExtSurf(SurfNum) -
                        F1 * construct.CTFCross(0)); // ODB used to approx ground surface temp | MAT use here is problem for room air models
            }
            // Outside Heat Balance case: Other Side Conditions Model
        } else { //( Surface(SurfNum)%OSCMPtr > 0 ) THEN
            // local copies of variables for clarity in radiation terms
            Real64 RadTemp = state.dataSurface->OSCM(Surface(SurfNum).OSCMPtr).TRad;
            Real64 HRad = state.dataSurface->OSCM(Surface(SurfNum).OSCMPtr).HRad;
            // patterned after "No movable insulation, quick conduction," but with new radiation terms and no sun,
            if (construct.SourceSinkPresent) {
                TH11 =
                    (-state.dataHeatBalSurf->CTFConstOutPart(SurfNum) + state.dataHeatBalSurf->HcExtSurf(SurfNum) * TempExt +
                     state.dataHeatBalSurf->SurfQAdditionalHeatSourceOutside(SurfNum) + HRad * RadTemp +
                     construct.CTFSourceOut(0) * state.dataHeatBalSurf->QsrcHist(SurfNum, 1) +
                     F1 * (state.dataHeatBalSurf->CTFConstInPart(SurfNum) + state.dataHeatBalSurf->SurfOpaqQRadSWInAbs(SurfNum) +
                           state.dataHeatBal->SurfQRadThermInAbs(SurfNum) + construct.CTFSourceIn(0) * state.dataHeatBalSurf->QsrcHist(SurfNum, 1) +
                           state.dataHeatBal->HConvIn(SurfNum) * state.dataHeatBalFanSys->MAT(ZoneNum) +
                           state.dataHeatBalSurf->SurfNetLWRadToSurf(SurfNum))) /
                    (construct.CTFOutside(0) + state.dataHeatBalSurf->HcExtSurf(SurfNum) + HRad -
                     F1 * construct.CTFCross(0)); // MAT use here is problem for room air models
            } else {
                TH11 = (-state.dataHeatBalSurf->CTFConstOutPart(SurfNum) + state.dataHeatBalSurf->HcExtSurf(SurfNum) * TempExt +
                        state.dataHeatBalSurf->SurfQAdditionalHeatSourceOutside(SurfNum) + HRad * RadTemp +
                        F1 * (state.dataHeatBalSurf->CTFConstInPart(SurfNum) + state.dataHeatBalSurf->SurfOpaqQRadSWInAbs(SurfNum) +
                              state.dataHeatBal->SurfQRadThermInAbs(SurfNum) +
                              state.dataHeatBal->HConvIn(SurfNum) * state.dataHeatBalFanSys->MAT(ZoneNum) +
                              state.dataHeatBalSurf->SurfNetLWRadToSurf(SurfNum))) /
                       (construct.CTFOutside(0) + state.dataHeatBalSurf->HcExtSurf(SurfNum) + HRad -
                        F1 * construct.CTFCross(0)); // MAT use here is problem for room air models
            }
        }
        // Outside heat balance case: Movable insulation, slow conduction
    } else if ((MovInsulPresent) && (!QuickConductionSurf)) {

        F2 = HMovInsul / (HMovInsul + state.dataHeatBalSurf->HcExtSurf(SurfNum) + state.dataHeatBalSurf->HAirExtSurf(SurfNum) +
                          state.dataHeatBalSurf->HSkyExtSurf(SurfNum) + state.dataHeatBalSurf->HGrdExtSurf(SurfNum));

        TH11 = (-state.dataHeatBalSurf->CTFConstOutPart(SurfNum) + state.dataHeatBalSurf->SurfOpaqQRadSWOutAbs(SurfNum) +
                state.dataHeatBalSurf->SurfQRadLWOutSrdSurfs(SurfNum) + construct.CTFCross(0) * state.dataHeatBalSurf->TempSurfIn(SurfNum) +
                F2 * (state.dataHeatBalSurf->SurfQRadSWOutMvIns(SurfNum) +
                      (state.dataHeatBalSurf->HcExtSurf(SurfNum) + state.dataHeatBalSurf->HAirExtSurf(SurfNum)) * TempExt +
                      state.dataHeatBalSurf->SurfQAdditionalHeatSourceOutside(SurfNum) + state.dataHeatBalSurf->HSkyExtSurf(SurfNum) * TSky +
                      state.dataHeatBalSurf->HGrdExtSurf(SurfNum) * TGround)) /
               (construct.CTFOutside(0) + HMovInsul - F2 * HMovInsul); // ODB used to approx ground surface temp

        // Outside heat balance case: Movable insulation, quick conduction
    } else if ((MovInsulPresent) && (QuickConductionSurf)) {

        F2 = HMovInsul / (HMovInsul + state.dataHeatBalSurf->HcExtSurf(SurfNum) + state.dataHeatBalSurf->HAirExtSurf(SurfNum) +
                          state.dataHeatBalSurf->HSkyExtSurf(SurfNum) + state.dataHeatBalSurf->HGrdExtSurf(SurfNum));

        TH11 = (-state.dataHeatBalSurf->CTFConstOutPart(SurfNum) + state.dataHeatBalSurf->SurfOpaqQRadSWOutAbs(SurfNum) +
                state.dataHeatBalSurf->SurfQRadLWOutSrdSurfs(SurfNum) +
                F1 * (state.dataHeatBalSurf->CTFConstInPart(SurfNum) + state.dataHeatBalSurf->SurfOpaqQRadSWInAbs(SurfNum) +
                      state.dataHeatBal->SurfQRadThermInAbs(SurfNum) + state.dataHeatBal->HConvIn(SurfNum) * state.dataHeatBalFanSys->MAT(ZoneNum) +
                      state.dataHeatBalSurf->SurfNetLWRadToSurf(SurfNum)) +
                F2 * (state.dataHeatBalSurf->SurfQRadSWOutMvIns(SurfNum) +
                      (state.dataHeatBalSurf->HcExtSurf(SurfNum) + state.dataHeatBalSurf->HAirExtSurf(SurfNum)) * TempExt +
                      state.dataHeatBalSurf->SurfQAdditionalHeatSourceOutside(SurfNum) + state.dataHeatBalSurf->HSkyExtSurf(SurfNum) * TSky +
                      state.dataHeatBalSurf->HGrdExtSurf(SurfNum) * TGround)) /
               (construct.CTFOutside(0) + HMovInsul - F2 * HMovInsul - F1 * construct.CTFCross(0)); // ODB used to approx ground surface temp

    } // ...end of outside heat balance cases IF-THEN block

    // multiply out linearized radiation coeffs for reporting
    Real64 const HExtSurf_fac(-(state.dataHeatBalSurf->HSkyExtSurf(SurfNum) * (TH11 - TSky) +
                                state.dataHeatBalSurf->HAirExtSurf(SurfNum) * (TH11 - TempExt) +
                                state.dataHeatBalSurf->HGrdExtSurf(SurfNum) * (TH11 - TGround)));
    Real64 QRadLWOutSrdSurfsRep;
    QRadLWOutSrdSurfsRep = 0;
    // Report LWR from surrounding surfaces for current exterior surf temp
    // Current exterior surf temp would be used for the next step LWR calculation.
    if (state.dataSurface->SurfHasSurroundingSurfProperties(SurfNum)) {
        int SrdSurfsNum = state.dataSurface->SurfSurroundingSurfacesNum(SurfNum);
        for (int SrdSurfNum = 1; SrdSurfNum <= state.dataSurface->SurroundingSurfsProperty(SrdSurfsNum).TotSurroundingSurface; SrdSurfNum++) {
            Real64 SrdSurfViewFac = state.dataSurface->SurroundingSurfsProperty(SrdSurfsNum).SurroundingSurfs(SrdSurfNum).ViewFactor;
            Real64 SrdSurfTempAbs =
                GetCurrentScheduleValue(state, state.dataSurface->SurroundingSurfsProperty(SrdSurfsNum).SurroundingSurfs(SrdSurfNum).TempSchNum) +
                DataGlobalConstants::KelvinConv;
            QRadLWOutSrdSurfsRep += DataGlobalConstants::StefanBoltzmann *
                                    state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).AbsorpThermal *
                                    SrdSurfViewFac * (pow_4(SrdSurfTempAbs) - pow_4(TH11 + DataGlobalConstants::KelvinConv));
        }
    }
    state.dataHeatBalSurf->QdotRadOutRep(SurfNum) = Surface(SurfNum).Area * HExtSurf_fac + Surface(SurfNum).Area * QRadLWOutSrdSurfsRep;
    state.dataHeatBalSurf->QdotRadOutRepPerArea(SurfNum) = state.dataHeatBalSurf->QdotRadOutRep(SurfNum) / Surface(SurfNum).Area;

    state.dataHeatBalSurf->QRadOutReport(SurfNum) = state.dataHeatBalSurf->QdotRadOutRep(SurfNum) * state.dataGlobal->TimeStepZoneSec;

    // Calculate surface heat emission to the air, positive values indicates heat transfer from surface to the outside
    state.dataHeatBalSurf->QAirExtReport(SurfNum) = Surface(SurfNum).Area * state.dataHeatBalSurf->HAirExtSurf(SurfNum) *
                                                    (state.dataHeatBalSurf->TH(1, 1, SurfNum) - state.dataSurface->SurfOutDryBulbTemp(SurfNum));

    // Set the radiant system heat balance coefficients if this surface is also a radiant system
    if (construct.SourceSinkPresent) {

        if (MovInsulPresent) {
            // Note: if movable insulation is ever added back in correctly, the heat balance equations above must be fixed
            ShowSevereError(state, "Exterior movable insulation is not valid with embedded sources/sinks");
            ShowContinueError(state, "Construction " + construct.Name + " contains an internal source or sink but also uses");
            ShowContinueError(state,
                              "exterior movable insulation " +
                                  state.dataMaterial->Material(state.dataSurface->SurfMaterialMovInsulExt(SurfNum)).Name +
                                  " for a surface with that construction.");
            ShowContinueError(state,
                              "This is not currently allowed because the heat balance equations do not currently accommodate this combination.");
            ErrorFlag = true;
            return;

        } else {
            Real64 const RadSysDiv(1.0 / (construct.CTFOutside(0) + state.dataHeatBalSurf->HcExtSurf(SurfNum) +
                                          state.dataHeatBalSurf->HAirExtSurf(SurfNum) + state.dataHeatBalSurf->HSkyExtSurf(SurfNum) +
                                          state.dataHeatBalSurf->HGrdExtSurf(SurfNum)));

            state.dataHeatBalFanSys->RadSysToHBConstCoef(SurfNum) =
                (-state.dataHeatBalSurf->CTFConstOutPart(SurfNum) + state.dataHeatBalSurf->SurfOpaqQRadSWOutAbs(SurfNum) +
                 state.dataHeatBalSurf->SurfQRadLWOutSrdSurfs(SurfNum) +
                 (state.dataHeatBalSurf->HcExtSurf(SurfNum) + state.dataHeatBalSurf->HAirExtSurf(SurfNum)) * TempExt +
                 state.dataHeatBalSurf->HSkyExtSurf(SurfNum) * TSky + state.dataHeatBalSurf->HGrdExtSurf(SurfNum) * TGround) *
                RadSysDiv; // ODB used to approx ground surface temp

            state.dataHeatBalFanSys->RadSysToHBTinCoef(SurfNum) = construct.CTFCross(0) * RadSysDiv;

            state.dataHeatBalFanSys->RadSysToHBQsrcCoef(SurfNum) = construct.CTFSourceOut(0) * RadSysDiv;
        }
    }
}

void CalcExteriorVentedCavity(EnergyPlusData &state, int const SurfNum) // index of surface
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B Griffith
    //       DATE WRITTEN   January 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // manages calculating the temperatures of baffle and air cavity for
    // multi-skin configuration.

    // METHODOLOGY EMPLOYED:
    // derived from CalcPassiveTranspiredCollector

    // Using/Aliasing
    using ConvectionCoefficients::InitExteriorConvectionCoeff;
    using Psychrometrics::PsyCpAirFnW;
    using Psychrometrics::PsyRhoAirFnPbTdbW;
    using Psychrometrics::PsyWFnTdbTwbPb;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static std::string const BlankString;

    // local working variables
    Real64 AspRat; // Aspect Ratio of gap
    Real64 TmpTscoll;
    Real64 TmpTaPlen;
    Real64 RhoAir;
    Real64 holeArea;
    Real64 HrPlen;
    Real64 HcPlen;
    Real64 Isc;
    Real64 MdotVent;
    Real64 VdotWind;
    Real64 VdotThermal;
    int CavNum; // do loop counter
    int iter;   // do loop counter
    int thisOSCM;
    Real64 TempExt;
    Real64 OutHumRatExt;

    CavNum = state.dataSurface->SurfExtCavNum(SurfNum);

    TempExt = state.dataSurface->SurfOutDryBulbTemp(SurfNum);

    OutHumRatExt = PsyWFnTdbTwbPb(
        state, state.dataSurface->SurfOutDryBulbTemp(SurfNum), state.dataSurface->SurfOutWetBulbTemp(SurfNum), state.dataEnvrn->OutBaroPress);

    RhoAir = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, TempExt, OutHumRatExt);

    holeArea = state.dataSurface->ExtVentedCavity(CavNum).ActualArea * state.dataSurface->ExtVentedCavity(CavNum).Porosity;

    AspRat = state.dataSurface->ExtVentedCavity(CavNum).HdeltaNPL * 2.0 / state.dataSurface->ExtVentedCavity(CavNum).PlenGapThick;
    TmpTscoll = state.dataSurface->ExtVentedCavity(CavNum).TbaffleLast;
    TmpTaPlen = state.dataSurface->ExtVentedCavity(CavNum).TairLast;

    // all the work is done in this routine located in GeneralRoutines.cc

    for (iter = 1; iter <= 3; ++iter) { // this is a sequential solution approach.

        CalcPassiveExteriorBaffleGap(state,
                                     state.dataSurface->ExtVentedCavity(CavNum).SurfPtrs,
                                     holeArea,
                                     state.dataSurface->ExtVentedCavity(CavNum).Cv,
                                     state.dataSurface->ExtVentedCavity(CavNum).Cd,
                                     state.dataSurface->ExtVentedCavity(CavNum).HdeltaNPL,
                                     state.dataSurface->ExtVentedCavity(CavNum).SolAbsorp,
                                     state.dataSurface->ExtVentedCavity(CavNum).LWEmitt,
                                     state.dataSurface->ExtVentedCavity(CavNum).Tilt,
                                     AspRat,
                                     state.dataSurface->ExtVentedCavity(CavNum).PlenGapThick,
                                     state.dataSurface->ExtVentedCavity(CavNum).BaffleRoughness,
                                     state.dataSurface->ExtVentedCavity(CavNum).QdotSource,
                                     TmpTscoll,
                                     TmpTaPlen,
                                     HcPlen,
                                     HrPlen,
                                     Isc,
                                     MdotVent,
                                     VdotWind,
                                     VdotThermal);

    } // sequential solution
    // now fill results into derived types
    state.dataSurface->ExtVentedCavity(CavNum).Isc = Isc;
    state.dataSurface->ExtVentedCavity(CavNum).TAirCav = TmpTaPlen;
    state.dataSurface->ExtVentedCavity(CavNum).Tbaffle = TmpTscoll;
    state.dataSurface->ExtVentedCavity(CavNum).HrPlen = HrPlen;
    state.dataSurface->ExtVentedCavity(CavNum).HcPlen = HcPlen;
    state.dataSurface->ExtVentedCavity(CavNum).PassiveACH =
        (MdotVent / RhoAir) *
        (1.0 / (state.dataSurface->ExtVentedCavity(CavNum).ProjArea * state.dataSurface->ExtVentedCavity(CavNum).PlenGapThick)) *
        DataGlobalConstants::SecInHour;
    state.dataSurface->ExtVentedCavity(CavNum).PassiveMdotVent = MdotVent;
    state.dataSurface->ExtVentedCavity(CavNum).PassiveMdotWind = VdotWind * RhoAir;
    state.dataSurface->ExtVentedCavity(CavNum).PassiveMdotTherm = VdotThermal * RhoAir;

    // now do some updates
    state.dataSurface->ExtVentedCavity(CavNum).TairLast = state.dataSurface->ExtVentedCavity(CavNum).TAirCav;
    state.dataSurface->ExtVentedCavity(CavNum).TbaffleLast = state.dataSurface->ExtVentedCavity(CavNum).Tbaffle;

    // update the OtherSideConditionsModel coefficients.
    thisOSCM = state.dataSurface->ExtVentedCavity(CavNum).OSCMPtr;

    state.dataSurface->OSCM(thisOSCM).TConv = state.dataSurface->ExtVentedCavity(CavNum).TAirCav;
    state.dataSurface->OSCM(thisOSCM).HConv = state.dataSurface->ExtVentedCavity(CavNum).HcPlen;
    state.dataSurface->OSCM(thisOSCM).TRad = state.dataSurface->ExtVentedCavity(CavNum).Tbaffle;
    state.dataSurface->OSCM(thisOSCM).HRad = state.dataSurface->ExtVentedCavity(CavNum).HrPlen;
}

void GatherComponentLoadsSurfAbsFact(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Jason Glazer
    //       DATE WRITTEN   September 2012

    // PURPOSE OF THIS SUBROUTINE:
    //   Gather values during sizing used for surface absorption factors

    // METHODOLOGY EMPLOYED:
    //   Save sequence of values for report during sizing.

    auto &Surface(state.dataSurface->Surface);

    if (state.dataGlobal->CompLoadReportIsReq && !state.dataGlobal->isPulseZoneSizing) {
        int TimeStepInDay = (state.dataGlobal->HourOfDay - 1) * state.dataGlobal->NumOfTimeStepInHour + state.dataGlobal->TimeStep;
        for (int enclosureNum = 1; enclosureNum <= state.dataViewFactor->NumOfRadiantEnclosures; ++enclosureNum) {
            state.dataOutRptTab->TMULTseq(state.dataSize->CurOverallSimDay, TimeStepInDay, enclosureNum) = state.dataHeatBal->TMULT(enclosureNum);
        }
        for (int jSurf = 1; jSurf <= state.dataSurface->TotSurfaces; ++jSurf) {
            if (!Surface(jSurf).HeatTransSurf || Surface(jSurf).Zone == 0) continue; // Skip non-heat transfer surfaces
            if (Surface(jSurf).Class == SurfaceClass::TDD_Dome) continue;            // Skip tubular daylighting device domes
            state.dataOutRptTab->ITABSFseq(state.dataSize->CurOverallSimDay, TimeStepInDay, jSurf) = state.dataHeatBal->ITABSF(jSurf);
        }
    }
}

} // namespace EnergyPlus::HeatBalanceSurfaceManager
